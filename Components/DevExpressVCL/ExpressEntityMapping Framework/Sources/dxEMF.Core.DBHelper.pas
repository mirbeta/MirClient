{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxEMF.Core.DBHelper;

interface

{$I dxEMF.inc}

uses
  SysUtils, Generics.Collections, TypInfo,
  dxEMF.Attributes,
  dxEMF.Metadata,
  dxEMF.DB.Model,
  dxEMF.Types;

type

  { TdxDBTableHelper }

  TdxDBTableHelper = class sealed
  strict private const
    OrdinalTypes = [tkInteger, tkChar, tkWChar, tkEnumeration, tkInt64];
    SimpleTypes = OrdinalTypes + [tkFloat];
    StringTypes = [tkString, tkLString, tkWString, tkUString];
  protected
    class function CreateForeignKeyColumns(AEntityInfo: TdxEntityInfo; const AColumnName: string): TArray<TdxDBColumn>; static;
    class function GetPrimaryKeyTable(AEntityInfo: TdxEntityInfo; AColumns: TList<TdxDBColumn>): string; static;
    class function CreateColumn(AMemberInfo: TdxMappingMemberInfo; const AName: string): TArray<TdxDBColumn>; static;
    class procedure ProcessMembers(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo); static;
    class function ProcessMemberColumns(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo;
      AMemberInfo: TdxMappingMemberInfo): TArray<TdxDBColumn>; static;
    class procedure ProcessPrimaryKey(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo; AColumns: TList<TdxDBColumn>); overload; static;
    class procedure ProcessPrimaryKey(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo); overload; static;
    class procedure ProcessForeignKey(ATable: TdxDBTable; AAssociationEntityInfo: TdxEntityInfo; const AColumns: TArray<TdxDBColumn>); static;
    class procedure ProcessIndexes(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo); static;
  public
    class function ProcessEntityInfo(AEntityInfo: TdxEntityInfo): TObjectList<TdxDBTable>; overload; static;
    class function ProcessClassInfo(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo): TdxEntityInfo; static;
  end;

implementation

uses
  StrUtils, Rtti, dxCore,
  dxEMF.Utils,
  dxEMF.Utils.Exceptions,
  dxEMF.Strs,
  dxEMF.Serializer;

function DiscriminatorTypeAsDBType(ADiscriminatorType: TdxDiscriminatorType): TdxDBColumnType;
begin
  case ADiscriminatorType of
    TdxDiscriminatorType.String:
      Result := TdxDBColumnType.String;
    TdxDiscriminatorType.Integer:
      Result := TdxDBColumnType.Int32;
    else
      Result := TdxDBColumnType.Unknown;
  end;
end;

{ TdxDBTableHelper }

class function TdxDBTableHelper.ProcessEntityInfo(AEntityInfo: TdxEntityInfo): TObjectList<TdxDBTable>;
var
  ADBTable: TdxDBTable;
begin
  Result := TObjectList<TdxDBTable>.Create;
  repeat
    ADBTable := TdxDBTable.Create(AEntityInfo.ClassAttributes.TableName);
    AEntityInfo := ProcessClassInfo(ADBTable, AEntityInfo);
    Result.Add(ADBTable);
  until AEntityInfo = nil;
end;

class procedure TdxDBTableHelper.ProcessForeignKey(ATable: TdxDBTable;
  AAssociationEntityInfo: TdxEntityInfo; const AColumns: TArray<TdxDBColumn>);
var
  AForeignKey: TdxDBForeignKey;
begin
  AForeignKey := TdxDBForeignKey.Create(AColumns, AAssociationEntityInfo.ClassAttributes.TableName,
    AAssociationEntityInfo.KeyAttributes.GetColumnsNames);
  ATable.ForeignKeys.Add(AForeignKey);
end;

class procedure TdxDBTableHelper.ProcessIndexes(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo);
var
  AIndex: TdxDBIndex;
  AIsOwnTable: Boolean;
  AInstanceType: TRttiInstanceType;
  I: Integer;
begin
  AIsOwnTable := AEntityInfo.ClassAttributes.IsInheritance and
    (AEntityInfo.ClassAttributes.MapInheritance = TdxMapInheritanceType.OwnTable);
  AInstanceType := AEntityInfo.ClassAttributes.RttiType as TRttiInstanceType;
  repeat
    for I := 0 to AEntityInfo.ClassAttributes.Indexes.Count - 1 do
    begin
      AIndex := TdxDBIndex.Create(AEntityInfo.ClassAttributes.Indexes[I].Columns, AEntityInfo.ClassAttributes.Indexes[I].IsUnique);
      if not ATable.IsIndexIncluded(AIndex) then
        ATable.AddIndex(AIndex)
      else
        AIndex.Free;
    end;
    AInstanceType := AInstanceType.BaseType;
    AEntityInfo := EntityManager.GetEntityInfo((AEntityInfo.ClassAttributes.RttiType as TRttiInstanceType).
      BaseType.MetaclassType)
  until AIsOwnTable or not EntityManager.HasClass(AInstanceType.MetaclassType);
end;

class function TdxDBTableHelper.ProcessMemberColumns(ATable: TdxDBTable;
  AEntityInfo: TdxEntityInfo; AMemberInfo: TdxMappingMemberInfo): TArray<TdxDBColumn>;
var
  AAssociationEntityInfo: TdxEntityInfo;
  ADBColumn: TdxDBColumn;
begin
  if AMemberInfo.IsAssociationList { or AMemberInfo.IsNotSupportType } then
    Exit(nil);

//  if AMemberInfo.IsAssociationObject then
  if AMemberInfo.ReferenceType <> nil then
  begin
    AAssociationEntityInfo := AMemberInfo.ReferenceType;
    Result := CreateForeignKeyColumns(AAssociationEntityInfo, AMemberInfo.ColumnName);
    for ADBColumn in Result do
    begin
      ADBColumn.IsKey := AMemberInfo.IsKey;
      ADBColumn.IsNullable := not AMemberInfo.IsKey;
    end;
    if not (TdxAttribute.NoForeignKey in AMemberInfo.Attributes) then
      ProcessForeignKey(ATable, AAssociationEntityInfo, Result);
  end
  else
    Result := CreateColumn(AMemberInfo, AMemberInfo.ColumnName);
end;

class procedure TdxDBTableHelper.ProcessMembers(ATable: TdxDBTable; AEntityInfo: TdxEntityInfo);
var
  AMemberInfo: TdxMappingMemberInfo;
  ADBColumn: TdxDBColumn;
  ADBColumns: TArray<TdxDBColumn>;
  AIsNullable, AIsParentTable, AIsParentTableMember: Boolean;
  ADiscriminator: TdxDiscriminator;
  AClass: TClass;
  AMemberParentEntityInfo: TdxEntityInfo;
begin
  AClass := nil;
  AIsParentTable := AEntityInfo.ClassAttributes.IsParentTable;
  AIsParentTableMember := AIsParentTable;
  for AMemberInfo in AEntityInfo.PersistentProperties do
  begin
    if AClass <> AMemberInfo.ParentClass then
    begin
      AClass := AMemberInfo.ParentClass;
      AMemberParentEntityInfo := EntityManager.GetEntityInfo(AClass);
      AIsParentTableMember := AMemberParentEntityInfo.ClassAttributes.IsParentTable;
    end;

    AIsNullable := AMemberInfo.IsNullable;
    if not AMemberInfo.IsKey then
      if AEntityInfo.ClassAttributes.IsInheritance then
      begin
        if AIsParentTableMember then
          AIsNullable := True;
        if (AMemberInfo.Member.Parent <> AEntityInfo.ClassAttributes.RttiType) and not AIsParentTable then
          Continue;
      end;
    ADBColumns := ProcessMemberColumns(ATable, AEntityInfo, AMemberInfo);
    for ADBColumn in ADBColumns do
    begin
      ADBColumn.IsNullable := ADBColumn.IsNullable or AIsNullable;
      ATable.AddColumn(ADBColumn);
    end;
  end;
  repeat
    ADiscriminator := AEntityInfo.ClassAttributes.DiscriminatorInfo;
    if ADiscriminator.HasValue then
    begin
      ADBColumn := TdxDBColumn.Create(ADiscriminator.ColumnName, False, '',
        ADiscriminator.ColumnSize, DiscriminatorTypeAsDBType(ADiscriminator.DiscriminatorType));
      ADBColumn.IsNullable := True;
      ATable.AddColumn(ADBColumn);
    end;
    if AEntityInfo.ClassAttributes.IsParentTable then
      AEntityInfo := EntityManager.GetEntityInfo((AEntityInfo.ClassAttributes.RttiType as TRttiInstanceType).
        BaseType.MetaclassType)
    else
      AEntityInfo := nil;
  until AEntityInfo = nil;
end;

class procedure TdxDBTableHelper.ProcessPrimaryKey(ATable: TdxDBTable;
  AEntityInfo: TdxEntityInfo);
var
  AColumnName: string;
  AKeyAttribute: TdxMappingMemberInfo;
  AColumn: TdxDBColumn;
  AColumns: TList<TdxDBColumn>;
begin
  AColumns := TList<TdxDBColumn>.Create;
  try
    for AKeyAttribute in AEntityInfo.KeyAttributes do
    begin
      AColumnName := AKeyAttribute.ColumnName;
      AColumn := ATable.GetColumn(AColumnName);
      Assert(AColumn <> nil);
      AColumns.Add(AColumn);
    end;
    ProcessPrimaryKey(ATable, AEntityInfo, AColumns);
  finally
    AColumns.Free;
  end;
end;

class procedure TdxDBTableHelper.ProcessPrimaryKey(ATable: TdxDBTable;
  AEntityInfo: TdxEntityInfo; AColumns: TList<TdxDBColumn>);
var
  APrimaryKeyTable: string;
begin
  if ATable.PrimaryKey = nil then
    ATable.PrimaryKey := TdxDBPrimaryKey.Create(AColumns)
  else
    ATable.PrimaryKey.AddColumns(AColumns);
  if (AColumns.Count = 1) and
    not (AEntityInfo.ClassAttributes.IsInheritance and (AEntityInfo.ClassAttributes.MapInheritance = TdxMapInheritanceType.OwnTable)) and
    AEntityInfo.KeyProperty.IsIdentity then
    AColumns[0].IsIdentity := True;
  if AEntityInfo.ClassAttributes.IsInheritance and
    (AEntityInfo.ClassAttributes.MapInheritance = TdxMapInheritanceType.OwnTable) then
  begin
    APrimaryKeyTable := GetPrimaryKeyTable(AEntityInfo, AColumns);
    ATable.AddForeignKey(TdxDBForeignKey.Create(AColumns.ToArray, APrimaryKeyTable));
  end;
end;

class function TdxDBTableHelper.CreateColumn(AMemberInfo: TdxMappingMemberInfo;
  const AName: string): TArray<TdxDBColumn>;
var
  ADBColumnType: TdxDBColumnType;
  AType: PTypeInfo;
  AIsNullable: Boolean;
  ADBColumn: TdxDBColumn;
  AWorkMemberInfo: TdxMappingMemberInfo;
begin
  if AMemberInfo.ReferenceType = nil then
    AWorkMemberInfo := AMemberInfo
  else
    AWorkMemberInfo := AMemberInfo.ReferenceType.KeyProperty.Member;
  AType := AWorkMemberInfo.MemberType.Handle;
  AIsNullable := IsNullableType(AType);
  if AIsNullable then
    TryGetUnderlyingType(AType, AType);
  SetLength(Result, 1);
  ADBColumnType := TdxDBColumn.GetColumnType(AType);
  if (ADBColumnType = TdxDBColumnType.Unknown) and AMemberInfo.IsSerialize then
    ADBColumnType := TdxDBColumnType.ByteArray;
  ADBColumn := TdxDBColumn.Create(AName, AMemberInfo.IsKey,
    IfThen(AMemberInfo.DBColumnTypeName <> '', AMemberInfo.DBColumnTypeName),
    AWorkMemberInfo.DBColumnSize, ADBColumnType);
  ADBColumn.IsNullable := AIsNullable or AMemberInfo.IsNullable or (ADBColumnType = TdxDBColumnType.ByteArray) or
    AMemberInfo.IsBLOB;
  ADBColumn.Generator := AMemberInfo.DBGenerator;
  Result[0] := ADBColumn;
end;

class function TdxDBTableHelper.CreateForeignKeyColumns(AEntityInfo: TdxEntityInfo;
  const AColumnName: string): TArray<TdxDBColumn>;
var
  AColumns: TArray<TdxDBColumn>;
  AColumn: TdxDBColumn;
  AResult: TList<TdxDBColumn>;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AResult := TList<TdxDBColumn>.Create;
  try
    for AMemberInfo in AEntityInfo.KeyAttributes do
    begin
      AColumns := CreateColumn(AMemberInfo, AColumnName);
      for AColumn in AColumns do
        AColumn.IsNullable := True;
      AResult.AddRange(AColumns);
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

class function TdxDBTableHelper.GetPrimaryKeyTable(AEntityInfo: TdxEntityInfo; AColumns: TList<TdxDBColumn>): string;

  function FindMemberEntity(const AColumnName: string; AEntityInfo: TdxEntityInfo): TdxEntityInfo;
  var
    AMemberEntity: TdxEntityInfo;
    AMemberInfo: TdxMappingMemberInfo;
  begin
    Result := AEntityInfo.ParentEntity;
    if Result = nil then
      Exit;
    if AEntityInfo.ParentEntity.ClassAttributes.PersistentClass <> AEntityInfo.ClassAttributes.PersistentBaseClass then
      Result := EntityManager.GetEntityInfo(AEntityInfo.ClassAttributes.PersistentBaseClass);
    AMemberInfo := Result.FindMemberByColumnName(AColumnName);
    if AMemberInfo = nil then
      Exit(nil);
    AMemberEntity := FindMemberEntity(AColumnName, Result);
    if AMemberEntity <> nil then
      Result := AMemberEntity;
  end;

var
  AColumn: TdxDBColumn;
  AMemberInfo: TdxMappingMemberInfo;
  AMemberEntity: TdxEntityInfo;
begin
  Result := '';
  for AColumn in AColumns do
  begin
    AMemberInfo := AEntityInfo.FindMemberByColumnName(AColumn.Name);
    if AMemberInfo <> nil then
    begin
      AMemberEntity := FindMemberEntity(AColumn.Name, AEntityInfo);
      if AMemberEntity = nil then
        raise EdxEMFException.Create(sdxPrimaryKeyNotFoundInBaseClass);
      Result := AMemberEntity.ClassAttributes.TableName;
    end;
  end;
end;

class function TdxDBTableHelper.ProcessClassInfo(ATable: TdxDBTable;
  AEntityInfo: TdxEntityInfo): TdxEntityInfo;
begin
  ProcessMembers(ATable, AEntityInfo);
  ProcessPrimaryKey(ATable, AEntityInfo);
  ProcessIndexes(ATable, AEntityInfo);
  Result := AEntityInfo.ParentEntity;
end;

end.

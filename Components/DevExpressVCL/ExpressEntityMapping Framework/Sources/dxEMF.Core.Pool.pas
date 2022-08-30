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

unit dxEMF.Core.Pool;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, DB, RTTI,
  dxEMF.Types,
  dxEMF.Utils,
  dxEMF.Metadata;

type

  { TdxCustomEntityPool }

  TdxCustomEntityPool = class abstract
  strict private
    FEntityInfo: TdxEntityInfo;
  public
    function Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean; overload; virtual; abstract;
    function Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean; overload; virtual; abstract;
    function GetByKey(const AKeys: TArray<TValue>): TObject; virtual; abstract;
    procedure Extract(AObject: TObject); virtual; abstract;
    function IsNewObject(AObject: TObject): Boolean; virtual; abstract;
    property EntityInfo: TdxEntityInfo read FEntityInfo write FEntityInfo;
  end;

  { TdxCustomEntityPool<T> }

  TdxCustomEntityPool<T> = class(TdxCustomEntityPool)
  strict private
    FObjectKeys: TDictionary<Pointer, T>;
    FObjectPool: TDictionary<T, TObject>;
  protected
    procedure Add(const AKey: T; AObject: TObject); overload; inline;
    function GetComparer: TEqualityComparer<T>; virtual;
    property ObjectKeys: TDictionary<Pointer, T> read FObjectKeys;
    property ObjectPool: TDictionary<T, TObject> read FObjectPool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Extract(AObject: TObject); override;
    function IsNewObject(AObject: TObject): Boolean; override;
  end;

  { TdxIntegerEntityPool }

  TdxIntegerKeyEntityPool = class(TdxCustomEntityPool<Integer>)
  public
    function Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean; override;
    function Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean; override;
    function GetByKey(const AKeys: TArray<TValue>): TObject; override;
  end;

  { TdxInt64EntityPool }

  TdxInt64KeyEntityPool = class(TdxCustomEntityPool<Int64>)
  public
    function Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean; override;
    function Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean; override;
    function GetByKey(const AKeys: TArray<TValue>): TObject; override;
  end;

  { TdxSimpleKeyEntityPool<T> }

  TdxSimpleKeyEntityPool<T> = class(TdxCustomEntityPool<T>)
  public
    function Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean; overload; override;
    function Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean; overload; override;
    function GetByKey(const AKeys: TArray<TValue>): TObject; override;
  end;

  { TdxStringKeyEntityPool }

  TdxStringKeyEntityPool = class(TdxSimpleKeyEntityPool<string>)
  end;

  { TdxGUIDComparer }

  TdxGUIDComparer = class(TEqualityComparer<TGUID>)
  public
    function Equals(const Left, Right: TGUID): Boolean; override;
    function GetHashCode(const Value: TGUID): Integer; override;
  end;

  { TdxGUIDEntityPool }

  TdxGUIDKeyEntityPool = class(TdxSimpleKeyEntityPool<TGUID>)
  protected
    function GetComparer: TEqualityComparer<TGUID>; override;
  end;

  { TdxBytesComparer }

  TdxBytesComparer = class(TEqualityComparer<TBytes>)
  public
    function Equals(const Left, Right: TBytes): Boolean; override;
    function GetHashCode(const Value: TBytes): Integer; override;
  end;

  { TdxEntityPool }

  TdxEntityPool = class(TdxCustomEntityPool<TBytes>)
  protected
    function GetComparer: TEqualityComparer<TBytes>; override;
  public
    function Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean; override;
    function Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean; override;
    function GetByKey(const AKeys: TArray<TValue>): TObject; override;
    function GetKeys(AObject: TObject): TArray<TValue>;
    class function KeysToPoolKey(const AKeys: TArray<TValue>): TBytes; static;
    class procedure SaveToStream(AStream: TStream; const AValue: TValue); static;
  end;

  { TdxEntityPoolClasses }

  TdxEntityPoolClasses = class
  strict private
    FPool: TObjectDictionary<TClass, TdxCustomEntityPool>;
  protected
    class function CreatePool(AEntityInfo: TdxEntityInfo): TdxCustomEntityPool; static;
  public
    constructor Create;
    destructor Destroy; override;
    function AppendObject(const AKeys: TArray<TValue>; AObject: TObject; AIsLoaded: Boolean = False): Boolean; overload;
    function AppendObject(AObject: TObject; AIsLoaded: Boolean = False): Boolean; overload;
    procedure ExtractObject(AObject: TObject);
    function FindObjectByKey(AClass: TClass; const AKeys: TArray<TValue>): TObject;
    function GetPool(AClass: TClass): TdxCustomEntityPool;
    function IsNewObject(AObject: TObject): Boolean;
  end;

  { TdxObjectFinder }

  TdxObjectFinder = class
  strict private
    FEMFClass: TClass;
    FKeyMembers: TdxMemberInfoList;
    procedure PopulateKeyMembers;
  public
    constructor Create(AEMFClass: TClass);
    destructor Destroy; override;
    function GetHash(const AKeys: TArray<TValue>): Cardinal; overload;
    function GetHash(AObject: TObject): Cardinal; overload;
    function HasKey<TKey>(AObject: TObject; const AKey: TKey): Boolean; overload;
    function HasKey(AObject: TObject; const AKey: TValue): Boolean; overload;
    function HasKey(AObject: TObject; const AKeys: array of TValue): Boolean; overload;
    property EMFClass: TClass read FEMFClass;
    property KeyMembers: TdxMemberInfoList read FKeyMembers;
  end;

function ObjectFinder(AEMFClass: TClass): TdxObjectFinder;

implementation

uses
{$IFDEF DELPHIXE8}
  System.Hash,
{$ENDIF}
  Math, TypInfo, FMTBcd, TimeSpan,
  dxEMF.Core,
  dxEMF.Utils.Exceptions,
  dxEMF.Strs;

var
  FObjectFinder: TObjectDictionary<TClass, TdxObjectFinder>;

function ObjectFinder(AEMFClass: TClass): TdxObjectFinder;
begin
  if FObjectFinder = nil then
    FObjectFinder := TObjectDictionary<TClass, TdxObjectFinder>.Create([doOwnsValues]);
  if not FObjectFinder.TryGetValue(AEMFClass, Result) then
  begin
    Result := TdxObjectFinder.Create(AEMFClass);
    FObjectFinder.Add(AEMFClass, Result);
  end;
end;

{ TBytesComparer }

function TdxBytesComparer.Equals(const Left, Right: TBytes): Boolean;
begin
  if Length(Left) <> Length(Right) then
    Exit(False);
  Result := CompareMem(@Left[0], @Right[0], Length(Left));
end;

function TdxBytesComparer.GetHashCode(const Value: TBytes): Integer;
begin
{$IFDEF DELPHIXE8}
  Result := THashBobJenkins.GetHashValue(Value[0], Length(Value), 0);
{$ELSE}
  Result := BobJenkinsHash(Value[0], Length(Value), 0);
{$ENDIF}
end;

{ TdxObjectFinder }

constructor TdxObjectFinder.Create(AEMFClass: TClass);
begin
  inherited Create;
  FEMFClass := AEMFClass;
  FKeyMembers := TdxMemberInfoList.Create;
  PopulateKeyMembers;
end;

destructor TdxObjectFinder.Destroy;
begin
  FreeAndNil(FKeyMembers);
  inherited Destroy;
end;

function TdxObjectFinder.GetHash(const AKeys: TArray<TValue>): Cardinal;
begin
  Result := GetValueHash(AKeys);
end;

function TdxObjectFinder.GetHash(AObject: TObject): Cardinal;
const
  MagicNumber = $5BD1E995;
var
  I: Integer;
  AMemberInfo: TdxMappingMemberInfo;
  AValue: TValue;
  AValues: TArray<TValue>;
begin
  if FKeyMembers.Count = 1 then
  begin
    AMemberInfo := FKeyMembers[0];
    AValue := AMemberInfo.GetValue(AObject);
    Result := GetValueHash(AValue);
  end
  else
  begin
    SetLength(AValues, FKeyMembers.Count);
    for I := 0 to FKeyMembers.Count - 1 do
    begin
      AMemberInfo := FKeyMembers[I];
      AValues[I] := AMemberInfo.GetValue(AObject);
    end;
    Result := GetHash(AValues);
  end;
end;

procedure TdxObjectFinder.PopulateKeyMembers;
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := EntityManager.GetEntityInfo(FEMFClass);
  FKeyMembers.AddRange(AEntityInfo.KeyAttributes);
  if FKeyMembers.Count = 0 then
  begin
    FKeyMembers.AddRange(AEntityInfo.MemberAttributes);
  end;
end;

function TdxObjectFinder.HasKey(AObject: TObject; const AKey: TValue): Boolean;
var
  AMemberInfo: TdxMappingMemberInfo;
  AValue: TValue;
begin
  AMemberInfo := FKeyMembers[0];
  AValue := AMemberInfo.GetValue(AObject);
  Result := AValue.Equals(AKey);
end;

function TdxObjectFinder.HasKey(AObject: TObject; const AKeys: array of TValue): Boolean;
var
  I, ACount: Integer;
  AMemberInfo: TdxMappingMemberInfo;
  AValue: TValue;
begin
  ACount := Min(Length(AKeys), FKeyMembers.Count);
  for I := 0 to ACount - 1 do
  begin
    AMemberInfo := FKeyMembers[I];
    AValue := AMemberInfo.GetValue(AObject);
    if not AValue.Equals(AKeys[I]) then
      Exit(False);
  end;
  Result := True;
end;

function TdxObjectFinder.HasKey<TKey>(AObject: TObject; const AKey: TKey): Boolean;
var
  AMemberInfo: TdxMappingMemberInfo;
  AValue: TValue;
begin
  AMemberInfo := FKeyMembers[0];
  AValue := AMemberInfo.GetValue(AObject);
  Result := TComparer<TKey>.Default.Compare(AValue.AsType<TKey>, AKey) = 0;
end;

{ TdxCustomEntityPool<T> }

constructor TdxCustomEntityPool<T>.Create;
begin
  inherited Create;
  FObjectPool := TObjectDictionary<T, TObject>.Create([doOwnsValues], GetComparer);
  FObjectKeys := TDictionary<Pointer, T>.Create;
end;

destructor TdxCustomEntityPool<T>.Destroy;
begin
  FreeAndNil(FObjectKeys);
  FreeAndNil(FObjectPool);
  inherited Destroy;
end;

procedure TdxCustomEntityPool<T>.Add(const AKey: T; AObject: TObject);
begin
  ObjectPool.Add(AKey, AObject);
  ObjectKeys.Add(AObject, AKey);
end;

procedure TdxCustomEntityPool<T>.Extract(AObject: TObject);
var
  AKeys: T;
begin
  if FObjectKeys.TryGetValue(AObject, AKeys) then
  begin
    FObjectKeys.ExtractPair(AObject);
    FObjectPool.ExtractPair(AKeys);
  end;
end;

function TdxCustomEntityPool<T>.GetComparer: TEqualityComparer<T>;
begin
  Result := nil;
end;

function TdxCustomEntityPool<T>.IsNewObject(AObject: TObject): Boolean;
begin
  Result := not ObjectKeys.ContainsKey(AObject);
end;

{ TdxIntegerKeyEntityPool1 }

function TdxIntegerKeyEntityPool.Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean;
var
  APoolKey: Integer;
begin
  APoolKey := AKeys[0].AsInteger;
  if ObjectPool.ContainsKey(APoolKey) then
    Exit(False);
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxIntegerKeyEntityPool.Add(AObject: TObject; AIsLoaded: Boolean): Boolean;
var
  APoolKey: Integer;
begin
  if ObjectKeys.ContainsKey(AObject) then
    Exit(False);
  APoolKey := EntityInfo.KeyProperty.GetValue(AObject).AsInteger;
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxIntegerKeyEntityPool.GetByKey(const AKeys: TArray<TValue>): TObject;
var
  AKey: Integer;
begin
  AKey := AKeys[0].AsInteger;
  ObjectPool.TryGetValue(AKey, Result)
end;

{ TdxInt64KeyEntityPool }

function TdxInt64KeyEntityPool.Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean;
var
  APoolKey: Int64;
begin
  APoolKey := AKeys[0].AsOrdinal;
  if ObjectPool.ContainsKey(APoolKey) then
    Exit(False);
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxInt64KeyEntityPool.Add(AObject: TObject; AIsLoaded: Boolean): Boolean;
var
  APoolKey: Int64;
begin
  if ObjectKeys.ContainsKey(AObject) then
    Exit(False);
  APoolKey := EntityInfo.KeyProperty.GetValue(AObject).AsOrdinal;
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxInt64KeyEntityPool.GetByKey(const AKeys: TArray<TValue>): TObject;
var
  AKey: Int64;
begin
  AKey := AKeys[0].AsOrdinal;
  ObjectPool.TryGetValue(AKey, Result)
end;

{ TdxSimpleKeyEntityPool<T> }

function TdxSimpleKeyEntityPool<T>.Add(AObject: TObject; AIsLoaded: Boolean): Boolean;
var
  APoolKey: T;
begin
  if ObjectKeys.ContainsKey(AObject) then
    Exit(False);
  EntityInfo.KeyProperty.GetValue(AObject).TryAsType<T>(APoolKey);
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxSimpleKeyEntityPool<T>.Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean;
var
  APoolKey: T;
begin
  AKeys[0].TryAsType<T>(APoolKey);
  if ObjectPool.ContainsKey(APoolKey) then
    Exit(False);
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxSimpleKeyEntityPool<T>.GetByKey(const AKeys: TArray<TValue>): TObject;
var
  AKey: T;
begin
  if AKeys[0].TryAsType<T>(AKey) then
    ObjectPool.TryGetValue(AKey, Result)
  else
    Result := nil;
end;

{ TdxGUIDComparer }

function TdxGUIDComparer.Equals(const Left, Right: TGUID): Boolean;
begin
  Result := Left = Right;
end;

function TdxGUIDComparer.GetHashCode(const Value: TGUID): Integer;
begin
  Result := GetValueHash(Value);
end;

{ TdxGUIDKeyEntityPool }

function TdxGUIDKeyEntityPool.GetComparer: TEqualityComparer<TGUID>;
begin
  Result := TdxGUIDComparer.Create;
end;

{ TdxEntityPool }

function TdxEntityPool.Add(const AKeys: TArray<TValue>; AObject: TObject): Boolean;
var
  APoolKey: TBytes;
begin
  APoolKey := KeysToPoolKey(AKeys);
  if ObjectPool.ContainsKey(APoolKey) then
    Exit(False);
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxEntityPool.Add(AObject: TObject; AIsLoaded: Boolean = False): Boolean;
var
  APoolKey: TBytes;
begin
  if ObjectKeys.ContainsKey(AObject) then
    Exit(False);
  APoolKey := KeysToPoolKey(EntityInfo.KeyProperty.GetValues(AObject));
  Add(APoolKey, AObject);
  Result := True;
end;

function TdxEntityPool.GetByKey(const AKeys: TArray<TValue>): TObject;
begin
  ObjectPool.TryGetValue(KeysToPoolKey(AKeys), Result);
end;

function TdxEntityPool.GetComparer: TEqualityComparer<TBytes>;
begin
  Result := TdxBytesComparer.Create;
end;

function TdxEntityPool.GetKeys(AObject: TObject): TArray<TValue>;
begin
  if AObject.ClassType <> EntityInfo.ClassAttributes.PersistentClass then
    raise EInvalidArgument.Create('');
  Result := EntityInfo.KeyProperty.GetValues(AObject);
end;

class function TdxEntityPool.KeysToPoolKey(const AKeys: TArray<TValue>): TBytes;
var
  AStream: TBytesStream;
  I: Integer;
begin
  AStream := TBytesStream.Create;
  try
    AStream.Size := $400;
    for I := 0 to Length(AKeys) - 1 do
      SaveToStream(AStream, AKeys[I]);
    Result := AStream.Bytes;
    SetLength(Result, AStream.Position);
  finally
    AStream.Free;
  end;
end;

class procedure TdxEntityPool.SaveToStream(AStream: TStream; const AValue: TValue);
var
  ALength: Integer;
  ABuffer: TBytes;
  ATypeInfo: PTypeInfo;
  AUnderlyingValue: TValue;
begin
  case AValue.Kind of
    tkUnknown:
      ;
    tkInteger:
      AStream.WriteData(AValue.AsInteger);
    tkFloat:
      if AValue.IsSingle then
        AStream.WriteData(AValue.AsSingle)
      else
        AStream.WriteData(AValue.AsDouble);
    tkString, tkLString, tkUString, tkWString:
      begin
        ABuffer := TEncoding.UTF8.GetBytes(AValue.AsString);
        AStream.Write(ABuffer[0], Length(ABuffer));
      end;
    tkRecord{$IFDEF DELPHI103}, tkMRecord{$ENDIF}:
      begin
        ATypeInfo := AValue.TypeInfo;
        if (ATypeInfo = System.TypeInfo(TGUID)) or (ATypeInfo = System.TypeInfo(TTimeSpan)) or
          (ATypeInfo = System.TypeInfo(TBCD)) then
        begin
          ALength := AValue.DataSize;
          AStream.Write(AValue.GetReferenceToRawData^, ALength);
        end
        else if IsNullableType(ATypeInfo) then
        begin
          TryGetUnderlyingValue(AValue, AUnderlyingValue);
          SaveToStream(AStream, AUnderlyingValue);
        end;
      end;
    tkChar, tkWChar, tkEnumeration, tkInt64:
      AStream.WriteData(AValue.AsOrdinal);
  else
    NotImplemented;
  end;
end;

{ TdxEntityPoolClasses }

constructor TdxEntityPoolClasses.Create;
begin
  inherited Create;
  FPool := TObjectDictionary<TClass, TdxCustomEntityPool>.Create([doOwnsValues]);
end;

destructor TdxEntityPoolClasses.Destroy;
begin
  FreeAndNil(FPool);
  inherited Destroy;
end;

class function TdxEntityPoolClasses.CreatePool(AEntityInfo: TdxEntityInfo): TdxCustomEntityPool;
begin
  if not AEntityInfo.KeyProperty.IsCompositeKey then
    case AEntityInfo.KeyProperty.Member.TypeKind of
      tkInteger:
        Exit(TdxIntegerKeyEntityPool.Create);
      tkRecord:
        if AEntityInfo.KeyProperty.Member.RttiType.Handle = TypeInfo(TGUID) then
          Exit(TdxGUIDKeyEntityPool.Create);
      tkChar, tkWChar, tkEnumeration, tkInt64:
        Exit(TdxInt64KeyEntityPool.Create);
      tkUString:
        Exit(TdxStringKeyEntityPool.Create);
    end;
  Result := TdxEntityPool.Create;
end;

function TdxEntityPoolClasses.AppendObject(const AKeys: TArray<TValue>; AObject: TObject; AIsLoaded: Boolean): Boolean;
var
  APool: TdxCustomEntityPool;
begin
  APool := GetPool(AObject.ClassType);
  Result := APool.Add(AKeys, AObject);
end;

function TdxEntityPoolClasses.AppendObject(AObject: TObject; AIsLoaded: Boolean = False): Boolean;
var
  APool: TdxCustomEntityPool;
begin
  APool := GetPool(AObject.ClassType);
  Result := APool.Add(AObject, AIsLoaded);
end;

procedure TdxEntityPoolClasses.ExtractObject(AObject: TObject);
var
  APool: TdxCustomEntityPool;
begin
  APool := GetPool(AObject.ClassType);
  APool.Extract(AObject);
end;

function TdxEntityPoolClasses.FindObjectByKey(AClass: TClass; const AKeys: TArray<TValue>): TObject;
begin
  Result := GetPool(AClass).GetByKey(AKeys);
end;

function TdxEntityPoolClasses.GetPool(AClass: TClass): TdxCustomEntityPool;
var
  AEntityInfo: TdxEntityInfo;
begin
  if not FPool.TryGetValue(AClass, Result) then
  begin
    AEntityInfo := EntityManager.GetEntityInfo(AClass);
    if AEntityInfo = nil then
      raise EdxNoEntityInfoException.CreateFmt(sdxClassIsNotEntity, [AClass.ClassName]);
    Result := CreatePool(AEntityInfo);
    Result.EntityInfo := AEntityInfo;
    FPool.Add(AClass, Result);
  end;
end;

function TdxEntityPoolClasses.IsNewObject(AObject: TObject): Boolean;
var
  APool: TdxCustomEntityPool;
begin
  APool := GetPool(AObject.ClassType);
  Result := APool.IsNewObject(AObject);
end;

initialization

finalization
  FreeAndNil(FObjectFinder);
end.

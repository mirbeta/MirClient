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

unit dxEMF.Attributes;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Classes, SysUtils,
  dxEMF.Serializer,
  dxEMF.Types;

type

  { EntityAttribute }

  EntityAttribute = class(TCustomAttribute);

  { SchemaNameAttribute }

  SchemaNameAttribute = class(TCustomAttribute)
  strict private
    FSchemaName: string;
  public
    constructor Create(const ASchemaName: string);
    property SchemaName: string read FSchemaName;
  end;

  { AutomappingAttribute }

  AutomappingAttribute = class(TCustomAttribute);

  { TableAttribute }

  TableAttribute = class(TCustomAttribute)
  strict private
    FTableName: string;
    FSchemaName: string;
  public
    constructor Create(const ATableName: string); overload;
    constructor Create(const ASchemaName, ATableName: string); overload;
    property SchemaName: string read FSchemaName;
    property TableName: string read FTableName;
  end;

  { InheritanceAttribute }

  InheritanceAttribute = class(TCustomAttribute)
  strict private
    FMapInheritance: TdxMapInheritanceType;
  public
    constructor Create; overload;
    constructor Create(AMapInheritance: TdxMapInheritanceType); overload;
    property MapInheritance: TdxMapInheritanceType read FMapInheritance;
  end;


  { DiscriminatorColumnAttribute }

  DiscriminatorColumnAttribute = class(TCustomAttribute)
  strict private
    FDiscriminatorType: TdxDiscriminatorType;
    FDiscriminatorColumn: string;
    FSize: Integer;
  public
    constructor Create; overload;
    constructor Create(const AColumnName: string; ADiscriminatorType: TdxDiscriminatorType = TdxDiscriminatorType.Integer;
      ASize: Integer = 0); overload;
    property DiscriminatorType: TdxDiscriminatorType read FDiscriminatorType;
    property DiscriminatorColumn: string read FDiscriminatorColumn;
    property Size: Integer read FSize;
  end;

  { DiscriminatorAttribute }

  DiscriminatorAttribute = class(TCustomAttribute)
  strict private
    FValue: Variant;
  public
    constructor Create(const AValue: string); overload;
    constructor Create(AValue: Integer); overload;
    property Value: Variant read FValue;
  end;

  { ColumnAttribute }

  ColumnAttribute = class(TCustomAttribute)
  strict private
    FColumnName: string;
  public
    constructor Create(const AColumnName: string); overload;
    property ColumnName: string read FColumnName;
  end;

  { IndexesAttribute }

  IndexesAttribute = class(TCustomAttribute)
  strict private
    FColumns: TArray<string>;
  public
    constructor Create(const AColumns: string);
    property Columns: TArray<string> read FColumns;
  end;

  { IndexedAttribute  }

  IndexedAttribute = class(TCustomAttribute);

  { UniqueAttribute }

  UniqueAttribute = class(TCustomAttribute);

  { SizeAttribute }

  SizeAttribute = class(TCustomAttribute)
  strict private
    FSize: Integer;
  public
    constructor Create(ASize: Integer);
    property Size: Integer read FSize;
  end;

  { ReadOnlyAttribute }

  ReadOnlyAttribute = class(TCustomAttribute);

  { VirtualColumnAttribute }

  VirtualColumnAttribute = class(TCustomAttribute);


  { NullableAttribute }

  NullableAttribute = class(TCustomAttribute);

  { DefaultAttribute }
{$IFDEF DELPHIXE3}
  DefaultAttribute = Classes.DefaultAttribute;
{$ELSE}
  DefaultAttribute = class(TCustomAttribute)
  strict private
    FValue: Variant;
  public
    constructor Create(const ADefaultValue: Boolean); overload;
    constructor Create(const ADefaultValue: Char); overload;
    constructor Create(const ADefaultValue: Integer); overload;
    constructor Create(const ADefaultValue: Cardinal); overload;
    constructor Create(const ADefaultValue: Int64); overload;
    constructor Create(const ADefaultValue: UInt64); overload;
    constructor Create(const ADefaultValue: String); overload;
    constructor Create(const ADefaultValue: Extended); overload;
    property Value: Variant read FValue;
  end;
{$ENDIF}

  { KeyAttribute }

  KeyAttribute = class(TCustomAttribute)
  strict private
    FMemberNames: TArray<string>;
  public
    constructor Create(const AFieldNames: string); overload;
    property MemberNames: TArray<string> read FMemberNames;
  end;

  { NoForeignKeyAttribute }

  NoForeignKeyAttribute = class(TCustomAttribute);

  { GeneratorAttribute }

  GeneratorAttribute = class(TCustomAttribute)
  strict private
    FGeneratorType: TdxGeneratorType;
    FSource: string;
  public
    constructor Create(AGeneratorType: TdxGeneratorType); overload;
    // Sequence
    constructor Create(AGeneratorType: TdxGeneratorType; const ASequenceName: string); overload;
    function CreateGenerator: TdxGenerator;
    property GeneratorType: TdxGeneratorType read FGeneratorType;
    property SequenceName: string read FSource;
  end;

  { NonPersistentAttribute }

  NonPersistentAttribute = class(TCustomAttribute);

  { AssociationAttribute }

  AssociationAttribute = class(TCustomAttribute)
  strict private
    FAssociationName: string;
  public
    constructor Create(const AAssociationName: string); overload;
    property AssociationName: string read FAssociationName;
  end;

  { AggregatedAttribute }

  AggregatedAttribute = class(TCustomAttribute);


  { BlobAttribute }

  BlobAttribute = class(TCustomAttribute)
  strict private
    FSerializerClass: TdxCustomBlobSerializerClass;
  public
    constructor Create(ASerializerClass: TdxCustomBlobSerializerClass); overload;
    property SerializerClass: TdxCustomBlobSerializerClass read FSerializerClass;
  end;

  { DBTypeAttribute  }

  DBTypeAttribute = class(TCustomAttribute)
  strict private
    FDBColumnTypeName: string;
  public
    constructor Create(const ADBColumnTypeName: string);
    property DBColumnTypeName: string read FDBColumnTypeName;
  end;

  TCustomAttributeClass = class of TCustomAttribute;

implementation

uses
  dxStringHelper,
  dxEMF.Metadata,
  dxEMF.Utils;

{ TableAttribute }

constructor TableAttribute.Create(const ATableName: string);
begin
  inherited Create;
  FTableName := ATableName;
end;

constructor TableAttribute.Create(const ASchemaName, ATableName: string);
begin
  inherited Create;
  FSchemaName := ASchemaName;
  FTableName := ATableName;
end;

{ ColumnAttribute }

constructor ColumnAttribute.Create(const AColumnName: string);
begin
  inherited Create;
  FColumnName := AColumnName;
end;

{ KeyAttribute }

constructor KeyAttribute.Create(const AFieldNames: string);
begin
  inherited Create;
  FMemberNames := {$IFDEF DELPHIXE3}AFieldNames.Split([',', ';']);{$ELSE}TdxStringHelper.Split(AFieldNames, [',', ';']);{$ENDIF}
end;

{ GeneratorAttribute }

constructor GeneratorAttribute.Create(AGeneratorType: TdxGeneratorType);
begin
  inherited Create;
  FGeneratorType := AGeneratorType;
end;

constructor GeneratorAttribute.Create(AGeneratorType: TdxGeneratorType; const ASequenceName: string);
begin
  Create(AGeneratorType);
  FSource := ASequenceName;
end;

function GeneratorAttribute.CreateGenerator: TdxGenerator;
begin
  case GeneratorType of
    TdxGeneratorType.Sequence:
      Result := TdxGenerator.Create(TdxGeneratorType.Sequence, FSource);
  else
    Result := TdxGenerator.Create(GeneratorType);
  end;
end;

{ InheritanceAttribute }

constructor InheritanceAttribute.Create(AMapInheritance: TdxMapInheritanceType);
begin
  inherited Create;
  FMapInheritance := AMapInheritance;
end;

constructor InheritanceAttribute.Create;
begin
  Create(TdxMapInheritanceType.ParentTable);
end;

{ BlobAttribute }

constructor BlobAttribute.Create(ASerializerClass: TdxCustomBlobSerializerClass);
begin
  inherited Create;
  FSerializerClass := ASerializerClass;
end;

{ LengthAttribute }

constructor SizeAttribute.Create(ASize: Integer);
begin
  FSize := ASize;
end;

{ DefaultAttribute }

{$IFNDEF DELPHIXE3}
constructor DefaultAttribute.Create(const ADefaultValue: Integer);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: Cardinal);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: Boolean);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: Char);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: String);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: Extended);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: Int64);
begin
  inherited Create;
  FValue := ADefaultValue;
end;

constructor DefaultAttribute.Create(const ADefaultValue: UInt64);
begin
  inherited Create;
  FValue := ADefaultValue;
end;
{$ENDIF}

{ SchemaNameAttribute }

constructor SchemaNameAttribute.Create(const ASchemaName: string);
begin
  inherited Create;
  FSchemaName := ASchemaName;
end;

{ DiscriminatorAttribute }

constructor DiscriminatorColumnAttribute.Create;
begin
  Create(TdxDiscriminator.ObjectTypePropertyName);
end;

constructor DiscriminatorColumnAttribute.Create(const AColumnName: string;
  ADiscriminatorType: TdxDiscriminatorType; ASize: Integer);
begin
  inherited Create;
  FDiscriminatorColumn := AColumnName;
  FDiscriminatorType := ADiscriminatorType;
  FSize := ASize;
end;

{ DiscriminatorAttribute }

constructor DiscriminatorAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

constructor DiscriminatorAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

{ IndexesAttribute }

constructor IndexesAttribute.Create(const AColumns: string);
begin
  {$IFDEF DELPHIXE3}
  FColumns := AColumns.Split([',', ';']);
  {$ELSE}
  FColumns := TdxStringHelper.Split(AColumns, [',', ';']);
  {$ENDIF}
end;

{ DBTypeAttribute }

constructor DBTypeAttribute.Create(const ADBColumnTypeName: string);
begin
  inherited Create;
  FDBColumnTypeName := ADBColumnTypeName;
end;

{ AssociationAttribute }

constructor AssociationAttribute.Create(const AAssociationName: string);
begin
  inherited Create;
  FAssociationName := AAssociationName;
end;

end.

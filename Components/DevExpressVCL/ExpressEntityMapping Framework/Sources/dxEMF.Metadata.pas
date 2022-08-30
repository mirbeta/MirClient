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

unit dxEMF.Metadata;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Classes, Generics.Defaults, Generics.Collections, dxHash,
  SysUtils, Rtti, TypInfo, DB,
  dxCoreClasses, dxTypeHelpers,
  dxEMF.Attributes,
  dxEMF.Types,
  dxEMF.Serializer,
  dxEMF.DB.Model;

type

  IdxMappingClassInfo = interface;

  { IdxClassInfoMember }

  IdxMappingMemberInfo = interface
  ['{6534BDBF-41AD-42F2-8274-110AC098469A}']
    function RegisterField(const AFieldName: string): IdxMappingMemberInfo;
    function RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;

    function Aggregated: IdxMappingMemberInfo;
    function Association(const AAssociationName: string = ''): IdxMappingMemberInfo;
    function Blob: IdxMappingMemberInfo; overload;
    function Blob(ASerializerClass: TdxCustomBlobSerializerClass): IdxMappingMemberInfo; overload;
    function Column(const AColumnName: string): IdxMappingMemberInfo;
    function DBType(const ADBColumnTypeName: string): IdxMappingMemberInfo;
    function Default(const ADefaultValue: Variant): IdxMappingMemberInfo;
    function Generator(AGeneratorType: TdxGeneratorType; const ASequenceName: string = ''): IdxMappingMemberInfo;
    function Indexed: IdxMappingMemberInfo;
    function Key: IdxMappingMemberInfo;
    function NoForeignKey: IdxMappingMemberInfo;
    function NonPersistent: IdxMappingMemberInfo;
    function Nullable: IdxMappingMemberInfo;
    function ReadOnly: IdxMappingMemberInfo;
    function Size(ASize: Integer): IdxMappingMemberInfo;
    function Unique: IdxMappingMemberInfo;
    function VirtualColumn: IdxMappingMemberInfo;
  end;

  { IdxClassInfo }

  IdxMappingClassInfo = interface
  ['{4C75F2A3-2B8A-469F-BCA6-D733A0E61F34}']
    function RegisterField(const AFieldName: string): IdxMappingMemberInfo;
    function RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;

    function Automapping: IdxMappingClassInfo;
    function Discriminator(AValue: Integer): IdxMappingClassInfo; overload;
    function Discriminator(const AValue: string): IdxMappingClassInfo; overload;
    function DiscriminatorColumn: IdxMappingClassInfo; overload;
    function DiscriminatorColumn(const AColumnName: string): IdxMappingClassInfo; overload;
    function DiscriminatorColumn(const AColumnName: string; ADiscriminatorType: TdxDiscriminatorType;
      ASize: Integer = 0): IdxMappingClassInfo; overload;
    function Indexes(const AColumns: string): IdxMappingClassInfo;
    function Inheritance(AMapInheritance: TdxMapInheritanceType): IdxMappingClassInfo;
    function Key(const AFieldNames: string): IdxMappingClassInfo;
    function SchemaName(const ASchemaName: string): IdxMappingClassInfo;
    function Table(const ATableName: string): IdxMappingClassInfo;
  end;

  TdxEntityInfo = class;

  { TdxDiscriminator }

  TdxDiscriminator = record
  public const
    ObjectTypePropertyName = 'ObjectType';
    ObjectTypePropertySize = 128;
  public
    HasValue: Boolean;
    DiscriminatorType: TdxDiscriminatorType;
    ColumnName: string;
    ColumnSize: Integer;
    constructor Create(ADiscriminatorType: TdxDiscriminatorType; const AColumnName: string; AColumnSize: Integer = 0); overload;
    constructor Create(AAttribute: DiscriminatorColumnAttribute); overload;
    class function CreateDefault: TdxDiscriminator; static;
  end;

  { TdxAssociationAttributeInfo }

  TdxAssociationAttributeInfo = record
  private
    FAssociationName: string;
  public
    constructor Create(const AAssociationName: string);
    property AssociationName: string read FAssociationName;
  end;

  { TdxIndexes }

  TdxIndexes = class
  public type
    TIndex = record
      Columns: TArray<string>;
      IsUnique: Boolean;
      constructor Create(const AColumns: TArray<string>; AIsUnique: Boolean);
    end;
  strict private
    FIndexColumns: TList<TIndex>;
    function GetCount: Integer; inline;
    function GetIndexColumns(Index: Integer): TIndex;
  protected
    procedure Add(const AColumns: TArray<string>; AIsUnique: Boolean = False); overload;
    procedure Add(const AColumn: string; AIsUnique: Boolean = False); overload;
  public
    constructor Create;
    destructor Destroy; override;
    property IndexColumns[Index: Integer]: TIndex read GetIndexColumns; default;
    property Count: Integer read GetCount;
  end;

  { TdxAttributeInfo }

  TdxAttributeInfo = class//(TcxIUnknownObject)
  strict private
    FOwner: TdxEntityInfo;
    FName: string;
    function GetIsDataMember: Boolean; inline;
  protected
    procedure Assign(ASource: TdxAttributeInfo); virtual;
    procedure SetName(const AName: string);
    function FindAttribute(AClass: TCustomAttributeClass): TCustomAttribute; virtual; abstract;
    function GetActualName: string; virtual;
  public
    AttributeType: TdxAttributeType;
    RttiType: TRttiType;
    TypeKind: TTypeKind;
    Attributes: TdxAttributes;
    constructor Create(AOwner: TdxEntityInfo);
    property AttributeName: string read FName;
    property ActualName: string read GetActualName;
    property IsDataMember: Boolean read GetIsDataMember;
    property Owner: TdxEntityInfo read FOwner;
  end;

  { TdxMappingClassInfo }

  TdxMappingClassInfo = class(TdxAttributeInfo, IdxMappingClassInfo)
  strict private
    FRefCount: Integer;
    FClass: TClass;
    FKeyMemberNames: TList<string>;
    FDiscriminatorValue: Variant;
    FIndexes: TdxIndexes;
    FIdEntity: TdxEntityInfo;
    FSchemaName: string;
    FTableName: string;
    function GetTableName: string;
    function GetInheritance: Boolean; inline;
    function GetIsParentTable: Boolean;
    function GetPersistentBaseClass: TClass;
    function GetIsPersistent: Boolean;
    function GetIdEntity: TdxEntityInfo;
  private
    FDiscriminatorInfo: TdxDiscriminator;
    function GetEntityName: string;
    function GetIsOwnTable: Boolean;
    function GetIsDiscriminatorColumn: Boolean;
  protected
    FIsCodeRegistry: Boolean;
    procedure Assign(ASource: TdxAttributeInfo); override;
    function FindAttribute(AClass: TCustomAttributeClass): TCustomAttribute; override;
    procedure SetSchemaName(const ASchemaName: string);
    property IsDiscriminatorColumn: Boolean read GetIsDiscriminatorColumn;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Automapping: IdxMappingClassInfo;
    function Discriminator(const AValue: string): IdxMappingClassInfo; overload;
    function Discriminator(AValue: Integer): IdxMappingClassInfo; overload;
    function DiscriminatorColumn: IdxMappingClassInfo; overload;
    function DiscriminatorColumn(const AColumnName: string): IdxMappingClassInfo; overload;
    function DiscriminatorColumn(const AColumnName: string; ADiscriminatorType: TdxDiscriminatorType; ASize: Integer = 0): IdxMappingClassInfo; overload;
    function TableIndexes(const AColumns: string): IdxMappingClassInfo;
    function Inheritance(AMapInheritance: TdxMapInheritanceType): IdxMappingClassInfo;
    function Key(const AFieldNames: string): IdxMappingClassInfo; overload;
    function RegisterField(const AFieldName: string): IdxMappingMemberInfo;
    function RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;
    function TableSchemaName(const ASchemaName: string): IdxMappingClassInfo;
    function Table(const ATableName: string): IdxMappingClassInfo;
    function IdxMappingClassInfo.Indexes = TableIndexes;
    function IdxMappingClassInfo.SchemaName = TableSchemaName;
  public
    MapInheritance: TdxMapInheritanceType;
    constructor Create(AOwner: TdxEntityInfo; AClass: TClass);
    destructor Destroy; override;
    property EntityName: string read GetEntityName;
    property TableName: string read GetTableName;
    property IsInheritance: Boolean read GetInheritance;
    property IsParentTable: Boolean read GetIsParentTable;
    property IsOwnTable: Boolean read GetIsOwnTable;
    property IsPersistent: Boolean read GetIsPersistent;
    property Indexes: TdxIndexes read FIndexes;
    property KeyMemberNames: TList<string> read FKeyMemberNames;
    property DiscriminatorInfo: TdxDiscriminator read FDiscriminatorInfo write FDiscriminatorInfo;
    property DiscriminatorValue: Variant read FDiscriminatorValue write FDiscriminatorValue;
    property PersistentBaseClass: TClass read GetPersistentBaseClass;
    property PersistentClass: TClass read FClass;
    property IdEntity: TdxEntityInfo read GetIdEntity;
    property SchemaName: string read FSchemaName;
  end;

  { TdxMappingMemberInfo }

  TdxMappingMemberInfo = class(TdxAttributeInfo, IdxMappingMemberInfo)
  public const
    OrdinalTypes = [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
    StringTypes = [tkString, tkLString, tkWString, tkUString];
  strict private
    FAssociatedMember: TdxMappingMemberInfo;
    FIsAssociationList: Boolean;
    FIsDynamicArrayType: Boolean;
    FReferenceType: TdxEntityInfo;
    FCollectionElementType: TdxEntityInfo;
    FMemberName: string;
    FSerializer: TdxCustomBlobSerializer;
    FValueConverter: TdxValueConverter;
    FSubMembers: TArray<TdxMappingMemberInfo>;
    FFieldType: TdxNullableValue<TFieldType>;
    function GetAggregated: Boolean;
    function GetAssociatedMember: TdxMappingMemberInfo;
    function GetCollectionElementType: TdxEntityInfo;
    function GetColumnName: string;
    function GetConverter: TdxValueConverter;
    function GetFieldType: TFieldType;
    function GetIsAliased: Boolean;
    function GetIsAssociation: Boolean;
    function GetIsAssociationObject: Boolean;
    function GetIsAutoGenerate: Boolean;
    function GetIsBLOB: Boolean;
    function GetIsCollection: Boolean;
    function GetIsColumn: Boolean;
    function GetIsDelayed: Boolean;
    function GetIsEnumeration: Boolean;
    function GetIsExpandableToPersistent: Boolean;
    function GetIsIdentity: Boolean;
    function GetIsKey: Boolean; inline;
    function GetIsManyToMany: Boolean;
    function GetIsNullable: Boolean;
    function GetIsReadable: Boolean;
    function GetIsReadOnly: Boolean;
    function GetIsReference: Boolean;
    function GetIsRequired: Boolean;
    function GetIsSerialize: Boolean; inline;
    function GetIsSupportType: Boolean;
    function GetIsUnique: Boolean;
    function GetIsVirtualColumn: Boolean;
    function GetIsWritable: Boolean;
    function GetMappingField: string;
    function GetMemberName: string; inline;
    function GetMemberType: TRttiType;
    function GetParentClass: TClass;
  protected
    FID: Integer;
    FMember: TRttiMember;
    FAssociationAttributeInfo: TdxNullableValue<TdxAssociationAttributeInfo>;
    procedure Assign(ASource: TdxAttributeInfo); override;
    function GetActualName: string; override;
    function FindAttribute(AClass: TCustomAttributeClass): TCustomAttribute; override;
    function GetPersistentClass: TClass; virtual;
    function GetIsPersistent: Boolean; virtual;
    function GetIsStruct: Boolean; virtual;
    function GetIsLoadable: Boolean; virtual;
    procedure SetFieldType(AFieldType: TFieldType);
    procedure SetIsDynamicArrayType(const Value: Boolean);
    procedure SetMember(const Value: TRttiMember); virtual;
    procedure SetMemberName(const AValue: string);
    procedure SetMemberValue(AObject: TObject; const AValue: TValue); inline;
    procedure SetSerializer(ASerializerClass: TdxCustomBlobSerializerClass);
    function IsMember(const AMemberName: string): Boolean;
    procedure UpdateReferenceType;
    class function GetGenericIListTypeArgument(AElementType: TRttiType): TRttiType; static;
    class function Context: TRttiContext; static; inline;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Aggregated: IdxMappingMemberInfo;
    function Association(const AAssociationName: string): IdxMappingMemberInfo;
    function Blob: IdxMappingMemberInfo; overload;
    function Blob(ASerializerClass: TdxCustomBlobSerializerClass): IdxMappingMemberInfo; overload;
    function Column(const AColumnName: string): IdxMappingMemberInfo;
    function Size(ASize: Integer): IdxMappingMemberInfo;
    function DBType(const ADBColumnTypeName: string): IdxMappingMemberInfo;
    function Default(const ADefaultValue: Variant): IdxMappingMemberInfo;
    function Generator(AGeneratorType: TdxGeneratorType; const ASequenceName: string = ''): IdxMappingMemberInfo;
    function Indexed: IdxMappingMemberInfo;
    function Key: IdxMappingMemberInfo; overload;
    function NoForeignKey: IdxMappingMemberInfo;
    function NonPersistent: IdxMappingMemberInfo;
    function Nullable: IdxMappingMemberInfo;
    function ReadOnly: IdxMappingMemberInfo;
    function RegisterField(const AFieldName: string): IdxMappingMemberInfo;
    function RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;
    function Unique: IdxMappingMemberInfo;
    function VirtualColumn: IdxMappingMemberInfo;
  public
    DBColumnSize: Integer;
    DefaultValue: Variant;
    DBColumnTypeName: string;
    DBGenerator: TdxGenerator;
    destructor Destroy; override;
    function ConvertArray(ABytes: TBytes): TValue;
    function GetValue(AObject: TObject): TValue; virtual;
    procedure SetValue(AObject: TObject; const AValue: TValue); virtual;
    function GetMappingClass(ABranch: TdxEntityInfo): TdxEntityInfo;
    function IsMappingClass(ABranch: TdxEntityInfo): Boolean;
    function IsType(ATypeInfo: PTypeInfo): Boolean; virtual;

    property AssociatedMember: TdxMappingMemberInfo read GetAssociatedMember;
    property CollectionElementType: TdxEntityInfo read GetCollectionElementType;
    property ColumnName: string read GetColumnName;
    property Converter: TdxValueConverter read GetConverter;
    property FieldType: TFieldType read GetFieldType;
    property IsAggregated: Boolean read GetAggregated;
    property IsAliased: Boolean read GetIsAliased;
    property IsAssociation: Boolean read GetIsAssociation;
    property IsAssociationList: Boolean read FIsAssociationList write FIsAssociationList;
    property IsAssociationObject: Boolean read GetIsAssociationObject;
    property IsAutoGenerate: Boolean read GetIsAutoGenerate;
    property IsBLOB: Boolean read GetIsBLOB;
    property IsCollection: Boolean read GetIsCollection;
    property IsColumn: Boolean read GetIsColumn;
    property IsDelayed: Boolean read GetIsDelayed;
    property IsDynamicArrayType: Boolean read FIsDynamicArrayType write SetIsDynamicArrayType;
    property IsEnumeration: Boolean read GetIsEnumeration;
    property IsExpandableToPersistent: Boolean read GetIsExpandableToPersistent;
    property IsIdentity: Boolean read GetIsIdentity;
    property IsKey: Boolean read GetIsKey;
    property IsLoadable: Boolean read GetIsLoadable;
    property IsManyToMany: Boolean read GetIsManyToMany;
    property IsNullable: Boolean read GetIsNullable;
    property IsPersistent: Boolean read GetIsPersistent;
    property IsReadable: Boolean read GetIsReadable;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property IsReference: Boolean read GetIsReference;
    property IsRequired: Boolean read GetIsRequired;
    property IsSerialize: Boolean read GetIsSerialize;
    property IsStruct: Boolean read GetIsStruct;
    property IsSupportType: Boolean read GetIsSupportType;
    property IsUnique: Boolean read GetIsUnique;
    property IsVirtualColumn: Boolean read GetIsVirtualColumn;
    property IsWritable: Boolean read GetIsWritable;
    property MappingField: string read GetMappingField;
    property Member: TRttiMember read FMember write SetMember;
    property MemberName: string read GetMemberName;
    property MemberType: TRttiType read GetMemberType;
    property ParentClass: TClass read GetParentClass;
    property ReferenceType: TdxEntityInfo read FReferenceType;
    property Serializer: TdxCustomBlobSerializer read FSerializer;
    property SubMembers: TArray<TdxMappingMemberInfo> read FSubMembers;
  end;

  { TdxMemberInfoList }

  TdxMemberInfoList = class(TList<TdxMappingMemberInfo>)
  public
    function IndexOfName(const AMemberName: string): Integer;
  end;

  { TdxKeyMemberInfo }

  TdxKeyMemberInfo = class
  strict private
    FOwner: TdxEntityInfo;
    FKeyFieldCount: Integer;
    function GetMembers: TdxMemberInfoList; inline;
    function GetMember: TdxMappingMemberInfo; inline;
    function GetIsIdentity: Boolean;
  protected
    procedure CalcKeyFieldCount;
  public
    constructor Create(AOwner: TdxEntityInfo);
    function GetValue(AObject: TObject): TValue; inline;
    function GetValues(AObject: TObject): TArray<TValue>;
    procedure SetValue(AObject: TObject; const AValue: TValue);
    procedure SetValues(AObject: TObject; const AKeyValues: TArray<TValue>);
    property KeyFieldCount: Integer read FKeyFieldCount;
    function IsCompositeKey: Boolean;
    property Owner: TdxEntityInfo read FOwner;
    property IsIdentity: Boolean read GetIsIdentity;
    property Member: TdxMappingMemberInfo read GetMember;
    property Members: TdxMemberInfoList read GetMembers;
  end;

  { TdxServiceMember }

  TdxServiceMember = class abstract(TdxMappingMemberInfo)
  protected
    function GetActualName: string; override;
  public
    constructor Create(AOwner: TdxEntityInfo);
  end;

  { TdxDiscriminatorMember }

  TdxDiscriminatorMember = class(TdxServiceMember)
  private
    FDiscriminatorValue: TValue;
    FDiscriminator: TdxDiscriminator;
    FPersistentClass: TClass;
    function GetAsInteger: Integer;
    function GetAsString: string;
    procedure SetDiscriminator(const Value: TdxDiscriminator);
  protected
    function GetPersistentClass: TClass; override;
  public
    constructor Create(AOwner: TdxEntityInfo);
    function GetValue(AObject: TObject): TValue; override;
    property AsInteger: Integer read GetAsInteger;
    property AsString: string read GetAsString;
    property DiscriminatorValue: TValue read FDiscriminatorValue write FDiscriminatorValue;
    property Discriminator: TdxDiscriminator read FDiscriminator write SetDiscriminator;
  end;

  { TdxServiceFields }

  TdxServiceFields = class(TList<TdxServiceMember>)
  strict private
    FDiscriminatorField: TdxDiscriminatorMember;
  protected
    procedure Notify(const Value: TdxServiceMember; Action: TCollectionNotification); override;
  public
    property DiscriminatorField: TdxDiscriminatorMember read FDiscriminatorField;
  end;

  { TdxMemberInfoCollection }

  TdxMemberInfoCollection = class(TdxMemberInfoList)
  strict private const
    UpCastSymbols: array[0..1] of Char = ('<', '>');
    NamespaceSplitters: array[0..1] of Char = ('.', '+');
  strict private
    FEntityInfo: TdxEntityInfo;
    FHasNonPersistent: Boolean;
  public
    constructor Create(AEntityInfo: TdxEntityInfo); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const AMembers: array of TdxMappingMemberInfo); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const APath: string); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const APath: string; AAddNonPersistent: Boolean); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const APath: string; AAddNonPersistent: Boolean;
      AThrowOnError: Boolean); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const AMatches: TArray<string>; AAddNonPersistent: Boolean; AThrowOnError: Boolean); overload;
    class function ParsePath(AEntityInfo: TdxEntityInfo; const APath: string): TdxMemberInfoCollection; static;
    class function ParsePersistentPath(AEntityInfo: TdxEntityInfo; const APath: string): TdxMemberInfoCollection; static;
    class function FindMember(ACurrentEntityInfo: TdxEntityInfo; const AMatch: string): TdxMappingMemberInfo; static;
    class function SplitPath(const APath: string): TArray<string>; static;
    class function LastIndexOfSplittingDotInPath(const APath: string): Integer; static;
    class function GetSplitPartCount(const APath: string): Integer; static;
    class function TryResolveType(const AClassName: string; ARootEntityInfo: TdxEntityInfo; out AEntityInfo: TdxEntityInfo): Boolean; static;
    class function TryResolveTypeAlsoByShortName(const AClassName: string; ARootEntityInfo: TdxEntityInfo;
      out AEntityInfo: TdxEntityInfo): Boolean; static;
    procedure Remove(ACollection: TEnumerable<TdxMappingMemberInfo>); overload;
    function ToString: string; override;

    property HasNonPersistent: Boolean read FHasNonPersistent;
  end;

  { TdxMemberPathCollection }

  TdxMemberPathCollection = class(TObjectList<TdxMemberInfoCollection>);

  { TdxPropertyDescriptorCollection }

  TdxPropertyDescriptorCollection = class
  end;

  { TdxEntityInfo }

  TdxEntityInfo = class
  private type
    TAttributeInfoList = TObjectList<TdxAttributeInfo>;
    TMemberAttributes = TdxMemberInfoList;
    TKeyAttributes = class(TdxMemberInfoList)
    public
      function GetColumnsNames: TArray<string>;
    end;
  strict private
    FAssociationListProperties: TArray<TdxMappingMemberInfo>;
    FAttributeInfoList: TAttributeInfoList;
    FCachedPaths: TObjectDictionary<string, TdxMemberInfoCollection>;
    FClassAttribute: TdxMappingClassInfo;
    FHasDelayedProperties: Boolean;
    FIsParentTable: Boolean;
    FKeyAttributes: TKeyAttributes;
    FKeyProperty: TdxKeyMemberInfo;
    FMemberAttributes: TMemberAttributes;
    FMembers: TDictionary<string, TdxMappingMemberInfo>;
    FObjectProperties: TArray<TdxMappingMemberInfo>;
    FParentEntity: TdxNullableValue<TdxEntityInfo>;
    FPersistentProperties: TMemberAttributes;
    FPropertiesForInsert: TdxMemberInfoCollection;
    FPropertiesForUpdate: TdxMemberInfoCollection;
    FRegisterEntityProcessEnd: Boolean;
    FServiceFields: TdxServiceFields;
    FTable: TdxDBTable;
    FTableName: string;
    procedure CreateTable;
    function GetItems(AIndex: Integer): TdxAttributeInfo; inline;
    function GetCount: Integer; inline;
    function GetDBTable: TdxDBTable;
    function GetTableName: string;
    function IsAssignableToCore(AEntityInfo: TdxEntityInfo): Boolean;
    function GetOptimisticLockField: TdxMappingMemberInfo;
    function GetParentEntity: TdxEntityInfo;
    function GetPersistentProperties: TMemberAttributes;
    function GetOptimisticLockingBehavior: TdxOptimisticLockingBehavior;
    procedure InitServiceMembers;
    procedure InitAssociationListMembers;
    procedure InitObjectMembers;
    function GetFullName: string;
    function GetIsPersistent: Boolean;
  private
    function CreateMappingMemberInfo(AMember: TRttiMember): TdxMappingMemberInfo;
    function GetEntityUnitName: string;
  protected
    class function GetQualifiedClassName(AClass: TClass): string; overload; static; inline;
    class function IsEntity(AType: TRttiType): Boolean;
    procedure Add(AAttributeInfo: TdxAttributeInfo);
    function CanPersist: Boolean;
    procedure Clear;
    procedure CreateDiscriminator;
    function GetBaseEntityInfo: TdxEntityInfo;
    function GenerateDiscriminantValue: TValue;
    procedure EndRegisterEntity;
    function HasMember(AMember: TRttiMember): Boolean;
    procedure RegistrationFinalization;
    function ParsePath(const APath: string): TdxMemberInfoCollection;
    procedure SetKeyProperty(AValue: TdxKeyMemberInfo);
    procedure InitMembers;
    property HasDelayedProperties: Boolean read FHasDelayedProperties;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckAbstractReference;
    function GetEnumerator: TAttributeInfoList.TEnumerator;
    function GetPropertiesListForUpdateInsert(AObject: TObject;
      AIsUpdate, AAddDelayedReference: Boolean): TdxMemberInfoCollection;
    function GetKeyValue(AObject: TObject): TValue;
    function GetMemberValues(AObject: TObject): TArray<Variant>;
    function GetQualifiedEntityClassName: string;
    function HasModifications(AObject: TObject): Boolean;
    function FindMember(const AMemberName: string): TdxMappingMemberInfo;
    function FindMemberByColumnName(const AColumnName: string): TdxMappingMemberInfo;
    function TryGetValueByName(const AName: string; out AAttributeInfo: TdxAttributeInfo): Boolean;
    function ParsePersistentPath(const APath: string): TdxMemberInfoCollection;
    procedure InitAssociations(ASession: TComponent; AObject: TObject);
    procedure InitAssociation(ASession: TComponent; AObject: TObject; const AQueryable: IdxQueryable;
      const AMemberName: string = '');
    procedure InitSession(ASession: TComponent; AObject: TObject);
    function IsAssignableTo(AEntityInfo: TdxEntityInfo): Boolean;

    property AssociationListProperties: TArray<TdxMappingMemberInfo> read FAssociationListProperties;
    property BaseEntityInfo: TdxEntityInfo read GetBaseEntityInfo;
    property ClassAttributes: TdxMappingClassInfo read FClassAttribute;
    property CollectionProperties: TArray<TdxMappingMemberInfo> read FAssociationListProperties;
    property Count: Integer read GetCount;
    property DBTable: TdxDBTable read GetDBTable;
    property EntityUnitName: string read GetEntityUnitName;
    property FullName: string read GetFullName;
    property IsPersistent: Boolean read GetIsPersistent;
    property Items[AIndex: Integer]: TdxAttributeInfo read GetItems; default;
    property KeyAttributes: TKeyAttributes read FKeyAttributes;
    property KeyProperty: TdxKeyMemberInfo read FKeyProperty;
    property MemberAttributes: TMemberAttributes read FMemberAttributes;
    property Members: TDictionary<string, TdxMappingMemberInfo> read FMembers;
    property ObjectProperties: TArray<TdxMappingMemberInfo> read FObjectProperties;
    property OptimisticLockField: TdxMappingMemberInfo read GetOptimisticLockField;
    property OptimisticLockingBehavior: TdxOptimisticLockingBehavior read GetOptimisticLockingBehavior;
    property ParentEntity: TdxEntityInfo read GetParentEntity;
    property PersistentProperties: TMemberAttributes read GetPersistentProperties;
    property ServiceFields: TdxServiceFields read FServiceFields;
    property TableName: string read GetTableName;
  end;

  { TdxEntityManager }

  TdxEntityManager = class
  public const
    DefaultKeyName = 'Id';
  private type
    TEnumerator = TObjectDictionary<TClass, TdxEntityInfo>.TValueEnumerator;
  strict private class var
    FContext: TRttiContext;
  strict private
    FClassAttributes: TObjectDictionary<TClass, TdxEntityInfo>;
    FProcessingEntities: TDictionary<TClass, TdxEntityInfo>;
    function CreateNonPersistentObject(AClass: TClass): TObject;
  protected
    procedure BeginCreateEntityInfo(AClass: TClass; AEntityInfo: TdxEntityInfo);
    procedure EndCreateEntityInfo(AClass: TClass);
    function GetProcessingEntityInfo(AClass: TClass): TdxEntityInfo;
    procedure ForceRegistration(AEntityInfo: TdxEntityInfo);
    function IsProcessingEntityInfo(AClass: TClass): Boolean;
    procedure ProcessCreateEntityInfo(AEntityInfo: TdxEntityInfo; AClass: TClass);
    procedure RegisterEntityInfo(AEntityInfo: TdxEntityInfo);

    procedure AutoMappingAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
    procedure AppendInheritanceMembers(AEntityInfo: TdxEntityInfo);
    function CheckAttributes(AMember: TRttiMember; AAttributeInfo: TdxMappingMemberInfo): Boolean;
    function CheckAutoMappingKeys(AMember: TRttiField; AMemberInfo: TdxMappingMemberInfo): Boolean;
    procedure CheckColumnAttribute(AAttribute: TCustomAttribute; AAttributeInfo: TdxMappingMemberInfo);
    function CheckIdentity(AMember: TRttiMember): Boolean;
    procedure CheckTableName(AType: TRttiType; var AAttributeInfo: TdxAttributeInfo);
    function CreateEntityInfo(AClass: TClass): TdxEntityInfo;
    function GetMember(AClass: TClass; const AMemberName: string): TRttiMember;
    procedure FieldsAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
    procedure PropertyAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
    function SerializeMember(AAttributeInfo: TdxMappingMemberInfo): Boolean;

    class constructor Create;
    class destructor Destroy;
    class function GetQualifiedTableName(const AName: string): string; static;
    class function GetQualifiedFieldName(const AName: string): string; static;
    class function GetTableName(AEntityInfo: TdxEntityInfo): string; overload; static; inline;
    class property Context: TRttiContext read FContext;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator;
    function GetTableName(AClass: TClass): string; overload;

    function RegisterEntity(AClass: TClass): IdxMappingClassInfo;
    procedure RegisterEntities(const AClasses: array of TClass);
    procedure UnRegisterEntity(AClass: TClass);
    procedure UnRegisterEntities(const AClasses: array of TClass);

    function CreateObject(AClass: TClass): TObject;
    function GetEntityInfo(const AClassName: string): TdxEntityInfo; overload;
    function GetEntityInfo(AClass: TClass): TdxEntityInfo; overload;
    function GetEntityInfo(AObject: TObject): TdxEntityInfo; overload;
    function GetSimpleKey(AObject: TObject): TValue;
    function HasClass(AClass: TClass): Boolean;
    function IsEntity(AClass: TClass): Boolean;
  end;

function EntityManager: TdxEntityManager;

implementation

uses
  Variants, FmtBCD, StrUtils, SyncObjs,
  dxCore, dxStringHelper,
  dxEMF.Core,
  dxEMF.Utils,
  dxEMF.Core.DBHelper,
  dxEMF.Utils.Exceptions,
  dxEMF.Strs;

var
  FEntityManager: TdxEntityManager;

function EntityManager: TdxEntityManager;
begin
  if FEntityManager = nil then
    FEntityManager := TdxEntityManager.Create;
  Result := FEntityManager;
end;

function IsAssociationListType(ATypeInfo: PTypeInfo): Boolean;

  function GetEMFInterface(ATypeInfo: PTypeInfo): PTypeInfo;
  var
    ATypeData: PTypeData;
  begin
    {$IFDEF DELPHIXE3}
    ATypeData := ATypeInfo.TypeData;
    {$ELSE}
    ATypeData := GetTypeData(ATypeInfo);
    {$ENDIF}
    if (ATypeInfo = TypeInfo(IdxEMFCollection)) or (Pos('IdxEMFCollection', GetTypeName(ATypeInfo)) = 1) then
      Result := ATypeInfo
    else
    begin
      if ATypeData.IntfParent = nil then
        Result := nil
      else
        Result := GetEMFInterface(ATypeData.IntfParent^);
    end;
  end;

var
  ATypeData: PTypeData;
begin
  if ATypeInfo = nil then
    Exit(False);
  case ATypeInfo.Kind of
    tkClass:
      begin
        {$IFDEF DELPHIXE3}
        ATypeData := ATypeInfo.TypeData;
        {$ELSE}
        ATypeData := GetTypeData(ATypeInfo);
        {$ENDIF}
        Result := Supports(ATypeData.ClassType, IdxEMFCollection);
      end;
    tkInterface:
      Result := GetEMFInterface(ATypeInfo) <> nil;
  else
    Result := False;
  end;
end;

type

  { TdxMappingNullableMemberInfo }

  TdxMappingNullableMemberInfo = class(TdxMappingMemberInfo)
  protected
    FUnderlyingType: TRttiType;
    procedure SetMember(const Value: TRttiMember); override;
  public
    function IsType(ATypeInfo: PTypeInfo): Boolean; override;
    function GetValue(AObject: TObject): TValue; override;
    procedure SetValue(AObject: TObject; const AValue: TValue); override;
  end;

{ TdxMappingNullableMemberInfo }

function TdxMappingNullableMemberInfo.IsType(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := FUnderlyingType.Handle = ATypeInfo;
end;

function TdxMappingNullableMemberInfo.GetValue(AObject: TObject): TValue;
var
  AValue: TValue;
begin
  AValue := inherited GetValue(AObject);
  GetUnderlyingValue(AValue, Result);
end;

procedure TdxMappingNullableMemberInfo.SetMember(const Value: TRttiMember);
begin
  inherited SetMember(Value);
  TryGetUnderlyingRttiType(MemberType.Handle, FUnderlyingType);
end;

procedure TdxMappingNullableMemberInfo.SetValue(AObject: TObject; const AValue: TValue);
var
  ANullableValue: TValue;
begin
  ANullableValue := inherited GetValue(AObject);
  SetUnderlyingValue(ANullableValue, AValue);
  inherited SetValue(AObject, ANullableValue);
end;

{ TdxDiscriminator }

constructor TdxDiscriminator.Create(ADiscriminatorType: TdxDiscriminatorType; const AColumnName: string;
  AColumnSize: Integer);
begin
  HasValue := True;
  DiscriminatorType := ADiscriminatorType;
  ColumnName := AColumnName;
  if (ADiscriminatorType = TdxDiscriminatorType.String) and (AColumnSize = 0) then
    ColumnSize := ObjectTypePropertySize
  else
    ColumnSize := AColumnSize;
end;

constructor TdxDiscriminator.Create(AAttribute: DiscriminatorColumnAttribute);
begin
  Create(AAttribute.DiscriminatorType, AAttribute.DiscriminatorColumn, AAttribute.Size);
end;

class function TdxDiscriminator.CreateDefault: TdxDiscriminator;
begin
  Result := TdxDiscriminator.Create(TdxDiscriminatorType.Integer, ObjectTypePropertyName, 0);
end;

{ TdxAssociationAttributeInfo }

constructor TdxAssociationAttributeInfo.Create(const AAssociationName: string);
begin
  FAssociationName := AAssociationName;
end;

{ TdxIndexes.TIndex }

constructor TdxIndexes.TIndex.Create(const AColumns: TArray<string>; AIsUnique: Boolean);
begin
  Columns := AColumns;
  IsUnique := AIsUnique;
end;

{ TdxIndexes }

constructor TdxIndexes.Create;
begin
  inherited Create;
  FIndexColumns := TList<TIndex>.Create;
end;

destructor TdxIndexes.Destroy;
begin
  FreeAndNil(FIndexColumns);
  inherited Destroy;
end;

procedure TdxIndexes.Add(const AColumns: TArray<string>; AIsUnique: Boolean = False);

  function IsEqual(const AIndexColumns, ANewColumns: TArray<string>): Boolean;
  var
    I: Integer;
  begin
    if Length(AIndexColumns) <> Length(ANewColumns) then
      Exit(False);
    for I := 0 to Length(AIndexColumns) - 1 do
      if not SameText(AIndexColumns[I], ANewColumns[I]) then
        Exit(False);
    Result := True;
  end;

var
  AIndex: TIndex;
begin
  if Length(AColumns) = 0 then
    Exit;
  for AIndex in FIndexColumns do
    if IsEqual(AIndex.Columns, AColumns) then
      Exit;
  FIndexColumns.Add(TIndex.Create(AColumns, AIsUnique));
end;

procedure TdxIndexes.Add(const AColumn: string; AIsUnique: Boolean = False);
begin
  Add(TArray<string>.Create(AColumn), AIsUnique);
end;

function TdxIndexes.GetCount: Integer;
begin
  Result := FIndexColumns.Count;
end;

function TdxIndexes.GetIndexColumns(Index: Integer): TIndex;
begin
  Result := FIndexColumns[Index];
end;

{ TAttributeInfo }

procedure TdxAttributeInfo.Assign(ASource: TdxAttributeInfo);
begin
  FName := ASource.FName;
  AttributeType := ASource.AttributeType;
  RttiType := ASource.RttiType;
  TypeKind := ASource.TypeKind;
  Attributes := ASource.Attributes;
end;

constructor TdxAttributeInfo.Create(AOwner: TdxEntityInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxAttributeInfo.GetActualName: string;
begin
  Result := FName;
end;

function TdxAttributeInfo.GetIsDataMember: Boolean;
begin
  Result := AttributeType in [TdxAttributeType.&Property, TdxAttributeType.Field];
end;

procedure TdxAttributeInfo.SetName(const AName: string);
begin
  FName := AName;
end;

{ TdxEntityManager }

constructor TdxEntityManager.Create;
begin
  inherited Create;
  FClassAttributes := TObjectDictionary<TClass, TdxEntityInfo>.Create([doOwnsValues]);
  FProcessingEntities := TDictionary<TClass, TdxEntityInfo>.Create;
end;

class constructor TdxEntityManager.Create;
begin
  FContext := TRttiContext.Create;
end;

destructor TdxEntityManager.Destroy;
begin
  FreeAndNil(FClassAttributes);
  FreeAndNil(FProcessingEntities);
  inherited Destroy;
end;

class destructor TdxEntityManager.Destroy;
begin
  FContext.Free;
end;

procedure TdxEntityManager.EndCreateEntityInfo(AClass: TClass);
begin
  FProcessingEntities.Remove(AClass);
end;

procedure TdxEntityManager.AutoMappingAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
var
  AAttribute: TCustomAttribute;
  AAttributes: TArray<TCustomAttribute>;
  AAttributeInfo: TdxMappingMemberInfo;
  AFields: TArray<TRttiField>;
  AMember: TRttiField;
begin
  AFields := AType.GetFields;
  for AMember in AFields do
  begin
    if AEntityInfo.HasMember(AMember) then
      Continue;
    AAttributeInfo := AEntityInfo.CreateMappingMemberInfo(AMember);
    if AAttributeInfo = nil then
      Continue;
    try
      AAttributes := AMember.GetAttributes;
      for AAttribute in AAttributes do
        CheckColumnAttribute(AAttribute, AAttributeInfo);
      if TdxAttribute.NonPersistent in AAttributeInfo.Attributes then
        Continue;
      if CheckAttributes(AMember, AAttributeInfo) then
      begin
        CheckAutoMappingKeys(AMember, AAttributeInfo);
        Include(AAttributeInfo.Attributes, TdxAttribute.Column);
        AEntityInfo.Add(AAttributeInfo);
        AAttributeInfo := nil;
      end;
    finally
      AAttributeInfo.Free;
    end;
  end;
end;

procedure TdxEntityManager.CheckColumnAttribute(AAttribute: TCustomAttribute; AAttributeInfo: TdxMappingMemberInfo);
begin
  if AAttribute is NonPersistentAttribute then
    AAttributeInfo.NonPersistent
  else
  if AAttribute is ColumnAttribute then
    AAttributeInfo.Column(ColumnAttribute(AAttribute).ColumnName)
  else
  if AAttribute is SizeAttribute then
    AAttributeInfo.Size(SizeAttribute(AAttribute).Size)
  else
  if AAttribute is ReadOnlyAttribute then
    AAttributeInfo.ReadOnly
  else
  if AAttribute is NullableAttribute then
    AAttributeInfo.Nullable
  else
  if AAttribute is DefaultAttribute then
    AAttributeInfo.Default(DefaultAttribute(AAttribute).Value)
  else
  if AAttribute is KeyAttribute then
    Include(AAttributeInfo.Attributes, TdxAttribute.Key)
  else
  if AAttribute is GeneratorAttribute then
    AAttributeInfo.Generator(GeneratorAttribute(AAttribute).GeneratorType, GeneratorAttribute(AAttribute).SequenceName)
  else
  if AAttribute is IndexedAttribute then
    AAttributeInfo.Indexed
  else
  if AAttribute is UniqueAttribute then
    AAttributeInfo.Unique
  else
  if AAttribute is AssociationAttribute then
    AAttributeInfo.Association(AssociationAttribute(AAttribute).AssociationName)
  else
  if AAttribute is NoForeignKeyAttribute then
    AAttributeInfo.NoForeignKey
  else
  if AAttribute is AggregatedAttribute then
    AAttributeInfo.Aggregated
  else
  if AAttribute is VirtualColumnAttribute then
    AAttributeInfo.VirtualColumn
  else
  if AAttribute is DBTypeAttribute then
    AAttributeInfo.DBType(DBTypeAttribute(AAttribute).DBColumnTypeName)
  else
  if AAttribute is BlobAttribute then
    AAttributeInfo.Blob(BlobAttribute(AAttribute).SerializerClass)
end;

function TdxEntityManager.CheckIdentity(AMember: TRttiMember): Boolean;
begin
  if AMember is TRttiProperty then
    Result := not TRttiProperty(AMember).IsWritable
  else
    Result := False;
end;

procedure TdxEntityManager.AppendInheritanceMembers(AEntityInfo: TdxEntityInfo);
var
  ABaseEntityInfo: TdxEntityInfo;
  AMemberInfo, ABaseMemberInfo: TdxMappingMemberInfo;
begin
  ABaseEntityInfo := AEntityInfo.BaseEntityInfo;
  if ABaseEntityInfo = nil then
    Exit;
  if TdxAttribute.Automapping in ABaseEntityInfo.ClassAttributes.Attributes then
  begin
    for ABaseMemberInfo in ABaseEntityInfo.MemberAttributes do
    begin
      AMemberInfo := AEntityInfo.CreateMappingMemberInfo(ABaseMemberInfo.Member);
      AMemberInfo.Assign(ABaseMemberInfo);
      AEntityInfo.Add(AMemberInfo);
    end;
    AEntityInfo.ClassAttributes.KeyMemberNames.AddRange(ABaseEntityInfo.ClassAttributes.KeyMemberNames);
  end;
end;

procedure TdxEntityManager.BeginCreateEntityInfo(AClass: TClass; AEntityInfo: TdxEntityInfo);
begin
  FProcessingEntities.Add(AClass, AEntityInfo);
end;

function TdxEntityManager.CheckAttributes(AMember: TRttiMember; AAttributeInfo: TdxMappingMemberInfo): Boolean;
var
  AFieldType: TRttiType;
begin
  if AMember is TRttiField then
  begin
    AAttributeInfo.SetMemberName(GetQualifiedFieldName(AMember.Name));
    AFieldType := TRttiField(AMember).FieldType;

    if AFieldType = nil then
    begin
      Exit(False);
    end;

    AAttributeInfo.TypeKind := AFieldType.TypeKind;
  end
  else
    if AMember is TRttiProperty then
    begin
      AAttributeInfo.SetMemberName(AMember.Name);
      AAttributeInfo.AttributeType := TdxAttributeType.&Property;
      AFieldType := TRttiProperty(AMember).PropertyType
    end
    else
      Exit(False);

  if AAttributeInfo.ColumnName = '' then
    AAttributeInfo.SetName(AAttributeInfo.MemberName);
  AAttributeInfo.Member := AMember;
  AAttributeInfo.TypeKind := AFieldType.TypeKind;

  if AAttributeInfo.TypeKind in (TdxMappingMemberInfo.OrdinalTypes + TdxMappingMemberInfo.StringTypes) then
    Result := True
  else
    if AAttributeInfo.TypeKind = tkDynArray then
    begin
      Result := AFieldType.Handle = TypeInfo(TBytes);
      if not Result then
      begin
        AFieldType := (AFieldType as TRttiDynamicArrayType).ElementType;
        Result := AFieldType.TypeKind in TdxMappingMemberInfo.OrdinalTypes;
      end;
      AAttributeInfo.SetIsDynamicArrayType(Result);
    end
    else
      if TdxAttribute.Association in AAttributeInfo.Attributes then
      begin
        if IsAssociationListType(AFieldType.Handle) then
        begin
          AAttributeInfo.IsAssociationList := True;
          Result := True
        end
        else
          if AFieldType.IsInstance then
            Result := AAttributeInfo.Owner.ClassAttributes.FIsCodeRegistry or TdxEntityInfo.IsEntity(AFieldType)
          else
            Result := False;
        if Result then
          AAttributeInfo.UpdateReferenceType;
      end
      else
        if AFieldType.IsRecord then
        begin
          if (PTypeInfo(AFieldType.Handle) = TypeInfo(TGUID)) or (PTypeInfo(AFieldType.Handle) = TypeInfo(TBCD)) then
            Result := True
          else
            if IsNullableType(AFieldType.Handle) then
            begin
              Include(AAttributeInfo.Attributes, TdxAttribute.Nullable);
              Result := True;
            end
            else
              Result := False;
        end
        else
          if AFieldType.IsInstance then
          begin
            Result := SerializeMember(AAttributeInfo);
            if not Result then
            begin
              AAttributeInfo.UpdateReferenceType;
              Result := AAttributeInfo.ReferenceType <> nil;
            end
            else
              Include(AAttributeInfo.Attributes, TdxAttribute.Nullable);
          end
          else
            Result := False;
end;

function TdxEntityManager.CheckAutoMappingKeys(AMember: TRttiField; AMemberInfo: TdxMappingMemberInfo): Boolean;
var
  AFieldType: TRttiType;
  AProperties: TArray<TRttiProperty>;
  AProperty: TRttiProperty;
  AIsReadOnly: Boolean;
begin
  AFieldType := AMember.FieldType;
  Result := SameText(AMemberInfo.ColumnName, DefaultKeyName) and (
    (AMemberInfo.TypeKind in [tkInteger, tkInt64]) or (IsRecord(AMemberInfo.TypeKind) and (PTypeInfo(AFieldType.Handle) = TypeInfo(TGUID)))
    );
  if Result then
  begin
    Include(AMemberInfo.Attributes, TdxAttribute.Key);
    AProperties := AMember.Parent.GetProperties;
    AIsReadOnly := False;
    for AProperty in AProperties do
      if SameText(AProperty.Name, DefaultKeyName) then
      begin
        if CheckIdentity(AProperty) then
        begin
          Include(AMemberInfo.Attributes, TdxAttribute.Generator);
          AMemberInfo.DBGenerator := TdxGenerator.Create(TdxGeneratorType.Identity);
          AIsReadOnly := True;
        end;
      end;
    if AIsReadOnly then
      Include(AMemberInfo.Attributes, TdxAttribute.ReadOnly);
  end;
end;

procedure TdxEntityManager.CheckTableName(AType: TRttiType; var AAttributeInfo: TdxAttributeInfo);
begin
  AAttributeInfo.AttributeType := TdxAttributeType.&Class;
  AAttributeInfo.SetName(GetQualifiedTableName(AType.Name));
end;

function TdxEntityManager.CreateEntityInfo(AClass: TClass): TdxEntityInfo;
begin
  if IsProcessingEntityInfo(AClass) then
    Exit(GetProcessingEntityInfo(AClass));
  if not IsEntity(AClass) then
    Exit(nil);
  Result := TdxEntityInfo.Create;
  try
    BeginCreateEntityInfo(AClass, Result);
    try
      ProcessCreateEntityInfo(Result, AClass);
      if Result.KeyProperty.KeyFieldCount = 0 then
      begin
        FreeAndNil(Result);
        raise EdxKeyPropertyAbsentException.CreateFmt(sdxMetadataKeyPropertyDoesNotExist, [AClass.ClassName]);
      end;
      RegisterEntityInfo(Result);
    finally
      EndCreateEntityInfo(AClass);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TdxEntityManager.CreateNonPersistentObject(AClass: TClass): TObject;
var
  ARttiType: TRttiType;
  AMethod: TRttiMethod;
begin
  ARttiType := FContext.GetType(AClass);
  for AMethod in ARttiType.GetMethods do
    if AMethod.IsConstructor and (Length(AMethod.GetParameters) = 0) then
      Exit(AMethod.Invoke(AClass, []).AsObject);
  Result := AClass.Create;
end;

function TdxEntityManager.CreateObject(AClass: TClass): TObject;
begin
    Result := CreateNonPersistentObject(AClass);
end;

procedure TdxEntityManager.FieldsAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
var
  AAttribute: TCustomAttribute;
  AAttributes: TArray<TCustomAttribute>;
  AAttributeInfo: TdxMappingMemberInfo;
  AFields: TArray<TRttiField>;
  AMember: TRttiField;
begin
  AFields := AType.GetFields;
  for AMember in AFields do
  begin
    AAttributeInfo := AEntityInfo.CreateMappingMemberInfo(AMember);
    if AAttributeInfo = nil then
      Continue;
    try
      AAttributes := AMember.GetAttributes;
      for AAttribute in AAttributes do
        CheckColumnAttribute(AAttribute, AAttributeInfo);
      if AAttributeInfo.IsColumn then
        if CheckAttributes(AMember, AAttributeInfo) then
        begin
          AEntityInfo.Add(AAttributeInfo);
          AAttributeInfo := nil;
        end;
    finally
      AAttributeInfo.Free;
    end;
  end;
end;

procedure TdxEntityManager.ForceRegistration(AEntityInfo: TdxEntityInfo);
begin
  AEntityInfo.EndRegisterEntity;
end;

function TdxEntityManager.GetEntityInfo(AClass: TClass): TdxEntityInfo;
begin
  if AClass = nil then
    Exit(nil);
  if not FClassAttributes.TryGetValue(AClass, Result) then
    Result := CreateEntityInfo(AClass);
end;

function TdxEntityManager.GetEntityInfo(AObject: TObject): TdxEntityInfo;
begin
  if AObject = nil then
    Result := nil
  else
    Result := GetEntityInfo(AObject.ClassType);
end;

function TdxEntityManager.GetEntityInfo(const AClassName: string): TdxEntityInfo;
var
  AIsQualifiedClassName: Boolean;
  AItem: TPair<TClass, TdxEntityInfo>;
begin
  AIsQualifiedClassName := Pos('.', AClassName) > 0;

  for AItem in FClassAttributes do
    if AIsQualifiedClassName then
    begin
      if AnsiSameText(AItem.Value.GetQualifiedEntityClassName, AClassName) then
        Exit(AItem.Value);
    end
    else
      if AnsiSameText(AItem.Value.ClassAttributes.PersistentClass.ClassName, AClassName) then
        Exit(AItem.Value);
  Result := nil;
end;

function TdxEntityManager.GetEnumerator: TEnumerator;
begin
  Result := FClassAttributes.Values.GetEnumerator;
end;

function TdxEntityManager.GetProcessingEntityInfo(AClass: TClass): TdxEntityInfo;
begin
  if not FProcessingEntities.TryGetValue(AClass, Result) then
    Exit;
  if Result.ClassAttributes.FIsCodeRegistry then
    ForceRegistration(Result);
end;

class function TdxEntityManager.GetQualifiedFieldName(const AName: string): string;
var
  ALength: Integer;
begin
  ALength := Length(AName);
  if ALength = 0 then
    Exit('');
  if AName[1] = 'F' then
    Result := Copy(AName, 2, ALength)
  else
    Result := AName;
end;

class function TdxEntityManager.GetQualifiedTableName(const AName: string): string;
var
  ALength: Integer;
begin
  ALength := Length(AName);
  if ALength = 0 then
    Exit('');
  if AName[1] = 'T' then
    Result := Copy(AName, 2, ALength)
  else
    Result := AName;
end;

function TdxEntityManager.GetMember(AClass: TClass; const AMemberName: string): TRttiMember;
var
  AType: TRttiType;
begin
  AType := FContext.GetType(AClass);
  Result := AType.GetField(AMemberName);
  if Result = nil then
    Result := AType.GetProperty(AMemberName);
end;

function TdxEntityManager.GetSimpleKey(AObject: TObject): TValue;
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := GetEntityInfo(AObject.ClassType);
  if AEntityInfo = nil then
    raise EdxNoEntityInfoException.Create(AObject.ClassNAme);
  Result := AEntityInfo.KeyProperty.GetValue(AObject);
end;

function TdxEntityManager.GetTableName(AClass: TClass): string;
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := GetEntityInfo(AClass);
  if AEntityInfo <> nil then
    Result := GetTableName(AEntityInfo)
  else
    Result := '';
end;

function TdxEntityManager.HasClass(AClass: TClass): Boolean;
begin
  Result := FClassAttributes.ContainsKey(AClass);
end;

function TdxEntityManager.IsEntity(AClass: TClass): Boolean;
var
  AType: TRttiType;
  AAttributes: TArray<TCustomAttribute>;
  I: Integer;
begin
  if AClass = nil then
    Exit(False);
  AType := FContext.GetType(AClass);
  AAttributes := AType.GetAttributes;
  for I := 0 to Length(AAttributes) - 1 do
    if AAttributes[I] is EntityAttribute then
      Exit(True);
  Result := False;
end;

function TdxEntityManager.IsProcessingEntityInfo(AClass: TClass): Boolean;
begin
  Result := FProcessingEntities.ContainsKey(AClass);
end;

procedure TdxEntityManager.ProcessCreateEntityInfo(AEntityInfo: TdxEntityInfo; AClass: TClass);
var
  AAutomapping: Boolean;
  AType: TRttiType;
  AAttributes: TArray<TCustomAttribute>;
  AAttribute: TCustomAttribute;
  AAttributeInfo: TdxMappingClassInfo;
begin
  AType := FContext.GetType(AClass);
  AAttributes := AType.GetAttributes;
  AAttributeInfo := TdxMappingClassInfo.Create(AEntityInfo, AClass);
  AEntityInfo.Add(AAttributeInfo);
  AAttributeInfo.RttiType := AType;
  AAutomapping := False;
  for AAttribute in AAttributes do
  begin
    if AAttribute is EntityAttribute then
      Include(AAttributeInfo.Attributes, TdxAttribute.Entity)
    else
    if AAttribute is AutomappingAttribute then
    begin
      Include(AAttributeInfo.Attributes, TdxAttribute.Automapping);
      AAutomapping := True;
    end
    else
    if AAttribute is SchemaNameAttribute then
      AAttributeInfo.TableSchemaName(SchemaNameAttribute(AAttribute).SchemaName)
    else
    if AAttribute is TableAttribute then
    begin
      AAttributeInfo.Table(TableAttribute(AAttribute).TableName);
      if TableAttribute(AAttribute).SchemaName <> '' then
        AAttributeInfo.SetSchemaName(TableAttribute(AAttribute).SchemaName);
    end
    else
    if AAttribute is KeyAttribute then
      AAttributeInfo.KeyMemberNames.AddRange(KeyAttribute(AAttribute).MemberNames)
    else
    if AAttribute is IndexesAttribute then
      AAttributeInfo.Indexes.Add(IndexesAttribute(AAttribute).Columns)
    else
    if AAttribute is InheritanceAttribute then
      AAttributeInfo.Inheritance(InheritanceAttribute(AAttribute).MapInheritance)
    else
    if AAttribute is DiscriminatorColumnAttribute then
      AAttributeInfo.DiscriminatorInfo := TdxDiscriminator.Create(DiscriminatorColumnAttribute(AAttribute))
    else
    if AAttribute is DiscriminatorAttribute then
      AAttributeInfo.DiscriminatorValue := DiscriminatorAttribute(AAttribute).Value;
  end;
  if AAttributeInfo.IsInheritance then
    AppendInheritanceMembers(AEntityInfo);

  if AAutomapping then
    AutoMappingAttributes(AType, AEntityInfo)
  else
  begin
    FieldsAttributes(AType, AEntityInfo);
    PropertyAttributes(AType, AEntityInfo);
  end;
  AEntityInfo.RegistrationFinalization;
end;

procedure TdxEntityManager.PropertyAttributes(AType: TRttiType; AEntityInfo: TdxEntityInfo);
var
  AAttribute: TCustomAttribute;
  AAttributes: TArray<TCustomAttribute>;
  AAttributeInfo: TdxMappingMemberInfo;
  AProperties: TArray<TRttiProperty>;
  AMember: TRttiProperty;
begin
  AProperties := AType.GetProperties;
  for AMember in AProperties do
  begin
    AAttributeInfo := AEntityInfo.CreateMappingMemberInfo(AMember);
    if AAttributeInfo = nil then
      Continue;
    try
      AAttributes := AMember.GetAttributes;
      for AAttribute in AAttributes do
        CheckColumnAttribute(AAttribute, AAttributeInfo);
      if not AAttributeInfo.IsColumn then
        Continue;
      if CheckAttributes(AMember, AAttributeInfo) then
      begin
        AEntityInfo.Add(AAttributeInfo);
        AAttributeInfo := nil;
      end;
    finally
      AAttributeInfo.Free;
    end;
  end;
end;

procedure TdxEntityManager.RegisterEntities(const AClasses: array of TClass);
var
  AClass: TClass;
begin
  for AClass in AClasses do
    RegisterEntity(AClass);
end;

function TdxEntityManager.RegisterEntity(AClass: TClass): IdxMappingClassInfo;
var
  AType: TRttiType;
  AEntityInfo: TdxEntityInfo;
  AAttributeInfo: TdxMappingClassInfo;
begin
  if IsEntity(AClass) then
    AEntityInfo := GetEntityInfo(AClass)
  else
  begin
    if IsProcessingEntityInfo(AClass) then
      Exit(GetProcessingEntityInfo(AClass).ClassAttributes);
    if not FClassAttributes.TryGetValue(AClass, AEntityInfo) then
    begin
      AType := FContext.GetType(AClass);
      AEntityInfo := TdxEntityInfo.Create;
      BeginCreateEntityInfo(AClass, AEntityInfo);
      AAttributeInfo := TdxMappingClassInfo.Create(AEntityInfo, AClass);
      AAttributeInfo.FIsCodeRegistry := True;
      AAttributeInfo.RttiType := AType;
      AEntityInfo.Add(AAttributeInfo);
      Include(AEntityInfo.ClassAttributes.Attributes, TdxAttribute.Entity);
    end;
  end;
  Result := AEntityInfo.ClassAttributes;
end;

procedure TdxEntityManager.RegisterEntityInfo(AEntityInfo: TdxEntityInfo);
begin
  FClassAttributes.Add(AEntityInfo.ClassAttributes.PersistentClass, AEntityInfo);
end;

function TdxEntityManager.SerializeMember(AAttributeInfo: TdxMappingMemberInfo): Boolean;
var
  AFieldType: TRttiType;
  AClass: TClass;
begin
  case AAttributeInfo.AttributeType of
    TdxAttributeType.Field:
      AFieldType := TRttiField(AAttributeInfo.Member).FieldType;
    TdxAttributeType.&Property:
      AFieldType := TRttiProperty(AAttributeInfo.Member).PropertyType;
    else
      raise EInvalidOperation.Create('');
  end;
  if not AFieldType.IsInstance then
    Exit(False);
  AClass := TRttiInstanceType(AFieldType).MetaclassType;
  AAttributeInfo.SetSerializer(TdxBlobSerializerFactory.GetSerializer(AClass));
  Result := AAttributeInfo.Serializer <> nil;
end;

procedure TdxEntityManager.UnRegisterEntities(const AClasses: array of TClass);
var
  AClass: TClass;
begin
  for AClass in AClasses do
    UnRegisterEntity(AClass);
end;

procedure TdxEntityManager.UnRegisterEntity(AClass: TClass);
begin
  FClassAttributes.Remove(AClass);
  FProcessingEntities.Remove(AClass);
end;

class function TdxEntityManager.GetTableName(AEntityInfo: TdxEntityInfo): string;
begin
  Result := AEntityInfo.ClassAttributes.TableName;
end;

{ TdxEntityInfo }

constructor TdxEntityInfo.Create;
begin
  inherited Create;
  FAttributeInfoList := TAttributeInfoList.Create;
  FMemberAttributes := TMemberAttributes.Create;
  FKeyAttributes := TKeyAttributes.Create;
  FServiceFields := TdxServiceFields.Create;
  FMembers := TDictionary<string, TdxMappingMemberInfo>.Create;
end;

destructor TdxEntityInfo.Destroy;
begin
  FreeAndNil(FMembers);
  FreeAndNil(FMemberAttributes);
  FreeAndNil(FPersistentProperties);
  FreeAndNil(FAttributeInfoList);
  FreeAndNil(FKeyAttributes);
  FreeAndNil(FCachedPaths);
  FreeAndNil(FPropertiesForUpdate);
  FreeAndNil(FPropertiesForInsert);
  FreeAndNil(FServiceFields);
  FreeAndNil(FKeyProperty);
  if not FIsParentTable then
    FreeAndNil(FTable);
  inherited Destroy;
end;

procedure TdxEntityInfo.EndRegisterEntity;
var
  AMemberInfos: TArray<TdxMappingMemberInfo>;
  AMemberInfo: TdxMappingMemberInfo;
begin
  if FRegisterEntityProcessEnd then
    Exit;
  FRegisterEntityProcessEnd := True;
  AMemberInfos := MemberAttributes.ToArray;
  for AMemberInfo in AMemberInfos do
  begin
    if TdxAttribute.NonPersistent in AMemberInfo.Attributes then
    begin
      FMembers.Remove(AMemberInfo.MemberName);
      FKeyAttributes.Remove(AMemberInfo);
      FMemberAttributes.Remove(AMemberInfo);
    end
    else
      EntityManager.CheckAttributes(AMemberInfo.Member, AMemberInfo);
  end;
  RegistrationFinalization;
  EntityManager.RegisterEntityInfo(Self);
  EntityManager.EndCreateEntityInfo(ClassAttributes.PersistentClass);
  ClassAttributes.FIsCodeRegistry := False;
  FRegisterEntityProcessEnd := False;
end;

procedure TdxEntityInfo.CreateDiscriminator;
var
  ADiscriminatorField: TdxDiscriminatorMember;
begin { TODO : refactor CreateDiscriminator! }
  if not ClassAttributes.FDiscriminatorInfo.HasValue then
    ClassAttributes.FDiscriminatorInfo := TdxDiscriminator.CreateDefault;
  ADiscriminatorField := TdxDiscriminatorMember.Create(Self);
  ADiscriminatorField.FDiscriminator := ClassAttributes.DiscriminatorInfo;
  ADiscriminatorField.DiscriminatorValue := TValue.FromVariant(ClassAttributes.DiscriminatorValue);
  ServiceFields.Add(ADiscriminatorField);
  Add(ADiscriminatorField);
end;

function TdxEntityInfo.CreateMappingMemberInfo(AMember: TRttiMember): TdxMappingMemberInfo;
var
  AFieldType: TRttiType;
  AAttributeType: TdxAttributeType;
begin
  if AMember is TRttiField then
  begin
    AFieldType := TRttiField(AMember).FieldType;
    AAttributeType := TdxAttributeType.Field;
  end
  else
  begin
    AFieldType := TRttiProperty(AMember).PropertyType;
    AAttributeType := TdxAttributeType.&Property;
  end;
  if AFieldType = nil then
    Exit(nil);
  if IsNullableType(AFieldType.Handle) then
    Result := TdxMappingNullableMemberInfo.Create(Self)
  else
    Result := TdxMappingMemberInfo.Create(Self);
  Result.RttiType := AFieldType;
  Result.AttributeType := AAttributeType;
end;

procedure TdxEntityInfo.CreateTable;
var
  ATmpTable: TdxDBTable;
  ABaseEntityInfo: TdxEntityInfo;
begin
  ABaseEntityInfo := BaseEntityInfo;
  if (ABaseEntityInfo <> nil) and (ABaseEntityInfo.TableName = TableName) then
  begin
    ATmpTable := ABaseEntityInfo.DBTable;
    FIsParentTable := True;
  end
  else
    ATmpTable := TdxDBTable.Create(TableName);

    TdxDBTableHelper.ProcessClassInfo(ATmpTable, Self);
  FTable := ATmpTable;
end;

function TdxEntityInfo.FindMember(const AMemberName: string): TdxMappingMemberInfo;
var
  AEntityInfo: TdxEntityInfo;
  AMember: TdxAttributeInfo;
begin
  if AMemberName = '' then
    Exit(nil);

  AEntityInfo := Self;
  while AEntityInfo <> nil do
  begin
    AEntityInfo.TryGetValueByName(AMemberName, AMember);
    if AMember <> nil then
      Exit(TdxMappingMemberInfo(AMember));
    AEntityInfo := AEntityInfo.BaseEntityInfo;
  end;
  Result := nil;
end;

function TdxEntityInfo.FindMemberByColumnName(const AColumnName: string): TdxMappingMemberInfo;
begin
  for Result in MemberAttributes do
    if SameText(Result.ColumnName, AColumnName) then
      Exit;
  Result := nil;
end;

function TdxEntityInfo.CanPersist: Boolean;
begin
  Result := not (TdxAttribute.NonPersistent in ClassAttributes.Attributes);
end;

procedure TdxEntityInfo.CheckAbstractReference;
begin
  if not IsPersistent then
    raise EdxNonPersistentReferenceFoundException.Create(FullName);
end;

procedure TdxEntityInfo.Clear;
begin
  FMemberAttributes.Clear;
  FAttributeInfoList.Clear;
end;

function TdxEntityInfo.GenerateDiscriminantValue: TValue;
var
  AQualifiedClassName: string;
  AMaxSize: Integer;
begin
  AQualifiedClassName := GetQualifiedEntityClassName;
  case ServiceFields.DiscriminatorField.Discriminator.DiscriminatorType of
    TdxDiscriminatorType.&String:
      begin
        AMaxSize := ServiceFields.DiscriminatorField.Discriminator.ColumnSize;
        if Length(AQualifiedClassName) > AMaxSize then
          Delete(AQualifiedClassName, 1, Length(AQualifiedClassName) - AMaxSize);
        Result := AQualifiedClassName;
      end;
    TdxDiscriminatorType.Integer:
      Result := dxElfHash(AQualifiedClassName);
  end;
end;

function TdxEntityInfo.GetBaseEntityInfo: TdxEntityInfo;
var
  ABaseClass: TClass;
begin
  ABaseClass := ClassAttributes.PersistentBaseClass;
  Result := EntityManager.GetEntityInfo(ABaseClass);
end;

function TdxEntityInfo.GetCount: Integer;
begin
  Result := FAttributeInfoList.Count;
end;

function TdxEntityInfo.GetEnumerator: TAttributeInfoList.TEnumerator;
begin
  Result := FAttributeInfoList.GetEnumerator;
end;

function TdxEntityInfo.GetFullName: string;
begin
  Result := GetQualifiedEntityClassName;
end;

procedure TdxEntityInfo.Add(AAttributeInfo: TdxAttributeInfo);
var
  AMemberAttributes: TdxMappingMemberInfo absolute AAttributeInfo;
  AExistMemberAttributes: TdxMappingMemberInfo;
begin
  if AAttributeInfo.IsDataMember then
  begin
    if FMembers.ContainsKey(AMemberAttributes.MemberName) then
    begin
      AExistMemberAttributes := FMembers[AMemberAttributes.MemberName];
      raise EdxDuplicateInheritedMemberNameFoundException.CreateFmt(sdxMetadataDuplicateInheritedMemberNameFound,
        [AMemberAttributes.MemberName, AMemberAttributes.Member.Parent.Name, AExistMemberAttributes.Member.Parent.Name]);
    end;
    AMemberAttributes.FID := FMemberAttributes.Add(AMemberAttributes);
    if AMemberAttributes.IsKey then
      FKeyAttributes.Add(AMemberAttributes);
    FMembers.Add(AMemberAttributes.MemberName, AMemberAttributes);
  end
  else
    if AAttributeInfo is TdxMappingClassInfo then
      FClassAttribute := TdxMappingClassInfo(AAttributeInfo);
  FAttributeInfoList.Add(AAttributeInfo);
end;

function TdxEntityInfo.GetIsPersistent: Boolean;
begin
  Result := CanPersist;
end;

function TdxEntityInfo.GetItems(AIndex: Integer): TdxAttributeInfo;
begin
  Result := FAttributeInfoList.Items[AIndex];
end;

function TdxEntityInfo.GetKeyValue(AObject: TObject): TValue;
var
  AKey: TdxKeyMemberInfo;
begin
  Assert(AObject.ClassType = ClassAttributes.PersistentClass);
  AKey := KeyProperty;
  if AKey = nil then
    Result := TValue.Empty
  else
    if AKey.IsCompositeKey then
      Result := TValue.FromArray(TypeInfo(TArray<TValue>), AKey.GetValues(AObject))
    else
      Result := AKey.GetValue(AObject);
end;

function TdxEntityInfo.GetMemberValues(AObject: TObject): TArray<Variant>;
var
  I: Integer;
begin
  SetLength(Result, MemberAttributes.Count);
  for I := 0 to Length(Result) - 1 do
    Result[I] := MemberAttributes[I].GetValue(AObject).ToVariant;
end;

function TdxEntityInfo.GetOptimisticLockField: TdxMappingMemberInfo;
begin
  Result := nil;
end;

function TdxEntityInfo.GetOptimisticLockingBehavior: TdxOptimisticLockingBehavior;
begin
  Result := TdxOptimisticLockingBehavior.NoLocking;
end;

function TdxEntityInfo.GetParentEntity: TdxEntityInfo;
var
  AClass: TClass;
begin
  if FParentEntity.HasValue then
    Result := FParentEntity.Value
  else
  begin
    if ClassAttributes.IsInheritance and (ClassAttributes.MapInheritance = TdxMapInheritanceType.OwnTable) then
    begin
      AClass := (ClassAttributes.RttiType as TRttiInstanceType).BaseType.MetaclassType;
      repeat
        Result := EntityManager.GetEntityInfo(AClass);
        AClass := AClass.ClassParent;
      until (Result <> nil) or (AClass = nil);
    end
    else
      Result := nil;
    FParentEntity := Result;
  end;
end;

function TdxEntityInfo.GetPersistentProperties: TMemberAttributes;
var
  AMemberInfo: TdxMappingMemberInfo;
begin
  if FPersistentProperties = nil then
  begin
    FPersistentProperties := TMemberAttributes.Create;
    for AMemberInfo in MemberAttributes do
      if AMemberInfo.IsPersistent and not AMemberInfo.IsAssociationList then
        FPersistentProperties.Add(AMemberInfo);
  end;
  Result := FPersistentProperties;
end;

function TdxEntityInfo.GetPropertiesListForUpdateInsert(AObject: TObject; AIsUpdate,
  AAddDelayedReference: Boolean): TdxMemberInfoCollection;
var
  AList: TdxMemberInfoCollection;
  M: TdxMappingMemberInfo;
  S: TdxServiceMember;
begin
  if AIsUpdate then
  begin
    if FPropertiesForUpdate = nil then
    begin
      AList := TdxMemberInfoCollection.Create(Self);
      for M in PersistentProperties do
      begin
        if M.IsKey then
          Continue;
        if M.IsDelayed then
        begin
          FHasDelayedProperties := True;
          NotImplemented;
        end;
        AList.Add(M);
      end;
      FPropertiesForUpdate := AList;
    end;
    Exit(FPropertiesForUpdate);
  end;
  if FPropertiesForInsert = nil then
  begin
    AList := TdxMemberInfoCollection.Create(Self);
    for M in PersistentProperties do
      AList.Add(M);
{ TODO : ServiceFields in GetPropertiesListForUpdateInsert }
    for S in ServiceFields do
      AList.Add(S);
    FPropertiesForInsert := AList;
  end;
  Result := FPropertiesForInsert;
end;

class function TdxEntityInfo.GetQualifiedClassName(AClass: TClass): string;
begin
{$IFDEF DELPHIXE2}
  Result := AClass.QualifiedClassName;
{$ELSE}
  Result := AClass.UnitName + '.' + AClass.ClassName;
{$ENDIF}
end;

function TdxEntityInfo.GetQualifiedEntityClassName: string;
begin
  Result := GetQualifiedClassName(ClassAttributes.PersistentClass);
end;

function TdxEntityInfo.GetDBTable: TdxDBTable;
begin
  if FTable = nil then
  begin
    if TableName = '' then
      Exit(nil);
    TMonitor.Enter(Self);
    try
      if FTable = nil then
        CreateTable;
    finally
      TMonitor.Exit(Self);
    end;
  end;
  Result := FTable;
end;

function TdxEntityInfo.GetTableName: string;
begin
  if FTableName = '' then
  begin
    if ClassAttributes.IsPersistent and not ClassAttributes.IsParentTable then
      FTableName := ClassAttributes.TableName
    else
      if BaseEntityInfo <> nil then
        FTableName := BaseEntityInfo.TableName;
  end;
  Result := FTableName;
end;

function TdxEntityInfo.GetEntityUnitName: string;
begin
  Result := FClassAttribute.ClassType.UnitName;
end;

function TdxEntityInfo.HasMember(AMember: TRttiMember): Boolean;
var
  AMemberInfo: TdxMappingMemberInfo;
  AAttributeInfo: TdxAttributeInfo absolute AMemberInfo;
begin
  if TryGetValueByName(TdxEntityManager.GetQualifiedFieldName(AMember.Name), AAttributeInfo) then
    Exit(True);
  for AMemberInfo in FMemberAttributes do
    if AMemberInfo.Member = AMember then
      Exit(True);
  Result := False;
end;

function TdxEntityInfo.HasModifications(AObject: TObject): Boolean;
begin
  Result := False;
end;


procedure TdxEntityInfo.InitAssociationListMembers;
var
  AMembers: TMemberAttributes;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AMembers := TMemberAttributes.Create;
  try
    for AMemberInfo in MemberAttributes do
      if AMemberInfo.IsAssociationList then
        AMembers.Add(AMemberInfo);
    FAssociationListProperties := AMembers.ToArray;
  finally
    AMembers.Free;
  end;
end;

procedure TdxEntityInfo.InitAssociation(ASession: TComponent;
  AObject: TObject; const AQueryable: IdxQueryable; const AMemberName: string);
var
  AAssociationClass: TClass;
  AAssociationMember: TdxMappingMemberInfo;
  AFoundAssociation: Boolean;
  AList: TdxEMFCustomCollection;
begin
  AList := AQueryable as TdxEMFCustomCollection;
  AAssociationClass := AList.CollectionElementClass;
  if AMemberName <> '' then
  begin
    for AAssociationMember in AssociationListProperties do
      if AAssociationMember.IsMember(AMemberName) then
      begin
        AList.Init(TdxEMFCustomSession(ASession), AObject, AAssociationMember);
        Exit;
      end;
    if EntityManager.GetMember(AObject.ClassType, AMemberName) = nil then
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataFieldPropertyNotFound,
        [AMemberName, AObject.ClassName])
    else
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataFieldPropertyNotPersistent,
        [AMemberName, AObject.ClassName]);
  end
  else
  begin
    AFoundAssociation := False;
    for AAssociationMember in AssociationListProperties do
    begin
      if AAssociationClass = AAssociationMember.CollectionElementType.ClassAttributes.PersistentClass then
      begin
        if AFoundAssociation then
          raise EdxAssociationInvalidException.CreateFmt(sdxMetadataCannotResolveUnnamedAssociation,
            [AObject.ClassName, AMemberName]);
        AList.Init(TdxEMFCustomSession(ASession), AObject, AAssociationMember);
        AFoundAssociation := True;
      end;
    end;
  end;
end;

procedure TdxEntityInfo.InitAssociations(ASession: TComponent; AObject: TObject);
var
  AAssociationMember: TdxMappingMemberInfo;
  AMemberValue: TValue;
  AList: TdxEMFCustomCollection;
  I: Integer;
begin
  Assert((ASession = nil) or (ASession is TdxEMFCustomSession));
  for I := 0 to Length(AssociationListProperties) - 1 do
  begin
    AAssociationMember := AssociationListProperties[I];
    case AAssociationMember.AttributeType of
      TdxAttributeType.&Property:
        AMemberValue := TRttiProperty(AAssociationMember.Member).GetValue(AObject);
      TdxAttributeType.Field:
        AMemberValue := TRttiField(AAssociationMember.Member).GetValue(AObject);
      else
        Continue;
    end;
    if AMemberValue.IsEmpty then
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationNotCreated, [AObject.ClassName, AAssociationMember.MemberName]);
    Assert(AMemberValue.Kind = TTypeKind.tkInterface);
    AList := TdxEMFCustomCollection(AMemberValue.AsInterface);
    AList.Init(TdxEMFCustomSession(ASession), AObject, AAssociationMember);
  end;
end;

procedure TdxEntityInfo.InitSession(ASession: TComponent; AObject: TObject);
var
  AAssociationMember: TdxMappingMemberInfo;
  AMemberValue: TValue;
  AList: TdxEMFCustomCollection;
  I: Integer;
begin
  Assert(ASession is TdxEMFCustomSession);
  for I := 0 to Length(AssociationListProperties) - 1 do
  begin
    AAssociationMember := AssociationListProperties[I];
    case AAssociationMember.AttributeType of
      TdxAttributeType.&Property:
        AMemberValue := TRttiProperty(AAssociationMember.Member).GetValue(AObject);
      TdxAttributeType.Field:
        AMemberValue := TRttiField(AAssociationMember.Member).GetValue(AObject);
      else
        Continue;
    end;
    if AMemberValue.IsEmpty then
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationNotCreated, [AObject.ClassName, AAssociationMember.MemberName]);
    Assert(AMemberValue.Kind = TTypeKind.tkInterface);
    AList := TdxEMFCustomCollection(AMemberValue.AsInterface);
    AList.Init(TdxEMFCustomSession(ASession));
  end;
end;

procedure TdxEntityInfo.InitMembers;
begin
  InitServiceMembers;
  InitAssociationListMembers;
  InitObjectMembers;
end;

procedure TdxEntityInfo.InitObjectMembers;
var
  AMembers: TMemberAttributes;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AMembers := TMemberAttributes.Create;
  try
    for AMemberInfo in MemberAttributes do
      if AMemberInfo.IsPersistent and (AMemberInfo.ReferenceType <> nil) then
        AMembers.Add(AMemberInfo);
    FObjectProperties := AMembers.ToArray;
  finally
    AMembers.Free;
  end;
end;

procedure TdxEntityInfo.InitServiceMembers;
var
  ADiscriminatorField: TdxDiscriminatorMember;
  ABaseClass: TClass;
  ABaseEntity: TdxEntityInfo;
begin
  if ClassAttributes.IsInheritance then
  begin
    ABaseEntity := Self;
    repeat
      ABaseClass := ABaseEntity.ClassAttributes.PersistentBaseClass;
      ABaseEntity := EntityManager.GetEntityInfo(ABaseClass);
    until (ABaseEntity = nil) or (ABaseEntity.ClassAttributes.DiscriminatorInfo.HasValue);
    if ABaseEntity = nil then
    begin
      if not ClassAttributes.IsParentTable then
        Exit;
      if BaseEntityInfo = nil then
        raise EdxNoEntityInfoException.CreateFmt(sdxMetadataBaseEntityNotFound, [FullName])
      else
        raise EdxBaseEntityNoDiscriminatorColumnException.CreateFmt(sdxMetadataBaseEntityNoDiscriminatorColumn, [FullName]);
    end;

    if ClassAttributes.IsParentTable and not ABaseEntity.ClassAttributes.DiscriminatorInfo.HasValue then
      ABaseEntity.CreateDiscriminator;

    ADiscriminatorField := TdxDiscriminatorMember.Create(Self);
    ADiscriminatorField.Discriminator := ABaseEntity.ClassAttributes.DiscriminatorInfo;
    ServiceFields.Add(ADiscriminatorField);
    Add(ADiscriminatorField);

    if VarIsEmpty(ClassAttributes.DiscriminatorValue) or VarIsNull(ClassAttributes.DiscriminatorValue) then
      ADiscriminatorField.DiscriminatorValue := GenerateDiscriminantValue
    else
      ADiscriminatorField.DiscriminatorValue := TValue.FromVariant(ClassAttributes.DiscriminatorValue);
  end;
end;


function TdxEntityInfo.IsAssignableTo(AEntityInfo: TdxEntityInfo): Boolean;
begin
  if AEntityInfo = nil then
    Exit(False);
  if IsAssignableToCore(AEntityInfo) then
    Exit(True);
  Result := False;
end;

function TdxEntityInfo.IsAssignableToCore(AEntityInfo: TdxEntityInfo): Boolean;
var
  ABaseClass: TdxEntityInfo;
begin
  if AEntityInfo = Self then
    Exit(True);
  ABaseClass := EntityManager.GetEntityInfo(ClassAttributes.PersistentBaseClass);
  if ABaseClass = nil then
    Exit(False);
  Result := ABaseClass.IsAssignableToCore(AEntityInfo);
end;

class function TdxEntityInfo.IsEntity(AType: TRttiType): Boolean;
var
  AAttributes: TArray<TCustomAttribute>;
  AAttribute: TCustomAttribute;
begin
  if AType = nil then
    Exit(False);
  AAttributes := AType.GetAttributes;
  for AAttribute in AAttributes do
    if AAttribute is EntityAttribute then
      Exit(True);
  Result := False;
end;

function TdxEntityInfo.ParsePath(const APath: string): TdxMemberInfoCollection;
begin
  if FCachedPaths = nil then
    FCachedPaths := TObjectDictionary<string, TdxMemberInfoCollection>.Create([doOwnsValues]);
  if not FCachedPaths.TryGetValue(APath, Result) then
  begin
    Result := TdxMemberInfoCollection.Create(Self, APath, True);
    FCachedPaths.Add(APath, Result);
  end;
end;

function TdxEntityInfo.ParsePersistentPath(const APath: string): TdxMemberInfoCollection;
begin
  Result := ParsePath(APath);
end;

procedure TdxEntityInfo.RegistrationFinalization;

  procedure KeyAttributesProcess;
  var
    AEI: TdxEntityInfo;
    AMemberName: string;
    AAttributeInfo: TdxAttributeInfo;
    AMemberInfo: TdxMappingMemberInfo absolute AAttributeInfo;
    ARttiType: TRttiInstanceType;
    AKeyProperty: TdxKeyMemberInfo;
  begin
    ARttiType := ClassAttributes.RttiType as TRttiInstanceType;
    AEI := Self;
    repeat
      for AMemberName in AEI.ClassAttributes.KeyMemberNames do
      begin
        if TryGetValueByName(AMemberName, AAttributeInfo) then
        begin
          Include(AAttributeInfo.Attributes, TdxAttribute.Key);
          if KeyAttributes.IndexOf(AMemberInfo) <= 0 then
            KeyAttributes.Add(AMemberInfo);
        end
      end;
      ARttiType := ARttiType.BaseType;
      if ARttiType = nil then
        Break;
      AEI := EntityManager.GetEntityInfo(ARttiType.MetaclassType);
    until AEI = nil;
    AKeyProperty := TdxKeyMemberInfo.Create(Self);
    AKeyProperty.CalcKeyFieldCount;
    SetKeyProperty(AKeyProperty);
  end;

  procedure IndexesAttributesProcess;
  var
    AMemberInfo: TdxMappingMemberInfo;
  begin
    for AMemberInfo in MemberAttributes do
      if not AMemberInfo.IsKey and ([TdxAttribute.Indexed, TdxAttribute.Unique] * AMemberInfo.Attributes <> []) and
        AMemberInfo.IsMappingClass(Self) then
        ClassAttributes.Indexes.Add(AMemberInfo.ColumnName, AMemberInfo.IsUnique);
  end;

begin
  if ClassAttributes.AttributeName = '' then
    ClassAttributes.SetName(EntityManager.GetQualifiedTableName(ClassAttributes.PersistentClass.ClassName));
  if ClassAttributes.DiscriminatorInfo.HasValue then
    CreateDiscriminator;
  KeyAttributesProcess;
  IndexesAttributesProcess;
  InitMembers;
end;

procedure TdxEntityInfo.SetKeyProperty(AValue: TdxKeyMemberInfo);
begin
  FKeyProperty := AValue;
end;

function TdxEntityInfo.TryGetValueByName(const AName: string; out AAttributeInfo: TdxAttributeInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    AAttributeInfo := FAttributeInfoList{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
    if SameText(AName, AAttributeInfo.GetActualName) then
      Exit(True);
  end;
  AAttributeInfo := System.Default(TdxAttributeInfo);
  Result := False;
end;

{ TdxEntityInfo.TKeyAttributes }

destructor TdxMappingMemberInfo.Destroy;
begin
  FreeAndNil(FSerializer);
  FreeAndNil(FValueConverter);
  inherited Destroy;
end;

function TdxEntityInfo.TKeyAttributes.GetColumnsNames: TArray<string>;
var
  AResult: TList<string>;
  AKey: TdxMappingMemberInfo;
begin
  AResult := TList<string>.Create;
  try
    for AKey in Self do
      AResult.Add(AKey.ColumnName);
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

{ TdxMappingMemberInfo }

function TdxMappingMemberInfo.Generator(AGeneratorType: TdxGeneratorType; const ASequenceName: string = ''): IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Generator);
  DBGenerator := TdxGenerator.Create(AGeneratorType, ASequenceName);
  Result := Self;
end;

function TdxMappingMemberInfo.GetActualName: string;
begin
  Result := FMemberName;
end;

function TdxMappingMemberInfo.Aggregated: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Aggregated);
  Result := Self;
end;

procedure TdxMappingMemberInfo.Assign(ASource: TdxAttributeInfo);
var
  AMappingMember: TdxMappingMemberInfo;
begin
  inherited Assign(ASource);
  AMappingMember := Safe<TdxMappingMemberInfo>.Cast(ASource);
  if AMappingMember = nil then
    Exit;
  FAssociatedMember := AMappingMember.FAssociatedMember;
  FIsAssociationList := AMappingMember.FIsAssociationList;
  FReferenceType := AMappingMember.FReferenceType;
  FCollectionElementType := AMappingMember.FCollectionElementType;
  FMemberName := AMappingMember.FMemberName;
  if AMappingMember.FSerializer <> nil then
    FSerializer := TdxCustomBlobSerializerClass(AMappingMember.FSerializer.ClassType).Create;
  if Length(AMappingMember.FSubMembers) > 0 then
    TArray.Copy<TdxMappingMemberInfo>(AMappingMember.FSubMembers, FSubMembers, Length(AMappingMember.FSubMembers));
  Member := AMappingMember.Member;
  DBColumnSize := AMappingMember.DBColumnSize;
  DefaultValue := AMappingMember.DefaultValue;
  DBColumnTypeName := AMappingMember.DBColumnTypeName;
  DBGenerator := AMappingMember.DBGenerator;
end;

function TdxMappingMemberInfo.Association(const AAssociationName: string): IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Association);
  if AAssociationName <> '' then
    FAssociationAttributeInfo := TdxAssociationAttributeInfo.Create(AAssociationName);
  Result := Self;
end;

function TdxMappingMemberInfo.Blob(ASerializerClass: TdxCustomBlobSerializerClass): IdxMappingMemberInfo;
begin
  Result := Blob;
  SetSerializer(ASerializerClass)
end;

function TdxMappingMemberInfo.Blob: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Blob);
  DBColumnSize := -1;
  Result := Self;
end;

function TdxMappingMemberInfo.VirtualColumn: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.VirtualColumn);
  Result := Self;
end;

function TdxMappingMemberInfo.Column(const AColumnName: string): IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Column);
  SetName(AColumnName);
  Result := Self;
end;

class function TdxMappingMemberInfo.Context: TRttiContext;
begin
  Result := TdxEntityManager.Context;
end;

function TdxMappingMemberInfo.ConvertArray(ABytes: TBytes): TValue;
begin
  if RttiType.Handle <> TypeInfo(TBytes) then
    Result := TValue.ConvertArray(TypeInfo(TBytes), RttiType.Handle, ABytes)
  else
    Result := TValue.From<TBytes>(ABytes);
end;

function TdxMappingMemberInfo.DBType(const ADBColumnTypeName: string): IdxMappingMemberInfo;
begin
  DBColumnTypeName := ADBColumnTypeName;
  Result := Self;
end;

function TdxMappingMemberInfo.Default(const ADefaultValue: Variant): IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Default);
  DefaultValue := ADefaultValue;
  Result := Self;
end;

function TdxMappingMemberInfo.FindAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
var
  AAttributes: TArray<TCustomAttribute>;
begin
  AAttributes := Member.GetAttributes;
  for Result in AAttributes do
    if Result is AClass then
      Exit;
  Result := nil;
end;

function TdxMappingMemberInfo.GetAggregated: Boolean;
begin
  Result := TdxAttribute.Aggregated in Attributes;
end;

function TdxMappingMemberInfo.GetAssociatedMember: TdxMappingMemberInfo;
var
  AAssociatedName, AAssociationName, ARelAssociationName: string;
  ARelRefEntityInfo, ARefEntityInfo: TdxEntityInfo;
  AResult, AMemberInfo: TdxMappingMemberInfo;
begin
  if FAssociatedMember = nil then
  begin
    if FAssociationAttributeInfo.HasValue then
      AAssociationName := FAssociationAttributeInfo.Value.AssociationName
    else
      AAssociationName := '';
    if not Owner.ClassAttributes.IsPersistent then
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidNonPersistentClassInTheAssociation,
        [Owner.ClassAttributes.EntityName, MemberName, Owner.ClassAttributes.EntityName]);
    if IsAssociationList then
      ARefEntityInfo := CollectionElementType
    else
      if (ReferenceType <> nil) and IsPersistent then
        ARefEntityInfo := ReferenceType
      else
        raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidAssociationAttributeOnlyForCollectionOrReference,
          [Owner.FullName, ActualName]);
    if not ARefEntityInfo.ClassAttributes.IsPersistent then
      raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidNonPersistentClassInTheAssociation,
        [ARefEntityInfo.ClassAttributes.EntityName, MemberName, ARefEntityInfo.ClassAttributes.EntityName]);
    AResult := nil;
    for AMemberInfo in ARefEntityInfo.MemberAttributes do
    begin
      if Self = AMemberInfo then
        Continue;
      if ((AMemberInfo.ReferenceType = nil) or (not AMemberInfo.IsPersistent)) and not AMemberInfo.IsAssociationList then
        Continue;
      if not (TdxAttribute.Association in AMemberInfo.Attributes) then
        Continue;
      if AMemberInfo.FAssociationAttributeInfo.HasValue then
        ARelAssociationName := AMemberInfo.FAssociationAttributeInfo.Value.AssociationName
      else
        ARelAssociationName := '';
      if AAssociationName <> ARelAssociationName then
        Continue;
      if AMemberInfo.IsAssociationList then
        ARelRefEntityInfo := AMemberInfo.CollectionElementType
      else
        ARelRefEntityInfo := AMemberInfo.ReferenceType;
      if not ((ARelRefEntityInfo = Owner) or
        Owner.ClassAttributes.PersistentClass.InheritsFrom(ARelRefEntityInfo.ClassAttributes.PersistentClass)) then
      begin
        if Length(AAssociationName) = 0 then
          Continue;
        raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidPropertyTypeMismatch,
          [AAssociationName, Owner.FullName, MemberName, AMemberInfo.Owner.FullName, AMemberInfo.MemberName, ARelRefEntityInfo.FullName]);
      end;
      if AResult <> nil then
        raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidMoreThanOneAssociatedMemberFound,
          [AAssociationName, AResult.MemberName, AMemberInfo.MemberName, ARefEntityInfo.FullName]);
      if not IsAssociationList and not AMemberInfo.IsAssociationList then
        raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidNoAssociationCollectionInAssociation,
          [AAssociationName, Owner.FullName, MemberName, AMemberInfo.Owner.FullName, AMemberInfo.MemberName]);
      if IsAssociationList and AMemberInfo.IsAssociationList then
      begin
        if not (IsCollection and AMemberInfo.IsCollection) then
          raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidNoAssociationCollectionInAssociation,
          [AAssociationName, Owner.FullName, MemberName, AMemberInfo.Owner.FullName, AMemberInfo.MemberName]);
      end;
      AResult := AMemberInfo;
    end;
    FAssociatedMember := AResult;
      if AResult = nil then
        raise EdxAssociationInvalidException.CreateFmt(sdxMetadataAssociationInvalidAssociatedMemberNotFound,
          [AAssociatedName, Owner.FullName, ActualName, ARefEntityInfo.FullName]);
  end;
  Result := FAssociatedMember;
end;

function TdxMappingMemberInfo.GetCollectionElementType: TdxEntityInfo;
var
  AMemberType, AType: TRttiType;
begin
  if IsAssociationList and (FCollectionElementType = nil) then
  begin
    case AttributeType of
      TdxAttributeType.&Property:
        AMemberType := TRttiProperty(Member).PropertyType;
      TdxAttributeType.Field:
        AMemberType := TRttiField(Member).FieldType;
      else
        raise ENotSupportedException.Create('');
    end;
    AType := GetGenericIListTypeArgument(AMemberType);
    if AType <> nil then
      FCollectionElementType := EntityManager.GetEntityInfo(TRttiInstanceType(AType).MetaclassType);
  end;
  Result := FCollectionElementType;
end;

function TdxMappingMemberInfo.GetColumnName: string;
begin
  Result := Self.AttributeName;
end;

function TdxMappingMemberInfo.GetConverter: TdxValueConverter;
begin
  Result := FValueConverter;
end;

function TdxMappingMemberInfo.GetFieldType: TFieldType;
var
  ADBColumnType: TdxDBColumnType;
begin
  if FFieldType.HasValue then
    Exit(FFieldType.Value);
  if IsIdentity then
    Result := TFieldType.ftAutoInc
  else
  begin
    ADBColumnType := TdxDBColumn.GetColumnType(RttiType.Handle);
    case ADBColumnType of
      TdxDBColumnType.Boolean:
        Result := TFieldType.ftBoolean;
      TdxDBColumnType.Byte:
        Result := TFieldType.ftByte;
      TdxDBColumnType.SByte:
        Result := TFieldType.ftShortint;
      TdxDBColumnType.Char:
        if DBColumnSize > 1 then
          Result := TFieldType.ftWideString
        else
          Result := TFieldType.ftFixedWideChar;
      TdxDBColumnType.Decimal:
        if IsType(TypeInfo(Currency)) then
          Result := TFieldType.ftCurrency
        else
          Result := TFieldType.ftFmtBcd;
      TdxDBColumnType.Double:
        Result := TFieldType.ftFloat;
      TdxDBColumnType.Single:
        Result := TFieldType.ftSingle;
      TdxDBColumnType.Int32:
        Result := TFieldType.ftInteger;
      TdxDBColumnType.UInt32:
        Result := TFieldType.ftLongWord;
      TdxDBColumnType.Int16:
        Result := TFieldType.ftSmallint;
      TdxDBColumnType.UInt16:
        Result := TFieldType.ftWord;
      TdxDBColumnType.Int64:
        Result := TFieldType.ftLargeint;
      TdxDBColumnType.UInt64:
        Result := TFieldType.ftLargeint;
      TdxDBColumnType.&String:
        Result := TFieldType.ftWideString;
      TdxDBColumnType.DateTime:
        Result := TFieldType.ftDateTime;
      TdxDBColumnType.Guid:
        Result := TFieldType.ftGuid;
      TdxDBColumnType.ByteArray:
        Result := TFieldType.ftBlob;
      TdxDBColumnType.TimeSpan:
        Result := TFieldType.ftTimeStamp;
    else
      if (RttiType.Handle.Kind = tkClass) and IsSerialize then
        Result := Serializer.GetStorageDBType
      else
        Result := TFieldType.ftUnknown;
    end;
  end;
  FFieldType.Value := Result;
end;

class function TdxMappingMemberInfo.GetGenericIListTypeArgument(AElementType: TRttiType): TRttiType;
var
  ATypeInfo: PTypeInfo;
  APos: Integer;
  ATypeInfoName: string;
  AGenericArgumentName: string;
begin
  if AElementType = nil then
    Exit(nil);
  ATypeInfo := AElementType.Handle;
  ATypeInfoName := string(ATypeInfo.Name);
  APos := Pos('<', ATypeInfoName);
  if APos > 0 then
  begin
    AGenericArgumentName := Copy(ATypeInfoName, APos + 1, Pos('>', ATypeInfoName) - APos - 1);
    Result := Context.FindType(AGenericArgumentName);
  end
  else
    Result := nil;
end;

function TdxMappingMemberInfo.GetIsAliased: Boolean;
begin
  Result := False;
end;

function TdxMappingMemberInfo.GetIsAssociation: Boolean;
begin
  Result := TdxAttribute.Association in Attributes;
end;

function TdxMappingMemberInfo.GetIsAssociationObject: Boolean;
begin
  Result := (TdxAttribute.Association in Attributes) and MemberType.IsInstance;
end;

function TdxMappingMemberInfo.GetIsAutoGenerate: Boolean;
begin
  if TdxAttribute.Generator in Attributes then
    Result := DBGenerator.GeneratorType in [TdxGeneratorType.Identity, TdxGeneratorType.GUID, TdxGeneratorType.SequentialGUID, TdxGeneratorType.Sequence]
  else
    Result := False;
end;

function TdxMappingMemberInfo.GetIsBLOB: Boolean;
begin
  Result := TdxAttribute.Blob in Attributes;
end;

function TdxMappingMemberInfo.GetIsVirtualColumn: Boolean;
begin
  Result := TdxAttribute.VirtualColumn in Attributes;
end;

function TdxMappingMemberInfo.GetIsCollection: Boolean;
begin
  Result := IsAssociationList;
end;

function TdxMappingMemberInfo.GetIsColumn: Boolean;
begin
  Result := [TdxAttribute.Column, TdxAttribute.Association, TdxAttribute.VirtualColumn] * Attributes <> [];
end;

function TdxMappingMemberInfo.GetIsDelayed: Boolean;
begin
  Result := TdxAttribute.Delayed in Attributes;
end;

function TdxMappingMemberInfo.GetIsEnumeration: Boolean;
begin
  Result := (TypeKind = tkEnumeration) and (MemberType.Handle <> TypeInfo(Boolean));
end;

function TdxMappingMemberInfo.GetIsExpandableToPersistent: Boolean;
begin
  Result := IsAssociationList;
end;

function TdxMappingMemberInfo.GetIsIdentity: Boolean;
begin
  Result := (DBGenerator.GeneratorType = TdxGeneratorType.Identity) or
    ((TypeKind in [TTypeKind.tkInteger, TTypeKind.tkInt64]) and IsAutoGenerate);
end;

function TdxMappingMemberInfo.GetIsKey: Boolean;
begin
  Result := TdxAttribute.Key in Attributes;
end;

function TdxMappingMemberInfo.GetIsLoadable: Boolean;
begin
  Result := IsWritable or IsSerialize;
end;

function TdxMappingMemberInfo.GetIsManyToMany: Boolean;
begin
  Result := False;
end;

function TdxMappingMemberInfo.GetIsNullable: Boolean;
begin
  Result := TdxAttribute.Nullable in Attributes;
end;

function TdxMappingMemberInfo.GetIsPersistent: Boolean;
begin
  Result := [TdxAttribute.NonPersistent, TdxAttribute.VirtualColumn] * Attributes = [];
end;

function TdxMappingMemberInfo.GetIsReadable: Boolean;
begin
  Result := (AttributeType = TdxAttributeType.Field) or
    ((AttributeType = TdxAttributeType.&Property) and TRttiProperty(Member).IsReadable);
end;

function TdxMappingMemberInfo.GetIsReadOnly: Boolean;
begin
  Result := TdxAttribute.ReadOnly in Attributes;
end;

function TdxMappingMemberInfo.GetIsReference: Boolean;
begin
  Result := ReferenceType <> nil;
end;

function TdxMappingMemberInfo.GetIsRequired: Boolean;
begin
  Result := not (IsNullable or IsIdentity or (FieldType in [Low(TBlobType)..High(TBlobType)]));
end;

function TdxMappingMemberInfo.GetIsSerialize: Boolean;
begin
  Result := Serializer <> nil;
end;

function TdxMappingMemberInfo.GetIsStruct: Boolean;
begin
  Result := False;
end;

function TdxMappingMemberInfo.GetIsSupportType: Boolean;
begin
  Result := True;
end;

function TdxMappingMemberInfo.GetIsUnique: Boolean;
begin
  Result := TdxAttribute.Unique in Attributes;
end;

function TdxMappingMemberInfo.GetIsWritable: Boolean;
begin
  Result := (AttributeType = TdxAttributeType.Field) or
    ((AttributeType = TdxAttributeType.&Property) and TRttiProperty(Member).IsWritable);
end;

function TdxMappingMemberInfo.GetMappingClass(ABranch: TdxEntityInfo): TdxEntityInfo;
var
  ACurrentClass: TClass;
  AFirstPersistentOwnerDescendant: TdxEntityInfo;
begin
  if ABranch = nil then
    Exit(nil);
  if not Owner.ClassAttributes.PersistentClass.InheritsFrom(ABranch.ClassAttributes.PersistentClass) then
    Exit(nil);
  if (MemberName <> '') and (ABranch.FindMember(MemberName) = nil) then
    Exit(nil);
  if IsKey then
    Exit(ABranch);
  Result := Owner;


  AFirstPersistentOwnerDescendant := ABranch;
  repeat
    if AFirstPersistentOwnerDescendant.ClassAttributes.PersistentClass = GetPersistentClass then
      Exit(AFirstPersistentOwnerDescendant);
    ACurrentClass := AFirstPersistentOwnerDescendant.ClassAttributes.PersistentBaseClass;
    if (ACurrentClass = nil) and ABranch.ClassAttributes.IsInheritance and (not AFirstPersistentOwnerDescendant.ClassAttributes.IsInheritance) then
      Exit(AFirstPersistentOwnerDescendant);
    if (AFirstPersistentOwnerDescendant.ParentEntity = nil) or
      (not ACurrentClass.InheritsFrom(AFirstPersistentOwnerDescendant.ParentEntity.ClassAttributes.PersistentClass)) then
      Exit(AFirstPersistentOwnerDescendant);
    AFirstPersistentOwnerDescendant := EntityManager.GetEntityInfo(ACurrentClass);
  until AFirstPersistentOwnerDescendant = nil;


end;

function TdxMappingMemberInfo.GetMappingField: string;
begin
  Result := AttributeName;
end;

function TdxMappingMemberInfo.GetMemberName: string;
begin
  Result := FMemberName;
end;

function TdxMappingMemberInfo.GetMemberType: TRttiType;
begin
  if Member is TRttiField then
    Result := TRttiField(Member).FieldType
  else
    if Member is TRttiProperty then
      Result := TRttiProperty(Member).PropertyType
    else
      Result := nil;
end;

function TdxMappingMemberInfo.GetParentClass: TClass;
begin
  if Member.Parent = nil then
    Result := nil
  else
    Result := (Member.Parent as TRttiInstanceType).MetaclassType;
end;

function TdxMappingMemberInfo.GetPersistentClass: TClass;
begin
  Result := TRttiInstanceType(Member.Parent).MetaclassType;
end;

function TdxMappingMemberInfo.GetValue(AObject: TObject): TValue;
begin
  if AObject = nil then
    Exit(TValue.Empty);
  case AttributeType of
    TdxAttributeType.&Property:
      Result := TRttiProperty(Member).GetValue(AObject);
    TdxAttributeType.Field:
      Result := TRttiField(Member).GetValue(AObject);
    else
      Result := TValue.Empty;
  end;
end;

function TdxMappingMemberInfo.Unique: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Unique);
  Result := Self;
end;

procedure TdxMappingMemberInfo.UpdateReferenceType;
begin
  if (FReferenceType = nil) and {(TdxAttribute.Association in Attributes) and} not FIsAssociationList then
    FReferenceType := EntityManager.GetEntityInfo(MemberType.Handle.TypeData.ClassType);
end;

function TdxMappingMemberInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TdxMappingMemberInfo._Release: Integer;
begin
  Result := -1;
end;

function TdxMappingMemberInfo.Indexed: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Indexed);
  Result := Self;
end;

function TdxMappingMemberInfo.IsMappingClass(ABranch: TdxEntityInfo): Boolean;
var
  AMappingClass: TdxEntityInfo;
begin
  AMappingClass := GetMappingClass(ABranch);
  if AMappingClass = nil then
    Result := False
  else
    Result := SameText(ABranch.TableName, AMappingClass.TableName);
end;

function TdxMappingMemberInfo.IsMember(const AMemberName: string): Boolean;
begin
  case AttributeType of
    TdxAttributeType.Field:
      Result := SameText(TdxEntityManager.GetQualifiedFieldName(AMemberName), MemberName);
    TdxAttributeType.Property:
      Result := SameText(AMemberName, MemberName);
    else
      Result := False;
  end;

end;

function TdxMappingMemberInfo.IsType(ATypeInfo: PTypeInfo): Boolean;
var
  AMemberType: TRttiType;
begin
  AMemberType := MemberType;
  if AMemberType = nil then
    Result := False
  else
    Result := MemberType.Handle = ATypeInfo;
end;

function TdxMappingMemberInfo.Key: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Key);
  if not Owner.KeyAttributes.Contains(Self) then
    Owner.KeyAttributes.Add(Self);
  Result := Self;
end;

function TdxMappingMemberInfo.NoForeignKey: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.NoForeignKey);
  Result := Self;
end;

function TdxMappingMemberInfo.NonPersistent: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.NonPersistent);
  Result := Self;
end;

function TdxMappingMemberInfo.Nullable: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Nullable);
  Result := Self;
end;

function TdxMappingMemberInfo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxMappingMemberInfo.ReadOnly: IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.ReadOnly);
  Result := Self;
end;

function TdxMappingMemberInfo.RegisterField(const AFieldName: string): IdxMappingMemberInfo;
begin
  Result := Owner.ClassAttributes.RegisterField(AFieldName);
end;

function TdxMappingMemberInfo.RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;
begin
  Result := Owner.ClassAttributes.RegisterProperty(APropertyName);
end;

procedure TdxMappingMemberInfo.SetFieldType(AFieldType: TFieldType);
begin
  FFieldType := AFieldType;
end;

procedure TdxMappingMemberInfo.SetIsDynamicArrayType(const Value: Boolean);
begin
  FIsDynamicArrayType := Value;
end;

procedure TdxMappingMemberInfo.SetMember(const Value: TRttiMember);
begin
  FMember := Value;
end;

procedure TdxMappingMemberInfo.SetMemberName(const AValue: string);
begin
  FMemberName := AValue;
end;

procedure TdxMappingMemberInfo.SetMemberValue(AObject: TObject; const AValue: TValue);
begin
  case AttributeType of
    TdxAttributeType.&Property:
      TRttiProperty(Member).SetValue(AObject, AValue);
    TdxAttributeType.Field:
      TRttiField(Member).SetValue(AObject, AValue);
  end;
end;

procedure TdxMappingMemberInfo.SetSerializer(ASerializerClass: TdxCustomBlobSerializerClass);
begin
  if ASerializerClass = nil then
    Exit;
  if FSerializer <> nil then
    Exit;
  FSerializer := ASerializerClass.Create;
end;

procedure TdxMappingMemberInfo.SetValue(AObject: TObject; const AValue: TValue);
var
  AHandle: PTypeInfo;
begin
  if TypeKind = tkEnumeration then
  begin
    AHandle := RttiType.Handle;
    if AHandle <> AValue.TypeInfo then
    begin
      SetMemberValue(AObject, TValue.FromOrdinal(AHandle, AValue.AsInt64));
      Exit;
    end;
  end;
  SetMemberValue(AObject, AValue);
end;

function TdxMappingMemberInfo.Size(ASize: Integer): IdxMappingMemberInfo;
begin
  Include(Attributes, TdxAttribute.Size);
  if ASize = -1 then
    Blob
  else
    DBColumnSize := ASize;
  Result := Self;
end;

{ TdxMemberInfoCollection }

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo; const AMatches: TArray<string>;
  AAddNonPersistent: Boolean; AThrowOnError: Boolean);
var
  ACurrentEntityInfo: TdxEntityInfo;
  I: Integer;
  AMemberInfo: TdxMappingMemberInfo;
begin
  Create(AEntityInfo);
  ACurrentEntityInfo := AEntityInfo;
  I := 0;
  while (I < Length(AMatches)) and (ACurrentEntityInfo <> nil) do
  begin
    AMemberInfo := FindMember(ACurrentEntityInfo, AMatches[I]);
    while ((AMemberInfo <> nil) and AMemberInfo.IsStruct) and (I < Length(AMatches) - 1) do
    begin
      Inc(I);
      AMemberInfo := ACurrentEntityInfo.FindMember(AMemberInfo.MemberName + '.' + AMatches[I]);
    end;
    if (AMemberInfo = nil) or (not AAddNonPersistent and not AMemberInfo.IsExpandableToPersistent) or
      ((I < Length(AMatches) - 1) and (AMemberInfo.ReferenceType = nil)) then
    begin
      if AThrowOnError then
        raise EdxInvalidPropertyPathException.CreateFmt(sdxMetadataIncorrectPathMemberDoesNotExist,
          [TdxStringHelper.Join('.', AMatches), AEntityInfo.ClassAttributes.PersistentClass.ClassName,
          ACurrentEntityInfo.ClassAttributes.EntityName, AMatches[I]]);
      Clear;
      Exit;
    end;
    if not AMemberInfo.IsExpandableToPersistent then
      FHasNonPersistent := True;
    Add(AMemberInfo);
    ACurrentEntityInfo := AMemberInfo.ReferenceType;
    Inc(I);
  end;
end;

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo; const APath: string; AAddNonPersistent: Boolean; AThrowOnError: Boolean);
begin
  Create(AEntityInfo, SplitPath(APath), AAddNonPersistent, AThrowOnError);
end;

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo; const APath: string; AAddNonPersistent: Boolean);
begin
  Create(AEntityInfo, APath, AAddNonPersistent, True);
end;

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo; const APath: string);
begin
  Create(AEntityInfo, APath, False);
end;

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo; const AMembers: array of TdxMappingMemberInfo);
begin
  Create(AEntityInfo);
  AddRange(AMembers);
end;

constructor TdxMemberInfoCollection.Create(AEntityInfo: TdxEntityInfo);
begin
  inherited Create;
  FEntityInfo := AEntityInfo;
end;

class function TdxMemberInfoCollection.ParsePath(AEntityInfo: TdxEntityInfo; const APath: string): TdxMemberInfoCollection;
begin
  Result := AEntityInfo.ParsePath(APath);
end;

class function TdxMemberInfoCollection.ParsePersistentPath(AEntityInfo: TdxEntityInfo; const APath: string): TdxMemberInfoCollection;
begin
  Result := AEntityInfo.ParsePersistentPath(APath);
end;

procedure TdxMemberInfoCollection.Remove(ACollection: TEnumerable<TdxMappingMemberInfo>);
var
  AMappingMemberInfo: TdxMappingMemberInfo;
begin
  for AMappingMemberInfo in ACollection do
    inherited Remove(AMappingMemberInfo);
end;

class function TdxMemberInfoCollection.FindMember(ACurrentEntityInfo: TdxEntityInfo; const AMatch: string): TdxMappingMemberInfo;
var
  APos, ADotPos: Integer;
  AClassName, AMemberName: string;
  AResolvedWithNamespace, AEntityInfo: TdxEntityInfo;
  AResult: TdxMappingMemberInfo;
  AShouldBeClassNameDelimiter: Char;
begin
  if (Length(AMatch) = 0) or (AMatch[1] <> '<') then
    Exit(ACurrentEntityInfo.FindMember(AMatch));

  APos := TdxStringHelper.IndexOf(AMatch, '>');
  if APos < 0 then
    Exit(nil);
  AClassName := TdxStringHelper.Substring(AMatch, 1, APos - 1);
  AMemberName := TdxStringHelper.Substring(AMatch, APos + 1);
  if TryResolveType(AClassName, ACurrentEntityInfo, AResolvedWithNamespace) then
  begin
    if (AResolvedWithNamespace <> nil) and AResolvedWithNamespace.IsAssignableTo(ACurrentEntityInfo) then
    begin
      AResult := FindMember(AResolvedWithNamespace, AMemberName);
      if AResult <> nil then
        Exit(AResult);
    end;
  end;
  Result := nil;
  for AEntityInfo in EntityManager do
  begin
    if not TdxStringHelper.EndsWith(AEntityInfo.FullName, AClassName) then
      Continue;
    ADotPos := Length(AEntityInfo.FullName) - Length(AClassName) - 1;
    if ADotPos >= 0 then
    begin
      AShouldBeClassNameDelimiter := AEntityInfo.FullName[ADotPos];
      if (AShouldBeClassNameDelimiter <> '.') and (AShouldBeClassNameDelimiter <> '+') then
        Continue;
    end;
    AResult := FindMember(AEntityInfo, AMemberName);
    if AResult = nil then
      Continue;
    if (AEntityInfo.IsPersistent and ACurrentEntityInfo.IsPersistent) and
      (AEntityInfo.ClassAttributes.PersistentClass <> ACurrentEntityInfo.ClassAttributes.PersistentClass) then
      Continue;
    if Result <> nil then
      raise EInvalidOperation.Create('sdxMetadataAmbiguousClassName'{, AClassName, AResult.Owner.FullName, Result.Owner.FullName)});
    Result := AResult;
  end;
end;

function TdxMemberInfoCollection.ToString: string;
var
  ACurrentEntityInfo: TdxEntityInfo;
  ARes: TStringBuilder;
  I: Integer;
  AName: string;
begin
  ACurrentEntityInfo := FEntityInfo;
  ARes := TStringBuilder.Create;
  try
    for I := 0 to Count - 1 do
    begin
      if ARes.Length <> 0 then
        ARes.Append('.');
      if Self[I] <> nil then
      begin
        if (ACurrentEntityInfo <> nil) and not ACurrentEntityInfo.IsAssignableTo(Self[I].Owner) then
        begin
          AName := Self[I].Owner.FullName;

          ARes.Append('<');
          ARes.Append(AName);
          ARes.Append('>');
        end;
        ARes.Append(Self[I].MemberName);
        ACurrentEntityInfo := Self[I].ReferenceType;
      end
      else
        ARes.Append('^');
    end;
    Result := ARes.ToString;
  finally
    ARes.Free;
  end;
end;

class function TdxMemberInfoCollection.SplitPath(const APath: string): TArray<string>;
var
  AResult: TList<string>;
  AInUpCast: Boolean;
  APrevCutPos, I: Integer;
begin
  {$IFDEF DELPHIXE3}
  if APath.IndexOf('.') < 0 then
    Exit(TArray<string>.Create(APath));
  if APath.IndexOfAny(UpCastSymbols) < 0 then
    Exit(APath.Split(['.']));
  {$ELSE}
  if TdxStringHelper.IndexOf(APath, '.') < 0 then
    Exit(TArray<string>.Create(APath));
  if TdxStringHelper.IndexOfAny(APath, UpCastSymbols) < 0 then
    Exit(TdxStringHelper.Split(APath, ['.']));
  {$ENDIF}
  AResult := TList<string>.Create;
  try
    AInUpCast := False;
    APrevCutPos := 0;
    for I := 1 to Length(APath) do
    begin
      case APath[I] of
        '.':
          begin
            if AInUpCast then
              Continue;
            AResult.Add(TdxStringHelper.Substring(APath, APrevCutPos, I - APrevCutPos - 1));
            APrevCutPos := I;
          end;
        '<':
          AInUpCast := True;
        '>':
          AInUpCast := False;
        else
      end;
    end;
    if APrevCutPos < Length(APath) then
    begin
      if APrevCutPos = 0 then
        Exit(TArray<string>.Create(APath))
      else
        AResult.Add(TdxStringHelper.Substring(APath, APrevCutPos, Length(APath) - APrevCutPos));
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

class function TdxMemberInfoCollection.LastIndexOfSplittingDotInPath(const APath: string): Integer;
var
  ALastDotPos, AClosingAngBr, AOpeningAngBr: Integer;
begin
  ALastDotPos := {$IFDEF DELPHIXE3}APath.LastIndexOf('.'){$ELSE}TdxStringHelper.LastIndexOf(APath, '.'){$ENDIF};
  if ALastDotPos < 0 then
    Exit(ALastDotPos);
  AClosingAngBr := {$IFDEF DELPHIXE3}APath.IndexOf('>', ALastDotPos){$ELSE}TdxStringHelper.IndexOf(APath, '>', ALastDotPos){$ENDIF};
  if AClosingAngBr < 0 then
    Exit(ALastDotPos);
  AOpeningAngBr := {$IFDEF DELPHIXE3}APath.LastIndexOf('<', AClosingAngBr){$ELSE}TdxStringHelper.LastIndexOf(APath, '<', AClosingAngBr){$ENDIF};
  if AOpeningAngBr < 0 then
    raise EInvalidOperation.Create('Unbalanced <UpCasting> within '#$27 + APath + #$27' property path');
  Result := {$IFDEF DELPHIXE3}APath.LastIndexOf('.', AOpeningAngBr){$ELSE}TdxStringHelper.LastIndexOf(APath, '.', AOpeningAngBr){$ENDIF};
end;

class function TdxMemberInfoCollection.GetSplitPartCount(const APath: string): Integer;
var
  ACount, APos: Integer;
  ACh: Char;
begin
  if Length(APath) = 0 then
    Exit(0);
  ACount := 1;
  APos := 0;
  while APos < Length(APath) - 1 do
  begin
    ACh := APath[APos + 1];
    if ACh = '.' then
      Inc(ACount)
    else
      if ACh = '<' then
      begin
        while APath[APos + 1] <> '>' do
        begin
          Inc(APos);
          if APos >= Length(APath) then
            raise EInvalidOperation.Create('Unbalanced UpCasting '#$27 + APath + #$27);
        end;
      end;
    Inc(APos);
  end;
  Result := ACount;
end;

class function TdxMemberInfoCollection.TryResolveType(const AClassName: string; ARootEntityInfo: TdxEntityInfo;
  out AEntityInfo: TdxEntityInfo): Boolean;
var
  ANamespaceLenInCurrentClassInfo: Integer;
  AResolvedWithNamespace: TdxEntityInfo;
begin
  AEntityInfo := nil;
  {$IFDEF DELPHIXE3}
  ANamespaceLenInCurrentClassInfo := ARootEntityInfo.FullName.LastIndexOfAny(NamespaceSplitters);
  {$ELSE}
  ANamespaceLenInCurrentClassInfo := TdxStringHelper.LastIndexOfAny(ARootEntityInfo.FullName, NamespaceSplitters);
  {$ENDIF}
  if ANamespaceLenInCurrentClassInfo >= 0 then
  begin
    if {$IFDEF DELPHIXE3}AClassName.LastIndexOfAny(NamespaceSplitters){$ELSE}TdxStringHelper.LastIndexOfAny(AClassName, NamespaceSplitters){$ENDIF} >= 0 then
      AResolvedWithNamespace := EntityManager.GetEntityInfo(ARootEntityInfo.EntityUnitName + '.' + AClassName)
    else
      AResolvedWithNamespace := EntityManager.GetEntityInfo(ARootEntityInfo.EntityUnitName + '.' +
      {$IFDEF DELPHIXE3}
        ARootEntityInfo.FullName.Substring(0, ANamespaceLenInCurrentClassInfo + 1) + AClassName);
      {$ELSE}
        TdxStringHelper.Substring(ARootEntityInfo.FullName, 0, ANamespaceLenInCurrentClassInfo + 1) + AClassName);
      {$ENDIF}
    AEntityInfo := AResolvedWithNamespace;
  end;
  Result := AEntityInfo <> nil;
end;

class function TdxMemberInfoCollection.TryResolveTypeAlsoByShortName(const AClassName: string;
  ARootEntityInfo: TdxEntityInfo; out AEntityInfo: TdxEntityInfo): Boolean;
var
  AIsFullClassName: Boolean;
  ADotIndex: Integer;
  AEntity: TdxEntityInfo;
begin
  AEntityInfo := nil;
  if not TdxMemberInfoCollection.TryResolveType(AClassName, ARootEntityInfo, AEntityInfo) then
  begin
  {$IFDEF DELPHIXE3}
    AIsFullClassName := AClassName.LastIndexOfAny(NamespaceSplitters) >= 0;
  {$ELSE}
    AIsFullClassName := TdxStringHelper.LastIndexOfAny(               AClassName, NamespaceSplitters) >= 0;
  {$ENDIF}
    for AEntity in EntityManager do
    begin
      if AEntity.FullName <> AClassName then
      begin
        if AIsFullClassName then
          Continue
        else
        begin
          if AEntity.ClassType = nil then
          begin
          {$IFDEF DELPHIXE3}
            ADotIndex := AEntity.FullName.LastIndexOfAny(NamespaceSplitters);
          {$ELSE}
            ADotIndex := TdxStringHelper.LastIndexOfAny(AEntity.FullName, NamespaceSplitters);
          {$ENDIF}
            if not ((((ADotIndex >= 0) and
            {$IFDEF DELPHIXE3}
              (AEntity.FullName.Substring(ADotIndex) = AClassName))) or
            {$ELSE}
              (TdxStringHelper.Substring(AEntity.FullName, ADotIndex) = AClassName))) or
            {$ENDIF}
              (((ADotIndex < 0) and (AEntity.FullName = AClassName)))) then
              Continue;
          end
          else
            if AEntity.FullName <> AClassName then
              Continue;
        end;
      end;
      AEntityInfo := AEntity;
      Exit(True);
    end;
    Result := False;
  end
  else
    Result := True;
end;

{ TdxMappingClassInfo }

procedure TdxMappingClassInfo.Assign(ASource: TdxAttributeInfo);
var
  AMappingClass: TdxMappingClassInfo;
begin
  inherited Assign(ASource);
  AMappingClass := Safe<TdxMappingClassInfo>.Cast(ASource);
  if AMappingClass = nil then
    Exit;
  FClass := AMappingClass.FClass;
  FKeyMemberNames.Clear;
  FKeyMemberNames.AddRange(AMappingClass.FKeyMemberNames);
  FDiscriminatorValue := AMappingClass.FDiscriminatorValue;
  FDiscriminatorInfo := AMappingClass.FDiscriminatorInfo;
  MapInheritance := AMappingClass.MapInheritance;
end;

constructor TdxMappingClassInfo.Create(AOwner: TdxEntityInfo; AClass: TClass);
begin
  inherited Create(AOwner);
  AttributeType := TdxAttributeType.&Class;
  TypeKind := tkClass;
  FKeyMemberNames := TList<string>.Create;
  FClass := AClass;
  MapInheritance := TdxMapInheritanceType.OwnTable;
  FIndexes := TdxIndexes.Create;
  FDiscriminatorValue := Null;
end;

destructor TdxMappingClassInfo.Destroy;
begin
  FreeAndNil(FKeyMemberNames);
  FreeAndNil(FIndexes);
  inherited Destroy;
end;

function TdxMappingClassInfo.Automapping: IdxMappingClassInfo;
begin
  if not (TdxAttribute.Automapping in Attributes) then
  begin
    EntityManager.AutoMappingAttributes(EntityManager.Context.GetType(PersistentClass), Owner);
  end;
  Include(Attributes, TdxAttribute.Automapping);
  Result := Self;
end;

function TdxMappingClassInfo.Discriminator(AValue: Integer): IdxMappingClassInfo;
begin
  FDiscriminatorValue := AValue;
  Result := Self;
end;

function TdxMappingClassInfo.Discriminator(const AValue: string): IdxMappingClassInfo;
begin
  FDiscriminatorValue := AValue;
  Result := Self;
end;

function TdxMappingClassInfo.DiscriminatorColumn: IdxMappingClassInfo;
begin
  FDiscriminatorInfo := TdxDiscriminator.CreateDefault;
  Result := Self;
end;

function TdxMappingClassInfo.DiscriminatorColumn(const AColumnName: string): IdxMappingClassInfo;
begin
  FDiscriminatorInfo := TdxDiscriminator.Create(TdxDiscriminatorType.Integer, AColumnName);
  Result := Self;
end;

function TdxMappingClassInfo.DiscriminatorColumn(const AColumnName: string; ADiscriminatorType: TdxDiscriminatorType;
  ASize: Integer): IdxMappingClassInfo;
begin
  FDiscriminatorInfo := TdxDiscriminator.Create(ADiscriminatorType, AColumnName, ASize);
  Result := Self;
end;

function TdxMappingClassInfo.FindAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
var
  AAttributes: TArray<TCustomAttribute>;
begin
  AAttributes := RttiType.GetAttributes;
  for Result in AAttributes do
    if Result is AClass then
      Exit;
  Result := nil;
end;

function TdxMappingClassInfo.GetEntityName: string;
begin
  Result := FClass.ClassName;
end;

function TdxMappingClassInfo.GetInheritance: Boolean;
begin
  Result := TdxAttribute.Inheritance in Attributes;
end;

function TdxMappingClassInfo.GetIdEntity: TdxEntityInfo;
var
  AEntityInfo: TdxEntityInfo;
begin
  if FIdEntity = nil then
  begin
    AEntityInfo := Owner;
    while AEntityInfo <> nil do
    begin
      if AEntityInfo.IsPersistent then
        FIdEntity := AEntityInfo;
      AEntityInfo := AEntityInfo.BaseEntityInfo;
    end;
  end;
  Result := FIdEntity;
end;

function TdxMappingClassInfo.GetIsDiscriminatorColumn: Boolean;
begin
  Result := FDiscriminatorInfo.HasValue;
end;

function TdxMappingClassInfo.GetIsOwnTable: Boolean;
begin
  Result := IsInheritance and (MapInheritance = TdxMapInheritanceType.OwnTable);
end;

function TdxMappingClassInfo.GetIsParentTable: Boolean;
begin
  Result := IsInheritance and (MapInheritance = TdxMapInheritanceType.ParentTable);
end;

function TdxMappingClassInfo.GetIsPersistent: Boolean;
begin
  Result := True;
end;

function TdxMappingClassInfo.GetPersistentBaseClass: TClass;
var
  AClassType: TRttiInstanceType;
  AEntityInfo: TdxEntityInfo;
begin
  AClassType := (RttiType as TRttiInstanceType).BaseType;
  while AClassType <> nil do
  begin
    if TdxEntityInfo.IsEntity(AClassType) then
    begin
      AEntityInfo := EntityManager.GetEntityInfo(AClassType.MetaclassType);
      if not (AEntityInfo.ClassAttributes.IsPersistent and AEntityInfo.ClassAttributes.IsParentTable) then
        Exit(AClassType.MetaclassType);
    end;
    AClassType := AClassType.BaseType;
  end;
  Result := nil;
end;

function TdxMappingClassInfo.GetTableName: string;
var
  AEntityInfo: TdxEntityInfo;
begin
  if FTableName = '' then
  begin
    AEntityInfo := Owner;
    if AEntityInfo.ClassAttributes.IsInheritance and
      (AEntityInfo.ClassAttributes.MapInheritance = TdxMapInheritanceType.ParentTable) then
    begin
      AEntityInfo := EntityManager.GetEntityInfo(AEntityInfo.ClassAttributes.PersistentBaseClass);
    end;
    if AEntityInfo.ClassAttributes.FTableName = '' then
      AEntityInfo.ClassAttributes.FTableName := AEntityInfo.ClassAttributes.AttributeName;
    FTableName := AEntityInfo.ClassAttributes.TableName;
  end;
  Result := FTableName;
end;

function TdxMappingClassInfo.Inheritance(AMapInheritance: TdxMapInheritanceType): IdxMappingClassInfo;
begin
  Include(Attributes, TdxAttribute.Inheritance);
  MapInheritance := AMapInheritance;
  Result := Self;
end;

function TdxMappingClassInfo.Key(const AFieldNames: string): IdxMappingClassInfo;
begin
  KeyMemberNames.AddRange({$IFDEF DELPHIXE3}AFieldNames.Split([',', ';']){$ELSE}TdxStringHelper.Split(AFieldNames, [',', ';']){$ENDIF});
  Result := Self;
end;

function TdxMappingClassInfo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxMappingClassInfo.RegisterField(const AFieldName: string): IdxMappingMemberInfo;
var
  AMember: TRttiMember;
  AMemberInfo: TdxMappingMemberInfo;
  AMemberName: string;
begin
  AMemberName := EntityManager.GetQualifiedFieldName(AFieldName);
  if Owner.Members.TryGetValue(AMemberName, AMemberInfo) then
    Exit(AMemberInfo);
  AMember := EntityManager.GetMember(PersistentClass, AFieldName);
  if (AMember = nil) or not (AMember is TRttiField) then
    raise EdxEMFException.CreateFmt(sdxMetadataFieldPropertyNotFound, [AFieldName, PersistentClass.ClassName]);
  AMemberInfo := Owner.CreateMappingMemberInfo(AMember);
  if AMemberInfo = nil then
    raise EdxEMFException.Create('');
  AMemberInfo.Member := AMember;
  AMemberInfo.SetMemberName(AMemberName);
  Include(AMemberInfo.Attributes, TdxAttribute.Column);
  Owner.Add(AMemberInfo);
  Result := AMemberInfo;
end;

function TdxMappingClassInfo.RegisterProperty(const APropertyName: string): IdxMappingMemberInfo;
var
  AMember: TRttiMember;
  AMemberInfo: TdxMappingMemberInfo;
begin
  if Owner.Members.TryGetValue(APropertyName, AMemberInfo) then
    Exit(AMemberInfo);
  AMember := EntityManager.GetMember(PersistentClass, APropertyName);
  if (AMember = nil) or not (AMember is TRttiProperty) then
    raise EdxEMFException.CreateFmt(sdxMetadataFieldPropertyNotFound, [APropertyName, PersistentClass.ClassName]);
  AMemberInfo := Owner.CreateMappingMemberInfo(AMember);
  if AMemberInfo = nil then
    raise EdxEMFException.Create('');
  AMemberInfo.Member := AMember;
  AMemberInfo.SetMemberName(APropertyName);
  Include(AMemberInfo.Attributes, TdxAttribute.Column);
  Owner.Add(AMemberInfo);
  Result := AMemberInfo;
end;

procedure TdxMappingClassInfo.SetSchemaName(const ASchemaName: string);
begin
  FSchemaName := ASchemaName;
end;

function TdxMappingClassInfo.Table(const ATableName: string): IdxMappingClassInfo;
begin
  SetName(ATableName);
  Result := Self;
end;

function TdxMappingClassInfo.TableIndexes(const AColumns: string): IdxMappingClassInfo;
begin
  Indexes.Add({$IFDEF DELPHIXE3}AColumns.Split([',', ';']){$ELSE}TdxStringHelper.Split(AColumns, [',', ';']){$ENDIF});
  Result := Self;
end;

function TdxMappingClassInfo.TableSchemaName(const ASchemaName: string): IdxMappingClassInfo;
begin
  SetSchemaName(ASchemaName);
  Result := Self;
end;

function TdxMappingClassInfo._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := -1;
end;

function TdxMappingClassInfo._Release: Integer;
begin
  Dec(FRefCount);
  if (FRefCount = 0) and FIsCodeRegistry then
    Owner.EndRegisterEntity;
  Result := -1;
end;

{ TdxMemberInfoList }

function TdxMemberInfoList.IndexOfName(const AMemberName: string): Integer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if SameText({$IFDEF DELPHIXE3}List{$ELSE}Items{$ENDIF}[I].MemberName, AMemberName) then
      Exit(I);
  Result := -1;
end;

{ TdxKeyMemberInfo }

constructor TdxKeyMemberInfo.Create(AOwner: TdxEntityInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxKeyMemberInfo.CalcKeyFieldCount;
var
  I: Integer;
  ASubMemberCount: Integer;
begin
  FKeyFieldCount := 0;
  for I := 0 to Owner.KeyAttributes.Count - 1 do
  begin
    ASubMemberCount := Length(Owner.KeyAttributes[I].SubMembers);
    if ASubMemberCount = 0 then
    begin
      if Owner.KeyAttributes[I].IsReference then
        ASubMemberCount := Owner.KeyAttributes[I].ReferenceType.KeyProperty.KeyFieldCount
      else
        ASubMemberCount := 1;
    end;
    Inc(FKeyFieldCount, ASubMemberCount);
  end;
end;

function TdxKeyMemberInfo.GetMembers: TdxMemberInfoList;
begin
  Result := FOwner.KeyAttributes;
end;

function TdxKeyMemberInfo.GetIsIdentity: Boolean;
begin
  Result := Member.IsIdentity;
end;

function TdxKeyMemberInfo.GetMember: TdxMappingMemberInfo;
begin
  Result := Members[0];
end;

function TdxKeyMemberInfo.GetValue(AObject: TObject): TValue;
begin
  Result := Member.GetValue(AObject);
end;

function TdxKeyMemberInfo.GetValues(AObject: TObject): TArray<TValue>;
var
  I, J, K, ASubMemberCount: Integer;
  AMember: TdxMappingMemberInfo;
  AKeyValues: TArray<TValue>;
begin
  if IsCompositeKey or Owner.KeyAttributes[0].IsReference then
  begin
    SetLength(Result, KeyFieldCount);
    K := 0;
    for I := 0 to Owner.KeyAttributes.Count - 1 do
    begin
      AMember := Owner.KeyAttributes{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
      ASubMemberCount := Length(AMember.SubMembers);
      if ASubMemberCount > 0 then
        for J := 0 to ASubMemberCount - 1 do
        begin
          Result[K] := AMember.SubMembers[J].GetValue(AObject);
          Inc(K);
        end
      else
        begin
          if AMember.IsReference then
          begin
            AKeyValues := AMember.ReferenceType.KeyProperty.GetValues(AMember.GetValue(AObject).AsObject);
            for J := 0 to Length(AKeyValues) - 1 do
            begin
              Result[K] := AKeyValues[J];
              Inc(K);
            end
          end
          else
          begin
            Result[K] := AMember.GetValue(AObject);
            Inc(K);
          end;
        end;
    end;
  end
  else
    Result := TArray<TValue>.Create(Member.GetValue(AObject));
end;

function TdxKeyMemberInfo.IsCompositeKey: Boolean;
begin
  Result := KeyFieldCount > 1;
end;

procedure TdxKeyMemberInfo.SetValue(AObject: TObject; const AValue: TValue);
begin
  if AValue.IsArray then
    SetValues(AObject, AValue.AsType<TArray<TValue>>)
  else
    Member.SetValue(AObject, AValue);
end;

procedure TdxKeyMemberInfo.SetValues(AObject: TObject;
  const AKeyValues: TArray<TValue>);
var
  I, J, K, ASubMemberCount: Integer;
  AMember: TdxMappingMemberInfo;
begin
  if IsCompositeKey then
  begin
    K := 0;
    for I := 0 to Owner.KeyAttributes.Count - 1 do
    begin
      AMember := Owner.KeyAttributes{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
      ASubMemberCount := Length(AMember.SubMembers);
      if ASubMemberCount > 0 then
        for J := 0 to ASubMemberCount - 1 do
        begin
          AMember.SubMembers[J].SetValue(AObject, AKeyValues[K]);
          Inc(K);
        end
      else
        begin
          AMember.SetValue(AObject, AKeyValues[K]);
          Inc(K);
        end;
    end;
  end
  else
    Member.SetValue(AObject, AKeyValues[0]);
end;

{ TdxDiscriminatorMember }

constructor TdxDiscriminatorMember.Create(AOwner: TdxEntityInfo);
begin
  inherited Create(AOwner);
  SetName(AOwner.ClassAttributes.DiscriminatorInfo.ColumnName);
  Include(Attributes, TdxAttribute.NonPersistent);
end;

function TdxDiscriminatorMember.GetAsInteger: Integer;
begin
  Result := FDiscriminatorValue.AsInteger;
end;

function TdxDiscriminatorMember.GetAsString: string;
begin
  Result := FDiscriminatorValue.AsString;
end;

function TdxDiscriminatorMember.GetPersistentClass: TClass;
var
  AEntityInfo: TdxEntityInfo;
  AClass: TClass;
begin
  if FPersistentClass = nil then
  begin
    AEntityInfo := Owner;
    repeat
      AClass := AEntityInfo.ClassAttributes.PersistentBaseClass;
      if AClass = nil then
        Break;
      AEntityInfo := EntityManager.GetEntityInfo(AClass);
    until AEntityInfo.ClassAttributes.IsDiscriminatorColumn;
    FPersistentClass := AEntityInfo.ClassAttributes.PersistentClass;//AClass;
  end;
  Result := FPersistentClass;
end;

function TdxDiscriminatorMember.GetValue(AObject: TObject): TValue;
begin
  Result := FDiscriminatorValue;
end;

procedure TdxDiscriminatorMember.SetDiscriminator(const Value: TdxDiscriminator);
begin
  FDiscriminator := Value;
  SetName(Value.ColumnName);
end;

{ TdxServiceFields }

procedure TdxServiceFields.Notify(const Value: TdxServiceMember; Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnAdded then
    if Value is TdxDiscriminatorMember then
      FDiscriminatorField := TdxDiscriminatorMember(Value)
end;

{ TdxServiceMember }

function TdxServiceMember.GetActualName: string;
begin
  Result := AttributeName;
end;

constructor TdxServiceMember.Create(AOwner: TdxEntityInfo);
begin
  inherited Create(AOwner);
  AttributeType := TdxAttributeType.Service;
end;

initialization
finalization
  FreeAndNil(FEntityManager);
end.

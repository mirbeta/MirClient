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

unit dxEMF.DataSet;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Classes, DB, Rtti,
{$IFNDEF DELPHIXE3}
  DBPlatform,
{$ENDIF}
  dxCore, dxGenerics,
  dxEMF.Core,
  dxEMF.Core.Loader,
  dxEMF.Data,
  dxEMF.Types,
  dxEMF.Metadata,
  dxEMF.DB.Criteria;

type

  { TdxEntityField }

  TdxEntityField = class(TObjectField)
  strict private
    FInternalDataSet: TDataSet;
    FObject: TObject;
    function GetAsObject: TObject;
    procedure SetAsObject(const Value: TObject);
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure Bind(Binding: Boolean); override;
    function GetAsVariant: Variant; override;
    function GetFields: TFields; override;
    function GetFieldValue(Index: Integer): Variant; override;
    function GetDataSize: Integer; override;
    procedure SetFieldValue(Index: Integer; const Value: Variant); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function &As<T: class>: T;
    property AsObject: TObject read GetAsObject write SetAsObject;
  end;

  { TdxCriteriaOperatorHolder }

  TdxCriteriaOperatorHolder = record
  private
    FCriteriaOperatorText: string;
    FCriteriaOperator: IdxCriteriaOperator;
    FOperandValue: TArray<IdxOperandValue>;
    function GetCriteriaOperatorText: string;
    function GetHasParams: Boolean;
  public
    class operator Implicit(const A: TdxCriteriaOperatorHolder): IdxCriteriaOperator;
    class operator Implicit(const A: TdxCriteriaOperatorHolder): string;
    class operator Implicit(const ACriteriaOperator: IdxCriteriaOperator): TdxCriteriaOperatorHolder;
    class operator Implicit(const ACriteriaOperatorText: string): TdxCriteriaOperatorHolder;
    class operator Equal(const A: TdxCriteriaOperatorHolder; const ACriteriaOperator: IdxCriteriaOperator): Boolean;
    class operator Equal(const A: TdxCriteriaOperatorHolder; const ACriteriaOperatorText: string): Boolean;

    property CriteriaOperatorText: string read GetCriteriaOperatorText;
    property CriteriaOperator: IdxCriteriaOperator read FCriteriaOperator;
    property OperandValues: TArray<IdxOperandValue> read FOperandValue;
    property HasParams: Boolean read GetHasParams;
  end;

  { TdxEMFCustomDataSet }

  TdxEMFCustomDataSet = class(TDataSet)
  strict private type
{$REGION 'internal types'}

    TDataLinkList = class
    strict private
      FDataSet: TdxEMFCustomDataSet;
      FCollection: IdxEMFCollection;
      FInternalCollection: TdxEMFCustomCollection;
      FIsExternalData: Boolean;
      function GetCount: Integer;
      function GetItems(AIndex: Integer): TObject;
      function GetSession: TdxEMFCustomSession;
      procedure SetCollection(const Value: IdxEMFCollection);
      function GetHasLoader: Boolean;
      function GetIsAssigned: Boolean;
    protected
      procedure Close;
      procedure CreateCollection;
      procedure FetchAll;
      procedure Reset;
      function MoveNext: Boolean;
      property HasLoader: Boolean read GetHasLoader;
      property IsAssigned: Boolean read GetIsAssigned;
      property IsExternalData: Boolean read FIsExternalData;
    public
      constructor Create(ADataSet: TdxEMFCustomDataSet);
      procedure Add(AObject: TObject);
      procedure Assign(AObject: TObject); overload;
      procedure Assign(const AEMFCollection: IdxEMFCollection); overload;
      procedure Delete(AObject: TObject);
      procedure Insert(AIndex: Integer; AObject: TObject);
      function IndexOf(AObject: TObject): Integer;

      property Count: Integer read GetCount;
      property DataSet: TdxEMFCustomDataSet read FDataSet;
      property Session: TdxEMFCustomSession read GetSession;
      property Collection: IdxEMFCollection read FCollection write SetCollection;
      property Items[AIndex: Integer]: TObject read GetItems; default;
    end;

    PRecInfo = ^TRecInfo;
    TRecInfo = packed record
    private
      function GetActualObject: TObject;
    public
      TheObject: TObject;
      BookmarkFlag: TBookmarkFlag;
      property ActualObject: TObject read GetActualObject;

      procedure GetBookmark(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); inline;
      procedure SetBookmark(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); inline;
      class function BookmarkToObject(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}): TObject; static; inline;
      class procedure ObjectToBookmark(AValue: TObject; ABookmark: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); static; inline;
    end;

  protected type
    TFieldInfo = record
    private
      FField: TField;
      FMemberInfo: TdxMappingMemberInfo;
      FOffset: Integer;
    public
      constructor Create(AField: TField; AMemberInfo: TdxMappingMemberInfo; AOffset: Integer);
      procedure CopyFromBuffer(ARecordBuffer: TRecordBuffer; ABuffer: TValueBuffer);
      procedure CopyToBuffer(ARecordBuffer: TRecordBuffer; ABuffer: TValueBuffer);
      function HasBufferData(ARecordBuffer: TRecordBuffer): Boolean;
      function GetFieldBuffer(ARecordBuffer: TRecordBuffer): TRecordBuffer; inline;
      function GetData(ARecordBuffer: TRecordBuffer): TBytes;
      function GetByEdited(ARecordBuffer: TRecordBuffer): Boolean;
      function IsEmpty(ARecordBuffer: TRecordBuffer): Boolean;
      procedure SetByEdited(ARecordBuffer: TRecordBuffer; const AValue: Boolean);
      property Field: TField read FField;
      property MemberInfo: TdxMappingMemberInfo read FMemberInfo;
      property Offset: Integer read FOffset;
    end;

    TFieldInfos = TDictionary<TField, TFieldInfo>;
{$ENDREGION}
  strict private
    FArrayFieldInfos: TArray<TFieldInfo>;
    FBlobStreams: TObjectDictionary<TFieldInfo, TMemoryStream>;
    FCurrentRecord: Integer;
    FData: TDataLinkList;
    FFieldInfos: TFieldInfos;
    FFieldsIndex: Boolean;
    FFilterBuffer: TRecordBuffer;
    FFilterCurrentRecord: Integer;
    FFilterList: TdxIntegerList;
    FInternalIndexName: string;
    FIsCursorOpen: Boolean;
    FIsFiltered: Boolean;
    FPackageName: string;
    FReadOnly: Boolean;
    FRecBufSize: Integer;
    FRecInfoOffset: Integer;
    FRecordSize: Integer;
    FEMFDataSource: TdxCustomEMFDataSource;
  {$IFDEF DELPHIXE3}
    FBookmarkBuffer: TBookmark;
  {$ENDIF}
    class function GetCharSize(AFieldType: TFieldType): Integer; static; inline;
    class function GetDataSize(AValue: Variant; AField: TField): Integer; overload; static;
    class function GetDataSize(AField: TField): Integer; overload; static; inline;
    procedure CalculateFilterList;
    function CreateObject: TObject;
    function GetBufferRecInfo(ABuffer: TRecordBuffer): PRecInfo; inline;
    function GetFieldType(AMemberInfo: TdxMappingMemberInfo): TFieldType;
    function GetInternalRecordCount: Integer;
    function GetIndexFieldNames: string;
    function GetIndexName: string;
    function GetIsDesigning: Boolean;
    function FindDefaultComponent<T: TComponent>(AOwner: TComponent): T;
    procedure SetDataContext(Value: TdxEMFCustomDataContext);
    procedure SetDataSetFieldEntity;
    procedure SetEntityName(const Value: string);
    procedure SetIndex(const AValue: string);
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetPackageName(const Value: string);
    procedure SetEntityClass(const Value: TClass);
    procedure SetSession(const Value: TdxEMFCustomSession);
    procedure PropertyChanged;
    procedure UpdateFilterRecord;
    function GetDataContext: TdxEMFCustomDataContext; inline;
    function GetEntityName: string; inline;
    function GetPackageName: string; inline;
    function GetSession: TdxEMFCustomSession; inline;
    function GetEntityClass: TClass; inline;
    function GetEntityInfo: TdxEntityInfo; inline;
    procedure SetEntityInfo(const Value: TdxEntityInfo);
  protected
    function CreateEMFDataSource: TdxCustomEMFDataSource; virtual;
    procedure CheckCanModify;
    procedure CheckEntityInfo;
    procedure CheckSession;
    procedure CheckState;
    procedure DoAssignEntity; virtual;
    procedure SwitchToIndex; virtual;
    procedure UpdateCurrentRecord;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Changed; virtual;

    function ActiveBuffer: TRecordBuffer; inline;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure ClearCalcFields(Buffer: PByte); override;
    procedure DataEvent(AEvent: TDataEvent; AInfo: {$IFDEF DELPHIXE2}NativeInt{$ELSE}Longint{$ENDIF}); override;
    procedure DoAfterOpen; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure FreeFieldBuffers; override;
    function GetActiveRecordBuffer(var ARecordBuffer: TRecordBuffer): Boolean;
    function GetFieldClass(AFieldType: TFieldType): TFieldClass; override;
    function GetFieldValue(AActiveBuffer: TRecordBuffer; ABuffer: TValueBuffer; AField: TField): Boolean;
    function GetRecNo: Integer; override;
    function GetRecord(ABuffer: TRecordBuffer; AGetMode: TGetMode; ADoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    function GetIsExternalData: Boolean; virtual;
    procedure InternalInitRecord(ABuffer: TRecordBuffer); override;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure SetFieldData(AField: TField; ABuffer: TValueBuffer); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetObjectMemberValue(AObject: TObject; AActiveBuffer: TRecordBuffer; const AFieldInfo: TFieldInfo);
    procedure SetRecNo(AValue: Integer); override;

    procedure InternalAddRecord(Buffer: {$IFDEF DELPHIXE3}TRecordBuffer{$ELSE}Pointer{$ENDIF}; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalHandleException; override;
    procedure InternalFirst; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitFieldDefsFromEntity; virtual;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalLast; override;
    procedure InternalGotoBookmark(ABookmark: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); override;
    procedure InternalSetToRecord(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}); override;
    function IsCursorOpen: Boolean; override;
    function GetCanModify: Boolean; override;
    function GetRecordCount: Integer; override;
    procedure GetBookmarkData(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};
      AData: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); override;
    function GetBookmarkFlag(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}): TBookmarkFlag; override;
    procedure SetBookmarkFlag(ABuffer: TRecordBuffer; AValue: TBookmarkFlag); override;
    procedure SetBookmarkData(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};
      AData: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}); override;

    function GetDataSetFieldCollection(ADataSetField: TDataSetField): IdxEMFCollection;
    function GetBlobData(ABuffer: TRecordBuffer; AField: TField; AStream: TStream): Boolean;
    procedure SetBlobData(ABuffer: TRecordBuffer; AField: TField; AStream: TStream);
    procedure OpenCursor(AInfoQuery: Boolean = False); override;
    procedure UpdateDataSetField;

    function LocateRecord(const AKeyFields: string; const AKeyValues: Variant;
      AOptions: TLocateOptions; ASyncCursor: Boolean): Boolean;
{$IFNDEF DELPHIXE6}
    function DefaultFields: Boolean;
{$ENDIF}
    property IsExternalData: Boolean read GetIsExternalData;
    property IsDesigning: Boolean read GetIsDesigning;

    property CurrentRecord: Integer read FCurrentRecord;
    property Data: TDataLinkList read FData;
    property EntityInfo: TdxEntityInfo read GetEntityInfo write SetEntityInfo;
    property FieldInfos: TFieldInfos read FFieldInfos;
    property InternalIndexName: string read FInternalIndexName;
    property InternalRecordCount: Integer read GetInternalRecordCount;
    property EntityClass: TClass read GetEntityClass write SetEntityClass;

    property IndexName: string read GetIndexName write SetIndexName;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;

    property EMFDataSource: TdxCustomEMFDataSource read FEMFDataSource;
    property DataContext: TdxEMFCustomDataContext read GetDataContext write SetDataContext;
    property EntityName: string read GetEntityName write SetEntityName;
    property PackageName: string read GetPackageName write SetPackageName;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AppendObject(AObject: TObject);
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(ABookmark1, ABookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure FlushChanges;
    function GetFieldData(AField: TField; {$IFDEF DELPHIXE4}var{$ENDIF} Buffer: TValueBuffer): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function CurrentObject: TObject;
    function Current<T: class>: T;

    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Session: TdxEMFCustomSession read GetSession write SetSession;
  end;

  { TdxEMFCustomQueryDataSet }

  TdxEMFCustomQueryDataSet = class(TdxEMFCustomDataSet)
  private type

    TEMFDataLink = class(TDetailDataLink)
    strict private
      FDataSet: TdxEMFCustomQueryDataSet;
    protected
      procedure ActiveChanged; override;
      procedure CheckBrowseMode; override;
      function GetDetailDataSet: TDataSet; override;
      procedure RecordChanged(AField: TField); override;
    public
      constructor Create(ADataSet: TdxEMFCustomQueryDataSet);
    end;

  strict private
    FQueryData: TdxQueryData;
    FDataLink: TEMFDataLink;
    function GetSortByExpressionDefinitions: TdxSortByExpressionDefinitions;
    function GetCriteriaText: string;
    function GetGroupCriteriaText: string;
    function GetSortByExpressions: IdxSortByExpressions;
    function GetGroupCriteria: IdxCriteriaOperator;
    function GetCriteria: IdxCriteriaOperator;
    procedure SetCriteria(const Value: IdxCriteriaOperator);
    procedure SetCriteriaText(const Value: string);
    procedure SetFieldExpressions(const Value: TdxFieldExpressions);
    procedure SetGroupCriteria(const Value: IdxCriteriaOperator);
    procedure SetGroupCriteriaText(const Value: string);
    procedure SetParams(const Value: TParams);
    procedure SetSortByExpressionDefinitions(const Value: TdxSortByExpressionDefinitions);
    procedure SetSortByExpressions(const Value: IdxSortByExpressions);
    procedure SetSkipSelectedRecords(const Value: Integer);
    procedure SetTopSelectedRecords(const Value: Integer);

    procedure FetchQueryData;
    procedure InternalLoad(AInfoQuery: Boolean = False);
    procedure PrepareLoad(out AProperties, AGroupProperties: IdxCriteriaOperatorCollection; out ASorting: IdxSortByExpressions);
    procedure EndLoad(const AProperties, AGroupProperties: IdxCriteriaOperatorCollection; ASorting: IdxSortByExpressions;
      AInfoQuery: Boolean = False);
    procedure SetParamsFromCursor;
    procedure DoUpdateExpressions;

    function IsSortByExpressionDefinitionsStored: Boolean;
    function IsFieldExpressionsStored: Boolean;
    function IsGroupByExpressionDefinitionsStored: Boolean;

    function GetGroupByExpressionDefinitions: TdxGroupByExpressionDefinitions;
    function GetIsProjection: Boolean;
    procedure SetGroupByExpressionDefinitions(const Value: TdxGroupByExpressionDefinitions);
    function GetFieldExpressions: TdxFieldExpressions;
    function GetParams: TParams;
    function GetSkipSelectedRecords: Integer;
    function GetTopSelectedRecords: Integer;
    function GetEMFDataSource: TdxCustomEMFQueryDataSource; inline;

  protected
    function CreateEMFDataSource: TdxCustomEMFDataSource; override;
    function GetDataSource: TDataSource; override;
    procedure DataEvent(AEvent: TDataEvent; AInfo: {$IFDEF DELPHIXE2}NativeInt{$ELSE}Longint{$ENDIF}); override;
    procedure InternalInitFieldDefsFromEntity; override;
    procedure InternalRefresh; override;
    procedure OpenCursor(AInfoQuery: Boolean = False); override;
    procedure RefreshParams;
    procedure SetDataSource(const Value: TDataSource);

    property EMFDataSource: TdxCustomEMFQueryDataSource read GetEMFDataSource;

    property Criteria: IdxCriteriaOperator read GetCriteria write SetCriteria;
    property CriteriaText: string read GetCriteriaText write SetCriteriaText;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property GroupCriteria: IdxCriteriaOperator read GetGroupCriteria write SetGroupCriteria;
    property GroupCriteriaText: string read GetGroupCriteriaText write SetGroupCriteriaText;
    property GroupByExpressionDefinitions: TdxGroupByExpressionDefinitions read GetGroupByExpressionDefinitions write SetGroupByExpressionDefinitions stored IsGroupByExpressionDefinitionsStored;
    property FieldExpressions: TdxFieldExpressions read GetFieldExpressions write SetFieldExpressions stored IsFieldExpressionsStored;
    property IsProjection: Boolean read GetIsProjection;
    property Params: TParams read GetParams write SetParams;
    property SortByExpressions: IdxSortByExpressions read GetSortByExpressions write SetSortByExpressions;
    property SortByExpressionDefinitions: TdxSortByExpressionDefinitions read GetSortByExpressionDefinitions write SetSortByExpressionDefinitions stored IsSortByExpressionDefinitionsStored;
    property SkipSelectedRecords: Integer read GetSkipSelectedRecords write SetSkipSelectedRecords default 0;
    property TopSelectedRecords: Integer read GetTopSelectedRecords write SetTopSelectedRecords default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxEMFDataSet }

  TdxEMFDataSet = class(TdxEMFCustomQueryDataSet)
  public
    procedure AssignData(AData: TObject); overload;
    procedure AssignData(const AData: IdxEMFCollection); overload;
    procedure AssignData<T: class>(const AData: IdxEMFCollection<T>); overload;
    procedure AssignData<T: class>(const AData: TArray<T>); overload;
    property Criteria;
    property GroupCriteria;
    property SortByExpressions;
    property EntityClass;
  published
    property EntityName;
    property DataContext;
    property PackageName;
    property Session;

    property DataSetField;
    property DataSource;
    property CriteriaText;
    property GroupByExpressionDefinitions;
    property GroupCriteriaText;
    property FieldExpressions;
    property SkipSelectedRecords;
    property TopSelectedRecords;
    property SortByExpressionDefinitions;
    property Params;
    property ObjectView default True;

    property Active;
    property ReadOnly;
    property AutoCalcFields;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TdxEMFTable }

  TdxEMFTable = class(TdxEMFCustomDataSet)
  strict private
    FDetailFields: string;
    FMasterDataLink: TMasterDataLink;
    function GetSortByExpressions: IdxSortByExpressions;
    function GetCriteria: IdxCriteriaOperator;
    function GetMasterFields: string;
    procedure DoAssignData; inline;
    procedure InternalTableRefresh;
    procedure Reopen;
    procedure SetMasterFields(const Value: string);
  protected
    function GetDataSource: TDataSource; override;
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetDataSource(const Value: TDataSource);
    procedure SwitchToIndex; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EntityClass;
  published
    property DataContext;
    property EntityName;
    property PackageName;
    property Session;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property IndexFieldNames;
    property IndexName;
    property DetailFields: string read FDetailFields write FDetailFields;
    property ReadOnly;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TdxEMFDataSetBlobStream }

  TdxEMFDataSetBlobStream = class(TMemoryStream)
  private
    FDataSet: TdxEMFCustomDataSet;
    FField: TBlobField;
    FBuffer: TRecordBuffer;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    procedure ReadData;
  public
    constructor Create(AField: TBlobField; AMode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

uses
{$IFDEF DELPHIXE4}
  AnsiStrings,
{$ENDIF}
  DBConsts, Variants, FmtBCD,
  Math, IOUtils,
  dxEMF.Core.Collections,
  dxEMF.Strs,
  dxEMF.Serializer,
  dxEMF.Linq,
  dxEMF.Utils,
  dxEMF.DB.Model,
  dxEMF.DB.SQLConnectionProvider,
  dxEMF.DB.Utils,
  dxEMF.Utils.Exceptions;

const
  ftStrings = [ftString, ftWideString, ftGuid];

type
  TdxEMFCustomCollectionAccess = class(TdxEMFCustomCollection);
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);
  TdxEMFCustomDataProviderAccess = class(TdxEMFCustomDataProvider);
  TdxSQLConnectionProviderAccess = class(TdxSQLConnectionProvider);
  TdxEMFObjectCollectionAccess = class(TdxEMFObjectCollection);
  TdxCustomEMFDataSourceAccess = class(TdxCustomEMFDataSource);
  TdxCustomEMFQueryPropertiesAccess = class(TdxCustomEMFQueryProperties);

  { TdxCustomEMFQueryDataSourceHelper }

  TdxCustomEMFQueryDataSourceHelper = class helper for TdxCustomEMFQueryDataSource
  private
    function GetQueryProperties: TdxEMFQueryProperties; inline;
  public
    property QueryProperties: TdxEMFQueryProperties read GetQueryProperties;
  end;

  { TdxEMFTableDataSource }

  TdxEMFTableDataSource = class(TdxCustomEMFDataSource)
  protected
    procedure SetActive(const Value: Boolean); override;
  public
    property Active;
    property DataContext;
    property EntityName;
    property PackageName;
    property Session;
  end;

  { TdxEMFEntityFieldDataSet }

  TdxEMFEntityFieldDataSet = class(TdxEMFCustomDataSet)
  strict private
    FCollection: IdxEMFCollection;
    FEntityField: TdxEntityField;
    FMemberInfo: TdxMappingMemberInfo;
    function GetParentDataSet: TdxEMFCustomDataSet; inline;
  protected
    procedure InitDataSet;
    function GetIsExternalData: Boolean; override;
    property ParentDataSet: TdxEMFCustomDataSet read GetParentDataSet;
    property MemberInfo: TdxMappingMemberInfo read FMemberInfo;
  public
    constructor Create(AOwner: TComponent; AEntityField: TdxEntityField); reintroduce;
    procedure SetObject(AObject: TObject);
  end;

function ValueBufferToRecordBuffer(ABuffer: TValueBuffer): TRecordBuffer; inline;
begin
{$IFDEF DELPHIXE3}
  Result := @ABuffer[0];
{$ELSE}
  Result := ABuffer;
{$ENDIF}
end;

function ToSortDirection(AValue: TdxSortOrder): TdxSortDirection; inline;
begin
  if AValue = TdxSortOrder.soDescending then
    Result := TdxSortDirection.Descending
  else
    Result := TdxSortDirection.Ascending;
end;

{ TdxCustomEMFQueryDataSourceHelper }

function TdxCustomEMFQueryDataSourceHelper.GetQueryProperties: TdxEMFQueryProperties;
begin
  Result := inherited QueryProperties;
end;

{ TdxEMFEntityFieldDataSet }

constructor TdxEMFEntityFieldDataSet.Create(AOwner: TComponent; AEntityField: TdxEntityField);
begin
  inherited Create(AOwner);
  FEntityField := AEntityField;
end;

function TdxEMFEntityFieldDataSet.GetIsExternalData: Boolean;
begin
  Result := True;
end;

function TdxEMFEntityFieldDataSet.GetParentDataSet: TdxEMFCustomDataSet;
begin
  Result := TdxEMFCustomDataSet(FEntityField.DataSet);
end;

procedure TdxEMFEntityFieldDataSet.InitDataSet;
var
  AMemberInfo: TdxMappingMemberInfo;
begin
  if csDesigning in ComponentState then
    Exit;
  if ParentDataSet.EntityInfo = nil then
    Exit;
  for AMemberInfo in ParentDataSet.EntityInfo.MemberAttributes do
    if SameText(AMemberInfo.ColumnName, FEntityField.FieldName) then
    begin
      FMemberInfo := AMemberInfo;
      Break;
    end;
  Assert((FMemberInfo <> nil) and FMemberInfo.IsReference);
  EntityClass := FMemberInfo.ReferenceType.ClassAttributes.PersistentClass;
end;

procedure TdxEMFEntityFieldDataSet.SetObject(AObject: TObject);
begin
  if EntityClass = nil then
    InitDataSet;
  if FCollection = nil then
  begin
    FCollection := TdxEMFCollections.Create(EntityClass);
    Data.Assign(FCollection);
    Session := ParentDataSet.Session;
    Open;
  end;
  TdxEMFObjectCollectionAccess(TdxEMFObjectCollection(FCollection)).Clear;
  FCollection.Add(AObject);
  Refresh;
end;

{ TdxEntityField }

constructor TdxEntityField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalDataSet := TdxEMFEntityFieldDataSet.Create(nil, Self);
  SetDataType(ftObject);
end;

destructor TdxEntityField.Destroy;
begin
  FreeAndNil(FInternalDataSet);
  inherited Destroy;
end;

class procedure TdxEntityField.CheckTypeSize(Value: Integer);
begin
  if (Value <> 0) and (Value <> SizeOf(TObject)) then
    DatabaseError(SInvalidFieldSize);
end;

function TdxEntityField.&As<T>: T;
begin
  Result := AsObject as T;
end;

procedure TdxEntityField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Binding then
    TdxEMFEntityFieldDataSet(FInternalDataSet).InitDataSet;
end;

function TdxEntityField.GetAsObject: TObject;
var
  ATempBuff: TValueBuffer;
begin
  {$IFDEF DELPHIXE3}
  SetLength(ATempBuff, SizeOf(TObject));
  if GetData(ATempBuff) then
    System.Move(ATempBuff[0], Result, SizeOf(TObject))
  {$ELSE}
  if GetData(@ATempBuff) then
    Result := ATempBuff
  {$ENDIF}
  else
    Result := nil;
end;

function TdxEntityField.GetAsVariant: Variant;
begin
  if IsNull or (TdxEMFEntityFieldDataSet(FInternalDataSet).MemberInfo = nil) then
    Result := Null
  else
    Result := TdxEMFEntityFieldDataSet(FInternalDataSet).MemberInfo.ReferenceType.KeyProperty.GetValue(AsObject).AsVariant;
end;

procedure TdxEntityField.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  AField: TField;
begin
  for I := 0 to Fields.Count - 1 do
  begin
    AField := Fields[I];
    if AField.Owner = Root then
      Proc(AField);
  end;
end;

function TdxEntityField.GetDataSize: Integer;
begin
  Result := SizeOf(TObject);
end;

function TdxEntityField.GetFields: TFields;
var
  AObject: TObject;
begin
  AObject := GetAsObject;
  if AObject <> FObject then
  begin
    TdxEMFEntityFieldDataSet(FInternalDataSet).SetObject(AObject);
    FObject := AObject;
  end;
  Result := FInternalDataSet.Fields;
end;

function TdxEntityField.GetFieldValue(Index: Integer): Variant;
begin
  Result := Fields[Index].Value;
end;

procedure TdxEntityField.SetAsObject(const Value: TObject);
begin
{$IFDEF DELPHIXE3}
  SetData(BytesOf(@Value, SizeOf(TObject)));
{$ELSE}
  SetData(@Value);
{$ENDIF}
end;

procedure TdxEntityField.SetFieldValue(Index: Integer; const Value: Variant);
begin
  Fields[Index].Value := Value;
end;

procedure TdxEntityField.SetVarValue(const Value: Variant);
var
  AId: TValue;
begin
  if VarIsNull(Value) then
    AsObject := nil
  else
  begin
    AId := TValue.FromVariant(Value);
    AsObject := TdxEMFCustomDataSet(DataSet).Session.Find(TdxEMFCustomDataSet(FInternalDataSet).EntityClass, AId);
  end;
end;

{ TdxCriteriaOperatorHolder }

class operator TdxCriteriaOperatorHolder.Implicit(const ACriteriaOperator: IdxCriteriaOperator): TdxCriteriaOperatorHolder;
begin
  Result.FCriteriaOperator := ACriteriaOperator;
  Result.FCriteriaOperatorText := '';
  Result.FOperandValue := nil;
end;

class operator TdxCriteriaOperatorHolder.Equal(const A: TdxCriteriaOperatorHolder;
  const ACriteriaOperator: IdxCriteriaOperator): Boolean;
begin
  Result := A.FCriteriaOperator = ACriteriaOperator;
end;

class operator TdxCriteriaOperatorHolder.Equal(const A: TdxCriteriaOperatorHolder;
  const ACriteriaOperatorText: string): Boolean;
begin
  Result := A.FCriteriaOperatorText = ACriteriaOperatorText;
end;

function TdxCriteriaOperatorHolder.GetCriteriaOperatorText: string;
begin
  if (FCriteriaOperatorText <> '') then
    Result := FCriteriaOperatorText
  else
    Result := TdxCriteriaOperator.ToString(FCriteriaOperator);
end;

function TdxCriteriaOperatorHolder.GetHasParams: Boolean;
begin
  Result := Length(FOperandValue) > 0;
end;

class operator TdxCriteriaOperatorHolder.Implicit(const A: TdxCriteriaOperatorHolder): IdxCriteriaOperator;
begin
  Result := A.FCriteriaOperator;
end;

class operator TdxCriteriaOperatorHolder.Implicit(const ACriteriaOperatorText: string): TdxCriteriaOperatorHolder;
begin
  Result.FCriteriaOperatorText := ACriteriaOperatorText;
  Result.FCriteriaOperator := TdxCriteriaOperator.Parse(ACriteriaOperatorText, Result.FOperandValue);
end;

class operator TdxCriteriaOperatorHolder.Implicit(const A: TdxCriteriaOperatorHolder): string;
begin
  Result := A.CriteriaOperatorText;
end;

{ TdxEMFCustomDataSet.TRecInfo }

class function TdxEMFCustomDataSet.TRecInfo.BookmarkToObject(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF}): TObject;
begin
{$IFDEF DELPHIXE3}
  Move(AValue[0], Result, SizeOf(TObject));
{$ELSE}
  Move(AValue^, Result, SizeOf(TObject));
{$ENDIF}
end;

class procedure TdxEMFCustomDataSet.TRecInfo.ObjectToBookmark(AValue: TObject; ABookmark: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
begin
{$IFDEF DELPHIXE3}
  Move(AValue, ABookmark[0], SizeOf(TObject));
{$ELSE}
  Move(AValue, ABookmark^, SizeOf(TObject));
{$ENDIF}
end;

function TdxEMFCustomDataSet.TRecInfo.GetActualObject: TObject;
begin
  if BookmarkFlag = TBookmarkFlag.bfInserted then
    Result := nil
  else
    Result := TheObject;
end;

procedure TdxEMFCustomDataSet.TRecInfo.GetBookmark(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
begin
  ObjectToBookmark(TheObject, AValue);
end;

procedure TdxEMFCustomDataSet.TRecInfo.SetBookmark(AValue: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
begin
  TheObject := BookmarkToObject(AValue);
end;

{ TdxEMFCustomDataSet.TFieldInfo }

constructor TdxEMFCustomDataSet.TFieldInfo.Create(AField: TField; AMemberInfo: TdxMappingMemberInfo; AOffset: Integer);
begin
  FField := AField;
  FMemberInfo := AMemberInfo;
  FOffset := AOffset;
end;

function TdxEMFCustomDataSet.TFieldInfo.GetByEdited(ARecordBuffer: TRecordBuffer): Boolean;
begin
  Result := (ARecordBuffer[FOffset] and 2) <> 0;
end;

function TdxEMFCustomDataSet.TFieldInfo.GetFieldBuffer(ARecordBuffer: TRecordBuffer): TRecordBuffer;
begin
  Result := @ARecordBuffer[Offset + 1];
end;

function TdxEMFCustomDataSet.TFieldInfo.GetData(ARecordBuffer: TRecordBuffer): TBytes;
begin
  SetLength(Result, FField.DataSize);
  Move(GetFieldBuffer(ARecordBuffer)^, Result[0], FField.DataSize);
end;

procedure TdxEMFCustomDataSet.TFieldInfo.CopyFromBuffer(ARecordBuffer: TRecordBuffer; ABuffer: TValueBuffer);
begin
  if ABuffer = nil then
  begin
    ARecordBuffer[FOffset] := 0;
    Exit;
  end;
{$IFDEF DELPHIXE3}
  Move(ABuffer[0], ARecordBuffer[Offset + 1], FField.DataSize);
{$ELSE}
  Move(ABuffer^, ARecordBuffer[Offset + 1], FField.DataSize);
{$ENDIF}
  ARecordBuffer[FOffset] := ARecordBuffer[FOffset] or 1;
end;

procedure TdxEMFCustomDataSet.TFieldInfo.CopyToBuffer(ARecordBuffer: TRecordBuffer; ABuffer: TValueBuffer);
begin
{$IFDEF DELPHIXE3}
  Move(ARecordBuffer[Offset + 1], ABuffer[0], FField.DataSize);
{$ELSE}
  Move(ARecordBuffer[Offset + 1], ABuffer^, FField.DataSize);
{$ENDIF}
end;

function TdxEMFCustomDataSet.TFieldInfo.HasBufferData(ARecordBuffer: TRecordBuffer): Boolean;
begin
  Result := ARecordBuffer[FOffset] <> 0;
end;

procedure TdxEMFCustomDataSet.TFieldInfo.SetByEdited(ARecordBuffer: TRecordBuffer; const AValue: Boolean);
var
  B: Byte;
begin
  B := ARecordBuffer[FOffset];
  if AValue then
    B := B or 2
  else
    B := B and not 2;
  ARecordBuffer[FOffset] := B;
end;

function TdxEMFCustomDataSet.TFieldInfo.IsEmpty(ARecordBuffer: TRecordBuffer): Boolean;
begin
  Result := (ARecordBuffer[FOffset] and 1) = 0;
end;

{ TdxEMFDataSet }

constructor TdxEMFCustomDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEMFDataSource := CreateEMFDataSource;
  FData := TdxEMFCustomDataSet.TDataLinkList.Create(Self);
  FBlobStreams := TObjectDictionary<TFieldInfo, TMemoryStream>.Create([doOwnsValues]);
  FFieldInfos := TFieldInfos.Create;
  FFilterList := TdxIntegerList.Create;
  if IsDesigning then
  begin
    Session := FindDefaultComponent<TdxEMFCustomSession>(AOwner);
    DataContext := FindDefaultComponent<TdxEMFCustomDataContext>(AOwner);
  end;
end;


destructor TdxEMFCustomDataSet.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFilterList);
  FreeAndNil(FData);
  FreeAndNil(FFieldInfos);
  FreeAndNil(FBlobStreams);
end;

function TdxEMFCustomDataSet.GetDataContext: TdxEMFCustomDataContext;
begin
  Result := FEMFDataSource.DataContext;
end;

function TdxEMFCustomDataSet.GetEntityClass: TClass;
begin
  Result := EMFDataSource.EntityClass;
end;

function TdxEMFCustomDataSet.GetEntityInfo: TdxEntityInfo;
begin
  Result := EMFDataSource.EntityInfo;
end;

function TdxEMFCustomDataSet.GetEntityName: string;
begin
  Result := EMFDataSource.EntityName;
end;

function TdxEMFCustomDataSet.GetRecordCount: Integer;
begin
  CheckActive;
  Result := InternalRecordCount;
end;

function TdxEMFCustomDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TdxEMFCustomDataSet.GetSession: TdxEMFCustomSession;
begin
  if EMFDataSource = nil then
    Result := nil
  else
    Result := EMFDataSource.Session;
end;

function TdxEMFCustomDataSet.CompareBookmarks(ABookmark1, ABookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
var
  O1, O2: TObject;
  R1, R2: Integer;
begin
  Result := RetCodes[ABookmark1 = nil, ABookmark2 = nil];
  if Result = 2 then
  begin
    O1 := TRecInfo.BookmarkToObject(ABookmark1);
    O2 := TRecInfo.BookmarkToObject(ABookmark2);
    if O1 = O2 then
      Result := 0
    else
    begin
      R1 := FData.IndexOf(O1);
      R2 := FData.IndexOf(O2);
      if R1 > R2 then
        Result := 1
      else
        Result := -1;
    end;
  end;
end;

function TdxEMFCustomDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TdxEMFDataSetBlobStream.Create(Field as TBlobField, Mode);
end;

function TdxEMFCustomDataSet.CreateEMFDataSource: TdxCustomEMFDataSource;
begin
  Result := TdxEMFTableDataSource.Create(Self);
end;

function TdxEMFCustomDataSet.GetBufferRecInfo(ABuffer: TRecordBuffer): PRecInfo;
begin
  Result := PRecInfo(ABuffer + FRecInfoOffset);
end;

function TdxEMFCustomDataSet.GetIndexFieldNames: string;
begin
  if FFieldsIndex then
    Result := FInternalIndexName
  else
    Result := '';
end;

function TdxEMFCustomDataSet.GetIndexName: string;
begin
  if FFieldsIndex then
    Result := ''
  else
    Result := FInternalIndexName;
end;

function TdxEMFCustomDataSet.GetInternalRecordCount: Integer;
begin
  if not FIsFiltered then
    Result := FData.Count
  else
    Result := FFilterList.Count;
end;

function TdxEMFCustomDataSet.GetIsDesigning: Boolean;
begin
  Result := ([csDesigning, csLoading] * ComponentState = [csDesigning]) and
    ((Owner = nil) or ([csDesigning, csLoading] * Owner.ComponentState = [csDesigning]));
end;

function TdxEMFCustomDataSet.GetIsExternalData: Boolean;
begin
  Result := Data.IsExternalData or (DataSetField <> nil);
end;

function TdxEMFCustomDataSet.GetPackageName: string;
begin
  Result := EMFDataSource.PackageName
end;

procedure TdxEMFCustomDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Session then
      Session := nil
    else if AComponent = DataContext then
      DataContext := nil;
  end;
end;

procedure TdxEMFCustomDataSet.OpenCursor(AInfoQuery: Boolean);
var
  AParentDataSet: TdxEMFCustomDataSet;
begin
  if DataSetField <> nil then
  begin
    if not AInfoQuery then
    begin
      AParentDataSet := DataSetField.DataSet as TdxEMFCustomDataSet;
      OpenParentDataSet(AParentDataSet);
      Data.Assign(GetDataSetFieldCollection(DataSetField));
    end;
    SetDataSetFieldEntity;
  end;
  inherited OpenCursor(AInfoQuery);
end;

procedure TdxEMFCustomDataSet.PropertyChanged;
begin
  if not (csReading in ComponentState) then
    DataEvent(dePropertyChange, 0);
end;

procedure TdxEMFCustomDataSet.Changed;
begin
  CheckState;
end;

function TdxEMFCustomDataSet.ActiveBuffer: TRecordBuffer;
begin
  Result := TRecordBuffer(inherited ActiveBuffer);
end;

procedure TdxEMFCustomDataSet.CheckState;
begin
  if Session = nil then
    Close;
end;

procedure TdxEMFCustomDataSet.ClearCalcFields(Buffer: PByte);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TdxEMFCustomDataSet.CheckCanModify;
begin
  if not CanModify then
    DatabaseError(SDataSetReadOnly, Self);
end;

procedure TdxEMFCustomDataSet.CheckEntityInfo;
begin
  if EntityInfo = nil then
    if EntityName = '' then
      raise EdxNoEntityInfoException.CreateFmt(sdxEntityNameNotSpecified, [Name])
    else
      raise EdxNoEntityInfoException.CreateFmt(sdxEntityCannotBeFound, [EntityName, Name]);
end;

procedure TdxEMFCustomDataSet.CheckSession;
begin
  if Session = nil then
    DatabaseError(sdxMissingSession, Self);
end;

function TdxEMFCustomDataSet.CreateObject: TObject;
begin
  Assert(Session <> nil);
  Result := Session.CreateObject(EntityClass);
end;

function TdxEMFCustomDataSet.CurrentObject: TObject;
var
  ARecBuf: TRecordBuffer;
begin
  if not GetActiveRecordBuffer(ARecBuf) then
    Exit(nil);
  Result := GetBufferRecInfo(ARecBuf).ActualObject;
end;

function TdxEMFCustomDataSet.Current<T>: T;
begin
  Result := T(CurrentObject);
end;

function TdxEMFCustomDataSet.BookmarkValid(ABookmark: TBookmark): Boolean;
var
  AIndex: Integer;
begin
  Result := ABookmark <> nil;
  if Result then
  begin
    AIndex := FData.IndexOf(TRecInfo.BookmarkToObject(ABookmark));
    Result := AIndex > -1;
    if FIsFiltered then
      Result := FFilterList.IndexOf(AIndex) > -1;
  end;
end;

function TdxEMFCustomDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
end;

procedure TdxEMFCustomDataSet.AppendObject(AObject: TObject);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  if AObject.ClassType <> EntityClass then
    raise Exception.CreateFmt('Class mismatch (found %s; expected %s)', [AObject.ClassType.ClassName, EntityClass.ClassName]);
  FData.Insert(-1, AObject);
  FCurrentRecord := FData.Count - 1;
  Session.Save(AObject);
  Resync([]);
end;

procedure TdxEMFCustomDataSet.DoAfterOpen;
begin
  CalculateFilterList;
  inherited DoAfterOpen;
end;

procedure TdxEMFCustomDataSet.DoAssignEntity;
begin
  TdxCustomEMFDataSourceAccess(EMFDataSource).DoAssignEntity;
end;

procedure TdxEMFCustomDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TdxEMFCustomDataSet.FreeFieldBuffers;
begin
  FBlobStreams.Clear;
  inherited FreeFieldBuffers;
end;

procedure TdxEMFCustomDataSet.GetBookmarkData(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};
  AData: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
begin
  GetBufferRecInfo(Pointer(ABuffer))^.GetBookmark(AData);
end;

function TdxEMFCustomDataSet.GetBookmarkFlag(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF}): TBookmarkFlag;
begin
  Result := GetBufferRecInfo(Pointer(ABuffer))^.BookmarkFlag;
end;

function TdxEMFCustomDataSet.GetActiveRecordBuffer(var ARecordBuffer: TRecordBuffer): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        ARecordBuffer := nil
      else
        ARecordBuffer := ActiveBuffer;
    dsEdit, dsInsert:
      ARecordBuffer := ActiveBuffer;
    dsCalcFields:
      ARecordBuffer := TRecordBuffer(CalcBuffer);
    dsFilter:
      ARecordBuffer := FFilterBuffer;
  else
    ARecordBuffer := nil;
  end;
  Result := ARecordBuffer <> nil;
end;

function TdxEMFCustomDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

function TdxEMFCustomDataSet.GetFieldValue(AActiveBuffer: TRecordBuffer; ABuffer: TValueBuffer; AField: TField): Boolean;
var
  AFieldInfo: TFieldInfo;
  AObject, ARefObject: TObject;
  AData: TValue;
  AInt: Integer;
  AInt64: Int64;
  ASmallInt: Smallint;
  AByte: Byte;
  AWord: Word;
  ABool: Boolean;
  ADouble: Double;
  ASingle: Single;
  ADateTime: TDateTime;
  AStr: string;
  AGUID: TGUID;
  ABCD: TBCD;
  {$IFDEF DELPHIXE3}
  ATempBuffer: TValueBuffer;
  {$ENDIF}
begin
  if not FFieldInfos.TryGetValue(AField, AFieldInfo) then
    Exit(False);
  if AFieldInfo.HasBufferData(AActiveBuffer) then
  begin
    if ABuffer <> nil then
      AFieldInfo.CopyToBuffer(AActiveBuffer, ABuffer);
    Exit(not AFieldInfo.IsEmpty(AActiveBuffer));
  end;
  if AFieldInfo.MemberInfo = nil then
    Exit(False);

  AObject := GetBufferRecInfo(AActiveBuffer).ActualObject;
  if ((AObject = nil) or (State = dsInsert)) and (ABuffer <> nil) then
  begin
    cxZeroMemory(PByte(ABuffer), AField.DataSize);
    if AObject = nil then
      Exit(False);
  end;
  if (AObject = nil) then
    Exit(ABuffer <> nil);
  AData := AFieldInfo.MemberInfo.GetValue(AObject);
  if AData.IsEmpty then
    Exit(False);
  if ABuffer = nil then
    Exit(not AData.IsEmpty);
  case AField.DataType of
    ftSmallint:
      begin
        ASmallInt := AData.AsType<Smallint>;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Smallint>(ASmallInt, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromSmallInt(ASmallInt, ABuffer);
        {$ELSE}
        Smallint(ABuffer^) := ASmallInt;
        {$IFEND}
      end;
    ftInteger, ftLongWord, ftAutoInc:
      begin
        if AData.IsEnumeration then
          AInt := AData.AsOrdinal
        else
          AInt := AData.AsInteger;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Integer>(AInt, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromInteger(AInt, ABuffer);
        {$ELSE}
        Integer(ABuffer^) := AInt;
        {$IFEND}
      end;
    ftWord:
      begin
        if AData.IsEnumeration then
          AWord := AData.AsOrdinal
        else
          AWord := AData.AsType<Word>;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Word>(AWord, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromWord(AWord, ABuffer);
        {$ELSE}
        Word(ABuffer^) := AWord;
        {$IFEND}
      end;
    ftBoolean:
      begin
        ABool := AData.AsBoolean;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Boolean>(ABool, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromWordBool(ABool, ABuffer);
        {$ELSE}
        WordBool(ABuffer^) := ABool;
        {$IFEND}
      end;
    ftFloat, ftCurrency:
      begin
        ADouble := AData.AsExtended;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Double>(ADouble, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromDouble(ADouble, ABuffer);
        {$ELSE}
        Double(ABuffer^) := ADouble;
        {$IFEND}
      end;
    ftDate, ftTime, ftDateTime:
      begin
        ADateTime := AData.AsType<TDateTime>;
        {$IF Defined(DELPHIXE8)}
        SetLength(ATempBuffer, SizeOf(TDateTime));
        TBitConverter.UnsafeFrom<TDateTime>(ADateTime, ATempBuffer);
        DataConvert(AField, ATempBuffer, ABuffer, True);
        {$ELSEIF Defined(DELPHIXE3)}
        SetLength(ATempBuffer, SizeOf(TDateTime));
        TBitConverter.FromDouble(ADateTime, ATempBuffer);
        DataConvert(AField, ATempBuffer, ABuffer, True);
        {$ELSE}
        DataConvert(AField, @ADateTime, ABuffer, True)
        {$IFEND}
      end;
    ftWideString, ftFixedWideChar:
      begin
        {$IFDEF DELPHIXE3}
        FillChar(ABuffer[0], AField.DataSize, 0);
        {$ELSE}
        FillChar(ABuffer^, AField.DataSize, 0);
        {$ENDIF}
        AStr := AData.AsString;
        StrLCopy(PChar(ABuffer), PChar(AStr), AField.Size);
      end;
    ftLargeint:
      begin
        AInt64 := AData.AsInt64;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Int64>(AInt64, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromLargeInt(AInt64, ABuffer);
        {$ELSE}
        Int64(ABuffer^) := AInt64;
        {$IFEND}
      end;
    ftGuid:
      begin
        AGUID := AData.AsType<TGUID>;
        AStr := AGUID.ToString;
      {$IFNDEF NEXTGEN}
        {$IFDEF DELPHIXE4}
        AnsiStrings.StrLCopy(PAnsiChar(ABuffer), PAnsiChar(AnsiString(AStr)), AField.Size);
        {$ELSE}
        StrLCopy(PAnsiChar(ABuffer), PAnsiChar(AnsiString(AStr)), AField.Size);
        {$ENDIF}
        {$IFDEF DELPHIXE3}
        ABuffer[AField.DataSize - 1] := 0;
        {$ELSE}
        PByte(ABuffer)[AField.DataSize - 1] := 0;
        {$ENDIF}
      {$ELSE}
        ATempBuffer := TEncoding.ANSI.GetBytes(AStr);
        Move(ATempBuffer[0], ABuffer[0], Length(ATempBuffer));
      {$ENDIF}
      end;
    ftFMTBcd:
      begin
        ABCD := AData.AsBCD;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<TBCD>(ABCD, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromBcd(ABCD, ABuffer);
        {$ELSE}
        TBCD(ABuffer^) := ABCD;
        {$IFEND}
      end;
    ftShortint, ftByte:
      begin
        if AData.IsEnumeration then
          AByte := AData.AsOrdinal
        else
          AByte := AData.AsType<Byte>;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Byte>(AByte, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromByte(AByte, ABuffer);
        {$ELSE}
        PByte(ABuffer)^ := AByte;
        {$IFEND}
      end;
    ftObject:
      begin
        ARefObject := AData.AsObject;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<TObject>(ARefObject, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
          {$IFDEF CPUX64}
          TBitConverter.FromLargeInt(Int64(ARefObject), ABuffer);
          {$ELSE}
          TBitConverter.FromInteger(Integer(ARefObject), ABuffer);
          {$ENDIF}
        {$ELSE}
        PObject(ABuffer)^ := ARefObject;
        {$IFEND}
      end;
    ftSingle:
      begin
        ASingle := AData.AsType<Single>;
        {$IF Defined(DELPHIXE8)}
        TBitConverter.UnsafeFrom<Single>(ASingle, ABuffer);
        {$ELSEIF Defined(DELPHIXE3)}
        TBitConverter.FromSingle(ASingle, ABuffer);
        {$ELSE}
        PSingle(ABuffer)^ := ASingle;
        {$IFEND}
      end;
    else
      Exit(False);
  end;
  AFieldInfo.CopyFromBuffer(AActiveBuffer, ABuffer);
  Result := True;
end;

function TdxEMFCustomDataSet.GetFieldClass(AFieldType: TFieldType): TFieldClass;
begin
  case AFieldType of
    ftObject: Result := TdxEntityField;
  else
    Result := inherited GetFieldClass(AFieldType);
  end;
end;

function TdxEMFCustomDataSet.GetFieldData(AField: TField; {$IFDEF DELPHIXE4}var{$ENDIF} Buffer: TValueBuffer): Boolean;
var
  ARecBuf: TRecordBuffer;
begin
  if not GetActiveRecordBuffer(ARecBuf) then
    Exit(False);

  if AField.FieldKind in [fkData, fkCalculated, fkLookup, fkInternalCalc] then
    Result := GetFieldValue(ARecBuf, Buffer, AField)
  else
    Result := False;
end;

function TdxEMFCustomDataSet.GetFieldType(AMemberInfo: TdxMappingMemberInfo): TFieldType;
var
  ASQLConnectionProvider: TdxSQLConnectionProvider;
begin
  if AMemberInfo.IsReference then
    Exit(TFieldType.ftObject);
  Result := AMemberInfo.FieldType;
  if (Session <> nil) and (TdxEMFCustomDataProviderAccess(TdxEMFCustomSessionAccess(Session).DataProvider) <> nil) then
    ASQLConnectionProvider := TdxEMFCustomDataProviderAccess(TdxEMFCustomSessionAccess(Session).DataProvider).SQLConnectionProvider
  else
    ASQLConnectionProvider := nil;
  if (Result = TFieldType.ftWideString) and (AMemberInfo.IsBlob or
    ((ASQLConnectionProvider <> nil) and (AMemberInfo.DBColumnSize > TdxSQLConnectionProviderAccess(ASQLConnectionProvider).MaximumStringSize))) then
    Result := TFieldType.ftWideMemo;
end;

function TdxEMFCustomDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TdxEMFCustomDataSet.FindDefaultComponent<T>(AOwner: TComponent): T;
var
  I: Integer;
begin
  Result := nil;
  while AOwner <> nil do
  begin
    for I := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[i] is T then
      begin
        Result := T(AOwner.Components[i]);
        Exit;
      end;
    AOwner := AOwner.Owner;
  end;
end;

procedure TdxEMFCustomDataSet.FlushChanges;
begin
  CheckBrowseMode;
  CheckSession;
  Session.FlushChanges;
  Refresh;
end;

function TdxEMFCustomDataSet.GetRecNo: Integer;
var
  ARecordBuffer: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if State = dsInternalCalc then
    Exit;
  if not GetActiveRecordBuffer(ARecordBuffer) then
    Exit;
  if GetBufferRecInfo(ARecordBuffer)^.ActualObject = nil then
    Exit;

  if State <> dsCalcFields then
    UpdateCursorPos;
  if (FCurrentRecord = -1) and (RecordCount > 0) then
    Result := 1
  else
    if not FIsFiltered then
      Result := FCurrentRecord + 1
    else
      Result := FFilterCurrentRecord + 1;
end;

function TdxEMFCustomDataSet.GetRecord(ABuffer: TRecordBuffer; AGetMode: TGetMode; ADoCheck: Boolean): TGetResult;

  function CalculateRecNo(var ARecNo: Integer): TGetResult;
  begin
    Result := grOK;
    case AGetMode of
      gmNext:
        if ARecNo >= InternalRecordCount - 1 then
          Result := grEOF
        else
          Inc(ARecNo);
      gmPrior:
        if ARecNo <= 0 then
          Result := grBOF
        else
          Dec(ARecNo);
      gmCurrent:
        if (ARecNo < 0) or (ARecNo >= InternalRecordCount) then
          Result := grError;
    end;
  end;

begin
  if FData.Collection = nil then
  begin
    Result := grError;
    Exit;
  end;
  if (AGetMode = TGetMode.gmNext) and not FData.MoveNext then
    Result := grEOF
  else
  begin
    if not FIsFiltered then
      Result := CalculateRecNo(FCurrentRecord)
    else
    begin
      Result := CalculateRecNo(FFilterCurrentRecord);
      if (Result = grOK) then
        FCurrentRecord := FFilterList[FFilterCurrentRecord]
      else
        FCurrentRecord := -1;
    end;
    if Result = grOK then
    begin
      FillChar(ABuffer[0], FRecBufSize, 0);
      FFilterBuffer := ABuffer;
      with GetBufferRecInfo(ABuffer)^ do
      begin
        BookmarkFlag := bfCurrent;
        if BookmarkAvailable then
          ObjectToBookmark(FData[FCurrentRecord], Bookmark);
        TheObject := FData[FCurrentRecord];
      end;
    {$IFDEF DELPHIXE4}
      GetCalcFields(TRecBuf(ABuffer));
    {$ELSE}
      GetCalcFields(ABuffer);
    {$ENDIF}
    end
    else
      if (Result = grError) and ADoCheck then
        DatabaseError(SDataSetEmpty, Self);
  end;
end;

procedure TdxEMFCustomDataSet.InternalAddRecord(Buffer: {$IFDEF DELPHIXE3}TRecordBuffer{$ELSE}Pointer{$ENDIF};
  Append: Boolean);
begin
  if Append then
    SetBookmarkFlag(Buffer, bfEOF);
  InternalPost;
end;

procedure TdxEMFCustomDataSet.InternalClose;
begin
  FCurrentRecord := -1;
  FFilterCurrentRecord := -1;

  BindFields(False);
{$IFNDEF DELPHIXE6}
  if DefaultFields then
{$ENDIF}
    DestroyFields;
  Data.Close;
  FIsCursorOpen := False;
end;

procedure TdxEMFCustomDataSet.InternalDelete;
var
  AObject: TObject;
  AIndex: Integer;
  I: Integer;
begin
  CheckCanModify;
  AObject := CurrentObject;
  AIndex := FData.IndexOf(AObject);
  if AIndex > -1 then
  begin
    FData.Delete(AObject);
    if FIsFiltered then
    begin
      FFilterList.Delete(FFilterCurrentRecord);
      if FFilterCurrentRecord < FFilterList.Count then
      begin
        for I := FFilterCurrentRecord to FFilterList.Count - 1 do
          FFilterList[I] := FFilterList[I] - 1;
      end
      else
        Dec(FFilterCurrentRecord);
    end;
    UpdateCurrentRecord;
  end;
end;

procedure TdxEMFCustomDataSet.InternalFirst;
begin
  FCurrentRecord := -1;
  FFilterCurrentRecord := -1;
end;

procedure TdxEMFCustomDataSet.InternalGotoBookmark(ABookmark: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
var
  AIndex: Integer;
begin
  AIndex := FData.IndexOf(TRecInfo.BookmarkToObject(ABookmark));
  if AIndex <> -1 then
  begin
    FFilterCurrentRecord := FFilterList.IndexOf(AIndex);
    FCurrentRecord := AIndex
  end;
end;

procedure TdxEMFCustomDataSet.InternalHandleException;
begin
  ApplicationHandleException(Self);
end;

procedure TdxEMFCustomDataSet.InternalInitFieldDefs;
begin
  InternalInitFieldDefsFromEntity;
end;

procedure TdxEMFCustomDataSet.InternalInitFieldDefsFromEntity;
var
  AFieldDef: TFieldDef;
  AFieldType: TFieldType;
  AMemberInfo: TdxMappingMemberInfo;
  ASize: Integer;
begin
  FieldDefs.Clear;
  if EntityInfo = nil then
    Exit;

  for AMemberInfo in EntityInfo.MemberAttributes do
  begin
    if AMemberInfo.IsAssociationList then
    begin
      AFieldDef := FieldDefs.AddFieldDef;
      AFieldDef.Name := AMemberInfo.MemberName;
      AFieldDef.DataType := ftDataSet;
      AFieldDef.Required := False;
      AFieldDef.Attributes := AFieldDef.Attributes + [TFieldAttribute.faReadonly];
      Continue;
    end
    else if not AMemberInfo.IsColumn then
      Continue;
    AFieldType := GetFieldType(AMemberInfo);
    if AFieldType = TFieldType.ftUnknown then
      Continue;
    AFieldDef := FieldDefs.AddFieldDef;
    AFieldDef.Name := AMemberInfo.ColumnName;
    AFieldDef.DataType := AFieldType;
    AFieldDef.InternalCalcField := AMemberInfo.IsVirtualColumn;
    if not(AFieldType in ftFixedSizeTypes) then
    begin
      case AFieldType of
        ftGuid:
          ASize := 38;
        ftFMTBcd:
          ASize := SizeOf(TBCD);
        ftObject:
          ASize := SizeOf(TObject);
      else
        begin
          ASize := AMemberInfo.DBColumnSize;
          if ASize = 0 then
            ASize := TdxSQLConnectionProvider.DefaultStringSize;
        end;
      end;
      AFieldDef.Size := ASize;
    end;
    AFieldDef.Required := AMemberInfo.IsRequired;
    if not AMemberInfo.IsWritable or AMemberInfo.IsReadOnly then
      AFieldDef.Attributes := AFieldDef.Attributes + [TFieldAttribute.faReadonly];
  end;
end;

procedure TdxEMFCustomDataSet.UpdateCurrentRecord;
begin
  if FIsFiltered then
  begin
    if (FFilterCurrentRecord >= 0) and (FFilterCurrentRecord < FFilterList.Count) then
      FCurrentRecord := FFilterList[FFilterCurrentRecord]
    else
      FCurrentRecord := -1;
  end
  else if FCurrentRecord >= FData.Count then
    FCurrentRecord := FData.Count - 1;
end;

procedure TdxEMFCustomDataSet.UpdateDataSetField;
begin
  Data.Assign(GetDataSetFieldCollection(DataSetField));
  UpdateCurrentRecord;
  Resync([]);
end;

procedure TdxEMFCustomDataSet.UpdateFilterRecord;
var
  Accepted: Boolean;
  I: Integer;
begin
  if not(Filtered and Assigned(OnFilterRecord)) then
    Exit;
  Accepted := True;
  OnFilterRecord(Self, Accepted);
  if (State = dsInsert) and (FFilterCurrentRecord < FFilterList.Count) then
    for I := FFilterCurrentRecord to FFilterList.Count - 1 do
      FFilterList[I] := FFilterList[I] + 1;
  if Accepted then
  begin
    if EOF then
      FFilterList.Add(FCurrentRecord)
    else
    begin
      if State = dsInsert then
        FFilterList.Insert(FFilterCurrentRecord, FCurrentRecord)
      else
      begin
        if FFilterList.IndexOf(FCurrentRecord) = -1 then
          FFilterList.Insert(FFilterCurrentRecord, FCurrentRecord);
      end;
    end;
  end
  else
  begin
    if State = dsEdit then
      FFilterList.Remove(FCurrentRecord);
  end;
  FIsFiltered := FFilterList.Count <> FData.Count;
end;

procedure TdxEMFCustomDataSet.CalculateFilterList;
var
  Accepted: Boolean;
begin
  FIsFiltered := False;
  if not Active then
    Exit;
  CheckBrowseMode;
  DisableControls;
  try
    FFilterList.Clear;
    if Filtered and Assigned(OnFilterRecord) then
    begin
      First;
      while not EOF do
      begin
        Accepted := True;
        OnFilterRecord(Self, Accepted);
        if Accepted then
          FFilterList.Add(RecNo - 1);
        Next;
      end;
      FIsFiltered := FData.Count <> FFilterList.Count;
    end;
    ClearBuffers;
    First;
  finally
    EnableControls;
  end;
end;

procedure TdxEMFCustomDataSet.InternalInitRecord(ABuffer: TRecordBuffer);
begin
  FillChar(ABuffer^, FRecInfoOffset, 0);
  GetBufferRecInfo(ABuffer).TheObject := nil;
end;

procedure TdxEMFCustomDataSet.InternalLast;
begin
  if not FIsFiltered then
  begin
    FData.FetchAll;
    FCurrentRecord := FData.Count;
  end
  else
  begin
    FFilterCurrentRecord := FFilterList.Count;
    FCurrentRecord := FData.Count;
  end;
end;

class function TdxEMFCustomDataSet.GetCharSize(AFieldType: TFieldType): Integer;
begin
  case AFieldType of
    ftString, ftGuid:
      Result := 1;
    ftWideString:
      Result := 2;
  else
    Result := 0;
  end;
end;

class function TdxEMFCustomDataSet.GetDataSize(AValue: Variant; AField: TField): Integer;
var
  ADataSize: Integer;
begin
  if AField.DataType in ftStrings then
  begin
    if not VarIsNull(AValue) then
      ADataSize := Length(AValue)
    else
      ADataSize := AField.Size;
    Result := (ADataSize + 1) * GetCharSize(AField.DataType)
  end
  else
    Result := AField.DataSize;
end;

function TdxEMFCustomDataSet.GetDataSetFieldCollection(ADataSetField: TDataSetField): IdxEMFCollection;
var
  ADataSet: TdxEMFCustomDataSet;
  AMasterData: TObject;
  AField: TField;
  AFieldInfo: TdxEMFCustomDataSet.TFieldInfo;
  AData: TValue;
begin
  ADataSet := ADataSetField.DataSet as TdxEMFCustomDataSet;
  AField := ADataSet.FindField(ADataSetField.FieldName);
  if not ADataSet.FieldInfos.TryGetValue(AField, AFieldInfo) then
    Exit(nil);
  AMasterData := ADataSet.CurrentObject;
  AData := AFieldInfo.MemberInfo.GetValue(AMasterData);
  Result := AData.AsInterface as IdxEMFCollection;
end;

class function TdxEMFCustomDataSet.GetDataSize(AField: TField): Integer;
begin
  Result := GetDataSize(Null, AField);
end;

procedure TdxEMFCustomDataSet.InternalOpen;
var
  I: Integer;
  AField: TField;
  AFieldInfo: TFieldInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  if not IsExternalData then
  begin
    CheckSession;
    if not(csReading in ComponentState) then
      CheckEntityInfo;
  end;
  FIsCursorOpen := True;
  FCurrentRecord := -1;
  FFilterCurrentRecord := -1;

  FieldDefs.Updated := False;
  FieldDefs.Update;
  FieldDefList.Update;

{$IFNDEF DELPHIXE6}
  if DefaultFields then
{$ENDIF}
    CreateFields;

  FFieldInfos.Clear;
  FRecordSize := 0;
  for I := 0 to FieldCount - 1 do
  begin
    AField := Fields[I];
    AMemberInfo := EntityInfo.FindMemberByColumnName(AField.FieldName);
    AFieldInfo := TFieldInfo.Create(AField, AMemberInfo, FRecordSize);
    FFieldInfos.Add(AField, AFieldInfo);
    Inc(FRecordSize, GetDataSize(AField) + 1);
  end;
  FArrayFieldInfos := FFieldInfos.Values.ToArray;
  BindFields(True);
  FRecInfoOffset := FRecordSize + CalcFieldsSize;
  FRecBufSize := FRecInfoOffset + SizeOf(TRecInfo);
  BookmarkSize := SizeOf(TObject);
{$IFDEF DELPHIXE3}
  SetLength(FBookmarkBuffer, BookmarkSize);
{$ENDIF}
end;

procedure TdxEMFCustomDataSet.InternalPost;
var
  ARecordBuffer: TRecordBuffer;
  AFieldInfo: TFieldInfo;
  AObject: TObject;
  AIndexToInsert: Integer;
begin
  inherited InternalPost;
  GetActiveRecordBuffer(ARecordBuffer);
  if State = dsEdit then
  begin
    AObject := GetBufferRecInfo(ARecordBuffer).TheObject;
    for AFieldInfo in FArrayFieldInfos do
      if AFieldInfo.GetByEdited(ARecordBuffer) then
      begin
        SetObjectMemberValue(AObject, ARecordBuffer, AFieldInfo);
        AFieldInfo.SetByEdited(ARecordBuffer, False);
      end;
    Session.Save(AObject);
  end
  else
  begin
    case GetBufferRecInfo(ARecordBuffer)^.BookmarkFlag of
      bfBOF:
        AIndexToInsert := 0;
      bfEOF:
        AIndexToInsert := -1;
    else
      AIndexToInsert := FCurrentRecord;
    end;
    if not TdxEMFCustomSessionAccess(Session).TrackingChanges then
      Session.BeginTrackingChanges;
    AObject := CreateObject;
    for AFieldInfo in FArrayFieldInfos do
      if AFieldInfo.GetByEdited(ARecordBuffer) then
      begin
        SetObjectMemberValue(AObject, ARecordBuffer, AFieldInfo);
        AFieldInfo.SetByEdited(ARecordBuffer, False);
      end;
    FData.Insert(AIndexToInsert, AObject);
    if AIndexToInsert = -1 then
      FCurrentRecord := FData.Count - 1
    else
      FCurrentRecord := AIndexToInsert;
    Session.Save(AObject);
    if not TdxEMFCustomSessionAccess(Session).TrackingChanges then
      Session.FlushChanges;
  end;
  UpdateFilterRecord;
end;

procedure TdxEMFCustomDataSet.InternalSetToRecord(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF});
{$IFNDEF DELPHIXE3}
var
  ABookmark: TObject;
  FBookmarkBuffer: Pointer;
{$ENDIF}
begin
{$IFNDEF DELPHIXE3}
  FBookmarkBuffer := @ABookmark;
{$ENDIF}
  GetBufferRecInfo(TRecordBuffer(ABuffer))^.GetBookmark(FBookmarkBuffer);
  InternalGotoBookmark(FBookmarkBuffer);
end;

function TdxEMFCustomDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsCursorOpen;
end;

procedure TdxEMFCustomDataSet.DataEvent(AEvent: TDataEvent; AInfo: {$IFDEF DELPHIXE2}NativeInt{$ELSE}Longint{$ENDIF});
begin
  if (AEvent = deParentScroll) and (DataSetField <> nil) then
  begin
    CheckBrowseMode;
    UpdateDataSetField
  end;
  inherited DataEvent(AEvent, AInfo);
end;

{$IFNDEF DELPHIXE6}
function TdxEMFCustomDataSet.DefaultFields: Boolean;
begin
  Result := inherited DefaultFields or (FieldCount = 0);
end;
{$ENDIF}

procedure TdxEMFCustomDataSet.SetBookmarkData(ABuffer: {$IFDEF DELPHIXE4}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};
  AData: {$IFDEF DELPHIXE3}TBookmark{$ELSE}Pointer{$ENDIF});
begin
  GetBufferRecInfo(Pointer(ABuffer))^.SetBookmark(AData);
end;

function TdxEMFCustomDataSet.GetBlobData(ABuffer: TRecordBuffer; AField: TField; AStream: TStream): Boolean;
var
  AFieldInfo: TFieldInfo;
  AObject, AMember: TObject;
  AValue: TValue;
  APosition: Integer;
  ATmpStream: TMemoryStream;
  S: string;
begin
  Result := False;
  if not FFieldInfos.TryGetValue(AField, AFieldInfo) then
    Exit;
  if FBlobStreams.TryGetValue(AFieldInfo, ATmpStream) then
  begin
    Result := ATmpStream.Size > 0;
    if Result then
    begin
      ATmpStream.Position := 0;
      APosition := AStream.Position;
      ATmpStream.SaveToStream(AStream);
      AStream.Position := APosition;
    end;
    Exit;
  end;
  AObject := GetBufferRecInfo(ABuffer).ActualObject;
  if AObject = nil then
    Exit;
  AValue := AFieldInfo.MemberInfo.GetValue(AObject);
  if AValue.IsEmpty then
    Exit;
  if AValue.IsObject then
  begin
    AMember := AValue.AsObject;
    Result := AMember <> nil;
    if Result then
    begin
      APosition := AStream.Position;
      AFieldInfo.MemberInfo.Serializer.SaveToStream(AMember, AStream);
      AStream.Position := APosition;
    end;
  end
  else
  if AValue.IsArray then
  begin
    APosition := AStream.Position;
    AStream.WriteBuffer(TBytes(AValue.GetReferenceToRawData^)[0], AValue.GetArrayLength);
    AStream.Position := APosition;
  end
  else
  begin
    S := AValue.ToString;
    APosition := AStream.Position;
    AStream.WriteBuffer(S[1], Length(S) * SizeOf(Char));
    AStream.Position := APosition;
  end
end;

procedure TdxEMFCustomDataSet.SetBlobData(ABuffer: TRecordBuffer; AField: TField; AStream: TStream);
var
  AFieldInfo: TFieldInfo;
  ATmpStream: TMemoryStream;
  APosition: Integer;
begin
  if not FFieldInfos.TryGetValue(AField, AFieldInfo) then
    Exit;
  AFieldInfo.SetByEdited(ABuffer, True);
  if not FBlobStreams.TryGetValue(AFieldInfo, ATmpStream) then
  begin
    ATmpStream := TMemoryStream.Create;
    FBlobStreams.Add(AFieldInfo, ATmpStream);
  end;
  ATmpStream.Clear;
  APosition := AStream.Position;
  ATmpStream.LoadFromStream(AStream);
  AStream.Position := APosition;
end;

function TdxEMFCustomDataSet.LocateRecord(const AKeyFields: string; const AKeyValues: Variant;
  AOptions: TLocateOptions; ASyncCursor: Boolean): Boolean;

  function GetValueList(ACount: Integer): Variant;
  var
    I: Integer;
  begin
    Result := VarArrayCreate([0, ACount - 1], varVariant);
    for I := 0 to ACount - 1 do
    begin
      if not VarIsArray(AKeyValues) then
        Result[I] := AKeyValues
      else
        Result[I] := AKeyValues[I];
    end;
  end;

var
  AFields: TList{$IFDEF DELPHIXE3}<TField>{$ENDIF};
  AValueList: Variant;
  I, AStoreIndex: Integer;
  AValue: Variant;
  AField: TField;
  ABuffer: TRecordBuffer;
  S: string;
begin
  Result := False;
  CheckBrowseMode;
  AStoreIndex := FCurrentRecord;
  UpdateCursorPos;
  CursorPosChanged;

  AFields := TList{$IFDEF DELPHIXE3}<TField>{$ENDIF}.Create;
  try
    GetFieldList(AFields, AKeyFields);
    AValueList := GetValueList(AFields.Count);
    SetTempState(dsFilter);
    try
      InternalFirst;
      ABuffer := TRecordBuffer(TempBuffer);
      while GetRecord(ABuffer, gmNext, True) = grOK do
      begin
        for I := 0 to AFields.Count - 1 do
        begin
          AField := TField(AFields[I]);
          AValue := AValueList[I];
          if not AField.IsNull and VarIsStr(AValue) and
            (AField.DataType in [ftString, ftFixedChar, ftWideString, ftGuid, ftFixedWideChar]) then
          begin
            S := AField.AsString;
            if loPartialKey in AOptions then
              S := Copy(S, 1, Length(AValue));
            if loCaseInsensitive in AOptions then
              Result := CompareText(S, AValue) = 0
            else
              Result := CompareStr(S, AValue) = 0;
          end
          else
            Result := VarSameValue(AField.Value, AValue);
          if not Result then
            Break;
        end;
        if Result then
        begin
          if not ASyncCursor then
            GetCurrentRecord(TempBuffer);
          Break;
        end;
      end;
      if not Result or not ASyncCursor then
        FCurrentRecord := AStoreIndex;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    AFields.Free;
  end;
end;

function TdxEMFCustomDataSet.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  VarClear(Result);
  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

procedure TdxEMFCustomDataSet.SetBookmarkFlag(ABuffer: TRecordBuffer; AValue: TBookmarkFlag);
begin
  GetBufferRecInfo(ABuffer)^.BookmarkFlag := AValue;
end;

procedure TdxEMFCustomDataSet.SetDataContext(Value: TdxEMFCustomDataContext);
begin
  if DataContext = Value then
    Exit;
  CheckInactive;
  FEMFDataSource.DataContext := Value;
  FPackageName := '';
  Data.Reset;
  DoAssignEntity;
  PropertyChanged;
end;

procedure TdxEMFCustomDataSet.SetDataSetField(const Value: TDataSetField);
begin
  if Value <> DataSetField then
    Data.Reset;
  EntityClass := nil;
  inherited SetDataSetField(Value);
end;

procedure TdxEMFCustomDataSet.SetDataSetFieldEntity;
var
  ADataSet: TdxEMFCustomDataSet;
  AFieldInfo: TFieldInfo;
  AEntityInfo: TdxEntityInfo;
begin
  ADataSet := DataSetField.DataSet as TdxEMFCustomDataSet;
  if ADataSet.FieldInfos.TryGetValue(ADataSet.FindField(DataSetField.FieldName), AFieldInfo) then
  begin
    AEntityInfo := AFieldInfo.MemberInfo.CollectionElementType;
    EMFDataSource.EntityClass := AEntityInfo.ClassAttributes.PersistentClass;
  end;
  Session := ADataSet.Session;
end;

procedure TdxEMFCustomDataSet.SetEntityName(const Value: string);
begin
  if EntityName = Value then
    Exit;
  CheckInactive;
  FEMFDataSource.EntityName := Value;
  Data.Reset;
  PropertyChanged;
end;

procedure TdxEMFCustomDataSet.SetFieldData(AField: TField; ABuffer: TValueBuffer);
var
  ARecBuf: TRecordBuffer;
  AFieldInfo: TFieldInfo;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);
  if not GetActiveRecordBuffer(ARecBuf) then
    Exit;
  if AField.ReadOnly and not(State in [dsSetKey, dsFilter]) then
    DatabaseErrorFmt(SFieldReadOnly, [AField.DisplayName]);
  AField.Validate(ABuffer);

  if not FFieldInfos.TryGetValue(AField, AFieldInfo) then
    Exit;
  AFieldInfo.CopyFromBuffer(ARecBuf, ABuffer);
  AFieldInfo.SetByEdited(ARecBuf, True);

  if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
{$IFDEF DELPHIXE3}
    DataEvent(deFieldChange, IntPtr(AField));
{$ELSE}
    DataEvent(deFieldChange, NativeInt(AField));
{$ENDIF}
end;

procedure TdxEMFCustomDataSet.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    if Active then
    begin
      CheckBrowseMode;
      inherited SetFiltered(Value);
      CalculateFilterList;
    end
    else
      inherited SetFiltered(Value);
  end;
end;

procedure TdxEMFCustomDataSet.SetIndex(const AValue: string);
begin
  if Active then
    CheckBrowseMode;
  if (FInternalIndexName <> AValue) then
  begin
    FInternalIndexName := AValue;
    if Active then
      SwitchToIndex;
  end;
end;

procedure TdxEMFCustomDataSet.SetIndexFieldNames(const Value: string);
begin
  FFieldsIndex := True;
  SetIndex(Value);
end;

procedure TdxEMFCustomDataSet.SetIndexName(const Value: string);
begin
  FFieldsIndex := False;
  SetIndex(Value);
end;

procedure TdxEMFCustomDataSet.SetPackageName(const Value: string);
begin
  if PackageName = Value then
    Exit;
  FEMFDataSource.PackageName := Value;
  Data.Reset;
  DoAssignEntity;
end;

procedure TdxEMFCustomDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  inherited SetOnFilterRecord(Value);
  CalculateFilterList;
end;

{$WARN USE_BEFORE_DEF OFF}

procedure TdxEMFCustomDataSet.SetObjectMemberValue(AObject: TObject; AActiveBuffer: TRecordBuffer;
  const AFieldInfo: TFieldInfo);
var
  AData: TValue;
  AInt64: Int64;
  AStr: string;
  AGUID: TGUID;
  ADateTime: TDateTime;
  AMember: TObject;
  AStream: TMemoryStream;
  {$IFDEF DELPHIXE3}
  ATempBuffer: TValueBuffer;
  {$ENDIF}
  ABytesStream: TBytesStream;
  ABytes: TBytes;
  AEncoding: TEncoding;
begin
  if (AFieldInfo.MemberInfo = nil) or AFieldInfo.MemberInfo.IsVirtualColumn or
    (not AFieldInfo.HasBufferData(AActiveBuffer) and AFieldInfo.MemberInfo.IsNullable) then
    Exit;
  if AFieldInfo.MemberInfo.IsEnumeration then
  begin
    case AFieldInfo.Field.DataType of
      ftShortint, ftByte:
        AInt64 := PShortint(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftSmallint, ftWord:
        AInt64 := PSmallint(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftInteger, ftLongWord:
        AInt64 := PInteger(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftLargeint:
        AInt64 := PInt64(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      else
        NotImplemented;
    end;
    AData := TValue.FromOrdinal(AFieldInfo.MemberInfo.MemberType.Handle, AInt64);
  end
  else
    case AFieldInfo.Field.DataType of
      ftSmallint:
        AData := TValue.From(PSmallint(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftInteger:
        AData := PInteger(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftWord:
        AData := TValue.From(PWord(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftBoolean:
        AData := PBoolean(PWordBool(AFieldInfo.GetFieldBuffer(AActiveBuffer)))^;
      ftFloat, ftCurrency:
        AData := PDouble(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftFmtBcd:
        AData := TValue.From(PBcd(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftDate, ftTime, ftDateTime:
        begin
          {$IFDEF DELPHIXE3}
          SetLength(ATempBuffer, SizeOf(TDateTime));
          DataConvert(AFieldInfo.Field, AFieldInfo.GetData(AActiveBuffer), ATempBuffer, False);
          {$IFDEF DELPHIXE8}
          ADateTime := TBitConverter.UnsafeInTo<TDateTime>(ATempBuffer);
          {$ELSE}
          ADateTime := TBitConverter.ToDouble(ATempBuffer);
          {$ENDIF}
          {$ELSE}
          DataConvert(AFieldInfo.Field, AFieldInfo.GetFieldBuffer(AActiveBuffer), @ADateTime, False);
          {$ENDIF}
          AData := TValue.From(ADateTime);
        end;
      ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftWideMemo:
        begin
          if FBlobStreams.TryGetValue(AFieldInfo, AStream) then
          begin
            AStream.Position := 0;
            if AFieldInfo.MemberInfo.IsSerialize then
            begin
              AData := AFieldInfo.MemberInfo.GetValue(AObject);
              AMember := AData.AsObject;
              if AMember <> nil then
                AFieldInfo.MemberInfo.Serializer.LoadFromStream(AMember, AStream);
              Exit;
            end
            else
              if AFieldInfo.MemberInfo.IsType(TypeInfo(string)) or AFieldInfo.MemberInfo.IsDynamicArrayType then
              begin
                ABytesStream := TBytesStream.Create;
                try
                  ABytesStream.LoadFromStream(AStream);
                  ABytes := ABytesStream.Bytes;
                  SetLength(ABytes, ABytesStream.Size);
                  if AFieldInfo.MemberInfo.IsType(TypeInfo(string)) then
                  begin
                    AEncoding := nil;
                    TEncoding.GetBufferEncoding(ABytes, AEncoding, TEncoding.Unicode);
                    AStr := AEncoding.GetString(ABytes);
                    AData := AStr;
                  end
                  else
                    AData := AFieldInfo.MemberInfo.ConvertArray(ABytes);
                finally
                  ABytesStream.Free;
                end;
              end
              else
                Exit;
          end;
        end;
      ftWideString, ftFixedWideChar:
        begin
          AStr := WideCharToString(PChar(AFieldInfo.GetFieldBuffer(AActiveBuffer)));
          AData := AStr;
        end;
      ftLargeint, ftAutoInc:
        AData := PInt64(AFieldInfo.GetFieldBuffer(AActiveBuffer))^;
      ftGuid:
        begin
        {$IFNDEF NEXTGEN}
          AStr := dxAnsiStringToString(PAnsiChar(AFieldInfo.GetFieldBuffer(AActiveBuffer)));
          AGUID := TGUID.Create(AStr);
          AData := TValue.From<TGUID>(AGUID);
        {$ELSE}
          NotImplemented;
        {$ENDIF}
        end;
      ftLongWord:
        AData := TValue.From(PLongWord(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftShortint:
        AData := TValue.From(PShortint(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftByte:
        AData := TValue.From(PByte(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftObject:
        AData := TValue.From(PObject(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      ftSingle:
        AData := TValue.From(PSingle(AFieldInfo.GetFieldBuffer(AActiveBuffer))^);
      else
        NotImplemented;
    end;
  AFieldInfo.MemberInfo.SetValue(AObject, AData);
end;
{$WARN USE_BEFORE_DEF DEFAULT}

procedure TdxEMFCustomDataSet.SetRecNo(AValue: Integer);

  function InternalSetRecNo(const Value: Integer): Integer;
  begin
    if not FIsFiltered then
      Result := Value - 1
    else
    begin
      FFilterCurrentRecord := Value - 1;
      Result := FFilterList[FFilterCurrentRecord];
    end;
  end;

var
  ANewCurrentRecord: Integer;
begin
  if Active then
    CheckBrowseMode;
  if (AValue > 0) and (AValue <= InternalRecordCount) then
  begin
    ANewCurrentRecord := InternalSetRecNo(AValue);
    if (ANewCurrentRecord <> FCurrentRecord) then
    begin
      DoBeforeScroll;
      FCurrentRecord := ANewCurrentRecord;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end;
end;

procedure TdxEMFCustomDataSet.SetEntityClass(const Value: TClass);
begin
  if EntityClass = Value then
    Exit;
  EMFDataSource.EntityClass := Value;
  Data.Reset;
end;

procedure TdxEMFCustomDataSet.SetEntityInfo(const Value: TdxEntityInfo);
begin
  EMFDataSource.EntityInfo := Value;
end;

procedure TdxEMFCustomDataSet.SetSession(const Value: TdxEMFCustomSession);
begin
  if Session <> Value then
  begin
    if Session <> nil then
      Session.RemoveFreeNotification(Self);
    EMFDataSource.Session := Value;
    if Session <> nil then
      Session.FreeNotification(Self);
    Changed;
  end;
end;

procedure TdxEMFCustomDataSet.SwitchToIndex;
begin

end;

{ TdxEMFCustomDataSet.TDataLinkList }

constructor TdxEMFCustomDataSet.TDataLinkList.Create(ADataSet: TdxEMFCustomDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Add(AObject: TObject);
begin
  FCollection.Add(AObject);
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Assign(AObject: TObject);
begin
  FCollection := nil;
  CreateCollection;
  Add(AObject);
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Assign(const AEMFCollection: IdxEMFCollection);
begin
  FCollection := nil;
  Collection := AEMFCollection;
  FIsExternalData := True;
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Close;
begin
  if not FIsExternalData then
    Collection := nil;
end;

procedure TdxEMFCustomDataSet.TDataLinkList.CreateCollection;
begin
  Assert((FDataSet <> nil) and (FDataSet.EntityClass <> nil));
  Assign(TdxEMFCollections.Create(FDataSet.EntityClass));
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Delete(AObject: TObject);
begin
  if not FCollection.DeleteObjectOnRemove then
    Session.Delete(AObject);
  FCollection.Remove(AObject);
end;

procedure TdxEMFCustomDataSet.TDataLinkList.FetchAll;
begin
  if HasLoader then
    while TdxEMFCustomCollectionAccess(FInternalCollection).LoadNextObject do;
end;

function TdxEMFCustomDataSet.TDataLinkList.GetCount: Integer;
begin
  if FCollection = nil then
    Result := 0
  else
    Result := FCollection.Count;
end;

function TdxEMFCustomDataSet.TDataLinkList.GetHasLoader: Boolean;
begin
  Result := (FInternalCollection <> nil) and (TdxEMFCustomCollection(FInternalCollection).Loader <> nil);
end;

function TdxEMFCustomDataSet.TDataLinkList.GetIsAssigned: Boolean;
begin
  Result := FCollection <> nil;
end;

function TdxEMFCustomDataSet.TDataLinkList.GetItems(AIndex: Integer): TObject;
begin
  Result := FCollection[AIndex];
end;

function TdxEMFCustomDataSet.TDataLinkList.GetSession: TdxEMFCustomSession;
begin
  if FDataSet <> nil then
    Result := FDataSet.Session
  else
    Result := nil;
end;

function TdxEMFCustomDataSet.TDataLinkList.IndexOf(AObject: TObject): Integer;
var
  ACurObject: TObject;
begin
  Result := -1;
  if FCollection = nil then
    Exit;
  if FInternalCollection = nil then
  begin
    for ACurObject in FCollection do
    begin
      Inc(Result);
      if ACurObject = AObject then
        Exit;
    end;
    Result := -1;
  end
  else
    Result := FCollection.IndexOf(AObject);
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Insert(AIndex: Integer; AObject: TObject);
var
  AList: IdxList;
begin
  if (AIndex = -1) or (FInternalCollection = nil) then
    Add(AObject)
  else
  begin
    if Supports(FInternalCollection, IdxList, AList) and AList.Contains(AObject) then
      Exit;
    FInternalCollection.Insert(AIndex, AObject);
  end;
end;

function TdxEMFCustomDataSet.TDataLinkList.MoveNext: Boolean;
begin
  Result := (FDataSet.CurrentRecord + 1) < Count;
  if not Result and HasLoader then
    Result := TdxEMFCustomCollectionAccess(FInternalCollection).LoadNextObject;
end;

procedure TdxEMFCustomDataSet.TDataLinkList.Reset;
begin
  DataSet.Close;
  FIsExternalData := False;
  FCollection := nil;
  FInternalCollection := nil;
end;

procedure TdxEMFCustomDataSet.TDataLinkList.SetCollection(const Value: IdxEMFCollection);
begin
  FCollection := Value;
  FInternalCollection := IEnumerable(Collection) as TdxEMFCustomCollection;
end;

{ TdxEMFDataSetBlobStream }

constructor TdxEMFDataSetBlobStream.Create(AField: TBlobField; AMode: TBlobStreamMode);
begin
  inherited Create;
  FField := AField;
  FDataSet := TdxEMFCustomDataSet(FField.DataSet);
  FMode := AMode;
  if not FDataSet.GetActiveRecordBuffer(FBuffer) then
    Exit;
  if not FField.Modified and (FMode <> bmRead) then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not(FDataSet.State in dsEditModes) then
      DatabaseError(SNotEditing);
  end;
  ReadData;
end;

destructor TdxEMFDataSetBlobStream.Destroy;
begin
  if FModified then
  begin
    try
      Position := 0;
      FDataSet.SetBlobData(FBuffer, FField, Self);
      FField.Modified := True;
      FDataSet.DataEvent(deFieldChange, TdxNativeInt(FField));
    except
      if Assigned(Classes.ApplicationHandleException) then
        ApplicationHandleException(Self);
    end;
  end;
  inherited Destroy;
end;

procedure TdxEMFDataSetBlobStream.ReadData;
begin
  if FMode = bmWrite then
  begin
    Clear;
    FModified := True;
  end
  else
  begin
    FDataSet.GetBlobData(FBuffer, FField, Self);
    FModified := False;
  end;
end;

function TdxEMFDataSetBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

{ TdxEMFTableDataSource }

procedure TdxEMFTableDataSource.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value or IsExternalData then
    Exit;
  if EntityInfo = nil then
    DoAssignEntity;
  if EntityInfo <> nil then
    FDataCollection := Session.GetObjects(EntityInfo.ClassAttributes.PersistentClass);
end;

{ TdxEMFDataSet }

procedure TdxEMFDataSet.AssignData(AData: TObject);
begin
  CheckSession;
  Close;
  EntityName := '';
  EntityClass := AData.ClassType;
  Data.Assign(AData);
end;

procedure TdxEMFDataSet.AssignData(const AData: IdxEMFCollection);
var
  ACollection: TdxEMFCustomCollection;
begin
  Close;
  EntityName := '';
  ACollection := AData as TdxEMFCustomCollection;
  if ACollection.Session <> nil then
    Session := ACollection.Session;
  EntityClass := ACollection.CollectionElementClass;
  Data.Assign(AData);
end;

procedure TdxEMFDataSet.AssignData<T>(const AData: TArray<T>);
var
  AItem: T;
begin
  Close;
  EntityName := '';
  if Length(AData) = 0 then
    Exit;
  EntityClass := TClass(T);
  Data.CreateCollection;
  for AItem in AData do
    Data.Add(AItem);
end;


procedure TdxEMFDataSet.AssignData<T>(const AData: IdxEMFCollection<T>);
begin
  AssignData(AData as IdxEMFCollection);
end;


type
  TdxSelectStatementResultAccess = class(TdxSelectStatementResult);

  { TdxEMFCustomQueryDataSet.TEMFDataLink }

constructor TdxEMFCustomQueryDataSet.TEMFDataLink.Create(ADataSet: TdxEMFCustomQueryDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

procedure TdxEMFCustomQueryDataSet.TEMFDataLink.ActiveChanged;
begin
  if FDataSet.Active then
    FDataSet.RefreshParams;
end;

procedure TdxEMFCustomQueryDataSet.TEMFDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

function TdxEMFCustomQueryDataSet.TEMFDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TdxEMFCustomQueryDataSet.TEMFDataLink.RecordChanged(AField: TField);
begin
  if (AField = nil) and FDataSet.Active then
    FDataSet.RefreshParams;
end;

{ TdxEMFCustomQueryDataSet }

constructor TdxEMFCustomQueryDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TEMFDataLink.Create(Self);
  ObjectView := True;
end;

function TdxEMFCustomQueryDataSet.CreateEMFDataSource: TdxCustomEMFDataSource;
begin
  Result := TdxCustomEMFQueryDataSource.Create(Self);
end;

destructor TdxEMFCustomQueryDataSet.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FQueryData);
  FreeAndNil(FDataLink);
end;

function TdxEMFCustomQueryDataSet.GetEMFDataSource: TdxCustomEMFQueryDataSource;
begin
  Result := TdxCustomEMFQueryDataSource(inherited EMFDataSource);
end;

procedure TdxEMFCustomQueryDataSet.DataEvent(AEvent: TDataEvent; AInfo: {$IFDEF DELPHIXE2}NativeInt{$ELSE}Longint{$ENDIF});
begin
  if (AEvent = deParentScroll) and (DataSetField = nil) then
  begin
    CheckBrowseMode;
    RefreshParams;
  end;
  inherited DataEvent(AEvent, AInfo);
end;

procedure TdxEMFCustomQueryDataSet.DoUpdateExpressions;
begin
  FreeAndNil(FQueryData);
  EntityInfo := nil;
end;

procedure TdxEMFCustomQueryDataSet.EndLoad(const AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
  ASorting: IdxSortByExpressions; AInfoQuery: Boolean);
var
  AEntityInfo: TdxEntityInfo;
begin
  FreeAndNil(FQueryData);
  AEntityInfo := EntityManager.GetEntityInfo(EntityName);
  if AEntityInfo = nil then
    Exit;
  Data.Reset;
  EMFDataSource.QueryProperties.ApplyParams;
  if (AProperties.Count = 0) and (AGroupProperties.Count = 0) then
  begin
    if not AInfoQuery then
    begin
      CheckSession;
      Data.Collection := Session.GetObjects(AEntityInfo.ClassAttributes.PersistentClass, Criteria, SortByExpressions,
        SkipSelectedRecords, TopSelectedRecords);
    end;
  end
  else
  begin
    CheckSession;
    FQueryData := TdxEMFCustomSessionAccess(Session).SelectData(AEntityInfo, AProperties, Criteria, AGroupProperties,
      GroupCriteria, { SelectDeleted } False, SkipSelectedRecords, TopSelectedRecords, ASorting);
    if not AInfoQuery then
      FetchQueryData;
  end;
end;

procedure TdxEMFCustomQueryDataSet.FetchQueryData;
var
  ASelectStatementResult: TdxSelectStatementResult;
begin
  if FQueryData = nil then
    Exit;
  ASelectStatementResult := TdxSelectStatementResult.Create(FQueryData);
  Data.Collection := TdxSelectStatementCollections.Create(ASelectStatementResult);
  EntityInfo := TdxSelectStatementResultAccess(ASelectStatementResult).EntityInfo;
  FQueryData := nil;
end;

function TdxEMFCustomQueryDataSet.GetCriteria: IdxCriteriaOperator;
begin
  Result := EMFDataSource.QueryProperties.Criteria;
end;

function TdxEMFCustomQueryDataSet.GetCriteriaText: string;
begin
  Result := EMFDataSource.QueryProperties.CriteriaText;
end;

function TdxEMFCustomQueryDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TdxEMFCustomQueryDataSet.GetFieldExpressions: TdxFieldExpressions;
begin
  Result := EMFDataSource.QueryProperties.FieldExpressions;
end;

function TdxEMFCustomQueryDataSet.GetGroupByExpressionDefinitions: TdxGroupByExpressionDefinitions;
begin
  Result := EMFDataSource.QueryProperties.GroupByExpressionDefinitions;
end;

function TdxEMFCustomQueryDataSet.GetGroupCriteria: IdxCriteriaOperator;
begin
  Result := EMFDataSource.QueryProperties.GroupCriteria;
end;

function TdxEMFCustomQueryDataSet.GetGroupCriteriaText: string;
begin
  Result := EMFDataSource.QueryProperties.GroupCriteriaText;
end;

function TdxEMFCustomQueryDataSet.GetIsProjection: Boolean;
begin
  Result := IsGroupByExpressionDefinitionsStored or IsFieldExpressionsStored;
end;

function TdxEMFCustomQueryDataSet.GetParams: TParams;
begin
  Result := EMFDataSource.QueryProperties.Params;
end;

function TdxEMFCustomQueryDataSet.GetSkipSelectedRecords: Integer;
begin
  Result := EMFDataSource.QueryProperties.SkipSelectedRecords;
end;

function TdxEMFCustomQueryDataSet.GetSortByExpressionDefinitions: TdxSortByExpressionDefinitions;
begin
  Result := EMFDataSource.QueryProperties.SortByExpressionDefinitions;
end;

function TdxEMFCustomQueryDataSet.GetSortByExpressions: IdxSortByExpressions;
begin
  Result := EMFDataSource.QueryProperties.SortByExpressions;
end;

function TdxEMFCustomQueryDataSet.GetTopSelectedRecords: Integer;
begin
  Result := EMFDataSource.QueryProperties.TopSelectedRecords;
end;

procedure TdxEMFCustomQueryDataSet.InternalInitFieldDefsFromEntity;

  procedure CreateFieldDefs(AFields: TFields; AFieldDefs: TFieldDefs);
  var
    I: Integer;
    AField: TField;
    AFieldDef: TFieldDef;
  begin
    for I := 0 to AFields.Count - 1 do
    begin
      AField := AFields[I];
      if AField.FieldKind = fkData then
      begin
        AFieldDef := AFieldDefs.AddFieldDef;
        AFieldDef.Name := AField.FieldName;
        AFieldDef.DataType := AField.DataType;
        AFieldDef.Size := AField.Size;
        if AField.Required then
          AFieldDef.Attributes := [faRequired];
        if AField.ReadOnly then
          AFieldDef.Attributes := AFieldDef.Attributes + [faReadonly];
        if (AField.DataType = ftBCD) and (AField is TBCDField) then
          AFieldDef.Precision := TBCDField(AField).Precision
        else if (AField.DataType = ftFMTBcd) and (AField is TFMTBCDField) then
          AFieldDef.Precision := TFMTBCDField(AField).Precision;
        if AField is TObjectField then
          CreateFieldDefs(TObjectField(AField).Fields, AFieldDef.ChildDefs);
      end;
    end;
  end;

begin
  if FQueryData <> nil then
  begin
    FieldDefs.BeginUpdate;
    try
      // # todo: FQueryData.DataLoader as TdxDataSetLoader ??
      CreateFieldDefs((FQueryData.DataLoader as TdxDataSetLoader).DataSet.Fields, FieldDefs);
    finally
      FieldDefs.EndUpdate;
    end;
  end
  else
    inherited InternalInitFieldDefsFromEntity;
end;

procedure TdxEMFCustomQueryDataSet.InternalLoad(AInfoQuery: Boolean);
var
  AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
  ASorting: IdxSortByExpressions;
begin
  PrepareLoad(AProperties, AGroupProperties, ASorting);
  EndLoad(AProperties, AGroupProperties, ASorting, AInfoQuery);
end;

procedure TdxEMFCustomQueryDataSet.InternalRefresh;
begin
  SetState(dsInactive);
  CloseCursor;
  OpenCursor(False);
  SetState(dsBrowse);
end;

function TdxEMFCustomQueryDataSet.IsFieldExpressionsStored: Boolean;
begin
  Result := TdxCustomEMFQueryPropertiesAccess(EMFDataSource.QueryProperties).IsFieldExpressionsStored;
end;

function TdxEMFCustomQueryDataSet.IsGroupByExpressionDefinitionsStored: Boolean;
begin
  Result := TdxCustomEMFQueryPropertiesAccess(EMFDataSource.QueryProperties).IsGroupByExpressionDefinitionsStored;
end;

function TdxEMFCustomQueryDataSet.IsSortByExpressionDefinitionsStored: Boolean;
begin
  Result := TdxCustomEMFQueryPropertiesAccess(EMFDataSource.QueryProperties).IsSortByExpressionDefinitionsStored;
end;

procedure TdxEMFCustomQueryDataSet.OpenCursor(AInfoQuery: Boolean);
begin
  if DataSetField = nil then
  begin
    if DataSource <> nil then
      SetParamsFromCursor;
    if not AInfoQuery or IsProjection then
    begin
      if not Data.IsAssigned and (EntityName <> '') then
      begin
        if EntityInfo = nil then
          DoAssignEntity;
        InternalLoad(AInfoQuery);
      end
      else
        FetchQueryData;
    end;
  end;
  inherited OpenCursor(AInfoQuery);
end;

procedure TdxEMFCustomQueryDataSet.PrepareLoad(out AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
  out ASorting: IdxSortByExpressions);
var
  AFieldExpression: TdxFieldExpression;
  ASortBy: TdxSortByExpressions;
  AGroupByExpressionDefinition: TdxGroupByExpressionDefinition;
  ASortByExpressionDefinition: TdxSortByExpressionDefinition;
  I: Integer;
begin
  AProperties := TdxCriteriaOperatorCollection.Create;
  AGroupProperties := TdxCriteriaOperatorCollection.Create;
  ASortBy := TdxSortByExpressions.Create;
  for I := 0 to FieldExpressions.Count - 1 do
  begin
    AFieldExpression := FieldExpressions[I];
    AProperties.Add(AFieldExpression.Expression);
  end;
  for I := 0 to GroupByExpressionDefinitions.Count - 1 do
  begin
    AGroupByExpressionDefinition := GroupByExpressionDefinitions[I];
    AGroupProperties.Add(AGroupByExpressionDefinition.Expression);
  end;
  for I := 0 to SortByExpressionDefinitions.Count - 1 do
  begin
    ASortByExpressionDefinition := SortByExpressionDefinitions[I];
    ASortBy.Add(TdxSortByExpression.Create(ASortByExpressionDefinition.Expression, ASortByExpressionDefinition.SortDirection));
  end;
  ASorting := ASortBy;
end;

procedure TdxEMFCustomQueryDataSet.RefreshParams;
var
  ADataSet: TDataSet;
begin
  DisableControls;
  try
    if DataSource <> nil then
    begin
      ADataSet := DataSource.DataSet;
      if ADataSet <> nil then
        if ADataSet.Active and (ADataSet.State <> dsSetKey) then
          Refresh;
    end;
  finally
    EnableControls;
  end;
end;

procedure TdxEMFCustomQueryDataSet.SetCriteria(const Value: IdxCriteriaOperator);
begin
  EMFDataSource.QueryProperties.Criteria := Value;
  Data.Reset;
end;

procedure TdxEMFCustomQueryDataSet.SetCriteriaText(const Value: string);
begin
  EMFDataSource.QueryProperties.CriteriaText := Value;
  Data.Reset;
end;

procedure TdxEMFCustomQueryDataSet.SetDataSource(const Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

procedure TdxEMFCustomQueryDataSet.SetFieldExpressions(const Value: TdxFieldExpressions);
begin
  EMFDataSource.QueryProperties.FieldExpressions := Value;
  DoUpdateExpressions;
end;

procedure TdxEMFCustomQueryDataSet.SetGroupByExpressionDefinitions(const Value: TdxGroupByExpressionDefinitions);
begin
  GroupByExpressionDefinitions.Assign(Value);
  DoUpdateExpressions;
end;

procedure TdxEMFCustomQueryDataSet.SetGroupCriteria(const Value: IdxCriteriaOperator);
begin
  EMFDataSource.QueryProperties.GroupCriteria := Value;
  Data.Reset;
end;

procedure TdxEMFCustomQueryDataSet.SetGroupCriteriaText(const Value: string);
begin
  EMFDataSource.QueryProperties.GroupCriteriaText := Value;
  Data.Reset;
end;

procedure TdxEMFCustomQueryDataSet.SetParams(const Value: TParams);
begin
  EMFDataSource.QueryProperties.Params.Assign(Value);
end;

procedure TdxEMFCustomQueryDataSet.SetParamsFromCursor;
var
  I: Integer;
  ADataSet: TDataSet;
begin
  if (DataSource <> nil) and (Params.Count > 0) then
  begin
    ADataSet := DataSource.DataSet;
    if ADataSet <> nil then
    begin
      for I := 0 to Params.Count - 1 do
        if not Params[I].Bound then
        begin
          if not ADataSet.EOF then
            Params[I].AssignField(ADataSet.FieldByName(Params[I].Name))
          else
            Params[I].Clear;
          Params[I].Bound := False;
        end;
    end;
  end;
end;

procedure TdxEMFCustomQueryDataSet.SetSkipSelectedRecords(const Value: Integer);
begin
  if EMFDataSource.QueryProperties.SkipSelectedRecords = Value then
    Exit;
  EMFDataSource.QueryProperties.SkipSelectedRecords := Value;
  Data.Reset;
end;

procedure TdxEMFCustomQueryDataSet.SetSortByExpressionDefinitions(const Value: TdxSortByExpressionDefinitions);
begin
  SortByExpressionDefinitions.Assign(Value);
end;

procedure TdxEMFCustomQueryDataSet.SetSortByExpressions(const Value: IdxSortByExpressions);
begin
  EMFDataSource.QueryProperties.SortByExpressions := Value;
end;

procedure TdxEMFCustomQueryDataSet.SetTopSelectedRecords(const Value: Integer);
begin
  if EMFDataSource.QueryProperties.TopSelectedRecords = Value then
    Exit;
  EMFDataSource.QueryProperties.TopSelectedRecords := Value;
  Data.Reset;
end;

{ TdxEMFTable }

constructor TdxEMFTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMasterDataLink := TMasterDataLink.Create(Self);
  FMasterDataLink.OnMasterChange := MasterChanged;
  FMasterDataLink.OnMasterDisable := MasterDisabled;
end;

destructor TdxEMFTable.Destroy;
begin
  FreeAndNil(FMasterDataLink);
  inherited Destroy;
end;

procedure TdxEMFTable.DoAssignData;
begin
  if IsExternalData then
    Exit;
  Close;
  DoAssignEntity;
  if EntityInfo <> nil then
  begin
    Data.Collection := Session.GetObjects(EntityInfo.ClassAttributes.PersistentClass, GetCriteria, GetSortByExpressions);
  end;
end;

function TdxEMFTable.GetCriteria: IdxCriteriaOperator;
var
  I: Integer;
  AFieldNames: string;
  ADetailFieldsList: TStringList;
begin
  Result := nil;
  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and (MasterFields <> '') then
  begin
    if DetailFields <> '' then
      AFieldNames := DetailFields
    else
      AFieldNames := InternalIndexName;
    if AFieldNames = '' then
      Exit;
    ADetailFieldsList := TStringList.Create;
    try
      I := 1;
      while I <= Length(AFieldNames) do
        ADetailFieldsList.Add(ExtractFieldName(AFieldNames, I));
      if ADetailFieldsList.Count < FMasterDataLink.Fields.Count then
        DatabaseError('UnknownError');
      for I := 0 to FMasterDataLink.Fields.Count - 1 do
      begin
        Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&And,
          Result, TdxBinaryOperator.Create(ADetailFieldsList[I],
            TValue.FromVariant(TField(FMasterDataLink.Fields[I]).Value)));
      end;
    finally
      ADetailFieldsList.Free;
    end;
  end;
end;

function TdxEMFTable.GetDataSource: TDataSource;
begin
  Result := FMasterDataLink.DataSource;
end;

function TdxEMFTable.GetMasterFields: string;
begin
  Result := FMasterDataLink.FieldNames;
end;

function TdxEMFTable.GetSortByExpressions: IdxSortByExpressions;
var
  AMemberInfo: TdxMappingMemberInfo;
  AFieldName: String;
  I: Integer;
begin
  if InternalIndexName = '' then
    Exit(nil);
  Result := TdxSortByExpressions.Create;
  I := 1;
  while I <= Length(InternalIndexName) do
  begin
    AFieldName := ExtractFieldName(InternalIndexName, I);
    AMemberInfo := EntityInfo.FindMember(AFieldName);
    if (AMemberInfo = nil) or not AMemberInfo.IsPersistent then
      DatabaseErrorFmt(SIndexFieldMissing, [AFieldName], Self);
    TdxSortByExpressions(Result).Add(TdxSortByExpression.Create(TdxOperandProperty.Create(AFieldName)));
  end;
end;

procedure TdxEMFTable.InternalOpen;
begin
  DoAssignData;
  inherited InternalOpen;
end;

procedure TdxEMFTable.InternalRefresh;
begin
  InternalTableRefresh;
end;

procedure TdxEMFTable.InternalTableRefresh;
begin
  DisableControls;
  try
    Reopen;
  finally
    EnableControls;
  end;
end;

procedure TdxEMFTable.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  Reopen;
end;

procedure TdxEMFTable.MasterDisabled(Sender: TObject);
begin
  DataEvent(dePropertyChange, 0);
  Reopen;
end;

procedure TdxEMFTable.Reopen;
begin
  if not Active then
    Exit;
  DisableControls;
  try
    SetState(dsInactive);
    CloseCursor;
    OpenCursor;
    SetState(dsBrowse);
  finally
    EnableControls;
  end;
end;

procedure TdxEMFTable.SetDataSource(const Value: TDataSource);
begin
  if (Value <> nil) and (DataSetField <> nil) then
    DatabaseError(SNoNestedMasterSource, Self);
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FMasterDataLink.DataSource := Value;
end;

procedure TdxEMFTable.SetMasterFields(const Value: string);
begin
  FMasterDataLink.FieldNames := Value;
end;

procedure TdxEMFTable.SwitchToIndex;
begin
  CheckActive;
  InternalTableRefresh;
end;

end.


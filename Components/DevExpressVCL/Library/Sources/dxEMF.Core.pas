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

unit dxEMF.Core;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

{$RTTI EXPLICIT
  FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
  PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  Types, Classes, SysUtils, TypInfo, Generics.Defaults, Generics.Collections, RTTI, DB,
  dxEMF.Types,
  dxEMF.DB.Model,
  dxEMF.Utils,
  dxEMF.Metadata,
  dxEMF.Core.Pool,
  dxEMF.Core.DBHelper,
  dxEMF.DB.Utils,
  dxEMF.DB.AliasExpander,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.Generator,
  dxEMF.DB.SQLConnectionProvider,
  dxEMF.DB.SQLGenerator,
  dxEMF.Linq,
  dxEMF.Linq.Expressions,
  dxEMF.Core.Loader,
  dxEMF.Core.Helpers;

type

  TdxEMFCustomSession = class;
  TdxEMFCustomDataProvider = class;
  TdxEMFCollectionHelper = class;

  TdxObjectCreatingEvent = procedure(Sender: TObject; AClass: TClass; var AObject: TObject) of object;
  TdxObjectNotifyEvent = procedure(Sender: TObject; AObject: TObject) of object;

{$IFDEF CXTTEST}
  { TdxPersistentObject }

  IdxPersistentObject = interface
  ['{789C8EE3-AADB-4A97-B59C-587FC67B45DC}']
    procedure DoChange; overload;
    procedure DoChange(const AMemberName: string); overload;
    procedure DoChange(const AMemberName: string; const AOldValue, ANewValue: TValue); overload;
  end;

{$ENDIF}

  { TdxEMFCollections }

  TdxEMFCollections = class sealed
  public
    class function Create<T: class>: IdxEMFCollection<T>; overload; static;
    class function Create<T: class>(AObject: TObject; const APropertyName: string = ''): IdxEMFCollection<T>; overload; static;
    class function Create(AClass: TClass): IdxEMFCollection; overload; static;
    class procedure Initialize(AObject: TObject); static;
  end;

  { TdxLoadObjectsContext }

  TdxLoadObjectsContext = class
  strict private
    FQueries: TArray<TdxObjectsQuery>;
    FResult: TArray<TValue>;
    FExpanded: TArray<TdxExpandedCriteriaHolder>;
    FRequiresResorting: TArray<Boolean>;
    FLoadDataQuery: TArray<TdxObjectsQuery>;
  public
    constructor Create(const AQueries: TArray<TdxObjectsQuery>; const AResult: TArray<TValue>;
      const AExpanded: TArray<TdxExpandedCriteriaHolder>; const ARequiresResorting: TArray<Boolean>;
      const ALoadDataQuery: TArray<TdxObjectsQuery>); overload;
    constructor Create(const AQueries: TArray<TdxObjectsQuery>; const AResult: TArray<TValue>;
      const AExpanded: TArray<TdxExpandedCriteriaHolder>; const ARequiresResorting: TArray<Boolean>); overload;
    destructor Destroy; override;

    property Queries: TArray<TdxObjectsQuery> read FQueries;
    property Result: TArray<TValue> read FResult;
    property Expanded: TArray<TdxExpandedCriteriaHolder> read FExpanded;
    property RequiresResorting: TArray<Boolean> read FRequiresResorting;
    property LoadDataQuery: TArray<TdxObjectsQuery> read FLoadDataQuery;
  end;

  { TdxEMFCustomCollection }

  TdxEMFCustomCollection = class(TInterfacedObject, IEnumerable, IdxQueryable)
  protected type
    TCollectionProperty = (
      DeleteObjectOnRemove,
      // reserved for future use
      CaseSensitive,
      SelectDeleted,
      LoadingEnabled,
      // internal
      IsEntity,
      IsPersistentClass,
      IsReadOnly,
      IsLoading,
      IsLoaded
    );
    TCollectionProperties = set of TCollectionProperty;
  strict private
    FSession: TdxEMFCustomSession;
    FHelper: TdxEMFCollectionHelper;
    FLoader: TdxDataLoader;
    FCollectionProperties: TCollectionProperties;
    FCriteria: IdxCriteriaOperator;
    FSelectDeleted: Boolean;
    FTopReturnedObjects: Integer;
    FSkipReturnedObjects: Integer;
    procedure SetLoader(Value: TdxDataLoader);
    procedure SetCriteria(const Value: IdxCriteriaOperator);
  protected
    procedure Clear; virtual; abstract;
    procedure DetachSession; virtual;
    procedure FetchAll; virtual; abstract;
    function InternalGetEnumerator: IEnumerator; virtual; abstract;
    function LoadNextObject: Boolean; virtual; abstract;
    function GetCollectionProperties(const Index: TCollectionProperty): Boolean;
    procedure SetCollectionProperties(const Index: TCollectionProperty; const Value: Boolean);
    function CreateLoader(AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate = nil): TdxDataLoader; overload;
    function GetCollectionElementClass: TClass; virtual; abstract;
    property Helper: TdxEMFCollectionHelper read FHelper write FHelper;
    { IdxQueryable }
    function GetProvider: IdxQueryProvider;
    { IEnumerable }
    function GetEnumerator: IEnumerator;

    property CollectionProperties: TCollectionProperties read FCollectionProperties;
    property IsReadOnly: Boolean index TCollectionProperty.IsReadOnly read GetCollectionProperties write SetCollectionProperties;
    property IsLoaded: Boolean index TCollectionProperty.IsLoaded read GetCollectionProperties write SetCollectionProperties;
    property IsLoading: Boolean index TCollectionProperty.IsLoading read GetCollectionProperties write SetCollectionProperties;
    property LoadingEnabled: Boolean index TCollectionProperty.LoadingEnabled read GetCollectionProperties write SetCollectionProperties;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(ASession: TdxEMFCustomSession); overload; virtual;
    procedure Init(ASession: TdxEMFCustomSession; AOwner: TObject; AReferencedProperty: TdxMappingMemberInfo); overload; virtual; abstract;
    procedure Insert(AIndex: Integer; AObject: TObject); virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Reload;
    function ToArray: TArray<TObject>;
    property Session: TdxEMFCustomSession read FSession;
    property CollectionElementClass: TClass read GetCollectionElementClass;
    property Criteria: IdxCriteriaOperator read FCriteria write SetCriteria;
    property Loader: TdxDataLoader read FLoader write SetLoader;
    property SkipReturnedObjects: Integer read FSkipReturnedObjects write FSkipReturnedObjects;
    property TopReturnedObjects: Integer read FTopReturnedObjects write FTopReturnedObjects;
    property SelectDeleted: Boolean read FSelectDeleted write FSelectDeleted;
  end;

  { TdxEMFCollectionHelper }

  TdxEMFCollectionHelper = class
  strict private
    FParentCollection: TdxEMFCustomCollection;
    FEntityInfo: TdxEntityInfo;
    function GetSession: TdxEMFCustomSession;
    function GetLoadCollectionOnModify: Boolean; inline;
  protected
    function GetLoadCollectionOnModifyCore: Boolean; virtual;
    procedure BeforeAfterRemove(AObject: TObject); virtual;
    property ParentCollection: TdxEMFCustomCollection read FParentCollection;
  public
    constructor Create(AOwner: TdxEMFCustomCollection);
    procedure Add(ANewObject: TObject); virtual;
    procedure Remove(AObject: TObject); virtual;
    procedure Reload; virtual;
    function PatchCriteriaFromUserToFetch(const AFetchCriteria: IdxCriteriaOperator): IdxCriteriaOperator; virtual;
    function GetHardcodedCriterion: IdxCriteriaOperator; virtual;
    property LoadCollectionOnModify: Boolean read GetLoadCollectionOnModify;
    property EntityInfo: TdxEntityInfo read FEntityInfo write FEntityInfo;
    property Session: TdxEMFCustomSession read GetSession;
  end;

  { TdxReferencedCollectionHelper }

  TdxReferencedCollectionHelper = class abstract(TdxEMFCollectionHelper)
  strict private
    FOwnerObject: TObject;
    FReferencedProperty: TdxMappingMemberInfo;
  protected
    property OwnerObject: TObject read FOwnerObject;
    property ReferencedProperty: TdxMappingMemberInfo read FReferencedProperty;
  public
    constructor Create(AParentCollection: TdxEMFCustomCollection; AOwnerObject: TObject; AReferencedProperty: TdxMappingMemberInfo);
    procedure ClearChangesCache; virtual; abstract;
    procedure Save; virtual; abstract;
  end;

  { TdxDataSetLoader }

  TdxDataSetLoader = class(TdxDataLoader)
  strict private
    FFieldDefs: TFieldDefs;
    FDataSet: TDataSet;
    FOwnDataSet: Boolean;
    procedure CheckActive; inline;
  protected
    function GetEof: boolean; override;
    function GetFieldCount: Integer; override;
    function GetFieldNames(AIndex: Integer): string; override;
    function GetFieldDef(AIndex: Integer): TdxDataLoaderFieldDef; override;
    function GetRecordCount: Integer; override;
    function MoveNext: Boolean; override;
    procedure InternalSetDataSet(const Value: TDataSet); virtual;
    procedure BeginLoad; override;
    function HasSkipTopRecords: Boolean; virtual;
    procedure SkipTopRecords; virtual;
    property OwnDataSet: Boolean read FOwnDataSet write FOwnDataSet;
    property FieldDefs: TFieldDefs read FFieldDefs;
  public
    constructor Create(const ASession: IdxSession; ADataSet: TDataSet); overload;
    destructor Destroy; override;
    property DataSet: TDataSet read FDataSet write InternalSetDataSet;
  end;

  { TdxDataSetLoader<T, F> }

  TdxDataSetLoader<T: TDataSet; F> = class(TdxDataSetLoader)
  protected type
    TFieldCache = TPair<TdxMappingMemberInfo, TArray<F>>;
  private
    FFieldsCache: TArray<TFieldCache>;
    FKeyFieldsCache: TArray<TFieldCache>;
    function GetDataSet: T; inline;
    procedure SetDataSet(const Value: T);
  protected type
    TProc<TC: class> = reference to procedure(const ADBFields: TArray<F>; AObject: TC);
    TProc = reference to procedure(const ADBFields: TArray<F>; AObject: TObject);
  protected
    class var FReaderFunctions: TDictionary<TClass, TProc>;
    class procedure RegisterReader<TC: class>(AProc: TProc<TC>);
  protected
    procedure BeginLoad; override;
    procedure CreateFieldsCache; virtual; abstract;
    property FieldsCache: TArray<TFieldCache> read FFieldsCache;
    property KeyFieldsCache: TArray<TFieldCache> read FKeyFieldsCache;
  public
    property DataSet: T read GetDataSet write SetDataSet;
  end;

  { TdxDataSetLoader<T> }

  TdxDataSetLoader<T: TDataSet> = class(TdxDataSetLoader<T, TField>)
  strict private
    function GetFields(AMemberInfo: TdxMappingMemberInfo; var AFieldIndex: Integer): TArray<TField>;
  protected
    procedure CreateFieldsCache; override;
    function ReadCurrentRecordKeys: TArray<TValue>; override;
    procedure ReadCurrentRecord(AInstance: Pointer); override;
    procedure ReadCurrentRecord(out AValues: TArray<Variant>); override;
    procedure ReadCurrentRecord(out AValues: TArray<TValue>); override;
  end;

  { TdxEMFCollectionOptions }

  TdxEMFCollectionOptions = class(TPersistent)
  strict private
    FLoadingStrategy: TdxLoadingStrategy;
    procedure SetLoadingStrategy(const Value: TdxLoadingStrategy);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property LoadingStrategy: TdxLoadingStrategy read FLoadingStrategy write SetLoadingStrategy default TdxLoadingStrategy.Lazy;
  end;

  { TdxEMFSessionOptions }

  TdxEMFSessionOptions = class(TPersistent)
  strict private
    FCollections: TdxEMFCollectionOptions;
    FLockingOption: TdxLockingOption;
    procedure SetCollections(const Value: TdxEMFCollectionOptions);
  protected
    property LockingOption: TdxLockingOption read FLockingOption write FLockingOption default TdxLockingOption.None;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Collections: TdxEMFCollectionOptions read FCollections write SetCollections;
  end;

  { TdxEMFCustomSession }

  TdxEMFCustomSession = class(TComponent, IdxPersistentValueExtractor, IdxQueryProvider, IdxDataContext, IdxSession)
  strict private type

    TObjectsByKeyQuery = record
    strict private
      FEntityInfo: TdxEntityInfo;
      FKeys: TArray<TValue>;
      function GetCount: Integer; inline;
    public
      constructor Create(AEntityInfo: TdxEntityInfo; const AKey: TValue); overload;
      constructor Create(AEntityInfo: TdxEntityInfo; const AKeys: array of TValue); overload;

      property Count: Integer read GetCount;
      property EntityInfo: TdxEntityInfo read FEntityInfo;
      property Keys: TArray<TValue> read FKeys;
    end;

    TCommitChangesContext = class
    strict private
      FSession: TdxEMFCustomSession;
      FBatchWideData: TdxBatchWideDataHolderForModification;
      FProcessingSave: TdxProcessingSave;
    protected
      Statements: TArray<TdxModificationStatement>;
      Result: TdxModificationResult;
      procedure FreeStatements;
      procedure FreeResult;
      property ProcessingSave: TdxProcessingSave read FProcessingSave;
    public
      constructor Create(ASession: TdxEMFCustomSession; ABatchWideData: TdxBatchWideDataHolderForModification);
      destructor Destroy; override;

      property Session: TdxEMFCustomSession read FSession;
      property BatchWideData: TdxBatchWideDataHolderForModification read FBatchWideData;
    end;

    TQueryProvider<T: class> = class(TInterfacedObject, IdxQueryProvider<T>)
    private
      FSession: TdxEMFCustomSession;
    protected
      function CreateQuery(const AExpression: IdxExpression): IdxQueryable;
      function CreateQueryGeneric(const AExpression: IdxExpression): IdxQueryable<T>;
      function IdxQueryProvider<T>.CreateQuery = CreateQueryGeneric;
    public
      constructor Create(ASession: TdxEMFCustomSession);
    end;

  strict private
    FPoolClasses: TdxEntityPoolClasses;
    FOptions: TdxEMFSessionOptions;
    FDataProvider: TdxEMFCustomDataProvider;
    FLoadingObjects: TdxObjectSet;
    FObjectsMarkedSaved: TdxObjectSet;
    FObjectsMarkedDeleted: TdxObjectSet;
    FObjectsMarkedInserted: TdxObjectSet;
    FCollectionsMarkedSaved: TDictionary<TdxReferencedCollectionHelper, TObject>;
    FTrackingChanges: Boolean;
    FLinkedCollections: TDictionary<TdxEMFCustomCollection, Byte>;

    FOnCreateFunc: TDictionary<TClass, TFunc<TObject>>;
    FOnObjectCreating: TdxObjectCreatingEvent;
    FOnObjectDeleting: TdxObjectNotifyEvent;
    FOnAfterConnect: TNotifyEvent;
    FOnAfterBeginTrackingChanges: TNotifyEvent;
    FOnAfterDropChanges: TNotifyEvent;
    FOnObjectDeleted: TdxObjectNotifyEvent;
    FOnAfterFlushChanges: TNotifyEvent;
    FOnObjectSaving: TdxObjectNotifyEvent;
    FOnObjectLoading: TdxObjectNotifyEvent;
    FOnBeforeConnect: TNotifyEvent;
    FOnBeforeBeginTrackingChanges: TNotifyEvent;
    FOnBeforeDropChanges: TNotifyEvent;
    FOnObjectSaved: TdxObjectNotifyEvent;
    FOnObjectLoaded: TdxObjectNotifyEvent;
    FOnBeforeFlushChanges: TNotifyEvent;
    procedure FreeMarkedInsertedObjects;
    function GetUseCache: Boolean; inline;
    function GetQueryForFindObject(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
      ASelectDeleted: Boolean): TdxObjectsQuery;
    function GetQueriesByKeys(const AGetQuery: TObjectsByKeyQuery): TObjectList<TdxObjectsQuery>;
    function FilterListForDelete(const AFullListForDelete: TArray<TObject>): TArray<TObject>;
    procedure PreProcessSavedList;
    procedure ProcessDeletedObjects(AContext: TCommitChangesContext;
      const AFullListForDelete, ACompleteListForSave: TArray<TObject>;
      out ABeforeInserts, AAfterUpdates: TList<TdxModificationStatement>);
    procedure ProcessingProcessNextObject(AMarkedObjectsHolder: TdxObjectSet; AObject: TObject);
    procedure ProcessingProcessObjectListOrCollection(AMarkedObjectsHolder: TdxObjectSet; AObjectOrCollection: TObject);
    procedure InternalFlushChanges;
    procedure SetOptions(const Value: TdxEMFSessionOptions);
    procedure TriggerObjectsSaved(const AObjects: TArray<TObject>);

    function SelectDataInternal(AObjectsQuery: TdxObjectsQuery; const AProperties, AGroupProperties: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator): TdxQueryData;

    class function CollectClassInfosFormObjects(const AObjects: TArray<TObject>): TDictionary<TdxEntityInfo, TdxEntityInfo>; static;
    class procedure CheckFilteredAggregateDeletion(AObject: TObject; AMemberInfo: TdxMappingMemberInfo; ACollection: TdxEMFCustomCollection); static;
  protected
    FStateStack: TdxSessionStates;
    procedure AppendObject(AObject: TObject); overload;
    procedure AppendObject(const AKeys: TArray<TValue>; AObject: TObject); overload;
    procedure BeginCommitChangesInsideTransaction(AContext: TCommitChangesContext;
      const AFullListForDelete, ACompleteListForSave: TArray<TObject>);
    function BeginFlushChanges: TArray<TObject>; virtual;
    procedure BeginSelect; inline;
    function CreateLoader(AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate = nil): TdxDataLoader;
    procedure CheckConnection;
    procedure CheckDuplicateObjectInIdentityMap(AObject: TObject);
    procedure EndSelect; inline;
    procedure EndFlushChanges(const AObjectsToFireSaved: TArray<TObject>);
    procedure EndCommitChangesInsideTransaction(AContext: TCommitChangesContext);
    procedure CommitChanges(const AListForDelete, AListForSave: TArray<TObject>);
    procedure DeleteObject(AObject: TObject);
    procedure DeleteCore(AEntityInfo: TdxEntityInfo; AObject: TObject);
    procedure SetKeyValue(AObject: TObject; const AKeyValue: TValue); overload;
    procedure SetKeyValue(AObject: TObject; const AKeyValue: TArray<TValue>); overload;

    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    procedure DoCreateObject(AClass: TClass; out AObject: TObject);
    procedure DoAfterFlushChanges;
    procedure DoBeforeFlushChanges;
    procedure DoObjectDeleting(AObject: TObject);
    procedure DoObjectDeleted(AObject: TObject);
    procedure DoBeforeDropChanges;
    procedure DoAfterDropChanges;
    procedure DoBeforeBeginTrackingChanges;
    procedure DoAfterBeginTrackingChanges;
    procedure DoBeforeConnect;
    procedure DoAfterConnect;
    procedure DoObjectSaving;
    procedure DoObjectSaved;
    procedure DoObjectLoading;
    procedure DoObjectLoaded;

    function GetObjectsInternal(const AQueries: TArray<TdxObjectsQuery>): TArray<IdxEMFCollection>; virtual;
    function GetKeyValue(AObject: TObject): TValue; inline;
    class function GetHash(AObject: TObject): Cardinal; static;
    procedure SetDataProvider(Value: TdxEMFCustomDataProvider); virtual;
    function GetDataProvider: TdxEMFCustomDataProvider;
    function FindObjectInCache(AClass: TClass; const AKeys: TArray<TValue>): TObject;
    function PrepareLoadObjects(const AQueries: TArray<TdxObjectsQuery>): TdxLoadObjectsContext;
    procedure ProcessingProcess(AMarkedObjectsHolder: TdxObjectSet; AObject: TObject);
    procedure RegisterInsertedObject(AObject: TObject);
    procedure RemoveFromDeleteList(AObject: TObject); inline;
    procedure RemoveFromSaveList(AObject: TObject); inline;
    procedure RemoveFromLists(AObject: TObject);
    procedure UnregisterObject(AObject: TObject);
    procedure RegisterCollection(ACollection: TdxEMFCustomCollection);
    procedure UnregisterCollection(ACollection: TdxEMFCustomCollection);
    procedure DetachLinkedCollections;
    function IsUnitOfWork: Boolean; virtual;

    function GetObjectsToSave(AIncludeParent: Boolean): TArray<TObject>; overload; virtual;
    function GetObjectsToDelete(AIncludeParent: Boolean): TArray<TObject>; overload; virtual;
    function Find(AClass: TClass; const AKeys: TArray<TValue>): TObject; overload;

    class function GetPFEntityInfo(AClass: TClass): TdxEntityInfo; overload; inline;
    class function GetPFEntityInfo(AObject: TObject): TdxEntityInfo; overload; static;
    function GetLoadedObjectByKey(AClass: TClass; const AKey: TValue): TObject; overload;
    function GetLoadedObjectByKey(AClass: TClass; const AKeys: TArray<TValue>): TObject; overload;
    function GetLoadedObjectByKey<T: class>(const AKey: TValue): T; overload;
    function GetLoadedObjectByKey<T: class>(const AKeys: TArray<TValue>): T; overload;

    function GetObjects<T: class>(const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer;
      ASelectDeleted: Boolean; AForceReload: Boolean = False): IdxEMFCollection<T>; overload;
    function GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer;
      ASelectDeleted: Boolean; AForceReload: Boolean = False): IdxEMFCollection; overload;

    function SelectData(AEntityInfo: TdxEntityInfo; const AProperties: IdxCriteriaOperatorCollection;
      const ACriteria: IdxCriteriaOperator;
      const AGroupProperties: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator;
      ASelectDeleted: Boolean;
      ASkipSelectedRecords, ATopSelectedRecords: Integer;
      const ASortByExpressions: IdxSortByExpressions): TdxQueryData;
    function GetProjections(AClass: TClass; const AProperties: IdxCriteriaOperatorCollection;
      const ACriteria: IdxCriteriaOperator;
      const AGroupProperties: IdxCriteriaOperatorCollection;
      const AGroupCriteria: IdxCriteriaOperator;
      ASelectDeleted: Boolean;
      ASkipSelectedRecords, ATopSelectedRecords: Integer;
      const ASortByExpressions: IdxSortByExpressions): IdxEMFCollection; overload;


    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetLoadingStrategy: TdxLoadingStrategy;
    function GetCaseSensitive: Boolean;
    function ExtractPersistentValue(const ACriterionValue: TValue): TValue;
    function CreateQuery(const AExpression: IdxExpression): IdxQueryable;
    function GetSession: IdxSession;
    function EntityByName(const AName: string): IdxEntityInfo;

    property LoadingObjects: TdxObjectSet read FLoadingObjects;
    property ObjectsMarkedSaved: TdxObjectSet read FObjectsMarkedSaved;
    property ObjectsMarkedDeleted: TdxObjectSet read FObjectsMarkedDeleted;
    property PoolClasses: TdxEntityPoolClasses read FPoolClasses;
    property TrackingChanges: Boolean read FTrackingChanges;
    property UseCache: Boolean read GetUseCache;

    property DataProvider: TdxEMFCustomDataProvider read FDataProvider write SetDataProvider;

    property OnAfterBeginTrackingChanges: TNotifyEvent read FOnAfterBeginTrackingChanges write FOnAfterBeginTrackingChanges;
    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnAfterDropChanges: TNotifyEvent read FOnAfterDropChanges write FOnAfterDropChanges;
    property OnAfterFlushChanges: TNotifyEvent read FOnAfterFlushChanges write FOnAfterFlushChanges;
    property OnBeforeBeginTrackingChanges: TNotifyEvent read FOnBeforeBeginTrackingChanges write FOnBeforeBeginTrackingChanges;
    property OnBeforeConnect: TNotifyEvent read FOnBeforeConnect write FOnBeforeConnect;
    property OnBeforeDropChanges: TNotifyEvent read FOnBeforeDropChanges write FOnBeforeDropChanges;
    property OnBeforeFlushChanges: TNotifyEvent read FOnBeforeFlushChanges write FOnBeforeFlushChanges;
    property OnObjectCreating: TdxObjectCreatingEvent read FOnObjectCreating write FOnObjectCreating;
    property OnObjectDeleted: TdxObjectNotifyEvent read FOnObjectDeleted write FOnObjectDeleted;
    property OnObjectDeleting: TdxObjectNotifyEvent read FOnObjectDeleting write FOnObjectDeleting;
    property OnObjectLoaded: TdxObjectNotifyEvent read FOnObjectLoaded write FOnObjectLoaded;
    property OnObjectLoading: TdxObjectNotifyEvent read FOnObjectLoading write FOnObjectLoading;
    property OnObjectSaved: TdxObjectNotifyEvent read FOnObjectSaved write FOnObjectSaved;
    property OnObjectSaving: TdxObjectNotifyEvent read FOnObjectSaving write FOnObjectSaving;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetObjectsToSave: TArray<TObject>; overload; inline;
    function GetObjectsToDelete: TArray<TObject>; overload; inline;

    procedure BeginTrackingChanges;
    procedure DropChanges;
    procedure FlushChanges;
    procedure Delete(AObject: TObject); virtual;
    procedure Save(AObject: TObject); virtual;
    function IsObjectToDelete(AObject: TObject): Boolean;
    function IsNewObject(AObject: TObject): Boolean;
    function CustomCreate<T: class>(AFunc: TFunc<T>): TdxEMFCustomSession;

    procedure Attach(AObject: TObject);
    procedure Detach(AObject: TObject);
    procedure Reload(AObject: TObject);

    function CreateObject(AClass: TClass): TObject; virtual;

    function Find<T: class>(const AKey: TValue): T; overload;
    function Find<T: class>(APredicate: TdxPredicate<T>): T; overload;
    function Find<T: class>(const ACriteria: IdxCriteriaOperator): T; overload;
    function Find(AClass: TClass; const AKey: TValue): TObject; overload;
    function Find(AClass: TClass; APredicate: TdxPredicate): TObject; overload;
    function Find(AClass: TClass; const ACriteria: IdxCriteriaOperator): TObject; overload;
    function GetObjects<T: class>: IdxEMFCollection<T>; overload;
    function GetObjects<T: class>(APredicate: TdxPredicate<T>): IdxEMFCollection<T>; overload;
    function GetObjects<T: class>(const ACriteria: IdxCriteriaOperator): IdxEMFCollection<T>; overload;
    function GetObjects<T: class>(const AQuery: IdxQueryable<T>): IdxEMFCollection<T>; overload;
    function GetObjects<T: class>(const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ASkipSelectedRecords: Integer = 0; ATopSelectedRecords: Integer = 0): IdxEMFCollection<T>; overload;

    function GetObjects(AClass: TClass): IdxEMFCollection; overload;
    function GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator): IdxEMFCollection; overload;
    function GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
      ASkipSelectedRecords: Integer = 0; ATopSelectedRecords: Integer = 0): IdxEMFCollection; overload;
    function GetObjects(const AQuery: IdxQueryable): IdxEMFCollection; overload;

    function GetEntityInfo(AClass: TClass): IdxEntityInfo; overload;
    function GetEntityInfo<T: IdxEntityInfo>: T; overload;
    function GetDataContext: IdxDataContext; overload;
    function GetDataContext<T: IdxDataContext>: T; overload;
    function GetQueryProvider<T: class>: IdxQueryProvider<T>;

    property Options: TdxEMFSessionOptions read FOptions write SetOptions;
  end;

  { TdxEMFDataProviderOptions }

  TdxEMFDataProviderOptions = class(TPersistent)
  strict private
    FOwner: TdxEMFCustomDataProvider;
    FAutoCreate: TdxAutoCreateOption;
    FAutoDetectDBEngine: Boolean;
    FProviderSQLClass: TdxSQLConnectionProviderClass;
    function GetDBEngine: TdxDBEngine;
    procedure SetAutoCreate(const Value: TdxAutoCreateOption);
    procedure SetAutoDetectDBEngine(const Value: Boolean);
    procedure SetProviderSQLClass(const Value: TdxSQLConnectionProviderClass);
    procedure SetDBEngine(const Value: TdxDBEngine);
  protected
    procedure Changed; virtual;
    property AutoDetectDBEngine: Boolean read FAutoDetectDBEngine write SetAutoDetectDBEngine default True;
    property ProviderSQLClass: TdxSQLConnectionProviderClass read FProviderSQLClass write SetProviderSQLClass;
  public
    constructor Create(AOwner: TdxEMFCustomDataProvider);
    procedure Assign(Source: TPersistent); override;
  published
    property AutoCreate: TdxAutoCreateOption read FAutoCreate write SetAutoCreate default TdxAutoCreateOption.None;
    property DBEngine: TdxDBEngine read GetDBEngine write SetDBEngine;
  end;

  { TdxEMFCustomDataProvider }

  TdxEMFCustomDataProvider = class(TComponent)
  strict private type

    TEntityDBModel = class
    strict private
      FDBTables: TList<TdxDBTable>;
      FPersistentClass: TClass;
      FEntityInfo: TdxEntityInfo;
      procedure InitScheme;
    public
      constructor Create(AClass: TClass);
      destructor Destroy; override;
      property DBTables: TList<TdxDBTable> read FDBTables;
      property EntityInfo: TdxEntityInfo read FEntityInfo;
    end;

    TTableList = class(TObjectList<TdxDBTable>)
    protected
      FRefCount: Integer;
      procedure AddUnique(AList: TList<TdxDBTable>);
      function GetTableByName(const ATableName: string): TdxDBTable;
    end;

  strict private
    FConnection: TCustomConnection;
    FConnectionVendor: TdxCustomConnectionVendor;
    FSQLConnectionProvider: TdxSQLConnectionProvider;
    FOptions: TdxEMFDataProviderOptions;
    FTableList: TTableList;
    FEntityDBModels: TObjectDictionary<TClass, TEntityDBModel>;
    FQueryDataPool: TdxQueryDataPool;
    procedure SetConnection(const Value: TCustomConnection);
    procedure SetOptions(const Value: TdxEMFDataProviderOptions);
  protected
    procedure BeginCreateTable;
    procedure EndCreateTable;
    function CreateConnectionVendor: TdxCustomConnectionVendor; virtual; abstract;
    procedure CheckConnectAndCreate; virtual; abstract;
    function CreateProviderSQL(const ADBEngineType: TdxDBEngine): TdxSQLConnectionProvider; virtual;
    procedure CreateTable(const ATableName: string; AEntityInfo: TdxEntityInfo); virtual;
    procedure UpdateSchema(AEntityInfo: TdxEntityInfo); virtual;
    function GetCanCreateDatabase: Boolean;
    function GetCanCreateSchema: Boolean;

    procedure CheckProvider; virtual;
    function GetActualDBEngineType: TdxDBEngine;
    procedure RecreateProviderSQL;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function ModifyData(const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult; overload;
    function ModifyData(const ADMLStatements: array of TdxModificationStatement): TdxModificationResult; overload;

    function CreateLoader(ASession: TdxEMFCustomSession; AClass: TClass; APredicate: TdxPredicate = nil): TdxDataLoader; overload;
    function CreateLoader(ASession: TdxEMFCustomSession; AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate = nil): TdxDataLoader; overload;
    function CreateLoader(ASession: TdxEMFCustomSession; AConnectionVendor: TdxCustomConnectionVendor; AQuery: TdxQuery;
      AEntityInfo: TdxEntityInfo; APredicate: TdxPredicate = nil): TdxDataLoader; overload; virtual; abstract;

    function GetQueryData(AEntityInfo: TdxEntityInfo): TdxQueryData;
    function GetEntityDBModel(AClass: TClass): TEntityDBModel;
    function GetDBSchema: TObjectList<TdxDBTable>;
    function DoCreateLoader(ASession: TdxEMFCustomSession; AEntityInfo: TdxEntityInfo; APredicate: TdxPredicate = nil): TdxDataLoader; overload;
    function DoCreateLoader(ASession: TdxEMFCustomSession; AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate = nil): TdxDataLoader; overload;
    procedure GetTableNames(AList: TStrings; AIncludeViews: Boolean = False); virtual;

    property Connection: TCustomConnection read FConnection write SetConnection;
    property ConnectionVendor: TdxCustomConnectionVendor read FConnectionVendor;
    property EntityDBModels: TObjectDictionary<TClass, TEntityDBModel> read FEntityDBModels;
    property SQLConnectionProvider: TdxSQLConnectionProvider read FSQLConnectionProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActualDBEngineType: TdxDBEngine read GetActualDBEngineType;
    property CanCreateDatabase: Boolean read GetCanCreateDatabase;
    property CanCreateSchema: Boolean read GetCanCreateSchema;
  published
    property Options: TdxEMFDataProviderOptions read FOptions write SetOptions;
  end;

  { TdxEMFSession }

  TdxEMFSession = class(TdxEMFCustomSession)
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateSchema(AClass: TClass); overload;
    procedure CreateSchema(const AClasses: array of TClass); overload;

  published
    property DataProvider;
    property Options;
  end;


implementation

uses
  Math, Variants, dxEMF.Core.Collections, dxEMF.Utils.Exceptions, dxCore,
  dxEMF.Strs;

type
  TdxCustomConnectionVendorAccess = class(TdxCustomConnectionVendor);

 { TDataSetInfoHelper }

  TDataSetInfoHelper = class helper for TDataSet
  public
    function GetFieldDefs: TFieldDefs;
  end;

 { TdxEntity }

  TdxEntity = class
  public
    class procedure SetSession(ASession: TdxEMFCustomSession; AObject: TObject); static;
    class procedure Initialize(AObject: TObject); static;
    class procedure Finalize(AObject: TObject); static;
  end;

 { TDataSetInfoHelper }

function TDataSetInfoHelper.GetFieldDefs: TFieldDefs;
var
  I: Integer;
  AFieldDef: TFieldDef;
begin
  OpenCursor(True);
  try
    Result := TFieldDefs.Create(Self);
    for I := 0 to FieldDefs.Count - 1 do
    begin
      AFieldDef := Result.AddFieldDef;
      AFieldDef.Assign(FieldDefs[I]);
    end;
  finally
    CloseCursor;
  end;
end;

{ TdxEntity }

class procedure TdxEntity.SetSession(ASession: TdxEMFCustomSession; AObject: TObject);
var
  AEntityInfo: TdxEntityInfo;
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  AEntityInfo := TdxEMFCustomSession.GetPFEntityInfo(AObject);
  AEntityInfo.InitSession(ASession, AObject);
end;

class procedure TdxEntity.Finalize(AObject: TObject);
begin

end;

class procedure TdxEntity.Initialize(AObject: TObject);
begin
  TdxEMFCollections.Initialize(AObject);
end;


{ TdxCollections<T> }


class function TdxEMFCollections.Create<T>: IdxEMFCollection<T>;
begin
  Result := TdxEMFEntityCollection<T>.Create;
end;

class function TdxEMFCollections.Create<T>(AObject: TObject; const APropertyName: string): IdxEMFCollection<T>;
var
  ACollection: TdxEMFEntityCollection<T>;
  AEntityInfo: TdxEntityInfo;
begin
  ACollection := TdxEMFEntityCollection<T>.Create;
  try
    AEntityInfo := TdxEMFCustomSession.GetPFEntityInfo(AObject);
    AEntityInfo.InitAssociation(nil, AObject, ACollection, APropertyName);
    Result := ACollection;
    ACollection := nil;
  finally
    ACollection.Free;
  end;
end;

class function TdxEMFCollections.Create(AClass: TClass): IdxEMFCollection;
var
  ACollection: TdxEMFObjectCollection;
begin
  ACollection := TdxEMFObjectCollection.Create;
  ACollection.CollectionElementClass := AClass;
  Result := ACollection;
end;

class procedure TdxEMFCollections.Initialize(AObject: TObject);
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := TdxEMFCustomSession.GetPFEntityInfo(AObject);
  AEntityInfo.InitAssociations(nil, AObject);
end;

{ TdxEMFCustomCollection }

destructor TdxEMFCustomCollection.Destroy;
begin
  if FSession <> nil then
  begin
    FSession.UnregisterCollection(Self);
    FSession := nil;
  end;
  FreeAndNil(FHelper);
  FreeAndNil(FLoader);
  inherited Destroy;
end;

procedure TdxEMFCustomCollection.DetachSession;
begin
  FSession := nil;
  if FLoader <> nil then
    FLoader.Session := nil;
end;

constructor TdxEMFCustomCollection.Create;
begin
  inherited Create;
  FCollectionProperties := [TCollectionProperty.LoadingEnabled];
end;

function TdxEMFCustomCollection.CreateLoader(AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate): TdxDataLoader;
begin
  if Session = nil then
    Exit(nil);
  Result := Session.CreateLoader(AObjectsQuery, APredicate);
end;

procedure TdxEMFCustomCollection.Init(ASession: TdxEMFCustomSession);
begin
  if FSession = ASession then
    Exit;
  FSession := ASession;
  if FSession <> nil then
    FSession.RegisterCollection(Self);
end;

function TdxEMFCustomCollection.GetCollectionProperties(const Index: TCollectionProperty): Boolean;
begin
  Result := Index in FCollectionProperties;
end;

function TdxEMFCustomCollection.GetEnumerator: IEnumerator;
begin
  Result := InternalGetEnumerator;
end;

function TdxEMFCustomCollection.GetProvider: IdxQueryProvider;
begin
  Result := nil;
  Supports(FSession, IdxQueryProvider, Result);
end;

procedure TdxEMFCustomCollection.SetCollectionProperties(const Index: TCollectionProperty; const Value: Boolean);
begin
  if Value then
    Include(FCollectionProperties, Index)
  else
    Exclude(FCollectionProperties, Index);
end;

procedure TdxEMFCustomCollection.SetCriteria(const Value: IdxCriteriaOperator);
begin
  if FCriteria = Value then
    Exit;
  FCriteria := Value;
  Reload;
end;

procedure TdxEMFCustomCollection.SetLoader(Value: TdxDataLoader);
begin
  if FLoader = Value then
    Exit;
  FLoader.Free;
  FLoader := Value;
  SetCollectionProperties(TCollectionProperty.IsLoading, Value <> nil);
  if (FLoader <> nil) and (FLoader.LoadingStrategy = TdxLoadingStrategy.Eager) then
    FetchAll;
end;

function TdxEMFCustomCollection.ToArray: TArray<TObject>;
var
  AResult: TList<TObject>;
  O: TObject;
begin
  AResult := TList<TObject>.Create;
  try
    for O in Self do
      AResult.Add(O);
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

procedure TdxEMFCustomCollection.Reload;
begin
  Clear;
  if Helper <> nil then
    Helper.Reload;
end;

{ TdxEMFCollectionHelper }

constructor TdxEMFCollectionHelper.Create(AOwner: TdxEMFCustomCollection);
begin
  inherited Create;
  FParentCollection := AOwner;
end;

procedure TdxEMFCollectionHelper.Add(ANewObject: TObject);
begin
  if (Session <> nil) and Session.IsNewObject(ANewObject) then
    Session.Save(ANewObject);
end;

procedure TdxEMFCollectionHelper.Remove(AObject: TObject);
begin
end;

procedure TdxEMFCollectionHelper.BeforeAfterRemove(AObject: TObject);
begin
end;

procedure TdxEMFCollectionHelper.Reload;
begin
end;

function TdxEMFCollectionHelper.PatchCriteriaFromUserToFetch(const AFetchCriteria: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := AFetchCriteria;
end;

function TdxEMFCollectionHelper.GetLoadCollectionOnModify: Boolean;
begin
  Result := GetLoadCollectionOnModifyCore or (not ParentCollection.LoadingEnabled);
end;

function TdxEMFCollectionHelper.GetLoadCollectionOnModifyCore: Boolean;
begin
  Result := True;
end;

function TdxEMFCollectionHelper.GetHardcodedCriterion: IdxCriteriaOperator;
begin
  Result := nil;
end;

function TdxEMFCollectionHelper.GetSession: TdxEMFCustomSession;
begin
  Result := FParentCollection.Session;
end;


{ TdxReferencedCollectionHelper }

constructor TdxReferencedCollectionHelper.Create(AParentCollection: TdxEMFCustomCollection; AOwnerObject: TObject;
  AReferencedProperty: TdxMappingMemberInfo);
begin
  inherited Create(AParentCollection);
  FOwnerObject := AOwnerObject;
  FReferencedProperty := AReferencedProperty;
end;


{ TdxLoadObjectsContext }

constructor TdxLoadObjectsContext.Create(const AQueries: TArray<TdxObjectsQuery>; const AResult: TArray<TValue>; const AExpanded: TArray<TdxExpandedCriteriaHolder>; const ARequiresResorting: TArray<Boolean>);
begin
  FQueries := AQueries;
  FResult := AResult;
  FExpanded := AExpanded;
  FRequiresResorting := ARequiresResorting;
end;

constructor TdxLoadObjectsContext.Create(const AQueries: TArray<TdxObjectsQuery>; const AResult: TArray<TValue>; const AExpanded: TArray<TdxExpandedCriteriaHolder>; const ARequiresResorting: TArray<Boolean>; const ALoadDataQuery: TArray<TdxObjectsQuery>);
begin
  Create(AQueries, AResult, AExpanded, ARequiresResorting);
  FLoadDataQuery := ALoadDataQuery;
end;

destructor TdxLoadObjectsContext.Destroy;
begin

  inherited Destroy;
end;

{ TdxDataSetLoader }

constructor TdxDataSetLoader.Create(const ASession: IdxSession; ADataSet: TDataSet);
begin
  Create(ASession);
  DataSet := ADataSet;
  if FDataSet <> nil then
    FOwnDataSet := True;
end;

destructor TdxDataSetLoader.Destroy;
begin
  FreeAndNil(FFieldDefs);
  if FOwnDataSet then
    FreeAndNil(FDataSet)
  else
    if FDataSet <> nil then
      FDataSet.Close;
  inherited Destroy;
end;

procedure TdxDataSetLoader.CheckActive;
begin
  if not FDataSet.Active then
  begin
    BeginLoad;
  end;
end;

procedure TdxDataSetLoader.BeginLoad;
begin
  FDataSet.Open;
  if HasSkipTopRecords then
    SkipTopRecords;
end;



function TdxDataSetLoader.GetEof: boolean;
begin
  CheckActive;
  Result := FDataSet.Eof;
end;

function TdxDataSetLoader.GetFieldCount: Integer;
begin
  if (Query.ConstantValues <> nil) then
    Result := Query.ConstantValues.Count + Query.OperandIndexes.Count
  else
    Result := FFieldDefs.Count;
end;

function TdxDataSetLoader.GetFieldDef(AIndex: Integer): TdxDataLoaderFieldDef;
var
  AFieldDef: TFieldDef;
begin
  if (Query.ConstantValues <> nil) and Query.ConstantValues.ContainsKey(AIndex) then
    Exit(TdxDataLoaderFieldDef.Create(Query.ConstantValues[AIndex].Value))
  else
  begin
    if (Query.OperandIndexes <> nil) then
      AIndex := Query.OperandIndexes[AIndex];
    AFieldDef := FFieldDefs[AIndex];
    Result.DataType := AFieldDef.DataType;
    Result.Precision := AFieldDef.Precision;
    Result.Size := AFieldDef.Size;
  end;
end;

function TdxDataSetLoader.GetFieldNames(AIndex: Integer): string;
begin
//  Result := FDataSet.Fields[AIndex].FieldName;
  if (Query.ConstantValues <> nil) and Query.ConstantValues.ContainsKey(AIndex) then
    Result := Format('Field%d', [AIndex])
  else
  begin
    if (Query.OperandIndexes <> nil) then
      AIndex := Query.OperandIndexes[AIndex];
    Result := FFieldDefs[AIndex].Name;
  end;
end;

function TdxDataSetLoader.GetRecordCount: Integer;
begin
  CheckActive;
  Result := FDataSet.RecordCount;
end;

function TdxDataSetLoader.HasSkipTopRecords: Boolean;
begin
  Result := SkipReturnedObjects <> 0;
end;

procedure TdxDataSetLoader.InternalSetDataSet(const Value: TDataSet);
begin
  if FDataSet = Value then
    Exit;
  FDataSet := Value;
  if FDataSet <> nil then
  begin
    FFieldDefs.Free;
    FFieldDefs := FDataSet.GetFieldDefs;
  end;
end;

function TdxDataSetLoader.MoveNext: Boolean;
begin
  CheckActive;
//  if ReadRecords > 0 then
    FDataSet.Next;
  Result := not FDataSet.Eof;
end;


procedure TdxDataSetLoader.SkipTopRecords;
var
  I: Integer;
begin
  I := 0;
  while not DataSet.Eof and (I < SkipReturnedObjects) do
  begin
    Inc(I);
    DataSet.Next;
  end;
end;

{ TdxEMFCollectionOptions }

procedure TdxEMFCollectionOptions.Assign(Source: TPersistent);
var
  ASource: TdxEMFCollectionOptions;
begin
  if Source is TdxEMFCollectionOptions then
  begin
    ASource := TdxEMFCollectionOptions(Source);
    LoadingStrategy := ASource.LoadingStrategy;
  end
  else
    inherited Assign(Source);
end;

procedure TdxEMFCollectionOptions.SetLoadingStrategy(const Value: TdxLoadingStrategy);
begin
  FLoadingStrategy := Value;
end;

{ TdxEMFSessionOptions }

constructor TdxEMFSessionOptions.Create;
begin
  inherited Create;
  FCollections := TdxEMFCollectionOptions.Create;
end;

destructor TdxEMFSessionOptions.Destroy;
begin
  FreeAndNil(FCollections);
  inherited Destroy;
end;

procedure TdxEMFSessionOptions.Assign(Source: TPersistent);
var
  ASource: TdxEMFSessionOptions;
begin
  if Source is TdxEMFSessionOptions then
  begin
    ASource := TdxEMFSessionOptions(Source);
    Collections := ASource.Collections;
  end
  else
    inherited Assign(Source);
end;

procedure TdxEMFSessionOptions.SetCollections(const Value: TdxEMFCollectionOptions);
begin
  FCollections.Assign(Value);
end;

{ TdxEMFCustomSession.TdxObjectsByKeyQuery }

constructor TdxEMFCustomSession.TObjectsByKeyQuery.Create(AEntityInfo: TdxEntityInfo; const AKey: TValue);
begin
  FEntityInfo := AEntityInfo;
  FKeys := TArray<TValue>.Create(AKey);
end;

constructor TdxEMFCustomSession.TObjectsByKeyQuery.Create(AEntityInfo: TdxEntityInfo; const AKeys: array of TValue);
var
  I: Integer;
begin
  FEntityInfo := AEntityInfo;
  SetLength(FKeys, Length(AKeys));
  for I := 0 to Length(AKeys) - 1 do
    FKeys[I] := AKeys[I];
end;

function TdxEMFCustomSession.TObjectsByKeyQuery.GetCount: Integer;
begin
  Result := Length(FKeys);
end;

{ TdxEMFCustomSession }

constructor TdxEMFCustomSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TdxEMFSessionOptions.Create;
  FPoolClasses := TdxEntityPoolClasses.Create;
  FLoadingObjects := TdxObjectSet.Create;
  FObjectsMarkedSaved := TdxObjectSet.Create;
  FObjectsMarkedDeleted := TdxObjectSet.Create;
  FObjectsMarkedInserted := TdxObjectSet.Create;
  FCollectionsMarkedSaved := TDictionary<TdxReferencedCollectionHelper, TObject>.Create;
  FLinkedCollections := TDictionary<TdxEMFCustomCollection, Byte>.Create;
end;

destructor TdxEMFCustomSession.Destroy;
begin
  FreeMarkedInsertedObjects;
  DetachLinkedCollections;
  FreeAndNil(FLinkedCollections);
  FreeAndNil(FObjectsMarkedInserted);
  FreeAndNil(FPoolClasses);
  FreeAndNil(FOptions);
  FreeAndNil(FLoadingObjects);
  FreeAndNil(FObjectsMarkedSaved);
  FreeAndNil(FObjectsMarkedDeleted);
  FreeAndNil(FCollectionsMarkedSaved);
  inherited Destroy;
end;

procedure TdxEMFCustomSession.Detach(AObject: TObject);
begin
  PoolClasses.ExtractObject(AObject);
end;

procedure TdxEMFCustomSession.DetachLinkedCollections;
var
  I: TPair<TdxEMFCustomCollection, Byte>;
begin
  for I in FLinkedCollections do
    I.Key.DetachSession();
end;

function TdxEMFCustomSession.GetObjectsToSave: TArray<TObject>;
begin
  Result := FObjectsMarkedSaved.ToArray;
end;

function TdxEMFCustomSession.GetObjectsToDelete: TArray<TObject>;
begin
  Result := FObjectsMarkedDeleted.ToArray;
end;

procedure TdxEMFCustomSession.BeginCommitChangesInsideTransaction(AContext: TCommitChangesContext;
  const AFullListForDelete, ACompleteListForSave: TArray<TObject>);
var
  ADeleteFirstList, ADeleteLastList, AInsertUpdateList: TList<TdxModificationStatement>;
  AStatements: TdxArray<TdxModificationStatement>;
  AStatement: TdxModificationStatement;
begin
  ProcessDeletedObjects(AContext, AFullListForDelete, ACompleteListForSave, ADeleteFirstList, ADeleteLastList);
  try
    AInsertUpdateList := AContext.ProcessingSave.Process;
    try
      AStatements := TdxArray<TdxModificationStatement>.Create(ADeleteFirstList.Count + AInsertUpdateList.Count +
        ADeleteLastList.Count);
      AStatements.AddRange(ADeleteFirstList);
      AStatements.AddRange(AInsertUpdateList);
      AStatements.AddRange(ADeleteLastList);
      if (AStatements.Count <> 0) and (AContext.Session.Options.LockingOption = TdxLockingOption.None) then
        for AStatement in AStatements.Value do
          AStatement.AffectedRecordCount := 0;
      AContext.Statements := AStatements.Value;
    finally
      AInsertUpdateList.Free;
    end;
  finally
    ADeleteFirstList.Free;
    ADeleteLastList.Free;
  end;
end;

function TdxEMFCustomSession.BeginFlushChanges: TArray<TObject>;
var
  AObjectsToFireSaved: TList<TObject>;
  AObject: TObject;
begin
  Result := nil;
  TdxSessionStateStack.Enter(Self, TdxSessionState.CommitTransactionNonReentrant);
  try
    PreProcessSavedList;
    DoBeforeFlushChanges;
    AObjectsToFireSaved := TList<TObject>.Create;
    try
      AObjectsToFireSaved.Capacity := FObjectsMarkedSaved.Count;
      for AObject in FObjectsMarkedSaved do
        if not IsObjectToDelete(AObject) then
          AObjectsToFireSaved.Add(AObject);
      Result := AObjectsToFireSaved.ToArray;
    finally
      AObjectsToFireSaved.Free;
    end;
  finally
    TdxSessionStateStack.Leave(Self, TdxSessionState.CommitTransactionNonReentrant);
  end;
end;


procedure TdxEMFCustomSession.BeginSelect;
begin
end;

procedure TdxEMFCustomSession.BeginTrackingChanges;
begin
  DoBeforeBeginTrackingChanges;
  if TrackingChanges then
    raise EdxTransactionSequenceException.Create(sdxSessionTranSequenceBegin);
  TdxSessionStateStack.Enter(Self, TdxSessionState.BeginTransaction);
  try
    Assert(Length(GetObjectsToSave) = 0);
    Assert(Length(GetObjectsToDelete) = 0);
    FTrackingChanges := True;
  finally
    TdxSessionStateStack.Leave(Self, TdxSessionState.BeginTransaction);
  end;
  DoAfterBeginTrackingChanges;
end;

procedure TdxEMFCustomSession.BeginTransaction;
begin
  BeginTrackingChanges;
end;

procedure TdxEMFCustomSession.CheckConnection;
begin
  if DataProvider = nil then
    raise EdxNoSessionConnection.CreateFmt(sdxSessionMissingConnection, [Name]);
  DataProvider.CheckProvider;
  DataProvider.CheckConnectAndCreate;
end;

procedure TdxEMFCustomSession.CheckDuplicateObjectInIdentityMap(AObject: TObject);
var
  APool: TdxCustomEntityPool;
  ACachedObj: TObject;
begin
  APool := FPoolClasses.GetPool(AObject.ClassType);
  if APool = nil then
    Exit;
  ACachedObj := APool.GetByKey(APool.EntityInfo.KeyProperty.GetValues(AObject));
  if (ACachedObj <> nil) and not (ACachedObj = AObject) then
    raise EdxDifferentObjectsWithSameKeyException.Create(sdxSessionDifferentObjectsWithSameKeyException);
end;

class procedure TdxEMFCustomSession.CheckFilteredAggregateDeletion(AObject: TObject; AMemberInfo: TdxMappingMemberInfo;
  ACollection: TdxEMFCustomCollection);
var
  AMessage: string;
  AExceptionWouldBeReallyThrown: Boolean;
  ACauses: TStringBuilder;
  AException: EInvalidOperation;
begin
  if (ACollection.Criteria <> nil) or (ACollection.SkipReturnedObjects > 0) or (ACollection.TopReturnedObjects > 0) then
  begin
    ACauses := TStringBuilder.Create;
    try
      AMessage := Format(sdxCollectionsWantNotDeleteFilteredAggregateCollection, [AObject.ClassName, AMemberInfo.MemberName]);
      AExceptionWouldBeReallyThrown := (TdxAttribute.NoForeignKey in AMemberInfo.AssociatedMember.Attributes);
      if ACollection.Criteria <> nil then
        if AExceptionWouldBeReallyThrown then
          try
            ACauses.Append('.Criteria = '#$27 + TdxCriteriaOperator.ToString(ACollection.Criteria as TdxCriteriaOperator) + #$27);
          except
          end
        else
          ACauses.Append('.Criteria');
      if ACollection.SkipReturnedObjects > 0 then
        ACauses.Append('.SkipReturnedObjects = ').Append(ACollection.SkipReturnedObjects);
      if ACollection.TopReturnedObjects > 0 then
        ACauses.Append('.TopReturnedObjects = ').Append(ACollection.TopReturnedObjects);
      AException := EInvalidOperation.Create(ACauses.ToString);
      if AExceptionWouldBeReallyThrown then
        raise AException
      else
        try
          raise AException;
        except
        end;
    finally
      ACauses.Free;
    end;
  end;
end;

class function TdxEMFCustomSession.CollectClassInfosFormObjects(const AObjects: TArray<TObject>): TDictionary<TdxEntityInfo, TdxEntityInfo>;
var
  AObject: TObject;
  AEntityInfo: TdxEntityInfo;
begin
 Result := TDictionary<TdxEntityInfo, TdxEntityInfo>.Create;
 for AObject in AObjects do
 begin
   AEntityInfo := GetPFEntityInfo(AObject);
   Result.AddOrSetValue(AEntityInfo, AEntityInfo);
 end;
end;

procedure TdxEMFCustomSession.CommitChanges(const AListForDelete, AListForSave: TArray<TObject>);
var
  AReadyListForDelete: TArray<TObject>;
  ABatchWideData: TdxBatchWideDataHolderForModification;
  AContext: TCommitChangesContext;
begin
  AReadyListForDelete := FilterListForDelete(AListForDelete);
  ABatchWideData := TdxBatchWideDataHolderForModification.Create;
  try
    ABatchWideData.RegisterDeletedObjects(AListForDelete);
    AContext := TCommitChangesContext.Create(Self, ABatchWideData);
    try
      BeginCommitChangesInsideTransaction(AContext, AReadyListForDelete, AListForSave);
      if Length(AContext.Statements) <> 0 then
      begin
        AContext.Result := DataProvider.ModifyData(AContext.Statements);
      end;
      EndCommitChangesInsideTransaction(AContext);
    finally
      AContext.Free;
    end;
  finally
    ABatchWideData.Free;
  end;
end;

procedure TdxEMFCustomSession.CommitTransaction;
begin
  FlushChanges;
end;

class function TdxEMFCustomSession.GetPFEntityInfo(AClass: TClass): TdxEntityInfo;
begin
  Result := EntityManager.GetEntityInfo(AClass);
end;

function TdxEMFCustomSession.GetCaseSensitive: Boolean;
begin
  Result := TdxEMFDefault.DefaultCaseSensitive;
end;

class function TdxEMFCustomSession.GetPFEntityInfo(AObject: TObject): TdxEntityInfo;
begin
  if AObject = nil then
    Result := nil
  else
  begin
    Result := GetPFEntityInfo(AObject.ClassType);
    if Result = nil then
      raise EdxNoEntityInfoException.CreateFmt(sdxClassIsNotEntity, [AObject.ClassName]);
  end;
end;

function TdxEMFCustomSession.GetProjections(AClass: TClass; const AProperties: IdxCriteriaOperatorCollection;
  const ACriteria: IdxCriteriaOperator; const AGroupProperties: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator; ASelectDeleted: Boolean; ASkipSelectedRecords,
  ATopSelectedRecords: Integer; const ASortByExpressions: IdxSortByExpressions): IdxEMFCollection;
var
  AEntityInfo: TdxEntityInfo;
  AQueryData: TdxQueryData;
  ASelectStatementResult: TdxSelectStatementResult;
begin
  BeginSelect;
  try
    CheckConnection;
    AEntityInfo := GetPFEntityInfo(AClass);
    AQueryData := SelectData(AEntityInfo, AProperties, ACriteria, AGroupProperties,
      AGroupCriteria, { SelectDeleted } False, ASkipSelectedRecords, ATopSelectedRecords, ASortByExpressions);
    ASelectStatementResult := TdxSelectStatementResult.Create(AQueryData);
    Result := TdxSelectStatementCollections.Create(ASelectStatementResult);
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.GetDataProvider: TdxEMFCustomDataProvider;
begin
  CheckConnection;
  Result := FDataProvider;
end;

function TdxEMFCustomSession.GetDataContext: IdxDataContext;
begin
  Result := TdxLinqExpressionFactory.GetDataContext(Self);
end;

function TdxEMFCustomSession.GetDataContext<T>: T;
var
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := TypeInfo(T);
  Supports(TdxLinqExpressionFactory.GetDataContext(ATypeInfo, Self), ATypeInfo.TypeData.Guid, Result);
end;

function TdxEMFCustomSession.GetEntityInfo(AClass: TClass): IdxEntityInfo;
begin
  Result := TdxLinqExpressionFactory.GetEntityMetadata(AClass, Self);
end;

function TdxEMFCustomSession.GetEntityInfo<T>: T;
var
  AClass: TClass;
begin
  AClass := TdxLinqExpressionFactory.FindClass(TypeInfo(T));
  if AClass = nil then
    raise EdxNoEntityInfoException.CreateFmt(sdxExpressionNotRegistered, [PTypeInfo(TypeInfo(T)).Name]);
  Result := T(TdxLinqExpressionFactory.GetEntityMetadata(AClass, Self));
end;

class function TdxEMFCustomSession.GetHash(AObject: TObject): Cardinal;
var
  AObjectFinder: TdxObjectFinder;
begin
  AObjectFinder := ObjectFinder(AObject.ClassType);
  Result := AObjectFinder.GetHash(AObject);
end;

function TdxEMFCustomSession.GetKeyValue(AObject: TObject): TValue;
begin
  Result := EntityManager.GetEntityInfo(AObject).GetKeyValue(AObject);
end;

function TdxEMFCustomSession.GetQueryForFindObject(AEntityInfo: TdxEntityInfo; const ACriteria: IdxCriteriaOperator;
  ASelectDeleted: Boolean): TdxObjectsQuery;
begin
  Result := TdxObjectsQuery.Create(AEntityInfo, ACriteria, nil, 0, 1,
    TdxCollectionCriteriaPatcher.Create(ASelectDeleted), False);
end;

function TdxEMFCustomSession.GetQueryProvider<T>: IdxQueryProvider<T>;
begin
  Result := TQueryProvider<T>.Create(Self);
end;

function TdxEMFCustomSession.GetSession: IdxSession;
begin
  Result := Self;
end;

function TdxEMFCustomSession.GetUseCache: Boolean;
begin
  Result := True;
end;

function TdxEMFCustomSession.GetObjectsToSave(AIncludeParent: Boolean): TArray<TObject>;
var
  AMyObjects: TArray<TObject>;
begin
  AMyObjects := GetObjectsToSave;
  if not AIncludeParent then
    Exit(AMyObjects);
end;

function TdxEMFCustomSession.GetObjectsToDelete(AIncludeParent: Boolean): TArray<TObject>;
var
  AMyObjects: TArray<TObject>;
begin
  AMyObjects := GetObjectsToDelete;
  if not AIncludeParent then
    Exit(AMyObjects);
end;

procedure TdxEMFCustomSession.InternalFlushChanges;
var
  AFullListForDelete: TArray<TObject>;
  AProcessedListForSave: TdxArray<TObject>;
  AObject: TObject;
begin
  AFullListForDelete := FObjectsMarkedDeleted.ToArray;

  AProcessedListForSave := TdxArray<TObject>.Create(FObjectsMarkedSaved.Count);
  for AObject in FObjectsMarkedSaved do
    if not IsObjectToDelete(AObject) then
      AProcessedListForSave.Add(AObject);
  CommitChanges(AFullListForDelete, AProcessedListForSave.Value);
end;

function TdxEMFCustomSession.IsNewObject(AObject: TObject): Boolean;
begin
  Result := FPoolClasses.IsNewObject(AObject) and not FLoadingObjects.Contains(AObject);
end;

function TdxEMFCustomSession.IsObjectToDelete(AObject: TObject): Boolean;
begin
  if AObject = nil then
    Result := False
  else
    Result := FObjectsMarkedDeleted.Contains(AObject);
end;

function TdxEMFCustomSession.IsUnitOfWork: Boolean;
begin
  Result := False;
end;

procedure TdxEMFCustomSession.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = DataProvider) then
    DataProvider := nil;
  inherited Notification(AComponent, Operation);
end;

function TdxEMFCustomSession.PrepareLoadObjects(const AQueries: TArray<TdxObjectsQuery>): TdxLoadObjectsContext;
var
  AResult: TArray<TValue>;
  AExpanded: TArray<TdxExpandedCriteriaHolder>;
  ALoadDataQuery: TList<TdxObjectsQuery>;
  ARequiresResorting: TArray<Boolean>;
  I, ASkipPhysicallySelected, ATopPhysicallySelected: Integer;
  ALoadEntityInfo: TdxEntityInfo;
  ASortByExpressions: TdxSortByExpressions;
  ASortingExpression: IdxSortByExpression;
  AExpandedSort: TdxExpandedCriteriaHolder;
  AOrderByExpression: TdxLinqExpression;
begin
  if AQueries = nil then
    raise EArgumentNilException.Create('');
  ALoadDataQuery := TList<TdxObjectsQuery>.Create;
  try
    SetLength(AResult, Length(AQueries));
    SetLength(AExpanded, Length(AQueries));
    SetLength(ARequiresResorting, Length(AQueries));
    for I := 0 to Length(AQueries) - 1 do
    begin
      ALoadEntityInfo := AQueries[I].EntityInfo;
      AExpanded[I] := TdxPersistentCriterionExpander.ExpandToLogical(Self, ALoadEntityInfo, AQueries[I].Criteria, True);
      if AExpanded[I].IsFalse {or (Session.UpdateSchema(True, ALoadClassInfo) = UpdateSchemaResult.FirstTableNotExists)} then
        AResult[I] := 0
      else
      begin
        ASortByExpressions := TdxSortByExpressions.Create;
        if AQueries[I].Sorting <> nil then
        begin
          for ASortingExpression in AQueries[I].Sorting do
          begin
            AOrderByExpression := ASortingExpression.Expression;
            AExpandedSort := TdxPersistentCriterionExpander.ExpandToValue(Self, ALoadEntityInfo,
              AOrderByExpression, True);
            if AExpandedSort.RequiresPostProcessing then
            begin
              ARequiresResorting[I] := True;
              ASortByExpressions.Clear;
              Break;
            end
            else
              ASortByExpressions.Add(TdxSortByExpression.Create(
                TdxExpandedCriteriaHolder.IfNeededConvertToBoolOperator(AExpandedSort.ExpandedCriteria), ASortingExpression.SortDirection));
          end;
        end;
        ASkipPhysicallySelected := AQueries[I].SkipSelectedRecords;
        ATopPhysicallySelected := AQueries[I].TopSelectedRecords;
        if ((ATopPhysicallySelected + ASkipPhysicallySelected <> 0) and AExpanded[I].RequiresPostProcessing) or
          ARequiresResorting[I] then
        begin
          ATopPhysicallySelected := 0;
          ASkipPhysicallySelected := 0;
        end;
        ALoadDataQuery.Add(TdxObjectsQuery.Create(ALoadEntityInfo, AExpanded[I].ExpandedCriteria, ASortByExpressions,
          ASkipPhysicallySelected, ATopPhysicallySelected, AQueries[I].CollectionCriteriaPatcher, AQueries[I].Force));
      end;
    end;
    if ALoadDataQuery.Count = 0 then
      Result := TdxLoadObjectsContext.Create(AQueries, AResult, nil, nil)
    else
      Result := TdxLoadObjectsContext.Create(AQueries, AResult, AExpanded, ARequiresResorting, ALoadDataQuery.ToArray);
  finally
    ALoadDataQuery.Free;
  end;
end;

procedure TdxEMFCustomSession.PreProcessSavedList;
var
  AProcessedItems: TdxObjectSet;
  AObject: TObject;
  ARefObject: TValue;
  AEntityInfo: TdxEntityInfo;
  AMi: TdxMappingMemberInfo;
  ACollection: TdxEMFCustomCollection;
  AReferencedCollectionHelper: TdxReferencedCollectionHelper;
begin
  AProcessedItems := TdxObjectSet.Create;
  try
    while FObjectsMarkedSaved.Count <> AProcessedItems.Count do
    begin
      for AObject in GetObjectsToSave do
      begin
        if AProcessedItems.Contains(AObject) then
          Continue;
        AProcessedItems.Add(AObject);

        AEntityInfo := GetPFEntityInfo(AObject.ClassType);

        for AMi in AEntityInfo.ObjectProperties do
        begin
          if AMi.IsDelayed then
            Continue;
          ARefObject := AMi.GetValue(AObject);
          if ARefObject.IsEmpty then
            Continue;
          if IsNewObject(ARefObject.AsObject) or ((AMi.IsAggregated and not IsUnitOfWork)) then
            Save(ARefObject.AsObject);
        end;
        for AMi in AEntityInfo.CollectionProperties do
        begin
          ACollection := TdxEMFCustomCollection(AMi.GetValue(AObject).AsInterface);
          if ACollection = nil then
            Continue;
           AReferencedCollectionHelper := Safe<TdxReferencedCollectionHelper>.Cast(ACollection.Helper);
           if AReferencedCollectionHelper <> nil then
             AReferencedCollectionHelper.Save;
        end;
      end;
    end;
    for AObject in GetObjectsToDelete do
    begin
      if AProcessedItems.Contains(AObject) then
        Continue;
      AProcessedItems.Add(AObject);
      AEntityInfo := GetPFEntityInfo(AObject.ClassType);
      for AMi in AEntityInfo.CollectionProperties do
      begin
        ACollection := TdxEMFCustomCollection(AMi.GetValue(AObject).AsInterface);
        if ACollection = nil then
          Continue;
        AReferencedCollectionHelper := Safe<TdxReferencedCollectionHelper>.Cast(ACollection.Helper);
        if AReferencedCollectionHelper <> nil then
          AReferencedCollectionHelper.Save;
      end;
    end;
  finally
    AProcessedItems.Free;
  end;
end;

procedure TdxEMFCustomSession.ProcessDeletedObjects(AContext: TCommitChangesContext; const AFullListForDelete,
  ACompleteListForSave: TArray<TObject>; out ABeforeInserts, AAfterUpdates: TList<TdxModificationStatement>);
var
  ADeleteBeforeClassInfos, ABadClassInfos: TDictionary<TdxEntityInfo, TdxEntityInfo>;
  AChanges: Boolean;
  ABadCi, AGoodCi: TdxEntityInfo;
  AMi: TdxMappingMemberInfo;
  AObjectsDeletedFirst, AObjectsDeletedLast: TList<TObject>;
  AObject: TObject;
begin
  ADeleteBeforeClassInfos := CollectClassInfosFormObjects(AFullListForDelete);
  ABadClassInfos := CollectClassInfosFormObjects(ACompleteListForSave);
  try
    while True do
    begin
      AChanges := False;
      for ABadCi in ABadClassInfos.Keys.ToArray do
        for AMi in ABadCi.ObjectProperties do
          for AGoodCi in ADeleteBeforeClassInfos.Keys.ToArray do
            if AGoodCi.IsAssignableTo(AMi.ReferenceType) then
            begin
              ADeleteBeforeClassInfos.Remove(AGoodCi);
              ABadClassInfos.AddOrSetValue(AGoodCi, AGoodCi);
              AChanges := True;
            end;
      if not AChanges then
        Break;
    end;
    AObjectsDeletedFirst := TList<TObject>.Create;
    AObjectsDeletedLast := TList<TObject>.Create;
    try
      for AObject in AFullListForDelete do
        if ADeleteBeforeClassInfos.ContainsKey(AContext.Session.GetPFEntityInfo(AObject)) then
          AObjectsDeletedFirst.Add(AObject)
        else
          AObjectsDeletedLast.Add(AObject);
      ABeforeInserts := TdxDeleteQueryGenerator.GenerateDelete(AObjectsDeletedFirst, Options.LockingOption, AContext.BatchWideData);
      AAfterUpdates := TdxDeleteQueryGenerator.GenerateDelete(AObjectsDeletedLast, Options.LockingOption, AContext.BatchWideData);
    finally
      AObjectsDeletedFirst.Free;
      AObjectsDeletedLast.Free;
    end;
  finally
    ADeleteBeforeClassInfos.Free;
    ABadClassInfos.Free;
  end;
end;

procedure TdxEMFCustomSession.ProcessingProcess(AMarkedObjectsHolder: TdxObjectSet; AObject: TObject);
begin
  if not TrackingChanges then
  begin
    BeginTrackingChanges;
    if IsUnitOfWork then
      ProcessingProcessObjectListOrCollection(AMarkedObjectsHolder, AObject)
    else
    begin
      try
        ProcessingProcessObjectListOrCollection(AMarkedObjectsHolder, AObject);
        FlushChanges;
      except
        on E: Exception do
        begin
          if TrackingChanges then
          begin
              DropChanges;
          end;
          raise;
        end
      end;
    end;
  end
  else
    ProcessingProcessObjectListOrCollection(AMarkedObjectsHolder, AObject);
end;

procedure TdxEMFCustomSession.ProcessingProcessNextObject(AMarkedObjectsHolder: TdxObjectSet; AObject: TObject);
begin
  if not AMarkedObjectsHolder.Contains(AObject) then
  begin
    AMarkedObjectsHolder.Add(AObject);
  end;
end;

procedure TdxEMFCustomSession.ProcessingProcessObjectListOrCollection(AMarkedObjectsHolder: TdxObjectSet;
  AObjectOrCollection: TObject);
begin
    ProcessingProcessNextObject(AMarkedObjectsHolder, AObjectOrCollection);
end;

procedure TdxEMFCustomSession.RegisterCollection(ACollection: TdxEMFCustomCollection);
begin
  FLinkedCollections.AddOrSetValue(ACollection, 0);
end;

procedure TdxEMFCustomSession.RegisterInsertedObject(AObject: TObject);
begin
  AppendObject(AObject);
end;

procedure TdxEMFCustomSession.Reload(AObject: TObject);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  RemoveFromLists(AObject);
  if IsNewObject(AObject) then
    Exit;
  if GetObjects(TClass(AObject.ClassType),
    TdxBaseObjectQueryGenerator.BuildKeyCriteria(AObject),
      nil, 0, 1, True, True).Count = 0 then
    raise EdxCannotLoadObjectsException.Create(AObject.ClassName + '(' + GetKeyValue(AObject).ToString + ')');
end;

procedure TdxEMFCustomSession.RemoveFromDeleteList(AObject: TObject);
begin
  if AObject <> nil then
    FObjectsMarkedDeleted.Remove(AObject);
end;

procedure TdxEMFCustomSession.RemoveFromSaveList(AObject: TObject);
begin
  if AObject <> nil then
    FObjectsMarkedSaved.Remove(AObject);
end;

procedure TdxEMFCustomSession.RemoveFromLists(AObject: TObject);
begin
  RemoveFromDeleteList(AObject);
  RemoveFromSaveList(AObject);
end;

procedure TdxEMFCustomSession.RollbackTransaction;
begin
  DropChanges;
end;

procedure TdxEMFCustomSession.Attach(AObject: TObject);
begin
  AppendObject(AObject);
  TdxEntity.SetSession(Self, AObject);
end;

procedure TdxEMFCustomSession.AppendObject(AObject: TObject);
begin
  PoolClasses.AppendObject(AObject);
  FObjectsMarkedInserted.Remove(AObject);
end;

procedure TdxEMFCustomSession.AppendObject(const AKeys: TArray<TValue>; AObject: TObject);
begin
  PoolClasses.AppendObject(AKeys, AObject);
end;

procedure TdxEMFCustomSession.Delete(AObject: TObject);
begin
  if TrackingChanges then
    DeleteObject(AObject)
  else
  begin
    BeginTrackingChanges;
    try
      DeleteObject(AObject);
      if not IsUnitOfWork then
        FlushChanges;
    except
      on E: Exception do
      begin
        if TrackingChanges then
        begin
            DropChanges;
        end;
        raise;
      end
    end;
  end;
end;

procedure TdxEMFCustomSession.DeleteCore(AEntityInfo: TdxEntityInfo; AObject: TObject);
var
  AMemberInfo: TdxMappingMemberInfo;
  ACollection: IInterface;
  AEMFCollection: TdxEMFObjectCollection;
  AToDelete: TArray<TObject>;
  AAggregated, AAssociatedObject: TObject;
  AList, AAssociatedCollection: IdxList;
  AInterface: IInterface;
begin
  for AMemberInfo in AEntityInfo.AssociationListProperties do
    if AMemberInfo.IsAggregated then
    begin
      if AMemberInfo.IsCollection then
      begin
        ACollection := AMemberInfo.GetValue(AObject).AsInterface;
        AEMFCollection := Safe<TdxEMFObjectCollection>.Cast(ACollection as TObject);
        CheckFilteredAggregateDeletion(AObject, AMemberInfo, AEMFCollection);
        AToDelete := AEMFCollection.ToArray;
        for AAggregated in AToDelete do
          Delete(AAggregated);
        if not AEMFCollection.SelectDeleted then
        begin
          AList := AEMFCollection;
          for AAggregated in AToDelete do
            AList.Remove(AAggregated);
        end;
      end
    end
    else
    begin
      ACollection := AMemberInfo.GetValue(AObject).AsInterface;
      AEMFCollection := Safe<TdxEMFObjectCollection>.Cast(ACollection as TObject);
      if not AEMFCollection.SelectDeleted then
      begin
        for AAggregated in AEMFCollection.ToArray do
        begin
          AList := AEMFCollection;
          AList.Remove(AAggregated);
        end;
      end;
    end;
  for AMemberInfo in AEntityInfo.ObjectProperties do
  begin
    if AMemberInfo.IsAggregated then
    begin
      AAggregated := AMemberInfo.GetValue(AObject).AsObject;
      Delete(AAggregated);
    end;
    if AMemberInfo.IsAssociation then
    begin
      AAssociatedObject := AMemberInfo.GetValue(AObject).AsObject;
      if AAssociatedObject <> nil then
      begin
        AInterface := AMemberInfo.AssociatedMember.GetValue(AAssociatedObject).AsInterface;
        Supports(AInterface, IdxList, AAssociatedCollection);
        if AAssociatedCollection <> nil then
          AAssociatedCollection.Remove(AObject);
      end;
    end;
  end;
end;

procedure TdxEMFCustomSession.DeleteObject(AObject: TObject);
var
  AEntityInfo: TdxEntityInfo;
begin
  if AObject = nil then
    Exit;
  AEntityInfo := GetPFEntityInfo(AObject);
    if IsObjectToDelete(AObject) then
      Exit;
    DoObjectDeleting(AObject);
    ProcessingProcess(FObjectsMarkedDeleted, AObject);
    DeleteCore(AEntityInfo, AObject);
    DoObjectDeleted(AObject);
end;

procedure TdxEMFCustomSession.DoAfterBeginTrackingChanges;
begin
end;

procedure TdxEMFCustomSession.DoAfterConnect;
begin

end;

procedure TdxEMFCustomSession.DoAfterDropChanges;
begin

end;

procedure TdxEMFCustomSession.DoAfterFlushChanges;
begin
end;

procedure TdxEMFCustomSession.DoBeforeBeginTrackingChanges;
begin
end;

procedure TdxEMFCustomSession.DoBeforeConnect;
begin

end;

procedure TdxEMFCustomSession.DoBeforeDropChanges;
begin

end;

procedure TdxEMFCustomSession.DoBeforeFlushChanges;
begin
end;

procedure TdxEMFCustomSession.DoCreateObject(AClass: TClass; out AObject: TObject);
var
  AFunc: TFunc<TObject>;
begin
  if (FOnCreateFunc <> nil) and FOnCreateFunc.TryGetValue(AClass, AFunc) then
  begin
    AObject := AFunc;
    Exit;
  end;
  AObject := nil;
  if Assigned(FOnObjectCreating) then
    FOnObjectCreating(Self, AClass, AObject);
end;

procedure TdxEMFCustomSession.DoObjectDeleted(AObject: TObject);
begin
end;

procedure TdxEMFCustomSession.DoObjectDeleting(AObject: TObject);
begin
end;

procedure TdxEMFCustomSession.DoObjectLoaded;
begin

end;

procedure TdxEMFCustomSession.DoObjectLoading;
begin

end;

procedure TdxEMFCustomSession.DoObjectSaved;
begin

end;

procedure TdxEMFCustomSession.DoObjectSaving;
begin

end;

procedure TdxEMFCustomSession.DropChanges;
begin
  DoBeforeDropChanges;
  if not IsUnitOfWork and not TrackingChanges then
    raise EdxTransactionSequenceException.Create(sdxSessionTranSequenceRollback);
  TdxSessionStateStack.Enter(Self, TdxSessionState.RollbackTransaction);
  try
    FCollectionsMarkedSaved.Clear;
    FObjectsMarkedSaved.Clear;
    FObjectsMarkedDeleted.Clear;
    FTrackingChanges := False;
  finally
    TdxSessionStateStack.Leave(Self, TdxSessionState.RollbackTransaction);
  end;
  DoAfterDropChanges;
end;

procedure TdxEMFCustomSession.EndCommitChangesInsideTransaction(AContext: TCommitChangesContext);
begin
  AContext.ProcessingSave.ProcessResults(AContext.Result);
end;

procedure TdxEMFCustomSession.EndFlushChanges(const AObjectsToFireSaved: TArray<TObject>);
var
  C: TdxReferencedCollectionHelper;
begin
  TdxSessionStateStack.Enter(Self, TdxSessionState.CommitTransactionNonReentrant);
  try
    for C in FCollectionsMarkedSaved.Keys do
      C.ClearChangesCache;
    FreeMarkedInsertedObjects;
    FCollectionsMarkedSaved.Clear;
    FObjectsMarkedSaved.Clear;
    FObjectsMarkedDeleted.Clear;
    FTrackingChanges := False;
  finally
    TdxSessionStateStack.Leave(Self, TdxSessionState.CommitTransactionNonReentrant);
  end;
  TriggerObjectsSaved(AObjectsToFireSaved);
  DoAfterFlushChanges;
end;

procedure TdxEMFCustomSession.EndSelect;
begin
  FreeAndNil(FOnCreateFunc);
end;

function TdxEMFCustomSession.EntityByName(const AName: string): IdxEntityInfo;
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := EntityManager.GetEntityInfo(AName);
  if AEntityInfo = nil then
    Exit(nil);
  Result := TdxLinqExpressionFactory.GetEntityMetadata(AEntityInfo.ClassAttributes.PersistentClass, Self);
end;

function TdxEMFCustomSession.ExtractPersistentValue(const ACriterionValue: TValue): TValue;
begin
  if ACriterionValue.IsObject then
    Result := GetKeyValue(ACriterionValue.AsObject)
  else
    Result := ACriterionValue;
end;

procedure TdxEMFCustomSession.Save(AObject: TObject);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  TdxEntity.SetSession(Self, AObject);
  if IsNewObject(AObject) then
    FObjectsMarkedInserted.Add(AObject);
  ProcessingProcess(FObjectsMarkedSaved, AObject);
end;

function TdxEMFCustomSession.SelectData(AEntityInfo: TdxEntityInfo; const AProperties: IdxCriteriaOperatorCollection;
  const ACriteria: IdxCriteriaOperator; const AGroupProperties: IdxCriteriaOperatorCollection;
  const AGroupCriteria: IdxCriteriaOperator;
  ASelectDeleted: Boolean; ASkipSelectedRecords, ATopSelectedRecords: Integer;
  const ASortByExpressions: IdxSortByExpressions): TdxQueryData;
var
  AQuery: TdxObjectsQuery;
begin
  CheckConnection;
  AQuery := TdxObjectsQuery.Create(AEntityInfo, ACriteria, ASortByExpressions, ASkipSelectedRecords, ATopSelectedRecords,
    TdxCollectionCriteriaPatcher.Create(ASelectDeleted), True);
  try
    Result := SelectDataInternal(AQuery, AProperties, AGroupProperties, AGroupCriteria);
  finally
    AQuery.Free;
  end;
end;

function TdxEMFCustomSession.SelectDataInternal(AObjectsQuery: TdxObjectsQuery; const AProperties,
  AGroupProperties: IdxCriteriaOperatorCollection; const AGroupCriteria: IdxCriteriaOperator): TdxQueryData;
var
  ARoot: TdxSelectStatement;
  ASQLGenerator: TdxSelectSQLGenerator;
  AQuery: TdxQuery;
  ADataLoader: TdxDataLoader;
begin
  ARoot := TdxClientSelectSqlGenerator.GenerateSelect(AObjectsQuery.EntityInfo, AObjectsQuery.Criteria, AProperties, AObjectsQuery.Sorting,
    AGroupProperties, AGroupCriteria, AObjectsQuery.CollectionCriteriaPatcher, AObjectsQuery.SkipSelectedRecords, AObjectsQuery.TopSelectedRecords);
  try
    ASQLGenerator := TdxSelectSQLGenerator.Create(DataProvider.SQLConnectionProvider);
    try
      AQuery := ASQLGenerator.GenerateSQL(ARoot);
      ADataLoader := DataProvider.CreateLoader(Self, DataProvider.ConnectionVendor, AQuery,
        AObjectsQuery.EntityInfo);
      ADataLoader.Query := AQuery;
      Result := TdxQueryData.Create(AObjectsQuery.EntityInfo, ARoot.Operands.ToArray, ADataLoader);
    finally
      ASQLGenerator.Free;
    end;
  finally
    ARoot.Free;
  end;
end;

procedure TdxEMFCustomSession.SetDataProvider(Value: TdxEMFCustomDataProvider);
begin
  if FDataProvider = Value then
    Exit;
  if FDataProvider <> nil then
    FDataProvider.RemoveFreeNotification(Self);
  FDataProvider := Value;
  if FDataProvider <> nil then
    FDataProvider.FreeNotification(Self);
end;

procedure TdxEMFCustomSession.SetKeyValue(AObject: TObject;
  const AKeyValue: TArray<TValue>);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  GetPFEntityInfo(AObject).KeyProperty.SetValues(AObject, AKeyValue);
end;

procedure TdxEMFCustomSession.SetKeyValue(AObject: TObject; const AKeyValue: TValue);
begin
  if AObject = nil then
    raise EArgumentNilException.Create('');
  GetPFEntityInfo(AObject).KeyProperty.SetValue(AObject, AKeyValue);
end;

procedure TdxEMFCustomSession.SetOptions(const Value: TdxEMFSessionOptions);
begin
  FOptions.Assign(Value);
end;

procedure TdxEMFCustomSession.TriggerObjectsSaved(const AObjects: TArray<TObject>);
begin
end;

procedure TdxEMFCustomSession.UnregisterCollection(ACollection: TdxEMFCustomCollection);
begin
  FLinkedCollections.Remove(ACollection);
end;

procedure TdxEMFCustomSession.UnregisterObject(AObject: TObject);
begin
  Detach(AObject);
  if UseCache then
    AObject.Free;
end;

function TdxEMFCustomSession.CreateObject(AClass: TClass): TObject;
begin
  DoCreateObject(AClass, Result);
  if Result <> nil then
    Exit;
  Result := EntityManager.CreateObject(AClass);
end;

function TdxEMFCustomSession.CreateQuery(const AExpression: IdxExpression): IdxQueryable;
var
  ALinqQuery: IdxLinqQueryBuilder;
  ACriteria: IdxCriteriaOperator;
begin
  ALinqQuery := AExpression as IdxLinqQueryBuilder;
  ACriteria := ALinqQuery.WhereClause;
  Result := GetObjects(ALinqQuery.FromClause.FromClass, ACriteria, ALinqQuery.OrderByClause,
    ALinqQuery.SkipSelectedRecords, ALinqQuery.TopSelectedRecords) as IdxQueryable;
end;

function TdxEMFCustomSession.CustomCreate<T>(AFunc: TFunc<T>): TdxEMFCustomSession;
begin
  if FOnCreateFunc = nil then
    FOnCreateFunc := TDictionary<TClass, TFunc<TObject>>.Create;
  FOnCreateFunc.AddOrSetValue(TClass(T), TFunc<TObject>(AFunc));
  Result := Self;
end;

function TdxEMFCustomSession.CreateLoader(AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate): TdxDataLoader;
begin
    Result := GetDataProvider.CreateLoader(Self, AObjectsQuery, APredicate);
end;

{$REGION 'Find'}
function TdxEMFCustomSession.Find<T>(const AKey: TValue): T;
begin
  Result := T(Find(TClass(T), AKey));
end;


function TdxEMFCustomSession.FilterListForDelete(const AFullListForDelete: TArray<TObject>): TArray<TObject>;
var
  AObject: TObject;
  J: Integer;
begin
  if AFullListForDelete = nil then
    Exit(nil);
  SetLength(Result, Length(AFullListForDelete));
  J := 0;
  for AObject in AFullListForDelete do
    if not IsNewObject(AObject) then
    begin
      Result[J] := AObject;
      Inc(J);
    end;
  SetLength(Result, J);
end;

function TdxEMFCustomSession.Find(AClass: TClass; const ACriteria: IdxCriteriaOperator): TObject;
var
  AEntityInfo: TdxEntityInfo;
  AObjectsQuery: TdxObjectsQuery;
  ALoader: TdxDataLoader;
begin
  BeginSelect;
  try
    CheckConnection;
    AEntityInfo := GetPFEntityInfo(AClass);
    AObjectsQuery := GetQueryForFindObject(AEntityInfo, ACriteria, False);
    try
      ALoader := CreateLoader(AObjectsQuery);
      try
        Result := ALoader.GetNextObject;
      finally
        ALoader.Free;
      end;
    finally
      AObjectsQuery.Free;
    end;
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.Find<T>(const ACriteria: IdxCriteriaOperator): T;
begin
  Result := T(Find(TClass(T), ACriteria));
end;


function TdxEMFCustomSession.Find(AClass: TClass; APredicate: TdxPredicate): TObject;
var
  ALoader: TdxDataLoader;
begin
  BeginSelect;
  try
    ALoader := GetDataProvider.CreateLoader(Self, AClass, APredicate);
    try
      Result := ALoader.GetNextObject;
    finally
      ALoader.Free;
    end;
  finally
    EndSelect;
  end;
end;


function TdxEMFCustomSession.Find(AClass: TClass; const AKey: TValue): TObject;
var
  AEntityInfo: TdxEntityInfo;
  AObjectsQueries: TObjectList<TdxObjectsQuery>;
  AObjectsQuery: TdxObjectsQuery;
  ALoader: TdxDataLoader;
begin
  BeginSelect;
  try
    CheckConnection;
    AEntityInfo := GetPFEntityInfo(AClass);
    AObjectsQueries := GetQueriesByKeys(TObjectsByKeyQuery.Create(AEntityInfo, AKey));
    try
      if AObjectsQueries.Count = 0 then
        Exit(nil);
      AObjectsQuery := AObjectsQueries[0];
      AObjectsQuery.TopSelectedRecords := 1;
      AObjectsQuery.Force := False;
      ALoader := CreateLoader(AObjectsQuery);
      try
        Result := ALoader.GetNextObject;
      finally
        ALoader.Free;
      end;
    finally
      AObjectsQueries.Free;
    end;
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.Find(AClass: TClass; const AKeys: TArray<TValue>): TObject;
begin
  if Length(AKeys) = 1 then
    Exit(Find(AClass, AKeys[0]));
  Result := nil;
  NotImplemented;
end;

function TdxEMFCustomSession.Find<T>(APredicate: TdxPredicate<T>): T;
begin
  Result := T(Find(TClass(T), TdxPredicate(APredicate)));
end;
{$ENDREGION}

function TdxEMFCustomSession.GetLoadingStrategy: TdxLoadingStrategy;
begin
  Result := Options.Collections.LoadingStrategy;
end;

function TdxEMFCustomSession.GetQueriesByKeys(const AGetQuery: TObjectsByKeyQuery): TObjectList<TdxObjectsQuery>;
var
  AKeyProperty: IdxOperandProperty;
  ALength, APos, ACurrentSize: Integer;
  AId: TValue;
  AIdList: TdxCriteriaOperatorList;
  AUseGetTerminalInSize: Boolean;
begin
  Result := TObjectList<TdxObjectsQuery>.Create;
  AKeyProperty := TdxOperandProperty.Create(AGetQuery.EntityInfo.KeyAttributes[0].MemberName);
  ALength := AGetQuery.Count;
  if ALength = 0 then
    Exit;
  if ALength = 1 then
  begin
    Result.Add(TdxObjectsQuery.Create(AGetQuery.EntityInfo,
      TdxBinaryOperator.Create(AKeyProperty, TdxOperandValue.Create(AGetQuery.Keys[0]), TdxBinaryOperatorType.Equal),
      nil, 0, 0, TdxCollectionCriteriaPatcher.Create(True), True));
  end
  else
  begin
    AIdList := TdxCriteriaOperatorList.Create;
    try
      APos := 0;
      ACurrentSize := 0;
      AUseGetTerminalInSize := AGetQuery.Count > TdxEMFDefault.MaxInSize;
      for AId in AGetQuery.Keys do
      begin
        if AIdList.Count = 0 then
        begin
          if AUseGetTerminalInSize then
            ACurrentSize := TdxEMFDefault.GetTerminalInSize(ALength - APos)
          else
            ACurrentSize := ALength - APos;
        end;
        AIdList.Add(TdxOperandValue.Create(AId));
        Inc(APos);
        Dec(ACurrentSize);
        if ACurrentSize = 0 then
        begin
          Result.Add(TdxObjectsQuery.Create(AGetQuery.EntityInfo, TdxInOperator.Create(AKeyProperty, AIdList), nil, 0, 0,
            TdxCollectionCriteriaPatcher.Create(True), True));
          AIdList.Clear;
        end;
      end;
    finally
      AIdList.Free;
    end;
  end;
end;

function TdxEMFCustomSession.GetObjectsInternal(const AQueries: TArray<TdxObjectsQuery>): TArray<IdxEMFCollection>;
begin
end;

{$REGION 'GetObjects'}

function TdxEMFCustomSession.GetObjects(AClass: TClass): IdxEMFCollection;
var
  ALoader: TdxDataLoader;
  AList: TdxEMFObjectCollection;
begin
  BeginSelect;
  try
    ALoader := GetDataProvider.CreateLoader(Self, AClass);
    AList := TdxEMFObjectCollection.Create(Self);
    AList.CollectionElementClass := AClass;
    AList.Loader := ALoader;
    Result := AList;
  finally
    EndSelect;
  end;
end;


function TdxEMFCustomSession.GetObjects<T>(const ACriteria: IdxCriteriaOperator): IdxEMFCollection<T>;
begin
  Result := GetObjects<T>(ACriteria, nil);
end;

function TdxEMFCustomSession.GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator): IdxEMFCollection;
begin
  Result := GetObjects(AClass, ACriteria, nil);
end;

function TdxEMFCustomSession.GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
  ASkipSelectedRecords, ATopSelectedRecords: Integer; ASelectDeleted: Boolean; AForceReload: Boolean): IdxEMFCollection;
var
  AEntityInfo: TdxEntityInfo;
  AObjectsQuery: TdxObjectsQuery;
  ALoader: TdxDataLoader;
  AList: TdxEMFObjectCollection;
begin
  BeginSelect;
  try
    CheckConnection;
    AEntityInfo := GetPFEntityInfo(AClass);
    AObjectsQuery := TdxObjectsQuery.Create(AEntityInfo, ACriteria, ASortByExpressions, ASkipSelectedRecords, ATopSelectedRecords,
      TdxCollectionCriteriaPatcher.Create(ASelectDeleted), AForceReload);
    try
      ALoader := CreateLoader(AObjectsQuery);
      AList := TdxEMFObjectCollection.Create(Self);
      AList.CollectionElementClass := AClass;
      AList.Loader := ALoader;
      Result := AList;
    finally
      AObjectsQuery.Free;
    end;
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.GetObjects<T>(const ACriteria: IdxCriteriaOperator; const ASortByExpressions: IdxSortByExpressions;
  ASkipSelectedRecords, ATopSelectedRecords: Integer; ASelectDeleted: Boolean; AForceReload: Boolean): IdxEMFCollection<T>;
var
  AEntityInfo: TdxEntityInfo;
  AObjectsQuery: TdxObjectsQuery;
  ALoader: TdxDataLoader;
  AList: TdxEMFEntityCollection<T>;
begin
  BeginSelect;
  try
    CheckConnection;
    AEntityInfo := GetPFEntityInfo(TClass(T));
    AObjectsQuery := TdxObjectsQuery.Create(AEntityInfo, ACriteria, ASortByExpressions, ASkipSelectedRecords, ATopSelectedRecords,
      TdxCollectionCriteriaPatcher.Create(ASelectDeleted), AForceReload);
    try
      ALoader := CreateLoader(AObjectsQuery);
      AList := TdxEMFEntityCollection<T>.Create(Self);
      AList.SelectDeleted := ASelectDeleted;
      AList.SkipReturnedObjects := ASkipSelectedRecords;
      AList.TopReturnedObjects := ATopSelectedRecords;
      AList.Loader := ALoader;
      Result := AList;
    finally
      AObjectsQuery.Free;
    end;
  finally
    EndSelect;
  end;
end;


function TdxEMFCustomSession.GetObjects<T>(APredicate: TdxPredicate<T>): IdxEMFCollection<T>;
var
  ALoader: TdxDataLoader;
  AList: TdxEMFEntityCollection<T>;
begin
  BeginSelect;
  try
    ALoader := GetDataProvider.CreateLoader(Self, TClass(T), TdxPredicate(APredicate));
    AList := TdxEMFEntityCollection<T>.Create(Self);
    AList.Loader := ALoader;
    Result := AList;
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.GetObjects<T>: IdxEMFCollection<T>;
var
  ALoader: TdxDataLoader;
  AList: TdxEMFEntityCollection<T>;
begin
  BeginSelect;
  try
    ALoader := GetDataProvider.CreateLoader(Self, TClass(T));
    AList := TdxEMFEntityCollection<T>.Create(Self);
    AList.Loader := ALoader;
    Result := AList;
  finally
    EndSelect;
  end;
end;

function TdxEMFCustomSession.GetObjects<T>(const AQuery: IdxQueryable<T>): IdxEMFCollection<T>;
var
  ALinqQuery: IdxLinqQueryBuilder;
  ACriteria: IdxCriteriaOperator;
begin
  ALinqQuery := AQuery as IdxLinqQueryBuilder;
  if not ALinqQuery.FromClause.FromClass.InheritsFrom(T) then
    raise EInvalidOperation.Create(sdxTypeMismatch);
  ACriteria := ALinqQuery.WhereClause; //.AddReference as TdxCriteriaOperator;
  Result := GetObjects<T>(ACriteria, ALinqQuery.OrderByClause,
    ALinqQuery.SkipSelectedRecords, ALinqQuery.TopSelectedRecords);
end;

{$ENDREGION}

{$REGION 'GetLoadedObjectByKey'}
function TdxEMFCustomSession.GetLoadedObjectByKey(AClass: TClass; const AKey: TValue): TObject;
begin
  Result := FindObjectInCache(AClass, TArray<TValue>.Create(AKey));
end;

function TdxEMFCustomSession.GetLoadedObjectByKey(AClass: TClass; const AKeys: TArray<TValue>): TObject;
begin
  Result := FindObjectInCache(AClass, AKeys);
end;

function TdxEMFCustomSession.GetLoadedObjectByKey<T>(const AKeys: TArray<TValue>): T;
begin
  Result := T(GetLoadedObjectByKey(TClass(T), AKeys));
end;
{$ENDREGION}

{ TdxEMFDataProviderOptions }

constructor TdxEMFDataProviderOptions.Create(AOwner: TdxEMFCustomDataProvider);
begin
  inherited Create;
  FOwner := AOwner;
  FAutoDetectDBEngine := True;
end;

procedure TdxEMFDataProviderOptions.Assign(Source: TPersistent);
var
  ASource: TdxEMFDataProviderOptions;
begin
  if Source is TdxEMFDataProviderOptions then
  begin
    ASource := TdxEMFDataProviderOptions(Source);
    DBEngine := ASource.DBEngine;
    AutoCreate := ASource.AutoCreate;
    AutoDetectDBEngine := ASource.AutoDetectDBEngine;
  end
  else
    inherited Assign(Source);
end;

procedure TdxEMFDataProviderOptions.Changed;
begin
end;

function TdxEMFDataProviderOptions.GetDBEngine: TdxDBEngine;
begin
  if FProviderSQLClass = nil then
    Result := TdxDBEngines.Empty
  else
    Result := FProviderSQLClass.GetDBEngine;
end;

procedure TdxEMFDataProviderOptions.SetAutoCreate(const Value: TdxAutoCreateOption);
begin
  if AutoCreate <> Value then
  begin
    FAutoCreate := Value;
    FOwner.RecreateProviderSQL;
    Changed;
  end;
end;

procedure TdxEMFDataProviderOptions.SetAutoDetectDBEngine(const Value: Boolean);
begin
  if FAutoDetectDBEngine <> Value then
  begin
    FAutoDetectDBEngine := Value;
    FOwner.RecreateProviderSQL;
    Changed;
  end;
end;

procedure TdxEMFDataProviderOptions.SetProviderSQLClass(const Value: TdxSQLConnectionProviderClass);
begin
  if FProviderSQLClass <> Value then
  begin
    FProviderSQLClass := Value;
    FOwner.RecreateProviderSQL;
    Changed;
  end;
end;

procedure TdxEMFDataProviderOptions.SetDBEngine(const Value: TdxDBEngine);
begin
  if DBEngine <> Value  then
    ProviderSQLClass := TdxSQLConnectionProviderFactory.GetProviderSQL(Value);
end;

{ TdxEMFCustomDataProvider.TEntityDBModel }

constructor TdxEMFCustomDataProvider.TEntityDBModel.Create(AClass: TClass);
begin
  inherited Create;
  FPersistentClass := AClass;
  InitScheme;
end;

destructor TdxEMFCustomDataProvider.TEntityDBModel.Destroy;
begin
  FreeAndNil(FDBTables);
  inherited Destroy;
end;

procedure TdxEMFCustomDataProvider.TEntityDBModel.InitScheme;
begin
  FEntityInfo := EntityManager.GetEntityInfo(FPersistentClass);
  if FEntityInfo = nil then
    raise EdxNoEntityInfoException.CreateFmt(sdxClassIsNotEntity, [FPersistentClass.ClassName]);
  FDBTables := TdxDBTableHelper.ProcessEntityInfo(FEntityInfo);
end;

{ TdxEMFCustomDataProvider.TTableList }

procedure TdxEMFCustomDataProvider.TTableList.AddUnique(AList: TList<TdxDBTable>);
var
  ATable: TdxDBTable;
  AExistsTable: TdxDBTable;
begin
  for ATable in AList do
  begin
    AExistsTable := GetTableByName(ATable.Name);
    if (AExistsTable <> nil) then
    begin
      if (AExistsTable.Columns.Count >= ATable.Columns.Count) then
      begin
        ATable.Free;
        Continue;
      end
      else
      if (AExistsTable.Columns.Count < ATable.Columns.Count) then
        Remove(AExistsTable)
    end;
    Add(ATable);
  end;
end;

function TdxEMFCustomDataProvider.TTableList.GetTableByName(const ATableName: string): TdxDBTable;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if SameText(ATableName, Items[I].Name) then
      Exit(Items[I]);
  Result := nil;
end;

{ TdxEMFCustomDataProvider }

constructor TdxEMFCustomDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TdxEMFDataProviderOptions.Create(Self);
  FEntityDBModels := TObjectDictionary<TClass, TEntityDBModel>.Create([doOwnsValues]);
  FQueryDataPool := TdxQueryDataPool.Create;
end;

destructor TdxEMFCustomDataProvider.Destroy;
begin
  FreeAndNil(FEntityDBModels);
  FreeAndNil(FConnectionVendor);
  FreeAndNil(FSQLConnectionProvider);
  FreeAndNil(FOptions);
  FreeAndNil(FQueryDataPool);
  FreeAndNil(FTableList);
  inherited Destroy;
end;

function TdxEMFCustomDataProvider.DoCreateLoader(ASession: TdxEMFCustomSession; AObjectsQuery: TdxObjectsQuery;
  APredicate: TdxPredicate): TdxDataLoader;
var
  ARoot: TdxSelectStatement;
  ASQLGenerator: TdxSelectSQLGenerator;
  AQueryData: TdxQueryData;
  AQuery: TdxQuery;
begin
  AQueryData := GetQueryData(AObjectsQuery.EntityInfo);
  ARoot := TdxClientSelectSQLGenerator.GenerateSelect(AObjectsQuery.EntityInfo, AObjectsQuery.Criteria,
    AQueryData.Properties, AObjectsQuery.Sorting, nil, nil, AObjectsQuery.CollectionCriteriaPatcher,
    AObjectsQuery.SkipSelectedRecords, AObjectsQuery.TopSelectedRecords);
  try
    ASQLGenerator := TdxSelectSQLGenerator.Create(SQLConnectionProvider);
    try
      AQuery := ASQLGenerator.GenerateSQL(ARoot);
      try
        Result := CreateLoader(ASession, ConnectionVendor, AQuery, AObjectsQuery.EntityInfo, APredicate);
        Result.TopReturnedObjects := AQuery.TopSelectedRecords;
        Result.SkipReturnedObjects := AQuery.SkipSelectedRecords;
        Result.ForceReload := AObjectsQuery.Force;
      finally
        AQuery.Free;
      end;
    finally
      ASQLGenerator.Free;
    end;
  finally
    ARoot.Free;
  end;
end;

function TdxEMFCustomDataProvider.DoCreateLoader(ASession: TdxEMFCustomSession; AEntityInfo: TdxEntityInfo;
  APredicate: TdxPredicate): TdxDataLoader;
var
  AObjectsQuery: TdxObjectsQuery;
begin
  AObjectsQuery := TdxObjectsQuery.Create(AEntityInfo, nil, nil, 0, 0,
    TdxCollectionCriteriaPatcher.Create(False), False);
  try
    Result := CreateLoader(ASession, AObjectsQuery, APredicate);
  finally
    AObjectsQuery.Free;
  end;
end;

function TdxEMFCustomDataProvider.GetCanCreateDatabase: Boolean;
begin
  Result := Options.AutoCreate = TdxAutoCreateOption.DatabaseAndSchema;
end;

function TdxEMFCustomDataProvider.GetCanCreateSchema: Boolean;
begin
  Result := (Options.AutoCreate = TdxAutoCreateOption.SchemaOnly) or CanCreateDatabase;
end;

function TdxEMFCustomDataProvider.GetDBSchema: TObjectList<TdxDBTable>;
var
  ATables: TStringList;
  ADBTable: TdxDBTable;
  I: Integer;
begin
  CheckProvider;
  ATables := TStringList.Create;
  try
    GetTableNames(ATables);
    Result := TObjectList<TdxDBTable>.Create;
    Result.Capacity := ATables.Count;
    for I := 0 to ATables.Count - 1 do
    begin
      ADBTable := TdxDBTable.Create(ATables[I]);
      SQLConnectionProvider.GetTableSchema(ADBTable, True, True);
      Result.Add(ADBTable);
    end;
  finally
    ATables.Free;
  end;
end;

function TdxEMFCustomDataProvider.CreateLoader(ASession: TdxEMFCustomSession; AClass: TClass; APredicate: TdxPredicate = nil): TdxDataLoader;
var
  AEntityInfo: TdxEntityInfo;
begin
  CheckProvider;
  AEntityInfo := EntityManager.GetEntityInfo(AClass);
  if AEntityInfo = nil then
    raise EdxNoEntityInfoException.CreateFmt(sdxClassIsNotEntity, [AClass.ClassName]);
  Result := DoCreateLoader(ASession, AEntityInfo, APredicate);
end;

function TdxEMFCustomDataProvider.CreateLoader(ASession: TdxEMFCustomSession; AObjectsQuery: TdxObjectsQuery; APredicate: TdxPredicate = nil): TdxDataLoader;
begin
  CheckProvider;
  Result := DoCreateLoader(ASession, AObjectsQuery, APredicate);
end;

function TdxEMFCustomDataProvider.GetEntityDBModel(AClass: TClass): TEntityDBModel;
begin
  if not FEntityDBModels.TryGetValue(AClass, Result) then
  begin
    Result := TEntityDBModel.Create(AClass);
    FEntityDBModels.Add(AClass, Result);
  end;
end;

function TdxEMFCustomDataProvider.GetQueryData(AEntityInfo: TdxEntityInfo): TdxQueryData;
begin
  Result := FQueryDataPool.GetQueryData(AEntityInfo);
end;

procedure TdxEMFCustomDataProvider.BeginCreateTable;
begin
  if FTableList = nil then
    FTableList := TTableList.Create;
  Inc(FTableList.FRefCount);
end;

procedure TdxEMFCustomDataProvider.EndCreateTable;
begin
  if FTableList <> nil then
  begin
    Dec(FTableList.FRefCount);
    if FTableList.FRefCount <= 0 then
    try
      if FTableList.Count > 0 then
        SQLConnectionProvider.ProcessUpdateSchema(False, FTableList.ToArray);
    finally
      FreeAndNil(FTableList);
    end;
  end;
end;

procedure TdxEMFCustomDataProvider.UpdateSchema(AEntityInfo: TdxEntityInfo);
var
  ADBTables: TObjectList<TdxDBTable>;
begin
  if AEntityInfo = nil then
    Exit;
  if Options.AutoCreate = TdxAutoCreateOption.SchemaAlreadyExists then
  begin
    AEntityInfo.CheckAbstractReference;
    Exit;
  end;
  CheckProvider;
  BeginCreateTable;
  try
    ADBTables := TdxDBTableHelper.ProcessEntityInfo(AEntityInfo);
    try
      if ADBTables.Count = 0 then
        raise EdxUnableToCreateDBObjectException.Create('');
      FTableList.AddUnique(ADBTables);
      ADBTables.OwnsObjects := False;
    finally
      ADBTables.Free;
    end;
  finally
    EndCreateTable;
  end;
end;

function TdxEMFCustomDataProvider.CreateProviderSQL(const ADBEngineType: TdxDBEngine): TdxSQLConnectionProvider;
var
  AProviderSQLClass: TdxSQLConnectionProviderClass;
begin
  if (ADBEngineType = TdxDBEngines.Unknown) or (ADBEngineType = TdxDBEngines.Empty) then
    Exit(nil);
  AProviderSQLClass :=  TdxSQLConnectionProviderFactory.GetProviderSQL(ADBEngineType);
  if AProviderSQLClass = nil then
    raise EdxUnknownProviderSQL.CreateFmt(sdxUnknownProviderSQL, [ADBEngineType]);
  Result := AProviderSQLClass.Create(ConnectionVendor, Options.AutoCreate);
end;

procedure TdxEMFCustomDataProvider.CreateTable(const ATableName: string; AEntityInfo: TdxEntityInfo);
var
  ADBTables: TObjectList<TdxDBTable>;
  ADBTable: TdxDBTable;
begin
  CheckProvider;
  BeginCreateTable;
  try
    ADBTables := TdxDBTableHelper.ProcessEntityInfo(AEntityInfo);
    try
      if ADBTables.Count = 0 then
        raise EdxUnableToCreateDBObjectException.Create('');
      ADBTable := ADBTables[0];
      SQLConnectionProvider.CreateTable(ADBTable);
    finally
      ADBTables.Free;
    end;
  finally
    EndCreateTable;
  end;
end;

procedure TdxEMFCustomDataProvider.GetTableNames(AList: TStrings; AIncludeViews: Boolean);
begin
  AList.AddStrings(SQLConnectionProvider.GetStorageTableNames(AIncludeViews));
end;

procedure TdxEMFCustomDataProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Connection) then
    Connection := nil;
  inherited Notification(AComponent, Operation);
end;

function TdxEMFCustomDataProvider.ModifyData(const ADMLStatements: TArray<TdxModificationStatement>): TdxModificationResult;
begin
  CheckProvider;
  Result := SQLConnectionProvider.ModifyData(ADMLStatements);
end;

function TdxEMFCustomDataProvider.ModifyData(const ADMLStatements: array of TdxModificationStatement): TdxModificationResult;
begin
  CheckProvider;
  Result := SQLConnectionProvider.ModifyData(ADMLStatements);
end;

procedure TdxEMFCustomDataProvider.CheckProvider;
begin
  if Connection = nil then
    raise EArgumentNilException.CreateFmt(sdxConnectionIsNull, [Name]);
  if SQLConnectionProvider = nil then
    raise EdxUnknownProviderSQL.Create(sdxMissingProviderSQL);
end;

procedure TdxEMFCustomDataProvider.SetConnection(const Value: TCustomConnection);
begin
  if FConnection = Value then
    Exit;
  if FConnection <> nil then
    FConnection.RemoveFreeNotification(Self);
  FConnection := Value;
  if FConnection <> nil then
    FConnection.FreeNotification(Self);
  FConnectionVendor.Free;
  FConnectionVendor := CreateConnectionVendor;
  RecreateProviderSQL;
end;

procedure TdxEMFCustomDataProvider.SetOptions(const Value: TdxEMFDataProviderOptions);
begin
  FOptions.Assign(Value);
end;

function TdxEMFCustomDataProvider.GetActualDBEngineType: TdxDBEngine;
begin
  if FConnection = nil then
    Result := TdxDBEngines.Unknown
  else
  begin
    Result := Options.DBEngine;
    if ((Result = TdxDBEngines.Unknown) or (Result = TdxDBEngines.Empty)) and Options.AutoDetectDBEngine then
      Result := TdxCustomConnectionVendorAccess(ConnectionVendor).GetExpectedDBEngine;
  end;
end;

procedure TdxEMFCustomDataProvider.RecreateProviderSQL;
begin
  FreeAndNil(FSQLConnectionProvider);
  FSQLConnectionProvider := CreateProviderSQL(GetActualDBEngineType);
end;

{ TdxEMFCustomSession }

function TdxEMFCustomSession.FindObjectInCache(AClass: TClass; const AKeys: TArray<TValue>): TObject;
begin
  Result := FPoolClasses.FindObjectByKey(AClass, AKeys);
end;

procedure TdxEMFCustomSession.FlushChanges;
var
  AObjectsToFireSaved: TArray<TObject>;
begin
    AObjectsToFireSaved := BeginFlushChanges;
    TdxSessionStateStack.Enter(Self, TdxSessionState.CommitTransactionNonReentrant);
    try
      TdxSessionStateStack.Enter(Self, TdxSessionState.CommitChangesToDataLayer);
      try
        InternalFlushChanges;
      finally
        TdxSessionStateStack.Leave(Self, TdxSessionState.CommitChangesToDataLayer);
      end;
    finally
      TdxSessionStateStack.Leave(Self, TdxSessionState.CommitTransactionNonReentrant);
    end;
    EndFlushChanges(AObjectsToFireSaved);
end;

procedure TdxEMFCustomSession.FreeMarkedInsertedObjects;
var
  AObject: TObject;
begin
  TMonitor.Enter(FObjectsMarkedInserted);
  for AObject in FObjectsMarkedInserted do
    AObject.Free;
  FObjectsMarkedInserted.Clear;
  TMonitor.Exit(FObjectsMarkedInserted);
end;


function TdxEMFCustomSession.GetLoadedObjectByKey<T>(const AKey: TValue): T;
begin
  Result := T(GetLoadedObjectByKey(TClass(T), AKey));
end;

function TdxEMFCustomSession.GetObjects(const AQuery: IdxQueryable): IdxEMFCollection;
var
  ALinqQuery: IdxLinqQueryBuilder;
  ACriteria: IdxCriteriaOperator;
begin
  ALinqQuery := AQuery as IdxLinqQueryBuilder;
  ACriteria := ALinqQuery.WhereClause; //.AddReference as TdxCriteriaOperator;
  Result := GetObjects(ALinqQuery.FromClause.FromClass, ACriteria, ALinqQuery.OrderByClause,
    ALinqQuery.SkipSelectedRecords, ALinqQuery.TopSelectedRecords);
end;

function TdxEMFCustomSession.GetObjects(AClass: TClass; const ACriteria: IdxCriteriaOperator;
  const ASortByExpressions: IdxSortByExpressions; ASkipSelectedRecords, ATopSelectedRecords: Integer): IdxEMFCollection;
begin
  Result := GetObjects(AClass, ACriteria, ASortByExpressions, ASkipSelectedRecords, ATopSelectedRecords, False, False);
end;

function TdxEMFCustomSession.GetObjects<T>(const ACriteria: IdxCriteriaOperator;
  const ASortByExpressions: IdxSortByExpressions; ASkipSelectedRecords,
  ATopSelectedRecords: Integer): IdxEMFCollection<T>;
begin
  Result := GetObjects<T>(ACriteria, ASortByExpressions, ASkipSelectedRecords, ATopSelectedRecords, False, False);
end;

{ TdxEMFCustomSession.TCommitChangesContext }

constructor TdxEMFCustomSession.TCommitChangesContext.Create(ASession: TdxEMFCustomSession;
  ABatchWideData: TdxBatchWideDataHolderForModification);
begin
  FSession := ASession;
  FBatchWideData := ABatchWideData;
  FProcessingSave := TdxProcessingSave.Create(FSession, FBatchWideData);
end;

destructor TdxEMFCustomSession.TCommitChangesContext.Destroy;
begin
  FreeAndNil(FProcessingSave);
  FreeStatements;
  FreeResult;
  inherited Destroy;
end;

procedure TdxEMFCustomSession.TCommitChangesContext.FreeResult;
begin
  FreeAndNil(Result);
end;

procedure TdxEMFCustomSession.TCommitChangesContext.FreeStatements;
var
  AStatement: TdxModificationStatement;
begin
  for AStatement in Statements do
    AStatement.Free;
  Finalize(Statements);
end;

{ TdxEMFCustomSession.TdxQueryProvider<T> }

constructor TdxEMFCustomSession.TQueryProvider<T>.Create(ASession: TdxEMFCustomSession);
begin
  inherited Create;
  FSession := ASession;
end;

function TdxEMFCustomSession.TQueryProvider<T>.CreateQuery(const AExpression: IdxExpression): IdxQueryable;
begin
  Result := FSession.CreateQuery(AExpression);
end;

function TdxEMFCustomSession.TQueryProvider<T>.CreateQueryGeneric(const AExpression: IdxExpression): IdxQueryable<T>;
var
  ALinqQuery: IdxLinqQueryBuilder;
  ACriteria: IdxCriteriaOperator;
begin
  ALinqQuery := AExpression as IdxLinqQueryBuilder;
  ACriteria := ALinqQuery.WhereClause;
  Result := IdxQueryable<T>(FSession.GetObjects<T>(ACriteria, ALinqQuery.OrderByClause,
    ALinqQuery.SkipSelectedRecords, ALinqQuery.TopSelectedRecords));
end;

{ TdxEMFSession }

constructor TdxEMFSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TdxEMFSession.CreateSchema(AClass: TClass);
begin
  CheckConnection;
  DataProvider.UpdateSchema(EntityManager.GetEntityInfo(AClass));
end;

procedure TdxEMFSession.CreateSchema(const AClasses: array of TClass);
var
  AClass: TClass;
begin
  CheckConnection;
  DataProvider.BeginCreateTable;
  try
    for AClass in AClasses do
      CreateSchema(AClass);
  finally
    DataProvider.EndCreateTable;
  end;
end;




{ TdxDataSetLoader<TDataSet, TField> }

procedure TdxDataSetLoader<T, F>.BeginLoad;
begin
  inherited BeginLoad;
  CreateFieldsCache;
end;

function TdxDataSetLoader<T, F>.GetDataSet: T;
begin
  Result := T(inherited DataSet)
end;

class procedure TdxDataSetLoader<T, F>.RegisterReader<TC>(AProc: TProc<TC>);
begin
  FReaderFunctions.AddOrSetValue(TClass(TC), TProc(AProc));
end;

procedure TdxDataSetLoader<T, F>.SetDataSet(const Value: T);
begin
  inherited DataSet := Value;
end;

{ TdxDataSetLoader<T> }

procedure TdxDataSetLoader<T>.CreateFieldsCache;
var
  AFieldsCache: TList<TFieldCache>;
  AKeyFieldsCache: TList<TFieldCache>;
  AMemberInfo: TdxMappingMemberInfo;
  AFields: TArray<TField>;
  AFields1: TArray<TField>;
  I, AFieldIndex: Integer;
begin
  AFieldsCache := TList<TFieldCache>.Create;
  AKeyFieldsCache := TList<TFieldCache>.Create;
  try
    AFieldsCache.Capacity := EntityInfo.PersistentProperties.Count;
    AFieldIndex := 0;
    for I := 0 to EntityInfo.PersistentProperties.Count - 1 do
    begin
      AMemberInfo := EntityInfo.PersistentProperties{$IFDEF DELPHIXE3}.List{$ENDIF}[I];
      if AMemberInfo.IsDataMember then
        if not (AMemberInfo.IsAssociationList{ or AMemberInfo.IsAssociationObject}) then
        begin
          if not AMemberInfo.IsLoadable then
          begin
            AFields := nil;
            SetLength(AFields, Max(Length(AMemberInfo.SubMembers), 1))
          end
          else
            AFields := GetFields(AMemberInfo, AFieldIndex);

          AFieldsCache.Add(TFieldCache.Create(AMemberInfo, AFields));
          if AMemberInfo.IsKey then
            AKeyFieldsCache.Add(TFieldCache.Create(AMemberInfo, AFields));
        end;
    end;
    FFieldsCache := AFieldsCache.ToArray;
    FKeyFieldsCache := AKeyFieldsCache.ToArray;
  finally
    FreeAndNil(AFieldsCache);
    FreeAndNil(AKeyFieldsCache);
  end;
end;

function TdxDataSetLoader<T>.GetFields(AMemberInfo: TdxMappingMemberInfo; var AFieldIndex: Integer): TArray<TField>;
var
  I: Integer;
//  AFieldMemberInfo: TdxMappingMemberInfo;
begin
//  if AMemberInfo.IsReference then
//    AFieldMemberInfo := AMemberInfo.ReferenceType.KeyProperty.Member
//  else
//    AFieldMemberInfo := AMemberInfo;
  if Length(AMemberInfo.SubMembers) > 0 then
  begin
    SetLength(Result, Length(AMemberInfo.SubMembers));
    for I := 0 to Length(AMemberInfo.SubMembers) - 1 do
    begin
      Result[I] := DataSet.Fields[AFieldIndex];
      Inc(AFieldIndex);
    end
  end
  else
  begin
    Result := TArray<TField>.Create(DataSet.Fields[AFieldIndex]);
    Inc(AFieldIndex);
  end;
end;

procedure TdxDataSetLoader<T>.ReadCurrentRecord(out AValues: TArray<Variant>);
var
  I, J, K, AFieldCount: Integer;
begin
  SetLength(AValues, FieldCount);
  J := 0;
  for I := 0 to Length(FFieldsCache) - 1 do
    for J := 0 to Length(FFieldsCache[I].Value) - 1 do
    begin
      if FFieldsCache[I].Value[J] <> nil then
        AValues[I] := FFieldsCache[I].Value[J].Value
      else
        AValues[I] := FFieldsCache[I].Key.GetValue(nil).ToVariant;
    end;
end;

procedure TdxDataSetLoader<T>.ReadCurrentRecord(out AValues: TArray<TValue>);
var
  I, J, K, AFieldCount: Integer;
begin
  SetLength(AValues, FieldCount);
  J := 0;
  for I := 0 to Length(FFieldsCache) - 1 do
    for J := 0 to Length(FFieldsCache[I].Value) - 1 do
    begin
      if FFieldsCache[I].Value[J] <> nil then
        AValues[I] := TdxFieldValueConverter.ToValue(FFieldsCache[I].Value[J], FFieldsCache[I].Key)
      else
        AValues[I] := FFieldsCache[I].Key.GetValue(nil);
    end;
end;

function TdxDataSetLoader<T>.ReadCurrentRecordKeys: TArray<TValue>;
var
  I, J, K: Integer;
begin
  SetLength(Result, EntityInfo.KeyProperty.KeyFieldCount);
  K := 0;
  for I := 0 to Length(FKeyFieldsCache) - 1 do
    for J := 0 to Length(FKeyFieldsCache[I].Value) - 1 do
    begin
      Result[K] := TdxFieldValueConverter.ToValue(FKeyFieldsCache[I].Value[J], FKeyFieldsCache[I].Key);
      Inc(K);
    end;
end;

procedure TdxDataSetLoader<T>.ReadCurrentRecord(AInstance: Pointer);
var
  I: Integer;
  AAttributeField: TFieldCache;
  AKeys: TArray<TValue>;
  ARefObject: TObject;
  ASession: TdxEMFCustomSession;
begin
  ASession := Session as TdxEMFCustomSession;
  for I := 0 to Length(FFieldsCache) - 1 do
  begin
    AAttributeField := FFieldsCache[I];
    if not (AAttributeField.Key.IsWritable or AAttributeField.Key.IsSerialize) then
      Continue;
    if AAttributeField.Key.IsReference then
    begin
      AKeys := TdxFieldValueConverter.ToValues(AAttributeField.Value, AAttributeField.Key.ReferenceType.KeyProperty.Member);
      ARefObject := ASession.Find(AAttributeField.Key.ReferenceType.ClassAttributes.PersistentClass, AKeys);
      AAttributeField.Key.SetValue(AInstance, ARefObject);
    end
    else
    begin
      if AAttributeField.Value <> nil then
        TdxFieldValueConverter.SetValue(AInstance, AAttributeField.Value, AAttributeField.Key);
    end;
  end;
end;

end.

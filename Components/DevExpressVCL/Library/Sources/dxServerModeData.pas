{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxServerModeData;

{$I cxVer.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Variants, DB, dxCore, cxClasses, dxCoreClasses,
  cxFilter, cxDBFilter, cxCustomData, cxDataUtils, cxDataStorage, cxVariants,
  cxDB, cxDBData, dxServerModeClasses, dxServerModeHelpers, dxServerModeUtils;

type
  TdxServerModeCache = class;
  TdxServerModeCustomDataSource = class;
  TdxServerModeCriteria = class;
  TdxServerModeDataField = class;
  TdxServerModeDataControllerInfo = class;
  TdxServerModeDataController = class;
  TdxCustomServerModeDataProvider = class;
  TdxServerModeDescriptor = class;
  TdxServerModeDescriptors = class;
  TdxServerModeSummaryDescriptors = class;
  TdxServerModeSummaryDescriptor = class;
  TdxServerModeDataFocusingInfo = class;
  TdxServerModeParam = class;
  TdxServerModeParams = class;
  TdxServerModeCustomSQLQueryBuilder = class;
  TdxServerModeDataFindFilterCriteria = class;

  TdxNullsSortOrder = (nsoFirstIfDesc, nsoFirstIfAsc);

  IdxProviderSupport = {$IFDEF DELPHI17}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF};

  { TdxServerModeFindFilterCriteria }

  TdxServerModeFindFilterCriteria = class(TcxFindFilterCriteria)
  private
    function GetDataSource: TdxServerModeCustomDataSource;
    function GetFilter: TdxServerModeCriteria;
  protected
    function CreateFilter: TcxFilterCriteria; override;
    procedure SetEscapedFlagForFilterItems;

    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
    property Filter: TdxServerModeCriteria read GetFilter;
  public
    procedure AssignConditions(AFindFilter: TdxServerModeFindFilterCriteria);
    function IsEqual(AFindFilter: TdxServerModeFindFilterCriteria): Boolean;
  end;

  { TdxServerModeCriteriaItem }

  TdxServerModeCriteriaItem = class(TcxFilterCriteriaItem)
  private
    FDescriptor: TdxServerModeDescriptor;
    FEscapedValue: Boolean;
    function GetField: TField;
    function InternalGetCriteria: TdxServerModeCriteria;
  protected
    function GetFieldCaption: string; override;
    function GetFieldName: string; override;
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;
    procedure ReadData(AStream: TStream); override;
    procedure WriteData(AStream: TStream); override;

    property EscapedValue: Boolean read FEscapedValue write FEscapedValue;
  public
    function IsEmpty: Boolean; override;
    property Criteria: TdxServerModeCriteria read InternalGetCriteria;
    property Descriptor: TdxServerModeDescriptor read FDescriptor;
    property Field: TField read GetField;
  end;

  { TdxServerModeCriteriaItemList }

  TdxServerModeCriteriaItemList = class(TcxFilterCriteriaItemList)
  public
    procedure Assign(Source: TdxServerModeCriteriaItemList);
    procedure AssignItems(Source: TdxServerModeCriteriaItemList);
  end;

  { TdxServerModeCriteria }

  TdxServerModeCriteriaGetValueEvent = procedure (AItem: TcxFilterCriteriaItem; const AValue: Variant;
    var ADisplayValue: string) of object;
  TdxServerModeCriteriaGetFieldCaptionEvent = procedure (AItem: TdxServerModeCriteriaItem; var AFieldCaption: string) of object;

  TdxServerModeCriteria = class(TcxFilterCriteria)
  private
    FDataSource: TdxServerModeCustomDataSource;
    FSQLCalculating: Boolean;
    FOnGetFieldCaption: TdxServerModeCriteriaGetFieldCaptionEvent;
    FOnGetValue: TdxServerModeCriteriaGetValueEvent;
    function GetRoot: TdxServerModeCriteriaItemList;
  protected
    procedure DoGetFieldCaption(AItem: TdxServerModeCriteriaItem; var AFieldCaption: string);
    function FilterTextUsed: Boolean; override;
    procedure FormatFilterTextValue(AItem: TcxFilterCriteriaItem; const AValue: Variant;
      var ADisplayValue: string); override;
    function GetIDByItemLink(AItemLink: TObject): Integer; override;
    function GetItemLinkByID(AID: Integer): TObject; override;
    function GetItemClass: TcxFilterCriteriaItemClass; override;
    function GetItemListClass: TcxFilterCriteriaItemListClass; override;
    function GetRootClass: TcxFilterCriteriaItemListClass; override;
    procedure Operation(Value: TdxServerModeCriteria; AOperator: TcxFilterBoolOperatorKind; ADestroyValue: Boolean);

    property SQLCalculating: Boolean read FSQLCalculating write FSQLCalculating;
    property OnGetFieldCaption: TdxServerModeCriteriaGetFieldCaptionEvent read FOnGetFieldCaption write FOnGetFieldCaption;
    property OnGetValue: TdxServerModeCriteriaGetValueEvent read FOnGetValue write FOnGetValue;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); reintroduce; virtual;

    function AddItem(ADescriptor: TdxServerModeDescriptor; AOperatorKind: TcxFilterOperatorKind;
      const AValue: Variant): TdxServerModeCriteriaItem; reintroduce; overload; virtual;
    function AddItem(AParent: TcxFilterCriteriaItemList; ADescriptor: TdxServerModeDescriptor;
      AOperatorKind: TcxFilterOperatorKind; const AValue: Variant): TdxServerModeCriteriaItem; reintroduce; overload; virtual;
    procedure AndCriteria(Value: TdxServerModeCriteria; ADestroyValue: Boolean = False);
    procedure OrCriteria(Value: TdxServerModeCriteria; ADestroyValue: Boolean = False);
    function Clone: TdxServerModeCriteria;
    function IsEqual(AValue: TdxServerModeCriteria): Boolean;

    property DataSource: TdxServerModeCustomDataSource read FDataSource;
    property Root: TdxServerModeCriteriaItemList read GetRoot;
  end;
  TdxServerModeCriteriaClass = class of TdxServerModeCriteria;

  { TdxServerModeDescriptor }

  TdxServerModeDescriptorChangeType = (dctLink, dctSorting, dctSummaryKind);
  TdxServerModeDescriptorChanges = set of TdxServerModeDescriptorChangeType;

  TdxDateTimeGrouping = (dgDefault, dgByDateAndTime, dgRelativeToToday,
    dgByHour, dgByDate, dgByWeekDay, dgByMonth, dgByYear);

  TdxServerModeDescriptor = class
  private
    FChanges: TdxServerModeDescriptorChanges;
    FLinks: TdxFastList;
    FDateTimeGrouping: TdxDateTimeGrouping;
    FLockCount: Integer;
    FLinkedObject: TdxServerModeDescriptor;
    FOwner: TdxServerModeDescriptors;
    function GetLink(Index: Integer): TdxServerModeDescriptor;
    function GetLinkCount: Integer;
    function GetIndex: Integer;
    procedure SetDateTimeGrouping(Value: TdxDateTimeGrouping);
    procedure SetIndex(Value: Integer);
    procedure SetOwner(Value: TdxServerModeDescriptors);
    procedure SetLinkedObject(Value: TdxServerModeDescriptor);
  protected
    procedure Changed(AChanges: TdxServerModeDescriptorChanges = []); virtual;
    procedure DoAssign(Source: TdxServerModeDescriptor); virtual;
    function GetField: TField; virtual;

    procedure AddLink(ALink: TdxServerModeDescriptor);
    procedure ClearLinks;
    procedure RemoveLink(ALink: TdxServerModeDescriptor);

    procedure BeginUpdate;
    function IsLocked: Boolean;
    procedure EndUpdate;

    function IsDateTimeGrouping: Boolean;
    function IsSorting: Boolean; virtual;
    function IsSummary: Boolean; virtual;

    property LinkCount: Integer read GetLinkCount;
    property Links[Index: Integer]: TdxServerModeDescriptor read GetLink;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TdxServerModeDescriptor);
    function Clone: TdxServerModeDescriptor;

    function IsEqual(AValue: TdxServerModeDescriptor): Boolean; virtual;
    function ToString: string; override;

    property DateTimeGrouping: TdxDateTimeGrouping read FDateTimeGrouping write SetDateTimeGrouping;
    property Field: TField read GetField;
    property Index: Integer read GetIndex write SetIndex;
    property Owner: TdxServerModeDescriptors read FOwner write SetOwner;
    property LinkedObject: TdxServerModeDescriptor read FLinkedObject write SetLinkedObject;
  end;
  TdxServerModeDescriptorClass = class of TdxServerModeDescriptor;

  { TdxServerModeDescriptors }

  TdxServerModeDescriptorsChangedEvent = procedure (Sender: TdxServerModeDescriptors;
    AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges) of object;

  TdxServerModeDescriptors = class
  private
    FDataSource: TdxServerModeCustomDataSource;
    FList: TdxFastObjectList;
    FOnChanged: TdxServerModeDescriptorsChangedEvent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxServerModeDescriptor;
  protected
    procedure AddItem(AItem: TdxServerModeDescriptor);
    procedure RemoveItem(AItem: TdxServerModeDescriptor);

    procedure DoChanged(AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges); virtual;
    function GetDescriptorClass: TdxServerModeDescriptorClass; virtual;

    property List: TdxFastObjectList read FList;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource);
    destructor Destroy; override;

    procedure Assign(Source: TdxServerModeDescriptors);

    function Add(AField: TField): TdxServerModeDescriptor; overload; virtual;
    procedure AddItems(AList: TdxServerModeDescriptors);
    function CreateLink(ALinkedObject: TdxServerModeDescriptor): TdxServerModeDescriptor;
    procedure CreateLinks(AList: TdxServerModeDescriptors);

    procedure Clear;
    function Clone: TdxServerModeDescriptors;
    function Contains(AField: TField): Boolean;
    function IndexOf(AField: TField): Integer; overload;
    function IndexOf(AItem: TdxServerModeDescriptor): Integer; overload;
    function IsEmpty: Boolean;
    function IsEqual(AValue: TdxServerModeDescriptors): Boolean; virtual;
    procedure Remove(AField: TField); overload;

    property Count: Integer read GetCount;
    property DataSource: TdxServerModeCustomDataSource read FDataSource;
    property Items[Index: Integer]: TdxServerModeDescriptor read GetItem; default;
    property OnChanged: TdxServerModeDescriptorsChangedEvent read FOnChanged write FOnChanged;
  end;
  TdxServerModeDescriptorsClass = class of TdxServerModeDescriptors;

  { TdxServerModeSummaryDescriptor }

  TdxServerModeSummaryDescriptor = class(TdxServerModeDescriptor)
  private
    FGroupLevel: Integer;
    FKind: TcxSummaryKind;
    FSorted: Boolean;
    procedure SetGroupLevel(Value: Integer);
    procedure SetKind(Value: TcxSummaryKind);
    procedure SetSorted(Value: Boolean);
  protected
    procedure DoAssign(Source: TdxServerModeDescriptor); override;
    function IsSummary: Boolean; override;
  public
    constructor Create; overload; override;
    constructor Create(AKind: TcxSummaryKind); reintroduce; overload;
    function IsEqual(AValue: TdxServerModeDescriptor): Boolean; override;

    property GroupLevel: Integer read FGroupLevel write SetGroupLevel;
    property Kind: TcxSummaryKind read FKind write SetKind;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  { TdxServerModeSummaryDescriptors }

  TdxServerModeSummaryDescriptors = class(TdxServerModeDescriptors)
  private
    function GetItem(Index: Integer): TdxServerModeSummaryDescriptor;
  protected
    function GetDescriptorClass: TdxServerModeDescriptorClass; override;
  public
    function Add(AField: TField; AKind: TcxSummaryKind): TdxServerModeSummaryDescriptor; overload; virtual;
    function Add(const AFieldName: string; AKind: TcxSummaryKind): TdxServerModeSummaryDescriptor; overload; virtual;

    function Clone: TdxServerModeSummaryDescriptors;
    function IsGroupLevelSorted(AGroupLevel: Integer): Boolean;

    property Items[Index: Integer]: TdxServerModeSummaryDescriptor read GetItem; default;
  end;
  TdxServerModeSummaryDescriptorsClass = class of TdxServerModeSummaryDescriptors;

  { TdxServerModeGroupInfoDescriptor }

  TdxServerModeGroupInfoDescriptor = class(TdxServerModeDescriptor)
  public
    function IsEqual(AValue: TdxServerModeDescriptor): Boolean; override;
  end;

  { TdxServerModeGroupInfoDescriptors }

  TdxServerModeGroupInfoDescriptors = class(TdxServerModeDescriptors)
  private
    function GetItem(Index: Integer): TdxServerModeGroupInfoDescriptor;
  public
    property Items[Index: Integer]: TdxServerModeGroupInfoDescriptor read GetItem; default;
  end;
  TdxServerModeGroupInfoDescriptorsClass = class of TdxServerModeGroupInfoDescriptors;

  { TdxServerModeSortInfoDescriptor }

  TdxServerModeSortInfoDescriptor = class(TdxServerModeGroupInfoDescriptor)
  private
    FIsDesc: Boolean;
    FIsKey: Boolean;
    function GetIsDesc: Boolean;
    procedure SetIsDesc(Value: Boolean);
  protected
    procedure CheckGroupValue(var AValue: Variant; AIsGroupValue: Boolean);
    procedure DoAssign(Source: TdxServerModeDescriptor); override;
    function IsSorting: Boolean; override;
  public
    constructor Create(AIsDesc, AIsKey: Boolean); reintroduce;

    function IsEqual(AValue: TdxServerModeDescriptor): Boolean; override;

    property IsDesc: Boolean read GetIsDesc write SetIsDesc;
    property IsKey: Boolean read FIsKey;
  end;

  { TdxServerModeSortInfoDescriptors }

  TdxServerModeSortInfoDescriptors = class(TdxServerModeGroupInfoDescriptors)
  private
    function GetItem(Index: Integer): TdxServerModeSortInfoDescriptor;
    function GetWithoutKeySortInfoCount: Integer;
  protected
    function Add(AField: TField; AIsDesc: Boolean; AIsKey: Boolean): TdxServerModeSortInfoDescriptor; overload;
    function AddKeySortInfo(AField: TField; AIsDesc: Boolean): TdxServerModeSortInfoDescriptor;
    function GetDescriptorClass: TdxServerModeDescriptorClass; override;
  public
    function Add(AField: TField; AIsDesc: Boolean): TdxServerModeSortInfoDescriptor; overload;
    function Add(const AFieldName: string; AIsDesc: Boolean): TdxServerModeSortInfoDescriptor; overload;
    procedure ClearKeySortInfo;
    function Clone(AReversed: Boolean): TdxServerModeSortInfoDescriptors;

    property Items[Index: Integer]: TdxServerModeSortInfoDescriptor read GetItem; default;
    property WithoutKeySortInfoCount: Integer read GetWithoutKeySortInfoCount;
  end;
  TdxServerModeSortInfoDescriptorsClass = class of TdxServerModeSortInfoDescriptors;

  { TdxServerModeFieldDescriptor }

  TdxServerModeFieldDescriptor = class(TdxServerModeDescriptor)
  private
    FField: TField;
  protected
    procedure DoAssign(Source: TdxServerModeDescriptor); override;
    function GetField: TField; override;
  end;

  { TdxServerModeFieldDescriptors }

  TdxServerModeFieldDescriptors = class(TdxServerModeDescriptors)
  private
    function GetItem(Index: Integer): TdxServerModeFieldDescriptor;
  protected
    function GetDescriptorClass: TdxServerModeDescriptorClass; override;
  public
    function Add(AField: TField): TdxServerModeDescriptor; override;
    function FieldByName(const AFieldName: string): TField;
    function IndexByName(const AFieldName: string): Integer;
    function ItemByField(AField: TField): TdxServerModeDescriptor;

    property Items[Index: Integer]: TdxServerModeFieldDescriptor read GetItem; default;
  end;

  { TdxServerModeStateInfo }

  TdxServerModeStateInfo = class
  private
    FSortInfo: TdxServerModeSortInfoDescriptors;
    FGroupCount: Integer;
  public
    constructor Create(AGroupCount: Integer; ASortInfo: TdxServerModeSortInfoDescriptors);
    destructor Destroy; override;
    function GetMaxEqualDepth(ACache: TdxServerModeCache): Integer;

    property GroupCount: Integer read FGroupCount;
    property SortInfo: TdxServerModeSortInfoDescriptors read FSortInfo;
  end;

  { TdxServerModeCache }

  TdxServerModeInconsistencyDetectedEvent = procedure(Sender: TdxServerModeCache; const AMessage: string) of object;
  TdxServerModeExceptionThrownEvent = procedure(Sender: TdxServerModeCache; AException: Exception) of object;

  TdxServerModeCacheState = (smcsConsistent, smcsInconsistent, smcsFatal);

  TdxServerModeCache = class
  private
    FDataSource: TdxServerModeCustomDataSource;
    FLastErrorMessage: string;
    FState: TdxServerModeCacheState;
    FTopGroupInfo: TdxServerModeGroupInfo;
    FOnExceptionThrown: TdxServerModeExceptionThrownEvent;
    FOnInconsistencyDetected: TdxServerModeInconsistencyDetectedEvent;

    procedure CalculateTotalSummaries(AGroupInfo: TdxServerModeGroupInfo);
    procedure CheckTopGroupInfo(APrevStateInfo: TdxServerModeStateInfo);
    function CreateTopGroupInfo: TdxServerModeGroupInfo;
    procedure FillChildrenIfNeeded(AGroupInfo: TdxServerModeGroupInfo);
    function GetFilter: TdxServerModeCriteria;
    function GetGroupCount: Integer;
    function GetIsLanded: Boolean;
    function GetKeyFields: TdxServerModeFieldDescriptors;
    function GetSortInfo: TdxServerModeSortInfoDescriptors;
    function GetSummaries: TdxServerModeSummaryDescriptors;
    function GetTotalSummaries: TdxServerModeSummaryDescriptors;
    function PrepareTopGroupInfoWithTrick(ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData;
  protected
    function CanTrickCreateTopGroupFromNextGroups: Boolean;
    function EvaluateOnInstance(const ARow: TdxServerModeRow; AField: TField): Variant; virtual;
    procedure Fatal(AException: Exception); virtual;
    function GetCount(ACriteria: TdxServerModeCriteria): Integer; virtual;
    function GetGroupWhere(AGroupInfo: TdxServerModeGroupInfo): TdxServerModeCriteria;
    function GetInternalCount: Integer; virtual;
    function GetTopGroupInfo: TdxServerModeGroupInfo;
    procedure MakeStrongClause(ACriteria: TdxServerModeCriteria; AParentList: TcxFilterCriteriaItemList;
      ADescriptor: TdxServerModeDescriptor; AIsDesc, ACanEqual: Boolean; const AValue: Variant); overload;
    procedure MakeStrongClause(ACriteria: TdxServerModeCriteria; AParentList: TcxFilterCriteriaItemList;
      ASortInfoDescriptor: TdxServerModeSortInfoDescriptor; const AValue: Variant); overload;
    function PrepareChildren(AGroupWhere: TdxServerModeCriteria; AGroupByDescriptor: TdxServerModeSortInfoDescriptor;
      ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoDataList; virtual;
    function PrepareTopGroupCount: Integer; virtual;
    function PrepareTopGroupInfo(ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData; virtual;
    procedure RaiseInconsistencyDetected(const AFormat: string; Args: array of const); overload;
    procedure RaiseInconsistencyDetected(const AMessage: string); overload;
    procedure ResetTopGroupInfo;
    property IsLanded: Boolean read GetIsLanded;
    property LastErrorMessage: string read FLastErrorMessage;
    property State: TdxServerModeCacheState read FState;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ClearGroupInfo;
    procedure ClearTopGroupInfo;

    function GetGroupInfoList(AParentGroup: TdxServerModeGroupInfo): TdxServerModeGroupInfoList;
    function GetTotalSummary: TdxServerModeSummaryList;
    function IsNothingButCount(ASummaries: TdxServerModeSummaryDescriptors): Boolean;
    procedure RecalculateSummaries;
    procedure SoftReset; virtual;

    function Contains(const AValue: TdxServerModeRow): Boolean; virtual; abstract;
    function GetKeyFromRow(const ARow: TdxServerModeRow): Variant; virtual;
    function GetRowIndexByKey(const AKey: Variant): Integer; virtual; abstract;
    function GetRowKey(AIndex: Integer): Variant; virtual; abstract;
    function Indexer(ARecordIndex: Integer): TdxServerModeRow; virtual; abstract;
    function IndexOf(const AValue: TdxServerModeRow): Integer; virtual; abstract;

    property Count: Integer read GetInternalCount;
    property DataSource: TdxServerModeCustomDataSource read FDataSource;
    property Filter: TdxServerModeCriteria read GetFilter;
    property GroupCount: Integer read GetGroupCount;
    property KeyFields: TdxServerModeFieldDescriptors read GetKeyFields;
    property SortInfo: TdxServerModeSortInfoDescriptors read GetSortInfo;
    property Summaries: TdxServerModeSummaryDescriptors read GetSummaries;
    property TopGroupInfo: TdxServerModeGroupInfo read GetTopGroupInfo;
    property TotalSummaries: TdxServerModeSummaryDescriptors read GetTotalSummaries;
    property OnExceptionThrown: TdxServerModeExceptionThrownEvent
      read FOnExceptionThrown write FOnExceptionThrown;
    property OnInconsistencyDetected: TdxServerModeInconsistencyDetectedEvent
      read FOnInconsistencyDetected write FOnInconsistencyDetected;
  end;
  TdxServerModeCacheClass = class of TdxServerModeCache;

  { TdxServerModeKeyedCache }

  TdxServerModeKeyedCache = class(TdxServerModeCache)
  private
    FKeysByIndex: TdxServerModeKeysDictionary;
    FLastUsedRecordIndex: Integer;
    FLastUsedRow: TdxServerModeRow;
    FRegressor: TdxServerModeServerAndChannelModelBuilder;
    FRowsByIndex: TdxServerModeRowsDictionary;
    FSafeModel: TdxServerModeServerAndChannelModel;

    ActualFetchRowsIsGood: TdxNullableBoolean;
    UseTakeEnforcer: TdxNullableBoolean;
    procedure FetchRowsAll(AGroupInfo: TdxServerModeGroupInfo);
    procedure FetchRowsTop(AGroupInfo: TdxServerModeGroupInfo; AIsFromBottom: Boolean; ATop: Integer);
    function FindFromCache(ARecordIndex: Integer; var ARow: TdxServerModeRow): Boolean;
    function GetCountBeforeRow(const ARow: TdxServerModeRow; AGroupInfo: TdxServerModeGroupInfo;
      ASortInfo: TdxServerModeSortInfoDescriptors): Integer;
    function GetGroupForKeysFetchingAround(Index: Integer): TdxServerModeGroupInfo;
    function GetMagicNumberAllowedSlowerThenBase: Double;
    function GetMagicNumberAllowedSlowerThenBaseVariance: Double;
    function GetMagicNumberFetchKeysTopPenaltyGap: Integer;
    function GetMagicNumberTakeKeysBase: Integer;
    function GetMaxInSize: Integer; virtual;
    procedure CalculateModel(ARecordIndex, APositionInGroup: Integer; AGroupInfo: TdxServerModeGroupInfo;
      out ASkipTake, ASkipSkip: Integer; out ASkipIsFromBottom: Boolean);
    procedure CalculateTake(AGroupInfo: TdxServerModeGroupInfo; AFetchCount: Integer; AIsFromBottom: Boolean;
      out APureTake: Integer; out APureTakeIsFromBottom: Boolean);
    procedure DoFetchKeys(AGroupInfo: TdxServerModeGroupInfo; AIsFromBottom: Boolean; ASkip, ATake: Integer);
    procedure DoFetchKeysSkipWithTakeBackup(AGroupInfo: TdxServerModeGroupInfo; ASkipIsFromBottom: Boolean;
      ASkipSkip, ASkipTake: Integer; APureTakeIsFromBottom: Boolean; APureTake: Integer);
    procedure FetchKeysTimed(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer);
    procedure FillKeys(AGroupInfo: TdxServerModeGroupInfo; AIsFromBottom: Boolean; ASkip, ATake: Integer;
      AKeys: TdxServerModeKeyList);
    function GetGlobalIndex(AGroupInfo: TdxServerModeGroupInfo; APos: Integer; AIsReversed: Boolean): Integer;
    procedure PopulateDownKeysToFetch(AKeysToFetch: TdxServerModeKeyList;
      AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; ARecordIndex: Integer; ASplitLen: Integer;
      out AFillFromSplit: Boolean);
    procedure PopulateFetchKeys(AGroupInfo: TdxServerModeGroupInfo; APureTake, ASkipSkip, ASkipTake: Integer;
      ASkipIsFromBottom, APureTakeIsFromBottom: Boolean);
    procedure PopulateUpKeysToFetch(AKeysToFetch: TdxServerModeKeyList;
      AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; ARecordIndex: Integer; AInSize: Integer);
    function PopulateRowsByIndex(const ARows: TdxServerModeRowList;
      AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; AReturnRecordIndex: Integer): TdxServerModeRow;
  protected
    function DecideIsFetchRowsGoodIdeaForSure: Boolean; virtual;
    procedure FetchRowsByKeys(ARows: TdxServerModeRowList; AKeys: TdxServerModeKeyList);
    function GetMagicNumberFetchKeysAllThreshold: Integer; virtual;
    function GetMagicNumberFetchKeysModulo: Integer; virtual;
    function GetMagicNumberFetchRowsAllThreshold: Integer; virtual;
    function GetMagicNumberFetchRowsInSize: Integer; virtual;
    function GetMagicNumberFetchRowsTop: Integer; virtual;
    function GetMagicNumberFetchRowsTopThreshold: Integer; virtual;
    function GetMagicNumberScanKeysBase: Integer; virtual;
    function GetMagicNumberTakeIncreaseStepMultiplier: Double; virtual;
    function GetMagicNumberTakeKeysUpperLimitAfterSkip: Integer; virtual;
    function IsFetchRowsGoodIdeaForSure: Boolean;
    function ResolveKeyType(ASingleKeyToResolve: TdxServerModeFieldDescriptors): TdxKeyType; virtual; abstract;
    function ResolveRowType: TdxRowType; virtual; abstract;

    procedure FetchKeys(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer); virtual;
    procedure FetchRows(ARows: TdxServerModeRowList; AWhere: TdxServerModeCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; ATake: Integer); overload; virtual;
    procedure FetchRows(AWhere: TdxServerModeCriteria; ATake, AValidateCount, AFirstRecord: Integer;
      AIsFromBottom: Boolean); overload;

    function FetchRowByKey(const AKey: Variant): TdxServerModeRow;
    function GetRowGroup(const ARow: TdxServerModeRow): TdxServerModeGroupInfo;
    function PrepareTopGroupCount: Integer; override;

    property MagicNumberAllowedSlowerThenBase: Double read GetMagicNumberAllowedSlowerThenBase;
    property MagicNumberAllowedSlowerThenBaseVariance: Double read GetMagicNumberAllowedSlowerThenBaseVariance;
    property MagicNumberFetchKeysAllThreshold: Integer read GetMagicNumberFetchKeysAllThreshold;
    property MagicNumberFetchKeysModulo: Integer read GetMagicNumberFetchKeysModulo;
    property MagicNumberFetchKeysTopPenaltyGap: Integer read GetMagicNumberFetchKeysTopPenaltyGap;
    property MagicNumberFetchRowsAllThreshold: Integer read GetMagicNumberFetchRowsAllThreshold;
    property MagicNumberFetchRowsInSize: Integer read GetMagicNumberFetchRowsInSize;
    property MagicNumberFetchRowsTop: Integer read GetMagicNumberFetchRowsTop;
    property MagicNumberFetchRowsTopThreshold: Integer read GetMagicNumberFetchRowsTopThreshold;
    property MagicNumberScanKeysBase: Integer read GetMagicNumberScanKeysBase;
    property MagicNumberTakeIncreaseStepMultiplier: Double read GetMagicNumberTakeIncreaseStepMultiplier;
    property MagicNumberTakeKeysBase: Integer read GetMagicNumberTakeKeysBase;
    property MagicNumberTakeKeysUpperLimitAfterSkip: Integer read GetMagicNumberTakeKeysUpperLimitAfterSkip;
    property MaxInSize: Integer read GetMaxInSize;

    property Regressor: TdxServerModeServerAndChannelModelBuilder read FRegressor;
    property SafeModel: TdxServerModeServerAndChannelModel read FSafeModel;
    property KeysByIndex: TdxServerModeKeysDictionary read FKeysByIndex;
    property RowsByIndex: TdxServerModeRowsDictionary read FRowsByIndex;
  public
    FetchRowsIsGood: TdxNullableBoolean;
    ForceSkip: TdxNullableBoolean;
    ForceTake: TdxNullableBoolean;
    MinimizeSingleRequestTransferSizeInsteadOfOverallOptimization: Boolean;

    constructor Create(ADataSource: TdxServerModeCustomDataSource); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SoftReset; override;

    function DecideIsFetchRowsGoodIdea(ASortInfo: TdxServerModeSortInfoDescriptors): Boolean;
    function GetRowKey(ARecordIndex: Integer): Variant; override;
    function Indexer(ARecordIndex: Integer): TdxServerModeRow; override;
    function Contains(const AValue: TdxServerModeRow): Boolean; override;
    function GetRowIndexByKey(const AKey: Variant): Integer; override;
    function IndexOf(const AValue: TdxServerModeRow): Integer; override;
    function KeyEquals(const AKey1, AKey2: Variant): Boolean;
  end;

  { TdxServerModeCustomSQLAdapter }

  TdxServerModeDataSourceChangeType = (ctConnection, ctCache, ctGroups, ctOptions, ctSummaries,
    ctTotalSummaries, ctSortingSummaries, ctFiltering, ctSorting);
  TdxServerModeDataSourceChanges = set of TdxServerModeDataSourceChangeType;

  TdxSQLDatePart = (dpYear, dpQuarter, dpMonth, dpDayOfYear, dpDay, dpWeekDay, dpWeek, dpHour,
    dpMinute, dpSecond, dpMillisecond, dpMicrosecond, dpNanosecond);

  TdxServerModeCustomSQLAdapter = class(TcxInterfacedPersistent)
  private
    FDataSource: TdxServerModeCustomDataSource;
  protected
    procedure Changed(const AChanges: TdxServerModeDataSourceChanges = [ctOptions]);
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); virtual;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); virtual;
    procedure CorrectCommandText(AParams: TdxServerModeParams; var ACommandText: string); virtual;
    function CreateParam(AParams: TdxServerModeParams; const AValue: Variant; const ADataType: TFieldType;
      out ADisplayValue: string): TdxServerModeParam;
    function GetParamCheck: Boolean; virtual;
    function GetParamFakeNameFormat: string; virtual;
    function GetParamFormat: string; virtual;
    function GetParamNameFormat: string; virtual;
    function GetParamServerFormat: string; virtual;
    function IsMicrosecondSupported: Boolean; virtual;
    function IsMillisecondSupported: Boolean; virtual;
    function IsSkipClauseSupported: Boolean; virtual;
    function GetNullsSortOrder: TdxNullsSortOrder; virtual;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); virtual;
    procedure SortParams(AParams: TdxServerModeParams; var ACommandText: string); virtual;

    function CanUseParam(AParam: TdxServerModeParam): Boolean; virtual;
    function CanIdentifyInsertingRow: Boolean; virtual;
    function CanUseResultFieldName: Boolean; virtual;
    function CanUseGroupingByDateRangeParams: Boolean; virtual;
    function ConvertBoolToStr(const AValue: Variant): string; virtual;
    procedure CheckFilterFieldCaption(var AFieldCaption: string; AOperatorKind: TcxFilterOperatorKind); virtual;
    function DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; virtual;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; virtual;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; virtual;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; virtual;
    function DateTimeToString(const ADateTime: TDateTime): string; virtual;
    function GetCastAsDateFormat: string; virtual;
    function GetCastAsDateTimeFormat: string; virtual;
    function GetCastAsTimeFormat: string; virtual;
    function GetDatePart(APart: TdxSQLDatePart): string; virtual;
    function GetDateTimeFormat: string; virtual;
    function GetDateTimeGroupingResultFieldFormat: string;
    function GetDateTimeGroupingResultFieldName(AField: TField): string;
    function IsDateRangeGroupingSupported: Boolean; virtual;
    function IsNull(const AExpression: string): string; virtual;
    function NeedCastGroupingByDateRange: Boolean; virtual;
    function TruncGroupingDate(const ADate: string; ADateTimeGrouping: TdxDateTimeGrouping): string; virtual;

    function GetCaseOperatorFormat(AKind: TcxFilterOperatorKind): string;
    function GetCaseFullFormat: string; virtual;

    function GetEscapeTextForParamName: string; virtual;
    function GetPercentWildcard: Char; virtual;
    function GetUnderscoreWildcard: Char; virtual;
    procedure EscapeFakeParamName(AParam: TdxServerModeParam; ABuilder: TdxServerModeCustomSQLQueryBuilder); virtual;
    procedure EscapeLikeValue(AParam: TdxServerModeParam; APercentWildcard, AUnderscoreWildcard: Char);
    procedure GetLikeValue(ABuilder: TdxServerModeCustomSQLQueryBuilder; AParam: TdxServerModeParam; APercentWildcard,
      AUnderscoreWildcard: Char); virtual;
    function MakeEscapedValue(const AValue: string; AEscapePercentWildcard, AEscapeUnderscoreWildcard: Boolean): string; virtual;
    function NeedEscapeFakeParamName(AParam: TdxServerModeParam): Boolean; virtual;

    function GetFieldsRetrieveQueryFormat: string; virtual;
    function GetFieldName(AField: TField): string; virtual;
    function GetFieldNameFormat: string; virtual;
    function GetFromQueryFormat: string; virtual;
    function GetInsertingRowOutKey(const AKey: string): string; virtual;
    function GetOrderFormat(AIsDesc: Boolean): string; virtual;
    function GetServerFieldName(AField: TField): string;
    function GetSummaryFormat(AKind: TcxSummaryKind): string; virtual;
    function GetTableNameFormat: string; virtual;
    function GetQuoteChar: string; virtual;
    function QuotedString(const AParamValue: string): string; virtual;

    function GetDeleteSQLString(const AFrom, AWhere: string): string; virtual;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; virtual;
    function GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string; virtual;
    function GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string; virtual;
    function GetQueryFormat: string; virtual;

    property ParamCheck: Boolean read GetParamCheck;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); reintroduce; virtual;

    class procedure Register;
    class procedure Unregister;

    class function GetDisplayName: string; virtual;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; virtual;
    function GetSQLQuerySchemaNames: string; virtual;

    property DataSource: TdxServerModeCustomDataSource read FDataSource;
  end;
  TdxServerModeCustomSQLAdapterClass = class of TdxServerModeCustomSQLAdapter;

  TdxServerModeParamPosition = (ppCriteria, ppFields, ppGroupingByDateRange, ppSortInfo);
  TdxServerModeParamPositions = set of TdxServerModeParamPosition;

  { TdxServerModeParam }

  TdxServerModeParam = class(TParam)
  private
    FEscapedValue: Boolean;
    FPosition: TdxServerModeParamPositions;
    FCriteriaOperatorKind: TcxFilterOperatorKind;
  public
    property EscapedValue: Boolean read FEscapedValue write FEscapedValue;
    property CriteriaOperatorKind: TcxFilterOperatorKind read FCriteriaOperatorKind write FCriteriaOperatorKind;
    property Position: TdxServerModeParamPositions read FPosition write FPosition;
  end;

  TdxServerModeParamClass = class of TdxServerModeParam;

  { TdxServerModeParams }

  TdxServerModeParams = class(TParams)
  private
    FOwner: TdxServerModeCustomSQLQueryBuilder;
    function GetItem(Index: Integer): TdxServerModeParam;
    procedure SetItem(Index: Integer; const Value: TdxServerModeParam);
  protected
    function GetParamClass: TParamClass; override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TdxServerModeCustomSQLQueryBuilder; AParamClass: TdxServerModeParamClass);

    function AddParameter: TdxServerModeParam;
    procedure AddParam(Value: TdxServerModeParam);
    function CreateParam(AFieldType: TFieldType; const ParamName: string; ParamType: TParamType): TdxServerModeParam;
    function ParamByName(const Value: string): TdxServerModeParam;
    function FindParam(const Value: string): TdxServerModeParam;

    property Items[Index: Integer]: TdxServerModeParam read GetItem write SetItem; default;
  end;

  { TdxServerModeCustomSQLQueryBuilder }

  TdxServerModeCustomQueryType = (qtSelect, qtInsert, qtUpdate, qtDelete);

  TdxServerModeCustomSQLQueryBuilder = class(TPersistent)
  private
    FCommandText: string;
    FCriteria: TdxServerModeCriteria;
    FDataSource: TdxServerModeCustomDataSource;
    FDistinct: Boolean;
    FFields: TdxServerModeDescriptors;
    FFieldsCalculating: Boolean;
    FGroupInfo: TdxServerModeGroupInfoDescriptors;
    FGroupingByDateRangeCalculating: Boolean;
    FParams: TdxServerModeParams;
    FSortInfo: TdxServerModeSortInfoDescriptors;
    FSortInfoCalculating: Boolean;
    FSummaries: TdxServerModeSummaryDescriptors;
    FSkipRecords: Integer;
    FQueryType: TdxServerModeCustomQueryType;
    FTopRecords: Integer;
    FValues: TdxServerModeRow;
    procedure CheckParams;
    function GetAdapter: TdxServerModeCustomSQLAdapter;
    procedure GetFilterFieldCaptionHandler(AItem: TdxServerModeCriteriaItem; var AFieldCaption: string);
    procedure GetFilterValueHandler(AItem: TcxFilterCriteriaItem; const AValue: Variant;
      var ADisplayValue: string);
    procedure SetCriteria(Value: TdxServerModeCriteria);
    function GetCriteriaCalculating: Boolean;
  protected
    function CanUseParam(AParam: TdxServerModeParam): Boolean; virtual;
    function CanDateTimeGrouping(ADescriptor: TdxServerModeDescriptor): Boolean;
    function CanSorting(ADescriptor: TdxServerModeDescriptor): Boolean;
    function CanUseParams: Boolean; virtual;
    function CreateParam(const ADataType: TFieldType; const AValue: Variant;
      out ADisplayValue: string): TdxServerModeParam; virtual;
    function DoCreateParam(const ADataType: TFieldType; const AValue: Variant;
      out ADisplayValue: string): TdxServerModeParam; virtual;
    function GetParamCheck: Boolean; virtual;
    function GetParamValueText(AParam: TdxServerModeParam): string; virtual;
    procedure ReplaceFakeParamNameWithText(AParam: TdxServerModeParam; const AValue: string);
    procedure ValidateCommandTextWithParams;

    function CreateDateTimeParam(const ADateTime: TDateTime): string;
    function DateTimeGroupingByRelativeToToday(const AFieldName: string): string;
    function GetGroupingStartDate(ADateTimeGrouping: TdxDateTimeGrouping): string;
    function GroupingByDateRange(const AFieldName: string; ADescriptor: TdxServerModeDescriptor): string;

    function GetFieldsSQLString: string; virtual;
    function GetFromSQLString: string; virtual;
    function GetGroupSQLString: string; virtual;
    function GetInsertingRowOutKeys: string; virtual;
    function GetSortInfoSQLString: string; virtual;
    function GetWhereSQLString: string; virtual;
    function GetValuesSQLString: string; virtual;

    function DescriptorToString(ADescriptor: TdxServerModeDescriptor): string;
    function DescriptorsToString(ADescriptors: TdxServerModeDescriptors): string;

    function GetSQLString: string; virtual;

    procedure CalculateQueryType;

    property Adapter: TdxServerModeCustomSQLAdapter read GetAdapter;
    property ParamCheck: Boolean read GetParamCheck;
    property QueryType: TdxServerModeCustomQueryType read FQueryType;

    property CriteriaCalculating: Boolean read GetCriteriaCalculating;
    property FieldsCalculating: Boolean read FFieldsCalculating;
    property GroupingByDateRangeCalculating: Boolean read FGroupingByDateRangeCalculating;
    property SortInfoCalculating: Boolean read FSortInfoCalculating;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); virtual;
    destructor Destroy; override;

    procedure Calculate;

    property CommandText: string read FCommandText;
    property Params: TdxServerModeParams read FParams;

    property Criteria: TdxServerModeCriteria read FCriteria write SetCriteria;
    property DataSource: TdxServerModeCustomDataSource read FDataSource;
    property Distinct: Boolean read FDistinct write FDistinct;
    property Fields: TdxServerModeDescriptors read FFields write FFields;
    property GroupInfo: TdxServerModeGroupInfoDescriptors read FGroupInfo write FGroupInfo;
    property SortInfo: TdxServerModeSortInfoDescriptors read FSortInfo write FSortInfo;
    property SkipRecords: Integer read FSkipRecords write FSkipRecords;
    property Summaries: TdxServerModeSummaryDescriptors read FSummaries write FSummaries;
    property TopRecords: Integer read FTopRecords write FTopRecords;
    property Values: TdxServerModeRow read FValues write FValues;
  end;
  TdxServerModeCustomSQLQueryBuilderClass = class of TdxServerModeCustomSQLQueryBuilder;

  { TdxServerModeDataSourceOptions }

  TdxServerModeDataSourceOptions = class(TPersistent)
  private
    FOwner: TdxServerModeCustomDataSource;
    FUseQueryParams: Boolean;
    procedure ReadSQLAdapter(Reader: TReader);
  protected
    procedure Changed(const AChanges: TdxServerModeDataSourceChanges = [ctOptions]); virtual;
    procedure CheckInactive;
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TdxServerModeCustomDataSource);
    procedure Assign(Source: TPersistent); override;
    function GetSchemaName: string; virtual;

    property Owner: TdxServerModeCustomDataSource read FOwner;
  published
    property UseQueryParams: Boolean read FUseQueryParams write FUseQueryParams default True;
  end;
  TdxServerModeDataSourceOptionsClass = class of TdxServerModeDataSourceOptions;

  TdxDataSetClass = class of TDataSet;

  TdxServerModeGetSQLAdapterClassEvent = procedure (Sender: TObject; var AClass: TdxServerModeCustomSQLAdapterClass) of object;
  TdxServerModeDataSourceNotifyEvent = procedure(Sender: TdxServerModeCustomDataSource) of object;
  TdxServerModeDataSourceFatalErrorEvent = procedure(Sender: TdxServerModeCustomDataSource; const AMessage: string) of object;
  TdxServerModeDataSourceInconsistentCacheEvent = procedure(Sender: TdxServerModeCustomDataSource; const AMessage: string; var ARecoverCache: Boolean) of object;

  { TdxServerModeDataSourceCustomHelper }

  PDataSet = ^TDataSet;

  TdxServerModeDataSourceCustomHelper = class
  private
    FDataSource: TdxServerModeCustomDataSource;
    function GetConnection: TCustomConnection;
    function GetDataSet: TDataSet;
  protected
    function GetDataSetClass: TdxDataSetClass; virtual; abstract;
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass; virtual;
    function GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass; virtual;

    procedure CheckParamValue(var AParam: TdxServerModeParam); virtual;
    procedure CheckSQLAdapter;
    procedure DoExecuteCommand(AResultSet: PDataSet; const ACommand: string; AParams: TParams; AParamCheck: Boolean); virtual;
    procedure DoInitializeDataSet(ADataSet: TDataSet); virtual; abstract;
    procedure DoSynchronizeDataSetConnection(ADataSet: TDataSet); virtual; abstract;
    function GetSQLQuerySchemaNames: string;
    procedure InitializeDataSet;
    function IsConnected: Boolean;
    procedure PrepareDatasetForIterate(ADataSet: TDataSet); virtual;

    function GetFieldsRetrieveQuery: string;

    property Connection: TCustomConnection read GetConnection;
    property DataSet: TDataSet read GetDataSet;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource);
    function ExecuteCommand(AResultSet: PDataSet; const ACommandText: string; AParams: TParams; AParamCheck: Boolean;
      ACheckSettings: Boolean = True): Boolean;

    property DataSource: TdxServerModeCustomDataSource read FDataSource;
  end;
  TdxServerModeDataSourceCustomHelperClass = class of TdxServerModeDataSourceCustomHelper;

  { TdxServerModeCustomDataSource }

  TdxServerModeCustomDataSource = class(TcxCustomComponent)
  private
    FActive: Boolean;
    FCache: TdxServerModeCache;
    FCacheInconsistencyCount: Integer;
    FChanges: TdxServerModeDataSourceChanges;
    FConnection: TCustomConnection;
    FDataController: TdxServerModeDataController;
    FDataSet: TDataSet;
    FDataSetDesigner: TDataSetDesigner;
    FErrorTimer: TcxTimer;
    FFieldDescriptors: TdxServerModeFieldDescriptors;
    FFilter: TdxServerModeCriteria;
    FFindFilter: TdxServerModeFindFilterCriteria;
    FGeneralFilter: TdxServerModeCriteria;
    FGroupCount: Integer;
    FHelper: TdxServerModeDataSourceCustomHelper;
    FIsDisconnecting: Boolean;
    FKeyFieldNames: TStringList;
    FKeyFields: TdxServerModeFieldDescriptors;
    FLoadingFieldNames: TStrings;
    FLockCount: Integer;
    FPrevStateInfo: TdxServerModeStateInfo;
    FOptions: TdxServerModeDataSourceOptions;
    FSortInfo: TdxServerModeSortInfoDescriptors;
    FStreamedActive: Boolean;
    FSQLAdapter: TdxServerModeCustomSQLAdapter;
    FSQLAdapterClass: TdxServerModeCustomSQLAdapterClass;
    FSQLAdapterEvents: TNotifyEvent;
    FSummaries: TdxServerModeSummaryDescriptors;
    FTotalSummaries: TdxServerModeSummaryDescriptors;
    //events
    FBeforeOpen: TdxServerModeDataSourceNotifyEvent;
    FAfterOpen: TdxServerModeDataSourceNotifyEvent;
    FBeforeClose: TdxServerModeDataSourceNotifyEvent;
    FAfterClose: TdxServerModeDataSourceNotifyEvent;
    FOnFatalError: TdxServerModeDataSourceFatalErrorEvent;
    FOnGetAdapterClass: TdxServerModeGetSQLAdapterClassEvent;
    FOnInconsistentCache: TdxServerModeDataSourceInconsistentCacheEvent;
    procedure BeginUpdateFields;
    procedure EndUpdateFields;
    procedure SendNotifications;

    procedure ConnectionChangedHandler(Sender: TObject; Connecting: Boolean);
    procedure FilterChangedHandler(Sender: TObject);
    procedure SortInfoChangedHandler(Sender: TdxServerModeDescriptors;
      AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
    procedure SummariesChangedHandler(Sender: TdxServerModeDescriptors;
      AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
    procedure TotalSummariesChangedHandler(Sender: TdxServerModeDescriptors;
      AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);

    procedure AssignFields;
    procedure ReadFields(Reader: TReader);
    procedure WriteFields(Writer: TWriter);

    function GetDataControllerPercentWildcard: Char;
    function GetDataControllerUnderscoreWildcard: Char;

    procedure DoSetActive(Value: Boolean);
    function GetCacheState: TdxServerModeCacheState;
    function GetFieldDefs: TFieldDefs;
    function GetFields: TFields;
    function GetKeyFieldNames: string;
    function GetLastErrorMessage: string;
    function GetRow(Index: Integer): TdxServerModeRow;
    function GetRowCount: Integer;
    function GetSQLAdapterClassName: string;
    procedure RecreateSQLAdapter;
    procedure SetActive(Value: Boolean);
    procedure SetConnection(Value: TCustomConnection);
    procedure SetGroupCount(Value: Integer);
    procedure SetKeyFieldNames(const Value: string);
    procedure SetOptions(Value: TdxServerModeDataSourceOptions);
    procedure SetSQLAdapter(const Value: TdxServerModeCustomSQLAdapter);
    procedure SetSQLAdapterClass(const Value: TdxServerModeCustomSQLAdapterClass);
    procedure SetSQLAdapterClassName(const Value: string);
    function GetAdapter: TdxServerModeCustomSQLAdapter;
    procedure RecoveryFromInconsistentState;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const ANewName: TComponentName); override;

    procedure AddDataController(ADataController: TdxServerModeDataController);
    procedure RemoveDataController(ADataController: TdxServerModeDataController);

    procedure AddKeyField(AField: TField); virtual;
    function CanModify: Boolean; virtual;
    function CanUpdate: Boolean; virtual;
    procedure CheckCacheInconsistency(ARecoverCache: Boolean); virtual;
    procedure CheckInactive; virtual;
    procedure CheckParamValue(var AParam: TdxServerModeParam);
    procedure CheckSQLAdapter;
    procedure CheckSettings; virtual;
    function CreateCriteria: TdxServerModeCriteria; virtual;
    function CreateField(AFieldDef: TFieldDef; AOwner: TComponent): TField; virtual;
    function CreateFindFilterCriteria: TdxServerModeFindFilterCriteria;
    function CreateGroupInfo: TdxServerModeGroupInfoDescriptors;
    function CreateKeyFieldNames: TStringList; virtual;
    function CreateSortInfo: TdxServerModeSortInfoDescriptors;
    function CreateSummaries: TdxServerModeSummaryDescriptors;
    procedure DoInitializeDataSet(ADataSet: TDataSet);
    procedure DoSynchronizeDataSetConnection(ADataSet: TDataSet);
    procedure DoUpdate; virtual;
    function GetFieldsRetrieveQuery: string;
    function GetFieldUniqueName(AField: TField): string;
    function GetFromSQLString: string; virtual;
    procedure InitializeKeyFields;
    procedure InternalClose; virtual;
    procedure InternalHandleException; virtual;
    procedure InternalOpen; virtual;
    procedure MakeCriteriaWithBetweenOperator(var ACriteria: TdxServerModeCriteria;
      const AKeys: TdxServerModeKeyList);
    procedure OpenComplete;
    procedure DestroyNonPersistentFields;
    function IsIgnoreNullOnSummary: Boolean;
    function IsPersistentField(AField: TField): Boolean;
    procedure InitializeConnection;
    procedure InitializeDataSet;
    procedure UpdateFieldDefs; virtual;
    procedure UpdateFields; virtual;
    function GetBookmark(ARecordIndex: Integer): Variant;
    function BookmarkValid(ABookmark: Variant): Boolean;
    // events
    procedure DoAfterClose; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoFatalError; virtual;
    procedure DoInconsistentCache(out ARecoverCache: Boolean); virtual;
    procedure DoFatalErrorTimerHandler;
    procedure DoInconsistentCacheTimerHandler;
    procedure ErrorTimerHandler(Sender: TObject);
    procedure StartErrorTimer;

    function GetCacheClass: TdxServerModeCacheClass; virtual;
    function GetCriteriaClass: TdxServerModeCriteriaClass; virtual;
    function GetGroupInfoDescriptorsClass: TdxServerModeGroupInfoDescriptorsClass; virtual;
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; virtual;
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass;
    function GetSortInfoDescriptorsClass: TdxServerModeSortInfoDescriptorsClass; virtual;
    function GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
    function GetDataSetClass: TdxDataSetClass;
    function GetSummariesClass: TdxServerModeSummaryDescriptorsClass; virtual;

    procedure DoExecuteCommand(AResultSet: PDataSet; const ACommand: string; AParams: TParams; AParamCheck: Boolean);
    function ExecuteCommand(AResultSet: PDataSet; const ACommandText: string; AParams: TParams; AParamCheck: Boolean;
      ACheckSettings: Boolean = True): Boolean; overload;
    function ExecuteCommand(AResultSet: PDataSet; const AFields: TdxServerModeDescriptors;
      ACriteria: TdxServerModeCriteria; ASortInfo: TdxServerModeSortInfoDescriptors;
      AGroupInfo: TdxServerModeGroupInfoDescriptors; ATopRecords: Integer; ASkipRecords: Integer;
      ADistinct: Boolean): Boolean; overload;

    procedure FetchKeys(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer); virtual;
    procedure FetchRows(ARows: TdxServerModeRowList; AWhere: TdxServerModeCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; ATopRecords: Integer); virtual;
    procedure FetchRowsByKeys(ARows: TdxServerModeRowList; AKeys: TdxServerModeKeyList); virtual;
    function FindFirstKey(AWhere: TdxServerModeCriteria; ASearchDown: Boolean): TdxServerModeRow;
    function GetCount(ACriteria: TdxServerModeCriteria): Integer; virtual;
    function GetFetchRowByIndexCondition(ARowIndex: Integer): TdxServerModeCriteria;
    function GetFetchRowByKeyCondition(AKey: TdxServerModeRow): TdxServerModeCriteria;
    function GetFetchRowsByKeysCondition(AKeys: TdxServerModeKeyList): TdxServerModeCriteria;
    procedure DoPopulateFields(AList: TStrings); virtual;
    procedure DoPopulateKeyFields(AList: TStrings); virtual;
    function PrepareChildren(AGroupWhere: TdxServerModeCriteria; AGroupByDescriptor: TdxServerModeSortInfoDescriptor;
      ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoDataList; virtual;
    function PrepareTopGroupInfo(ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData; virtual;

    procedure Change(AChanges: TdxServerModeDataSourceChanges);
    procedure Changed;
    procedure Clear;
    procedure Update;
    procedure UpdateFieldIndex(AField: TdxServerModeDataField);

    procedure Apply(AFilter: TdxServerModeCriteria; AFindFilter: TdxServerModeFindFilterCriteria;
      ASortInfo: TdxServerModeSortInfoDescriptors; AGroupCount: Integer; ASummaries,
      ATotalSummaries: TdxServerModeSummaryDescriptors);
    function CreateEmptyRow: Variant;
    procedure Delete(ACriteria: TdxServerModeCriteria); virtual;
    function FindIncremental(AField: TField; const ASubStr: string; AStartIndex: Integer;
      ASearchDown: Boolean = True; AIgnoreStartRow: Boolean = False): Integer; overload;
    function FindIncremental(const AFieldName, ASubStr: string; AStartIndex: Integer;
      ASearchDown: Boolean = True; AIgnoreStartRow: Boolean = False): Integer; overload;
    procedure PopulateFilterValues(AList: TcxDataFilterValueList; AField: TField;
      ACriteria: TcxFilterCriteria; var AUseFilteredRecords: Boolean; out ANullExists: Boolean); virtual;
    function PostEditingData(ARowIndex: Integer; const AEditingRow: Variant): Variant; virtual;

    property Adapter: TdxServerModeCustomSQLAdapter read GetAdapter;
    property Cache: TdxServerModeCache read FCache;
    property CacheInconsistencyCount: Integer read FCacheInconsistencyCount write FCacheInconsistencyCount;
    property CacheState: TdxServerModeCacheState read GetCacheState;
    property DataController: TdxServerModeDataController read FDataController;
    property DataSet: TDataSet read FDataSet;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property FieldDescriptors: TdxServerModeFieldDescriptors read FFieldDescriptors;
    property Filter: TdxServerModeCriteria read FFilter;
    property FindFilter: TdxServerModeFindFilterCriteria read FFindFilter;
    property GeneralFilter: TdxServerModeCriteria read FGeneralFilter;
    property GroupCount: Integer read FGroupCount write SetGroupCount;
    property Helper: TdxServerModeDataSourceCustomHelper read FHelper;
    property KeyFields: TdxServerModeFieldDescriptors read FKeyFields;
    property LastErrorMessage: string read GetLastErrorMessage;
    property PrevStateInfo: TdxServerModeStateInfo read FPrevStateInfo;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TdxServerModeRow read GetRow;
    property SortInfo: TdxServerModeSortInfoDescriptors read FSortInfo;
    property Summaries: TdxServerModeSummaryDescriptors read FSummaries;
    property TotalSummaries: TdxServerModeSummaryDescriptors read FTotalSummaries;
    property OnGetAdapterClass: TdxServerModeGetSQLAdapterClassEvent read FOnGetAdapterClass write FOnGetAdapterClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure Open;
    procedure Close;
    procedure Refresh;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    procedure PopulateFields(AList: TStrings);
    procedure PopulateKeyFields(AList: TStrings);

    function IsConnected: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;
    function IsLocked: Boolean;
    function IsConsistentCache: Boolean;

    property Fields: TFields read GetFields;
    property Connection: TCustomConnection read FConnection write SetConnection;

    property SQLAdapterClass: TdxServerModeCustomSQLAdapterClass read FSQLAdapterClass write SetSQLAdapterClass;
  published
    property Active: Boolean read FActive write SetActive default False;
    property KeyFieldNames: string read GetKeyFieldNames write SetKeyFieldNames;
    property Options: TdxServerModeDataSourceOptions read FOptions write SetOptions;
    property SQLAdapterClassName: string read GetSQLAdapterClassName write SetSQLAdapterClassName;
    property SQLAdapter: TdxServerModeCustomSQLAdapter read FSQLAdapter write SetSQLAdapter;

    // events
    property BeforeOpen: TdxServerModeDataSourceNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TdxServerModeDataSourceNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TdxServerModeDataSourceNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TdxServerModeDataSourceNotifyEvent read FAfterClose write FAfterClose;
    property SQLAdapterEvents: TNotifyEvent read FSQLAdapterEvents write FSQLAdapterEvents;
    property OnFatalError: TdxServerModeDataSourceFatalErrorEvent read FOnFatalError write FOnFatalError;
    property OnInconsistentCache: TdxServerModeDataSourceInconsistentCacheEvent
      read FOnInconsistentCache write FOnInconsistentCache;
  end;
  TdxServerModeCustomDataSourceClass = class of TdxServerModeCustomDataSource;

  { TdxServerModeDataSource }

  TdxServerModeDataSource = class(TdxServerModeCustomDataSource)
  private
    FIsViewLinked: Boolean;
    FIsViewLinkedCalculated: Boolean;
    FReadOnly: Boolean;
    FTableName: string;
    procedure SetReadOnly(Value: Boolean);
    procedure SetTableName(const Value: string);
    function GetProvider: TdxCustomServerModeDataProvider;
  protected
    function CanModify: Boolean; override;
    procedure CheckSettings; override;
    procedure CheckTableName;
    function GetFromSQLString: string; override;
    procedure Delete(ACriteria: TdxServerModeCriteria); override;
    procedure DoPopulateKeyFields(AList: TStrings); override;
    procedure DoPopulateTableNames(AList: TStrings); virtual; abstract;
    procedure DoPopulateViewNames(AList: TStrings); virtual; abstract;
    function IsViewLinked: Boolean;
    procedure PopulateViewNames(AList: TStrings);
    function PostEditingData(ARowIndex: Integer; const AEditingRow: Variant): Variant; override;

    property Provider: TdxCustomServerModeDataProvider read GetProvider;
  public
    procedure PopulateTableNames(AList: TStrings);
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property TableName: string read FTableName write SetTableName;
  end;

  { TdxServerModeQueryDataSource }

  TdxServerModeQueryDataSource = class(TdxServerModeCustomDataSource)
  private
    FSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
  protected
    procedure CheckSettings; override;
    procedure CheckSQL;
    function GetFromSQLString: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SQL: TStrings read FSQL write SetSQL;
  end;

  { TdxServerModeDataField }

  TdxServerModeDataField = class(TcxCustomDataField)
  private
    FField: TField;
    FFieldIndex: Integer;
    FFieldName: string;
    FFields: TList;
    FIsCurrency: Boolean;
    FModified: Boolean;
    FParentField: TdxServerModeDataField;
    function GetDataController: TdxServerModeDataController;
    function GetField: TField;
    function GetFieldIndex: Integer;
    function GetFieldItem(Index: Integer): TdxServerModeDataField;
    function GetFieldCount: Integer;
    function GetIsCurrency: Boolean;
    function GetIsLookup: Boolean;
    function GetProvider: TdxCustomServerModeDataProvider;
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: string);
  protected
    procedure AddField(ADataField: TdxServerModeDataField);
    procedure ClearFields;
    procedure Reassign(ASource: TcxCustomDataField); override;
    procedure RemoveField(ADataField: TdxServerModeDataField);
    procedure RemoveNotification(AComponent: TComponent); override;
    function SetEditingData(ARecordIndex: Integer; const AValue: Variant): Boolean;
    procedure SetPropertiesByField(AField: TField; AFieldChanged: Boolean);

    property DataController: TdxServerModeDataController read GetDataController;
    property Field: TField read GetField write SetField;
    property FieldCount: Integer read GetFieldCount;
    property FieldName: string read FFieldName write SetFieldName;
    property Fields[Index: Integer]: TdxServerModeDataField read GetFieldItem; default;
    property FieldIndex: Integer read GetFieldIndex;
    property IsCurrency: Boolean read GetIsCurrency;
    property IsLookup: Boolean read GetIsLookup;
    property ParentField: TdxServerModeDataField read FParentField;
    property Provider: TdxCustomServerModeDataProvider read GetProvider;
  public
    constructor Create(AFieldList: TcxCustomDataFieldList); override;
    destructor Destroy; override;
    function CanModify(AEditValueSource: TcxDataEditValueSource): Boolean; override;
    function IsUnbound: Boolean; override;
    function IsValueDefInternal: Boolean; override;
  end;

  { TdxServerModeSummaryItem }

  TdxServerModeSummaryItem = class(TcxDataSummaryItem)
  private
    FDataField: TdxServerModeDataField;
    FFieldName: string;
    function GetFieldName: string;
    function GetServerModeDataController: TdxServerModeDataController;
    procedure SetFieldName(const Value: string);
  protected
    function AllowKind(Value: TcxSummaryKind): Boolean;
    function CanSetKind(Value: TcxSummaryKind): Boolean; override;
    function IsCurrency(AVarType: TVarType): Boolean; override;
    function IsDataBinded: Boolean; override;

    function ServerModeDataField: TdxServerModeDataField;
  public
    procedure Assign(Source: TPersistent); override;
    property DataController: TdxServerModeDataController read GetServerModeDataController;
    function DataField: TcxCustomDataField; override;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TdxServerModeDataSummary }

  TdxServerModeDataGroupInfo = class;

  TdxServerModeDataSummary = class(TcxDataSummary)
  private
    function GetDataController: TdxServerModeDataController;
    procedure PopulateSummary(ASummaryItems: TcxDataSummaryItems; AValues: TdxServerModeSummaryList;
      var ACountValues: TcxDataSummaryCountValues; var ASummaryValues: TcxDataSummaryValues);
  protected
    procedure CalculateSummary(ASummaryItems: TcxDataSummaryItems;
      ABeginIndex, AEndIndex: Integer; var ACountValues: TcxDataSummaryCountValues;
      var ASummaryValues: TcxDataSummaryValues); override;
  public
    procedure CalculateGroupSummary; override;
    procedure UpdateGroupSummary(AParentGroup: TdxServerModeDataGroupInfo = nil);

    property DataController: TdxServerModeDataController read GetDataController;
  end;

  { TdxServerModeProviderDataSource }

  TdxServerModeProviderDataSource = class(TcxCustomDataSource)
  private
    function GetDataController: TdxServerModeDataController; inline;
    function GetProvider: TdxCustomServerModeDataProvider; inline;
  public
    function GetRecordHandleByIndex(ARecordIndex: Integer): TcxDataRecordHandle; override;
    property DataController: TdxServerModeDataController read GetDataController;
    property Provider: TdxCustomServerModeDataProvider read GetProvider;
  end;

  { TdxCustomServerModeDataProvider }

  TdxCustomServerModeDataProvider = class(TcxCustomDataProvider)
  private
    FEditingRow: TdxServerModeRow;
    FInCanInitEditing: Boolean;
    FIsAppending: Boolean;
    procedure AddRecord(AIsAppending: Boolean);
    procedure ClearEditingRow;
    procedure CreateEditingRow;
    procedure SetCustomDataSource(Value: TdxServerModeProviderDataSource);
    function GetCustomDataSource: TdxServerModeProviderDataSource;
    function GetDataController: TdxServerModeDataController;
    function GetDataSource: TdxServerModeCustomDataSource;
  protected
    function IsActive: Boolean; override;
    // navigation
    procedure SavePos; override;
    procedure RestorePos; override;
    // Editing
    procedure Append; override;
    function CanDelete: Boolean; override;
    function CanInitEditing(ARecordIndex: Integer): Boolean; override;
    function CanModify: Boolean; override;
    procedure DeleteRecord(ARecordIndex: Integer); override;
    procedure DeleteRecords(ACriteria: TdxServerModeCriteria); reintroduce;
    procedure DeleteSelection; override;
    procedure DoInsertingRecord(AIsAppending: Boolean); override;
    procedure Edit; override;
    procedure Insert; override;
    procedure RecordChanged(Field: TField); virtual;

    procedure AssignItemValue(ARecordIndex: Integer; AField: TcxCustomDataField; const AValue: Variant); override;
    procedure Post(AForcePost: Boolean = False); override;
    procedure PostEditingData; override;
    procedure ResetEditing; override;
    procedure UpdateData;

    function GetEditingRowKey: Variant;
    function GetRowIndexByKey(AKey: Variant): Integer;
    function GetRowKey(ARecordIndex: Integer): Variant;
    function GetServerRowValue(ARecordIndex, AFieldIndex: Integer): Variant;
    procedure InternalPost;
    function SetEditingData(ARecordIndex, AFieldIndex: Integer; const AValue: Variant): Boolean;
    function SetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField; const AValue: Variant;
      AEditValueSource: TcxDataEditValueSource): Boolean; override;

    property CustomDataSource: TdxServerModeProviderDataSource read GetCustomDataSource write SetCustomDataSource;
    property DataController: TdxServerModeDataController read GetDataController;
    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
    property EditingRow: Variant read FEditingRow;
    property IsAppending: Boolean read FIsAppending;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;
  end;

  { TdxServerModeDataFocusingInfo }

  TdxServerModeDataFocusingInfo = class(TcxDataFocusingInfo)
  private
    FKey: Variant;
    FIsGroupKey: Boolean;
    function GetDataControllerInfo: TdxServerModeDataControllerInfo;
  protected
    procedure Assign(AFocusingInfo: TcxDataFocusingInfo); override;
    procedure CalculateKey(ADataGroupIndex: Integer);
    procedure Clear; override;
    function IsEqual(AFocusingInfo: TcxDataFocusingInfo): Boolean; override;
    function IsNeedUpdate: Boolean; override;
    function IsUndefined: Boolean; inline;
    procedure SavePos; override;
    procedure SetRowKey(const AKey: Variant);
  public
    constructor Create(ADataControllerInfo: TcxCustomDataControllerInfo); override;

    property DataControllerInfo: TdxServerModeDataControllerInfo read GetDataControllerInfo;
    property Key: Variant read FKey;
    property IsGroupKey: Boolean read FIsGroupKey;
  end;

  { TdxServerModeDataGroupInfo }

  TdxServerModeDataGroupInfo = class(TcxDataGroupInfo)
  private
    FServerGroupInfo: TdxServerModeGroupInfo;
    function GetGroupValue: Variant;
    function GetDisplayText: Variant;
  protected
    procedure ServerGroupInfoDestroyHandler(Sender: TObject); virtual;
    procedure SetServerGroupInfo(const Value: TdxServerModeGroupInfo);

    procedure AdjustByInsertingRecord(ARecordIndex: Integer; AAppending: Boolean);

    property DisplayText: Variant read GetDisplayText;
  public
    ChildrenIsReady: Boolean;
    ParentGroup: TdxServerModeDataGroupInfo;
    constructor Create(AOwner: TcxDataGroups; AParentGroup: TdxServerModeDataGroupInfo; AServerGroupInfo: TdxServerModeGroupInfo);
    destructor Destroy; override;
    function IsEqual(const AValue: Variant; AHash: Cardinal): Boolean;

    property GroupValue: Variant read GetGroupValue;
    property ServerGroupInfo: TdxServerModeGroupInfo read FServerGroupInfo;
  end;

  { TdxServerModeDataGroups }

  TdxServerModeDataGroups = class(TcxDataGroups)
  private
    FIsFullExpanding: Boolean;
    function GetItem(Index: Integer): TdxServerModeDataGroupInfo;
    function GetDataControllerInfo: TdxServerModeDataControllerInfo;
    function GetTopGroupInfo: TdxServerModeGroupInfo; inline;
  protected
    procedure CreateGroups;
    procedure CreateGroup(AList: TdxServerModeGroupInfoList; AParentGroup: TdxServerModeDataGroupInfo);
    procedure ExpandItem(AItem: TdxServerModeDataGroupInfo; AExpanded: Boolean);
    function FindByServerGroupInfo(const AServerGroupInfo: TdxServerModeGroupInfo): Integer;
    function FindInnerGroup(ARecordListIndex: Integer): Integer;
    function GetFirstDataRecordListIndex(AInfo: TcxDataGroupInfo): Integer; override;
    function GetLastDataRecordListIndex(AInfo: TcxDataGroupInfo): Integer; override;
    function GetRowIndexByGroupValue(const AGroupValue: Variant): Integer;
    function MakeVisible(AIndex: Integer; AExpand: Boolean): Boolean; override;
    procedure RequestChildren(AParentGroup: TdxServerModeDataGroupInfo);
    procedure UpdateGroupSummary(AParentGroup: TdxServerModeDataGroupInfo);

    property DataControllerInfo: TdxServerModeDataControllerInfo read GetDataControllerInfo;
    property TopGroupInfo: TdxServerModeGroupInfo read GetTopGroupInfo;
  public
    procedure ChangeExpanding(ARowIndex: Integer; AExpanded, ARecursive: Boolean); override;
    procedure FullExpanding(AExpanded: Boolean); override;
    procedure Rebuild; override;
    function GetChildCount(AIndex: Integer): Integer; override;
    function GetChildIndex(AParentIndex, AIndex: Integer): Integer; override;

    property Items[Index: Integer]: TdxServerModeDataGroupInfo read GetItem; default;
  end;

  { TdxServerModeDataSelection }

  TdxServerModeDataSelection = class(TcxDataSelection)
  private
    function GetDataController: TdxServerModeDataController;
  public
    property DataController: TdxServerModeDataController read GetDataController ;
  end;

  { TdxServerModeDataControllerInfo }

  TdxServerModeDataControllerInfo = class(TcxCustomDataControllerInfo)
  private
    FRecoveryFocusingInfo: TdxServerModeDataFocusingInfo;

    procedure ConvertFilterItems(ADestination: TdxServerModeCriteria; ASource: TcxFilterCriteria);
    procedure ConvertFindFilter(ADestination: TdxServerModeFindFilterCriteria; ASource: TdxServerModeDataFindFilterCriteria);
    function CreateCriteria: TdxServerModeCriteria;
    function CreateFindFilterCriteria: TdxServerModeFindFilterCriteria;
    function CreateSortInfo: TdxServerModeSortInfoDescriptors;
    function CreateSummariesByItems(AItems: TcxDataSummaryItems; ALevel: Integer): TdxServerModeSummaryDescriptors;
    function CreateGroupSummaries: TdxServerModeSummaryDescriptors;
    function CreateTotalSummaries: TdxServerModeSummaryDescriptors;
    function GetDataController: TdxServerModeDataController;
    function GetDataGroups: TdxServerModeDataGroups;
    function GetSelection: TdxServerModeDataSelection;
    function GetSource: TdxServerModeCache;
    function GetFocusingInfo: TdxServerModeDataFocusingInfo;
    procedure PopulateSummariesByItems(AList: TdxServerModeSummaryDescriptors;
      AItems: TcxDataSummaryItems; ALevel: Integer);
    function GetDataSource: TdxServerModeCustomDataSource; inline;
  protected
    procedure DoFilter; override;
    procedure DoLoad; override;
    procedure DoSort; override;
    procedure DoUpdate(ASummaryChanged: Boolean); override;
    function IsDataSourceValid: Boolean;
    function IsUpdateDataSourceNeeded: Boolean; virtual;
    procedure UpdateDataSource;
    procedure ApplyRecoveryInfo;
    procedure MakeRecoveryInfo;
    function GetDataFocusingInfoClass: TcxDataFocusingInfoClass; override;
    function GetDataGroupsClass: TcxDataGroupsClass; override;

    procedure CalculateSummary;
    function CanCreateGroups: Boolean; override;
    procedure CheckExpanding; override;
    procedure CheckFocusing; override;
    procedure CreateGroups; override;
    function FindDataGroup(ARecordListIndex: Integer): Integer; override;
    function FindFocusedGroup: Integer;
    function FindFocusedRow(ANearest: Boolean): Integer; override;
    function GetInternalRecordCount: Integer; override;
    function GetRecordListIndexByFocusingInfo: Integer; override;
    function IsResetFocusingNeeded: Boolean; override;
    function LocateDetail(AGroupIndex, ARecordListIndex: Integer; AMakeVisible: Boolean): Integer; override;
    function LocateGroupByLevel(AGroupIndex, ALevel: Integer): Integer; override;
    procedure ResetFocusing; override;
    procedure SaveExpanding(ASaveStates: TcxDataExpandingInfoStateSet); override;

    property FocusingInfo: TdxServerModeDataFocusingInfo read GetFocusingInfo;
    property Source: TdxServerModeCache read GetSource;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;
    function GetRowInfo(ARowIndex: Integer): TcxRowInfo; override;
    procedure RemoveField(AField: TcxCustomDataField); override;
    procedure SelectionChanged; override;

    property DataController: TdxServerModeDataController read GetDataController;
    property DataGroups: TdxServerModeDataGroups read GetDataGroups;
    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
    property Selection: TdxServerModeDataSelection read GetSelection;
  end;

  { TdxServerModeDataControllerSearch }

  TdxServerModeDataControllerSearch = class(TcxDataControllerSearch)
  private
    function GetDataController: TdxServerModeDataController;
  protected
    function DoSearch(AStartFilteredRecordIndex, AEndFilteredRecordIndex: Integer;
      const ASubText: string; AForward, AIsAnywhere: Boolean): Integer; override;
  public
    property DataController: TdxServerModeDataController read GetDataController;
  end;

  { TdxServerModeDataControllerGroups }

  TdxServerModeDataControllerGroups = class(TcxDataControllerGroups)
  private
    function GetDataController: TdxServerModeDataController;
  protected
    function GetCriteria(ARowIndex: Integer): TdxServerModeCriteria;

    function GetGroupDisplayText(ADataGroupIndex: TcxDataGroupIndex): string; override;
    function GetGroupValue(ADataGroupIndex: TcxDataGroupIndex): Variant; override;
  public
    property DataController: TdxServerModeDataController read GetDataController;
  end;

  { TdxServerModeDataController }

  TdxServerModeDataFilterCriteriaItem = class(TcxDataFilterCriteriaItem)
  protected
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;
  end;

  TdxServerModeDataFilterCriteria = class(TcxDataFilterCriteria)
  protected
    function GetItemClass: TcxFilterCriteriaItemClass; override;
  end;

  TdxServerModeDataFindFilterCriteria = class(TcxDataFindFilterCriteria)
  private
    function GetDataController: TdxServerModeDataController;
  protected
    function GetEscapedCondition(const AText: string): string; override;
    function GetEscapeWildcard: Char; override;
    function IsConditionsLowerCase: Boolean; override;

    property DataController: TdxServerModeDataController read GetDataController;
  end;

  TdxServerModeDataControllerOption =
    (sdcoAssignGroupingValues, sdcoGroupsAlwaysExpanded, sdcoImmediatePost,
     sdcoInsertOnNewItemRowFocusing, sdcoKeepFocusedRowOnUpdate, sdcoSaveExpanding);
  TdxServerModeDataControllerOptions = set of TdxServerModeDataControllerOption;

  TdxServerModeDataController = class(TcxCustomDataController)
  private
    FCustomDataSource: TdxServerModeProviderDataSource;
    FDataSource: TdxServerModeCustomDataSource;
    FBookmark: Variant;
    FOptions: TdxServerModeDataControllerOptions;
    function GetFindFilterCriteria: TdxServerModeDataFindFilterCriteria;
    function GetGroups: TdxServerModeDataControllerGroups; inline;
    function GetProvider: TdxCustomServerModeDataProvider;
    function GetField(Index: Integer): TdxServerModeDataField; inline;
    function GetFieldCount: Integer;
    function GetDataControllerInfo: TdxServerModeDataControllerInfo; inline;
    function GetSummary: TdxServerModeDataSummary;inline;
    procedure SetDataSource(const Value: TdxServerModeCustomDataSource);
    procedure SetOptions(const Value: TdxServerModeDataControllerOptions);
    procedure SetSummary(Value: TdxServerModeDataSummary);
  protected
    function CreateDataControllerInfo: TcxCustomDataControllerInfo; override;
    function GetDataProviderClass: TcxCustomDataProviderClass; override;
    function GetDataSelectionClass: TcxDataSelectionClass; override;
    function GetFilterCriteriaClass: TcxDataFilterCriteriaClass; override;
    function GetSearchClass: TcxDataControllerSearchClass; override;
    function GetSummaryClass: TcxDataSummaryClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;

    function CanFixRows: Boolean; override;
    function CanFocusRecord(ARecordIndex: Integer): Boolean; override;
    function CreateFindFilterCriteria: TcxDataFindFilterCriteria; override;
    function GetIsRowInfoValid: Boolean; override;
    function IsMergedGroupsSupported: Boolean; override;
    function IsSmartLoad: Boolean; override;
    procedure PrepareField(AField: TcxCustomDataField); override;
    procedure RemoveNotification(AComponent: TComponent); override;
    function TryFocusRecord(ARecordIndex: Integer): Boolean; virtual;
    procedure Unlocked; override;
    function InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean; override;
    procedure InternalClearBookmark; override;
    procedure InternalGotoBookmark; override;
    function InternalSaveBookmark: Boolean; override;
    function AppendInSmartLoad: Integer; override;
    procedure DeleteInSmartLoad(ARecordIndex: Integer); override;
    procedure DoUpdateRecord(ARecordIndex: Integer);
    procedure UpdateEditingRecord;
    // Structure
    procedure ChangeFieldName(AItemIndex: Integer; const AFieldName: string);
    function GetFieldClass: TcxCustomDataFieldClass; override;
    function GetFieldByFieldName(const AFieldName: string): TdxServerModeDataField;
    function GetGroupsClass: TcxDataControllerGroupsClass; override;
    function GetItemByFieldName(const AFieldName: string): TObject;
    function GetItemField(AItemIndex: Integer): TField;
    function GetItemFieldName(AItemIndex: Integer): string;
    function IsDataField(AField: TcxCustomDataField): Boolean; override;
    function IsDataSourcePersistentField(AField: TField): Boolean;
    procedure UpdateField(ADataField: TdxServerModeDataField; const AFieldNames: string; AIsLookup: Boolean);
    procedure UpdateInternalField(const AFieldName: string; var AField: TdxServerModeDataField);

    procedure DataSourceChanged;
    function DoGetGroupRowDisplayText(const ARowInfo: TcxRowInfo; var AItemIndex: Integer): string; override;
    procedure InitializeDescriptor(AField: TcxCustomDataField; var ADescriptor: TdxServerModeDescriptor); virtual;
    function GetGroupRowValueByItemIndex(const ARowInfo: TcxRowInfo; AItemIndex: Integer): Variant; override;
    function GetInternalDisplayText(ARecordIndex: Integer; AField: TcxCustomDataField): string; override;
    function GetInternalValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    procedure GetKeyFields(AList: TList); override;
    function IsResetFocusedRowNeeded: Boolean; virtual;
    procedure ResetDataSourceCache;
    // Editing
    function CalcEditingRecordIndex: Integer; override;
    // Options
    function GetAnsiSortSetting: Boolean; override;
    function GetCaseInsensitiveSetting: Boolean; override;
    function GetAssignGroupingValuesSetting: Boolean; override;
    function GetAssignMasterDetailKeysSetting: Boolean; override;
    function GetSaveExpandingSetting: Boolean; override;
    function GetSortByDisplayTextSetting: Boolean; override;
    function GetFocusTopRowAfterSortingSetting: Boolean; override;
    function GetGroupsAlwaysExpandedSetting: Boolean; override;
    function GetImmediatePostSetting: Boolean; override;
    function GetInsertOnNewItemRowFocusingSetting: Boolean; override;
    function GetKeepFocusedRowOnUpdatingSetting: Boolean; virtual;

    procedure PopulateFilterValues(AList: TcxDataFilterValueList;
      AItemIndex: Integer; ACriteria: TcxFilterCriteria;
      var AUseFilteredRecords: Boolean; out ANullExists: Boolean; AUniqueOnly: Boolean); override;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TdxServerModeDataField read GetField;
    property FindFilterCriteria: TdxServerModeDataFindFilterCriteria read GetFindFilterCriteria;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel; override;
    procedure CheckBrowseMode; override;
    procedure DeleteFocused; override;
    function GetFilterItemFieldName(AItem: TObject): string; override;
    function GetRecordCount: Integer; override;
    function InsertRecord(ARecordIndex: Integer): Integer; override;
    function IsKeyField(AField: TField): Boolean;
    function IsProviderMode: Boolean; override;
    function MultiSelectionSyncGroupWithChildren: Boolean; override;
    //external
    function FindRecordIndexByKey(const AKeyFieldValues: Variant): Integer;
    function GetRecordId(ARecordIndex: Integer): Variant; override;
    function LocateByKey(const AKeyFieldValues: Variant): Boolean;
    procedure RefreshExternalData; override;
    function IsBookmarkAvailable: Boolean; override;
    function IsBookmarkRow(ARowIndex: Integer): Boolean; override;

    property Groups: TdxServerModeDataControllerGroups read GetGroups;
    property Provider: TdxCustomServerModeDataProvider read GetProvider;
    property Summary: TdxServerModeDataSummary read GetSummary write SetSummary;
    property DataControllerInfo: TdxServerModeDataControllerInfo read GetDataControllerInfo;
    property DataSource: TdxServerModeCustomDataSource read FDataSource write SetDataSource;
    property Options: TdxServerModeDataControllerOptions read FOptions write SetOptions
      default [sdcoAssignGroupingValues, sdcoKeepFocusedRowOnUpdate, sdcoSaveExpanding];
  end;

procedure dxServerModeRegisterAdapter(AClass: TdxServerModeCustomSQLAdapterClass; const AName: string);
procedure dxServerModeUnregisterAdapter(AClass: TdxServerModeCustomSQLAdapterClass);
//design-time
function dxGetServerModeSQLAdapters: TcxRegisteredClasses;

implementation

uses
  RTLConsts, Math, cxDateUtils, StrUtils, dxServerModeConsts, dxServerModeSQLAdapters,
  DateUtils;

const
  dxDefaultMagicNumberFetchRowsInSize = 55;
  dxDefaultMagicNumberFetchRowsAllThreshold = 256;
  dxDefaultMagicNumberFetchKeysAllThreshold = 2048;
  dxDefaultMagicNumberFetchRowsTopThreshold = 100;
  dxDefaultMagicNumberFetchRowsTop = 128;
  dxDefaultMagicNumberFetchKeysModulo = 50;
  dxDefaultMagicNumberScanKeysBase = 256;
  dxDefaultMagicNumberTakeKeysUpperLimitAfterSkip = High(Integer);
  dxDefaultMagicNumberTakeIncreaseStepMultiplier = 4.0;
  dxDefaultDateTimeFormat = 'yyyy-mm-dd''T''hh:mm:ss.zzz';

type
  TDataSetAccess = class(TDataSet);
  TCustomConnectionAccess = class(TCustomConnection);
  TdxDataSummaryItemAccess = class(TcxDataSummaryItem);

  { TdxServerModeDataSetDesigner }

  TdxServerModeDataSetDesigner = class(TDataSetDesigner)
  private
    FDataSource: TdxServerModeCustomDataSource;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); reintroduce;
    procedure DataEvent(Event: TDataEvent; Info: TdxNativeInt); override;

    property DataSource: TdxServerModeCustomDataSource read FDataSource;
  end;

var
  FAdapters: TcxRegisteredClasses;

function dxGetServerModeSQLAdapters: TcxRegisteredClasses;
begin
  if FAdapters = nil then
    FAdapters := TcxRegisteredClasses.Create;
  Result := FAdapters;
end;

procedure dxServerModeRegisterAdapter(AClass: TdxServerModeCustomSQLAdapterClass; const AName: string);
begin
  dxGetServerModeSQLAdapters.Register(AClass, AName);
end;

procedure dxServerModeUnregisterAdapter(AClass: TdxServerModeCustomSQLAdapterClass);
begin
  dxGetServerModeSQLAdapters.Unregister(AClass);
  if dxGetServerModeSQLAdapters.Count = 0 then
    FreeAndNil(FAdapters);
end;

{ TdxServerModeDataSetDesigner }

constructor TdxServerModeDataSetDesigner.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create(ADataSource.DataSet);
  FDataSource := ADataSource;
end;

procedure TdxServerModeDataSetDesigner.DataEvent(Event: TDataEvent; Info: TdxNativeInt);
begin
  inherited;
  if (Event = deFieldListChange) and not DataSource.IsLocked then
  begin
    if DataSource.Active then
      DataSource.Active := False
    else
    begin
      DataSource.BeginUpdate;
      try
        DataSource.Clear;
      finally
        DataSource.CancelUpdate;
      end;
    end;
  end;
end;

{ TdxServerModeFindFilterCriteria }

procedure TdxServerModeFindFilterCriteria.AssignConditions(AFindFilter: TdxServerModeFindFilterCriteria);
begin
  Filter.Root.Assign(AFindFilter.Filter.Root);
end;

function TdxServerModeFindFilterCriteria.IsEqual(AFindFilter: TdxServerModeFindFilterCriteria): Boolean;
begin
  Result := ((AFindFilter = nil) and Filter.IsEmpty) or
    ((AFindFilter <> nil) and Filter.IsEqual(AFindFilter.Filter));
end;

function TdxServerModeFindFilterCriteria.CreateFilter: TcxFilterCriteria;
begin
  Result := DataSource.CreateCriteria;
end;

procedure TdxServerModeFindFilterCriteria.SetEscapedFlagForFilterItems;

  procedure SetRecursiveEscapedFlag(AList: TdxServerModeCriteriaItemList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
      if AList[I].IsItemList then
        SetRecursiveEscapedFlag(TdxServerModeCriteriaItemList(AList[I]))
      else
        TdxServerModeCriteriaItem(AList[I]).EscapedValue := True;
  end;

begin
  SetRecursiveEscapedFlag(Filter.Root);
end;

function TdxServerModeFindFilterCriteria.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := TdxServerModeCustomDataSource(inherited Owner);
end;

function TdxServerModeFindFilterCriteria.GetFilter: TdxServerModeCriteria;
begin
  Result := TdxServerModeCriteria(inherited Filter);
end;

{ TdxServerModeCriteriaItem }

function TdxServerModeCriteriaItem.IsEmpty: Boolean;
begin
  Result := Descriptor = nil;
end;

function TdxServerModeCriteriaItem.GetFieldCaption: string;
begin
  if Descriptor <> nil then
    Result := Descriptor.ToString
  else
    Result := inherited GetFieldCaption;
  Criteria.DoGetFieldCaption(Self, Result);
end;

function TdxServerModeCriteriaItem.GetFieldName: string;
begin
  Result := GetFieldCaption;
end;

function TdxServerModeCriteriaItem.GetFilterOperatorClass: TcxFilterOperatorClass;
const
  ANullOperatorClasses: array[Boolean] of TcxFilterOperatorClass = (
    TcxFilterSQLNullOperator, TcxFilterSQLNotNullOperator);
begin
  if (OperatorKind in [foEqual, foNotEqual, foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith]) and (ValueIsNull(Value)) then
    Result := ANullOperatorClasses[OperatorKind in [foNotEqual, foNotLike]]
  else
    Result := inherited GetFilterOperatorClass;
end;

procedure TdxServerModeCriteriaItem.ReadData(AStream: TStream);
begin
  inherited ReadData(AStream);
  AStream.ReadBuffer(FDescriptor, SizeOf(Pointer));
  ReadBooleanProc(AStream, FEscapedValue);
end;

procedure TdxServerModeCriteriaItem.WriteData(AStream: TStream);
begin
  inherited WriteData(AStream);
  AStream.WriteBuffer(FDescriptor, SizeOf(Pointer));
  WriteBooleanProc(AStream, EscapedValue);
end;

function TdxServerModeCriteriaItem.GetField: TField;
begin
  Result := FDescriptor.Field;
end;

function TdxServerModeCriteriaItem.InternalGetCriteria: TdxServerModeCriteria;
begin
  Result := TdxServerModeCriteria(inherited Criteria);
end;

{ TdxServerModeCriteriaItemList }

procedure TdxServerModeCriteriaItemList.Assign(Source: TdxServerModeCriteriaItemList);
begin
  Clear;
  if (Source <> nil) and not Source.IsEmpty then
  begin
    BoolOperatorKind := Source.BoolOperatorKind;
    AssignItems(Source);
    Changed;
  end;
end;

procedure TdxServerModeCriteriaItemList.AssignItems(Source: TdxServerModeCriteriaItemList);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    Source.WriteData(AStream);
    AStream.Position := 0;
    ReadData(AStream);
  finally
    AStream.Free;
  end;
end;

{ TdxServerModeCriteria }

constructor TdxServerModeCriteria.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
end;

function TdxServerModeCriteria.AddItem(ADescriptor: TdxServerModeDescriptor;
  AOperatorKind: TcxFilterOperatorKind; const AValue: Variant): TdxServerModeCriteriaItem;
begin
  Result := AddItem(nil, ADescriptor, AOperatorKind, AValue);
end;

function TdxServerModeCriteria.AddItem(AParent: TcxFilterCriteriaItemList; ADescriptor: TdxServerModeDescriptor;
  AOperatorKind: TcxFilterOperatorKind; const AValue: Variant): TdxServerModeCriteriaItem;
begin
  Result := TdxServerModeCriteriaItem(inherited AddItem(AParent, nil, AOperatorKind,
    AValue, dxVarArrayToDelimitedText(AValue, ',', '"')));
  Result.FDescriptor := ADescriptor;
end;

procedure TdxServerModeCriteria.AndCriteria(Value: TdxServerModeCriteria; ADestroyValue: Boolean = False);
begin
  Operation(Value, fboAnd, ADestroyValue);
end;

procedure TdxServerModeCriteria.OrCriteria(Value: TdxServerModeCriteria; ADestroyValue: Boolean = False);
begin
  Operation(Value, fboOr, ADestroyValue);
end;

function TdxServerModeCriteria.Clone: TdxServerModeCriteria;
begin
  Result := TdxServerModeCriteriaClass(ClassType).Create(DataSource);
  Result.Assign(Self);
end;

function TdxServerModeCriteria.IsEqual(AValue: TdxServerModeCriteria): Boolean;
begin
  Result := ((AValue = nil) and IsEmpty) or
    ((AValue <> nil) and EqualItems(AValue));
end;

procedure TdxServerModeCriteria.DoGetFieldCaption(AItem: TdxServerModeCriteriaItem;
  var AFieldCaption: string);
begin
  if Assigned(OnGetFieldCaption) then
    OnGetFieldCaption(AItem, AFieldCaption);
end;

function TdxServerModeCriteria.FilterTextUsed: Boolean;
begin
  Result := not SQLCalculating;
end;

procedure TdxServerModeCriteria.FormatFilterTextValue(AItem: TcxFilterCriteriaItem; const AValue: Variant;
  var ADisplayValue: string);
begin
  inherited;
  if Assigned(OnGetValue) then
    OnGetValue(AItem, AValue, ADisplayValue);
end;

function TdxServerModeCriteria.GetIDByItemLink(AItemLink: TObject): Integer;
begin
  Result := -1;
end;

function TdxServerModeCriteria.GetItemLinkByID(AID: Integer): TObject;
begin
  Result := nil;
end;

function TdxServerModeCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TdxServerModeCriteriaItem;
end;

function TdxServerModeCriteria.GetItemListClass: TcxFilterCriteriaItemListClass;
begin
  Result := TdxServerModeCriteriaItemList;
end;

function TdxServerModeCriteria.GetRootClass: TcxFilterCriteriaItemListClass;
begin
  Result := TdxServerModeCriteriaItemList;
end;

procedure TdxServerModeCriteria.Operation(Value: TdxServerModeCriteria;
  AOperator: TcxFilterBoolOperatorKind; ADestroyValue: Boolean);
var
  ACriteria: TdxServerModeCriteria;
  AList: TdxServerModeCriteriaItemList;
begin
  try
    if Value.IsEmpty then
      Exit;
    ACriteria := DataSource.CreateCriteria;
    try
      AList := TdxServerModeCriteriaItemList(ACriteria.Root.AddItemList(fboAnd));
      AList.AssignItems(Root);
      AList := TdxServerModeCriteriaItemList(ACriteria.Root.AddItemList(fboAnd));
      AList.AssignItems(Value.Root);
      ACriteria.Root.BoolOperatorKind := AOperator;
      Assign(ACriteria);
    finally
      ACriteria.Free;
    end;
  finally
    if ADestroyValue then
      Value.Free;
  end;
end;

function TdxServerModeCriteria.GetRoot: TdxServerModeCriteriaItemList;
begin
  Result := TdxServerModeCriteriaItemList(inherited Root);
end;

{ TdxServerModeDescriptor }

constructor TdxServerModeDescriptor.Create;
begin
  inherited Create;
  FLinks := TdxFastList.Create;
end;

destructor TdxServerModeDescriptor.Destroy;
begin
  LinkedObject := nil;
  ClearLinks;
  FreeAndNil(FLinks);
  inherited Destroy;
end;

procedure TdxServerModeDescriptor.Assign(Source: TdxServerModeDescriptor);
begin
  if IsEqual(Source) then
    Exit;
  BeginUpdate;
  try
    DoAssign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxServerModeDescriptor.Clone: TdxServerModeDescriptor;
begin
  Result := TdxServerModeDescriptorClass(ClassType).Create;
  Result.Assign(Self);
end;

function TdxServerModeDescriptor.IsEqual(AValue: TdxServerModeDescriptor): Boolean;
begin
  Result := (AValue <> nil) and (Field = AValue.Field);
end;

function TdxServerModeDescriptor.ToString: string;
begin
  if LinkedObject <> nil then
    Result := LinkedObject.ToString
  else
    if Field = nil then
      Result := '*'
    else
      Result := Field.FieldName;
end;

procedure TdxServerModeDescriptor.Changed(AChanges: TdxServerModeDescriptorChanges = []);
begin
  FChanges := FChanges + AChanges;
  if not IsLocked and (FChanges <> []) and (Owner <> nil) then
  begin
    Owner.DoChanged(Self, FChanges);
    FChanges := [];
  end;
end;

procedure TdxServerModeDescriptor.DoAssign(Source: TdxServerModeDescriptor);
begin
  LinkedObject := Source.LinkedObject;
  DateTimeGrouping := Source.DateTimeGrouping;
end;

function TdxServerModeDescriptor.GetField: TField;
begin
  if LinkedObject <> nil then
    Result := LinkedObject.Field
  else
    Result := nil;
end;

procedure TdxServerModeDescriptor.AddLink(ALink: TdxServerModeDescriptor);
begin
  FLinks.Add(ALink);
end;

procedure TdxServerModeDescriptor.ClearLinks;
begin
  while FLinks.Count > 0 do
    Links[0].LinkedObject := nil;
end;

procedure TdxServerModeDescriptor.RemoveLink(ALink: TdxServerModeDescriptor);
begin
  FLinks.Remove(ALink);
end;

procedure TdxServerModeDescriptor.BeginUpdate;
begin
  Inc(FLockCount);
end;

function TdxServerModeDescriptor.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxServerModeDescriptor.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

function TdxServerModeDescriptor.IsDateTimeGrouping: Boolean;
begin
  Result :=
    (DateTimeGrouping in [dgByDateAndTime, dgRelativeToToday, dgByHour, dgByDate, dgByMonth, dgByYear]) and
    (Field.DataType in [ftDate, ftTime, ftDateTime, ftTimeStamp]);
end;

function TdxServerModeDescriptor.IsSorting: Boolean;
begin
  Result := False;
end;

function TdxServerModeDescriptor.IsSummary: Boolean;
begin
  Result := False;
end;

function TdxServerModeDescriptor.GetLink(Index: Integer): TdxServerModeDescriptor;
begin
  Result := TdxServerModeDescriptor(FLinks[Index]);
end;

function TdxServerModeDescriptor.GetLinkCount: Integer;
begin
  Result := FLinks.Count;
end;

function TdxServerModeDescriptor.GetIndex: Integer;
begin
  Result := Owner.IndexOf(Self);
end;

procedure TdxServerModeDescriptor.SetDateTimeGrouping(Value: TdxDateTimeGrouping);
begin
  if DateTimeGrouping <> Value then
  begin
    FDateTimeGrouping := Value;
    Changed([dctSorting]);
  end;
end;

procedure TdxServerModeDescriptor.SetIndex(Value: Integer);
begin
  if Index <> Value then
    Owner.List.Move(Index, Value);
end;

procedure TdxServerModeDescriptor.SetOwner(Value: TdxServerModeDescriptors);
begin
  if Owner <> Value then
  begin
    FOwner := Value;
    Changed([]);
  end;
end;

procedure TdxServerModeDescriptor.SetLinkedObject(Value: TdxServerModeDescriptor);
begin
  if LinkedObject <> Value then
  begin
    if LinkedObject <> nil then
      LinkedObject.RemoveLink(Self);
    FLinkedObject := Value;
    if LinkedObject <> nil then
      LinkedObject.AddLink(Self);
    Changed([dctLink]);
  end;
end;

{ TdxServerModeDescriptors }

constructor TdxServerModeDescriptors.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
  FList := TdxFastObjectList.Create;
end;

destructor TdxServerModeDescriptors.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxServerModeDescriptors.Assign(Source: TdxServerModeDescriptors);
var
  I: Integer;
  ACount: Integer;
begin
  if IsEqual(Source) then
    Exit;
  if (Source = nil) or Source.IsEmpty then
    FList.Clear
  else
  begin
    ACount := Min(Count, Source.Count);
    for I := 0 to ACount - 1 do
      if not Items[I].IsEqual(Source[I]) then
        Items[I].Assign(Source[I]);

    if Count > Source.Count then
      for I := Count - 1 downto Source.Count do
        RemoveItem(Items[I])
    else
      for I := Count to Source.Count - 1 do
        AddItem(Source[I].Clone);
  end;
end;

function TdxServerModeDescriptors.Add(AField: TField): TdxServerModeDescriptor;
begin
  Result := GetDescriptorClass.Create;
  if AField <> nil then
    Result.LinkedObject := DataSource.FieldDescriptors.ItemByField(AField);
  AddItem(Result);
end;

procedure TdxServerModeDescriptors.AddItems(AList: TdxServerModeDescriptors);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AddItem(AList[I].Clone);
end;

function TdxServerModeDescriptors.CreateLink(ALinkedObject: TdxServerModeDescriptor): TdxServerModeDescriptor;
begin
  Result := Add(nil);
  Result.LinkedObject := ALinkedObject;
end;

procedure TdxServerModeDescriptors.CreateLinks(AList: TdxServerModeDescriptors);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    CreateLink(AList[I]);
end;

procedure TdxServerModeDescriptors.Clear;
begin
  FList.Clear;
end;

function TdxServerModeDescriptors.Clone: TdxServerModeDescriptors;
begin
  Result := TdxServerModeDescriptorsClass(ClassType).Create(DataSource);
  Result.Assign(Self);
end;

function TdxServerModeDescriptors.Contains(AField: TField): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Field = AField;
    if Result then
      Break;
  end;
end;

function TdxServerModeDescriptors.IndexOf(AField: TField): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Field = AField then
    begin
      Result := I;
      Break;
    end;
end;

function TdxServerModeDescriptors.IndexOf(AItem: TdxServerModeDescriptor): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

function TdxServerModeDescriptors.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TdxServerModeDescriptors.IsEqual(AValue: TdxServerModeDescriptors): Boolean;
var
  I: Integer;
begin
  Result := (AValue = nil) and IsEmpty or (AValue <> nil) and (Count = AValue.Count);
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(AValue[I]);
      if not Result then
        Break;
    end;
end;

procedure TdxServerModeDescriptors.Remove(AField: TField);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AField);
  if AIndex >= 0 then
    RemoveItem(Items[AIndex]);
end;

procedure TdxServerModeDescriptors.AddItem(AItem: TdxServerModeDescriptor);
begin
  FList.Add(AItem);
  AItem.Owner := Self;
end;

procedure TdxServerModeDescriptors.RemoveItem(AItem: TdxServerModeDescriptor);
begin
  FList.Remove(AItem);
end;

procedure TdxServerModeDescriptors.DoChanged(AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, AItem, AChanges);
end;

function TdxServerModeDescriptors.GetDescriptorClass: TdxServerModeDescriptorClass;
begin
  Result := TdxServerModeDescriptor;
end;

function TdxServerModeDescriptors.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxServerModeDescriptors.GetItem(Index: Integer): TdxServerModeDescriptor;
begin
  Result := TdxServerModeDescriptor(FList[Index]);
end;

{ TdxServerModeGroupInfoDescriptor }

function TdxServerModeGroupInfoDescriptor.IsEqual(AValue: TdxServerModeDescriptor): Boolean;
begin
  Result := inherited IsEqual(AValue) and
    (AValue is TdxServerModeGroupInfoDescriptor) and
    (AValue.DateTimeGrouping = DateTimeGrouping);
end;

{ TdxServerModeGroupInfoDescriptors }

function TdxServerModeGroupInfoDescriptors.GetItem(Index: Integer): TdxServerModeGroupInfoDescriptor;
begin
  Result := TdxServerModeGroupInfoDescriptor(inherited Items[Index]);
end;

{ TdxServerModeSortInfoDescriptor }

constructor TdxServerModeSortInfoDescriptor.Create(AIsDesc, AIsKey: Boolean);
begin
  inherited Create;
  FIsDesc := AIsDesc;
  FIsKey := AIsKey;
end;

function TdxServerModeSortInfoDescriptor.IsEqual(AValue: TdxServerModeDescriptor): Boolean;
begin
  Result := inherited IsEqual(AValue) and (AValue is TdxServerModeSortInfoDescriptor) and
    (IsDesc = TdxServerModeSortInfoDescriptor(AValue).IsDesc);
end;

procedure TdxServerModeSortInfoDescriptor.CheckGroupValue(var AValue: Variant; AIsGroupValue: Boolean);

  function GetGroupValueRelativeToToday(const AValue: Variant): Variant;
  var
    ADate, ACheckDate: TDateTime;
    AToday: TDateTime;
    AYesterday, ALastYear, ALastMonth, ALastWeek, AStartOfYear, AStartOfMonth, AStartOfWeek: TDateTime;
    ATomorrow, ANextYear, ANext2Year, ANextMonth, ANext2Month, ANextWeek, ANext2Week: TDateTime;
  begin
    ADate := Today;
    AToday := dxDateOf(ADate);
    ACheckDate := dxDateOf(AValue);
    if ACheckDate = AToday then
      Result := ACheckDate
    else
      if ACheckDate > AToday then
      begin
        ATomorrow := ADate + 1;
        ANextYear := IncYear(StartOfTheYear(ADate), 1);
        ANext2Year := IncYear(StartOfTheYear(ADate), 2);
        ANextMonth := IncMonth(StartOfTheMonth(ADate), 1);
        ANext2Month := IncMonth(StartOfTheMonth(ADate), 2);
        ANextWeek := IncWeek(StartOfTheWeek(ADate), 1);
        ANext2Week := IncWeek(StartOfTheWeek(ADate), 1);
        if ACheckDate = ATomorrow then
          Result := ATomorrow
        else
          if ACheckDate < ANextWeek then
            Result := ANextWeek
          else
            if ACheckDate < ANext2Week then
              Result := ANext2Week
            else
              if ACheckDate < ANextMonth then
                Result := ANextMonth
              else
                if ACheckDate < ANext2Month then
                  Result := ANext2Month
                else
                  if ACheckDate < ANextYear then
                    Result := ANextYear
                  else
                    if ACheckDate < ANext2Year then
                      Result := ANext2Year
                    else
                      Result := ACheckDate;
      end
      else
      begin
        AYesterday := ADate - 1;
        AStartOfYear := StartOfTheYear(ADate);
        AStartOfMonth := StartOfTheMonth(ADate);
        AStartOfWeek := StartOfTheWeek(ADate);
        ALastYear := StartOfAYear(YearOf(ADate) - 1);
        ALastMonth := StartOfTheMonth(StartOfTheMonth(ADate) - 1);
        ALastWeek := StartOfTheWeek(ADate - 7);
        if ACheckDate >= AYesterday then
          Result := AYesterday
        else
          if ACheckDate >= AStartOfWeek then
            Result := AStartOfWeek
          else
            if ACheckDate >= ALastWeek then
              Result := ALastWeek
            else
              if ACheckDate >= AStartOfMonth then
                Result := AStartOfMonth
              else
                if ACheckDate >= ALastMonth then
                  Result := ALastMonth
                else
                  if ACheckDate >= AStartOfYear then
                    Result := AStartOfYear
                  else
                    if ACheckDate >= ALastYear then
                      Result := ALastYear
                    else
                      Result := ACheckDate;
      end;
  end;

begin
  if VarIsNull(AValue) then
    Exit;
  if IsDateTimeGrouping then
    case DateTimeGrouping of
      dgRelativeToToday:
        if AIsGroupValue then
          AValue := dxDateOf(AValue)
        else
          AValue := GetGroupValueRelativeToToday(dxDateOf(AValue));
      dgByDate:
        AValue := dxDateOf(AValue);
      dgByWeekDay:
        AValue := StartOfTheWeek(AValue);
      dgByMonth:
        AValue := StartOfTheMonth(AValue);
      dgByYear:
        AValue := StartOfTheYear(AValue);
      dgByHour:
        AValue := HourOf(AValue) / HoursPerDay;
    end;
end;

procedure TdxServerModeSortInfoDescriptor.DoAssign(Source: TdxServerModeDescriptor);
begin
  if Source is TdxServerModeSortInfoDescriptor then
  begin
    IsDesc := TdxServerModeSortInfoDescriptor(Source).IsDesc;
    FIsKey := TdxServerModeSortInfoDescriptor(Source).IsKey;
  end;
  inherited;
end;

function TdxServerModeSortInfoDescriptor.IsSorting: Boolean;
begin
  Result := True;
end;

function TdxServerModeSortInfoDescriptor.GetIsDesc: Boolean;
var
  AOwner: TdxServerModeSortInfoDescriptors;
begin
  if not IsKey then
    Result := FIsDesc
  else
  begin
    AOwner := Owner as TdxServerModeSortInfoDescriptors;
    if AOwner.WithoutKeySortInfoCount > 0 then
      Result := AOwner[AOwner.WithoutKeySortInfoCount - 1].IsDesc
    else
      Result := FIsDesc;
  end;
end;

procedure TdxServerModeSortInfoDescriptor.SetIsDesc(Value: Boolean);
begin
  if FIsDesc <> Value then
  begin
    FIsDesc := Value;
    Changed([dctSorting]);
  end;
end;

{ TdxServerModeSortInfoDescriptors }

function TdxServerModeSortInfoDescriptors.Add(AField: TField; AIsDesc: Boolean): TdxServerModeSortInfoDescriptor;
var
  AIndex: Integer;
begin
  AIndex := WithoutKeySortInfoCount;
  Result := Add(AField, AIsDesc, False);
  Result.Index := AIndex;
end;

function TdxServerModeSortInfoDescriptors.Add(const AFieldName: string;
  AIsDesc: Boolean): TdxServerModeSortInfoDescriptor;
begin
  Result := Add(DataSource.FieldDescriptors.FieldByName(AFieldName), AIsDesc);
end;

procedure TdxServerModeSortInfoDescriptors.ClearKeySortInfo;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].IsKey then
      RemoveItem(Items[I])
    else
      Break;
  end;
end;

function TdxServerModeSortInfoDescriptors.Clone(AReversed: Boolean): TdxServerModeSortInfoDescriptors;
var
  I: Integer;
begin
  Result := TdxServerModeSortInfoDescriptors(inherited Clone);
  if AReversed then
    for I := 0 to Result.Count - 1 do
      Result[I].IsDesc := not Result[I].IsDesc;
end;

function TdxServerModeSortInfoDescriptors.Add(AField: TField; AIsDesc: Boolean;
  AIsKey: Boolean): TdxServerModeSortInfoDescriptor;
begin
  Result := Add(AField) as TdxServerModeSortInfoDescriptor;
  Result.FIsDesc := AIsDesc;
  Result.FIsKey := AIsKey;
end;

function TdxServerModeSortInfoDescriptors.AddKeySortInfo(AField: TField;
  AIsDesc: Boolean): TdxServerModeSortInfoDescriptor;
begin
  Result := Add(AField, AIsDesc, True);
end;

function TdxServerModeSortInfoDescriptors.GetDescriptorClass: TdxServerModeDescriptorClass;
begin
  Result := TdxServerModeSortInfoDescriptor;
end;

function TdxServerModeSortInfoDescriptors.GetItem(Index: Integer): TdxServerModeSortInfoDescriptor;
begin
  Result := TdxServerModeSortInfoDescriptor(inherited Items[Index]);
end;

function TdxServerModeSortInfoDescriptors.GetWithoutKeySortInfoCount: Integer;
begin
  if Count > 0 then
  begin
    for Result := 0 to Count - 1 do
      if Items[Result].IsKey then
        Break;
  end
  else
    Result := 0;
end;

{ TdxServerModeSummaryDescriptor }

constructor TdxServerModeSummaryDescriptor.Create;
begin
  inherited Create;
  FGroupLevel := -1;
end;

constructor TdxServerModeSummaryDescriptor.Create(AKind: TcxSummaryKind);
begin
  inherited Create;
  FKind := AKind;
end;

function TdxServerModeSummaryDescriptor.IsEqual(AValue: TdxServerModeDescriptor): Boolean;
begin
  Result := inherited IsEqual(AValue) and
    (AValue is TdxServerModeSummaryDescriptor) and
    (TdxServerModeSummaryDescriptor(AValue).Kind = Kind) and
    (TdxServerModeSummaryDescriptor(AValue).Sorted = Sorted) and
    (TdxServerModeSummaryDescriptor(AValue).GroupLevel = GroupLevel);
end;

procedure TdxServerModeSummaryDescriptor.SetGroupLevel(Value: Integer);
begin
  if FGroupLevel <> Value then
  begin
    FGroupLevel := Value;
    if Sorted then
      Changed([dctSorting]);
  end;
end;

procedure TdxServerModeSummaryDescriptor.SetKind(Value: TcxSummaryKind);
begin
  if Kind <> Value then
  begin
    FKind := Value;
    Changed([dctSummaryKind]);
  end;
end;

procedure TdxServerModeSummaryDescriptor.SetSorted(Value: Boolean);
begin
  if Sorted <> Value then
  begin
    FSorted := Value;
    Changed([dctSorting]);
  end;
end;

procedure TdxServerModeSummaryDescriptor.DoAssign(Source: TdxServerModeDescriptor);
begin
  if Source is TdxServerModeSummaryDescriptor then
  begin
    Kind := TdxServerModeSummaryDescriptor(Source).Kind;
    Sorted := TdxServerModeSummaryDescriptor(Source).Sorted;
    GroupLevel := TdxServerModeSummaryDescriptor(Source).GroupLevel;
  end;
  inherited;
end;

function TdxServerModeSummaryDescriptor.IsSummary: Boolean;
begin
  Result := True;
end;

{ TdxServerModeSummaryDescriptors }

function TdxServerModeSummaryDescriptors.Add(AField: TField; AKind: TcxSummaryKind): TdxServerModeSummaryDescriptor;
begin
  Result := inherited Add(AField) as TdxServerModeSummaryDescriptor;
  Result.FKind := AKind;
end;

function TdxServerModeSummaryDescriptors.Add(const AFieldName: string;
  AKind: TcxSummaryKind): TdxServerModeSummaryDescriptor;
begin
  Result := Add(DataSource.FieldDescriptors.FieldByName(AFieldName), AKind);
end;

function TdxServerModeSummaryDescriptors.Clone: TdxServerModeSummaryDescriptors;
begin
  Result := TdxServerModeSummaryDescriptors(inherited Clone);
end;

function TdxServerModeSummaryDescriptors.IsGroupLevelSorted(AGroupLevel: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if (Items[I].GroupLevel = AGroupLevel) and Items[I].Sorted then
    begin
      Result := True;
      Break;
    end;
end;

function TdxServerModeSummaryDescriptors.GetDescriptorClass: TdxServerModeDescriptorClass;
begin
  Result := TdxServerModeSummaryDescriptor;
end;

function TdxServerModeSummaryDescriptors.GetItem(Index: Integer): TdxServerModeSummaryDescriptor;
begin
  Result := TdxServerModeSummaryDescriptor(inherited Items[Index]);
end;

{ TdxServerModeFieldDescriptor }

procedure TdxServerModeFieldDescriptor.DoAssign(Source: TdxServerModeDescriptor);
begin
  if Source is TdxServerModeFieldDescriptor then
    FField := TdxServerModeFieldDescriptor(Source).FField;
  inherited;
end;

function TdxServerModeFieldDescriptor.GetField: TField;
begin
  Result := FField;
end;

{ TdxServerModeFieldDescriptors }

function TdxServerModeFieldDescriptors.Add(AField: TField): TdxServerModeDescriptor;
begin
  Result := inherited Add(nil);
  TdxServerModeFieldDescriptor(Result).FField := AField;
end;

function TdxServerModeFieldDescriptors.FieldByName(const AFieldName: string): TField;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if CompareText(AFieldName, Items[I].Field.FieldName) = 0 then
    begin
      Result := Items[I].Field;
      Break;
    end;
end;

function TdxServerModeFieldDescriptors.IndexByName(const AFieldName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if CompareText(AFieldName, Items[I].Field.FieldName) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function TdxServerModeFieldDescriptors.ItemByField(AField: TField): TdxServerModeDescriptor;
var
  I: Integer;
begin
  I := IndexOf(AField);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

function TdxServerModeFieldDescriptors.GetItem(Index: Integer): TdxServerModeFieldDescriptor;
begin
  Result := TdxServerModeFieldDescriptor(inherited Items[Index]);
end;

function TdxServerModeFieldDescriptors.GetDescriptorClass: TdxServerModeDescriptorClass;
begin
  Result := TdxServerModeFieldDescriptor;
end;

{ TdxServerModeStateInfo }

constructor TdxServerModeStateInfo.Create(AGroupCount: Integer; ASortInfo: TdxServerModeSortInfoDescriptors);
begin
  inherited Create;
  FGroupCount := AGroupCount;
  FSortInfo := ASortInfo.Clone(False);
end;

destructor TdxServerModeStateInfo.Destroy;
begin
  FreeAndNil(FSortInfo);
  inherited Destroy;
end;

function TdxServerModeStateInfo.GetMaxEqualDepth(ACache: TdxServerModeCache): Integer;
var
  I: Integer;
begin
  Result := Min(ACache.GroupCount, GroupCount);
  for I := 0 to Result - 1 do
    if (SortInfo[I].Field <> ACache.SortInfo[I].Field) or
      (SortInfo[I].IsDateTimeGrouping and (SortInfo[I].DateTimeGrouping <> ACache.SortInfo[I].DateTimeGrouping)) then
    begin
      Result := I;
      Break;
    end;
end;

{ TdxServerModeCache }

constructor TdxServerModeCache.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
  FState := smcsConsistent;
end;

destructor TdxServerModeCache.Destroy;
begin
  FreeAndNil(FTopGroupInfo);
  inherited Destroy;
end;

procedure TdxServerModeCache.Clear;
begin
// do nothing
end;

procedure TdxServerModeCache.ClearGroupInfo;
begin
  if Assigned(FTopGroupInfo) then
    FTopGroupInfo.ResetChildren;
end;

procedure TdxServerModeCache.ClearTopGroupInfo;
begin
  if Assigned(FTopGroupInfo) then
    FreeAndNil(FTopGroupInfo);
end;

function TdxServerModeCache.GetGroupInfoList(AParentGroup: TdxServerModeGroupInfo): TdxServerModeGroupInfoList;
var
  AGroupInfo: TdxServerModeGroupInfo;
begin
  if AParentGroup = nil then
    AParentGroup := TopGroupInfo;
  AGroupInfo := AParentGroup;
  FillChildrenIfNeeded(AGroupInfo);
  Result := AGroupInfo.ChildrenGroups;
end;

function TdxServerModeCache.GetTotalSummary: TdxServerModeSummaryList;
begin
  Result := TopGroupInfo.Summary;
end;

function TdxServerModeCache.IsNothingButCount(ASummaries: TdxServerModeSummaryDescriptors): Boolean;
var
  I: Integer;
begin
  Result := not DataSource.IsIgnoreNullOnSummary;
  if Result then
    for I := 0 to ASummaries.Count - 1 do
      if ASummaries[I].Kind <> skCount then
      begin
        Result := False;
        Break;
      end;
end;

procedure TdxServerModeCache.RecalculateSummaries;

  procedure CalculateSummary(AParentGroup: TdxServerModeGroupInfo);
  var
    I, J: Integer;
    AGroupWhere: TdxServerModeCriteria;
    AOrdDs: TdxServerModeSortInfoDescriptor;
    AGroupSrc: TdxServerModeGroupInfoData;
    AGroup: TdxServerModeGroupInfo;
    AChildren: TdxServerModeGroupInfoDataList;
  begin
    if IsLanded or (AParentGroup.ChildrenGroups = nil) or (AParentGroup.ChildrenGroups.Count = 0) then
      Exit;

    AGroupWhere := GetGroupWhere(AParentGroup);
    try
      AOrdDs := SortInfo[AParentGroup.Level + 1];
      AChildren := PrepareChildren(AGroupWhere, AOrdDs, Summaries);
      try
        if AChildren.Count <> AParentGroup.ChildrenGroups.Count then
        begin
          ClearGroupInfo;
          Exit;
        end;
        for J := 0 to AChildren.Count - 1 do
        begin
          AGroupSrc := AChildren[J];
          AGroup := AParentGroup.ChildrenGroups[J];
          AGroup.Summary.Count := 0;
          for I := 0 to Summaries.Count - 1 do
            AGroup.Summary.Add(AGroupSrc.Summary[I]);
          CalculateSummary(AGroup);
        end;
      finally
        AChildren.Free;
      end;
    finally
      AGroupWhere.Free;
    end;
  end;

begin
  if IsLanded or (FTopGroupInfo = nil) then
    Exit;
  CalculateTotalSummaries(TopGroupInfo);
  CalculateSummary(TopGroupInfo);
end;

procedure TdxServerModeCache.SoftReset;
begin
  FState := smcsConsistent;
  Clear;
  ClearTopGroupInfo;
end;

function TdxServerModeCache.GetKeyFromRow(const ARow: TdxServerModeRow): Variant;
var
  I: Integer;
begin
  if KeyFields.Count = 0 then
    Result := Null
  else
    if KeyFields.Count = 1 then
      Result := EvaluateOnInstance(ARow, KeyFields[0].Field)
    else
    begin
      Result := VarArrayCreate([0, KeyFields.Count - 1], varVariant);
      for I := VarArrayLowBound(Result, 1) to VarArrayHighBound(Result, 1) do
        Result[I] := EvaluateOnInstance(ARow, KeyFields[I].Field);
    end;
end;

function TdxServerModeCache.CanTrickCreateTopGroupFromNextGroups: Boolean;
begin
  Result := (GroupCount > 0) and IsNothingButCount(TotalSummaries)
end;

function TdxServerModeCache.EvaluateOnInstance(const ARow: TdxServerModeRow; AField: TField): Variant;
begin
  if DataSource.FieldDescriptors.Count = 1 then
    Result := ARow
  else
    Result := ARow[AField.Index];
end;

procedure TdxServerModeCache.Fatal(AException: Exception);
begin
  if IsLanded then
    Exit;
  FState := smcsFatal;
  FLastErrorMessage := AException.Message;
  DataSource.StartErrorTimer;
  if Assigned(FOnExceptionThrown) then
    FOnExceptionThrown(Self, AException);
end;

function TdxServerModeCache.GetCount(ACriteria: TdxServerModeCriteria): Integer;
begin
  Result := DataSource.GetCount(ACriteria);
end;

function TdxServerModeCache.GetGroupWhere(AGroupInfo: TdxServerModeGroupInfo): TdxServerModeCriteria;
var
  ACurrentGroup: TdxServerModeGroupInfo;
begin
  ACurrentGroup := AGroupInfo;
  Result := DataSource.CreateCriteria;
  if ACurrentGroup.Level >= 0 then
  begin
    while ACurrentGroup.Level >= 0 do
    begin
      Result.AddItem(SortInfo[ACurrentGroup.Level], foEqual, ACurrentGroup.GroupValue);
      ACurrentGroup := ACurrentGroup.Parent;
    end;
  end;
  Result.AndCriteria(Filter);
end;

function TdxServerModeCache.GetTopGroupInfo: TdxServerModeGroupInfo;
begin
  if not Assigned(FTopGroupInfo) then
    FTopGroupInfo := CreateTopGroupInfo;
  Result := FTopGroupInfo;
end;

procedure TdxServerModeCache.MakeStrongClause(ACriteria: TdxServerModeCriteria; AParentList: TcxFilterCriteriaItemList;
  ADescriptor: TdxServerModeDescriptor; AIsDesc, ACanEqual: Boolean; const AValue: Variant);
const
  AOperationMap: array[Boolean, Boolean] of TcxFilterOperatorKind = ((foLess, foLessEqual),(foGreater, foGreaterEqual));
var
  AOperation: TcxFilterOperatorKind;
  AParent: TcxFilterCriteriaItemList;
begin
  if VarIsNull(AValue) then
  begin
    if AIsDesc and (DataSource.SQLAdapter.GetNullsSortOrder = nsoFirstIfAsc) or
      not AIsDesc and (DataSource.SQLAdapter.GetNullsSortOrder = nsoFirstIfDesc) then
      ACriteria.AddItem(AParentList, ADescriptor, foNotEqual, AValue);
  end
  else
    if VarType(AValue) = varBoolean then
    begin
      if AIsDesc then
        ACriteria.AddItem(AParentList, ADescriptor, foEqual, not AValue);
    end
    else
    begin
      AOperation := AOperationMap[AIsDesc, ACanEqual];
      if AIsDesc and (DataSource.SQLAdapter.GetNullsSortOrder = nsoFirstIfAsc) or
        not AIsDesc and (DataSource.SQLAdapter.GetNullsSortOrder = nsoFirstIfDesc) then
        ACriteria.AddItem(AParentList, ADescriptor, AOperation, AValue)
      else
      begin
        AParent := AParentList.AddItemList(fboOr);
        ACriteria.AddItem(AParent, ADescriptor, AOperation, AValue);
        ACriteria.AddItem(AParent, ADescriptor, foEqual, Null);
      end;
    end;
end;

procedure TdxServerModeCache.MakeStrongClause(ACriteria: TdxServerModeCriteria;
  AParentList: TcxFilterCriteriaItemList; ASortInfoDescriptor: TdxServerModeSortInfoDescriptor; const AValue: Variant);
begin
  MakeStrongClause(ACriteria, AParentList, ASortInfoDescriptor, ASortInfoDescriptor.IsDesc, False, AValue);
end;

function TdxServerModeCache.PrepareChildren(AGroupWhere: TdxServerModeCriteria;
  AGroupByDescriptor: TdxServerModeSortInfoDescriptor;
  ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoDataList;
begin
  Result := DataSource.PrepareChildren(AGroupWhere, AGroupByDescriptor, ASummaries);
end;

function TdxServerModeCache.PrepareTopGroupCount: Integer;
var
  ASummary: TdxServerModeSummaryDescriptors;
  AGroupInfo: TdxServerModeGroupInfoData;
begin
  ASummary := TdxServerModeSummaryDescriptors.Create(DataSource);
  try
    AGroupInfo := PrepareTopGroupInfo(ASummary);
    try
      Result := AGroupInfo.DataRowCount;
    finally
      AGroupInfo.Free;
    end;
  finally
    ASummary.Free;
  end;
end;

function TdxServerModeCache.PrepareTopGroupInfo(ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData;
begin
  Result := DataSource.PrepareTopGroupInfo(ASummaries);
end;

procedure TdxServerModeCache.RaiseInconsistencyDetected(const AFormat: string; Args: array of const);
var
  AMessage: string;
begin
  AMessage := Format(AFormat, Args);
  RaiseInconsistencyDetected(AMessage);
end;

procedure TdxServerModeCache.RaiseInconsistencyDetected(const AMessage: string);
begin
  if IsLanded then
    Exit;
  FState := smcsInconsistent;
  FLastErrorMessage := AMessage;
  DataSource.StartErrorTimer;
  if Assigned(FOnInconsistencyDetected) then
    FOnInconsistencyDetected(Self, AMessage);
end;

procedure TdxServerModeCache.ResetTopGroupInfo;
var
  I: Integer;
begin
  if Assigned(FTopGroupInfo) then
  begin
    ClearGroupInfo;
    FTopGroupInfo.DataRowCount := 0;
    for I := 0 to FTopGroupInfo.Summary.Count - 1 do
      FTopGroupInfo.Summary.List[I] := 0;
  end;
end;

procedure TdxServerModeCache.CalculateTotalSummaries(AGroupInfo: TdxServerModeGroupInfo);
var
  ATopGroupInfoData: TdxServerModeGroupInfoData;
  I, ATotalCount: Integer;
  AChildren: TdxServerModeGroupInfoList;
  ASummaries: TdxServerModeRow;
begin
  if IsLanded then
    Exit;
  ATopGroupInfoData := nil;
  if CanTrickCreateTopGroupFromNextGroups then
  begin
    AGroupInfo.DataRowCount := -1;
    AChildren := GetGroupInfoList(AGroupInfo);
    ATotalCount := 0;
    for I := 0 to AChildren.Count - 1 do
      Inc(ATotalCount, AChildren[I].DataRowCount);
    ASummaries := VarArrayCreate([0, TotalSummaries.Count - 1], varVariant);
    for I := 0 to TotalSummaries.Count - 1 do
      ASummaries[I] := ATotalCount;
    ATopGroupInfoData := TdxServerModeGroupInfoData.Create(Null, ATotalCount, ASummaries);
  end
  else
    try
      ATopGroupInfoData := PrepareTopGroupInfoWithTrick(TotalSummaries);
    except
      on E : Exception do
      begin
        ATopGroupInfoData.Free;
        Fatal(E);
        Exit;
      end;
    end;
  try
    AGroupInfo.DataRowCount := ATopGroupInfoData.DataRowCount;
    AGroupInfo.Summary.Count := 0;
    for I := 0 to TotalSummaries.Count - 1 do
      AGroupInfo.Summary.Add(ATopGroupInfoData.Summary[I]);
  finally
    ATopGroupInfoData.Free;
  end;
end;

procedure TdxServerModeCache.CheckTopGroupInfo(APrevStateInfo: TdxServerModeStateInfo);

  procedure CheckChildren(AParent: TdxServerModeGroupInfo; ANewSortInfo,
    APrevSortInfo: TdxServerModeSortInfoDescriptors; ACurrentDepth, AMaxDepth: Integer);
  var
    I: Integer;
    AChildren: TdxServerModeGroupInfoList;
  begin
    AChildren := AParent.ChildrenGroups;
    if AChildren = nil then
      Exit;
    if (ACurrentDepth = AMaxDepth) or (AChildren.Count = 0) and (ACurrentDepth < AMaxDepth) then
    begin
      AParent.ResetChildren;
      Exit;
    end;
    if ANewSortInfo[ACurrentDepth].IsDesc <> APrevSortInfo[ACurrentDepth].IsDesc then
      AChildren.Reverse;
    AChildren.UpdateDataRowIndexes;
    for I := 0 to AChildren.Count - 1 do
      CheckChildren(AChildren[I], ANewSortInfo, APrevSortInfo, ACurrentDepth + 1, AMaxDepth);
  end;

var
  AMaxDepth: Integer;
  ATopGroupInfo: TdxServerModeGroupInfo;
begin
  if IsLanded then
    Exit;
  ATopGroupInfo := FTopGroupInfo;
  if (ATopGroupInfo = nil) or (ATopGroupInfo.DataRowCount = 0) then
    Exit;
  AMaxDepth := APrevStateInfo.GetMaxEqualDepth(Self);
  CheckChildren(ATopGroupInfo, SortInfo, APrevStateInfo.SortInfo, 0, AMaxDepth);
end;

function TdxServerModeCache.CreateTopGroupInfo: TdxServerModeGroupInfo;
begin
  Result := TdxServerModeGroupInfo.Create(nil, Null, -1, 0);
  if IsLanded then
    Exit;
  CalculateTotalSummaries(Result);
end;

procedure TdxServerModeCache.FillChildrenIfNeeded(AGroupInfo: TdxServerModeGroupInfo);
var
  AGroupWhere: TdxServerModeCriteria;
  AOrdDs: TdxServerModeSortInfoDescriptor;
  AGroupSrc: TdxServerModeGroupInfoData;
  ANextGroup: TdxServerModeGroupInfo;
  AChildren: TdxServerModeGroupInfoDataList;
  ACurrentTop: Integer;
  I, J: Integer;
begin
  if AGroupInfo.ChildrenGroups <> nil then
    Exit;
  AGroupInfo.CreateChildren;
  if IsLanded or (AGroupInfo.Level + 1 >= GroupCount) or (AGroupInfo.DataRowCount = 0) then
    Exit;
  AChildren := nil;
  AGroupWhere := GetGroupWhere(AGroupInfo);
  try
    AOrdDs := SortInfo[AGroupInfo.Level + 1];
    try
      try
        AChildren := PrepareChildren(AGroupWhere, AOrdDs, Summaries);
      except
        on E : Exception do
        begin
          Fatal(E);
          Exit;
        end;
      end;
      ACurrentTop := AGroupInfo.TopDataRowIndex;
      for J := 0 to AChildren.Count - 1 do
      begin
        AGroupSrc := AChildren[J];
        ANextGroup := TdxServerModeGroupInfo.Create(AGroupInfo, AGroupSrc.GroupValue, AGroupInfo.Level + 1, ACurrentTop);
        ANextGroup.DataRowCount := AGroupSrc.DataRowCount;
        Inc(ACurrentTop, ANextGroup.DataRowCount);
        for I := 0 to Summaries.Count - 1 do
          ANextGroup.Summary.Add(AGroupSrc.Summary[I]);
        AGroupInfo.ChildrenGroups.Add(ANextGroup);
      end;
    finally
      AChildren.Free;
    end;
  finally
    AGroupWhere.Free;
  end;
  if AGroupInfo.DataRowCount < 0 then
  else
    if ACurrentTop - AGroupInfo.TopDataRowIndex <> AGroupInfo.DataRowCount then
      RaiseInconsistencyDetected(sdxInconsistencyWrongNestedGroupsRowCount,
        [ACurrentTop - AGroupInfo.TopDataRowIndex, AGroupInfo.DataRowCount]);
end;

function TdxServerModeCache.GetFilter: TdxServerModeCriteria;
begin
  Result := DataSource.GeneralFilter;
end;

function TdxServerModeCache.GetGroupCount: Integer;
begin
  Result := DataSource.GroupCount;
end;

function TdxServerModeCache.GetIsLanded: Boolean;
begin
  Result := State <> smcsConsistent;
end;

function TdxServerModeCache.GetInternalCount: Integer;
begin
  Result := TopGroupInfo.DataRowCount;
end;

function TdxServerModeCache.GetKeyFields: TdxServerModeFieldDescriptors;
begin
  Result := DataSource.KeyFields;
end;

function TdxServerModeCache.GetSortInfo: TdxServerModeSortInfoDescriptors;
begin
  Result := DataSource.SortInfo;
end;

function TdxServerModeCache.GetSummaries: TdxServerModeSummaryDescriptors;
begin
  Result := DataSource.Summaries;
end;

function TdxServerModeCache.GetTotalSummaries: TdxServerModeSummaryDescriptors;
begin
  Result := DataSource.TotalSummaries;
end;

function TdxServerModeCache.PrepareTopGroupInfoWithTrick(ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData;
var
  I, AGroupCount: Integer;
  ASummary: TdxServerModeRow;
begin
  if IsNothingButCount(ASummaries) then
  begin
    AGroupCount := PrepareTopGroupCount;
    ASummary := VarArrayCreate([0, ASummaries.Count - 1], varVariant);
    for I := 0 to ASummaries.Count - 1 do
      ASummary[I] := AGroupCount;
    Result := TdxServerModeGroupInfoData.Create(Null, AGroupCount, ASummary);
  end
  else
    Result := PrepareTopGroupInfo(ASummaries);
end;

{ TdxServerModeKeyedCache }

constructor TdxServerModeKeyedCache.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited;
  FKeysByIndex := TdxServerModeKeysDictionary.Create;
  FRowsByIndex := TdxServerModeRowsDictionary.Create;
  FSafeModel := TdxServerModeServerAndChannelModel.Create(0.0, 1.0, 0.0);
  FRegressor := TdxServerModeServerAndChannelModelBuilder.Create;
  ActualFetchRowsIsGood := bNull;
  FLastUsedRecordIndex := -1;
end;

destructor TdxServerModeKeyedCache.Destroy;
begin
  FreeAndNil(FRegressor);
  FreeAndNil(FSafeModel);
  FreeAndNil(FKeysByIndex);
  FreeAndNil(FRowsByIndex);
  inherited Destroy;
end;

procedure TdxServerModeKeyedCache.Clear;
begin
  RowsByIndex.Clear;
  KeysByIndex.Clear;
  FLastUsedRecordIndex := -1;
end;

procedure TdxServerModeKeyedCache.SoftReset;
begin
  inherited SoftReset;
  FreeAndNil(FRegressor);
  FRegressor := TdxServerModeServerAndChannelModelBuilder.Create;
  ActualFetchRowsIsGood := bNull;
end;

function TdxServerModeKeyedCache.DecideIsFetchRowsGoodIdea(ASortInfo: TdxServerModeSortInfoDescriptors): Boolean;
begin
  Result := ASortInfo.Count <= 2;
end;

function TdxServerModeKeyedCache.GetRowKey(ARecordIndex: Integer): Variant;
var
  AGroupInfo: TdxServerModeGroupInfo;
  AIsFromBottom, ASkipIsFromBottom, APureTakeIsFromBottom: Boolean;
  APositionInGroup, APositionFromBottom, AFetchPosition, AFetchCount, APureTake, ASkipSkip, ASkipTake: Integer;
begin
  Result := Unassigned;
  if (ARecordIndex < 0) or (ARecordIndex >= Count) or IsLanded then
    Exit;
  if KeysByIndex.TryGetValue(ARecordIndex, Result) then
    Exit;
  AGroupInfo := GetGroupForKeysFetchingAround(ARecordIndex);
  if IsLanded then
    Exit;
  if (AGroupInfo.DataRowCount <= MagicNumberFetchRowsAllThreshold) and IsFetchRowsGoodIdeaForSure then
  begin
    FetchRowsAll(AGroupInfo);
    if not KeysByIndex.TryGetValue(ARecordIndex, Result) and not IsLanded then
      InvalidOperation(sdxInternalErrorKeyAbsentFetchRowsAll);
    Exit;
  end;
  APositionInGroup := ARecordIndex - AGroupInfo.TopDataRowIndex;
  APositionFromBottom := AGroupInfo.DataRowCount - APositionInGroup - 1;
  AIsFromBottom := APositionInGroup > APositionFromBottom;
  if AIsFromBottom then
    AFetchPosition := APositionFromBottom
  else
    AFetchPosition := APositionInGroup;
  AFetchCount := AFetchPosition + 1;
  if (AFetchCount <= MagicNumberFetchRowsTopThreshold) and IsFetchRowsGoodIdeaForSure then
  begin
    FetchRowsTop(AGroupInfo, AIsFromBottom, MagicNumberFetchRowsTop);
    if not KeysByIndex.TryGetValue(ARecordIndex, Result) and not IsLanded then
      InvalidOperation(sdxInternalErrorKeyAbsentFetchRowsTop);
    Exit;
  end;
  CalculateTake(AGroupInfo, AFetchCount, AIsFromBottom, APureTake, APureTakeIsFromBottom);
  CalculateModel(ARecordIndex, APositionInGroup, AGroupInfo, ASkipTake, ASkipSkip, ASkipIsFromBottom);
  PopulateFetchKeys(AGroupInfo, APureTake, ASkipSkip, ASkipTake, ASkipIsFromBottom, APureTakeIsFromBottom);
  if not KeysByIndex.TryGetValue(ARecordIndex, Result) and not IsLanded then
    InvalidOperation(sdxInternalErrorKeyAbsentDoFetchKeys);
end;

function TdxServerModeKeyedCache.Indexer(ARecordIndex: Integer): TdxServerModeRow;
var
  I, AInSize, ASplitLen, AOldIndex: Integer;
  ARows: TdxServerModeRowList;
  AKeysToFetch: TdxServerModeKeyList;
  AFillKey: TdxServerModeRow;
  AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary;
  AFillFromSplit: Boolean;
begin
  Result := Null;
  if IsLanded then
    Exit;
  if FindFromCache(ARecordIndex, Result) then
    Exit;
  AInSize := Min(MagicNumberFetchRowsInSize, MaxInSize);
  AKeysToFetch := TdxServerModeKeyList.Create;
  try
    AIndexesByKeys := TdxServerModeLoadedKeysChunkDictionary.Create;
    try
      if AInSize = 1 then
        ASplitLen := 1
      else
        ASplitLen := AInSize * 2 div 3;
      PopulateDownKeysToFetch(AKeysToFetch, AIndexesByKeys, ARecordIndex, ASplitLen, AFillFromSplit);
      if IsLanded then
        Exit;
      Assert(AKeysToFetch.Count > 0, 'KeysToFetch.Count = 0!');
      PopulateUpKeysToFetch(AKeysToFetch, AIndexesByKeys, ARecordIndex, AInSize);
      if IsLanded then
        Exit;
      if AFillFromSplit then
      begin
        I := ARecordIndex + ASplitLen;
        while AKeysToFetch.Count < AInSize do
        begin
          if RowsByIndex.ContainsKey(I) then
            Break;
          if not KeysByIndex.TryGetValue(I, AFillKey) then
            Break;
          if AIndexesByKeys.TryGetValue(AFillKey, AOldIndex) then
          begin
            RaiseInconsistencyDetected(sdxInconsistencyKeyFoundTwice,
              [dxKeyToText(AFillKey), dxKeyToText(AOldIndex), I]);
            Exit;
          end;
          AKeysToFetch.Add(AFillKey);
          AIndexesByKeys.Add(AFillKey, I);
          Inc(I);
        end;
      end;
      ARows := TdxServerModeRowList.Create;
      try
        try
          FetchRowsByKeys(ARows, AKeysToFetch);
        except
          on E : Exception do
          begin
            Fatal(E);
            Exit;
          end;
        end;
        if IsLanded then
          Exit;
        if ARows.Count <> AKeysToFetch.Count then
        begin
          RaiseInconsistencyDetected(sdxInconsistencyWrongRowCountOfKeys,
            [ARows.Count, AKeysToFetch.Count]);
          Exit;
        end;
        Result := PopulateRowsByIndex(ARows, AIndexesByKeys, ARecordIndex);
        if IsLanded then
          Exit;
        if VarIsNull(Result) then
          InvalidOperation(sdxInternalErrorResultValueNotFound);
      finally
        ARows.Free;
      end;
    finally
      AIndexesByKeys.Free;
    end;
  finally
    AKeysToFetch.Free;
  end;
  FLastUsedRecordIndex := ARecordIndex;
  FLastUsedRow := Result;
end;

function TdxServerModeKeyedCache.Contains(const AValue: TdxServerModeRow): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TdxServerModeKeyedCache.GetRowIndexByKey(const AKey: Variant): Integer;

  function IsAllKeysFetched(AGroupInfo: TdxServerModeGroupInfo): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to AGroupInfo.DataRowCount - 1 do
      if not KeysByIndex.ContainsKey(I + AGroupInfo.TopDataRowIndex) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  AIndex: Integer;
  ARow: TdxServerModeRow;
  AGroupInfo: TdxServerModeGroupInfo;
  ACount: Integer;
  AGuessedIndex: Integer;
  AGuessedRowKey: Variant;
begin
  Result := -1;
  if VarIsEmpty(AKey) or IsLanded then
    Exit;
  if KeysByIndex.TryGetKeyByValue(AKey, AIndex) then
  begin
    Result := AIndex;
    Exit;
  end;
  ARow := FetchRowByKey(AKey);
  if IsLanded or VarIsNull(ARow) then
    Exit;
  AGroupInfo := GetRowGroup(ARow);
  if IsLanded then
    Exit;
  if IsAllKeysFetched(AGroupInfo) then
  begin
    if KeysByIndex.TryGetKeyByValue(AKey, AIndex) then
    begin
      Result := AIndex;
      Exit;
    end;
    RaiseInconsistencyDetected(sdxInconsistencyCantFindKeyInCompletelyFetchedGroup,
      [dxKeyToText(AKey)]);
    Exit;
  end;

  if AGroupInfo.DataRowCount <= MagicNumberFetchKeysAllThreshold then
  begin
    if(AGroupInfo.DataRowCount <= MagicNumberFetchRowsAllThreshold) and IsFetchRowsGoodIdeaForSure then
      FetchRowsAll(AGroupInfo)
    else
      DoFetchKeys(AGroupInfo, False, 0, 0);
    if KeysByIndex.TryGetKeyByValue(AKey, AIndex) then
    begin
      Result := AIndex;
      Exit;
    end;
    if IsLanded then
      Exit;
    RaiseInconsistencyDetected(sdxInconsistencyCantFindKeyInNewlyFetchedGroup,
      [dxKeyToText(AKey)]);
    Exit;
  end;
  ACount := GetCountBeforeRow(ARow, AGroupInfo, SortInfo);

  AGuessedIndex := AGroupInfo.TopDataRowIndex + ACount;
  AGuessedRowKey := GetRowKey(AGuessedIndex);
  if KeyEquals(AKey, AGuessedRowKey) then
  begin
    Result := AGuessedIndex;
    Exit;
  end;
  if KeysByIndex.TryGetKeyByValue(AKey, AIndex) then
    Result := AIndex;
end;

function TdxServerModeKeyedCache.IndexOf(const AValue: TdxServerModeRow): Integer;
begin
  if VarIsNull(AValue) then
    Result := -1
  else
    Result := GetRowIndexByKey(GetKeyFromRow(AValue));
end;

function TdxServerModeKeyedCache.KeyEquals(const AKey1, AKey2: Variant): Boolean;
begin
  Result := VarEquals(AKey1, AKey2);
end;

function TdxServerModeKeyedCache.DecideIsFetchRowsGoodIdeaForSure: Boolean;
begin
  Result := DecideIsFetchRowsGoodIdea(SortInfo);
end;

procedure TdxServerModeKeyedCache.FetchRowsByKeys(ARows: TdxServerModeRowList; AKeys: TdxServerModeKeyList);
begin
  DataSource.FetchRowsByKeys(ARows, AKeys);
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchKeysAllThreshold: Integer;
begin
  Result := dxDefaultMagicNumberFetchKeysAllThreshold;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchKeysModulo: Integer;
begin
  Result := dxDefaultMagicNumberFetchKeysModulo;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchRowsAllThreshold: Integer;
begin
  Result := dxDefaultMagicNumberFetchRowsAllThreshold;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchRowsInSize: Integer;
begin
  Result := dxDefaultMagicNumberFetchRowsInSize;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchRowsTop: Integer;
begin
  Result := dxDefaultMagicNumberFetchRowsTop;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchRowsTopThreshold: Integer;
begin
  Result := dxDefaultMagicNumberFetchRowsTopThreshold;
end;

function TdxServerModeKeyedCache.GetMagicNumberScanKeysBase: Integer;
begin
  Result := dxDefaultMagicNumberScanKeysBase;
end;

function TdxServerModeKeyedCache.GetMagicNumberTakeIncreaseStepMultiplier: Double;
begin
  Result := dxDefaultMagicNumberTakeIncreaseStepMultiplier;
end;

function TdxServerModeKeyedCache.GetMagicNumberTakeKeysUpperLimitAfterSkip: Integer;
begin
  Result := dxDefaultMagicNumberTakeKeysUpperLimitAfterSkip;
end;

function TdxServerModeKeyedCache.IsFetchRowsGoodIdeaForSure: Boolean;
const
  AMap: array[Boolean] of TdxNullableBoolean = (bFalse, bTrue);
begin
  if ActualFetchRowsIsGood = bNull then
  begin
    if FetchRowsIsGood = bTrue then
      ActualFetchRowsIsGood := FetchRowsIsGood
    else
      ActualFetchRowsIsGood := AMap[DecideIsFetchRowsGoodIdeaForSure];
  end;
  Result := ActualFetchRowsIsGood = bTrue;
end;

procedure TdxServerModeKeyedCache.FetchKeys(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer);
begin
  DataSource.FetchKeys(AKeys, AWhere, ASortInfo, ASkip, ATake);
end;

procedure TdxServerModeKeyedCache.FetchRows(ARows: TdxServerModeRowList; AWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors; ATake: Integer);
begin
  DataSource.FetchRows(ARows, AWhere, ASortInfo, ATake);
end;

procedure TdxServerModeKeyedCache.FetchRows(AWhere: TdxServerModeCriteria;
  ATake, AValidateCount, AFirstRecord: Integer; AIsFromBottom: Boolean);
var
  ACurrentRow, AIncrement: Integer;
  I: Integer;
  AFetchedKey, AOldKey: Variant;
  AFetchedRow: TdxServerModeRow;
  ASortInfo: TdxServerModeSortInfoDescriptors;
  ARows: TdxServerModeRowList;
begin
  ASortInfo := SortInfo.Clone(AIsFromBottom);
  try
    ARows := TdxServerModeRowList.Create;
    try
      try
        FetchRows(ARows, AWhere, ASortInfo, ATake);
      except
        on E : Exception do
        begin
          Fatal(E);
          Exit;
        end;
      end;
      if ARows.Count <> AValidateCount then
      begin
        RaiseInconsistencyDetected(sdxInconsistencyUnexpectedNumberOfRows,
          [ARows.Count, AValidateCount]);
        Exit;
      end;
      ACurrentRow := AFirstRecord;
      if AIsFromBottom then
        AIncrement := -1
      else
        AIncrement := 1;
      for I := 0 to ARows.Count - 1 do
      begin
        AFetchedRow := ARows[I];
        AFetchedKey := GetKeyFromRow(AFetchedRow);
        if KeysByIndex.TryGetValue(ACurrentRow, AOldKey) then
        begin
          if not KeyEquals(AOldKey, AFetchedKey) then
          begin
            RaiseInconsistencyDetected(sdxInconsistencyWrongKeyByRowIndex,
              [dxKeyToText(AOldKey), ACurrentRow, dxKeyToText(AFetchedKey)]);
            Exit;
          end;
        end
        else
        begin
          KeysByIndex.Add(ACurrentRow, AFetchedKey);
          if not RowsByIndex.ContainsKey(ACurrentRow) then
            RowsByIndex.Add(ACurrentRow, AFetchedRow);
        end;
        Inc(ACurrentRow, AIncrement);
      end;
      KeysByIndex.LoadedRecordsHelper.Add(AFirstRecord, ARows.Count, AIsFromBottom);
    finally
      ARows.Free;
    end;
  finally
    ASortInfo.Free;
  end;
end;

function TdxServerModeKeyedCache.FetchRowByKey(const AKey: Variant): TdxServerModeRow;
var
  AKeys: TdxServerModeKeyList;
  ACriteria: TdxServerModeCriteria;
  ARows: TdxServerModeRowList;
begin
  Result := Null;
  AKeys := TdxServerModeKeyList.Create;
  try
    AKeys.Add(AKey);
    ACriteria := DataSource.GetFetchRowsByKeysCondition(AKeys);
    try
      ACriteria.AndCriteria(Filter);
      ARows := TdxServerModeRowList.Create;
      try
        FetchRows(ARows, ACriteria, SortInfo, 1);
        if ARows.Count = 0 then
          Exit;
        Result := ARows[0];
      finally
        ARows.Free;
      end;
    finally
      ACriteria.Free;
    end;
  finally
    AKeys.Free;
  end;
end;

function TdxServerModeKeyedCache.GetRowGroup(const ARow: TdxServerModeRow): TdxServerModeGroupInfo;
var
  AGroups: TdxServerModeGroupInfoList;
  AGroupLevel: Integer;
  ARowValue, AGroupValue: Variant;
  ASortInfo: TdxServerModeSortInfoDescriptor;
  I: Integer;
begin
  Result := TopGroupInfo;
  repeat
    AGroupLevel := Result.Level + 1;
    if AGroupLevel >= GroupCount then
      Break;
    ASortInfo := SortInfo[AGroupLevel];
    ARowValue := EvaluateOnInstance(ARow, ASortInfo.Field);
    ASortInfo.CheckGroupValue(ARowValue, False);
    AGroups := GetGroupInfoList(Result);
    if IsLanded then
      Exit;
    Result := nil;
    for I := 0 to AGroups.Count - 1 do
    begin
      AGroupValue := AGroups[I].GroupValue;
      ASortInfo.CheckGroupValue(AGroupValue, True);
      if KeyEquals(ARowValue, AGroupValue) then
      begin
        Result := AGroups[I];
        Break;
      end;
    end;
    if Result = nil then
    begin
      RaiseInconsistencyDetected(sdxInconsistencyCantFindAppropriateGroupForKey,
        [dxKeyToText(GetKeyFromRow(ARow))]);
      Exit;
    end;
  until False;
end;

function TdxServerModeKeyedCache.PrepareTopGroupCount: Integer;
begin
  Result := GetCount(Filter);
end;

procedure TdxServerModeKeyedCache.FetchRowsAll(AGroupInfo: TdxServerModeGroupInfo);
var
  AWhere: TdxServerModeCriteria;
begin
  AWhere := GetGroupWhere(AGroupInfo);
  try
    FetchRows(AWhere, AGroupInfo.DataRowCount + 1, AGroupInfo.DataRowCount, AGroupInfo.TopDataRowIndex, False);
  finally
    AWhere.Free;
  end;
end;

procedure TdxServerModeKeyedCache.FetchRowsTop(AGroupInfo: TdxServerModeGroupInfo; AIsFromBottom: Boolean; ATop: Integer);
var
  AWhere: TdxServerModeCriteria;
  AFirstRecord: Integer;
begin
  AWhere := GetGroupWhere(AGroupInfo);
  try
    if AIsFromBottom then
      AFirstRecord := AGroupInfo.TopDataRowIndex + AGroupInfo.DataRowCount - 1
    else
      AFirstRecord := AGroupInfo.TopDataRowIndex;
    FetchRows(AWhere, ATop, ATop, AFirstRecord, AIsFromBottom);
  finally
    AWhere.Free;
  end;
end;

function TdxServerModeKeyedCache.FindFromCache(ARecordIndex: Integer; var ARow: TdxServerModeRow): Boolean;
var
  AKey: Variant;
begin
  if FLastUsedRecordIndex = ARecordIndex then
  begin
    ARow := FLastUsedRow;
    Result := True;
    Exit;
  end;
  Result := RowsByIndex.TryGetValue(ARecordIndex, ARow);
  if not Result then
  begin
    AKey := GetRowKey(ARecordIndex);
    if not VarIsEmpty(AKey) then
      Result := RowsByIndex.TryGetValue(ARecordIndex, ARow) ;
  end;
  if Result then
  begin
    FLastUsedRecordIndex := ARecordIndex;
    FLastUsedRow := ARow;
  end;
end;

function TdxServerModeKeyedCache.GetCountBeforeRow(const ARow: TdxServerModeRow;
  AGroupInfo: TdxServerModeGroupInfo; ASortInfo: TdxServerModeSortInfoDescriptors): Integer;

  function FindGroupInfo(AField: TField; AInfo: TdxServerModeGroupInfo): TdxServerModeGroupInfo;
  begin
    if (AInfo <> nil) and (AInfo.Level >= 0) then
    begin
      if SortInfo[AInfo.Level].Field = AField then
        Result := AInfo
      else
        Result := FindGroupInfo(AField, AInfo.Parent);
    end
    else
      Result := nil;
  end;

var
  AGroupWhere: TdxServerModeCriteria;
  AParentList: TcxFilterCriteriaItemList;
  ASortInfoDescriptor: TdxServerModeSortInfoDescriptor;
  ADescriptorIsGroup: Boolean;
  AValue: Variant;
  I: Integer;
  AList: TcxObjectList;
begin
  AList := TcxObjectList.Create;
  AGroupWhere := GetGroupWhere(AGroupInfo);
  try
    AParentList := AGroupWhere.Root;

    for I := 0 to ASortInfo.Count - 1 do
    begin
      ASortInfoDescriptor := ASortInfo[I];
      ADescriptorIsGroup := FindGroupInfo(ASortInfoDescriptor.Field, AGroupInfo) <> nil;
      if ADescriptorIsGroup and not ASortInfoDescriptor.IsDateTimeGrouping then
        Continue;
      AValue := EvaluateOnInstance(ARow, ASortInfoDescriptor.Field);
      if not ADescriptorIsGroup then
        ASortInfoDescriptor.CheckGroupValue(AValue, False)
      else
        if ASortInfoDescriptor.IsDateTimeGrouping then
        begin
          ASortInfoDescriptor := TdxServerModeSortInfoDescriptor(ASortInfoDescriptor.Clone);
          if ASortInfoDescriptor.DateTimeGrouping = dgByHour then
            AValue := TimeOf(AValue)
          else
            ASortInfoDescriptor.DateTimeGrouping := dgDefault;
          AList.Add(ASortInfoDescriptor);
        end;

      AParentList := AParentList.AddItemList(fboOr);
      MakeStrongClause(AGroupWhere, AParentList, ASortInfoDescriptor, AValue);

      if I < ASortInfo.Count - 1 then
      begin
        AParentList := AParentList.AddItemList(fboAnd);
        AGroupWhere.AddItem(AParentList, ASortInfoDescriptor, foEqual, AValue);
      end;
    end;

    Result := GetCount(AGroupWhere);
  finally
    AGroupWhere.Free;
    AList.Free;
  end;
end;

function TdxServerModeKeyedCache.GetGroupForKeysFetchingAround(Index: Integer): TdxServerModeGroupInfo;
var
  ANextGroupInfo, AGroupInfo: TdxServerModeGroupInfo;
  AGroups: TdxServerModeGroupInfoList;
  I: Integer;
begin
   Result := TopGroupInfo;
   repeat
     if Result.Level + 1 >= GroupCount then
       Break;
     ANextGroupInfo := nil;
     AGroups := GetGroupInfoList(Result);
     for I := 0 to AGroups.Count - 1 do
     begin
       AGroupInfo := AGroups[I];
       if AGroupInfo.TopDataRowIndex + AGroupInfo.DataRowCount <= Index then
         Continue;
       ANextGroupInfo := AGroupInfo;
       Break;
     end;
     if IsLanded then
       Break;
     if ANextGroupInfo = nil then
     begin
       RaiseInconsistencyDetected(sdxInconsistencyCantFindAppropriateGroupForRow, [Index]);
       Break;
     end;
     Result := ANextGroupInfo;
   until False;
end;

function TdxServerModeKeyedCache.GetMagicNumberAllowedSlowerThenBase: Double;
begin
  Result := 1.6;
end;

function TdxServerModeKeyedCache.GetMagicNumberAllowedSlowerThenBaseVariance: Double;
begin
  Result := 0.5;
end;

function TdxServerModeKeyedCache.GetMagicNumberFetchKeysTopPenaltyGap: Integer;
begin
  Result := 100;
end;

function TdxServerModeKeyedCache.GetMagicNumberTakeKeysBase: Integer;
begin
  Result := 256;
end;

function TdxServerModeKeyedCache.GetMaxInSize: Integer;
begin
  Result := High(Integer);
end;

procedure TdxServerModeKeyedCache.CalculateModel(ARecordIndex, APositionInGroup: Integer;
  AGroupInfo: TdxServerModeGroupInfo; out ASkipTake, ASkipSkip: Integer; out ASkipIsFromBottom: Boolean);
var
  ADraftScan, AMinPosInGroupToFetch, ARealScan, AMaxPosInGroupToFetch, AMaxAllowedTake, AMaxObservableTake,
  AStartPopulatedIndex, AFinishPopulatedIndex: Integer;
  AChosenResult: TdxServerModeOptimalFetchResult;
  AOptimalFetchParam: TdxServerModeOptimalFetchParam;
  AModel: TdxServerModeServerAndChannelModel;
begin
  ASkipSkip := -1;
  ASkipTake := -1;
  if UseTakeEnforcer <> bTrue then
  begin
    AModel := Regressor.Resolve;
    try
      if AModel = nil then
        AModel := SafeModel;
      KeysByIndex.LoadedRecordsHelper.GetClosestPopulatedIndexes(ARecordIndex, AStartPopulatedIndex, AFinishPopulatedIndex);
      AMinPosInGroupToFetch := Math.Max(AStartPopulatedIndex - AGroupInfo.TopDataRowIndex + 1, 0);
      AMaxPosInGroupToFetch := Math.Min(AFinishPopulatedIndex - AGroupInfo.TopDataRowIndex,
        AGroupInfo.DataRowCount) - 1;
      AMaxObservableTake := Regressor.GetMaxObservableTake;
      if AMaxObservableTake < 0 then
        AMaxObservableTake := 0;
      AMaxAllowedTake := Math.Min(MagicNumberTakeKeysUpperLimitAfterSkip,
        Math.Max(AMaxObservableTake, 1024) * LongInt(4));
      if MinimizeSingleRequestTransferSizeInsteadOfOverallOptimization then
        AOptimalFetchParam := TdxServerModeOptimalFetchParam.Create(SafeModel, APositionInGroup, AMinPosInGroupToFetch,
          AMaxPosInGroupToFetch, AGroupInfo.DataRowCount, 64, AMaxAllowedTake, 1, 1, 1)
      else
        AOptimalFetchParam := TdxServerModeOptimalFetchParam.Create(AModel, APositionInGroup, AMinPosInGroupToFetch,
          AMaxPosInGroupToFetch, AGroupInfo.DataRowCount, 128, AMaxAllowedTake, 1.66, 1.33, 1.15);
      try
        AChosenResult := TdxServerModeOptimalFetchHelper.CalculateOptimalFetchResult(AOptimalFetchParam);
        try
          ASkipIsFromBottom := AChosenResult.IsFromEnd;
          ASkipSkip := AChosenResult.Skip div MagicNumberFetchKeysModulo * MagicNumberFetchKeysModulo;
          ADraftScan := AChosenResult.Skip + AChosenResult.Take;
        finally
          AChosenResult.Free;
        end;
      finally
        AOptimalFetchParam.Free;
      end;
    finally
      if AModel <> SafeModel then
        AModel.Free;
    end;
    ARealScan := ADraftScan div MagicNumberFetchKeysModulo * MagicNumberFetchKeysModulo + 1;
    if ARealScan < ADraftScan then
      Inc(ARealScan, MagicNumberFetchKeysModulo);
    if ARealScan > AGroupInfo.DataRowCount then
      ARealScan := AGroupInfo.DataRowCount;
    if ARealScan = AGroupInfo.DataRowCount then
      ASkipTake := 0
    else
      ASkipTake := ARealScan - ASkipSkip;
  end;
end;

procedure TdxServerModeKeyedCache.CalculateTake(AGroupInfo: TdxServerModeGroupInfo;
  AFetchCount: Integer; AIsFromBottom: Boolean; out APureTake: Integer; out APureTakeIsFromBottom: Boolean);
var
  ATake, AFetchCountWithPenalty: Integer;
begin
  APureTakeIsFromBottom := False;
  APureTake := -1;
  if UseTakeEnforcer <> bFalse then
  begin
    AFetchCountWithPenalty := AFetchCount + MagicNumberFetchKeysTopPenaltyGap;
    ATake := MagicNumberTakeKeysBase;
    while AFetchCountWithPenalty > ATake do
      ATake := ATake * 2;
    if ATake > AGroupInfo.DataRowCount / 2 then
      ATake := AGroupInfo.DataRowCount;
    if ATake < AFetchCount then
      InvalidOperation(Format(sdxInternalErrorTakeLessFetchCount, [APureTake, AFetchCount, AGroupInfo.DataRowCount]));
    if ATake > AGroupInfo.DataRowCount then
      InvalidOperation(Format(sdxInternalErrorTakeGreaterChildDataRowCount, [APureTake, AGroupInfo.DataRowCount]));
    APureTake := ATake;
    if not (ATake = AGroupInfo.DataRowCount) and AIsFromBottom then
      APureTakeIsFromBottom := True;
  end;
end;

procedure TdxServerModeKeyedCache.DoFetchKeys(AGroupInfo: TdxServerModeGroupInfo;
  AIsFromBottom: Boolean; ASkip, ATake: Integer);
var
  AKeys: TdxServerModeKeyList;
  AGroupWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors;
  APatchedTake: Integer;
begin
  if IsLanded then
    Exit;
  AGroupWhere := GetGroupWhere(AGroupInfo);
  try
    ASortInfo := SortInfo.Clone(AIsFromBottom);
    try
      APatchedTake := ATake;
      if (ATake = 0) or (ASkip + ATake = AGroupInfo.DataRowCount) then
        APatchedTake := AGroupInfo.DataRowCount - ASkip + 1;
      AKeys := TdxServerModeKeyList.Create;
      try
        try
          FetchKeysTimed(AKeys, AGroupWhere, ASortInfo, ASkip, APatchedTake);
        except
          on E : Exception do
          begin
            Fatal(E);
            Exit;
          end;
        end;
        FillKeys(AGroupInfo, AIsFromBottom, ASkip, ATake, AKeys);
      finally
        AKeys.Free;
      end;
    finally
      ASortInfo.Free;
    end;
  finally
    AGroupWhere.Free;
  end;
end;

procedure TdxServerModeKeyedCache.DoFetchKeysSkipWithTakeBackup(AGroupInfo: TdxServerModeGroupInfo;
  ASkipIsFromBottom: Boolean; ASkipSkip, ASkipTake: Integer; APureTakeIsFromBottom: Boolean; APureTake: Integer);
var
  AKeys: TdxServerModeKeyList;
  AGroupWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors;
  APatchedTake: Integer;
begin
  if IsLanded then
    Exit;
  AGroupWhere := GetGroupWhere(AGroupInfo);
  try
    ASortInfo := SortInfo.Clone(ASkipIsFromBottom);
    try
      try
        APatchedTake := ASkipTake;
        if (ASkipTake = 0) or (ASkipSkip + ASkipTake = AGroupInfo.DataRowCount) then
          APatchedTake := AGroupInfo.DataRowCount - ASkipSkip + 1;
        AKeys := TdxServerModeKeyList.Create;
        try
          FetchKeysTimed(AKeys, AGroupWhere, ASortInfo, ASkipSkip, APatchedTake);
          UseTakeEnforcer := bFalse;
          FillKeys(AGroupInfo, ASkipIsFromBottom, ASkipSkip, ASkipTake, AKeys);
        finally
          AKeys.Free;
        end;
      except
        UseTakeEnforcer := bTrue;
        DoFetchKeys(AGroupInfo, APureTakeIsFromBottom, 0, APureTake);
        Exit;
      end;
    finally
      ASortInfo.Free;
    end;
  finally
    AGroupWhere.Free;
  end;
end;

function GetPerformanceTime: TLargeInteger; inline;
begin
  if not QueryPerformanceCounter(Result) then
    Result := GetTickCount;
end;

function GetPerformanceMilliSeconds(ATick: TLargeInteger): Double;
var
  AInterval: TLargeInteger;
begin
  if not QueryPerformanceFrequency(AInterval) then
    AInterval := MSecsPerSec;
  Result := (MSecsPerSec * (ATick)) / AInterval;
end;

procedure TdxServerModeKeyedCache.FetchKeysTimed(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer);
var
  AActualFetch, AActualScan: Integer;
  ATime: TLargeInteger;
begin
  ATime := GetPerformanceTime;
  FetchKeys(AKeys, AWhere, ASortInfo, ASkip, ATake);
  ATime := GetPerformanceTime - ATime;
  AActualFetch := AKeys.Count;
  AActualScan := AActualFetch + ASkip;
  Regressor.RegisterSample(AActualFetch, AActualScan, GetPerformanceMilliSeconds(ATime) / 1000);
end;

procedure TdxServerModeKeyedCache.FillKeys(AGroupInfo: TdxServerModeGroupInfo; AIsFromBottom: Boolean;
  ASkip, ATake: Integer; AKeys: TdxServerModeKeyList);
var
  AFirstRecord, AValidateCount, ACurrentRow, AIncrement: Integer;
  I: Integer;
  AFetchedKey, AOldKey: Variant;
begin
  if IsLanded then
    Exit;
  AFirstRecord := GetGlobalIndex(AGroupInfo, ASkip, AIsFromBottom);
  if ATake > 0 then
    AValidateCount := ATake
  else
    AValidateCount := AGroupInfo.DataRowCount - ASkip;
  if AKeys.Count <> AValidateCount then
  begin
    RaiseInconsistencyDetected(sdxInconsistencyUnexpectedNumberOfKeys, [AKeys.Count, AValidateCount]);
    Exit;
  end;
  ACurrentRow := AFirstRecord;
  if AIsFromBottom then
    AIncrement := -1
  else
    AIncrement := 1;
  for I := 0 to AKeys.Count - 1 do
  begin
    AFetchedKey := AKeys[I];
    if KeysByIndex.TryGetValue(ACurrentRow, AOldKey) then
    begin
      if not KeyEquals(AOldKey, AFetchedKey) then
      begin
        RaiseInconsistencyDetected(sdxInconsistencyWrongKeyByIndex,
          [dxKeyToText(AOldKey), ACurrentRow, dxKeyToText(AFetchedKey)]);
        Exit;
      end;
    end
    else
      KeysByIndex.Add(ACurrentRow, AFetchedKey);
    Inc(ACurrentRow, AIncrement);
  end;
  KeysByIndex.LoadedRecordsHelper.Add(AFirstRecord, AKeys.Count, AIsFromBottom);
end;

function TdxServerModeKeyedCache.GetGlobalIndex(AGroupInfo: TdxServerModeGroupInfo;
  APos: Integer; AIsReversed: Boolean): Integer;
begin
  if AIsReversed then
    Result := AGroupInfo.TopDataRowIndex + AGroupInfo.DataRowCount - APos - 1
  else
    Result := AGroupInfo.TopDataRowIndex + APos;
end;

procedure TdxServerModeKeyedCache.PopulateDownKeysToFetch(AKeysToFetch: TdxServerModeKeyList;
  AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; ARecordIndex: Integer; ASplitLen: Integer;
  out AFillFromSplit: Boolean);
var
  I, AOldIndex: Integer;
  AFillKey: TdxServerModeRow;
begin
  AFillFromSplit := True;
  for I := ARecordIndex to ARecordIndex + ASplitLen - 1 do
  begin
    if RowsByIndex.ContainsKey(I) then
    begin
      AFillFromSplit := False;
      Break;
    end;
    if not KeysByIndex.TryGetValue(I, AFillKey) then
    begin
      AFillFromSplit := False;
      Break;
    end;
    if AIndexesByKeys.TryGetValue(AFillKey, AOldIndex) then
    begin
      RaiseInconsistencyDetected(sdxInconsistencyKeyFoundTwice,
        [dxKeyToText(AFillKey), dxKeyToText(AOldIndex), I]);
      Exit;
    end;
    AKeysToFetch.Add(AFillKey);
    AIndexesByKeys.Add(AFillKey, I);
  end;
end;

procedure TdxServerModeKeyedCache.PopulateFetchKeys(AGroupInfo: TdxServerModeGroupInfo;
  APureTake, ASkipSkip, ASkipTake: Integer; ASkipIsFromBottom, APureTakeIsFromBottom: Boolean);
begin
  if IsLanded then
    Exit;
  if ASkipSkip >= 0 then
  begin
    if (ASkipSkip = 0) or (APureTake < 0) then
      DoFetchKeys(AGroupInfo, ASkipIsFromBottom, ASkipSkip, ASkipTake)
    else
      if APureTake >= 0 then
        DoFetchKeysSkipWithTakeBackup(AGroupInfo, ASkipIsFromBottom, ASkipSkip, ASkipTake, APureTakeIsFromBottom, APureTake);
  end
  else if APureTake >= 0 then
    DoFetchKeys(AGroupInfo, APureTakeIsFromBottom, 0, APureTake)
  else
    InvalidOperation(sdxInternalErrorSkipAndTakeWereNotChosen);
end;

procedure TdxServerModeKeyedCache.PopulateUpKeysToFetch(AKeysToFetch: TdxServerModeKeyList;
  AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; ARecordIndex: Integer; AInSize: Integer);
var
  I, AOldIndex: Integer;
  AFillKey: TdxServerModeRow;
begin
  I := ARecordIndex - 1;
  while (I >= 0) and (AKeysToFetch.Count < AInSize) do
  begin
    if RowsByIndex.ContainsKey(I) then
      Break;
    if not KeysByIndex.TryGetValue(I, AFillKey) then
      Break;
    if AIndexesByKeys.TryGetValue(AFillKey, AOldIndex) then
    begin
      RaiseInconsistencyDetected(sdxInconsistencyKeyFoundTwice,
        [dxKeyToText(AFillKey), dxKeyToText(AOldIndex), I]);
      Exit;
    end;
    AKeysToFetch.Add(AFillKey);
    AIndexesByKeys.Add(AFillKey, I);
    Dec(I);
  end;
end;

function TdxServerModeKeyedCache.PopulateRowsByIndex(const ARows: TdxServerModeRowList;
  AIndexesByKeys: TdxServerModeLoadedKeysChunkDictionary; AReturnRecordIndex: Integer): TdxServerModeRow;
var
  I, ARowIndex: Integer;
  AFetchedKey: Variant;
  AFetchedRow: TdxServerModeRow;
begin
  Result := Null;
  for I := ARows.Count - 1 downto 0 do
  begin
    AFetchedRow := ARows[I];
    AFetchedKey := GetKeyFromRow(AFetchedRow);
    if not AIndexesByKeys.TryGetValue(AFetchedKey, ARowIndex) then
    begin
      RaiseInconsistencyDetected(sdxInconsistencyWrongFetchedRow,
        [dxKeyToText(AFetchedKey)]);
      Exit;
    end;
    RowsByIndex.Add(ARowIndex, AFetchedRow);
    if ARowIndex = AReturnRecordIndex then
    begin
      if Result <> Null then
        InvalidOperation(sdxInternalErrorDoubleResult);
      Result := AFetchedRow;
    end;
  end;
end;

{ TdxServerModeCustomSQLAdapter }

constructor TdxServerModeCustomSQLAdapter.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create(ADataSource.Options);
  FDataSource := ADataSource;
end;

class procedure TdxServerModeCustomSQLAdapter.Register;
begin
  dxServerModeRegisterAdapter(Self, GetDisplayName);
end;

class procedure TdxServerModeCustomSQLAdapter.Unregister;
begin
  dxServerModeUnregisterAdapter(Self);
end;

class function TdxServerModeCustomSQLAdapter.GetDisplayName: string;
begin
  Result := '';
end;

function CompareParams(AItem1, AItem2: Pointer): Integer;
var
  AParam1, AParam2: TParam;
  AParamName1, AParamName2: string;
  ABuilder: TdxServerModeCustomSQLQueryBuilder;
  AStr: string;
begin
  AParam1 := TParam(AItem1);
  AParam2 := TParam(AItem2);

  ABuilder := TdxServerModeCustomSQLQueryBuilder(AParam1.Collection.Owner);
  AStr := ABuilder.CommandText;

  AParamName1 := Format(ABuilder.Adapter.GetParamFormat, [AParam1.Name]);
  AParamName2 := Format(ABuilder.Adapter.GetParamFormat, [AParam2.Name]);

  Result := dxCompareValues(PosEx(AParamName1, AStr, 1), PosEx(AParamName2, AStr, 1));
end;

procedure TdxServerModeCustomSQLAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
  CorrectCommandText(AParams, ACommandText);
end;

procedure TdxServerModeCustomSQLAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I, J: Integer;
  AParamName1, AParamName2: string;
begin
  for I := 0 to AParams.Count - 1 do
    for J := AParams.Count - 1 downto I + 1 do
      if (AParams[I].DataType = AParams[J].DataType) and VarEquals(AParams[I].Value, AParams[J].Value) then
      begin
        AParamName1 := Format(GetParamFormat, [AParams[I].Name]);
        AParamName2 := Format(GetParamFormat, [AParams[J].Name]);
        ACommandText := StringReplace(ACommandText, AParamName2, AParamName1, [rfReplaceAll, rfIgnoreCase]);
        AParams.Delete(J);
      end;
end;

procedure TdxServerModeCustomSQLAdapter.CorrectCommandText(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        ANewName := Format(GetParamServerFormat, [AParams[I].Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeCustomSQLAdapter.CreateParam(AParams: TdxServerModeParams; const AValue: Variant; const ADataType: TFieldType;
  out ADisplayValue: string): TdxServerModeParam;
begin
  Result := AParams.AddParameter;
  Result.ParamType := ptInput;
  Result.Name := Format(GetParamFakeNameFormat, [Result.Index + 1]);
  Result.DataType := ADataType;
  Result.Value := AValue;
  ADisplayValue := Format(GetParamFormat, [Result.Name]);
  DataSource.CheckParamValue(Result);
end;

procedure TdxServerModeCustomSQLAdapter.Changed(
  const AChanges: TdxServerModeDataSourceChanges);
begin
  DataSource.Change(AChanges);
end;

function TdxServerModeCustomSQLAdapter.GetParamCheck: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.GetParamFakeNameFormat: string;
begin
  Result := 'fake_param%d';
end;

function TdxServerModeCustomSQLAdapter.GetParamFormat: string;
begin
  Result := '(:%s)';
end;

function TdxServerModeCustomSQLAdapter.GetParamNameFormat: string;
begin
  Result := 'P%d';
end;

function TdxServerModeCustomSQLAdapter.GetParamServerFormat: string;
begin
  Result := '(@%s)';
end;

function TdxServerModeCustomSQLAdapter.IsMicrosecondSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeCustomSQLAdapter.IsMillisecondSupported: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.IsSkipClauseSupported: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.GetNullsSortOrder: TdxNullsSortOrder;
begin
  Result := nsoFirstIfAsc;
end;

procedure TdxServerModeCustomSQLAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := Format(GetParamFormat, [AParams[I].Name]);
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TdxServerModeCustomSQLAdapter.SortParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    for I := 0 to AParams.Count - 1 do
      AList.Add(AParams[I]);
    AList.Sort(CompareParams);
    for I := 0 to AList.Count - 1 do
      TParam(AList[I]).Index := I;
  finally
    AList.Free;
  end;
end;

function TdxServerModeCustomSQLAdapter.CanUseParam(AParam: TdxServerModeParam): Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.CanIdentifyInsertingRow: Boolean;
begin
  Result := False;
end;

function TdxServerModeCustomSQLAdapter.CanUseResultFieldName: Boolean;
begin
  Result := False;
end;

function TdxServerModeCustomSQLAdapter.CanUseGroupingByDateRangeParams: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.ConvertBoolToStr(const AValue: Variant): string;
const
  AResultMap: array[Boolean] of string = ('0', '1');
begin
  Result := AResultMap[AValue = True];
end;

procedure TdxServerModeCustomSQLAdapter.CheckFilterFieldCaption(var AFieldCaption: string;
  AOperatorKind: TcxFilterOperatorKind);
begin
// do nothing
end;

function TdxServerModeCustomSQLAdapter.DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('DATEADD(%s, -(%s), %s)', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeCustomSQLAdapter.DateDiff(APart: TdxSQLDatePart;
  const ADateExpression1, ADateExpression2: string): string;
begin
  Result := Format('DATEDIFF(%s, %s, %s)', [GetDatePart(APart), ADateExpression1, ADateExpression2]);
end;

function TdxServerModeCustomSQLAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('DATEADD(%s, %s, %s)', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeCustomSQLAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  Result := Format('DATEPART(%s, %s)', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModeCustomSQLAdapter.DateTimeToString(const ADateTime: TDateTime): string;
begin
  SysUtils.DateTimeToString(Result, GetDateTimeFormat, ADateTime);
  Result := Format('''%s''', [Result]);
  Result := Format(GetCastAsDateTimeFormat, [Result]);
end;

function TdxServerModeCustomSQLAdapter.GetCastAsDateFormat: string;
begin
  Result := '%s';
end;

function TdxServerModeCustomSQLAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := '%s';
end;

function TdxServerModeCustomSQLAdapter.GetCastAsTimeFormat: string;
begin
  Result := '%s';
end;

function TdxServerModeCustomSQLAdapter.GetDatePart(APart: TdxSQLDatePart): string;
const
  AResultMap: array[TdxSQLDatePart] of string = ('year', 'quarter', 'month', 'dayofyear',
    'day', 'weekday', 'week', 'hour', 'minute', 'second', 'millisecond', 'microsecond', 'nanosecond');
begin
  Result := AResultMap[APart];
end;

function TdxServerModeCustomSQLAdapter.GetDateTimeFormat: string;
begin
  Result := dxDefaultDateTimeFormat;
end;

function TdxServerModeCustomSQLAdapter.GetDateTimeGroupingResultFieldFormat: string;
begin
  Result := '%s as %s';
end;

function TdxServerModeCustomSQLAdapter.GetDateTimeGroupingResultFieldName(AField: TField): string;
begin
  Result := Format(GetFieldNameFormat, ['GroupingBy' + GetServerFieldName(AField)]);
end;

function TdxServerModeCustomSQLAdapter.IsDateRangeGroupingSupported: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomSQLAdapter.IsNull(const AExpression: string): string;
begin
  Result := Format('%s IS NULL', [AExpression]);
end;

function TdxServerModeCustomSQLAdapter.NeedCastGroupingByDateRange: Boolean;
begin
  Result := False;
end;

function TdxServerModeCustomSQLAdapter.TruncGroupingDate(const ADate: string;
  ADateTimeGrouping: TdxDateTimeGrouping): string;
begin
  Result := ADate;
  if not (ADateTimeGrouping in [dgDefault, dgByDateAndTime]) then
  begin
    if IsMicrosecondSupported then
      Result := DateDec(dpMicrosecond, Result, DatePart(dpMicrosecond, ADate));
    if IsMillisecondSupported then
      Result := DateDec(dpMillisecond, Result, DatePart(dpMillisecond, ADate));
    Result := DateDec(dpSecond, Result, DatePart(dpSecond, ADate));
    Result := DateDec(dpMinute, Result, DatePart(dpMinute, ADate));
    if ADateTimeGrouping <> dgByHour then
    begin
      Result := DateDec(dpHour, Result, DatePart(dpHour, ADate));
      if not (ADateTimeGrouping in [dgRelativeToToday, dgByDate]) then
      begin
        if ADateTimeGrouping = dgByWeekDay then
          Result := DateDec(dpDay, Result, IntToStr(DayOfTheWeek(Today) - 1))
        else
        begin
          Result := DateDec(dpDay, Result, DatePart(dpDay, ADate) + ' - 1');
          if ADateTimeGrouping <> dgByMonth then
            Result := DateDec(dpMonth, Result, DatePart(dpMonth, ADate) + ' - 1');
        end;
      end;
    end;
  end;
end;

function TdxServerModeCustomSQLAdapter.GetCaseOperatorFormat(AKind: TcxFilterOperatorKind): string;
begin
  case AKind of
    foEqual: Result := '%s = %s';
    foNotEqual: Result := '%s <> %s';
    foLess: Result := '%s < %s';
    foLessEqual: Result := '%s <= %s';
    foGreater: Result := '%s > %s';
    foGreaterEqual: Result := '%s >= %s';
  else
    Result := '%s';
  end;
end;

function TdxServerModeCustomSQLAdapter.GetCaseFullFormat: string;
begin
  Result := 'CASE WHEN (%s) THEN %s ELSE %s END';
end;

function TdxServerModeCustomSQLAdapter.GetEscapeTextForParamName: string;
begin
  Result := ' {ESCAPE ' + QuotedString('\') + '}';
end;

function TdxServerModeCustomSQLAdapter.GetPercentWildcard: Char;
begin
  Result := '%';
end;

function TdxServerModeCustomSQLAdapter.GetUnderscoreWildcard: Char;
begin
  Result := '_';
end;

procedure TdxServerModeCustomSQLAdapter.EscapeFakeParamName(AParam: TdxServerModeParam;
  ABuilder: TdxServerModeCustomSQLQueryBuilder);
var
  AParamName: string;
begin
  AParamName := Format(GetParamFormat, [AParam.Name]);
  ABuilder.ReplaceFakeParamNameWithText(AParam, AParamName + GetEscapeTextForParamName);
end;

procedure TdxServerModeCustomSQLAdapter.EscapeLikeValue(AParam: TdxServerModeParam; APercentWildcard, AUnderscoreWildcard: Char);
var
  AEscapePercentWildcard, AEscapeUnderscoreWildcard: Boolean;
begin
  AEscapePercentWildcard := (AParam.CriteriaOperatorKind in [foContains..foEndsWith]) or
    (APercentWildcard <> GetPercentWildcard);
  AEscapeUnderscoreWildcard := (AParam.CriteriaOperatorKind in [foContains..foEndsWith]) or
    (AUnderscoreWildcard <> GetUnderscoreWildcard);
  AParam.Value := MakeEscapedValue(AParam.Value, AEscapePercentWildcard, AEscapeUnderscoreWildcard);
  case AParam.CriteriaOperatorKind of
    foContains, foNotContains:
      AParam.Value := GetPercentWildcard + AParam.Value + GetPercentWildcard;
    foBeginsWith:
      AParam.Value := AParam.Value + GetPercentWildcard;
    foEndsWith:
      AParam.Value := GetPercentWildcard + AParam.Value;
    foLike, foNotLike:
      begin
        if AEscapePercentWildcard then
          AParam.Value := StringReplace(AParam.Value, APercentWildcard, GetPercentWildcard, [rfReplaceAll]);
        if AEscapeUnderscoreWildcard then
          AParam.Value := StringReplace(AParam.Value, AUnderscoreWildcard, GetUnderscoreWildcard, [rfReplaceAll]);
      end;
  end;
end;

procedure TdxServerModeCustomSQLAdapter.GetLikeValue(ABuilder: TdxServerModeCustomSQLQueryBuilder;
  AParam: TdxServerModeParam; APercentWildcard, AUnderscoreWildcard: Char);
begin
  if not AParam.EscapedValue then
    EscapeLikeValue(AParam, APercentWildcard, AUnderscoreWildcard);
  if NeedEscapeFakeParamName(AParam) then
    EscapeFakeParamName(AParam, ABuilder);
end;

function TdxServerModeCustomSQLAdapter.MakeEscapedValue(const AValue: string;
  AEscapePercentWildcard, AEscapeUnderscoreWildcard: Boolean): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  if AEscapePercentWildcard then
    Result := StringReplace(Result, GetPercentWildcard, '\' + GetPercentWildcard, [rfReplaceAll]);
  if AEscapeUnderscoreWildcard then
    Result := StringReplace(Result, GetUnderscoreWildcard, '\' + GetUnderscoreWildcard, [rfReplaceAll]);
end;

function TdxServerModeCustomSQLAdapter.NeedEscapeFakeParamName(AParam: TdxServerModeParam): Boolean;
begin
  Result := Pos('\', AParam.Value) > 0;
end;

function TdxServerModeCustomSQLAdapter.GetFieldsRetrieveQueryFormat: string;
begin
  Result := 'SELECT * FROM %s WHERE 1 = 2';
end;

function TdxServerModeCustomSQLAdapter.GetFieldName(AField: TField): string;
begin
  Result := Format(GetFieldNameFormat, [GetServerFieldName(AField)]);
end;

function TdxServerModeCustomSQLAdapter.GetFieldNameFormat: string;
var
  AQuoteChar: string;
begin
  AQuoteChar := GetQuoteChar;
  Result := Format('%s%%s%s', [AQuoteChar, AQuoteChar]);
end;

function TdxServerModeCustomSQLAdapter.GetFromQueryFormat: string;
begin
  Result := '(%s) DX0';
end;

function TdxServerModeCustomSQLAdapter.GetOrderFormat(AIsDesc: Boolean): string;
const
  OrderFormatMap: array[Boolean] of string = ('%s ASC', '%s DESC');
begin
  Result := OrderFormatMap[AIsDesc];
end;

function TdxServerModeCustomSQLAdapter.GetInsertingRowOutKey(const AKey: string): string;
begin
  Result := AKey;
end;

function TdxServerModeCustomSQLAdapter.GetServerFieldName(AField: TField): string;
begin
  if AField.Origin <> '' then
    Result := AField.Origin
  else
    Result := AField.FieldName;
end;

function TdxServerModeCustomSQLAdapter.GetSummaryFormat(AKind: TcxSummaryKind): string;
const
  SummaryKindMap: array[TcxSummaryKind] of string = ('count(%s)', 'sum(%s)',
    'min(%s)', 'max(%s)', 'count(%s)', 'avg(%s)');
begin
  Result := SummaryKindMap[AKind];
end;

function TdxServerModeCustomSQLAdapter.GetTableNameFormat: string;
var
  AQuoteChar: string;
begin
  AQuoteChar := GetQuoteChar;
  Result := Format('%s%%s%s', [AQuoteChar, AQuoteChar]);
end;

function TdxServerModeCustomSQLAdapter.GetDeleteSQLString(const AFrom, AWhere: string): string;
begin
  Result := Format('DELETE FROM %s %s', [AFrom, AWhere]);
end;

function TdxServerModeCustomSQLAdapter.GetSelectSQLString(
  const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
  ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
begin
  Result := '';
  dxAbstractError;
end;

function TdxServerModeCustomSQLAdapter.GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string;
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s)', [AFrom, AFields, AValues]);
end;

function TdxServerModeCustomSQLAdapter.GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string;
begin
  Result := Format('UPDATE %s SET %s %s', [AFrom, AValues, AWhere]);
end;

function TdxServerModeCustomSQLAdapter.GetQueryFormat: string;
begin
  Result := '%s FROM %s %s %s %s';
end;

function TdxServerModeCustomSQLAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  Result := '';
end;

function TdxServerModeCustomSQLAdapter.GetSQLQuerySchemaNames: string;
begin
  Result := '';
end;

function TdxServerModeCustomSQLAdapter.GetQuoteChar: string;
begin
  Result := '"';
end;

function TdxServerModeCustomSQLAdapter.QuotedString(const AParamValue: string): string;
begin
  Result := QuotedStr(AParamValue);
end;

{ TdxServerModeParams }

constructor TdxServerModeParams.Create(AOwner: TdxServerModeCustomSQLQueryBuilder; AParamClass: TdxServerModeParamClass);
begin
  inherited Create(AParamClass);
  FOwner := AOwner;
end;

procedure TdxServerModeParams.AddParam(Value: TdxServerModeParam);
begin
  inherited AddParam(Value);
end;

function TdxServerModeParams.AddParameter: TdxServerModeParam;
begin
{$IFDEF DELPHI15}
  Result := TdxServerModeParam(inherited AddParameter);
{$ELSE}
  Result := TdxServerModeParam(inherited Add);
{$ENDIF}
end;

function TdxServerModeParams.CreateParam(AFieldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TdxServerModeParam;
begin
  Result := TdxServerModeParam(inherited CreateParam(AFieldType, ParamName, ParamType));
end;

function TdxServerModeParams.FindParam(const Value: string): TdxServerModeParam;
begin
  Result := TdxServerModeParam(inherited FindParam(Value));
end;

function TdxServerModeParams.ParamByName(const Value: string): TdxServerModeParam;
begin
  Result := TdxServerModeParam(inherited ParamByName(Value));
end;

function TdxServerModeParams.GetParamClass: TParamClass;
begin
  Result := TdxServerModeParam;
end;

function TdxServerModeParams.GetItem(Index: Integer): TdxServerModeParam;
begin
  Result := TdxServerModeParam(inherited Items[Index]);
end;

procedure TdxServerModeParams.SetItem(Index: Integer; const Value: TdxServerModeParam);
begin
  inherited Items[Index] := Value;
end;

function TdxServerModeParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TdxServerModeCustomSQLQueryBuilder }

constructor TdxServerModeCustomSQLQueryBuilder.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
  FParams := TdxServerModeParams.Create(Self, TdxServerModeParam);
  FCriteria := DataSource.CreateCriteria;
  FCriteria.OnGetValue := GetFilterValueHandler;
  FCriteria.OnGetFieldCaption := GetFilterFieldCaptionHandler;
end;

destructor TdxServerModeCustomSQLQueryBuilder.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCriteria);
  inherited Destroy;
end;

procedure TdxServerModeCustomSQLQueryBuilder.Calculate;
begin
  Params.BeginUpdate;
  try
    Params.Clear;
    CalculateQueryType;
    FCommandText := GetSQLString;
    ValidateCommandTextWithParams;
    CheckParams;
  finally
    Params.EndUpdate;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.GetSQLString: string;
var
  AFields: string;
  AFrom: string;
  AGroup: string;
  ASortInfo: string;
  AWhere: string;
  AValues: string;
  AOutRowKey: string;
begin
  AFrom := GetFromSQLString;
  case FQueryType of
    qtInsert:
      begin
        AValues := GetValuesSQLString;
        AFields := GetFieldsSQLString;
        AOutRowKey := GetInsertingRowOutKeys;
        Result := Adapter.GetInsertSQLString(AFrom, AOutRowKey, AFields, AValues);
      end;
    qtUpdate:
      begin
        AValues := GetValuesSQLString;
        AWhere := GetWhereSQLString;
        AOutRowKey := GetInsertingRowOutKeys;
        Result := Adapter.GetUpdateSQLString(AFrom, AOutRowKey, AValues, AWhere);
      end;
    qtDelete:
      begin
        AWhere := GetWhereSQLString;
        Result := Adapter.GetDeleteSQLString(AFrom, AWhere);
      end;
  else
    AFields := GetFieldsSQLString;
    AWhere := GetWhereSQLString;
    AGroup := GetGroupSQLString;
    ASortInfo := GetSortInfoSQLString;
    Result := Adapter.GetSelectSQLString(AFields, AFrom, AWhere, AGroup, ASortInfo, TopRecords, SkipRecords, Distinct);
  end;
end;

procedure TdxServerModeCustomSQLQueryBuilder.CalculateQueryType;
begin
  if VarIsEmpty(Values) then
  begin
    if Fields = nil then
      FQueryType := qtDelete
    else
      FQueryType := qtSelect;
  end
  else
    if Criteria.IsEmpty then
      FQueryType := qtInsert
    else
      FQueryType := qtUpdate;
end;

function TdxServerModeCustomSQLQueryBuilder.CanUseParam(AParam: TdxServerModeParam): Boolean;
begin
  Result := CanUseParams and not VarIsNull(AParam.Value) and
    (not ((ppGroupingByDateRange in AParam.Position) and not (ppCriteria in AParam.Position)) or
      Adapter.CanUseGroupingByDateRangeParams) and Adapter.CanUseParam(AParam);
end;

function TdxServerModeCustomSQLQueryBuilder.CanDateTimeGrouping(ADescriptor: TdxServerModeDescriptor): Boolean;
begin
  Result := Adapter.IsDateRangeGroupingSupported and ADescriptor.IsDateTimeGrouping and
    (((GroupInfo <> nil) and GroupInfo.Contains(ADescriptor.Field)) or
    (DataSource.SortInfo.IndexOf(ADescriptor.Field) < DataSource.GroupCount));
end;

function TdxServerModeCustomSQLQueryBuilder.CanSorting(ADescriptor: TdxServerModeDescriptor): Boolean;
begin
  Result := SortInfoCalculating and ADescriptor.IsSorting;
end;

function TdxServerModeCustomSQLQueryBuilder.CanUseParams: Boolean;
begin
  Result := DataSource.Options.UseQueryParams;
end;

function TdxServerModeCustomSQLQueryBuilder.CreateParam(const ADataType: TFieldType; const AValue: Variant;
  out ADisplayValue: string): TdxServerModeParam;
begin
  Result := DoCreateParam(ADataType, AValue, ADisplayValue)
end;

function TdxServerModeCustomSQLQueryBuilder.DoCreateParam(
  const ADataType: TFieldType; const AValue: Variant; out ADisplayValue: string): TdxServerModeParam;
var
  APosition: TdxServerModeParamPositions;
begin
  Result := Adapter.CreateParam(FParams, AValue, ADataType, ADisplayValue);
  APosition := [];
  if CriteriaCalculating then
    Include(APosition, ppCriteria);
  if FieldsCalculating then
    Include(APosition, ppFields);
  if GroupingByDateRangeCalculating then
    Include(APosition, ppGroupingByDateRange);
  if SortInfoCalculating then
    Include(APosition, ppSortInfo);
  Result.Position := APosition;
end;

function TdxServerModeCustomSQLQueryBuilder.GetFieldsSQLString: string;
var
  I: Integer;
begin
  FFieldsCalculating := True;
  try
    if FQueryType = qtInsert then
    begin
      Result := '';
      for I := VarArrayLowBound(Values, 1) to VarArrayHighBound(Values, 1) do
        if not VarIsEmpty(Values[I]) then
        begin
          if Result <> '' then
            Result := Format('%s, %s', [Result, DescriptorToString(Fields[I])])
          else
            Result := DescriptorToString(Fields[I]);
        end;
    end
    else
    begin
      if Fields.Count = 0 then
        Result := '*'
      else
        Result := DescriptorsToString(Fields);
    end;
  finally
    FFieldsCalculating := False;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.GetFromSQLString: string;
begin
  Result := DataSource.GetFromSQLString;
end;

function TdxServerModeCustomSQLQueryBuilder.GetGroupSQLString: string;
begin
  if GroupInfo <> nil then
    Result := DescriptorsToString(GroupInfo)
  else
    Result := '';
  if Result <> '' then
    Result := Format('GROUP BY %s', [Result]);
end;

function TdxServerModeCustomSQLQueryBuilder.GetInsertingRowOutKeys: string;
var
  I: Integer;
begin
  if Adapter.CanIdentifyInsertingRow then
  begin
    Result := Adapter.GetInsertingRowOutKey(DescriptorToString(DataSource.KeyFields[0]));
    for I := 1 to DataSource.KeyFields.Count - 1 do
      Result := Format('%s, %s', [Result, Adapter.GetInsertingRowOutKey(DescriptorToString(DataSource.KeyFields[I]))]);
  end
  else
    Result := '';
end;

function TdxServerModeCustomSQLQueryBuilder.GetSortInfoSQLString: string;
begin
  FSortInfoCalculating := True;
  try
    if SortInfo <> nil then
      Result := DescriptorsToString(SortInfo)
    else
      Result := '';

    if Result <> '' then
      Result := Format('ORDER BY %s', [Result]);
  finally
    FSortInfoCalculating := False;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.GetWhereSQLString: string;
begin
  Criteria.SQLCalculating := True;
  try
    if (Criteria <> nil) and not Criteria.IsEmpty then
      Result := Criteria.FilterText
    else
      Result := '';
    if Result <> '' then
      Result := Format('WHERE %s', [Result]);
  finally
    Criteria.SQLCalculating := False;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.GetValuesSQLString: string;
var
  I: Integer;
  AValue: string;
begin
  Result := '';
  for I := VarArrayLowBound(Values, 1) to VarArrayHighBound(Values, 1) do
    if not VarIsEmpty(Values[I]) then
    begin
      CreateParam(Fields[I].Field.DataType, Values[I], AValue);
      if FQueryType = qtInsert then
      begin
        if Result = '' then
          Result := AValue
        else
          Result := Format('%s, %s', [Result, AValue]);
      end
      else
      begin
        if Result = '' then
          Result := Format('%s = %s', [DescriptorToString(Fields[I]), AValue])
        else
          Result := Format('%s, %s = %s', [Result, DescriptorToString(Fields[I]), AValue]);
      end;
    end;
end;

function TdxServerModeCustomSQLQueryBuilder.DescriptorToString(ADescriptor: TdxServerModeDescriptor): string;
var
  ADateTimeGrouping: Boolean;
  AFieldName: string;
  AResult: string;
begin
  if ADescriptor.LinkedObject <> nil then
    Result := DescriptorToString(ADescriptor.LinkedObject)
  else
    if ADescriptor.Field = nil then
      Result := '*'
    else
      Result := Adapter.GetFieldName(ADescriptor.Field);

  ADateTimeGrouping := CanDateTimeGrouping(ADescriptor);
  if ADateTimeGrouping then
  begin
    if FieldsCalculating or not Adapter.CanUseResultFieldName or
      (GroupInfo = nil) or not GroupInfo.Contains(ADescriptor.Field) then
    begin
      Result := GroupingByDateRange(Result, ADescriptor);
      if FieldsCalculating then
      begin
        Result := Format(Adapter.GetDateTimeGroupingResultFieldFormat,
          [Result, Adapter.GetDateTimeGroupingResultFieldName(ADescriptor.Field)]);
      end;
    end
    else
      if Adapter.CanUseResultFieldName then
        Result := Adapter.GetDateTimeGroupingResultFieldName(ADescriptor.Field);
  end;

  if ADescriptor.IsSummary then
  begin
    if (TdxServerModeSummaryDescriptor(ADescriptor).Kind = skCount) and not DataSource.IsIgnoreNullOnSummary then
      Result := Format(Adapter.GetSummaryFormat(TdxServerModeSummaryDescriptor(ADescriptor).Kind), ['*'])
    else
      Result := Format(Adapter.GetSummaryFormat(TdxServerModeSummaryDescriptor(ADescriptor).Kind), [Result]);
  end;

  if CanSorting(ADescriptor) then
  begin
    AResult := Result;
    Result := Format(Adapter.GetOrderFormat(TdxServerModeSortInfoDescriptor(ADescriptor).IsDesc), [Result]);
    if ADateTimeGrouping and (GroupInfo = nil) then
    begin
      AFieldName := Adapter.GetFieldName(ADescriptor.Field);
      if CompareText(AResult, AFieldName) <> 0 then
      begin
        if ADescriptor.DateTimeGrouping = dgByHour then
          AFieldName := Format(Adapter.GetCastAsTimeFormat, [AFieldName]);
        Result := Format('%s, %s', [Result, Format(Adapter.GetOrderFormat(TdxServerModeSortInfoDescriptor(ADescriptor).IsDesc), [AFieldName])]);
      end;
    end;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.DescriptorsToString(ADescriptors: TdxServerModeDescriptors): string;
var
  I: Integer;
begin
  Result := '';
  if ADescriptors.Count > 0 then
  begin
    Result := DescriptorToString(ADescriptors[0]);
    for I := 1 to ADescriptors.Count - 1 do
      Result := Format('%s, %s', [Result, DescriptorToString(ADescriptors[I])]);
  end;
end;

procedure TdxServerModeCustomSQLQueryBuilder.CheckParams;
begin
  if Params.Count > 0 then
    Adapter.CheckParams(Params, FCommandText);
end;

function TdxServerModeCustomSQLQueryBuilder.GetAdapter: TdxServerModeCustomSQLAdapter;
begin
  Result := DataSource.SQLAdapter;
end;

function TdxServerModeCustomSQLQueryBuilder.GetCriteriaCalculating: Boolean;
begin
  Result := Criteria.SQLCalculating;
end;

procedure TdxServerModeCustomSQLQueryBuilder.GetFilterFieldCaptionHandler(AItem: TdxServerModeCriteriaItem;
  var AFieldCaption: string);
begin
  AFieldCaption := DescriptorToString(AItem.Descriptor);
  Adapter.CheckFilterFieldCaption(AFieldCaption, AItem.OperatorKind);
end;

procedure TdxServerModeCustomSQLQueryBuilder.GetFilterValueHandler(AItem: TcxFilterCriteriaItem; const AValue: Variant;
  var ADisplayValue: string);
var
  ADateType: TFieldType;
  AParam: TdxServerModeParam;
begin
  ADateType := TdxServerModeCriteriaItem(AItem).Descriptor.Field.DataType;
  AParam := CreateParam(ADateType, AValue, ADisplayValue);
  AParam.CriteriaOperatorKind := AItem.OperatorKind;
  if AItem is TdxServerModeCriteriaItem then
    AParam.EscapedValue := TdxServerModeCriteriaItem(AItem).EscapedValue;
end;

function TdxServerModeCustomSQLQueryBuilder.GetParamCheck: Boolean;
begin
  Result := Adapter.ParamCheck;
end;

function TdxServerModeCustomSQLQueryBuilder.GetParamValueText(AParam: TdxServerModeParam): string;
var
  AVarType: TVarType;
begin
  AVarType := VarType(AParam.Value);
  if VarIsStr(AParam.Value) then
    Result := Adapter.QuotedString(VarToStr(AParam.Value))
  else
    if AVarType = varBoolean then
      Result := Adapter.ConvertBoolToStr(AParam.Value)
    else
      if AVarType = varNull then
        Result := 'NULL'
      else
        if VarIsDate(AParam.Value) then
          Result := Adapter.DateTimeToString(AParam.Value)
        else
          Result := VarToStr(AParam.Value);
end;

procedure TdxServerModeCustomSQLQueryBuilder.ReplaceFakeParamNameWithText(AParam: TdxServerModeParam;
  const AValue: string);
begin
  FCommandText := StringReplace(FCommandText, Format(Adapter.GetParamFormat, [AParam.Name]), AValue, [rfReplaceAll]);
end;

procedure TdxServerModeCustomSQLQueryBuilder.ValidateCommandTextWithParams;
var
  I: Integer;
  AParam: TdxServerModeParam;
  S, AParamName: string;
  V: Variant;
begin
  for I := Params.Count - 1 downto 0 do
  begin
    AParam := Params[I];
    S := '';
    if VarType(AParam.Value) <> varNull then
    begin
      if AParam.CriteriaOperatorKind in [foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith] then
      begin
        if VarIsStr(AParam.Value) then
        begin
          V := AParam.Value;
          AParam.DataType := VarTypeToDataType(VarType(AParam.Value));
          AParam.Value := V;
        end;
        Adapter.GetLikeValue(Self, AParam,
          DataSource.GetDataControllerPercentWildcard,
          DataSource.GetDataControllerUnderscoreWildcard);
      end
      else
        case AParam.DataType of
          ftDate:
            begin
              AParamName := Format(Adapter.GetParamFormat, [AParam.Name]);
              S := Format(Adapter.GetCastAsDateFormat, [AParamName]);
              ReplaceFakeParamNameWithText(AParam, S);
            end;
          ftTime:
            begin
              AParamName := Format(Adapter.GetParamFormat, [AParam.Name]);
              S := Format(Adapter.GetCastAsTimeFormat, [AParamName]);
              ReplaceFakeParamNameWithText(AParam, S);
            end;
        end;
    end;

    if not CanUseParam(AParam) then
    begin
      ReplaceFakeParamNameWithText(AParam, GetParamValueText(AParam));
      AParam.Free;
    end;
  end;
end;

function TdxServerModeCustomSQLQueryBuilder.CreateDateTimeParam(const ADateTime: TDateTime): string;
begin
  CreateParam(ftDateTime, ADateTime, Result);
end;

function TdxServerModeCustomSQLQueryBuilder.DateTimeGroupingByRelativeToToday(const AFieldName: string): string;
var
  ACheckDate: string;
  AToday, ATomorrow, AYesterday: string;
  AStartOfYear, AStartOfMonth, AStartOfWeek: string;
  ALastYear, ALastMonth, ALastWeek: string;
  ANextYear, ANext2Year, ANextMonth, ANext2Month, ANextWeek, ANext2Week: string;
  ANext, ALast: string;
  ADate: TDateTime;
begin
  ADate := Today;
  AToday := CreateDateTimeParam(ADate);
  AStartOfYear := CreateDateTimeParam(StartOfTheYear(ADate));
  AStartOfMonth := CreateDateTimeParam(StartOfTheMonth(ADate));
  AStartOfWeek := CreateDateTimeParam(StartOfTheWeek(ADate));

  ATomorrow := CreateDateTimeParam(ADate + 1);
  AYesterday := CreateDateTimeParam(ADate - 1);

  ALastYear := CreateDateTimeParam(StartOfAYear(YearOf(ADate) - 1));
  ALastMonth := CreateDateTimeParam(StartOfTheMonth(StartOfTheMonth(ADate) - 1));
  ALastWeek := CreateDateTimeParam(StartOfTheWeek(ADate - 7));

  ANextYear := CreateDateTimeParam(IncYear(StartOfTheYear(ADate), 1));
  ANext2Year := CreateDateTimeParam(IncYear(StartOfTheYear(ADate), 2));
  ANextMonth := CreateDateTimeParam(IncMonth(StartOfTheMonth(ADate), 1));
  ANext2Month := CreateDateTimeParam(IncMonth(StartOfTheMonth(ADate), 2));
  ANextWeek := CreateDateTimeParam(IncWeek(StartOfTheWeek(ADate), 1));
  ANext2Week := CreateDateTimeParam(IncWeek(StartOfTheWeek(ADate), 1));

  ACheckDate := Adapter.TruncGroupingDate(AFieldName, dgByDate);

  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANext2Year]), ANext2Year, ACheckDate]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANextYear]), ANextYear, ANext]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANext2Month]), ANext2Month, ANext]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANextMonth]), ANextMonth, ANext]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANext2Week]), ANext2Week, ANext]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ANextWeek]), ANextWeek, ANext]);
  ANext := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foEqual), [ACheckDate, ATomorrow]), ATomorrow, ANext]);

  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ALastYear]), ACheckDate, ALastYear]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, AStartOfYear]), ALast, AStartOfYear]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ALastMonth]), ALast, ALastMonth]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, AStartOfMonth]), ALast, AStartOfMonth]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, ALastWeek]), ALast, ALastWeek]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, AStartOfWeek]), ALast, AStartOfWeek]);
  ALast := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, AYesterday]), ALast, AYesterday]);

  Result := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foLess), [ACheckDate, AToday]), ALast, ANext]);
  Result := Format(Adapter.GetCaseFullFormat, [Format(Adapter.GetCaseOperatorFormat(foEqual), [ACheckDate, AToday]), ACheckDate, Result]);
end;

function TdxServerModeCustomSQLQueryBuilder.GetGroupingStartDate(ADateTimeGrouping: TdxDateTimeGrouping): string;
var
  ADate: TDateTime;
begin
  ADate := Now;
  case ADateTimeGrouping of
    dgRelativeToToday, dgByHour, dgByDate:
      ADate := dxDateOf(ADate);
    dgByWeekDay:
      ADate := StartOfTheWeek(ADate);
    dgByMonth:
      ADate := StartOfTheMonth(ADate);
    dgByYear:
      ADate := StartOfTheYear(ADate);
  end;
  Result := CreateDateTimeParam(ADate);
end;

function TdxServerModeCustomSQLQueryBuilder.GroupingByDateRange(const AFieldName: string;
  ADescriptor: TdxServerModeDescriptor): string;
var
  AStartDate: string;
  ADatePart: TdxSQLDatePart;
  ADateTimeGrouping: TdxDateTimeGrouping;
begin
  ADateTimeGrouping := ADescriptor.DateTimeGrouping;
  FGroupingByDateRangeCalculating := True;
  try
    case ADateTimeGrouping of
      dgRelativeToToday:
        Result := DateTimeGroupingByRelativeToToday(AFieldName);
      dgByHour:
        begin
          AStartDate := GetGroupingStartDate(dgByDate);
          Result := Adapter.DatePart(dpHour, AFieldName);
          Result := Adapter.DateInc(dpHour, AStartDate, Result);
        end;
      dgByDate, dgByMonth, dgByYear:
        begin
          AStartDate := GetGroupingStartDate(ADateTimeGrouping);
          case ADateTimeGrouping of
            dgByDate: ADatePart := dpDay;
            dgByMonth: ADatePart := dpMonth;
          else
            ADatePart := dpYear;
          end;
          Result := Adapter.DateDiff(ADatePart, AStartDate, AFieldName);
          Result := Adapter.DateInc(ADatePart, AStartDate, Result);
        end;
    else
      Result := AFieldName;
    end;
    if Adapter.NeedCastGroupingByDateRange then
      Result := Format(Adapter.GetCastAsDateTimeFormat, [Result])
    else
      case ADescriptor.Field.DataType of
        ftDate:
          Result := Format(Adapter.GetCastAsDateFormat, [Result]);
        ftTime:
          Result := Format(Adapter.GetCastAsTimeFormat, [Result]);
      end;
    if ADateTimeGrouping in [dgRelativeToToday, dgByHour, dgByDate, dgByMonth, dgByYear] then
      Result := Format(Adapter.GetCaseFullFormat,
        [Adapter.IsNull(Adapter.GetFieldName(ADescriptor.Field)), 'NULL', Result]);
  finally
    FGroupingByDateRangeCalculating := False;
  end;
end;

procedure TdxServerModeCustomSQLQueryBuilder.SetCriteria(Value: TdxServerModeCriteria);
begin
  Criteria.Clear;
  if (Value <> nil) and not Value.IsEmpty then
    Criteria.Root.Assign(Value.Root)
end;

{ TdxServerModeDataSourceOptions }

constructor TdxServerModeDataSourceOptions.Create(AOwner: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FOwner := AOwner;
  FUseQueryParams := True;
end;

procedure TdxServerModeDataSourceOptions.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeDataSourceOptions then
  begin
    UseQueryParams := TdxServerModeDataSourceOptions(Source).UseQueryParams;
  end
  else
    inherited Assign(Source);
end;

function TdxServerModeDataSourceOptions.GetSchemaName: string;
begin
  Result := '';
end;

function TdxServerModeDataSourceOptions.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TdxServerModeDataSourceOptions.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SQLAdapter', ReadSQLAdapter, nil, False);
end;

procedure TdxServerModeDataSourceOptions.Changed(const AChanges: TdxServerModeDataSourceChanges = [ctOptions]);
begin
  Owner.Change(AChanges);
end;

procedure TdxServerModeDataSourceOptions.CheckInactive;
begin
  Owner.CheckInactive;
end;

procedure TdxServerModeDataSourceOptions.ReadSQLAdapter(Reader: TReader);
var
  ASQLAdapterClass: TdxServerModeCustomSQLAdapterClass;
begin
  ASQLAdapterClass := TdxServerModeCustomSQLAdapterClass(dxGetServerModeSQLAdapters.FindByDescription(Reader.ReadString));
  if (ASQLAdapterClass <> nil) and (Owner.SQLAdapterClass = nil) then
    Owner.SQLAdapterClass := ASQLAdapterClass;
end;

{ TdxServerModeDataSourceCustomHelper }

constructor TdxServerModeDataSourceCustomHelper.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
end;

function TdxServerModeDataSourceCustomHelper.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := TdxServerModeDataSourceOptions;
end;

function TdxServerModeDataSourceCustomHelper.GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
begin
  Result := TdxServerModeCustomSQLQueryBuilder;
end;

procedure TdxServerModeDataSourceCustomHelper.CheckParamValue(var AParam: TdxServerModeParam);
begin
// do nothing
end;

procedure TdxServerModeDataSourceCustomHelper.CheckSQLAdapter;
begin
  DataSource.CheckSQLAdapter;
end;

procedure TdxServerModeDataSourceCustomHelper.DoExecuteCommand(AResultSet: PDataSet; const ACommand: string;
  AParams: TParams; AParamCheck: Boolean);
var
  AIntf: IdxProviderSupport;
  ASendParams: TParams;
{$IFDEF DELPHI17}
  ADataSet: TDataSet;
{$ENDIF}
begin
  Supports(DataSet, IdxProviderSupport, AIntf);
  if (AParams = nil) or (AParams.Count = 0) then
    ASendParams := nil
  else
    ASendParams := AParams;
{$IFDEF DELPHI17}
  if AResultSet = nil then
    AIntf.PSExecuteStatement(ACommand, ASendParams)
  else
  begin
    AIntf.PSExecuteStatement(ACommand, ASendParams, ADataSet);
    AResultSet^ := ADataSet;
  end;
{$ELSE}
  AIntf.PSExecuteStatement(ACommand, ASendParams, AResultSet);
{$ENDIF}
end;

function TdxServerModeDataSourceCustomHelper.ExecuteCommand(AResultSet: PDataSet;
  const ACommandText: string; AParams: TParams; AParamCheck: Boolean; ACheckSettings: Boolean = True): Boolean;
begin
  Result := DataSource.ExecuteCommand(AResultSet, ACommandText, AParams, AParamCheck, ACheckSettings);
end;

function TdxServerModeDataSourceCustomHelper.GetSQLQuerySchemaNames: string;
begin
  Result := DataSource.Adapter.GetSQLQuerySchemaNames;
end;

procedure TdxServerModeDataSourceCustomHelper.InitializeDataSet;
begin
  DataSource.InitializeDataSet;
end;

function TdxServerModeDataSourceCustomHelper.IsConnected: Boolean;
begin
  Result := DataSource.IsConnected;
end;

procedure TdxServerModeDataSourceCustomHelper.PrepareDatasetForIterate(ADataSet: TDataSet);
begin
  ADataSet.DisableControls;
end;

function TdxServerModeDataSourceCustomHelper.GetFieldsRetrieveQuery: string;
begin
  Result := DataSource.GetFieldsRetrieveQuery;
end;

function TdxServerModeDataSourceCustomHelper.GetConnection: TCustomConnection;
begin
  Result := DataSource.Connection;
end;

function TdxServerModeDataSourceCustomHelper.GetDataSet: TDataSet;
begin
  Result := DataSource.DataSet;
end;

{ TdxServerModeCustomDataSource }

constructor TdxServerModeCustomDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoadingFieldNames := TStringList.Create;
  FHelper := GetHelperClass.Create(Self);
  FOptions := GetOptionsClass.Create(Self);
  FDataSet := GetDataSetClass.Create(Self);
  FDataSetDesigner := TdxServerModeDataSetDesigner.Create(Self);
  FFieldDescriptors := TdxServerModeFieldDescriptors.Create(Self);
  FKeyFields := TdxServerModeFieldDescriptors.Create(Self);
  FKeyFieldNames := CreateKeyFieldNames;
  FFilter := CreateCriteria;
  FFilter.OnChanged := FilterChangedHandler;
  FFindFilter := CreateFindFilterCriteria;
  FFindFilter.Filter.OnChanged := FilterChangedHandler;
  FGeneralFilter := CreateCriteria;
  FGeneralFilter.OnChanged := FilterChangedHandler;
  FSortInfo := CreateSortInfo;
  FSortInfo.OnChanged := SortInfoChangedHandler;
  FSummaries := GetSummariesClass.Create(Self);
  FSummaries.OnChanged := SummariesChangedHandler;
  FTotalSummaries := GetSummariesClass.Create(Self);
  FTotalSummaries.OnChanged := TotalSummariesChangedHandler;
  FCache := GetCacheClass.Create(Self);
  FErrorTimer := TcxTimer.Create(Self);
  FErrorTimer.Enabled := False;
  FErrorTimer.OnTimer := ErrorTimerHandler;
  FErrorTimer.Interval := 0;
end;

destructor TdxServerModeCustomDataSource.Destroy;
begin
  Connection := nil;

  FreeAndNil(FErrorTimer);
  FreeAndNil(FCache);
  FreeAndNil(FTotalSummaries);
  FreeAndNil(FSummaries);
  FreeAndNil(FSortInfo);
  FreeAndNil(FGeneralFilter);
  FreeAndNil(FFindFilter);
  FreeAndNil(FFilter);

  FreeAndNil(FKeyFieldNames);
  FreeAndNil(FKeyFields);
  FreeAndNil(FFieldDescriptors);

  FreeAndNil(FDataSetDesigner);
  FreeAndNil(FDataSet);
  FreeAndNil(FOptions);
  FreeAndNil(FHelper);
  FreeAndNil(FSQLAdapter);
  FreeAndNil(FLoadingFieldNames);
  inherited Destroy;
end;

procedure TdxServerModeCustomDataSource.Apply(AFilter: TdxServerModeCriteria;
  AFindFilter: TdxServerModeFindFilterCriteria; ASortInfo: TdxServerModeSortInfoDescriptors;
  AGroupCount: Integer; ASummaries, ATotalSummaries: TdxServerModeSummaryDescriptors);

  procedure UpdateFilter;
  begin
    if not Filter.IsEqual(AFilter) then
      Filter.Root.Assign(AFilter.Root);
  end;

  procedure UpdateFindFilter;
  begin
    if not FindFilter.IsEqual(AFindFilter) then
      FindFilter.AssignConditions(AFindFilter);
  end;

  procedure UpdateGeneralFilter;
  begin
    GeneralFilter.Clear;
    if not Filter.IsEmpty then
      GeneralFilter.AndCriteria(Filter);
    if not FindFilter.Filter.IsEmpty then
      GeneralFilter.AndCriteria(FindFilter.Filter);
  end;

  procedure UpdateSortInfo;
  var
    I: Integer;
    ACloneSortInfo: TdxServerModeSortInfoDescriptors;
  begin
    ACloneSortInfo := ASortInfo.Clone(False);
    try
      for I := 0 to KeyFields.Count - 1 do
        if not ACloneSortInfo.Contains(KeyFields[I].Field) then
          ACloneSortInfo.AddKeySortInfo(KeyFields[I].Field, False);
      for I := ACloneSortInfo.Count - 1 downto 0 do
        if ACloneSortInfo[I].Field = nil then
          ACloneSortInfo.RemoveItem(ACloneSortInfo[I]);
      if not SortInfo.IsEqual(ACloneSortInfo) then
        SortInfo.Assign(ACloneSortInfo);
    finally
      ACloneSortInfo.Free;
    end;
  end;

  procedure UpdateSummaries;
  begin
    if not Summaries.IsEqual(ASummaries) then
      Summaries.Assign(ASummaries);
    if not TotalSummaries.IsEqual(ATotalSummaries) then
      TotalSummaries.Assign(ATotalSummaries);
  end;

begin
  FPrevStateInfo := TdxServerModeStateInfo.Create(GroupCount, SortInfo);
  try
    BeginUpdate;
    try
      GroupCount := AGroupCount;
      UpdateFilter;
      UpdateFindFilter;
      UpdateGeneralFilter;
      UpdateSortInfo;
      UpdateSummaries;
    finally
      EndUpdate;
    end;
  finally
    FreeAndNil(FPrevStateInfo);
  end;
end;

function TdxServerModeCustomDataSource.CreateEmptyRow: Variant;
begin
  Result := VarArrayCreate([0, Fields.Count - 1], varVariant);
end;

procedure TdxServerModeCustomDataSource.Delete(ACriteria: TdxServerModeCriteria);
begin
//do nothing
end;

function TdxServerModeCustomDataSource.FindIncremental(AField: TField; const ASubStr: string; AStartIndex: Integer;
  ASearchDown: Boolean = True; AIgnoreStartRow: Boolean = False): Integer;

  function CalculateCriteria(const ARow: TdxServerModeRow): TdxServerModeCriteria;
  var
    AList: TdxServerModeCriteriaItemList;
    I: Integer;
    AIsDown: Boolean;
    AValue: Variant;
    AItem: TdxServerModeCriteriaItem;
    ALikeValue: string;
  begin
    Result := CreateCriteria;

    if not GeneralFilter.IsEmpty then
    begin
      AList := Result.Root.AddItemList(fboAnd) as TdxServerModeCriteriaItemList;
      AList.AssignItems(GeneralFilter.Root);

      AList := Result.Root.AddItemList(fboAnd) as TdxServerModeCriteriaItemList;
    end
    else
      AList := Result.Root;

    ALikeValue := Adapter.MakeEscapedValue(ASubStr, True, True);
    AItem := Result.AddItem(AList, FieldDescriptors.ItemByField(AField), foLike, ALikeValue + Adapter.GetPercentWildcard);
    AItem.EscapedValue := True;
    for I := 0 to SortInfo.Count - 1 do
    begin
      AValue := Cache.EvaluateOnInstance(ARow, SortInfo[I].Field);
      AIsDown := ASearchDown xor SortInfo[I].IsDesc;

      AList := AList.AddItemList(fboOr) as TdxServerModeCriteriaItemList;
      Cache.MakeStrongClause(Result, AList, SortInfo[I], AIsDown, (I = SortInfo.Count - 1) and not AIgnoreStartRow, AValue);

      if I < SortInfo.Count - 1 then
      begin
        AList := AList.AddItemList(fboAnd) as TdxServerModeCriteriaItemList;
        Result.AddItem(AList, SortInfo[I], foEqual, AValue);
      end;
    end;
  end;

var
  ACriteria: TdxServerModeCriteria;
  ARow, AKey: TdxServerModeRow;
begin
  Result := -1;
  if Length(ASubStr) = 0 then
    Exit;
  ARow := Rows[AStartIndex];
  if not IsConsistentCache then
    Exit;
  ACriteria := CalculateCriteria(ARow);
  try
    AKey := FindFirstKey(ACriteria, ASearchDown);
    if not VarIsNull(AKey) then
    begin
      Result := Cache.GetRowIndexByKey(AKey);

      if (AStartIndex >= 0) and (Result >= 0) and
          ((ASearchDown and (AStartIndex > Result)) or (not ASearchDown and (AStartIndex < Result))) then
        Result := -1;
    end;
  finally
    ACriteria.Free;
  end;
end;

function TdxServerModeCustomDataSource.FindIncremental(const AFieldName, ASubStr: string; AStartIndex: Integer;
  ASearchDown: Boolean = True; AIgnoreStartRow: Boolean = False): Integer;
var
  AField: TField;
begin
  AField := FieldDescriptors.FieldByName(AFieldName);
  if AField <> nil then
    Result := FindIncremental(AField, ASubStr, AStartIndex, ASearchDown, AIgnoreStartRow)
  else
    Result := -1;
end;

procedure TdxServerModeCustomDataSource.AddDataController(
  ADataController: TdxServerModeDataController);
var
  AOldDataController: TdxServerModeDataController;
begin
  if FDataController <> ADataController then
  begin
    AOldDataController := FDataController;
    FDataController := ADataController;
    if Assigned(AOldDataController) then
      AOldDataController.DataSource := nil;
  end;
end;

procedure TdxServerModeCustomDataSource.RemoveDataController(
  ADataController: TdxServerModeDataController);
begin
  if FDataController = ADataController then
  begin
    Close;
    FDataController := nil;
  end;
end;

procedure TdxServerModeCustomDataSource.Open;
begin
  Active := True;
end;

procedure TdxServerModeCustomDataSource.Close;
begin
  Active := False;
end;

procedure TdxServerModeCustomDataSource.Refresh;
begin
  Change([ctConnection]);
end;

procedure TdxServerModeCustomDataSource.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxServerModeCustomDataSource.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxServerModeCustomDataSource.EndUpdate;
begin
  Dec(FLockCount);
  Update;
end;

procedure TdxServerModeCustomDataSource.PopulateFilterValues(AList: TcxDataFilterValueList; AField: TField;
  ACriteria: TcxFilterCriteria; var AUseFilteredRecords: Boolean; out ANullExists: Boolean);
var
  AFilterCriteria: TdxServerModeCriteria;
  ADataSet: TDataSet;
  AFields: TdxServerModeFieldDescriptors;
  ASortInfo: TdxServerModeSortInfoDescriptors;
  AValue: Variant;
begin
  ANullExists := False;
  if not IsConsistentCache then
    Exit;
  if AUseFilteredRecords then
    AFilterCriteria := GeneralFilter
  else
    AFilterCriteria := nil;

  AFields := TdxServerModeFieldDescriptors.Create(Self);
  try
    AFields.Add(AField);
    ASortInfo := TdxServerModeSortInfoDescriptors.Create(Self);
    try
      ASortInfo.Add(AField, False);
      if ExecuteCommand(@ADataSet, AFields, AFilterCriteria, ASortInfo, nil, ACriteria.MaxValueListCount, 0, True) then
      try
        while not ADataSet.Eof do
        begin
          AValue := dxServerModeGetFieldValue(ADataSet.Fields[0]);
          if ACriteria.ValueIsNull(AValue) then
            ANullExists := True
          else
            AList.Add(fviValue, AValue, ADataSet.Fields[0].DisplayText, True);
          ADataSet.Next;
        end;
      finally
        ADataSet.Free;
      end;
    finally
      ASortInfo.Free;
    end;
  finally
    AFields.Free;
  end;
end;

function TdxServerModeCustomDataSource.PostEditingData(ARowIndex: Integer; const AEditingRow: Variant): Variant;
begin
  Result := Unassigned;
end;

procedure TdxServerModeCustomDataSource.PopulateFields(AList: TStrings);
begin
  AList.Clear;
  DoPopulateFields(AList);
end;

procedure TdxServerModeCustomDataSource.PopulateKeyFields(AList: TStrings);
begin
  AList.Clear;
  if FKeyFieldNames.Count > 0 then
    AList.Assign(FKeyFieldNames)
  else
    DoPopulateKeyFields(AList);
end;

procedure TdxServerModeCustomDataSource.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Fields', ReadFields, WriteFields, FDataSet.Fields.Count > 0);
end;

procedure TdxServerModeCustomDataSource.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Fields.Count - 1 do
    if Fields[I].Owner = Root then
      Proc(Fields[I]);
end;

procedure TdxServerModeCustomDataSource.Loaded;
begin
  inherited Loaded;
  AssignFields;
  try
    if FStreamedActive then
      Active := True;
  except
    if csDesigning in ComponentState then
      InternalHandleException
    else
      raise;
  end;
end;

procedure TdxServerModeCustomDataSource.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Connection) then
    Connection := nil;
  inherited Notification(AComponent, Operation);
end;

function GetField(ACaller: TComponent; Index: Integer): TComponent;
begin
  Result := TdxServerModeCustomDataSource(ACaller).Fields[Index];
end;

procedure TdxServerModeCustomDataSource.SetName(const ANewName: TComponentName);
var
  AOldName: string;
begin
  AOldName := Name;
  inherited SetName(ANewName);
  RenameComponents(Self, Owner, ANewName, AOldName, Fields.Count, @GetField);
end;

procedure TdxServerModeCustomDataSource.AddKeyField(AField: TField);
begin
  FKeyFields.Add(AField);
  FSortInfo.AddKeySortInfo(AField, False);
end;

function TdxServerModeCustomDataSource.CanModify: Boolean;
begin
  Result := False;
end;

function TdxServerModeCustomDataSource.CanUpdate: Boolean;
begin
  Result := not IsLocked and not IsDestroying and not IsLoading;
end;

procedure TdxServerModeCustomDataSource.CheckCacheInconsistency(ARecoverCache: Boolean);
begin
  if ARecoverCache and (CacheState = smcsInconsistent) then
  begin
    Inc(FCacheInconsistencyCount);
    if CacheInconsistencyCount >= 3 then
    begin
      CacheInconsistencyCount := 0;
      DoFatalErrorTimerHandler;
    end;
  end
  else
    CacheInconsistencyCount := 0;
end;

procedure TdxServerModeCustomDataSource.CheckInactive;
begin
  if Active then
    if ([csUpdating, csDesigning, csDestroying] * ComponentState) <> [] then
      Close
    else
      DatabaseError(sdxDataSourceOpen, Self);
end;

procedure TdxServerModeCustomDataSource.CheckParamValue(var AParam: TdxServerModeParam);
begin
  Helper.CheckParamValue(AParam);
end;

procedure TdxServerModeCustomDataSource.CheckSQLAdapter;
begin
  if SQLAdapterClass = nil then
    DatabaseError(sdxMissingSQLAdapter, Self);
end;

procedure TdxServerModeCustomDataSource.CheckSettings;
begin
  CheckSQLAdapter;
end;

function TdxServerModeCustomDataSource.CreateCriteria: TdxServerModeCriteria;
begin
  Result := GetCriteriaClass.Create(Self);
end;

function TdxServerModeCustomDataSource.CreateField(AFieldDef: TFieldDef; AOwner: TComponent): TField;
begin
  Result := AFieldDef.CreateField(AOwner, nil, AFieldDef.Name, False);
  if AOwner = Owner then
    Result.Name := GetFieldUniqueName(Result);
end;

function TdxServerModeCustomDataSource.CreateFindFilterCriteria: TdxServerModeFindFilterCriteria;
begin
  Result := TdxServerModeFindFilterCriteria.Create(Self);
end;

function TdxServerModeCustomDataSource.CreateGroupInfo: TdxServerModeGroupInfoDescriptors;
begin
  Result := GetGroupInfoDescriptorsClass.Create(Self)
end;

function TdxServerModeCustomDataSource.CreateKeyFieldNames: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Duplicates := dupIgnore;
  Result.Delimiter := ';';
end;

function TdxServerModeCustomDataSource.CreateSortInfo: TdxServerModeSortInfoDescriptors;
begin
  Result := GetSortInfoDescriptorsClass.Create(Self);
end;

function TdxServerModeCustomDataSource.CreateSummaries: TdxServerModeSummaryDescriptors;
begin
  Result := GetSummariesClass.Create(Self);
end;

procedure TdxServerModeCustomDataSource.DoInitializeDataSet(ADataSet: TDataSet);
begin
  Helper.DoInitializeDataSet(ADataSet);
end;

procedure TdxServerModeCustomDataSource.DoSynchronizeDataSetConnection(ADataSet: TDataSet);
begin
  Helper.DoSynchronizeDataSetConnection(ADataSet);
end;

procedure TdxServerModeCustomDataSource.DoUpdate;
begin
  if ctConnection in FChanges then
    Cache.SoftReset
  else
  begin
    if [ctCache, ctFiltering] * FChanges <> [] then
    begin
      Cache.Clear;
      Cache.ClearTopGroupInfo;
    end
    else
    begin
      if [ctSorting, ctSortingSummaries] * FChanges <> [] then
        Cache.Clear;
      if ctSortingSummaries in FChanges then
        Cache.ClearGroupInfo
      else
        if ([ctSorting, ctGroups] * FChanges <> []) then
          if PrevStateInfo <> nil then
            Cache.CheckTopGroupInfo(PrevStateInfo)
          else
            Cache.ClearGroupInfo;
      if [ctTotalSummaries, ctSummaries] * FChanges <> [] then
        Cache.RecalculateSummaries;
    end;
  end;
  FChanges := [];
end;

function TdxServerModeCustomDataSource.GetFieldUniqueName(AField: TField): string;
begin
  Result := CreateUniqueName(Owner, Self, AField, '', AField.FieldName);
end;

function TdxServerModeCustomDataSource.GetFromSQLString: string;
begin
  Result := '';
  dxAbstractError;
end;

procedure TdxServerModeCustomDataSource.InternalClose;
begin
  BeginUpdate;
  try
    Clear;
  finally
    CancelUpdate;
    DoSetActive(False);
  end;
end;

procedure TdxServerModeCustomDataSource.InternalHandleException;
begin
  ApplicationHandleException(Self)
end;

procedure TdxServerModeCustomDataSource.InternalOpen;
begin
  InitializeConnection;
  InitializeDataSet;
  if KeyFields.Count = 0 then
    DatabaseError(sdxPrimaryKeysNotFound, Self);
  DoSetActive(True);
end;

procedure TdxServerModeCustomDataSource.OpenComplete;
begin
  if Active then
    DoAfterOpen
  else
    InternalClose;
end;

procedure TdxServerModeCustomDataSource.DestroyNonPersistentFields;
var
  I: Integer;
begin
  for I := Fields.Count - 1 downto 0 do
    if not IsPersistentField(Fields[I]) then
      Fields[I].Free;
end;

function TdxServerModeCustomDataSource.IsIgnoreNullOnSummary: Boolean;
begin
  Result := (DataController <> nil) and (soNullIgnore in DataController.Summary.Options);
end;

function TdxServerModeCustomDataSource.IsPersistentField(AField: TField): Boolean;
begin
  Result := (AField.Owner = Owner) or (FLoadingFieldNames.IndexOf(AField.FieldName) <> -1);
end;

procedure TdxServerModeCustomDataSource.InitializeConnection;
begin
  if not Assigned(Connection) then
    DatabaseError(sdxMissingConnection, Self);
  FConnection.Connected := True;
end;

procedure TdxServerModeCustomDataSource.UpdateFieldDefs;
begin
  FieldDefs.Clear;
  FieldDefs.Update;
end;

procedure TdxServerModeCustomDataSource.UpdateFields;
var
  I: Integer;
begin
  DestroyNonPersistentFields;
  UpdateFieldDefs;
  if Fields.Count = 0 then
  begin
    for I := 0 to FieldDefs.Count - 1 do
      if Fields.FindField(FieldDefs[I].Name) = nil then
        CreateField(FieldDefs[I], nil);
  end;
  for I := 0 to Fields.Count - 1 do
    FieldDescriptors.Add(Fields[I]);
  InitializeKeyFields;
end;

procedure TdxServerModeCustomDataSource.InitializeDataSet;
begin
  BeginUpdateFields;
  try
    BeginUpdate;
    try
      Clear;
      CheckSettings;
      DoInitializeDataSet(DataSet);
      UpdateFields;
    finally
      CancelUpdate;
    end;
  finally
    EndUpdateFields;
  end;
end;

function TdxServerModeCustomDataSource.GetBookmark(ARecordIndex: Integer): Variant;
begin
  Result := Cache.GetRowKey(ARecordIndex);
end;

function TdxServerModeCustomDataSource.BookmarkValid(ABookmark: Variant): Boolean;
begin
  Result := Cache.GetRowIndexByKey(ABookmark) >= 0;
end;

procedure TdxServerModeCustomDataSource.DoAfterClose;
begin
  if Assigned(FAfterClose) then
    FAfterClose(Self);
end;

procedure TdxServerModeCustomDataSource.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TdxServerModeCustomDataSource.DoBeforeClose;
begin
  if Assigned(FBeforeClose) then
    FBeforeClose(Self);
end;

procedure TdxServerModeCustomDataSource.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TdxServerModeCustomDataSource.DoFatalError;
begin
  if Assigned(FOnFatalError) then
    FOnFatalError(Self, LastErrorMessage);
end;

procedure TdxServerModeCustomDataSource.DoInconsistentCache(out ARecoverCache: Boolean);
begin
  ARecoverCache := True;
  if Assigned(FOnInconsistentCache) then
    FOnInconsistentCache(Self, LastErrorMessage, ARecoverCache);
end;

procedure TdxServerModeCustomDataSource.DoFatalErrorTimerHandler;
begin
  Close;
  DoFatalError;
end;

procedure TdxServerModeCustomDataSource.DoInconsistentCacheTimerHandler;
var
  ARecoverCache: Boolean;
begin
  Cache.ResetTopGroupInfo;
  if Assigned(DataController) then
  begin
    DataController.BeginUpdate;
    try
      DataController.ResetDataSourceCache;
    finally
      DataController.EndUpdate;
    end;
    DoInconsistentCache(ARecoverCache);
    if ARecoverCache then
      RecoveryFromInconsistentState;
  end
  else
  begin
    DoInconsistentCache(ARecoverCache);
    if ARecoverCache then
      Refresh;
  end;
  CheckCacheInconsistency(ARecoverCache);
end;

procedure TdxServerModeCustomDataSource.ErrorTimerHandler(Sender: TObject);
begin
  TcxTimer(Sender).Enabled := False;
  case CacheState of
    smcsInconsistent:
      DoInconsistentCacheTimerHandler;
    smcsFatal:
      DoFatalErrorTimerHandler;
  end;
end;

procedure TdxServerModeCustomDataSource.StartErrorTimer;
begin
  if Assigned(DataController) then
    DataController.DataControllerInfo.MakeRecoveryInfo;
  FErrorTimer.Enabled := True;
end;

function TdxServerModeCustomDataSource.GetCacheClass: TdxServerModeCacheClass;
begin
  Result := TdxServerModeKeyedCache;
end;

function TdxServerModeCustomDataSource.GetCriteriaClass: TdxServerModeCriteriaClass;
begin
  Result := TdxServerModeCriteria;
end;

function TdxServerModeCustomDataSource.GetGroupInfoDescriptorsClass: TdxServerModeGroupInfoDescriptorsClass;
begin
  Result := TdxServerModeGroupInfoDescriptors;
end;

function TdxServerModeCustomDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeDataSourceCustomHelper;
end;

function TdxServerModeCustomDataSource.GetKeyFieldNames: string;
begin
  Result := FKeyFieldNames.DelimitedText;
end;

function TdxServerModeCustomDataSource.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := Helper.GetOptionsClass;
end;

function TdxServerModeCustomDataSource.GetSortInfoDescriptorsClass: TdxServerModeSortInfoDescriptorsClass;
begin
  Result := TdxServerModeSortInfoDescriptors;
end;

function TdxServerModeCustomDataSource.GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
begin
  Result := Helper.GetQueryBuilderClass;
end;

function TdxServerModeCustomDataSource.GetDataSetClass: TdxDataSetClass;
begin
  Result := Helper.GetDataSetClass;
end;

function TdxServerModeCustomDataSource.GetSummariesClass: TdxServerModeSummaryDescriptorsClass;
begin
  Result := TdxServerModeSummaryDescriptors;
end;

procedure TdxServerModeCustomDataSource.DoExecuteCommand(AResultSet: PDataSet;
  const ACommand: string; AParams: TParams; AParamCheck: Boolean);
begin
  Helper.DoExecuteCommand(AResultSet, ACommand, AParams, AParamCheck);
  if AResultSet <> nil then
    Helper.PrepareDatasetForIterate(AResultSet^);
end;

function TdxServerModeCustomDataSource.ExecuteCommand(AResultSet: PDataSet;
  const ACommandText: string; AParams: TParams; AParamCheck: Boolean;
  ACheckSettings: Boolean = True): Boolean;
begin
  if ACheckSettings then
    CheckSettings;
  Result := IsConnected;
  if Result then
    DoExecuteCommand(AResultSet, ACommandText, AParams, AParamCheck);
end;

function TdxServerModeCustomDataSource.ExecuteCommand(AResultSet: PDataSet; const AFields: TdxServerModeDescriptors;
  ACriteria: TdxServerModeCriteria; ASortInfo: TdxServerModeSortInfoDescriptors;
  AGroupInfo: TdxServerModeGroupInfoDescriptors;
  ATopRecords: Integer; ASkipRecords: Integer; ADistinct: Boolean): Boolean;
var
  I: Integer;
  ABuilder: TdxServerModeCustomSQLQueryBuilder;
begin
  Result := IsConnected;
  if Result then
  begin
    ABuilder := GetQueryBuilderClass.Create(Self);
    try
      ABuilder.Distinct := ADistinct;
      ABuilder.Fields := AFields;
      ABuilder.Criteria := ACriteria;
      ABuilder.SortInfo := ASortInfo;
      ABuilder.GroupInfo := AGroupInfo;
      ABuilder.Summaries := Summaries;
      ABuilder.TopRecords := ATopRecords;
      ABuilder.SkipRecords := ASkipRecords;
      ABuilder.Calculate;
      Result := ExecuteCommand(AResultSet, ABuilder.CommandText, ABuilder.Params, ABuilder.ParamCheck);
      if Result and not Adapter.IsSkipClauseSupported then
        for I := 1 to ASkipRecords do
          AResultSet.Next;
    finally
      ABuilder.Free;
    end;
  end;
end;

function TdxServerModeCustomDataSource.GetFieldsRetrieveQuery: string;
var
  ABuilder: TdxServerModeCustomSQLQueryBuilder;
begin
  ABuilder := GetQueryBuilderClass.Create(Self);
  try
    Result := Format(Adapter.GetFieldsRetrieveQueryFormat, [ABuilder.GetFromSQLString]);
  finally
    ABuilder.Free;
  end;
end;

procedure TdxServerModeCustomDataSource.FetchKeys(AKeys: TdxServerModeKeyList; AWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors; ASkip, ATake: Integer);
var
  ADataSet: TDataSet;
begin
  if ExecuteCommand(@ADataSet, KeyFields, AWhere, ASortInfo, nil, ATake, ASkip, False) then
  try
    while not ADataSet.Eof do
    begin
      AKeys.Add(dxCreateServerModeRow(ADataSet.Fields));
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TdxServerModeCustomDataSource.FetchRows(ARows: TdxServerModeRowList; AWhere: TdxServerModeCriteria;
  ASortInfo: TdxServerModeSortInfoDescriptors; ATopRecords: Integer);
var
  ADataSet: TDataSet;
begin
  if ExecuteCommand(@ADataSet, FieldDescriptors, AWhere, ASortInfo, nil, ATopRecords, 0, False) then
  try
    while not ADataSet.Eof do
    begin
      ARows.Add(dxCreateServerModeRow(ADataSet.Fields));
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TdxServerModeCustomDataSource.FetchRowsByKeys(ARows: TdxServerModeRowList; AKeys: TdxServerModeKeyList);
var
  ACriteria: TdxServerModeCriteria;
begin
  ACriteria := GetFetchRowsByKeysCondition(AKeys);
  try
    FetchRows(ARows, ACriteria, nil, 0);
  finally
    ACriteria.Free;
  end;
end;

function TdxServerModeCustomDataSource.FindFirstKey(AWhere: TdxServerModeCriteria;
  ASearchDown: Boolean): TdxServerModeRow;
var
  ADataSet: TDataSet;
  ASortInfo: TdxServerModeSortInfoDescriptors;
begin
  Result := Null;
  ASortInfo := SortInfo.Clone(not ASearchDown);
  try
    if ExecuteCommand(@ADataSet, KeyFields, AWhere, ASortInfo, nil, 1, 0, False) then
    try
      if not ADataSet.Eof then
        Result := dxCreateServerModeRow(ADataSet.Fields);
    finally
      ADataSet.Free;
    end;
  finally
    ASortInfo.Free;
  end;
end;

function TdxServerModeCustomDataSource.GetCount(ACriteria: TdxServerModeCriteria): Integer;
var
  ADataSet: TDataSet;
  AFields: TdxServerModeDescriptors;
begin
  Result := 0;
  AFields := TdxServerModeDescriptors.Create(Self);
  try
    AFields.AddItem(TdxServerModeSummaryDescriptor.Create(skCount));
    if ExecuteCommand(@ADataSet, AFields, ACriteria, nil, nil, 0, 0, False) then
    try
      if not ADataSet.Eof then
        Result := ADataSet.Fields[0].AsInteger
    finally
      ADataSet.Free;
    end;
  finally
    AFields.Free;
  end;
end;

function TdxServerModeCustomDataSource.GetFetchRowByIndexCondition(ARowIndex: Integer): TdxServerModeCriteria;
var
  AKey: TdxServerModeRow;
begin
  AKey := Cache.GetRowKey(ARowIndex);
  Result := GetFetchRowByKeyCondition(AKey);
end;

function TdxServerModeCustomDataSource.GetFetchRowByKeyCondition(AKey: TdxServerModeRow): TdxServerModeCriteria;
var
  I: Integer;
begin
  Result := CreateCriteria;
  if KeyFields.Count = 0 then
    Exit
  else
    if KeyFields.Count = 1 then
      Result.AddItem(Result.Root, KeyFields[0], foEqual, AKey)
    else
    begin
      Result.Root.BoolOperatorKind := fboAnd;
      for I := 0 to KeyFields.Count - 1 do
        Result.AddItem(Result.Root, KeyFields[I], foEqual, AKey[I]);
    end;
end;

function TdxServerModeCustomDataSource.GetFetchRowsByKeysCondition(AKeys: TdxServerModeKeyList): TdxServerModeCriteria;
var
  I, J: Integer;
  ACriteriaItemList: TcxFilterCriteriaItemList;
begin
  Result := CreateCriteria;
  if KeyFields.Count = 0 then
    Exit
  else
    if KeyFields.Count = 1 then
      MakeCriteriaWithBetweenOperator(Result, AKeys)
    else
    begin
      Result.Root.BoolOperatorKind := fboOr;
      for I := 0 to AKeys.Count - 1 do
      begin
        ACriteriaItemList := Result.Root.AddItemList(fboAnd);
        for J := 0 to KeyFields.Count - 1 do
          Result.AddItem(ACriteriaItemList, KeyFields[J], foEqual, AKeys[I][J]);
      end;
    end;
end;

procedure TdxServerModeCustomDataSource.DoPopulateFields(AList: TStrings);
begin
  if Fields.Count > 0 then
    Fields.GetFieldNames(AList)
  else
    FieldDefs.GetItemNames(AList);
end;

procedure TdxServerModeCustomDataSource.DoPopulateKeyFields(
  AList: TStrings);
begin
// do nothing
end;

function TdxServerModeCustomDataSource.PrepareChildren(AGroupWhere: TdxServerModeCriteria;
  AGroupByDescriptor: TdxServerModeSortInfoDescriptor;
  ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoDataList;

  procedure PopulateChildren(var AList: TdxServerModeGroupInfoDataList; ADataSet: TDataSet;
    ASummaryCount: Integer);
  var
    ARow: TdxServerModeRow;
    I, ACount: Integer;
    AGroupValue: Variant;
  begin
    while not ADataSet.Eof do
    begin
      ACount := ADataSet.Fields[0].AsInteger;
      AGroupValue := dxServerModeGetFieldValue(ADataSet.Fields[1]);
      if ASummaryCount > 0 then
      begin
        ARow := VarArrayCreate([0, ASummaryCount], varVariant);
        if ADataSet.FieldCount - 2 = ASummaryCount then
          for I := 0 to ASummaryCount - 1 do
            ARow[I] := dxServerModeGetFieldValue(ADataSet.Fields[I + 2]);
      end
      else
        ARow := Null;
      Result.Add(TdxServerModeGroupInfoData.Create(AGroupValue, ACount, ARow));
      ADataSet.Next;
    end;
  end;

var
  AFields: TdxServerModeDescriptors;
  ASortInfo: TdxServerModeSortInfoDescriptors;
  AGroups: TdxServerModeGroupInfoDescriptors;
  ADataSet: TDataSet;
  AGroupLevelSorted: Boolean;
  AExecuted: Boolean;
  I: Integer;
begin
  Result := TdxServerModeGroupInfoDataList.Create;

  AFields := TdxServerModeDescriptors.Create(Self);
  try
    AFields.AddItem(TdxServerModeSummaryDescriptor.Create(skCount));
    AFields.CreateLink(AGroupByDescriptor);
    AFields.CreateLinks(ASummaries);

    ASortInfo := TdxServerModeSortInfoDescriptors.Create(Self);
    try
      AGroupLevelSorted := ASummaries.IsGroupLevelSorted(AGroupByDescriptor.Index);
      for I := ASummaries.Count - 1 downto 0 do
      begin
        if (ASummaries[I].Sorted) and (ASummaries[I].Kind <> skNone) and
          (((ASummaries[I].GroupLevel = -1) and not AGroupLevelSorted) or
          (AGroupLevelSorted and (ASummaries[I].GroupLevel = AGroupByDescriptor.Index))) then
        begin
          ASortInfo.CreateLink(ASummaries[I]).Index := 0;
          ASortInfo[0].IsDesc := AGroupByDescriptor.IsDesc;
        end;
      end;
      ASortInfo.AddItem(AGroupByDescriptor.Clone);
      AGroups := CreateGroupInfo;
      try
        AExecuted := False;
        AGroups.CreateLink(AGroupByDescriptor);
        try
          try
            AExecuted := ExecuteCommand(@ADataSet, AFields, AGroupWhere, ASortInfo, AGroups, 0, 0, False);
          except
            AFields.Clear;
            AFields.AddItem(TdxServerModeSummaryDescriptor.Create(skCount));
            AFields.CreateLink(AGroupByDescriptor);
            AExecuted := ExecuteCommand(@ADataSet, AFields, AGroupWhere, ASortInfo, AGroups, 0, 0, False);
          end;
        finally
          if AExecuted then
          try
            PopulateChildren(Result, ADataSet, ASummaries.Count);
          finally
            ADataSet.Free;
          end;
        end;
      finally
        AGroups.Free;
      end;
    finally
      ASortInfo.Free;
    end;
  finally
    AFields.Free;
  end;
end;

function TdxServerModeCustomDataSource.PrepareTopGroupInfo(
  ASummaries: TdxServerModeSummaryDescriptors): TdxServerModeGroupInfoData;
var
  ARow: TdxServerModeRow;
  ACount: Integer;
  I: Integer;
  ADataSet: TDataSet;
  AFields: TdxServerModeDescriptors;
  AExecuted: Boolean;
begin
  ACount := 0;
  ARow := Null;
  AFields := TdxServerModeDescriptors.Create(Self);
  try
    AFields.AddItem(TdxServerModeSummaryDescriptor.Create(skCount));
    AFields.CreateLinks(ASummaries);
    AExecuted := False;
    try
      try
        AExecuted := ExecuteCommand(@ADataSet, AFields, GeneralFilter, nil, nil, 0, 0, False);
      except
        AFields.Clear;
        AFields.AddItem(TdxServerModeSummaryDescriptor.Create(skCount));
        AExecuted := ExecuteCommand(@ADataSet, AFields, GeneralFilter, nil, nil, 0, 0, False);
      end;
    finally
      if AExecuted then
      try
        if not ADataSet.Eof then
        begin
          ACount := ADataSet.Fields[0].AsInteger;
          if ASummaries.Count > 0 then
          begin
            ARow := VarArrayCreate([0, ASummaries.Count - 1], varVariant);
            if ADataSet.FieldCount - 1 = ASummaries.Count then
              for I := 0 to ASummaries.Count - 1 do
                ARow[I] := dxServerModeGetFieldValue(ADataSet.Fields[I + 1]);
          end;
        end;
      finally
        ADataSet.Free;
      end;
    end;
  finally
    AFields.Free;
    Result := TdxServerModeGroupInfoData.Create(Null, ACount, ARow);
  end;
end;

procedure TdxServerModeCustomDataSource.Change(AChanges: TdxServerModeDataSourceChanges);
begin
  FChanges := FChanges + AChanges;
  Changed;
end;

procedure TdxServerModeCustomDataSource.Changed;
begin
  Update;
end;

procedure TdxServerModeCustomDataSource.Clear;
begin
  BeginUpdate;
  try
    Cache.SoftReset;
    FindFilter.Clear;
    Filter.Clear;
    GeneralFilter.Clear;
    SortInfo.Clear;
    GroupCount := 0;
    TotalSummaries.Clear;
    Summaries.Clear;
    FieldDescriptors.Clear;
    KeyFields.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TdxServerModeCustomDataSource.Update;
var
  ASendNotification: Boolean;
begin
  if CanUpdate then
  begin
    ASendNotification := ctConnection in FChanges;
    BeginUpdate;
    try
      DoUpdate;
    finally
      CancelUpdate;
      if ASendNotification then
        SendNotifications;
    end;
  end;
end;

procedure TdxServerModeCustomDataSource.UpdateFieldIndex(AField: TdxServerModeDataField);
begin
  AField.FFieldIndex := FieldDescriptors.IndexByName(AField.FieldName);
end;

function TdxServerModeCustomDataSource.IsConnected: Boolean;
begin
  Result := not FIsDisconnecting and (Connection <> nil) and
    Connection.Connected;
end;

function TdxServerModeCustomDataSource.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxServerModeCustomDataSource.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxServerModeCustomDataSource.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TdxServerModeCustomDataSource.IsConsistentCache: Boolean;
begin
  Result := CacheState = smcsConsistent;
end;

procedure TdxServerModeCustomDataSource.RecoveryFromInconsistentState;
begin
  DataController.BeginUpdate;
  try
    Refresh;
    if IsConsistentCache then
      DataController.DataControllerInfo.ApplyRecoveryInfo;
  finally
    DataController.EndUpdate;
  end;
end;

procedure TdxServerModeCustomDataSource.MakeCriteriaWithBetweenOperator(
  var ACriteria: TdxServerModeCriteria; const AKeys: TdxServerModeKeyList);

  procedure AddKeys(const AKeyField: TdxServerModeFieldDescriptor; const AKeyValues: TVariantArray);
  var
    AKeyValue: Variant;
  begin
    if AKeys.Count = 1 then
    begin
      AKeyValue := AKeyValues[0];
      if VarIsNull(AKeyValue) then
        ACriteria.AddItem(AKeyField, foEqual, Null)
      else
        ACriteria.AddItem(AKeyField, foEqual, AKeyValue);
    end
    else
      ACriteria.AddItem(AKeyField, foInList, AKeyValues);
  end;

var
  I, AIndex: Integer;
  AHasNullValueInList: Boolean;
  AKeyValues: TVariantArray;
  AKeyField: TdxServerModeFieldDescriptor;
begin
  AHasNullValueInList := False;
  for I := Low(AKeys.List) to High(AKeys.List) do
    if VarIsNull(AKeys.List[I]) then
    begin
      AHasNullValueInList := True;
      Break;
    end;
  AKeyField := KeyFields[0];
  if AHasNullValueInList then
  begin
    ACriteria.AddItem(AKeyField, foEqual, Null);
    AKeyValues := AKeys.List;
    SetLength(AKeyValues, AKeys.Count);
    try
      AIndex := 0;
      for I := 0 to AKeys.Count - 1 do
        if not VarIsNull(AKeys.List[I]) then
        begin
          AKeyValues[AIndex] := AKeys.List[I];
          Inc(AIndex);
        end;
      if AIndex > 0 then
      begin
        ACriteria.Root.BoolOperatorKind := fboOr;
        SetLength(AKeyValues, AIndex);
        AddKeys(AKeyField, AKeyValues);
      end;
    finally
      AKeyValues := nil;
    end;
  end
  else
    AddKeys(AKeyField, AKeys.List);
end;

procedure TdxServerModeCustomDataSource.InitializeKeyFields;
var
  AList: TStrings;
  I: Integer;
  AField: TField;
begin
  AList := TStringList.Create;
  try
    PopulateKeyFields(AList);
    for I := 0 to AList.Count - 1 do
    begin
      if Fields.FindField(AList[I]) = nil then
      begin
        AField := CreateField(FieldDefs.Find(AList[I]), nil);
        FieldDescriptors.Add(AField);
      end;
      AddKeyField(Fields.FieldByName(AList[I]));
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxServerModeCustomDataSource.FilterChangedHandler(Sender: TObject);
begin
  Change([ctFiltering, ctCache]);
end;

procedure TdxServerModeCustomDataSource.ConnectionChangedHandler(Sender: TObject; Connecting: Boolean);
begin
  Include(FChanges, ctConnection);
  FIsDisconnecting := not Connecting;
  if Active and FIsDisconnecting and not (csDestroying in ComponentState) then
    Close;
end;

procedure TdxServerModeCustomDataSource.SortInfoChangedHandler(Sender: TdxServerModeDescriptors;
  AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
begin
  Include(FChanges, ctSorting);
  if (dctSorting in AChanges) and (AItem.Index < GroupCount) then
    Include(FChanges, ctGroups);
  Changed;
end;

procedure TdxServerModeCustomDataSource.SummariesChangedHandler(Sender: TdxServerModeDescriptors;
  AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
begin
  if (dctSorting in AChanges) or (TdxServerModeSummaryDescriptor(AItem).Sorted and ([dctLink, dctSummaryKind] * AChanges <> [])) then
    Include(FChanges, ctSortingSummaries);
  Include(FChanges, ctSummaries);
  Changed;
end;

procedure TdxServerModeCustomDataSource.TotalSummariesChangedHandler(Sender: TdxServerModeDescriptors;
  AItem: TdxServerModeDescriptor; AChanges: TdxServerModeDescriptorChanges);
begin
  Change([ctTotalSummaries]);
end;

procedure TdxServerModeCustomDataSource.AssignFields;
var
  I: Integer;
  AComponent: TComponent;
  AField: TField;
begin
  BeginUpdate;
  try
    for I := 0 to FLoadingFieldNames.Count - 1 do
    begin
      AComponent := Owner.FindComponent(FLoadingFieldNames[I]);
      if AComponent is TField then
      begin
        AField := TField(AComponent);
        if not FieldDefs.IndexOf(AField.FieldName) <> -1 then
          FieldDefs.Add(AField.FieldName, AField.DataType, AField.Size, AField.Required);
        AField.DataSet := DataSet;
      end;
    end;
  finally
    CancelUpdate;
  end;
end;

procedure TdxServerModeCustomDataSource.ReadFields(Reader: TReader);
begin
  Reader.ReadListBegin;
  try
    FLoadingFieldNames.Clear;
    while not Reader.EndOfList do
      FLoadingFieldNames.Add(Reader.ReadString);
  finally
    Reader.ReadListEnd;
  end;
end;

procedure TdxServerModeCustomDataSource.WriteFields(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Fields.Count - 1 do
    if Fields[I].Owner = Writer.Root then
      Writer.WriteString(FDataSet.Fields[I].Name);
  Writer.WriteListEnd;
end;

function TdxServerModeCustomDataSource.GetDataControllerPercentWildcard: Char;
begin
  if DataController <> nil then
    Result := DataController.Filter.PercentWildcard
  else
    Result := Adapter.GetPercentWildcard;
end;

function TdxServerModeCustomDataSource.GetDataControllerUnderscoreWildcard: Char;
begin
  if DataController <> nil then
    Result := DataController.Filter.UnderscoreWildcard
  else
    Result := Adapter.GetUnderscoreWildcard;
end;

procedure TdxServerModeCustomDataSource.DoSetActive(Value: Boolean);
begin
  FActive := Value;
  Change([ctConnection]);
end;

function TdxServerModeCustomDataSource.GetAdapter: TdxServerModeCustomSQLAdapter;
begin
  CheckSQLAdapter;
  Result := SQLAdapter;
end;

function TdxServerModeCustomDataSource.GetCacheState: TdxServerModeCacheState;
begin
  Result := Cache.State;
end;

function TdxServerModeCustomDataSource.GetFieldDefs: TFieldDefs;
begin
  Result := DataSet.FieldDefs;
end;

function TdxServerModeCustomDataSource.GetFields: TFields;
begin
  Result := DataSet.Fields;
end;

function TdxServerModeCustomDataSource.GetLastErrorMessage: string;
begin
  Result := Cache.LastErrorMessage;
end;

function TdxServerModeCustomDataSource.GetRow(Index: Integer): TdxServerModeRow;
begin
  Result := Cache.Indexer(Index);
end;

function TdxServerModeCustomDataSource.GetRowCount: Integer;
begin
  if Active then
    Result := Cache.Count
  else
    Result := 0;
end;

function TdxServerModeCustomDataSource.GetSQLAdapterClassName: string;
begin
  if FSQLAdapter = nil then
    Result := ''
  else
    Result := FSQLAdapter.ClassName;
end;

procedure TdxServerModeCustomDataSource.RecreateSQLAdapter;
begin
  FreeAndNil(FSQLAdapter);
  if FSQLAdapterClass <> nil then
  begin
    FSQLAdapter := FSQLAdapterClass.Create(Self);
    Change([ctConnection]);
  end;
end;

procedure TdxServerModeCustomDataSource.BeginUpdateFields;
begin
  if Assigned(DataController) then
    DataController.BeginUpdateFields;
end;

procedure TdxServerModeCustomDataSource.EndUpdateFields;
begin
  if Assigned(DataController) then
    DataController.EndUpdateFields;
end;

procedure TdxServerModeCustomDataSource.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
    FStreamedActive := Value
  else
    if Active <> Value then
    begin
      if Value then
      begin
        DoBeforeOpen;
        try
          InternalOpen;
        finally
          OpenComplete;
        end;
      end
      else
      begin
        if not IsDestroying then
          DoBeforeClose;
        InternalClose;
        if not IsDestroying then
          DoAfterClose;
      end;
    end;
end;

procedure TdxServerModeCustomDataSource.SendNotifications;
begin
  if not IsLocked and Assigned(DataController) and not DataController.DataChangedNotifyLocked then
    DataController.ActiveChanged(Self.Active);
end;

procedure TdxServerModeCustomDataSource.SetConnection(Value: TCustomConnection);
begin
  if Connection <> Value then
  begin
    CheckInactive;
    if Connection <> nil then
    begin
      TCustomConnectionAccess(Connection).UnRegisterClient(Self);
      Connection.RemoveFreeNotification(Self);
    end;
    FConnection := Value;
    FIsDisconnecting := False;
    if Connection <> nil then
    begin
      Connection.FreeNotification(Self);
      TCustomConnectionAccess(Connection).RegisterClient(Self, ConnectionChangedHandler);
    end;
    Include(FChanges, ctConnection);
    DoSynchronizeDataSetConnection(DataSet);
    Changed;
  end;
end;

procedure TdxServerModeCustomDataSource.SetGroupCount(Value: Integer);
begin
  if GroupCount <> Value then
  begin
    FGroupCount := Value;
    Change([ctGroups]);
  end;
end;

procedure TdxServerModeCustomDataSource.SetKeyFieldNames(const Value: string);
var
  APos: Integer;
begin
  if KeyFieldNames <> Value then
  begin
    CheckInactive;
    FKeyFieldNames.Clear;
    APos := 1;
    while APos <= Length(Value) do
      FKeyFieldNames.Add(ExtractFieldName(Value, APos));
  end;
end;

procedure TdxServerModeCustomDataSource.SetOptions(Value: TdxServerModeDataSourceOptions);
begin
  FOptions.Assign(Value);
end;

procedure TdxServerModeCustomDataSource.SetSQLAdapter(
  const Value: TdxServerModeCustomSQLAdapter);
begin
  if (FSQLAdapter <> nil) and (Value <> nil) then
    FSQLAdapter.Assign(Value);
end;

procedure TdxServerModeCustomDataSource.SetSQLAdapterClass(
  const Value: TdxServerModeCustomSQLAdapterClass);
begin
  if FSQLAdapterClass <> Value then
  begin
    FSQLAdapterClass := Value;
    RecreateSQLAdapter;
  end;
end;

procedure TdxServerModeCustomDataSource.SetSQLAdapterClassName(
  const Value: string);
begin
  if SQLAdapterClassName <> Value then
  begin
    with dxGetServerModeSQLAdapters do
      SQLAdapterClass := TdxServerModeCustomSQLAdapterClass(FindByClassName(Value));
  end;
end;

{ TdxServerModeDataSource }

procedure TdxServerModeDataSource.PopulateTableNames(AList: TStrings);
begin
  AList.Clear;
  if IsConnected then
    DoPopulateTableNames(AList);
end;

function TdxServerModeDataSource.CanModify: Boolean;
begin
  Result := not ReadOnly and not IsViewLinked;
end;

procedure TdxServerModeDataSource.CheckSettings;
begin
  inherited;
  CheckTableName;
end;

procedure TdxServerModeDataSource.CheckTableName;
begin
  if TableName = '' then
    DatabaseError(sdxMissingTableName, Self);
end;

function TdxServerModeDataSource.GetFromSQLString: string;
begin
  Result := Format(Adapter.GetTableNameFormat, [TableName]);
end;

procedure TdxServerModeDataSource.Delete(ACriteria: TdxServerModeCriteria);
var
  ABuilder: TdxServerModeCustomSQLQueryBuilder;
begin
  ABuilder := GetQueryBuilderClass.Create(Self);
  try
    ABuilder.Criteria := ACriteria;
    ABuilder.Calculate;
    ExecuteCommand(nil, ABuilder.CommandText, ABuilder.Params, ABuilder.ParamCheck);
  finally
    ABuilder.Free;
  end;
  Change([ctCache]);
end;

function TdxServerModeDataSource.GetProvider: TdxCustomServerModeDataProvider;
begin
  if DataController <> nil then
    Result := DataController.Provider
  else
    Result := nil;
end;

procedure TdxServerModeDataSource.DoPopulateKeyFields(AList: TStrings);
var
  ACommand: string;
  ADataSet: TDataSet;
begin
  ACommand := Adapter.GetSQLQueryKeyFieldNames(TableName);
  if (ACommand <> '') and ExecuteCommand(@ADataSet, ACommand, nil, False) then
  try
    while not ADataSet.Eof do
    begin
      AList.Add(Trim(ADataSet.Fields[0].AsString));
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

function TdxServerModeDataSource.IsViewLinked: Boolean;
var
  AList: TStrings;
  I: Integer;
begin
  if not FIsViewLinkedCalculated then
  begin
    FIsViewLinked := False;
    AList := TStringList.Create;
    try
      PopulateViewNames(AList);
      for I := 0 to AList.Count - 1 do
      begin
        FIsViewLinked := CompareText(TableName, AList[I]) = 0;
        if FIsViewLinked then
          Break;
      end;
    finally
      AList.Free;
    end;
    FIsViewLinkedCalculated := True;
  end;
  Result := FIsViewLinked;
end;

procedure TdxServerModeDataSource.PopulateViewNames(AList: TStrings);
begin
  AList.Clear;
  if IsConnected then
    DoPopulateViewNames(AList);
end;

function TdxServerModeDataSource.PostEditingData(ARowIndex: Integer; const AEditingRow: Variant): Variant;
var
  ABuilder: TdxServerModeCustomSQLQueryBuilder;
  ACriteria: TdxServerModeCriteria;
  ADataSet: TDataSet;
  AKey: Variant;
  I: Integer;
begin
  Result := inherited PostEditingData(ARowIndex, AEditingRow);
  ABuilder := GetQueryBuilderClass.Create(Self);
  try
    ABuilder.Fields := FieldDescriptors;
    if ARowIndex < RowCount then
    begin
      Result := Cache.GetRowKey(ARowIndex);
      ACriteria := GetFetchRowByKeyCondition(Result);
    end
    else
      ACriteria := CreateCriteria;
    try
      AKey := Cache.GetKeyFromRow(AEditingRow);
      if VarIsArray(AKey) then
      begin
        if VarIsEmpty(Result) then
          Result := AKey
        else
          for I := VarArrayLowBound(AKey, 1) to VarArrayHighBound(AKey, 1) do
            if not VarIsEmpty(AKey[I]) then
              Result[I] := AKey[I];
      end
      else
        if not VarIsEmpty(AKey) then
          Result := AKey;
      ABuilder.Criteria := ACriteria;
      ABuilder.Values := AEditingRow;
      ABuilder.Fields := FieldDescriptors;
      ABuilder.Calculate;
    finally
      ACriteria.Free;
    end;
    if Adapter.CanIdentifyInsertingRow then
    begin
      if ExecuteCommand(@ADataSet, ABuilder.CommandText, ABuilder.Params, ABuilder.ParamCheck) then
      try
        if not ADataSet.Eof then
          Result := dxCreateServerModeRow(ADataSet.Fields);
      finally
        ADataSet.Free;
      end;
    end
    else
      ExecuteCommand(nil, ABuilder.CommandText, ABuilder.Params, ABuilder.ParamCheck);
  finally
    ABuilder.Free;
  end;
  Change([ctCache]);
end;

procedure TdxServerModeDataSource.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
  end;
end;

procedure TdxServerModeDataSource.SetTableName(const Value: string);
begin
  if csReading in ComponentState then
    FTableName := Value
  else
    if (FTableName <> Value) then
    begin
      CheckInactive;
      FTableName := Value;
      FIsViewLinkedCalculated := False;
      Include(FChanges, ctConnection);
      Changed;
    end;
end;

{ TdxServerModeQueryDataSource }

constructor TdxServerModeQueryDataSource.Create(AOwner: TComponent);
begin
  inherited;
  FSQL := TStringList.Create;
end;

destructor TdxServerModeQueryDataSource.Destroy;
begin
  FreeAndNil(FSQL);
  inherited;
end;

procedure TdxServerModeQueryDataSource.CheckSettings;
begin
  inherited;
  CheckSQL;
end;

procedure TdxServerModeQueryDataSource.CheckSQL;
begin
  if SQL.Text = '' then
    DatabaseError(sdxMissingSQL, Self);
end;

function TdxServerModeQueryDataSource.GetFromSQLString: string;
begin
  Result := Format(Adapter.GetFromQueryFormat, [SQL.Text]);
end;

procedure TdxServerModeQueryDataSource.SetSQL(const Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    CheckInactive;
    SQL.Assign(Value);
    Include(FChanges, ctConnection);
    Changed;
  end;
end;

{ TdxServerModeDataField }

constructor TdxServerModeDataField.Create(AFieldList: TcxCustomDataFieldList);
begin
  inherited Create(AFieldList);
  FFieldIndex := -1;
end;

destructor TdxServerModeDataField.Destroy;
begin
  if Assigned(ParentField) then
    ParentField.RemoveField(Self);
  ClearFields;
  inherited Destroy;
end;

function TdxServerModeDataField.CanModify(AEditValueSource: TcxDataEditValueSource): Boolean;
begin
  Result := True;
end;

function TdxServerModeDataField.IsUnbound: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataField.IsValueDefInternal: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeDataField.AddField(ADataField: TdxServerModeDataField);
begin
  if FFields = nil then
    FFields := TList.Create;
  if FFields.IndexOf(ADataField) = -1 then
  begin
    if ADataField.FParentField <> nil then
      InvalidOperation('ADataField.FParentField <> nil');
    FFields.Add(ADataField);
    ADataField.FParentField := Self;
  end;
end;

procedure TdxServerModeDataField.ClearFields;
var
  I: Integer;
begin
  if FFields <> nil then
  begin
    for I := FFields.Count - 1 downto 0 do
      TdxServerModeDataField(FFields[I]).Free;
    FFields.Free;
    FFields := nil;
  end;
end;

procedure TdxServerModeDataField.Reassign(ASource: TcxCustomDataField);
begin
  inherited Reassign(ASource);
  if ASource is TdxServerModeDataField then
  begin
    FField := TdxServerModeDataField(ASource).FField;
    FFields := TdxServerModeDataField(ASource).FFields;
    TdxServerModeDataField(ASource).FField := nil;
    TdxServerModeDataField(ASource).FFields := nil;
  end;
end;

procedure TdxServerModeDataField.RemoveField(ADataField: TdxServerModeDataField);
begin
  if FFields <> nil then
    FFields.Remove(ADataField);
  ADataField.FParentField := nil;
end;

procedure TdxServerModeDataField.RemoveNotification(AComponent: TComponent);
begin
  if Assigned(ReferenceField) then Exit;
  inherited RemoveNotification(AComponent);
  if Field = AComponent then
    Field := nil;
end;

function TdxServerModeDataField.SetEditingData(ARecordIndex: Integer; const AValue: Variant): Boolean;
begin
  FModified := (FieldIndex > -1) and Provider.SetEditingData(ARecordIndex, FieldIndex, AValue);
  Result := FModified;
end;

procedure TdxServerModeDataField.SetPropertiesByField(AField: TField; AFieldChanged: Boolean);
var
  ATextStored, AIsCurrency, AValueTypeClassChanged: Boolean;
  AValueTypeClass: TcxValueTypeClass;
  AChanged: Boolean;
begin
  if Assigned(ReferenceField) then Exit;
  repeat
    if not IsInternal then
      ATextStored := IsFieldFormatted(AField, DataController.GetItemValueSource(Index) = evsText)
    else
      ATextStored := IsFieldFormatted(AField, False);
    AValueTypeClass := GetValueTypeClassByField(AField);
    AIsCurrency := IsSimpleCurrencyField(AField);
    AValueTypeClassChanged := (ValueTypeClass <> AValueTypeClass) or (IsCurrency <> AIsCurrency);
    AChanged := AValueTypeClassChanged or (TextStored <> ATextStored);
    if AChanged then
    begin
      BeginRecreateData;
      try
        TextStored := ATextStored;
        ValueTypeClass := AValueTypeClass;
        FIsCurrency := AIsCurrency;
      finally
        EndRecreateData;
      end;
      if AValueTypeClassChanged then
      begin
        DoPropertiesChanged;
        AFieldChanged := False;
      end;
    end
    else
      if AFieldChanged then
        DoPropertiesChanged;
  until not AChanged;
end;

function TdxServerModeDataField.GetDataController: TdxServerModeDataController;
begin
  Result := inherited DataController as TdxServerModeDataController;
end;

function TdxServerModeDataField.GetField: TField;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TdxServerModeDataField).Field
  else
    Result := FField;
end;

function TdxServerModeDataField.GetFieldIndex: Integer;
begin
  if Assigned(ReferenceField) then
    Result := TdxServerModeDataField(ReferenceField).FieldIndex
  else
    Result := FFieldIndex;
end;

function TdxServerModeDataField.GetFieldItem(Index: Integer): TdxServerModeDataField;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TdxServerModeDataField).Fields[Index]
  else
    Result := TdxServerModeDataField(FFields[Index]);
end;

function TdxServerModeDataField.GetFieldCount: Integer;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TdxServerModeDataField).FieldCount
  else
    if FFields <> nil then
      Result := FFields.Count
    else
      Result := 0;
end;

function TdxServerModeDataField.GetIsCurrency: Boolean;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TdxServerModeDataField).IsCurrency
  else
    Result := FIsCurrency;
end;

function TdxServerModeDataField.GetIsLookup: Boolean;
begin
  Result := Assigned(Field) and Field.Lookup;
end;

function TdxServerModeDataField.GetProvider: TdxCustomServerModeDataProvider;
begin
  Result := DataController.Provider;
end;

procedure TdxServerModeDataField.SetField(Value: TField);
begin
  if Assigned(ReferenceField) then Exit;
  if (Value <> nil) and (csDestroying in Value.ComponentState) then
    Value := nil;
  if FField <> Value then
  begin
    if FField <> nil then
      FField.RemoveFreeNotification(Notifier);
    FField := Value;
    FFieldIndex := -1;
    if Assigned(Value) then
      Value.FreeNotification(Notifier);
    ClearFields;
  end;
end;

procedure TdxServerModeDataField.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    DataController.PrepareField(Self);
  end;
end;

{ TdxServerModeSummaryItem }

procedure TdxServerModeSummaryItem.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeSummaryItem then
  begin
    BeginUpdate;
    try
      FieldName := TdxServerModeSummaryItem(Source).FieldName;
      inherited Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxServerModeSummaryItem.DataField: TcxCustomDataField;
begin
  if Assigned(FDataField) then
    Result := FDataField
  else
    Result := inherited DataField;
end;

function TdxServerModeSummaryItem.GetFieldName: string;
begin
  if ServerModeDataField <> nil then
    Result := ServerModeDataField.FieldName
  else
    Result := FFieldName;
end;

function TdxServerModeSummaryItem.GetServerModeDataController: TdxServerModeDataController;
begin
  Result := inherited DataController as TdxServerModeDataController;
end;

function TdxServerModeSummaryItem.AllowKind(Value: TcxSummaryKind): Boolean;
begin
  Result := not DataController.Active or
    DataController.IsLoading or (DataField = nil) or
    (Value in DataController.GetAllowedSummaryKinds(DataField));
end;

function TdxServerModeSummaryItem.CanSetKind(Value: TcxSummaryKind): Boolean;
begin
  Result := inherited CanSetKind(Value) and AllowKind(Value);
end;

function TdxServerModeSummaryItem.IsCurrency(AVarType: TVarType): Boolean;
begin
  Result := inherited IsCurrency(AVarType);
  if (DataField <> nil) and
    (DataController.Fields[DataField.Index].Field <> nil) then
    Result := DataController.Fields[DataField.Index].IsCurrency;
end;

function TdxServerModeSummaryItem.IsDataBinded: Boolean;
begin
  Result := inherited IsDataBinded and (DataField <> nil) and
    (ServerModeDataField.Field <> nil);
end;

function TdxServerModeSummaryItem.ServerModeDataField: TdxServerModeDataField;
begin
  Result := DataField as TdxServerModeDataField;
end;

procedure TdxServerModeSummaryItem.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    DataController.UpdateInternalField(Value, FDataField);
    if not AllowKind(Kind) then
      Kind := skNone;
  end;
end;

{ TdxServerModeDataSummary }

procedure TdxServerModeDataSummary.CalculateGroupSummary;
begin
  UpdateGroupSummary;
end;

procedure TdxServerModeDataSummary.UpdateGroupSummary(AParentGroup: TdxServerModeDataGroupInfo = nil);
var
  I, ALevel, ALevelGroupedIndex: Integer;
  ASummaryItems: TcxDataSummaryItems;
  ACountValues: TcxDataSummaryCountValues;
  ASummaryValues: TcxDataSummaryValues;
  SV: Variant;
  AValues: TdxServerModeSummaryList;
  ADataGroupInfo: TdxServerModeDataGroupInfo;
  ADataGroups: TdxServerModeDataGroups;
begin
  ADataGroups := DataController.DataControllerInfo.DataGroups;
  for I := 0 to ADataGroups.Count - 1 do
  begin
    ADataGroupInfo := ADataGroups[I];
    if ((AParentGroup <> nil) and (ADataGroupInfo.ParentGroup <> AParentGroup)) or (ADataGroupInfo.ServerGroupInfo = nil) then
      Continue;
    ALevel := ADataGroupInfo.Level;
    for ALevelGroupedIndex := 0 to ADataGroups.GetLevelGroupedFieldCount(ALevel) - 1 do
    begin
      ASummaryItems := GetGroupSummaryItems(ALevel, ALevelGroupedIndex);
      if ASummaryItems = nil then
        Continue;
      AValues := ADataGroupInfo.ServerGroupInfo.Summary;
      if AValues = nil then
        Continue;
      BeginCalculateSummary(ASummaryItems, ACountValues, ASummaryValues);
      PopulateSummary(ASummaryItems, AValues, ACountValues, ASummaryValues);
      SV := ADataGroupInfo.SummaryInfos[ALevelGroupedIndex].Values;
      EndCalculateSummary(ASummaryItems, ACountValues, ASummaryValues, SV);
      ADataGroupInfo.SummaryInfos[ALevelGroupedIndex].Values := SV;
    end;
  end;
end;

procedure TdxServerModeDataSummary.CalculateSummary(
  ASummaryItems: TcxDataSummaryItems; ABeginIndex, AEndIndex: Integer;
  var ACountValues: TcxDataSummaryCountValues;
  var ASummaryValues: TcxDataSummaryValues);
begin
  if (DataController.DataSource <> nil) and DataController.DataSource.Active and
    DataController.DataSource.IsConsistentCache then
    PopulateSummary(ASummaryItems, DataController.DataSource.Cache.GetTotalSummary, ACountValues, ASummaryValues);
end;

function TdxServerModeDataSummary.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

procedure TdxServerModeDataSummary.PopulateSummary(ASummaryItems: TcxDataSummaryItems;
  AValues: TdxServerModeSummaryList; var ACountValues: TcxDataSummaryCountValues;
  var ASummaryValues: TcxDataSummaryValues);
var
  I: Integer;
  AValueIndex: Integer;
begin
  AValueIndex := 0;
  for I := 0 to ASummaryItems.Count - 1 do
  begin
    if not TdxDataSummaryItemAccess(ASummaryItems[I]).IsDataBinded then
      Continue;
    if ASummaryItems[I].Kind = skNone then
    else if ASummaryItems[I].Kind = skCount then
      ACountValues[I] := AValues[AValueIndex]
    else
      ASummaryValues[I] := AValues[AValueIndex];
    Inc(AValueIndex);
  end;
end;

{ TdxCustomServerModeDataProvider }

constructor TdxCustomServerModeDataProvider.Create(
  ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
end;

destructor TdxCustomServerModeDataProvider.Destroy;
begin
  inherited Destroy;
end;

procedure TdxCustomServerModeDataProvider.Append;
begin
  AddRecord(True);
end;

function TdxCustomServerModeDataProvider.CanDelete: Boolean;
begin
  Result := inherited CanDelete and CanModify;
end;

function TdxCustomServerModeDataProvider.CanInitEditing(
  ARecordIndex: Integer): Boolean;
begin
  Result := True;
  // bug in Delphi 7 with update pack 1
  FInCanInitEditing := True;
  try
    if DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused and not IsInserting then
      Insert
    else
      Edit;
    SetChanging;
    // Update Navigator
    if DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused then
      DataController.Change([dccUpdateRecord]);
  finally
    FInCanInitEditing := False;
  end;
end;

function TdxCustomServerModeDataProvider.CanModify: Boolean;

  function CanGetGroupValue: Boolean;
  var
    AGroupCount: Integer;
  begin
    AGroupCount := DataController.DataSource.GroupCount;
    Result := (AGroupCount = 0) or (AGroupCount > 0) and (DataController.RecordCount > 0);
  end;

begin
  Result := DataController.Active and DataController.DataSource.CanModify and CanGetGroupValue;
end;

procedure TdxCustomServerModeDataProvider.DeleteRecord(ARecordIndex: Integer);
var
  ACriteria: TdxServerModeCriteria;
begin
  ACriteria := DataSource.GetFetchRowByIndexCondition(ARecordIndex);
  try
    DeleteRecords(ACriteria);
  finally
    ACriteria.Free;
  end;
end;

procedure TdxCustomServerModeDataProvider.DeleteRecords(ACriteria: TdxServerModeCriteria);
begin
  if not IsInserting then
  begin
    DataController.LockStateInfo;
    try
      if not FInserting then
        DataSource.Delete(ACriteria);
      DataController.DataControllerInfo.FocusingInfo.Clear;
      DataChanged(dcTotal, -1, -1);
    finally
      DataController.UnlockStateInfo;
    end;
  end;
end;

procedure TdxCustomServerModeDataProvider.DeleteSelection;
var
  I, ARowIndex: Integer;
  ARowInfo: TcxRowInfo;
  ACriteria: TdxServerModeCriteria;
  AKeys: TdxServerModeKeyList;
begin
  ACriteria := DataSource.CreateCriteria;
  try
    AKeys := TdxServerModeKeyList.Create;
    try
      for I := 0 to DataController.GetSelectedCount - 1 do
      begin
        ARowIndex := DataController.GetSelectedRowIndex(I);
        ARowInfo := DataController.GetRowInfo(ARowIndex);
        if ARowInfo.Level < DataController.Groups.LevelCount then // It's Group Row
          ACriteria.OrCriteria(DataController.Groups.GetCriteria(ARowIndex), True)
        else
          AKeys.Add(DataSource.Cache.GetRowKey(ARowInfo.RecordIndex));
      end;
      if AKeys.Count > 0 then
        ACriteria.OrCriteria(DataSource.GetFetchRowsByKeysCondition(AKeys), True);
      DeleteRecords(ACriteria);
    finally
      AKeys.Free;
    end;
  finally
    ACriteria.Free;
  end;
end;

procedure TdxCustomServerModeDataProvider.DoInsertingRecord(AIsAppending: Boolean);
begin
  inherited DoInsertingRecord(AIsAppending);
  DataController.DataControllerInfo.Refresh;
end;

procedure TdxCustomServerModeDataProvider.Edit;
begin
  if IsUnboundColumnMode then
    inherited Edit
  else
    if Assigned(DataSource) and not IsEditing then
    begin
      if VarIsEmpty(EditingRow) then
        CreateEditingRow;
      EditingRecord;
    end;
end;

procedure TdxCustomServerModeDataProvider.Insert;
begin
  AddRecord(False);
end;

procedure TdxCustomServerModeDataProvider.RecordChanged(Field: TField);
begin
  if IsEditing then
    DataController.UpdateEditingRecord;
  ResetChanging;
end;

procedure TdxCustomServerModeDataProvider.AssignItemValue(ARecordIndex: Integer;
  AField: TcxCustomDataField; const AValue: Variant);
var
  ADataField: TdxServerModeDataField absolute AField;
begin
  if ADataField.Field <> nil then
    SetEditingData(ARecordIndex, ADataField.FieldIndex, AValue)
  else
    inherited AssignItemValue(ARecordIndex, AField, AValue);
end;

function TdxCustomServerModeDataProvider.GetCustomDataSource: TdxServerModeProviderDataSource;
begin
  Result := TdxServerModeProviderDataSource(inherited CustomDataSource);
end;

function TdxCustomServerModeDataProvider.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

function TdxCustomServerModeDataProvider.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := DataController.DataSource;
end;

procedure TdxCustomServerModeDataProvider.RestorePos;
begin
  inherited;
end;

function TdxCustomServerModeDataProvider.IsActive: Boolean;
begin
  Result := (DataSource <> nil) and DataSource.Active;
end;

procedure TdxCustomServerModeDataProvider.Post(AForcePost: Boolean);
var
  ALink: TcxDataListenerLink;
begin
  if IsActive then
  begin
    ALink := TdxServerModeDataController.AddListenerLink(DataController);
    try
      DataController.LockStateInfo(True);
      try
        UpdateData;
        if AForcePost then
          InternalPost
        else
          DataController.CheckBrowseMode;
      finally
        DataController.UnlockStateInfo(True);
      end;
    finally
      if ALink.Ref <> nil then
        InsertedRecordIndex := -1;
      TdxServerModeDataController.RemoveListenerLink(ALink);
    end;
  end;
end;

procedure TdxCustomServerModeDataProvider.PostEditingData;
begin
  UpdateData;
end;

procedure TdxCustomServerModeDataProvider.ResetEditing;
begin
  inherited ResetEditing;
  FIsAppending := False;
  ClearEditingRow;
end;

function TdxCustomServerModeDataProvider.SetEditingData(ARecordIndex, AFieldIndex: Integer;
  const AValue: Variant): Boolean;
begin
  Result := (IsEditing or IsInserting) and (EditingRecordIndex = ARecordIndex);
  if Result then
    FEditingRow[AFieldIndex] := AValue;
end;

procedure TdxCustomServerModeDataProvider.UpdateData;
begin
  DoUpdateData;
  RecordChanged(nil);
  ResetChanging;
end;

function TdxCustomServerModeDataProvider.GetEditingRowKey: Variant;
begin
  Result := Null;
end;

function TdxCustomServerModeDataProvider.GetRowIndexByKey(AKey: Variant): Integer;
begin
  if DataSource = nil then
    Result := -1
  else
    if IsInserting and VarEquals(AKey, GetEditingRowKey) then
      Result := EditingRecordIndex
    else
      Result := DataSource.Cache.GetRowIndexByKey(AKey);
end;

function TdxCustomServerModeDataProvider.GetRowKey(ARecordIndex: Integer): Variant;
begin
  if IsInserting and (ARecordIndex = EditingRecordIndex) then
    Result := GetEditingRowKey
  else
    Result := DataSource.Cache.GetRowKey(ARecordIndex);
end;

function TdxCustomServerModeDataProvider.GetServerRowValue(ARecordIndex, AFieldIndex: Integer): Variant;
begin
  if (ARecordIndex = EditingRecordIndex) and (IsInserting or
      (IsEditing and not VarIsEmpty(EditingRow[AFieldIndex]))) then
    Result := EditingRow[AFieldIndex]
  else
    if not DataSource.IsConsistentCache then
      Result := Null
    else
    begin
      if IsInserting and (ARecordIndex > EditingRecordIndex) then
        Dec(ARecordIndex);
      try
        Result := DataSource.Rows[ARecordIndex];
        if VarIsArray(Result) then
          Result := Result[AFieldIndex];
      except
        Result := Null;
      end;
    end;
end;

procedure TdxCustomServerModeDataProvider.InternalPost;
var
  ARecordIndex: Integer;
  AKey: Variant;
begin
  if IsModified then
  begin
    DataController.DoBeforePost;
    DataController.LockStateInfo;
    try
      if IsInserting then
        ARecordIndex := MaxInt
      else
        ARecordIndex := EditingRecordIndex;
      AKey := DataSource.PostEditingData(ARecordIndex, EditingRow);
      if not VarIsEmpty(AKey) then
        DataController.DataControllerInfo.FocusingInfo.SetRowKey(AKey);
      DataController.DataControllerInfo.FChanges := DataController.DataControllerInfo.FChanges +
        [dcicFocusedRow, dcicLoad, dcicGrouping, dcicSummary];
      ResetEditing;
    finally
      DataController.UnlockStateInfo;
    end;
    DataController.DoAfterPost;
  end;
end;

function TdxCustomServerModeDataProvider.SetEditValue(ARecordIndex: Integer;
  AField: TcxCustomDataField; const AValue: Variant;
  AEditValueSource: TcxDataEditValueSource): Boolean;
var
  AServerField: TdxServerModeDataField absolute AField;
begin
  Result := AServerField.SetEditingData(ARecordIndex, AValue);
  if Result then
    SetModified;
end;

procedure TdxCustomServerModeDataProvider.SavePos;
begin
  inherited;
end;

procedure TdxCustomServerModeDataProvider.AddRecord(AIsAppending: Boolean);
begin
  if Assigned(DataSource) then
  begin
    DataController.CheckBrowseMode;
    DataController.LockStateInfo(False);
    FInInserting := True;
    FIsAppending := AIsAppending;
    try
      CreateEditingRow;
      InsertingRecord(AIsAppending);
    finally
      FInInserting := False;
      DataController.Change([dccFocus, dccUpdateRecord]);
      DataController.UnlockStateInfo(False);
    end;
  end;
end;

procedure TdxCustomServerModeDataProvider.ClearEditingRow;
begin
  VarClear(FEditingRow);
end;

procedure TdxCustomServerModeDataProvider.CreateEditingRow;
begin
  ClearEditingRow;
  FEditingRow := DataSource.CreateEmptyRow;
end;

procedure TdxCustomServerModeDataProvider.SetCustomDataSource(
  Value: TdxServerModeProviderDataSource);
begin
  inherited CustomDataSource := Value;
end;

{ TdxServerModeProviderDataSource }

function TdxServerModeProviderDataSource.GetRecordHandleByIndex(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(ARecordIndex);
end;

function TdxServerModeProviderDataSource.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

function TdxServerModeProviderDataSource.GetProvider: TdxCustomServerModeDataProvider;
begin
  Result := TdxCustomServerModeDataProvider(inherited Provider);
end;

{ TdxServerModeDataFocusingInfo }

constructor TdxServerModeDataFocusingInfo.Create(
  ADataControllerInfo: TcxCustomDataControllerInfo);
begin
  inherited Create(ADataControllerInfo);
  FKey := Unassigned;
end;

procedure TdxServerModeDataFocusingInfo.Assign(
  AFocusingInfo: TcxDataFocusingInfo);
begin
  if AFocusingInfo is TdxServerModeDataFocusingInfo then
  begin
    FKey := TdxServerModeDataFocusingInfo(AFocusingInfo).Key;
    FIsGroupKey := TdxServerModeDataFocusingInfo(AFocusingInfo).IsGroupKey;
  end;
  inherited Assign(AFocusingInfo);
end;

procedure TdxServerModeDataFocusingInfo.Clear;
begin
  inherited Clear;
  FIsGroupKey := False;
  VarClear(FKey);
end;

function TdxServerModeDataFocusingInfo.GetDataControllerInfo: TdxServerModeDataControllerInfo;
begin
  Result := TdxServerModeDataControllerInfo(inherited DataControllerInfo);
end;

function TdxServerModeDataFocusingInfo.IsEqual(
  AFocusingInfo: TcxDataFocusingInfo): Boolean;
var
  AServerModeFocusingInfo: TdxServerModeDataFocusingInfo absolute AFocusingInfo;
begin
  Result := (Level = AServerModeFocusingInfo.Level) and (IsGroupKey = AServerModeFocusingInfo.IsGroupKey) and
    VarEquals(Key, AServerModeFocusingInfo.Key);
  if Result and (dcicSorting in DataControllerInfo.Changes) and (RowIndex <> AServerModeFocusingInfo.RowIndex) then
    Result := False;
end;

function TdxServerModeDataFocusingInfo.IsNeedUpdate: Boolean;
begin
  Result := not IsUndefined and inherited IsNeedUpdate;
end;

function TdxServerModeDataFocusingInfo.IsUndefined: Boolean;
begin
  Result := (RowIndex < 0) or VarIsEmpty(FKey);
end;

procedure TdxServerModeDataFocusingInfo.SavePos;
var
  ARowInfo: TcxRowInfo;
begin
  if RowIndex <> -1 then
  begin
    ARowInfo := DataControllerInfo.GetRowInfo(RowIndex);
    RecordIndex := ARowInfo.RecordIndex;
    Level := ARowInfo.Level;
    FIsGroupKey := Level < DataControllerInfo.GroupingFieldList.Count;
    CalculateKey(ARowInfo.GroupIndex);
  end
  else
    ResetPos;
end;

procedure TdxServerModeDataFocusingInfo.SetRowKey(const AKey: Variant);
begin
  if not IsGroupKey {and VarIsEmpty(Key)} then
    FKey := AKey;
end;

procedure TdxServerModeDataFocusingInfo.CalculateKey(ADataGroupIndex: Integer);
var
  ALevel: Integer;
  AGroupInfo: TdxServerModeDataGroupInfo;
begin
  if IsGroupKey then
  begin
    if ADataGroupIndex <> -1 then // Group
    begin
      ALevel := Level;
      FKey := VarArrayCreate([0, ALevel], varVariant);
      AGroupInfo := DataControllerInfo.DataGroups[ADataGroupIndex];
      repeat
        FKey[ALevel] := AGroupInfo.GroupValue;
        Dec(ALevel);
        AGroupInfo := AGroupInfo.ParentGroup;
      until AGroupInfo = nil;
    end
    else
      FKey := Unassigned;
  end
  else
    FKey := DataControllerInfo.DataController.Provider.GetRowKey(RecordIndex);
end;

{ TdxServerModeDataGroupInfo }

constructor TdxServerModeDataGroupInfo.Create(AOwner: TcxDataGroups; AParentGroup: TdxServerModeDataGroupInfo;
  AServerGroupInfo: TdxServerModeGroupInfo);
begin
  inherited Create(AOwner);
  RowIndex := -1;
  SetServerGroupInfo(AServerGroupInfo);
  ParentGroup := AParentGroup;
  Level := AServerGroupInfo.Level;
  FirstRecordListIndex := AServerGroupInfo.TopDataRowIndex;
  LastRecordListIndex := FirstRecordListIndex + AServerGroupInfo.DataRowCount - 1;
end;


destructor TdxServerModeDataGroupInfo.Destroy;
begin
  SetServerGroupInfo(nil);
  inherited Destroy;
end;

procedure TdxServerModeDataGroupInfo.AdjustByInsertingRecord(ARecordIndex: Integer;
  AAppending: Boolean);
begin
  if ARecordIndex < FirstRecordListIndex then
  begin
    Inc(FirstRecordListIndex);
    Inc(LastRecordListIndex);
  end
  else
    if ARecordIndex <= LastRecordListIndex then
      Inc(LastRecordListIndex)
    else
      if AAppending and (ARecordIndex = LastRecordListIndex + 1) then
        Inc(LastRecordListIndex);
end;

function TdxServerModeDataGroupInfo.GetDisplayText: Variant;
begin
  Result := VarToStr(GroupValue);
end;

function TdxServerModeDataGroupInfo.GetGroupValue: Variant;
begin
  if ServerGroupInfo <> nil then
    Result := ServerGroupInfo.GroupValue
  else
    Result := Null;
end;

function TdxServerModeDataGroupInfo.IsEqual(const AValue: Variant;
  AHash: Cardinal): Boolean;
begin
  Result := (ServerGroupInfo <> nil) and ServerGroupInfo.IsEqual(AValue, AHash);
end;

procedure TdxServerModeDataGroupInfo.ServerGroupInfoDestroyHandler(Sender: TObject);
begin
  FServerGroupInfo := nil;
end;

procedure TdxServerModeDataGroupInfo.SetServerGroupInfo(const Value: TdxServerModeGroupInfo);
begin
  if FServerGroupInfo <> Value then
  begin
    if FServerGroupInfo <> nil then
      FServerGroupInfo.OnDestroy := nil;
    FServerGroupInfo := Value;
    if FServerGroupInfo <> nil then
      FServerGroupInfo.OnDestroy := ServerGroupInfoDestroyHandler;
  end;
end;

{ TcxServerModeDataGroups }

procedure TdxServerModeDataGroups.CreateGroups;
var
  AList: TdxServerModeGroupInfoList;
begin
  AList := DataControllerInfo.Source.GetGroupInfoList(nil);
  CreateGroup(AList, nil);
  Rebuild;
end;

procedure TdxServerModeDataGroups.ChangeExpanding(ARowIndex: Integer; AExpanded,
  ARecursive: Boolean);
var
  AItem: TcxDataGroupInfo;
  I, AItemIndex: Integer;
begin
  AItemIndex := Find(ARowIndex, AItem);
  if (AItemIndex <> -1) and Assigned(AItem) then
  begin
    ExpandItem(AItem as TdxServerModeDataGroupInfo, AExpanded);
    if ARecursive and (AItem.Level < (LevelCount - 1)) then
    begin
      for I := AItemIndex + 1 to Count - 1 do
        if Items[I].Level > AItem.Level then
          ExpandItem(Items[I], AExpanded)
        else
          Break;
    end;
    Rebuild;
  end;
end;

procedure TdxServerModeDataGroups.FullExpanding(AExpanded: Boolean);
var
  I: Integer;
begin
  FIsFullExpanding := AExpanded;
  try
    for I := 0 to FItems.Count - 1 do
      TcxDataGroupInfo(FItems[I]).Free;
    FItems.Clear;
    CreateGroups;
    DataControllerInfo.CalculateSummary;
  finally
    FIsFullExpanding := False;
  end;
end;

procedure TdxServerModeDataGroups.CreateGroup(
  AList: TdxServerModeGroupInfoList;
  AParentGroup: TdxServerModeDataGroupInfo);
var
  ALastLevel: Boolean;
  AGroup: TdxServerModeDataGroupInfo;
  AGroupInfo: TdxServerModeGroupInfo;
  I, AParentIndex, ALastChildGroupIndex, AGroupCountBeforeRequestChildren: Integer;
begin
  if AParentGroup = nil then
    ALastLevel := LevelCount = 0
  else
    ALastLevel := LevelCount = AParentGroup.Level;
  AParentIndex := FItems.IndexOf(AParentGroup, ldFromEnd);
  ALastChildGroupIndex := AParentIndex;
  for I := 0 to AList.Count - 1 do
  begin
    AGroupInfo := AList[I];
    AGroup := TdxServerModeDataGroupInfo.Create(Self, AParentGroup, AGroupInfo);
    AGroup.Expanded := DataControllerInfo.IsAlwaysExpanded or FIsFullExpanding;
    AGroup.ChildrenIsReady := ALastLevel;
    with DataControllerInfo.DataController.Provider do
    begin
      if IsInserting then
        AGroup.AdjustByInsertingRecord(EditingRecordIndex, IsAppending);
    end;
    if AParentIndex < 0 then
      FItems.Add(AGroup)
    else
      FItems.Insert(ALastChildGroupIndex + 1, AGroup);
    Inc(ALastChildGroupIndex);
    if not ALastLevel and AGroup.Expanded then
    begin
      AGroupCountBeforeRequestChildren := FItems.Count;
      RequestChildren(AGroup);
      Inc(ALastChildGroupIndex, FItems.Count - AGroupCountBeforeRequestChildren);
    end;
  end;
end;

procedure TdxServerModeDataGroups.ExpandItem(AItem: TdxServerModeDataGroupInfo;
  AExpanded: Boolean);
begin
  AItem.Expanded := AExpanded;
  if AExpanded and not AItem.ChildrenIsReady then
    RequestChildren(AItem);
end;

function TdxServerModeDataGroups.GetChildIndex(AParentIndex, AIndex: Integer): Integer;
var
  I, AParentLevel, AGroupLevel, ACurrentChildGroupIndex: Integer;
begin
  if AParentIndex = -1 then
    Result := GetFirstLevelIndex(AIndex)
  else
  begin
    AParentLevel := Items[AParentIndex].Level;
    Result := -1;
    ACurrentChildGroupIndex := -1;
    for I := AParentIndex + 1 to Count - 1 do
    begin
      AGroupLevel := Items[I].Level;
      if AParentLevel < AGroupLevel then
      begin
        if AParentLevel = AGroupLevel - 1 then
        begin
          Inc(ACurrentChildGroupIndex);
          if ACurrentChildGroupIndex = AIndex then
          begin
            Result := I;
            Break;
          end;
        end;
      end
      else
        Break;
    end;
  end;
end;

function TdxServerModeDataGroups.GetChildCount(AIndex: Integer): Integer;
var
  AServerGroupInfo: TdxServerModeGroupInfo;
begin
  if AIndex = -1 then
    Result := GetFirstLevelCount
  else
  begin
    if Items[AIndex].Level + 1 = LevelCount then
      Result := Items[AIndex].RecordCount
    else
    begin
      if not Items[AIndex].ChildrenIsReady then
      begin
        RequestChildren(Items[AIndex]);
        Rebuild;
      end;
      AServerGroupInfo := Items[AIndex].ServerGroupInfo;
      if AServerGroupInfo = nil then
        Result := 0
      else
        Result := AServerGroupInfo.ChildrenGroups.Count;
    end;
  end;
end;

function TdxServerModeDataGroups.GetDataControllerInfo: TdxServerModeDataControllerInfo;
begin
  Result := TdxServerModeDataControllerInfo(inherited DataControllerInfo);
end;

function TdxServerModeDataGroups.GetItem(Index: Integer): TdxServerModeDataGroupInfo;
begin
  Result := TdxServerModeDataGroupInfo(inherited Items[Index]);
end;

procedure TdxServerModeDataGroups.Rebuild;
var
  AIndex, ARowIndex: Integer;
  AGroup, ASubGroup: TcxDataGroupInfo;
begin
  AIndex := 0;
  ARowIndex := 0;
  while AIndex < Count do
  begin
    AGroup := Items[AIndex];
    AGroup.RowIndex := ARowIndex;
    if not AGroup.Expanded then
    begin
      Inc(AIndex);
      while AIndex < Count do
      begin
        ASubGroup := Items[AIndex];
        if ASubGroup.Level > AGroup.Level then
          ASubGroup.RowIndex := ARowIndex
        else
        begin
          Dec(AIndex);
          Break;
        end;
        Inc(AIndex)
      end;
    end;
    if AGroup.Expanded and (AGroup.Level = (LevelCount - 1)) then
      Inc(ARowIndex, AGroup.RecordCount);
    Inc(ARowIndex);
    Inc(AIndex)
  end;
end;

function TdxServerModeDataGroups.FindByServerGroupInfo(const AServerGroupInfo: TdxServerModeGroupInfo): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ServerGroupInfo = AServerGroupInfo then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TdxServerModeDataGroups.FindInnerGroup(ARecordListIndex: Integer): Integer;

  function FindMostInnerGroup(ARecordListIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Count - 1 downto 0 do
      if Items[I].Contains(ARecordListIndex) then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AIndex: Integer;
begin
  Result := FindMostInnerGroup(ARecordListIndex);
  while (Result >= 0) and (Items[Result].Level < (LevelCount - 1)) do
  begin
    ExpandItem(Items[Result], True);
    Rebuild;
    AIndex := FindMostInnerGroup(ARecordListIndex);
    if AIndex >= 0 then
      Result := AIndex
    else
      Break;
  end;
end;

function TdxServerModeDataGroups.GetFirstDataRecordListIndex(AInfo: TcxDataGroupInfo): Integer;
begin
  Result := AInfo.FirstRecordListIndex;
end;

function TdxServerModeDataGroups.GetLastDataRecordListIndex(AInfo: TcxDataGroupInfo): Integer;
begin
  Result := AInfo.LastRecordListIndex;
end;

function TdxServerModeDataGroups.GetRowIndexByGroupValue(
  const AGroupValue: Variant): Integer;
var
  I, AGroupLevel, AStartIndex: Integer;
  ALevelGroupValue: Variant;
  AGroupValueHash: Cardinal;
begin
  Result := -1;
  AStartIndex := 0;
  for AGroupLevel := VarArrayLowBound(AGroupValue, 1) to VarArrayHighBound(AGroupValue, 1) do
  begin
    ALevelGroupValue := AGroupValue[AGroupLevel];
    AGroupValueHash := GetVariantHash(ALevelGroupValue);
    for I := AStartIndex to Count - 1 do
    begin
      with Items[I] do
        if (Level = AGroupLevel) and IsEqual(ALevelGroupValue, AGroupValueHash) then
        begin
          Result := RowIndex;
          AStartIndex := I + 1;
          Break;
        end;
    end;
    if Result < 0 then
      Break;
  end;
end;

function TdxServerModeDataGroups.MakeVisible(AIndex: Integer; AExpand: Boolean): Boolean;
var
  I, ALevel: Integer;
  AItem: TdxServerModeDataGroupInfo;
begin
  Result := False;
  I := AIndex;
  repeat
    AItem := Items[I];
    if AExpand then
    begin
      if not AItem.Expanded then
      begin
        ExpandItem(AItem, True);
        Result := True;
      end;
    end;
    AExpand := True;
    ALevel := AItem.Level;
    if ALevel = 0 then
      Break
    else
      while I > 0 do
      begin
        Dec(I);
        if Items[I].Level < ALevel then
          Break;
      end;
  until False;
  if Result then
    Rebuild;
end;

procedure TdxServerModeDataGroups.RequestChildren(AParentGroup: TdxServerModeDataGroupInfo);
var
  AList: TdxServerModeGroupInfoList;
begin
  if AParentGroup.ChildrenIsReady then
    Exit;
  AList := DataControllerInfo.Source.GetGroupInfoList(AParentGroup.ServerGroupInfo);
  if AList.Empty then
    Exit;
  CreateGroup(AList, AParentGroup);
  UpdateGroupSummary(AParentGroup);
  AParentGroup.ChildrenIsReady := True;
end;

procedure TdxServerModeDataGroups.UpdateGroupSummary(AParentGroup: TdxServerModeDataGroupInfo);
begin
  DataControllerInfo.DataController.Summary.UpdateGroupSummary(AParentGroup);
end;

function TdxServerModeDataGroups.GetTopGroupInfo: TdxServerModeGroupInfo;
begin
  Result := DataControllerInfo.DataController.DataSource.Cache.TopGroupInfo;
end;

{ TdxServerModeDataSelection }

function TdxServerModeDataSelection.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TdxServerModeDataControllerInfo }

constructor TdxServerModeDataControllerInfo.Create(ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FRecoveryFocusingInfo := TdxServerModeDataFocusingInfo.Create(Self);
end;

destructor TdxServerModeDataControllerInfo.Destroy;
begin
  FreeAndNil(FRecoveryFocusingInfo);
  inherited Destroy;
end;

procedure TdxServerModeDataControllerInfo.ConvertFilterItems(ADestination: TdxServerModeCriteria; ASource: TcxFilterCriteria);

  procedure ConvertItems(ADestinationList: TdxServerModeCriteriaItemList; ASourceList: TcxFilterCriteriaItemList);
  var
    I: Integer;
    AField: TdxServerModeDataField;
    AItem: TcxDataFilterCriteriaItem;
    AValue: Variant;
  begin
    ADestinationList.BoolOperatorKind := ASourceList.BoolOperatorKind;
    for I := 0 to ASourceList.Count - 1 do
      if ASourceList[I].IsItemList then
      begin
        ConvertItems(ADestinationList.AddItemList(TcxFilterCriteriaItemList(ASourceList[I]).BoolOperatorKind) as TdxServerModeCriteriaItemList,
          TcxFilterCriteriaItemList(ASourceList[I]));
      end
      else
      begin
        AItem := TcxDataFilterCriteriaItem(ASourceList[I]);
        AValue := AItem.Value;
        AField := TdxServerModeDataField(AItem.Field);
        if AField.Field <> nil then
          ADestination.AddItem(ADestinationList, DataSource.FieldDescriptors.ItemByField(AField.Field),
            AItem.OperatorKind, AValue);
      end;
  end;

begin
  ConvertItems(ADestination.Root, ASource.Root);
end;

procedure TdxServerModeDataControllerInfo.ConvertFindFilter(ADestination: TdxServerModeFindFilterCriteria;
  ASource: TdxServerModeDataFindFilterCriteria);
begin
  ConvertFilterItems(ADestination.Filter, ASource.Filter);
  ADestination.SetEscapedFlagForFilterItems;
end;

function TdxServerModeDataControllerInfo.CreateCriteria: TdxServerModeCriteria;
begin
  Result := DataSource.CreateCriteria;
  if DataController.Filter.IsFiltering then
    ConvertFilterItems(Result, DataController.Filter);
end;

function TdxServerModeDataControllerInfo.CreateFindFilterCriteria: TdxServerModeFindFilterCriteria;
begin
  Result := DataSource.CreateFindFilterCriteria;
  if DataController.FindFilterCriteria.IsFiltering then
    ConvertFindFilter(Result, DataController.FindFilterCriteria);
end;

function TdxServerModeDataControllerInfo.CreateSortInfo: TdxServerModeSortInfoDescriptors;
var
  I: Integer;
  AItem: TdxServerModeDescriptor;
begin
  GetTotalSortingFields;
  Result := DataController.DataSource.CreateSortInfo;
  for I := 0 to TotalSortingFieldList.Count - 1 do
  begin
    AItem := Result.Add(
      TdxServerModeDataField(TotalSortingFieldList.Items[I].Field).FieldName,
      TotalSortingFieldList.Items[I].SortOrder = soDescending);
    DataController.InitializeDescriptor(TotalSortingFieldList.Items[I].Field, AItem);
  end;
end;

function TdxServerModeDataControllerInfo.CreateSummariesByItems(AItems: TcxDataSummaryItems;
  ALevel: Integer): TdxServerModeSummaryDescriptors;
begin
  Result := DataController.DataSource.CreateSummaries;
  PopulateSummariesByItems(Result, AItems, ALevel);
end;

function TdxServerModeDataControllerInfo.CreateTotalSummaries: TdxServerModeSummaryDescriptors;
begin
  Result := CreateSummariesByItems(DataController.Summary.FooterSummaryItems, -1);
end;

function TdxServerModeDataControllerInfo.CreateGroupSummaries: TdxServerModeSummaryDescriptors;
var
  ALevel: Integer;
  ASummaryGroups: TcxDataSummaryGroups;
  ASummaryGroup: TcxDataSummaryGroup;
begin
  Result := CreateSummariesByItems(DataController.Summary.DefaultGroupSummaryItems, -1);
  ASummaryGroups := DataController.Summary.SummaryGroups;
  for ALevel := 0 to DataController.DataControllerInfo.GroupingFieldList.Count - 1 do
  begin
    ASummaryGroup := ASummaryGroups.FindByItemLink(DataController.DataControllerInfo.GroupingFieldList[ALevel].Field.Item);
    if ASummaryGroup <> nil then
      PopulateSummariesByItems(Result, ASummaryGroup.SummaryItems, ALevel);
  end;
end;

function TdxServerModeDataControllerInfo.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

function TdxServerModeDataControllerInfo.GetDataGroups: TdxServerModeDataGroups;
begin
  Result := TdxServerModeDataGroups(inherited DataGroups);
end;

function TdxServerModeDataControllerInfo.GetSelection: TdxServerModeDataSelection;
begin
  Result := TdxServerModeDataSelection(inherited Selection);
end;

function TdxServerModeDataControllerInfo.GetSource: TdxServerModeCache;
begin
  Result := DataController.DataSource.Cache;
end;

function TdxServerModeDataControllerInfo.GetDataFocusingInfoClass: TcxDataFocusingInfoClass;
begin
  Result := TdxServerModeDataFocusingInfo;
end;

function TdxServerModeDataControllerInfo.GetDataGroupsClass: TcxDataGroupsClass;
begin
  Result := TdxServerModeDataGroups;
end;

function TdxServerModeDataControllerInfo.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := DataController.DataSource;
end;

function TdxServerModeDataControllerInfo.GetFocusingInfo: TdxServerModeDataFocusingInfo;
begin
  Result := TdxServerModeDataFocusingInfo(inherited FocusingInfo);
end;

procedure TdxServerModeDataControllerInfo.PopulateSummariesByItems(AList: TdxServerModeSummaryDescriptors;
  AItems: TcxDataSummaryItems; ALevel: Integer);
var
  I: Integer;
  ASummary: TdxServerModeSummaryItem;
  AItem: TdxServerModeSummaryDescriptor;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    ASummary := AItems[I] as TdxServerModeSummaryItem;
    if not ASummary.IsDataBinded then
      Continue;
    AItem := TdxServerModeSummaryDescriptor(AList.CreateLink(DataController.DataSource.FieldDescriptors.ItemByField(ASummary.ServerModeDataField.Field)));
    AItem.Kind := ASummary.Kind;
    AItem.Sorted := ASummary.Sorted;
    AItem.GroupLevel := ALevel;
  end;
end;

procedure TdxServerModeDataControllerInfo.CalculateSummary;
begin
  DataController.Summary.CalculateFooterSummary;
  DataController.Summary.CalculateGroupSummary;
end;

function TdxServerModeDataControllerInfo.CanCreateGroups: Boolean;
begin
  Result := IsDataSourceValid and DataSource.Active and (DataGroups.LevelCount > 0);
end;

procedure TdxServerModeDataControllerInfo.CheckExpanding;
var
  AExpandingInfoIndex: Integer;

  function IsEqualFields: Boolean;
  var
    I: Integer;
  begin
    Result := ExpandingInfo.FieldCount = DataGroups.GroupFieldInfoCount;
    if Result then
      for I := 0 to DataGroups.GroupFieldInfoCount - 1 do
        if ExpandingInfo.Fields[I] <> DataGroups.GroupFieldInfos[I].Field then
        begin
          Result := False;
          Break;
        end;
  end;

  function GroupNeedsRestoration(AExpandingInfo: TcxDataExpandingInfo): Boolean;
  begin
    Result := (eisExpanded in AExpandingInfo.State) and (eisExpanded in ExpandingInfo.SaveStates)
  end;

  procedure ExpandGroup(AParentGroupInfo: TdxServerModeGroupInfo;
    const AGroupValue: Variant);
  var
    AGroupIndex, AServerGroupIndex: Integer;
    AExpandingInfo: TcxDataExpandingInfo;
    AList: TdxServerModeGroupInfoList;
  begin
    Inc(AExpandingInfoIndex);
    AList := Source.GetGroupInfoList(AParentGroupInfo);
    if AList.Empty then
      Exit;
    AServerGroupIndex := AList.Find(AGroupValue);
    if AServerGroupIndex < 0 then
    begin
      while (AExpandingInfoIndex < ExpandingInfo.Count) and
            (ExpandingInfo[AExpandingInfoIndex].GroupIndex > AParentGroupInfo.Level) do
        Inc(AExpandingInfoIndex);
      Exit;
    end;
    AParentGroupInfo := AList[AServerGroupIndex];
    AGroupIndex := DataGroups.FindByServerGroupInfo(AParentGroupInfo);
    Assert(AGroupIndex >= 0, 'Inconsistent groups data');
    DataGroups.ExpandItem(DataGroups[AGroupIndex], True);
    DataGroups.Rebuild;
    while AExpandingInfoIndex < ExpandingInfo.Count do
    begin
      AExpandingInfo := ExpandingInfo[AExpandingInfoIndex];
      if GroupNeedsRestoration(AExpandingInfo) and (AExpandingInfo.GroupIndex > AParentGroupInfo.Level) then
        ExpandGroup(AParentGroupInfo, AExpandingInfo.Value)
      else
        Break;
    end;
  end;

  procedure ExpandGroups;
  var
    AExpandingInfo: TcxDataExpandingInfo;
  begin
    AExpandingInfoIndex := 0;
    while AExpandingInfoIndex < ExpandingInfo.Count do
    begin
      AExpandingInfo := ExpandingInfo[AExpandingInfoIndex];
      if GroupNeedsRestoration(AExpandingInfo) and (AExpandingInfo.GroupIndex = 0) then
        ExpandGroup(Source.TopGroupInfo, AExpandingInfo.Value)
      else
        Inc(AExpandingInfoIndex);
    end;
  end;

begin
  if IsGrouped and IsAlwaysExpanded then
    DataGroups.FullExpanding(True);
  if ExpandingInfo.SaveStates <> [] then
  begin
    if IsEqualFields then
    begin
      PrepareSelectionBeforeExpandGroups;
      if IsGrouped then
      try
        ExpandGroups;
      finally
        RebuildSelectionAfterExpandGroups;
      end;
    end
    else
    begin
      if eisSelected in ExpandingInfo.SaveStates then
        Selection.ClearAll;
    end;
  end;
end;

procedure TdxServerModeDataControllerInfo.CheckFocusing;
var
  ANewFocusedRowIndex: Integer;
begin
  ANewFocusedRowIndex := FindFocusedRow(False);
  if FocusingInfo.RowIndex <> ANewFocusedRowIndex then
    DoChangeFocusedRow(ANewFocusedRowIndex, True);
  if (dcicFocusedRow in Changes) and (PrevFocusingInfo.RowIndex >= GetRowCount) then
    PrevFocusingInfo.RowIndex := -1;
  if DataController.FilterChangedFlag then
    CheckFocusingAfterFilter;
end;

procedure TdxServerModeDataControllerInfo.CreateGroups;
begin
  DataGroups.CreateGroups;
end;

function TdxServerModeDataControllerInfo.FindDataGroup(ARecordListIndex: Integer): Integer;
begin
  Result := DataGroups.FindInnerGroup(ARecordListIndex);
end;

function TdxServerModeDataControllerInfo.FindFocusedGroup: Integer;
begin
  if not DataController.GroupingChangedFlag then
    Result := DataGroups.GetRowIndexByGroupValue(FocusingInfo.Key)
  else
    if DataController.RecordCount > 0 then
      Result := 0
    else
      Result := -1;
end;

function TdxServerModeDataControllerInfo.FindFocusedRow(ANearest: Boolean): Integer;

  function FindFocused(ARecordListIndex, AGroupIndex: Integer): Integer;
  begin
    if FocusingInfo.Level = -1 then
    begin
      if DataGroups[AGroupIndex].FirstRecordListIndex <> ARecordListIndex then // Seek Detail
        Result := LocateDetail(AGroupIndex, ARecordListIndex, True)
      else
        Result := LocateGroupByLevel(AGroupIndex, -1);
    end
    else
      Result := LocateDetail(AGroupIndex, ARecordListIndex, True);
  end;

  function FindParentGroup(AIndex, ALevel: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := AIndex downto 0 do
      if DataGroups[I].Level = ALevel then
      begin
        Result := I;
        Break;
      end;
  end;

  function FindNearest(ARecordListIndex, AGroupIndex: Integer): Integer;
  var
    I, ALevel: Integer;
    AItem: TcxDataGroupInfo;
  begin
    if AGroupIndex <> -1 then
    begin
      I := AGroupIndex;
      AItem := DataGroups.GetTopVisibleItem(DataGroups[AGroupIndex].RowIndex, I);
      ALevel := FocusingInfo.Level;
      if ALevel = DataGroups.LevelCount then // Detail
      begin
        if (AGroupIndex = I) and DataGroups[AGroupIndex].Expanded then
        begin
          Result := AItem.RowIndex;
          Inc(Result, ARecordListIndex - AItem.FirstRecordListIndex + 1);
          Exit;
        end
        else
          Dec(ALevel);
      end;
      if (ALevel < 0) and (AGroupIndex = I) then
      begin
        Result := AItem.RowIndex;
        Exit;
      end;
      AGroupIndex := FindParentGroup(AGroupIndex, ALevel);
      if AGroupIndex <> -1 then
        Result := DataGroups[AGroupIndex].RowIndex
      else
        Result := -1;
    end
    else
      Result := -1;
  end;

var
  ARecordListIndex, AGroupIndex: Integer;
begin
  Result := -1;
  if FocusingInfo.IsUndefined then
    Exit;
  if FocusingInfo.IsGroupKey then
    Result := FindFocusedGroup
  else
  begin
    ARecordListIndex := GetRecordListIndexByFocusingInfo;
    if ARecordListIndex <> -1 then
      if IsGrouped then
      begin
        AGroupIndex := FindDataGroup(ARecordListIndex); // LastLevel Group
        if ANearest or (AGroupIndex < 0) then
          Result := FindNearest(ARecordListIndex, AGroupIndex)
        else
          Result := FindFocused(ARecordListIndex, AGroupIndex);
      end
      else
        Result := ARecordListIndex;
  end;
end;

function TdxServerModeDataControllerInfo.GetInternalRecordCount: Integer;
begin
  Result := DataController.GetRecordCount;
end;

function TdxServerModeDataControllerInfo.GetRecordListIndexByFocusingInfo: Integer;
begin
  if FocusingInfo.IsUndefined then
    Result := -1
  else
    Result := DataController.Provider.GetRowIndexByKey(FocusingInfo.Key);
end;

function TdxServerModeDataControllerInfo.IsResetFocusingNeeded: Boolean;
begin
  Result := (DataController.GroupingChangedFlag and FocusingInfo.IsGroupKey) or
    DataController.IsResetFocusedRowNeeded;
end;

function TdxServerModeDataControllerInfo.LocateDetail(
  AGroupIndex, ARecordListIndex: Integer; AMakeVisible: Boolean): Integer;
begin
  Result := inherited LocateDetail(AGroupIndex, ARecordListIndex, AMakeVisible);
end;

function TdxServerModeDataControllerInfo.LocateGroupByLevel(
  AGroupIndex, ALevel: Integer): Integer;

  function FindParentGroup(AIndex: Integer): Integer;
  var
    I, ALevel: Integer;
  begin
    Result := -1;
    ALevel := DataGroups[AIndex].Level - 1;
    for I := AIndex downto 0 do
      if DataGroups[I].Level = ALevel then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AParentGroupIndex: Integer;
begin
  repeat
    if DataGroups[AGroupIndex].Level = ALevel then
      Break;
    AParentGroupIndex := FindParentGroup(AGroupIndex);
    if (AParentGroupIndex <> -1) and
       (DataGroups[AParentGroupIndex].FirstRecordListIndex = DataGroups[AGroupIndex].FirstRecordListIndex) then
      AGroupIndex := AParentGroupIndex
    else
      Break;
  until False;
  if DataGroups.MakeVisible(AGroupIndex, False) then
    ExpandingChanged;
  Result := DataGroups[AGroupIndex].RowIndex;
end;

procedure TdxServerModeDataControllerInfo.ResetFocusing;
begin
  if DataController.MultiSelect then
  begin
    Selection.Clear;
  end;
  inherited;
end;

procedure TdxServerModeDataControllerInfo.SaveExpanding(
  ASaveStates: TcxDataExpandingInfoStateSet);

  procedure AddGroup(ADataGroupInfo: TdxServerModeDataGroupInfo; AGroupIndex: Integer; AExpanded: Boolean);
  var
    ARecordIndex: Integer;
    AStateSet: TcxDataExpandingInfoStateSet;
  begin
    ARecordIndex := ADataGroupInfo.FirstRecordListIndex;
    if ARecordIndex < DataController.RecordCount then
    begin
      AStateSet := [];

      if AExpanded and ((Selection.Count > 0) or not IsAlwaysExpanded) then
        AStateSet := AStateSet + [eisExpanded];


      if AStateSet <> [] then
        ExpandingInfo.AddItem(ADataGroupInfo.Level, ADataGroupInfo.GroupValue, AStateSet);
    end;
  end;

  procedure AddGroups;
  var
    I: Integer;
    ADataGroupInfo: TdxServerModeDataGroupInfo;
  begin
    for I := 0 to DataGroups.Count - 1 do
    begin
      ADataGroupInfo := DataGroups[I];
      if ADataGroupInfo.Expanded then
        AddGroup(ADataGroupInfo, I, True);
    end;
  end;

var
  I: Integer;
begin
  if ExpandingInfo.SaveStates <> ASaveStates then
  begin
    if (ASaveStates <> []) and IsValidDataGroupInfo then
    begin
      ExpandingInfo.SaveStates := ASaveStates;
      ExpandingInfo.ClearFields;
      for I := 0 to DataGroups.GroupFieldInfoCount - 1 do
        ExpandingInfo.AddField(DataGroups.GroupFieldInfos[I].Field);
      if IsGrouped then
        AddGroups;
    end
    else
    begin
      ExpandingInfo.Clear;
      ExpandingInfo.SaveStates := ASaveStates;
    end;
  end;
end;

procedure TdxServerModeDataControllerInfo.SelectionChanged;
begin
  FChanges := FChanges + [dcicSelection];
end;

procedure TdxServerModeDataControllerInfo.DoFilter;
begin
  if DataController.RecordCount > 0 then
    DataController.FilterChangedFlag := True;
  Selection.InternalClear;
end;

procedure TdxServerModeDataControllerInfo.DoLoad;
begin
  inherited DoLoad;
  if not DataController.Active then
  begin
    ClearInfo;
    FocusingInfo.Clear;
  end;
end;

procedure TdxServerModeDataControllerInfo.DoSort;
begin
// do nothing
end;

procedure TdxServerModeDataControllerInfo.DoUpdate(ASummaryChanged: Boolean);
begin
  if IsUpdateDataSourceNeeded then
    UpdateDataSource;

  if dcicLoad in Changes then
  begin
    DoLoad;
    DoFilter;
  end;

  if dcicGrouping in Changes then
  begin
    DataController.NotifyControl(TcxGroupingChangingInfo.Create);
    DoGrouping;
  end;

  if ([dcicLoad, dcicSorting, dcicGrouping] * Changes <> []) then
    ResetFocusing;

  CheckInfo;

  if ASummaryChanged then
    CalculateSummary;
end;

function TdxServerModeDataControllerInfo.IsDataSourceValid: Boolean;
begin
  Result := not DataController.IsDestroying and (DataSource <> nil) and
    not DataSource.IsDestroying;
end;

function TdxServerModeDataControllerInfo.IsUpdateDataSourceNeeded: Boolean;
begin
  Result := IsDataSourceValid and
    ([dcicLoad, dcicGrouping, dcicSorting, dcicSummary, dcicView] * Changes <> []);
end;

procedure TdxServerModeDataControllerInfo.UpdateDataSource;
var
  ASortInfo: TdxServerModeSortInfoDescriptors;
  ASummaries, ATotalSummaries: TdxServerModeSummaryDescriptors;
  ACriteria: TdxServerModeCriteria;
  AFindFilterCriteria: TdxServerModeFindFilterCriteria;
begin
  ACriteria := CreateCriteria;
  try
    AFindFilterCriteria := CreateFindFilterCriteria;
    try
      ASortInfo := CreateSortInfo;
      try
        ATotalSummaries := CreateTotalSummaries;
        try
          ASummaries := CreateGroupSummaries;
          try
            DataController.LockDataChangedNotify;
            try
              DataController.DataSource.Apply(ACriteria, AFindFilterCriteria, ASortInfo, GroupingFieldList.Count, ASummaries, ATotalSummaries);
            finally
              DataController.UnlockDataChangedNotify;
            end;
          finally
            ASummaries.Free;
          end;
        finally
          ATotalSummaries.Free;
        end;
      finally
        ASortInfo.Free;
      end;
    finally
      AFindFilterCriteria.Free;
    end;
  finally
    ACriteria.Free;
  end;
end;

procedure TdxServerModeDataControllerInfo.ApplyRecoveryInfo;
begin
  FocusingInfo.Assign(FRecoveryFocusingInfo);
  DataController.Change([dccFocus]);
end;

procedure TdxServerModeDataControllerInfo.MakeRecoveryInfo;
begin
  FRecoveryFocusingInfo.Assign(PrevFocusingInfo);
end;

function TdxServerModeDataControllerInfo.GetRowInfo(ARowIndex: Integer): TcxRowInfo;
var
  AGroupsRowInfo: TcxGroupsRowInfo;
begin
  CheckRowIndex(ARowIndex);
  if IsGrouped then
    AGroupsRowInfo := DataGroups.RowInfo[ARowIndex]
  else
  begin
    AGroupsRowInfo.Level := 0;
    AGroupsRowInfo.Expanded := False;
    AGroupsRowInfo.RecordListIndex := ARowIndex;
    AGroupsRowInfo.Index := -1;
  end;
  Result.Expanded := AGroupsRowInfo.Expanded;
  Result.Level := AGroupsRowInfo.Level;
  Result.RecordIndex := AGroupsRowInfo.RecordListIndex;
  Result.DataRowIndex := AGroupsRowInfo.RecordListIndex;
  Result.RowIndex := ARowIndex;
  Result.GroupIndex := AGroupsRowInfo.Index;
end;

procedure TdxServerModeDataControllerInfo.RemoveField(AField: TcxCustomDataField);
begin
  FChanges := FChanges + [dcicView];
  inherited RemoveField(AField);
end;

{ TdxServerModeDataControllerSearch }

function TdxServerModeDataControllerSearch.DoSearch(
  AStartFilteredRecordIndex, AEndFilteredRecordIndex: Integer;
  const ASubText: string; AForward, AIsAnywhere: Boolean): Integer;
begin
  Result := DataController.DataSource.FindIncremental(DataController.Fields[ItemIndex].FieldName,
    ASubText, AStartFilteredRecordIndex, AForward, AIsAnywhere);
end;

function TdxServerModeDataControllerSearch.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TdxServerModeDataControllerGroups }

function TdxServerModeDataControllerGroups.GetCriteria(ARowIndex: Integer): TdxServerModeCriteria;
var
  AIndex: Integer;
  AServerGroupInfo: TdxServerModeGroupInfo;
begin
  AIndex := DataGroupIndexByRowIndex[ARowIndex];
  AServerGroupInfo := TdxServerModeDataGroupInfo(DataGroups[AIndex]).ServerGroupInfo;
  Result := DataController.DataSource.Cache.GetGroupWhere(AServerGroupInfo);
end;

function TdxServerModeDataControllerGroups.GetGroupDisplayText(ADataGroupIndex: TcxDataGroupIndex): string;
begin
  Result := TdxServerModeDataGroupInfo(DataGroups[ADataGroupIndex]).DisplayText;
end;

function TdxServerModeDataControllerGroups.GetGroupValue(ADataGroupIndex: TcxDataGroupIndex): Variant;
begin
  Result := TdxServerModeDataGroupInfo(DataGroups[ADataGroupIndex]).GroupValue;
end;

function TdxServerModeDataControllerGroups.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TdxServerModeDataFilterCriteriaItem }

function TdxServerModeDataFilterCriteriaItem.GetFilterOperatorClass: TcxFilterOperatorClass;
const
  ANullOperatorClasses: array[Boolean] of TcxFilterOperatorClass = (
    TcxFilterSQLNullOperator, TcxFilterSQLNotNullOperator);
begin
  if (OperatorKind in [foEqual, foNotEqual, foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith]) and (ValueIsNull(Value)) then
    Result := ANullOperatorClasses[OperatorKind in [foNotEqual, foNotLike]]
  else
    Result := inherited GetFilterOperatorClass;
end;

{ TdxServerModeDataFilterCriteria }

function TdxServerModeDataFilterCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TdxServerModeDataFilterCriteriaItem;
end;

{ TdxServerModeDataFindFilterCriteria }

function TdxServerModeDataFindFilterCriteria.GetEscapedCondition(
  const AText: string): string;
begin
  Result := DataController.DataSource.Adapter.MakeEscapedValue(AText, not UseExtendedSyntax, not UseExtendedSyntax);
end;

function TdxServerModeDataFindFilterCriteria.GetEscapeWildcard: Char;
begin
  Result := #0;
end;

function TdxServerModeDataFindFilterCriteria.IsConditionsLowerCase: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataFindFilterCriteria.GetDataController: TdxServerModeDataController;
begin
  Result := TdxServerModeDataController(inherited DataController);
end;

{ TdxServerModeDataController }

constructor TdxServerModeDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [sdcoAssignGroupingValues, sdcoKeepFocusedRowOnUpdate, sdcoSaveExpanding];
  FCustomDataSource := TdxServerModeProviderDataSource.Create;
  CustomDataSource := FCustomDataSource;
end;

destructor TdxServerModeDataController.Destroy;
begin
  CustomDataSource := nil;
  FreeAndNil(FCustomDataSource);
  if FDataSource <> nil then
    FDataSource.RemoveDataController(Self);
  inherited Destroy;
end;

procedure TdxServerModeDataController.Cancel;
begin
  LockStateInfo(False);
  try
    inherited Cancel;
    CheckFocusedSelected;
  finally
    UnlockStateInfo(False);
  end;
end;

procedure TdxServerModeDataController.CheckBrowseMode;
begin
  if Provider.IsEditing then
  begin
    LockStateInfo(False);
    try
      PostEditingData;
      Provider.InternalPost;
      Cancel;
    finally
      UnlockStateInfo(False);
    end;
  end;
end;

procedure TdxServerModeDataController.ChangeFieldName(AItemIndex: Integer;
  const AFieldName: string);
begin
  CheckItemRange(AItemIndex);
  if GetItemFieldName(AItemIndex) <> AFieldName then
    UpdateField(Fields[AItemIndex], AFieldName, False);
end;

function TdxServerModeDataController.CreateDataControllerInfo: TcxCustomDataControllerInfo;
begin
  Result := TdxServerModeDataControllerInfo.Create(Self);
end;

procedure TdxServerModeDataController.RemoveNotification(AComponent: TComponent);
begin
  if AComponent = DataSource then
    DataSource := nil
  else
    inherited RemoveNotification(AComponent);
end;

function TdxServerModeDataController.TryFocusRecord(ARecordIndex: Integer): Boolean;
begin
  if ARecordIndex < 0 then
    Result := False
  else
  begin
    if not Provider.FInInserting then
      CheckBrowseMode;
    Result := True;
  end;
end;

procedure TdxServerModeDataController.Unlocked;
begin
  inherited Unlocked;
end;

function TdxServerModeDataController.InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean;
begin
  Result := not ((DataSource <> nil) and DataSource.BookmarkValid(FBookmark));
end;

procedure TdxServerModeDataController.InternalClearBookmark;
begin
  FBookmark := Unassigned;
  inherited InternalClearBookmark;
end;

procedure TdxServerModeDataController.InternalGotoBookmark;
begin
  if (DataSource <> nil) and not LocateByKey(FBookmark) then
  begin
    InternalClearBookmark;
    Change([dccBookmark]);
  end;
end;

function TdxServerModeDataController.InternalSaveBookmark: Boolean;
var
  ABookmark: Variant;
begin
  Result := False;
  if DataSource <> nil then
  begin
    ABookmark := DataSource.GetBookmark(FocusedRecordIndex);
    if not VarEquals(FBookmark, ABookmark) then
    begin
      FBookmark := ABookmark;
      Result := True;
    end;
  end;
end;

function TdxServerModeDataController.AppendInSmartLoad: Integer;
begin
  Result := RecordCount;
  DataChanged(dcNew, -1, -1);
end;

procedure TdxServerModeDataController.DeleteInSmartLoad(ARecordIndex: Integer);
begin
  if IsProviderMode then
    Provider.DeleteRecord(ARecordIndex);
  if not Provider.IsInserting then
    DataStorage.DeleteRecord(ARecordIndex)
  else
    DataChanged(dcDeleted, -1, -1);
end;

procedure TdxServerModeDataController.DeleteFocused;
var
  ARowIndex: Integer;
  ACriteria: TdxServerModeCriteria;
begin
  ARowIndex := GetFocusedRowIndex;
  if (ARowIndex <> -1) and Provider.CanDelete then
  begin
    Provider.BeginDeleting;
    try
      if GetRowInfo(ARowIndex).Level < Groups.LevelCount then // It's Group Row
      begin
        ACriteria := Groups.GetCriteria(ARowIndex);
        try
          Provider.DeleteRecords(ACriteria);
        finally
          ACriteria.Free;
        end;
      end
      else
        DeleteFocusedRecord;
      ClearSelection; // !!!
    finally
      Provider.EndDeleting;
    end;
  end;
end;

procedure TdxServerModeDataController.DoUpdateRecord(ARecordIndex: Integer);
begin
  if not Provider.FInCanInitEditing then
    Change([dccUpdateRecord]);
end;

procedure TdxServerModeDataController.UpdateEditingRecord;
begin
  if (Provider.EditingRecordIndex = cxNullEditingRecordIndex) {or not Provider.CanModify} then Exit;
  DoUpdateRecord(Provider.EditingRecordIndex);
end;

procedure TdxServerModeDataController.DataSourceChanged;
var
  ANewActive: Boolean;
begin
  ANewActive := (DataSource <> nil) and DataSource.Active;
  if Active <> ANewActive then
    ActiveChanged(ANewActive);
end;

function TdxServerModeDataController.DoGetGroupRowDisplayText(
  const ARowInfo: TcxRowInfo; var AItemIndex: Integer): string;
var
  AGroupIndex: Integer;
begin
  Result := '';
  if (DataSource = nil) or not DataSource.IsConsistentCache then
    Exit;
  Provider.Freeze;
  try
    AGroupIndex := DataControllerInfo.DataGroups.GetIndexByRowIndex(ARowInfo.RowIndex);
    Result := DataControllerInfo.DataGroups.Items[AGroupIndex].DisplayText;
  finally
    Provider.Unfreeze;
  end;
end;

procedure TdxServerModeDataController.InitializeDescriptor(AField: TcxCustomDataField;
  var ADescriptor: TdxServerModeDescriptor);
begin
//do nothing
end;

procedure TdxServerModeDataController.SetDataSource(
  const Value: TdxServerModeCustomDataSource);
begin
  if Value <> FDataSource then
  begin
    if (FDataSource <> nil) and not FDataSource.IsDestroying then
    begin
      FDataSource.RemoveFreeNotification(Notifier);
      FDataSource.RemoveDataController(Self);
    end;
    FDataSource := Value;
    if FDataSource <> nil then
    begin
      FDataSource.AddDataController(Self);
      FDataSource.FreeNotification(Notifier);
    end;
    DataSourceChanged;
  end;
end;

procedure TdxServerModeDataController.SetOptions(const Value: TdxServerModeDataControllerOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Refresh;
  end;
end;

function TdxServerModeDataController.IsDataSourcePersistentField(AField: TField): Boolean;
begin
  Result := DataSource.IsPersistentField(AField);
end;

procedure TdxServerModeDataController.UpdateField(
  ADataField: TdxServerModeDataField; const AFieldNames: string;
  AIsLookup: Boolean);

  function CheckExistField(const AFieldName: string): Boolean;
  var
    I: Integer;
    ACurrentField: TdxServerModeDataField;
    APrevValueTypeClass: TcxValueTypeClass;
  begin
    APrevValueTypeClass := ADataField.ValueTypeClass;
    ADataField.ReferenceField := nil;
    Result := False;
    if ADataField.FieldName = '' then
      ADataField.ValueTypeClass := nil
    else
    begin
      for I := 0 to FieldCount - 1 do
      begin
        ACurrentField := Fields[I];
        if ACurrentField <> ADataField then
        begin
          if IsEqualFieldNames(ACurrentField.FieldName, ADataField.FieldName) then
          begin
            ADataField.ReferenceField := ACurrentField;
            Result := True;
            Break;
          end;
        end;
    end;
    end;
    if (ADataField.ValueTypeClass <> APrevValueTypeClass) and not ADataField.IsInternal then
      DoValueTypeClassChanged(ADataField.Index);
  end;

begin
  ADataField.ClearFields;
  if not AIsLookup then
  begin
    if ADataField.FieldName <> AFieldNames then
      inherited Fields.ReassignFields(ADataField);
    ADataField.FieldName := AFieldNames;
  end;

    if CheckExistField(AFieldNames) then
      Change([dccData])
    else
      LayoutChanged([lcStructure]);
end;

procedure TdxServerModeDataController.UpdateInternalField(const AFieldName: string; var AField: TdxServerModeDataField);
begin
  if AFieldName = '' then
  begin
    AField.Free;
    AField := nil;
    Change([dccData]);
  end
  else
  begin
    if AField = nil then
      AField := AddInternalField as TdxServerModeDataField;
    UpdateField(AField, AFieldName, False);
    PrepareField(AField);
  end;
end;

function TdxServerModeDataController.GetDataControllerInfo: TdxServerModeDataControllerInfo;
begin
  Result := TdxServerModeDataControllerInfo(inherited DataControllerInfo);
end;

function TdxServerModeDataController.GetSummary: TdxServerModeDataSummary;
begin
  Result := TdxServerModeDataSummary(inherited Summary);
end;

procedure TdxServerModeDataController.SetSummary(Value: TdxServerModeDataSummary);
begin
  inherited Summary := Value;
end;

function TdxServerModeDataController.GetDataProviderClass: TcxCustomDataProviderClass;
begin
  Result := TdxCustomServerModeDataProvider;
end;

function TdxServerModeDataController.GetDataSelectionClass: TcxDataSelectionClass;
begin
  Result := TdxServerModeDataSelection;
end;

function TdxServerModeDataController.GetFilterCriteriaClass: TcxDataFilterCriteriaClass;
begin
  Result := TdxServerModeDataFilterCriteria;
end;

function TdxServerModeDataController.GetSearchClass: TcxDataControllerSearchClass;
begin
  Result := TdxServerModeDataControllerSearch;
end;

function TdxServerModeDataController.GetSummaryClass: TcxDataSummaryClass;
begin
  Result := TdxServerModeDataSummary;
end;

function TdxServerModeDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TdxServerModeSummaryItem;
end;

function TdxServerModeDataController.CanFixRows: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.CanFocusRecord(ARecordIndex: Integer): Boolean;
begin
  if (Provider.IsInserting and (ARecordIndex = Provider.EditingRecordIndex)) or
    (UseNewItemRowForEditing and (ARecordIndex < 0)) then
  begin
    if not Provider.IsInserting and (ARecordIndex < 0) then
      Post;
    Result := True;
    Exit;
  end;
  Provider.BeginLocate;
  try
    Result := TryFocusRecord(ARecordIndex)
  finally
    Provider.EndLocate;
  end;
  SyncMasterPos;
end;

function TdxServerModeDataController.CreateFindFilterCriteria: TcxDataFindFilterCriteria;
begin
  Result := TdxServerModeDataFindFilterCriteria.Create(Self);
end;

function TdxServerModeDataController.GetIsRowInfoValid: Boolean;
begin
  Result := inherited GetIsRowInfoValid and not IsDataLoading;
end;

function TdxServerModeDataController.IsMergedGroupsSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.IsSmartLoad: Boolean;
begin
  Result := True;
end;

procedure TdxServerModeDataController.PrepareField(AField: TcxCustomDataField);
var
  ADataField: TdxServerModeDataField;
  AValueTypeClass: TcxValueTypeClass;
begin
  inherited PrepareField(AField);
  if (DataSource <> nil) and not DataSource.IsDestroying and not Assigned(AField.ReferenceField) then
  begin
    ADataField := AField as TdxServerModeDataField;
    ADataField.Field := DataSource.Fields.FindField(ADataField.FieldName);
    if ADataField.Field <> nil then
    begin
      DataSource.UpdateFieldIndex(ADataField);
      AValueTypeClass := GetValueTypeClassByField(ADataField.Field);
      ADataField.SetPropertiesByField(ADataField.Field, ADataField.ValueTypeClass <> AValueTypeClass);
    end;
  end;
end;

function TdxServerModeDataController.GetField(Index: Integer): TdxServerModeDataField;
begin
  Result := inherited Fields[Index] as TdxServerModeDataField;
end;

function TdxServerModeDataController.GetFieldClass: TcxCustomDataFieldClass;
begin
  Result := TdxServerModeDataField;
end;

function TdxServerModeDataController.GetFieldByFieldName(const AFieldName: string): TdxServerModeDataField;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FieldCount - 1 do
    if SameText(Fields[I].FieldName, AFieldName) then
    begin
      Result := Fields[I];
      Break;
    end;
end;

function TdxServerModeDataController.GetGroupsClass: TcxDataControllerGroupsClass;
begin
  Result := TdxServerModeDataControllerGroups;
end;

function TdxServerModeDataController.GetFieldCount: Integer;
begin
  Result := inherited Fields.Count;
end;

function TdxServerModeDataController.GetGroupRowValueByItemIndex(
  const ARowInfo: TcxRowInfo; AItemIndex: Integer): Variant;
var
  AGroupIndex: Integer;
begin
  Provider.Freeze;
  try
    AGroupIndex := DataControllerInfo.DataGroups.GetIndexByRowIndex(ARowInfo.RowIndex);
    Result := DataControllerInfo.DataGroups.Items[AGroupIndex].GroupValue;
  finally
    Provider.Unfreeze;
  end;
end;

function TdxServerModeDataController.GetInternalDisplayText(
  ARecordIndex: Integer; AField: TcxCustomDataField): string;
begin
  Provider.Freeze;
  try
    if (ARecordIndex >= 0) and IsDataField(AField) and Assigned(TdxServerModeDataField(AField).ValueDef) then
      try
        Result := VarToStr(GetInternalValue(ARecordIndex, AField));
      except
        on EVariantError do
          Result := '';
      end
    else
      Result := '';
  finally
    Provider.Unfreeze;
  end;
end;

function TdxServerModeDataController.GetInternalValue(ARecordIndex: Integer;
  AField: TcxCustomDataField): Variant;
var
  AServerField: TdxServerModeDataField absolute AField;
  AIndex: Integer;
begin
  Provider.Freeze;
  try
    if (ARecordIndex >= 0) and IsDataField(AField) and Assigned(TdxServerModeDataField(AField).ValueDef) then
    begin
      AIndex := AServerField.FieldIndex;
      if AIndex = -1 then
        Result := inherited GetInternalValue(ARecordIndex, AField)
      else
        Result := Provider.GetServerRowValue(ARecordIndex, AIndex);
    end
    else
      Result := Null;
  finally
    Provider.Unfreeze;
  end;
end;

procedure TdxServerModeDataController.GetKeyFields(AList: TList);
var
  I: Integer;
begin
  if DataSource <> nil then
    for I := 0 to DataSource.KeyFields.Count - 1 do
      if GetItemByFieldName(DataSource.KeyFields[I].Field.FieldName) <> nil then
        AList.Add(GetItemByFieldName(DataSource.KeyFields[I].Field.FieldName));
end;

function TdxServerModeDataController.IsResetFocusedRowNeeded: Boolean;
begin
  Result := not GetKeepFocusedRowOnUpdatingSetting and
   (GroupingChangedFlag or SortingChangedFlag or FilterChangedFlag or LoadedStorage);
end;

procedure TdxServerModeDataController.ResetDataSourceCache;
begin
  DataControllerInfo.FChanges := DataControllerInfo.FChanges +
    [dcicFocusedRow, dcicLoad, dcicGrouping, dcicSummary];
  DataControllerInfo.FullCollapse;
end;

procedure TdxServerModeDataController.PopulateFilterValues(AList: TcxDataFilterValueList;
  AItemIndex: Integer; ACriteria: TcxFilterCriteria;
  var AUseFilteredRecords: Boolean; out ANullExists: Boolean; AUniqueOnly: Boolean);
begin
  if (DataSource <> nil) and (Fields[AItemIndex].Field <> nil) and DataSource.Active then
    DataSource.PopulateFilterValues(AList, Fields[AItemIndex].Field, ACriteria, AUseFilteredRecords, ANullExists)
  else
    inherited PopulateFilterValues(AList, AItemIndex, ACriteria, AUseFilteredRecords, ANullExists, AUniqueOnly);
end;

function TdxServerModeDataController.CalcEditingRecordIndex: Integer;
begin
  if NewItemRowFocused then
    Result := NewItemRecordIndex
  else
    if GetFocusedRowIndex <> -1 then
      Result := GetRowInfo(GetFocusedRowIndex).RecordIndex
    else
      Result := cxNullEditingRecordIndex;
end;

function TdxServerModeDataController.GetAnsiSortSetting: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.GetAssignGroupingValuesSetting: Boolean;
begin
  Result := sdcoAssignGroupingValues in Options;
end;

function TdxServerModeDataController.GetAssignMasterDetailKeysSetting: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.GetCaseInsensitiveSetting: Boolean;
begin
  Result := True;
end;

function TdxServerModeDataController.GetGroupsAlwaysExpandedSetting: Boolean;
begin
  Result := sdcoGroupsAlwaysExpanded in Options;
end;

function TdxServerModeDataController.GetSaveExpandingSetting: Boolean;
begin
  Result := sdcoSaveExpanding in Options;
end;

function TdxServerModeDataController.GetSortByDisplayTextSetting: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.GetFocusTopRowAfterSortingSetting: Boolean;
begin
  Result := not GetKeepFocusedRowOnUpdatingSetting;
end;

function TdxServerModeDataController.GetImmediatePostSetting: Boolean;
begin
  Result := sdcoImmediatePost in Options;
end;

function TdxServerModeDataController.GetInsertOnNewItemRowFocusingSetting: Boolean;
begin
  Result := sdcoInsertOnNewItemRowFocusing in Options;
end;

function TdxServerModeDataController.GetKeepFocusedRowOnUpdatingSetting: Boolean;
begin
  Result := sdcoKeepFocusedRowOnUpdate in Options;
end;

function TdxServerModeDataController.GetItemByFieldName(
  const AFieldName: string): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ItemCount - 1 do
    if AnsiCompareText(GetItemFieldName(I), AFieldName) = 0 then
    begin
      Result := GetItem(I);
      Break;
    end;
end;

function TdxServerModeDataController.GetItemField(AItemIndex: Integer): TField;
begin
  CheckItemRange(AItemIndex);
  if (Fields[AItemIndex].Field = nil) and (Fields[AItemIndex].FieldName <> '') then
    PrepareField(Fields[AItemIndex]);
  Result := Fields[AItemIndex].Field;
end;

function TdxServerModeDataController.GetItemFieldName(
  AItemIndex: Integer): string;
begin
  CheckItemRange(AItemIndex);
  Result := Fields[AItemIndex].FieldName;
end;

function TdxServerModeDataController.IsDataField(AField: TcxCustomDataField): Boolean;
begin
  Result := (TdxServerModeDataField(AField).Field <> nil) and inherited IsDataField(AField);
end;

function TdxServerModeDataController.GetFindFilterCriteria: TdxServerModeDataFindFilterCriteria;
begin
  Result := TdxServerModeDataFindFilterCriteria(inherited FindFilterCriteria);
end;

function TdxServerModeDataController.GetGroups: TdxServerModeDataControllerGroups;
begin
  Result := TdxServerModeDataControllerGroups(inherited Groups);
end;

function TdxServerModeDataController.GetProvider: TdxCustomServerModeDataProvider;
begin
  Result := TdxCustomServerModeDataProvider(inherited Provider);
end;

function TdxServerModeDataController.GetFilterItemFieldName(AItem: TObject): string;
var
  AField: TdxServerModeDataField;
  I, AFieldCount: Integer;
begin
  Result := '';
  AField := inherited Fields.FieldByItem(AItem) as TdxServerModeDataField;
  if Assigned(AField) then
    if Assigned(AField.Field) then
      if AField.IsLookup then
      begin
        AFieldCount := AField.FieldCount;
        if AField.FieldCount > 0 then
          if AFieldCount = 1 then
            Result := GetFilterFieldName(AField[0].Field, True)
          else
          begin
            // Multiply Lookup Keys
            Result := GetFilterFieldName(AField[0].Field, True);
            for I := 1 to AFieldCount - 1 do
              Result := Result + ';' + GetFilterFieldName(AField[I].Field, True);
          end
        else
          Result := '';
      end
      else
        Result := GetFilterFieldName(AField.Field, True)
    else
      Result := AField.FieldName;
end;

function TdxServerModeDataController.GetRecordCount: Integer;
begin
  if Provider.IsActive then
  begin
    Result := DataSource.RowCount;
    if Provider.IsInserting then
      Inc(Result);
  end
  else
    Result := 0;
end;

function TdxServerModeDataController.InsertRecord(ARecordIndex: Integer): Integer;
begin
  Result := ARecordIndex;
end;

function TdxServerModeDataController.IsKeyField(AField: TField): Boolean;
begin
  Result := (DataSource <> nil) and (DataSource.KeyFields.IndexOf(AField) <> -1);
end;

function TdxServerModeDataController.IsProviderMode: Boolean;
begin
  Result := True;
end;

function TdxServerModeDataController.MultiSelectionSyncGroupWithChildren: Boolean;
begin
  Result := False;
end;

function TdxServerModeDataController.FindRecordIndexByKey(const AKeyFieldValues: Variant): Integer;
begin
  if DataSource <> nil then
    Result := DataSource.Cache.GetRowIndexByKey(AKeyFieldValues)
  else
    Result := -1;
end;

function TdxServerModeDataController.GetRecordId(ARecordIndex: Integer): Variant;
begin
  CheckRecordRange(ARecordIndex);
  Result := Provider.GetRowKey(ARecordIndex);
end;

function TdxServerModeDataController.LocateByKey(const AKeyFieldValues: Variant): Boolean;
var
  ARecordIndex, ARowIndex: Integer;
begin
  ARecordIndex := FindRecordIndexByKey(AKeyFieldValues);
  ARowIndex := -1;
  if ARecordIndex <> -1 then
    ARowIndex := GetRowIndexByRecordIndex(ARecordIndex, True);
  ChangeFocusedRowIndex(ARowIndex);
  Result := ARowIndex <> -1;
end;

procedure TdxServerModeDataController.RefreshExternalData;
begin
  if DataSource = nil then
    Exit;
  LockStateInfo;
  try
    DataSource.Refresh;
  finally
    UnlockStateInfo;
  end;
end;

function TdxServerModeDataController.IsBookmarkAvailable: Boolean;
begin
  Result := not VarIsEmpty(FBookmark);
end;

function TdxServerModeDataController.IsBookmarkRow(ARowIndex: Integer): Boolean;
var
  ARecordIndex: Integer;
begin
  Result := False;
  if IsBookmarkAvailable and (DataSource <> nil) then
  begin
    ARecordIndex := FindRecordIndexByKey(FBookmark);
    if ARecordIndex <> -1 then
      Result := ARowIndex = GetRowIndexByRecordIndex(ARecordIndex, False);
  end;
end;

end.

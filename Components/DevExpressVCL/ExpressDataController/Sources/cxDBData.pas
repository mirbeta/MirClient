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

unit cxDBData;

{$I cxVer.inc}

interface

uses
  Types, Variants, SysUtils, Classes,  DB, dxCore, cxDataUtils, cxCustomData,
  cxDataStorage, cxVariants, cxFilter, cxDBFilter, cxDB;

type
  TcxBookmark = TBookmark;
  TcxDBDataController = class;
  TcxDBDataProvider = class;
  TcxDBDataModeController = class;
  TcxDBDataFilterCriteria = class;
  TGetListProc = procedure(AList: TList) of object;

  { TcxDBProviderDetailFilterAdapter }

  TcxDBProviderDetailFilterAdapter = class(TcxDBAdapterItem)
  public
    function IsCurrentQuery(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant): Boolean; virtual;
    procedure ReopenSQL(ADataSet: TDataSet; const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean); virtual;
    procedure SetFilter(ADataSet: TDataSet; const AMasterDetailKeyFieldNames: string;
      const AMasterDetailKeyValues: Variant; AIsSQLMode: Boolean; var AReopened: Boolean); virtual;
  end;

  TcxDBCustomDataLink = class(TDataLink)
  private
    function GetIsDataSetBusy: Boolean;
  protected
    function GetIsDataSetBusyState: Boolean; virtual;

    property IsDataSetBusy: Boolean read GetIsDataSetBusy;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcxDBDataLink }

  TcxDBDataLink = class(TcxDBCustomDataLink)
  private
    FLayoutChangedFlag: Boolean;
    FPreventLayoutChanged: Boolean;
    FProvider: TcxDBDataProvider;
    procedure DoInsertingRecord;
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: TdxNativeInt); override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    function GetIsDataSetBusyState: Boolean; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;

    property Provider: TcxDBDataProvider read FProvider;
  public
    constructor Create(AProvider: TcxDBDataProvider);
  end;

  { TcxDBDataProvider }

  TcxDBDataProviderSavePosInfo = record
    Bookmark: TcxBookmark;
    Bof: Boolean; // conflicts with C++ macro
    Eof: Boolean; // conflicts with C++ macro
    ActiveRecord: Integer;
  end;

  TcxDBDataAssignedFields = class
  private
    FFields: TcxDBFieldList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(AField: TField);
    procedure AddFieldsByFieldList(AFieldList: TList);
    procedure AddFieldsByFieldNames(ADataSet: TDataSet; const AFieldNames: string);
    procedure Clear;
    function IsEmpty: Boolean;
    function IsFieldExists(AField: TField): Boolean;
  end;

  TcxDBDataProvider = class(TcxCustomDataProvider)
  private
    FAssignedFields: TcxDBDataAssignedFields;
    FDataLink: TcxDBDataLink;
    FInCanInitEditing: Boolean;
    FInSetDataSource: Boolean;
    FIncrementalSearching: Boolean;
    FInFirst: Boolean;
    FInInsert: Boolean;
    FInsertOperation: Boolean;
    FInUpdateGridMode: Boolean;
    FIsDataSetCurrent: Boolean;
    FInNotify: Boolean;
    FListeners: TList;
    FNewRecordKeyValue: Variant;
    FPostLocateFlag: Boolean;
    FRecordIndex: Integer;
//    FNearestRecordID: Variant;
    FSavePosInfo: TcxDBDataProviderSavePosInfo;
    FUpdatingPrevFocusedRecordIndex: Integer;
    FUpdatingRecordIndex: Integer;
    FUpdatingRecordIsInserting: Boolean;
    FUpdatingNewRecordIndex: Integer;
    procedure AddNewRecord(AOperation: TDataOperation);
    function GetDataController: TcxDBDataController;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetIsDataSetCurrent: Boolean;
    function GetIsSmartRefresh: Boolean;
    procedure RemoveFromListeners;
    procedure SetDataSource(Value: TDataSource);
    procedure SetIsDataSetCurrent(Value: Boolean);
  protected
    function IsCustomDataSourceSupported: Boolean; override;
    function IsGridMode: Boolean; override;
    function IsDataSource: Boolean; override;
    function IsKeyNavigation: Boolean;
    function IsOtherInsert: Boolean; override;
    function IsSyncInsert: Boolean;
    function IsSyncMode: Boolean; override;
    // DataSet
    function DataSet_ActiveRecord: Integer;
    function DataSet_BookmarkAvailable: Boolean;
    function DataSet_BufferCount: Integer;
    // State
    function IsActive: Boolean; override;
    function IsActiveDataSet: Boolean; override;
    function IsBOF: Boolean; override;
    function IsChanging: Boolean; override;
    function IsEditing: Boolean; override;
    function IsEOF: Boolean; override;
    function IsGridModeUpdating: Boolean; override;
    function IsInserting: Boolean; override;
    function IsModified: Boolean; override;
    function IsDataSetModified: Boolean; virtual;
    // Navigation
    function BookmarkValid(const ABookmark: TcxBookmark): Boolean; virtual;
    function CompareBookmarks(const AItem1, AItem2: TcxBookmark): Integer; virtual;
    procedure CorrectRecordIndex(ARecordIndex: Integer); override;
    procedure First; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Last; override;
    procedure MoveBy(ADistance: Integer); override;
    procedure Scroll(ADistance: Integer); override;
    procedure SavePos; override;
    procedure RestorePos; override;
    // Editing
    function CanDelete: Boolean; override;
    function CanInitEditing(ARecordIndex: Integer): Boolean; override;
    function CanModify: Boolean; override;

    procedure Append; override;
    procedure InternalCancel; virtual;
    procedure Cancel; override;
    procedure Delete; override;
    procedure DeleteRecords(AList: TList); override;
    procedure DeleteSelection; override;
    procedure Edit; override;
    function GetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField;
      AEditValueSource: TcxDataEditValueSource): Variant; override;
    procedure Insert; override;
    procedure Post(AForcePost: Boolean = False); override;
    procedure PostEditingData; override;
    function SetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField; const AValue: Variant; AEditValueSource: TcxDataEditValueSource): Boolean; override;

    procedure EndDeleting; override;

    procedure AssignItemValue(ARecordIndex: Integer; AField: TcxCustomDataField; const AValue: Variant); override;
    procedure DoInitInsertingRecord(AInsertingRecordIndex: Integer; const AGroupValues: TcxDataSummaryValues); override;
    // Notification
    procedure ActiveChanged(AActive: Boolean); override;
    procedure AddListener(AProvider: TcxDBDataProvider);
    procedure BeginLocate; override;
    procedure DataScrolled(ADistance: Integer); override;
    procedure Freeze; override;
    procedure LayoutChanged(ADataLayoutChanges: TcxDataLayoutChanges); override;
    procedure RecordChanged(Field: TField); virtual;
    procedure ResetPostLocateFlag;
    procedure ResetSmartRefresh;
    procedure SetPostLocateFlag;
    procedure Unfreeze; override;
    // Smart Refresh
    procedure SmartDataChanged;
    // GridMode
    procedure CheckExpandedDetails;
    function DoLocate(const ASubText: string; AForward, ANext: Boolean): Boolean;
    function GetDataBufferCount: Integer;
    function GetDataRecordCount: Integer;
    function GetExternalDataDisplayText(ARecordIndex: Integer; AField: TcxCustomDataField): string; override;
    function GetExternalDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    procedure LoadDataBuffer; override;
    procedure UpdateGridMode;
    // Master Detail
    function GetDetailHasChildren(ARecordIndex: Integer; ARelationIndex: Integer): Boolean; override;
    // Structure
    function GetRecordIndex: Integer; override;
    function GetValueDefReaderClass: TcxValueDefReaderClass; override;
    property DataController: TcxDBDataController read GetDataController;
    property DataLink: TcxDBDataLink read FDataLink;
    property UpdatingRecordIndex: Integer read FUpdatingRecordIndex;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;
    function IsCanInitEditingBusy: Boolean;
    function IsDataSetBusy: Boolean;
    function IsDetailDataSetBusy: Boolean;
    function IsOtherDataControllerUpdating: Boolean;
    function IsOtherDetailDataSetInInsert: Boolean;
    function IsOtherDetailChanged: Boolean;
    function IsUniDirectional: Boolean;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property IsDataSetCurrent: Boolean read GetIsDataSetCurrent write SetIsDataSetCurrent;
    property IsSmartRefresh: Boolean read GetIsSmartRefresh;
  end;

  TcxValueDefDBReader = class(TcxValueDefReader)
  public
    function GetDisplayText(AValueDef: TcxValueDef): string; override;
    function GetValue(AValueDef: TcxValueDef): Variant; override;
    function IsInternal(AValueDef: TcxValueDef): Boolean; override;
  end;

  { TcxDBDataField }

  TcxDBDataField = class(TcxCustomDataField)
  private
    FField: TField;
    FFieldName: string;
    FFields: TList;
    FIsCurrency: Boolean;
    FParentField: TcxDBDataField;
    function GetDataController: TcxDBDataController;
    function GetField: TField;
    function GetFieldItem(Index: Integer): TcxDBDataField;
    function GetFieldCount: Integer;
    function GetIsLookup: Boolean;
    function GetProvider: TcxDBDataProvider;
    procedure SetField(Value: TField);
  protected
    procedure AddField(ADataField: TcxDBDataField);
    procedure ClearFields;
    procedure Reassign(ASource: TcxCustomDataField); override;
    procedure RemoveField(ADataField: TcxDBDataField);
    procedure RemoveNotification(AComponent: TComponent); override;
    procedure SetPropertiesByField(AField: TField; AFieldChanged: Boolean);
    function SupportsMultiThreading: Boolean; override;

    property DataController: TcxDBDataController read GetDataController;
    property Field: TField read GetField write SetField;
    property FieldCount: Integer read GetFieldCount;
    property FieldName: string read FFieldName write FFieldName;
    property Fields[Index: Integer]: TcxDBDataField read GetFieldItem; default;
    property IsCurrency: Boolean read FIsCurrency;
    property IsLookup: Boolean read GetIsLookup;
    property ParentField: TcxDBDataField read FParentField;
    property Provider: TcxDBDataProvider read GetProvider;
  public
    destructor Destroy; override;
    function CanModify(AEditValueSource: TcxDataEditValueSource): Boolean; override;
    function IsUnbound: Boolean; override;
    function IsValueDefInternal: Boolean; override;
  end;

  { TcxDBDataRelation }

  TcxDBDataRelation = class(TcxCustomDataRelation)
  private
    FMasterKeyField: TcxDBDataField;
    function GetDataController: TcxDBDataController;
    function GetDetailKeyFieldNames: string;
    function GetMasterKeyFieldCount: Integer;
    function GetMasterKeyFieldNames: string;
  protected
    procedure GetMasterKeyFields(AList: TList);
    function IsLinked: Boolean; virtual;
    procedure RemoveDataField(ADataField: TcxCustomDataField); override;
    procedure RemoveMasterKeyField;
    procedure UpdateMasterDetailKeyFieldNames;

    property MasterKeyFieldCount: Integer read GetMasterKeyFieldCount;
  public
    destructor Destroy; override;
    function GetMasterRecordID(ARecordIndex: Integer): Variant;
    property DataController: TcxDBDataController read GetDataController;
    property DetailKeyFieldNames: string read GetDetailKeyFieldNames;
    property MasterKeyFieldNames: string read GetMasterKeyFieldNames;
  end;

  {DB Find Filter Criteria}

  TcxDBDataFindFilterCriteria = class(TcxDataFindFilterCriteria)
  private
    function GetDataController: TcxDBDataController;
  protected
    procedure Update; override;

    property DataController: TcxDBDataController read GetDataController;
  end;

  { DB Filter }

  TcxDBDataFilterCriteriaItem = class(TcxDataFilterCriteriaItem)
  private
    function GetDataController: TcxDBDataController;
    function GetDBField: TcxDBDataField;
  protected
    function GetFilterOperatorClass: TcxFilterOperatorClass; override;
  public
    property DataController: TcxDBDataController read GetDataController;
    property DBField: TcxDBDataField read GetDBField;
  end;

  TcxDBDataFilterBeforeChangeEvent = procedure(Sender: TcxDBDataFilterCriteria;
    ADataSet: TDataSet; const AFilterText: string) of object;
  TcxDBDataFilterFormatFilterTextValueEvent = procedure(Sender: TcxDBDataFilterCriteria;
    AItem: TcxFilterCriteriaItem; const AValue: Variant; var ADisplayValue: string) of object;

  TcxDBDataFilterCriteria = class(TcxDataFilterCriteria)
  private
    FAutoDataSetFilter: Boolean;
    FIgnoreOrigin: Boolean;
    FOnBeforeChange: TcxDBDataFilterBeforeChangeEvent;
    FOnFormatFilterTextValue: TcxDBDataFilterFormatFilterTextValueEvent;
    function GetDataController: TcxDBDataController;
    procedure SetAutoDataSetFilter(Value: Boolean);
    procedure SetIgnoreOrigin(Value: Boolean);
  protected
    procedure FormatFilterTextValue(AItem: TcxFilterCriteriaItem; const AValue: Variant;
      var ADisplayValue: string); override;
    function GetItemClass: TcxFilterCriteriaItemClass; override;
    procedure Update; override;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    procedure Assign(Source: TPersistent; AIgnoreItemNames: Boolean = False); override;
    procedure AssignEvents(Source: TPersistent); override;
    function IsAutoDataSetFilter: Boolean;
    function IsFiltering: Boolean; override;
    procedure RestoreDefaults; override;
    property DataController: TcxDBDataController read GetDataController;
  published
    // TODO: add property CustomFiltering -> IsFiltering = False!
    property AutoDataSetFilter: Boolean read FAutoDataSetFilter write SetAutoDataSetFilter default False;
    property DateTimeFormat;
    property IgnoreOrigin: Boolean read FIgnoreOrigin write SetIgnoreOrigin default True;
    property TranslateBetween;
    property TranslateIn;
    property TranslateLike;
    property SupportedLike;
    property OnBeforeChange: TcxDBDataFilterBeforeChangeEvent read FOnBeforeChange write FOnBeforeChange;
    property OnFormatFilterTextValue: TcxDBDataFilterFormatFilterTextValueEvent read FOnFormatFilterTextValue write FOnFormatFilterTextValue;
  end;

  { DB Summary }

  TcxDBDataSummaryItem = class(TcxDataSummaryItem)
  private
    FDataField: TcxDBDataField;
    FFieldName: string;
    function GetDBDataController: TcxDBDataController;
    procedure SetFieldName(const Value: string);
  protected
    function IsCurrency(AVarType: TVarType): Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    property DataController: TcxDBDataController read GetDBDataController;
    function DataField: TcxCustomDataField; override;
  published
    property FieldName: string read FFieldName write SetFieldName;
  end;

  { TcxDBDataModeController }

  TcxDBDataModeControllerDetailIsCurrentQueryEvent = function(Sender: TcxDBDataModeController;
    ADataSet: TDataSet; const AMasterDetailKeyFieldNames: string;
    const AMasterDetailKeyValues: Variant): Boolean of object;

  TcxDBDataModeControllerDetailFirstEvent = procedure(Sender: TcxDBDataModeController;
    ADataSet: TDataSet; const AMasterDetailKeyFieldNames: string;
    const AMasterDetailKeyValues: Variant; var AReopened: Boolean) of object;

  TcxDBDataModeController = class(TPersistent)
  private
    FDataController: TcxDBDataController;
    FDetailInSQLMode: Boolean;
    FDetailInSyncMode: Boolean;
    FGridMode: Boolean;
    FGridModeBufferCount: Integer;
    FSmartRefresh: Boolean;
    FSyncInsert: Boolean;
    FSyncMode: Boolean;
    FOnDetailFirst: TcxDBDataModeControllerDetailFirstEvent;
    FOnDetailIsCurrentQuery: TcxDBDataModeControllerDetailIsCurrentQueryEvent;
    procedure SetGridMode(Value: Boolean);
    procedure SetGridModeBufferCount(Value: Integer);
    procedure SetSmartRefresh(Value: Boolean);
    procedure SetSyncMode(Value: Boolean);
  protected
    function DetailIsCurrentQuery(const AMasterDetailKeyFieldNames: string; const AMasterDetailKeyValues: Variant): Boolean; virtual;
    procedure DoDetailFirst(const AMasterDetailKeyFieldNames: string; const AMasterDetailKeyValues: Variant; var AReopened: Boolean); virtual;

    property DetailInSyncMode: Boolean read FDetailInSyncMode write FDetailInSyncMode default True;
  public
    constructor Create(ADataController: TcxDBDataController);
    procedure Assign(Source: TPersistent); override;
    property DataController: TcxDBDataController read FDataController;
    property SyncInsert: Boolean read FSyncInsert write FSyncInsert default True;
  published
    property DetailInSQLMode: Boolean read FDetailInSQLMode write FDetailInSQLMode default False;
    property GridMode: Boolean read FGridMode write SetGridMode default False;
    property GridModeBufferCount: Integer read FGridModeBufferCount write SetGridModeBufferCount default 0;
    property SmartRefresh: Boolean read FSmartRefresh write SetSmartRefresh default False;
    property SyncMode: Boolean read FSyncMode write SetSyncMode default True;
    property OnDetailFirst: TcxDBDataModeControllerDetailFirstEvent read FOnDetailFirst write FOnDetailFirst;
    property OnDetailIsCurrentQuery: TcxDBDataModeControllerDetailIsCurrentQueryEvent read FOnDetailIsCurrentQuery write FOnDetailIsCurrentQuery;
  end;

  TcxCompareBookmarksFunc = function (const AItem1, AItem2: TcxBookmark): Integer of object;

  { TcxBookmarkList }

  TcxBookmarkList = class
  private
    FItems: array of TcxBookmark;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TcxBookmark;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AItem: TcxBookmark);
    procedure Insert(AIndex: Integer; AItem: TcxBookmark);
    procedure Delete(AIndex: Integer);
    procedure QuickSort(L, R: Integer; ACompareFunc: TcxCompareBookmarksFunc);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxBookmark read GetItem; default;
  end;

  { TcxDBDataSelection }

  TcxDBDataSelection = class(TcxDataSelection)
  private
    FAnchorBookmark: TcxBookmark;
    FBookmarks: TcxBookmarkList;
    FInSelectAll: Boolean;
    function GetDataController: TcxDBDataController;
  protected
    procedure ClearAnchor; override;
    function CompareBookmarks(const AItem1, AItem2: TcxBookmark): Integer;
    procedure InternalAdd(AIndex, ARowIndex, ARecordIndex, ALevel: Integer); override;
    procedure InternalClear; override;
    procedure InternalDelete(AIndex: Integer); override;
    function FindBookmark(const ABookmark: TcxBookmark; var AIndex: Integer): Boolean;
    function GetRowBookmark(ARowIndex: Integer): TcxBookmark; virtual;
    function RefreshBookmarks: Boolean;
    procedure SyncCount;
    procedure EnsureBookmarkListOrder;
  public
    constructor Create(ADataController: TcxCustomDataController); override;
    destructor Destroy; override;
    function FindByRowIndex(ARowIndex: Integer; var AIndex: Integer): Boolean; override;
    procedure SelectAll;
    procedure SelectFromAnchor(AToBookmark: TcxBookmark; AKeepSelection: Boolean);
    property DataController: TcxDBDataController read GetDataController ;
  end;

  { TcxDBDataControllerInfo }

  TcxDBDataControllerInfo = class(TcxCustomDataControllerInfo)
  private
    function GetDataController: TcxDBDataController;
  protected
    function GetSelectionMaxRecordCount: Integer; override;
  public
    property DataController: TcxDBDataController read GetDataController;
  end;

  { TcxDBDataController }

  TcxDBDataDetailHasChildrenEvent = procedure(Sender: TcxDBDataController;
    ARecordIndex, ARelationIndex: Integer; const AMasterDetailKeyFieldNames: string;
    const AMasterDetailKeyValues: Variant; var HasChildren: Boolean) of object;

  TcxDBDataControllerGridModeChangedEvent = procedure(Sender: TcxDBDataController) of object;

  TcxDBDataController = class(TcxCustomDataController)
  private
    FBookmark: TcxBookmark;
    FCreatedDataController: TcxCustomDataController;
    FDataModeController: TcxDBDataModeController;
    FDetailKeyFieldNames: string;
    FFocusingRecord: Boolean;
    FFocusingRecordIndex: Integer;
    FInCheckBrowseMode: Boolean;
    FInCheckCurrentQuery: Boolean;
    FInResetDataSetCurrent: Boolean;
    FInUnboundCopy: Boolean;
    FInUpdateGridModeBufferCount: Boolean;
    FKeyField: TcxDBDataField;
    FKeyFieldNames: string;
    FLoaded: Boolean;
    FMasterDetailKeyFields: TList;
    FMasterDetailKeyValues: Variant;
    FMasterKeyFieldNames: string;
    FResetDBFields: Boolean;
    FUpdateDataSetPos: Boolean;
    FOnDetailHasChildren: TcxDBDataDetailHasChildrenEvent;
    FOnGridModeChanged: TcxDBDataControllerGridModeChangedEvent;
    function AddInternalDBField: TcxDBDataField;
    function GetDataControllerInfo: TcxDBDataControllerInfo;
    function GetDataSet: TDataSet;
    function GetDataSetRecordCount: Integer;
    function GetDataSource: TDataSource;
    function GetDBField(Index: Integer): TcxDBDataField;
    function GetDBSelection: TcxDBDataSelection;
    function GetFilter: TcxDBDataFilterCriteria;
    function GetMasterDetailKeyFieldNames: string;
    function GetMasterDetailKeyFields: TList;
    function GetProvider: TcxDBDataProvider;
    function GetRecNo: Integer;
    procedure MasterDetailKeyFieldsRemoveNotification(AComponent: TComponent);
    procedure RemoveKeyField;
    procedure SetDataModeController(Value: TcxDBDataModeController);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDetailKeyFieldNames(const Value: string);
    procedure SetFilter(Value: TcxDBDataFilterCriteria);
    procedure SetKeyFieldNames(const Value: string);
    procedure SetMasterKeyFieldNames(const Value: string);
    procedure SetRecNo(Value: Integer);
    procedure SyncDataSetPos;
    function SyncMasterDetail: TcxCustomDataController;
    procedure SyncMasterDetailDataSetPos;
    procedure UpdateRelationFields;
  protected
    procedure ActiveChanged(AActive: Boolean); override;
    function CanChangeDetailExpanding(ARecordIndex: Integer; AExpanded: Boolean): Boolean; override;
    function CanFocusRecord(ARecordIndex: Integer): Boolean; override;
    function CanLoadData: Boolean; override;
    procedure CheckDataSetCurrent; override;
    function CheckMasterBrowseMode: Boolean; override;
    procedure ClearMasterDetailKeyFields;
    procedure CorrectAfterDelete(ARecordIndex: Integer); override;
    function CreateDataControllerInfo: TcxCustomDataControllerInfo; override;
    function CreateFindFilterCriteria: TcxDataFindFilterCriteria; override;
    procedure DoDataSetCurrentChanged(AIsCurrent: Boolean); virtual;
    procedure DoDataSourceChanged; virtual;
    procedure DoGridModeChanged; override;
    procedure DoInitInsertingRecord(AInsertingRecordIndex: Integer); virtual;
    function DoSearchInGridMode(const ASubText: string; AForward, ANext: Boolean): Boolean; override;
    function FindRecordByFields(ABufferRecordIndex: Integer; AFields: TList): Integer; override;
    function FindRecordIndexInGridMode(const AKeyFieldValues: Variant): Integer;
    function GetActiveRecordIndex: Integer; override;
    function GetClearDetailsOnCollapse: Boolean; override;
    function GetDataProviderClass: TcxCustomDataProviderClass; override;
    function GetDataRowCount: Integer; override;
    function GetDataSelectionClass: TcxDataSelectionClass; override;
    function GetDefaultGridModeBufferCount: Integer; virtual;
    function GetFieldClass: TcxCustomDataFieldClass; override;
    function GetFilterCriteriaClass: TcxDataFilterCriteriaClass; override;
    function GetFocusedDataRowIndex: Integer; override;
    procedure GetKeyFields(AList: TList); override;
    function GetRelationClass: TcxCustomDataRelationClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean; override;
    procedure InternalClearBookmark; override;
    procedure InternalGotoBookmark; override;
    function InternalSaveBookmark: Boolean; override;
    procedure InvalidateDataBuffer; virtual;
    function IsDataField(AField: TcxCustomDataField): Boolean; override;
    function IsKeyNavigation: Boolean; override;
    function IsOtherDetailChanged: Boolean;
    function IsOtherDetailCreating: Boolean;
    function IsProviderDataSource: Boolean; override;
    function IsSmartRefresh: Boolean; override;
    procedure LoadStorage; override;
    function LocateRecordIndex(AGetFieldsProc: TGetListProc): Integer; virtual;
    function LockOnAfterSummary: Boolean; override;
    procedure NotifyDataControllers; override;
    procedure NotifyDetailAfterFieldsRecreating(ADataController: TcxCustomDataController);
    procedure NotifyDetailsAfterFieldsRecreating(ACreatingLinkObject: Boolean);
    procedure PrepareField(AField: TcxCustomDataField); override;
    procedure RemoveNotification(AComponent: TComponent); override;
    procedure ResetDataSetCurrent(ADataController: TcxCustomDataController);
    procedure ResetDBFields;
    procedure RestructData; override;
    procedure ResyncDBFields;
    procedure RetrieveField(ADataField: TcxDBDataField; AIsLookupKeyOnly: Boolean);
    function TryFocusRecord(ARecordIndex: Integer): Boolean; virtual;
    procedure UpdateEditingRecord;
    procedure UpdateField(ADataField: TcxDBDataField; const AFieldNames: string; AIsLookup: Boolean);
    procedure UpdateFields; override;
    procedure UpdateFocused; override;
    procedure UpdateInternalKeyFields(const AFieldNames: string; var AField: TcxDBDataField);
    procedure UpdateLookupFields;
    procedure UpdateRelations(ARelation: TcxCustomDataRelation); override;
    procedure UpdateScrollBars; virtual;
    // Locate
    procedure BeginReadRecord; override;
    procedure EndReadRecord; override;

    property DBFields[Index: Integer]: TcxDBDataField read GetDBField;
    property DBSelection: TcxDBDataSelection read GetDBSelection;
    property KeyField: TcxDBDataField read FKeyField;
    property MasterDetailKeyFieldNames: string read GetMasterDetailKeyFieldNames;
    property MasterDetailKeyFields: TList read GetMasterDetailKeyFields;
    property MasterDetailKeyValues: Variant read FMasterDetailKeyValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Actions
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    // Structure
    procedure ChangeFieldName(AItemIndex: Integer; const AFieldName: string);
    procedure ChangeValueTypeClass(AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass); override;
    function GetItemByFieldName(const AFieldName: string): TObject;
    function GetItemField(AItemIndex: Integer): TField;
    function GetItemFieldName(AItemIndex: Integer): string;
    function IsDisplayFormatDefined(AItemIndex: Integer; AIgnoreSimpleCurrency: Boolean): Boolean; override;
    procedure Loaded; override;
    // Data
    procedure BeginLocate;
    procedure EndLocate;
    procedure DoUpdateRecord(ARecordIndex: Integer);
    function GetGroupValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    procedure GetKeyDBFields(AList: TList);
    function GetKeyFieldsValues: Variant;
    function GetRecordId(ARecordIndex: Integer): Variant; override;
    procedure UpdateGridModeBufferCount;
    // Data Editing
    procedure CheckBrowseMode; override;
    function DataChangedNotifyLocked: Boolean; override;
    procedure RefreshExternalData; override;
    // Navigation
    procedure SetFocus; override;
    // Bookmark
    function IsBookmarkAvailable: Boolean; override;
    function IsBookmarkRow(ARowIndex: Integer): Boolean; override;
    // Filter
    function GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    function GetFilterItemFieldName(AItem: TObject): string; override;
    // Search
    function FindRecordIndexByKey(const AKeyFieldValues: Variant): Integer;
    function LocateByKey(const AKeyFieldValues: Variant): Boolean;
    // Master-Detail
    procedure CheckCurrentQuery;
    function GetDetailFilterAdapter: TcxDBProviderDetailFilterAdapter; virtual;
    procedure SetMasterRelation(AMasterRelation: TcxCustomDataRelation; AMasterRecordIndex: Integer); override;
    // MultiSelect in GridMode
    function GetRowId(ARowIndex: Integer): Variant; override;
    function GetSelectedBookmark(Index: Integer): TcxBookmark;
    function GetSelectedRowIndex(Index: Integer): Integer; override;
    function GetSelectionAnchorBookmark: TcxBookmark;
    function GetSelectionAnchorRowIndex: Integer; override;
    function IsSelectionAnchorExist: Boolean; override;
    procedure SelectAll; override;
    procedure SelectFromAnchor(ARowIndex: Integer; AKeepSelection: Boolean); override;
    procedure SetSelectionAnchor(ARowIndex: Integer); override;
    // Export
    function FocusSelectedRow(ASelectedIndex: Integer): Boolean; override;
    // View Data
    procedure ForEachRow(ASelectedRows: Boolean; AProc: TcxDataControllerEachRowProc); override;
    function IsSequenced: Boolean;

    property DataControllerInfo: TcxDBDataControllerInfo read GetDataControllerInfo;
    property DataModeController: TcxDBDataModeController read FDataModeController write SetDataModeController;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DetailKeyFieldNames: string read FDetailKeyFieldNames write SetDetailKeyFieldNames;
    property Filter: TcxDBDataFilterCriteria read GetFilter write SetFilter;
    property KeyFieldNames: string read FKeyFieldNames write SetKeyFieldNames;
    property MasterKeyFieldNames: string read FMasterKeyFieldNames write SetMasterKeyFieldNames;
    property Provider: TcxDBDataProvider read GetProvider;
    property RecNo: Integer read GetRecNo write SetRecNo; // Sequenced
    property DataSetRecordCount: Integer read GetDataSetRecordCount; // Sequenced
    property OnDetailHasChildren: TcxDBDataDetailHasChildrenEvent read FOnDetailHasChildren write FOnDetailHasChildren;
    property OnGridModeChanged: TcxDBDataControllerGridModeChangedEvent read FOnGridModeChanged write FOnGridModeChanged;
  end;

  { TcxDataSetBookmark }

  TcxDataSetBookmark = class
  public
    SaveActiveRecord: Integer;
    SaveBOF: Boolean;
    SaveBookmark: TcxBookMark;
    SaveEOF: Boolean;
  end;

var
  cxDetailFilterControllers: TcxDBAdapterList;

function CanCallDataSetLocate(ADataSet: TDataSet; const AKeyFieldNames: string;
  const AValue: Variant): Boolean;
function GetValueTypeClassByField(AField: TField): TcxValueTypeClass;

function cxDataSetCreateBookmark(ADataSet: TDataSet): TcxDataSetBookmark;
procedure cxDataSetRestoreBookmark(ADataSet: TDataSet; var ABookmark: TcxDataSetBookmark);

implementation

uses
  Windows, TypInfo, RTLConsts, Contnrs, cxDataConsts, dxCoreClasses;

type
  TDataSetAccess = class(TDataSet);

var
  DBDataProviders: TList;
  DBDataLinks: TList;

function cxDataSetCreateBookmark(ADataSet: TDataSet): TcxDataSetBookmark;
begin
  Result := TcxDataSetBookmark.Create;
  with TDataSetAccess(ADataSet) do
  begin
    if BookmarkAvailable then
      Result.SaveBookmark := Bookmark;
    Result.SaveActiveRecord := ActiveRecord;
    Result.SaveBOF := BOF;
    Result.SaveEOF := EOF;
  end;
end;

procedure cxDataSetRestoreBookmark(ADataSet: TDataSet; var ABookmark: TcxDataSetBookmark);
begin
  try
    with TDataSetAccess(ADataSet) do
    begin
      if (Length(ABookmark.SaveBookmark) > 0) and BookmarkValid(TBookmark(ABookmark.SaveBookmark)) then
      try
        Bookmark := ABookmark.SaveBookmark;
        if ActiveRecord > ABookmark.SaveActiveRecord then
        begin
          MoveBy(BufferCount - ActiveRecord - 1 + ActiveRecord - ABookmark.SaveActiveRecord);
          MoveBy(ABookmark.SaveActiveRecord - BufferCount + 1);
        end
        else
          if ActiveRecord < ABookmark.SaveActiveRecord then
          begin
            MoveBy(-ActiveRecord + ActiveRecord - ABookmark.SaveActiveRecord);
            MoveBy(ABookmark.SaveActiveRecord);
          end;
        if ABookmark.SaveBOF and not BOF then Prior;
        if ABookmark.SaveEOF and not EOF then Next;
      except
        on EDatabaseError do;
      end;
    end;
  finally
    FreeAndNil(ABookmark);
  end;
end;

procedure GetInternalKeyFields(ADataField: TcxDBDataField; AList: TList);
var
  I: Integer;
begin
  if Assigned(ADataField) then
  begin
    if ADataField.FieldCount = 0 then
      AList.Add(ADataField)
    else
      for I := 0 to ADataField.FieldCount - 1 do
        AList.Add(ADataField.Fields[I]);
  end;
end;

function CanCallDataSetLocate(ADataSet: TDataSet; const AKeyFieldNames: string;
  const AValue: Variant): Boolean;

  function TryGetFieldList(ADataSet: TDataSet; const AFieldNames: string; AList: TList): Boolean;
  var
    AField: TField;
    APos: Integer;
  begin
    Result := True;
    APos := 1;
    while APos <= Length(AFieldNames) do
    begin
      AField := ADataSet.FindField(ExtractFieldName(AFieldNames, APos));
      Result := AField <> nil;
      if not Result then
        Break;
      AList.Add(AField);
    end;
  end;

  function IsNullValidToLocate(AField: TField): Boolean;
  begin
    Result := not (AField is TAutoIncField);
  end;

var
  AArrayLowBound, I: Integer;
  AField: TField;
  AFieldList: TObjectList;
begin
  if VarIsArray(AValue) then
  begin
    AFieldList := TObjectList.Create(False);
    try
      AArrayLowBound := VarArrayLowBound(AValue, 1);
      Result := TryGetFieldList(ADataSet, AKeyFieldNames, AFieldList) and
        (AFieldList.Count = VarArrayHighBound(AValue, 1) - AArrayLowBound + 1);
      if Result then
        for I := 0 to AFieldList.Count - 1 do
        begin
          Result := not VarIsNull(AValue[I + AArrayLowBound]) or
            IsNullValidToLocate(TField(AFieldList[I]));
          if not Result then
            Break;
        end;
    finally
      AFieldList.Free;
    end;
  end
  else
  begin
    AField := nil;
    if Pos(';', AKeyFieldNames) = 0 then
      AField := ADataSet.FindField(AKeyFieldNames);
    Result := (AField <> nil) and (not VarIsNull(AValue) or
      IsNullValidToLocate(AField));
  end;
end;

function GetValueTypeClassByField(AField: TField): TcxValueTypeClass;
begin
  if AField = nil then
    Result := TcxStringValueType
  else
  begin
    case AField.DataType of
      ftString, ftMemo, ftFmtMemo, ftFixedChar:
        Result := TcxStringValueType;
      ftWideString, ftWideMemo, ftFixedWideChar:
        Result := TcxStringValueType;
      ftSmallint:
        Result := TcxSmallintValueType;
      ftInteger, ftAutoInc:
        Result := TcxIntegerValueType;
      ftWord:
        Result := TcxWordValueType;
      ftBoolean:
        Result := TcxBooleanValueType;
      TFieldType.ftSingle:
        Result := TcxSingleValueType;
      ftCurrency, ftFloat:
        Result := TcxFloatValueType;
      {ftCurrency, }ftBCD:
        Result := TcxCurrencyValueType;
      ftDate, ftTime, ftDateTime:
        Result := TcxDateTimeValueType;
      ftFMTBcd:
        Result := TcxFMTBcdValueType;
      ftLargeint:
        Result := TcxLargeIntValueType;
      ftTimeStamp:
        Result := TcxSQLTimeStampValueType;
      ftBlob:
        Result := TcxBLOBValueType;
    else
      Result := TcxVariantValueType;
    end;
  end;
end;

{ TcxDBProviderDetailFilterAdapter }

function TcxDBProviderDetailFilterAdapter.IsCurrentQuery(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant): Boolean;
var
  AParams: TParams;
begin
  Result := False;
  if IsPublishedProp(ADataSet, 'Params') then
  begin
    AParams := GetObjectProp(ADataSet, 'Params') as TParams;
    if AParams <> nil then
    begin
      if VarEquals(AParams.ParamValues[AParamNames], AParamValues) then
        Result := True;
    end;
  end;
end;

procedure TcxDBProviderDetailFilterAdapter.ReopenSQL(ADataSet: TDataSet;
  const AParamNames: string; const AParamValues: Variant; var AReopened: Boolean);
var
  AParams: TParams;
begin
  if IsPublishedProp(ADataSet, 'Params') then
  begin
    AParams := GetObjectProp(ADataSet, 'Params') as TParams;
    if AParams <> nil then
    begin
      if VarEquals(AParams.ParamValues[AParamNames], AParamValues) then
        ADataSet.First // TODO: Update method - for Query with Post
      else
      begin
        ADataSet.DisableControls;
        try
          ADataSet.Active := False;
          AParams.ParamValues[AParamNames] := AParamValues;
          ADataSet.Active := True;
        finally
          ADataSet.EnableControls;
        end;
        AReopened := True; // set Flag if Query reopened
      end;
    end;
  end;
end;

procedure TcxDBProviderDetailFilterAdapter.SetFilter(ADataSet: TDataSet;
  const AMasterDetailKeyFieldNames: string; const AMasterDetailKeyValues: Variant;
  AIsSQLMode: Boolean; var AReopened: Boolean);
begin
  if AIsSQLMode then
    ReopenSQL(ADataSet, AMasterDetailKeyFieldNames, AMasterDetailKeyValues, AReopened)
  else
    ADataSet.Locate(AMasterDetailKeyFieldNames, AMasterDetailKeyValues, []); // must be sort by Detail Keys
end;

{ TcxDBDataModeController }

constructor TcxDBDataModeController.Create(ADataController: TcxDBDataController);
begin
  inherited Create;
  FDetailInSyncMode := True;
  FDataController := ADataController;
  FGridModeBufferCount := 0;
  FSyncInsert := True;
  FSyncMode := True;
end;

procedure TcxDBDataModeController.Assign(Source: TPersistent);
begin
  if Source is TcxDBDataModeController then
  begin
    DetailInSQLMode := TcxDBDataModeController(Source).DetailInSQLMode;
    DetailInSyncMode := TcxDBDataModeController(Source).DetailInSyncMode;
    GridMode := TcxDBDataModeController(Source).GridMode;
    GridModeBufferCount := TcxDBDataModeController(Source).GridModeBufferCount;
    SmartRefresh := TcxDBDataModeController(Source).SmartRefresh;
    SyncInsert := TcxDBDataModeController(Source).SyncInsert;
    SyncMode := TcxDBDataModeController(Source).SyncMode;
    OnDetailFirst := TcxDBDataModeController(Source).OnDetailFirst;
    OnDetailIsCurrentQuery := TcxDBDataModeController(Source).OnDetailIsCurrentQuery;
  end
  else
    inherited Assign(Source);
end;

function TcxDBDataModeController.DetailIsCurrentQuery(const AMasterDetailKeyFieldNames: string;
  const AMasterDetailKeyValues: Variant): Boolean;
var
  ADetailFilterAdapter: TcxDBProviderDetailFilterAdapter;
begin
  if Assigned(FOnDetailIsCurrentQuery) then
    Result := FOnDetailIsCurrentQuery(Self, DataController.DataSet,
      AMasterDetailKeyFieldNames, AMasterDetailKeyValues)
  else
  begin
    ADetailFilterAdapter := DataController.GetDetailFilterAdapter;
    Result := ADetailFilterAdapter.IsCurrentQuery(DataController.DataSet,
      AMasterDetailKeyFieldNames, AMasterDetailKeyValues);
  end;
end;

procedure TcxDBDataModeController.DoDetailFirst(const AMasterDetailKeyFieldNames: string;
  const AMasterDetailKeyValues: Variant; var AReopened: Boolean);
var
  ADetailFilterAdapter: TcxDBProviderDetailFilterAdapter;
begin
  AReopened := False;
  if Assigned(FOnDetailFirst) then
    FOnDetailFirst(Self, DataController.DataSet, AMasterDetailKeyFieldNames,
      AMasterDetailKeyValues, AReopened)
  else
  begin
    ADetailFilterAdapter := DataController.GetDetailFilterAdapter;
    ADetailFilterAdapter.SetFilter(DataController.DataSet, AMasterDetailKeyFieldNames,
      AMasterDetailKeyValues, DetailInSQLMode, AReopened);
  end;
end;

procedure TcxDBDataModeController.SetGridMode(Value: Boolean);
var
  APrevGridMode: Boolean;
begin
  APrevGridMode := DataController.IsGridMode;
  if FGridMode <> Value then
  begin
    FGridMode := Value;
    if DataController.IsGridMode <> APrevGridMode then
    begin
      DataController.Change([dccGridMode]);
      DataController.RestructData;
    end;
  end;
end;

procedure TcxDBDataModeController.SetGridModeBufferCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FGridModeBufferCount <> Value then
  begin
    FGridModeBufferCount := Value;
    DataController.UpdateGridModeBufferCount;
  end;
end;

procedure TcxDBDataModeController.SetSmartRefresh(Value: Boolean);
begin
  if FSmartRefresh <> Value then
  begin
    FSmartRefresh := Value;
    if not DataController.Provider.IsGridMode then
      DataController.Provider.DataScrolled(0);
  end;
end;

procedure TcxDBDataModeController.SetSyncMode(Value: Boolean);
begin
  if FSyncMode <> Value then
  begin
    FSyncMode := Value;
    if DataController.Provider.IsActive then
      DataController.Provider.DataScrolled(0);
  end;
end;

{ TcxBookmarkList }

constructor TcxBookmarkList.Create;
begin
  inherited Create;
  SetLength(FItems, 0);
end;

destructor TcxBookmarkList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TcxBookmarkList.Add(AItem: TcxBookmark);
begin
  Insert(Count, AItem);
end;

procedure TcxBookmarkList.Clear;
begin
  if Length(FItems) = 0 then Exit;
  SetLength(FItems, 0);
end;

procedure TcxBookmarkList.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EListError.Create(SListIndexError);
  FItems[AIndex] := nil;
  if AIndex < Count - 1 then
  begin
    System.Move(FItems[AIndex + 1], FItems[AIndex],
      (Count - AIndex - 1) * SizeOf(Pointer));
    PPointer(@FItems[Count - 1])^ := nil;
  end;
  SetLength(FItems, Count - 1);
end;

function TcxBookmarkList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TcxBookmarkList.GetItem(AIndex: Integer): TcxBookmark;
begin
  Result := FItems[AIndex];
end;

procedure TcxBookmarkList.Insert(AIndex: Integer; AItem: TcxBookmark);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EListError.Create(SListIndexError);
  SetLength(FItems, Count + 1);
  if AIndex < Count - 1 then
  begin
    Move(FItems[AIndex], FItems[AIndex + 1],
      (Count - AIndex - 1) * SizeOf(Pointer));
    PPointer(@FItems[AIndex])^ := nil;
  end;
  FItems[AIndex] := AItem;
end;

procedure TcxBookmarkList.QuickSort(L, R: Integer; ACompareFunc: TcxCompareBookmarksFunc);
var
  I, J: Integer;
  APivot, ATemp: TcxBookmark;
begin
  repeat
    I := L;
    J := R;
    APivot := FItems[(L + R) shr 1];
    repeat
      while ACompareFunc(FItems[I], APivot) < 0 do
        Inc(I);
      while ACompareFunc(FItems[J], APivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          ATemp := FItems[I];
          FItems[I] := FItems[J];
          FItems[J] := ATemp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, ACompareFunc);
    L := I;
  until I >= R;
end;

{ TcxDBDataSelection }

constructor TcxDBDataSelection.Create(ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FBookmarks := TcxBookmarkList.Create;
end;

destructor TcxDBDataSelection.Destroy;
begin
  FBookmarks.Free;
  FBookmarks := nil;
  inherited Destroy;
end;

function TcxDBDataSelection.FindByRowIndex(ARowIndex: Integer; var AIndex: Integer): Boolean;
begin
  if DataController.IsGridMode then
    Result := FindBookmark(GetRowBookmark(ARowIndex), AIndex)
  else
    Result := inherited FindByRowIndex(ARowIndex, AIndex);
end;

procedure TcxDBDataSelection.SelectAll;
begin
  if DataController.IsGridMode and Assigned(DataController.DataSet) then
  begin
    with DataController.Provider do
    begin
      Freeze;
      try
        SavePos;
        First;
        InternalClear;
        try
          while not IsEOF do
          begin
            if DataController.CanSelectRow(-1) then
              FBookmarks.Add(DataSet.Bookmark);
            Next;
          end;
          SyncCount;
        except
          FBookmarks.Clear;
          SyncCount;
          raise;
        end;
        RestorePos;
      finally
        Unfreeze;
        Changed;
        DataController.DataControllerInfo.CheckChanges;
        DataController.SyncMasterPos;
      end;
    end;
  end;
end;

procedure TcxDBDataSelection.SelectFromAnchor(AToBookmark: TcxBookmark; AKeepSelection: Boolean);

  function CheckBookmarks(const ABkm1, ABkm2: TcxBookmark; ADirectionDown: Boolean): Boolean;
  begin
    Result := False;
    if AKeepSelection or (FBookmarks.Count = 0) then Exit;
    with DataController.Provider do
    begin
      if CompareBookmarks(ABkm1, ABkm2) = 0 then
      begin
        FBookmarks.Clear;
        if DataController.CanSelectRow(-1) then
          FBookmarks.Add(ABkm1);
        Result := True;
      end
      else
        if ADirectionDown then
        begin
          if (FBookmarks.Count > 1) and
            (CompareBookmarks(FBookmarks[FBookmarks.Count - 2], ABkm2) = 0) then // dec last
          begin
            if CompareBookmarks(FBookmarks[0], ABkm1) = 0 then
            begin
              FBookmarks.Delete(FBookmarks.Count - 1);
              Result := True;
            end;
          end
          else
          begin
            DataSet.Bookmark := ABkm2;
            DataSet.Prior;
            if CompareBookmarks(FBookmarks[FBookmarks.Count - 1], DataSet.Bookmark) = 0 then // inc
            begin
              if CompareBookmarks(FBookmarks[0], ABkm1) = 0 then
              begin
                DataSet.Bookmark := ABkm2; // for OnCanSelectRow event
                if DataController.CanSelectRow(-1) then
                  FBookmarks.Add(ABkm2);
                Result := True;
              end;
            end;
          end;
        end
        else
        begin
          if (FBookmarks.Count > 1) and
            (CompareBookmarks(FBookmarks[1], ABkm1) = 0) then // dec top
          begin
            if CompareBookmarks(FBookmarks[FBookmarks.Count - 1], ABkm2) = 0 then
            begin
              FBookmarks.Delete(0);
              Result := True;
            end;
          end
          else
          begin
            DataSet.Bookmark := ABkm1;
            DataSet.Next;
            if CompareBookmarks(FBookmarks[0], DataSet.Bookmark) = 0 then // inc
            begin
              if CompareBookmarks(FBookmarks[FBookmarks.Count - 1], ABkm2) = 0 then
              begin
                DataSet.Bookmark := ABkm1; // for OnCanSelectRow event
                if DataController.CanSelectRow(-1) then
                  FBookmarks.Insert(0, ABkm1);
                Result := True;
              end;
            end;
          end;
        end;
    end;
  end;

  procedure LoadBookmarks(const ABkm1, ABkm2: TcxBookmark);
  var
    ABkm: TcxBookmark;
    ACheckNeeded, AFound: Boolean;
    I, C, AIndex1, AIndex2: Integer;
  begin
    with DataController.Provider do
    begin
      if not AKeepSelection then
        FBookmarks.Clear;
      try
        ACheckNeeded := FBookmarks.Count > 0;
        if ACheckNeeded then
        begin
          FindBookmark(ABkm1, AIndex1);
          FindBookmark(ABkm2, AIndex2);
          if AIndex1 = FBookmarks.Count then
            ACheckNeeded := False;
        end;
        DataSet.Bookmark := ABkm1;
        while not IsEOF do
        begin
          ABkm := DataSet.Bookmark;
          if ACheckNeeded then
          begin
            AFound := False;
            for I := AIndex1 to AIndex2 do
            begin
              if I >= FBookmarks.Count then
                Break;
              C := -CompareBookmarks(FBookmarks[I], ABkm);
              if C < 0 then
              begin
                if DataController.CanSelectRow(-1) then
                  FBookmarks.Insert(I, ABkm);
                Inc(AIndex1);
                Inc(AIndex2);
                AFound := True;
                Break;
              end
              else
                if C = 0 then
                begin
                  Inc(AIndex1);
                  AFound := True;
                  Break;
                end;
            end;
            if not AFound then
            begin
              if DataController.CanSelectRow(-1) then
                FBookmarks.Add(ABkm);
            end;
          end
          else
          begin
            if DataController.CanSelectRow(-1) then
              FBookmarks.Add(ABkm);
          end;
          if CompareBookmarks(ABkm, ABkm2) = 0 then
            Break;
          Next;
        end;
      except
        FBookmarks.Clear;
        SyncCount;
        raise;
      end;
    end;
  end;

var
  ABkm1, ABkm2: TcxBookmark;
  ADirectionDown: Boolean;
begin
  // TODO: optimize - Do Selection (up/down)
  if DataController.IsGridMode and Assigned(DataController.DataSet) then
  begin
    with DataController.Provider do
    begin
      if (DataSet.State = dsInsert) and DataSet.Eof then
        Exit;
      Freeze;
      try
        SavePos;
        ABkm1 := FAnchorBookmark;
        ABkm2 := AToBookmark;
        ADirectionDown := CompareBookmarks(ABkm1, ABkm2) < 0;
        if not ADirectionDown then
        begin
          ABkm2 := FAnchorBookmark;
          ABkm1 := AToBookmark;
        end;
        // check - select shift + up/down
        if not CheckBookmarks(ABkm1, ABkm2, ADirectionDown) then
          LoadBookmarks(ABkm1, ABkm2);
        SyncCount;
        RestorePos;
      finally
        Unfreeze;
        Changed;
        DataController.DataControllerInfo.CheckChanges;
        DataController.SyncMasterPos;
      end;
    end;
  end;
end;

procedure TcxDBDataSelection.ClearAnchor;
begin
  inherited ClearAnchor;
  SetLength(FAnchorBookmark, 0);
end;

function TcxDBDataSelection.CompareBookmarks(const AItem1, AItem2: TcxBookmark): Integer;
begin
  Result := DataController.Provider.CompareBookmarks(AItem1, AItem2);
end;

procedure TcxDBDataSelection.InternalAdd(AIndex, ARowIndex, ARecordIndex, ALevel: Integer);
begin
  inherited InternalAdd(AIndex, ARowIndex, ARecordIndex, ALevel);
  if not DataController.IsGridMode or FInSelectAll then Exit;
  FBookmarks.Insert(AIndex, GetRowBookmark(ARowIndex));
end;

procedure TcxDBDataSelection.InternalClear;
begin
  inherited InternalClear;
  if FBookmarks <> nil then
    FBookmarks.Clear;
end;

procedure TcxDBDataSelection.InternalDelete(AIndex: Integer);
begin
  inherited InternalDelete(AIndex);
  if not DataController.IsGridMode or FInSelectAll then Exit;
  if CompareBookmarks(FBookmarks[AIndex], FAnchorBookmark) = 0 then
    ClearAnchor;
  FBookmarks.Delete(AIndex);
end;

function TcxDBDataSelection.FindBookmark(const ABookmark: TcxBookmark; var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  AIndex := 0;
  Result := False;
  L := 0;
  H := FBookmarks.Count - 1;
  if L <= H then
    repeat
      I := (L + H) div 2;
      C := CompareBookmarks(FBookmarks[I], ABookmark);
      if C = 0 then
      begin
        AIndex := I;
        Result := True;
        Break;
      end
      else
        if C < 0 then
          L := I + 1
        else
          H := I - 1;
      if L > H then
      begin
        AIndex := L;
        Break;
      end;
    until False;
end;

function TcxDBDataSelection.GetRowBookmark(ARowIndex: Integer): TcxBookmark;
var
  APrevActiveRecord: Integer;
begin
  SetLength(Result, 0);
  if not DataController.IsGridMode then Exit;
  APrevActiveRecord := DataController.Provider.DataLink.ActiveRecord;
  try
    DataController.Provider.DataLink.ActiveRecord := ARowIndex;
    Result := DataController.DataSet.Bookmark;
  finally
    DataController.Provider.DataLink.ActiveRecord := APrevActiveRecord;
  end;
end;

function TcxDBDataSelection.RefreshBookmarks: Boolean;
var
  I: Integer;
  ADataSet: TDataSet;
begin
  Result := False;
  if not DataController.IsGridMode then Exit;
  ADataSet := DataController.DataSet;
  if Assigned(ADataSet) then
  begin
    if (Length(FAnchorBookmark) <> 0) and
      not DataController.Provider.BookmarkValid(FAnchorBookmark) then
    begin
      Result := True;
      ClearAnchor;
    end;
    for I := Count - 1 downto 0 do
      if (Length(FBookmarks[I]) = 0) or
        not DataController.Provider.BookmarkValid(FBookmarks[I]) then
      begin
        Result := True;
        Delete(I);
      end;
    EnsureBookmarkListOrder;
  end;
end;

procedure TcxDBDataSelection.SyncCount;
begin
  FInSelectAll := True;
  try
    SetInternalCount(FBookmarks.Count);
  finally
    FInSelectAll := False;
  end;
end;

procedure TcxDBDataSelection.EnsureBookmarkListOrder;
begin
  if FBookmarks.Count > 1 then
    FBookmarks.QuickSort(0, FBookmarks.Count - 1, CompareBookmarks);
end;

function TcxDBDataSelection.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

{ TcxDBCustomDataLink }

constructor TcxDBCustomDataLink.Create;
begin
  inherited Create;
  if DBDataLinks = nil then
    DBDataLinks := TList.Create;
  DBDataLinks.Add(Self);
end;

destructor TcxDBCustomDataLink.Destroy;
begin
  if DBDataLinks <> nil then
    DBDataLinks.Remove(Self);
  inherited Destroy;
end;

function TcxDBCustomDataLink.GetIsDataSetBusyState: Boolean;
begin
  Result := False;
end;

function TcxDBCustomDataLink.GetIsDataSetBusy: Boolean;
var
  I: Integer;
  ADataLink: TcxDBCustomDataLink;
begin
  Result := False;
  for I := 0 to DBDataLinks.Count - 1 do
  begin
    ADataLink := TcxDBCustomDataLink(DBDataLinks[I]);
    Result := (ADataLink.DataSet = DataSet) and ADataLink.GetIsDataSetBusyState;
    if Result then Break;
  end;
end;

{ TcxDBDataLink }

constructor TcxDBDataLink.Create(AProvider: TcxDBDataProvider);
begin
  inherited Create;
  FProvider := AProvider;
  VisualControl := True;
end;

procedure TcxDBDataLink.ActiveChanged;
begin
  Provider.ActiveChanging := True;
  try
    Provider.ActiveChanged(Active);
    if Provider.DataController.IsLoading then
      Provider.DataController.FLoaded := True;
  finally
    Provider.ActiveChanging := False;
  end;
  Provider.FInSetDataSource := Assigned(DataSet) and (DataSet.State = dsInsert);
  if not Active then
    FLayoutChangedFlag := False;
end;

procedure TcxDBDataLink.DataEvent(Event: TDataEvent;
  Info: TdxNativeInt);
begin
  inherited DataEvent(Event, Info);
  if (Event = deDisabledStateChange) and not Boolean(Info) then
    FLayoutChangedFlag := True;
end;

procedure TcxDBDataLink.DataSetChanged;
begin
  if FLayoutChangedFlag then
  begin
    LayoutChanged;
    Exit;
  end;
  if Provider.IsOtherDataControllerUpdating then Exit;
  if Provider.IsOtherDetailChanged then Exit;
{
  if Provider.IsGridMode and Provider.DataController.IsDetailMode and
    not VarEquals(GetDataSetValues(DataSet, Provider.DataController.MasterDetailKeyFields),
      Provider.DataController.MasterDetailKeyValues) then
    Exit;
}
  if Provider.IsDataSetBusy then
    DataSetScrolled(0)
  else
  begin
    if Provider.IsGridMode then
    begin
      Provider.FInUpdateGridMode := True;
      try
        Provider.DataController.BeginFullUpdate;
        try
          Provider.UpdateGridMode;
          if Provider.DataController.MultiSelect then
            Provider.DataController.DBSelection.RefreshBookmarks;
          Provider.DataController.CheckBookmarkValid(-1);
          if DataSet.State = dsInsert then
            DoInsertingRecord;
        finally
          Provider.DataController.EndFullUpdate;
        end;
        Provider.CheckExpandedDetails;
      finally
        Provider.FInUpdateGridMode := False;
      end;
//      if DataSet.State = dsInsert then
//        Provider.InsertingRecord(Provider.IsEOF);
    end
    else
    begin
      if not (DataSet.State in dsEditModes) then
      begin
        if Provider.IsSmartRefresh then
        begin
          Provider.SmartDataChanged;
          DataSetScrolled(0);
        end
        else
        begin
          Provider.DataController.BeginFullUpdate;
          try
            Provider.DataChanged(dcTotal, -1, -1);
            DataSetScrolled(0);
          finally
            Provider.DataController.EndFullUpdate;
          end;
        end;
        Provider.ResetSmartRefresh;
      end
      else
        if (DataSet.State = dsInsert) and not Provider.FInserting then
        begin
          if (not Provider.DataController.IsDetailMode or Provider.IsDataSetCurrent) and
            Provider.IsSyncInsert and not Provider.FInInserting then
            DoInsertingRecord;
        end;
    end;
  end;
end;

procedure TcxDBDataLink.DataSetScrolled(Distance: Integer);
begin
  Provider.DataScrolled(Distance);
end;

procedure TcxDBDataLink.EditingChanged;
begin
  if not Provider.IsSyncMode then Exit;
  if not Active or not (DataSet.State in dsEditModes) then
  begin
    if {not Provider.IsGridMode and} Provider.IsSmartRefresh and
      (Provider.EditingRecordIndex <> cxNullEditingRecordIndex) and
      (Provider.EditingRecordIndex = Provider.DataController.NewItemRecordIndex) then
    begin
      if Provider.FUpdatingRecordIsInserting then
      begin
        Provider.DataController.FInUnboundCopy := True; // !!!
        try
          Provider.FUpdatingNewRecordIndex := Provider.DataController.AppendRecord;
          Provider.DataController.CopyRecord(Provider.DataController.NewItemRecordIndex,
            Provider.FUpdatingNewRecordIndex);
        finally
          Provider.DataController.FInUnboundCopy := False;
        end;
      end;
    end;
    Provider.ResetEditing;
  end
  else
  begin
    if DataSet.State = dsEdit then
    begin
      Provider.DataController.FInUnboundCopy := TRue;
      try
        Provider.EditingRecord;
      finally
        Provider.DataController.FInUnboundCopy := False;
      end;
    end
    else
      if (DataSet.State = dsInsert) and not DataSet.ControlsDisabled and
        (Provider.FInSetDataSource or Provider.DataController.FInUpdateGridModeBufferCount) then
      begin
        if Provider.IsGridMode and not Provider.FInInsert then
          DoInsertingRecord;
      end;
  end;
  Provider.FInSetDataSource := False;
end;


procedure TcxDBDataLink.FocusControl(Field: TFieldRef);

  function FindItemIndex(AField: TField): Integer;
  var
    I: Integer;
    ADataField: TcxDBDataField;
  begin
    Result := -1;
    for I := 0 to Provider.DataController.Fields.Count - 1 do
    begin
      ADataField := Provider.DataController.DBFields[I];
      if not ADataField.IsInternal and (ADataField.Field = AField) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;

var
  ADone: Boolean;
  AItemIndex: Integer;
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    ADone := False;
    AItemIndex := FindItemIndex(Field^);
    if (AItemIndex <> -1) and (AItemIndex < Provider.DataController.GetItemCount) then
    begin
      Provider.DataController.FocusControl(AItemIndex, ADone);
      if ADone then
        Field^ := nil;
    end;
  end;
end;

function TcxDBDataLink.GetIsDataSetBusyState: Boolean;
begin
  Result := Provider.IsDataSetBusy;
end;

procedure TcxDBDataLink.LayoutChanged;
begin
  // bug in Delphi 7 with update pack 1
  if (DataSet.State = dsInsert) or
    (Provider.IsSmartRefresh and FPreventLayoutChanged) then
  begin
    FLayoutChangedFlag := False;
    DataSetChanged;
    Exit;
  end;
  Provider.LayoutChanged([lcStructure, lcData]);
  FLayoutChangedFlag := False;
end;

procedure TcxDBDataLink.RecordChanged(Field: TField);
begin
  Provider.RecordChanged(Field);
end;

procedure TcxDBDataLink.UpdateData;
begin
  Provider.DoUpdateData;
  // A2136!!!
//  if Provider.IsSmartRefresh then
    RecordChanged(nil);
  Provider.ResetChanging;
end;

procedure TcxDBDataLink.DoInsertingRecord;
begin
  Provider.FInInserting := True;
  try
    Provider.InsertingRecord(Provider.IsEOF and not Provider.FInsertOperation);
  finally
    Provider.FInInserting := False;
  end;
end;

{ TcxDBDataAssignedFieldNames }

constructor TcxDBDataAssignedFields.Create;
begin
  inherited;
  FFields := TcxDBFieldList.Create;
end;

destructor TcxDBDataAssignedFields.Destroy;
begin
  FFields.Free;
  FFields := nil;
  inherited Destroy;
end;

procedure TcxDBDataAssignedFields.AddField(AField: TField);
begin
  if FFields.IndexOf(AField) = -1 then
    FFields.Add(AField);
end;

procedure TcxDBDataAssignedFields.AddFieldsByFieldList(AFieldList: TList);
var
  I: Integer;
begin
  for I := 0 to AFieldList.Count - 1 do
    if AFieldList[I] <> nil then
      FFields.Add(AFieldList[I]);
end;

procedure TcxDBDataAssignedFields.AddFieldsByFieldNames(ADataSet: TDataSet; const AFieldNames: string);
begin
  ADataSet.GetFieldList(FFields, AFieldNames);
end;

procedure TcxDBDataAssignedFields.Clear;
begin
  FFields.Clear;
end;

function TcxDBDataAssignedFields.IsEmpty: Boolean;
begin
  Result := FFields.Count = 0;
end;

function TcxDBDataAssignedFields.IsFieldExists(AField: TField): Boolean;
begin
  Result := FFields.IndexOf(AField) <> -1;
end;

{ TcxDBDataProvider }

constructor TcxDBDataProvider.Create(ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FAssignedFields := TcxDBDataAssignedFields.Create;
  if DBDataProviders = nil then
    DBDataProviders := TList.Create;
  DBDataProviders.Add(Self);
  FDataLink := TcxDBDataLink.Create(Self);
  FUpdatingRecordIndex := cxNullEditingRecordIndex;
  FUpdatingNewRecordIndex := -1;
  FListeners := TList.Create;
end;

destructor TcxDBDataProvider.Destroy;
begin
  RemoveFromListeners;
  FListeners.Free;
  FListeners := nil;
  FDataLink.Free;
  FDataLink := nil;
  DBDataProviders.Remove(Self);
  if DBDataProviders.Count = 0 then
  begin
    DBDataProviders.Free;
    DBDataProviders := nil;
  end;
  FAssignedFields.Free;
  FAssignedFields := nil;
  inherited Destroy;
end;

function TcxDBDataProvider.IsCanInitEditingBusy: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider.DataSet = Self.DataSet) and
      ADBDataProvider.FInCanInitEditing then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsDataSetBusy: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if {(ADBDataProvider <> Self) and}
      (ADBDataProvider.DataSet = Self.DataSet) and
       ((ADBDataProvider.LockCount <> 0) or ADBDataProvider.FInNotify or
        ((Self.IsKeyNavigation or Self.DataController.IsGridMode) and (ADBDataProvider.LocateCount <> 0))) then
    begin
      if ADBDataProvider.FInNotify then
      begin
        if not FPostLocateFlag then
          Result := True;
      end
      else
        if not FPostLocateFlag then
        begin
          Result := True;
          if Self.DataController.IsGridMode and (Self.DataController.LockGridModeNotifyCount <> 0) then
            Result := False;
        end;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsDetailDataSetBusy: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider.DataSet = Self.DataSet) and
      ((ADBDataProvider.LockCount <> 0){ or ADBDataProvider.FInFirst}) and
      ADBDataProvider.DataController.IsDetailMode {ADBDataProvider.FInFirst} then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsOtherDataControllerUpdating: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider <> Self) and
      (ADBDataProvider.DataSet = Self.DataSet) and
      ((ADBDataProvider.LockCount <> 0) or
       (Self.IsKeyNavigation and (ADBDataProvider.LocateCount <> 0))) then
    begin
      if ADBDataProvider.DataController.LockGridModeNotifyCount = 0 then
        ADBDataProvider.AddListener(Self);
      Result := True;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsOtherDetailDataSetInInsert: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider <> Self) and (ADBDataProvider.DataSet = Self.DataSet) and
      ADBDataProvider.FInInsert then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsOtherDetailChanged: Boolean;
begin
  Result := DataController.IsOtherDetailChanged;
end;

function TcxDBDataProvider.IsUniDirectional: Boolean;
begin
  if Assigned(DataSet) and DataSet.IsUniDirectional then
    Result := True
  else
    Result := False;
end;

function TcxDBDataProvider.IsCustomDataSourceSupported: Boolean;
begin
  Result := False;
end;

function TcxDBDataProvider.IsGridMode: Boolean;
begin
  Result := DataController.DataModeController.GridMode and
    not IsUniDirectional and not FLoadAllNeeded and
    {not DataController.IsDetailMode}
    (DataController.DetailMode <> dcdmPattern);
end;

function TcxDBDataProvider.IsDataSource: Boolean;
begin
  Result := (DataSet <> nil) and (DataSet.State <> dsInactive);
end;

function TcxDBDataProvider.IsKeyNavigation: Boolean;
begin
  Result := DataController.IsKeyNavigation;
end;

function TcxDBDataProvider.IsOtherInsert: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider <> Self) and
      (ADBDataProvider.DataSet = Self.DataSet) and ADBDataProvider.FInInsert then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TcxDBDataProvider.IsSyncInsert: Boolean;
begin
  Result := not DataController.IsPattern and
    DataController.DataModeController.SyncInsert and
    IsSyncMode;
end;

function TcxDBDataProvider.IsSyncMode: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.Active and
    (IsGridMode or DataController.DataModeController.SyncMode) and
    not IsUniDirectional;
  if DataController.IsDetailMode then
  begin
    if not DataController.DataModeController.DetailInSyncMode then
      Result := False
    else
      if DataController.DataModeController.DetailInSQLMode then
      begin
        if not IsDataSetCurrent then
          Result := False;
      end
      else
        if not (IsKeyNavigation or IsGridMode) then
          Result := False;
  end;
end;

function TcxDBDataProvider.DataSet_ActiveRecord: Integer;
begin
  Result := TDataSetAccess(DataSet).ActiveRecord;
end;

function TcxDBDataProvider.DataSet_BookmarkAvailable: Boolean;
begin
  Result := TDataSetAccess(DataSet).BookmarkAvailable;
end;

function TcxDBDataProvider.DataSet_BufferCount: Integer;
begin
  Result := TDataSetAccess(DataSet).BufferCount;
end;

function TcxDBDataProvider.IsActive: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.Active; // WARNING: DataLink.Active <> DataSet.Active!
  Result := Result and not DataSet.ControlsDisabled;
end;

function TcxDBDataProvider.IsActiveDataSet: Boolean;
begin
  Result := Assigned(DataSet) and (DataSet.FieldCount > 0) and
    (DataSet.Active or not IsDefaultFields(DataSet));
end;

function TcxDBDataProvider.IsBOF: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.BOF or not DataLink.Active
  else
    Result := inherited IsBOF;
end;

function TcxDBDataProvider.IsChanging: Boolean;
begin
  Result := inherited IsChanging;
end;

function TcxDBDataProvider.IsEditing: Boolean;
begin
  if IsUnboundColumnMode then
    Result := inherited IsEditing
  else
    Result := Assigned(DataSet) and (DataSet.State in dsEditModes);
end;

function TcxDBDataProvider.IsEOF: Boolean;
begin
  if Assigned(DataSet) then
  begin
    with DataController do
    begin
      if IsDetailMode and not IsGridMode then
      begin
        if not VarIsEmpty(MasterDetailKeyValues) then
          Result := DataSet.EOF or
            (not (DataSet.State in dsEditModes) and
             not VarEquals(GetDataSetValues(DataSet, MasterDetailKeyFields), MasterDetailKeyValues))
        else
          Result := True;
      end
      else
        Result := DataSet.EOF or not DataLink.Active;
    end;
  end
  else
    Result := inherited IsEOF;
end;

function TcxDBDataProvider.IsGridModeUpdating: Boolean;
begin
  Result := IsGridMode and FInUpdateGridMode;
end;

function TcxDBDataProvider.IsInserting: Boolean;
begin
  Result := Assigned(DataSet) and (DataSet.State = dsInsert);
end;

function TcxDBDataProvider.IsModified: Boolean;
begin
  Result := inherited IsModified {for Unbound column} or
    IsDataSetModified;
end;

function TcxDBDataProvider.IsDataSetModified: Boolean;
var
  I: Integer;
begin
  Result := Assigned(DataSet) and DataSet.Modified;
  if Result and not FAssignedFields.IsEmpty then // check Null values
  begin
    Result := False;
    for I := 0 to DataSet.FieldCount - 1 do
      if not FAssignedFields.IsFieldExists(DataSet.Fields[I]) and
        not VarIsNull(GetFieldValue(DataSet.Fields[I])) then
      begin
        Result := True;
        Break;
      end;
  end;
  // check unbound column
  if not Result and (inherited IsModified) then
  begin
    for I := 0 to DataController.ItemCount - 1 do
      if DataController.Fields[I].IsUnbound and
        not VarIsNull(DataController.GetEditValue(I, evsValue)) then
      begin
        Result := True;
        Break;
      end;
  end;
end;

function TcxDBDataProvider.BookmarkValid(const ABookmark: TcxBookmark): Boolean;
begin
  Result := Dataset.BookmarkValid(TBookmark(ABookmark));
end;

function TcxDBDataProvider.CompareBookmarks(const AItem1, AItem2: TcxBookmark): Integer;
begin
  if not DataLink.Active then
    Result := -1
  else
    Result := Dataset.CompareBookmarks(TBookmark(AItem1), TBookmark(AItem2));
end;

procedure TcxDBDataProvider.CorrectRecordIndex(ARecordIndex: Integer);
begin
  if Assigned(DataSet) then
  begin
    if not IsKeyNavigation and (Length(FSavePosInfo.Bookmark) <> 0) and
      (CompareBookmarks(FSavePosInfo.Bookmark, DataSet.Bookmark) = 0) then
      FRecordIndex := ARecordIndex;
  end;
end;

procedure TcxDBDataProvider.First;
var
  AReopened: Boolean;
begin
  FInFirst := True;
  try
    inherited First;
    if Assigned(DataSet) then
      with DataController do
        if IsDetailMode and not IsGridMode then
        begin
          if not VarIsEmpty(MasterDetailKeyValues) then
          begin
            DataModeController.DoDetailFirst(MasterDetailKeyFieldNames,
              MasterDetailKeyValues, AReopened);
            if AReopened then
              SavePos;
            IsDataSetCurrent := True;
            if RecreatedFieldsAfterFirst then
              NotifyDetailsAfterFieldsRecreating(not DataController.FInCheckCurrentQuery);
          end;
        end
        else
          if DataSet.Active then
            DataSet.First;
  finally
    FInFirst := False;
  end;
end;

procedure TcxDBDataProvider.Prev;
begin
  if Assigned(DataSet) then
    DataSet.Prior;
end;

procedure TcxDBDataProvider.Next;
begin
  if Assigned(DataSet) then
    DataSet.Next;
end;

procedure TcxDBDataProvider.Last;
begin
  if Assigned(DataSet) and DataSet.Active then
    DataSet.Last;
end;

procedure TcxDBDataProvider.MoveBy(ADistance: Integer);
begin
  if Assigned(DataSet) then
    DataSet.MoveBy(ADistance);
end;

procedure TcxDBDataProvider.Scroll(ADistance: Integer);
var
  ARecordIndex: Integer;
begin
  if not IsGridMode or (ADistance = 0) or not IsActive then Exit;
  if ADistance < 0 then
  begin
    ARecordIndex := (ADistance - DataLink.ActiveRecord);
    DataSet.MoveBy(ARecordIndex);
  end
  else
  begin
    ARecordIndex := DataLink.RecordCount - DataLink.ActiveRecord - 1 + ADistance;
    DataSet.MoveBy(ARecordIndex);
  end;
  DataController.CorrectPrevSelectionChangedInfo;
  DataController.SyncSelected(True);
end;

procedure TcxDBDataProvider.SavePos;
begin
  if Assigned(DataSet) then
    with DataSet do
    begin
      if DataSet_BookmarkAvailable then
        FSavePosInfo.Bookmark := Bookmark
      else
        SetLength(FSavePosInfo.Bookmark, 0);
      FSavePosInfo.ActiveRecord := DataSet_ActiveRecord; // save offset in DataSet buffer
      FSavePosInfo.BOF := BOF;
      FSavePosInfo.EOF := EOF;
    end;
end;

procedure TcxDBDataProvider.RestorePos;
begin
  if Assigned(DataSet) then
    with DataSet do
    begin
      if (Length(FSavePosInfo.Bookmark) <> 0) and
        Self.BookmarkValid(FSavePosInfo.Bookmark) then
      begin
        try
          Bookmark := FSavePosInfo.Bookmark;
          // restore offset in DataSet buffer
          if DataSet_ActiveRecord > FSavePosInfo.ActiveRecord then
          begin
            MoveBy(DataSet_BufferCount - DataSet_ActiveRecord - 1 + DataSet_ActiveRecord - FSavePosInfo.ActiveRecord);
            MoveBy(FSavePosInfo.ActiveRecord - DataSet_BufferCount + 1);
          end
          else
            if DataSet_ActiveRecord < FSavePosInfo.ActiveRecord then
            begin
              MoveBy(-DataSet_ActiveRecord + DataSet_ActiveRecord - FSavePosInfo.ActiveRecord);
              MoveBy(FSavePosInfo.ActiveRecord);
            end;
          // BOF and EOF
          if FSavePosInfo.BOF and not BOF then Prior;
          if FSavePosInfo.EOF and not EOF then Next;
        except
          on EDatabaseError do;
        end;
      end;
    end;
end;

function TcxDBDataProvider.CanDelete: Boolean;
begin
  Result := inherited CanDelete and CanModify;
end;

function TcxDBDataProvider.CanInitEditing(ARecordIndex: Integer): Boolean;
begin
  if IsUnboundColumnMode then
    Result := inherited CanInitEditing(ARecordIndex)
  else
  begin
    FInCanInitEditing := True;
    try
      // bug in Delphi 7 with update pack 1
      if DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused and
        not IsInserting then
      begin
        Insert;
        Result := True;
      end
      else
        Result := DataLink.Edit;
      if Result then
      begin
        SetChanging;
        // Update Navigator
        if DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused then
          DataController.Change([dccUpdateRecord]);
      end;
    finally
      FInCanInitEditing := False;
    end;
  end;
end;

function TcxDBDataProvider.CanModify: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.Active and DataSet.CanModify and IsSyncMode;
end;

procedure TcxDBDataProvider.Append;
begin
  if Assigned(DataSet) then
    AddNewRecord(DataSet.Append);
end;

procedure TcxDBDataProvider.InternalCancel;
var
  ARecordIndex: Integer;
  ARecordId: Variant;
begin
  if not Assigned(DataSet) then Exit;
  if DataController.IsDetailMode and IsEOF then // Appending in M-D
  begin
    DataSet.DisableControls;
    try
      if (DataController.GetRowCount - 2) >= 0 then
      begin
        ARecordIndex := DataController.GetRowInfo(DataController.GetRowCount - 2).RecordIndex;
        ARecordId := DataController.GetRecordId(ARecordIndex);
      end
      else
        ARecordIndex := -1;
      DataSet.Cancel;
      if ARecordIndex <> -1 then
        DataSet.Locate(DataController.KeyFieldNames, ARecordId, []);
    finally
      DataSet.EnableControls;
    end;
  end
  else
    DataSet.Cancel;
end;

procedure TcxDBDataProvider.Cancel;
begin
  if IsUnboundColumnMode then
    inherited Cancel
  else
  begin
    if DataController.IsSmartRefresh then // !!!
    begin
      DataController.FInUnboundCopy := True;
      try
        inherited Cancel;
      finally
        DataController.FInUnboundCopy := False;
      end;
    end;
    InternalCancel;
  end;
end;

procedure TcxDBDataProvider.Delete;
begin
  if Assigned(DataSet) then
  begin
    if DataController.GetFocusedRecordIndex <> -1 then
    begin
      if IsSmartRefresh then
        FUpdatingPrevFocusedRecordIndex := DataController.GetFocusedRecordIndex;

      DataSet.DisableControls;
      try
        DataSet.Delete;
        // bug in Delphi 7 with update pack 1
        DataLink.FPreventLayoutChanged := True;
        if DataController.FNearestRecordIndex <> -1 then
          DataController.TryFocusRecord(DataController.FNearestRecordIndex);
      finally
        DataController.LockStateInfo(False);
        try
          DataSet.EnableControls;
          // bug in Delphi 7 with update pack 1
          DataLink.FPreventLayoutChanged := False;
        finally
          DataController.UnlockStateInfo(False);
        end;
      end;

      if IsSmartRefresh then
      begin
        DataController.LockStateInfo(True);
        try
          if FUpdatingPrevFocusedRecordIndex < DataController.RecordCount then
            DataController.DeleteRecord(FUpdatingPrevFocusedRecordIndex);
          DataController.UpdateFocused;
        finally
          DataController.UnlockStateInfo(True);
        end;
      end;
    end;
  end;
end;

procedure TcxDBDataProvider.DeleteRecords(AList: TList);

  procedure InternalDelete(AUpdateFocused: Boolean);
  var
    ARecordIndex: Integer;
  begin
    if IsSmartRefresh then
    begin
      ARecordIndex := DataController.LocateRecordIndex(DataController.GetKeyFields);
      if ARecordIndex <> -1 then
      begin
        DataSet.Delete;
        DataController.DeleteRecord(ARecordIndex);
      end;
    end
    else
      DataSet.Delete;
  end;

  procedure DeleteWithLocate;
  var
    AKeyList: TList;
    AFieldList: TList;
    I: Integer;
    P: PVariant;
  begin
    AKeyList := TList.Create;
    AFieldList := TList.Create;
    try
      DataController.GetKeyFields(AFieldList);
      for I := 0 to AList.Count - 1 do
      begin
        New(P);
        P^ := DataController.GetInternalRecordId(Integer(AList[I]), AFieldList);
        AKeyList.Add(P);
      end;
      for I := AKeyList.Count - 1 downto 0 do
      begin
        P := PVariant(AKeyList[I]);
        if DataSet.Locate(DataController.KeyFieldNames, P^, []) then
          InternalDelete(False);
      end;
      if IsSmartRefresh then
        DataController.UpdateFocused;
    finally
      for I := 0 to AKeyList.Count - 1 do
      begin
        P := AKeyList[I];
        Dispose(P);
      end;
      AKeyList.Free;
      AFieldList.Free;
    end;
  end;

  procedure DeleteWithMoveBy;
  var
    I, AAnchorRecordIndex, J, ARecordCount, ARecordIndex: Integer;
  begin
    ARecordCount := DataController.DataSet.RecordCount;
    AAnchorRecordIndex := Integer(AList[AList.Count - 1]);
    if AAnchorRecordIndex = ARecordCount then
    begin
      AList.Delete(AList.Count - 1);
      AAnchorRecordIndex := Integer(AList[AList.Count - 1]);
    end;
    J := 0;
    if DataController.CanFocusRecord(AAnchorRecordIndex) then
      for I := AList.Count - 1 downto 0 do
      begin
        ARecordIndex := Integer(AList[I]);
        DataSet.MoveBy(ARecordIndex - AAnchorRecordIndex + J);
        DataSet.Delete;
        AAnchorRecordIndex := ARecordIndex;
        if AAnchorRecordIndex = (ARecordCount - 1) then
          J := 1
        else
          J := 0;
        Dec(ARecordCount);
      end;
  end;

begin
  if Assigned(DataSet) then
  begin
    DataSet.DisableControls;
    try
      if DataController.IsSmartRefresh then
      begin
        DataController.BeginFullUpdate;
        DataController.LockStateInfo(False);
      end;
      try
        if IsKeyNavigation then
          DeleteWithLocate
        else
          DeleteWithMoveBy;
      finally
        if DataController.IsSmartRefresh then
        begin
          if DataController.FInDeleteSelection then
            DataController.ClearSelection;
          DataController.UnlockStateInfo(False);
          DataController.EndFullUpdate;
        end
        else
          if not DataController.IsKeyNavigation then
            DataController.ClearSelection;
      end;
    finally
      if not DataController.IsSmartRefresh then
        DataController.LockStateInfo(False);
      try
        DataSet.EnableControls;
      finally
        if not DataController.IsSmartRefresh then
          DataController.UnlockStateInfo(False);
      end;
    end;
  end;
end;

procedure TcxDBDataProvider.DeleteSelection;
var
  I: Integer;
  ASelection: TcxDBDataSelection;
begin
  if IsGridMode then
  begin
    if Assigned(DataSet) then
    begin
      ASelection := DataController.DBSelection;
      DataSet.DisableControls;
      try
        for I := ASelection.Count - 1 downto 0 do
        begin
          DataSet.Bookmark := ASelection.FBookmarks[I];
          DataSet.Delete;
          ASelection.Delete(I);
        end;
      finally
        DataSet.EnableControls;
      end;
    end;
  end
  else
    inherited DeleteSelection;
end;

procedure TcxDBDataProvider.Edit;
begin
  if IsUnboundColumnMode then
    inherited Edit
  else
    if Assigned(DataSet) then
      DataSet.Edit;
end;

function TcxDBDataProvider.GetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField;
  AEditValueSource: TcxDataEditValueSource): Variant;
var
  ADataSetField: TField;
begin
  Result := Null;
  ADataSetField := TcxDBDataField(AField).Field;
  if Assigned(ADataSetField) then
  begin
    if AEditValueSource = evsText then
    begin
      if ADataSetField.IsNull and not Assigned(ADataSetField.OnGetText) then
        Result := Null
      else
        Result := ADataSetField.Text;
    end
    else
      if (AEditValueSource = evsKey) and (ADataSetField.KeyFields <> '') then
        Result := ADataSetField.DataSet.FieldValues[ADataSetField.KeyFields]
      else
        if not ADataSetField.IsNull then
          Result := ADataSetField.Value; // evsValue
  end
  else
    if AField.IsUnbound then
      Result := inherited GetEditValue(ARecordIndex, AField, AEditValueSource);
end;

procedure TcxDBDataProvider.Insert;
begin
  if Assigned(DataSet) then
  begin
    FInsertOperation := True;
    try
      AddNewRecord(DataSet.Insert);
    finally
      FInsertOperation := False;
    end;
  end;
end;

procedure TcxDBDataProvider.Post(AForcePost: Boolean);
var
  ALock: Boolean;
  ALink: TcxDataListenerLink;
begin
  if IsUnboundColumnMode then
    inherited Post(AForcePost)
  else
    if Assigned(DataSet) then
    begin
      ALock := not IsSmartRefresh;
      ALink := TcxDBDataController.AddListenerLink(DataController);
      try
        if ALock then
          DataController.BeginFullUpdate;
        try
          if AForcePost then
            DataSet.Post
          else
            DataSet.CheckBrowseMode;
        finally
          if ALock and (ALink.Ref <> nil) then
            DataController.EndFullUpdate;
        end;
      finally
        TcxDBDataController.RemoveListenerLink(ALink);
      end;
    end;
end;

procedure TcxDBDataProvider.PostEditingData;
begin
  if DataLink.Active and Assigned(DataSet) and DataSet.Active then
    DataLink.UpdateData;
end;

procedure TcxDBDataProvider.AssignItemValue(ARecordIndex: Integer; AField: TcxCustomDataField;
  const AValue: Variant);
var
  ADBField: TcxDBDataField;
  ADataSetField: TField;
begin
  ADBField := AField as TcxDBDataField;
  ADataSetField := ADBField.Field;
  if Assigned(ADataSetField) then
  begin
    if ADBField.IsLookup and (ADataSetField.KeyFields <> '') then
    begin
      ADataSetField.DataSet.FieldValues[ADataSetField.KeyFields] := AValue;
      FAssignedFields.AddFieldsByFieldNames(ADataSetField.DataSet, ADataSetField.KeyFields);
    end
    else
    begin
      ADataSetField.Value := AValue; // SetFieldValue(ADataSetField, AValue);?
      FAssignedFields.AddField(ADataSetField);
    end;
  end;
end;

procedure TcxDBDataProvider.DoInitInsertingRecord(AInsertingRecordIndex: Integer;
  const AGroupValues: TcxDataSummaryValues);
begin
  if not (FInInsert or FInCanInitEditing or IsOtherInsert) then
  begin
    DataController.UpdateEditingRecord;
    Exit;
  end;
  FAssignedFields.Clear; // reset assigned field names
  inherited DoInitInsertingRecord(AInsertingRecordIndex, AGroupValues);
  if DataController.GetAssignMasterDetailKeysSetting and DataController.IsDetailMode then
  begin
    SetDataSetValues(DataSet, DataController.MasterDetailKeyFields, DataController.MasterDetailKeyValues);
    FAssignedFields.AddFieldsByFieldList(DataController.MasterDetailKeyFields);
  end;
  DataController.DoInitInsertingRecord(AInsertingRecordIndex);
  DataController.UpdateEditingRecord;
end;

function TcxDBDataProvider.SetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField;
  const AValue: Variant; AEditValueSource: TcxDataEditValueSource): Boolean;
var
  ADataSetField: TField;
begin
  Result := False;
  ADataSetField := TcxDBDataField(AField).Field;
  if Assigned(ADataSetField) then
  begin
    if (AEditValueSource = evsText) and not VarIsNull(AValue) or Assigned(ADataSetField.OnSetText) then
      ADataSetField.Text := VarToStr(AValue)
    else
      if (AEditValueSource = evsKey) and (ADataSetField.KeyFields <> '') then
        ADataSetField.DataSet.FieldValues[ADataSetField.KeyFields] := AValue
      else
        SetFieldValue(ADataSetField, AValue);
    Result := True;
  end
  else
    if AField.IsUnbound then
      Result := inherited SetEditValue(ARecordIndex, AField, AValue, AEditValueSource);
end;


procedure TcxDBDataProvider.EndDeleting;
begin
  try
    if DataController.FNearestRecordIndex <> -1 then
    begin
      if DataController.FNearestRecordIndex < DataController.RecordCount then
        DataController.ChangeFocusedRecordIndex(DataController.FNearestRecordIndex);
    end;
  finally
    inherited;
  end;
end;

procedure TcxDBDataProvider.ActiveChanged(AActive: Boolean);
begin
  inherited ActiveChanged(AActive);
  if Assigned(DataSet) and not DataSet.Active and IsDefaultFields(DataSet) then
    DataController.ResetDBFields;
end;

procedure TcxDBDataProvider.AddListener(AProvider: TcxDBDataProvider);
begin
  if FListeners.IndexOf(AProvider) = -1 then
    FListeners.Add(AProvider);
end;

procedure TcxDBDataProvider.BeginLocate;
begin
  inherited BeginLocate;
  if Assigned(DataSet) and (DataSet.State in dsEditModes) then
    SetPostLocateFlag;
end;

procedure TcxDBDataProvider.DataScrolled(ADistance: Integer);
begin
  if IsGridMode then
  begin
    FRecordIndex := DataLink.ActiveRecord;
    DataController.BeginUpdate;
    try
      if ADistance <> 0 then
        DataController.InvalidateDataBuffer;
      if not DataController.Relations.IsEmpty then
        DataController.CollapseDetails;
    finally
      DataController.EndUpdate;
    end;
  end
  else
    Inc(FRecordIndex, ADistance);
  inherited DataScrolled(ADistance);
  if IsGridMode and DataController.IsSequenced then
    DataController.UpdateScrollBars;
end;

procedure TcxDBDataProvider.Freeze;
begin
  inherited Freeze;
  if Assigned(DataSet) then
  begin
    DataSet.DisableControls;
    if DataSet.Active and (DataSet.State in dsEditModes) then
    begin
      SetPostLocateFlag;
      DataSet.Cancel;
    end;
  end;
end;

procedure TcxDBDataProvider.LayoutChanged(ADataLayoutChanges: TcxDataLayoutChanges);
begin
  if IsDetailDataSetBusy then
  begin
    if IsActive then
      DataController.ResyncDBFields;
    Exit;
  end;
  inherited LayoutChanged(ADataLayoutChanges);
end;

procedure TcxDBDataProvider.RecordChanged(Field: TField);
var
  AIsDataSetCurrent: Boolean;
begin
  if DataController.IsDetailMode and DataController.DataModeController.DetailInSyncMode and
    not IsDataSetCurrent then
    AIsDataSetCurrent := False
  else
    AIsDataSetCurrent := True;
  if IsEditing and AIsDataSetCurrent then
    DataController.UpdateEditingRecord;
  ResetChanging;
end;

procedure TcxDBDataProvider.ResetPostLocateFlag;
begin
  FPostLocateFlag := False;
end;

procedure TcxDBDataProvider.ResetSmartRefresh;
begin
  FUpdatingRecordIndex := cxNullEditingRecordIndex;
  FUpdatingNewRecordIndex := -1;
  FUpdatingRecordIsInserting := False;
end;

procedure TcxDBDataProvider.SetPostLocateFlag;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider.DataSet = Self.DataSet) and
      (ADBDataProvider.DataController.DetailMode <> dcdmPattern) then
      ADBDataProvider.FPostLocateFlag := True;
  end;
end;

procedure TcxDBDataProvider.Unfreeze;
begin
  if Assigned(DataSet) then
    DataSet.EnableControls;
  inherited Unfreeze;
end;

procedure TcxDBDataProvider.SmartDataChanged;

  function IsKeyFieldAutoInc: Boolean;
  var
    AKeyFields: TList;
  begin
    Result := False;
    AKeyFields := TList.Create;
    try
      DataController.GetKeyDBFields(AKeyFields);
      if (AKeyFields.Count = 1) and (TField(AKeyFields[0]).DataType = ftAutoInc) then
        Result := True;
    finally
      AKeyFields.Free;
    end;
  end;

  function IsNewRecordInserted(const AFocusedRecordValues, AKeyFieldsValues: Variant): Boolean;
  begin
    Result := False;
    if not FUpdatingRecordIsInserting then Exit;
    // check auto inc
    if VarEquals(AFocusedRecordValues, FNewRecordKeyValue) and IsKeyFieldAutoInc then
    begin
      if DataController.FindRecordIndexByKey(AKeyFieldsValues) = -1 then
        Result := True;
    end;
    // new item ?
  end;

var
  AFocusedRecordIndex, ARecordIndex: Integer;
  AFocusedRecordValues, AKeyFieldsValues: Variant;
  ADataChangeInfo: TcxDataChangeInfo;
begin
  if FUpdatingRecordIndex <> cxNullEditingRecordIndex then
  begin
    AFocusedRecordIndex := DataController.GetFocusedRecordIndex;
    if AFocusedRecordIndex = FUpdatingRecordIndex then
    begin
      if (AFocusedRecordIndex < 0) and (FUpdatingNewRecordIndex <> -1) then
        AFocusedRecordIndex := FUpdatingNewRecordIndex;
      AFocusedRecordValues := DataController.GetRecordId(AFocusedRecordIndex);
      AKeyFieldsValues := DataController.GetKeyFieldsValues;
      if not VarEquals(AFocusedRecordValues, AKeyFieldsValues) then
      begin
        if IsNewRecordInserted(AFocusedRecordValues, AKeyFieldsValues) then
        begin
          if DataController.NewItemRowFocused then
          begin
            if FUpdatingNewRecordIndex <> -1 then
              AFocusedRecordIndex := FUpdatingNewRecordIndex
            else
            begin
              AFocusedRecordIndex := DataController.AppendRecord;
              DataController.CopyRecord(DataController.NewItemRecordIndex, AFocusedRecordIndex);
            end;
          end;
          DataController.DoUpdateRecord(AFocusedRecordIndex);
        end
        else
        begin
          ARecordIndex := DataController.FindRecordIndexByKey(AFocusedRecordValues);
          if (ARecordIndex = -1) or (ARecordIndex = AFocusedRecordIndex) then
          begin
            if AFocusedRecordIndex <> -1 then
              DataController.DeleteRecord(AFocusedRecordIndex);
            DataController.UpdateFocused;
          end;
        end;
      end
      else
        DataController.DoUpdateRecord(AFocusedRecordIndex);
    end;
    // record changed notify
    if AFocusedRecordIndex <> -1 then
    begin
      ADataChangeInfo := DataController.FDataChangeInfo;
      ADataChangeInfo.Kind := dcRecord;
      ADataChangeInfo.RecordIndex := AFocusedRecordIndex;
      DataController.FDataChangeInfo := ADataChangeInfo;
    end;
  end;
  DataController.FilterChanged;
  DataController.SummaryChanged(False);
  ResetPostLocateFlag; // ?
end;

procedure TcxDBDataProvider.CheckExpandedDetails;
var
  I, AFocusedRecordIndex: Integer;
begin
  AFocusedRecordIndex := DataController.FocusedRecordIndex;
  for I := 0 to DataController.RecordCount - 1 do
    if (I <> AFocusedRecordIndex) and DataController.IsDetailDataControllerExist(I, -1) then
    begin
      DataController.CollapseDetails;
      Break;
    end;
end;

function TcxDBDataProvider.DoLocate(const ASubText: string; AForward, ANext: Boolean): Boolean;
var
  ASearchItemIndex: Integer;
  AStartPos: TcxBookmark;
  AExit, AStartEOF: Boolean;

  function IsCurrentEqual: Boolean;
  var
    S: string;
  begin
    FIncrementalSearching := True;
    try
      S := DataController.GetIncrementalSearchText(-1{set FIncrementalSearching flag!}, ASearchItemIndex);
      Result := DataCompareText(S, ASubText, True);
    finally
      FIncrementalSearching := False;
    end;
  end;

  procedure CalcNextRecord;
  begin
    with DataSet do
      if not AForward then
      begin
        Prior;
        if BOF then
        begin
          if not ANext then
            Last
          else
            AExit := True;
          AStartPos := FSavePosInfo.Bookmark;
        end;
      end
      else
      begin
        Next;
        if EOF then
        begin
          if not ANext then
            First
          else
            AExit := True;
          AStartPos := FSavePosInfo.Bookmark;
        end;
      end;
  end;

begin
  Result := False;
  if not Assigned(DataSet) or IsEditing then Exit;
  with DataSet do
  begin
    if Active and not (BOF and EOF) then
    begin
      ASearchItemIndex := DataController.Search.ItemIndex;
      Result := not ANext and IsCurrentEqual;
      if Result then
        DataController.UpdateFocused
      else
      begin
        Freeze;
        try
          SavePos;
          SetLength(AStartPos, 0);
          AExit := False;
          AStartEOF := False;
          if ANext then
            CalcNextRecord
          else
            if EOF then
              AStartEOF := True;
          while (not EOF or ANext or AStartEOF) and
            ((Length(AStartPos) = 0) or (Self.CompareBookmarks(Bookmark,  AStartPos) <> 0)) do
          begin
            AStartEOF := False;
            if IsCurrentEqual then
            begin
              Result := True;
              Break;
            end;
            CalcNextRecord;
            if AExit then Break;
          end;
          if not Result then
            RestorePos;
        finally
          Unfreeze; // Invalidate disabled
        end;
        if Result then
        begin
          DataController.CorrectPrevSelectionChangedInfo;
          DataController.Change([dccLayout]); // Invalidate
        end;
        DataController.SyncMasterPos;
      end;
    end;
  end;
end;

function TcxDBDataProvider.GetDataBufferCount: Integer;
begin
  if DataController.DataModeController.GridModeBufferCount = 0 then
    Result := DataController.GetDefaultGridModeBufferCount
  else
    Result := DataController.DataModeController.GridModeBufferCount;
end;

function TcxDBDataProvider.GetDataRecordCount: Integer;
begin
  Result := DataLink.RecordCount;
end;

function TcxDBDataProvider.GetExternalDataDisplayText(ARecordIndex: Integer;
  AField: TcxCustomDataField): string;
var
  ADataSetField: TField;
  APrevActiveRecord: Integer;
begin
  ADataSetField := TcxDBDataField(AField).Field;
  if Assigned(ADataSetField) then
  begin
    if (0 <= ARecordIndex) and (ARecordIndex < DataLink.RecordCount) then
    begin
      APrevActiveRecord := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := ARecordIndex;
        Result := ADataSetField.DisplayText;
      finally
        DataLink.ActiveRecord := APrevActiveRecord;
      end;
    end
    else
      if IsInserting or FIncrementalSearching then
        Result := ADataSetField.DisplayText
      else
        Result := '';
  end
  else
    Result := inherited GetExternalDataDisplayText(ARecordIndex, AField);
end;

function TcxDBDataProvider.GetExternalDataValue(ARecordIndex: Integer;
  AField: TcxCustomDataField): Variant;

  function GetFieldValue(AField: TField): Variant;
  begin
    if AField.IsNull then
      Result := Null
    else
      Result := AField.Value;
  end;

var
  ADataSetField: TField;
  APrevActiveRecord: Integer;
begin
  ADataSetField := TcxDBDataField(AField).Field;
  if Assigned(ADataSetField) then
  begin
    if (0 <= ARecordIndex) and (ARecordIndex < DataLink.RecordCount) then
    begin
      APrevActiveRecord := DataLink.ActiveRecord;
      try
        DataLink.ActiveRecord := ARecordIndex;
        Result := GetFieldValue(ADataSetField);
      finally
        DataLink.ActiveRecord := APrevActiveRecord;
      end;
    end
    else
      if IsInserting or FIncrementalSearching then
        Result := GetFieldValue(ADataSetField)
      else
        Result := Null;
  end
  else
    Result := inherited GetExternalDataValue(ARecordIndex, AField);
end;

procedure TcxDBDataProvider.LoadDataBuffer;
begin
  if DataLink.Active and IsGridMode then
  begin
    DataLink.BufferCount := GetDataBufferCount;
    DataController.DataStorage.Clear(False);
    DataController.DataStorage.RecordCount := GetDataRecordCount;
    FRecordIndex := DataLink.ActiveRecord;
  end;
  inherited LoadDataBuffer;
end;

procedure TcxDBDataProvider.UpdateGridMode;
var
  APrevRecordCount: Integer;
begin
  if DataLink.Active and IsGridMode then
  begin
    APrevRecordCount := DataController.RecordCount;
    DataLink.BufferCount := GetDataBufferCount;
    FRecordIndex := DataLink.ActiveRecord;
    if ((APrevRecordCount <> GetDataRecordCount) or IsInserting) {and
      not DataController.FInUpdateGridModeBufferCount} then // !!!
    begin
      DataChanged(dcTotal, -1, -1);
      if DataController.FInUpdateGridModeBufferCount and
        (DataSet.State in dsEditModes) and not DataLink.ReadOnly then
        DataLink.EditingChanged;
    end
    else
    begin
      if not IsEditing then
        DataChanged(dcRecord, -1, -1);
      DataController.UpdateFocused;
    end;
  end;
end;

function TcxDBDataProvider.GetDetailHasChildren(ARecordIndex: Integer; ARelationIndex: Integer): Boolean;
var
  ARelation: TcxDBDataRelation;
  APatternDataController, ADetailDataController: TcxCustomDataController;
  ADataController: TcxDBDataController;
  AMasterDetailKeyValues: Variant;
begin
  APatternDataController := DataController.GetPatternDataController;
  if APatternDataController <> nil then
    ADetailDataController := APatternDataController.Relations[ARelationIndex].DetailDataController
  else
    ADetailDataController := nil; //!!!
  if ADetailDataController is TcxDBDataController then
  begin
    Result := True;
    ADataController := TcxDBDataController(ADetailDataController);
    ARelation := TcxDBDataRelation(DataController.Relations[ARelationIndex]);
    // event
    AMasterDetailKeyValues := ARelation.GetMasterRecordID(ARecordIndex);
    if Assigned(DataController.FOnDetailHasChildren) then
      DataController.FOnDetailHasChildren(DataController, ARecordIndex, ARelationIndex,
        ADataController.MasterDetailKeyFieldNames, AMasterDetailKeyValues, Result)
    else
      if not (ADataController.IsGridMode or ADataController.DataModeController.DetailInSQLMode) then
      begin
        if Assigned(ADataController.DataSet) and ADataController.DataSet.Active then
        begin
          ADataController.Provider.Freeze;
          try
            ADataController.Provider.SavePos;
            try
              Result := ADataController.DataSet.Locate(ADataController.MasterDetailKeyFieldNames,
                AMasterDetailKeyValues, []); // must be sort by Detail Keys
            finally
              ADataController.Provider.RestorePos;
            end;
          finally
            ADataController.Provider.Unfreeze;
            ADataController.SyncMasterPos;
          end;
        end
        else
          Result := False;
      end;
  end
  else
    Result := inherited GetDetailHasChildren(ARecordIndex, ARelationIndex);
end;

function TcxDBDataProvider.GetRecordIndex: Integer;
begin
  if not FInUpdateGridMode and IsInserting then
    Result := EditingRecordIndex
  else
    if FInUpdateGridMode and IsInserting and DataController.NewItemRowFocused then
      Result := DataController.NewItemRecordIndex
    else
      Result := FRecordIndex;
end;

function TcxDBDataProvider.GetValueDefReaderClass: TcxValueDefReaderClass;
begin
  Result := TcxValueDefDBReader;
end;

procedure TcxDBDataProvider.AddNewRecord(AOperation: TDataOperation);

  procedure CheckDataSetBrowseMode;
  begin
    DataSet.CheckBrowseMode;
    SmartDataChanged;
    ResetSmartRefresh;
    ResetEditing;
  end;

begin
  if Assigned(DataSet) then
  begin
    FInInsert := True;
    try
      if DataSet.State in dsEditModes then
      begin
        if not IsSmartRefresh and (DataSet.State = dsInsert) then
          DataController.CheckBrowseMode;

        if DataSet.State in dsEditModes then
          DataLink.UpdateRecord;

        if IsSmartRefresh then
          DataSet.DisableControls;
        try
          if IsSmartRefresh then
            CheckDataSetBrowseMode;
          AOperation;
        finally
          if IsSmartRefresh then
            DataSet.EnableControls;
        end;
      end
      else
        AOperation;
      if IsSmartRefresh and (EditingRecordIndex <> cxNullEditingRecordIndex) then
        FNewRecordKeyValue := DataController.GetRecordId(EditingRecordIndex);
    finally
      FInInsert := False;
    end;
  end;
end;

function TcxDBDataProvider.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

function TcxDBDataProvider.GetDataSet: TDataSet;
begin
  if Assigned(DataLink) then
    Result := DataLink.DataSet
  else
    Result := nil;
end;

function TcxDBDataProvider.GetDataSource: TDataSource;
begin
  if Assigned(DataLink) then
    Result := DataLink.DataSource
  else
    Result := nil;
end;

function TcxDBDataProvider.GetIsDataSetCurrent: Boolean;
begin
  if DataController.IsDetailMode and not DataController.DataModeController.DetailInSQLMode and IsSyncMode then
    Result := FInCanInitEditing or
      ((FInInsert or
        (((DataController.GetFocusedRowIndex <> -1) or FIsDataSetCurrent) and not IsOtherDetailDataSetInInsert) or
        (DataController.UseNewItemRowForEditing and DataController.NewItemRowFocused and IsInserting)) and
       not IsCanInitEditingBusy
      )
  else
    Result := FIsDataSetCurrent;
end;

function TcxDBDataProvider.GetIsSmartRefresh: Boolean;
begin
  Result := IsKeyNavigation and not IsGridMode and
    DataController.DataModeController.SmartRefresh;
end;

procedure TcxDBDataProvider.RemoveFromListeners;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  if DataSet = nil then Exit;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if (ADBDataProvider <> Self) and
      (ADBDataProvider.DataSet = Self.DataSet) then
      ADBDataProvider.FListeners.Remove(Self);
  end;
end;

procedure TcxDBDataProvider.SetDataSource(Value: TDataSource);
var
  APrevDataLinkActive: Boolean;
begin
  if Assigned(DataLink) and (DataLink.DataSource <> Value) then
  begin
//    FInSetDataSource := True;
    try
      APrevDataLinkActive := DataLink.Active;
      DataLink.DataSource := Value;
      if DataLink.Active = APrevDataLinkActive then
        DataController.UpdateFields;
    finally
//      FInSetDataSource := False;
    end;
  end;
end;

procedure TcxDBDataProvider.SetIsDataSetCurrent(Value: Boolean);
begin
  if FIsDataSetCurrent <> Value then
  begin
    if Value then
      DataController.ForEachDetail(DataController.GetMasterRelation,
        DataController.ResetDataSetCurrent);
    FIsDataSetCurrent := Value;
    // Reopen?
    DataController.DoDataSetCurrentChanged(Value);
  end;
end;

{ TcxValueDefDBReader }

function TcxValueDefDBReader.GetDisplayText(AValueDef: TcxValueDef): string;
var
  AField: TField;
begin
  AField := TcxDBDataField(AValueDef.LinkObject).Field;
  if Assigned(AField) then
    Result := AField.DisplayText
  else
    Result := inherited GetDisplayText(AValueDef);
end;

function TcxValueDefDBReader.GetValue(AValueDef: TcxValueDef): Variant;
var
  AField: TField;
begin
  AField := TcxDBDataField(AValueDef.LinkObject).Field;
  if Assigned(AField) then
    Result := GetFieldValue(AField)
  else
    Result := inherited GetValue(AValueDef);
end;

function TcxValueDefDBReader.IsInternal(AValueDef: TcxValueDef): Boolean;
begin
  Result := TcxDBDataField(AValueDef.LinkObject).IsValueDefInternal;
end;

{ TcxDBDataField }

destructor TcxDBDataField.Destroy;
begin
  if Assigned(ParentField) then
    ParentField.RemoveField(Self);
  ClearFields;
  inherited Destroy;
end;

function TcxDBDataField.CanModify(AEditValueSource: TcxDataEditValueSource): Boolean;

  function IsLookupCanModify: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to FieldCount - 1 do
      if not Fields[I].CanModify(evsValue) then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  Result := inherited CanModify(AEditValueSource);
  if not IsUnbound then
  begin
//    Result := inherited CanModify(AEditValueSource) and Assigned(Field);
    Result := Result and Assigned(Field);
    if Result then
    begin
      if IsLookup then
        Result := (AEditValueSource = evsKey) and IsLookupCanModify
      else
        Result := IsFieldCanModify(Field, AEditValueSource = evsValue);
    end;
  end;
end;

function TcxDBDataField.IsUnbound: Boolean;
begin
  Result := not IsInternal and (FieldName = '') and
    (Field = nil) and (ValueDef <> nil);
end;

function TcxDBDataField.IsValueDefInternal: Boolean;
begin
  Result := ((Field = nil) or DataController.IsGridMode) and
    (not (IsUnbound and DataController.FInUnboundCopy));
end;

procedure TcxDBDataField.AddField(ADataField: TcxDBDataField);
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

procedure TcxDBDataField.ClearFields;
var
  I: Integer;
begin
  if FFields <> nil then
  begin
    for I := FFields.Count - 1 downto 0 do
      TcxDBDataField(FFields[I]).Free;
    FFields.Free;
    FFields := nil;
  end;
end;

procedure TcxDBDataField.Reassign(ASource: TcxCustomDataField);
begin
  inherited Reassign(ASource);
  if ASource is TcxDBDataField then
  begin
    FField := TcxDBDataField(ASource).FField;
    FFields := TcxDBDataField(ASource).FFields;
    TcxDBDataField(ASource).FField := nil;
    TcxDBDataField(ASource).FFields := nil;
  end;
end;

procedure TcxDBDataField.RemoveField(ADataField: TcxDBDataField);
begin
  if FFields <> nil then
    FFields.Remove(ADataField);
  ADataField.FParentField := nil;
end;

procedure TcxDBDataField.RemoveNotification(AComponent: TComponent);
begin
  if Assigned(ReferenceField) then Exit;
  inherited RemoveNotification(AComponent);
  if Field = AComponent then
  begin
    if Provider.FInFirst and Provider.DataLink.Active and
      not Provider.DataSet.Active then
      Provider.RecreatedFieldsAfterFirst := True;
    Field := nil;
    Changed;
  end;
end;

procedure TcxDBDataField.SetPropertiesByField(AField: TField; AFieldChanged: Boolean);
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

function TcxDBDataField.SupportsMultiThreading: Boolean;
begin
  Result := inherited SupportsMultiThreading and not IsLookup;
end;

function TcxDBDataField.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

function TcxDBDataField.GetField: TField;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TcxDBDataField).Field
  else
    Result := FField;
end;

function TcxDBDataField.GetFieldItem(Index: Integer): TcxDBDataField;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TcxDBDataField).Fields[Index]
  else
    Result := TcxDBDataField(FFields[Index]);
end;

function TcxDBDataField.GetFieldCount: Integer;
begin
  if Assigned(ReferenceField) then
    Result := (ReferenceField as TcxDBDataField).FieldCount
  else
    if FFields <> nil then
      Result := FFields.Count
    else
      Result := 0;
end;

function TcxDBDataField.GetIsLookup: Boolean;
begin
  Result := Assigned(Field) and Field.Lookup;
end;

function TcxDBDataField.GetProvider: TcxDBDataProvider;
begin
  Result := DataController.Provider;
end;

procedure TcxDBDataField.SetField(Value: TField);
begin
  if Assigned(ReferenceField) then Exit;
  if (Value <> nil) and (csDestroying in Value.ComponentState) then
    Value := nil;
  if FField <> Value then
  begin
    FField := Value;
    if Assigned(Value) then
      Value.FreeNotification(Notifier);
    ClearFields;
  end;
end;

{ TcxDBDataRelation }

destructor TcxDBDataRelation.Destroy;
begin
  RemoveMasterKeyField;
  inherited Destroy;
end;

function TcxDBDataRelation.GetMasterRecordID(ARecordIndex: Integer): Variant;
var
  AList: TList;
begin
  DataController.CheckRecordRange(ARecordIndex);
  Result := Unassigned;
  if IsLinked then
  begin
    AList := TList.Create;
    try
      GetMasterKeyFields(AList);
      Result := DataController.GetInternalRecordId(ARecordIndex, AList);
    finally
      AList.Free;
    end;
  end;
end;

procedure TcxDBDataRelation.GetMasterKeyFields(AList: TList);
begin
  GetInternalKeyFields(FMasterKeyField, AList);
end;

function TcxDBDataRelation.IsLinked: Boolean;
begin
  Result := Assigned(FMasterKeyField) and (DetailKeyFieldNames <> '');
end;

procedure TcxDBDataRelation.RemoveDataField(ADataField: TcxCustomDataField);
begin
  inherited RemoveDataField(ADataField);
  if FMasterKeyField = ADataField then
    FMasterKeyField := nil;
end;

procedure TcxDBDataRelation.RemoveMasterKeyField;
begin
  FMasterKeyField.Free;
  FMasterKeyField := nil;
end;

procedure TcxDBDataRelation.UpdateMasterDetailKeyFieldNames;
begin
  if DataController.DetailMode = dcdmClone then Exit;
  Changed;
end;

function TcxDBDataRelation.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

function TcxDBDataRelation.GetDetailKeyFieldNames: string;
var
  ADataController: TcxCustomDataController;
begin
  ADataController := DataController.GetPatternDataController;
  if ADataController <> nil then
    ADataController := ADataController.Relations[Self.Index].DetailDataController;
  if (ADataController <> nil) and (ADataController is TcxDBDataController) then
    Result := (ADataController as TcxDBDataController).DetailKeyFieldNames
  else
    Result := '';
end;

function TcxDBDataRelation.GetMasterKeyFieldCount: Integer;
begin
  if FMasterKeyField = nil then
    Result := 0
  else
    if FMasterKeyField.FieldCount = 0 then
      Result := 1
    else
      Result := FMasterKeyField.FieldCount;
end;

function TcxDBDataRelation.GetMasterKeyFieldNames: string;
var
  ADataController: TcxCustomDataController;
begin
  ADataController := DataController.GetPatternDataController;
  if ADataController <> nil then
    ADataController := ADataController.Relations[Self.Index].DetailDataController;
  if (ADataController <> nil) and (ADataController is TcxDBDataController) then
    Result := (ADataController as TcxDBDataController).MasterKeyFieldNames
  else
    Result := '';
end;

{ TcxDBDataFindFilterCriteria }

procedure TcxDBDataFindFilterCriteria.Update;
var
  ADataSet: TDataSet;
begin
  if not DataController.IsDestroying then
  begin
    ADataSet := DataController.Provider.DataSet;
    if Assigned(ADataSet) and (ADataSet.State in dsEditModes) then
      try
        DataController.CheckBrowseMode;
      except
        Text := '';
        Filter.Active := False;
        raise;
      end;
  end;
  inherited Update;
  if not DataController.IsDestroying and DataController.Provider.IsSyncMode then
    DataController.CheckFocusedRow;
end;

function TcxDBDataFindFilterCriteria.GetDataController: TcxDBDataController;
begin
  Result := TcxDBDataController(inherited DataController);
end;

{ TcxDBDataFilterCriteriaItem }

function TcxDBDataFilterCriteriaItem.GetFilterOperatorClass: TcxFilterOperatorClass;
var
  ADataSet: TDataSet;
  AOperatorAdapter: TcxDBFilterOperatorAdapter;
begin
  Result := inherited GetFilterOperatorClass;
  ADataSet := DataController.Provider.DataSet;
  AOperatorAdapter := cxGetFilterOperatorAdapter(ADataSet);
  if Assigned(AOperatorAdapter) then
    AOperatorAdapter.PrepareOperatorClass(Self, ADataSet, Result);
end;

function TcxDBDataFilterCriteriaItem.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

function TcxDBDataFilterCriteriaItem.GetDBField: TcxDBDataField;
begin
  Result := Field as TcxDBDataField;
end;

{ TcxDBDataFilterCriteria }

constructor TcxDBDataFilterCriteria.Create(ADataController: TcxCustomDataController);
begin
  inherited Create(ADataController);
  FIgnoreOrigin := True;
end;

procedure TcxDBDataFilterCriteria.Assign(Source: TPersistent; AIgnoreItemNames: Boolean = False);
begin
  if Source is TcxDBDataFilterCriteria then
  begin
    BeginUpdate;
    try
      inherited;
      AutoDataSetFilter := TcxDBDataFilterCriteria(Source).AutoDataSetFilter;
      IgnoreOrigin := TcxDBDataFilterCriteria(Source).IgnoreOrigin;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TcxDBDataFilterCriteria.AssignEvents(Source: TPersistent);
begin
  inherited AssignEvents(Source);
  if Source is TcxDBDataFilterCriteria then
  begin
    OnBeforeChange := TcxDBDataFilterCriteria(Source).OnBeforeChange;
    OnFormatFilterTextValue := TcxDBDataFilterCriteria(Source).OnFormatFilterTextValue;
  end;
end;

function TcxDBDataFilterCriteria.IsAutoDataSetFilter: Boolean;
begin
  Result := AutoDataSetFilter and not DataController.Provider.IsSmartRefresh;
end;

function TcxDBDataFilterCriteria.IsFiltering: Boolean;
begin
  Result := inherited IsFiltering and
    not (Assigned(FOnBeforeChange) or IsAutoDataSetFilter);
end;

procedure TcxDBDataFilterCriteria.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    AutoDataSetFilter := False;
    IgnoreOrigin := True;
  finally
    EndUpdate;
  end;
end;

procedure TcxDBDataFilterCriteria.FormatFilterTextValue(AItem: TcxFilterCriteriaItem;
  const AValue: Variant; var ADisplayValue: string);
begin
  if Assigned(FOnFormatFilterTextValue) then
    FOnFormatFilterTextValue(Self, AItem, AValue, ADisplayValue);
end;

function TcxDBDataFilterCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TcxDBDataFilterCriteriaItem;
end;

procedure TcxDBDataFilterCriteria.Update;
var
  ADataSet: TDataSet;
  AFilterText: string;
  AFilterOptions: TFilterOptions;
  AFilterActive: Boolean;
begin
  if IsInternal then Exit;
  if not IsDestroying then
  begin
    ADataSet := DataController.Provider.DataSet;
    if Assigned(ADataSet) and (ADataSet.State in dsEditModes) then
      try
        DataController.CheckBrowseMode;
      except
        Active := False;
        raise;
      end;
    if {Active and }Assigned(FOnBeforeChange) and not IsLoading then
      FOnBeforeChange(Self, ADataSet, FilterText);

    if IsAutoDataSetFilter and Assigned(ADataSet) then
    begin
      AFilterText := FilterText;
      AFilterOptions := [];
      if fcoCaseInsensitive in Options then
        AFilterOptions := AFilterOptions + [foCaseInsensitive]
      else
        AFilterOptions := AFilterOptions - [foCaseInsensitive];
      AFilterActive := Active and (AFilterText <> '');

      if (ADataSet.Filter <> AFilterText) or (ADataSet.FilterOptions <> AFilterOptions) or
        (ADataSet.Filtered <> AFilterActive) then
      begin
        ADataSet.DisableControls;
        try
          ADataSet.Filter := AFilterText;
          ADataSet.FilterOptions := AFilterOptions;
          ADataSet.Filtered := AFilterActive;
        finally
          ADataSet.EnableControls;
        end;
      end;
    end;
  end;

  inherited Update;
  if not IsDestroying and DataController.Provider.IsSyncMode then
    DataController.CheckFocusedRow;
end;

function TcxDBDataFilterCriteria.GetDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

procedure TcxDBDataFilterCriteria.SetAutoDataSetFilter(Value: Boolean);
begin
  if FAutoDataSetFilter <> Value then
  begin
    FAutoDataSetFilter := Value;
    Changed;
  end;
end;

procedure TcxDBDataFilterCriteria.SetIgnoreOrigin(Value: Boolean);
begin
  if FIgnoreOrigin <> Value then
  begin
    FIgnoreOrigin := Value;
    Changed;
  end;
end;

{ TcxDBDataSummaryItem }

procedure TcxDBDataSummaryItem.Assign(Source: TPersistent);
begin
  if Source is TcxDBDataSummaryItem then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      FieldName := TcxDBDataSummaryItem(Source).FieldName;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxDBDataSummaryItem.DataField: TcxCustomDataField;
begin
  if Assigned(FDataField) then
    Result := FDataField
  else
    Result := inherited DataField;
end;

function TcxDBDataSummaryItem.IsCurrency(AVarType: TVarType): Boolean;
begin
  Result := inherited IsCurrency(AVarType);
  if (DataField <> nil) and
    (DataController.DBFields[DataField.Index].Field <> nil) then
    Result := DataController.DBFields[DataField.Index].IsCurrency;
end;

function TcxDBDataSummaryItem.GetDBDataController: TcxDBDataController;
begin
  Result := inherited DataController as TcxDBDataController;
end;

procedure TcxDBDataSummaryItem.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    DataController.UpdateInternalKeyFields(FFieldName, FDataField);
  end;
end;

{ TcxDBDataControllerInfo }

function TcxDBDataControllerInfo.GetDataController: TcxDBDataController;
begin
  Result := TcxDBDataController(inherited DataController);
end;

function TcxDBDataControllerInfo.GetSelectionMaxRecordCount: Integer;
begin
  Result := DataController.RecordCount;
  if (Result <> 0) and DataController.IsGridMode then
    Result := DataController.DataSetRecordCount;
end;

{ TcxDBDataController }

constructor TcxDBDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataModeController := TcxDBDataModeController.Create(Self);
end;

destructor TcxDBDataController.Destroy;
begin
  ClearMasterDetailKeyFields;
  RemoveKeyField;
  FDataModeController.Free;
  FDataModeController := nil;
  inherited Destroy;
end;

procedure TcxDBDataController.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxDBDataController then
  begin
    DataModeController := TcxDBDataController(Source).DataModeController;
    DataSource := TcxDBDataController(Source).DataSource;
    KeyFieldNames := TcxDBDataController(Source).KeyFieldNames;
    DetailKeyFieldNames := TcxDBDataController(Source).DetailKeyFieldNames;
    MasterKeyFieldNames := TcxDBDataController(Source).MasterKeyFieldNames;
    OnDetailHasChildren := TcxDBDataController(Source).OnDetailHasChildren;
    OnGridModeChanged := TcxDBDataController(Source).OnGridModeChanged;
  end;
end;

function TcxDBDataController.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or Provider.DataLink.ExecuteAction(Action);
end;

function TcxDBDataController.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or Provider.DataLink.UpdateAction(Action);
end;

procedure TcxDBDataController.ActiveChanged(AActive: Boolean);
begin
  if AActive then
  begin
    BeginFullUpdate;
    try
      RestructData;
      inherited ActiveChanged(AActive);
    finally
      EndFullUpdate;
    end;
  end
  else
    inherited ActiveChanged(AActive);
end;

function TcxDBDataController.CanChangeDetailExpanding(ARecordIndex: Integer;
  AExpanded: Boolean): Boolean;
begin
  if IsGridMode and AExpanded then
  begin
    ChangeFocusedRecordIndex(ARecordIndex);
    if FocusedRecordIndex <> ARecordIndex then // scroll by
    begin
      ChangeFocusedRecordIndex(ARecordIndex);
      if FocusedRecordIndex <> ARecordIndex then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := inherited CanChangeDetailExpanding(ARecordIndex, AExpanded);
end;

function TcxDBDataController.CanFocusRecord(ARecordIndex: Integer): Boolean;
var
  AInResetDataSetCurrent: Boolean;
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
    AInResetDataSetCurrent := False;
    if IsDetailMode and DataModeController.DetailInSyncMode then
    begin
      if FInResetDataSetCurrent then
        AInResetDataSetCurrent := True
      else
        CheckCurrentQuery;
    end;
    if not AInResetDataSetCurrent and Provider.IsSyncMode then
      Result := TryFocusRecord(ARecordIndex)
    else
      Result := inherited CanFocusRecord(ARecordIndex);
  finally
    Provider.EndLocate;
  end;
  SyncMasterPos;
end;

function TcxDBDataController.CanLoadData: Boolean;
var
  AMasterRelation: TcxDBDataRelation;
begin
  AMasterRelation := TcxDBDataRelation(GetMasterRelation);
  Result := inherited CanLoadData and (IsGridMode or ((DetailMode <> dcdmClone) or
    (AMasterRelation <> nil) and AMasterRelation.IsLinked and
    (AMasterRelation.MasterKeyFieldCount = MasterDetailKeyFields.Count)));
end;

procedure TcxDBDataController.CheckDataSetCurrent;
begin
  if FocusedRecordIndex <> -1 then
  begin
    FInResetDataSetCurrent := True;
    try
      Provider.IsDataSetCurrent := True;
    finally
      FInResetDataSetCurrent := False;
    end;
  end;
end;

function TcxDBDataController.CheckMasterBrowseMode: Boolean;
var
  AMasterRelation: TcxDBDataRelation;
  AMasterDataController: TcxDBDataController;
  ADataController: TcxDBDataController;
  ALink, AMasterLink: TcxDataListenerLink;
begin
  if FInResetDataSetCurrent then
  begin
    Result := True;
    Exit;
  end;
  AMasterRelation := GetMasterRelation as TcxDBDataRelation;
  if AMasterRelation <> nil then
  begin
    Result := False;
    AMasterDataController := AMasterRelation.DataController;
    if AMasterDataController.CheckMasterBrowseMode then
    begin
      ADataController := Self;
      ALink := AddListenerLink(ADataController);
      AMasterLink := AddListenerLink(AMasterDataController);
      try
        AMasterDataController.CheckBrowseMode;
        if ALink.Ref <> nil then
        begin
          ADataController.CheckBrowseMode;
          if (ALink.Ref <> nil) and (AMasterLink.Ref <> nil) then
            Result := True;
        end;
      finally
        RemoveListenerLink(AMasterLink);
        RemoveListenerLink(ALink);
      end;
    end;
  end
  else
    Result := True;
end;

procedure TcxDBDataController.ClearMasterDetailKeyFields;
begin
  FMasterDetailKeyFields.Free;
  FMasterDetailKeyFields := nil;
end;

procedure TcxDBDataController.CorrectAfterDelete(ARecordIndex: Integer);
begin
  inherited CorrectAfterDelete(ARecordIndex);
  if Provider.FUpdatingRecordIndex = ARecordIndex then
    Provider.ResetSmartRefresh;
  if Provider.FUpdatingPrevFocusedRecordIndex = ARecordIndex then
    Provider.FUpdatingPrevFocusedRecordIndex := -1;
end;

function TcxDBDataController.CreateDataControllerInfo: TcxCustomDataControllerInfo;
begin
  Result := TcxDBDataControllerInfo.Create(Self);
end;

function TcxDBDataController.CreateFindFilterCriteria: TcxDataFindFilterCriteria;
begin
  Result := TcxDBDataFindFilterCriteria.Create(Self);
end;

procedure TcxDBDataController.DoDataSetCurrentChanged(AIsCurrent: Boolean);
begin
end;

procedure TcxDBDataController.DoDataSourceChanged;
begin
end;

procedure TcxDBDataController.DoGridModeChanged;
begin
  if Assigned(FOnGridModeChanged) then
    FOnGridModeChanged(Self);
end;

procedure TcxDBDataController.DoInitInsertingRecord(AInsertingRecordIndex: Integer);
begin
end;

function TcxDBDataController.DoSearchInGridMode(const ASubText: string;
  AForward, ANext: Boolean): Boolean;
begin
  Search.Lock;
  try
    Result := Provider.DoLocate(ASubText, AForward, ANext);
    if Result then
      SyncSelectionFocusedRecord;
  finally
    Search.Unlock;
  end;
end;

function TcxDBDataController.FindRecordByFields(ABufferRecordIndex: Integer; AFields: TList): Integer;
var
  I: Integer;
begin
  Result := -1;
  DataControllerInfo.PrepareSorting(dccmOther);
  try
    if FFocusingRecord and (FFocusingRecordIndex >= 0) and (FFocusingRecordIndex < RecordCount) and
      AreFieldValuesEqual(ABufferRecordIndex, FFocusingRecordIndex, AFields) then
    begin
      Result := FFocusingRecordIndex;
      Exit;
    end;
    for I := 0 to RecordCount - 1 do
      if AreFieldValuesEqual(ABufferRecordIndex, I, AFields) then
      begin
        Result := I;
        Break;
      end;
  finally
    DataControllerInfo.UnprepareSorting;
  end;
end;

function TcxDBDataController.FindRecordIndexInGridMode(const AKeyFieldValues: Variant): Integer;
var
  AFieldList: TList;
  I: Integer;
begin
  Result := -1;
  AFieldList := TList.Create;
  try
    GetKeyFields(AFieldList);
    for I := 0 to RecordCount - 1 do
    begin
      if VarEquals(GetInternalRecordId(I, AFieldList), AKeyFieldValues) then
      begin
        Result := I;
        Break;
      end;
    end;
  finally
    AFieldList.Free;
  end;
end;

procedure TcxDBDataController.DoUpdateRecord(ARecordIndex: Integer);
var
  AValueDefReader: TcxValueDefReader;
begin
  AValueDefReader := Provider.GetValueDefReaderClass.Create;
  try
    DataStorage.ReadRecord(ARecordIndex, AValueDefReader);
    DoReadRecord(ARecordIndex);
  finally
    AValueDefReader.Free;
  end;
  if not Provider.FInCanInitEditing then
    Change([dccUpdateRecord]);
end;

function TcxDBDataController.GetActiveRecordIndex: Integer;
begin
  if IsGridMode then
    Result := Provider.DataLink.ActiveRecord
  else
    Result := inherited GetActiveRecordIndex;
end;

function TcxDBDataController.GetClearDetailsOnCollapse: Boolean;
begin
  Result := True;
end;

function TcxDBDataController.GetDataProviderClass: TcxCustomDataProviderClass;
begin
  Result := TcxDBDataProvider;
end;

function TcxDBDataController.GetDataRowCount: Integer;
begin
  if IsGridMode then
    if IsSequenced then
      Result := DataSetRecordCount
    else
      Result := -1
  else
    Result := inherited GetDataRowCount;
end;

function TcxDBDataController.GetDataSelectionClass: TcxDataSelectionClass;
begin
  Result := TcxDBDataSelection;
end;

function TcxDBDataController.GetDefaultGridModeBufferCount: Integer;
begin
  Result := 100;
end;

function TcxDBDataController.GetFieldClass: TcxCustomDataFieldClass;
begin
  Result := TcxDBDataField;
end;

function TcxDBDataController.GetFilterCriteriaClass: TcxDataFilterCriteriaClass;
begin
  Result := TcxDBDataFilterCriteria;
end;

function TcxDBDataController.GetFocusedDataRowIndex: Integer;
begin
  if IsGridMode then
    if IsSequenced then
      Result := RecNo - 1
    else
      Result := -1
  else
    Result := inherited GetFocusedDataRowIndex;
end;

procedure TcxDBDataController.UpdateEditingRecord;
begin
  if (Provider.EditingRecordIndex = cxNullEditingRecordIndex) {or not Provider.CanModify} then Exit;
  Provider.FUpdatingRecordIndex := Provider.EditingRecordIndex;
  Provider.FUpdatingRecordIsInserting := Provider.IsInserting;
  DoUpdateRecord(Provider.EditingRecordIndex);
end;

procedure TcxDBDataController.UpdateField(ADataField: TcxDBDataField; const AFieldNames: string; AIsLookup: Boolean);

  function CheckExistField(const AFieldName: string): Boolean;
  var
    I: Integer;
    ACurrentField: TcxDBDataField;
    APrevValueTypeClass: TcxValueTypeClass;
  begin
    APrevValueTypeClass := ADataField.ValueTypeClass;
    ADataField.ReferenceField := nil;
    Result := False;
    if ADataField.FieldName = '' then
      ADataField.ValueTypeClass := nil
    else
    begin
      for I := 0 to Fields.Count - 1 do
      begin
        ACurrentField := DBFields[I];
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

var
  APos: Integer;
  ASubDataField: TcxDBDataField;
begin
  ADataField.ClearFields;
  if not AIsLookup then
  begin
    if ADataField.FieldName <> AFieldNames then
      Fields.ReassignFields(ADataField);
    ADataField.FieldName := AFieldNames;
  end;
  if AIsLookup or IsMultipleFieldNames(AFieldNames) then
  begin
    BeginUpdate;
    try
      ADataField.ReferenceField := nil;
      APos := 1;
      while APos <= Length(AFieldNames) do
      begin
        ASubDataField := AddInternalDBField;
        ADataField.AddField(ASubDataField);
      {$WARNINGS OFF}
        UpdateField(ASubDataField, ExtractFieldName(AFieldNames, APos), False);
      {$WARNINGS ON}
      end;
    finally
      EndUpdate;
    end;
  end
  else
  begin
    if CheckExistField(AFieldNames) then
      Change([dccData])
    else
      LayoutChanged([lcStructure]);
  end;
end;

procedure TcxDBDataController.ChangeFieldName(AItemIndex: Integer; const AFieldName: string);
begin
  CheckItemRange(AItemIndex);
  if GetItemFieldName(AItemIndex) <> AFieldName then
    UpdateField(DBFields[AItemIndex], AFieldName, False);
end;

function TcxDBDataController.GetItemByFieldName(const AFieldName: string): TObject;
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

function TcxDBDataController.GetItemField(AItemIndex: Integer): TField;
begin
  CheckItemRange(AItemIndex);
  Result := DBFields[AItemIndex].Field;
end;

function TcxDBDataController.GetItemFieldName(AItemIndex: Integer): string;
begin
  CheckItemRange(AItemIndex);
  Result := DBFields[AItemIndex].FieldName;
end;

function TcxDBDataController.IsDisplayFormatDefined(AItemIndex: Integer;
  AIgnoreSimpleCurrency: Boolean): Boolean;
begin
  Result := inherited IsDisplayFormatDefined(AItemIndex, AIgnoreSimpleCurrency);
  if Result and AIgnoreSimpleCurrency and (GetItemField(AItemIndex) <> nil) and
    IsSimpleCurrencyField(GetItemField(AItemIndex)) then
    Result := False;
end;

procedure TcxDBDataController.Loaded;
begin
  if not FLoaded and Provider.IsActiveDataSet then
    LayoutChanged([lcStructure]);
  inherited;
  FLoaded := True;
end;

procedure TcxDBDataController.BeginLocate;
begin
  Provider.BeginLocate;
end;

procedure TcxDBDataController.EndLocate;
begin
  Provider.EndLocate;
end;

function TcxDBDataController.GetGroupValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
var
  ADBField: TcxDBDataField;
  I: Integer;
  V: array of Variant;
begin
  ADBField := AField as TcxDBDataField;
  if ADBField.IsLookup then
  begin
    if ADBField.FieldCount = 1 then
      Result := inherited GetGroupValue(ARecordIndex, ADBField[0])
    else
    begin
      SetLength(V, ADBField.FieldCount);
      for I := 0 to ADBField.FieldCount - 1 do
        V[I] := inherited GetGroupValue(ARecordIndex, ADBField[I]);
      Result := V;
    end;
  end
  else
    Result := inherited GetGroupValue(ARecordIndex, AField);
end;

procedure TcxDBDataController.GetKeyDBFields(AList: TList);
var
  I: Integer;
begin
  GetKeyFields(AList);
  for I := 0 to AList.Count - 1 do
  begin
    AList[I] := TcxDBDataField(AList[I]).Field;
    if AList[I] = nil then
      InvalidOperation(cxSDBKeyFieldNotFound);
  end;
end;

function TcxDBDataController.GetKeyFieldsValues: Variant;
var
  AKeyFields: TList;
begin
  AKeyFields := TList.Create;
  try
    GetKeyDBFields(AKeyFields);
    Result := GetDataSetValues(DataSet, AKeyFields);
  finally
    AKeyFields.Free;
  end;
end;

function TcxDBDataController.GetRecordId(ARecordIndex: Integer): Variant;
var
  AList: TList;
begin
  CheckRecordRange(ARecordIndex);
  AList := TList.Create;
  try
    GetKeyFields(AList);
    Result := GetInternalRecordId(ARecordIndex, AList);
  finally
    AList.Free;
  end;
end;

procedure TcxDBDataController.UpdateGridModeBufferCount;
begin
  if IsGridMode then
  begin
    FInUpdateGridModeBufferCount := True;
    try
      Provider.UpdateGridMode;
    finally
      FInUpdateGridModeBufferCount := False;
    end;
  end;
end;

procedure TcxDBDataController.CheckBrowseMode;
begin
  FInCheckBrowseMode := True;
  try
    if Assigned(DataSet) then
    begin
      if DataSet.State <> dsInactive then
        TDataSetAccess(DataSet).DataEvent(deCheckBrowseMode, 0);
      if DataSet.State in [dsEdit, dsInsert] then
      begin
        DataSet.UpdateRecord;
        if Provider.IsDataSetModified {DataSet.Modified} then
        begin
          if not IsSmartRefresh and not IsGridMode then
            DataSet.DisableControls; // support AfterPost Requery
          try
            DataSet.Post;
          finally
            if not IsSmartRefresh and not IsGridMode then
              DataSet.EnableControls;
          end;
        end
        else
          Cancel;
      end;
    end;
    CheckDetailsBrowseMode;
  finally
    FInCheckBrowseMode := False;
  end;
end;

function TcxDBDataController.DataChangedNotifyLocked: Boolean;
begin
  Result := inherited DataChangedNotifyLocked;
  if not Result and IsGridMode and Assigned(DataSet) and
    (DataSet.State = dsInsert) then
    Result := True;
end;

procedure TcxDBDataController.RefreshExternalData;
begin
  if Assigned(DataSet) then
    DataSet.Refresh;
end;

procedure TcxDBDataController.SetFocus;
begin
  if IsDetailMode and DataModeController.DetailInSyncMode then
    CheckCurrentQuery;
end;

function TcxDBDataController.IsBookmarkAvailable: Boolean;
begin
  if IsGridMode then
    Result := Length(FBookmark) <> 0
  else
    Result := inherited IsBookmarkAvailable;
end;

function TcxDBDataController.IsBookmarkRow(ARowIndex: Integer): Boolean;
var
  ABookmark: TcxBookmark;
begin
  if IsGridMode then
  begin
    Result := False;
    if IsBookmarkAvailable and Assigned(DataSet) then
    begin
      ABookmark := DBSelection.GetRowBookmark(ARowIndex);
      if Provider.CompareBookmarks(FBookmark, ABookmark) = 0 then
        Result := True;
    end;
  end
  else
    Result := inherited IsBookmarkRow(ARowIndex);
end;

function TcxDBDataController.GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
var
  ADBField: TcxDBDataField;
  I, AFieldCount: Integer;
begin
  ADBField := AField as TcxDBDataField;
  if Assigned(ADBField) and ADBField.IsLookup then
  begin
    AFieldCount := ADBField.FieldCount;
    if AFieldCount > 0 then
    begin
      if AFieldCount = 1 then
        Result := GetFilterDataValue(ARecordIndex, ADBField[0])
      else
      begin
        // Multiply Lookup Keys
        Result := VarArrayCreate([0, AFieldCount - 1], varVariant);
        for I := 0 to AFieldCount - 1 do
          Result[I] := GetFilterDataValue(ARecordIndex, ADBField[I]);
      end;
    end
    else
      Result := Null;
  end
  else
    Result := inherited GetFilterDataValue(ARecordIndex, AField);
end;

function TcxDBDataController.GetFilterItemFieldName(AItem: TObject): string;
var
  ADBField: TcxDBDataField;
  I, AFieldCount: Integer;
begin
  Result := '';
  ADBField := Fields.FieldByItem(AItem) as TcxDBDataField;
  if Assigned(ADBField) then
    if Assigned(ADBField.Field) then
      if ADBField.IsLookup then
      begin
        AFieldCount := ADBField.FieldCount;
        if ADBField.FieldCount > 0 then
          if AFieldCount = 1 then
            Result := GetFilterFieldName(ADBField[0].Field, Filter.IgnoreOrigin)
          else
          begin
            // Multiply Lookup Keys
            Result := GetFilterFieldName(ADBField[0].Field, Filter.IgnoreOrigin);
            for I := 1 to AFieldCount - 1 do
              Result := Result + ';' + GetFilterFieldName(ADBField[I].Field, Filter.IgnoreOrigin);
          end
        else
          Result := '';
      end
      else
        Result := GetFilterFieldName(ADBField.Field, Filter.IgnoreOrigin)
    else
      Result := ADBField.FieldName;
end;

function TcxDBDataController.FindRecordIndexByKey(const AKeyFieldValues: Variant): Integer;

  function GetVarArrayElementCount(const AVarArray: Variant): Integer;
  begin
    Result := VarArrayHighBound(AVarArray, 1) - VarArrayLowBound(AVarArray, 1) + 1;
  end;

var
  ALocateObject: TcxDataControllerKeyLocateObject;
  AValueDefReader: TcxValueDefUnboundReader;
  I: Integer;
begin
  if IsGridMode then
    Result := FindRecordIndexInGridMode(AKeyFieldValues)
  else
  begin
    ALocateObject := TcxDataControllerKeyLocateObject.Create(Self);
    try
      AValueDefReader := TcxValueDefUnboundReader.Create;
      try
        GetKeyFields(ALocateObject.FieldList);
        if (ALocateObject.FieldList.Count = 1) and VarIsArray(AKeyFieldValues) or
          (ALocateObject.FieldList.Count <> 1) and (not VarIsArray(AKeyFieldValues) or (GetVarArrayElementCount(AKeyFieldValues) < ALocateObject.FieldList.Count)) then
        begin
          Result := -1;
          Exit;
        end;
        if ALocateObject.FieldList.Count = 1 then
          AValueDefReader.SetValue(TcxDBDataField(ALocateObject.FieldList[0]).ValueDef, AKeyFieldValues)
        else
        begin
          for I := 0 to ALocateObject.FieldList.Count - 1 do
            AValueDefReader.SetValue(TcxDBDataField(ALocateObject.FieldList[I]).ValueDef, AKeyFieldValues[I]);
        end;
        ALocateObject.ReadData(AValueDefReader);
        Result := ALocateObject.FindRecordIndex;
      finally
        AValueDefReader.Free;
      end;
    finally
      ALocateObject.Free;
    end;
  end;
end;

function TcxDBDataController.LocateByKey(const AKeyFieldValues: Variant): Boolean;

  function TryLocateInGridMode: Boolean;
  begin
    Result := False;
    BeginLocate;
    try
      if DataSet <> nil then
        Result := CanCallDataSetLocate(DataSet, KeyFieldNames, AKeyFieldValues) and
          DataSet.Locate(KeyFieldNames, AKeyFieldValues, []);
    finally
      EndLocate;
    end;
  end;

var
  ARecordIndex, ARowIndex: Integer;
begin
  ARecordIndex := FindRecordIndexByKey(AKeyFieldValues);
  if (ARecordIndex = -1) and IsGridMode then
  begin
    Result := TryLocateInGridMode;
    if Result then
      SyncSelected(True);
    Exit;
  end;
  ARowIndex := -1;
  if ARecordIndex <> -1 then
    ARowIndex := GetRowIndexByRecordIndex(ARecordIndex, True);
  ChangeFocusedRowIndex(ARowIndex);
  Result := ARowIndex <> -1;
end;

procedure TcxDBDataController.CheckCurrentQuery;
begin
  if DataModeController.DetailInSQLMode and
    not DataModeController.DetailIsCurrentQuery(MasterDetailKeyFieldNames,
      MasterDetailKeyValues) then
    with Provider do
    begin
      Freeze;
      FInCheckCurrentQuery := True;
      try
        First;
        SavePos;
        RestorePos;
        FRecordIndex := 0;
      finally
        Unfreeze;
        FInCheckCurrentQuery := False;
      end;
    end;
end;

function TcxDBDataController.GetDetailFilterAdapter: TcxDBProviderDetailFilterAdapter;
var
  AIndex: Integer;
begin
  if cxDetailFilterControllers.FindAdapter(TDataSetClass(DataSet.ClassType), AIndex) then
    Result := cxDetailFilterControllers[AIndex] as TcxDBProviderDetailFilterAdapter
  else
  begin
    Result := nil;
    InvalidOperation(cxSDBDetailFilterControllerNotFound);
  end;
end;

procedure TcxDBDataController.SetMasterRelation(AMasterRelation: TcxCustomDataRelation;
  AMasterRecordIndex: Integer);
var
  ARelation: TcxDBDataRelation;
begin
  BeginUpdate;
  try
    inherited SetMasterRelation(AMasterRelation, AMasterRecordIndex);
    if (AMasterRelation <> nil) and (AMasterRecordIndex <> -1) and
      (AMasterRelation is TcxDBDataRelation) then
    begin
      ARelation := AMasterRelation as TcxDBDataRelation;
      FMasterDetailKeyValues := ARelation.GetMasterRecordID(AMasterRecordIndex);
      LayoutChanged([lcData]);
    end;
  finally
    EndUpdate;
  end;
end;

function TcxDBDataController.GetRowId(ARowIndex: Integer): Variant;
begin
  if not IsGridMode or (ARowIndex < 0) then
    Result := inherited GetRowId(ARowIndex)
  else
    Result := DBSelection.GetRowBookmark(ARowIndex);
end;

function TcxDBDataController.GetSelectedBookmark(Index: Integer): TcxBookmark;
begin
  if not IsGridMode then InvalidOperation(cxSDBNotInGridMode);
  if IsFocusedSelectedMode then
    Result := DBSelection.GetRowBookmark(FocusedRowIndex)
  else
    Result := DBSelection.FBookmarks[Index];
end;

function TcxDBDataController.GetSelectedRowIndex(Index: Integer): Integer;
var
  ASelection: TcxDBDataSelection;
  I, ARowCount: Integer;
  ABookmark: TcxBookmark;
begin
  if not IsGridMode or IsFocusedSelectedMode then
    Result := inherited GetSelectedRowIndex(Index)
  else
  begin
    Result := -1;
    ASelection := DBSelection;
    ARowCount := GetRowCount;
    ABookmark := ASelection.FBookmarks[Index];
// bug in BDEDataSet.CachedUpdates mode
    if (ASelection.CompareBookmarks(ASelection.GetRowBookmark(0), ABookmark) <= 0) and
      (ASelection.CompareBookmarks(ABookmark, ASelection.GetRowBookmark(ARowCount - 1)) <= 0) then
    begin
      for I := 0 to ARowCount - 1 do
        if ASelection.CompareBookmarks(ASelection.GetRowBookmark(I), ABookmark) = 0 then
        begin
          Result := I;
          Break;
        end;
    end;
  end;
end;

function TcxDBDataController.GetSelectionAnchorBookmark: TcxBookmark;
begin
  if not IsGridMode then InvalidOperation(cxSDBNotInGridMode);
  Result := DBSelection.FAnchorBookmark;
end;

function TcxDBDataController.GetSelectionAnchorRowIndex: Integer;
begin
  if not IsGridMode then
    Result := inherited GetSelectionAnchorRowIndex
  else
    Result := -1;
end;

function TcxDBDataController.IsSelectionAnchorExist: Boolean;
begin
  if not IsGridMode then
    Result := inherited IsSelectionAnchorExist
  else
    Result := Length(GetSelectionAnchorBookmark) <> 0;
end;

procedure TcxDBDataController.SelectAll;
begin
  if not IsGridMode then
    inherited SelectAll
  else
    DBSelection.SelectAll;
end;

procedure TcxDBDataController.SelectFromAnchor(ARowIndex: Integer; AKeepSelection: Boolean);
var
  ASelectionAnchorBookmark: TcxBookmark;
  ASelection: TcxDBDataSelection;
begin
  if not IsGridMode then
    inherited SelectFromAnchor(ARowIndex, AKeepSelection)
  else
  begin
    DataControllerInfo.CheckRowIndex(ARowIndex);
    ASelectionAnchorBookmark := GetSelectionAnchorBookmark;
    if Length(ASelectionAnchorBookmark) <> 0 then
    begin
      ASelection := DBSelection;
      ASelection.SelectFromAnchor(ASelection.GetRowBookmark(ARowIndex), AKeepSelection);
    end;
  end;
end;

procedure TcxDBDataController.SetSelectionAnchor(ARowIndex: Integer);
var
  ASelection: TcxDBDataSelection;
begin
  if not IsGridMode then
    inherited SetSelectionAnchor(ARowIndex)
  else
  begin
    DataControllerInfo.CheckRowIndex(ARowIndex);
    ASelection := DBSelection;
    ASelection.FAnchorBookmark := ASelection.GetRowBookmark(ARowIndex);
  end;
end;

function TcxDBDataController.FocusSelectedRow(ASelectedIndex: Integer): Boolean;
begin
  Result := False;
  if Assigned(DataSet) then // DataSet_BookmarkAvailable
  begin
    DataSet.Bookmark := GetSelectedBookmark(ASelectedIndex);
    Result := True;
  end;
end;

procedure TcxDBDataController.ForEachRow(ASelectedRows: Boolean; AProc: TcxDataControllerEachRowProc);

  procedure DoProc;
  var
    ARowIndex: Integer;
  begin
    ARowIndex := GetFocusedRowIndex;
    AProc(ARowIndex, GetRowInfo(ARowIndex));
  end;

var
  I: Integer;
begin
  if not IsGridMode then
    inherited ForEachRow(ASelectedRows, AProc)
  else
    if DataSet <> nil then
    begin
      if ASelectedRows then
      begin
        if IsFocusedSelectedMode then
        begin
          if GetFocusedRowIndex <> -1 then
            DoProc;
        end
        else
          for I := 0 to GetSelectedCount - 1 do
          begin
            DataSet.Bookmark := GetSelectedBookmark(I);
            DoProc;
          end;
      end
      else
      begin
        DataSet.First;
        while not DataSet.EOF do
        begin
          DoProc;
          DataSet.Next;
        end;
      end;
    end;
end;

function TcxDBDataController.IsSequenced: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.IsSequenced;
end;

procedure TcxDBDataController.ChangeValueTypeClass(AItemIndex: Integer;
  AValueTypeClass: TcxValueTypeClass);
begin
  CheckItemRange(AItemIndex);
  if not Assigned(DBFields[AItemIndex].Field) then
    inherited ChangeValueTypeClass(AItemIndex, AValueTypeClass);
end;

procedure TcxDBDataController.GetKeyFields(AList: TList);
begin
  GetInternalKeyFields(FKeyField, AList);
end;

function TcxDBDataController.GetRelationClass: TcxCustomDataRelationClass;
begin
  Result := TcxDBDataRelation;
end;

function TcxDBDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxDBDataSummaryItem;
end;

procedure TcxDBDataController.InvalidateDataBuffer;
begin
  Change([dccLayout]);
end;

function TcxDBDataController.InternalCheckBookmark(ADeletedRecordIndex: Integer): Boolean;
begin
  if IsGridMode then
    Result := not (Assigned(DataSet) and Provider.BookmarkValid(FBookmark))
  else
    Result := inherited InternalCheckBookmark(ADeletedRecordIndex);
end;

procedure TcxDBDataController.InternalClearBookmark;
begin
  SetLength(FBookmark, 0);
  inherited InternalClearBookmark;
end;

procedure TcxDBDataController.InternalGotoBookmark;
begin
  if IsGridMode then
  begin
    if Assigned(DataSet) then
    begin
      DataSet.Bookmark := FBookmark;
      CorrectPrevSelectionChangedInfo;
    end;
  end
  else
    inherited InternalGotoBookmark;
end;

function TcxDBDataController.InternalSaveBookmark: Boolean;
var
  ABookmark: TcxBookmark;
begin
  if IsGridMode then
  begin
    Result := False;
    if Assigned(DataSet) then
    begin
      ABookmark := DataSet.Bookmark;
      if (Length(FBookmark) = 0) or
        (Provider.CompareBookmarks(FBookmark, ABookmark) <> 0) then
        FBookmark := ABookmark;
    end;
  end
  else
    Result := inherited InternalSaveBookmark;
end;

function TcxDBDataController.IsDataField(AField: TcxCustomDataField): Boolean;
begin
  Result := inherited IsDataField(AField) or
    (TcxDBDataField(AField).Field <> nil);
end;

function TcxDBDataController.IsKeyNavigation: Boolean;
begin
  Result := (FKeyField <> nil) and not IsGridMode;
end;

function TcxDBDataController.LocateRecordIndex(AGetFieldsProc: TGetListProc): Integer;
var
  ALocateObject: TcxDataControllerKeyLocateObject;
  AValueDefReader: TcxValueDefReader;
begin
  ALocateObject := TcxDataControllerKeyLocateObject.Create(Self);
  try
    AGetFieldsProc(ALocateObject.FieldList);
    AValueDefReader := Provider.GetValueDefReaderClass.Create;
    try
      ALocateObject.ReadData(AValueDefReader);
    finally
      AValueDefReader.Free;
    end;
    Result := ALocateObject.FindRecordIndex;
  finally
    ALocateObject.Free;
  end;
end;

function TcxDBDataController.LockOnAfterSummary: Boolean;
begin
  Result := inherited LockOnAfterSummary or FInUpdateGridModeBufferCount or
    ((DataSet <> nil) and not DataSet.Active and IsDefaultFields(DataSet) and
     ((Provider.ActiveChanging and FResetDBFields) or DataSet.ControlsDisabled));
  {
  if not Result and IsGridMode and IsDetailMode and not IsPattern and
    DataSet.Active then
  begin
    if not VarEquals(GetDataSetValues(DataSet, MasterDetailKeyFields), MasterDetailKeyValues) then
      Result := True;
  end;
  }
end;

procedure TcxDBDataController.NotifyDataControllers;
var
  I: Integer;
begin
  if Provider.FInNotify then Exit;
  Provider.FInNotify := True;
  try
    for I := Provider.FListeners.Count - 1 downto 0 do
    begin
      TcxDBDataProvider(Provider.FListeners[I]).DataLink.DataSetChanged;
      Provider.FListeners.Delete(I);
    end;
  finally
    Provider.FInNotify := False;
  end;
end;

procedure TcxDBDataController.NotifyDetailAfterFieldsRecreating(ADataController: TcxCustomDataController);
var
  ADBDataController: TcxDBDataController;
begin
  if ADataController = FCreatedDataController then Exit;
  ADBDataController := ADataController as TcxDBDataController;
  ADBDataController.UpdateFields;
  ADBDataController.Change([dccData]); // for Grid
end;

procedure TcxDBDataController.NotifyDetailsAfterFieldsRecreating(ACreatingLinkObject: Boolean);
begin
  if ACreatingLinkObject then
    FCreatedDataController := Self
  else
    FCreatedDataController := nil;
  ForEachDetail(GetMasterRelation, NotifyDetailAfterFieldsRecreating);
end;

procedure TcxDBDataController.PrepareField(AField: TcxCustomDataField);
begin
  inherited PrepareField(AField);
  RetrieveField(AField as TcxDBDataField, False);
end;

procedure TcxDBDataController.RemoveNotification(AComponent: TComponent);
begin
  inherited RemoveNotification(AComponent);
  MasterDetailKeyFieldsRemoveNotification(AComponent);
end;

procedure TcxDBDataController.ResetDataSetCurrent(ADataController: TcxCustomDataController);
var
  ADBDataController: TcxDBDataController;
begin
  ADBDataController := ADataController as TcxDBDataController;
  if ADBDataController.FInResetDataSetCurrent then Exit;
  ADBDataController.FInResetDataSetCurrent := True;
  try
    ADBDataController.Provider.IsDataSetCurrent := False;
    ADBDataController.ChangeFocusedRecordIndex(-1);
  finally
    ADBDataController.FInResetDataSetCurrent := False;
  end;
end;

procedure TcxDBDataController.ResetDBFields;
begin
  FResetDBFields := True;
  try
    UpdateFields;
  finally
    FResetDBFields := False;
  end;
end;

procedure TcxDBDataController.RestructData;
begin
  if not IsGridMode then
    Provider.DataLink.BufferCount := 1;
  inherited RestructData;
end;

procedure TcxDBDataController.ResyncDBFields;
begin
  UpdateFields;
end;

function TcxDBDataController.IsOtherDetailChanged: Boolean;
begin
  Result := IsDetailMode and DataModeController.DetailInSQLMode and
    (not DataModeController.DetailInSyncMode or
     (not Provider.IsDataSetCurrent and not VarIsEmpty(MasterDetailKeyValues) and
      not VarEquals(GetDataSetValues(DataSet, MasterDetailKeyFields), MasterDetailKeyValues))
     );
end;

function TcxDBDataController.IsOtherDetailCreating: Boolean;
var
  I: Integer;
  ADBDataProvider: TcxDBDataProvider;
begin
  Result := False;
  for I := 0 to DBDataProviders.Count - 1 do
  begin
    ADBDataProvider := TcxDBDataProvider(DBDataProviders[I]);
    if {(ADBDataProvider <> Provider) and}
      (ADBDataProvider.DataSet = Provider.DataSet) then
    begin
      if ADBDataProvider.DataController.FInCheckCurrentQuery or
        ((ADBDataProvider.DataController.GetMasterDataController <> nil) and
         ADBDataProvider.DataController.GetMasterDataController.IsCreatingLinkObject) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TcxDBDataController.IsProviderDataSource: Boolean;
begin
  Result := True;
end;

function TcxDBDataController.IsSmartRefresh: Boolean;
begin
  Result := Provider.IsSmartRefresh;
end;

procedure TcxDBDataController.LoadStorage;
begin
  SaveKeys;
  try
    inherited LoadStorage;
    Provider.ResetPostLocateFlag;
  finally
    RestoreKeys;
  end;
end;

procedure TcxDBDataController.RetrieveField(ADataField: TcxDBDataField; AIsLookupKeyOnly: Boolean);

  function IsLookupKeyField(ADataField: TcxDBDataField): Boolean;
  begin
    Result := Assigned(ADataField.ParentField) and Assigned(ADataField.ParentField.Field) and
      ADataField.ParentField.Field.Lookup;
  end;

var
  APrevField: TField;
begin
  if Assigned(ADataField.ReferenceField) then Exit;
  if AIsLookupKeyOnly and not IsLookupKeyField(ADataField) then Exit;
  if Assigned(Provider.DataSet) then
  begin
    APrevField := ADataField.Field;
    if not FResetDBFields then
    begin
      ADataField.Field := FindField(Provider.DataSet.Fields, ADataField.FieldName);
      if ADataField.Field = nil then
        ADataField.Field := FindField(Provider.DataSet.AggFields, ADataField.FieldName);
    end
    else
      ADataField.Field := nil;
    if Assigned(ADataField.Field) then
    begin
      if (ADataField.Field = APrevField) and IsOtherDetailCreating then
        Exit;
      ADataField.SetPropertiesByField(ADataField.Field, ADataField.Field <> APrevField);
      if ADataField.Field.Lookup then
        UpdateField(ADataField, ADataField.Field.KeyFields, True);
    end
    else
      ADataField.DoPropertiesChanged;
  end;
end;

function TcxDBDataController.TryFocusRecord(ARecordIndex: Integer): Boolean;
var
  ARecordId: Variant;
begin
  if ARecordIndex < 0 then
    Result := False
  else
  begin
    if IsKeyNavigation then
    begin
      // check Equals before Locate
      ARecordId := GetRecordId(ARecordIndex);
      FFocusingRecordIndex := ARecordIndex;
      if VarEquals(ARecordId, GetKeyFieldsValues) and
        (Provider.FRecordIndex = ARecordIndex) then
      begin
        CheckBrowseMode; // !!!!!
        Result := True;
      end
      else
      begin
        CheckBrowseMode; // !!!!!
        FFocusingRecord := True;
        try
          Result := Provider.DataSet.Locate(KeyFieldNames, ARecordId, []);
        finally
          FFocusingRecord := False;
        end;
      end;
    end
    else
    begin
      CheckBrowseMode; // !!!!!
      Provider.MoveBy(ARecordIndex - Provider.FRecordIndex);
      Result := True;
    end;
  end;
end;

function TcxDBDataController.AddInternalDBField: TcxDBDataField;
begin
  Result := AddInternalField as TcxDBDataField;
end;

procedure TcxDBDataController.UpdateFields;
begin
  inherited UpdateFields;
  UpdateLookupFields;
end;

procedure TcxDBDataController.UpdateFocused;
begin
  BeginUpdate;
  try
    inherited UpdateFocused;
    if Provider.IsSyncMode then
      SyncDataSetPos;
  finally
    EndUpdate;
  end;
end;

procedure TcxDBDataController.UpdateInternalKeyFields(const AFieldNames: string;
  var AField: TcxDBDataField);
begin
  if AFieldNames = '' then
  begin
    AField.Free;
    AField := nil;
    Change([dccData]);
  end
  else
  begin
    if AField = nil then
      AField := AddInternalDBField;
    UpdateField(AField, AFieldNames, False);
  end;
end;

procedure TcxDBDataController.UpdateLookupFields;
var
  I: Integer;
begin
  for I := 0 to Fields.Count  - 1 do
    RetrieveField(DBFields[I], True);
end;

procedure TcxDBDataController.UpdateRelations(ARelation: TcxCustomDataRelation);
begin
  inherited UpdateRelations(ARelation);
  UpdateRelationFields;
end;

procedure TcxDBDataController.UpdateScrollBars;
begin
end;

procedure TcxDBDataController.BeginReadRecord;
begin
  FInUnboundCopy := True;
end;

procedure TcxDBDataController.EndReadRecord;
begin
  FInUnboundCopy := False;
end;

function TcxDBDataController.GetDataControllerInfo: TcxDBDataControllerInfo;
begin
  Result := TcxDBDataControllerInfo(inherited DataControllerInfo);
end;

function TcxDBDataController.GetDataSet: TDataSet;
begin
  Result := Provider.DataSet;
end;

function TcxDBDataController.GetDataSetRecordCount: Integer;
begin
  if Assigned(DataSet) and DataSet.Active then
    Result := DataSet.RecordCount
  else
    Result := 0;
end;

function TcxDBDataController.GetDataSource: TDataSource;
begin
  Result := Provider.DataSource;
end;

function TcxDBDataController.GetDBField(Index: Integer): TcxDBDataField;
begin
  Result := Fields[Index] as TcxDBDataField;
end;

function TcxDBDataController.GetDBSelection: TcxDBDataSelection;
begin
  Result := DataControllerInfo.Selection as TcxDBDataSelection;
end;

function TcxDBDataController.GetFilter: TcxDBDataFilterCriteria;
begin
  Result := inherited Filter as TcxDBDataFilterCriteria;
end;

function TcxDBDataController.GetMasterDetailKeyFieldNames: string;
begin
  if MasterKeyFieldNames <> '' then
    Result := DetailKeyFieldNames
  else
    Result := '';
end;

function TcxDBDataController.GetMasterDetailKeyFields: TList;
var
  I: Integer;
begin
  if FMasterDetailKeyFields = nil then
  begin
    FMasterDetailKeyFields := TList.Create;
    if Assigned(Provider.DataSet) then
    begin
    {$WARNINGS OFF}
      Provider.DataSet.GetFieldList(FMasterDetailKeyFields, MasterDetailKeyFieldNames);
    {$WARNINGS ON}
      for I := 0 to FMasterDetailKeyFields.Count - 1 do
        if FMasterDetailKeyFields[I] <> nil then
          TField(FMasterDetailKeyFields[I]).FreeNotification(Notifier);
    end;
  end;
  Result := FMasterDetailKeyFields;
end;

function TcxDBDataController.GetProvider: TcxDBDataProvider;
begin
  Result := TcxDBDataProvider(inherited Provider);
end;

function TcxDBDataController.GetRecNo: Integer;
begin
  if DataSet.Active then
    Result := DataSet.RecNo
  else
    Result := -1;
end;

procedure TcxDBDataController.MasterDetailKeyFieldsRemoveNotification(AComponent: TComponent);
begin
  if Assigned(FMasterDetailKeyFields) and
    (FMasterDetailKeyFields.IndexOf(AComponent) <> -1) then
    ClearMasterDetailKeyFields;
end;

procedure TcxDBDataController.RemoveKeyField;
begin
  FKeyField.Free;
  FKeyField := nil;
end;

procedure TcxDBDataController.SetDataModeController(Value: TcxDBDataModeController);
begin
  FDataModeController.Assign(Value);
end;

procedure TcxDBDataController.SetDataSource(Value: TDataSource);
var
  APrevDataSource: TDataSource;
begin
  APrevDataSource := DataSource;
  Provider.DataSource := Value;
  if DataSource <> APrevDataSource then
    DoDataSourceChanged;
end;

procedure TcxDBDataController.SetDetailKeyFieldNames(const Value: string);
begin
  if FDetailKeyFieldNames <> Value then
  begin
    FDetailKeyFieldNames := Value;
    if GetMasterRelation <> nil then
      (GetMasterRelation as TcxDBDataRelation).UpdateMasterDetailKeyFieldNames;
  end;
end;

procedure TcxDBDataController.SetFilter(Value: TcxDBDataFilterCriteria);
begin
  inherited Filter := Value;
end;

procedure TcxDBDataController.SetKeyFieldNames(const Value: string);
begin
  if FKeyFieldNames <> Value then
  begin
    FKeyFieldNames := Value;
    UpdateInternalKeyFields(FKeyFieldNames, FKeyField);
  end;
end;

procedure TcxDBDataController.SetMasterKeyFieldNames(const Value: string);
begin
  if FMasterKeyFieldNames <> Value then
  begin
    FMasterKeyFieldNames := Value;
    if GetMasterRelation <> nil then
      (GetMasterRelation as TcxDBDataRelation).UpdateMasterDetailKeyFieldNames;
  end;
end;

procedure TcxDBDataController.SetRecNo(Value: Integer);
begin
  DataSet.RecNo := Value;
end;

procedure TcxDBDataController.SyncDataSetPos;
var
  APrevRecordIndex: Integer;
begin
  if DetailMode = dcdmPattern then
  begin
    if not IsLoading and not Provider.ActiveChanging and not Provider.IsDataSetBusy then // !!!
      SyncMasterDetailDataSetPos;
  end
  else
    if IsKeyNavigation then
    begin
      APrevRecordIndex := Provider.FRecordIndex;
      Provider.FRecordIndex := LocateRecordIndex(GetKeyFields);
      if APrevRecordIndex <> Provider.FRecordIndex then
        UpdateFocused;
    end;
end;

function TcxDBDataController.SyncMasterDetail: TcxCustomDataController;

  function ExpandDetails(ADataController: TcxCustomDataController;
    ARelationIndex: Integer; const AMasterKeyValues: Variant): TcxCustomDataController;
  var
    ARowIndex, ARecordIndex, ACurRelationIndex: Integer;
    ACurDetailDataController: TcxDBDataController;
    ACurMasterRelation: TcxDBDataRelation;
    ACurMasterDetailKeyValues: Variant;
  begin
    Result := nil;
    ARowIndex := ADataController.GetFocusedRowIndex;
    if ARowIndex <> -1 then
    begin
      ARecordIndex := ADataController.GetRowInfo(ARowIndex).RecordIndex;
      ADataController.ChangeDetailExpanding(ARecordIndex, True);
      if ADataController.GetDetailExpanding(ARecordIndex) then
      begin
        // Check current detail
        ACurRelationIndex := ADataController.GetDetailActiveRelationIndex(ARecordIndex);
        if ARelationIndex <> ACurRelationIndex then
        begin
          ACurDetailDataController := ADataController.GetDetailDataController(ARecordIndex, ACurRelationIndex) as TcxDBDataController;
          if not ACurDetailDataController.Provider.IsActiveDataSet then
            Exit;
          ACurMasterRelation := ACurDetailDataController.GetMasterRelation as TcxDBDataRelation;
          ACurMasterDetailKeyValues := ACurDetailDataController.Provider.DataSet.FieldValues[ACurMasterRelation.DetailKeyFieldNames];
          if VarEquals(AMasterKeyValues, ACurMasterDetailKeyValues) then
          begin
            Result := ACurDetailDataController;
            Exit;
          end;
        end;
        ADataController.ChangeDetailActiveRelationIndex(ARecordIndex, ARelationIndex);
        Result := ADataController.GetDetailDataController(ARecordIndex, ARelationIndex)
      end;
    end;
  end;

  function IsMastersInSyncMode(AMasterDataController: TcxDBDataController): Boolean;
  var
    AMasterRelation: TcxDBDataRelation;
  begin
    Result := AMasterDataController.Provider.IsSyncMode and
      AMasterDataController.IsKeyNavigation and
      not AMasterDataController.Provider.IsEditing;
    if Result then
    begin
      AMasterRelation := AMasterDataController.GetMasterRelation as TcxDBDataRelation;
      if AMasterRelation <> nil then
        Result := IsMastersInSyncMode(AMasterRelation.DataController);
    end;
  end;

var
  AMasterRelation: TcxDBDataRelation;
  AMasterDetailKeyValues, AMasterKeyValues: Variant;
  AMasterDataController: TcxDBDataController;
  ADataController: TcxCustomDataController;
  AEqualFlag: Boolean;
begin
  if (GetMasterRelation <> nil) and (MasterDetailKeyFieldNames <> '') then
  begin
    Result := nil;
    if not (GetMasterRelation is TcxDBDataRelation) then Exit;

    AMasterRelation := GetMasterRelation as TcxDBDataRelation;
    AMasterDataController := AMasterRelation.DataController;

    if Provider.IsSyncMode and IsMastersInSyncMode(AMasterDataController) and
      not AMasterDataController.IsCreatingLinkObject then
    begin
      AMasterDataController.FUpdateDataSetPos := True;
      try
        AMasterDetailKeyValues := Provider.DataSet.FieldValues[AMasterRelation.DetailKeyFieldNames];
        AMasterKeyValues := AMasterDataController.Provider.DataSet.FieldValues[AMasterRelation.MasterKeyFieldNames];
        AEqualFlag := VarEquals(AMasterKeyValues, AMasterDetailKeyValues);
        if not (AEqualFlag and AMasterDataController.FInFocusDetails) then // !!!
        begin
          if not AEqualFlag then
          begin
            AMasterDataController.Provider.BeginLocate;
            try
              AEqualFlag := AMasterDataController.Provider.DataSet.Locate(AMasterRelation.MasterKeyFieldNames,
                AMasterDetailKeyValues, []);
            finally
              AMasterDataController.Provider.EndLocate;
            end;
          end;
          if AEqualFlag then
          begin
            ADataController := AMasterDataController.SyncMasterDetail;
            if ADataController <> nil then
              Result := ExpandDetails(ADataController, AMasterRelation.Index, AMasterKeyValues);
            SyncMasterPos;
          end;
        end;
      finally
        AMasterDataController.FUpdateDataSetPos := False;
      end;
    end;
  end
  else
    Result := Self;
end;

procedure TcxDBDataController.SyncMasterDetailDataSetPos;
begin
  if FUpdateDataSetPos or FInCheckBrowseMode then Exit;
  SyncMasterDetail;
end;

procedure TcxDBDataController.UpdateRelationFields;
var
  I: Integer;
  AMasterKeyFieldNames: string;
  AMasterKeyField: TcxDBDataField;
begin
  for I := 0 to Relations.Count - 1 do
    if Relations[I] is TcxDBDataRelation then
    begin
      AMasterKeyFieldNames := (Relations[I] as TcxDBDataRelation).MasterKeyFieldNames;
      AMasterKeyField := (Relations[I] as TcxDBDataRelation).FMasterKeyField;
      UpdateInternalKeyFields(AMasterKeyFieldNames, AMasterKeyField);
      (Relations[I] as TcxDBDataRelation).FMasterKeyField := AMasterKeyField;
    end;
end;

procedure cxDBDataInitialize;
begin
  cxDetailFilterControllers := TcxDBAdapterList.Create;
  cxDetailFilterControllers.RegisterAdapter(TDataSet, TcxDBProviderDetailFilterAdapter);
end;

procedure cxDBDataFinalize;
begin
  cxDetailFilterControllers.Free;
  cxDetailFilterControllers := nil;
  FreeAndNil(DBDataLinks);
end;

initialization
  cxDBDataInitialize;

finalization
  cxDBDataFinalize;

end.

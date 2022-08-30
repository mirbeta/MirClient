{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulercxGridConnection;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Windows, Forms, Math, Graphics, Variants, Menus,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxCustomData, cxDataStorage, cxVariants,
  cxLookAndFeels, cxSchedulerStorage, cxGridCustomView, cxGridCustomTableView,
  cxSchedulerUtils, cxSchedulerStrs, cxSchedulerDialogs, cxGridTableView,
  cxTextEdit, cxMemo, cxDropDownEdit, cxImageCombobox, cxCalendar, cxEdit,
  cxColorCombobox, cxSpinEdit, cxProgressBar, cxMaskEdit, cxCheckBox,
  cxGridPopupMenu, cxGridCustomPopupMenu, cxGridLevel, cxGrid,
  cxSchedulerCustomControls;

type
  TcxSchedulerGridBuildInPopupMenu = class;
  TcxSchedulerGridBuildInPopupMenuController = class;
  TcxSchedulerGridConnection = class;
  TcxSchedulerFieldAdapter = class;

  { TcxSchedulerStorageDataSource }

  TcxSchedulerStorageDataSource = class(TcxCustomDataSource)
  private
    FConnection: TcxSchedulerGridConnection;
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    function AddNewEvent: TcxDataRecordHandle;
    function AppendRecord: TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetAdapter(AItemHandle: TcxDataItemHandle; out Adapter: TcxSchedulerFieldAdapter): Boolean;
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    property Connection: TcxSchedulerGridConnection read FConnection;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  end;

  { TcxSchedulerFieldAdapter }

  TcxSchedulerFieldAdapter = class(TPersistent)
  private
    FConnection: TcxSchedulerGridConnection;
    FEditProperties: TcxCustomEditProperties;
    FField: TcxCustomSchedulerStorageField;
    FFreeNotificator: TcxFreeNotificator;
    FItem: TcxCustomGridTableItem;
    FLink: TCollectionItem;

    procedure FreeNotification(Sender: TComponent);

    function GetCaption: string;
    function GetProperties: TcxCustomEditProperties;
    function GetReadOnly: Boolean;
    function GetStorage: TcxCustomSchedulerStorage;
    function GetVisible: Boolean;
    procedure SetCaption(AValue: string);
    procedure SetItem(AValue: TcxCustomGridTableItem);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure DeleteItem; virtual;
    procedure DoGetPropertiesForEdit(Sender: TcxCustomGridTableItem;
      ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties); virtual;
    function GetEvent(ARecordIndex: Integer): TcxSchedulerEvent;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; virtual;
    function GetPropertiesForEditClass: TcxCustomEditPropertiesClass; virtual;
    function GetValue(AEvent: TcxSchedulerEvent): Variant; virtual;
    function GetValueTypeClass: TcxValueTypeClass; virtual;
    procedure Initialize; virtual;
    procedure InitializeAdapter; virtual;
    procedure InitializeEditProperties; virtual;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); virtual;

    property Link: TCollectionItem read FLink write FLink;
  public
    constructor Create(AConnection: TcxSchedulerGridConnection;
      ALink: TCollectionItem); virtual;
    constructor CreateWithField(AConnection: TcxSchedulerGridConnection;
      AField: TcxCustomSchedulerStorageField); virtual;
    destructor Destroy; override;
    class procedure Register;
    class procedure Unregister;

    property Caption: string read GetCaption write SetCaption;
    property Connection: TcxSchedulerGridConnection read FConnection;
    property EditProperties: TcxCustomEditProperties read FEditProperties write FEditProperties;
    property Field: TcxCustomSchedulerStorageField read FField;
    property Item: TcxCustomGridTableItem read FItem write SetItem;
    property Properties: TcxCustomEditProperties read GetProperties;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
    property Values[AEvent: TcxSchedulerEvent]: Variant read GetValue write SetValue;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TcxSchedulerFieldAdapterClass = class of TcxSchedulerFieldAdapter;

  { TcxSchedulerNullAdapter }

  TcxSchedulerNullAdapter = class(TcxSchedulerFieldAdapter)
  protected
    procedure DeleteItem; override;
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  public
    constructor CreateWithItem(AConnection: TcxSchedulerGridConnection; AItem: TcxCustomGridTableItem); virtual;
  end;

  { TcxSchedulerIntegerFieldAdapter }

  TcxSchedulerIntegerFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValueTypeClass: TcxValueTypeClass; override;
  end;

  { TcxSchedulerTextFieldAdapter }

  TcxSchedulerTextFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TcxSchedulerCheckFieldAdapter }

  TcxSchedulerCheckFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValueTypeClass: TcxValueTypeClass; override;
  end;

  { TcxSchedulerDateTimeFieldAdapter }

  TcxSchedulerDateTimeFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure Initialize; override;
  end;

  { TcxSchedulerEventStartFieldAdapter }

  TcxSchedulerEventStartFieldAdapter = class(TcxSchedulerDateTimeFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
  end;

  { TcxSchedulerEventFinishFieldAdapter }

  TcxSchedulerEventFinishFieldAdapter = class(TcxSchedulerDateTimeFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
  end;

  { TcxSchedulerImageComboFieldAdapter }

  TcxSchedulerImageComboFieldAdapter = class(TcxSchedulerFieldAdapter)
  private
    function GetProperties: TcxImageComboBoxProperties;
  protected
    procedure AddItems(const AItemsValues: array of Variant);
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  public
    property Properties: TcxImageComboBoxProperties read GetProperties;
  end;

  { TcxSchedulerIDFieldAdapter }

  TcxSchedulerIDFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerGroupIDFieldAdapter }

  TcxSchedulerGroupIDFieldAdapter = class(TcxSchedulerIDFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
  end;

  { TcxSchedulerTypeFieldAdapter }

  TcxSchedulerTypeFieldAdapter = class(TcxSchedulerImageComboFieldAdapter)
  protected
    procedure Initialize; override;
  end;

  { TcxSchedulerAllDayFieldAdapter }

  TcxSchedulerAllDayFieldAdapter = class(TcxSchedulerCheckFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerEnabledFieldAdapter }

  TcxSchedulerEnabledFieldAdapter = class(TcxSchedulerCheckFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerReminderFieldAdapter }

  TcxSchedulerReminderFieldAdapter = class(TcxSchedulerCheckFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerLabelColorFieldAdapter }

  TcxSchedulerLabelColorFieldAdapter = class(TcxSchedulerImageComboFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure Initialize; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerRecurringFieldAdapter }

  TcxSchedulerRecurringFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    function GetValueTypeClass: TcxValueTypeClass; override;
  end;

  { TcxSchedulerStateFieldAdapter }

  TcxSchedulerStateFieldAdapter = class(TcxSchedulerImageComboFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure Initialize; override;
  end;

  { TcxSchedulerResourceIDFieldAdapter }

  TcxSchedulerResourceIDFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetValueTypeClass: TcxValueTypeClass; override;
    procedure Initialize; override;
  end;

  { TcxSchedulerCompleteFieldAdapter }

  TcxSchedulerCompleteFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetPropertiesForEditClass: TcxCustomEditPropertiesClass; override;
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure InitializeEditProperties; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerLinksFieldAdapter }

  TcxSchedulerLinksFieldAdapter = class(TcxSchedulerFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
  end;

  { TcxSchedulerStatusFieldAdapter }

  TcxSchedulerStatusFieldAdapter = class(TcxSchedulerImageComboFieldAdapter)
  protected
    function GetValue(AEvent: TcxSchedulerEvent): Variant; override;
    procedure Initialize; override;
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerGridConnection }

  TcxSchedulerInitializeGridItemEvent = procedure(Sender: TcxSchedulerGridConnection;
    AField: TcxCustomSchedulerStorageField; AItem: TcxCustomGridTableItem; Adapter: TcxSchedulerFieldAdapter) of object;
  TcxSchedulerGetFieldAdapterEvent = procedure(Sender: TcxSchedulerGridConnection;
    AField: TcxCustomSchedulerStorageField; var AdapterClass: TcxSchedulerFieldAdapterClass) of object;
  TcxSchedulerConnectionEditEventEvent = procedure(Sender: TcxSchedulerGridConnection;
    AEvent: TcxSchedulerControlEvent; var AModified: Boolean; var AHandled: Boolean) of object;
  TcxSchedulerConnectionGridViewDblClickEvent = procedure (Sender: TObject;
    var AHandled: Boolean) of object;
  TcxSchedulerConnectionNewEventEvent = procedure(Sender: TcxSchedulerGridConnection;
    var AEvent: TcxSchedulerControlEvent; var AHandled: Boolean) of object;
  TcxSchedulerConnectionInitEditDialogEvent = procedure(Sender: TcxSchedulerGridConnection;
    var AEditInfo: TcxSchedulerEventEditInfo) of object;

  TcxSchedulerGridConnection = class(TcxCustomComponent, IUnknown, IcxSchedulerStorageListener)
  private
    FActive: Boolean;
    FAdapters: TCollection;
    FEventEditInfo: TcxSchedulerEventEditInfo;
    FGridPopupMenu: TcxSchedulerGridBuildInPopupMenu;
    FGridPopupMenuEvents: TNotifyEvent;
    FGridPopupMenuController: TcxSchedulerGridBuildInPopupMenuController;
    FGridView: TcxCustomGridTableView;
    FIsChanged: Boolean;
    FLockCount: Integer;
    FProvider: TcxSchedulerStorageDataSource;
    FStorage: TcxCustomSchedulerStorage;
    FOnEditEvent: TcxSchedulerConnectionEditEventEvent;
    FOnGetFieldAdapter: TcxSchedulerGetFieldAdapterEvent;
    FOnGridViewDblClick: TcxSchedulerConnectionGridViewDblClickEvent;
    FOnInitEditDialog: TcxSchedulerConnectionInitEditDialogEvent;
    FOnInitializeItem: TcxSchedulerInitializeGridItemEvent;
    FOnNewEvent: TcxSchedulerConnectionNewEventEvent;
    function GetAdapter(AIndex: Integer): TcxSchedulerFieldAdapter;
    function GetCount: Integer;
    function GetDataController: TcxCustomDataController;
    function GetEvent(ARowIndex: Integer): TcxSchedulerEvent;
    function GetEventCount: Integer;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetIsLocked: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetGridPopupMenu(AValue: TcxSchedulerGridBuildInPopupMenu);
    procedure SetGridView(AValue: TcxCustomGridTableView);
    procedure SetStorage(AValue: TcxCustomSchedulerStorage);
    procedure SetupBuildInPopupMenu;
    procedure ReadLinks(Reader: TReader);
    procedure WriteLinks(Writer: TWriter);
  protected
    function AddNewEvent(ARecurrence: Boolean = False;
      AnAllDayEvent: Boolean = False): TcxSchedulerEvent; virtual;
    function AdapterByItem(AItem: TcxCustomGridTableItem): TcxSchedulerFieldAdapter;
    function AddAdapterLink(Adapter: TcxSchedulerFieldAdapter): TCollectionItem;
    function AddAdapter(AField: TcxCustomSchedulerStorageField;
      AdapterClass: TcxSchedulerFieldAdapterClass; ACaption: Pointer;
      AVisible, ACanEdit: Boolean): TcxSchedulerFieldAdapter;
    function AddAdapterEx(ALink: TCollectionItem;
      AdapterClass: TcxSchedulerFieldAdapterClass): TcxSchedulerFieldAdapter;
    function AddAdapterForCustomField(
      AField: TcxCustomSchedulerStorageField): TcxSchedulerFieldAdapter;
    procedure BeginUpdate;
    function CreateDataSource: TcxSchedulerStorageDataSource;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DblClickGridHandler(Sender: TObject); virtual;
    function DoGridViewDblClick(Sender: TObject): Boolean;
    procedure DoInitEditDialog; virtual;
    procedure DoInitializeItem(AField: TcxCustomSchedulerStorageField;
      AItem: TcxCustomGridTableItem; AAdapter: TcxSchedulerFieldAdapter); virtual;
    procedure DoEditEventUsingDialog(AEvent: TcxSchedulerEvent;
      ARecurrence: Boolean = False; AReadOnly: Boolean = False;
      AForcePatternEditing: Boolean = False); virtual;
    procedure DoGetItemAdapter(AField: TcxCustomSchedulerStorageField;
      var AdapterClass: TcxSchedulerFieldAdapterClass); virtual;
    procedure EditingGridHandler(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem; var AAllow: Boolean); virtual;
    procedure EndUpdate;
    function GetEventEditInfo(AEvent: TcxSchedulerControlEvent;
      ARecurrence: Boolean = False): TcxSchedulerEventEditInfo;
    function GetFocusedEvent: TcxSchedulerEvent;
    function GetIsFieldActive(AField: TcxCustomSchedulerStorageField): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenEventEditDialog(AForcePatternEditing: Boolean = False;
      AReadOnly: Boolean = False);
    procedure Reactivate;
    procedure SetupView; virtual;

    //
    procedure Loaded; override;
    procedure RestoreItems;
    procedure StoreItems;
    procedure Updated; override;
    procedure Updating; override;

    // IcxSchedulerStorageListener
    procedure StorageChanged(Sender: TObject); virtual;
    procedure StorageRemoved(Sender: TObject); virtual;

    property Adapters[Index: Integer]: TcxSchedulerFieldAdapter read GetAdapter;
    property Count: Integer read GetCount;
    property DataController: TcxCustomDataController read GetDataController;
    property EventCount: Integer read GetEventCount;
    property Events[ARowIndex: Integer]: TcxSchedulerEvent read GetEvent;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsLocked: Boolean read GetIsLocked;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure Assign(Source: TPersistent); override;
    procedure Refresh;

    property Provider: TcxSchedulerStorageDataSource read FProvider;
  published
    property GridPopupMenu: TcxSchedulerGridBuildInPopupMenu read FGridPopupMenu write SetGridPopupMenu;
    property GridPopupMenuEvents: TNotifyEvent read FGridPopupMenuEvents write FGridPopupMenuEvents;
    property GridView: TcxCustomGridTableView read FGridView write SetGridView;
    property Storage: TcxCustomSchedulerStorage read FStorage write SetStorage;
    property Active: Boolean read FActive write SetActive default False;
    property OnEditEvent: TcxSchedulerConnectionEditEventEvent read FOnEditEvent write FOnEditEvent;
    property OnGetFieldAdapter: TcxSchedulerGetFieldAdapterEvent read FOnGetFieldAdapter write FOnGetFieldAdapter;
    property OnGridViewDblClick: TcxSchedulerConnectionGridViewDblClickEvent read FOnGridViewDblClick write FOnGridViewDblClick;
    property OnInitializeItem: TcxSchedulerInitializeGridItemEvent read FOnInitializeItem write FOnInitializeItem;
    property OnInitEditDialog: TcxSchedulerConnectionInitEditDialogEvent read FOnInitEditDialog write FOnInitEditDialog;
    property OnNewEvent: TcxSchedulerConnectionNewEventEvent read FOnNewEvent write FOnNewEvent;
  end;

  { TcxSchedulerGridBuildInPopupMenu }

  TcxSchedulerGridBuildInPopupMenuItem = (bpmiNewEvent, bpmiNewAllDayEvent,
    bpmiNewReccuringEvent, bpmiOpen, bpmiEditSeries,
    bpmiShowTimeAs, bpmiLabel, bpmiDelete);
  TcxSchedulerGridBuildInPopupMenuItems = set of TcxSchedulerGridBuildInPopupMenuItem;

  TcxSchedulerGridBuildInPopupMenuPopupEvent = procedure (Sender: TcxSchedulerGridBuildInPopupMenu;
    ABuiltInMenu: TPopupMenu; var AHandled: Boolean) of object;
  TcxSchedulerGridBuildInPopupMenuClickEvent = procedure (Sender: TcxSchedulerGridBuildInPopupMenu;
    AItem: TcxSchedulerGridBuildInPopupMenuItem; ASubItemIndex: Integer;
    var AHandled: Boolean) of object;

  TcxSchedulerGridBuildInPopupMenu = class(TPersistent)
  private
    FItems: TcxSchedulerGridBuildInPopupMenuItems;
    FOwner: TcxSchedulerGridConnection;
    FPopupMenu: TComponent;
    FUseBuiltInPopupMenu: Boolean;
    FOnPopup: TcxSchedulerGridBuildInPopupMenuPopupEvent;
    FOnClick: TcxSchedulerGridBuildInPopupMenuClickEvent;
    procedure SetPopupMenu(AValue: TComponent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
  public
    constructor Create(ASchedulerGridConnection: TcxSchedulerGridConnection);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Items: TcxSchedulerGridBuildInPopupMenuItems
      read FItems write FItems default [bpmiNewEvent, bpmiNewAllDayEvent,
        bpmiNewReccuringEvent, bpmiOpen, bpmiEditSeries,
        bpmiShowTimeAs, bpmiLabel, bpmiDelete];
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
    property UseBuiltInPopupMenu: Boolean read FUseBuiltInPopupMenu write FUseBuiltInPopupMenu default True;

    property OnClick: TcxSchedulerGridBuildInPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxSchedulerGridBuildInPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

  TcxSchedulerGridBuildInPopupMenuController = class
  private
    FEvent: TcxSchedulerEvent;
    FGridPopupMenu: TcxGridPopupMenu;
    FInternalMenu: TPopupMenu;
    FMenuInfo: TcxPopupMenuInfo;
    FOwner: TcxSchedulerGridConnection;
    function CanEventEdit: Boolean;
    procedure CreateLabelItems;
    procedure CreateTimeItems;
    procedure DeleteEvent;
    procedure EditEvent;
    procedure EditSeries;
    function GetBuildInPopupMenu: TcxSchedulerGridBuildInPopupMenu;
    function GetCommand(AItem: TcxSchedulerGridBuildInPopupMenuItem;
      ASubItemIndex: Integer): Integer;
    function GetItems: TcxSchedulerGridBuildInPopupMenuItems;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure GridMenuPopup(ASenderMenu: TComponent;
      AHitTest: TcxCustomGridHitTest; X, Y: Integer);
    function IsEventsReadOnly: Boolean;
    procedure NewAllDayEvent;
    procedure NewEvent;
    procedure NewRecurrence;
    procedure SetEvent;
    procedure SetEventLabelColor(AColor: Integer);
    procedure SetEventState(AState: Integer);
    procedure UnpackCommand(ACommand: Integer;
      out AItem: TcxSchedulerGridBuildInPopupMenuItem; out ASubItemIndex: Integer);
  protected
    function AddValidSeparator(AOwner: TMenuItem): TMenuItem;
    procedure CreateInternalMenu;
    procedure CreateItems;
    function CreateSubItem(AOwner: TMenuItem; const ACaption: string; ACommand: Integer = -1;
      AImageIndex: Integer = -1; AEnabled: Boolean = True; AChecked: Boolean = False): TMenuItem;
    function DoOnClick(ACommand: Integer): Boolean;
    function DoOnPopup: Boolean;
    function IsValidCommand(ACommand: Integer): Boolean;

    procedure OnItemClickHandler(Sender: TObject);

    property BuildInPopupMenu: TcxSchedulerGridBuildInPopupMenu read GetBuildInPopupMenu;
    property Items: TcxSchedulerGridBuildInPopupMenuItems read GetItems;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor Create(ASchedulerGridConnection: TcxSchedulerGridConnection);
    destructor Destroy; override;
    function Popup(X, Y: Integer): Boolean;
  end;

implementation

type
  TStorageAccess = class(TcxSchedulerStorage);
  TStorageFieldAccess = class(TcxCustomSchedulerStorageField);

  { TcxSchedulerFieldAdapters }

  TcxSchedulerFieldAdapters = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TcxSchedulerGridConnection); reintroduce; overload;
  end;

  { TcxSchedulerFieldAdapterItemLink }

  TcxSchedulerFieldAdapterItemLink = class(TCollectionItem)
  private
    FAdapterClass: string;
    FFieldIndex: Integer;
    FItemIndex: Integer;
    FLink: TcxSchedulerFieldAdapter;
    function GetAdapterClass: string;
    function GetConnection: TcxSchedulerGridConnection;
    function GetFieldIndex: Integer;
    function GetItem: TcxCustomGridTableItem;
    procedure SetAdapterClass(AValue: string);
    procedure SetFieldIndex(AValue: Integer);
    procedure SetItem(AValue: TcxCustomGridTableItem);
  protected
    function IsLocked: Boolean;
    procedure RestoreProperties;
    procedure StoreProperties;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property Connection: TcxSchedulerGridConnection read GetConnection;
    property Link: TcxSchedulerFieldAdapter read FLink write FLink;
  published
    property AdapterClass: string read GetAdapterClass write SetAdapterClass;
    property FieldIndex: Integer read GetFieldIndex write SetFieldIndex default -1;
    property Item: TcxCustomGridTableItem read GetItem write SetItem;
  end;

function IsPopupMenuPossible(AGridView: TcxCustomGridTableView;
  out AGrid: TcxGrid): Boolean;
var
  ALevel: TComponent;
  AControl: TcxControl;
begin
  Result := False;
  AGrid := nil;
  if AGridView <> nil then
  begin
    ALevel := AGridView.Level;
    if (ALevel <> nil) and (ALevel is TcxGridLevel) then
    begin
      AControl := TcxGridLevel(ALevel).Control;
      if (AControl <> nil) and (AControl is TcxGrid) then
      begin
        AGrid := TcxGrid(AControl);
        Result := True;
      end;
    end;
  end;
end;

{ TcxSchedulerStorageDataSource }

function TcxSchedulerStorageDataSource.AddNewEvent: TcxDataRecordHandle;
var
  AEvent: TcxSchedulerEvent;
begin
  Result := nil;
  AEvent := Connection.AddNewEvent();
  if AEvent <> nil then
    Result := AEvent.RecordHandle
  else
    Abort;
end;

function TcxSchedulerStorageDataSource.AppendRecord: TcxDataRecordHandle;
begin
  Result := AddNewEvent;
end;

procedure TcxSchedulerStorageDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
var
  AEvent: TcxSchedulerEvent;
begin
  Connection.Storage.BeginUpdate;
  try
    AEvent := Connection.Storage.Events[GetRecordIndexByHandle(ARecordHandle)];
    if AEvent.EventType in [etCustom, etOccurrence] then
      AEvent.Delete;
    AEvent.Delete;
  finally
    Connection.Storage.EndUpdate;
  end;
end;

function TcxSchedulerStorageDataSource.GetAdapter(
  AItemHandle: TcxDataItemHandle; out Adapter: TcxSchedulerFieldAdapter): Boolean;
begin
  Adapter := Connection.AdapterByItem(
    DataController.GetItem(Integer(AItemHandle)) as TcxCustomGridTableItem);
  Result := Adapter <> nil;
end;

function TcxSchedulerStorageDataSource.GetRecordCount: Integer;
begin
  if Storage = nil then
    Result := 0
  else
    Result := Storage.EventCount;
end;

function TcxSchedulerStorageDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := Storage.Events[ARecordIndex];
end;

function TcxSchedulerStorageDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  Adapter: TcxSchedulerFieldAdapter;
begin
  if (Storage <> nil) and GetAdapter(AItemHandle, Adapter) then
    Result := Adapter.GetValue(TcxSchedulerEvent(ARecordHandle))
  else
    Result := Null;
end;

function TcxSchedulerStorageDataSource.InsertRecord(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
  Result := AddNewEvent;
end;

procedure TcxSchedulerStorageDataSource.SetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  Adapter: TcxSchedulerFieldAdapter;
begin
  Connection.BeginUpdate;
  try
    if (Storage <> nil) and GetAdapter(AItemHandle, Adapter) then
      Adapter.SetValue(TcxSchedulerEvent(ARecordHandle), AValue);
  finally
    Connection.EndUpdate;
  end;
end;

function TcxSchedulerStorageDataSource.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := FConnection.Storage;
end;

{ TcxSchedulerFieldAdapter }

constructor TcxSchedulerFieldAdapter.Create(
  AConnection: TcxSchedulerGridConnection; ALink: TCollectionItem);
begin
  inherited Create;
  FConnection := AConnection;
  FLink := ALink;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
end;

constructor TcxSchedulerFieldAdapter.CreateWithField(
  AConnection: TcxSchedulerGridConnection; AField: TcxCustomSchedulerStorageField);
begin
  Create(AConnection, AConnection.AddAdapterLink(Self));
  FField := AField;
  with Connection do
  begin
    Item := GridView.CreateItem as TcxCustomGridTableItem;
    Item.Name := CreateUniqueName(GridView.Owner, GridView, Item,
      Copy(Item.ClassName, 1, Pos('Item', FItem.ClassName) - 1), '');
  end;
  Item.DataBinding.ValueTypeClass := GetValueTypeClass;
  Item.PropertiesClass := GetPropertiesClass;
  InitializeAdapter;
end;

destructor TcxSchedulerFieldAdapter.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FEditProperties);
  DeleteItem;
  inherited Destroy;
end;

class procedure TcxSchedulerFieldAdapter.Register;
begin
  Classes.RegisterClass(Self);
end;

class procedure TcxSchedulerFieldAdapter.Unregister;
begin
  Classes.UnRegisterClass(Self);
end;

procedure TcxSchedulerFieldAdapter.DeleteItem;
begin
  if (FItem <> nil) and ([csDestroying, csLoading] *
    (FItem.ComponentState + Connection.ComponentState) = []) then
    FreeAndNil(FItem);
end;

procedure TcxSchedulerFieldAdapter.DoGetPropertiesForEdit(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
begin
  if EditProperties <> nil then
    AProperties := EditProperties;
end;

function TcxSchedulerFieldAdapter.GetEvent(
  ARecordIndex: Integer): TcxSchedulerEvent;
begin
  Result := Storage.Events[ARecordIndex];
end;

function TcxSchedulerFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties;
end;

function TcxSchedulerFieldAdapter.GetPropertiesForEditClass: TcxCustomEditPropertiesClass;
begin
  Result := nil;
end;

function TcxSchedulerFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Values[Field.Index];
end;

function TcxSchedulerFieldAdapter.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := TStorageAccess(Storage).GetFieldValueTypeClass(Field)
end;

procedure TcxSchedulerFieldAdapter.Initialize;
begin
end;

procedure TcxSchedulerFieldAdapter.InitializeAdapter;
begin
  Initialize;
  InitializeEditProperties;
end;

procedure TcxSchedulerFieldAdapter.InitializeEditProperties;
begin
  if csDesigning in Connection.ComponentState then Exit;
  if GetPropertiesForEditClass <> nil then
  begin
    FreeAndNil(FEditProperties);
    FEditProperties := GetPropertiesForEditClass.Create(FConnection);
  end;
  if (FEditProperties <> nil) and not Assigned(FItem.OnGetPropertiesForEdit) then
    FItem.OnGetPropertiesForEdit := DoGetPropertiesForEdit;
end;

procedure TcxSchedulerFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  AEvent.Values[Field.Index] := AValue;
end;

procedure TcxSchedulerFieldAdapter.FreeNotification(Sender: TComponent);
begin
  if Item = Sender then
  begin
    Item := nil;
    FreeAndNil(FLink);
  end;
end;

function TcxSchedulerFieldAdapter.GetCaption: string;
begin
  Result := Item.Caption;
end;

function TcxSchedulerFieldAdapter.GetProperties: TcxCustomEditProperties;
begin
  Result := Item.Properties;
end;

function TcxSchedulerFieldAdapter.GetReadOnly: Boolean;
begin
  Result := Item.Options.Editing;
end;

function TcxSchedulerFieldAdapter.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Connection.Storage;
end;

function TcxSchedulerFieldAdapter.GetVisible: Boolean;
begin
  Result := Item.Visible;
end;

procedure TcxSchedulerFieldAdapter.SetCaption(AValue: string);
begin
  Item.Caption := AValue;
end;

procedure TcxSchedulerFieldAdapter.SetItem(AValue: TcxCustomGridTableItem);
begin
  if FItem = AValue then Exit;
  if FItem <> nil then
    Item.RemoveFreeNotification(FFreeNotificator);
  FItem := AValue;
  if Item <> nil then
  begin
    Item.FreeNotification(FFreeNotificator);
    InitializeEditProperties;
  end;
end;

procedure TcxSchedulerFieldAdapter.SetReadOnly(const AValue: Boolean);
begin
  Item.Options.Editing := not AValue;
end;

procedure TcxSchedulerFieldAdapter.SetVisible(const AValue: Boolean);
begin
  Item.Visible := AValue;
end;

{ TcxSchedulerNullAdapter }

constructor TcxSchedulerNullAdapter.CreateWithItem(
  AConnection: TcxSchedulerGridConnection; AItem: TcxCustomGridTableItem);
begin
  inherited CreateWithField(AConnection, nil);
  Item := AItem;
end;

procedure TcxSchedulerNullAdapter.DeleteItem;
begin
end;

function TcxSchedulerNullAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := Null;
end;

procedure TcxSchedulerNullAdapter.SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
end;

{ TcxSchedulerIntegerFieldAdapter }

function TcxSchedulerIntegerFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := inherited GetPropertiesClass;
end;

function TcxSchedulerIntegerFieldAdapter.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxIntegerValueType;
end;

{ TcxSchedulerEventTextFieldAdapter }

function TcxSchedulerTextFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMemoProperties;
end;

{ TcxSchedulerCheckFieldAdapter }

function TcxSchedulerCheckFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

function TcxSchedulerCheckFieldAdapter.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxBooleanValueType;
end;

{ TcxSchedulerDateTimeFieldAdapter }

function TcxSchedulerDateTimeFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties;
end;

procedure TcxSchedulerDateTimeFieldAdapter.Initialize;
begin
  TcxDateEditProperties(Properties).Kind := ckDateTime;
end;

{ TcxSchedulerEventStartFieldAdapter }

function TcxSchedulerEventStartFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Start;
end;

{ TcxSchedulerEventFinishFieldAdapter }

function TcxSchedulerEventFinishFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Finish;
end;

{ TcxSchedulerImageComboFieldAdapter }

procedure TcxSchedulerImageComboFieldAdapter.AddItems(
  const AItemsValues: array of Variant);
var
  AIndex: Integer;
begin
  Properties.Items.Clear;
  for AIndex := Low(AItemsValues) to High(AItemsValues) div 3 do
    if AItemsValues[AIndex * 3 + 2] <> -1 then
      with Properties.Items.Add() do
      begin
        Description := AItemsValues[AIndex * 3];
        Value := AItemsValues[AIndex * 3 + 1];
        ImageIndex := AItemsValues[AIndex * 3 + 2];
      end;
end;

function TcxSchedulerImageComboFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageComboBoxProperties;
end;

function TcxSchedulerImageComboFieldAdapter.GetProperties: TcxImageComboBoxProperties;
begin
  Result := TcxImageComboBoxProperties(inherited Properties);
end;

{ TcxSchedulerIDFieldAdapter }

function TcxSchedulerIDFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  if Field = nil then
    Result := AEvent.ID
  else
  begin
    Result := AEvent.ParentID;
    if VarIsNumeric(Result) and (Result = -2) then
      Result := Null;
  end;
end;

procedure TcxSchedulerIDFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
end;

{ TcxSchedulerGroupIDFieldAdapter }

function TcxSchedulerGroupIDFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.GroupID;
end;

{ TcxSchedulerTypeFieldAdapter }

procedure TcxSchedulerTypeFieldAdapter.Initialize;
begin
  Properties.BeginUpdate;
  try
    Properties.Images := EventImages;
    Properties.ShowDescriptions := False;
    AddItems([cxGetResourceString(@scxNone), Integer(etNone), -1,// MaxInt,
              cxGetResourceString(@scxPattern), Integer(etPattern), Integer(2),
              cxGetResourceString(@scxOccurrence), Integer(etOccurrence), -1,//MaxInt - 1,
              cxGetResourceString(@scxException), Integer(etException), -1,// MaxInt - 2,
              cxGetResourceString(@scxCustom), Integer(etCustom), Integer(3)]);
    Properties.Buttons.Clear;
  finally
    Properties.EndUpdate;
  end;
end;

{ TcxSchedulerAllDayFieldAdapter }

function TcxSchedulerAllDayFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.AllDayEvent;
end;

procedure TcxSchedulerAllDayFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  AEvent.AllDayEvent := AValue;
end;

{ TcxSchedulerEnabledFieldAdapter }

function TcxSchedulerEnabledFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Enabled;
end;

procedure TcxSchedulerEnabledFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  AEvent.Enabled := AValue;
end;

{ TcxSchedulerReminderFieldAdapter }

function TcxSchedulerReminderFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Reminder;
end;

procedure TcxSchedulerReminderFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  AEvent.Reminder := AValue;
end;

{ TcxSchedulerLabelColorFieldAdapter }

function TcxSchedulerLabelColorFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxColorComboBoxProperties;
end;

function TcxSchedulerLabelColorFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := inherited GetValue(AEvent);
  if VarIsNumeric(Result) and (Result = clDefault) then
    Result := Null
end;

procedure TcxSchedulerLabelColorFieldAdapter.Initialize;
var
  I: Integer;
begin
  with TcxColorComboBoxProperties(Properties) do
  begin
    BeginUpdate();
    try
      PrepareList := cxplNone;
      CustomColors.Clear;
      CustomColors.AddColor(clNone, cxGetResourceString(@scxEventLabelNone));
      for I := 1 to EventLabels.Count - 1 do
        CustomColors.AddColor(EventLabels[I].Color, EventLabels[I].Caption);
      DefaultColor := clNone;
      DefaultDescription := EventLabels[0].Caption;
      DefaultColorStyle := cxdcClear;
      NamingConvention := cxncNone;
    finally
      EndUpdate();
    end;
  end;
end;

procedure TcxSchedulerLabelColorFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  if VarIsNull(AValue) or (AValue = clNone) then
    inherited SetValue(AEvent, clDefault)
  else
    inherited SetValue(AEvent, AValue);
end;

{ TcxSchedulerRecurringFieldAdapter }

function TcxSchedulerRecurringFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  if Assigned(cxGetRecurrenceDescriptionStringProc) then
    Result := cxGetRecurrenceDescriptionStringProc(AEvent.RecurrenceInfo)
  else
    Result := '';
end;

function TcxSchedulerRecurringFieldAdapter.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxStringValueType;
end;

{ TcxSchedulerStateFieldAdapter }

function TcxSchedulerStateFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.State;
end;

procedure TcxSchedulerStateFieldAdapter.Initialize;
begin
  AddItems([
    cxGetResourceString(@scxFree), tlsFree, 0,
    cxGetResourceString(@scxTentative), tlsTentative, 1,
    cxGetResourceString(@scxBusy), tlsBusy, 2,
    cxGetResourceString(@scxOutOfOffice), tlsOutOfOffice, 3]);
  Properties.Images := TimeLinePatterns;
end;

{ TcxSchedulerResourceIDFieldAdapter }

function TcxSchedulerResourceIDFieldAdapter.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxStringValueType;
end;

procedure TcxSchedulerResourceIDFieldAdapter.Initialize;
begin
end;

{ TcxSchedulerCompleteFieldAdapter }

function TcxSchedulerCompleteFieldAdapter.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxProgressBarProperties;
end;

function TcxSchedulerCompleteFieldAdapter.GetPropertiesForEditClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties;
end;

function TcxSchedulerCompleteFieldAdapter.GetValue(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := inherited GetValue(AEvent);
  if VarIsNumeric(Result) then
    Result := Min(100, Max(0, Result))
  else
    Result := 0;
end;

procedure TcxSchedulerCompleteFieldAdapter.InitializeEditProperties;
begin
  inherited InitializeEditProperties;
  if EditProperties = nil then Exit;
  with TcxSpinEditProperties(EditProperties) do
  begin
    DisplayFormat := cxGetResourceString(@scxCompleteDisplayFormat);
    MaxValue := 100;
    MinValue := 0;
  end;
end;

procedure TcxSchedulerCompleteFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  if VarIsNull(AValue) then
    AEvent.TaskComplete := 0
  else
    AEvent.TaskComplete := AValue
end;

{ TcxSchedulerLinksFieldAdapter }

function TcxSchedulerLinksFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.TaskLinks.DisplayText;
end;

{ TcxSchedulerStatusFieldAdapter }

function TcxSchedulerStatusFieldAdapter.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.TaskStatus;
end;

procedure TcxSchedulerStatusFieldAdapter.Initialize;
begin
  inherited Initialize;
  AddItems([cxGetResourceString(@scxNotStarted), Integer(tsNotStarted), 1,
    cxGetResourceString(@scxInProgress), Integer(tsInProgress), 2,
    cxGetResourceString(@scxComplete), Integer(tsComplete), 2,
    cxGetResourceString(@scxWaiting), Integer(tsWaiting), 4,
    cxGetResourceString(@scxDeferred), Integer(tsDeferred), 5]);
end;

procedure TcxSchedulerStatusFieldAdapter.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  if VarIsNull(AValue) then
    AEvent.TaskStatus := tsNotStarted
  else
    AEvent.TaskStatus := AValue;
end;

{ TcxSchedulerFieldAdapters }

constructor TcxSchedulerFieldAdapters.Create(AOwner: TcxSchedulerGridConnection);
begin
  inherited Create(TcxSchedulerFieldAdapterItemLink);
  FOwner := AOwner;
end;

function TcxSchedulerFieldAdapters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TcxSchedulerFieldAdapterItemLink }

constructor TcxSchedulerFieldAdapterItemLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldIndex := -1;
end;

destructor TcxSchedulerFieldAdapterItemLink.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TcxSchedulerFieldAdapterItemLink.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerFieldAdapterItemLink then
  begin
    AdapterClass := TcxSchedulerFieldAdapterItemLink(Source).AdapterClass;
    FieldIndex := TcxSchedulerFieldAdapterItemLink(Source).FieldIndex;
    Item := TcxSchedulerFieldAdapterItemLink(Source).Item;
  end;
end;

function TcxSchedulerFieldAdapterItemLink.IsLocked: Boolean;
begin
  Result := Connection.IsLoading or Connection.IsDestroying or
    (csUpdating in Connection.ComponentState);
end;

procedure TcxSchedulerFieldAdapterItemLink.RestoreProperties;
begin
  if AdapterClass = '' then
    AdapterClass := FAdapterClass;
  if FFieldIndex >= 0 then
    FLink.FField := TStorageAccess(Connection.Storage).DataFields[FFieldIndex];
  if FItemIndex >= 0 then
    FLink.FItem := Connection.GridView.Items[FItemIndex];
  FLink.InitializeAdapter;
end;

procedure TcxSchedulerFieldAdapterItemLink.StoreProperties;
begin
  FFieldIndex := -1;
  FItemIndex := -1;
  if Item <> nil then
    FItemIndex := Item.Index;
  if FLink.Field <> nil then
    FFieldIndex := FLink.Field.Index;
  FAdapterClass := AdapterClass;
end;

function TcxSchedulerFieldAdapterItemLink.GetAdapterClass: string;
begin
  if FLink = nil then
    Result := ''
  else
    Result := FLink.ClassName;
end;

function TcxSchedulerFieldAdapterItemLink.GetConnection: TcxSchedulerGridConnection;
begin
  Result := Collection.Owner as TcxSchedulerGridConnection;
end;

function TcxSchedulerFieldAdapterItemLink.GetFieldIndex: Integer;
begin
  if FLink.FField <> nil then
    Result := FLink.FField.Index
  else
    Result := -1;
end;

function TcxSchedulerFieldAdapterItemLink.GetItem: TcxCustomGridTableItem;
begin
  Result := FLink.Item;
end;

procedure TcxSchedulerFieldAdapterItemLink.SetAdapterClass(AValue: string);
begin
  if IsLocked then
    FAdapterClass := AValue
  else
    FLink := Connection.AddAdapterEx(Self, TcxSchedulerFieldAdapterClass(FindClass(AValue)));
end;

procedure TcxSchedulerFieldAdapterItemLink.SetFieldIndex(AValue: Integer);
begin
  FFieldIndex := AValue;
end;

procedure TcxSchedulerFieldAdapterItemLink.SetItem(AValue: TcxCustomGridTableItem);
begin
  if IsLocked then
  begin
    FItemIndex := -1;
    if AValue <> nil then
      FItemIndex := AValue.Index;
  end
  else
    FLink.Item := AValue;
end;

{ TcxSchedulerGridConnection }

constructor TcxSchedulerGridConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdapters := TcxSchedulerFieldAdapters.Create(Self);
  FGridPopupMenu := TcxSchedulerGridBuildInPopupMenu.Create(Self);
  FEventEditInfo := TcxSchedulerEventEditInfo.Create;
  FProvider := CreateDataSource;
  FProvider.FConnection := Self;
end;

destructor TcxSchedulerGridConnection.Destroy;
begin
  GridView := nil;
  Storage := nil;
  FreeAndNil(FProvider);
  FreeAndNil(FEventEditInfo);
  FreeAndNil(FGridPopupMenu);
  FreeAndNil(FAdapters);
  FreeAndNil(FGridPopupMenuController);
  inherited Destroy;
end;

{procedure TcxSchedulerGridConnection.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerGridConnection then
  begin
    GridPopupMenu := TcxSchedulerGridConnection(Source).GridPopupMenu;

  end;
  inherited Assign(Source);
end;}

procedure TcxSchedulerGridConnection.Refresh;
var
  APrevActive: Boolean;
begin
  APrevActive := Active;
  Active := False;
  Active := APrevActive;
end;

function TcxSchedulerGridConnection.AddNewEvent(
  ARecurrence: Boolean = False; AnAllDayEvent: Boolean = False): TcxSchedulerEvent;
var
  AEvent: TcxSchedulerControlEvent;
  AHandled, AModified: Boolean;
  AInfo: TcxSchedulerEventEditInfo;
  ADestroyEvent: Boolean;
begin
  Result := nil;
  ADestroyEvent := True;
  AEvent := TcxSchedulerControlEvent.Create(Storage);
  try
    AHandled := False;
    if Assigned(FOnNewEvent) then
      FOnNewEvent(Self, AEvent, AHandled);
    if not AHandled and (AEvent <> nil) then
    begin
      AEvent.AllDayEvent := AnAllDayEvent;
      AInfo := GetEventEditInfo(AEvent, ARecurrence);
      DoInitEditDialog;
      cxShowEventEditorEx(AInfo, AModified);
      ADestroyEvent := AInfo.ShowModal;
    end;
  finally
    if ADestroyEvent then
      AEvent.Free;
  end;
end;

function TcxSchedulerGridConnection.AdapterByItem(
  AItem: TcxCustomGridTableItem): TcxSchedulerFieldAdapter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Adapters[I];
    if Result.Item = AItem then
      Break;
  end;
end;

function TcxSchedulerGridConnection.AddAdapterLink(
  Adapter: TcxSchedulerFieldAdapter): TCollectionItem;
begin
  Result := FAdapters.Add;
  TcxSchedulerFieldAdapterItemLink(Result).Link := Adapter;
end;

function TcxSchedulerGridConnection.AddAdapter(AField: TcxCustomSchedulerStorageField;
  AdapterClass: TcxSchedulerFieldAdapterClass; ACaption: Pointer;
  AVisible, ACanEdit: Boolean): TcxSchedulerFieldAdapter;
begin
  try
    DoGetItemAdapter(AField, AdapterClass);
    if AdapterClass = nil then
    begin
      Result := nil;
      Exit;
    end;
    try
      Result := AdapterClass.CreateWithField(Self, AField);
    except
      TcxSchedulerFieldAdapterItemLink(FAdapters.Items[Count - 1]).FLink := nil;
      raise;
    end;
    Result.Caption := cxGetResourceString(ACaption);
    Result.ReadOnly := not ACanEdit and GetIsFieldActive(AField);
    Result.Visible := AVisible and GetIsFieldActive(AField);
    DoInitializeItem(AField, Result.Item, Result);
  except
    FAdapters.Delete(FAdapters.Count - 1);
    raise;
  end;
end;

function TcxSchedulerGridConnection.AddAdapterEx(ALink: TCollectionItem;
  AdapterClass: TcxSchedulerFieldAdapterClass): TcxSchedulerFieldAdapter;
begin
  Result := AdapterClass.Create(Self, ALink);
end;

function TcxSchedulerGridConnection.AddAdapterForCustomField(
  AField: TcxCustomSchedulerStorageField): TcxSchedulerFieldAdapter;
var
  AClass: TcxSchedulerFieldAdapterClass;
begin
  AClass := TcxSchedulerFieldAdapter;
  DoGetItemAdapter(AField, AClass);
  if AClass = nil then
  begin
    Result := nil;
    Exit;
  end;
  Result := AClass.CreateWithField(Self, AField);
  DoInitializeItem(AField, Result.Item, Result);
  Result.Caption := TStorageFieldAccess(AField).Name;
  Result.ReadOnly := True;
  Result.Visible := False;
  DoInitializeItem(AField, Result.Item, Result);
end;

procedure TcxSchedulerGridConnection.BeginUpdate;
begin
  Inc(FLockCount);
end;

function TcxSchedulerGridConnection.CreateDataSource(): TcxSchedulerStorageDataSource;
begin
  Result := TcxSchedulerStorageDataSource.Create;
end;

procedure TcxSchedulerGridConnection.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Links', ReadLinks, WriteLinks, Count > 0);
end;

procedure TcxSchedulerGridConnection.DblClickGridHandler(Sender: TObject);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if not DoGridViewDblClick(Sender) then
    with GridView.Controller do
    begin
      with Site.ScreenToClient(GetMouseCursorPos) do
        AHitTest := Site.ViewInfo.GetHitTest(X, Y);
      if AHitTest is TcxGridRecordHitTest and (FocusedRecordIndex >= 0) and
        (TcxGridRecordHitTest(AHitTest).GridRecord is TcxGridDataRow) then
        OpenEventEditDialog;
    end;
end;

function TcxSchedulerGridConnection.DoGridViewDblClick(Sender: TObject): Boolean;
begin
  Result := False;
  if Assigned(FOnGridViewDblClick) then
    FOnGridViewDblClick(Sender, Result);
end;

procedure TcxSchedulerGridConnection.DoInitEditDialog;
begin
  if Assigned(FOnInitEditDialog) then
    FOnInitEditDialog(Self, FEventEditInfo);
end;

procedure TcxSchedulerGridConnection.DoInitializeItem(
  AField: TcxCustomSchedulerStorageField; AItem: TcxCustomGridTableItem;
  AAdapter: TcxSchedulerFieldAdapter);
begin
  if Assigned(FOnInitializeItem) then
    FOnInitializeItem(Self, AField, AItem, AAdapter);
end;

procedure TcxSchedulerGridConnection.DoEditEventUsingDialog(
  AEvent: TcxSchedulerEvent; ARecurrence: Boolean = False; AReadOnly: Boolean = False;
  AForcePatternEditing: Boolean = False);
var
  AModified, AHandled: Boolean;
  AControlEvent: TcxSchedulerControlEvent;
var
  AInfo: TcxSchedulerEventEditInfo;
begin
  AControlEvent := TcxSchedulerControlEvent.Create(AEvent);
  try
    AControlEvent.Pattern := AEvent.Pattern;
    AModified := False;
    AHandled := False;
    AInfo := GetEventEditInfo(AControlEvent, ARecurrence);
    AInfo.ForcePatternEditing := AInfo.ForcePatternEditing or AForcePatternEditing;
    AInfo.ReadOnly := AReadOnly;
    DoInitEditDialog;
    if Assigned(FOnEditEvent) then
      FOnEditEvent(Self, AControlEvent, AModified, AHandled);
    if not AHandled then
      cxShowEventEditorEx(AInfo, AModified);
  finally
    AControlEvent.Free;
  end;
end;

procedure TcxSchedulerGridConnection.DoGetItemAdapter(
  AField: TcxCustomSchedulerStorageField;
  var AdapterClass: TcxSchedulerFieldAdapterClass);
begin
  if Assigned(FOnGetFieldAdapter) then
    FOnGetFieldAdapter(Self, AField, AdapterClass);
end;

procedure TcxSchedulerGridConnection.EditingGridHandler(Sender: TcxCustomGridTableView;
  AItem: TcxCustomGridTableItem; var AAllow: Boolean);
var
  AEvent: TcxSchedulerEvent;
begin
  AAllow := Storage <> nil;
  if AAllow then
  begin
    AEvent := TcxSchedulerEvent(DataController.CustomDataSource.GetRecordHandleByIndex(Sender.Controller.FocusedRecordIndex));
    AAllow := not Storage.EditingEventInfoList.IsEventEditing(AEvent, AEvent.RecurrenceIndex, AEvent.EventType);
  end;
end;

procedure TcxSchedulerGridConnection.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and FIsChanged then
    StorageChanged(nil);
end;

function TcxSchedulerGridConnection.GetEventEditInfo(AEvent: TcxSchedulerControlEvent;
  ARecurrence: Boolean = False): TcxSchedulerEventEditInfo;
begin
  Result := FEventEditInfo;
  Result.Event := AEvent;
  Result.Recurrence := ARecurrence;
  Result.RecurrenceButton := True;
  Result.ShowResources := Storage.ResourceCount > 0;
  Result.DisableShare := True;
  Result.AllowDelete := AEvent.Source <> nil;
  Result.ForcePatternEditing := AEvent.EventType = etPattern;
  Result.ShowModal := True;
end;

function TcxSchedulerGridConnection.GetFocusedEvent;
begin
  if (GridView.DataController.RecordCount = 0) or (GridView.Controller.FocusedRecordIndex < 0) then
    Result := nil
  else
    with GridView.DataController do
      Result := Storage.Events[GetRowInfo(FocusedRowIndex).RecordIndex];
end;

function TcxSchedulerGridConnection.GetIsFieldActive(
  AField: TcxCustomSchedulerStorageField): Boolean;
begin
  Result := (AField = nil) or
    TStorageFieldAccess(AField).GetIsActive;
end;

procedure TcxSchedulerGridConnection.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = GridView then
    begin
      Active := False;
      GridView := nil;
    end;
    if AComponent = Storage then
    begin
      Active := False;
      Storage := nil;
    end;
  end;
  if FGridPopupMenu <> nil then
    FGridPopupMenu.Notification(AComponent, Operation);
end;

procedure TcxSchedulerGridConnection.OpenEventEditDialog(
AForcePatternEditing: Boolean = False; AReadOnly: Boolean = False);
begin
  DoEditEventUsingDialog(GetFocusedEvent, False, AReadOnly,
    AForcePatternEditing);
end;

procedure TcxSchedulerGridConnection.Reactivate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Adapters[I].InitializeAdapter;
    DoInitializeItem(Adapters[I].Field, Adapters[I].Item, Adapters[I]);
  end;
  if (Count > 0) and (DataController <> nil) then
    GridView.DataController.CustomDataSource := Provider;
  Provider.DataChanged;
end;

procedure TcxSchedulerGridConnection.SetupView;
var
  I: Integer;
begin
  if (Storage = nil) or (GridView = nil) or (Count <> 0) then Exit;
  if IsLoading then Exit;
  GridView.BeginUpdate;
  try
    try
      with TStorageAccess(Storage) do
      begin
        AddAdapter(nil, TcxSchedulerIDFieldAdapter, @scxIDField, False, False);
        AddAdapter(FParentIDField, TcxSchedulerIDFieldAdapter, @scxParentIDField, False, False);
        AddAdapter(FGroupIDField, TcxSchedulerGroupIDFieldAdapter, @scxGroupIDField, False, False);

        AddAdapter(FStateField, TcxSchedulerStateFieldAdapter, @scxStateField, True, True);
        AddAdapter(FCaptionField, TcxSchedulerTextFieldAdapter, @scxCaptionField, True, True);
        AddAdapter(FLocationField, TcxSchedulerTextFieldAdapter, @scxLocationField, True, True);
        AddAdapter(FActualFinishField, TcxSchedulerDateTimeFieldAdapter, @scxActualFinishField, False, False);
        AddAdapter(FActualStartField, TcxSchedulerDateTimeFieldAdapter, @scxActualStartField, False, False);
        AddAdapter(FFinishField, TcxSchedulerEventFinishFieldAdapter, @scxFinishField, True, True);
        AddAdapter(FStartField, TcxSchedulerEventStartFieldAdapter, @scxStartField, True, True);
        AddAdapter(FMessageField, TcxSchedulerTextFieldAdapter, @scxMessageField, False, True);
        AddAdapter(FEventTypeField, TcxSchedulerTypeFieldAdapter, @scxEventTypeField, False, False);

        AddAdapter(FOptionsField, TcxSchedulerReminderFieldAdapter, @scxReminderField, True, True);
        AddAdapter(FOptionsField, TcxSchedulerAllDayFieldAdapter, @scxAllDayEventField, True, True);
        AddAdapter(FOptionsField, TcxSchedulerEnabledFieldAdapter, @scxEnabledField, True, True);

        AddAdapter(FLabelColorField, TcxSchedulerLabelColorFieldAdapter, @scxLabelField, True, True);

        AddAdapter(FRecurrenceInfoField, TcxSchedulerRecurringFieldAdapter, @scxRecurrenceField, True, False);
        AddAdapter(FRecurrenceIndexField, TcxSchedulerIntegerFieldAdapter, @scxRecurrenceIndexField, False, False);
        AddAdapter(FReminderDateField, TcxSchedulerDateTimeFieldAdapter, @scxReminderDateField, False, False);
        AddAdapter(FReminderMinutesBeforeStartField, TcxSchedulerIntegerFieldAdapter, @scxReminderMinutesBeforeStartField, False, False);

        AddAdapter(FResourceIDField, TcxSchedulerResourceIDFieldAdapter, @scxResourceField, False, False);

        AddAdapter(FTaskCompleteField, TcxSchedulerCompleteFieldAdapter, @scxTaskCompleteField, False, True);
        AddAdapter(FTaskLinksField, TcxSchedulerLinksFieldAdapter, @scxTaskLinksField, False, False);
        AddAdapter(FTaskIndexField, TcxSchedulerIntegerFieldAdapter, @scxTaskIndexField, False, False);
        AddAdapter(FTaskStatusField, TcxSchedulerStatusFieldAdapter, @scxTaskStatusField, False, True);

        for I := 0 to CustomFields.Count - 1 do
          AddAdapterForCustomField(CustomFields.Items[I]);
      end;
    except
      Active := False;
      raise;
    end;
  finally
    GridView.EndUpdate;
    GridView.BeginUpdate;
    try
      for I := 0 to Count - 1 do
        if Adapters[I].Item.ActuallyVisible then
          Adapters[I].Item.ApplyBestFit();
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxSchedulerGridConnection.StorageChanged(Sender: TObject);
begin
  if IsLocked then
  begin
    FIsChanged := True;
    Exit;
  end;
  Provider.DataChanged;
end;

procedure TcxSchedulerGridConnection.StorageRemoved(Sender: TObject);
begin
  Storage := nil;
end;

function TcxSchedulerGridConnection.GetAdapter(
  AIndex: Integer): TcxSchedulerFieldAdapter;
begin
  Result := TcxSchedulerFieldAdapterItemLink(FAdapters.Items[AIndex]).Link;
end;

function TcxSchedulerGridConnection.GetCount: Integer;
begin
  Result := FAdapters.Count;
end;

procedure TcxSchedulerGridConnection.Loaded;
begin
  inherited Loaded;
  RestoreItems;
  Reactivate;
end;

procedure TcxSchedulerGridConnection.RestoreItems;
var
  AIndex: Integer;
begin
  for AIndex := 0 to FAdapters.Count - 1 do
    TcxSchedulerFieldAdapterItemLink(FAdapters.Items[AIndex]).RestoreProperties;
end;

procedure TcxSchedulerGridConnection.StoreItems;
var
  AIndex: Integer;
begin
  for AIndex := 0 to FAdapters.Count - 1 do
    TcxSchedulerFieldAdapterItemLink(FAdapters.Items[AIndex]).StoreProperties;
end;

procedure TcxSchedulerGridConnection.Updated;
begin
  inherited Updated;
  RestoreItems;
end;

procedure TcxSchedulerGridConnection.Updating;
begin
  StoreItems;
  inherited Updating;
end;

function TcxSchedulerGridConnection.GetDataController: TcxCustomDataController;
begin
  Result := nil;
  if GridView <> nil then
    Result := GridView.DataController;
end;

function TcxSchedulerGridConnection.GetEvent(ARowIndex: Integer): TcxSchedulerEvent;
begin
  with GridView.DataController.GetRowInfo(ARowIndex) do
    Result := Storage.Events[RecordIndex];
end;

function TcxSchedulerGridConnection.GetEventCount: Integer;
begin
  Result := GridView.DataController.RowCount;
end;

function TcxSchedulerGridConnection.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState
end;

function TcxSchedulerGridConnection.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState
end;

function TcxSchedulerGridConnection.GetIsLocked: Boolean;
begin
  Result := FLockCount <> 0;
end;

procedure TcxSchedulerGridConnection.SetActive(AValue: Boolean);
begin
  if not AValue and IsLoading then Exit;
  if FActive and not AValue and (DataController <> nil) then
    DataController.CustomDataSource := nil;
  FActive := AValue;
  if (DataController <> nil) and AValue then
    DataController.CustomDataSource := Provider;
  if IsLoading and not AValue then Exit;
  if not Active then
    FAdapters.Clear
  else
  begin
    if (GridView <> nil) and (Storage <> nil) and Storage.IsActive then
      SetupView
    else
      Active := False;
  end;
end;

procedure TcxSchedulerGridConnection.SetGridPopupMenu(
  AValue: TcxSchedulerGridBuildInPopupMenu);
begin
  if FGridPopupMenu <> AValue then
    FGridPopupMenu := AValue;
end;

procedure TcxSchedulerGridConnection.SetGridView(AValue: TcxCustomGridTableView);
begin
  if AValue = GridView then Exit;
  Active := False;
  if GridView <> nil then
  begin
    GridView.RemoveFreeNotification(Self);
    if GridView.DataController <> nil then
      GridView.DataController.CustomDataSource := nil;
  end;
  FGridView := AValue;
  if GridView <> nil then
  begin
    GridView.FreeNotification(Self);
    GridView.OptionsBehavior.AlwaysShowEditor := False;
    GridView.OptionsBehavior.ImmediateEditor := False;
    GridView.OptionsBehavior.CellHints := True;
    GridView.OnDblClick := DblClickGridHandler;
    GridView.OnEditing := EditingGridHandler;
  end;
  SetupBuildInPopupMenu;
end;

procedure TcxSchedulerGridConnection.SetStorage(AValue: TcxCustomSchedulerStorage);
begin
  if AValue = Storage then Exit;
  Active := False;
  if Storage <> nil then
  begin
    Storage.RemoveListener(Self);
    Storage.RemoveFreeNotification(Self);
  end;
  FStorage := AValue;
  if Storage <> nil then
  begin
    Storage.AddListener(Self);
    Storage.FreeNotification(Self);
  end;
  SetupBuildInPopupMenu;
end;

procedure TcxSchedulerGridConnection.SetupBuildInPopupMenu;
begin
  if csDesigning in ComponentState then Exit;
  if FGridPopupMenuController <> nil then
    FGridPopupMenuController.Free;
  FGridPopupMenuController := TcxSchedulerGridBuildInPopupMenuController.Create(Self);
end;

procedure TcxSchedulerGridConnection.ReadLinks(Reader: TReader);
begin
  FAdapters.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(FAdapters);
end;

procedure TcxSchedulerGridConnection.WriteLinks(Writer: TWriter);
begin
  Writer.WriteCollection(FAdapters);
end;

{ TcxSchedulerGridBuildInPopupMenu }

constructor TcxSchedulerGridBuildInPopupMenu.Create(
  ASchedulerGridConnection: TcxSchedulerGridConnection);
begin
  FOwner := ASchedulerGridConnection;
  FUseBuiltInPopupMenu := True;
  FItems := [bpmiNewEvent, bpmiNewAllDayEvent, bpmiNewReccuringEvent,
    bpmiOpen, bpmiEditSeries, bpmiShowTimeAs, bpmiLabel, bpmiDelete];
end;

destructor TcxSchedulerGridBuildInPopupMenu.Destroy;
begin
  PopupMenu := nil;
  inherited Destroy;
end;

procedure TcxSchedulerGridBuildInPopupMenu.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerGridBuildInPopupMenu then
  begin
    PopupMenu := TcxSchedulerGridBuildInPopupMenu(Source).PopupMenu;
    UseBuiltInPopupMenu := TcxSchedulerGridBuildInPopupMenu(Source).UseBuiltInPopupMenu;
    Items := TcxSchedulerGridBuildInPopupMenu(Source).Items;
    OnClick := TcxSchedulerGridBuildInPopupMenu(Source).OnClick;
    OnPopup := TcxSchedulerGridBuildInPopupMenu(Source).OnPopup;
  end;
end;

procedure TcxSchedulerGridBuildInPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

procedure TcxSchedulerGridBuildInPopupMenu.SetPopupMenu(AValue: TComponent);
begin
  if FPopupMenu <> AValue then
  begin
    if (FPopupMenu <> nil) and not (csDestroying in FPopupMenu.ComponentState) then
      FPopupMenu.RemoveFreeNotification(FOwner);
    FPopupMenu := AValue;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(FOwner);
  end;
end;

{ TcxSchedulerGridBuildInPopupMenuController }

constructor TcxSchedulerGridBuildInPopupMenuController.Create(
  ASchedulerGridConnection: TcxSchedulerGridConnection);
var
  AGrid: TcxGrid;
begin
  inherited Create;
  FOwner := ASchedulerGridConnection;
  if IsPopupMenuPossible(FOwner.GridView, AGrid) then
  begin
    FGridPopupMenu := TcxGridPopupMenu.Create(FOwner);
    FInternalMenu := TPopupMenu.Create(FOwner);
    FGridPopupMenu.Grid := AGrid;
    FMenuInfo := TcxPopupMenuInfo(FGridPopupMenu.PopupMenus.Add);
    FMenuInfo.GridView := FOwner.GridView;
    FMenuInfo.HitTypes := [gvhtCell, gvhtNone];
    FMenuInfo.OnPopup := GridMenuPopup;
    FMenuInfo.PopupMenu := FInternalMenu;
  end;
end;

destructor TcxSchedulerGridBuildInPopupMenuController.Destroy;
begin
  FreeAndNil(FGridPopupMenu);
  FreeAndNil(FInternalMenu);
  inherited Destroy;
end;

function TcxSchedulerGridBuildInPopupMenuController.Popup(X, Y: Integer): Boolean;
var
  AGrid: TcxGrid;
begin
  Result := False;
  SetEvent;
  if not IsPopupMenuPossible(FOwner.GridView, AGrid){ or (FEvent = nil)} then Exit;
  CreateInternalMenu;
  Result := DoOnPopup;
  if not Result then
  begin
    if BuildInPopupMenu.UseBuiltInPopupMenu then
      Result := ShowPopupMenu(FOwner, FInternalMenu, X, Y)
    else
      Result := ShowPopupMenu(FOwner, BuildInPopupMenu.FPopupMenu, X, Y);
  end;
end;

function TcxSchedulerGridBuildInPopupMenuController.AddValidSeparator(
  AOwner: TMenuItem): TMenuItem;
begin
  if AOwner.Count > 0 then
    Result := CreateSubItem(AOwner, '-')
  else
    Result := nil
end;

procedure TcxSchedulerGridBuildInPopupMenuController.CreateInternalMenu;
begin
  FreeAndNil(FInternalMenu);
  FInternalMenu := TPopupMenu.Create(nil);
  FInternalMenu.Images := MenuImages;
  FMenuInfo.PopupMenu := FInternalMenu;
  CreateItems;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.CreateItems;
begin
  if bpmiNewEvent in Items then
    CreateSubItem(FInternalMenu.Items,
      cxGetResourceString(@scxpmNewEvent), Ord(bpmiNewEvent), 2);
  if bpmiNewAllDayEvent in Items then
    CreateSubItem(FInternalMenu.Items,
      cxGetResourceString(@scxpmNewAllDayEvent), Ord(bpmiNewAllDayEvent), -1);
  if (bpmiNewReccuringEvent in Items) and (Storage.IsRecurrenceAvailable) then
    CreateSubItem(FInternalMenu.Items,
      cxGetResourceString(@scxpmNewRecurringEvent), Ord(bpmiNewReccuringEvent), 3);

  if FEvent <> nil then
  begin
    if (bpmiOpen in Items) and not (FEvent.EventType = etPattern) then
    begin
      AddValidSeparator(FInternalMenu.Items);
      CreateSubItem(FInternalMenu.Items, cxGetResourceString(@scxpmOpen),
        Ord(bpmiOpen));
    end;
    if FEvent.EventType = etPattern then
      AddValidSeparator(FInternalMenu.Items);
    if (bpmiEditSeries in Items) and (FEvent.EventType <> etNone) then
      CreateSubItem(FInternalMenu.Items, cxGetResourceString(@scxpmEditSeries),
        Ord(bpmiEditSeries), 8);

    if Items * [bpmiLabel, bpmiShowTimeAs] <> [] then
      AddValidSeparator(FInternalMenu.Items);
    CreateTimeItems;
    CreateLabelItems;
    if bpmiDelete in Items then
    begin
      AddValidSeparator(FInternalMenu.Items);
      CreateSubItem(FInternalMenu.Items, cxGetResourceString(@scxpmDelete),
        Ord(bpmiDelete), 10).Enabled := not IsEventsReadOnly;
    end;
  end;
end;

function TcxSchedulerGridBuildInPopupMenuController.CreateSubItem(AOwner: TMenuItem;
  const ACaption: string; ACommand: Integer = -1; AImageIndex: Integer = -1;
  AEnabled: Boolean = True; AChecked: Boolean = False): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := ACaption;
  Result.Enabled := AEnabled;
  Result.ImageIndex := AImageIndex;
  Result.Checked := AChecked;
  Result.Tag := ACommand;
  Result.OnClick := OnItemClickHandler;
  AOwner.Add(Result);
end;

function TcxSchedulerGridBuildInPopupMenuController.DoOnClick(ACommand: Integer): Boolean;
var
  AItem: TcxSchedulerGridBuildInPopupMenuItem;
  ASubItemIndex: Integer;
begin
  Result := False;
  UnpackCommand(ACommand, AItem, ASubItemIndex);
  if Assigned(BuildInPopupMenu.FOnClick) then
    BuildInPopupMenu.FOnClick(BuildInPopupMenu,
      TcxSchedulerGridBuildInPopupMenuItem(ACommand), ASubItemIndex, Result);
end;

function TcxSchedulerGridBuildInPopupMenuController.DoOnPopup: Boolean;
begin
  Result := False;
  if Assigned(BuildInPopupMenu.FOnPopup) then
    BuildInPopupMenu.FOnPopup(BuildInPopupMenu, FInternalMenu, Result);
end;

function TcxSchedulerGridBuildInPopupMenuController.IsValidCommand(ACommand: Integer): Boolean;
begin
  Result := (ACommand >= Ord(bpmiNewEvent)) and (ACommand <= Ord(bpmiDelete)) or
   (ACommand in [20..20 + EventLabels.Count - 1]) or //Label
   (ACommand in [10..13]);   //TimeLine
end;

procedure TcxSchedulerGridBuildInPopupMenuController.OnItemClickHandler(Sender: TObject);
var
  ACommand: Integer;
begin
  if (Sender <> nil) and (Sender is TComponent) then
  begin
    ACommand := TComponent(Sender).Tag;
    if IsValidCommand(ACommand) and not DoOnClick(ACommand) then
      case ACommand of
        Ord(bpmiNewEvent): NewEvent;
        Ord(bpmiNewAllDayEvent): NewAllDayEvent;
        Ord(bpmiNewReccuringEvent): NewRecurrence;
        Ord(bpmiDelete): DeleteEvent;
        Ord(bpmiOpen): EditEvent;
        Ord(bpmiEditSeries): EditSeries;
        10..13:
          SetEventState(ACommand - 10);
      else
        if (ACommand >= 20) and (ACommand < 20 + EventLabels.Count) then
          SetEventLabelColor(EventLabels[ACommand - 20].Color);
      end;
  end;
end;

function TcxSchedulerGridBuildInPopupMenuController.CanEventEdit: Boolean;
begin
  Result := (FEvent <> nil) and not FEvent.ReadOnly;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.CreateLabelItems;
var
  AOwner: TMenuItem;
  I: Integer;

  function GetColorChecked(AColor: TColor): Boolean;
  begin
    if FEvent = nil then
      Result := False
    else
      Result := ColorToRgb(FEvent.LabelColor) = ColorToRgb(AColor);
  end;

  procedure CreateEventColorItem(ALabel: TcxSchedulerEventLabel);
  var
    AChecked: Boolean;
    AItem: TMenuItem;
  begin
    AChecked := GetColorChecked(ALabel.Color);
    AItem := CreateSubItem(AOwner, ALabel.Caption,
      GetCommand(bpmiLabel, ALabel.Index), -1, CanEventEdit, AChecked);
    AItem.Default := AChecked;
    EventLabels.Images.GetBitmap(ALabel.Index, AItem.Bitmap);
  end;

begin
  if not (bpmiLabel in Items) or not Storage.IsLabelColorAvailable or
    (EventLabels.Count = 0) then Exit;
  AOwner := CreateSubItem(FInternalMenu.Items, cxGetResourceString(@scxpmLabel),
    Ord(bpmiLabel));
  //label colors
  CreateEventColorItem(EventLabels[0]);
  if EventLabels.Count > 1 then
  begin
    AddValidSeparator(AOwner);
    for I := 1 to EventLabels.Count - 1 do
      CreateEventColorItem(EventLabels[I]);
  end;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.CreateTimeItems;
var
  AOwner: TMenuItem;

  function GetTimeLineChecked(AIndex: Integer): Boolean;
  begin
    if FEvent = nil then
      Result := False
    else
      Result := Integer(FEvent.State) = AIndex;
  end;

  procedure CreateTimeLineItem(const ACaption: string; AIndex: Integer);
  var
    AChecked: Boolean;
  begin
    AChecked := GetTimeLineChecked(AIndex);
    CreateSubItem(AOwner, ACaption, GetCommand(bpmiShowTimeAs, AIndex), AIndex,
      CanEventEdit, AChecked).Default := AChecked;
  end;

begin
  if not (bpmiShowTimeAs in Items) or not Storage.IsStateAvailable then Exit;
  AOwner := CreateSubItem(FInternalMenu.Items,
    cxGetResourceString(@scxpmShowTimeAs), Ord(bpmiShowTimeAs));
  AOwner.SubMenuImages := TimeLinePatterns;
  // TimeLine styles
  CreateTimeLineItem(cxGetResourceString(@scxpmFree), 0);
  CreateTimeLineItem(cxGetResourceString(@scxpmTentative), 1);
  CreateTimeLineItem(cxGetResourceString(@scxpmBusy), 2);
  CreateTimeLineItem(cxGetResourceString(@scxpmOutOfOffice), 3);
end;

procedure TcxSchedulerGridBuildInPopupMenuController.DeleteEvent;
begin
  FOwner.FGridView.Controller.DeleteSelection;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.EditEvent;
begin
  FOwner.OpenEventEditDialog(False, IsEventsReadOnly);
end;

procedure TcxSchedulerGridBuildInPopupMenuController.EditSeries;
begin
  FOwner.OpenEventEditDialog(True, IsEventsReadOnly);
end;

function TcxSchedulerGridBuildInPopupMenuController.GetBuildInPopupMenu: TcxSchedulerGridBuildInPopupMenu;
begin
  Result := FOwner.GridPopupMenu;
end;

function TcxSchedulerGridBuildInPopupMenuController.GetCommand(
  AItem: TcxSchedulerGridBuildInPopupMenuItem; ASubItemIndex: Integer): Integer;
begin
  Result := Ord(AItem);
  case AItem of
    bpmiShowTimeAs:
      if (ASubItemIndex >= 0) and (ASubItemIndex <= 3) then
        Result := 10 + ASubItemIndex;
    bpmiLabel:
      if (ASubItemIndex >= 0) and (ASubItemIndex < EventLabels.Count) then
        Result := 20 + ASubItemIndex;
  end;
end;

function TcxSchedulerGridBuildInPopupMenuController.GetItems: TcxSchedulerGridBuildInPopupMenuItems;
begin
  Result := BuildInPopupMenu.Items;
end;

function TcxSchedulerGridBuildInPopupMenuController.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := FOwner.Storage;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.GridMenuPopup(ASenderMenu: TComponent;
  AHitTest: TcxCustomGridHitTest; X, Y: Integer);
begin
  Popup(X, Y);
end;

function TcxSchedulerGridBuildInPopupMenuController.IsEventsReadOnly: Boolean;
var
  I, ACount: Integer;
begin
  Result := False;
  ACount := FOwner.FGridView.Controller.SelectedRecordCount;
  I := 0;
  while not Result and (I < ACount) do
  begin
    with FOwner.GridView do
      Result := GetStorage.Events[DataController.GetRowInfo(
        Controller.SelectedRecords[I].Index).RecordIndex].ReadOnly;
    Inc(I);
  end;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.NewAllDayEvent;
begin
  FOwner.AddNewEvent(False, True);
end;

procedure TcxSchedulerGridBuildInPopupMenuController.NewEvent;
begin
  FOwner.AddNewEvent;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.NewRecurrence;
begin
  FOwner.AddNewEvent(True);
end;

procedure TcxSchedulerGridBuildInPopupMenuController.SetEvent;
begin
  FEvent := FOwner.GetFocusedEvent;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.SetEventLabelColor(AColor: Integer);
begin
  if FEvent = nil then Exit;
  if FEvent.EventType = etOccurrence then
    FEvent.Pattern.LabelColor := AColor
  else
    FEvent.LabelColor := AColor;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.SetEventState(AState: Integer);
begin
  if FEvent = nil then Exit;
  FEvent.State := AState;
end;

procedure TcxSchedulerGridBuildInPopupMenuController.UnpackCommand(ACommand: Integer;
  out AItem: TcxSchedulerGridBuildInPopupMenuItem; out ASubItemIndex: Integer);
begin
  AItem := bpmiOpen;
  ASubItemIndex := -1;
  if not IsValidCommand(ACommand) then Exit;
  if ACommand in [20..20 + EventLabels.Count - 1] then
  begin
    AItem := bpmiLabel;
    ASubItemIndex := ACommand - 20;
  end
  else
    if ACommand in [10..13] then
    begin
      AItem := bpmiShowTimeAs;
      ASubItemIndex := ACommand - 10;
    end
    else
      AItem := TcxSchedulerGridBuildInPopupMenuItem(ACommand);
end;

procedure RegisterAssistants;
begin
  TcxSchedulerNullAdapter.Register;
  TcxSchedulerIntegerFieldAdapter.Register;
  TcxSchedulerTextFieldAdapter.Register;
  TcxSchedulerCheckFieldAdapter.Register;
  TcxSchedulerDateTimeFieldAdapter.Register;
  TcxSchedulerEventFinishFieldAdapter.Register;
  TcxSchedulerEventStartFieldAdapter.Register;
  TcxSchedulerImageComboFieldAdapter.Register;
  TcxSchedulerIDFieldAdapter.Register;
  TcxSchedulerGroupIDFieldAdapter.Register;
  TcxSchedulerTypeFieldAdapter.Register;
  TcxSchedulerAllDayFieldAdapter.Register;
  TcxSchedulerEnabledFieldAdapter.Register;
  TcxSchedulerReminderFieldAdapter.Register;
  TcxSchedulerLabelColorFieldAdapter.Register;
  TcxSchedulerRecurringFieldAdapter.Register;
  TcxSchedulerStateFieldAdapter.Register;
  TcxSchedulerResourceIDFieldAdapter.Register;
  TcxSchedulerCompleteFieldAdapter.Register;
  TcxSchedulerLinksFieldAdapter.Register;
  TcxSchedulerStatusFieldAdapter.Register;
  RegisterClasses([TcxSchedulerFieldAdapterItemLink]);
end;

procedure UnregisterAssistants;
begin
  TcxSchedulerNullAdapter.Unregister;
  TcxSchedulerIntegerFieldAdapter.Unregister;
  TcxSchedulerTextFieldAdapter.Unregister;
  TcxSchedulerCheckFieldAdapter.Unregister;
  TcxSchedulerDateTimeFieldAdapter.Unregister;
  TcxSchedulerEventFinishFieldAdapter.Unregister;
  TcxSchedulerEventStartFieldAdapter.Unregister;
  TcxSchedulerImageComboFieldAdapter.Unregister;
  TcxSchedulerIDFieldAdapter.Unregister;
  TcxSchedulerGroupIDFieldAdapter.Unregister;
  TcxSchedulerTypeFieldAdapter.Unregister;
  TcxSchedulerAllDayFieldAdapter.Unregister;
  TcxSchedulerEnabledFieldAdapter.Unregister;
  TcxSchedulerReminderFieldAdapter.Unregister;
  TcxSchedulerLabelColorFieldAdapter.Unregister;
  TcxSchedulerRecurringFieldAdapter.Unregister;
  TcxSchedulerStateFieldAdapter.Unregister;
  TcxSchedulerResourceIDFieldAdapter.Unregister;
  TcxSchedulerCompleteFieldAdapter.Unregister;
  TcxSchedulerLinksFieldAdapter.Unregister;
  TcxSchedulerStatusFieldAdapter.Unregister;
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;

end.

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

unit cxSchedulerEventEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, cxTextEdit, cxMemo, cxImageComboBox, cxCalendar,
  cxTimeEdit, cxLookAndFeelPainters, cxDropDownEdit, cxCheckBox,
  cxSpinEdit, cxMaskEdit, cxControls, cxContainer, cxEdit, cxButtons,
  cxSchedulerStorage, cxLookAndFeels,
  cxCheckComboBox, cxGroupBox, Menus, cxGraphics, cxLabel, dxBevel, ComCtrls,
  dxCore, cxDateUtils, cxSchedulerEditorFormManager, cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxForms;

type
  { TcxShedulerStandardEventEditorFormStyleInfo }

  TcxShedulerStandardEventEditorFormStyleInfo = class(TcxShedulerEventEditorFormStyleInfo)
  public
    class function CreateEditor(AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm; override;
    class function GetName: string; override;
  end;

  { TcxSchedulerCustomEditorFormHelper }

  TcxSchedulerEditorIsValidEvent = function: Boolean of object;

  TcxSchedulerCustomEditorFormHelper = class
  private
    FEvent: TcxSchedulerControlEvent;
    FForceSeries: Boolean;
    FForm: TForm;
    FLockCount: Integer;
    FModified: Boolean;
    FReadOnly: Boolean;
    FOnChanged: TNotifyEvent;
    FOnIsValid: TcxSchedulerEditorIsValidEvent;
    FOnSaveChanges: TNotifyEvent;

    function GetStorage: TcxCustomSchedulerStorage;
    function IsValid: Boolean;
    procedure SetModified(AValue: Boolean);
    procedure SetReadOnly(AValue: Boolean);
  protected
    procedure DoChanged; virtual;
    procedure DoExecuteAction(var AModalResult: TModalResult); virtual;
    function DoIsValid: Boolean; virtual;
    procedure DoRefresh; virtual;
    procedure DoSaveChanges; virtual;
    function GetEvent: TcxSchedulerControlEvent; virtual;
    function IsEditorModal: Boolean; virtual;

    procedure Cancel(var AModalResult: TModalResult); virtual;
    procedure Close(var AModalResult: TModalResult); virtual;
    procedure Delete(var AModalResult: TModalResult); virtual;
    procedure Save(var AModalResult: TModalResult); virtual;

    property Form: TForm read FForm;
    property OnIsValid: TcxSchedulerEditorIsValidEvent read FOnIsValid write FOnIsValid;
  public
    constructor Create(AForm: TForm; AEvent: TcxSchedulerControlEvent); virtual;

    procedure ExecuteAction(AModalResult: TModalResult);
    procedure Changed;
    function ValidateOccurrenceDate(AEvent: TcxSchedulerControlEvent; ANewStart, ANewFinish: TDateTime): Boolean;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;

    property Event: TcxSchedulerControlEvent read GetEvent;
    property ForceSeries: Boolean read FForceSeries write FForceSeries;
    property Modified: Boolean read FModified write SetModified;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnSaveChanges: TNotifyEvent read FOnSaveChanges write FOnSaveChanges;
  end;

  { TcxSchedulerEventEditorFormHelper }

  TcxSchedulerEventEditorFormHelper = class(TcxSchedulerCustomEditorFormHelper)
  private
    FAllowDelete: Boolean;
    FAllowHiddenEvents: Boolean;
    FAllowRecurrence: Boolean;
    FAllowShare: Boolean;
    FBiasTime: TDateTime;
    FCanDelete: Boolean;
    FSource: TcxSchedulerEvent;
    FDeleteExceptions: Boolean;
    FDeleteSeries: Boolean;
    FDuration: TDateTime;
    FEditingInfo: TcxSchedulerEditingEventInfo;
    FIntersection: Boolean;
    FIsIntersectionValid: Boolean;
    FIsEventRecurring: Boolean;
    FIsRemindersActive: Boolean;
    FLookAndFeel: TcxLookAndFeel;
    FNeedCheckLossOfExceptions: Boolean;
    FPatternStart: TDateTime;
    FPatternFinish: TDateTime;
    FRecurrenceActivate: Boolean;
    FRecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
    FShowResources: Boolean;
    FShowTaskComplete: Boolean;
    FOnDelete: TcxOnDeleteEventFunc;
    FOnApplyEventTime: TNotifyEvent;
    function HasStorageEvent: Boolean;
    procedure SetAllowDelete(AValue: Boolean);
    procedure SetAllowRecurrence(AValue: Boolean);
    procedure SetIntersection(const Value: Boolean);
    procedure SetIsIntersectionValid(const Value: Boolean);
  protected
    procedure ApplyEventTime;
    procedure CalculateIntersection;
    procedure UpdateEditingInfo;

    procedure DoExecuteAction(var AModalResult: TModalResult); override;
    procedure DoInitialize(AEventInfo: TcxSchedulerEventEditInfo;
      AEditingInfo: TcxSchedulerEditingEventInfo); virtual;
    procedure DoRefresh; override;
    function GetEvent: TcxSchedulerControlEvent; override;
    procedure NormalizeTopMostWindows;
    function PostEvent: TcxSchedulerEvent; virtual;
    procedure RefreshRecurrenceInfo;

    procedure Cancel(var AModalResult: TModalResult); override;
    procedure Close(var AModalResult: TModalResult); override;
    procedure Delete(var AModalResult: TModalResult); override;
    procedure Save(var AModalResult: TModalResult); override;

    property Source: TcxSchedulerEvent read FSource;
    property CanDelete: Boolean read FCanDelete;
    property EditingInfo: TcxSchedulerEditingEventInfo read FEditingInfo;
    property IsEventRecurring: Boolean read FIsEventRecurring;
  public
    constructor Create(AForm: TForm; AEvent: TcxSchedulerControlEvent); override;
    destructor Destroy; override;

    function CanClose: Boolean;
    function GetRecurrenceDescription: string;
    procedure Initialize(AEventInfo: TcxSchedulerEventEditInfo;
      AEditingInfo: TcxSchedulerEditingEventInfo);
    function IsNewEvent: Boolean;
    function CanDeleteEvent: Boolean;
    procedure PopulateLabelColor(AProperties: TcxImageComboBoxProperties);
    procedure PopulateShowTimeAsPanel(AProperties: TcxImageComboBoxProperties);

    property AllowDelete: Boolean read FAllowDelete write SetAllowDelete;
    property AllowHiddenEvents: Boolean read FAllowHiddenEvents write FAllowHiddenEvents;
    property AllowRecurrence: Boolean read FAllowRecurrence write SetAllowRecurrence;
    property AllowShare: Boolean read FAllowShare write FAllowShare;
    property BiasTime: TDateTime read FBiasTime write FBiasTime;
    property DeleteExceptions: Boolean read FDeleteExceptions write FDeleteExceptions;
    property DeleteSeries: Boolean read FDeleteSeries write FDeleteSeries;
    property Duration: TDateTime read FDuration write FDuration;
    property Intersection: Boolean read FIntersection write SetIntersection;
    property IsIntersectionValid: Boolean read FIsIntersectionValid write SetIsIntersectionValid;
    property IsRemindersActive: Boolean read FIsRemindersActive;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
    property NeedCheckLossOfExceptions: Boolean read FNeedCheckLossOfExceptions
      write FNeedCheckLossOfExceptions;
    property PatternStart: TDateTime read FPatternStart write FPatternStart;
    property PatternFinish: TDateTime read FPatternFinish write FPatternFinish;
    property RecurrenceActivate: Boolean read FRecurrenceActivate write FRecurrenceActivate;
    property RecurrenceInfo: TcxSchedulerEventRecurrenceInfo read FRecurrenceInfo;
    property ShowResources: Boolean read FShowResources write FShowResources;
    property ShowTaskComplete: Boolean read FShowTaskComplete write FShowTaskComplete;
    property OnDelete: TcxOnDeleteEventFunc read FOnDelete write FOnDelete;
    property OnApplyEventTime: TNotifyEvent read FOnApplyEventTime write FOnApplyEventTime;
  end;

  TcxSchedulerEditorHelperClass = class of TcxSchedulerCustomEditorFormHelper;

  { TcxSchedulerEventStandardEditorFormHelper }

  TcxSchedulerEventStandardEditorFormHelper = class(TcxSchedulerEventEditorFormHelper);

  { TcxSchedulerCustomEditorForm }

  TcxSchedulerCustomEditorForm = class(TdxForm, IcxShedulerCustomEditorForm)
  private
    FHelper: TcxSchedulerCustomEditorFormHelper;

    function GetEvent: TcxSchedulerControlEvent;
    function GetModified: Boolean;
    function GetReadOnly: Boolean;
    procedure HelperChangedHandler(Sender: TObject);
    function HelperIsValidHandler: Boolean;
    procedure HelperSaveChangesHandler(Sender: TObject);
    procedure SetModified(AValue: Boolean);
    procedure SetReadOnly(AValue: Boolean);

    // IcxShedulerCustomEditorForm
    function IcxShedulerCustomEditorForm.GetModified = GetModified;
    procedure IcxShedulerCustomEditorForm.SetModified = SetModified;
  protected
    procedure ClearModifiedFlag; virtual;
    procedure DoHelperChanged; virtual;
    function DoHelperIsValid: Boolean; virtual;
    procedure DoHelperSaveChanges; virtual;
    function GetFormColor: TColor; virtual;
    function GetHelperClass: TcxSchedulerEditorHelperClass; virtual;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure InitializeControls; virtual;
    procedure InitializeHelper(AEvent: TcxSchedulerControlEvent); virtual;
    procedure LoadEventValuesIntoControls; virtual;
    procedure UpdateEventValuesFromControls; virtual;

    property Helper: TcxSchedulerCustomEditorFormHelper read FHelper;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor CreateEx(AEvent: TcxSchedulerControlEvent); virtual;
    destructor Destroy; override;

    procedure Initialize; overload;

    property Event: TcxSchedulerControlEvent read GetEvent;
    property Modified: Boolean read GetModified write SetModified;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  { TcxSchedulerCustomEventEditor }

  TcxSchedulerCustomEventEditor = class(TcxSchedulerCustomEditorForm, IcxSchedulerEventEditorForm)
  private
    function GetHelper: TcxSchedulerEventEditorFormHelper;
    function GetAllowDelete: Boolean;
    function GetAllowHiddenEvents: Boolean;
    function GetAllowRecurrence: Boolean;
    function GetAllowShare: Boolean;
    function GetBiasTime: TDateTime;
    function GetForceSeries: Boolean;
    function GetIntersection: Boolean;
    function GetOnDeleteFunc: TcxOnDeleteEventFunc;
    function GetRecurrenceActivate: Boolean;
    function GetShowResources: Boolean;
    function GetShowTaskComplete: Boolean;
    procedure SetAllowDelete(const Value: Boolean);
    procedure SetAllowHiddenEvents(const Value: Boolean);
    procedure SetAllowRecurrence(const Value: Boolean);
    procedure SetAllowShare(const Value: Boolean);
    procedure SetBiasTime(const Value: TDateTime);
    procedure SetForceSeries(const Value: Boolean);
    procedure SetIntersection(const Value: Boolean);
    procedure SetOnDeleteFunc(const Value: TcxOnDeleteEventFunc);
    procedure SetRecurrenceActivate(const Value: Boolean);
    procedure SetShowResources(const Value: Boolean);
    procedure SetShowTaskComplete(const Value: Boolean);

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    function GetHelperClass: TcxSchedulerEditorHelperClass; override;
    // IcxShedulerEventEditorForm
    procedure CreateParams(var Params: TCreateParams); override;
    function GetForm: TForm;
    procedure Initialize(AEventInfo: TcxSchedulerEventEditInfo;
      AEditingInfo: TcxSchedulerEditingEventInfo); overload; virtual;
    property Helper: TcxSchedulerEventEditorFormHelper read GetHelper;
  public
    property AllowDelete: Boolean read GetAllowDelete write SetAllowDelete;
    property AllowHiddenEvents: Boolean read GetAllowHiddenEvents write SetAllowHiddenEvents;
    property AllowRecurrence: Boolean read GetAllowRecurrence write SetAllowRecurrence;
    property AllowShare: Boolean read GetAllowShare write SetAllowShare;
    property BiasTime: TDateTime read GetBiasTime write SetBiasTime;
    property ForceSeries: Boolean read GetForceSeries write SetForceSeries;
    property Intersection: Boolean read GetIntersection write SetIntersection;
    property RecurrenceActivate: Boolean read GetRecurrenceActivate write SetRecurrenceActivate;
    property ShowResources: Boolean read GetShowResources write SetShowResources;
    property ShowTaskComplete: Boolean read GetShowTaskComplete write SetShowTaskComplete;
    property OnDeleteFunc: TcxOnDeleteEventFunc read GetOnDeleteFunc write SetOnDeleteFunc;
  end;

  { TcxSchedulerEventEditorForm }

  TcxSchedulerEventEditorForm = class(TcxSchedulerCustomEventEditor, IcxSchedulerEventEditorForm)
    btnFindTime: TcxButton;
    cxGroupBox1: TcxGroupBox;
    lbInformation: TcxLabel;
    teSubject: TcxTextEdit;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    pnlInformation: TdxLayoutGroup;
    lbSubject: TdxLayoutItem;
    lbLocation: TdxLayoutItem;
    teLocation: TcxTextEdit;
    lbLabel: TdxLayoutItem;
    icbLabel: TcxImageComboBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    pnlCaption: TdxLayoutGroup;
    lbStartTime: TdxLayoutItem;
    deStart: TcxDateEdit;
    liStart: TdxLayoutItem;
    teStart: TcxTimeEdit;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutItem4: TdxLayoutItem;
    cbAllDayEvent: TcxCheckBox;
    lbEndTime: TdxLayoutItem;
    deEnd: TcxDateEdit;
    liFinish: TdxLayoutItem;
    teEnd: TcxTimeEdit;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    pnlTime: TdxLayoutGroup;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    lbTaskComplete: TdxLayoutItem;
    seTaskComplete: TcxSpinEdit;
    lbTaskStatus: TdxLayoutItem;
    cbxTaskStatus: TcxComboBox;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutSeparatorItem3: TdxLayoutSeparatorItem;
    pnlTaskComplete: TdxLayoutGroup;
    lbRecurrence: TdxLayoutItem;
    lbRecurrencePattern: TcxLabel;
    pnlRecurrenceInfo: TdxLayoutGroup;
    dxLayoutSeparatorItem4: TdxLayoutSeparatorItem;
    lbResource: TdxLayoutItem;
    cbResources: TcxCheckComboBox;
    pnlResource: TdxLayoutGroup;
    dxLayoutItem6: TdxLayoutItem;
    cbReminder: TcxCheckBox;
    dxLayoutItem7: TdxLayoutItem;
    cbReminderMinutesBeforeStart: TcxComboBox;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    lbShowTimeAs: TdxLayoutItem;
    icbShowTimeAs: TcxImageComboBox;
    pnlReminder: TdxLayoutGroup;
    pnlShowTimeAs: TdxLayoutGroup;
    pnlPlaceHolder: TdxLayoutGroup;
    dxLayoutSeparatorItem5: TdxLayoutSeparatorItem;
    dxLayoutItem8: TdxLayoutItem;
    meMessage: TcxMemo;
    dxLayoutSeparatorItem6: TdxLayoutSeparatorItem;
    pnlMessage: TdxLayoutGroup;
    dxLayoutItem9: TdxLayoutItem;
    btnCancel: TcxButton;
    dxLayoutItem10: TdxLayoutItem;
    btnDelete: TcxButton;
    dxLayoutItem11: TdxLayoutItem;
    btnOk: TcxButton;
    pnlThreeButtons: TdxLayoutGroup;
    pnlRecurrence: TdxLayoutItem;
    btnRecurrence: TcxButton;
    pnlButtons: TdxLayoutGroup;
    procedure OnChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure cbAllDayEventPropertiesChange(Sender: TObject);
    procedure StartDateChanged(Sender: TObject);
    procedure cbReminderClick(Sender: TObject);
    procedure cbReminderMinutesBeforeStartPropertiesValidate(
      Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure cbReminderMinutesBeforeStartPropertiesPopup(Sender: TObject);
    procedure OnEventTimeChanged(Sender: TObject);
    procedure btnRecurrenceClick(Sender: TObject);
    procedure OnResourceIDChanged(Sender: TObject);
    procedure btnFindTimeClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure seTaskCompleteChange(Sender: TObject);
    procedure cbxTaskStatusChange(Sender: TObject);
    procedure cbResourcesPropertiesClickCheck(Sender: TObject;
      ItemIndex: Integer; var AllowToggle: Boolean);
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
  private
    FRecurrenceInfoHeight: Integer;
    procedure DoApplyEventTime(Sender: TObject);
    function GetAllowHiddenEvents: Boolean;
    function GetDeleteExceptions: Boolean;
    function GetEventName: string;
    function GetForceSeries: Boolean;
    function GetHelper: TcxSchedulerEventStandardEditorFormHelper;
    procedure SetAllowHiddenEvents(AValue: Boolean);
    procedure SetDeleteExceptions(AValue: Boolean);
    procedure SetForceSeries(AValue: Boolean);
  protected
    procedure ApplyEventTime;
    procedure CheckControlStates; virtual;
    function CheckTimeRange(var AStart, AFinish: TDateTime): Boolean; virtual;
    procedure CheckVisible; virtual;
    procedure ClearModifiedFlag; override;
    procedure DoHelperChanged; override;
    function DoHelperIsValid: Boolean; override;
    procedure DoHelperSaveChanges; override;
    function FindAvailableTime: Boolean; virtual;
    procedure GetEditingEventTime(var AStart, AFinish: TDateTime; var AState: Integer); virtual;
    function GetFormColor: TColor; override;
    function GetHelperClass: TcxSchedulerEditorHelperClass; override;
    function GetRecurrenceDescription: string; virtual;
    function GetResourcesPanelVisible: Boolean; virtual;
    procedure Initialize(AEventInfo: TcxSchedulerEventEditInfo;
      AEditingInfo: TcxSchedulerEditingEventInfo); override;
    procedure InitializeControls; override;
    procedure InitializeHelper(AEvent: TcxSchedulerControlEvent); override;
    procedure InitializeTimeControls(const AStart, AFinish: TDateTime; AllDayEvent: Boolean); virtual;
    procedure InitLabelColor;
    procedure InitReminderPanel;
    procedure InitResources;
    procedure InitShowTimeAsPanel;
    procedure InitTaskCompletePanel;
    function IsValid: Boolean; virtual;
    procedure LoadEventValuesIntoControls; override;
    procedure RefreshRecurrenceInfo; virtual;
    procedure SaveResourceID; virtual;
    procedure SetActiveControl; virtual;
    procedure SetCaptions; virtual;
    procedure SetReminderMinutesBeforeStart; virtual;

    property DeleteExceptions: Boolean read GetDeleteExceptions write SetDeleteExceptions;
    property Helper: TcxSchedulerEventStandardEditorFormHelper read GetHelper;
  public
    property AllowHiddenEvents: Boolean read GetAllowHiddenEvents write SetAllowHiddenEvents;
    property EventName: string read GetEventName;
    property ForceSeries: Boolean read GetForceSeries write SetForceSeries;
  end;

  TcxSchedulerEventEditor = TcxSchedulerEventEditorForm; //workaround for Delphi 2005

  TcxSchedulerEventEditorClass = class of TcxSchedulerCustomEventEditor;

implementation

uses
  Variants, DateUtils, Math, cxSchedulerStrs, cxSchedulerUtils,
  cxSchedulerDialogs, cxVariants, cxSchedulerRecurrenceSelectionDialog;

{$R *.dfm}

type
  TdxTouchScrollUIModeManagerAccess = class(TdxTouchScrollUIModeManager);
  TdxHybridScrollbarManagersAccess = class(TdxHybridScrollbarManagers);

{ TcxShedulerStandardEventEditorFormStyleInfo }

class function TcxShedulerStandardEventEditorFormStyleInfo.CreateEditor(AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm;
begin
  Result := cxEventEditorClass.CreateEx(AEvent);
end;

class function TcxShedulerStandardEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Standard';
end;

{ TcxSchedulerCustomEditorFormHelper }

constructor TcxSchedulerCustomEditorFormHelper.Create(AForm: TForm;
  AEvent: TcxSchedulerControlEvent);
begin
  inherited Create;
  FForm := AForm;
  FEvent := AEvent;
end;

procedure TcxSchedulerCustomEditorFormHelper.ExecuteAction(AModalResult: TModalResult);
begin
  case AModalResult of
    mrOk:
      if IsValid then
        Save(AModalResult)
      else
        AModalResult := mrNone;
    mrCancel:
      Cancel(AModalResult);
    mrClose:
      Close(AModalResult);
    mrAbort:
      Delete(AModalResult);
  end;
  DoExecuteAction(AModalResult);
end;

procedure TcxSchedulerCustomEditorFormHelper.Changed;
begin
  if not IsUpdating then
    DoChanged;
end;

function TcxSchedulerCustomEditorFormHelper.ValidateOccurrenceDate(
  AEvent: TcxSchedulerControlEvent; ANewStart, ANewFinish: TDateTime): Boolean;
var
  AStatus: TcxOccurrenceDateStatus;
  AMessage: string;
begin
  Result := True;
  if Event.EventType in [etOccurrence, etCustom] then
  begin
    AStatus := Event.Pattern.ValidateOccurrenceTimeBounds(Event, ANewStart, ANewFinish);
    if AStatus <> odsValid then
    begin
      AMessage := cxGetOccurrenceDateStatusMessage(Event, AStatus);
      MessageBox(Form.Handle, PChar(AMessage), nil, MB_ICONEXCLAMATION or MB_OK);
      Result := False;
    end;
  end;
end;

procedure TcxSchedulerCustomEditorFormHelper.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxSchedulerCustomEditorFormHelper.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TcxSchedulerCustomEditorFormHelper.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

function TcxSchedulerCustomEditorFormHelper.IsUpdating: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TcxSchedulerCustomEditorFormHelper.Cancel(var AModalResult: TModalResult);
begin
end;

procedure TcxSchedulerCustomEditorFormHelper.Close(var AModalResult: TModalResult);
begin
end;

procedure TcxSchedulerCustomEditorFormHelper.Delete(var AModalResult: TModalResult);
begin
end;

procedure TcxSchedulerCustomEditorFormHelper.Save(var AModalResult: TModalResult);
begin
  FModified := False;
end;

procedure TcxSchedulerCustomEditorFormHelper.DoChanged;
begin
  CallNotify(FOnChanged, Self);
end;

procedure TcxSchedulerCustomEditorFormHelper.DoExecuteAction(var AModalResult: TModalResult);
begin
end;

function TcxSchedulerCustomEditorFormHelper.DoIsValid: Boolean;
begin
  Result := True;
  if Assigned(OnIsValid) then
    Result := OnIsValid;
end;

procedure TcxSchedulerCustomEditorFormHelper.DoRefresh;
begin
end;

procedure TcxSchedulerCustomEditorFormHelper.DoSaveChanges;
begin
  CallNotify(FOnSaveChanges, Self);
end;

function TcxSchedulerCustomEditorFormHelper.GetEvent: TcxSchedulerControlEvent;
begin
  Result := FEvent;
end;

function TcxSchedulerCustomEditorFormHelper.IsEditorModal: Boolean;
begin
  Result := fsModal in Form.FormState;
end;

function TcxSchedulerCustomEditorFormHelper.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Event.Storage;
end;

function TcxSchedulerCustomEditorFormHelper.IsValid: Boolean;
begin
  Result := DoIsValid;
end;

procedure TcxSchedulerCustomEditorFormHelper.SetModified(AValue: Boolean);
begin
  FModified := AValue;
  Changed;
end;

procedure TcxSchedulerCustomEditorFormHelper.SetReadOnly(AValue: Boolean);
begin
  if FReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    Changed;
  end;
end;

{ TcxSchedulerEventEditorFormHelper }

constructor TcxSchedulerEventEditorFormHelper.Create(AForm: TForm;
  AEvent: TcxSchedulerControlEvent);
begin
  inherited;
  FIsIntersectionValid := True;
  FIsEventRecurring := AEvent.IsRecurring;
  FSource := AEvent.Source;
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FBiasTime := AEvent.TimeBias;
  RefreshRecurrenceInfo;
  if AEvent.Pattern <> nil then
  begin
    FPatternStart := AEvent.Pattern.Start - FBiasTime + AEvent.StorageTimeBias;
    FPatternFinish := AEvent.Pattern.Finish - FBiasTime + AEvent.StorageTimeBias;
    FRecurrenceInfo.Assign(AEvent.Pattern.RecurrenceInfo);
  end
  else
  begin
    FPatternStart := AEvent.Start;
    FPatternFinish := AEvent.Finish;
    FRecurrenceInfo.Assign(AEvent.RecurrenceInfo);
  end;
  FAllowShare := True;
  FNeedCheckLossOfExceptions := (AEvent.Pattern <> nil) and AEvent.Pattern.HasExceptions;
  FDuration := AEvent.Duration;
end;

destructor TcxSchedulerEventEditorFormHelper.Destroy;
begin
  FreeAndNil(FRecurrenceInfo);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

function TcxSchedulerEventEditorFormHelper.CanClose: Boolean;
var
  AMsgRes: Integer;
begin
  Result := (Form.ModalResult in [mrOk, mrAbort]) or not Modified or
    ((Form.ModalResult = mrCancel) and not IsIntersectionValid);
  if not Result then
  begin
    if Form.ModalResult in [mrCancel, mrNone] then
    begin
      AMsgRes := MessageDlg(cxGetResourceString(@scxExitConfirmation),
        mtWarning, [mbYes, mbNo, mbCancel], 0);
      Result := AMsgRes in [mrYes, mrNo];
      if AMsgRes = mrYes then
      begin
        if (Event.Pattern <> nil) and (Event.EventType = etNone) then
          Event.Pattern.DeleteExceptions;
        ExecuteAction(mrOk);
        if Form.ModalResult = mrNone then
          Result := False;
      end;
    end;
    if not Result then
      Form.ModalResult := mrNone;
  end;
end;

procedure TcxSchedulerEventEditorFormHelper.UpdateEditingInfo;
var
  AEvent: TcxSchedulerControlEvent;
begin
  AEvent := EditingInfo.CreateEvent(Source, Event, False);
  if Source.IsRecurring and not ForceSeries then
    AEvent.Pattern := Source.Pattern;
  EditingInfo.ReplaceEvent(AEvent);
end;

procedure TcxSchedulerEventEditorFormHelper.DoExecuteAction(var AModalResult: TModalResult);
begin
  inherited;
  Form.ModalResult := AModalResult;
end;

function TcxSchedulerEventEditorFormHelper.GetRecurrenceDescription: string;
begin
  Event.RecurrenceInfo.DisplayTimeBias := 0;
  if (Event.Pattern <> nil) and not DeleteExceptions then
  begin
    Event.Pattern.RecurrenceInfo.DisplayTimeBias := -Storage.TimeBias +
      BiasTime - DateTimeHelper.CurrentTimeZoneBias;
    Result := cxGetRecurrenceDescriptionStringProc(Event.Pattern.RecurrenceInfo, True)
  end
  else
    Result := cxGetRecurrenceDescriptionStringProc(Event.RecurrenceInfo, True);
end;

procedure TcxSchedulerEventEditorFormHelper.Initialize(AEventInfo: TcxSchedulerEventEditInfo;
  AEditingInfo: TcxSchedulerEditingEventInfo);
begin
  BeginUpdate;
  try
    DoInitialize(AEventInfo, AEditingInfo);
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerEventEditorFormHelper.IsNewEvent: Boolean;
begin
  Result := not Assigned(Event.Source);
end;

function TcxSchedulerEventEditorFormHelper.CanDeleteEvent: Boolean;
var
  AIsOccurrence: Boolean;
begin
  Result := not Assigned(OnDelete) or OnDelete(Event);
  if Result then
  begin
    if (Event.Pattern <> nil) and Event.IsRecurring then
      if not ForceSeries then
      begin
        if not cxShowRecurrenceSelectionDialog(Event, rsmDeleting, LookAndFeel,
          AIsOccurrence) then
        begin
          Result := False;
          Exit;
        end;
        DeleteSeries := not AIsOccurrence;
      end
      else
        DeleteSeries := True;
  end;
  if Result and Modified then
    Result := MessageDlg(cxGetResourceString(@scxDeleteConfirmation),
      mtWarning, [mbYes, mbNo], 0) = mrYes
end;

procedure TcxSchedulerEventEditorFormHelper.PopulateLabelColor(
  AProperties: TcxImageComboBoxProperties);
var
  AItem: TcxImageComboboxItem;
  I: Integer;
begin
  for I := 0 to EventLabels.Count - 1 do
  begin
    AItem := TcxImageComboboxItem(AProperties.Items.Add);
    AItem.Description := EventLabels[I].Caption;
    AItem.ImageIndex := I;
    AItem.Value := EventLabels[I].Color;
  end;
  AProperties.Images := EventLabels.Images;
end;

procedure TcxSchedulerEventEditorFormHelper.PopulateShowTimeAsPanel(
  AProperties: TcxImageComboBoxProperties);
const
  ATimeTypes: array[0..3] of Pointer = (@scxFree, @scxTentative, @scxBusy,
    @scxOutOfOffice);
var
  I: Integer;
  AItem: TcxImageComboboxItem;
begin
  for I := 0 to 3 do
  begin
    AItem := TcxImageComboboxItem(AProperties.Items.Add);
    AItem.Description := cxGetResourceString(ATimeTypes[I]);
    AItem.ImageIndex := I;
    AItem.Value := I;
  end;
  AProperties.Images := TimeLinePatterns;
end;

procedure TcxSchedulerEventEditorFormHelper.DoInitialize(AEventInfo: TcxSchedulerEventEditInfo;
  AEditingInfo: TcxSchedulerEditingEventInfo);
begin
  FEditingInfo := AEditingInfo;
  FLookAndFeel.MasterLookAndFeel := AEventInfo.LookAndFeel;
  FShowResources := AEventInfo.ShowResources;
  FShowTaskComplete := AEventInfo.ShowTaskComplete;
  ForceSeries := AEventInfo.ForcePatternEditing;
  RecurrenceActivate := AEventInfo.Recurrence;
  FIntersection := AEventInfo.Intersection;
  FIsRemindersActive := AEventInfo.IsRemindersActive;
  FCanDelete := AEventInfo.AllowDelete;
  AllowDelete := CanDelete and (Event.Source <> nil);
  AllowHiddenEvents := AEventInfo.AllowHiddenEvents;
  AllowRecurrence := AEventInfo.RecurrenceButton and Assigned(cxRecurrenceEventEditorClass) and
    Event.Storage.IsRecurrenceAvailable;
  AllowShare := not AEventInfo.DisableShare;
  OnDelete := AEventInfo.OnDeleteFunc;
  BiasTime := AEventInfo.BiasTime;
  ReadOnly := AEventInfo.ReadOnly;
end;

procedure TcxSchedulerEventEditorFormHelper.DoRefresh;
begin
  inherited DoRefresh;
  RefreshRecurrenceInfo;
  NeedCheckLossOfExceptions := (Event.Pattern <> nil) and Event.Pattern.HasExceptions;
  AllowDelete := CanDelete and Assigned(Event.Source);
  FIsEventRecurring := Event.IsRecurring;
  FDeleteExceptions := False;
  Modified := False;
end;

function TcxSchedulerEventEditorFormHelper.GetEvent: TcxSchedulerControlEvent;
begin
  if EditingInfo = nil then
    Result := inherited GetEvent
  else
    Result := EditingInfo.Event;
end;

procedure TcxSchedulerEventEditorFormHelper.NormalizeTopMostWindows;
begin
  if dxApplicationActivateWindowHelper <> nil then
    dxApplicationActivateWindowHelper.NormalizeTopMostWindows(Form);
end;

function TcxSchedulerEventEditorFormHelper.PostEvent: TcxSchedulerEvent;

  function GetStorageEvent: TcxSchedulerEvent;
  begin
    if not HasStorageEvent then
      Result := Storage.CreateEvent
    else
      Result := Event.Source;
  end;

  procedure CheckDeleteException;
  begin
    if FDeleteExceptions and (Event.Pattern <> nil) then
      Event.Pattern.DeleteExceptions;
  end;

  function ChangeType(ADest, ASource: TcxSchedulerEvent; AType: TcxEventType;
    AEndEditing: Boolean = True; AKeepTime: Boolean = False): TcxSchedulerEvent;
  var
    AUTCStart, AUTCFinish: TDateTime;
  begin
    Result := ADest;
    Result.BeginEditing;
    AUTCStart := Result.UTCStart;
    AUTCFinish := Result.UTCFinish;
    if ASource <> nil then
      Result.Assign(ASource);
    Result.EventType := AType;
    if AKeepTime then
    begin
      Result.UTCStart := AUTCStart;
      Result.UTCFinish := AUTCFinish;
    end;
    if AEndEditing then
      Result.EndEditing;
  end;

begin
  CheckDeleteException;
  Result := Event.Source;
  if not FModified and (Event.Source <> nil) then
    Exit;
  if Event.IsRecurring then
  begin
    if ForceSeries then
    begin
      if Event.Pattern <> nil then
        Result := ChangeType(Event.Pattern, Event, etPattern, True, not DeleteExceptions)
      else
        Result := ChangeType(GetStorageEvent, Event, etPattern);
    end
    else
    begin
      Result := ChangeType(GetStorageEvent, Event, etCustom, False);
      try
        Result.ParentID := Event.Pattern.ID;
      finally
        Result.EndEditing;
      end;
    end;
  end
  else
    if Event.Pattern <> nil then
      Result := ChangeType(Event.Pattern, Event, Event.EventType)
    else
      Result := ChangeType(GetStorageEvent, Event, etNone);
end;

procedure TcxSchedulerEventEditorFormHelper.RefreshRecurrenceInfo;
begin
  FRecurrenceInfo.Free;
  FRecurrenceInfo := TcxSchedulerEventRecurrenceInfo.Create(Event);
end;

procedure TcxSchedulerEventEditorFormHelper.Cancel(var AModalResult: TModalResult);
begin
  if CanClose then
  begin
    FModified := False;
    Close(AModalResult);
  end
  else
    AModalResult := mrNone;
end;

procedure TcxSchedulerEventEditorFormHelper.Close(var AModalResult: TModalResult);
begin
  Form.Close;
end;

procedure TcxSchedulerEventEditorFormHelper.Delete(var AModalResult: TModalResult);
begin
  if CanDeleteEvent then
    EditingInfo.DeleteEvent(DeleteSeries)
  else
    AModalResult := mrNone;
end;

procedure TcxSchedulerEventEditorFormHelper.Save(var AModalResult: TModalResult);
var
  AStorage: TcxCustomSchedulerStorage;
  ALink: TcxObjectLink;
begin
  AStorage := Storage;
  AStorage.EditingEventInfoList.BeginUpdate;
  try
    AStorage.BeginUpdate;
    try
      DoSaveChanges;
      Event.IsDataValid := False;
      FSource := PostEvent;
    finally
      ALink := cxAddObjectLink(FSource);
      try
        AStorage.EndUpdate;
        if ALink.Ref = nil then
          FSource := nil;
      finally
        cxRemoveObjectLink(ALink);
      end;
    end;
    if FSource <> nil then
      UpdateEditingInfo
    else
      AModalResult := mrClose;
  finally
    AStorage.EditingEventInfoList.EndUpdate;
  end;
  inherited Save(AModalResult);
end;

procedure TcxSchedulerEventEditorFormHelper.ApplyEventTime;
begin
  CallNotify(FOnApplyEventTime, Self);
end;

procedure TcxSchedulerEventEditorFormHelper.CalculateIntersection;
begin
  FIsIntersectionValid := Intersection;
  if IsIntersectionValid or IsUpdating then
    Exit;
  ApplyEventTime;
  Event.SkipExceptions := DeleteExceptions;
  FIsIntersectionValid := not Event.Conflicts(AllowHiddenEvents);
end;

function TcxSchedulerEventEditorFormHelper.HasStorageEvent: Boolean;
begin
  Result := not ((Event.Source = nil) or (Event.EventType = etOccurrence));
end;

procedure TcxSchedulerEventEditorFormHelper.SetAllowDelete(AValue: Boolean);
begin
  if FAllowDelete <> AValue then
  begin
    FAllowDelete := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerEventEditorFormHelper.SetAllowRecurrence(AValue: Boolean);
begin
  if FAllowRecurrence <> AValue then
  begin
    FAllowRecurrence := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerEventEditorFormHelper.SetIntersection(
  const Value: Boolean);
begin
  if FIntersection <> Value then
  begin
    FIntersection := Value;
    FIsIntersectionValid := False;
  end;
end;

procedure TcxSchedulerEventEditorFormHelper.SetIsIntersectionValid(const Value: Boolean);
begin
  if IsIntersectionValid <> Value then
  begin
    FIsIntersectionValid := Value;
    CalculateIntersection;
  end;
end;

{ TcxSchedulerCustomEditorForm }

constructor TcxSchedulerCustomEditorForm.CreateEx(AEvent: TcxSchedulerControlEvent);
begin
  inherited Create(nil);
  FHelper := GetHelperClass.Create(Self, AEvent);
end;

destructor TcxSchedulerCustomEditorForm.Destroy;
begin
  cxDialogsMetricsStore.StoreMetrics(Self);
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TcxSchedulerCustomEditorForm.ClearModifiedFlag;
begin
  Modified := False;
end;

procedure TcxSchedulerCustomEditorForm.DoHelperChanged;
begin
//do nothing
end;

function TcxSchedulerCustomEditorForm.DoHelperIsValid: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerCustomEditorForm.DoHelperSaveChanges;
begin
//do nothing
end;

function TcxSchedulerCustomEditorForm.GetFormColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxSchedulerCustomEditorForm.GetHelperClass:
  TcxSchedulerEditorHelperClass;
begin
  Result := TcxSchedulerCustomEditorFormHelper;
end;

function TcxSchedulerCustomEditorForm.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Helper.Storage;
end;

procedure TcxSchedulerCustomEditorForm.Initialize;
begin
  InitializeHelper(Event);
  Helper.BeginUpdate;
  try
    InitializeControls;
    LoadEventValuesIntoControls;
    ClearModifiedFlag;
  finally
    Helper.EndUpdate;
  end;
end;

procedure TcxSchedulerCustomEditorForm.InitializeControls;
begin
end;

procedure TcxSchedulerCustomEditorForm.InitializeHelper(
  AEvent: TcxSchedulerControlEvent);
begin
  Helper.OnChanged := HelperChangedHandler;
  Helper.OnSaveChanges := HelperSaveChangesHandler;
  Helper.OnIsValid := HelperIsValidHandler;
end;

procedure TcxSchedulerCustomEditorForm.LoadEventValuesIntoControls;
begin
end;

procedure TcxSchedulerCustomEditorForm.UpdateEventValuesFromControls;
begin
  DoHelperSaveChanges;
end;

function TcxSchedulerCustomEditorForm.GetEvent: TcxSchedulerControlEvent;
begin
  Result := Helper.Event;
end;

function TcxSchedulerCustomEditorForm.GetModified: Boolean;
begin
  Result := Helper.Modified;
end;

function TcxSchedulerCustomEditorForm.GetReadOnly: Boolean;
begin
  Result := Helper.ReadOnly;
end;

procedure TcxSchedulerCustomEditorForm.HelperChangedHandler(Sender: TObject);
begin
  DoHelperChanged;
end;

function TcxSchedulerCustomEditorForm.HelperIsValidHandler: Boolean;
begin
  Result := DoHelperIsValid;
end;

procedure TcxSchedulerCustomEditorForm.HelperSaveChangesHandler(Sender: TObject);
begin
  UpdateEventValuesFromControls;
end;

procedure TcxSchedulerCustomEditorForm.SetModified(AValue: Boolean);
begin
  Helper.Modified := AValue;
end;

procedure TcxSchedulerCustomEditorForm.SetReadOnly(AValue: Boolean);
begin
  Helper.ReadOnly := AValue;
end;

{ TcxSchedulerCustomEventEditor }

function TcxSchedulerCustomEventEditor.GetHelperClass: TcxSchedulerEditorHelperClass;
begin
  Result := TcxSchedulerEventEditorFormHelper;
end;

procedure TcxSchedulerCustomEventEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

function TcxSchedulerCustomEventEditor.GetForm: TForm;
begin
  Result := Self;
end;

procedure TcxSchedulerCustomEventEditor.Initialize(AEventInfo: TcxSchedulerEventEditInfo;
  AEditingInfo: TcxSchedulerEditingEventInfo);
begin
  Initialize;
end;

function TcxSchedulerCustomEventEditor.GetIntersection: Boolean;
begin
  Result := Helper.Intersection;
end;

function TcxSchedulerCustomEventEditor.GetOnDeleteFunc: TcxOnDeleteEventFunc;
begin
  Result := Helper.OnDelete;
end;

function TcxSchedulerCustomEventEditor.GetRecurrenceActivate: Boolean;
begin
  Result := Helper.RecurrenceActivate;
end;

function TcxSchedulerCustomEventEditor.GetShowResources: Boolean;
begin
  Result := Helper.ShowResources;
end;

function TcxSchedulerCustomEventEditor.GetShowTaskComplete: Boolean;
begin
  Result := Helper.ShowTaskComplete;
end;

function TcxSchedulerCustomEventEditor.GetAllowDelete: Boolean;
begin
  Result := Helper.AllowDelete;
end;

function TcxSchedulerCustomEventEditor.GetAllowHiddenEvents: Boolean;
begin
  Result := Helper.AllowHiddenEvents;
end;

function TcxSchedulerCustomEventEditor.GetAllowRecurrence: Boolean;
begin
  Result := Helper.AllowRecurrence;
end;

function TcxSchedulerCustomEventEditor.GetAllowShare: Boolean;
begin
  Result := Helper.AllowShare;
end;

function TcxSchedulerCustomEventEditor.GetBiasTime: TDateTime;
begin
  Result := Helper.BiasTime;
end;

function TcxSchedulerCustomEventEditor.GetForceSeries: Boolean;
begin
  Result := Helper.ForceSeries;
end;

procedure TcxSchedulerCustomEventEditor.SetAllowDelete(const Value: Boolean);
begin
  Helper.AllowDelete := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetAllowHiddenEvents(
  const Value: Boolean);
begin
  Helper.AllowHiddenEvents := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetAllowRecurrence(
  const Value: Boolean);
begin
  Helper.AllowRecurrence := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetAllowShare(const Value: Boolean);
begin
  Helper.AllowShare := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetBiasTime(const Value: TDateTime);
begin
  Helper.BiasTime := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetForceSeries(const Value: Boolean);
begin
  Helper.ForceSeries := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetIntersection(const Value: Boolean);
begin
  Helper.Intersection := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetOnDeleteFunc(
  const Value: TcxOnDeleteEventFunc);
begin
  Helper.OnDelete := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetRecurrenceActivate(
  const Value: Boolean);
begin
  Helper.RecurrenceActivate := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetShowResources(const Value: Boolean);
begin
  Helper.ShowResources := Value;
end;

procedure TcxSchedulerCustomEventEditor.SetShowTaskComplete(
  const Value: Boolean);
begin
  Helper.ShowTaskComplete := Value;
end;

function TcxSchedulerCustomEventEditor.GetHelper: TcxSchedulerEventEditorFormHelper;
begin
  Result := TcxSchedulerEventEditorFormHelper(inherited Helper);
end;

procedure TcxSchedulerCustomEventEditor.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_ACTIVE then
    Helper.NormalizeTopMostWindows;
end;

{ TcxSchedulerEventEditor }

procedure TcxSchedulerEventEditorForm.SaveResourceID;
var
  I: Integer;
begin
  Event.ResourceID := Null;
  for I := 0 to cbResources.Properties.Items.Count - 1 do
    if cbResources.States[I] = cbsChecked then
      Event.ShareWithResource(Storage.ResourceIDs[cbResources.Properties.Items[I].Tag]);
end;

procedure TcxSchedulerEventEditorForm.SetActiveControl;
begin
  if (Event.Source = nil) and teSubject.CanFocus then
    ActiveControl := teSubject
  else
    if meMessage.CanFocus then
      ActiveControl := meMessage
    else
      if deStart.CanFocus then
        ActiveControl := deStart;
end;

procedure TcxSchedulerEventEditorForm.ApplyEventTime;
var
  AState: Integer;
  AStart, AFinish: TDateTime;
begin
  GetEditingEventTime(AStart, AFinish, AState);
  Event.State :=  AState;
  Event.Start := AStart;
  Event.Finish := AFinish;
  if Helper.ShowResources then
    SaveResourceID;
end;

procedure TcxSchedulerEventEditorForm.CheckControlStates;
begin
  if Helper.IsUpdating then
    Exit;
  Helper.CalculateIntersection;
  pnlInformation.Visible := not Helper.IsIntersectionValid;
  pnlRecurrence.Visible := Helper.AllowRecurrence;
  pnlCaption.Enabled := not ReadOnly;
  pnlReminder.Enabled := not ReadOnly;
  pnlTime.Enabled := not ReadOnly;
  pnlResource.Enabled := not ReadOnly;
  pnlTaskComplete.Enabled := not ReadOnly;
  meMessage.Properties.ReadOnly := ReadOnly;
  icbShowTimeAs.Enabled := not ReadOnly;
  icbLabel.Enabled := not ReadOnly;
  btnOk.Enabled := not ReadOnly and (Modified or Helper.IsNewEvent) and
    Helper.IsIntersectionValid;
  btnDelete.Enabled := not (ReadOnly or Helper.IsNewEvent)
    and Helper.AllowDelete;
end;

function TcxSchedulerEventEditorForm.CheckTimeRange(var AStart, AFinish: TDateTime): Boolean;
var
  AEventStart, AEventFinish: TDateTime;
begin
  Event.GetValidTaskTimeRange(AStart, AFinish);
  AEventStart := deStart.Date + teStart.Time;
  AEventFinish := deEnd.Date + teEnd.Time;
  Result := (AStart <= AEventStart) and (AFinish >= AEventFinish);
end;

procedure TcxSchedulerEventEditorForm.CheckVisible;
var
  ATimeVisible: Boolean;
begin
  if Helper.IsUpdating then
    Exit;
  pnlTaskComplete.Visible := Helper.ShowTaskComplete;
  pnlResource.Visible := GetResourcesPanelVisible;
  ATimeVisible := (Event.EventType = etNone) or
    ((Event.EventType in [etOccurrence, etCustom]) and not ForceSeries);
  if ATimeVisible then
  begin
    pnlTime.Visible := True;
    pnlRecurrenceInfo.Visible := False;
  end
  else
    if Assigned(cxGetRecurrenceDescriptionStringProc) then
    begin
      RefreshRecurrenceInfo;
      pnlRecurrenceInfo.Visible := True;
      pnlTime.Visible := False;
    end;
  teStart.Time := dxTimeOf(Event.Start);
  teEnd.Time := dxTimeOf(Event.Finish);
  liStart.Visible := not cbAllDayEvent.Checked;
  liFinish.Visible := not cbAllDayEvent.Checked;
  CheckControlStates;
end;

procedure TcxSchedulerEventEditorForm.ClearModifiedFlag;

  procedure CheckComponent(AComponent: TComponent);
  begin
    if AComponent is TcxCustomEdit then
      TcxCustomEdit(AComponent).EditModified := False;
  end;

var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    CheckComponent(Components[I]);
  inherited ClearModifiedFlag;
end;

procedure TcxSchedulerEventEditorForm.DoApplyEventTime(Sender: TObject);
begin
  ApplyEventTime;
end;

procedure TcxSchedulerEventEditorForm.DoHelperChanged;
begin
  inherited;
  CheckControlStates;
end;

function TcxSchedulerEventEditorForm.DoHelperIsValid: Boolean;
begin
  Result := IsValid;
end;

procedure TcxSchedulerEventEditorForm.DoHelperSaveChanges;
var
  AStart, AFinish: TDateTime;
begin
  inherited;
  if teSubject.EditModified then
    Event.Caption := teSubject.Text;
  if teLocation.EditModified then
    Event.Location := teLocation.Text;
  if meMessage.EditModified then
    Event.Message := meMessage.Text;
  if (Event.EventType = etNone) or (Event.Source = nil)
    or ((Event.EventType = etPattern) and not Event.Source.IsRecurring) or not ForceSeries then
  begin
    AStart := deStart.Date;
    AFinish := deEnd.Date + Ord(cbAllDayEvent.Checked);
    if not cbAllDayEvent.Checked then
    begin
      AStart := AStart + teStart.Time;
      AFinish := AFinish + teEnd.Time;
    end;
    Event.Start := AStart;
    Event.Finish := AFinish;
    if cbAllDayEvent.EditModified then
      Event.AllDayEvent := cbAllDayEvent.Checked;
  end;
  if icbShowTimeAs.EditModified then
    Event.State := icbShowTimeAs.ItemIndex;
  if icbLabel.EditModified and (icbLabel.ItemIndex <> -1) then
    Event.LabelColor := EventLabels[icbLabel.ItemIndex].Color;
  if pnlResource.Visible and cbResources.EditModified then
    SaveResourceID;
  if pnlReminder.Visible then
  begin
    if cbReminder.EditModified then
      Event.Reminder := cbReminder.Checked;
    if cbReminderMinutesBeforeStart.EditModified then
      SetReminderMinutesBeforeStart;
  end;
end;

function TcxSchedulerEventEditorForm.FindAvailableTime: Boolean;
var
  AStart, AFinish: TDateTime;
begin
  Result := Storage.FindAvailableTime(Event, AllowHiddenEvents, AStart, AFinish);
  if Result then
    InitializeTimeControls(AStart, AFinish, Event.AllDayEvent);
  if Result and (Event.EventType = etPattern) then
    ApplyEventTime;
end;

procedure TcxSchedulerEventEditorForm.GetEditingEventTime(
  var AStart, AFinish: TDateTime; var AState: Integer);
begin
  AStart := deStart.Date + teStart.Time;
  AFinish := deEnd.Date + teEnd.Time + Ord(cbAllDayEvent.Checked);
  AState := icbShowTimeAs.ItemIndex;
end;

function TcxSchedulerEventEditorForm.GetFormColor: TColor;
begin
  Result := Helper.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

function TcxSchedulerEventEditorForm.GetHelperClass: TcxSchedulerEditorHelperClass;
begin
  Result := TcxSchedulerEventStandardEditorFormHelper;
end;

function TcxSchedulerEventEditorForm.GetRecurrenceDescription: string;
begin
  Result := Helper.GetRecurrenceDescription;
end;

function TcxSchedulerEventEditorForm.GetResourcesPanelVisible: Boolean;
begin
  Result := Helper.ShowResources;
end;

procedure TcxSchedulerEventEditorForm.Initialize(AEventInfo: TcxSchedulerEventEditInfo;
  AEditingInfo: TcxSchedulerEditingEventInfo);
begin
  Helper.Initialize(AEventInfo, AEditingInfo);
  inherited Initialize(AEventInfo, AEditingInfo);
  SetControlLookAndFeel(Self, Helper.LookAndFeel);
end;

procedure TcxSchedulerEventEditorForm.InitializeControls;
begin
  InitReminderPanel;
  InitShowTimeAsPanel;
  InitLabelColor;
  InitTaskCompletePanel;
  pnlInformation.Visible := False;
  SetCaptions;
  teStart.ActiveProperties.Use24HourFormat := Is24HourTimeFormat;
  teEnd.ActiveProperties.Use24HourFormat := Is24HourTimeFormat;
end;

procedure TcxSchedulerEventEditorForm.InitializeHelper(AEvent: TcxSchedulerControlEvent);
begin
  inherited InitializeHelper(AEvent);
  Helper.OnApplyEventTime := DoApplyEventTime;
end;

procedure TcxSchedulerEventEditorForm.InitializeTimeControls(
  const AStart, AFinish: TDateTime; AllDayEvent: Boolean);
begin
  deStart.Date := dxDateOf(AStart);
  teStart.Time := dxTimeOf(AStart);
  deEnd.Date := dxDateOf(AFinish) - Ord(AllDayEvent);
  teEnd.Time := dxTimeOf(AFinish);
  cbAllDayEvent.Checked := AllDayEvent;
end;

procedure TcxSchedulerEventEditorForm.InitLabelColor;
begin
  icbLabel.Enabled := Storage.IsLabelColorAvailable and (EventLabels.Count > 0);
  Helper.PopulateLabelColor(icbLabel.Properties);
end;

procedure TcxSchedulerEventEditorForm.InitReminderPanel;
begin
  pnlReminder.Visible := Helper.IsRemindersActive;
  if pnlReminder.Visible then
    cxComboBoxPopulateWithTimeIntervals(cbReminderMinutesBeforeStart);
end;

procedure TcxSchedulerEventEditorForm.InitResources;
var
  I, J, AResourceIndex: Integer;
  AResource: TcxSchedulerStorageResourceItem;
begin
  AResourceIndex := -1;
  with cbResources.Properties.Items do
  begin
    BeginUpdate;
    Clear;
    J := 0;
    for I := 0 to Storage.ResourceCount - 1 do
    begin
      AResource := Storage.Resources.ResourceItems[I];
      if VarIsNull(AResource.ResourceID) or VarIsEmpty(AResource.ResourceID) then
        Continue;
      if (not AResource.Visible or not AResource.CanModify) and
        not Event.IsSharedWithResource(AResource) then
        Continue;
      with TcxCheckComboBoxItem(Add) do
      begin
        Description := AResource.DisplayName;
        Enabled := AResource.CanModify;
        Tag := I;
      end;
      cbResources.States[J] := TcxCheckBoxState(
        Event.IsSharedWithResource(AResource.ResourceID));
      Inc(J);
    end;
    EndUpdate;
  end;
  cbResources.ItemIndex := AResourceIndex;
end;

procedure TcxSchedulerEventEditorForm.InitShowTimeAsPanel;
begin
  Helper.PopulateShowTimeAsPanel(icbShowTimeAs.Properties);
end;

procedure TcxSchedulerEventEditorForm.InitTaskCompletePanel;
var
  AStatus: TcxSchedulerEventTaskStatus;
begin
  if pnlTaskComplete.Visible then
    seTaskComplete.Value := Event.TaskComplete;
  with cbxTaskStatus.Properties do
  begin
    BeginUpdate;
    try
      Items.Clear;
      for AStatus := tsNotStarted to tsDeferred do
        Items.Add(cxGetResourceString(sEventTaskStatus[Integer(AStatus)]));
    finally
      EndUpdate(True);
      cbxTaskStatus.ItemIndex := Integer(Event.TaskStatus);
    end;
  end;
end;

function TcxSchedulerEventEditorForm.IsValid: Boolean;
var
  AStart, AFinish, ANewStart, ANewFinish: TDateTime;
begin
  ANewStart := deStart.Date + teStart.Time;
  ANewFinish := deEnd.Date + teEnd.Time;
  Result := not (DateTimeHelper.RoundTime(ANewStart) > DateTimeHelper.RoundTime(ANewFinish));
  if not Result then
  begin
    MessageDlg(cxGetResourceString(@scxWrongTimeBounds), mtWarning, [mbOk], 0);
    if deEnd.CanFocusEx then
      ActiveControl := deEnd;
  end
  else
  begin
    Result := CheckTimeRange(AStart, AFinish);
    if not Result then
    begin
      MessageDlg(Format(cxGetResourceString(@scxTaskWrongTimeBounds),
        [cxDateTimeToText(AStart), cxDateTimeToText(AFinish)]), mtWarning, [mbOk], 0);
      if deStart.CanFocusEx then
        ActiveControl := deStart;
    end
    else
      if not Helper.ValidateOccurrenceDate(Event, ANewStart, ANewFinish) then
      begin
        Result := False;
        if deStart.CanFocusEx then
          ActiveControl := deStart;
      end;
  end;
end;

procedure TcxSchedulerEventEditorForm.LoadEventValuesIntoControls;
var
  AAllowTimeChange: Boolean;
begin
  teSubject.Text := Event.Caption;
  teSubject.Enabled := Storage.IsCaptionAvailable;

  teLocation.Text := Event.Location;
  teLocation.Enabled := Storage.IsLocationAvailable;

  icbLabel.Enabled := Storage.IsLabelColorAvailable and (EventLabels.Count > 0);
  icbLabel.ItemIndex := EventLabels.IndexOfColor(Event.LabelColor);

  meMessage.Text := Event.Message;
  meMessage.Enabled := Storage.IsMessageAvailable;

  InitializeTimeControls(Event.Start, Event.Finish, Event.AllDayEvent);

  icbShowTimeAs.ItemIndex := Integer(Event.State);
  icbShowTimeAs.Enabled := Storage.IsStateAvailable;

  cbReminder.Checked := Event.Reminder;
  cbReminderMinutesBeforeStart.Text := cxMinutesToTextProc(Event.ReminderMinutesBeforeStart);
  cbReminderMinutesBeforeStart.Enabled := cbReminder.Checked;

  AAllowTimeChange := not Event.IsGroup or not Event.HasChildren;
  deStart.Enabled := AAllowTimeChange;
  teStart.Enabled := AAllowTimeChange;
  deEnd.Enabled := AAllowTimeChange;
  teEnd.Enabled := AAllowTimeChange;
  cbAllDayEvent.Enabled := AAllowTimeChange;

  InitResources;
end;

procedure TcxSchedulerEventEditorForm.RefreshRecurrenceInfo;
var
  ADelta: Integer;
begin
  if Assigned(cxGetRecurrenceDescriptionStringProc) then
  begin
    lbRecurrencePattern.Caption := GetRecurrenceDescription;
    ADelta := cxGetLabelGrowDelta(lbRecurrencePattern);
    if ADelta > 0 then
      pnlRecurrenceInfo.Height := FRecurrenceInfoHeight + ADelta;
  end;
end;

procedure TcxSchedulerEventEditorForm.SetCaptions;
begin
  Caption := cxGetResourceString(@scxEvent) + ' - ' + EventName;
  // events
  lbInformation.Caption := cxGetResourceString(@scxEventsConflict);
  lbResource.Caption := cxGetResourceString(@scxResource);
  lbSubject.Caption := cxGetResourceString(@scxSubject);
  lbLocation.Caption := cxGetResourceString(@scxLocation);
  lbLabel.Caption := cxGetResourceString(@scxLabel);
  lbShowTimeAs.Caption := cxGetResourceString(@scxShowTimeAs);
  lbStartTime.Caption := cxGetResourceString(@scxStartTime);
  lbEndTime.Caption := cxGetResourceString(@scxEndTime);
  cbAllDayEvent.Caption := cxGetResourceString(@scxAllDayEvent);
  cbReminder.Caption := cxGetResourceString(@scxReminder);
  lbRecurrence.Caption := cxGetResourceString(@scxRecurrenceLabel);
  lbTaskComplete.Caption := cxGetResourceString(@scxTaskComplete);
  lbTaskStatus.Caption := cxGetResourceString(@scxTaskStatus);
  // buttons
  btnFindTime.Caption := cxGetResourceString(@scxFindAvailableTime);
  btnOk.Caption := cxGetResourceString(@scxOk);
  btnCancel.Caption := cxGetResourceString(@scxCancel);
  btnDelete.Caption := cxGetResourceString(@scxDelete);
  btnRecurrence.Caption := cxGetResourceString(@scxRecurrence);
  btnRecurrence.Enabled := Storage.IsRecurrenceAvailable;
end;

procedure TcxSchedulerEventEditorForm.SetReminderMinutesBeforeStart;
var
  AMinutes: Integer;
begin
  if cxTextToMinutesProc(cbReminderMinutesBeforeStart.Text, AMinutes) then
    Event.ReminderMinutesBeforeStart := AMinutes;
end;

function TcxSchedulerEventEditorForm.GetAllowHiddenEvents: Boolean;
begin
  Result := Helper.AllowHiddenEvents;
end;

function TcxSchedulerEventEditorForm.GetDeleteExceptions: Boolean;
begin
  Result := Helper.DeleteExceptions;
end;

function TcxSchedulerEventEditorForm.GetEventName: string;
begin
  Result := cxGetResourceString(@scxUntitled);
  if Event.Caption <> '' then
    Result := Event.Caption;
end;

function TcxSchedulerEventEditorForm.GetForceSeries: Boolean;
begin
  Result := Helper.ForceSeries;
end;

function TcxSchedulerEventEditorForm.GetHelper:
  TcxSchedulerEventStandardEditorFormHelper;
begin
  Result := TcxSchedulerEventStandardEditorFormHelper(inherited Helper);
end;

procedure TcxSchedulerEventEditorForm.SetAllowHiddenEvents(
  AValue: Boolean);
begin
  Helper.AllowHiddenEvents := AValue;
end;

procedure TcxSchedulerEventEditorForm.SetDeleteExceptions(
  AValue: Boolean);
begin
  Helper.DeleteExceptions := AValue;
end;

procedure TcxSchedulerEventEditorForm.SetForceSeries(AValue: Boolean);
begin
  Helper.ForceSeries := AValue;
end;

procedure TcxSchedulerEventEditorForm.OnChanged(Sender: TObject);
begin
  Modified := True;
end;

procedure TcxSchedulerEventEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  TdxTouchScrollUIModeManagerAccess.CheckUIVisibility(Handle);
  TdxHybridScrollbarManagersAccess.CheckScrollbarsVisibility(Handle);
  dxApplicationActivateWindowHelper.RemoveWindow(Self);
  Action := caFree;
end;

procedure TcxSchedulerEventEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Helper.CanClose;
end;

procedure TcxSchedulerEventEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and not HasOpenedPopupWindow(ActiveControl) then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TcxSchedulerEventEditorForm.FormShow(Sender: TObject);
begin
  FRecurrenceInfoHeight := pnlRecurrenceInfo.Height;
  CheckVisible;
  SetActiveControl;
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
  dxApplicationActivateWindowHelper.AddWindow(Self);
end;

procedure TcxSchedulerEventEditorForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if Helper.RecurrenceActivate then
    btnRecurrenceClick(nil);
end;

procedure TcxSchedulerEventEditorForm.cbAllDayEventPropertiesChange(
  Sender: TObject);
var
  ACanCorrect: Boolean;
begin
  ACanCorrect := icbShowTimeAs.ItemIndex = cxOriginalEventStates[Event.AllDayEvent];
  Event.AllDayEvent := cbAllDayEvent.Checked;
  if ACanCorrect then
  begin
    icbShowTimeAs.ItemIndex := cxOriginalEventStates[Event.AllDayEvent];
    icbShowTimeAs.EditModified := True;
  end;
  Modified := True;
  CheckVisible;
end;

procedure TcxSchedulerEventEditorForm.StartDateChanged(Sender: TObject);
begin
  OnChanged(nil);
  deEnd.Date := deStart.Date +
    dxDateOf(Helper.Duration) - Ord(cbAllDayEvent.Checked);
  if cbAllDayEvent.Checked and (Helper.Duration < 1) then
    deEnd.Date := deEnd.Date + 1;
  Helper.Duration := (dxDateOf(deEnd.Date) + dxTimeOf(teEnd.Time)) -
    (dxDateOf(deStart.Date) + dxTimeOf(teStart.Time)) +  Ord(cbAllDayEvent.Checked);
  Helper.IsIntersectionValid := False;
end;

procedure TcxSchedulerEventEditorForm.cbReminderClick(Sender: TObject);
begin
  Modified := True;
  cbReminderMinutesBeforeStart.Enabled := cbReminder.Checked;
end;

procedure TcxSchedulerEventEditorForm.cbReminderMinutesBeforeStartPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AMinutes: Integer;
begin
  Error := not cxTextToMinutesProc(VarToStr(DisplayValue), AMinutes);
  if not Error then
  begin
    DisplayValue := cxMinutesToTextProc(AMinutes);
    cxComboBoxCaretToEnd(TcxComboBox(Sender));
  end
  else
    ErrorText := cxGetResourceString(@scxInvalidNumber);
end;

procedure TcxSchedulerEventEditorForm.cbReminderMinutesBeforeStartPropertiesPopup(
  Sender: TObject);
var
  AMinutes: Integer;
  AText: string;
begin
  AText := cbReminderMinutesBeforeStart.Text;
  if cxTextToMinutesProc(AText, AMinutes) then
  begin
    cbReminderMinutesBeforeStart.Text := cxMinutesToTextProc(AMinutes);
    cbReminderMinutesBeforeStart.EditModified := not SameText(AText, cbReminderMinutesBeforeStart.Text);
    cxComboBoxCaretToEnd(cbReminderMinutesBeforeStart);
    cxComboBoxSetNearestTimeInterval(cbReminderMinutesBeforeStart, AMinutes);
  end;
end;

procedure TcxSchedulerEventEditorForm.OnEventTimeChanged(Sender: TObject);
begin
  OnChanged(Sender);
end;

procedure TcxSchedulerEventEditorForm.btnRecurrenceClick(Sender: TObject);
var
  AModified: Boolean;
  AStart, AFinish, ADateTime: TDateTime;
begin
  AModified := False;
  AStart := Event.Start;
  AFinish := Event.Finish;
  if (Event.EventType = etNone) then
  begin
    Event.AllDayEvent := cbAllDayEvent.Checked;
    Event.Start := deStart.Date + teStart.Time;
    Event.Finish := deEnd.Date + teEnd.Time + Ord(cbAllDayEvent.Checked);
  end
  else
  begin
    Event.Start := Helper.PatternStart;
    Event.Finish := Helper.PatternFinish;
  end;
  Event.RecurrenceInfo.Assign(Helper.RecurrenceInfo);
  if cxShowRecurrenceEditor(Event, Helper.LookAndFeel, AModified,
    ReadOnly, Helper.NeedCheckLossOfExceptions, True, Helper.Form) then
  begin
    if not ReadOnly then
    begin
      cbAllDayEvent.Checked := Event.AllDayEvent;
      Helper.PatternStart := Event.Start;
      Helper.PatternFinish := Event.Finish;
      Helper.RecurrenceInfo.Assign(Event.RecurrenceInfo);
      ADateTime := Event.Start;
      deStart.Date := dxDateOf(ADateTime);
      teStart.Time := dxTimeOf(ADateTime);
      ADateTime := Helper.PatternFinish;
      deEnd.Date := dxDateOf(ADateTime) - Ord(cbAllDayEvent.Checked);
      teEnd.Time := dxTimeOf(ADateTime);
      DeleteExceptions := True;
      Helper.NeedCheckLossOfExceptions := False;
      ForceSeries := True;
    end;
  end
  else
  begin
    Event.Start := AStart;
    Event.Finish := AFinish;
    DeleteExceptions := not ReadOnly and (DeleteExceptions or
      ((Event.Pattern <> nil) and (Event.EventType = etNone)));
  end;
  Modified := not ReadOnly and (Modified or AModified);
  CheckVisible;
end;

procedure TcxSchedulerEventEditorForm.OnResourceIDChanged(Sender: TObject);
begin
  Modified := True;
end;

procedure TcxSchedulerEventEditorForm.btnCancelClick(Sender: TObject);
begin
  Helper.ExecuteAction(mrCancel);
end;

procedure TcxSchedulerEventEditorForm.btnDeleteClick(Sender: TObject);
begin
  Helper.ExecuteAction(mrAbort);
  if not Helper.IsEditorModal and (ModalResult <> mrNone) then
    Close;
end;

procedure TcxSchedulerEventEditorForm.btnFindTimeClick(Sender: TObject);
begin
  if not FindAvailableTime then
     MessageBox(0, PChar(cxGetResourceString(@scxNoAvailableFreeTime)), nil, MB_ICONINFORMATION or MB_OK);
  CheckControlStates;
  if pnlRecurrenceInfo.Visible then
    RefreshRecurrenceInfo;
  teSubject.SetFocus;
end;

procedure TcxSchedulerEventEditorForm.btnOkClick(Sender: TObject);
begin
  Helper.ExecuteAction(mrOk);
  if not Helper.IsEditorModal and (ModalResult <> mrNone) then
    Close;
end;

procedure TcxSchedulerEventEditorForm.seTaskCompleteChange(
  Sender: TObject);
begin
  Modified := True;
  Event.TaskComplete := seTaskComplete.Value;
  Helper.BeginUpdate;
  try
    cbxTaskStatus.ItemIndex := Integer(Event.TaskStatus);
  finally
    Helper.EndUpdate;
  end;
end;

procedure TcxSchedulerEventEditorForm.cbxTaskStatusChange(
  Sender: TObject);
begin
  Modified := True;
  if Helper.IsUpdating then Exit;
  Helper.BeginUpdate;
  try
    Event.TaskStatus := TcxSchedulerEventTaskStatus(cbxTaskStatus.ItemIndex);
    seTaskComplete.Value := Event.TaskComplete;
  finally
    Helper.EndUpdate;
  end;
end;

type
  TcxCheckComboBoxAccess = class(TcxCheckComboBox);

procedure TcxSchedulerEventEditorForm.cbResourcesPropertiesClickCheck(
  Sender: TObject; ItemIndex: Integer; var AllowToggle: Boolean);
var
  I: Integer;
begin
  if not Helper.AllowShare then
  begin
    cbResources.Properties.OnClickCheck := nil;
    for I := 0 to cbResources.Properties.Items.Count - 1 do
      if I <> ItemIndex then
        cbResources.SetItemState(I, cbsUnchecked);
    TcxCheckComboBoxAccess(cbResources).LookupData.ActiveControl.Refresh;
    cbResources.Properties.OnClickCheck := cbResourcesPropertiesClickCheck;
  end;
end;

procedure Initialize;
begin
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerStandardEventEditorFormStyleInfo);
end;

procedure Finalize;
begin
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerStandardEventEditorFormStyleInfo);
end;

initialization
  dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.


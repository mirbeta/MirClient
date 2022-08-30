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

unit cxSchedulerRibbonStyleEventEditor;

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
  dxCore, cxDateUtils, dxRibbonForm,
  cxClasses, dxRibbon, dxRibbonBackstageView, ActnList, dxBar,
  ImgList, cxSchedulerEventEditor, dxBarApplicationMenu, cxBarEditItem,
  dxRibbonSkins, cxSchedulerEditorFormManager, dxLayoutContainer, dxLayoutControl,
  dxLayoutLookAndFeels, dxLayoutControlAdapters, dxLayoutcxEditAdapters;

type

  { TcxShedulerRibbonStyleEventEditorFormStyleInfo }

  TcxShedulerRibbonStyleEventEditorFormStyleInfo = class(TcxShedulerEventEditorFormStyleInfo)
  public
    class function CreateEditor(AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm; override;
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); virtual;
  end;

  { TcxShedulerRibbon2010StyleEventEditorFormStyleInfo }

  TcxShedulerRibbon2010StyleEventEditorFormStyleInfo = class(TcxShedulerRibbonStyleEventEditorFormStyleInfo)
  public
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); override;
  end;

  { TcxShedulerRibbon2013StyleEventEditorFormStyleInfo }

  TcxShedulerRibbon2013StyleEventEditorFormStyleInfo = class(TcxShedulerRibbonStyleEventEditorFormStyleInfo)
  public
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); override;
  end;

  { TcxShedulerRibbon2016StyleEventEditorFormStyleInfo }

  TcxShedulerRibbon2016StyleEventEditorFormStyleInfo = class(TcxShedulerRibbonStyleEventEditorFormStyleInfo)
  public
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); override;
  end;

  { TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo }

  TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo = class(TcxShedulerRibbonStyleEventEditorFormStyleInfo)
  public
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); override;
  end;

  { TcxShedulerRibbon2019StyleEventEditorFormStyleInfo }

  TcxShedulerRibbon2019StyleEventEditorFormStyleInfo = class(TcxShedulerRibbonStyleEventEditorFormStyleInfo)
  public
    class function GetName: string; override;
    class procedure InitialRibbonStyle(AEditor: IcxSchedulerEventEditorForm); override;
  end;

  { TcxSchedulerEventRibbonStyleEditorFormHelper }

  TcxSchedulerEventRibbonStyleEditorFormHelper = class(TcxSchedulerEventEditorFormHelper);
  TcxSchedulerEventRibbonStyleEditorFormHelperClass = class of TcxSchedulerEventRibbonStyleEditorFormHelper;

  { TcxSchedulerEventRibbonEditorForm }

  TcxSchedulerEventRibbonStyleEditorForm = class(TdxRibbonForm, IcxSchedulerEventEditorForm)
    abbbSave: TdxBarLargeButton;
    acClose: TAction;
    acDelete: TAction;
    acRecurrency: TAction;
    acSave: TAction;
    acSaveAndClose: TAction;
    alActions: TActionList;
    bbClose: TdxBarLargeButton;
    bbDelete: TdxBarLargeButton;
    bbRecurrency: TdxBarLargeButton;
    bbSave: TdxBarLargeButton;
    bbSaveAndClose: TdxBarLargeButton;
    beiLabel: TcxBarEditItem;
    beiShowTimeAs: TcxBarEditItem;
    btnFindTime: TcxButton;
    cbAllDayEvent: TcxCheckBox;
    cbReminder: TcxCheckBox;
    cbReminderMinutesBeforeStart: TcxComboBox;
    cbResources: TcxCheckComboBox;
    cbxTaskStatus: TcxComboBox;
    cxGroupBox1: TcxGroupBox;
    deEnd: TcxDateEdit;
    deStart: TcxDateEdit;
    dxbActions: TdxBar;
    dxbApplicationMenu: TdxBarApplicationMenu;
    dxBarManager: TdxBarManager;
    dxbOptions: TdxBar;
    dxbQuickAccessToolbar: TdxBar;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    liStart: TdxLayoutItem;
    liFinish: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem3: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem4: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem5: TdxLayoutSeparatorItem;
    ilLargeImages: TcxImageList;
    ilSmallImages: TcxImageList;
    lbEndTime: TdxLayoutItem;
    lbInformation: TcxLabel;
    lbLocation: TdxLayoutItem;
    lbRecurrence: TdxLayoutItem;
    lbRecurrencePattern: TcxLabel;
    lbResource: TdxLayoutItem;
    lbStartTime: TdxLayoutItem;
    lbSubject: TdxLayoutItem;
    lbTaskComplete: TdxLayoutItem;
    lbTaskStatus: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    meMessage: TcxMemo;
    pnlCaption: TdxLayoutGroup;
    pnlInformation: TdxLayoutGroup;
    pnlMessage: TdxLayoutItem;
    pnlRecurrenceInfo: TdxLayoutGroup;
    pnlReminder: TdxLayoutGroup;
    pnlResource: TdxLayoutGroup;
    pnlTaskComplete: TdxLayoutGroup;
    pnlTime: TdxLayoutGroup;
    Ribbon: TdxRibbon;
    seTaskComplete: TcxSpinEdit;
    tabAppointment: TdxRibbonTab;
    teEnd: TcxTimeEdit;
    teLocation: TcxTextEdit;
    teStart: TcxTimeEdit;
    teSubject: TcxTextEdit;
    dxLayoutSeparatorItem6: TdxLayoutSeparatorItem;

    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acRecurrencyExecute(Sender: TObject);
    procedure acSaveAndCloseExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure btnFindTimeClick(Sender: TObject);
    procedure cbAllDayEventPropertiesChange(Sender: TObject);
    procedure cbReminderClick(Sender: TObject);
    procedure cbReminderMinutesBeforeStartPropertiesPopup(Sender: TObject);
    procedure cbReminderMinutesBeforeStartPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure cbResourcesPropertiesClickCheck(Sender: TObject; ItemIndex: Integer; var AllowToggle: Boolean);
    procedure cbxTaskStatusChange(Sender: TObject);
    procedure EditorsChanged(Sender: TObject);
    procedure EventTimeChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OnResourceIDChanged(Sender: TObject);
    procedure seTaskCompleteChange(Sender: TObject);
    procedure StartDateChanged(Sender: TObject);
  private
    FHelper: TcxSchedulerEventRibbonStyleEditorFormHelper;
    FRecurrenceInfoHeight: Integer;

    procedure DoApplyEventTime(Sender: TObject);
    function GetAllowHiddenEvents: Boolean;
    function GetDeleteExceptions: Boolean;
    function GetEventName: string;
    function GetEvent: TcxSchedulerControlEvent;
    function GetForceSeries: Boolean;
    function GetReadOnly: Boolean;
    procedure HelperChangedHandler(Sender: TObject);
    function HelperIsValidHandler: Boolean;
    procedure HelperSaveChangesHandler(Sender: TObject);
    procedure SetAllowHiddenEvents(AValue: Boolean);
    procedure SetDeleteExceptions(AValue: Boolean);
    procedure SetForceSeries(AValue: Boolean);
    procedure SetReadOnly(AValue: Boolean);

    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    // IcxShedulerCustomEventEditorForm
    function GetForm: TForm;
    function GetModified: Boolean;
    procedure Initialize(AEventInfo: TcxSchedulerEventEditInfo;
      AEditingInfo: TcxSchedulerEditingEventInfo); virtual;
    procedure SetModified(AValue: Boolean);

    procedure ApplyEventTime;
    procedure CheckControlStates; virtual;
    function CheckTimeRange(var AStart, AFinish: TDateTime): Boolean; virtual;
    procedure CheckVisible; virtual;
    procedure ClearModifiedFlag; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHelperChanged; virtual;
    function DoHelperIsValid: Boolean; virtual;
    procedure DoHelperSaveChanges; virtual;
    function FindAvailableTime: Boolean; virtual;
    procedure GetEditingEventTime(var AStart, AFinish: TDateTime; var AState: Integer); virtual;
    function GetFormColor: TColor; virtual;
    function GetHelperClass: TcxSchedulerEventRibbonStyleEditorFormHelperClass; virtual;
    function GetRecurrenceDescription: string; virtual;
    function GetResourcesPanelVisible: Boolean; virtual;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure InitializeControls; virtual;
    procedure InitializeHelper(AEvent: TcxSchedulerControlEvent); virtual;
    procedure InitializeTimeControls(const AStart, AFinish: TDateTime; AllDayEvent: Boolean); virtual;
    procedure InitializationRibbonColorScheme;
    procedure InitLabelColor;
    procedure InitReminderPanel;
    procedure InitResources;
    procedure InitShowTimeAsPanel;
    procedure InitTaskCompletePanel;
    procedure LoadEventValuesIntoControls; virtual;
    function IsValid: Boolean; virtual;
    procedure RefreshRecurrenceInfo; virtual;
    procedure SaveResourceID; virtual;
    procedure SetActiveControl; virtual;
    procedure SetCaptions; virtual;
    procedure SetReminderMinutesBeforeStart; virtual;
    procedure UpdateEventValuesFromControls; virtual;

    property DeleteExceptions: Boolean read GetDeleteExceptions write SetDeleteExceptions;
    property Helper: TcxSchedulerEventRibbonStyleEditorFormHelper read FHelper;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor CreateEx(AEvent: TcxSchedulerControlEvent); virtual;
    destructor Destroy; override;

    property AllowHiddenEvents: Boolean read GetAllowHiddenEvents write SetAllowHiddenEvents;
    property EventName: string read GetEventName;
    property Event: TcxSchedulerControlEvent read GetEvent;
    property ForceSeries: Boolean read GetForceSeries write SetForceSeries;
    property Modified: Boolean read GetModified write SetModified;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  TcxSchedulerEventRibbonStyleEditor = TcxSchedulerEventRibbonStyleEditorForm; //workaround for Delphi 2005

  TcxSchedulerEventRibbonStyleEditorClass = class of TcxSchedulerEventRibbonStyleEditorForm;

implementation

uses
  Variants, DateUtils, Math, cxSchedulerStrs, cxSchedulerUtils,
  cxSchedulerDialogs, cxVariants, cxSchedulerRecurrenceSelectionDialog;

{$R *.dfm}

type
  TdxTouchScrollUIModeManagerAccess = class(TdxTouchScrollUIModeManager);
  TdxHybridScrollbarManagersAccess = class(TdxHybridScrollbarManagers);

{ TcxShedulerRibbonStyleEventEditorFormStyleInfo }

class function TcxShedulerRibbonStyleEventEditorFormStyleInfo.CreateEditor(
  AEvent: TcxSchedulerControlEvent): IcxSchedulerEventEditorForm;
begin
  Result := TcxSchedulerEventRibbonStyleEditorForm.CreateEx(AEvent);
  InitialRibbonStyle(Result);
end;

class function TcxShedulerRibbonStyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon';
end;

class procedure TcxShedulerRibbonStyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
end;

{ TcxShedulerRibbon2010StyleEventEditorFormStyleInfo }

class function TcxShedulerRibbon2010StyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon2010';
end;

class procedure TcxShedulerRibbon2010StyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).Ribbon.Style := rs2010;
end;

{ TcxShedulerRibbon2013StyleEventEditorFormStyleInfo }

class function TcxShedulerRibbon2013StyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon2013';
end;

class procedure TcxShedulerRibbon2013StyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).DisableAero := True;
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).Ribbon.Style := rs2013;
end;

{ TcxShedulerRibbon2016StyleEventEditorFormStyleInfo }

class function TcxShedulerRibbon2016StyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon2016';
end;

class procedure TcxShedulerRibbon2016StyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).DisableAero := True;
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).Ribbon.Style := rs2016;
end;

{ TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo }

class function TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon2016Tablet';
end;

class procedure TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).DisableAero := True;
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).Ribbon.Style := rs2016Tablet;
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).beiLabel.Links[0].BeginGroup := True;
end;

{ TcxShedulerRibbon2019StyleEventEditorFormStyleInfo }

class function TcxShedulerRibbon2019StyleEventEditorFormStyleInfo.GetName: string;
begin
  Result := 'Ribbon2019';
end;

class procedure TcxShedulerRibbon2019StyleEventEditorFormStyleInfo.InitialRibbonStyle(
  AEditor: IcxSchedulerEventEditorForm);
begin
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).DisableAero := True;
  TcxSchedulerEventRibbonStyleEditorForm(AEditor.Form).Ribbon.Style := rs2019;
end;

{ TcxSchedulerEventRibbonStyleEditor }

constructor TcxSchedulerEventRibbonStyleEditorForm.CreateEx(AEvent: TcxSchedulerControlEvent);
begin
  inherited Create(nil);
  FHelper := GetHelperClass.Create(Self, AEvent);
end;

destructor TcxSchedulerEventRibbonStyleEditorForm.Destroy;
begin
  cxDialogsMetricsStore.StoreMetrics(Self);
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SaveResourceID;
var
  I: Integer;
begin
  Event.ResourceID := Null;
  for I := 0 to cbResources.Properties.Items.Count - 1 do
    if cbResources.States[I] = cbsChecked then
      Event.ShareWithResource(Storage.ResourceIDs[cbResources.Properties.Items[I].Tag]);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetActiveControl;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.acCloseExecute(Sender: TObject);
begin
  Helper.ExecuteAction(mrCancel);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.acDeleteExecute(Sender: TObject);
begin
  Helper.ExecuteAction(mrAbort);
  if not Helper.IsEditorModal and (ModalResult <> mrNone) then
    Close;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.acRecurrencyExecute(
  Sender: TObject);
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

procedure TcxSchedulerEventRibbonStyleEditorForm.acSaveAndCloseExecute(
  Sender: TObject);
begin
  Helper.ExecuteAction(mrOk);
  if ModalResult = mrOk then
    Close;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.acSaveExecute(Sender: TObject);
begin
  Helper.ExecuteAction(mrOk);
  if ModalResult = mrOk then
  begin
    ModalResult := mrNone;
    Helper.DoRefresh;
  end;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.ApplyEventTime;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.CheckControlStates;
begin
  if Helper.IsUpdating then
    Exit;
  Helper.CalculateIntersection;
  pnlInformation.Visible := not Helper.IsIntersectionValid;
  pnlCaption.Enabled := not ReadOnly;
  pnlReminder.Enabled := not ReadOnly;
  pnlTime.Enabled := not ReadOnly;
  pnlResource.Enabled := not ReadOnly;
  pnlTaskComplete.Enabled := not ReadOnly;
  meMessage.Properties.ReadOnly := ReadOnly;
  beiShowTimeAs.Enabled := not ReadOnly;
  beiLabel.Enabled := not ReadOnly;
  acSave.Enabled := not ReadOnly and (Modified or Helper.IsNewEvent) and
    Helper.IsIntersectionValid;
  acSaveAndClose.Enabled := not ReadOnly and (Modified or Helper.IsNewEvent) and
    Helper.IsIntersectionValid;
  acDelete.Enabled := not (ReadOnly or Helper.IsNewEvent) and Helper.AllowDelete;
end;

function TcxSchedulerEventRibbonStyleEditorForm.CheckTimeRange(var AStart, AFinish: TDateTime): Boolean;
var
  AEventStart, AEventFinish: TDateTime;
begin
  Event.GetValidTaskTimeRange(AStart, AFinish);
  AEventStart := deStart.Date + teStart.Time;
  AEventFinish := deEnd.Date + teEnd.Time;
  Result := (AStart <= AEventStart) and (AFinish >= AEventFinish);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.CheckVisible;
var
  ATimeVisible: Boolean;
begin
  if Helper.IsUpdating then
    Exit;
  pnlTaskComplete.Visible := Helper.ShowTaskComplete;
  pnlResource.Visible := GetResourcesPanelVisible;
  ATimeVisible := (Event.EventType = etNone) or (Event.EventType in [etOccurrence, etCustom]) and not ForceSeries;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.ClearModifiedFlag;

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
  Modified := False;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.DoHelperChanged;
begin
  CheckControlStates;
end;

function TcxSchedulerEventRibbonStyleEditorForm.DoHelperIsValid: Boolean;
begin
  Result := IsValid;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.DoHelperSaveChanges;
var
  AStart, AFinish: TDateTime;
begin
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
  if beiShowTimeAs.EditValue <> -1 then
    Event.State := beiShowTimeAs.EditValue;
  if beiLabel.EditValue <> -1 then
    Event.LabelColor := beiLabel.EditValue;
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

function TcxSchedulerEventRibbonStyleEditorForm.FindAvailableTime: Boolean;
var
  AStart, AFinish: TDateTime;
begin
  Result := Storage.FindAvailableTime(Event, AllowHiddenEvents, AStart, AFinish);
  if Result then
    InitializeTimeControls(AStart, AFinish, Event.AllDayEvent);
  if Result and (Event.EventType = etPattern) then
    ApplyEventTime;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.GetEditingEventTime(
  var AStart, AFinish: TDateTime; var AState: Integer);
begin
  AStart := deStart.Date + teStart.Time;
  AFinish := deEnd.Date + teEnd.Time + Ord(cbAllDayEvent.Checked);
  AState := beiShowTimeAs.EditValue;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetFormColor: TColor;
begin
  Result := Helper.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetHelperClass: TcxSchedulerEventRibbonStyleEditorFormHelperClass;
begin
  Result := TcxSchedulerEventRibbonStyleEditorFormHelper;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetRecurrenceDescription: string;
begin
  Result := Helper.GetRecurrenceDescription;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetResourcesPanelVisible: Boolean;
begin
  Result := Helper.ShowResources;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Helper.Storage;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.Initialize(AEventInfo: TcxSchedulerEventEditInfo;
  AEditingInfo: TcxSchedulerEditingEventInfo);
begin
  Helper.Initialize(AEventInfo, AEditingInfo);
  InitializeHelper(Event);
  Helper.BeginUpdate;
  try
    InitializeControls;
    LoadEventValuesIntoControls;
    ClearModifiedFlag;
  finally
    Helper.EndUpdate;
  end;
  SetControlLookAndFeel(Self, Helper.LookAndFeel);
  dxBarManager.LookAndFeel.MasterLookAndFeel := Helper.LookAndFeel;
  InitializationRibbonColorScheme;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitializeControls;
begin
  InitReminderPanel;
  InitShowTimeAsPanel;
  InitLabelColor;
  InitTaskCompletePanel;
  pnlInformation.Visible := False;
  SetCaptions;
  teEnd.ActiveProperties.Use24HourFormat := Is24HourTimeFormat;
  teStart.ActiveProperties.Use24HourFormat := Is24HourTimeFormat;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitializeHelper(
  AEvent: TcxSchedulerControlEvent);
begin
  Helper.OnChanged := HelperChangedHandler;
  Helper.OnSaveChanges := HelperSaveChangesHandler;
  Helper.OnIsValid := HelperIsValidHandler;
  Helper.OnApplyEventTime := DoApplyEventTime;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitializeTimeControls(
  const AStart, AFinish: TDateTime; AllDayEvent: Boolean);
begin
  deStart.Date := dxDateOf(AStart);
  teStart.Time := dxTimeOf(AStart);
  deEnd.Date := dxDateOf(AFinish) - Ord(AllDayEvent);
  teEnd.Time := dxTimeOf(AFinish);
  cbAllDayEvent.Checked := AllDayEvent;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitializationRibbonColorScheme;
begin
  Ribbon.ColorSchemeName := Helper.LookAndFeel.SkinName
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitReminderPanel;
begin
  pnlReminder.Visible := Helper.IsRemindersActive;
  if pnlReminder.Visible then
    cxComboBoxPopulateWithTimeIntervals(cbReminderMinutesBeforeStart);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitResources;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.InitShowTimeAsPanel;
var
  AProperties: TcxImageComboBoxProperties;
begin
  AProperties := TcxImageComboBoxProperties(beiShowTimeAs.Properties);
  Helper.PopulateShowTimeAsPanel(AProperties);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.InitTaskCompletePanel;
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

function TcxSchedulerEventRibbonStyleEditorForm.IsValid: Boolean;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.RefreshRecurrenceInfo;
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

procedure TcxSchedulerEventRibbonStyleEditorForm.LoadEventValuesIntoControls;
var
  AAllowTimeChange: Boolean;
begin
  teSubject.Text := Event.Caption;
  teSubject.Enabled := Storage.IsCaptionAvailable;

  teLocation.Text := Event.Location;
  teLocation.Enabled := Storage.IsLocationAvailable;

  beiLabel.Enabled := Storage.IsLabelColorAvailable and (EventLabels.Count > 0);
  beiLabel.EditValue := Event.LabelColor;

  meMessage.Text := Event.Message;
  meMessage.Enabled := Storage.IsMessageAvailable;

  InitializeTimeControls(Event.Start, Event.Finish, Event.AllDayEvent);

  beiShowTimeAs.EditValue := Integer(Event.State);
  beiShowTimeAs.Enabled := Storage.IsStateAvailable;

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

procedure TcxSchedulerEventRibbonStyleEditorForm.InitLabelColor;
var
  AProperties: TcxImageComboBoxProperties;
begin
  beiLabel.Enabled := Storage.IsLabelColorAvailable and (EventLabels.Count > 0);
  AProperties := TcxImageComboBoxProperties(beiLabel.Properties);
  Helper.PopulateLabelColor(AProperties);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetCaptions;
begin
  Caption := cxGetResourceString(@scxEvent) + ' - ' + EventName;
  // events
  lbInformation.Caption := cxGetResourceString(@scxEventsConflict);
  lbResource.Caption := cxGetResourceString(@scxResource);
  lbSubject.Caption := cxGetResourceString(@scxSubject);
  lbLocation.Caption := cxGetResourceString(@scxLocation);
  beiLabel.Caption := cxGetResourceString(@scxLabelAs);
  beiShowTimeAs.Caption := cxGetResourceString(@scxShowAs);
  lbStartTime.Caption := cxGetResourceString(@scxStartTime);
  lbEndTime.Caption := cxGetResourceString(@scxEndTime);
  cbAllDayEvent.Caption := cxGetResourceString(@scxAllDayEvent);
  cbReminder.Caption := cxGetResourceString(@scxReminder);
  lbRecurrence.Caption := cxGetResourceString(@scxRecurrenceLabel);
  lbTaskComplete.Caption := cxGetResourceString(@scxTaskComplete);
  lbTaskStatus.Caption := cxGetResourceString(@scxTaskStatus);
  // buttons
  tabAppointment.Caption := cxGetResourceString(@scxAppointment);
  dxbActions.Caption := cxGetResourceString(@scxActions);
  dxbOptions.Caption := cxGetResourceString(@scxOptions);
  btnFindTime.Caption := cxGetResourceString(@scxFindAvailableTime);
  acSaveAndClose.Caption := cxGetResourceString(@scxSaveAndClose);
  acSaveAndClose.Hint := cxGetResourceString(@scxSaveAndCloseHint);
  acSave.Caption := cxGetResourceString(@scxSave);
  acSave.Hint := cxGetResourceString(@scxSave);
  acClose.Caption := cxGetResourceString(@scxActionClose);
  acClose.Hint := cxGetResourceString(@scxActionClose);
  acDelete.Caption := cxGetResourceString(@scxDelete1);
  acDelete.Hint := cxGetResourceString(@scxDelete1);
  acRecurrency.Caption := cxGetResourceString(@scxActionRecurrence);
  acRecurrency.Hint := cxGetResourceString(@scxActionRecurrence);
  acRecurrency.Enabled := Storage.IsRecurrenceAvailable;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.DoApplyEventTime(Sender: TObject);
begin
  ApplyEventTime;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetAllowHiddenEvents: Boolean;
begin
  Result := Helper.AllowHiddenEvents;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetDeleteExceptions: Boolean;
begin
  Result := Helper.DeleteExceptions;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetEventName: string;
begin
  Result := cxGetResourceString(@scxUntitled);
  if Event.Caption <> '' then
    Result := Event.Caption;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetEvent: TcxSchedulerControlEvent;
begin
  Result := Helper.Event;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetForceSeries: Boolean;
begin
  Result := Helper.ForceSeries;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetForm: TForm;
begin
  Result := Self;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetModified: Boolean;
begin
  Result := Helper.Modified;
end;

function TcxSchedulerEventRibbonStyleEditorForm.GetReadOnly: Boolean;
begin
  Result := Helper.ReadOnly;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.HelperChangedHandler(Sender: TObject);
begin
  DoHelperChanged;
end;

function TcxSchedulerEventRibbonStyleEditorForm.HelperIsValidHandler: Boolean;
begin
  Result := DoHelperIsValid;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.HelperSaveChangesHandler(Sender: TObject);
begin
  UpdateEventValuesFromControls;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetAllowHiddenEvents(
  AValue: Boolean);
begin
  Helper.AllowHiddenEvents := AValue;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetDeleteExceptions(
  AValue: Boolean);
begin
  Helper.DeleteExceptions := AValue;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetForceSeries(AValue: Boolean);
begin
  Helper.ForceSeries := AValue;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetModified(AValue: Boolean);
begin
  Helper.Modified := AValue;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetReadOnly(AValue: Boolean);
begin
  Helper.ReadOnly := AValue;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_ACTIVE then
    Helper.NormalizeTopMostWindows;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.EditorsChanged(Sender: TObject);
begin
  Helper.IsIntersectionValid := False;
  Modified := True;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TdxTouchScrollUIModeManagerAccess.CheckUIVisibility(Handle);
  TdxHybridScrollbarManagersAccess.CheckScrollbarsVisibility(Handle);
  dxApplicationActivateWindowHelper.RemoveWindow(Self);
  Action := caFree;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Helper.CanClose;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and not HasOpenedPopupWindow(ActiveControl) then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.FormShow(Sender: TObject);
begin
  FRecurrenceInfoHeight := pnlRecurrenceInfo.Height;
  CheckVisible;
  SetActiveControl;
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
  dxApplicationActivateWindowHelper.AddWindow(Self);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if Helper.RecurrenceActivate then
    acRecurrencyExecute(nil);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.cbAllDayEventPropertiesChange(Sender: TObject);
var
  ACanCorrect: Boolean;
begin
  ACanCorrect := beiShowTimeAs.EditValue = cxOriginalEventStates[Event.AllDayEvent];
  Event.AllDayEvent := cbAllDayEvent.Checked;
  if ACanCorrect then
    beiShowTimeAs.EditValue := cxOriginalEventStates[Event.AllDayEvent];
  Modified := True;
  CheckVisible;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.StartDateChanged(Sender: TObject);
begin
  EditorsChanged(nil);
  deEnd.Date := deStart.Date +
    dxDateOf(Helper.Duration) - Ord(cbAllDayEvent.Checked);
  if cbAllDayEvent.Checked and (Helper.Duration < 1) then
    deEnd.Date := deEnd.Date + 1;
  Helper.Duration := (dxDateOf(deEnd.Date) + dxTimeOf(teEnd.Time)) -
    (dxDateOf(deStart.Date) + dxTimeOf(teStart.Time)) +  Ord(cbAllDayEvent.Checked);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.SetReminderMinutesBeforeStart;
var
  AMinutes: Integer;
begin
  if cxTextToMinutesProc(cbReminderMinutesBeforeStart.Text, AMinutes) then
    Event.ReminderMinutesBeforeStart := AMinutes;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.UpdateEventValuesFromControls;
begin
  DoHelperSaveChanges;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.cbReminderClick(Sender: TObject);
begin
  Modified := True;
  cbReminderMinutesBeforeStart.Enabled := cbReminder.Checked;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.cbReminderMinutesBeforeStartPropertiesValidate(
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

procedure TcxSchedulerEventRibbonStyleEditorForm.cbReminderMinutesBeforeStartPropertiesPopup(
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

procedure TcxSchedulerEventRibbonStyleEditorForm.EventTimeChanged(Sender: TObject);
begin
  EditorsChanged(Sender);
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.OnResourceIDChanged(Sender: TObject);
begin
  Modified := True;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.btnFindTimeClick(Sender: TObject);
begin
  if not FindAvailableTime then
     MessageBox(0, PChar(cxGetResourceString(@scxNoAvailableFreeTime)), nil, MB_ICONINFORMATION or MB_OK);
  CheckControlStates;
  if pnlRecurrenceInfo.Visible then
    RefreshRecurrenceInfo;
  teSubject.SetFocus;
end;

procedure TcxSchedulerEventRibbonStyleEditorForm.seTaskCompleteChange(
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

procedure TcxSchedulerEventRibbonStyleEditorForm.cbxTaskStatusChange(
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

procedure TcxSchedulerEventRibbonStyleEditorForm.cbResourcesPropertiesClickCheck(
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
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbonStyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbon2010StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbon2013StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbon2016StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.RegisterShedulerEditorForm(TcxShedulerRibbon2019StyleEventEditorFormStyleInfo);
end;

procedure Finalize;
begin
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbonStyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbon2010StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbon2013StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbon2016StyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbon2016TabletStyleEventEditorFormStyleInfo);
  cxSchedulerEditorManager.UnregisterShedulerEditorForm(TcxShedulerRibbon2019StyleEventEditorFormStyleInfo);
end;

initialization
  dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.



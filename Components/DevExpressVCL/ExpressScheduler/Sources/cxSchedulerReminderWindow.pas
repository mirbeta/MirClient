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

unit cxSchedulerReminderWindow;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Controls, Graphics, ExtCtrls, Forms,
  StdCtrls, cxLookAndFeels, cxSchedulerStorage, cxListView, cxButtons, ComCtrls, Menus,
  dxCore, dxMessages, cxControls, cxContainer, cxClasses, cxGraphics,
  cxLookAndFeelPainters, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxLabel, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxLayoutLookAndFeels;

const
  cxDefaultResourceNameColumnWidth: Word = 120;

type
  { TcxSchedulerReminderFormItem }

  TcxSchedulerReminderFormItem = class(TcxSchedulerReminderEventID)
  protected
    ResourceID: Variant;
    ReminderDate: TDateTime;
  public
    constructor Create(AReminder: TcxSchedulerReminder);
    function Equals(AReminder: TcxSchedulerReminder): Boolean;
  end;

  { TcxSchedulerReminderForm }

  TcxReminderFormLayoutData = array[0..2] of Integer;

  TcxSchedulerReminderForm = class(TcxSchedulerCustomReminderForm,
    IUnknown,
    IcxDialogMetricsInfoData
  )
    btnDismiss: TcxButton;
    btnDismissAll: TcxButton;
    btnOpenItem: TcxButton;
    btnSnooze: TcxButton;
    cbSnoozeTime: TcxComboBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutLabeledItem2: TdxLayoutLabeledItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lbEventCaption: TcxLabel;
    lbEventStartTime: TdxLayoutLabeledItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liImage: TdxLayoutItem;
    liOpenItem: TdxLayoutItem;
    lvItems: TcxListView;
    pbImage: TPaintBox;
    tmUpdate: TTimer;

    procedure ButtonClick(Sender: TObject);
    procedure DrawIcon(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvItemsDblClick(Sender: TObject);
    procedure tmUpdateTimer(Sender: TObject);
    procedure lvItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FLayoutData: TcxReminderFormLayoutData;
    FAdvanceCount: Integer;
    FSelections: TcxObjectList;
    function GetFocusedReminder: TcxSchedulerReminder;
    function GetVisibleReminder(Index: Integer): TcxSchedulerReminder;
    function GetVisibleReminderCount: Integer;
    procedure SynchronizeSelection;
    procedure WMRedrawHeader(var Msg: TMsg); message DXM_REFRESHCUSTOMIZATION;
  protected
    //IcxDialogMetricsInfoData
    function GetInfoData: Pointer; virtual;
    function GetInfoDataSize: Integer; virtual;
    procedure SetInfoData(AData: Pointer); virtual;

    function AddReminder(AReminder: TcxSchedulerReminder): TListItem;
    procedure AddReminderInfo(AReminder: TcxSchedulerReminder; AItem: TListItem; const ADueInText: string);
    function AddSnoozeAdvanceTime(const AText: string; AMinutes: Integer): Integer;
    function AddSnoozeTime(const AText: string; AMinutes: Integer): Integer;
    procedure CheckColumnCaptions;
    procedure CheckColumnCount; virtual;
    procedure CheckFormPosition; override;
    procedure CreateSnoozeAdvanceTimeItems; virtual;
    procedure DismissEvents(All: Boolean);
    procedure DoShow; override;
    function GetDueInColumnIndex: Integer;
    function GetImageIndexForReminder(AReminder: TcxSchedulerReminder): Integer; virtual;
    function GetMaxAdvanceMinutes: Integer;
    function GetResourceNameForReminder(AReminder: TcxSchedulerReminder): string; virtual;
    function GetShowResourceColumn: Boolean; virtual;
    function GetSnoozeTime: TDateTime; virtual;
    procedure LayoutChanged; override;
    procedure SnoozeEvents;
    procedure OpenFocusedEvent;
    procedure RefreshSelectedInfo; virtual;
    procedure ResetAdvanceCount;
    procedure RestoreSelection; override;
    procedure SaveSelection; override;
    procedure SetupCaptions; virtual;
    procedure UpdateDueTimeTexts;
    procedure UpdateReminderList; override;
    procedure UpdateSelectionCaptions; override;
    procedure UpdateSnoozeComboBox; virtual;

    property AdvanceCount: Integer read FAdvanceCount;
    property VisibleReminderCount: Integer read GetVisibleReminderCount;
    property VisibleReminders[Index: Integer]: TcxSchedulerReminder read GetVisibleReminder;
  public
    constructor Create(AReminders: TcxSchedulerReminders); override;
    destructor Destroy; override;
  end;

implementation

{$R *.DFM}

uses
  cxSchedulerStrs, cxSchedulerDialogs, cxSchedulerUtils,
  CommCtrl, Math, cxDateUtils, cxVariants;

{ TcxSchedulerReminderFormItem }

constructor TcxSchedulerReminderFormItem.Create(AReminder: TcxSchedulerReminder);
begin
  inherited Create(AReminder.Event);
  ResourceID := AReminder.ResourceID;
  ReminderDate := AReminder.ReminderDate;
end;

function TcxSchedulerReminderFormItem.Equals(AReminder: TcxSchedulerReminder): Boolean;
begin
  Result := SameEvent(AReminder.Event) and (ReminderDate = AReminder.ReminderDate) and
    VarEquals(ResourceID, AReminder.ResourceID);
end;

{ TcxSchedulerReminderForm }

constructor TcxSchedulerReminderForm.Create(
  AReminders: TcxSchedulerReminders);
var
  AStream: TStream;
begin
  inherited Create(AReminders);
  lvItems.DoubleBuffered := True;
  AStream := TResourceStream.Create(HInstance, 'REMINDERBELL', RT_RCDATA);
  try
    Icon.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
  lvItems.SmallImages := MenuImages;
end;

destructor TcxSchedulerReminderForm.Destroy;
begin
  FSelections.Free;
  inherited Destroy;
end;

//IcxDialogMetricsInfoData
function TcxSchedulerReminderForm.GetInfoData: Pointer;
var
  I: Integer;
begin
  FillChar(FLayoutData, SizeOf(FLayoutData), 0);
  for I := 0 to lvItems.Columns.Count - 1 do
    FLayoutData[I] := lvItems.Columns[I].Width;
  Result := @FLayoutData;
end;

function TcxSchedulerReminderForm.GetInfoDataSize: Integer;
begin
  Result := SizeOf(FLayoutData);
end;

procedure TcxSchedulerReminderForm.SetInfoData(AData: Pointer);
var
  I: Integer;
begin
  lvItems.Items.BeginUpdate;
  try
    CheckColumnCount;
    for I := 0 to lvItems.Columns.Count - 1 do
      if TcxReminderFormLayoutData(AData^)[I] <> 0 then
        lvItems.Columns[I].Width := TcxReminderFormLayoutData(AData^)[I];
  finally
    lvItems.Items.EndUpdate;
  end;
end;

function TcxSchedulerReminderForm.AddReminder(AReminder: TcxSchedulerReminder): TListItem;
begin
  Result := lvItems.Items.Add;
  with Result do
  begin
    Data := AReminder;
    Caption := AReminder.Event.Caption;
    ImageIndex := GetImageIndexForReminder(AReminder);
  end;
end;


procedure TcxSchedulerReminderForm.AddReminderInfo(
  AReminder: TcxSchedulerReminder; AItem: TListItem; const ADueInText: string);
begin
  with AItem do
  begin
    if GetShowResourceColumn then
      SubItems.Add(GetResourceNameForReminder(AReminder));
    SubItems.Add(ADueInText);
  end;
end;

function TcxSchedulerReminderForm.AddSnoozeAdvanceTime(const AText: string;
  AMinutes: Integer): Integer;
begin
  Result := AddSnoozeTime(AText, AMinutes);
  Inc(FAdvanceCount);
end;

function TcxSchedulerReminderForm.AddSnoozeTime(const AText: string;
  AMinutes: Integer): Integer;
begin
  Result := cbSnoozeTime.Properties.Items.AddObject(AText, TObject(AMinutes));
end;

procedure TcxSchedulerReminderForm.CheckColumnCaptions;
begin
  lvItems.Columns[0].Caption := cxGetResourceString(@scxrSubject);
  if GetShowResourceColumn then
    lvItems.Columns[1].Caption := cxGetResourceString(@scxResource);
  lvItems.Columns[1 + GetDueInColumnIndex].Caption := cxGetResourceString(@scxrDueIn);
end;

procedure TcxSchedulerReminderForm.CheckColumnCount;
begin
  if (lvItems.Columns.Count = 2) and GetShowResourceColumn then
  begin
    lvItems.Columns.Add.Width := lvItems.Column[1].Width;
    lvItems.Column[1].Width := cxDefaultResourceNameColumnWidth;
    Width := Width + cxDefaultResourceNameColumnWidth;
  end
  else if (lvItems.Columns.Count = 3) and not GetShowResourceColumn then
  begin
    Width := Width - cxDefaultResourceNameColumnWidth;
    lvItems.Column[1].Free;
  end;
  CheckColumnCaptions;
end;

procedure TcxSchedulerReminderForm.CheckFormPosition;
begin
  CheckColumnCount;
  inherited CheckFormPosition;
end;

procedure TcxSchedulerReminderForm.CreateSnoozeAdvanceTimeItems;
var
  AMaxAdvanceTime: Integer;
begin
  if lvItems.SelCount = 1 then
  begin
    AMaxAdvanceTime := GetMaxAdvanceMinutes;
    if AMaxAdvanceTime > 15 then
      AddSnoozeAdvanceTime(cxGetResourceString(@scxAdvance15m), 15);
    if AMaxAdvanceTime > 10 then
      AddSnoozeAdvanceTime(cxGetResourceString(@scxAdvance10m), 10);
    if AMaxAdvanceTime > 5 then
      cbSnoozeTime.ItemIndex := AddSnoozeAdvanceTime(cxGetResourceString(@scxAdvance5m), 5);
    if AMaxAdvanceTime > 0 then
      AddSnoozeAdvanceTime(cxGetResourceString(@scxAdvance0h), 0);
  end;
end;

procedure TcxSchedulerReminderForm.DismissEvents(All: Boolean);
var
  I: Integer;
begin
  with Reminders do
  begin
    if All then
      DismissAll
    else
    begin
      BeginUpdate;
      try
        for I := 0 to lvItems.Items.Count - 1 do
          if lvItems.Items[I].Selected then
            VisibleReminders[I].Dismiss;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TcxSchedulerReminderForm.DoShow;
begin
  CheckColumnCount;
  SetupCaptions;
  inherited DoShow;
  with lvItems do
  begin
    if Items.Count > 0 then
      Selected := Items[Items.Count - 1];
  end;
  liOpenItem.Visible := OpenEventSupported;
  RefreshSelectedInfo;
  //force repaint header
  PostMessage(Handle, DXM_REFRESHCUSTOMIZATION, 0, 0);
  tmUpdate.Enabled := True;
end;

function TcxSchedulerReminderForm.GetDueInColumnIndex: Integer;
begin
  Result := Ord(GetShowResourceColumn);
end;

function TcxSchedulerReminderForm.GetImageIndexForReminder(
  AReminder: TcxSchedulerReminder): Integer;
begin
  if AReminder.Event.EventType <> etNone then
    Result := 3
  else
    Result := 2;
end;

function TcxSchedulerReminderForm.GetMaxAdvanceMinutes: Integer;
var
  I: Integer;
  ANow, AStart: TDateTime;
begin
  ANow := Now;
  Result := 0;
  for I := 0 to lvItems.Items.Count - 1 do
  begin
    if lvItems.Items[I].Selected then
    begin
      AStart := TcxSchedulerReminder(lvItems.Items[I].Data).Event.Start;
      Result := Max(Trunc((AStart - ANow) * MinsPerDay), Result);
    end;
  end;
end;

function TcxSchedulerReminderForm.GetResourceNameForReminder(
  AReminder: TcxSchedulerReminder): string;
begin
  Result := Storage.Resources.GetResourceNameByID(AReminder.ResourceID);
end;

function TcxSchedulerReminderForm.GetShowResourceColumn: Boolean;
begin
  Result := Reminders.IsReminderByResourceAvailable and
    Reminders.ShowResourcesInReminderWindow;
end;

function TcxSchedulerReminderForm.GetSnoozeTime: TDateTime;
begin
  with cbSnoozeTime do
  begin
    if ItemIndex >= 0 then
      Result := Integer(Properties.Items.Objects[ItemIndex]) * MinuteToTime
    else
      Result := 5 * MinuteToTime;
  end;
end;

procedure TcxSchedulerReminderForm.LayoutChanged;
begin
  lvItems.Items.BeginUpdate;
  try
    SaveSelection;
    CheckColumnCount;
    UpdateReminderList;
    RestoreSelection;
  finally
    lvItems.Items.EndUpdate;
  end;
end;

procedure TcxSchedulerReminderForm.SnoozeEvents;
var
  I: Integer;
  AIsAdvanceTime: Boolean;
  ASnoozeTime: TDateTime;
begin
  AIsAdvanceTime := (lvItems.SelCount = 1) and (cbSnoozeTime.ItemIndex < AdvanceCount);
  ASnoozeTime := GetSnoozeTime;
  with Reminders do
  begin
    BeginUpdate;
    try
      if AIsAdvanceTime then
        TcxSchedulerReminder(lvItems.Selected.Data).Snooze(-ASnoozeTime)
      else
        for I := 0 to lvItems.Items.Count - 1 do
          if lvItems.Items[I].Selected then
            VisibleReminders[I].Snooze(ASnoozeTime);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxSchedulerReminderForm.OpenFocusedEvent;
var
  AReminder: TcxSchedulerReminder;
begin
  AReminder := GetFocusedReminder;
  if AReminder <> nil then
    OpenEvent(AReminder.Event);
end;

procedure TcxSchedulerReminderForm.RefreshSelectedInfo;
var
  AEnabled: Boolean;
begin
  if IsLocked then Exit;
  AEnabled := lvItems.SelCount > 0;
  if Assigned(lvItems.ItemFocused) then
    lvItems.ItemFocused.MakeVisible(False);
  btnDismiss.Enabled := AEnabled;
  btnDismissAll.Enabled := lvItems.Items.Count > 0;
  btnOpenItem.Enabled := lvItems.SelCount = 1;
  btnSnooze.Enabled := AEnabled;
  cbSnoozeTime.Enabled := AEnabled;
  pbImage.Invalidate;
  UpdateSelectionCaptions;
end;

procedure TcxSchedulerReminderForm.ResetAdvanceCount;
begin
  FAdvanceCount := 0;
end;

procedure TcxSchedulerReminderForm.RestoreSelection;
begin
  with lvItems.Items do
  begin
    BeginUpdate;
    try
      UpdateReminderList;
      SynchronizeSelection;
    finally
      EndUpdate;
    end;
  end;
  FreeAndNil(FSelections);
  RefreshSelectedInfo;
end;

procedure TcxSchedulerReminderForm.SaveSelection;
var
  I: Integer;
begin
  FreeAndNil(FSelections);
  with lvItems do
  begin
    if SelCount = 0 then Exit;
    FSelections := TcxObjectList.Create;
    for I := 0 to Items.Count - 1 do
      if Items[I].Selected and Reminders.IsReminderValid(Items[I].Data) then
        FSelections.Add(TcxSchedulerReminderFormItem.Create(Items[I].Data));
  end;
end;

procedure TcxSchedulerReminderForm.SetupCaptions;
begin
  CheckColumnCaptions;
  btnDismissAll.Caption := cxGetResourceString(@scxrDismissAllButton);
  btnOpenItem.Caption := cxGetResourceString(@scxrOpenItemButton);
  btnDismiss.Caption := cxGetResourceString(@scxrDismissButton);
  btnSnooze.Caption := cxGetResourceString(@scxrSnoozeButton);
end;

procedure TcxSchedulerReminderForm.UpdateDueTimeTexts;
var
  I, AColumnIndex: Integer;
  ANow: TDateTime;
begin
  ANow := Now;
  lvItems.Items.BeginUpdate;
  try
    AColumnIndex := GetDueInColumnIndex;
    for I := 0 to VisibleReminderCount - 1 do
      lvItems.Items[I].SubItems[AColumnIndex] := Reminders.GetEventDueTimeText(VisibleReminders[I], ANow);
  finally
    lvItems.Items.EndUpdate;
  end;
end;

procedure TcxSchedulerReminderForm.UpdateReminderList;
var
  I: Integer;
  AReminder: TcxSchedulerReminder;
  ANow: TDateTime;
  AItem: TListItem;
begin
  ANow := Now;
  with lvItems.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to Reminders.Count - 1 do
      begin
        AReminder := Reminders[I];
        AItem := AddReminder(AReminder);
        AddReminderInfo(AReminder, AItem, Reminders.GetEventDueTimeText(AReminder, ANow));
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxSchedulerReminderForm.ButtonClick(Sender: TObject);
begin
  if Sender is TcxButton then
    case TcxButton(Sender).Tag of
      0: DismissEvents(True);
      1: OpenFocusedEvent;
      2: DismissEvents(False);
      3: SnoozeEvents;
    end;
end;

procedure TcxSchedulerReminderForm.DrawIcon(Sender: TObject);
begin
  if lvItems.SelCount = 1 then
    MenuImages.Draw(pbImage.Canvas,
      (pbImage.Width - MenuImages.Width) div 2,
      (pbImage.Height - MenuImages.Height) div 2,
      GetImageIndexForReminder(lvItems.Selected.Data));
end;

procedure TcxSchedulerReminderForm.lvItemsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if csDestroying in ComponentState then Exit;
  if not Item.Deleting then
    RefreshSelectedInfo;
end;

function TcxSchedulerReminderForm.GetFocusedReminder: TcxSchedulerReminder;
begin
  if lvItems.SelCount = 1 then
    Result := TcxSchedulerReminder(lvItems.Selected.Data)
  else
    Result := nil;
end;

function TcxSchedulerReminderForm.GetVisibleReminder(Index: Integer): TcxSchedulerReminder;
begin
  with lvItems do
    if (Index >= 0) and (Index < Items.Count) then
      Result := TcxSchedulerReminder(Items[Index].Data)
    else
      Result := nil;
end;

function TcxSchedulerReminderForm.GetVisibleReminderCount: Integer;
begin
  Result := lvItems.Items.Count;
end;

procedure TcxSchedulerReminderForm.SynchronizeSelection;
var
  I, J: Integer;
  AItem: TcxSchedulerReminderFormItem;
begin
  if (FSelections = nil) or (VisibleReminderCount = 0) then Exit;
  for I := 0 to FSelections.Count - 1 do
  begin
    AItem := TcxSchedulerReminderFormItem(FSelections[I]);
    for J := 0 to VisibleReminderCount - 1 do
      if AItem.Equals(VisibleReminders[J]) then
        lvItems.Items[J].Selected := True;
  end;
  with lvItems do
  begin
    if SelCount = 0 then
      Items[Items.Count - 1].Selected := True;
    Items[Items.Count - 1].Focused := True;
  end;
end;

procedure TcxSchedulerReminderForm.WMRedrawHeader(var Msg: TMsg);
var
  AHeader: HWND;
begin
  AHeader := ListView_GetHeader(lvItems.InnerListView.Handle);
  Windows.InvalidateRect(AHeader, nil, True);
end;

procedure TcxSchedulerReminderForm.UpdateSelectionCaptions;

  function GetValidCaption(const ACaption: string): string;
  const
    Ellipsis = '...';
  var
    ATotalWidth: Integer;
  begin
    Result := ACaption;
    ATotalWidth := lbEventCaption.Width;

    cxScreenCanvas.Font.Assign(lbEventCaption.Style.Font);
    if cxScreenCanvas.TextWidth(Result) > ATotalWidth then
    begin
      Dec(ATotalWidth, cxScreenCanvas.TextWidth(Ellipsis));
      while (Length(Result) > 0) and (cxScreenCanvas.TextWidth(Result) > ATotalWidth) do
        Delete(Result, Length(Result), 1);
      Result := Result + Ellipsis;
    end;
    cxScreenCanvas.Dormant;
  end;

var
  AEvent: TcxSchedulerEvent;
begin
  if lvItems.SelCount = 1 then
  begin
    AEvent := TcxSchedulerReminder(lvItems.Selected.Data).Event;
    lbEventCaption.Caption := GetValidCaption(AEvent.Caption);
    lbEventStartTime.Caption := Format(cxGetResourceString(@scxrStartTime), [FormatDateTime('dddddd t', AEvent.Start)]);
  end
  else
  begin
    lbEventCaption.Caption := '';
    lbEventStartTime.Caption := Format(cxGetResourceString(@scxrSelected), [lvItems.SelCount]);
  end;
  UpdateSnoozeComboBox;
  inherited UpdateSelectionCaptions;
end;

procedure TcxSchedulerReminderForm.UpdateSnoozeComboBox;
const
  SnoozeTimes: array[1..15] of record P: Pointer; M: Integer end = (
    (P: @scxTime5m;  M: 5),
    (P: @scxTime10m; M: 10),
    (P: @scxTime15m; M: 15),
    (P: @scxTime30m; M: 30),
    (P: @scxTime1h;  M: MinsPerHour),
    (P: @scxTime2h;  M: 2 * MinsPerHour),
    (P: @scxTime4h;  M: 4 * MinsPerHour),
    (P: @scxTime8h;  M: 8 * MinsPerHour),
    (P: @scxTime12h; M: 12 * MinsPerHour),
    (P: @scxTime1d;  M: MinsPerDay),
    (P: @scxTime2d;  M: 2 * MinsPerDay),
    (P: @scxTime3d;  M: 3 * MinsPerDay),
    (P: @scxTime4d;  M: 4 * MinsPerDay),
    (P: @scxTime1w;  M: MinsPerWeek),
    (P: @scxTime2w;  M: 2 * MinsPerWeek));
var
  I: Integer;
begin
  ResetAdvanceCount;
  cbSnoozeTime.Properties.Items.BeginUpdate;
  try
    cbSnoozeTime.Properties.Items.Clear;
    cbSnoozeTime.ItemIndex := -1;
    if lvItems.SelCount = 1 then
      CreateSnoozeAdvanceTimeItems;
    for I := Low(SnoozeTimes) to High(SnoozeTimes) do
      AddSnoozeTime(cxGetResourceString(SnoozeTimes[I].P), SnoozeTimes[I].M);
    if cbSnoozeTime.ItemIndex = -1 then cbSnoozeTime.ItemIndex := 1;
  finally
    cbSnoozeTime.Properties.Items.EndUpdate;
  end;
end;

procedure TcxSchedulerReminderForm.lvItemsDblClick(Sender: TObject);
begin
  OpenFocusedEvent;
end;

procedure TcxSchedulerReminderForm.tmUpdateTimer(Sender: TObject);
begin
  if not IsLocked then
    UpdateDueTimeTexts;
end;

procedure TcxSchedulerReminderForm.lvItemsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    with lvItems do
    begin
      if SelCount > 0 then
        DismissEvents(False)
      else
        if Items.Count > 0 then
        begin
          Items[Items.Count - 1].Selected := True;
          Items[Items.Count - 1].Focused := True;
        end;
    end;
  end;
end;

procedure TcxSchedulerReminderForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and not HasOpenedPopupWindow(ActiveControl) then
    Close;
end;

end.

unit ResourceMainUnit;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxClasses, cxStyles, cxGraphics, cxEdit, cxScheduler,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, ExtCtrls, Menus, ComCtrls, 
  StdCtrls, cxControls, cxSchedulerStorage, ImgList, cxDateUtils, cxSchedulerUtils,
  cxLookAndFeels, ToolWin, cxgeometry, Buttons, cxLookAndFeelPainters,
  cxButtons, Grids, Math, cxSchedulerTimeGridView, cxSchedulerWeekView,
  cxSchedulerYearView, cxSchedulerRecurrence, cxSchedulerAgendaView;

type
  TResourceDemoMainForm = class(TDemoBasicMainForm)
    cxAutumnStyle: TcxStyle;
    cxContentStyle: TcxStyle;
    cxSelectionStyle: TcxStyle;
    cxSpringStyle: TcxStyle;
    cxStyleRepository: TcxStyleRepository;
    cxSummerStyle: TcxStyle;
    cxTimeLineStyle: TcxStyle;
    cxTimeStyle: TcxStyle;
    cxWinterStyle: TcxStyle;
    dlgOpen: TOpenDialog;
    lvCarsBar: TListView;
    imgCars: TImageList;
    imgGlyph: TImageList;
    miCancelReservation: TMenuItem;
    miOpen: TMenuItem;
    miRent: TMenuItem;
    miRentCar: TMenuItem;
    miSaveAs: TMenuItem;
    miUseColorScheme: TMenuItem;
    btnCancelReserv: TSpeedButton;
    btnRent: TSpeedButton;
    pnlTools: TPanel;
    Storage: TcxSchedulerStorage;
    TaskGrid: TStringGrid;
    SmallCars: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure lvCarsBarSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure OpenSaveClick(Sender: TObject);
    procedure RentClick(Sender: TObject);
    procedure SchedulerLayoutChanged(Sender: TObject);
    procedure SchedulerDateNavigatorCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
      var ADone: Boolean);
    procedure SchedulerViewDayCustomDrawContainer(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContainerCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContentCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerStylesGetContentStyle(Sender: TObject;
      AResource: TcxSchedulerStorageResourceItem;
      const ADateTime: TDateTime; var AStyle: TcxStyle);
    procedure miUseColorSchemeClick(Sender: TObject);
    procedure SchedulerEventPopupMenuPopup(
      Sender: TcxSchedulerEventPopupMenu; ABuiltInMenu: TPopupMenu;
      var AHandled: Boolean);
    procedure SchedulerContentPopupMenuPopup(
      Sender: TcxSchedulerContentPopupMenu; ABuiltInMenu: TPopupMenu;
      var AHandled: Boolean);
    procedure SchedulerBeforeDeleting(Sender: TcxCustomScheduler;
      AEvent: TcxSchedulerControlEvent; var Allow: Boolean);
    procedure SchedulerSelectionChanged(Sender: TObject);
    procedure TaskGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    TaskEvents: TcxSchedulerEventList;
  public
    procedure CreateRecurrenceEvent(AResourceID: Integer;
      const ACaption: string; AOccurDays: TDays);
    procedure DrawDateNavigatorContent(ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo; var ADone: Boolean);
    procedure FillTaskGrid;
    function GetResourceColor(AResource: TcxSchedulerResourceViewInfo): TColor;
    procedure SelectEvents(AStartIndex: Integer);
    procedure SyncVisibility(ADate: TDateTime; AIndex: Integer);
  end;

  function GetIndex(AListView: TListView): Integer;
  procedure SetIndex(AListView: TListView; AIndex: Integer);

var
  ResourceDemoMainForm: TResourceDemoMainForm;
const
  ContentColors: array[0..3] of Integer =
    (12317183, 16574923, 12381397, 12701439);


implementation

uses RentUnit, CancelReservationUnit, dxOffice11;

{$R *.dfm}

function GetIndex(AListView: TListView): Integer;
begin
  Result := AListView.ItemIndex;
end;

procedure SetIndex(AListView: TListView; AIndex: Integer);
begin
  AListView.ItemIndex := AIndex;
end;

function scxTaskCompare(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  Result := AnsiCompareText(AEvent1.Caption, AEvent2.Caption);
  if Result = 0 then
  begin
    Result := AnsiCompareText(AEvent1.GetResourceItem.Name,
      AEvent2.GetResourceItem.Name);
  end;
  if (Result = 0) and (AEvent1.Start < AEvent2.Start) then
     Result := -1;
end;

procedure TResourceDemoMainForm.CreateRecurrenceEvent(AResourceID: Integer;
  const ACaption: string; AOccurDays: TDays);
begin
  with Storage.createEvent do
  begin
    ResourceID := AResourceID;
    Caption := ACaption;
    Duration := 45 * MinuteToTime;
    MoveTo(Date + (8 + AResourceID) * HourToTime);
    EventType := etPattern;
    LabelColor := $51B0F7;
    RecurrenceInfo.Count := -1;
    RecurrenceInfo.Recurrence := cxreWeekly;
    RecurrenceInfo.Periodicity := 1;
    RecurrenceInfo.OccurDays := AOccurDays;
    RecurrenceInfo.DayType := cxdtDay;
  end;
end;

procedure TResourceDemoMainForm.DrawDateNavigatorContent(ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo; var ADone: Boolean);
var
  ABitmap: TBitmap;
begin
  if not miUseColorScheme.Checked then Exit;
  case AViewInfo.Month of
    3..5:
      ABitmap := cxSpringStyle.Bitmap;
    6..8:
      ABitmap := cxSummerStyle.Bitmap;
    9..11:
      ABitmap := cxAutumnStyle.Bitmap;
  else
    ABitmap := cxWinterStyle.Bitmap;
  end;
  ACanvas.Canvas.StretchDraw(AViewInfo.Bounds, ABitmap);
  ADone := True;
  AViewInfo.Transparent := True;
end;

procedure TResourceDemoMainForm.FillTaskGrid;
var
  ASelRow, I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  if TaskEvents = nil then Exit;
  TaskEvents.Clear;
  for I := 0 to Storage.EventCount - 1 do
    if Storage.Events[I].EventType = etNone then
      TaskEvents.Add(Storage.Events[I]);
  TaskEvents.Sort(scxTaskCompare);
  TaskGrid.RowCount := Max(TaskEvents.Count + 1, 2);
  TaskGrid.Rows[1].Clear;
  ASelRow := 1;
  for I := 0 to TaskEvents.Count - 1 do
  begin
    AEvent := TaskEvents[I];
    TaskGrid.Cells[0, I + 1] := AEvent.Caption;
    TaskGrid.Cells[1, I + 1] := AEvent.GetResourceItem.Name;
    TaskGrid.Cells[2, I + 1] := DateTimeToStr(AEvent.Start);
    TaskGrid.Cells[3, I + 1] := DateTimeToStr(AEvent.Finish);
    if (Scheduler.SelectedEventCount > 0) and (Scheduler.SelectedEvents[0].Source = AEvent) then
      ASelRow := I + 1;
  end;
  TaskGrid.Selection := TGridRect(Rect(0, ASelRow, 3, ASelRow));
end;

function TResourceDemoMainForm.GetResourceColor(
  AResource: TcxSchedulerResourceViewInfo): TColor;
begin
  Result := ContentColors[AResource.ResourceItem.ID];
end;

procedure TResourceDemoMainForm.SelectEvents(AStartIndex: Integer);
var
  I: Integer;
begin
  Scheduler.BeginUpdate;
  try
    Scheduler.UnselectEvents;
    for I := 0 to Scheduler.VisibleEventCount - 1 do
      if Scheduler.VisibleEvents[I].Index >= AStartIndex then
        Scheduler.SelectEvent(Scheduler.VisibleEvents[I], [ssCtrl, ssShift]);
  finally
    Scheduler.EndUpdate;
    Scheduler.CurrentView.Refresh;
  end;
end;

procedure TResourceDemoMainForm.SyncVisibility(
  ADate: TDateTime; AIndex: Integer);
begin
  if (GetIndex(lvCarsBar) <> 0) and (AIndex <> -1) then
    SetIndex(lvCarsBar, AIndex + 1);
  if ADate <> NullDate then
  begin
    with Scheduler.SelectedDays do
      if not ((ADate >= Items[0]) and (ADate <= Items[Count - 1])) then
        Scheduler.SelectDays(ADate, ADate + Count - 1, True);
  end;
end;

procedure TResourceDemoMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  TaskEvents := TcxSchedulerEventList.Create;
  inherited;
  miEventsOpt.Visible := False; 
  WindowState := wsMaximized;
  lvCarsBar.Items[0].Selected := True;
  Storage.BeginUpdate;
  for I := 0 to 3 do
  begin
    CreateRecurrenceEvent(I, 'Maintenance', [dMonday]);
    CreateRecurrenceEvent(I, 'Car wash', [dWednesday, dSaturday]);
  end;
  Storage.EndUpdate;
  miUseColorSchemeClick(miUseColorScheme);
  TaskGrid.Cells[0, 0] := 'Customer';
  TaskGrid.Cells[1, 0] := 'Model';
  TaskGrid.Cells[2, 0] := 'Start rent';
  TaskGrid.Cells[3, 0] := 'Finish rent';
end;

procedure TResourceDemoMainForm.lvCarsBarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  I: Integer;
begin
  inherited;
  if not Selected then Exit;
  Storage.BeginUpdate;
  try
    for I := 0 to Storage.ResourceCount - 1 do
      with Storage.Resources.ResourceItems[I] do
        Visible :=(Item.Index = 0) or (ID = (Item.Index - 1));
  finally
    Storage.EndUpdate;
    Scheduler.FullRefresh;
  end;
end;

procedure TResourceDemoMainForm.OpenSaveClick(Sender: TObject);
begin
  inherited;
  if (TMenuItem(Sender).Tag = 0) and dlgOpen.Execute then
    Storage.LoadFromFile(dlgOpen.FileName)
  else
    if (TMenuItem(Sender).Tag = 1) and SaveDialog.Execute then
      Storage.SaveToFile(SaveDialog.FileName);
end;

procedure TResourceDemoMainForm.RentClick(Sender: TObject);
var
  ACount: Integer;
begin
  if (Sender as TComponent).Tag = 0 then
    with TfrmRentCar.Create(nil) do
    try
      ACount := Storage.EventCount;
      if ShowModal = mrOk then
      begin
        SyncVisibility(DateNavigator.Date, GetIndex(lvCars));
        SelectEvents(ACount);
      end;
    finally
      Free;
    end
  else
    with TfrmCancelReservation.Create(nil) do
    try
      if ShowModal = mrOk then
        SyncVisibility(Date, Index);
    finally
      Free;
    end;
  FillTaskGrid;
end;

procedure TResourceDemoMainForm.SchedulerLayoutChanged(Sender: TObject);
begin
  FillTaskGrid;
end;

procedure TResourceDemoMainForm.SchedulerDateNavigatorCustomDrawContent(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
  var ADone: Boolean);
begin
  DrawDateNavigatorContent(ACanvas, AViewInfo, ADone);
end;

procedure TResourceDemoMainForm.SchedulerViewDayCustomDrawContainer(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerContainerCellViewInfo; var ADone: Boolean);
begin
  if not AViewInfo.Selected and miUseColorScheme.Checked and (AViewInfo.Resource <> nil) then
    ACanvas.Brush.Color := dxGetMiddleRGB(GetResourceColor(AViewInfo.Resource), 0, 80);
end;

procedure TResourceDemoMainForm.SchedulerCustomDrawContent(Sender: TObject;
  ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContentCellViewInfo;
  var ADone: Boolean);
var
  AParams: TcxViewParams;
begin
  if miUseColorScheme.Checked and (AViewInfo.Resource <> nil) then
  begin
    AParams := AViewInfo.TimeLineParams;
    AParams.Color := dxGetMiddleRGB(GetResourceColor(AViewInfo.Resource), 0, 80);
    AViewInfo.TimeLineParams := AParams;
  end;
end;

procedure TResourceDemoMainForm.SchedulerStylesGetContentStyle(
  Sender: TObject; AResource: TcxSchedulerStorageResourceItem;
  const ADateTime: TDateTime; var AStyle: TcxStyle);
begin
  if miUseColorScheme.Checked and (AResource <> nil) then
  begin
    AStyle := cxContentStyle;
    AStyle.Color := ContentColors[AResource.ID];
  end;
end;

procedure TResourceDemoMainForm.miUseColorSchemeClick(Sender: TObject);
begin
  miUseColorScheme.Checked := not miUseColorScheme.Checked;
  if miUseColorScheme.Checked then
    Scheduler.DateNavigator.Styles.Selection := cxSelectionStyle
  else
    Scheduler.DateNavigator.Styles.Selection := nil;
  Scheduler.LayoutChanged; 
end;

procedure TResourceDemoMainForm.SchedulerEventPopupMenuPopup(
  Sender: TcxSchedulerEventPopupMenu; ABuiltInMenu: TPopupMenu;
  var AHandled: Boolean);
begin
  inherited;
  if (Sender.Event <> nil) and Sender.Event.IsRecurring then
    AHandled := True
  else
    Sender.GetMenuItem(epmiDelete).Caption := 'Cancel reservation';
end;

procedure TResourceDemoMainForm.SchedulerContentPopupMenuPopup(
  Sender: TcxSchedulerContentPopupMenu; ABuiltInMenu: TPopupMenu;
  var AHandled: Boolean);
var
  AItem: TMenuItem;
begin
  inherited;
  ABuiltInMenu.Images := imgGlyph;
  AItem := NewItem('Rent a car', scNone, False, True, RentClick, 0, '');
  AItem.ImageIndex := 0;
  ABuiltInMenu.Items.Insert(0, AItem);
  AItem := NewItem('Cancel reservation', scNone, False, True, RentClick, 0, '');
  AItem.ImageIndex := 1;
  AItem.Tag := 1;
  ABuiltInMenu.Items.Insert(1, AItem);
  ABuiltInMenu.Items.Insert(2, NewLine);
end;

procedure TResourceDemoMainForm.SchedulerBeforeDeleting(
  Sender: TcxCustomScheduler; AEvent: TcxSchedulerControlEvent;
  var Allow: Boolean);
begin
  Allow := not AEvent.IsRecurring;
end;

procedure TResourceDemoMainForm.SchedulerSelectionChanged(Sender: TObject);
begin
  FillTaskGrid;
end;

procedure TResourceDemoMainForm.TaskGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  CanSelect := ARow > 0;
  if (ARow > 0) and (ARow <= TaskEvents.Count) then
  begin
    AEvent := TaskEvents[ARow - 1];
    SyncVisibility(dxDateOf(AEvent.Start), AEvent.GetResourceItem.ID);
    Scheduler.UnselectEvents;
    for I := 0 to Scheduler.VisibleEventCount - 1 do
      if Scheduler.VisibleEvents[I].Source = AEvent then
        Scheduler.SelectEvent(Scheduler.VisibleEvents[I], [ssShift]);
  end;
end;

end.



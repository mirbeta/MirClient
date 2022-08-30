unit RentUnit;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxGraphics, cxEdit, cxScheduler, cxSchedulerUtils,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView, cxSchedulerStorage,
  cxSchedulerDayView, cxSchedulerDateNavigator, StdCtrls, cxContainer,
  cxDateNavigator, cxControls, ComCtrls, ExtCtrls, Math, cxDateUtils,
  DateUtils,cxLookAndFeelPainters, cxButtons, cxTextEdit, cxListBox, cxListView,
  ToolWin, Menus, cxSchedulerTimeGridView, cxSchedulerWeekView,
  cxSchedulerYearView;

type
  TfrmRentCar = class(TForm)
    lvCars: TListView;
    DateNavigator: TcxDateNavigator;
    edtUserName: TcxTextEdit;
    lbxPeriod: TcxListBox;
    TimeScheduler: TcxScheduler;
    btnRent: TcxButton;
    btnCancel: TcxButton;
    lbChoosePeriod: TLabel;
    lbChooseDate: TLabel;
    lbChooseCar: TLabel;
    lbCustomerName: TLabel;
    lbChooseTime: TLabel;
    cxStyleRepository: TcxStyleRepository;
    cxBoldStyle: TcxStyle;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimeSchedulerCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContentCellViewInfo;
      var ADone: Boolean);
    procedure DateNavigatorSelectionChanged(Sender: TObject;
      const AStart, AFinish: TDateTime);
    procedure DateNavigatorCustomDrawDayNumber(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorDayNumberViewInfo;
      var ADone: Boolean);
    procedure DateNavigatorCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
      var ADone: Boolean);
    procedure lvCarsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lbxPeriodClick(Sender: TObject);
    procedure TimeSchedulerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRentClick(Sender: TObject);
  private
    FDayEvents: TcxSchedulerEventList;
    FDays: TcxSchedulerDateList;
    FEventsList: TcxSchedulerFilteredEventList;
    FPeriods: TcxSchedulerDateList;
    procedure AddPeriodTime(AStart, AFinish: TDateTime);
    procedure AddRent(AStart, AFinish: TDateTime);
    function DateTimeToTime(ATime: TDateTime): TDateTime;
    procedure FillTimeList;
    function Intersect(AStart1, AFinish1, AStart2, AFinish2: TDateTime): Boolean;
    function IntersectTime(var AStart1, AFinish1, AStart2, AFinish2: TDateTime): Boolean;
    procedure ProcessSelectItem(AIndex: Integer);
    procedure RentCar;
    function Storage: TcxSchedulerStorage;
    function TimeToDateTime(ATime: TDateTime): TDateTime;
  end;

implementation

uses
  ResourceMainUnit;


{$R *.dfm}

function CompareEvents(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  if AEvent1.Start = AEvent2.Start then
    Result := 0
  else
    if AEvent1.Start < AEvent2.Start then
      Result := -1
    else
      Result := 1;
end;

procedure TfrmRentCar.AddPeriodTime(AStart, AFinish: TDateTime);
begin
  if AStart = AFinish then Exit;
  FPeriods.Add(TimeToDateTime(AStart));
  FPeriods.Add(TimeToDateTime(AFinish));
  lbxPeriod.Items.Add(TimeToStr(AStart)  + '-' + TimeToStr(AFinish));
end;

procedure TfrmRentCar.AddRent(AStart, AFinish: TDateTime);
begin
  if AStart >= AFinish then Exit;
  with Storage.createEvent do
  begin
    Start := AStart;
    Finish := AFinish;
    Caption := edtUserName.Text;
    ResourceID := GetIndex(lvCars);
  end;
end;

function TfrmRentCar.DateTimeToTime(ATime: TDateTime): TDateTime;
begin
  Result := DateNavigator.Date +
    EncodeTime(Round(ATime) div 60, Round(ATime) mod 60, 0, 0);
end;

procedure TfrmRentCar.FillTimeList;
var
  I: Integer;
  AInfo: TcxSchedulerEventConflictsInfo;
begin
  lbxPeriod.Items.BeginUpdate;
  try
    FPeriods.Clear;
    lbxPeriod.Items.Clear;
    AInfo := TcxSchedulerEventConflictsInfo.Create(
      Storage, False, DateNavigator.Date + EncodeTime(0, 0, 0, 0),
      DateNavigator.Date + EncodeTime(23, 59, 0, 0), GetIndex(lvCars));
    try
      for I := 0 to AInfo.TimeRanges.Count - 1 do
        AddPeriodTime(AInfo.TimeRanges[I].Start, AInfo.TimeRanges[I].Finish);
    finally
      AInfo.Free;
    end;
  finally
    lbxPeriod.Items.EndUpdate;
  end;
end;

function TfrmRentCar.Intersect(AStart1, AFinish1, AStart2, AFinish2: TDateTime): Boolean;
begin
  Result := (AStart1 < AFinish2) and (AStart2 < AFinish1);
end;

function TfrmRentCar.IntersectTime(
  var AStart1, AFinish1, AStart2, AFinish2: TDateTime): Boolean;
begin
  if AStart2 > AStart1 then
    AStart1 := AStart2;
  if AFinish2 < AFinish1 then
    AFinish1 := AFinish2;
  Result := AFinish1 > AStart1;
end;

procedure TfrmRentCar.ProcessSelectItem(AIndex: Integer);
var
  I, J: Integer;
begin
  FDays.Clear;
  Storage.GetEvents(FEventsList, DateNavigator.RealFirstDate,
    DateNavigator.RealLastDate, AIndex);
  for I := Trunc(DateNavigator.RealFirstDate) to Trunc(DateNavigator.RealLastDate) do
    for J := 0 to FEventsList.Count - 1 do
      if FEventsList[J].IsDayEvent(I) then
      begin
        FDays.Add(I);
        Break
      end;
  DateNavigator.LayoutChanged;
  DateNavigatorSelectionChanged(DateNavigator, DateNavigator.Date, DateNavigator.Date);
end;

procedure TfrmRentCar.RentCar;
var
  I: Integer;
  AStart, AFinish, AStart1, AFinish1: TDateTime;
begin
  AStart := TimeScheduler.SelStart;
  AFinish := TimeScheduler.SelFinish;
  try
    for I := 0 to FDayEvents.Count do
    begin
      if I = 0 then
        AStart1 := DateOf(AStart);
      if I = FDayEvents.Count then
        AFinish1 := DateOf(AStart) + EncodeTime(23, 59, 0, 0)
      else
        AFinish1 := FDayEvents[I].Start;
      if (AStart1 < AFinish1) and IntersectTime(AStart1, AFinish1, AStart, AFinish) then
        AddRent(AStart1, AFinish1);
      if (FDayEvents.Count > 0) and (I < FDayEvents.Count) then
        AStart1 := FDayEvents[I].Finish;
    end;
  finally
    Storage.PostEvents;
    ProcessSelectItem(GetIndex(lvCars));
    TimeScheduler.Refresh;
  end;
end;

function TfrmRentCar.Storage: TcxSchedulerStorage;
begin
  Result := ResourceDemoMainForm.Storage;
end;

function TfrmRentCar.TimeToDateTime(ATime: TDateTime): TDateTime;
var
  H, M, S, MS: Word;
begin
  DecodeTime(ATime, H, M, S, MS);
  Result := H * 60 + M;
end;

procedure TfrmRentCar.FormCreate(Sender: TObject);
begin
  FPeriods := TcxSchedulerDateList.Create;
  FEventsList := TcxSchedulerFilteredEventList.Create;
  FDays := TcxSchedulerDateList.Create;
  FDayEvents := TcxSchedulerEventList.Create;
end;

procedure TfrmRentCar.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEventsList);
  FreeAndNil(FDays);
  FreeAndNil(FDayEvents);
  FreeAndNil(FPeriods);
end;

procedure TfrmRentCar.FormShow(Sender: TObject);
var
  AScheduler: TcxCustomScheduler;
begin
  Storage.FullRefresh;
  AScheduler := ResourceDemoMainForm.Scheduler;
  if AScheduler.SelResource = nil then
    SetIndex(lvCars, 0)
  else
    SetIndex(lvCars, AScheduler.SelResource.ID);
  ProcessSelectItem(GetIndex(lvCars));
  DateNavigator.Date := DateOf(AScheduler.SelStart);
  ProcessSelectItem(GetIndex(lvCars));
  TimeScheduler.SelectDays(DateNavigator.Date, DateNavigator.Date, True);
  TimeScheduler.SelectTime(AScheduler.SelStart, AScheduler.SelFinish, nil);
end;

procedure TfrmRentCar.TimeSchedulerCustomDrawContent(Sender: TObject;
  ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContentCellViewInfo;
  var ADone: Boolean);
var
  I: Integer;
const
  scxBusyTime = 'Busy time';
begin
  for I := 0 to FDayEvents.Count - 1 do
    if Intersect(AViewInfo.TimeStart, AViewInfo.TimeFinish,
      FDayEvents[I].Start, FDayEvents[I].Finish) then
    begin
      ACanvas.Brush.Color := clBtnShadow;
      ACanvas.FillRect(AViewInfo.Bounds);
      ACanvas.Font.Color := clBtnText;
      ACanvas.DrawTexT(scxBusyTime, AViewInfo.Bounds, cxAlignCenter);
      AViewInfo.Transparent := True;
    end;
end;

procedure TfrmRentCar.DateNavigatorSelectionChanged(Sender: TObject;
  const AStart, AFinish: TDateTime);
var
  I: Integer;
begin
  FDayEvents.Clear;
  for I := 0 to FEventsList.Count - 1 do
    if FEventsList[I].IsDayEvent(Trunc(AStart)) then
      FDayEvents.Add(FEventsList[I]);
  FDayEvents.Sort(CompareEvents);
  FillTimeList;
  TimeScheduler.SelectDays(AStart, AFinish, True);
  lbxPeriod.ItemIndex := 0;
  lbxPeriodClick(lbxPeriod);
end;

procedure TfrmRentCar.DateNavigatorCustomDrawDayNumber(Sender: TObject;
  ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorDayNumberViewInfo;
  var ADone: Boolean);
begin
  if FDays.IndexOf(AViewInfo.Date) <> -1 then
    ACanvas.Font := cxBoldStyle.Font;
end;

procedure TfrmRentCar.DateNavigatorCustomDrawContent(Sender: TObject;
  ACanvas: TcxCanvas; AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
  var ADone: Boolean);
begin
  ResourceDemoMainForm.DrawDateNavigatorContent(ACanvas, AViewInfo, ADone);
end;

procedure TfrmRentCar.lvCarsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then Exit;
  ProcessSelectItem(Item.Index);
end;

procedure TfrmRentCar.lbxPeriodClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := lbxPeriod.ItemIndex;
  if AIndex <> -1 then
  begin
    TimeScheduler.SelectTime(DateTimeToTime(FPeriods[AIndex * 2]),
      DateTimeToTime(FPeriods[AIndex * 2 + 1]) + HourToTime, nil);
  end;
end;

procedure TfrmRentCar.TimeSchedulerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AControl: TWinControl;
begin
  if Key <> VK_TAB then Exit;
  AControl := FindNextControl(TimeScheduler, not (ssCtrl in Shift), True, True);
  if AControl <> nil then
    AControl.SetFocus;
end;

procedure TfrmRentCar.btnRentClick(Sender: TObject);
begin
  RentCar;
end;


end.



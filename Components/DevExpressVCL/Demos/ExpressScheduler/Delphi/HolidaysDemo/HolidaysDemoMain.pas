unit HolidaysDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Variants, 
  Graphics, Controls, Forms, Dialogs, DemoBasicMain, cxLookAndFeels, ActnList,
  ImgList, Menus, StdCtrls, ComCtrls, cxStyles, cxGraphics, cxEdit, cxScheduler,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, cxSchedulerStorage,
  cxControls, cxContainer, cxTextEdit, cxMemo, cxRichEdit, ExtCtrls, Buttons,
  cxSchedulerWeekView, cxSchedulerTimeGridView, cxSchedulerUtils,
  cxSchedulerYearView, cxSchedulerHolidays, cxHint, cxSchedulerGanttView,
  cxLookAndFeelPainters, cxButtons, cxSchedulerRecurrence,
  cxClasses, cxSchedulerAgendaView;

const
  ColorsCount = 4;
  Colors: array [0..3] of TColor = (clRed, clYellow, clGreen, clBlue);

type
  THolidaysDemoMainForm = class(TDemoBasicMainForm)
    SchedulerUnboundStorage: TcxSchedulerStorage;
    EventImages: TImageList;
    Holidays: TcxSchedulerHolidays;
    Holidays1: TMenuItem;
    miShowHolidaysHints: TMenuItem;
    miHighlightHolidays: TMenuItem;
    miHolidaySeparator1: TMenuItem;
    miHighlight: TMenuItem;
    miRed: TMenuItem;
    miYellow: TMenuItem;
    miGreen: TMenuItem;
    miBlue: TMenuItem;
    PopupMenu: TPopupMenu;
    Forall1: TMenuItem;
    OnllyFOXSPORTS11: TMenuItem;
    FOXFOOTYandFUEL1: TMenuItem;
    btnGenerate: TcxButton;
    btnHolidaysEditor: TcxButton;
    imgResources: TcxImageList;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SchedulerInitEventImages(Sender: TcxCustomScheduler;
      AEvent: TcxSchedulerControlEvent; AImages: TcxSchedulerEventImages);
    procedure SchedulerUnboundStorageRemindersOpenEvent(
      Sender: TcxSchedulerReminders; AEvent: TcxSchedulerControlEvent);
    procedure miShowHolidaysHintsClick(Sender: TObject);
    procedure miHighlightClick(Sender: TObject);
    procedure miColorClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Forall1Click(Sender: TObject);
    procedure OnllyFOXSPORTS11Click(Sender: TObject);
    procedure FOXFOOTYandFUEL1Click(Sender: TObject);
    procedure SchedulerShowDateHint(Sender: TObject;
      const ADate: TDateTime; var AHintText: String; var AAllow: Boolean);
    procedure btnHolidaysEditorClick(Sender: TObject);
  private
    FArrayBitmap: array [0..ColorsCount] of TBitmap;
  protected
    procedure OnNewEvent(AEvent: TcxSchedulerEvent; AIndex: Integer); override;
  public
    { Public declarations }
  end;

var
  HolidaysDemoMainForm: THolidaysDemoMainForm;

implementation

{$R *.dfm}
uses
  cxSchedulerDialogs, Types;

const
  GenerateCount = 500;

procedure THolidaysDemoMainForm.btnDeleteClick(Sender: TObject);
var
  AHolidays: TcxSchedulerHolidays;
begin
  AHolidays := Holidays;
  cxShowHolidaysEditor(AHolidays, Scheduler.LookAndFeel);
end;

procedure THolidaysDemoMainForm.btnGenerateClick(Sender: TObject);
var
  APos: TPoint;
begin
  APos := btnGenerate.ClientToScreen(Point(0, 0));
  PopupMenu.Popup(APos.X, APos.Y);
end;

procedure THolidaysDemoMainForm.FormCreate(Sender: TObject);

  function CreateBitmap(AColor: TColor): TBitmap;
  var
    ADefaultSize: TSize;
  begin
    ADefaultSize.cx := 13;
    ADefaultSize.cy := 13;
    Result := TBitmap.Create;
    with Result do
    begin
      Width := ADefaultSize.cx;
      Height := ADefaultSize.cy;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(Rect(0, 0, ADefaultSize.cx, ADefaultSize.cy));
      Canvas.Brush.Color := AColor;
      Canvas.FillRect(Rect(1, 1, ADefaultSize.cx - 1, ADefaultSize.cy - 1));
    end;
  end;
var
  I: Integer;
begin
  inherited;
  Scheduler.SelectDays(Date, Date, True);
  for I := 0 to ColorsCount - 1 do
    FArrayBitmap[I] := CreateBitmap(Colors[I]);
  miRed.Bitmap := FArrayBitmap[0];
  miYellow.Bitmap := FArrayBitmap[1];
  miGreen.Bitmap := FArrayBitmap[2];
  miBlue.Bitmap := FArrayBitmap[3];
  Holidays.RestoreFromIniFile('..\..\Data\Holidays.ini');
end;

procedure THolidaysDemoMainForm.SchedulerInitEventImages(
  Sender: TcxCustomScheduler; AEvent: TcxSchedulerControlEvent;
  AImages: TcxSchedulerEventImages);
var
  AValue: Variant;
begin
  AValue := AEvent.GetCustomFieldValueByName('IconIndex');
  if VarIsNull(AValue) or VarIsEmpty(AValue) or (AValue = -1) then Exit;
  AImages.Add(AValue, False);
end;

procedure THolidaysDemoMainForm.OnNewEvent(
  AEvent: TcxSchedulerEvent; AIndex: Integer);
begin
  AEvent.SetCustomFieldValueByName('IconIndex', AIndex);
  AEvent.Reminder := (Trunc(AEvent.Start) >= Trunc(Now)) and Boolean(Random(2));
end;


procedure THolidaysDemoMainForm.SchedulerUnboundStorageRemindersOpenEvent(
  Sender: TcxSchedulerReminders; AEvent: TcxSchedulerControlEvent);
begin
  Scheduler.EditEventUsingDialog(AEvent);
end;

procedure THolidaysDemoMainForm.miShowHolidaysHintsClick(Sender: TObject);
begin
  Scheduler.OptionsView.ShowHints := not Scheduler.OptionsView.ShowHints;
  miShowHolidaysHints.Checked := Scheduler.OptionsView.ShowHints;
end;

procedure THolidaysDemoMainForm.miHighlightClick(Sender: TObject);
begin
  Scheduler.DateNavigator.ShowDatesContainingHolidaysInColor := not Scheduler.DateNavigator.ShowDatesContainingHolidaysInColor;
  miHighlight.Checked := Scheduler.DateNavigator.ShowDatesContainingHolidaysInColor;
  miShowHolidaysHints.Enabled := miHighlight.Checked;
end;

procedure THolidaysDemoMainForm.miColorClick(Sender: TObject);
begin
  Scheduler.DateNavigator.HolidayColor := Colors[(Sender as TMenuItem).Tag];
end;

procedure THolidaysDemoMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ColorsCount - 1 do
    FArrayBitmap[I].Free;
  inherited;
end;

procedure THolidaysDemoMainForm.Forall1Click(Sender: TObject);
var
  I: Integer;
  AArrayVariant: array of Variant;
begin
  SetLength(AArrayVariant, SchedulerUnboundStorage.ResourceCount);
  try
    for I := 0 to SchedulerUnboundStorage.ResourceCount - 1 do
      AArrayVariant[I] := SchedulerUnboundStorage.ResourceIDs[I];
    SchedulerUnboundStorage.GenerateHolidayEvents(VarArrayOf(AArrayVariant));
  finally
    SetLength(AArrayVariant, 0);
  end;
end;

procedure THolidaysDemoMainForm.OnllyFOXSPORTS11Click(Sender: TObject);
begin
  SchedulerUnboundStorage.GenerateHolidayEvents('0');
end;

procedure THolidaysDemoMainForm.FOXFOOTYandFUEL1Click(Sender: TObject);
var
  AResources: array [0..1] of Variant;
begin
  AResources[0] := '3';
  AResources[1] := '4';
  SchedulerUnboundStorage.GenerateHolidayEvents(VarArrayOf(AResources));
end;

procedure THolidaysDemoMainForm.SchedulerShowDateHint(Sender: TObject;
  const ADate: TDateTime; var AHintText: String; var AAllow: Boolean);
begin
  if AHintText = '' then
    AHintText := DateToStr(ADate);
  AAllow := True;
end;

procedure THolidaysDemoMainForm.btnHolidaysEditorClick(Sender: TObject);
var
  AHolidays: TcxSchedulerHolidays;
begin
  AHolidays := Holidays;
  cxShowHolidaysEditor(AHolidays, Scheduler.LookAndFeel);
end;

end.

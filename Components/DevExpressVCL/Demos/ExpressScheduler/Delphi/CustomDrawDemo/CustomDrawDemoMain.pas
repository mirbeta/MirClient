unit CustomDrawDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Types, 
  Dialogs, DemoBasicMain, cxClasses, cxStyles, cxGraphics, cxEdit, cxLookAndFeelPainters, cxScheduler,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, cxLookAndFeels, Menus,
  StdCtrls, cxControls, cxSchedulerStorage, ExtCtrls, ComCtrls,
  cxSchedulerTimeGridView, cxSchedulerUtils, cxSchedulerWeekView,
  cxSchedulerYearView, cxGeometry, cxSchedulerAgendaView, cxSchedulerHolidays,
  cxSchedulerGanttView;

type
  TCustomDrawDemoMainForm = class(TDemoBasicMainForm)
    Storage: TcxSchedulerStorage;
    CustomDraw1: TMenuItem;
    miEvents: TMenuItem;
    miTimeRuler: TMenuItem;
    miHeaders: TMenuItem;
    miContent: TMenuItem;
    DateNavigator1: TMenuItem;
    miMonthHeaders: TMenuItem;
    miDayCaptions: TMenuItem;
    miDays: TMenuItem;
    ViewDay1: TMenuItem;
    miContainer: TMenuItem;
    miResources: TMenuItem;
    miGroupSeparator: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    csBoldItalic: TcxStyle;
    csItalic: TcxStyle;
    csRed: TcxStyle;
    miDNContent: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure SchedulerDateNavigatorCustomDrawHeader(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorMonthHeaderViewInfo;
      var ADone: Boolean);
    procedure SchedulerDateNavigatorCustomDrawDayCaption(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorDayCaptionViewInfo;
      var ADone: Boolean);
    procedure SchedulerDateNavigatorCustomDrawDayNumber(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorDayNumberViewInfo;
      var ADone: Boolean);
    procedure SchedulerDateNavigatorCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawDayHeader(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerDayHeaderCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerViewDayCustomDrawRuler(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerTimeRulerCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerViewDayCustomDrawContainer(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContainerCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawEvent(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerEventCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawContent(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContentCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawResourceHeader(Sender: TObject;
      ACanvas: TcxCanvas; AViewInfo: TcxSchedulerHeaderCellViewInfo;
      var ADone: Boolean);
    procedure SchedulerCustomDrawGroupSeparator(Sender: TObject;
      ACanvas: TcxCanvas;
      AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo;
      var ADone: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateCustomDraw(Sender: TObject);
  private
    FDaysOffIndexes: array[1..2] of Byte;
  end;

var
  CustomDrawDemoMainForm: TCustomDrawDemoMainForm;

implementation

uses
  cxDateUtils, DateUtils,dxOffice11, cxFormats;

{$R *.dfm}

var
  W1, H1, C1, C2: Integer;
  ABitmap: TBitmap;

procedure FillGradientRect(ACanvas: TcxCanvas;
  const ARect: TRect; AColor1, AColor2: TColor; AHorizontal: Boolean);
begin
  with ARect do
  begin
    W1 := Right - Left;
    H1 := Bottom - Top;
    if (W1 < 1) or (H1 < 1) then Exit;
    if (ABitmap.Height <> H1) or (ABitmap.Width <> W1) or (AColor1 <> C1) or (AColor2 <> C2) then
    begin
      C1 := AColor1;
      C2 := AColor2;
      ABitmap.Free;
      ABitmap := TBitmap.Create;
      ABitmap.PixelFormat := pf32Bit;
      ABitmap.Height := H1;
      ABitmap.Width := W1;
      Office11FillTubeGradientRect(ABitmap.Canvas.Handle, Rect(0, 0, W1, H1), AColor1, AColor2, AHorizontal);
    end;
    BitBlt(ACanvas.Handle, Left, Top, W1, H1, ABitmap.Canvas.Handle, 0, 0, srcCopy);
  end;
end;

procedure TCustomDrawDemoMainForm.FormCreate(Sender: TObject);
var
  I, J: Integer;
begin
  inherited FormCreate(Sender);
  ABitmap := TBitmap.Create;
  ABitmap.PixelFormat := pf32bit;
  Scheduler.OptionsView.RotateResourceCaptions := False;
  Scheduler.ViewDay.EventShadows := False;
  I := Ord(cxFormatController.StartOfWeek) + Ord(dSunday);
  J := Ord(dSunday) - I;
  if J < 0 then Inc(J, 7);
  FDaysOffIndexes[1] := J;
  J := Ord(dSaturday) - I;
  if J < 0 then Inc(J, 7);
  FDaysOffIndexes[2] := J;
  GenerateRandomEvents(100, True);
end;

procedure TCustomDrawDemoMainForm.SchedulerDateNavigatorCustomDrawHeader(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorMonthHeaderViewInfo;
  var ADone: Boolean);
begin
  if not miMonthHeaders.Checked then Exit;
  FillGradientRect(ACanvas, AViewInfo.Bounds, $FFE0E0, $FF8080, False);
  ACanvas.FrameRect(AViewInfo.Bounds, clBlue);
  AViewInfo.Transparent := True;
end;

procedure TCustomDrawDemoMainForm.SchedulerDateNavigatorCustomDrawDayCaption(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorDayCaptionViewInfo;
  var ADone: Boolean);
begin
  if not miDayCaptions.Checked then Exit;
  if AViewInfo.Index in [FDaysOffIndexes[1], FDaysOffIndexes[2]] then
    ACanvas.Font := csRed.Font;
end;

procedure TCustomDrawDemoMainForm.SchedulerDateNavigatorCustomDrawDayNumber(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorDayNumberViewInfo;
  var ADone: Boolean);
begin
  if not miDays.Checked then Exit;
  if AViewInfo.Selected then
    ACanvas.Brush.Color := clAppWorkSpace
  else
    if DayOfWeek(AViewInfo.Date) in [1, 7] then
      ACanvas.Font := csRed.Font;
end;

procedure TCustomDrawDemoMainForm.SchedulerDateNavigatorCustomDrawContent(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDateNavigatorMonthContentViewInfo;
  var ADone: Boolean);
var
  AColor: TColor;
  R: TRect;
begin
  if not miDNContent.Checked then Exit;
  R := AViewInfo.Bounds;
  case AViewInfo.Month of
    3..5: AColor := $D0FFD0;
    6..8: AColor := $D0D0FF;
    9..11: AColor := $D0FFFF;
  else
    AColor := $FFE7E7;
  end;
  with ACanvas do
  begin
    Brush.Color := AColor;
    FillRect(R);
    Font.Height := R.Bottom - R.Top;
    Font.Color := dxGetMiddleRGB(AColor, 0, 85);
    DrawText(IntToStr(AViewInfo.Month), R, cxAlignCenter);
  end;
  ACanvas.Font := AViewInfo.ViewParams.Font;
  AViewInfo.Transparent := True;
  ADone := True;
end;

procedure TCustomDrawDemoMainForm.SchedulerCustomDrawDayHeader(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean);
var
  ADisplayText: string;
  ABounds: TRect;
  P: Integer;
begin
  if not miHeaders.Checked then Exit;
  AViewInfo.Transparent := True;
  FillGradientRect(ACanvas, AViewInfo.Bounds, $A0A0A0, $707070, False);
  ACanvas.FrameRect(AViewInfo.Bounds, clGray);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := csItalic.Font;
  if (AViewInfo is TcxSchedulerAgendaViewDayHeaderCellViewInfo) and
    (Scheduler.ViewAgenda.DayHeaderOrientation = dhoVertical) then
  begin
    ADisplayText := AViewInfo.DisplayText;
    P := Pos(',', ADisplayText);
    ADisplayText := Copy(ADisplayText, 1, P - 1) + #13#10 + Copy(ADisplayText, P + 2, Length(ADisplayText) - P - 1);
    ABounds := cxRectInflate(AViewInfo.Bounds, -2 * cxTextOffset, -2 * cxTextOffset);
    ACanvas.DrawText(ADisplayText, ABounds, cxAlignHCenter or cxAlignTop or cxWordBreak);
  end
  else
    ACanvas.DrawText(AViewInfo.DisplayText, AViewInfo.Bounds, cxAlignCenter);
  ACanvas.Brush.Style := bsSolid;
  ADone := True;
end;

procedure TCustomDrawDemoMainForm.SchedulerViewDayCustomDrawRuler(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerTimeRulerCellViewInfo; var ADone: Boolean);
begin
  if not miTimeRuler.Checked then Exit;
  AViewInfo.Transparent := True;
  FillGradientRect(ACanvas, AViewInfo.Bounds[True], $909090, $E0E0E0, True);
end;

procedure TCustomDrawDemoMainForm.SchedulerViewDayCustomDrawContainer(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerContainerCellViewInfo; var ADone: Boolean);
begin
  if not miContainer.Checked or AViewInfo.Selected then Exit;
  AViewInfo.Transparent := True;
  FillGradientRect(ACanvas, AViewInfo.Bounds, $C0E0C0, $70A070, False);
end;

procedure TCustomDrawDemoMainForm.SchedulerCustomDrawEvent(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean);
begin
  if not miEvents.Checked then Exit;
  AViewInfo.Transparent := True;
  FillGradientRect(ACanvas, AViewInfo.Bounds, clWhite,
    AViewInfo.Color, False);
end;

procedure TCustomDrawDemoMainForm.SchedulerCustomDrawContent(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean);
begin
  if not miContent.Checked then Exit;
  AViewInfo.Transparent := True;
  if AViewInfo is TcxSchedulerAgendaViewFreeDayCellViewInfo then
    FillGradientRect(ACanvas, AViewInfo.Bounds, clWhite, AViewInfo.Color, False)
  else
    FillGradientRect(ACanvas, AViewInfo.Bounds, AViewInfo.Color, dxGetMiddleRGB(AViewInfo.Color, 0, 75), True);
end;

procedure TCustomDrawDemoMainForm.SchedulerCustomDrawResourceHeader(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean);
begin
  if not miResources.Checked then Exit;
  AViewInfo.Transparent := True;
  FillGradientRect(ACanvas, AViewInfo.Bounds, $50F0F0, $009090, False);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := csBoldItalic.Font;
  ACanvas.DrawText(AViewInfo.DisplayText, AViewInfo.Bounds, cxAlignCenter);
  ACanvas.Brush.Style := bsSolid;
  ADone := True;
end;

procedure TCustomDrawDemoMainForm.SchedulerCustomDrawGroupSeparator(
  Sender: TObject; ACanvas: TcxCanvas;
  AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean);
begin
  if not miGroupSeparator.Checked then Exit;
  FillGradientRect(ACanvas, cxRectInflate(AViewInfo.Bounds, 0, 0, 1, 0), $50F0F0, $009090, True);
  ADone := True;
end;

procedure TCustomDrawDemoMainForm.FormDestroy(Sender: TObject);
begin
  inherited;
  ABitmap.Free;
end;

procedure TCustomDrawDemoMainForm.UpdateCustomDraw(Sender: TObject);
begin
  with TMenuItem(Sender) do Checked := not Checked;
  Scheduler.LayoutChanged;
end;

end.

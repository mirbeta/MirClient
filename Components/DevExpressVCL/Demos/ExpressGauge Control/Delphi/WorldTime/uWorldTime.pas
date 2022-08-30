unit uWorldTime;

interface

uses
  Forms, StdCtrls, Classes, Controls, ExtCtrls, cxLookAndFeels, cxLookAndFeelPainters, cxGraphics, cxClasses, cxControls,
  dxGaugeCustomScale, dxGaugeQuantitativeScale, dxGaugeCircularScale, dxGaugeControl, dxGaugeDigitalScale, cxSplitter,
  cxContainer, cxEdit, cxLabel;

type
  TTimeInfo = record
    Hour: Word;
    Min: Word;
    Sec: Word;
  end;

  TfrmWorldTime = class(TForm)
    Timer1: TTimer;
    pnlClocks: TPanel;
    pnlWashingtonClock: TPanel;
    gcWashingtonTime: TdxGaugeControl;
    gsWashingtonTimeBackgroundLayer: TdxGaugeCircularScale;
    gsWashingtonTimeHourNeedle: TdxGaugeCircularScale;
    gsWashingtonTimeMinuteNeedle: TdxGaugeCircularScale;
    gsWashingtonTimeSecondNeedle: TdxGaugeCircularScale;
    pnlParisClock: TPanel;
    pnlMoscowClock: TPanel;
    gcParisTime: TdxGaugeControl;
    dxGaugeCircularScale1: TdxGaugeCircularScale;
    dxGaugeCircularScale2: TdxGaugeCircularScale;
    dxGaugeCircularScale3: TdxGaugeCircularScale;
    dxGaugeCircularScale4: TdxGaugeCircularScale;
    gcMoscowTime: TdxGaugeControl;
    dxGaugeCircularScale5: TdxGaugeCircularScale;
    dxGaugeCircularScale6: TdxGaugeCircularScale;
    dxGaugeCircularScale7: TdxGaugeCircularScale;
    dxGaugeCircularScale8: TdxGaugeCircularScale;
    gcLocalTime: TdxGaugeControl;
    gsDigital: TdxGaugeDigitalScale;
    pnlLocalTime: TPanel;
    cxSplitter1: TcxSplitter;
    cxLabel1: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel2: TcxLabel;
    lbDescription: TcxLabel;
    cxLookAndFeelController1: TcxLookAndFeelController;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure pnlClocksResize(Sender: TObject);
  protected
    function GetTimeInfo(ATimeZone: Integer): TTimeInfo;
    procedure UpdateClock(AGaugeControl: TdxGaugeControl; ATimeInfo: TTimeInfo);
    procedure UpdateClocks;
  end;

var
  frmWorldTime: TfrmWorldTime;

implementation

{$R *.dfm}

uses
  Types, Windows, SysUtils, cxGeometry, cxDateUtils;

const
  Offset = 5;

{ TfrmWorldTime }

function TfrmWorldTime.GetTimeInfo(ATimeZone: Integer): TTimeInfo;
var
  AMilli : Word;
  ASystemTime: TSystemTime;
begin
  GetSystemTime(ASystemTime);
  DecodeTime(SystemTimeToDateTime(ASystemTime), Result.Hour, Result.Min, Result.Sec, AMilli);
  Result.Hour := (Result.Hour + ATimeZone) mod 12;
end;

procedure TfrmWorldTime.UpdateClock(AGaugeControl: TdxGaugeControl; ATimeInfo: TTimeInfo);
begin
  AGaugeControl.BeginUpdate;
  TdxGaugeCircularScale(AGaugeControl.Scales[3]).Value := ATimeInfo.Min;
  TdxGaugeCircularScale(AGaugeControl.Scales[2]).Value := ATimeInfo.Hour + ATimeInfo.Min / 60;
  TdxGaugeCircularScale(AGaugeControl.Scales[1]).Value := ATimeInfo.Sec;
  AGaugeControl.EndUpdate;
end;

procedure TfrmWorldTime.UpdateClocks;
begin
  UpdateClock(gcWashingtonTime, GetTimeInfo(-4));
  UpdateClock(gcParisTime, GetTimeInfo(1));
  UpdateClock(gcMoscowTime, GetTimeInfo(3));
  gsDigital.Value := cxTimeToStr(Now, 'hh:mm:ss');
end;

procedure TfrmWorldTime.Timer1Timer(Sender: TObject);
begin
  UpdateClocks;
end;

procedure TfrmWorldTime.FormCreate(Sender: TObject);
begin
  Application.Title := 'World Time';
  frmWorldTime.Constraints.MinHeight := LbDescription.Height + pnlClocks.Constraints.MinHeight +
    cxSplitter1.Height + pnlLocalTime.Constraints.MinHeight;
  frmWorldTime.Constraints.MinWidth := gcWashingtonTime.Constraints.MinWidth + Offset * gcWashingtonTime.Constraints.MinWidth;
  UpdateClocks;
end;

procedure TfrmWorldTime.FormShow(Sender: TObject);
begin
  Caption := Application.Title;
  Application.Hint := Caption;
end;

procedure TfrmWorldTime.pnlClocksResize(Sender: TObject);
begin
  pnlWashingtonClock.Width := (pnlClocks.Width - Offset * 4) div 3;
  pnlWashingtonClock.Height := pnlClocks.Height - Offset * 3;
  pnlWashingtonClock.Left := Offset;
  pnlWashingtonClock.Top := Offset;
  pnlParisClock.BoundsRect := cxRectOffsetHorz(pnlWashingtonClock.BoundsRect, pnlWashingtonClock.Width + Offset);
  pnlMoscowClock.BoundsRect := cxRectOffsetHorz(pnlParisClock.BoundsRect, pnlParisClock.Width + Offset);
end;

end.

unit uCarDashboard;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, ExtCtrls, StdCtrls,
  Dialogs, cxClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxGDIPlusClasses, dxGaugeCustomScale,
  dxGaugeQuantitativeScale, cxButtons, dxGaugeCircularScale, dxGaugeControl, dxGaugeDigitalScale
{$IFDEF EXPRESSSKINS}
  , dxSkinMetropolisDark, dxSkinsForm
{$ENDIF};

type
  TcxButton = class(cxButtons.TcxButton)
  private
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  end;

  TdxGaugeControl = class(dxGaugeCOntrol.TdxGaugeControl)
  public
    function CanFocus: Boolean; override;
  end;

  TfrmCarDashboard = class(TForm)
    Image1: TImage;
    btnEngineStart: TcxButton;
    btnEngineStop: TcxButton;
    btnDownGear: TcxButton;
    btnNeutralGear: TcxButton;
    btnUpGear: TcxButton;
    dxGaugeControl1: TdxGaugeControl;
    gsBackground: TdxGaugeCircularScale;
    gsCentralPanel: TdxGaugeCircularScale;
    gsSpeedometr: TdxGaugeCircularScale;
    gsTachometer: TdxGaugeCircularScale;
    gsTemperature: TdxGaugeCircularScale;
    gsGear: TdxGaugeDigitalScale;
    btnAccelerate: TcxButton;
    Timer1: TTimer;
    btnFueling: TcxButton;
    gsFuelIndicator: TdxGaugeDigitalScale;
    btnBrake: TcxButton;
    gsCarStateIndicator: TdxGaugeDigitalScale;
    gsFuel: TdxGaugeCircularHalfScale;
    tDigitalScaleTimer: TTimer;
    gsDescription: TdxGaugeDigitalScale;
    gsTachometerRange: TdxGaugeCircularScaleRange;
    cxLookAndFeelController1: TcxLookAndFeelController;
    dxGaugeControl2: TdxGaugeControl;
    gsFuelCaption1: TdxGaugeQuantitativeScaleCaption;
    gsFuelCaption2: TdxGaugeQuantitativeScaleCaption;
    gsSpeedometrCaption1: TdxGaugeQuantitativeScaleCaption;
    gsTachometerCaption1: TdxGaugeQuantitativeScaleCaption;
    gsSpeedometrCaption2: TdxGaugeQuantitativeScaleCaption;
    procedure FormCreate(Sender: TObject);
    procedure btnEngineStartClick(Sender: TObject);
    procedure btnUpGearClick(Sender: TObject);
    procedure btnDownGearClick(Sender: TObject);
    procedure btnNeutralGearClick(Sender: TObject);
    procedure btnEngineStopClick(Sender: TObject);
    procedure btnAccelerateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure btnAccelerateMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnFuelingClick(Sender: TObject);
    procedure btnBrakeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnBrakeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure tDigitalScaleTimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FIndex: Integer;
    FDescriptionText: string;

    FGear: Integer;
    FGearFactors: array[0..5] of Single;

    FStarted: Boolean;
    FIsStarting: Boolean;
    FIsBreaking: Boolean;

    FFuelValueDelta: Single;
    FTachometerValueDelta: Single;
    FTemperatureValueDelta: Single;

    {$IFDEF EXPRESSSKINS}
    FSkinController: TcxLookAndFeelController;
    {$ENDIF}

    function GetTachometerValue: Single;
    procedure AccelerateDown;
    procedure AccelerateUp;
    procedure BreakDown;
    procedure BreakUp;
    procedure GearDown;
    procedure GearUp;
    procedure SetGear(ADelta: Integer);
    procedure Start;
    procedure Stop;
    procedure UpdateIndicators;
    procedure UpdateButtonStates;
    procedure UpdateGear;
    procedure UpdateFuelLevel;
    procedure UpdateSpeed;
    procedure UpdateTachometer;
    procedure UpdateTemperature;
    procedure PrepareDesription;
  end;

var
  frmCarDashboard: TfrmCarDashboard;

implementation

uses
  Math, dxCore, dxCoreGraphics;

{$R *.dfm}

const
  cAccelerateTachometerValueDelta = 0.15;
  cBreakTachometerValueDelta = -0.03;
  cBreakSpeedometerValueDelta = -4;
  cSpeedometerValueDelta = -0.5;
  cFuelValueDelta = -0.05;
  cStartingTachometerValueDelta = 0.05;
  cTemperatureValueDelta = 0.1;

{ TcxButton }

procedure TcxButton.CNKeyDown(var Message: TWMKeyDown);
begin
  Message.Result := 0;
end;

{ TdxGaugeControl }

function TdxGaugeControl.CanFocus: Boolean;
begin
  Result := False;
end;

{ TForm1 }

procedure TfrmCarDashboard.FormCreate(Sender: TObject);
begin
  gsFuel.Value := gsFuel.OptionsView.MaxValue;
  gsFuelIndicator.OptionsView.SegmentColorOn := clNone;
  gsCarStateIndicator.OptionsView.SegmentColorOn := clNone;

  FGearFactors[0] := 0;
  FGearFactors[1] := 3.636;
  FGearFactors[2] := 1.95;
  FGearFactors[3] := 1.357;
  FGearFactors[4] := 0.941;
  FGearFactors[5] := 0.784;

{$IFDEF EXPRESSSKINS}
  FSkinController := TdxSkinController.Create(Self);
  FSkinController.NativeStyle := False;
  FSkinController.SkinName := 'MetropolisDark';
{$ENDIF}
  UpdateButtonStates;
  PrepareDesription;
end;

procedure TfrmCarDashboard.btnEngineStartClick(Sender: TObject);
begin
  Start;
end;

function TfrmCarDashboard.GetTachometerValue: Single;
begin
  Result := gsSpeedometr.Value / (0.377 * 0.28 / 3.7 / FGearFactors[FGear]) / 1000 + 0.8;
end;

procedure TfrmCarDashboard.AccelerateUp;
begin
  if FStarted then
  begin
    FTachometerValueDelta := cAccelerateTachometerValueDelta;
    FFuelValueDelta := cFuelValueDelta;
  end;
end;

procedure TfrmCarDashboard.AccelerateDown;
begin
  FTachometerValueDelta := cBreakTachometerValueDelta;
  FFuelValueDelta := 0;
end;

procedure TfrmCarDashboard.BreakUp;
begin
  FIsBreaking := True;
  if FGear > 0 then
    FTachometerValueDelta := cBreakTachometerValueDelta * 2;
end;

procedure TfrmCarDashboard.GearDown;
begin
  SetGear(-1);
  if FGear > 0 then
    gsTachometer.Value := GetTachometerValue;
end;

procedure TfrmCarDashboard.GearUp;
begin
  SetGear(1);
  if (FGear > 0) and (gsTachometer.Value < 1) then
    Stop;
  if FGear > 1 then
    gsTachometer.Value := GetTachometerValue;
end;

procedure TfrmCarDashboard.BreakDown;
begin
  FIsBreaking := False;
  FTachometerValueDelta := cBreakTachometerValueDelta;
  if (FGear > 0) and (gsTachometer.Value < 1) then
    Stop;
end;

procedure TfrmCarDashboard.SetGear(ADelta: Integer);
begin
  FGear := Max(Min(FGear + ADelta, 5), 0);
end;

procedure TfrmCarDashboard.Start;
begin
  FStarted := True;
  FIsStarting := True;
  FGear := 0;
  FFuelValueDelta := 0;
  FTachometerValueDelta := cStartingTachometerValueDelta;
  FTemperatureValueDelta := cTemperatureValueDelta;
  Timer1.Enabled := FStarted;
end;

procedure TfrmCarDashboard.Stop;
begin
  FStarted := False;
  FGear := 0;
  FFuelValueDelta := 0;
  FTemperatureValueDelta := -FTemperatureValueDelta;
  FTachometerValueDelta := -0.1;
end;

procedure TfrmCarDashboard.UpdateIndicators;
begin
  if FStarted then
    gsCarStateIndicator.OptionsView.SegmentColorOn := dxColorToAlphaColor(clGreen)
  else
    gsCarStateIndicator.OptionsView.SegmentColorOn := clNone;

  if gsFuel.Value = 0 then
    gsFuelIndicator.OptionsView.SegmentColorOn := dxColorToAlphaColor(clRed);
end;

procedure TfrmCarDashboard.UpdateButtonStates;
begin
  btnAccelerate.Enabled := FStarted;
  btnBrake.Enabled := FStarted;  
  btnEngineStop.Enabled := FStarted;
  btnEngineStart.Enabled := not FStarted and (gsFuel.Value > 0);
  btnUpGear.Enabled := FStarted and (FGear < 5);
  btnDownGear.Enabled := FStarted and (FGear > 0);
  btnNeutralGear.Enabled := FStarted and (FGear > 0);
end;

procedure TfrmCarDashboard.UpdateGear;
begin
  if FGear = 0 then
    gsGear.Value := 'N'
  else
    gsGear.Value := IntToStr(FGear);
end;

procedure TfrmCarDashboard.UpdateFuelLevel;
begin
  gsFuel.Value := gsFuel.Value + FFuelValueDelta;
  if gsFuel.Value = 0 then
    Stop;
end;

procedure TfrmCarDashboard.UpdateSpeed;
begin
  if FGear > 0 then
    gsSpeedometr.Value := 0.377 * 0.28 / 3.7 / FGearFactors[FGear] * (gsTachometer.Value - 0.8) * 1000
  else
    if FIsBreaking then
      gsSpeedometr.Value := gsSpeedometr.Value + cBreakSpeedometerValueDelta
    else
      gsSpeedometr.Value := gsSpeedometr.Value + cSpeedometerValueDelta;
end;

procedure TfrmCarDashboard.UpdateTachometer;
var
  AValue: Single;
begin
  AValue := gsTachometer.Value + FTachometerValueDelta;
  if ((gsTachometer.Value >= gsTachometer.OptionsView.MaxValue) or (gsTachometer.Value < 0.8)) and FStarted and
    not FIsStarting then
    Stop
  else
  begin
    if FStarted and (not FIsStarting and (AValue < 0.8)) or (FIsStarting and (AValue > 0.8)) then
    begin
      AValue := 0.8;
      FTachometerValueDelta := 0;
      FIsStarting := not FIsStarting;
    end;
    gsTachometer.Value := AValue;
  end;
end;

procedure TfrmCarDashboard.UpdateTemperature;
begin
  gsTemperature.Value := Max(Min(gsTemperature.Value + FTemperatureValueDelta, 90), 0);
end;

procedure TfrmCarDashboard.PrepareDesription;
const
  sdxDescriptionText = 'This is a typical car dashboard, created with Express Gauge Control';
var
  I: Integer;
begin
  for I := 0 to gsDescription.OptionsView.DigitCount do
    FDescriptionText := FDescriptionText + ' ';
  FDescriptionText := FDescriptionText + sdxDescriptionText;
  for I := 0 to gsDescription.OptionsView.DigitCount do
    FDescriptionText := FDescriptionText + ' ';
end;

procedure TfrmCarDashboard.btnUpGearClick(Sender: TObject);
begin
  GearUp;
end;

procedure TfrmCarDashboard.btnDownGearClick(Sender: TObject);
begin
  GearDown;
end;

procedure TfrmCarDashboard.btnNeutralGearClick(Sender: TObject);
begin
  SetGear(-FGear);
end;

procedure TfrmCarDashboard.btnEngineStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TfrmCarDashboard.Timer1Timer(Sender: TObject);
begin
  UpdateGear;
  UpdateTachometer;
  UpdateSpeed;
  UpdateFuelLevel;
  UpdateTemperature;
  UpdateIndicators;
  UpdateButtonStates;
  if (gsTachometer.Value < 1) and (FGear > 0) then
    Stop;
end;

procedure TfrmCarDashboard.tDigitalScaleTimerTimer(Sender: TObject);
begin
  gsDescription.Value := copy(FDescriptionText, FIndex, gsDescription.OptionsView.DigitCount);
  FIndex := (FIndex + 1) mod Length(FDescriptionText);
end;

procedure TfrmCarDashboard.btnAccelerateMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AccelerateUp;
end;

procedure TfrmCarDashboard.btnAccelerateMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AccelerateDown;
end;

procedure TfrmCarDashboard.btnFuelingClick(Sender: TObject);
begin
  FFuelValueDelta := 2;
  gsFuelIndicator.OptionsView.SegmentColorOn := clNone;
end;

procedure TfrmCarDashboard.btnBrakeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BreakUp;
end;

procedure TfrmCarDashboard.btnBrakeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BreakDown;
end;

procedure TfrmCarDashboard.FormDestroy(Sender: TObject);
begin
{$IFDEF EXPRESSSKINS}
  FSkinController.Free;
{$ENDIF}
end;

procedure TfrmCarDashboard.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      AccelerateUp;
    VK_DOWN:
      BreakUp;
    VK_Shift:
      GearUp;
    VK_CONTROL:
      GearDown;
    VK_SPACE:
      if FStarted then
        Stop
      else
        Start;
  end;

end;

procedure TfrmCarDashboard.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      AccelerateDown;
    VK_DOWN:
      BreakDown;
  end;
end;

end.


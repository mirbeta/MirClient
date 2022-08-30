unit uSampleGauges;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, dxGaugeCustomScale, dxGaugeDigitalScale, dxGaugeControl,
  dxGaugeQuantitativeScale, dxGaugeCircularScale, dxLayoutContainer,
  dxLayoutControl, ComCtrls, StdCtrls, cxContainer, cxEdit, cxLabel, dxGaugeLinearScale, ExtCtrls;

type
  TfrmSampleGauges = class(TForm)
    gcCircularCleanWhite: TdxGaugeControl;
    dxGaugeCircularScale1: TdxGaugeCircularScale;
    gcCircularDeepFire: TdxGaugeControl;
    dxGaugeCircularScale3: TdxGaugeCircularScale;
    dxGaugeControl6dxGaugeCircularScale1: TdxGaugeCircularScale;
    gcCircularWhite: TdxGaugeControl;
    dxGaugeCircularScale4: TdxGaugeCircularScale;
    PageControl1: TPageControl;
    tsCircularGauges: TTabSheet;
    tsDigital: TTabSheet;
    gcDigitalIceColdZone: TdxGaugeControl;
    dxGaugeDigitalScale3: TdxGaugeDigitalScale;
    gcDigitalDeepFire: TdxGaugeControl;
    dxGaugeDigitalScale5: TdxGaugeDigitalScale;
    gcDigitalWhite: TdxGaugeControl;
    dxGaugeDigitalScale6: TdxGaugeDigitalScale;
    tsLinearGauges: TTabSheet;
    gcDigitalScaleText: TdxGaugeControl;
    dxGaugeDigitalScale7: TdxGaugeDigitalScale;
    gcLinearSmart: TdxGaugeControl;
    dxGaugeLinearScale1: TdxGaugeLinearScale;
    gcLinearYellowSubmarine: TdxGaugeControl;
    dxGaugeLinearScale2: TdxGaugeLinearScale;
    dxGaugeControl3LinearScale1: TdxGaugeLinearScale;
    dxGaugeControl2LinearScale1: TdxGaugeLinearScale;
    lbDescription: TcxLabel;
    gcLinearIceColdZone: TdxGaugeControl;
    dxGaugeControl1LinearScale1: TdxGaugeLinearScale;
    tsCircularHalfGauges: TTabSheet;
    gcCircularHalfYellowSubmarine: TdxGaugeControl;
    dxGaugeControl4CircularHalfScale1: TdxGaugeCircularHalfScale;
    gcCircularHalfClassic: TdxGaugeControl;
    dxGaugeCircularHalfScale1: TdxGaugeCircularHalfScale;
    gcCircularHalfCleanWhite: TdxGaugeControl;
    dxGaugeCircularHalfScale2: TdxGaugeCircularHalfScale;
    dxGaugeCircularScale4Range: TdxGaugeCircularScaleRange;
    dxGaugeCircularScale4Range1: TdxGaugeCircularScaleRange;
    dxGaugeCircularScale4Range2: TdxGaugeCircularScaleRange;
    dxGaugeControl2LinearScale1Range: TdxGaugeLinearScaleRange;
    dxGaugeControl2LinearScale1Range1: TdxGaugeLinearScaleRange;
    dxGaugeCircularHalfScale2Range: TdxGaugeCircularScaleRange;
    dxGaugeCircularHalfScale2Range1: TdxGaugeCircularScaleRange;
    dxGaugeCircularHalfScale2Range2: TdxGaugeCircularScaleRange;
    dxGaugeControl4CircularHalfScale2: TdxGaugeCircularHalfScale;
    tsCircularQuarterGauges: TTabSheet;
    gcCircularQuarterYellowSubmarine: TdxGaugeControl;
    gcCircularQuarterDeepFire: TdxGaugeControl;
    gcCircularQuarterSmart: TdxGaugeControl;
    dxGaugeControl7CircularQuarterLeftScale1: TdxGaugeCircularQuarterLeftScale;
    dxGaugeControl9CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale;
    dxGaugeControl8CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale;
    dxGaugeControl8CircularQuarterRightScale1Range: TdxGaugeCircularScaleRange;
    dxGaugeControl8CircularQuarterRightScale1Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl8CircularQuarterRightScale1Range2: TdxGaugeCircularScaleRange;
    tsHybridGauges: TTabSheet;
    pcHybridGauges: TPageControl;
    tsCarTester: TTabSheet;
    tsTemperatureGauges: TTabSheet;
    gcTester: TdxGaugeControl;
    gsMainScaleBackground: TdxGaugeCircularScale;
    gsColoredScaleBackground: TdxGaugeCircularScale;
    gsTesterValue4: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale3Range: TdxGaugeCircularScaleRange;
    dxGaugeControl3CircularScale3Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl3CircularScale3Range2: TdxGaugeCircularScaleRange;
    gsTesterValue5: TdxGaugeDigitalScale;
    gsTesterValue6: TdxGaugeDigitalScale;
    gsTesterValue7: TdxGaugeDigitalScale;
    gsTesterValue3: TdxGaugeCircularScale;
    gsScaleTicks2: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale5Range: TdxGaugeCircularScaleRange;
    gsScaleTicks1: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale6Range: TdxGaugeCircularScaleRange;
    gsMainBackground: TdxGaugeLinearScale;
    gsBackground: TdxGaugeDigitalScale;
    gsTesterValue1: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale7Range: TdxGaugeCircularScaleRange;
    gsTesterValue2: TdxGaugeCircularScale;
    dxGaugeControl1: TdxGaugeControl;
    gsPressureValue: TdxGaugeCircularScale;
    gsVolumeValue: TdxGaugeLinearScale;
    gsVolumeValueSecondary: TdxGaugeLinearScale;
    dxGaugeControl1LinearScale2Range: TdxGaugeLinearScaleRange;
    dxGaugeControl1LinearScale2Range1: TdxGaugeLinearScaleRange;
    dxGaugeControl1LinearScale2Range2: TdxGaugeLinearScaleRange;
    tsSimpleHybrid: TTabSheet;
    gcHybridDarkNight: TdxGaugeControl;
    dxGaugeCircularScale2: TdxGaugeCircularScale;
    dxGaugeDigitalScale4: TdxGaugeDigitalScale;
    gcHybrid: TdxGaugeControl;
    dxGaugeCircularScale5: TdxGaugeCircularScale;
    dxGaugeControl11dxGaugeDigitalScale1: TdxGaugeDigitalScale;
    gcHybridIceColdZone: TdxGaugeControl;
    dxGaugeControl3dxGaugeCircularScale1: TdxGaugeCircularScale;
    dxGaugeDigitalScale2: TdxGaugeDigitalScale;
    gcHybridIceColdZoneCircularScale1: TdxGaugeCircularScale;
    tCarTesterTimer: TTimer;
    tTemperatureTimer: TTimer;
    cxLookAndFeelController1: TcxLookAndFeelController;
    tsCircularWide: TTabSheet;
    gcCircularWideMechanical: TdxGaugeControl;
    gcCircularWideWhite: TdxGaugeControl;
    gcCircularWideDeepFire: TdxGaugeControl;
    Scale3: TdxGaugeCircularWideScale;
    Scale2: TdxGaugeCircularWideScale;
    Scale1: TdxGaugeCircularWideScale;
    gcHybridIceColdZoneCircularScale1Caption1: TdxGaugeQuantitativeScaleCaption;
    gsTesterValue3Caption1: TdxGaugeQuantitativeScaleCaption;
    gsTesterValue1Caption1: TdxGaugeQuantitativeScaleCaption;
    gsTesterValue1Caption2: TdxGaugeQuantitativeScaleCaption;
    dxGaugeCircularScale4Caption1: TdxGaugeQuantitativeScaleCaption;
    dxGaugeCircularScale4Caption2: TdxGaugeQuantitativeScaleCaption;
    dxGaugeCircularScale3Caption1: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl1LinearScale1Caption1: TdxGaugeQuantitativeScaleCaption;
    gsTesterValue1Caption3: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl6dxGaugeCircularScale1Caption1: TdxGaugeQuantitativeScaleCaption;
    dxGaugeCircularScale3Caption2: TdxGaugeQuantitativeScaleCaption;
    tsCircularThreeFourth: TTabSheet;
    gcCircularThreeFourthAfrica: TdxGaugeControl;
    gcCircularThreeFourthFuture: TdxGaugeControl;
    gcCircularThreeFourthDisco: TdxGaugeControl;
    dxGaugeControl5CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale;
    dxGaugeControl6CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale;
    dxGaugeControl7CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale;
    gcHybridContainerScale1: TdxGaugeContainerScale;
    gcHybridLinearScale1: TdxGaugeLinearScale;
    gcHybridCircularWideScale1: TdxGaugeCircularWideScale;
    dxGaugeControl1ContainerScale1: TdxGaugeContainerScale;
    gsPressureValueCaption1: TdxGaugeQuantitativeScaleCaption;
    gsPressureValueRange1: TdxGaugeCircularScaleRange;
    gsPressureValueRange2: TdxGaugeCircularScaleRange;
    gsPressureValueCaption2: TdxGaugeQuantitativeScaleCaption;
    gcHybridLinearScale1Caption1: TdxGaugeQuantitativeScaleCaption;
    gsVolumeValueSecondaryCaption1: TdxGaugeQuantitativeScaleCaption;
    gsVolumeValueCaption1: TdxGaugeQuantitativeScaleCaption;
    gcDigitalClassic: TdxGaugeControl;
    dxGaugeDigitalScale1: TdxGaugeDigitalScale;
    gcDigitalFuture: TdxGaugeControl;
    dxGaugeDigitalScale8: TdxGaugeDigitalScale;
    gcCircularWideSportCar: TdxGaugeControl;
    dxGaugeCircularWideScale1: TdxGaugeCircularWideScale;
    gcCircularWideAfrica: TdxGaugeControl;
    dxGaugeCircularWideScale2: TdxGaugeCircularWideScale;
    gcCircularWideDarkNight: TdxGaugeControl;
    dxGaugeCircularWideScale3: TdxGaugeCircularWideScale;
    TabSheet1: TTabSheet;
    dxGaugeControl2: TdxGaugeControl;
    gcsHazeGas: TdxGaugeCircularScale;
    dxGaugeControl2CircularScale1Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl2CircularScale1Range2: TdxGaugeCircularScaleRange;
    gcsHazeGasCaptionValue: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2ContainerScale1: TdxGaugeContainerScale;
    gcsHazeCaption1: TdxGaugeQuantitativeScaleCaption;
    gcsHazeColdWater: TdxGaugeCircularScale;
    gcsHazeColdWaterCaptionValue: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale1Caption3: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale1Range3: TdxGaugeCircularScaleRange;
    dxGaugeControl2CircularScale1Range4: TdxGaugeCircularScaleRange;
    gcsHazeHotWater: TdxGaugeCircularScale;
    gcsHazeHotWaterCaptionValue: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale2Caption2: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale2Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl2CircularScale2Range2: TdxGaugeCircularScaleRange;
    gcsHazeElectricity: TdxGaugeCircularScale;
    gcsHazeElectricityCaptionValue: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale3Caption2: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl2CircularScale3Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl2CircularScale3Range2: TdxGaugeCircularScaleRange;
    tHazesTimer: TTimer;
    tsWeather: TTabSheet;
    gcWeather: TdxGaugeControl;
    dxGaugeControl3ContainerScale1: TdxGaugeContainerScale;
    gcsWeatherLosAnglesHumidity: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale1Range1: TdxGaugeCircularScaleRange;
    dxGaugeControl3CircularScale1Range2: TdxGaugeCircularScaleRange;
    gcsWeatherLosAnglesTemperature: TdxGaugeCircularScale;
    gcsWeatherLosAnglesTemperatureCaption: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale3Caption3: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale3Range3: TdxGaugeCircularScaleRange;
    gcsWeatherLosAnglesTemperatureRange: TdxGaugeCircularScaleRange;
    gcsWeatherMoscowHumidity: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale4Range1: TdxGaugeCircularScaleRange;
    gcsWeatherMowcowHumidityRange: TdxGaugeCircularScaleRange;
    gcsWeatherMoscowTemperature: TdxGaugeCircularScale;
    gcsWeatherMoscowTemperatureCaption: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale2Caption3: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale2Range1: TdxGaugeCircularScaleRange;
    gcsWeatherMoscowTemperatureRange: TdxGaugeCircularScaleRange;
    gcsWeatherLondonHumidity: TdxGaugeCircularScale;
    dxGaugeControl3CircularScale5Range1: TdxGaugeCircularScaleRange;
    gcsWeatherLondonHumidityRange: TdxGaugeCircularScaleRange;
    gcsWeatherLondonTemperature: TdxGaugeCircularScale;
    gcsWeatherLondonTemperatureCaption: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale6Caption3: TdxGaugeQuantitativeScaleCaption;
    dxGaugeControl3CircularScale6Range1: TdxGaugeCircularScaleRange;
    gcsWeatherLondonTemperatureRange: TdxGaugeCircularScaleRange;
    gcsWeatherLosAnglesHumidityCaption: TdxGaugeQuantitativeScaleCaption;
    tWeatherTimer: TTimer;
    gcsWeatherMoscowHumidityCaption: TdxGaugeQuantitativeScaleCaption;
    gcsWeatherLondonHumidityCaption: TdxGaugeQuantitativeScaleCaption;
    gcsWeatherDate: TdxGaugeQuantitativeScaleCaption;
    procedure tsCircularGaugesResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Resize(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure tCarTesterTimerTimer(Sender: TObject);
    procedure pcHybridGaugesChange(Sender: TObject);
    procedure tTemperatureTimerTimer(Sender: TObject);
    procedure tHazesTimerTimer(Sender: TObject);
    procedure gcsHazeGasAnimate(Sender: TObject);
    procedure gcsHazeColdWaterAnimate(Sender: TObject);
    procedure gcsHazeHotWaterAnimate(Sender: TObject);
    procedure gcsHazeElectricityAnimate(Sender: TObject);
    procedure tWeatherTimerTimer(Sender: TObject);
    procedure gcsWeatherLosAnglesHumidityAnimate(Sender: TObject);
    procedure gcsWeatherLosAnglesTemperatureAnimate(Sender: TObject);
    procedure gcsWeatherMoscowHumidityAnimate(Sender: TObject);
    procedure gcsWeatherMoscowTemperatureAnimate(Sender: TObject);
    procedure gcsWeatherLondonTemperatureAnimate(Sender: TObject);
    procedure gcsWeatherLondonHumidityAnimate(Sender: TObject);
  private
    FDate: TDateTime;
    procedure LoadCustomStyles;
    procedure UpdateCircularGauges;
    procedure UpdateCircularHalfGauges;
    procedure UpdateCircularQuarterGauges;
    procedure UpdateCircularThreeFouthGauges;
    procedure UpdateCircularWideGauges;
    procedure UpdateDigitalGauges;
    procedure UpdateLinearGauges;
    procedure UpdateLondonWeather;
    procedure UpdateLosAngeles;
    procedure UpdateMoscowWeather;
    procedure UpdateSityWeather(ATemperatureScale, AHumidityScale: TdxGaugeCircularScale; ATemperature, AHumidity: Single);
    procedure UpdateSityTemperature(AValue: Single; ACaption: TdxGaugeQuantitativeScaleCaption;
      ARange: TdxGaugeCircularScaleRange);
    procedure UpdateHybridGauges;
    procedure UpdateTimers;
    procedure UpdateGauges;
    procedure UpdateWeatherDate;
  public
    { Public declarations }
  end;

var
  frmSampleGauges: TfrmSampleGauges;

implementation

uses
  Types, DateUtils, Math, cxGeometry, dxCore, dxCoreGraphics;
{$R *.dfm}

const
  Offset = 5;

function GetRandomValueDelta(AFrom, ATo, ARange: Integer): Single;
begin
  Randomize;
  Result := Sign(RandomFrom([-1, 1])) * RandomRange(AFrom, ATo) / ARange;
end;

function GetRandomValue(AFrom, ATo, ARange: Integer): Single;
begin
  Randomize;
  Result := RandomRange(AFrom, ATo) / ARange;
end;

procedure TfrmSampleGauges.tCarTesterTimerTimer(Sender: TObject);
var
  AFormatSettings: TFormatSettings;
begin
  gcTester.BeginUpdate;
  gsTesterValue1.Value := gsTesterValue1.Value + GetRandomValueDelta(1, 5, 100);
  gsTesterValue2.Value := gsTesterValue2.Value + GetRandomValueDelta(1, 20, 10);
  gsTesterValue3.Value := gsTesterValue3.Value + GetRandomValueDelta(1, 4, 10);
  gsTesterValue4.Value := gsTesterValue4.Value + GetRandomValueDelta(1, 3, 1);
  AFormatSettings.DecimalSeparator := ',';
  gsTesterValue5.Value := FormatFloat('000.0',
    Min(Max(dxStrToFloat(gsTesterValue5.Value, ',') + GetRandomValueDelta(1, 5, 10), 0), 100), AFormatSettings);
  gsTesterValue6.Value := FormatFloat('000', Min(Max(dxStrToFloat(gsTesterValue6.Value, ',') + GetRandomValueDelta(-10,
          10, 1), -99), 999), AFormatSettings);
  gsTesterValue7.Value := IntToStr(StrToInt(gsTesterValue7.Value) + Round(GetRandomValueDelta(-1, 1, 1)));
  gcTester.EndUpdate;
end;

procedure TfrmSampleGauges.tHazesTimerTimer(Sender: TObject);
begin
  gcsHazeGas.Value := Max(gcsHazeGas.Value + GetRandomValueDelta(1, 30, 1), 10);
  gcsHazeColdWater.Value := Max(gcsHazeColdWater.Value + GetRandomValueDelta(1, 30, 1), 10);
  gcsHazeHotWater.Value := Max(gcsHazeHotWater.Value + GetRandomValueDelta(1, 30, 1), 10);
  gcsHazeElectricity.Value := Max(gcsHazeElectricity.Value + GetRandomValueDelta(1, 30, 1), 10);
end;

procedure TfrmSampleGauges.tsCircularGaugesResize(Sender: TObject);
begin
  UpdateGauges;
end;

procedure TfrmSampleGauges.tTemperatureTimerTimer(Sender: TObject);
begin
  gsVolumeValue.Value := gsVolumeValue.Value + GetRandomValueDelta(1, 150, 1);
  gsPressureValue.Value := Min(Max(gsPressureValue.Value + GetRandomValueDelta(1, 1, 1), 0.5), 6.5);
end;

procedure TfrmSampleGauges.UpdateLondonWeather;
var
  ATemperature: Single;
  AHumidity: Single;
begin
  ATemperature := 10 * Sin(((DayOfTheYear(FDate) * Pi) / 90) / 2 - ((91 * Pi) / 180)) + GetRandomValue(-5, 2, 1) + 13;
  AHumidity := GetRandomValue(65, 100, 1);
  UpdateSityWeather(gcsWeatherLondonTemperature, gcsWeatherLondonHumidity, ATemperature, AHumidity);
end;

procedure TfrmSampleGauges.UpdateLosAngeles;
var
  ATemperature: Single;
  AHumidity: Single;
begin
  ATemperature := 7.5 * Sin(((DayOfTheYear(FDate) * Pi) / 90) / 2 - ((91 * Pi) / 180)) + GetRandomValue(-2, 2, 1) + 20.5;
  AHumidity := GetRandomValue(40, 92, 1);
  UpdateSityWeather(gcsWeatherLosAnglesTemperature, gcsWeatherLosAnglesHumidity, ATemperature, AHumidity);
end;

procedure TfrmSampleGauges.UpdateMoscowWeather;
var
  ATemperature: Single;
  AHumidity: Single;
begin
  ATemperature := 28 * Sin(((DayOfTheYear(FDate) * Pi) / 90) / 2 - ((91 * Pi) / 180)) + GetRandomValue(0, 2, 1);
  AHumidity := GetRandomValue(60, 100, 1);
  UpdateSityWeather(gcsWeatherMoscowTemperature, gcsWeatherMoscowHumidity, ATemperature, AHumidity);
end;

procedure TfrmSampleGauges.UpdateSityWeather(ATemperatureScale, AHumidityScale: TdxGaugeCircularScale;
  ATemperature, AHumidity: Single);
begin
  ATemperatureScale.Value := ATemperature;
  AHumidityScale.Value := AHumidity;
end;

procedure TfrmSampleGauges.tWeatherTimerTimer(Sender: TObject);
begin
  UpdateWeatherDate;
  UpdateLosAngeles;
  UpdateMoscowWeather;
  UpdateLondonWeather;
end;

procedure TfrmSampleGauges.gcsHazeColdWaterAnimate(Sender: TObject);
begin
  gcsHazeColdWaterCaptionValue.Text := IntToStr(Round(gcsHazeColdWater.Value));
end;

procedure TfrmSampleGauges.gcsHazeElectricityAnimate(Sender: TObject);
begin
  gcsHazeElectricityCaptionValue.Text := IntToStr(Round(gcsHazeElectricity.Value));
end;

procedure TfrmSampleGauges.gcsHazeGasAnimate(Sender: TObject);
begin
  gcsHazeGasCaptionValue.Text := IntToStr(Round(gcsHazeGas.Value));
end;

procedure TfrmSampleGauges.gcsHazeHotWaterAnimate(Sender: TObject);
begin
  gcsHazeHotWaterCaptionValue.Text := IntToStr(Round(gcsHazeHotWater.Value));
end;

procedure TfrmSampleGauges.gcsWeatherLondonHumidityAnimate(Sender: TObject);
begin
  gcsWeatherLondonHumidityCaption.Text := 'h: ' + IntToStr(Round(gcsWeatherLondonHumidity.Value)) + '%';
end;

procedure TfrmSampleGauges.UpdateSityTemperature(AValue: Single; ACaption: TdxGaugeQuantitativeScaleCaption;
  ARange: TdxGaugeCircularScaleRange);
const
  ColorMap: array[Boolean] of TColor = (clBlue, clRed);
var
  ATemperature: Integer;
begin
  ATemperature := Round(AValue);
  ACaption.Text := 't: ' + IntToStr(ATemperature) + ' ' + Chr(176) + 'C';
  ACaption.OptionsView.Font.Color := ColorMap[ATemperature >= 0];
  ARange.Color := dxColorToAlphaColor(ColorMap[ATemperature >= 0]);
end;

procedure TfrmSampleGauges.gcsWeatherLondonTemperatureAnimate(Sender: TObject);
begin
  gcWeather.BeginUpdate;
  UpdateSityTemperature(gcsWeatherLondonTemperature.Value, gcsWeatherLondonTemperatureCaption, gcsWeatherLondonTemperatureRange);
  gcWeather.EndUpdate;
end;

procedure TfrmSampleGauges.gcsWeatherLosAnglesHumidityAnimate(Sender: TObject);
begin
  gcsWeatherLosAnglesHumidityCaption.Text := 'h: ' + IntToStr(Round(gcsWeatherLosAnglesHumidity.Value)) + '%';
end;

procedure TfrmSampleGauges.gcsWeatherLosAnglesTemperatureAnimate(Sender: TObject);
begin
  gcWeather.BeginUpdate;
  UpdateSityTemperature(gcsWeatherLosAnglesTemperature.Value, gcsWeatherLosAnglesTemperatureCaption, gcsWeatherLosAnglesTemperatureRange);
  gcWeather.EndUpdate;
end;

procedure TfrmSampleGauges.gcsWeatherMoscowHumidityAnimate(Sender: TObject);
begin
  gcsWeatherMoscowHumidityCaption.Text := 'h: ' + IntToStr(Round(gcsWeatherMoscowHumidity.Value)) + '%';
end;

procedure TfrmSampleGauges.gcsWeatherMoscowTemperatureAnimate(Sender: TObject);
begin
  gcWeather.BeginUpdate;
  UpdateSityTemperature(gcsWeatherMoscowTemperature.Value, gcsWeatherMoscowTemperatureCaption, gcsWeatherMoscowTemperatureRange);
  gcWeather.EndUpdate;
end;

procedure TfrmSampleGauges.LoadCustomStyles;
begin
  dxGaugeRegisterStyleFromFile(stCircularScale, 'RedNeedle.xml');
  dxGaugeRegisterStyleFromFile(stCircularScale, 'GreenNeedle.xml');
  dxGaugeRegisterStyleFromFile(stLinearScale, 'CustomLinearScaleStyle.xml');
  gsMainBackground.OptionsView.ShowBackground := True;
  gsMainBackground.StyleName := 'CustomLinearScaleStyle';
  gsTesterValue1.StyleName := 'GreenNeedle';
  gsTesterValue2.StyleName := 'RedNeedle';
end;

procedure TfrmSampleGauges.UpdateCircularGauges;
begin
  gcCircularDeepFire.BeginUpdate;
  gcCircularDeepFire.Width := (tsCircularGauges.Width - Offset * 4) div 3;
  gcCircularDeepFire.Height := tsCircularGauges.Height - Offset * 3;
  gcCircularDeepFire.Left := Offset;
  gcCircularDeepFire.Top := Offset;
  gcCircularDeepFire.EndUpdate;
  gcCircularWhite.BoundsRect := cxRectOffsetHorz(gcCircularDeepFire.BoundsRect, gcCircularDeepFire.Width + Offset);
  gcCircularCleanWhite.BoundsRect := cxRectOffsetHorz(gcCircularWhite.BoundsRect, gcCircularDeepFire.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateCircularHalfGauges;
begin
  gcCircularHalfYellowSubmarine.BeginUpdate;
  gcCircularHalfYellowSubmarine.Width := (tsCircularHalfGauges.Width - Offset * 4) div 3;
  gcCircularHalfYellowSubmarine.Height := tsCircularHalfGauges.Height - Offset * 3;
  gcCircularHalfYellowSubmarine.Left := Offset;
  gcCircularHalfYellowSubmarine.Top := Offset;
  gcCircularHalfYellowSubmarine.EndUpdate;
  gcCircularHalfClassic.BoundsRect := cxRectOffsetHorz(gcCircularHalfYellowSubmarine.BoundsRect,
    gcCircularHalfYellowSubmarine.Width + Offset);
  gcCircularHalfCleanWhite.BoundsRect := cxRectOffsetHorz(gcCircularHalfClassic.BoundsRect,
    gcCircularHalfClassic.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateCircularQuarterGauges;
begin
  gcCircularQuarterYellowSubmarine.BeginUpdate;
  gcCircularQuarterYellowSubmarine.Width := (tsCircularQuarterGauges.Width - Offset * 4) div 3;
  gcCircularQuarterYellowSubmarine.Height := tsCircularQuarterGauges.Height - Offset * 3;
  gcCircularQuarterYellowSubmarine.Left := Offset;
  gcCircularQuarterYellowSubmarine.Top := Offset;
  gcCircularQuarterYellowSubmarine.EndUpdate;
  gcCircularQuarterDeepFire.BoundsRect := cxRectOffsetHorz(gcCircularQuarterYellowSubmarine.BoundsRect,
    gcCircularQuarterYellowSubmarine.Width + Offset);
  gcCircularQuarterSmart.BoundsRect := cxRectOffsetHorz(gcCircularQuarterDeepFire.BoundsRect,
    gcCircularQuarterDeepFire.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateCircularThreeFouthGauges;
begin
  gcCircularThreeFourthAfrica.BeginUpdate;
  gcCircularThreeFourthAfrica.Width := (tsCircularThreeFourth.Width - Offset * 4) div 3;
  gcCircularThreeFourthAfrica.Height := tsCircularThreeFourth.Height - Offset * 3;
  gcCircularThreeFourthAfrica.Left := Offset;
  gcCircularThreeFourthAfrica.Top := Offset;
  gcCircularThreeFourthAfrica.EndUpdate;
  gcCircularThreeFourthFuture.BoundsRect := cxRectOffsetHorz(gcCircularThreeFourthAfrica.BoundsRect,
    gcCircularThreeFourthAfrica.Width + Offset);
  gcCircularThreeFourthDisco.BoundsRect := cxRectOffsetHorz(gcCircularThreeFourthFuture.BoundsRect,
    gcCircularThreeFourthFuture.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateCircularWideGauges;
begin
  gcCircularWideAfrica.BeginUpdate;
  gcCircularWideAfrica.Width := (tsCircularWide.Width - Offset * 4) div 3;
  gcCircularWideAfrica.Height := (tsCircularWide.Height - Offset * 3) div 2;
  gcCircularWideAfrica.Left := Offset;
  gcCircularWideAfrica.Top := Offset;
  gcCircularWideAfrica.EndUpdate;
  gcCircularWideSportCar.BoundsRect := cxRectOffsetHorz(gcCircularWideAfrica.BoundsRect,
    gcCircularWideAfrica.Width + Offset);
  gcCircularWideDarkNight.BoundsRect := cxRectOffsetHorz(gcCircularWideSportCar.BoundsRect,
    gcCircularWideSportCar.Width + Offset);

  gcCircularWideWhite.BoundsRect := cxRectOffsetVert(gcCircularWideAfrica.BoundsRect,
    gcCircularWideAfrica.Height + Offset);
  gcCircularWideMechanical.BoundsRect := cxRectOffsetHorz(gcCircularWideWhite.BoundsRect,
    gcCircularWideWhite.Width + Offset);
  gcCircularWideDeepFire.BoundsRect := cxRectOffsetHorz(gcCircularWideMechanical.BoundsRect,
    gcCircularWideMechanical.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateLinearGauges;
begin
  gcLinearIceColdZone.BeginUpdate;
  gcLinearIceColdZone.Width := (tsLinearGauges.Width - Offset * 4) div 5;
  gcLinearIceColdZone.Height := tsLinearGauges.Height - Offset * 3;
  gcLinearIceColdZone.Left := Offset;
  gcLinearIceColdZone.Top := Offset;
  gcLinearIceColdZone.EndUpdate;
  gcLinearYellowSubmarine.BoundsRect := cxRectOffsetHorz(gcLinearIceColdZone.BoundsRect,
    gcLinearIceColdZone.Width + Offset);
  gcLinearSmart.BeginUpdate;
  gcLinearSmart.Top := gcLinearYellowSubmarine.BoundsRect.Top;
  gcLinearSmart.Left := gcLinearYellowSubmarine.BoundsRect.Right + Offset;
  gcLinearSmart.Width := tsLinearGauges.BoundsRect.Right - gcLinearSmart.Left - Offset;
  gcLinearSmart.Height := gcLinearYellowSubmarine.Height;
  gcLinearSmart.EndUpdate;
end;

procedure TfrmSampleGauges.UpdateHybridGauges;
begin
  UpdateTimers;
  gcHybridDarkNight.BeginUpdate;
  gcHybridDarkNight.Width := (tsSimpleHybrid.Width - Offset * 4) div 3;
  gcHybridDarkNight.Height := tsSimpleHybrid.Height - Offset * 3;
  gcHybridDarkNight.Left := Offset;
  gcHybridDarkNight.Top := Offset;
  gcHybridDarkNight.EndUpdate;
  gcHybridIceColdZone.BoundsRect := cxRectOffsetHorz(gcHybridDarkNight.BoundsRect, gcHybridDarkNight.Width + Offset);
  gcHybrid.BoundsRect := cxRectOffsetHorz(gcHybridIceColdZone.BoundsRect, gcHybridIceColdZone.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateTimers;
begin
  case pcHybridGauges.ActivePageIndex of
    0:
      begin
        tCarTesterTimer.Enabled := True;
        tWeatherTimer.Enabled := False;
        tTemperatureTimer.Enabled := False;
        tHazesTimer.Enabled := False;
      end;
    1:
      begin
        tTemperatureTimer.Enabled := True;
        tWeatherTimer.Enabled := False;
        tCarTesterTimer.Enabled := False;
        tHazesTimer.Enabled := False;
      end;
    2:
      begin
        tHazesTimer.Enabled := True;
        tWeatherTimer.Enabled := False;
        tTemperatureTimer.Enabled := False;
        tCarTesterTimer.Enabled := False;
      end;
    3:
      begin
        tWeatherTimer.Enabled := True;
        tHazesTimer.Enabled := False;
        tTemperatureTimer.Enabled := False;
        tCarTesterTimer.Enabled := False;
      end;
    4:
      begin
        tWeatherTimer.Enabled := False;
        tCarTesterTimer.Enabled := False;
        tTemperatureTimer.Enabled := False;
        tHazesTimer.Enabled := False;
      end;
  end;
end;

procedure TfrmSampleGauges.UpdateDigitalGauges;
begin
  gcDigitalIceColdZone.BeginUpdate;
  gcDigitalIceColdZone.Width := (tsDigital.Width - Offset * 3) div 2;
  gcDigitalIceColdZone.Height := (tsDigital.Height - Offset * 3) div 3;
  gcDigitalIceColdZone.Left := Offset;
  gcDigitalIceColdZone.Top := Offset;
  gcDigitalIceColdZone.EndUpdate;
  gcDigitalWhite.BoundsRect := cxRectOffsetHorz(gcDigitalIceColdZone.BoundsRect, gcDigitalIceColdZone.Width + Offset);
  gcDigitalDeepFire.BoundsRect := cxRectOffsetVert(gcDigitalIceColdZone.BoundsRect,
    gcDigitalIceColdZone.Height + Offset);
  gcDigitalScaleText.BoundsRect := cxRectOffsetHorz(gcDigitalDeepFire.BoundsRect, gcDigitalDeepFire.Width + Offset);
  gcDigitalClassic.BoundsRect := cxRectOffsetVert(gcDigitalDeepFire.BoundsRect, gcDigitalDeepFire.Height + Offset);
  gcDigitalFuture.BoundsRect := cxRectOffsetHorz(gcDigitalClassic.BoundsRect, gcDigitalClassic.Width + Offset);
end;

procedure TfrmSampleGauges.UpdateGauges;
begin
  tWeatherTimer.Enabled := False;
  tCarTesterTimer.Enabled := False;
  tTemperatureTimer.Enabled := False;
  tHazesTimer.Enabled := False;
  case PageControl1.TabIndex of
    0:
      UpdateCircularGauges;
    1:
      UpdateCircularHalfGauges;
    2:
      UpdateCircularQuarterGauges;
    3:
      UpdateCircularThreeFouthGauges;
    4:
      UpdateCircularWideGauges;
    5:
      UpdateLinearGauges;
    6:
      UpdateDigitalGauges;
    7:
      UpdateHybridGauges;
  end;
end;

procedure TfrmSampleGauges.UpdateWeatherDate;
var
  ADate: string;
begin
  ADate := gcsWeatherDate.Text;
  FDate := FDate + GetRandomValue(0, 30, 1);
  DateTimeToString(ADate, 'dd MMMM', FDate);
  gcsWeatherDate.Text := ADate;
end;

procedure TfrmSampleGauges.FormCreate(Sender: TObject);
begin
  FDate := Now;
  UpdateWeatherDate;
  UpdateLosAngeles;
  UpdateMoscowWeather;
  UpdateLondonWeather;
  pcHybridGauges.TabIndex := 0;
  PageControl1.TabIndex := 0;
  UpdateGauges;
  LoadCustomStyles;
  gsTesterValue3.Value := 0;
  gsVolumeValueSecondaryCaption1.Text := 'Temperature ' + Chr(176) + 'C';
end;

procedure TfrmSampleGauges.PageControl1Resize(Sender: TObject);
begin
  UpdateGauges;
end;

procedure TfrmSampleGauges.pcHybridGaugesChange(Sender: TObject);
begin
  UpdateTimers;
end;

procedure TfrmSampleGauges.PageControl1Change(Sender: TObject);
begin
  UpdateGauges;
end;

end.

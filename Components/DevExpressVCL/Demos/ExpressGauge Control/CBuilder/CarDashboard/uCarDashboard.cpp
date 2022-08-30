//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "uCarDashboard.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxButtons"
#pragma link "dxGaugeCircularScale"
#pragma link "dxGaugeControl"
#pragma link "dxGaugeCustomScale"
#pragma link "dxGaugeDigitalScale"
#pragma link "dxGaugeQuantitativeScale"
#pragma link "dxGDIPlusClasses"
#pragma link "dxSkinMetropolisDark"
#pragma resource "*.dfm"
TfrmCarDashboard *frmCarDashboard;
Single cAccelerateTachometerValueDelta = 0.15;
Single cBreakTachometerValueDelta = -0.03;
Single cBreakSpeedometerValueDelta = -4;
Single cSpeedometerValueDelta = -0.5;
Single cFuelValueDelta = -0.05;
Single cStartingTachometerValueDelta = 0.05;
Single cTemperatureValueDelta = 0.1;

//---------------------------------------------------------------------------
__fastcall TfrmCarDashboard::TfrmCarDashboard(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::PrepareDesription()
{
  String sdxDescriptionText = "This is a typical car dashboard, created with Express Gauge Control";
  for (int I = 0; I < gsDescription->OptionsView->DigitCount; I++)
	FDescriptionText = FDescriptionText + ' ';
  FDescriptionText = FDescriptionText + sdxDescriptionText;
  for (int I = 0; I < gsDescription->OptionsView->DigitCount; I++)
	FDescriptionText = FDescriptionText + ' ';
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::FormCreate(TObject *Sender)
{
  gsFuel->Value = gsFuel->OptionsView->MaxValue;
  gsFuelIndicator->OptionsView->SegmentColorOn = clNone;
  gsCarStateIndicator->OptionsView->SegmentColorOn = clNone;

  FGearFactors[0] = 0;
  FGearFactors[1] = 3.636;
  FGearFactors[2] = 1.95;
  FGearFactors[3] = 1.357;
  FGearFactors[4] = 0.941;
  FGearFactors[5] = 0.784;

  FSkinController = new TdxSkinController(this);
  FSkinController->NativeStyle = False;
  FSkinController->SkinName = "MetropolisDark";
  FStarted = false;
  UpdateButtonStates();
  PrepareDesription();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::FormDestroy(TObject *Sender)
{
  FSkinController->Free();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateGear()
{
  if (FGear == 0)
	gsGear->Value = 'N';
  else
	gsGear->Value = IntToStr(FGear);
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::Stop()
{
  FStarted = False;
  FGear = 0;
  FFuelValueDelta = 0;
  FTemperatureValueDelta = -FTemperatureValueDelta;
  FTachometerValueDelta = -0.1;
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateTachometer()
{
  Single AValue = gsTachometer->Value + FTachometerValueDelta;
  if (((gsTachometer->Value >= gsTachometer->OptionsView->MaxValue) || (gsTachometer->Value < 0.8)) && FStarted &&
	!FIsStarting)
	Stop();
  else
  {
	if ((FStarted && (!FIsStarting && (AValue < 0.8))) || (FIsStarting && (AValue > 0.8)))
	{
	  AValue = 0.8;
	  FTachometerValueDelta = 0;
	  FIsStarting = !FIsStarting;
	}
	gsTachometer->Value = AValue;
  }
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateSpeed()
{
  if (FGear > 0)
	gsSpeedometr->Value = (Single)0.377 * 0.28 / 3.7 / FGearFactors[FGear] * (gsTachometer->Value - 0.8) * 1000;
  else
  {
	if (FIsBreaking)
	  gsSpeedometr->Value = gsSpeedometr->Value + cBreakSpeedometerValueDelta;
	else
	  gsSpeedometr->Value = gsSpeedometr->Value + cSpeedometerValueDelta;
  }
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateFuelLevel()
{
  gsFuel->Value = gsFuel->Value + FFuelValueDelta;
  if (gsFuel->Value == 0)
	Stop();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateTemperature()
{
  gsTemperature->Value = Max(Min((double)(gsTemperature->Value + FTemperatureValueDelta), 90.0), 0.0);
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateIndicators()
{
  if (FStarted)
	gsCarStateIndicator->OptionsView->SegmentColorOn = dxColorToAlphaColor(clGreen);
  else
	gsCarStateIndicator->OptionsView->SegmentColorOn = clNone;

  if (gsFuel->Value == 0)
	gsFuelIndicator->OptionsView->SegmentColorOn = dxColorToAlphaColor(clRed);
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::UpdateButtonStates()
{
  btnAccelerate->Enabled = FStarted;
  btnBrake->Enabled = FStarted;
  btnEngineStop->Enabled = FStarted;
  btnEngineStart->Enabled = !FStarted && (gsFuel->Value > 0);
  btnUpGear->Enabled = FStarted && (FGear < 5);
  btnDownGear->Enabled = FStarted && (FGear > 0);
  btnNeutralGear->Enabled = FStarted && (FGear > 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::Timer1Timer(TObject *Sender)
{
  UpdateGear();
  UpdateTachometer();
  UpdateSpeed();
  UpdateFuelLevel();
  UpdateTemperature();
  UpdateIndicators();
  UpdateButtonStates();
  if ((gsTachometer->Value < 1) && (FGear > 0))
	Stop();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::tDigitalScaleTimerTimer(TObject *Sender)
{
  gsDescription->Value = FDescriptionText.SubString(FIndex, gsDescription->OptionsView->DigitCount);
  FIndex = (FIndex + 1) % FDescriptionText.Length();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::AccelerateUp()
{
  if (FStarted)
  {
	FTachometerValueDelta = cAccelerateTachometerValueDelta;
	FFuelValueDelta = cFuelValueDelta;
  }
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::AccelerateDown()
{
  FTachometerValueDelta = cBreakTachometerValueDelta;
  FFuelValueDelta = 0;
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::BreakUp()
{
  FIsBreaking = True;
  if (FGear > 0)
	FTachometerValueDelta = cBreakTachometerValueDelta * 2;
}
//---------------------------------------------------------------------------
Single TfrmCarDashboard::GetTachometerValue()
{
  return (Single)(gsSpeedometr->Value / (0.377 * 0.28 / 3.7 / FGearFactors[FGear]) / 1000 + 0.8);
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::GearDown()
{
  SetGear(-1);
  if (FGear > 0)
	gsTachometer->Value = GetTachometerValue();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::GearUp()
{
  SetGear(1);
  if ((FGear > 0) && (gsTachometer->Value < 1))
	Stop();
  if (FGear > 1)
	gsTachometer->Value = GetTachometerValue();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::BreakDown()
{
  FIsBreaking = False;
  FTachometerValueDelta = cBreakTachometerValueDelta;
  if ((FGear > 0) && (gsTachometer->Value < 1))
	Stop();
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::SetGear(int ADelta)
{
  FGear = Max(Min(FGear + ADelta, 5), 0);
}
//---------------------------------------------------------------------------
void TfrmCarDashboard::Start()
{
  FStarted = true;
  FIsStarting = true;
  FGear = 0;
  FFuelValueDelta = 0;
  FTachometerValueDelta = cStartingTachometerValueDelta;
  FTemperatureValueDelta = cTemperatureValueDelta;
  Timer1->Enabled = FStarted;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnEngineStartClick(TObject *Sender)
{
  Start();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnEngineStopClick(TObject *Sender)
{
  Stop();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnFuelingClick(TObject *Sender)
{
  FFuelValueDelta = 2;
  gsFuelIndicator->OptionsView->SegmentColorOn = clNone;
}
//---------------------------------------------------------------------------

void __fastcall TfrmCarDashboard::btnBrakeMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
  BreakUp();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnBrakeMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
  BreakDown();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCarDashboard::btnAccelerateMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
  AccelerateDown();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnAccelerateMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
  AccelerateUp();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnUpGearClick(TObject *Sender)
{
  GearUp();
}
//---------------------------------------------------------------------------

void __fastcall TfrmCarDashboard::btnNeutralGearClick(TObject *Sender)
{
  SetGear(-FGear);
}
//---------------------------------------------------------------------------
void __fastcall TfrmCarDashboard::btnDownGearClick(TObject *Sender)
{
  GearDown();
}
//---------------------------------------------------------------------------

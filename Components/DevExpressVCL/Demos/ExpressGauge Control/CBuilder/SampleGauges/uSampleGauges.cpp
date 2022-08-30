//---------------------------------------------------------------------------

#include <vcl.h>
#include <DateUtils.hpp>
#include <Math.h>

#pragma hdrstop

#include "uSampleGauges.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "dxGaugeCircularScale"
#pragma link "dxGaugeControl"
#pragma link "dxGaugeCustomScale"
#pragma link "dxGaugeDigitalScale"
#pragma link "dxGaugeLinearScale"
#pragma link "dxGaugeQuantitativeScale"
#pragma resource "*.dfm"
TfrmSampleGauges *frmSampleGauges;
int offset = 5;
//---------------------------------------------------------------------------
__fastcall TfrmSampleGauges::TfrmSampleGauges(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
double TfrmSampleGauges::GetRandomValueDelta(int AFrom, int ATo, int ARange)
{
  Randomize();
  int AArray[] = {-1, 1};
  return Sign(RandomFrom(AArray, 2)) * (double)RandomRange(AFrom, ATo) / ARange;
}
//---------------------------------------------------------------------------
double TfrmSampleGauges::GetRandomValue(int AFrom, int ATo, int ARange)
{
  Randomize();
  return (double)RandomRange(AFrom, ATo) / ARange;
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateWeatherDate()
{
  String ADate;
  ADate = gcsWeatherDate->Text;
  FDate = FDate + GetRandomValue(0, 30, 1);
  DateTimeToString(ADate, "dd MMMM", FDate);
  gcsWeatherDate->Text = ADate;
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateCircularGauges()
{
  gcCircularDeepFire->BeginUpdate();
  gcCircularDeepFire->Width = (tsCircularGauges->Width - offset * 4) / 3;
  gcCircularDeepFire->Height = tsCircularGauges->Height - offset * 3;
  gcCircularDeepFire->Left = offset;
  gcCircularDeepFire->Top = offset;
  gcCircularDeepFire->EndUpdate();
  gcCircularWhite->BoundsRect = cxRectOffsetHorz(gcCircularDeepFire->BoundsRect, gcCircularDeepFire->Width + offset);
  gcCircularCleanWhite->BoundsRect = cxRectOffsetHorz(gcCircularWhite->BoundsRect, gcCircularDeepFire->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateCircularHalfGauges()
{
  gcCircularHalfYellowSubmarine->BeginUpdate();
  gcCircularHalfYellowSubmarine->Width = (tsCircularHalfGauges->Width - offset * 4) / 3;
  gcCircularHalfYellowSubmarine->Height = tsCircularHalfGauges->Height - offset * 3;
  gcCircularHalfYellowSubmarine->Left = offset;
  gcCircularHalfYellowSubmarine->Top = offset;
  gcCircularHalfYellowSubmarine->EndUpdate();
  gcCircularHalfClassic->BoundsRect = cxRectOffsetHorz(gcCircularHalfYellowSubmarine->BoundsRect,
	gcCircularHalfYellowSubmarine->Width + offset);
  gcCircularHalfCleanWhite->BoundsRect = cxRectOffsetHorz(gcCircularHalfClassic->BoundsRect,
	gcCircularHalfClassic->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateCircularQuarterGauges()
{
  gcCircularQuarterYellowSubmarine->BeginUpdate();
  gcCircularQuarterYellowSubmarine->Width = (tsCircularQuarterGauges->Width - offset * 4) / 3;
  gcCircularQuarterYellowSubmarine->Height = tsCircularQuarterGauges->Height - offset * 3;
  gcCircularQuarterYellowSubmarine->Left = offset;
  gcCircularQuarterYellowSubmarine->Top = offset;
  gcCircularQuarterYellowSubmarine->EndUpdate();
  gcCircularQuarterDeepFire->BoundsRect = cxRectOffsetHorz(gcCircularQuarterYellowSubmarine->BoundsRect,
	gcCircularQuarterYellowSubmarine->Width + offset);
  gcCircularQuarterSmart->BoundsRect = cxRectOffsetHorz(gcCircularQuarterDeepFire->BoundsRect,
	gcCircularQuarterDeepFire->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateCircularThreeFouthGauges()
{
  gcCircularThreeFourthAfrica->BeginUpdate();
  gcCircularThreeFourthAfrica->Width = (tsCircularThreeFourth->Width - offset * 4) / 3;
  gcCircularThreeFourthAfrica->Height = tsCircularThreeFourth->Height - offset * 3;
  gcCircularThreeFourthAfrica->Left = offset;
  gcCircularThreeFourthAfrica->Top = offset;
  gcCircularThreeFourthAfrica->EndUpdate();
  gcCircularThreeFourthFuture->BoundsRect = cxRectOffsetHorz(gcCircularThreeFourthAfrica->BoundsRect,
	gcCircularThreeFourthAfrica->Width + offset);
  gcCircularThreeFourthDisco->BoundsRect = cxRectOffsetHorz(gcCircularThreeFourthFuture->BoundsRect,
	gcCircularThreeFourthFuture->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateCircularWideGauges()
{
  gcCircularWideAfrica->BeginUpdate();
  gcCircularWideAfrica->Width = (tsCircularWide->Width - offset * 4) / 3;
  gcCircularWideAfrica->Height = (tsCircularWide->Height - offset * 3) / 2;
  gcCircularWideAfrica->Left = offset;
  gcCircularWideAfrica->Top = offset;
  gcCircularWideAfrica->EndUpdate();
  gcCircularWideSportCar->BoundsRect = cxRectOffsetHorz(gcCircularWideAfrica->BoundsRect,
	gcCircularWideAfrica->Width + offset);
  gcCircularWideDarkNight->BoundsRect = cxRectOffsetHorz(gcCircularWideSportCar->BoundsRect,
	gcCircularWideSportCar->Width + offset);

  gcCircularWideWhite->BoundsRect = cxRectOffsetVert(gcCircularWideAfrica->BoundsRect,
	gcCircularWideAfrica->Height + offset);
  gcCircularWideMechanical->BoundsRect = cxRectOffsetHorz(gcCircularWideWhite->BoundsRect,
	gcCircularWideWhite->Width + offset);
  gcCircularWideDeepFire->BoundsRect = cxRectOffsetHorz(gcCircularWideMechanical->BoundsRect,
	gcCircularWideMechanical->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateLinearGauges()
{
  gcLinearIceColdZone->BeginUpdate();
  gcLinearIceColdZone->Width = (tsLinearGauges->Width - offset * 4) / 5;
  gcLinearIceColdZone->Height = tsLinearGauges->Height - offset * 3;
  gcLinearIceColdZone->Left = offset;
  gcLinearIceColdZone->Top = offset;
  gcLinearIceColdZone->EndUpdate();
  gcLinearYellowSubmarine->BoundsRect = cxRectOffsetHorz(gcLinearIceColdZone->BoundsRect,
	gcLinearIceColdZone->Width + offset);
  gcLinearSmart->BeginUpdate();
  gcLinearSmart->Top = gcLinearYellowSubmarine->BoundsRect.Top;
  gcLinearSmart->Left = gcLinearYellowSubmarine->BoundsRect.Right + offset;
  gcLinearSmart->Width = tsLinearGauges->BoundsRect.Right - gcLinearSmart->Left - offset;
  gcLinearSmart->Height = gcLinearYellowSubmarine->Height;
  gcLinearSmart->EndUpdate();
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateTimers()
{
  switch (pcHybridGauges->ActivePageIndex){
	case 0:
	  {
		tCarTesterTimer->Enabled = true;
		tWeatherTimer->Enabled = false;
		tTemperatureTimer->Enabled = false;
		tHazesTimer->Enabled = false;
		break;
	  }
	case 1:
	  {
		tTemperatureTimer->Enabled = true;
		tWeatherTimer->Enabled = false;
		tCarTesterTimer->Enabled = false;
		tHazesTimer->Enabled = false;
		break;
	  }
	case 2:
	  {
		tHazesTimer->Enabled = true;
		tWeatherTimer->Enabled = false;
		tTemperatureTimer->Enabled = false;
		tCarTesterTimer->Enabled = false;
		break;
	  }
	case 3:
	  {
		tWeatherTimer->Enabled = true;
		tHazesTimer->Enabled = false;
		tTemperatureTimer->Enabled = false;
		tCarTesterTimer->Enabled = false;
		break;
	  }
	case 4:
	  {
		tWeatherTimer->Enabled = false;
		tCarTesterTimer->Enabled = false;
		tTemperatureTimer->Enabled = false;
		tHazesTimer->Enabled = false;
		break;
	  }
  }
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateHybridGauges()
{
  UpdateTimers();
  gcHybridDarkNight->BeginUpdate();
  gcHybridDarkNight->Width = (tsSimpleHybrid->Width - offset * 4) / 3;
  gcHybridDarkNight->Height = tsSimpleHybrid->Height - offset * 3;
  gcHybridDarkNight->Left = offset;
  gcHybridDarkNight->Top = offset;
  gcHybridDarkNight->EndUpdate();
  gcHybridIceColdZone->BoundsRect = cxRectOffsetHorz(gcHybridDarkNight->BoundsRect, gcHybridDarkNight->Width + offset);
  gcHybrid->BoundsRect = cxRectOffsetHorz(gcHybridIceColdZone->BoundsRect, gcHybridIceColdZone->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateDigitalGauges()
{
  gcDigitalIceColdZone->BeginUpdate();
  gcDigitalIceColdZone->Width = (tsDigital->Width - offset * 3) / 2;
  gcDigitalIceColdZone->Height = (tsDigital->Height - offset * 3) / 3;
  gcDigitalIceColdZone->Left = offset;
  gcDigitalIceColdZone->Top = offset;
  gcDigitalIceColdZone->EndUpdate();
  gcDigitalWhite->BoundsRect = cxRectOffsetHorz(gcDigitalIceColdZone->BoundsRect, gcDigitalIceColdZone->Width + offset);
  gcDigitalDeepFire->BoundsRect = cxRectOffsetVert(gcDigitalIceColdZone->BoundsRect,
	gcDigitalIceColdZone->Height + offset);
  gcDigitalScaleText->BoundsRect = cxRectOffsetHorz(gcDigitalDeepFire->BoundsRect, gcDigitalDeepFire->Width + offset);
  gcDigitalClassic->BoundsRect = cxRectOffsetVert(gcDigitalDeepFire->BoundsRect, gcDigitalDeepFire->Height + offset);
  gcDigitalFuture->BoundsRect = cxRectOffsetHorz(gcDigitalClassic->BoundsRect, gcDigitalClassic->Width + offset);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateGauges()
{
  tWeatherTimer->Enabled = false;
  tCarTesterTimer->Enabled = false;
  tTemperatureTimer->Enabled = false;
  tHazesTimer->Enabled = false;
  switch (PageControl1->TabIndex) {
	case 0:
	  {
		UpdateCircularGauges();
		break;
	  }
	case 1:
	  {
		UpdateCircularHalfGauges();
		break;
	  }
	case 2:
	  {
		UpdateCircularQuarterGauges();
		break;
	  }
	case 3:
	  {
		UpdateCircularThreeFouthGauges();
		break;
	  }
	case 4:
	  {
		UpdateCircularWideGauges();
		break;
	  }
	case 5:
	  {
		UpdateLinearGauges();
		break;
	  }
	case 6:
	  {
		UpdateDigitalGauges();
		break;
	  }
	case 7:
	  {
		UpdateHybridGauges();
		break;
	  }
  }
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::LoadCustomStyles()
{
  dxGaugeRegisterStyleFromFile(stCircularScale, "RedNeedle.xml");
  dxGaugeRegisterStyleFromFile(stCircularScale, "GreenNeedle.xml");
  dxGaugeRegisterStyleFromFile(stLinearScale, "CustomLinearScaleStyle.xml");
  gsMainBackground->OptionsView->ShowBackground = true;
  gsMainBackground->StyleName = "CustomLinearScaleStyle";
  gsTesterValue1->StyleName = "GreenNeedle";
  gsTesterValue2->StyleName = "RedNeedle";
}
//---------------------------------------------------------------------------
void __fastcall TfrmSampleGauges::FormCreate(TObject *Sender)
{
  FDate = Now();
  UpdateWeatherDate();
  UpdateLosAngeles();
  UpdateMoscowWeather();
  UpdateLondonWeather();
  pcHybridGauges->TabIndex = 0;
  PageControl1->TabIndex = 0;
  UpdateGauges();
  LoadCustomStyles();
  gsTesterValue3->Value = 0;
  gsVolumeValueSecondaryCaption1->Text = "Temperature ";
  gsVolumeValueSecondaryCaption1->Text = gsVolumeValueSecondaryCaption1->Text + Char(176) + 'C';
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::PageControl1Change(TObject *Sender)
{
  UpdateGauges();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::PageControl1Resize(TObject *Sender)
{
  UpdateGauges();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::pcHybridGaugesChange(TObject *Sender)
{
  UpdateTimers();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::tCarTesterTimerTimer(TObject *Sender)
{
  TFormatSettings AFormatSettings;
  gcTester->BeginUpdate();
  gsTesterValue1->Value = gsTesterValue1->Value + GetRandomValueDelta(1, 5, 100);
  gsTesterValue2->Value = gsTesterValue2->Value + GetRandomValueDelta(1, 20, 10);
  gsTesterValue3->Value = gsTesterValue3->Value + GetRandomValueDelta(1, 4, 10);
  gsTesterValue4->Value = gsTesterValue4->Value + GetRandomValueDelta(1, 3, 1);
  AFormatSettings.DecimalSeparator = ',';
  gsTesterValue5->Value = FormatFloat("000.0", (double)dxStrToFloat(gsTesterValue5->Value, ',') +
	(double)GetRandomValueDelta(1, 5, 10), AFormatSettings);
  gsTesterValue6->Value = FormatFloat("000", Min(Max((double)dxStrToFloat(gsTesterValue6->Value, ',') +
	(double)GetRandomValueDelta(-10, 10, 1), -99.0), 999.0), AFormatSettings);
  gsTesterValue7->Value = IntToStr(StrToInt(gsTesterValue7->Value) + (int)GetRandomValueDelta(-1, 1, 1));
  gcTester->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::tHazesTimerTimer(TObject *Sender)
{
  gcsHazeGas->Value = Max((double)gcsHazeGas->Value + (double)GetRandomValueDelta(1, 30, 1), 10.0);
  gcsHazeColdWater->Value = Max((double)gcsHazeColdWater->Value + (double)GetRandomValueDelta(1, 30, 1), 10.0);
  gcsHazeHotWater->Value = Max((double)gcsHazeHotWater->Value + (double)GetRandomValueDelta(1, 30, 1), 10.0);
  gcsHazeElectricity->Value = Max((double)gcsHazeElectricity->Value + (double)GetRandomValueDelta(1, 30, 1), 10.0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmSampleGauges::tTemperatureTimerTimer(TObject *Sender)
{
  gsVolumeValue->Value = gsVolumeValue->Value + GetRandomValueDelta(1, 150, 1);
  gsPressureValue->Value = Min(Max((double)gsPressureValue->Value + (double)GetRandomValueDelta(1, 1, 1), 0.5), 6.5);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateSityWeather(TdxGaugeCircularScale * ATemperatureScale, TdxGaugeCircularScale* AHumidityScale,
  Single ATemperature, Single AHumidity)
{
  ATemperatureScale->Value = ATemperature;
  AHumidityScale->Value = AHumidity;
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateLosAngeles()
{
  Single ATemperature;
  Single AHumidity;
  ATemperature = 7.5 * Sin((Single)((Single)(DayOfTheYear(FDate) * M_PI) / 90) / 2 - ((Single)(91 * M_PI) / 180)) +
	GetRandomValue(-2, 2, 1) + 20.5;
  AHumidity = GetRandomValue(40, 92, 1);
  UpdateSityWeather(gcsWeatherLosAnglesTemperature, gcsWeatherLosAnglesHumidity, ATemperature, AHumidity);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateMoscowWeather()
{
  Single ATemperature;
  Single AHumidity;
  ATemperature = 28 * Sin((Single)((Single)(DayOfTheYear(FDate) * M_PI) / 90) / 2 - ((Single)(91 * M_PI) / 180)) +
	GetRandomValue(0, 2, 1);
  AHumidity = GetRandomValue(60, 100, 1);
  UpdateSityWeather(gcsWeatherMoscowTemperature, gcsWeatherMoscowHumidity, ATemperature, AHumidity);
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateLondonWeather()
{
  Single ATemperature;
  Single AHumidity;
  ATemperature = 10 * Sin((Single)((Single)(DayOfTheYear(FDate) * M_PI) / 90) / 2 - ((Single)(91 * M_PI) / 180)) +
	GetRandomValue(-5, 2, 1) + 13;
  AHumidity = GetRandomValue(65, 100, 1);
  UpdateSityWeather(gcsWeatherLondonTemperature, gcsWeatherLondonHumidity, ATemperature, AHumidity);
}
//---------------------------------------------------------------------------
void __fastcall TfrmSampleGauges::tWeatherTimerTimer(TObject *Sender)
{
  UpdateWeatherDate();
  UpdateLosAngeles();
  UpdateMoscowWeather();
  UpdateLondonWeather();
}
//---------------------------------------------------------------------------
void TfrmSampleGauges::UpdateSityTemperature(Single AValue, TdxGaugeQuantitativeScaleCaption * ACaption,
  TdxGaugeCircularScaleRange * ARange)
{
  int ATemperature;
  ATemperature = int(AValue);
  ACaption->Text = "t: " + IntToStr(ATemperature) + " " + Char(176) + "C";
  if (ATemperature >= 0) {
	  ACaption->OptionsView->Font->Color = clRed;
	  ARange->Color = dxColorToAlphaColor(clRed);
  }
  else
  {
	  ACaption->OptionsView->Font->Color = clBlue;
	  ARange->Color = dxColorToAlphaColor(clBlue);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmSampleGauges::gcsWeatherLosAnglesTemperatureAnimate(TObject *Sender)
{
  gcWeather->BeginUpdate();
  UpdateSityTemperature(gcsWeatherLosAnglesTemperature->Value, gcsWeatherLosAnglesTemperatureCaption, gcsWeatherLosAnglesTemperatureRange);
  gcWeather->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::gcsWeatherLosAnglesHumidityAnimate(TObject *Sender)
{
  gcsWeatherLosAnglesHumidityCaption->Text = "h: " + IntToStr((int)gcsWeatherLosAnglesHumidity->Value) + '%';
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::gcsWeatherMoscowHumidityAnimate(TObject *Sender)
{
  gcsWeatherMoscowHumidityCaption->Text = "h: " + IntToStr((int)gcsWeatherMoscowHumidity->Value) + '%';
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::gcsWeatherLondonHumidityAnimate(TObject *Sender)
{
  gcsWeatherLondonHumidityCaption->Text = "h: " + IntToStr((int)gcsWeatherLondonHumidity->Value) + '%';
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::gcsWeatherMoscowTemperatureAnimate(TObject *Sender)
{
  gcWeather->BeginUpdate();
  UpdateSityTemperature(gcsWeatherMoscowTemperature->Value, gcsWeatherMoscowTemperatureCaption, gcsWeatherMoscowTemperatureRange);
  gcWeather->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TfrmSampleGauges::gcsWeatherLondonTemperatureAnimate(TObject *Sender)
{
  gcWeather->BeginUpdate();
  UpdateSityTemperature(gcsWeatherLondonTemperature->Value, gcsWeatherLondonTemperatureCaption, gcsWeatherLondonTemperatureRange);
  gcWeather->EndUpdate();
}
//---------------------------------------------------------------------------


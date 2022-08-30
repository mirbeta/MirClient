//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uWorldTime.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxSplitter"
#pragma link "dxGaugeCircularScale"
#pragma link "dxGaugeControl"
#pragma link "dxGaugeCustomScale"
#pragma link "dxGaugeDigitalScale"
#pragma link "dxGaugeQuantitativeScale"
#pragma resource "*.dfm"
TfrmWorldTime *frmWorldTime;
int offset = 5;
//---------------------------------------------------------------------------
__fastcall TfrmWorldTime::TfrmWorldTime(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
TTimeInfo TfrmWorldTime::GetTimeInfo(int ATimeZone)
{
  TTimeInfo result;
  Word AMilli;
  TSystemTime ASystemTime;

  GetSystemTime(&ASystemTime);
  DecodeTime(SystemTimeToDateTime(ASystemTime), result.Hour, result.Min, result.Sec, AMilli);
  result.Hour = (result.Hour + ATimeZone) % 12;
  return result;
}
//---------------------------------------------------------------------------
void TfrmWorldTime::UpdateClock(TdxGaugeControl* AGaugeControl, TTimeInfo ATimeInfo)
{
  AGaugeControl->BeginUpdate();
  ((TdxGaugeCircularScale*)AGaugeControl->Scales->Items[3])->Value = ATimeInfo.Min;
  ((TdxGaugeCircularScale*)AGaugeControl->Scales->Items[2])->Value = ATimeInfo.Hour + ATimeInfo.Min / 60;
  ((TdxGaugeCircularScale*)AGaugeControl->Scales->Items[1])->Value = ATimeInfo.Sec;
  AGaugeControl->EndUpdate();
}
//---------------------------------------------------------------------------
void TfrmWorldTime::UpdateClocks()
{
  UpdateClock(gcWashingtonTime, GetTimeInfo(-5));
  UpdateClock(gcParisTime, GetTimeInfo(1));
  UpdateClock(gcMoscowTime, GetTimeInfo(3));
  gsDigital->Value = cxTimeToStr(Now(), "hh:mm:ss");
}
//---------------------------------------------------------------------------
void __fastcall TfrmWorldTime::FormCreate(TObject *Sender)
{
  Application->Title = "World Time";
  frmWorldTime->Constraints->MinHeight = lbDescription->Height + pnlClocks->Constraints->MinHeight +
	cxSplitter1->Height + pnlLocalTime->Constraints->MinHeight;
  frmWorldTime->Constraints->MinWidth = gcWashingtonTime->Constraints->MinWidth + offset * gcWashingtonTime->Constraints->MinWidth;
  CalculateGaugeBounds();
  UpdateClocks();
}
//---------------------------------------------------------------------------
void __fastcall TfrmWorldTime::Timer1Timer(TObject *Sender)
{
  UpdateClocks();
}
//---------------------------------------------------------------------------
void __fastcall TfrmWorldTime::FormShow(TObject *Sender)
{
  Caption = Application->Title;
  Application->Hint = Caption;
}
//---------------------------------------------------------------------------
void TfrmWorldTime::CalculateGaugeBounds()
{
  pnlWashingtonClock->Width = (pnlClocks->Width - offset * 4) / 3;
  pnlWashingtonClock->Height = pnlClocks->Height - offset * 3;
  pnlWashingtonClock->Left = offset;
  pnlWashingtonClock->Top = offset;
  pnlParisClock->BoundsRect = cxRectOffsetHorz(pnlWashingtonClock->BoundsRect, pnlWashingtonClock->Width + offset);
  pnlMoscowClock->BoundsRect = cxRectOffsetHorz(pnlParisClock->BoundsRect, pnlParisClock->Width + offset);
}
//---------------------------------------------------------------------------
void __fastcall TfrmWorldTime::pnlClocksResize(TObject *Sender)
{
  CalculateGaugeBounds();
}
//---------------------------------------------------------------------------


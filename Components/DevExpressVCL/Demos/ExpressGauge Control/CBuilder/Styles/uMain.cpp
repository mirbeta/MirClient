//---------------------------------------------------------------------------

#include <vcl.h>
#include <Math.h>
#pragma hdrstop

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxPC"
#pragma link "cxTrackBar"
#pragma link "dxBarBuiltInMenu"
#pragma link "dxGaugeCircularScale"
#pragma link "dxGaugeControl"
#pragma link "dxGaugeCustomScale"
#pragma link "dxGaugeDigitalScale"
#pragma link "dxGaugeLinearScale"
#pragma link "dxGaugeQuantitativeScale"
#pragma link "dxGalleryControl"
#pragma resource "*.dfm"
TfrmGaugeStyles *frmGaugeStyles;
//---------------------------------------------------------------------------
__fastcall TfrmGaugeStyles::TfrmGaugeStyles(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
String TfrmGaugeStyles::GetStyleNameByCaption(const String ACaption)
{
  return StringReplace(ACaption, " ", "", TReplaceFlags() << rfReplaceAll);
}
//---------------------------------------------------------------------------
String TfrmGaugeStyles::GetStyleCaption(const String AStyleName)
{
  String result = "";
  for (int i = 1; i <= AStyleName.Length(); i++)
  {
	if ((UpperCase(AnsiString(AStyleName[i])) == AStyleName[i]) && (i != 1) & (AStyleName[1] != 'i') && (AnsiString(AStyleName[2]) != " "))
	  result = result + " ";
	result = result + AStyleName[i];
  }
  return result;
}
//---------------------------------------------------------------------------
void TfrmGaugeStyles::UpdateGauges()
{
  dxGaugeControl1->BeginUpdate();
  for(int I = 0; I < dxGaugeControl1->Scales->Count; I++)
  {
	dxGaugeControl1->Scales->Items[I]->StyleName = GetStyleNameByCaption(tcStyles->Tabs->Strings[tcStyles->TabIndex]);
	if (dxGaugeControl1DigitalScale1->Index == I)
	  ((TdxGaugeDigitalScale*)dxGaugeControl1->Scales->Items[I])->OptionsView->SegmentColorOff = dxMakeAlphaColor(clNone);
	else
	  ((TdxGaugeQuantitativeScaleAccess*)dxGaugeControl1->Scales->Items[I])->Value = Double(cxTrackBar1->Position);
  }
  dxGaugeControl1->EndUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmGaugeStyles::cxTrackBar1PropertiesChange(TObject *Sender)
{
  UpdateGauges();
}
//---------------------------------------------------------------------------
void TfrmGaugeStyles::PopulateStyleList()
{
  TStringList* AStyleNames;
  AStyleNames = new TStringList();
  try
  {
	dxGaugeGetPredefinedStyleNames(AStyleNames);
	for (int I = 0; I < AStyleNames->Count; I++)
	  tcStyles->Tabs->Add(GetStyleCaption(AStyleNames->Strings[I]));
	if (AStyleNames->Count > 0)
	  tcStyles->TabIndex = 0;
  }
  catch (Exception &E)
  {
	AStyleNames->Free();
  }
}
//---------------------------------------------------------------------------
void TfrmGaugeStyles::InitTrackBar()
{
  cxTrackBar1->Properties->Max = dxGaugeControl1CircularScale1->OptionsView->MaxValue;
  cxTrackBar1->Properties->Min = dxGaugeControl1CircularScale1->OptionsView->MinValue;
}
//---------------------------------------------------------------------------
void TfrmGaugeStyles::UpdateTime()
{
  dxGaugeControl1DigitalScale1->Value = "Gauges " + cxTimeToStr(Now(), "hh:mm:ss");
}
//---------------------------------------------------------------------------
void __fastcall TfrmGaugeStyles::FormCreate(TObject *Sender)
{
  InitTrackBar();
  PopulateStyleList();
  UpdateGauges();
  UpdateTime();
  Timer1->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmGaugeStyles::tcStylesChange(TObject *Sender)
{
  UpdateGauges();
}
//---------------------------------------------------------------------------

void __fastcall TfrmGaugeStyles::Timer1Timer(TObject *Sender)
{
  UpdateTime();
}
//---------------------------------------------------------------------------


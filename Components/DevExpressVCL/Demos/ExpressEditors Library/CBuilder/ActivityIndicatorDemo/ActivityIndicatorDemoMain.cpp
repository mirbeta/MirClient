//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ActivityIndicatorDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxListView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxActivityIndicator"
#pragma link "cxColorComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxPC"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxBevel"
#pragma resource "*.dfm"
TdxActivityIndicatorDemoForm *dxActivityIndicatorDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxActivityIndicatorDemoForm::TdxActivityIndicatorDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdxActivityIndicatorDemoForm::FormCreate(TObject* Sender)
{
  tcMain->Properties->Tabs->BeginUpdate();
  tcMain->Properties->Tabs->Clear();
  for (int I = 0; I < GetRegisteredActivityIndicatorProperties()->Count; I++)
    tcMain->Properties->Tabs->Add(GetRegisteredActivityIndicatorProperties()->Descriptions[I]);
  tcMain->Properties->Tabs->EndUpdate();
  tcMainChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TdxActivityIndicatorDemoForm::seAnimationTimePropertiesChange(TObject* Sender)
{
  ActivityIndicator->Properties->AnimationTime = seAnimationTime->Value;
}
//---------------------------------------------------------------------------
void __fastcall TdxActivityIndicatorDemoForm::seArcPropertiesChange(TObject* Sender)
{
	if (!FLoading)
	{
	  TdxActivityIndicatorArcBasedProperties* AProperties = dynamic_cast<TdxActivityIndicatorArcBasedProperties*>(ActivityIndicator->Properties);
	  AProperties->ArcColor = dxColorToAlphaColor(ccbArcColor->ColorValue);
	  AProperties->ArcThickness = seArcThickness->Value;
	};
}
//---------------------------------------------------------------------------
void __fastcall TdxActivityIndicatorDemoForm::seDotPropertiesChange(TObject* Sender)
{
    if (!FLoading)
	{
	  TdxActivityIndicatorDotBasedProperties* AProperties = dynamic_cast<TdxActivityIndicatorDotBasedProperties*>(ActivityIndicator->Properties);
	  AProperties->DotColor = dxColorToAlphaColor(ccbDotColor->ColorValue);
      AProperties->DotCount = seDotCount->Value;
      AProperties->DotSize = seDotSize->Value;
    };
}
//---------------------------------------------------------------------------
void __fastcall TdxActivityIndicatorDemoForm::tcMainChange(TObject* Sender)
{
  if (tcMain->TabIndex < 0)
    return;

  FLoading = True;
  ActivityIndicator->PropertiesClassName = GetRegisteredActivityIndicatorProperties()->Items[tcMain->TabIndex]->ClassName();
  seAnimationTime->Value = ActivityIndicator->Properties->AnimationTime;

  //# ArcBased
  TdxActivityIndicatorArcBasedProperties* AArcBasedProperties = dynamic_cast<TdxActivityIndicatorArcBasedProperties*>(ActivityIndicator->Properties);
  gbArcBased->Visible = AArcBasedProperties;
  if (gbArcBased->Visible)
  {
	seArcThickness->Value = AArcBasedProperties->ArcThickness;
    ccbArcColor->ColorValue = dxAlphaColorToColor(AArcBasedProperties->ArcColor);
  }

  //# DotBased
  TdxActivityIndicatorDotBasedProperties* ADotBasedProperties = dynamic_cast<TdxActivityIndicatorDotBasedProperties*>(ActivityIndicator->Properties);
  gbDotBased->Visible = ADotBasedProperties;
  if (gbDotBased->Visible)
  {
	ccbDotColor->ColorValue = dxAlphaColorToColor(ADotBasedProperties->DotColor);
    seDotCount->Value = ADotBasedProperties->DotCount;
    seDotSize->Value = ADotBasedProperties->DotSize;
  }
  FLoading = False;
}
//---------------------------------------------------------------------------

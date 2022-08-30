//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ImageViewerDemoResizeImage.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxCheckGroupBox"
#pragma resource "*.dfm"
TImageViewerDemoResizeImageForm *ImageViewerDemoResizeImageForm;

//---------------------------------------------------------------------------
__fastcall TImageViewerDemoResizeImageForm::TImageViewerDemoResizeImageForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoResizeImageForm::cgbPixelsPropertiesChange(TObject *Sender)
{
  seScale->Enabled = ! cgbPixels->CheckBox->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoResizeImageForm::teHorizontalPropertiesValidate(TObject *Sender,
  Variant &DisplayValue, TCaption &ErrorText, Boolean &Error)
{
  int AValue = StrToIntDef(VarToStr(DisplayValue), 0);
  Error = (AValue < 1)||(AValue > 10000);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoResizeImageForm::teWidthPropertiesChange(TObject *Sender)
{
  CalculateHeight();
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoResizeImageForm::teHeightPropertiesChange(TObject *Sender)
{
  CalculateWidth();
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoResizeImageForm::cbAspectRatioPropertiesChange(TObject *Sender)
{
  CalculateHeight();
}
//---------------------------------------------------------------------------

void TImageViewerDemoResizeImageForm::SetGlyphHeight(Integer Value)
{
  if (FGlyphHeight != Value) {
	FGlyphHeight = Value;
	teHeight->Text = IntToStr(Value);
  }
}
//---------------------------------------------------------------------------

void TImageViewerDemoResizeImageForm::SetGlyphWidth(Integer Value)
{
  if (FGlyphWidth != Value) {
	FGlyphWidth = Value;
	teWidth->Text = IntToStr(Value);
  }
}
//---------------------------------------------------------------------------

void TImageViewerDemoResizeImageForm::CalculateHeight()
{
  if (FLockCount == 0 && teWidth->Text != "" && cbAspectRatio->Checked) {
	FLockCount++;
	try {
	  teHeight->Text = IntToStr(div(StrToInt(teWidth->Text) * GlyphHeight, GlyphWidth).quot);
	}
	__finally {
	  FLockCount--;
	}
  }
}
//---------------------------------------------------------------------------

void TImageViewerDemoResizeImageForm::CalculateWidth()
{
  if (FLockCount == 0 && teHeight->Text != "" && cbAspectRatio->Checked) {
	FLockCount++;
	try {
	  teWidth->Text = IntToStr(div(StrToInt(teHeight->Text) * GlyphWidth, GlyphHeight).quot);
	}
	__finally {
	  FLockCount--;
	}
  }
}
//---------------------------------------------------------------------------






//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#pragma hdrstop

#include "EditorsStylesDemoConvert.h"
#include "Jpeg.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxImage"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxProgressBar"
#pragma link "cxSpinButton"
#pragma link "cxSpinEdit"
#pragma link "cxSplitter"
#pragma link "cxTrackBar"
#pragma link "dxZoomTrackBar"
#pragma link "cxClasses"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeels"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"
TEditorsStylesDemoConvertFrame *EditorsStylesDemoConvertFrame;
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoConvertFrame::TEditorsStylesDemoConvertFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  FBitmap = new Graphics::TBitmap();
  if (cxImage->Picture) {
    cxImage->Width = cxImage->Picture->Width;
    cxImage->Height = cxImage->Picture->Height;
  }
  HintStyle = hcstLightSlideLeft;
  FFileName = "Untitled";
  FDisplayStyle = shtBrick;
  FTempDisplayStyle = shtBrick;
  btnPreviewClick(this);
}
//---------------------------------------------------------------------------

__fastcall TEditorsStylesDemoConvertFrame::~TEditorsStylesDemoConvertFrame()
{
  delete FBitmap;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoConvertFrame::ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle)
{
  TEditorsStylesDemoBaseFrame::ChangeDisplayStyle(ADisplayStyle);
  AdjustTrackBarThumb(ADisplayStyle, dxZoomTrackBar, FBitmap);
}
//---------------------------------------------------------------------------

TColor TEditorsStylesDemoConvertFrame::GetStyleBackgroundColor()
{
  return (gbConvertingOptions->Color);
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoConvertFrame::Name()
{
  return "Image Processing";
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoConvertFrame::BriefName()
{
  return "Image";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoConvertFrame::StylesIniPath()
{
  return "StylesFrmConvert\\";
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoConvertFrame::MenuOpenFileVisible()
{
  return true;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoConvertFrame::OpenFile(TObject* Sender)
{
  btnLoadImageClick(NULL);
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoConvertFrame::Description()
{
  return "Image Processing Notes";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::btnLoadImageClick(
      TObject *Sender)
{
  if (OpenDialog->Execute()) {
    cxImage->Picture->LoadFromFile(OpenDialog->FileName);
    FFileName = OpenDialog->FileName;
    DoOnFileNameChanged();
    cxImage->Width = cxImage->Picture->Width;
    cxImage->Height = cxImage->Picture->Height;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::tbImageQualityPropertiesChange(
      TObject *Sender)
{
  sbImageQuality->Value = tbImageQuality->Position;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::dxZoomTrackBarPropertiesChange(
      TObject *Sender)
{
  cxImage->Width = div(cxImage->Picture->Width * dxZoomTrackBar->Position, 100).quot;
  cxImage->Height = div(cxImage->Picture->Height * dxZoomTrackBar->Position, 100).quot;
  cxProgressBar->Position = dxZoomTrackBar->Position;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::cxTextEditPropertiesChange(
      TObject *Sender)
{
  if (cxTextEdit->Text != "") {
    int Val = Max(StrToInt(cxTextEdit->Text), 110);
    dxZoomTrackBar->Properties->Max = Val;
    cxProgressBar->Properties->Max = Val;
    cxProgressBar->Properties->OverloadValue = 100;
    cxProgressBar->Properties->PeakValue = 100;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::sbImageQualityPropertiesChange(
      TObject *Sender)
{
  tbImageQuality->Position = sbImageQuality->Value;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::cbScalePropertiesChange(
      TObject *Sender)
{
  TJPEGScale JPEGScale = jsFullSize;
  TcxComboBox* AComboBox = (TcxComboBox*)Sender;
  String AValue = VarToStr(AComboBox->EditValue);
  if (AValue == "Full Size")
    JPEGScale = jsFullSize; else
  if (AValue == "Half")
    JPEGScale = jsHalf; else
  if (AValue == "Quarter")
    JPEGScale = jsQuarter; else
  if (AValue == "Eighth")
    JPEGScale = jsEighth;
  ((TJPEGImage*)imgPreview->Picture->Graphic)->Scale = JPEGScale;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::btnPreviewClick(TObject *Sender)
{
  TJPEGImage* J = new TJPEGImage();
  ScrollBox1->Visible = false;
  try {
    J->Assign(cxImage->Picture->Graphic);
    TJPEGOptions AJPEGOptions = GetJPEGOptions();
    J->CompressionQuality = AJPEGOptions.CompressionQuality;
    J->ProgressiveEncoding = AJPEGOptions.ProgressiveEncoding;
    J->ProgressiveDisplay = true;
    J->Compress();
    J->Smoothing = !J->Smoothing;
    imgPreview->Picture->Assign(J);
    ((TJPEGImage*)imgPreview->Picture->Graphic)->Grayscale = AJPEGOptions.Grayscale;
    ((TJPEGImage*)imgPreview->Picture->Graphic)->ProgressiveDisplay = true;
    cbScalePropertiesChange(cbScale);
  }
  __finally {
    ScrollBox1->Visible = true;
    delete J;
  }
}
//---------------------------------------------------------------------------

TJPEGOptions __fastcall TEditorsStylesDemoConvertFrame::GetJPEGOptions()
{
  TJPEGOptions Result;
  Result.CompressionQuality = meCompressionQuality->EditValue;
  Result.Grayscale = cbGrayScale->Checked;
  Result.ProgressiveEncoding = lbProgressive->Checked;
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::bntConvertClick(TObject *Sender)
{
  if (SaveDialog->Execute())
    ConvertBitmapToJPEG(cxImage->Picture->Bitmap, SaveDialog->FileName, GetJPEGOptions());
}
//---------------------------------------------------------------------------

class PASCALIMPLEMENTATION TControlAccess : public TcxCustomButton {
public:
  __property Font;
};

void __fastcall TEditorsStylesDemoConvertFrame::btnGetDrawParams(
      TcxCustomButton *Sender, TcxButtonState AState, TColor &AColor,
      TFont *AFont)
{
  AFont->Color = ((TControlAccess *)Sender)->Font->Color;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::sbScalePropertiesEditValueChanged(
      TObject *Sender)
{
  cbScale->ItemIndex = sbScale->Value;
}
//---------------------------------------------------------------------------



void __fastcall TEditorsStylesDemoConvertFrame::dxZoomTrackBarPropertiesDrawThumb(
      TObject *Sender, TcxCanvas *ACanvas, const TRect &ARect)
{
  ACanvas->Draw(ARect.Left, ARect.Top, FBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::dxZoomTrackBarPropertiesGetThumbRect(
      TObject *Sender, TRect &ARect)
{
  ARect = FBitmap->Canvas->ClipRect;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::AdjustTrackBarThumb(
  TcxStyleSheetType AStyleSheetType, TdxZoomTrackBar* ATrackBar,
  Graphics::TBitmap* ABitmap)
{
  const String AThumbImagesPath = "StylesFrmStylePalette\\";
  switch (AStyleSheetType) {
    case shtWood:
      {
	ABitmap->LoadFromFile(AThumbImagesPath + "Wood.bmp");
	ATrackBar->Properties->ThumbType = cxttCustom;
	break;
      }
      case shtDeepSea:
      {
	ABitmap->LoadFromFile(AThumbImagesPath + "DeepSea.bmp");
	ATrackBar->Properties->ThumbType = cxttCustom;
	break;
      }
	default :  ATrackBar->Properties->ThumbType = cxttRegular;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::cxTextEditExit(TObject *Sender)
{
  cxTextEdit->Text = IntToStr(dxZoomTrackBar->Properties->Max);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::aProgressiveExecute(TObject *Sender)
{
  if (tsProgressive->Checked)
  {
	tsProgressive->Caption = "On";
  }
  else
  {
	tsProgressive->Caption = "Off";
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoConvertFrame::aGrayScaleExecute(TObject *Sender)
{
  if (tsGrayScale->Checked)
  {
	tsGrayScale->Caption = "On";
  }
  else
  {
	tsGrayScale->Caption = "Off";
  }
}
//---------------------------------------------------------------------------


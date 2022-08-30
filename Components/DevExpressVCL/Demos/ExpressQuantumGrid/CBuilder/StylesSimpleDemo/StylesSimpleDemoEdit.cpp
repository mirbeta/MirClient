//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtonEdit"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxStyles"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TStylesSimpleDemoEditForm *StylesSimpleDemoEditForm;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoEditForm::TStylesSimpleDemoEditForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedTextColorKeyPress(TObject *Sender, Char &Key)
{
  Key = 0;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormShow(TObject *Sender)
{
  SaveStyleParams();
  RefreshStyleInfo();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::nbtCancelClick(TObject *Sender)
{
  RestoreStyleParams();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  if (ModalResult != mrOk)
     RestoreStyleParams();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedColorPropertiesButtonClick(TObject *Sender,
    int AButtonIndex)
{
  if (ColorDialog->Execute())
  {
     if (((TComponent*)Sender)->Tag == 0)
       CurrentStyle->Color = ColorDialog->Color;
     else
       CurrentStyle->TextColor = ColorDialog->Color;
     RefreshStyleInfo();
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedFontPropertiesButtonClick(TObject *Sender,
    int AButtonIndex)
{
  FontDialog->Font->Assign(CurrentStyle->Font);
  if (FontDialog->Execute())
  {
    CurrentStyle->Font = FontDialog->Font;
    RefreshStyleInfo();
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormCreate(TObject *Sender)
{
  HoldFont = new TFont();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete HoldFont;
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoEditForm::SaveStyleParams()
{
  HoldColor = CurrentStyle->Color;
  HoldTextColor = CurrentStyle->TextColor;
  HoldFont->Assign(CurrentStyle->Font);
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoEditForm::RestoreStyleParams()
{
  CurrentStyle->Color = HoldColor;
  CurrentStyle->TextColor = HoldTextColor;
  CurrentStyle->Font = HoldFont;
  CurrentStyle->Font->Assign(HoldFont);
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoEditForm::RefreshStyleInfo()
{
  btnedColor->Style->Color = CurrentStyle->Color;
  lbColorValue->Caption = ColorToString(CurrentStyle->Color);

  btnedTextColor->Style->Color = CurrentStyle->TextColor;
  lbTextColorValue->Caption = ColorToString(CurrentStyle->TextColor);

  btnedFont->Text = CurrentStyle->Font->Name;

  if (CurrentStyle->Bitmap->Empty){
    imgExample->Canvas->Brush->Color = CurrentStyle->Color;
    btnedBitmap->Text = "(none)";
  }
  else
  {
    if (imgExample->Canvas->Brush->Bitmap == NULL)
      imgExample->Canvas->Brush->Bitmap = new Graphics::TBitmap();
    imgExample->Canvas->Brush->Bitmap->Assign(CurrentStyle->Bitmap);
    btnedBitmap->Text = "(bitmap)";
  }

  imgExample->Canvas->FillRect(imgExample->ClientRect);
  imgExample->Canvas->Brush->Style = bsClear;
  imgExample->Canvas->Font = CurrentStyle->Font;
  imgExample->Canvas->Font->Color = CurrentStyle->TextColor;
  imgExample->Canvas->TextOut(10, 10, "Style sample here.");

}
//---------------------------------------------------------------------------


void __fastcall TStylesSimpleDemoEditForm::btnedBitmapPropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  switch (AButtonIndex) {
    case 0:
      if (OpenPictureDialog->Execute())
        CurrentStyle->Bitmap->LoadFromFile(OpenPictureDialog->FileName);
      break;
    case 1:
      CurrentStyle->Bitmap->FreeImage();
      CurrentStyle->Bitmap->ReleaseHandle();
      TcxStyleValues AValues;
      AValues << svBitmap;
      CurrentStyle->AssignedValues = CurrentStyle->AssignedValues - AValues;
      break;
  }
  RefreshStyleInfo();
}
//---------------------------------------------------------------------------


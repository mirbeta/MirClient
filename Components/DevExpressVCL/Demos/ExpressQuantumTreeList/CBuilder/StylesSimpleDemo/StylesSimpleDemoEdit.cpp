//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtonEdit"
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxStyles"
#pragma resource "*.dfm"

bool ChangeStyle(TcxStyle *AStyle)
{
 TStylesSimpleDemoEditForm *AForm = new TStylesSimpleDemoEditForm(Application);
 bool Result;
  __try{
    AForm->CurrentStyle = AStyle;
    AForm->Caption = "Edit Style - " + AStyle->Name;
    Result = AForm->ShowModal() == mrOk;
  }
  __finally{
    delete AForm;
  }
  return Result;
}
//---------------------------------------------------------------------------

__fastcall TStylesSimpleDemoEditForm::TStylesSimpleDemoEditForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedTextColorKeyPress(
  TObject *Sender, Char &Key)
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

void __fastcall TStylesSimpleDemoEditForm::FormCloseQuery(
  TObject *Sender, bool &CanClose)
{
   if (ModalResult != mrOk)
     RestoreStyleParams();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedColorPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
   if (ColorDialog->Execute()){
     if (((TComponent*)Sender)->Tag == 0)
       CurrentStyle->Color = ColorDialog->Color;
     else
       CurrentStyle->TextColor = ColorDialog->Color;
     RefreshStyleInfo();
   }
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedFontPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
  FontDialog->Font->Assign(CurrentStyle->Font);
  if (FontDialog->Execute()){
    CurrentStyle->Font = FontDialog->Font;
    RefreshStyleInfo();
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormCreate(TObject *Sender)
{
  HoldFont = new TFont();
  HoldBitmap = new Graphics::TBitmap();
  FSampleBitmap = new Graphics::TBitmap();
  FSampleBitmap->Width = imgExample->Width;
  FSampleBitmap->Height = imgExample->Height;
  imgExample->Picture->Bitmap = FSampleBitmap;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::FormClose(
  TObject *Sender, TCloseAction &Action)
{
  delete HoldFont;
  delete HoldBitmap;
  delete FSampleBitmap;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::btnedBitmapPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
  switch (AButtonIndex){
    case 0:
      if (OpenPictureDialog->Execute())
        CurrentStyle->Bitmap->LoadFromFile(OpenPictureDialog->FileName);
        break;
    case 1:
      CurrentStyle->Bitmap->FreeImage();
      CurrentStyle->Bitmap->ReleaseHandle();
      CurrentStyle->AssignedValues =
        CurrentStyle->AssignedValues - (TcxStyleValues()<<svBitmap);
  }
  RefreshStyleInfo();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::SaveStyleParams()
{
  HoldColor = CurrentStyle->Color;
  HoldTextColor = CurrentStyle->TextColor;
  HoldFont->Assign(CurrentStyle->Font);
  HoldBitmap->Assign(CurrentStyle->Bitmap);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::RestoreStyleParams()
{
  CurrentStyle->Color = HoldColor;
  CurrentStyle->TextColor = HoldTextColor;
  CurrentStyle->Font = HoldFont;
  CurrentStyle->Font->Assign(HoldFont);
  CurrentStyle->Bitmap = HoldBitmap;
  CurrentStyle->Bitmap->Assign(HoldBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoEditForm::RefreshStyleInfo()
{
  btnedColor->Style->Color = CurrentStyle->Color;
  lbColorValue->Caption = ColorToString(CurrentStyle->Color);
  btnedTextColor->Style->Color = CurrentStyle->TextColor;
  lbTextColorValue->Caption = ColorToString(CurrentStyle->TextColor);
  btnedFont->Text = CurrentStyle->Font->Name;
  FSampleBitmap->Canvas->Brush->Style = bsSolid;
  if (CurrentStyle->Bitmap->Empty){
    FSampleBitmap->Canvas->Brush->Color = CurrentStyle->Color;
    btnedBitmap->Text = "(none)";
  }
  else{
    FSampleBitmap->Canvas->Brush->Bitmap = CurrentStyle->Bitmap;
    btnedBitmap->Text = "(bitmap)";
  }
  FSampleBitmap->Canvas->FillRect(
    Rect(0, 0, FSampleBitmap->Width, FSampleBitmap->Height));
  FSampleBitmap->Canvas->Brush->Style = bsClear;
  FSampleBitmap->Canvas->Font->Assign(CurrentStyle->Font);
  FSampleBitmap->Canvas->Font->Color = CurrentStyle->TextColor;
  FSampleBitmap->Canvas->TextOut(10, 10, "Style sample here->");
  imgExample->Picture->Bitmap = FSampleBitmap;

}
//---------------------------------------------------------------------------


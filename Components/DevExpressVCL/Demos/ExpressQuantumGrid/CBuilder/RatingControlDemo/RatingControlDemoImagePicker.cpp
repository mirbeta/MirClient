//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "RatingControlDemoImagePicker.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmImagePicker *frmImagePicker;
//---------------------------------------------------------------------------
__fastcall TfrmImagePicker::TfrmImagePicker(TComponent* Owner)
	: TForm(Owner)
{
}

void __fastcall TfrmImagePicker::ApplyClick(TObject* Sender)
{
  FProperties->BeginUpdate();
  FProperties->Glyph->Assign(imgUnchecked->Picture->Graphic);
  FProperties->HoverGlyph->Assign(imgHover->Picture->Graphic);
  FProperties->CheckedGlyph->Assign(imgChecked->Picture->Graphic);
  FProperties->EndUpdate();
}

void __fastcall TfrmImagePicker::ResetClick(TObject* Sender)
{
  FProperties->BeginUpdate();
  FProperties->Glyph->Clear();
  FProperties->HoverGlyph->Clear();
  FProperties->CheckedGlyph->Clear();
  FProperties->EndUpdate();
}

void TfrmImagePicker::Initialize(TdxRatingControlProperties* AProperties)
{
  FProperties = AProperties;
}

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoCheckBoxes.h"
#include "cxGeometry.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmCheckBoxes *frmCheckBoxes;
//---------------------------------------------------------------------------
__fastcall TfrmCheckBoxes::TfrmCheckBoxes(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmCheckBoxes::Panel1Resize(TObject *Sender)
{
  Panel2->BoundsRect = cxRectCenter(Panel1->ClientRect, Panel2->Width, Panel2->Height);	
}
//---------------------------------------------------------------------------
void __fastcall TfrmCheckBoxes::SetColor(TShape* AShape, bool ASelected, TColor AColor)
{
  if (ASelected)
  {
    AShape->Brush->Color = AColor;
    AShape->Pen->Color = AColor;
  }
  else
  {
    AShape->Brush->Color = clWindow;
    AShape->Pen->Color = TColor(0x99A8AC);
  }
}

void __fastcall TfrmCheckBoxes::SetColors(TColor AColors[], AnsiString ASelectValue, AnsiString AMultiSelectValue)
{
  if (AMultiSelectValue == "") AMultiSelectValue = "000";
  SetColor(Shape1, AMultiSelectValue[1] != '0', AColors[0]);
  SetColor(Shape2, AMultiSelectValue[2] != '0', AColors[1]);
  SetColor(Shape3, AMultiSelectValue[3] != '0', AColors[2]);
  SetColor(Shape4, ASelectValue == '0', AColors[0]);
  SetColor(Shape5, ASelectValue == '1', AColors[1]);
  SetColor(Shape6, ASelectValue == '2', AColors[2]);
}

void __fastcall TfrmCheckBoxes::SetParameters(AnsiString ASelectValue, AnsiString AMultiSelectValue, bool ABlackAndWhite)
{
  TColor ARGBColors[3] = {clRed, clYellow, clLime};
  TColor ABlackAndWhiteColors[3] = {TColor(0x828282), TColor(0xF8F8F8), TColor(0xDCDCDC)};

  if (ABlackAndWhite)
    SetColors(ABlackAndWhiteColors, ASelectValue, AMultiSelectValue);
  else
    SetColors(ARGBColors, ASelectValue, AMultiSelectValue);
}

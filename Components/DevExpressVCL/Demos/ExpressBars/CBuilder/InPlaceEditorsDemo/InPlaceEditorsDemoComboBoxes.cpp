//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <math.hpp>
#include "cxGraphics.hpp"
#include "cxGeometry.hpp"
#include "InPlaceEditorsDemoComboBoxes.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmComboBoxes *frmComboBoxes;
//---------------------------------------------------------------------------
__fastcall TfrmComboBoxes::TfrmComboBoxes(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
  FImage = new Graphics::TBitmap;
}
//---------------------------------------------------------------------------
void __fastcall TfrmComboBoxes::SetParameters(TColor AColor, AnsiString AFontName, AnsiString AFolder,
	  int AFontSize, Graphics::TBitmap* AImage, String APerson)
{
  FColor = AColor;
  FFontName = AFontName;
  FFontSize = AFontSize;
  FPath = AFolder;
  FPerson = "Contact person: " + APerson;
  FImage->Assign(AImage);
  PaintBox1->Invalidate();
}

void __fastcall TfrmComboBoxes::FormDestroy(TObject *Sender)
{
  delete FImage;
}
//---------------------------------------------------------------------------

void __fastcall TfrmComboBoxes::PaintBox1Paint(TObject *Sender)
{
  if (FImage != 0)
  {
    PaintBox1->Canvas->Brush->Color = FColor;
    PaintBox1->Canvas->FillRect(PaintBox1->ClientRect);
    PaintBox1->Canvas->Font->Name = FFontName;
    PaintBox1->Canvas->Font->Size = FFontSize;
    PaintBox1->Canvas->Brush->Style = bsClear;
    int AIndent = Min((PaintBox1->Width - FImage->Width - PaintBox1->Canvas->TextWidth(FPerson) - 4) / 2,
      (PaintBox1->Width - FImage->Width - PaintBox1->Canvas->TextWidth(FPath) - 4) / 2);
    AIndent = Max(0, AIndent);
	TRect ARect = cxRectBounds(AIndent, (PaintBox1->Height - FImage->Height) / 2, FImage->Width, FImage->Height);
	cxPaintCanvas()->BeginPaint(PaintBox1->Canvas);
	cxDrawImage(cxPaintCanvas(), ARect, FImage, 0, -1, true);
	cxPaintCanvas()->EndPaint();
	PaintBox1->Canvas->TextOut(ARect.Right + 4, (PaintBox1->Height - PaintBox1->Canvas->TextHeight(FPath)) / 2, FPath);
    PaintBox1->Canvas->TextOut(ARect.Right + 4, (PaintBox1->Height - PaintBox1->Canvas->TextHeight(FPerson)) / 2 + PaintBox1->Canvas->TextHeight(FPath), FPerson);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmComboBoxes::FormCreate(TObject *Sender)
{
//
}
//---------------------------------------------------------------------------


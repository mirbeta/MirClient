//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoValue.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmValueEditors *frmValueEditors;
//---------------------------------------------------------------------------
__fastcall TfrmValueEditors::TfrmValueEditors(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmValueEditors::PaintBox1Paint(TObject *Sender)
{
  PaintBox1->Canvas->Font->Name = FFontName;
  PaintBox1->Canvas->Font->Size = FFontSize;
  AnsiString AStrings[3] = {FDate, FTime, FValue};
  DrawText(AStrings, PaintBox1, 3);
}
//---------------------------------------------------------------------------
void __fastcall TfrmValueEditors::SetParameters(int AFontSize, AnsiString ADate, AnsiString ATime, Currency AMoney)
{
  FFontName = "Times New Roman";
  FFontSize = (AFontSize + 8) / 2;
  FDate = "Date: " + ADate;
  FTime = "Time: " + ATime;
  TVarRec args = AMoney;
  FValue = Format("Value: %m", &args, 0);
  PaintBox1->Invalidate();
}

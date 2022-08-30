//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoText.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmTextEditors *frmTextEditors;
//---------------------------------------------------------------------------
__fastcall TfrmTextEditors::TfrmTextEditors(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmTextEditors::SetParameters(AnsiString ACompany, AnsiString AHyperLink, AnsiString APhone)
{
  FCompany = "COMPANY: " + ACompany;
  FWWW = "WWW: " + AHyperLink;
  FPhone = "PHONE: " + APhone;
  PaintBox1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmTextEditors::PaintBox1Paint(TObject *Sender)
{
  PaintBox1->Canvas->Brush->Color = clWindow;
  PaintBox1->Canvas->FillRect(PaintBox1->ClientRect);
  AnsiString AStrings[3] = {FCompany, FWWW, FPhone};
  DrawText(AStrings, PaintBox1, 3);
}
//---------------------------------------------------------------------------


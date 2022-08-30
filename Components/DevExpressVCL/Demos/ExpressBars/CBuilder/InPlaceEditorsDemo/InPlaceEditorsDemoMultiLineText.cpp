//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoMultiLineText.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMultiLineTextEditors *frmMultiLineTextEditors;
//---------------------------------------------------------------------------
__fastcall TfrmMultiLineTextEditors::TfrmMultiLineTextEditors(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
}

void __fastcall TfrmMultiLineTextEditors::LoadText(TStrings* AStrings, TStream* AStream)
{
  if (AStream != 0)
  {
    AStrings->Clear();
    AStrings->LoadFromStream(AStream);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMultiLineTextEditors::SetParameters(TStringStream* ARichTextStream, TStringStream* APlainTextStream)
{
  LoadText(RichEdit1->Lines, ARichTextStream);
  LoadText(Memo1->Lines, APlainTextStream);
}
void __fastcall TfrmMultiLineTextEditors::FormCreate(TObject *Sender)
{
  Splitter1->Left = Memo1->Left;
}
//---------------------------------------------------------------------------


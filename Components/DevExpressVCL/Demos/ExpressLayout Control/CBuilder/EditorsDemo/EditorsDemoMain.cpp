//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsDemoMain.h"
#include "DemoDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BasicDemoMain"
#pragma link "cxControls"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControlAdapters"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TfrmEditorsDemoMain *frmEditorsDemoMain;
//---------------------------------------------------------------------------
__fastcall TfrmEditorsDemoMain::TfrmEditorsDemoMain(TComponent* Owner)
	: TfrmBasicDemoMain(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditorsDemoMain::lcMainGroup2Button0Click(TObject *Sender)
{
  lcMainGroup2->Parent = NULL;
}
//---------------------------------------------------------------------------


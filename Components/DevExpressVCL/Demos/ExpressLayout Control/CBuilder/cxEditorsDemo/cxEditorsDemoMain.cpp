//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "cxEditorsDemoMain.h"
#include "DemoDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BasicDemoMain"
#pragma link "cxGraphics"
#pragma link "cxControls"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutContainer"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBEdit"
#pragma link "cxDBNavigator"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxNavigator"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxTimeEdit"
#pragma link "dxLayoutcxEditAdapters"
#pragma resource "*.dfm"
TfrmEditorsDemoMain *frmEditorsDemoMain;
//---------------------------------------------------------------------------
__fastcall TfrmEditorsDemoMain::TfrmEditorsDemoMain(TComponent* Owner)
	: TfrmBasicDemoMain(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmEditorsDemoMain::lcMainGroup1Button0Click(TObject *Sender)
{
  lcMainGroup1->Parent = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfrmEditorsDemoMain::lcMainSplitterItem3CanResize(TObject *Sender,
          TdxCustomLayoutItem *AItem, int &ANewSize, bool &AAccept)
{
  AAccept = AItem == lcMainItem32;
  if (AAccept && (ANewSize < 200))
  {
	ANewSize = 200;
  }
}
//---------------------------------------------------------------------------


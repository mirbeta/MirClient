//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CheckGroupsDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "DemoBasicMain"
#pragma resource "*.dfm"
TfmGheckGroupsDemo *fmGheckGroupsDemo;
//---------------------------------------------------------------------------
__fastcall TfmGheckGroupsDemo::TfmGheckGroupsDemo(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TfmGheckGroupsDemo::ShowTreeLines1Click(TObject *Sender)
{
  if (((TMenuItem*)(Sender))->Checked)
	tlDXInstallation->OptionsView->TreeLineStyle = tllsDot;
  else
	tlDXInstallation->OptionsView->TreeLineStyle = tllsNone;
}
//---------------------------------------------------------------------------


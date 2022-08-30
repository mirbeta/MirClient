//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "LayoutViewDemoGroupScrollingMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBLayoutView"
#pragma link "cxGridLayoutView"
#pragma link "cxGridLevel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "dxLayoutContainer"
#pragma link "dxmdaset"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxRadioGroup"
#pragma link "cxLabel"
#pragma link "cxDropDownEdit"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}

void __fastcall TfrmMain::seRecordHeightPropertiesChange(TObject* Sender)
{
	LayoutView->OptionsView->RecordSize->Height = seRecordHeight->Value;
}

void __fastcall TfrmMain::seRecordWidthPropertiesChange(TObject* Sender)
{
	LayoutView->OptionsView->RecordSize->Width = seRecordWidth->Value;
}
//---------------------------------------------------------------------------

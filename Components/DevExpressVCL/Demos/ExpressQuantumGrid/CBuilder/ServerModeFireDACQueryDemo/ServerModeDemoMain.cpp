//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ServerModeDemoMain.h"
#include "ServerModeDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxCalendar"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCardView"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridPopupMenu"
#pragma link "cxGridServerModeTableView"
#pragma link "cxGridTableView"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridServerModeBandedTableView"
#pragma link "cxCheckBox"
#pragma link "cxSpinEdit"
#pragma resource "*.dfm"
TServerModeDemoMainForm *ServerModeDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TServerModeDemoMainForm::TServerModeDemoMainForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoMainForm::cxGrid1ActiveTabChanged(TcxCustomGrid *Sender,
	  TcxGridLevel &ALevel)
{
  if (ALevel.GridView == cxGrid1ServerModeBandedTableView1){
	cxGrid1ServerModeTableView1->DataController->DataSource = NULL;
	cxGrid1ServerModeBandedTableView1->DataController->DataSource = ServerModeDemoDataDM->ServerModeQueryDataSource;
	cxGrid1ServerModeBandedTableView1->DataController->DataSource->Active = True;
  }else{
	cxGrid1ServerModeBandedTableView1->DataController->DataSource = NULL;
	cxGrid1ServerModeTableView1->DataController->DataSource = ServerModeDemoDataDM->ServerModeQueryDataSource;
	cxGrid1ServerModeTableView1->DataController->DataSource->Active = True;
  }
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoMainForm::FormClose(TObject *Sender,
	  TCloseAction &Action)
{
  Application->Terminate();
}
//---------------------------------------------------------------------------
void TServerModeDemoMainForm::Initialize()
{
  Caption = GetCaption();
  lbDescription->Caption = GetDescription();
  cxGrid1ServerModeTableView1->DataController->DataSource = ServerModeDemoDataDM->ServerModeQueryDataSource;
  ServerModeDemoDataDM->ServerModeQueryDataSource->Active = true;
}

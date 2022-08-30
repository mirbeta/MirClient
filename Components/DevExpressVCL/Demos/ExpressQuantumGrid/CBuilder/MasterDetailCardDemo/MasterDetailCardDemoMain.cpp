//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "MasterDetailCardDemoMain.h"
#include "FilmsDemoData.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxDBEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxLookupEdit"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxTextEdit"
#pragma link "cxDBLookupEdit"
#pragma link "cxNavigator"
#pragma link "cxDBNavigator"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TMasterDetailCardDemoMainForm *MasterDetailCardDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TMasterDetailCardDemoMainForm::TMasterDetailCardDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailCardDemoMainForm::FormCreate(TObject *Sender)
{
	FilmsDemoDM->FilmsFiltered = false;
	FilmsDemoDM->cdsFilmsPersons->MasterSource = FilmsDemoDM->dsFilms;
	FilmsDemoDM->cdsFilmsPersons->IndexFieldNames = "FILMID";
	FilmsDemoDM->cdsFilmsPersons->MasterFields = "ID";
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailCardDemoMainForm::miGridClick(TObject *Sender)
{
  if (miGrid->Checked)
	SetGridMasterDetailStyle();
  else
	SetStandardMasterDetailStyle();
}
//---------------------------------------------------------------------------
void TMasterDetailCardDemoMainForm::SetStandardMasterDetailStyle(void)
{
	TcxGridDBDataController *AData;
	lvFilmsPersons->Visible = False;
	AData = cvFilmsPersons->DataController;
	AData->DataModeController->SmartRefresh = False;
	FilmsDemoDM->cdsFilmsPersons->MasterSource = FilmsDemoDM->dsFilms;
	pnlDetail->Visible = True;
	lblMaster->Visible = True;
	lblStyle->Caption = "Standard master-detail style";
}
//---------------------------------------------------------------------------

void TMasterDetailCardDemoMainForm::SetGridMasterDetailStyle(void)
{
	TcxGridDBDataController *AData;
	lvFilmsPersons->Visible = True;
	AData = cvFilmsPersons->DataController;
	AData->DataModeController->SmartRefresh = True;
	pnlDetail->Visible = False;
	FilmsDemoDM->cdsFilmsPersons->MasterSource = NULL;
	lblMaster->Visible = False;
	lblStyle->Caption = "ExpressQuantumGrid master-detail style";
}




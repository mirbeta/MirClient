//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "MasterDetailTableDemoMain.h"
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
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxContainer"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCardView"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TMasterDetailTableDemoMainForm *MasterDetailTableDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TMasterDetailTableDemoMainForm::TMasterDetailTableDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailTableDemoMainForm::FormCreate(TObject *Sender)
{
	FilmsDemoDM->FilmsFiltered = false;
	FilmsDemoDM->cdsFilmsPersons->MasterSource = FilmsDemoDM->dsFilms;
	FilmsDemoDM->cdsFilmsPersons->IndexFieldNames = "FILMID";
	FilmsDemoDM->cdsFilmsPersons->MasterFields = "ID";
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailTableDemoMainForm::miGridClick(TObject *Sender)
{
 if (miGrid->Checked)
   SetGridMasterDetailStyle();
 else
   SetStandardMasterDetailStyle();
}
//---------------------------------------------------------------------------

void TMasterDetailTableDemoMainForm::SetGridMasterDetailStyle(void)
{
  TcxGridDBTableView *AView;
  TcxGridDBDataController *AData;

  // remove master/detail link in the data module
  FilmsDemoDM->cdsFilmsPersons->MasterSource = NULL;

  // create view in the first grid (Grid)
  AView = (TcxGridDBTableView*)(Grid->CreateView(__classid(TcxGridDBTableView)));
  AView->Assign(lvDetail->GridView);
  AData = (TcxGridDBDataController*)(AView->DataController);
  AData->KeyFieldNames = "ID";
  AData->MasterKeyFieldNames = "ID";
  AData->DetailKeyFieldNames = "FilmID";
  AData->DataModeController->SmartRefresh = True;

  // hide the second grid (GridDetail)
  delete lvDetail->GridView;
  pnlDetail->Visible = False;
  Splitter->Visible = False;
  // bind AView to first grid's detail level
  lvFilmsPersonsStaff->Visible = True;
  lvFilmsPersonsStaff->GridView = AView;

  lblMaster->Visible = False;
  lblStyle->Caption = "ExpressQuantumGrid master-detail style";
}
//---------------------------------------------------------------------------

void TMasterDetailTableDemoMainForm::SetStandardMasterDetailStyle(void)
{
  TcxGridDBTableView *AView;
  TcxGridDBDataController *AData;

  // restore master/detail link in the data module
  FilmsDemoDM->cdsFilmsPersons->MasterSource = FilmsDemoDM->dsFilms;
  FilmsDemoDM->cdsFilmsPersons->IndexFieldNames = "FILMID";
  FilmsDemoDM->cdsFilmsPersons->MasterFields = "ID";

  // create view in the second grid (GridDetail)
  AView = (TcxGridDBTableView*)(GridDetail->CreateView(__classid(TcxGridDBTableView)));
  AView->Assign(lvFilmsPersonsStaff->GridView);
  AData = (TcxGridDBDataController*)(AView->DataController);
  AData->KeyFieldNames = "ID";
  AData->MasterKeyFieldNames = "";
  AData->DetailKeyFieldNames = "";
  AData->DataModeController->SmartRefresh = False;

  // remove the detail level from the first grid (Grid)
  lvFilmsPersonsStaff->Visible = False;
  delete lvFilmsPersonsStaff->GridView;

  // bind AView to second grid's level
  lvDetail->GridView = AView;
  pnlDetail->Visible = True;
  Splitter->Visible = True;

  lblMaster->Visible = True;
  lblStyle->Caption = "Standard master-detail style";
}

//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "MasterDetailDemoMain.h"
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
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxImage"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TMasterDetailDemoMainForm *MasterDetailDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TMasterDetailDemoMainForm::TMasterDetailDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailDemoMainForm::FormCreate(TObject *Sender)
{
	FilmsDemoDM->FilmsFiltered = False;
}
//---------------------------------------------------------------------------
void __fastcall TMasterDetailDemoMainForm::FormShow(TObject *Sender)
{
	if (FilmsDemoDM->cdsFilms->Active)
	{
		FilmsDemoDM->cdsFilms->First();
		if (tvFilms->Controller->FocusedRecord != NULL)
			tvFilms->Controller->FocusedRecord->Expanded = true;
  	}
}
//---------------------------------------------------------------------------

void __fastcall TMasterDetailDemoMainForm::miShowPreviewDataClick(
      TObject *Sender)
{
  if ((tvFilms->Preview->Column) == NULL)
    tvFilms->Preview->Column = tvFilmsPLOTOUTLINE;
  tvFilms->Preview->Visible = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMasterDetailDemoMainForm::miTabsPositionClick(
      TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  lvFilms->Options->DetailTabsPosition =
    (TcxGridDetailTabsPosition)(((TMenuItem*)Sender)->Tag + 1);
}
//---------------------------------------------------------------------------

void __fastcall TMasterDetailDemoMainForm::miDetailViewsSynchronizationClick(
  TObject *Sender)
{
  for (int I = 0; I < Grid->ViewCount; I++)
    Grid->Views[I]->Synchronization = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------



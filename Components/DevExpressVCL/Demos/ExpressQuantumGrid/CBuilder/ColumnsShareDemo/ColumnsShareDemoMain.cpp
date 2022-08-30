//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ColumnsShareDemoMain.h"
#include "ColumnsShareDemoData.h"
#include "AboutDemoForm.h"
#include "ColumnsShareDemoLookupCustomize.h"
#include "shellapi.h"
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
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridDBCardView"
#pragma link "cxGridPopupMenu"
#pragma link "cxDBEditRepository"
#pragma link "cxDBExtLookupComboBox"
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma resource "*.dfm"
TColumnsShareDemoMainForm *ColumnsShareDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TColumnsShareDemoMainForm::TColumnsShareDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoMainForm::FormShow(TObject *Sender)
{
  ColumnsShareDemoLookupCustomizeForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoMainForm::miCustomizePersonsLookupComboboxClick(
	  TObject *Sender)
{
  ColumnsShareDemoLookupCustomizeForm->Show();
}
//---------------------------------------------------------------------------


void __fastcall TColumnsShareDemoMainForm::miLookUpEditorClick(
	  TObject *Sender)
{
  if (((TMenuItem*)Sender)->Checked) {
	tvItemsCREATORID->RepositoryItem = cxEditRepository->Items[1];
    tvItemsOWNERID->RepositoryItem = cxEditRepository->Items[1];
    tvProjectsMANAGERID->RepositoryItem = cxEditRepository->Items[1];
    tvTeamUSERID->RepositoryItem = cxEditRepository->Items[1];
	tvProjects->DataController->ClearDetails();
    miCustomizePersonsLookupCombobox->Enabled = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TColumnsShareDemoMainForm::miExtLookUpEditorClick(
      TObject *Sender)
{
  if (((TMenuItem*)Sender)->Checked) {
    tvItemsCREATORID->RepositoryItem = cxEditRepository->Items[0];
    tvItemsOWNERID->RepositoryItem = cxEditRepository->Items[0];
    tvProjectsMANAGERID->RepositoryItem = cxEditRepository->Items[0];
    tvTeamUSERID->RepositoryItem = cxEditRepository->Items[0];
	tvProjects->DataController->ClearDetails();
	if ((ColumnsShareDemoLookupCustomizeForm) &&
      ColumnsShareDemoLookupCustomizeForm->Showing)
      ColumnsShareDemoLookupCustomizeForm->Hide();
    miCustomizePersonsLookupCombobox->Enabled = false;
  }
}
//---------------------------------------------------------------------------


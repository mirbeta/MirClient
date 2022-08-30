//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "SummaryFooterDemoMain.h"
#include "SummaryFooterDemoData.h"
#include "AboutDemoForm.h"
#include "SummaryFooterDemoEditSummary.h"
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
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCardView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma resource "*.dfm"
TSummaryFooterDemoMainForm *SummaryFooterDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSummaryFooterDemoMainForm::TSummaryFooterDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::FormShow(TObject *Sender)
{
  if(dmGridCars->mdModels->Active) {
    dmGridCars->mdModels->First();
    if (tvCars->Controller->FocusedRecord != NULL)
      tvCars->Controller->FocusedRecord->Expanded = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::miCustomizeSummariesClick(TObject *Sender)
{
  SummaryFooterDemoEditSummaryForm->ShowModal();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::miSelectedRecordOnlyClick(TObject *Sender)
{
  if(Grid->FocusedView == NULL)
    return;
  TcxDataSummary* ASummary = Grid->FocusedView->DataController->Summary;
  if(ASummary->Options.Contains(soSelectedRecords))
    ASummary->Options = ASummary->Options>>soSelectedRecords;
  else
    ASummary->Options = ASummary->Options<<soSelectedRecords;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::miIgnoreNullValuesClick(TObject *Sender)
{
  if(Grid->FocusedView == NULL)
    return;
  TcxDataSummary* ASummary = Grid->FocusedView->DataController->Summary;
  if(ASummary->Options.Contains(soNullIgnore))
    ASummary->Options = ASummary->Options>>soNullIgnore;
  else
    ASummary->Options = ASummary->Options<<soNullIgnore;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  if(Grid->FocusedView == NULL)
    return;
  TcxGridDBTableView* AView = ((TcxGridDBTableView*)Grid->FocusedView);
  AView->OptionsSelection->MultiSelect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::GridFocusedViewChanged(
      TcxCustomGrid *Sender, TcxCustomGridView *APrevFocusedView,
      TcxCustomGridView *AFocusedView)
{
  TcxGridDBTableView* AView = ((TcxGridDBTableView*)Grid->FocusedView);
  miSelectedRecordOnly->Checked = AView->DataController->Summary->Options.Contains(soSelectedRecords);
  miIgnoreNullValues->Checked = AView->DataController->Summary->Options.Contains(soNullIgnore);
  miMultiSelect->Checked = AView->OptionsSelection->MultiSelect;
  miFooter->Checked = AView->OptionsView->Footer;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryFooterDemoMainForm::miFooterClick(TObject *Sender)
{
  if(Grid->FocusedView == NULL)
    return;
  TcxGridDBTableView* AView = ((TcxGridDBTableView*)Grid->FocusedView);
  AView->OptionsView->Footer = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------



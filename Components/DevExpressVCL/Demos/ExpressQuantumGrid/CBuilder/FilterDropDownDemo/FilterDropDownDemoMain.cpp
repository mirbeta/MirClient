//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FilterDropDownDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TfmBaseForm(Owner)
{
}

void __fastcall TfrmMain::acClassicModeApplyChangesExecute(TObject* Sender)
{
  if (acClassicModeApplyChangesImmediately->Checked)
	TableView->Filtering->ColumnPopup->ApplyMultiSelectChanges = fpacImmediately;
  else
	TableView->Filtering->ColumnPopup->ApplyMultiSelectChanges = fpacOnButtonClick;
}

void __fastcall TfrmMain::acClassicModeMultiSelectExecute(TObject* Sender)
{
  TableView->Filtering->ColumnPopup->MultiSelect = acClassicModeMultiSelect->Checked;
}

void __fastcall TfrmMain::acDateTimePageTypeExecute(TObject* Sender)
{
  if (acExcelModeDateTimePageTypeTree->Checked)
	TableView->Filtering->ColumnExcelPopup->DateTimeValuesPageType = dvptTree;
  else
	TableView->Filtering->ColumnExcelPopup->DateTimeValuesPageType = dvptList;
}

void __fastcall TfrmMain::acDoNothingExecute(TObject* Sender)
{
//do nothing
}

void __fastcall TfrmMain::acExcelModeApplyChangesExecute(TObject* Sender)
{
  if (acExcelModeApplyChangesImmediately->Checked)
	TableView->Filtering->ColumnExcelPopup->ApplyChanges = efacImmediately;
  else
	TableView->Filtering->ColumnExcelPopup->ApplyChanges = efacOnTabOrOKButtonClick;
}

void __fastcall TfrmMain::acFilterPopupModeExecute(TObject* Sender)
{
  if (acFilterPopupModeClassic->Checked)
	TableView->Filtering->ColumnPopupMode = fpmClassic;
  else
	TableView->Filtering->ColumnPopupMode = fpmExcel;
  UpdateFilterPopupActions();
}

void __fastcall TfrmMain::acNumericPageTypeExecute(TObject* Sender)
{
  if (acExcelModeNumericPageTypeRange->Checked)
	TableView->Filtering->ColumnExcelPopup->NumericValuesPageType = nvptRange;
  else
	TableView->Filtering->ColumnExcelPopup->NumericValuesPageType = nvptList;
}

void TfrmMain::UpdateFilterPopupActions()
{
  acExcelModeDateTimePageType->Visible = TableView->Filtering->ColumnPopupMode == fpmExcel;
  acExcelModeNumericPageType->Visible = TableView->Filtering->ColumnPopupMode == fpmExcel;
  acExcelModeApplyChanges->Visible = TableView->Filtering->ColumnPopupMode == fpmExcel;
  acClassicModeApplyChanges->Visible = TableView->Filtering->ColumnPopupMode == fpmClassic;
  acClassicModeMultiSelect->Visible = TableView->Filtering->ColumnPopupMode == fpmClassic;
  if (TableView->Filtering->ColumnPopupMode == fpmExcel)
	acFilterPopupMode->Caption = "Mode (Excel)";
  else
	acFilterPopupMode->Caption = "Mode (Classic)";
}

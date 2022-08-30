//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CheckBoxMultiSelectMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TfmBaseForm(Owner)
{
}

void TfrmMain::UpdateCheckBoxesVisibility(TcxGridCheckBoxVisibilityOption AOption, bool AInclude)
{
  if (AInclude)
	TableView->OptionsSelection->CheckBoxVisibility << AOption;
  else
	TableView->OptionsSelection->CheckBoxVisibility >> AOption;
}

void __fastcall TfrmMain::cbShowCheckBoxesDynamicallyChanged(TObject* Sender)
{
  TableView->OptionsSelection->ShowCheckBoxesDynamically = cbShowCheckBoxesDynamically->Checked;
}

void __fastcall TfrmMain::cbClearSelectionWithAClickOutsideChanged(TObject* Sender)
{
  TableView->OptionsSelection->ClearPersistentSelectionOnOutsideClick = cbClearSelectionOnClickOutsideSelection->Checked;
}

void __fastcall TfrmMain::cbDataRowCheckBoxVisibilityChanged(TObject* Sender)
{
  UpdateCheckBoxesVisibility(cbvDataRow, cbDataRowCheckBoxVisible->Checked);
}

void __fastcall TfrmMain::cbGroupRowCheckBoxVisibilityChanged(TObject* Sender)
{
  UpdateCheckBoxesVisibility(cbvGroupRow, cbGroupRowCheckBoxVisible->Checked);
}

void __fastcall TfrmMain::cbColumnHeaderCheckBoxVisibilityChanged(TObject* Sender)
{
  UpdateCheckBoxesVisibility(cbvColumnHeader, cbColumnHeaderCheckBoxSelectorVisible->Checked);
}

void __fastcall TfrmMain::cdsOrdersCalcFields(TDataSet* DataSet)
{
  cdsOrdersPaymentAmount->Value = cdsOrdersQuantity->Value * cdsOrdersUnitPrice->Value;
}

void __fastcall TfrmMain::cbPersistentSelectionPropertiesEditValueChanged(TObject* Sender)
{
  if (cbPersistentSelection->Checked)
	TableView->OptionsSelection->MultiSelectMode = msmPersistent;
  else
	TableView->OptionsSelection->MultiSelectMode = msmStandard;
}

void __fastcall TfrmMain::CheckBoxPositionChanged(TObject* Sender)
{
  if (rbFirstColumn->Checked)
	TableView->OptionsSelection->CheckBoxPosition = cbpFirstColumn;
  else
	TableView->OptionsSelection->CheckBoxPosition = cbpIndicator;
  cbShowCheckBoxesDynamically->Enabled = TableView->OptionsSelection->CheckBoxPosition == cbpFirstColumn;
}

void __fastcall TfrmMain::FormCreate(TObject* Sender)
{
  cdsCustomers->Open();
  cdsOrders->Open();
}

void __fastcall TfrmMain::FormShow(TObject* Sender)
{
  TableView->Controller->FocusRecord(2, True);
  TableView->DataController->SelectRows(4, 6);
  TableView->DataController->SelectRows(8, 10);
}

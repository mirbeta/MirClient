//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FindPanelDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TfmBaseForm(Owner)
{
}

void __fastcall TfrmMain::icbFindFilterColumnsPropertiesChange(TObject* Sender)
{
  for (int I = 0; I < TableView->ColumnCount; I++)
  {
	TableView->Columns[I]->Options->FilteringWithFindPanel =
		(icbFindFilterColumns->EditValue == "All") ||
		(Pos(TableView->Columns[I]->Name, icbFindFilterColumns->EditValue) > 0);
  }
}

void TfrmMain::UpdateFindFilterColumns()
{
  icbFindFilterColumns->Properties->Items->Clear();
  TcxImageComboBoxItem* AImageComboBoxItem = icbFindFilterColumns->Properties->Items->Add();
  AImageComboBoxItem->Description = "All";
  AImageComboBoxItem->Value = "All";
  for (int I = 0; I < TableView->ColumnCount; I++)
  {
	TcxGridColumn* AColumn = TableView->Columns[I];
	if (!AColumn->Visible)
	  continue;
	UnicodeString AFindFilterColumnsDescription = TableView->Columns[I]->Caption;
	UnicodeString AFindFilterColumnsValue = TableView->Columns[I]->Name;
	for (int J = I; J < TableView->ColumnCount; J++)
	{
	  AColumn = TableView->Columns[J];
	  if (!AColumn->Visible)
		continue;
	  if (J != I)
	  {
		AFindFilterColumnsDescription = AFindFilterColumnsDescription + ";" + TableView->Columns[J]->Caption;
		AFindFilterColumnsValue = AFindFilterColumnsValue + ";" + TableView->Columns[J]->Name;
	  }
	  AImageComboBoxItem = icbFindFilterColumns->Properties->Items->Add();
	  AImageComboBoxItem->Description = AFindFilterColumnsDescription;
	  AImageComboBoxItem->Value = AFindFilterColumnsValue;
	}
  }
  icbFindFilterColumns->ItemIndex = 0;
}

void __fastcall TfrmMain::seFindDelayPropertiesChange(TObject* Sender)
{
  TableView->FindPanel->ApplyInputDelay = seFindDelay->Value;
}

void __fastcall TfrmMain::actClearFindOnCloseChange(TObject* Sender)
{
  TableView->FindPanel->ClearFindFilterTextOnClose = actClearFindOnClose->Checked;
}

void __fastcall TfrmMain::actHighlightFindResultChange(TObject* Sender)
{
  TableView->FindPanel->HighlightSearchResults = actHighlightSearchResults->Checked;
}

void __fastcall TfrmMain::actShowClearButtonChange(TObject* Sender)
{
  TableView->FindPanel->ShowClearButton = actShowClearButton->Checked;
}

void __fastcall TfrmMain::actShowCloseButtonChange(TObject* Sender)
{
  TableView->FindPanel->ShowCloseButton = actShowCloseButton->Checked;
}

void __fastcall TfrmMain::actShowFindButtonEChange(TObject* Sender)
{
  TableView->FindPanel->ShowFindButton = actShowFindButton->Checked;
}

void __fastcall TfrmMain::actUseDelayedSearchExecute(TObject* Sender)
{
  TableView->FindPanel->UseDelayedFind = actUseDelayedSearch->Checked;
}

void __fastcall TfrmMain::actUseExtendedSyntaxExecute(TObject* Sender)
{
  TableView->FindPanel->UseExtendedSyntax = actUseExtendedSyntax->Checked;
}

void __fastcall TfrmMain::cbDisplayModePropertiesChange(TObject* Sender)
{
  if (cbeDisplayMode->ItemIndex == 0)
	TableView->FindPanel->DisplayMode = fpdmNever;
  else
	if (cbeDisplayMode->ItemIndex == 1)
	  TableView->FindPanel->DisplayMode = fpdmManual;
	else
	  TableView->FindPanel->DisplayMode = fpdmAlways;
  actShowCloseButton->Enabled = TableView->FindPanel->DisplayMode != fpdmAlways;
}

void __fastcall TfrmMain::cbFindPanelPositionPropertiesChange(TObject* Sender)
{
  if (cbeFindPanelPosition->Text == "Top")
	TableView->FindPanel->Position = fppTop;
  else
	TableView->FindPanel->Position = fppBottom;
}

void __fastcall TfrmMain::cdsOrdersCalcFields(TDataSet* DataSet)
{
  cdsOrdersPaymentAmount->Value = cdsOrdersQuantity->Value * cdsOrdersUnitPrice->Value;
}

void __fastcall TfrmMain::FormCreate(TObject* Sender)
{
  cdsCustomers->Open();
  cdsOrders->Open();

  UpdateFindFilterColumns();

  TableView->Controller->ShowFindPanel();
}

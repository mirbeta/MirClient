//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FixedGroupsDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TfmBaseForm(Owner)
{
}

void __fastcall TfrmMain::acFixedGroupsExecute(TObject* Sender)
{
  TableView->OptionsBehavior->FixedGroups = !TableView->OptionsBehavior->FixedGroups;
}

void __fastcall TfrmMain::cdsOrdersCalcFields(TDataSet* DataSet)
{
  cdsOrdersPaymentAmount->Value = cdsOrdersQuantity->Value * cdsOrdersUnitPrice->Value;
}

void __fastcall TfrmMain::FormCreate(TObject* Sender)
{
  cdsCustomers->Open();
  cdsOrders->Open();

  ActiveControl = Grid;
  TableView->Controller->FocusedRowIndex = 0;
  TableView->ViewData->Expand(True);
}

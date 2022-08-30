//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FixedDataRowsMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
		: TfmBaseForm(Owner)
{
}

void __fastcall TfrmMain::FixationCapabilityChanged(TObject* Sender)
{
  UpdateFixationCapability();
}

void __fastcall TfrmMain::PinVisibilityChanged(TObject* Sender)
{
  UpdatePinVisibility();
}

void __fastcall TfrmMain::tbSeparatorWidthPropertiesChange(TObject* Sender)
{
  TableView->FixedDataRows->SeparatorWidth = tbSeparatorWidth->Position;
}

void TfrmMain::UpdateFixationCapability()
{
  if (rbShowPopup->Checked)
	TableView->FixedDataRows->PinClickAction = rpcaShowPopup;
  else
	if (rbFixRowToTop->Checked)
	  TableView->FixedDataRows->PinClickAction = rpcaFixToTop;
	else
	  if (rbFixRowToBottom->Checked)
		TableView->FixedDataRows->PinClickAction = rpcaFixToBottom;
	  else
		TableView->FixedDataRows->PinClickAction = rpcaNone;
}

void TfrmMain::UpdatePinVisibility()
{
  if (rbPinVisibilityNever->Checked)
	TableView->FixedDataRows->PinVisibility = rpvNever;
  else
	if (rbPinVisibilityAlways->Checked)
	  TableView->FixedDataRows->PinVisibility = rpvAlways;
	else
	  if (rbPinVisibilityHover->Checked)
		TableView->FixedDataRows->PinVisibility = rpvHotTrack;
	  else
		TableView->FixedDataRows->PinVisibility = rpvRowHotTrack;
}

void __fastcall TfrmMain::FormCreate(TObject* Sender)
{
  cdsCustomers->Open();
  cdsOrders->Open();
}

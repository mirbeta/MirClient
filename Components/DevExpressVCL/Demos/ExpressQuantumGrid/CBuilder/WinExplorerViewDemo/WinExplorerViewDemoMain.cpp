//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WinExplorerViewDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbGroupByPropertiesEditValueChanged(TObject* Sender)
{
  UpdateGroup();
}

void __fastcall TfrmMain::cbHotTrackPropertiesEditValueChanged(TObject* Sender)
{
  WinExplorerView->OptionsBehavior->HotTrack = cbHotTrack->Checked;
}

void __fastcall TfrmMain::cbMultiSelectPropertiesEditValueChanged(TObject* Sender)
{
  WinExplorerView->OptionsSelection->MultiSelect = cbMultiSelect->Checked;
}

void __fastcall TfrmMain::cbShowCheckBoxesPropertiesEditValueChanged(TObject* Sender)
{
  ShowCheckBoxes(cbShowCheckBoxes->Checked);
}

void __fastcall TfrmMain::cbShowExpandButtonPropertiesEditValueChanged(TObject* Sender)
{
  ShowExpandButtons(cbShowExpandButtons->Checked);
}

void __fastcall TfrmMain::cbSortByPropertiesEditValueChanged(TObject* Sender)
{
  UpdateSortOrder();
}

void __fastcall TfrmMain::gcDisplayModesItemClick(TObject* Sender, TdxGalleryControlItem* AItem)
{
  TcxGridWinExplorerViewDisplayMode ADisplayMode = GetDisplayModeByTag(AItem->Tag);
  SetDisplayMode(ADisplayMode);
}

void TfrmMain::ShowCheckBoxes(bool AValue)
{
  WinExplorerView->OptionsView->ShowItemCheckBoxes = AValue;
}

void TfrmMain::ShowExpandButtons(bool AValue)
{
  WinExplorerView->OptionsView->ShowExpandButtons = AValue;
}

void TfrmMain::UpdateGroup()
{
  WinExplorerView->BeginGroupingUpdate();
  try
  {
	SetGroupItemSortOrder(Dxcore::soNone);
	int AGroupItemTag = cbGroupBy->Properties->Items->IndexOf(cbGroupBy->Text);
	TcxGridWinExplorerViewItem* AGroupItem = GetGroupItemByTag(AGroupItemTag);
	SetGroupItem(AGroupItem);
	UpdateSortOrder();
	WinExplorerView->Controller->FocusNextRecord(cxRecordIndexNone, True, False, False, False);
  }
  __finally
  {
	WinExplorerView->EndGroupingUpdate();
  }
}

void TfrmMain::UpdateSortOrder()
{
  WinExplorerView->BeginSortingUpdate();
  try
  {
	TdxSortOrder ASortOrder = GetSortOrderByText(cbSortBy->Text);
	SetGroupItemSortOrder(ASortOrder);
	SetTextItemSortOrder(ASortOrder);
  }
  __finally
  {
	WinExplorerView->EndSortingUpdate();
  }
}

TcxGridWinExplorerViewItem* TfrmMain::GetGroupItemByTag(int AValue)
{
  TcxCustomGridTableItem* AItem = WinExplorerView->FindItemByTag(AValue);
  return (TcxGridWinExplorerViewItem*) AItem;
}

TdxSortOrder TfrmMain::GetSortOrderByText(UnicodeString AValue)
{
  if (AValue == "Ascending")
	return soAscending;
  else
	if (AValue == "Descending")
	  return soDescending;
	else
	  return Dxcore::soNone;
}

TcxGridWinExplorerViewDisplayMode TfrmMain::GetDisplayModeByTag(int AValue)
{
  return (TcxGridWinExplorerViewDisplayMode) AValue;	
}

void TfrmMain::SetGroupItem(TcxGridWinExplorerViewItem* AValue)
{
  WinExplorerView->ItemSet->GroupItem = AValue;
}

void TfrmMain::SetGroupItemSortOrder(TdxSortOrder ASortOrder)
{
  if (WinExplorerView->ItemSet->GroupItem != NULL)
	WinExplorerView->ItemSet->GroupItem->SortOrder = ASortOrder;
}

void TfrmMain::SetTextItemSortOrder(TdxSortOrder ASortOrder)
{
  if (WinExplorerView->ItemSet->TextItem != NULL)
	WinExplorerView->ItemSet->TextItem->SortOrder = ASortOrder;
}

void TfrmMain::SetDisplayMode(TcxGridWinExplorerViewDisplayMode AValue)
{
  WinExplorerView->ActiveDisplayMode = AValue;
}

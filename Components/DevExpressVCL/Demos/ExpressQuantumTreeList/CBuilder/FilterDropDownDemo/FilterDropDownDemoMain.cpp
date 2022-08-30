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
		: TDemoBasicMainForm(Owner)
{
}

void __fastcall TfrmMain::FormCreate(TObject* Sender)
{
  TreeList->FullExpand();
}

void __fastcall TfrmMain::acClassicModeApplyChangesExecute(TObject* Sender)
{
  if (acClassicModeApplyChangesImmediately->Checked)
	TreeList->Filtering->ColumnPopup->ApplyMultiSelectChanges = fpacImmediately;
  else
	TreeList->Filtering->ColumnPopup->ApplyMultiSelectChanges = fpacOnButtonClick;
}

void __fastcall TfrmMain::acClassicModeMultiSelectExecute(TObject* Sender)
{
  TreeList->Filtering->ColumnPopup->MultiSelect = acClassicModeMultiSelect->Checked;
}

void __fastcall TfrmMain::acDateTimePageTypeExecute(TObject* Sender)
{
  if (acExcelModeDateTimePageTypeTree->Checked)
	TreeList->Filtering->ColumnExcelPopup->DateTimeValuesPageType = dvptTree;
  else
	TreeList->Filtering->ColumnExcelPopup->DateTimeValuesPageType = dvptList;
}

void __fastcall TfrmMain::acDoNothingExecute(TObject* Sender)
{
//do nothing
}

void __fastcall TfrmMain::acExcelModeApplyChangesExecute(TObject* Sender)
{
  if (acExcelModeApplyChangesImmediately->Checked)
	TreeList->Filtering->ColumnExcelPopup->ApplyChanges = efacImmediately;
  else
	TreeList->Filtering->ColumnExcelPopup->ApplyChanges = efacOnTabOrOKButtonClick;
}

void __fastcall TfrmMain::acFilterPopupModeExecute(TObject* Sender)
{
  if (acFilterPopupModeClassic->Checked)
	TreeList->Filtering->ColumnPopupMode = fpmClassic;
  else
	TreeList->Filtering->ColumnPopupMode = fpmExcel;
  UpdateFilterPopupActions();
}

void __fastcall TfrmMain::acNumericPageTypeExecute(TObject* Sender)
{
  if (acExcelModeNumericPageTypeRange->Checked)
	TreeList->Filtering->ColumnExcelPopup->NumericValuesPageType = nvptRange;
  else
	TreeList->Filtering->ColumnExcelPopup->NumericValuesPageType = nvptList;
}

void __fastcall TfrmMain::TreeListStylesGetContentStyle(TcxCustomTreeList* Sender, TcxTreeListColumn* AColumn,
	TcxTreeListNode* ANode, TcxStyle* &AStyle)
{
	if ((ANode != NULL) && (ANode->Level == 0))
	{
	   AStyle = stMaroon;
	}
}

void TfrmMain::UpdateFilterPopupActions()
{
  acExcelModeDateTimePageType->Visible = TreeList->Filtering->ColumnPopupMode == fpmExcel;
  acExcelModeNumericPageType->Visible = TreeList->Filtering->ColumnPopupMode == fpmExcel;
  acExcelModeApplyChanges->Visible = TreeList->Filtering->ColumnPopupMode == fpmExcel;
  acClassicModeApplyChanges->Visible = TreeList->Filtering->ColumnPopupMode == fpmClassic;
  acClassicModeMultiSelect->Visible = TreeList->Filtering->ColumnPopupMode == fpmClassic;
  if (TreeList->Filtering->ColumnPopupMode == fpmExcel)
	acFilterPopupMode->Caption = "Mode (Excel)";
  else
	acFilterPopupMode->Caption = "Mode (Classic)";
}

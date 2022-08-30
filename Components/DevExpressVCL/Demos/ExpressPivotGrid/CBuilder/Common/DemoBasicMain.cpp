//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoBasicMain.h"
#include "cxExportPivotGridLink.hpp"
#include "AboutDemoForm.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxLookAndFeels"
#pragma link "cxClasses"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TfrmDemoBasicMain::TfrmDemoBasicMain(TComponent* Owner)
  : TForm(Owner)
{
  Application->Title = Caption;
  FLookAndFeelController = new TcxLookAndFeelController(this);
  SyncMenuWithTotalsPosition();
  SyncMenuWithTotalVisibility();
  SyncMenuWithElementsVisibility();
  SyncMenuWithOptionsSelection();
}

TcxCustomPivotGrid* __fastcall TfrmDemoBasicMain::PivotGrid()
{
  return NULL;
}

void __fastcall TfrmDemoBasicMain::miTouchModeClick(TObject *Sender)
{
  cxLookAndFeelController1->TouchMode = miTouchMode->Checked;
}

void __fastcall TfrmDemoBasicMain::miExportToClick(TObject *Sender)
{
  if (SaveDialog->Execute()) {
	switch (((TMenuItem*)Sender)->Tag){
	  case 2:
		cxExportPivotGridToText(SaveDialog->FileName, PivotGrid(), true, "", "", "", "txt"); break;
	  case 3:
		cxExportPivotGridToHTML(SaveDialog->FileName, PivotGrid(), true, "html"); break;
	  case 4:
		cxExportPivotGridToXML(SaveDialog->FileName, PivotGrid(), true, "xml"); break;
	  case 5:
		cxExportPivotGridToXLSX(SaveDialog->FileName, PivotGrid(), true, "xlsx"); break;
	  case 11:
        cxExportPivotGridDataToExcel(ChangeFileExt(SaveDialog->FileName, ".xls"), PivotGrid(), NULL, true, true, true, false); break;
	  case 12:
		cxExportPivotGridToExcel(SaveDialog->FileName, PivotGrid(), true, true, "xls"); break;
	  case 51:
        cxExportPivotGridDataToExcel(ChangeFileExt(SaveDialog->FileName, ".xlsx"), PivotGrid(), NULL, true, true, true, false); break;
	  case 52:
		cxExportPivotGridToXLSX(SaveDialog->FileName, PivotGrid(), true, "xlsx"); break;
	}
  }
}

void __fastcall TfrmDemoBasicMain::miTotalsLocationClick(TObject *Sender)
{
	int ATag = ((TMenuItem*)Sender)->Tag;
	if (ATag < 2)
		PivotGrid()->OptionsView->ColumnTotalsLocation = TcxPivotGridColumnTotalsLocation(ATag);
	else
		PivotGrid()->OptionsView->RowTotalsLocation = TcxPivotGridRowTotalsLocation(ATag - 2);
	SyncMenuWithTotalsPosition();
}

void __fastcall TfrmDemoBasicMain::miExitClick(TObject *Sender)
{
  Application->Terminate();
}

void __fastcall TfrmDemoBasicMain::FormCreate(TObject *Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  miTouchMode->Checked = cxIsTouchModeEnabled;
}

void __fastcall TfrmDemoBasicMain::miTotalsVisibilityClick(TObject *Sender)
{
  SyncTotalVisibilityWithMenu();
}

void __fastcall TfrmDemoBasicMain::miElementsVisibilityClick(TObject *Sender)
{
  SyncElementsVisibilityWithMenu();
}
void __fastcall TfrmDemoBasicMain::miAboutClick(TObject *Sender)
{
  ShowAboutDemoForm();
}

void __fastcall TfrmDemoBasicMain::SyncElementsVisibilityWithMenu()
{
  PivotGrid()->OptionsView->ColumnFields = miShowColumnFields->Checked;
  PivotGrid()->OptionsView->DataFields = miShowDataFields->Checked;
  PivotGrid()->OptionsView->FilterFields = miShowFilterFields->Checked;
  PivotGrid()->OptionsView->FilterSeparator = miShowFilterSeparator->Checked;
  PivotGrid()->OptionsView->RowFields = miShowRowFields->Checked;
}

void __fastcall TfrmDemoBasicMain::SyncTotalVisibilityWithMenu()
{
  PivotGrid()->OptionsView->ColumnTotals = miShowColumnTotals->Checked;
  PivotGrid()->OptionsView->RowTotals = miShowRowTotals->Checked;
  PivotGrid()->OptionsView->ColumnGrandTotals = miShowColumnGrandTotals->Checked;
  PivotGrid()->OptionsView->RowGrandTotals = miShowRowGrandTotals->Checked;
  PivotGrid()->OptionsView->TotalsForSingleValues = miShowTotalsForSingleValues->Checked;
  PivotGrid()->OptionsView->GrandTotalsForSingleValues = miShowGrandTotalsForSingleValues->Checked;
}

void __fastcall TfrmDemoBasicMain::SyncMenuWithElementsVisibility()
{
  miShowColumnFields->Checked = PivotGrid()->OptionsView->ColumnFields;
  miShowDataFields->Checked = PivotGrid()->OptionsView->DataFields;
  miShowFilterFields->Checked = PivotGrid()->OptionsView->FilterFields;
  miShowFilterSeparator->Checked = PivotGrid()->OptionsView->FilterSeparator;
  miShowRowFields->Checked = PivotGrid()->OptionsView->RowFields;
}

void __fastcall TfrmDemoBasicMain::SyncMenuWithOptionsSelection()
{
  miMultiSelect->Checked = PivotGrid()->OptionsSelection->MultiSelect;
  miCrossCells->Checked = PivotGrid()->OptionsSelection->IncludeCells.Contains(osiCrossCells);
  miGrandTotalsCells->Checked = PivotGrid()->OptionsSelection->IncludeCells.Contains(osiGrandTotalCells);
  miTotalsCells->Checked = PivotGrid()->OptionsSelection->IncludeCells.Contains(osiTotalCells);
  miHideFocusRect->Checked = PivotGrid()->OptionsSelection->HideFocusRect;
  miHideSelection->Checked = PivotGrid()->OptionsSelection->HideSelection;
}

void __fastcall TfrmDemoBasicMain::SyncMenuWithTotalVisibility()
{
  miShowColumnTotals->Checked = PivotGrid()->OptionsView->ColumnTotals;
  miShowRowTotals->Checked = PivotGrid()->OptionsView->RowTotals;
  miShowColumnGrandTotals->Checked = PivotGrid()->OptionsView->ColumnGrandTotals;
  miShowRowGrandTotals->Checked = PivotGrid()->OptionsView->RowGrandTotals;
  miShowTotalsForSingleValues->Checked = PivotGrid()->OptionsView->TotalsForSingleValues;
  miShowGrandTotalsForSingleValues->Checked = PivotGrid()->OptionsView->GrandTotalsForSingleValues;
}

void __fastcall TfrmDemoBasicMain::miMultiSelectClick(TObject *Sender)
{
  PivotGrid()->OptionsSelection->MultiSelect = !PivotGrid()->OptionsSelection->MultiSelect;
  ((TMenuItem*)Sender)->Checked = PivotGrid()->OptionsSelection->MultiSelect;
}

void __fastcall TfrmDemoBasicMain::miHideFocusRectClick(TObject *Sender)
{
  PivotGrid()->OptionsSelection->HideFocusRect = !PivotGrid()->OptionsSelection->HideFocusRect;
  ((TMenuItem*)Sender)->Checked = PivotGrid()->OptionsSelection->HideFocusRect;
}

void __fastcall TfrmDemoBasicMain::miHideSelectionClick(TObject *Sender)
{
  PivotGrid()->OptionsSelection->HideSelection = !PivotGrid()->OptionsSelection->HideSelection;
  ((TMenuItem*)Sender)->Checked = PivotGrid()->OptionsSelection->HideSelection;
}

void __fastcall TfrmDemoBasicMain::IncludeCellsClick(TObject *Sender)
{
  TcxPivotGridOptionsSelectionIncludes AIncludeCells;

  AIncludeCells.Clear();
  if (miCrossCells->Checked)
	AIncludeCells << osiCrossCells;
  if (miGrandTotalsCells->Checked)
	AIncludeCells << osiGrandTotalCells;
  if (miTotalsCells->Checked)
	AIncludeCells << osiTotalCells;
  PivotGrid()->OptionsSelection->IncludeCells = AIncludeCells;
}

void __fastcall TfrmDemoBasicMain::AddLookAndFeelMenu()
{
  mmMain->Items->Insert(mmMain->Items->IndexOf(miAbout),
	CreateLookAndFeelMenuItems(mmMain->Items, FLookAndFeelController));
}

TcxLookAndFeelKind __fastcall TfrmDemoBasicMain::GetDefaultLookAndFeelKind()
{
  return(lfOffice11);
}

bool __fastcall TfrmDemoBasicMain::IsNativeDefaultStyle()
{
  return(false);
}

void __fastcall TfrmDemoBasicMain::SetDefaultLookAndFeel()
{
  FLookAndFeelController->NativeStyle = IsNativeDefaultStyle();
  FLookAndFeelController->Kind = GetDefaultLookAndFeelKind();
}
//---------------------------------------------------------------------------
void __fastcall TfrmDemoBasicMain::SyncMenuWithTotalsPosition()
{
  miColumnTotalsPositionFar->Checked = PivotGrid()->OptionsView->ColumnTotalsLocation == ctlFar;
  miColumnTotalsPositionNear->Checked = PivotGrid()->OptionsView->ColumnTotalsLocation == ctlNear;
  miRowTotalsPositionFar->Checked = PivotGrid()->OptionsView->RowTotalsLocation == rtlFar;
  miRowTotalsPositionNear->Checked = PivotGrid()->OptionsView->RowTotalsLocation == rtlNear;
  miRowTotalsPositionTree->Checked = PivotGrid()->OptionsView->RowTotalsLocation == rtlTree;
}
//---------------------------------------------------------------------------

void __fastcall TfrmDemoBasicMain::CopyToClipboard()
{
  PivotGrid()->CopyToClipboard(False, miCopyToClipboardIncludeHeaders->Checked,
	miCopyToClipboardRowHeadersAll->Checked, miCopyToClipboardColumnHeadersAll->Checked);
}
//---------------------------------------------------------------------------

void __fastcall TfrmDemoBasicMain::Action1Execute(TObject *Sender)
{
  CopyToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TfrmDemoBasicMain::miCopyToClipboardIncludeHeadersClick(TObject *Sender)
{
  miCopyToClipboardColumnHeaders->Enabled = miCopyToClipboardIncludeHeaders->Checked;
  miCopyToClipboardRowHeaders->Enabled = miCopyToClipboardIncludeHeaders->Checked;
}
//---------------------------------------------------------------------------


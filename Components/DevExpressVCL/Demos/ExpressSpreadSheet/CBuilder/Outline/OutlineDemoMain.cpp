//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OutlineDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCore"
#pragma link "dxCoreClasses"
#pragma link "dxHashUtils"
#pragma link "dxSpreadSheet"
#pragma link "dxSpreadSheetClasses"
#pragma link "dxSpreadSheetCore"
#pragma link "dxSpreadSheetFormulas"
#pragma link "dxSpreadSheetFunctions"
#pragma link "dxSpreadSheetGraphics"
#pragma link "dxSpreadSheetTypes"
#pragma link "dxSpreadSheetCoreHistory"
#pragma link "dxSpreadSheetPrinting"
#pragma link "dxSpreadSheetFormulaBar"
#pragma link "cxSplitter"
#pragma resource "*.dfm"
TfrmOutline *frmOutline;
//---------------------------------------------------------------------------
__fastcall TfrmOutline::TfrmOutline(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::FormCreate(TObject *Sender)
{
  TfmBaseForm::FormCreate(Sender);
  SpreadSheet->LoadFromFile("..\\..\\Data\\OutlineGrouping.xlsx");
  WindowState = wsMaximized;
  UpdateExpandButtonState();
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmOutline::GetSpreadSheet()
{
  return SpreadSheet;
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::miGroupColumnsClick(TObject *Sender)
{
   if (SpreadSheet->ActiveSheetAsTable->Selection->Count > 0)
      SpreadSheet->ActiveSheetAsTable->Columns->Groups->Add(
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Left,
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Right);
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::miGroupRowsClick(TObject *Sender)
{
   if (SpreadSheet->ActiveSheetAsTable->Selection->Count > 0)
	  SpreadSheet->ActiveSheetAsTable->Rows->Groups->Add(
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Top,
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Bottom);
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::miUngroupColumnsClick(TObject *Sender)
{
   if (SpreadSheet->ActiveSheetAsTable->Selection->Count > 0)
      SpreadSheet->ActiveSheetAsTable->Columns->Groups->Delete(
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Left,
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Right);
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::miUngroupRowsClick(TObject *Sender)
{
   if (SpreadSheet->ActiveSheetAsTable->Selection->Count > 0)
      SpreadSheet->ActiveSheetAsTable->Rows->Groups->Delete(
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Top,
		 SpreadSheet->ActiveSheetAsTable->Selection->Area.Bottom);
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::miGroupFinishClick(TObject *Sender)
{
  SpreadSheet->ActiveSheetAsTable->Rows->Groups->ExpandButtonPosition =
    (TdxSpreadSheetTableItemGroupExpandButtonPosition)((TComponent *)Sender)->Tag;
  SpreadSheet->ActiveSheetAsTable->Columns->Groups->ExpandButtonPosition =
    (TdxSpreadSheetTableItemGroupExpandButtonPosition)((TComponent *)Sender)->Tag;
  UpdateExpandButtonState();
}
//---------------------------------------------------------------------------
void __fastcall TfrmOutline::UpdateExpandButtonState()
{
	miGroupStart->Checked = SpreadSheet->ActiveSheetAsTable->Rows->Groups->ExpandButtonPosition == gebpGroupStart;
	miGroupFinish->Checked = SpreadSheet->ActiveSheetAsTable->Rows->Groups->ExpandButtonPosition == gebpGroupFinish;
}


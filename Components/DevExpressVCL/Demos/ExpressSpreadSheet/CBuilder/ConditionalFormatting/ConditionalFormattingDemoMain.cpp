//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ConditionalFormattingDemoMain.h"
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
#pragma link "dxSpreadSheetConditionalFormatting"
#pragma link "dxSpreadSheetConditionalFormattingRules"
#pragma link "dxSpreadSheetConditionalFormattingRulesManagerDialog"
#pragma link "dxSpreadSheetFormulaBar"
#pragma link "cxSplitter"
#pragma resource "*.dfm"
TfrmConditionalFormatting *frmConditionalFormatting;
//---------------------------------------------------------------------------
__fastcall TfrmConditionalFormatting::TfrmConditionalFormatting(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmConditionalFormatting::FormCreate(TObject *Sender)
{
  TfmBaseForm::FormCreate(Sender);
  SpreadSheet->LoadFromFile("..\\..\\Data\\TopTradingPartners.xlsx");
  WindowState = wsMaximized;
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmConditionalFormatting::GetSpreadSheet()
{
  return SpreadSheet;
}
//---------------------------------------------------------------------------
void __fastcall TfrmConditionalFormatting::miManageRulesClick(TObject *Sender)
{
	ShowConditionalFormattingRulesManagerDialog(SpreadSheet);
}



//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CommentsDemoMain.h"
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
#pragma link "dxSpreadSheetFormulaBar"
#pragma link "cxSplitter"
#pragma resource "*.dfm"
TfrmComments *frmComments;
//---------------------------------------------------------------------------
__fastcall TfrmComments::TfrmComments(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmComments::FormCreate(TObject *Sender)
{
  TfmBaseForm::FormCreate(Sender);
  SpreadSheet->LoadFromFile("..\\..\\Data\\Comments_template.xlsx");
  WindowState = wsMaximized;
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmComments::GetSpreadSheet()
{
  return SpreadSheet;
}



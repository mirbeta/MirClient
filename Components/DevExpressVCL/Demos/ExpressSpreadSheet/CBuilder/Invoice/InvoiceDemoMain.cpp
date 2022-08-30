//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InvoiceDemoMain.h"
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
#pragma link "dxSpreadSheetFormulaBar"
#pragma link "cxSplitter"
#pragma resource "*.dfm"
TfrmInvoice *frmInvoice;
//---------------------------------------------------------------------------
__fastcall TfrmInvoice::TfrmInvoice(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmInvoice::FormCreate(TObject *Sender)
{
  TfmBaseForm::FormCreate(Sender);
  SpreadSheet->LoadFromFile("..\\..\\Data\\Invoice.xlsx");
  WindowState = wsMaximized;
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmInvoice::GetSpreadSheet()
{
  return SpreadSheet;
}
//---------------------------------------------------------------------------


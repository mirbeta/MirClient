//---------------------------------------------------------------------------
#include <vcl.h>
#include <stdlib.h>
#pragma hdrstop

#include "SpreadSheetRLMain.h"
#include "string.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "dxPSBaseGridLnk"
#pragma link "dxPSCore"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxDrawTextUtils"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "DemoBasicMain"
#pragma link "dxmdaset"
#pragma link "cxClasses"
#pragma link "dxPSStdGrLnk"
#pragma link "dxBarBuiltInMenu"
#pragma link "dxCore"
#pragma link "dxCoreClasses"
#pragma link "dxHashUtils"
#pragma link "dxPSdxSpreadSheetLnk"
#pragma link "dxSpreadSheet"
#pragma link "dxSpreadSheetClasses"
#pragma link "dxSpreadSheetCore"
#pragma link "dxSpreadSheetCoreHistory"
#pragma link "dxSpreadSheetFormulas"
#pragma link "dxSpreadSheetFunctions"
#pragma link "dxSpreadSheetGraphics"
#pragma link "dxSpreadSheetPrinting"
#pragma link "dxSpreadSheetTypes"
#pragma resource "*.dfm"
TSpreadSheetRLForm *SpreadSheetRLForm;
//---------------------------------------------------------------------------
__fastcall TSpreadSheetRLForm::TSpreadSheetRLForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
//
}

TdxSpreadSheetCell* TSpreadSheetRLForm::GetCell(int ARow, int AColumn)
{
  return SpreadSheet->ActiveSheetAsTable->Cells[ARow][AColumn];
}

TdxSpreadSheet* TSpreadSheetRLForm::GetSpreadSheet()
{
  return dxSpreadSheet1;
}

void __fastcall TSpreadSheetRLForm::actPreviewExecute(TObject* Sender)
{
  dxComponentPrinter->ReportLink[0]->Preview();
}

void __fastcall TSpreadSheetRLForm::actPrintExecute(TObject* Sender)
{
  dxComponentPrinter->ReportLink[0]->Print(true, NULL);
}

void __fastcall TSpreadSheetRLForm::actSetPrintAreaExecute(TObject* Sender)
{
  String sdxInvalidPrintArea;
  sdxInvalidPrintArea = "You've selected a single cell for print area.\r\rIf this is correct, click OK.\r";
  sdxInvalidPrintArea += "If you selected single cell by mistake, click Cancel, select the cell you want to include, and then click Set Print Area again.";

  TRect R = SpreadSheet->ActiveSheetAsTable->Selection->Area;
  if (((R.Right != R.Left) || (R.Bottom != R.Top)) || (MessageDlg(sdxInvalidPrintArea, mtWarning, TMsgDlgButtons() << mbOK << mbCancel, 0) == mrOk))
  {
	((TdxSpreadSheetReportLnk*)dxComponentPrinter->ReportLink[0])->PrintArea = R;
  }
}

void __fastcall TSpreadSheetRLForm::actClearPrintAreaExecute(TObject* Sender)
{
  ((TdxSpreadSheetReportLnk*)dxComponentPrinter->ReportLink[0])->ClearPrintArea();
}

void __fastcall TSpreadSheetRLForm::actPrintSetupExecute(TObject* Sender)
{
  dxComponentPrinter->ReportLink[0]->PageSetup();
}

void __fastcall TSpreadSheetRLForm::FormCreate(TObject* Sender)
{
  TResourceStream* ASource = new TResourceStream((int)HInstance, "DXSPREADSHEETTEMPLATE", PChar(RT_RCDATA));
  try {
	SpreadSheet->LoadFromStream(ASource);
  }
  __finally {
	delete ASource;
  }
 miFormat->Visible = false;  // todo: errors in genercs collection when add format cells dialog unit
}

void __fastcall TSpreadSheetRLForm::actOpenExecute(TObject* Sender)
{
  OpenDialog->Filter = dxSpreadSheetFormatsRepository()->GetOpenDialogFilter();
  if (OpenDialog->Execute()) {
	SpreadSheet->LoadFromFile(OpenDialog->FileName);
  }
}

void __fastcall TSpreadSheetRLForm::actSaveExecute(TObject* Sender)
{
  SaveDialog->Filter = dxSpreadSheetFormatsRepository()->GetSaveDialogFilter();
  if (SaveDialog->Execute()) {
	SpreadSheet->SaveToFile(SaveDialog->FileName);
  }
}

void __fastcall TSpreadSheetRLForm::actCutExecute(TObject* Sender)
{
  SpreadSheet->ActiveSheetAsTable->CutToClipboard();
}

void __fastcall TSpreadSheetRLForm::actCopyExecute(TObject* Sender)
{
  SpreadSheet->ActiveSheetAsTable->CopyToClipboard();
}

void __fastcall TSpreadSheetRLForm::actPasteExecute(TObject* Sender)
{
  SpreadSheet->ActiveSheetAsTable->PasteFromClipboard();
}

void __fastcall TSpreadSheetRLForm::actExitExecute(TObject* Sender)
{
  Application->Terminate();
}

void __fastcall TSpreadSheetRLForm::actDeleteCellsExecute(TObject* Sender)
{
  SpreadSheet->ActiveSheetAsTable->DeleteCells();
}

void __fastcall TSpreadSheetRLForm::actInsertCellsExecute(TObject* Sender)
{
  SpreadSheet->ActiveSheetAsTable->InsertCells();
}
void __fastcall TSpreadSheetRLForm::dxSpreadSheet1ActiveCellChanging(TdxSpreadSheetTableView* Sender,
  const TPoint ANewActiveCell, bool &ACanSelect)
{
  TdxSpreadSheetCell* ACell = GetCell(ANewActiveCell.y, ANewActiveCell.x);
  if (ACell != NULL) {
	if (ACell->IsFormula) {
	  edtCellEdit->Text = ACell->AsFormula->AsText;
	}
	else {
	  edtCellEdit->Text = ACell->AsString;
	}
  }
  else {
	edtCellEdit->Text = "";
  }
}

void __fastcall TSpreadSheetRLForm::edtCellEditChange(TObject* Sender)
{
  if (!edtCellEdit->Focused()) return;
  TRect R = SpreadSheet->ActiveSheetAsTable->Selection->Area;
  SpreadSheet->ActiveSheetAsTable->CreateCell(R.Top, R.Left)->SetText(edtCellEdit->Text, true);
}

void  __fastcall TSpreadSheetRLForm::actFormatCellsExecute(TObject* Sender)
{
//
}





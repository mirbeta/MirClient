//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ReportPreviewUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCore"
#pragma link "dxCoreClasses"
#pragma link "dxHashUtils"
#pragma link "dxSpreadSheet"
#pragma link "dxSpreadSheetClasses"
#pragma link "dxSpreadSheetConditionalFormatting"
#pragma link "dxSpreadSheetConditionalFormattingRules"
#pragma link "dxSpreadSheetContainers"
#pragma link "dxSpreadSheetCore"
#pragma link "dxSpreadSheetCoreHistory"
#pragma link "dxSpreadSheetFormulas"
#pragma link "dxSpreadSheetFunctions"
#pragma link "dxSpreadSheetGraphics"
#pragma link "dxSpreadSheetHyperlinks"
#pragma link "dxSpreadSheetPrinting"
#pragma link "dxSpreadSheetTypes"
#pragma link "dxSpreadSheetUtils"
#pragma resource "*.dfm"

TfrmPreview *Preview;
//---------------------------------------------------------------------------
__fastcall TfrmPreview::TfrmPreview(TComponent* Owner)
	: TForm(Owner)
{
}

inline __fastcall TfrmPreview::~TfrmPreview()
{
  Preview = NULL;
}

//---------------------------------------------------------------------------
void __fastcall TfrmPreview::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
  Preview = NULL;
}
//---------------------------------------------------------------------------


void __fastcall TfrmPreview::miCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmPreview::miShowFormulasClick(TObject *Sender)
{
  ssResult->BeginUpdate();
  try {
	for (int i = 0; i < ssResult->SheetCount; i++) {
	  ((TdxSpreadSheetTableView*)ssResult->Sheets[i])->Options->ShowFormulas = bDefault;
	}
	ssResult->OptionsView->ShowFormulas = !ssResult->OptionsView->ShowFormulas;
  }
  __finally {
	ssResult->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmPreview::miSaveAsClick(TObject *Sender)
{
  String AFileName;
  TdxSpreadSheetCustomFormatClass AFormatClass;
  sveDialog->Filter = dxSpreadSheetFormatsRepository()->GetSaveDialogFilter();
  if (sveDialog->Execute()) {
	AFileName = sveDialog->FileName;
	if (ExtractFileExt(AFileName) == "") {
	  if (sveDialog->FilterIndex > 0)
		AFileName = AFileName + dxSpreadSheetFormatsRepository()->Exts[sveDialog->FilterIndex - 1];
	  else
		AFileName = AFileName + ".xlsx";
	}
	ssResult->SaveToFile(AFileName);
  }
}
//---------------------------------------------------------------------------


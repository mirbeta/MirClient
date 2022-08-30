// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include "PivotGridRLMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxPivotGrid"
#pragma link "dxPSCore"
#pragma link "dxPScxCommon"
#pragma link "dxPScxPivotGridLnk"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "cxDrawTextUtils"
#pragma link "DemoBasicMain"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma resource "*.dfm"
TPivotGridRLMainForm *PivotGridRLMainForm;

// ---------------------------------------------------------------------------
__fastcall TPivotGridRLMainForm::TPivotGridRLMainForm(TComponent* Owner)
: TDemoBasicMainForm(Owner) {
}

// ---------------------------------------------------------------------------
void __fastcall TPivotGridRLMainForm::FormCreate(TObject *Sender) {

	TDemoBasicMainForm::FormCreate(Sender);
#if defined(_DELPHI_STRING_UNICODE)
	PChar S = PChar(WideString("PIVOTDATA"));
#else
	PChar S = "PIVOTDATA";
#endif
	int AInstance;
	TStream * AStream = new TFileStream("PIVOTPREVIEWDATA.DAT", fmOpenRead);
	try {
		AStream->Position = 0;
		PivotGrid->DataController->LoadFromStream(AStream);
	}
	__finally {
		delete AStream;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TPivotGridRLMainForm::pgfPaymentTypeGetGroupImageIndex
(TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
	int &AImageIndex, TAlignment &AImageAlignHorz, TcxAlignmentVert &AImageAlignVert) {
	AnsiString Card = ((TcxPivotGridViewDataItem*)AItem)->Value;
	if (SameText(Card, "AmEx"))
		AImageIndex = 0;
	else if (SameText(Card, "Cash"))
		AImageIndex = 1;
	else if (SameText(Card, "Master"))
		AImageIndex = 2;
	else if (SameText(Card, "Visa"))
		AImageIndex = 3;
}
// ---------------------------------------------------------------------------

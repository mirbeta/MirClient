//---------------------------------------------------------------------------
#define STRICT
#include <vcl.h>
#pragma hdrstop

#include "TeeChartRLMain.h"
#include <stdlib.h>
#include <shellAPI.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxPSCore"
#pragma link "dxPSDBTCLnk"
#pragma link "dxPSTCLnk"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSGraphicLnk"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "cxDrawTextUtils"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "cxGraphics"
#pragma link "DemoBasicMain"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TTeeChartRLMainForm *TeeChartRLMainForm;
//---------------------------------------------------------------------------
__fastcall TTeeChartRLMainForm::TTeeChartRLMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TTeeChartRLMainForm::PageControl1Change(TObject *Sender)
{
  dxComponentPrinter->CurrentLink = dxComponentPrinter->ReportLink[((TPageControl*)Sender)->ActivePage->PageIndex];
  TeeChart->AnimatedZoom = true;
  TeeChart->AnimatedZoomSteps = 4;
  for (int t=0; t < TeeChart->SeriesCount(); t++)
    TeeChart->Series[t]->FillSampleValues(TeeChart->Series[t]->NumSampleValues());
  TeeChart->UndoZoom();
}
//---------------------------------------------------------------------------

void __fastcall TTeeChartRLMainForm::FormCreate(TObject *Sender)
{
 TDemoBasicMainForm::FormCreate(Sender);
 mdTeeChart->LoadFromBinaryFile("..\\..\\Data\\DBTreeChart.dat");
 mdTeeChart->Open();
 PageControl1Change(PageControl1);
}
//---------------------------------------------------------------------------


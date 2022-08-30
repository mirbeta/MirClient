//---------------------------------------------------------------------------
#define STRICT
#include <vcl.h>
#pragma hdrstop

#include "FlowChartRLMain.h"
#include <stdlib.h>
#include <shellAPI.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxPSCore"
#pragma link "dxPSdxFCLnk"
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
#pragma link "DemoBasicMain"
#pragma link "cxGraphics"
#pragma resource "*.dfm"
#pragma link "dxflchrt"
TFlowChartRLMainForm *FlowChartRLMainForm;
//---------------------------------------------------------------------------
__fastcall TFlowChartRLMainForm::TFlowChartRLMainForm(TComponent* Owner)
    : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

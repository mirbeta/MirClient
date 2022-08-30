//---------------------------------------------------------------------------
#define STRICT
#include <vcl.h>
#pragma hdrstop

#include "OrgChartRLMain.h"
#include <stdlib.h>
#include <shellAPI.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxPSCore"
#pragma link "dxPSdxDBOCLnk"
#pragma link "dxPSdxOCLnk"
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
#pragma link "cxControls"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
#pragma link "dxorgchr"
#pragma link "dxdborgc"
TOrgChartRLMainForm *OrgChartRLMainForm;
//---------------------------------------------------------------------------
__fastcall TOrgChartRLMainForm::TOrgChartRLMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TOrgChartRLMainForm::PageControl1Change(TObject *Sender)
{
  dxComponentPrinter->CurrentLink = dxComponentPrinter->ReportLink[PageControl1->ActivePage->PageIndex];
}
//---------------------------------------------------------------------------

void __fastcall TOrgChartRLMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  Table1->LoadFromFile("..\\..\\Data\\OCDEMO.xml");
  Table1->Open();
  dxOrgChart->FullExpand();
  dxDBOrgChart->FullExpand();
  PageControl1Change(PageControl1);
}
//---------------------------------------------------------------------------

void __fastcall TOrgChartRLMainForm::dxDBOrgChartCreateNode(TObject *Sender,
	  TdxOcNode *Node)
{
	if (Table1->FindField("width")->AsInteger > 50)
	  Node->Width = Table1->FindField("width")->AsInteger;
	if (Table1->FindField("height")->AsInteger > 50)
	  Node->Height = Table1->FindField("height")->AsInteger;
	Node->Shape = GetShape(Table1->FindField("type")->AsString);
	Node->Color = (TColor)Table1->FindField("color")->AsInteger;
	Node->ImageAlign = GetImageAlign(Table1->FindField("ImageAlign")->AsString);
}
//---------------------------------------------------------------------------
TdxOcShape __fastcall TOrgChartRLMainForm::GetShape(AnsiString ShapeName)
{
  const AnsiString ShapeArray[] = {"Rectangle", "Round Rect", "Ellipse", "Diamond"};
  for (int i = 0; i<4; i++)
	if (AnsiUpperCase(ShapeArray[i]) == AnsiUpperCase(ShapeName))
	  return TdxOcShape(i);
};
//---------------------------------------------------------------------------
TdxOcImageAlign __fastcall TOrgChartRLMainForm::GetImageAlign(AnsiString AlignName)
{
   const AnsiString AlignArray[] = {
   "None",
   "Left-Top", "Left-Center", "Left-Bottom",
   "Right-Top", "Right-Center", "Right-Bottom",
   "Top-Left", "Top-Center", "Top-Right",
   "Bottom-Left", "Bottom-Center", "Bottom-Right"
   };
  for(int i=0; i< 13; i++)
	if (AnsiUpperCase(AlignArray[i]) == AnsiUpperCase(AlignName))
	  return(TdxOcImageAlign(i));
  return(TdxOcImageAlign(0));
};






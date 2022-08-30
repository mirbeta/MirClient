//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
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
TfrmCustomDraw *frmCustomDraw;
//---------------------------------------------------------------------------
__fastcall TfrmCustomDraw::TfrmCustomDraw(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::FormCreate(TObject *Sender)
{
  TfmBaseForm::FormCreate(Sender);
  SpreadSheet->LoadFromFile("..\\..\\Data\\CalculatorOfArea.xlsx");
  WindowState = wsMaximized;
}
//---------------------------------------------------------------------------

TdxSpreadSheet* TfrmCustomDraw::GetSpreadSheet()
{
  return SpreadSheet;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomDraw::SpreadSheetCustomDrawTableViewCell(TdxSpreadSheetTableView *Sender, TcxCanvas *ACanvas, TdxSpreadSheetTableViewCellViewInfo *AViewInfo,
		  bool &AHandled)
{
  if ((AViewInfo->Cell == NULL)||(AViewInfo->Cell->DataType != cdtFormula))
	return;
  if ((AViewInfo->Cell->AsFormula->ErrorCode != ecNone)||(AViewInfo->Cell->AsFormula->ResultValue->ErrorCode != ecNone)||
	(!(VarIsNumeric(AViewInfo->Cell->AsFormula->Value))||(AViewInfo->Cell->AsFormula->Value < 0)))
  {
	ACanvas->Brush->Color = clRed;
	ACanvas->Font->Color = clAqua;
	DrawCallout(ACanvas, AViewInfo);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmCustomDraw::DrawCallout(TcxCanvas* ACanvas, TdxSpreadSheetTableViewCellViewInfo* AViewInfo)
{
	TcxRegion *AClipRgn, *AFrameRgn;
	TPoint FramePoints[6], TL;

	TL.x = AViewInfo->Bounds.left;
	TL.y = AViewInfo->Bounds.top;
	FramePoints[0] = cxPointOffset(TL, 0, 10);
	FramePoints[1] = cxPointOffset(FramePoints[0], 5, -10);
	FramePoints[2] = cxPointOffset(FramePoints[1], 200, 0);
	FramePoints[3] = cxPointOffset(FramePoints[2], 0, -20);
	FramePoints[4] = cxPointOffset(FramePoints[3], -205, 0);
	FramePoints[5] = FramePoints[0];

	AFrameRgn = new TcxRegion();
	AFrameRgn->Handle = CreatePolygonRgn(FramePoints, 6, WINDING);
	ACanvas->SaveState();
	AClipRgn = new TcxRegion(AViewInfo->ViewInfo->CellsArea);
	AClipRgn->Combine(AFrameRgn, roIntersect, false);
	ACanvas->SetClipRegion(AClipRgn, roSet, false);
	FillRegionByColor(ACanvas->Handle, AFrameRgn->Handle, clInfoBk);
	FrameRgn(ACanvas->Handle, AFrameRgn->Handle, static_cast<HBRUSH__*>(GetStockObject(BLACK_BRUSH)), 1, 1);
	ACanvas->Font->Name = "Tahoma";
	ACanvas->Font->Size = 10;
	ACanvas->Font->Color = clBlack;
	ACanvas->Font->Style = TFontStyles() << fsBold;
	ACanvas->Brush->Style = bsClear;
	ACanvas->DrawTexT("Incorrect parameter values", cxRect(FramePoints[4], FramePoints[2]), cxAlignVCenter || cxAlignHCenter || cxSingleLine);
	ACanvas->RestoreState();
	ACanvas->SetClipRegion(AClipRgn, roSubtract);
	AFrameRgn->Free();
}
//---------------------------------------------------------------------------


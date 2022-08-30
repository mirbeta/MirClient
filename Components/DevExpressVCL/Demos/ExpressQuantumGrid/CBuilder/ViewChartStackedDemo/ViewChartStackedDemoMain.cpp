//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewChartStackedDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxmdaset"
#pragma link "cxGridCardView"
#pragma resource "*.dfm"
TfrmMain *frmMain;

const
  String Description[4] =
   {"Stacked %s Diagram", "Full Stacked %s Diagram",
	"Side-By-Side Stacked %s Diagram", "Side-By-Side Full Stacked %s Diagram"};

  String Orientation[2] = {"Bars", "Columns"};

bool CheckPrefix(const String APrefix, TObject *AMenuItem)
{
	return Pos(UpperCase(APrefix), UpperCase(dynamic_cast<TComponent*>(AMenuItem)->Name)) == 1;
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner): TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
TcxGridChartView* TfrmMain::GetActiveChart()
{
  if (dynamic_cast<TcxGridChartView*>(grMain->ActiveView) == 0)
	return NULL;
  else
	return (TcxGridChartView*)grMain->ActiveView;
}

TcxGridChartHistogram* TfrmMain::GetActiveHistogram()
{
  if (dynamic_cast<TcxGridChartHistogram*>(ActiveChart->ActiveDiagram) == 0)
	return NULL;
  else
	return (TcxGridChartHistogram*)ActiveChart->ActiveDiagram;
}

TcxGridChartHistogramAxis* TfrmMain::GetActiveAxis(TObject* AMenuItem)
{
	if (CheckPrefix("miCategoryAxis", AMenuItem))
	{
		return ActiveHistogram->AxisCategory;
	}
	else
		if (CheckPrefix("miValueAxis", AMenuItem))
		{
			return ActiveHistogram->AxisValue;
		}
		else
			return NULL;
}

void TfrmMain::SetCheckedSubItem(TMenuItem *AParent, int ACheckedTag)
{
  for (int i = 0; i < AParent->Count; i++)
	AParent->Items[i]->Checked = (AParent->Items[i]->Tag == ACheckedTag);
}

void TfrmMain::UpdateControls()
{
  TcxGridChartColumnDiagram *ColumnDiagram = NULL;
  TcxGridChartLineDiagram *LineDiagram = NULL;
  TcxGridChartStackedColumnDiagram *StackedColumnDiagram = NULL;
  TcxGridChartStackedAreaDiagram *StackedAreaDiagram = NULL;
  TcxGridChartView* AChart = ActiveChart;
  miView->Enabled = AChart != NULL;
  miDiagram->Enabled = miView->Enabled;

  if (AChart == NULL) return;

  if (dynamic_cast<TcxGridChartLineDiagram*>(ActiveHistogram) !=0)
	LineDiagram = (TcxGridChartLineDiagram*)ActiveHistogram;
  if (dynamic_cast<TcxGridChartColumnDiagram*>(ActiveHistogram) !=0)
	ColumnDiagram = (TcxGridChartColumnDiagram*)ActiveHistogram;
  if (dynamic_cast<TcxGridChartStackedColumnDiagram*>(ActiveHistogram) !=0)
	StackedColumnDiagram = (TcxGridChartStackedColumnDiagram*)ActiveHistogram;
  else
	if (dynamic_cast<TcxGridChartStackedAreaDiagram*>(ActiveHistogram) !=0)
	  StackedAreaDiagram = (TcxGridChartStackedAreaDiagram*)ActiveHistogram;

  if (StackedColumnDiagram != NULL)
	glBarsChart->Caption = "Population (Chart View) " +
	  Format(Description[(int)StackedColumnDiagram->StackedStyle], ARRAYOFCONST((Orientation[gvBarsChartView->DiagramStackedColumn->Active])));
  else
	 glBarsChart->Caption = "Population (Chart View) " + gvBarsChartView->ActiveDiagram->DisplayText;

  glAreaChart->Caption = "Website Popularity - " + gvAreaChartView->ActiveDiagram->DisplayText;
  miStackedStyle->Visible = StackedColumnDiagram != NULL;
  if (miStackedStyle->Visible) {
	SetCheckedSubItem(miStackedStyle, (int)StackedColumnDiagram->StackedStyle);
  }

  miAreaStackedStyle->Visible = StackedAreaDiagram != NULL;
  if (miAreaStackedStyle->Visible) {
	SetCheckedSubItem(miAreaStackedStyle, (int)StackedAreaDiagram->StackedStyle);
  }

  miTransparency->Visible = StackedAreaDiagram != NULL;
  if (miTransparency->Visible) {
	SetCheckedSubItem(miTransparency, StackedAreaDiagram->Transparency);
  }

  miEmptyPointsMode->Visible = LineDiagram != NULL;
  if (miEmptyPointsMode->Visible) {
	SetCheckedSubItem(miEmptyPointsMode, (int)LineDiagram->EmptyPointsDisplayMode);
  }

  /* view */
  miTitlePosition->Items[(int)AChart->Title->Position]->Checked = true;
  miLegendPosition->Items[(int)AChart->ActiveDiagram->Legend->Position]->Checked = true;
  miLegendBorder->Checked = AChart->ActiveDiagram->Legend->GetBorder() == lbSingle;
  miToolBox->Checked = AChart->ToolBox->Visible != tvNever;
  miToolBoxPosition->Items[(int)AChart->ToolBox->Position]->Checked = true;


  miLineDiagramValueCaptionPosition->Visible = LineDiagram != NULL;
  miValueCaptionPosition->Visible = ColumnDiagram != NULL;
  if (miValueCaptionPosition->Visible)
	miValueCaptionPosition->Items[(int)ColumnDiagram->Values->CaptionPosition]->Checked = true;
  else
	if (miLineDiagramValueCaptionPosition->Visible)
	  miLineDiagramValueCaptionPosition->Items[(int)LineDiagram->Values->CaptionPosition]->Checked = true;

  /* diagram */
  // histogram
  TcxGridChartHistogram* AHistogram = ActiveHistogram;
  miCategoryAxis->Visible = AHistogram != NULL;
  miValueAxis->Visible = AHistogram != NULL;
  if (AHistogram != NULL)
  {
	// category axis
	miCategoryAxisVisible->Checked = AHistogram->AxisCategory->Visible;
	miCategoryAxisGridLines->Checked = AHistogram->AxisCategory->GridLines;
	miCategoryAxisTickMarkKind->Items[(int)AHistogram->AxisCategory->TickMarkKind]->Checked = true;
	miCategoryAxisTickMarkLabels->Checked = AHistogram->AxisCategory->TickMarkLabels;
	miCategoryAxisCategoriesInReverseOrder->Checked = AHistogram->AxisCategory->CategoriesInReverseOrder;
	miCategoryAxisValueAxisAtMaxCategory->Checked = AHistogram->AxisCategory->ValueAxisAtMaxCategory;
	miCategoryAxisValueAxisBetweenCategories->Checked = AHistogram->AxisCategory->ValueAxisBetweenCategories;
	// value axis
	miValueAxisVisible->Checked = AHistogram->AxisValue->Visible;
	miValueAxisGridLines->Checked = AHistogram->AxisValue->GridLines;
	miValueAxisTickMarkKind->Items[(int)AHistogram->AxisValue->TickMarkKind]->Checked = true;
	miValueAxisTickMarkLabels->Checked = AHistogram->AxisValue->TickMarkLabels;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormActivate(TObject *Sender)
{
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::gvChartViewActiveDiagramChanged(
  TcxGridChartView *Sender, TcxGridChartDiagram *ADiagram)
{
  UpdateControls();
}

//---------------------------------------------------------------------------
// we do not use dxOffice11.FillGradientRect because of the problem in C++Builder during linking
void __fastcall FillGradientRectEx(TcxCanvas* ACanvas, const TRect &ARect, TColor AColor1, TColor AColor2, bool AHorizontal)
{
  int AFirstOffset, ALastOffset, APixelSteps, AColorStepsR, AColorStepsG, AColorStepsB;
  TRect R = ARect;

  if (AHorizontal) {
	AFirstOffset = ARect.Left;
	ALastOffset = ARect.Right - 1;
  }
  else
  {
	AFirstOffset = ARect.Top;
	ALastOffset = ARect.Bottom - 1;
  }
  APixelSteps = ALastOffset - AFirstOffset;
  AColorStepsR = GetRValue(AColor2) - GetRValue(AColor1);
  AColorStepsG = GetGValue(AColor2) - GetGValue(AColor1);
  AColorStepsB = GetBValue(AColor2) - GetBValue(AColor1);

  for (int I = AFirstOffset; I <= ALastOffset; I++) {
	if (AHorizontal) {
	  R.Left = I;
	  R.Right = I + 1;
	}
	else
	{
	  R.Top = I;
	  R.Bottom = I + 1;
	}
	ACanvas->Brush->Color = (TColor)RGB(
	  GetRValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsR, APixelSteps),
	  GetGValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsG, APixelSteps),
	  GetBValue(AColor1) + MulDiv(I - AFirstOffset, AColorStepsB, APixelSteps));
	ACanvas->FillRect(R, NULL, False);
  }
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::grMainActiveTabChanged(TcxCustomGrid *Sender,
	  TcxGridLevel *ALevel)
{
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miTitlePositionItemClick(TObject *Sender)
{
  ActiveChart->Title->Position = (TcxGridChartPartPosition)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLegendPositionItemClick(TObject *Sender)
{
  ActiveChart->ActiveDiagram->Legend->Position = (TcxGridChartPartPosition)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLegendBorderClick(TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked)
	ActiveChart->ActiveDiagram->Legend->Border = lbSingle;
  else
	ActiveChart->ActiveDiagram->Legend->Border = lbNone;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAxisVisibleClick(TObject *Sender)
{
  GetActiveAxis(Sender)->Visible = !GetActiveAxis(Sender)->Visible;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAxisGridLinesClick(TObject *Sender)
{
  GetActiveAxis(Sender)->GridLines = !GetActiveAxis(Sender)->GridLines;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAxisTickMarkKindItemClick(TObject *Sender)
{
  GetActiveAxis(Sender)->TickMarkKind = (TcxGridChartHistogramTickMarkKind)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAxisTickMarkLabelsClick(TObject *Sender)
{
  GetActiveAxis(Sender)->TickMarkLabels = !GetActiveAxis(Sender)->TickMarkLabels;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCategoryAxisCategoriesInReverseOrderClick(
	  TObject *Sender)
{
  TcxGridChartHistogramAxisCategory* AAxis = (TcxGridChartHistogramAxisCategory*)GetActiveAxis(Sender);
  AAxis->CategoriesInReverseOrder = !AAxis->CategoriesInReverseOrder;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCategoryAxisValueAxisAtMaxCategoryClick(
	  TObject *Sender)
{
  TcxGridChartHistogramAxisCategory* AAxis = (TcxGridChartHistogramAxisCategory*)GetActiveAxis(Sender);
  AAxis->ValueAxisAtMaxCategory = !AAxis->ValueAxisAtMaxCategory;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCategoryAxisValueAxisBetweenCategoriesClick(
	  TObject *Sender)
{
  TcxGridChartHistogramAxisCategory* AAxis = (TcxGridChartHistogramAxisCategory*)GetActiveAxis(Sender);
  AAxis->ValueAxisBetweenCategories = !AAxis->ValueAxisBetweenCategories;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLineDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramLine->Active = true;
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAreaDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramArea->Active = true;
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miPieDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramPie->Active = true;
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miToolBoxClick(TObject *Sender)
{
  if (ActiveChart->ToolBox->Visible == tvNever)
	ActiveChart->ToolBox->Visible = tvAlways;
  else
	ActiveChart->ToolBox->Visible = tvNever;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miToolBoxPositionClick(TObject *Sender)
{
  ActiveChart->ToolBox->Position = (TcxGridChartToolBoxPosition)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::gvChartViewDiagramStackedBarCustomDrawValue(
 TcxGridChartDiagram *Sender, TcxCanvas *ACanvas,
 TcxGridChartDiagramValueViewInfo *AViewInfo, bool &ADone)
{
  TColor C1 = dxGetLighterColor(AViewInfo->Params.Color, 75);
  FillGradientRectEx(ACanvas, AViewInfo->Bounds, C1, AViewInfo->Params.Color,
	gvBarsChartView->DiagramStackedBar->Active);
  ADone = True;
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::StackedStyleClick(TObject *Sender)
{
  ((TcxGridChartStackedColumnDiagram*)ActiveHistogram)->StackedStyle =
	(TcxGridChartStackedDiagramStyle)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::StackedAreaStyleClick(TObject *Sender)
{
  ((TcxGridChartStackedAreaDiagram*)ActiveHistogram)->StackedStyle =
	(TcxGridChartStackedAreaDiagramStyle)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miValueCaptionPositionItemClick(TObject *Sender)
{
  ((TcxGridChartStackedColumnDiagram*)ActiveHistogram)->Values->CaptionPosition =
	(TcxGridChartColumnDiagramValueCaptionPosition)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLineDiagramValueCaptionPositionItemClick(TObject *Sender)
{
  if(dynamic_cast<TcxGridChartLineDiagram*>(ActiveHistogram) !=0)
	((TcxGridChartLineDiagram*)ActiveHistogram)->Values->CaptionPosition =
		(TcxGridChartLineDiagramValueCaptionPosition)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miTransparencyClick(TObject *Sender)
{
  if(dynamic_cast<TcxGridChartAreaDiagram*>(ActiveHistogram) !=0)
	((TcxGridChartAreaDiagram*)ActiveHistogram)->Transparency = ((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------

void __fastcall TfrmMain::miEmptyPointsDisplayModeClick(TObject *Sender)
{
  if(dynamic_cast<TcxGridChartLineDiagram*>(ActiveHistogram) !=0)
	((TcxGridChartLineDiagram*)ActiveHistogram)->EmptyPointsDisplayMode =
	  (TcxGridChartEmptyPointsDisplayMode)((TMenuItem*)Sender)->Tag;
  UpdateControls();
}

//---------------------------------------------------------------------------


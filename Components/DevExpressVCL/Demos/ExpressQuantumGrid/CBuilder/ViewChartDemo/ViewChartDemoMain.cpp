//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewChartDemoMain.h"
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
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
bool CheckPrefix(const String APrefix, TObject *AMenuItem)
{
	return Pos(UpperCase(APrefix), UpperCase(dynamic_cast<TComponent*>(AMenuItem)->Name)) == 1;
}
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
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

TcxGridChartColumnDiagram* TfrmMain::GetActiveColumnDiagram()
{
  if (dynamic_cast<TcxGridChartColumnDiagram*>(ActiveChart->ActiveDiagram) == 0)
    return NULL;
  else
    return (TcxGridChartColumnDiagram*)ActiveChart->ActiveDiagram;
}

TcxGridChartHistogram* TfrmMain::GetActiveHistogram()
{
  if (dynamic_cast<TcxGridChartHistogram*>(ActiveChart->ActiveDiagram) == 0)
    return NULL;
  else
    return (TcxGridChartHistogram*)ActiveChart->ActiveDiagram;
}

TcxGridChartLineDiagram* TfrmMain::GetActiveLineDiagram()
{
  if (dynamic_cast<TcxGridChartLineDiagram*>(ActiveChart->ActiveDiagram) == 0)
    return NULL;
  else
    return (TcxGridChartLineDiagram*)ActiveChart->ActiveDiagram;
}

TcxGridChartPieDiagram* TfrmMain::GetActivePieDiagram()
{
  if (dynamic_cast<TcxGridChartPieDiagram*>(ActiveChart->ActiveDiagram) == 0)
    return NULL;
  else
    return (TcxGridChartPieDiagram*)ActiveChart->ActiveDiagram;
}

void TfrmMain::UpdateControls()
{
  TcxGridChartView* AChart = ActiveChart;
  miView->Enabled = AChart != NULL;
  miDiagram->Enabled = miView->Enabled;
  if (AChart == NULL) return;

  /* view */
  if (AChart->DiagramColumn->Active)
    miColumnDiagram->Checked = true;
  else
    if (AChart->DiagramBar->Active)
      miBarDiagram->Checked = true;
    else
      if (AChart->DiagramLine->Active)
        miLineDiagram->Checked = true;
      else
        if (AChart->DiagramArea->Active)
          miAreaDiagram->Checked = true;
        else
		  miPieDiagram->Checked = true;

  miTitlePosition->Items[(int)AChart->Title->Position]->Checked = true;
  miLegendPosition->Items[(int)AChart->ActiveDiagram->Legend->Position]->Checked = true;
  miLegendBorder->Checked = AChart->ActiveDiagram->Legend->GetBorder() == lbSingle;
  miToolBox->Checked = AChart->ToolBox->Visible != tvNever;
  miToolBoxPosition->Items[(int)AChart->ToolBox->Position]->Checked = true;

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
  //column
  TcxGridChartColumnDiagram* AColumnDiagram = ActiveColumnDiagram;
  miValueCaptionPosition->Visible = AColumnDiagram != NULL;
  if (AColumnDiagram != NULL)
    miValueCaptionPosition->Items[(int)AColumnDiagram->Values->CaptionPosition]->Checked = true;
  // line
  TcxGridChartLineDiagram* ALineDiagram = ActiveLineDiagram;
  miLineValueCaptionPosition->Visible = ALineDiagram != NULL;
  miLineStyle->Visible = ALineDiagram != NULL;
  miLineWidth->Visible = ALineDiagram != NULL;
  miMarkerStyle->Visible = ALineDiagram != NULL;
  miMarkerSize->Visible = ALineDiagram != NULL;
  miValueStacking->Visible = ALineDiagram != NULL;
  if (ALineDiagram != NULL)
  {
    miLineValueCaptionPosition->Items[(int)ALineDiagram->Values->CaptionPosition]->Checked = true;
    miLineStyle->Items[(int)ALineDiagram->Values->LineStyle]->Checked = true;
    miLineWidth->Find(IntToStr(ALineDiagram->Values->LineWidth))->Checked = true;
    miMarkerStyle->Items[(int)ALineDiagram->Values->MarkerStyle]->Checked = true;
    miMarkerSize->Find(IntToStr(ALineDiagram->Values->MarkerSize))->Checked = true;
    miValueStacking->Items[(int)ALineDiagram->Values->Stacking]->Checked = true;
  }
  // pie
  TcxGridChartPieDiagram* APieDiagram = ActivePieDiagram;
  miSeriesCaptions->Visible = APieDiagram != NULL;
  miSeriesSites->Visible = APieDiagram != NULL;
  miSeriesColumnCount->Visible = APieDiagram != NULL;
  miPieValueCaptionPosition->Visible = APieDiagram != NULL;
  miPieValueCaptionItems->Visible = APieDiagram != NULL;
  if (APieDiagram != NULL)
  {
    miSeriesCaptions->Checked = APieDiagram->SeriesCaptions;
    miSeriesSites->Checked = APieDiagram->SeriesSites;
    miSeriesColumnCount->Enabled = APieDiagram->GetSeriesColumnCount() > 0;
    if (miSeriesColumnCount->Enabled)
      miSeriesColumnCount->Find(IntToStr(APieDiagram->GetSeriesColumnCount()))->Checked = true;
    miPieValueCaptionPosition->Items[(int)APieDiagram->Values->CaptionPosition]->Checked = true;
    for (int i = 0; i < miPieValueCaptionItems->Count; i++)
      miPieValueCaptionItems->Items[i]->Checked =
        APieDiagram->Values->CaptionItems.Contains((TcxGridChartPieDiagramValueCaptionItem)i);
  }
}

void TfrmMain::CalculateSalesInfo()
{
	CalculateSalesInfoForQuarter(1, cdsQuarter1);
	CalculateSalesInfoForQuarter(2, cdsQuarter2);
	CalculateSalesInfoForQuarter(3, cdsQuarter3);
	CalculateSalesInfoForQuarter(4, cdsQuarter4);
}

void TfrmMain::CalculateSalesInfoForQuarter(int AQuarter, TClientDataSet *AQuarterData)
{
  AQuarterData->Active = true;
  chvSalesByQuarter->ViewData->CategoryCount = AQuarterData->RecordCount;

  chvSalesByQuarter->BeginUpdate();
  try {
    int I = 0;
	AQuarterData->First();
	while (!AQuarterData->Eof)
	{
	  chvSalesByQuarter->ViewData->Categories[I] = AQuarterData->FieldValues["Name"];
      chvSalesByQuarter->ViewData->Values[AQuarter - 1][I] = AQuarterData->FieldValues["Amount"];
      AQuarterData->Next();
      I++;
	}
  }  
  __finally
  {
    chvSalesByQuarter->EndUpdate();
  }
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

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  tblCustomers->LoadFromFile(APath + "Customers.xml");
  tblProducts->LoadFromFile(APath + "Products.xml");
  tblOrders->LoadFromFile(APath + "Orders.xml");

  grMainLevel3->Active = true;
  CalculateSalesInfo();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  grMainLevel3->Active = True;
  CalculateSalesInfo();
}

//---------------------------------------------------------------------------


void __fastcall TfrmMain::chvSalesByQuarterGetValueHint(
      TcxGridChartView *Sender, TcxGridChartSeries *ASeries,
      int AValueIndex, String &AHint)
{
  AHint = Format(AnsiString("%s sales for Q%d are %s"),
    ARRAYOFCONST((Sender->ViewData->Categories[AValueIndex], ASeries->Index + 1, ASeries->VisibleDisplayTexts[AValueIndex])));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miAboutClick(TObject *Sender)
{
  ShowAboutDemoForm();
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

void __fastcall TfrmMain::chvSalesSeries1CustomDrawValue(
      TcxGridChartSeries *Sender, TcxCanvas *ACanvas,
      TcxGridChartDiagramValueViewInfo *AViewInfo, bool &ADone)
{
  if (Sender->GridView->DiagramColumn->Active || Sender->GridView->DiagramBar->Active)
  {
    TColor AEndColor;
    if (AViewInfo->State == gcsNone)
      AEndColor = clBlack;
    else
      AEndColor = clGray;
    FillGradientRectEx(ACanvas, AViewInfo->ContentBounds, clWhite, AEndColor,
      Sender->GridView->DiagramColumn->Active);
    ADone = true;
  }  
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::grMainActiveTabChanged(TcxCustomGrid *Sender,
      TcxGridLevel *ALevel)
{
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miColumnDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramColumn->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miBarDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramBar->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miStackedBarDiagramClick(TObject *Sender)
{
  ActiveChart->DiagramStackedBar->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miTitlePositionItemClick(TObject *Sender)
{
  ActiveChart->Title->Position = (TcxGridChartPartPosition)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLegendPositionItemClick(TObject *Sender)
{
  ActiveChart->ActiveDiagram->Legend->Position = (TcxGridChartPartPosition)((TMenuItem*)Sender)->MenuIndex;
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

void __fastcall TfrmMain::miValueCaptionPositionItemClick(
      TObject *Sender)
{
  ActiveColumnDiagram->Values->CaptionPosition =
    (TcxGridChartColumnDiagramValueCaptionPosition)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::chvSalesActiveDiagramChanged(
      TcxGridChartView *Sender, TcxGridChartDiagram *ADiagram)
{
  if (Sender == ActiveChart) UpdateControls();
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
  GetActiveAxis(Sender)->TickMarkKind = (TcxGridChartHistogramTickMarkKind)((TMenuItem*)Sender)->MenuIndex;
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
  ActiveChart->ToolBox->Position = (TcxGridChartToolBoxPosition)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLineStyleClick(TObject *Sender)
{
  ActiveLineDiagram->Values->LineStyle = (TcxGridChartLineStyle)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLineWidthClick(TObject *Sender)
{
  ActiveLineDiagram->Values->LineWidth = StrToInt(((TMenuItem*)Sender)->Caption);
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miMarkerStyleClick(TObject *Sender)
{
  ActiveLineDiagram->Values->MarkerStyle = (TcxGridChartMarkerStyle)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miMarkerSizeClick(TObject *Sender)
{
  ActiveLineDiagram->Values->MarkerSize = StrToInt(((TMenuItem*)Sender)->Caption);
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miValueStackingClick(TObject *Sender)
{
  ActiveLineDiagram->Values->Stacking = (TcxGridChartValuesStacking)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miLineValueCaptionPositionClick(
      TObject *Sender)
{
  ActiveLineDiagram->Values->CaptionPosition =
    (TcxGridChartLineDiagramValueCaptionPosition)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miSeriesCaptionsClick(TObject *Sender)
{
  ActivePieDiagram->SeriesCaptions = !ActivePieDiagram->SeriesCaptions;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miSeriesSitesClick(TObject *Sender)
{
  ActivePieDiagram->SeriesSites = !ActivePieDiagram->SeriesSites;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miSeriesColumnsClick(TObject *Sender)
{
  ActivePieDiagram->SeriesColumnCount = StrToInt(((TMenuItem*)Sender)->Caption);
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miPieValueCaptionPositionClick(
      TObject *Sender)
{
  ActivePieDiagram->Values->CaptionPosition =
    (TcxGridChartPieDiagramValueCaptionPosition)((TMenuItem*)Sender)->MenuIndex;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miPieValueCaptionItemClick(
      TObject *Sender)
{
  TcxGridChartPieDiagramValueCaptionItem ACaptionItem =
    (TcxGridChartPieDiagramValueCaptionItem)((TMenuItem*)Sender)->MenuIndex;
  TcxGridChartPieDiagramValueCaptionItems ACaptionItems = ActivePieDiagram->Values->CaptionItems;
  if (ACaptionItems.Contains(ACaptionItem))
    ACaptionItems >> ACaptionItem;
  else
    ACaptionItems << ACaptionItem;
  ActivePieDiagram->Values->CaptionItems = ACaptionItems;
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormActivate(TObject *Sender)
{
  UpdateControls();  // to update menu after chart view was changed via Customization Form
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::chvSalesDiagramAreaCustomDrawValueArea(
      TcxGridChartAreaDiagram *Sender, TcxCanvas *ACanvas,
      TcxGridChartAreaDiagramValueViewInfo *AViewInfo, bool &ADone)
{
  TcxRegion* AClipRegion = ACanvas->GetClipRegion(true);
  try {
    TcxRegion* ARegion = AViewInfo->CreateAreaRegion();
    try {
      ACanvas->SetClipRegion(ARegion, roIntersect, false, true);
      FillGradientRectEx(ACanvas, AViewInfo->GetRealBounds(), clWhite, clMedGray, true);
    }  
    __finally {
      ARegion->Free();
    }
  }
  __finally {
    ACanvas->SetClipRegion(AClipRegion, roSet, true, true);
  }
  ADone = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::chvSalesValueClick(TcxGridChartView *Sender,
      TcxGridChartSeries *ASeries, int AValueIndex, bool &AHandled)
{
  cdsSales->Locate("Name", Sender->Categories->VisibleValues[AValueIndex], TLocateOptions());
  tblOrders->Locate("ProductID", cdsSales->FieldByName("ID")->AsInteger, TLocateOptions());
  grMainLevel1->Active = true;
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#ifndef ViewChartStackedDemoMainH
#define ViewChartStackedDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBChartView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeelPainters.hpp"
#include "dxmdaset.hpp"
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid *grMain;
	TcxGridDBChartView *gvBarsChartView;
	TcxGridDBChartSeries *cxGridDBChartSeries1;
	TcxGridDBChartSeries *cxGridDBChartSeries2;
	TcxGridDBChartSeries *cxGridDBChartSeries3;
	TcxGridDBChartSeries *cxGridDBChartSeries4;
	TcxGridDBChartSeries *cxGridDBChartSeries5;
	TcxGridDBChartSeries *cxGridDBChartSeries6;
	TcxGridDBTableView *gvBarsTableView;
	TcxGridDBColumn *gvBarsTableViewRecId;
	TcxGridDBColumn *gvBarsTableViewCountry;
	TcxGridDBColumn *gvBarsTableViewMale14;
	TcxGridDBColumn *gvBarsTableViewMale64;
	TcxGridDBColumn *gvBarsTableViewMale65;
	TcxGridDBColumn *gvBarsTableViewFemale14;
	TcxGridDBColumn *gvBarsTableViewFemale64;
	TcxGridDBColumn *gvBarsTableViewFemale65;
	TcxGridDBChartView *gvAreaChartView;
	TcxGridDBChartSeries *gvAreaChartViewSeries1;
	TcxGridDBChartSeries *gvAreaChartViewSeries2;
	TcxGridDBChartSeries *gvAreaChartViewSeries3;
	TcxGridDBTableView *gvAreaTableView;
	TcxGridDBColumn *gvAreaTableViewRecId;
	TcxGridDBColumn *gvAreaTableViewCategory;
	TcxGridDBColumn *gvAreaTableViewPolitics;
	TcxGridDBColumn *gvAreaTableViewEntertainment;
	TcxGridDBColumn *gvAreaTableViewTravel;
	TcxGridLevel *glBarsChart;
	TcxGridLevel *glBarsTable;
	TcxGridLevel *glAreaChart;
	TcxGridLevel *glAreaTable;
	TImageList *PaymentTypeImages;
	TMenuItem *miView;
	TMenuItem *miTitlePosition;
	TMenuItem *miTitlePositionDefault;
	TMenuItem *miTitlePositionNone;
	TMenuItem *miTitlePositionLeft;
	TMenuItem *miTitlePositionTop;
	TMenuItem *miTitlePositionRight;
	TMenuItem *miTitlePositionBottom;
	TMenuItem *N7;
	TMenuItem *miLegendPosition;
	TMenuItem *miLegendPositionDefault;
	TMenuItem *miLegendPositionNone;
	TMenuItem *miLegendPositionLeft;
	TMenuItem *miLegendPositionTop;
	TMenuItem *miLegendPositionRight;
	TMenuItem *miLegendPositionBottom;
	TMenuItem *miLegendBorder;
	TMenuItem *N4;
	TMenuItem *miToolBox;
	TMenuItem *miToolBoxPosition;
	TMenuItem *miToolBoxPositionTop;
	TMenuItem *miToolBoxPositionBottom;
	TMenuItem *miDiagram;
	TMenuItem *miAreaStackedStyle;
	TMenuItem *Default1;
	TMenuItem *N100Percent2;
	TMenuItem *miStackedStyle;
	TMenuItem *Normal1;
	TMenuItem *N100Percent1;
	TMenuItem *SideBySide1;
	TMenuItem *N100PercentSideBySide1;
	TMenuItem *N1;
	TMenuItem *miLineDiagramValueCaptionPosition;
	TMenuItem *mildvcpNone;
	TMenuItem *mildvcpLeft;
	TMenuItem *mildvcpAbove;
	TMenuItem *mildvcpRight;
	TMenuItem *mildvcpBelow;
	TMenuItem *mildvcpCenter;
	TMenuItem *mildvcpAboveRight;
	TMenuItem *mildvcpBelowRight;
	TMenuItem *mildvcpAboveLeft;
	TMenuItem *mildvcpBelowLeft;
	TMenuItem *miValueCaptionPosition;
	TMenuItem *miValueCaptionPositionNone;
	TMenuItem *miValueCaptionPositionInsideBase;
	TMenuItem *miValueCaptionPositionCenter;
	TMenuItem *miValueCaptionPositionInsideEnd;
	TMenuItem *miValueCaptionPositionOutsideEnd;
	TMenuItem *N2;
	TMenuItem *miCategoryAxis;
	TMenuItem *miCategoryAxisVisible;
	TMenuItem *miCategoryAxisGridLines;
	TMenuItem *miCategoryAxisTickMarkKind;
	TMenuItem *miCategoryAxisTickMarkKindNone;
	TMenuItem *miCategoryAxisTickMarkKindCross;
	TMenuItem *miCategoryAxisTickMarkKindInside;
	TMenuItem *miCategoryAxisTickMarkKindOutside;
	TMenuItem *miCategoryAxisTickMarkLabels;
	TMenuItem *N3;
	TMenuItem *miCategoryAxisCategoriesInReverseOrder;
	TMenuItem *miCategoryAxisValueAxisAtMaxCategory;
	TMenuItem *miCategoryAxisValueAxisBetweenCategories;
	TMenuItem *miValueAxis;
	TMenuItem *miValueAxisVisible;
	TMenuItem *miValueAxisGridLines;
	TMenuItem *miValueAxisTickMarkKind;
	TMenuItem *miValueAxisTickMarkKindNone;
	TMenuItem *miValueAxisTickMarkKindCross;
	TMenuItem *miValueAxisTickMarkKindInside;
	TMenuItem *miValueAxisTickMarkKindOutside;
	TMenuItem *miValueAxisTickMarkLabels;
	TMenuItem *N5;
	TMenuItem *miEmptyPointsMode;
	TMenuItem *miepdmZero;
	TMenuItem *miepdmGap;
	TMenuItem *N6;
	TMenuItem *miTransparency;
	TMenuItem *mi0;
	TMenuItem *mi45;
	TMenuItem *mi90;
	TMenuItem *mi135;
	TMenuItem *mi180;
	TMenuItem *mi225;
	TMenuItem *mi255;
	TDataSource *dsSales;
	TdxMemData *dxMemData1;
	TStringField *dxMemData1Country;
	TFloatField *dxMemData1Male14;
	TFloatField *dxMemData1Male64;
	TFloatField *dxMemData1Male65;
	TFloatField *dxMemData1Female14;
	TFloatField *dxMemData1Female64;
	TFloatField *dxMemData1Female65;
	TdxMemData *dxMemData2;
	TDateTimeField *dxMemData2Category;
	TIntegerField *dxMemData2Politics;
	TIntegerField *dxMemData2Entertainment;
	TIntegerField *dxMemData2Travel;
	TDataSource *DataSource1;

	void __fastcall FormActivate(TObject *Sender);
	void __fastcall gvChartViewActiveDiagramChanged(TcxGridChartView *Sender, TcxGridChartDiagram *ADiagram);
	void __fastcall grMainActiveTabChanged(TcxCustomGrid *Sender,
	  TcxGridLevel *ALevel);
	void __fastcall miTitlePositionItemClick(TObject *Sender);
	void __fastcall miLegendPositionItemClick(TObject *Sender);
	void __fastcall miLegendBorderClick(TObject *Sender);
	void __fastcall gvChartViewDiagramStackedBarCustomDrawValue(
	 TcxGridChartDiagram *Sender, TcxCanvas *ACanvas,
	 TcxGridChartDiagramValueViewInfo *AViewInfo, bool &ADone);
	void __fastcall StackedAreaStyleClick(TObject *Sender);
	void __fastcall StackedStyleClick(TObject *Sender);
	void __fastcall miAxisVisibleClick(TObject *Sender);
	void __fastcall miAxisGridLinesClick(TObject *Sender);
	void __fastcall miAxisTickMarkKindItemClick(TObject *Sender);
	void __fastcall miAxisTickMarkLabelsClick(TObject *Sender);
	void __fastcall miCategoryAxisCategoriesInReverseOrderClick(
	  TObject *Sender);
	void __fastcall miCategoryAxisValueAxisAtMaxCategoryClick(
	  TObject *Sender);
	void __fastcall miCategoryAxisValueAxisBetweenCategoriesClick(
	  TObject *Sender);
	void __fastcall miLineDiagramClick(TObject *Sender);
	void __fastcall miAreaDiagramClick(TObject *Sender);
	void __fastcall miPieDiagramClick(TObject *Sender);
	void __fastcall miToolBoxClick(TObject *Sender);
	void __fastcall miToolBoxPositionClick(TObject *Sender);
	void __fastcall miValueCaptionPositionItemClick(TObject *Sender);
	void __fastcall miLineDiagramValueCaptionPositionItemClick(TObject *Sender);
	void __fastcall miTransparencyClick(TObject *Sender);
	void __fastcall miEmptyPointsDisplayModeClick(TObject *Sender);
protected:
	void UpdateControls();
	void SetCheckedSubItem(TMenuItem *AParent, int ACheckedTag);
	TcxGridChartView* GetActiveChart();
	TcxGridChartHistogram* GetActiveHistogram();
	TcxGridChartHistogramAxis* TfrmMain::GetActiveAxis(TObject* AMenuItem);
public:
	__fastcall TfrmMain(TComponent* Owner);
	__property TcxGridChartView* ActiveChart = {read = GetActiveChart};
	__property TcxGridChartHistogram* ActiveHistogram = {read = GetActiveHistogram};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif

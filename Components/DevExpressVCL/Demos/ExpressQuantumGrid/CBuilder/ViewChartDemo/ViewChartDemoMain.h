//---------------------------------------------------------------------------

#ifndef ViewChartDemoMainH
#define ViewChartDemoMainH
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
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include <ComCtrls.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
        TDataSource *dsOrders;
		TClientDataSet *tblOrders;
		TAutoIncField *tblOrdersID;
		TIntegerField *tblOrdersCustomerID;
		TIntegerField *tblOrdersProductID;
		TDateTimeField *tblOrdersPurchaseDate;
		TDateTimeField *tblOrdersTime;
		TStringField *tblOrdersPaymentType;
		TCurrencyField *tblOrdersPaymentAmount;
		TMemoField *tblOrdersDescription;
		TIntegerField *tblOrdersQuantity;
		TStringField *tblOrdersProductName;
		TStringField *tblOrdersCustomer;
		TClientDataSet *cdsQuarter1;
		TClientDataSet *cdsQuarter2;
		TClientDataSet *cdsQuarter3;
		TClientDataSet *cdsQuarter4;
		TClientDataSet *cdsSales;
		TDataSource *dsSales;
		TClientDataSet *tblProducts;
		TClientDataSet *tblCustomers;
        TImageList *PaymentTypeImages;
        TMenuItem *miView;
        TMenuItem *miColumnDiagram;
        TMenuItem *miBarDiagram;
        TMenuItem *miLineDiagram;
        TMenuItem *miAreaDiagram;
        TMenuItem *miPieDiagram;
        TMenuItem *N1;
        TMenuItem *miTitlePosition;
        TMenuItem *miTitlePositionDefault;
        TMenuItem *miTitlePositionNone;
        TMenuItem *miTitlePositionLeft;
        TMenuItem *miTitlePositionTop;
        TMenuItem *miTitlePositionRight;
        TMenuItem *miTitlePositionBottom;
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
        TMenuItem *miLineStyle;
        TMenuItem *miLineStyleNone;
        TMenuItem *miLineStyleSolid;
        TMenuItem *miLineStyleDash;
        TMenuItem *miLineStyleDot;
        TMenuItem *miLineStyleDashDot;
        TMenuItem *miLineStyleDashDotDot;
        TMenuItem *miLineWidth;
        TMenuItem *miLineWidth1;
        TMenuItem *miLineWidth2;
        TMenuItem *miLineWidth3;
        TMenuItem *miLineWidth4;
        TMenuItem *miLineWidth5;
        TMenuItem *miMarkerStyle;
        TMenuItem *miMarkerStyleNone;
        TMenuItem *miMarkerStyleSquare;
        TMenuItem *miMarkerStyleTriangle;
        TMenuItem *miMarkerStyleDiamond;
        TMenuItem *miMarkerStyleCircle;
        TMenuItem *miMarkerSize;
        TMenuItem *miMarkerSize3;
        TMenuItem *miMarkerSize5;
        TMenuItem *miMarkerSize7;
        TMenuItem *miMarkerSize9;
        TMenuItem *miMarkerSize11;
        TMenuItem *miValueStacking;
        TMenuItem *miValueStackingNone;
        TMenuItem *miValueStackingNormal;
        TMenuItem *miValueStacking100Percent;
        TMenuItem *miValueCaptionPosition;
        TMenuItem *miValueCaptionPositionNone;
        TMenuItem *miValueCaptionPositionInsideBase;
        TMenuItem *miValueCaptionPositionCenter;
        TMenuItem *miValueCaptionPositionInsideEnd;
        TMenuItem *miValueCaptionPositionOutsideEnd;
        TMenuItem *miLineValueCaptionPosition;
        TMenuItem *miLineValueCaptionPositionNone;
        TMenuItem *miLineValueCaptionPositionLeft;
        TMenuItem *miLineValueCaptionPositionAbove;
        TMenuItem *miLineValueCaptionPositionRight;
        TMenuItem *miLineValueCaptionPositionBelow;
        TMenuItem *miLineValueCaptionPositionCenter;
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
        TMenuItem *miSeriesCaptions;
        TMenuItem *miSeriesSites;
        TMenuItem *miSeriesColumnCount;
        TMenuItem *miSeriesColumns1;
        TMenuItem *miSeriesColumns2;
        TMenuItem *miSeriesColumns3;
        TMenuItem *miSeriesColumns4;
        TMenuItem *miPieValueCaptionPosition;
        TMenuItem *miPieValueCaptionPositionNone;
        TMenuItem *miPieValueCaptionPositionCenter;
        TMenuItem *miPieValueCaptionPositionInsideEnd;
        TMenuItem *miPieValueCaptionPositionOutsideEnd;
        TMenuItem *miPieValueCaptionPositionOutsideEndWithLeaderLines;
        TMenuItem *miPieValueCaptionItems;
        TMenuItem *miPieValueCaptionItemCategory;
        TMenuItem *miPieValueCaptionItemValue;
        TMenuItem *miPieValueCaptionItemPercentage;
        TcxStyleRepository *cvStyleRepository1;
		TcxStyle *cvStyle1;
		TcxStyle *cvStyle2;
		TcxStyle *cvStyle3;
		TcxStyle *cvStyle4;
		TcxStyle *cvStyle5;
		TcxStyle *cvStyle6;
        TcxStyle *cvStyle7;
        TcxGrid *grMain;
        TcxGridDBTableView *tvData;
        TcxGridDBColumn *tvDataID;
        TcxGridDBColumn *tvDataProductName;
        TcxGridDBColumn *tvDataCompany;
        TcxGridDBColumn *tvDataPurchaseDate;
        TcxGridDBColumn *tvDataPaymentType;
        TcxGridDBColumn *tvDataPaymentAmount;
        TcxGridDBColumn *tvDataQuantity;
        TcxGridDBChartView *chvSales;
        TcxGridDBChartSeries *chvSalesSeries1;
        TcxGridChartView *chvSalesByQuarter;
        TcxGridChartSeries *chvSalesByQuarterSeries1;
        TcxGridChartSeries *chvSalesByQuarterSeries2;
        TcxGridChartSeries *chvSalesByQuarterSeries3;
        TcxGridChartSeries *chvSalesByQuarterSeries4;
        TcxGridLevel *grMainLevel1;
        TcxGridLevel *grMainLevel2;
        TcxGridLevel *grMainLevel3;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall chvSalesByQuarterGetValueHint(
          TcxGridChartView *Sender, TcxGridChartSeries *ASeries,
          int AValueIndex, String &AHint);
        void __fastcall miExitClick(TObject *Sender);
        void __fastcall miAboutClick(TObject *Sender);
        void __fastcall chvSalesSeries1CustomDrawValue(
          TcxGridChartSeries *Sender, TcxCanvas *ACanvas,
          TcxGridChartDiagramValueViewInfo *AViewInfo, bool &ADone);
        void __fastcall grMainActiveTabChanged(TcxCustomGrid *Sender,
          TcxGridLevel *ALevel);
        void __fastcall miColumnDiagramClick(TObject *Sender);
        void __fastcall miBarDiagramClick(TObject *Sender);
        void __fastcall miTitlePositionItemClick(TObject *Sender);
        void __fastcall miLegendPositionItemClick(TObject *Sender);
        void __fastcall miLegendBorderClick(TObject *Sender);
        void __fastcall miValueCaptionPositionItemClick(
          TObject *Sender);
        void __fastcall chvSalesActiveDiagramChanged(
          TcxGridChartView *Sender, TcxGridChartDiagram *ADiagram);
        void __fastcall miAxisVisibleClick(TObject *Sender);
        void __fastcall miAxisGridLinesClick(TObject *Sender);
        void __fastcall miAxisTickMarkKindItemClick(TObject *Sender);
        void __fastcall miAxisTickMarkLabelsClick(TObject *Sender);
		void __fastcall miCategoryAxisCategoriesInReverseOrderClick(TObject *Sender);
		void __fastcall miCategoryAxisValueAxisAtMaxCategoryClick(TObject *Sender);
		void __fastcall miCategoryAxisValueAxisBetweenCategoriesClick(TObject *Sender);
		void __fastcall miLineDiagramClick(TObject *Sender);
		void __fastcall miAreaDiagramClick(TObject *Sender);
		void __fastcall miPieDiagramClick(TObject *Sender);
		void __fastcall miToolBoxClick(TObject *Sender);
		void __fastcall miToolBoxPositionClick(TObject *Sender);
		void __fastcall miLineStyleClick(TObject *Sender);
		void __fastcall miLineWidthClick(TObject *Sender);
		void __fastcall miMarkerStyleClick(TObject *Sender);
		void __fastcall miMarkerSizeClick(TObject *Sender);
		void __fastcall miValueStackingClick(TObject *Sender);
		void __fastcall miLineValueCaptionPositionClick(TObject *Sender);
		void __fastcall miSeriesCaptionsClick(TObject *Sender);
		void __fastcall miSeriesSitesClick(TObject *Sender);
		void __fastcall miSeriesColumnsClick(TObject *Sender);
		void __fastcall miPieValueCaptionPositionClick(TObject *Sender);
		void __fastcall miPieValueCaptionItemClick(TObject *Sender);
		void __fastcall miStackedBarDiagramClick(TObject *Sender);
		void __fastcall FormActivate(TObject *Sender);
		void __fastcall FormShow(TObject *Sender);
		void __fastcall chvSalesDiagramAreaCustomDrawValueArea(
          TcxGridChartAreaDiagram *Sender, TcxCanvas *ACanvas,
          TcxGridChartAreaDiagramValueViewInfo *AViewInfo, bool &ADone);
        void __fastcall chvSalesValueClick(TcxGridChartView *Sender,
          TcxGridChartSeries *ASeries, int AValueIndex, bool &AHandled);
private:
        TcxGridChartView* GetActiveChart();
        TcxGridChartColumnDiagram* GetActiveColumnDiagram();
        TcxGridChartHistogram* GetActiveHistogram();
        TcxGridChartLineDiagram* GetActiveLineDiagram();
        TcxGridChartPieDiagram* GetActivePieDiagram();
protected:
        void UpdateControls();
public:
        __fastcall TfrmMain(TComponent* Owner);
        void CalculateSalesInfo();
        void CalculateSalesInfoForQuarter(int AQuarter, TClientDataSet *AQuarterData);
        TcxGridChartHistogramAxis* GetActiveAxis(TObject* AMenuItem);

        __property TcxGridChartView* ActiveChart = {read = GetActiveChart};
        __property TcxGridChartColumnDiagram* ActiveColumnDiagram = {read = GetActiveColumnDiagram};
        __property TcxGridChartHistogram* ActiveHistogram = {read = GetActiveHistogram};
        __property TcxGridChartLineDiagram* ActiveLineDiagram = {read = GetActiveLineDiagram};
        __property TcxGridChartPieDiagram* ActivePieDiagram = {read = GetActivePieDiagram};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif

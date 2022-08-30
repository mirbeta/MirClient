//---------------------------------------------------------------------------

#ifndef ChartDataDrillingDemoMainH
#define ChartDataDrillingDemoMainH
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
#include "cxStyles.hpp"
#include <DB.hpp>
#include <Menus.hpp>
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
        TcxGrid *Grid;
        TcxGridDBChartView *ChartView;
        TcxGridDBChartDataGroup *ChartViewDataGroupPurchaseDate;
        TcxGridDBChartDataGroup *ChartViewDataGroupProduct;
        TcxGridDBChartDataGroup *ChartViewDataGroupCustomer;
        TcxGridDBChartDataGroup *ChartViewDataGroupPaymentType;
        TcxGridDBChartSeries *ChartViewSeriesPaymentAmount;
        TcxGridDBChartSeries *ChartViewSeriesQuantity;
        TcxGridDBChartSeries *ChartViewSeriesCount;
        TcxGridDBTableView *TableView;
        TcxGridDBColumn *TableViewProduct;
        TcxGridDBColumn *TableViewCustomer;
        TcxGridDBColumn *TableViewPaymentType;
        TcxGridDBColumn *TableViewPurchaseDate;
        TcxGridDBColumn *TableViewQuantity;
        TcxGridDBColumn *TableViewPaymentAmount;
        TcxGridDBColumn *TableViewOrderCount;
        TcxGridLevel *GridLevelChart;
        TcxGridLevel *GridLevelTable;
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
        TStringField *tblOrdersCustomer;
        TStringField *tblOrdersProduct;
		TClientDataSet *tblCustomers;
		TClientDataSet *tblProducts;
        TcxStyle *styleActiveGroup;
		void __fastcall GridActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel *ALevel);
		void __fastcall TableViewStylesGetGroupStyle(TcxGridTableView *Sender,
			TcxCustomGridRecord *ARecord, int ALevel, TcxStyle *&AStyle);
		void __fastcall FormCreate(TObject *Sender);
protected:
        DynamicArray<int> ActiveDataGroups;
        TcxGridDBColumn* GetColumnByChartItem(TcxGridChartItem* AChartItem);
        void ShowTableActiveGroup();
        void SynchronizeTableWithChart();
        void UpdateTableGroupingAndColumnVisibility();
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif

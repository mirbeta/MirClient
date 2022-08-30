//---------------------------------------------------------------------------

#ifndef DataSetsMainH
#define DataSetsMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxPivotGridCustomDataSet.hpp"
#include "cxPivotGridDrillDownDataSet.hpp"
#include "cxPivotGridSummaryDataSet.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBChartView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include "dxSparkline.hpp"
#include "dxmdaset.hpp"
#include <DB.hpp>

#define CM_APPLYBESTFIT         (WM_USER + 1)
#define CM_UPDATEGRIDVIEW       (WM_USER + 2)

//---------------------------------------------------------------------------
class TfrmDataSets : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
    TcxDBPivotGrid *DBPivotGrid;
    TcxDBPivotGridField *pgfPurchaseDate;
    TcxDBPivotGridField *pgfPaymentType;
    TcxDBPivotGridField *pgfQuantity;
    TcxDBPivotGridField *pgfCarName;
    TcxDBPivotGridField *pgfUnitPrice;
    TcxDBPivotGridField *pgfCompanyName;
    TcxDBPivotGridField *pgfPaymentAmount;
    TcxGridLevel *SummaryChartLevel;
    TcxGrid *Grid;                    
    TcxGridDBChartView *SummaryChartView;
    TcxPivotGridSummaryDataSet *PivotGridSummaryDataSet;
    TDataSource *PivotGridSummaryDataSource;
    TcxGridLevel *SummaryTableLevel;
    TcxGridDBTableView *SummaryTableView;
    TcxPivotGridDrillDownDataSet *PivotGridDrillDownDataSet;
    TDataSource *PivotGridDrillDownDataSource;
    TcxGridLevel *DrillDownTableLevel;
    TcxGridDBTableView *DrillDownTableView;
    void __fastcall GridActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel *ALevel);
    void __fastcall PivotGridDataSetCreateField(TcxPivotGridCustomDataSet *Sender, TcxPivotGridField *APivotGridField, TField *ADataSetField);
    void __fastcall PivotGridDataSetDataChanged(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall pgfPaymentAmountCalculateCustomSummary(TcxPivotGridField* Sender, TcxPivotGridCrossCellSummary* ASummary);
private:	// User declarations
    TdxSparklineProperties* SparklineTemplate;
    TcxCustomPivotGrid* __fastcall PivotGrid();

	void CreateFooterSummaryCell(TcxGridColumn *AGridColumn);
	TcxGridColumn* ColumnByCaption(TcxGridTableView *ATableView, AnsiString ACaption);
	void CreateFooterSummary(TcxGridTableView *ATableView);
    void UpdateSummaryChartView();
    void UpdateSummaryTableView();
	void UpdateDrillDownTableView();

	void __fastcall CMApplyBestFit(TMessage &Message);
	void __fastcall CMUpdateGridView(TMessage &Message);
public:		// User declarations
  __fastcall TfrmDataSets(TComponent* Owner);

BEGIN_MESSAGE_MAP
  MESSAGE_HANDLER(CM_APPLYBESTFIT, TMessage, CMApplyBestFit)
  MESSAGE_HANDLER(CM_UPDATEGRIDVIEW, TMessage, CMUpdateGridView)
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmDataSets *frmDataSets;
//---------------------------------------------------------------------------
#endif
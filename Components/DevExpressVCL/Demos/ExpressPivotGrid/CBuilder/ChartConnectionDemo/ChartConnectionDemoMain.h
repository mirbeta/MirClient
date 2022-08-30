//---------------------------------------------------------------------------

#ifndef ChartConnectionDemoMainH
#define ChartConnectionDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxPivotGridChartConnection.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TfrmChartConnection : public TfrmDemoBasicMain
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
	TcxPivotGridChartConnection *cxPivotGridChartConnection;
	TcxGridDBTableView *cxGridDBTableView1;
	TcxGrid *cxGrid;
	TcxGridLevel *cxGridLevel;
	TcxGridChartView *cxGridChartView;
	TMenuItem *miChartOptions;
	TMenuItem *miSourceData;
	TMenuItem *miVisibleCells;
	TMenuItem *miSelectedCells;
	TMenuItem *miSourceRow;
	TMenuItem *miSourceColumn;
	TMenuItem *N3;
	TMenuItem *miChartType;
	TMenuItem *miSourceForCategories;
	void __fastcall FormCreate(TObject *Sender);
		void __fastcall pgfPaymentTypeGetGroupImageIndex(
		  TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
		  int &AImageIndex, TAlignment &AImageAlignHorz,
		  TcxAlignmentVert &AImageAlignVert);
	void __fastcall miVisibleCellsClick(TObject *Sender);
	void __fastcall miSourceColumnClick(TObject *Sender);
	void __fastcall miChartColumDiagramClick(TObject *Sender);
	void __fastcall cxGridChartViewActiveDiagramChanged(TcxGridChartView *Sender, 
		TcxGridChartDiagram *ADiagram);
private:	// User declarations
	TcxCustomPivotGrid* __fastcall PivotGrid();
	void __fastcall InitializeDiagramList();
public:		// User declarations
    __fastcall TfrmChartConnection(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmChartConnection *frmChartConnection;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------

#ifndef MultipleTotalsMainH
#define MultipleTotalsMainH
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
//---------------------------------------------------------------------------
class TfrmMultipleTotals : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
    TcxDBPivotGridField *pgfPurchaseMonth;
    TcxDBPivotGridField *cxDBPivotGrid1PaymentType;
    TcxDBPivotGridField *cxDBPivotGrid1Quantity;
    TcxDBPivotGridField *cxDBPivotGrid1CarName;
    TcxDBPivotGridField *cxDBPivotGrid1UnitPrice;
    TcxDBPivotGridField *pgfCompanyName;
    TcxDBPivotGridField *cxDBPivotGrid1PaymentAmount;
    TcxDBPivotGridField *pgfPurchaseQuarter;
    TcxDBPivotGrid *DBPivotGrid;
    void __fastcall FormCreate(TObject *Sender);
        void __fastcall CalculateCustomSummary(
          TcxPivotGridField *Sender,
          TcxPivotGridCrossCellSummary *ASummary);
        void __fastcall PivotGridStylesGetGroupHeaderStyle(
          TcxCustomPivotGrid *Sender, TcxPivotGridViewDataItem *AItem,
          TcxStyle *&AStyle);
        void __fastcall PivotGridStylesGetContentStyle(
          TcxCustomPivotGrid *Sender, TcxPivotGridDataCellViewInfo *ACell,
          TcxStyle *&AStyle);
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();
public:		// User declarations
    __fastcall TfrmMultipleTotals(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMultipleTotals *frmMultipleTotals;
//---------------------------------------------------------------------------
#endif

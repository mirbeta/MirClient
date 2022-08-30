//---------------------------------------------------------------------------

#ifndef OrderReportsMainH
#define OrderReportsMainH
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
class TfrmOrderReport : public TfrmDemoBasicMain
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
    void __fastcall FormCreate(TObject *Sender);
        void __fastcall pgfPaymentTypeGetGroupImageIndex(
          TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
          int &AImageIndex, TAlignment &AImageAlignHorz,
          TcxAlignmentVert &AImageAlignVert);
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();
public:		// User declarations
    __fastcall TfrmOrderReport(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmOrderReport *frmOrderReport;
//---------------------------------------------------------------------------
#endif

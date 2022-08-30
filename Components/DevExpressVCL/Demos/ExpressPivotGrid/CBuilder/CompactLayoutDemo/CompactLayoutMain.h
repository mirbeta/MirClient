//---------------------------------------------------------------------------

#ifndef CompactLayoutMainH
#define CompactLayoutMainH
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
#include "cxSplitter.hpp"
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
//---------------------------------------------------------------------------
class TfrmCompactLayout : public TfrmDemoBasicMain
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
    TMenuItem *miCustomization;
    TcxSplitter *cxSplitter1;
    void __fastcall FormCreate(TObject *Sender);
        void __fastcall pgfPaymentTypeGetGroupImageIndex(
          TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
          int &AImageIndex, TAlignment &AImageAlignHorz,
          TcxAlignmentVert &AImageAlignVert);
    void __fastcall miCustomizationClick(TObject * Sender);
    void __fastcall DBPivotGridCustomization(TObject * Sender);
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();
public:		// User declarations
    __fastcall TfrmCompactLayout(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCompactLayout *frmCompactLayout;
//---------------------------------------------------------------------------
#endif

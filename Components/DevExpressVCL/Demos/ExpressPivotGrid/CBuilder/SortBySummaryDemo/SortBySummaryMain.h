//---------------------------------------------------------------------------

#ifndef SortBySummaryMainH
#define SortBySummaryMainH
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
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmSortBySummary : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
        TcxDBPivotGrid *DBPivotGrid;
        TcxDBPivotGridField *pgfPurchaseQuarter;
        TcxDBPivotGridField *pgfPaymentType;
        TcxDBPivotGridField *pgfQuantity;
        TcxDBPivotGridField *pgfCarName;
        TcxDBPivotGridField *pgfUnitPrice;
        TcxDBPivotGridField *pgfCompanyName;
        TcxDBPivotGridField *pgfPaymentAmount;
        TPanel *pnSettings;
        TLabel *lbSortThe;
        TLabel *lbSortBy;
        TLabel *lbShowTop;
        TLabel *lbValues;
        TBevel *bvSplitter;
        TcxComboBox *cbxSortField;
        TcxComboBox *cbxSortByField;
        TcxSpinEdit *speTopCount;
        TcxCheckBox *cbxTopValuesShowOthers;
        TcxRadioGroup *rgSortOrder;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall SortFieldChanged(TObject *Sender);
        void __fastcall SortByChanged(TObject *Sender);
        void __fastcall speTopCountPropertiesChange(TObject *Sender);
        void __fastcall cbxTopValuesShowOthersPropertiesChange(
          TObject *Sender);
        void __fastcall GetGroupHeaderStyle(TcxCustomPivotGrid *Sender,
          TcxPivotGridViewDataItem *AItem, TcxStyle *&AStyle);
        void __fastcall rgSortOrderClick(TObject *Sender);
        void __fastcall DBPivotGridLayoutChanged(TObject *Sender);
private:	// User declarations
    bool FLocked;
    TcxCustomPivotGrid* __fastcall PivotGrid();
    TcxPivotGridField* __fastcall CurrentField();
public:		// User declarations
    __fastcall TfrmSortBySummary(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSortBySummary *frmSortBySummary;
//---------------------------------------------------------------------------
#endif

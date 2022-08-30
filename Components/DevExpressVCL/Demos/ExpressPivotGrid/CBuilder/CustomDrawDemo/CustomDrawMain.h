//---------------------------------------------------------------------------

#ifndef CustomDrawMainH
#define CustomDrawMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxStyles.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TfrmCustomDraw : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
        TMenuItem *Drawing1;
        TMenuItem *N3;
        TMenuItem *Content1;
		TMenuItem *GroupHeaders1;
		TMenuItem *miLimitValues;
		TcxDBPivotGrid *DBPivotGrid;
        TcxDBPivotGridField *pgfPaymentType;
        TcxDBPivotGridField *pgfQuantity;
        TcxDBPivotGridField *pgfCarName;
		TcxDBPivotGridField *pgfUnitPrice;
		TcxDBPivotGridField *pgfCompanyName;
		TcxDBPivotGridField *pgfPaymentAmount;
		TTimer *tmrColorChange;
		void __fastcall DrawingClick(TObject *Sender);
		void __fastcall DBPivotGridCustomDrawColumnHeader(
          TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
          TcxPivotGridHeaderCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall DBPivotGridCustomDrawRowHeader(
          TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
          TcxPivotGridHeaderCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall DBPivotGridCustomDrawCell(
          TcxCustomPivotGrid *Sender, TcxCanvas *ACanvas,
          TcxPivotGridDataCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall DBPivotGridSelectionChanged(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall tmrColorChangeTimer(TObject *Sender);

private:	// User declarations
    int FCurrentColorIndex;
	TColor FColors[11];
	int FColorCount;
    TcxPivotGridRecords *FList;
    TFont *FSupportFont;

    TcxCustomPivotGrid* __fastcall PivotGrid();
public:		// User declarations
	__fastcall TfrmCustomDraw(TComponent* Owner);
    virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
};

class TcxPivotGridViewDataItemAccess : public TcxPivotGridViewDataItem
{
public:
	int __fastcall GetChildLeftVisibleIndex();
	int __fastcall GetChildRightVisibleIndex();
};
class TcxPivotGridHeaderCellViewInfoAccess : public TcxPivotGridHeaderCellViewInfo
{
public:
  __property Data;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCustomDraw *frmCustomDraw;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoMainH
#define EditorsInPlaceDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeels.hpp"
#include "cxProgressBar.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmEditorsInPlace : public TfrmDemoBasicMain
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
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();
public:		// User declarations
	__fastcall TfrmEditorsInPlace(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEditorsInPlace *frmEditorsInPlace;
//---------------------------------------------------------------------------
#endif

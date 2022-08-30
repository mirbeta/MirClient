//---------------------------------------------------------------------------

#ifndef MasterDetailReportDemoMainH
#define MasterDetailReportDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "ReportDesignerBaseForm.h"
#include "ReportPreviewUnit.h"
#include "cxClasses.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetReportDesigner.hpp"
#include <ExtCtrls.hpp>
#include <DB.hpp>
#include <DBClient.hpp>
#include <dxmdaset.hpp>
#include "cxFilterControl.hpp"
#include "ReportPreviewUnit.h"
#include "dxSpreadSheetTypes.hpp"
//---------------------------------------------------------------------------
class TfrmMasterDetail : public TfrmReportDesignerBase
{
__published:	// IDE-managed Components
    TdxSpreadSheetReportDetail *ReportDesignerDetail1;
    TdxSpreadSheetReportDetail *ReportDesignerDetail2;
    TDataSource *dsMaster;
    TdxMemData *mdsMaster;
    TAutoIncField *mdsMasterSupplierID;
    TWideStringField *mdsMasterCompanyName;
    TWideStringField *mdsMasterContactName;
    TWideStringField *mdsMasterContactTitle;
    TWideStringField *mdsMasterAddress;
    TWideStringField *mdsMasterCity;
    TWideStringField *mdsMasterRegion;
    TWideStringField *mdsMasterPostalCode;
    TWideStringField *mdsMasterCountry;
    TWideStringField *mdsMasterPhone;
    TWideStringField *mdsMasterFax;
    TWideMemoField *mdsMasterHomePage;
    TDataSource *dsDetailLevel0;
    TdxMemData *mdsDetailLevel0;
    TAutoIncField *mdsDetailLevel0ProductID;
    TWideStringField *mdsDetailLevel0ProductName;
    TIntegerField *mdsDetailLevel0SupplierID;
    TIntegerField *mdsDetailLevel0CategoryID;
    TWideStringField *mdsDetailLevel0QuantityPerUnit;
    TBCDField *mdsDetailLevel0UnitPrice;
    TSmallintField *mdsDetailLevel0UnitsInStock;
    TSmallintField *mdsDetailLevel0UnitsOnOrder;
    TSmallintField *mdsDetailLevel0ReorderLevel;
    TBooleanField *mdsDetailLevel0Discontinued;
    TWideStringField *mdsDetailLevel0EAN13;
    TDataSource *dsDetailLevel1;
    TdxMemData *mdsDetailLevel1;
    TIntegerField *mdsDetailLevel1OrderID;
    TIntegerField *mdsDetailLevel1ProductID;
    TBCDField *mdsDetailLevel1UnitPrice;
    TSmallintField *mdsDetailLevel1Quantity;
    TFloatField *mdsDetailLevel1Discount;
    TCurrencyField *mdsDetailLevel1SubTotal;
    void __fastcall  mdsDetailLevel1CalcFields(TDataSet *DataSet);
    void __fastcall ReportDesignerAfterBuild(TObject *Sender);
private:	// User declarations
protected:
public:		// User declarations
	__fastcall TfrmMasterDetail(TComponent* Owner);
	virtual void Initialize();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMasterDetail *frmMasterDetail;
//---------------------------------------------------------------------------
#endif

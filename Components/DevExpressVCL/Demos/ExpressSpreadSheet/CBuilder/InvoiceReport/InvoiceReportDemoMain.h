//---------------------------------------------------------------------------

#ifndef InvoiceReportDemoMainH
#define InvoiceReportDemoMainH
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
class TfrmInvoiceReport : public TfrmReportDesignerBase
{
__published:	// IDE-managed Components
    TdxMemData *mdsInvoice;
    TDataSource *dsInvoice;
    TWideStringField *mdsInvoiceShipName;
    TWideStringField *mdsInvoiceShipAddress;
    TWideStringField *mdsInvoiceShipCity;
    TWideStringField *mdsInvoiceShipRegion;
    TWideStringField *mdsInvoiceShipPostalCode;
    TWideStringField *mdsInvoiceShipCountry;
    TWideStringField *mdsInvoiceCustomerID;
    TWideStringField *mdsInvoiceCustomers_CompanyName;
    TWideStringField *mdsInvoiceAddress;
    TWideStringField *mdsInvoiceCity;
    TWideStringField *mdsInvoiceRegion;
    TWideStringField *mdsInvoicePostalCode;
    TWideStringField *mdsInvoiceCountry;
    TWideStringField *mdsInvoiceSalesperson;
    TAutoIncField *mdsInvoiceOrderID;
    TDateTimeField *mdsInvoiceOrderDate;
    TDateTimeField *mdsInvoiceRequiredDate;
    TDateTimeField *mdsInvoiceShippedDate;
    TWideStringField *mdsInvoiceShippers_CompanyName;
	TIntegerField *mdsInvoiceProductID;
	TWideStringField *mdsInvoiceProductName;
	TBCDField *mdsInvoiceUnitPrice;
	TSmallintField *mdsInvoiceQuantity;
    TFloatField *mdsInvoiceDiscount;
    TBCDField *mdsInvoiceExtendedPrice;
    TBCDField *mdsInvoiceFreight;
    void __fastcall ReportDesignerAfterBuild(TObject *Sender);
private:	// User declarations
protected:
public:		// User declarations
	__fastcall TfrmInvoiceReport(TComponent* Owner);
	virtual void Initialize();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmInvoiceReport *frmInvoiceReport;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------

#ifndef MasterDetailChartDemoMainH
#define MasterDetailChartDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBChartView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <Menus.hpp>
#include "cxLookAndFeelPainters.hpp"
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include <ComCtrls.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
        TDataSource *dsOrders;
        TDataSource *dsCustomers;
        TDataSource *dsProducts;
        TcxGrid *grMain;
        TcxGridDBTableView *tvCustomers;
        TcxGridDBColumn *tvCustomersID;
        TcxGridDBColumn *tvCustomersFirstName;
        TcxGridDBColumn *tvCustomersLastName;
        TcxGridDBColumn *tvCustomersCompany;
        TcxGridDBColumn *tvCustomersAddress;
        TcxGridDBColumn *tvCustomersCity;
        TcxGridDBColumn *tvCustomersState;
        TcxGridDBColumn *tvCustomersZipCode;
        TcxGridDBColumn *tvCustomersEmail;
        TcxGridDBChartView *chvOrders;
        TcxGridDBChartSeries *chvOrdersPaymentAmountSeries;
        TcxGridDBChartSeries *chvOrdersProductIDSeries;
        TcxGridDBChartSeries *chvOrdersQuantitySeries;
        TcxGridDBChartView *chvProducts;
        TcxGridDBChartSeries *chvProductsCopiesSeries;
        TcxGridLevel *grMainLevel1;
        TcxGridLevel *grMainLevel2;
        TcxGridLevel *grMainLevel3;
	TClientDataSet *cdsProductSumOfQuantity;
	TIntegerField *cdsProductSumOfQuantityCustomerID;
	TStringField *cdsProductSumOfQuantityName;
	TFloatField *cdsProductSumOfQuantitySUMOFQuantity;
	TClientDataSet *cdsOrders;
	TAutoIncField *cdsOrdersID;
	TIntegerField *cdsOrdersCustomerID;
	TIntegerField *cdsOrdersProductID;
	TDateTimeField *cdsOrdersPurchaseDate;
	TDateTimeField *cdsOrdersTime;
	TStringField *cdsOrdersPaymentType;
	TMemoField *cdsOrdersDescription;
	TIntegerField *cdsOrdersQuantity;
	TCurrencyField *cdsOrdersPaymentAmount;
	TClientDataSet *cdsProducts;
	TAutoIncField *cdsProductsID;
	TStringField *cdsProductsName;
	TMemoField *cdsProductsDescription;
	TStringField *cdsProductsPlatform;
	TBlobField *cdsProductsLogo;
	TMemoField *cdsProductsLink;
	TClientDataSet *cdsCustomers;
	TAutoIncField *cdsCustomersID;
	TStringField *cdsCustomersFirstName;
	TStringField *cdsCustomersLastName;
	TStringField *cdsCustomersCompany;
	TStringField *cdsCustomersPrefix;
	TStringField *cdsCustomersTitle;
	TStringField *cdsCustomersAddress;
	TStringField *cdsCustomersCity;
	TStringField *cdsCustomersState;
	TStringField *cdsCustomersZipCode;
	TStringField *cdsCustomersSource;
	TStringField *cdsCustomersCustomer;
	TStringField *cdsCustomersHomePhone;
	TStringField *cdsCustomersFaxPhone;
	TStringField *cdsCustomersSpouse;
	TStringField *cdsCustomersOccupation;
	TIntegerField *cdsCustomersCalcField;
		void __fastcall FormCreate(TObject *Sender);
		void __fastcall chvOrdersGetValueHint(TcxGridChartView *Sender,
		  TcxGridChartSeries *ASeries, int AValueIndex, String &AHint);
		void __fastcall chvOrdersCategoriesGetValueDisplayText(
		  TObject *Sender, const Variant &AValue,
		  String &ADisplayText);
protected:
    AnsiString GetProductName(int AID);
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif

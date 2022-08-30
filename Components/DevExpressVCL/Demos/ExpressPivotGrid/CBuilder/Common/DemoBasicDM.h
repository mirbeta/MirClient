//----------------------------------------------------------------------------
#ifndef DemoBasicDMH
#define DemoBasicDMH
//----------------------------------------------------------------------------
#include <System.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <Messages.hpp>
#include <Classes.hpp>
#include <Graphics.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <DB.hpp>
#include <Db.hpp>
#include <ImgList.hpp>
#include <cxStyles.hpp>
#include "cxClasses.hpp"
#include <DBClient.hpp>
//----------------------------------------------------------------------------
class TdmOrders : public TDataModule
{
__published:
        TDataSource *dsCustomers;
        TDataSource *dsOrders;
		TClientDataSet *cdsCustomers;
		TIntegerField *cdsCustomersID;
		TStringField *cdsCustomersCompany;
		TClientDataSet *cdsOrders;
		TAutoIncField *cdsOrdersID;
		TIntegerField *cdsOrdersCustomerID;
		TIntegerField *cdsOrdersProductID;
		TDateTimeField *cdsOrdersPurchaseDate;
		TStringField *cdsOrdersPaymentType;
		TIntegerField *cdsOrdersQuantity;
		TStringField *cdsOrdersCarName;
		TCurrencyField *cdsOrdersUnitPrice;
		TStringField *cdsOrdersCompanyName;
		TCurrencyField *cdsOrdersPaymentAmount;
		TClientDataSet *cdsCars;
		TAutoIncField *cdsCarsID;
		TWideStringField *cdsCarsTrademark;
        TWideStringField *cdsCarsModel;
        TBCDField *cdsCarsPrice;
        TWideStringField *cdsCarsCarName;
        TDataSource *dsCars;
        TImageList *PaymentTypeImages;
        TcxStyleRepository *cxStyleRepository1;
        TcxStyle *stBoldBlueFont;
        TcxStyle *stBoldRedFont;
        TcxStyle *stBoldBlackFont;

        void __fastcall cdsCarsCalcFields(TDataSet *DataSet);
		void __fastcall cdsOrdersCalcFields(TDataSet *DataSet);
private:
public:
	virtual __fastcall TdmOrders(TComponent* AOwner);
};
//----------------------------------------------------------------------------
extern PACKAGE TdmOrders *dmOrders;
//----------------------------------------------------------------------------
#endif    

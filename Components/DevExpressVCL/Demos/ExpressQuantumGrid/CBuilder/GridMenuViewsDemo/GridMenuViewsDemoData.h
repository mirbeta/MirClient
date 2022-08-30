//---------------------------------------------------------------------------

#ifndef GridMenuViewsDemoDataH
#define GridMenuViewsDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ImgList.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TGridMenuViewsDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsOrders;
  TDataSource *dsCustomers;
  TImageList *PaymentTypeImages;
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
	TStringField *cdsOrdersPurchaseMonth;
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
	TMemoField *cdsCustomersDescription;
	void __fastcall cdsOrdersCalcFields(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TGridMenuViewsDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGridMenuViewsDemoDataDM *GridMenuViewsDemoDataDM;
//---------------------------------------------------------------------------
#endif

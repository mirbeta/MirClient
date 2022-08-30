//---------------------------------------------------------------------------

#ifndef SummaryFooterDemoDataH
#define SummaryFooterDemoDataH
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
class TSummaryFooterDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
        TDataSource *dsOrders;
        TDataSource *dsCustomers;
	TClientDataSet *tblOrders;
        TAutoIncField *tblOrdersID;
        TIntegerField *tblOrdersCustomerID;
        TIntegerField *tblOrdersProductID;
        TDateTimeField *tblOrdersPurchaseDate;
        TDateTimeField *tblOrdersTime;
        TStringField *tblOrdersPaymentType;
        TMemoField *tblOrdersDescription;
        TIntegerField *tblOrdersQuantity;
        TCurrencyField *tblOrdersPaymentAmount;
	TClientDataSet *tblCustomers;
        TIntegerField *tblCustomersID;
        TStringField *tblCustomersFirstName;
        TStringField *tblCustomersLastName;
        TStringField *tblCustomersCompany;
        TStringField *tblCustomersPrefix;
        TStringField *tblCustomersTitle;
        TStringField *tblCustomersAddress;
        TStringField *tblCustomersCity;
        TStringField *tblCustomersState;
        TStringField *tblCustomersZipCode;
        TStringField *tblCustomersSource;
        TStringField *tblCustomersCustomer;
        TStringField *tblCustomersHomePhone;
        TStringField *tblCustomersFaxPhone;
        TStringField *tblCustomersSpouse;
        TStringField *tblCustomersOccupation;
        TMemoField *tblCustomersDescription;
        TStringField *tblCustomersName;
        TImageList *PaymentTypeImages;
		void __fastcall DataModuleCreate(TObject *Sender);
		void __fastcall tblCustomersCalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
  __fastcall TSummaryFooterDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryFooterDemoDataDM *SummaryFooterDemoDataDM;
//---------------------------------------------------------------------------
#endif

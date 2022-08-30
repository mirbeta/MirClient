//---------------------------------------------------------------------------

#ifndef CustomDrawTableViewDemoDataH
#define CustomDrawTableViewDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TCustomDrawTableViewDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
  TcxStyleRepository *StyleRepository;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxStyle *cxStyle8;
  TcxStyle *cxStyle9;
  TcxStyle *cxStyle10;
  TcxStyle *cxStyle11;
  TcxStyle *cxStyle12;
  TcxStyle *cxStyle13;
  TcxStyle *cxStyle14;
  TcxGridTableViewStyleSheet *GridTableViewStyleSheetDevExpress;
  TDataSource *dsCustomers;
  TDataSource *dsOrders;
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
  TStringField *cdsCustomersEmail;
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
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TCustomDrawTableViewDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawTableViewDemoMainDM *CustomDrawTableViewDemoMainDM;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------

#ifndef FilterControlDemoDataH
#define FilterControlDemoDataH
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
#include <Db.hpp>
#include <DBClient.hpp>
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TFilterControlDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TDataSource *dsOrders;
  TDataSource *dsCustomers;
  TImageList *PaymentTypeImages;
  TDataSource *dsOrdersStd;
  TClientDataSet *tblOrders;
  TAutoIncField *tblOrdersID;
  TIntegerField *tblOrdersCustomerID;
  TIntegerField *tblOrdersProductID;
  TDateTimeField *tblOrdersPurchaseDate;
  TDateTimeField *tblOrdersTime;
  TStringField *tblOrdersPaymentType;
  TCurrencyField *tblOrdersPaymentAmount;
  TMemoField *tblOrdersDescription;
  TIntegerField *tblOrdersQuantity;
  TClientDataSet *tblCustomers;
  TAutoIncField *tblCustomersID;
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
  TClientDataSet *tblOrdersStd;
  TStringField *tblOrdersStdCompany;
  TStringField *tblOrdersStdCar;
  TIntegerField *tblOrdersStdCustomerID;
  TIntegerField *tblOrdersStdProductID;
  TDateTimeField *tblOrdersStdPurchaseDate;
  TDateTimeField *tblOrdersStdTime;
  TStringField *tblOrdersStdPaymentType;
  TCurrencyField *tblOrdersStdPaymentAmount;
  TMemoField *tblOrdersStdDescription;
  TIntegerField *tblOrdersStdQuantity;
  void __fastcall tblCustomersCalcFields(TDataSet *DataSet);
  void __fastcall tblOrdersStdCalcFields(TDataSet *DataSet);
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TFilterControlDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilterControlDemoDataDM *FilterControlDemoDataDM;
//---------------------------------------------------------------------------
#endif

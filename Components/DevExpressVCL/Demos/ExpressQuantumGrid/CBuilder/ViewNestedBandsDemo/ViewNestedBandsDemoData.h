//---------------------------------------------------------------------------

#ifndef ViewNestedBandsDemoDataH
#define ViewNestedBandsDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxDBEditRepository.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGridCardView.hpp"
#include <ImgList.hpp>
#include <DBClient.hpp>
#include <Db.hpp>
//---------------------------------------------------------------------------
class TViewNestedBandsDemoDataDM : public TDataModule
{
__published:  // IDE-managed Components
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
  TcxStyleRepository *StyleRepository;
  TcxStyle *cxStyle31;
  TcxStyle *cxStyle35;
  TImageList *PaymentTypeImages;
  void __fastcall tblCustomersCalcFields(TDataSet *DataSet);
  void __fastcall DataModuleCreate(TObject *Sender);
private:  // User declarations
public:    // User declarations
  __fastcall TViewNestedBandsDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewNestedBandsDemoDataDM *ViewNestedBandsDemoDataDM;
//---------------------------------------------------------------------------
#endif

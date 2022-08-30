//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoDataH
#define EditorsInPlaceDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxTL.hpp"
#include <ImgList.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
  TdxMemData *mdCustomers;
  TDataSource *dsCustomers;
  TdxMemData *mdOrders;
  TAutoIncField *mdOrdersID;
  TIntegerField *mdOrdersCustomerID;
  TIntegerField *mdOrdersProductID;
  TDateTimeField *mdOrdersPurchaseDate;
  TDateTimeField *mdOrdersTime;
  TStringField *mdOrdersPaymentType;
  TCurrencyField *mdOrdersPaymentAmount;
  TMemoField *mdOrdersDescription;
  TIntegerField *mdOrdersQuantity;
  TStringField *mdOrdersCustomerEmail;
  TDataSource *dsOrders;
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
  TcxStyle *styCaption;
  TcxTreeListStyleSheet *TreeListStyleSheetDevExpress;
  TImageList *PaymentTypeImages;
  void __fastcall mdOrdersCalcFields(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoDataDM *EditorsInPlaceDemoDataDM;
//---------------------------------------------------------------------------
#endif

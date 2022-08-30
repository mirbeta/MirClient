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
#include "cxVGrid.hpp"
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
  TStringField *mdOrdersPaymentType;
  TDateTimeField *mdOrdersTime;
  TFloatField *mdOrdersPaymentAmount;
  TMemoField *mdOrdersDescription;
  TIntegerField *mdOrdersQuantity;
  TStringField *mdOrdersCustomerEmail;
  TDataSource *dsOrders;
  TcxStyleRepository *StyleRepository;
  TcxStyle *styCaption;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  TImageList *PaymentTypeImages;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoDataDM *EditorsInPlaceDemoDataDM;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoDataH
#define StylesSimpleDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "cxVGrid.hpp"
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TStylesSimpleDemoDataDM : public TDataModule
{
__published:  // IDE-managed Components
  TDataSource *dsOrders;
  TcxStyleRepository *StyleRepository;
  TcxStyle *Sunny;
  TcxStyle *Dark;
  TcxStyle *Golden;
  TcxStyle *Summer;
  TcxStyle *Autumn;
  TcxStyle *Bright;
  TcxStyle *Cold;
  TcxStyle *Spring;
  TcxStyle *Light;
  TcxStyle *Winter;
  TcxStyle *Depth;
  TcxVerticalGridStyleSheet *UserStyleSheet;
	TdxMemData *mdOrders;
	TDateTimeField *mdOrdersPurchaseDate;
	TStringField *mdOrdersPaymentType;
	TFloatField *mdOrdersPaymentAmount;
	TIntegerField *mdOrdersQuantity;
	TStringField *mdOrdersFirstName;
	TStringField *mdOrdersLastName;
	TStringField *mdOrdersCompany;
	TStringField *mdOrdersPrefix;
	TStringField *mdOrdersTitle;
	TStringField *mdOrdersAddress;
	TStringField *mdOrdersCity;
	TStringField *mdOrdersState;
	TStringField *mdOrdersZipCode;
	TStringField *mdOrdersSource;
	TStringField *mdOrdersCustomer;
	TStringField *mdOrdersHomePhone;
	TStringField *mdOrdersFaxPhone;
	TStringField *mdOrdersSpouse;
	TStringField *mdOrdersOccupation;
	TStringField *mdOrdersEmail;
	TStringField *mdOrdersTrademark;
	TStringField *mdOrdersModel;
	TSmallintField *mdOrdersHP;
	TFloatField *mdOrdersLiter;
	TSmallintField *mdOrdersCyl;
	TSmallintField *mdOrdersTransmissSpeedCount;
	TStringField *mdOrdersTransmissAutomatic;
	TSmallintField *mdOrdersMPG_City;
	TSmallintField *mdOrdersMPG_Highway;
	TStringField *mdOrdersCategory;
	TMemoField *mdOrdersCars_Description;
	TStringField *mdOrdersHyperlink;
	TBlobField *mdOrdersPicture;
	TFloatField *mdOrdersPrice;
	TIntegerField *mdOrdersCustomers_ID;
	TIntegerField *mdOrdersCustomerID;
	TIntegerField *mdOrdersOrders_ID;
	TIntegerField *mdOrdersProductID;
	TIntegerField *mdOrdersCars_ID;
	TDateTimeField *mdOrdersOrders_Time;
	void __fastcall StylesSimpleDemoDataDMCreate(TObject *Sender);
private:  // User declarations
public:   // User declarations
  __fastcall TStylesSimpleDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoDataDM *StylesSimpleDemoDataDM;
//---------------------------------------------------------------------------
#endif

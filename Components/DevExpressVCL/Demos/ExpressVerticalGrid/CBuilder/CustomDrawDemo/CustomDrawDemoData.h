//---------------------------------------------------------------------------

#ifndef CustomDrawDemoDataH
#define CustomDrawDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include "cxVGrid.hpp"
#include <DB.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TCustomDrawDemoDataDM : public TDataModule
{
__published:  // IDE-managed Components
  TcxStyleRepository *StyleRepository;
  TcxStyle *stCustomer;
  TcxStyle *stNoCustomer;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  TdxMemData *mdOrders;
  TDateTimeField *mdOrdersPurchaseDate;
  TDateTimeField *mdOrdersOrders_Time;
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
  TDataSource *dsOrders;
	void __fastcall CustomDrawDemoDataDMCreate(TObject *Sender);
private:  // User declarations
public:   // User declarations
  __fastcall TCustomDrawDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoDataDM *CustomDrawDemoDataDM;
//---------------------------------------------------------------------------
#endif

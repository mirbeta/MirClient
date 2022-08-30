//---------------------------------------------------------------------------

#ifndef DemoDMH
#define DemoDMH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DB.hpp>
#include <cxLookAndFeels.hpp>
#include <dxLayoutLookAndFeels.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TdmDemo : public TDataModule
{
__published:	// IDE-managed Components
	TDataSource *dsOrders;
	TdxLayoutLookAndFeelList *llcfMain;
	TdxLayoutStandardLookAndFeel *dxLayoutStandardLookAndFeel1;
	TdxLayoutOfficeLookAndFeel *dxLayoutOfficeLookAndFeel1;
	TdxLayoutWebLookAndFeel *dxLayoutWebLookAndFeel1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
	TdxMemData *mdOrders;
	TdxMemData *mdCars;
	TdxMemData *mdCustomers;
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
	TIntegerField *mdOrdersCustomers_ID;
	TIntegerField *mdOrdersOrders_ID;
	TIntegerField *mdOrdersCars_ID;
	
	void __fastcall DataModuleCreate(TObject* Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TdmDemo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmDemo *dmDemo;
//---------------------------------------------------------------------------
#endif

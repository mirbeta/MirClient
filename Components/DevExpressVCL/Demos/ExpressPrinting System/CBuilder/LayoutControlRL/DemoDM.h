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
	TdxMemData *mdLayoutControl;
	TDateTimeField *mdLayoutControlPurchaseDate;
	TDateTimeField *mdLayoutControlOrders_Time;
	TStringField *mdLayoutControlPaymentType;
	TFloatField *mdLayoutControlPaymentAmount;
	TIntegerField *mdLayoutControlQuantity;
	TStringField *mdLayoutControlFirstName;
	TStringField *mdLayoutControlLastName;
	TStringField *mdLayoutControlCompany;
	TStringField *mdLayoutControlPrefix;
	TStringField *mdLayoutControlTitle;
	TStringField *mdLayoutControlAddress;
	TStringField *mdLayoutControlCity;
	TStringField *mdLayoutControlState;
	TStringField *mdLayoutControlZipCode;
	TStringField *mdLayoutControlSource;
	TStringField *mdLayoutControlCustomer;
	TStringField *mdLayoutControlHomePhone;
	TStringField *mdLayoutControlFaxPhone;
	TStringField *mdLayoutControlSpouse;
	TStringField *mdLayoutControlOccupation;
	TStringField *mdLayoutControlEmail;
	TStringField *mdLayoutControlTrademark;
	TStringField *mdLayoutControlModel;
	TSmallintField *mdLayoutControlHP;
	TFloatField *mdLayoutControlLiter;
	TSmallintField *mdLayoutControlCyl;
	TSmallintField *mdLayoutControlTransmissSpeedCount;
	TStringField *mdLayoutControlTransmissAutomatic;
	TSmallintField *mdLayoutControlMPG_City;
	TSmallintField *mdLayoutControlMPG_Highway;
	TStringField *mdLayoutControlCategory;
	TMemoField *mdLayoutControlCars_Description;
	TStringField *mdLayoutControlHyperlink;
	TBlobField *mdLayoutControlPicture;
	TFloatField *mdLayoutControlPrice;
	TIntegerField *mdLayoutControlCustomers_ID;
	TIntegerField *mdLayoutControlOrders_ID;
	TIntegerField *mdLayoutControlCars_ID;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TdmDemo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmDemo *dmDemo;
//---------------------------------------------------------------------------
#endif

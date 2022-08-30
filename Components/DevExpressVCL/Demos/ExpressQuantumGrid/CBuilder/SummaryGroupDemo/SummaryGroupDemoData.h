//---------------------------------------------------------------------------

#ifndef SummaryGroupDemoDataH
#define SummaryGroupDemoDataH
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
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TSummaryGroupDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
        TDataSource *dsCars;
        TDataSource *dsOrders;
        TDataSource *dsCustomers;
        TdxMemData *mdCars;
        TAutoIncField *mdCarsID;
        TStringField *mdCarsTrademark;
        TStringField *mdCarsModel;
        TSmallintField *mdCarshp;
        TFloatField *mdCarsliter;
        TSmallintField *mdCarscyl;
        TSmallintField *mdCarsTransmissSpeedCount;
        TStringField *mdCarsTransmissAutomatic;
        TSmallintField *mdCarsMPG_City;
        TSmallintField *mdCarsMPG_Highway;
        TStringField *mdCarsCategory;
        TMemoField *mdCarsDescription;
        TStringField *mdCarsHyperlink;
        TBlobField *mdCarsPicture;
        TFloatField *mdCarsPrice;
        TStringField *mdCarsCarName;
        TdxMemData *mdOrders;
        TAutoIncField *mdOrdersID;
        TIntegerField *mdOrdersCustomerID;
        TIntegerField *mdOrdersProductID;
        TDateTimeField *mdOrdersPurchaseDate;
        TDateTimeField *mdOrdersTime;
        TStringField *mdOrdersPaymentType;
        TMemoField *mdOrdersDescription;
        TIntegerField *mdOrdersQuantity;
        TCurrencyField *mdOrdersPaymentAmount;
        TStringField *mdOrdersPurchaseMonth;
        TdxMemData *mdCustomers;
	TAutoIncField *mdCustomersID;
        TStringField *mdCustomersFirstName;
        TStringField *mdCustomersLastName;
        TStringField *mdCustomersCompany;
        TcxStyleRepository *StyleRepository;
        TcxStyle *stBlueLight;
        TcxStyle *stGreyLight;
        TcxStyle *stBlueSky;
        TImageList *PaymentTypeImages;
        TcxStyle *stClear;
        TcxStyle *stRed;
        void __fastcall mdCarsCalcFields(TDataSet *DataSet);
        void __fastcall mdOrdersCalcFields(TDataSet *DataSet);
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSummaryGroupDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryGroupDemoDataDM *SummaryGroupDemoDataDM;
//---------------------------------------------------------------------------
#endif

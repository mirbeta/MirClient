//---------------------------------------------------------------------------

#ifndef SummaryMultiDemoDataH
#define SummaryMultiDemoDataH
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
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TSummaryMultiDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
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
        TcxStyleRepository *StyleRepository;
        TcxStyle *styleGold;
        TcxStyle *styleBlueLight;
        TcxStyle *styleYellowLight;
        TcxStyle *styleBlue;
        TcxStyle *styleRed;
        TcxStyle *styleNormal;
        TcxStyle *styleSortedSummary;
        TImageList *PaymentTypeImages;
		void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSummaryMultiDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryMultiDemoDataDM *SummaryMultiDemoDataDM;
//---------------------------------------------------------------------------
#endif

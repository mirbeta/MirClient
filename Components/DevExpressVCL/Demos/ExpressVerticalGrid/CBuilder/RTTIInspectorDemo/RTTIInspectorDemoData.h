//---------------------------------------------------------------------------

#ifndef RTTIInspectorDemoDataH
#define RTTIInspectorDemoDataH
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
class TRTTIInspectorDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
        TdxMemData *mdCustomers;
        TDataSource *dsCustomers;
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
        TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetUserFormat3;
        TImageList *PaymentTypeImages;
	TdxMemData *mdOrders;
	TAutoIncField *mdOrdersID;
	TIntegerField *mdOrdersCustomerID;
	TIntegerField *mdOrdersProductID;
	TDateTimeField *mdOrdersPurchaseDate;
	TDateTimeField *mdOrdersTime;
	TStringField *mdOrdersPaymentType;
	TFloatField *mdOrdersPaymentAmount;
	TMemoField *mdOrdersDescription;
	TIntegerField *mdOrdersQuantity;
	TStringField *mdOrdersCustomerEmail;
	TStringField *mdOrdersCarsTrademark;
	TStringField *mdOrdersCarsModel;
	TStringField *mdOrdersCar;
	void __fastcall DataModuleCreate(TObject *Sender);
	void __fastcall mdOrdersCalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
  __fastcall TRTTIInspectorDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRTTIInspectorDemoMainDM *RTTIInspectorDemoMainDM;
//---------------------------------------------------------------------------
#endif

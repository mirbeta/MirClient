//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DemoBasicDM.h"
//---------------------------------------------------------------------
#pragma link "cxClasses"
#pragma resource "*.dfm"
TdmOrders *dmOrders;
//---------------------------------------------------------------------
__fastcall TdmOrders::TdmOrders(TComponent* AOwner)
	: TDataModule(AOwner)
{
}
//---------------------------------------------------------------------
void __fastcall TdmOrders::cdsCarsCalcFields(TDataSet *DataSet)
{
  cdsCarsCarName->Value = cdsCarsTrademark->Value + ": " + cdsCarsModel->Value;
}
//---------------------------------------------------------------------------

void __fastcall TdmOrders::cdsOrdersCalcFields(TDataSet *DataSet)
{
  cdsOrdersPaymentAmount->Value = cdsOrdersQuantity->Value * cdsOrdersUnitPrice->Value;
}
//---------------------------------------------------------------------------


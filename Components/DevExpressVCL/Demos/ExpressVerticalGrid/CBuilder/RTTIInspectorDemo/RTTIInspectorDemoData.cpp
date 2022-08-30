//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RTTIInspectorDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TRTTIInspectorDemoMainDM *RTTIInspectorDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TRTTIInspectorDemoMainDM::TRTTIInspectorDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRTTIInspectorDemoMainDM::DataModuleCreate(TObject *Sender)
{
  mdCustomers->LoadFromBinaryFile("..\\..\\Data\\Customers.dat");
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\Orders.dat");
  mdCustomers->Open();
  mdOrders->Open();
}
//---------------------------------------------------------------------------

void __fastcall TRTTIInspectorDemoMainDM::mdOrdersCalcFields(TDataSet *DataSet)
{
  mdOrdersCar->AsString = mdOrdersCarsTrademark->AsString + ' ' + mdOrdersCarsModel->AsString;
}
//---------------------------------------------------------------------------



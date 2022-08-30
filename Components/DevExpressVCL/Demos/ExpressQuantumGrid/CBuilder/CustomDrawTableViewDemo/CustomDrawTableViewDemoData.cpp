//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawTableViewDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TCustomDrawTableViewDemoMainDM *CustomDrawTableViewDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TCustomDrawTableViewDemoMainDM::TCustomDrawTableViewDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawTableViewDemoMainDM::DataModuleCreate(TObject *Sender)
{
  cdsCustomers->LoadFromFile("..\\..\\Data\\CustomDrawTableViewDemo_Customers.xml");
  cdsCustomers->Open();
  cdsOrders->LoadFromFile("..\\..\\Data\\CustomDrawTableViewDemo_Orders.xml");
  cdsOrders->Open();
}
//---------------------------------------------------------------------------

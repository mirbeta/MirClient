//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TCustomDrawDemoDataDM *CustomDrawDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TCustomDrawDemoDataDM::TCustomDrawDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawDemoDataDM::CustomDrawDemoDataDMCreate(TObject *Sender)
{
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\OrdersCarsCustomers.dat");
  mdOrders->Open();
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TdmDemo *dmDemo;
//---------------------------------------------------------------------------
__fastcall TdmDemo::TdmDemo(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmDemo::DataModuleCreate(TObject* Sender)
{
  mdCars->LoadFromBinaryFile(ExtractFilePath(Application->ExeName) + "..\\..\\Data\\Cars.dat");
  mdCustomers->LoadFromBinaryFile(ExtractFilePath(Application->ExeName) + "..\\..\\Data\\Customers.dat");
  mdOrders->LoadFromBinaryFile(ExtractFilePath(Application->ExeName) + "..\\..\\Data\\OrdersCarsCustomers.dat");
}

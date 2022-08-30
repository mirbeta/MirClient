//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SummaryGroupDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TSummaryGroupDemoDataDM *SummaryGroupDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TSummaryGroupDemoDataDM::TSummaryGroupDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSummaryGroupDemoDataDM::mdCarsCalcFields(
      TDataSet *DataSet)
{
  mdCarsCarName->AsString = mdCarsTrademark->AsString + " " +
    mdCarsModel->AsString;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoDataDM::mdOrdersCalcFields(
      TDataSet *DataSet)
{
  #if (__BORLANDC__ >= 0x0610) // C++Builder 12
    UnicodeString s;
  #else
    AnsiString s;
  #endif
  DateTimeToString(s, "mmmm", mdOrdersPurchaseDate->AsDateTime);
  mdOrdersPurchaseMonth->AsString = s;
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdCars->LoadFromBinaryFile("..\\..\\Data\\Cars.dat");
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\Orders.dat");
  mdCustomers->LoadFromBinaryFile("..\\..\\Data\\Customers.dat");
  mdCars->Open();
  mdOrders->Open();
  mdCustomers->Open();
}
//---------------------------------------------------------------------------


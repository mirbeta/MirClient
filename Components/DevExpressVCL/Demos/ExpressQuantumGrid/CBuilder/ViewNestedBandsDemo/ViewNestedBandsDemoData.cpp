//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewNestedBandsDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxDBEditRepository"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGridCardView"
#pragma resource "*.dfm"
TViewNestedBandsDemoDataDM *ViewNestedBandsDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TViewNestedBandsDemoDataDM::TViewNestedBandsDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewNestedBandsDemoDataDM::tblCustomersCalcFields(TDataSet *DataSet)
{
  tblCustomersName->AsString = tblCustomersFirstName->AsString + " " +
    tblCustomersLastName->AsString;
}
//---------------------------------------------------------------------------
void __fastcall TViewNestedBandsDemoDataDM::DataModuleCreate(TObject *Sender)
{
  String APath =  ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  tblCustomers->LoadFromFile(APath + "Customers.xml");
  tblOrders->LoadFromFile(APath + "Orders.xml");
}
//---------------------------------------------------------------------------


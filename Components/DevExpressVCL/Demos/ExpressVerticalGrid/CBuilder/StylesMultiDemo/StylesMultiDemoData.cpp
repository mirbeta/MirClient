//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesMultiDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TStylesMultiDemoDataDM *StylesMultiDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TStylesMultiDemoDataDM::TStylesMultiDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void PopulateStyleSheetsList(TList *AList)
{
  if (AList != NULL){
    AList->Clear();
    TcxStyleRepository *AStRep = StylesMultiDemoDataDM->strepUserDefined;
     for (int I = 0; I < AStRep->StyleSheetCount; I++)
        AList->Add(AStRep->StyleSheets[I]);
  }
}

void __fastcall TStylesMultiDemoDataDM::StylesMultiDemoDataDMCreate(TObject *Sender)
{
  mdCustomers->LoadFromBinaryFile("..\\..\\Data\\Customers.dat");
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\Orders.dat");
  mdCustomers->Open();
  mdOrders->Open();
}
//---------------------------------------------------------------------------


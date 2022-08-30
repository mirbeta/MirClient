//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TStylesSimpleDemoDataDM *StylesSimpleDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoDataDM::TStylesSimpleDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoDataDM::StylesSimpleDemoDataDMCreate(TObject *Sender)
{
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\OrdersCarsCustomers.dat");
  mdOrders->Open();
}
//---------------------------------------------------------------------------


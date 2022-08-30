//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RowsMultiEditorsDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TRowsMultiEditorsDemoDataDM *RowsMultiEditorsDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TRowsMultiEditorsDemoDataDM::TRowsMultiEditorsDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TRowsMultiEditorsDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdOrders->LoadFromBinaryFile("..\\..\\Data\\OrdersCarsCustomers.dat");
  mdOrders->Open();
}
//---------------------------------------------------------------------------


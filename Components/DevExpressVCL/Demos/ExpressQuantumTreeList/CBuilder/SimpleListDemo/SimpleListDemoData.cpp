//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleListDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TSimpleListDemoDataDM *SimpleListDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TSimpleListDemoDataDM::TSimpleListDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------


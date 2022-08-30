//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleVerticalGridDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TSimpleVerticalGridDemoMainDM *SimpleVerticalGridDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TSimpleVerticalGridDemoMainDM::TSimpleVerticalGridDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

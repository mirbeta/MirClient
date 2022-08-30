//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnboundModeDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma resource "*.dfm"
TUnboundModeDemoDataDM *UnboundModeDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TUnboundModeDemoDataDM::TUnboundModeDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

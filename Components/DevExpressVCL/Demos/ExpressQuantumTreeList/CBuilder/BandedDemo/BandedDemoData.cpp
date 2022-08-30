//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BandedDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma resource "*.dfm"
TBandedDemoDataDM *BandedDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TBandedDemoDataDM::TBandedDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void TBandedDemoDataDM::SetParentValue(Variant AValue)
{
  if (mdDepartments->State == dsEdit ||
    mdDepartments->State == dsInsert)
    mdDepartments->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------


void __fastcall TBandedDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdPersons->LoadFromBinaryFile("..\\..\\Data\\Persons.dat");
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdPersons->Open();
  mdDepartments->Open();
}
//---------------------------------------------------------------------------


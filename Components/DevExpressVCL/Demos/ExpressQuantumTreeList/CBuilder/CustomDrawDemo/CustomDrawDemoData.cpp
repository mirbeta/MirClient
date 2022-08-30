//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TCustomDrawDemoDataDM *CustomDrawDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TCustomDrawDemoDataDM::TCustomDrawDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void TCustomDrawDemoDataDM::SetParentValue(Variant AValue)
{
  if ((mdDepartments->State == dsEdit) || (mdDepartments->State == dsInsert))
	mdDepartments->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------


void __fastcall TCustomDrawDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdPersons->LoadFromBinaryFile("..\\..\\Data\\Persons.dat");
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdPersons->Open();
  mdDepartments->Open();
}
//---------------------------------------------------------------------------


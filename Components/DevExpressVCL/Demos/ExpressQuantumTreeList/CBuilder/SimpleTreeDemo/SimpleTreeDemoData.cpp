//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleTreeDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TSimpleTreeDemoDataDM *SimpleTreeDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TSimpleTreeDemoDataDM::TSimpleTreeDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoDataDM::SetParentValue(Variant AValue)
{
  if (mdDepartments->State == dsEdit || mdDepartments->State == dsInsert)
    mdDepartments->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------


void __fastcall TSimpleTreeDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdDepartments->Open();
}
//---------------------------------------------------------------------------


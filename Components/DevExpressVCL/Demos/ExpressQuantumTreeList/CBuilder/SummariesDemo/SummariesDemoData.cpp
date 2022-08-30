//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SummariesDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TSummariesDemoDataDM *SummariesDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TSummariesDemoDataDM::TSummariesDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdDepartments->Open();
}
//---------------------------------------------------------------------------


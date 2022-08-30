//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsLookupDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma resource "*.dfm"
TEditorsLookupDemoDataDM *EditorsLookupDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TEditorsLookupDemoDataDM::TEditorsLookupDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoDataDM::tblUsersCalcFields(TDataSet *DataSet)
{
  tblUsersUserName->AsString = tblUsersFNAME->AsString + " " +
	tblUsersLNAME->AsString;
}
//---------------------------------------------------------------------------
void __fastcall TEditorsLookupDemoDataDM::DataModuleCreate(TObject *Sender)
{
	tblItems->LoadFromFile("..\\..\\Data\\Items.xml");
	tblProjects->LoadFromFile("..\\..\\Data\\Projects.xml");
	tblUsers->LoadFromFile("..\\..\\Data\\Users.xml");
	tblDepartments->LoadFromFile("..\\..\\Data\\Departments.xml");
}
//---------------------------------------------------------------------------




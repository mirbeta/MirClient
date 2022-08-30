//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxDBEditRepository"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TdmMain *dmMain;
//---------------------------------------------------------------------------
__fastcall TdmMain::TdmMain(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmMain::DataModuleCreate(TObject *Sender)
{
  cdsDepartments->LoadFromFile("..\\..\\Data\\Departments.xml");
  cdsDepartments->Open();
  cdsUsers->LoadFromFile("..\\..\\Data\\Users.xml");
  cdsUsers->Open();
  cdsItems->LoadFromFile("..\\..\\Data\\Items.xml");
  cdsItems->Open();
  cdsProjects->LoadFromFile("..\\..\\Data\\Projects.xml");
  cdsProjects->Open();
  cdsTeam->LoadFromFile("..\\..\\Data\\Team.xml");
  cdsTeam->Open();
  cdsScheduler->LoadFromFile("..\\..\\Data\\Scheduler.xml");
  cdsScheduler->Open();
}
//---------------------------------------------------------------------------

void __fastcall TdmMain::cdsUsersCalcFields(TDataSet *DataSet)
{
  cdsUsersFullName->AsString = cdsUsersFNAME->AsString + ' ' +
    cdsUsersMNAME->AsString + ' ' + cdsUsersLNAME->AsString;
}
//---------------------------------------------------------------------------

void __fastcall TdmMain::cdsSchedulerCalcFields(TDataSet *DataSet)
{
  cdsSchedulerRowSum->AsFloat = cdsSchedulerSUNDAY->AsFloat +
    cdsSchedulerMONDAY->AsFloat + cdsSchedulerTUESDAY->AsFloat +
    cdsSchedulerWEDNESDAY->AsFloat + cdsSchedulerTHURSDAY->AsFloat +
    cdsSchedulerFRIDAY->AsFloat + cdsSchedulerSATURDAY->AsFloat;
  cdsSchedulerRowAvg->AsString = FormatFloat("0.00", cdsSchedulerRowSum->AsFloat / 7);
}
//---------------------------------------------------------------------------


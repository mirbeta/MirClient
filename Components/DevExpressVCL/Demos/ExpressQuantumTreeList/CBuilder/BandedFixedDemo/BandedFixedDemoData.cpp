//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BandedFixedDemoData.h"
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
TBandedFixedDemoDataDM *BandedFixedDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TBandedFixedDemoDataDM::TBandedFixedDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoDataDM::mdShedulerCalcFields(TDataSet *DataSet)
{
  mdShedulerWeekSum->Value = mdShedulerSUNDAY->Value + mdShedulerMONDAY->Value +
    mdShedulerTUESDAY->Value + mdShedulerWEDNESDAY->Value + mdShedulerTHURSDAY->Value +
    mdShedulerFRIDAY->Value + mdShedulerSATURDAY->Value;
  mdShedulerWeekAVG->Value = mdShedulerWeekSum->Value/7;
}
//---------------------------------------------------------------------------

String TBandedFixedDemoDataDM::GetProjectNameByID(int AProjectID)
{
  return mdProjects->Lookup("ID", AProjectID, "Name");
}
//---------------------------------------------------------------------------

String TBandedFixedDemoDataDM::GetPersonNameByID(int APersonID)
{
  return mdPersons->Lookup("ID", APersonID, "Name");
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdPersons->LoadFromBinaryFile("..\\..\\Data\\Persons.dat");
  mdProjects->LoadFromBinaryFile("..\\..\\Data\\Projects.dat");
  mdSheduler->LoadFromBinaryFile("..\\..\\Data\\Scheduler.dat");
  mdPersons->Open();
  mdProjects->Open();
  mdSheduler->Open();
}
//---------------------------------------------------------------------------


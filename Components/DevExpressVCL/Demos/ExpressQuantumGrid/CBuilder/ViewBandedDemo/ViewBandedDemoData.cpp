//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewBandedDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridBandedTableView"
#pragma link "cxStyles"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TViewBandedDemoDataDM *ViewBandedDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TViewBandedDemoDataDM::TViewBandedDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedDemoDataDM::mdUsersCalcFields(
	  TDataSet *DataSet)
{
  mdUsersName->Value = mdUsersFNAME->Value + " " + mdUsersLNAME->Value;
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdProjects->LoadFromBinaryFile("..\\..\\Data\\Projects.dat");
  mdItems->LoadFromBinaryFile("..\\..\\Data\\Items.dat");
  mdUsers->LoadFromBinaryFile("..\\..\\Data\\Users.dat");
  mdProjects->Open();
  mdItems->Open();
  mdUsers->Open();
}
//---------------------------------------------------------------------------


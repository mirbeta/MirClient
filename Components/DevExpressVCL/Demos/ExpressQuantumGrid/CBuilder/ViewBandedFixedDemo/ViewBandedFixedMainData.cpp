//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewBandedFixedMainData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridBandedTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"

TViewBandedFixedDemoDMMain *ViewBandedFixedDemoDMMain;

//---------------------------------------------------------------------------
__fastcall TViewBandedFixedDemoDMMain::TViewBandedFixedDemoDMMain(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TViewBandedFixedDemoDMMain::tblSCHEDULERCalcFields(TDataSet *DataSet)
{
  String Val;
  RowSum->AsFloat = SUNDAY->AsFloat +  MONDAY->AsFloat +  TUESDAY->AsFloat +   WEDNESDAY->AsFloat +
  THURSDAY->AsFloat +  FRIDAY->AsFloat +  SATURDAY->AsFloat;
  RowAvg->AsString = FormatFloat("0.0", RowSum->AsFloat / 7);

  if(MiddleName->Value != "")
	Val = Format(AnsiString("%s %s %s"), ARRAYOFCONST((FirstName->Value, MiddleName->Value, LastName->Value)));
  else
	Val = Format(AnsiString("%s %s"), ARRAYOFCONST((FirstName->Value, LastName->Value)));
  UserName->Value = Val;
}
//---------------------------------------------------------------------------
void __fastcall TViewBandedFixedDemoDMMain::DataModuleCreate(TObject *Sender)
{
	String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
	tblUSERS->LoadFromFile(APath + "Users.xml");
	tblSCHEDULER->LoadFromFile(APath + "Scheduler.xml");
	tblPROJECTS->LoadFromFile(APath + "Projects.xml");	
}
//---------------------------------------------------------------------------


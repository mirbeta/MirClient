//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewCardDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxDBEditRepository"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGridCardView"
#pragma resource "*.dfm"
TViewCardDemoDataDM *ViewCardDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TViewCardDemoDataDM::TViewCardDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TViewCardDemoDataDM::tblPersonsCalcFields(TDataSet *DataSet)
{
  tblPersonsFullName->AsString = tblPersonsFIRSTNAME->AsString + " " +
    tblPersonsSECONDNAME->AsString + " " + "(" +
    IntToStr(tblPersonsID->AsInteger) + ")";
}
//---------------------------------------------------------------------------
void __fastcall TViewCardDemoDataDM::DataModuleCreate(TObject *Sender)
{
	String APath =  ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
	tblPersons->LoadFromFile(APath + "Persons.xml");
	tblCountries->LoadFromFile(APath + "Countries.xml");
}
//---------------------------------------------------------------------------



//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesCardViewDemoData.h"
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
TStylesCardViewDemoMainDM *StylesCardViewDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TStylesCardViewDemoMainDM::TStylesCardViewDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesCardViewDemoMainDM::DataModuleCreate(TObject *Sender)
{
  cdsPersons->LoadFromFile("..\\..\\Data\\Persons.xml");
  cdsPersons->Open();
  cdsCountries->LoadFromFile("..\\..\\Data\\Countries.xml");
  cdsCountries->Open();
}
//---------------------------------------------------------------------------

void __fastcall TStylesCardViewDemoMainDM::cdsPersonsCalcFields(TDataSet *DataSet)
{
  cdsPersonsFullName->Value = cdsPersonsFIRSTNAME->Value + " " +
    cdsPersonsSECONDNAME->Value + " " + "(" +
    IntToStr(cdsPersonsID->Value) + ")";
}
//---------------------------------------------------------------------------


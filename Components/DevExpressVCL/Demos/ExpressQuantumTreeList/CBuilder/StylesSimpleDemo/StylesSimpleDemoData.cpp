//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TStylesSimpleDemoDataDM *StylesSimpleDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoDataDM::TStylesSimpleDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoDataDM::SetParentValue(Variant AValue)
{
  if (mdDEPARTMENTS->State == dsEdit || mdDEPARTMENTS->State == dsInsert)
    mdDEPARTMENTS->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------


void __fastcall TStylesSimpleDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdPERSONS->LoadFromBinaryFile("..\\..\\Data\\Persons.dat");
  mdDEPARTMENTS->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdPERSONS->Open();
  mdDEPARTMENTS->Open();
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesMultiDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TStylesMultiDemoDataDM *StylesMultiDemoDataDM;
//---------------------------------------------------------------------------

void PopulateStyleSheetsList(TList *AList)
{
  if (AList != NULL){
    TcxStyleRepository *ARepository = StylesMultiDemoDataDM->strepUserDefined;
    AList->Clear();
     for (int I = 0; I < ARepository->StyleSheetCount; I++)
        AList->Add(ARepository->StyleSheets[I]);
  }
}
//---------------------------------------------------------------------------

__fastcall TStylesMultiDemoDataDM::TStylesMultiDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoDataDM::SetParentValue(Variant AValue)
{
  if (mdDepartments->State == dsEdit || mdDepartments->State == dsInsert)
    mdDepartments->FindField("ParentID")->Value = AValue;
}
//---------------------------------------------------------------------------



void __fastcall TStylesMultiDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdPersons->LoadFromBinaryFile("..\\..\\Data\\Persons.dat");
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdPersons->Open();
  mdDepartments->Open();
}
//---------------------------------------------------------------------------


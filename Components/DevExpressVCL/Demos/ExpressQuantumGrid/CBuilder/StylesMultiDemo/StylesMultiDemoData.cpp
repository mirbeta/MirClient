//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesMultiDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxDBEditRepository"
#pragma link "cxEdit"
#pragma resource "*.dfm"
TStylesMultiDemoMainDM *StylesMultiDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TStylesMultiDemoMainDM::TStylesMultiDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainDM::tblUsersCalcFields(TDataSet *DataSet)
{
 tblUsersFullName->AsString = tblUsersFNAME->AsString + " " +
   tblUsersMNAME->AsString + " " + tblUsersLNAME->AsString;
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainDM::PopulateStyleSheetsList(TList *AList)
{
  if (AList != NULL)
  {
    AList->Clear();
     for (int I = 0; I < strepUserDefined->StyleSheetCount; I++)
        AList->Add(strepUserDefined->StyleSheets[I]);
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainDM::DataModuleCreate(TObject *Sender)
{
	String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
	tblProjects->LoadFromFile(APath + "Projects.xml");
	tblTeam->LoadFromFile(APath + "ProjectTeam.xml");
	tblUsers->LoadFromFile(APath + "Users.xml");
}
//---------------------------------------------------------------------------




//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewCardSimpleDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TViewCardSimpleDemoMainDM *ViewCardSimpleDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TViewCardSimpleDemoMainDM::TViewCardSimpleDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TViewCardSimpleDemoMainDM::DataModuleCreate(TObject *Sender)
{
	String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
	tlbDEPARTMENTS->LoadFromFile(APath + "Departments.xml");
	tlbUSERS->LoadFromFile(APath + "Users.xml");
}
//---------------------------------------------------------------------------

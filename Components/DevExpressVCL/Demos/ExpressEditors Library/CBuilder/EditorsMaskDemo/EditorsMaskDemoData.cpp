//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsMaskDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TEditorsMaskDemoMainDM *EditorsMaskDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TEditorsMaskDemoMainDM::TEditorsMaskDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsMaskDemoMainDM::DataModuleCreate(TObject *Sender)
{
	cdsDepartments->LoadFromFile("..\\..\\Data\\Departments.xml");
	cdsUsers->LoadFromFile("..\\..\\Data\\Users.xml");		
};
//---------------------------------------------------------------------------

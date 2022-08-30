//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsLookupDemoNewUser.h"
#include "EditorsLookupDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDBEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookupEdit"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxDBLookupEdit"
#pragma resource "*.dfm"
TEditorsLookupDemoNewUserForm *EditorsLookupDemoNewUserForm;
//---------------------------------------------------------------------------
__fastcall TEditorsLookupDemoNewUserForm::TEditorsLookupDemoNewUserForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoNewUserForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  if (ModalResult == mrOk) EditorsLookupDemoDataDM->tblUsers->Post();
  else EditorsLookupDemoDataDM->tblUsers->Cancel();
}
//---------------------------------------------------------------------------

int TEditorsLookupDemoNewUserForm::ShowEx(String AName)
{
  DeleteSpaces(AName);
  int APos = AName.Pos(" ");
  String LName;
  if (APos != 0){
    LName = AName.SubString(APos + 1,AName.Length() - APos);
    AName = AName.SubString(1, APos - 1);
  }
  EditorsLookupDemoDataDM->tblUsers->Append();
  EditorsLookupDemoDataDM->tblUsersFNAME->AsString = AName;
  EditorsLookupDemoDataDM->tblUsersLNAME->AsString = LName;
  return ShowModal();
}
//---------------------------------------------------------------------------

void TEditorsLookupDemoNewUserForm::DeleteSpaces(String &AStr)
{
  AStr = Trim(AStr);
  int APos;
  while ((APos = AStr.Pos("  ")) != 0)
    AStr.Delete(APos + 1, 1);
}
//---------------------------------------------------------------------------



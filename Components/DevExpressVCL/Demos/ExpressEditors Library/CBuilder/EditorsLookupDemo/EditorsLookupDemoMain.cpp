//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "EditorsLookupDemoMain.h"
#include "EditorsLookupDemoData.h"
#include "AboutDemoForm.h"
#include "EditorsLookupDemoNewUser.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxDBEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxImageComboBox"
#pragma link "cxLookupEdit"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxTextEdit"
#pragma link "cxDBLookupEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma link "cxDBNavigator"
#pragma link "cxPropertiesStore"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TEditorsLookupDemoMainForm *EditorsLookupDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsLookupDemoMainForm::TEditorsLookupDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsLookupDemoMainForm::FormCreate(TObject *Sender)
{
  FStream = new TMemoryStream();
  cxPropertiesStore->StorageStream = FStream;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoMainForm::FormDestroy(TObject *Sender)
{
  if (cxPropertiesStore->StorageType == stStream)
    miIniStoreType->Click();
  delete FStream;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoMainForm::lcbCreatorNewLookupDisplayText(TObject *Sender,
    const TCaption AText)
{
  TcxDBLookupComboBox *ALookupControl = (TcxDBLookupComboBox*)Sender;
  ALookupControl->Reset();
  if (EditorsLookupDemoNewUserForm->ShowEx(AText) == mrOk) {
    ALookupControl->Text = EditorsLookupDemoDataDM->tblUsersUserName->AsString;
    ALookupControl->DataBinding->Field->Value =
      EditorsLookupDemoDataDM->tblUsersID->Value;
  }
  Abort();
}
//---------------------------------------------------------------------------


void __fastcall TEditorsLookupDemoMainForm::ChangeLookupModeClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  switch (((TMenuItem*)Sender)->Tag){
    case 1: SetPickLookupMode(); break;
    case 2: SetStandardLookupMode(); break;
    case 3: SetEditLookupMode(); break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoMainForm::StorageTypeClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  ((TMenuItem*)Sender)->Parent->Tag = ((TMenuItem*)Sender)->Tag;
  cxPropertiesStore->StorageType = (TcxStorageType)((TMenuItem*)Sender)->Tag;
  if (((TMenuItem*)Sender)->Tag == 0)
    cxPropertiesStore->StorageName = cxPropertiesStore->Name + ".ini";
  else
    cxPropertiesStore->StorageName = cxPropertiesStore->Name;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoMainForm::miStoreClick(TObject *Sender)
{
  cxPropertiesStore->StoreTo(true);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsLookupDemoMainForm::miRestoreClick(TObject *Sender)
{
  FStream->Position = 0;
  cxPropertiesStore->RestoreFrom();
}
//---------------------------------------------------------------------------

int TEditorsLookupDemoMainForm::IndexOfPropertiesStoreComponent(TComponent *AComponent)
{
  for (int I = 0; I < cxPropertiesStore->Components->Count; I++)
    if (cxPropertiesStore->Components->ComponentItems[I]->Component == AComponent)
      return I;
  return -1;
}

//---------------------------------------------------------------------------
void __fastcall TEditorsLookupDemoMainForm::miStorageActiveClick(TObject *Sender)
{
  cxPropertiesStore->Active = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void TEditorsLookupDemoMainForm::SetPickLookupMode()
{
  lcbCreator->Properties->DropDownListStyle = lsEditFixedList;
  lcbCreator->Properties->ImmediateDropDown = true;
  lcbOwner->Properties->DropDownListStyle = lsEditFixedList;
  lcbOwner->Properties->ImmediateDropDown = true;
  lcbProject->Properties->DropDownListStyle = lsEditFixedList;
}
//---------------------------------------------------------------------------

void TEditorsLookupDemoMainForm::SetStandardLookupMode()
{
  lcbCreator->Properties->DropDownListStyle = lsFixedList;
  lcbCreator->Properties->ImmediateDropDown = true;
  lcbOwner->Properties->DropDownListStyle = lsFixedList;
  lcbOwner->Properties->ImmediateDropDown = true;
  lcbProject->Properties->DropDownListStyle = lsFixedList;
}
//---------------------------------------------------------------------------

void TEditorsLookupDemoMainForm::SetEditLookupMode()
{
  lcbCreator->Properties->DropDownListStyle = lsEditList;
  lcbCreator->Properties->ImmediateDropDown = false;
  lcbOwner->Properties->DropDownListStyle = lsEditList;
  lcbOwner->Properties->ImmediateDropDown = false;
  lcbProject->Properties->DropDownListStyle = lsEditFixedList;
}
//---------------------------------------------------------------------------


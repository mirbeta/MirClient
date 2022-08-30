//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ColumnsMultiEditorsDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TColumnsMultiEditorsDemoMainForm *ColumnsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TColumnsMultiEditorsDemoMainForm::TColumnsMultiEditorsDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TColumnsMultiEditorsDemoMainForm::FormCreate(
      TObject *Sender)
{
  tvSkills->BeginUpdate();
  try {
    clnSkill->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
    clnGrade->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxVariantValueType);
    clnName->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  }  
  __finally {
    tvSkills->EndUpdate();
  }
   SkillDataSource = new TSkillDataSource(tvSkills,
        ImageComboLanguages->Properties->Items->Count,
        ImageComboCommunication->Properties->Items->Count);
  tvSkills->DataController->CustomDataSource = SkillDataSource;
  tvSkills->DataController->CustomDataSource->DataChanged();
  tvSkills->DataController->Groups->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::FormDestroy(
      TObject *Sender)
{
  tvSkills->DataController->CustomDataSource->Free();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::clnGradeGetProperties(
      TcxCustomGridTableItem *Sender, TcxCustomGridRecord *ARecord,
      TcxCustomEditProperties *&AProperties)
{
  switch (div(ARecord->RecordIndex, SkillCount).rem) {
    case 0: {AProperties = SpinItemYears->Properties; break;}
    case 1: {AProperties = ImageComboLanguages->Properties; break; }
    case 2: {AProperties = ImageComboLanguages->Properties; break; }
    case 3: {AProperties = ImageComboCommunication->Properties; break; }
    case 4: {AProperties = DateItemStartWorkFrom->Properties; break; }
  }
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::miEditButtonsAlwaysClick(
      TObject *Sender)
{
  if(tvSkills->OptionsView->ShowEditButtons != gsebAlways){
    ((TMenuItem*)Sender)->Checked = true;
    tvSkills->OptionsView->ShowEditButtons = gsebAlways;
  };
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::miEditButtonsFocusedRecordClick(
      TObject *Sender)
{
  if(tvSkills->OptionsView->ShowEditButtons != gsebForFocusedRecord) {
    ((TMenuItem*)Sender)->Checked = true;
    tvSkills->OptionsView->ShowEditButtons = gsebForFocusedRecord;
  };
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::miEditButtonsNeverClick(
      TObject *Sender)
{
  if(tvSkills->OptionsView->ShowEditButtons != gsebNever) {
    ((TMenuItem*)Sender)->Checked = true;
    tvSkills->OptionsView->ShowEditButtons = gsebNever;
  };
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::DateItemStartWorkFromPropertiesGetDayOfWeekState(TObject *Sender, TDay ADayOfWeek,
  TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor)
{
  if ((ADayOfWeek == dSaturday) || (ADayOfWeek == dSunday))
	AFont->Color = clRed;
}
//---------------------------------------------------------------------------

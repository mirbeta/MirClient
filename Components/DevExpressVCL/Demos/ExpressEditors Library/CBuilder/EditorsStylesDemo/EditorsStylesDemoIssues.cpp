//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoIssues.h"
#include "EditorsStylesDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxCheckComboBox"
#pragma link "cxCheckListBox"
#pragma link "cxClasses"
#pragma link "cxDBCheckComboBox"
#pragma link "cxDBCheckListBox"
#pragma link "cxDBEdit"
#pragma link "cxDBLabel"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBLookupEdit"
#pragma link "cxDBProgressBar"
#pragma link "cxDBTrackBar"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookupEdit"
#pragma link "cxMaskEdit"
#pragma link "cxNavigator"
#pragma link "cxProgressBar"
#pragma link "cxSpinEdit"
#pragma link "cxStyles"
#pragma link "cxTrackBar"
#pragma link "cxDBNavigator"
#pragma resource "*.dfm"
TEditorsStylesDemoIssuesFrame *EditorsStylesDemoIssuesFrame;
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoIssuesFrame::TEditorsStylesDemoIssuesFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  HintStyle = hcstBlueSlideUp;
  FDisplayStyle = shtRainyDay;
  FTempDisplayStyle = shtRainyDay;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoIssuesFrame::FillUsersCheckListBox()
{
  chlbUsers->Clear();
  EditorsStylesDemoDataDM->tblUsers->First();
  while (!EditorsStylesDemoDataDM->tblUsers->Eof) {
    chlbUsers->Items->Add()->Text =
        EditorsStylesDemoDataDM->tblUsersUserName->Value + " <" +
        EditorsStylesDemoDataDM->tblUsersEMAIL->Value + ">";
    EditorsStylesDemoDataDM->tblUsers->Next();
  }
  EditorsStylesDemoDataDM->tblItems->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoIssuesFrame::seFirstTargetPropertiesChange(
      TObject *Sender)
{
  pgbProgress->Properties->PeakValue = ((TcxDBSpinEdit*)Sender)->Value;
}
//---------------------------------------------------------------------------

String __fastcall __fastcall TEditorsStylesDemoIssuesFrame::Name()
{
  return "Issues database";
}
//---------------------------------------------------------------------------

String __fastcall __fastcall TEditorsStylesDemoIssuesFrame::BriefName()
{
  return "Issues";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoIssuesFrame::StylesIniPath()
{
  return "StylesFrmIssues\\";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoIssuesFrame::Description()
{
  return "Issues Database Notes";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoIssuesFrame::seProgressPropertiesChange(
      TObject *Sender)
{
  seCheckProgress->Properties->MaxValue = seProgress->Value;
  if (seCheckProgress->Value > seProgress->Value)
    seCheckProgress->Value = seProgress->Value;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoIssuesFrame::seCheckProgressPropertiesChange(
      TObject *Sender)
{
  pgbProgress->Properties->OverloadValue = seCheckProgress->Value;
}
//---------------------------------------------------------------------------


void __fastcall TEditorsStylesDemoIssuesFrame::FormShow(TObject *Sender)
{
  FillUsersCheckListBox();
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "Office11GroupRowStyleDemoMain.h"
#include "AboutDemoForm.h"
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
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxCalendar"
#pragma link "cxDropDownEdit"
#pragma link "cxDataStorage"
#pragma link "cxImageComboBox"
#pragma link "BaseForm"
#pragma link "dxmdaset"
#pragma resource "*.dfm"

TOffice11GroupRowStyleDemoMainForm *Office11GroupRowStyleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TOffice11GroupRowStyleDemoMainForm::TOffice11GroupRowStyleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
  mdPersons->LoadFromBinaryFile("..\\..\\Data\\USERS.dat");
  mdPersons->Open();
  AddRecordsIntoTable();
  UpdateMenu();
}
//---------------------------------------------------------------------------

void TOffice11GroupRowStyleDemoMainForm::UpdateMenu()
{
  miOffice11GroupRowStyle->Checked = tvMail->OptionsView->GroupRowStyle == grsOffice11;
  miGroupBySorting->Checked = tvMail->OptionsCustomize->GroupBySorting;
  miAlwaysExpandedGroups->Checked = tvMail->DataController->Options.Contains(dcoGroupsAlwaysExpanded);
  miDateTimeGrouping->Items[(int)tvMail->DateTimeHandling->Grouping - 1]->Checked = true;
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::tvMailStylesGetContentStyle(
      TcxCustomGridTableView *Sender, TcxCustomGridRecord *ARecord,
      TcxCustomGridTableItem *AItem, TcxStyle *&AStyle)
{
  if ((dynamic_cast<TcxGridDataRow*>(ARecord) != 0) && (ARecord->Values[tvMailIcon->Index] == 0))
    AStyle = UnreadStyle;
}
//---------------------------------------------------------------------------


void __fastcall TOffice11GroupRowStyleDemoMainForm::miOffice11GroupRowStyleClick(TObject *Sender)
{
  if (tvMail->OptionsView->GroupRowStyle == grsStandard)
    tvMail->OptionsView->GroupRowStyle = grsOffice11;
  else
    tvMail->OptionsView->GroupRowStyle = grsStandard;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::miGroupBySortingClick(TObject *Sender)
{
  tvMail->OptionsCustomize->GroupBySorting = !tvMail->OptionsCustomize->GroupBySorting;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::miAlwaysExpandedGroupsClick(TObject *Sender)
{
  if (tvMail->DataController->Options.Contains(dcoGroupsAlwaysExpanded))
    tvMail->DataController->Options =
      tvMail->DataController->Options >> dcoGroupsAlwaysExpanded;
  else
    tvMail->DataController->Options =
      tvMail->DataController->Options << dcoGroupsAlwaysExpanded;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::miDateTimeGroupingClick(TObject *Sender)
{
  tvMail->DateTimeHandling->Grouping = (TcxGridDateTimeGrouping)(1 + ((TMenuItem*)Sender)->MenuIndex);
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::AddRecordIntoTable(int ARecordIndex)
{
  tvMail->DataController->SetValue(ARecordIndex, tvMailImportance->Index, GetImportance());
  tvMail->DataController->SetValue(ARecordIndex, tvMailIcon->Index, GetIcon());
  tvMail->DataController->SetValue(ARecordIndex, tvMailAttachment->Index, GetIcon());
  tvMail->DataController->SetValue(ARecordIndex, tvMailFrom->Index, mdPersonsFullName->Text);
  tvMail->DataController->SetValue(ARecordIndex, tvMailSubject->Index, GetSubject());
  TDateTime ASent = GetSent();
  tvMail->DataController->SetValue(ARecordIndex, tvMailReceived->Index, GetReceived(ASent));
  tvMail->DataController->SetValue(ARecordIndex, tvMailSent->Index, ASent);
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::AddRecordsIntoTable()
{
  const int RecordCount = 10;
  Randomize();
  tvMail->BeginUpdate();
  mdPersons->DisableControls();
  try {
    tvMail->DataController->RecordCount = mdPersons->RecordCount * RecordCount;
    mdPersons->First();
    while (!mdPersons->Eof) {
      for (int J = 0; J < RecordCount; J++)
        AddRecordIntoTable((mdPersons->RecNo - 1) * RecordCount + J);
      mdPersons->Next();
    }
  }
  __finally {
    mdPersons->EnableControls();
    tvMail->EndUpdate();
  }
}
//---------------------------------------------------------------------------

int TOffice11GroupRowStyleDemoMainForm::GetImportance()
{
  int Result = random(10);
  if (Result > 2)
    Result = 1;
  return Result;
}
//---------------------------------------------------------------------------

int TOffice11GroupRowStyleDemoMainForm::GetIcon()
{
  int Result = random(4);
  if (Result > 1)
    Result = 1;
  return Result;
}
//---------------------------------------------------------------------------

TDateTime TOffice11GroupRowStyleDemoMainForm::GetSent()
{
  TDateTime Result = Now();
  if(random(6) == 1)
    return Result;
  Result = IncDay(Result, -random(50));
  Result = IncHour(Result, -random(4));
  Result = IncMinute(Result, -random(60));
  return Result;
}
//---------------------------------------------------------------------------

TDateTime TOffice11GroupRowStyleDemoMainForm::GetReceived(TDateTime ASent)
{
  return IncMinute(ASent, 10 + random(120));
}
//---------------------------------------------------------------------------

String TOffice11GroupRowStyleDemoMainForm::GetSubject()
{
  const int Count = 21;
  const String Subjects[Count] = {
    "Implementing the Developer Express MasterView control into an Accounting System.",
    "Web Edition: Data Entry Page. The date validation issue.",
    "Payables Due Calculator. It is ready for testing.",
    "Web Edition: Search Page. It is ready for testing.",
    "Main Menu: Duplicate Items. Somebody has to review all the menu items in the system.",
    "Receivables Calculator. Where can I find the complete specs",
    "Ledger: Inconsistency. Please fix it.",
    "Receivables Printing. It is ready for testing.",
    "Screen Redraw. Somebody has to look at it.",
    "Email System. What library are we going to use?",
    "Adding New Vendors Fails. This module doesn't work properly!",
    "History. Will we track the sales history in our system?",
    "Main Menu: Add a File menu. File menu is missing!!!",
    "Currency Mask. The current currency mask is extremely inconvenient.",
    "Drag & Drop. In the schedule module drag & drop is not available.",
    "Data Import. What competitors databases will we support?",
    "Reports. The list of incomplete reports.",
    "Data Archiving. This features is still missing in our application",
    "Email Attachments. How to add multiple attachments? I can't see how to do it.",
    "Check Register. We are using different paths for different modules.",
    "Data Export. Our customers asked for export into Excel"};
  return Subjects[random(Count)];
}
//---------------------------------------------------------------------------

void __fastcall TOffice11GroupRowStyleDemoMainForm::mdPersonsCalcFields(TDataSet *DataSet)
{
  mdPersonsFullName->AsString = mdPersonsFNAME->AsString + " " +
    mdPersonsLNAME->AsString;
}
//---------------------------------------------------------------------------


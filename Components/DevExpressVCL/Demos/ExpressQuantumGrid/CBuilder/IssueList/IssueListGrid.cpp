//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListGrid.h"
#include "IssueListConst.h"
#include "IssueListData.h"
#include "IssueListFrames.h"
#include "IssueListProjects.h"
#include "IssueListItems.h"
#include "IssueListUsers.h"
#include "IssueListDepartments.h"
#include "IssueListTeams.h"
#include "IssueListSchedule.h"

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
#pragma link "cxGridBandedTableView"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridPopupMenu"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxBlobEdit"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TIssueListGridForm *IssueListGridForm;
//---------------------------------------------------------------------------
__fastcall TIssueListGridForm::TIssueListGridForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TIssueListGridForm::cxGridActiveTabChanged(
      TcxCustomGrid *Sender, TcxGridLevel *ALevel)
{
  ChangeDescription(ALevel->Tag);
  if (ALevel == lvDepartments || ALevel == lvUsers)
    ChangeUsersLevel(ALevel);
  if (ALevel == lvProjects || ALevel == lvItems)
    ChangeItemsLevel(ALevel);

}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::RegisterFrames()
{
  FrameManager()->RegisterFrame(ProjectsFrameID, "PROJECTS");
  FrameManager()->RegisterFrame(ItemsFrameID,"PROJECT ITEMS");
  FrameManager()->RegisterFrame(DepartmentsFrameID, "DEPARTMENTS");
  FrameManager()->RegisterFrame(UsersFrameID, "USERS");
  FrameManager()->RegisterFrame(TeamsFrameID,"PROJECT TEAMS");
  FrameManager()->RegisterFrame(ScheduleFrameID, "SCHEDULE");
}

void __fastcall TIssueListGridForm::FormCreate(TObject *Sender)
{
  RegisterFrames();
  GoToFrame(ProjectsFrameID);
  FIsDependsOnData = true;
}
//---------------------------------------------------------------------------
void __fastcall TIssueListGridForm::cxGridFocusedViewChanged(
      TcxCustomGrid *Sender, TcxCustomGridView *APrevFocusedView,
      TcxCustomGridView *AFocusedView)
{
  if (AFocusedView) {
    AFocusedView->BeginUpdate();
    try
    {
     TcxGridDBDataController *ADataController =
      (TcxGridDBDataController*)((TcxGridDBTableView*)AFocusedView)->DataController;
      if (ADataController->DataSet) {
        int AID = GetFrameIDByDataSetName(ADataController->DataSet->Name);
        GoToFrame(AID);
        if (ADataController->RecordCount == 0)
          ADataController->Append();
      }
      ((TcxGridTableController*)AFocusedView->Controller)->Customization = false;
    }
    __finally {
      AFocusedView->EndUpdate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TIssueListGridForm::FormActivate(TObject *Sender)
{
  ChangeDescription(1);
}
//---------------------------------------------------------------------------
void __fastcall TIssueListGridForm::tvItemsStylesGetContentStyle(
      TcxCustomGridTableView *Sender, TcxCustomGridRecord *ARecord,
      TcxCustomGridTableItem *AItem, TcxStyle *&AStyle)
{
 if (!IsDependsOnData)
   return;

  int AStatus = -1;
  if (ARecord && AItem)
    if (!VarIsNull(((TcxCustomGridRecord*)ARecord)->Values[tvItemsSTATUS->Index]))
     AStatus = VarAsType(((TcxCustomGridRecord*)ARecord)->Values[tvItemsSTATUS->Index], varInteger);

  switch (AStatus) {
    case 1: AStyle = dmMain->stNew;
            break;
    case 2: AStyle = dmMain->stPostponed;
            break;
    case 3: AStyle = dmMain->stFixed;
            break;
    case 4: AStyle = dmMain->stRejected;
  }
}
//---------------------------------------------------------------------------


void __fastcall TIssueListGridForm::GoToFrame(int AID)
{
  if (FrameManager()->CanCreate(AID)) {
    FrameManager()->CreateFrame(AID, CreateFrameByID(AID));
  }
  FrameManager()->ShowFrame(AID, pnlForm);
}
//---------------------------------------------------------------------------

int __fastcall TIssueListGridForm::GetFrameIDByDataSetName(const AnsiString ADataSetName)
{
  if (ADataSetName == "cdsItems")
    return ItemsFrameID;
  if (ADataSetName == "cdsProjects")
    return ProjectsFrameID;
  if (ADataSetName == "cdsUsers")
    return UsersFrameID;
  if (ADataSetName == "cdsTeam")
    return TeamsFrameID;
  if (ADataSetName == "cdsDepartments")
    return DepartmentsFrameID;
  if (ADataSetName == "cdsScheduler")
    return ScheduleFrameID;
  return -1;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::ChangeUsersLevel(TcxGridLevel *ALevel)
{
  tvUsers->BeginUpdate();
  try
  {
    lvDepartmentUsers->Visible =  (ALevel == lvDepartments);
    tvUsersDEPARTMENTID->Visible = false;
    if (ALevel == lvDepartments)
    {
      tvUsersDEPARTMENTID->GroupIndex = -1;
      lvDepartmentUsers->GridView = tvUsers;
      cxGrid->FocusedView = tvDepartments;
     }
    else
    {
     tvUsersDEPARTMENTID->GroupIndex = 0;
     lvUsers->GridView = tvUsers;
     cxGrid->FocusedView = tvUsers;
     }
   }
  __finally
  {
    tvUsers->EndUpdate();
   }
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::ChangeItemsLevel(TcxGridLevel *ALevel)
{
  tvItems->BeginUpdate();
  try
  {
    lvProjectItems->Visible = (ALevel == lvProjects);
    tvItemsPROJECTID->Visible = False;
    if (ALevel == lvProjects)
    {
     tvItemsPROJECTID->GroupIndex = -1;
     lvProjectItems->GridView = tvItems;
     cxGrid->FocusedView = tvProjects;
    }
    else
    {
      tvItemsPROJECTID->GroupIndex = 0;
      lvItems->GridView = tvItems;
      cxGrid->FocusedView = tvItems;
    }
  }
  __finally
  {
   tvItems->EndUpdate();
   }
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::ChangeDescription(int AIndex)
{
  String ADescription[6]  = {
	"A project list showing project managers and the issues for each project",
	"A list of issues with problem descriptions and classifications ",
	"A list of departments displaying employee details",
	"A list of employees working on a project showing their work status",
	"A list of employees displaying personal data",
	"An operating schedule of users working on a project during a week" };

  lbDesciption->Caption = ADescription[AIndex-1];
}
//---------------------------------------------------------------------------

TcxGridLevel* __fastcall TIssueListGridForm::GetGridLevelByTag(int ATag)
{
  for (int i=0; cxGrid->Levels->Count-1; i++) {
    if (cxGrid->Levels->Items[i]->Tag == ATag)
      return cxGrid->Levels->Items[i];
  }
  return NULL;
}
//---------------------------------------------------------------------------


TcxGridDBTableView* __fastcall TIssueListGridForm::GetFocusedView()
{
  return (TcxGridDBTableView*)cxGrid->FocusedView;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoGoProject(int AIndex)
{
  TcxGridLevel* ALevel = GetGridLevelByTag(AIndex);
  TcxGridTopDetailsSiteViewInfo* AInfo =
    (TcxGridTopDetailsSiteViewInfo*)cxGrid->ViewInfo->DetailsSiteViewInfo;
   if (AInfo)
    AInfo->ChangeActiveTab(ALevel, false);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowPictures(bool Value)
{
  TImageList* AImageList = NULL;
  if (Value)
    AImageList = dmMain->imStat;

  for (int i = 0; i < dmMain->edrepMain->Count; i++)
    if (TcxEditRepositoryImageComboBoxItem* icbItem = dynamic_cast<TcxEditRepositoryImageComboBoxItem
      *>(dmMain->edrepMain->Items[i]))
     icbItem->Properties->Images = AImageList;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowDescription(bool Value)
{
  lbDesciption->Visible = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowDependsOnData(bool Value)
{
  FIsDependsOnData = Value;
  tvItems->LayoutChanged(true);
  tvProjects->DataController->ClearDetails();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowNewItemRow(bool Value)
{
  FocusedView->OptionsView->NewItemRow = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetNativeStyle(bool Value)
{
  cxGrid->LookAndFeel->NativeStyle = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetLookAndFeelKind(TcxLookAndFeelKind AKind)
{
  cxGrid->LookAndFeel->Kind = AKind;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetEditorsShadow(bool Value)
{
  dmMain->edstcMain->Style->Shadow = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowIndicator(bool Value)
{
  FocusedView->OptionsView->Indicator = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowGrouping(bool Value)
{
  FocusedView->OptionsView->GroupByBox = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowHeader(bool Value)
{
  FocusedView->OptionsView->Header = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowFooter(bool Value)
{
  FocusedView->OptionsView->Footer = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetAutoWidth(bool Value)
{
  FocusedView->OptionsView->ColumnAutoWidth = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetInvertSelected(bool Value)
{
  FocusedView->OptionsSelection->InvertSelect = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetAutoPreview(bool Value)
{
  FocusedView->Preview->Visible = Value;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowEditButtons(bool Value)
{
  if (Value)
    FocusedView->OptionsView->ShowEditButtons = gsebForFocusedRecord;
  else
    FocusedView->OptionsView->ShowEditButtons = gsebNever;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoSetShowGridLines(bool Value)
{
  if (Value)
    FocusedView->OptionsView->GridLines = glBoth;
  else
   FocusedView->OptionsView->GridLines = glNone;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoFullCollapse()
{
  FocusedView->ViewData->Collapse(true);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoFullExpand()
{
  FocusedView->ViewData->Expand(true);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoColumnsCustomization()
{
  ((TcxGridTableController*)cxGrid->FocusedView->Controller)->Customization = true;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::DoBestFit()
{
 FocusedView->ApplyBestFit(NULL, False, False);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListGridForm::cxGridRootLevelStylesGetTabStyle(
      TcxGridLevel *Sender, TcxGridLevel *ATabLevel, TcxStyle *&AStyle)
{
  if (((TcxGridLevel*)ATabLevel)->GridView == cxGrid->ActiveView)
    AStyle = dmMain->stSelected;
}
//---------------------------------------------------------------------------

TfrmBasic* __fastcall TIssueListGridForm::CreateFrameByID(int AID)
{
  TfrmBasic* AFrame;
  switch (AID) {
    case ProjectsFrameID: {
      AFrame = new TfrmProjects(this);
      break;
    }
   case ItemsFrameID: {
     AFrame = new TfrmItems(this);
      break;
    }
   case DepartmentsFrameID: {
     AFrame = new TfrmDepartments(this);
      break;
    }
   case UsersFrameID: {
     AFrame = new TfrmUsers(this);
      break;
    }
   case TeamsFrameID: {
     AFrame = new TfrmTeams(this);
      break;
    }
   case ScheduleFrameID:
     AFrame = new TfrmSchedule(this);
  }
  return AFrame;
}
//---------------------------------------------------------------------------



//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "SummaryGroupDemoMain.h"
#include "SummaryGroupDemoData.h"
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
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxImageComboBox"
#pragma link "cxSpinEdit"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TSummaryGroupDemoMainForm *SummaryGroupDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSummaryGroupDemoMainForm::TSummaryGroupDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSummaryGroupDemoMainForm::FormCreate(TObject *Sender)
{
  UpdateMenu();
}
//---------------------------------------------------------------------------
void TSummaryGroupDemoMainForm::UpdateMenu()
{
  miSelectedRecordsOnly->Checked = tvOrders->DataController->Summary->Options.Contains(soSelectedRecords);
  miIgnoreNullValues->Checked = tvOrders->DataController->Summary->Options.Contains(soNullIgnore);
  miGroupSummaryLayout->Items[(int)tvOrders->OptionsView->GroupSummaryLayout]->Checked = true;
  miGroupFooter->Items[(int)tvOrders->OptionsView->GroupFooters]->Checked = true;
  miGroupFootersAtCarLevel->Checked = tvOrdersProductID->Options->GroupFooters;
  miGroupFootersAtCompanyLevel->Checked = tvOrdersCustomerID->Options->GroupFooters;
  miMultiSelect->Checked = tvOrders->OptionsSelection->MultiSelect;
}
 //---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::FormShow(TObject *Sender)
{
  tvOrders->DataController->Groups->FullExpand();
  tvOrders->Controller->GoToFirst(true);
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miSelectedRecordsOnlyClick(TObject *Sender)
{
  TcxSummaryOptions AOptions = tvOrders->DataController->Summary->Options;
  if (AOptions.Contains(soSelectedRecords))
    AOptions >> soSelectedRecords;
  else
    AOptions << soSelectedRecords;
  tvOrders->DataController->Summary->Options = AOptions;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miIgnoreNullValuesClick(TObject *Sender)
{
  TcxSummaryOptions AOptions = tvOrders->DataController->Summary->Options;
  if (AOptions.Contains(soNullIgnore))
    AOptions >> soNullIgnore;
  else
    AOptions << soNullIgnore;
  tvOrders->DataController->Summary->Options = AOptions;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  tvOrders->OptionsSelection->MultiSelect = !tvOrders->OptionsSelection->MultiSelect;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miGroupFooterShowClick(TObject *Sender)
{
  tvOrders->OptionsView->GroupFooters = (TcxGridGroupFootersMode)((TMenuItem*)Sender)->Tag;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miGroupSummaryLayoutClick(TObject *Sender)
{
  tvOrders->OptionsView->GroupSummaryLayout = (TcxGridGroupSummaryLayout)((TMenuItem*)Sender)->Tag;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSummaryGroupDemoMainForm::miGroupFootersAtLevelClick(TObject *Sender)
{
  TcxGridColumn* AColumn;
  if (((TMenuItem*)Sender)->Tag == 1)
    AColumn = tvOrdersProductID;
  else
    AColumn = tvOrdersCustomerID;
  AColumn->Options->GroupFooters = !AColumn->Options->GroupFooters;
  UpdateMenu();
}
//---------------------------------------------------------------------------


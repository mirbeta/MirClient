//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ViewBandedFixedMain.h"
#include "AboutDemoForm.h"
#include "ViewBandedFixedMainData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridPopupMenu"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TViewBandedFixedDemoMainForm *ViewBandedFixedDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TViewBandedFixedDemoMainForm::TViewBandedFixedDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedFixedDemoMainForm::FormShow(TObject *Sender)
{
  ((TcxGridTableView*)cxGrid->FocusedView)->DataController->Groups->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedFixedDemoMainForm::miShowBandsHeadersClick(TObject *Sender)
{
   btvUsersSchedule->OptionsView->BandHeaders = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedFixedDemoMainForm::miShowIndicatorClick(TObject *Sender)
{
  btvUsersSchedule->OptionsView->Indicator = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedFixedDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  btvUsersSchedule->OptionsSelection->MultiSelect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewBandedFixedDemoMainForm::miShowColumnsHeadersClick(TObject *Sender)
{
  btvUsersSchedule->OptionsView->Header = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------



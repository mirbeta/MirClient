//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "ViewCardSimpleDemoMain.h"
#include "ViewCardSimpleDemoData.h"
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
#pragma link "cxGridCardView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBCardView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma resource "*.dfm"
TViewCardSimpleDemoMainForm *ViewCardSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TViewCardSimpleDemoMainForm::TViewCardSimpleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewCardSimpleDemoMainForm::miInvertSelectClick(TObject *Sender)
{
  cxGridUsersDBCardView->OptionsSelection->InvertSelect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardSimpleDemoMainForm::miCellSelectClick(TObject *Sender)
{
  cxGridUsersDBCardView->OptionsSelection->CellSelect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardSimpleDemoMainForm::miHideFocusRectClick(TObject *Sender)
{
  cxGridUsersDBCardView->OptionsSelection->HideFocusRect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardSimpleDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  cxGridUsersDBCardView->OptionsSelection->MultiSelect = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardSimpleDemoMainForm::miShowNavigatorClick(TObject *Sender)
{
  cxGridUsersDBCardView->Navigator->Visible = ((TMenuItem*)Sender)->Checked;
}

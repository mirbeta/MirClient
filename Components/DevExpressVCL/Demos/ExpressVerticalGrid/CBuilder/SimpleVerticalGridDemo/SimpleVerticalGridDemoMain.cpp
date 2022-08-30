//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleVerticalGridDemoMain.h"
#include "SimpleVerticalGridDemoData.h"
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
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBVGrid"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TSimpleVerticalGridDemoMainForm *SimpleVerticalGridDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSimpleVerticalGridDemoMainForm::TSimpleVerticalGridDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::actBandSizingExecute(TObject *Sender)
{
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
  cxDBVerticalGrid->OptionsBehavior->BandSizing = ((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::actCellHintsExecute(TObject *Sender)
{
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
  cxDBVerticalGrid->OptionsBehavior->CellHints = ((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::actRowSizingExecute(TObject *Sender)
{
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
  cxDBVerticalGrid->OptionsBehavior->RowSizing = ((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::actImmediateEditorExecute(TObject *Sender)
{
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
  cxDBVerticalGrid->OptionsBehavior->ImmediateEditor = ((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::actPaintStyleExecute(TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked){
    ((TMenuItem*)Sender)->Checked = true;
    cxDBVerticalGrid->OptionsView->PaintStyle =
      (TcxvgPaintStyle)((TMenuItem*)Sender)->Tag;
    switch (cxDBVerticalGrid->OptionsView->PaintStyle){
      case psdotNet:
        cxDBVerticalGrid->Styles->StyleSheet =
          SimpleVerticalGridDemoMainDM->cxVerticalGridStyleSheetDevExpress;
        cxDBVerticalGrid->Styles->OnGetContentStyle =
          cxDBVerticalGridStylesGetContentStyle;
        break;
      case psDelphi:
        cxDBVerticalGrid->Styles->StyleSheet = NULL;
        cxDBVerticalGrid->Styles->OnGetContentStyle = NULL;
    }
    cxDBVerticalGrid->LayoutChanged();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::cxDBVerticalGridStylesGetContentStyle(
  TObject *Sender, TcxCustomEditorRowProperties *AEditProp, bool AFocused,
  int ARecordIndex, TcxStyle *&AStyle)
{
  if (ARecordIndex == cxDBVerticalGrid->DataController->FocusedRowIndex)
    AStyle = SimpleVerticalGridDemoMainDM->cxStyle8;
  else
    AStyle = SimpleVerticalGridDemoMainDM->cxStyle3;
}
//---------------------------------------------------------------------------


void __fastcall TSimpleVerticalGridDemoMainForm::LayOutStyleExecute(TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked){
    ((TMenuItem*)Sender)->Checked = true;
    cxDBVerticalGrid->LayoutStyle = (TcxvgLayoutStyle)((TMenuItem*)Sender)->Tag;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::miExplorerStyleCategoryClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBVerticalGrid->OptionsView->CategoryExplorerStyle = ((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Checked)
    cxDBVerticalGrid->Styles->Category = SimpleVerticalGridDemoMainDM->cxStyle1;
  else
    cxDBVerticalGrid->Styles->Category = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::miGridLinesClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Checked)
    cxDBVerticalGrid->OptionsView->GridLines = vglBoth;
  else
    cxDBVerticalGrid->OptionsView->GridLines = vglNone;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::miHeadersClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBVerticalGrid->OptionsView->ShowHeaders = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleVerticalGridDemoMainForm::miIncSearchClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBVerticalGrid->OptionsBehavior->IncSearch = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------



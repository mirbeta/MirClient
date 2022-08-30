//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "ViewTableSimpleDemoMain.h"
#include "ViewTableSimpleDemoData.h"
#include "dialogs.hpp"
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
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMemo"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TViewTableSimpleDemoMainForm *ViewTableSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TViewTableSimpleDemoMainForm::TViewTableSimpleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::FocusRomanceCategory()
{
  for(int i=0; i < cxgGenrasDBTableView->ViewInfo->RecordsViewInfo->Count; i++) {
    if(VarToStr(cxgGenrasDBTableView->ViewData->Records[i]->Values[0]) == "Romance") {
      cxgGenrasDBTableView->ViewData->Records[i]->Focused = true;
      break;
	}
  }
}

void __fastcall TViewTableSimpleDemoMainForm::FormShow(TObject *Sender)
{
  ((TcxGridTableView*)cxgFilms->FocusedView)->DataController->Groups->FullExpand();
  FocusRomanceCategory();
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miShowIndicatorClick(TObject *Sender)
{
  cxgFilmsDBTableView->OptionsView->Indicator = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miShowPreviewRowClick( TObject *Sender)
{
   cxgFilmsDBTableViewTAGLINE->IsPreview = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miFocusCellOnTabClick(TObject *Sender)
{
  cxgFilmsDBTableView->OptionsBehavior->FocusCellOnTab = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miIncSearchClick(TObject *Sender)
{
  cxgFilmsDBTableView->OptionsBehavior->IncSearch = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miImmediateEditorClick(
      TObject *Sender)
{
  cxgFilmsDBTableView->OptionsBehavior->ImmediateEditor = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  cxgFilmsDBTableView->OptionsSelection->MultiSelect = ((TMenuItem*)Sender)->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainForm::miShowNavigatorClick(TObject *Sender)
{
  cxgFilmsDBTableView->Navigator->Visible = ((TMenuItem*)Sender)->Checked;
}


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoMain.h"
#include "StylesSimpleDemoData.h"
#include "StylesSimpleDemoStylesDialog.h"
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
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxMRUEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoMainForm::TStylesSimpleDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

bool __fastcall TStylesSimpleDemoMainForm::IsNativeDefaultStyle()
{
  return(false);
}

void __fastcall TStylesSimpleDemoMainForm::FormShow(TObject *Sender)
{
  StylesSimpleDemoStylesDialogForm->RestoreDefaults = RestoreDefaults;
  StylesSimpleDemoStylesDialogForm->Show();

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actHeadersExecute(TObject *Sender)
{
  cxDBTreeList->OptionsView->Headers = !cxDBTreeList->OptionsView->Headers;
  ((TAction*)Sender)->Checked = cxDBTreeList->OptionsView->Headers;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actFooterExecute(TObject *Sender)
{
  cxDBTreeList->OptionsView->Footer = !cxDBTreeList->OptionsView->Footer;
  ((TAction*)Sender)->Checked = cxDBTreeList->OptionsView->Footer;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actIndicatorExecute(TObject *Sender)
{
  cxDBTreeList->OptionsView->Indicator = !cxDBTreeList->OptionsView->Indicator;
  ((TAction*)Sender)->Checked = cxDBTreeList->OptionsView->Indicator;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actPreviewExecute(TObject *Sender)
{
  cxDBTreeList->Preview->Visible = !cxDBTreeList->Preview->Visible;
  ((TAction*)Sender)->Checked = cxDBTreeList->Preview->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actShowStyleDialogExecute(TObject *Sender)
{
  StylesSimpleDemoStylesDialogForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  cxDBTreeList->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::cxDBTreeListInitInsertingRecord(
  TcxCustomDBTreeList *Sender, TcxDBTreeListNode *AFocusedNode, bool &AHandled)
{
  StylesSimpleDemoDataDM->SetParentValue(AFocusedNode->ParentKeyValue);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::RestoreDefaults(TObject *Sender)
{
  TcxTreeListStyles *AStyles = cxDBTreeList->Styles;
  AStyles->Background = NULL;
  AStyles->BandBackground = NULL;
  AStyles->BandContent = NULL;
  AStyles->BandHeader = NULL;
  AStyles->ColumnFooter = NULL;
  AStyles->ColumnHeader = NULL;
  AStyles->Content = NULL;
  AStyles->ContentEven = NULL;
  AStyles->ContentOdd = NULL;
  AStyles->Footer = NULL;
  AStyles->Inactive = NULL;
  AStyles->IncSearch = NULL;
  AStyles->Indicator = NULL;
  AStyles->Preview = NULL;
  AStyles->Selection = NULL;
  AStyles->StyleSheet = StylesSimpleDemoDataDM->UserStyleSheet;
}
//---------------------------------------------------------------------------



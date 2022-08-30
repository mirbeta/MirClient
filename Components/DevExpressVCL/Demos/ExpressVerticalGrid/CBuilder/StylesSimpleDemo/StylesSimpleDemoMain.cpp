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
#pragma link "cxDBVGrid"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma link "cxHyperLinkEdit"
#pragma link "cxBlobEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxTimeEdit"
#pragma resource "*.dfm"
TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoMainForm::TStylesSimpleDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::FormShow(TObject *Sender)
{
  StylesSimpleDemoStylesDialogForm->RestoreDefaults = RestoreDefaults;
  StylesSimpleDemoStylesDialogForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actShowStyleDialogExecute(TObject *Sender)
{
  if (!StylesSimpleDemoStylesDialogForm->Visible){
    StylesSimpleDemoStylesDialogForm->Show();
    ((TCustomAction*)Sender)->Checked = true;
  }
  else{
    StylesSimpleDemoStylesDialogForm->Hide();
    ((TCustomAction*)Sender)->Checked = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  cxDBVerticalGrid->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::RestoreDefaults(TObject *Sender)
{
  TcxVerticalGridStyles *AStyles = cxDBVerticalGrid->Styles;
  AStyles->Background = NULL;
  AStyles->Category = NULL;
  AStyles->Header = NULL;
  AStyles->Content = NULL;
  AStyles->Inactive = NULL;
  AStyles->IncSearch = NULL;
  AStyles->Selection = NULL;
  AStyles->StyleSheet = StylesSimpleDemoDataDM->UserStyleSheet;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::StylesFormClosed(TObject *Sender,
  TCloseAction &Action)
{
  actShowStyleDialog->Checked = false;
}


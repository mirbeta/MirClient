//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleListDemoMain.h"
#include "SimpleListDemoData.h"
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
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxHyperLinkEdit"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"
TSimpleListDemoMainForm *SimpleListDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSimpleListDemoMainForm::TSimpleListDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::FormShow(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miBandsClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Bands = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miHeadersClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Headers = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miGridLinesClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Checked)
    cxDBTreeList->OptionsView->GridLines = tlglBoth;
  else
    cxDBTreeList->OptionsView->GridLines = tlglNone;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miIncSearchClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsBehavior->IncSearch = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miFocusCellOnCycleClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsBehavior->FocusCellOnCycle = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miImmediateEditorClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsBehavior->ImmediateEditor = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleListDemoMainForm::miMultiSelectClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsSelection->MultiSelect = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------



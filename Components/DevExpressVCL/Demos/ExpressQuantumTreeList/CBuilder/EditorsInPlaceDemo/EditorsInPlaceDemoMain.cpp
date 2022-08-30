//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoMain.h"
#include "EditorsInPlaceDemoData.h"
#include "EditorsInPlaceDemoCarInfo.h"
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
#pragma link "cxButtonEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxDropDownEdit"
#pragma link "cxImageComboBox"
#pragma link "cxInplaceContainer"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxMRUEdit"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoMainForm::TEditorsInPlaceDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::FormShow(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::tlOrdersCarInfoGetDisplayText(
  TcxTreeListColumn *Sender, TcxTreeListNode *ANode, String &Value)
{
  Value =  "Click here.";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::tlOrdersCompanyEmailPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  AnsiString AMailTo = "mailto:" +
    VarToStr(tlOrders->FocusedNode->Values[tlOrdersCompanyEmail->ItemIndex]);
  ShellExecute(Handle, "OPEN", AMailTo.c_str(),
    NULL, NULL, SW_SHOWMAXIMIZED);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::tlOrdersCarInfoPropertiesInitPopup(
  TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  EditorsInPlaceDemoCarInfoForm->PopupEdit = (TcxPopupEdit*)Sender;
  TcxDBTreeListNode *ANode = (TcxDBTreeListNode*)tlOrders->FocusedNode;
  EditorsInPlaceDemoCarInfoForm->InitPopupPanel(ANode->Values[tlOrdersProductID->ItemIndex]);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::tlOrdersCarInfoPropertiesCloseUp(
  TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  if ((tlOrders->FocusedNode->Values[tlOrdersProductID->ItemIndex] !=
    EditorsInPlaceDemoCarInfoForm->EditValue) && EditorsInPlaceDemoCarInfoForm->Accepted)
    tlOrders->DataController->SetEditValue(tlOrdersProductID->ItemIndex,
      EditorsInPlaceDemoCarInfoForm->EditValue, evsValue);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::miShowEditBtnsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  tlOrders->OptionsView->ShowEditButtons =
    (TcxEditingControlEditShowButtons)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

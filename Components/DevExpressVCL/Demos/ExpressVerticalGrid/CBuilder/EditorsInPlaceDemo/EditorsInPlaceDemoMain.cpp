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
#pragma link "cxDBVGrid"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoMainForm::TEditorsInPlaceDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::vgOrdersCompanyEmailPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
  AnsiString AMailTo = "mailto:" + VarToStr(vgOrdersCustomerEmail->Properties->Value);
  ShellExecute(Handle, "OPEN", AMailTo.c_str(), NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::miShowEditBtnsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  vgOrders->OptionsView->ShowEditButtons = (TcxEditingControlEditShowButtons)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::actAboutExecute(TObject *Sender)
{
  ShowAbout(false, true);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::vgOrdersCarInfoPropertiesGetDisplayText(
  TcxCustomEditorRowProperties *Sender, int ARecord, String &AText)
{
  AText = "Click here";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::vgOrdersCarInfoEditPropertiesInitPopup(
  TObject *Sender)
{
  EditorsInPlaceDemoCarInfoForm->PopupEdit = (TcxPopupEdit*)Sender;
  Variant ACarID = vgOrdersProductID->Properties->Value;
  EditorsInPlaceDemoCarInfoForm->InitPopupPanel(ACarID);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::vgOrdersCarInfoEditPropertiesCloseUp(
  TObject *Sender)
{
  if ((vgOrdersProductID->Properties->Value !=
    EditorsInPlaceDemoCarInfoForm->EditValue) && EditorsInPlaceDemoCarInfoForm->Accepted)
    vgOrdersProductID->Properties->Value = EditorsInPlaceDemoCarInfoForm->EditValue;
}
//---------------------------------------------------------------------------



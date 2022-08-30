//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoCarInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxBlobEdit"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxControls"
#pragma link "cxCurrencyEdit"
#pragma link "cxCustomData"
#pragma link "cxDBTL"
#pragma link "cxGraphics"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxMRUEdit"
#pragma link "cxRadioGroup"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"
TEditorsInPlaceDemoCarInfoForm *EditorsInPlaceDemoCarInfoForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoCarInfoForm::TEditorsInPlaceDemoCarInfoForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::btnCancelClick(TObject *Sender)
{
  ClosePopup(false);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::btnOKClick(TObject *Sender)
{
  ClosePopup(true);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::tlCarInfoTopRecordIndexChanged(
  TObject *Sender)
{
  FEditValue = ((TcxDBTreeListNode*)tlCarInfo->TopVisibleNode)->KeyValue;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::tlCarInfoTrademarkPropertiesButtonClick(
  TObject *Sender)
{
  ShowMessage("Button click event handler");
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::tlCarInfoGetCaptionDisplayText(
  TcxTreeListColumn *Sender, TcxTreeListNode *ANode, String &Value)
{
  Value = Sender->Caption->Text;
}
//---------------------------------------------------------------------------

void TEditorsInPlaceDemoCarInfoForm::InitPopupPanel(Variant ACarID)
{
  tlCarInfo->HandleNeeded();
  tlCarInfo->DataController->LocateByKey(ACarID);
  FAccepted = false;
}
//---------------------------------------------------------------------------

void TEditorsInPlaceDemoCarInfoForm::ClosePopup(bool AAccepted)
{
  if (FPopupEdit == NULL) return;
  FAccepted = AAccepted;
  FPopupEdit->DroppedDown = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::FormShow(TObject *Sender)
{
  FEditValue = ((TcxDBTreeListNode*)tlCarInfo->TopVisibleNode)->KeyValue;
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceDemoCarInfo.h"
#include "EditorsInPlaceDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxControls"
#pragma link "cxDBVGrid"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TEditorsInPlaceDemoCarInfoForm *EditorsInPlaceDemoCarInfoForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoCarInfoForm::TEditorsInPlaceDemoCarInfoForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::GetDisplayText(
  TcxCustomEditorRowProperties *Sender, int ARecord, String &AText)
{
  AText = Sender->Caption;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::vgCarInfoStylesGetContentStyle(
  TObject *Sender, TcxCustomEditorRowProperties *AEditProp, bool AFocused,
  int ARecordIndex, TcxStyle *&AStyle)
{
  if (dynamic_cast<TcxCustomMultiEditorRow*>(AEditProp->Row) != 0)
    if (((TcxMultiEditorRow*)AEditProp->Row)->Properties->Editors->Items[0] == AEditProp)
      AStyle = EditorsInPlaceDemoDataDM->styCaption;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::vgCarInfoLeftVisibleRecordIndexChanged(
  TObject *Sender)
{
  FEditValue = vgCarInfoID->Properties->Values[vgCarInfo->LeftVisibleRecord];
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::cxButtonClick(TObject *Sender)
{
  ClosePopup((bool)((TcxButton*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::OnEditPropertiesButtonClick(
  TObject *Sender)
{
  ShowMessage("Button click event handler");
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::ClosePopup(bool AAccepted)
{
  if (FPopupEdit == NULL) return;
  FAccepted = AAccepted;
  FPopupEdit->DroppedDown = false;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoCarInfoForm::InitPopupPanel(Variant ACarID)
{
  vgCarInfo->DataController->KeyFieldNames = "ID";
  vgCarInfo->DataController->LocateByKey(ACarID);
  FAccepted = false;
}
//---------------------------------------------------------------------------



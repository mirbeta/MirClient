//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AlertWindowDemoOptions.h"
#include "AlertWindowDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLabel"
#pragma resource "*.dfm"
TFormOptions *FormOptions;
//---------------------------------------------------------------------------

__fastcall TFormOptions::TFormOptions(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TFormOptions::LoadWindowOptions(TdxAlertWindowManager* AManager)
{
  cbPosition->ItemIndex = (Integer)AManager->WindowPosition;
  cbShowingAnimation->ItemIndex = (int)AManager->OptionsAnimate->ShowingAnimation;
  cbShowingDirection->ItemIndex = (int)AManager->OptionsAnimate->ShowingAnimationDirection;
  cbHidingAnimation->ItemIndex = (int)AManager->OptionsAnimate->HidingAnimation;
  cbHidingDirection->ItemIndex = (int)AManager->OptionsAnimate->HidingAnimationDirection;
  seWidth->Value = AManager->OptionsSize->Width;
  seHeight->Value = AManager->OptionsSize->Height;
  cbAutoWidth->Checked = AManager->OptionsSize->AutoWidth;
  cbAutoHeight->Checked = AManager->OptionsSize->AutoHeight;
  seTime->Value = AManager->OptionsBehavior->DisplayTime;
  cbCloseOnRightClick->Checked = AManager->OptionsBehavior->CloseOnRightClick;
  cbHotTrack->Checked = AManager->OptionsAnimate->HotTrack;
  seMaxInDisplay->Value = AManager->WindowMaxCount;
  cbAutoSizeAdjustment->Checked = AManager->OptionsSize->AutoSizeAdjustment;
  cbCollapseEmptySlots->Checked = AManager->OptionsAnimate->CollapseEmptySlots;
}
//---------------------------------------------------------------------------

void TFormOptions::SaveWindowOptions(TdxAlertWindowManager* AManager)
{
  AManager->WindowPosition = (TdxAlertWindowPosition)cbPosition->ItemIndex;
  AManager->OptionsSize->Width = seWidth->Value;
  AManager->OptionsSize->Height = seHeight->Value;
  AManager->OptionsSize->AutoWidth = cbAutoWidth->Checked;
  AManager->OptionsSize->AutoHeight = cbAutoHeight->Checked;
  AManager->OptionsBehavior->DisplayTime = seTime->Value;
  AManager->OptionsBehavior->CloseOnRightClick = cbCloseOnRightClick->Checked;
  AManager->OptionsAnimate->HotTrack = cbHotTrack->Checked;
  AManager->WindowMaxCount = seMaxInDisplay->Value;
  AManager->OptionsSize->AutoSizeAdjustment = cbAutoSizeAdjustment->Checked;
  AManager->OptionsAnimate->CollapseEmptySlots = cbCollapseEmptySlots->Checked;
  AManager->OptionsAnimate->ShowingAnimation =
	(TdxAlertWindowAnimation)cbShowingAnimation->ItemIndex;
  AManager->OptionsAnimate->HidingAnimation =
	(TdxAlertWindowAnimation)cbHidingAnimation->ItemIndex;
  AManager->OptionsAnimate->ShowingAnimationDirection =
	(TdxAlertWindowMovingDirection)cbShowingDirection->ItemIndex;
  AManager->OptionsAnimate->HidingAnimationDirection =
	(TdxAlertWindowMovingDirection)cbHidingDirection->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TFormOptions::btnApplyClick(TObject *Sender)
{
  SaveWindowOptions(AlertWindowDemoForm->dxAlertWindowManager1);
}
//---------------------------------------------------------------------------

void __fastcall TFormOptions::btCancelClick(TObject *Sender)
{
  Hide();
}
//---------------------------------------------------------------------------

void __fastcall TFormOptions::btOkClick(TObject *Sender)
{
  SaveWindowOptions(AlertWindowDemoForm->dxAlertWindowManager1);
  Hide();
}
//---------------------------------------------------------------------------


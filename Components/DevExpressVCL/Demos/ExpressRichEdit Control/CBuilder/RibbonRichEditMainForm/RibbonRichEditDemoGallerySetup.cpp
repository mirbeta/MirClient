//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonRichEditDemoGallerySetup.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TColorDialogSetupForm *ColorDialogSetupForm;
//---------------------------------------------------------------------------
__fastcall TColorDialogSetupForm::TColorDialogSetupForm(TComponent* Owner)
		: TForm(Owner)
{
}
//---------------------------------------------------------------------------
bool __fastcall TColorDialogSetupForm::GetSettings(bool &RemoveHorizontalItemPadding,
  bool &RemoveVerticalItemPadding)
{
  chkRemoveHorizontalItemPadding->Checked = RemoveHorizontalItemPadding;
  chkRemoveVerticalItemPadding->Checked = RemoveVerticalItemPadding;

  bool Result = ShowModal() == 1;

  if (Result)
  {
	RemoveHorizontalItemPadding = chkRemoveHorizontalItemPadding->Checked;
	RemoveVerticalItemPadding = chkRemoveVerticalItemPadding->Checked;
  }
  return (Result);
}
//---------------------------------------------------------------------------

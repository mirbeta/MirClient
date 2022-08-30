//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RatingControlDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::AllowHoverChange(TObject* Sender)
{
  EditRepositoryRating->Properties->AllowHover = cbAllowHover->Checked;
}

void __fastcall TfrmMain::ChooseImageCloseUp(TObject* Sender)
{
  FPopupWindow->Release();
}

void __fastcall TfrmMain::ChooseImageInitPopup(TObject* Sender)
{
  FPopupWindow = new TfrmImagePicker(this);
  FPopupWindow->Initialize(EditRepositoryRating->Properties);
  peChooseImage->Properties->PopupControl = FPopupWindow;
}

void __fastcall TfrmMain::FillPrecisionChange(TObject* Sender)
{
  if (rbFull->Checked)
	EditRepositoryRating->Properties->FillPrecision = rcfpFull;
  else
	if (rbHalf->Checked)
	  EditRepositoryRating->Properties->FillPrecision = rcfpHalf;
	else
	  EditRepositoryRating->Properties->FillPrecision = rcfpExact;
}

void __fastcall TfrmMain::ItemCountChange(TObject* Sender)
{
  EditRepositoryRating->Properties->ItemCount = seItemCount->Value;
}

void __fastcall TfrmMain::OrientationChange(TObject* Sender)
{
  if (cmbOrientation->Text == "Horizontal")
	EditRepositoryRating->Properties->Orientation = orHorizontal;
  else
	EditRepositoryRating->Properties->Orientation = orVertical;
}

void __fastcall TfrmMain::ReverseDirectionChange(TObject* Sender)
{
  EditRepositoryRating->Properties->ReverseDirection = cbReverseDirection->Checked;
}

void __fastcall TfrmMain::StepChange(TObject* Sender)
{
  EditRepositoryRating->Properties->Step = seStep->Value;
}

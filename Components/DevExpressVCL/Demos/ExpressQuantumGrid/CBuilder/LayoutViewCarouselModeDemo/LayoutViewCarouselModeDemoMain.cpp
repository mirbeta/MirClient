//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "LayoutViewCarouselModeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBLayoutView"
#pragma link "cxGridLayoutView"
#pragma link "cxGridLevel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma link "cxStyles"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutLookAndFeels"
#pragma link "dxmdaset"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxDropDownEdit"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxTrackBar"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::miCustomizeClick(TObject *Sender)
{
	LayoutView->Controller->Customization = True;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnCustomizeClick(TObject *Sender)
{
	LayoutView->Controller->Customization = True;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbExpandableRecordsClick(TObject *Sender)
{
	LayoutView->OptionsCustomize->RecordExpanding = cbExpandableRecords->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbMultiSelectRecordsClick(TObject *Sender)
{
	LayoutView->OptionsSelection->MultiSelect = cbMultiSelectRecords->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbRecordCaptionsClick(TObject *Sender)
{
	LayoutView->OptionsView->RecordCaption->Visible = cbRecordCaptions->Checked;
}
//---------------------------------------------------------------------------

String __fastcall GetDir(String const AFileName, Integer ALevelUp)
{
	Integer I;
	String rv = AFileName;
	for (I = 1; I <= ALevelUp; I++) {
		rv = ExtractFileDir(rv);
	}
	return rv;
}

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  FLockCount++;
  try {
	tsAutoPitchAngle->Checked = LayoutView->OptionsView->CarouselMode->AutoPitchAngle;
	cbInterpolationMode->ItemIndex = (int)LayoutView->OptionsView->CarouselMode->InterpolationMode;
	tbPitchAngle->Position = (int)LayoutView->OptionsView->CarouselMode->PitchAngle;
	tbRollAngle->Position = (int)LayoutView->OptionsView->CarouselMode->RollAngle;
	tbRecordCount->Position = LayoutView->OptionsView->CarouselMode->RecordCount;
	tbBackgroundAlphaLevel->Position = LayoutView->OptionsView->CarouselMode->BackgroundRecordAlphaLevel;
	tbStartRecordScale->Position = LayoutView->OptionsView->CarouselMode->BackgroundRecordStartScale;
	tbEndRecordScale->Position = LayoutView->OptionsView->CarouselMode->BackgroundRecordEndScale;
  }
  __finally {
	FLockCount--;
  }
  mdHomes->LoadFromBinaryFile(GetDir(Application->ExeName, 3) + "\\Data\\Homes.dat");
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::CarouselModePropertiesChange(TObject *Sender)
{
  if (FLockCount) {
	  return ;
  }
  FLockCount++;
  try{
	LayoutView->OptionsView->CarouselMode->AutoPitchAngle = tsAutoPitchAngle->Checked;
	LayoutView->OptionsView->CarouselMode->InterpolationMode = (TdxGPInterpolationMode)(cbInterpolationMode->ItemIndex);
	LayoutView->OptionsView->CarouselMode->PitchAngle = tbPitchAngle->Position;
	LayoutView->OptionsView->CarouselMode->RollAngle = tbRollAngle->Position;
	LayoutView->OptionsView->CarouselMode->RecordCount = tbRecordCount->Position;
	LayoutView->OptionsView->CarouselMode->BackgroundRecordAlphaLevel = tbBackgroundAlphaLevel->Position;
	LayoutView->OptionsView->CarouselMode->BackgroundRecordStartScale = tbStartRecordScale->Position;
	LayoutView->OptionsView->CarouselMode->BackgroundRecordEndScale = tbEndRecordScale->Position;
  }
  __finally {
	FLockCount--;
  }
  liPitchAngle->Enabled = !tsAutoPitchAngle->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  LayoutView->DataController->RecNo = (int)(LayoutView->DataController->RecordCount / 2);
}
//---------------------------------------------------------------------------


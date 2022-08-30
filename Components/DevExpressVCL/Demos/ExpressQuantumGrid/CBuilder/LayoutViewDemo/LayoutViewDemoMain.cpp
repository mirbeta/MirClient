//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "LayoutViewDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
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
#pragma link "cxStyles"
#pragma link "dxLayoutContainer"
#pragma link "dxmdaset"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxRadioGroup"
#pragma link "cxLabel"
#pragma link "cxDropDownEdit"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::rgViewModeClick(TObject *Sender)
{
  LayoutView->OptionsView->ViewMode = (TcxGridLayoutViewViewMode)rgViewMode->ItemIndex;
  cbStretch->Enabled = (LayoutView->OptionsView->ViewMode == lvvmSingleRecord);
  lbStretch->Enabled = cbStretch->Enabled;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCustomizeClick(TObject *Sender)
{
  Grid->ActiveView->Controller->Customization = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbCenterRecordsClick(TObject *Sender)
{
  LayoutView->OptionsView->CenterRecords = cbCenterRecords->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbShowOnlyEntireRecordsClick(TObject *Sender)
{
  LayoutView->OptionsView->ShowOnlyEntireRecords = cbShowOnlyEntireRecords->Checked;
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

void __fastcall TfrmMain::cbExpandableRecordsClick(TObject *Sender)
{
  LayoutView->OptionsCustomize->RecordExpanding = cbExpandableRecords->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnCustomizeClick(TObject *Sender)
{
  LayoutView->Controller->Customization = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::cbStretchPropertiesChange(TObject *Sender)
{
  LayoutView->OptionsView->SingleRecordStretch = (TcxGridLayoutViewSingleRecordStretch)cbStretch->ItemIndex;
}
//---------------------------------------------------------------------------

String GetDir(String AFileName, int ALevelUp)
{
  int I;
  String Result;
  Result = AFileName;
  for (I = 1; I <= ALevelUp; I++)
	Result = ExtractFileDir(Result);
  return Result;
}
//---------------------------------------------------------------------------


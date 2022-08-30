//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "StylesMultiDemoMain.h"
#include "StylesMultiDemoData.h"
#include "AboutDemoForm.h"
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
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxButtons"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyleSheetEditor"
#pragma link "cxRadioGroup"
#pragma link "cxListBox"
#pragma link "cxContainer"
#pragma link "cxGroupBox"
#pragma link "cxGridStyleSheetsPreview"
#pragma link "cxDataStorage"
#pragma link "cxDropDownEdit"
#pragma link "cxGridCardView"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TStylesMultiDemoMainForm *StylesMultiDemoMainForm;

const int cNone = 0;
const int cPredefined = 1;
const int cUserDefined = 2;

//---------------------------------------------------------------------------
__fastcall TStylesMultiDemoMainForm::TStylesMultiDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::FormCreate(TObject *Sender)
{
  CreateUserStyleSheetsList();
  CreatePredefinedStyleSheetsList();
  SetPredefinedStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::btnSaveClick(TObject *Sender)
{
    if (SaveDialog->Execute())
      SaveUserDefinedStyleSheets(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::btnLoadClick(TObject *Sender)
{
  if (OpenDialog->Execute())
    LoadUserDefinedStyleSheets(OpenDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::RadioGroupClick(TObject *Sender)
{
  switch (((TcxRadioGroup*)Sender)->ItemIndex) {
    case cNone:
      UpdateGridStyleSheets(NULL);
    case cPredefined:
      SetPredefinedStyleSheets();
    case cUserDefined:
      SetUserDefinedStyleSheets();
  }
  ChangeVisibility(((TcxRadioGroup*)Sender)->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::cbUserStyleSheetsChange(TObject *Sender)
{
  SetUserDefinedStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::lbPredefinedStyleSheetsClick(TObject *Sender)
{
  SetPredefinedStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::btnEditClick(TObject *Sender)
{
  ShowcxStyleSheetEditor((TcxGridTableViewStyleSheet*)(cbUserStyleSheets->
  	Properties->Items->Objects[cbUserStyleSheets->ItemIndex]), NULL);
}
//---------------------------------------------------------------------------

void __fastcall TStylesMultiDemoMainForm::FormActivate(TObject *Sender)
{
  OpenDialog->InitialDir = ExtractFileDir(Application->ExeName);
  SaveDialog->InitialDir = OpenDialog->InitialDir;
}
//---------------------------------------------------------------------------

TcxGridTableViewStyleSheet* TStylesMultiDemoMainForm::GetCurrentStyleSheet()
{
  return (TcxGridTableViewStyleSheet*)tvProjects->Styles->StyleSheet;
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::CreateUserStyleSheetsList()
{
  cbUserStyleSheets->Properties->Items->Clear();
  for (int I = 0 ; I < StylesMultiDemoMainDM->strepUserDefined->StyleSheetCount; I++)
    cbUserStyleSheets->Properties->Items->AddObject(
	  StylesMultiDemoMainDM->strepUserDefined->StyleSheets[I]->Caption,
      StylesMultiDemoMainDM->strepUserDefined->StyleSheets[I]);
  cbUserStyleSheets->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::CreatePredefinedStyleSheetsList()
{
  lbPredefinedStyleSheets->Clear();
  for (int I = 0; I < StylesMultiDemoMainDM->strepPredefined->StyleSheetCount; I++)
	lbPredefinedStyleSheets->Items->AddObject(
	  StylesMultiDemoMainDM->strepPredefined->StyleSheets[I]->Caption,
      StylesMultiDemoMainDM->strepPredefined->StyleSheets[I]);
  lbPredefinedStyleSheets->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::UpdateView(TcxGridDBTableView *AView,
  TcxGridTableViewStyleSheet *AStyleSheet)
{
  AView->BeginUpdate();
  AView->Styles->StyleSheet = AStyleSheet;
  AView->EndUpdate();
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::UpdateGridStyleSheets(TcxGridTableViewStyleSheet *AStyleSheet)
{
  if (GetCurrentStyleSheet() == AStyleSheet) return;
  UpdateView(tvProjects, AStyleSheet);
  UpdateView(tvTeam, AStyleSheet);
  tvProjects->DataController->ClearDetails();  // refresh detail level

  if (AStyleSheet != NULL)
    pnlCurrentStyleSheet->Caption = AStyleSheet->Caption;
  else
    pnlCurrentStyleSheet->Caption = "";
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::ChangeVisibility(int AType)
{
  cbUserStyleSheets->Enabled = (AType == cUserDefined);
  gbUserDefined->Enabled = (AType == cUserDefined);
  btnEdit->Enabled = (AType == cUserDefined);
  btnLoad->Enabled = (AType == cUserDefined);
  btnSave->Enabled = (AType == cUserDefined);

  lbPredefinedStyleSheets->Enabled = (AType == cPredefined);
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::SetPredefinedStyleSheets()
{
  if (lbPredefinedStyleSheets->Items->Count > 0)
    UpdateGridStyleSheets(
    (TcxGridTableViewStyleSheet*)lbPredefinedStyleSheets->Items->
      Objects[lbPredefinedStyleSheets->ItemIndex]);
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::SetUserDefinedStyleSheets()
{
  if (cbUserStyleSheets->Properties->Items->Count > 0)
	UpdateGridStyleSheets((TcxGridTableViewStyleSheet*)cbUserStyleSheets->
	  Properties->Items->Objects[cbUserStyleSheets->ItemIndex]);
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::ClearUserDefinedStyleSheets()
{
  StylesMultiDemoMainDM->strepUserDefined->Clear();
  StylesMultiDemoMainDM->strepUserDefined->ClearStyleSheets();
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::LoadUserDefinedStyleSheets(TFileName AFileName)
{
  UpdateGridStyleSheets(NULL);
  ClearUserDefinedStyleSheets();

  LoadStyleSheetsFromIniFile(AFileName, StylesMultiDemoMainDM->strepUserDefined,
    __classid(TcxGridTableViewStyleSheet), NULL, NULL, NULL, NULL);

  CreateUserStyleSheetsList();
  SetUserDefinedStyleSheets();
}
//---------------------------------------------------------------------------

void TStylesMultiDemoMainForm::SaveUserDefinedStyleSheets(TFileName AFileName)
{
  TList *AList = new TList();
  try {
    StylesMultiDemoMainDM->PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
  }
  __finally {
    delete AList;
  }
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesMultiMain.h"
//---------------------------------------------------------------------------
#include "cxStyleSheetEditor.hpp"
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxPivotGridStyleSheetsPreview"
#pragma link "cxDBPivotGrid"
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxListBox"
#pragma link "cxRadioGroup"
#pragma link "cxStyles"
#pragma link "cxCustomData"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"

  TfrmStylesMulti *frmStylesMulti;

  const int cNone = 0;
  const int cPredefined = 1;
  const int cUserDefined = 2;

//---------------------------------------------------------------------------
__fastcall TfrmStylesMulti::TfrmStylesMulti(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

TcxLookAndFeelKind __fastcall TfrmStylesMulti::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

void __fastcall TfrmStylesMulti::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender);
  CreateUserStyleSheetsList();
  CreatePredefinedStyleSheetsList();
  SetPredefinedStyleSheets();
}

TcxCustomPivotGrid* __fastcall TfrmStylesMulti::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::RadioGroupClick(TObject *Sender)
{
  switch (((TcxRadioGroup*)Sender)->ItemIndex){
    case cNone: {
      UpdateGridStyleSheets(NULL);
      break;
    }
    case cPredefined: {
      SetPredefinedStyleSheets();
      break;
    }
    case cUserDefined: {
      SetUserDefinedStyleSheets();
      break;
    }
  }
  ChangeVisibility(((TcxRadioGroup*)Sender)->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::lbPredefinedStyleSheetsClick(
      TObject *Sender)
{
  SetPredefinedStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::cbUserStyleSheetsChange(TObject *Sender)
{
  SetUserDefinedStyleSheets();

}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::btnEditClick(TObject *Sender)
{
  ShowcxStyleSheetEditor(((TcxPivotGridStyleSheet*)cbUserStyleSheets->Items->Objects[cbUserStyleSheets->ItemIndex]), NULL);
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::btnLoadClick(TObject *Sender)
{
  if (OpenDialog->Execute())
    LoadUserDefinedStyleSheets(OpenDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::btnSaveClick(TObject *Sender)
{
  if (SaveDialog1->Execute())
    SaveUserDefinedStyleSheets(SaveDialog1->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::ChangeVisibility(int AType)
{
  cbUserStyleSheets->Enabled = AType == cUserDefined;
  gbUserDefined->Enabled = AType == cUserDefined;
  btnEdit->Enabled = AType == cUserDefined;
  btnLoad->Enabled = AType == cUserDefined;
  btnSave->Enabled = AType == cUserDefined;
  lbPredefinedStyleSheets->Enabled = AType == cPredefined;
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::ClearUserDefinedStyleSheets()
{
  srUserDefined->Clear();
  srUserDefined->ClearStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::CreatePredefinedStyleSheetsList()
{
  lbPredefinedStyleSheets->Clear();
  for (int I = 0; I < srPredefined->StyleSheetCount; I++)
    lbPredefinedStyleSheets->Items->AddObject(
      srPredefined->StyleSheets[I]->Caption, srPredefined->StyleSheets[I]);
  lbPredefinedStyleSheets->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::CreateUserStyleSheetsList()
{
  cbUserStyleSheets->Clear();
  for (int I = 0; I < srUserDefined->StyleSheetCount; I++)
    cbUserStyleSheets->Items->AddObject(
      srUserDefined->StyleSheets[I]->Caption, srUserDefined->StyleSheets[I]);
  cbUserStyleSheets->ItemIndex = 0;
}
//---------------------------------------------------------------------------

TcxPivotGridStyleSheet* __fastcall TfrmStylesMulti::GetCurrentStyleSheet()
{
  return (TcxPivotGridStyleSheet*)PivotGrid()->Styles->StyleSheet;
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::LoadUserDefinedStyleSheets(AnsiString AFileName)
{
  UpdateGridStyleSheets(NULL);
  ClearUserDefinedStyleSheets();

  LoadStyleSheetsFromIniFile(AFileName, srUserDefined,
    __classid(TcxPivotGridStyleSheet), NULL, NULL, NULL, NULL);

  CreateUserStyleSheetsList();
  SetUserDefinedStyleSheets();
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::SaveUserDefinedStyleSheets(AnsiString AFileName)
{
  TList* AList = new TList();
  try {
    for (int I = 0; I < srUserDefined->StyleSheetCount; I++)
      AList->Add(srUserDefined->StyleSheets[I]);
    SaveStyleSheetsToIniFile(AFileName, AList);
  }
  __finally {
    delete AList;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::SetPredefinedStyleSheets()
{
  if (lbPredefinedStyleSheets->Items->Count > 0)
    UpdateGridStyleSheets((TcxPivotGridStyleSheet*)lbPredefinedStyleSheets->Items->Objects[lbPredefinedStyleSheets->ItemIndex]);
}
//---------------------------------------------------------------------------
void __fastcall TfrmStylesMulti::SetUserDefinedStyleSheets()
{
  if (cbUserStyleSheets->Items->Count > 0) 
    UpdateGridStyleSheets((TcxPivotGridStyleSheet*)cbUserStyleSheets->Items->Objects[cbUserStyleSheets->ItemIndex]);
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::UpdateGridStyleSheets(TcxPivotGridStyleSheet* AStyleSheet)
{
  if (GetCurrentStyleSheet() == AStyleSheet) 
    return;
  PivotGrid()->Styles->StyleSheet = AStyleSheet;
  if (AStyleSheet != NULL)
    pnlCurrentStyleSheet->Caption = AStyleSheet->Caption;
  else
    pnlCurrentStyleSheet->Caption = "";
}
//---------------------------------------------------------------------------

void __fastcall TfrmStylesMulti::FormActivate(TObject *Sender)
{
  OpenDialog->InitialDir = ExtractFileDir(Application->ExeName);
  SaveDialog->InitialDir = OpenDialog->InitialDir;
  PivotGridCarName->Area = faRow;
}
//---------------------------------------------------------------------------


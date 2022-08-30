//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListStyles.h"
#include "IssueListStyleData.h"
#include "cxStyleSheetEditor.hpp"
#include "IssueListData.h"
#include "IssueListMain.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyleSheetEditor"
#pragma link "cxListBox"
#pragma link "cxGridStyleSheetsPreview"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TIssueListStylesForm::TIssueListStylesForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TIssueListStylesForm::lbPredefinedStyleSheetsClick(
      TObject *Sender)
{
  if ( ((TcxListBox*)Sender)->Items->Count > 0)
    SelectStyleFromList();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::btnClearClick(TObject *Sender)
{
  ResetStyleSheet();
  ResetStyleAndController();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::btnEditClick(TObject *Sender)
{
  ShowcxStyleSheetEditor(
  (TcxGridTableViewStyleSheet*)(lbPredefinedStyleSheets->Items->
    Objects[lbPredefinedStyleSheets->ItemIndex]) , NULL);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::cxButton1Click(TObject *Sender)
{
    if (OpenDialog->Execute())
    {
      LoadStyleSheets(OpenDialog->FileName);
      SelectStyleFromList();
    }
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::cxButton2Click(TObject *Sender)
{
    if (SaveDialog->Execute())
      SaveStyleSheets(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::FormCreate(TObject *Sender)
{
  FStyleData = new TdmStyles(this);
  SelectedStyleSheet = NULL;
  PopulateStylesList();
  SelectStyleFromList();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::FormDestroy(TObject *Sender)
{
  delete FStyleData;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::PopulateStylesList()
{
  lbPredefinedStyleSheets->Clear();

  for (int i = 0; i< StyleData->strepPredefined->StyleSheetCount; i++)
    lbPredefinedStyleSheets->Items->AddObject(
       StyleData->strepPredefined->StyleSheets[i]->Caption,
       StyleData->strepPredefined->StyleSheets[i]);

    lbPredefinedStyleSheets->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::LoadStyleSheets(const TFileName AFileName)
{
  StyleData->strepPredefined->Clear();
  StyleData->strepPredefined->ClearStyleSheets();
  LoadStyleSheetsFromIniFile( AFileName, StyleData->strepPredefined,
    __classid(TcxGridTableViewStyleSheet), NULL, NULL, NULL, NULL);
  PopulateStylesList();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::SaveStyleSheets(const TFileName AFileName)
{
  TList* AList = new TList();
  try
  {
    for (int i = 0; i < StyleData->strepPredefined->StyleSheetCount; i++)
       AList->Add(StyleData->strepPredefined->StyleSheets[i]);

    SaveStyleSheetsToIniFile(AFileName, AList);
  }
  __finally
  {
     delete AList;
  }
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::UpdateStyleAndController()
{
    if (SelectedStyleSheet->Styles->ContentOdd != NULL)
      dmMain->edstcMain->Style->Color = SelectedStyleSheet->Styles->ContentOdd->Color;
    if (SelectedStyleSheet->Styles->Selection != NULL && dmMain->stSelected != NULL)
    {
      dmMain->stSelected->Color = SelectedStyleSheet->Styles->Selection->Color;
      dmMain->stSelected->Font = SelectedStyleSheet->Styles->Selection->Font;
    }
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::SetStyleSheet()
{
  IssueListMainForm->GridForm->cxGrid->BeginUpdate();
  try
  {
    dmMain->strepMain->StyleSheets[0]->CopyFrom(SelectedStyleSheet);

    TcxGridDBDataController* const dcProjects =
        IssueListMainForm->GridForm->tvProjects->DataController;
    dcProjects->ClearDetails();

    TcxGridDBDataController* const dcDepartments =
        IssueListMainForm->GridForm->tvDepartments->DataController;
    dcDepartments->ClearDetails();
  }
  __finally
  {
    IssueListMainForm->GridForm->cxGrid->EndUpdate();
  }
  UpdateStyleAndController();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::ResetStyleSheet()
{
  IssueListMainForm->GridForm->cxGrid->BeginUpdate();
  try
  {
    TcxGridTableViewStyles* const AStyles = dmMain->ssTableStyles->Styles;

    AStyles->Background = NULL;
    AStyles->Content = NULL;
    AStyles->ContentEven  = NULL;
    AStyles->ContentOdd = NULL;
    AStyles->FilterBox = NULL;
    AStyles->Footer = NULL;
    AStyles->Group = NULL;
    AStyles->GroupByBox = NULL;
    AStyles->Header = NULL;
    AStyles->Inactive = NULL;
    AStyles->IncSearch = NULL;
    AStyles->Indicator = NULL;
    AStyles->Preview = NULL;
    AStyles->Selection = NULL;

    TcxGridDBDataController* const dcProjects =
        IssueListMainForm->GridForm->tvProjects->DataController;
    dcProjects->ClearDetails();

    TcxGridDBDataController* const dcDepartments =
        IssueListMainForm->GridForm->tvDepartments->DataController;
    dcDepartments->ClearDetails();
   }
    __finally
    {
      IssueListMainForm->GridForm->cxGrid->EndUpdate();
    };
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::SelectStyleFromList()
{
  SelectedStyleSheet = (TcxGridTableViewStyleSheet*)lbPredefinedStyleSheets->
    Items->Objects[lbPredefinedStyleSheets->ItemIndex];
  SetStyleSheet();
  UpdateStyleAndController();
}
//---------------------------------------------------------------------------

void __fastcall TIssueListStylesForm::ResetStyleAndController()
{
  dmMain->edstcMain->Style->Color = clWindow;
  dmMain->stSelected->Color = dmMain->stBlue->Color;
  dmMain->stSelected->Font = dmMain->stBlue->Font;
}
//---------------------------------------------------------------------------



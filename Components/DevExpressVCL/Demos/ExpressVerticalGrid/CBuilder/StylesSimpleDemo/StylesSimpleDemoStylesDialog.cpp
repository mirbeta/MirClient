//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoStylesDialog.h"
#include "StylesSimpleDemoMain.h"
#include "StylesSimpleDemoData.h"
#include "StylesSimpleDemoEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TStylesSimpleDemoStylesDialogForm *StylesSimpleDemoStylesDialogForm;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoStylesDialogForm::TStylesSimpleDemoStylesDialogForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::FormCreate(TObject *Sender)
{
  OnClose = StylesSimpleDemoMainForm->StylesFormClosed;
  TStrings *ALookupItems = cxEditRepositoryMRUItem->Properties->LookupItems;
  TcxStyleRepository *ARespository = StylesSimpleDemoDataDM->StyleRepository;
  ALookupItems->Clear();
  TcxStyle *AStyle;
  for (int I = 0; I < ARespository->Count; I++){
    AStyle = (TcxStyle*)ARespository->Items[I];
    ALookupItems->AddObject(AStyle->Name, AStyle);
  }
  RefreshBinding();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::OnButtonClick(TObject *Sender)
{
  ChangeStyle(GetSelectedStyle());
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::btnRestoreClick(TObject *Sender)
{
  if (FRestoreDefaults != NULL) FRestoreDefaults(Sender);
  RefreshBinding();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::OnEditValueChanged(TObject *Sender)
{
  int ItemIndex = cxEditRepositoryMRUItem->Properties->LookupItems->IndexOf(
    ((TcxCustomMRUEdit*)Sender)->EditValue);
  TcxStyle *AStyle =
    (TcxStyle*)cxEditRepositoryMRUItem->Properties->LookupItems->Objects[ItemIndex];
  SetCurrentStyle(AStyle, (TStyles)(cxVerticalGrid->FocusedRow->VisibleIndex - 1));
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::cxVerticalGridStylesGetContentStyle(
  TObject *Sender, TcxCustomEditorRowProperties *AEditProp, bool AFocused,
  int ARecordIndex, TcxStyle *&AStyle)
{
  if (((TcxEditorRow*)AEditProp->Row)->VisibleIndex == 0)
    AStyle = cxVerticalGridStyleSheetDevExpress->Styles->Category;
  else
    AStyle = cxVerticalGridStyleSheetDevExpress->Styles->Content;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::cxVerticalGridStylesGetHeaderStyle(
  TObject *Sender, TcxCustomRow *ARow, TcxStyle *&AStyle)
{
  if (ARow->VisibleIndex == 0)
    AStyle = cxVerticalGridStyleSheetDevExpress->Styles->Category;
  else
    AStyle = cxVerticalGridStyleSheetDevExpress->Styles->Header;
}
//---------------------------------------------------------------------------

TcxStyle* __fastcall TStylesSimpleDemoStylesDialogForm::GetSelectedStyle()
{
  int ItemIndex = cxEditRepositoryMRUItem->Properties->LookupItems->
    IndexOf(((TcxEditorRow*)cxVerticalGrid->FocusedRow)->Properties->Value);
  return (TcxStyle*)cxEditRepositoryMRUItem->Properties->LookupItems->Objects[ItemIndex];
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::RefreshBinding()
{
  TcxStyle *AStyle;
  for (int I = 1; I < cxVerticalGrid->Rows->Count; I++){
    AStyle = GetCurrentStyle((TStyles)(I - 1));
    if (AStyle != NULL)
      ((TcxEditorRow*)cxVerticalGrid->Rows->Items[I])->Properties->Value =
        AStyle->Name;
  }
}
//---------------------------------------------------------------------------

TcxStyle* __fastcall TStylesSimpleDemoStylesDialogForm::GetCurrentStyle(
  TStyles AStyleID)
{
  TcxStyle *Result = NULL;
  TcxDBVerticalGrid *cxDBVerticalGrid = StylesSimpleDemoMainForm->cxDBVerticalGrid;
  switch (AStyleID){
    case sBackground:
      Result = cxDBVerticalGrid->Styles->Background;
      break;
    case sCategory:
      Result = cxDBVerticalGrid->Styles->Category;
      break;
    case sHeader:
      Result = cxDBVerticalGrid->Styles->Header;
      break;
    case sContent:
      Result = cxDBVerticalGrid->Styles->Content;
      break;
    case sInactive:
      Result = cxDBVerticalGrid->Styles->Inactive;
      break;
    case sIncSearch:
      Result = cxDBVerticalGrid->Styles->IncSearch;
      break;
    case sSelection:
      Result = cxDBVerticalGrid->Styles->Selection;
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::SetCurrentStyle(
  TcxStyle *AStyle, TStyles AStyleID)
{
  TcxDBVerticalGrid *cxDBVerticalGrid = StylesSimpleDemoMainForm->cxDBVerticalGrid;
  switch (AStyleID){
    case sBackground:
      cxDBVerticalGrid->Styles->Background = AStyle;
      break;
    case sCategory:
      cxDBVerticalGrid->Styles->Category = AStyle;
      break;
    case sHeader:
      cxDBVerticalGrid->Styles->Header = AStyle;
      break;
    case sContent:
      cxDBVerticalGrid->Styles->Content = AStyle;
      break;
    case sInactive:
      cxDBVerticalGrid->Styles->Inactive = AStyle;
      break;
    case sIncSearch:
      cxDBVerticalGrid->Styles->IncSearch = AStyle;
      break;
    case sSelection:
      cxDBVerticalGrid->Styles->Selection = AStyle;
      break;
  }
}
//---------------------------------------------------------------------------


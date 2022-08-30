//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoStylesDialog.h"
#include "StylesSimpleDemoData.h"
#include "StylesSimpleDemoEdit.h"
#include "StylesSimpleDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMRUEdit"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxMaskEdit"
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
  TStrings *ALookupItems =
    ((TcxMRUEditProperties*)tlcStyleNames->Properties)->LookupItems;
  TcxStyleRepository *ARespository = StylesSimpleDemoDataDM->StyleRepository;
  ALookupItems->Clear();
  TcxStyle *AStyle;
  for (int I = 0; I < ARespository->Count; I++){
    AStyle = (TcxStyle*)ARespository->Items[I];
    ALookupItems->AddObject(AStyle->Name, AStyle);
  }

/* remove/add the closing slash on this line to disable/enable the following code*/

  RefreshBinding();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::tlcStyleNamesPropertiesButtonClick(
  TObject *Sender)
{
  TcxStyle *AStyle = GetSelectedStyle();
  if (AStyle != NULL) 
    ChangeStyle(AStyle);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::btnRestoreClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  if (FRestoreDefaults != NULL) FRestoreDefaults(Sender);
  RefreshBinding();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::tlcStyleNamesPropertiesEditValueChanged(
  TObject *Sender)
{
  int AItemIndex = ((TcxMRUEditProperties*)tlcStyleNames->Properties)->
    LookupItems->IndexOf(((TcxCustomMRUEdit*)Sender)->EditValue);
  TcxStyle *AStyle = (TcxStyle*)((TcxMRUEditProperties*)tlcStyleNames->Properties)->
    LookupItems->Objects[AItemIndex];
  SetCurrentStyle(AStyle, (TStyles)cxTreeList->FocusedNode->Index);
}
//---------------------------------------------------------------------------

TcxStyle* __fastcall TStylesSimpleDemoStylesDialogForm::GetSelectedStyle()
{
  int AItemIndex = ((TcxMRUEditProperties*)tlcStyleNames->Properties)->LookupItems->
    IndexOf(tlcStyleNames->Values[cxTreeList->FocusedNode]);
  if (AItemIndex != -1)
    return (TcxStyle*)((TcxMRUEditProperties*)tlcStyleNames->Properties)->
      LookupItems->Objects[AItemIndex];
  else
    return NULL;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::RefreshBinding()
{
  TcxStyle *AStyle;
  for (int I = 0; I < cxTreeList->Root->Count; I++){
    AStyle = GetCurrentStyle((TStyles)I);
    if (AStyle != NULL)
      cxTreeList->Root->Items[I]->Values[1] = AStyle->Name;
    else
      cxTreeList->Root->Items[I]->Values[1] = "";
  }
}
//---------------------------------------------------------------------------

TcxStyle* __fastcall TStylesSimpleDemoStylesDialogForm::GetCurrentStyle(
  TStyles AStyleID)
{
  TcxStyle *Result = NULL;
  TcxTreeListStyles *AStyles = StylesSimpleDemoMainForm->cxDBTreeList->Styles;
  switch (AStyleID){
    case sBackground:
      Result = AStyles->Background; break;
    case sBandbackground:
      Result = AStyles->BandBackground; break;
    case sBandContent:
      Result = AStyles->BandContent; break;
    case sBandHeader:
      Result = AStyles->BandHeader; break;
    case sColumnFooter:
      Result = AStyles->ColumnFooter; break;
    case sColumnHeader:
      Result = AStyles->ColumnHeader; break;
    case sContent:
      Result = AStyles->Content; break;
    case sContentEven:
      Result = AStyles->ContentEven; break;
    case sContentOdd:
      Result = AStyles->ContentOdd; break;
    case sFooter:
      Result = AStyles->Footer; break;
    case sInactive:
      Result = AStyles->Indicator; break;
    case sIncSearch:
      Result = AStyles->IncSearch; break;
    case sIndicator:
      Result = AStyles->Indicator; break;
    case sPreview:
      Result = AStyles->Preview; break;
    case sSelection:
       Result = AStyles->Selection;
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoStylesDialogForm::SetCurrentStyle(
  TcxStyle *AStyle, TStyles AStyleID)
{
  TcxTreeListStyles *AStyles = StylesSimpleDemoMainForm->cxDBTreeList->Styles;
  switch (AStyleID){
    case sBackground:
      AStyles->Background = AStyle; break;
    case sBandbackground:
      AStyles->BandBackground = AStyle; break;
    case sBandContent:
      AStyles->BandContent = AStyle; break;
    case sBandHeader:
      AStyles->BandHeader = AStyle; break;
    case sColumnFooter:
      AStyles->ColumnFooter = AStyle; break;
    case sColumnHeader:
      AStyles->ColumnHeader = AStyle; break;
    case sContent:
      AStyles->Content = AStyle; break;
    case sContentEven:
      AStyles->ContentEven = AStyle; break;
    case sContentOdd:
      AStyles->ContentOdd = AStyle; break;
    case sFooter:
      AStyles->Footer = AStyle; break;
    case sInactive:
      AStyles->Inactive = AStyle; break;
    case sIncSearch:
      AStyles->IncSearch = AStyle; break;
    case sIndicator:
      AStyles->Indicator = AStyle; break;
    case sPreview:
      AStyles->Preview = AStyle; break;
    case sSelection:
      AStyles->Selection = AStyle;
  }
}
//---------------------------------------------------------------------------


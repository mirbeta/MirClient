//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "StylesSimpleDemoAssign.h"
#include "StylesSimpleDemoMain.h"
#include "StylesSimpleDemoData.h"
#pragma link "cxStyles"
#pragma link "cxGridTableView"

#include "cxStyles.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

enum TcxStyleType {sContent, sContentEven, sContentOdd, sBackground, sFilterBox, sFooter, sGroup,
  sGroupByBox, sHeader, sIndicator, sInactive, sIncSearch, sPreview, sSelection};


TStylesSimpleDemoAssignForm *StylesSimpleDemoAssignForm;
//---------------------------------------------------------------------------
__fastcall TStylesSimpleDemoAssignForm::
  TStylesSimpleDemoAssignForm(TComponent* Owner) : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoAssignForm::ComboBoxChange(TObject *Sender)
{
  TComboBox *ACombo;
  ACombo = ((TComboBox*)Sender);
  SetCurrentStyle(
    (TcxStyle*)(ACombo->Items->Objects[ACombo->ItemIndex]), ACombo->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoAssignForm::btnRestoreClick(TObject *Sender)
{
  if (RestoreDefaults != NULL)
    RestoreDefaults(Sender);
  RefreshBinding();
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoAssignForm::FormCreate(TObject *Sender)
{
  RefreshBinding();
}
//---------------------------------------------------------------------------

TcxStyle* TStylesSimpleDemoAssignForm::GetCurrentStyle(int AGridItemID)
{
  TcxGridTableViewStyles *AStyles = StylesSimpleDemoMainForm->tvPersons->Styles;
  switch ((TcxStyleType)AGridItemID) {
    case sContent:
      return AStyles->Content;
    case sContentEven:
      return AStyles->ContentEven;
    case sContentOdd:
      return AStyles->ContentOdd;
    case sBackground:
      return AStyles->Background;
    case sFooter:
      return AStyles->Footer;
    case sFilterBox:
      return AStyles->FilterBox;
    case sGroup:
      return AStyles->Group;
    case sGroupByBox:
      return AStyles->GroupByBox;
    case sHeader:
      return AStyles->Header;
    case sIndicator:
      return AStyles->Indicator;
    case sIncSearch:
      return AStyles->IncSearch;
    case sInactive:
      return AStyles->Inactive;
    case sPreview:
      return AStyles->Preview;
    case sSelection:
      return AStyles->Selection;
    default:
      return NULL;
  }
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoAssignForm::SetCurrentStyle(TcxStyle *AStyle, int AGridItemID)
{
  TcxGridTableViewStyles *AStyles = StylesSimpleDemoMainForm->tvPersons->Styles;
  switch ((TcxStyleType)AGridItemID) {
    case sContent:
      AStyles->Content = AStyle;
      break;
    case sContentEven:
      AStyles->ContentEven = AStyle;
      break;
    case sContentOdd:
      AStyles->ContentOdd = AStyle;
      break;
    case sBackground:
      AStyles->Background = AStyle;
      break;
    case sFooter:
      AStyles->Footer = AStyle;
      break;
    case sFilterBox:
      AStyles->FilterBox = AStyle;
      break;
    case sGroup:
      AStyles->Group = AStyle;
      break;
    case sGroupByBox:
      AStyles->GroupByBox = AStyle;
      break;
    case sHeader:
      AStyles->Header = AStyle;
      break;
    case sIndicator:
      AStyles->Indicator = AStyle;
      break;
    case sIncSearch:
      AStyles->IncSearch = AStyle;
      break;
    case sInactive:
      AStyles->Inactive = AStyle;
      break;
    case sPreview:
      AStyles->Preview = AStyle;
      break;
    case sSelection:
      AStyles->Selection = AStyle;
  }
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoAssignForm::InitComboBox(TComboBox *AComboBox)
{
  String s, CurrentStyle;
  AComboBox->Clear();
  CurrentStyle = "";
  for (int I = 0; I < StylesSimpleDemoMainDM->StyleRepository->Count; I++)
  {
	s = StylesSimpleDemoMainDM->StyleRepository->Items[I]->Name;
	AComboBox->Items->AddObject(s, StylesSimpleDemoMainDM->StyleRepository->Items[I]);
    if (StylesSimpleDemoMainDM->StyleRepository->Items[I] ==
      GetCurrentStyle(AComboBox->Tag))
        CurrentStyle = s;
  }
  AComboBox->ItemIndex = AComboBox->Items->IndexOf(CurrentStyle);
}
//---------------------------------------------------------------------------

void TStylesSimpleDemoAssignForm::RefreshBinding()
{
  for (int I = 0; I < ComponentCount; I++)
    if (dynamic_cast<TComboBox *>(Components[I]) != 0)
      InitComboBox((TComboBox*)Components[I]);
}
//---------------------------------------------------------------------------


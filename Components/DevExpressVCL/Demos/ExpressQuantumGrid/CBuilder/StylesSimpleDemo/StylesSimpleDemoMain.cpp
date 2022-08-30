//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.h>
#include <controls.hpp>

#pragma hdrstop

#include "StylesSimpleDemoMain.h"
#include "StylesSimpleDemoData.h"
#include "AboutDemoForm.h"
#include "StylesSimpleDemoAssign.h"
#include "StylesSimpleDemoEdit.h"

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
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxListBox"
#pragma link "cxContainer"
#pragma link "cxLookAndFeels"
#pragma link "cxButtons"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCardView"
#pragma link "cxGroupBox"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------

__fastcall TStylesSimpleDemoMainForm::TStylesSimpleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::btnEditClick(TObject *Sender)
{
  ChangeStyle(GetSelectedStyle());
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::btnViewClick(TObject *Sender)
{
  ChangeStyleBinding(RestoreDefaults);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::FormCreate(TObject *Sender)
{
  TcxStyle *AStyle;
  ListBox->Clear();
  for (int I = 0; I < StylesSimpleDemoMainDM->StyleRepository->Count; I++)
  {
     AStyle = (TcxStyle*)(StylesSimpleDemoMainDM->StyleRepository->Items[I]);
     ListBox->Items->AddObject(AStyle->Name, AStyle);
  }
  ListBox->ItemIndex = 0;
}
//---------------------------------------------------------------------------

TcxStyle *TStylesSimpleDemoMainForm::GetSelectedStyle()
{
  return (TcxStyle*)(ListBox->Items->Objects[ListBox->ItemIndex]);
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::RestoreDefaults(TObject *Sender)
{
  tvPersons->Styles->Background = NULL;
  tvPersons->Styles->Content = NULL;
  tvPersons->Styles->ContentEven = NULL;
  tvPersons->Styles->ContentOdd = NULL;
  tvPersons->Styles->IncSearch = NULL;
  tvPersons->Styles->Footer = NULL;
  tvPersons->Styles->FilterBox = NULL;
  tvPersons->Styles->Group = NULL;
  tvPersons->Styles->GroupByBox = NULL;
  tvPersons->Styles->Header = NULL;
  tvPersons->Styles->Indicator = NULL;
  tvPersons->Styles->Inactive = NULL;
  tvPersons->Styles->Selection = NULL;
  tvPersons->Styles->Preview = NULL;
  tvPersons->Styles->StyleSheet = StylesSimpleDemoMainDM->UserStyleSheet;
}
//---------------------------------------------------------------------------

bool TStylesSimpleDemoMainForm::ChangeStyle(TcxStyle *AStyle)
{
  bool Result;
  TStylesSimpleDemoEditForm *AStylesSimpleDemoEditForm =
    new TStylesSimpleDemoEditForm(Application);
  try {
    AStylesSimpleDemoEditForm->CurrentStyle = AStyle;
    AStylesSimpleDemoEditForm->Caption = "Edit Style - " + AStyle->Name;
    Result = (AStylesSimpleDemoEditForm->ShowModal() == mrOk);
  }
  __finally{
    delete AStylesSimpleDemoEditForm;
  }
  return Result;
}
//---------------------------------------------------------------------------

bool TStylesSimpleDemoMainForm::ChangeStyleBinding(TNotifyEvent ACallback)
{
  bool Result;
  TStylesSimpleDemoAssignForm *AStylesSimpleDemoAssignForm =
    new TStylesSimpleDemoAssignForm(Application);
  try {
    AStylesSimpleDemoAssignForm->RestoreDefaults = ACallback;
    Result = (AStylesSimpleDemoAssignForm->ShowModal() == mrOk);
  }
  __finally{
    delete AStylesSimpleDemoAssignForm;
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actHeaderExecute(TObject *Sender)
{
  tvPersons->OptionsView->Header = !(tvPersons->OptionsView->Header);
  ((TAction*)Sender)->Checked = tvPersons->OptionsView->Header;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actFooterExecute(TObject *Sender)
{
  tvPersons->OptionsView->Footer = !(tvPersons->OptionsView->Footer);
  ((TAction*)Sender)->Checked = tvPersons->OptionsView->Footer;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actIndicatorExecute(TObject *Sender)
{
  tvPersons->OptionsView->Indicator = !(tvPersons->OptionsView->Indicator);
  ((TAction*)Sender)->Checked = tvPersons->OptionsView->Indicator;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actGroupBoxExecute(TObject *Sender)
{
  tvPersons->OptionsView->GroupByBox = !(tvPersons->OptionsView->GroupByBox);
  ((TAction*)Sender)->Checked = tvPersons->OptionsView->GroupByBox;
}
//---------------------------------------------------------------------------

void __fastcall TStylesSimpleDemoMainForm::actPreviewExecute(TObject *Sender)
{
  tvPersons->Preview->Visible = !(tvPersons->Preview->Visible);
  ((TAction*)Sender)->Checked = tvPersons->Preview->Visible;
}





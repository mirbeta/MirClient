//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "EditorsMaskDemoMain.h"
#include "EditorsMaskDemoData.h"
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
#pragma link "cxStyles"
#pragma link "cxButtonEdit"
#pragma link "cxContainer"
#pragma link "cxDBEdit"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma link "cxDBNavigator"
#pragma link "cxDataStorage"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TEditorsMaskDemoMainForm *EditorsMaskDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsMaskDemoMainForm::TEditorsMaskDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void TEditorsMaskDemoMainForm::ChangeLabel(TLabel* ALabel, TcxCustomEditProperties* AProperties)
{
  AnsiString AText;
  if(((TcxCustomMaskEditPropertiesAccessor*)AProperties)->EmptyMaskAccess(
    ((TcxCustomMaskEditPropertiesAccessor*)AProperties)->EditMaskAccess()))
    AText = "No mask";
  else
    AText = GetMaskKindLabel(((TcxCustomMaskEditPropertiesAccessor*)AProperties)->MaskKindAccess());
  ALabel->Caption = AText;
}
//---------------------------------------------------------------------------

AnsiString TEditorsMaskDemoMainForm::GetMaskKindLabel(const TcxEditMaskKind AMaskKind)
{                               
  switch(AMaskKind)
  {
    case emkStandard:
      return "Delphi Standard Mask";
    case emkRegExpr:
      return "Regular Expression";
    case emkRegExprEx:
      return "Regular Expression with Auto Complete Function";
    default:
      return "Unknown";
  };
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::miShowMaskButtonsClick(
      TObject *Sender)
{
  edtPostalCode->Properties->Buttons->Items[0]->Visible = ((TMenuItem*)Sender)->Checked;
  edtPhone->Properties->Buttons->Items[0]->Visible = ((TMenuItem*)Sender)->Checked;
  edtFax->Properties->Buttons->Items[0]->Visible = ((TMenuItem*)Sender)->Checked;
  edtHomePage->Properties->Buttons->Items[0]->Visible = ((TMenuItem*)Sender)->Checked;
  edtEmail->Properties->Buttons->Items[0]->Visible = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::miDefaultMaskSettingsClick(
      TObject *Sender)
{
  edtPostalCode->Properties->MaskKind = cxDefaultPostalCodeMaskKind;
  edtPostalCode->Properties->EditMask = cxDefaultPostalCodeEditMask;
  ChangeLabel(lbInfoPostalCode, edtPostalCode->Properties);
  edtPhone->Properties->MaskKind = cxDefaultPhoneMaskKind;
  edtPhone->Properties->EditMask = cxDefaultPhoneEditMask;
  ChangeLabel(lbInfoPhone, edtPhone->Properties);
  edtFax->Properties->MaskKind = cxDefaultFaxMaskKind;
  edtFax->Properties->EditMask = cxDefaultFaxEditMask;
  ChangeLabel(lbInfoFax, edtFax->Properties);
  edtHomePage->Properties->MaskKind = cxDefaultHomePageMaskKind;
  edtHomePage->Properties->EditMask = cxDefaultHomePageEditMask;
  ChangeLabel(lbInfoHomePage, edtHomePage->Properties);
  edtEmail->Properties->MaskKind = cxDefaultEmailMaskKind;
  edtEmail->Properties->EditMask = cxDefaultEmailEditMask;
  ChangeLabel(lbInfoEmail, edtEmail->Properties);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::edtPostalCodePropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{

  ShowEditMaskDialog(((TcxDBButtonEdit*)Sender)->Properties);
  ChangeLabel(lbInfoPostalCode, ((TcxDBButtonEdit*)Sender)->Properties);
}
//---------------------------------------------------------------------------

void TEditorsMaskDemoMainForm::ShowEditMaskDialog(TcxCustomEditProperties* AProperties)
{
#if (__BORLANDC__ >= 0x0610) // C++Builder 12
  TcxEditMaskEditorDlg* ADialog = new TcxEditMaskEditorDlg(this);
#else
  TcxEditMaskEditorDlg* ADialog = new TcxEditMaskEditorDlg((void*)0);
#endif
  try {
    ADialog->MaskEditProperties = (TcxCustomMaskEditProperties*)AProperties;
    ADialog->ShowModal();
  }
  __finally {
    delete ADialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorsMaskDemoMainForm::cxDBButtonEdit1PropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  ShowEditMaskDialog(((TcxDBButtonEdit*)Sender)->Properties);
  ChangeLabel(lbInfoPhone, ((TcxDBButtonEdit*)Sender)->Properties);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::cxDBButtonEdit2PropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  ShowEditMaskDialog(((TcxDBButtonEdit*)Sender)->Properties);
  ChangeLabel(lbInfoFax, ((TcxDBButtonEdit*)Sender)->Properties);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::cxDBButtonEdit3PropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  ShowEditMaskDialog(((TcxDBButtonEdit*)Sender)->Properties);
  ChangeLabel(lbInfoHomePage, ((TcxDBButtonEdit*)Sender)->Properties);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsMaskDemoMainForm::cxDBButtonEdit4PropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  ShowEditMaskDialog(((TcxDBButtonEdit*)Sender)->Properties);
  ChangeLabel(lbInfoEmail, ((TcxDBButtonEdit*)Sender)->Properties);
}
//---------------------------------------------------------------------------


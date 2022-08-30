//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TokenEditDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxCheckBox"
#pragma link "cxCheckComboBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxCheckGroupBox"
#pragma link "dxTokenEdit"
#pragma resource "*.dfm"
TdxTokenEditDemoForm *dxTokenEditDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxTokenEditDemoForm::TdxTokenEditDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdxTokenEditDemoForm::cbCloseGlyphPositionPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->CloseGlyphPosition = TdxTokenEditElementPosition(cbCloseGlyphPosition->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TdxTokenEditDemoForm::cbDisplayMaskPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->Lookup->DisplayMask = cbDisplayMask->Text;
  if(cbDisplayMask->Properties->Items->IndexOf(cbDisplayMask->Text) == -1)
    cbDisplayMask->Properties->Items->Add(cbDisplayMask->Text);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::cbGlyphPositionPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->GlyphPosition = TdxTokenEditElementPosition(cbGlyphPosition->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::cbLookupFilterModePropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->Lookup->FilterMode = TdxTokenEditLookupFilterMode(cbLookupFilterMode->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chbAllowCustomTokensPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->AllowAddCustomTokens = chbAllowCustomTokens->Checked;
  dxTokenEdit->ValidateEdit();
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chbLookupSortedPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->Lookup->Sorted = chbLookupSorted->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chbPostOnFocusLeavePropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->PostEditValueOnFocusLeave  = chbPostOnFocusLeave->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chbReadOnlyPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->ReadOnly = chbReadOnly->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chcbLookupFilterSourcesPropertiesEditValueChanged(TObject *Sender)
{
  TdxTokenEditLookupFilterSources AFilterSources;

  AFilterSources.Clear();
  if(chcbLookupFilterSources->States[0] == cbsChecked)
	AFilterSources << tefsText;
  if(chcbLookupFilterSources->States[1] == cbsChecked)
	AFilterSources << tefsDisplayText;
  dxTokenEdit->Properties->Lookup->FilterSources = AFilterSources;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::chgbLookupPropertiesEditValueChanged(TObject *Sender)
{
  dxTokenEdit->Properties->Lookup->Active = chgbLookup->CheckBox->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::dxTokenEditPropertiesEditValueChanged(TObject *Sender)
{
  UnicodeString ACaption;

  ACaption = "Edit Value: ";
  if(dxTokenEdit->Text != "")
	ACaption += dxTokenEdit->Text;
  else
	ACaption += "(empty)";
  lbEditValue->Caption = ACaption;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::dxTokenEditPropertiesTokenClick(TObject *Sender,
          const UnicodeString ATokenText, TdxTokenEditToken *AToken)
{
  MessageDlg("Clicked token: " + ATokenText, mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::dxTokenEditPropertiesTokenDelete(TObject *Sender,
          const UnicodeString ATokenText, TdxTokenEditToken *AToken, bool &AAllow)
{
  AAllow = !chbConfirmTokenDeletion->Checked ||
	(MessageDlg("Do you want to delete token: " + ATokenText + "?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::dxTokenEditPropertiesTokenGlyphClick(TObject *Sender,
          const UnicodeString ATokenText, TdxTokenEditToken *AToken)
{
  MessageDlg("Clicked glyph of token: " + ATokenText, mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::dxTokenEditPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  bool AError;

  AError = (VarToStr(DisplayValue) != "") && !chbAllowCustomTokens->Checked &&
  	(dxTokenEdit->Properties->Tokens->FindByText(DisplayValue) == (0x0));
  Error = Error || AError;
  if(AError)
  {
	if(ErrorText != "")
	  ErrorText = ErrorText + '\n';
	ErrorText = ErrorText + "Custom tokens are not allowed: " + DisplayValue;
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::seLookupDropDownRowsPropertiesChange(TObject *Sender)
{
  if((seLookupDropDownRows->Value != Null()) && (seLookupDropDownRows->Value >= 0))
	dxTokenEdit->Properties->Lookup->DropDownRows = seLookupDropDownRows->Value;
  else
	seLookupDropDownRows->Value = 0;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::seMaxLineCountPropertiesChange(TObject *Sender)
{
  if((seMaxLineCount->Value != Null()) && (seMaxLineCount->Value >= 0))
	dxTokenEdit->Properties->MaxLineCount = seMaxLineCount->Value;
  else
	seMaxLineCount->Value = 0;
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::teEditValueDelimiterPropertiesEditValueChanged(TObject *Sender)
{
  if(teEditValueDelimiter->Text != "")
	dxTokenEdit->Properties->EditValueDelimiter = teEditValueDelimiter->Text[1];
  else
	teEditValueDelimiter->Text = dxTokenEditDefaultEditValueDelimiter;
  dxTokenEdit->ValidateEdit();
}
//---------------------------------------------------------------------------

void __fastcall TdxTokenEditDemoForm::teInputDelimitersPropertiesEditValueChanged(TObject *Sender)
{
  if(teInputDelimiters->Text != "")
	dxTokenEdit->Properties->InputDelimiters = teInputDelimiters->Text;
  else
	teInputDelimiters->Text = dxTokenEditDefaultInputDelimiters;
}
//---------------------------------------------------------------------------


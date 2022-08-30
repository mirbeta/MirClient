//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceValidationDemoMain.h"
#include "EditorsInPlaceValidationDemoData.h"
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
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBVGrid"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDropDownEdit"
#pragma link "cxHint"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMemo"
#pragma link "cxMRUEdit"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma link "dxCustomHint"
#pragma link "dxScreenTip"
#pragma resource "*.dfm"
TEditorsInPlaceValidationDemoMainForm *EditorsInPlaceValidationDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceValidationDemoMainForm::TEditorsInPlaceValidationDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::miShowEditBtnsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  VerticalGrid->OptionsView->ShowEditButtons = (TcxEditingControlEditShowButtons)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::actAboutExecute(TObject *Sender)
{
  ShowAbout(false, true);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxHintStyleControllerShowHintEx(TObject *Sender,
          String &Caption, String &HintStr, bool &CanShow, THintInfo &HintInfo)

{
	stGrid->Header->Glyph->Assign(NULL);
	TObject *AViewInfo = static_cast<TObject*>(HintInfo.HintData);
	if (dynamic_cast<TcxRowValueInfo*>(AViewInfo))
	{
		switch ((static_cast<TcxRowValueInfo*>(AViewInfo))->EditViewInfo->ErrorData->ErrorType)
		{
			case eetError: {stGrid->Header->Glyph->Assign(cxEditErrorIcon); break;}
			case eetWarning: {stGrid->Header->Glyph->Assign(cxEditWarningIcon); break; }
			case eetInfo: {stGrid->Header->Glyph->Assign(cxEditInfoIcon); break; }
			case eetCustom: {stGrid->Header->Glyph->Assign(icCustomIcon1->Picture->Bitmap); break; }
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridFirstNamePropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
		  int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  String AErrorText;
  if (DoFirstNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridFirstNameEditPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoFirstNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridLastNamePropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoLastNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridLastNameEditPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoLastNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridAddressPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  String AErrorText;
  if (DoAddressValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetInfo;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridAddressEditPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoAddressValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridPhoneNumberPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
		  int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  String AErrorText;
  if (DoPhoneNumberValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetWarning;
	String AFullName = GetPersonFullName(Sender, ARecordIndex);
	if (Trim(AFullName) > "")
	{
	  AData->ErrorText = AErrorText + " for " + AFullName;
	}
	else
	{
	  AData->ErrorText = AErrorText;
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridPhoneNumberEditPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoPhoneNumberValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridEmailPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
		  int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  String AErrorText;
  if (DoEmailValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetCustom;
	AData->ErrorIcon->Assign(icCustomIcon1->Picture->Bitmap);
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::VerticalGridEmailEditPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoEmailValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::InitializeEditors(TObject *Sender)
{
  TcxEditValidationOptions AValidationOptions;
  if (miValidationRaiseException->Checked)
  {
	AValidationOptions << evoRaiseException;
  }
  if (miValidationShowErrorIcon->Checked)
  {
	AValidationOptions << evoShowErrorIcon;
  }
  if (miValidationAllowLoseFocus->Checked)
  {
	AValidationOptions << evoAllowLoseFocus;
  }

  VerticalGridFirstName->Properties->EditProperties->ValidationOptions = AValidationOptions;
  VerticalGridLastName->Properties->EditProperties->ValidationOptions = AValidationOptions;
  VerticalGridAddress->Properties->EditProperties->ValidationOptions = AValidationOptions;
  VerticalGridPhoneNumber->Properties->EditProperties->ValidationOptions = AValidationOptions;
  VerticalGridEmail->Properties->EditProperties->ValidationOptions = AValidationOptions;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoAddressValidate(const Variant &AValue, TCaption &AErrorText)
{
  int I = (dynamic_cast<TcxComboBoxProperties*> (VerticalGridAddress->Properties->EditProperties))->Items->IndexOf(VarToStr(AValue));
  bool rv = (I == -1);
  if (rv)
  {
	AErrorText = "Please select an address from the list";
  }
  return rv;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoEmailValidate(const Variant &AValue, TCaption &AErrorText)
{
  String S = VarToStr(AValue);
  bool rv = ((S != "")&&(!Cxregexpr::IsTextFullValid(S, "[A-z0-9_-]+@[A-z0-9_-]+\\.[A-z0-9_-]+(\\.[A-z]+)*")));
  if (rv)
  {
	AErrorText = "Please enter a valid email address";
  }
  return rv;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoFirstNameValidate(const Variant &AValue, TCaption &AErrorText)
{
  bool rv = (VarToStr(AValue) == "");
  if (rv)
  {
	AErrorText = "Please enter a value";
  }
  return rv;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoLastNameValidate(const Variant &AValue, TCaption &AErrorText)
{
  bool rv = (VarToStr(AValue) == "");
  if (rv)
  {
	AErrorText = "Please enter a value";
  }
  return rv;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoPhoneNumberValidate(const Variant &AValue, TCaption &AErrorText)
{
  String S = VarToStr(AValue);
  bool rv = ((S == "")||(!Cxregexpr::IsTextValid(S, "(\\(\\d\\d\\d\\)' ')?\\d\\d\\d-\\d\\d\\d\\d")));
  if (rv)
  {
	AErrorText = "Please enter a valid phone number";
  }
  return rv;
}
//---------------------------------------------------------------------------

String TEditorsInPlaceValidationDemoMainForm::GetPersonFullName(TcxCustomEditorRowProperties *AEditorRowProperties, int ARecordIndex)
{
  String AFirstName = VarToStr(VerticalGridFirstName->Properties->Values[ARecordIndex]);
  String ALastName = VarToStr(VerticalGridLastName->Properties->Values[ARecordIndex]);
  if ((Trim(AFirstName) > "")&&(Trim(ALastName) > ""))
  {
	return AFirstName + " " + ALastName;
  }
  else
  {
	return AFirstName + ALastName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::FormCreate(TObject *Sender)

{
  InitializeEditors(this);
}
//---------------------------------------------------------------------------


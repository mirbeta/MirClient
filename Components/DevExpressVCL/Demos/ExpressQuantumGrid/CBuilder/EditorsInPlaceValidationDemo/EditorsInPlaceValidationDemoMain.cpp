//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceValidationDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridPopupMenu"
#pragma link "cxHint"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma link "cxTextEdit"
#pragma link "dxCustomHint"
#pragma link "dxScreenTip"
#pragma link "cxRegExpr"
#pragma link "cxGridRows"
#pragma resource "*.dfm"
TEditorsInPlaceValidationDemoMainForm *EditorsInPlaceValidationDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceValidationDemoMainForm::TEditorsInPlaceValidationDemoMainForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnAddressPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoAddressValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnAddressValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoAddressValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetInfo;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnEmailPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoEmailValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnEmailValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
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

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnFirstNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoFirstNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnFirstNameValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoFirstNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnLastNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoLastNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnLastNameValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoLastNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnPhoneNumberPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoPhoneNumberValidate(DisplayValue, ErrorText);
  String AFullName = GetPersonFullName(cxGridTableView->ViewData->EditingRecord);
  if (Trim(AFullName) > "")
  {
	ErrorText = ErrorText + " for " + AFullName;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxGridTableViewColumnPhoneNumberValidateDrawValue(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoPhoneNumberValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetWarning;
	String AFullName = GetPersonFullName(ARecord);
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

void __fastcall TEditorsInPlaceValidationDemoMainForm::cxHintStyleControllerShowHintEx(TObject *Sender,
		  String &Caption, String &HintStr, bool &CanShow, THintInfo &HintInfo)

{
	stGrid->Header->Glyph->Assign(NULL);
	TObject *AViewInfo = static_cast<TObject*>(HintInfo.HintData);
	if (dynamic_cast<TcxGridDataCellViewInfo*>(AViewInfo))
	{
		switch ((static_cast<TcxGridDataCellViewInfo*>(AViewInfo))->EditViewInfo->ErrorData->ErrorType)
		{
			case eetError: {stGrid->Header->Glyph->Assign(cxEditErrorIcon); break;}
			case eetWarning: {stGrid->Header->Glyph->Assign(cxEditWarningIcon); break; }
			case eetInfo: {stGrid->Header->Glyph->Assign(cxEditInfoIcon); break; }
			case eetCustom: {stGrid->Header->Glyph->Assign(icCustomIcon1->Picture->Bitmap); break; }
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::FormCreate(TObject *Sender)

{
  InitializeEditors(this);
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

  cxGridTableViewColumnFirstName->Properties->ValidationOptions = AValidationOptions;
  cxGridTableViewColumnLastName->Properties->ValidationOptions = AValidationOptions;
  cxGridTableViewColumnAddress->Properties->ValidationOptions = AValidationOptions;
  cxGridTableViewColumnPhoneNumber->Properties->ValidationOptions = AValidationOptions;
  cxGridTableViewColumnEmail->Properties->ValidationOptions = AValidationOptions;
}
//---------------------------------------------------------------------------

bool TEditorsInPlaceValidationDemoMainForm::DoAddressValidate(const Variant &AValue, TCaption &AErrorText)
{
  int I = (dynamic_cast<TcxComboBoxProperties*> (cxGridTableViewColumnAddress->Properties))->Items->IndexOf(VarToStr(AValue));
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

String TEditorsInPlaceValidationDemoMainForm::GetPersonFullName(TcxCustomGridRecord *ARecord)
{
  String AFirstName = VarToStr(ARecord->Values[cxGridTableViewColumnFirstName->Index]);
  String ALastName = VarToStr(ARecord->Values[cxGridTableViewColumnLastName->Index]);
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



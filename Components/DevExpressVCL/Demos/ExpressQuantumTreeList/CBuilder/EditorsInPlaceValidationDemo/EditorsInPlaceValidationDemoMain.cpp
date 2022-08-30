//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsInPlaceValidationDemoMain.h"
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
#pragma link "cxButtonEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxDropDownEdit"
#pragma link "cxImageComboBox"
#pragma link "cxInplaceContainer"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxMRUEdit"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxTLdxBarBuiltInMenu"
#pragma link "cxHint"
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
  cxTreeList1->OptionsView->ShowEditButtons =
    (TcxEditingControlEditShowButtons)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxHintStyleControllerShowHintEx(TObject *Sender,
		  String &Caption, String &HintStr, bool &CanShow, THintInfo &HintInfo)

{
	stGrid->Header->Glyph->Assign(NULL);
	TObject *AViewInfo = static_cast<TObject*>(HintInfo.HintData);
	if (dynamic_cast<TcxTreeListEditCellViewInfo*>(AViewInfo))
	{
		switch ((static_cast<TcxTreeListEditCellViewInfo*>(AViewInfo))->EditViewInfo->ErrorData->ErrorType)
		{
			case eetError: {stGrid->Header->Glyph->Assign(cxEditErrorIcon); break;}
			case eetWarning: {stGrid->Header->Glyph->Assign(cxEditWarningIcon); break; }
			case eetInfo: {stGrid->Header->Glyph->Assign(cxEditInfoIcon); break; }
			case eetCustom: {stGrid->Header->Glyph->Assign(icCustomIcon1->Picture->Bitmap); break; }
		}
	}
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

  cxTreeList1ColumnFirstName->Properties->ValidationOptions = AValidationOptions;
  cxTreeList1ColumnLastName->Properties->ValidationOptions = AValidationOptions;
  cxTreeList1ColumnAddress->Properties->ValidationOptions = AValidationOptions;
  cxTreeList1ColumnPhoneNumber->Properties->ValidationOptions = AValidationOptions;
  cxTreeList1ColumnEmail->Properties->ValidationOptions = AValidationOptions;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceValidationDemoMainForm::FormCreate(TObject *Sender)

{
  InitializeEditors(this);
}
//---------------------------------------------------------------------------
bool TEditorsInPlaceValidationDemoMainForm::DoAddressValidate(const Variant &AValue, TCaption &AErrorText)
{
  int I = (dynamic_cast<TcxComboBoxProperties*> (cxTreeList1ColumnAddress->Properties))->Items->IndexOf(VarToStr(AValue));
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
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnFirstNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoFirstNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnFirstNameValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoFirstNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnAddressPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoAddressValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnAddressValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoAddressValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetInfo;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnEmailPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoEmailValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnEmailValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)
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
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnLastNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoLastNameValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnLastNameValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)

{
  String AErrorText;
  if (DoLastNameValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnPhoneNumberPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
  Error = DoPhoneNumberValidate(DisplayValue, ErrorText);
}
//---------------------------------------------------------------------------
void __fastcall TEditorsInPlaceValidationDemoMainForm::cxTreeList1ColumnPhoneNumberValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)
{
  String AErrorText;
  if (DoPhoneNumberValidate(AValue, AErrorText))
  {
	AData->ErrorType = eetError;
	AData->ErrorText = AErrorText;
  }
}
//---------------------------------------------------------------------------

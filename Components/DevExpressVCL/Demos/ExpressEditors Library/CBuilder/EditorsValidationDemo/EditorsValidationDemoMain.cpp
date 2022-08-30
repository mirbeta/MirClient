//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsValidationDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxHint"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxCustomHint"
#pragma link "dxScreenTip"
#pragma link "cxRegExpr"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"
TEditorsValidationDemoMainForm *EditorsValidationDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsValidationDemoMainForm::TEditorsValidationDemoMainForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEditorsValidationDemoMainForm::btValidateClick(TObject *Sender)
{
	Validation();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::cbAddressPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
	Error = (cbAddress->ItemIndex < 0);
	ErrorText = "Please select an address from the list";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::cxHintStyleControllerShowHintEx(
	  TObject *Sender, String &Caption, String &HintStr, bool &CanShow, THintInfo &HintInfo)
{
	CanShow = (HintInfo.HintStr > " ");
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::cxSpinEditPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
	int AValue = StrToIntDef(VarToStr(DisplayValue), 0);
	Error = (AValue < 1)||(AValue > 100);
	ErrorText = "Please enter a value between 1 and 100";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::edEMailPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
	Error = Pos("@", VarToStr(DisplayValue)) == 0;
	ErrorText = "Please enter a valid email address";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::edNotEmptyPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
	Error = VarToStr(DisplayValue) == "";
	ErrorText = "Please enter a value";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::edPersonPropertiesValidate(TObject *Sender,
		  Variant &DisplayValue, TCaption &ErrorText, bool &Error)
{
	String AValue = VarToStr(DisplayValue);
	Error = (AValue == "")||(!Cxregexpr::IsTextValid(AValue, "(Dr\. | Mr\. | Mrs\. | Miss | Ms\.) ' ' .+"));
	ErrorText = "Please enter a valid person name";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::FormShow(TObject *Sender)
{
	InitializeEditors(this);
	Validation();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::InitializeEditors(TObject *Sender)
{
	TcxEditValidationOptions AValidationOptions;

	edEMail->Properties->ErrorIcon->Assign(cxEditWarningIcon);
	cbAddress->Properties->ErrorIcon->Assign(cxEditInfoIcon);

	Graphics::TBitmap *ACustomIcon = new Graphics::TBitmap();
	CustomIconList->GetImage(0, ACustomIcon);
	edPerson->Properties->ErrorIcon->SetBitmap(ACustomIcon);

	if (cbValidationRaiseException->Checked)
	{
		AValidationOptions << evoRaiseException;
	}
	if (cbValidationShowErrorIcons->Checked)
	{
		AValidationOptions << evoShowErrorIcon;
	}
	if (cbValidationAllowLoseFocus->Checked)
	{
		AValidationOptions << evoAllowLoseFocus;
	}
	cbAddress->Properties->ValidationOptions = AValidationOptions;
	cxSpinEdit->Properties->ValidationOptions = AValidationOptions;
	edEMail->Properties->ValidationOptions = AValidationOptions;
	edNotEmpty->Properties->ValidationOptions = AValidationOptions;
	edPerson->Properties->ValidationOptions = AValidationOptions;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsValidationDemoMainForm::Validation()
{
	edNotEmpty->ValidateEdit();
	cxSpinEdit->ValidateEdit();
	edEMail->ValidateEdit();
	cbAddress->ValidateEdit();
	edPerson->ValidateEdit();
}
//---------------------------------------------------------------------------
void __fastcall TEditorsValidationDemoMainForm::dxToggleSwitch1PropertiesChange(TObject *Sender)
{
	TLeftRight AAlign;
	if (dxToggleSwitch1->Checked) {
		AAlign = taRightJustify;
	}
	else
	{
		AAlign = taLeftJustify;
	}
	cbAddress->Properties->ValidationErrorIconAlignment = AAlign;
	cxSpinEdit->Properties->ValidationErrorIconAlignment = AAlign;
	edEMail->Properties->ValidationErrorIconAlignment = AAlign;
	edNotEmpty->Properties->ValidationErrorIconAlignment = AAlign;
	edPerson->Properties->ValidationErrorIconAlignment = AAlign;
}
//---------------------------------------------------------------------------


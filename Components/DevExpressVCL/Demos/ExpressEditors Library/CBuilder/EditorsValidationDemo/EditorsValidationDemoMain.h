//---------------------------------------------------------------------------

#ifndef EditorsValidationDemoMainH
#define EditorsValidationDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include <Menus.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxHint.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxCustomHint.hpp"
#include "dxScreenTip.hpp"
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include "dxToggleSwitch.hpp"
//---------------------------------------------------------------------------
class TEditorsValidationDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TcxComboBox *cbAddress;
	TcxLabel *cxLabel1;
	TcxLabel *cxLabel2;
	TcxLabel *cxLabel3;
	TcxLabel *cxLabel4;
	TcxLabel *cxLabel5;
	TcxSpinEdit *cxSpinEdit;
	TcxTextEdit *edEMail;
	TcxTextEdit *edNotEmpty;
	TcxTextEdit *edPerson;
	TcxGroupBox *cxGroupBox1;
	TcxCheckBox *cbValidationRaiseException;
	TcxCheckBox *cbValidationShowErrorIcons;
	TcxCheckBox *cbValidationAllowLoseFocus;
	TcxButton *btValidate;
	TcxHintStyleController *cxHintStyleController;
	TdxScreenTipRepository *dxScreenTipRepository;
	TdxScreenTip *stError;
	TdxScreenTip *stWarning;
	TdxScreenTip *stInfo;
	TdxScreenTip *stCustom;
	TcxImageList *CustomIconList;
	TdxToggleSwitch *dxToggleSwitch1;
	void __fastcall btValidateClick(TObject *Sender);
	void __fastcall cbAddressPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxHintStyleControllerShowHintEx(TObject *Sender,
		  String &Caption, String &HintStr, bool &CanShow, THintInfo &HintInfo);
	void __fastcall cxSpinEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall edEMailPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall edNotEmptyPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall edPersonPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall InitializeEditors(TObject *Sender);
	void __fastcall dxToggleSwitch1PropertiesChange(TObject *Sender);
private:	// User declarations
	void __fastcall Validation();
public:		// User declarations
	__fastcall TEditorsValidationDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsValidationDemoMainForm *EditorsValidationDemoMainForm;
//---------------------------------------------------------------------------
#endif

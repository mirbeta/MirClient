//---------------------------------------------------------------------------

#ifndef EditorsInPlaceValidationDemoMainH
#define EditorsInPlaceValidationDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxDBVGrid.hpp"
#include "cxInplaceContainer.hpp"
#include "cxVGrid.hpp"
#include "cxBlobEdit.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDropDownEdit.hpp"
#include "cxHint.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMemo.hpp"
#include "cxMRUEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "dxCustomHint.hpp"
#include "dxScreenTip.hpp"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TEditorsInPlaceValidationDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxHintStyleController *cxHintStyleController;
	TdxScreenTipRepository *dxScreenTipRepository;
	TdxScreenTip *stGrid;
	TcxImageCollection *icCustomIconList;
	TcxImageCollectionItem *icCustomIcon1;
	TcxDBVerticalGrid *VerticalGrid;
	TcxDBEditorRow *VerticalGridFirstName;
	TcxDBEditorRow *VerticalGridLastName;
	TcxDBEditorRow *VerticalGridAddress;
	TcxDBEditorRow *VerticalGridPhoneNumber;
	TcxDBEditorRow *VerticalGridEmail;
	TMenuItem *miValidationAllowLoseFocus;
	TMenuItem *miValidationRaiseException;
	TMenuItem *miValidationShowErrorIcon;
	TMenuItem *ValidationOptions1;
  void __fastcall miShowEditBtnsClick(TObject *Sender);
  void __fastcall actAboutExecute(TObject *Sender);
	void __fastcall cxHintStyleControllerShowHintEx(TObject *Sender, String &Caption,
          String &HintStr, bool &CanShow, THintInfo &HintInfo);
	void __fastcall VerticalGridFirstNameEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall VerticalGridFirstNamePropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall VerticalGridLastNamePropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall VerticalGridLastNameEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall VerticalGridAddressEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall VerticalGridAddressPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall VerticalGridPhoneNumberPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall VerticalGridPhoneNumberEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall VerticalGridEmailPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall VerticalGridEmailEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
		  TCaption &ErrorText, bool &Error);
	void __fastcall InitializeEditors(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);

private:	// User declarations
	bool DoAddressValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoEmailValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoFirstNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoLastNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoPhoneNumberValidate(const Variant &AValue, TCaption &AErrorText);
//	AEditorRowProperties: TcxCustomEditorRowProperties; ARecordIndex: Integer
	String GetPersonFullName(TcxCustomEditorRowProperties *AEditorRowProperties, int ARecordIndex);
public:		// User declarations
  __fastcall TEditorsInPlaceValidationDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceValidationDemoMainForm *EditorsInPlaceValidationDemoMainForm;
//---------------------------------------------------------------------------
#endif

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
#include "cxButtonEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxDropDownEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxInplaceContainer.hpp"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxBlobEdit.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxMRUEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxTLdxBarBuiltInMenu.hpp"
#include <Dialogs.hpp>
#include "cxHint.hpp"
#include "dxCustomHint.hpp"
#include "dxScreenTip.hpp"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TEditorsInPlaceValidationDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxTreeList *cxTreeList1;
	TcxTreeListColumn *cxTreeList1ColumnFirstName;
	TcxTreeListColumn *cxTreeList1ColumnLastName;
	TcxTreeListColumn *cxTreeList1ColumnAddress;
	TcxTreeListColumn *cxTreeList1ColumnPhoneNumber;
	TcxTreeListColumn *cxTreeList1ColumnEmail;
	TcxHintStyleController *cxHintStyleController;
	TdxScreenTipRepository *dxScreenTipRepository;
	TdxScreenTip *stGrid;
	TcxImageCollection *icCustomIconList;
	TcxImageCollectionItem *icCustomIcon1;
	TMenuItem *miValidationRaiseException;
	TMenuItem *miValidationShowErrorIcon;
	TMenuItem *miValidationAllowLoseFocus;
	void __fastcall miShowEditBtnsClick(TObject *Sender);
	void __fastcall cxHintStyleControllerShowHintEx(TObject *Sender, String &Caption,
		  String &HintStr, bool &CanShow, THintInfo &HintInfo);
	void __fastcall cxTreeList1ColumnFirstNamePropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxTreeList1ColumnFirstNameValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall cxTreeList1ColumnAddressPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxTreeList1ColumnAddressValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall cxTreeList1ColumnEmailPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxTreeList1ColumnEmailValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall cxTreeList1ColumnLastNamePropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxTreeList1ColumnLastNameValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall cxTreeList1ColumnPhoneNumberPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall cxTreeList1ColumnPhoneNumberValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall InitializeEditors(TObject *Sender);
private:	// User declarations
	bool DoAddressValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoEmailValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoFirstNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoLastNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoPhoneNumberValidate(const Variant &AValue, TCaption &AErrorText);
public:		// User declarations
  __fastcall TEditorsInPlaceValidationDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceValidationDemoMainForm *EditorsInPlaceValidationDemoMainForm;
//---------------------------------------------------------------------------
#endif

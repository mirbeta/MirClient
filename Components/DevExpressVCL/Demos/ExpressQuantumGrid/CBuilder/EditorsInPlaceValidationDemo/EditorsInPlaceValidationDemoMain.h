//---------------------------------------------------------------------------

#ifndef EditorsInPlaceValidationDemoMainH
#define EditorsInPlaceValidationDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxHint.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
#include "cxTextEdit.hpp"
#include "dxCustomHint.hpp"
#include "dxScreenTip.hpp"
#include "cxGridRows.hpp"
#include <Graphics.hpp>
#include <typeinfo>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TEditorsInPlaceValidationDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid *cxGrid;
	TcxGridTableView *cxGridTableView;
	TcxGridColumn *cxGridTableViewColumnFirstName;
	TcxGridColumn *cxGridTableViewColumnLastName;
	TcxGridColumn *cxGridTableViewColumnAddress;
	TcxGridColumn *cxGridTableViewColumnPhoneNumber;
	TcxGridColumn *cxGridTableViewColumnEmail;
	TcxGridLevel *cxGridLevel;
	TcxGridPopupMenu *cxGridPopupMenu1;
	TcxHintStyleController *cxHintStyleController;
	TdxScreenTipRepository *dxScreenTipRepository;
	TdxScreenTip *stGrid;
	TcxImageCollection *icCustomIconList;
	TcxImageCollectionItem *icCustomIcon1;
	TMenuItem *ValidationOptions1;
	TMenuItem *miValidationRaiseException;
	TMenuItem *miValidationShowErrorIcon;
	TMenuItem *miValidationAllowLoseFocus;
	void __fastcall cxGridTableViewColumnAddressPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall cxGridTableViewColumnAddressValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall cxGridTableViewColumnEmailPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall cxGridTableViewColumnEmailValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall cxGridTableViewColumnFirstNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall cxGridTableViewColumnFirstNameValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall cxGridTableViewColumnLastNamePropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall cxGridTableViewColumnLastNameValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall cxGridTableViewColumnPhoneNumberPropertiesValidate(TObject *Sender,
          Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall cxGridTableViewColumnPhoneNumberValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall cxHintStyleControllerShowHintEx(TObject *Sender, String &Caption,
          String &HintStr, bool &CanShow, THintInfo &HintInfo);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall InitializeEditors(TObject *Sender);

private:	// User declarations
	bool DoAddressValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoEmailValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoFirstNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoLastNameValidate(const Variant &AValue, TCaption &AErrorText);
	bool DoPhoneNumberValidate(const Variant &AValue, TCaption &AErrorText);
	String GetPersonFullName(TcxCustomGridRecord *ARecord);
public:		// User declarations
	__fastcall TEditorsInPlaceValidationDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceValidationDemoMainForm *EditorsInPlaceValidationDemoMainForm;
//---------------------------------------------------------------------------
#endif

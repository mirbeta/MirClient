//---------------------------------------------------------------------------

#ifndef TokenEditDemoMainH
#define TokenEditDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Menus.hpp>
#include "BaseForm.h"
#include "cxCheckBox.hpp"
#include "cxCheckComboBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxCheckGroupBox.hpp"
#include "dxTokenEdit.hpp"
//---------------------------------------------------------------------------
class TdxTokenEditDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGroupBox *gbMain;
	TcxGroupBox *gbSample;
	TdxTokenEdit *dxTokenEdit;
	TcxLabel *lbEditValue;
	TcxGroupBox *gbOptions;
	TcxLabel *lbCloseGlyphPosition;
	TcxComboBox *cbCloseGlyphPosition;
	TcxLabel *lbGlyphPosition;
	TcxComboBox *cbGlyphPosition;
	TcxLabel *lbEditValueDelimiter;
	TcxTextEdit *teEditValueDelimiter;
	TcxLabel *lbInputDelimiters;
	TcxTextEdit *teInputDelimiters;
	TcxLabel *lbMaxLineCount;
	TcxSpinEdit *seMaxLineCount;
	TcxCheckBox *chbReadOnly;
	TcxCheckBox *chbAllowCustomTokens;
	TcxCheckBox *chbConfirmTokenDeletion;
	TdxCheckGroupBox *chgbLookup;
	TcxCheckBox *chbLookupSorted;
	TcxLabel *lbLookupDropDownRows;
	TcxSpinEdit *seLookupDropDownRows;
	TcxLabel *lbLookupFilterMode;
	TcxComboBox *cbLookupFilterMode;
	TcxLabel *lbLookupFilterSources;
	TcxCheckComboBox *chcbLookupFilterSources;
	TcxImageList *ilSmall;
	TcxLabel *lbDisplayMask;
	TcxComboBox *cbDisplayMask;
	TcxCheckBox *chbPostOnFocusLeave;
    void __fastcall cbCloseGlyphPositionPropertiesEditValueChanged(TObject *Sender);
	void __fastcall cbDisplayMaskPropertiesEditValueChanged(TObject *Sender);
	void __fastcall cbGlyphPositionPropertiesEditValueChanged(TObject *Sender);
	void __fastcall cbLookupFilterModePropertiesEditValueChanged(TObject *Sender);
	void __fastcall chbAllowCustomTokensPropertiesEditValueChanged(TObject *Sender);
	void __fastcall chbLookupSortedPropertiesEditValueChanged(TObject *Sender);
	void __fastcall chbPostOnFocusLeavePropertiesEditValueChanged(TObject *Sender);
    void __fastcall chbReadOnlyPropertiesEditValueChanged(TObject *Sender);
	void __fastcall chcbLookupFilterSourcesPropertiesEditValueChanged(TObject *Sender);
	void __fastcall chgbLookupPropertiesEditValueChanged(TObject *Sender);
	void __fastcall dxTokenEditPropertiesEditValueChanged(TObject *Sender);
	void __fastcall dxTokenEditPropertiesTokenClick(TObject *Sender, const UnicodeString ATokenText,
          TdxTokenEditToken *AToken);
	void __fastcall dxTokenEditPropertiesTokenDelete(TObject *Sender, const UnicodeString ATokenText,
          TdxTokenEditToken *AToken, bool &AAllow);
	void __fastcall dxTokenEditPropertiesTokenGlyphClick(TObject *Sender, const UnicodeString ATokenText,
          TdxTokenEditToken *AToken);
	void __fastcall dxTokenEditPropertiesValidate(TObject *Sender, Variant &DisplayValue,
          TCaption &ErrorText, bool &Error);
	void __fastcall seLookupDropDownRowsPropertiesChange(TObject *Sender);
	void __fastcall seMaxLineCountPropertiesChange(TObject *Sender);
	void __fastcall teEditValueDelimiterPropertiesEditValueChanged(TObject *Sender);
	void __fastcall teInputDelimitersPropertiesEditValueChanged(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TdxTokenEditDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxTokenEditDemoForm *dxTokenEditDemoForm;
//---------------------------------------------------------------------------
#endif

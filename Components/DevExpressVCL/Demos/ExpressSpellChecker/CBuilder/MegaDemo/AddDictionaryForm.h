//---------------------------------------------------------------------------

#ifndef AddDictionaryFormH
#define AddDictionaryFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtonEdit.hpp"
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "dxSpellChecker.hpp"
#include "dxISpellDecompressor.hpp"
#include "dxHunspellDictionary.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfmAddDictionary : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TBevel *Bevel1;
	TLabel *Label5;
	TcxRadioGroup *rgDictionatyType;
	TcxHyperLinkEdit *hlLink;
	TcxButton *btnAdd;
	TcxButton *btnCancel;
	TcxButtonEdit *beAffFile;
	TcxButtonEdit *beDicFile;
	TcxComboBox *cbLang;
	TcxComboBox *cbCodePage;
	TOpenDialog *OpenDialog1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall rgDictionatyTypePropertiesChange(TObject *Sender);
	void __fastcall beAffFilePropertiesButtonClick(TObject *Sender, int AButtonIndex);
	void __fastcall beDicFilePropertiesButtonClick(TObject *Sender, int AButtonIndex);
	void __fastcall beAffFilePropertiesChange(TObject *Sender);


private:	// User declarations
public:		// User declarations
	__fastcall TfmAddDictionary(TComponent* Owner);
	void __fastcall Add(TdxCustomSpellChecker *ASpellChecker);
};

//---------------------------------------------------------------------------
extern PACKAGE TfmAddDictionary *fmAddDictionary;
//---------------------------------------------------------------------------
#endif

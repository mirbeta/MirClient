//---------------------------------------------------------------------------

#ifndef SimpleDemoMainH
#define SimpleDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCalendar.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxSpellChecker.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxGraphics.hpp"
//---------------------------------------------------------------------------
class TfmCV : public TForm
{
__published:	// IDE-managed Components
	TdxSpellChecker *dxSpellChecker1;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TActionList *alMain;
	TAction *actDownloads;
	TAction *actSupport;
	TAction *actDXOnTheWeb;
	TAction *actProducts;
	TAction *actExit;
	TAction *aOutlookSpellType;
	TAction *aWordSpellType;
	TAction *aCheckFromCursorPos;
	TAction *aCheckSelectedTextFirst;
	TAction *aIgnoreEmails;
	TAction *aIgnoreMixedCaseWords;
	TAction *aCAYTActive;
	TAction *aIgnoreRepeatedWords;
	TAction *aIgnoreUpperCaseWords;
	TAction *aIgnoreURLs;
	TAction *aIgnoreWordsWithNumbers;
	TAction *aCheckSpelling;
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *CheckSpelling1;
	TMenuItem *N1;
	TMenuItem *Exit1;
	TMenuItem *Options1;
	TMenuItem *dfsd1;
	TMenuItem *Outlook1;
	TMenuItem *Word1;
	TMenuItem *Spelling1;
	TMenuItem *CheckFromCursorPos1;
	TMenuItem *CheckSelectedTextFirst1;
	TMenuItem *IgnoreEmails1;
	TMenuItem *IgnoreMixedCaseWords1;
	TMenuItem *IgnoreRepeatedWords1;
	TMenuItem *IgnoreUppercaseWords1;
	TMenuItem *IgnoreURLs1;
	TMenuItem *IgnoreWordsWithNumbers1;
	TMenuItem *aCAYTActive1;
	TMenuItem *Help1;
	TMenuItem *DeveloperExpressProducts1;
	TMenuItem *DeveloperExpressDownloads1;
	TMenuItem *DeveloperExpressontheWeb1;
	TMenuItem *DevExpressSupportCenter1;
	TcxGroupBox *gbPersonal;
	TcxLabel *cxLabel8;
	TcxLabel *cxLabel1;
	TcxTextEdit *cxTextEdit5;
	TcxLabel *cxLabel13;
	TcxMaskEdit *cxMaskEdit3;
	TcxLabel *cxLabel14;
	TcxDateEdit *cxDateEdit2;
	TcxMaskEdit *cxMaskEdit4;
	TcxLabel *cxLabel15;
	TcxTextEdit *cxTextEdit6;
	TcxTextEdit *edtName;
	TcxLabel *cxLabel16;
	TcxLabel *cxLabel17;
	TcxGroupBox *gbProfessional;
	TcxLabel *cxLabel6;
	TcxTextEdit *cxTextEdit3;
	TcxLabel *cxLabel7;
	TcxMemo *cxMemo1;
	TcxLabel *cxLabel10;
	TcxLabel *cxLabel11;
	TcxRichEdit *cxRichEdit2;
	TcxRichEdit *cxRichEdit1;
	TPanel *Panel1;
	TcxButton *btnCheckSpelling;
	TcxButton *cxButton1;
	TcxGroupBox *cxGroupBox1;
	void __fastcall dxSpellChecker1CheckAsYouTypeStart(
          TdxCustomSpellChecker *Sender, TWinControl *AControl, bool &AAllow);
	void __fastcall dxSpellChecker1CheckControlInContainer(
		  TdxCustomSpellChecker *Sender, TWinControl *AControl, bool &AAllow,
		  bool &AContinue);
	void __fastcall actDXOnTheWebExecute(TObject *Sender);
	void __fastcall actExitExecute(TObject *Sender);
	void __fastcall aOutlookSpellTypeExecute(TObject *Sender);
	void __fastcall aCheckFromCursorPosExecute(TObject *Sender);
	void __fastcall aCheckSelectedTextFirstExecute(TObject *Sender);
	void __fastcall aIgnoreEmailsExecute(TObject *Sender);
	void __fastcall aIgnoreMixedCaseWordsExecute(TObject *Sender);
	void __fastcall aCAYTActiveExecute(TObject *Sender);
	void __fastcall aIgnoreRepeatedWordsExecute(TObject *Sender);
	void __fastcall aIgnoreUpperCaseWordsExecute(TObject *Sender);
	void __fastcall aIgnoreURLsExecute(TObject *Sender);
	void __fastcall aIgnoreWordsWithNumbersExecute(TObject *Sender);
	void __fastcall aCheckSpellingExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfmCV(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmCV *fmCV;
//---------------------------------------------------------------------------
#endif

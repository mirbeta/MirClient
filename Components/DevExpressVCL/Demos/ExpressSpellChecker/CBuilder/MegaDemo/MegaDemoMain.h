//---------------------------------------------------------------------------

#ifndef MegaDemoMainH
#define MegaDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxBarEditItem.hpp"
#include "cxButtons.hpp"
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxCheckGroup.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxImage.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxPC.hpp"
#include "cxRadioGroup.hpp"
#include "cxRichEdit.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "dxBar.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxmdaset.hpp"
#include "dxSpellChecker.hpp"
#include <ActnList.hpp>
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include <jpeg.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
enum dxSitePage { spDownloads, spSupport, spStart, spProducts, spMyDX };
//---------------------------------------------------------------------------
const
  PCHAR 
    dxDownloadURL = "http://www.devexpress.com/downloads",
    dxSupportURL = "http://www.devexpress.com/Support/Center",
    dxStartURL = "http://www.devexpress.com",
    dxProductsURL = "http://www.devexpress.com/products",
    dxMyDXURL = "http://www.mydevexpress.com";
//---------------------------------------------------------------------------
class TfmMain : public TForm
{
__published:	// IDE-managed Components
	TdxSpellChecker *dxSpellChecker1;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TdxBarManager *dxBarManager;
	TdxBar *dxBarManager1Bar1;
	TcxBarEditItem *beiSearch;
	TdxBarButton *dxBarButton1;
	TdxBarButton *CheckSpelling1;
	TdxBarButton *Exit1;
	TdxBarSubItem *dfsd1;
	TdxBarButton *Outlook1;
	TdxBarButton *Word1;
	TdxBarButton *aCAYTActive1;
	TdxBarSubItem *Spelling1;
	TdxBarButton *CheckFromCursorPos1;
	TdxBarButton *CheckSelectedTextFirst1;
	TdxBarButton *IgnoreEmails1;
	TdxBarButton *IgnoreMixedCaseWords1;
	TdxBarButton *IgnoreRepeatedWords1;
	TdxBarButton *IgnoreUppercaseWords1;
	TdxBarButton *IgnoreURLs1;
	TdxBarButton *aFlat1;
	TdxBarButton *Standard1;
	TdxBarButton *UltraFlat1;
	TdxBarButton *Office111;
	TdxBarButton *NativeStyel1;
	TdxBarButton *DeveloperExpressProducts1;
	TdxBarButton *DeveloperExpressDownloads1;
	TdxBarButton *DeveloperExpressontheWeb1;
	TdxBarButton *DevExpressSupportCenter1;
	TdxBarSubItem *File1;
	TdxBarSubItem *Options1;
	TdxBarSubItem *View1;
	TdxBarSubItem *Help1;
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
	TDataSource *DataSource1;
	TdxMemData *dxMemData1;
	TBlobField *dxMemData1Photo;
	TStringField *dxMemData1TitleOfCourtesy;
	TStringField *dxMemData1FirstName;
	TStringField *dxMemData1LastName;
	TStringField *dxMemData1Title;
	TDateField *dxMemData1BirthDate;
	TMemoField *dxMemData1Notes;
	TcxPageControl *cxPageControl3;
	TcxTabSheet *cxTabSheet4;
	TcxRadioGroup *rgSpellingFormType;
	TcxGroupBox *cgSpellingOptions;
	TcxCheckBox *cxCheckBox1;
	TcxCheckBox *cxCheckBox2;
	TcxCheckBox *cxCheckBox4;
	TcxCheckBox *cxCheckBox5;
	TcxCheckBox *cxCheckBox6;
	TcxCheckBox *cxCheckBox7;
	TcxCheckBox *cxCheckBox8;
	TcxCheckBox *cxCheckBox9;
	TcxPageControl *cxPageControl4;
	TcxTabSheet *cxTabSheet5;
	TcxButton *btnCheckSpelling;
	TcxCheckBox *cxCheckBox3;
	TcxButton *cxButton1;
	TPanel *Panel2;
	TcxPageControl *cxPageControl1;
	TcxTabSheet *cxTabSheet1;
	TImage *Image2;
	TcxLabel *cxLabel16;
	TcxTextEdit *edtName;
	TcxLabel *cxLabel14;
	TcxDateEdit *deBirthDate;
	TcxLabel *cxLabel17;
	TcxTextEdit *edtObjective;
	TcxRichEdit *reAbout;
	TcxLabel *cxLabel7;
	TcxLabel *cxLabel1;
	TcxTextEdit *edtAdress;
	TcxButton *cxButton3;
	TcxButton *cxButton5;
	TcxButton *cxButton6;
	TcxLabel *cxLabel2;
	TcxMemo *memInterests;
	TcxButton *cxButton7;
	TcxTabSheet *cxTabSheet2;
	TcxGrid *cxGrid1;
	TcxGridDBTableView *cxGrid1DBTableView1;
	TcxGridDBColumn *cxGrid1DBTableView1RecId;
	TcxGridDBColumn *cxGrid1DBTableView1Photo;
	TcxGridDBColumn *cxGrid1DBTableView1CourtesyTitle;
	TcxGridDBColumn *cxGrid1DBTableView1FirstName;
	TcxGridDBColumn *cxGrid1DBTableView1LastName;
	TcxGridDBColumn *cxGrid1DBTableView1Title;
	TcxGridDBColumn *cxGrid1DBTableView1BirthDate;
	TcxGridDBColumn *cxGrid1DBTableView1Notes;
	TcxGridLevel *cxGrid1Level1;
	TcxPageControl *cxPageControl2;
	TcxTabSheet *cxTabSheet3;
	TImage *Image1;
	TImage *Image3;
	TImage *Image4;
	TcxLabel *cxLabel3;
	TcxGroupBox *cxGroupBox1;
	TcxLabel *cxLabel4;
	TImage *Image5;
        TcxGroupBox *gbAutoCorrectOptions;
        TcxCheckBox *cbActive;
    	TcxCheckBox *cbCorrectCapsLock;
    	TcxCheckBox *cbCorrectInitialCaps;
    	TcxCheckBox *cbCorrectSentenceCaps;
    	TcxCheckBox *cbDisableCapsLock;
    	TcxCheckBox *cbReplaceTextAsYouType;
    	TAction *aAutoCorrectActive;
    	TAction *aCorrectCapsLock;
    	TAction *aCorrectInitialCaps;
    	TAction *aCorrectSentenceCaps;
    	TAction *aDisableCapsLock;
    	TAction *aReplaceTextAsYouType;

	void __fastcall FormCreate(TObject *Sender);
	void __fastcall cxButton3Click(TObject *Sender);
	void __fastcall cxButton5Click(TObject *Sender);
	void __fastcall cxButton6Click(TObject *Sender);
	void __fastcall cxButton7Click(TObject *Sender);
	void __fastcall cxPageControl1Change(TObject *Sender);
	void __fastcall rgSpellingFormTypeClick(TObject *Sender);

	void __fastcall actExitExecute(TObject *Sender);
	void __fastcall actDXOnTheWebExecute(TObject *Sender);
	void __fastcall aViewExecute(TObject *Sender);

	void __fastcall aOutlookSpellTypeExecute(TObject *Sender);
	void __fastcall aCheckFromCursorPosExecute(TObject *Sender);
	void __fastcall aCheckSelectedTextFirstExecute(TObject *Sender);
	void __fastcall aIgnoreEmailsExecute(TObject *Sender);
	void __fastcall aIgnoreMixedCaseWordsExecute(TObject *Sender);
	void __fastcall aCAYTActiveExecute(TObject *Sender);
	void __fastcall aIgnoreRepeatedWordsExecute(TObject *Sender);
	void __fastcall aIgnoreUpperCaseWordsExecute(TObject *Sender);
	void __fastcall aIgnoreWordsWithNumbersExecute(TObject *Sender);
	void __fastcall aIgnoreURLsExecute(TObject *Sender);
	void __fastcall aCheckSpellingExecute(TObject *Sender);
	void __fastcall aAutoCorrectActiveExecute(TObject *Sender);
	void __fastcall aCorrectCapsLockExecute(TObject *Sender);
	void __fastcall aCorrectInitialCapsExecute(TObject *Sender);
	void __fastcall aCorrectSentenceCapsExecute(TObject *Sender);
	void __fastcall aDisableCapsLockExecute(TObject *Sender);
	void __fastcall aReplaceTextAsYouTypeExecute(TObject *Sender);
	void __fastcall aAddDictionaryExecute(TObject *Sender);
	void __fastcall dxSpellChecker1CheckAsYouTypeStart(TdxCustomSpellChecker *Sender,
          TWinControl *AControl, bool &AAllow);

public:		// User declarations
	__fastcall TfmMain(TComponent* Owner);
	void __fastcall AutoCorrectOptionsChanged(TdxSpellCheckerAutoCorrectOptions *Sender);
	void __fastcall SpellingOptionsChanged(TdxSpellCheckerSpellingOptions* Sender);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmMain *fmMain;
//---------------------------------------------------------------------------
#endif

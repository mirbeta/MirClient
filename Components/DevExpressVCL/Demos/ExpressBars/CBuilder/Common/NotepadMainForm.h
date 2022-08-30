//---------------------------------------------------------------------------

#ifndef NotepadMainFormH
#define NotepadMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxBarEditItem.hpp"
#include "cxDropDownEdit.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeels.hpp"
#include "cxPC.hpp"
#include "dxBar.hpp"
#include "dxRibbonGallery.hpp"
#include "dxSkinChooserGallery.hpp"
#include "dxTabbedMDI.hpp"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include "NotepadChildForm.h"
#include "cxClasses.hpp"
#include "dxColorDialog.hpp"
#include "dxCoreGraphics.hpp"
//---------------------------------------------------------------------------

int __fastcall Round(double const AVal);

class TRecentDocumentsController : public TObject
{
protected:
  virtual void DoLoad(TCustomIniFile *AConfig);
  virtual void DoSave(TCustomIniFile *AConfig);
public:
  virtual void Add(const String AFileName);
  virtual void SetCurrentFileName(const String AFileName);

  void LoadFromIniFile(const String AFileName);
  void SaveToIniFile(const String AFileName);
};


class TfrmNotepadMain : public TdxRibbonForm
{
__published:	// IDE-managed Components
	TcxImageList *cxLargeImages;
	TcxImageList *cxSmallImages;
	TcxLookAndFeelController *cxLookAndFeelController;
	TdxBarManager *dxBarManager;
	TdxBarButton *bbCursorLine;
	TdxBarButton *bbCursorColumn;
	TdxBarButton *bbLocked;
	TdxBarButton *bbModified;
	TcxBarEditItem *beFontName;
	TcxBarEditItem *beFontSize;
	TdxSkinChooserGalleryItem *scgiLookAndFeel;
	TdxTabbedMDIManager *dxTabbedMDIManager1;
	TActionList *alActions;
	TAction *acNew;
	TAction *acExit;
	TAction *acOpen;
	TAction *acSave;
	TAction *acSaveAs;
	TAction *acCut;
	TAction *acCopy;
	TAction *acPaste;
	TAction *acClear;
	TAction *acSelectAll;
	TAction *acPrint;
	TAction *acFont;
	TAction *acBold;
	TAction *acItalic;
	TAction *acUnderline;
	TAction *acAlignLeft;
	TAction *acAlignRight;
	TAction *acAlignCenter;
	TAction *acBullets;
	TAction *acFontColor;
	TAction *acFind;
	TAction *acReplace;
	TAction *acUndo;
	TAction *acRedo;
	TOpenDialog *OpenDialog;
	TPrintDialog *PrintDialog1;
	TFontDialog *FontDialog1;
	TdxColorDialog *ColorDialog1;
	TReplaceDialog *ReplaceDialog;
	TFindDialog *FindDialog;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall dxTabbedMDIManager1GetTabHint(TdxTabbedMDIManager *Sender, TdxTabbedMDIPage *APage,
          UnicodeString &AHint, bool &ACanShow);
	void __fastcall FindDialogFind(TObject *Sender);
	void __fastcall ReplaceDialogReplace(TObject *Sender);
	void __fastcall acAlignExecute(TObject *Sender);
	void __fastcall acBoldExecute(TObject *Sender);
	void __fastcall acBulletsExecute(TObject *Sender);
	void __fastcall acClearExecute(TObject *Sender);
	void __fastcall acCopyExecute(TObject *Sender);
	void __fastcall acCutExecute(TObject *Sender);
	void __fastcall acExitExecute(TObject *Sender);
	void __fastcall acFindExecute(TObject *Sender);
	void __fastcall acFontExecute(TObject *Sender);
	void __fastcall acFontColorExecute(TObject *Sender);
	void __fastcall acItalicExecute(TObject *Sender);
	void __fastcall acNewExecute(TObject *Sender);
	void __fastcall acOpenExecute(TObject *Sender);
	void __fastcall acPasteExecute(TObject *Sender);
	void __fastcall acPrintExecute(TObject *Sender);
	void __fastcall acRedoExecute(TObject *Sender);
	void __fastcall acReplaceExecute(TObject *Sender);
	void __fastcall acSaveExecute(TObject *Sender);
	void __fastcall acSaveAsExecute(TObject *Sender);
	void __fastcall acSelectAllExecute(TObject *Sender);
	void __fastcall acUnderlineExecute(TObject *Sender);
	void __fastcall acUndoExecute(TObject *Sender);
	void __fastcall bbLockedClick(TObject *Sender);
	void __fastcall beFontNameChange(TObject *Sender);
	void __fastcall beFontSizeChange(TObject *Sender);
	void __fastcall scgiLookAndFeelSkinChanged(TObject *Sender, const UnicodeString ASkinName);

private:	// User declarations
    TRecentDocumentsController *FRecentDocumentsController;
	TfrmNotepadChild* GetActiveChild();
	TcxRichEdit* GetEditor();
	void __fastcall EditorAddToRecentListHandler(TObject* Sender, const String AFileName);
	void __fastcall EditorChangeHandler(TObject *Sender);
	void __fastcall EditorUndoListChangeHandler(TObject *Sender);
	void __fastcall MDIStateChanged(TObject *Sender, const void *AEventArgs);
	virtual void SetSkin(TdxSkinChooserGalleryGroupItem *ASkinItem);
protected:
    Integer FUpdatingControls;
	virtual TfrmNotepadChild* CreateChildForm();
	virtual TRecentDocumentsController* CreateRecentDocumentsController();
	virtual void DoUpdateControls(TfrmNotepadChild *AActiveChild);
	virtual void InitializeLookAndFeel();
	void UpdateControls();
	void UpdateUndoRelatedControls();

public:		// User declarations
	__fastcall TfrmNotepadMain(TComponent* Owner);

    TfrmNotepadChild* CreateNewChild();
	TfrmNotepadChild* FindChild(const String AFileName);
	TfrmNotepadChild* OpenFile(const String AFileName);
	Boolean OpenFile();

	__property TfrmNotepadChild *ActiveChild = {read = GetActiveChild};
	__property TcxRichEdit *Editor = {read = GetEditor};
	__property TRecentDocumentsController *RecentDocumentsController = {read = FRecentDocumentsController};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmNotepadMain *frmNotepadMain;
//---------------------------------------------------------------------------
#endif

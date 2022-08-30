//---------------------------------------------------------------------------

#ifndef BarNotepadMainFormH
#define BarNotepadMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "NotepadMainForm.h"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
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

//---------------------------------------------------------------------------
class TBarRecentDocumentsController : public TRecentDocumentsController
{
private:
	TdxBarListItem* FBarList;
protected:
	virtual void DoLoad(TCustomIniFile* AConfig);
	virtual void DoSave(TCustomIniFile* AConfig);
public:
	__fastcall TBarRecentDocumentsController(TdxBarListItem* ABarList);
	virtual void Add(const String AFileName);
};

//---------------------------------------------------------------------------
class TfrmBarsNotepadMain : public TfrmNotepadMain
{
__published:	// IDE-managed Components
	TdxBarButton *bbAbout;
	TdxBarButton *bbAlignCenter;
	TdxBarButton *bbAlignLeft;
	TdxBarButton *bbAlignRight;
	TdxBarButton *bbBarsHelp;
	TdxBarButton *bbBold;
	TdxBarButton *bbBullets;
	TdxBarButton *bbCopy;
	TdxBarButton *bbCut;
	TdxBarButton *bbDockingHelp;
	TdxBarButton *bbDXDownloads;
	TdxBarButton *bbDXOnWeb;
	TdxBarButton *bbDXProducts;
	TdxBarButton *bbDXSupport;
	TdxBarButton *bbExit;
	TdxBarButton *bbFind;
	TdxBarButton *bbFont;
	TdxBarButton *bbFontColor;
	TdxBarButton *bbItalic;
	TdxBarButton *bbMyDX;
	TdxBarButton *bbNew;
	TdxBarButton *bbOpen;
	TdxBarButton *bbPaste;
	TdxBarButton *bbPrint;
	TdxBarButton *bbRedo;
	TdxBarButton *bbReplace;
	TdxBarButton *bbSave;
	TdxBarButton *bbSaveAs;
	TdxBarButton *bbSelectAll;
	TdxBarButton *bbTabbedView;
	TdxBarButton *bbUnderline;
	TdxBarButton *bbUndo;
	TdxBarSubItem *bsHelp;
	TdxBarSubItem *bsLookAndFeel;
	TdxBarSubItem *bsZoom;
	TdxBar *dxbFile;
	TdxBar *dxbFont;
	TdxBar *dxbFormat;
	TdxBar *dxbMain;
	TdxBar *dxbStatusBar;
	TdxBar *dxbView;
	TdxBarListItem *liRecentDocuments;
	TdxBarListItem *liZoom;
	TdxBarPopupMenu *pmEditor;
	TdxBarSubItem *siEdit;
	TdxBarSubItem *siFile;
	TdxBarSubItem *siFormat;
	TdxBarSubItem *siView;
	void __fastcall bbTabbedViewClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall liZoomClick(TObject *Sender);
	void __fastcall liRecentDocumentsClick(TObject *Sender);
private:	// User declarations
	void PopulateZoomFactors();
	void UpdateImageIndexes();
public:		// User declarations
	__fastcall TfrmBarsNotepadMain(TComponent* Owner);
	virtual TRecentDocumentsController* CreateRecentDocumentsController();
	virtual void DoUpdateControls(TfrmNotepadChild* AActiveChild);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmBarsNotepadMain *frmBarsNotepadMain;
//---------------------------------------------------------------------------
#endif

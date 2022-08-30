//---------------------------------------------------------------------------

#ifndef ImageViewerDemoMainH
#define ImageViewerDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxClasses.hpp"
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
#include "cxPC.hpp"
#include "cxSpinEdit.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxBevel.hpp"
#include "dxCheckGroupBox.hpp"
#include "dxColorEdit.hpp"
#include "dxDockControl.hpp"
#include "dxDockPanel.hpp"
#include "dxGalleryControl.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include "dxZoomTrackBar.hpp"
#include <DB.hpp>
#include <DBClient.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxImage.hpp"
#include "dxToggleSwitch.hpp"
//---------------------------------------------------------------------------
const String ActionGenre = "Action";
const String ComedyGenre = "Comedy";
const String DramaGenre = "Drama";
const String OtherGenres = "Other Genres";

//---------------------------------------------------------------------------

class TImageViewerDemoMainForm : public TForm
{
__published:	// IDE-managed Components
	TClientDataSet *cdsFilms;
	TAutoIncField *cdsFilmsID;
	TStringField *cdsFilmsCAPTION;
	TIntegerField *cdsFilmsYEAR;
	TStringField *cdsFilmsTAGLINE;
	TStringField *cdsFilmsPLOTOUTLINE;
	TIntegerField *cdsFilmsRUNTIME;
	TStringField *cdsFilmsCOLOR;
	TBlobField *cdsFilmsPHOTO;
	TBlobField *cdsFilmsICON;
	TStringField *cdsFilmsWEBSITE;
	TdxGalleryControl *dxGalleryControl;
	TcxSplitter *cxSplitter1;
	TPanel *pnlToolBar;
	TImage *imToolBar;
	TPopupMenu *pmTextSettings;
	TMenuItem *miInvisible;
	TMenuItem *miBottomSide;
	TMenuItem *miTopSide;
	TMenuItem *miLeftSide;
	TMenuItem *miRightSide;
	TPopupMenu *pmColumnSettings;
	TMenuItem *miAutoColumnCount;
	TMenuItem *miLine;
	TMenuItem *miThreeColumns;
	TMenuItem *miFourColumns;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TcxButton *sbTextSettings;
	TcxButton *sbColumnSettings;
	TcxTrackBar *tbItemSize;
	TcxLabel *lbItemSize;
	TcxCheckBox *cbSorted;
	TcxButton *sbInfo;
	TcxGroupBox *gbRightPanel;
	TMenuItem *miFiveColumns;
	TClientDataSet *cdsGenres;
	TClientDataSet *cdsFilmsGenres;
	TAutoIncField *cdsGenresID;
	TStringField *cdsGenresNAME;
	TAutoIncField *cdsFilmsGenresID;
	TIntegerField *cdsFilmsGenresFILMID;
	TIntegerField *cdsFilmsGenresGENREID;
	TBlobField *cdsFilmsGenresPHOTO;
	TBlobField *cdsFilmsGenresICON;
	TcxButton *sbResize;
	TcxImage *imgMain;
	TdxToggleSwitch *tsHidePanel;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall cbSortedClick(TObject *Sender);
	void __fastcall dxGalleryControlItemClick(TObject *Sender, TdxGalleryControlItem *AItem);
	void __fastcall miInvisibleClick(TObject *Sender);
	void __fastcall miBottomSideClick(TObject *Sender);
	void __fastcall miTopSideClick(TObject *Sender);
	void __fastcall miLeftSideClick(TObject *Sender);
	void __fastcall miRightSideClick(TObject *Sender);
	void __fastcall sbTextSettingsClick(TObject *Sender);
	void __fastcall miAutoColumnCountClick(TObject *Sender);
	void __fastcall miThreeColumnsClick(TObject *Sender);
	void __fastcall miFourColumnsClick(TObject *Sender);
	void __fastcall sbColumnSettingsClick(TObject *Sender);
	void __fastcall tbItemSizePropertiesChange(TObject *Sender);
	void __fastcall sbInfoClick(TObject *Sender);
	void __fastcall miFiveColumnsClick(TObject *Sender);
	void __fastcall sbResizeClick(TObject *Sender);
	void __fastcall tsHidePanelPropertiesChange(TObject *Sender);
private:	// User declarations
	TdxGalleryControlItem *FCurrentItem;
	TcxBitmap *FGlyph;
	Integer FStoredPanelWidth;
	TcxPosition FStoredPosition;
	void GetGenresIDs(Integer &AActionID, Integer &AComedyID, Integer &ADramaID);
	int FilmGenreGroup(Integer AFilmID, Integer AActionID, Integer AComedyID, Integer ADramaID);
	void SetupTextMenu(TcxPosition APosition);
	void Draw();
	void PopulateGallery(bool ASorted);
	void ResizeGlyph(Single AScale, Integer AWidth, Integer AHeight, Boolean IsInPixels);
	void SetDefaultItem();
	void SetTextPosition(TcxPosition APosition);
public:		// User declarations
	__fastcall TImageViewerDemoMainForm(TComponent* Owner);
	__fastcall ~TImageViewerDemoMainForm();
};


//---------------------------------------------------------------------------
extern PACKAGE TImageViewerDemoMainForm *ImageViewerDemoMainForm;
//---------------------------------------------------------------------------
#endif

// ---------------------------------------------------------------------------

#ifndef RibbonNotepadMainFormH
#define RibbonNotepadMainFormH
// ---------------------------------------------------------------------------
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
#include "dxRibbon.hpp"
#include "dxRibbonForm.hpp"
#include "dxSkinChooserGallery.hpp"
#include "dxTabbedMDI.hpp"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include "NotepadChildForm.h"
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <Menus.hpp>
#include <ExtDlgs.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMemo.hpp"
#include "cxScrollBox.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxBevel.hpp"
#include "dxGallery.hpp"
#include "dxGalleryControl.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxRibbonBackstageView.hpp"
#include "dxRibbonBackstageViewGalleryControl.hpp"
#include "dxRibbonSkins.hpp"
#include "dxRibbonStatusBar.hpp"
#include "dxStatusBar.hpp"
#include "dxZoomTrackBar.hpp"
#include "RibbonNotepadDemoGallerySetup.h"
#include "dxBarApplicationMenu.hpp"
#include "dxBarExtItems.hpp"
#include "dxRibbonMiniToolbar.hpp"
#include "dxRibbonRadialMenu.hpp"
#include "dxScreenTip.hpp"
#include "RibbonNotepadDemoOptions.h"
#include "RibbonNotepadChildForm.h"
#include "EBarsUtils.h"
#include "dxColorDialog.hpp"
#include "dxCoreGraphics.hpp"
#include "cxMaskEdit.hpp"
#include "dxRibbonColorGallery.hpp"
#include "dxOfficeSearchBox.hpp"
#include "dxCore.hpp"
#include "cxImageList.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "dxSkinsdxRibbonPainter.hpp"
// ---------------------------------------------------------------------------

const
  int SchemeColorCount = 10;

struct TColorMapInfo
{
  char *Name;
  TColor Map[SchemeColorCount];
};

enum TAccent {aLight80, aLight60, aLight50, aLight40, aLight35, aLight25, aLight15, aLight5, aDark10, aDark25, aDark50, aDark75, aDark90};

class TColorPickerController : public TObject {
private:
	TdxRibbonColorGalleryItem* FColorGallery;
	int FColorGlyphSize;
	TdxRibbonGalleryItem *FColorMapItem;

	void FillGlyph(TcxAlphaBitmap *AGlyph);
	void __fastcall ColorMapItemClick(TdxRibbonGalleryItem *Sender,
		TdxRibbonGalleryGroupItem *AItem);

protected:
	void BuildColorSchemeGallery();
	void ColorMapChanged();
public:
	__fastcall TColorPickerController(TdxRibbonGalleryItem *AColorMapItem,
		TdxRibbonColorGalleryItem* AColorGallery, TdxCustomRibbon *ARibbon);
	TcxAlphaBitmap* CreateColorBitmap(TColor AColor, int AGlyphSize);
};

class TdxRibbonRecentDocumentsController : public TRecentDocumentsController {
private:
	TdxRibbonBackstageViewGalleryControl* FCurrentFolder;
	TdxRibbonBackstageViewGalleryControl* FRecentDocuments;
	TdxRibbonBackstageViewGalleryControl* FRecentPaths;
	TdxBarApplicationMenu* FApplicationMenu;
	
	TdxRibbonBackstageViewGalleryItem* GetItemByValue
		(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue);
	TdxRibbonBackstageViewGalleryItem* InternalAdd
		(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue);
	void InternalLoad(TdxRibbonBackstageViewGalleryGroup* AGroup,
		TCustomIniFile* AIniFile, const String ASection);
	void InternalSave(TdxRibbonBackstageViewGalleryGroup* AGroup,
		TCustomIniFile* AIniFile, const String ASection);
	void UpdateApplicationMenu();
protected:
	virtual void DoLoad(TCustomIniFile* AConfig);
	virtual void DoSave(TCustomIniFile* AConfig);

public:
	TdxRibbonRecentDocumentsController
		(TdxRibbonBackstageViewGalleryControl* ARecentPaths,
		TdxRibbonBackstageViewGalleryControl* ACurrentFolder,
		TdxRibbonBackstageViewGalleryControl* ARecentDocuments,
		TdxBarApplicationMenu* AApplicationMenu);
	virtual void Add(const String AFileName);
	virtual void SetCurrentFileName(const String AFileName);
};

class TfrmRibbonNotepadMain : public TfrmNotepadMain {
__published: // IDE-managed Components
	TdxBarApplicationMenu* ApplicationMenu;
	TdxRibbonBackstageView* BackstageView;
	TdxBarLargeButton* bbAlignCenter;
	TdxBarLargeButton* bbAlignLeft;
	TdxBarLargeButton* bbAlignRight;
	TdxBarLargeButton* bbApplicationButton;
	TdxBarLargeButton* bbBarsHelp;
	TdxBarLargeButton* bbBold;
	TdxBarLargeButton* bbBullets;
	TdxBarLargeButton* bbClear;
	TdxBarLargeButton* bbCopy;
	TdxBarLargeButton* bbCut;
	TdxBarLargeButton* bbDockingHelp;
	TdxBarLargeButton* bbDXDownloads;
	TdxBarLargeButton* bbDXOnWeb;
	TdxBarLargeButton* bbDXProducts;
	TdxBarLargeButton* bbDXSupport;
	TdxBarButton* bbExit;
	TdxBarLargeButton* bbFind;
	TdxBarButton* bbFont;
	TdxRibbonColorGalleryItem* cgiFontColor;
	TdxBarLargeButton* bbItalic;
	TdxBarButton *bbLogo;
	TdxBarLargeButton* bbMyDX;
	TdxBarLargeButton* bbNew;
	TdxBarLargeButton* bbOpen;
	TdxBarButton* bbOptions;
	TdxBarLargeButton* bbPaste;
	TdxBarLargeButton* bbPrint;
	TdxBarButton* bbQATAboveRibbon;
	TdxBarButton* bbQATBelowRibbon;
	TdxBarLargeButton* bbQATVisible;
	TdxBarLargeButton* bbReplace;
	TdxBarLargeButton* bbRibbonForm;
	TdxBarLargeButton* bbSave;
	TdxBarLargeButton* bbSaveAsRTF;
	TdxBarLargeButton* bbSaveAsText;
	TdxBarLargeButton* bbSelectAll;
	TdxBarLargeButton* bbTouchMode;
	TdxBarLargeButton* bbUnderline;
	TdxBarLargeButton* bbUndo;
	TdxBarControlContainerItem* bccZoom;
	TdxBarSubItem* bsSaveAs;
	TdxBarStatic* bsSelectionInfo;
	TcxButton* btnBrowsePath;
	TdxRibbonBackstageViewGalleryControl* bvgcCurrentFolder;
	TdxRibbonBackstageViewGalleryControl* bvgcLocations;
	TdxRibbonBackstageViewGalleryItem* bvgcLocationsComputerItem;
	TdxRibbonBackstageViewGalleryGroup* bvgcLocationsGroup1;
	TdxRibbonBackstageViewGalleryGroup* bvgcLocationsRecentDocumentsGroup;
	TdxRibbonBackstageViewGalleryItem* bvgcLocationsRecentDocumentsItem;
	TdxRibbonBackstageViewGalleryControl* bvgcRecentDocuments;
	TdxRibbonBackstageViewGalleryControl* bvgcRecentPaths;
	TdxRibbonBackstageViewGalleryGroup* bvgcRecentPathsGroup;
	TdxRibbonBackstageViewTabSheet* bvtsAbout;
	TdxRibbonBackstageViewTabSheet* bvtsHelp;
	TdxRibbonBackstageViewTabSheet* bvtsOpen;
	TdxRibbonBackstageViewTabSheet* bvtsSaveAs;
	TcxLabel* cxLabel2;
	TdxBarLargeButton* dxBarLargeButton1;
	TdxBarLargeButton* dxBarLargeButton3;
	TdxRibbonPopupMenu* dxBarPopupMenu;
	TdxBarScreenTipRepository* dxBarScreenTipRepository1;
	TdxBarSeparator* bsepSaveAs;
	TdxBar* dxbClipboard;
	TdxBar* dxbColorScheme;
	TdxBar* dxbEditing;
	TdxBevel* dxBevel1;
	TdxBar* dxbFile;
	TdxBar* dxbFontAndColors;
	TdxBar* dxbHelp;
	TdxBar* dxbLinks;
	TdxBar* dxbParagraph;
	TdxBar* dxbQAT;
	TdxBar* dxbQATOptions;
	TdxBar* dxbRibbonOptions;
	TdxBar* dxbSelectionTools;
	TdxBar* dxbStatusBarToolbar1;
	TdxBar* dxbStatusBarToolbar2;
	TdxBar* dxbStatusBarToolbar3;
	TdxRibbonGalleryGroup* dxRibbonGalleryItem1Group1;
	TdxRibbonGalleryGroup* dxRibbonGalleryItem1Group2;
	TdxRibbonGalleryGroup* dxRibbonGalleryItem1Group3;
	TdxRibbonGalleryGroup* dxRibbonGalleryItem1Group4;
	TdxRibbonStatusBar* dxStatusBar;
	TdxBar* dxbTabAreaToolbar;
	TcxGroupBox* gbBackstageViewTabCaption;
	TcxGroupBox* gbHelpContent;
	TcxGroupBox* gbLocationsMain;
	TcxGroupBox* gbLocationsPane;
	TcxScrollBox* gbRecentDocumentsPane;
	TcxScrollBox* gbRecentPathsPane;
	TcxGroupBox* gbRecentPathsPaneBottom;
	TcxGroupBox* gbRecentPathsPaneCurrentFolder;
	TcxImageList* ilLargeColorSchemesGlyphs;
	TcxImageList* ilSmallColorSchemesGlyphs;
	TImage* Image1;
	TImage* imDot1;
	TImage* imDot2;
	TImage* imDot3;
	TImage* imDot4;
	TImage* imDot5;
	TImage* imDot6;
	TImage* imLogo;
	TcxLabel* lbbvTabCaption2010;
	TcxLabel* lbbvTabCaption2013;
	TcxLabel* lbComputer;
	TcxLabel* lbCurrentFolder;
	TcxLabel* lblClientCenter;
	TcxLabel* lblDownloads;
	TcxLabel* lblDXonWeb;
	TcxLabel* lblHelpBars;
	TcxLabel* lblHelpDocking;
	TcxLabel* lblProducts;
	TcxLabel* lblSupport;
	TcxLabel* lblSupportCenter;
	TcxLabel* lbRecentDocuments;
	TcxLabel* lbRecentFolders;
	TcxMemo* meAbout;
	TdxRibbonMiniToolbar* MiniToolbar;
	TdxRibbonGalleryItem* rgiColorTheme;
	TdxRibbonGalleryItem* rgiFontColor;
	TdxRibbonGalleryItem* rgiItemSymbol;
	TdxRibbonGalleryItem* rgiUndo;
	TdxRibbon* Ribbon;
	TdxRibbonRadialMenu* RibbonRadialMenu;
	TdxRibbonTab* rtAppearance;
	TdxRibbonTab* rtHelp;
	TdxRibbonTab* rtSelection;
	TdxBarScreenTip* stAlignCenter;
	TdxBarScreenTip* stAlignLeft;
	TdxBarScreenTip* stAlignRight;
	TdxBarScreenTip* stAppButton;
	TdxBarScreenTip* stAppMenu;
	TdxBarScreenTip* stBlack;
	TdxBarScreenTip* stBlue;
	TdxBarScreenTip* stBold;
	TdxBarScreenTip* stBullets;
	TdxBarScreenTip* stCopy;
	TdxBarScreenTip* stCut;
	TdxBarScreenTip* stFind;
	TdxBarScreenTip* stFontDialog;
	TdxBarScreenTip* stHelpButton;
	TdxBarScreenTip* stItalic;
	TdxBarScreenTip* stNew;
	TdxBarScreenTip* stOpen;
	TdxBarScreenTip* stPaste;
	TdxBarScreenTip* stPrint;
	TdxBarScreenTip* stQAT;
	TdxBarScreenTip* stQATAbove;
	TdxBarScreenTip* stQATBelow;
	TdxBarScreenTip* stReplace;
	TdxBarScreenTip* stRibbonForm;
	TdxBarScreenTip* stSilver;
	TdxBarScreenTip* stUnderline;
	TdxRibbonTab* tabHome;
	TdxZoomTrackBar* tbZoom;
	TdxRibbonDropDownGallery* UndoDropDownGallery;
	TdxBarStatic* bsZoom;
	TdxBarSubItem* dxBarSubItem1;
	TdxBarButton* dxBarButton1;
	TdxBarButton* dxBarButton2;
	TdxBarButton* bbUndoAll;
	TdxBarSubItem* dxBarSubItem2;
	TdxBar* dxbContextMenuStyle;
	TdxBarLargeButton* bbRadialMenu;
	TdxBarLargeButton* bbMiniToolbar;
    TAction* acQATBelowRibbon;
    TAction* acQATAboveRibbon;
	TdxRibbonBackstageViewTabSheet* bvtsOptions;
	TcxGroupBox* gbOptionsPane;
	TcxGroupBox* gbColorSchemeAccent;
	TcxLabel* lbColorSchemeAccent;
	TcxGroupBox* gbRibbonStyle;
	TcxLabel* lbRibbonStyle;
	TcxGroupBox* gbColorScheme;
	TcxLabel* lbColorScheme;
    TcxComboBox* cbRibbonStyle;
	TcxComboBox* cbColorScheme;
	TcxComboBox* cbColorSchemeAccent;
	TcxGroupBox* gbScreenTipStyle;
	TcxLabel* lbScreenTipStyle;
	TcxComboBox* cbScreenTipStyle;
	TcxGroupBox* gbRibbonBackgroundImagePane;
	TOpenPictureDialog* OpenPictureDialog;
	TcxGroupBox* gbUserInterfaceOptions;
	TcxGroupBox* gbPersonalizationOptions;
	TcxLabel* lbUserInterfaceOptions;
	TcxLabel* lbPersonalizationOptions;
	TcxButton* btnSpecifyImage;
	TdxRibbonPopupMenu* ppmRibbonBackgroundImage;
	TdxBarLargeButton* bbLoadImage;
	TdxBarLargeButton* bbClearImage;
	TcxGroupBox* gbRibbonBackgroundImageInfo;
	TcxLabel* lbRibbonBackgroundImageCaption;
    TcxLabel* lbRibbonBackgroundImageDescription;
	TdxBarSubItem* bsSave;
	TdxBar* dxbTabAreaSearchToolbar;
	TcxBarEditItem* beOfficeSearchBox;
    TdxBar* dxbSearchOptions;
    TdxBarLargeButton* bbRecursiveSearch;
    TdxBarLargeButton* bbShowPaths;
	void __fastcall BackstageViewPopup(TObject *Sender);
	void __fastcall BackstageViewTabChanged(TObject *Sender);
	void __fastcall bbApplicationButtonClick(TObject *Sender);
	void __fastcall cgiFontColorColorChanged(TObject *Sender);
	void __fastcall bbOptionsClick(TObject *Sender);
	void __fastcall bbQATVisibleClick(TObject *Sender);
	void __fastcall bbRecursiveSearchClick(TObject *Sender);
	void __fastcall bbRibbonFormClick(TObject *Sender);
	void __fastcall bbSaveAsRTFClick(TObject *Sender);
	void __fastcall bbSaveAsTextClick(TObject *Sender);
	void __fastcall bbShowPathsClick(TObject *Sender);
	void __fastcall bbTouchModeClick(TObject *Sender);
	void __fastcall btnBrowsePathClick(TObject *Sender);
	void __fastcall bvgcLocationsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall bvgcRecentDocumentsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall bvgcRecentPathsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall dxbFontAndColorsCaptionButtons0Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall rgiItemSymbolGroupItemClick(TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem);
	void __fastcall rgiUndoGroupItemClick(TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem);
	void __fastcall rgiUndoHotTrackedItemChanged(TdxRibbonGalleryGroupItem *APrevHotTrackedGroupItem,
          TdxRibbonGalleryGroupItem *ANewHotTrackedGroupItem);
	void __fastcall scgiLookAndFeelPopulate(TObject *Sender);
	void __fastcall scgiLookAndFeelSelected(TObject *Sender, const UnicodeString ASkinName);
	void __fastcall tbZoomPropertiesChange(TObject *Sender);
	void __fastcall UndoDropDownGalleryPopup(TObject *Sender);
	void __fastcall bbUndoAllClick(TObject *Sender);
    void __fastcall acQATBelowRibbonExecute(TObject *Sender);
	void __fastcall acQATAboveRibbonUpdate(TObject *Sender);
	void __fastcall cbColorSchemeAccentPropertiesChange(TObject *Sender);
	void __fastcall cbColorSchemePropertiesChange(TObject *Sender);
	void __fastcall cbRibbonStylePropertiesChange(TObject *Sender);
	void __fastcall cbScreenTipStylePropertiesChange(TObject *Sender);
	void __fastcall bbLoadImageClick(TObject *Sender);
	void __fastcall bbClearImageClick(TObject *Sender);
	void __fastcall ApplicationMenuExtraPaneItemClick(TObject* Sender, int AIndex);

private: // User declarations
	TColorPickerController* FColorPickerController;

	TRibbonDemoStyle GetRibbonDemoStyle();
	void SetColorScheme(const String AName);
	void SetRibbonDemoStyle(TRibbonDemoStyle AStyle);

	void AddItem(TdxRibbonGalleryGroup* AGroup, Integer ACode);
	TcxAlphaBitmap* CreateBitmap(const String AFont, WideChar AChar);
	void PopulateGroup(Integer AGroupIndex, Integer AMap[]);

	void AssignFontColorGlyph();
#if (__BORLANDC__ >= 0x0660)  // C++Builder XE4
	void _fastcall EditorContextPopup(System::TObject* Sender, const Types::TPoint &MousePos, bool &Handled);
#else
#if (__BORLANDC__ >= 0x0650)  // C++Builder XE3
	#ifndef _WIN64
	void _fastcall EditorContextPopup(System::TObject* Sender, System::Types::TPoint &MousePos, bool &Handled);
	#else
	void _fastcall EditorContextPopup(System::TObject* Sender, System::Types::TPoint MousePos, bool &Handled);
	#endif
#else
	void _fastcall EditorContextPopup(System::TObject* Sender, const Types::TPoint &MousePos, bool &Handled);
#endif
#endif
	void __fastcall EditorMouseUp(System::TObject* Sender, TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FontColorChanged(TObject* Sender);
	void InitOptions();
	void InitSymbolGallery();
	void UpdateColorSchemeRelatedControls();
	void UpdateImageIndexes();
	void UpdateOptionsVisibility();

	Boolean GetGlyphIndex(const String ASkinName, Integer &AIndex);
	void SetGlyph(TdxSmartGlyph* ABitmap, TcxImageList *AImageList, Integer AGlyphIndex);
protected:
	virtual TfrmNotepadChild* CreateChildForm();
	virtual TRecentDocumentsController* CreateRecentDocumentsController();
	virtual void DoUpdateControls(TfrmNotepadChild* AActiveChild);
	virtual void InitializeLookAndFeel();
	virtual void UpdateUndoRelatedControls();
public: // User declarations
	__fastcall TfrmRibbonNotepadMain(TComponent* Owner);
    __property TRibbonDemoStyle RibbonDemoStyle = {read = GetRibbonDemoStyle, write = SetRibbonDemoStyle};
};

// ---------------------------------------------------------------------------
extern PACKAGE TfrmRibbonNotepadMain *frmRibbonNotepadMain;
// ---------------------------------------------------------------------------
#endif

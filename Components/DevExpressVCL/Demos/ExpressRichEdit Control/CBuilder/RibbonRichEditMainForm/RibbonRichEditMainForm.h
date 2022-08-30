// ---------------------------------------------------------------------------
#ifndef RibbonRichEditMainFormH
#define RibbonRichEditMainFormH
// ---------------------------------------------------------------------------
  #include "Windows.hpp"
  #include "Forms.hpp"
  #include "RichEditControlBase.h"
  #include "IniFiles.hpp"
  #include "cxGraphics.hpp"
  #include "cxControls.hpp"
  #include "cxLookAndFeels.hpp"
  #include "cxLookAndFeelPainters.hpp"
  #include "cxContainer.hpp"
  #include "cxEdit.hpp"
  #include "dxRibbonCustomizationForm.hpp"
  #include "dxRibbonSkins.hpp"
  #include "Menus.hpp"
  #include "dxCore.hpp"
  #include "dxCoreClasses.hpp"
  #include "dxGDIPlusAPI.hpp"
  #include "dxGDIPlusClasses.hpp"
  #include "dxRichEdit.Types.hpp"
  #include "dxRichEdit.Options.hpp"
  #include "dxRichEdit.Control.hpp"
  #include "dxRichEdit.OpenXML.hpp"
  #include "dxRichEdit.HTML.hpp"
  #include "dxRichEdit.DOC.hpp"
  #include "cxFontNameComboBox.hpp"
  #include "cxDropDownEdit.hpp"
  #include "dxRibbon.hpp"
  #include "dxBar.hpp"
  #include "dxBarApplicationMenu.hpp"
  #include "dxScreenTip.hpp"
  #include "Dialogs.hpp"
  #include "dxRichEdit.Actions.hpp"
  #include "dxActions.hpp"
  #include "Classes.hpp"
  #include "ActnList.hpp"
  #include "dxBarExtItems.hpp"
  #include "dxRibbonGallery.hpp"
  #include "dxSkinChooserGallery.hpp"
  #include "cxBarEditItem.hpp"
  #include "ImgList.hpp"
  #include "Controls.hpp"
  #include "dxRichEdit.Platform.Win.Control.hpp"
  #include "Graphics.hpp"
  #include "ExtCtrls.hpp"
  #include "cxTextEdit.hpp"
  #include "cxMemo.hpp"
  #include "StdCtrls.hpp"
  #include "cxButtons.hpp"
  #include "cxScrollBox.hpp"
  #include "dxGallery.hpp"
  #include "dxGalleryControl.hpp"
  #include "dxRibbonBackstageViewGalleryControl.hpp"
  #include "dxBevel.hpp"
  #include "cxLabel.hpp"
  #include "cxGroupBox.hpp"
  #include "dxBarBuiltInMenu.hpp"
  #include "dxRibbonBackstageView.hpp"
  #include "dxStatusBar.hpp"
  #include "dxRibbonStatusBar.hpp"
  #include "cxClasses.hpp"
  #include "cxTrackBar.hpp"
  #include "dxZoomTrackBar.hpp"
  #include "dxHttpIndyRequest.hpp"
  #include "dxPSEngn.hpp"
  #include "dxPSGlbl.hpp"
  #include "dxPSUtl.hpp"
  #include "dxBkgnd.hpp"
  #include "dxWrap.hpp"
  #include "dxPrnDev.hpp"
  #include "dxPSCompsProvider.hpp"
  #include "dxPSFillPatterns.hpp"
  #include "dxPSEdgePatterns.hpp"
  #include "dxPSPDFExportCore.hpp"
  #include "dxPSPDFExport.hpp"
  #include "cxDrawTextUtils.hpp"
  #include "dxPSPrVwStd.hpp"
  #include "dxPSPrVwAdv.hpp"
  #include "dxPSPrVwRibbon.hpp"
  #include "dxPScxPageControlProducer.hpp"
  #include "dxPScxEditorProducers.hpp"
  #include "dxPScxExtEditorProducers.hpp"
  #include "dxPSCore.hpp"
  #include "dxPSRichEditControlLnk.hpp"
  #include "dxColorDialog.hpp"
  #include "SysUtils.hpp"
  #include "dxCoreGraphics.hpp"
  #include "cxGeometry.hpp"
  #include "RibbonRichEditDemoGallerySetup.h"
  #include "dxPrnPg.hpp"
  #include "dxRibbonColorGallery.hpp"
  #include "cxImageComboBox.hpp"
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
	TColor FDefaultColor;
	TColor FColor;
	int FColorGlyphSize;
	TdxColorDialog* FColorDialog;
	TdxRibbonGalleryItem *FColorItem;
	TdxRibbonGalleryItem *FColorMapItem;
	TdxCustomRibbon *FRibbon;

	TdxRibbonGalleryGroup *FThemeColorsGroup;
	TdxRibbonGalleryGroup *FAccentColorsGroup;
	TdxRibbonGalleryGroup *FStandardColorsGroup;
	TdxRibbonGalleryGroup *FCustomColorsGroup;

	TdxBarButton *FMoreColorsButton;
	TdxBarButton *FNoColorButton;
	TdxBarButton *FColorDialogSetup;
	TNotifyEvent FOnColorChanged;

	TdxBarManager* GetBarManager();

	void InitColorItem();
	void InitColorMapItem();
	void InitDropDownGallery(TdxBarItemLinks *AItemLinks, UnicodeString ADefaultColorCaption);
	void PopulateGalleries();
	void SelectDefaultColor();

	void FillGlyph(TcxAlphaBitmap *AGlyph);

	void SetColor(TColor Value);

	void __fastcall ColorItemClick(TdxRibbonGalleryItem *Sender,
		TdxRibbonGalleryGroupItem *AItem);
	void __fastcall ColorMapItemClick(TdxRibbonGalleryItem *Sender,
		TdxRibbonGalleryGroupItem *AItem);
	void __fastcall NoColorButtonClick(TObject *Sender);
	void __fastcall MoreColorsClick(TObject *Sender);
	void __fastcall ColorDialogSetupButtonClick(TObject *Sender);

	__property TdxBarManager* BarManager = {
		read = GetBarManager
	};

protected:
	TdxRibbonGalleryGroupItem* AddColorItem
		(TdxRibbonGalleryGroup *AGalleryGroup, TColor AColor);
	TcxAlphaBitmap* CreateColorBitmap(TColor AColor, int AGlyphSize);
	void CreateColorRow(TdxRibbonGalleryGroup *AGalleryGroup,
		const TColor *AColorMap);
	void BuildThemeColorGallery();
	void BuildStandardColorGallery();
	void BuildColorSchemeGallery();

	void ColorChanged();
	void ColorMapChanged();

public:
	__fastcall TColorPickerController(TdxRibbonGalleryItem *AColorItem,
		TdxRibbonGalleryItem *AColorMapItem,
		TdxBarItemLinks *AItemLinks, TdxCustomRibbon *ARibbon, TColor ADefaultColor, UnicodeString ADefaultColorCaption);
	__fastcall ~TColorPickerController();

	__property TColor Color = {
		read = FColor
	};
	__property TColor DefaultColor = {
		read = FColor
	};
	__property TNotifyEvent OnColorChanged = {
		read = FOnColorChanged, write = FOnColorChanged
	};
};

class TfrmRibbonRichEditMain : public TfrmRichEditControlBase
{
__published: // IDE-managed Components
	TdxBar* dxbQAT;
	TdxBar* bmbHomeClipboard;
	TdxBar* bmbHomeEditing;
	TdxBar* bmbHomeParagraph;
	TdxBar* bmbHomeFont;
	TdxBar* dxbStatusBarToolbar1;
	TdxBar* dxbStatusBarToolbar2;
	TdxBar* dxbStatusBarToolbar3;
	TdxBar* dxbHelp;
	TdxBar* dxbLinks;
	TdxBar* bmbFileCommon;
	TdxBar* bmbInsertPages;
	TdxBar* bmbInsertTables;
	TdxBar* bmbInserIllustrations;
	TdxBar* bmbInsertLinks;
	TdxBar* bmbInsertHeaderAndFooter;
	TdxBar* bmbInsertText;
	TdxBar* bmbInsertSymbols;
	TdxBar* bmbPageLayoutPageSetup;
	TdxBar* bmbPageLayoutPageBackground;
	TdxBar* bmbReferencesTableOfContents;
	TdxBar* bmbReferencesCaptions;
	TdxBar* bmbMailingsMailMerge;
	TdxBar* bmbReviewProofing;
	TdxBar* bmbReviewProtect;
	TdxBar* bmbViewDocumentViews;
	TdxBar* bmbViewShow;
	TdxBar* bmbViewZoom;
	TdxBar* bmbHFTNavigation;
	TdxBar* bmbHFTOptions;
	TdxBar* bmbHFTClose;
	TdxBar* bmbTableToolsTable;
	TdxBar* bmbTableToolsRowsAndColumns;
	TdxBar* bmbTableToolsMerge;
	TdxBar* bmbTableToolsCellSize;
	TdxBar* bmbTableToolsTableStyleOptions;
	TdxBar* bmbTableToolsTableStyles;
	TdxBar* bmbTableToolsBordersShadings;
	TdxBar* bmbPictureToolsShapeStyles;
	TdxBar* bmbPictureToolsArrange;
	TdxBar* bmbTableToolsAlignment;
	TdxBarButton* bbCursorLine;
	TdxBarButton* bbCursorColumn;
	TdxBarButton* bbLocked;
	TdxBarButton* bbModified;
	TcxBarEditItem* beFontName;
	TcxBarEditItem* beFontSize;
	TdxBarLargeButton* bbNew;
	TdxBarLargeButton* bbOpen;
	TdxBarLargeButton* bbSave;
	TdxBarLargeButton* bbPrint;
	TdxBarLargeButton* bbPaste;
	TdxBarLargeButton* bbCut;
	TdxBarLargeButton* bbCopy;
	TdxBarLargeButton* bbSelectAll;
	TdxBarLargeButton* bbFind;
	TdxBarLargeButton* bbReplace;
	TdxBarLargeButton* bbUndo;
	TdxBarLargeButton* bbBold;
	TdxBarLargeButton* bbItalic;
	TdxBarLargeButton* bbUnderline;
	TdxBarLargeButton* bbAlignLeft;
	TdxBarLargeButton* bbAlignCenter;
	TdxBarLargeButton* bbAlignRight;
	TdxBarLargeButton* bbBullets;
	TdxRibbonGalleryItem* rgiColorTheme;
	TdxRibbonGalleryItem* rgiPageColorTheme;
	TdxRibbonGalleryItem* rgiFontColor;
	TdxRibbonGalleryItem* rgiPageColor;
	TdxBarControlContainerItem* bccZoom;
	TdxBarButton* bbOptions;
	TdxBarButton* bbExit;
	TdxBarLargeButton* bbBarsHelp;
	TdxBarLargeButton* bbDockingHelp;
	TdxBarLargeButton* bbDXOnWeb;
	TdxBarLargeButton* bbDXSupport;
	TdxBarLargeButton* bbDXProducts;
	TdxBarLargeButton* bbDXDownloads;
	TdxBarLargeButton* bbMyDX;
	TdxBarStatic* bsZoom;
	TdxBarLargeButton* bbFontSuperscript;
	TdxBarLargeButton* bbFontSubscript;
	TdxBarLargeButton* bbIncreaseFontSize;
	TdxBarLargeButton* bbDecreaseFontSize;
	TdxBarSubItem* bsiLineSpacing;
	TdxBarLargeButton* bbSingleLineSpacing;
	TdxBarLargeButton* bbSesquialteralLineSpacing;
	TdxBarLargeButton* bbDoubleLineSpacing;
	TdxBarLargeButton* bbLineSpacingOptions;
	TdxBarLargeButton* bbAlignJustify;
	TdxBarLargeButton* bbRedo;
	TdxBarLargeButton* bbNumbering;
	TdxBarLargeButton* bbMultiLevelList;
	TdxBarLargeButton* bbDoubleUnderline;
	TdxBarLargeButton* bbStrikeout;
	TdxBarLargeButton* bbDoubleStrikeout;
	TdxBarLargeButton* bbShowWhitespace;
	TdxBarLargeButton* bbDecrementIndent;
	TdxBarLargeButton* bbIncrementIndent;
	TdxBarButton* bbParagraph;
	TdxBarSubItem* bbRadialMenuAlligns;
	TdxBarLargeButton* bbSaveAs;
	TdxBarLargeButton* bbTableProperties;
	TdxBarLargeButton* bbSymbol;
	TdxBarLargeButton* bbInsertTable;
	TdxBarLargeButton* bbHorizontalRuler;
	TdxBarLargeButton* bbVerticalRuler;
	TdxBarLargeButton* bbZoomOut;
	TdxBarLargeButton* bbZoomIn;
	TdxBarSubItem* bsiBorders;
	TdxBarLargeButton* bbBottomBorder;
	TdxBarLargeButton* bbTopBorder;
	TdxBarLargeButton* bbLeftBorder;
	TdxBarLargeButton* bbRightBorder;
	TdxBarLargeButton* bbNoBorder;
	TdxBarLargeButton* bbAllBorder;
	TdxBarLargeButton* bbOutsideBorder;
	TdxBarLargeButton* bbInsideBorders;
	TdxBarLargeButton* bbInsideHorizontalBorder;
	TdxBarLargeButton* bbInsideVerticalBorder;
	TdxBarLargeButton* bbCellsAlignTopLeft;
	TdxBarLargeButton* bbCellsAlignCenterLeft;
	TdxBarLargeButton* bbCellsAlignBottomLeft;
	TdxBarLargeButton* bbCellsAlignTopCenter;
	TdxBarLargeButton* bbCellsAlignCenter;
	TdxBarLargeButton* bbCellsBottomCenterAlign;
	TdxBarLargeButton* bbCellsTopRightAlign;
	TdxBarLargeButton* bbCellsCenterRightAlign;
	TdxBarLargeButton* bbBottomRightAlign;
	TdxBarLargeButton* bbSplitCells;
	TdxBarLargeButton* bbInsertRowAbove;
	TdxBarLargeButton* bbInsertRowBelow;
	TdxBarLargeButton* bbInsertColumnToTheLeft;
	TdxBarLargeButton* bbInsertColumnToTheRight;
	TdxBarSubItem* bsiDelete;
	TdxBarLargeButton* bbDeleteCells;
	TdxBarLargeButton* bbHyperlink;
	TAction* acExit;
	TdxRichEditControlCutSelection* acCut;
	TdxRichEditControlCopySelection* acCopy;
	TdxRichEditControlPasteSelection* acPaste;
	TdxRichEditControlSelectAll* acSelectAll;
	TdxRichEditControlToggleFontBold* acBold;
	TdxRichEditControlToggleFontItalic* acItalic;
	TdxRichEditControlToggleFontUnderline* acUnderline;
	TdxRichEditControlToggleParagraphAlignmentLeft* acAlignLeft;
	TdxRichEditControlToggleParagraphAlignmentRight* acAlignRight;
	TdxRichEditControlToggleParagraphAlignmentCenter* acAlignCenter;
	TdxRichEditControlToggleParagraphAlignmentJustify* acJustify;
	TdxRichEditControlToggleBulletedList* acBullets;
	TdxRichEditControlRedo* acRedo;
	TdxRichEditControlUndo* acUndo;
	TdxRichEditControlShowParagraphForm* acParagraph;
	TdxRichEditControlIncreaseFontSize* acIncreaseFontSize;
	TdxRichEditControlDecreaseFontSize* acDecreaseFontSize;
	TdxRichEditControlToggleFontSuperscript* acFontSuperscript;
	TdxRichEditControlToggleFontSubscript* acFontSubscript;
	TdxRichEditControlSetSingleParagraphSpacing* acSingleLineSpacing;
	TdxRichEditControlSetDoubleParagraphSpacing* acDoubleLineSpacing;
	TdxRichEditControlSetSesquialteralParagraphSpacing* acSesquialteralLineSpacing;
	TdxRichEditControlToggleSimpleNumberingList* acNumbering;
	TdxRichEditControlToggleMultiLevelList* acMultiLevelList;
	TdxRichEditControlToggleFontDoubleUnderline* acDoubleUnderline;
	TdxRichEditControlToggleFontStrikeout* acStrikeout;
	TdxRichEditControlToggleFontDoubleStrikeout* acDoubleStrikeout;
	TdxRichEditControlToggleShowWhitespace* acShowWhitespace;
	TdxRichEditControlIncrementIndent* acIncrementIndent;
	TdxRichEditControlDecrementIndent* acDecrementIndent;
	TdxRichEditControlChangeFontName* acFontName;
	TdxRichEditControlChangeFontSize* acFontSize;
	TdxRichEditControlNewDocument* acNewDocument;
	TdxRichEditControlLoadDocument* acOpenDocument;
	TdxRichEditControlSaveDocument* acSave;
	TdxRichEditControlSaveDocumentAs* acSaveAs;
	TdxRichEditControlShowFontForm* acFont;
	TdxRichEditControlShowTablePropertiesForm* acShowTablePropertiesForm;
	TdxRichEditControlSearchFind* acFind;
	TdxRichEditControlSearchReplace* acReplace;
	TdxRichEditControlSearchFindNext* acFindNext;
	TdxRichEditControlShowSymbolForm* acSymbol;
	TdxRichEditControlShowInsertTableForm* acInsertTableForm;
	TdxRichEditControlChangeFontColor* acFontColor;
	TdxRichEditControlToggleShowHorizontalRuler* acHorizontalRuler;
	TdxRichEditControlToggleShowVerticalRuler* acVerticalRuler;
	TdxRichEditControlZoomIn* acZoomIn;
	TdxRichEditControlZoomOut* acZoomOut;
	TdxRichEditControlToggleTableCellsAllBorders* acAllBorders;
	TdxRichEditControlResetTableCellsBorders* acNoBorder;
	TdxRichEditControlToggleTableCellsOutsideBorder* acOutsideBorders;
	TdxRichEditControlToggleTableCellsInsideBorder* acInsideBorders;
	TdxRichEditControlToggleTableCellsLeftBorder* acLeftBorder;
	TdxRichEditControlToggleTableCellsRightBorder* acRightBorder;
	TdxRichEditControlToggleTableCellsTopBorder* acTopBorder;
	TdxRichEditControlToggleTableCellsBottomBorder* acBottomBorder;
	TdxRichEditControlToggleTableCellsInsideHorizontalBorder* acHorizontalInsideBorder;
	TdxRichEditControlToggleTableCellsInsideVerticalBorder* acVerticalInsideBorder;
	TdxRichEditControlToggleTableCellsTopLeftAlignment* acTableCellsTopLeftAlignment;
	TdxRichEditControlToggleTableCellsTopCenterAlignment* acTableCellsTopCenterAlignment;
	TdxRichEditControlToggleTableCellsTopRightAlignment* acTableCellsTopRightAlignment;
	TdxRichEditControlToggleTableCellsMiddleLeftAlignment* acTableCellsMiddleLeftAlignment;
	TdxRichEditControlToggleTableCellsMiddleCenterAlignment* acTableCellsMiddleCenterAlignment;
	TdxRichEditControlToggleTableCellsMiddleRightAlignment* acTableCellsMiddleRightAlignment;
	TdxRichEditControlToggleTableCellsBottomLeftAlignment* acTableCellsBottomLeftAlignment;
	TdxRichEditControlToggleTableCellsBottomCenterAlignment* acTableCellsBottomCenterAlignment;
	TdxRichEditControlToggleTableCellsBottomRightAlignment* acTableCellsBottomRightAlignment;
	TdxRichEditControlShowSplitTableCellsForm* acSplitCells;
	TdxRichEditControlShowInsertTableCellsForm* acInsertTableCellsForm;
	TdxRichEditControlShowDeleteTableCellsForm* acDeleteTableCellsForm;
	TdxRichEditControlInsertTableRowAbove* acInsertRowAbove;
	TdxRichEditControlInsertTableRowBelow* acInsertRowBelow;
	TdxRichEditControlInsertTableColumnToTheLeft* acInsertColumnToTheLeft;
	TdxRichEditControlInsertTableColumnToTheRight* acInsertColumnToTheRight;
	TdxRichEditControlShowHyperlinkForm* acHyperlinkForm;
	TdxRichEditControlShowBookmarkForm* acShowBookmarkForm;
	TdxBarLargeButton* bbBookmarks;
	TdxRichEditControlShowAllFieldResults* acShowAllFieldResults;
	TdxRichEditControlShowAllFieldCodes* acShowAllFieldCodes;
	TdxRichEditControlShowInsertMergeFieldForm* acShowInsertMergeFieldForm;
	TdxRichEditControlToggleViewMergedData* acToggleViewMergedData;
	TdxBarLargeButton* bbInsertMergeField;
	TdxBarLargeButton* bbShowAllFieldCodes;
	TdxBarLargeButton* bbShowAllFieldResults;
	TdxBarLargeButton* bbViewMergedData;
	TdxRichEditControlInsertPageCountField* acPageCount;
	TdxRichEditControlInsertPageNumberField* acPageNumber;
	TdxRichEditControlEditPageHeader* acPageHeader;
	TdxRichEditControlEditPageFooter* acPageFooter;
	TdxBarLargeButton* bbHeader;
	TdxBarLargeButton* bbFooter;
	TdxBarLargeButton* bbPageNumber;
	TdxBarLargeButton* bbPageCount;
	TdxBarLargeButton* bbMargins;
	TdxBarLargeButton* bbSize;
	TdxRichEditControlShowPageMarginsSetupForm* acMargins;
	TdxRichEditControlShowPagePaperSetupForm* acSize;
	TdxRichEditControlSetPortraitPageOrientation* acPortrait;
	TdxRichEditControlSetLandscapePageOrientation* acLandscape;
	TdxBarSubItem* bsiOrientation;
	TdxBarLargeButton* bbPortrait;
	TdxBarLargeButton* bbLandscape;
	TdxBarLargeButton* bbLineNumberingOptions;
	TdxRichEditControlShowLineNumberingForm* acLineNumbering;
	TdxRichEditControlShowPageSetupForm* acShowPageSetupForm;
	TdxRichEditControlClosePageHeaderFooter* acClosePageHeaderFooter;
	TdxRichEditControlGoToPageHeader* acGoToPageHeader;
	TdxRichEditControlGoToPageFooter* acGoToPageFooter;
	TdxRichEditControlToggleHeaderFooterLinkToPrevious* acLinkToPrevious;
	TdxRichEditControlGoToPreviousPageHeaderFooter* acGoToPreviousPageHeaderFooter;
	TdxRichEditControlGoToNextPageHeaderFooter* acGoToNextPageHeaderFooter;
	TdxRichEditControlToggleDifferentFirstPage* acDifferentFirstPage;
	TdxRichEditControlToggleDifferentOddAndEvenPages* acDifferentOddAndEvenPages;
	TdxBarLargeButton* bbGoToHeader;
	TdxBarLargeButton* bbGoToFooter;
	TdxBarLargeButton* bbShowNext;
	TdxBarLargeButton* bbShowPrevious;
	TdxBarLargeButton* bbLinkToPrevious;
	TdxBarLargeButton* bbDifferentFirstPage;
	TdxBarLargeButton* bbDifferentOddAndEvenPages;
	TdxBarLargeButton* bbCloseHeaderAndFooter;
	TdxRibbonTab* rtFile;
	TdxRibbonTab* tabHome;
	TdxRibbonTab* rtInsert;
	TdxRibbonTab* rtPageLayout;
	TdxRibbonTab* rtReferences;
	TdxRibbonTab* rtMailings;
	TdxRibbonTab* rtReview;
	TdxRibbonTab* rtView;
	TdxRibbonTab* rtHeaderAndFooterTools;
	TdxRibbonTab* rtTableToolsLayout;
	TdxRibbonTab* rtTableToolsDesign;
	TdxRibbonTab* rtPictureTools;
	TdxRibbonTab* rtHelp;
	TdxZoomTrackBar* tbZoom;
	TdxScreenTip* stBold;
	TdxScreenTip* stItalic;
	TdxScreenTip* stNew;
	TdxScreenTip* stUnderline;
	TdxScreenTip* stBullets;
	TdxScreenTip* stFind;
	TdxScreenTip* stPaste;
	TdxScreenTip* stCut;
	TdxScreenTip* stReplace;
	TdxScreenTip* stCopy;
	TdxScreenTip* stAlignLeft;
	TdxScreenTip* stAlignRight;
	TdxScreenTip* stAlignCenter;
	TdxScreenTip* stAppMenu;
	TdxScreenTip* stOpen;
	TdxScreenTip* stPrint;
	TdxScreenTip* stBlue;
	TdxScreenTip* stBlack;
	TdxScreenTip* stSilver;
	TdxScreenTip* stFontDialog;
	TdxScreenTip* stHelpButton;
	TdxScreenTip* stParagraphDialog;
	TdxScreenTip* stAlignJustify;
	TdxScreenTip* stFontSuperscript;
	TdxScreenTip* stFontSubscript;
	TdxScreenTip* stIncreaseFontSize;
	TdxScreenTip* stDecreaseFontSize;
	TdxScreenTip* stNumbering;
	TdxScreenTip* stLineSpacing;
	TdxScreenTip* stMultiLevelList;
	TdxScreenTip* stDoubleUnderline;
	TdxScreenTip* stStrikethrough;
	TdxScreenTip* stDoubleStrikethrough;
	TdxScreenTip* stFontName;
	TdxScreenTip* stFontSize;
	TdxScreenTip* stFontColor;
	TdxScreenTip* stShowWhitespace;
	TdxScreenTip* stiItemSymbol;
	TdxScreenTip* stIncrementIndent;
	TdxScreenTip* stDecrementIndent;
	TdxRibbonPopupMenu* ppmFontColor;
	TdxRichEditControlInsertPicture* acInsertPicture;
	TdxBarLargeButton* bbInsertPicture;
	TdxScreenTip* stSave;
	TdxScreenTip* stSaveAs;
	TdxScreenTip* stUndo;
	TdxScreenTip* stRedo;
	TdxScreenTip* stSelectAll;
	TdxScreenTip* stInsertTable;
	TdxScreenTip* stInlinePicture;
	TdxScreenTip* stHyperlink;
	TdxScreenTip* stSymbol;
	TdxScreenTip* stHorizontalRuler;
	TdxScreenTip* stVerticalRuler;
	TdxScreenTip* stZoomOut;
	TdxScreenTip* stZoomIn;
	TdxScreenTip* stTableProperties;
	TdxScreenTip* stDelete;
	TdxScreenTip* stInsertRowsAbove;
	TdxScreenTip* stInsertRowsBelow;
	TdxScreenTip* stInsertColumnsToTheLeft;
	TdxScreenTip* stInsertColumnsToTheRight;
	TdxScreenTip* stSplitCells;
	TdxScreenTip* stBorders;
	TdxScreenTip* stCellsAlignTopLeft;
	TdxScreenTip* stCellsAlignCenterLeft;
	TdxScreenTip* stCellsAlignBottomLeft;
	TdxScreenTip* stCellsAlignTopCenter;
	TdxScreenTip* stCellsAlignCenter;
	TdxScreenTip* stCellsAlignBottomCenter;
	TdxScreenTip* stCellsAlignTopRight;
	TdxScreenTip* stCellsAlignCenterRight;
	TdxScreenTip* stCellsAlignBottomRight;
	TdxScreenTip* stInsertCells;
	TdxRichEditControlToggleTableAutoFitContents* acAutoFitContents;
	TdxRichEditControlToggleTableAutoFitWindow* acAutoFitWindow;
	TdxRichEditControlToggleTableFixedColumnWidth* acFixedColumnWidth;
	TdxBarSubItem* bsiAutoFit;
	TdxBarLargeButton* bbAutoFitContents;
	TdxBarLargeButton* bbFixedColumnWidth;
	TdxBarLargeButton* bbAutoFitWindow;
	TdxScreenTip* stAutoFit;
	TdxRichEditControlSplitTable* acSplitTable;
	TdxBarLargeButton* bbSplitTable;
	TdxScreenTip* stSplitTable;
	TdxRichEditControlMergeTableCells* acMergeCells;
	TdxBarLargeButton* bbMergeCells;
	TdxScreenTip* stMergeCells;
	TdxRichEditControlTextHighlight* acTextHighlight;
	TdxBarLargeButton* bbTextHighlight;
	TdxScreenTip* stTextHighlight;
	TdxRichEditControlInsertPageBreak* acPageBreak;
	TdxRichEditControlSwitchToDraftView* acDraftView;
	TdxRichEditControlSwitchToPrintLayoutView* acPrintLayoutView;
	TdxRichEditControlSwitchToSimpleView* acSimpleView;
	TdxBarLargeButton* bbPage;
	TdxBarLargeButton* bbSimpleView;
	TdxBarLargeButton* bbDraftView;
	TdxBarLargeButton* bbPrintLayoutView;
	TdxScreenTip* stPage;
	TdxScreenTip* stSimpleView;
	TdxScreenTip* stDraftView;
	TdxScreenTip* stPrintLayoutView;
	TdxRichEditControlDeleteTable* acDeleteTable;
	TdxRichEditControlDeleteTableRows* acDeleteTableRows;
	TdxRichEditControlDeleteTableColumns* acDeleteTableColumns;
	TdxBarLargeButton* bbDeleteTable;
	TdxBarLargeButton* bbDeleteRows;
	TdxBarLargeButton* bbDeleteColumns;
	TdxRichEditControlTextLowerCase* acLowerCase;
	TdxRichEditControlTextUpperCase* acUpperCase;
	TdxRichEditControlToggleTextCase* acToggleCase;
	TdxBarSubItem* bsiChangeCase;
	TdxBarLargeButton* bbUpperCase;
	TdxBarLargeButton* bbToggleCase;
	TdxBarLargeButton* bbLowerCase;
	TdxScreenTip* stChangeCase;
	TdxBarSubItem* sbiColumns;
	TdxBarLargeButton* bbOneColumn;
	TdxBarLargeButton* bbTwoColumns;
	TdxBarLargeButton* bbThreeColumn;
	TdxRichEditControlSetSectionOneColumn* acSectionOneColumn;
	TdxRichEditControlSetSectionThreeColumns* acSectionThreeColumns;
	TdxRichEditControlSetSectionTwoColumns* acSectionTwoColumns;
	TdxScreenTip* stColumns;
	TdxBarLargeButton* bbFontColor;
	TdxRibbonPopupMenu* ppmTextHighlightColor;
	TdxRibbonGalleryItem* rgiTextHighlightColorTheme;
	TdxRibbonGalleryItem* rgiTextHighlightColor;
	TdxRichEditControlShowColumnsSetupForm* acMoreColumns;
	TdxBarLargeButton* bbMoreColumns;
	TdxBarSubItem* bsiBreaks;
	TdxBarLargeButton* bbColumn;
	TdxBarLargeButton* bbSectionNext;
	TdxBarLargeButton* bbSectionEvenPage;
	TdxBarLargeButton* bbSectionOdd;
	TdxRichEditControlInsertSectionBreakNextPage* acSectionBreakNextPage;
	TdxRichEditControlInsertSectionBreakOddPage* acSectionBreakOddPage;
	TdxRichEditControlInsertSectionBreakEvenPage* acSectionBreakEvenPage;
	TdxRichEditControlInsertColumnBreak* acColumnBreak;
	TdxScreenTip* stBreaks;
	TdxRichEditControlChangePageColor* acPageColor;
	TdxScreenTip* stPageColor;
	TdxBarSubItem* bsiPageColor;
	TdxRichEditControlSetSectionLineNumberingNone* acLineNumberingNone;
	TdxRichEditControlSetSectionLineNumberingContinuous* acLineNumberingContinuous;
	TdxRichEditControlSetSectionLineNumberingRestartNewPage* acLineNumberingRestartNewPage;
	TdxRichEditControlSetSectionLineNumberingRestartNewSection* acLineNumberingRestartNewSection;
	TdxBarSubItem* bsiLineNumbers;
	TdxBarLargeButton* bbLineNumberingNone;
	TdxBarLargeButton* bbacLineNumberingRestartNewSection;
	TdxBarLargeButton* bbLineNumberingRestartNewPage;
	TdxBarLargeButton* bbLineNumberingContinuous;
	TdxScreenTip* stLineNumbers;
	TdxRichEditControl* RichEditControl;
	TdxPSEngineController* PrinterEngine;
	TdxComponentPrinter* Printer;
	TdxRichEditControlReportLink* RichEditPrinterLink;
	TdxBar* bmbPrint;
	TdxBarLargeButton* bbPrintPreview;
	TdxBarLargeButton* bbPageSetup;
	TdxBarLargeButton* bbPicture;
	TdxRichEditControlInsertFloatingObjectPicture* acPicture;
	TdxRichEditControlShowFloatingObjectLayoutOptionsForm* acFloatingObjectLayoutOptionsForm;
	TdxRichEditControlSetFloatingObjectSquareTextWrapType* acFloatingObjectSquareTextWrapType;
	TdxRichEditControlSetFloatingObjectBehindTextWrapType* acFloatingObjectBehindTextWrapType;
	TdxRichEditControlSetFloatingObjectInFrontOfTextWrapType* acFloatingObjectInFrontOfTextWrapType;
	TdxRichEditControlSetFloatingObjectThroughTextWrapType* acFloatingObjectThroughTextWrapType;
	TdxRichEditControlSetFloatingObjectTightTextWrapType* acFloatingObjectTightTextWrapType;
	TdxRichEditControlSetFloatingObjectTopAndBottomTextWrapType* acFloatingObjectTopAndBottomTextWrapType;
	TdxRichEditControlSetFloatingObjectTopLeftAlignment* acFloatingObjectTopLeftAlignment;
	TdxRichEditControlSetFloatingObjectTopCenterAlignment* acFloatingObjectTopCenterAlignment;
	TdxRichEditControlSetFloatingObjectTopRightAlignment* acFloatingObjectTopRightAlignment;
	TdxRichEditControlSetFloatingObjectMiddleLeftAlignment* acFloatingObjectMiddleLeftAlignment;
	TdxRichEditControlSetFloatingObjectMiddleCenterAlignment* acFloatingObjectMiddleCenterAlignment;
	TdxRichEditControlSetFloatingObjectMiddleRightAlignment* acFloatingObjectMiddleRightAlignment;
	TdxRichEditControlSetFloatingObjectBottomLeftAlignment* acFloatingObjectBottomLeftAlignment;
	TdxRichEditControlSetFloatingObjectBottomCenterAlignment* acFloatingObjectBottomCenterAlignment;
	TdxRichEditControlSetFloatingObjectBottomRightAlignment* acFloatingObjectBottomRightAlignment;
	TdxRichEditControlFloatingObjectBringForward* acFloatingObjectBringForward;
	TdxRichEditControlFloatingObjectBringToFront* acFloatingObjectBringToFront;
	TdxRichEditControlFloatingObjectBringInFrontOfText* acFloatingObjectBringInFrontOfText;
	TdxRichEditControlFloatingObjectSendBackward* acFloatingObjectSendBackward;
	TdxRichEditControlFloatingObjectSendToBack* acFloatingObjectSendToBack;
	TdxRichEditControlFloatingObjectSendBehindText* acFloatingObjectSendBehindText;
	TdxBarSubItem* bsiWrapText;
	TdxBarSubItem* bsiPosition;
	TdxBarSubItem* bsiBringToFront;
	TdxBarSubItem* bsiSendToBack;
	TdxBarLargeButton* bbSquare;
	TdxBarLargeButton* bbTight;
	TdxBarLargeButton* bbThrough;
	TdxBarLargeButton* bbTopAndBottom;
	TdxBarLargeButton* bbBehindText;
	TdxBarLargeButton* bbInFrontOfText;
	TdxBarLargeButton* bbFloatingObjectTopLeft;
	TdxBarLargeButton* bbFloatingObjectTopCenter;
	TdxBarLargeButton* bbFloatingObjectTopRight;
	TdxBarLargeButton* bbFloatingObjectMiddleLeft;
	TdxBarLargeButton* bbFloatingObjectMiddleCenter;
	TdxBarLargeButton* bbFloatingObjectMiddleRight;
	TdxBarLargeButton* bbFloatingObjectBottomLeft;
	TdxBarLargeButton* bbFloatingObjectBottomCenter;
	TdxBarLargeButton* bbFloatingObjectBottomRight;
	TdxBarLargeButton* bbBringForward;
	TdxBarLargeButton* bbBringToFront;
	TdxBarLargeButton* bbBringInFrontOfText;
	TdxBarLargeButton* bbSendBackward;
	TdxBarLargeButton* bbSendToBack;
	TdxBarLargeButton* bbSendBehindText;
	TdxBarLargeButton* bbTextBox;
	TdxRichEditControlInsertTextBox* acTextBox;
	TdxRichEditControlShowPrintForm* acShowPrintForm;
	TdxRichEditControlShowPrintPreviewForm* acShowPrintPreviewForm;
	TdxRichEditControlToggleShowTableGridLines* acShowTableGridLines;
	TdxBarLargeButton* bbViewGridLines;
	TdxRichEditControlChangeFloatingObjectFillColor* acChangeFloatingObjectFillColor;
	TdxRichEditControlChangeFloatingObjectOutlineColor* acChangeFloatingObjectOutlineColor;
	TcxImageList* ilWidthLines;
	TcxBarEditItem* bbWidthLines;
	TdxRichEditControlChangeFloatingObjectOutlineWidth* acOutlineWidth;
	TdxRibbonPopupMenu* ppmFloatingObjectFillColor;
	TdxRibbonPopupMenu* ppmFloatingObjectOutlineColor;
	TdxBarButton* bbFloatingObjectFillColor;
	TdxBarButton* bbFloatingObjectOutlineColor;
	TdxRibbonGalleryItem* rgiFloatingObjectFillColor;
	TdxRibbonGalleryItem* rgiFloatingObjectOutlineColor;
	TdxRibbonGalleryItem* rgiFloatingObjectFillColorTheme;
	TdxRibbonGalleryItem* rgiFloatingObjectOutlineColorTheme;

	void __fastcall bbApplicationButtonClick(TObject *Sender);
	void __fastcall bmbHomeFontClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tbZoomPropertiesChange(TObject *Sender);
	void __fastcall bmbHomeParagraphClick(TObject *Sender);
	void __fastcall bmbTableToolsCellSizeClick(TObject *Sender);
	void __fastcall RowsAndColumnsCaptionButtonsClick(TObject *Sender);
	void __fastcall bbFontColorClick(TObject *Sender);
	void __fastcall acExitExecute(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall recRichEditControlSelectionChanged(TObject *Sender);
	void __fastcall recRichEditControlZoomChanged(TObject *Sender);
	void __fastcall bbTextHighlightClick(TObject *Sender);
	void __fastcall recRichEditControlActiveViewChanged(TObject *Sender);
	void __fastcall acPrintExecute(TObject *Sender);
	void __fastcall acPrintPreviewExecute(TObject *Sender);
	void __fastcall bmbPageLayoutPageSetupCaptionButtons0Click(TObject *Sender);
	void __fastcall bmbPictureToolsArrangeCaptionButtons0Click(TObject *Sender);
	void __fastcall beFontNamePropertiesFontPreviewButtonClick(TObject *Sender, TcxFontButtonType ButtonType);
	void __fastcall recRichEditControlHyperlinkClick(TObject *Sender, const TdxHyperlinkClickEventArgs *Args);
	void __fastcall recRichEditControlDocumentClosing(TObject *Sender, Boolean &CanClose);
	void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall bbFloatingObjectOutlineColorClick(TObject *Sender);
	void __fastcall bbFloatingObjectFillColorClick(TObject *Sender);
private: // User declarations
    TColorPickerController* FFontColorPicker;
	TColorPickerController* FPageColorPicker;
	TColorPickerController* FTextHighlightColorPicker;
	TColorPickerController* FFloatingObjectFillColorPicker;
	TColorPickerController* FFloatingObjectOutlineColorPicker;

	void CheckDocumentClosing(Boolean &CanClose);
#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
	void DrawHelpedColorLine(Vcl::Graphics::TGraphic* AGlyph, TColor AColor, TcxImageList* AImageBasicGlyph, Integer AIndexBasicGlyph);
#else
	void DrawHelpedColorLine(Graphics::TGraphic* AGlyph, TColor AColor, TcxImageList* AImageBasicGlyph, Integer AIndexBasicGlyph);
#endif
	void __fastcall PageColorChangedHandler(TObject *Sender);
	void UpdateFloatingPictureContext();
	void UpdateFloatingObjectFillColor();
	void UpdateFloatingObjectFillColorImage();
	void UpdateFloatingObjectOutlineColor();
	void UpdateFloatingObjectOutlineColorImage();
	void UpdateHeaderAndFooterContext();
	void UpdateRibbonContexstsStates();
	void UpdateTableContext();
	void UpdateFontColor();
	void UpdateFontColorImage();
	void UpdateFontNameComboBoxPropertiesFontStyle(TFontStyles AStyle, Boolean AChecked);
	void UpdateTextHighlight();
	void UpdateTextHighlightImage();
	void UpdateZoom();
public: // User declarations
	__fastcall TfrmRibbonRichEditMain(TComponent* Owner);
};
// ---------------------------------------------------------------------------
extern PACKAGE TfrmRibbonRichEditMain *frmRibbonRichEditMain;
// ---------------------------------------------------------------------------
#endif

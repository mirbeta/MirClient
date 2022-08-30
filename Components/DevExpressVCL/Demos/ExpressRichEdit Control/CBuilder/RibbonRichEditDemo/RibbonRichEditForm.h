// ---------------------------------------------------------------------------
#ifndef RibbonRichEditFormH
#define RibbonRichEditFormH
// ---------------------------------------------------------------------------
  #include "Windows.hpp"
  #include "Forms.hpp"
  #include "RichEditControlBase.h"
  #include "RibbonRichEditDemoOptions.h"
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
  #include "..\RibbonRichEditMainForm\RibbonRichEditMainForm.h"
  #include "dxPSGlbl.hpp"
  #include "dxPSUtl.hpp"
  #include "dxPSEngn.hpp"
  #include "dxPrnPg.hpp"
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
  #include "dxPSRichEditControlLnk.hpp"
  #include "dxPScxEditorProducers.hpp"
  #include "dxPScxExtEditorProducers.hpp"
  #include "dxPSCore.hpp"
  #include "EBarsUtils.h"

class TdxRibbonRecentDocumentsController : public TObject {
private:
	TdxRibbonBackstageViewGalleryControl* FRecentDocuments;
	TdxRibbonBackstageViewGalleryControl* FRecentPaths;

	TdxRibbonBackstageViewGalleryItem* GetItemByValue
		(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue);
	TdxRibbonBackstageViewGalleryItem* InternalAdd
		(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue);
	void InternalLoad(TdxRibbonBackstageViewGalleryGroup* AGroup,
		TCustomIniFile* AIniFile, const String ASection);
	void InternalSave(TdxRibbonBackstageViewGalleryGroup* AGroup,
		TCustomIniFile* AIniFile, const String ASection);

protected:
	virtual void DoLoad(TCustomIniFile* AConfig);
	virtual void DoSave(TCustomIniFile* AConfig);

public:
	TdxRibbonRecentDocumentsController
		(TdxRibbonBackstageViewGalleryControl* ARecentPaths,
		TdxRibbonBackstageViewGalleryControl* ARecentDocuments);

	void LoadFromIniFile(const String AFileName);
  	void SaveToIniFile(const String AFileName);
};

class TfrmRibbonRichEditForm : public TfrmRibbonRichEditMain
{
__published: // IDE-managed Components
	TdxRibbonBackstageView* rbvBackstageView;
	TdxRibbonBackstageViewTabSheet* bvtsOpen;
	TBevel* bvlSpacer1;
	TBevel* bvSpacer2;
	TBevel* bvSpacer7;
	TcxGroupBox* gbBackstageViewTabCaption;
	TBevel* bvSpacer4;
	TBevel* bvSpacer3;
	TcxLabel* lbbvTabCaption2010;
	TcxLabel* lbbvTabCaption2013;
	TcxGroupBox* gbLocationsMain;
	TcxGroupBox* gbLocationsPane;
	TBevel* bvSpacer5;
	TdxBevel* dxBevel1;
	TdxRibbonBackstageViewGalleryControl* bvgcLocations;
	TdxRibbonBackstageViewGalleryGroup* bvgcLocationsRecentDocumentsGroup;
	TdxRibbonBackstageViewGalleryItem* bvgcLocationsRecentDocumentsItem;
	TdxRibbonBackstageViewGalleryGroup* bvgcLocationsGroup1;
	TdxRibbonBackstageViewGalleryItem* bvgcLocationsComputerItem;
	TcxScrollBox* gbRecentPathsPane;
	TBevel* bvSpacer6;
	TdxRibbonBackstageViewGalleryControl* bvgcRecentPaths;
	TdxRibbonBackstageViewGalleryGroup* bvgcRecentPathsGroup;
	TcxGroupBox* gbRecentPathsPaneBottom;
	TcxButton* btnBrowsePath;
	TcxGroupBox* gbRecentPathsPaneCurrentFolder;
	TcxLabel* lbCurrentFolder;
	TdxRibbonBackstageViewGalleryControl* bvgcCurrentFolder;
	TcxLabel* lbComputer;
	TcxLabel* lbRecentFolders;
	TcxScrollBox* gbRecentDocumentsPane;
	TBevel* bvSpacer8;
	TcxLabel* lbRecentDocuments;
	TdxRibbonBackstageViewGalleryControl* bvgcRecentDocuments;
	TdxRibbonBackstageViewTabSheet* bvtsSaveAs;
	TBevel* Bevel1;
	TBevel* Bevel2;
	TBevel* Bevel3;
	TdxRibbonBackstageViewTabSheet* bvtsAbout;
	TBevel* Bevel4;
	TBevel* Bevel5;
	TBevel* Bevel6;
	TcxMemo* meAbout;
	TdxRibbonBackstageViewTabSheet* bvtsHelp;
	TBevel* Bevel7;
	TBevel* Bevel8;
	TBevel* Bevel9;
	TcxGroupBox* gbHelpContent;
	TImage* Image1;
	TImage* imDot1;
	TImage* imDot2;
	TImage* imDot3;
	TImage* imDot4;
	TImage* imDot5;
	TImage* imDot6;
	TImage* imLogo;
	TcxLabel* lblSupport;
	TcxLabel* cxLabel1;
	TcxLabel* lblClientCenter;
	TcxLabel* lblDownloads;
	TcxLabel* lblDXonWeb;
	TcxLabel* lblHelpBars;
	TcxLabel* lblHelpDocking;
	TcxLabel* lblProducts;
	TcxLabel* lblSupportCenter;
	void __fastcall rbvBackstageViewPopup(TObject *Sender);
	void __fastcall rbvBackstageViewTabChanged(TObject *Sender);
	void __fastcall btnBrowsePathClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall bvgcLocationsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall bvgcRecentDocumentsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall bvgcRecentPathsItemClick(TObject *Sender, TdxRibbonBackstageViewGalleryItem *AItem);
	void __fastcall  RichEditControlDocumentLoaded(TObject *Sender);
private: // User declarations
	TdxRibbonRecentDocumentsController* FRecentDocumentsController;

	TdxRibbonRecentDocumentsController* CreateRecentDocumentsController();
    void DoBackstageViewTabChanged();
    void UpdateColorSchemeRelatedControls();
protected:
	void UpdateColorSchemeRelatedControls(const UnicodeString AName);
	void SetRibbonDemoStyle(TRibbonDemoStyle AStyle);

	__property TdxRibbonRecentDocumentsController* RecentDocumentsController = {read = FRecentDocumentsController};
public: // User declarations
	__fastcall TfrmRibbonRichEditForm(TComponent* Owner);
};
// ---------------------------------------------------------------------------
extern PACKAGE TfrmRibbonRichEditForm *frmRibbonRichEditForm;
// ---------------------------------------------------------------------------
#endif

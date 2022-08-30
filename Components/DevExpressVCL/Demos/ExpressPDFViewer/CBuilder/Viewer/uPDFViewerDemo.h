//---------------------------------------------------------------------------

#ifndef uPDFViewerDemoH
#define uPDFViewerDemoH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxBarEditItem.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxBar.hpp"
#include "dxRibbon.hpp"
#include "dxRibbonForm.hpp"
#include "dxRibbonBackstageView.hpp"
#include "dxRibbonSkins.hpp"
#include "dxRibbonStatusBar.hpp"
#include "dxStatusBar.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxLabel.hpp"
#include "cxShellBrowserDialog.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxActions.hpp"
#include "dxBarExtItems.hpp"
#include "dxCustomPreview.hpp"
#include "dxPDFViewer.hpp"
#include "dxPDFViewerActions.hpp"
#include "dxRibbonCustomizationForm.hpp"
#include "dxRibbonGallery.hpp"
#include "dxSkinChooserGallery.hpp"
#include "dxZoomTrackBar.hpp"
#include <ActnList.hpp>
#include <ImgList.hpp>
#include "dxProgressDialog.h"
#include "uExportToBitmaps.h"
#include "dxPSActions.hpp"
#include "dxPSdxPDFViewerLnk.hpp"
#include "cxDrawTextUtils.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSCore.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwAdv.hpp"
#include "dxPSPrVwRibbon.hpp"
#include "dxPSPrVwStd.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "dxPDFDocument.hpp"
#include "dxPrinting.hpp"
#include "cxImageList.hpp"
#include "dxPDFBase.hpp"
#include "dxPDFDocumentViewer.hpp"
#include "dxPDFRecognizedObject.hpp"
#include "dxPDFText.hpp"
//---------------------------------------------------------------------------
class TfrmPDFViewer : public TdxRibbonForm
{
__published:	// IDE-managed Components
	TdxRibbon *dxRibbon1;
	TdxRibbonTab *dxRibbonTabHome;
	TdxPDFViewer *dxPDFViewer1;
	TdxRibbonStatusBar *dxRibbonStatusBar1;
	TdxStatusBarContainerControl *dxRibbonStatusBar1Container5;
	TdxZoomTrackBar *tbZoom;
	TdxBarManager *dxBarManager1;
	TdxBar *dxBarManager1Bar2;
	TdxBar *dxBarFile;
	TdxBar *dxBarZoom;
	TdxBar *dxBarManager1Bar3;
	TdxBar *dxBarAbout;
	TdxBar *dxBarManager1Bar4;
	TdxBar *bmiNavigation;
	TdxBar *biStatusBarZoom;
	TdxBar *dxRibbonNavigationGroup;
	TcxBarEditItem *cxBarEditItem1;
	TdxSkinChooserGalleryItem *dxSkinChooserGalleryItem1;
	TdxBarLargeButton *btnAbout;
	TdxBarLargeButton *dxBarLargeButton2;
	TdxBarLargeButton *dxBarLargeButton3;
	TdxBarLargeButton *dxBarExit1;
	TdxBarLargeButton *dxBarButtonAbout;
	TdxBarLargeButton *dxBarButtonExportToTIFF;
	TdxBarLargeButton *dxBarButtonExportToPNG;
	TcxBarEditItem *cxBarEditItem2;
	TcxBarEditItem *bilPageCount;
	TdxBarStatic *biStatusBarCurrentZoom;
	TcxBarEditItem *cxBarEditItem3;
	TdxBarSeparator *dxBarSeparator1;
	TcxBarEditItem *bteActualZoom;
	TcxBarEditItem *bseActivePage;
	TdxBarSpinEdit *dxRibbonStatusBarActivePage;
	TcxBarEditItem *sseActivePage;
	TdxBarLargeButton *dxBarLargeButtonZoomOut;
	TdxBarLargeButton *dxBarLargeButtonZoomIn;
	TdxBarSubItem *dxBarSubItem2;
	TdxBarLargeButton *dxBarLargeButton10;
	TdxBarLargeButton *dxBarLargeButton25;
	TdxBarLargeButton *dxBarLargeButton50;
	TdxBarLargeButton *dxBarLargeButton75;
	TdxBarLargeButton *dxBarLargeButton100;
	TdxBarLargeButton *dxBarLargeButton125;
	TdxBarLargeButton *dxBarLargeButton150;
	TdxBarLargeButton *dxBarLargeButton200;
	TdxBarLargeButton *dxBarLargeButton400;
	TdxBarLargeButton *dxBarLargeButton500;
	TdxBarLargeButton *dxBarLargeButtonActualSize;
	TdxBarLargeButton *dxBarLargeButtonZoomtoPageLevel;
	TdxBarLargeButton *dxBarLargeButtonFitWidth;
	TdxBarSubItem *dxBarSubItem3;
	TdxBarLargeButton *dxBarLargeButtonOpen;
	TdxBarLargeButton *dxBarLargeButtonPreviousPage;
	TdxBarLargeButton *dxBarLargeButtonNextPage;
	TdxBarSubItem *dxBarSubItem1;
	TdxBarLargeButton *dxBarLargeButtonFirstPage;
	TdxBarLargeButton *dxBarLargeButtonLastPage;
	TdxBarSubItem *dxBarSubItem4;
	TcxShellBrowserDialog *sbdSelectFolderDialog;
	TActionList *ActionList1;
	TdxPDFViewerOpenDocument *dxPDFViewerOpenDocumentAction;
	TdxPDFViewerGoToPrevPage *dxPDFViewerGoToPrevPageAction;
	TdxPDFViewerGoToNextPage *dxPDFViewerGoToNextPageAction;
	TdxPDFViewerGoToFirstPage *dxPDFViewerGoToFirstPageAction;
	TdxPDFViewerGoToLastPage *dxPDFViewerGoToLastPageAction;
	TdxPDFViewerZoomOut *dxPDFViewerZoomOutAction;
	TdxPDFViewerZoomIn *dxPDFViewerZoomInAction;
	TdxPDFViewerZoom10 *dxPDFViewerZoom10Action;
	TdxPDFViewerZoom25 *dxPDFViewerZoom25Action;
	TdxPDFViewerZoom50 *dxPDFViewerZoom50Action;
	TdxPDFViewerZoom75 *dxPDFViewerZoom75Action;
	TdxPDFViewerZoom100 *dxPDFViewerZoom100Action;
	TdxPDFViewerZoom125 *dxPDFViewerZoom125Action;
	TdxPDFViewerZoom150 *dxPDFViewerZoom150Action;
	TdxPDFViewerZoom200 *dxPDFViewerZoom200Action;
	TdxPDFViewerZoom400 *dxPDFViewerZoom400Action;
	TdxPDFViewerZoom500 *dxPDFViewerZoom500Action;
	TdxPDFViewerZoomActualSize *dxPDFViewerZoomActualSizeAction;
	TdxPDFViewerZoomToPageLevel *dxPDFViewerZoomToPageLevelAction;
	TdxPDFViewerZoomFitWidth *dxPDFViewerZoomFitWidthAction;
	TAction *actExit;
	TcxImageList *cxImageList1;
	TcxImageList *cxImageList2;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TdxBarStatic *dxBarStatic1;
	TdxBarStatic *sbPageCount;
	TdxPDFViewerShowPrintForm *dxPDFViewerShowPrintForm;
	TdxBarLargeButton *dxBarLargeButtonPrint;
	TdxComponentPrinter *dxComponentPrinter1;
	TdxPDFViewerReportLink *dxComponentPrinter1Link1;
	TdxPDFViewerCloseDocument *dxPDFViewerCloseDocument;
	TdxBarLargeButton *dxBarLargeButtonClose;
	TdxPDFViewerSelectTool *dxPDFViewerSelectTool;
	TdxBar *dxBarTools;
	TdxBarLargeButton *dxBarLargeButtonSelectTool;
	TdxPDFViewerHandTool *dxPDFViewerHandTool;
	TdxBarLargeButton *dxBarLargeButtonHandTool;
	TdxPDFViewerSelectAll *dxPDFViewerSelectAll;
	TdxBarLargeButton *dxBarLargeButtonSelectAll;
	TdxPDFViewerGoToPrevView *dxPDFViewerGoToPrevView;
	TdxBarLargeButton *dxBarLargeButtonPreviousView;
	TdxPDFViewerGoToNextView *dxPDFViewerGoToNextView;
	TdxBarLargeButton *dxBarLargeButtonNextView;
	TdxBarSubItem *dxBarSubItem5;
	TdxPDFViewerRotateClockwise *dxPDFViewerRotateClockwise;
	TdxBarLargeButton *dxBarLargeButtonRotateClockwise;
	TdxPDFViewerRotateCounterClockwise *dxPDFViewerRotateCounterClockwise;
	TdxBarLargeButton *dxBarLargeButtonRotateCounterclockwise;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall dxPDFViewer1ZoomFactorChanged(TObject *Sender);
	void __fastcall dxPDFViewer1SelectedPageChanged(TObject *Sender, int APageIndex);
	void __fastcall bseActivePageChange(TObject *Sender);
	void __fastcall sseActivePageChange(TObject *Sender);
	void __fastcall tbZoomPropertiesChange(TObject *Sender);
	void __fastcall dxPDFViewer1DocumentLoaded(TdxPDFDocument *ASender, const TdxPDFDocumentLoadInfo &AInfo);
	void __fastcall dxBarButtonExportToTIFFClick(TObject *Sender);
	void __fastcall dxBarButtonExportToPNGClick(TObject *Sender);
	void __fastcall dxBarButtonAboutClick(TObject *Sender);
	void __fastcall dxPDFViewer1CustomDrawPreRenderPage(TObject *Sender, TcxCanvas *ACanvas,
          const TdxPDFPreRenderPageInfo &APageInfo, bool &ADone);


private:
	int FLockCount;
	TfrmProgress* FProgressDialog;

	void AfterExport();
	void BeforeExport();

	void InitializeLookAndFeel();
	bool IsLocked();
	void BeginUpdate();
	void CancelUpdate();
	void UpdateActivePage(int AIndex);
	void UpdateActivePageEditor(TcxBarEditItem *ASpinEdit);
	void ShowExportToPNGDialog();
	void ShowExportToTIFFDialog();
public:	// User declarations
	__fastcall TfrmPDFViewer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmPDFViewer *frmPDFViewer;
//---------------------------------------------------------------------------
#endif


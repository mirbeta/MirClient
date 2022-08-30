// ---------------------------------------------------------------------------
#ifndef RichEditControlBaseH
#define RichEditControlBaseH
// ---------------------------------------------------------------------------
#include "Windows.hpp"
#include "Forms.hpp"
#include "cxGraphics.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "dxRibbonCustomizationForm.hpp"
#include "dxRibbonSkins.hpp"
#include "dxCore.hpp"
#include "dxCoreClasses.hpp"
#include "dxGDIPlusAPI.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxRichEdit.Types.hpp"
#include "dxRichEdit.Options.hpp"
#include "dxRichEdit.Control.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "dxBar.hpp"
#include "dxBarApplicationMenu.hpp"
#include "dxRibbon.hpp"
#include "dxScreenTip.hpp"
#include "ImgList.hpp"
#include "Controls.hpp"
#include "Classes.hpp"
#include "ActnList.hpp"
#include "dxRibbonGallery.hpp"
#include "dxSkinChooserGallery.hpp"
#include "dxRibbonForm.hpp"
#include "dxStatusBar.hpp"
#include "dxRibbonStatusBar.hpp"
#include "dxRichEdit.Platform.Win.Control.hpp"
#include "cxClasses.hpp"
#include "RibbonRichEditDemoOptions.h"
// ---------------------------------------------------------------------------
class TfrmRichEditControlBase: public TdxRibbonForm
{
__published: // IDE-managed Components
	TdxRibbon* Ribbon;
	TdxBarManager* bmBarManager;
	TActionList* acActions;
	TdxBarLargeButton* bbApplicationButton;
	TdxBarLargeButton* bbQATVisible;
	TdxBarLargeButton* bbRibbonForm;
	TdxSkinChooserGalleryItem* scgiLookAndFeel;
	TAction* acQATAboveRibbon;
	TAction* acQATBelowRibbon;
	TdxScreenTipRepository* stBarScreenTips;
	TcxLookAndFeelController* cxLookAndFeelController;
	TdxBarApplicationMenu* ApplicationMenu;
	TdxRibbonStatusBar* rsbStatusBar;
	TcxImageList *ilSmallImages;
	TcxImageList *ilLargeImages;
	TcxImageList *ilLargeColorSchemesGlyphs;
	TcxImageList *ilSmallColorSchemesGlyphs;
	void __fastcall bbRibbonFormClick(TObject *Sender);
	void __fastcall acQATApplicationButtonExecute(TObject *Sender);
	void __fastcall bbQATVisibleClick(TObject *Sender);
	void __fastcall acQATAboveAndBelowRibbonExecute(TObject *Sender);
	void __fastcall acQATAboveAndBelowRibbonUpdate(TObject *Sender);
	void __fastcall scgiLookAndFeelPopulate(TObject *Sender);
	void __fastcall scgiLookAndFeelSelected(TObject *Sender, const UnicodeString ASkinName);
	void __fastcall bbOptionsClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:
        TRibbonDemoStyle GetRibbonDemoStyle();
	void InitializeLookAndFeel();
protected:
	virtual void UpdateColorSchemeRelatedControls();
	virtual void SetRibbonDemoStyle(TRibbonDemoStyle AStyle);

	__property TRibbonDemoStyle RibbonDemoStyle = {read = GetRibbonDemoStyle, write = SetRibbonDemoStyle};
  public: // User declarations
	__fastcall TfrmRichEditControlBase(TComponent* Owner);
};
// ---------------------------------------------------------------------------
extern PACKAGE TfrmRichEditControlBase *frmRichEditControlBase;
// ---------------------------------------------------------------------------
#endif

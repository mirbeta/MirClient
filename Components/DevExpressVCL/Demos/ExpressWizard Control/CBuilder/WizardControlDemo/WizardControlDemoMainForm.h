//---------------------------------------------------------------------------

#ifndef TfrmWizardControlDemoMainH
#define TfrmWizardControlDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxCustomWizardControl.hpp"
#include "dxWizardControl.hpp"
#include "dxWizardControlForm.hpp"
#include "cxCheckBox.hpp"
#include "cxCheckListBox.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxLabel.hpp"
#include "cxMemo.hpp"
#include "cxProgressBar.hpp"
#include "cxRadioGroup.hpp"
#include "cxShellCommon.hpp"
#include "cxShellTreeView.hpp"
#include "cxTextEdit.hpp"
#include "cxTreeView.hpp"
#include "dxBevel.hpp"
#include "dxBreadcrumbEdit.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxImageSlider.hpp"
#include "dxShellBreadcrumbEdit.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ShlObj.hpp>
#include "WizardControlDemoSetupForm.h"
//---------------------------------------------------------------------------
#pragma option push -b-
enum TInstallMode { imModify, imRepair, imRemove };
#pragma option pop
//---------------------------------------------------------------------------
class TfrmWizardControlDemoMain : public TdxWizardControlForm
{
__published:	// IDE-managed Components
  TcxCheckBox *cbAcceptEULA;
  TcxCheckBox *cbInstallForAllUsers;
  TcxCheckBox *cbInstallHelp;
  TcxCheckListBox *clbPlatforms;
  TdxBevel *dxbSpacer;
  TdxBevel *dxbSpacer2;
  TdxBevel *dxbSpacer3;
  TdxBevel *dxbSpacer4;
  TcxImageCollection *icFeatures;
  TcxImageCollectionItem *icFeaturesItem1;
  TcxImageCollectionItem *icFeaturesItem2;
  TcxImageCollectionItem *icFeaturesItem3;
  TcxImageCollectionItem *icStepItem;
  TcxImageCollection *icWatermark;
  TcxImageCollectionItem *icWatermarkItem;
  TdxImageSlider *isFeatures;
  TcxLabel *lbActionBack;
  TcxLabel *lbActionCancel;
  TcxLabel *lbActionHint;
  TcxLabel *lbActionTitle;
  TcxLabel *lbDemoDescription;
  TcxLabel *lbFinishText;
  TcxLabel *lbInstallingItem;
  TcxLabel *lbModifyDescription;
  TcxLabel *lbProcessNext;
  TcxLabel *lbRemoveDescription;
  TcxLabel *lbRepairDescription;
  TcxLabel *lbSeparator;
  TcxLabel *lbWelcomeText;
  TcxLookAndFeelController *LookAndFeelController;
  TcxMemo *mmEULA;
  TcxProgressBar *pbInstallationProgress;
  TcxRadioButton *rbModify;
  TcxRadioButton *rbRemove;
  TcxRadioButton *rbRepair;
  TdxShellBreadcrumbEdit *sbcPath;
  TcxShellTreeView *stvTree;
  TTimer *tmFeaturesSlider;
  TTimer *tmProgress;
  TdxWizardControl *wcMain;
  TdxWizardControlPage *wcpActionPage;
  TdxWizardControlPage *wcpEULAPage;
  TdxWizardControlPage *wcpFinishPage;
  TdxWizardControlPage *wcpInProgressPage;
  TdxWizardControlPage *wcpPathPage;
  TdxWizardControlPage *wcpReadyToInstall;
  TdxWizardControlPage *wcpSelectionPage;
  TdxWizardControlPage *wcpWelcomePage;
  void __fastcall cbAcceptEULAClick(TObject *Sender);
  void __fastcall clbPlatformsClickCheck(TObject *Sender, int AIndex, TcxCheckBoxState *APrevState, TcxCheckBoxState *ANewState);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall rbModifyClick(TObject *Sender);
  void __fastcall tmFeaturesSliderTimer(TObject *Sender);
  void __fastcall tmProgressTimer(TObject *Sender);
  void __fastcall wcMainButtonClick(TObject *Sender, TdxWizardControlButtonKind AKind, bool &AHandled);
  void __fastcall wcMainPageChanging(TObject *Sender, TdxWizardControlCustomPage *NewPage, bool &AllowChange);
  void __fastcall wcMainCustomButtons0Click(TObject *Sender);
private:	// User declarations
  TStringList *FInstallingItems;
  TInstallMode GetInstallMode();
  bool GetIsPlatformsSelected();
  void PrepareEULAPage();
  void PrepareFinishPage();
  void PopulateInstallingItems();
  void PrepareInProgressPage();
  void PopulateSteps(TList *ASteps);
  void DrawSteps(TList *ASteps, TcxBitmap *AWatermark, TdxWizardControlCustomPage *ACurPage);
  void PreparePageWatermark(TdxWizardControlCustomPage *ACurPage);
  void PreparePathPage();
  void PrepareReadyToInstallPage();
  void PrepareSelectionPage();
  void UpdateButtonsState();
  void SetupAeroStyleFormIcon();
  void SetupAeroStyle();
  void PlaceInfoLabels();
public:		// User declarations
  __fastcall TfrmWizardControlDemoMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmWizardControlDemoMain *frmWizardControlDemoMain;
//---------------------------------------------------------------------------
#endif

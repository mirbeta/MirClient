//---------------------------------------------------------------------------

#ifndef SampleDockingMainH
#define SampleDockingMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "dxStatusBar.hpp"
#include "dxBar.hpp"
#include "dxBarExtItems.hpp"
#include "dxDockControl.hpp"
#include <ActnList.hpp>
#include <ImgList.hpp>
#include "dxDockPanel.hpp"
#include "cxGraphics.hpp"
#include "EBarsUtils.h"
#include "cxPC.hpp"
#include "cxClasses.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
//---------------------------------------------------------------------------
const int ListBoxFrame = 0;
const int TreeViewFrame = 1;
const int RadioGroupFrame = 2;
const int RichTextFrame = 3;

struct TPersistInfo {
  TWinControl* WinControl;
  int SpecInfo;
};
typedef TPersistInfo *PPersistInfo;

struct TRichEditSelInfo {
  int SelStart;
  int SelLength;
};
typedef TRichEditSelInfo *PRichEditSelInfo;

class TSampleDockingMainForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDescription;
  TdxDockSite *DockSite1;
  TdxDockingManager *dxDockingManager;
  TdxBarManager *BarManager;
  TdxBarButton *dxBarButtonExit;
  TdxBarLargeButton *dxBarButton1;
  TdxBarLargeButton *dxBarButton2;
  TdxBarLargeButton *dxBarButton3;
  TdxBarLargeButton *dxBarButton4;
  TdxBarLargeButton *dxBarButton5;
  TdxBarLargeButton *dxBarButton6;
  TdxBarLargeButton *dxBarButton7;
  TdxBarSubItem *dxBarSubItem1;
  TdxBarSubItem *dxBarSubItem2;
  TdxBarSubItem *dxBarSubItem3;
  TdxBarButton *dxBarButton8;
  TdxBarButton *dxBarButton9;
  TdxBarButton *dxBarButton10;
  TdxBarButton *dxBarButton11;
  TdxBarButton *dxBarButton12;
  TdxBarSubItem *dxBarSubItem4;
  TdxBarButton *dxBarButton18;
  TdxBarButton *dxBarButton19;
  TdxBarButton *dxBarButton20;
  TdxBarButton *dxBarButtonDockable;
  TdxBarButton *dxBarButton22;
  TdxBarButton *dxBarButtonAutoHide;
  TdxBarButton *dxBarButtonFloating;
  TdxBarLargeButton *dxBarLargeButton1;
  TdxBarLargeButton *dxBarLargeButton2;
  TImageList *imBarIcons;
  TdxBarPopupMenu *dxBarPopupMenu;
  TImageList *ilDockPanels;
  TImageList *ilDisabledImages;
  TImageList *ilHotImages;
  TdxStatusBar *dxStatusBar;
  TdxBarListItem *dxbLookAndFeelKinds;
  TdxBarButton *dxbNativeStyle;
  TcxLookAndFeelController *cxLookAndFeelController;
  TdxBarSubItem *dxBarDockStyle;
  TdxBarButton *dxBarDockStyleStandard;
  TdxBarButton *dxBarDockStyleVS2005;
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ShowDockControl(TdxDockSite *Sender,
          TdxCustomDockControl *AControl);
  void __fastcall AutoHideChanged(TdxCustomDockControl *Sender);
  void __fastcall AutoHideChanging(TdxCustomDockControl *Sender);
  void __fastcall dpContextPopup(TObject *Sender, const TPoint &MousePos,
          bool &Handled);
  void __fastcall SchemeClick(TObject *Sender);
  void __fastcall dxBarButton18Click(TObject *Sender);
  void __fastcall dxBarButton19Click(TObject *Sender);
  void __fastcall dxBarButton12Click(TObject *Sender);
  void __fastcall HideDockControl(TdxDockSite *Sender,
          TdxCustomDockControl *AControl);
  void __fastcall dxBarButtonDockableClick(TObject *Sender);
  void __fastcall dxBarButton22Click(TObject *Sender);
  void __fastcall dxBarButtonAutoHideClick(TObject *Sender);
  void __fastcall dxBarButtonFloatingClick(TObject *Sender);
  void __fastcall dxBarButtonExitClick(TObject *Sender);
  void __fastcall dxbLookAndFeelKindsClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall dxbNativeStyleClick(TObject *Sender);
  void __fastcall dxBarDockStyleStandardClick(TObject *Sender);
private:	// User declarations
  TdxCustomDockControl* FPopupMenuDockControl;
  void __fastcall StartDock(TdxCustomDockControl *Sender, int X, int Y);
  void __fastcall CreateTabContainer(TdxCustomDockControl *Sender,
          TdxTabContainerDockSite *ATabContainer);
  void __fastcall CreateSiteContainer(TdxCustomDockControl *Sender, TdxSideContainerDockSite *ASideContainer);
  void __fastcall CreateFloatSite(TdxCustomDockControl *Sender, TdxFloatDockSite *AFloatSite);
  void __fastcall EndDock(TdxCustomDockControl *Sender, TdxZone *Zone, int X, int Y);
  int FTreeViewFrameCount, FDockingRichTextFrameCount, FRadioGroupFrameCount, FListBoxFrameCount;
  void __fastcall HookupEvents(TdxCustomDockControl* Sender);
  void __fastcall ClearContent();
  void __fastcall SetPanelsVisibility(bool AVisible);
  void __fastcall CreateScheme1();
  void __fastcall CreateScheme2();
  void __fastcall CreateScheme3();
  void __fastcall CreateScheme4();
  void __fastcall CreateScheme5();
  void __fastcall CreateFrame(int AFrameClassID, TdxDockPanel* AOwner);
  void __fastcall UpdateLookAndFeelMenu();
public:		// User declarations
  __fastcall TSampleDockingMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSampleDockingMainForm *SampleDockingMainForm;
//---------------------------------------------------------------------------
#endif

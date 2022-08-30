//---------------------------------------------------------------------------

#ifndef DockingMegaDemoMainH
#define DockingMegaDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxBar.hpp"
#include "dxDockControl.hpp"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include "dxDockPanel.hpp"
#include "dxBarExtItems.hpp"
#include "cxPC.hpp"
#include "cxLookAndFeels.hpp" 
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include "EBarsUtils.h"
#include "cxClasses.hpp"
#include "dxNavBar.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
//---------------------------------------------------------------------------
class TDockingMegaDemoMainForm : public TForm
{
__published:	// IDE-managed Components
  TdxDockSite *dsHost;
  TdxLayoutDockSite *dxLayoutDockSite4;
  TdxLayoutDockSite *dxLayoutDockSite2;
  TdxLayoutDockSite *dxLayoutDockSite1;
  TdxLayoutDockSite *dxLayoutDockSite3;
  TdxDockPanel *dpStartPage;
  TPanel *Panel2;
  TImage *Image2;
  TPanel *Panel3;
  TLabel *Label4;
  TImage *Image3;
  TImage *Image4;
  TLabel *Label5;
  TPanel *Panel5;
  TImage *Image1;
  TdxVertContainerDockSite *dxVertContainerDockSite1;
  TdxDockPanel *dpProperties;
  TScrollBox *ScrollBox1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TCheckBox *cbManagerColor;
  TCheckBox *cbManagerFont;
  TEdit *eCaption;
  TCheckBox *cbShowCaption;
  TCheckBox *cbShowCloseButton;
  TCheckBox *cbShowMaxButton;
  TCheckBox *cbShowHideButton;
  TCheckBox *cbAllowDockLeft;
  TCheckBox *cbAllowDockTop;
  TCheckBox *cbAllowDockClient;
  TCheckBox *cbAllowFloating;
  TCheckBox *cbAllowDockBottom;
  TCheckBox *cbAllowDockRight;
  TCheckBox *cbAllowDockClientsLeft;
  TCheckBox *cbAllowDockClientsTop;
  TCheckBox *cbAllowDockClientsClient;
  TCheckBox *cbAllowDockClientsBottom;
  TCheckBox *cbAllowDockClientsRight;
  TCheckBox *cbTabsOnTop;
  TCheckBox *cbTabsScrollable;
  TPanel *Panel4;
  TButton *btnApply;
  TButton *btnCancel;
  TdxTabContainerDockSite *dxTabContainerDockSite2;
  TdxDockPanel *dpSolutionExplorer;
  TTreeView *tvSolutionExplorer;
  TdxDockPanel *dpClassView;
  TTreeView *tvClassView;
  TdxTabContainerDockSite *dxTabContainerDockSite1;
  TdxDockPanel *dpOutput;
  TMemo *Memo1;
  TPanel *Panel1;
  TComboBox *ComboBox1;
  TdxDockPanel *dpCallStack;
  TListView *ListView2;
  TdxDockPanel *dpWatch;
  TListView *ListView1;
  TdxDockPanel *dpToolbox;
  TdxDockingManager *dxDockingManager1;
  TdxBarManager *BarManager;
  TdxBarLargeButton *dxBarButtonLoad;
  TdxBarLargeButton *dxBarButtonSave;
  TdxBarSubItem *dxBarSubItemFile;
  TdxBarSubItem *dxBarSubItemInsert;
  TdxBarSubItem *dxBarSubItemFormat;
  TdxBarSubItem *dxBarSubItemWindow;
  TdxBarLargeButton *dxBarButtonExit;
  TdxBarSubItem *dxBarSubItemHelp;
  TdxBarLargeButton *dxBarButtonStartPage;
  TdxBarLargeButton *dxBarButtonToolBox;
  TdxBarLargeButton *dxBarButtonWatch;
  TdxBarLargeButton *dxBarButtonOutput;
  TdxBarLargeButton *dxBarButtonCallStack;
  TdxBarLargeButton *dxBarButtonProperties;
  TdxBarLargeButton *dxBarButtonClassView;
  TdxBarLargeButton *dxBarButtonFont;
  TdxBarLargeButton *dxBarButtonColor;
  TdxBarLargeButton *dxBarButtonSolutionExplorer;
  TdxBarButton *dxBarButtonDockable;
  TdxBarButton *dxBarButtonHide;
  TdxBarButton *dxBarButtonFloating;
  TdxBarButton *dxBarButtonAutoHide;
  TdxBarSubItem *dxBarSubItemOtherWindows;
  TcxLookAndFeelController *LookAndFeelController;
  TdxBarSubItem *dxBarLookAndFeel;
  TdxBarButton *dxbNative;
  TdxBarListItem *dxbLookAndFeelKinds;
  TPanel *Panel6;
  TLabel *lbTabsPosition;
  TComboBox *cbTabsPosition;
  TCheckBox *cbTabsRotate;
  TLabel *lbTabsCloseButton;
  TComboBox *cbCloseButtonMode;
  TCheckBox *cbVisible;  
  TdxBarLargeButton *dxBarButton1;
  TdxBarLargeButton *dxBarButton2;
  TdxBarLargeButton *dxBarButton3;
  TdxBarLargeButton *dxBarButton4;
  TdxBarLargeButton *dxBarButton5;
  TdxBarLargeButton *dxBarButton6;
  TdxBarLargeButton *dxBarButton7;
  TdxBarLargeButton *dxBarLargeButton1;
  TdxBarLargeButton *dxBarLargeButton2;
  TImageList *imBarIcons;
  TColorDialog *ColorDialog1;
  TFontDialog *FontDialog1;
  TImageList *ilDockIcons;
  TdxBarPopupMenu *dxBarPopupMenu1;
  TOpenDialog *OpenDialog1;
  TSaveDialog *SaveDialog1;
  TImageList *ilDisabledImages;
  TImageList *ilHotImages;
  TImageList *iComponentsIcons;
  TdxBarSubItem *dxBarDockStyle;
  TdxBarButton *dxBarDockStyleStandard;
  TdxBarButton *dxBarDockStyleVS2005;
  TdxNavBar *dxNavBar1;
  TdxNavBarItem *dxNavBarItem1;
  TdxNavBarItem *dxNavBarItem2;
  TdxNavBarItem *dxNavBarItem3;
  TdxNavBarItem *dxNavBarItem4;
  TdxNavBarItem *dxNavBarItem5;
  TdxNavBarItem *dxNavBarItem6;
  TdxNavBarItem *dxNavBarItem7;
  TdxNavBarGroup *dxNavBarGroup1;
  TdxNavBarItem *dxNavBarItem8;
  TdxNavBarGroup *dxNavBarGroup2;
  TdxNavBarItem *dxNavBarItem9;
  TdxNavBarItem *dxNavBarItem10;
  TdxNavBarItem *dxNavBarItem11;
  TdxNavBarItem *dxNavBarItem12;
  TdxNavBarItem *dxNavBarItem13;
  TdxNavBarItem *dxNavBarItem14;
  TdxNavBarGroup *dxNavBarGroup3;
  TdxNavBarGroup *dxNavBarGroup4;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall dxBarButtonPageClick(TObject *Sender);
  void __fastcall dxBarButtonLoadClick(TObject *Sender);
  void __fastcall dxBarButtonSaveClick(TObject *Sender);
  void __fastcall dxBarButtonColorClick(TObject *Sender);
  void __fastcall dxBarButtonFontClick(TObject *Sender);
  void __fastcall frStartPage1Label1Click(TObject *Sender);
  void __fastcall frStartPage1Label2Click(TObject *Sender);
  void __fastcall dxBarButtonExitClick(TObject *Sender);
  void __fastcall dxBarButtonDockableClick(TObject *Sender);
  void __fastcall dxBarButtonHideClick(TObject *Sender);
  void __fastcall dxBarButtonFloatingClick(TObject *Sender);
  void __fastcall dxBarButtonAutoHideClick(TObject *Sender);
  void __fastcall dpContextPopup(TObject *Sender, const TPoint &MousePos, bool &Handled);
  void __fastcall btnApplyClick(TObject *Sender);
  void __fastcall btnCancelClick(TObject *Sender);
  void __fastcall tvSolutionExplorerChange(TObject *Sender, TTreeNode *Node);
  void __fastcall dxDockingManager1LayoutChanged(TdxCustomDockControl *Sender);
  void __fastcall dxBarButtonStartPageClick(TObject *Sender);
  void __fastcall dxBarButtonSolutionExplorerClick(TObject *Sender);
  void __fastcall dxBarButtonClassViewClick(TObject *Sender);
  void __fastcall dxBarButtonPropertiesClick(TObject *Sender);
  void __fastcall dxBarButtonToolBoxClick(TObject *Sender);
  void __fastcall dxBarButtonCallStackClick(TObject *Sender);
  void __fastcall dxBarButtonOutputClick(TObject *Sender);
  void __fastcall dxBarButtonWatchClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall dxbLookAndFeelKindClick(TObject *Sender);
  void __fastcall dxbNativeClick(TObject *Sender);  
  void __fastcall dxBarDockStyleVS2005Click(TObject *Sender);
private:	// User declarations
  TdxCustomDockControl* FPopupMenuDockControl;
  TdxCustomDockControl* FDockControl;
  void __fastcall SetDockControl(TdxCustomDockControl* Value);
  void __fastcall SetEnableState(TWinControl *AContainer, Boolean AValue);
public:		// User declarations
  __fastcall TDockingMegaDemoMainForm(TComponent* Owner);
  void __fastcall UpdateSolutionTreeView();
  void __fastcall UpdateClassViewTreeView();
  void __fastcall UpdateLookAndFeelMenu();
  void __fastcall CheckSite(TdxCustomDockControl* AControl, TTreeNode* ANode);
  void __fastcall MakeVisible(TdxDockPanel* AControl);
  void __fastcall UpdateProperties();
  void __fastcall ApplyProperties();
  __property TdxCustomDockControl* DockControl={read=FDockControl, write=SetDockControl};
};
//---------------------------------------------------------------------------
extern PACKAGE TDockingMegaDemoMainForm *DockingMegaDemoMainForm;
//---------------------------------------------------------------------------
#endif
 
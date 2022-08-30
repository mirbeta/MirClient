//---------------------------------------------------------------------------
#ifndef FeaturesMainH
#define FeaturesMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <StdLib.h>
#include <Forms.hpp>
#include "dxNavBar.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
#include "dxNavBarStyles.hpp"
#include "dxNavBarViewsFact.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
#include "NavBarUtils.h"
//---------------------------------------------------------------------------
class TfmFeaturesMain : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label9;
        TdxNavBar *nbMain;
        TdxNavBarGroup *bgNews;
        TdxNavBarItem *biInbox;
        TdxNavBarItem *biOutbox;
        TdxNavBarItem *biSentItems;
        TdxNavBarItem *biDeletedItems;
        TdxNavBarItem *biDrafts;
        TdxNavBarItem *biNews;
        TdxNavBarStyleItem *stThirdGroupBackGround;
        TdxNavBarStyleItem *stThirdGroupHeader;
        TdxNavBarStyleItem *stThirdGroupHeaderHotTracked;
        TdxNavBarStyleItem *stThirdGroupHeaderPressed;
        TPanel *Panel1;
        TPanel *pnlHierarchy;
        TLabel *lbNavBarHierarchy;
        TTreeView *tvNavBar;
        TPanel *pnlNavBarActions;
        TButton *btAddGroup;
        TButton *btDeleteGroup;
        TButton *btAddLink;
        TButton *btDeleteLink;
        TGroupBox *gbProperties;
        TPageControl *pcProperties;
        TTabSheet *tsSelectedGroupProps;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TCheckBox *cbGExpanded;
        TCheckBox *cbGVisible;
        TCheckBox *cbGLinkUseSmallImages;
        TCheckBox *cbGUseSmallImages;
        TEdit *eGCaption;
        TCheckBox *cbGShowAsIconView;
        TComboBox *cbGSmallImageIndex;
        TComboBox *cbGLargeImageIndex;
        TTabSheet *tsSelectedItemProps;
        TLabel *Label6;
        TLabel *Label7;
        TLabel *Label8;
        TEdit *eICaption;
        TCheckBox *cbIEnabled;
        TCheckBox *cbIVisible;
        TComboBox *cbILargeImageIndex;
        TComboBox *cbISmallImageIndex;
        TImageList *imgSmall;
        TImageList *imgLarge;
        TMainMenu *mmMain;
        TMenuItem *miFile;
        TMenuItem *miExit;
        TMenuItem *miOptions;
        TMenuItem *miOptionsView;
        TMenuItem *miLookAndFeel;
        TMenuItem *miLookAndFeelItem;
        TMenuItem *miShowCaptions;
        TMenuItem *miShowSpecialGroup;
        TMenuItem *miOptionsBehavour;
        TMenuItem *miAllowSelectLinks;
        TMenuItem *miEachGroupHasSelectedLink;
        TMenuItem *miShowGroupHints;
        TMenuItem *miShowLinkHints;
        TMenuItem *miHelp;
        TMenuItem *miProducts;
        TMenuItem *miDownloads;
        TMenuItem *miDeveloperExpressontheweb;
        TActionList *alMain;
        TAction *actShowCaptions;
        TAction *actShowSpecialGroup;
        TAction *actAllowSelectLinks;
        TAction *actEachGroupHasSelectedLink;
        TAction *actShowGroupHints;
        TAction *actShowLinkHints;
        TPopupMenu *pmnuItems;
        TMenuItem *pmnuItem;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall btAddGroupClick(TObject *Sender);
        void __fastcall btDeleteGroupClick(TObject *Sender);
        void __fastcall btAddLinkClick(TObject *Sender);
        void __fastcall btDeleteLinkClick(TObject *Sender);
        void __fastcall FormActivate(TObject *Sender);
        void __fastcall nbMainEndDrag(TObject *Sender, TObject *Target,
          int X, int Y);
        void __fastcall miLookAndFeelItemClick(TObject *Sender);
        void __fastcall actShowCaptionsExecute(TObject *Sender);
        void __fastcall actShowSpecialGroupExecute(TObject *Sender);
        void __fastcall actAllowSelectLinksExecute(TObject *Sender);
        void __fastcall actEachGroupHasSelectedLinkExecute(TObject *Sender);
        void __fastcall actShowGroupHintsExecute(TObject *Sender);
        void __fastcall actShowLinkHintsExecute(TObject *Sender);
        void __fastcall pmnuItemClick(TObject *Sender);
        void __fastcall pmnuItemsPopup(TObject *Sender);
        void __fastcall tvNavBarChange(TObject *Sender, TTreeNode *Node);
        void __fastcall cbGExpandedClick(TObject *Sender);
        void __fastcall cbGVisibleClick(TObject *Sender);
        void __fastcall cbGShowAsIconViewClick(TObject *Sender);
        void __fastcall cbGLinkUseSmallImagesClick(TObject *Sender);
        void __fastcall cbGUseSmallImagesClick(TObject *Sender);
        void __fastcall eGCaptionChange(TObject *Sender);
        void __fastcall cbGSmallImageIndexChange(TObject *Sender);
        void __fastcall cbGLargeImageIndexChange(TObject *Sender);
        void __fastcall cbIEnabledClick(TObject *Sender);
        void __fastcall cbIVisibleClick(TObject *Sender);
        void __fastcall eICaptionChange(TObject *Sender);
        void __fastcall cbISmallImageIndexChange(TObject *Sender);
        void __fastcall cbILargeImageIndexChange(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall NavBarItemClick(TObject *Sender);
        void __fastcall nbMainActiveGroupChanged(TObject *Sender);
        void __fastcall nbMainLinkClick(TObject *Sender,
          TdxNavBarItemLink *ALink);
private:	// User declarations
        void AddDropDownMenuItem(TMenuItem *AMenuItems, String ACaption,
          int AImageIndex, bool ARadioItem, TNotifyEvent AClickHandler);
        void HidePropertiesTabSheet(TTabSheet *ATabSheet);
        void InitImageIndexComoboBoxes();
        void SelectDefaultGroup();
        void SetNodeImageIndex(TTreeNode *ANode, int AImageIndex);
        void ShowPropertiesTabSheet(TTabSheet *ATabSheet);
        void UpdateGroupProperties();
        void UpdateItemProperties();
        void UpdateItemsDropDownMenu();
        void UpdateGroupPropertiesState();
        void UpdateItemPropertiesState();
        void UpdateTreeView();
        TdxNavBarGroup* GetCurrentGroup();
        TdxNavBarItem* GetCurrentItem();
        TdxNavBarItemLink* GetCurrentLink();
public:		// User declarations
        __fastcall TfmFeaturesMain(TComponent* Owner);
	__property TdxNavBarGroup* CurrentGroup = {read=GetCurrentGroup, nodefault};
	__property TdxNavBarItem* CurrentItem = {read=GetCurrentItem, nodefault};
	__property TdxNavBarItemLink* CurrentLink = {read=GetCurrentLink, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TfmFeaturesMain *fmFeaturesMain;
//---------------------------------------------------------------------------
#endif

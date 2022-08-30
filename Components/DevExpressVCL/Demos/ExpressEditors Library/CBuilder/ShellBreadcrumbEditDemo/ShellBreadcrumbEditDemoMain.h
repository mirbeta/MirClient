//---------------------------------------------------------------------------

#ifndef ShellBreadcrumbEditDemoMainH
#define ShellBreadcrumbEditDemoMainH
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp> 
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ShlObj.hpp>
#include <ShellAPI.hpp>
#include <Menus.hpp>
#include <Buttons.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <ActnList.hpp>
#include <Buttons.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>

#include "cxGraphics.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "dxBreadcrumbEdit.hpp"
#include "dxShellBreadcrumbEdit.hpp"
#include "cxShellTreeView.hpp"
#include "cxSplitter.hpp"
#include "cxShellControls.hpp"
#include "cxShellListView.hpp"
#include "dxGDIPlusClasses.hpp"
#include "cxButtons.hpp"
#include "cxListBox.hpp"
#include "cxShellCommon.hpp"

//---------------------------------------------------------------------------
class TdxBreadcrumbEditDemoForm : public TForm
{
__published:  // IDE-managed Components
    TAction *acBrowseParent;
    TAction *acRefresh;
    TActionList *ActionList1;
    TdxShellBreadcrumbEdit *bceAddressBar;
    TBevel *bvTopSpacer;
    TcxEditStyleController *edStyleController;
    TcxLookAndFeelController *cxLookAndFeelController;
    TcxImageList *ilImages;
    TImage *imStatusBar;
    TImage *imToolBar;
    TLabel *lbInfo;
    TLabel *lbName;
    TcxShellListView *lvFiles;
    TMenuItem *miViewDetails;
    TMenuItem *miViewExtraLargeIcons;
    TMenuItem *miViewIcons;
    TMenuItem *miViewLargeIcons;
    TMenuItem *miViewList;
    TMenuItem *miViewSmallIcons;
    TPaintBox *pbSelectedItemIcon;
    TPopupMenu *pmView;
    TPanel *pnlAddressBarContainer;
    TPanel *pnlStatusBar;
    TPanel *pnlToolBar;
    TSpeedButton *sbHelp;
    TSpeedButton *sbView;
    TcxSplitter *sltFolderTree;
    TcxShellTreeView *tvFolders;
    TAction *acAbout;
    void __fastcall acBrowseParentExecute(TObject *Sender);
    void __fastcall acRefreshExecute(TObject *Sender);
    void __fastcall bceAddressBarPathSelected(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
	void __fastcall lvFilesCurrentFolderChanged(TcxCustomShellListView *Sender);
    void __fastcall lvFilesSelectItem(TObject *Sender, TListItem *Item, Boolean Selected);
    void __fastcall miViewDetailsClick(TObject *Sender);
    void __fastcall pbSelectedItemIconPaint(TObject *Sender);
    void __fastcall sbViewClick(TObject *Sender);
    void __fastcall acAboutExecute(TObject *Sender);
private:
    TImageList *ShellLargeImages;
	void GetItemInfo(PItemIDList APidl, TSHFileInfo &AFileInfo, Boolean ADisposePidl = false);
	void GetItemInfoByItemIndex(int AIndex, TSHFileInfo &AFileInfo);
	void InitializeLookAndFeel();
	void InitializeShellLargeImages();
	void UpdateCaption();
	void UpdateSelectionInfo();
	void SetupIconsView(int AThumbnailSize);
public:   // User declarations
  __fastcall TdxBreadcrumbEditDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxBreadcrumbEditDemoForm *dxBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
#endif


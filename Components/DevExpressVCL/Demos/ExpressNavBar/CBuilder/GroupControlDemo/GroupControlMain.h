//---------------------------------------------------------------------------
#ifndef GroupControlMainH
#define GroupControlMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxNavBar.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <FileCtrl.hpp>
#include <ShellAPI.h>
#include <ToolWin.hpp>
#include <comObj.hpp>
#include <shlobj.hpp>
//---------------------------------------------------------------------------

//ShellItem Struct
struct PShellItem
{
        public:
                //Variables
                PItemIDList FullID, ParentID, ID;
                LPSHELLFOLDER ShellFolder;
                bool Empty;
                AnsiString DisplayName, TypeName, ModDate;
                int ImageIndex, Size;
                DWORD Attributes;
                //Constructor
                PShellItem();
};

// Function Declarations
int __fastcall ListSortFunc(void * Item1, void * Item2);
int __fastcall GetShellImage(PItemIDList PIDL, bool Large, bool Open);
int __fastcall GetPIDLSize(PItemIDList IDList);
bool IsFolder(LPSHELLFOLDER ShellFolder, PItemIDList ID);
bool __fastcall ValidFileTime(TFileTime FileTime);
bool IsFolder(PShellItem * ShellItem);
void __fastcall DisposePIDL(PItemIDList ID);
PItemIDList __fastcall CopyITEMID(LPMALLOC Malloc, PItemIDList ID);
PItemIDList __fastcall NextPIDL(PItemIDList IDList);
PItemIDList __fastcall CreatePIDL(int Size);
PItemIDList __fastcall CopyPIDL(PItemIDList IDList);
PItemIDList __fastcall StripLastID(PItemIDList IDList);
PItemIDList __fastcall ConcatPIDLs(PItemIDList IDList1, PItemIDList IDList2);
AnsiString __fastcall GetDisplayName(LPSHELLFOLDER ShellFolder, PItemIDList PIDL,
                        bool ForParsing);


class TfmGroupControlMain : public TForm
{
__published:	// IDE-managed Components
        TSplitter *Splitter1;
        TListView *lvMain;
        TCoolBar *CoolBar1;
        TToolBar *ToolBar2;
        TToolButton *btnBack;
        TToolButton *ToolButton3;
        TToolButton *btnLargeIcons;
        TToolButton *btnSmallIcons;
        TToolButton *btnList;
        TToolButton *btnReport;
        TdxNavBar *nbMain;
        TdxNavBarGroup *bgMyComputer;
        TdxNavBarGroup *bgPlaces;
        TdxNavBarGroup *bgFavorites;
        TdxNavBarGroup *bgSearch;
        TdxNavBarItem *biDesktop;
        TdxNavBarItem *biMyDocuments;
        TdxNavBarItem *biNetwork;
        TdxNavBarGroupControl *bgSearchControl;
        TButton *btnSearch;
        TEdit *edSearch;
        TdxNavBarGroupControl *bgMyComputerControl;
        TTreeView *tvMyComputer;
        TdxNavBarGroupControl *bgFavoritesControl;
        TListView *lvMyFavorites;
        TStatusBar *StatusBar1;
        TImageList *ToolbarImages;
        TImageList *ilSmall;
        TImageList *ilLarge;
        TPanel *Panel1;
        TComboBox *cbViews;
        TToolButton *ToolButton1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall lvMainCustomDrawItem(TCustomListView *Sender,
          TListItem *Item, TCustomDrawState State, bool &DefaultDraw);
        void __fastcall lvMainCustomDrawSubItem(TCustomListView *Sender,
          TListItem *Item, int SubItem, TCustomDrawState State,
          bool &DefaultDraw);
        void __fastcall lvMainData(TObject *Sender, TListItem *Item);
        void __fastcall lvMainDataFind(TObject *Sender, TItemFind Find,
          const String FindString, const TPoint &FindPosition,
          Pointer FindData, int StartIndex, TSearchDirection Direction,
          bool Wrap, int &Index);
        void __fastcall lvMainDataHint(TObject *Sender, int StartIndex,
          int EndIndex);
        void __fastcall lvMainDblClick(TObject *Sender);
        void __fastcall lvMainKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
        void __fastcall btnBackClick(TObject *Sender);
        void __fastcall btnLargeIconsClick(TObject *Sender);
        void __fastcall biDesktopClick(TObject *Sender);
        void __fastcall biMyDocumentsClick(TObject *Sender);
        void __fastcall biNetworkClick(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall Form1Close(TObject *Sender, TCloseAction &Action);
        void __fastcall btnSearchClick(TObject *Sender);
        void __fastcall cbViewsChange(TObject *Sender);
        void __fastcall lvMyFavoritesChange(TObject *Sender, TListItem *Item, TItemChange Change);
        void __fastcall tvMyComputerChange(TObject *Sender, TTreeNode *Node);
private:	// User declarations
        TList * FIDList;
        PItemIDList FSearchShellID;
        PItemIDList FShellID;
        LPSHELLFOLDER FDesktopFolder;
        bool FSearching;

        LPSHELLFOLDER __fastcall GetShellFolder();
        Integer __fastcall GetShellItemCount();
        PShellItem* __fastcall GetShellItem(int Index);

        void __fastcall ClearIDList();
protected:
        PItemIDList __fastcall GetIDByPath(AnsiString APath);
        PItemIDList __fastcall GetIDBySpetialFolder(Integer ASpetialFolder);
        LPSHELLFOLDER __fastcall GetShellFolderByID(PItemIDList AID);
        LPENUMIDLIST __fastcall GetEnumIDListByFolder(LPSHELLFOLDER AFolder);

        bool __fastcall CompareNames(AnsiString Path, AnsiString Pattern);
        void __fastcall SetSearch(PItemIDList AID, AnsiString Pattern);
        void __fastcall SetPath(const AnsiString Value);
        void __fastcall SetPath(LPITEMIDLIST ID);
        void __fastcall PopulateIDList(PItemIDList AID);
        void __fastcall PopulateSearchIDList(PItemIDList ASearchID, AnsiString Pattern);
        void __fastcall PopulateMyFavoritesList(PItemIDList AID);
        void __fastcall PopulateMyComputerTree(PItemIDList AID);

        void __fastcall CheckFolder(PItemIDList AID, AnsiString Pattern);
        void __fastcall CheckShellItems(int StartIndex, int EndIndex);

public:		// User declarations
        __fastcall TfmGroupControlMain(TComponent* Owner);

        __property LPSHELLFOLDER DesktopFolder = { read = FDesktopFolder };
        __property PItemIDList SearchShellID = { read = FSearchShellID };
        __property LPSHELLFOLDER ShellFolder = { read = GetShellFolder };
        __property PItemIDList ShellID = { read = FShellID };

        __property PShellItem* ShellItems[int Index] = { read = GetShellItem };
        __property int ShellItemCount = { read = GetShellItemCount };

};
//---------------------------------------------------------------------------
extern PACKAGE TfmGroupControlMain *fmGroupControlMain;
//---------------------------------------------------------------------------
#endif

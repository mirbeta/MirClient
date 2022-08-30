//---------------------------------------------------------------------------
#ifndef Office12ViewsMainH
#define Office12ViewsMainH
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
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include <ActnList.hpp>
#include "dxNavBarGroupItems.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxTextEdit.hpp"
#include "dxNavBarOfficeNavigationBar.hpp"
#include <Menus.hpp>
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
AnsiString __fastcall GetDisplayName(LPSHELLFOLDER ShellFolder, PItemIDList PIDL, bool ForParsing);


class TfmMain : public TForm
{
__published:	// IDE-managed Components
		TdxNavBarItem *nbMainDesktop;
		TdxNavBarItem *nbMainMyDocuments;
		TdxNavBarItem *nbMainNetwork;
		TdxNavBarGroup *bgColorScheme;
		TdxNavBar *nbMyComputer;
		TdxNavBarGroup *nbMyComputerGroup1;
		TdxNavBarGroup *nbMyComputerGroup2;
		TdxNavBarGroupControl *nbMyComputerGroup2Control;
		TImageList *ilMainSmall;
		TImageList *ilMainLarge;
		TdxNavBarItem *nbMainBlue;
		TdxNavBarItem *nbMainBlack;
		TdxNavBarItem *nbMainSilver;
		TdxNavBarGroup *bgOptions;
		TdxNavBarGroupControl *bgOptionsControl;
		TdxNavBar *nbOptions;
		TdxNavBarGroup *nbOptionsListOptions;
		TdxNavBarGroup *nbOptionsNavBarOptions;
		TdxNavBarItem *nbOptionsLargeIcons;
		TdxNavBarItem *nbOptionsSmallIcons;
		TdxNavBarItem *nbOptionsList;
		TdxNavBarItem *nbOptionsReport;
		TdxNavBarItem *nbMainLargeIcons;
		TdxNavBarItem *nbMainSmallIcons;
		TdxNavBarItem *nbMainList;
		TdxNavBarItem *nbMainReport;
		TdxNavBarItem *nbOptionsAdjustWidthByPopup;
		TdxNavBarItem *nbOptionsCollapsible;
		TdxNavBarItem *nbOptionsAllowCustomize;
		TdxNavBarItem *nbMyComputerDesktop;
		TdxNavBarItem *nbMyComputerMyDocuments;
		TdxNavBarItem *nbMyComputerMyNetworkPlaces;
                TdxNavBarSeparator *nbMyComputerSeparator1;
		TdxNavBarItem *nbMyComputerUp;
		TdxNavBarItem *nbMainUp;
		TdxNavBarItem *nbOptionsTabStop;
		TLabel *Label9;

		TSplitter *Splitter1;
		TListView *lvMain;
		TdxNavBar *nbMain;
		TdxNavBarGroup *bgMyComputer;
		TdxNavBarGroup *bgFavorites;
		TdxNavBarGroup *bgSearch;
		TdxNavBarGroupControl *bgSearchControl;
		TButton *btnSearch;
		TcxTextEdit *edSearch;
		TdxNavBarGroupControl *bgMyComputerControl;
		TTreeView *tvMyComputer;
		TdxNavBarGroupControl *bgFavoritesControl;
		TListView *lvMyFavorites;
		TStatusBar *StatusBar1;
		TcxImageList *ilSmall;
		TcxImageList *ilLarge;
	TdxNavBar *dxNavBar2;
	TdxNavBarGroup *dxNavBarGroup3;
	TdxNavBarItem *dxNavBarItem9;
	TdxNavBarItem *dxNavBarItem10;
	TdxNavBarItem *dxNavBarItem11;
	TdxNavBar *dxNavBar1;
	TdxNavBarGroup *dxNavBarGroup1;
	TdxNavBarGroup *dxNavBarGroup2;
	TdxNavBarItem *dxNavBarItem1;
	TdxNavBarItem *dxNavBarItem2;
	TdxNavBarItem *dxNavBarItem3;
	TdxNavBarItem *dxNavBarItem4;
	TdxNavBarItem *dxNavBarItem5;
	TdxNavBarItem *dxNavBarItem6;
	TdxNavBarItem *dxNavBarItem7;
	TdxNavBarItem *dxNavBarItem8;
	TcxGroupBox *cxGroupBox2;
	TcxTextEdit *cxTextEdit1;
	TButton *Button1;
	TMainMenu *MainMenu1;
	TMenuItem *Options1;
	TMenuItem *ShowOfficeNavigationBar1;
	TActionList *ActionList1;
	TAction *actUp;
	TAction *actLargeIcons;
	TAction *actSmallIcons;
	TAction *actList;
	TAction *actReport;
	TAction *actDesktop;
	TAction *actMyDocuments;
	TAction *actMyNetworkPlaces;
	TAction *actAdjustWidthByPopup;
	TAction *actAllowCustomize;
	TAction *actOptionsCollapsible;
	TAction *actOptionsTabStop;
	TAction *actSearch;
	TAction *actBlue;
	TAction *actBlack;
	TAction *actSilver;
	TdxNavBarOfficeNavigationBar *dxNavBarOfficeNavigationBar1;
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
		void __fastcall FormDestroy(TObject *Sender);
		void __fastcall Form1Close(TObject *Sender, TCloseAction &Action);
		void __fastcall btnSearchClick(TObject *Sender);
		void __fastcall lvMyFavoritesClick(TObject *Sender);
		void __fastcall tvMyComputerClick(TObject *Sender);

		void __fastcall nbMainDesktopClick(TObject *Sender);
		void __fastcall nbMainMyDocumentsClick(TObject *Sender);
		void __fastcall nbMainNetworkClick(TObject *Sender);
		void __fastcall actSchemeExecute(TObject *Sender);
		void __fastcall actLargeIconsExecute(TObject *Sender);
		void __fastcall nbOptionsAdjustWidthByPopupClick(TObject *Sender);
		void __fastcall nbOptionsAllowCustomizeClick(TObject *Sender);
		void __fastcall nbOptionsCollapsibleClick(TObject *Sender);
		void __fastcall nbOptionsTabStopClick(TObject *Sender);
	        void __fastcall tvMyComputerAdvancedCustomDraw(TCustomTreeView *Sender,
                  const TRect &ARect, TCustomDrawStage Stage, bool &DefaultDraw);
	        void __fastcall tvMyComputerMouseDown(TObject *Sender, TMouseButton Button,
                  TShiftState Shift, int X, int Y);
	        void __fastcall tvMyComputerMouseMove(TObject *Sender, TShiftState Shift,
                  int X, int Y);
	void __fastcall cxTextEdit1PropertiesChange(TObject *Sender);
	void __fastcall edSearchPropertiesChange(TObject *Sender);
	void __fastcall dxNavBarOfficeNavigationBar1QueryPeekFormContent(TObject *ASender,
          IdxNavigationItem *ANavigationItem, TWinControl *&AControl);
	void __fastcall ShowOfficeNavigationBar1Click(TObject *Sender);

private:	// User declarations
		TList * FIDList;
		PItemIDList FSearchShellID;
		PItemIDList FShellID;
		LPSHELLFOLDER FDesktopFolder;
		bool FSearching;
		bool FLockSearchTextChange;

		LPSHELLFOLDER __fastcall GetShellFolder();
		Integer __fastcall GetShellItemCount();
		PShellItem* __fastcall GetShellItem(int Index);

		void __fastcall ClearIDList();
		bool __fastcall SwitchOption(TObject *Sender, bool AValue);
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
		void __fastcall CloseNavBarPopup();
public:		// User declarations
		__fastcall TfmMain(TComponent* Owner);

		__property LPSHELLFOLDER DesktopFolder = { read = FDesktopFolder };
		__property PItemIDList SearchShellID = { read = FSearchShellID };
        __property LPSHELLFOLDER ShellFolder = { read = GetShellFolder };
        __property PItemIDList ShellID = { read = FShellID };

        __property PShellItem* ShellItems[int Index] = { read = GetShellItem };
        __property int ShellItemCount = { read = GetShellItemCount };

};
//---------------------------------------------------------------------------
extern PACKAGE TfmMain *fmMain;
//---------------------------------------------------------------------------
#endif

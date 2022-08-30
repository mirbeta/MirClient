//---------------------------------------------------------------------------
#ifndef DragDropMainH
#define DragDropMainH
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
#include <Graphics.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
#include "NavBarUtils.h"
//---------------------------------------------------------------------------
class TfmDragDropMain : public TForm
{
__published:  // IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TdxNavBar *nbMain;
        TdxNavBarGroup *bgStandard;
        TdxNavBarGroup *bgSystem;
        TdxNavBarGroup *bgDX;
        TdxNavBarGroup *bgTemp;
        TdxNavBarItem *biLabel;
        TdxNavBarItem *biEdit;
        TdxNavBarItem *biButton;
        TdxNavBarItem *biCheckBox;
        TdxNavBarItem *biRadioButton;
        TdxNavBarItem *biGroupBox;
        TdxNavBarItem *biPanel;
        TdxNavBarItem *biImage;
        TdxNavBarItem *biMainMenu;
        TdxNavBarItem *biTimer;
        TdxNavBarItem *biGrid;
        TdxNavBarItem *biTreeList;
        TdxNavBarItem *biBarManager;
        TdxNavBarItem *biDBNavigatorBar;
        TdxNavBarItem *biCalcEdit;
        TdxNavBarItem *biButtonEdit;
        TdxNavBarItem *biNavBar;
        TListView *lvComponents;
        TPanel *Panel1;
        TImage *imRecycleBin;
        TImageList *iComponents;
        TMainMenu *mmMain;
        TMenuItem *miFile;
        TMenuItem *miExit;
        TMenuItem *miDragDropOptions;
        TMenuItem *miDragAndDrop;
        TMenuItem *miAllowDragLink;
        TMenuItem *miAllowDropLink;
        TMenuItem *miAllowDragGroup;
        TMenuItem *miAllowDropGroup;
        TMenuItem *SelectLinks1;
        TMenuItem *miHelp;
        TMenuItem *miProducts;
        TMenuItem *miDownloads;
        TMenuItem *miSupport;
        TMenuItem *miDeveloperExpressontheweb;
        TActionList *alMain;
        TAction *actDragLink;
        TAction *actDropLink;
        TAction *actDragGroup;
        TAction *actDropGroup;
        TAction *actSelectLinks;
        void __fastcall lvComponentsStartDrag(TObject *Sender,
          TDragObject *&DragObject);
        void __fastcall lvComponentsEndDrag(TObject *Sender, 
          TObject *Target, int X, int Y);
        void __fastcall imRecycleBinDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
        void __fastcall nbMainEndDrag(TObject *Sender, TObject *Target,
          int X, int Y);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall actDragDropOptionExecute(TObject *Sender);
        void __fastcall actSelectLinksExecute(TObject *Sender);
private:  // User declarations
public:   // User declarations
        __fastcall TfmDragDropMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmDragDropMain *fmDragDropMain;
//---------------------------------------------------------------------------
#endif

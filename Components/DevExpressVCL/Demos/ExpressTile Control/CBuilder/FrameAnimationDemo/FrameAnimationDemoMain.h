//---------------------------------------------------------------------------

#ifndef FrameAnimationDemoMainH
#define FrameAnimationDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxCustomTileControl.hpp"
#include "dxSkinsCore.hpp"
#include "dxTileControl.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfmFrameAnimationMain : public TForm
{
__published:	// IDE-managed Components
	TdxTileControl *tcMain;
	TdxTileControlGroup *dxTiledxTileControlGroup1;
	TdxTileControlGroup *dxTiledxTileControlGroup2;
	TdxTileControlItem *tiHouses;
	TdxTileControlItem *tiInteriors;
	TdxTileControlItem *tiAgents;
	TMainMenu *mmMain;
	TMenuItem *miFile;
	TMenuItem *miExit;
	TMenuItem *miOptions;
	TMenuItem *miScrollMode;
	TMenuItem *miCenterContent;
	TMenuItem *miCenterContentHorz;
	TMenuItem *miCenterContentVert;
	TMenuItem *miHelp;
	TPopupMenu *pmItemAnimate;
	TMenuItem *pmAnimationInterval;
	TMenuItem *pmAnimationMode;
	TMenuItem *pmAnimateText;
	TcxLookAndFeelController *cxLookAndFeelController;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall miExitClick(TObject *Sender);
	void __fastcall miCenterContentClick(TObject *Sender);
	void __fastcall tcMainContextPopup(TObject *Sender, TPoint &MousePos,
		  bool &Handled);
	void __fastcall miCenterContentHorzClick(TObject *Sender);
	void __fastcall miCenterContentVertClick(TObject *Sender);
	void __fastcall pmAnimateTextClick(TObject *Sender);
	void __fastcall pmItemAnimatePopup(TObject *Sender);
private:	// User declarations
	TdxTileControlItem *SelectedItem;
	//
	void __fastcall AddLookAndFeelMenu();
	//
	void __fastcall InitializeItemsAnimation();
	void __fastcall PopulateAgents();
	void __fastcall SetTexts(TdxTileControlCustomItem *AItem, String AText2, String AText3);
	void __fastcall PopulateHousesAndInteriors();
	void __fastcall PopulateItemsFrames();
	//
	void __fastcall ChangeItemAnimationInterval(TObject *Sender);
	void __fastcall ChangeItemAnimationMode(TObject *Sender);
	void __fastcall ChangeScrollMode(TObject *Sender);
	void __fastcall CheckCurrentAnimationInterval(TObject *Sender);
	void __fastcall CheckCurrentAnimationMode(TObject *Sender);
	void __fastcall CheckCurrentScrollMode(TObject *Sender);
	void __fastcall DoShowAboutDemoForm(TObject *Sender);
	void __fastcall Initialize_miHelp();
	void __fastcall Initialize_miScrollMode();
	void __fastcall Initialize_pmAnimationInterval();
	void __fastcall Initialize_pmItemAnimate();
	bool __fastcall IsItemAnimationAvailable(TdxTileControlItem *AItem);
	void __fastcall SelectHelpUrl(TObject *Sender);
public:		// User declarations
	__fastcall TfmFrameAnimationMain(TComponent* Owner);
};

const int ItemAnimationInterval[] = {0, 500, 1000, 1500, 2000, 3000};

//---------------------------------------------------------------------------
extern PACKAGE TfmFrameAnimationMain *fmFrameAnimationMain;
//---------------------------------------------------------------------------
#endif

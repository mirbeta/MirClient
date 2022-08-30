//---------------------------------------------------------------------------

#ifndef DesktopDemoMainH
#define DesktopDemoMainH
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
#include "dxGDIPlusClasses.hpp"
#include "dxTileControl.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TDesktopDemoMainForm : public TForm
{
__published:	// IDE-managed Components
	TdxTileControl *dxTileControl1;
	TdxTileControlActionBarItem *tcaCustomizeOn;
	TdxTileControlActionBarItem *tcaExit;
	TdxTileControlActionBarItem *tcaClearSelection;
	TdxTileControlGroup *dxTileControl1Group1;
	TdxTileControlGroup *dxTileControl1Group2;
	TdxTileControlGroup *dxTileControl1Group3;
	TdxTileControlItem *ItemDesktop;
	TdxTileControlItem *ItemPeople;
	TdxTileControlItem *ItemMail;
	TdxTileControlItem *ItemGames;
	TdxTileControlItem *ItemCamera;
	TdxTileControlItem *ItemVideo;
	TdxTileControlItem *ItemMusic;
	TdxTileControlItem *ItemSkyDrive;
	TdxTileControlItem *ItemStore;
	TdxTileControlItem *ItemCalendar;
	TdxTileControlItem *ItemMaps;
	TdxTileControlItem *ItemReadingList;
	TdxTileControlItem *ItemSports;
	TdxTileControlItem *ItemNews;
	TdxTileControlItem *ItemIE;
	TdxTileControlItem *ItemPhotos;
	TdxTileControlItem *ItemFinances;
	TdxTileControlItem *ItemWeather;
	TdxTileControlItem *ItemFoods;
	TdxTileControlItemFrame *ItemFoodsdxTileControlItemFrame2;
	TdxTileControlItemFrame *ItemFoodsdxTileControlItemFrame1;
	TdxTileControlItem *ItemTravel;
	TdxTileControlItem *ItemHealth;
	TdxTileControlItemFrame *ItemHealthdxTileControlItemFrame2;
	TdxTileControlItemFrame *ItemHealthdxTileControlItemFrame1;
	TdxTileControlItem *ItemHelp;
	TcxImageCollection *icPeople;
	TcxImageCollectionItem *icPeopleItem1;
	TcxImageCollectionItem *icPeopleItem2;
	TcxImageCollectionItem *icPeopleItem3;
	TcxImageCollectionItem *icPeopleItem4;
	TcxImageCollectionItem *icPeopleItem5;
	TcxImageCollectionItem *icPeopleItem6;
	TcxImageCollectionItem *icPeopleItem7;
	TcxImageCollectionItem *icPeopleItem8;
	TcxImageCollectionItem *icPeopleItem9;
	TcxImageCollectionItem *icPeopleItem10;
	TcxImageCollectionItem *icPeopleItem11;
	TcxImageCollectionItem *icPeopleItem12;
	TcxImageList *ilMedium;
	TcxImageList *ilSmall;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall ItemFoodsActiveFrameChanged(TdxTileControlItem *Sender);
	void __fastcall tcaClearSelectionClick(TdxTileControlActionBarItem *Sender);
	void __fastcall tcaCustomizeOnClick(TdxTileControlActionBarItem *Sender);
	void __fastcall tcaExitClick(TdxTileControlActionBarItem *Sender);
	void __fastcall dxTileControl1ItemCheck(TdxCustomTileControl *Sender, TdxTileControlItem *AItem);
        void __fastcall dxTileControl1KeyPress(TObject *Sender, Char &Key);
private:	// User declarations
	String __fastcall GetDayName(Word ADayOfWeek);
	void __fastcall UpdateActionBarsItems();
public:		// User declarations
	__fastcall TDesktopDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDesktopDemoMainForm *DesktopDemoMainForm;
//---------------------------------------------------------------------------
#endif

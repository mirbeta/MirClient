//---------------------------------------------------------------------------

#ifndef RealtorWorldMainH
#define RealtorWorldMainH
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
#include "dxSkinsForm.hpp"
#include "dxSkinsDefaultPainters.hpp"
#include "dxTileControl.hpp"
//---------------------------------------------------------------------------
class TfrmRealtorWorld : public TForm
{
__published:	// IDE-managed Components
	TdxSkinController *dxSkinController1;
	TdxTileControl *dxTile;
	TdxTileControlGroup *dxTiledxTileControlGroup1;
	TdxTileControlGroup *dxTiledxTileControlGroup2;
	TdxTileControlGroup *dxTiledxTileControlGroup3;
	TdxTileControlItem *tlPhotos;
	TdxTileControlItem *tlUserManagement;
	TdxTileControlItem *tlSystemInformation;
	TdxTileControlItem *tlAgents;
	TdxTileControlItem *tlResearch;
	TdxTileControlItem *tlStatistics;
	TdxTileControlItem *tlZillow;
	TdxTileControlItem *tlLoanCalculator;
	TdxTileControlItem *tlMortgageRates;
	TdxTileControlActionBarItem *tcaBlackTheme;
	TdxTileControlActionBarItem *tcaWhiteTheme;
	TdxTileControlActionBarItem *tcaExit;
	TdxTileControlActionBarItem *tcaMakeTileItemLarger;
	TdxTileControlActionBarItem *tcaMakeTileItemSmaller;
	TdxTileControlActionBarItem *tcaClearSelection;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall dxTileItemCheck(TdxCustomTileControl *Sender, TdxTileControlItem *AItem);
	void __fastcall tlActivateDetail(TdxTileControlItem *Sender);
	void __fastcall tlZillowClick(TdxTileControlItem *Sender);
	void __fastcall tlUnderConstructionClick(TdxTileControlItem *Sender);
	void __fastcall tcaChangeThemeClick(TdxTileControlActionBarItem *Sender);
	void __fastcall tcaExitClick(TdxTileControlActionBarItem *Sender);
    void __fastcall tcaMakeTileItemLargerClick(TdxTileControlActionBarItem *Sender);
	void __fastcall tlDeactivateDetail(TdxTileControlItem *Sender);
	void __fastcall tcaClearSelectionClick(TdxTileControlActionBarItem *Sender);
private:	// User declarations
	void __fastcall SelectSkin(Boolean ABlackSkin);
	void __fastcall InitializeTileControlItemPhotos();
	void __fastcall InitializeTileControlItemAgents();
	void __fastcall SetTexts(TdxTileControlCustomItem *AItem, String AText2, String AText3);
	void __fastcall UpdateActionBarsItems();
public:		// User declarations
	__fastcall TfrmRealtorWorld(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmRealtorWorld *frmRealtorWorld;
//---------------------------------------------------------------------------
#endif

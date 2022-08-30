//---------------------------------------------------------------------------

#ifndef DemoUtilsH
#define DemoUtilsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"

enum TdxWebPageType { wpMain, wpClienCenter, wpDownloads, wpProducts, wpSupport };

const String WebPageHelpMenuCaptions[] = {
	"Developer Express",
	"DevExpress Client Center",
	"DevExpress Downloads",
	"DevExpress VCL",
	"DevExpress Support Center"
	};
//---------------------------------------------------------------------------

class TcxLookAndFeelClickController: public TObject
{
  private:
	TcxLookAndFeelController* FLookAndFeelController;
  public:
	__fastcall TcxLookAndFeelClickController(TcxLookAndFeelController* ALookAndFeelController);
	void __fastcall LookAndFeelHandler(TObject *Sender);
	__property TcxLookAndFeelController* LookAndFeelController = {read=FLookAndFeelController};
};

TMenuItem* CreateLookAndFeelMenuItems(TComponent* AOwner, TcxLookAndFeelController* ALookAndFeelContoller,
  int AGroupIndex = 1);
void HandleLookAndFeelChangeCommand(TObject *ASender, TcxLookAndFeelController *ALookAndFeelController);
void ShowWebPage(TdxWebPageType AWebPage);

#endif

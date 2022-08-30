//---------------------------------------------------------------------------


#pragma hdrstop

#include "DemoUtils.h"
#pragma link "dxCore"
#pragma link "cxClasses"
#pragma link "cxControls"
//---------------------------------------------------------------------------

#pragma package(smart_init)
const
  PCHAR
    dxDownloadURL = "http://www.devexpress.com/downloads",
    dxSupportURL = "http://www.devexpress.com/Support/Center",
    dxStartURL = "http://www.devexpress.com",
    dxProductsURL = "http://www.devexpress.com/products",
	dxClientCenterURL = "http://www.devexpress.com/ClientCenter";

TcxLookAndFeelClickController *FLookAndFeelClickController;

void HandleLookAndFeelChangeCommand(TObject *ASender, TcxLookAndFeelController *ALookAndFeelController)
{
  switch (((TComponent*)ASender)->Tag)
  {
	case 0:
	case 1:
	case 2:
	case 3:
	  {
		ALookAndFeelController->Kind = TcxLookAndFeelKind(((TComponent*)ASender)->Tag);
		ALookAndFeelController->NativeStyle = false;
		break;
	  };
	case 4: ALookAndFeelController->NativeStyle = true; break;
  };
}

void ShowWebPage(TdxWebPageType AWebPage)
{
  PCHAR AURL;
  switch (AWebPage)
  {
	case wpDownloads: AURL = dxDownloadURL; break;
	case wpSupport: AURL = dxSupportURL; break;
	case wpMain: AURL = dxStartURL; break;
	case wpProducts: AURL = dxProductsURL; break;
	case wpClienCenter: AURL = dxClientCenterURL; break;
  }
  dxShellExecute(AURL);
}

void __fastcall AddMenuItem(TMenuItem *AParent, String ACaption, int ATag, int AGroupIndex = 1)
{
	TMenuItem* AItem = new TMenuItem(AParent);
	AItem->Caption = ACaption;
	AItem->RadioItem = true;
	AItem->AutoCheck = true;
	AItem->GroupIndex = AGroupIndex;
	AItem->Tag = ATag;
	AParent->Add(AItem);
	TcxLookAndFeelController* ALookAndFeelController = FLookAndFeelClickController->LookAndFeelController;
	AItem->Checked = ALookAndFeelController->NativeStyle && (ATag == 4) ||
	  !ALookAndFeelController->NativeStyle && ((int)((ALookAndFeelController->Kind)) == ATag);
	AItem->OnClick = FLookAndFeelClickController->LookAndFeelHandler;
}

TMenuItem* CreateLookAndFeelMenuItems(TComponent *AOwner, TcxLookAndFeelController *ALookAndFeelContoller,
  int AGroupIndex)
{
	if (FLookAndFeelClickController == NULL)
	  {FLookAndFeelClickController = new TcxLookAndFeelClickController(ALookAndFeelContoller);};
	TMenuItem* Result = new TMenuItem(AOwner);
	Result->Caption = "&Look&&Feel";
	AddMenuItem(Result, "&Flat", 0, AGroupIndex);
	AddMenuItem(Result, "&Standard", 1, AGroupIndex);
	AddMenuItem(Result, "&UltraFlat", 2, AGroupIndex);
	AddMenuItem(Result, "&Office11", 3, AGroupIndex);
	AddMenuItem(Result, "&Native", 4, AGroupIndex);
	return(Result);
}

__fastcall TcxLookAndFeelClickController::TcxLookAndFeelClickController(TcxLookAndFeelController* ALookAndFeelController)
{
  FLookAndFeelController = ALookAndFeelController;
}

void __fastcall TcxLookAndFeelClickController::LookAndFeelHandler(TObject *Sender)
{
  HandleLookAndFeelChangeCommand(Sender, FLookAndFeelController);
}

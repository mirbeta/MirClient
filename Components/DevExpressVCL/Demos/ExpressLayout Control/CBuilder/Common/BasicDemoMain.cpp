//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BasicDemoMain.h"
#include "DemoDM.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutContainer"
#pragma resource "*.dfm"
TfrmBasicDemoMain *frmBasicDemoMain;

const PAnsiChar dxDownloadURL = "http://www.devexpress.com/downloads";
const PAnsiChar dxSupportURL = "http://www.devexpress.com/Support/Center";
const PAnsiChar dxStartURL = "http://www.devexpress.com";
const PAnsiChar dxProductsURL = "http://www.devexpress.com/products";
const PAnsiChar dxMyDXURL = "http://www.mydevexpress.com";

//---------------------------------------------------------------------------
void __fastcall Browse(dxSitePage ASitePage)
{
  PAnsiChar AURL;
  switch (ASitePage) {
	 spDownloads:
	  AURL = dxDownloadURL;
	  break;
	case spSupport:
	  AURL = dxSupportURL;
	  break;
	case spStart:
	  AURL = dxStartURL;
	  break;
	case spProducts:
	  AURL = dxProductsURL;
	  break;
	case spMyDX:
	  AURL = dxMyDXURL;
	  break;
  };
  ShellExecute(0, PAnsiChar("OPEN"), AURL, NULL, NULL, SW_SHOW);
}
//---------------------------------------------------------------------------
__fastcall TfrmBasicDemoMain::TfrmBasicDemoMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmBasicDemoMain::FormCreate(TObject *Sender)
{
  if (miStyle->Visible)
	acLayoutStandard->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::LayoutStyleExecute(TObject *Sender)
{
  int ATag = ((TAction*)Sender)->Tag;
  switch (ATag) {
	case 0:
	  lcMain->LayoutLookAndFeel = dmDemo->dxLayoutStandardLookAndFeel1;
	  break;
	case 1:
	  lcMain->LayoutLookAndFeel = dmDemo->dxLayoutOfficeLookAndFeel1;
	  break;
	case 2:
	  lcMain->LayoutLookAndFeel = dmDemo->dxLayoutWebLookAndFeel1;
	  break;
  default:
	lcMain->LayoutLookAndFeel = dmDemo->dxLayoutCxLookAndFeel1;
	switch (ATag) {
	  case 3:
	  case 4:
	  case 5:
	  case 6:
		dmDemo->dxLayoutCxLookAndFeel1->LookAndFeel->NativeStyle = false;
		dmDemo->dxLayoutCxLookAndFeel1->LookAndFeel->Kind = TcxLookAndFeelKind(ATag - 3);
		break;
	  case 7:
		dmDemo->dxLayoutCxLookAndFeel1->LookAndFeel->NativeStyle = true;
		break;
	};
  };
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::aAutosizeExecute(TObject *Sender)
{
  if (aAutosize->Checked) {
	lcMain->Align = alNone;
	lcMainGroup_Root1->AlignHorz = ahLeft;
    lcMainGroup_Root1->AlignVert = avTop;
    lcMain->AutoSize = true;
	AutoSize = true;}
  else {
	AutoSize = false;
	lcMain->AutoSize = false;
    lcMainGroup_Root1->AlignHorz = ahClient;
    lcMainGroup_Root1->AlignVert = avClient;
	lcMain->Align = alClient;
  };
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::Aboutthisdemo1Click(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::DeveloperExpressProducts1Click(TObject *Sender)

{
  Browse((dxSitePage)((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::Exit1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfrmBasicDemoMain::Customization1Click(TObject *Sender)
{
  lcMain->Customization = true;
}
//---------------------------------------------------------------------------


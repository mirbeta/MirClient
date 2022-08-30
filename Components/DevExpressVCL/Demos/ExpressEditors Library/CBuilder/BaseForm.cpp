//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BaseForm.h"
#include "AboutDemoForm.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TfmBaseForm::TfmBaseForm(TComponent* Owner): TForm(Owner)
{
	BuildLookAndFeelMenu();
};
//---------------------------------------------------------------------------
void __fastcall TfmBaseForm::BuildLookAndFeelMenu()
{
	mmMain->Items->Insert(mmMain->Items->IndexOf(miAbout),
		CreateLookAndFeelMenuItems(mmMain->Items, NULL, 1));
}
//---------------------------------------------------------------------------
void __fastcall TfmBaseForm::miAboutClick(TObject *Sender)
{
	ShowAboutDemoForm();
}
//---------------------------------------------------------------------------
void __fastcall TfmBaseForm::miExitClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------


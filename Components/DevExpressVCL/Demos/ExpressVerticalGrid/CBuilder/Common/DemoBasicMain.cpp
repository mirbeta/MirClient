//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoBasicMain.h"
#include "DemoRating.h"
#include "DemoBasicAbout.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
__fastcall TDemoBasicMainForm::TDemoBasicMainForm(TComponent* Owner)
  : TForm(Owner)
{
  Application->HelpFile = "..\\..\\Help\\cxVertGrid.hlp";  
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::AddLookAndFeelMenu()
{
  miOptions->Insert(miOptions->IndexOf(miSeparator),
	CreateLookAndFeelMenuItems(miOptions, cxLookAndFeelController));
}

TcxLookAndFeelKind __fastcall TDemoBasicMainForm::GetDefaultLookAndFeelKind()
{
  return(lfUltraFlat);
}

bool __fastcall TDemoBasicMainForm::IsNativeDefaultStyle()
{
  return(true);
}

void __fastcall TDemoBasicMainForm::miTouchModeClick(TObject *Sender)
{
  cxLookAndFeelController->TouchMode = miTouchMode->Checked;
}

void __fastcall TDemoBasicMainForm::SetDefaultLookAndFeel()
{
  cxLookAndFeelController->NativeStyle = IsNativeDefaultStyle();
  cxLookAndFeelController->Kind = GetDefaultLookAndFeelKind();
}

void __fastcall TDemoBasicMainForm::actDXOnTheWebExecute(TObject *Sender)
{
  ShowWebPage((TdxWebPageType)(((TComponent*)Sender)->Tag));
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actShowDemoDescriptionExecute(TObject *Sender)
{
  lbDescrip->Visible = !lbDescrip->Visible;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actHelpExecute(TObject *Sender)
{
  Application->HelpCommand(HELP_FINDER, 0);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actRateDemoExecute(TObject *Sender)
{
  TDemoRatingForm *AForm = new TDemoRatingForm(this);
  try{
    AForm->ShowModal();
  }
  __finally{
    delete AForm;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::actAboutExecute(TObject *Sender)
{
  ShowAbout(true, false);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::AdjustAboutText(TStrings *AAboutText)
{
  AAboutText->Assign(memAboutText->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::ShowAbout(bool AModal, bool AOnTop)
{
  if (DemoBasicAboutForm == NULL)
    DemoBasicAboutForm = new TDemoBasicAboutForm(this);
  AdjustAboutText(DemoBasicAboutForm->reDemoInfo->Lines);
  DemoBasicAboutForm->lbDemoName->Caption =
    ChangeFileExt(ExtractFileName(Application->ExeName),"");
  if (AOnTop)
    DemoBasicAboutForm->FormStyle = fsStayOnTop;
  else
    DemoBasicAboutForm->FormStyle = fsNormal;
  if (AModal)
    DemoBasicAboutForm->ShowModal();
  else
    DemoBasicAboutForm->Show();
}
//---------------------------------------------------------------------------


void __fastcall TDemoBasicMainForm::FormCreate(TObject *Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  miTouchMode->Checked = cxIsTouchModeEnabled;
}
//---------------------------------------------------------------------------


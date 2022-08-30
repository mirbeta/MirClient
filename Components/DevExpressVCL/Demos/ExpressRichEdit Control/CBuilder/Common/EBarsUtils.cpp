//---------------------------------------------------------------------------

#include <vcl.h>
#include <Forms.hpp>
#pragma hdrstop

#include "EBarsUtils.h"
#include "EBarsDemoRating.h"
#include "AboutDemoForm.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TdmCommonData *dmCommonData;
//---------------------------------------------------------------------------
__fastcall TdmCommonData::TdmCommonData(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actSBarsHelpExecute(TObject *Sender)
{
  Application->HelpFile = "..\\..\\..\\..\\ExpressSideBar\\Help\\eSideBar.hlp";
  Application->HelpCommand(HELP_FINDER, 0);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actBarsHelpExecute(TObject *Sender)
{
  Application->HelpFile = "..\\..\\..\\Help\\ExpressBars.hlp";
  Application->HelpCommand(HELP_FINDER, 0);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actDockingHelpExecute(TObject *Sender)
{
  Application->HelpFile = "..\\..\\..\\..\\ExpressDocking Library\\Help\\docking.hlp";
  Application->HelpCommand(HELP_FINDER, 0);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actRateDemoExecute(TObject *Sender)
{
  TEBarsDemoRatingForm* EBarsDemoRatingForm = new TEBarsDemoRatingForm(NULL);
  try {
    EBarsDemoRatingForm->ShowModal();
  }
  __finally {
    delete EBarsDemoRatingForm;
  }
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actMyDXExecute(TObject *Sender)
{
  Browse(spMyDX);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actDownloadsExecute(TObject *Sender)
{
  Browse(spDownloads);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actSupportExecute(TObject *Sender)
{
  Browse(spSupport);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actDXOnTheWebExecute(TObject *Sender)
{
  Browse(spStart);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actProductsExecute(TObject *Sender)
{
  Browse(spProducts);
}
//---------------------------------------------------------------------------
void __fastcall TdmCommonData::actAboutExecute(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------
void Browse(dxSitePage ASitePage)
{
  PCHAR AURL;
  switch (ASitePage)
  {
    case spDownloads: AURL = dxDownloadURL; break;
    case spSupport: AURL = dxSupportURL; break;
    case spStart: AURL = dxStartURL; break;
    case spProducts: AURL = dxProductsURL; break;
    case spMyDX: AURL = dxMyDXURL; break;
  }
  ShellExecute(0, "OPEN", AURL, NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

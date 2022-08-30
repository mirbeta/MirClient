//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "NavBarUtils.h"
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
void __fastcall TdmCommonData::actExitExecute(TObject *Sender)
{
  Application->MainForm->Close();        
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

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ConditionalFormattingDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TConditionalFormattingDemoMainDM *ConditionalFormattingDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TConditionalFormattingDemoMainDM::TConditionalFormattingDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainDM::DataModuleCreate(TObject *Sender)
{
  cdsConditionalFormatting->LoadFromFile("..\\..\\Data\\ConditionalFormatting.cds");
  cdsConditionalFormatting->Open();
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FrameAnimationDemoDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TDM *DM;
//---------------------------------------------------------------------------
__fastcall TDM::TDM(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDM::DataModuleCreate(TObject *Sender)
{
  AnsiString APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  clHomePhotos->LoadFromFile(APath + "HomePhotos.cds");
  clHomesAndAgents->LoadFromFile(APath + "HomesAndAgents.cds");
  clHomesAndHomes->LoadFromFile(APath + "HomesAndHomes.cds");
}
//---------------------------------------------------------------------------

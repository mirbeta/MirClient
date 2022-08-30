//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TDMRealtorWorld *DMRealtorWorld;
//---------------------------------------------------------------------------
__fastcall TDMRealtorWorld::TDMRealtorWorld(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDMRealtorWorld::DataModuleCreate(TObject *Sender)
{
  AnsiString APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  clHomePhotos->LoadFromFile(APath + "HomePhotos.cds");
  clHomesAndAgents->LoadFromFile(APath + "HomesAndAgents.cds");
  clHomesAndHomes->LoadFromFile(APath + "HomesAndHomes.cds");
  clHousesSales->LoadFromFile(APath + "HousesSales.cds");
  clMortgage->LoadFromFile(APath + "Mortgage.cds");
  clHomesDetail->Active = True;
  clHouseSalesChart->LoadFromFile(APath + "HouseSalesChart.cds");
  clHouseRating->LoadFromFile(APath + "HouseRating.cds");
  clHousesSimular->LoadFromFile(APath + "HouseSimular.cds");
  clHousePrice->LoadFromFile(APath + "HousePrice.cds");
  clResearchChart->LoadFromFile(APath + "HousesSalesResearch.cds");
}
//---------------------------------------------------------------------------

void __fastcall TDMRealtorWorld::clHomesAndHomesCalcFields(TDataSet *DataSet)
{
  clHomesAndHomesAgentID->Value = clHomesAndHomesID->Value % 6 + 1;
  clHomesAndHomesYearID->Value = StrToInt(clHomesAndHomesYearBuilt->Value);
}
//---------------------------------------------------------------------------


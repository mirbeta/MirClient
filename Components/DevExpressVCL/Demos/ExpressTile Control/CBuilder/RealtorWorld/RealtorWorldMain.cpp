//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldMain.h"
#include "RealtorWorldDM.h"
#include "RealtorWorldUnderConstruction.h"
#include "RealtorWorldListing.h"
#include "RealtorWorldMortgageRate.h"
#include "RealtorWorldResearch.h"
#include "RealtorWorldAgents.h"
#include "RealtorWorldStatistic.h"
#include "RealtorWorldLoanCalculator.h"
#include "RealtorWorldBaseFrame.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxCustomTileControl"
#pragma link "dxSkinsForm"
#pragma link "dxSkinsDefaultPainters"
#pragma link "dxTileControl"
#pragma resource "*.dfm"
TfrmRealtorWorld *frmRealtorWorld;
//---------------------------------------------------------------------------
__fastcall TfrmRealtorWorld::TfrmRealtorWorld(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::SelectSkin(Boolean ABlackSkin)
{
	if (ABlackSkin)
	{
		dxSkinsUserSkinLoadFromFile("..\\..\\Data\\MetroBlack.skinres");
	}
	else
	{
		dxSkinsUserSkinLoadFromFile("..\\..\\Data\\MetroWhite.skinres");
	}
	tcaBlackTheme->Visible = !ABlackSkin;
	tcaWhiteTheme->Visible = ABlackSkin;
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::tcaExitClick(TdxTileControlActionBarItem *Sender)
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::tcaChangeThemeClick(TdxTileControlActionBarItem *Sender)
{
	SelectSkin(Sender->Tag == 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::SetTexts(TdxTileControlCustomItem *AItem, String AText2, String AText3)
{
  AItem->Style->Font->Size = 13;
  AItem->Text2->Value = AText2;
  AItem->Text2->IndentHorz = 0;
  AItem->Text2->IndentVert = 0;
  AItem->Text2->Transparent = false;
  AItem->Text3->Value = AText3;
  AItem->Text3->IndentHorz = 0;
  AItem->Text3->IndentVert = 0;
  AItem->Text3->Transparent = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::InitializeTileControlItemPhotos()
{
  TdxTileControlItemFrame* AFrame;
  String AText2;
  String AText3;
  int AParentID;
  TDataSet* dsHomes;
  TDataSet* dsHomesInterior;
  TFormatSettings AFmtSettings;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, AFmtSettings);
  dsHomes = DMRealtorWorld->clHomesAndHomes;
  dsHomesInterior = DMRealtorWorld->clHomePhotos;
  dsHomes->First();
  while (!dsHomes->Eof)
  {
    AFrame = tlPhotos->Frames->Add();
    AFrame->Glyph->Mode = ifmFill;
    AFrame->Glyph->Image->LoadFromFieldValue(dsHomes->FieldByName("Photo")->Value);
	AFrame->Tag = dsHomes->FieldByName("ID")->AsInteger;
	int AHomeID = AFrame->Tag;

    AParentID = dsHomes->FieldByName("ID")->AsInteger % 7 + 1;
	AText2 = " " + dsHomes->FieldByName("Beds")->AsString + " Beds" + "\n" + " " + dsHomes->FieldByName("Baths")->AsString + " Baths ";
    AText3 = " " + FloatToStrF(dsHomes->FieldByName("Price")->AsFloat, ffCurrency, 10, 0, AFmtSettings) + " ";
	SetTexts(AFrame, AText2, AText3);

	TLocateOptions ALocateOptions;
	ALocateOptions.Clear();
	dsHomesInterior->Locate("ParentID", AParentID, ALocateOptions);
	while ((!dsHomesInterior->Eof) &&
	  (dsHomesInterior->FieldByName("ParentID")->AsInteger == AParentID))
	{
      AFrame = tlPhotos->Frames->Add();
      AFrame->Glyph->Image->LoadFromFieldValue(dsHomesInterior->FieldByName("Photo")->Value);
      AFrame->Glyph->Align = oaMiddleCenter;
	  AFrame->Glyph->Mode = ifmFill;
	  AFrame->Tag = AHomeID;
	  SetTexts(AFrame, AText2, AText3);
	  dsHomesInterior->Next();
	};
	dsHomes->Next();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::InitializeTileControlItemAgents()
{
  TDataSet* dsAgents;
  TdxTileControlItemFrame* AFrame;
  dsAgents = DMRealtorWorld->clHomesAndAgents;
  dsAgents->First();
  while (!dsAgents->Eof)
  {
    AFrame = tlAgents->Frames->Add();
    AFrame->Glyph->Image->LoadFromFieldValue(dsAgents->FieldByName("Photo")->Value);
    AFrame->Glyph->Image->Scale(70, 100);
	AFrame->Glyph->Align = oaMiddleRight;
    AFrame->Tag = dsAgents->FieldByName("ID")->AsInteger;
	AFrame->Style->Font->Size = 13;
	AFrame->Text1->Value = dsAgents->FieldByName("FirstName")->AsString + " " + dsAgents->FieldByName("LastName")->AsString;
	AFrame->Text1->IndentHorz = 10;
	AFrame->Text1->IndentVert = 10;
	AFrame->Text2->Value = dsAgents->FieldByName("Phone")->AsString;
	AFrame->Text2->Align = oaTopLeft;
	AFrame->Text2->IndentHorz = 10;
	AFrame->Text2->IndentVert = 30;
	dsAgents->Next();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::FormCreate(TObject *Sender)
{
  dxSkinController1->NativeStyle = false;
  dxTile->LookAndFeel->AssignedValues.Clear();
  SelectSkin(true);
  InitializeTileControlItemPhotos();
  InitializeTileControlItemAgents();
  UpdateActionBarsItems();
  dxTile->Controller->StopItemContentAnimation();
  __try
	{
    for (int i = 0; i >= dxTile->Items->Count - 1; i++)
	  if (dxTile->Items->Items[i]->Frames->Count > 0)
		{
		  dxTile->Items->Items[i]->ActiveFrame = dxTile->Items->Items[i]->Frames->Items[0];
		}
    }
  __finally
	{
	  dxTile->Controller->StartItemContentAnimation();
	};
  dxSkinController1->TouchMode = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRealtorWorld::tlActivateDetail(TdxTileControlItem *Sender)
{
  if (Sender->DetailOptions->DetailControl == NULL)
  {
	Sender->DetailOptions->DetailControl = CreateFrameByID(this, Sender->Tag);
  };
  TfrmBase* ADetail = (TfrmBase*)(Sender->DetailOptions->DetailControl);
  ADetail->SelectItem(tlPhotos->ActiveFrame->Tag, tlAgents->ActiveFrame->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TfrmRealtorWorld::tlZillowClick(TdxTileControlItem *Sender)
{
  ShellExecuteA(0, "open", "http://www.zillow.com", NULL, NULL, SW_SHOW);
}
//---------------------------------------------------------------------------


void __fastcall TfrmRealtorWorld::tlUnderConstructionClick(TdxTileControlItem *Sender)
{
  if (Sender->DetailOptions->DetailControl == NULL)
  {
	Sender->DetailOptions->DetailControl = new TfrmUnderConstruction(this);
	Sender->DetailOptions->DetailControl->Name = Sender->Name + "DetailControl";
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmRealtorWorld::tcaMakeTileItemLargerClick(TdxTileControlActionBarItem *Sender)
{
    for (int I = 0; I < dxTile->CheckedItemCount; I++)
	{
		dxTile->CheckedItems[I]->IsLarge = Sender->Tag == 1;
	}
}
//---------------------------------------------------------------------------

void __fastcall TfrmRealtorWorld::UpdateActionBarsItems()
{
  bool AAllCheckedItemsAreLarge = true;
  bool AAllCheckedItemsAreSmall = true;
  TdxTileControlItem *AItem;

  for (int I = 0; I < dxTile->CheckedItemCount; I++)
  {
      AItem = dxTile->CheckedItems[I];
      AAllCheckedItemsAreLarge = AAllCheckedItemsAreLarge && (AItem->RowCount == 1) && AItem->IsLarge;
      AAllCheckedItemsAreSmall = AAllCheckedItemsAreSmall && (AItem->RowCount == 1) && !AItem->IsLarge;
  }
  tcaMakeTileItemSmaller->Visible = (dxTile->CheckedItemCount > 0) && AAllCheckedItemsAreLarge;
  tcaMakeTileItemLarger->Visible = (dxTile->CheckedItemCount > 0) && AAllCheckedItemsAreSmall;
  tcaClearSelection->Visible = dxTile->CheckedItemCount > 0;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRealtorWorld::dxTileItemCheck(TdxCustomTileControl *Sender, TdxTileControlItem *AItem)
{
	UpdateActionBarsItems();
}
void __fastcall TfrmRealtorWorld::tlDeactivateDetail(TdxTileControlItem *Sender)

{
//
}
//---------------------------------------------------------------------------

void __fastcall TfrmRealtorWorld::tcaClearSelectionClick(TdxTileControlActionBarItem *Sender)
{
  for (int I = dxTile->CheckedItemCount - 1; I >= 0 ; I--)
    dxTile->CheckedItems[I]->Checked = false;
}
//---------------------------------------------------------------------------


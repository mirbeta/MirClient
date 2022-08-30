//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldHomePhotosBase.h"
#include "RealtorWorldDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSplitter"
#pragma link "dxCustomTileControl"
#pragma link "dxTileControl"
#pragma resource "*.dfm"
TfrmHomePhotosBase *frmHomePhotosBase;
//---------------------------------------------------------------------------
__fastcall TfrmHomePhotosBase::TfrmHomePhotosBase(TComponent* Owner)
	: TfrmBase(Owner)
{
  InitializeFrame();
}
//---------------------------------------------------------------------------
void __fastcall TfrmHomePhotosBase::cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose)

{
  AllowClose = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmHomePhotosBase::SelectItem(int APhotoID, int AAgentID)
{
  for (int i = 0; i < tcHomePhotos->Items->Count; i++)
  {
	if (tcHomePhotos->Items->Items[i]->Tag == APhotoID)
	{
	  tcHomePhotos->Items->Items[i]->MakeVisible();
	  tcHomePhotos->Items->Items[i]->Click();
	};
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmHomePhotosBase::InitializeFrame()
{
  TDataSet* dsHomes;
  TdxTileControlItem* AItem;
  TFormatSettings AFmtSettings;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, AFmtSettings);
  tcHomePhotos->BeginUpdate();
  __try
	{
		dsHomes = DMRealtorWorld->clHomesAndHomes;
		dsHomes->First();
		while (!dsHomes->Eof)
		{
		  AItem = tcHomePhotos->CreateItem(true);
		  AItem->Glyph->Image->LoadFromFieldValue(dsHomes->FieldByName("Photo")->Value);
		  AItem->Glyph->Mode = ifmStretch;
		  AItem->Text2->Value = " " + dsHomes->FieldByName("Beds")->AsString + " Beds" + "\n" + " " + dsHomes->FieldByName("Baths")->AsString + " Baths ";
		  AItem->Text2->IndentHorz = 0;
		  AItem->Text2->Font->Size = 13;
		  AItem->Text2->IndentVert = 0;
		  AItem->Text2->Transparent = false;
		  AItem->Text3->Value = " " + FloatToStrF(dsHomes->FieldByName("Price")->AsFloat, ffCurrency, 10, 0, AFmtSettings) + " ";
		  AItem->Text3->IndentHorz = 0;
		  AItem->Text3->IndentVert = 0;
		  AItem->Text3->Font->Size = 13;
		  AItem->Text3->Transparent = false;
		  AItem->Tag = dsHomes->FieldByName("ID")->AsInteger;
		  AItem->OnClick = OnItemClick;
		  dsHomes->Next();
		}
	}
  __finally
	{
	  tcHomePhotos->EndUpdate();
    }
}
//---------------------------------------------------------------------------
void __fastcall TfrmHomePhotosBase::OnItemClick(TdxTileControlItem *Sender)
{

}
//---------------------------------------------------------------------------

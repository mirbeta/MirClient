//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldListing.h"
#include "RealtorWorldMain.h"
#include "RealtorWorldDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "RealtorWorldHomePhotosBase"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxImage"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "dxCustomTileControl"
#pragma link "dxImageSlider"
#pragma link "dxTileControl"
#pragma resource "*.dfm"
TfrmListing *frmListing;
//---------------------------------------------------------------------------
__fastcall TfrmListing::TfrmListing(TComponent* Owner)
	: TfrmHomePhotosBase(Owner)
{
  imgHomePlan->Properties->Center = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::imgHomePlanMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
//
}
//---------------------------------------------------------------------------

void __fastcall TfrmListing::cxSplitter3BeforeClose(TObject *Sender, bool &AllowClose)

{
  AllowClose = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::InitializeFeatures()
{
  reFeatures->Lines->BeginUpdate();
  __try
  {
	reFeatures->Lines->Clear();
	reFeatures->Properties->StreamModes.Clear();
	reFeatures->Lines->LoadFromFile("..\\..\\Data\\ListingFmt.rtf");
    reFeatures->Properties->StreamModes << resmSelection;
	TLocateOptions ALocateOptions;
	ALocateOptions.Clear();
    if (DMRealtorWorld->clHomesAndAgents->Locate("ID", DMRealtorWorld->clHomesAndHomes->FieldByName("AgentID")->AsInteger, ALocateOptions))
	{
	  InitializeFeaturesOfAgent();
    };
	InitializeFeaturesOfHouse();
  }
  __finally
  {
	reFeatures->Lines->EndUpdate();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::InitializeFeaturesOfAgent()
{
  TStringStream* AStream;
  TdxGPImage* APhoto;
  TcxBitmap32* AImage;
  int ASelLength;
  AnsiString AName;
  AnsiString APhone;
  AnsiString AMail;
  AnsiString ATemp;
  ATemp = "__dxImageAgent__";
  ASelLength = ATemp.Length();
  TSearchTypes ASearchTypes;
  ASearchTypes.Clear();
  reFeatures->SelStart = reFeatures->FindTexT("__dxImageAgent__", 0, -1, ASearchTypes);
  reFeatures->SelLength = ASelLength;
  APhoto = new TdxSmartImage;
  AImage = new TcxBitmap32(100, 120);
  try
  {
    APhoto->LoadFromFieldValue(DMRealtorWorld->clHomesAndAgents->FieldByName("Photo")->Value);
    AImage->cxCanvas->FillRect(AImage->ClientRect, clWindow);
    AImage->Canvas->StretchDraw(cxRectInflate(AImage->ClientRect, -5, -5), APhoto);
	AStream = new TStringStream(dxAnsiStringToString(dxBitmapToRTF(AImage)));
	try
	{
      reFeatures->ActiveProperties->StreamModes = reFeatures->ActiveProperties->StreamModes << resmSelection;
	  reFeatures->Lines->LoadFromStream(AStream);
    }
	__finally
	{
      delete AStream;
	};
  }
  __finally
  {
	delete AImage;
	delete APhoto;
  };

  AName = DMRealtorWorld->clHomesAndAgents->FieldByName("FirstName")->AsString + " " +
    DMRealtorWorld->clHomesAndAgents->FieldByName("LastName")->AsString;
  ReplaceInFeatures("__AgentName__", AName);

  APhone = DMRealtorWorld->clHomesAndAgents->FieldByName("Phone")->AsString;
  ReplaceInFeatures("__AgentPhone__", APhone);

  AMail = DMRealtorWorld->clHomesAndAgents->FieldByName("EMail")->AsString;
  ReplaceInFeatures("__AgentMail__", AMail);
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::InitializeFeaturesOfHouse()
{
  TFormatSettings AFmtSettings;
  AnsiString AFeatures, AAddress, ARooms, AYearBuilt, ASquares, APrice;
  AAddress = DMRealtorWorld->clHomesAndHomes->FieldByName("Address")->AsString;
  ReplaceInFeatures("__Address__", AAddress);
  ARooms = DMRealtorWorld->clHomesAndHomes->FieldByName("Beds")->AsString + " bedrooms, " +
    DMRealtorWorld->clHomesAndHomes->FieldByName("Baths")->AsString + " bathrooms";
  ReplaceInFeatures("__Rooms__", ARooms);

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, AFmtSettings);
  ASquares = "House size " +
	FloatToStrF(DMRealtorWorld->clHomesAndHomes->FieldByName("HouseSize")->AsFloat, ffNumber, 10, 0, AFmtSettings) + " Sq Ft, " +
	"lot size " + FloatToStrF(DMRealtorWorld->clHomesAndHomes->FieldByName("LotSize")->AsFloat, ffNumber, 1, 2, AFmtSettings) + " Acres";
  ReplaceInFeatures("__Squares__", ASquares);

  AYearBuilt = "Built in " + DMRealtorWorld->clHomesAndHomes->FieldByName("YearBuilt")->AsString;
  ReplaceInFeatures("__YearBuilt__", AYearBuilt);

  APrice = CurrToStrF(DMRealtorWorld->clHomesAndHomes->FieldByName("Price")->AsCurrency, ffCurrency, 0, AFmtSettings);
  ReplaceInFeatures("__Price__", APrice);

  AFeatures = DMRealtorWorld->clHomesAndHomes->FieldByName("Features")->AsString;
  AFeatures = AFeatures.Trim();
  TReplaceFlags AReplaceFlags;
  AReplaceFlags << rfReplaceAll;
  AFeatures = StringReplace(AFeatures, ", ", "\r", AReplaceFlags);
  ReplaceInFeatures("__Features__", AFeatures);
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::OnItemClick(TdxTileControlItem *Sender)
{
  int AParentID;
  TDataSet* dsHomesInterior;
  TdxGPImage* AGraphic;
  imgHomePlan->Picture = icPlans->Items->Items[
    (Sender->Tag - 1) % icPlans->Items->Count]->Picture;
  AParentID = Sender->Tag % 7 + 1;
  icSlider->Items->BeginUpdate();
  try
  {
    icSlider->Items->Clear();
	AGraphic = new TdxSmartImage;
	try
	{
	  TLocateOptions ALocateOptions;
	  ALocateOptions.Clear();
	  if (DMRealtorWorld->clHomesAndHomes->Locate("ID", Sender->Tag, ALocateOptions))
	  {
        AGraphic->LoadFromFieldValue(DMRealtorWorld->clHomesAndHomes->FieldByName("Photo")->Value);
		icSlider->Items->Add()->Picture->Graphic = AGraphic;
        InitializeFeatures();
	  };
	  ((TcxImageAccess*)imgHomePlan)->Centre();
	  dsHomesInterior = DMRealtorWorld->clHomePhotos;
	  dsHomesInterior->Locate("ParentID", AParentID, ALocateOptions);
      while ((!dsHomesInterior->Eof) &&
		(dsHomesInterior->FieldByName("ParentID")->AsInteger == AParentID))
	  {
        AGraphic->LoadFromFieldValue(dsHomesInterior->FieldByName("Photo")->Value);
		icSlider->Items->Add()->Picture->Graphic = AGraphic;
		dsHomesInterior->Next();
	  };
	}
	__finally
	{
	  delete AGraphic;
	};
  }
  __finally
  {
	icSlider->Items->EndUpdate(true);
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmListing::ReplaceInFeatures(String ATokenStr, String S)
{
  int ASelLength = ATokenStr.Length();
  TSearchTypes ASearchTypes;
  ASearchTypes.Clear();
  reFeatures->SelStart = reFeatures->FindTexT(ATokenStr, 0, -1, ASearchTypes);
  reFeatures->SelLength = ASelLength;
  reFeatures->SelText = S;
}
//---------------------------------------------------------------------------
void __fastcall TcxImageAccess::Centre(void)
{
  TcxImage::Centre();
}
//---------------------------------------------------------------------------

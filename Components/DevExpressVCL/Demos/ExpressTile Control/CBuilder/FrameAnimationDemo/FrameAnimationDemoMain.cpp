//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FrameAnimationDemoMain.h"
#include "FrameAnimationDemoDM.h"
#include "..\Common\DemoUtils.h"
#include "..\Common\AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCustomTileControl"
#pragma link "dxSkinsCore"
#pragma link "dxTileControl"
#pragma resource "*.dfm"
TfmFrameAnimationMain *fmFrameAnimationMain;
//---------------------------------------------------------------------------
__fastcall TfmFrameAnimationMain::TfmFrameAnimationMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::FormCreate(TObject *Sender)
{
  AddLookAndFeelMenu();
  Initialize_miScrollMode();
  Initialize_pmAnimationInterval();
  Initialize_pmItemAnimate();
  Initialize_miHelp();
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::FormShow(TObject *Sender)
{
  Caption = Application->Title;
  Application->Hint = Caption;
  PopulateItemsFrames();
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::miExitClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::miCenterContentClick(TObject *Sender)
{
  miCenterContentHorz->Checked = tcMain->OptionsView->CenterContentHorz;
  miCenterContentVert->Checked = tcMain->OptionsView->CenterContentVert;
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::tcMainContextPopup(TObject *Sender,
	  TPoint &MousePos, bool &Handled)
{
  TPoint AMousePos = GetMouseCursorPos();
  AMousePos = tcMain->ScreenToClient(AMousePos);
  tcMain->ActiveHitTest->Calculate(AMousePos.x, AMousePos.y);
  AMousePos = tcMain->ClientToScreen(AMousePos);
  TdxTileControlItem *AItem = tcMain->ActiveHitTest->Item;
  Handled = AItem != NULL;
  if (Handled) {
	SelectedItem = AItem;
	pmItemAnimate->Popup(AMousePos.x, AMousePos.y);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::miCenterContentHorzClick(TObject *Sender)
{
  tcMain->OptionsView->CenterContentHorz = !tcMain->OptionsView->CenterContentHorz;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::miCenterContentVertClick(TObject *Sender)
{
  tcMain->OptionsView->CenterContentVert = !tcMain->OptionsView->CenterContentVert;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::pmAnimateTextClick(TObject *Sender)
{
  SelectedItem->OptionsAnimate->AnimateText = !SelectedItem->OptionsAnimate->AnimateText;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::pmItemAnimatePopup(TObject *Sender)
{
  pmAnimationMode->Enabled = IsItemAnimationAvailable(SelectedItem);
  pmAnimateText->Enabled = pmAnimationMode->Enabled;
  pmAnimateText->Checked = pmAnimateText->Enabled && SelectedItem->OptionsAnimate->AnimateText;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::AddLookAndFeelMenu()
{
	mmMain->Items->Insert(mmMain->Items->IndexOf(miHelp),
		CreateLookAndFeelMenuItems(mmMain->Items, cxLookAndFeelController));
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::InitializeItemsAnimation()
{
  randomize();
  int AModesCount = 7;
  int I;
  for (I = 0; I<tcMain->Items->Count; I++) {
	tcMain->Items->Items[I]->ActiveFrameIndex = 0;
	tcMain->Items->Items[I]->AnimationMode = (TdxDrawAnimationMode)(I % AModesCount);
	tcMain->Items->Items[I]->AnimationInterval = ItemAnimationInterval[random(5) + 1];
	tcMain->Items->Items[I]->OptionsAnimate->AssignedValues.Clear();
	tcMain->Items->Items[I]->OptionsAnimate->AnimateText = (I % 2) == 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::PopulateAgents()
{
  TDataSet* dsAgents;
  TdxTileControlItemFrame* AFrame;
  dsAgents = DM->clHomesAndAgents;
  dsAgents->First();
  while (!dsAgents->Eof)
  {
	AFrame = tiAgents->Frames->Add();
	AFrame->Glyph->Image->LoadFromFieldValue(dsAgents->FieldByName("PhotoBMP")->Value);
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

void __fastcall TfmFrameAnimationMain::SetTexts(TdxTileControlCustomItem *AItem, String AText2, String AText3)
{
  AItem->Style->Font->Size = 13;
  AItem->Text2->Value = AText2;
  AItem->Text2->IndentHorz = 0;
  AItem->Text2->IndentVert = 0;
  AItem->Text2->Transparent = false;
  AItem->Text2->Color = (TColor)dxTileItemLightColor;
  AItem->Text3->Value = AText3;
  AItem->Text3->IndentHorz = 0;
  AItem->Text3->IndentVert = 0;
  AItem->Text3->Transparent = false;
  AItem->Text3->Color = (TColor)dxTileItemLightColor;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::PopulateHousesAndInteriors()
{
  TdxTileControlItemFrame* AFrame;
  String AText2;
  String AText3;
  int AParentID;
  TDataSet* dsHomes;
  TDataSet* dsHomesInterior;
  TFormatSettings AFormatSettings;

  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, AFormatSettings);
  dsHomes = DM->clHomesAndHomes;
  dsHomesInterior = DM->clHomePhotos;
  dsHomes->First();
  while (!dsHomes->Eof)
  {
	AFrame = tiHouses->Frames->Add();
	AFrame->Glyph->Image->LoadFromFieldValue(dsHomes->FieldByName("PhotoBMP")->Value);
	AFrame->OptionsAnimate->AssignedValues.Clear();
	AParentID = dsHomes->FieldByName("ID")->AsInteger % 7 + 1;
	AText2 = " " + dsHomes->FieldByName("Beds")->AsString + " Beds" + "\n" + " " + dsHomes->FieldByName("Baths")->AsString + " Baths ";
	AText3 = " " + FloatToStrF(dsHomes->FieldByName("Price")->AsFloat, ffCurrency, 10, 0, AFormatSettings) + " ";
	SetTexts(AFrame, AText2, AText3);

	TLocateOptions ALocateOptions;
	ALocateOptions.Clear();
	dsHomesInterior->Locate("ParentID", AParentID, ALocateOptions);
	while ((!dsHomesInterior->Eof) &&
	  (dsHomesInterior->FieldByName("ParentID")->AsInteger == AParentID))
	{
	  AFrame = tiInteriors->Frames->Add();
	  AFrame->Glyph->Image->LoadFromFieldValue(dsHomesInterior->FieldByName("PhotoBMP")->Value);
	  AFrame->Glyph->Align = oaMiddleCenter;
	  SetTexts(AFrame, AText2, AText3);
	  dsHomesInterior->Next();
	};
	dsHomes->Next();
  };
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::PopulateItemsFrames()
{
  PopulateHousesAndInteriors();
  PopulateAgents();
  InitializeItemsAnimation();
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::ChangeItemAnimationInterval(TObject *Sender)
{
  if (SelectedItem != NULL) {
	SelectedItem->AnimationInterval = ((TMenuItem*)Sender)->Tag;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::ChangeItemAnimationMode(TObject *Sender)
{
  if (SelectedItem != NULL) {
	SelectedItem->AnimationMode = (TdxDrawAnimationMode)(((TMenuItem*)Sender)->Tag);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::ChangeScrollMode(TObject *Sender)
{
  tcMain->OptionsBehavior->ScrollMode = (TdxTileControlScrollMode)(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::CheckCurrentAnimationInterval(TObject *Sender)
{
  int I;
  for (I = 0; I<((TMenuItem*)Sender)->Count; I++) {
	TMenuItem *AMenuItem = ((TMenuItem*)Sender)->Items[I];
	AMenuItem->Checked = AMenuItem->Tag == SelectedItem->AnimationInterval;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::CheckCurrentAnimationMode(TObject *Sender)
{
  int I;
  for (I = 0; I<((TMenuItem*)Sender)->Count; I++) {
	TMenuItem *AMenuItem = ((TMenuItem*)Sender)->Items[I];
	AMenuItem->Checked = AMenuItem->Tag == (int)(SelectedItem->AnimationMode);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::CheckCurrentScrollMode(TObject *Sender)
{
  int I;
  for (I = 0; I<((TMenuItem*)Sender)->Count; I++) {
	TMenuItem *AMenuItem = ((TMenuItem*)Sender)->Items[I];
	AMenuItem->Checked = AMenuItem->Tag == (int)(tcMain->OptionsBehavior->ScrollMode);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::DoShowAboutDemoForm(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall AddMenuItem(TMenuItem *AParent, String ACaption, int ATag,
  bool ARadioItem, TNotifyEvent AOnClick)
{
	TMenuItem* AItem = new TMenuItem(AParent);
	AItem->Caption = ACaption;
	AItem->RadioItem = ARadioItem;
	AItem->GroupIndex = 0;
	AItem->Tag = ATag;
	AParent->Add(AItem);
	AItem->OnClick = AOnClick;
}
//---------------------------------------------------------------------------
void __fastcall TfmFrameAnimationMain::Initialize_miHelp()
{
  TMenuItem *AParentItem = miHelp;
  TdxWebPageType *APage;
  int I;
  for (I = 0; I<5; I++) {
	AddMenuItem(AParentItem, WebPageHelpMenuCaptions[I],
	  (int)((TdxWebPageType)(I)), True, SelectHelpUrl);
  }
  AddMenuItem(AParentItem, "-", 0, False, NULL);
  AddMenuItem(AParentItem, "About", 0, False, DoShowAboutDemoForm);
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::Initialize_miScrollMode()
{
  const String AScrollModeStr[] = {"Default", "Scrollbars", "Scroll Buttons"};
  TMenuItem *AParentItem = miScrollMode;
  int I;
  for (I = 0; I<3; I++) {
	AddMenuItem(AParentItem, AScrollModeStr[I], I, True, ChangeScrollMode);
  }
  AParentItem->OnClick = CheckCurrentScrollMode;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::Initialize_pmAnimationInterval()
{
  TMenuItem *AParentItem = pmAnimationInterval;
  int I;
  for (I = 0; I<6; I++) {
	AddMenuItem(AParentItem, (String)ItemAnimationInterval[I],
	  ItemAnimationInterval[I], True, ChangeItemAnimationInterval);
  }
  AParentItem->OnClick = CheckCurrentAnimationInterval;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::Initialize_pmItemAnimate()
{
  const String AAnimationModeStr[] = {"Scrolling right to left", "Scrolling bottom to top",
	"Scrolling left to right", "Scrolling top to bottom", "Fade", "Segmented fade",
	"Random segmented fade"};
  TMenuItem *AParentItem = pmAnimationMode;
  int I;
  for (I = 0; I<7; I++) {
	AddMenuItem(AParentItem, AAnimationModeStr[I], I, True, ChangeItemAnimationMode);
  }
  AParentItem->OnClick = CheckCurrentAnimationMode;
}
//---------------------------------------------------------------------------

bool __fastcall TfmFrameAnimationMain::IsItemAnimationAvailable(TdxTileControlItem *AItem)
{
  return AItem->AnimationInterval > 0 && AItem->Frames->Count > 1;
}
//---------------------------------------------------------------------------

void __fastcall TfmFrameAnimationMain::SelectHelpUrl(TObject *Sender)
{
  ShowWebPage((TdxWebPageType)(((TMenuItem*)Sender)->Tag));
}
//---------------------------------------------------------------------------



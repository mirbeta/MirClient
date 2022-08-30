//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonNotepadMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "NotepadMainForm"
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMemo"
#pragma link "cxScrollBox"
#pragma link "cxTextEdit"
#pragma link "cxTrackBar"
#pragma link "dxBevel"
#pragma link "dxGallery"
#pragma link "dxGalleryControl"
#pragma link "dxGDIPlusClasses"
#pragma link "dxRibbonBackstageView"
#pragma link "dxRibbonBackstageViewGalleryControl"
#pragma link "dxRibbonSkins"
#pragma link "dxRibbonStatusBar"
#pragma link "dxStatusBar"
#pragma link "dxZoomTrackBar"
#pragma link "dxBarApplicationMenu"
#pragma link "dxBarExtItems"
#pragma link "dxRibbonMiniToolbar"
#pragma link "dxRibbonRadialMenu"
#pragma link "dxScreenTip"
#pragma link "dxColorDialog"
#pragma link "dxCoreGraphics"
#pragma link "cxMaskEdit"
#pragma link "dxOfficeSearchBox"
#pragma link "dxCore"
#pragma link "cxImageList"
#pragma link "dxBarBuiltInMenu"
#pragma link "dxSkinsdxRibbonPainter"
#pragma resource "*.dfm"
#pragma resource "RibbonNotepadDemoAppGlyphs.res"
#pragma resource "WindowsXP.res"
TfrmRibbonNotepadMain *frmRibbonNotepadMain;
//---------------------------------------------------------------------------

const
  int AnAccentCount = 5;

  TColor AStandardColorMap[] =
	{(TColor)0x0000C0, (TColor)0x0000FF, (TColor)0x00C0FF, (TColor)0x00FFFF, (TColor)0x50D092, (TColor)0x50B000, (TColor)0xF0B000, (TColor)0xC07000, (TColor)0x602000, (TColor)0xA03070};

  TColorMapInfo AColorMaps[] =
  {
	{"Default", {clWindow, clWindowText, (TColor)0xD2B48C, (TColor)0x00008B, (TColor)0x0000FF, (TColor)0xFF0000, (TColor)0x556B2F, (TColor)0x800080, clAqua, (TColor)0xFFA500}},
	{"Theme1", {clWindow, clWindowText, (TColor)0x7D491F, (TColor)0xE1ECEE, (TColor)0xBD814F, (TColor)0x4D50C0, (TColor)0x59BB9B, (TColor)0xA26480, (TColor)0xC6AC4B, (TColor)0x4696F7}},
	{"Theme2", {clWindow, clWindowText, (TColor)0x6D6769, (TColor)0xD1C2C9, (TColor)0x66B9CE, (TColor)0x84B09C, (TColor)0xC9B16B, (TColor)0xCF8565, (TColor)0xC96B7E, (TColor)0xBB79A3}},
	{"Theme3", {clWindow, clWindowText, (TColor)0x323232, (TColor)0xD1DEE3, (TColor)0x097FF0, (TColor)0x36299F, (TColor)0x7C581B, (TColor)0x42854E, (TColor)0x784860, (TColor)0x5998C1}},
	{"Theme4", {clWindow, clWindowText, (TColor)0x866B64, (TColor)0xD7D1C5, (TColor)0x4963D1, (TColor)0x00B4CC, (TColor)0xAEAD8C, (TColor)0x707B8C, (TColor)0x8CB08F, (TColor)0x4990D1}},
	{"Theme5", {clWindow, clWindowText, (TColor)0x464646, (TColor)0xFAF5DE, (TColor)0xBFA22D, (TColor)0x281FDA, (TColor)0x1B64EB, (TColor)0x9D6339, (TColor)0x784B47, (TColor)0x4A3C7D}}
  };

float DPIRatio()
{
  return Screen->PixelsPerInch / 96;
}

  void __fastcall SetBuffer(System::Word Format, void *Buffer, int Size);

__fastcall TColorPickerController::TColorPickerController(TdxRibbonGalleryItem *AColorMapItem, TdxRibbonColorGalleryItem* AColorGallery, TdxCustomRibbon *ARibbon) : TObject()
{
  FColorMapItem = AColorMapItem;
  FColorGallery = AColorGallery;
  FColorGlyphSize = cxTextHeight(ARibbon->Fonts->Group, "Wg", 0);

  FColorMapItem->GalleryOptions->ColumnCount = 1;
  FColorMapItem->GalleryOptions->SpaceBetweenItemsAndBorder = 0;
  FColorMapItem->GalleryOptions->ItemTextKind = itkCaption;
  FColorMapItem->GalleryGroups->Add();
  FColorMapItem->OnGroupItemClick = ColorMapItemClick;
  
  BuildColorSchemeGallery();
}

//---------------------------------------------------------------------------

void __fastcall TColorPickerController::ColorMapItemClick(TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem)
{
  FColorGallery->Theme = TdxRibbonColorGalleryTheme(AItem->Tag);
  ColorMapChanged();
}

//---------------------------------------------------------------------------

TcxAlphaBitmap* TColorPickerController::CreateColorBitmap(TColor AColor, int AGlyphSize)
{
  TcxAlphaBitmap *Result;

  if (AGlyphSize == 0)
    AGlyphSize = FColorGlyphSize;
  Result = new TcxAlphaBitmap(AGlyphSize, AGlyphSize);
  Result->Canvas->Brush->Color = AColor;
  Result->Canvas->Pen->Color = clGray;
  Result->Canvas->FillRect(Result->ClientRect);
  Result->Canvas->FrameRect(Result->ClientRect);
  if (AColor == clNone)
	Result->RecoverAlphaChannel(clBlack);
  else
	Result->TransformBitmap(btmSetOpaque);

  return Result;
}
//---------------------------------------------------------------------------

int GetBrightness (int ARGBColor)
{
  return (GetBValue(ARGBColor) + GetGValue(ARGBColor) + GetRValue(ARGBColor)) / 3;
}

void CreateAccent(TAccent *AnAccents, int AMapIndex, TColor *AColorMap, TColor **AnAccentColorScheme)
{
  TColor AColor;

  for (int I = 0; I < AnAccentCount; I++)
  {
	switch (AnAccents[I])
    {
	case aLight80: AColor = dxGetLighterColor(AColorMap[AMapIndex], 80); break;
	case aLight60: AColor = dxGetLighterColor(AColorMap[AMapIndex], 60); break;
	case aLight50: AColor = dxGetLighterColor(AColorMap[AMapIndex], 50); break;
	case aLight40: AColor = dxGetLighterColor(AColorMap[AMapIndex], 40); break;
	case aLight35: AColor = dxGetLighterColor(AColorMap[AMapIndex], 35); break;
	case aLight25: AColor = dxGetLighterColor(AColorMap[AMapIndex], 25); break;
	case aLight15: AColor = dxGetLighterColor(AColorMap[AMapIndex], 15); break;
	case aLight5: AColor = dxGetLighterColor(AColorMap[AMapIndex], 5); break;
	case aDark10: AColor = dxGetDarkerColor(AColorMap[AMapIndex], 90); break;
	case aDark25: AColor = dxGetDarkerColor(AColorMap[AMapIndex], 75); break;
	case aDark50: AColor = dxGetDarkerColor(AColorMap[AMapIndex], 50); break;
	case aDark75: AColor = dxGetDarkerColor(AColorMap[AMapIndex], 25); break;
	default /*aDark90*/: AColor = dxGetDarkerColor(AColorMap[I], 10);
    }
    AnAccentColorScheme[I][AMapIndex] = AColor;
  }
}

void GetAccentColorScheme(TColor *AColorMap, TColor **AnAccentColorScheme)
{
  TAccent AccentMap1[] = {aLight50, aLight35, aLight25, aLight15, aLight5};
  TAccent AccentMap2[] = {aLight80, aLight60, aLight60, aDark25, aDark50};
  TAccent AccentMap3[] = {aDark10, aDark25, aDark50, aDark75, aDark90};
  for (int I = 0; I < SchemeColorCount; I++)
  {
    if (GetBrightness(ColorToRGB(AColorMap[I])) < 20)
      CreateAccent(AccentMap1, I, AColorMap, AnAccentColorScheme);
    else
      if (GetBrightness(ColorToRGB(AColorMap[I])) < 230)
        CreateAccent(AccentMap2, I, AColorMap, AnAccentColorScheme);
      else
        CreateAccent(AccentMap3, I, AColorMap, AnAccentColorScheme);
  }
};
//---------------------------------------------------------------------------

void TColorPickerController::BuildColorSchemeGallery()
{
  int ASystemColorCount = 2;
  int AGlyphOffset = 1;
  TcxAlphaBitmap *ABitmap;
  TcxAlphaBitmap *AColorBitmap;
  TRect ARect;
  TdxRibbonGalleryGroupItem *AGroupItem;
  int AThemeColorCount;

  FColorMapItem->BarManager->BeginUpdate();
  try
  {
	AThemeColorCount = SchemeColorCount - ASystemColorCount;
	ABitmap = new TcxAlphaBitmap(FColorGlyphSize * AThemeColorCount + (AThemeColorCount - 1) * AGlyphOffset, FColorGlyphSize);
	try
	{
	  for (int I = 5; I >= 0; I--)
	  {
		AGroupItem = FColorMapItem->GalleryGroups->Items[0]->Items->Insert(0);
		for (int J = 0 + ASystemColorCount; J < SchemeColorCount; J++)
		{
		  AColorBitmap = CreateColorBitmap(AColorMaps[I].Map[J], 0);
		  try
		  {
			ARect = cxRectOffset(AColorBitmap->ClientRect, (AColorBitmap->Width + AGlyphOffset) * (J - ASystemColorCount), 0);
			ABitmap->CopyBitmap(AColorBitmap, ARect, cxNullPoint, SRCCOPY);
		  }
		  __finally
		  {
			delete AColorBitmap;
		  }
		}
		AGroupItem->Glyph->Assign(ABitmap);
		AGroupItem->Caption = AColorMaps[I].Name;
		AGroupItem->Tag = I;
	  };
	  AGroupItem->Selected = true;
	}
	__finally
	{
	  delete ABitmap;
	}
  }
  __finally
  {
	FColorMapItem->BarManager->EndUpdate(true);
  }
}
//---------------------------------------------------------------------------

void TColorPickerController::FillGlyph(TcxAlphaBitmap *AGlyph)
{
  TRect ARect;
  HDC ADC;

  ARect = Rect(0, 0, AGlyph->Width / 2, AGlyph->Height / 2);
  AGlyph->Canvas->Pen->Color = clGray;
  AGlyph->Canvas->Brush->Color = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[2];
  AGlyph->Canvas->FillRect(ARect);
  AGlyph->Canvas->Brush->Color = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[3];
  AGlyph->Canvas->FillRect(cxRectOffset(ARect, cxRectWidth(ARect), 0));
  AGlyph->Canvas->Brush->Color = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[4];
  AGlyph->Canvas->FillRect(cxRectOffset(ARect, 0, cxRectHeight(ARect)));
  AGlyph->Canvas->Brush->Color = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[5];
  AGlyph->Canvas->FillRect(cxRectOffset(ARect, cxRectWidth(ARect), cxRectHeight(ARect)));
  AGlyph->Canvas->FrameRect(AGlyph->ClientRect);
  AGlyph->TransformBitmap(btmSetOpaque);
}
//---------------------------------------------------------------------------

void TColorPickerController::ColorMapChanged()
{
  TcxAlphaBitmap *AGlyph;

  FColorMapItem->BarManager->BeginUpdate();
  try
  {
    AGlyph = new TcxAlphaBitmap(16, 16);
    FillGlyph(AGlyph);
    FColorMapItem->Glyph->Assign(AGlyph);
    AGlyph->SetSize(32, 32);
    FillGlyph(AGlyph);
    FColorMapItem->LargeGlyph->Assign(AGlyph);
    delete AGlyph;
  }
  __finally
  {
    FColorMapItem->BarManager->EndUpdate(False);
  }
}
//---------------------------------------------------------------------------

TdxRibbonRecentDocumentsController::TdxRibbonRecentDocumentsController(
	TdxRibbonBackstageViewGalleryControl* ARecentPaths, 
	TdxRibbonBackstageViewGalleryControl* ACurrentFolder, 
	TdxRibbonBackstageViewGalleryControl* ARecentDocuments,
	TdxBarApplicationMenu* AApplicationMenu) :
	TRecentDocumentsController()
{
  FApplicationMenu = AApplicationMenu;
	
  FCurrentFolder = ACurrentFolder;
  FCurrentFolder->Tag = 1;

  FRecentDocuments = ARecentDocuments;
  FRecentDocuments->Gallery->Groups->Add();
  FRecentDocuments->Tag = 0;

  FRecentPaths = ARecentPaths;
  FRecentPaths->Gallery->Groups->Add();
  FRecentPaths->Tag = 1;
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::Add(const String AFileName)
{
  InternalAdd((TdxRibbonBackstageViewGalleryGroup*)(FRecentDocuments->Gallery->Groups->Items[0]), AFileName);
  InternalAdd((TdxRibbonBackstageViewGalleryGroup*)(FRecentPaths->Gallery->Groups->Items[0]), ExtractFileDir(AFileName));
  UpdateApplicationMenu();
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::SetCurrentFileName(const String AFileName)
{
  FCurrentFolder->Gallery->Groups->BeginUpdate();
  try
  {
	FCurrentFolder->Gallery->Groups->Clear();
	if (ExtractFileDir(AFileName) != "")
	  InternalAdd(FCurrentFolder->Gallery->Groups->Add(), ExtractFileDir(AFileName));
  }
  __finally
  {
	FCurrentFolder->Gallery->Groups->EndUpdate();
  }
}
//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem* TdxRibbonRecentDocumentsController::GetItemByValue(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue)
{
  TdxRibbonBackstageViewGalleryItem* AResult;
  for (int I = 0; I <= AGroup->Items->Count - 1; I++)
	if (SameText(AGroup->Items->Items[I]->Hint, AValue))
	  return AGroup->Items->Items[I];
  return NULL;
}
//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem* TdxRibbonRecentDocumentsController::InternalAdd(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue)
{
  TdxRibbonBackstageViewGalleryItem* AResult;
  AResult = GetItemByValue(AGroup, AValue);
  if (AResult == NULL)
  {
	AResult = AGroup->Items->Add();
	AResult->Caption = ExtractFileName(AValue);
	AResult->Hint = AValue;
	AResult->Description = AValue;
	AResult->ImageIndex = AGroup->GetParentComponent()->Tag;
  }
  AResult->Index = 0;

  while (AGroup->Items->Count > 10)
	AGroup->Items->Delete(9);
  return AResult;
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::InternalLoad(TdxRibbonBackstageViewGalleryGroup* AGroup, TCustomIniFile* AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem* AItem;
  AGroup->Items->BeginUpdate();
  try
  {
	AGroup->Items->Clear();
	for (int I = 0; I <= AIniFile->ReadInteger(ASection, "Count", 0) - 1; I++)
	{
	  AItem = InternalAdd(AGroup, AIniFile->ReadString(ASection, IntToStr(I), ""));
	  AItem->Pinned = AIniFile->ReadBool(ASection, IntToStr(I) + "Pinned", False);
	}
  }
  __finally
  {
	AGroup->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::InternalSave(TdxRibbonBackstageViewGalleryGroup* AGroup, TCustomIniFile* AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem *AItem;
  AIniFile->EraseSection(ASection);
  AIniFile->WriteInteger(ASection, "Count", AGroup->Items->Count);
  for (int I = 0; I <= AGroup->Items->Count - 1; I++)
  {
	AItem = AGroup->Items->Items[I];
	AIniFile->WriteString(ASection, IntToStr(I), AItem->Hint);
	AIniFile->WriteBool(ASection, IntToStr(I) + "Pinned", AItem->Pinned);
  }
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::DoLoad(TCustomIniFile* AConfig)
{
  InternalLoad((TdxRibbonBackstageViewGalleryGroup*)(FRecentDocuments->Gallery->Groups->Items[0]), AConfig, "RecentDocuments");
  InternalLoad((TdxRibbonBackstageViewGalleryGroup*)(FRecentPaths->Gallery->Groups->Items[0]), AConfig, "RecentPaths");
  UpdateApplicationMenu();
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::DoSave(TCustomIniFile* AConfig)
{
  InternalSave((TdxRibbonBackstageViewGalleryGroup*)(FRecentDocuments->Gallery->Groups->Items[0]), AConfig, "RecentDocuments");
  InternalSave((TdxRibbonBackstageViewGalleryGroup*)(FRecentPaths->Gallery->Groups->Items[0]), AConfig, "RecentPaths");
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::UpdateApplicationMenu()
{
  TdxBarExtraPaneItem* AItem;
  TdxRibbonBackstageViewGalleryGroup* AGroup;

  FApplicationMenu->ExtraPaneItems->BeginUpdate();
  FApplicationMenu->ExtraPaneItems->Clear();
  AGroup = FRecentDocuments->Gallery->Groups->Groups[0];
  for (int I = 0; I < AGroup->ItemCount; I++)
  {
      AItem = FApplicationMenu->ExtraPaneItems->Add();
	  AItem->DisplayText = AGroup->Items->Items[I]->Caption;
	  AItem->Pin = AGroup->Items->Items[I]->Pinned;
      AItem->Text = AGroup->Items->Items[I]->Hint;
  }
  FApplicationMenu->ExtraPaneItems->EndUpdate();
}

//---------------------------------------------------------------------------

__fastcall TfrmRibbonNotepadMain::TfrmRibbonNotepadMain(TComponent* Owner)
	: TfrmNotepadMain(Owner)
{
}
//---------------------------------------------------------------------------

TRibbonDemoStyle TfrmRibbonNotepadMain::GetRibbonDemoStyle()
{
  switch(Ribbon->Style)
  {
	case rs2007:
	  return rdsOffice2007;
	case rs2013:
	  return rdsOffice2013;
	case rs2016:
	  return rdsOffice2016;
	case rs2016Tablet:
	  return rdsOffice2016Tablet;
	case rs2019:
	  return rdsOffice2019;
  }
  if (Ribbon->EnableTabAero)
	return rdsOffice2010;
  else
	return rdsScenic;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::SetColorScheme(const String AName)
{
  String ASkinName;
  scgiLookAndFeel->SelectedSkinName = AName;
  Ribbon->ColorSchemeName = AName;
  UpdateColorSchemeRelatedControls();

  ASkinName = AName;
  if (cxLookAndFeelPaintersManager->GetPainter(AName) == NULL)
  {
	if (Ribbon->ColorScheme->Style == rs2013)
	  ASkinName = "Office2013White";
  }

  cxLookAndFeelController->BeginUpdate();
  try
  {
	cxLookAndFeelController->SkinName = ASkinName;
	cxLookAndFeelController->NativeStyle = cxLookAndFeelPaintersManager->GetPainter(ASkinName) == NULL;
  }
  __finally
  {
	cxLookAndFeelController->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::SetRibbonDemoStyle(TRibbonDemoStyle AStyle)
{
  const String NamesMap[rs2019 + 1] = {"RIBBONAPPGLYPH", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010"};
  Ribbon->Style = RibbonDemoStyleToRibbonStyle(AStyle);
  if ((AStyle == rdsOffice2007) || (AStyle == rdsScenic))
  {
	Ribbon->EnableTabAero = False;
	Ribbon->ApplicationButton->Menu = ApplicationMenu;
	rtHelp->Visible = True;
  }
  else
  {
	Ribbon->ApplicationButton->Menu = BackstageView;
	Ribbon->EnableTabAero = True;
	rtHelp->Visible = False;
  }

  Ribbon->ApplicationButton->Glyph->LoadFromResource((unsigned int)HInstance, NamesMap[Ribbon->Style], (wchar_t*)RT_BITMAP);
  Ribbon->ApplicationButton->StretchGlyph = Ribbon->Style == rs2007;
  Ribbon->TabAreaToolbar->Visible = (Ribbon->Style >= rs2016);
  Ribbon->TabAreaSearchToolbar->Visible = (Ribbon->Style >= rs2016);
  dxbSearchOptions->Visible =  (Ribbon->Style >= rs2016);
  lbbvTabCaption2010->Visible = AStyle == rdsOffice2010;
  lbbvTabCaption2013->Visible = (AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet) || (AStyle == rdsOffice2019);
  if (AStyle == rdsOffice2016Tablet)
	bbQATAboveRibbon->Visible = ivNever;
  else
	bbQATAboveRibbon->Visible = ivAlways;
  bbQATBelowRibbon->Visible = bbQATAboveRibbon->Visible;
  if (AStyle == rdsOffice2007)
	bsepSaveAs->Visible = ivAlways;
  else
	bsepSaveAs->Visible = ivNever;
  if ((AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet) || (AStyle == rdsOffice2019))
	gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2013->Style->Font) + gbLocationsMain->Margins->Bottom;
  else
	gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2010->Style->Font) + gbLocationsMain->Margins->Bottom;

  DisableAero = (AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet)|| (AStyle == rdsOffice2019);
  scgiLookAndFeel->PopulateGallery();
  SetColorScheme(Ribbon->ColorSchemeName);
  UpdateOptionsVisibility();
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::AssignFontColorGlyph()
{
  TcxAlphaBitmap *AGlyph;

  AGlyph = FColorPickerController->CreateColorBitmap(cgiFontColor->Color, 16);
  try
  {
    cgiFontColor->Glyph->Assign(AGlyph);
  }
  __finally
  {
    delete AGlyph;
  };
}
//---------------------------------------------------------------------------

#if (__BORLANDC__ >= 0x0660) // C++Builder XE4
void _fastcall TfrmRibbonNotepadMain::EditorContextPopup(System::TObject* Sender, const System::Types::TPoint &MousePos, bool &Handled)
#else

#if (__BORLANDC__ >= 0x0650) // C++Builder XE3
	#ifndef _WIN64
void _fastcall TfrmRibbonNotepadMain::EditorContextPopup(System::TObject* Sender, System::Types::TPoint &MousePos, bool &Handled)
	#else
void _fastcall TfrmRibbonNotepadMain::EditorContextPopup(System::TObject* Sender, System::Types::TPoint MousePos, bool &Handled)
	#endif
#else
void _fastcall TfrmRibbonNotepadMain::EditorContextPopup(System::TObject* Sender, const Types::TPoint &MousePos, bool &Handled)
#endif
#endif

{
  if (bbRadialMenu->Down)
	RibbonRadialMenu->PopupFromCursorPos();
  else
	if (Editor->SelLength != 0)
	  MiniToolbar->Popup(dxBarPopupMenu);
	else
	  dxBarPopupMenu->PopupFromCursorPos();

  Handled = True;
}
//---------------------------------------------------------------------------

void _fastcall TfrmRibbonNotepadMain::EditorMouseUp(System::TObject* Sender, TMouseButton Button, Classes::TShiftState Shift, int X, int Y)
{
  if ((Button == mbLeft) && (Editor->SelLength != 0))
	MiniToolbar->Popup();
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::AddItem(TdxRibbonGalleryGroup* AGroup, Integer ACode)
{
  TdxRibbonGalleryGroupItem* AItem;
  String AFont;
  TcxAlphaBitmap *ABitmap;
  AItem = AGroup->Items->Add();
  AFont = "Times New Roman";
  AItem->Caption = AFont + " #" + IntToStr(ACode);
  AItem->Description = AFont;
  AItem->Tag = ACode;

  ABitmap = CreateBitmap(AFont, WideChar(ACode));
  try
  {
	AItem->Glyph->Assign(ABitmap);
  }
  __finally
  {
	ABitmap->Free();
  }
}
//---------------------------------------------------------------------------

TcxAlphaBitmap* TfrmRibbonNotepadMain::CreateBitmap(const String AFont, WideChar AChar)
{
  Integer AGlyphSize;
  TRect R;
  TcxAlphaBitmap* Result;
  AGlyphSize = Round(32 * DPIRatio());
  R = Rect(0, 0, AGlyphSize, AGlyphSize);
  Result = new TcxAlphaBitmap(cxRectWidth(R), cxRectHeight(R));
  Result->Canvas->Brush->Color = (TColor)0xFAFAFA;
  Result->Canvas->FillRect(R);
  Result->Canvas->Font->Name = AFont;
  Result->Canvas->Font->Color = (TColor)0x5C534C;
  Result->Canvas->Font->Size = Round(16 * DPIRatio());
  DrawTextW(Result->Canvas->Handle, &AChar, 1, &R, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
  Result->TransformBitmap(btmSetOpaque);
  return Result;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::PopulateGroup(Integer AGroupIndex, Integer AMap[])
{
  for (int I = 0; I <= Round(sizeof(AMap)/sizeof(Integer)); I++)
	AddItem(rgiItemSymbol->GalleryGroups->Items[AGroupIndex], AMap[I]);
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::InitSymbolGallery()
{
  Integer CurrencyMap[5] = {0x20AC, 0x24, 0xA3, 0xA5, 0x20A3};
  Integer GreekMap[10] = {0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA};
  Integer MathMap[8] = {0xB1, 0x2260, 0x2264, 0x2265, 0xF7, 0xD7, 0x221E, 0x2211};
  Integer SymbolMap[3] = {0xA9, 0xAE, 0x2122};
  dxBarManager->BeginUpdate();
  try
  {
	Integer A;
	PopulateGroup(0, MathMap);
	PopulateGroup(1, GreekMap);
	PopulateGroup(2, SymbolMap);
	PopulateGroup(3, CurrencyMap);
  }
  __finally
  {
	dxBarManager->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::UpdateColorSchemeRelatedControls()
{
  TColor AColor;
  AColor = Ribbon->ColorScheme->GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR);
  lbComputer->Style->TextColor = AColor;
  lbRecentDocuments->Style->TextColor = AColor;
  lbUserInterfaceOptions->Style->TextColor = AColor;
  lbRibbonStyle->Style->TextColor = AColor;
  lbScreenTipStyle->Style->TextColor = AColor;
  lbPersonalizationOptions->Style->TextColor = AColor;
  lbColorScheme->Style->TextColor = AColor;
  lbColorSchemeAccent->Style->TextColor = AColor;
  lbRibbonBackgroundImageCaption->Style->TextColor = AColor;
  lbRibbonBackgroundImageDescription->Style->TextColor = AColor;
  lbbvTabCaption2010->Style->TextColor = AColor;
  lbbvTabCaption2013->Style->TextColor = AColor;
  lbRecentFolders->Style->TextColor = AColor;
  lbCurrentFolder->Style->TextColor = AColor;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::UpdateImageIndexes()
{
  bbBarsHelp->ImageIndex = 18;
  bbDockingHelp->ImageIndex = 18;
  dxBarLargeButton1->ImageIndex = 20;
  bbDXSupport->ImageIndex = 20;
  bbDXDownloads->ImageIndex = 20;
  bbDXOnWeb->ImageIndex = 20;
  bbDXProducts->ImageIndex = 20;
  bbMyDX->ImageIndex = 20;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::UpdateOptionsVisibility()
{
  BOOLEAN AWasActive;

  AWasActive = bvtsOptions->Active;
  bvtsOptions->TabVisible = (RibbonDemoStyle == rdsOffice2013) || (RibbonDemoStyle == rdsOffice2016) || (RibbonDemoStyle == rdsOffice2016Tablet);
  if (AWasActive && (!bvtsOptions->TabVisible))
	bvtsOpen->Active = True;
  bbOptions->Visible = VisibleTodxBarVisible(!bvtsOptions->TabVisible);
  gbRibbonBackgroundImagePane->Visible = (DisableAero || (!Ribbon->SupportNonClientDrawing)) &&
	((RibbonDemoStyle == rdsOffice2013) || (RibbonDemoStyle == rdsOffice2016) || (RibbonDemoStyle == rdsOffice2016Tablet));
}
//---------------------------------------------------------------------------

TfrmNotepadChild* TfrmRibbonNotepadMain::CreateChildForm()
{
  TfrmRibbonNotepadChild* Result = new TfrmRibbonNotepadChild(this);
#ifndef _WIN64
  Result->Editor->OnContextPopup = EditorContextPopup;
#else
  Result->Editor->OnContextPopup = (TContextPopupEvent)EditorContextPopup;
#endif
  Result->Editor->OnMouseUp = EditorMouseUp;
  return Result;
}
//---------------------------------------------------------------------------

TRecentDocumentsController* TfrmRibbonNotepadMain::CreateRecentDocumentsController()
{
  return new TdxRibbonRecentDocumentsController(bvgcRecentDocuments, bvgcRecentPaths, bvgcCurrentFolder, ApplicationMenu);
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::DoUpdateControls(TfrmNotepadChild* AActiveChild)
{
  const int SelectionContextIndex = 0;
  TfrmNotepadMain::DoUpdateControls(AActiveChild);
  rgiItemSymbol->Enabled = AActiveChild != NULL;
  rgiColorTheme->Enabled = AActiveChild != NULL;
  cgiFontColor->Enabled = AActiveChild != NULL;
  bbUndo->Enabled = AActiveChild != NULL;
  tbZoom->Enabled = AActiveChild != NULL;
  bsZoom->Enabled = AActiveChild != NULL;

  if (AActiveChild != NULL)
  {
	tbZoom->Position = Round(100 * Editor->ActiveProperties->ZoomFactor);
	tbZoomPropertiesChange(NULL);
  }

  if ((AActiveChild != NULL) && (Editor->SelLength > 0))
	Ribbon->Contexts->Items[SelectionContextIndex]->Activate(False);
  else
	Ribbon->Contexts->Items[SelectionContextIndex]->Visible = False;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::InitializeLookAndFeel()
{
  RibbonDemoStyle = rdsOffice2019;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::UpdateUndoRelatedControls()
{
  TfrmNotepadMain::UpdateUndoRelatedControls();
  bbUndo->Enabled = acUndo->Enabled;
  bbUndoAll->Enabled = acUndo->Enabled;
}

void TfrmRibbonNotepadMain::InitOptions()
{
  PopulateRibbonStyles(cbRibbonStyle->Properties->Items);
  PopulateColorSchemeAccents(cbColorSchemeAccent->Properties->Items);

  cbRibbonStyle->ItemIndex = (Integer)RibbonDemoStyle;
  cbRibbonStylePropertiesChange(NULL);
  cbColorScheme->ItemIndex = Max(0, cbColorScheme->Properties->Items->IndexOf(Ribbon->ColorSchemeName));
  cbColorSchemeAccent->ItemIndex = (Integer)Ribbon->ColorSchemeAccent;
  if (dxBarManager->ShowHint)
	cbScreenTipStyle->ItemIndex = (Integer)(!dxBarScreenTipRepository1->ShowDescription);
  else
	cbScreenTipStyle->ItemIndex = 2;

  bbClearImage->Enabled = !Ribbon->BackgroundImage->Empty;
  OpenPictureDialog->DefaultExt = GraphicExtension((TGraphicClass)Ribbon->BackgroundImage->ClassType());
  OpenPictureDialog->Filter = GraphicFilter((TGraphicClass)Ribbon->BackgroundImage->ClassType());
}

void __fastcall TfrmRibbonNotepadMain::BackstageViewPopup(TObject *Sender)
{
  gbRecentPathsPaneCurrentFolder->Visible = bvgcCurrentFolder->Gallery->Groups->Count > 0;
  BackstageViewTabChanged(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::BackstageViewTabChanged(TObject *Sender)
{
  if (!ComponentState.Contains(csReading))
  {
	gbBackstageViewTabCaption->Parent = BackstageView->ActiveTab;
	lbbvTabCaption2010->Caption = BackstageView->ActiveTab->Caption;
	lbbvTabCaption2013->Caption = BackstageView->ActiveTab->Caption;
	if (bvtsOpen->Active || bvtsSaveAs->Active)
	{
	  bvgcLocationsRecentDocumentsGroup->Visible = BackstageView->ActiveTab == bvtsOpen;
	  if (bvgcLocationsRecentDocumentsGroup->Visible && !bvgcLocationsComputerItem->Checked)
		bvgcLocationsRecentDocumentsItem->Checked = True;
	  else
		bvgcLocationsComputerItem->Checked = True;

	  gbLocationsMain->Parent = BackstageView->ActiveTab;
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbApplicationButtonClick(TObject *Sender)
{
  Ribbon->ApplicationButton->Visible = bbApplicationButton->Down;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbClearImageClick(TObject *Sender)
{
  Ribbon->BackgroundImage->Clear();
  bbClearImage->Enabled = False;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::cgiFontColorColorChanged(TObject *Sender)
{
  Editor->SelAttributes->Color = cgiFontColor->Color;
  ActiveChild->UndoController->AddAction(6);
  AssignFontColorGlyph();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbLoadImageClick(TObject *Sender)
{
  if (OpenPictureDialog->Execute())
  {
	Ribbon->BackgroundImage->LoadFromFile(OpenPictureDialog->FileName);
	bbClearImage->Enabled = True;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbOptionsClick(TObject *Sender)
{
  TdxRibbonColorSchemeAccent AColorSchemeAccent;
  String AColorSchemeName;
  TScreenTipOptions *AScreenTipOptions = new TScreenTipOptions;
  TRibbonDemoStyle AStyle;
  AStyle = RibbonDemoStyle;
  AColorSchemeName = Ribbon->ColorSchemeName;
  AColorSchemeAccent = Ribbon->ColorSchemeAccent;
  AScreenTipOptions->ShowScreenTips = dxBarManager->ShowHint;
  AScreenTipOptions->ShowDescripitons = dxBarScreenTipRepository1->ShowDescription;
  if (ExecuteRibbonDemoOptions(&AColorSchemeName, AScreenTipOptions, &AStyle, &AColorSchemeAccent))
  {
	RibbonDemoStyle = AStyle;
	Ribbon->ColorSchemeAccent = AColorSchemeAccent;
	dxBarManager->ShowHint = AScreenTipOptions->ShowScreenTips;
	dxBarScreenTipRepository1->ShowDescription = AScreenTipOptions->ShowDescripitons;
	SetColorScheme(AColorSchemeName);
	InitOptions();
  }
  delete AScreenTipOptions;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::acQATAboveRibbonUpdate(TObject *Sender)
{
  acQATAboveRibbon->Checked = Ribbon->QuickAccessToolbar->Position == qtpAboveRibbon;
  acQATBelowRibbon->Checked = Ribbon->QuickAccessToolbar->Position == qtpBelowRibbon;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::acQATBelowRibbonExecute(TObject *Sender)
{
  if (((TAction *)Sender)->Tag != 0)
    Ribbon->QuickAccessToolbar->Position = qtpBelowRibbon;
  else
    Ribbon->QuickAccessToolbar->Position = qtpAboveRibbon;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbQATVisibleClick(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Visible = bbQATVisible->Down;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbRecursiveSearchClick(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Visible = bbQATVisible->Down;
  if (bbRecursiveSearch->Down)
    ((TdxOfficeSearchBoxProperties *)beOfficeSearchBox->Properties)->RecursiveSearch = bTrue;
  else
    ((TdxOfficeSearchBoxProperties *)beOfficeSearchBox->Properties)->RecursiveSearch = bFalse;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbRibbonFormClick(TObject *Sender)
{
  Ribbon->SupportNonClientDrawing = bbRibbonForm->Down;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbSaveAsRTFClick(TObject *Sender)
{
  ActiveChild->SaveFile(True);
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbSaveAsTextClick(TObject *Sender)
{
  ActiveChild->ExportAsPlainText();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbShowPathsClick(TObject *Sender)
{
  ((TdxOfficeSearchBoxProperties *)beOfficeSearchBox->Properties)->ShowResultPaths = bbShowPaths->Down;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbTouchModeClick(TObject *Sender)
{
  Integer AHeight;
  cxLookAndFeelController->TouchMode = bbTouchMode->Down;
  AHeight = tbZoom->Tag;
  dxAdjustToTouchableSize(AHeight);
  tbZoom->Height = AHeight;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::btnBrowsePathClick(TObject *Sender)
{
  Boolean AHandled;
  if (bvtsSaveAs->Active)
	AHandled = ActiveChild->SaveFile(True);
  else
	AHandled = OpenFile();

  if (AHandled)
	BackstageView->Hide();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bvgcLocationsItemClick(TObject *Sender,
	TdxRibbonBackstageViewGalleryItem *AItem)
{
  gbRecentDocumentsPane->Visible = bvgcLocationsRecentDocumentsItem->Checked;
  gbRecentPathsPane->Visible = bvgcLocationsComputerItem->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bvgcRecentDocumentsItemClick(TObject *Sender,
          TdxRibbonBackstageViewGalleryItem *AItem)
{
  OpenFile(AItem->Hint);
  BackstageView->Hide();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bvgcRecentPathsItemClick(TObject *Sender,
	TdxRibbonBackstageViewGalleryItem *AItem)
{
  Boolean AHandled;
  String APrevInitialDir;
  if (bvtsSaveAs->Active)
  {
	APrevInitialDir = ActiveChild->SaveDialog->InitialDir;
	try
	{
	  ActiveChild->SaveDialog->InitialDir = AItem->Hint;
	  AHandled = ActiveChild->SaveFile(True);
	}
	__finally
	{
	  ActiveChild->SaveDialog->InitialDir = APrevInitialDir;
	}
  }
  else
  {
	APrevInitialDir = OpenDialog->InitialDir;
	try
	{
	  OpenDialog->InitialDir = AItem->Hint;
	  AHandled = OpenFile();
	}
	__finally
	{
	  OpenDialog->InitialDir = APrevInitialDir;
	}
  }
  if (AHandled)
	BackstageView->Hide();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::cbColorSchemeAccentPropertiesChange(TObject *Sender)
{
  Ribbon->ColorSchemeAccent = (TdxRibbonColorSchemeAccent)cbColorSchemeAccent->ItemIndex;
  UpdateColorSchemeRelatedControls();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::cbColorSchemePropertiesChange(TObject *Sender)
{
  SetColorScheme(cbColorScheme->Text);
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::cbRibbonStylePropertiesChange(TObject *Sender)
{
  RibbonDemoStyle = (TRibbonDemoStyle)cbRibbonStyle->ItemIndex;

  cbColorSchemeAccent->Enabled = cbRibbonStyle->ItemIndex > 0;
  lbColorSchemeAccent->Enabled = cbRibbonStyle->ItemIndex > 0;

  String ASelectedColorSchemeName = cbColorScheme->Text;
  PopulateColorSchemes(cbColorScheme->Properties->Items, RibbonDemoStyleToRibbonStyle((TRibbonDemoStyle)cbRibbonStyle->ItemIndex));
  cbColorScheme->ItemIndex = Max(0, cbColorScheme->Properties->Items->IndexOf(ASelectedColorSchemeName));
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::cbScreenTipStylePropertiesChange(TObject *Sender)
{
  dxBarManager->ShowHint = cbScreenTipStyle->ItemIndex != 2;
  dxBarScreenTipRepository1->ShowDescription = cbScreenTipStyle->ItemIndex == 0;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::dxbFontAndColorsCaptionButtons0Click(TObject *Sender)
{
  acFont->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::FormCreate(TObject *Sender)
{
  String AAboutFileName;
  Integer ATextWidth;
  TfrmNotepadMain::FormCreate(Sender);
  tbZoom->Tag = tbZoom->Height;
  FColorPickerController = new TColorPickerController(rgiColorTheme, cgiFontColor, Ribbon);

  AAboutFileName = ExtractFilePath(Application->ExeName) + "About->txt";
  if (FileExists(AAboutFileName))
	meAbout->Lines->LoadFromFile(AAboutFileName);

  lblSupport->OnClick = dmCommonData->actSupport->OnExecute;
  lblHelpBars->OnClick = dmCommonData->actBarsHelp->OnExecute;
  lblHelpDocking->OnClick = dmCommonData->actDockingHelp->OnExecute;
  lblProducts->OnClick = dmCommonData->actProducts->OnExecute;
  lblClientCenter->OnClick = dmCommonData->actMyDX->OnExecute;
  lblDXonWeb->OnClick = dmCommonData->actDXOnTheWeb->OnExecute;
  lblDownloads->OnClick = dmCommonData->actDownloads->OnExecute;

  bbLogo->OnClick = dmCommonData->actDXOnTheWeb->OnExecute;

  ATextWidth = cxTextWidth(dxBarManager->Font, "Undo 9999 Actions");
  rgiUndo->GalleryOptions->ItemPullHighlighting->Active = True;
  rgiUndo->GalleryOptions->ColumnCount = 1;
  rgiUndo->GalleryOptions->SubmenuResizing = gsrNone;
  rgiUndo->GalleryOptions->ItemSize->Width = ATextWidth;
  int AValue1 = cxTextHeight(dxBarManager->Font);
  int AValue2 = Round(21 * DPIRatio());
  rgiUndo->GalleryOptions->ItemSize->Height = Max(AValue1, AValue2);
  rgiUndo->GalleryGroups->Add();

  bsSelectionInfo->Width = ATextWidth;
  bsSelectionInfo->Caption = "Cancel";

  bbRibbonForm->Down = Ribbon->SupportNonClientDrawing;
  bbTouchMode->Down = cxLookAndFeelController->TouchMode;
  bbApplicationButton->Down = Ribbon->ApplicationButton->Visible;
  bbQATVisible->Down = Ribbon->QuickAccessToolbar->Visible;
  AssignFontColorGlyph();
  InitSymbolGallery();
  UpdateImageIndexes();
  InitOptions();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::FormDestroy(TObject *Sender)
{
  delete FColorPickerController;
  TfrmNotepadMain::FormDestroy(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::rgiItemSymbolGroupItemClick(TdxRibbonGalleryItem *Sender,
          TdxRibbonGalleryGroupItem *AItem)
{
  WideChar AChar;
  AChar = WideChar(AItem->Tag);
  Editor->SelAttributes->Name = AItem->Description;
  Clipboard()->Open();
  try
  {
	Clipboard()->AsText = AChar;
  }
  __finally
  {
	Clipboard()->Close();
  }
  Editor->PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::rgiUndoGroupItemClick(TdxRibbonGalleryItem *Sender,
          TdxRibbonGalleryGroupItem *AItem)
{
  ActiveChild->UndoController->Undo(AItem->Index + 1);
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::rgiUndoHotTrackedItemChanged(TdxRibbonGalleryGroupItem *APrevHotTrackedGroupItem,
		  TdxRibbonGalleryGroupItem *ANewHotTrackedGroupItem)
{
  Integer ACount;
  String AString;
  if (ANewHotTrackedGroupItem != NULL)
  {
	ACount = ANewHotTrackedGroupItem->Index + 1;
	if (ACount == 1)
	  AString = " Action";
	else
	  AString = " Actions";

	bsSelectionInfo->Caption = "Undo " + IntToStr(ACount) + AString;
  }
  else
	bsSelectionInfo->Caption = "Cancel";
}
//---------------------------------------------------------------------------

Boolean TfrmRibbonNotepadMain::GetGlyphIndex(const String ASkinName, Integer &AIndex)
{
  const	String NameMap[8] = {"Blue", "Black", "Silver", "DarkGray", "LightGray", "White", "MediumGray", "Colorful"};
  for (int I = 0; I <= 7; I++)
	if (SameText(ASkinName, NameMap[I]))
	{
	  AIndex = I;
      return True;
	}
  return False;
}
//---------------------------------------------------------------------------

void TfrmRibbonNotepadMain::SetGlyph(TdxSmartGlyph* ABitmap, TcxImageList *AImageList, Integer AGlyphIndex)
{
  if ((Ribbon->Style == rs2016) || (Ribbon->Style == rs2016Tablet) || (Ribbon->Style == rs2019))
	switch (AGlyphIndex)
	{
		case 3: //DarkGray
		  AGlyphIndex = 1;
		  break;
		case 4: //LightGray
		  AGlyphIndex = 2;
		  break;
		case 6: //DarkGray
		  AGlyphIndex = 3;
		  break;
		case 7: //Colorful
		  AGlyphIndex = 0;
		  break;
	}
  TcxBitmap32* ABitmap32 = new TcxBitmap32(AImageList->Width, AImageList->Height, True);
  try
  {
	AImageList->Draw(ABitmap32->Canvas, 0, 0, AGlyphIndex);
	ABitmap->Assign(ABitmap32);
  }
  __finally
  {
	ABitmap32->Free();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::scgiLookAndFeelPopulate(TObject *Sender)
{
  const String DisplayNameMap[8] = {"Blue", "Black", "Silver", "Dark Gray", "Light Gray", "White", "Medium Gray", "Colorful"};
  const String RibbonColorSchemesGroupName = "Ribbon Color Schemes";
  Integer AIndex;
  TdxCustomRibbonSkin* ASkin;
  TdxSkinDetails* ASkinDetails;
  TdxSkinChooserGalleryGroupItem* ASkinItem;

  for (int I = 0; I <= dxRibbonSkinsManager()->SkinCount - 1; I++)
  {
	ASkin = dxRibbonSkinsManager()->Skins[I];
	if (ASkin->Style == Ribbon->Style)
	{
		if (ASkin->InheritsFrom(__classid(TdxSkinRibbonPainter)))
		{
			if (((TdxSkinRibbonPainter*)(ASkin))->Painter->GetPainterDetails(&ASkinDetails))
				scgiLookAndFeel->AddSkin(ASkinDetails);
		}
		else

		if (GetGlyphIndex(ASkin->Name, AIndex))
		{
			ASkinItem = scgiLookAndFeel->AddSkin(ASkin->Name, RibbonColorSchemesGroupName);
			ASkinItem->Caption = DisplayNameMap[AIndex];
			SetGlyph(ASkinItem->GlyphInDropDown, ilLargeColorSchemesGlyphs, AIndex);
			SetGlyph(ASkinItem->Glyph, ilSmallColorSchemesGlyphs, AIndex);
		}
	}
  }
}
//---------------------------------------------------------------------------



void __fastcall TfrmRibbonNotepadMain::scgiLookAndFeelSelected(TObject *Sender, const UnicodeString ASkinName)
{
  SetColorScheme(ASkinName);
  cbColorScheme->ItemIndex = Max(0, cbColorScheme->Properties->Items->IndexOf(Ribbon->ColorSchemeName));
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::tbZoomPropertiesChange(TObject *Sender)
{
  const String AStr = "%d %%";
  TVarRec V[] = {tbZoom->Position};
  bsZoom->Caption = Format(AStr, V, 0);
  if (FUpdatingControls == 0)
	Editor->ActiveProperties->ZoomFactor = (double (tbZoom->Position)) / 100;
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::UndoDropDownGalleryPopup(TObject *Sender)
{
  TdxRibbonGalleryGroup* AGroup;
  rgiUndo->GalleryCategories->BeginUpdate();
  try
  {
	rgiUndo->GalleryCategories->Clear();
	AGroup = rgiUndo->GalleryCategories->Add();
	for (int I = 0; I <= ActiveChild->UndoController->Actions->Count - 1; I++)
	  AGroup->Items->Add()->Caption = ActiveChild->UndoController->Actions->Strings[I];
  }
  __finally
  {
	rgiUndo->GalleryCategories->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmRibbonNotepadMain::bbUndoAllClick(TObject *Sender)
{
  ActiveChild->UndoController->Undo(MaxInt);
}
//---------------------------------------------------------------------------
void __fastcall TfrmRibbonNotepadMain::ApplicationMenuExtraPaneItemClick(TObject* Sender, int AIndex)
{
	OpenFile(ApplicationMenu->ExtraPaneItems->Items[AIndex]->Text);
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#if __BORLANDC__ < 0x610   // BCB version < 12
  #define HDC unsigned int
#endif

#include "RibbonNotepadDemoMain.h"
#include "RibbonNotepadDemoOptions.h"
#include "RibbonNotepadDemoGallerySetup.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGraphics"
#pragma link "dxBar"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxBarApplicationMenu"
#pragma link "dxRibbonSkins"
#pragma link "dxGDIPlusClasses"
#pragma link "dxRibbonBackstageView"
#pragma link "dxGDIPlusAPI"
#pragma link "dxRibbonMiniToolbar"
#pragma link "dxScreenTip"
#pragma link "cxBarEditItem"
#pragma link "cxContainer"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxSkinChooserGallery"
#pragma link "dxGalleryControl"
#pragma link "dxRibbonBackstageViewGalleryControl" 
#pragma link "dxGallery"
#pragma link "cxGroupBox" 
#pragma link "cxLabel"
#pragma link "cxButtons" 
#pragma link "dxBevel" 
#pragma link "cxScrollBox"
#pragma link "dxBarSkinConsts"
#pragma link "dxRibbonRadialMenu"
#pragma resource "*.dfm"
#pragma resource "RibbonNotepadDemoAppGlyphs.res"

const
  AnAccentCount = 5;

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

TRibbonDemoMainForm *RibbonDemoMainForm;
char
  *sRichEditFoundResultCaption = "Information",
  *sRichEditTextNotFound = "The search text is not found.",
  *sRichEditReplaceAllResult = "Replaced %d occurances.",
  *sDefaultDocName = "Document1.rtf";

char
  *RTFFilter = "Rich Text Files (*.RTF)|*.RTF",
  *TXTFilter = "Plain text (*.TXT)|*.TXT";
  
__fastcall TColorPickerController::TColorPickerController(TdxRibbonGalleryItem *AColorItem, TdxRibbonGalleryItem *AColorMapItem, TdxRibbonPopupMenu *AFontColorPopupMenu, TdxCustomRibbon *ARibbon) : TObject()
{
  FRibbon = ARibbon;
  FColorItem = AColorItem;
  FColorMapItem = AColorMapItem;
  FColorGlyphSize = cxTextHeight(FRibbon->Fonts->Group, "Wg", 0);
  FColorDialog = new TColorDialog(NULL);

  InitColorMapItem();
  InitColorItem();
  InitDropDownGallery(AFontColorPopupMenu);
  PopulateGalleries();
  SelectDefaultColor();
}
//---------------------------------------------------------------------------

__fastcall TColorPickerController::~TColorPickerController()
{
  delete FColorDialog;
}
//---------------------------------------------------------------------------

void TColorPickerController::InitColorItem()
{
  FColorItem->GalleryOptions->EqualItemSizeInAllGroups = False;
  FColorItem->GalleryOptions->ColumnCount = SchemeColorCount;
  FColorItem->GalleryOptions->SpaceBetweenGroups = 4;
  FColorItem->OnGroupItemClick = ColorItemClick;
  FColorItem->GalleryOptions->ItemTextKind = itkNone;

  FThemeColorsGroup = FColorItem->GalleryGroups->Add();
  FThemeColorsGroup->Header->Caption = "Theme Colors";
  FThemeColorsGroup->Header->Visible = true;
  FAccentColorsGroup = FColorItem->GalleryGroups->Add();
  FStandardColorsGroup = FColorItem->GalleryGroups->Add();
  FStandardColorsGroup->Header->Caption = "Standard Colors";
  FStandardColorsGroup->Header->Visible = true;
  FCustomColorsGroup = FColorItem->GalleryGroups->Add();
  FCustomColorsGroup->Header->Caption = "Custom Colors";
  FColorItem->GalleryGroups->Items[1]->Options->SpaceBetweenItemsVertically = -1;
}
//---------------------------------------------------------------------------

void TColorPickerController::InitColorMapItem()
{
  FColorMapItem->GalleryOptions->ColumnCount = 1;
  FColorMapItem->GalleryOptions->SpaceBetweenItemsAndBorder = 0;
  FColorMapItem->GalleryOptions->ItemTextKind = itkCaption;
  FColorMapItem->GalleryGroups->Add();
  FColorMapItem->OnGroupItemClick = ColorMapItemClick;
}
//---------------------------------------------------------------------------

void TColorPickerController::InitDropDownGallery(TdxRibbonPopupMenu *AFontColorPopupMenu)
{
  TcxAlphaBitmap *ANoColorGlyph;

  FNoColorButton = (TdxBarButton*)AFontColorPopupMenu->ItemLinks->AddButton()->Item;
  FNoColorButton->ButtonStyle = bsChecked;
  FNoColorButton->Caption = "&No Color";
  FNoColorButton->OnClick = NoColorButtonClick;
  ANoColorGlyph = CreateColorBitmap(clNone, 16);
  FNoColorButton->Glyph = ANoColorGlyph;
  delete ANoColorGlyph;
  AFontColorPopupMenu->ItemLinks->Add(FColorItem);
  FMoreColorsButton = (TdxBarButton*)AFontColorPopupMenu->ItemLinks->AddButton()->Item;
  FMoreColorsButton->Caption = "&More Colors...";
  FMoreColorsButton->OnClick = MoreColorsClick;
  FColorDialogSetup = (TdxBarButton*)AFontColorPopupMenu->ItemLinks->AddButton()->Item;
  FColorDialogSetup->Caption = "&Setup...";
  FColorDialogSetup->OnClick = ColorDialogSetupButtonClick;

}
//---------------------------------------------------------------------------

void TColorPickerController::PopulateGalleries()
{
  BuildColorSchemeGallery();
  BuildStandardColorGallery();
}
//---------------------------------------------------------------------------

void TColorPickerController::SelectDefaultColor()
{
  FNoColorButton->Click();
}
//---------------------------------------------------------------------------

TdxBarManager* TColorPickerController::GetBarManager()
{
  return FColorItem->BarManager;
}
//---------------------------------------------------------------------------

void TColorPickerController::SetColor(TColor Value)
{
  if (FColor != Value)
  {
    FColor = Value;
    ColorChanged();
  }
}
//---------------------------------------------------------------------------

void __fastcall TColorPickerController::ColorItemClick(TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem)
{
  FNoColorButton->Down = false;
  if (FColorItem->SelectedGroupItem != NULL)
    SetColor((TColor)FColorItem->SelectedGroupItem->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TColorPickerController::ColorMapItemClick(TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem)
{
  BuildThemeColorGallery();
  ColorMapChanged();
}
//---------------------------------------------------------------------------

void __fastcall TColorPickerController::NoColorButtonClick(TObject *Sender)
{
  if (FColorItem->SelectedGroupItem != NULL)
	FColorItem->SelectedGroupItem->Selected = false;
  SetColor(clNone);
}
//---------------------------------------------------------------------------

void __fastcall TColorPickerController::ColorDialogSetupButtonClick(TObject *Sender)
{
  bool RemoveHorizontalPadding, RemoveVerticalPadding;
  RemoveHorizontalPadding = FColorItem->GalleryOptions->SpaceBetweenItemsHorizontally == -1;
  RemoveVerticalPadding = FColorItem->GalleryGroups->Items[1]->Options->SpaceBetweenItemsVertically == -1;
  if (ColorDialogSetupForm->GetSettings(RemoveHorizontalPadding, RemoveVerticalPadding))
  {
	FColorItem->GalleryOptions->SpaceBetweenItemsHorizontally =
	  -RemoveHorizontalPadding;
	FColorItem->GalleryGroups->Items[1]->Options->SpaceBetweenItemsVertically =
	  -RemoveVerticalPadding;
  }
}
//---------------------------------------------------------------------------

void __fastcall TColorPickerController::MoreColorsClick(TObject *Sender)
{
  FColorDialog->Color = Color;
  if (FColorDialog->Execute())
  {
    FCustomColorsGroup->Header->Visible = true;
    AddColorItem(FCustomColorsGroup, FColorDialog->Color)->Selected = true;
  };
}
//---------------------------------------------------------------------------

TdxRibbonGalleryGroupItem* TColorPickerController::AddColorItem(TdxRibbonGalleryGroup *AGalleryGroup, TColor AColor)
{
  TcxAlphaBitmap *ABitmap;
  String AColorName;
  TdxRibbonGalleryGroupItem *Result;

  Result = AGalleryGroup->Items->Add();

  ABitmap = CreateColorBitmap(AColor, 0);
  try
  {
    Result->Glyph->Assign(ABitmap);
    if (cxNameByColor(AColor, AColorName))
      Result->Caption = AColorName;
    else
      Result->Caption = "$" + IntToHex(AColor, 6);
    Result->Tag = AColor;
  }
  __finally
  {
    ABitmap->Free();
  };

  return Result;
}

//---------------------------------------------------------------------------

TcxAlphaBitmap* TColorPickerController::CreateColorBitmap(TColor AColor, int AGlyphSize)
{
  TcxAlphaBitmap *Result;

  if (AGlyphSize == 0)
    AGlyphSize = FColorGlyphSize;
  Result = new TcxAlphaBitmap(AGlyphSize, AGlyphSize);
  FillRectByColor((HDC)Result->Canvas->Handle, Result->ClientRect, AColor);
  FrameRectByColor((HDC)Result->Canvas->Handle, Result->ClientRect, clGray);
  if (AColor == clNone)
    Result->RecoverAlphaChannel(clBlack);
  else
	Result->TransformBitmap(btmSetOpaque);

  return Result;
}
//---------------------------------------------------------------------------

void TColorPickerController::CreateColorRow(TdxRibbonGalleryGroup *AGalleryGroup, const TColor *AColorMap)
{
  int I;
  for (I = 0; I < SchemeColorCount; I++)
    AddColorItem(AGalleryGroup, AColorMap[I]);
}
//---------------------------------------------------------------------------

int GetBrightness (int ARGBColor)
{
  return (GetBValue(ARGBColor) + GetGValue(ARGBColor) + GetRValue(ARGBColor)) / 3;
}

void CreateAccent(TAccent *AnAccents, int AMapIndex, TColor *AColorMap, TColor **AnAccentColorScheme)
{
  int I;
  TColor AColor;

  for (I = 0; I < AnAccentCount; I++)
  {
    switch (AnAccents[I])
    {
      case aLight80: AColor = Light(AColorMap[AMapIndex], 80); break;
      case aLight60: AColor = Light(AColorMap[AMapIndex], 60); break;
      case aLight50: AColor = Light(AColorMap[AMapIndex], 50); break;
      case aLight40: AColor = Light(AColorMap[AMapIndex], 40); break;
      case aLight35: AColor = Light(AColorMap[AMapIndex], 35); break;
      case aLight25: AColor = Light(AColorMap[AMapIndex], 25); break;
      case aLight15: AColor = Light(AColorMap[AMapIndex], 15); break;
      case aLight5: AColor = Light(AColorMap[AMapIndex], 5); break;
      case aDark10: AColor = Dark(AColorMap[AMapIndex], 90); break;
      case aDark25: AColor = Dark(AColorMap[AMapIndex], 75); break;
      case aDark50: AColor = Dark(AColorMap[AMapIndex], 50); break;
      case aDark75: AColor = Dark(AColorMap[AMapIndex], 25); break;
      default /*aDark90*/: AColor = Dark(AColorMap[I], 10);
    }
    AnAccentColorScheme[I][AMapIndex] = AColor;
  }
}

void GetAccentColorScheme(TColor *AColorMap, TColor **AnAccentColorScheme)
{
  int I;
  TAccent AccentMap1[] = {aLight50, aLight35, aLight25, aLight15, aLight5};
  TAccent AccentMap2[] = {aLight80, aLight60, aLight60, aDark25, aDark50};
  TAccent AccentMap3[] = {aDark10, aDark25, aDark50, aDark75, aDark90};
  for (I = 0; I < SchemeColorCount; I++)
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

void TColorPickerController::BuildThemeColorGallery()
{
  int I;
  TColor *AColorMap;
  TColor *AnAccentColorScheme[AnAccentCount];

  for (I = 0; I < AnAccentCount; I++)
    AnAccentColorScheme[I] = new TColor[SchemeColorCount];

  BarManager->BeginUpdate();
  try
  {
    FThemeColorsGroup->Items->Clear();
    AColorMap = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map;
    CreateColorRow(FThemeColorsGroup, AColorMap);

    FAccentColorsGroup->Items->Clear();
    GetAccentColorScheme(AColorMap, AnAccentColorScheme);
    for (I = 0; I < AnAccentCount; I++)
      CreateColorRow(FAccentColorsGroup, AnAccentColorScheme[I]);
  }
  __finally
  {
    BarManager->EndUpdate(true);
  };

  for (I = 0; I < AnAccentCount; I++)
    delete AnAccentColorScheme[I];
}
//---------------------------------------------------------------------------

void TColorPickerController::BuildStandardColorGallery()
{
  BarManager->BeginUpdate();
  try
  {
    FStandardColorsGroup->Items->Clear();
    CreateColorRow(FStandardColorsGroup, AStandardColorMap);
  }
  __finally
  {
	BarManager->EndUpdate(true);
  };
}
//---------------------------------------------------------------------------

void TColorPickerController::BuildColorSchemeGallery()
{
  int ASystemColorCount = 2;
  int AGlyphOffset = 1;
  int I, J;
  TcxAlphaBitmap *ABitmap;
  TcxAlphaBitmap *AColorBitmap;
  TRect ARect;
  TdxRibbonGalleryGroupItem *AGroupItem;
  int AThemeColorCount;

  BarManager->BeginUpdate();
  try
  {
    AThemeColorCount = SchemeColorCount - ASystemColorCount;
    ABitmap = new TcxAlphaBitmap(FColorGlyphSize * AThemeColorCount + (AThemeColorCount - 1) * AGlyphOffset, FColorGlyphSize);
    try
    {
      for (I = 5; I>=0; I--)
      {
		AGroupItem = FColorMapItem->GalleryGroups->Items[0]->Items->Insert(0);
        for (J = 0 + ASystemColorCount; J < SchemeColorCount; J++)
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
    BarManager->EndUpdate(true);
  }
}
//---------------------------------------------------------------------------

void TColorPickerController::ColorChanged()
{
  TcxAlphaBitmap *AGlyph;

  AGlyph = CreateColorBitmap(Color, 16 * DPIRatio());
  try
  {
    FColorItem->Glyph.Assign(AGlyph);
  }
  __finally
  {
    delete AGlyph;
  };

  if (FOnColorChanged)
    FOnColorChanged(this);
}
//---------------------------------------------------------------------------

void TColorPickerController::FillGlyph(TcxAlphaBitmap *AGlyph)
{
  TRect ARect;
  HDC ADC;

  ARect = Rect(0, 0, AGlyph->Width / 2, AGlyph->Height / 2);
  ADC = (HDC)AGlyph->Canvas->Handle;
  FillRectByColor(ADC, ARect, AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[2]);
  FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), 0), AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[3]);
  FillRectByColor(ADC, cxRectOffset(ARect, 0, cxRectHeight(ARect)), AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[4]);
  FillRectByColor(ADC, cxRectOffset(ARect, cxRectWidth(ARect), cxRectHeight(ARect)), AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map[5]);
  FrameRectByColor(ADC, AGlyph->ClientRect, clGray);
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

__fastcall TRibbonRecentsController::TRibbonRecentsController(
	TdxRibbonBackstageViewGalleryControl *ARecentDocuments, 
	TdxRibbonBackstageViewGalleryControl *ARecentPaths,
	TdxRibbonBackstageViewGalleryControl *ACurrentFolder) : TObject()
{
  FCurrentFolder = ACurrentFolder;
  FCurrentFolder->Tag = 1;

  FRecentDocuments = ARecentDocuments;
  FRecentDocuments->Gallery->Groups->Add();
  FRecentDocuments->Tag = 0;

  FRecentPaths = ARecentPaths;
  FRecentPaths->Gallery->Groups->Add();
  FRecentPaths->Tag = 1;

  LoadFromIniFile(GetConfigFileName());
}

//---------------------------------------------------------------------------

__fastcall TRibbonRecentsController::~TRibbonRecentsController()
{
  SaveToIniFile(GetConfigFileName());
};

//---------------------------------------------------------------------------

void TRibbonRecentsController::Add(const String AFileName)
{
  InternalAdd(FRecentDocuments->Gallery->Groups->Groups[0], AFileName);
  InternalAdd(FRecentPaths->Gallery->Groups->Groups[0], ExtractFileDir(AFileName));
}
//---------------------------------------------------------------------------

void TRibbonRecentsController::LoadFromIniFile(const String AFileName)
{
  TIniFile *AIniFile = new TIniFile(AFileName);
  try
  {
	InternalLoad(FRecentDocuments->Gallery->Groups->Groups[0], AIniFile, "RecentDocuments");
    InternalLoad(FRecentPaths->Gallery->Groups->Groups[0], AIniFile, "RecentPaths");
  }
  __finally
  {
    delete AIniFile;
  }
}

//---------------------------------------------------------------------------

void TRibbonRecentsController::SaveToIniFile(const String AFileName)
{
  TIniFile *AIniFile = new TIniFile(AFileName);
  try
  {
	InternalSave(FRecentDocuments->Gallery->Groups->Groups[0], AIniFile, "RecentDocuments");
	InternalSave(FRecentPaths->Gallery->Groups->Groups[0], AIniFile, "RecentPaths");
  }
  __finally
  {
     delete AIniFile;
  };
};

//---------------------------------------------------------------------------

void TRibbonRecentsController::SetCurrentFileName(const String AFileName)
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
  };
};

//---------------------------------------------------------------------------

String TRibbonRecentsController::GetConfigFileName()
{
  return ExtractFilePath(Application->ExeName) + "Recents.ini";
};

//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem *TRibbonRecentsController::GetItemByValue(
	TdxRibbonBackstageViewGalleryGroup *AGroup, const String AValue)
{
  for (int I = 0; I < AGroup->Items->Count; I++)
	if (SameText(AGroup->Items->Items[I]->Hint, AValue))
      return AGroup->Items->Items[I];
	
  return NULL;
};

//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem *TRibbonRecentsController::InternalAdd(
	TdxRibbonBackstageViewGalleryGroup *AGroup, const String AValue)
{
  TdxRibbonBackstageViewGalleryItem *Result = GetItemByValue(AGroup, AValue);
  if (Result == NULL)
  {
	Result = AGroup->Items->Add();
    Result->Caption = ExtractFileName(AValue);
	Result->Hint = AValue;
	Result->Description = AValue;
	Result->ImageIndex = AGroup->GetParentComponent()->Tag;
  };
  Result->Index = 0;

  while (AGroup->Items->Count > 10)
	AGroup->Items->Delete(9);

  return Result;
}

//---------------------------------------------------------------------------

void TRibbonRecentsController::InternalLoad(
	TdxRibbonBackstageViewGalleryGroup *AGroup, TCustomIniFile *AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem *AItem;
  
  AGroup->Items->BeginUpdate();
  try
  {
    AGroup->Items->Clear();
	for (int I = 0; I < AIniFile->ReadInteger(ASection, "Count", 0); I++)
	{
	  AItem = InternalAdd(AGroup, AIniFile->ReadString(ASection, IntToStr(I), ""));
	  AItem->Pinned = AIniFile->ReadBool(ASection, IntToStr(I) + "Pinned", False);
    }
  }
  __finally
  {
    AGroup->Items->EndUpdate();
  };
};

//---------------------------------------------------------------------------

void TRibbonRecentsController::InternalSave(
  TdxRibbonBackstageViewGalleryGroup *AGroup, TCustomIniFile *AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem *AItem;

  AIniFile->EraseSection(ASection);
  AIniFile->WriteInteger(ASection, "Count", AGroup->Items->Count);
  for (int I = 0; I < AGroup->Items->Count; I++)
  {
	AItem = AGroup->Items->Items[I];
	AIniFile->WriteString(ASection, IntToStr(I), AItem->Hint);
    AIniFile->WriteBool(ASection, IntToStr(I) + "Pinned", AItem->Pinned);
  }
};
 
//---------------------------------------------------------------------------
__fastcall TRibbonDemoMainForm::TRibbonDemoMainForm(TComponent* Owner)
        : TdxCustomRibbonForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::ApplicationMenuRecentDocumentsClick(TObject *Sender, int AIndex)
{
  OpenFile(ApplicationMenu->ExtraPaneItems->Items[AIndex]->Text);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonNewClick(TObject *Sender)
{
  if (EditorIsModified)
	switch (QuerySaveFile())
	{
	  case ID_YES:
	  {
		if (SaveFile(false))
		{
		  MakeNewDocument();
		}
		break;
	  }
	  case ID_NO:
	  {
        MakeNewDocument();
      }
    }
  else {
    MakeNewDocument();
  }
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonOpenClick(TObject *Sender)
{
  OpenFile("");
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonSaveClick(TObject *Sender)
{
  SaveFile(false);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonSaveAsRTFClick(TObject *Sender)
{
   SaveFile(true);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonSaveAsTextClick(TObject *Sender)
{
   SaveFile(true, true);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonPrintClick(TObject *Sender)
{
  if (PrintDialog->Execute()) Editor->Print(FileName);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonUndoClick(TObject *Sender)
{
  Undo(1);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonCutClick(TObject *Sender)
{
  Editor->CutToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonCopyClick(TObject *Sender)
{
  Editor->CopyToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonPasteClick(TObject *Sender)
{
  Editor->PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonClearClick(TObject *Sender)
{
  Editor->ClearSelection();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonSelectAllClick(TObject *Sender)
{
  Editor->SelectAll();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonFindClick(TObject *Sender)
{
  Editor->SelLength = 0;
  FindDialog->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonReplaceClick(TObject *Sender)
{
  Editor->SelLength = 0;
  ReplaceDialog->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarComboFontNameChange(TObject *Sender)
{
  if (!FUpdating)
    Editor->SelAttributes->Name = dxBarComboFontName->EditValue;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarComboFontSizeChange(TObject *Sender)
{
  if (!FUpdating)
    Editor->SelAttributes->Size = dxBarComboFontSize->EditValue;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonBoldClick(TObject *Sender)
{
  if (dxBarButtonBold->Down)
    Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsBold;
  else
    Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsBold;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonItalicClick(TObject *Sender)
{
  if (dxBarButtonItalic->Down)
    Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsItalic;
  else
    Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsItalic;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonUnderlineClick(TObject *Sender)
{
  if (dxBarButtonUnderline->Down)
    Editor->SelAttributes->Style = Editor->SelAttributes->Style << fsUnderline;
  else
    Editor->SelAttributes->Style = Editor->SelAttributes->Style >> fsUnderline;
}
//---------------------------------------------------------------------------


void __fastcall TRibbonDemoMainForm::dxBarButtonBulletsClick(TObject *Sender)
{
  Editor->Paragraph->Numbering = TNumberingStyle(dxBarButtonBullets->Down);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::btnLockedClick(TObject *Sender)
{
  AnsiString AHint;
  Editor->Properties->ReadOnly = ((TdxBarButton*)Sender)->Down;
  if (Editor->Properties->ReadOnly)
  {
	AHint = "Editing protection: Read only. Click for editing.";
	cxStyle1->TextColor = clMaroon;
  }
  else
  {
    AHint = "Editing protection: Writable. Click for read-only mode.";
    cxStyle1->TextColor = clGray;
  }
  ((TdxBarButton*)Sender)->Hint = AHint;
  EditorSelectionChange(0);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonAlignClick(TObject *Sender)
{
  if (((TdxBarLargeButton*)Sender)->Down)
    Editor->Paragraph->Alignment = ((TAlignment)((TdxBarLargeButton*)Sender)->Tag);
  else
    Editor->Paragraph->Alignment = taLeftJustify;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonProtectedClick(TObject *Sender)
{
  Editor->SelAttributes->Protected = !Editor->SelAttributes->Protected;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarUseRibbonFormClick(TObject *Sender)
{
  Ribbon->SupportNonClientDrawing = !Ribbon->SupportNonClientDrawing;
  dxBarLargeButton7->Enabled = Ribbon->SupportNonClientDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarLargeButton7Click(TObject *Sender)
{
  Ribbon->ApplicationButton->Visible = !Ribbon->ApplicationButton->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarLargeButton8Click(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Position = qtpAboveRibbon;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarLargeButton9Click(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Position = qtpBelowRibbon;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarLargeButton10Click(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Visible = !Ribbon->QuickAccessToolbar->Visible;
  dxBarLargeButton8->Enabled = Ribbon->QuickAccessToolbar->Visible;
  dxBarLargeButton9->Enabled = Ribbon->QuickAccessToolbar->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::EditorChange(TObject *Sender)
{
  if (Editor == NULL) return;

  RibbonAutoHideMode->HideUI();
  Editor->Properties->OnSelectionChange(Editor);
  SetModified(EditorIsModified);
  dxBarButtonUndo->Enabled = SendMessage(Editor->InnerControl->Handle, EM_CANUNDO, 0, 0) != 0;
  RibbonDemoMainForm->FEditorUndoController->AnalyseMessage();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::EditorSelectionChange(TObject *Sender)
{
  FUpdating = true;
  dxBarComboFontSize->OnChange = 0;
  dxBarComboFontName->OnChange = 0;
  try
  {
	btnLineNumber->Caption = Format((String)" Line: %3d ", ARRAYOFCONST((1 + EditorRow)));
	btnColumnNumber->Caption = Format((String)" Position: %3d ", ARRAYOFCONST((1 + EditorCol)));

	dxBarButtonCopy->Enabled = Editor->SelLength > 0;
	dxBarButtonCut->Enabled = dxBarButtonCopy->Enabled;
	dxBarButtonPaste->Enabled = SendMessage(Editor->InnerControl->Handle, EM_CANPASTE, 0, 0) != 0;
	dxBarButtonClear->Enabled = dxBarButtonCopy->Enabled;

	dxBarComboFontSize->EditValue = IntToStr(Editor->SelAttributes->Size);
	dxBarComboFontName->EditValue = Editor->SelAttributes->Name;

	dxBarButtonBold->Down = Editor->SelAttributes->Style.Contains(fsBold);
	dxBarButtonItalic->Down = Editor->SelAttributes->Style.Contains(fsItalic);
	dxBarButtonUnderline->Down = Editor->SelAttributes->Style.Contains(fsUnderline);

	dxBarButtonBullets->Down = ((bool)Editor->Paragraph->Numbering);
	switch (Editor->Paragraph->Alignment) {
	 case taLeftJustify: dxBarButtonAlignLeft->Down = true; break;
	 case taRightJustify: dxBarButtonAlignRight->Down = true; break;
	 case taCenter: dxBarButtonCenter->Down = true;
	}

	dxBarButtonProtected->Down = Editor->SelAttributes->Protected;

	FUpdating = false;
  }
  __finally
  {
    dxBarComboFontSize->OnChange = dxBarComboFontSizeChange;
    dxBarComboFontName->OnChange = dxBarComboFontNameChange;
  }
  if (dxBarButtonCopy->Enabled) 
    Ribbon->Contexts->Items[0]->Activate(false);
  else
    Ribbon->Contexts->Items[0]->Visible = false;

}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::EditorMouseUp(
	TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if ((Button == mbLeft) && (Editor->SelLength > 0))
    MiniToolbar->Popup();
  if (Button == mbRight)
    if (Editor->SelLength > 0)
      MiniToolbar->Popup(dxBarPopupMenu);
    else
      dxBarPopupMenu->PopupFromCursorPos();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FindOne(TObject *Sender)
{
  int StartPos, FindLength, FoundAt;
  TSearchTypes Flags;
  TPoint P;
  TRect CaretR, R, IntersectR;
  TFindDialog *ADialog = ((TFindDialog*) Sender);

  if (ADialog->Options.Contains(frDown))
  {
    if (Editor->SelLength == 0) StartPos = Editor->SelStart;
    else StartPos = Editor->SelStart + Editor->SelLength;
    FindLength = Editor->Text.Length() - StartPos;
  }
  else
  {
    StartPos = Editor->SelStart;
    FindLength = -StartPos;
  }
  Flags.Clear();
  if (ADialog->Options.Contains(frMatchCase)) Flags << stMatchCase;
  if (ADialog->Options.Contains(frWholeWord)) Flags << stWholeWord;
  Screen->Cursor = crHourGlass;
  FoundAt = Editor->FindTexT(ADialog->FindText, StartPos, FindLength, Flags);
  if (!(ADialog->Options.Contains(frReplaceAll))) Screen->Cursor = crDefault;
  if (FoundAt > -1)
    if (ADialog->Options.Contains(frReplaceAll))
    {
      Editor->SelStart = FoundAt;
      Editor->SelLength = ADialog->FindText.Length();
    }
    else
    {
      Editor->SetFocus();
      Editor->SelStart = FoundAt;
      Editor->SelLength = ADialog->FindText.Length();

      GetCaretPos(&P);
      P = Editor->ClientToScreen(P);
      CaretR = Rect(P.x, P.y, P.x + 2, P.y + 20);
      GetWindowRect(Editor->Handle, &R);
      if (IntersectRect(&IntersectR, &CaretR, &R))
        if (P.y < Screen->Height / 2)
          ADialog->Top = P.y + 40;
        else
          ADialog->Top = P.y - (R.bottom - R.top + 20);
      if (ADialog->Top < 0) ADialog->Top = 0;    
    }
  else
    if (!(ADialog->Options.Contains(frReplaceAll)))
      MessageBox(0, sRichEditTextNotFound,
        sRichEditFoundResultCaption, MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FontClick(TObject *Sender)
{
  FontDialog->Font->Assign(Editor->SelAttributes);
  if (FontDialog->Execute())
    Editor->SelAttributes->Assign(FontDialog->Font);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FormCreate(TObject *Sender)
{
  FRecents = new TRibbonRecentsController(bvgcRecentDocuments, bvgcRecentPaths, bvgcCurrentFolder);

  OpenDialog->Filter = RTFFilter;
  OpenDialog->InitialDir = ExtractFilePath(ParamStr(0));
  SaveDialog->InitialDir = OpenDialog->InitialDir;
  ShowItems(true);


  dxBarButton7->LargeImageIndex = 18;
  dxBarButton7->ImageIndex = 18;
  dxBarLargeButton1->LargeImageIndex = 18;
  dxBarLargeButton1->ImageIndex = 18;
  dxBarButton1->LargeImageIndex = 19;
  dxBarButton1->ImageIndex = 19;
  dxBarButton3->LargeImageIndex = 20;
  dxBarButton3->ImageIndex = 20;
  dxBarButton4->LargeImageIndex = 21;
  dxBarButton4->ImageIndex = 21;
  dxBarButton5->LargeImageIndex = 22;
  dxBarButton5->ImageIndex = 22;

  FColorPickerController = new TColorPickerController(rgiFontColor, rgiColorTheme, ppmFontColor, Ribbon);
  FColorPickerController->OnColorChanged = FontColorChanged;
  AssignFontColorGlyph();

  InitSymbolGallery();

  FEditorUndoController = new TRichEditUndoController(rgiUndo, (TRichEdit *)Editor->InnerControl);

  int ATextWidth = cxTextWidth(BarManager->Font, "Undo 9999 Actions");

  rgiUndo->GalleryOptions->ItemPullHighlighting->Active = True;
  rgiUndo->GalleryOptions->ColumnCount = 1;
  rgiUndo->GalleryInMenuOptions->DropDownGalleryResizing = gsrNone;
  rgiUndo->GalleryOptions->ItemSize->Width = ATextWidth;
  rgiUndo->GalleryOptions->ItemSize->Height = 21 * DPIRatio();
  rgiUndo->GalleryGroups->Add();
  bstSelectionInfo->Width = ATextWidth;
  bstSelectionInfo->Caption = "Cancel";

  RibbonAboutDemoLoad(meAbout->Lines);
  lblSupport->OnClick = dmCommonData->actSupport->OnExecute;
  lblHelpBars->OnClick = dmCommonData->actBarsHelp->OnExecute;
  lblHelpDocking->OnClick = dmCommonData->actDockingHelp->OnExecute;
  lblProducts->OnClick = dmCommonData->actProducts->OnExecute;
  lblClientCenter->OnClick = dmCommonData->actMyDX->OnExecute;
  lblDXonWeb->OnClick = dmCommonData->actDXOnTheWeb->OnExecute;
  lblDownloads->OnClick = dmCommonData->actDownloads->OnExecute;

  EditorIsModified = false;
  if (FileExists(sDefaultDocName))
	OpenFile(ExpandFileName((String)sDefaultDocName));
  else
	FileName = sDefaultDocName;

  RibbonDemoStyle = rdsOffice2013;
  bvgcLocationsComputerItem->Checked = True;  
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FormDestroy(TObject *Sender)
{
  delete FEditorUndoController;
  delete FColorPickerController;
  delete FRecents;
}

//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::SetRibbonDemoStyle(TRibbonDemoStyle AStyle)
{
	Ribbon->Style = RibbonDemoStyleToRibbonStyle(AStyle);
	if ((AStyle == rdsOffice2007) || (AStyle == rdsScenic))
	{
		Ribbon->EnableTabAero = false;
		Ribbon->ApplicationButton->Menu = ApplicationMenu;
		tabHelp->Visible = true;
	}
	else
	{
		Ribbon->ApplicationButton->Menu = BackstageView;
		Ribbon->EnableTabAero = true;
		tabHelp->Visible = false;
	}

	if (Ribbon->Style == rs2007)
		Ribbon->ApplicationButton->Glyph->LoadFromResourceName((unsigned int)HInstance, "RIBBONAPPGLYPH");
	else
		Ribbon->ApplicationButton->Glyph->LoadFromResourceName((unsigned int)HInstance, "RIBBONAPPGLYPH2010");

	lbbvTabCaption2010->Visible = AStyle == rdsOffice2010;
    lbbvTabCaption2013->Visible = AStyle == rdsOffice2013;
	if (AStyle == rdsOffice2013)
		gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2013->Style->Font) + bvSpacer7->Height;
	else
		gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2010->Style->Font) + bvSpacer7->Height;
		
	Ribbon->ApplicationButton->StretchGlyph = Ribbon->Style == rs2007;
	tabHelp->Visible = AStyle != rdsOffice2010;
	DisableAero = AStyle == rdsOffice2013;
	dxSkinChooserGalleryItem1->PopulateGallery();
	SetColorScheme(Ribbon->ColorSchemeName);
};

void __fastcall TRibbonDemoMainForm::FormActivate(TObject *Sender)
{
  if (FCanOnChange)
  {
	Editor->Properties->OnChange(Editor);
  }
  else {
    FCanOnChange = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  if (EditorIsModified)
	switch (QuerySaveFile())
	{
	  case ID_YES:
		CanClose = SaveFile(false);
		return;

	  case ID_NO:
		CanClose = true;
		return;

	  case ID_CANCEL:
	  	CanClose = false;
	}
}
//---------------------------------------------------------------------------

int __fastcall TRibbonDemoMainForm::GetEditorCol()
{
  return (Editor->SelStart - SendMessage(Editor->InnerControl->Handle, EM_LINEINDEX, EditorRow, 0));
}
//---------------------------------------------------------------------------

int __fastcall TRibbonDemoMainForm::GetEditorRow()
{
  return (SendMessage(Editor->InnerControl->Handle, EM_LINEFROMCHAR, Editor->SelStart, 0));
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::MakeNewDocument()
{
  Editor->Clear();
  SetModified(False);
  FileName = sDefaultDocName;
  FRecents->SetCurrentFileName(FileName);
}
//---------------------------------------------------------------------------

bool __fastcall TRibbonDemoMainForm::OpenFile(const String AFileName)
{
  bool Result = false;
  if (EditorIsModified)
  {
	switch (QuerySaveFile())
	{
	  case mrYes:
		if (! SaveFile(false))
			return false;
		break;

	  case mrCancel:
		return false;
    }

  }
  OpenDialog->FileName = AFileName;
  if ((AFileName != "") || OpenDialog->Execute()) 
  {
    FileName = OpenDialog->FileName;
    Editor->Lines->LoadFromFile(FileName);
	FRecents->Add(FileName);
    FRecents->SetCurrentFileName(FileName);
    SetModified(false);
	Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------

int __fastcall TRibbonDemoMainForm::QuerySaveFile()
{
  AnsiString S = "Do you want to save the changes you made to " + Ribbon->DocumentName + "?";
  AnsiString Title = Application->Title;
  return (MessageBox(0, S.c_str(), Title.c_str(), /*MB_ICONQUESTION ||*/ MB_YESNOCANCEL));
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::ReplaceOne(TObject *Sender)
{
  TReplaceDialog *ADialog = ((TReplaceDialog*) Sender);
  int ReplacedCount, OldSelStart, PrevSelStart;
  AnsiString S;

  ReplacedCount = 0;
  OldSelStart = Editor->SelStart;
  if (ADialog->Options.Contains(frReplaceAll)) Screen->Cursor = crHourGlass;
  do
  {
    if ((Editor->SelLength > 0) && ((Editor->SelText == ADialog->FindText) ||
      (!(ADialog->Options.Contains(frMatchCase)) &&
       (AnsiUpperCase(Editor->SelText) == AnsiUpperCase(ADialog->FindText)))))
    {
      Editor->SelText = ADialog->ReplaceText;
      ReplacedCount++;
    }
    PrevSelStart = Editor->SelStart;
    FindOne(Sender);
  }
  while (ADialog->Options.Contains(frReplaceAll) && (Editor->SelStart != PrevSelStart));
  if (ADialog->Options.Contains(frReplaceAll))
  {
    Screen->Cursor = crDefault;
    if (ReplacedCount == 0) 
      S = sRichEditTextNotFound;
    else
    {
      Editor->SelStart = OldSelStart;
	  S = Format((String)sRichEditReplaceAllResult, ARRAYOFCONST((ReplacedCount)));
    }
    MessageBox(0, S.c_str(), sRichEditFoundResultCaption, MB_ICONINFORMATION);
  }
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::RibbonResize(TObject *Sender)
{
  if (Ribbon->QuickAccessToolbar->Position == qtpAboveRibbon)
    dxBarLargeButton8->Down = true;
  else
    dxBarLargeButton9->Down = true;
}
//---------------------------------------------------------------------------

bool __fastcall TRibbonDemoMainForm::SaveFile(bool ASaveAs, bool APlainText)
{
  bool Result;

  SaveDialog->FileName = ChangeFileExt(ExtractFileName(FileName), "");

  if (APlainText)
    SaveDialog->Filter = TXTFilter;
  else
    SaveDialog->Filter = RTFFilter;

  ASaveAs = ASaveAs || (FileName == "") || (FileName == sDefaultDocName);
  Result = !ASaveAs || SaveDialog->Execute();
  if (Result)
  {
    Editor->Properties->PlainText = APlainText;
	FRecents->Add(SaveDialog->FileName);
    Editor->Lines->SaveToFile(SaveDialog->FileName);
    if (!APlainText)
    {
      SetModified(false);
      if (ASaveAs)
        FileName = SaveDialog->FileName;
    };
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::SetColorScheme(String AName)
{
  Ribbon->ColorSchemeName = AName;

  Panel1->Color = Ribbon->ColorScheme->GetPartColor(rfspRibbonForm);
  lbComputer->Style->TextColor = Ribbon->ColorScheme->GetPartColor(rspTabHeaderText, DXBAR_ACTIVE, Ribbon->ColorSchemeAccent);
  lbRecentDocuments->Style->TextColor = lbComputer->Style->TextColor;
  
  cxLookAndFeelController1->BeginUpdate();
  try
  {
    cxLookAndFeelController1->SkinName = AName;
    cxLookAndFeelController1->NativeStyle = cxLookAndFeelPaintersManager->GetPainter(AName) == NULL;
    dxSkinChooserGalleryItem1->SelectedSkinName = AName;
  }
  __finally
  {
    cxLookAndFeelController1->EndUpdate();
  };
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::BackstageViewPopup(TObject *Sender)
{
  gbRecentPathsPaneCurrentFolder->Visible = bvgcCurrentFolder->Gallery->Groups->Count > 0;
  BackstageViewTabChanged(Sender);
};
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonUndo2Click(TObject *Sender)
{
  Undo(1);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonRedoClick(TObject *Sender)
{
  Redo();
}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::dxBarButtonUndoAllClick(TObject *Sender)
{
  Undo(MaxInt);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::BackstageViewTabChanged(TObject *Sender)
{
  if (!ComponentState.Contains(csReading))
  {
    gbBackstageViewTabCaption->Parent = BackstageView->ActiveTab;
    lbbvTabCaption2010->Caption = BackstageView->ActiveTab->Caption;
    lbbvTabCaption2013->Caption = BackstageView->ActiveTab->Caption;
    if (bvtsOpen->Active || bvtsSaveAs->Active)
    {
      bvgcLocationsRecentDocumentsGroup->Visible = BackstageView->ActiveTab == bvtsOpen;
      if (!bvgcLocationsRecentDocumentsGroup->Visible)
        bvgcLocationsComputerItem->Checked = True;
      gbLocationsMain->Parent = BackstageView->ActiveTab;
    }
  }
}

//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::SetFileName(AnsiString Value)
{
  int AIndex;

  Ribbon->DocumentName = ExtractFileName(Value);
  FFileName = Value;

  if (FileExists(FFileName))
  {
    AIndex = ApplicationMenu->ExtraPaneItems->IndexOf(FFileName);
    if (AIndex > -1)
      ApplicationMenu->ExtraPaneItems->Delete(AIndex);
    ApplicationMenu->ExtraPaneItems->Insert(0)->Text = FFileName;
  }
}
//---------------------------------------------------------------------------
void TRibbonDemoMainForm::RibbonAboutDemoLoad(TStrings *AItems)
{
	AItems->LoadFromFile(ExtractFilePath(Application->ExeName) + "About.txt");
};
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::AssignFontColorGlyph()
{
  dxBarButtonFontColor->Glyph = rgiFontColor->Glyph;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::FontColorChanged(TObject *Sender)
{
  AssignFontColorGlyph();
  dxBarButtonFontColorClick(NULL);
}
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::SetFontColor()
{
  Editor->SelAttributes->Color = FColorPickerController->Color;
  FEditorUndoController->AddAction(6);
}
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::Undo(int Count)
{
  FEditorUndoController->Lock();
  try
  {
	while (Count > 0)
	{
	  SendMessage(Editor->InnerControl->Handle, EM_UNDO, 0, 0);
	  FEditorUndoController->PopUndo();
	  Count--;
	}
  }
  __finally
  {
	FEditorUndoController->UnLock();
  }
}
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::Redo()
{
  SendMessage(Editor->InnerControl->Handle, EM_REDO, 0, 0);
}
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::AddSymbolItem(TdxRibbonGalleryGroup *AGroup, int ACode)
{
  TdxRibbonGalleryGroupItem *AItem;
  String AFont;
  TcxAlphaBitmap *ABitmap;
  WideChar AChar;

  AItem = AGroup->Items->Add();
  AFont = "Times New Roman";
  AItem->Caption = AFont + " #" + IntToStr(ACode);
  AItem->Description = AFont;
  AItem->Tag = ACode;

  // CreateBitmap
  try
  {
	int AGlyphSize;
	TRect R;

	AGlyphSize = 32 * DPIRatio();
	R = Rect(0, 0, AGlyphSize, AGlyphSize);
	ABitmap = new TcxAlphaBitmap(AGlyphSize, AGlyphSize);
	ABitmap->Canvas->Brush->Color = (TColor)0xFAFAFA;
	ABitmap->Canvas->FillRect(R);
	ABitmap->Canvas->Font->Name = AFont;
	ABitmap->Canvas->Font->Color = (TColor)0x5C534C;
	ABitmap->Canvas->Font->Size = 16 * DPIRatio();
	AChar = (WideChar)ACode;
	DrawTextW(ABitmap->Canvas->Handle, &AChar, 1, &R, DT_CENTER || DT_VCENTER || DT_SINGLELINE);
	ABitmap->TransformBitmap(btmSetOpaque);
	AItem->Glyph->Assign(ABitmap);
  }
  __finally
  {
	ABitmap->Free();
  }
}
//---------------------------------------------------------------------------

void TRibbonDemoMainForm::InitSymbolGallery()
{
  const
	int AMathMap[] = {0xB1, 0x2260, 0x2264, 0x2265, 0xF7, 0xD7, 0x221E, 0x2211};
	int AGreekMap[] = {0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA};
	int ASymbolMap[] = {0xA9, 0xAE, 0x2122};
	int ACurrencyMap[] = {0x20AC, 0x24, 0xA3, 0xA5, 0x20A3};

  int I;

  for (I = 0; I < 8; I++)
	AddSymbolItem(rgiItemSymbol->GalleryGroups->Items[0], AMathMap[I]);
  for (I = 0; I < 10; I++)
	AddSymbolItem(rgiItemSymbol->GalleryGroups->Items[1], AGreekMap[I]);
  for (I = 0; I < 3; I++)
	AddSymbolItem(rgiItemSymbol->GalleryGroups->Items[2], ASymbolMap[I]);
  for (I = 0; I < 5; I++)
	AddSymbolItem(rgiItemSymbol->GalleryGroups->Items[3], ACurrencyMap[I]);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::SetModified(bool Value)
{
  EditorIsModified = Value;
  if (Value)
  {
    stModified->ImageIndex = 2;
    stModified->Caption = "Modified";
  }
  else
  {
    stModified->ImageIndex = -1;
    stModified->Caption = "";
  }
  dxBarButtonSave->Enabled = Value;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::ShowItems(bool AShow)
{
  BarManager->BeginUpdate();
  try
  {
    if (!AShow) 
    {
      btnLineNumber->Caption = "";
      btnColumnNumber->Caption = "";
      stModified->Caption = "";
	}
	BarManager->Groups[0]->Enabled = AShow;
  }
  __finally
  {
	BarManager->EndUpdate(false);
  }

}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::RibbonHelpButtonClick(TdxCustomRibbon *Sender)
{
  ShowMessage("Clicked!");
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::RibbonHideMinimizedByClick(
	  TdxCustomRibbon *Sender, HWND AWnd, TShiftState AShift,
	  const TPoint &APos, bool &AAllowProcessing)
{
  AAllowProcessing = (AWnd != Editor->Handle);
}
//---------------------------------------------------------------------------
TRibbonDemoStyle TRibbonDemoMainForm::GetRibbonDemoStyle()
{
    if (Ribbon->Style == rs2007)
	  return rdsOffice2007;
	else
	  if (Ribbon->EnableTabAero)
		return rdsOffice2010;
	  else
        return rdsScenic;
}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::dxBarButtonOptionsClick(TObject *Sender)
{
  String AColorSchemeName;
  TScreenTipOptions AScreenTipOptions;
  TRibbonDemoStyle AStyle = GetRibbonDemoStyle();
  TdxRibbonColorSchemeAccent AColorSchemeAccent = Ribbon->ColorSchemeAccent;
  AColorSchemeName = Ribbon->ColorSchemeName;
  AScreenTipOptions.ShowScreenTips = BarManager->ShowHint;
  AScreenTipOptions.ShowDescripitons = dxBarScreenTipRepository1->ShowDescription;

  if (ExecuteRibbonDemoOptions(&AColorSchemeName, &AScreenTipOptions, &AStyle, &AColorSchemeAccent))
  {
	RibbonDemoStyle = AStyle;
	SetColorScheme(AColorSchemeName);
	Ribbon->ColorSchemeAccent = AColorSchemeAccent;
	BarManager->ShowHint = AScreenTipOptions.ShowScreenTips;
	dxBarScreenTipRepository1->ShowDescription = AScreenTipOptions.ShowDescripitons;
	SetColorScheme(Ribbon->ColorSchemeName);
  }
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxBarButtonFontColorClick(TObject *Sender)
{
  SetFontColor();
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::rgiItemSymbolGroupItemClick(
	  TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem)
{
//
}
//---------------------------------------------------------------------------

__fastcall TRichEditUndoController::TRichEditUndoController(TdxRibbonGalleryItem *AGalleryItem,
  TRichEdit *AEditor)
{
  FGalleryItem = AGalleryItem;
  FEditor = AEditor;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::AnalyseMessage()
{
  int AMessageID;
  if (!FIsLocked)
  {
	AMessageID = SendMessage(FEditor->Handle, EM_GETUNDONAME, 0, 0);
	if ((AMessageID > 1) || (AMessageID == 1) && (AMessageID != FLastMessageID))
	  AddAction(AMessageID);
  }
}
//---------------------------------------------------------------------------

void TRichEditUndoController::Lock()
{
  FIsLocked = True;
  FLastMessageID = 0;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::UnLock()
{
  FIsLocked = False;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::AddAction(int AnActionID)
{
  String RichEditAction[7] = {"Unknown", "Typing", "Delete",
	  "Drag And Drop", "Cut", "Paste", "Color Change"};
  //if (AnActionID != 6 /*|| (FEditor->GetSelLength() != 0)*/)
	PushUndo(RichEditAction[AnActionID]);
  FLastMessageID = AnActionID;
}
//---------------------------------------------------------------------------

void TRichEditUndoController::PopUndo()
{
  TdxRibbonGalleryGroup *AGroup;
  AGroup = FGalleryItem->GalleryGroups->Items[0];
  if (AGroup->Items->Count > 0)
	AGroup->Items->Delete(0);
}
//---------------------------------------------------------------------------

void TRichEditUndoController::PushUndo(AnsiString AnAction)
{
  TdxRibbonGalleryGroup *AGroup;
  AGroup = RibbonDemoMainForm->rgiUndo->GalleryGroups->Items[0];
  AGroup->Items->Insert(0);
  AGroup->Items->Items[0]->Caption = AnAction;
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::rgiUndoHotTrackedItemChanged(
  TdxRibbonGalleryGroupItem *APrevHotTrackedGroupItem,
  TdxRibbonGalleryGroupItem *ANewHotTrackedGroupItem)
{
  int ACount;
  String AString;
  if (ANewHotTrackedGroupItem != NULL)
  {
	ACount = ANewHotTrackedGroupItem->Index + 1;
	bstSelectionInfo->Caption = "Undo " + IntToStr(ACount);
	if (ACount == 1)
	{
	  AString = " Action";
	}
	else
	{
	  AString = " Actions";
	}
	bstSelectionInfo->Caption = bstSelectionInfo->Caption + AString;
  }
  else
	bstSelectionInfo->Caption = "Cancel";
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::rgiUndoGroupItemClick(
	  TdxRibbonGalleryItem *Sender, TdxRibbonGalleryGroupItem *AItem)
{
  Undo(AItem->Index + 1);
}
//---------------------------------------------------------------------------
bool __fastcall TRibbonDemoMainForm::GetEditorIsModified()
{
	return ((TRichEdit *)Editor->InnerControl)->Modified;
}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::SetEditorIsModified(bool AValue)
{
	((TRichEdit *)Editor->InnerControl)->Modified = AValue;
}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::EditorContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled)
{
  if (Editor->SelLength != 0)
	MiniToolbar->Popup(dxBarPopupMenu);
  else
	dxBarPopupMenu->PopupFromCursorPos();

  Handled = True;
}
//---------------------------------------------------------------------------
void __fastcall TRibbonDemoMainForm::dxBarTouchModeClick(TObject *Sender)
{
	cxLookAndFeelController1->TouchMode = dxBarTouchMode->Down;	
}
//---------------------------------------------------------------------------

int GetGlyphIndex(String ASkinName)
{
	if (SameText(ASkinName, "Blue")) return 0;
	if (SameText(ASkinName, "Black")) return 1;
	if (SameText(ASkinName, "Silver")) return 2;
	if (SameText(ASkinName, "DarkGray")) return 3;
	if (SameText(ASkinName, "LightGray")) return 4;
	if (SameText(ASkinName, "White")) return 5;
	return -1;
}

void SetGlyph(TGraphic *ABitmap, TcxImageList *AImageList, int AGlyphIndex)
{
	TcxBitmap32 *ABitmap32 = new TcxBitmap32(AImageList->Width, AImageList->Height, True);
    try
	{
     	AImageList->Draw(ABitmap32->Canvas, 0, 0, AGlyphIndex);
		ABitmap->Assign(ABitmap32);
	}
	__finally
	{
		ABitmap32->Free();
	};
};


void __fastcall TRibbonDemoMainForm::dxSkinChooserGalleryItem1Populate(TObject *Sender)
{
	for (int I = 0; I < dxRibbonSkinsManager()->SkinCount; I++)
	{
	   TdxCustomRibbonSkin *ASkin = dxRibbonSkinsManager()->Skins[I];
	   if (ASkin->Style == Ribbon->Style)
	   {
			int AGlyphIndex = GetGlyphIndex(ASkin->Name);
			if (AGlyphIndex >= 0)
			{
				TdxSkinChooserGalleryGroupItem *ASkinItem = dxSkinChooserGalleryItem1->AddSkin(ASkin->Name, "Ribbon Color Schemes");
		  		ASkinItem->Caption = ASkin->Name;
				SetGlyph(ASkinItem->GlyphInDropDown, ilLargeColorSchemesGlyphs, AGlyphIndex);
				SetGlyph(ASkinItem->Glyph, ilSmallColorSchemesGlyphs, AGlyphIndex);
			}
	   }
	}
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::dxSkinChooserGalleryItem1SkinChanged(
	TObject *Sender, const String ASkinName)
{
	SetColorScheme(ASkinName);
}
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::btnBrowsePathClick(TObject *Sender)
{
  bool AHandled;

  if (bvtsSaveAs->Active)
    AHandled = SaveFile(True);
  else
    AHandled = OpenFile("");

  if (AHandled)
    BackstageView->Hide();
};
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::bvgcFolderItemClick(TObject *Sender, TdxGalleryControlItem *AItem)
{
  bool AHandled;
  String APrevInitialDir;

  if (bvtsSaveAs->Active)
  {
    APrevInitialDir = SaveDialog->InitialDir;
    AHandled = SaveFile(True, False);
    SaveDialog->InitialDir = APrevInitialDir;
  }
  else
  {
    APrevInitialDir = OpenDialog->InitialDir;
    AHandled = OpenFile("");
    OpenDialog->InitialDir = APrevInitialDir;
  }
  if (AHandled)
    BackstageView->Hide();
};
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::bvgcLocationsItemClick(TObject *Sender, TdxGalleryControlItem *AItem)
{
  gbRecentDocumentsPane->Visible = bvgcLocationsRecentDocumentsItem->Checked;
  gbRecentPathsPane->Visible = bvgcLocationsComputerItem->Checked;
};
//---------------------------------------------------------------------------

void __fastcall TRibbonDemoMainForm::bvgcRecentDocumentsItemClick(TObject *Sender, TdxGalleryControlItem *AItem)
{
  if (OpenFile(AItem->Hint))
    BackstageView->Hide();
};

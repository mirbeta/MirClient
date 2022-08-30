//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonRichEditMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TfrmRibbonRichEditMain *frmRibbonRichEditMain;
//---------------------------------------------------------------------------

const
  int AnAccentCount = 5;
  String ASaveDocumentQuery = "Do you want to save changes to Document?";

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

__fastcall TColorPickerController::TColorPickerController(TdxRibbonGalleryItem *AColorItem,
	TdxRibbonGalleryItem *AColorMapItem, TdxBarItemLinks *AItemLinks, TdxCustomRibbon *ARibbon, TColor ADefaultColor,
	UnicodeString ADefaultColorCaption)
{
  FRibbon = ARibbon;
  FColorItem = AColorItem;
  FColorMapItem = AColorMapItem;
  FColorGlyphSize = cxTextHeight(FRibbon->Fonts->Group, "Wg", 0);
  FColorDialog = new TdxColorDialog(NULL);
  FColorDialog->Options->ColorPicker->AllowEditAlpha = False;
  FColorDialog->Options->ColorPicker->DefaultVisible = True;
  FDefaultColor = ADefaultColor;

  InitColorMapItem();
  InitColorItem();
  InitDropDownGallery(AItemLinks, ADefaultColorCaption);
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

void TColorPickerController::InitDropDownGallery(TdxBarItemLinks *AItemLinks, UnicodeString ADefaultColorCaption)
{
  TcxAlphaBitmap *ANoColorGlyph;

  FNoColorButton = (TdxBarButton*)AItemLinks->AddButton()->Item;
  FNoColorButton->ButtonStyle = bsChecked;
  FNoColorButton->Caption = ADefaultColorCaption;
  FNoColorButton->OnClick = NoColorButtonClick;
  ANoColorGlyph = CreateColorBitmap(FDefaultColor, 16);
  FNoColorButton->Glyph->Assign(ANoColorGlyph);
  delete ANoColorGlyph;
  AItemLinks->Add(FColorItem);
  FMoreColorsButton = (TdxBarButton*)AItemLinks->AddButton()->Item;
  FMoreColorsButton->Caption = "&More Colors...";
  FMoreColorsButton->OnClick = MoreColorsClick;
  FColorDialogSetup = (TdxBarButton*)AItemLinks->AddButton()->Item;
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
	FColor = Value;
	ColorChanged();
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
  SetColor(FDefaultColor);
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
  FColorDialog->Color = dxColorToAlphaColor(Color);
  if (FColorDialog->Execute())
  {
    FCustomColorsGroup->Header->Visible = true;
    AddColorItem(FCustomColorsGroup, dxAlphaColorToColor(FColorDialog->Color))->Selected = true;
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
  Result->Canvas->Brush->Color = AColor;
  Result->Canvas->Pen->Color = clGray;
  Result->Canvas->FillRect(Result->ClientRect);
  Result->Canvas->FrameRect(Result->ClientRect);
#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  if (AColor == Vcl::Graphics::clNone)
#else
  if (AColor == Graphics::clNone)
#endif
	Result->RecoverAlphaChannel(clBlack);
  else
	Result->TransformBitmap(btmSetOpaque);

  return Result;
}
//---------------------------------------------------------------------------

void TColorPickerController::CreateColorRow(TdxRibbonGalleryGroup *AGalleryGroup, const TColor *AColorMap)
{
  for (int I = 0; I < SchemeColorCount; I++)
	AddColorItem(AGalleryGroup, AColorMap[I]);
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

void TColorPickerController::BuildThemeColorGallery()
{
  TColor *AColorMap;
  TColor *AnAccentColorScheme[AnAccentCount];

  for (int I = 0; I < AnAccentCount; I++)
    AnAccentColorScheme[I] = new TColor[SchemeColorCount];

  BarManager->BeginUpdate();
  try
  {
    FThemeColorsGroup->Items->Clear();
    AColorMap = AColorMaps[FColorMapItem->SelectedGroupItem->Index].Map;
    CreateColorRow(FThemeColorsGroup, AColorMap);

    FAccentColorsGroup->Items->Clear();
    GetAccentColorScheme(AColorMap, AnAccentColorScheme);
	for (int I = 0; I < AnAccentCount; I++)
	  CreateColorRow(FAccentColorsGroup, AnAccentColorScheme[I]);
  }
  __finally
  {
    BarManager->EndUpdate(true);
  };

  for (int I = 0; I < AnAccentCount; I++)
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
    FColorItem->Glyph->Assign(AGlyph);
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

__fastcall TfrmRibbonRichEditMain::TfrmRibbonRichEditMain(TComponent* Owner)
	: TfrmRichEditControlBase(Owner)
{
}

void TfrmRibbonRichEditMain::CheckDocumentClosing(Boolean &CanClose)
{
  if (RichEditControl->DocumentModelModified)
	switch (MessageDlg(ASaveDocumentQuery, mtWarning, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
	{
	  case mrYes:
	  {
		acSave->Execute();
		CanClose = !RichEditControl->DocumentModelModified;
		break;
	  }
	  case mrCancel:
	  {
		CanClose = False;
		break;
	  }
	}
}

#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  void TfrmRibbonRichEditMain::DrawHelpedColorLine(Vcl::Graphics::TGraphic* AGlyph, TColor AColor, TcxImageList* AImageBasicGlyph, Integer AIndexBasicGlyph)
#else
  void TfrmRibbonRichEditMain::DrawHelpedColorLine(Graphics::TGraphic* AGlyph, TColor AColor, TcxImageList* AImageBasicGlyph, Integer AIndexBasicGlyph)
#endif
{
  const int ALineWidth = 14;
  const int ALineHeight = 3;
  TcxAlphaBitmap* ALineGlyph;
  TcxAlphaBitmap* ABasicGlyph;
  ALineGlyph = new TcxAlphaBitmap();
  ABasicGlyph = new TcxAlphaBitmap();
  try
  {
	AImageBasicGlyph->GetImageInfo(AIndexBasicGlyph, ABasicGlyph, NULL);

	ALineGlyph->Height = ALineHeight;
	ALineGlyph->Width = ALineWidth;
	ALineGlyph->Canvas->Brush->Color = AColor;
	ALineGlyph->Canvas->FillRect(Rect(0, 0, ALineWidth, ALineHeight));
#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
	ALineGlyph->RecoverAlphaChannel(Vcl::Graphics::clNone);
#else
	ALineGlyph->RecoverAlphaChannel(Graphics::clNone);
#endif
	BitBlt(ABasicGlyph->Canvas->Handle, 1, 13, ALineWidth, ALineHeight, ALineGlyph->Canvas->Handle, 0, 0, SRCCOPY);
	AImageBasicGlyph->Draw(ABasicGlyph->Canvas, ABasicGlyph->ClientRect, AIndexBasicGlyph);

	AGlyph->Assign(ABasicGlyph);
  }
  __finally
  {
	delete ABasicGlyph;
	delete ALineGlyph;
  }
}

void __fastcall TfrmRibbonRichEditMain::bbTextHighlightClick(TObject *Sender)
{
  UpdateTextHighlight();
}

void __fastcall TfrmRibbonRichEditMain::PageColorChangedHandler(TObject *Sender)
{
  acPageColor->UpdateTarget(RichEditControl);
  acPageColor->Value = dxColorToAlphaColor(FPageColorPicker->Color);
}

void TfrmRibbonRichEditMain::UpdateFloatingPictureContext()
{
  TdxRibbonContext* APictureContext = Ribbon->Contexts->Find("Picture Tools");
  APictureContext->Visible = RichEditControl->IsFloatingObjectSelected();
}

void TfrmRibbonRichEditMain::UpdateFloatingObjectFillColor()
{
  acChangeFloatingObjectFillColor->UpdateTarget(RichEditControl);
  UpdateFloatingObjectFillColorImage();
  acChangeFloatingObjectFillColor->Value = dxColorToAlphaColor(FFloatingObjectFillColorPicker->Color);
}

void TfrmRibbonRichEditMain::UpdateFloatingObjectFillColorImage()
{
  DrawHelpedColorLine(bbFloatingObjectFillColor->Glyph, FFloatingObjectFillColorPicker->Color,
	ilSmallImages, acChangeFloatingObjectFillColor->ImageIndex);
}

void TfrmRibbonRichEditMain::UpdateFloatingObjectOutlineColor()
{
  acChangeFloatingObjectOutlineColor->UpdateTarget(RichEditControl);
  UpdateFloatingObjectOutlineColorImage();
  acChangeFloatingObjectOutlineColor->Value = dxColorToAlphaColor(FFloatingObjectOutlineColorPicker->Color);
}

void TfrmRibbonRichEditMain::UpdateFloatingObjectOutlineColorImage()
{
  DrawHelpedColorLine(bbFloatingObjectOutlineColor->Glyph, FFloatingObjectOutlineColorPicker->Color,
	ilSmallImages, acChangeFloatingObjectOutlineColor->ImageIndex);
}

void TfrmRibbonRichEditMain::UpdateHeaderAndFooterContext()
{
  TdxRibbonContext* AHeaderAndFooterContext = Ribbon->Contexts->Find("Header & Footer Tools");
  AHeaderAndFooterContext->Visible = RichEditControl->IsSelectionInHeaderOrFooter();
}

void TfrmRibbonRichEditMain::UpdateRibbonContexstsStates()
{
  UpdateFloatingPictureContext();
  UpdateHeaderAndFooterContext();
  UpdateTableContext();
}

void TfrmRibbonRichEditMain::UpdateTableContext()
{
  TdxRibbonContext* ATableToolsContext = Ribbon->Contexts->Find("Table Tools");
  ATableToolsContext->Visible = RichEditControl->IsSelectionInTable();
}

void TfrmRibbonRichEditMain::UpdateFontColor()
{
  acFontColor->UpdateTarget(RichEditControl);
  UpdateFontColorImage();
  acFontColor->Value = dxColorToAlphaColor(FFontColorPicker->Color);
}

void TfrmRibbonRichEditMain::UpdateFontColorImage()
{
  DrawHelpedColorLine(bbFontColor->Glyph, FFontColorPicker->Color, ilSmallImages, acFontColor->ImageIndex);
}

void TfrmRibbonRichEditMain::UpdateFontNameComboBoxPropertiesFontStyle(TFontStyles AStyle, Boolean AChecked)
{
  TcxFontPreview* AFontPreview;
  TcxCustomFontNameComboBoxProperties* Properties;
  Properties = (TcxCustomFontNameComboBoxProperties *)beFontName->Properties;
  AFontPreview = Properties->FontPreview;
  if (AChecked)
    AFontPreview->FontStyle =  AFontPreview->FontStyle + AStyle;
  else
    AFontPreview->FontStyle = AFontPreview->FontStyle - AStyle;
}

void TfrmRibbonRichEditMain::UpdateTextHighlight()
{
  acTextHighlight->UpdateTarget(RichEditControl);
  UpdateTextHighlightImage();
  acTextHighlight->Value = dxColorToAlphaColor(FTextHighlightColorPicker->Color);
}

void TfrmRibbonRichEditMain::UpdateTextHighlightImage()
{
  DrawHelpedColorLine(bbTextHighlight->Glyph, FTextHighlightColorPicker->Color, ilSmallImages, acTextHighlight->ImageIndex);
}

int CorrectZoomPosition(double const AValue)
{
  int Result;
  if (AValue > 0)
	Result = AValue + 0.5;
  else
	Result = AValue - 0.5;
  return Result;
}

void TfrmRibbonRichEditMain::UpdateZoom()
{
  if (RichEditControl->ActiveView->ZoomFactor > 5)
    RichEditControl->ActiveView->ZoomFactor = 5;
  tbZoom->Position = CorrectZoomPosition(RichEditControl->ActiveView->ZoomFactor * 100);
}

void __fastcall TfrmRibbonRichEditMain::acExitExecute(TObject *Sender)
{
  Close();
}

void __fastcall TfrmRibbonRichEditMain::acPrintExecute(TObject *Sender)
{
  RichEditPrinterLink->Print(True, NULL);
}

void __fastcall TfrmRibbonRichEditMain::acPrintPreviewExecute(TObject *Sender)
{
  RichEditPrinterLink->Preview();
}

void __fastcall TfrmRibbonRichEditMain::recRichEditControlActiveViewChanged(TObject *Sender)
{
  UpdateZoom();
}

void __fastcall TfrmRibbonRichEditMain::recRichEditControlDocumentClosing(TObject *Sender, Boolean &CanClose)
{
  CheckDocumentClosing(CanClose);
}

void __fastcall TfrmRibbonRichEditMain::recRichEditControlHyperlinkClick(TObject *Sender, const TdxHyperlinkClickEventArgs *Args)
{
//do nothing
}

void __fastcall TfrmRibbonRichEditMain::recRichEditControlSelectionChanged(TObject *Sender)
{
  UpdateRibbonContexstsStates();
}

void __fastcall TfrmRibbonRichEditMain::recRichEditControlZoomChanged(TObject *Sender)
{
  UpdateZoom();
}

void __fastcall TfrmRibbonRichEditMain::bbApplicationButtonClick(TObject *Sender)
{
  Ribbon->ApplicationButton->Visible = bbApplicationButton->Down;
}

void __fastcall TfrmRibbonRichEditMain::bbFloatingObjectFillColorClick(TObject *Sender)
{
  UpdateFloatingObjectFillColor();
}

void __fastcall TfrmRibbonRichEditMain::bbFloatingObjectOutlineColorClick(TObject *Sender)
{
  UpdateFloatingObjectOutlineColor();
}

void __fastcall TfrmRibbonRichEditMain::bbFontColorClick(TObject *Sender)
{
  UpdateFontColor();
}

void __fastcall TfrmRibbonRichEditMain::beFontNamePropertiesFontPreviewButtonClick(TObject *Sender, TcxFontButtonType ButtonType)
{
  switch (ButtonType)
  {
	case cxfbtBold:
	  acBold->Execute();
	case cxfbtItalic:
	  acItalic->Execute();
	case cxfbtUnderline:
	  acUnderline->Execute();
	case cxfbtStrikeOut:
	  acStrikeout->Execute();
  }
}

void __fastcall TfrmRibbonRichEditMain::bmbTableToolsCellSizeClick(TObject *Sender)
{
  acShowTablePropertiesForm->Execute();
}

void __fastcall TfrmRibbonRichEditMain::RowsAndColumnsCaptionButtonsClick(TObject *Sender)
{
  acInsertTableCellsForm->Execute();
}

void __fastcall TfrmRibbonRichEditMain::bmbHomeFontClick(TObject *Sender)
{
  acFont->Execute();
}

void __fastcall TfrmRibbonRichEditMain::bmbHomeParagraphClick(TObject *Sender)
{
  acParagraph->Execute();
}

void __fastcall TfrmRibbonRichEditMain::tbZoomPropertiesChange(TObject *Sender)
{
  int Pos = tbZoom->Position;
  TVarRec args[1] = {Pos};
  bsZoom->Caption = Format(AnsiString("%3d %%"), args, 1);
  RichEditControl->ActiveView->ZoomFactor = (float) Pos / 100;
}

void __fastcall TfrmRibbonRichEditMain::FormCloseQuery(TObject* Sender, bool &CanClose)
{
  CheckDocumentClosing(CanClose);
}

void __fastcall TfrmRibbonRichEditMain::FormClose(TObject* Sender, TCloseAction &Action)
{
  Action = caFree;
}

void __fastcall TfrmRibbonRichEditMain::FormCreate(TObject *Sender)
{
  TfrmRichEditControlBase::FormCreate(Sender);

  tbZoom->Tag = tbZoom->Height;

#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  FFontColorPicker = new TColorPickerController(rgiFontColor, rgiColorTheme, ppmFontColor->ItemLinks,
	Ribbon, Vcl::Graphics::clNone, "Automatic");
#else
  FFontColorPicker = new TColorPickerController(rgiFontColor, rgiColorTheme, ppmFontColor->ItemLinks,
	Ribbon, Graphics::clNone, "Automatic");
#endif
  FFontColorPicker->OnColorChanged = bbFontColorClick;
  UpdateFontColorImage();

  FPageColorPicker = new TColorPickerController(rgiPageColor, rgiPageColorTheme, bsiPageColor->ItemLinks,
	Ribbon, clWindow, "No Color");
  FPageColorPicker->OnColorChanged = PageColorChangedHandler;

#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  FTextHighlightColorPicker = new TColorPickerController(rgiTextHighlightColor,
	rgiTextHighlightColorTheme, ppmTextHighlightColor->ItemLinks, Ribbon, Vcl::Graphics::clNone, "No Color");
#else
  FTextHighlightColorPicker = new TColorPickerController(rgiTextHighlightColor,
	rgiTextHighlightColorTheme, ppmTextHighlightColor->ItemLinks, Ribbon, Graphics::clNone, "No Color");
#endif
  FTextHighlightColorPicker->OnColorChanged = bbTextHighlightClick;
  UpdateTextHighlightImage();

#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  FFloatingObjectFillColorPicker = new TColorPickerController(rgiFloatingObjectFillColor,
	rgiFloatingObjectFillColorTheme, ppmFloatingObjectFillColor->ItemLinks, Ribbon, Vcl::Graphics::clNone, "No Fill");
#else
  FFloatingObjectFillColorPicker = new TColorPickerController(rgiFloatingObjectFillColor,
	rgiFloatingObjectFillColorTheme, ppmFloatingObjectFillColor->ItemLinks, Ribbon, Graphics::clNone, "No Fill");
#endif
  FFloatingObjectFillColorPicker->OnColorChanged = bbFloatingObjectFillColorClick;
  UpdateFloatingObjectFillColorImage();

#if (__CODEGEARC__ >= 0x0640) // C++Builder XE2
  FFloatingObjectOutlineColorPicker = new TColorPickerController(rgiFloatingObjectOutlineColor,
	rgiFloatingObjectOutlineColorTheme, ppmFloatingObjectOutlineColor->ItemLinks, Ribbon, Vcl::Graphics::clNone,
	"No Outline");
#else
  FFloatingObjectOutlineColorPicker = new TColorPickerController(rgiFloatingObjectOutlineColor,
	rgiFloatingObjectOutlineColorTheme, ppmFloatingObjectOutlineColor->ItemLinks, Ribbon, Graphics::clNone,
	"No Outline");
#endif
  FFloatingObjectOutlineColorPicker->OnColorChanged = bbFloatingObjectOutlineColorClick;
  UpdateFloatingObjectOutlineColorImage();
}

void __fastcall TfrmRibbonRichEditMain::FormDestroy(TObject *Sender)
{
  delete FTextHighlightColorPicker;
  delete FPageColorPicker;
  delete FFontColorPicker;
}

void __fastcall TfrmRibbonRichEditMain::bmbPageLayoutPageSetupCaptionButtons0Click(TObject *Sender)
{
  acShowPageSetupForm->Execute();
}

void __fastcall TfrmRibbonRichEditMain::bmbPictureToolsArrangeCaptionButtons0Click(TObject *Sender)
{
  acFloatingObjectLayoutOptionsForm->Execute();
}

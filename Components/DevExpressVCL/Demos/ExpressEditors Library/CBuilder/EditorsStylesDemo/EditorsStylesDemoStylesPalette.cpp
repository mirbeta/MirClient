//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoStylesPalette.h"
#include "cxEdit.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxCheckComboBox"
#pragma link "cxCheckListBox"
#pragma link "cxColorComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxGroupBox"
#pragma link "cxHeader"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxProgressBar"
#pragma link "cxRadioGroup"
#pragma link "cxSpinButton"
#pragma link "cxSpinEdit"
#pragma link "cxTrackBar"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxClasses"
#pragma link "cxEditRepositoryItems"
#pragma link "cxExtEditRepositoryItems"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxCheckGroupBox"
#pragma resource "*.dfm"
TEditorsStylesDemoStylesPaletteFrame *EditorsStylesDemoStylesPaletteFrame;

const int ValueColumnTypeCount = vctTextStyle - vctBorderColor + 1;
const int StyleCategoryCount = 4;
const String StyleValueNames[ValueColumnTypeCount] =
  {"BorderColor", "BorderStyle", "Color", "TextColor", "TextStyle"};
//TcxContainerStateItem = (csNormal, csActive, csDisabled, csHotTrack);
const int StyleCategoryOrders[csHotTrack - csNormal + 1] = {0, 2, 1, 3};


int FontStylesToInteger(TFontStyles AFontStyles)
{
  int Result = 0;
  for (int AFontStyle = fsBold; AFontStyle <= fsStrikeOut; AFontStyle++)
    if (AFontStyles.Contains((TFontStyle)AFontStyle))
      Result = Result | (1 << AFontStyle);
  return Result;
}

TFontStyles IntegerToFontStyles(int Value)
{
  TFontStyles Result = TFontStyles();
  for (int AFontStyle = fsBold; AFontStyle <= fsStrikeOut; AFontStyle++)
    if (Value & (1 << AFontStyle))
      Result << (TFontStyle)AFontStyle;
  return Result;    
}

//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoStylesPaletteFrame::TEditorsStylesDemoStylesPaletteFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  FBitmap = new Graphics::TBitmap();
  FBitmap->TransparentColor = clFuchsia;
  FBitmap->Transparent = true;

  cxColorComboBox->Properties->PrepareDelphiColorList(false, false);
  HintStyle = hcstNoHint;
  FDisplayStyle = shtLightBlue;
  FTempDisplayStyle = shtLightBlue;
}
//---------------------------------------------------------------------------

__fastcall TEditorsStylesDemoStylesPaletteFrame::~TEditorsStylesDemoStylesPaletteFrame()
{
  delete FBitmap;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoStylesPaletteFrame::ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle)
{
  TEditorsStylesDemoBaseFrame::ChangeDisplayStyle(ADisplayStyle);
  FDisplayStyle = FTempDisplayStyle;
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoStylesPaletteFrame::Name()
{
  return "Style Palette";
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoStylesPaletteFrame::BriefName()
{
  return "Styles";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoStylesPaletteFrame::Description()
{
  return "Style Palette Notes";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoStylesPaletteFrame::StylesIniPath()
{
  return "StylesFrmStylePalette\\";
}
//---------------------------------------------------------------------------


void __fastcall TEditorsStylesDemoStylesPaletteFrame::rgStylesPropertiesChange(
      TObject *Sender)
{
  SendMessage(cgbEditors->Handle, WM_SETREDRAW, 0, 0);
  try{
    TcxStyleSheetType AStyleSheetType = (TcxStyleSheetType)((TcxRadioGroup*)Sender)->ItemIndex;
    ChangeDisplayStyle(AStyleSheetType);
    SetProgressBarBitmap(AStyleSheetType, cxProgressBar);
    AdjustCheckListGlyphs(AStyleSheetType, cxCheckListBox);
    AdjustTrackBarThumb(AStyleSheetType, cxTrackBar, FBitmap);
    InitStylesView(cxEditStyleController);
  }
  __finally{
    SendMessage(cgbEditors->Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(cgbEditors->Handle, NULL, NULL, RDW_INVALIDATE | RDW_ALLCHILDREN);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::SetProgressBarBitmap(TcxStyleSheetType AStyleSheetType, TcxProgressBar* AProgressBar)
{
  Graphics::TBitmap* ABitmap = new Graphics::TBitmap();
  try {
    switch(AStyleSheetType) {
      case shtRainyDay: {
        ilForegroundBitmaps->GetBitmap(1, ABitmap); break; }
      case shtBrick: {
        ilForegroundBitmaps->GetBitmap(0, ABitmap); break; }
    }
    AProgressBar->Properties->ForegroundImage = ABitmap;
  }
  __finally {
    delete ABitmap;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::AdjustTrackBarThumb(TcxStyleSheetType AStyleSheetType, TcxTrackBar* ATrackBar, Graphics::TBitmap* ABitmap)
{
  switch (AStyleSheetType) {
    case shtWood:
      {
        ABitmap->LoadFromFile(StylesIniPath() + "Wood.bmp");
        ATrackBar->Properties->ThumbType = cxttCustom;
        break;
      }
    case shtDeepSea:
      {
        ABitmap->LoadFromFile(StylesIniPath() + "DeepSea.bmp");
        ATrackBar->Properties->ThumbType = cxttCustom;
        break;
      }
    default :  ATrackBar->Properties->ThumbType = cxttRegular;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::AdjustCheckListGlyphs(TcxStyleSheetType AStyleSheetType, TcxCheckListBox* ACheckListBox)
{
  Graphics::TBitmap* ABitmap = new Graphics::TBitmap();
  try {
    switch(AStyleSheetType) {
      case shtDeepSea: {
	ilCheckGlyphs->GetBitmap(0, ABitmap); break; }
      case shtWood: {
	ilCheckGlyphs->GetBitmap(1, ABitmap); break; }
    }
    ACheckListBox->Glyph->Assign(ABitmap);
  }
  __finally {
    delete ABitmap;
  }
}
//---------------------------------------------------------------------------

void SetLookAndFeel(TcxEditStyleController* AEditStyleController, int AItemIndex)
{
  if (AItemIndex < 4) {
    TcxLookAndFeelKind Kind = (TcxLookAndFeelKind)AItemIndex;
    AEditStyleController->Style->LookAndFeel->Kind = Kind;
    AEditStyleController->Style->LookAndFeel->NativeStyle = false;
  } else
    AEditStyleController->Style->LookAndFeel->NativeStyle = true;
}

void __fastcall TEditorsStylesDemoStylesPaletteFrame::rgLookAndFeelPropertiesChange(
      TObject *Sender)
{
  int AItemIndex = ((TcxRadioGroup*)Sender)->ItemIndex;
  SetLookAndFeel(cxLabelStyleController, AItemIndex);
  SetLookAndFeel(cxEditStyleController, AItemIndex);

  for (int AStyleState = csNormal; AStyleState <=  csHotTrack; AStyleState++){
    cxEditStyleController->Styles[(TcxContainerStateItem)AStyleState]->AssignedValues =
      cxEditStyleController->Styles[(TcxContainerStateItem)AStyleState]->
        AssignedValues>>svBorderStyle>>svButtonStyle;
  }
  InitStylesView(cxEditStyleController);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::cxTrackBarPropertiesDrawThumb(
      TObject *Sender, TcxCanvas *ACanvas, const TRect &ARect)
{
  ACanvas->Draw(ARect.Left, ARect.Top, FBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::cxTrackBarPropertiesGetThumbRect(
      TObject *Sender, TRect &ARect)
{
  ARect = FBitmap->Canvas->ClipRect;
}
//---------------------------------------------------------------------------

int __fastcall TEditorsStylesDemoStylesPaletteFrame::GetStyleStartRecordIndex(
  TcxContainerStateItem AStyleState)
{
  return StyleCategoryOrders[(int)AStyleState] * ValueColumnTypeCount;
}
//---------------------------------------------------------------------------

TcxContainerStateItem __fastcall TEditorsStylesDemoStylesPaletteFrame::GetStyleStateByRecordIndex(
  int ARecordIndex)
{
  int Result;
  for (Result = csNormal; Result <= csHotTrack; Result++)
    if (ARecordIndex/ValueColumnTypeCount == StyleCategoryOrders[Result])
      break;
  return TcxContainerStateItem(Result);
}
//---------------------------------------------------------------------------

Variant __fastcall  TEditorsStylesDemoStylesPaletteFrame::GetStyleValue(
#if (__BORLANDC__ == 0x0610)
  TcxCustomContainerStyle AStyle
#else
  TcxCustomContainerStyle *AStyle
#endif
  , TValueColumnType AValueColumnType)
{
  switch(AValueColumnType) {
    case vctBorderColor:
	  return AStyle->BorderColor;
    case vctBorderStyle:
      return (int)AStyle->BorderStyle;
    case vctColor:
	  return AStyle->Color;
    case vctTextColor:
      return AStyle->TextColor;
    default:
      return FontStylesToInteger(AStyle->TextStyle);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::SetStyleValue(
#if (__BORLANDC__ == 0x0610)
  TcxCustomContainerStyle AStyle
#else
  TcxCustomContainerStyle *AStyle
#endif
  , TValueColumnType AValueColumnType, Variant Value)
{
  switch(AValueColumnType) {
    case vctBorderColor:
      AStyle->BorderColor =  (TColor)int(Value);
      break;
    case vctBorderStyle:
      AStyle->BorderStyle = (TcxContainerBorderStyle)int(Value);
      break;
    case vctColor:
      AStyle->Color = (TColor)int(Value);
      break;
    case vctTextColor:
      AStyle->TextColor = (TColor)int(Value);
      break;
    case vctTextStyle:
      AStyle->TextStyle = IntegerToFontStyles(Value);
      break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::InitStyleRecords(
#if (__BORLANDC__ == 0x0610)
  TcxCustomContainerStyle AStyle
#else
  TcxCustomContainerStyle *AStyle
#endif
  )
{
  int AStartRecordIndex = GetStyleStartRecordIndex(AStyle->State);
  for (int I = 0; I < ValueColumnTypeCount; I++){
    tvStyles->DataController->Values[AStartRecordIndex + I][clnStyleValue->Index] =
      GetStyleValue(AStyle, (TValueColumnType)I);
  }
}

void __fastcall TEditorsStylesDemoStylesPaletteFrame::InitStylesView(
  TcxEditStyleController *AStyleController)
{
  tvStyles->BeginUpdate();
  try{
    for (int I = csNormal; I <= csHotTrack; I++)
      InitStyleRecords(AStyleController->Styles[(TcxContainerStateItem)I]);
  }
  __finally{
    tvStyles->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::InitRecords()
{
  tvStyles->BeginUpdate();
  try{
    tvStyles->DataController->RecordCount = StyleCategoryCount * ValueColumnTypeCount;
    for (int I = 0; I < tvStyles->DataController->RecordCount; I++){
      tvStyles->DataController->Values[I][clnStyleCategory->Index] =
        I / ValueColumnTypeCount;
      tvStyles->DataController->Values[I][clnStyleValueName->Index] =
        StyleValueNames[(TValueColumnType)(I % ValueColumnTypeCount)];
    }
  }
  __finally{
    tvStyles->EndUpdate();
  }
  clnStyleValueName->ApplyBestFit(False, False);
  clnStyleValue->Width = 150;
}

void __fastcall TEditorsStylesDemoStylesPaletteFrame::FormCreate(
      TObject *Sender)
{
  InitRecords();
  InitStylesView(cxEditStyleController);
  cxDateEdit->Date = Now();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::clnStyleValueGetProperties(
      TcxCustomGridTableItem *Sender, TcxCustomGridRecord *ARecord,
      TcxCustomEditProperties *&AProperties)
{
  TValueColumnType AValueColumnType =
    (TValueColumnType)(ARecord->RecordIndex % ValueColumnTypeCount);
  switch (AValueColumnType) {
    case vctBorderColor:
    case vctColor:
    case vctTextColor:
      AProperties = eriColor->Properties;
      break;
    case vctBorderStyle:
      AProperties = eriBorderStyle->Properties;
      break;
    case vctTextStyle:
      AProperties = eriTextStyle->Properties;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoStylesPaletteFrame::StyleValueColumnPropertiesEditValueChanged(
  TObject *Sender)
{
  int AFocusedRecordIndex = tvStyles->DataController->FocusedRecordIndex;
  SetStyleValue(
    cxEditStyleController->Styles[GetStyleStateByRecordIndex(AFocusedRecordIndex)],
    (TValueColumnType)(AFocusedRecordIndex % ValueColumnTypeCount),
    ((TcxCustomEdit*)Sender)->EditValue);
  InitStylesView(cxEditStyleController);
}
//---------------------------------------------------------------------------


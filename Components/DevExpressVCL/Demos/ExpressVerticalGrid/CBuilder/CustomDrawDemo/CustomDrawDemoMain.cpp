//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoMain.h"
#include "CustomDrawDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxDBVGrid"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxStyles"
#pragma link "cxVGrid"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImageComboBox"
#pragma resource "*.dfm"
#include "CustomDrawDemoEditor.h"
TCustomDrawDemoMainForm *CustomDrawDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TcxCustomDrawInfo::TcxCustomDrawInfo(TControl* AControl)
{
  FControl = AControl;
  FBitmaps = new TList();
  LoadResourceBitmaps();
  FDefaultFont = new TFont();
  FCustomDrawData = new TList();
}
//---------------------------------------------------------------------------

void TcxCustomDrawInfo::LoadResourceBitmaps()
{
  Graphics::TBitmap *ABitmap;
  for (int I = 0; I < BkImageCount; I++){
    ABitmap = new Graphics::TBitmap();
    LoadImageFromRes(ABitmap, BkImageResNames[I]);
    FBitmaps->Add(ABitmap);
  }
}
//---------------------------------------------------------------------------

__fastcall TcxCustomDrawInfo::~TcxCustomDrawInfo(void)
{
  for (int I = 0; I < FCustomDrawData->Count; I++)
    delete (TcxItemCustomDrawInfo*)FCustomDrawData->Items[I];
  for (int I = 0; I < FBitmaps->Count; I++)
    delete (Graphics::TBitmap*)FBitmaps->Items[I];
  delete FCustomDrawData;
  delete FBitmaps;
  delete FDefaultFont;
}
//---------------------------------------------------------------------------

void TcxCustomDrawInfo::AddNewItem(TCustomDrawArea ADrawArea,
  TcxItemCustomDrawType AItemType)
{
  FCustomDrawData->Add(new TcxItemCustomDrawInfo(this, ADrawArea, AItemType));
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TcxCustomDrawInfo::GetItemByIndex(int AIndex)
{
  return ((TcxItemCustomDrawInfo*)FCustomDrawData->Items[AIndex]);
}
//---------------------------------------------------------------------------

Graphics::TBitmap* TcxCustomDrawInfo::GetBkBitmap(TBkImage ABkImage)
{
  return (Graphics::TBitmap*)FBitmaps->Items[int(ABkImage)];
}
//---------------------------------------------------------------------------

int TcxCustomDrawInfo::GetCount(void)
{
  return FCustomDrawData->Count;
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TcxCustomDrawInfo::GetItem(TCustomDrawArea ADrawArea)
{
  TcxItemCustomDrawInfo *Result = NULL;
  for (int I = 0; I < FCustomDrawData->Count; I++)
    if (((TcxItemCustomDrawInfo*)FCustomDrawData->Items[I])->DrawArea == ADrawArea){
      Result = ((TcxItemCustomDrawInfo*)FCustomDrawData->Items[I]);
      break;
    }
  return Result;
}
//---------------------------------------------------------------------------

__fastcall TcxItemCustomDrawInfo::TcxItemCustomDrawInfo(TcxCustomDrawInfo *AOwner,
  TCustomDrawArea ADrawArea, TcxItemCustomDrawType AItemType)
{
  FOwner = AOwner;
  FDrawArea = ADrawArea;
  FItemType = AItemType;
  if (FOwner != NULL)
    FBitmap = AOwner->Bitmaps[TBkImage(0)];
  else
    FBitmap = NULL;
  if (FOwner != NULL)
    FFont = AOwner->DefaultFont;
  else
    FBitmap = NULL;
  FBkImageType = (TBkImage)0;
  FDrawingStyle = (TCustomDrawingStyle)0;
  FColorScheme = (TcxColorScheme)0;
  FIsBitmapAssigned = false;
  FIsFontAssigned = false;
}
//---------------------------------------------------------------------------

__fastcall TcxItemCustomDrawInfo::~TcxItemCustomDrawInfo()
{
  if (FIsBitmapAssigned)
    delete FBitmap;
  if (FIsFontAssigned)
    delete FFont;
}
//---------------------------------------------------------------------------

Graphics::TBitmap* TcxItemCustomDrawInfo::GetBitmap()
{
  if ((FBkImageType != bkiUserDefined || !FIsBitmapAssigned) &&
    (FOwner != NULL))
    return FOwner->Bitmaps[FBkImageType];
  else
    return FBitmap;
}
//---------------------------------------------------------------------------

TFont* TcxItemCustomDrawInfo::GetFont()
{
  return FFont;
}
//---------------------------------------------------------------------------

void TcxItemCustomDrawInfo::SetBitmap(Graphics::TBitmap *Value)
{
  if (FIsBitmapAssigned)
    delete FBitmap;
  FBitmap = Value;
  FIsBitmapAssigned = true;
  FBkImageType = bkiUserDefined;
}
//---------------------------------------------------------------------------

void TcxItemCustomDrawInfo::SetFont(TFont *Value)
{
  ((TControlAccess*)Owner->Control)->Font = Value;
  if (FIsFontAssigned)
    delete FFont;
  FFont = Value;
  FIsFontAssigned = true;
}
//---------------------------------------------------------------------------

__fastcall TCustomDrawDemoMainForm::TCustomDrawDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  FCustomDrawInfo = new TcxCustomDrawInfo(cxDBVerticalGrid);
  AddCustomDrawInfos();
  AdjustCustomDrawItems();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::FormShow(TObject *Sender)
{
  cxDBVerticalGrid->FullExpand();
  CustomDrawDemoEditorForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::actCustomizationFormExecute(
      TObject *Sender)
{
  CustomDrawDemoEditorForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBVerticalGridDrawBackground(
	  TObject *Sender, TcxCanvas *ACanvas, const TRect &R,
      const TcxViewParams &AViewParams, bool &Done)
{
  Done = DrawBackgroundItem(FCustomDrawInfo->Items[cdaBackground], ACanvas, R, AViewParams, Sender);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBVerticalGridDrawRowHeader(
      TObject *Sender, TcxCanvas *ACanvas, TcxvgPainter *APainter,
      TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done)
{
  if (dynamic_cast<TcxEditorRowHeaderInfo*>(AHeaderViewInfo))
    Done = DrawHeaderItem(FCustomDrawInfo->Items[cdaHeader], ACanvas, AHeaderViewInfo, APainter, Sender); else
  if (dynamic_cast<TcxCategoryRowHeaderInfo*>(AHeaderViewInfo))
    Done = DrawCategoryItem(FCustomDrawInfo->Items[cdaCategory], ACanvas, (TcxCategoryRowHeaderInfo*)AHeaderViewInfo,
      APainter, Sender);

/*  AHeaderViewInfo->Transparent = true;
  Done = false;*/
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::cxDBVerticalGridDrawValue(
      TObject *Sender, TcxCanvas *ACanvas, TcxvgPainter *APainter,
      TcxRowValueInfo *AValueInfo, bool &Done)
{
  Done = DrawCellItem(FCustomDrawInfo->Items[cdaCell], ACanvas, AValueInfo, APainter, Sender);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::LayoutStyleClick(TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked) {
    ((TMenuItem*)Sender)->Checked = true;
    cxDBVerticalGrid->LayoutStyle = (TcxvgLayoutStyle)((TMenuItem*)Sender)->Tag;
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::AddCustomDrawInfos()
{
  FCustomDrawInfo->AddNewItem(cdaBackground, itNormal);
  FCustomDrawInfo->AddNewItem(cdaCategory, itNormal);
  FCustomDrawInfo->AddNewItem(cdaCell, itCell);
  FCustomDrawInfo->AddNewItem(cdaHeader, itNormal);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::AdjustCustomDrawItems()
{
  FCustomDrawInfo->Items[cdaBackground]->BkImageType = bkiTile;
  FCustomDrawInfo->Items[cdaCategory]->DrawingStyle = cdsGradient;
  FCustomDrawInfo->Items[cdaCategory]->ColorScheme = csBlue;
  FCustomDrawInfo->Items[cdaCell]->DrawingStyle = cdsGradient;
  FCustomDrawInfo->Items[cdaHeader]->DrawingStyle = cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall AddRectLines(TLineInfoList* ALines, const TRect &R, TColor AColor)
{
	ALines->Add(R.Left, R.Top, R.Right - R.Left, 1, AColor);
	ALines->Add(R.Left, R.Top, R.Bottom - R.Top, 1, AColor);
	ALines->Add(R.Right - 1, R.Top, R.Right - R.Left, 1, AColor);
	ALines->Add(R.Left, R.Bottom - 1, R.Right - R.Left, 1, AColor);
}

bool __fastcall TCustomDrawDemoMainForm::DrawCellItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
	  TcxRowValueInfo* AValueInfo, TcxvgPainter* APainter, TObject* Sender)
{
  TcxStyle *AStyle;     
  int i;
  ((TcxVerticalGrid*)Sender)->ViewInfo->LinesInfo->Clear();
  for (i = 0; i < ((TcxVerticalGrid*)Sender)->ViewInfo->ViewRects->BandRects->Count; i++) {
	AddRectLines(((TcxVerticalGrid*)Sender)->ViewInfo->LinesInfo,
	  ((TcxVerticalGrid*)Sender)->ViewInfo->ViewRects->BandRects->Rects[i],
	  ((TcxVerticalGrid*)Sender)->ViewInfo->BandBorderColor);
  }
  AValueInfo->Transparent = (AValueInfo->ViewParams.Bitmap != NULL) &&
	(!AValueInfo->ViewParams.Bitmap->Empty);
  if (AItem->DrawingStyle == cdsDefaultDrawing){
	APainter->DrawRowValueCell(AValueInfo);
	DrawCellsLines(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor,
	  AValueInfo->BoundsRect);
	DrawRightLine(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor,
	  AValueInfo->BoundsRect);
	return true;
  }
  bool Result;
  if (AItem->DrawingStyle == cdsDependsOnData)
  {
	if (VarToStr(cxDBVerticalGridCustomer->Properties->Values[AValueInfo->RecordIndex]) == "Y")
      AStyle = CustomDrawDemoDataDM->stCustomer;
    else
      AStyle = CustomDrawDemoDataDM->stNoCustomer;
    ACanvas->Brush->Color = AStyle->Color;
    ACanvas->FillRect(AValueInfo->VisibleRect, NULL, False);
    OwnerDrawText(ACanvas, AValueInfo, AStyle->TextColor, AStyle->Font);
    DrawCellsLines(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor,
      AValueInfo->BoundsRect);
    Result = true;
  }
  else{
    Result = DrawItem(AItem, ACanvas, cxRectInflate(AValueInfo->BoundsRect, 0, 0, 1, 1),
      (int)AItem->ColorScheme > 1);
    if (AItem->OwnerTextDraw)
      Result = Result & OwnerDrawText(ACanvas, AValueInfo, clGray, AItem->Font);
    else{
      AValueInfo->Transparent = true;
      Result = false;
    }
  }
  return Result;
}

bool __fastcall TCustomDrawDemoMainForm::OwnerDrawText(TcxCanvas* ACanvas,
  TcxRowValueInfo* AValueInfo, TColor ALineColor, TFont *AFont)
{
  bool Result = false;
  ACanvas->Pen->Color = ALineColor;
  ACanvas->Brush->Style = bsClear;
  ACanvas->Font = AFont;
  TRect ARect = cxRectInflate(AValueInfo->BoundsRect, -1, 0, 0, -1);
  ACanvas->Canvas->Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  if (dynamic_cast<TcxCustomCheckBoxViewInfo*>(AValueInfo->EditViewInfo) != 0){
    AValueInfo->Transparent = true;
  }
  else
    if (dynamic_cast<TcxCustomTextEditViewInfo*>(AValueInfo->EditViewInfo) != 0){
      ACanvas->DrawTexT(((TcxCustomTextEditViewInfo*)AValueInfo->EditViewInfo)->Text,
        cxRectInflate(AValueInfo->BoundsRect, -2, -2), 0, true);
      Result = true;
    }
  return Result;
}

//---------------------------------------------------------------------------

bool __fastcall TCustomDrawDemoMainForm::DrawHeaderItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
      TcxCustomRowHeaderInfo* AHeaderViewInfo, TcxvgPainter* APainter, TObject* Sender)
{
  bool Result;
  TRect ARect;
  ((TcxVerticalGrid*)Sender)->ViewInfo->LinesInfo->Clear();
  if ((AItem->DrawingStyle == cdsDefaultDrawing)){
    APainter->DrawRowHeader(AHeaderViewInfo);
    DrawIndents(AHeaderViewInfo, cdaHeader, ACanvas, AHeaderViewInfo->ViewParams,
      APainter, AHeaderViewInfo->RowIndents);
    Result = true;
  }
  else{
    ARect = cxRectInflate(AHeaderViewInfo->HeaderRect, 0, 0, 1, 1);
    ARect.Left = AHeaderViewInfo->RowIndents->Items[0]->Bounds.Left;
    DrawItem(FCustomDrawInfo->Items[cdaHeader], ACanvas, ARect, (int)AItem->ColorScheme > 1);
    AHeaderViewInfo->Transparent = true;
    Result = false;
  }

  TcxViewParams ACategoryViewParams;
  ACategoryViewParams.Bitmap = cxDBVerticalGrid->Styles->Category->Bitmap;
  ACategoryViewParams.Color = cxDBVerticalGrid->Styles->Category->Color;
  DrawIndents(AHeaderViewInfo, cdaCategory, ACanvas, ACategoryViewParams,
    APainter, AHeaderViewInfo->CategoryIndents);
  if ((FCustomDrawInfo->Items[cdaHeader]->DrawingStyle == cdsDefaultDrawing) ||
     (FCustomDrawInfo->Items[cdaCategory]->DrawingStyle == cdsDefaultDrawing)){
    DrawRightLine(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor,
      AHeaderViewInfo->HeaderRect);
    if (cxRectIsNull(AHeaderViewInfo->HeaderCellsRect))
      ARect =
         AHeaderViewInfo->RowIndents->Items[AHeaderViewInfo->RowIndents->Count - 1]->Bounds;
    else
      ARect = AHeaderViewInfo->HeaderCellsRect;
    TLineInfos ALineInfo = GetAdditionalLines((TcxDBVerticalGrid*)Sender, AHeaderViewInfo);
    DrawDefaultLines(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor,
      AHeaderViewInfo, ARect, ALineInfo);
  }
  AHeaderViewInfo->LinesInfo->Clear();
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::DrawIndents(TcxCustomRowHeaderInfo* AHeaderViewInfo,
  TCustomDrawArea ACustomDrawArea, TcxCanvas* ACanvas, TcxViewParams AViewParams,
  TcxvgPainter* APainter, TIndentInfoList* AIndentInfoList)
{
  if (FCustomDrawInfo->Items[ACustomDrawArea]->DrawingStyle == cdsDefaultDrawing)
    for (int I = 0; I < AIndentInfoList->Count; I++){
      TIndentInfo *AInfo = AIndentInfoList->Items[I];
      APainter->Canvas->Brush->Color = AViewParams.Color;
      APainter->Canvas->FillRect(cxRectInflate(AInfo->Bounds, 0, 0, 0, 1), AInfo->ViewParams.Bitmap);
    }
  else
  for (int I = 0; I < AIndentInfoList->Count; I++)
    DrawItem(FCustomDrawInfo->Items[ACustomDrawArea], ACanvas,
      cxRectInflate(AIndentInfoList->Items[I]->Bounds, 0, 0, 0, 1),  true);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::DrawCellsLines(TcxCanvas* ACanvas,
  TColor AColor, TRect ARect)
{
  ACanvas->Brush->Color = AColor;
  ACanvas->FillRect(cxRectBounds(ARect.Left, ARect.Top - 1, ARect.Right - ARect.Left + 1, 1), NULL, False);
  ACanvas->FillRect(cxRectBounds(ARect.Left, ARect.Bottom, ARect.Right - ARect.Left + 1, 1), NULL, False);
}
//---------------------------------------------------------------------------

TLineInfos __fastcall TCustomDrawDemoMainForm::GetAdditionalLines(TcxDBVerticalGrid* Sender,
  TcxCustomRowHeaderInfo* AHeaderViewInfo)
{
  TLineInfos Result = TLineInfos();
  if (Sender->LayoutStyle != lsBandsView) return Result;
  for (int I = 0; I < Sender->ViewInfo->BandInfo->Count; I++)
    if (Sender->ViewInfo->BandInfo->Items[I].FirstRow == AHeaderViewInfo->Row){
      Result << liTop;
      if (Sender->ViewInfo->BandInfo->Items[I].RowsCount == 1)
        Result << liBottom;
    }
  for (int I = 0; I < Sender->ViewInfo->BandInfo->Count; I++)
    if ((Sender->PrevVisibleRow(Sender->ViewInfo->BandInfo->Items[I].FirstRow)
        == AHeaderViewInfo->Row) || (Sender->LastVisibleRow() == AHeaderViewInfo->Row)){
      Result << liBottom;
      break;
    }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::DrawDefaultLines(TcxCanvas* ACanvas, TColor AColor,
      TcxCustomRowHeaderInfo* AHeaderViewInfo, TRect ARect, TLineInfos ALineInfos)
{
  FillRects(ALineInfos, AHeaderViewInfo, ACanvas, AColor);
  DrawCellsLines(ACanvas, AColor, ARect);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoMainForm::DrawRightLine(TcxCanvas* ACanvas, TColor AColor,
      TRect ARect)
{
  ACanvas->Brush->Color = AColor;
  ACanvas->FillRect(cxRectBounds(ARect.Right, ARect.Top, 1, ARect.Bottom - ARect.Top), NULL, False);
}
//---------------------------------------------------------------------------

bool __fastcall TCustomDrawDemoMainForm::DrawItem(TcxItemCustomDrawInfo *AItem, TcxCanvas *ACanvas,
  const TRect &R, bool AHorizontal)
{
  switch(AItem->DrawingStyle) {
    case cdsBkImage:
       ACanvas->FillRect(R, AItem->Bitmap, False); break;
    case cdsGradient:
      DrawGradient(ACanvas->Canvas, R,
        (TColor)ColorScheme[(int)AItem->ColorScheme][1],
        (TColor)ColorScheme[(int)AItem->ColorScheme][0],
        40, AHorizontal);
     break;
  };
  return (AItem->DrawingStyle != cdsDefaultDrawing);
}
//---------------------------------------------------------------------------

bool  __fastcall TCustomDrawDemoMainForm::DrawBackgroundItem(TcxItemCustomDrawInfo* AItem,
  TcxCanvas* ACanvas, const TRect R, TcxViewParams AViewParams, TObject* Sender)
{
  bool Result = false;
  if (AItem->DrawingStyle != cdsDefaultDrawing)
    Result = DrawItem(AItem, ACanvas, R, (int)AItem->ColorScheme > 1);
  return Result;
}
//---------------------------------------------------------------------------

bool TCustomDrawDemoMainForm::DrawCategoryItem(TcxItemCustomDrawInfo* AItem, TcxCanvas* ACanvas,
    TcxCategoryRowHeaderInfo* ACategoryViewInfo, TcxvgPainter* APainter, TObject* Sender)
{
  bool Result;
  ((TcxVerticalGrid*)Sender)->ViewInfo->LinesInfo->Clear();
  if (ACategoryViewInfo->RowIndents->Count > 0) {
    if (FCustomDrawInfo->Items[cdaHeader]->DrawingStyle != cdsDefaultDrawing) {
      int ADelta = ACategoryViewInfo->RowIndents->Items[0]->Bounds.Left - ACategoryViewInfo->HeaderRect.Left;
      TRect ARect = ACategoryViewInfo->HeaderRect;
      ARect.Left = ACategoryViewInfo->RowIndents->Items[0]->Bounds.Left;
      ARect.Right = ARect.Left + (((TcxVerticalGrid*)Sender)->ViewInfo->FullHeaderWidth - ADelta);
      DrawItem(FCustomDrawInfo->Items[cdaHeader], ACanvas, cxRectInflate(ARect, 0, 0, 0, 1), (int)FCustomDrawInfo->Items[cdaHeader]->ColorScheme > 1);
    }
    else {
      TcxViewParams ARowViewParams;
      ARowViewParams.Bitmap = cxDBVerticalGrid->Styles->Header->Bitmap;
      ARowViewParams.Color = cxDBVerticalGrid->Styles->Header->Color;
      DrawIndents(ACategoryViewInfo, cdaHeader, ACanvas, ARowViewParams, APainter, ACategoryViewInfo->RowIndents);
    }
  }
  if (AItem->DrawingStyle == cdsDefaultDrawing) {
    APainter->DrawRowHeader(ACategoryViewInfo);
    APainter->DrawRowHeaderCell(ACategoryViewInfo->CaptionsInfo->Items[0], false);
    Result = true;
  }
  else {
    DrawItem(AItem, ACanvas, cxRectInflate(ACategoryViewInfo->HeaderCellsRect, 0, 0, 1, 1), Integer(AItem->ColorScheme) > 1);
    ACategoryViewInfo->Transparent = true;
    Result = false;
  }
  DrawIndents(ACategoryViewInfo, cdaCategory, ACanvas, ACategoryViewInfo->ViewParams,
    APainter, ACategoryViewInfo->CategoryIndents);

  if ((FCustomDrawInfo->Items[cdaHeader]->DrawingStyle == cdsDefaultDrawing) ||
    (FCustomDrawInfo->Items[cdaCategory]->DrawingStyle == cdsDefaultDrawing)) {
    DrawRightLine(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor, ACategoryViewInfo->HeaderRect);
    TLineInfos ALineInfo = GetAdditionalLines((TcxDBVerticalGrid*)Sender, ACategoryViewInfo);
    DrawDefaultLines(ACanvas, ((TcxVerticalGrid*)Sender)->OptionsView->GridLineColor, ACategoryViewInfo,
      ACategoryViewInfo->HeaderCellsRect, ALineInfo);
  }
  ACategoryViewInfo->LinesInfo->Clear();
  return Result;    
}
//---------------------------------------------------------------------------

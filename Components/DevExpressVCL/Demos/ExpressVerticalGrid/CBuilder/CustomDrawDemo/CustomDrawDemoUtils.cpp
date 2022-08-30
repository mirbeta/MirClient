#include "CustomDrawDemoUtils.h"
#include "cxGraphics.hpp"
#include "cxGeometry.hpp"
#include "Classes.hpp"
#pragma link "cxVGrid"
const TcxColorSchemeArr ColorScheme = {{clSilver, clWhite, clGray},
                                    {clGold, clGreyLight, clGoldDark},
                                    {clBlueDark, clBlueLight, clBlueDark},
                                    {clGreenDark, clGreenLight, clGreen}};

const String BkImageResNames[BkImageCount] = {"TILE", "SKY", "EGYPT", "MYFACE", "CAR"};
const String ColorSchemeNames[ColorSchemeCount] = {"Grey", "Gold", "Blue", "Green"};
const String CustomDrawAreaNames[CustomDrawAreaCount] = {"Background",
    "Category", "Cell", "Header"};

void LoadImageFromRes(Graphics::TBitmap* ABitmap, String AResName)
{
  Graphics::TBitmap* Bitmap = new Graphics::TBitmap();
  TResourceStream* Rs = new TResourceStream((int)HInstance, AResName, PChar(RT_RCDATA));
  try {
    Bitmap->LoadFromStream(Rs);
    ABitmap->Assign(Bitmap);
  }
  __finally {
    delete Bitmap;
    delete Rs;
  }
}

void AddIndentRect(TRect ARect, TcxBorders ABorders, TList* AList, TLineInfos ALineInfos)
{
  if (ALineInfos.Contains(liTop))
    ABorders = ABorders << bTop;
  if (ALineInfos.Contains(liBottom))
    ABorders = ABorders << bBottom;

  PRect rec;
  if (ABorders.Contains(bLeft)) {
    rec = new TRect;
    *rec = cxRectBounds(ARect.Left-1, ARect.Top, 1, ARect.Bottom - ARect.Top + 1);
    AList->Add(rec);
  }
  if (ABorders.Contains(bTop)) {
    rec = new TRect;
    *rec = cxRectBounds(ARect.Left-1, ARect.Top - 1, ARect.Right - ARect.Left + 1, 1);
    AList->Add((void*)rec);
  }
  if (ABorders.Contains(bRight)) {
    rec = new TRect;
    *rec = cxRectBounds(ARect.Right-1, ARect.Top, 1, ARect.Bottom - ARect.Top + 1);
    AList->Add((void*)rec);
  }
  if (ABorders.Contains(bBottom)) {
    rec = new TRect;
    *rec = cxRectBounds(ARect.Left-1, ARect.Bottom, ARect.Right - ARect.Left + 1, 1);
    AList->Add((void*)rec);
  }

}

void DrawGradient(TCanvas* ACanvas, TRect ARect, TColor FromColor, TColor ToColor,
  int AStepCount, bool AHorizontal)
{
  Byte R, G, B;
  Byte FromR = GetRValue(FromColor);
  Byte FromG = GetGValue(FromColor);
  Byte FromB = GetBValue(FromColor);
  Byte ToR = GetRValue(ToColor);
  Byte ToG = GetGValue(ToColor);
  Byte ToB = GetBValue(ToColor);
  TRect SR = ARect;

  int H;

  if (AHorizontal)
    H = ARect.Right - ARect.Left;
  else
    H = ARect.Bottom - ARect.Top;

  for(int i = 0; i < AStepCount; i++) {
    if (AHorizontal)
      SR.Right = ARect.Left + MulDiv(i + 1, H, AStepCount);
    else
      SR.Bottom = ARect.Top + MulDiv(i + 1, H, AStepCount);

    R = (byte)(FromR + MulDiv(i, ToR - FromR, AStepCount - 1));
    G = (byte)(FromG + MulDiv(i, ToG - FromG, AStepCount - 1));
    B = (byte)(FromB + MulDiv(i, ToB - FromB, AStepCount - 1));
    ACanvas->Brush->Color = (TColor)RGB(R, G, B);
    ACanvas->FillRect(SR);

    if (AHorizontal)
      SR.Left = SR.Right;
    else
      SR.Top = SR.Bottom;
  }

}

void GetIndents(TLineInfos ALineInfos, TcxCustomRowHeaderInfo* AHeaderViewInfo, TList* AIndents)
{
  TcxCustomRow* ARow = AHeaderViewInfo->Row;
  bool IsBottomNeeded = true;
  if (dynamic_cast<TcxCategoryRow*>(ARow) && ARow->HasChildren())
    IsBottomNeeded = false;

  TcxBorders ABorders = TcxBorders() << bLeft << bTop << bBottom;
  if (!IsBottomNeeded)
    ABorders = ABorders >> bBottom >> bTop;
  for (int I = AHeaderViewInfo->RowIndents->Count - 1; I >= 0; I--)  {
    ARow = ARow->Parent;
    AddIndentRect(AHeaderViewInfo->RowIndents->Items[I]->Bounds, ABorders, AIndents, ALineInfos);
    ABorders = ABorders << bTop << bBottom;
  }

  bool IsTopNeeded = false;
  if (dynamic_cast<TcxCategoryRow*>(AHeaderViewInfo->Row))
    IsTopNeeded = true;

  ARow = AHeaderViewInfo->Row;
  if (IsTopNeeded)
    ABorders = TcxBorders() << bLeft << bTop << bBottom;
  else
    ABorders = TcxBorders () << bLeft << bBottom;
  for (int I = AHeaderViewInfo->CategoryIndents->Count - 1; I >= 0; I--) {
    ARow = ARow->Parent;
    if (I == (AHeaderViewInfo->CategoryIndents->Count - 1))
       ABorders = ABorders >> bBottom;
    else {
      if (dynamic_cast<TcxCategoryRow*>(ARow))
        ABorders = ABorders >> bBottom >> bTop;
      else
        if (IsTopNeeded)
          ABorders = ABorders >> bBottom;
        else
          ABorders = ABorders >> bBottom >> bTop;
    }
    AddIndentRect(AHeaderViewInfo->CategoryIndents->Items[I]->Bounds, ABorders, AIndents, ALineInfos);
  }
}

void FillRects(TLineInfos ALineInfos, TcxCustomRowHeaderInfo *AHeaderViewInfo,
  TcxCanvas *ACanvas, TColor AColor)
{
  TList *ARects = new TList();
  ACanvas->Brush->Color = AColor;
  try{
    GetIndents(ALineInfos, AHeaderViewInfo, ARects);
    for (int I = 0; I < ARects->Count; I++){
      ACanvas->FillRect(*((PRect)ARects->Items[I]), NULL, False);
      delete (PRect)ARects->Items[I];
    }
  }
  __finally{
    delete ARects;
  }
}




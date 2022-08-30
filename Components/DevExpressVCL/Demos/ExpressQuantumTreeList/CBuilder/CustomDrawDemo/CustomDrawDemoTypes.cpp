#include "CustomDrawDemoTypes.h"

const TcxColorSchemeArr ColorScheme = {{clSilver, clWhite, clGray},
                                    {clGold, clGreyLight, clGoldDark},
                                    {clBlueDark, clBlueLight, clBlueDark},
                                    {clGreenDark, clGreenLight, clGreen}};

const String BkImageResNames[BkImageCount] = {"TILE", "SKY", "EGYPT", "MYFACE", "CAR"};
const String ColorSchemeNames[ColorSchemeCount] = {"Grey", "Gold", "Blue", "Green"};
const String CustomDrawAreaNames[CustomDrawAreaCount] = {"Background",
  "BandHeader", "Cell", "CellsGroup", "Footer", "FooterCell", "Header",
  "HeaderCell", "IndentCell", "IndicatorCell", "Preview", "GroupFooter"};

void DrawGradient(TCanvas* ACanvas, TRect ARect, TColor FromColor, TColor ToColor,
  int AStepCount, bool IsVertical)
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

  if (IsVertical)
    H = ARect.Bottom - ARect.Top;
  else
    H = ARect.Right - ARect.Left;

  for(int i = 0; i < AStepCount; i++) {
    if (IsVertical)
      SR.Bottom = ARect.Top + MulDiv(i + 1, H, AStepCount);
    else
      SR.Right = ARect.Left + MulDiv(i + 1, H, AStepCount);

    R = (byte)(FromR + MulDiv(i, ToR - FromR, AStepCount - 1));
    G = (byte)(FromG + MulDiv(i, ToG - FromG, AStepCount - 1));
    B = (byte)(FromB + MulDiv(i, ToB - FromB, AStepCount - 1));
    ACanvas->Brush->Color = (TColor)RGB(R, G, B);
    ACanvas->FillRect(SR);

    if (IsVertical)
      SR.Top = SR.Bottom;
    else
      SR.Left = SR.Right;
  }

}

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






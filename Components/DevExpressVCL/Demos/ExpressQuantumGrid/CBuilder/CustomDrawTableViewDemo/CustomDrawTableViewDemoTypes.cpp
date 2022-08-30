#include "CustomDrawTableViewDemoTypes.h"

const SchemeArr ColorScheme = {{clSilver, clWhite, clGray},
                                    {clGold, clGreyLight, clGoldDark},
                                    {clBlueDark, clBlueLight, clBlueDark},
                                    {clGreenDark, clGreenLight, clGreen}};

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
#if (__BORLANDC__ >= 0x0610) // C++Builder 12
  TResourceStream* Rs = new TResourceStream((int)HInstance, AResName, (WideChar*)RT_RCDATA);
#else
  TResourceStream* Rs = new TResourceStream((int)HInstance, AResName, RT_RCDATA);
#endif
  try {
    Bitmap->LoadFromStream(Rs);
    ABitmap->Assign(Bitmap);
  }
  __finally {
    delete Bitmap;
    delete Rs;
  }
}


//---------------------------------------------------------------------------


#pragma hdrstop

#include "EditorsStylesDemoFrameControl.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

void __fastcall TcxFrameControl::AdjustFrameRgn()
{
  if (Parent) {
    const int AElipsWidth = 4;
    TRect ARect = Rect(0, 0, Width, Height);
    HRGN ARgn1 = CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom, AElipsWidth, AElipsWidth);
    InflateRect(&ARect, -2, -2);
    HRGN ARgn2 = CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right,
      ARect.Bottom, AElipsWidth, AElipsWidth);
    CombineRgn(ARgn1, ARgn2, ARgn1, RGN_XOR);
    SetWindowRgn(Handle, ARgn1, True);
    DeleteObject(ARgn1);
    DeleteObject(ARgn2);
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxFrameControl::Resize(void)
{
  AdjustFrameRgn();
  TWinControl::Resize();
}
//---------------------------------------------------------------------------

__fastcall TcxFrameControl::TcxFrameControl(Classes::TComponent* AOwner) : TWinControl(AOwner)
{
  Color = clRed;
}
//---------------------------------------------------------------------------

void __fastcall TcxFrameControl::FrameControl(TControl* AControl)
{
  FFramedControl = AControl;
  UpdateFrameControlPos();
}
//---------------------------------------------------------------------------

void __fastcall TcxFrameControl::UpdateFrameControlPos()
{
  if (FFramedControl == NULL) return;
  TRect ARect = FFramedControl->Parent->ClientRect;
  TRect ADestRect;
  if (FFramedControl->Left < 0)
    ADestRect.Left = 0;
  else
    ADestRect.Left = FFramedControl->Left;
  if (FFramedControl->Top < 0)
    ADestRect.Top = 0;
  else
    ADestRect.Top = FFramedControl->Top;
  if ((FFramedControl->Left +  FFramedControl->Width) >= ARect.Right)
    ADestRect.Right = ARect.Right - ADestRect.Left;
  else
    ADestRect.Right = FFramedControl->Width;
  if ((FFramedControl->Top + FFramedControl->Height) >= ARect.Bottom)
    ADestRect.Bottom = ARect.Bottom - ADestRect.Top;
  else
    ADestRect.Bottom = FFramedControl->Height;

  TPoint TopLeft = Point(ADestRect.Left, ADestRect.Top);

  TopLeft = FFramedControl->Parent->ClientToScreen(TopLeft);
  TopLeft = Parent->ScreenToClient(TopLeft);
  ADestRect.Left = TopLeft.x;
  ADestRect.Top = TopLeft.y;
  ADestRect.Right = ADestRect.Right + ADestRect.Left;
  ADestRect.Bottom = ADestRect.Bottom + ADestRect.Top;

  BoundsRect = ADestRect;
  BringToFront();
}
//---------------------------------------------------------------------------

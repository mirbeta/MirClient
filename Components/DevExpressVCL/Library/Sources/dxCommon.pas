{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCommon routines                                   }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxCommon;

{$I cxVer.inc}

interface

uses
  Windows, dxCore;

function dxDropDownNCHeight: Integer;
function GetCornerForRects(const EditRect, DropDownRect: TRect): TdxCorner;
function GetHitTestByCorner(ACorner: TdxCorner): Longint;

procedure DrawCloseButton(DC: HDC; var ARect: TRect; Selected, Pressed: Boolean;
  ACorner: TdxCorner);
procedure DrawSizeGrip(ADC: HDC; var ARect: TRect; ACorner: TdxCorner);

implementation

uses
  Classes;


function dxDropDownNCHeight: Integer; // copy in TcxCustomLookAndFeelPainter.CalculatePopupClientRect
begin
  Result := 2 + GetSystemMetrics(SM_CYHSCROLL);
  if Result < GetSystemMetrics(SM_CYSIZE) then
    Result := GetSystemMetrics(SM_CYSIZE);
  Inc(Result);
end;

function GetCornerForRects(const EditRect, DropDownRect: TRect): TdxCorner; //copy in TcxCustomLookAndFeelPainter.CalculatePopupClientRect
const
  Corners: array[Boolean, Boolean] of TdxCorner =
    ((coTopLeft, coBottomLeft), (coTopRight, coBottomRight));
begin
  Result :=
    Corners[EditRect.Left < DropDownRect.Right, EditRect.Bottom < DropDownRect.Bottom];
end;

function GetHitTestByCorner(ACorner: TdxCorner): Longint;
const
  HitTests: array[TdxCorner] of Longint =
    (HTTOPLEFT, HTTOPRIGHT, HTBOTTOMLEFT, HTBOTTOMRIGHT);
begin
  Result := HitTests[ACorner];
end;

procedure DrawCloseButton(DC: HDC; var ARect: TRect; Selected, Pressed: Boolean;
  ACorner: TdxCorner);
const
  Borders: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  States: array[Boolean] of Longint = (0, DFCS_PUSHED);
var
  Delta, SX, SY: Integer;
  Rgn1, Rgn2: HRGN;
  ClipRgnExists: Boolean;
begin
  SX := GetSystemMetrics(SM_CXSIZE);
  Delta := 1;//Byte(SX = 18);
  SY := GetSystemMetrics(SM_CXSIZE) + Delta;
  with ARect do
  begin
    if ACorner in [coTopRight, coBottomRight] then
      Right := Left + SX
    else
      Left := Right - SX;
    Top := (Top + Bottom - SY) div 2;
    if ACorner in [coBottomLeft, coBottomRight] then Inc(Top, Delta);
    Bottom := Top + SY;
  end;
  InflateRect(ARect, -1, -2);
  if Selected then
    DrawEdge(DC, ARect, Borders[Pressed], BF_RECT)
  else
    FrameRect(DC, ARect, GetSysColorBrush(COLOR_BTNFACE));
  InflateRect(ARect, -1, -1);
  FrameRect(DC, ARect, GetSysColorBrush(COLOR_BTNFACE));

  Rgn1 := CreateRectRgn(0, 0, 0, 0);
  ClipRgnExists := GetClipRgn(DC, Rgn1) = 1;
  with ARect do
    Rgn2 := CreateRectRgn(Left + 1, Top + 1, Right - 1, Bottom - 1);
  SelectClipRgn(DC, Rgn2);
  DeleteObject(Rgn2);
  InflateRect(ARect, 1, 1);
  if SX <> 22 then Inc(ARect.Top, Delta);
  DrawFrameControl(DC, ARect, DFC_CAPTION, DFCS_CAPTIONCLOSE or DFCS_FLAT or States[Pressed]);
  if SX <> 22 then Dec(ARect.Top, Delta);
  if ClipRgnExists then
    SelectClipRgn(DC, Rgn1)
  else
    SelectClipRgn(DC, 0);
  DeleteObject(Rgn1);
end;

procedure DrawSizeGrip(ADC: HDC; var ARect: TRect; ACorner: TdxCorner);
const
  AOffset = 2;
var
  AMirrorX, AMirrorY: Boolean;
  R: TRect;
  X, Y: Integer;
  AMDC: HDC;
  APrevB: HBITMAP;
begin
  with ARect do
  begin
    if ACorner in [coTopRight, coBottomRight] then
      Left := Right - GetSystemMetrics(SM_CXVSCROLL)
    else
      Right := Left + GetSystemMetrics(SM_CXVSCROLL);
    if ACorner in [coBottomLeft, coBottomRight] then
      Top := Bottom - GetSystemMetrics(SM_CYHSCROLL)
    else
      Bottom := Top + GetSystemMetrics(SM_CYHSCROLL);
    AMirrorX := ACorner in [coTopLeft, coBottomLeft];
    AMirrorY := ACorner in [coTopLeft, coTopRight];
  end;
  R := ARect;
  with R do
  begin
    if AMirrorX then
    begin
      Inc(Left, AOffset);
      FillRect(ADC, Rect(ARect.Left, Top, Left, Bottom), COLOR_BTNFACE + 1);
    end
    else
    begin
      Dec(Right, AOffset);
      FillRect(ADC, Rect(Right, Top, ARect.Right, Bottom), COLOR_BTNFACE + 1);
    end;
    if AMirrorY then
    begin
      Inc(Top, AOffset);
      FillRect(ADC, Rect(Left, ARect.Top, Right, Top), COLOR_BTNFACE + 1);
    end
    else
    begin
      Dec(Bottom, AOffset);
      FillRect(ADC, Rect(Left, Bottom, Right, ARect.Bottom), COLOR_BTNFACE + 1);
    end;
    X := Left;
    Y := Top;
    OffsetRect(R, -Left, -Top);
  end;

  AMDC := CreateCompatibleDC(ADC);
  APrevB := SelectObject(AMDC, CreateCompatibleBitmap(ADC, R.Right, R.Bottom));
  DrawFrameControl(AMDC, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
  with R do
    StretchBlt(ADC, X, Y, Right, Bottom,
      AMDC, Byte(AMirrorX) * (Right - 1), Byte(AMirrorY) * (Bottom - 1),
      (2 * Byte(not AMirrorX) - 1) * Right,
      (2 * Byte(not AMirrorY) - 1) * Bottom, SRCCOPY);
  DeleteObject(SelectObject(AMDC, APrevB));
  DeleteDC(AMDC);
end;

end.

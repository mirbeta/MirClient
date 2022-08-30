{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDocking                                           }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSDOCKING AND ALL ACCOMPANYING   }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxDockControlXPView;

{$I cxVer.inc}

interface

uses
  Menus, Windows, Graphics, Classes, Controls, ExtCtrls, Messages, Forms, Types,
  dxDockControl, cxLookAndFeelPainters, cxGraphics, dxCoreGraphics;

type

  { TdxDockControlXPPainter }

  TdxDockControlXPPainter = class(TdxDockControlPainter)
  protected
    function NeedRedrawOnResize: Boolean; override;
  public
    class function HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean; override;
    // CustomDockControl
    function CanVerticalCaption: Boolean; override;
    function GetCaptionButtonBorderWidths: TRect; override;
    function GetCaptionButtonDefaultGlyphSize: TSize; override;
    function GetCaptionHeight: Integer; override;

    procedure DrawBorder(ACanvas: TcxCanvas; ARect: TRect); override;
    procedure DrawCaption(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean); override;
    procedure DrawCaptionSeparator(ACanvas: TcxCanvas; ARect: TRect); override;
    procedure DrawCaptionText(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean); override;
    procedure DrawCaptionButton(ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionCloseButton(ACanvas: TcxCanvas; ARect: TRect;
      AIsActive: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionHideButton(ACanvas: TcxCanvas; ARect: TRect;
      IsActive, IsSwitched: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionMaximizeButton(ACanvas: TcxCanvas; ARect: TRect;
      IsActive, IsSwitched: Boolean; AState: TcxButtonState); override;
    procedure DrawClient(ACanvas: TcxCanvas; ARect: TRect); override;

    // AutoHideContainer
    procedure DrawHideBar(ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarButtonBackground(ACanvas: TcxCanvas;
      AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarScrollButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AArrow: TcxArrowDirection); override;
    function GetHideBarScrollButtonSize: TSize; override;
  end;

implementation

uses
  dxUxTheme, dxThemeManager, dxThemeConsts, dxDockConsts,
  cxGeometry, dxOffice11, Math;

function TdxDockControlXPPainter.CanVerticalCaption: Boolean;
begin
  if OpenTheme(totTab) <> 0 then
    Result := False
  else
    Result := inherited CanVerticalCaption;
end;

function TdxDockControlXPPainter.GetCaptionButtonBorderWidths: TRect;
begin
  if OpenTheme(totTab) <> 0 then
    Result := ScaleFactor.Apply(cxRect(2, 2, 2, 2))
  else
    Result := inherited GetCaptionButtonBorderWidths;
end;

function TdxDockControlXPPainter.GetCaptionButtonDefaultGlyphSize: TSize;
begin
  if OpenTheme(totTab) <> 0 then
    Result := ScaleFactor.Apply(cxSize(10, 10))
  else
    Result := inherited GetCaptionButtonDefaultGlyphSize;
end;

function TdxDockControlXPPainter.GetCaptionHeight: Integer;
begin
  if OpenTheme(totTab) <> 0 then
    Result := Max(2 * ScaleFactor.Apply(7) + GetFontSize, 2 * ScaleFactor.Apply(2) + GetImageHeight)
  else
    Result := inherited GetCaptionHeight;
end;

procedure TdxDockControlXPPainter.DrawBorder(ACanvas: TcxCanvas; ARect: TRect);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolBar);
  if ATheme <> 0 then
  begin
    if IsThemeBackgroundPartiallyTransparent(ATheme, TP_BUTTON, TS_HOT) then
      cxDrawTransparentControlBackground(DockControl, ACanvas, ARect);
    DrawThemeBackground(ATheme, ACanvas.Handle, TP_BUTTON, TS_HOT, ARect);
  end
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawCaption(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean);
var
  ATheme: TdxTheme;
  AClipRgn: HRGN;
  AClipRgnExists: Boolean;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
  begin
    if IsActive then
    begin
      AClipRgn := CreateRectRgn(0, 0, 0, 0);
      try
        AClipRgnExists := GetClipRgn(ACanvas.Handle, AClipRgn) = 1;
        with ARect do
        begin
          ExcludeClipRect(ACanvas.Handle, Left, Top, Left + 2, Top + 2);
          ExcludeClipRect(ACanvas.Handle, Right - 2, Top, Right, Top + 2);
          ExcludeClipRect(ACanvas.Handle, Right - 2, Bottom - 2, Right, Bottom);
          ExcludeClipRect(ACanvas.Handle, Left, Bottom - 2, Left + 2, Bottom);
        end;
        DrawThemeBackground(OpenTheme(totExplorerBar), ACanvas.Handle, EBP_NORMALGROUPBACKGROUND, 0, ARect);
        if AClipRgnExists then
          SelectClipRgn(ACanvas.Handle, AClipRgn)
        else
          SelectClipRgn(ACanvas.Handle, 0);
      finally
        DeleteObject(AClipRgn);
      end;
    end
    else
      DrawThemeBackground(ATheme, ACanvas.Handle, TABP_BODY, 0, ARect);
    DrawThemeBackground(OpenTheme(totButton), ACanvas.Handle, BP_GROUPBOX, 0, ARect);
  end
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawCaptionButton(
  ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), ARect)
  else
    inherited DrawCaptionButton(ACanvas, ARect, AIsActive, AState);
end;

procedure TdxDockControlXPPainter.DrawCaptionSeparator(ACanvas: TcxCanvas; ARect: TRect);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, TABP_BODY, 0, ARect)
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawCaptionCloseButton(
  ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer =
    (CBS_NORMAL, CBS_NORMAL, CBS_HOT, CBS_PUSHED, CBS_DISABLED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totWindow);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, WP_SMALLCLOSEBUTTON, StateMap[AState], ARect)
  else
    inherited DrawCaptionCloseButton(ACanvas, ARect, AIsActive, AState);
end;

procedure TdxDockControlXPPainter.DrawCaptionHideButton(ACanvas: TcxCanvas;
  ARect: TRect; IsActive, IsSwitched: Boolean; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer =
    (EBHP_NORMAL, EBHP_NORMAL, EBHP_HOT, EBHP_PRESSED, EBHP_NORMAL);
var
  ABitmap, ARotatedBitmap: TcxBitmap;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerBar);
  if ATheme <> 0 then
  begin
    ABitmap := TcxBitmap.CreateSize(16, 16, pf32bit);
    try
      DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, EBP_HEADERPIN, StateMap[AState], ABitmap.ClientRect);
      if not IsSwitched then
        ABitmap.Rotate(raPlus90);
      ARotatedBitmap := TcxBitmap.CreateSize(cxRectInflate(ARect, -1, -1), pf24bit);
      try
        cxStretchBlt(ARotatedBitmap.Canvas.Handle, ABitmap.Canvas.Handle,
          ARotatedBitmap.ClientRect, ABitmap.ClientRect, SRCCOPY);
        ARotatedBitmap.Transparent := True;
        ACanvas.Draw(ARect.Left + 1, ARect.Top + 1, ARotatedBitmap);
      finally
        ARotatedBitmap.Free;
      end;
    finally
      ABitmap.Free;
    end;
  end
  else
    inherited DrawCaptionHideButton(ACanvas, ARect, IsActive, IsSwitched, AState);
end;

procedure TdxDockControlXPPainter.DrawCaptionMaximizeButton(ACanvas: TcxCanvas;
  ARect: TRect; IsActive, IsSwitched: Boolean; AState: TcxButtonState);
const
  MaximizeButtonStateMap: array[TcxButtonState] of Integer =
    (MAXBS_NORMAL, MAXBS_NORMAL, MAXBS_HOT, MAXBS_PUSHED, MAXBS_DISABLED);
  RestoreButtonStateMap: array[TcxButtonState] of Integer =
    (RBS_NORMAL, RBS_NORMAL, RBS_HOT, RBS_PUSHED, RBS_DISABLED);
var
  ATheme: TdxTheme;
  APart, APartState: Integer;
begin
  ATheme := OpenTheme(totWindow);
  if ATheme = 0 then
    inherited DrawCaptionMaximizeButton(ACanvas, ARect, IsActive, IsSwitched, AState)
  else
  begin
    if IsSwitched then
    begin
      APart := WP_RESTOREBUTTON;
      APartState := RestoreButtonStateMap[AState];
    end
    else
    begin
      APart := WP_MAXBUTTON;
      APartState := MaximizeButtonStateMap[AState];
    end;
    DrawThemeBackground(ATheme, ACanvas.Handle, APart, APartState, ARect);
  end;
end;

procedure TdxDockControlXPPainter.DrawClient(ACanvas: TcxCanvas; ARect: TRect);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, TABP_BODY, 0, ARect)
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawCaptionText(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean);
var
  R: TRect;
begin
  if OpenTheme(totTab) <> 0 then
  begin
    if IsValidImageIndex(DockControl.ImageIndex) then
    begin
      R.Left := ARect.Left + 2 * GetSpaceBetweenCaptionButtons;
      R.Top := ARect.Top + (ARect.Bottom - ARect.Top - GetImageHeight) div 2;
      R.Right := R.Left + GetImageWidth;
      R.Bottom := R.Top + GetImageHeight;
      if RectInRect(R, ARect) then
      begin
        cxDrawImage(ACanvas, R, nil, DockControl.Images, DockControl.ImageIndex, True, GetCaptionColorPalette, ScaleFactor);
        ARect.Left := R.Right + 2 * GetSpaceBetweenCaptionButtons;
      end;
    end;

    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := GetFont;
    ACanvas.Font.Color := GetCaptionFontColor(IsActive);
    cxDrawText(ACanvas.Handle, DockControl.Caption, ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
  end
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawHideBar(ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, TABP_BODY, 0, ARect)
  else
    inherited;
end;

procedure TdxDockControlXPPainter.DrawHideBarButtonBackground(
  ACanvas: TcxCanvas; AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition);
var
  R: TRect;
  ABitmap: TcxBitmap;
  ATheme: TdxTheme;
  Temp: Integer;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
  begin
    R := AButton.Bounds;
    OffsetRect(R, -R.Left, -R.Top);
    if APosition in [ahpLeft, ahpRight] then
    begin
      Temp := R.Right;
      R.Right := R.Left + (R.Bottom - R.Top);
      R.Bottom := R.Top + (Temp - R.Left);
    end;

    ABitmap := TcxBitmap.CreateSize(R, pf32bit);
    try
      DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, TABP_TOPTABITEM, TTIS_NORMAL, R);
      case APosition of
        ahpTop: ABitmap.Rotate(ra180);
        ahpLeft: ABitmap.Rotate(raMinus90);
        ahpRight: ABitmap.Rotate(raPlus90);
      end;
      cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, AButton.Bounds, cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end
  else
    inherited DrawHideBarButtonBackground(ACanvas, AButton, APosition);
end;

procedure TdxDockControlXPPainter.DrawHideBarScrollButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AArrow: TcxArrowDirection);
const
  ArrowButtonPartIdMap: array [TcxArrowDirection] of Integer = (SPNP_UP, SPNP_DOWN,
    SPNP_DOWNHORZ, SPNP_UPHORZ);
  ArrowButtonStateIdMap: array [TcxArrowDirection, TcxButtonState] of Integer = (
    (UPS_NORMAL, UPS_NORMAL, UPS_HOT, UPS_PRESSED, UPS_DISABLED),
    (DNS_NORMAL, DNS_NORMAL, DNS_HOT, DNS_PRESSED, DNS_DISABLED),
    (DNHZS_NORMAL, DNHZS_NORMAL, DNHZS_HOT, DNHZS_PRESSED, DNHZS_DISABLED),
    (UPHZS_NORMAL, UPHZS_NORMAL, UPHZS_HOT, UPHZS_PRESSED, UPHZS_DISABLED)
  );
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totSpin);
  if ATheme <> 0 then
  begin
    DrawThemeBackground(ATheme, ACanvas.Handle, ArrowButtonPartIdMap[AArrow],
      ArrowButtonStateIdMap[AArrow, AState], ARect);
  end
  else
    inherited DrawHideBarScrollButton(ACanvas, ARect, AState, AArrow);
end;

function TdxDockControlXPPainter.GetHideBarScrollButtonSize: TSize;
begin
  if OpenTheme(totSpin) <> 0 then
    Result := ScaleFactor.Apply(cxSize(17, 17))
  else
    Result := inherited GetHideBarScrollButtonSize;
end;

class function TdxDockControlXPPainter.HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean;
begin
  Result := AStyle = lfsNative;
end;

function TdxDockControlXPPainter.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

initialization
  dxDockingPaintersManager.Register(TdxDockControlXPPainter);

finalization
  dxDockingPaintersManager.Unregister(TdxDockControlXPPainter);
end.

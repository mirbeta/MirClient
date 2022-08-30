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

unit dxDockControlNETView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Menus, Windows, Graphics, Classes, Controls, ExtCtrls, Messages, Forms,
  dxDockControl, dxDockPanel, cxLookAndFeelPainters, cxGraphics;

type
  TdxDockControlNETPainter = class(TdxDockControlPainter)
  protected
    function GetCaptionColor(IsActive: Boolean): TColor; override;
    function GetCaptionFontColor(IsActive: Boolean): TColor; override;
    function GetHideBarColor: TColor; override;
    function GetNETBackColor: TColor; virtual;
  public
    class function HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean; override;
    // CustomDockControl
    function CanVerticalCaption: Boolean; override;
    function GetCaptionHeight: Integer; override;

    procedure DrawBorder(ACanvas: TcxCanvas; ARect: TRect); override;
    procedure DrawCaption(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean); override;
    procedure DrawCaptionText(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean); override;
    procedure DrawCaptionButton(ACanvas: TcxCanvas;
      ARect: TRect; AIsActive: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionCloseButton(ACanvas: TcxCanvas; ARect: TRect;
      AIsActive: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionHideButton(ACanvas: TcxCanvas; ARect: TRect;
      IsActive, IsSwitched: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionMaximizeButton(ACanvas: TcxCanvas; ARect: TRect;
      IsActive, IsSwitched: Boolean; AState: TcxButtonState); override;
    function GetCaptionButtonBorderWidths: TRect; override;
    function GetCaptionButtonDefaultGlyphSize: TSize; override;

    // AutoHideContainer
    function GetHideBarHeight: Integer; override;
    function GetHideBarWidth: Integer; override;

    procedure DrawHideBar(ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarButtonBackground(ACanvas: TcxCanvas;
      AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarScrollButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AArrow: TcxArrowDirection); override;
  end;

implementation

uses
  Math, dxDockConsts, cxGeometry, dxDPIAwareUtils;

{ TdxDockControlNETPainter }

function TdxDockControlNETPainter.GetNETBackColor: TColor;
var
  r, g, b, m, d, md: Integer;
begin
  Result := ColorToRGB(GetColor);
  r := GetRValue(Result);
  g := GetGValue(Result);
  b := GetBValue(Result);
  m := Max(Max(r, g), b);
  d := $23;
  md := (255 - (m + d));
  if md > 0 then md := 0;
  Inc(r, d + md);
  Inc(g, d + md);
  Inc(b, d + md);
  Result := RGB(r, g, b);
end;

function TdxDockControlNETPainter.GetCaptionButtonBorderWidths: TRect;
begin
  Result := ScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TdxDockControlNETPainter.GetCaptionButtonDefaultGlyphSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(9, 9));
end;

function TdxDockControlNETPainter.GetCaptionColor(IsActive: Boolean): TColor;
begin
  if IsActive then
    Result := clActiveCaption
  else
    Result := GetColor;
end;

function TdxDockControlNETPainter.GetCaptionFontColor(IsActive: Boolean): TColor;
begin
  if IsActive then
    Result := clCaptionText
  else
    Result := clBlack;
end;

function TdxDockControlNETPainter.GetHideBarColor: TColor;
begin
  Result := GetNETBackColor;
end;

procedure TdxDockControlNETPainter.DrawBorder(ACanvas: TcxCanvas; ARect: TRect);
var
  ABorderWidths: TRect;
begin
  ACanvas.Brush.Color := ColorToRGB(GetBorderColor);
  ACanvas.Brush.Style := bsSolid;
  with ARect do
  begin
    ABorderWidths := GetBorderWidths;
    ACanvas.FillRect(Rect(Left, Top, Left + ABorderWidths.Left, Bottom));
    ACanvas.FillRect(Rect(Left, Bottom - ABorderWidths.Bottom, Right, Bottom));
    ACanvas.FillRect(Rect(Right - ABorderWidths.Right, Top, Left + Right, Bottom));
    ACanvas.FillRect(Rect(Left, Top, Right, Top + ABorderWidths.Top));
  end;
  if DockControl.AutoHide then
  begin
    DrawColorEdge(ACanvas, ARect, GetColor, etSunkenInner, [epTopLeft]);
    DrawColorEdge(ACanvas, ARect, GetColor, etRaisedInner, [epBottomRight]);
  end;
end;

procedure TdxDockControlNETPainter.DrawCaption(
  ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean);
begin
  ACanvas.Brush.Style := bsSolid;
  if IsActive then
  begin
    ACanvas.Pen.Color := ColorToRGB(GetCaptionColor(IsActive));
    ACanvas.Brush.Color := ColorToRGB(GetCaptionColor(IsActive));
    ACanvas.FillRect(ARect);
  end
  else
  begin
    ACanvas.FillRect(ARect, ColorToRGB(GetCaptionColor(IsActive)));
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := DarkColor(GetCaptionColor(IsActive));
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.ExcludeClipRect(cxRectSetSize(ARect, 1, 1));
    ACanvas.ExcludeClipRect(cxRect(ARect.Right - 1, ARect.Top, ARect.Right, ARect.Top + 1));
    ACanvas.ExcludeClipRect(cxRect(ARect.Right - 1, ARect.Bottom - 1, ARect.Right, ARect.Bottom));
    ACanvas.ExcludeClipRect(cxRect(ARect.Left, ARect.Bottom - 1, ARect.Left + 1, ARect.Bottom));
    ACanvas.Rectangle(ARect);
  end;
end;

procedure TdxDockControlNETPainter.DrawCaptionButton(
  ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState);
begin
  case AState of
    cxbsPressed:
      begin
        DrawColorEdge(ACanvas, ARect, GetCaptionColor(AIsActive), etSunkenInner, [epTopLeft]);
        DrawColorEdge(ACanvas, ARect, GetCaptionColor(AIsActive), etSunkenOuter, [epBottomRight]);
      end;

    cxbsHot:
      begin
        DrawColorEdge(ACanvas, ARect, GetCaptionColor(AIsActive), etRaisedOuter, [epTopLeft]);
        DrawColorEdge(ACanvas, ARect, GetCaptionColor(AIsActive), etRaisedInner, [epBottomRight]);
      end;
  end;
end;

procedure TdxDockControlNETPainter.DrawCaptionCloseButton(
  ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState);
begin
  DrawCaptionButton(ACanvas, ARect, AIsActive, AState);

  ARect := cxRectCenter(ARect, GetCaptionButtonDefaultGlyphSize);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ColorToRGB(GetCaptionSignColor(AIsActive, AState));

  ACanvas.Line(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Bottom - 1);
  ACanvas.Line(ARect.Right - 2, ARect.Top + 1, ARect.Left, ARect.Bottom - 1);
end;

procedure TdxDockControlNETPainter.DrawCaptionHideButton(ACanvas: TcxCanvas;
  ARect: TRect; IsActive, IsSwitched: Boolean; AState: TcxButtonState);
var
  ASide1, ASide2, ACenter, ABase, ADelta: Integer;
begin
  DrawCaptionButton(ACanvas, ARect, IsActive, AState);

  ARect := cxRectCenter(ARect, GetCaptionButtonDefaultGlyphSize);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ColorToRGB(GetCaptionSignColor(IsActive, AState));

  if IsSwitched then
  begin
    ASide1 := ARect.Top + ScaleFactor.Apply(1);
    ASide2 := ARect.Bottom - ScaleFactor.Apply(4);
    ACenter := (ASide1 + ASide2) div 2;
    ABase := ARect.Left + ScaleFactor.Apply(2);
    ADelta := ScaleFactor.Apply(1);

    ACanvas.Polyline([Point(ABase, ASide1 - ADelta), Point(ABase, ASide1), Point(ARect.Right - ADelta, ASide1),
      Point(ARect.Right - ADelta, ASide2), Point(ABase, ASide2), Point(ABase, ASide2 + ADelta), Point(ABase, ASide1)]);
    ACanvas.FillRect(Rect(ABase, ASide2 - ADelta, ARect.Right - ADelta, ASide2), ACanvas.Pen.Color);
    ACanvas.Line(ARect.Left, ACenter, ABase, ACenter);
  end
  else
  begin
    ASide1 := ARect.Left + ScaleFactor.Apply(2);
    ASide2 := ARect.Right - ScaleFactor.Apply(3);
    ACenter := (ASide1 + ASide2) div 2;
    ABase := ARect.Bottom - ScaleFactor.Apply(3);
    ADelta := ScaleFactor.Apply(1);

    ACanvas.Polyline([Point(ASide1 - ADelta, ABase), Point(ASide1, ABase), Point(ASide1, ARect.Top),
      Point(ASide2, ARect.Top), Point(ASide2, ABase), Point(ASide2 + ADelta, ABase), Point(ASide1, ABase)]);
    ACanvas.FillRect(Rect(ASide2 - ADelta, ARect.Top, ASide2, ABase), ACanvas.Pen.Color);
    ACanvas.Line(ACenter, ABase, ACenter, ARect.Bottom);
  end;
end;

procedure TdxDockControlNETPainter.DrawCaptionMaximizeButton(ACanvas: TcxCanvas;
  ARect: TRect; IsActive, IsSwitched: Boolean; AState: TcxButtonState);
begin
  DrawCaptionButton(ACanvas, ARect, IsActive, AState);

  ARect := cxRectCenter(ARect, GetCaptionButtonDefaultGlyphSize);
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ColorToRGB(GetCaptionSignColor(IsActive, AState));

  if IsSwitched then
  begin
    ACanvas.Rectangle(ARect.Left + 4, ARect.Top, ARect.Right + 1, ARect.Bottom - 3);
    ACanvas.Rectangle(ARect.Left + 1, ARect.Top + 3, ARect.Right - 2, ARect.Bottom);
  end
  else
    ACanvas.Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Bottom - 1);
end;

procedure TdxDockControlNETPainter.DrawCaptionText(ACanvas: TcxCanvas; ARect: TRect; IsActive: Boolean);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := GetFont;
  ACanvas.Font.Color := ColorToRGB(GetCaptionFontColor(IsActive));
  cxDrawText(ACanvas.Handle, DockControl.Caption, ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
end;

procedure TdxDockControlNETPainter.DrawHideBar(
  ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition);
begin
  ACanvas.FillRect(ARect, GetHideBarColor);
  case APosition of
    ahpLeft:
      ARect.Right := ARect.Left + GetHideBarVertInterval;
    ahpTop:
      ARect.Bottom := ARect.Top + GetHideBarVertInterval;
    ahpRight:
      ARect.Left := ARect.Right - GetHideBarVertInterval;
    ahpBottom:
      ARect.Top := ARect.Bottom - GetHideBarVertInterval;
  end;
  ACanvas.FillRect(ARect, ColorToRGB(GetHideBarButtonColor));
  ACanvas.ExcludeClipRect(ARect);
end;

procedure TdxDockControlNETPainter.DrawHideBarButtonBackground(
  ACanvas: TcxCanvas; AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition);
const
  Edges: array[TdxAutoHidePosition] of TdxEdgePositions = ([epTop, epBottomRight],
    [epLeft, epBottomRight], [epTopLeft, epBottom], [epTopLeft, epRight], []);
begin
  ACanvas.FillRect(AButton.Bounds, ColorToRGB(GetHideBarButtonColor));
  DrawColorEdge(ACanvas, AButton.Bounds, GetHideBarButtonColor, etFlat, Edges[APosition]);
end;

procedure TdxDockControlNETPainter.DrawHideBarScrollButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState; AArrow: TcxArrowDirection);
begin
  cxLookAndFeelPaintersManager.GetPainter(lfsUltraFlat).DrawScaledArrow(ACanvas, ARect, AState, AArrow, ScaleFactor);
end;

function TdxDockControlNETPainter.CanVerticalCaption: Boolean;
begin
  Result := False;
end;

function TdxDockControlNETPainter.GetCaptionHeight: Integer;
begin
  Result := ScaleFactor.Apply(5) + GetFontSize + ScaleFactor.Apply(5);
end;

function TdxDockControlNETPainter.GetHideBarHeight: Integer;
var
  AIndent: Integer;
begin
  AIndent := ScaleFactor.Apply(8);
  Result := AIndent + GetFontSize + AIndent;
  AIndent := ScaleFactor.Apply(2);
  Result := Max(Result, GetHideBarVertInterval + AIndent + GetImageHeight + AIndent + GetHideBarVertInterval);
end;

function TdxDockControlNETPainter.GetHideBarWidth: Integer;
var
  AIndent: Integer;
begin
  AIndent := ScaleFactor.Apply(8);
  Result := AIndent + GetFontSize + AIndent;
  AIndent := ScaleFactor.Apply(2);
  Result := Max(Result, GetHideBarVertInterval + AIndent + GetImageWidth + AIndent + GetHideBarVertInterval);
end;

class function TdxDockControlNETPainter.HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean;
begin
  Result := (AStyle = lfsUltraFlat) or (AStyle = lfsFlat);
end;

initialization
  dxDockingPaintersManager.Register(TdxDockControlNETPainter);

finalization
  dxDockingPaintersManager.Unregister(TdxDockControlNETPainter);

end.

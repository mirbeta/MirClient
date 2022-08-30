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

unit dxDockControlOfficeView;

{$I cxVer.inc}

interface

uses
  Types, Menus, Windows, Graphics, Classes, Controls, ExtCtrls, Messages, Forms,
  dxDockControl, dxDockControlNETView, cxLookAndFeelPainters, cxGraphics;

type

  { TdxDockControlOfficePainter }

  TdxDockControlOfficePainter = class(TdxDockControlNETPainter)
  protected
    class procedure CreateColors; override;
    class procedure RefreshColors; override;
    class procedure ReleaseColors; override;

    function GetBorderColor: TColor; override;
    function GetCaptionColor(IsActive: Boolean): TColor; override;
    function GetCaptionFontColor(IsActive: Boolean): TColor; override;
    function GetCaptionSignColor(IsActive: Boolean; AState: TcxButtonState): TColor; override;

    function NeedRedrawOnResize: Boolean; override;
  public
    class function HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean; override;
    // CustomDockControl
    procedure DrawBorder(ACanvas: TcxCanvas; ARect: TRect); override;
    procedure DrawCaptionButton(ACanvas: TcxCanvas;
      ARect: TRect; AIsActive: Boolean; AState: TcxButtonState); override;
    procedure DrawCaptionMaximizeButton(ACanvas: TcxCanvas; ARect: TRect;
      IsActive, IsSwitched: Boolean; AState: TcxButtonState); override;
    procedure DrawClient(ACanvas: TcxCanvas; ARect: TRect); override;

    // AutoHideContainer
    procedure DrawHideBar(ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarButton(ACanvas: TcxCanvas;
      AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarButtonBackground(ACanvas: TcxCanvas;
      AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarButtonSection(ACanvas: TcxCanvas;
      AButtonSection: TdxDockSiteHideBarButtonSection; APosition: TdxAutoHidePosition); override;
    procedure DrawHideBarScrollButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AArrow: TcxArrowDirection); override;
  end;

implementation

uses
  dxDockConsts, dxOffice11, cxGeometry;

{ TdxDockControlOfficePainter }

class procedure TdxDockControlOfficePainter.CreateColors;
begin
//  CreateOffice11Colors;
// is calling indirectly in cxLookAndFeels
end;

class procedure TdxDockControlOfficePainter.RefreshColors;
begin
  RefreshOffice11Colors;
end;

class procedure TdxDockControlOfficePainter.ReleaseColors;
begin
//  ReleaseOffice11Colors;
// is calling indirectly in cxLookAndFeels
end;

function TdxDockControlOfficePainter.GetBorderColor: TColor;
begin
  Result := dxOffice11BarFloatingBorderColor2;
end;

function TdxDockControlOfficePainter.GetCaptionColor(IsActive: Boolean): TColor;
begin
  if IsActive then
    Result := dxOffice11BarFloatingCaptionColor
  else Result := dxOffice11BarFloatingBorderColor2;
end;

function TdxDockControlOfficePainter.GetCaptionFontColor(IsActive: Boolean): TColor;
begin
  if IsActive then
    Result := dxOffice11BarFloatingCaptionTextColor1
  else Result := dxOffice11BarFloatingCaptionTextColor2;
end;

function TdxDockControlOfficePainter.GetCaptionSignColor(IsActive: Boolean; AState: TcxButtonState): TColor;
begin
  Result := GetCaptionFontColor(IsActive and not (AState in [cxbsHot, cxbsPressed]));
end;

class function TdxDockControlOfficePainter.HasLookAndFeelStyle(AStyle: TcxLookAndFeelStyle): Boolean;
begin
  Result := AStyle = lfsOffice11;
end;

function TdxDockControlOfficePainter.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

procedure TdxDockControlOfficePainter.DrawBorder(ACanvas: TcxCanvas; ARect: TRect);
var
  ABorderWidths: TRect;
begin
  ACanvas.Brush.Style := bsSolid;
  with ARect do
  begin
    ABorderWidths := GetBorderWidths;
    ACanvas.FillRect(cxRectSetHeight(ARect, ABorderWidths.Top), dxOffice11BarFloatingBorderColor2);
    ACanvas.FillRect(cxRectSetBottom(ARect, ARect.Bottom, ABorderWidths.Bottom), dxOffice11BarFloatingBorderColor1);
    FillGradientRect(ACanvas.Handle, Rect(Left, Top, Left + ABorderWidths.Left, Bottom),
      dxOffice11BarFloatingBorderColor2, dxOffice11BarFloatingBorderColor1, False);
    FillGradientRect(ACanvas.Handle, Rect(Right - ABorderWidths.Right, Top, Left + Right, Bottom),
      dxOffice11BarFloatingBorderColor2, dxOffice11BarFloatingBorderColor1, False);
  end;
  if DockControl.AutoHide then
    DrawColorEdge(ACanvas, ARect, dxOffice11BarFloatingBorderColor1, etStandard, [epRect]);
end;

procedure TdxDockControlOfficePainter.DrawCaptionButton(
  ACanvas: TcxCanvas; ARect: TRect; AIsActive: Boolean; AState: TcxButtonState);
begin
  case AState of
    cxbsPressed:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11SelectedDownColor1, dxOffice11SelectedDownColor2, False);
        Office11FrameSelectedRect(ACanvas.Handle, ARect);
      end;

    cxbsHot:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11SelectedColor1, dxOffice11SelectedColor2, False);
        Office11FrameSelectedRect(ACanvas.Handle, ARect);
      end;
  end;
end;

procedure TdxDockControlOfficePainter.DrawCaptionMaximizeButton(
  ACanvas: TcxCanvas; ARect: TRect; IsActive, IsSwitched: Boolean; AState: TcxButtonState);
var
  APoints: array[0..2] of TPoint;
begin
  DrawCaptionButton(ACanvas, ARect, IsActive, AState);

  ARect := cxRectCenter(ARect, cxSize(ScaleFactor.Apply(7)));
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := ColorToRGB(GetCaptionSignColor(IsActive, AState));
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := ColorToRGB(GetCaptionSignColor(IsActive, AState));

  if DockControl.SideContainer is TdxVertContainerDockSite then
    if IsSwitched then
    begin
      APoints[0] := Point(ARect.Right - 1, ARect.Top);
      APoints[1] := Point(ARect.Left, ARect.Top);
    end
    else
    begin
      APoints[0] := Point(ARect.Right - 1, ARect.Bottom - 1);
      APoints[1] := Point(ARect.Left, ARect.Bottom - 1);
    end
  else
    if IsSwitched then
    begin
      APoints[0] := Point(ARect.Left, ARect.Top);
      APoints[1] := Point(ARect.Left, ARect.Bottom - 1);
    end
    else
    begin
      APoints[0] := Point(ARect.Right - 1, ARect.Top);
      APoints[1] := Point(ARect.Right - 1, ARect.Bottom - 1);
    end;
  APoints[2] := Point(ARect.Left + cxRectWidth(ARect) div 2, ARect.Top + cxRectHeight(ARect) div 2);

  ACanvas.Polygon(APoints);
end;

procedure TdxDockControlOfficePainter.DrawClient(ACanvas: TcxCanvas; ARect: TRect);
begin
  FillGradientRect(ACanvas.Handle, ARect, dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, False);
end;

procedure TdxDockControlOfficePainter.DrawHideBar(
  ACanvas: TcxCanvas; ARect: TRect; APosition: TdxAutoHidePosition);
begin
  case APosition of
    ahpLeft:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11ToolbarsColor2, dxOffice11ToolbarsColor2, True);
        ARect.Right := ARect.Left + GetHideBarVertInterval;
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11DockColor1, dxOffice11DockColor1, True);
      end;

    ahpTop:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11ToolbarsColor2, dxOffice11ToolbarsColor1, True);
        ARect.Bottom := ARect.Top + GetHideBarVertInterval;
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11DockColor1, dxOffice11DockColor2, True);
      end;

    ahpRight:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor1, True);
        ARect.Left := ARect.Right - GetHideBarVertInterval;
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11DockColor2, dxOffice11DockColor2, True);
      end;

    ahpBottom:
      begin
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11ToolbarsColor2, dxOffice11ToolbarsColor1, True);
        ARect.Top := ARect.Bottom - GetHideBarVertInterval;
        FillGradientRect(ACanvas.Handle, ARect, dxOffice11DockColor1, dxOffice11DockColor2, True);
      end;
  end;

  with ARect do
    ExcludeClipRect(ACanvas.Handle, Left, Top, Right, Bottom);
end;

procedure TdxDockControlOfficePainter.DrawHideBarButton(
  ACanvas: TcxCanvas; AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition);
const
  Edges: array[TdxAutoHidePosition] of TdxEdgePositions = (
    [epTop, epBottomRight], [epLeft, epBottomRight],
    [epTopLeft, epBottom], [epTopLeft, epRight], []
  );
begin
  inherited DrawHideBarButton(ACanvas, AButton, APosition);
  DrawColorEdge(ACanvas, AButton.Bounds, dxOffice11SelectedBorderColor, etStandard, Edges[APosition]);
end;

procedure TdxDockControlOfficePainter.DrawHideBarButtonBackground(
  ACanvas: TcxCanvas; AButton: TdxDockSiteHideBarButton; APosition: TdxAutoHidePosition);
begin
  // do nothing
end;

procedure TdxDockControlOfficePainter.DrawHideBarButtonSection(ACanvas: TcxCanvas;
  AButtonSection: TdxDockSiteHideBarButtonSection; APosition: TdxAutoHidePosition);
begin
 if AButtonSection.Expanded then
  begin
    case APosition of
      ahpLeft, ahpTop:
        FillGradientRect(ACanvas.Handle, AButtonSection.Bounds,
          dxOffice11DownedSelectedColor, dxOffice11DownedColor, APosition = ahpLeft);
      ahpRight, ahpBottom:
        FillGradientRect(ACanvas.Handle, AButtonSection.Bounds,
          dxOffice11DownedColor, dxOffice11DownedSelectedColor, APosition = ahpRight);
    end;
  end;
  inherited DrawHideBarButtonSection(ACanvas, AButtonSection, APosition);
end;

procedure TdxDockControlOfficePainter.DrawHideBarScrollButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AArrow: TcxArrowDirection);
begin
  cxLookAndFeelPaintersManager.GetPainter(lfsOffice11).DrawScaledArrow(ACanvas, ARect, AState, AArrow, ScaleFactor);
end;

initialization
  dxDockingPaintersManager.Register(TdxDockControlOfficePainter);

finalization
  dxDockingPaintersManager.Unregister(TdxDockControlOfficePainter);
end.

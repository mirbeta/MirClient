{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl main components                     }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxLayoutPainters;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Classes, Types, Windows, Graphics, cxGraphics, dxLayoutLookAndFeels, dxLayoutContainer,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutCommon;

type
  { Painters }

  TdxLayoutGroupStandardPainter = class(TdxLayoutGroupPainter)
  protected
    function GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass; override;
    procedure DoDrawBorders(ACanvas: TcxCanvas); override;
    procedure DrawFrame(ACanvas: TcxCanvas); virtual;
  end;

  TdxLayoutGroupOfficePainter = class(TdxLayoutGroupStandardPainter)
  protected
    function GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass; override;
    procedure DrawFrame(ACanvas: TcxCanvas); override;
  end;

  { TdxLayoutGroupCxLookAndFeelPainter }

  TdxLayoutGroupCxLookAndFeelPainter = class(TdxLayoutGroupStandardPainter)
  private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    function CanDrawBackground: Boolean; override;
    function GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass; override;
    function HasCaptionBackground: Boolean; override;
    procedure DrawFrame(ACanvas: TcxCanvas); override;
  public
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

  TdxLayoutGroupWebPainter = class(TdxLayoutGroupPainter)
  private
    function GetCaptionSeparatorAreaBounds: TRect;
    function GetCaptionSeparatorBounds: TRect;

    function GetLayoutLookAndFeel: TdxLayoutWebLookAndFeel;
  protected
    procedure DoDrawBorders(ACanvas: TcxCanvas); override;
    function HasCaptionBackground: Boolean; override;
    function HasCaptionSeparator: Boolean;
    function CanDrawCaptionSeparator: Boolean; virtual;
    procedure DoDrawCaptionSeparator(ACanvas: TcxCanvas); virtual;
    procedure DrawCaptionSeparator(ACanvas: TcxCanvas);
    procedure DrawFrame(ACanvas: TcxCanvas); virtual;
    property LayoutLookAndFeel: TdxLayoutWebLookAndFeel read GetLayoutLookAndFeel;
  end;

  TdxLayoutItemCxLookAndFeelPainter = class(TdxLayoutItemPainter)
  protected
    function CanDrawBackground: Boolean; override;
  end;

  TdxLayoutGroupCaptionStandardPainter = class(TdxLayoutGroupCaptionPainter)
  protected
    function DrawEnabled: Boolean; override;
    procedure DoDrawText(ACanvas: TcxCanvas); override;

    function DrawVCLThemesEnabled: Boolean; virtual;
  end;

  { TdxLayoutGroupCaptionCxLookAndFeelPainter }

  TdxLayoutGroupCaptionCxLookAndFeelPainter = class(TdxLayoutGroupCaptionPainter)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  public
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

implementation

uses
  UxTheme, Themes, Math, cxGeometry, cxControls;

const
  dxCaptionPositionMap: array[Boolean, TdxLayoutSide] of TcxGroupBoxCaptionPosition =
    ((cxgpCenter, cxgpCenter, cxgpCenter, cxgpCenter),
    (cxgpLeft, cxgpRight, cxgpTop, cxgpBottom));

type
  TdxCustomLayoutItemCaptionViewInfoAccess = class(TdxCustomLayoutItemCaptionViewInfo);
  TdxCustomLayoutLookAndFeelAccess = class(TdxCustomLayoutLookAndFeel);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxLayoutGroupViewInfoAccess = class(TdxLayoutGroupViewInfo);
  TdxLayoutGroupCaptionViewInfoAccess = class(TdxLayoutGroupCaptionViewInfo);

function GroupViewInfo(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxLayoutGroupViewInfoAccess;
begin
  Result := TdxLayoutGroupViewInfoAccess(AViewInfo as TdxLayoutGroupViewInfo);
end;

{ TdxLayoutGroupStandardPainter }

function TdxLayoutGroupStandardPainter.GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass;
begin
  Result := TdxLayoutGroupCaptionStandardPainter;
end;

procedure TdxLayoutGroupStandardPainter.DoDrawBorders(ACanvas: TcxCanvas);
begin
  inherited;
  DrawFrame(ACanvas);
end;

procedure TdxLayoutGroupStandardPainter.DrawFrame(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := LayoutLookAndFeel.GetGroupFrameBounds(ViewInfo);
  if cxIsVCLThemesEnabled then
    cxStyleServices.DrawElement(ACanvas.Handle, cxStyleServices.GetElementDetails(tbGroupBoxNormal), R)
  else
  begin
    ACanvas.DrawEdge(R, True, True);
    InflateRect(R, -1, -1);
    ACanvas.DrawEdge(R, False, False);
  end;
end;

{ TdxLayoutGroupOfficePainter }

function TdxLayoutGroupOfficePainter.GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass;
begin
  Result := TdxLayoutGroupCaptionPainter;
end;

procedure TdxLayoutGroupOfficePainter.DrawFrame(ACanvas: TcxCanvas);
const
  BordersMap: array[Boolean] of TcxBorders = ([bTop], [bLeft]);
var
  R: TRect;
begin
  R := LayoutLookAndFeel.GetGroupFrameBounds(ViewInfo);
  ACanvas.DrawEdge(R, True, True, BordersMap[GroupViewInfo(ViewInfo).IsVerticalCaption]);
  if GroupViewInfo(ViewInfo).IsVerticalCaption then
    Inc(R.Left)
  else
    Inc(R.Top);
  ACanvas.DrawEdge(R, False, False, BordersMap[GroupViewInfo(ViewInfo).IsVerticalCaption]);
end;

{ TdxLayoutGroupWebPainter }

function TdxLayoutGroupWebPainter.GetCaptionSeparatorAreaBounds: TRect;
begin
  Result := LayoutLookAndFeel.GetGroupRestSpaceBounds(ViewInfo);
  case GroupViewInfo(ViewInfo).CaptionSide of
    sdLeft:
      begin
        Result.Right := Result.Left;
        Dec(Result.Left, LayoutLookAndFeel.GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdRight:
      begin
        Result.Left := Result.Right;
        Inc(Result.Right, LayoutLookAndFeel.GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdTop:
      begin
        Result.Bottom := Result.Top;
        Dec(Result.Top, LayoutLookAndFeel.GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdBottom:
      begin
        Result.Top := Result.Bottom;
        Inc(Result.Bottom, LayoutLookAndFeel.GroupOptions.CaptionOptions.SeparatorWidth);
      end;
  end;
end;

function TdxLayoutGroupWebPainter.GetCaptionSeparatorBounds: TRect;
begin
  Result := GetCaptionSeparatorAreaBounds;
  if not LayoutLookAndFeel.GroupOptions.OffsetCaption and not LayoutLookAndFeel.GroupOptions.OffsetItems and
    (LayoutLookAndFeel.GroupOptions.FrameWidth = 0) and (ViewInfo.CaptionViewInfo.Color = ViewInfo.Color) then
  begin
    Result.Left := ViewInfo.ClientBounds.Left;
    Result.Right := ViewInfo.ClientBounds.Right;
  end;
end;

function TdxLayoutGroupWebPainter.GetLayoutLookAndFeel: TdxLayoutWebLookAndFeel;
begin
  Result := TdxLayoutWebLookAndFeel(inherited LayoutLookAndFeel);
end;

procedure TdxLayoutGroupWebPainter.DoDrawBorders(ACanvas: TcxCanvas);
begin
  DrawFrame(ACanvas);
  DrawCaptionSeparator(ACanvas);
  inherited;
end;

procedure TdxLayoutGroupWebPainter.DoDrawCaptionSeparator(ACanvas: TcxCanvas);
begin
  ACanvas.Brush.Color := ViewInfo.Color;
  ACanvas.FillRect(GetCaptionSeparatorAreaBounds);
  ACanvas.Brush.Color := LayoutLookAndFeel.GroupOptions.GetFrameColor;
  ACanvas.FillRect(GetCaptionSeparatorBounds);
end;

function TdxLayoutGroupWebPainter.HasCaptionBackground: Boolean;
begin
  Result := False;
end;

function TdxLayoutGroupWebPainter.HasCaptionSeparator: Boolean;
var
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
begin
  AGroupViewInfo := GroupViewInfo(ViewInfo);
  Result := AGroupViewInfo.IsExpanded and
    LayoutLookAndFeel.GroupOptions.HasCaptionSeparator(AGroupViewInfo.HasCaption or AGroupViewInfo.HasButtons);
end;

function TdxLayoutGroupWebPainter.CanDrawCaptionSeparator: Boolean;
begin
  Result := HasCaptionSeparator;
end;

procedure TdxLayoutGroupWebPainter.DrawCaptionSeparator(ACanvas: TcxCanvas);
begin
  if CanDrawCaptionSeparator then
    DoDrawCaptionSeparator(ACanvas);
end;

procedure TdxLayoutGroupWebPainter.DrawFrame(ACanvas: TcxCanvas);
var
  R: TRect;
  I: Integer;
begin
  if not GroupViewInfo(ViewInfo).IsTransparent then
  begin
    ACanvas.Brush.Color := ViewInfo.CaptionViewInfo.Color;
    R := cxRectInflate(ViewInfo.Bounds, -LayoutLookAndFeel.GroupOptions.FrameWidth, -LayoutLookAndFeel.GroupOptions.FrameWidth);
    case GroupViewInfo(ViewInfo).CaptionSide of
      sdLeft:
        R.Right := R.Left + Max(ViewInfo.CaptionViewInfo.CalculateWidth, ViewInfo.ButtonsViewInfo.CalculateWidth);
      sdRight:
        R.Left := R.Right - Max(ViewInfo.CaptionViewInfo.CalculateWidth, ViewInfo.ButtonsViewInfo.CalculateWidth);
      sdTop:
        R.Bottom := R.Top + Max(ViewInfo.CaptionViewInfo.CalculateHeight, ViewInfo.ButtonsViewInfo.CalculateHeight);
      sdBottom:
        R.Top := R.Bottom - Max(ViewInfo.CaptionViewInfo.CalculateHeight, ViewInfo.ButtonsViewInfo.CalculateHeight);
    end;
    ACanvas.FillRect(R);
  end;
  R := ViewInfo.Bounds;
  for I := 1 to LayoutLookAndFeel.GroupOptions.FrameWidth do
  begin
    ACanvas.FrameRect(R, LayoutLookAndFeel.GroupOptions.GetFrameColor);
    InflateRect(R, -1, -1);
  end;
end;

function TdxLayoutItemCxLookAndFeelPainter.CanDrawBackground: Boolean;
begin
  Result := False;
end;

{ TdxLayoutGroupCaptionStandardPainter }

function TdxLayoutGroupCaptionStandardPainter.DrawEnabled: Boolean;
begin
  Result := inherited DrawEnabled or DrawVCLThemesEnabled;
end;

procedure TdxLayoutGroupCaptionStandardPainter.DoDrawText(ACanvas: TcxCanvas);

  procedure DrawThemeText(ADC: HDC; const ARect: TRect);
  const
    AElementMap: array[Boolean] of TThemedButton = (tbGroupBoxNormal, tbGroupBoxDisabled);
  var
    AFlags: {$IFDEF DELPHI16}TTextFormatFlags{$ELSE}Cardinal{$ENDIF};
    R: TRect;
  begin
    AFlags := {$IFDEF DELPHI16}TTextFormatFlags{$ENDIF}(cxFlagsToDTFlags(TdxLayoutGroupCaptionViewInfoAccess(ViewInfo).CalculateTextFlags));
    R := ARect;
    cxStyleServices.DrawText(ADC, cxStyleServices.GetElementDetails(AElementMap[ViewInfo.Enabled]),
      ViewInfo.Text, R, AFlags{$IFNDEF DELPHI16}, 0{$ENDIF});
  end;

var
  ABitmap: TcxAlphaBitmap;
  ATextRect: TRect;
begin
  if DrawVCLThemesEnabled then
  begin
    ATextRect := GetTextRect;
    if TdxLayoutGroupCaptionViewInfoAccess(ViewInfo).GetRotationAngle = ra0 then
      DrawThemeText(ACanvas.Handle, ATextRect)
    else
    begin
      ABitmap := TcxAlphaBitmap.CreateSize(cxRectRotate(ATextRect), True);
      try
        ABitmap.cxCanvas.FillRect(ABitmap.ClientRect,
          TdxCustomLayoutItemViewInfoAccess(TdxLayoutGroupCaptionViewInfoAccess(ViewInfo).ItemViewInfo).GetBackgroundColor);
        TdxLayoutGroupCaptionViewInfoAccess(ViewInfo).PrepareCanvas(ABitmap.cxCanvas);
        DrawThemeText(ABitmap.cxCanvas.Handle, ABitmap.ClientRect);
        ABitmap.Rotate(TdxLayoutGroupCaptionViewInfoAccess(ViewInfo).GetRotationAngle);
        cxDrawImage(ACanvas.Handle, ATextRect, ATextRect, ABitmap, nil, -1, idmNormal);
      finally
        ABitmap.Free;
      end;
    end;
  end
  else
    inherited;
end;

function TdxLayoutGroupCaptionStandardPainter.DrawVCLThemesEnabled: Boolean;
begin
  Result := (ViewInfo.TextColor = clDefault) and cxIsVCLThemesEnabled;
end;

{ TdxLayoutGroupCaptionCxLookAndFeelPainter }

function TdxLayoutGroupCaptionCxLookAndFeelPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TdxLayoutCxLookAndFeel(LayoutLookAndFeel).LookAndFeelPainter;
end;

{ TdxLayoutGroupCxLookAndFeelPainter }

procedure TdxLayoutGroupCxLookAndFeelPainter.DrawFrame(ACanvas: TcxCanvas);
var
  ACaptionPosition: TcxGroupBoxCaptionPosition;
  ACaptionRect, AFrameBounds: TRect;
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
  AHasCaption: Boolean;
begin
  AGroupViewInfo := GroupViewInfo(ViewInfo);
  AFrameBounds := LayoutLookAndFeel.GetGroupFrameBounds(ViewInfo);
  AHasCaption := AGroupViewInfo.HasCaption or AGroupViewInfo.HasButtons;
  ACaptionPosition := dxCaptionPositionMap[AHasCaption, AGroupViewInfo.CaptionSide];
  LookAndFeelPainter.DrawGroupBoxFrame(ACanvas, AFrameBounds, ViewInfo.Enabled, ACaptionPosition);

  if AGroupViewInfo.IsSkinPainterUsed and AHasCaption then
  begin
    ACaptionRect := ViewInfo.Bounds;
    case AGroupViewInfo.CaptionSide of
      sdLeft:
        ACaptionRect.Right := AFrameBounds.Left;
      sdRight:
        ACaptionRect.Left := AFrameBounds.Right;
      sdTop:
        ACaptionRect.Bottom := AFrameBounds.Top;
      sdBottom:
        ACaptionRect.Top := AFrameBounds.Bottom;
    end;
    LookAndFeelPainter.DrawGroupBoxCaption(ACanvas, ACaptionRect, cxNullRect, ACaptionPosition);

    if LookAndFeelPainter.IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition) then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.SetClipRegion(TcxRegion.Create(ViewInfo.CaptionViewInfo.Bounds), roSet);
        ACanvas.SetClipRegion(TcxRegion.Create(ViewInfo.ButtonsViewInfo.Bounds), roAdd);
        ACaptionRect := cxRectUnion(ViewInfo.CaptionViewInfo.Bounds, ViewInfo.ButtonsViewInfo.Bounds);
        LookAndFeelPainter.DrawGroupBoxCaption(ACanvas, ACaptionRect, ACaptionRect, ACaptionPosition);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  end;
end;

function TdxLayoutGroupCxLookAndFeelPainter.CanDrawBackground: Boolean;
var
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
begin
  AGroupViewInfo := GroupViewInfo(ViewInfo);
  Result := not AGroupViewInfo.IsSkinPainterUsed and not (AGroupViewInfo.IsSkinPainterUsed and AGroupViewInfo.Group.IsRoot) and inherited CanDrawBackground;
end;

function TdxLayoutGroupCxLookAndFeelPainter.GetCaptionPainterClass: TdxCustomLayoutItemCaptionPainterClass;
begin
  Result := TdxLayoutGroupCaptionCxLookAndFeelPainter;
end;

function TdxLayoutGroupCxLookAndFeelPainter.HasCaptionBackground: Boolean;
begin
  Result := not TdxLayoutGroupViewInfoAccess(ViewInfo).IsSkinPainterUsed;
end;

function TdxLayoutGroupCxLookAndFeelPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TdxLayoutCxLookAndFeel(LayoutLookAndFeel).LookAndFeelPainter;
end;

end.

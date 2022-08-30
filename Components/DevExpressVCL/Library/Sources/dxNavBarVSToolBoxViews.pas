{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
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

unit dxNavBarVSToolBoxViews;

{$I cxVer.inc}

interface

uses
  Windows, Graphics, Forms, Controls, dxNavBar, dxNavBarCollns, dxNavBarStyles,
  dxNavBarCustomPainters, dxNavBarBaseViews;

type
  TdxNavBarVSToolBoxLinkViewInfo = class(TdxNavBarLinkViewInfo)
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarVSToolBoxChildGroupViewInfo = class(TdxNavBarChildGroupViewInfo)
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarVSToolBoxViewInfo = class(TdxNavBarBaseViewInfo)
  protected
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetGroupCaptionSeparatorWidth: Integer; override;

    function CanHasScrollButtonInGroupCaption: Boolean; override;
    function CanLinksUseLargeImages: Boolean; override;
    function CanSelectLinkByRect: Boolean; override;

    function GetTopScrollButtonRect: TRect; override;
    function GetBottomScrollButtonRect: TRect; override;
  public
    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
  end;

  TdxNavBarVSToolBoxController = class(TdxNavBarNavigationBarController)
  protected
    procedure DoShowLinkHint(var AHintInfo: THintInfo; ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    function GetLinkHintRect(ALink: TdxNavBarItemLink): TRect; override;
    function GetLinkHintText(ALink: TdxNavBarItemLink): string; override;
  end;

  TdxNavBarVSToolBoxPainter = class(TdxNavBarFlatPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    function GetControllerClass: TdxNavBarControllerClass; override;

    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;
  public
    procedure DrawLinkHintWindow(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  TdxNavBarVSToolBoxSelectionPainter = class(TdxNavBarBaseSelectionPainter)
  protected
    class procedure GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor; out AFillColor,
      ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor); override;
  end;

  TdxNavBarVSToolBoxButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

implementation

uses
  Types, cxGraphics, cxGeometry,
  dxNavBarViewsFact, dxNavBarGraphics, dxNavBarConsts;

{ TdxNavBarVSToolBoxLinkViewInfo }

function TdxNavBarVSToolBoxLinkViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
  InflateRect(Result, -1, -1);
end;

{ TdxNavBarVSToolBoxChildGroupViewInfo }

function TdxNavBarVSToolBoxChildGroupViewInfo.SelectionRect: TRect;
begin
  Result := CaptionRect;
  InflateRect(Result, -1, -1);
end;

{ TdxNavBarVSToolBoxViewInfo }

procedure TdxNavBarVSToolBoxViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clBtnFace;
  NavBar.DefaultStyles.Background.BackColor2 := clBtnFace;
end;

procedure TdxNavBarVSToolBoxViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clBtnFace;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clBtnFace        ;
end;

procedure TdxNavBarVSToolBoxViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clBtnText;
end;

procedure TdxNavBarVSToolBoxViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clBtnText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarVSToolBoxViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := clBtnShadow;
end;

function TdxNavBarVSToolBoxViewInfo.CanHasScrollButtonInGroupCaption: Boolean;
begin
  Result := not NavBar.OptionsBehavior.Common.AllowChildGroups;
end;

function TdxNavBarVSToolBoxViewInfo.CanLinksUseLargeImages: Boolean;
begin
  Result := False;
end;

function TdxNavBarVSToolBoxViewInfo.CanSelectLinkByRect: Boolean;
begin
  Result := True;
end;

function TdxNavBarVSToolBoxViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarVSToolBoxViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(1);
end;

function TdxNavBarVSToolBoxViewInfo.GetGroupCaptionSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(2);
end;

function TdxNavBarVSToolBoxViewInfo.GetTopScrollButtonRect: TRect;
begin
  SetRectEmpty(Result);
  if GroupCount > 0 then
  begin
    Result := ActiveGroupViewInfo.CaptionRect;
    Result.Left := Result.Right - (ActiveGroupViewInfo.CaptionRect.Bottom - ActiveGroupViewInfo.CaptionRect.Top);
    OffsetRect(Result, (Result.Right - Result.Left) + 2, 0);
  end;
end;

function TdxNavBarVSToolBoxViewInfo.GetBottomScrollButtonRect: TRect;
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  SetRectEmpty(Result);
  if GroupCount > 0 then
  begin
    if IndexOfGroupViewInfo(ActiveGroupViewInfo) < GroupCount - 1 then
    begin
      AGroupViewInfo := Groups[IndexOfGroupViewInfo(ActiveGroupViewInfo) + 1];
      Result := AGroupViewInfo.CaptionRect;
      Result.Left := Result.Right - (AGroupViewInfo.CaptionRect.Bottom - AGroupViewInfo.CaptionRect.Top);
      OffsetRect(Result, (Result.Right - Result.Left) + 2, 0);
    end
    else
    begin
      Result := ActiveGroupViewInfo.CaptionRect;
      Result.Left := Result.Right - (ActiveGroupViewInfo.CaptionRect.Bottom - ActiveGroupViewInfo.CaptionRect.Top);
      OffsetRect(Result, (Result.Right - Result.Left) + 2,
        ActiveGroupViewInfo.ItemsRect.Bottom - ActiveGroupViewInfo.ItemsRect.Top);
    end;
  end;
end;

{ TdxNavBarVSToolBoxController }

procedure TdxNavBarVSToolBoxController.DoShowLinkHint(var AHintInfo: THintInfo; ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
  inherited;
  AHintInfo.HintPos := NavBar.ClientToScreen(NavBar.ClientRect.TopLeft);
end;

function TdxNavBarVSToolBoxController.GetLinkHintRect(ALink: TdxNavBarItemLink): TRect;
var
  R: TRect;
  ALinkViewInfo: TdxNavBarLinkViewInfo;
begin
  ALinkViewInfo := ViewInfo.GetLinkViewInfoByLink(ALink);
  if ALinkViewInfo <> nil then
  begin
    Result := ALinkViewInfo.SelectionRect;
    R := Result;
    cxScreenCanvas.Font := ViewInfo.HintFont;
    cxDrawText(cxScreenCanvas.Handle, ViewInfo.HintText, R, DT_CALCRECT or ALinkViewInfo.GetDrawEdgeFlag);
    if cxRectWidth(R) > cxRectWidth(ALinkViewInfo.CaptionRect) then
      Result.Right := ALinkViewInfo.CaptionRect.Left + cxRectWidth(R) + 4;
  end
  else
    Result := inherited GetLinkHintRect(ALink);
end;

function TdxNavBarVSToolBoxController.GetLinkHintText(ALink: TdxNavBarItemLink): string;
begin
  Result := inherited GetLinkHintText(ALink);
  if Result = '' then
    Result := ALink.Item.Caption;
end;

{ TdxNavBarVSToolBoxPainter }

procedure TdxNavBarVSToolBoxPainter.DrawLinkHintWindow(ACanvas: TCanvas; const ARect: TRect);
var
  R: TRect;
  AWidth: Integer;
  ALinkViewInfo: TdxNavBarLinkViewInfo;
begin
  ALinkViewInfo := ViewInfo.GetLinkViewInfoByLink(NavBar.HotTrackedLink);
  if ALinkViewInfo <> nil then
    with ALinkViewInfo do
    begin
      AWidth := cxRectWidth(ARect) - (SelectionRect.Right - SelectionRect.Left);
      R := cxRectOffset(SelectionRect, - SelectionRect.Left, - SelectionRect.Top);
      if AWidth > 0 then
        R.Right := R.Right + AWidth;
      with ALinkViewInfo.GroupViewInfo do
        BackgroundPainterClass.DrawBackground(ACanvas, R, BgImage, False, clBtnFace,
            BgBackColor, BgBackColor2, BgAlphaBlend, BgAlphaBlend2, BgGradientMode);
      SelectionPainterClass.DrawSelection(ACanvas, R, GroupViewInfo.BgBackColor, [sHotTracked]);
      R := cxRectOffset(ImageRect, - SelectionRect.Left, - SelectionRect.Top);
      ImagePainterClass.DrawImage(ACanvas, ImageList, ImageIndex, R);
      R := cxRectOffset(CaptionRect, - SelectionRect.Left, - SelectionRect.Top);
      AWidth := cxRectWidth(ARect);
      R.Right := R.Left + AWidth;
      ACanvas.Font := ALinkViewInfo.Font;
      cxDrawText(ACanvas, ViewInfo.HintText, R, GetDrawEdgeFlag);
    end;
end;

class function TdxNavBarVSToolBoxPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarVSToolBoxViewInfo;
end;

class function TdxNavBarVSToolBoxPainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarVSToolBoxLinkViewInfo;
end;

class function TdxNavBarVSToolBoxPainter.GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass;
begin
  Result := TdxNavBarVSToolBoxChildGroupViewInfo;
end;

function TdxNavBarVSToolBoxPainter.GetControllerClass: TdxNavBarControllerClass;
begin
  Result := TdxNavBarVSToolBoxController;
end;

class function TdxNavBarVSToolBoxPainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarVSToolBoxButtonPainter;
end;

class function TdxNavBarVSToolBoxPainter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarVSToolBoxSelectionPainter;
end;

{ TdxNavBarVSToolBoxSelectionPainter }

class procedure TdxNavBarVSToolBoxSelectionPainter.GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor;
  out AFillColor, ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor);
begin
  inherited;
  if IsPressed(AState) then
  begin
    ATopLeftOuterColor := DarkBorderColor(ABackColor);
    ABottomRightOuterColor := LightBorderColor(ABackColor);
  end
  else
    ATopLeftInnerColor := LightBorderColor(ABackColor);
end;

{ TdxNavBarVSToolBoxButtonPainter }

class procedure TdxNavBarVSToolBoxButtonPainter.InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  inherited;
  if sPressed in AState then
    dxNavBarDrawSelectedFrame(ACanvas, ARect, DarkDarkBorderColor(AColor1), LightLightBorderColor(AColor1),
      DarkBorderColor(AColor1), clNone)
  else
    dxNavBarDrawSelectedFrame(ACanvas, ARect, LightLightBorderColor(AColor1), DarkDarkBorderColor(AColor1),
      LightBorderColor(AColor1), clNone);
end;

initialization
  RegisterView(dxNavBarVSToolBoxView, 'VSToolBoxView', TdxNavBarVSToolBoxPainter);

finalization
  UnRegisterView(dxNavBarVSToolBoxView);

end.

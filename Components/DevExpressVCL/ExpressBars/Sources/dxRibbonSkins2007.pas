{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonSkins2007;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Graphics, Forms, dxCore, dxGDIPlusAPI, dxGDIPlusClasses, cxGraphics, cxGeometry,
  dxBar, dxBarSkin, dxBarSkinConsts, cxLookAndFeelPainters, dxRibbonSkins, cxControls;

type

  { TdxCustomRibbon2007Skin }

  TdxCustomRibbon2007Skin = class(TdxCustomRibbonSkin)
  protected
    FApplicationButton: array[TdxRibbonColorSchemeAccent] of TThreeStateArray;
    FBorderIconGlyph: array[TdxRibbonBorderDrawIcon] of TFourStateArray;
    FBorderIcons: TThreeStateArray;
    FBottomBorderThick: array[Boolean] of TTwoStateArray;
    FBottomBorderThin: TTwoStateArray;
    FButtonGroupBorderLeft: Integer;
    FButtonGroupBorderMiddle: TStatesArray;
    FButtonGroupBorderRight: Integer;
    FButtonGroupSplitButtonSeparator: TStatesArray;
    FCaption: TTwoStateArray;
    FCaptionLeftBorder: TTwoStateArray;
    FCaptionRightBorder: TTwoStateArray;
    FCaptionZoomed: TTwoStateArray;
    FCollapsedToolbarGlyphBackgrounds: TStatesArray;
    FCollapsedToolbars: TStatesArray;
    FContextBackground: Integer;
    FContextBackgroundGlass: Integer;
    FContextTabGroupsArea: TTwoStateArray;
    FContextTabIndex: array[TdxRibbonTabState] of Integer;
    FContextTabSeparator: TTwoStateArray;
    FDropDownGalleryBottomSizingBand: Integer;
    FDropDownGalleryTopSizingBand: Integer;
    FFormStatusBarLeftParts: array[Boolean] of TFourStateArray;
    FFormStatusBarRightParts: array[Boolean] of TFourStateArray;
    FGalleryFilterBand: Integer;
    FGalleryGroupHeader: Integer;
    FGroupScrollButtons: array[Boolean] of TThreeStateArray;
    FInRibbonGalleryScrollBarDropDownButton: TStatesArray;
    FInRibbonGalleryScrollBarLineDownButton: TStatesArray;
    FInRibbonGalleryScrollBarLineUpButton: TStatesArray;
    FLargeButtonDropButtons: TStatesArray;
    FLargeButtonGlyphBackgrounds: TStatesArray;
    FLargeButtons: TStatesArray;
    FLaunchButtonBackgrounds: TStatesArray;
    FLeftBorder: TTwoStateArray;
    FMarkArrow: TStatesArray;
    FMenuArrowDown: Integer;
    FMenuArrowRight: Integer;
    FMenuCheck: TStatesArray;
    FMenuCheckMark: TStatesArray;
    FMenuDetachCaption: TStatesArray;
    FMenuScrollArea: TStatesArray;
    FProgressDiscreteBand: Integer;
    FProgressSolidBand: Integer;
    FQATDefaultGlyph: Integer;
    FQATPopup: Integer;
    FRibbonTopArea: Integer;
    FRightBorder: TTwoStateArray;
    FSmallButtonDropButtons: TStatesArray;
    FSmallButtonGlyphBackgrounds: TStatesArray;
    FSmallButtons: TStatesArray;
    FScrollBarBackground: array [Boolean] of TFourStateArray;
    FScrollBarButtonRightBottom: array [Boolean] of TFourStateArray;
    FScrollBarButtonRightBottomGlyph: array [Boolean] of Integer;
    FScrollBarButtonLeftTop: array [Boolean] of TFourStateArray;
    FScrollBarButtonLeftTopGlyph: array [Boolean] of Integer;
    FScrollBarThumb: array [Boolean] of TFourStateArray;
    FScrollBarThumbGlyph: array [Boolean] of Integer;
    FStatusBar: Integer;
    FStatusBarGripBackground: Integer;
    FStatusBarPanel: Integer;
    FStatusBarPanelLowered: Integer;
    FStatusBarPanelRaised: Integer;
    FStatusBarPanelSeparator: Integer;
    FStatusBarToolbarSeparator: Integer;
    FTabGroupsArea: TThreeStateArray;
    FTabIndex: array[TdxRibbonTabState] of Integer;
    FTabScrollButtons: array[Boolean] of TThreeStateArray;
    FToolbar: TStatesArray;
    FToolbarHeader: TStatesArray;

    function GetTexturesDPI: Integer; override;
    procedure InternalDrawFormBorderIconGlyph(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState;
      AColor: TColor = clDefault; AAlignment: TAlignment = taCenter);

    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor; override;
    // Common
    procedure LoadCommonButtons(ABitmap: GpBitmap);
    procedure LoadCommonElements(ABitmap: GpBitmap); override;
    procedure LoadCommonMenu(ABitmap: GpBitmap); override;
    // Ribbon
    procedure LoadRibbonApplicationButton(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonButtons(ABitmap: GpBitmap); override;
    procedure LoadRibbonCollapsedToolbar(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonContexts(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonElements(ABitmap: GpBitmap); override;
    procedure LoadRibbonForm(ABitmap: GpBitmap); override;
    procedure LoadRibbonMinimizeButton(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonFormBorderIcons(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonFormBorderIconsGlyphs(ABitmap: GpBitmap; X, Y, AWidth, AHeight: Integer); virtual;
    procedure LoadRibbonFormBorders(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonGallery(ABitmap: GpBitmap); override;
    procedure LoadRibbonGalleryInRibbonScrollBarButtons(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonGroup(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonMarkArrow(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonMenu(ABitmap: GpBitmap); override;
    procedure LoadRibbonMenuMarks(ABitmap: GpBitmap); override;
    procedure LoadRibbonQAT(ABitmap: GpBitmap); override;
    procedure LoadRibbonScrollBars(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonScrollBarsGlyphs(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonScrollButtons(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonStatusBar(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonTab(ABitmap: GpBitmap); override;
  public
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawArrowDown(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBorderLeft(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBorderRight(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;
    procedure DrawContextTabGroupsArea(DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean); override;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawFormBorders(DC: HDC; const ABordersWidth: TRect); override;
    procedure DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
      AState: TdxRibbonBorderIconState); override;
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    procedure DrawFormStatusBarPart(DC: HDC; const R: TRect; AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean); override;
    procedure DrawGalleryFilterBandBackground(DC: HDC; const R: TRect); override;
    procedure DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect); override;
    procedure DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer); override;
    procedure DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;

    // InRibbon Gallery
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); override;

    procedure DrawLargeButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); override;
    procedure DrawMDIButtonGlyph(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); override;
    procedure DrawMenuArrowDown(DC: HDC; const R: TRect); override;
    procedure DrawMenuArrowRight(DC: HDC; const R: TRect); override;
    procedure DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuDetachCaption(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawProgressDiscreteBand(DC: HDC; const R: TRect); override;
    procedure DrawProgressSolidBand(DC: HDC; const R: TRect); override;
    procedure DrawProgressSubstrate(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    procedure DrawRibbonClientTopArea(DC: HDC; const R: TRect); override;
    procedure DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawScrollBarPart(DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean); override;
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawStatusBar(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarGripBackground(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean); override;
    procedure DrawStatusBarPanelSeparator(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect); override;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    function GetPartSize(APart: Integer): Integer; override;
    function GetRibbonTopFrameAreaSeparatorSize: Integer; override;
  end;

  { TdxBlueRibbonSkin }

  TdxBlueRibbonSkin = class(TdxCustomRibbon2007Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetName: string; override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
  end;

  { TdxBlackRibbonSkin }

  TdxBlackRibbonSkin = class(TdxCustomRibbon2007Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetName: string; override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
  end;

  { TdxSilverRibbonSkin }

  TdxSilverRibbonSkin = class(TdxBlackRibbonSkin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetName: string; override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
  end;

implementation

uses
  Math, dxScreenTip;

{$R dxRibbonSkins2007.res}

{ TdxCustomRibbon2007Skin }

procedure TdxCustomRibbon2007Skin.DrawApplicationButton(DC: HDC; const R: TRect;
  AState: TdxRibbonApplicationButtonState);
begin
  if LowColors then
    inherited DrawApplicationButton(DC, R, AState)
  else
    Parts[FApplicationButton[ColorSchemeAccent][Ord(AState)]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2007Skin.DrawArrowDown(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawArrowDown(DC, R, AState)
  else
    DrawArrow(DC, cxRectOffset(R, 1, 0), adDown, GetPartColor(DXBAR_ARROWDOWN, AState));
end;

procedure TdxCustomRibbon2007Skin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
var
  R1: TRect;
  AColor: TColor;
begin
  R1 := cxRectInflate(R, -4, 0);
  if LowColors then
    AColor := clBtnShadow
  else
    AColor := $E0E0E0;
  FillRectByColor(DC, cxRectSetHeight(R1, 1), AColor);
  FillRectByColor(DC, cxRectSetBottom(R1, R1.Bottom, 1), clBtnHighlight);
end;

procedure TdxCustomRibbon2007Skin.DrawButtonGroupBorderLeft(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawButtonGroupBorderLeft(DC, R)
  else
    Parts[FButtonGroupBorderLeft].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawButtonGroupBorderMiddle(DC, R, AState)
  else
    DrawPart(FButtonGroupBorderMiddle, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawButtonGroupBorderRight(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawButtonGroupBorderRight(DC, R)
  else
    Parts[FButtonGroupBorderRight].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawButtonGroupSplitButtonSeparator(
  DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawButtonGroupSplitButtonSeparator(DC, R, AState)
  else
    DrawPart(FButtonGroupSplitButtonSeparator, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarBackground(DC, R, AState)
  else
    case AState of
      0, 2, 3, 4:
        Parts[FCollapsedToolbars[AState]].Draw(DC, R);
      else
        Parts[FCollapsedToolbars[0]].Draw(DC, R);
    end;
end;

procedure TdxCustomRibbon2007Skin.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FCollapsedToolbarGlyphBackgrounds, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
begin
  if LowColors then
    inherited DrawContextBackground(DC, R, AContextColor)
  else
  begin
    dxGpFillRect(DC, R, AContextColor);
    Parts[FContextBackground].Draw(DC, R);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor);
begin
  if LowColors then
    inherited DrawContextBackgroundGlass(DC, R, AContextColor)
  else
  begin
    dxGpFillRectByGradient(DC, R, 0, AContextColor, LinearGradientModeVertical, 0, 220);
    Parts[FContextBackgroundGlass].Draw(DC, R);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState;
  AContextColor: TColor);
begin
  if LowColors then
    inherited DrawContextTabBackground(DC, R, AState, AContextColor)
  else
  begin
    dxGpFillRect(DC, R, AContextColor);
    Parts[FContextTabIndex[AState]].Draw(DC, R);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawContextTabGroupsArea(
  DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if LowColors then
    inherited DrawContextTabGroupsArea(DC, R, AContextColor, AIsQATAtBottom, AIsInPopup)
  else
    Parts[FContextTabGroupsArea[AIsQATAtBottom]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawContextTabSeparator(
  DC: HDC; const R: TRect; ABeginGroup: Boolean);
begin
  if LowColors then
    inherited DrawContextTabSeparator(DC, R, ABeginGroup)
  else
    Parts[FContextTabSeparator[ABeginGroup]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBottomSizingBand(DC, R)
  else
    Parts[FDropDownGalleryBottomSizingBand].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryTopSizingBand(DC, R)
  else
    Parts[FDropDownGalleryTopSizingBand].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawFormBorders(DC: HDC; const ABordersWidth: TRect);
var
  AFormBounds: TRect;
  AIsFormActive: Boolean;
  ARectangularBottom: Boolean;
  R: TRect;
begin
  if LowColors then
    inherited DrawFormBorders(DC, ABordersWidth)
  else
  begin
    AFormBounds := FormPaintData.GetBounds;
    AIsFormActive := FormPaintData.GetIsActive;
    if PaintData.GetCaptionHeight + PaintData.GetCaptionAreaExtension > 0 then
    begin
      R := AFormBounds;
      R.Bottom := PaintData.GetCaptionHeight + PaintData.GetCaptionAreaExtension;
      R.Right := R.Left + ABordersWidth.Left;
      if UseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, AFormBounds);
      Parts[FCaptionLeftBorder[not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment);
      R.Right := AFormBounds.Right;
      R.Left := R.Right - ABordersWidth.Right;
      if UseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, AFormBounds);
      Parts[FCaptionRightBorder[not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment)
    end;
    ARectangularBottom := IsRectangularFormBottom(FormPaintData);
    if ABordersWidth.Bottom > 1 then
    begin
      R := AFormBounds;
      R.Top := R.Bottom - ABordersWidth.Bottom;
      Parts[FBottomBorderThick[ARectangularBottom][not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment);
      cxExcludeClipRect(DC, R);
    end
    else
    begin
      R := AFormBounds;
      R.Top := R.Bottom - ABordersWidth.Bottom;
      Inc(R.Left, ABordersWidth.Left);
      Dec(R.Right, ABordersWidth.Right);
      if UseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, AFormBounds);
      Parts[FBottomBorderThin[not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment);
    end;
    R := AFormBounds;
    if not ARectangularBottom then
      Dec(R.Bottom);
    R.Top := PaintData.GetCaptionHeight + PaintData.GetCaptionAreaExtension + ABordersWidth.Top;
    R.Right := R.Left + ABordersWidth.Left;
    if UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, AFormBounds);
    Parts[FLeftBorder[not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment);
    R.Right := AFormBounds.Right;
    R.Left := R.Right - ABordersWidth.Right;
    if UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, AFormBounds);
    Parts[FRightBorder[not AIsFormActive]].Draw(DC, R,  255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
  AState: TdxRibbonBorderIconState);
var
  APart: Integer;
begin
  if LowColors then
    inherited DrawFormBorderIcon(DC, R, AIcon, AState)
  else
  begin
    APart := BorderIconStateToPartIndex[AState];
    if APart >= 0 then
      Parts[FBorderIcons[APart]].Draw(DC, R);
    InternalDrawFormBorderIconGlyph(DC, R, AIcon, AState);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawFormCaption(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if LowColors then
    inherited DrawFormCaption(DC, R)
  else
    if FormPaintData.GetState = wsMaximized then
      Parts[FCaptionZoomed[not FormPaintData.GetIsActive]].Draw(DC, R)
    else
      if FormPaintData.GetState = wsMinimized then
      begin
        ARect := R;
        Dec(ARect.Bottom, 1);
        Parts[FCaption[not FormPaintData.GetIsActive]].Draw(DC, ARect);
        ARect := R;
        ARect.Top := ARect.Bottom - 1;
        Parts[FBottomBorderThin[not FormPaintData.GetIsActive]].Draw(DC, ARect);
      end
      else
        Parts[FCaption[not FormPaintData.GetIsActive]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawFormStatusBarPart(DC: HDC; const R: TRect;
  AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean);
var
  APart: Integer;
begin
  if LowColors then
    inherited DrawFormStatusBarPart(DC, R, AIsLeft, AIsActive, AIsRaised, AIsRectangular)
  else
  begin
    APart := 0;
    Inc(APart, Ord(AIsRaised));
    Inc(APart, Ord(not AIsActive) * 2);
    if AIsLeft then
      Parts[FFormStatusBarLeftParts[AIsRectangular][APart]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment)
    else
      Parts[FFormStatusBarRightParts[AIsRectangular][APart]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawGalleryFilterBandBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawGalleryFilterBandBackground(DC, R)
  else
    Parts[FGalleryFilterBand].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawGalleryGroupHeaderBackground(DC, R)
  else
    Parts[FGalleryGroupHeader].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer);

  function GetOuterBorderColor: TColor;
  begin
    case AState of
      DXBAR_CHECKED:
        Result := $1048EF;
      DXBAR_HOTCHECK:
        Result := $3695F2;
    else
      Result := $3694F2;
    end;
  end;

  function GetInnerBorderColor: TColor;
  begin
    if AState = DXBAR_HOTCHECK then
      Result := $95E3FF
    else
      Result := $94E2FF;
  end;

begin
  FrameRectByColor(DC, ARect, GetOuterBorderColor);
  FrameRectByColor(DC, cxRectInflate(ARect, -1), GetInnerBorderColor);
end;

procedure TdxCustomRibbon2007Skin.DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
var
  APartIndex: Integer;
begin
  if LowColors then
    inherited DrawGroupScrollButton(DC, R, ALeft, AState)
  else
  begin
    case AState of
      DXBAR_HOT:
        APartIndex := 1;
      DXBAR_PRESSED:
        APartIndex := 2;
      else
        APartIndex := 0;
    end;
    Parts[FGroupScrollButtons[ALeft][APartIndex]].Draw(DC, R);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
  AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);
begin
  if LowColors then
    inherited DrawInRibbonGalleryScrollBarButton(DC, R, AButtonKind, AState)
  else
  begin
    case AButtonKind of
      gsbkLineUp:
        DrawPart(FInRibbonGalleryScrollBarLineUpButton, DC, R, AState);
      gsbkLineDown:
        DrawPart(FInRibbonGalleryScrollBarLineDownButton, DC, R, AState);
      gsbkDropDown:
        DrawPart(FInRibbonGalleryScrollBarDropDownButton, DC, R, AState);
    end;
    DrawInRibbonGalleryScrollBarButtonGlyph(DC, R, AButtonKind, AState);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawLargeButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButton(DC, R, AState)
  else
    DrawPart(FLargeButtons, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButtonDropButtonArrowPart(DC, R, AState)
  else
    DrawPart(FLargeButtonDropButtons, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButtonDropButtonMainPart(DC, R, AState)
  else
    DrawPart(FLargeButtonGlyphBackgrounds, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    case AState of
      DXBAR_HOT:
        DrawFrame(DC, R, clHighlight, clBtnShadow);
      DXBAR_PRESSED:
        DrawFrame(DC, R, clHighlight, clHighlightText);
      DXBAR_DISABLED:
        DrawFrame(DC, R, clBtnFace, clGrayText)
      else
        DrawFrame(DC, R, clBtnFace, clBtnShadow);
    end
  else
    DrawPart(FLaunchButtonBackgrounds, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer);
var
  AGlyphColor: TColor;
begin
  case AState of
    DXBAR_NORMAL:
      AGlyphColor := clBtnText;
    DXBAR_DISABLED:
      AGlyphColor := clGrayText
    else
      AGlyphColor := clHighlightText;
  end;
  if LowColors then
    Parts[FLaunchButtonDefaultGlyphs[AState]].DrawColored(DC, R, AGlyphColor, 255, UseRightToLeftAlignment)
  else
    DrawPart(FLaunchButtonDefaultGlyphs, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer);
begin
  InternalDrawGlyph(DC, R, FMarkArrow[AState], IfThen(LowColors, clBtnFace, clDefault));
end;

procedure TdxCustomRibbon2007Skin.DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
begin
  DrawTabAreaButton(DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawMDIButtonGlyph(
  DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
const
  PartMap: array[TcxButtonState] of Integer = (0, 0, 1, 2, 3);
var
  AIcon: TdxRibbonBorderDrawIcon;
begin
  if LowColors then
    inherited DrawMDIButtonGlyph(DC, R, AButton, AState)
  else
  begin
    case AButton of
      mdibMinimize:
        AIcon := rbdiMinimize;
      mdibRestore:
        AIcon := rbdiRestore;
    else
      AIcon := rbdiClose;
    end;
    InternalDrawGlyph(DC, R, FBorderIconGlyph[AIcon][PartMap[AState]]);
  end;
end;

procedure TdxCustomRibbon2007Skin.DrawMenuArrowDown(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuArrowDown(DC, R)
  else
    InternalDrawGlyph(DC, R, FMenuArrowDown, clDefault, False);
end;

procedure TdxCustomRibbon2007Skin.DrawMenuArrowRight(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuArrowRight(DC, R)
  else
    InternalDrawGlyph(DC, R, FMenuArrowRight, clDefault, False);
end;

procedure TdxCustomRibbon2007Skin.DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FMenuCheck, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawMenuCheck(DC, R, AState);
  InternalDrawGlyph(DC, R, FMenuCheckMark[AState], clDefault, False);
end;

procedure TdxCustomRibbon2007Skin.DrawMenuDetachCaption(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FMenuDetachCaption, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FMenuScrollArea, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawProgressDiscreteBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawProgressDiscreteBand(DC, R)
  else
    Parts[FProgressDiscreteBand].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawProgressSolidBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawProgressSolidBand(DC, R)
  else
    Parts[FProgressSolidBand].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawProgressSubstrate(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawProgressSubstrate(DC, R)
  else
    Parts[FProgressSubstrate].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarDefaultGlyph(DC, R)
  else
    Parts[FQATDefaultGlyph].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarPopup(DC, R)
  else
    Parts[FQATPopup].Draw(DC, R)
end;

procedure TdxCustomRibbon2007Skin.DrawRibbonClientTopArea(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawRibbonClientTopArea(DC, R)
  else
    Parts[FRibbonTopArea].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawScrollBarBackground(DC, R, AHorizontal)
  else
    Parts[FScrollBarBackground[AHorizontal][0]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawScrollBarPart(DC: HDC; const R: TRect; APart: TcxScrollBarPart;
  AState: Integer; AHorizontal: Boolean);

  procedure DrawBackgroundPart(const AParts: TFourStateArray);
  var
    APartIndex: Integer;
  begin
    case AState of
      DXBAR_HOT:
        APartIndex := 1;
      DXBAR_PRESSED:
        APartIndex := 2;
      DXBAR_DISABLED:
        APartIndex := 3;
    else
      APartIndex := 0;
    end;
    Parts[AParts[APartIndex]].Draw(DC, R);
  end;

begin
  if LowColors then
    inherited DrawScrollBarPart(DC, R, APart, AState, AHorizontal)
  else
    case APart of
      sbpPageUp, sbpPageDown:
        DrawBackgroundPart(FScrollBarBackground[AHorizontal]);
      sbpThumbnail:
        begin
          DrawBackgroundPart(FScrollBarThumb[AHorizontal]);
          InternalDrawGlyph(DC, R, FScrollBarThumbGlyph[AHorizontal], clDefault, False);
        end;
      sbpLineUp:
        begin
          DrawBackgroundPart(FScrollBarButtonLeftTop[AHorizontal]);
          InternalDrawGlyph(DC, R, FScrollBarButtonLeftTopGlyph[AHorizontal], clDefault, False);
        end;
      sbpLineDown:
        begin
          DrawBackgroundPart(FScrollBarButtonRightBottom[AHorizontal]);
          InternalDrawGlyph(DC, R, FScrollBarButtonRightBottomGlyph[AHorizontal], clDefault, False);
        end;
    end;
end;

procedure TdxCustomRibbon2007Skin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FSmallButtons, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FSmallButtonDropButtons, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FSmallButtonGlyphBackgrounds, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawStatusBar(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBar(DC, R)
  else
    Parts[FStatusBar].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2007Skin.DrawStatusBarGripBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBarGripBackground(DC, R)
  else
    Parts[FStatusBarGripBackground].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2007Skin.DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean);
begin
  if LowColors then
    inherited DrawStatusBarPanel(DC, Bounds, R, AIsLowered)
  else
    if AIsLowered then
      Parts[FStatusBarPanelLowered].Draw(DC, R)
    else
      Parts[FStatusBarPanelRaised].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawStatusBarPanelSeparator(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBarPanelSeparator(DC, R)
  else
    Parts[FStatusBarPanelSeparator].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBarToolbarSeparator(DC, R)
  else
    Parts[FStatusBarToolbarSeparator].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  if LowColors then
    inherited DrawTab(DC, R, AState)
  else
    Parts[FTabIndex[AState]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawTabGroupBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
begin
  if LowColors then
    inherited DrawTabGroupBackground(DC, R, AState, AIsInPopup)
  else
    DrawPart(FToolbar, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawTabGroupHeaderBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
begin
  if LowColors then
    inherited DrawTabGroupHeaderBackground(DC, R, AState, AIsInPopup)
  else
    DrawPart(FToolbarHeader, DC, R, AState);
end;

procedure TdxCustomRibbon2007Skin.DrawTabGroupsArea(
  DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean);

  function GetPartIndex: Integer;
  begin
    if AIsInPopup then
      Result := 2
    else
      Result := Ord(AIsQATAtBottom);
  end;

begin
  if LowColors then
    inherited DrawTabGroupsArea(DC, R, AIsQATAtBottom, AIsInPopup)
  else
    Parts[FTabGroupsArea[GetPartIndex]].Draw(DC, R);
end;

procedure TdxCustomRibbon2007Skin.DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
var
  APartIndex: Integer;
begin
  if LowColors then
    inherited DrawTabScrollButton(DC, R, ALeft, AState)
  else
  begin
    case AState of
      DXBAR_HOT:
        APartIndex := 1;
      DXBAR_PRESSED:
        APartIndex := 2;
      else
        APartIndex := 0;
    end;
    Parts[FTabScrollButtons[ALeft][APartIndex]].Draw(DC, R);
  end;
end;

function TdxCustomRibbon2007Skin.GetPartSize(APart: Integer): Integer;

  function EncodeSize(const S: TSize): Integer;
  begin
    Result := MakeLong(S.cx, S.cy);
  end;

begin
  case APart of
    DXBAR_MENUBACKBUTTON, DXBAR_MENUARROWRIGHT:
      Result := EncodeSize(ScaleFactorForTextures.Apply(Parts[FMenuArrowRight].Size));
    DXBAR_MENUARROWDOWN:
      Result := EncodeSize(ScaleFactorForTextures.Apply(Parts[FMenuArrowDown].Size));
  else
    Result := inherited GetPartSize(APart);
  end;
end;

function TdxCustomRibbon2007Skin.GetRibbonTopFrameAreaSeparatorSize: Integer;
begin
  Result := 2;
end;

function TdxCustomRibbon2007Skin.GetTexturesDPI: Integer;
begin
  Result := Min(TargetDPI, 192);
end;

procedure TdxCustomRibbon2007Skin.InternalDrawFormBorderIconGlyph(DC: HDC;
  const  R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState;
  AColor: TColor = clDefault; AAlignment: TAlignment = taCenter);
const
  StateMap: array[TdxRibbonBorderIconState] of Integer = (0, 1, 2, 3, 3);
begin
  if LowColors then
  begin
    if IsHighContrastWhite and (AState in [rbisNormal, rbisInactive]) then
      AColor := clBlack
    else
      AColor := clWhite;
  end;
  InternalDrawGlyph(DC, R, FBorderIconGlyph[AIcon][StateMap[AState]], AColor, True, AAlignment);
end;

function TdxCustomRibbon2007Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_KEYTIP_TEXTCOLOR:
      Result := dxScreenTipFontColor;
    DXBAR_MINITOOLBAR_BACKGROUND:
      Result := $F5F5F5;
    rspContextTextOnGlass:
      Result := clBlack;
    DXBAR_BACKSTAGEVIEW:
      Result := clWhite;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR:
      Result := $646464;
    DXBAR_MENUEXTRAPANE:
      Result := $EEEAE9;
    DXBAR_MENUARROWSEPARATOR:
      Result := $BDB6A5;
    DXBAR_MENUDETACHCAPTIONAREA:
      Result := $F7F7F7;
    DXBAR_MENUBUTTONITEMTEXT:
      Result := GetPartColor(DXBAR_MENUITEMTEXT, AState);
    DXBAR_BUTTONITEMTEXT:
      Result := GetPartColor(DXBAR_ITEMTEXT, AState);
    DXBAR_DROPDOWNGALLERY:
      Result := $FAFAFA;
    DXBAR_DROPDOWNBORDER_INNERLINE:
      Result := $F5F5F5;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $A7A7A7
      else
        Result := inherited;

    DXBAR_ITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $8D8D8D
      else
        Result := GetPartColor(rspTabGroupText);

    DXBAR_ARROWDOWN:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $CBCED1
      else
        Result := $696D72;
  else
    Result := inherited;
  end;
end;

function TdxCustomRibbon2007Skin.DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_KEYTIP_TEXTCOLOR:
      Result := dxScreenTipFontColor;
    DXBAR_MINITOOLBAR_BACKGROUND:
      Result := $F5F5F5;
    rspContextTextOnGlass:
      Result := clBlack;
    DXBAR_BACKSTAGEVIEW:
      Result := clWindow;
    DXBAR_MENUEXTRAPANE:
      Result := clWindow;
    DXBAR_MENUARROWSEPARATOR:
      Result := clBtnShadow;
    DXBAR_MENUDETACHCAPTIONAREA:
      Result := clHighlightText;
    DXBAR_DROPDOWNGALLERY:
      Result := clWindow;
    DXBAR_DROPDOWNBORDER_INNERLINE:
      Result := clHighlightText;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := clGrayText
      else
        Result := GetPartColor(rspTabGroupText);

    DXBAR_MENUBUTTONITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := clGrayText
      else
        Result := inherited DoGetPartColor(DXBAR_MENUITEMTEXT, AState);

    DXBAR_ITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := clGrayText
      else
        Result := GetPartColor(rspTabGroupText);

    DXBAR_BUTTONITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := clGrayText
      else
        Result := inherited DoGetPartColor(DXBAR_ITEMTEXT, AState);
  else
    Result := inherited;
  end;
end;

procedure TdxCustomRibbon2007Skin.LoadCommonButtons(ABitmap: GpBitmap);
const
  SmallButtonSize  = 22;
  SmallButtonGlyphBackgroundWidth = 29;
  SmallButtonDropButtonWidth = 12;

  LargeButtonWidth  = 42;
  LargeButtonHeight = 66;
  LargeButtonGlyphBackgroundWidth = 42;
  LargeButtonGlyphBackgroundHeight = 39;
  LargeButtonDropButtonWidth = 42;
  LargeButtonDropButtonHeight = 27;

  LaunchButtonWidth = 15;
  LaunchButtonHeight = 14;
begin
  LoadElementParts(ABitmap, FSmallButtons,
    cxRectBounds(99, 155, SmallButtonSize, SmallButtonSize),
    rspSmallButtonNormal, DefaultFixedSize, [0, 0, 1, 2, 2, 3, 4], DXBAR_BTN_STATES);
  LoadElementParts(ABitmap, FSmallButtonGlyphBackgrounds,
    cxRectBounds(86, 0, SmallButtonGlyphBackgroundWidth, SmallButtonSize),
    rspSmallButtonGlyphBackgroundNormal, DefaultFixedSize, [], DXBAR_BTN_STATES);
  LoadElementParts(ABitmap, FSmallButtonDropButtons,
    cxRectBounds(86, 155, SmallButtonDropButtonWidth, SmallButtonSize),
    rspSmallButtonDropButtonNormal, DefaultFixedSize, [], DXBAR_BTN_STATES);

  LoadElementParts(ABitmap, FLargeButtons,
    cxRectBounds(0, 0, LargeButtonWidth, LargeButtonHeight),
    rspLargeButtonNormal, DefaultFixedSize, [0, 0, 1, 2, 2, 3, 4], DXBAR_BTN_STATES);
  LoadElementParts(ABitmap, FLargeButtonGlyphBackgrounds,
    cxRectBounds(43, 0, LargeButtonGlyphBackgroundWidth, LargeButtonGlyphBackgroundHeight),
    rspLargeButtonGlyphBackgroundNormal, DefaultFixedSize, [0, 1, 2, 1, 3, 4], DXBAR_BTN_STATES);
  LoadElementParts(ABitmap, FLargeButtonDropButtons,
    cxRectBounds(43, 235, LargeButtonDropButtonWidth, LargeButtonDropButtonHeight),
    rspLargeButtonDropButtonNormal, DefaultFixedSize, [0, 1, 1, 2, 2, 0, 3], DXBAR_BTN_STATES);

  LoadElementParts(ABitmap, FLaunchButtonBackgrounds,
    cxRectBounds(57, 353, LaunchButtonWidth, LaunchButtonHeight),
    rspLaunchButtonBackgroundNormal, DefaultFixedSize, [0, 0, 1],
    [DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED]);
end;

procedure TdxCustomRibbon2007Skin.LoadCommonElements(ABitmap: GpBitmap);
begin
  inherited LoadCommonElements(ABitmap);
  FProgressSolidBand := AddPart3x3(ABitmap, cxRectBounds(6, 344, 86, 8), DefaultFixedSize, rspProgressSolidBand);
  FProgressDiscreteBand := AddPart3x3(ABitmap, cxRectBounds(0, 344, 5, 8), DefaultFixedSize, rspProgressDiscreteBand);
  FContextBackgroundGlass := AddPart3x3(ABitmap,  cxRectBounds(39, 353, 17, 25), cxRect(2, 9, 2, 1), rspContextBackgroundGlass);
  LoadCommonButtons(ABitmap);
end;

procedure TdxCustomRibbon2007Skin.LoadCommonMenu(ABitmap: GpBitmap);

  procedure LoadMenuCheckMark(const R: TRect);
  begin
    LoadElementParts(ABitmap, FMenuCheckMark, R, rspMenuCheckMarkNormal,
      DefaultFixedSize, [0, 1, 0], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_DISABLED]);
  end;

begin
  inherited LoadCommonMenu(ABitmap);
  LoadElementParts(ABitmap, FMenuDetachCaption, cxRectBounds(1, 331, 5, 5),
    rspMenuDetachCaptionNormal, DefaultFixedSize, [], [DXBAR_NORMAL, DXBAR_HOT]);
  LoadElementParts(ABitmap, FMenuCheck, cxRectBounds(99, 310, 6, 6),
    rspMenuCheckNormal, DefaultFixedSize, [0, 1, 0], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_DISABLED]);
  LoadElementParts(ABitmap, FMenuScrollArea, cxRectBounds(86, 310, 6, 12),
    rspMenuScrollAreaNormal, DefaultFixedSize, [], [DXBAR_HOT, DXBAR_PRESSED]);
  FQATDefaultGlyph := AddPart1x1(ABitmap, cxRectBounds(39, 378, 16, 16), rspQATDefaultGlyph);

  if TargetDPI >= 192 then
    LoadMenuCheckMark(cxRectBounds(120, 270, 40, 40))
  else
    LoadMenuCheckMark(cxRectBounds(99, 270, 20, 20))
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonApplicationButton(ABitmap: GpBitmap);

  procedure DoLoad(X, Y, W, H: Integer);
  begin
    LoadThreeStateArray(ABitmap, cxRectBounds(X, Y, W, H), cxEmptyRect,
      FApplicationButton[rcsaYellow], rspApplicationButton, True, InterpolationModeHighQualityBicubic);
  end;

var
  AIndex: TdxRibbonColorSchemeAccent;
begin
  if TargetDPI >= 192 then
    DoLoad(143, 190, 84, 84)
  else if TargetDPI >= 144 then
    DoLoad(144, 0, 63, 63)
  else
    DoLoad(93, 166, 42, 42);

  for AIndex := Low(TdxRibbonColorSchemeAccent) to High(TdxRibbonColorSchemeAccent) do
    FApplicationButton[AIndex] := FApplicationButton[rcsaYellow];
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonButtons(ABitmap: GpBitmap);
const
  ButtonGroupHeight = 22;
  ButtonGroupMiddleBorderWidth = 1;
  ButtonGroupWidth = 3;
begin
  inherited LoadRibbonButtons(ABitmap);

  FButtonGroupBorderLeft := AddPart3x3(ABitmap, cxRectBounds(37, 197, 2, ButtonGroupHeight), Rect(0, 2, 0, 2), rspButtonGroupBorderLeft);
  FButtonGroupBorderRight := AddPart3x3(ABitmap, cxRectBounds(38, 197, 2, ButtonGroupHeight), Rect(0, 2, 0, 2), rspButtonGroupBorderRight);
  LoadElementParts(ABitmap, FButtonGroupBorderMiddle,
    cxRectBounds(40, 86, ButtonGroupMiddleBorderWidth, ButtonGroupHeight),
    rspButtonGroupBorderMiddleNormal, Rect(0, 2, 0, 2), [0, 1, 2, 2, 2, 2, 2, 2, 3], []);
  LoadElementParts(ABitmap, FButtonGroupSplitButtonSeparator, cxRectBounds(37, 86, 2, ButtonGroupHeight),
    rspButtonGroupSplitButtonSeparatorNormal, Rect(0, 2, 0, 2), [0, 1, 2, 2, 3, 2, 2, 2, 4], []);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonCollapsedToolbar(ABitmap: GpBitmap);
const
  CollapsedToolbarWidth  = 7;
  CollapsedToolbarHeight = 85;
  CollapsedToolbarFixedSize: TRect = (Left: 3; Top: 15; Right: 3; Bottom: 3);
  CollapsedToolbarGlyphBackgroundWidth = 10;
  CollapsedToolbarGlyphBackgroundHeight = 31;
  CollapsedToolbarGlyphBackgroundFixedSize: TRect = (Left: 4; Top: 9; Right: 4; Bottom: 8);
begin
  LoadElementParts(ABitmap, FCollapsedToolbars,
    cxRectBounds(25, 0, CollapsedToolbarWidth, CollapsedToolbarHeight),
    rspCollapsedToolbarNormal, CollapsedToolbarFixedSize, [0,1,3,2],
    [DXBAR_NORMAL, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED]);
  LoadElementParts(ABitmap, FCollapsedToolbarGlyphBackgrounds,
    cxRectBounds(66, 199, CollapsedToolbarGlyphBackgroundWidth,
    CollapsedToolbarGlyphBackgroundHeight),
    rspCollapsedToolbarGlyphBackgroundNormal,
    CollapsedToolbarGlyphBackgroundFixedSize, [0,1,3,2],
    [DXBAR_NORMAL, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED]);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonContexts(ABitmap: GpBitmap);
begin
  FContextBackground := AddPart3x3(ABitmap, cxRectBounds(25, 439, 17, 25), cxRect(2,9,2,1), rspContextBackground);
  FContextTabGroupsArea[False] := AddPart3x3(ABitmap, cxRectBounds(12, 250, 11, 92), cxRect(5, 17, 5, 7), rspContextTabGroupsArea);
  FContextTabGroupsArea[True] := FContextTabGroupsArea[False];
  FContextTabSeparator[False] := AddPart3x3(ABitmap, cxRectBounds(25, 465, 1, 16), cxNullRect, rspContextTabSeparatorBegin);
  FContextTabSeparator[True] := AddPart3x3(ABitmap, cxRectBounds(25, 465, 1, 16), cxNullRect, rspContextTabSeparatorEnd);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonElements(ABitmap: GpBitmap);
begin
  inherited LoadRibbonElements(ABitmap);
  FProgressSubstrate := AddPart3x3(ABitmap, cxRectBounds(11, 237, 7, 7), DefaultFixedSize, rspProgressSubstrate);
  LoadRibbonMarkArrow(ABitmap);
  LoadRibbonCollapsedToolbar(ABitmap);
  LoadRibbonScrollBars(ABitmap);
  LoadRibbonScrollButtons(ABitmap);
  LoadRibbonContexts(ABitmap);
  LoadRibbonGroup(ABitmap);
  LoadRibbonStatusBar(ABitmap);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonForm(ABitmap: GpBitmap);
begin
  inherited LoadRibbonForm(ABitmap);
  FRibbonTopArea := AddPart3x3(ABitmap, cxRectBounds(99, 38, 2, 30), cxRect(0, 9, 0, 5), rspRibbonClientTopArea);
  LoadRibbonMinimizeButton(ABitmap);
  LoadRibbonFormBorders(ABitmap);
  LoadRibbonFormBorderIcons(ABitmap);
  LoadRibbonApplicationButton(ABitmap);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonMinimizeButton(ABitmap: GpBitmap);
begin
  // do nothing
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonFormBorderIcons(ABitmap: GpBitmap);
begin
  if TargetDPI >= 192 then
    LoadRibbonFormBorderIconsGlyphs(ABitmap, 144, 496, 18, 18)
  else
    if TargetDPI >= 144 then
      LoadRibbonFormBorderIconsGlyphs(ABitmap, 144, 443, 13, 13)
    else
      LoadRibbonFormBorderIconsGlyphs(ABitmap, 93, 0, 9, 9);

  LoadElementParts(ABitmap, FBorderIcons, cxRectBounds(118, 37, 25, 25),
    rfspBorderIconBackground, DefaultFixedSize, [0, 1, 2], [0, 1, 2], True,
    InterpolationModeNearestNeighbor);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonFormBorderIconsGlyphs(ABitmap: GpBitmap; X, Y, AWidth, AHeight: Integer);
var
  I: TdxRibbonBorderDrawIcon;
  ID: Integer;
  R: TRect;
begin
  ID := rfspBorderIconMinimizeGlyph;
  for I := Low(TdxRibbonBorderDrawIcon) to High(TdxRibbonBorderDrawIcon) do
  begin
    R := cxRectBounds(X, Y, AWidth, AHeight);
    LoadElementParts(ABitmap, FBorderIconGlyph[I], R, ID, cxNullRect,
      [0, 1, 2, 3], [0, 1, 2, 3], True, InterpolationModeNearestNeighbor);
    Inc(X, AWidth + 1);
    Inc(ID, 4);
  end;
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonFormBorders(ABitmap: GpBitmap);
var
  R: TRect;
begin
  AddTwoStateElement(ABitmap, FCaption,
    cxRectBounds(93, 37, 14, 31), cxRect(6, 10, 6, 5), rfspActiveCaption);
  AddTwoStateElement(ABitmap, FCaptionZoomed,
    cxRectBounds(99, 37, 2, 31), cxRect(0, 10, 0, 5), rfspActiveCaptionZoomed);
  // Caption Borders
  R := cxRectBounds(93, 37, 4, 31);
  AddTwoStateElement(ABitmap, FCaptionLeftBorder, R, cxRect(0, 9, 0, 2), rfspActiveCaptionLeftBorder);
  OffsetRect(R, 10, 0);
  AddTwoStateElement(ABitmap, FCaptionRightBorder, R, cxRect(0, 9, 0, 2), rfspActiveCaptionRightBorder);
  //active border
  R := cxRectBounds(108, 37, 4, 6);
  AddTwoStateElement(ABitmap, FLeftBorder, R, cxRect(0, 0, 0, 5), rfspActiveLeftBorder);
  OffsetRect(R, 5, 0);
  AddTwoStateElement(ABitmap, FRightBorder, R, cxRect(0, 0, 0, 5), rfspActiveRightBorder);
  //bottom border
  AddTwoStateElement(ABitmap, FBottomBorderThin,
    cxRectBounds(108, 50, 2, 2), cxEmptyRect, rfspActiveBottomBorderThin);
  AddTwoStateElement(ABitmap, FBottomBorderThick[False],
    cxRectBounds(133, 113, 10, 4), cxRect(4, 0, 4, 0), rfspActiveBottomBorderThick);
  AddTwoStateElement(ABitmap, FBottomBorderThick[True],
    cxRectBounds(133, 121, 10, 4), cxRect(4, 0, 4, 0), rfspActiveBottomBorderThickRectangular);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonGallery(ABitmap: GpBitmap);
begin
  inherited LoadRibbonGallery(ABitmap);
  LoadRibbonGalleryInRibbonScrollBarButtons(ABitmap);
  FGalleryFilterBand := AddPart3x3(ABitmap, cxRectBounds(7, 250, 4, 13), cxRectBounds(1, 1, 1, 0), rspGalleryFilterBand);
  FGalleryGroupHeader := AddPart3x3(ABitmap, cxRectBounds(0, 273, 4, 4),  cxRectBounds(0, 0, 0, 2), rspGalleryGroupHeader);
  FDropDownGalleryTopSizingBand := AddPart3x3(ABitmap, cxRectBounds(38, 29, 4, 11), cxRectBounds(1, 1, 1, 1), rspDropDownGalleryTopSizingBand);
  FDropDownGalleryBottomSizingBand := AddPart3x3(ABitmap, cxRectBounds(33, 29, 4, 11), cxRectBounds(1, 1, 1, 1), rspDropDownGalleryBottomSizingBand);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonGalleryInRibbonScrollBarButtons(ABitmap: GpBitmap);
const
  ScrollBarButtonWidth = 15;
  ScrollBarButtonHeight = 20;
begin
  LoadElementParts(ABitmap, FInRibbonGalleryScrollBarLineUpButton,
    cxRectBounds(78, 0, ScrollBarButtonWidth, ScrollBarButtonHeight),
    rspInRibbonGalleryScrollBarLineUpButtonNormal, DefaultFixedSize,
    [0, 3, 1, 2], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_PRESSED]);
  LoadElementParts(ABitmap, FInRibbonGalleryScrollBarLineDownButton,
    cxRectBounds(78, 80, ScrollBarButtonWidth, ScrollBarButtonHeight),
    rspInRibbonGalleryScrollBarLineDownButtonNormal, DefaultFixedSize,
    [0, 3, 1, 2], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_PRESSED]);
  LoadElementParts(ABitmap, FInRibbonGalleryScrollBarDropDownButton,
    cxRectBounds(78, 160, ScrollBarButtonWidth, ScrollBarButtonHeight),
    rspInRibbonGalleryScrollBarDropDownButtonNormal, DefaultFixedSize,
    [0, 3, 1, 2], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_PRESSED]);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonGroup(ABitmap: GpBitmap);
begin
  LoadElementParts(ABitmap, FToolbar, cxRectBounds(66, 350, 10, 20),
    rspToolbarNormal, cxRect(3, 2, 3, 0), [], [DXBAR_NORMAL, DXBAR_HOT], False);
  LoadElementParts(ABitmap, FToolbarHeader, cxRectBounds(66, 370, 10, 17),
    rspToolbarHeaderNormal, cxRect(3, 0, 3, 3), [], [DXBAR_NORMAL, DXBAR_HOT], False);

  FTabGroupsArea[0] := AddPart3x3(ABitmap, cxRectBounds(13, 116, 11, 92), cxRect(5, 17, 5, 7), rspTabGroupsArea);
  FTabGroupsArea[1] := FTabGroupsArea[0];
  FTabGroupsArea[2] := FTabGroupsArea[0];
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonMarkArrow(ABitmap: GpBitmap);
var
  R: TRect;
begin
  if TargetDPI >= 192 then
    R := cxRectBounds(33, 320, 13, 13)
  else if TargetDPI >= 144 then
    R := cxRectBounds(51, 295, 11, 11)
  else
    R := cxRectBounds(36, 220, 7, 7);

  LoadElementParts(ABitmap, FMarkArrow, R, rspMarkArrowNormal,
    cxEmptyRect, [0, 0, 1], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED], True);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonMenu(ABitmap: GpBitmap);
begin
  inherited LoadRibbonMenu(ABitmap);
  FMenuScrollArea[DXBAR_NORMAL] := AddPart3x3(ABitmap, cxRectBounds(20, 237, 4, 12), Rect(1, 1, 1, 1), rspMenuScrollAreaNormal);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonMenuMarks(ABitmap: GpBitmap);

  procedure LoadMarkArrows(const ADownArrow, ARightArrow: TRect);
  begin
    FMenuArrowDown := AddPart1x1(ABitmap, cxRectBounds(5, 244, 9, 6), rspMenuArrowDown);
    FMenuArrowRight := AddPart1x1(ABitmap, cxRectBounds(5, 236, 6, 9), rspMenuArrowRight);
  end;

begin
  inherited LoadRibbonMenuMarks(ABitmap);

  if TargetDPI >= 192 then
    LoadMarkArrows(cxRectBounds(65, 338, 17, 11), cxRectBounds(0, 292, 11, 17))
  else
    if TargetDPI >= 144 then
      LoadMarkArrows(cxRectBounds(65, 329, 13, 9), cxRectBounds(0, 278, 9, 13))
    else
      LoadMarkArrows(cxRectBounds(5, 244, 9, 6), cxRectBounds(5, 236, 6, 9));
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonQAT(ABitmap: GpBitmap);
begin
  inherited LoadRibbonQAT(ABitmap);
  FQATPopup :=  AddPart3x3(ABitmap, cxRectBounds(33, 0, 6, 28), cxRect(2, 2, 2, 2), rspQATPopup);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonScrollBars(ABitmap: GpBitmap);
begin
  LoadFourStateArray(ABitmap, cxRectBounds(43, 466, 10, 17), cxRect(4, 4, 4, 4), FScrollBarBackground[True], rspScrollBarHorz);
  LoadFourStateArray(ABitmap, cxRectBounds(54, 466, 17, 10), cxRect(4, 4, 4, 4), FScrollBarBackground[False], rspScrollBarVert);

  LoadFourStateArray(ABitmap, cxRectBounds(72, 466, 17, 17), cxRect(4, 4, 4, 4), FScrollBarThumb[True], rspScrollBarHorzThumb);
  LoadFourStateArray(ABitmap, cxRectBounds(90, 466, 17, 17), cxRect(4, 4, 4, 4), FScrollBarThumb[False], rspScrollBarVertThumb);

  LoadFourStateArray(ABitmap, cxRectBounds(108, 466, 17, 17), cxRect(4, 4, 4, 4), FScrollBarButtonLeftTop[False], rspScrollBarButtonTop);
  LoadFourStateArray(ABitmap, cxRectBounds(43, 544, 17, 17), cxRect(4, 4, 4, 4), FScrollBarButtonLeftTop[True], rspScrollBarButtonLeft, False);
  LoadFourStateArray(ABitmap, cxRectBounds(126, 466, 17, 17), cxRect(4, 4, 4, 4), FScrollBarButtonRightBottom[False], rspScrollBarButtonBottom);
  LoadFourStateArray(ABitmap, cxRectBounds(43, 562, 17, 17), cxRect(4, 4, 4, 4), FScrollBarButtonRightBottom[True], rspScrollBarButtonRight, False);

  LoadRibbonScrollBarsGlyphs(ABitmap);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonScrollBarsGlyphs(ABitmap: GpBitmap);

  procedure DoLoadButtonsGlyphs(X, Y, W, H: Integer);
  begin
    FScrollBarButtonLeftTopGlyph[False] := AddPart1x1(ABitmap, cxRectBounds(X, Y, W, H), rspScrollBarButtonTopGlyph);
    FScrollBarButtonRightBottomGlyph[False] := AddPart1x1(ABitmap, cxRectBounds(X, Y + H, W, H), rspScrollBarButtonBottomGlyph);
    FScrollBarButtonLeftTopGlyph[True] := AddPart1x1(ABitmap, cxRectBounds(X, Y + 2 * H, W, H), rspScrollBarButtonLeftGlyph);
    FScrollBarButtonRightBottomGlyph[True] := AddPart1x1(ABitmap, cxRectBounds(X, Y + 3 * H, W, H), rspScrollBarButtonRightGlyph);
  end;

  procedure DoLoadThumbGlyphs(X, Y, W, H: Integer);
  begin
    FScrollBarThumbGlyph[False] := AddPart1x1(ABitmap, cxRectBounds(X, Y, W, H), rspScrollBarHorzThumbGlyph);
    FScrollBarThumbGlyph[True] := AddPart1x1(ABitmap, cxRectBounds(X, Y + H, W, H), rspScrollBarVertThumbGlyph);
  end;

begin
  if TargetDPI >= 192 then
  begin
    DoLoadButtonsGlyphs(126, 398, 17, 17);
    DoLoadThumbGlyphs(112, 535, 21, 21);
  end
  else
    if TargetDPI >= 144 then
    begin
      DoLoadButtonsGlyphs(112, 398, 13, 13);
      DoLoadThumbGlyphs(25, 522, 16, 16);
    end
    else
    begin
      DoLoadButtonsGlyphs(102, 398, 9, 9);
      DoLoadThumbGlyphs(54, 507, 11, 11);
    end;
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonScrollButtons(ABitmap: GpBitmap);
var
  R, FR: TRect;
begin
  FR := cxRect(3, 4, 3, 5);
  R := cxRectBounds(46, 350, 9, 24);
  LoadThreeStateArray(ABitmap, R, FR, FTabScrollButtons[True], rspTabScrollLeftButtonNormal);
  R := cxRectBounds(56, 350, 9, 24);
  LoadThreeStateArray(ABitmap, R, FR, FTabScrollButtons[False], rspTabScrollRightButtonNormal);
  R := cxRectBounds(48, 0, 8, 92);
  LoadThreeStateArray(ABitmap, R, cxRect(4, 4, 2, 4), FGroupScrollButtons[True], rspGroupScrollLeftButtonNormal);
  R := cxRectBounds(57, 0, 8, 92);
  LoadThreeStateArray(ABitmap, R, cxRect(2, 4, 4, 4), FGroupScrollButtons[False], rspGroupScrollRightButtonNormal);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonStatusBar(ABitmap: GpBitmap);
begin
  FStatusBar := AddPart1x3(ABitmap, cxRectBounds(42, 138, 2, 22), 2, 3, rspStatusBar);
  FStatusBarPanel := FStatusBar;
  FStatusBarPanelLowered := FStatusBar;
  FStatusBarPanelRaised := AddPart1x3(ABitmap, cxRectBounds(42, 160, 2, 22), 2, 3, rspStatusBarPanelRaised);

  FStatusBarPanelSeparator := AddPart1x3(ABitmap,
    cxRectBounds(42, 183, 3, 22), 2, 3, rspStatusBarPanelSeparator);
  FStatusBarToolbarSeparator := AddPart1x3(ABitmap,
    cxRectBounds(45, 138, 2, 22), 2, 3, rspStatusBarToolbarSeparator);
  FStatusBarGripBackground := AddPart3x3(ABitmap,
    cxRectBounds(42, 183, 5, 22), cxRect(3, 2, 0, 3), rspStatusBarGripBackground);

  LoadElementParts(ABitmap, FFormStatusBarLeftParts[False], cxRectBounds(77, 241, 4, 22),
    rspStatusBarFormLeftPart, cxRect(0, 2, 0, 3), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarLeftParts[True], cxRectBounds(85, 241, 4, 22),
    rspStatusBarFormLeftPartDialog, cxRect(0, 2, 0, 3), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarRightParts[False], cxRectBounds(81, 241, 4, 22),
    rspStatusBarFormRightPart, cxRect(0, 2, 0, 3), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarRightParts[True], cxRectBounds(89, 241, 4, 22),
    rspStatusBarFormRightPartDialog, cxRect(0, 2, 0, 3), [0, 1, 2, 3], [0, 1, 2, 3]);
end;

procedure TdxCustomRibbon2007Skin.LoadRibbonTab(ABitmap: GpBitmap);
begin
  inherited LoadRibbonTab(ABitmap);
  LoadElementParts(ABitmap, FTabIndex, cxRectBounds(0, 0, 24, 23), rspTabNormal,
    cxRect(4, 4, 4, 4), [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
  LoadElementParts(ABitmap, FContextTabIndex, cxRectBounds(0, 439, 24, 23), rspContextTabNormal,
    cxRect(5, 3, 5, 1), [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
end;

{ TdxBlueRibbonSkin }

procedure TdxBlueRibbonSkin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  FillRectByColor(DC, R1, $EBC3A4);
  OffsetRect(R1, 0, 1);
  FillRectByColor(DC, R1, $F3E2D5);
end;

function TdxBlueRibbonSkin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
const
  RibbonEditHotBackgroundColor = clWhite;
  RibbonEditNormalBorderColor = $DEC1AB;
  RibbonEditHotBorderColor = $E1C7B3;
  RibbonEditDisabledBorderColor = $C6BBB1;
begin
  case APart of
    DXBAR_SCREENTIP_FOOTERLINE:
      Result := $DDBB9E;
    DXBAR_DATENAVIGATOR_HEADER:
      Result := $DAD5D2;
    DXBAR_SEPARATOR_BACKGROUND:
      Result := $EFE7DE;
    DXBAR_INRIBBONGALLERY_BORDER:
      Result := $EDD0B9;

    rspRibbonBackground:
      Result := $FFDBBF;
    rspRibbonBottomEdge:
      Result := $F3E2D5;
    rfspRibbonForm:
      Result := $EBC3A4;
    rspTabGroupHeaderText:
      Result := $AA6A3E;
    rspStatusBarSizeGripColor1:
      Result := $805D45;
    rspStatusBarSizeGripColor2:
      Result := $E8C9B1;

    DXBAR_MENUEDITSEPARATOR:
      case AState of
        DXBAR_ACTIVE:
          Result := $85B6CA;
        DXBAR_ACTIVEDISABLED:
          Result := $CDCDCD;
      else
        Result := inherited;
      end;

    DXBAR_ARROWDOWN:
      if AState <> DXBAR_DISABLED then
        Result := $BD8C67
      else
        Result := inherited;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := inherited
      else
        Result := $6E1500;

    DXBAR_EDIT_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := RibbonEditNormalBorderColor;
        DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditHotBorderColor;
        DXBAR_DISABLED:
          Result := RibbonEditDisabledBorderColor;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN:
          Result := RibbonEditHotBorderColor;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_NORMAL:
          Result := $FBF2EA;
        DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditHotBackgroundColor;
        DXBAR_DISABLED:
          Result := $EFEFEF;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN:
          Result := RibbonEditHotBackgroundColor;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := RibbonEditNormalBorderColor;
        DXBAR_ACTIVE:
          Result := $DEC7AD;
        DXBAR_HOT:
          Result := $99CEDB;
        DXBAR_PRESSED:
          Result := $45667B;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditDisabledBorderColor;
        DXBAR_DROPPEDDOWN:
          Result := $6B99A5;
      else
        Result := inherited;
      end;

    DXBAR_INRIBBONGALLERY_BACKGROUND:
      if AState in [DXBAR_ACTIVE, DXBAR_HOT] then
        Result := $FBF3EC
      else
        Result := $F8E6D4;

    DXBAR_GALLERYFILTERBANDTEXT:
      if AState = DXBAR_NORMAL then
        Result := $6E1500
      else if AState = DXBAR_HOT then
        Result := $FF6600
      else
        raise EdxException.Create('');

    rspFormCaptionText:
      if AState = DXBAR_NORMAL then
        Result := $AA6A3E
      else
        Result := $A0A0A0;

    rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := $797069
      else
        Result := $A0A0A0;

    rspTabHeaderText, rspTabGroupText:
      if AState = DXBAR_DISABLED then
        Result := clGray
      else
        Result := $8B4215;

    rspStatusBarText:
      if AState = DXBAR_DISABLED then
        Result := $8D8D8D
      else
        Result := $8B4215;
  else
    Result := inherited;
  end;
end;

function TdxBlueRibbonSkin.GetName: string;
begin
  Result := 'Blue';
end;

procedure TdxBlueRibbonSkin.GetApplicationMenuContentColors(
  var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  if LowColors then
    inherited GetApplicationMenuContentColors(AInnerBorderColor, AOuterBorderColor, ASideColor)
  else
  begin
    AInnerBorderColor := $CAAF9B;
    AOuterBorderColor := clWhite;
    ASideColor := $EDD3BE;
  end;
end;

procedure TdxBlueRibbonSkin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONBLUE', AImage);
end;

{ TdxBlackRibbonSkin }

procedure TdxBlackRibbonSkin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  FillRectByColor(DC, R1, $4F4F4F);
  OffsetRect(R1, 0, 1);
  FillRectByColor(DC, R1, $626262);
end;

function TdxBlackRibbonSkin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
const
  RibbonEditHotBackgroundColor = clWhite;
  RibbonEditNormalBorderColor = $898989;
  RibbonEditHotBorderColor = $898989;
  RibbonEditDisabledBorderColor = $CCCCCC;
  RibbonItemText = $464646;
begin
  case APart of
    DXBAR_MENUEDITSEPARATOR:
      case AState of
        DXBAR_ACTIVE:
          Result := $85B6CA;
        DXBAR_ACTIVEDISABLED:
          Result := $CDCDCD;
      else
        Result := inherited;
      end;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := inherited
      else
        Result := RibbonItemText;

    DXBAR_EDIT_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := RibbonEditNormalBorderColor;
        DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditHotBorderColor;
        DXBAR_DISABLED:
          Result := RibbonEditDisabledBorderColor;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN:
          Result := RibbonEditHotBorderColor;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_NORMAL:
          Result := $E8E8E8;
        DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditHotBackgroundColor;
        DXBAR_DISABLED:
          Result := $EFEFEF;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN:
          Result := RibbonEditHotBackgroundColor;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := RibbonEditNormalBorderColor;
        DXBAR_ACTIVE:
          Result := $B7B7B7;
        DXBAR_HOT:
          Result := $99CEDB;
        DXBAR_PRESSED:
          Result := $45667B;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := RibbonEditDisabledBorderColor;
        DXBAR_DROPPEDDOWN:
          Result := $6B99A5;
      else
        Result := inherited;
      end;

    DXBAR_DATENAVIGATOR_HEADER:
      Result := $DAD5D2;
    DXBAR_SEPARATOR_BACKGROUND:
      Result := $EFEBEF;
    DXBAR_SCREENTIP_FOOTERLINE:
      Result := $A49991;
    DXBAR_INRIBBONGALLERY_BACKGROUND:
      if AState in [DXBAR_ACTIVE, DXBAR_HOT] then
        Result := $F7F7F7
      else
        Result := $E2E2DA;
    DXBAR_INRIBBONGALLERY_BORDER:
      Result := $ACACAC;
    DXBAR_GALLERYFILTERBANDTEXT:
      if AState = DXBAR_NORMAL then
        Result := $FFFFFF
      else if AState = DXBAR_HOT then
        Result := $32D2FF
      else
        raise EdxException.Create('');

    rspRibbonBackground:
      Result := $535353;
    rspRibbonBottomEdge:
      Result := $626262;
    rfspRibbonForm:
      Result := $696969;
    rspFormCaptionText:
      if AState = DXBAR_NORMAL then
        Result := $FFD1AE
      else
        Result := $E1E1E1;
    rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := $FFFFFF
      else
        Result := $E1E1E1;

    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE:
          Result := clBlack;
        DXBAR_DISABLED:
          Result := clGray;
      else
        Result := $FFFFFF;
      end;

    rspTabGroupText:
      Result := RibbonItemText;
    rspTabGroupHeaderText:
      Result := $FFFFFF;
    rspStatusBarText:
      case AState of
        DXBAR_NORMAL:
          Result := $FFFFFF;
        DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_CHECKED:
          Result := clBlack;
      else
        Result := $C2C2C2;
      end;
    rspStatusBarSizeGripColor1:
      Result := $252525;
    rspStatusBarSizeGripColor2:
      Result := $CCCCCC;

  else
    Result := inherited;
  end;
end;

function TdxBlackRibbonSkin.GetName: string;
begin
  Result := 'Black';
end;

procedure TdxBlackRibbonSkin.GetApplicationMenuContentColors(
  var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  if LowColors then
    inherited GetApplicationMenuContentColors(AInnerBorderColor, AOuterBorderColor, ASideColor)
  else
  begin
    AInnerBorderColor := $414243;
    AOuterBorderColor := $716C6B;
    ASideColor := $504F4F;
  end;
end;

procedure TdxBlackRibbonSkin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONBLACK', AImage);
end;

{ TdxSilverRibbonSkin }

procedure TdxSilverRibbonSkin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  FillRectByColor(DC, R1, $808080);
  OffsetRect(R1, 0, 1);
  FillRectByColor(DC, R1, $DCE1EB);
end;

function TdxSilverRibbonSkin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
const
  RibbonItemText = $5C534C;
begin
  case APart of
    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := inherited
      else
        Result := RibbonItemText;

    DXBAR_INRIBBONGALLERY_BACKGROUND:
      if AState in [DXBAR_ACTIVE, DXBAR_HOT] then
        Result := $F2F1F0
      else
        Result := $ECEAE8;

    DXBAR_INRIBBONGALLERY_BORDER:
      if AState in [DXBAR_ACTIVE, DXBAR_HOT] then
        Result := $A4A4A4
      else
        Result := $B8B1A9;

    DXBAR_GALLERYFILTERBANDTEXT:
      if AState = DXBAR_NORMAL then
        Result := $FFFFFF
      else if AState = DXBAR_HOT then
        Result := $32D2FF
      else
        raise EdxException.Create('');

    rspRibbonBackground:
      Result := $DDD4D0;
    rspRibbonBottomEdge:
      Result := $808080;
    rfspRibbonForm:
      Result := $B5AEAA;

    rspFormCaptionText:
      if AState = DXBAR_NORMAL then
        Result := $AA6E35
      else
        Result := $8A8A8A;

    rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := $6A625C
      else
        Result := $8A8A8A;

    rspTabHeaderText:
      if AState = DXBAR_DISABLED then
        Result := clGray
      else
        Result := $595453;

    rspTabGroupText, rspTabGroupHeaderText:
      Result := RibbonItemText;
    rspStatusBarSizeGripColor1:
      Result := $7E77670;
    rspStatusBarSizeGripColor2:
      Result := $D9D0CD;
    rspStatusBarText:
      if AState = DXBAR_DISABLED then
        Result := $8D8D8D
      else
        Result := $595453;

  else
    Result := inherited;
  end;
end;

function TdxSilverRibbonSkin.GetName: string;
begin
  Result := 'Silver';
end;

procedure TdxSilverRibbonSkin.GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  if LowColors then
    inherited GetApplicationMenuContentColors(AInnerBorderColor, AOuterBorderColor, ASideColor)
  else
  begin
    AInnerBorderColor := $B4AEA9;
    AOuterBorderColor := $FAFAFA;
    ASideColor := $D8D2CD;
  end;
end;

procedure TdxSilverRibbonSkin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONSILVER', AImage);
end;

{ RegisterSkins }

procedure RegisterSkins;
begin
  if CheckGdiPlus(True) then
  begin
    dxRibbonSkinsManager.Add(TdxBlueRibbonSkin.Create);
    dxRibbonSkinsManager.Add(TdxBlackRibbonSkin.Create);
    dxRibbonSkinsManager.Add(TdxSilverRibbonSkin.Create);
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterSkins, nil);
end.

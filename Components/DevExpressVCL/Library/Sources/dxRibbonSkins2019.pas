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

unit dxRibbonSkins2019;

{$I cxVer.inc}
{$R dxRibbonSkins2019.res}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Graphics, dxRibbonSkins, dxRibbonSkins2013, dxRibbonSkins2016, cxLookAndFeelPainters, dxCoreGraphics,
  dxGDIPlusClasses;

type

  { TdxCustomRibbon2019Skin }

  TdxCustomRibbon2019Skin = class(TdxCustomRibbon2013Skin)
  strict private const
    BackstageViewBackButtonSize = 21;
    BackstageViewGalleryItemIndent = 2;
    TabGroupsAreaShadowSize = 5;
  protected
    FBackButton: TdxSmartImage;

    function CreateTabsAreaColorPalette: IdxRibbonSkinColorPaletteSet; virtual;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetFormBorderIconBackgroundColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; virtual;
    function GetFormCaptionAreaColor(AApplicationMenuState: TdxRibbonApplicationMenuState = ramsHidden; AActive: Boolean = True): TColor; override;
    function GetMasterDarkColor: TColor; virtual;
    function GetStyle: TdxRibbonStyle; override;
    function GetTabSeparatorColor: TColor; virtual;
    procedure InitializeColorPalettes; override;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AdjustApplicationButtonFont(AFont: TFont; AState: TdxRibbonApplicationButtonState); override;

    procedure AdjustRibbonFormBorderIconSize(AIcon: TdxRibbonBorderDrawIcon;
      AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize); override;
    procedure DrawFormBorderIconBackground(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); override;

    procedure DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect); override;
    function GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor; override;

    procedure DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMinimizeRibbonButton(DC: HDC; const R: TRect; AState: TcxButtonState; AMinimized: Boolean); override;

    procedure AdjustTabFont(AFont: TFont; AState: Integer); override;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabBase(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaButtonCore(DC: HDC; const R: TRect; AFrameColor, AContentColor: TdxAlphaColor);
    procedure DrawTabAreaButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawTabAreaButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte); override;

    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;

    procedure DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    procedure DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawTabGroupsAreaShadow(DC: HDC; const R: TRect);
    function GetGroupBottomSeparatorColor: TColor; virtual;
    function GetGroupShadowColor1: TColor; virtual;
    function GetGroupShadowColor2: TColor; virtual;

    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbar(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean); override;

    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
    function GetRibbonTopFrameAreaSeparatorSize: Integer; override;

    function GetPartContentOffsets(APart: Integer): TRect; override;
    function GetPartSize(APart: Integer): Integer; override;

    procedure UpdateBitsPerPixel; override;
  end;

  { TdxCustomColorfulRibbon2019Skin }

  TdxCustomColorfulRibbon2019Skin = class(TdxCustomRibbon2019Skin)
  strict private
    FHighlightBorderColor: TColor;
    FHighlightContentColor: TColor;
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetFormBorderColor(AActive: Boolean): TColor; override;
    function GetHighlightBorderColor: TColor; override;
    function GetHighlightContentColor: TColor; override;
    procedure InitializeColorPalettes; override;
  public
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawApplicationMenuBackground(DC: HDC; const R: TRect; const AContentRect: TRect); override;

    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;

    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); override;
    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); override;

    procedure DrawStatusBar(DC: HDC; const R: TRect); override;
    procedure DrawTabBase(DC: HDC; const R: TRect); override;
    function GetTabAreaBackgroundColor(AApplicationMenuState: TdxRibbonApplicationMenuState): TColor; override;

    procedure DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer); override;

    procedure DrawQuickAccessToolbarArrowDown(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarMark(DC: HDC; const R: TRect; AState: Integer; AStatesArray: TStatesArray);
    procedure DrawQuickAccessToolbarMarkArrow(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarMarkTruncated(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    function GetQuickAccessToolbarMarkColor(AState: Integer): Integer;
  end;

  { TdxCustomGrayScaleRibbon2019Skin }

  TdxCustomGrayScaleRibbon2019Skin = class(TdxCustomColorfulRibbon2019Skin)
  protected
    function CreateTabsAreaColorPalette: IdxRibbonSkinColorPaletteSet; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; override;
  public
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    function GetScrollBarBackgroundColor: TColor; override;
    function GetScrollBarGlyphColor: TColor; override;
    function GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

  { TdxColorfulRibbon2019Skin }

  TdxColorfulRibbon2019Skin = class(TdxCustomColorfulRibbon2019Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; override;
    function GetFormCaptionAreaColor(AApplicationMenuState: TdxRibbonApplicationMenuState = ramsHidden; AActive: Boolean = True): TColor; override;
    function GetFrameColor: TColor; override;
    function GetMasterColor: TColor; override;
    function GetMasterDarkColor: TColor; override;
    function GetName: string; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    function GetGroupBottomSeparatorColor: TColor; override;
    function GetGroupScrollButtonBorderColor(AState: Integer): TColor; override;
    function GetGroupScrollButtonContentColor(AState: Integer): TColor; override;
    function GetGroupShadowColor1: TColor; override;
  end;

  { TdxBlackRibbon2019Skin }

  TdxBlackRibbon2019Skin = class(TdxCustomGrayScaleRibbon2019Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetFrameColor: TColor; override;
    function GetHighlightBorderColor: TColor; override;
    function GetHighlightContentColor: TColor; override;
    function GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor; override;
    function GetMasterColor: TColor; override;
    function GetMasterDarkColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor; override;
    function GetName: string; override;
    function GetTabGroupsAreaContentColor: TColor; override;
    function GetTabSeparatorColor: TColor; override;
  public
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSeparatorBackground(DC: HDC; const R: TRect); override;
    procedure DrawTabBase(DC: HDC; const R: TRect); override;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    function GetGroupBottomSeparatorColor: TColor; override;
  end;

  { TdxDarkGrayRibbon2019Skin }

  TdxDarkGrayRibbon2019Skin = class(TdxCustomGrayScaleRibbon2019Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetFrameColor: TColor; override;
    function GetMasterColor: TColor; override;
    function GetMasterDarkColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetName: string; override;
    function GetTabGroupsAreaContentColor: TColor; override;
    function GetTabSeparatorColor: TColor; override;
  public
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    function GetGroupBottomSeparatorColor: TColor; override;
  end;

  { TdxWhiteRibbon2019Skin }

  TdxWhiteRibbon2019Skin = class(TdxCustomRibbon2019Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetFormBackgroundColor2: TColor; override;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; override;
    function GetFrameColor: TColor; override;
    function GetMasterColor: TColor; override;
    function GetName: string; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    function GetGroupShadowColor1: TColor; override;
    function GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

implementation

uses
  dxCore, dxGDIPlusAPI, cxGraphics, dxBarSkinConsts, cxGeometry, Math, SysUtils, cxControls;

{ TdxCustomRibbon2019Skin }

destructor TdxCustomRibbon2019Skin.Destroy;
begin
  FreeAndNil(FBackButton);
  inherited Destroy;
end;

procedure TdxCustomRibbon2019Skin.AfterConstruction;
begin
  inherited;
  FBackButton := TdxSmartImage.Create;
  FBackButton.LoadFromResource(HInstance, 'RIBBON2019BACKBUTTON', RT_RCDATA);
end;

procedure TdxCustomRibbon2019Skin.AdjustApplicationButtonFont(AFont: TFont; AState: TdxRibbonApplicationButtonState);
begin
  inherited;
  if AState = rabsHot then
    AFont.Style := AFont.Style + [fsBold];
end;

procedure TdxCustomRibbon2019Skin.AdjustRibbonFormBorderIconSize(
  AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize);
begin
  ASize.cy := ACaptionHeight - 2;
  ASize.cx := MulDiv(ASize.cy, 4, 3);
end;

procedure TdxCustomRibbon2019Skin.DrawFormBorderIconBackground(
  DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
var
  AColor: TColor;
begin
  AColor := GetFormBorderIconBackgroundColor(AIcon, AState);
  if AColor <> clNone then
    FillRectByColor(DC, R, AColor);
end;

procedure TdxCustomRibbon2019Skin.DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer);
var
  AGlyphRect: TRect;
begin
  DrawBackstageViewTabButton(DC, R, AState);

  AGlyphRect := cxRectCenterVertically(R, ScaleFactor.Apply(BackstageViewBackButtonSize));
  AGlyphRect := cxRectSetWidth(AGlyphRect, ScaleFactor.Apply(BackstageViewBackButtonSize));
  AGlyphRect := cxRectOffset(AGlyphRect, GetPartContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB).Left, 0);
  if UseRightToLeftAlignment then
    AGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(AGlyphRect, R);
  if IsWinXP and UseRightToLeftAlignment then
    AGlyphRect := cxRectInflate(AGlyphRect, 1, 1);
  cxRightToLeftDependentDraw(DC, AGlyphRect, UseRightToLeftAlignment,
    procedure
    begin
      FBackButton.StretchDraw(DC, AGlyphRect, MaxByte, TdxSimpleColorPalette.Create(
        dxColorToAlphaColor(GetBackstageViewBackButtonGlyphColor(AState)), TdxAlphaColors.Empty));
    end);
end;

procedure TdxCustomRibbon2019Skin.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  FillRectByColor(DC, cxRectInflate(R, 0, -BackstageViewGalleryItemIndent),
    GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM, AState));
end;

procedure TdxCustomRibbon2019Skin.DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect);
begin
  inherited DrawBackstageViewGallerySeparator(DC, cxRectInflate(R, 0, -BackstageViewGalleryItemIndent));
end;

function TdxCustomRibbon2019Skin.GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor;
begin
  Result := clWhite;
end;

function TdxCustomRibbon2019Skin.CreateTabsAreaColorPalette: IdxRibbonSkinColorPaletteSet;
var
  AColor: TColor;
  AHSL: TdxHSL;
  APalette: TdxRibbonSkinColorPaletteSet;
begin
  AColor := GetAccentColor;
  APalette := TdxRibbonSkinColorPaletteSet.Create;
  APalette.Add(DXBAR_NORMAL, TdxSimpleColorPalette.Create(dxColorToAlphaColor(AColor), TdxAlphaColors.Empty));

  AHSL := TdxColorSpaceConverter.ColorToHSL(AColor);
  AHSL.S := 0;
  AColor := TdxColorSpaceConverter.HSLToColor(AHSL);
  APalette.Add(DXBAR_DISABLED, TdxSimpleColorPalette.Create(dxColorToAlphaColor(AColor), TdxAlphaColors.Empty));

  Result := APalette;
end;

function TdxCustomRibbon2019Skin.GetFormBorderIconBackgroundColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  if AIcon <> rbdiClose then
    Result := GetColorfulButtonColor(BorderIconStateToBarState[AState])
  else
    case AState of
      rbisHot, rbisHotInactive:
        Result := $2311E8;
      rbisPressed:
        Result := $7A70F1;
    else
      Result := clNone;
    end;
end;

function TdxCustomRibbon2019Skin.GetFormCaptionAreaColor(
  AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor;
begin
  if AApplicationMenuState = ramsShownAsFullScreenFrame then
    Result := GetPartColor(DXBAR_BACKSTAGEVIEW)
  else
    if AActive then
      Result := GetMasterDarkColor
    else
      Result := GetMasterColor;
end;

procedure TdxCustomRibbon2019Skin.DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer);
begin
  InternalDrawGlyph(DC, R, FMenuCheckMark[AState], GetPartColor(DXBAR_MENUITEMTEXT), False);
end;

procedure TdxCustomRibbon2019Skin.DrawMinimizeRibbonButton(
  DC: HDC; const R: TRect; AState: TcxButtonState; AMinimized: Boolean);
begin
  DrawSmallButton(DC, R, ButtonStateToRibbonState(AState));
end;

procedure TdxCustomRibbon2019Skin.AdjustTabFont(AFont: TFont; AState: Integer);
begin
  inherited;
  case AState of
    DXBAR_ACTIVE, DXBAR_HOT:
      AFont.Style := AFont.Style + [fsBold];
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  case AState of
    rtsActiveHot, rtsFocused, rtsHot:
      FillRectByColor(DC, R, $F8F9FA);
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTabBase(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetAccentColor);
end;

procedure TdxCustomRibbon2019Skin.DrawTabAreaButtonCore(DC: HDC; const R: TRect; AFrameColor, AContentColor: TdxAlphaColor);
begin
  dxGPPaintCanvas.BeginPaint(DC, R);
  try
    dxGPPaintCanvas.SmoothingMode := smAntiAlias;
    try
      dxGPPaintCanvas.RoundRect(cxRectInflate(R, -1), AFrameColor, AContentColor, 1, 1);
    finally
      dxGPPaintCanvas.SmoothingMode := smDefault;
    end;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTabAreaButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
var
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    DrawTabAreaButton(DC, cxRectInflate(R, 1, 0, 0, 0), RibbonStateToButtonState(AState));
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTabAreaButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
var
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    DrawTabAreaButton(DC, cxRectInflate(R, 0, 0, 2, 0), RibbonStateToButtonState(AState));
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte);
begin
  if Alpha < MaxByte then
    dxGpFillRect(DC, R, GetTabSeparatorColor, Alpha)
  else
    FillRectByColor(DC, R, GetTabSeparatorColor);
end;

function TdxCustomRibbon2019Skin.GetTabSeparatorColor: TColor;
begin
  Result := $DDDFE1;
end;

procedure TdxCustomRibbon2019Skin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  AdjustTabFont(AFont, AState);
  AFont.Color := TdxColorHelper.ChangeLightness(AContextColor, -0.2);
end;

procedure TdxCustomRibbon2019Skin.DrawContextTabBackground(
  DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor);
begin
  DrawTab(DC, R, AState);
end;

procedure TdxCustomRibbon2019Skin.DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
var
  R1: TRect;
begin
  R1 := R;
  Dec(R1.Bottom, TabGroupsAreaShadowSize - 1);
  inherited DrawGroupScrollButton(DC, R1, ALeft, AState);
end;

procedure TdxCustomRibbon2019Skin.DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean);
var
  R1: TRect;
begin
  if AIsInPopup then
    FillRectByColor(DC, R, GetTabGroupsAreaContentColor)
  else
  begin
    R1 := R;
    Dec(R1.Bottom, TabGroupsAreaShadowSize);
    FillRectByColor(DC, R1, GetTabGroupsAreaContentColor);

    R1.Top := R1.Bottom;
    R1.Bottom := R.Bottom;
    if AIsQATAtBottom then
      DrawFrame(DC, R1, GetTabGroupsAreaContentColor, GetFrameColor, [bTop], 2)
    else
      DrawTabGroupsAreaShadow(DC, R1);
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawTabGroupsAreaShadow(DC: HDC; const R: TRect);
begin
  FillGradientRect(DC, R, GetGroupShadowColor1, GetGroupShadowColor2, False);
  FillRectByColor(DC, cxRectSetHeight(R, 1), GetGroupBottomSeparatorColor);
end;

function TdxCustomRibbon2019Skin.GetGroupBottomSeparatorColor: TColor;
begin
  Result := GetFrameColor;
end;

function TdxCustomRibbon2019Skin.GetGroupShadowColor1: TColor;
begin
  Result := GetGroupBottomSeparatorColor;
end;

function TdxCustomRibbon2019Skin.GetGroupShadowColor2: TColor;
begin
  Result := GetFormBackgroundColor1;
end;

function TdxCustomRibbon2019Skin.GetMasterDarkColor: TColor;
begin
  Result := GetMasterColor;
end;

procedure TdxCustomRibbon2019Skin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_CHECKED, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
      dxGpFillRect(DC, R, GetHighlightContentColor);
    DXBAR_DROPPEDDOWN, DXBAR_PRESSED:
      dxGpFillRect(DC, R, GetHighlightBorderColor);
    DXBAR_HOTCHECK:
      DrawFrame(DC, R, GetHighlightContentColor, GetHighlightBorderColor, cxBordersAll, 1, True);
  end;
end;

procedure TdxCustomRibbon2019Skin.DrawQuickAccessToolbar(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean);
begin
  if ABellow then
    DrawTabGroupsArea(DC, R, False, False);
end;

procedure TdxCustomRibbon2019Skin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  Inc(R1.Top);
  DrawTabGroupsAreaShadow(DC, R1);
end;

function TdxCustomRibbon2019Skin.GetRibbonTopFrameAreaSeparatorSize: Integer;
begin
  Result := TabGroupsAreaShadowSize + 1;
end;

function TdxCustomRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  case APart of
    rtatpEditText:
      Result := GetPartColor(rspTabHeaderText, AState);

    rtatpEditBackground:
      begin
        Result := GetTabGroupsAreaContentColor;
        if AState = DXBAR_HOT then
          Result := TdxColorHelper.ChangeLightness(Result, 0.08);
      end;

    DXRIBBON_TAT_SMALLBUTTON:
      begin
        Result := GetAccentColor;
        if AState = DXBAR_DISABLED then
          Result := TdxColorHelper.ChangeSaturation(Result, -1);
      end;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM:
      case AState of
        DXBAR_HOTCHECK, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := GetHighlightContentColor;
        DXBAR_CHECKED, DXBAR_DROPPEDDOWN, DXBAR_PRESSED:
          Result := GetHighlightBorderColor;
      else
        Result := clNone;
      end;

    DXBAR_GALLERYFILTERBAND:
      Result := GetPartColor(DXBAR_GALLERYGROUPHEADERBACKGROUND);

    DXBAR_GALLERYGROUPHEADERBACKGROUND,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := TdxColorHelper.MultiplyLightness(GetPartColor(DXBAR_EDIT_BACKGROUND), 0.9);
  else
    Result := inherited;
  end;
end;

function TdxCustomRibbon2019Skin.GetPartContentOffsets(APart: Integer): TRect;
begin
  case APart of
    DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
      Result := ScaleFactor.Apply(cxRect(9, 2, 9, 3));
    DXRIBBON_TAT_SMALLBUTTON:
      Result := ScaleFactor.Apply(cxRect(8, 4, 8, 4));
    DXBAR_BACKSTAGEVIEW_MENUBAR:
      Result := ScaleFactor.Apply(cxRect(0, 0, 0, 17));
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM, DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      Result := ScaleFactor.Apply(cxRect(20, 11, 20, 11));
    DXBAR_RIBBONTABGROUP, DXBAR_RIBBONCONTEXTTABGROUP:
      begin
        Result := inherited;
        Inc(Result.Bottom, TabGroupsAreaShadowSize);
      end;
    DXBAR_QUICKACCESSTOOLBAR:
      begin
        Result.Top := 2;
        Result.Left := TabGroupsAreaShadowSize - 1;
        Result.Bottom := Result.Left + TabGroupsAreaShadowSize;
        Result.Right := Result.Left;
      end;
  else
    Result := inherited;
  end;
end;

function TdxCustomRibbon2019Skin.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_BACKBUTTON:
      Result := GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB);
    DXBAR_BACKSTAGEVIEW_BACKBUTTON_OFFSET:
      Result := 0;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
      Result := 1 + 2 * BackstageViewGalleryItemIndent;
    DXBAR_TABSGROUPSOVERLAPHEIGHT:
      Result := 0;
  else
    Result := inherited;
  end;
end;

function TdxCustomRibbon2019Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2019;
end;

procedure TdxCustomRibbon2019Skin.InitializeColorPalettes;
begin
  inherited;
  FColorPaletteTabAreaToolbar := CreateTabsAreaColorPalette;
end;

procedure TdxCustomRibbon2019Skin.UpdateBitsPerPixel;
begin
  // do nothing
end;

{ TdxCustomColorfulRibbon2019Skin }

procedure TdxCustomColorfulRibbon2019Skin.DrawApplicationButton(
  DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
const
  StateMap: array[TdxRibbonApplicationButtonState] of TdxRibbonTabState = (rtsNormal, rtsActiveHot, rtsActiveHot);
begin
  if AState = rabsPressed then
    FillRectByColor(DC, R, GetFormCaptionAreaColor)
  else
    DrawTab(DC, R, StateMap[AState]);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_MENUEXTRAPANE));
  FillRectByColor(DC, cxRectSetBottom(R, AContentRect.Top, AContentRect.Top - R.Top), GetFormCaptionAreaColor);
  DrawFrame(DC, R, clNone, GetFormBorderColor(True));
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
      DrawFrame(DC, R, clNone, GetHighlightContentColor, [bTop..bBottom], 1, True);
    else
      DrawSmallButton(DC, R, AState);
  end;
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
      DrawFrame(DC, R, clNone, GetHighlightContentColor, [bLeft, bTop, bBottom], 1, True);
    else
      DrawSmallButton(DC, R, AState);
  end;
end;

procedure TdxCustomColorfulRibbon2019Skin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
begin
  AFont.Color := GetPartColor(rspFormCaptionText, DXBAR_NORMAL);
end;

procedure TdxCustomColorfulRibbon2019Skin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  AdjustTabFont(AFont, AState);
  AFont.Color := GetPartColor(rspTabHeaderText, AState);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawTabBase(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetMasterDarkColor);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL));
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_CHECKED:
      AState := DXBAR_NORMAL;
    DXBAR_HOTCHECK:
      AState := DXBAR_HOT;
  end;
  DrawBackstageViewGalleryItem(DC, R, AState);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarArrowDown(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if ABelow then
    inherited
  else
    DrawArrow(DC, R, adDown, GetQuickAccessToolbarMarkColor(AState));
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarMark(
  DC: HDC; const R: TRect; AState: Integer; AStatesArray: TStatesArray);
begin
  InternalDrawGlyph(DC, R, AStatesArray[AState], GetQuickAccessToolbarMarkColor(AState));
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarMarkArrow(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if ABelow then
    inherited
  else
    DrawQuickAccessToolbarMark(DC, R, AState, FMarkArrow);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarMarkTruncated(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if ABelow then
    inherited
  else
    DrawQuickAccessToolbarMark(DC, R, AState, FMarkTruncated);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  DrawFrame(DC, R, GetTabGroupsAreaContentColor, $777777);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if PaintData.IsQuickAccessToolbarBelowRibbon then
    inherited
  else
    DrawColorfulButton(DC, R, AState);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(
  DC: HDC; const R: TRect; AState: Integer);
begin
  DrawQuickAccessToolbarSmallButton(DC, R, AState);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawQuickAccessToolbarSmallButtonDropButtonMainPart(
  DC: HDC; const R: TRect; AState: Integer);
begin
  DrawQuickAccessToolbarSmallButton(DC, R, AState);
end;

procedure TdxCustomColorfulRibbon2019Skin.DrawStatusBar(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetTabGroupsAreaContentColor);
end;

function TdxCustomColorfulRibbon2019Skin.GetQuickAccessToolbarMarkColor(AState: Integer): Integer;
begin
  case AState of
    DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED, DXBAR_DROPPEDDOWN, DXBAR_CHECKED, DXBAR_HOTCHECK:
      Result := $FFFFFF;
  else
    Result := $EFEFEF;
  end;
end;

function TdxCustomColorfulRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
const
  StateMap: array[Boolean] of TdxRibbonBorderIconState = (rbisInactive, rbisNormal);
begin
  case APart of
    DXBAR_MENUEXTRAPANE:
      Result := GetPartColor(DXBAR_MENUCONTENT);
    DXBAR_BACKSTAGEVIEW, DXBAR_BACKSTAGEVIEW_GALLERYCONTROL:
      Result := GetTabGroupsAreaContentColor;
    DXBAR_GALLERYFILTERBANDTEXT,
    DXBAR_GALLERYGROUPHEADERTEXT,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
      Result := $262626;
    rspStatusBarText:
      Result := GetPartColor(DXBAR_ITEMTEXT, AState);
    rspDocumentNameText, rspFormCaptionText:
      Result := GetFormBorderIconGlyphColor(rbdiHelp, StateMap[AState = DXBAR_NORMAL]);

    rspTabHeaderText:
      if AState = DXBAR_DISABLED then
        Result := $888888
      else
        Result := $232425;

    rspApplicationButton:
      if AState = DXBAR_PRESSED then
        Result := GetPartColor(rspFormCaptionText)
      else
        Result := GetPartColor(rspTabHeaderText, AState);
  else
    Result := inherited;
  end;
end;

function TdxCustomColorfulRibbon2019Skin.GetTabAreaBackgroundColor(AApplicationMenuState: TdxRibbonApplicationMenuState): TColor;
begin
  Result := GetTabGroupsAreaContentColor;
end;

function TdxCustomColorfulRibbon2019Skin.GetColorfulButtonColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := TdxColorHelper.ChangeLightness(GetMasterDarkColor, 0.08);
    DXBAR_PRESSED:
      Result := TdxColorHelper.ChangeLightness(GetMasterDarkColor, -0.08);
  else
    Result := clNone;
  end;
end;

function TdxCustomColorfulRibbon2019Skin.GetFormBorderColor(AActive: Boolean): TColor;
begin
  if AActive then
    Result := GetMasterDarkColor
  else
    Result := $848484;
end;

function TdxCustomColorfulRibbon2019Skin.GetHighlightBorderColor: TColor;
begin
  Result := FHighlightBorderColor;
end;

function TdxCustomColorfulRibbon2019Skin.GetHighlightContentColor: TColor;
begin
  Result := FHighlightContentColor;
end;

procedure TdxCustomColorfulRibbon2019Skin.InitializeColorPalettes;
var
  AValue: TdxHSL;
begin
  inherited;
  FColorPaletteQAT := FColorPaletteBackstageViewMenu;

  AValue := TdxColorSpaceConverter.ColorToHSL(GetTabGroupsAreaContentColor);
  AValue.S := Max(AValue.S - 0.035, 0);
  FHighlightContentColor := TdxColorSpaceConverter.HSLToColor(AValue.H, AValue.S, Max(AValue.L - 0.14, 0));
  FHighlightBorderColor := TdxColorSpaceConverter.HSLToColor(AValue.H, AValue.S, Max(AValue.L - 0.25, 0));
end;

{ TdxCustomGrayScaleRibbon2019Skin }

procedure TdxCustomGrayScaleRibbon2019Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
      FillRectByColor(DC, R, GetAccentColor);
    DXBAR_CHECKED:
      FillRectByColor(DC, R, GetAccentColor(-0.08));
  end;
end;

procedure TdxCustomGrayScaleRibbon2019Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  R1: TRect;
begin
  R1 := R;
  Inc(R1.Top);
  dxGpFillRect(DC, R1, GetMenuBackgroundColor, 128);
end;

procedure TdxCustomGrayScaleRibbon2019Skin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  case AState of
    rtsActiveHot, rtsFocused, rtsHot:
      FillRectByColor(DC, R, TdxColorHelper.ChangeLightness(GetTabGroupsAreaContentColor, 0.05));
  end;
end;

procedure TdxCustomGrayScaleRibbon2019Skin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
const
  Map: array[TcxButtonState] of Single = (0.18, 0.18, 0.26, -0.1, 0.073);
var
  AColor: TColor;
begin
  AColor := GetTabGroupsAreaContentColor;
  DrawTabAreaButtonCore(DC, R,
    dxColorToAlphaColor(TdxColorHelper.ChangeLightness(AColor, Map[cxbsPressed])),
    dxColorToAlphaColor(TdxColorHelper.ChangeLightness(AColor, Map[AState])));
end;

function TdxCustomGrayScaleRibbon2019Skin.GetScrollBarGlyphColor: TColor;
begin
  Result := GetMasterDarkColor;
end;

function TdxCustomGrayScaleRibbon2019Skin.GetScrollBarBackgroundColor: TColor;
begin
  Result := GetMasterColor;
end;

function TdxCustomGrayScaleRibbon2019Skin.GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  Result := GetMasterDarkColor;
end;

function TdxCustomGrayScaleRibbon2019Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
var
  ADelta: Single;
begin
  case AState of
    DXBAR_HOT:
      ADelta := 0.3;
    DXBAR_PRESSED:
      ADelta := 0.36;
    DXBAR_DISABLED:
      ADelta := -0.24;
  else
    ADelta := 0.2;
  end;
  Result := TdxColorHelper.ChangeLightness(GetMasterColor, ADelta);
end;

function TdxCustomGrayScaleRibbon2019Skin.CreateTabsAreaColorPalette: IdxRibbonSkinColorPaletteSet;
var
  APalette: TdxRibbonSkinColorPaletteSet;
begin
  APalette := TdxRibbonSkinColorPaletteSet.Create;
  APalette.Add(DXBAR_NORMAL, TdxSimpleColorPalette.Create(
    dxColorToAlphaColor(GetPartColor(rtatpEditText, DXBAR_NORMAL)), TdxAlphaColors.Empty));
  APalette.Add(DXBAR_DISABLED, TdxSimpleColorPalette.Create(
    dxColorToAlphaColor(GetPartColor(rtatpEditText, DXBAR_DISABLED)), TdxAlphaColors.Empty));
  Result := APalette;
end;

function TdxCustomGrayScaleRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := $D2D2D2;
    DXBAR_BACKSTAGEVIEW, DXBAR_BACKSTAGEVIEW_GALLERYCONTROL:
      Result := GetMasterColor;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH,
    DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
      Result := GetPartColor(rspFormCaptionText);
  else
    Result := inherited;
  end;
end;

function TdxCustomGrayScaleRibbon2019Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  if AState = rbisInactive then
    Result := TdxColorSpaceConverter.HSLToColor(TdxColorSpaceConverter.ColorToHSL(GetFormCaptionAreaColor).H, 0, 0.8)
  else
    Result := clWhite;
end;

{ TdxColorfulRibbon2019Skin }

function TdxColorfulRibbon2019Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  if (PaintData.GetApplicationMenuState = ramsShownAsFullScreenFrame) and (AState in [rbisNormal, rbisInactive]) then
    Result := inherited
  else
    if AState = rbisInactive then
      Result := TdxColorSpaceConverter.HSLToColor(TdxColorSpaceConverter.ColorToHSL(GetFormCaptionAreaColor).H, 0, 0.8)
    else
      Result := clWhite;
end;

function TdxColorfulRibbon2019Skin.GetFormCaptionAreaColor(
  AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor;
begin
  if AApplicationMenuState = ramsShownAsFullScreenFrame then
    Result := GetPartColor(DXBAR_BACKSTAGEVIEW)
  else
    Result := GetMasterDarkColor;
end;

function TdxColorfulRibbon2019Skin.GetFrameColor: TColor;
begin
  Result := $ADB0B3;
end;

function TdxColorfulRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
      Result := $E1E1E1;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM:
      case AState of
        DXBAR_HOT:
          Result := GetAccentColor(0.5);
        DXBAR_PRESSED, DXBAR_CHECKED, DXBAR_HOTCHECK:
          Result := GetAccentColor(0.45);
      else
        Result := clNone;
      end;

  else
    Result := inherited;
  end;
end;

procedure TdxColorfulRibbon2019Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
      FillRectByColor(DC, R, GetAccentColor(0.08));
    DXBAR_CHECKED:
      FillRectByColor(DC, R, GetAccentColor(-0.08));
  end;
end;

procedure TdxColorfulRibbon2019Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  R1: TRect;
begin
  R1 := R;
  Inc(R1.Top);
  dxGpFillRect(DC, R1, clBlack, 77);
end;

procedure TdxColorfulRibbon2019Skin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
var
  AContentColor: TColor;
begin
  case AState of
    cxbsHot:
      AContentColor := $DDDFE1;
    cxbsPressed:
      AContentColor := $CED0D2;
  else
    AContentColor := clWhite;
  end;
  DrawTabAreaButtonCore(DC, R, dxColorToAlphaColor(GetTabSeparatorColor), dxColorToAlphaColor(AContentColor));
end;

function TdxColorfulRibbon2019Skin.GetGroupBottomSeparatorColor: TColor;
begin
  Result := $CED0D2;
end;

function TdxColorfulRibbon2019Skin.GetGroupShadowColor1: TColor;
begin
  Result := $D3D3D3;
end;

function TdxColorfulRibbon2019Skin.GetGroupScrollButtonBorderColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_PRESSED, DXBAR_HOT:
      Result := $878787;
  else
    Result := $C4C4C4
  end;
end;

function TdxColorfulRibbon2019Skin.GetGroupScrollButtonContentColor(AState: Integer): TColor;
begin
  if AState = DXBAR_PRESSED then
    Result := $F3F3F3
  else
    Result := $FFFFFF;
end;

function TdxColorfulRibbon2019Skin.GetMasterColor: TColor;
begin
  Result := $E6E6E6;
end;

function TdxColorfulRibbon2019Skin.GetMasterDarkColor: TColor;
begin
  Result := GetAccentColor;
end;

function TdxColorfulRibbon2019Skin.GetName: string;
begin
  Result := 'Colorful';
end;

function TdxColorfulRibbon2019Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $F1F2F3;
end;

{ TdxBlackRibbon2019Skin }

procedure TdxBlackRibbon2019Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_PRESSED, DXBAR_HOT:
      FillRectByColor(DC, R, $444444);
  end;
  FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), GetFrameColor);
end;

procedure TdxBlackRibbon2019Skin.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawFrame(DC, R, $1E1E1E, $686868);
end;

procedure TdxBlackRibbon2019Skin.DrawTabGroupBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R1: TRect;
begin
  R1 := cxRectSetRight(R, R.Right, 1);
  if UseRightToLeftAlignment then
    R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
  FillRectByColor(DC, R1, GetFrameColor);
end;

procedure TdxBlackRibbon2019Skin.DrawTabGroupHeaderBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R1: TRect;
begin
  R1 := cxRectSetRight(R, R.Right, 1);
  if UseRightToLeftAlignment then
    R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
  R1 := cxRectSetHeight(R1, cxRectHeight(R1) - 4);
  FillRectByColor(DC, R1, GetFrameColor);
end;

procedure TdxBlackRibbon2019Skin.DrawSeparatorBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $4A4A4A);
end;

procedure TdxBlackRibbon2019Skin.DrawTabBase(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clWhite);
end;

function TdxBlackRibbon2019Skin.GetGroupBottomSeparatorColor: TColor;
begin
  Result := $191919;
end;

function TdxBlackRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  case APart of
    DXBAR_SEPARATOR_TEXTCOLOR:
      Result := $EBEBEB;
    DXBAR_MENUCONTENT:
      Result := $363636;
    DXBAR_EDIT_BACKGROUND:
      Result := $444444;
    DXBAR_MENUARROWSEPARATOR, DXBAR_MENUSEPARATORHORZ, DXBAR_MENUSEPARATORVERT:
      Result := $4E4E4E;
    DXRIBBON_TAT_SMALLBUTTON:
      Result := GetPartColor(rspTabHeaderText, AState);
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      Result := $B1B1B1;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := $363636;
    DXBAR_GALLERYGROUPHEADERBACKGROUND:
      Result := $5A5A5A;
    DXBAR_GALLERYFILTERBANDTEXT, DXBAR_GALLERYGROUPHEADERTEXT, DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
      Result := clWhite;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM:
      case AState of
        DXBAR_HOT:
          Result := GetAccentColor;
        DXBAR_PRESSED, DXBAR_CHECKED, DXBAR_HOTCHECK:
          Result := GetAccentColor(-0.08);
      else
        Result := clNone;
      end;

    rspGroupScrollArrow:
      Result := clWhite;
    rspTabGroupHeaderText:
      Result := clWhite;
    rspTabHeaderText, DXBAR_ITEMTEXT:
      if AState = DXBAR_DISABLED then
        Result := $888888
      else
        Result := clWhite;

    DXBAR_ARROWDOWN, DXBAR_EDIT_TEXTCOLOR:
      if AState = DXBAR_DISABLED then
        Result := $888888
      else
        Result := $DADADA;

    DXBAR_MENUITEMTEXT:
      case AState of
        DXBAR_HOT:
          Result := clWhite;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $888888;
      else
        Result := $F0F0F0;
      end;

    DXBAR_EDIT_BORDER, DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL, DXBAR_DISABLED:
          Result := $686868;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $B5B5B5;
      else
        Result := inherited;
      end;
  else
    Result := inherited;
  end;
end;

function TdxBlackRibbon2019Skin.GetColorfulButtonColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := $444444;
    DXBAR_PRESSED:
      Result := $969696;
  else
    Result := clNone;
  end;
end;

function TdxBlackRibbon2019Skin.GetFrameColor: TColor;
begin
  Result := $444648;
end;

function TdxBlackRibbon2019Skin.GetHighlightBorderColor: TColor;
begin
  Result := $6C6C6C;
end;

function TdxBlackRibbon2019Skin.GetHighlightContentColor: TColor;
begin
  Result := $505050;
end;

function TdxBlackRibbon2019Skin.GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor;
begin
  Result := clWhite;
end;

function TdxBlackRibbon2019Skin.GetMasterColor: TColor;
begin
  Result := $262626;
end;

function TdxBlackRibbon2019Skin.GetMasterDarkColor: TColor;
begin
  Result := $0A0A0A;
end;

function TdxBlackRibbon2019Skin.GetMenuBackgroundColor: TColor;
begin
  Result := $363636;
end;

function TdxBlackRibbon2019Skin.GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor;
begin
  Result := clWhite;
end;

function TdxBlackRibbon2019Skin.GetName: string;
begin
  Result := 'Black';
end;

function TdxBlackRibbon2019Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $303132;
end;

function TdxBlackRibbon2019Skin.GetTabSeparatorColor: TColor;
begin
  Result := GetFrameColor;
end;

{ TdxDarkGrayRibbon2019Skin }

procedure TdxDarkGrayRibbon2019Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_PRESSED, DXBAR_HOT:
      FillRectByColor(DC, R, $9F9F9F);
  end;
  FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), GetFrameColor);
end;

procedure TdxDarkGrayRibbon2019Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  DrawFrame(DC, R, clWhite, $777777);
end;

function TdxDarkGrayRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
      Result := $C6C6C6;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := $505050;
    DXBAR_MENUCONTENT:
      Result := $F0F0F0;
    DXBAR_EDIT_BACKGROUND:
      Result := $D4D4D4;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM:
      case AState of
        DXBAR_HOT:
          Result := $575757;
        DXBAR_PRESSED, DXBAR_CHECKED, DXBAR_HOTCHECK:
          Result := $444444;
      else
        Result := clNone;
      end;

    DXBAR_MENUITEMTEXT:
      if AState = DXBAR_HOT then
        Result := clBlack
      else
        Result := inherited;

  else
    Result := inherited;
  end;
end;

function TdxDarkGrayRibbon2019Skin.GetFrameColor: TColor;
begin
  Result := $757779;
end;

function TdxDarkGrayRibbon2019Skin.GetGroupBottomSeparatorColor: TColor;
begin
  Result := GetMasterDarkColor;
end;

function TdxDarkGrayRibbon2019Skin.GetMasterColor: TColor;
begin
  Result := $666666;
end;

function TdxDarkGrayRibbon2019Skin.GetMasterDarkColor: TColor;
begin
  Result := $4444444;
end;

function TdxDarkGrayRibbon2019Skin.GetMenuBackgroundColor: TColor;
begin
  Result := $262626;
end;

function TdxDarkGrayRibbon2019Skin.GetName: string;
begin
  Result := 'DarkGray';
end;

function TdxDarkGrayRibbon2019Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $B8BBBE;
end;

function TdxDarkGrayRibbon2019Skin.GetTabSeparatorColor: TColor;
begin
  Result := $9D9FA1;
end;

{ TdxWhiteRibbon2019Skin }

procedure TdxWhiteRibbon2019Skin.DrawFormCaption(DC: HDC; const R: TRect);
begin
  inherited;
  DrawTabSeparator(DC, cxRectSetBottom(R, R.Bottom, 1), MaxByte);
end;

function TdxWhiteRibbon2019Skin.GetGroupShadowColor1: TColor;
begin
  Result := $EAEAEA;
end;

function TdxWhiteRibbon2019Skin.DoGetPartColor(APart, AState: Integer): TColor;
begin
  if APart = rspTabGroupHeaderText then
    Result := $666666
  else
    Result := inherited;
end;

function TdxWhiteRibbon2019Skin.GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if APart <> sbpThumbnail then
    Result := inherited GetScrollBarPartBorderColor(APart, AState)
  else
    if AState = DXBAR_PRESSED then
      Result := $777777
    else
      Result := $ABABAB;
end;

function TdxWhiteRibbon2019Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  Result := clWhite;
  case AState of
    DXBAR_PRESSED:
      Result := $F0F0F0;
    DXBAR_HOT:
      if APart = sbpThumbnail then
        Result := $F0F0F0;
  end;
end;

procedure TdxWhiteRibbon2019Skin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
var
  AAccentColor: TColor;
  AContentColor: TdxAlphaColor;
  ABorderColor: TColor;
begin
  AAccentColor := GetAccentColor;
  case AState of
    cxbsHot:
      AContentColor := dxColorToAlphaColor(AAccentColor, 25);
    cxbsPressed:
      AContentColor := dxColorToAlphaColor(AAccentColor, 110);
  else
    AContentColor := TdxAlphaColors.Transparent;
  end;

  ABorderColor := AAccentColor;
  if AState = cxbsDisabled then
    ABorderColor := TdxColorHelper.ChangeSaturation(ABorderColor, -1);

  DrawTabAreaButtonCore(DC, R, dxColorToAlphaColor(ABorderColor, 96), AContentColor);
end;

function TdxWhiteRibbon2019Skin.GetColorfulButtonColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := TdxColorHelper.MultiplyLightness(GetMenuBackgroundColor, 2.3);
    DXBAR_PRESSED:
      Result := TdxColorHelper.MultiplyLightness(GetMenuBackgroundColor, 1.7);
  else
    Result := clNone;
  end;
end;

function TdxWhiteRibbon2019Skin.GetFormBackgroundColor2: TColor;
begin
  Result := clWhite;
end;

function TdxWhiteRibbon2019Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  Result := $777777;
  if AIcon = rbdiClose then
    case AState of
      rbisHot, rbisHotInactive, rbisPressed:
        Result := clWhite;
    end;
end;

function TdxWhiteRibbon2019Skin.GetFrameColor: TColor;
begin
  Result := $C4C6C8;
end;

function TdxWhiteRibbon2019Skin.GetMasterColor: TColor;
begin
  Result := clWhite;
end;

function TdxWhiteRibbon2019Skin.GetName: string;
begin
  Result := 'White';
end;

function TdxWhiteRibbon2019Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := clWhite;
end;

{ RegisterSkins }

procedure RegisterSkins;
begin
  if CheckGdiPlus(True) then
  begin
    dxRibbonSkinsManager.Add(TdxColorfulRibbon2019Skin.Create);
    dxRibbonSkinsManager.Add(TdxBlackRibbon2019Skin.Create);
    dxRibbonSkinsManager.Add(TdxDarkGrayRibbon2019Skin.Create);
    dxRibbonSkinsManager.Add(TdxWhiteRibbon2019Skin.Create);
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterSkins, nil);
end.

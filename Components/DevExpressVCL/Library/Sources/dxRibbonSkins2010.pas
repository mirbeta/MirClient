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

unit dxRibbonSkins2010;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Graphics, Forms, dxCore, dxGDIPlusAPI, dxGDIPlusClasses, cxGraphics, cxGeometry,
  dxBar, dxBarSkin, dxBarSkinConsts, cxLookAndFeelPainters, dxRibbonSkins, dxRibbonSkins2007;

type

  { TdxCustomRibbon2010Skin }

  TdxCustomRibbon2010Skin = class(TdxCustomRibbon2007Skin)
  protected
    FBackstageViewFrame: array[TdxRibbonColorSchemeAccent] of Integer;
    FBackstageViewMenuBackground: Integer;
    FBackstageViewMenuButton: array[TdxRibbonColorSchemeAccent] of TTwoStateArray;
    FBackstageViewMenuSeparator: Integer;
    FBackstageViewTabArrow: Integer;
    FBackstageViewTabs: array[TdxRibbonColorSchemeAccent] of TFourStateArray;
    FCloseButton: TThreeStateArray;
    FContextBackgroundMask: Integer;
    FContextTabMaskIndex: array[TdxRibbonTabState] of Integer;
    FItemsSeparator: TTwoStateArray;
    FMDIButtonGlyphs: array[TdxBarMDIButton] of TFourStateArray;
    FMinimizeRibbonButtonGlyph: array[TdxRibbonMinimizeButtonGlyph] of TTwoStateArray;
    FTabsAreaOnGlass: Integer;

    function GetStyle: TdxRibbonStyle; override;
    procedure DrawColoredElement(APartIndex: Integer; DC: HDC; const R: TRect; AColor: TColor);
    procedure DrawFormCaptionSeparator(DC: HDC; const R: TRect); virtual;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor; override;
    // Color Palettes
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; virtual; abstract;
    procedure InitializeColorPalettes; override;
    // Common
    procedure LoadCommonApplicationButton(ABitmap: GpBitmap); virtual;
    procedure LoadCommonBackstageView(ABitmap: GpBitmap); virtual;
    procedure LoadCommonElements(ABitmap: GpBitmap); override;
    procedure LoadCommonMenu(ABitmap: GpBitmap); override;
    procedure LoadCommonTexturesSet(AImage: TdxGPImage); override;
    // Ribbon
    procedure LoadRibbonApplicationButton(ABitmap: GpBitmap); override;
    procedure LoadRibbonBackstageView(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonCollapsedToolbar(ABitmap: GpBitmap); override;
    procedure LoadRibbonContexts(ABitmap: GpBitmap); override;
    procedure LoadRibbonElements(ABitmap: GpBitmap); override;
    procedure LoadRibbonForm(ABitmap: GpBitmap); override;
    procedure LoadRibbonFormBorderIcons(ABitmap: GpBitmap); override;
    procedure LoadRibbonFormBorders(ABitmap: GpBitmap); override;
    procedure LoadRibbonGroup(ABitmap: GpBitmap); override;
    procedure LoadRibbonLaunchButton(ABitmap: GpBitmap); override;
    procedure LoadRibbonMarkArrow(ABitmap: GpBitmap); override;
    procedure LoadRibbonMenuMarks(ABitmap: GpBitmap); override;
    procedure LoadRibbonMinimizeButton(ABitmap: GpBitmap); override;
    procedure LoadRibbonQATBorders(ABitmap: GpBitmap); override;
    procedure LoadRibbonScrollBarsGlyphs(ABitmap: Pointer); override;
    procedure LoadRibbonStatusBar(ABitmap: GpBitmap); override;
    procedure LoadRibbonTab(ABitmap: GpBitmap); override;
  public
    procedure AdjustBackstageViewTabButtonFont(AFont: TFont); override;
    procedure DrawBackstageViewBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;

    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;

    procedure DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect); override;
    procedure DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextTabGroupsArea(DC: HDC; const R: TRect; AContextColor: TColor;
      AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
      AState: TdxRibbonBorderIconState); override;
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    procedure DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawMDIButtonGlyph(DC: HDC; const R: TRect;
      AButton: TdxBarMDIButton; AState: TcxButtonState); override;
    procedure DrawMinimizeRibbonButtonGlyph(DC: HDC; const R: TRect; AState: TcxButtonState;
      AGlyph: TdxRibbonMinimizeButtonGlyph); override;
    procedure DrawQuickAccessToolbar(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean); override;
    procedure DrawQuickAccessToolbarArrowDown(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean); override;
    procedure DrawSeparatorLine(DC: HDC; const R: TRect); override;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    function GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect; override;
    function GetApplicationMenuGlyphSize: TSize; override;
    function GetPartContentOffsets(APart: Integer): TRect; override;
    function GetPartSize(APart: Integer): Integer; override;
    function GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton, ABelow: Boolean): Integer; override;
    function GetQuickAccessToolbarOverrideWidth(AHasApplicationButton, AUseAeroGlass: Boolean): Integer; override;
    function GetRibbonTopFrameAreaSeparatorSize: Integer; override;
    function GetWindowBordersWidth(AHasStatusBar: Boolean): TRect; override;
  end;

  { TdxBlueRibbon2010Skin }

  TdxBlueRibbon2010Skin = class(TdxCustomRibbon2010Skin)
  protected
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetName: string; override;
    procedure DrawFormCaptionSeparator(DC: HDC; const R: TRect); override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive: Boolean; AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); override;
  end;

  { TdxSilverRibbon2010Skin }

  TdxSilverRibbon2010Skin = class(TdxCustomRibbon2010Skin)
  protected
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    procedure DrawFormCaptionSeparator(DC: HDC; const R: TRect); override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    function GetName: string; override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); override;
  end;

  { TdxBlackRibbon2010Skin }

  TdxBlackRibbon2010Skin = class(TdxCustomRibbon2010Skin)
  protected
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    procedure DrawFormCaptionSeparator(DC: HDC; const R: TRect); override;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); override;
    function GetName: string; override;
    procedure InitializeColorPalettes; override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;
  public
    procedure DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive: Boolean; AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); override;
  end;

implementation

uses
  Math;

{$R dxRibbonSkins2010.res}

{ TdxCustomRibbon2010Skin }

procedure TdxCustomRibbon2010Skin.AdjustBackstageViewTabButtonFont(AFont: TFont);
begin
  if AFont.Size > 0 then
    AFont.Size := AFont.Size + 1;
end;

procedure TdxCustomRibbon2010Skin.DrawBackstageViewBackground(DC: HDC; const R: TRect);
begin
  inherited DrawBackstageViewBackground(DC, R);
  if not LowColors then
    Parts[FBackstageViewFrame[ColorSchemeAccent]].Draw(DC, cxRectSetHeight(R, 2), 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2010Skin.DrawBackstageViewMenuBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewMenuBackground(DC, R)
  else
    Parts[FBackstageViewMenuBackground].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2010Skin.DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuButton(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED:
        AState := 0;
      DXBAR_ACTIVEDISABLED:
        AState := 1;
      else
        Exit;
    end;
    Parts[FBackstageViewMenuButton[ColorSchemeAccent][AState = 1]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuSeparator(DC, R, AState)
  else
    Parts[FBackstageViewMenuSeparator].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2010Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
var
  APart: TdxSkinnedRect;
  APartIndex: Integer;
  R1: TRect;
begin
  if LowColors then
  begin
    inherited DrawBackstageViewTabButton(DC, R, AState);
    Exit;
  end;

  case AState of
    DXBAR_HOT, DXBAR_ACTIVE:
      APartIndex := 0;
    DXBAR_CHECKED, DXBAR_PRESSED:
      APartIndex := 1;
    DXBAR_HOTCHECK:
      APartIndex := 2;
    DXBAR_ACTIVEDISABLED:
      APartIndex := 3;
    else
      Exit;
  end;
  if APartIndex >= 0 then
    Parts[FBackstageViewTabs[ColorSchemeAccent][APartIndex]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  if APartIndex in [1, 2] then
  begin
    APart := Parts[FBackstageViewTabArrow];
    R1 := cxRectCenterVertically(R, APart.Size.cy);
    if UseRightToLeftAlignment then
      R1 := cxRectSetLeft(R1, R1.Left, APart.Size.cx)
    else
      R1 := cxRectSetRight(R1, R1.Right, APart.Size.cx);
    APart.Draw(DC, R1, 255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbon2010Skin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
begin
  inherited AdjustContextFont(AFont, AUseGlass, AContextColor);
  AFont.Style := AFont.Style + [fsBold];
end;

procedure TdxCustomRibbon2010Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
begin
  if LowColors then
    inherited DrawContextBackground(DC, R, AContextColor)
  else
  begin
    DrawColoredElement(FContextBackgroundMask, DC, R, AContextColor);
    Parts[FContextBackground].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor);
var
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    DrawContextBackground(DC, cxRectOffset(R, 0, -1), AContextColor);
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawContextTabBackground(DC: HDC; const R: TRect;
  AState: TdxRibbonTabState; AContextColor: TColor);
begin
  if LowColors then
    inherited DrawContextTabBackground(DC, R, AState, AContextColor)
  else
  begin
    DrawColoredElement(FContextTabMaskIndex[AState], DC, R, AContextColor);
    Parts[FContextTabIndex[AState]].Draw(DC, R);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect);
var
  ABorderRect, R1: TRect;
  AHeaderRect, AFooterRect: TRect;
  ABorderColor, ATempColor: TColor;
begin
  R1 := cxRectInflate(R, -2, -2);
  ABorderRect := cxRectInflate(AContentRect, 0, 1, 0, 1);
  AHeaderRect := cxRectSetHeight(R1, ABorderRect.Top - R1.Top);
  AFooterRect := cxRectSetTop(R1, ABorderRect.Bottom, R1.Bottom - ABorderRect.Bottom);

  if LowColors then
  begin
    FillRectByColor(DC, AHeaderRect, clMenu);
    FillRectByColor(DC, AFooterRect, clMenu);
    DrawFrame(DC, R, clMenu, clMenuText);
  end
  else
  begin
    Parts[FApplicationMenuContentHeader].Draw(DC, AHeaderRect, 255, clDefault, UseRightToLeftAlignment);
    Parts[FApplicationMenuContentFooter].Draw(DC, AFooterRect, 255, clDefault, UseRightToLeftAlignment);
    Parts[FApplicationMenuBorder].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  end;

  GetApplicationMenuContentColors(ATempColor, ABorderColor, ATempColor);
  DrawFrame(DC, ABorderRect, clNone, ABorderColor);
end;

procedure TdxCustomRibbon2010Skin.DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2010Skin.DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonArrowPart(DC, R, AState);
end;

procedure TdxCustomRibbon2010Skin.DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxCustomRibbon2010Skin.DrawContextTabGroupsArea(
  DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if AIsInPopup then
    DrawTabGroupsArea(DC, R, AIsQATAtBottom, AIsInPopup)
  else
    inherited DrawContextTabGroupsArea(DC, R, AContextColor, AIsQATAtBottom, AIsInPopup);
end;

procedure TdxCustomRibbon2010Skin.DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
  AState: TdxRibbonBorderIconState);
var
  APart: Integer;
begin
  if LowColors or (AIcon <> rbdiClose) then
    inherited DrawFormBorderIcon(DC, R, AIcon, AState)
  else
  begin
    APart := BorderIconStateToPartIndex[AState];
    if APart >= 0 then
      Parts[FCloseButton[APart]].Draw(DC, R);
    InternalDrawFormBorderIconGlyph(DC, R, AIcon, AState);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawFormCaption(DC: HDC; const R: TRect);
begin
  inherited DrawFormCaption(DC, R);
  if (PaintData.IsRibbonHidden or (PaintData.GetCaptionHeight = PaintData.GetRibbonHeight)) and
    (FormPaintData.GetState <> wsMinimized) then
    DrawFormCaptionSeparator(DC, cxRectSetBottom(R, R.Bottom, 1));
end;

procedure TdxCustomRibbon2010Skin.DrawItemSeparator(
  DC: HDC; const R: TRect; AHorizontal: Boolean);
var
  APart: TdxSkinnedRect;
  R1: TRect;
begin
  if LowColors then
    inherited DrawItemSeparator(DC, R, AHorizontal)
  else
  begin
    APart := Parts[FItemsSeparator[AHorizontal]];
    if AHorizontal then
      R1 := cxRectCenterVertically(R, APart.Size.cy)
    else
      R1 := cxRectCenterHorizontally(R, APart.Size.cx);
    APart.Draw(DC, R1);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawMenuExtraSeparator(
  DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawMenuExtraSeparator(DC, R, AHorizontal)
  else
    InternalDrawSeparator(DC, R, AHorizontal, clWhite, $EBDBCF);
end;

procedure TdxCustomRibbon2010Skin.DrawMDIButtonGlyph(
  DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
const
  PartStateMap: array[TcxButtonState] of Integer = (0, 0, 1, 2, 3);
begin
  InternalDrawGlyph(DC, R, FMDIButtonGlyphs[AButton][PartStateMap[AState]]);
end;

procedure TdxCustomRibbon2010Skin.DrawMinimizeRibbonButtonGlyph(
  DC: HDC; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
begin
  InternalDrawGlyph(DC, R, FMinimizeRibbonButtonGlyph[AGlyph][AState = cxbsDisabled]);
end;

procedure TdxCustomRibbon2010Skin.DrawQuickAccessToolbar(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean);
begin
  if not LowColors then
    inherited DrawQuickAccessToolbar(DC, R, ABellow, ANonClientDraw, False, AIsActive, ABellow);
end;

procedure TdxCustomRibbon2010Skin.DrawQuickAccessToolbarArrowDown(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if LowColors then
    DrawArrow(DC, R, adDown, IfThen(AState = DXBAR_DISABLED, clGrayText, clHighlightText))
  else
    inherited;
end;

procedure TdxCustomRibbon2010Skin.DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean);
var
  R1: TRect;
begin
  if AUseAeroGlass then
  begin
    R1 := R;
    Dec(R1.Bottom);
    Parts[FTabsAreaOnGlass].Draw(DC, R1);
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawSeparatorLine(DC: HDC; const R: TRect);
begin
end;

procedure TdxCustomRibbon2010Skin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
var
  R1: TRect;
begin
  if LowColors then
  begin
    R1 := R;
    Dec(R1.Bottom);
    case AState of
      rtsNormal:
        FillRectByColor(DC, R1, clBtnFace);
      rtsHot:
        DrawFrame(DC, R1, clBtnFace, clBtnText, [bLeft, bTop, bRight]);
      else
        DrawFrame(DC, R1, clHighlight, clBtnShadow, [bLeft, bTop, bRight]);
    end;
  end
  else
    inherited DrawTab(DC, R, AState);
end;

procedure TdxCustomRibbon2010Skin.DrawTabGroupBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
begin
  if not AIsInPopup then
    inherited DrawTabGroupBackground(DC, R, AState, AIsInPopup);
end;

procedure TdxCustomRibbon2010Skin.DrawTabGroupHeaderBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
begin
  if not AIsInPopup then
    inherited DrawTabGroupHeaderBackground(DC, R, AState, AIsInPopup);
end;

function TdxCustomRibbon2010Skin.GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect;
begin
  Result := ScaleFactor.Apply(cxRect(2, 0, 2, 30));
  Result.Top := cxRectHeight(ATabsBounds) + 1;
end;

function TdxCustomRibbon2010Skin.GetApplicationMenuGlyphSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(56, 25));
end;

function TdxCustomRibbon2010Skin.GetPartContentOffsets(APart: Integer): TRect;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW:
      Result := Rect(0, 2, 0, 0);
    DXBAR_APPLICATIONMENUBUTTON:
       Result := Rect(0, 4, 2, 4);
    DXBAR_APPLICATIONBUTTON:
      Result := cxRect(0, 0, ScaleFactor.Apply(4), 0);
    DXBAR_APPLICATIONBUTTONICONOFFSET:
      Result := ScaleFactor.Apply(cxRect(12, 5, 12, 4));
    DXBAR_COLLAPSEDTOOLBAR:
      Result := cxRect(2, 2, 5, 2);
    DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND:
      Result := Rect(7, 7, 7, 7);
    DXBAR_TOOLBAR:
      Result := cxRect(2, 3, 5, 1);
    DXBAR_TOOLBARINPOPUP:
      Result := cxRect(2, 3, 2, 1);
    DXBAR_RIBBONTABGROUP, DXBAR_RIBBONCONTEXTTABGROUP:
      Result := cxRect(1, 3, 3, 4);
    else
      Result := inherited GetPartContentOffsets(APart);
  end;
end;

function TdxCustomRibbon2010Skin.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    DXBAR_SEPARATOR_LINE:
      Result := 0;
    rspContextTabSeparatorBegin, rspContextTabSeparatorEnd:
      Result := 2;
    rspContextTabOverlap:
      Result := 1;
    DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
      Result := 0;
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      if LowColors then
        Result := inherited GetPartSize(APart)
      else
        Result := Parts[FBackstageViewMenuSeparator].Size.cy;
    else
      Result := inherited GetPartSize(APart);
  end;
end;

function TdxCustomRibbon2010Skin.GetQuickAccessToolbarMarkButtonOffset(
  AHasApplicationButton, ABelow: Boolean): Integer;
begin
  Result := 8;
end;

function TdxCustomRibbon2010Skin.GetQuickAccessToolbarOverrideWidth(
  AHasApplicationButton, AUseAeroGlass: Boolean): Integer;
begin
  Result := 4;
end;

function TdxCustomRibbon2010Skin.GetRibbonTopFrameAreaSeparatorSize: Integer;
begin
  Result := 1;
end;

function TdxCustomRibbon2010Skin.GetWindowBordersWidth(AHasStatusBar: Boolean): TRect;
begin
  if LowColors then
    Result := cxRect(3, 0, 3, 3)
  else
    Result := cxRect(8, 0, 8, 8);
end;

function TdxCustomRibbon2010Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2010;
end;

procedure TdxCustomRibbon2010Skin.DrawColoredElement(
  APartIndex: Integer; DC: HDC; const R: TRect; AColor: TColor);
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(R, True);
  try
    Parts[APartIndex].Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect);
    cxMakeColoredBitmap(ABitmap, AColor);
    cxAlphaBlend(DC, ABitmap, R, ABitmap.ClientRect);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxCustomRibbon2010Skin.DrawFormCaptionSeparator(DC: HDC; const R: TRect);
begin
end;

function TdxCustomRibbon2010Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_MENUEXTRAPANE:
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
    DXBAR_DROPDOWNGALLERY:
      Result := clWhite;
    DXBAR_SEPARATOR_BACKGROUND, DXBAR_DATENAVIGATOR_HEADER:
      Result := $F5F2F0;
    DXBAR_INRIBBONGALLERY_BORDER:
      Result := GetPartColor(DXBAR_EDIT_BORDER);
    DXBAR_MINITOOLBAR_BACKGROUND:
      Result := $FFFFFF;

    rspApplicationButton:
      Result := clWhite;

    rspFormCaptionText, rspDocumentNameText, rspTabGroupText:
      Result := $5B391E;

    rspTabHeaderText:
      if AState = DXBAR_DISABLED then
        Result := clGray
      else
        Result := $5B391E;

    rspStatusBarText:
      if AState = DXBAR_DISABLED then
        Result := GetPartColor(DXBAR_ITEMTEXT, DXBAR_DISABLED)
      else
        Result := $5B391E;

    rspContextText:
      Result := clWhite;
    rspContextTextShadow:
      Result := clGray;
    rspContextTextOnGlass:
      Result := $5B391E;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := inherited
      else
        Result := $5B391E;

    DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_HOT:
          Result := $54CCF0;
        DXBAR_DROPPEDDOWN, DXBAR_PRESSED:
          Result := $3888C2;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED, DXBAR_NORMAL:
          Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
        DXBAR_ACTIVE:
          Result := GetPartColor(DXBAR_EDIT_BORDER, DXBAR_ACTIVE);
      else
        Result := inherited;
      end;

    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $AAA6A5;
      else
        Result := clBlack;
      end;

    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $AAA6A5;
        DXBAR_HOTCHECK, DXBAR_CHECKED, DXBAR_PRESSED:
          if ColorSchemeAccent = rcsaYellow then
            Result := clBlack
          else
            Result := clWhite;
      else
        Result := clBlack;
      end;

  else
    Result := inherited;
  end;
end;

function TdxCustomRibbon2010Skin.DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE:
          Result := clHighlightText;
        DXBAR_DISABLED:
          Result := clGrayText;
      else
        Result := clBtnText;
      end;

  else
    Result := inherited;
  end;
end;

procedure TdxCustomRibbon2010Skin.InitializeColorPalettes;
var
  APalette: IdxRibbonSkinColorPaletteSet;
begin
  APalette := CreateMajorColorPalette;

  FColorPaletteBackstageViewMenu := APalette;
  FColorPaletteMenu := APalette;
  FColorPaletteMiniToolbar := APalette;
  FColorPaletteQAT := APalette;
  FColorPaletteRadialMenu := APalette;
  FColorPaletteStatusBar := APalette;
  FColorPaletteTabAreaToolbar := APalette;
  FColorPaletteTabGroup := APalette;
end;

procedure TdxCustomRibbon2010Skin.LoadCommonApplicationButton(ABitmap: GpBitmap);
var
  AIndex: TdxRibbonColorSchemeAccent;
  R: TRect;
begin
  R := cxRectBounds(238, 0, 34, 21);
  for AIndex := Low(TdxRibbonColorSchemeAccent) to High(TdxRibbonColorSchemeAccent) do
  begin
    LoadThreeStateArray(ABitmap, R, cxRect(4, 4, 4, 4), FApplicationButton[AIndex],
      rspApplicationButton + 3 * Ord(AIndex), True, InterpolationModeHighQualityBicubic);
    OffsetRect(R, 0, 3 * cxRectHeight(R));
  end;
end;

procedure TdxCustomRibbon2010Skin.LoadCommonBackstageView(ABitmap: GpBitmap);
var
  AIndex: TdxRibbonColorSchemeAccent;
  R: TRect;
begin
  R := cxRectBounds(0, 398, 360, 2);
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    FBackstageViewFrame[AIndex] := AddPart1x1(ABitmap, R, rbvpBackstageViewFrame + Ord(AIndex));
    OffsetRect(R, 0, cxRectHeight(R));
  end;

  R := cxRectBounds(0, 409, 120, 41);
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    LoadElementParts(ABitmap, FBackstageViewTabs[AIndex],
      R, rbvpBackstageViewMenuTabButton + 4 * Ord(AIndex),
      cxRect(0, 4, 0, 4), [0, 1, 2, 3], [0, 1, 2, 3]);
    OffsetRect(R, cxRectWidth(R), 0);
  end;

  R := cxRectBounds(122, 0, 115, 27);
  for AIndex := Low(AIndex) to High(AIndex) do
  begin
    LoadElementParts(ABitmap, FBackstageViewMenuButton[AIndex], R,
      rbvpBackstageViewMenuItem + 2 * Ord(AIndex), DefaultFixedSize, [0, 1], [0, 1]);
    OffsetRect(R, 0, 2 * cxRectHeight(R));
  end;

  FBackstageViewMenuSeparator := AddPart1x1(ABitmap, cxRectBounds(273, 0, 113, 3), rbvpBackstageViewMenuSeparator);
end;

procedure TdxCustomRibbon2010Skin.LoadCommonElements(ABitmap: GpBitmap);
begin
  inherited LoadCommonElements(ABitmap);
  LoadCommonApplicationButton(ABitmap);
  LoadCommonBackstageView(ABitmap);
end;

procedure TdxCustomRibbon2010Skin.LoadCommonMenu(ABitmap: GpBitmap);
begin
  inherited LoadCommonMenu(ABitmap);
  Parts[FMenuSeparatorHorz].StretchMode := srsmTile;
end;

procedure TdxCustomRibbon2010Skin.LoadCommonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONCOMMON2010', AImage);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonApplicationButton(ABitmap: GpBitmap);
begin
  // nothing to do
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonBackstageView(ABitmap: GpBitmap);
begin
  FBackstageViewMenuBackground := AddPart3x3(ABitmap,
    cxRectBounds(240, 209, 17, 115), cxRect(0, 0, 7, 0), rbvpBackstageViewMenu);
  FBackstageViewTabArrow := AddPart1x1(ABitmap,
    cxRectBounds(175, 208, 15, 30), rbvpBackstageViewMenuTabButtonArrow);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonCollapsedToolbar(ABitmap: GpBitmap);
begin
  LoadElementParts(ABitmap, FCollapsedToolbars, cxRectBounds(94, 177, 68, 68),
    rspCollapsedToolbarNormal, cxRect(0, 0, 3, 3), [0, 1, 3, 2],
    [DXBAR_NORMAL, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED]);
  LoadElementParts(ABitmap, FCollapsedToolbarGlyphBackgrounds,
    cxRectBounds(66, 199, 10, 31), rspCollapsedToolbarGlyphBackgroundNormal,
    DefaultFixedSize, [0, 1, 3, 2], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED]);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonContexts(ABitmap: GpBitmap);
begin
  FContextBackground := AddPart3x3(ABitmap,
    cxRectBounds(25, 439, 17, 25), cxRect(2,9,2,1), rspContextBackground);
  FContextBackgroundMask := AddPart3x3(ABitmap,
    cxRectBounds(8, 439, 17, 25), cxRect(2, 9, 2, 1), rspContextBackgroundMask);
  LoadElementParts(ABitmap, FContextTabSeparator, cxRectBounds(0, 440, 2, 16),
    rspContextTabSeparatorBegin, cxNullRect, [0, 1], [0, 1], False);
  LoadElementParts(ABitmap, FContextTabGroupsArea, cxRectBounds(12, 250, 5, 92),
    rspContextTabGroupsArea, cxRect(0, 17, 0, 7), [0, 1], [0, 1], False);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonElements(ABitmap: GpBitmap);
begin
  inherited LoadRibbonElements(ABitmap);
  FItemsSeparator[False] := AddPart1x1(ABitmap, cxRectBounds(24, 320, 3, 22), rspItemSeparatorVertical);
  FItemsSeparator[True] := AddPart1x1(ABitmap, cxRectBounds(12, 343, 22, 3), rspItemSeparatorHorizontal);
  LoadRibbonBackstageView(ABitmap);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonForm(ABitmap: GpBitmap);
begin
  LoadRibbonMinimizeButton(ABitmap);
  LoadRibbonFormBorders(ABitmap);
  LoadRibbonFormBorderIcons(ABitmap);
  FRibbonTopArea := AddPart3x3(ABitmap, cxRectBounds(169, 38, 2, 30), cxRect(0, 9, 0, 5), rspRibbonClientTopArea);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonFormBorderIcons(ABitmap: GpBitmap);

  procedure DoLoad(X, Y, W, H: Integer);
  begin
    LoadRibbonFormBorderIconsGlyphs(ABitmap, X + W + 1, Y, W, H);

    LoadElementParts(ABitmap, FMDIButtonGlyphs[mdibMinimize], cxRectBounds(X + W + 1, Y, W, H),
      rspMDIButtonMinimize, cxNullRect, [0, 1, 2, 3], [0, 1, 2, 3], True, InterpolationModeNearestNeighbor);
    LoadElementParts(ABitmap, FMDIButtonGlyphs[mdibRestore], cxRectBounds(X + (W + 1) * 3, Y, W, H),
      rspMDIButtonRestore, cxNullRect, [0, 1, 2, 3], [0, 1, 2, 3], True, InterpolationModeNearestNeighbor);
    LoadElementParts(ABitmap, FMDIButtonGlyphs[mdibClose], cxRectBounds(X, Y, W, H),
      rspMDIButtonClose, cxNullRect, [0, 1, 2, 3], [0, 1, 2, 3], True, InterpolationModeNearestNeighbor);
  end;

begin
  LoadElementParts(ABitmap, FBorderIcons, cxRectBounds(195, 37, 17, 17), rfspBorderIconBackground,
    DefaultFixedSize, [0, 1, 2], [0, 1, 2], True, InterpolationModeNearestNeighbor);
  LoadElementParts(ABitmap, FCloseButton, cxRectBounds(213, 37, 17, 17), rfspCloseButtonHot,
    DefaultFixedSize, [0, 1, 2], [0, 1, 2], True, InterpolationModeNearestNeighbor);

  if TargetDPI >= 192 then
    DoLoad(108, 580, 26, 22)
  else if TargetDPI >= 144 then
    DoLoad(144, 511, 20, 17)
  else
    DoLoad(144, 466, 13, 11);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonFormBorders(ABitmap: GpBitmap);
var
  R: TRect;
begin
  // Caption
  AddTwoStateElement(ABitmap, FCaption, cxRectBounds(163, 37, 14, 31), cxRect(6, 10, 6, 5), rfspActiveCaption);
  AddTwoStateElement(ABitmap, FCaptionZoomed, cxRectBounds(169, 37, 2, 31), cxRect(0, 10, 0, 5), rfspActiveCaptionZoomed);
  R := cxRectBounds(163, 37, 8, 31);
  AddTwoStateElement(ABitmap, FCaptionLeftBorder, R, cxRect(0, 9, 0, 2), rfspActiveCaptionLeftBorder);
  OffsetRect(R, 6, 0);
  AddTwoStateElement(ABitmap, FCaptionRightBorder, R, cxRect(0, 9, 0, 2), rfspActiveCaptionRightBorder);
  //active border
  R := cxRectBounds(178, 37, 8, 11);
  AddTwoStateElement(ABitmap, FLeftBorder, R, cxRect(0, 0, 0, 7), rfspActiveLeftBorder);
  OffsetRect(R, 8, 0);
  AddTwoStateElement(ABitmap, FRightBorder, R, cxRect(0, 0, 0, 7), rfspActiveRightBorder);
  //bottom border
  AddTwoStateElement(ABitmap, FBottomBorderThin, cxRectBounds(178, 60, 3, 1), cxEmptyRect, rfspActiveBottomBorderThin);
  AddTwoStateElement(ABitmap, FBottomBorderThick[False], cxRectBounds(178, 67, 16, 8), cxRect(7, 0, 7, 0), rfspActiveBottomBorderThick);
  AddTwoStateElement(ABitmap, FBottomBorderThick[True], cxRectBounds(178, 83, 16, 8), cxRect(7, 0, 7, 0), rfspActiveBottomBorderThickRectangular);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonGroup(ABitmap: GpBitmap);
begin
  LoadElementParts(ABitmap, FTabGroupsArea, cxRectBounds(13, 116, 5, 92), rspTabGroupsArea, cxRect(0, 17, 0, 7), [0, 1], [0, 1], False);
  FTabGroupsArea[2] := AddPart3x3(ABitmap, cxRectBounds(24, 116, 5, 89), cxRect(2, 17, 2, 7), rspTabGroupsAreaInPopup);

  LoadElementParts(ABitmap, FToolbar, cxRectBounds(94, 0, 68, 68), rspToolbarNormal, cxRect(0, 0, 3, 0), [], [DXBAR_NORMAL, DXBAR_HOT]);
  LoadElementParts(ABitmap, FToolbarHeader, cxRectBounds(94, 136, 68, 20), rspToolbarHeaderNormal, cxRect(0, 0, 3, 2), [], [DXBAR_NORMAL, DXBAR_HOT]);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonLaunchButton(ABitmap: GpBitmap);
var
  R: TRect;
begin
  if TargetDPI >= 192 then
    R := cxRectBounds(195, 185, 24, 24)
  else if TargetDPI >= 144 then
    R := cxRectBounds(176, 185, 18, 18)
  else
    R := cxRectBounds(163, 185, 12, 12);

  LoadElementParts(ABitmap, FLaunchButtonDefaultGlyphs, R, rspLaunchButtonDefaultGlyphNormal, cxNullRect,
    [0, 0, 0, 0, 0], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED], True, 5);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonMarkArrow(ABitmap: GpBitmap);
var
  R: TRect;
begin
  if TargetDPI >= 192 then
    R := cxRectBounds(231, 60, 14, 14)
  else if TargetDPI >= 144 then
    R := cxRectBounds(239, 37, 11, 11)
  else
    R := cxRectBounds(231, 37, 7, 7);

  LoadElementParts(ABitmap, FMarkArrow, R, rspMarkArrowNormal, cxEmptyRect, [0, 0, 1], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED], True);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonMenuMarks(ABitmap: GpBitmap);

  procedure DoLoadMarkTruncated(const R: TRect);
  begin
    LoadElementParts(ABitmap, FMarkTruncated, R, rspMarkTruncatedNormal,
      cxEmptyRect, [0, 0, 1], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED], True);
  end;

  procedure DoLoadMarkArrows(const DownArrow, RightArrow: TRect);
  begin
    FMenuArrowDown := AddPart1x1(ABitmap, DownArrow, rspMenuArrowDown);
    FMenuArrowRight := AddPart1x1(ABitmap, RightArrow, rspMenuArrowRight);
  end;

begin
  FMenuMark := AddPart1x1(ABitmap, cxRectBounds(48, 277, 17, 17), rspMenuMark);

  if TargetDPI >= 192 then
  begin
    DoLoadMarkArrows(cxRectBounds(218, 134, 18, 12), cxRectBounds(233, 115, 12, 18));
    DoLoadMarkTruncated(cxRectBounds(222, 89, 16, 12));
  end
  else
    if TargetDPI >= 144 then
    begin
      DoLoadMarkArrows(cxRectBounds(218, 125, 14, 9), cxRectBounds(200, 104, 9, 14));
      DoLoadMarkTruncated(cxRectBounds(209, 89, 12, 9))
    end
    else
    begin
      DoLoadMarkArrows(cxRectBounds(218, 119, 9, 6), cxRectBounds(209, 109, 6, 9));
      DoLoadMarkTruncated(cxRectBounds(200, 89, 8, 6));
    end;
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonMinimizeButton(ABitmap: GpBitmap);

  procedure DoLoadRibbonMinimizeButton(X, Y, W, H: Integer);
  begin
    LoadElementParts(ABitmap, FMinimizeRibbonButtonGlyph[rmbMinimize], cxRectBounds(X, Y, W, H),
      rspMinimizeRibbonButtonMinimize, cxNullRect, [0, 1], [0, 1], True, InterpolationModeNearestNeighbor);
    LoadElementParts(ABitmap, FMinimizeRibbonButtonGlyph[rmbRestore], cxRectBounds(X + W, Y, W, H),
      rspMinimizeRibbonButtonRestore, cxNullRect, [0, 1], [0, 1], True, InterpolationModeNearestNeighbor);
    LoadElementParts(ABitmap, FMinimizeRibbonButtonGlyph[rmbPin], cxRectBounds(X + 2 * W, Y, W, H),
      rspMinimizeRibbonButtonPin, cxNullRect, [0, 1], [0, 1], True, InterpolationModeNearestNeighbor);
  end;

begin
  if TargetDPI >= 192 then
    DoLoadRibbonMinimizeButton(163, 148, 24, 18)
  else if TargetDPI >= 144 then
    DoLoadRibbonMinimizeButton(163, 119, 18, 14)
  else
    DoLoadRibbonMinimizeButton(163, 100, 12, 9);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonQATBorders(ABitmap: GpBitmap);
begin
  FQATAtBottom := AddPart3x3(ABitmap, cxRectBounds(13, 209, 10, 26), cxRect(3, 3, 3, 3), rspQATAtBottom);
  FQATAtTopLeft[True][True] := AddPart3x3(ABitmap, cxRectBounds(252, 110, 5, 21), cxRect(3, 4, 0, 4), rspQATNonClientLeft1Active);
  FQATAtTopLeft[True][False] := FQATAtTopLeft[True][True];
  FQATAtTopLeft[False] := FQATAtTopLeft[True];
  FQATAtTopRight[True] := AddPart3x3(ABitmap, cxRectBounds(250, 110, 5, 21), cxRect(0, 4, 3, 4), rspQATNonClientRightActive);
  FQATAtTopRight[False] := FQATAtTopRight[True];
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonScrollBarsGlyphs(ABitmap: Pointer);

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
    DoLoadButtonsGlyphs(234, 354, 17, 17);
    DoLoadThumbGlyphs(197, 403, 21, 21);
  end
  else
    if TargetDPI >= 144 then
    begin
      DoLoadButtonsGlyphs(220, 354, 13, 13);
      DoLoadThumbGlyphs(218, 413, 16, 16);
    end
    else
    begin
      DoLoadButtonsGlyphs(210, 354, 9, 9);
      DoLoadThumbGlyphs(234, 423, 11, 11);
    end;
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonStatusBar(ABitmap: GpBitmap);
begin
  FStatusBar := AddPart1x3(ABitmap, cxRectBounds(198, 354, 3, 30), 2, 0, rspStatusBar);
  FStatusBarGripBackground := FStatusBar;
  FStatusBarPanelLowered := FStatusBar;
  FStatusBarPanelRaised := FStatusBar;
  FStatusBarPanel := FStatusBar;

  FStatusBarPanelSeparator := AddPart1x3(ABitmap,
    cxRectBounds(202, 354, 3, 30), 2, 0, rspStatusBarPanelSeparator);
  FStatusBarToolbarSeparator := AddPart1x3(ABitmap,
    cxRectBounds(206, 354, 3, 20), 2, 0, rspStatusBarToolbarSeparator);

  LoadElementParts(ABitmap, FFormStatusBarLeftParts[False], cxRectBounds(163, 354, 8, 21),
    rspStatusBarFormLeftPart, cxRect(0, 2, 0, 8), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarRightParts[False], cxRectBounds(172, 354, 8, 21),
    rspStatusBarFormRightPart, cxRect(0, 2, 0, 8), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarLeftParts[True], cxRectBounds(180, 354, 8, 22),
    rspStatusBarFormLeftPartDialog, cxRect(0, 2, 0, 8), [0, 1, 2, 3], [0, 1, 2, 3]);
  LoadElementParts(ABitmap, FFormStatusBarRightParts[True], cxRectBounds(189, 354, 8, 22),
    rspStatusBarFormRightPartDialog, cxRect(0, 2, 0, 8), [0, 1, 2, 3], [0, 1, 2, 3]);
end;

procedure TdxCustomRibbon2010Skin.LoadRibbonTab(ABitmap: GpBitmap);
begin
  LoadElementParts(ABitmap, FTabIndex, cxRectBounds(0, 0, 24, 23),
    rspTabNormal, cxRect(4, 4, 4, 4), [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
  LoadElementParts(ABitmap, FContextTabIndex, cxRectBounds(163, 253, 36, 20),
    rspContextTabNormal, cxRect(5, 3, 5, 1), [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
  LoadElementParts(ABitmap, FContextTabMaskIndex, cxRectBounds(199, 253, 36, 20),
    rspContextTabMaskNormal, cxRect(5, 3, 5, 1), [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
  FTabSeparator := AddPart1x1(ABitmap, cxRectBounds(42, 86, 2, 16), rspTabSeparator);
  FTabsAreaOnGlass := AddPart3x3(ABitmap, cxRectBounds(163, 0, 95, 36),
    cxRect(42, 0, 42, 0), rspTabsAreaOnGlass, '', InterpolationModeHighQualityBicubic);
end;

{ TdxBlueRibbon2010Skin }

procedure TdxBlueRibbon2010Skin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $AD8E72);
end;

function TdxBlueRibbon2010Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_INRIBBONGALLERY_BACKGROUND:
      case AState of
        DXBAR_ACTIVE, DXBAR_HOT:
          Result := $FCF7F2
      else
        Result := $FDF5ED;
      end;

    DXBAR_EDIT_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := $D6C0B1;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $D0BAAB;
        DXBAR_DISABLED:
          Result := $DEDDCD;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_NORMAL:
          Result := $FDF5ED;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $FCF7F2;
        DXBAR_DISABLED:
          Result := $FDF5ED;
      else
        Result := inherited;
      end;

    DXBAR_SCREENTIP_FOOTERLINE:
      Result := $DDBB9E;
    rspTabGroupHeaderText:
      Result := $734E38;
    rspStatusBarSizeGripColor1:
      Result := $BD9D84;
    rspStatusBarSizeGripColor2:
      Result := $F1E1D4;
    rspRibbonBackground, rfspRibbonForm:
      Result := $E4CCB7;
    rspFormCaptionText, rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := inherited
      else
        Result := $A36736;

  else
    Result := inherited;
  end;
end;

procedure TdxBlueRibbon2010Skin.DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
  AApplicationMenuState: TdxRibbonApplicationMenuState);
const
  ColorMap: array[Boolean] of TColor = ($F7EBDF, $E8D0BD);
begin
  if LowColors then
    inherited DrawTabAreaBackground(DC, R, AActive, AUseAeroGlass, AApplicationMenuState)
  else
    if not AUseAeroGlass then
      FillRectByColor(DC, R, ColorMap[AActive]);
end;

procedure TdxBlueRibbon2010Skin.DrawFormCaptionSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $AD8E72);
end;

procedure TdxBlueRibbon2010Skin.GetApplicationMenuContentColors(
  var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  AInnerBorderColor := clNone;
  AOuterBorderColor := $B9AFA8;
  ASideColor := clNone;
end;

function TdxBlueRibbon2010Skin.CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($5945E7, $4DA741, $FFFFFF, $AB7452, $059EFF, $D5885A));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($B8ABA2, $B8ABA2, $FFFFFF, $B8ABA2, $B8ABA2, $B8ABA2));
end;

function TdxBlueRibbon2010Skin.GetName: string;
begin
  Result := 'Blue';
end;

procedure TdxBlueRibbon2010Skin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONBLUE2010', AImage);
end;

{ TdxSilverRibbon2010Skin }

procedure TdxSilverRibbon2010Skin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $928C87);
end;

procedure TdxSilverRibbon2010Skin.DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
  AApplicationMenuState: TdxRibbonApplicationMenuState);
const
  ColorMap: array[Boolean] of TColor = ($FCFCFC, $E8E6E3);
begin
  if LowColors then
    inherited DrawTabAreaBackground(DC, R, AActive, AUseAeroGlass, AApplicationMenuState)
  else
    if not AUseAeroGlass then
      FillRectByColor(DC, R, ColorMap[AActive]);
end;

function TdxSilverRibbon2010Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_GALLERYFILTERBANDTEXT:
      if AState = DXBAR_HOT then
        Result := $32D2FF
      else
        Result := $FFFFFF;

    DXBAR_INRIBBONGALLERY_BACKGROUND:
      Result := $FFFFFF;
    DXBAR_EDIT_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := $D9D6D4;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $C4BFBB;
        DXBAR_DISABLED:
          Result := $EBE7E4;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_NORMAL, DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $FFFFFF;
        DXBAR_DISABLED:
          Result := $FAFAFA;
      else
        Result := inherited;
      end;

    DXBAR_SCREENTIP_FOOTERLINE:
      Result := $BDBDBD;
    rspTabGroupHeaderText:
      Result := $7C6D66;
    rspContextText:
      Result := clGray;
    rspContextTextShadow:
      Result := clWhite;
    rspStatusBarSizeGripColor1:
      Result := $C7BEB5;
    rspStatusBarSizeGripColor2:
      Result := $FFFFFF;
    rspRibbonBackground, rfspRibbonForm:
      Result := $F1EDE9;
    rspFormCaptionText, rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := inherited
      else
        Result := $8A8A8A;

  else
    Result := inherited;
  end;
end;

function TdxSilverRibbon2010Skin.CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($5945E7, $4DA741, $FFFFFF, $706A65, $059EFF, $D5885A));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($BABABA, $BABABA, $FFFFFF, $BABABA, $BABABA, $BABABA));
end;

procedure TdxSilverRibbon2010Skin.DrawFormCaptionSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $928C87);
end;

procedure TdxSilverRibbon2010Skin.GetApplicationMenuContentColors(
  var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  AInnerBorderColor := clNone;
  AOuterBorderColor := $C6C3C0;
  ASideColor := clNone;
end;

function TdxSilverRibbon2010Skin.GetName: string;
begin
  Result := 'Silver';
end;

procedure TdxSilverRibbon2010Skin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONSILVER2010', AImage);
end;

{ TdxBlackRibbon2010Skin }

procedure TdxBlackRibbon2010Skin.DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer);
begin
  Parts[FLaunchButtonDefaultGlyphs[AState]].DrawColored(DC, R, $484848, 255, UseRightToLeftAlignment);
end;

procedure TdxBlackRibbon2010Skin.DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  InternalDrawSeparator(DC, R, AHorizontal, $CECECE, $ABABAB);
end;

procedure TdxBlackRibbon2010Skin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $484848);
end;

procedure TdxBlackRibbon2010Skin.DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
  AApplicationMenuState: TdxRibbonApplicationMenuState);
const
  ColorMap: array[Boolean] of TColor = ($9E9E9E, $717171);
begin
  if LowColors then
    inherited DrawTabAreaBackground(DC, R, AActive, AUseAeroGlass, AApplicationMenuState)
  else
    if not AUseAeroGlass then
      FillRectByColor(DC, R, ColorMap[AActive]);
end;

function TdxBlackRibbon2010Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_INRIBBONGALLERY_BACKGROUND:
      case AState of
        DXBAR_ACTIVE, DXBAR_HOT:
          Result := $C1C1C1
      else
        Result := $BBBBBB;
      end;

    DXBAR_GALLERYFILTERBANDTEXT:
      if AState = DXBAR_HOT then
        Result := $32D2FF
      else
        Result := $FFFFFF;

    DXBAR_EDIT_BORDER:
      case AState of
        DXBAR_NORMAL, DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $848484;
        DXBAR_DISABLED:
          Result := $A0A0A0;
      else
        Result := inherited;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_NORMAL:
          Result := $C6C6C6;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $CCCCCC;
        DXBAR_DISABLED:
          Result := $B6B6B6;
      else
        Result := inherited;
      end;

    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE:
          Result := clBlack;
        DXBAR_HOT:
          Result := $F0F0F0;
        DXBAR_DISABLED:
          Result := $9D9D9D;
      else
        Result := $E2E2E2;
      end;

    rspStatusBarText:
      case AState of
        DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_CHECKED:
          Result := clBlack;
        DXBAR_DISABLED:
          Result := GetPartColor(DXBAR_ITEMTEXT, DXBAR_DISABLED);
      else
        Result := $E2E2E2;
      end;

    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $A7A7A7;
      else
        Result := clWhite;
      end;

    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $A7A7A7;
        DXBAR_HOTCHECK, DXBAR_CHECKED, DXBAR_PRESSED:
          if ColorSchemeAccent = rcsaYellow then
            Result := clBlack
          else
            Result := clWhite;
      else
        Result := clWhite;
      end;

   DXBAR_SCREENTIP_FOOTERLINE:
      Result := $BDBDBD;
    rspTabGroupHeaderText:
      Result := $393631;
    rspStatusBarSizeGripColor1:
      Result := $1B1B1B;
    rspStatusBarSizeGripColor2:
      Result := $595959;
    rspRibbonBackground, rfspRibbonForm:
      Result := $8B8B8B;
    rspFormCaptionText, rspDocumentNameText:
      if AState <> DXBAR_NORMAL then
        Result := $D4D4D4
      else
        Result := $E2E2E2;
  else
    Result := inherited;
  end;
end;

function TdxBlackRibbon2010Skin.CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($4D50C0, $2D900B, $B7B7B7, $595959, $67CBFF, $D5885A));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($909090, $909090, $FFFFFF, $909090, $909090, $909090));
end;

procedure TdxBlackRibbon2010Skin.DrawFormCaptionSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, $484848);
end;

procedure TdxBlackRibbon2010Skin.GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  AInnerBorderColor := clNone;
  AOuterBorderColor := $ABABAB;
  ASideColor := clNone;
end;

function TdxBlackRibbon2010Skin.GetName: string;
begin
  Result := 'Black';
end;

procedure TdxBlackRibbon2010Skin.InitializeColorPalettes;
begin
  inherited;
  FColorPaletteRadialMenu := nil;
  FColorPaletteMenu := nil;
end;

procedure TdxBlackRibbon2010Skin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONBLACK2010', AImage);
end;

{ RegisterSkins }

procedure RegisterSkins;
begin
  if CheckGdiPlus(True) then
  begin
    dxRibbonSkinsManager.Add(TdxBlueRibbon2010Skin.Create);
    dxRibbonSkinsManager.Add(TdxSilverRibbon2010Skin.Create);
    dxRibbonSkinsManager.Add(TdxBlackRibbon2010Skin.Create);
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterSkins, nil);
end.

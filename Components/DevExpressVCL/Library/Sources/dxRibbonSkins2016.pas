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

unit dxRibbonSkins2016;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Graphics, dxCore, dxGDIPlusAPI, dxGDIPlusClasses, cxGraphics, cxGeometry,
  dxBarSkin, dxBarSkinConsts, cxLookAndFeelPainters, dxRibbonSkins, dxRibbonSkins2013;

type

  { TdxLightGrayRibbon2016Skin }

  TdxLightGrayRibbon2016Skin = class(TdxLightGrayRibbon2013Skin)
  protected
    function GetStyle: TdxRibbonStyle; override;
  end;

  { TdxColorfulRibbon2016Skin }

  TdxColorfulRibbon2016Skin = class(TdxLightGrayRibbon2016Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetAccentColor: TColor; override;
    function GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor; override;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetFormBorderColor(AActive: Boolean): TColor; override;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; override;
    function GetFormCaptionAreaColor(AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor; override;
    function GetFrameColor: TColor; override;
    function GetHighlightBorderColor: TColor; override;
    function GetHighlightContentColor: TColor; override;
    function GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor; override;
    function GetMasterColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor; override;
    function GetName: string; override;
    function GetTabGroupsAreaContentColor: TColor; override;
    procedure InitializeColorPalettes; override;
  public
    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); override;
    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); override;
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawEditButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawFormBorderIconBackground(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); override;
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); override;
    procedure DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuSeparatorHorz(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarArrowDown(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarMark(DC: HDC; const R: TRect; AState: Integer; AStatesArray: TStatesArray);
    procedure DrawQuickAccessToolbarMarkArrow(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarMarkTruncated(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean); override;
    procedure DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0); override;
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawStatusBar(DC: HDC; const R: TRect); override;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    procedure DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte); override;
    procedure DrawTabAreaArrowDown(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawTabAreaMarkArrow(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawTabAreaMarkTruncated(DC: HDC; const R: TRect; AState: Integer); override;
    function GetInRibbonGalleryScrollBarButtonGlyphColor(
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor; override;
  end;

  { TdxColorfulRibbon2016TabletSkin }

  TdxColorfulRibbon2016TabletSkin = class(TdxColorfulRibbon2016Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    procedure DrawFormBorderIconButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawTabBackground(DC: HDC; const R: TRect; AColor, ABorderColor: TColor); virtual;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetFormBorderIconButtonColor(AState: Integer): TColor; virtual;
    function GetName: string; override;
    function GetStyle: TdxRibbonStyle; override;
  public
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawFormBorderIconBackground(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); override;
    procedure DrawMinimizeRibbonButton(DC: HDC; const R: TRect; AState: TcxButtonState; AMinimized: Boolean); override;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    function GetPartSize(APart: Integer): Integer; override;
  end;

  { TdxMediumGrayRibbon2016Skin }

  TdxMediumGrayRibbon2016Skin = class(TdxDarkGrayRibbon2013Skin)
  protected
    function GetName: string; override;
    function GetStyle: TdxRibbonStyle; override;
  end;

  { TdxDarkGrayRibbon2016Skin }

  TdxDarkGrayRibbon2016Skin = class(TdxDarkGrayRibbon2013Skin)
  protected
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetAccentColor: TColor; override;
    function GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor; override;
    function GetColorfulButtonColor(AState: Integer): TColor; override;
    function GetEditButtonContentColor(AState: Integer): TColor; virtual;
    function GetFormBorderColor(AActive: Boolean): TColor; override;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; override;
    function GetFormCaptionAreaColor(AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor; override;
    function GetFrameColor: TColor; override;
    function GetHighlightBorderColor: TColor; override;
    function GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor; override;
    function GetMasterColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor; override;
    function GetName: string; override;
    function GetStyle: TdxRibbonStyle; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); override;
    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); override;
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;
    procedure DrawDropDownBorder(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawEditButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawFormBorderIconBackground(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); override;
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); override;
    procedure DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuSeparatorHorz(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean); override;
    procedure DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0); override;
    procedure DrawSeparatorBackground(DC: HDC; const R: TRect); override;
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte); override;
    function GetInRibbonGalleryScrollBarButtonGlyphColor(AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor; override;
    function GetScrollBarBackgroundColor: TColor; override;
    function GetScrollBarGlyphColor: TColor; override;
    function GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

  { TdxWhiteRibbon2016Skin }

  TdxWhiteRibbon2016Skin = class(TdxWhiteRibbon2013Skin)
  protected
    function GetStyle: TdxRibbonStyle; override;
  end;

implementation

uses
  Math;

{ TdxColorfulRibbon2016kin }

procedure TdxColorfulRibbon2016Skin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
const
  StatesMap: array[Boolean] of Integer = (DXBAR_HOT, DXBAR_ACTIVE);
begin
  if LowColors then
    inherited AdjustContextFont(AFont, AUseGlass, AContextColor)
  else
    AFont.Color := GetPartColor(rspTabHeaderText, StatesMap[AUseGlass]);
end;

procedure TdxColorfulRibbon2016Skin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  if LowColors then
    inherited AdjustContextTabFont(AFont, AState, AContextColor)
  else
    AFont.Color := GetPartColor(rspTabHeaderText, AState);
end;

procedure TdxColorfulRibbon2016Skin.DrawApplicationButton(
  DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
var
  AColor: TColor;
begin
  if LowColors then
    inherited DrawApplicationButton(DC, R, AState)
  else
  begin
    if AState = rabsHot then
      AColor := GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT)
    else
      AColor := GetAccentColor;

    FillRectByColor(DC, cxRectInflate(R, 0, 0, 0, -1), AColor);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryBackground(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL));
end;

procedure TdxColorfulRibbon2016Skin.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItem(DC, R, AState)
  else
    if AState in [DXBAR_CHECKED, DXBAR_HOTCHECK] then
      FillRectByColor(DC, R, GetPartColor(rspTabHeaderText, DXBAR_HOT))
    else
      DrawColorfulButton(DC, R, AState);
end;

procedure TdxColorfulRibbon2016Skin.DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItemPinButton(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_CHECKED:
        AState := DXBAR_NORMAL;
      DXBAR_HOTCHECK:
        AState := DXBAR_HOT;
    end;
    DrawColorfulButton(DC, R, AState);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuSeparator(DC, R, AState)
  else
    FillRectByColor(DC, cxRectInflate(cxRectCenterVertically(R, 1), -15, 0), GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT));
end;

procedure TdxColorfulRibbon2016Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewTabButton(DC, R, AState)
  else
    case AState of
      DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        FillRectByColor(DC, R, GetMinimizeRibbonButtonGlyphColor(DXBAR_PRESSED));
      DXBAR_CHECKED:
        FillRectByColor(DC, R, GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT));
    end;
end;

procedure TdxColorfulRibbon2016Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarBackground(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_HOT:
        FillRectByColor(DC, R, $DCDCDC);
      DXBAR_PRESSED:
        FillRectByColor(DC, R, $D0D0D0);
    end;
    FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), GetFrameColor);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawContextBackground(DC, R, AContextColor)
  else
  begin
    R1 := R;
    Inc(R1.Top);
    dxGpFillRect(DC, R1, $000000, 77);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState;
  AContextColor: TColor);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawContextTabBackground(DC, R, AState, AContextColor)
  else
  begin
    R1 := cxRectSetHeight(R, cxRectHeight(R) - GetPartSize(DXBAR_TABSGROUPSOVERLAPHEIGHT));
    dxGpFillRect(DC, R1, $000000, 77);
    DrawTab(DC, R, AState);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBottomSizingBand(DC, R)
  else
    FillRectByColor(DC, R, $F6F6F6);
end;

procedure TdxColorfulRibbon2016Skin.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryTopSizingBand(DC, R)
  else
    FillRectByColor(DC, R, $F6F6F6);
end;

procedure TdxColorfulRibbon2016Skin.DrawEditButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawEditButton(DC, R, AState)
  else
    case AState of
      DXBAR_HOT:
        FillRectByColor(DC, R, $E3E3E3);
      DXBAR_ACTIVE:
        DrawFrame(DC, cxRectInflate(R, 1), GetPartColor(DXBAR_EDIT_BACKGROUND),
          GetPartColor(DXBAR_EDIT_BUTTON_BORDER, AState));
      DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        FillRectByColor(DC, R, GetPartColor(DXBAR_EDIT_BUTTON_BORDER, DXBAR_DROPPEDDOWN));
    end;
end;

procedure TdxColorfulRibbon2016Skin.DrawFormBorderIconBackground(
  DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
var
  AColor: TColor;
begin
  if LowColors then
    inherited
  else
  begin
    AColor := clNone;
    if AIcon <> rbdiClose then
      case AState of
        rbisHot, rbisHotInactive:
          AColor := TdxColorHelper.ChangeLightness(GetAccentColor, 0.07);
        rbisPressed:
          AColor := TdxColorHelper.ChangeLightness(GetAccentColor, -0.09);
      end
    else
      case AState of
        rbisHot, rbisHotInactive:
          AColor := $2311E8;
        rbisPressed:
          AColor := $7A70F1;
      end;
    if AColor <> clNone then
      FillRectByColor(DC, R, AColor);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
  AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);

  function GetBorderColor(AState: Integer): TColor;
  begin
    if AState in [DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN] then
      Result := GetColorfulButtonColor(DXBAR_PRESSED)
    else
      Result := GetPartColor(DXBAR_EDIT_BORDER);
  end;

  function GetContentColor(AState: Integer): TColor;
  begin
    if AState in [DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN] then
      Result := GetColorfulButtonColor(AState)
    else
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
  end;

var
  ASaveIndex: Integer;
  R1: TRect;
begin
  if LowColors then
    inherited DrawInRibbonGalleryScrollBarButton(DC, R, AButtonKind, AState)
  else
  begin
    R1 := R;
    ASaveIndex := SaveDC(DC);
    try
      IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      Inc(R1.Bottom, Ord(AButtonKind <> gsbkDropDown));
      DrawFrame(DC, R1, GetContentColor(AState), GetBorderColor(AState));
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        DrawFrame(DC, R1, clNone, GetBorderColor(DXBAR_NORMAL), [bTop..bBottom]);
      DrawInRibbonGalleryScrollBarButtonGlyph(DC, R, AButtonKind, AState);
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited
  else
    InternalDrawGlyph(DC, R, FMarkArrow[AState], GetPartColor(DXBAR_ARROWDOWN, AState));
end;

procedure TdxColorfulRibbon2016Skin.DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawMarkTruncated(DC, R, AState)
  else
    InternalDrawGlyph(DC, R, FMarkTruncated[AState], GetMinimizeRibbonButtonGlyphColor(AState));
end;

procedure TdxColorfulRibbon2016Skin.DrawMenuSeparatorHorz(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuSeparatorHorz(DC, R)
  else
    FillRectByColor(DC, cxRectCenterVertically(R, 1), $E3E3E3);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarArrowDown(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
var
  AColor: TColor;
begin
  if ABelow then
    inherited
  else
  begin
    if AState = DXBAR_NORMAL then
      AColor := $EFEFEF
    else
      AColor := $FFFFFF;

    DrawArrow(DC, R, adDown, AColor);
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarMark(
  DC: HDC; const R: TRect; AState: Integer; AStatesArray: TStatesArray);
var
  AColor: TColor;
begin
  if AState = DXBAR_NORMAL then
    AColor := $EFEFEF
  else
    AColor := $FFFFFF;

  InternalDrawGlyph(DC, R, AStatesArray[AState], AColor);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarMarkArrow(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if ABelow then
    inherited
  else
    DrawQuickAccessToolbarMark(DC, R, AState, FMarkArrow);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarMarkTruncated(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  if ABelow then
    inherited
  else
    DrawQuickAccessToolbarMark(DC, R, AState, FMarkTruncated);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarPopup(DC, R)
  else
    DrawDropDownBorder(DC, R);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButton(DC, R, AState)
  else
  begin
    if AState = DXBAR_ACTIVE then
      AState := DXBAR_HOT;
    case AState of
      DXBAR_CHECKED:
        FillRectByColor(DC, R, GetPartColor(rspTabHeaderText, DXBAR_HOT));
      DXBAR_HOTCHECK:
        DrawFrame(DC, R, GetColorfulButtonColor(DXBAR_HOT), GetColorfulButtonColor(DXBAR_PRESSED), [bLeft..bBottom], 1, True);
    else
      DrawColorfulButton(DC, R, AState);
    end;
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect;
  AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC, R, AState)
  else
    DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxColorfulRibbon2016Skin.DrawQuickAccessToolbarSmallButtonDropButtonMainPart(
  DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE:
        DrawFrame(DC, R, $FFFFFF, $ABABAB, [bLeft..bBottom], 1, True);
      DXBAR_DROPPEDDOWN:
        DrawColorfulButton(DC, R, DXBAR_HOT);
    else
      DrawColorfulButton(DC, R, AState);
    end;
end;

procedure TdxColorfulRibbon2016Skin.DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean);
begin
  if AUseAeroGlass then
    Parts[FTabsAreaOnGlass].DrawColored(DC, cxRectInflate(R, 0, 0, 0, -1), GetAccentColor);
end;

procedure TdxColorfulRibbon2016Skin.DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0);
begin
  if LowColors then
    inherited DrawScrollArrow(DC, R, AState)
  else
    Parts[FScrollArrow].DrawColored(DC, R, GetInRibbonGalleryScrollBarButtonGlyphColor(gsbkLineUp, AState));
end;

procedure TdxColorfulRibbon2016Skin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors or not (AState in [DXBAR_CHECKED, DXBAR_DROPPEDDOWN]) then
    inherited DrawSmallButton(DC, R, AState)
  else
    FillRectByColor(DC, R, $C4C4C4);
end;

procedure TdxColorfulRibbon2016Skin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonArrowPart(DC, R, AState)
  else
    if AState in [DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED] then
      DrawFrame(DC, R, $E6E6E6, GetHighlightContentColor, [bTop..bBottom], 1, True)
    else
      DrawSmallButton(DC, R, AState);
end;

procedure TdxColorfulRibbon2016Skin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonMainPart(DC, R, AState)
  else
    if AState in [DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED] then
      DrawFrame(DC, R, $E6E6E6, GetHighlightContentColor, [bLeft, bTop, bBottom], 1, True)
    else
      DrawSmallButton(DC, R, AState);
end;

procedure TdxColorfulRibbon2016Skin.DrawStatusBar(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBar(DC, R)
  else
    FillRectByColor(DC, R, GetTabGroupsAreaContentColor);
end;

procedure TdxColorfulRibbon2016Skin.DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte);
begin
  if LowColors then
    inherited DrawTabSeparator(DC, R, Alpha)
  else
    dxGpFillRect(DC, R, $E1E1E1, Alpha);
end;

function TdxColorfulRibbon2016Skin.GetInRibbonGalleryScrollBarButtonGlyphColor(
  AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor;
begin
  case AState of
    DXBAR_NORMAL, DXBAR_ACTIVE:
      Result := $777777;
    DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
      Result := $C6C6C6;
    DXBAR_HOT:
      Result := $444444;
    DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
      Result := $000000;
  else
    Result := GetPartColor(DXBAR_ARROWDOWN, AState);
  end;
end;

function TdxColorfulRibbon2016Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
const
  HotTabHeaderTextColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $F2D5C2, $B7D59F, $B6CDFC, $DEBFE0);
begin
  case APart of
    rspDocumentNameText, rspFormCaptionText:
      if AState = DXBAR_NORMAL then
        Result := $F9F9F9
      else
        Result := $E9E9E9;

    rspStatusBarText:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $989898
      else
        Result := $505050;

    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE:
          Result := GetAccentColor;
        DXBAR_HOT:
          Result := HotTabHeaderTextColorMap[ColorSchemeAccent];
        DXBAR_DISABLED:
          Result := TdxColorHelper.MultiplyLightness(GetAccentColor, 1.5);
      else
        Result := $F9F9F9;
      end;

    rtatpEditText:
      case AState of
        DXBAR_NORMAL:
          Result := GetBackstageViewBackButtonGlyphColor(DXBAR_HOT);
        DXBAR_DISABLED:
          Result := $989898;
      else
        Result := $FFFFFF;
      end;

    rtatpEditBackground:
      begin
        Result := GetAccentColor;
        if not (AState in [DXBAR_NORMAL, DXBAR_DISABLED]) then
          Result := TdxColorHelper.ChangeLightness(Result, -0.08);
      end;

    rspTabGroupHeaderText:
      Result := $666666;

    DXBAR_ARROWDOWN:
      case AState of
        DXBAR_NORMAL:
          Result := $777777;
        DXBAR_DISABLED:
          Result := $C6C6C6;
      else
        Result := $444444;
      end;

    DXBAR_ITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $989898
      else
        Result := $444444;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $B1B1B1
      else
        Result := $5E5E5E;

    DXBAR_EDIT_BACKGROUND:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $FDFDFD
      else
        Result := $FFFFFF;

    DXBAR_EDIT_BORDER, DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := $ABABAB;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE:
          if APart = DXBAR_EDIT_BORDER then
            Result := GetColorfulButtonColor(DXBAR_PRESSED)
          else
            Result := $C3C3C3;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $E1E1E1;
      else
        Result := inherited ;
      end;

    DXBAR_BACKSTAGEVIEW, DXBAR_BACKSTAGEVIEW_GALLERYCONTROL:
      Result := GetTabGroupsAreaContentColor;

    DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
      Result := $444444;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $B1B1B1
      else
        Result := $262626;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $B1B1B1
      else
        Result := $444444;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $B1B1B1
      else
        Result := GetAccentColor;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $E1E1E1;
        DXBAR_HOTCHECK:
          Result := GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT);
      else
        Result := GetMinimizeRibbonButtonGlyphColor(AState);
      end;

    DXRIBBON_TAT_SMALLBUTTON:
      if PaintData.GetApplicationMenuState <> ramsShownAsFullScreenFrame then
        Result := inherited
      else
        Result := GetPartColor(DXBAR_BUTTONITEMTEXT, AState);

  else
    Result := inherited;
  end;
end;

procedure TdxColorfulRibbon2016Skin.DrawTabAreaArrowDown(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawQuickAccessToolbarArrowDown(DC, R, AState, False);
end;

procedure TdxColorfulRibbon2016Skin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
begin
  if LowColors then
    inherited DrawTabAreaButton(DC, R, AState)
  else
    DrawColorfulButton(DC, R, ButtonStateToRibbonState(AState));
end;

procedure TdxColorfulRibbon2016Skin.DrawTabAreaMarkArrow(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawQuickAccessToolbarMarkArrow(DC, R, AState, False);
end;

procedure TdxColorfulRibbon2016Skin.DrawTabAreaMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawQuickAccessToolbarMarkTruncated(DC, R, AState, False);
end;

function TdxColorfulRibbon2016Skin.GetAccentColor: TColor;
const
  ColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($3EA5EA, $9A572B, $467321, $2A47B7, $7B3980);
begin
  Result := ColorMap[ColorSchemeAccent];
end;

function TdxColorfulRibbon2016Skin.GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor;
const
  ColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $D9B298, $B0CC99, $A2B0EB, $D1AED4);
begin
  if LowColors then
    Result := inherited GetBackstageViewBackButtonGlyphColor(AState)
  else
    if AState in [DXBAR_HOT, DXBAR_PRESSED] then
      Result := ColorMap[ColorSchemeAccent]
    else
      Result := $FFFFFF;
end;

function TdxColorfulRibbon2016Skin.GetFormCaptionAreaColor(
  AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor;
begin
  if AApplicationMenuState = ramsShownAsFullScreenFrame then
    Result := GetTabGroupsAreaContentColor
  else
    Result := GetAccentColor;
end;

function TdxColorfulRibbon2016Skin.GetFormBorderColor(AActive: Boolean): TColor;
begin
  if AActive then
    Result := GetAccentColor
  else
    Result := $838383;
end;

function TdxColorfulRibbon2016Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
const
  DisabledStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($C8E3FD, $6B3D1D, $325118, $1E3280, $562859);
begin
  if AState in [rbisInactive, rbisHotInactive] then
    Result := DisabledStateColorMap[ColorSchemeAccent]
  else
    if PaintData.GetApplicationMenuState = ramsShownAsFullScreenFrame then
      Result := GetMinimizeRibbonButtonGlyphColor(BorderIconStateToBarState[AState])
    else
      Result := $FFFFFF;
end;

function TdxColorfulRibbon2016Skin.GetFrameColor: TColor;
begin
  Result := $D4D4D4;
end;

function TdxColorfulRibbon2016Skin.GetHighlightBorderColor: TColor;
begin
  Result := $B0B0B0;
end;

function TdxColorfulRibbon2016Skin.GetHighlightContentColor: TColor;
begin
  Result := $CDCDCD;
end;

function TdxColorfulRibbon2016Skin.GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_NORMAL:
      Result := $6B6B6B;
    DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
      Result := $B3B3B3;
    DXBAR_PRESSED:
      Result := $494949;
    else
      Result := $6D6D6D;
  end;
end;

function TdxColorfulRibbon2016Skin.GetMasterColor: TColor;
begin
  Result := $DFDFDF;
end;

function TdxColorfulRibbon2016Skin.GetMenuBackgroundColor: TColor;
begin
  Result := GetAccentColor;
end;

function TdxColorfulRibbon2016Skin.GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor;
const
  HotStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $B56D3E, $679443, $3E62F0, $9E56A3);
  PressedStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $8A4719, $32630A, $1D3BB8, $682F6C);
begin
  case AState of
    DXBAR_HOT:
      Result := HotStateColorMap[ColorSchemeAccent];
    DXBAR_PRESSED:
      Result := PressedStateColorMap[ColorSchemeAccent];
    else
      Result := $777777;
  end;
end;

function TdxColorfulRibbon2016Skin.GetName: string;
begin
  Result := 'Colorful';
end;

function TdxColorfulRibbon2016Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $F1F1F1;
end;

function TdxColorfulRibbon2016Skin.GetColorfulButtonColor(AState: Integer): TColor;
const
  HotStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($C8E3FD, $F2E1D5, $E0F0D3, $DCE4FC, $EEDAF0);
  PressedStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $E3BDA3, $A0BF86, $9DBAF5, $D1A9D4);
begin
  case AState of
    DXBAR_HOT, DXBAR_ACTIVE:
      Result := HotStateColorMap[ColorSchemeAccent];
    DXBAR_PRESSED:
      Result := PressedStateColorMap[ColorSchemeAccent];
  else
    Result := inherited;
  end;
end;

procedure TdxColorfulRibbon2016Skin.InitializeColorPalettes;
begin
  inherited;
  FColorPaletteQAT := FColorPaletteBackstageViewMenu;
  FColorPaletteTabAreaToolbar := FColorPaletteBackstageViewMenu;
end;

{ TdxColorfulRibbon2016TabletSkin }

procedure TdxColorfulRibbon2016TabletSkin.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItem(DC, R, AState)
  else
    DrawFormBorderIconButton(DC, R, IfThen(AState in [DXBAR_CHECKED, DXBAR_HOTCHECK], DXBAR_PRESSED, AState));
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItemPinButton(DC, R, AState)
  else
    DrawFormBorderIconButton(DC, R, IfThen(AState = DXBAR_HOTCHECK, DXBAR_HOT, AState));
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarGlyphBackground(DC, R, AState);
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawFormBorderIconBackground(
  DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
begin
  if LowColors or (PaintData.GetApplicationMenuState <> ramsShownAsFullScreenFrame) then
    inherited
  else
    DrawFormBorderIconButton(DC, R, BorderIconStateToBarState[AState]);
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawMinimizeRibbonButton(DC: HDC; const R: TRect; AState: TcxButtonState;
  AMinimized: Boolean);
begin
  if LowColors then
    inherited DrawMinimizeRibbonButton(DC, R, AState, AMinimized)
  else
    DrawFormBorderIconButton(DC, R, ButtonStateToRibbonState(AState));
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  if LowColors then
    inherited DrawTab(DC, R, AState)
  else
    case AState of
      rtsHot:
        DrawTabBackground(DC, R, GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT), GetMinimizeRibbonButtonGlyphColor(DXBAR_HOT));
      rtsActive, rtsActiveHot:
        DrawTabBackground(DC, R, GetTabGroupsAreaContentColor, GetFrameColor);
      rtsFocused:
        DrawTabBackground(DC, R, GetHighlightContentColor, GetHighlightBorderColor);
    end;
end;

function TdxColorfulRibbon2016TabletSkin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  if (APart = rspTabHeaderText) and (AState = DXBAR_HOT) then
    Result := $F9F9F9
  else
    Result := inherited;
end;

function TdxColorfulRibbon2016TabletSkin.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    rspTabGroupInPopupBottomOffset, rspTabGroupBottomOffset:
      Result := 2;
  else
    Result := inherited GetPartSize(APart);
  end;
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawFormBorderIconButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if AState in [DXBAR_HOT, DXBAR_PRESSED] then
    FillRectByColor(DC, R, GetFormBorderIconButtonColor(AState));
end;

procedure TdxColorfulRibbon2016TabletSkin.DrawTabBackground(DC: HDC; const R: TRect; AColor, ABorderColor: TColor);
const
  Radius = 2;
begin
  dxGpRoundRect(DC, R, ABorderColor, AColor, Radius);
  DrawFrame(DC, cxRectInflate(R, 0, -Radius, 0, 0), AColor, ABorderColor, [bLeft, bRight], 1, True);
end;

function TdxColorfulRibbon2016TabletSkin.GetName: string;
begin
  Result := 'Colorful';
end;

function TdxColorfulRibbon2016TabletSkin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016Tablet;
end;

function TdxColorfulRibbon2016TabletSkin.GetFormBorderIconButtonColor(AState: Integer): TColor;
begin
  Result := inherited GetColorfulButtonColor(AState);
end;

function TdxColorfulRibbon2016TabletSkin.GetColorfulButtonColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := GetMinimizeRibbonButtonGlyphColor(AState);
    DXBAR_PRESSED:
      Result := TdxColorHelper.ChangeLightness(GetMinimizeRibbonButtonGlyphColor(AState), -0.05);
  else
    Result := inherited;
  end;
end;

{ TdxLightGrayRibbon2016kin }

function TdxLightGrayRibbon2016Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016;
end;

{ TdxMediumGrayRibbon2016Skin }

function TdxMediumGrayRibbon2016Skin.GetName: string;
begin
  Result := 'MediumGray';
end;

function TdxMediumGrayRibbon2016Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016;
end;

{ TdxDarkGrayRibbon2016Skin }

procedure TdxDarkGrayRibbon2016Skin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
begin
  if LowColors then
    inherited AdjustContextFont(AFont, AUseGlass, AContextColor)
  else
    AFont.Color := GetPartColor(rspTabHeaderText);
end;

procedure TdxDarkGrayRibbon2016Skin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  if LowColors then
    inherited AdjustContextTabFont(AFont, AState, AContextColor)
  else
    AFont.Color := GetPartColor(rspTabHeaderText, AState);
end;

function TdxDarkGrayRibbon2016Skin.CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($2C4BBE, $758A4C, $E0E0E0, $333333, $A0D7F2, $8F5C29));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($A1A1A1, $A1A1A1, $FFFFFF, $A1A1A1, $A1A1A1, $A1A1A1));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawApplicationButton(
  DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
begin
  if LowColors then
    inherited DrawApplicationButton(DC, R, AState)
  else
    DrawColorfulButton(DC, R, IfThen(AState = rabsHot, DXBAR_PRESSED, DXBAR_HOT));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryBackground(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryGroupHeader(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItem(DC, R, AState)
  else
  begin
    if AState in [DXBAR_CHECKED, DXBAR_HOTCHECK] then
      AState := DXBAR_PRESSED;
    DrawColorfulButton(DC, R, AState);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewGalleryItemPinButton(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_CHECKED:
        AState := DXBAR_NORMAL;
      DXBAR_HOTCHECK:
        AState := DXBAR_HOT;
    end;
    DrawColorfulButton(DC, R, AState);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuSeparator(DC, R, AState)
  else
    FillRectByColor(DC, cxRectInflate(cxRectCenterVertically(R, 1), -15, 0), GetPartColor(DXBAR_BACKSTAGEVIEW));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewTabButton(DC, R, AState)
  else
    case AState of
      DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        FillRectByColor(DC, R, GetColorfulButtonColor(DXBAR_PRESSED));
      DXBAR_CHECKED:
        FillRectByColor(DC, R, GetBackstageViewBackButtonGlyphColor(DXBAR_HOT));
    end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarBackground(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_HOT:
        FillRectByColor(DC, R, $A3A3A3);
      DXBAR_PRESSED:
        FillRectByColor(DC, R, $959595);
    end;
    FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), GetFrameColor);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawContextBackground(DC, R, AContextColor)
  else
  begin
    R1 := R;
    Inc(R1.Top);
    dxGpFillRect(DC, R1, AContextColor);
    Inc(R1.Top, 4);
    dxGpFillRect(DC, R1, GetFrameColor);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState;
  AContextColor: TColor);
const
  BackgroundColorMap: array [Boolean] of TColor = ($444444, $454545);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawContextTabBackground(DC, R, AState, AContextColor)
  else
  begin
    R1 := cxRectSetHeight(R, cxRectHeight(R) - GetPartSize(DXBAR_TABSGROUPSOVERLAPHEIGHT));
    DrawFrame(DC, R1, BackgroundColorMap[AState = rtsHot], GetFrameColor, [bLeft..bBottom], 1, True);
    DrawTab(DC, R, AState);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawDropDownBorder(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownBorder(DC, R)
  else
    DrawFrame(DC, R, GetPartColor(DXBAR_MENUCONTENT), $ACACAC);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawDropDownGalleryBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBackground(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_DROPDOWNGALLERY));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBottomSizingBand(DC, R)
  else
    FillRectByColor(DC, R, GetTabGroupsAreaContentColor);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryTopSizingBand(DC, R)
  else
    FillRectByColor(DC, R, GetTabGroupsAreaContentColor);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawEditButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawEditButton(DC, R, AState)
  else
    if AState in [DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN] then
      DrawFrame(DC, cxRectInflate(R, 1), GetEditButtonContentColor(AState), GetEditButtonContentColor(DXBAR_PRESSED));
end;

procedure TdxDarkGrayRibbon2016Skin.DrawFormBorderIconBackground(DC: HDC; const R: TRect;
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
begin
  if LowColors then
    inherited
  else
    DrawColorfulButton(DC, R, BorderIconStateToBarState[AState]);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawInRibbonGalleryScrollBarButton(
  DC: HDC; const R: TRect; AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);

  function GetBorderColor(AState: Integer): TColor;
  begin
    case AState of
      DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        Result := GetEditButtonContentColor(DXBAR_PRESSED);
      DXBAR_DISABLED:
        Result := GetInRibbonGalleryScrollBarButtonGlyphColor(AButtonKind, AState);
      else
        Result := GetPartColor(DXBAR_EDIT_BORDER, AState);
    end;
  end;

  function GetContentColor(AState: Integer): TColor;
  begin
    if AState in [DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN] then
      Result := GetEditButtonContentColor(AState)
    else
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND, AState);
  end;

var
  ASaveIndex: Integer;
  R1: TRect;
begin
  if LowColors then
    inherited DrawInRibbonGalleryScrollBarButton(DC, R, AButtonKind, AState)
  else
  begin
    R1 := R;
    ASaveIndex := SaveDC(DC);
    try
      IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      Inc(R1.Bottom, Ord(AButtonKind <> gsbkDropDown));
      DrawFrame(DC, R1, GetContentColor(AState), GetBorderColor(AState));
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        DrawFrame(DC, R1, clNone, GetBorderColor(DXBAR_NORMAL), [bTop..bBottom]);
      DrawInRibbonGalleryScrollBarButtonGlyph(DC, R, AButtonKind, AState);
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLaunchButtonBackground(DC, R, AState)
  else
    DrawColorfulButton(DC, R, AState);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer);
var
  AColor: TColor;
begin
  if LowColors then
    inherited DrawMarkArrow(DC, R, AState)
  else
  begin
    if AState = DXBAR_NORMAL then
      AColor := $EFEFEF
    else
      AColor := $FFFFFF;

    InternalDrawGlyph(DC, R, FMarkArrow[AState], AColor);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
var
  AColor: TColor;
begin
  if LowColors then
    inherited DrawMarkTruncated(DC, R, AState)
  else
  begin
    if AState = DXBAR_NORMAL then
      AColor := $EFEFEF
    else
      AColor := $FFFFFF;

    InternalDrawGlyph(DC, R, FMarkTruncated[AState], AColor, False);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawMenuSeparatorHorz(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuSeparatorHorz(DC, R)
  else
    FillRectByColor(DC, cxRectCenterVertically(R, 1), $E3E3E3);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarPopup(DC, R)
  else
    DrawFrame(DC, R, GetTabGroupsAreaContentColor, GetFrameColor);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButton(DC, R, AState)
  else
  begin
    if AState = DXBAR_ACTIVE then
      AState := DXBAR_HOT;
    DrawColorfulButton(DC, R, AState);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect;
  AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC, R, AState)
  else
    DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect;
  AState: Integer);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE:
        DrawFrame(DC, R, GetMasterColor, GetFrameColor, [bLeft..bBottom], 1, True);
      DXBAR_DROPPEDDOWN:
        DrawColorfulButton(DC, R, DXBAR_HOT);
    else
      DrawColorfulButton(DC, R, AState);
    end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean);
begin
  if AUseAeroGlass then
    Parts[FTabsAreaOnGlass].DrawColored(DC, cxRectInflate(R, 0, 0, 0, -1), GetMasterColor);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0);
var
  AColor: TColor;
begin
  if LowColors then
    inherited DrawScrollArrow(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_NORMAL, DXBAR_ACTIVE:
        AColor := $777777;
      DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
        AColor := $727272;
      DXBAR_HOT:
        AColor := $444444;
      DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        AColor := $000000;
      else
        AColor := GetPartColor(DXBAR_ARROWDOWN, AState);
    end;
    Parts[FScrollArrow].DrawColored(DC, R, AColor);
  end;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawSeparatorBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawSeparatorBackground(DC, R)
  else
    FillRectByColor(DC, R, $E0E0E0);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
const
  CheckedColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $F2D5C2, $B7D59F, $B6CDFC, $DEBFE0);
begin
  if LowColors or not (AState in [DXBAR_CHECKED, DXBAR_DROPPEDDOWN]) then
    inherited DrawSmallButton(DC, R, AState)
  else
    FillRectByColor(DC, R, CheckedColorMap[ColorSchemeAccent]);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonArrowPart(DC, R, AState)
  else
    if AState in [DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED] then
      DrawFrame(DC, R, $FFFFFF, GetHighlightContentColor, [bTop..bBottom], 1, True)
    else
      DrawSmallButton(DC, R, AState);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonMainPart(DC, R, AState)
  else
    if AState in [DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED] then
      DrawFrame(DC, R, $FFFFFF, GetHighlightContentColor, [bLeft, bTop, bBottom], 1, True)
    else
      DrawSmallButton(DC, R, AState);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer;
  AIsInPopup: Boolean);
begin
  if LowColors or not AIsInPopup then
    inherited DrawTabGroupBackground(DC, R, AState, AIsInPopup)
  else
    DrawFrame(DC, R, GetPartColor(DXBAR_MENUCONTENT), GetFrameColor, [bLeft..bRight]);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer;
  AIsInPopup: Boolean);
begin
  if LowColors or not AIsInPopup then
    inherited DrawTabGroupHeaderBackground(DC, R, AState, AIsInPopup)
  else
    DrawFrame(DC, R, GetPartColor(DXBAR_MENUCONTENT), GetFrameColor, [bLeft, bRight, bBottom]);
end;

procedure TdxDarkGrayRibbon2016Skin.DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte);
begin
  if LowColors then
    inherited DrawTabSeparator(DC, R, Alpha)
  else
    dxGpFillRect(DC, R, $ABABAB, Alpha);
end;

function TdxDarkGrayRibbon2016Skin.GetInRibbonGalleryScrollBarButtonGlyphColor(
  AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor;
begin
  case AState of
    DXBAR_NORMAL:
      Result := $444444;
    DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
      Result := $A0A0A0;
    else
      Result := $262626;
  end;
end;

function TdxDarkGrayRibbon2016Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    rspFormCaptionText, rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := $F0F0F0
      else
        Result := $B1B1B1;

    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE:
          Result := $262626;
        DXBAR_HOT:
          Result := $FFFFFF;
        DXBAR_DISABLED:
          Result := $9D9D9D;
      else
        Result := $F0F0F0;
      end;

    DXRIBBON_TAT_SMALLBUTTON:
      Result := GetPartColor(rspTabHeaderText, AState);

    DXBAR_ARROWDOWN:
      if AState = DXBAR_DISABLED then
        Result := $727272
      else
        Result := $444444;

    DXBAR_MENUCONTENT:
      Result := $F0F0F0;

    DXBAR_MENUITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $888888
      else
        Result := $444444;

    DXBAR_ITEMTEXT:
      if AState in [DXBAR_DISABLED, DXBAR_ACTIVEDISABLED] then
        Result := $6A6A6A
      else
        Result := $262626;

    DXBAR_EDIT_BACKGROUND:
      Result := $D4D4D4;

    DXBAR_GALLERYGROUPHEADERBACKGROUND:
      Result := $C6C6C6;

    DXBAR_EDIT_BORDER, DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL:
          Result := $686868;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := GetHighlightBorderColor;
        DXBAR_DISABLED:
          Result := $E1E1E1;
      else
        Result := inherited;
      end;

    rtatpEditText:
      case AState of
        DXBAR_NORMAL:
          Result := $ABABAB;
        DXBAR_DISABLED:
          Result := $989898;
      else
        Result := $FFFFFF;
      end;

    rtatpEditBackground:
      if AState in [DXBAR_NORMAL, DXBAR_DISABLED] then
        Result := GetMasterColor
      else
        Result := $363636;

    DXBAR_DROPDOWNGALLERY:
      Result := $D4D4D4;
    DXBAR_SEPARATOR_TEXTCOLOR:
      Result := $606060;
    DXBAR_BACKSTAGEVIEW:
      Result := $6A6A6A;
    DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
      Result := GetPartColor(rspFormCaptionText, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := GetMasterColor;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH:
      Result := $FFFFFF;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
      case AState of
        DXBAR_NORMAL:
          Result := $FFFFFF;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $B1B1B1;
      else
        Result := $F0F0F0;
      end;

  else
    Result := inherited;
  end;
end;

function TdxDarkGrayRibbon2016Skin.GetScrollBarBackgroundColor: TColor;
begin
  Result := $575757;
end;

function TdxDarkGrayRibbon2016Skin.GetScrollBarGlyphColor: TColor;
begin
  Result := $303030;
end;

function TdxDarkGrayRibbon2016Skin.GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if AState in [DXBAR_HOT, DXBAR_PRESSED] then
    Result := $101010
  else
    Result := $3B3B3B;
end;

function TdxDarkGrayRibbon2016Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if AState = DXBAR_PRESSED then
    Result := $C4C4C4
  else
    Result := $999999;
end;

procedure TdxDarkGrayRibbon2016Skin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
begin
  if LowColors then
    inherited DrawTabAreaButton(DC, R, AState)
  else
    DrawColorfulButton(DC, R, ButtonStateToRibbonState(AState));
end;

function TdxDarkGrayRibbon2016Skin.GetAccentColor: TColor;
const
  ColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($3EA5EA, $9A572B, $467321, $2647D2, $7B3980);
begin
  Result := ColorMap[ColorSchemeAccent];
end;

function TdxDarkGrayRibbon2016Skin.GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor;
const
  ColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $B56D3E, $679443, $3E62F0, $9E56A3);
begin
  if LowColors then
    Result := inherited GetBackstageViewBackButtonGlyphColor(AState)
  else
    if AState in [DXBAR_HOT, DXBAR_PRESSED] then
      Result := ColorMap[ColorSchemeAccent]
    else
      Result := $FFFFFF;
end;

function TdxDarkGrayRibbon2016Skin.GetFormCaptionAreaColor(
  AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor;
begin
  Result := GetMasterColor;
end;

function TdxDarkGrayRibbon2016Skin.GetEditButtonContentColor(AState: Integer): TColor;
const
  HotColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($C8E3FD, $F3E3D7, $E2EED8, $D4E0FF, $F3E2F5);
  PressedColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $DBB5A0, $8DAA70, $88A6F1, $CBA4CE);
begin
  case AState of
    DXBAR_HOT:
      Result := HotColorMap[ColorSchemeAccent];
    DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
      Result := PressedColorMap[ColorSchemeAccent];
    else
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND, AState);
  end;
end;

function TdxDarkGrayRibbon2016Skin.GetFormBorderColor(AActive: Boolean): TColor;
begin
  if AActive then
    Result := GetFrameColor
  else
    Result := $838383;
end;

function TdxDarkGrayRibbon2016Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  if AState in [rbisInactive, rbisHotInactive] then
    Result := $A3A3A3
  else
    Result := $FFFFFF;
end;

function TdxDarkGrayRibbon2016Skin.GetFrameColor: TColor;
begin
  Result := $444444;
end;

function TdxDarkGrayRibbon2016Skin.GetHighlightBorderColor: TColor;
const
  ColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $E3BDA3, $A0BF86, $9DBAF5, $D1A9D4);
begin
  Result := ColorMap[ColorSchemeAccent];
end;

function TdxDarkGrayRibbon2016Skin.GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_NORMAL:
      Result := $444444;
    DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
      Result := $727272;
    else
      Result := $FFFFFF;
  end;
end;

function TdxDarkGrayRibbon2016Skin.GetMasterColor: TColor;
begin
  Result := $6A6A6A;
end;

function TdxDarkGrayRibbon2016Skin.GetMenuBackgroundColor: TColor;
begin
  Result := $262626;
end;

function TdxDarkGrayRibbon2016Skin.GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor;
begin
  Result := $444444;
end;

function TdxDarkGrayRibbon2016Skin.GetName: string;
begin
  Result := 'DarkGray';
end;

function TdxDarkGrayRibbon2016Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016;
end;

function TdxDarkGrayRibbon2016Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $B2B2B2;
end;

function TdxDarkGrayRibbon2016Skin.GetColorfulButtonColor(AState: Integer): TColor;
const
  PressedStateColorMap: array [TdxRibbonColorSchemeAccent] of TColor = ($9CD0F4, $8A4719, $32630A, $1D3BB8, $682F6C);
begin
  case AState of
    DXBAR_HOT, DXBAR_ACTIVE:
      Result := GetAccentColor;
    DXBAR_PRESSED:
      Result := PressedStateColorMap[ColorSchemeAccent];
  else
    Result := clNone;
  end;
end;

{ TdxWhiteRibbon2016Skin }

function TdxWhiteRibbon2016Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016;
end;

{ RegisterSkins }

procedure RegisterSkins;
begin
  if CheckGdiPlus(True) then
  begin
    dxRibbonSkinsManager.Add(TdxColorfulRibbon2016Skin.Create);
    dxRibbonSkinsManager.Add(TdxColorfulRibbon2016TabletSkin.Create);
    dxRibbonSkinsManager.Add(TdxDarkGrayRibbon2016Skin.Create);
    dxRibbonSkinsManager.Add(TdxLightGrayRibbon2016Skin.Create);
    dxRibbonSkinsManager.Add(TdxMediumGrayRibbon2016Skin.Create);
    dxRibbonSkinsManager.Add(TdxWhiteRibbon2016Skin.Create);
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterSkins, nil);
end.

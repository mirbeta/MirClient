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

unit dxRibbonSkins2013;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, Graphics, Forms, dxCore, dxGDIPlusAPI, dxGDIPlusClasses, cxGraphics, cxGeometry,
  dxBar, dxBarSkin, dxBarSkinConsts, cxLookAndFeelPainters, dxRibbonSkins, dxRibbonSkins2010;

type

  { TdxCustomRibbon2013Skin }

  TdxCustomRibbon2013Skin = class(TdxCustomRibbon2010Skin)
  protected const
    BorderIconStateToBarState: array[TdxRibbonBorderIconState] of Integer = (
      DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_NORMAL, DXBAR_HOT
    );
  protected
    FBackstageViewBackButton: TTwoStateArray;

    // Color Palettes
    function CreateBackstageViewMenuPalette: IdxRibbonSkinColorPaletteSet; virtual;
    function CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet; override;
    procedure InitializeColorPalettes; override;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;

    procedure LoadCommonBackstageView(ABitmap: Pointer); override;
    procedure LoadCommonTexturesSet(AImage: TdxGPImage); override;
    procedure LoadGlyphs(ABitmap: GpBitmap; var AParts; const R: TRect; AID: Integer;
      AStateCount: Integer; AInterpolationMode: Integer = InterpolationModeNearestNeighbor);
    procedure LoadRibbonFormBorderIcons(ABitmap: Pointer); override;
    procedure LoadRibbonFormBorderIconsGlyphs(ABitmap: GpBitmap; X, Y, AWidth, AHeight: Integer); override;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;

    // ColorfulButton
    procedure DrawColorfulButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    function GetColorfulButtonColor(AState: Integer): TColor; virtual;

    function GetAccentColor(const ALightnessDelta: Double): TColor; overload;
    function GetAccentColor: TColor; overload; virtual;
    function GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor; virtual;
    function GetFormCaptionAreaColor(AApplicationMenuState: TdxRibbonApplicationMenuState = ramsHidden; AActive: Boolean = True): TColor; virtual;
    function GetFormBackgroundColor1: TColor; virtual;
    function GetFormBackgroundColor2: TColor; virtual;
    function GetFormBorderColor(AActive: Boolean): TColor; virtual;
    function GetFormBorderIconGlyphColor(AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor; virtual;
    function GetFrameColor: TColor; virtual;
    function GetGlyphColor(AState: Integer): TColor; virtual;
    function GetHighlightBorderColor: TColor; virtual;
    function GetHighlightContentColor: TColor; virtual;
    function GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor; virtual;
    function GetMasterColor: TColor; virtual; abstract;
    function GetMenuBackgroundColor(const ALightnessDelta: Double): TColor; overload;
    function GetMenuBackgroundColor: TColor; overload; virtual;
    function GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor; virtual;
    function GetStyle: TdxRibbonStyle; override;
    function GetTabGroupsAreaContentColor: TColor; virtual; abstract;
  public
    procedure InitializePaintData(const APaintData: IdxRibbonPaintData); override;

    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawRibbonClientTopArea(DC: HDC; const R: TRect); override;

    // Application Menu
    procedure DrawApplicationMenuBackground(DC: HDC; const R: TRect; const AContentRect: TRect); override;
    procedure DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawApplicationMenuExtraPanePinButtonGlyph(DC: HDC; const R: TRect; AState: Integer; AChecked: Boolean); override;
    procedure DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;

    // BackstageView
    procedure AdjustBackstageViewTabButtonFont(AFont: TFont); override;
    procedure DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuHeader(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;

    // ButtonGroup
    procedure DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBorderLeft(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBorderRight(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer); override;

    // Collapsed Toolbar
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;

    // Context
    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); override;
    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); override;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;
    procedure DrawContextTabGroupsArea(DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean); override;

    // Gallery
    procedure DrawDropDownGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawGalleryFilterBandBackground(DC: HDC; const R: TRect); override;
    procedure DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect); override;
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); override;

    // Edit
    procedure DrawEditButton(DC: HDC; const R: TRect; AState: Integer); override;

    // Form
    procedure DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); override;
    procedure DrawFormBorderIconBackground(DC: HDC; const R: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); virtual;
    procedure DrawFormBorders(DC: HDC; const ABordersWidth: TRect); override;
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    procedure DrawFormStatusBarPart(DC: HDC; const R: TRect; AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean); override;
    procedure DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer); override;
    function GetRibbonTopFrameAreaSeparatorSize: Integer; override;

    // SmallButton
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;

    // LargeButton
    procedure DrawLargeButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;

    // LaunchButton
    procedure DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer); override;

    //Menu
    procedure DrawDropDownBorder(DC: HDC; const R: TRect); override;
    procedure DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuContent(DC: HDC; const R: TRect); override;
    procedure DrawMenuGlyph(DC: HDC; const R: TRect); override;
    procedure DrawMenuSeparatorHorz(DC: HDC; const R: TRect); override;
    procedure DrawMenuSeparatorVert(DC: HDC; const R: TRect); override;

    // QuickAccessToolbar
    procedure DrawQuickAccessToolbar(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean); override;
    procedure DrawQuickAccessToolbarGroupButton(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;

    // ScrollButton
    procedure DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    procedure DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    function GetGroupScrollButtonBorderColor(AState: Integer): TColor; virtual;
    function GetGroupScrollButtonContentColor(AState: Integer): TColor; virtual;

    // ScrollBar
    procedure DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawScrollBarPart(DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean); override;
    procedure DrawScrollBarPartGlyph(DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean); virtual;
    procedure DrawScrollBoxSizeGripArea(DC: HDC; const R: TRect); override;
    function GetScrollBarBackgroundColor: TColor; virtual;
    function GetScrollBarGlyphColor: TColor; virtual;
    function GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor; virtual;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; virtual;

    // Separator
    procedure DrawSeparatorBackground(DC: HDC; const R: TRect); override;
    procedure DrawSeparatorLine(DC: HDC; const R: TRect); override;

    // StatusBar
    procedure DrawStatusBar(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarGripBackground(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarPanel(DC: HDC; const Bounds: TRect; const R: TRect; AIsLowered: Boolean); override;
    procedure DrawStatusBarPanelSeparator(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect); override;

    // Tab
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); override;
    function GetTabAreaBackgroundColor(AApplicationMenuState: TdxRibbonApplicationMenuState): TColor; virtual;

    // TabGroup
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean); override;

    procedure DrawMinimizeRibbonButtonGlyph(DC: HDC; const R: TRect;
      AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph); override;
    procedure DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawKeyTip(DC: HDC; const R: TRect); override;

    function HasExternalRibbonFormShadow: Boolean; override;
    function GetPartContentOffsets(APart: Integer): TRect; override;
    function GetPartSize(APart: Integer): Integer; override;
    function GetWindowBordersWidth(AHasStatusBar: Boolean): TRect; override;
    function UseRoundedWindowCorners: Boolean; override;
  end;

  { TdxLightGrayRibbon2013Skin }

  TdxLightGrayRibbon2013Skin = class(TdxCustomRibbon2013Skin)
  protected
    function GetName: string; override;
    function GetFormBackgroundColor2: TColor; override;
    function GetMasterColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    // ScrollBar
    function GetScrollBarBackgroundColor: TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

  { TdxDarkGrayRibbon2013Skin }

  TdxDarkGrayRibbon2013Skin = class(TdxCustomRibbon2013Skin)
  protected
    function GetName: string; override;
    function GetFormBackgroundColor2: TColor; override;
    function GetFrameColor: TColor; override;
    function GetMasterColor: TColor; override;
    function GetMenuBackgroundColor: TColor; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    // ScrollBar
    function GetScrollBarBackgroundColor: TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

  { TdxWhiteRibbon2013Skin }

  TdxWhiteRibbon2013Skin = class(TdxCustomRibbon2013Skin)
  protected
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetName: string; override;
    function GetFormBackgroundColor2: TColor; override;
    function GetMasterColor: TColor; override;
    function GetTabGroupsAreaContentColor: TColor; override;
  public
    // ScrollBar
    function GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
    function GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor; override;
  end;

implementation

uses
  Math, SysUtils, cxControls;

{$R dxRibbonSkins2013.res}

{ TdxCustomRibbon2013Skin }

procedure TdxCustomRibbon2013Skin.DrawFormStatusBarPart(DC: HDC; const R: TRect;
  AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean);
begin
  FillRectByColor(DC, R, GetFormBorderColor(AIsActive));
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean);
begin
  DrawStatusBar(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawTabAreaBackground(
  DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean; AApplicationMenuState: TdxRibbonApplicationMenuState);
begin
  if LowColors then
    inherited DrawTabAreaBackground(DC, R, AActive, AUseAeroGlass, AApplicationMenuState)
  else
    if not AUseAeroGlass then
      FillRectByColor(DC, R, GetTabAreaBackgroundColor(AApplicationMenuState));
end;

function TdxCustomRibbon2013Skin.GetTabAreaBackgroundColor(AApplicationMenuState: TdxRibbonApplicationMenuState): TColor;
begin
  Result := GetFormCaptionAreaColor(AApplicationMenuState);
end;

procedure TdxCustomRibbon2013Skin.InitializePaintData(const APaintData: IdxRibbonPaintData);
var
  APrevColorSchemeAccent: TdxRibbonColorSchemeAccent;
begin
  APrevColorSchemeAccent := ColorSchemeAccent;
  inherited;
  if APrevColorSchemeAccent <> ColorSchemeAccent then
    InitializeColorPalettes;
end;

procedure TdxCustomRibbon2013Skin.DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
begin
  if LowColors then
    inherited DrawApplicationButton(DC, R, AState)
  else
    case AState of
      rabsNormal, rabsPressed:
        FillRectByColor(DC, R, GetMenuBackgroundColor);
      rabsHot:
        FillRectByColor(DC, R, GetAccentColor(0.1));
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawRibbonClientTopArea(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawRibbonClientTopArea(DC, R)
  else
    FillRectByColor(DC, R, GetFormCaptionAreaColor);
end;

procedure TdxCustomRibbon2013Skin.DrawApplicationMenuBackground(DC: HDC; const R: TRect; const AContentRect: TRect);
begin
  if LowColors then
    inherited DrawApplicationMenuBackground(DC, R, AContentRect)
  else
  begin
    FillRectByColor(DC, R, GetMasterColor);
    FillRectByColor(DC, cxRectSetTop(R, AContentRect.Bottom, R.Bottom - AContentRect.Bottom), $F0F0F0);
    FillRectByColor(DC, cxRectSetBottom(R, AContentRect.Top, AContentRect.Top - R.Top), GetMenuBackgroundColor);
    DrawFrame(DC, R, clNone, GetFormBorderColor(True));
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawApplicationMenuExtraPanePinButtonGlyph(
  DC: HDC; const R: TRect; AState: Integer; AChecked: Boolean);
begin
  InternalDrawGlyph(DC, R, FPinButtonGlyphs[AChecked],
    GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH, AState), False);
end;

procedure TdxCustomRibbon2013Skin.DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawMenuExtraSeparator(DC, R, AHorizontal)
  else
    InternalDrawSeparator(DC, R, AHorizontal, clWhite, GetFrameColor);
end;

procedure TdxCustomRibbon2013Skin.AdjustBackstageViewTabButtonFont(AFont: TFont);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer);

  procedure DoDrawPart(APart: TdxSkinnedRect; AColor: TColor);
  const
    SizeOfSmoothingZone = 2;
  var
    AImage: TdxSmartImage;
    AImageCanvas: TdxGPCanvas;
    ARect: TRect;
  begin
    AImage := TdxSmartImage.CreateSize(cxRect(0, 0, APart.Size.cx + 2 * SizeOfSmoothingZone, APart.Size.cy + 2 * SizeOfSmoothingZone), 0);
    try
      AImageCanvas := AImage.CreateCanvas;
      try
        APart.DrawEx(AImageCanvas.Handle, cxRectCenter(AImage.ClientRect, APart.Size));
      finally
        AImageCanvas.Free;
      end;
      AImage.ChangeColor(AColor);
      ARect := R;
      cxRightToLeftDependentDraw(DC, ARect, UseRightToLeftAlignment,
        procedure
        begin
          AImage.StretchDraw(DC, ARect);
        end);
    finally
      AImage.Free;
    end;
  end;

var
  ABackgroundColor: TColor;
  AGlyphColor: TColor;
begin
  if LowColors then
  begin
    ABackgroundColor := $FCFCFC;
    AGlyphColor := clBlack;
  end
  else
  begin
    ABackgroundColor := GetMenuBackgroundColor;
    AGlyphColor := GetBackstageViewBackButtonGlyphColor(AState);
  end;

  DoDrawPart(Parts[FBackstageViewBackButton[True]], ABackgroundColor);
  DoDrawPart(Parts[FBackstageViewBackButton[False]], AGlyphColor);
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER));
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewMenuBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewMenuBackground(DC, R)
  else
    FillRectByColor(DC, R, GetMenuBackgroundColor);
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuButton(DC, R, AState)
  else
    DrawBackstageViewTabButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewMenuHeader(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawBackstageViewMenuHeader(DC, R)
  else
    DrawBackstageViewMenuBackground(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewMenuSeparator(DC, R, AState)
  else
    FillRectByColor(DC, cxRectInflate(cxRectCenterVertically(R, 1), -15, 0), GetPartColor(DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR));
end;

procedure TdxCustomRibbon2013Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewTabButton(DC, R, AState)
  else
    case AState of
      DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        FillRectByColor(DC, R, GetMenuBackgroundColor(-0.08));
      DXBAR_CHECKED:
        FillRectByColor(DC, R, GetMenuBackgroundColor(0.08));
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupBorderLeft(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupBorderRight(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonArrowPart(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawButtonGroupSplitButtonSeparator(
  DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarGlyphBackground(DC, R, AState)
  else
    DrawFrame(DC, R, GetPartColor(DXBAR_EDIT_BACKGROUND), GetFrameColor);
end;

procedure TdxCustomRibbon2013Skin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarBackground(DC, R, AState)
  else
  begin
    case AState of
      DXBAR_HOT:
        FillRectByColor(DC, R, $ECECEC);
      DXBAR_PRESSED:
        FillRectByColor(DC, R, $E6E6E6);
    end;
    FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), GetFrameColor);
  end;
end;

procedure TdxCustomRibbon2013Skin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
begin
  if LowColors then
    inherited AdjustContextFont(AFont, AUseGlass, AContextColor)
  else
    AFont.Color := TdxColorHelper.ChangeLightness(AContextColor, -0.2);
end;

procedure TdxCustomRibbon2013Skin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  if LowColors or not (AState in [DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED]) then
    inherited AdjustContextTabFont(AFont, AState, AContextColor)
  else
    AFont.Color := TdxColorHelper.ChangeLightness(AContextColor, -0.2);
end;

procedure TdxCustomRibbon2013Skin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
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
    dxGpFillRect(DC, R1, clWhite, 200);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor);
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(R, True);
  try
    DrawContextBackground(ABitmap.Canvas.Handle, ABitmap.ClientRect, AContextColor);
    cxBitBlt(DC, ABitmap.Canvas.Handle, R, cxNullPoint, SRCCOPY);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawContextTabBackground(
  DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawContextTabBackground(DC, R, AState, AContextColor)
  else
  begin
    R1 := cxRectSetHeight(R, cxRectHeight(R) - GetPartSize(DXBAR_TABSGROUPSOVERLAPHEIGHT));
    dxGpFillRect(DC, R1, AContextColor);
    dxGpFillRect(DC, R1, clWhite, 200);
    DrawTab(DC, R, AState);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawContextTabGroupsArea(
  DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if LowColors then
    inherited DrawContextTabGroupsArea(DC, R, AContextColor, AIsQATAtBottom, AIsInPopup)
  else
    DrawTabGroupsArea(DC, R, AIsQATAtBottom, AIsInPopup);
end;

procedure TdxCustomRibbon2013Skin.DrawContextTabSeparator(
  DC: HDC; const R: TRect; ABeginGroup: Boolean);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawDropDownGalleryBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBackground(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_EDIT_BACKGROUND));
end;

procedure TdxCustomRibbon2013Skin.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryBottomSizingBand(DC, R)
  else
    DrawDropDownGalleryBackground(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownGalleryTopSizingBand(DC, R)
  else
    DrawDropDownGalleryBackground(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawGalleryFilterBandBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawGalleryFilterBandBackground(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_GALLERYFILTERBAND));
end;

procedure TdxCustomRibbon2013Skin.DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_GALLERYGROUPHEADERBACKGROUND));
end;

procedure TdxCustomRibbon2013Skin.DrawInRibbonGalleryScrollBarButton(
  DC: HDC; const R: TRect; AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);

  function GetBorderColor(AState: Integer): TColor;
  begin
    case AState of
      DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        Result := GetHighlightBorderColor;
    else
      Result := GetPartColor(DXBAR_EDIT_BORDER);
    end;
  end;

  function GetContentColor(AState: Integer): TColor;
  begin
    case AState of
      DXBAR_HOT:
        Result := GetHighlightContentColor;
      DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        Result := GetHighlightBorderColor;
    else
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
    end;
  end;

const
  OuterBorders: array[TdxInRibbonGalleryScrollBarButtonKind] of TcxBorders = (
    [bTop, bRight], [bRight], [bBottom, bRight]
  );
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
      DrawFrame(DC, R1, clNone, GetBorderColor(DXBAR_NORMAL), OuterBorders[AButtonKind]);
      DrawInRibbonGalleryScrollBarButtonGlyph(DC, R, AButtonKind, AState);
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawEditButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawEditButton(DC, R, AState)
  else
    case AState of
      DXBAR_HOT:
        FillRectByColor(DC, R, GetHighlightContentColor);
      DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
        FillRectByColor(DC, R, GetHighlightBorderColor);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawFormBorderIcon(
  DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
const
  AlignmentMap: array[Boolean] of TAlignment = (taRightJustify, taCenter);
begin
  DrawFormBorderIconBackground(DC, R, AIcon, AState);
  InternalDrawFormBorderIconGlyph(DC, R, AIcon, AState,
    GetFormBorderIconGlyphColor(AIcon, AState), AlignmentMap[AIcon <> rbdiAutoHideModeShowUI]);
end;

procedure TdxCustomRibbon2013Skin.DrawFormBorderIconBackground(DC: HDC; const R: TRect;
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
begin
  DrawEditButton(DC, R, BorderIconStateToBarState[AState]);
end;

procedure TdxCustomRibbon2013Skin.DrawFormBorders(DC: HDC; const ABordersWidth: TRect);
var
  ASaveIndex: Integer;
begin
  if LowColors then
    inherited DrawFormBorders(DC, ABordersWidth)
  else
  begin
    ASaveIndex := SaveDC(DC);
    try
      cxExcludeClipRect(DC, cxRectContent(FormPaintData.GetBounds, ABordersWidth));
      FillRectByColor(DC, FormPaintData.GetBounds, GetFormBorderColor(FormPaintData.GetIsActive));
    finally
      RestoreDC(DC, ASaveIndex)
    end;
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawFormCaption(DC: HDC; const R: TRect);
const
  BordersMap: array[Boolean] of TcxBorders = ([bTop], cxBordersAll);
var
  AIsActive: Boolean;
begin
  if LowColors then
    inherited DrawFormCaption(DC, R)
  else
  begin
    AIsActive := FormPaintData.GetIsActive;
    DrawFrame(DC, R,
      GetFormCaptionAreaColor(PaintData.GetApplicationMenuState, AIsActive),
      GetFormBorderColor(AIsActive), BordersMap[FormPaintData.GetState = wsMinimized]);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer);
var
  AColor1, AColor2: TColor;
begin
  if LowColors then
    inherited DrawRibbonFormBackground(DC, R, ARibbonHeight)
  else
  begin
    if UseRightToLeftAlignment then
    begin
      AColor1 := GetFormBackgroundColor2;
      AColor2 := GetFormBackgroundColor1;
    end
    else
    begin
      AColor1 := GetFormBackgroundColor1;
      AColor2 := GetFormBackgroundColor2;
    end;
    DrawRibbonBackground(DC, cxRectSetHeight(R, ARibbonHeight));
    FillGradientRect(DC, cxRectSetBottom(R, R.Bottom, cxRectHeight(R) - ARibbonHeight), AColor1, AColor2, False);
  end;
end;

function TdxCustomRibbon2013Skin.GetRibbonTopFrameAreaSeparatorSize: Integer;
begin
  Result := 2;
end;

procedure TdxCustomRibbon2013Skin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButton(DC, R, AState)
  else
    case AState of
      DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        dxGpFillRect(DC, R, GetHighlightContentColor);
      DXBAR_CHECKED, DXBAR_DROPPEDDOWN, DXBAR_PRESSED:
        dxGpFillRect(DC, R, GetHighlightBorderColor);
      DXBAR_HOTCHECK:
        DrawFrame(DC, R, GetHighlightContentColor, GetHighlightBorderColor, cxBordersAll, 1, True);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonArrowPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        DrawFrame(DC, R, $FEFEFE, GetHighlightContentColor, [bTop..bBottom], 1, True);
      else
        DrawSmallButton(DC, R, AState);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawSmallButtonDropButtonMainPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        DrawFrame(DC, R, $FEFEFE, GetHighlightContentColor, [bLeft, bTop, bBottom], 1, True);
      else
        DrawSmallButton(DC, R, AState);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawLargeButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButton(DC, R, AState)
  else
    DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButtonDropButtonArrowPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        DrawFrame(DC, R, clNone, GetHighlightContentColor, [bLeft, bRight, bBottom]);
    else
      DrawLargeButton(DC, R, AState);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLargeButtonDropButtonMainPart(DC, R, AState)
  else
    case AState of
      DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        DrawFrame(DC, R, clNone, GetHighlightContentColor, [bLeft, bTop, bRight]);
      DXBAR_NORMAL:
        begin
          DrawLargeButton(DC, R, AState);
          DrawFrame(DC, cxRectInflate(R, -6, 0), clNone, $D4D4D4, [bBottom]);
        end;
    else
      DrawLargeButton(DC, R, AState);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLaunchButtonBackground(DC, R, AState)
  else
    DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawLaunchButtonDefaultGlyph(DC, R, AState)
  else
    Parts[FLaunchButtonDefaultGlyphs[AState]].DrawColored(DC, R, GetLaunchButtonDefaultGlyphColor(AState), 255, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbon2013Skin.DrawDropDownBorder(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawDropDownBorder(DC, R)
  else
    DrawFrame(DC, R, GetPartColor(DXBAR_EDIT_BACKGROUND), GetPartColor(DXBAR_EDIT_BORDER));
end;

procedure TdxCustomRibbon2013Skin.DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawMenuCheck(DC, R, AState)
  else
    DrawFrame(DC, R, GetHighlightContentColor, GetHighlightBorderColor);
end;

procedure TdxCustomRibbon2013Skin.DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawMenuCheckMark(DC, R, AState)
  else
    InternalDrawGlyph(DC, R, FMenuCheckMark[AState], clDefault, False);
end;

procedure TdxCustomRibbon2013Skin.DrawMenuContent(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuContent(DC, R)
  else
    FillRectByColor(DC, R, GetPartColor(DXBAR_MENUCONTENT));
end;

procedure TdxCustomRibbon2013Skin.DrawMenuGlyph(DC: HDC; const R: TRect);
begin
  DrawMenuContent(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawMenuSeparatorHorz(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawMenuSeparatorHorz(DC, R)
  else
    FillRectByColor(DC, cxRectCenterVertically(R, 1), GetPartColor(DXBAR_MENUSEPARATORHORZ));
end;

procedure TdxCustomRibbon2013Skin.DrawMenuSeparatorVert(DC: HDC; const R: TRect);
begin
  DrawMenuContent(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawQuickAccessToolbar(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawQuickAccessToolbarGroupButton(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawQuickAccessToolbarPopup(DC, R)
  else
    DrawFrame(DC, R, GetFormCaptionAreaColor, $777777);
end;

procedure TdxCustomRibbon2013Skin.DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
begin
  if LowColors then
    inherited DrawGroupScrollButton(DC, R, ALeft, AState)
  else
    DrawFrame(DC, R, GetGroupScrollButtonContentColor(AState), GetGroupScrollButtonBorderColor(AState));
end;

function TdxCustomRibbon2013Skin.GetGroupScrollButtonBorderColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := GetHighlightBorderColor;
    DXBAR_PRESSED:
      Result := TdxColorHelper.ChangeLightness(GetHighlightBorderColor, -0.1);
  else
    Result := GetFrameColor;
  end;
end;

function TdxCustomRibbon2013Skin.GetGroupScrollButtonContentColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := GetHighlightContentColor;
    DXBAR_PRESSED:
      Result := GetHighlightBorderColor;
  else
    Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
begin
  DrawGroupScrollButton(DC, R, ALeft, AState);
end;

procedure TdxCustomRibbon2013Skin.DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawScrollBarBackground(DC, R, AHorizontal)
  else
    FillRectByColor(DC, R, GetScrollBarBackgroundColor);
end;

procedure TdxCustomRibbon2013Skin.DrawScrollBarPart(DC: HDC;
  const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawScrollBarPart(DC, R, APart, AState, AHorizontal)
  else
  begin
    if APart in [sbpThumbnail, sbpLineUp, sbpLineDown] then
      DrawFrame(DC, R, GetScrollBarPartContentColor(APart, AState), GetScrollBarPartBorderColor(APart, AState));
    DrawScrollBarPartGlyph(DC, R, APart, AState, AHorizontal);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawScrollBarPartGlyph(
  DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean);
begin
  case APart of
    sbpLineUp:
      InternalDrawGlyph(DC, R, FScrollBarButtonLeftTopGlyph[AHorizontal], GetScrollBarGlyphColor, False);
    sbpLineDown:
      InternalDrawGlyph(DC, R, FScrollBarButtonRightBottomGlyph[AHorizontal], GetScrollBarGlyphColor, False);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawScrollBoxSizeGripArea(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clWhite);
end;

function TdxCustomRibbon2013Skin.GetScrollBarBackgroundColor: TColor;
begin
  Result := $F3F3F3;
end;

function TdxCustomRibbon2013Skin.GetScrollBarGlyphColor: TColor;
begin
  Result := clDefault;
end;

function TdxCustomRibbon2013Skin.GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT, DXBAR_PRESSED:
      Result := $777777;
  else
    Result := $ABABAB;
  end;
end;

function TdxCustomRibbon2013Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  Result := clWhite;
end;

procedure TdxCustomRibbon2013Skin.DrawSeparatorBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawSeparatorBackground(DC, R)
  else
    FillRectByColor(DC, R, $EEEEEE);
end;

procedure TdxCustomRibbon2013Skin.DrawSeparatorLine(DC: HDC; const R: TRect);
begin
  DrawSeparatorBackground(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBar(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawStatusBar(DC, R)
  else
    FillRectByColor(DC, R, GetMenuBackgroundColor);
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBarGripBackground(DC: HDC; const R: TRect);
begin
  DrawStatusBar(DC, R);
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBarSizeGrip(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBarPanelSeparator(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbon2013Skin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  if LowColors then
    inherited DrawTab(DC, R, AState)
  else
    case AState of
      rtsActive, rtsActiveHot:
        DrawFrame(DC, R, GetTabGroupsAreaContentColor, GetFrameColor, [bLeft..bRight], 1, True);
      rtsFocused:
        DrawFrame(DC, R, GetHighlightContentColor, GetHighlightBorderColor, [bLeft..bRight], 1, True);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawTabGroupBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawTabGroupBackground(DC, R, AState, AIsInPopup)
  else
    if AIsInPopup then
      DrawFrame(DC, R, $FFFFFF, $C6C6C6, [bLeft..bRight])
    else
    begin
      R1 := cxRectSetRight(R, R.Right, 1);
      if UseRightToLeftAlignment then
        R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
      FillRectByColor(DC, R1, GetFrameColor);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawTabGroupHeaderBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R1: TRect;
begin
  if LowColors then
    inherited DrawTabGroupHeaderBackground(DC, R, AState, AIsInPopup)
  else
    if AIsInPopup then
      DrawFrame(DC, R, $FFFFFF, $C6C6C6, [bLeft, bRight, bBottom])
    else
    begin
      R1 := cxRectSetRight(R, R.Right, 1);
      if UseRightToLeftAlignment then
        R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
      R1 := cxRectSetHeight(R1, cxRectHeight(R1) - 4);
      FillRectByColor(DC, R1, GetFrameColor);
    end;
end;

procedure TdxCustomRibbon2013Skin.DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if LowColors then
    inherited DrawTabGroupsArea(DC, R, AIsQATAtBottom, AIsInPopup)
  else
  begin
    FillRectByColor(DC, cxRectSetHeight(R, 1), GetFrameColor);
    FillRectByColor(DC, cxRectSetBottom(R, R.Bottom, 1), GetFrameColor);
    FillRectByColor(DC, cxRectInflate(R, 0, -1), GetTabGroupsAreaContentColor);
  end;
end;

procedure TdxCustomRibbon2013Skin.DrawMinimizeRibbonButtonGlyph(
  DC: HDC; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
const
  StateMap: array[TcxButtonState] of Integer = (
    DXBAR_NORMAL, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_DISABLED
  );
begin
  if LowColors then
    inherited DrawMinimizeRibbonButtonGlyph(DC, R, AState, AGlyph)
  else
    InternalDrawGlyph(DC, R,
      FMinimizeRibbonButtonGlyph[AGlyph][AState = cxbsDisabled],
      GetMinimizeRibbonButtonGlyphColor(StateMap[AState]), False);
end;

procedure TdxCustomRibbon2013Skin.DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    inherited DrawItemSeparator(DC, R, AHorizontal)
  else
    InternalDrawSeparator(DC, R, AHorizontal, GetFrameColor, clNone);
end;

procedure TdxCustomRibbon2013Skin.DrawKeyTip(DC: HDC; const R: TRect);
begin
  if LowColors then
    inherited DrawKeyTip(DC, R)
  else
    FillRectByColor(DC, R, $535353);
end;

function TdxCustomRibbon2013Skin.HasExternalRibbonFormShadow: Boolean;
begin
  Result := True;
end;

function TdxCustomRibbon2013Skin.GetPartContentOffsets(APart: Integer): TRect;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW:
      Result := cxNullRect;
    DXBAR_BACKSTAGEVIEW_MENUBAR:
      Result := ScaleFactor.Apply(cxRect(0, 18, 0, 0));
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
      Result := ScaleFactor.Apply(cxRect(16, 0, 0, 0));
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      Result := ScaleFactor.Apply(cxRect(21, 11, 11, 11));
    DXBAR_TOOLBAR:
      Result := cxRect(2, 2, 3, 2);
    DXBAR_RIBBONTABGROUP, DXBAR_RIBBONCONTEXTTABGROUP:
      Result := cxRect(0, 2, 2, 3);
  else
    Result := inherited GetPartContentOffsets(APart);
  end;
end;

function TdxCustomRibbon2013Skin.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    rspTabGroupBottomOffset:
      Result := 0;
    DXBAR_BACKSTAGEVIEW_BACKBUTTON:
      Result := ScaleFactor.Apply(35);
    DXBAR_BACKSTAGEVIEW_MENUBAR_INDENTBETWEENITEMS:
      Result := 0;
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM, DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      Result := ScaleFactor.Apply(38);
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      if LowColors then
        Result := inherited GetPartSize(APart)
      else
        Result := 23;
    DXBAR_BUTTONGROUPSPLITBUTTONSEPARATOR:
      Result := 0;
  else
    Result := inherited GetPartSize(APart);
  end;
end;

function TdxCustomRibbon2013Skin.GetWindowBordersWidth(AHasStatusBar: Boolean): TRect;
begin
  Result := Rect(1, 0, 1, 1);
end;

function TdxCustomRibbon2013Skin.UseRoundedWindowCorners: Boolean;
begin
  Result := False;
end;

function TdxCustomRibbon2013Skin.CreateBackstageViewMenuPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($FFFFFF, $FFFFFF, GetMenuBackgroundColor, $FFFFFF, $FFFFFF, $FFFFFF));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($F0F0F0, $F0F0F0, GetMenuBackgroundColor, $F0F0F0, $F0F0F0, $F0F0F0));
end;

function TdxCustomRibbon2013Skin.CreateMajorColorPalette: IdxRibbonSkinColorPaletteSet;
begin
  Result := TdxRibbonSkinColorPaletteSet.Create;
  Result.Add(DXBAR_NORMAL, TdxRibbonSkinColorPalette.Create($4463D8, $97A776, $FFFFFF, $727272, $82C2EA, $B8824D));
  Result.Add(DXBAR_DISABLED, TdxRibbonSkinColorPalette.Create($BDBDBD, $BDBDBD, $FFFFFF, $BDBDBD, $BDBDBD, $BDBDBD));
end;

procedure TdxCustomRibbon2013Skin.InitializeColorPalettes;
begin
  inherited;
  FColorPaletteBackstageViewMenu := CreateBackstageViewMenuPalette;
end;

function TdxCustomRibbon2013Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    rspApplicationButton:
      Result := clWhite;
    rspContextTextShadow:
      Result := clNone;
    rspTabGroupHeaderText:
      Result := $262626;
    rspRibbonBackground, rfspRibbonForm:
      Result := GetMasterColor;
    DXBAR_MENUCONTENT:
      Result := clWhite;
    DXBAR_BACKSTAGEVIEW:
      Result := clWhite;
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      Result := GetMenuBackgroundColor(0.1);
     DXBAR_MENUARROWSEPARATOR, DXBAR_MENUSEPARATORVERT, DXBAR_MENUSEPARATORHORZ:
      Result := $E5E2DF;
    DXBAR_SEPARATOR_TEXTCOLOR:
      Result := $646464;
    DXBAR_KEYTIP_TEXTCOLOR:
      Result := clWhite;
    DXBAR_INRIBBONGALLERY_BACKGROUND:
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND);
    DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
      Result := clBlack;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR:
      Result := $262626;
    DXBAR_GALLERYFILTERBAND:
      Result := $E5E5E5;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := GetPartColor(DXBAR_GALLERYGROUPHEADERBACKGROUND);
    DXBAR_GALLERYGROUPHEADERBACKGROUND:
      Result := $EEEEEE;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH:
      if AState in [DXBAR_HOT, DXBAR_HOTCHECK] then
        Result := GetAccentColor(0.1)
      else
        Result := clDefault;

    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := GetMenuBackgroundColor(0.2);
      else
        Result := clWhite;
      end;

    rspStatusBarText:
      case AState of
        DXBAR_NORMAL:
          Result := clWhite;
        DXBAR_DISABLED:
          Result := GetMenuBackgroundColor(0.2);
      else
        Result := GetPartColor(DXBAR_ITEMTEXT, AState);
      end;

    rspTabHeaderText:
      case AState of
        DXBAR_ACTIVE, DXBAR_HOT, DXBAR_FOCUSED:
          begin
            Result := GetAccentColor;
            if ColorSchemeAccent = rcsaYellow then
              Result := TdxColorHelper.ChangeLightness(Result, -0.2);
          end;
      else
        Result := GetPartColor(DXBAR_ITEMTEXT, AState);
      end;

    rspFormCaptionText, rspDocumentNameText:
      if AState <> DXBAR_NORMAL then
        Result := $818181
      else
        Result := $292929;

    DXBAR_ITEMTEXT:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $8D8D8D;
      else
        Result := $282828;
      end;

    DXBAR_EDIT_BACKGROUND:
      case AState of
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := $FFFFFF;
      else
        Result := $FEFEFE;
      end;

    DXBAR_ARROWDOWN:
      case AState of
        DXBAR_DISABLED:
          Result := $C6C6C6;
        DXBAR_HOT, DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
          Result := $444444;
      else
        Result := $777777;
      end;

    DXBAR_MENUITEMTEXT:
      case AState of
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := $B1B1B1;
      else
        Result := $646464;
      end;

    DXBAR_EDIT_BORDER, DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_DISABLED, DXBAR_NORMAL:
          Result := GetFrameColor;
        DXBAR_FOCUSED, DXBAR_DROPPEDDOWN, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
          Result := GetHighlightBorderColor;
      else
        Result := inherited;
      end;

    rtatpEditBackground:
      begin
        Result := GetMasterColor;
        if not (AState in [DXBAR_NORMAL, DXBAR_DISABLED]) then
          Result := TdxColorHelper.ChangeLightness(Result, -0.082);
      end;

    rtatpEditText:
      case AState of
        DXBAR_NORMAL:
          Result := $7F7F7F;
        DXBAR_DISABLED:
          Result := $8D8D8D;
      else
        Result := $2B2B2B;
      end;

    DXBAR_RADIALMENUACCENT:
      Result := GetMenuBackgroundColor;
    DXBAR_RADIALMENUBACKGROUND:
      Result := GetPartColor(rspRibbonBackground);

  else
    Result := inherited;
  end;
end;

procedure TdxCustomRibbon2013Skin.LoadCommonBackstageView(ABitmap: Pointer);
begin
  inherited LoadCommonBackstageView(ABitmap);
  LoadElementParts(ABitmap, FBackstageViewBackButton, cxRect(0, 0, 40, 40), rbvpBackstageViewBackButton, cxNullRect, [0, 1], [0, 1]);
end;

procedure TdxCustomRibbon2013Skin.LoadCommonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONCOMMON2013', AImage);
end;

procedure TdxCustomRibbon2013Skin.LoadGlyphs(ABitmap: GpBitmap; var AParts;
  const R: TRect; AID: Integer; AStateCount: Integer; AInterpolationMode: Integer = InterpolationModeNearestNeighbor);
var
  I: Integer;
begin
  for I := 0 to AStateCount - 1 do
    TStatesArray(AParts)[I] := AddPart3x3(ABitmap, R, cxNullRect, AID + I, '', AInterpolationMode);
end;

procedure TdxCustomRibbon2013Skin.LoadRibbonFormBorderIcons(ABitmap: Pointer);

  procedure DoLoadFormBorderIcons(X, Y, W, H: Integer);
  begin
    LoadRibbonFormBorderIconsGlyphs(ABitmap, X, Y, W, H);
    LoadGlyphs(ABitmap, FMDIButtonGlyphs[mdibMinimize], cxRectBounds(X, Y, W, H), rspMDIButtonMinimize, 4);
    LoadGlyphs(ABitmap, FMDIButtonGlyphs[mdibRestore], cxRectBounds(X + 2 * (W + 1), Y, W, H), rspMDIButtonRestore, 4);
    LoadGlyphs(ABitmap, FMDIButtonGlyphs[mdibClose], cxRectBounds(X + 3 * (W + 1), Y, W, H), rspMDIButtonClose, 4);
  end;

begin
  if TargetDPI >= 192 then
    DoLoadFormBorderIcons(0, 576, 30, 22)
  else if TargetDPI >= 144 then
    DoLoadFormBorderIcons(0, 558, 23, 17)
  else
    DoLoadFormBorderIcons(0, 546, 15, 11);

  LoadElementParts(ABitmap, FBorderIcons, cxRectBounds(195, 37, 17, 17), rfspBorderIconBackground,
    DefaultFixedSize, [0, 1, 2], [0, 1, 2], True, InterpolationModeNearestNeighbor);
  LoadElementParts(ABitmap, FCloseButton, cxRectBounds(213, 37, 17, 17), rfspCloseButtonHot,
    DefaultFixedSize, [0, 1, 2], [0, 1, 2], True, InterpolationModeNearestNeighbor);
end;

procedure TdxCustomRibbon2013Skin.LoadRibbonFormBorderIconsGlyphs(ABitmap: GpBitmap; X, Y, AWidth, AHeight: Integer);
var
  I: TdxRibbonBorderDrawIcon;
  ID: Integer;
begin
  ID := rfspBorderIconMinimizeGlyph;
  for I := Low(TdxRibbonBorderDrawIcon) to High(TdxRibbonBorderDrawIcon) do
  begin
    LoadGlyphs(ABitmap, FBorderIconGlyph[I], cxRectBounds(X, Y, AWidth, AHeight), ID, 4, InterpolationModeNearestNeighbor);
    Inc(X, AWidth + 1);
    Inc(ID, 4);
  end;
end;

procedure TdxCustomRibbon2013Skin.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONALL2013', AImage);
end;

function TdxCustomRibbon2013Skin.GetAccentColor: TColor;
const
  ColorMap: array[TdxRibbonColorSchemeAccent] of TColor = (
    $3EA5EA, $9A572B, $457220, $2647D2, $7B3880
  );
begin
  Result := ColorMap[ColorSchemeAccent];
end;

procedure TdxCustomRibbon2013Skin.DrawColorfulButton(DC: HDC; const R: TRect; AState: Integer);
begin
  FillRectByColor(DC, R, GetColorfulButtonColor(AState));
end;

function TdxCustomRibbon2013Skin.GetColorfulButtonColor(AState: Integer): TColor;
begin
  Result := clNone;
end;

function TdxCustomRibbon2013Skin.GetAccentColor(const ALightnessDelta: Double): TColor;
begin
  Result := TdxColorHelper.ChangeLightness(GetAccentColor, ALightnessDelta);
end;

function TdxCustomRibbon2013Skin.GetBackstageViewBackButtonGlyphColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT:
      Result := GetAccentColor(0.3);
    DXBAR_PRESSED:
      Result := GetAccentColor(0.1);
    else
      Result := clWhite;
  end;
end;

function TdxCustomRibbon2013Skin.GetFormCaptionAreaColor(
  AApplicationMenuState: TdxRibbonApplicationMenuState; AActive: Boolean): TColor;
begin
  if AApplicationMenuState = ramsShownAsFullScreenFrame then
    Result := clWhite
  else
    Result := GetMasterColor;
end;

function TdxCustomRibbon2013Skin.GetFormBackgroundColor1: TColor;
begin
  Result := GetMasterColor;
end;

function TdxCustomRibbon2013Skin.GetFormBackgroundColor2: TColor;
begin
  Result := GetFormBackgroundColor1;
end;

function TdxCustomRibbon2013Skin.GetFormBorderColor(AActive: Boolean): TColor;
begin
  if AActive then
    Result := GetMenuBackgroundColor
  else
    Result := $848484;
end;

function TdxCustomRibbon2013Skin.GetFormBorderIconGlyphColor(
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState): TColor;
begin
  Result := GetGlyphColor(BorderIconStateToBarState[AState]);
end;

function TdxCustomRibbon2013Skin.GetFrameColor: TColor;
begin
  Result := $C6C6C6;
end;

function TdxCustomRibbon2013Skin.GetGlyphColor(AState: Integer): TColor;
begin
  case AState of
    DXBAR_HOT, DXBAR_ACTIVEDISABLED:
      Result := TdxColorHelper.ChangeLightness(GetHighlightBorderColor, -0.3);
    DXBAR_PRESSED:
      Result := TdxColorHelper.ChangeLightness(GetHighlightBorderColor, -0.4);
    else
      Result := $777777;
  end;
end;

function TdxCustomRibbon2013Skin.GetHighlightBorderColor: TColor;
const
  ColorMap: array[TdxRibbonColorSchemeAccent] of TColor = (
    $9CD0F4, $E2BCA2, $A0BF86, $9CB9F4, $DEBFE0
  );
begin
  Result := ColorMap[ColorSchemeAccent];
end;

function TdxCustomRibbon2013Skin.GetHighlightContentColor: TColor;
const
  ColorMap: array[TdxRibbonColorSchemeAccent] of TColor = (
    $C8E3FD, $F2E1D5, $E0F0D3, $DDE5FD, $EEDAF0
  );
begin
  Result := ColorMap[ColorSchemeAccent];
end;

function TdxCustomRibbon2013Skin.GetLaunchButtonDefaultGlyphColor(AState: Integer): TColor;
begin
  Result := GetGlyphColor(AState);
end;

function TdxCustomRibbon2013Skin.GetMenuBackgroundColor: TColor;
begin
  Result := GetAccentColor;
end;

function TdxCustomRibbon2013Skin.GetMenuBackgroundColor(const ALightnessDelta: Double): TColor;
begin
  Result := TdxColorHelper.ChangeLightness(GetMenuBackgroundColor, ALightnessDelta);
end;

function TdxCustomRibbon2013Skin.GetMinimizeRibbonButtonGlyphColor(AState: Integer): TColor;
begin
  Result := GetGlyphColor(AState);
end;

function TdxCustomRibbon2013Skin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2013;
end;

{ TdxLightGrayRibbon2013Skin }

function TdxLightGrayRibbon2013Skin.GetScrollBarBackgroundColor: TColor;
begin
  Result := $E6E6E6;
end;

function TdxLightGrayRibbon2013Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if AState = DXBAR_PRESSED then
    Result := $E1E1E1
  else
    Result := clWhite;
end;

function TdxLightGrayRibbon2013Skin.GetName: string;
begin
  Result := 'LightGray';
end;

function TdxLightGrayRibbon2013Skin.GetFormBackgroundColor2: TColor;
begin
  Result := $E8E8E8;
end;

function TdxLightGrayRibbon2013Skin.GetMasterColor: TColor;
begin
  Result := $F6F6F6;
end;

function TdxLightGrayRibbon2013Skin.GetMenuBackgroundColor: TColor;
begin
  Result := GetAccentColor(-0.05);
end;

function TdxLightGrayRibbon2013Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $FCFCFC;
end;

{ TdxDarkGrayRibbon2013Skin }

procedure TdxDarkGrayRibbon2013Skin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawBackstageViewTabButton(DC, R, AState)
  else
    case AState of
      DXBAR_PRESSED, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED:
        FillRectByColor(DC, R, GetAccentColor(-0.1));
      DXBAR_CHECKED:
        FillRectByColor(DC, R, GetAccentColor(0.1));
    end;
end;

function TdxDarkGrayRibbon2013Skin.GetScrollBarBackgroundColor: TColor;
begin
  Result := $D6D6D6;
end;

function TdxDarkGrayRibbon2013Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if AState <> DXBAR_PRESSED then
    Result := clWhite
  else
    if APart = sbpThumbnail then
      Result := $E1E1E1
    else
      Result := $F0F0F0;
end;

function TdxDarkGrayRibbon2013Skin.GetName: string;
begin
  Result := 'DarkGray';
end;

function TdxDarkGrayRibbon2013Skin.GetFormBackgroundColor2: TColor;
begin
  Result := $C1C1C1;
end;

function TdxDarkGrayRibbon2013Skin.GetFrameColor: TColor;
begin
  Result := $ABABAB;
end;

function TdxDarkGrayRibbon2013Skin.GetMasterColor: TColor;
begin
  Result := $E5E5E5;
end;

function TdxDarkGrayRibbon2013Skin.GetMenuBackgroundColor: TColor;
begin
  Result := $333333;
end;

function TdxDarkGrayRibbon2013Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := $F3F3F3;
end;

{ TdxWhiteRibbon2013Skin }

function TdxWhiteRibbon2013Skin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  if APart = rspTabGroupHeaderText then
    Result := $666666
  else
    Result := inherited DoGetPartColor(APart, AState);
end;

function TdxWhiteRibbon2013Skin.GetScrollBarPartBorderColor(APart: TcxScrollBarPart; AState: Integer): TColor;
begin
  if APart <> sbpThumbnail then
    Result := inherited GetScrollBarPartBorderColor(APart, AState)
  else
    if AState = DXBAR_PRESSED then
      Result := $777777
    else
      Result := $ABABAB;
end;

function TdxWhiteRibbon2013Skin.GetScrollBarPartContentColor(APart: TcxScrollBarPart; AState: Integer): TColor;
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

function TdxWhiteRibbon2013Skin.GetName: string;
begin
  Result := 'White';
end;

function TdxWhiteRibbon2013Skin.GetFormBackgroundColor2: TColor;
begin
  Result := clWhite;
end;

function TdxWhiteRibbon2013Skin.GetMasterColor: TColor;
begin
  Result := clWhite;
end;

function TdxWhiteRibbon2013Skin.GetTabGroupsAreaContentColor: TColor;
begin
  Result := clWhite;
end;

{ RegisterSkins }

procedure RegisterSkins;
begin
  if CheckGdiPlus(True) then
  begin
    dxRibbonSkinsManager.Add(TdxWhiteRibbon2013Skin.Create);
    dxRibbonSkinsManager.Add(TdxLightGrayRibbon2013Skin.Create);
    dxRibbonSkinsManager.Add(TdxDarkGrayRibbon2013Skin.Create);
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterSkins, nil);
end.

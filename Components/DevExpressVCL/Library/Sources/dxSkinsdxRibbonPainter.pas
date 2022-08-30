{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
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

unit dxSkinsdxRibbonPainter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, SysUtils, dxSkinsCore, Graphics, Forms, Math,
  dxRibbonSkins, dxBarSkin, cxLookAndFeels, cxLookAndFeelPainters, cxGraphics, cxClasses, dxBarSkinConsts, cxGeometry,
  dxRibbon, dxBar, cxDWMApi, dxSkinInfo, dxGDIPlusAPI, dxGDIPlusClasses, dxOffice11, dxCore, dxCoreClasses,
  dxDPIAwareUtils, dxCoreGraphics;

type
  TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo = class;

  { TdxSkinRibbonPainter }

  TdxSkinRibbonPainterClass = class of TdxSkinRibbonPainter;
  TdxSkinRibbonPainter = class(TdxCustomRibbonSkin)
  strict private
    FContextTabGroupsAreaCache: TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo;
    FPainter: TcxCustomLookAndFeelPainter;

    function GetElementTextColor(AElement: TdxSkinElement; AState: Integer;
      const APropertyPrefix: string = ''; const ADefaultTextColor: TColor = clDefault): TColor;
    function GetPopupMenuColor: TColor;
    function GetSkinInfo: TdxSkinInfo; inline;
    function GetTabAeroSupport: Boolean;
    function GetUseSkins: Boolean;
  protected
    function CorrectTabHeaderRect(const R: TRect): TRect;
    function CorrectTabPanelRect(AIsInPopup: Boolean; const R: TRect): TRect;
    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; override;
    function GetActualColorSchemeAccentImageIndex: Integer;
    function GetApplicationButtonElement: TdxSkinElement; virtual;
    function GetApplicationButtonIndent(const AName: string): Integer;
    function GetBorderBounds(ASide: TcxBorder; const ABorders, ABounds: TRect): TRect;
    function GetBorderIconElement(AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean): TdxSkinElement;
    function GetBorderSkinElement(ASide: TcxBorder; AIsRectangular: Boolean): TdxSkinElement;
    function GetBordersWidth(AHasStatusBar: Boolean): TRect;
    function GetColorPalette(AElement: TdxSkinElement; AState: Integer): IdxColorPalette;
    function GetCustomizeButtonOutsizeQAT(AHasAppButton: Boolean): Boolean;
    function GetElementContentIndents(AElement: TdxSkinElement; AConsideMargins: Boolean; out ALeftIndent, ARightIndent: Integer): Boolean;
    function GetElementMinSize(AElement: TdxSkinElement; AScaleFactor: TdxScaleFactor): TSize;
    function GetName: string; override;
    function GetPropertyColor(AColor: TdxSkinColor): TColor;
    function GetQATBackgroundElement(ABellow, AHasApplicationButton: Boolean): TdxSkinElement;
    function GetQATLeftOffset(AHasApplicationButton: Boolean): Integer;

    function DoDrawButtonGroupElement(DC: HDC; const R: TRect; AState: Integer; AElement: TdxSkinElement): Boolean;
    function DoDrawEditButtonGlyph(DC: HDC; const R: TRect; AState: Integer; AButtonKind: TcxEditBtnKind): Boolean;
    function DoDrawStatusBarBackground(DC: HDC; const R, AVisibleArea: TRect; AIsRaised: Boolean): Boolean;
    function DoDrawStatusBarPart(DC: HDC; const R: TRect; AIsRaised, AActive, AIsLeft: Boolean): Boolean;
    function DoDrawStatusBarRectangularPart(DC: HDC; const R: TRect; AIsRaised, AActive, AIsLeft: Boolean): Boolean;

    function DrawElement(DC: HDC; const R: TRect; AElement: TdxSkinElement): Boolean; overload;
    function DrawElement(DC: HDC; const R: TRect; AState: TdxSkinElementState; AElement: TdxSkinElement; AImageIndex: Integer = 0): Boolean; overload;
    function DrawElement(DC: HDC; const R: TRect; AState: Integer; AElement: TdxSkinElement; AImageIndex: Integer = 0): Boolean; overload;

    procedure DrawClippedElement(DC: HDC; const R: TRect; const ASource: TRect;
      AElement: TdxSkinElement; AState: TdxSkinElementState = esNormal;
      AOperation: TcxRegionOperation = roIntersect; AImageIndex: Integer = 0);
    procedure DrawColoredElement(DC: HDC; const R: TRect;
      AColor: TColor; AElement: TdxSkinElement; AState: TdxSkinElementState);
    procedure DrawFormBorder(DC: HDC; ASide: TcxBorder;
      const ABorderWidths, R: TRect; AElement: TdxSkinElement; AActive: Boolean);
    procedure DrawStatusBarFormBorder(DC: HDC; const ABorders: TRect);

    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); override;

    property ApplicationButtonElement: TdxSkinElement read GetApplicationButtonElement;
    property PopupMenuColor: TColor read GetPopupMenuColor;
    property SkinInfo: TdxSkinInfo read GetSkinInfo;
    property TabAeroSupport: Boolean read GetTabAeroSupport;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; ATargetDPI: Integer = dxDefaultDPI);
    destructor Destroy; override;
    function Clone(ATargetDPI: Integer): TdxCustomRibbonSkin; override;

    //  Application
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); override;
    procedure DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect); override;
    procedure DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawApplicationMenuExtraPaneButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawApplicationMenuExtraPanePinButtonGlyph(DC: HDC; const R: TRect; AState: Integer; AChecked: Boolean); override;

    // BackstageView
    function CanShowTabAreaToolbarInBackstageView: Boolean; override;
    procedure DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewMenuHeader(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); override;
    function GetBackstageViewMenuButtonColorPalette(AState: Integer): IdxColorPalette; override;

    // BackstageViewGallery
    procedure DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect); override;

    // Button Group
    procedure DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBackground(DC: HDC; const R: TRect);
    procedure DrawButtonGroupBorderLeft(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawButtonGroupBorderRight(DC: HDC; const R: TRect); override;
    procedure DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer); override;

    // CollapsedToolbar
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;

    // EditButton
    procedure DrawEditArrowButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawEditEllipsisButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawEditSpinDownButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawEditSpinUpButton(DC: HDC; const R: TRect; AState: Integer); override;

    // Custom controls
    procedure DrawProgressDiscreteBand(DC: HDC; const R: TRect); override;
    procedure DrawProgressSolidBand(DC: HDC; const R: TRect); override;
    procedure DrawProgressSubstrate(DC: HDC; const R: TRect); override;

    // DropDown Gallery
    procedure DrawDropDownBorder(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBackground(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBottomSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryBottomVerticalSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawDropDownGalleryTopVerticalSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawGalleryFilterBandBackground(DC: HDC; const R: TRect); override;
    procedure DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect); override;
    procedure DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer); override;
    procedure DrawInRibbonGalleryBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawInRibbonGalleryScrollBarBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); override;
    procedure DrawInRibbonGalleryScrollBarDropDownTouchButton(DC: HDC; const R: TRect; AState: Integer); override;

    // Form
    procedure DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer); override;
    procedure DrawFormBorders(DC: HDC; const ABordersWidth: TRect); override;
    procedure DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
      AState: TdxRibbonBorderIconState); override;
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    procedure DrawFormStatusBarPart(DC: HDC; const R: TRect; AIsLeft, AIsActive: Boolean;
      AIsRaised, AIsRectangular: Boolean); override;

    // Contexts
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor); override;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); override;
    procedure DrawContextTabGroupsArea(DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean); override;

    // Others
    procedure DrawArrowDown(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawEditButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    procedure DrawGroupScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean); override;
    procedure DrawHelpButton(DC: HDC; const R: TRect; AState: TcxButtonState); override;
    procedure DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawKeyTip(DC: HDC; const R: TRect); override;
    procedure DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); override;
    procedure DrawMDIButtonGlyph(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); override;
    procedure DrawMinimizeRibbonButtonGlyph(DC: HDC; const R: TRect; AState: TcxButtonState;
      AGlyph: TdxRibbonMinimizeButtonGlyph); override;
    procedure DrawRibbonClientTopArea(DC: HDC; const R: TRect); override;
    procedure DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean); override;
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); override;
    procedure DrawScreenTip(DC: HDC; const R: TRect); override;
    procedure DrawSeparatorBackground(DC: HDC; const R: TRect); override;

    // Large buttons
    procedure DrawLargeButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;

    // Launch
    procedure DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer); override;

    // Menus
    procedure DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuContent(DC: HDC; const R: TRect); override;
    procedure DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); override;
    procedure DrawMenuGlyph(DC: HDC; const R: TRect); override;
    procedure DrawMenuItem(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuItemDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuItemDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuMark(DC: HDC; const R: TRect); override;
    procedure DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMenuSeparatorHorz(DC: HDC; const R: TRect); override;
    procedure DrawMenuSeparatorVert(DC: HDC; const R: TRect); override;
    function GetMenuColorPalette(AState: Integer): IdxColorPalette; override;

    // Small buttons
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); override;
    function GetSmallButtonColorPalette(AState: Integer): IdxColorPalette; override;

    // Status Bar
    procedure DrawStatusBar(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarGripBackground(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean); override;
    procedure DrawStatusBarPanelSeparator(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarSizeGrip(DC: HDC; const R: TRect); override;
    procedure DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect); override;
    function GetStatusBarPanelColorPalette(AState: Integer): IdxColorPalette; override;

    // ScrollBar
    function GetScrollBarPainter: TcxCustomLookAndFeelPainter; override;

    // Tabs
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); override;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive: Boolean; AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); override;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); override;
    procedure DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean); override;
    procedure DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); override;
    procedure DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte); override;
    function GetTabAreaButtonColorPalette(AState: Integer): IdxColorPalette; override;

    // MiniToolbar
    procedure DrawMiniToolbarBackground(DC: HDC; const R: TRect); override;
    function GetMiniToolbarColorPalette(AState: Integer): IdxColorPalette; override;

    // QuickAccess
    procedure DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer); override;
    procedure DrawQuickAccessToolbar(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean); override;
    procedure DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect); override;
    procedure DrawQuickAccessToolbarGroupButton(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer); override;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); override;
    function GetQuickAccessToolbarColorPalette(AState: Integer; ABelow: Boolean): IdxColorPalette; override;
    function GetQuickAccessToolbarLeftIndent(AHasApplicationButton, AUseAeroGlass: Boolean): Integer; override;
    function GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton, ABelow: Boolean): Integer; override;
    function GetQuickAccessToolbarOverrideWidth(AHasApplicationButton, AUseAeroGlass: Boolean): Integer; override;
    function GetQuickAccessToolbarRightIndent(AHasApplicationButton: Boolean): Integer; override;

    // Radial Menu
    function GetRadialMenuColorPalette: IdxColorPalette; override;

    function AdjustCaptionFontSize(ASize: Integer; AUseAeroGlass: Boolean): Integer; override;
    procedure AdjustRibbonFormBorderIconSize(AIcon: TdxRibbonBorderDrawIcon;
      AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize); override;
    procedure FlushCache; override;
    function GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect; override;
    function GetApplicationMenuGlyphSize: TSize; override;
    function GetIsAlphaUsed(APart: Integer): Boolean; override;
    function GetMenuSeparatorSize: Integer; override;
    function GetPartContentOffsets(APart: Integer): TRect; override;
    function GetPartSize(APart: Integer): Integer; override;
    function GetSkinName: string; override;
    function GetStatusBarSeparatorSize: Integer; override;
    function GetWindowBordersWidth(AHasStatusBar: Boolean): TRect; override;
    function HasExternalRibbonFormShadow: Boolean; override;
    function IsInternalPainter: Boolean; override;
    function UseRoundedWindowCorners: Boolean; override;
    //
    property Painter: TcxCustomLookAndFeelPainter read FPainter;
    property SkinName: string read GetSkinName;
    property UseSkins: Boolean read GetUseSkins;
  end;

  { TdxSkinRibbon2010Painter }

  TdxSkinRibbon2010Painter = class(TdxSkinRibbonPainter)
  protected
    function GetApplicationButtonElement: TdxSkinElement; override;
    function GetStyle: TdxRibbonStyle; override;
  public
    procedure AdjustBackstageViewTabButtonFont(AFont: TFont); override;
    function ExtendCaptionAreaOnTabs: Boolean; override;
    function GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect; override;
    function GetPartContentOffsets(APart: Integer): TRect; override;
  end;

  { TdxSkinRibbon2013Painter }

  TdxSkinRibbon2013Painter = class(TdxSkinRibbon2010Painter)
  strict private
    FCachedColors: array[Boolean] of TColor;
  protected
    procedure DrawBackstageViewBorders(DC: HDC; const R, ABordersWidth: TRect; AIsActive: Boolean); virtual;
    function GetBackstageViewBorderColor(AIsActive: Boolean): TColor;
    function GetStyle: TdxRibbonStyle; override;
  public
    procedure AfterConstruction; override;
    procedure AdjustBackstageViewTabButtonFont(AFont: TFont); override;
    procedure DrawBackstageViewMenuBackground(DC: HDC; const R: TRect); override;
    procedure DrawBackstageViewMenuHeader(DC: HDC; const R: TRect); override;
    procedure DrawFormBorders(DC: HDC; const BordersWidth: TRect); override;
    procedure DrawFormCaption(DC: HDC; const R: TRect); override;
    function GetPartContentOffsets(APart: Integer): TRect; override;
  end;

  { TdxSkinRibbon2016Painter }

  TdxSkinRibbon2016Painter = class(TdxSkinRibbon2013Painter)
  protected
    function GetStyle: TdxRibbonStyle; override;
  end;

  { TdxSkinRibbon2016TabletPainter }

  TdxSkinRibbon2016TabletPainter = class(TdxSkinRibbon2016Painter)
  protected
    function GetStyle: TdxRibbonStyle; override;
  public
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); override;
    function GetPartSize(APart: Integer): Integer; override;
  end;

  { TdxSkinRibbon2019Painter }

  TdxSkinRibbon2019Painter = class(TdxSkinRibbon2013Painter)
  protected
    function GetStyle: TdxRibbonStyle; override;
  public
    procedure DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer); override;
    function GetPartSize(APart: Integer): Integer; override;
  end;

  { TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo }

  TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo = class(TcxBitmap32)
  public
    ContextColor: TColor;
    Element: TdxSkinElement;
    ImageCount: Integer;

    procedure Check(AElement: TdxSkinElement; AContextColor: TColor; const R: TRect; AScaleFactor: TdxScaleFactor);
  end;

implementation

uses
  dxSkinsStrs, cxControls;

const
  QATLeftDefaultOffset = 15;
  QATRightDefaultOffset = 12;

  RibbonFormBorderStates: array[Boolean] of TdxSkinElementState = (esActiveDisabled, esActive);
  RibbonTabStatesMap: array[TdxRibbonTabState] of TdxSkinElementState = (esNormal, esHot, esActive, esFocused, esFocused);

type
  TdxSkinElementAccess = class(TdxSkinElement);

  { TdxSkinsRibbonPainterManager }

  TdxSkinsRibbonPainterManager = class(TcxIUnknownObject, IcxLookAndFeelPainterListener)
  strict private
    class var FInstance: TdxSkinsRibbonPainterManager;
  strict private
    procedure FreePaintersList;
    procedure InitializePaintersList;
  protected
    procedure AddSkin(APainter: TcxCustomLookAndFeelPainter);
    // IcxLookAndFeelPainterListener
    procedure PainterAdded(APainter: TcxCustomLookAndFeelPainter);
    procedure PainterRemoved(APainter: TcxCustomLookAndFeelPainter);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Register;
    class procedure Unregister;
  end;

function RibbonStateToButtonState(AState: Integer): TcxButtonState;
begin
  case AState of
    DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
      Result := cxbsDisabled;
    DXBAR_DROPPEDDOWN, DXBAR_PRESSED, DXBAR_CHECKED, DXBAR_HOTCHECK:
      Result := cxbsPressed;
    DXBAR_HOT:
      Result := cxbsHot;
    else
      Result := cxbsNormal;
  end;
end;

function RibbonStateToSkinElementState(AState: Integer): TdxSkinElementState;
const
  StateMap: array[0..8] of TdxSkinElementState = (
    esNormal, esDisabled, esHot, esActive, esPressed,
    esChecked, esChecked, esHotCheck, esActiveDisabled
  );
begin
  if (Low(StateMap) <= AState) and (High(StateMap) >= AState) then
    Result := StateMap[AState]
  else
    Result := esNormal;
end;

function SkinElementCheckState(AElement: TdxSkinElement; AState: Integer): TdxSkinElementState;
begin
  Result := dxSkinElementCheckState(AElement, RibbonStateToSkinElementState(AState));
end;

{ TdxSkinRibbonPainter }

constructor TdxSkinRibbonPainter.Create(APainter: TcxCustomLookAndFeelPainter; ATargetDPI: Integer = dxDefaultDPI);
begin
  FPainter := APainter;
  inherited Create(ATargetDPI);
  FContextTabGroupsAreaCache := TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo.Create;
end;

destructor TdxSkinRibbonPainter.Destroy;
begin
  FreeAndNil(FContextTabGroupsAreaCache);
  inherited Destroy;
end;

function TdxSkinRibbonPainter.Clone(ATargetDPI: Integer): TdxCustomRibbonSkin;
begin
  Result := TdxSkinRibbonPainterClass(ClassType).Create(FPainter, ATargetDPI);
end;

function TdxSkinRibbonPainter.AdjustCaptionFontSize(ASize: Integer; AUseAeroGlass: Boolean): Integer;
begin
  Result := inherited AdjustCaptionFontSize(ASize, AUseAeroGlass);
  if not AUseAeroGlass then
  begin
    if UseSkins and (SkinInfo.RibbonCaptionFontDelta <> nil) then
      Inc(Result, SkinInfo.RibbonCaptionFontDelta.Value);
  end;
end;

procedure TdxSkinRibbonPainter.AdjustRibbonFormBorderIconSize(
  AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize);
var
  AElement: TdxSkinElement;
begin
  AElement := GetBorderIconElement(AIcon, AIsToolWindow);
  if AElement <> nil then
  begin
    ASize := TdxSkinElementHelper.CalculateCaptionButtonSize(ASize.cy, AElement);
    ASize := cxSizeMax(ASize, AElement.MinSize.Size);
  end;
end;

procedure TdxSkinRibbonPainter.FlushCache;
begin
  inherited;
  FContextTabGroupsAreaCache.Element := nil;
end;

function TdxSkinRibbonPainter.CorrectTabHeaderRect(const R: TRect): TRect;
begin
  Result := cxRectInflate(R, -1, -2, -1, 0);
end;

function TdxSkinRibbonPainter.CorrectTabPanelRect(AIsInPopup: Boolean; const R: TRect): TRect;
begin
  Result := R;
  if AIsInPopup and (SkinInfo.RibbonTabPanelBottomIndent <> nil) then
    Inc(Result.Bottom, SkinInfo.RibbonTabPanelBottomIndent.Value);
end;

function TdxSkinRibbonPainter.GetActualColorSchemeAccentImageIndex: Integer;
begin
  if Style = rs2007 then
    Result := 0
  else
    Result := Ord(ColorSchemeAccent);
end;

function TdxSkinRibbonPainter.GetApplicationButtonElement: TdxSkinElement;
begin
  if UseSkins then
    Result := SkinInfo.RibbonApplicationButton
  else
    Result := nil;
end;

function TdxSkinRibbonPainter.GetApplicationButtonIndent(const AName: string): Integer;
begin
  if UseSkins then
    Result := ScaleFactor.Apply(SkinInfo.GetIntegerPropertyValue(ApplicationButtonElement, AName))
  else
    Result := 0
end;

function TdxSkinRibbonPainter.GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect;
begin
  if not UseSkins or (SkinInfo.RibbonApplicationBackground = nil) or
    (SkinInfo.RibbonApplicationHeaderBackground = nil) or
    (SkinInfo.RibbonApplicationFooterBackground = nil)
  then
    Exit(inherited GetApplicationMenuContentOffset(ATabsBounds));

  Result := cxRect(
    ScaleFactor.Apply(SkinInfo.RibbonApplicationBackground.ContentOffset.Left),
    GetElementMinSize(SkinInfo.RibbonApplicationHeaderBackground, ScaleFactor).cy,
    ScaleFactor.Apply(SkinInfo.RibbonApplicationBackground.ContentOffset.Right),
    Max(ScaleFactor.Apply(30), GetElementMinSize(SkinInfo.RibbonApplicationFooterBackground, ScaleFactor).cy));
end;

function TdxSkinRibbonPainter.GetName: string;
begin
  Result := SkinName;
end;

function TdxSkinRibbonPainter.GetBorderBounds(ASide: TcxBorder; const ABorders, ABounds: TRect): TRect;
begin
  Result := ABounds;
  case ASide of
    bLeft:
      begin
        Inc(Result.Top, ABorders.Top);
        Result.Right := Result.Left + ABorders.Left;
      end;
    bRight:
      begin
        Inc(Result.Top, ABorders.Top);
        Result.Left := Result.Right - ABorders.Right;
      end;
    bBottom:
      begin
        Result.Top := Result.Bottom - ABorders.Bottom;
        Dec(Result.Right, ABorders.Right);
        Inc(Result.Left, ABorders.Left);
      end;
    else
      Result.Bottom := ABorders.Top;
  end;
end;

function TdxSkinRibbonPainter.GetBorderIconElement(AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean): TdxSkinElement;
const
  RibbonIconsToSkinFormIcons: array[TdxRibbonBorderDrawIcon] of TdxSkinFormIcon =
    (sfiMinimize, sfiMaximize, sfiRestore, sfiClose, sfiHelp, sfiHelp, sfiHelp);
begin
  Result := nil;
  if UseSkins then
    case AIcon of
      rbdiAutoHideModeShowUI:
        Result := SkinInfo.RibbonFormButtonAutoHideModeShowUI;
      rbdiDisplayOptions:
        Result := SkinInfo.RibbonFormButtonDisplayOptions;
      rbdiMinimize:
        Result := SkinInfo.RibbonFormButtonMinimize;
      rbdiMaximize:
        Result := SkinInfo.RibbonFormButtonMaximize;
      rbdiRestore:
        Result := SkinInfo.RibbonFormButtonRestore;
      rbdiClose:
        Result := SkinInfo.RibbonFormButtonClose;
      rbdiHelp:
        Result := SkinInfo.RibbonFormButtonHelp;
    end;
end;

function TdxSkinRibbonPainter.GetBorderSkinElement(ASide: TcxBorder; AIsRectangular: Boolean): TdxSkinElement;
begin
  case ASide of
    bLeft:
      Result := SkinInfo.RibbonFormLeft[AIsRectangular];
    bTop:
      Result := SkinInfo.RibbonFormCaption;
    bRight:
      Result := SkinInfo.RibbonFormRight[AIsRectangular];
    bBottom:
      Result := SkinInfo.RibbonFormBottom[AIsRectangular];
    else
      Result := nil;
  end;
end;

function TdxSkinRibbonPainter.GetBordersWidth(AHasStatusBar: Boolean): TRect;

  function GetBorderSize(ASide: TcxBorder): TSize;
  var
    AElement: TdxSkinElement;
  begin
    AElement := GetBorderSkinElement(ASide, False);
    if AElement = nil then
      Result := cxNullSize
    else if AElement.MinSize.IsEmpty then
      Result := AElement.Size
    else
      Result := AElement.MinSize.Size
  end;

  function GetBottomBorderSize: Integer;
  begin
    if AHasStatusBar then
    begin
      Result := 1;
      if SkinInfo.RibbonStatusBarBackground <> nil then
        Result := Max(Result, SkinInfo.RibbonStatusBarBackground.ContentOffset.Bottom);
    end
    else
      Result := GetBorderSize(bBottom).cy;
  end;

begin
  Result.Top := 0;
  Result.Left := GetBorderSize(bLeft).cx;
  Result.Right := GetBorderSize(bRight).cx;
  Result.Bottom := GetBottomBorderSize;
end;

function TdxSkinRibbonPainter.GetColorPalette(AElement: TdxSkinElement; AState: Integer): IdxColorPalette;
begin
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(SkinElementCheckState(AElement, AState))
  else
    Result := nil;
end;

function TdxSkinRibbonPainter.GetCustomizeButtonOutsizeQAT(AHasAppButton: Boolean): Boolean;
var
  AProperty: TdxSkinBooleanProperty;
begin
  Result := False;
  if UseSkins then
  begin
    AProperty := SkinInfo.RibbonQATCustomizeButtonOutsizeQAT[AHasAppButton];
    if AProperty <> nil then
      Result := AProperty.Value;
  end;
end;

function TdxSkinRibbonPainter.GetElementMinSize(AElement: TdxSkinElement; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result.cx := Max(cxMarginsWidth(AElement.Image.Margins.Margin), Max(AElement.MinSize.Width, AElement.Size.cx));
  Result.cy := Max(cxMarginsHeight(AElement.Image.Margins.Margin), Max(AElement.MinSize.Height, AElement.Size.cy));
  Result := AScaleFactor.Apply(Result);
end;

function TdxSkinRibbonPainter.GetElementTextColor(AElement: TdxSkinElement; AState: Integer;
  const APropertyPrefix: string = ''; const ADefaultTextColor: TColor = clDefault): TColor;
var
  AProperty: TdxSkinProperty;
  APropertyStateName: string;
begin
  Result := clDefault;
  if AElement <> nil then
  begin
    APropertyStateName := APropertyPrefix + dxSkinElementTextColorPropertyNames[RibbonStateToButtonState(AState)];
    if AElement.GetPropertyByName(APropertyStateName + dxRibbonColorSchemeAccentNames[ColorSchemeAccent], AProperty) or
       AElement.GetPropertyByName(APropertyStateName, AProperty)
    then
      if AProperty is TdxSkinColor then
        Result := TdxSkinColor(AProperty).Value;

    if Result = clDefault then
      Result := cxGetActualColor(ADefaultTextColor, AElement.TextColor);
  end;
end;

function TdxSkinRibbonPainter.GetPopupMenuColor: TColor;
begin
  if SkinInfo.PopupMenu = nil then
    Result := clWhite
  else
    Result := SkinInfo.PopupMenu.Color;
end;

function TdxSkinRibbonPainter.GetSkinInfo: TdxSkinInfo;
begin
  if not Painter.GetPainterData(Result) then
    raise EdxException.Create('Internal Error');
end;

function TdxSkinRibbonPainter.GetQATBackgroundElement(ABellow, AHasApplicationButton: Boolean): TdxSkinElement;
begin
  if not UseSkins then
    Result := nil
  else
    if ABellow then
      Result := SkinInfo.RibbonQuickToolbarBelow
    else
      Result := SkinInfo.RibbonQuickToolbar[AHasApplicationButton];
end;

function TdxSkinRibbonPainter.GetQATLeftOffset(AHasApplicationButton: Boolean): Integer;
const
  QATOffsetDelta = 10;
begin
  if UseSkins then
  begin
    Result := ScaleFactor.Apply(QATOffsetDelta) + GetApplicationButtonIndent(sdxRibbonAppButtonRightIndent) +
      ScaleFactor.Apply(SkinInfo.GetIntegerPropertyValue(GetQATBackgroundElement(False, AHasApplicationButton), sdxRibbonQuickAccessToolbarOffset));
  end
  else
    Result := 0;
end;

function TdxSkinRibbonPainter.GetTabAeroSupport: Boolean;
begin
  Result := UseSkins and Assigned(SkinInfo.RibbonTabAeroSupport) and SkinInfo.RibbonTabAeroSupport.Value;
end;

function TdxSkinRibbonPainter.GetUseSkins: Boolean;
begin
  Result := cxUseSkins;
end;

procedure TdxSkinRibbonPainter.DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
const
  dxApplicationButtonStateToElementState: array[TdxRibbonApplicationButtonState] of TdxSkinElementState =
    (esNormal, esHot, esPressed);
begin
  if ApplicationButtonElement <> nil then
    DrawElement(DC, R, dxApplicationButtonStateToElementState[AState], ApplicationButtonElement, GetActualColorSchemeAccentImageIndex)
  else
    inherited DrawApplicationButton(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer);
const
  ButtonState: array [Boolean] of TdxSkinElementState = (esNormal, esHot);
begin
  if not UseSkins or (SkinInfo.ButtonElements = nil) then
    inherited DrawApplicationMenuButton(DC, R, AState)
  else
    SkinInfo.ButtonElements.Draw(DC, R, ScaleFactor, 0, ButtonState[AState = DXBAR_HOT]);
end;

procedure TdxSkinRibbonPainter.DrawApplicationMenuExtraPaneButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not UseSkins or (SkinInfo.RibbonExtraPaneButton = nil) then
    inherited DrawApplicationMenuExtraPaneButton(DC, R, AState)
  else
    SkinInfo.RibbonExtraPaneButton.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState));
end;

procedure TdxSkinRibbonPainter.DrawApplicationMenuExtraPanePinButtonGlyph(DC: HDC; const R: TRect; AState: Integer;
  AChecked: Boolean);
begin
  if not UseSkins or (SkinInfo.RibbonExtraPanePinButtonGlyph = nil) then
    inherited DrawApplicationMenuExtraPanePinButtonGlyph(DC, R, AState, AChecked)
  else
    if UseRightToLeftAlignment then
      SkinInfo.RibbonExtraPanePinButtonGlyph.DrawRTL(DC, R, ScaleFactor, Ord(AChecked))
    else
      SkinInfo.RibbonExtraPanePinButtonGlyph.Draw(DC, R, ScaleFactor, Ord(AChecked));
end;

procedure TdxSkinRibbonPainter.DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect);
var
  R1: TRect;
begin
  if not UseSkins or (SkinInfo.RibbonApplicationBackground = nil) or
     (SkinInfo.RibbonApplicationFooterBackground = nil) or
     (SkinInfo.RibbonApplicationHeaderBackground = nil)
  then
    inherited DrawApplicationMenuBackground(DC, R, AContentRect)
  else
  begin
    R1 := cxRect(R.Left, R.Top, R.Right, AContentRect.Top);
    DrawTabAreaBackground(DC, R1, True, False, ramsShownAsMenu);
    SkinInfo.RibbonApplicationHeaderBackground.Draw(DC, R1, ScaleFactor);

    R1 := cxRect(R.Left, AContentRect.Bottom, R.Right, R.Bottom);
    DrawTabAreaBackground(DC, R1, True, False, ramsShownAsMenu);
    SkinInfo.RibbonApplicationFooterBackground.Draw(DC, R1, ScaleFactor);

    R1 := cxRect(R.Left, AContentRect.Top, R.Right, AContentRect.Bottom);
    if SkinInfo.RibbonApplicationBackground.IsAlphaUsed then
      FillRectByColor(DC, R1, PopupMenuColor);
    SkinInfo.RibbonApplicationBackground.Draw(DC, R1, ScaleFactor);

    R1.Left := R.Left + SkinInfo.RibbonApplicationBackground.ContentOffset.Left;
    R1.Right := AContentRect.Left;
    FillRectByColor(DC, R1, PopupMenuColor);

    R1.Right := R.Right - SkinInfo.RibbonApplicationBackground.ContentOffset.Right;
    R1.Left := AContentRect.Right;
    FillRectByColor(DC, R1, GetPartColor(DXBAR_MENUEXTRAPANE));
  end;
end;

function TdxSkinRibbonPainter.CanShowTabAreaToolbarInBackstageView: Boolean;
begin
  Result := False;
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.RibbonBackstageViewBackButton <> nil) then
  begin
    if UseRightToLeftAlignment then
      SkinInfo.RibbonBackstageViewBackButton.DrawRTL(DC,
        cxRectCenter(R, ScaleFactor.Apply(SkinInfo.RibbonBackstageViewBackButton.MinSize.Size)),
        ScaleFactor, 0, RibbonStateToSkinElementState(AState))
    else
      SkinInfo.RibbonBackstageViewBackButton.Draw(DC,
        cxRectCenter(R, ScaleFactor.Apply(SkinInfo.RibbonBackstageViewBackButton.MinSize.Size)),
        ScaleFactor, 0, RibbonStateToSkinElementState(AState));
  end
  else
    inherited DrawBackstageViewBackButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewBackground(DC: HDC; const R: TRect);
var
  ALogoRect: TRect;
  AOffsets: TRect;
begin
  if UseSkins and (SkinInfo.RibbonBackstageView <> nil) then
  begin
    SkinInfo.RibbonBackstageView.UseCache := True;

    FillRectByColor(DC, R, SkinInfo.RibbonBackstageView.Color);
    if SkinInfo.RibbonBackstageViewImage <> nil then
    begin
      ALogoRect := R;
      ALogoRect.Left := ALogoRect.Right - ScaleFactor.Apply(SkinInfo.RibbonBackstageViewImage.Size.cx);
      ALogoRect.Top := ALogoRect.Bottom - ScaleFactor.Apply(SkinInfo.RibbonBackstageViewImage.Size.cy);
      SkinInfo.RibbonBackstageViewImage.Draw(DC, ALogoRect, ScaleFactor);
    end;

    AOffsets := GetPartContentOffsets(DXBAR_BACKSTAGEVIEW);
    if not cxRectIsEqual(AOffsets, cxNullRect) then
    begin
      cxPaintCanvas.BeginPaint(DC);
      try
        cxPaintCanvas.ExcludeClipRect(cxRectContent(R, AOffsets));
        SkinInfo.RibbonBackstageView.Draw(DC, R, ScaleFactor, GetActualColorSchemeAccentImageIndex);
      finally
        cxPaintCanvas.EndPaint;
      end;
    end;
  end
  else
    inherited DrawBackstageViewBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewMenuBackground(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonBackstageViewMenu) then
    inherited DrawBackstageViewMenuBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonBackstageViewMenuButton, GetActualColorSchemeAccentImageIndex) then
    inherited DrawBackstageViewMenuButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewMenuHeader(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonBackstageViewMenuHeader) then
    inherited DrawBackstageViewMenuHeader(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonBackstageViewMenuSeparator) then
    inherited DrawBackstageViewMenuSeparator(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
var
  AArrowRect: TRect;
  AArrowSize: TSize;
begin
  if DrawElement(DC, R, AState, SkinInfo.RibbonBackstageViewTab, GetActualColorSchemeAccentImageIndex) then
  begin
    case AState of
      DXBAR_CHECKED, DXBAR_HOTCHECK, DXBAR_PRESSED:
        if SkinInfo.RibbonBackstageViewTabArrow <> nil then
        begin
          AArrowSize := SkinInfo.RibbonBackstageViewTabArrow.Size;
          AArrowRect := cxRectCenterVertically(R, AArrowSize.cy);
          if UseRightToLeftAlignment then
            AArrowRect := cxRectSetLeft(AArrowRect, AArrowRect.Left, AArrowSize.cx)
          else
            AArrowRect := cxRectSetRight(AArrowRect, AArrowRect.Right, AArrowSize.cx);
          if UseRightToLeftAlignment then
            SkinInfo.RibbonBackstageViewTabArrow.DrawRTL(DC, AArrowRect, dxDefaultScaleFactor)
          else
            SkinInfo.RibbonBackstageViewTabArrow.Draw(DC, AArrowRect, dxDefaultScaleFactor);
        end;
    end;
  end
  else
    inherited DrawBackstageViewTabButton(DC, R, AState)
end;

function TdxSkinRibbonPainter.GetBackstageViewMenuButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  if UseSkins then
    Result := GetColorPalette(SkinInfo.RibbonBackstageViewMenuButton, AState)
  else
    Result := inherited;
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect);
begin
  if UseSkins and (SkinInfo.RibbonBackstageView <> nil) then
    FillRectByColor(DC, R, SkinInfo.RibbonBackstageView.Color)
  else
    inherited DrawBackstageViewGalleryBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.GalleryItem <> nil) then
    SkinInfo.GalleryItem.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState))
  else
    inherited DrawBackstageViewGalleryItem(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect);
begin
  if UseSkins then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      Painter.DrawLabelLine(cxPaintCanvas, R, clDefault, clDefault, False);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end
  else
    inherited DrawBackstageViewGallerySeparator(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawButtonGroupElement(DC, R, AState, SkinInfo.RibbonButtonGroupButton) then
    inherited DrawButtonGroup(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawButtonGroupElement(DC, R, AState, SkinInfo.RibbonButtonGroupSplitButtonRight) then
    inherited DrawButtonGroupDropButtonArrowPart(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawButtonGroupElement(DC, R, AState, SkinInfo.RibbonButtonGroupSplitButtonLeft) then
    inherited DrawButtonGroupDropButtonMainPart(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupBackground(DC: HDC; const R: TRect);
begin
  if SkinInfo.RibbonButtonGroup <> nil then
  begin
    with SkinInfo.RibbonButtonGroup.Image.Margins do
      DrawClippedElement(DC, R, cxRectInflate(R, Left, 0, Right, 0), SkinInfo.RibbonButtonGroup, esNormal);
  end;
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins then
    DrawButtonGroupBackground(DC, R)
  else
    inherited DrawButtonGroupSplitButtonSeparator(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupBorderLeft(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonButtonGroup <> nil) then
  begin
    ARect := R;
    ARect.Right := Max(R.Left + SkinInfo.RibbonButtonGroup.Size.cx, R.Right);
    DrawClippedElement(DC, R, ARect, SkinInfo.RibbonButtonGroup, esNormal);
  end
  else
    inherited DrawButtonGroupBorderLeft(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupBorderRight(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonButtonGroup <> nil) then
  begin
    if not IsRectEmpty(R) then
    begin
      ARect := R;
      ARect.Left := Min(R.Left, R.Right - SkinInfo.RibbonButtonGroup.Size.cx);
      DrawClippedElement(DC, R, ARect, SkinInfo.RibbonButtonGroup, esNormal, roSet);
    end;
  end
  else
    inherited DrawButtonGroupBorderRight(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.RibbonButtonGroupSeparator <> nil) then
    SkinInfo.RibbonButtonGroupSeparator.Draw(DC, R, dxDefaultScaleFactor, 0, RibbonStateToSkinElementState(AState))
  else
    inherited DrawButtonGroupBorderMiddle(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.RibbonCollapsedToolBarBackground <> nil) then
    SkinInfo.RibbonCollapsedToolBarBackground.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState))
  else
    inherited DrawCollapsedToolbarBackground(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonCollapsedToolBarGlyphBackground) then
    inherited DrawCollapsedToolbarGlyphBackground(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonContextualTabSeparator) then
    inherited DrawContextTabSeparator(DC, R, ABeginGroup);
end;

procedure TdxSkinRibbonPainter.DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonContextualTabHeader <> nil) then
  begin
    if SkinInfo.RibbonContextualTabHeader.ImageCount > 1 then
      DrawColoredElement(DC, R, AContextColor, SkinInfo.RibbonContextualTabHeader, RibbonTabStatesMap[AState])
    else
    begin
      ARect := CorrectTabHeaderRect(R);
      dxGpFillRect(DC, ARect, AContextColor);
      SkinInfo.RibbonContextualTabHeader.Draw(DC, ARect, ScaleFactor, 0, RibbonTabStatesMap[AState]);
    end;
  end
  else
    inherited DrawContextTabBackground(DC, R, AState, AContextColor);
end;

procedure TdxSkinRibbonPainter.DrawContextTabGroupsArea(
  DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if UseSkins and (SkinInfo.RibbonContextualTabPanel <> nil) then
  begin
    FContextTabGroupsAreaCache.Check(SkinInfo.RibbonContextualTabPanel, AContextColor, R, ScaleFactor);
    cxBitBlt(DC, FContextTabGroupsAreaCache.Canvas.Handle, R, cxNullPoint, SRCCOPY);
  end
  else
    inherited DrawContextTabGroupsArea(DC, R, AContextColor, AIsQATAtBottom, AIsInPopup)
end;

procedure TdxSkinRibbonPainter.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  ARect: TRect;
  ASaveIndex: Integer;
begin
  if UseSkins and (SkinInfo.RibbonContextualTabLabel <> nil) then
  begin
    ASaveIndex := SaveDC(DC);
    try
      ARect := R;
      Inc(ARect.Bottom);
      IntersectClipRect(DC, ARect.Left, ARect.Top + 1, ARect.Right, ARect.Bottom);
      if SkinInfo.RibbonContextualTabLabel.ImageCount > 1 then
        DrawColoredElement(DC, ARect, AContextColor, SkinInfo.RibbonContextualTabLabel, esNormal)
      else
      begin
        FillRectByColor(DC, ARect, AContextColor);
        SkinInfo.RibbonContextualTabLabel.Draw(DC, ARect, ScaleFactor);
      end;
    finally
      RestoreDC(DC, ASaveIndex)
    end;
  end
  else
    inherited DrawContextBackground(DC, R, AContextColor);
end;

procedure TdxSkinRibbonPainter.DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor);
begin
  if UseSkins and (SkinInfo.RibbonContextualTabLabelOnGlass <> nil) then
  begin
    if SkinInfo.RibbonContextualTabLabelOnGlass.ImageCount > 1 then
      DrawColoredElement(DC, R, AContextColor, SkinInfo.RibbonContextualTabLabelOnGlass, esNormal)
    else
    begin
      dxGpFillRectByGradient(DC, R, 0, AContextColor, LinearGradientModeVertical, 0, 220);
      SkinInfo.RibbonContextualTabLabelOnGlass.Draw(DC, R, ScaleFactor);
    end;
  end
  else
    inherited DrawContextBackgroundGlass(DC, R, AContextColor);
end;

procedure TdxSkinRibbonPainter.DrawEditArrowButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawEditButtonGlyph(DC, R, AState, cxbkComboBtn) then
    inherited DrawEditArrowButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawEditButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins then
  begin
    cxPaintCanvas.BeginPaint(DC);
    Painter.DrawScaledEditorButton(cxPaintCanvas, R, cxbkEditorBtn, RibbonStateToButtonState(AState), ScaleFactor);
    cxPaintCanvas.EndPaint;
  end
  else
    inherited DrawEditButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawEditEllipsisButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawEditButtonGlyph(DC, R, AState, cxbkEllipsisBtn) then
    inherited DrawEditEllipsisButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawEditSpinDownButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawEditButtonGlyph(DC, R, AState, cxbkSpinDownBtn) then
    inherited DrawEditSpinDownButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawEditSpinUpButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DoDrawEditButtonGlyph(DC, R, AState, cxbkSpinUpBtn) then
    inherited DrawEditSpinUpButton(DC, R, AState);
end;

function TdxSkinRibbonPainter.DrawElement(DC: HDC; const R: TRect; AElement: TdxSkinElement): Boolean;
begin
  Result := UseSkins and (AElement <> nil);
  if Result then
    if UseRightToLeftAlignment then
      AElement.DrawRTL(DC, R, ScaleFactor)
    else
      AElement.Draw(DC, R, ScaleFactor);
end;

function TdxSkinRibbonPainter.DrawElement(DC: HDC; const R: TRect; AState: TdxSkinElementState; AElement: TdxSkinElement;
  AImageIndex: Integer = 0): Boolean;
begin
  Result := UseSkins and (AElement <> nil);
  if Result then
    if UseRightToLeftAlignment then
      AElement.DrawRTL(DC, R, ScaleFactor, AImageIndex, AState)
    else
      AElement.Draw(DC, R, ScaleFactor, AImageIndex, AState);
end;

function TdxSkinRibbonPainter.DrawElement(DC: HDC; const R: TRect;
  AState: Integer; AElement: TdxSkinElement; AImageIndex: Integer = 0): Boolean;
begin
  Result := UseSkins and (AElement <> nil);
  if Result then
    if UseRightToLeftAlignment then
      AElement.DrawRTL(DC, R, ScaleFactor, AImageIndex, SkinElementCheckState(AElement, AState))
    else
      AElement.Draw(DC, R, ScaleFactor, AImageIndex, SkinElementCheckState(AElement, AState));
end;

procedure TdxSkinRibbonPainter.DrawProgressDiscreteBand(DC: HDC; const R: TRect);
var
  AElement: TdxSkinElement;
  ARect: TRect;

  function CheckRect(const R: TRect): TRect;
  begin
    Result := R;
    if SkinInfo.ProgressBarElements[False, False] <> nil then
    begin
      InflateRect(Result, 0, 2);
      with SkinInfo.ProgressBarElements[False, False].ContentOffset.Rect do
      begin
        Inc(Result.Top, Top);
        Dec(Result.Bottom, Bottom);
      end;
    end;
  end;

begin
  if UseSkins then
    AElement := SkinInfo.ProgressBarElements[True, False]
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawProgressDiscreteBand(DC, R)
  else
  begin
    ARect := CheckRect(R);
    with AElement.Image.Margins.Margin do
    begin
      Dec(ARect.Left, Left);
      Inc(ARect.Right, Right);
    end;
    DrawClippedElement(DC, R, ARect, AElement, esNormal);
  end;
end;

procedure TdxSkinRibbonPainter.DrawProgressSolidBand(DC: HDC; const R: TRect);
var
  AElement: TdxSkinElement;
  ARect: TRect;
begin
  if UseSkins then
    AElement := SkinInfo.ProgressBarElements[True, False]
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawProgressDiscreteBand(DC, R)
  else
  begin
    ARect := R;
    if SkinInfo.ProgressBarElements[False, False] <> nil then
    begin
      InflateRect(ARect, 2, 2);
      ARect := cxRectContent(ARect, SkinInfo.ProgressBarElements[False, False].ContentOffset.Rect);
    end;
    DrawClippedElement(DC, R, ARect, AElement, esNormal);
  end;
end;

procedure TdxSkinRibbonPainter.DrawProgressSubstrate(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.ProgressBarElements[False, False]) then
    inherited DrawProgressSubstrate(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownBorder(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.PopupMenu) then
    inherited DrawDropDownBorder(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryBackground(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGalleryBackground) then
    inherited DrawDropDownGalleryBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryBottomSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonGallerySizeGrips <> nil) then
  begin
    ARect := cxRectInflate(R, 0, -3, -2, -1);
    ARect.Left := ARect.Right - cxRectHeight(ARect);
    if UseRightToLeftAlignment then
    begin
      ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, R);
      SkinInfo.RibbonGallerySizeGrips.DrawRTL(DC, ARect, ScaleFactor, 1);
    end
    else
      SkinInfo.RibbonGallerySizeGrips.Draw(DC, ARect, ScaleFactor, 1);
  end
  else
    inherited DrawDropDownGalleryBottomSizeGrip(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGallerySizingPanel) then
    inherited DrawDropDownGalleryBottomSizingBand(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryBottomVerticalSizeGrip(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGallerySizeGrips) then
    inherited DrawDropDownGalleryBottomVerticalSizeGrip(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGallerySizingPanel) then
    inherited DrawDropDownGalleryTopSizingBand(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryTopSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonGallerySizeGrips <> nil) then
  begin
    ARect := cxRectInflate(R, 0, -3, -2, -1);
    ARect.Left := ARect.Right - cxRectHeight(ARect);
    if UseRightToLeftAlignment then
    begin
      ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, R);
      SkinInfo.RibbonGallerySizeGrips.DrawRTL(DC, ARect, ScaleFactor, 2);
    end
    else
      SkinInfo.RibbonGallerySizeGrips.Draw(DC, ARect, ScaleFactor, 2);
  end
  else
    inherited DrawDropDownGalleryTopSizeGrip(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawDropDownGalleryTopVerticalSizeGrip(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGallerySizeGrips) then
    inherited DrawDropDownGalleryTopVerticalSizeGrip(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawGalleryFilterBandBackground(DC: HDC; const R: TRect);
begin
  DrawGalleryGroupHeaderBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonGalleryGroupCaption) then
    inherited DrawGalleryGroupHeaderBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.GalleryItem <> nil) then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.ExcludeClipRect(cxRectInflate(ARect, -2));
      DrawElement(DC, ARect, AState, SkinInfo.GalleryItem);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end
  else
    inherited DrawGalleryGroupItemSelectionFrame(DC, ARect, AState);
end;

procedure TdxSkinRibbonPainter.DrawInRibbonGalleryBackground(DC: HDC; const R: TRect; AState: Integer);
var
  R1: TRect;
begin
  if UseSkins and (SkinInfo.RibbonGalleryPane <> nil) then
  begin
    R1 := R;
    Inc(R1.Right, SkinInfo.RibbonGalleryPane.Image.Margins.Right);
    DrawClippedElement(DC, R, R1, SkinInfo.RibbonGalleryPane, RibbonStateToSkinElementState(AState));
  end
  else
    inherited DrawInRibbonGalleryBackground(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawInRibbonGalleryScrollBarButton(
  DC: HDC; const R: TRect; AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);
var
  AElement: TdxSkinElement;
begin
  AElement := nil;
  if UseSkins then
  begin
    case AButtonKind of
      gsbkLineUp:
        AElement := SkinInfo.RibbonGalleryButtonUp;
      gsbkLineDown:
        AElement := SkinInfo.RibbonGalleryButtonDown;
      gsbkDropDown:
        AElement := SkinInfo.RibbonGalleryButtonDropDown;
    end;
  end;

  if AElement <> nil then
    AElement.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState))
  else
    inherited DrawInRibbonGalleryScrollBarButton(DC, R, AButtonKind, AState);
end;

procedure TdxSkinRibbonPainter.DrawInRibbonGalleryScrollBarDropDownTouchButton(DC: HDC; const R: TRect; AState: Integer);
const
  TopBorderWidth = 2;
var
  ASaveIndex: Integer;
begin
  if UseSkins then
  begin
    ASaveIndex := SaveDC(DC);
    try
      IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Top + TopBorderWidth);
      DrawInRibbonGalleryScrollBarButton(DC, R, gsbkLineUp, AState);
    finally
      RestoreDC(DC, ASaveIndex)
    end;

    ASaveIndex := SaveDC(DC);
    try
      IntersectClipRect(DC, R.Left, R.Top + TopBorderWidth, R.Right, R.Bottom);
      DrawInRibbonGalleryScrollBarButton(DC, R, gsbkDropDown, AState);
    finally
      RestoreDC(DC, ASaveIndex)
    end;
  end
  else
    inherited DrawInRibbonGalleryScrollBarDropDownTouchButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawInRibbonGalleryScrollBarBackground(DC: HDC; const R: TRect; AState: Integer);
var
  R1: TRect;
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonGalleryPane
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawInRibbonGalleryScrollBarBackground(DC, R, AState)
  else
  begin
    R1 := R;
    Dec(R1.Left, Max(AElement.Size.cx, AElement.Image.Margins.Left));
    DrawClippedElement(DC, R, R1, AElement, RibbonStateToSkinElementState(AState));
  end;
end;

procedure TdxSkinRibbonPainter.DrawArrowDown(DC: HDC; const R: TRect; AState: Integer);
const
  StateMap: array[0..8] of TdxSkinElementState = (esNormal, esDisabled,
    esHot, esActive, esNormal, esNormal, esNormal, esNormal, esNormal
  );
begin
  if UseSkins and (SkinInfo.RibbonButtonArrow <> nil) then
    SkinInfo.RibbonButtonArrow.Draw(DC, R, ScaleFactor, 0, StateMap[AState])
  else
    inherited DrawArrowDown(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawClippedElement(DC: HDC; const R: TRect;
  const ASource: TRect; AElement: TdxSkinElement; AState: TdxSkinElementState = esNormal;
  AOperation: TcxRegionOperation = roIntersect; AImageIndex: Integer = 0);
begin
  if not cxRectIsEmpty(R) then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.SetClipRegion(TcxRegion.Create(R), AOperation);
      AElement.Draw(cxPaintCanvas.Handle, ASource, ScaleFactor, AImageIndex, AState);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxSkinRibbonPainter.DrawColoredElement(DC: HDC; const R: TRect;
  AColor: TColor; AElement: TdxSkinElement; AState: TdxSkinElementState);
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(R, True);
  try
    AElement.Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, ScaleFactor, 0, AState);
    cxMakeColoredBitmap(ABitmap, AColor);
    AElement.Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, ScaleFactor, 1, AState);
    cxAlphaBlend(DC, ABitmap, R, ABitmap.ClientRect);
  finally
    ABitmap.Free;
  end;
end;

function TdxSkinRibbonPainter.DoDrawButtonGroupElement(
  DC: HDC; const R: TRect; AState: Integer; AElement: TdxSkinElement): Boolean;
begin
  Result := UseSkins and (AElement <> nil);
  if Result then
  begin
    DrawButtonGroupBackground(DC, R);
    DrawElement(DC, R, AState, AElement);
  end;
end;

function TdxSkinRibbonPainter.DoDrawEditButtonGlyph(DC: HDC;
  const R: TRect; AState: Integer; AButtonKind: TcxEditBtnKind): Boolean;
const
  ButtonState: array [DXBAR_NORMAL..DXBAR_ACTIVEDISABLED] of TcxButtonState = (
    cxbsNormal, cxbsDisabled, cxbsHot, cxbsNormal, cxbsPressed, cxbsPressed,
    cxbsDefault, cxbsDefault, cxbsDisabled);
begin
  Result := UseSkins;
  if Result then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      Painter.DrawScaledEditorButtonGlyph(cxPaintCanvas, R, AButtonKind, ButtonState[AState], ScaleFactor);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxSkinRibbonPainter.DoDrawStatusBarBackground(DC: HDC; const R, AVisibleArea: TRect; AIsRaised: Boolean): Boolean;
var
  AElement: TdxSkinElement;
  AIndents: TRect;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonStatusBarBackground
  else
    AElement := nil;

  Result := AElement <> nil;
  if Result then
  begin
    AIndents := GetBordersWidth(True);
    AIndents.Top := 0;
    if AIsRaised and AElement.IsAlphaUsed then
      DrawClippedElement(DC, AVisibleArea, cxRectInflate(R, AIndents), AElement, esNormal, roIntersect, 0);
    DrawClippedElement(DC, AVisibleArea, cxRectInflate(R, AIndents), AElement, esNormal, roIntersect, Integer(AIsRaised));
  end;
end;

function TdxSkinRibbonPainter.DoDrawStatusBarPart(
  DC: HDC; const R: TRect; AIsRaised, AActive, AIsLeft: Boolean): Boolean;

  function CalculateDestRect(AElement: TdxSkinElement): TRect;
  var
    AMinSize: TSize;
  begin
    AMinSize := GetElementMinSize(AElement, dxDefaultScaleFactor);
    Result := cxRectSetBottom(R, R.Bottom, Max(AMinSize.cy, cxRectHeight(R)));
    if AIsLeft xor UseRightToLeftAlignment then
      Result.Right := Max(Result.Left + AMinSize.cx, Result.Right + AElement.Image.Margins.Right)
    else
      Result.Left := Min(Result.Right - AMinSize.cx, Result.Left - AElement.Image.Margins.Left);
  end;

const
  StateMap: array[Boolean] of TdxSkinElementState = (esActiveDisabled, esActive);
begin
  Result := UseSkins and (SkinInfo.RibbonStatusBarBackground <> nil);
  if Result then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.IntersectClipRect(R);
      if UseRightToLeftAlignment then
        SkinInfo.RibbonStatusBarBackground.DrawRTL(cxPaintCanvas.Handle,
          CalculateDestRect(SkinInfo.RibbonStatusBarBackground),
          ScaleFactor, Integer(AIsRaised and not AIsLeft), StateMap[AActive])
      else
        SkinInfo.RibbonStatusBarBackground.Draw(cxPaintCanvas.Handle,
          CalculateDestRect(SkinInfo.RibbonStatusBarBackground),
          ScaleFactor, Integer(AIsRaised and not AIsLeft), StateMap[AActive]);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxSkinRibbonPainter.DoDrawStatusBarRectangularPart(
  DC: HDC; const R: TRect; AIsRaised, AActive, AIsLeft: Boolean): Boolean;
const
  SideMap: array[Boolean] of TcxBorder = (bRight, bLeft);
var
  AElement: TdxSkinElement;
begin
  Result := UseSkins;
  if Result then
  begin
    AElement := GetBorderSkinElement(SideMap[AIsLeft], True);
    if AElement <> nil then
    begin
      cxPaintCanvas.BeginPaint(DC);
      try
        cxPaintCanvas.IntersectClipRect(R);
        DrawFormBorder(DC, SideMap[AIsLeft], cxRect(cxRectWidth(R), 0, cxRectWidth(R), 0),
          cxRect(R.Left, R.Top - AElement.Image.Margins.Top, R.Right, R.Bottom), AElement, AActive);
      finally
        cxPaintCanvas.EndPaint;
      end;
    end;
  end;
end;

procedure TdxSkinRibbonPainter.DrawFormBorders(DC: HDC; const ABordersWidth: TRect);

  procedure DrawBottomCorner(ASide: TcxBorder; R: TRect);
  var
    ABitmap: TcxBitmap;
    AElement: TdxSkinElement;
  begin
    AElement := GetBorderSkinElement(ASide, IsRectangularFormBottom(FormPaintData));
    if Assigned(AElement) then
    begin
      ABitmap := TcxBitmap.CreateSize(R);
      try
        ABitmap.cxCanvas.WindowOrg := R.TopLeft;
        Dec(R.Top, AElement.Size.cy);
        AElement.Draw(ABitmap.Canvas.Handle, R, ScaleFactor, Integer(not FormPaintData.GetIsActive));
        cxBitBlt(DC, ABitmap.Canvas.Handle, R, cxNullPoint, SRCCOPY);
      finally
        ABitmap.Free;
      end;
    end;
  end;

  procedure DrawBorder(ASide: TcxBorder; const ABordersWidth: TRect);
  begin
    if (ASide = bBottom) and PaintData.HasStatusBar then
      DrawStatusBarFormBorder(DC, ABordersWidth)
    else
      DrawFormBorder(DC, ASide, ABordersWidth, FormPaintData.GetBounds,
        GetBorderSkinElement(ASide, IsRectangularFormBottom(FormPaintData)), FormPaintData.GetIsActive);
  end;

  procedure DrawBorders(const ABordersWidth: TRect);
  var
    ASide: TcxBorder;
    R: TRect;
  begin
    for ASide := Low(TcxBorder) to High(TcxBorder) do
      DrawBorder(ASide, ABordersWidth);
    if ABordersWidth.Bottom > 1 then
    begin
      R := FormPaintData.GetBounds;
      R.Top := R.Bottom - ABordersWidth.Bottom;
      DrawBottomCorner(bLeft, cxRectSetWidth(R, 4));
      DrawBottomCorner(bRight, cxRectSetLeft(R, R.Right - 4, 4));
    end;
  end;

var
  ABorders: TRect;
begin
  if UseSkins then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.ExcludeClipRect(cxRectContent(FormPaintData.GetBounds, ABordersWidth));
      ABorders := ABordersWidth;
      if TabAeroSupport then
        Inc(ABorders.Top, PaintData.GetTabsHeight);
      Inc(ABorders.Top, PaintData.GetCaptionHeight);
      DrawBorders(ABorders);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end
  else
    inherited DrawFormBorders(DC, ABordersWidth);
end;

procedure TdxSkinRibbonPainter.DrawFormBorder(DC: HDC; ASide: TcxBorder;
  const ABorderWidths, R: TRect; AElement: TdxSkinElement; AActive: Boolean);

  procedure CorrectBorderSourceRect(var R: TRect; const ASize: TSize);
  begin
    case ASide of
      bTop:
        R.Bottom := Max(R.Bottom, R.Top + ASize.cy);
      bLeft:
        R.Right := Max(R.Right, R.Left + ASize.cx);
      bRight:
        R.Left := Min(R.Left, R.Right - ASize.cx);
      bBottom:
        R.Top := Min(R.Top, R.Bottom - ASize.cy);
    end;
  end;

var
  ABorderRect: TRect;
  ASaveIndex: Integer;
begin
  if Assigned(AElement) and not IsRectEmpty(R) then
  begin
    ASaveIndex := SaveDC(DC);
    try
      ABorderRect := GetBorderBounds(ASide, ABorderWidths, R);
      CorrectBorderSourceRect(ABorderRect, AElement.Size);
      AElement.UseCache := True;
      if UseRightToLeftAlignment then
        ABorderRect := TdxRightToLeftLayoutConverter.ConvertRect(ABorderRect, R);
      IntersectClipRect(DC, ABorderRect.Left, ABorderRect.Top, ABorderRect.Right, ABorderRect.Bottom);
      DrawElement(DC, ABorderRect, RibbonFormBorderStates[AActive], AElement, Integer(not AActive));
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end;
end;

procedure TdxSkinRibbonPainter.DrawStatusBarFormBorder(DC: HDC; const ABorders: TRect);
var
  AOffset: Integer;
  R1, R2: TRect;
begin
  if UseSkins then
  begin
    AOffset := 0;
    if SkinInfo.RibbonStatusBarBackground <> nil then
    begin
      AOffset := Ord(SkinInfo.RibbonStatusBarBackground.Borders.Bottom.ContentMargin = 0);
      if ABorders.Bottom > AOffset then
      begin
        R1 := cxRectSetTop(FormPaintData.GetBounds,
          FormPaintData.GetBounds.Bottom - ABorders.Bottom - AOffset, ABorders.Bottom);
        R2 := R1;
        Dec(R2.Top, SkinInfo.RibbonStatusBarBackground.Size.cy - ABorders.Bottom);
        DrawClippedElement(DC, R1, R2, SkinInfo.RibbonStatusBarBackground, esNormal);
      end;
    end;
    DrawFormBorder(DC, bBottom, Rect(ABorders.Left, 0, ABorders.Right, AOffset), FormPaintData.GetBounds,
      GetBorderSkinElement(bBottom, IsRectangularFormBottom(FormPaintData)), FormPaintData.GetIsActive);
  end;
end;

procedure TdxSkinRibbonPainter.DrawStatusBarGripBackground(DC: HDC; const R: TRect);
var
  AElement: TdxSkinElement;
  ALeftIndent, ARightIndent: Integer;
  R1: TRect;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonStatusBarBackground
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawStatusBarGripBackground(DC, R)
  else
    if not AElement.IsAlphaUsed then
    begin
      GetElementContentIndents(AElement, False, ALeftIndent, ARightIndent);
      R1 := GetBordersWidth(True);
      R1.Top := 0;
      R1.Left := ALeftIndent;
      DrawClippedElement(DC, R, cxRectInflate(R, R1), AElement, esNormal, roIntersect, 1);
    end;
end;

procedure TdxSkinRibbonPainter.DrawFormBorderIcon(DC: HDC;
  const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
const
  RibbonIconStateToSkinElementState: array[TdxRibbonBorderIconState] of TdxSkinElementState =
    (esNormal, esHot, esPressed, esActive, esHot);
var
  AElement: TdxSkinElement;
begin
  AElement := GetBorderIconElement(AIcon, False);
  if AElement <> nil then
    AElement.Draw(DC, cxRectInflate(R, -ScaleFactor.Apply(1)), ScaleFactor,
      Ord((PaintData.GetApplicationMenuState = ramsShownAsFullScreenFrame) and UseSkins),
      RibbonIconStateToSkinElementState[AState])
  else
    inherited DrawFormBorderIcon(DC, R, AIcon, AState);
end;

procedure TdxSkinRibbonPainter.DrawFormCaption(DC: HDC; const R: TRect);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esActiveDisabled, esActive);
var
  AElement: TdxSkinElement;
begin
  AElement := nil;
  if UseSkins then
  begin
    if PaintData.IsRibbonHidden then
      AElement := SkinInfo.RibbonFormCaptionRibbonHidden;
    if AElement = nil then
      AElement := SkinInfo.RibbonFormCaption;
  end;

  if AElement <> nil then
  begin
    AElement.UseCache := True;
    AElement.Draw(DC, R, ScaleFactor, Byte(not FormPaintData.GetIsActive), StateMap[FormPaintData.GetIsActive]);
  end
  else
    inherited DrawFormCaption(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawFormStatusBarPart(DC: HDC; const R: TRect;
  AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean);
var
  AHandled: Boolean;
begin
  if AIsRectangular and UseRoundedWindowCorners then
    AHandled := DoDrawStatusBarRectangularPart(DC, R, AIsRaised, AIsActive, AIsLeft)
  else
    AHandled := DoDrawStatusBarPart(DC, R, AIsRaised, AIsActive, AIsLeft);

  if not AHandled then
    inherited DrawFormStatusBarPart(DC, R, AIsLeft, AIsActive, AIsRaised, AIsRectangular);
end;

procedure TdxSkinRibbonPainter.DrawLargeButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonLargeButton) then
    inherited DrawLargeButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonLargeSplitButtonBottom) then
    inherited DrawLargeButton(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonLargeSplitButtonTop) then
    inherited DrawLargeButton(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer);
const
  StateMap: array[0..8] of TdxSkinElementState = (esNormal, esNormal, esHot,
    esHot, esPressed, esNormal, esNormal, esNormal, esNormal);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonTabPanelGroupButton <> nil) then
  begin
    ARect := R;
    OffsetRect(ARect, 0, 1);
    InflateRect(ARect, -1, -1);
    if UseRightToLeftAlignment then
      SkinInfo.RibbonTabPanelGroupButton.DrawRTL(DC, ARect, ScaleFactor, 0, StateMap[AState])
    else
      SkinInfo.RibbonTabPanelGroupButton.Draw(DC, ARect, ScaleFactor, 0, StateMap[AState]);
  end
  else
    inherited DrawLaunchButtonBackground(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer);
begin
  if not UseSkins then
    inherited DrawLaunchButtonDefaultGlyph(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawHelpButton(DC: HDC; const R: TRect; AState: TcxButtonState);
const
  StateMap: array [TcxButtonState] of Integer = (
    DXBAR_NORMAL, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_DISABLED
  );
begin
  if UseSkins then
    DrawSmallButton(DC, R, StateMap[AState])
  else
    inherited DrawHelpButton(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonGroupScroll[ALeft]
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawGroupScrollButton(DC, R, ALeft, AState)
  else
    AElement.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState));
end;

procedure TdxSkinRibbonPainter.DrawGroupScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean);
begin
  // do nothing
end;

procedure TdxSkinRibbonPainter.DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if UseSkins and (SkinInfo.RibbonTabGroupItemsSeparator <> nil) then
  begin
    SkinInfo.RibbonTabGroupItemsSeparator.Draw(DC,
      cxRectCenterHorizontally(R, SkinInfo.RibbonTabGroupItemsSeparator.MinSize.Width),
      dxDefaultScaleFactor);
  end
  else
    inherited DrawItemSeparator(DC, R, AHorizontal);
end;

procedure TdxSkinRibbonPainter.DrawKeyTip(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonKeyTip) then
    inherited DrawKeyTip(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
const
  RibbonIconsToSkinFormIcons: array[TdxBarMDIButton] of TdxSkinFormIcon =
    (sfiMinimize, sfiRestore, sfiClose);
  ButtonStateToSkinElementState: array[TcxButtonState] of TdxSkinElementState =
    (esActive, esNormal, esHot, esPressed, esDisabled);
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.FormIcons[True, RibbonIconsToSkinFormIcons[AButton]]
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawMDIButton(DC, R, AButton, AState)
  else
    AElement.Draw(DC, cxRectInflate(R, -1), ScaleFactor, 0, ButtonStateToSkinElementState[AState]);
end;

procedure TdxSkinRibbonPainter.DrawMDIButtonGlyph(
  DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
begin
  if not UseSkins then
    inherited DrawMDIButtonGlyph(DC, R, AButton, AState);
end;

procedure TdxSkinRibbonPainter.DrawMinimizeRibbonButtonGlyph(
  DC: HDC; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
var
  AElement: TdxSkinElement;
  AImageIndex: Integer;
begin
  AElement := SkinInfo.RibbonMinimizeButtonGlyph;
  if AElement <> nil then
  begin
    if AElement.ImageCount > 2 then
      AImageIndex := Ord(AGlyph)
    else
      AImageIndex := Ord(AGlyph <> rmbMinimize);

    DrawElement(DC, R, 0, AElement, AImageIndex);
  end
  else
    inherited DrawMinimizeRibbonButtonGlyph(DC, R, AState, AGlyph);
end;

procedure TdxSkinRibbonPainter.DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.PopupMenuCheck <> nil) then
    SkinInfo.PopupMenuCheck.Draw(DC, R, ScaleFactor, 1)
  else
    inherited DrawMenuCheck(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.PopupMenuCheck) then
    inherited DrawMenuCheckMark(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawMenuContent(DC: HDC; const R: TRect);
begin
  if UseSkins and (SkinInfo.PopupMenu <> nil) then
    FillRectByColor(DC, R, SkinInfo.PopupMenu.Color)
  else
    inherited DrawMenuContent(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawMenuExtraSeparator(
  DC: HDC; const R: TRect; AHorizontal: Boolean);
var
  AColor: TdxSkinColor;
  R1: TRect;
begin
  if UseSkins then
    AColor := SkinInfo.RibbonExtraPaneHeaderSeparator
  else
    AColor := nil;

  if (AColor = nil) or (AColor.Value = clDefault) then
    inherited DrawMenuExtraSeparator(DC, R, AHorizontal)
  else
  begin
    if AHorizontal then
      R1 := cxRectCenterVertically(R, 1)
    else
      R1 := cxRectCenterHorizontally(R, 1);

    FillRectByColor(DC, R1, AColor.Value);
  end;
end;

procedure TdxSkinRibbonPainter.DrawMenuGlyph(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  if UseSkins and (SkinInfo.PopupMenuSideStrip <> nil) then
  begin
    R1 := R;
    Inc(R1.Right, Max(2, SkinInfo.PopupMenuSideStrip.Image.Margins.Right));
    DrawClippedElement(DC, R, R1, SkinInfo.PopupMenuSideStrip, esNormal);
  end
  else
    inherited DrawMenuGlyph(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer);
begin
  if not UseSkins then
    inherited DrawMenuScrollArea(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawMenuSeparatorHorz(DC: HDC; const R: TRect);
begin
  if UseSkins and (SkinInfo.PopupMenuSeparator <> nil) then
  begin
    if SkinInfo.PopupMenuSeparator.IsAlphaUsed then
      DrawMenuContent(DC, R);
    SkinInfo.PopupMenuSeparator.Draw(DC, R, dxDefaultScaleFactor);
  end
  else
    inherited DrawMenuSeparatorHorz(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawMenuSeparatorVert(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  if UseSkins and (SkinInfo.PopupMenuSideStrip <> nil) then
  begin
    R1 := R;
    R1.Left := R.Right - Max(2, SkinInfo.PopupMenuSideStrip.Size.cx);
    DrawClippedElement(DC, R, R1, SkinInfo.PopupMenuSideStrip, esNormal);
  end
  else
    inherited DrawMenuGlyph(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawMenuItem(DC: HDC; const R: TRect; AState: Integer);
begin
  if UseSkins and (SkinInfo.PopupMenuLinkSelected <> nil) then
  begin
    if AState in [DXBAR_HOT, DXBAR_ACTIVE, DXBAR_ACTIVEDISABLED] then
      SkinInfo.PopupMenuLinkSelected.Draw(DC, R, ScaleFactor, 0, esHot);
  end
  else
    inherited DrawMenuItem(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawMenuItemDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.PopupMenuSplitButton2) then
    inherited DrawMenuItemDropButtonArrowPart(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawMenuItemDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.PopupMenuSplitButton) then
    inherited DrawMenuItemDropButtonMainPart(DC, R, AState);
end;

procedure TdxSkinRibbonPainter.DrawMenuMark(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.PopupMenuExpandButton) then
    inherited DrawMenuMark(DC, R);
end;

function TdxSkinRibbonPainter.GetMenuColorPalette(AState: Integer): IdxColorPalette;
begin
  if UseSkins then
    Result := GetColorPalette(SkinInfo.PopupMenu, AState)
  else
    Result := nil;
end;

procedure TdxSkinRibbonPainter.DrawRibbonClientTopArea(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  if UseSkins and (SkinInfo.RibbonFormCaption <> nil) then
  begin
    ARect := R;
    with SkinInfo.RibbonFormCaption.Image.Margins.Margin do
      ARect := Rect(R.Left - Left, R.Top - 1, R.Right + Right, R.Bottom);
    DrawClippedElement(DC, R, ARect, SkinInfo.RibbonFormCaption, esNormal);
  end
  else
    inherited DrawRibbonClientTopArea(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonFormContent) then
    inherited DrawRibbonFormBackground(DC, R, ARibbonHeight);
end;

procedure TdxSkinRibbonPainter.DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean);
begin
  if AUseAeroGlass and not DrawElement(DC, R, SkinInfo.RibbonHeaderBackgroundOnGlass) then
    inherited DrawRibbonTopFrameArea(DC, R, AUseAeroGlass)
end;

procedure TdxSkinRibbonPainter.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
begin
  if not UseSkins then
    inherited DrawRibbonTopFrameAreaSeparator(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawScreenTip(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.ScreenTipWindow) then
    inherited DrawScreenTip(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawSeparatorBackground(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.ItemSeparator) then
    inherited DrawSeparatorBackground(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonSmallButton
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawSmallButton(DC, R, AState)
  else
  begin
    AElement.UseCache := True;
    AElement.Draw(DC, R, ScaleFactor, 0, dxSkinElementCheckState(AElement, RibbonStateToSkinElementState(AState)));
  end;
end;

procedure TdxSkinRibbonPainter.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonSplitButtonRight) then
    inherited DrawSmallButtonDropButtonArrowPart(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, AState, SkinInfo.RibbonSplitButtonLeft) then
    inherited DrawSmallButtonDropButtonMainPart(DC, R, AState);
end;

function TdxSkinRibbonPainter.GetSmallButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  if UseSkins then
    Result := GetColorPalette(SkinInfo.RibbonSmallButton, AState)
  else
    Result := nil;
end;

procedure TdxSkinRibbonPainter.DrawStatusBar(DC: HDC; const R: TRect);
begin
  if not DoDrawStatusBarBackground(DC, R, R, False) then
    inherited DrawStatusBar(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawStatusBarSizeGrip(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  if UseSkins and (SkinInfo.SizeGrip <> nil) then
  begin
    R1 := cxRectSetBottom(R, R.Bottom, ScaleFactor.Apply(SkinInfo.SizeGrip.MinSize.Height));
    if UseRightToLeftAlignment then
    begin
      R1 := cxRectSetLeft(R1, R1.Left + ScaleFactor.Apply(2), ScaleFactor.Apply(SkinInfo.SizeGrip.MinSize.Width));
      SkinInfo.SizeGrip.DrawRTL(DC, R1, ScaleFactor);
    end
    else
    begin
      R1 := cxRectSetRight(R1, R1.Right - ScaleFactor.Apply(2), ScaleFactor.Apply(SkinInfo.SizeGrip.MinSize.Width));
      SkinInfo.SizeGrip.Draw(DC, R1, ScaleFactor);
    end;
  end
  else
    inherited DrawStatusBarSizeGrip(DC, R);
end;

procedure TdxSkinRibbonPainter.DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean);
begin
  if not DoDrawStatusBarBackground(DC, Bounds, R, not AIsLowered) then
    inherited DrawStatusBarPanel(DC, Bounds, R, AIsLowered);
end;

procedure TdxSkinRibbonPainter.DrawStatusBarPanelSeparator(DC: HDC; const R: TRect);
begin
  if UseSkins and (SkinInfo.RibbonStatusBarSeparator <> nil) then
    SkinInfo.RibbonStatusBarSeparator.Draw(DC, R, dxDefaultScaleFactor)
  else
    inherited DrawStatusBarPanelSeparator(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  if UseSkins and (SkinInfo.RibbonStatusBarSeparator <> nil) then
  begin
    R1 := R;
    Inc(R1.Bottom, GetBordersWidth(True).Bottom);
    SkinInfo.RibbonStatusBarSeparator.Draw(DC, R1, dxDefaultScaleFactor);
  end
  else
    inherited DrawStatusBarToolbarSeparator(DC, R);
end;

function TdxSkinRibbonPainter.GetStatusBarPanelColorPalette(AState: Integer): IdxColorPalette;
begin
  if UseSkins then
    Result := GetColorPalette(SkinInfo.RibbonStatusBarButton, AState)
  else
    Result := nil;
end;

function TdxSkinRibbonPainter.GetScrollBarPainter: TcxCustomLookAndFeelPainter;
begin
  if UseSkins then
    Result := Painter
  else
    Result := inherited GetScrollBarPainter;
end;

procedure TdxSkinRibbonPainter.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
begin
  if UseSkins and (SkinInfo.RibbonTab <> nil) then
    SkinInfo.RibbonTab.Draw(DC, CorrectTabHeaderRect(R), ScaleFactor, 0, RibbonTabStatesMap[AState])
  else
    inherited DrawTab(DC, R, AState)
end;

procedure TdxSkinRibbonPainter.DrawTabAreaBackground(DC: HDC; const R: TRect;
  AActive, AUseAeroGlass: Boolean; AApplicationMenuState: TdxRibbonApplicationMenuState);
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonHeaderBackground
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawTabAreaBackground(DC, R, AActive, AUseAeroGlass, AApplicationMenuState)
  else
    if not (AUseAeroGlass and ExtendCaptionAreaOnTabs) then
    begin
      if AElement.IsAlphaUsed then
        AElement.Draw(DC, R, ScaleFactor, 1);
      AElement.Draw(DC, R, ScaleFactor);
    end;
end;

procedure TdxSkinRibbonPainter.DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esNormal, esHot);
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonTabGroup
  else
    AElement := nil;

  if AElement = nil then
    inherited DrawTabGroupBackground(DC, R, AState, AIsInPopup)
  else
  begin
    AElement.UseCache := True;
    if UseRightToLeftAlignment then
      AElement.DrawRTL(DC, R, ScaleFactor, 0, StateMap[AState = DXBAR_HOT])
    else
      AElement.Draw(DC, R, ScaleFactor, 0, StateMap[AState = DXBAR_HOT]);
  end;
end;

procedure TdxSkinRibbonPainter.DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esNormal, esHot);
begin
  if UseSkins and (SkinInfo.RibbonTabGroupHeader <> nil) then
    if UseRightToLeftAlignment then
      SkinInfo.RibbonTabGroupHeader.DrawRTL(DC, CorrectTabPanelRect(AIsInPopup, R), ScaleFactor, 0, StateMap[AState = DXBAR_HOT])
    else
      SkinInfo.RibbonTabGroupHeader.Draw(DC, CorrectTabPanelRect(AIsInPopup, R), ScaleFactor, 0, StateMap[AState = DXBAR_HOT])
  else
    inherited DrawTabGroupHeaderBackground(DC, R, AState, AIsInPopup);
end;

procedure TdxSkinRibbonPainter.DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  if UseSkins and (SkinInfo.RibbonTabPanel <> nil) then
  begin
    SkinInfo.RibbonTabPanel.UseCache := True;
    SkinInfo.RibbonTabPanel.Draw(DC, CorrectTabPanelRect(AIsInPopup, R), ScaleFactor);
  end
  else
    inherited DrawTabGroupsArea(DC, R, AIsQATAtBottom, AIsInPopup);
end;

procedure TdxSkinRibbonPainter.DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
begin
  if UseSkins and (SkinInfo.RibbonSmallButton <> nil) then
    SkinInfo.RibbonSmallButton.Draw(DC, R, ScaleFactor, 0, RibbonStateToSkinElementState(AState))
  else
    inherited DrawTabScrollButton(DC, R, ALeft, AState);
end;

procedure TdxSkinRibbonPainter.DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte);
var
  ABitmap: TcxBitmap32;
begin
  if UseSkins and (SkinInfo.RibbonTabSeparatorLine <> nil) then
  begin
    ABitmap := TcxBitmap32.CreateSize(R, True);
    try
      SkinInfo.RibbonTabSeparatorLine.Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, dxDefaultScaleFactor);
      cxAlphaBlend(DC, ABitmap, R, ABitmap.ClientRect, False, Alpha);
    finally
      ABitmap.Free;
    end;
  end
  else
    inherited DrawTabSeparator(DC, R, Alpha);
end;

function TdxSkinRibbonPainter.GetTabAreaButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  if UseSkins then
    Result := GetColorPalette(SkinInfo.RibbonHeaderBackground, AState)
  else
    Result := nil;
end;

procedure TdxSkinRibbonPainter.DrawMiniToolbarBackground(DC: HDC; const R: TRect);
var
  AElement: TdxSkinElement;
  ASaveIndex: Integer;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonTabGroup
  else
    AElement := nil;

  if AElement <> nil then
  begin
    ASaveIndex := SaveDC(DC);
    try
      FillRectByColor(DC, R, Painter.DefaultContentColor);
      IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
      AElement.Draw(DC, cxRectInflate(R, AElement.Image.Margins.Margin), ScaleFactor);
    finally
      RestoreDC(DC, ASaveIndex)
    end;
  end
  else
    inherited DrawMiniToolbarBackground(DC, R);
end;

function TdxSkinRibbonPainter.GetMiniToolbarColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetSmallButtonColorPalette(AState);
end;

procedure TdxSkinRibbonPainter.DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonQuickToolbarGlyph) then
    inherited;
end;

procedure TdxSkinRibbonPainter.DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.RibbonQuickToolbarGlyph;
  if (AElement <> nil) and (AElement.ImageCount > 1) then
    DrawElement(DC, R, DXBAR_NORMAL, AElement, 1)
  else
    inherited;
end;

procedure TdxSkinRibbonPainter.DrawQuickAccessToolbar(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean);

  function IsAeroBackgroundUsed: Boolean;
  begin
    Result := ANonClientDraw and IsCompositionEnabled and not (ADontUseAero or ABellow);
  end;

  function NeedDrawCustomQuickAccessToolBar(AElement: TdxSkinElement): Boolean;
  begin
    Result := (AElement = nil) or IsAeroBackgroundUsed and not AElement.Image.Empty;
  end;

  function ValidateQATRect(const R: TRect; AParent: TdxSkinElement): TRect;
  var
    ARightIndent: Integer;
  begin
    Result := R;
    if not ABellow then
    begin
      if AParent = nil then
        ARightIndent := 0
      else
        with AParent.ContentOffset.Rect do
        begin
          ARightIndent := GetQuickAccessToolbarRightIndent(AHasApplicationButton);
          Result := cxRectInflate(R, 0, -Top, 0, -Bottom);
        end;

      if GetCustomizeButtonOutsizeQAT(AHasApplicationButton) then
      begin
        Dec(ARightIndent, ScaleFactor.Apply(QATRightDefaultOffset));
        Dec(ARightIndent, GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton, ABellow));
      end;
      Inc(Result.Right, ARightIndent);
    end;
    if UseRightToLeftAlignment then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, R);
  end;

var
  AElement: TdxSkinElement;
  ARect: TRect;
begin
  AElement := GetQATBackgroundElement(ABellow, AHasApplicationButton);
  if NeedDrawCustomQuickAccessToolBar(AElement) then
    inherited DrawQuickAccessToolbar(DC, R, ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero)
  else
    if not IsAeroBackgroundUsed then
    begin
      ARect := ValidateQATRect(R, SkinInfo.RibbonFormCaption);
      if ABellow or (cxRectWidth(ARect) >= cxRectHeight(ARect)) then
        DrawElement(DC, ARect, AElement);
    end;
end;

procedure TdxSkinRibbonPainter.DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonQuickToolbarButtonGlyph) then
    inherited DrawQuickAccessToolbarDefaultGlyph(DC, R)
end;

procedure TdxSkinRibbonPainter.DrawQuickAccessToolbarGroupButton(
  DC: HDC; const R: TRect; ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer);
var
  ABackground: TdxSkinElement;
  AElement: TdxSkinElement;
begin
  if UseSkins then
  begin
    ABackground := SkinInfo.RibbonButtonGroup;
    AElement := SkinInfo.RibbonButtonGroupButton;
  end
  else
  begin
    AElement := nil;
    ABackground := nil;
  end;

  if (AElement = nil) or (ABackground = nil) then
    inherited DrawQuickAccessToolbarGroupButton(DC, R, ABellow, ANonClientDraw, AIsActive, AState)
  else
  begin
    ABackground.Draw(DC, R, ScaleFactor);
    AElement.Draw(DC,
      cxRectContent(R, ScaleFactor.Apply(ABackground.ContentOffset.Rect)),
      ScaleFactor, 0, RibbonStateToSkinElementState(AState));
  end;
end;

procedure TdxSkinRibbonPainter.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  if not DrawElement(DC, R, SkinInfo.RibbonQuickToolbarDropDown) then
    inherited DrawQuickAccessToolbarPopup(DC, R)
end;

function TdxSkinRibbonPainter.GetQuickAccessToolbarColorPalette(AState: Integer; ABelow: Boolean): IdxColorPalette;
begin
  Result := GetColorPalette(GetQATBackgroundElement(ABelow, False), AState);
  if Result = nil then
    Result := GetSmallButtonColorPalette(AState);
end;

procedure TdxSkinRibbonPainter.LoadRibbonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONBLACK', AImage);
end;

function TdxSkinRibbonPainter.GetApplicationMenuGlyphSize: TSize;
begin
  if ApplicationButtonElement <> nil then
    Result := GetElementMinSize(ApplicationButtonElement, ScaleFactor)
  else
    Result := inherited GetApplicationMenuGlyphSize;
end;

function TdxSkinRibbonPainter.GetPropertyColor(AColor: TdxSkinColor): TColor;
begin
  if AColor = nil then
    Result := clDefault
  else
    Result := AColor.Value;
end;

function TdxSkinRibbonPainter.GetElementContentIndents(AElement: TdxSkinElement;
  AConsideMargins: Boolean; out ALeftIndent, ARightIndent: Integer): Boolean;
begin
  Result := Assigned(AElement);
  if Result then
  begin
    ALeftIndent := AElement.ContentOffset.Left;
    ARightIndent := AElement.ContentOffset.Right;
    if AConsideMargins then
    begin
      ALeftIndent := Max(AElement.Image.Margins.Left, ALeftIndent);
      ARightIndent := Max(AElement.Image.Margins.Right, ARightIndent);
    end;
  end;
end;

function TdxSkinRibbonPainter.GetIsAlphaUsed(APart: Integer): Boolean;
var
  AElement: TdxSkinElement;
begin
  AElement := nil;
  if UseSkins then
  begin
    if APart = DXBAR_INRIBBONGALLERYSCROLLBAR_BACKGROUND then
      AElement := SkinInfo.RibbonGalleryPane;
  end;
  if AElement = nil then
    Result := inherited GetIsAlphaUsed(APart)
  else
    Result := AElement.IsAlphaUsed;
end;

function TdxSkinRibbonPainter.GetRadialMenuColorPalette: IdxColorPalette;
begin
  if UseSkins then
    Result := SkinInfo.Skin.ActiveColorPalette
  else
    Result := nil;
end;

function TdxSkinRibbonPainter.GetMenuSeparatorSize: Integer;
begin
  if UseSkins and (SkinInfo.PopupMenuSeparator <> nil) then
    Result := Max(SkinInfo.PopupMenuSeparator.Size.cy, SkinInfo.PopupMenuSeparator.MinSize.Height)
  else
    Result := inherited GetMenuSeparatorSize;
end;

function TdxSkinRibbonPainter.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  Result := clDefault;
  if UseSkins then
    case APart of
      DXBAR_RADIALMENUACCENT:
        Result := GetPropertyColor(SkinInfo.RadialMenuBaseColor);
      DXBAR_RADIALMENUBACKGROUND:
        Result := GetPropertyColor(SkinInfo.RadialMenuBackgroundColor);
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_BORDER:
        Result := GetPropertyColor(SkinInfo.ContainerBorderColor);
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.GalleryGroup, AState);
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.GalleryItem, AState);
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.GalleryItem, AState, sdxDescriptionTextColorPrefix);
      DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.RibbonBackstageViewTab, AState);
      DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.RibbonBackstageViewMenuButton, AState);

      DXBAR_DROPDOWNGALLERYITEM_TEXTCOLOR:
        begin
          if AState <> DXBAR_NORMAL then
            Result := GetElementTextColor(SkinInfo.RibbonSmallButton, AState);
          if Result = clDefault then
            Result := GetElementTextColor(SkinInfo.RibbonGalleryBackground, AState);
        end;

      DXBAR_ITEMTEXT, DXBAR_BUTTONITEMTEXT:
        case AState of
          DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
            Result := SkinInfo.RibbonButtonText[True]
          else
            Result := SkinInfo.RibbonButtonText[False];
        end;

      DXBAR_SEPARATOR_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.ItemSeparator, AState);

      DXBAR_APPLICATIONMENUBUTTON:
        Result := GetElementTextColor(SkinInfo.ButtonElements, AState);

      DXBAR_MENUITEMTEXT, DXBAR_MENUBUTTONITEMTEXT:
        case AState of
          DXBAR_DISABLED:
            Result := GetPropertyColor(SkinInfo.BarDisabledTextColor);
          else
            Result := GetElementTextColor(SkinInfo.PopupMenu, AState);
        end;

      DXBAR_EDIT_BORDER:
        if AState = DXBAR_ACTIVE then
          Result := GetPropertyColor(SkinInfo.ContainerHighlightBorderColor)
        else
          Result := GetPropertyColor(SkinInfo.ContainerBorderColor);

      rfspRibbonForm, rspRibbonBackground:
        if SkinInfo.FormContent <> nil then
          Result := SkinInfo.FormContent.Color;

      DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
        if SkinInfo.RibbonBackstageView <> nil then
        begin
          if AState = DXBAR_DISABLED then
            Result := SkinInfo.EditorTextColors[esckDisabled].Value;
          Result := GetElementTextColor(SkinInfo.RibbonBackstageView, AState, '', Result);
        end;

      rspContextTabHeaderText:
        begin
          if AState = DXBAR_HOT then
            Result := SkinInfo.RibbonContextualTabHeaderTextHot
          else
            Result := SkinInfo.RibbonContextualTabHeaderText[AState = DXBAR_ACTIVE];

          if not cxColorIsValid(Result) then
            Result := GetPartColor(rspTabHeaderText, AState);
        end;

      rspTabHeaderText:
        begin
          case AState of
            DXBAR_HOT:
              Result := SkinInfo.RibbonTabTextHot;
            DXBAR_ACTIVE:
              Result := SkinInfo.RibbonTabText[True];
            else
              Result := clDefault;
          end;
          if Result = clDefault then
            Result := SkinInfo.RibbonTabText[False];
        end;

      rspContextText:
        Result := GetElementTextColor(SkinInfo.RibbonContextualTabLabel, AState);
      rspContextTextOnGlass:
        Result := GetElementTextColor(SkinInfo.RibbonContextualTabLabelOnGlass, AState);
      rspContextTextShadow:
        Result := GetPropertyColor(SkinInfo.RibbonContextualTabLabelShadowColor);
      rspContextTextOnGlassShadow:
        begin
          Result := GetPropertyColor(SkinInfo.RibbonContextualTabLabelOnGlassShadowColor);
          if Result = clDefault then
            Result := GetPartColor(rspContextTextShadow, AState);
        end;

      DXBAR_KEYTIP_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.RibbonKeyTip, AState);
      DXBAR_SCREENTIP_TITLE:
        Result := GetPropertyColor(SkinInfo.ScreenTipTitleItem);
      DXBAR_SCREENTIP_DESCRIPTION:
        Result := GetPropertyColor(SkinInfo.ScreenTipItem);
      DXBAR_MENUEXTRAPANE:
        Result := GetPropertyColor(SkinInfo.RibbonExtraPaneColor);
      DXBAR_MENUEXTRAPANE_BUTTON_TEXTCOLOR, DXBAR_MENUEXTRAPANE_HEADER_TEXTCOLOR:
        Result := GetElementTextColor(SkinInfo.RibbonExtraPaneButton, AState);
      DXBAR_EDIT_BACKGROUND:
        Result := GetPropertyColor(SkinInfo.RibbonEditorBackground);
      DXBAR_SEPARATOR_BACKGROUND:
        Result := GetPropertyColor(SkinInfo.ContentColor);
      DXBAR_EDIT_TEXTCOLOR:
        Result := Painter.DefaultEditorTextColor(AState = DXBAR_DISABLED);
      DXBAR_SCREENTIP_FOOTERLINE:
        Result := GetPropertyColor(SkinInfo.ContainerBorderColor);
      rspFormCaptionText:
        Result := SkinInfo.RibbonCaptionText[AState = DXBAR_NORMAL];
      rspDocumentNameText:
        Result := SkinInfo.RibbonDocumentNameTextColor[AState = DXBAR_NORMAL];
      rspTabGroupText:
        Result := GetElementTextColor(SkinInfo.RibbonSmallButton, AState);
      rspTabGroupHeaderText:
        Result := GetElementTextColor(SkinInfo.RibbonTabGroupHeader, AState);
      rspApplicationButton:
        Result := GetElementTextColor(SkinInfo.RibbonApplicationButton2010, AState);
      rspStatusBarText:
        case AState of
          DXBAR_NORMAL, DXBAR_HOT, DXBAR_HOTCHECK, DXBAR_DISABLED:
            Result := GetElementTextColor(SkinInfo.RibbonStatusBarButton, AState);
          DXBAR_CHECKED:
            begin
              Result := SkinInfo.RibbonStatusBarTextSelected;
              if Result = clDefault then
                Result := GetPartColor(rspStatusBarText, DXBAR_NORMAL);
            end;
        end;

      DXBAR_GALLERYGROUPHEADERTEXT, DXBAR_GALLERYFILTERBANDTEXT:
        Result := GetElementTextColor(SkinInfo.RibbonGalleryGroupCaption, AState);
    end;

  if Result = clDefault then
    Result := inherited DoGetPartColor(APart, AState);
end;

function TdxSkinRibbonPainter.GetPartContentOffsets(APart: Integer): TRect;

  function GetElementForPart(APart: Integer; out AElement: TdxSkinElement): Boolean;
  begin
    case APart of
      DXBAR_BACKSTAGEVIEW:
        AElement := SkinInfo.RibbonBackstageView;
      DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
        AElement := SkinInfo.RibbonBackstageViewMenuButton;
      DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
        AElement := SkinInfo.RibbonBackstageViewTab;
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
        AElement := SkinInfo.RibbonGalleryGroupCaption;
      DXBAR_APPLICATIONMENUBUTTON:
        AElement := SkinInfo.RibbonApplicationBackground;
      DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
        AElement := SkinInfo.RibbonTabGroup;
      DXBAR_RIBBONTABGROUP:
        AElement := SkinInfo.RibbonTabPanel;
      DXBAR_RIBBONCONTEXTTABGROUP:
        AElement := SkinInfo.RibbonContextualTabPanel;
      DXBAR_QUICKACCESSTOOLBAR:
        AElement := SkinInfo.RibbonQuickToolbarBelow;
      DXBAR_APPLICATIONBUTTONICONOFFSET:
        AElement := ApplicationButtonElement;
    else
      AElement := nil;
    end;
    Result := AElement <> nil;
  end;

var
  AElement: TdxSkinElement;
begin
  Result := inherited GetPartContentOffsets(APart);
  if UseSkins then
  begin
    if GetElementForPart(APart, AElement) then
    begin
      if APart = DXBAR_APPLICATIONMENUBUTTON then
        Result.Right := Max(Result.Top, ScaleFactor.Apply(AElement.ContentOffset.Right)) - ScaleFactor.Apply(AElement.ContentOffset.Right)
      else
        Result := ScaleFactor.Apply(AElement.ContentOffset.Rect);
    end;

    if APart = DXBAR_BACKSTAGEVIEW_MENUBAR then
    begin
      Result.Right := 0;
      Result.Left := 0;
    end;
  end;
end;

function TdxSkinRibbonPainter.GetPartSize(APart: Integer): Integer;

  function GetElementSize(AElement: TdxSkinElement): TSize;
  begin
    if AElement <> nil then
      Result := ScaleFactor.Apply(AElement.MinSize.Size)
    else
      Result := cxNullSize;
  end;

  function GetValue(AProperty: TdxSkinIntegerProperty): Integer;
  begin
    if AProperty <> nil then
      Result := AProperty.Value
    else
      Result := 1;

    Result := ScaleFactor.Apply(Result);
  end;

begin
  Result := inherited GetPartSize(APart);
  if UseSkins then
  begin
    case APart of
      DXBAR_BACKSTAGEVIEW_MENUBAR:
        Result := GetElementSize(SkinInfo.RibbonBackstageViewMenu).cx;
      DXBAR_BACKSTAGEVIEW_BACKBUTTON:
        Result := GetElementSize(SkinInfo.RibbonBackstageViewBackButton).cy;
      DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
        Result := Max(Result, GetElementSize(SkinInfo.RibbonBackstageViewTab).cy);
      DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
        Result := Max(Result, GetElementSize(SkinInfo.RibbonBackstageViewMenuButton).cy);
      DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
        Result := GetElementSize(SkinInfo.RibbonBackstageViewMenuSeparator).cy;
      DXBAR_SEPARATOR_LINE:
        Result := 0;
      DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
        Result := GetValue(SkinInfo.RibbonSpaceBetweenTabGroups);
      DXBAR_TABSGROUPSOVERLAPHEIGHT:
        Result := GetValue(SkinInfo.RibbonTabHeaderDownGrowIndent);
      DXBAR_BUTTONGROUPSPLITBUTTONSEPARATOR:
        Result := 0;
      DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
        Result := Painter.LabelLineHeight;
      rspTabGroupBottomOffset:
        if SkinInfo.RibbonTabGroupHeader <> nil then
          Result := ScaleFactor.Apply(SkinInfo.RibbonTabGroupHeader.ContentOffset.Bottom);
    end;
  end;
end;

function TdxSkinRibbonPainter.GetQuickAccessToolbarLeftIndent(
  AHasApplicationButton: Boolean; AUseAeroGlass: Boolean): Integer;
begin
  if not AUseAeroGlass and UseSkins then
  begin
    Result := -QATLeftDefaultOffset;
    if SkinInfo.RibbonQuickToolbar[AHasApplicationButton] <> nil then
      Inc(Result, ScaleFactor.Apply(SkinInfo.RibbonQuickToolbar[AHasApplicationButton].ContentOffset.Left));
    Inc(Result, GetQATLeftOffset(AHasApplicationButton));
  end
  else
    Result := inherited GetQuickAccessToolbarLeftIndent(AHasApplicationButton, AUseAeroGlass);
end;

function TdxSkinRibbonPainter.GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton: Boolean; ABelow: Boolean): Integer;
var
  AOffsetProperty: TdxSkinIntegerProperty;
begin
  if UseSkins then
    AOffsetProperty := SkinInfo.RibbonQATIndentBeforeCustomizeButton[AHasApplicationButton]
  else
    AOffsetProperty := nil;

  if ABelow or (AOffsetProperty = nil) then
    Result := inherited GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton, ABelow)
  else
    Result := ScaleFactor.Apply(AOffsetProperty.Value);
end;

function TdxSkinRibbonPainter.GetQuickAccessToolbarOverrideWidth(AHasApplicationButton: Boolean; AUseAeroGlass: Boolean): Integer;
begin
  if not AUseAeroGlass and UseSkins then
  begin
    Result := GetQuickAccessToolbarLeftIndent(AHasApplicationButton, AUseAeroGlass) + QATLeftDefaultOffset;
    Dec(Result, GetQATLeftOffset(AHasApplicationButton));
  end
  else
    Result := inherited GetQuickAccessToolbarOverrideWidth(AHasApplicationButton, AUseAeroGlass);
end;

function TdxSkinRibbonPainter.GetQuickAccessToolbarRightIndent(AHasApplicationButton: Boolean): Integer;
begin
  if UseSkins and (SkinInfo.RibbonQuickToolbar[AHasApplicationButton] <> nil) then
    Result := ScaleFactor.Apply(SkinInfo.RibbonQuickToolbar[AHasApplicationButton].ContentOffset.Right)
  else
    Result := inherited GetQuickAccessToolbarRightIndent(AHasApplicationButton);
end;

function TdxSkinRibbonPainter.GetSkinName: string;
begin
  Result := Painter.LookAndFeelName;
end;

function TdxSkinRibbonPainter.GetStatusBarSeparatorSize: Integer;
var
  AElement: TdxSkinElement;
begin
  if UseSkins then
    AElement := SkinInfo.RibbonStatusBarSeparator
  else
    AElement := nil;

  if AElement <> nil then
  begin
    Result := AElement.MinSize.Width;
    if Result = 0 then
      Result := AElement.Image.Size.cx;
    if Result = 0 then
      Result := inherited GetStatusBarSeparatorSize;
  end
  else
    Result := inherited GetStatusBarSeparatorSize;
end;

function TdxSkinRibbonPainter.GetWindowBordersWidth(AHasStatusBar: Boolean): TRect;
begin
  if UseSkins then
    Result := GetBordersWidth(AHasStatusBar)
  else
    Result := inherited GetWindowBordersWidth(AHasStatusBar);
end;

function TdxSkinRibbonPainter.HasExternalRibbonFormShadow: Boolean;
begin
  Result := not UseRoundedWindowCorners and (cxMarginsWidth(GetBordersWidth(True)) <= 4);
end;

function TdxSkinRibbonPainter.IsInternalPainter: Boolean;
begin
  Result := inherited IsInternalPainter or Painter.IsInternalPainter;
end;

function TdxSkinRibbonPainter.UseRoundedWindowCorners: Boolean;
begin
  if UseSkins and (SkinInfo.RibbonUseRoundedWindowCorners <> nil) then
    Result := SkinInfo.RibbonUseRoundedWindowCorners.Value
  else
    Result := inherited UseRoundedWindowCorners;
end;

{ TdxSkinRibbon2010Painter }

procedure TdxSkinRibbon2010Painter.AdjustBackstageViewTabButtonFont(AFont: TFont);
begin
  if AFont.Size > 0 then
    AFont.Size := AFont.Size + ScaleFactor.Apply(1);
end;

function TdxSkinRibbon2010Painter.ExtendCaptionAreaOnTabs: Boolean;
begin
  if UseSkins then
    Result := TabAeroSupport
  else
    Result := inherited ExtendCaptionAreaOnTabs;
end;

function TdxSkinRibbon2010Painter.GetApplicationButtonElement: TdxSkinElement;
begin
  if UseSkins then
    Result := SkinInfo.RibbonApplicationButton2010
  else
    Result := nil;
end;

function TdxSkinRibbon2010Painter.GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect;
begin
  Result := inherited GetApplicationMenuContentOffset(ATabsBounds);
  if UseSkins then
    Result.Top := cxRectHeight(ATabsBounds);
end;

function TdxSkinRibbon2010Painter.GetPartContentOffsets(APart: Integer): TRect;
begin
  if UseSkins and (APart = DXBAR_APPLICATIONBUTTON) then
    Result := cxRect(GetApplicationButtonIndent(sdxRibbonAppButtonLeftIndent) + 1,
      2, GetApplicationButtonIndent(sdxRibbonAppButtonRightIndent), 0)
  else
    Result := inherited GetPartContentOffsets(APart);
end;

function TdxSkinRibbon2010Painter.GetStyle: TdxRibbonStyle;
begin
  Result := rs2010;
end;

{ TdxSkinRibbon2013Painter }

procedure TdxSkinRibbon2013Painter.AfterConstruction;
begin
  inherited;
  FCachedColors[False] := clDefault;
  FCachedColors[True] := clDefault;
end;

procedure TdxSkinRibbon2013Painter.DrawBackstageViewMenuBackground(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  Dec(R1.Left, GetBordersWidth(False).Left);
  if UseRightToLeftAlignment then
    R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
  inherited DrawBackstageViewMenuBackground(DC, R1);
end;

procedure TdxSkinRibbon2013Painter.DrawBackstageViewMenuHeader(DC: HDC; const R: TRect);
var
  R1: TRect;
begin
  R1 := R;
  Dec(R1.Left, GetBordersWidth(False).Left);
  Inc(R1.Top, GetBordersWidth(False).Bottom - 1);
  if UseRightToLeftAlignment then
    R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
  inherited DrawBackstageViewMenuHeader(DC, R1);
end;

procedure TdxSkinRibbon2013Painter.AdjustBackstageViewTabButtonFont(AFont: TFont);
begin
  // do nothing
end;

procedure TdxSkinRibbon2013Painter.DrawBackstageViewBorders(DC: HDC; const R, ABordersWidth: TRect; AIsActive: Boolean);
var
  ASaveIndex: Integer;
  AThinBorders: TRect;
  R1: TRect;
begin
  ASaveIndex := SaveDC(DC);
  try
    R1 := cxRectContent(R, ABordersWidth);
    ExcludeClipRect(DC, R1.Left, R1.Top, R1.Right, R1.Bottom);

    AThinBorders.Bottom := Ord(ABordersWidth.Bottom > 0);
    AThinBorders.Right := Ord(ABordersWidth.Right > 0);
    AThinBorders.Left := Ord(ABordersWidth.Left > 0);
    AThinBorders.Top := Ord(ABordersWidth.Top > 0);

    R1 := cxRectContent(R, AThinBorders);
    FillRectByColor(DC, R1, SkinInfo.RibbonBackstageView.Color);
    ExcludeClipRect(DC, R1.Left, R1.Top, R1.Right, R1.Bottom);
    FillRectByColor(DC, R, GetBackstageViewBorderColor(AIsActive));
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxSkinRibbon2013Painter.DrawFormBorders(DC: HDC; const BordersWidth: TRect);
var
  ABordersWidth, R1: TRect;
  ASaveIndex: Integer;
begin
  if (PaintData.GetApplicationMenuState = ramsShownAsFullScreenFrame) and UseSkins then
  begin
    ASaveIndex := SaveDC(DC);
    try
      ABordersWidth := BordersWidth;

      R1 := cxRectContent(FormPaintData.GetBounds, ABordersWidth);
      ExcludeClipRect(DC, R1.Left, R1.Top, R1.Right, R1.Bottom);

      ABordersWidth.Top := PaintData.GetCaptionHeight;
      DrawBackstageViewBorders(DC, FormPaintData.GetBounds, ABordersWidth, FormPaintData.GetIsActive);
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end
  else
    inherited DrawFormBorders(DC, BordersWidth);
end;

procedure TdxSkinRibbon2013Painter.DrawFormCaption(DC: HDC; const R: TRect);
begin
  if (PaintData.GetApplicationMenuState = ramsShownAsFullScreenFrame) and UseSkins then
    DrawBackstageViewBorders(DC, R, cxRect(0, cxRectHeight(R), 0, 0), FormPaintData.GetIsActive)
  else
    inherited DrawFormCaption(DC, R);
end;

function TdxSkinRibbon2013Painter.GetPartContentOffsets(APart: Integer): TRect;
begin
  if (APart = DXBAR_BACKSTAGEVIEW) and UseSkins then
    Result := cxNullRect
  else
    Result := inherited GetPartContentOffsets(APart);
end;

function TdxSkinRibbon2013Painter.GetBackstageViewBorderColor(AIsActive: Boolean): TColor;

  function ExtractBorderColor(AIsActive: Boolean): TColor;
  var
    AAttributes: TdxSkinAlternateImageAttributes;
    ABackgroundLuminance: Single;
    ABitmap: TBitmap;
    AColor1, AColor2: TColor;
    AElement: TdxSkinElementAccess;
  begin
    AElement := TdxSkinElementAccess(SkinInfo.RibbonFormBottom[True]);
    Result := AElement.Borders.Bottom.Color;
    if AElement.CanUseAlternateImageSet(0, RibbonFormBorderStates[AIsActive], False, AAttributes) then
    begin
      Result := AAttributes.Borders.Bottom.Color;
      if not cxColorIsValid(Result) then
        Result := AAttributes.Borders.Top.Color;
      if not cxColorIsValid(Result) then
        Result := AAttributes.GradientBeginColor;
    end
    else
      if not AElement.Image.Empty then
      begin
        ABitmap := TcxBitmap.Create;
        try
          AElement.Image.GetBitmap(0, RibbonFormBorderStates[AIsActive], ABitmap);
          AColor1 := ABitmap.Canvas.Pixels[0, 0];
          AColor2 := ABitmap.Canvas.Pixels[0, ABitmap.Height - 1];
          ABackgroundLuminance := dxGetColorRelativeLuminance(SkinInfo.RibbonBackstageView.Color);
          if Abs(dxGetColorRelativeLuminance(AColor1) - ABackgroundLuminance) >
             Abs(dxGetColorRelativeLuminance(AColor2) - ABackgroundLuminance)
          then
            Result := AColor1
          else
            Result := AColor2;
        finally
          ABitmap.Free;
        end;
      end;
  end;

begin
  if FCachedColors[AIsActive] = clDefault then
    FCachedColors[AIsActive] := ExtractBorderColor(AIsActive);
  Result := FCachedColors[AIsActive];
end;

function TdxSkinRibbon2013Painter.GetStyle: TdxRibbonStyle;
begin
  Result := rs2013;
end;

{ TdxSkinRibbon2016Painter }

function TdxSkinRibbon2016Painter.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016;
end;

{ TdxSkinRibbon2016TabletPainter }

procedure TdxSkinRibbon2016TabletPainter.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  if LowColors then
    inherited DrawCollapsedToolbarGlyphBackground(DC, R, AState);
end;

function TdxSkinRibbon2016TabletPainter.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    rspTabGroupBottomOffset, rspTabGroupInPopupBottomOffset:
      Result := 2;
  else
    Result := inherited GetPartSize(APart);
  end;
end;

function TdxSkinRibbon2016TabletPainter.GetStyle: TdxRibbonStyle;
begin
  Result := rs2016Tablet;
end;

{ TdxSkinRibbon2019Painter }

procedure TdxSkinRibbon2019Painter.DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer);
var
  AGlyphRect: TRect;
  AGlyphSize: TSize;
begin
  if UseSkins and (SkinInfo.RibbonBackstageViewBackButton <> nil) then
  begin
    DrawBackstageViewMenuButton(DC, R, AState);

    AGlyphSize := ScaleFactor.Apply(SkinInfo.RibbonBackstageViewBackButton.MinSize.Size);
    AGlyphRect := cxRectCenterVertically(R, AGlyphSize.cy);
    AGlyphRect := cxRectSetWidth(AGlyphRect, AGlyphSize.cx);
    AGlyphRect := cxRectOffset(AGlyphRect, GetPartContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB).Left, 0);
    if UseRightToLeftAlignment then
      AGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(AGlyphRect, R);

    if UseRightToLeftAlignment then
      SkinInfo.RibbonBackstageViewBackButton.DrawRTL(DC, AGlyphRect, ScaleFactor, 0, RibbonStateToSkinElementState(AState))
    else
      SkinInfo.RibbonBackstageViewBackButton.Draw(DC, AGlyphRect, ScaleFactor, 0, RibbonStateToSkinElementState(AState));
  end
  else
    inherited;
end;

function TdxSkinRibbon2019Painter.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_BACKBUTTON:
      Result := GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB);
    DXBAR_BACKSTAGEVIEW_BACKBUTTON_OFFSET:
      Result := 0;
  else
    Result := inherited;
  end
end;

function TdxSkinRibbon2019Painter.GetStyle: TdxRibbonStyle;
begin
  Result := rs2019;
end;

{ TdxSkinsRibbonPainterManager }

constructor TdxSkinsRibbonPainterManager.Create;
begin
  inherited Create;
  cxLookAndFeelPaintersManager.AddListener(Self);
  InitializePaintersList;
end;

destructor TdxSkinsRibbonPainterManager.Destroy;
begin
  cxLookAndFeelPaintersManager.RemoveListener(Self);
  FreePaintersList;
  inherited Destroy;
end;

procedure TdxSkinsRibbonPainterManager.AddSkin(APainter: TcxCustomLookAndFeelPainter);

  function IsSkinAlreadyExists(const AName: string): Boolean;
  var
    AStyle: TdxRibbonStyle;
  begin
    Result := False;
    for AStyle := Low(TdxRibbonStyle) to High(TdxRibbonStyle) do
      Result := Result or dxRibbonSkinsManager.Contains(AName, AStyle);
  end;

begin
  if APainter.LookAndFeelStyle = lfsSkin then
  begin
    if not IsSkinAlreadyExists(APainter.LookAndFeelName) then
    begin
      dxRibbonSkinsManager.Add(TdxSkinRibbonPainter.Create(APainter));
      dxRibbonSkinsManager.Add(TdxSkinRibbon2010Painter.Create(APainter));
      dxRibbonSkinsManager.Add(TdxSkinRibbon2013Painter.Create(APainter));
      dxRibbonSkinsManager.Add(TdxSkinRibbon2016Painter.Create(APainter));
      dxRibbonSkinsManager.Add(TdxSkinRibbon2016TabletPainter.Create(APainter));
      dxRibbonSkinsManager.Add(TdxSkinRibbon2019Painter.Create(APainter));
    end;
  end;
end;

procedure TdxSkinsRibbonPainterManager.FreePaintersList;
var
  ASkin: TdxCustomRibbonSkin;
  I: Integer;
begin
  for I := dxRibbonSkinsManager.SkinCount - 1 downto 0 do
  begin
    ASkin := dxRibbonSkinsManager.Skins[I];
    if ASkin is TdxSkinRibbonPainter then
      dxRibbonSkinsManager.Remove(ASkin);
  end;
end;

procedure TdxSkinsRibbonPainterManager.InitializePaintersList;
var
  I: Integer;
begin
  for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
    AddSkin(cxLookAndFeelPaintersManager[I]);
end;

procedure TdxSkinsRibbonPainterManager.PainterAdded(APainter: TcxCustomLookAndFeelPainter);
begin
  AddSkin(APainter);
end;

procedure TdxSkinsRibbonPainterManager.PainterRemoved(APainter: TcxCustomLookAndFeelPainter);
var
  ASkin: TdxCustomRibbonSkin;
  I: Integer;
begin
  for I := dxRibbonSkinsManager.SkinCount - 1 downto 0 do
  begin
    ASkin := dxRibbonSkinsManager.Skins[I];
    if (ASkin is TdxSkinRibbonPainter) and (TdxSkinRibbonPainter(ASkin).Painter = APainter) then
      dxRibbonSkinsManager.Remove(ASkin);
  end;
end;

class procedure TdxSkinsRibbonPainterManager.Register;
begin
  FInstance := TdxSkinsRibbonPainterManager.Create;
end;

class procedure TdxSkinsRibbonPainterManager.Unregister;
begin
  FreeAndNil(FInstance);
end;

{ TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo }

procedure TdxSkinRibbonPainterContextTabGroupsAreaCacheInfo.Check(
  AElement: TdxSkinElement; AContextColor: TColor; const R: TRect; AScaleFactor: TdxScaleFactor);
var
  ABitmap: TcxBitmap32;
begin
  if (AElement <> Element) or (ContextColor <> AContextColor) or (ImageCount <> AElement.ImageCount) or
    (cxRectWidth(R) <> Width) or (cxRectHeight(R) <> Height) then
  begin
    SetSize(R);
    Element := AElement;
    ImageCount := AElement.ImageCount;
    ContextColor := AContextColor;

    FillRectByColor(Canvas.Handle, ClientRect, ContextColor);
    Element.Draw(Canvas.Handle, ClientRect, AScaleFactor);
    if ImageCount > 1 then
    begin
      ABitmap := TcxBitmap32.CreateSize(ClientRect, True);
      try
        Element.Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, AScaleFactor, 1);
        cxMakeColoredBitmap(ABitmap, ContextColor);
        cxAlphaBlend(Canvas.Handle, ABitmap, ClientRect, ABitmap.ClientRect);
      finally
        ABitmap.Free;
      end;
    end;
  end;
end;

initialization
  dxUnitsLoader.AddUnit(@TdxSkinsRibbonPainterManager.Register, @TdxSkinsRibbonPainterManager.Unregister);
finalization
  dxUnitsLoader.RemoveUnit(@TdxSkinsRibbonPainterManager.Unregister);
end.

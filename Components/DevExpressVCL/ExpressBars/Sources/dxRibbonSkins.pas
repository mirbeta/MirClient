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

unit dxRibbonSkins;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, cxGeometry,
  StdCtrls, ImgList, ExtCtrls, dxCore, cxClasses, dxCoreClasses, cxGraphics, dxBarSkin, dxBar,
  dxBarSkinConsts, dxGDIPlusAPI, dxGDIPlusClasses, cxLookAndFeelPainters, dxScreenTip, dxDPIAwareUtils, dxCoreGraphics,
  Generics.Collections, Generics.Defaults;

const
  DXRIBBON_QAT_GROUPBUTTON = 5000;
  DXRIBBON_QAT_SMALLBUTTON = 5001;
  DXRIBBON_QAT_SMALLBUTTON_DROPBUTTON= 5002;
  DXRIBBON_QAT_SMALLBUTTON_GLYPH = 5003;
  DXRIBBON_QAT_MARKARROW = 5004;
  DXRIBBON_QAT_MARKTRUNCATED = 5005;
  DXRIBBON_QAT_ARROWDOWN = 5006;

  DXRIBBON_TAT_SMALLBUTTON = 5100;
  DXRIBBON_TAT_SMALLBUTTON_DROPBUTTON = 5101;
  DXRIBBON_TAT_SMALLBUTTON_GLYPH = 5102;
  DXRIBBON_TAT_MARKARROW = 5103;
  DXRIBBON_TAT_MARKTRUNCATED = 5104;
  DXRIBBON_TAT_ARROWDOWN = 5105;

  //ribbon's form consts
  rfspActiveCaption              = 10000; //don't change order
  rfspInactiveCaption            = 10001;
  rfspActiveCaptionZoomed        = 10002;
  rfspInactiveCaptionZoomed      = 10003;
  rfspActiveCaptionLeftBorder    = 10004;
  rfspInactiveCaptionLeftBorder  = 10005;
  rfspActiveCaptionRightBorder   = 10006;
  rfspInactiveCaptionRightBorder = 10007;
  rfspActiveLeftBorder           = 10008;
  rfspInactiveLeftBorder         = 10009;
  rfspActiveRightBorder          = 10010;
  rfspInactiveRightBorder        = 10011;
  rfspActiveBottomBorderThin     = 10012;
  rfspInactiveBottomBorderThin   = 10013;
  rfspActiveBottomBorderThick    = 10014;
  rfspInactiveBottomBorderThick  = 10015;
  rfspActiveBottomBorderThickRectangular   = 10016;
  rfspInactiveBottomBorderThickRectangular = 10017;
  rfspRibbonForm                 = 10018;

  //ribbon skin consts
  rspTabNormal                   = 10043;
  rspTabHot                      = 10044;
  rspTabActive                   = 10045;
  rspTabActiveHot                = 10046;
  rspTabFocused                  = 10047;
  rspTabGroupsArea               = 10048;
  rspTabGroupsArea2              = 10049;
  rspTabSeparator                = 10050;
  rspTabGroupsAreaInPopup        = 10051;

  rspQATDefaultGlyph             = 10052;
  rspQATAtBottom                 = 10053;
  rspRibbonClientTopArea         = 10054;

  rspQATNonClientLeft1Vista      = 10055;
  rspQATNonClientLeft2Vista      = 10056;
  rspQATNonClientRightVista      = 10057;
  rspQATPopup                    = 10058;

  rspQATNonClientLeft1Active     = 10059;
  rspQATNonClientLeft1Inactive   = 10060;
  rspQATNonClientLeft2Active     = 10061;
  rspQATNonClientLeft2Inactive   = 10062;
  rspQATNonClientRightActive     = 10063;
  rspQATNonClientRightInactive   = 10064;

  rspRibbonBackground            = 10065;
  rspRibbonBottomEdge            = 10066;

  rspApplicationMenuBorder       = 10070;
  rspApplicationMenuContentHeader= 10071;
  rspApplicationMenuContentFooter= 10072;
  rspDropDownBorder              = 10073;
  rspMenuContent                 = 10074;
  rspMenuGlyph                   = 10075;
  rspMenuMark                    = 10076;
  rspMenuSeparatorHorz           = 10077;
  rspMenuSeparatorVert           = 10078;
  rspMenuArrowDown               = 10079;
  rspMenuArrowRight              = 10080;
  rspProgressSolidBand           = 10081;
  rspProgressDiscreteBand        = 10082;
  rspProgressSubstrate           = 10083;
  rspButtonGroupBorderLeft       = 10084;
  rspButtonGroupBorderRight      = 10085;
  rspScrollArrow                 = 10086;
  rspScreenTip                   = 10087;
  rspHelpButton                  = 10088;
  rspApplicationMenuButton       = 10089;

  rspStatusBar                   = 10090;
  rspStatusBarPanel              = 10091;
  rspStatusBarPanelLowered       = 10092;
  rspStatusBarPanelRaised        = 10093;
  rspStatusBarPanelSeparator     = 10094;
  rspStatusBarGripBackground     = 10095;
  rspStatusBarToolbarSeparator   = 10096;
  rspStatusBarSizeGripColor1     = 10098;
  rspStatusBarSizeGripColor2     = 10099;
  rspStatusBarFormLeftPart       = 10100;
  rspStatusBarFormRightPart      = 10104;
  rspStatusBarFormLeftPartDialog = 10108;
  rspStatusBarFormRightPartDialog= 10112;

  rspDropDownGalleryTopSizingBand = 10120;
  rspDropDownGalleryBottomSizingBand = 10121;
  rspDropDownGalleryTopSizeGrip  = 10122;
  rspDropDownGalleryBottomSizeGrip = 10123;
  rspDropDownGalleryVerticalSizeGrip = 10124;
  rspGalleryFilterBand           = 10125;
  rspGalleryGroupHeader          = 10126;

  rspGroupScrollArrow            = 10127;

  //ribbon font colors
  rspFormCaptionText             = 10130;
  rspDocumentNameText            = 10131;
  rspTabHeaderText               = 10132;
  rspTabGroupText                = 10133;
  rspTabGroupHeaderText          = 10134;
  rspStatusBarText               = 10138;
  rspContextText                 = 10139;
  rspContextTextOnGlass          = 10140;
  rspContextTextShadow           = 10141;
  rspContextTextOnGlassShadow    = 10142;

  //context tabs
  rspContextTabNormal            = 10143;
  rspContextTabHot               = 10144;
  rspContextTabActive            = 10145;
  rspContextTabActiveHot         = 10146;
  rspContextTabFocused           = 10147;
  rspContextTabGroupsArea        = 10148;
  rspContextTabGroupsArea2       = 10149;
  rspContextTabSeparatorBegin    = 10150;
  rspContextTabSeparatorEnd      = 10151;
  rspContextBackground           = 10152;
  rspContextBackgroundGlass      = 10153;
  rspContextTabHeaderText        = 10154;
  rspContextTabOverlap           = 10155;

  //border icons
  rfspBorderIconBackground       = 10160; // 3 states
  rfspBorderIconMinimizeGlyph    = rfspBorderIconBackground + 3; // 4 states
  rfspBorderIconMaximizeGlyph    = rfspBorderIconMinimizeGlyph + 4; // 4 states
  rfspBorderIconCloseGlyph       = rfspBorderIconMaximizeGlyph + 4; // 4 states
  rfspBorderIconRestoreGlyph     = rfspBorderIconCloseGlyph + 4; // 4 states
  rfspBorderIconHelpGlyph        = rfspBorderIconRestoreGlyph + 4; // 4 states;
  rfspBorderIconDisplayOptionsGlyph = rfspBorderIconHelpGlyph + 4; // 4 states;
  rfspBorderIconAutoHideShowUIGlyph = rfspBorderIconDisplayOptionsGlyph + 4; // 4 states;

  rspTabGroupBottomOffset        = rfspBorderIconAutoHideShowUIGlyph + 4;
  rspTabGroupInPopupBottomOffset = rspTabGroupBottomOffset + 1;

  //state's groups const
  rspQATGroupButtonActive        = rspTabGroupInPopupBottomOffset + 1;
  rspQATGroupButtonInactive      = rspQATGroupButtonActive + DXBAR_STATESCOUNT;
  rspArrowDownNormal             = rspQATGroupButtonInactive + DXBAR_STATESCOUNT;
  rspMenuDetachCaptionNormal     = rspArrowDownNormal + DXBAR_STATESCOUNT;
  rspMenuCheckNormal             = rspMenuDetachCaptionNormal + DXBAR_STATESCOUNT;
  rspMenuCheckMarkNormal         = rspMenuCheckNormal + DXBAR_STATESCOUNT;
  rspMenuScrollAreaNormal        = rspMenuCheckMarkNormal + DXBAR_STATESCOUNT;

  rspCollapsedToolbarNormal = rspMenuScrollAreaNormal + DXBAR_STATESCOUNT;
  rspCollapsedToolbarGlyphBackgroundNormal = rspCollapsedToolbarNormal + DXBAR_STATESCOUNT;

  rspEditButtonNormal            = rspCollapsedToolbarGlyphBackgroundNormal + DXBAR_STATESCOUNT;

  rspSmallButtonNormal           = rspEditButtonNormal + DXBAR_STATESCOUNT;
  rspSmallButtonGlyphBackgroundNormal = rspSmallButtonNormal + DXBAR_STATESCOUNT;
  rspSmallButtonDropButtonNormal = rspSmallButtonGlyphBackgroundNormal + DXBAR_STATESCOUNT;

  rspLargeButtonNormal           = rspSmallButtonDropButtonNormal + DXBAR_STATESCOUNT;
  rspLargeButtonGlyphBackgroundNormal = rspLargeButtonNormal + DXBAR_STATESCOUNT;
  rspLargeButtonDropButtonNormal = rspLargeButtonGlyphBackgroundNormal + DXBAR_STATESCOUNT;

  rspButtonGroupNormal           = rspLargeButtonDropButtonNormal + DXBAR_STATESCOUNT;
  rspButtonGroupBorderMiddleNormal = rspButtonGroupNormal + DXBAR_STATESCOUNT;
  rspButtonGroupSplitButtonSeparatorNormal = rspButtonGroupBorderMiddleNormal + DXBAR_STATESCOUNT;

  rspToolbarNormal               = rspButtonGroupSplitButtonSeparatorNormal + DXBAR_STATESCOUNT;
  rspToolbarHeaderNormal         = rspToolbarNormal + DXBAR_STATESCOUNT;

  rspMarkArrowNormal             = rspToolbarHeaderNormal + DXBAR_STATESCOUNT;
  rspMarkTruncatedNormal         = rspMarkArrowNormal + DXBAR_STATESCOUNT;
  rspLaunchButtonBackgroundNormal= rspMarkTruncatedNormal + DXBAR_STATESCOUNT;
  rspLaunchButtonDefaultGlyphNormal = rspLaunchButtonBackgroundNormal + DXBAR_STATESCOUNT;

  rspTabScrollLeftButtonNormal   = rspLaunchButtonDefaultGlyphNormal + DXBAR_STATESCOUNT;
  rspTabScrollRightButtonNormal  = rspTabScrollLeftButtonNormal + DXBAR_STATESCOUNT;
  rspGroupScrollLeftButtonNormal = rspTabScrollRightButtonNormal + DXBAR_STATESCOUNT;
  rspGroupScrollRightButtonNormal= rspGroupScrollLeftButtonNormal + DXBAR_STATESCOUNT;

  rspInRibbonGalleryScrollBarLineUpButtonNormal = rspGroupScrollRightButtonNormal + DXBAR_STATESCOUNT;
  rspInRibbonGalleryScrollBarLineDownButtonNormal = rspInRibbonGalleryScrollBarLineUpButtonNormal + DXBAR_STATESCOUNT;
  rspInRibbonGalleryScrollBarDropDownButtonNormal = rspInRibbonGalleryScrollBarLineDownButtonNormal + DXBAR_STATESCOUNT;

  // For Ribbon 2010
  rfspCloseButtonHot               = rspInRibbonGalleryScrollBarDropDownButtonNormal + DXBAR_STATESCOUNT;
  rfspCloseButtonPressed           = rfspCloseButtonHot + 1;
  rfspCloseButtonInactiveHot       = rfspCloseButtonPressed + 1;
  rspMinimizeRibbonButtonMinimize  = rfspCloseButtonInactiveHot + 1; // 2 states
  rspMinimizeRibbonButtonRestore   = rspMinimizeRibbonButtonMinimize + 2;
  rspMinimizeRibbonButtonPin       = rspMinimizeRibbonButtonRestore + 2;
  rspItemSeparatorHorizontal       = rspMinimizeRibbonButtonPin + 2;
  rspItemSeparatorVertical         = rspItemSeparatorHorizontal + 1;
  rspMDIButtonMinimize             = rspItemSeparatorVertical + 1;
  rspMDIButtonRestore              = rspMDIButtonMinimize + 4;
  rspMDIButtonClose                = rspMDIButtonRestore + 4;
  rspTabsAreaOnGlass               = rspMDIButtonClose + 4;

  rspContextBackgroundMask         = rspTabsAreaOnGlass + 1;
  rspContextTabMaskNormal          = rspContextBackgroundMask + 1;
  rspContextTabMaskHot             = rspContextTabMaskNormal + 1;
  rspContextTabMaskActive          = rspContextTabMaskHot + 1;
  rspContextTabMaskActiveHot       = rspContextTabMaskActive + 1;
  rspContextTabMaskFocused         = rspContextTabMaskActiveHot + 1;

  // Ribbon BackstageView consts
  rbvpBackstageView               = rspContextTabMaskFocused + 1;
  rbvpBackstageViewBackButton     = rbvpBackstageView + 1; // 1 glyph + 1 mask
  rbvpBackstageViewFrame          = rbvpBackstageViewBackButton + 2; // 5 color schemes
  rbvpBackstageViewMenu           = rbvpBackstageViewFrame + 5;
  rbvpBackstageViewMenuHeader     = rbvpBackstageViewMenu + 1;
  rbvpBackstageViewMenuItem       = rbvpBackstageViewMenuHeader + 1; // 2 states * 5 color schemes
  rbvpBackstageViewMenuTabButton  = rbvpBackstageViewMenuItem + 10; // 4 states * 5 color schemes
  rbvpBackstageViewMenuTabButtonArrow  = rbvpBackstageViewMenuTabButton + 20;
  rbvpBackstageViewMenuSeparator  = rbvpBackstageViewMenuTabButtonArrow + 1;

  // PinButton
  rspPinButtonGlyph = rbvpBackstageViewMenuSeparator + 1; // 2 states

  // Application Button
  rspApplicationButton = rspPinButtonGlyph + 2; // 3 states * 5 color schemes

  // ScrollBar
  rspScrollBarHorz = rspApplicationButton + 15;
  rspScrollBarHorzThumb = rspScrollBarHorz + 4;
  rspScrollBarHorzThumbGlyph = rspScrollBarHorzThumb + 4;

  rspScrollBarVert = rspScrollBarHorzThumbGlyph + 1;
  rspScrollBarVertThumb = rspScrollBarVert + 4;
  rspScrollBarVertThumbGlyph = rspScrollBarVertThumb + 4;

  rspScrollBarButtonBottom = rspScrollBarVertThumbGlyph + 1;
  rspScrollBarButtonBottomGlyph = rspScrollBarButtonBottom + 4;
  rspScrollBarButtonTop = rspScrollBarButtonBottomGlyph + 1;
  rspScrollBarButtonTopGlyph = rspScrollBarButtonTop + 4;
  rspScrollBarButtonLeft = rspScrollBarButtonTopGlyph + 1;
  rspScrollBarButtonLeftGlyph = rspScrollBarButtonLeft + 4;
  rspScrollBarButtonRight = rspScrollBarButtonLeftGlyph + 1;
  rspScrollBarButtonRightGlyph = rspScrollBarButtonRight + 4;

  // TabAreaToolbar
  rtatpEditBackground = rspScrollBarButtonRightGlyph + 1;
  rtatpEditText = rtatpEditBackground + 1;

  //next = rtatpEditText + 1;

type
  TdxInRibbonGalleryScrollBarButtonKind = (gsbkLineUp, gsbkLineDown, gsbkDropDown);

  TdxRibbonApplicationButtonState = (rabsNormal, rabsHot, rabsPressed);
  TdxRibbonApplicationMenuState = (ramsHidden, ramsShownAsMenu, ramsShownAsFrame, ramsShownAsFullScreenFrame);
  TdxRibbonColorSchemeAccent = (rcsaYellow, rcsaBlue, rcsaGreen, rcsaOrange, rcsaPurple);
  TdxRibbonMinimizeButtonGlyph = (rmbMinimize, rmbRestore, rmbPin);
  TdxRibbonStyle = (rs2007, rs2010, rs2013, rs2016, rs2016Tablet, rs2019);
  TdxRibbonTabState = (rtsNormal, rtsHot, rtsActive, rtsActiveHot, rtsFocused);

  TdxRibbonBorderIcon = (rbiSystemMenu, rbiMinimize, rbiMaximize, rbiHelp, rbiDisplayOptions, rbiAutoHideModeShowUI);
  TdxRibbonBorderIcons = set of TdxRibbonBorderIcon;
  TdxRibbonBorderIconState = (rbisNormal, rbisHot, rbisPressed, rbisInactive, rbisHotInactive);
  TdxRibbonBorderDrawIcon = (rbdiMinimize, rbdiMaximize, rbdiRestore, rbdiClose,
    rbdiHelp, rbdiDisplayOptions, rbdiAutoHideModeShowUI);

  { IdxRibbonFormPaintData }

  IdxRibbonFormPaintData = interface
  ['{79EB28B8-227E-4C23-BC59-7CF4311789FC}']
    function DontUseAero: Boolean;
    function GetBounds: TRect;
    function GetFormBorderStyle: TBorderStyle;
    function GetHandle: HWND;
    function GetIsActive: Boolean;
    function GetState: TWindowState;
    function GetStyle: TFormStyle;
    function UseRoundedWindowCorners: Boolean;
  end;

  { IdxRibbonPaintData }

  IdxRibbonPaintData = interface
  ['{C73027DB-CAE5-43CD-840C-225C2A9B520A}']
    function GetApplicationMenuState: TdxRibbonApplicationMenuState;
    function GetCaptionAreaExtension: Integer;
    function GetCaptionHeight: Integer;
    function GetColorSchemeAccent: TdxRibbonColorSchemeAccent;
    function GetRibbonFormPaintData: IdxRibbonFormPaintData;
    function GetRibbonHeight: Integer;
    function GetTabsHeight: Integer;
    function HasStatusBar: Boolean;
    function IsQuickAccessToolbarBelowRibbon: Boolean;
    function IsRibbonHidden: Boolean;
  end;

  TTwoStateArray = array[Boolean] of Integer;
  TThreeStateArray = array[0..2] of Integer;
  TFourStateArray = array[0..3] of Integer;
  TStatesArray = array[0..DXBAR_STATESCOUNT - 1] of Integer;

  { IdxRibbonSkinColorPaletteSet }

  IdxRibbonSkinColorPaletteSet = interface
  ['{6172F986-2682-4C8C-9DA3-E72766FE0D79}']
    function Add(AState: Integer; APalette: IdxColorPalette): IdxRibbonSkinColorPaletteSet;
    function Get(AState: Integer): IdxColorPalette;
  end;

  { TdxRibbonSkinColorPalette }

  TdxRibbonSkinColorPalette = class(TdxAdvancedColorPalette)
  public
    constructor Create(ARed, AGreen, AWhite, ABlack, AYellow, ABlue: TColor);
  end;

  { TdxRibbonSkinColorPaletteSet }

  TdxRibbonSkinColorPaletteSet = class(TInterfacedObject, IdxRibbonSkinColorPaletteSet)
  strict private
    FData: array[0..DXBAR_STATESCOUNT - 1] of IdxColorPalette;
  public
    // IdxRibbonSkinColorPaletteSet
    function Add(AState: Integer; APalette: IdxColorPalette): IdxRibbonSkinColorPaletteSet;
    function Get(AState: Integer): IdxColorPalette;
  end;

  { TdxCustomRibbonSkin }

  TdxCustomRibbonSkinClass = class of TdxCustomRibbonSkin;
  TdxCustomRibbonSkin = class(TdxCustomBarSkin)
  protected const
    BorderIconStateToPartIndex: array[TdxRibbonBorderIconState] of Integer = (-1, 0, 1, -1, 2);
    DefaultFixedSize: TRect = (Left: 2; Top: 2; Right: 2; Bottom: 2);
  strict private
    FColorSchemeAccent: TdxRibbonColorSchemeAccent;
    FPaintData: IdxRibbonPaintData;
    FReferenceCount: Integer;
    FScaleFactor: TdxScaleFactor;
    FScaleFactorForTextures: TdxScaleFactor;
    FTargetDPI: Integer;
    FUseRightToLeftAlignment: Boolean;

    function GetColorPalette(ASet: IdxRibbonSkinColorPaletteSet; AState: Integer): IdxColorPalette;
    function GetFormPaintData: IdxRibbonFormPaintData;
  protected
    FLowColors: Boolean;

    FApplicationMenuBorder: Integer;
    FApplicationMenuButton: Integer;
    FApplicationMenuContentFooter: Integer;
    FApplicationMenuContentHeader: Integer;
    FButtonGroup: TStatesArray;
    FColorPaletteBackstageViewMenu: IdxRibbonSkinColorPaletteSet;
    FColorPaletteMenu: IdxRibbonSkinColorPaletteSet;
    FColorPaletteMiniToolbar: IdxRibbonSkinColorPaletteSet;
    FColorPaletteQAT: IdxRibbonSkinColorPaletteSet;
    FColorPaletteRadialMenu: IdxRibbonSkinColorPaletteSet;
    FColorPaletteStatusBar: IdxRibbonSkinColorPaletteSet;
    FColorPaletteTabAreaToolbar: IdxRibbonSkinColorPaletteSet;
    FColorPaletteTabGroup: IdxRibbonSkinColorPaletteSet;
    FDropDownBorder: Integer;
    FDropDownGalleryBottomSizeGrip: Integer;
    FDropDownGalleryTopSizeGrip: Integer;
    FDropDownGalleryVerticalSizeGrip: Integer;
    FEditButtons: TStatesArray;
    FHelpButton: Integer;
    FLaunchButtonDefaultGlyphs: TStatesArray;
    FMarkTruncated: TStatesArray;
    FMenuContent: Integer;
    FMenuGlyph: Integer;
    FMenuMark: Integer;
    FMenuSeparatorHorz: Integer;
    FMenuSeparatorVert: Integer;
    FPinButtonGlyphs: TTwoStateArray;
    FProgressSubstrate: Integer;
    FQATAtBottom: Integer;
    FQATAtTopLeft: array[Boolean] of TTwoStateArray;
    FQATAtTopRight: TTwoStateArray;
    FQATGlassAtTopLeft: array[Boolean] of Integer;
    FQATGlassAtTopRight: Integer;
    FQATGroupButtonActive: TStatesArray;
    FQATGroupButtonInactive: TStatesArray;
    FScreenTip: Integer;
    FScrollArrow: Integer;
    FTabSeparator: Integer;

    procedure AddTwoStateElement(ABitmap: GpBitmap; var AParts; const R, F: TRect;
      ID: Integer; AInterpolationMode: Integer = InterpolationModeNearestNeighbor);
    procedure InternalDrawFormBorders(DC: HDC; const R: TRect; const AIsActive, AIsRectangular: Boolean);
    procedure InternalDrawGlyph(DC: HDC; const R: TRect; APart: Integer;
      AColor: TColor = clDefault; ANeedCorrection: Boolean = True; AAlignment: TAlignment = taCenter);
    procedure InternalDrawSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean; AColor1, AColor2: TColor);
    // Palettes
    procedure InitializeColorPalettes; virtual;
    // Textures
    procedure LoadCommonTexturesSet(AImage: TdxGPImage); virtual;
    procedure LoadRibbonTexturesSet(AImage: TdxGPImage); virtual; abstract;
    procedure LoadSkin;
    // Textures - Common
    procedure LoadCommonElements(ABitmap: GpBitmap); virtual;
    procedure LoadCommonHelpButton(ABitmap: GpBitmap); virtual;
    procedure LoadCommonMenu(ABitmap: GpBitmap); virtual;
    procedure LoadCommonPinButton(ABitmap: GpBitmap); virtual;
    // Textures - Ribbon
    procedure LoadRibbonButtons(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonElements(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonForm(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonGallery(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonLaunchButton(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonMenu(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonMenuMarks(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonQAT(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonQATBorders(ABitmap: GpBitmap); virtual;
    procedure LoadRibbonTab(ABitmap: GpBitmap); virtual;
    // Info
    function GetStyle: TdxRibbonStyle; virtual;
    function GetTexturesDPI: Integer; virtual;
    //
    procedure CalculateArrowPoints(R: TRect; var P: TcxArrowPoints; AArrowDirection: TcxArrowDirection);
    procedure DrawArrow(DC: HDC; const R: TRect; AArrowDirection: TcxArrowDirection; AColor: TColor);
    procedure DrawDropDownGalleryVerticalSizeGrip(DC: HDC; const R: TRect);
    procedure DrawPart(DC: HDC; const R: TRect; AState: Integer); overload;
    procedure DrawPart(const AParts: TStatesArray; DC: HDC; const R: TRect; AState: Integer); overload;
    procedure GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor); virtual;
    function GetArrowSize: Integer;

    function DoGetPartColor(APart: Integer; AState: Integer = 0): TColor; virtual;
    function DoGetPartColorCore(APart: Integer; AState: Integer = 0): TColor;
    function DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor; virtual;

    property ColorSchemeAccent: TdxRibbonColorSchemeAccent read FColorSchemeAccent;
    property FormPaintData: IdxRibbonFormPaintData read GetFormPaintData;
    property LowColors: Boolean read FLowColors write FLowColors;
    property PaintData: IdxRibbonPaintData read FPaintData;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property ScaleFactorForTextures: TdxScaleFactor read FScaleFactorForTextures;
  public
    constructor Create(ATargetDPI: Integer = dxDefaultDPI);
    destructor Destroy; override;
    procedure AddReference;
    procedure RemoveReference;
    function Clone(ATargetDPI: Integer): TdxCustomRibbonSkin; virtual;
    procedure InitializePaintData(const APaintData: IdxRibbonPaintData); virtual;

    // Load
    procedure LoadBitmapFromStream(const AResName: string; AImage: TdxGPImage; AInstance: THandle = 0); virtual;
    procedure LoadElementParts(ABitmap: GpBitmap; var AParts; const R: TRect; AID: Integer; const AFixedSize: TRect;
      const AImageIndexes: array of Byte; const APossibleStates: TdxByteSet; AIsTopDown: Boolean = True;
      AInterpolationMode: Integer = InterpolationModeDefault);
    procedure LoadElementPartsFromFile(const AFileName: string; var AParts; AID: Integer; const AFixedSize: TRect;
      const AImageIndexes: array of Byte; const APossibleStates: TdxByteSet);
    procedure LoadFourStateArray(ABitmap: GpBitmap; R: TRect; const Fixed: TRect; var AStateArray: TFourStateArray;
      AStartID: Integer; AIsVerticalLayout: Boolean = True; AInterpolationMode: Integer = InterpolationModeDefault);
    procedure LoadThreeStateArray(ABitmap: GpBitmap; R: TRect; const Fixed: TRect; var AStateArray: TThreeStateArray;
      AStartID: Integer; AIsVerticalLayout: Boolean = True; AInterpolationMode: Integer = InterpolationModeDefault);

    // Application Button
    procedure AdjustApplicationButtonFont(AFont: TFont; AState: TdxRibbonApplicationButtonState); virtual;
    procedure DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState); virtual;
    procedure DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect); virtual;
    procedure DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawApplicationMenuExtraPaneButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawApplicationMenuExtraPanePinButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawApplicationMenuExtraPanePinButtonGlyph(DC: HDC; const R: TRect; AState: Integer; AChecked: Boolean); virtual;
    function GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect; virtual;
    function GetApplicationMenuGlyphSize: TSize; virtual;
    function GetApplicationMenuTextColor(AState: TdxRibbonApplicationButtonState): TColor; virtual;

    // Button Group
    procedure DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawButtonGroupBorderLeft(DC: HDC; const R: TRect); virtual;
    procedure DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawButtonGroupBorderRight(DC: HDC; const R: TRect); virtual;
    procedure DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer); virtual;

    // CollapsedToolbar
    procedure DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer); virtual;

    // DropDownGallery
    procedure DrawDropDownGalleryBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryBottomSizeGrip(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryBottomVerticalSizeGrip(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryTopSizeGrip(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect); virtual;
    procedure DrawDropDownGalleryTopVerticalSizeGrip(DC: HDC; const R: TRect); virtual;

    // Edit
    procedure DrawEditArrowButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawEditButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawEditEllipsisButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawEditSpinDownButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawEditSpinUpButton(DC: HDC; const R: TRect; AState: Integer); virtual;

    // Form
    function AdjustCaptionFontSize(ASize: Integer; AUseAeroGlass: Boolean): Integer; virtual;
    procedure AdjustRibbonFormBorderIconSize(AIcon: TdxRibbonBorderDrawIcon;
      AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize); virtual;
    function ExtendCaptionAreaOnTabs: Boolean; virtual;
    procedure DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); virtual;
    procedure DrawFormBorders(DC: HDC; const ABordersWidth: TRect); virtual;
    procedure DrawFormCaption(DC: HDC; const R: TRect); virtual;
    procedure DrawFormStatusBarPart(DC: HDC; const R: TRect; AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean); virtual;
    function GetWindowBordersWidth(AHasStatusBar: Boolean): TRect; virtual;
    function HasExternalRibbonFormShadow: Boolean; virtual;
    function UseRoundedWindowCorners: Boolean; virtual;

    // HelpButton
    procedure DrawHelpButton(DC: HDC; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawHelpButtonGlyph(DC: HDC; const R: TRect); virtual;

    // InRibbonGallery
    procedure DrawInRibbonGalleryBackground(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawInRibbonGalleryScrollBarBackground(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawInRibbonGalleryScrollBarButton(DC: HDC; const R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); virtual;
    procedure DrawInRibbonGalleryScrollBarButtonGlyph(DC: HDC; R: TRect;
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer); virtual;
    function GetInRibbonGalleryScrollBarButtonGlyphColor(
      AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor; virtual;
    procedure DrawInRibbonGalleryScrollBarDropDownTouchButton(DC: HDC; const R: TRect; AState: Integer); virtual;

    // LargeButton
    procedure DrawLargeButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;

    // LaunchButton
    procedure DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer); virtual;

    // MDI Button
    procedure DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); virtual;
    procedure DrawMDIButtonGlyph(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); virtual;

    // Menu
    procedure DrawMenuArrowDown(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuArrowRight(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuBackButton(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuContent(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuDetachCaption(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuExtraSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); virtual;
    procedure DrawMenuGlyph(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuItem(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuItemDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuItemDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuMark(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMenuSeparatorHorz(DC: HDC; const R: TRect); virtual;
    procedure DrawMenuSeparatorVert(DC: HDC; const R: TRect); virtual;
    function GetMenuColorPalette(AState: Integer): IdxColorPalette; virtual;
    function GetMenuSeparatorSize: Integer; virtual;

    // Minimize Button
    procedure DrawMinimizeRibbonButton(DC: HDC; const R: TRect; AState: TcxButtonState; AMinimized: Boolean); virtual;
    procedure DrawMinimizeRibbonButtonGlyph(DC: HDC; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph); virtual;

    // Progress
    procedure DrawProgressDiscreteBand(DC: HDC; const R: TRect); virtual;
    procedure DrawProgressSolidBand(DC: HDC; const R: TRect); virtual;
    procedure DrawProgressSubstrate(DC: HDC; const R: TRect); virtual;

    // SmallButton
    procedure DrawSmallButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    function GetSmallButtonColorPalette(AState: Integer): IdxColorPalette; virtual;

    // BackstageView
    procedure AdjustBackstageViewTabButtonFont(AFont: TFont); virtual;
    function CanShowTabAreaToolbarInBackstageView: Boolean; virtual;
    procedure DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawBackstageViewMenuBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewMenuHeader(DC: HDC; const R: TRect); virtual;
    procedure DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer); virtual;
    function GetBackstageViewMenuButtonColorPalette(AState: Integer): IdxColorPalette; virtual;

    // BackstageViewGalleryControl
    procedure DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect); virtual;
    procedure DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewGalleryItemPinButtonGlyph(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewGalleryItemPinTag(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect); virtual;

    // Marks
    procedure DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer); virtual;

    // Tab
    procedure AdjustTabFont(AFont: TFont; AState: Integer); virtual;
    procedure DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState); virtual;
    procedure DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
      AApplicationMenuState: TdxRibbonApplicationMenuState); virtual;
    procedure DrawTabBase(DC: HDC; const R: TRect); virtual;
    procedure DrawTabGroupBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); virtual;
    procedure DrawTabGroupHeaderBackground(DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean); virtual;
    procedure DrawTabGroupsArea(DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean); virtual;
    procedure DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); virtual;
    procedure DrawTabScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean); virtual;
    procedure DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte); virtual;

    // TAT SmallButton
    procedure DrawTabAreaArrowDown(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawTabAreaButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawTabAreaButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawTabAreaMarkArrow(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawTabAreaMarkTruncated(DC: HDC; const R: TRect; AState: Integer); virtual;
    function GetTabAreaButtonColorPalette(AState: Integer): IdxColorPalette; virtual;

    // Context
    procedure AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor); virtual;
    procedure AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor); virtual;
    procedure DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor); virtual;
    procedure DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor); virtual;
    procedure DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState; AContextColor: TColor); virtual;
    procedure DrawContextTabGroupsArea(DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean); virtual;
    procedure DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean); virtual;

    // QAT
    procedure AdjustQuickAccessToolbarVertical(var ABounds: TRect; ANonClientDraw, ADontUseAero: Boolean); virtual;
    procedure DrawQuickAccessToolbar(DC: HDC; const R: TRect;
      ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean); virtual;
    procedure DrawQuickAccessToolbarArrowDown(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); virtual;
    procedure DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect); virtual;
    procedure DrawQuickAccessToolbarGroupButton(DC: HDC; const R: TRect; ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer); virtual;
    procedure DrawQuickAccessToolbarMarkArrow(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); virtual;
    procedure DrawQuickAccessToolbarMarkTruncated(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean); virtual;
    procedure DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect); virtual;
    procedure DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer); virtual;
    function GetQuickAccessToolbarColorPalette(AState: Integer; ABelow: Boolean): IdxColorPalette; virtual;
    function GetQuickAccessToolbarLeftIndent(AHasApplicationButton, AUseAeroGlass: Boolean): Integer; virtual;
    function GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton, ABelow: Boolean): Integer; virtual;
    function GetQuickAccessToolbarOverrideWidth(AHasApplicationButton, AUseAeroGlass: Boolean): Integer; virtual;
    function GetQuickAccessToolbarRightIndent(AHasApplicationButton: Boolean): Integer; virtual;

    // StatusBar
    procedure DrawStatusBar(DC: HDC; const R: TRect); virtual;
    procedure DrawStatusBarGripBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean); virtual;
    procedure DrawStatusBarPanelSeparator(DC: HDC; const R: TRect); virtual;
    procedure DrawStatusBarSizeGrip(DC: HDC; const R: TRect); virtual;
    procedure DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect); virtual;
    function GetStatusBarPanelColorPalette(AState: Integer): IdxColorPalette; virtual;
    function GetStatusBarSeparatorSize: Integer; virtual;

    // ScrollBars
    procedure DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean); virtual;
    procedure DrawScrollBarPart(DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer; AHorizontal: Boolean); virtual;
    procedure DrawScrollBoxSizeGripArea(DC: HDC; const R: TRect); virtual;
    function GetScrollBarPainter: TcxCustomLookAndFeelPainter; virtual;

    // GroupScroll
    procedure DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer); virtual;
    procedure DrawGroupScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean); virtual;

    // MiniToolbar
    procedure DrawMiniToolbarBackground(DC: HDC; const R: TRect); virtual;
    function GetMiniToolbarColorPalette(AState: Integer): IdxColorPalette; virtual;

    // RadialMenu
    function GetRadialMenuColorPalette: IdxColorPalette; virtual;

    procedure DrawArrowDown(DC: HDC; const R: TRect; AState: Integer); virtual;
    procedure DrawDropDownBorder(DC: HDC; const R: TRect); virtual;
    procedure DrawGalleryFilterBandBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer); virtual;
    procedure DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean); virtual;
    procedure DrawKeyTip(DC: HDC; const R: TRect); virtual;
    procedure DrawRibbonBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawRibbonClientTopArea(DC: HDC; const R: TRect); virtual;
    procedure DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer); virtual;
    procedure DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean); virtual;
    procedure DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect); virtual;
    function GetRibbonTopFrameAreaSeparatorSize: Integer; virtual;
    procedure DrawScreenTip(DC: HDC; const R: TRect); virtual;
    procedure DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0); virtual;
    procedure DrawSeparatorBackground(DC: HDC; const R: TRect); virtual;
    procedure DrawSeparatorLine(DC: HDC; const R: TRect); virtual;

    function GetIsAlphaUsed(APart: Integer): Boolean; virtual;
    function GetPartColor(APart: Integer; AState: Integer = 0): TColor;
    function GetPartContentOffsets(APart: Integer): TRect; virtual;
    function GetPartSize(APart: Integer): Integer; virtual;
    function GetSkinName: string; virtual;
    function IsInternalPainter: Boolean; virtual;
    procedure FlushCache; virtual;
    procedure UpdateBitsPerPixel; virtual;
    //
    property Style: TdxRibbonStyle read GetStyle;
    property TargetDPI: Integer read FTargetDPI;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment write FUseRightToLeftAlignment;
  end;

  { TdxRibbonSkinsManager }

  TdxRibbonSkinsManager = class
  strict private
    FDefault: TdxCustomRibbonSkin;
    FList: TcxObjectList;

    function GetDefault: TdxCustomRibbonSkin;
    function GetSkin(Index: Integer): TdxCustomRibbonSkin;
    function GetSkinCount: Integer;
    procedure ReleaseDefault;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ASkin: TdxCustomRibbonSkin): Integer;
    function Contains(const AName: string; AStyle: TdxRibbonStyle): Boolean;
    function GetMostSuitable(const AName: string; AStyle: TdxRibbonStyle; ATargetDPI: Integer): TdxCustomRibbonSkin;
    function Remove(ASkin: TdxCustomRibbonSkin): Boolean; overload;
    function Remove(ASkinClass: TdxCustomRibbonSkinClass): Boolean; overload;
    //
    property Default: TdxCustomRibbonSkin read GetDefault;
    property SkinCount: Integer read GetSkinCount;
    property Skins[Index: Integer]: TdxCustomRibbonSkin read GetSkin; default;
  end;

const
  dxRibbonColorSchemeAccentNames: array[TdxRibbonColorSchemeAccent] of string = (
    'Yellow', 'Blue', 'Green', 'Orange', 'Purple'
  );

var
  dxRibbonUseLowColorsInHighContrastThemes: Boolean = True;

function dxRibbonSkinsManager: TdxRibbonSkinsManager;

procedure DrawFrame(DC: HDC; const R: TRect; AColor, ABorderColor: TColor;
  const ABorders: TcxBorders = cxBordersAll; ABorderWidth: Integer = 1; AIsPaintOnGlass: Boolean = False);
function ButtonStateToRibbonState(AState: TcxButtonState): Integer;
function RibbonStateToButtonState(AState: Integer): TcxButtonState;
function IsRectangularFormBottom(AFormData: IdxRibbonFormPaintData): Boolean; inline;
implementation

uses
  Types, dxOffice11, Math, cxDWMApi, cxLookAndFeels, cxControls;

var
  FSkinsManager: TdxRibbonSkinsManager;

function dxRibbonSkinsManager: TdxRibbonSkinsManager;
begin
  if FSkinsManager = nil then
    FSkinsManager := TdxRibbonSkinsManager.Create;
  Result := FSkinsManager;
end;

function IsRectangularFormBottom(AFormData: IdxRibbonFormPaintData): Boolean;
begin
  Result := (AFormData.GetFormBorderStyle in [bsDialog, bsSingle, bsToolWindow]) or
    (AFormData.GetStyle = fsMDIChild) or (AFormData.GetState = wsMinimized) or not AFormData.UseRoundedWindowCorners;
end;

procedure DrawFrame(DC: HDC; const R: TRect; AColor, ABorderColor: TColor;
  const ABorders: TcxBorders = cxBordersAll; ABorderWidth: Integer = 1; AIsPaintOnGlass: Boolean = False);

  function GetBorderBounds(ABorder: TcxBorder; var ABounds: TRect): TRect;
  begin
    Result := R;
    case ABorder of
      bLeft:
        begin
          Result.Right := Result.Left + ABorderWidth;
          Inc(ABounds.Left, ABorderWidth);
        end;
      bTop:
        begin
          Result.Bottom := Result.Top + ABorderWidth;
          Inc(ABounds.Top, ABorderWidth);
        end;
      bRight:
        begin
          Result.Left := Result.Right - ABorderWidth;
          Dec(ABounds.Right, ABorderWidth);
        end;
      bBottom:
        begin
          Result.Top := Result.Bottom - ABorderWidth;
          Dec(ABounds.Bottom, ABorderWidth);
        end;
    end;
  end;

  procedure DoFillRect(const R: TRect; AColor: TColor);
  begin
    if cxColorIsValid(AColor) and not cxRectIsEmpty(R) then
    begin
      if AIsPaintOnGlass then
        dxGpFillRect(DC, R, AColor)
      else
        FillRectByColor(DC, R, AColor);
    end;
  end;

var
  ABorder: TcxBorder;
  ABounds: TRect;
begin
  if not cxRectIsEmpty(R) then
  begin
    ABounds := R;
    if (ABorders <> []) and (ABorderColor <> clNone) then
    begin
      for ABorder := Low(ABorder) to High(ABorder) do
      begin
        if ABorder in ABorders then
          DoFillRect(GetBorderBounds(ABorder, ABounds), ABorderColor);
      end;
    end;
    DoFillRect(ABounds, AColor);
  end;
end;

function ButtonStateToRibbonState(AState: TcxButtonState): Integer;
begin
  case AState of
    cxbsDisabled:
      Result := DXBAR_DISABLED;
    cxbsHot:
      Result := DXBAR_HOT;
    cxbsPressed:
      Result := DXBAR_PRESSED;
    else
      Result := DXBAR_NORMAL;
  end;
end;

function RibbonStateToButtonState(AState: Integer): TcxButtonState;
begin
  case AState of
    DXBAR_DISABLED:
      Result := cxbsDisabled;
    DXBAR_HOT:
      Result := cxbsHot;
    DXBAR_PRESSED:
      Result := cxbsPressed;
    else
      Result := cxbsNormal;
  end;
end;

{ TdxRibbonSkinColorPalette }

constructor TdxRibbonSkinColorPalette.Create(ARed, AGreen, AWhite, ABlack, AYellow, ABlue: TColor);
begin
  inherited Create;
  FillColors['Black'] := dxColorToAlphaColor(ABlack);
  FillColors['Blue'] := dxColorToAlphaColor(ABlue);
  FillColors['Green'] := dxColorToAlphaColor(AGreen);
  FillColors['Red'] := dxColorToAlphaColor(ARed);
  FillColors['White'] := dxColorToAlphaColor(AWhite);
  FillColors['Yellow'] := dxColorToAlphaColor(AYellow);
end;

{ TdxRibbonSkinColorPaletteSet }

function TdxRibbonSkinColorPaletteSet.Add(AState: Integer; APalette: IdxColorPalette): IdxRibbonSkinColorPaletteSet;
begin
  Result := Self;
  FData[AState] := APalette;
end;

function TdxRibbonSkinColorPaletteSet.Get(AState: Integer): IdxColorPalette;
begin
  Result := FData[AState];
  if Result = nil then
    case AState of
      DXBAR_NORMAL:
        Result := nil;
      DXBAR_ACTIVEDISABLED:
        Result := Get(DXBAR_DISABLED);
      DXBAR_HOTCHECK:
        Result := Get(DXBAR_CHECKED);
    else
      Result := Get(DXBAR_NORMAL);
    end;
end;

{ TdxCustomRibbonSkin }

constructor TdxCustomRibbonSkin.Create(ATargetDPI: Integer = dxDefaultDPI);
begin
  inherited Create;
  FTargetDPI := ATargetDPI;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(TargetDPI, dxDefaultDPI);
  FScaleFactorForTextures := TdxScaleFactor.Create;
  FScaleFactorForTextures.Assign(TargetDPI, GetTexturesDPI);
  UpdateBitsPerPixel;
end;

destructor TdxCustomRibbonSkin.Destroy;
begin
  FreeAndNil(FScaleFactorForTextures);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TdxCustomRibbonSkin.AddReference;
begin
  Inc(FReferenceCount);
  if FReferenceCount = 1 then
    LoadSkin;
end;

procedure TdxCustomRibbonSkin.RemoveReference;
begin
  Dec(FReferenceCount);
  if FReferenceCount = 0 then
    Clear;
end;

function TdxCustomRibbonSkin.Clone(ATargetDPI: Integer): TdxCustomRibbonSkin;
begin
  Result := TdxCustomRibbonSkinClass(ClassType).Create(ATargetDPI);
end;

procedure TdxCustomRibbonSkin.InitializePaintData(const APaintData: IdxRibbonPaintData);
begin
  FPaintData := APaintData;
  if PaintData <> nil then
    FColorSchemeAccent := PaintData.GetColorSchemeAccent
  else
    FColorSchemeAccent := rcsaYellow;
end;

procedure TdxCustomRibbonSkin.LoadBitmapFromStream(const AResName: string; AImage: TdxGPImage; AInstance: THandle = 0);
var
  AResStream: TStream;
begin
  if AInstance = 0 then
    AInstance := HInstance;

  AResStream := TResourceStream.Create(AInstance, AResName, RT_RCDATA);
  try
    AImage.LoadFromStream(AResStream);
  finally
    AResStream.Free;
  end;
end;

procedure TdxCustomRibbonSkin.LoadElementParts(ABitmap: GpBitmap;
  var AParts; const R: TRect; AID: Integer; const AFixedSize: TRect;
  const AImageIndexes: array of Byte; const APossibleStates: TdxByteSet;
  AIsTopDown: Boolean = True; AInterpolationMode: Integer = InterpolationModeDefault);
var
  I, J, AImageIndex: Integer;
  AOffsetSize: TSize;
  ALoadRect: TRect;
begin
  J := 0;
  if AIsTopDown then
  begin
    AOffsetSize.cx := 0;
    AOffsetSize.cy := cxRectHeight(R);
  end
  else
  begin
    AOffsetSize.cx := cxRectWidth(R);
    AOffsetSize.cy := 0;
  end;
  for I := Low(TStatesArray) to High(TStatesArray) do
  begin
    if (APossibleStates = []) or (I in APossibleStates) then
    begin
      if Length(AImageIndexes) = 0 then
        AImageIndex := J
      else
        if J < Length(AImageIndexes) then
          AImageIndex := AImageIndexes[J]
        else
          AImageIndex := 0;
      ALoadRect := cxRectOffset(R, AOffsetSize.cx * AImageIndex, AOffsetSize.cy * AImageIndex);
      Inc(J);
      if cxRectIsEqual(cxEmptyRect, AFixedSize) then
        TStatesArray(AParts)[I] := AddPart1x1(ABitmap, ALoadRect, AID, '', AInterpolationMode)
      else
        TStatesArray(AParts)[I] := AddPart3x3(ABitmap, ALoadRect, AFixedSize, AID, '', AInterpolationMode);
    end;
    Inc(AID);
  end;
end;

procedure TdxCustomRibbonSkin.LoadElementPartsFromFile(const AFileName: string; var AParts; AID: Integer;
  const AFixedSize: TRect; const AImageIndexes: array of Byte; const APossibleStates: TdxByteSet);
var
  ABitmap: GpGraphics;
  AImageRect: TRect;
begin
  if not CheckGdiPlus then Exit;
  GdipCheck(GdipLoadImageFromFile(PWideChar(AFileName), ABitmap));
  AImageRect.Left := 0;
  AImageRect.Top := 0;
  GdipCheck(GdipGetImageWidth(ABitmap, AImageRect.Right));
  GdipCheck(GdipGetImageHeight(ABitmap, AImageRect.Bottom));
  LoadElementParts(ABitmap, AParts, AImageRect, AID, AFixedSize, AImageIndexes,
    APossibleStates);
  GdipDisposeImage(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadFourStateArray(ABitmap: GpBitmap; R: TRect; const Fixed: TRect;
  var AStateArray: TFourStateArray; AStartID: Integer; AIsVerticalLayout: Boolean; AInterpolationMode: Integer);
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    AStateArray[I] := AddPart3x3(ABitmap, R, Fixed, AStartID, '', AInterpolationMode);
    if AIsVerticalLayout then
      OffsetRect(R, 0, R.Bottom - R.Top)
    else
      OffsetRect(R, R.Right - R.Left, 0);
    Inc(AStartID);
  end;
end;

procedure TdxCustomRibbonSkin.LoadThreeStateArray(ABitmap: GpBitmap; R: TRect; const Fixed: TRect;
  var AStateArray: TThreeStateArray; AStartID: Integer; AIsVerticalLayout: Boolean = True;
  AInterpolationMode: Integer = InterpolationModeDefault);
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    AStateArray[I] := AddPart3x3(ABitmap, R, Fixed, AStartID, '', AInterpolationMode);
    if AIsVerticalLayout then
      OffsetRect(R, 0, R.Bottom - R.Top)
    else
      OffsetRect(R, R.Right - R.Left, 0);
    Inc(AStartID);
  end;
end;

procedure TdxCustomRibbonSkin.AdjustApplicationButtonFont(AFont: TFont; AState: TdxRibbonApplicationButtonState);
const
  StatesMap: array[TdxRibbonApplicationButtonState] of Integer = (DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED);
begin
  AFont.Color := GetPartColor(rspApplicationButton, StatesMap[AState]);
end;

procedure TdxCustomRibbonSkin.DrawApplicationButton(DC: HDC; const R: TRect; AState: TdxRibbonApplicationButtonState);
const
  BrushColorMap: array[Boolean] of TColor = (clHighlight, clBtnFace);
  PenColorMap: array[TdxRibbonApplicationButtonState] of TColor = (clBtnHighlight, clWhite, clBtnShadow);
  TabStateMap: array[TdxRibbonApplicationButtonState] of TdxRibbonTabState = (rtsNormal, rtsHot, rtsActive);
var
  ARect: TRect;
begin
  if Style >= rs2010 then
    DrawTab(DC, R, TabStateMap[AState])
  else
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      ARect := cxRectInflate(R, -1, -1);

      cxPaintCanvas.Brush.Color := BrushColorMap[AState = rabsNormal];
      cxPaintCanvas.Pen.Color := PenColorMap[AState];
      cxPaintCanvas.Pen.Width := 3;
      cxPaintCanvas.Canvas.Ellipse(ARect);

      cxPaintCanvas.Brush.Style := bsClear;
      cxPaintCanvas.Pen.Color := dxInvertColor(clWindow);
      cxPaintCanvas.Pen.Width := 1;
      cxPaintCanvas.Canvas.Ellipse(ARect);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxCustomRibbonSkin.DrawApplicationMenuBackground(DC: HDC; const R, AContentRect: TRect);
var
  AInnerBorderColor, AOuterBorderColor, ASideColor: TColor;
  AInnerBorderRect, AOuterBorderRect: TRect;
  R1, AHeaderRect, AFooterRect: TRect;
begin
  R1 := cxRectInflate(R, -2, -2);
  AInnerBorderRect := cxRectInflate(AContentRect, 1, 1);
  AOuterBorderRect := cxRectInflate(AInnerBorderRect, 1, 1);
  AHeaderRect := cxRectSetHeight(R1, AOuterBorderRect.Top - R1.Top);
  AFooterRect := cxRectSetTop(R1, AOuterBorderRect.Bottom, R1.Bottom - AOuterBorderRect.Bottom);

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

  GetApplicationMenuContentColors(AInnerBorderColor, AOuterBorderColor, ASideColor);
  DrawFrame(DC, AInnerBorderRect, clNone, AInnerBorderColor);
  DrawFrame(DC, AOuterBorderRect, clNone, AOuterBorderColor);
  FillRectByColor(DC, Rect(AOuterBorderRect.Right, AHeaderRect.Bottom, R1.Right, AFooterRect.Top), ASideColor);
  FillRectByColor(DC, Rect(R1.Left, AHeaderRect.Bottom, AOuterBorderRect.Left, AFooterRect.Top), ASideColor);
end;

procedure TdxCustomRibbonSkin.DrawApplicationMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if (AState = DXBAR_HOT) or LowColors then
    DrawSmallButton(DC, R, AState)
  else
    Parts[FApplicationMenuButton].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawApplicationMenuExtraPaneButton(DC: HDC; const R: TRect; AState: Integer);
begin
  if AState in [DXBAR_HOT, DXBAR_HOTCHECK] then
    DrawSmallButton(DC, R, DXBAR_HOT);
end;

procedure TdxCustomRibbonSkin.DrawApplicationMenuExtraPanePinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawApplicationMenuExtraPaneButton(DC, R, AState);
  DrawApplicationMenuExtraPanePinButtonGlyph(DC, R, AState, AState in [DXBAR_CHECKED, DXBAR_HOTCHECK]);
end;

procedure TdxCustomRibbonSkin.DrawApplicationMenuExtraPanePinButtonGlyph(
  DC: HDC; const R: TRect; AState: Integer; AChecked: Boolean);
var
  AColor: TColor;
begin
  if LowColors then
    AColor := GetPartColor(DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH, AState)
  else
    AColor := clDefault;

  InternalDrawGlyph(DC, R, FPinButtonGlyphs[AChecked], AColor, False);
end;

function TdxCustomRibbonSkin.GetApplicationMenuContentOffset(const ATabsBounds: TRect): TRect;
begin
  Result := GetPartContentOffsets(DXBAR_APPLICATIONMENUCONTENT);
end;

function TdxCustomRibbonSkin.GetApplicationMenuGlyphSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(42, 42));
end;

function TdxCustomRibbonSkin.GetApplicationMenuTextColor(AState: TdxRibbonApplicationButtonState): TColor;
begin
  Result := clMenuText;
end;

procedure TdxCustomRibbonSkin.DrawButtonGroup(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FButtonGroup, DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawButtonGroup(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawButtonGroup(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupBorderLeft(DC: HDC; const R: TRect);
begin
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupBorderMiddle(DC: HDC; const R: TRect; AState: Integer);
begin
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupBorderRight(DC: HDC; const R: TRect);
begin
end;

procedure TdxCustomRibbonSkin.DrawButtonGroupSplitButtonSeparator(DC: HDC; const R: TRect; AState: Integer);
begin
end;

procedure TdxCustomRibbonSkin.DrawCollapsedToolbarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawCollapsedToolbarGlyphBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_DROPDOWNGALLERY));
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryBottomSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  ARect := cxRectInflate(R, 0, -3, -2, -1);
  ARect.Left := ARect.Right - cxRectHeight(ARect);
  if UseRightToLeftAlignment then
    ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, R);
  if LowColors then
    Parts[FDropDownGalleryBottomSizeGrip].DrawColored(DC, ARect, clWindowText, 255, UseRightToLeftAlignment)
  else
    Parts[FDropDownGalleryBottomSizeGrip].Draw(DC, ARect, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryBottomSizingBand(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clWindow);
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryBottomVerticalSizeGrip(DC: HDC; const R: TRect);
begin
  DrawDropDownGalleryVerticalSizeGrip(DC, Rect(R.Left, R.Top + 1, R.Right, R.Bottom));
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryTopSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  ARect := cxRectInflate(R, 0, -1, -2, -3);
  ARect.Left := ARect.Right - cxRectHeight(ARect);
  if UseRightToLeftAlignment then
    ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, R);
  if LowColors then
    Parts[FDropDownGalleryTopSizeGrip].DrawColored(DC, ARect, clWindowText, 255, UseRightToLeftAlignment)
  else
    Parts[FDropDownGalleryTopSizeGrip].Draw(DC, ARect, 255, clDefault, UseRightToLeftAlignment);
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryTopSizingBand(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clWindow);
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryTopVerticalSizeGrip(DC: HDC; const R: TRect);
begin
  DrawDropDownGalleryVerticalSizeGrip(DC, Rect(R.Left, R.Top, R.Right, R.Bottom - 1));
end;

procedure TdxCustomRibbonSkin.DrawEditArrowButton(DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawEditButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(FEditButtons, DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawEditEllipsisButton(DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawEditSpinDownButton(DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawEditSpinUpButton(DC: HDC; const R: TRect; AState: Integer);
begin
  // do nothing
end;

function TdxCustomRibbonSkin.AdjustCaptionFontSize(ASize: Integer; AUseAeroGlass: Boolean): Integer;
begin
  Result := ASize;
end;

procedure TdxCustomRibbonSkin.AdjustRibbonFormBorderIconSize(
  AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize);
begin
  //nothing
end;

function TdxCustomRibbonSkin.ExtendCaptionAreaOnTabs: Boolean;
begin
  Result := Style >= rs2010;
end;

procedure TdxCustomRibbonSkin.DrawFormBorderIcon(DC: HDC; const R: TRect; AIcon: TdxRibbonBorderDrawIcon;
  AState: TdxRibbonBorderIconState);
const
  Pushes: array[Boolean] of Integer = (0, DFCS_PUSHED);
  Buttons: array[TdxRibbonBorderDrawIcon] of Integer = (
    DFCS_CAPTIONMIN, DFCS_CAPTIONMAX, DFCS_CAPTIONRESTORE,
    DFCS_CAPTIONCLOSE, DFCS_CAPTIONHELP, 0, 0
  );
begin
  DrawFrameControl(DC, cxRect(R.Left + 1, R.Top + 2, R.Right - 1, R.Bottom),
    DFC_CAPTION, Buttons[AIcon] or Pushes[AState = rbisPressed]);
end;

procedure TdxCustomRibbonSkin.DrawFormBorders(DC: HDC; const ABordersWidth: TRect);
var
  ASaveIndex: Integer;
  R: TRect;
begin
  ASaveIndex := SaveDC(DC);
  try
    R := FormPaintData.GetBounds;
    cxExcludeClipRect(DC, cxRectContent(R, ABordersWidth));
    InternalDrawFormBorders(DC, R, FormPaintData.GetIsActive, IsRectangularFormBottom(FormPaintData));
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbonSkin.DrawFormCaption(DC: HDC; const R: TRect);
const
  ColorMap: array[Boolean] of TColor = (clInactiveCaption, clActiveCaption);
var
  ARect: TRect;
  ASaveIndex: Integer;
  ATopBorderWidth: Integer;
begin
  ARect := R;
  ASaveIndex := SaveDC(DC);
  try
    if FormPaintData.GetState <> wsMaximized then
    begin
      InternalDrawFormBorders(DC, ARect, FormPaintData.GetIsActive, IsRectangularFormBottom(FormPaintData));
      ATopBorderWidth := GetWindowBordersWidth(PaintData.HasStatusBar).Left;
      ARect := cxRectInflate(ARect, 0, -ATopBorderWidth, 0, 0);
    end;
    FillRectByColor(DC, ARect, ColorMap[FormPaintData.GetIsActive]);
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbonSkin.DrawFormStatusBarPart(DC: HDC; const R: TRect;
  AIsLeft, AIsActive, AIsRaised, AIsRectangular: Boolean);
const
  BorderColorMap: array[Boolean] of TColor = (clInactiveBorder, clActiveBorder);
var
  ARect: TRect;
  ALeftOffset, ARightOffset, AOffsetByStyle: Integer;
begin
  ARect := R;
  ALeftOffset := 0;
  ARightOffset := 0;
  if AIsLeft then
    ALeftOffset := -1
  else
    ARightOffset := -1;
  if Style = rs2007 then
    AOffsetByStyle := 2
  else
    AOffsetByStyle := 1;

  FillRectByColor(DC, ARect, clHighlightText);
  ARect := cxRectInflate(ARect, ALeftOffset, 0, ARightOffset, -3);
  if Style < rs2013 then
  begin
    FillRectByColor(DC, ARect, clBtnShadow);
    ARect := cxRectInflate(ARect, ALeftOffset, 0, ARightOffset, AOffsetByStyle);
    if Style = rs2007 then
    begin
      FillRectByColor(DC, ARect, BorderColorMap[AIsActive]);
      ARect := cxRectInflate(ARect, ALeftOffset, 0, ARightOffset, 0);
    end;
  end;
  FillRectByColor(DC, ARect, clBtnFace);
end;

function TdxCustomRibbonSkin.GetWindowBordersWidth(AHasStatusBar: Boolean): TRect;
begin
  Result := cxRect(4, 0, 4, 4);
  if AHasStatusBar then
    Result.Bottom := 1;
end;

function TdxCustomRibbonSkin.HasExternalRibbonFormShadow: Boolean;
begin
  Result := False;
end;

function TdxCustomRibbonSkin.UseRoundedWindowCorners: Boolean;
begin
  Result := True;
end;

procedure TdxCustomRibbonSkin.DrawHelpButton(DC: HDC; const R: TRect; AState: TcxButtonState);
begin
  DrawTabAreaButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawHelpButtonGlyph(DC: HDC; const R: TRect);
begin
  InternalDrawGlyph(DC, R, FHelpButton);
end;

procedure TdxCustomRibbonSkin.DrawInRibbonGalleryBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawFrame(DC, R, GetPartColor(DXBAR_INRIBBONGALLERY_BACKGROUND, AState), GetPartColor(DXBAR_INRIBBONGALLERY_BORDER, AState));
end;

procedure TdxCustomRibbonSkin.DrawInRibbonGalleryScrollBarBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  //nothing
end;

procedure TdxCustomRibbonSkin.DrawInRibbonGalleryScrollBarButton(DC: HDC;
  const R: TRect; AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);

  procedure DrawButtonBackground(DC: HDC; R: TRect);
  begin
    FillRectByColor(DC, R, clBtnFace);
    if Style = rs2007 then
      R := cxRectInflate(R, -1, -1);
    case AState of
      DXBAR_HOT, DXBAR_CHECKED, DXBAR_HOTCHECK:
        DrawFrame(DC, R, clHighlight, clBtnText);
      DXBAR_PRESSED:
        DrawFrame(DC, R, clHighlight, clBtnShadow);
      DXBAR_DISABLED:
        DrawFrame(DC, R, clBtnFace, clGrayText)
      else
        DrawFrame(DC, R, clBtnFace, clBtnText);
    end;
  end;

var
  ASaveIndex: Integer;
  R1: TRect;
begin
  R1 := R;
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    Inc(R1.Bottom, Ord(AButtonKind <> gsbkDropDown));
    DrawButtonBackground(DC, R1);
    DrawInRibbonGalleryScrollBarButtonGlyph(DC, R, AButtonKind, AState);
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxCustomRibbonSkin.DrawInRibbonGalleryScrollBarButtonGlyph(
  DC: HDC; R: TRect; AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer);
var
  AArrowRect: TRect;
  AArrowSize: Integer;
  AColor: TColor;
  APoints: TcxArrowPoints;
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    AArrowSize := GetArrowSize;
    AArrowRect := cxRectOffset(cxRectCenter(R, cxSize(AArrowSize)), IfThen(not UseRightToLeftAlignment, 1, 2), 1);
    if Odd(AArrowSize) <> Odd(cxRectWidth(R)) then
      AArrowRect := cxRectOffset(AArrowRect, IfThen(not UseRightToLeftAlignment, 1), 0);

    AColor := GetInRibbonGalleryScrollBarButtonGlyphColor(AButtonKind, AState);
    case AButtonKind of
      gsbkLineUp:
        DrawArrow(DC, AArrowRect, adUp, AColor);
      gsbkLineDown:
        DrawArrow(DC, AArrowRect, adDown, AColor);
      gsbkDropDown:
        begin
          CalculateArrowPoints(AArrowRect, APoints, adDown);
          DrawArrow(DC, cxRectOffset(AArrowRect, 0, ScaleFactor.Apply(2)), adDown, AColor);
          AArrowRect := cxRectSetBottom(AArrowRect, AArrowRect.Top, ScaleFactor.Apply(1));
          AArrowRect.Left := APoints[2].X;
          AArrowRect.Right := APoints[0].X;
          FillRectByColor(DC, AArrowRect, AColor);
        end;
    end;
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

function TdxCustomRibbonSkin.GetInRibbonGalleryScrollBarButtonGlyphColor(
  AButtonKind: TdxInRibbonGalleryScrollBarButtonKind; AState: Integer): TColor;
begin
  Result := GetPartColor(DXBAR_ARROWDOWN, AState);
end;

procedure TdxCustomRibbonSkin.DrawInRibbonGalleryScrollBarDropDownTouchButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawInRibbonGalleryScrollBarButton(DC, R, gsbkDropDown, AState);
end;

procedure TdxCustomRibbonSkin.DrawLargeButton(DC: HDC; const R: TRect; AState: Integer);
var
  AColor: TColor;
begin
  if Style = rs2007 then
    AColor := clBtnShadow
  else
    AColor := clNone;

  case AState of
    DXBAR_HOT, DXBAR_CHECKED, DXBAR_HOTCHECK, DXBAR_ACTIVE:
      DrawFrame(DC, R, clHighlight, clHighlightText);
    DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
      DrawFrame(DC, R, clHighlight, AColor);
    else
      DrawFrame(DC, R, clBtnFace, clNone);
  end
end;

procedure TdxCustomRibbonSkin.DrawLargeButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
var
  AColor: TColor;
begin
  if Style = rs2007 then
    AColor := clBtnShadow
  else
    AColor := clNone;

  case AState of
    DXBAR_HOT, DXBAR_CHECKED, DXBAR_HOTCHECK, DXBAR_ACTIVE:
      DrawFrame(DC, R, clHighlight, clBtnText, [bLeft, bTop, bRight]);
    DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
      DrawFrame(DC, R, clHighlight, AColor, [bLeft, bTop, bRight]);
    else
      DrawFrame(DC, R, clBtnFace, clNone);
  end;
end;

procedure TdxCustomRibbonSkin.DrawLargeButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawLargeButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawLaunchButtonBackground(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawLaunchButtonDefaultGlyph(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMDIButton(DC: HDC; const R: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawMDIButtonGlyph(DC: HDC; const R: TRect; AButton: TdxBarMDIButton;
  AState: TcxButtonState);
const
  Buttons: array[TdxBarMDIButton] of Integer = (
    DFCS_CAPTIONMIN, DFCS_CAPTIONRESTORE, DFCS_CAPTIONCLOSE);
  Pushes: array[Boolean] of Integer = (0, DFCS_PUSHED);
begin
  DrawFrameControl(DC, cxRect(R.Left + 1, R.Top + 3, R.Right - 1, R.Bottom - 3),
    DFC_CAPTION, Buttons[AButton] or Pushes[AState = cxbsPressed]);
end;

procedure TdxCustomRibbonSkin.DrawMenuArrowDown(DC: HDC; const R: TRect);
begin
  DrawArrow(DC, R, adDown, GetPartColor(DXBAR_ARROWDOWN));
end;

procedure TdxCustomRibbonSkin.DrawMenuArrowRight(DC: HDC; const R: TRect);
begin
  DrawArrow(DC, R, adRight, GetPartColor(DXBAR_ARROWDOWN));
end;

procedure TdxCustomRibbonSkin.DrawMenuBackButton(DC: HDC; const R: TRect);
begin
  DrawArrow(DC, R, adLeft, GetPartColor(DXBAR_ARROWDOWN));
end;

procedure TdxCustomRibbonSkin.DrawMenuCheck(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuCheckMark(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuContent(DC: HDC; const R: TRect);
begin
  if LowColors then
    Parts[FMenuContent].DrawColored(DC, R, clBtnFace)
  else
    Parts[FMenuContent].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawMenuDetachCaption(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuExtraSeparator(
  DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    InternalDrawSeparator(DC, R, AHorizontal, clMenuText, clMenu)
  else
    InternalDrawSeparator(DC, R, AHorizontal, $F5F5F5, $D8D8D8);
end;

procedure TdxCustomRibbonSkin.DrawMenuGlyph(DC: HDC; const R: TRect);
begin
  if LowColors then
    Parts[FMenuGlyph].DrawColored(DC, R, clBtnFace)
  else
    Parts[FMenuGlyph].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawMenuItem(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuItemDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonArrowPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuItemDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuMark(DC: HDC; const R: TRect);
begin
  if LowColors then
    DrawMarkArrow(DC, R, DXBAR_NORMAL)
  else
    Parts[FMenuMark].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawMenuScrollArea(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMenuSeparatorHorz(DC: HDC; const R: TRect);
begin
  if LowColors then
    Parts[FMenuSeparatorHorz].DrawColored(DC, cxRectSetHeight(R, 1), clBtnText)
  else
    Parts[FMenuSeparatorHorz].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawMenuSeparatorVert(DC: HDC; const R: TRect);
begin
  if LowColors then
  begin
    Parts[FMenuSeparatorVert].DrawColored(DC, R, clMenu, 255, UseRightToLeftAlignment);
    Parts[FMenuSeparatorVert].DrawColored(DC, cxRectSetWidth(R, 1), clBtnShadow, 255, UseRightToLeftAlignment);
  end
  else
    Parts[FMenuSeparatorVert].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
end;

function TdxCustomRibbonSkin.GetMenuColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteMenu, AState);
end;

function TdxCustomRibbonSkin.GetMenuSeparatorSize: Integer;
begin
  Result := 2;
end;

procedure TdxCustomRibbonSkin.DrawMinimizeRibbonButton(
  DC: HDC; const R: TRect; AState: TcxButtonState; AMinimized: Boolean);
begin
  DrawTabAreaButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawMinimizeRibbonButtonGlyph(
  DC: HDC; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
begin
end;

procedure TdxCustomRibbonSkin.DrawProgressDiscreteBand(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clHighlight);
end;

procedure TdxCustomRibbonSkin.DrawProgressSolidBand(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clHighlight);
end;

procedure TdxCustomRibbonSkin.DrawProgressSubstrate(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
end;

procedure TdxCustomRibbonSkin.DrawSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawSmallButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawPart(DC, R, AState);
end;

function TdxCustomRibbonSkin.GetSmallButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteTabGroup, AState);
end;

procedure TdxCustomRibbonSkin.AdjustBackstageViewTabButtonFont(AFont: TFont);
begin
  // do nothing
end;

function TdxCustomRibbonSkin.CanShowTabAreaToolbarInBackstageView: Boolean;
begin
  Result := True;
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewBackButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_BACKSTAGEVIEW));
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewMenuBackground(DC: HDC; const R: TRect);
begin
  if LowColors then
  begin
    FillRectByColor(DC, R, clWindow);
    if UseRightToLeftAlignment then
      FillRectByColor(DC, cxRectSetLeft(R, R.Left, 1), dxInvertColor(clWindow))
    else
      FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), dxInvertColor(clWindow));
  end
  else
  begin
    FillRectByColor(DC, R, $FCFCFC);
    if UseRightToLeftAlignment then
      FillRectByColor(DC, cxRectSetLeft(R, R.Left, 1), $F0F0F0)
    else
      FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), $F0F0F0);
  end;
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewMenuButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewTabButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewMenuHeader(DC: HDC; const R: TRect);
begin
  if LowColors then
  begin
    FillRectByColor(DC, R, clWindow);
    if UseRightToLeftAlignment then
      FillRectByColor(DC, cxRectSetLeft(R, R.Left, 1), clWindowText)
    else
      FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), clWindowText);
  end
  else
  begin
    FillRectByColor(DC, R, $FCFCFC);
    if UseRightToLeftAlignment then
      FillRectByColor(DC, cxRectSetLeft(R, R.Left, 1), $F0F0F0)
    else
      FillRectByColor(DC, cxRectSetRight(R, R.Right, 1), $F0F0F0);
  end;
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewMenuSeparator(DC: HDC; const R: TRect; AState: Integer);
var
  R1: TRect;
begin
  R1 := cxRectInflate(R, -4, 0);
  FillRectByColor(DC, cxRectSetHeight(R1, 1), clBtnShadow);
  FillRectByColor(DC, cxRectSetBottom(R1, R1.Bottom, 1), clBtnHighlight);
end;

function TdxCustomRibbonSkin.GetBackstageViewMenuButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteBackstageViewMenu, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryBackground(DC: HDC; const R: TRect);
begin
  DrawDropDownGalleryBackground(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryGroupHeader(DC: HDC; const R: TRect);
begin
  DrawGalleryGroupHeaderBackground(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryItem(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryItemPinButton(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_CHECKED:
      AState := DXBAR_NORMAL;
    DXBAR_HOTCHECK:
      AState := DXBAR_HOT;
  end;
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryItemPinButtonGlyph(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawApplicationMenuExtraPanePinButtonGlyph(DC, R, AState, AState in [DXBAR_CHECKED, DXBAR_HOTCHECK]);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGalleryItemPinTag(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawBackstageViewGalleryItemPinButtonGlyph(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawBackstageViewGallerySeparator(DC: HDC; const R: TRect);
begin
  DrawMenuSeparatorHorz(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawMarkArrow(DC: HDC; const R: TRect; AState: Integer);
var
  H: Integer;
begin
  H := (R.Bottom - R.Top) div 7;
  DrawPart(DC, cxRect(R.Left + 3, R.Top + H * 3, R.Right - 3, R.Bottom - H * 2), AState);
end;

procedure TdxCustomRibbonSkin.DrawMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
var
  H: Integer;
begin
  if LowColors then
  begin
    H := (R.Bottom - R.Top) div 7;
    DrawPart(DC, cxRect(R.Left + H + 1, R.Top + H * 3, R.Right - H + 1, R.Bottom - H * 2), AState);
  end
  else
    InternalDrawGlyph(DC, R, FMarkTruncated[AState], clDefault, False);
end;

procedure TdxCustomRibbonSkin.AdjustTabFont(AFont: TFont; AState: Integer);
begin
  AFont.Color := GetPartColor(rspTabHeaderText, AState);
end;

procedure TdxCustomRibbonSkin.DrawTab(DC: HDC; const R: TRect; AState: TdxRibbonTabState);
var
  R1: TRect;
begin
  R1 := R;
  Dec(R1.Bottom);
  case AState of
    rtsNormal:
      FillRectByColor(DC, R1, clBtnFace);
    rtsHot:
      DrawFrame(DC, R1, clHighlight, clBtnFace, [bLeft, bTop, bRight]);
    rtsActiveHot:
      DrawFrame(DC, R1, clHighlight, clHighlight, [bLeft, bTop, bRight]);
    else
      DrawFrame(DC, R1, clHighlight, dxInvertColor(clBtnFace), [bLeft, bTop, bRight]);
  end;
end;

procedure TdxCustomRibbonSkin.DrawTabAreaArrowDown(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawArrowDown(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawTabAreaBackground(DC: HDC; const R: TRect; AActive, AUseAeroGlass: Boolean;
  AApplicationMenuState: TdxRibbonApplicationMenuState);
begin
  if not AUseAeroGlass then
    DrawRibbonBackground(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawTabBase(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawTabGroupBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
begin
  DrawFrame(DC, R, clBtnFace, clBtnShadow, [bTop, bLeft, bRight])
end;

procedure TdxCustomRibbonSkin.DrawTabGroupHeaderBackground(
  DC: HDC; const R: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R1: TRect;
begin
  R1 := cxRect(R.Left + 4, R.Top, R.Right - 4, R.Top + 1);
  FillRectByColor(DC, R1, clBtnShadow);
  cxExcludeClipRect(DC, R1);
  DrawFrame(DC, R, clBtnFace, clBtnShadow, [bBottom, bLeft, bRight]);
end;

procedure TdxCustomRibbonSkin.DrawTabGroupsArea(
  DC: HDC; const R: TRect; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  DrawFrame(DC, R, clBtnFace, clBtnShadow);
end;

procedure TdxCustomRibbonSkin.DrawTabScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
begin
  DrawGroupScrollButton(DC, R, ALeft, AState);
end;

procedure TdxCustomRibbonSkin.DrawTabScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean);
const
  ArrowDirection: array[Boolean] of TcxArrowDirection = (adRight, adLeft);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxLookAndFeelPaintersManager.GetPainter(lfsStandard).DrawArrow(
      cxPaintCanvas, cxRectInflate(R, -2, 0, -2, -4), ArrowDirection[ALeft],
      GetPartColor(rspGroupScrollArrow));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawTabSeparator(DC: HDC; const R: TRect; Alpha: Byte);
begin
  Parts[FTabSeparator].Draw(DC, R, Alpha);
end;

procedure TdxCustomRibbonSkin.AdjustContextFont(AFont: TFont; AUseGlass: Boolean; AContextColor: TColor);
const
  PartsMap: array[Boolean] of Integer = (rspContextText, rspContextTextOnGlass);
begin
  AFont.Color := GetPartColor(PartsMap[AUseGlass]);
end;

procedure TdxCustomRibbonSkin.AdjustContextTabFont(AFont: TFont; AState: Integer; AContextColor: TColor);
begin
  AFont.Color := GetPartColor(rspTabHeaderText, AState);
end;

procedure TdxCustomRibbonSkin.DrawContextBackground(DC: HDC; const R: TRect; AContextColor: TColor);
var
  ASaveIndex: Integer;
  R1: TRect;
begin
  ASaveIndex := SaveDC(DC);
  try
    R1 := cxRectInflate(R, -2, -4, -2, -2);
    FillRectByColor(DC, R1, clHighlight);
    cxExcludeClipRect(DC, R1);
    FillRectByColor(DC, R, AContextColor);
  finally
    RestoreDC(DC, ASaveIndex)
  end;
end;

procedure TdxCustomRibbonSkin.DrawContextBackgroundGlass(DC: HDC; const R: TRect; AContextColor: TColor);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawContextTabBackground(DC: HDC; const R: TRect; AState: TdxRibbonTabState;
  AContextColor: TColor);
begin
  if LowColors then
    DrawTab(DC, R, AState)
  else
  begin
    FillRectByColor(DC, R, AContextColor);
    case AState of
      rtsNormal:
        FillRectByColor(DC, R, clBtnFace);
      rtsActive, rtsActiveHot:
        DrawFrame(DC, R, clHighlight, clBtnText, [bLeft, bTop, bRight]);
      else
        DrawFrame(DC, R, clHighlight, clBtnFace, [bTop]);
    end;
  end;
end;

procedure TdxCustomRibbonSkin.DrawContextTabGroupsArea(
  DC: HDC; const R: TRect; AContextColor: TColor; AIsQATAtBottom, AIsInPopup: Boolean);
begin
  DrawFrame(DC, R, clBtnFace, clBtnShadow)
end;

procedure TdxCustomRibbonSkin.DrawContextTabSeparator(DC: HDC; const R: TRect; ABeginGroup: Boolean);
begin
end;

procedure TdxCustomRibbonSkin.AdjustQuickAccessToolbarVertical(
  var ABounds: TRect; ANonClientDraw, ADontUseAero: Boolean);
begin
  if ANonClientDraw then
  begin
    if IsCompositionEnabled and not ADontUseAero then
    begin
      Inc(ABounds.Top, 3);
      Dec(ABounds.Bottom, 2);
    end
    else
    begin
      Inc(ABounds.Top, 4);
      Dec(ABounds.Bottom, 3);
    end;
  end
  else
  begin
    Inc(ABounds.Top, 3);
    Dec(ABounds.Bottom, 4);
  end;
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbar(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AHasApplicationButton, AIsActive, ADontUseAero: Boolean);

  procedure GetQuickAccessToolBarParts(var ALeftPart, ARightPart: Integer);
  var
    AInactive: Boolean;
  begin
    if ANonClientDraw and IsCompositionEnabled and not ADontUseAero then
    begin
      ALeftPart := FQATGlassAtTopLeft[AHasApplicationButton];
      ARightPart := FQATGlassAtTopRight;
    end
    else
    begin
      AInactive := ANonClientDraw and not AIsActive;
      ALeftPart := FQATAtTopLeft[AHasApplicationButton][AInactive];
      ARightPart := FQATAtTopRight[AInactive];
    end;
  end;

  procedure InternalDrawQuickAccessToolbarInLowColors(ARect: TRect; const AIsLeft: Boolean);
  var
    APath: TdxGPPath;
    ALeft: Integer;
    AMatrix: TdxGPMatrix;
  begin
    dxGPPaintCanvas.BeginPaint(DC, R);
    APath := TdxGPPath.Create;
    try
      ARect.Top := ARect.Top + 1;
      ARect.Bottom := ARect.Bottom - 2;
      if AIsLeft then
      begin
        ALeft := ARect.Left + 10 + 3;
        APath.AddLine(ARect.Right, ARect.Bottom, ALeft, ARect.Bottom);
        APath.AddLine(ALeft, ARect.Bottom, ALeft, ARect.Bottom - (cxRectHeight(ARect) div 2));
        APath.AddArc(ALeft - 19, ARect.Top, 20, cxRectHeight(ARect) * 2, -45, -43);
        APath.AddLine(ALeft, ARect.Top, ARect.Right, ARect.Top);
      end
      else
      begin
        ALeft := 9;
        APath.AddLine(ARect.Left, ARect.Top, ARect.Right - ALeft, ARect.Top);
        APath.AddArc(ARect.Right - ALeft * 2, ARect.Top, ALeft * 2, cxRectHeight(ARect), -90, 180);
        APath.AddLine(ARect.Right - ALeft, ARect.Bottom, ARect.Left, ARect.Bottom);
      end;
      if UseRightToLeftAlignment then
      begin
        AMatrix := TdxGPMatrix.CreateFlip(True, False, cxRectCenter(ARect).X, 0);
        try
          APath.Transform(AMatrix);
        finally
          AMatrix.Free;
        end;
      end;
      dxGPPaintCanvas.Path(APath, dxInvertColor(clWindow), clWindow, 1, psSolid, 255, 255);
    finally
      APath.Free;
      dxGPPaintCanvas.EndPaint;
    end;
  end;

var
  AWidth, ALeftPart, ARightPart: Integer;
  R1, ALeftPartRect, ARightPartRect: TRect;
begin
  if ABellow then
    Parts[FQATAtBottom].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment)
  else
  begin
    AWidth := cxRectHeight(R) div 2;
    if cxRectWidth(R) >= 2 * AWidth then
    begin
      GetQuickAccessToolBarParts(ALeftPart, ARightPart);
      R1 := cxRectSetWidth(R, Parts[ALeftPart].Size.cx);
      AdjustQuickAccessToolbarVertical(R1, ANonClientDraw, ADontUseAero);
      ALeftPartRect := R1;
      R1.Left := R1.Right;
      R1.Right := R.Right - AWidth;
      ARightPartRect := R1;

      if UseRightToLeftAlignment then
      begin
        ALeftPartRect := TdxRightToLeftLayoutConverter.ConvertRect(ALeftPartRect, R);
        ARightPartRect := TdxRightToLeftLayoutConverter.ConvertRect(ARightPartRect, R);
      end;
      if LowColors then
      begin
        InternalDrawQuickAccessToolbarInLowColors(ALeftPartRect, True);
        InternalDrawQuickAccessToolbarInLowColors(ARightPartRect, False)
      end
      else
      begin
        Parts[ALeftPart].Draw(DC, ALeftPartRect, 255, clDefault, UseRightToLeftAlignment);
        Parts[ARightPart].Draw(DC, ARightPartRect, 255, clDefault, UseRightToLeftAlignment);
      end;
    end;
  end;
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarArrowDown(DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  DrawArrowDown(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarDefaultGlyph(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace)
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarGroupButton(DC: HDC; const R: TRect;
  ABellow, ANonClientDraw, AIsActive: Boolean; AState: Integer);
begin
  if ABellow or ANonClientDraw and not AIsActive then
    DrawPart(FQATGroupButtonInactive, DC, R, AState)
  else
    DrawPart(FQATGroupButtonActive, DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarMarkArrow(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  DrawMarkArrow(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarMarkTruncated(
  DC: HDC; const R: TRect; AState: Integer; ABelow: Boolean);
begin
  DrawMarkTruncated(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarPopup(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
end;

function TdxCustomRibbonSkin.GetQuickAccessToolbarMarkButtonOffset(AHasApplicationButton: Boolean; ABelow: Boolean): Integer;
begin
  if ABelow then
    Result := 5
  else
    Result := 12;

  Result := ScaleFactor.Apply(Result);
end;

function TdxCustomRibbonSkin.GetQuickAccessToolbarOverrideWidth(AHasApplicationButton: Boolean; AUseAeroGlass: Boolean): Integer;
begin
  if AHasApplicationButton then
    Result := ScaleFactor.Apply(14)
  else
    Result := 0;
end;

function TdxCustomRibbonSkin.GetQuickAccessToolbarColorPalette(AState: Integer; ABelow: Boolean): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteQAT, AState);
end;

function TdxCustomRibbonSkin.GetQuickAccessToolbarLeftIndent(AHasApplicationButton: Boolean; AUseAeroGlass: Boolean): Integer;
begin
  Result := 0;
end;

function TdxCustomRibbonSkin.GetQuickAccessToolbarRightIndent(AHasApplicationButton: Boolean): Integer;
begin
  Result := 0;
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarSmallButton(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawSmallButton(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC: HDC; const R: TRect;
  AState: Integer);
begin
  DrawSmallButtonDropButtonArrowPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC: HDC; const R: TRect;
  AState: Integer);
begin
  DrawSmallButtonDropButtonMainPart(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawStatusBar(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
end;

procedure TdxCustomRibbonSkin.DrawStatusBarGripBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
end;

procedure TdxCustomRibbonSkin.DrawStatusBarPanel(DC: HDC; const Bounds, R: TRect; AIsLowered: Boolean);
begin
  FillRectByColor(DC, R, clBtnFace);
end;

procedure TdxCustomRibbonSkin.DrawStatusBarPanelSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
  FillRectByColor(DC, cxRect(R.Left, R.Top + 1, R.Left + 1, R.Bottom - 1), clBtnShadow);
end;

procedure TdxCustomRibbonSkin.DrawStatusBarSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  ARect := R;
  cxRightToLeftDependentDraw(DC, ARect, UseRightToLeftAlignment,
    procedure
    begin
      if LowColors then
        Office11DrawSizeGrip(DC, ARect, clBtnText, clBtnShadow)
      else
        Office11DrawSizeGrip(DC, ARect, GetPartColor(rspStatusBarSizeGripColor1), GetPartColor(rspStatusBarSizeGripColor2));
    end, False);
end;

procedure TdxCustomRibbonSkin.DrawStatusBarToolbarSeparator(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnFace);
  FillRectByColor(DC, cxRect(R.Left, R.Top, R.Left + 1, R.Bottom - 1), clBtnShadow);
end;

function TdxCustomRibbonSkin.GetStatusBarPanelColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteStatusBar, AState);
end;

function TdxCustomRibbonSkin.GetStatusBarSeparatorSize: Integer;
begin
  Result := 3;
end;

procedure TdxCustomRibbonSkin.DrawScrollBarBackground(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    GetScrollBarPainter.DrawScaledScrollBarBackground(cxPaintCanvas, R, AHorizontal, ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawScrollBarPart(DC: HDC; const R: TRect; APart: TcxScrollBarPart; AState: Integer;
  AHorizontal: Boolean);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    GetScrollBarPainter.DrawScaledScrollBarPart(cxPaintCanvas, AHorizontal, R, APart, RibbonStateToButtonState(AState), ScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawScrollBoxSizeGripArea(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetScrollBarPainter.DefaultSizeGripAreaColor);
end;

function TdxCustomRibbonSkin.GetScrollBarPainter: TcxCustomLookAndFeelPainter;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsNative);
end;

procedure TdxCustomRibbonSkin.DrawGroupScrollButton(DC: HDC; const R: TRect; ALeft: Boolean; AState: Integer);
begin
  DrawFrame(DC, R, clBtnFace, clBtnText);
  DrawSmallButton(DC, R, AState)
end;

procedure TdxCustomRibbonSkin.DrawGroupScrollButtonGlyph(DC: HDC; const R: TRect; ALeft: Boolean);
const
  ArrowDirection: array[Boolean] of TcxArrowDirection = (adRight, adLeft);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxLookAndFeelPaintersManager.GetPainter(lfsStandard).DrawArrow(
      cxPaintCanvas, cxRectInflate(R, -2, 0, -2, -2), ArrowDirection[ALeft],
      GetPartColor(rspGroupScrollArrow));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawMiniToolbarBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_MINITOOLBAR_BACKGROUND));
end;

function TdxCustomRibbonSkin.GetMiniToolbarColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteMiniToolbar, AState);
end;

function TdxCustomRibbonSkin.GetRadialMenuColorPalette: IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteRadialMenu, DXBAR_NORMAL);
end;

procedure TdxCustomRibbonSkin.DrawArrowDown(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawArrow(DC, R, adDown, GetPartColor(DXBAR_ARROWDOWN, AState));
end;

procedure TdxCustomRibbonSkin.DrawDropDownBorder(DC: HDC; const R: TRect);
begin
  if LowColors then
  begin
    Parts[FDropDownBorder].DrawColored(DC, R, clWindowText);
    Parts[FDropDownBorder].DrawColored(DC, cxRectInflate(R, -1, -1), clWindow);
  end
  else
    Parts[FDropDownBorder].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawGalleryFilterBandBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clWindow);
end;

procedure TdxCustomRibbonSkin.DrawGalleryGroupHeaderBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, clBtnShadow);
end;

procedure TdxCustomRibbonSkin.DrawGalleryGroupItemSelectionFrame(DC: HDC; const ARect: TRect; AState: Integer);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.FrameRect(ARect, clHighlight, 2);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawItemSeparator(DC: HDC; const R: TRect; AHorizontal: Boolean);
begin
  if LowColors then
    InternalDrawSeparator(DC, R, AHorizontal, clBtnShadow, clNone)
  else
    InternalDrawSeparator(DC, R, AHorizontal, $EBE8E6, $AAA6A2);
end;

procedure TdxCustomRibbonSkin.DrawKeyTip(DC: HDC; const R: TRect);
begin
  DrawScreenTip(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawRibbonBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(rspRibbonBackground));
end;

procedure TdxCustomRibbonSkin.DrawRibbonClientTopArea(DC: HDC; const R: TRect);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawRibbonFormBackground(DC: HDC; const R: TRect; ARibbonHeight: Integer);
begin
  FillRectByColor(DC, R, GetPartColor(rfspRibbonForm));
end;

procedure TdxCustomRibbonSkin.DrawRibbonTopFrameArea(DC: HDC; const R: TRect; AUseAeroGlass: Boolean);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.DrawRibbonTopFrameAreaSeparator(DC: HDC; const R: TRect);
begin
  // do nothing
end;

function TdxCustomRibbonSkin.GetRibbonTopFrameAreaSeparatorSize: Integer;
begin
  Result := 1;
end;

procedure TdxCustomRibbonSkin.DrawScreenTip(DC: HDC; const R: TRect);
begin
  if LowColors then
    DrawFrame(DC, R, clInfoBk, clInfoText)
  else
    Parts[FScreenTip].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawScrollArrow(DC: HDC; const R: TRect; AState: Integer = 0);
begin
  if LowColors then
    Parts[FScrollArrow].DrawColored(DC, R, GetPartColor(DXBAR_ARROWDOWN, AState))
  else
    Parts[FScrollArrow].Draw(DC, R);
end;

procedure TdxCustomRibbonSkin.DrawSeparatorBackground(DC: HDC; const R: TRect);
begin
  FillRectByColor(DC, R, GetPartColor(DXBAR_SEPARATOR_BACKGROUND));
end;

procedure TdxCustomRibbonSkin.DrawSeparatorLine(DC: HDC; const R: TRect);
begin
  DrawMenuSeparatorHorz(DC, R);
end;

function TdxCustomRibbonSkin.GetIsAlphaUsed(APart: Integer): Boolean;
begin
  Result := False;
end;

function TdxCustomRibbonSkin.GetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  if LowColors then
    Result := DoGetPartLowColor(APart, AState)
  else
    Result := DoGetPartColor(APart, AState);
end;

function TdxCustomRibbonSkin.GetPartContentOffsets(APart: Integer): TRect;
begin
  case APart of
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      Result := ScaleFactor.Apply(cxRect(7, 4, 7, 4));
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINTAG:
      Result := Rect(3, 3, 3, 3);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTON,
    DXBAR_MENUEXTRAPANE_PINBUTTON:
      Result := ScaleFactor.Apply(Rect(7, 0, 8, 0));
    DXBAR_APPLICATIONMENUBUTTON:
      Result := Rect(0, 4, 0, 3);
    DXBAR_COLLAPSEDTOOLBAR:
      Result := Rect(2, 2, 2, 2);
    DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND:
      Result := Rect(7, 4, 7, 11);
    DXRIBBON_QAT_SMALLBUTTON, DXRIBBON_TAT_SMALLBUTTON:
      Result := GetPartContentOffsets(DXBAR_SMALLBUTTON);
    DXRIBBON_QAT_GROUPBUTTON, DXBAR_SMALLBUTTON:
      Result := ScaleFactor.Apply(Rect(3, 3, 3, 3));
    DXBAR_LARGEBUTTON:
      Result := ScaleFactor.Apply(Rect(5, 3, 5, 3));
    DXBAR_APPLICATIONBUTTON:
      Result := ScaleFactor.Apply(cxRect(3, 5, 4, 0));
    DXBAR_APPLICATIONMENUCONTENT:
      Result := ScaleFactor.Apply(cxRect(4, 16, 4, 29));
    DXBAR_APPLICATIONBUTTONICONOFFSET:
      Result := ScaleFactor.Apply(cxRect(8, 8, 10, 10));
    DXBAR_TOOLBARINPOPUP, DXBAR_TOOLBAR:
      Result := cxRect(2, 2, 2, 1);
    DXBAR_RIBBONTABGROUP, DXBAR_RIBBONCONTEXTTABGROUP:
      Result := cxRect(3, 3, 3, 4);
    DXBAR_QUICKACCESSTOOLBAR:
      Result := cxRect(2, 2, 2, 2);
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
      Result := ScaleFactor.Apply(cxRect(10, 0, 0, 0));
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      Result := ScaleFactor.Apply(cxRect(21, 12, 11, 12));
    DXBAR_BACKSTAGEVIEW_MENUBAR:
      Result := ScaleFactor.Apply(cxRect(6, 5, 6, 6));
    DXBAR_GALLERYFILTERBAND:
      Result := ScaleFactor.Apply(cxRect(0, 2, 0, 2));
  else
    Result := cxNullRect;
  end;
end;

function TdxCustomRibbonSkin.GetPartSize(APart: Integer): Integer;
begin
  case APart of
    rspContextTabOverlap:
      Result := 0;
    DXBAR_BACKSTAGEVIEW_BACKBUTTON_OFFSET:
      Result := GetPartContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB).Left;
    DXBAR_MENUSEPARATORHORZ, DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
      Result := GetMenuSeparatorSize;
    DXBAR_SEPARATOR_LINE:
      Result := 1;
    rspTabGroupBottomOffset, rspTabGroupInPopupBottomOffset:
      Result := 3;
    DXBAR_BACKSTAGEVIEW_BACKBUTTON:
      Result := ScaleFactor.Apply(32);
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      Result := ScaleFactor.Apply(2);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_BORDER:
      Result := 1;
    rspContextTabSeparatorBegin, rspContextTabSeparatorEnd:
      Result := 1;
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      Result := ScaleFactor.Apply(40);
    DXBAR_BACKSTAGEVIEW_MENUBAR_INDENTBETWEENITEMS:
      Result := ScaleFactor.Apply(1);
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
      Result := ScaleFactor.Apply(25);
    DXBAR_TOOLBARINPOPUP, DXBAR_TOOLBAR, DXBAR_TABSGROUPSOVERLAPHEIGHT:
      Result := 1;
    DXBAR_BUTTONGROUPBORDERLEFT, DXBAR_BUTTONGROUPBORDERRIGHT:
      Result :=  2;
    DXBAR_BUTTONGROUPSPLITBUTTONSEPARATOR:
      Result := 3;
    DXBAR_BUTTONGROUP:
      Result := 3;
    DXBAR_LAUNCHBUTTONDEFAULTGLYPH:
      Result := ScaleFactorForTextures.Apply(Parts[FLaunchButtonDefaultGlyphs[0]].Size.cx);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTON,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINTAG,
    DXBAR_MENUEXTRAPANE_PINBUTTON:
      Result := ScaleFactorForTextures.Apply(Parts[FPinButtonGlyphs[False]].Size.cx) + cxMarginsWidth(GetPartContentOffsets(APart));
  else
    Result := 0;
  end;
end;

function TdxCustomRibbonSkin.GetSkinName: string;
begin
  Result := '';
end;

function TdxCustomRibbonSkin.IsInternalPainter: Boolean;
begin
  Result := TargetDPI <> dxDefaultDPI;
end;

procedure TdxCustomRibbonSkin.FlushCache;
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.UpdateBitsPerPixel;
var
  DC: HDC;
begin
  DC := GetDC(0);
  FLowColors := (GetDeviceCaps(DC, BITSPIXEL) <= 8) or dxRibbonUseLowColorsInHighContrastThemes and IsHighContrast;
  ReleaseDC(0, DC);
end;

procedure TdxCustomRibbonSkin.AddTwoStateElement(
  ABitmap: GpBitmap; var AParts; const R, F: TRect; ID: Integer;
  AInterpolationMode: Integer = InterpolationModeNearestNeighbor);
begin
  LoadElementParts(ABitmap, AParts, R, ID, F, [0, 1], [0, 1]);
  Parts[TTwoStateArray(AParts)[False]].InterpolationMode := AInterpolationMode;
  Parts[TTwoStateArray(AParts)[True]].InterpolationMode := AInterpolationMode;
end;

procedure TdxCustomRibbonSkin.InternalDrawFormBorders(DC: HDC; const R: TRect; const AIsActive, AIsRectangular: Boolean);

  function CreateFrameRegion(const R: TRect): TcxRegion;
  const
    CornerRadius = 9;
  begin
    Result := TcxRegion.CreateRoundCorners(R, CornerRadius, CornerRadius);
    if AIsRectangular then
      Result.Combine(TcxRegion.Create(R.Left + 1, R.Top + CornerRadius, R.Right - 1, R.Bottom - 1), roAdd);
  end;

const
  ColorMap: array[Boolean] of TColor = (clInactiveBorder, clActiveBorder);
var
  ARegion: TcxRegion;
begin
  if (Style < rs2013) or not IsHighContrastWhite then
    FillRectByColor(DC, R, clHighlightText)
  else
    FillRectByColor(DC, R, clInactiveBorder);
  ARegion := CreateFrameRegion(R);
  try
    if Style < rs2013 then
    begin
      FillRegionByColor(DC, ARegion.Handle, clBtnShadow);
      ARegion.Combine(CreateFrameRegion(cxRectInflate(R, -1, -1)), roSet);
      if Style = rs2007 then
      begin
        FillRegionByColor(DC, ARegion.Handle, ColorMap[AIsActive]);
        ARegion.Combine(CreateFrameRegion(cxRectInflate(R, -2, -2)), roSet);
      end;
      FillRegionByColor(DC, ARegion.Handle, clBtnFace);
    end;
  finally
    ARegion.Free;
  end;
end;

procedure TdxCustomRibbonSkin.InternalDrawGlyph(DC: HDC; const R: TRect; APart: Integer;
  AColor: TColor = clDefault; ANeedCorrection: Boolean = True; AAlignment: TAlignment = taCenter);
var
  AGlyph: TdxSkinnedRect;
  AGlyphSize: TSize;
  ARect: TRect;
begin
  if APart <> 0 then
  begin
    AGlyph := Parts[APart];
    AGlyphSize := ScaleFactorForTextures.Apply(AGlyph.Size);
    ARect := cxRectCenterVertically(R, AGlyphSize.cy);
    if ANeedCorrection then
      ARect := cxRectOffset(ARect, 0, 1);

    case AAlignment of
      taLeftJustify:
        begin
          ARect := cxRectOffset(ARect, ARect.Top - R.Top, 0);
          ARect.Right := ARect.Left + AGlyphSize.cx;
        end;

      taRightJustify:
        begin
          ARect := cxRectOffset(ARect, R.Top - ARect.Top, 0);
          ARect.Left := ARect.Right - AGlyphSize.cx;
        end;

    else
      ARect := cxRectCenterHorizontally(ARect, AGlyphSize.cx);
    end;
    AGlyph.DrawColored(DC, ARect, AColor, 255, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbonSkin.InternalDrawSeparator(
  DC: HDC; const R: TRect; AHorizontal: Boolean; AColor1, AColor2: TColor);
var
  R1: TRect;
begin
  if AHorizontal then
  begin
    R1 := cxRectCenterVertically(R, 1);
    dxGpFillRect(DC, R1, AColor2);
    dxGpFillRect(DC, cxRectOffset(R1, 0, 1), AColor1);
  end
  else
  begin
    R1 := cxRectCenterHorizontally(R, 1);
    dxGpFillRect(DC, R1, AColor1);
    dxGpFillRect(DC, cxRectOffset(R1, 1, 0), AColor2);
  end;
end;

procedure TdxCustomRibbonSkin.LoadCommonElements(ABitmap: GpBitmap);

  function GetTargetDPI: Integer;
  begin
    Result := dxDefaultDPI;
    if TargetDPI >= 144 then
      Result := 144;
    if TargetDPI >= 192 then
      Result := 192;
  end;

  function LoadScaledElement(const R, F: TRect; ID: Integer): Integer;
  begin
    Result := AddPart3x3(ABitmap, R, F, ID, '', InterpolationModeHighQualityBicubic);
    Parts[Result].Scale(GetTargetDPI, dxDefaultDPI);
  end;

begin
  FQATGlassAtTopLeft[True] := LoadScaledElement(cxRectBounds(0, 353, 16, 26), cxRect(0, 2, 2, 2), rspQATNonClientLeft1Vista);
  FQATGlassAtTopLeft[False] := LoadScaledElement(cxRectBounds(34, 353, 4, 26), cxRect(3, 2, 0, 1), rspQATNonClientLeft2Vista);
  FQATGlassAtTopRight := LoadScaledElement(cxRectBounds(16, 353, 18, 26), cxRect(0, 7, 15, 7), rspQATNonClientRightVista);

  LoadCommonPinButton(ABitmap);
  LoadCommonHelpButton(ABitmap);
  LoadCommonMenu(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadCommonHelpButton(ABitmap: GpBitmap);
var
  R: TRect;
begin
  if TargetDPI >= 192 then
    R := cxRectBounds(115, 353, 32, 32)
  else if TargetDPI >= 144 then
    R := cxRectBounds(90, 353, 24, 24)
  else
    R := cxRectBounds(73, 353, 16, 16);

  FHelpButton := AddPart1x1(ABitmap, R, rspHelpButton);
end;

procedure TdxCustomRibbonSkin.LoadCommonMenu(ABitmap: GpBitmap);
begin
  FMenuGlyph := AddPart3x3(ABitmap, cxRectBounds(14, 331, 3, 4), Rect(1, 1, 0, 1), rspMenuGlyph);
  FMenuContent := AddPart3x3(ABitmap, cxRectBounds(18, 331, 3, 4), Rect(0, 1, 1, 1), rspMenuContent);
  FMenuSeparatorHorz := AddPart1x1(ABitmap, cxRectBounds(17, 337, 4, 2), rspMenuSeparatorHorz);
  FMenuSeparatorVert := AddPart1x1(ABitmap, cxRectBounds(14, 336, 2, 2), rspMenuSeparatorVert);
  FDropDownBorder := AddPart3x3(ABitmap, cxRectBounds(28, 331, 8, 8), Rect(3, 3, 3, 3), rspDropDownBorder);
end;

procedure TdxCustomRibbonSkin.LoadCommonPinButton(ABitmap: GpBitmap);
var
  R: TRect;
begin
  if TargetDPI >= 192 then
    R := cxRectBounds(168, 319, 32, 32)
  else if TargetDPI >= 144 then
    R := cxRectBounds(176, 270, 24, 24)
  else
    R := cxRectBounds(160, 270, 16, 16);

  LoadElementParts(ABitmap, FPinButtonGlyphs, R, rspPinButtonGlyph, cxNullRect, [0, 1], [0, 1]);
end;

procedure TdxCustomRibbonSkin.LoadRibbonButtons(ABitmap: GpBitmap);
begin
  FApplicationMenuButton := AddPart3x3(ABitmap, cxRectBounds(0, 250, 6, 22), DefaultFixedSize, rspApplicationMenuButton);
  LoadElementParts(ABitmap, FEditButtons, cxRectBounds(0, 116, 12, 20), rspEditButtonNormal, DefaultFixedSize,
    [0, 1, 2, 3, 4, 5, 1], [DXBAR_NORMAL..DXBAR_DROPPEDDOWN, DXBAR_ACTIVEDISABLED]);
  LoadElementParts(ABitmap, FButtonGroup, cxRectBounds(73, 0, 3, 22), rspButtonGroupNormal, Rect(1, 2, 1, 2), [], []);
  LoadRibbonLaunchButton(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadRibbonForm(ABitmap: GpBitmap);
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.LoadRibbonElements(ABitmap: GpBitmap);
begin
  FScrollArrow := AddPart1x1(ABitmap, cxRectBounds(14, 245, 5, 3), rspScrollArrow);
  FScreenTip := AddPart3x3(ABitmap, cxRectBounds(66, 0, 6, 165), DefaultFixedSize, rspScreenTip);

  LoadRibbonTab(ABitmap);
  LoadRibbonButtons(ABitmap);
  LoadRibbonMenu(ABitmap);
  LoadRibbonQAT(ABitmap);
  LoadRibbonGallery(ABitmap);
  LoadRibbonForm(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadRibbonGallery(ABitmap: GpBitmap);

  procedure LoadRibbonGalleryCore(X, Y, WC, WV, H: Integer);
  begin
    FDropDownGalleryBottomSizeGrip := AddPart3x3(ABitmap, cxRectBounds(X, Y, WC, H),
      cxEmptyRect, rspDropDownGalleryBottomSizeGrip, '', InterpolationModeNearestNeighbor);
    FDropDownGalleryTopSizeGrip := AddPart3x3(ABitmap, cxRectBounds(X + WC + 1, Y, WC, H),
      cxEmptyRect, rspDropDownGalleryTopSizeGrip, '', InterpolationModeNearestNeighbor);
    FDropDownGalleryVerticalSizeGrip := AddPart3x3(ABitmap, cxRectBounds(X + 2 * (WC + 1), Y, WV, H),
      cxEmptyRect, rspDropDownGalleryVerticalSizeGrip);
  end;

begin
  if TargetDPI >= 192 then
    LoadRibbonGalleryCore(43, 450, 14, 36, 14)
  else if TargetDPI >= 144 then
    LoadRibbonGalleryCore(43, 439, 10, 27, 10)
  else
    LoadRibbonGalleryCore(46, 423, 7, 18, 7);
end;

procedure TdxCustomRibbonSkin.LoadRibbonLaunchButton(ABitmap: GpBitmap);

  procedure LoadLaunchButton(X, Y, S: Integer);
  begin
    LoadElementParts(ABitmap, FLaunchButtonDefaultGlyphs, cxRectBounds(X, Y, S, S), rspLaunchButtonDefaultGlyphNormal,
      cxNullRect, [0, 1, 0, 0, 0], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED],
      True, InterpolationModeNearestNeighbor);
  end;

begin
  if TargetDPI >= 192 then
    LoadLaunchButton(106, 350, 24)
  else if TargetDPI >= 144 then
    LoadLaunchButton(87, 350, 18)
  else
    LoadLaunchButton(34, 249, 12);
end;

procedure TdxCustomRibbonSkin.LoadRibbonMenu(ABitmap: GpBitmap);
begin
  FApplicationMenuBorder := AddPart3x3(ABitmap, cxRectBounds(48, 321, 8, 8), Rect(3, 3, 3, 3), rspApplicationMenuBorder);
  FApplicationMenuContentHeader := AddPart1x1(ABitmap, cxRectBounds(57, 325, 2, 14), rspApplicationMenuContentHeader);
  FApplicationMenuContentFooter := AddPart1x1(ABitmap, cxRectBounds(62, 323, 2, 25), rspApplicationMenuContentFooter);
  LoadRibbonMenuMarks(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadRibbonMenuMarks(ABitmap: GpBitmap);

  procedure LoadMarkTruncated(X, Y, W, H: Integer);
  begin
    LoadElementParts(ABitmap, FMarkTruncated, cxRectBounds(X, Y, W, H), rspMarkTruncatedNormal,
      cxEmptyRect, [0, 0, 1], [DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED], True);
  end;

begin
  FMenuMark := AddPart1x1(ABitmap, cxRectBounds(48, 277, 17, 17), rspMenuMark);

  if TargetDPI >= 192 then
    LoadMarkTruncated(34, 295, 16, 12)
  else if TargetDPI >= 144 then
    LoadMarkTruncated(34, 274, 12, 9)
  else
    LoadMarkTruncated(36, 234, 8, 6);
end;

procedure TdxCustomRibbonSkin.LoadRibbonQAT(ABitmap: GpBitmap);
begin
  LoadElementParts(ABitmap, FQATGroupButtonActive, cxRectBounds(0, 350, 22, 22), rspQATGroupButtonActive, DefaultFixedSize,
    [0, 3, 1, 1, 2, 2, 1], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED, DXBAR_DROPPEDDOWN, DXBAR_ACTIVEDISABLED]);
  LoadElementParts(ABitmap, FQATGroupButtonInactive, cxRectBounds(23, 350, 22, 22), rspQATGroupButtonInactive, DefaultFixedSize,
    [0, 3, 1, 1, 2, 2, 1], [DXBAR_NORMAL, DXBAR_DISABLED, DXBAR_HOT, DXBAR_ACTIVE, DXBAR_PRESSED, DXBAR_DROPPEDDOWN, DXBAR_ACTIVEDISABLED]);
  LoadRibbonQATBorders(ABitmap);
end;

procedure TdxCustomRibbonSkin.LoadRibbonQATBorders(ABitmap: GpBitmap);

  function GetTargetDPI: Integer;
  begin
    Result := dxDefaultDPI;
    if TargetDPI >= 144 then
      Result := 144;
    if TargetDPI >= 192 then
      Result := 192;
  end;

  procedure LoadScaledElement(var AParts: TTwoStateArray; const R, F: TRect; ID: Integer);
  var
    I: Boolean;
  begin
    AddTwoStateElement(ABitmap, AParts, R, F, ID);
    for I := Low(AParts) to High(AParts) do
      Parts[AParts[I]].Scale(GetTargetDPI, dxDefaultDPI);
  end;

begin
  FQATAtBottom := AddPart3x3(ABitmap, cxRectBounds(13, 209, 10, 26), cxRect(3, 3, 3, 3), rspQATAtBottom);
  LoadScaledElement(FQATAtTopLeft[True], cxRectBounds(93, 113, 15, 26), cxRect(13, 5, 0, 5), rspQATNonClientLeft1Active);
  LoadScaledElement(FQATAtTopLeft[False], cxRectBounds(125, 113, 7, 26), cxRect(2, 5, 0, 5), rspQATNonClientLeft2Active);
  LoadScaledElement(FQATAtTopRight, cxRectBounds(106, 113, 18, 26), cxRect(0, 5, 13, 5), rspQATNonClientRightActive);
end;

procedure TdxCustomRibbonSkin.LoadRibbonTab(ABitmap: GpBitmap);
begin
  FTabSeparator := AddPart1x1(ABitmap, cxRectBounds(42, 86, 1, 22), rspTabSeparator);
end;

procedure TdxCustomRibbonSkin.InitializeColorPalettes;
begin
  // do nothing
end;

procedure TdxCustomRibbonSkin.LoadCommonTexturesSet(AImage: TdxGPImage);
begin
  LoadBitmapFromStream('RIBBONCOMMON', AImage);
end;

procedure TdxCustomRibbonSkin.LoadSkin;
var
  AImage: TdxGPImage;
begin
  AImage := TdxGPImage.Create;
  try
    InitializeColorPalettes;
    LoadCommonTexturesSet(AImage);
    LoadCommonElements(AImage.Handle);
    LoadRibbonTexturesSet(AImage);
    LoadRibbonElements(AImage.Handle);
  finally
    AImage.Free;
  end;
end;

function TdxCustomRibbonSkin.GetStyle: TdxRibbonStyle;
begin
  Result := rs2007;
end;

function TdxCustomRibbonSkin.GetTexturesDPI: Integer;
begin
  Result := dxDefaultDPI;
end;

procedure TdxCustomRibbonSkin.CalculateArrowPoints(R: TRect; var P: TcxArrowPoints; AArrowDirection: TcxArrowDirection);
begin
  cxLookAndFeelPaintersManager.GetPainter(lfsStandard).CalculateArrowPoints(R, P, AArrowDirection, False, GetArrowSize);
  if AArrowDirection in [adUp] then
    Dec(P[1].Y);
  if AArrowDirection in [adUp, adDown] then
  begin
    if LowColors then
      Inc(P[2].X)
    else
    begin
      Dec(P[0].X);
      Dec(P[1].X);
    end;
  end;
end;

procedure TdxCustomRibbonSkin.DrawArrow(DC: HDC; const R: TRect; AArrowDirection: TcxArrowDirection; AColor: TColor);
var
  APoints: TcxArrowPoints;
  ARegion: TcxRegion;
begin
  CalculateArrowPoints(R, APoints, AArrowDirection);
  cxPaintCanvas.BeginPaint(DC);
  try
    ARegion := TcxRegion.Create(CreatePolygonRgn(APoints, 3, WINDING));
    try
      cxPaintCanvas.SetClipRegion(ARegion, roIntersect, False);
      if LowColors then
        cxPaintCanvas.FillRect(ARegion.BoundsRect, AColor)
      else
        dxGpFillRect(cxPaintCanvas.Handle, ARegion.BoundsRect, AColor);
    finally
      ARegion.Free;
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxCustomRibbonSkin.DrawDropDownGalleryVerticalSizeGrip(DC: HDC; const R: TRect);
var
  ARect: TRect;
begin
  ARect := Rect(0, R.Top, 0, R.Bottom);
  ARect.Right := DropDownGalleryVerticalSizeGripBitmapSize.cx *
    cxRectHeight(ARect) div DropDownGalleryVerticalSizeGripBitmapSize.cy;
  OffsetRect(ARect, (cxRectWidth(R) - cxRectWidth(ARect)) div 2, 0);
  if LowColors then
    Parts[FDropDownGalleryVerticalSizeGrip].DrawColored(DC, ARect, clWindowText)
  else
    Parts[FDropDownGalleryVerticalSizeGrip].Draw(DC, ARect);
end;

procedure TdxCustomRibbonSkin.DrawPart(DC: HDC; const R: TRect; AState: Integer);
begin
  case AState of
    DXBAR_HOT, DXBAR_CHECKED, DXBAR_HOTCHECK, DXBAR_ACTIVE:
      DrawFrame(DC, R, clHighlight, clHighlightText);
    DXBAR_PRESSED, DXBAR_DROPPEDDOWN:
      DrawFrame(DC, R, clHighlight, clBtnShadow)
    else
      DrawFrame(DC, R, clBtnFace, clNone);
  end
end;

procedure TdxCustomRibbonSkin.DrawPart(const AParts: TStatesArray; DC: HDC; const R: TRect; AState: Integer);
begin
  if AParts[AState] <> 0 then
  begin
    if LowColors then
      DrawPart(DC, R, AState)
    else
      Parts[AParts[AState]].Draw(DC, R, 255, clDefault, UseRightToLeftAlignment);
  end;
end;

procedure TdxCustomRibbonSkin.DrawTabAreaButton(DC: HDC; const R: TRect; AState: TcxButtonState);
var
  APart: Integer;
begin
  case AState of
    cxbsHot:
      APart := DXBAR_HOT;
    cxbsPressed:
      APart := DXBAR_PRESSED;
  else
    APart := DXBAR_NORMAL;
  end;
  if APart <> DXBAR_NORMAL then
    DrawSmallButton(DC, R, APart);
end;

procedure TdxCustomRibbonSkin.DrawTabAreaButtonDropButtonArrowPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawTabAreaButton(DC, R, RibbonStateToButtonState(AState));
end;

procedure TdxCustomRibbonSkin.DrawTabAreaButtonDropButtonMainPart(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawTabAreaButton(DC, R, RibbonStateToButtonState(AState));
end;

procedure TdxCustomRibbonSkin.DrawTabAreaMarkArrow(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawMarkArrow(DC, R, AState);
end;

procedure TdxCustomRibbonSkin.DrawTabAreaMarkTruncated(DC: HDC; const R: TRect; AState: Integer);
begin
  DrawMarkTruncated(DC, R, AState);
end;

function TdxCustomRibbonSkin.GetTabAreaButtonColorPalette(AState: Integer): IdxColorPalette;
begin
  Result := GetColorPalette(FColorPaletteTabAreaToolbar, AState);
end;

procedure TdxCustomRibbonSkin.GetApplicationMenuContentColors(var AInnerBorderColor, AOuterBorderColor, ASideColor: TColor);
begin
  AInnerBorderColor := clMenuText;
  AOuterBorderColor := clMenu;
  ASideColor := clMenu;
end;

function TdxCustomRibbonSkin.GetArrowSize: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxCustomRibbonSkin.DoGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  Result := DoGetPartColorCore(APart, AState);
end;

function TdxCustomRibbonSkin.DoGetPartColorCore(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    DXBAR_SCREENTIP_FOOTERLINE:
      Result := $DDBB9E;
    DXBAR_DATENAVIGATOR_HEADER:
      Result := $DAD5D2;
    DXBAR_SEPARATOR_BACKGROUND:
      Result := $EFE7DE;
    rspRibbonBottomEdge:
      Result := $F3E2D5;
    DXBAR_EDIT_BACKGROUND:
      Result := clWindow;
    DXBAR_KEYTIP_TEXTCOLOR:
      Result := clBtnText;
    DXBAR_INRIBBONGALLERY_BACKGROUND:
      Result := clBtnFace;
    DXBAR_INRIBBONGALLERY_BORDER:
      Result := clBtnText;
    DXBAR_GALLERYFILTERBANDTEXT:
      Result := clBtnText;
    DXBAR_DROPDOWNGALLERY:
      Result := clBtnFace;
    DXBAR_APPLICATIONMENUBUTTON:
      Result := GetPartColor(DXBAR_BUTTONITEMTEXT, AState);
    DXBAR_INRIBBONGALLERYITEM_TEXTCOLOR:
      Result := GetPartColor(DXBAR_BUTTONITEMTEXT, AState);
    DXBAR_DROPDOWNGALLERYITEM_TEXTCOLOR:
      Result := GetPartColor(DXBAR_MENUITEMTEXT, AState);
    DXBAR_GALLERYGROUPHEADERTEXT:
      Result := GetPartColor(DXBAR_MENUITEMTEXT);
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR:
      Result := GetPartColor(DXBAR_MENUBUTTONITEMTEXT, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR:
      Result := GetPartColor(DXBAR_MENUITEMTEXT, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR:
      Result := clGrayText;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER_TEXTCOLOR:
      Result := GetPartColor(DXBAR_GALLERYGROUPHEADERTEXT, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_BORDER:
      Result := GetPartColor(DXBAR_INRIBBONGALLERY_BORDER, AState);
    DXBAR_SEPARATOR_TEXTCOLOR,
    DXBAR_MENUEXTRAPANE_BUTTON_TEXTCOLOR,
    DXBAR_MENUEXTRAPANE_HEADER_TEXTCOLOR:
      Result := GetPartColor(DXBAR_MENUITEMTEXT, AState);
    DXBAR_RADIALMENUACCENT:
      Result := GetPartColor(rspRibbonBackground);
    DXBAR_BACKSTAGEVIEW_TEXTCOLOR:
      Result := clBtnText;
    DXBAR_RADIALMENUBACKGROUND:
      Result := clWhite;

    rspGroupScrollArrow:
      Result := clBlack;
    rspApplicationButton:
      Result := clBtnText;
    rspContextText:
      Result := GetPartColor(rspFormCaptionText);
    rspContextTabHeaderText:
      Result := GetPartColor(rspTabHeaderText, AState);
    rspContextTextShadow, rspContextTextOnGlassShadow:
      Result := clNone;
    rtatpEditBackground:
      Result := GetPartColor(DXBAR_EDIT_BACKGROUND, AState);
    rtatpEditText:
      Result := GetPartColor(DXBAR_EDIT_TEXTCOLOR, AState);
    rfspRibbonForm:
      Result := clBtnShadow;
    rspTabGroupHeaderText:
      Result := clBtnText;

    rspContextTextOnGlass,
    rspTabHeaderText:
      case AState of
        DXBAR_NORMAL:
          Result := clBtnText;
        DXBAR_DISABLED:
          Result := clGrayText;
      else
        Result := clHighlightText;
      end;

    DXBAR_ARROWDOWN:
      case AState of
        DXBAR_NORMAL, DXBAR_ACTIVE:
          Result := clBtnText;
        DXBAR_DISABLED:
          Result := clGrayText;
      else
        Result := clHighlightText;
      end;

    DXRIBBON_TAT_SMALLBUTTON:
      begin
        if AState = DXBAR_ACTIVE then
          AState := DXBAR_NORMAL;
        if AState in [DXBAR_NORMAL, DXBAR_DISABLED] then
          Result := GetPartColor(rspTabHeaderText, AState)
        else
          Result := GetPartColor(DXBAR_BUTTONITEMTEXT, AState);
      end;

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH:
      if AState in [DXBAR_HOT, DXBAR_HOTCHECK] then
        Result := clHighlightText
      else
        Result := clMenuText;

    DXBAR_EDIT_TEXTCOLOR:
      if AState = DXBAR_DISABLED then
        Result := clGrayText
      else
        Result := clMenuText;

    rspFormCaptionText, rspDocumentNameText:
      if AState = DXBAR_NORMAL then
        Result := clCaptionText
      else
        Result := clInactiveCaptionText;

    DXBAR_MINITOOLBAR_BACKGROUND:
      Result := clWindow;

    DXBAR_MENUEDITSEPARATOR:
      case AState of
        DXBAR_ACTIVE:
          Result := clBtnText;
        DXBAR_ACTIVEDISABLED:
          Result := clBtnHighlight;
      else
        Result := clDefault;
      end;

    DXBAR_EDIT_BORDER, DXBAR_EDIT_BUTTON_BORDER:
      case AState of
        DXBAR_NORMAL, DXBAR_DISABLED:
          Result := clBtnShadow;
      else
        Result := clBtnText;
      end;

    DXBAR_ITEMTEXT, rspTabGroupText, rspStatusBarText:
      case AState of
        DXBAR_NORMAL:
          Result := clBtnText;
        DXBAR_DISABLED:
          Result := clGrayText;
      else
        Result := clHighlightText;
      end;

    DXBAR_MENUITEMTEXT:
      case AState of
        DXBAR_NORMAL, DXBAR_CHECKED:
          Result := clMenuText;
        DXBAR_DISABLED:
          Result := clGrayText;
      else
        Result := clHighlightText;
      end;

  else
    Result := clDefault;
  end;
end;

function TdxCustomRibbonSkin.DoGetPartLowColor(APart: Integer; AState: Integer = 0): TColor;
begin
  case APart of
    rspRibbonBottomEdge:
      Result := clBtnHighlight;
    DXBAR_SCREENTIP_TITLE:
      Result := clInfoText;
    DXBAR_RADIALMENUBACKGROUND:
      Result := clBtnShadow;
    DXBAR_SCREENTIP_FOOTERLINE:
      Result := clBtnShadow;
    DXBAR_DATENAVIGATOR_HEADER:
      Result := clWindow;
    DXBAR_SEPARATOR_BACKGROUND:
      Result := clBtnShadow;
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMDESCRIPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMCAPTIONTEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR,
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR:
      case AState of
        DXBAR_NORMAL:
          Result := clMenuText;
        DXBAR_DISABLED, DXBAR_ACTIVEDISABLED:
          Result := clGrayText;
      else
        Result := clHighlightText;
      end;
  else
    Result := DoGetPartColorCore(APart, AState);
    if Result = clDefault then
      Result := clMenu;
  end;
end;

function TdxCustomRibbonSkin.GetColorPalette(ASet: IdxRibbonSkinColorPaletteSet; AState: Integer): IdxColorPalette;
begin
  if ASet <> nil then
    Result := ASet.Get(AState)
  else
    Result := nil;
end;

function TdxCustomRibbonSkin.GetFormPaintData: IdxRibbonFormPaintData;
begin
  Result := PaintData.GetRibbonFormPaintData;
end;

{ TdxRibbonSkinsManager }

constructor TdxRibbonSkinsManager.Create;
begin
  inherited Create;
  FList := TcxObjectList.Create;
end;

destructor TdxRibbonSkinsManager.Destroy;
begin
  ReleaseDefault;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxRibbonSkinsManager.Add(ASkin: TdxCustomRibbonSkin): Integer;
begin
  if CheckGdiPlus and (ASkin <> nil) then
    Result := FList.Add(ASkin)
  else
    Result := -1;
end;

function TdxRibbonSkinsManager.Contains(const AName: string; AStyle: TdxRibbonStyle): Boolean;
var
  ASkin: TdxCustomRibbonSkin;
  I: Integer;
begin
  for I := 0 to SkinCount - 1 do
  begin
    ASkin := Skins[I];
    if (ASkin.Style = AStyle) and SameText(ASkin.Name, AName) then
      Exit(True);
  end;
  Result := False;
end;

function TdxRibbonSkinsManager.GetMostSuitable(const AName: string; AStyle: TdxRibbonStyle; ATargetDPI: Integer): TdxCustomRibbonSkin;

  function FindCore(const AName: string): TdxCustomRibbonSkin;
  var
    ASkin: TdxCustomRibbonSkin;
    I: Integer;
  begin
    Result := nil;
    for I := 0 to SkinCount - 1 do
    begin
      ASkin := Skins[I];
      if (ASkin.Style = AStyle) and ((AName = '') or SameText(ASkin.Name, AName)) then
      begin
        if (Result = nil) or (Abs(ASkin.TargetDPI - ATargetDPI) < Abs(Result.TargetDPI - ATargetDPI)) then
          Result := ASkin;
      end;
    end;
  end;

begin
  Result := FindCore(AName);
  if (Result = nil) and Assigned(GetSkinPainterProc) and (GetSkinPainterProc(AName) <> nil) then
    Result := FindCore(AName);
  if Result = nil then
    Result := FindCore('');
  if Result = nil then
    Result := Skins[0]
  else
    if Result.TargetDPI <> ATargetDPI then
      Result := Skins[Add(Result.Clone(ATargetDPI))];
end;

function TdxRibbonSkinsManager.Remove(ASkin: TdxCustomRibbonSkin): Boolean;
begin
  ReleaseDefault;
  Result := FList.FreeAndRemove(ASkin) >= 0;
end;

function TdxRibbonSkinsManager.Remove(ASkinClass: TdxCustomRibbonSkinClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  ReleaseDefault;
  for I := SkinCount - 1 downto 0 do
    if Skins[I].ClassType = ASkinClass then
    begin
      FList.FreeAndDelete(I);
      Result := True;
    end;
end;

function TdxRibbonSkinsManager.GetDefault: TdxCustomRibbonSkin;
begin
  if FDefault = nil then
  begin
    FDefault := Skins[0];
    FDefault.AddReference;
  end;
  Result := FDefault;
end;

function TdxRibbonSkinsManager.GetSkin(Index: Integer): TdxCustomRibbonSkin;
begin
  Result := TdxCustomRibbonSkin(FList[Index]);
end;

function TdxRibbonSkinsManager.GetSkinCount: Integer;
begin
  Result := FList.Count;
end;

procedure TdxRibbonSkinsManager.ReleaseDefault;
begin
  if FDefault <> nil then
  begin
    FDefault.RemoveReference;
    FDefault := nil;
  end;
end;

{ DestroySkins }

procedure DestroySkins;
begin
  FreeAndNil(FSkinsManager);
end;

initialization
  dxUnitsLoader.AddUnit(nil, @DestroySkins);

finalization
  dxUnitsLoader.RemoveUnit(@DestroySkins);
end.

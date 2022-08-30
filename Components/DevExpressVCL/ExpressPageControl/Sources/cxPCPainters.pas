{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPageControl                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPAGECONTROL AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxPCPainters;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Controls, Graphics, SysUtils, cxGraphics, cxLookAndFeels,
  cxPC, dxCore, dxCoreGraphics, cxLookAndFeelPainters, dxThemeManager;

const
  cxPCEmptyWOffset: TcxPCWOffset = (Left: 0; Right: 0);

  cxPCTabsStyle = 1;
  cxPCButtonsStyle = 2;
  cxPCFlatButtonsStyle = 3;
  cxPCExtraFlatStyle = 5;
  cxPCUltraFlatStyle = 6;
  cxPCFlatStyle = 7;
  cxPCOffice11Style = 8;
  cxPCSlantedStyle = 9;
  cxPCOneNoteStyle = 10;
  cxPCSkinStyle = 11;

  StandardPainterTabControlFrameBorderWidth = 2;

type

  TcxPCTabsDelimiterOffsets = record
    Top, Bottom: Integer;
  end;

  TcxPCArrow = (aTop, aBottom, aLeft, aRight);

  TcxPCNavigatorButtonContentParameters = record
    BrushColor: TColor;
    Color: TColor;
    Enabled: Boolean;
    LiteStyle: Boolean;
  end;

  TLinePosition = (lpL, lpLT, lpT, lpRT, lpR, lpRB, lpB, lpLB);
  TLinePositions = array of TLinePosition;

  { TcxPCStandardPainter }

  TcxPCStandardPainter = class(TcxPCCustomPainter)
  private
    FButtonsRect: TRect;
    function GetButtonsWidth: Integer;
  protected
    function CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer; override;
    procedure CorrectTabNormalWidth(var AValue: Integer); override;
    procedure DoDrawTabButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); override;
    procedure DoDrawTabCloseButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); override;
    procedure DrawButtonFrameAndBackround(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); virtual;
    function DoGetCloseButtonSize: TSize; override;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; override;
    function GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; override;
    function GetFrameWidth: Integer; override;
    function GetGoDialogPosition(ASize: TSize): TPoint; override;
    function GetHeaderButtonHeightCorrection: Integer;
    function GetImageTextDistance(ATabVisibleIndex: Integer): Integer; override;
    function GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetMinTabSelectionDistance: TcxPCDistance; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabsNormalDistance: TcxPCDistance; override;
    function GetTabsPosition: TcxPCTabsPosition; override;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function InternalCalculateTabNormalWidth(ATabVisibleIndex: Integer): Integer; virtual;
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; override;
    function NeedButtonContentPositionCentered: Boolean; virtual;
    procedure PaintButtonsRegion(ACanvas: TcxCanvas); override;
    procedure PaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); override;
    procedure RepaintButtonsRegion; override;
    procedure RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged); override;

    procedure CalculateButtonContentParameters(AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState; out AParameters: TcxPCNavigatorButtonContentParameters); virtual;
    procedure CalculateButtonsRect; virtual;
    procedure CalculateButtonsRegion; virtual;
    function CalculateButtonsRegionWidth: Integer; virtual;
    procedure CorrectTabHeightForImage(var AHeight: Integer);
    procedure DoPaintButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCCustomHeaderButtonViewInfo); virtual;
    procedure DrawButtonContent(ACanvas: TcxCanvas; AButton: TcxPCNavigatorButton;
      const AParameters: TcxPCNavigatorButtonContentParameters;
      AContentRectLeftTopCorner: TPoint);
    function Get3DButtonContentPosition(AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState): TPoint;
    function GetButtonArrow(AButton: TcxPCNavigatorButton): TcxPCArrow;
    function GetButtonCenteredContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState): TPoint;
    function GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor; virtual;
    function GetButtonContentColor(AButtonState: TcxPCNavigatorButtonState): TColor; virtual;
    function GetButtonContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint; virtual;
    function DoGetButtonHeight: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function DoGetDefaultTabNormalHeight: Integer; virtual;
    function GetDefaultTabNormalHeight: Integer; virtual;
    function GetDefaultTabNormalHTextOffset: Integer; virtual;
    function GetDefaultTabNormalWidth: Integer; virtual;
    function GetDrawFrameRect: TRect; virtual;
    function GetPageFrameRect: TRect; override;
    function GetMinFrameRectSize: Integer; virtual;
    function GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer; virtual;
    function GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer; virtual;
    function GetTabNormalWidth(ATabVisibleIndex: Integer): Integer; virtual;
    function GetTabsRectOffset: TRect; virtual;
    function GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer; virtual;
    function InternalCalculateTabNormalHeight: Integer; virtual;
    procedure InternalDrawEdge(ACanvas: TcxCanvas; const ARect: TRect; ASunken: Boolean; AThinFrame: Boolean = False);
    procedure InternalDrawFocusRect(ACanvas: TcxCanvas; TabVisibleIndex: Integer; R: TRect); virtual;
    procedure InternalPaintDragImage(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure InternalPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton); override;
    procedure PaintButtonBackground(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); virtual;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect;
      AButtonState: TcxPCNavigatorButtonState); virtual;
    procedure PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo); virtual;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    function GetRightToLeftDependentButtonType(AButton: TcxPCNavigatorButton): TcxPCNavigatorButton;
    procedure PaintFrameBorder(ACanvas: TcxCanvas; R: TRect); virtual;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual; abstract;
    property ButtonsRect: TRect read FButtonsRect;
  public
    function CalculateTabNormalHeight: Integer; override;
    class function IsMainTabBoundWithClient: boolean; override;
    class function IsMultiSelectionAccepted: boolean; override;
    class function IsStandardStyle: Boolean; override;
    class function IsTabPressable: Boolean; override;
  end;

  { TcxPCButtonedPainter }

  TcxPCButtonedPainter = class(TcxPCStandardPainter)
  strict private
    MainTabBrushBitmap: TBitmap;

    procedure CorrectContentWOffset(ATabVisibleIndex: Integer; var AOffset: TcxPCWOffset);
  protected
    function GetButtonContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint; override;
    function GetButtonsRegionHOffset: Integer; override;
    function GetButtonsRegionWOffset: Integer; override;
    function GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabsContainerOffsets: TRect; override;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    procedure DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    function InternalGetPageFrameRectOffset: TRect; override;
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; override;
    function UseLookAndFeelTabButton: Boolean; override;
  public
    constructor Create(AViewInfo: TcxCustomTabControlViewInfo); override;
    destructor Destroy; override;
  end;

  { TcxPCTabsPainter }

  TcxPCTabsPainter = class(TcxPCStandardPainter)
  protected
    function GetTabCorrection(ATabVisibleIndex: Integer): TRect; override;
    procedure DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet); override;
    function GetBorderWidths: TRect; override;
    function GetButtonContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint; override;
    function DoGetButtonHeight: Integer; override;
    procedure GetButtonNativePartAndState(AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton;
      out APartId, AStateId: Integer; out AThemeObjectType: TdxThemedObjectType);
    function GetButtonsRegionHOffset: Integer; override;
    function GetButtonsRegionWOffset: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect; override;
    function GetNativeButtonHeight: Integer; virtual;
    function GetPageClientRectOffset: TRect; override;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; override;
    function GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; override;
    function GetHeaderButtonGlyphOffset: TRect; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; override;
    function GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabFocusRect(const ATabBounds: TRect): TRect; virtual;
    procedure GetTabNativePartAndState(ATabVisibleIndex: Integer; out PartId, StateId: Integer); override;
    function GetTabsContainerOffsets: TRect; override;
    function GetTabsPosition: TcxPCTabsPosition; override;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    procedure DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    function InternalCalculateTabNormalHeight: Integer; override;
    function InternalGetPageFrameRectOffset: TRect; override;
    procedure InternalPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure InvalidateTabRect(ATabVisibleIndex: Integer); override;
    function IsNativePainting: Boolean; override;
    function PtInTab(TabVisibleIndex: Integer; X, Y: Integer): Boolean; override;
    function NeedShowFrame: Boolean; override;
    procedure PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton); override;
    procedure PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo); override;
    procedure PaintNativeButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton; ADrawBackground: Boolean);
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); override;
    procedure PaintNativeTabBackground(DC: HDC; ATabVisibleIndex: Integer; const ABounds: TRect); virtual;
    procedure PrepareTabBackground(ABitmap: TcxBitmap; ATabVisibleIndex: Integer); virtual;
    procedure PrepareTabBitmapBackground(ABitmap: TcxBitmap; const ARect: TRect; ATabViewInfo: TcxTabViewInfo); virtual;
    procedure RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged); override;
    function GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; override;
    function GetMinFrameRectSize: Integer; override;
    function GetNativeButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; virtual;
    procedure GetTabCornersColor(ATabVisibleIndex: Integer; out AColor1, AColor2: TColor); virtual;
    procedure PaintTabCorners(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure InternalPaintFrame(ACanvas: TcxCanvas); virtual;
    function UseLookAndFeelTabButton: Boolean; override;
  public
    procedure PaintPageClientArea(ACanvas: TcxCanvas); override;

    class function GetStandardStyle: TcxPCStandardStyle; override;
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
    class function IsDefault(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCButtonsPainter }

  TcxPCButtonsPainter = class(TcxPCButtonedPainter)
  protected
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; override;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
  public
    class function GetStandardStyle: TcxPCStandardStyle; override;
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCFlatButtonsPainter }

  TcxPCFlatButtonsPainter = class(TcxPCButtonedPainter)
  protected
    procedure PaintTab(ACanvas: TcxCanvas; TabVisibleIndex: Integer); override;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
  public
    class function GetStandardStyle: TcxPCStandardStyle; override;
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCExtraFlatPainter }

  TcxPCExtraFlatPainter = class(TcxPCStandardPainter)
  protected
    procedure CalculateButtonContentParameters(AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState; out AParameters: TcxPCNavigatorButtonContentParameters); override;
    function CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer; override;
    procedure CorrectTabNormalWidth(var AValue: Integer); override;
    function GetTabCorrection(ATabVisibleIndex: Integer): TRect; override;
    function GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function GetButtonContentColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function GetButtonContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState): TPoint; override;
    function DoGetButtonHeight: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect; override;
    function GetPageClientRectOffset: TRect; override;
    function GetDefaultClientColor: TColor; override;
    function DoGetDefaultTabNormalHeight: Integer; override;
    function GetDefaultTabNormalHTextOffset: Integer; override;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; override;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; override;
    function GetFrameWidth: Integer; override;
    function GetFreeSpaceColor: TColor; override;
    function GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer; override;
    function GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTabsRowsDelimiterWidth: Integer; virtual;
    function GetTabsNormalDistance: TcxPCDistance; override;
    function GetTabsPosition: TcxPCTabsPosition; override;
    function GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function InternalCalculateTabNormalHeight: Integer; override;
    function InternalGetPageFrameRectOffset: TRect; override;
    function IsPaintHeadersAreaFirst: Boolean; override;
    function IsTabsRectVisible(ACanvas: TcxCanvas): Boolean; override;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect;
      AButtonState: TcxPCNavigatorButtonState); override;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); override;
    procedure RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged); override;
    function GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; override;
    function GetButtonsRegionHOffset: Integer; override;
    function GetButtonsRegionWOffset: Integer; override;
    function GetFocusRect: TRect; virtual;
    function GetTabsDelimiterOffsets: TcxPCTabsDelimiterOffsets; virtual;
    procedure DrawTabBackground(ACanvas: TcxCanvas; ARect: TRect; ATabVisibleIndex: Integer); virtual;
    procedure DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure ExcludeUnderLine(var R: TRect); virtual;
    function GetMainTabRowUnderlineColor: TColor; virtual;
    function GetMainTabRowUnderlineRect: TRect; virtual;
    function GetTabBackgroundRect(ATabVisibleIndex: Integer; AForNormalState: Boolean): TRect; virtual;
    function GetTabsDelimiterWidth: Integer; virtual;
    function GetTabsRowColor: TColor; virtual;
    function GetTabsRowRect(ARowIndex: Integer): TRect;
    procedure InternalPaintFrame(ACanvas: TcxCanvas; ALeftTopColor, ARightBottomColor: TColor);
    procedure InternalPaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect; ALightColor, ADarkColor: TColor);
    procedure InternalPaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; AColors: array of TColor);
    function IsMainTabRow(AVisibleRow: Integer): Boolean;
    function NeedShowTabsRegionFrame: Boolean; virtual;
    procedure PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect); virtual;
    procedure PaintMainTabRowUnderline(ACanvas: TcxCanvas);
    procedure PaintTabsDelimiter(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer); virtual;
  public
    function CalculateTabNormalHeight: Integer; override;
    procedure PaintPageClientArea(ACanvas: TcxCanvas); override;

    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
    class function IsMainTabBoundWithClient: boolean; override;
    class function IsMultiSelectionAccepted: boolean; override;
    class function IsTabPressable: Boolean; override;
  end;

  { TcxPCUltraFlatPainter }

  TcxPCUltraFlatPainter = class(TcxPCExtraFlatPainter)
  protected
    procedure CalculateButtonContentParameters(AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState; out AParameters: TcxPCNavigatorButtonContentParameters); override;
    function GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function DoGetButtonHeight: Integer; override;
    function GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; override;
    function GetButtonsRegionHOffset: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetFocusRect: TRect; override;
    function NeedButtonContentPositionCentered: Boolean; override;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState); override;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect); override;
    procedure PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer); override;
  public
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCFlatPainter }

  TcxPCFlatPainter = class(TcxPCTabsPainter)
  protected
    function GetButtonContentPosition(const ARect: TRect; AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint; override;
    function DoGetButtonHeight: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetFrameWidth: Integer; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion; override;
    function IsNativePainting: Boolean; override;
    function NeedShowFrame: Boolean; override;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState); override;
    procedure PaintFrameBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
  public
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCOffice11Painter }

  TcxPCOffice11Painter = class(TcxPCUltraFlatPainter)
  strict private
    function GetSelectedColor1: TColor;
    function GetSelectedColor2: TColor;
    function IsGradientClientArea: Boolean;
    function NeedShowMainTabOppositeRowLine: Boolean;
    procedure PaintMainTabOppositeRowLine(ACanvas: TcxCanvas);
  protected
    class function GetFrameColor: TColor; virtual;
    procedure DrawTabBackground(ACanvas: TcxCanvas; ARect: TRect; ATabVisibleIndex: Integer); override;
    procedure ExcludeUnderLine(var R: TRect); override;
    procedure FillPageClientRect(ACanvas: TcxCanvas); override;
    function GetButtonContentColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function GetDefaultClientColor: TColor; override;
    function GetPageClientRectOffset: TRect; override;
    function GetFocusRect: TRect; override;
    function GetFreeSpaceColor: TColor; override;
    function GetMainTabRowUnderlineColor: TColor; override;
    function GetMainTabRowUnderlineRect: TRect; override;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; override;
    function GetTabsDelimiterOffsets: TcxPCTabsDelimiterOffsets; override;
    function GetTabsDelimiterWidth: Integer; override;
    function GetTabsRowsDelimiterWidth: Integer; override;
    function IsTabTransparent(ATabVisibleIndex: Integer): Boolean; override;
    function NeedRedrawOnResize: Boolean; override;
    function NeedShowTabsRegionFrame: Boolean; override;
    procedure PaintButtonBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxPCNavigatorButtonState); override;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState); override;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect); override;
    procedure PaintTabsDelimiter(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer); override;
    procedure PrepareDrawTabContentBitmapBackground(ABitmap: TcxBitmap; const ABitmapPos: TPoint; ATabVisibleIndex: Integer); override;
    procedure DrawTabContentBackground(ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer); override;
    procedure DrawGradientBackground(ACanvas: TcxCanvas; ARect: TRect; ATabVisibleIndex: Integer; AHorizontal, AInverse: Boolean);
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function UseLookAndFeelTabButton: Boolean; override;
  public
    procedure PaintPageClientArea(ACanvas: TcxCanvas); override;

    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCSlantedPainter }

  TcxPCSlantedPainter = class(TcxPCStandardPainter)
  strict private
    FCutValue: Integer;

    function GetTabsLineRect(ATabIndexInterval: TcxPCIndexInterval; AFullRect: Boolean): TRect;
    function GetTabUnderlineRect(ATabViewInfo: TcxTabViewInfo): TRect;
  protected
    function AlwaysColoredTabs: Boolean; override;
    class function AllowMultiLineTabCaptions: Boolean; override;
    procedure CalculateButtonContentParameters(AButton: TcxPCNavigatorButton;
      AState: TcxPCNavigatorButtonState; out AParameters: TcxPCNavigatorButtonContentParameters); override;
    function CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer; override;
    procedure CorrectTabNormalWidth(var AValue: Integer); override;
    function GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function GetButtonContentColor(AButtonState: TcxPCNavigatorButtonState): TColor; override;
    function DoGetButtonHeight: Integer; override;
    function GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; override;
    function GetButtonsRegionFromTabsOffset: Integer; override;
    function GetButtonsRegionHOffset: Integer; override;
    function GetButtonsRegionWOffset: Integer; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetClientColor: TColor; override;
    function DoGetCloseButtonSize: TSize; override;
    function GetCutSideOffset: Integer;
    function GetPageClientRectOffset: TRect; override;
    function DoGetDefaultTabNormalHeight: Integer; override;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; override;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; override;
    function GetFillClientRect: TRect; override;
    function GetFrameWidth: Integer; override;
    function GetImageTextDistance(ATabVisibleIndex: Integer): Integer; override;
    function GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; override;
    function GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer; override;
    function GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTabNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTabsContainerOffsets: TRect; override;
    function GetTabsNormalDistance: TcxPCDistance; override;
    function GetTabsRectOffset: TRect; override;
    function GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    procedure DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure Init; override;
    function InternalCalculateTabNormalHeight: Integer; override;
    function PtInTab(TabVisibleIndex: Integer; X, Y: Integer): Boolean; override;
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; override;
    function IsCloseButtonOnSlantedSide(ATabVisibleIndex: Integer): Boolean; virtual;
    procedure PaintButtonFrame(ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState); override;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); override;
    procedure PrepareDrawTabContentBitmapBackground(ABitmap: TcxBitmap; const ABitmapPos: TPoint; ATabVisibleIndex: Integer); override;
    procedure DrawTabContentBackground(ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer); override;

    function CanLightMainTab: Boolean; virtual;
    function DirectionalGetFigureRegion(const R: TRect; APoints: array of TPoint;
      ALinePositions: array of TLinePosition; ATabPositon: TcxTabPosition; AForContent: Boolean): TcxRegion;
    procedure DrawBackground(ACanvas: TcxCanvas; R: TRect; ATabVisibleIndex: Integer; AHorizontalGradient, AInverseGradient: Boolean); virtual;
    procedure DrawTabUnderline(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure GetBackgroundGradientColors(ATabVisibleIndex: Integer; out AColor1, AColor2: TColor); virtual;
    function GetCutValue: Integer;
    function GetGeometricalMinTabWidth: Integer; virtual;
    function GetFrameColor: TColor; virtual;
    function GetSlantedSides: TcxTabSlantPositions; virtual;
    procedure GetTabFramePolyline(ATabVisibleIndex: Integer; out APoints: TPoints; out ALinePositions: TLinePositions);
    function GetTabImageSize: TSize; virtual;
    function GetVerticalTextIndent: Integer; virtual;
    function InternalGetCutValue: Integer; virtual;
    function InternalGetTabClipRegion(ACanvas: TcxCanvas; ATabVisibleIndex: Integer; AForContent: Boolean): TcxRegion; virtual;
    procedure PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer; const R: TRect); virtual;
    procedure PaintTabStateMark(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;

    property SlantedSides: TcxTabSlantPositions read GetSlantedSides;
  public
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

  { TcxPCOneNotePainter }

  TcxPCOneNotePainter = class(TcxPCSlantedPainter)
  protected
    class function AllowRotate: Boolean; override;
    function CanLightMainTab: Boolean; override;
    procedure DrawTabUnderline(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
    procedure GetBackgroundGradientColors(ATabVisibleIndex: Integer; out AColor1, AColor2: TColor); override;
    function GetButtonsRegionHOffset: Integer; override;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; override;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; override;
    function GetFrameColor: TColor; override;
    function GetGeometricalMinTabWidth: Integer; override;
    function GetSlantedSides: TcxTabSlantPositions; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabsContainerOffsets: TRect; override;
    function GetTabsNormalDistance: TcxPCDistance; override;
    function GetTabsRectOffset: TRect; override;
    function InternalGetCutValue: Integer; override;
    function InternalGetTabClipRegion(ACanvas: TcxCanvas; ATabVisibleIndex: Integer; AForContent: Boolean): TcxRegion; override;
    function IsCloseButtonOnSlantedSide(ATabVisibleIndex: Integer): Boolean; override;
    procedure PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer; const R: TRect); override;
    procedure PaintTabStateMark(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); override;
  public
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

var
  cxPCLightBrushColor: TColor;

procedure DrawBorder(ACanvas: TcxCanvas; var ARect: TRect; ASides: array of TcxBorder; AColors: array of TColor; AExcludeBorder: Boolean = False);
procedure GetRectSize(const R: TRect; AIsHorizontal: Boolean; out ARWidth, ARHeight: Integer);
procedure RotateTabsDistance(var ADistance: TcxPCDistance);

implementation

uses
  Math, cxPCPaintersFactory, dxThemeConsts, cxControls,
  dxUxTheme, dxOffice11, cxGeometry, dxDPIAwareUtils;

const
  ButtonsPainterTabContentWOffsetA: array[Boolean] of TcxPCWOffset = (
    (Left: 4; Right: 4),
    (Left: 3; Right: 4)
  );

  StandardNativePainterButtonHeight = 17;
  StandardNativePainterButtonWidth = 17;
  StandardPainterButtonHeight = 20;
  StandardPainterButtonWidthA: array [Boolean, TcxPCNavigatorButton] of Integer = (
    (20, 20, 12, 20), // TabPosition in [tpLeft, tpRight]
    (20, 20, 13, 20)  // TabPosition in [tpTop, tpBottom]
  );
  StandardPainterTabBorderWidth = 2;
  StandardPainterDefaultTabNormalHeightA: array [TcxPCStandardStyle] of Integer = (18, 20, 20);
  StandardPainterDefaultTabNormalHTextOffsetA: array [TcxPCStandardStyle] of Integer = (2, 4, 4);
  StandardPainterDrawImageOffsetA: array [Boolean, TcxPCStandardStyle] of TRect = (
   ((Left: 6; Top: 1; Right: 0; Bottom: 0),
    (Left: 6; Top: -1; Right: 0; Bottom: 1),
    (Left: 6; Top: -1; Right: 0; Bottom: 1)
   ),
   ((Left: 8; Top: 1; Right: 0; Bottom: 3),
    (Left: 7; Top: -1; Right: 0; Bottom: 1),
    (Left: 7; Top: -1; Right: 0; Bottom: 1)
   )
  );
  StandardPainterDrawTextHOffsetA: array [Boolean, TcxPCStandardStyle] of TRect = (
   ((Left: 5; Top: 1; Right: 5; Bottom: -1),
    (Left: 6; Top: 0; Right: 4; Bottom: 1),
    (Left: 6; Top: 0; Right: 4; Bottom: 1)
   ),
   ((Left: 5; Top: 1; Right: 5; Bottom: 2),
    (Left: 6; Top: 0; Right: 4; Bottom: 1),
    (Left: 6; Top: 0; Right: 4; Bottom: 1)
   )
  );
  StandardPainterTabContentWOffsetA: array [Boolean, TcxPCStandardStyle] of TcxPCWOffset = (
   ((Left: 2; Right: 2),
    (Left: 3; Right: 3),
    (Left: 2; Right: 2)
   ),
   ((Left: 4; Right: 4),
    (Left: 3; Right: 3),
    (Left: 4; Right: 2)
   )
  );
  StandardPainterTabsNormalDistanceA: array [TcxPCStandardStyle] of TcxPCDistance = (
    (dw: 0; dh: 0),
    (dw: 3; dh: 3),
    (dw: 10; dh: 3)
  );
  StandardPainterMaxTabBorderWidth = 2;
  StandardPainterWDistanceBetweenImageBorderAndText = 1;
  StandardPainterTooNarrowTabContentWOffsetA: array [Boolean, TcxPCStandardStyle] of TcxPCWOffset = (
   ((Left: 2; Right: 2),
    (Left: 3; Right: 3),
    (Left: 2; Right: 2)
   ),
   ((Left: 4; Right: 4),
    (Left: 3; Right: 3),
    (Left: 3; Right: 2)
   )
  );

  TabsPainterContentWOffsetA: array[Boolean, Boolean] of TcxPCWOffset = (
   ((Left: 3; Right: 3),
    (Left: 3; Right: 3)),
   ((Left: 5; Right: 5),
    (Left: 4; Right: 5))
  );
  TabsPainterDrawImageWithoutTextRotatedMainTabWOffset: array[Boolean] of TcxPCWOffset =
    ((Left: 1; Right: 2), (Left: 2; Right: 1));
  TabsPainterButtonBorderWidth = 2;

  ButtonedPainterDistanceBetweenTabsAndClientRects = 4;

  ExtraFlatPainterButtonSize = 13;
  ExtraFlatPainterDefaultTabNormalWidth = 0;
  ExtraFlatPainterDrawImageOffsetA: array [Boolean] of TRect = (
    (Left: 2; Top: 0; Right: 0; Bottom: 0),
    (Left: 3; Top: 0; Right: 0; Bottom: 0)
  );
  ExtraFlatPainterDrawTextHOffsetA: array [Boolean] of TRect = (
    (Left: 5; Top: 2; Right: 3; Bottom: 0),
    (Left: 6; Top: 2; Right: 3; Bottom: 0)
  );
  ExtraFlatPainterMainTabBorderWidth = 1;
  ExtraFlatPainterMainTabRectCorrection: TRect = (
    Left: -1; Top: -1; Right: 1; Bottom: 1
  );
  ExtraFlatPainterTabContentWOffset: array [Boolean] of TcxPCWOffset = (
    (Left: 2; Right: 2),
    (Left: 3; Right: 3)
  );
  ExtraFlatPainterWDistanceBetweenImageBorderAndText = 0;
  ExtraFlatPainterTabsRowFreeSpaceWidth = 3;

  ExtraFlatPainterMainTabRowUnderlineWidth = 2;

  MinTabSelectionDistance: TcxPCDistance = (dw: 4; dh: 4);

  cxPCDarkEdgeColor = clBtnShadow;
  cxPCDarkestEdgeColor = cl3DDkShadow;
  cxPCTabBodyColor = clBtnFace;
  cxPCLightEdgeColor = {clNavy}cl3DLight;
  cxPCLightestEdgeColor = clBtnHighlight;

  cxPCLightBrushColorDelta = 20;

  cxPCArrowConvertionA: array [nbTopLeft .. nbBottomRight, Boolean] of TcxPCArrow = (
    (aLeft, aTop),
    (aRight, aBottom)
  );
  cxPCArrowSizeA: array [nbTopLeft .. nbGoDialog] of Integer = (5, 5, 4);

  UltraFlatPainterButtonWidthA: array[TcxPCNavigatorButton] of Integer =
    (15, 15, 11, 14);
  UltraFlatPainterButtonHeight = 15;

  FlatPainterButtonBorderWidth = 1;

  SlantedPainterButtonWidthA: array[TcxPCNavigatorButton] of Integer =
    (17, 17, 13, 16);
  SlantedPainterButtonHeight = 17;

  CutCornerSize = 6;
  SlantedPainterTabStateMarkWidth = 3;
  OneNotePainterTabFrameWidth = 2;

  CloseButtonCrossSize = 9;

type
  TWinControlAccess = class(TWinControl);
  TcxTabViewInfoAccess = class(TcxTabViewInfo);

  { TdxScaleFactorHelper }

  TdxScaleFactorHelper = class helper for TdxScaleFactor
  public
    function Apply(const P: TcxPCWOffset): TcxPCWOffset; overload;
  end;

  { TcxPaletteChangedNotifier }

  TcxPaletteChangedNotifier = class(TcxSystemPaletteChangedNotifier)
  protected
    procedure DoChanged; override;
  end;

var
  FPaletteChangedNotifier: TcxPaletteChangedNotifier;
  OneNoteMainTabBorderColor: COLORREF;
  OneNoteTabBorderColor: COLORREF;
  OneNoteMainTabInnerBorderColor: COLORREF;
  OneNoteTabHotBorderColor: COLORREF;
  OneNoteTabInnerBorderColor1: COLORREF;
  OneNoteTabInnerBorderColor2: COLORREF;

procedure AddPoints(var APoints: TPoints; const ANewPoints: array of TPoint;
  var ALinePositions: TLinePositions; const ANewLinePositions: array of TLinePosition);
var
  I, AArrayLength: Integer;
begin
  AArrayLength := Length(APoints);
  SetLength(APoints, AArrayLength + Length(ANewPoints));
  for I := 0 to Length(ANewPoints) - 1 do
    APoints[AArrayLength + I] := ANewPoints[I];

  AArrayLength := Length(ALinePositions);
  SetLength(ALinePositions, AArrayLength + Length(ANewLinePositions));
  for I := 0 to Length(ANewLinePositions) - 1 do
    ALinePositions[AArrayLength + I] := ANewLinePositions[I];
end;

procedure CalculateLightBrushColor;
var
  R, G, B: Integer;
  Color: Integer;
begin
  Color := ColorToRGB(clBtnFace);
  R := GetRValue(Color) + cxPCLightBrushColorDelta;
  if R > 255 then R := 255;
  G := GetGValue(Color) + cxPCLightBrushColorDelta;
  if G > 255 then G := 255;
  B := GetBValue(Color) + cxPCLightBrushColorDelta;
  if B > 255 then B := 255;
  cxPCLightBrushColor := RGB(R, G, B);
end;

procedure DrawBorder(ACanvas: TcxCanvas; var ARect: TRect; ASides: array of TcxBorder; AColors: array of TColor; AExcludeBorder: Boolean = False);
var
  I: Integer;
  ARegion: TcxRegion;
  AInitialRect: TRect;
begin
  AInitialRect := ARect;
  for I := 0 to High(ASides) do
  begin
    ACanvas.Pen.Color := AColors[I];
    case ASides[I] of
      bLeft:
        begin
          ACanvas.Polyline([ARect.TopLeft, Point(ARect.Left, ARect.Bottom)]);
          Inc(ARect.Left);
        end;
      bTop:
        begin
          ACanvas.Polyline([ARect.TopLeft, Point(ARect.Right, ARect.Top)]);
          Inc(ARect.Top);
        end;
      bRight:
        begin
          ACanvas.Polyline([Point(ARect.Right - 1, ARect.Top), Point(ARect.Right - 1, ARect.Bottom)]);
          Dec(ARect.Right);
        end;
      bBottom:
        begin
          ACanvas.Polyline([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right, ARect.Bottom - 1)]);
          Dec(ARect.Bottom);
        end;
    end;
  end;
  if AExcludeBorder then
  begin
    ARegion := TcxRegion.Create(AInitialRect);
    ARegion.Combine(ARect, roSubtract);
    ACanvas.SetClipRegion(ARegion, roSubtract);
  end;
end;

function GetFigureRegion(APoints: array of TPoint;
  const ALinePositions: array of TLinePosition; AForContent: Boolean): TcxRegion;

  function ThereIsLine(ALinePosition: TLinePosition): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to High(ALinePositions) do
      if ALinePositions[I] = ALinePosition then
      begin
        Result := True;
        Break;
      end;
  end;

var
  I: Integer;
  P1, P2: TPoint;
begin
  for I := 0 to High(APoints) - 1 do
  begin
    P1 := APoints[I];
    P2 := APoints[I + 1];
    if AForContent then
    begin
      case ALinePositions[I] of
        lpL:
          begin
            Inc(P1.X);
            Inc(P2.X);
            if ThereIsLine(lpLB) then
              Inc(P1.Y);
            if ThereIsLine(lpLT) then
              Dec(P2.Y);
          end;
        lpLT:
          begin
            if ThereIsLine(lpL) then
              Inc(P1.Y)
            else
              Inc(P1.X);
            if ThereIsLine(lpT) then
              Inc(P2.X)
            else
              Inc(P2.Y);
          end;
        lpLB:
          begin
            if ThereIsLine(lpB) then
              Inc(P1.X)
            else
              Dec(P1.Y);
            if ThereIsLine(lpL) then
              Dec(P2.Y)
            else
              Inc(P2.X);
          end;
        lpT:
          begin
            Inc(P1.Y);
            Inc(P2.Y);
            if ThereIsLine(lpLT) then
              Dec(P1.X);
            if ThereIsLine(lpRT) then
              Inc(P2.X);
          end;
      end;
    end
    else
      case ALinePositions[I] of
        lpRT:
          begin
            if ThereIsLine(lpT) then
              Inc(P1.X)
            else
              Dec(P1.Y);
            if ThereIsLine(lpR) then
              Dec(P2.Y)
            else
              Inc(P2.X);
          end;
        lpR:
          begin
            Inc(P1.X);
            Inc(P2.X);
            if ThereIsLine(lpRT) then
              Inc(P1.Y);
            if ThereIsLine(lpRB) then
              Dec(P2.Y);
          end;
        lpRB:
          begin
            if ThereIsLine(lpR) then
              Inc(P1.Y)
            else
              Inc(P1.X);
            if ThereIsLine(lpB) then
              Inc(P2.X)
            else
              Inc(P2.Y);
          end;
        lpB:
          begin
            Inc(P1.Y);
            Inc(P2.Y);
            if ThereIsLine(lpRB) then
              Dec(P1.X);
            if ThereIsLine(lpLB) then
              Inc(P2.X);
          end;
      end;
    APoints[I] := P1;
    APoints[I + 1] := P2;
  end;
  Result := TcxRegion.Create(CreatePolygonRgn(APoints, Length(APoints), WINDING));
end;

procedure GetRectSize(const R: TRect; AIsHorizontal: Boolean;
  out ARWidth, ARHeight: Integer);
begin
  if AIsHorizontal then
  begin
    ARWidth := R.Right - R.Left;
    ARHeight := R.Bottom - R.Top;
  end
  else
  begin
    ARWidth := R.Bottom - R.Top;
    ARHeight := R.Right - R.Left;
  end;
end;

function Light(AColor: TColor; APercentage: Byte): TColor;
var
  AHSV: TdxHSV;
begin
  AHSV := dxColorToHSV(AColor);
  AHSV.V := AHSV.V * (100 + APercentage) / 100;
  if AHSV.V > 1 then
    AHSV.V := 1;
  Result := dxHSVToColor(AHSV);
end;

procedure PrepareOneNoteStyleColors;
const
  AColors: array[TOffice11Scheme, 0..5] of TColor = (
    (clBtnShadow, clBtnShadow, clWhite, $6A240A, clBtnFace, clBtnFace),
    ($9C613B, $9A3500, clWhite, $800000, clWhite, $F1A675),
    ($588060, $6B7760, clWhite, $385D3F, clWhite, $8CC2B0),
    ($947C7C, $927476, clWhite, $6F4B4B, clWhite, $CEB9BA)
  );
var
  AColorScheme: TOffice11Scheme;
begin
  AColorScheme := GetOffice11Scheme;
  OneNoteMainTabBorderColor := AColors[AColorScheme, 0];
  OneNoteTabBorderColor := AColors[AColorScheme, 1];
  OneNoteMainTabInnerBorderColor := AColors[AColorScheme, 2];
  OneNoteTabHotBorderColor := AColors[AColorScheme, 3];
  OneNoteTabInnerBorderColor1 := AColors[AColorScheme, 4];
  OneNoteTabInnerBorderColor2 := AColors[AColorScheme, 5];
end;

procedure RotateTabsDistance(var ADistance: TcxPCDistance);
var
  A: Integer;
begin
  A := ADistance.dw;
  ADistance.dw := ADistance.dh;
  ADistance.dh := A;
end;

{ TcxPCTabsPainter }

function TcxPCTabsPainter.GetTabCorrection(ATabVisibleIndex: Integer): TRect;
const
  TabRectCorrectionA: array[TcxTabPosition] of TRect = (
    (Left: -2; Top: -2; Right: 2; Bottom: 1),
    (Left: -2; Top: -1; Right: 2; Bottom: 2),
    (Left: -2; Top: -2; Right: 1; Bottom: 2),
    (Left: -1; Top: -2; Right: 2; Bottom: 2)
  );
begin
  if ATabVisibleIndex = ViewInfo.MainTabVisibleIndex then
    Result := TabRectCorrectionA[TabViewInfo[ATabVisibleIndex].PaintingPosition]
  else
    Result := inherited GetTabCorrection(ATabVisibleIndex);
end;

procedure TcxPCTabsPainter.DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet);
var
  ATheme: TdxTheme;
  R: TRect;
  APageOffset: TPoint;
begin
  R := GetPageFrameRect;
  InflateRect(R, StandardPainterTabControlFrameBorderWidth, StandardPainterTabControlFrameBorderWidth);
  APageOffset := GetPageClientRect.TopLeft;
  OffsetRect(R, -APageOffset.X, -APageOffset.Y); // B158825
  ATheme := OpenTheme(totTab);
  DrawThemeBackground(ATheme, DC, TABP_PANE, 0, R);
end;

function TcxPCTabsPainter.GetBorderWidths: TRect;
begin
  if IsNativePainting then
    Result := GetNativeContentOffset
  else
    Result := inherited GetBorderWidths;
end;

function TcxPCTabsPainter.GetButtonContentPosition(const ARect: TRect;
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;
begin
  if NeedButtonContentPositionCentered then
    Result := inherited GetButtonContentPosition(ARect, AButton, AState)
  else
    Result := Get3DButtonContentPosition(AButton, AState);
end;

function TcxPCTabsPainter.DoGetButtonHeight: Integer;
begin
  if IsNativePainting then
    Result := GetNativeButtonHeight
  else
    Result := inherited DoGetButtonHeight;
end;

procedure TcxPCTabsPainter.GetButtonNativePartAndState(AState: TcxPCNavigatorButtonState;
  AType: TcxPCNavigatorButton; out APartId, AStateId: Integer;
  out AThemeObjectType: TdxThemedObjectType);
const
  AArrowButtonPartIdMapXP: array [TcxPCArrow] of Integer = (SPNP_UP, SPNP_DOWN,
    SPNP_DOWNHORZ, SPNP_UPHORZ);
  AArrowButtonStateIdMapXP: array [TcxPCArrow, TcxPCNavigatorButtonState] of Integer = (
    (UPS_NORMAL, UPS_PRESSED, UPS_HOT, UPS_DISABLED),
    (DNS_NORMAL, DNS_PRESSED, DNS_HOT, DNS_DISABLED),
    (DNHZS_NORMAL, DNHZS_PRESSED, DNHZS_HOT, DNHZS_DISABLED),
    (UPHZS_NORMAL, UPHZS_PRESSED, UPHZS_HOT, UPHZS_DISABLED)
  );
  AArrowButtonStateIdMap: array [TcxPCArrow, TcxPCNavigatorButtonState] of Integer = (
    (ABS_UPNORMAL, ABS_UPPRESSED, ABS_UPHOT, ABS_UPDISABLED),
    (ABS_DOWNNORMAL, ABS_DOWNPRESSED, ABS_DOWNHOT, ABS_DOWNDISABLED),
    (ABS_LEFTNORMAL, ABS_LEFTPRESSED, ABS_LEFTHOT, ABS_LEFTDISABLED),
    (ABS_RIGHTNORMAL, ABS_RIGHTPRESSED, ABS_RIGHTHOT, ABS_RIGHTDISABLED)
  );
  ACloseButtonStateIdMap: array [TcxPCNavigatorButtonState] of Integer =
    (CBS_NORMAL, CBS_PUSHED, CBS_HOT, CBS_DISABLED);
  AGoDialogButtonStateIdMap: array [TcxPCNavigatorButtonState] of Integer =
    (CBXS_NORMAL, CBXS_PRESSED, CBXS_HOT, CBXS_DISABLED);
var
  AArrow: TcxPCArrow;
begin
  if AType = nbClose then
  begin
    AThemeObjectType := totWindow;
    APartId := WP_SMALLCLOSEBUTTON;
    AStateId := ACloseButtonStateIdMap[AState];
  end
  else
    if AType = nbGoDialog then
    begin
      AThemeObjectType := totComboBox;
      APartId := CP_DROPDOWNBUTTON;
      AStateId := AGoDialogButtonStateIdMap[AState];
    end
    else
      if IsWinVistaOrLater then
      begin
        AThemeObjectType := totScrollBar;
        AArrow := GetButtonArrow(AType);
        APartId := SBP_ARROWBTN;
        AStateId := AArrowButtonStateIdMap[AArrow, AState];
      end
      else
      begin
        AThemeObjectType := totSpin;
        AArrow := GetButtonArrow(AType);
        APartId := AArrowButtonPartIdMapXP[AArrow];
        AStateId := AArrowButtonStateIdMapXP[AArrow, AState];
      end;
end;

function TcxPCTabsPainter.GetButtonsRegionHOffset: Integer;
begin
  Result := 0;
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset;
end;

function TcxPCTabsPainter.GetButtonsRegionWOffset: Integer;
begin
  Result := 0;
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset;
end;

function TcxPCTabsPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
var
  ASize: TSize;
begin
  if IsNativePainting then
  begin
    Result := ScaleFactor.Apply(StandardNativePainterButtonWidth);
    if Button = nbGoDialog then
    begin
      if IsWinSevenOrLater then
        Result := ScaleFactor.Apply(GetSystemMetrics(SM_CYHSCROLL), dxSystemScaleFactor)
      else
        if GetThemePartSize(OpenTheme(totComboBox), 0, CP_DROPDOWNBUTTON, CBXS_NORMAL, TS_TRUE, ASize) = S_OK then
          Result := ScaleFactor.Apply(ASize.cx, dxSystemScaleFactor);
    end;
  end
  else
    Result := inherited DoGetButtonWidth(Button);
end;

function TcxPCTabsPainter.GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect;
begin
  Result := inherited GetCloseButtonOffset(ATabVisibleIndex);
  if ATabVisibleIndex = -1 then
    Exit;
  if not IsNativePainting then
  begin
    if TabViewInfo[ATabVisibleIndex].GetRelativeTextRotationAngle = ra180 then
      Dec(Result.Top)
    else
      Inc(Result.Top);
  end;
end;

function TcxPCTabsPainter.GetNativeButtonHeight: Integer;
begin
  if IsWinVistaOrLater then
    Result := ScaleFactor.Apply(21)
  else
    Result := ScaleFactor.Apply(StandardNativePainterButtonHeight);
end;

function TcxPCTabsPainter.GetPageClientRectOffset: TRect;
begin
  Result := inherited GetPageClientRectOffset;
  if ViewInfo.IsTabsContainer and not IsNativePainting then
    Inc(Result.Top, TabsContainerBaseWidth - GetFrameWidth);
end;

function TcxPCTabsPainter.GetDrawImageOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := inherited GetDrawImageOffset(TabVisibleIndex);
  if TabViewInfo[TabVisibleIndex].GetRelativeTextRotationAngle = ra180 then
    Inc(Result.Bottom);
end;

function TcxPCTabsPainter.GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset;
begin
  if ViewInfo.ActuallyRotate and TabViewInfo[TabVisibleIndex].IsMainTab then
    Result := TabsPainterDrawImageWithoutTextRotatedMainTabWOffset[TabViewInfo[TabVisibleIndex].GetRelativeTextRotationAngle = raMinus90]
  else
    Result := inherited GetDrawImageWithoutTextWOffset(TabVisibleIndex);
  if IsNativePainting and (TabViewInfo[TabVisibleIndex].GetRelativeTextRotationAngle = raPlus90) then
    ExchangeLongWords(Result.Left, Result.Right);
end;

function TcxPCTabsPainter.GetDrawTextHOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := inherited GetDrawTextHOffset(TabVisibleIndex);
  if TabViewInfo[TabVisibleIndex].GetRelativeTextRotationAngle = ra180 then
    Inc(Result.Bottom);
end;

function TcxPCTabsPainter.GetHeaderButtonGlyphOffset: TRect;
begin
  if IsNativePainting then
    Result := ScaleFactor.Apply(Rect(4, 4, 4, 4))
  else
    Result := inherited GetHeaderButtonGlyphOffset;
end;

function TcxPCTabsPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
const
  StyleMap: array[Boolean] of TcxLookAndFeelStyle = (lfsStandard, lfsNative);
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(StyleMap[IsNativePainting]);
end;

{ TcxPCTabsPainter }

class function TcxPCTabsPainter.GetStandardStyle: TcxPCStandardStyle;
begin
  Result := tsTabs;
end;

class function TcxPCTabsPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCTabsStyle;
end;

class function TcxPCTabsPainter.GetStyleName: TCaption;
begin
  Result := 'Tabs';
end;

class function TcxPCTabsPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := ALookAndFeel.GetAvailablePainter([totTab]).LookAndFeelStyle in [lfsStandard, lfsNative];
end;

class function TcxPCTabsPainter.IsDefault(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := ALookAndFeel.NativeStyle;
end;

function TcxPCTabsPainter.GetTabBodyColor(TabVisibleIndex: Integer): TColor;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  if ATabViewInfo.IsHighlighted then
    Result := HighlightedTabBodyColor
  else
  begin
    Result := GetTabColor(TabVisibleIndex);
    if Result = clDefault then
      Result := cxPCTabBodyColor;
  end;
end;

function TcxPCTabsPainter.GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion;
var
  ATabViewInfo: TcxTabViewInfo;
  ATabRect: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];

  ATabRect := ATabViewInfo.VisibleRect;
  if ATabViewInfo.IsMainTab then
    ATabRect := GetExtendedRect(ATabViewInfo.VisibleRect, Rect(0, 0, 0, -1), ATabViewInfo.PaintingPosition);

  Result := TcxRegion.Create(ATabRect);
end;

function TcxPCTabsPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
var
  ATabViewInfo: TcxTabViewInfo;
  APartID, AStateID: Integer;
begin
  Result := TabsPainterContentWOffsetA[IsTabBorderThick(ATabVisibleIndex), ViewInfo.ActuallyRotate];
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if IsNativePainting and ATabViewInfo.HasButtons then
  begin
    GetTabNativePartAndState(ATabVisibleIndex, APartID, AStateID);
    if APartID = TABP_TABITEMRIGHTEDGE then
      Inc(Result.Right);
  end;

  if (ATabViewInfo.GetRelativeTextRotationAngle = raMinus90) or
    IsNativePainting and (ATabViewInfo.GetRelativeTextRotationAngle = raPlus90)
  then
    ExchangeLongWords(Result.Left, Result.Right);

  Result := ScaleFactor.Apply(Result);
end;

function TcxPCTabsPainter.GetTabFocusRect(const ATabBounds: TRect): TRect;
begin
  Result := cxRectInflate(ATabBounds, -ScaleFactor.Apply(StandardPainterTabBorderWidth));
end;

procedure TcxPCTabsPainter.GetTabNativePartAndState(ATabVisibleIndex: Integer; out PartId, StateId: Integer);
type
  TcxTabPositionWithinRow = (tprLeftMost, tprMiddle, tprRightMost);
const
  ATabNativePart: array[TcxTabPositionWithinRow] of Integer = (TABP_TABITEMLEFTEDGE, TABP_TABITEM, TABP_TABITEMRIGHTEDGE);
var
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  ATabViewInfo: TcxTabViewInfo;
  ATabPositionWithinRow: TcxTabPositionWithinRow;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);

  if ViewInfo.MultiLine and not ViewInfo.RaggedRight then
  begin
    if ViewInfo.IsTabsContainer then
      ATabPositionWithinRow := tprMiddle
    else
      if ALineIndexBoundsA[ATabViewInfo.VisibleRow].Left = ATabViewInfo.VisibleIndex then
        ATabPositionWithinRow := tprLeftMost
      else
        if (ALineIndexBoundsA[ATabViewInfo.VisibleRow].Right = ATabViewInfo.VisibleIndex) and (ViewInfo.RowCount > 1) then
          ATabPositionWithinRow := tprRightMost
        else
          ATabPositionWithinRow := tprMiddle;
  end
  else
    if ViewInfo.IsTabsContainer then
      ATabPositionWithinRow := tprMiddle
    else
      if ALineIndexBoundsA[ATabViewInfo.VisibleRow].Left = ATabViewInfo.VisibleIndex then
        ATabPositionWithinRow := tprLeftMost
      else
        if (ATabViewInfo.VisibleIndex = ViewInfo.TabsViewInfo.ViewInfoCount - 1) and
          not ATabViewInfo.IsMainTab then
          ATabPositionWithinRow := tprRightMost
        else
          ATabPositionWithinRow := tprMiddle;

  PartId := ATabNativePart[ATabPositionWithinRow];
  StateId := GetTabNativeState(ATabVisibleIndex);
end;

function TcxPCTabsPainter.GetTabsContainerOffsets: TRect;
begin
  Result := Rect(TabsContainerOffset, TabsContainerOffset, TabsContainerOffset, 0);
end;

function TcxPCTabsPainter.GetTabsPosition: TcxPCTabsPosition;
var
  NormalTabsRectCorrection: TRect;

  procedure DoHardCalculation(var ATabsPosition: TcxPCTabsPosition);
  var
    AButtonsWidth: Integer;
    ATabsContainerOffset: TRect;
  begin
    AButtonsWidth := CalculateButtonsRegionWidth;
    if ViewInfo.IsTabsContainer then
      ATabsContainerOffset := GetTabsContainerOffsets
    else
      ATabsContainerOffset := cxEmptyRect;

    Inc(ATabsContainerOffset.Top, GetHeaderButtonHeightCorrection);
    NormalTabsRectCorrection := RotateRect(Rect(0, 2 + ATabsContainerOffset.Top, 0, 0), ViewInfo.TabPosition);
    ATabsPosition.ExtendedTabsRect := cxRectContent(ATabsPosition.ExtendedTabsRect,
      RotateRect(Rect(0, ATabsContainerOffset.Top, 0, 0), ViewInfo.TabPosition));
    if ViewInfo.TabPosition in [tpTop, tpBottom] then
    begin
      if ViewInfo.MultiLine or (ViewInfo.NavigatorButtonCount = 0) then
      begin
        ATabsPosition.ExtendedTabsRect.Left := ATabsContainerOffset.Left;
        ATabsPosition.ExtendedTabsRect.Right := ViewInfo.Width - ATabsContainerOffset.Left;
        NormalTabsRectCorrection.Left := 2 + ATabsContainerOffset.Left;
        NormalTabsRectCorrection.Right := 2 + ATabsContainerOffset.Left;
      end
      else
      begin
        if ViewInfo.NavigatorPosition in [npLeftTop, npLeftBottom] then
        begin
          ATabsPosition.ExtendedTabsRect.Left := AButtonsWidth;
          ATabsPosition.ExtendedTabsRect.Right := ViewInfo.Width - ATabsContainerOffset.Left;
        end
        else
        begin
          ATabsPosition.ExtendedTabsRect.Left := ATabsContainerOffset.Left;
          ATabsPosition.ExtendedTabsRect.Right := ViewInfo.Width - AButtonsWidth;
        end;
        NormalTabsRectCorrection.Left := ATabsPosition.ExtendedTabsRect.Left + 2;
        NormalTabsRectCorrection.Right := ViewInfo.Width - ATabsPosition.ExtendedTabsRect.Right + 2;
      end;
      ATabsPosition.NormalRowWidth := ViewInfo.Width - NormalTabsRectCorrection.Left - NormalTabsRectCorrection.Right;
    end
    else
    begin
      if ViewInfo.MultiLine or (ViewInfo.NavigatorButtonCount = 0) then
      begin
        ATabsPosition.ExtendedTabsRect.Top := ATabsContainerOffset.Left;
        ATabsPosition.ExtendedTabsRect.Bottom := ViewInfo.Height - ATabsContainerOffset.Left;
        NormalTabsRectCorrection.Top := 2 + ATabsContainerOffset.Left;
        NormalTabsRectCorrection.Bottom := 2 + ATabsContainerOffset.Left;
      end
      else
      begin
        if ViewInfo.NavigatorPosition in [npLeftTop, npRightTop] then
        begin
          ATabsPosition.ExtendedTabsRect.Top := AButtonsWidth;
          ATabsPosition.ExtendedTabsRect.Bottom := ViewInfo.Height - ATabsContainerOffset.Left;
        end
        else
        begin
          ATabsPosition.ExtendedTabsRect.Top := ATabsContainerOffset.Left;
          ATabsPosition.ExtendedTabsRect.Bottom := ViewInfo.Height - AButtonsWidth;
        end;
        NormalTabsRectCorrection.Top := ATabsPosition.ExtendedTabsRect.Top + 2;
        NormalTabsRectCorrection.Bottom := ViewInfo.Height - ATabsPosition.ExtendedTabsRect.Bottom + 2;
      end;
      ATabsPosition.NormalRowWidth := ViewInfo.Height - NormalTabsRectCorrection.Top - NormalTabsRectCorrection.Bottom;
    end;
  end;

begin
  Result.ExtendedTabsRect := ViewInfo.ControlBounds;
  Result.NormalTabsRect := Result.ExtendedTabsRect;

  DoHardCalculation(Result);

  Result.NormalTabsRect := cxRectContent(Result.NormalTabsRect, NormalTabsRectCorrection);

  Result.ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 0;
  Result.ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := 0;
  if ViewInfo.TabPosition in [tpTop, tpLeft] then
    Result.ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 1
  else
    Result.ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := -1;
  Result.MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects := 0;

  CalculateButtonsRegion;
end;

function TcxPCTabsPainter.GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := GetTabContentWOffset(ATabVisibleIndex);
end;

procedure TcxPCTabsPainter.DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  R: TRect;
  ATabPaintingPosition: TcxTabPosition;
begin
  if TabViewInfo[ATabVisibleIndex].IsFocused then
  begin
    ATabPaintingPosition := TabViewInfo[ATabVisibleIndex].PaintingPosition;
    R := TabViewInfo[ATabVisibleIndex].FullRect;
    InflateRect(R, -StandardPainterTabBorderWidth, -StandardPainterTabBorderWidth);
    R := GetExtendedRect(R, Rect(0, 0, 0, -StandardPainterTabBorderWidth), ATabPaintingPosition);
    R := GetExtendedRect(R, Rect(0, 0, 0, 1), ATabPaintingPosition);
    InternalDrawFocusRect(ACanvas, ATabVisibleIndex, R);
  end;
end;

function TcxPCTabsPainter.InternalCalculateTabNormalHeight: Integer;
begin
  if ViewInfo.ActuallyRotate then
    Result := inherited InternalCalculateTabNormalHeight - ScaleFactor.Apply(2)
  else
    Result := inherited InternalCalculateTabNormalHeight - ScaleFactor.Apply(3);
end;

function TcxPCTabsPainter.InternalGetPageFrameRectOffset: TRect;
begin
  Result := inherited InternalGetPageFrameRectOffset;
  Dec(Result.Top);
end;

procedure TcxPCTabsPainter.InternalPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ABitmap: TcxBitmap;
  ATabRect: TRect;
  ATabViewInfo: TcxTabViewInfo;
  ATabOrigin: TPoint;
begin
  if IsNativePainting then
  begin
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    ATabRect := ATabViewInfo.FullRect;
    ABitmap := TcxBitmap.CreateSize(0, 0, pf32bit);
    try
      ABitmap.Canvas.Lock;
      try
        ATabOrigin := cxNullPoint;
        PrepareTabBackground(ABitmap, ATabVisibleIndex);
        DrawTabImageAndText(ABitmap.cxCanvas, ATabVisibleIndex);
        ABitmap.Rotate(ViewInfo.GetTextRotationAngle);
        if ATabViewInfo.IsMainTab then
          ATabRect := GetExtendedRect(ATabRect, Rect(0, 0, 0, -1), ATabViewInfo.PaintingPosition);
        cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, ATabRect, ATabOrigin, SRCCOPY);
      finally
       ABitmap.Canvas.Unlock;
      end;
    finally
      FreeAndNil(ABitmap);
    end;
    InternalDrawFocusRect(ACanvas, ATabVisibleIndex, GetTabFocusRect(ATabRect));
  end
  else
    inherited InternalPaintTab(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCTabsPainter.InvalidateTabRect(ATabVisibleIndex: Integer);
var
  ATabViewInfo: TcxTabViewInfo;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  R := ATabViewInfo.VisibleRect;
  R := GetExtendedRect(R, Rect(0, 0, 0, -1), ATabViewInfo.PaintingPosition);
  ViewInfo.InvalidateRect(R, False);
end;

function TcxPCTabsPainter.IsNativePainting: Boolean;
begin
  if ViewInfo.IControl.IsDestroying then
    Result := False
  else
    Result := ViewInfo.GetLookAndFeel.NativeStyle and AreVisualStylesAvailable and (OpenTheme(totTab) <> TC_NONE);
end;

function TcxPCTabsPainter.PtInTab(TabVisibleIndex: Integer; X, Y: Integer): Boolean;
begin
  Result := True;
end;

function TcxPCTabsPainter.NeedShowFrame: Boolean;
begin
  Result := True;
end;

procedure TcxPCTabsPainter.PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton);
begin
  if IsNativePainting then
    PaintNativeButton(ACanvas, ARect, AState, AType, True)
  else
    inherited;
end;

procedure TcxPCTabsPainter.PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo);
var
  ATheme: TdxTheme;
  ARect: TRect;
begin
  if IsNativePainting then
  begin
    ARect := AButtonInfo.Bounds;
    ATheme := OpenTheme(totButton);
    if ViewInfo.ParentBackground and
      IsThemeBackgroundPartiallyTransparent(ATheme, BP_PUSHBUTTON,
        BtnStateToXPBtnState(NavigatorBtnStateToLookAndFeelBtnState[AButtonInfo.State])) then
      cxDrawThemeParentBackground(ViewInfo.IControl.GetControl, ACanvas, ARect)
    else
      ACanvas.FillRect(ARect, ViewInfo.Color);
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON,
      BtnStateToXPBtnState(NavigatorBtnStateToLookAndFeelBtnState[AButtonInfo.State]), ARect);
  end
  else
    inherited;
end;

procedure TcxPCTabsPainter.PaintNativeButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton; ADrawBackground: Boolean);
var
  APartId, AStateId: Integer;
  AThemeObjectType: TdxThemedObjectType;
  ATheme: TdxTheme;
  AContentRect: TRect;
begin
  GetButtonNativePartAndState(AState, AType, APartId, AStateId, AThemeObjectType);
  ATheme := OpenTheme(AThemeObjectType);
  if ADrawBackground then
    if ViewInfo.ParentBackground and IsThemeBackgroundPartiallyTransparent(ATheme, APartId, AStateId) then
      cxDrawThemeParentBackground(ViewInfo.IControl.GetControl, ACanvas, ARect)
    else
      ACanvas.FillRect(ARect, ViewInfo.Color);

  AContentRect := ARect;
  if IsWinVistaOrLater and (AType in [nbGoDialog, nbClose]) then
  begin
    if ViewInfo.TabPosition in [tpLeft, tpRight] then
      AContentRect := cxRectInflate(AContentRect, -1, 0)
    else
      AContentRect := cxRectInflate(AContentRect, 0, -1)
  end;
  DrawThemeBackground(ATheme, ACanvas.Handle, APartId, AStateId, AContentRect);
end;

procedure TcxPCTabsPainter.PaintPageClientArea(ACanvas: TcxCanvas);
begin
  if IsNativePainting then
    InternalPaintFrame(ACanvas)
  else
    inherited;
end;

procedure TcxPCTabsPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
  inherited DoPaintPageFrame(ACanvas);
  if ViewInfo.IsTabsContainer then
    ACanvas.FillRect(cxRectInflate(GetPageFrameRect, -GetFrameWidth), cxPCTabBodyColor);
end;

procedure TcxPCTabsPainter.PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AMainTab: Boolean;
  ATabPaintingPosition: TcxTabPosition;
  ATabViewInfo: TcxTabViewInfo;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AMainTab := ATabViewInfo.IsMainTab;

  ATabPaintingPosition := ATabViewInfo.PaintingPosition;

  R := cxRectInflate(ATabViewInfo.FullRect, -StandardPainterTabBorderWidth);
  if AMainTab or ViewInfo.IsTabsContainer then
    R := GetExtendedRect(R, Rect(0, 0, 0, -StandardPainterTabBorderWidth), ATabPaintingPosition);

  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(R, GetTabBodyColor(ATabVisibleIndex));

  if not AMainTab then
  begin
    SubtractRect(R, GetExtendedRect(R, Rect(0, 0, 0, -StandardPainterTabBorderWidth), ATabPaintingPosition), R);
    ACanvas.FillRect(R, ViewInfo.Color);
  end;

  PaintTabFrame(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCTabsPainter.PaintTabsRegion(ACanvas: TcxCanvas);
var
  I: Integer;
  AFirstIndex, ALastIndex: Integer;
  AMainTabVisibleIndex: Integer;
  ASpecialTabViewInfo: TcxTabViewInfo;
begin
  AMainTabVisibleIndex := ViewInfo.MainTabVisibleIndex;
  if AMainTabVisibleIndex <> -1 then
  begin
    ASpecialTabViewInfo := TabViewInfo[AMainTabVisibleIndex];
    if ASpecialTabViewInfo.ActuallyVisible then
      PaintTab(ACanvas, AMainTabVisibleIndex);
  end;

  ViewInfo.InitializeVisibleTabRange(AFirstIndex, ALastIndex);
  for I := AFirstIndex to ALastIndex do
    if I <> AMainTabVisibleIndex then
      PaintTab(ACanvas, I);
end;

procedure TcxPCTabsPainter.PaintNativeTabBackground(
  DC: HDC; ATabVisibleIndex: Integer; const ABounds: TRect);
var
  ATheme: TdxTheme;
  APartId, AStateId: Integer;
begin
  GetTabNativePartAndState(ATabVisibleIndex, APartId, AStateId);
  ATheme := OpenTheme(totTab);
  DrawThemeBackground(ATheme, DC, APartId, AStateId, ABounds);
end;

procedure TcxPCTabsPainter.PrepareTabBackground(ABitmap: TcxBitmap; ATabVisibleIndex: Integer);

  procedure RotateTabBitmap(ABitmap: TcxBitmap;
    ATabPaintingPosition: TcxTabPosition; ARotateBack: Boolean);
  const
    PlusMinusAngleMap: array[Boolean] of TcxRotationAngle = (raPlus90, raMinus90);
  var
    ARotationAngle: TcxRotationAngle;
  begin
    if ATabPaintingPosition in [tpLeft, tpRight] then
      ARotationAngle := PlusMinusAngleMap[ARotateBack]
    else
      ARotationAngle := ra0;
    ABitmap.Rotate(ARotationAngle, ATabPaintingPosition in [tpLeft, tpBottom]);
  end;

var
  ARotationAngle: TcxRotationAngle;
  R: TRect;
  AMainTab: Boolean;
  ATabPaintingPosition: TcxTabPosition;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AMainTab := ATabViewInfo.IsMainTab;
  ATabPaintingPosition := ATabViewInfo.PaintingPosition;
  R := ATabViewInfo.FullRect;
  if AMainTab then
    R := GetExtendedRect(R, Rect(0, 0, 0, -1), ATabPaintingPosition);
  if ATabPaintingPosition in [tpLeft, tpRight] then
    ABitmap.SetSize(cxRectHeight(R), cxRectWidth(R))
  else
    ABitmap.SetSize(cxRectWidth(R), cxRectHeight(R));
  RotateTabBitmap(ABitmap, ATabPaintingPosition, True);
  PrepareTabBitmapBackground(ABitmap, R, ATabViewInfo);
  RotateTabBitmap(ABitmap, ATabPaintingPosition, False);
  PaintNativeTabBackground(ABitmap.Canvas.Handle, ATabVisibleIndex, ABitmap.ClientRect);
  case ATabViewInfo.PaintingPositionIndex of
    2, 9, 12:
      ARotationAngle := raMinus90;
    6, 11, 3:
      ARotationAngle := raPlus90;
    4, 7:
      ARotationAngle := ra180;
    else
      ARotationAngle := ra0;
  end;
  ABitmap.Rotate(ARotationAngle, ATabPaintingPosition in [tpLeft, tpBottom]);
end;

procedure TcxPCTabsPainter.PrepareTabBitmapBackground(
  ABitmap: TcxBitmap; const ARect: TRect; ATabViewInfo: TcxTabViewInfo);
begin
  if IsNativePainting and ViewInfo.ParentBackground then
    if CanDrawParentBackground then
      cxDrawTransparentControlBackground(ViewInfo.IControl.GetControl, ABitmap.cxCanvas, ARect, cxNullPoint, False)
    else
      cxBitBlt(ABitmap.Canvas.Handle, ViewInfo.Canvas.Handle, ABitmap.ClientRect, ARect.TopLeft, SRCCOPY)
  else
    FillFreeSpaceArea(ABitmap.cxCanvas, ABitmap.ClientRect);
end;

procedure TcxPCTabsPainter.RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged);
begin
  if (TabPropertyChanged = tpcIsMainTab) and not TabViewInfo[TabVisibleIndex].IsMainTab then
    InvalidateTabExtendedTabsRect(TabVisibleIndex)
  else
    InvalidateTabRect(TabVisibleIndex);
end;

function TcxPCTabsPainter.GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  if IsNativePainting then
    Result := GetNativeButtonsDistance(AButton1, AButton2)
  else
    Result := inherited GetButtonsDistance(AButton1, AButton2);
end;

function TcxPCTabsPainter.GetMinFrameRectSize: Integer;
begin
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerBaseWidth + GetFrameWidth
  else
    Result := inherited GetMinFrameRectSize;
end;

function TcxPCTabsPainter.GetNativeButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  if IsWinVistaOrLater and IsOneOfButtons(AButton1, AButton2, nbGoDialog) and IsOneOfButtons(AButton1, AButton2, nbClose) then
    Result := ScaleFactor.Apply(1)
  else
    Result := inherited GetButtonsDistance(AButton1, AButton2);
end;

procedure TcxPCTabsPainter.GetTabCornersColor(ATabVisibleIndex: Integer;
  out AColor1, AColor2: TColor);
var
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if (ATabViewInfo.VisibleRow = 0) and (ViewInfo.TopOrLeftPartRowCount > 0) or
    (ATabViewInfo.VisibleRow = ViewInfo.RowCount - 1) and (ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount) then
  begin
    AColor1 := ViewInfo.Color;
    AColor2 := AColor1;
  end
  else
  begin
    AColor1 := clBtnFace;
    AColor2 := AColor1;
    if ATabViewInfo.IsMainTab and not ViewInfo.ActuallyRotate then
    begin
      ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);
      if ALineIndexBoundsA[ATabViewInfo.VisibleRow].Left = ATabVisibleIndex then
        AColor1 := ViewInfo.Color;
      if ALineIndexBoundsA[ATabViewInfo.VisibleRow].Right = ATabVisibleIndex then
        AColor2 := ViewInfo.Color;
    end;
  end;
end;

procedure TcxPCTabsPainter.PaintTabCorners(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AColor1, AColor2: TColor;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];

  GetTabCornersColor(ATabVisibleIndex, AColor1, AColor2);
  if (ATabViewInfo.PaintingPosition in [tpLeft, tpRight]) and (ViewInfo.GetTextRotationAngle <> raMinus90) then
    ExchangeLongWords(AColor1, AColor2);

  with ATabViewInfo.FullRect do
    case ATabViewInfo.PaintingPosition of
      tpTop:
        begin
          InternalPolyLine([Point(Left, Top + 1), Point(Left, Top), Point(Left + 1, Top)], AColor1, ACanvas);
          InternalPolyLine([Point(Right - 2, Top), Point(Right - 1, Top), Point(Right - 1, Top + 1)], AColor2, ACanvas);
        end;
      tpBottom:
        begin
          InternalPolyLine([Point(Left, Bottom - 2), Point(Left, Bottom - 1), Point(Left + 1, Bottom - 1)], AColor1, ACanvas);
          InternalPolyLine([Point(Right - 2, Bottom - 1), Point(Right - 1, Bottom - 1), Point(Right - 1, Bottom - 2)], AColor2, ACanvas);
        end;
      tpLeft:
        begin
          InternalPolyLine([Point(Left, Top + 1), Point(Left, Top), Point(Left + 1, Top)], AColor1, ACanvas);
          InternalPolyLine([Point(Left, Bottom - 2), Point(Left, Bottom - 1), Point(Left + 1, Bottom - 1)], AColor2, ACanvas);
        end;
      tpRight:
        begin
          InternalPolyLine([Point(Right - 2, Top), Point(Right - 1, Top), Point(Right - 1, Top + 1)], AColor1, ACanvas);
          InternalPolyLine([Point(Right - 2, Bottom - 1), Point(Right - 1, Bottom - 1), Point(Right - 1, Bottom - 2)], AColor2, ACanvas);
        end;
    end;
end;

procedure TcxPCTabsPainter.PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ATabUnderlineColor: TColor;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ViewInfo.IsTabsContainer then
    ATabUnderlineColor := clBtnFace
  else
    ATabUnderlineColor := GetTabBodyColor(ATabVisibleIndex);
  with ATabViewInfo.FullRect do
    case ATabViewInfo.PaintingPosition of
      tpTop:
        begin
          InternalPolyLine([Point(Left, Bottom - 1), Point(Left, Top + 2), Point(Left + 2, Top), Point(Right - 3, Top)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Bottom - 1), Point(Left + 1, Top + 2), Point(Left + 2, Top + 1), Point(Right - 3, Top + 1)], cxPCLightEdgeColor, ACanvas);
          InternalPolyLine([Point(Right - 1, Bottom - 1), Point(Right - 1, Top + 2), Point(Right - 2, Top + 1)], cxPCDarkestEdgeColor, ACanvas);
          InternalPolyLine([Point(Right - 2, Bottom - 1), Point(Right - 2, Top + 2)], cxPCDarkEdgeColor, ACanvas);

          if ATabViewInfo.IsMainTab then
          begin
            if Left = 0 then
            begin
              InternalPolyLine([Point(Left, Bottom), Point(Left, Bottom)], cxPCLightestEdgeColor, ACanvas);
              InternalPolyLine([Point(Left + 1, Bottom), Point(Left + 1, Bottom)], cxPCLightEdgeColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Left, Bottom), Point(Left + 1, Bottom)], cxPCLightEdgeColor, ACanvas);
            if Right = ViewInfo.Width then
            begin
              InternalPolyLine([Point(Left + 2, Bottom), Point(Right - 3, Bottom)], ATabUnderlineColor, ACanvas);
              InternalPolyLine([Point(Right - 2, Bottom), Point(Right - 2, Bottom)], cxPCDarkEdgeColor, ACanvas);
              InternalPolyLine([Point(Right - 1, Bottom), Point(Right - 1, Bottom)], cxPCDarkestEdgeColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Left + 2, Bottom), Point(Right - 1, Bottom)], ATabUnderlineColor, ACanvas);
          end;
        end;
      tpBottom:
        begin
          InternalPolyLine([Point(Left, Top), Point(Left, Bottom - 3), Point(Left + 1, Bottom - 2)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Top), Point(Left + 1, Bottom - 3)], cxPCLightEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 2, Bottom - 1), Point(Right - 3, Bottom - 1), Point(Right - 1, Bottom - 3), Point(Right - 1, Top)], cxPCDarkestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 2, Bottom - 2), Point(Right - 3, Bottom - 2), Point(Right - 2, Bottom - 3), Point(Right - 2, Top)], cxPCDarkEdgeColor, ACanvas);

          if ATabViewInfo.IsMainTab then
          begin
            if Left = 0 then
            begin
              InternalPolyLine([Point(Left, Top - 1), Point(Left, Top - 1)], cxPCLightestEdgeColor, ACanvas);
              InternalPolyLine([Point(Left + 1, Top - 1), Point(Left + 1, Top - 1)], cxPCLightEdgeColor, ACanvas);
              InternalPolyLine([Point(Left + 2, Top - 1), Point(Right - 3, Top - 1)], ATabUnderlineColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Left, Top - 1), Point(Right - 3, Top - 1)], ATabUnderlineColor, ACanvas);
            if Right = ViewInfo.Width then
            begin
              InternalPolyLine([Point(Right - 2, Top - 1), Point(Right - 2, Top - 1)], cxPCDarkEdgeColor, ACanvas);
              InternalPolyLine([Point(Right - 1, Top - 1), Point(Right - 1, Top - 1)], cxPCDarkestEdgeColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Right - 2, Top - 1), Point(Right - 1, Top - 1)], cxPCDarkEdgeColor, ACanvas);
          end;
        end;
      tpLeft:
        begin
          InternalPolyLine([Point(Left, Bottom - 3), Point(Left, Top + 2), Point(Left + 2, Top), Point(Right - 1, Top)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 2), Point(Left + 2, Top + 1), Point(Right - 1, Top + 1)], cxPCLightEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 2, Bottom - 2), Point(Left + 3, Bottom - 1), Point(Right - 1, Bottom - 1)], cxPCDarkestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Bottom - 2), Point(Left + 2, Bottom - 1), Point(Left + 3, Bottom - 2), Point(Right - 1, Bottom - 2)], cxPCDarkEdgeColor, ACanvas);

          if ATabViewInfo.IsMainTab then
          begin
            if Top = 0 then
            begin
              InternalPolyLine([Point(Right, Top), Point(Right, Top)], cxPCLightestEdgeColor, ACanvas);
              InternalPolyLine([Point(Right, Top + 1), Point(Right, Top + 1)], cxPCLightEdgeColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Right, Top), Point(Right, Top + 1)], cxPCLightEdgeColor, ACanvas);
            if Bottom = ViewInfo.Height then
            begin
              InternalPolyLine([Point(Right, Top + 2), Point(Right, Bottom - 3)], ATabUnderlineColor, ACanvas);
              InternalPolyLine([Point(Right, Bottom - 2), Point(Right, Bottom - 2)], cxPCDarkEdgeColor, ACanvas);
              InternalPolyLine([Point(Right, Bottom - 1), Point(Right, Bottom - 1)], cxPCDarkestEdgeColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Right, Top + 2), Point(Right, Bottom - 1)], ATabUnderlineColor, ACanvas);
          end;
        end;
      tpRight:
        begin
          InternalPolyLine([Point(Left, Top), Point(Right - 3, Top), Point(Right - 2, Top + 1)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left, Top + 1), Point(Right - 3, Top + 1)], cxPCLightEdgeColor, ACanvas);
          InternalPolyLine([Point(Left, Bottom - 1), Point(Right - 3, Bottom - 1), Point(Right - 1, Bottom - 3), Point(Right - 1, Top + 2)], cxPCDarkestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left, Bottom - 2), Point(Right - 3, Bottom - 2), Point(Right - 2, Bottom - 3), Point(Right - 2, Top + 2)], cxPCDarkEdgeColor, ACanvas);

          if ATabViewInfo.IsMainTab then
          begin
            if Top = 0 then
            begin
              InternalPolyLine([Point(Left - 1, Top), Point(Left - 1, Top)], cxPCLightestEdgeColor, ACanvas);
              InternalPolyLine([Point(Left - 1, Top + 1), Point(Left - 1, Top + 1)], cxPCLightEdgeColor, ACanvas);
              InternalPolyLine([Point(Left - 1, Top + 2), Point(Left - 1, Bottom - 3)], ATabUnderlineColor, ACanvas);
            end
            else
              InternalPolyLine([Point(Left - 1, Top), Point(Left - 1, Bottom - 3)], ATabUnderlineColor, ACanvas);
              if Bottom = ViewInfo.Height then
              begin
                InternalPolyLine([Point(Left - 1, Bottom - 2), Point(Left - 1, Bottom - 2)], cxPCDarkEdgeColor, ACanvas);
                InternalPolyLine([Point(Left - 1, Bottom - 1), Point(Left - 1, Bottom - 1)], cxPCDarkestEdgeColor, ACanvas);
              end
              else
                InternalPolyLine([Point(Left - 1, Bottom - 2), Point(Left - 1, Bottom - 1)], cxPCDarkEdgeColor, ACanvas);
          end;
        end;
    end;
  PaintTabCorners(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCTabsPainter.InternalPaintFrame(ACanvas: TcxCanvas);
var
  ARect, AOffsets: TRect;
  ATheme: TdxTheme;
  AMinSize: TSize;
begin
  AOffsets := GetNativeContentOffset;
  ARect := GetDrawFrameRect;
  AMinSize := cxSize(AOffsets.Left + AOffsets.Right,
    AOffsets.Top + AOffsets.Bottom);
  if cxRectWidth(ARect) < AMinSize.cx then
    ARect.Right := ARect.Left + AMinSize.cx;
  if cxRectHeight(ARect) < AMinSize.cy then
    ARect.Bottom := ARect.Top + AMinSize.cy;

  ATheme := OpenTheme(totTab);
  if ViewInfo.ParentBackground and (ACanvas.Handle = ViewInfo.Canvas.Handle) and
    IsThemeBackgroundPartiallyTransparent(ATheme, TABP_PANE, 0) then
      cxDrawThemeParentBackground(ViewInfo.IControl.GetControl, ACanvas, ARect)
  else
    ACanvas.FillRect(ARect, ViewInfo.Color);
  DrawThemeBackground(ATheme, ACanvas.Handle, TABP_PANE, 0, ARect);
  if ACanvas.Handle = ViewInfo.Canvas.Handle then
    ACanvas.ExcludeClipRect(GetPageFrameRect);
end;

function TcxPCTabsPainter.UseLookAndFeelTabButton: Boolean;
begin
  Result := True;
end;

{ TcxPCFlatButtonsPainter }

class function TcxPCFlatButtonsPainter.GetStandardStyle: TcxPCStandardStyle;
begin
  Result := tsFlatButtons;
end;

class function TcxPCFlatButtonsPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCFlatButtonsStyle;
end;

class function TcxPCFlatButtonsPainter.GetStyleName: TCaption;
begin
  Result := 'FlatButtons';
end;

class function TcxPCFlatButtonsPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

procedure TcxPCFlatButtonsPainter.PaintTab(ACanvas: TcxCanvas; TabVisibleIndex: Integer);
var
  ATabViewInfo: TcxTabViewInfo;

  procedure InternalPaintTabsDelimiter;

    function IsDelimiterNeeded: Boolean;
    var
      FirstIndex, LastIndex, I: Integer;
      NextRow: Integer;
      IsY: Boolean;
      c: Integer; // coordinate
    begin
      Result := False;
      ViewInfo.InitializeVisibleTabRange(FirstIndex, LastIndex);

      with ViewInfo do
        if ActuallyRotate then
        begin
          if (RowCount = 1) or (ATabViewInfo.VisibleRow = RowCount - 1) then Exit;
          NextRow := ATabViewInfo.VisibleRow + 1;
          IsY := TabPosition in [tpLeft, tpRight];
          c := PointGetter(ATabViewInfo.FullRect.TopLeft, IsY);
          for I := FirstIndex to LastIndex do
            with TabsViewInfo[I] do
              if (VisibleRow = NextRow) and (PointGetter(FullRect.TopLeft, IsY) = c) then
              begin
                Result := True;
                Break;
              end;
        end else
          if (TabVisibleIndex < LastIndex) and
             (TabsViewInfo[TabVisibleIndex + 1].VisibleRow = ATabViewInfo.VisibleRow) then
            Result := True;
    end;

  const
    DelimiterWidth = 2;
  var
    R: TRect;
    TabsDistance: Integer;
    DelimiterLeftBorder, DelimiterTopBorder: Integer;
    DelimiterRect: TRect;
  begin
    if not IsDelimiterNeeded then Exit;
    R := ATabViewInfo.FullRect;
    TabsDistance := StandardPainterTabsNormalDistanceA[GetStandardStyle].dw;

    with R do
      if ViewInfo.IsVerticalText then
      begin
        if ViewInfo.IsBottomToTopAlignment then
        begin
          Bottom := Top;
          Top := Bottom - TabsDistance;
        end
        else
        begin
          Top := Bottom;
          Bottom := Top + TabsDistance;
        end;
        if not InternalSetClipRect(ACanvas, R) then Exit;
        DelimiterTopBorder := Top + (TabsDistance - DelimiterWidth) div 2;
        InternalPolyLine([Point(Left + 1, DelimiterTopBorder), Point(Right - 2, DelimiterTopBorder)], cxPCDarkEdgeColor, ACanvas);
        InternalPolyLine([Point(Left + 1, DelimiterTopBorder + 1), Point(Right - 2, DelimiterTopBorder + 1)], cxPCLightestEdgeColor, ACanvas);
        DelimiterRect := Rect(Left + 1, DelimiterTopBorder, Right - 1, DelimiterTopBorder + 2);
      end
      else
      begin
        if ViewInfo.IsRightToLeftAlignment then
        begin
          Right := Left;
          Left := Right - TabsDistance;
        end
        else
        begin
          Left := Right;
          Right := Left + TabsDistance;
        end;
        if not InternalSetClipRect(ACanvas, R) then
          Exit;
        DelimiterLeftBorder := Left + (TabsDistance - DelimiterWidth) div 2;
        InternalPolyLine([Point(DelimiterLeftBorder, Top + 1), Point(DelimiterLeftBorder, Bottom - 2)], cxPCDarkEdgeColor, ACanvas);
        InternalPolyLine([Point(DelimiterLeftBorder + 1, Top + 1), Point(DelimiterLeftBorder + 1, Bottom - 2)], cxPCLightestEdgeColor, ACanvas);
        DelimiterRect := Rect(DelimiterLeftBorder, Top + 1, DelimiterLeftBorder + 2, Bottom - 1);
      end;
    with ACanvas do
    begin
      SaveClipRegion;
      try
        ExcludeClipRect(DelimiterRect);
        Brush.Style := bsSolid;
        Brush.Color := ViewInfo.Color;
        FillRect(R);
      finally
        RestoreClipRegion;
      end;
    end;

    InternalResetClipRegion(ACanvas);
    ACanvas.ExcludeClipRect(R);
  end;

begin
  inherited;
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  InternalPaintTabsDelimiter;
end;

procedure TcxPCFlatButtonsPainter.PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ATabViewInfo: TcxTabViewInfo;
  AFullRect: TRect;
  ABorderWidth: Integer;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AFullRect := ATabViewInfo.FullRect;
  ABorderWidth := 0;
  if (not ATabViewInfo.IsTracking) and
    (ATabViewInfo.IsPressed or ATabViewInfo.IsSelected or ATabViewInfo.IsMainTab) then
  begin
    InternalDrawEdge(ACanvas, AFullRect, True);
    ABorderWidth := StandardPainterTabBorderWidth;
  end
  else
    if ATabViewInfo.IsHotTrack or ATabViewInfo.IsTracking then
    begin
      ACanvas.DrawComplexFrame(AFullRect, cxPCLightestEdgeColor, cxPCDarkEdgeColor);
      ABorderWidth := 1;
    end;

  ACanvas.FillRect(cxRectInflate(AFullRect, -ABorderWidth), GetTabBodyColor(ATabVisibleIndex));
end;

{ TcxPCStandardPainter }

procedure TcxPCStandardPainter.CalculateButtonContentParameters(
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState;
  out AParameters: TcxPCNavigatorButtonContentParameters);
begin
  AParameters.BrushColor := GetButtonContentColor(AState);
  AParameters.Color := AParameters.BrushColor;
  AParameters.Enabled := ViewInfo.IsEnabled and (AState <> nbsDisabled);
  AParameters.LiteStyle := False;
end;

procedure TcxPCStandardPainter.CalculateButtonsRect;
var
  AButtonHeight, AButtonsWidth: Integer;
begin
  AButtonHeight := ViewInfo.HeaderButtonHeight;
  AButtonsWidth := GetButtonsWidth;
  if ViewInfo.TabPosition in [tpTop, tpBottom] then
  begin
    if ViewInfo.NavigatorPosition in [npLeftTop, npLeftBottom] then
      FButtonsRect.Left := GetButtonsRegionWOffset
    else
      FButtonsRect.Left := ViewInfo.Width - AButtonsWidth - GetButtonsRegionWOffset;

    if ViewInfo.TabPosition = tpTop then
      FButtonsRect.Top := GetButtonsRegionHOffset
    else
      FButtonsRect.Top := ViewInfo.Height - AButtonHeight - GetButtonsRegionHOffset;

    FButtonsRect.Right := FButtonsRect.Left + AButtonsWidth;
    FButtonsRect.Bottom := FButtonsRect.Top + AButtonHeight;
  end
  else
  begin
    if ViewInfo.TabPosition = tpLeft then
      FButtonsRect.Left := GetButtonsRegionHOffset
    else
      FButtonsRect.Left := ViewInfo.Width - AButtonHeight - GetButtonsRegionHOffset;

    if ViewInfo.NavigatorPosition in [npLeftTop, npRightTop] then
      FButtonsRect.Top := GetButtonsRegionWOffset
    else
      FButtonsRect.Top := ViewInfo.Height - AButtonsWidth - GetButtonsRegionWOffset;

    FButtonsRect.Right := FButtonsRect.Left + AButtonHeight;
    FButtonsRect.Bottom := FButtonsRect.Top + AButtonsWidth;
  end;
end;

procedure TcxPCStandardPainter.CalculateButtonsRegion;
var
  AButtonLeftTopCorner: TPoint;
  AIsButtonsRotated: Boolean;
  I: Integer;
  AButtonInfo: TcxPCCustomHeaderButtonViewInfo;
  AButtonBounds: TRect;
begin
  CalculateButtonsRect;
  AButtonLeftTopCorner := FButtonsRect.TopLeft;
  AIsButtonsRotated := ViewInfo.TabPosition in [tpLeft, tpRight];
  for I := 0 to ViewInfo.NavigatorButtonCount - 1 do
  begin
    AButtonInfo := ViewInfo.NavigatorButtonInfos[I];
    AButtonBounds := ViewInfo.NavigatorButtonInfos[I].Bounds;
    AButtonBounds.TopLeft := AButtonLeftTopCorner;
    if AIsButtonsRotated then
    begin
      AButtonBounds.Bottom := AButtonBounds.Top + AButtonInfo.GetWidth;
      AButtonBounds.Right := AButtonBounds.Left + ViewInfo.HeaderButtonHeight;
      if I < ViewInfo.NavigatorButtonCount - 1 then
        AButtonLeftTopCorner.Y := AButtonBounds.Bottom + ViewInfo.GetHeaderButtonsDistance(AButtonInfo,
          ViewInfo.NavigatorButtonInfos[I + 1]);
    end
    else
    begin
      AButtonBounds.Right := AButtonBounds.Left + AButtonInfo.GetWidth;
      AButtonBounds.Bottom := AButtonBounds.Top + ViewInfo.HeaderButtonHeight;
      if I < ViewInfo.NavigatorButtonCount - 1 then
        AButtonLeftTopCorner.X := AButtonBounds.Right + ViewInfo.GetHeaderButtonsDistance(AButtonInfo,
          ViewInfo.NavigatorButtonInfos[I + 1]);
    end;
    ViewInfo.NavigatorButtonInfos[I].Bounds := AButtonBounds;
  end;
end;

function TcxPCStandardPainter.CalculateButtonsRegionWidth: Integer;
begin
  Result := GetButtonsWidth;
  if Result > 0 then
  begin
    Inc(Result, GetButtonsRegionWOffset);
    Inc(Result, GetButtonsRegionFromTabsOffset);
  end;
end;

procedure TcxPCStandardPainter.CorrectTabHeightForImage(var AHeight: Integer);
var
  ATabImageHeight: Integer;
begin
  ATabImageHeight := GetTabBaseImageSize.cy;
  if ViewInfo.ActuallyRotate and (ATabImageHeight > 0) and Odd(AHeight - ATabImageHeight) then
    Inc(AHeight);
end;

procedure TcxPCStandardPainter.DoPaintButton(ACanvas: TcxCanvas;
  AButtonInfo: TcxPCCustomHeaderButtonViewInfo);
var
  AHeaderButtonInfo: TcxPCHeaderButtonViewInfo;
begin
  if AButtonInfo.IsNavigatorButton then
    PaintButton(ACanvas, AButtonInfo.Bounds, AButtonInfo.State,
      GetRightToLeftDependentButtonType(TcxPCNavigatorButtonViewInfo(AButtonInfo).ButtonType))
  else
  begin
    AHeaderButtonInfo := TcxPCHeaderButtonViewInfo(AButtonInfo);
    PaintHeaderButton(ACanvas, AHeaderButtonInfo);
    DrawHeaderButtonGlyph(ACanvas, AHeaderButtonInfo);
  end;
end;

function TcxPCStandardPainter.CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer;
begin
  if ATabViewInfo.GetDefinedWidth > 0 then
    Result := ATabViewInfo.GetDefinedWidth
  else
    Result := InternalCalculateTabNormalWidth(ATabViewInfo.VisibleIndex);
end;

procedure TcxPCStandardPainter.CorrectTabNormalWidth(var AValue: Integer);
begin
  AValue := Max(AValue, GetDefaultTabNormalWidth);
end;

procedure TcxPCStandardPainter.DoDrawTabButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxPCNavigatorButtonState);
begin
  DrawButtonFrameAndBackround(ACanvas, ARect, AState);
end;

procedure TcxPCStandardPainter.DoDrawTabCloseButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxPCNavigatorButtonState);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  DrawButtonFrameAndBackround(ACanvas, ARect, AState);
  APainter := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
  APainter.DrawScaledButtonCross(ACanvas, ARect,
    APainter.ButtonSymbolColor(NavigatorBtnStateToLookAndFeelBtnState[AState]),
    NavigatorBtnStateToLookAndFeelBtnState[AState], ScaleFactor);
end;

procedure TcxPCStandardPainter.DrawButtonFrameAndBackround(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxPCNavigatorButtonState);
var
  R: TRect;
begin
  R := ARect;
  PaintButtonFrame(ACanvas, R, AState);
  PaintButtonBackground(ACanvas, R, AState);
end;

function TcxPCStandardPainter.DoGetCloseButtonSize: TSize;
begin
  if UseLookAndFeelTabButton then
    Result := GetLookAndFeelPainter.ScaledSmallCloseButtonSize(ScaleFactor)
  else
    Result := cxSize(DoGetButtonWidth(nbClose), DoGetButtonHeight);
end;

function TcxPCStandardPainter.CalculateTabNormalHeight: Integer;
begin
  Result := InternalCalculateTabNormalHeight;
  Result := Max(Result, GetDefaultTabNormalHeight);
  CorrectTabHeightForImage(Result);
  Result := Max(Result, ViewInfo.TabsViewInfo.GetTabDefaultHeight);
  Result := Max(Result, IfThen(ViewInfo.HasTabButtons, GetCloseButtonAreaHeight(-1)));
end;

function TcxPCStandardPainter.DoGetDefaultTabNormalHeight: Integer;
begin
  Result := ScaleFactor.Apply(StandardPainterDefaultTabNormalHeightA[GetStandardStyle]);
end;

function TcxPCStandardPainter.GetDefaultTabNormalHeight: Integer;
begin
  Result := DoGetDefaultTabNormalHeight;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxPCStandardPainter.GetDefaultTabNormalHTextOffset: Integer;
begin
  Result := StandardPainterDefaultTabNormalHTextOffsetA[GetStandardStyle];
end;

function TcxPCStandardPainter.GetDefaultTabNormalWidth: Integer;
begin
  Result := 42;
end;

function TcxPCStandardPainter.GetDrawFrameRect: TRect;
var
  ABorders: TcxBorders;
begin
  Result := GetPageFrameRect;
  ABorders := GetPageBorders;
  if not (bLeft in ABorders) then
    Dec(Result.Left, GetBorderWidths.Left);
  if not (bTop in ABorders) then
    Dec(Result.Top, GetBorderWidths.Top);
  if not (bRight in ABorders) then
    Inc(Result.Right, GetBorderWidths.Right);
  if not (bBottom in ABorders) then
    Inc(Result.Bottom, GetBorderWidths.Bottom);
end;

function TcxPCStandardPainter.GetPageFrameRect: TRect;
var
  AMinFrameRectSize: Integer;
begin
  Result := inherited GetPageFrameRect;
  if IsRectEmpty(Result) then Exit;
  AMinFrameRectSize := GetMinFrameRectSize;
  if cxRectWidth(Result) < AMinFrameRectSize then
    Result.Right := Result.Left + AMinFrameRectSize;
  if cxRectHeight(Result) < AMinFrameRectSize then
    Result.Bottom := Result.Top + AMinFrameRectSize;
end;

function TcxPCStandardPainter.GetMinFrameRectSize: Integer;
begin
  Result := GetFrameWidth * 2;
end;

function TcxPCStandardPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := ScaleFactor.Apply(StandardPainterTabContentWOffsetA[IsTabBorderThick(ATabVisibleIndex), GetStandardStyle]);
end;

function TcxPCStandardPainter.GetDrawImageOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := StandardPainterDrawImageOffsetA[IsTabBorderThick(TabVisibleIndex), GetStandardStyle];
end;

function TcxPCStandardPainter.GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := cxPCEmptyWOffset;
end;

function TcxPCStandardPainter.GetDrawTextHOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := StandardPainterDrawTextHOffsetA[IsTabBorderThick(TabVisibleIndex),
    GetStandardStyle];
end;

function TcxPCStandardPainter.GetFrameWidth: Integer;
begin
  Result := StandardPainterTabControlFrameBorderWidth;
end;

function TcxPCStandardPainter.GetGoDialogPosition(ASize: TSize): TPoint;
const
  TabAndNavigatorPosToGoDialoPos: array[TcxTabPosition, TcxPCNavigatorPosition] of TcxPCNavigatorPosition = (
    (npLeftTop, npLeftTop, npRightTop, npRightTop),
    (npLeftBottom, npLeftBottom, npRightBottom, npRightBottom),
    (npLeftTop, npLeftBottom, npLeftTop, npLeftBottom),
    (npRightTop, npRightBottom, npRightTop, npRightBottom)
  );
var
  AGoDialogPosition: TcxPCNavigatorPosition;
  AGoDialogInfo: TcxPCNavigatorButtonViewInfo;
begin
  AGoDialogInfo := ViewInfo.NavigatorButtonInfoByType[nbGoDialog];
  AGoDialogPosition := TabAndNavigatorPosToGoDialoPos[ViewInfo.TabPosition, ViewInfo.NavigatorPosition];
  if AGoDialogPosition in [npLeftTop, npLeftBottom] then
    Result.X := AGoDialogInfo.Bounds.Left
  else
    Result.X := AGoDialogInfo.Bounds.Right - ASize.cx;
  if AGoDialogPosition in [npLeftTop, npRightTop] then
    Result.Y := AGoDialogInfo.Bounds.Bottom
  else
    Result.Y := AGoDialogInfo.Bounds.Top - ASize.cy;
end;

function TcxPCStandardPainter.GetHeaderButtonHeightCorrection: Integer;
begin
  if ViewInfo.HeaderButtonHeight > GetButtonHeight then
    Result := ViewInfo.HeaderButtonHeight - GetButtonHeight
  else
    Result := 0;
end;

function TcxPCStandardPainter.GetImageTextDistance(ATabVisibleIndex: Integer): Integer;
begin
  Result := 6;
end;

function TcxPCStandardPainter.GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := Max(GetTabNormalWidth(ATabVisibleIndex), GetDefaultTabNormalWidth);
end;

function TcxPCStandardPainter.GetMinTabSelectionDistance: TcxPCDistance;
begin
  Result := MinTabSelectionDistance;
end;

function TcxPCStandardPainter.GetTabsNormalDistance: TcxPCDistance;
begin
  Result := StandardPainterTabsNormalDistanceA[GetStandardStyle];
end;

function TcxPCStandardPainter.GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer;
begin
  Result := ScaleFactor.Apply(12);
end;

function TcxPCStandardPainter.GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabImageAreaWidth + GetImageTextDistance(ATabVisibleIndex);
end;

function TcxPCStandardPainter.GetTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabNormalContentOffset(ATabVisibleIndex);
  if IsAssignedImages and ((ATabVisibleIndex = -1) or IsTabHasImage(ATabVisibleIndex) or ViewInfo.ActuallyRotate) then
    Inc(Result, GetTabNormalImageAreaWidth(ATabVisibleIndex));
  if (ATabVisibleIndex <> -1) and TabViewInfo[ATabVisibleIndex].HasButtons then
    Inc(Result, GetTabButtonsAreaWidth(ATabVisibleIndex));
end;

function TcxPCStandardPainter.GetTabsRectOffset: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxPCStandardPainter.GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := TcxTabViewInfoAccess(TabViewInfo[ATabVisibleIndex]).TextWidth;
end;

function TcxPCStandardPainter.GetTabsPosition: TcxPCTabsPosition;
var
  AButtonsWidth: Integer;
  ATabsContainerOffset: TRect;
begin
  AButtonsWidth := CalculateButtonsRegionWidth;
  ATabsContainerOffset := cxEmptyRect;
  if ViewInfo.IsTabsContainer then
    ATabsContainerOffset := GetTabsContainerOffsets;
  Inc(ATabsContainerOffset.Top, GetHeaderButtonHeightCorrection);
  with ViewInfo, Result do
  begin
    Result.ExtendedTabsRect := cxRectContent(ViewInfo.ControlBounds, GetTabsRectOffset);

    if NavigatorButtonCount > 0 then
    begin
      ExtendedTabsRect := cxRectContent(ExtendedTabsRect, RotateRect(Rect(0, ATabsContainerOffset.Top, 0, 0), TabPosition));
      ATabsContainerOffset := RotateRect(ATabsContainerOffset, TabPosition);
      if TabPosition in [tpTop, tpBottom] then
      begin
        if NavigatorPosition in [npLeftTop, npLeftBottom] then
        begin
          Inc(ExtendedTabsRect.Left, AButtonsWidth);
          Dec(ExtendedTabsRect.Right, ATabsContainerOffset.Right);
        end
        else
        begin
          Dec(ExtendedTabsRect.Right, AButtonsWidth);
          Inc(ExtendedTabsRect.Left, ATabsContainerOffset.Left);
        end;
      end
      else
      begin
        if NavigatorPosition in [npLeftTop, npRightTop] then
        begin
          Inc(ExtendedTabsRect.Top, AButtonsWidth);
          Dec(ExtendedTabsRect.Bottom, ATabsContainerOffset.Bottom);
        end
        else
        begin
          Dec(ExtendedTabsRect.Bottom, AButtonsWidth);
          Inc(ExtendedTabsRect.Top, ATabsContainerOffset.Top);
        end;
      end;
    end
    else
      ExtendedTabsRect := cxRectContent(ExtendedTabsRect, RotateRect(ATabsContainerOffset, TabPosition));
    if TabPosition in [tpTop, tpBottom] then
      NormalRowWidth := (ExtendedTabsRect.Right - ExtendedTabsRect.Left)
    else
      NormalRowWidth := (ExtendedTabsRect.Bottom - ExtendedTabsRect.Top);

    NormalTabsRect := ExtendedTabsRect;

    ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 0;
    ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := 0;

    MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects :=
      GetTabsNormalDistance.dh;
  end;

  CalculateButtonsRegion;
end;

function TcxPCStandardPainter.GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := StandardPainterTooNarrowTabContentWOffsetA[
    IsTabBorderThick(ATabVisibleIndex), GetStandardStyle];
  if IsNativePainting and (TabViewInfo[ATabVisibleIndex].GetRelativeTextRotationAngle = raPlus90) then
    ExchangeLongWords(Result.Left, Result.Right);
end;

function TcxPCStandardPainter.InternalCalculateTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabNormalWidth(ATabVisibleIndex);
  if TabViewInfo[ATabVisibleIndex].HasCaption then
    Inc(Result, GetTabTextNormalWidth(ATabVisibleIndex));
  TabViewInfo[ATabVisibleIndex].CorrectTabNormalWidth(Result);
end;

function TcxPCStandardPainter.InternalCalculateTabNormalHeight: Integer;
begin
  Result := IfThen(IsAssignedImages, GetTabImageAreaHeight + ScaleFactor.Apply(6));
  Result := Max(Result, GetMaxTabCaptionHeight + ScaleFactor.Apply(8));
end;

procedure TcxPCStandardPainter.InternalDrawEdge(ACanvas: TcxCanvas;
  const ARect: TRect; ASunken: Boolean; AThinFrame: Boolean = False);
const
  ThickFrameLineColorA: array [Boolean, 1 .. 4] of TColor = (
    (cxPCLightEdgeColor, cxPCLightestEdgeColor, cxPCDarkestEdgeColor, cxPCDarkEdgeColor),
    (cxPCDarkEdgeColor, cxPCDarkestEdgeColor, cxPCLightestEdgeColor, cxPCLightEdgeColor)
  );
  ThinFrameLineColorA: array [Boolean, 1 .. 2] of TColor = (
    (cxPCLightEdgeColor, cxPCDarkestEdgeColor), (cxPCDarkestEdgeColor, cxPCLightEdgeColor)
  );
begin
  if AThinFrame then
    ACanvas.DrawComplexFrame(ARect, ThinFrameLineColorA[ASunken, 1],
      ThinFrameLineColorA[ASunken, 2])
  else
  begin
    ACanvas.DrawComplexFrame(ARect, ThickFrameLineColorA[ASunken, 1],
      ThickFrameLineColorA[ASunken, 3]);
    ACanvas.DrawComplexFrame(cxRectInflate(ARect, -1, -1), ThickFrameLineColorA[ASunken, 2],
      ThickFrameLineColorA[ASunken, 4]);
  end;
end;

procedure TcxPCStandardPainter.InternalDrawFocusRect(ACanvas: TcxCanvas; TabVisibleIndex: Integer; R: TRect);
begin
  if TabViewInfo[TabVisibleIndex].IsFocused and ViewInfo.IsFocused then
  begin
    ACanvas.Pen.Color := ViewInfo.Font.Color; // DrawTabEx
    ACanvas.DrawFocusRect(cxRectInflate(R, -ScaleFactor.Apply(1)));
  end;
end;

procedure TcxPCStandardPainter.InternalPaintDragImage(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
begin
  if not DoCustomDraw(ATabVisibleIndex) then
    InternalPaintTab(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCStandardPainter.InternalPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ABitmap: TcxBitmap;
  ATabRect: TRect;
  ATabViewInfo: TcxTabViewInfo;
  ATabOrigin: TPoint;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  ATabRect := ATabViewInfo.FullRect;
  ABitmap := TcxBitmap.CreateSize(0, 0, pf32bit);
  ABitmap.Canvas.Lock;
  try
    if ATabViewInfo.IsMainTab then
      ATabRect := GetExtendedRect(ATabRect, Rect(0, 0, 0, -1), ATabViewInfo.PaintingPosition);
    ABitmap.SetSize(ATabRect);
    ATabOrigin := ATabRect.TopLeft;
    ABitmap.cxCanvas.WindowOrg := ATabOrigin;
    DrawTabContentBackground(ABitmap.cxCanvas, ATabRect, GetTabBodyColor(ATabVisibleIndex), ATabVisibleIndex);
    PaintTabShape(ABitmap.cxCanvas, ATabVisibleIndex);
    DrawTabImageAndText(ABitmap.cxCanvas, ATabVisibleIndex);
    DrawFocusRect(ABitmap.cxCanvas, ATabVisibleIndex);
    cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, ATabRect, ATabOrigin, SRCCOPY);
  finally
    ABitmap.Canvas.Unlock;
    FreeAndNil(ABitmap);
  end;
end;

procedure TcxPCStandardPainter.PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton);
var
  AContentLeftTopCorner: TPoint;
  AContentParameters: TcxPCNavigatorButtonContentParameters;
begin
  DrawButtonFrameAndBackround(ACanvas, ARect, AState);
  CalculateButtonContentParameters(AType, AState, AContentParameters);
  AContentLeftTopCorner := cxPointOffset(GetButtonContentPosition(ARect, AType, AState), ARect.TopLeft);
  DrawButtonContent(ACanvas, AType, AContentParameters, AContentLeftTopCorner);
end;

procedure TcxPCStandardPainter.PaintButtonBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxPCNavigatorButtonState);
begin
  ACanvas.Brush.Color := GetButtonColor(AState);
  ACanvas.FillRect(ARect);
end;

procedure TcxPCStandardPainter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);
begin
  InternalDrawEdge(ACanvas, ARect, AButtonState = nbsPressed);
  ARect := cxRectInflate(ARect, -StandardPainterTabBorderWidth);
end;

procedure TcxPCStandardPainter.PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo);
begin
  DrawButtonFrameAndBackround(ACanvas, AButtonInfo.Bounds, AButtonInfo.State);
end;

procedure TcxPCStandardPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
  PaintFrameBorder(ACanvas, GetDrawFrameRect);
  inherited DoPaintPageFrame(ACanvas);
end;

procedure TcxPCStandardPainter.PaintFrameBorder(ACanvas: TcxCanvas; R: TRect);
var
  ABorders: TcxBorders;
begin
  ABorders := GetPageBorders;
  ACanvas.DrawComplexFrame(R, cxPCLightestEdgeColor, cxPCDarkestEdgeColor, ABorders);
  InflateRect(R, -1, -1);
  ACanvas.DrawComplexFrame(R, cxPCLightEdgeColor, cxPCDarkEdgeColor, ABorders);
end;

class function TcxPCStandardPainter.IsMainTabBoundWithClient: boolean;
const
  IsMainTabBoundWithClientA: array [TcxPCStandardStyle] of boolean = (True, False, False);
begin
  Result := IsMainTabBoundWithClientA[GetStandardStyle];
end;

class function TcxPCStandardPainter.IsMultiSelectionAccepted: boolean;
const
  IsMultiSelectionAcceptedA: array [TcxPCStandardStyle] of boolean = (False, True, True);
begin
  Result := IsMultiSelectionAcceptedA[GetStandardStyle];
end;

class function TcxPCStandardPainter.IsStandardStyle: Boolean;
begin
  Result := True;
end;

function TcxPCStandardPainter.IsTabBorderThick(ATabVisibleIndex: Integer): Boolean;
begin
  Result := TabViewInfo[ATabVisibleIndex].IsMainTab;
end;

function TcxPCStandardPainter.NeedButtonContentPositionCentered: Boolean;
begin
  Result := (GetHeaderButtonHeightCorrection <> 0) or cxIsTouchModeEnabled or ScaleFactor.Assigned;
end;

class function TcxPCStandardPainter.IsTabPressable: Boolean;
const
  IsTabPressableA: array [TcxPCStandardStyle] of boolean = (False, True,
    True);
begin
  Result := IsTabPressableA[GetStandardStyle];
end;

procedure TcxPCStandardPainter.PaintButtonsRegion(ACanvas: TcxCanvas);
var
  I: Integer;
  AButtonInfo: TcxPCCustomHeaderButtonViewInfo;
begin
  for I := 0 to ViewInfo.NavigatorButtonCount - 1 do
  begin
    AButtonInfo := ViewInfo.NavigatorButtonInfos[I];
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipRegion(TcxRegion.Create(AButtonInfo.Bounds), roIntersect);
      DoPaintButton(ACanvas, AButtonInfo);
    finally
      ACanvas.RestoreClipRegion;
    end;
    ACanvas.ExcludeClipRect(AButtonInfo.Bounds);
  end;
end;

procedure TcxPCStandardPainter.PaintTabsRegion(ACanvas: TcxCanvas);
var
  I: Integer;
  FirstIndex, LastIndex: Integer;
begin
  ViewInfo.InitializeVisibleTabRange(FirstIndex, LastIndex);
  for I := FirstIndex to LastIndex do
    PaintTab(ACanvas, I);
end;

procedure TcxPCStandardPainter.RepaintButtonsRegion;
begin
  if ViewInfo.NavigatorButtonCount > 0 then
    ViewInfo.InvalidateRect(ButtonsRect, False);
end;

procedure TcxPCStandardPainter.RepaintTab(TabVisibleIndex: Integer;
  TabPropertyChanged: TcxPCTabPropertyChanged);
begin
  InvalidateTabRect(TabVisibleIndex);
end;

function TcxPCStandardPainter.DoGetButtonHeight: Integer;
begin
  Result := ScaleFactor.Apply(StandardPainterButtonHeight);
end;

function TcxPCStandardPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(StandardPainterButtonWidthA[ViewInfo.TabPosition in [tpLeft, tpRight], Button]);
end;

procedure TcxPCStandardPainter.DrawButtonContent(ACanvas: TcxCanvas; AButton: TcxPCNavigatorButton;
  const AParameters: TcxPCNavigatorButtonContentParameters; AContentRectLeftTopCorner: TPoint);
type
  TArrowPointA = array [0..2] of TPoint;

  procedure InternalDrawArrow(Points: TArrowPointA; LeftTopCorner: TPoint);
  var
    I: Integer;
  begin
    for I := 0 to 2 do
      Points[I] := cxPointOffset(Points[I], LeftTopCorner);
    ACanvas.Polygon(Points);
  end;

  procedure DrawArrow(const ALeftTopCorner: TPoint);
  const
    PolygonPointsMultipliersA: array[TcxPCArrow] of TArrowPointA = (
      ((X: 0; Y: 1), (X: 1; Y: 0), (X: 2; Y: 1)),
      ((X: 0; Y: 0), (X: 2; Y: 0), (X: 1; Y: 1)),
      ((X: 0; Y: 1), (X: 1; Y: 0), (X: 1; Y: 2)),
      ((X: 0; Y: 0), (X: 1; Y: 1), (X: 0; Y: 2))
    );
  var
    APolygonPoints: TArrowPointA;
    AArrowUnit: Integer;
    I: Integer;
  begin
    AArrowUnit := ScaleFactor.Apply(cxPCArrowSizeA[AButton] - 1);
    APolygonPoints := PolygonPointsMultipliersA[GetButtonArrow(AButton)];
    for I := 0 to 2 do
      APolygonPoints[I] := cxPointScale(APolygonPoints[I], AArrowUnit, 1);
    InternalDrawArrow(APolygonPoints, ALeftTopCorner);
  end;

  procedure DrawCross(const ALeftTopCorner: TPoint; ASize: Integer);
  begin
    with ALeftTopCorner do
      if AParameters.LiteStyle then
        if ViewInfo.TabPosition in [tpTop, tpBottom] then
        begin
          InternalPolyLine([Point(X + 1, Y), Point(X + ASize - 2, Y + ASize - 3)], ACanvas.Pen.Color, ACanvas);
          InternalPolyLine([Point(X, Y), Point(X + ASize - 3, Y + ASize - 3)], ACanvas.Pen.Color, ACanvas);

          InternalPolyLine([Point(X, Y + ASize - 3), Point(X + ASize - 3, Y)], ACanvas.Pen.Color, ACanvas);
          InternalPolyLine([Point(X + 1, Y + ASize - 3), Point(X + ASize - 2, Y)], ACanvas.Pen.Color, ACanvas);
        end
        else
        begin
          InternalPolyLine([Point(X, Y), Point(X + ASize - 3, Y + ASize - 3)], ACanvas.Pen.Color, ACanvas);
          InternalPolyLine([Point(X, Y + 1), Point(X + ASize - 3, Y + ASize - 2)], ACanvas.Pen.Color, ACanvas);

          InternalPolyLine([Point(X, Y + ASize - 3), Point(X + ASize - 3, Y)], ACanvas.Pen.Color, ACanvas);
          InternalPolyLine([Point(X, Y + ASize - 2), Point(X + ASize - 3, Y + 1)], ACanvas.Pen.Color, ACanvas);
        end
      else
      begin
        InternalPolyLine([Point(X + 1, Y), Point(X + ASize - 1, Y + ASize - 2)], ACanvas.Pen.Color, ACanvas);
        InternalPolyLine([Point(X, Y), Point(X + ASize - 1, Y + ASize - 1)], ACanvas.Pen.Color, ACanvas);
        InternalPolyLine([Point(X, Y + 1), Point(X + ASize - 2, Y + ASize - 1)], ACanvas.Pen.Color, ACanvas);

        InternalPolyLine([Point(X, Y + ASize - 2), Point(X + ASize - 2, Y)], ACanvas.Pen.Color, ACanvas);
        InternalPolyLine([Point(X, Y + ASize - 1), Point(X + ASize - 1, Y)], ACanvas.Pen.Color, ACanvas);
        InternalPolyLine([Point(X + 1, Y + ASize - 1), Point(X + ASize - 1, Y + 1)], ACanvas.Pen.Color, ACanvas);
      end;
  end;

  procedure InternalDrawContent(const ALeftTopCorner: TPoint);
  begin
    if AButton = nbClose then
      DrawCross(ALeftTopCorner, ScaleFactor.Apply(CloseButtonCrossSize))
    else
      DrawArrow(ALeftTopCorner);
  end;

begin
  if AParameters.Enabled then
  begin
    ACanvas.Brush.Color := AParameters.BrushColor;
    ACanvas.Pen.Color := AParameters.Color;
  end
  else
  begin
    ACanvas.Brush.Color := DisabledTextFaceColor;
    ACanvas.Pen.Color := DisabledTextFaceColor;
    with AContentRectLeftTopCorner do
      InternalDrawContent(Point(X + 1, Y + 1));
    ACanvas.Brush.Color := DisabledTextShadowColor;
    ACanvas.Pen.Color := DisabledTextShadowColor;
  end;
  InternalDrawContent(AContentRectLeftTopCorner);
  ACanvas.Brush.Style := bsSolid;
end;

function TcxPCStandardPainter.Get3DButtonContentPosition(
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;
const
  AArrowButtonArrowRectLeftTopCorners: array [TcxPCArrow] of TPoint = (
    (X: 5; Y: 7), (X: 5; Y: 7), (X: 6; Y: 5), (X: 8; Y: 5)
  );
  ACloseButtonCrossRectLeftTopCorner: TPoint = (X: 5; Y: 5);
  AGoDialogArrowRectLeftTopCorners: array [Boolean] of TPoint = (
    (X: 2; Y: 8), // TabPosition in [tpTop, tpBottom]
    (X: 6; Y: 4)
  );
begin
  case AButton of
    nbGoDialog:
      Result := AGoDialogArrowRectLeftTopCorners[ViewInfo.TabPosition in [tpLeft, tpRight]];
    nbClose:
      Result := ACloseButtonCrossRectLeftTopCorner;
  else
    Result := AArrowButtonArrowRectLeftTopCorners[GetButtonArrow(AButton)];
  end;
  if AState = nbsPressed then
  begin
    Inc(Result.X);
    Inc(Result.Y);
  end;
end;

function TcxPCStandardPainter.GetButtonArrow(AButton: TcxPCNavigatorButton): TcxPCArrow;
begin
  if AButton = nbGoDialog then
    Result := aBottom
  else
    Result := cxPCArrowConvertionA[AButton, ViewInfo.TabPosition in [tpLeft, tpRight]];
end;

function TcxPCStandardPainter.GetButtonCenteredContentPosition(
  const ARect: TRect; AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;

  procedure RotateSize(var ASize: TSize);
  begin
    ASize := cxSize(ASize.cy, ASize.cx);
  end;

  function GetContentSize: TSize;
  var
    AArrowSize: Integer;
  begin
    if AButton = nbClose then
    begin
      Result := cxSize(CloseButtonCrossSize - 1, CloseButtonCrossSize - 2);
      if ViewInfo.TabPosition in [tpLeft, tpRight] then
        RotateSize(Result);
    end
    else
    begin
      AArrowSize := cxPCArrowSizeA[AButton];
      Result := cxSize(AArrowSize, AArrowSize * 2 - 1);
      if (AButton = nbGoDialog) or (AButton in [nbTopLeft, nbBottomRight]) and (ViewInfo.TabPosition in [tpLeft, tpRight]) then
        RotateSize(Result);
    end;
    Result := ScaleFactor.Apply(Result);
  end;

var
  AButtonSize, AContentSize: TSize;
begin
  AContentSize := GetContentSize;

  AButtonSize := cxSize(ARect);
   // AButtonSize := Size(GetButtonWidth(AButton), GetButtonHeight);
  //if ViewInfo.TabPosition in [tpLeft, tpRight] then
  //  RotateSize(AButtonSize);

  Result.X := (AButtonSize.cx - AContentSize.cx) div 2;
  Result.Y := (AButtonSize.cy - AContentSize.cy) div 2;
  if AButton = nbGoDialog then
    Inc(Result.Y);
  if AState = nbsPressed then
  begin
    if AButton <> nbGoDialog then
      Inc(Result.X);
    Inc(Result.Y);
  end;
end;

function TcxPCStandardPainter.GetButtonColor(
  AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  Result := cxPCTabBodyColor;
end;

function TcxPCStandardPainter.GetButtonContentColor(
  AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if AButtonState = nbsHotTrack then
    Result := GetHotTrackColor
  else
    Result := clBtnText;
end;

function TcxPCStandardPainter.GetButtonContentPosition(const ARect: TRect;
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;
begin
  Result := GetButtonCenteredContentPosition(ARect, AButton, AState);
end;

procedure TcxPCStandardPainter.PaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AIsCustomDraw: Boolean;
  ARegion: TcxRegion;
  ARgnOperation: TcxRegionOperation;
begin
  ARegion := GetTabClipRgn(ACanvas, ATabVisibleIndex);
  ARgnOperation := GetTabClipRgnOperation(ATabVisibleIndex);
  try
    ACanvas.SaveClipRegion;
    try
      ACanvas.SetClipRegion(ARegion, ARgnOperation, False);
      AIsCustomDraw := DoCustomDraw(ATabVisibleIndex);
      if not AIsCustomDraw then
        InternalPaintTab(ACanvas, ATabVisibleIndex);
    finally
      ACanvas.RestoreClipRegion;
    end;
    if not IsTabTransparent(ATabVisibleIndex) or AIsCustomDraw then
    begin
      AfterPaintTab(ACanvas, ATabVisibleIndex);
      ACanvas.SetClipRegion(ARegion, roSubtract, False);
    end
    else
      ExcludeTabContentClipRegion(ACanvas, ATabVisibleIndex);
  finally
    ARegion.Free;
  end;
end;

function TcxPCStandardPainter.GetButtonsWidth: Integer;
var
  I: Integer;
  AButtonInfo: TcxPCCustomHeaderButtonViewInfo;
begin
  Result := 0;
  for I := 0 to ViewInfo.NavigatorButtonCount - 1 do
  begin
    AButtonInfo := ViewInfo.NavigatorButtonInfos[I];
    Inc(Result, AButtonInfo.GetWidth);
    if I < ViewInfo.NavigatorButtonCount - 1 then
      Inc(Result, ViewInfo.GetHeaderButtonsDistance(AButtonInfo,
        ViewInfo.NavigatorButtonInfos[I + 1]));
  end;
end;

function TcxPCStandardPainter.GetRightToLeftDependentButtonType(AButton: TcxPCNavigatorButton): TcxPCNavigatorButton;
begin
  Result := AButton;
  if ViewInfo.UseRightToLeftAlignment and (ViewInfo.TabPosition in [tpTop, tpBottom]) then
    case AButton of
      nbTopLeft:
        Result := nbBottomRight;
      nbBottomRight:
        Result := nbTopLeft;
    end;
end;

{ TcxPCButtonsPainter }

class function TcxPCButtonsPainter.GetStandardStyle: TcxPCStandardStyle;
begin
  Result := tsButtons;
end;

class function TcxPCButtonsPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCButtonsStyle;
end;

class function TcxPCButtonsPainter.GetStyleName: TCaption;
begin
  Result := 'Buttons';
end;

class function TcxPCButtonsPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

function TcxPCButtonsPainter.IsTabBorderThick(ATabVisibleIndex: Integer): Boolean;
begin
  with TabViewInfo[ATabVisibleIndex] do
    Result := IsTracking or IsPressed or IsSelected or IsMainTab;
end;

procedure TcxPCButtonsPainter.PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
const
  ABorderWidth = 2;
var
  ATabViewInfo: TcxTabViewInfo;
  AIsSunkenTab: Boolean;
  AInnerRect, AOuterRect, ABackgroundRect: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AIsSunkenTab := ATabViewInfo.IsPressed or ATabViewInfo.IsSelected or
    ATabViewInfo.IsMainTab or ATabViewInfo.IsTracking;
  AOuterRect := ATabViewInfo.FullRect;
  AInnerRect := cxRectInflate(AOuterRect, -1, -1);
  ABackgroundRect := cxRectInflate(AOuterRect, -ABorderWidth, -ABorderWidth);
  if AIsSunkenTab then
  begin
    ACanvas.DrawComplexFrame(AOuterRect, cxPCDarkestEdgeColor, cxPCLightestEdgeColor);
    ACanvas.DrawComplexFrame(AInnerRect, cxPCDarkEdgeColor, cxPCLightEdgeColor);
  end
  else
  begin
    ACanvas.DrawComplexFrame(AOuterRect, cxPCLightestEdgeColor, cxPCDarkestEdgeColor);
    ACanvas.DrawComplexFrame(AInnerRect, cxPCLightEdgeColor, cxPCDarkEdgeColor);
  end;
  ACanvas.FillRect(ABackgroundRect, GetTabBodyColor(ATabVisibleIndex));
end;

{ TcxPCButtonedPainter }

constructor TcxPCButtonedPainter.Create(AViewInfo: TcxCustomTabControlViewInfo);

  procedure InitializeMainTabBrushBitmap;
  const
    BrushBitmapSize = 8;
  var
    X, Y, XStart: Integer;
  begin
    with MainTabBrushBitmap, MainTabBrushBitmap.Canvas do
    begin
      Width := BrushBitmapSize;
      Height := BrushBitmapSize;

      Brush.Style := bsSolid;
      Brush.Color := cxPCLightEdgeColor;
      FillRect(Rect(0, 0, BrushBitmapSize, BrushBitmapSize));

      XStart := 0;
      X := XStart;
      Y := 0;
      repeat
        Pixels[X, Y] := cxPCLightestEdgeColor;
        Inc(X, 2);
        if X >= BrushBitmapSize then
        begin
          XStart := 1 - XStart;
          X := XStart;
          Inc(Y);
        end;
      until Y = BrushBitmapSize;
    end;
  end;

begin
  inherited;
  MainTabBrushBitmap := TBitmap.Create;
  InitializeMainTabBrushBitmap;
end;

destructor TcxPCButtonedPainter.Destroy;
begin
  MainTabBrushBitmap.Free;
  inherited Destroy;
end;

function TcxPCButtonedPainter.GetButtonContentPosition(const ARect: TRect;
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;
begin
  if NeedButtonContentPositionCentered then
    Result := inherited GetButtonContentPosition(ARect, AButton, AState)
  else
    Result := Get3DButtonContentPosition(AButton, AState);
end;

function TcxPCButtonedPainter.GetButtonsRegionHOffset: Integer;
begin
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset
  else
    Result := 0;
end;

function TcxPCButtonedPainter.GetButtonsRegionWOffset: Integer;
begin
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset
  else
    Result := 0;
end;

function TcxPCButtonedPainter.GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := inherited GetDrawImageWithoutTextWOffset(TabVisibleIndex);
  CorrectContentWOffset(TabVisibleIndex, Result);
end;

function TcxPCButtonedPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
end;

function TcxPCButtonedPainter.GetTabBodyColor(TabVisibleIndex: Integer): TColor;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  if ATabViewInfo.IsHighlighted then
    Result := HighlightedTabBodyColor
  else
  begin
    Result := GetTabColor(TabVisibleIndex);
    if Result = clDefault then
      if ATabViewInfo.IsMainTab then
        Result := cxPCLightestEdgeColor
      else
        Result := clBtnFace;
  end;
end;

function TcxPCButtonedPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := ButtonsPainterTabContentWOffsetA[IsAssignedImages];
  CorrectContentWOffset(ATabVisibleIndex, Result);
  if IsTabPressable and IsAssignedImages and (InternalGetTextRotationAngle(ViewInfo) = raPlus90) then
  begin
    Inc(Result.Left);
    Dec(Result.Right);
  end;
  Result := ScaleFactor.Apply(Result);
end;

function TcxPCButtonedPainter.GetTabsContainerOffsets: TRect;
begin
  Result := Rect(TabsContainerOffset, TabsContainerOffset, TabsContainerOffset, 0);
end;

function TcxPCButtonedPainter.GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := GetTabContentWOffset(ATabVisibleIndex);
end;

procedure TcxPCButtonedPainter.DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
begin
  InternalDrawFocusRect(ACanvas, ATabVisibleIndex,
    cxRectInflate(TabViewInfo[ATabVisibleIndex].FullRect, -ScaleFactor.Apply(1)));
end;

function TcxPCButtonedPainter.InternalGetPageFrameRectOffset: TRect;
begin
  Result := inherited InternalGetPageFrameRectOffset;
  Inc(Result.Top, ButtonedPainterDistanceBetweenTabsAndClientRects);
end;

function TcxPCButtonedPainter.IsTabBorderThick(ATabVisibleIndex: Integer): Boolean;
begin
  with TabViewInfo[ATabVisibleIndex] do
    Result := not IsTracking and (IsPressed or IsSelected or IsMainTab);
end;

function TcxPCButtonedPainter.UseLookAndFeelTabButton: Boolean;
begin
  Result := True;
end;

procedure TcxPCButtonedPainter.CorrectContentWOffset(ATabVisibleIndex: Integer; var AOffset: TcxPCWOffset);
begin
  if IsTabPressable and IsTabBorderThick(ATabVisibleIndex) then
  begin
    if InternalGetTextRotationAngle(ViewInfo) = raPlus90 then
    begin
      Dec(AOffset.Left);
      Inc(AOffset.Right);
    end
    else
    begin
      Inc(AOffset.Left);
      Dec(AOffset.Right);
    end;
  end;
end;

{ TcxPCExtraFlatPainter }

procedure TcxPCExtraFlatPainter.CalculateButtonContentParameters(
  AButton: TcxPCNavigatorButton;  AState: TcxPCNavigatorButtonState;
  out AParameters: TcxPCNavigatorButtonContentParameters);
begin
  inherited CalculateButtonContentParameters(AButton, AState, AParameters);
  AParameters.Enabled := True;
  AParameters.LiteStyle := (AButton = nbClose) or not ViewInfo.IsEnabled or
    (AState = nbsDisabled);
  if AParameters.LiteStyle then
    AParameters.BrushColor := cxPCLightBrushColor;
end;

function TcxPCExtraFlatPainter.CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer;
begin
  Result := Max(inherited CalculateTabNormalWidth(ATabViewInfo), GetMinTabNormalWidth(ATabViewInfo.VisibleIndex));
end;

procedure TcxPCExtraFlatPainter.CorrectTabNormalWidth(var AValue: Integer);
begin
  Inc(AValue, ScaleFactor.Apply(12));
end;

function TcxPCExtraFlatPainter.GetTabCorrection(ATabVisibleIndex: Integer): TRect;
begin
  if TabViewInfo[ATabVisibleIndex].IsMainTab then
    Result := ExtraFlatPainterMainTabRectCorrection
  else
    Result := inherited GetTabCorrection(ATabVisibleIndex);
end;

function TcxPCExtraFlatPainter.GetButtonColor(
  AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if AButtonState in [nbsHotTrack, nbsPressed] then
    Result := clBtnFace
  else
    Result := GetTabsRowColor;
end;

function TcxPCExtraFlatPainter.GetButtonContentColor(
  AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if not ViewInfo.IsEnabled or (AButtonState = nbsDisabled) then
    Result := cxPCDarkEdgeColor
  else
    if AButtonState = nbsPressed then
      Result := cxPCLightestEdgeColor
    else
      Result := clBtnText;
end;

function TcxPCExtraFlatPainter.GetButtonContentPosition(const ARect: TRect;
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;

  function InternalGetButtonContentPosition: TPoint;
  const
    AArrowButtonArrowRectLeftTopCorners: array [TcxPCArrow] of TPoint = (
      (X: 2; Y: 5), (X: 2; Y: 3), (X: 5; Y: 2), (X: 3; Y: 2)
    );
    ACloseButtonCrossRectLeftTopCorners: array[Boolean] of TPoint = ((X: 3; Y: 2), (X: 2; Y: 3));
    AGoDialogArrowRectLeftTopCorner: TPoint = (X: 3; Y: 4);
  begin
    case AButton of
      nbGoDialog:
        begin
          Result := AGoDialogArrowRectLeftTopCorner;
          if AState = nbsPressed then
            Inc(Result.Y);
        end;
      nbClose:
        Result := ACloseButtonCrossRectLeftTopCorners[ViewInfo.TabPosition in [tpTop, tpBottom]];
    else
      Result := AArrowButtonArrowRectLeftTopCorners[GetButtonArrow(AButton)];
    end;
  end;

begin
  if NeedButtonContentPositionCentered then
    Result := inherited GetButtonContentPosition(ARect, AButton, AState)
  else
    Result := InternalGetButtonContentPosition;
end;

function TcxPCExtraFlatPainter.DoGetButtonHeight: Integer;
begin
  Result := ScaleFactor.Apply(ExtraFlatPainterButtonSize);
end;

function TcxPCExtraFlatPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(ExtraFlatPainterButtonSize);
end;

function TcxPCExtraFlatPainter.GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect;
begin
  Result := inherited GetCloseButtonOffset(ATabVisibleIndex);
  Inc(Result.Left);
end;

function TcxPCExtraFlatPainter.GetPageClientRectOffset: TRect;
begin
  Result := inherited GetPageClientRectOffset;
  if ViewInfo.IsTabsVisible then
    Inc(Result.Top, ExtraFlatPainterMainTabRowUnderlineWidth);
  ExcludeUnderLine(Result);
end;

function TcxPCExtraFlatPainter.GetDefaultClientColor: TColor;
begin
  if HasActivePage then
    Result := inherited GetDefaultClientColor
  else
    Result := GetFreeSpaceColor;
end;

function TcxPCExtraFlatPainter.DoGetDefaultTabNormalHeight: Integer;
begin
  Result := ScaleFactor.Apply(17);
end;

function TcxPCExtraFlatPainter.GetDefaultTabNormalHTextOffset: Integer;
begin
  Result := 2;
end;

function TcxPCExtraFlatPainter.GetDrawImageOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := ExtraFlatPainterDrawImageOffsetA[TabViewInfo[TabVisibleIndex].IsMainTab];
end;

function TcxPCExtraFlatPainter.GetDrawTextHOffset(TabVisibleIndex: Integer): TRect;
var
  AIsMainTab: Boolean;
begin
  AIsMainTab := TabViewInfo[TabVisibleIndex].IsMainTab;
  Result := ExtraFlatPainterDrawTextHOffsetA[AIsMainTab];
  if TabViewInfo[TabVisibleIndex].GetRelativeTextRotationAngle = ra180 then
  begin
    Result.Top := 0;
    Result.Bottom := 0;
  end;

  if GetTabRotatedImageSize.cx > 0 then
    if AIsMainTab then
      Inc(Result.Left)
    else
      Inc(Result.Left, 2);
end;

function TcxPCExtraFlatPainter.GetFrameWidth: Integer;
begin
  Result := 1;
end;

function TcxPCExtraFlatPainter.GetFreeSpaceColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxPCExtraFlatPainter.GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabNormalWidth(ATabVisibleIndex);
end;

{ TcxPCExtraFlatPainter }

function TcxPCExtraFlatPainter.CalculateTabNormalHeight: Integer;
var
  ADefaultTabNormalHeight: Integer;
  ATabHeight: Integer;
begin
  Result := InternalCalculateTabNormalHeight;
  ADefaultTabNormalHeight := GetDefaultTabNormalHeight;
  Result := Max(Result, ADefaultTabNormalHeight);
  CorrectTabHeightForImage(Result);
  ATabHeight := ViewInfo.TabsViewInfo.GetTabDefaultHeight;
  if (ATabHeight - 4 >= ADefaultTabNormalHeight) and (ATabHeight - 4 > Result) then
    Result := ATabHeight - 4;
  Result := Max(Result, IfThen(ViewInfo.HasTabButtons, GetCloseButtonAreaHeight(-1)));
end;

class function TcxPCExtraFlatPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCExtraFlatStyle;
end;

class function TcxPCExtraFlatPainter.GetStyleName: TCaption;
begin
  Result := 'ExtraFlat';
end;

class function TcxPCExtraFlatPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

function TcxPCExtraFlatPainter.GetTabBodyColor(TabVisibleIndex: Integer): TColor;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  if ATabViewInfo.IsHighlighted then
    Result := HighlightedTabBodyColor
  else
  begin
    Result := GetTabColor(TabVisibleIndex);
    if Result = clDefault then
      if ATabViewInfo.IsMainTab then
        Result := cxPCTabBodyColor
      else
        if ViewInfo.IsTabsContainer then
          Result := ViewInfo.Color
        else
          Result := cxPCLightBrushColor;
  end;
end;

function TcxPCExtraFlatPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := ScaleFactor.Apply(ExtraFlatPainterTabContentWOffset[TabViewInfo[ATabVisibleIndex].IsMainTab]);
end;

function TcxPCExtraFlatPainter.GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer;
begin
  Result := 0;
end;

function TcxPCExtraFlatPainter.GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabImageAreaWidth + ScaleFactor.Apply(4);
end;

function TcxPCExtraFlatPainter.GetTabsRowsDelimiterWidth: Integer;
begin
  Result := 3;
end;

function TcxPCExtraFlatPainter.GetTabsNormalDistance: TcxPCDistance;
begin
  Result.dw := GetTabsDelimiterWidth;
  Result.dh := GetTabsRowsDelimiterWidth + ExtraFlatPainterTabsRowFreeSpaceWidth;
  if ViewInfo.ActuallyRotate then
    RotateTabsDistance(Result);
end;

function TcxPCExtraFlatPainter.GetTabsPosition: TcxPCTabsPosition;
const
  TabsRegionHOffset = ExtraFlatPainterTabsRowFreeSpaceWidth;
  TabsRegionWOffset = 6;

  NormalTabsRectCorrection: TRect = (Left: TabsRegionWOffset; Top: TabsRegionHOffset; Right: TabsRegionWOffset; Bottom: 0);
  ExtendedTabsRectCorrection: TRect = (Left: -1; Top: -1; Right: -1; Bottom: 0);

  procedure CalculateNormalParameters(var ATabsPosition: TcxPCTabsPosition);
  var
    AButtonsWidth: Integer;
    ANormalTabsRectCorrection: TRect;
  begin
    with ATabsPosition do
    begin
      AButtonsWidth := CalculateButtonsRegionWidth;
      ANormalTabsRectCorrection := NormalTabsRectCorrection;
      if NeedShowTabsRegionFrame then
        OffsetRect(ANormalTabsRectCorrection, 0, GetFrameWidth);
      Inc(ANormalTabsRectCorrection.Top, GetHeaderButtonHeightCorrection);
      NormalTabsRect := GetExtendedRect(ViewInfo.ControlBounds, ANormalTabsRectCorrection, ViewInfo.TabPosition);

      if ViewInfo.TabPosition in [tpTop, tpBottom] then
      begin
        NormalRowWidth := ViewInfo.Width - 2 * TabsRegionWOffset - AButtonsWidth;
        if ViewInfo.NavigatorPosition in [npLeftTop, npLeftBottom] then
          Inc(NormalTabsRect.Left, AButtonsWidth)
        else
          Dec(NormalTabsRect.Right, AButtonsWidth);
      end
      else
      begin
        NormalRowWidth := ViewInfo.Height - 2 * TabsRegionWOffset - AButtonsWidth;
        if ViewInfo.NavigatorPosition in [npLeftTop, npRightTop] then
          Inc(NormalTabsRect.Top, AButtonsWidth)
        else
          Dec(NormalTabsRect.Bottom, AButtonsWidth);
      end;
    end;
  end;

begin
  CalculateNormalParameters(Result);

  with Result do
  begin
    ExtendedTabsRect := GetExtendedRect(NormalTabsRect, ExtendedTabsRectCorrection, ViewInfo.TabPosition);

    ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 0;
    ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := 0;
    if ViewInfo.TabPosition in [tpTop, tpLeft] then
      ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 1
    else
      ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := -1;
    MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects := GetTabsNormalDistance.dh - 1;
  end;

  CalculateButtonsRegion;
end;

function TcxPCExtraFlatPainter.GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := inherited GetTabTextNormalWidth(ATabVisibleIndex) - 3;
end;

function TcxPCExtraFlatPainter.GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := GetTabContentWOffset(ATabVisibleIndex);
end;

function TcxPCExtraFlatPainter.InternalCalculateTabNormalHeight: Integer;
begin
  Result := inherited InternalCalculateTabNormalHeight - ScaleFactor.Apply(3);
end;

function TcxPCExtraFlatPainter.InternalGetPageFrameRectOffset: TRect;
begin
  Result := inherited InternalGetPageFrameRectOffset;
  if ViewInfo.IsTabsOnBothSides then
    Inc(Result.Bottom, ExtraFlatPainterTabsRowFreeSpaceWidth);
end;

function TcxPCExtraFlatPainter.IsPaintHeadersAreaFirst: Boolean;
begin
  Result := False;
end;

function TcxPCExtraFlatPainter.IsTabsRectVisible(ACanvas: TcxCanvas): Boolean;
begin
  Result := True;
end;

procedure TcxPCExtraFlatPainter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);
begin
  if AButtonState in [nbsHotTrack, nbsPressed] then
  begin
    InternalDrawEdge(ACanvas, ARect, AButtonState = nbsPressed, True);
    InflateRect(ARect, -1, -1);
  end;
end;

class function TcxPCExtraFlatPainter.IsMainTabBoundWithClient: boolean;
begin
  Result := True;
end;

class function TcxPCExtraFlatPainter.IsMultiSelectionAccepted: boolean;
begin
  Result := False;
end;

class function TcxPCExtraFlatPainter.IsTabPressable: Boolean;
begin
  Result := False;
end;

procedure TcxPCExtraFlatPainter.PaintPageClientArea(ACanvas: TcxCanvas);
begin
  PaintMainTabRowUnderline(ACanvas);
  inherited;
end;

procedure TcxPCExtraFlatPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
  InternalPaintFrame(ACanvas, clBtnHighlight, clBtnShadow);
end;

procedure TcxPCExtraFlatPainter.PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ATabViewInfo: TcxTabViewInfo;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  R := ATabViewInfo.FullRect;
  if ATabViewInfo.IsMainTab then
    PaintMainTabFrame(ACanvas, R);

  if not IsTabTransparent(ATabVisibleIndex) then
    DrawTabBackground(ACanvas, R, ATabVisibleIndex);
end;

procedure TcxPCExtraFlatPainter.PaintTabsRegion(ACanvas: TcxCanvas);

  procedure PaintDelimiter(TabVisibleIndex: Integer);

    function IsDelimiterNeeded: Boolean;
    var
      AFirstIndex, ALastIndex: Integer;
      ATabViewInfo, ANextTabViewInfo: TcxTabViewInfo;
    begin
      Result := False;
      ATabViewInfo := TabViewInfo[TabVisibleIndex];
      if ATabViewInfo.IsMainTab then Exit;
      ViewInfo.InitializeVisibleTabRange(AFirstIndex, ALastIndex);
      if TabVisibleIndex = ALastIndex then Exit;
      ANextTabViewInfo := TabViewInfo[TabVisibleIndex + 1];
      if ANextTabViewInfo.IsMainTab then Exit;
      Result := ATabViewInfo.VisibleRow = ANextTabViewInfo.VisibleRow;
    end;

  var
    ADelimiterOffsets: TcxPCTabsDelimiterOffsets;
    ADelimiterRect, ATabFullRect: TRect;
  begin
    if not IsDelimiterNeeded then
      Exit;
    ATabFullRect := TabViewInfo[TabVisibleIndex].FullRect;
    ADelimiterOffsets := GetTabsDelimiterOffsets;
    with ATabFullRect do
      case ViewInfo.TabPosition of
        tpTop, tpBottom:
          begin
            ADelimiterRect.Top := Top + ADelimiterOffsets.Top;
            ADelimiterRect.Bottom := Bottom - ADelimiterOffsets.Bottom;
            if ViewInfo.IsRightToLeftAlignment then
              ADelimiterRect.Left := Left - GetTabsDelimiterWidth
            else
              ADelimiterRect.Left := Right;
            ADelimiterRect.Right := ADelimiterRect.Left + GetTabsDelimiterWidth;
          end;
        tpLeft, tpRight:
          begin
            ADelimiterRect.Left := Left + ADelimiterOffsets.Top;
            ADelimiterRect.Right := Right - ADelimiterOffsets.Bottom;
            if ViewInfo.IsBottomToTopAlignment then
              ADelimiterRect.Top := Top - GetTabsDelimiterWidth
            else
              ADelimiterRect.Top := Bottom;
            ADelimiterRect.Bottom := ADelimiterRect.Top + GetTabsDelimiterWidth;
          end;
      end;
    if ViewInfo.UseRightToLeftAlignment and (ViewInfo.TabPosition in [tpTop, tpBottom]) then
      ADelimiterRect := TdxRightToLeftLayoutConverter.ConvertRect(ADelimiterRect, ATabFullRect);
    PaintTabsDelimiter(ACanvas, ADelimiterRect);
  end;

var
  AInterval: TcxPCIndexInterval;
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  ARowIndex, I: Integer;
  ARowRect, R: TRect;
begin
  ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);

  for ARowIndex := 0 to Length(ALineIndexBoundsA) - 1 do
  begin
    ACanvas.SaveClipRegion;
    try
      AInterval := ALineIndexBoundsA[ARowIndex];
      for I := AInterval.Left to AInterval.Right do
      begin
        PaintTab(ACanvas, I);
        PaintDelimiter(I);
      end;

      ARowRect := GetTabsRowRect(ARowIndex);
      R := ARowRect;
      PaintTabsRowsDelimiter(ACanvas, R, ARowIndex);
      DrawTabBackground(ACanvas, R, -1);
    finally
      ACanvas.RestoreClipRegion;
    end;

    for I := AInterval.Left to AInterval.Right do
      if IsTabTransparent(I) then
        AfterPaintTab(ACanvas, I);

    ACanvas.ExcludeClipRect(ARowRect);
  end;
end;

procedure TcxPCExtraFlatPainter.RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged);
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  if (TabPropertyChanged = tpcIsMainTab) and not ATabViewInfo.IsMainTab then
    InvalidateTabExtendedTabsRect(TabVisibleIndex)
  else
    InvalidateTabRect(TabVisibleIndex);
  if ATabViewInfo.IsMainTab then
    ViewInfo.InvalidateRect(GetMainTabRowUnderlineRect, False);
end;

function TcxPCExtraFlatPainter.GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  if IsOneOfButtons(AButton1, AButton2, nbClose) or not IsOneOfButtons(AButton1, AButton2, nbGoDialog) then
    Result := 3
  else
    Result := 0;

  Result := ScaleFactor.Apply(Result);
end;

function TcxPCExtraFlatPainter.GetButtonsRegionHOffset: Integer;
begin
  Result := ScaleFactor.Apply(6);
end;

function TcxPCExtraFlatPainter.GetButtonsRegionWOffset: Integer;
begin
  Result := 1;
  if NeedShowTabsRegionFrame then
    Inc(Result, GetFrameWidth);
  Result := ScaleFactor.Apply(Result);
end;

function TcxPCExtraFlatPainter.GetFocusRect: TRect;
const
  AFocusRectCorrectionA: array[TcxTabPosition] of TRect = (
    (Left:  0; Top:  0; Right: -1; Bottom:  1),
    (Left:  1; Top: -1; Right: -1; Bottom: -1),
    (Left:  0; Top:  0; Right:  1; Bottom: -1),
    (Left: -1; Top:  1; Right: -1; Bottom: -1)
  );
begin
  Result := cxRectTransform(TabViewInfo[ViewInfo.FocusedTabVisibleIndex].FullRect, AFocusRectCorrectionA[ViewInfo.TabPosition]);
end;

function TcxPCExtraFlatPainter.GetTabsDelimiterOffsets: TcxPCTabsDelimiterOffsets;
begin
  Result.Top := 2;
  Result.Bottom := 2;
end;

procedure TcxPCExtraFlatPainter.DrawTabBackground(ACanvas: TcxCanvas; ARect: TRect; ATabVisibleIndex: Integer);
begin
  if (ATabVisibleIndex <> -1) and (ATabVisibleIndex <> ViewInfo.MainTabVisibleIndex) then
  begin
    ACanvas.Brush.Color := GetTabsRowColor;
    ACanvas.FrameRect(ARect);
    InflateRect(ARect, -1, -1);
  end;

  if ATabVisibleIndex = -1 then
    ACanvas.Brush.Color := GetTabsRowColor
  else
    ACanvas.Brush.Color := GetTabBodyColor(ATabVisibleIndex);

  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect);
end;

procedure TcxPCExtraFlatPainter.DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
begin
  if TabViewInfo[ATabVisibleIndex].IsFocused then
    InternalDrawFocusRect(ACanvas, ATabVisibleIndex, GetFocusRect);
end;

procedure TcxPCExtraFlatPainter.ExcludeUnderLine(var R: TRect);
begin
  if NeedShowFrame and ViewInfo.IsTabsVisible then
    Dec(R.Top);
end;

function TcxPCExtraFlatPainter.GetMainTabRowUnderlineColor: TColor;
begin
  if ViewInfo.MainTabVisibleIndex = -1 then
    Result := cxPCTabBodyColor
  else
  begin
    Result := GetTabColor(ViewInfo.MainTabVisibleIndex);
    if Result = clDefault then
      Result := cxPCTabBodyColor;
  end;
end;

function TcxPCExtraFlatPainter.GetMainTabRowUnderlineRect: TRect;
begin
  if not ViewInfo.IsTabsVisible then
  begin
    Result := cxEmptyRect;
    Exit;
  end;

  Result := GetPageClientRect;
  SubtractRect(Result,
    GetExtendedRect(Result, Rect(0, - ExtraFlatPainterMainTabRowUnderlineWidth, 0, 0), ViewInfo.TabPosition),
    Result);
end;

function TcxPCExtraFlatPainter.GetTabBackgroundRect(ATabVisibleIndex: Integer;
  AForNormalState: Boolean): TRect;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if not AForNormalState and (ATabVisibleIndex = ViewInfo.MainTabVisibleIndex) then
    Result := GetExtendedRect(ATabViewInfo.FullRect, Rect(1, 1, 1, 0), ATabViewInfo.PaintingPosition)
  else
  begin
    Result := GetExtendedRect(ATabViewInfo.NormalRect,
      Rect(0, -ExtraFlatPainterTabsRowFreeSpaceWidth - GetHeaderButtonHeightCorrection, 0, -GetTabsRowsDelimiterWidth),
      ViewInfo.TabPosition);
  end;
end;

function TcxPCExtraFlatPainter.GetTabsDelimiterWidth: Integer;
begin
  Result := 1;
end;

function TcxPCExtraFlatPainter.GetTabsRowColor: TColor;
begin
  if ViewInfo.IsTabsContainer then
    Result := ViewInfo.Color
  else
    Result := cxPCLightBrushColor;
end;

function TcxPCExtraFlatPainter.GetTabsRowRect(ARowIndex: Integer): TRect;
var
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
begin
  ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);
  Result := GetTabBackgroundRect(ALineIndexBoundsA[ARowIndex].Left, True);
  if ViewInfo.TabPosition in [tpLeft, tpRight] then
  begin
    Result.Top := 0;
    Result.Bottom := ViewInfo.Height;
  end
  else
  begin
    Result.Left := 0;
    Result.Right := ViewInfo.Width;
  end;

  if NeedShowTabsRegionFrame then
    Result := GetExtendedRect(Result, Rect(1, 0, 1, 0), ViewInfo.TabPosition);
end;

procedure TcxPCExtraFlatPainter.InternalPaintFrame(ACanvas: TcxCanvas; ALeftTopColor, ARightBottomColor: TColor);
var
  R: TRect;
begin
  R := GetPageFrameRect;
  if not ViewInfo.IsTabsVisible then
    ACanvas.DrawComplexFrame(R, ALeftTopColor, ARightBottomColor)
  else
    case ViewInfo.TabPosition of
      tpTop:
        DrawBorder(ACanvas, R, [bRight, bBottom, bLeft], [ARightBottomColor, ARightBottomColor, ALeftTopColor]);
      tpBottom:
        DrawBorder(ACanvas, R, [bRight, bTop, bLeft], [ARightBottomColor, ALeftTopColor, ALeftTopColor]);
      tpLeft:
        DrawBorder(ACanvas, R, [bBottom, bRight, bTop], [ARightBottomColor, ARightBottomColor, ALeftTopColor]);
      tpRight:
        DrawBorder(ACanvas, R, [bBottom, bLeft, bTop], [ARightBottomColor, ALeftTopColor, ALeftTopColor]);
    end;
end;

procedure TcxPCExtraFlatPainter.InternalPaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect; ALightColor, ADarkColor: TColor);
var
  AMainTabViewInfo: TcxTabViewInfo;
begin
  AMainTabViewInfo := TabViewInfo[ViewInfo.MainTabVisibleIndex];
  case AMainTabViewInfo.PaintingPosition of
    tpTop:
      if ViewInfo.UseRightToLeftAlignment then
        DrawBorder(ACanvas, R, [bLeft, bTop, bRight], [ADarkColor, ALightColor, ALightColor])
      else
        DrawBorder(ACanvas, R, [bLeft, bTop, bRight], [ALightColor, ALightColor, ADarkColor]);
    tpBottom:
      begin
        if ViewInfo.UseRightToLeftAlignment then
        begin
          DrawBorder(ACanvas, R, [bLeft, bBottom, bRight], [ADarkColor, ADarkColor, ALightColor]);
          ACanvas.Pixels[R.Right, R.Top] := ADarkColor;
          ACanvas.Pixels[R.Right, R.Bottom] := ALightColor;
        end
        else
        begin
          DrawBorder(ACanvas, R, [bLeft, bBottom, bRight], [ALightColor, ADarkColor, ADarkColor]);
          ACanvas.Pixels[R.Left - 1, R.Top] := ADarkColor;
        end;
      end;
    tpLeft:
      DrawBorder(ACanvas, R, [bTop, bLeft, bBottom], [ALightColor, ALightColor, ADarkColor]);
    tpRight:
      begin
        DrawBorder(ACanvas, R, [bTop, bRight, bBottom], [ALightColor, ADarkColor, ADarkColor]);
        ACanvas.Pixels[R.Left, R.Top - 1] := ADarkColor;
      end;
  end;
end;

procedure TcxPCExtraFlatPainter.InternalPaintTabsRowsDelimiter(
  ACanvas: TcxCanvas; var ARowRect: TRect; AColors: array of TColor);
const
  BordersMap: array[TcxTabPosition] of TcxBorder = (bBottom, bTop, bRight, bLeft);
var
  AWidth: Integer;
  I: Integer;
begin
  AWidth := GetTabsRowsDelimiterWidth;
  for I := 0 to AWidth - 1 do
    DrawBorder(ACanvas, ARowRect, [BordersMap[ViewInfo.TabPosition]], [AColors[(I * Length(AColors)) div AWidth]]);
end;

function TcxPCExtraFlatPainter.IsMainTabRow(AVisibleRow: Integer): Boolean;
begin
  if ViewInfo.MainTabVisibleIndex <> -1 then
    Result := TabViewInfo[ViewInfo.MainTabVisibleIndex].VisibleRow = AVisibleRow
  else
    if ViewInfo.TabPosition in [tpTop, tpLeft] then
      Result := AVisibleRow = ViewInfo.RowCount - 1
    else
      Result := AVisibleRow = 0;
end;

function TcxPCExtraFlatPainter.NeedShowTabsRegionFrame: Boolean;
begin
  Result := False;
end;

procedure TcxPCExtraFlatPainter.PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect);
begin
  InternalPaintMainTabFrame(ACanvas, R, clWhite, clBlack);
end;

procedure TcxPCExtraFlatPainter.PaintMainTabRowUnderline(ACanvas: TcxCanvas);
begin
  if ViewInfo.IsTabsVisible then
  begin
    ACanvas.Brush.Color := GetMainTabRowUnderlineColor;
    ACanvas.FillRect(GetMainTabRowUnderlineRect);
  end;
end;

procedure TcxPCExtraFlatPainter.PaintTabsDelimiter(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.Brush.Color := cxPCDarkEdgeColor;
  ACanvas.FillRect(ARect);
  ACanvas.ExcludeClipRect(ARect);
end;

procedure TcxPCExtraFlatPainter.PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer);
begin
  if ViewInfo.TabPosition in [tpTop, tpLeft] then
    InternalPaintTabsRowsDelimiter(ACanvas, ARowRect, [cxPCTabBodyColor, cxPCTabBodyColor, clWhite])
  else
    InternalPaintTabsRowsDelimiter(ACanvas, ARowRect, [cxPCTabBodyColor, cxPCTabBodyColor, clBlack])
end;

{ TcxPCUltraFlatPainter }

class function TcxPCUltraFlatPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCUltraFlatStyle;
end;

class function TcxPCUltraFlatPainter.GetStyleName: TCaption;
begin
  Result := 'UltraFlat';
end;

class function TcxPCUltraFlatPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := ALookAndFeel.GetAvailablePainter([totTab]).LookAndFeelStyle = lfsUltraFlat;
end;

procedure TcxPCUltraFlatPainter.CalculateButtonContentParameters(
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState;
  out AParameters: TcxPCNavigatorButtonContentParameters);
begin
  inherited CalculateButtonContentParameters(AButton, AState, AParameters);
  AParameters.Enabled := ViewInfo.IsEnabled;
  AParameters.LiteStyle := AButton = nbClose;
  AParameters.BrushColor := AParameters.Color;
end;

function TcxPCUltraFlatPainter.GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if AButtonState = nbsHotTrack then
    Result := clBtnFace
  else
    if AButtonState = nbsPressed then
      Result := clBtnShadow
    else
      Result := cxPCLightBrushColor;
end;

function TcxPCUltraFlatPainter.DoGetButtonHeight: Integer;
begin
  Result := ScaleFactor.Apply(UltraFlatPainterButtonHeight);
end;

function TcxPCUltraFlatPainter.GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(1 + 2 * Ord(IsOneOfButtons(AButton1, AButton2, nbClose)));
end;

function TcxPCUltraFlatPainter.GetButtonsRegionHOffset: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

function TcxPCUltraFlatPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(UltraFlatPainterButtonWidthA[Button]);
end;

function TcxPCUltraFlatPainter.GetFocusRect: TRect;
begin
  Result := GetExtendedRect(TabViewInfo[ViewInfo.FocusedTabVisibleIndex].FullRect,
    ScaleFactor.Apply(Rect(1, 1, 1, 0)), ViewInfo.TabPosition);
end;

function TcxPCUltraFlatPainter.NeedButtonContentPositionCentered: Boolean;
begin
  Result := True;
end;

procedure TcxPCUltraFlatPainter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);

  function GetFrameColor: TColor;
  begin
    if AButtonState in [nbsNormal, nbsDisabled] then
      Result := clBtnShadow
    else
      Result := clBlack;
  end;

begin
  ACanvas.DrawComplexFrame(ARect, GetFrameColor, GetFrameColor);
  InflateRect(ARect, -1, -1);
end;

procedure TcxPCUltraFlatPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
  InternalPaintFrame(ACanvas, clBtnShadow, clBtnShadow);
end;

procedure TcxPCUltraFlatPainter.PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect);
begin
  InternalPaintMainTabFrame(ACanvas, R, clBlack, clBlack);
end;

procedure TcxPCUltraFlatPainter.PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer);
begin
  if IsMainTabRow(ARowIndex) then
    InternalPaintTabsRowsDelimiter(ACanvas, ARowRect, [cxPCTabBodyColor, cxPCTabBodyColor, clBlack])
  else
    InternalPaintTabsRowsDelimiter(ACanvas, ARowRect, [cxPCTabBodyColor, clBlack, cxPCTabBodyColor])
end;

{ TcxPCFlatPainter }

class function TcxPCFlatPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCFlatStyle;
end;

class function TcxPCFlatPainter.GetStyleName: TCaption;
begin
  Result := 'Flat';
end;

class function TcxPCFlatPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := ALookAndFeel.GetAvailablePainter([totTab]).LookAndFeelStyle = lfsFlat;
end;

function TcxPCFlatPainter.GetButtonContentPosition(const ARect: TRect;
  AButton: TcxPCNavigatorButton; AState: TcxPCNavigatorButtonState): TPoint;
begin
  Result := inherited GetButtonContentPosition(ARect, AButton, AState);
  if not NeedButtonContentPositionCentered then
  begin
    Dec(Result.X, TabsPainterButtonBorderWidth - FlatPainterButtonBorderWidth);
    Dec(Result.Y, TabsPainterButtonBorderWidth - FlatPainterButtonBorderWidth);
  end;
end;

function TcxPCFlatPainter.DoGetButtonHeight: Integer;
begin
  Result := inherited DoGetButtonHeight -
    ScaleFactor.Apply(TabsPainterButtonBorderWidth - FlatPainterButtonBorderWidth) * 2;
end;

function TcxPCFlatPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := inherited DoGetButtonWidth(Button) -
    ScaleFactor.Apply(TabsPainterButtonBorderWidth - FlatPainterButtonBorderWidth) * 2;
end;

function TcxPCFlatPainter.GetFrameWidth: Integer;
begin
  Result := 1;
end;

function TcxPCFlatPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsFlat);
end;

function TcxPCFlatPainter.GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion;
begin
  Result := TcxRegion.Create(TabViewInfo[ATabVisibleIndex].VisibleRect);
end;

function TcxPCFlatPainter.IsNativePainting: Boolean;
begin
  Result := False;
end;

function TcxPCFlatPainter.NeedShowFrame: Boolean;
begin
  Result := True;
end;

procedure TcxPCFlatPainter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);
begin
  if AButtonState = nbsPressed then
    ACanvas.DrawComplexFrame(ARect, cxPCDarkEdgeColor, cxPCLightestEdgeColor)
  else
    ACanvas.DrawComplexFrame(ARect, cxPCLightestEdgeColor, cxPCDarkEdgeColor);
  InflateRect(ARect, -1, -1);
end;

procedure TcxPCFlatPainter.PaintFrameBorder(ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.DrawComplexFrame(R, cxPCLightestEdgeColor, cxPCDarkEdgeColor, GetPageBorders);
end;

procedure TcxPCFlatPainter.PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);

  function GetContrastColor: TColor;
  begin
    Result := Light(GetTabBodyColor(ATabVisibleIndex), 85);
  end;

var
  AContrastColor: TColor;
  ATabViewInfo: TcxTabViewInfo;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  R := ATabViewInfo.FullRect;
  AContrastColor := GetContrastColor;
  with R do
    case ATabViewInfo.PaintingPosition of
      tpTop:
        begin
          InternalPolyLine([Point(Left, Bottom - 1), Point(Left, Top + 2), Point(Left + 2, Top), Point(Right - 3, Top), Point(Right - 2, Top + 1)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Right - 1, Top + 2), Point(Right - 1, Bottom - 1)], cxPCDarkEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Bottom - 1), Point(Left + 1, Top + 2), Point(Left + 2, Top + 1), Point(Right - 3, Top + 1), Point(Right - 2, Top + 2), Point(Right - 2, Bottom - 1)], AContrastColor, ACanvas);
        end;
      tpBottom:
        begin
          InternalPolyLine([Point(Left, Top), Point(Left, Bottom - 3), Point(Left + 1, Bottom - 2)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 2, Bottom - 1), Point(Right - 3, Bottom - 1), Point(Right - 1, Bottom - 3), Point(Right - 1, Top)], cxPCDarkEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Top), Point(Left + 1, Bottom - 3)], AContrastColor, ACanvas);
          InternalPolyLine([Point(Left + 2, Bottom - 2), Point(Right - 3, Bottom - 2), Point(Right - 2, Bottom - 3), Point(Right - 2, Top)], clBtnFace, ACanvas);
        end;
      tpLeft:
        begin
          InternalPolyLine([Point(Right - 1, Bottom - 1), Point(Left + 2, Bottom - 1)], cxPCDarkEdgeColor, ACanvas);
          InternalPolyLine([Point(Left + 1, Bottom - 2), Point(Left, Bottom - 3), Point(Left, Top + 2), Point(Left + 2, Top), Point(Right - 1, Top)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Right - 1, Bottom - 2), Point(Left + 2, Bottom - 2), Point(Left + 1, Bottom - 3), Point(Left + 1, Top + 2), Point(Left + 2, Top + 1), Point(Right - 1, Top + 1)], AContrastColor, ACanvas);
        end;
      tpRight:
        begin
          InternalPolyLine([Point(Left, Top), Point(Right - 3, Top), Point(Right - 2, Top + 1)], cxPCLightestEdgeColor, ACanvas);
          InternalPolyLine([Point(Right - 1, Top + 2), Point(Right - 1, Bottom - 3), Point(Right - 3, Bottom - 1), Point(Left, Bottom - 1)], cxPCDarkEdgeColor, ACanvas);
          InternalPolyLine([Point(Left, Top + 1), Point(Right - 3, Top + 1)], AContrastColor, ACanvas);
          InternalPolyLine([Point(Right - 2, Top + 2), Point(Right - 2, Bottom - 3), Point(Right - 3, Bottom - 2), Point(Left, Bottom - 2)], clBtnFace, ACanvas);
        end;
    end;
  PaintTabCorners(ACanvas, ATabVisibleIndex);
end;

{ TcxPCOffice11Painter }

class function TcxPCOffice11Painter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCOffice11Style;
end;

class function TcxPCOffice11Painter.GetStyleName: TCaption;
begin
  Result := 'Office11';
end;

class function TcxPCOffice11Painter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  with ALookAndFeel do
    Result := (ALookAndFeel.SkinPainter = nil) and (GetAvailablePainter([totTab]).LookAndFeelStyle = lfsOffice11);
end;

class function TcxPCOffice11Painter.GetFrameColor: TColor;
begin
  Result := dxOffice11SelectedBorderColor;
end;

procedure TcxPCOffice11Painter.DrawTabBackground(ACanvas: TcxCanvas; ARect: TRect; ATabVisibleIndex: Integer);
begin
  DrawGradientBackground(ACanvas, ARect, ATabVisibleIndex, ViewInfo.TabPosition in [tpLeft, tpRight], False);
end;

procedure TcxPCOffice11Painter.ExcludeUnderLine(var R: TRect);
begin
// do nothing
end;

procedure TcxPCOffice11Painter.FillPageClientRect(ACanvas: TcxCanvas);
begin
  if UseActivePageColor or not IsGradientClientArea then
    inherited FillPageClientRect(ACanvas)
  else
    FillGradientRect(ACanvas.Handle, GetPageClientRect,
      dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2,
      ViewInfo.TabPosition in [tpLeft, tpRight]);
end;

function TcxPCOffice11Painter.GetButtonContentColor(AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if AButtonState = nbsDisabled then
    Result := dxOffice11TextDisabledColor
  else
    Result := dxOffice11TextEnabledColor;
end;

function TcxPCOffice11Painter.GetDefaultClientColor: TColor;
begin
  Result := dxGetMiddleRGB(dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, 50);
end;

function TcxPCOffice11Painter.GetPageClientRectOffset: TRect;
begin
  Result := inherited GetPageClientRectOffset;
  if NeedShowMainTabOppositeRowLine then
    Inc(Result.Bottom);
end;

function TcxPCOffice11Painter.GetFocusRect: TRect;
begin
  Result := GetExtendedRect(TabViewInfo[ViewInfo.FocusedTabVisibleIndex].FullRect,
    ScaleFactor.Apply(Rect(1, 1, 1, 0)), ViewInfo.TabPosition);
end;

function TcxPCOffice11Painter.GetFreeSpaceColor: TColor;
begin
  Result := GetFrameColor;
end;

function TcxPCOffice11Painter.GetMainTabRowUnderlineColor: TColor;
begin
  if (ViewInfo.MainTabVisibleIndex <> -1) and (GetTabColor(ViewInfo.MainTabVisibleIndex) <> clDefault) then
    Result := GetTabColor(ViewInfo.MainTabVisibleIndex)
  else
    if not (pcoGradient in ViewInfo.Options) then
      Result := dxGetMiddleRGB(GetSelectedColor1, GetSelectedColor2, 50)
    else
      if ViewInfo.TabPosition in [tpTop, tpLeft] then
        Result := GetSelectedColor2
      else
        Result := GetSelectedColor1;
end;

function TcxPCOffice11Painter.GetMainTabRowUnderlineRect: TRect;
begin
  if not ViewInfo.IsTabsVisible then
    Exit(cxEmptyRect);

  Result := GetPageFrameRect;
  SubtractRect(Result, Result,
    GetExtendedRect(Result, Rect(0, ExtraFlatPainterMainTabRowUnderlineWidth, 0, 0), ViewInfo.TabPosition));
end;

function TcxPCOffice11Painter.GetTabBodyColor(TabVisibleIndex: Integer): TColor;
begin
  if TabViewInfo[TabVisibleIndex].IsHighlighted then
    Result := HighlightedTabBodyColor
  else
    Result := GetTabColor(TabVisibleIndex);
end;

function TcxPCOffice11Painter.GetTabsDelimiterOffsets: TcxPCTabsDelimiterOffsets;
begin
  if ViewInfo.TabPosition in [tpTop, tpLeft] then
  begin
    Result.Top := 0;
    Result.Bottom := 2;
  end
  else
  begin
    Result.Top := 3;
    Result.Bottom := -1;
  end;
end;

function TcxPCOffice11Painter.GetTabsDelimiterWidth: Integer;
begin
  Result := 2;
end;

function TcxPCOffice11Painter.GetTabsRowsDelimiterWidth: Integer;
begin
  Result := 1;
end;

function TcxPCOffice11Painter.IsTabTransparent(ATabVisibleIndex: Integer): Boolean;
begin
  Result := not (TabViewInfo[ATabVisibleIndex].IsMainTab or (GetTabBodyColor(ATabVisibleIndex) <> clDefault));
end;

function TcxPCOffice11Painter.NeedRedrawOnResize: Boolean;
begin
  Result := IsGradientClientArea;
end;

function TcxPCOffice11Painter.NeedShowTabsRegionFrame: Boolean;
begin
  Result := NeedShowFrame;
end;

procedure TcxPCOffice11Painter.PaintButtonBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxPCNavigatorButtonState);
var
  AColor1, AColor2: TColor;
  ATabsRowRect: TRect;
begin
  if not InternalSetClipRect(ACanvas, ARect) then
    Exit;
  ATabsRowRect := GetTabsRowRect(0);
  case AState of
    nbsNormal, nbsDisabled:
      DrawTabBackground(ACanvas, ATabsRowRect, -1);
    nbsPressed, nbsHotTrack:
      begin
        if AState = nbsHotTrack then
        begin
          AColor1 := GetSelectedColor1;
          AColor2 := GetSelectedColor2;
        end
        else
        begin
          AColor1 := dxOffice11SelectedDownColor1;
          AColor2 := dxOffice11SelectedDownColor2;
        end;
        if not (pcoGradient in ViewInfo.Options) then
        begin
          AColor1 := dxGetMiddleRGB(AColor1, AColor2, 50);
          AColor2 := AColor1;
        end;
        FillGradientRect(ACanvas.Handle, ATabsRowRect, AColor1, AColor2, ViewInfo.TabPosition in [tpLeft, tpRight]);
      end;
  end;
  ACanvas.RestoreClipRegion;
end;

procedure TcxPCOffice11Painter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);

  function GetFrameColor: TColor;
  begin
    if not ViewInfo.IsEnabled or (AButtonState = nbsDisabled) then
      Result := dxOffice11TextDisabledColor
    else
      Result := TcxPCOffice11Painter.GetFrameColor;
  end;

begin
  ACanvas.FrameRect(ARect, GetFrameColor);
  InflateRect(ARect, -1, -1);
end;

procedure TcxPCOffice11Painter.PaintPageClientArea(ACanvas: TcxCanvas);
begin
  PaintMainTabOppositeRowLine(ACanvas);
  inherited;
end;

procedure TcxPCOffice11Painter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
  ACanvas.FrameRect(cxRectInflate(GetPageClientRect, GetFrameWidth), GetFrameColor, GetFrameWidth);
end;

procedure TcxPCOffice11Painter.PaintMainTabFrame(ACanvas: TcxCanvas; var R: TRect);
begin
  InternalPaintMainTabFrame(ACanvas, R, GetFrameColor, GetFrameColor);
end;

procedure TcxPCOffice11Painter.PaintTabsDelimiter(ACanvas: TcxCanvas; const ARect: TRect);

  procedure PaintDelimiterLine(const R: TRect; AColor: TColor);
  begin
    ACanvas.Brush.Color := AColor;
    ACanvas.FillRect(R);
    ACanvas.ExcludeClipRect(R);
  end;

var
  R: TRect;
begin
  R := ARect;
  Dec(R.Right);
  Dec(R.Bottom);
  PaintDelimiterLine(R, dxOffice11BarSeparatorColor1);
  OffsetRect(R, 1, 1);
  PaintDelimiterLine(R, dxOffice11BarSeparatorColor2);
end;

procedure TcxPCOffice11Painter.PaintTabsRowsDelimiter(ACanvas: TcxCanvas; var ARowRect: TRect; ARowIndex: Integer);
begin
  InternalPaintTabsRowsDelimiter(ACanvas, ARowRect, [GetFrameColor]);
end;

procedure TcxPCOffice11Painter.PrepareDrawTabContentBitmapBackground(
  ABitmap: TcxBitmap; const ABitmapPos: TPoint; ATabVisibleIndex: Integer);
var
  P: TPoint;
  R: TRect;
begin
  R := GetExtendedRect(GetTabBackgroundRect(ATabVisibleIndex, False),
    Rect(0, 0, 0, GetTabsRowsDelimiterWidth), ViewInfo.TabPosition);
  P := ABitmapPos;
  if ViewInfo.IsVerticalText then
  begin
    R := Rect(R.Top, R.Left, R.Bottom, R.Right);
    P := Point(P.Y, P.X);
  end;
  if ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raPlus90) then
    OffsetRect(R, -R.Left + P.X + ABitmap.Width - R.Right, -P.Y)
  else if not ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raMinus90) then
    OffsetRect(R, -P.X, -R.Top - (R.Bottom - P.Y - ABitmap.Height))
  else
    OffsetRect(R, -P.X, -P.Y);

  DrawGradientBackground(ABitmap.cxCanvas, R, ATabVisibleIndex, ViewInfo.ActuallyRotate,
    not ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raMinus90) or
    ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raPlus90));
end;

procedure TcxPCOffice11Painter.DrawTabContentBackground(ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer);
var
  R: TRect;
begin
  R := GetExtendedRect(GetTabBackgroundRect(ATabVisibleIndex, False),
    Rect(0, 0, 0, GetTabsRowsDelimiterWidth), ViewInfo.TabPosition);
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(ABounds), roIntersect);
    DrawGradientBackground(ACanvas, R, ATabVisibleIndex,
      ViewInfo.TabPosition in [tpLeft, tpRight], False);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxPCOffice11Painter.DrawGradientBackground(ACanvas: TcxCanvas;
  ARect: TRect; ATabVisibleIndex: Integer; AHorizontal, AInverse: Boolean);

  procedure GetGradientColors(out AColor1, AColor2: TColor);
  begin
//    if ViewInfo.IsTabsContainer and ((ATabVisibleIndex = -1) or (ATabVisibleIndex <> ViewInfo.MainTabVisibleIndex)) then
//    begin
//      AColor1 := ViewInfo.Color;
//      AColor2 := AColor1;
//      Exit;
//    end;

    if (ATabVisibleIndex <> -1) and (ATabVisibleIndex = ViewInfo.MainTabVisibleIndex) then
    begin
      AColor1 := GetSelectedColor1;
      AColor2 := GetSelectedColor2;
    end
    else
    begin
      AColor1 := dxOffice11ToolbarsColor1;
      AColor2 := dxOffice11ToolbarsColor2;
    end;

    if not (pcoGradient in ViewInfo.Options) then
    begin
      AColor1 := dxGetMiddleRGB(AColor1, AColor2, 50);
      AColor2 := AColor1;
    end
    else
      if AInverse then
        ExchangeLongWords(AColor1, AColor2);
  end;

  procedure DrawColoredTabBackground;
  var
    AColor1, AColor2: TColor;
  begin
    GetGradientColors(AColor1, AColor2);
    if ATabVisibleIndex <> ViewInfo.MainTabVisibleIndex then
      case TabViewInfo[ATabVisibleIndex].PaintingPosition of
        tpTop:
          begin
            InternalPolyLine([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right - 1, ARect.Bottom - 1)], AColor2, ACanvas);
            Dec(ARect.Bottom);
          end;
        tpBottom:
          begin
            InternalPolyLine([Point(ARect.Left, ARect.Top), Point(ARect.Right - 1, ARect.Top)], AColor1, ACanvas);
            Inc(ARect.Top);
          end;
        tpLeft:
          begin
            InternalPolyLine([Point(ARect.Right - 1, ARect.Top), Point(ARect.Right - 1, ARect.Bottom - 1)], AColor2, ACanvas);
            Dec(ARect.Right);
          end;
        tpRight:
          begin
            InternalPolyLine([Point(ARect.Left, ARect.Top), Point(ARect.Left, ARect.Bottom - 1)], AColor1, ACanvas);
            Inc(ARect.Left);
          end;
      end;
    ACanvas.Brush.Color := GetTabBodyColor(ATabVisibleIndex);
    ACanvas.FillRect(ARect);
  end;

var
  AColor1, AColor2: TColor;
begin
  if (ATabVisibleIndex <> -1) and (GetTabBodyColor(ATabVisibleIndex) <> clDefault) then
    DrawColoredTabBackground
  else
  begin
    GetGradientColors(AColor1, AColor2);
    FillGradientRect(ACanvas.Handle, ARect, AColor1, AColor2, AHorizontal);
  end;
end;

function TcxPCOffice11Painter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsOffice11);
end;

function TcxPCOffice11Painter.UseLookAndFeelTabButton: Boolean;
begin
  Result := True;
end;

function TcxPCOffice11Painter.GetSelectedColor1: TColor;
begin
  Result := dxOffice11SelectedColor1;
end;

function TcxPCOffice11Painter.GetSelectedColor2: TColor;
begin
  Result := dxOffice11SelectedColor2;
end;

function TcxPCOffice11Painter.IsGradientClientArea: Boolean;
begin
  Result := ViewInfo.Options * [pcoGradient, pcoGradientClientArea] = [pcoGradient, pcoGradientClientArea];
end;

function TcxPCOffice11Painter.NeedShowMainTabOppositeRowLine: Boolean;
begin
  Result := not NeedShowFrame and
    ((ViewInfo.TabPosition in [tpTop, tpLeft]) and (ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount) or
    (ViewInfo.TabPosition in [tpBottom, tpRight]) and (ViewInfo.TopOrLeftPartRowCount <> 0));
end;

procedure TcxPCOffice11Painter.PaintMainTabOppositeRowLine(ACanvas: TcxCanvas);
var
  ARWidth, ARHeight: Integer;
  R: TRect;
begin
  if NeedShowMainTabOppositeRowLine then
  begin
    R := GetExtendedRect(GetPageClientRect, Rect(0, 0, 0, -1), ViewInfo.TabPosition);
    GetRectSize(R, ViewInfo.TabPosition in [tpTop, tpBottom], ARWidth, ARHeight);
    DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom - 1), Point(R.Left + ARWidth - 1, R.Bottom - 1)],
      ViewInfo.TabPosition, GetFrameColor);
  end;
end;

{ TcxPCSlantedPainter }

class function TcxPCSlantedPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCSlantedStyle;
end;

class function TcxPCSlantedPainter.GetStyleName: TCaption;
begin
  Result := 'Slanted';
end;

class function TcxPCSlantedPainter.HasLookAndFeel(
  ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

function TcxPCSlantedPainter.AlwaysColoredTabs: Boolean;
begin
  Result := True;
end;

class function TcxPCSlantedPainter.AllowMultiLineTabCaptions: Boolean;
begin
  Result := False;
end;

procedure TcxPCSlantedPainter.CalculateButtonContentParameters(
  AButton: TcxPCNavigatorButton;  AState: TcxPCNavigatorButtonState;
  out AParameters: TcxPCNavigatorButtonContentParameters);
begin
  inherited CalculateButtonContentParameters(AButton, AState, AParameters);
  AParameters.Enabled := True;
  AParameters.LiteStyle := AButton = nbClose;
end;

function TcxPCSlantedPainter.CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer;
begin
  Result := Max(inherited CalculateTabNormalWidth(ATabViewInfo),
    GetMinTabNormalWidth(ATabViewInfo.VisibleIndex));
end;

procedure TcxPCSlantedPainter.CorrectTabNormalWidth(var AValue: Integer);
begin
  //do nothing
end;

function TcxPCSlantedPainter.GetButtonColor(AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if AButtonState = nbsHotTrack then
    Result := dxGetMiddleRGB(clBtnFace, clWhite, 35)
  else
    if AButtonState = nbsPressed then
      Result := dxGetMiddleRGB(clBtnFace, clBlack, 90)
    else
      Result := clBtnFace;
end;

function TcxPCSlantedPainter.GetButtonContentColor(
  AButtonState: TcxPCNavigatorButtonState): TColor;
begin
  if (AButtonState = nbsDisabled) or not ViewInfo.IsEnabled then
    Result := clBtnShadow
  else
    if AButtonState = nbsPressed then
      Result := dxGetMiddleRGB(ViewInfo.Color, clWhite, 10)
    else
      Result := clBtnText;
end;

function TcxPCSlantedPainter.DoGetButtonHeight: Integer;
begin
  Result := ScaleFactor.Apply(SlantedPainterButtonHeight);
end;

function TcxPCSlantedPainter.GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(1 + 2 * Ord(IsOneOfButtons(AButton1, AButton2, nbClose)));
end;

function TcxPCSlantedPainter.GetButtonsRegionFromTabsOffset: Integer;
begin
  Result := ScaleFactor.Apply(1);
end;

function TcxPCSlantedPainter.GetButtonsRegionHOffset: Integer;
begin
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset
  else
    Result := ScaleFactor.Apply(1);
end;

function TcxPCSlantedPainter.GetButtonsRegionWOffset: Integer;
begin
  Result := 0;
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset;
end;

function TcxPCSlantedPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := ScaleFactor.Apply(SlantedPainterButtonWidthA[Button]);
end;

function TcxPCSlantedPainter.GetClientColor: TColor;
begin
  if not ViewInfo.IsTabsContainer or (ViewInfo.MainTabVisibleIndex = -1) then
    Result := inherited GetClientColor
  else
  begin
    Result := GetTabColor(ViewInfo.MainTabVisibleIndex);
    if Result = clDefault then
      if ViewInfo.IsTabsContainer then
        Result := cxPCTabBodyColor
      else
        Result := GetDefaultClientColor;
  end;
end;

function TcxPCSlantedPainter.DoGetCloseButtonSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(14, 15));
end;

function TcxPCSlantedPainter.GetCutSideOffset: Integer;
begin
  if ViewInfo.TabSlants.Kind = skSlant then
    Result := GetCutValue
  else
    Result := CutCornerSize;
end;

function TcxPCSlantedPainter.GetPageClientRectOffset: TRect;
begin
  Result := inherited GetPageClientRectOffset;
  if ViewInfo.IsTabsVisible and ViewInfo.IsTabsContainer then
    Inc(Result.Top, TabsContainerOffset);
  if not NeedShowFrame and ViewInfo.IsTabsVisible then
  begin
    Inc(Result.Top, ScaleFactor.Apply(1));
    if ViewInfo.IsTabsOnBothSides then
      Inc(Result.Bottom, ScaleFactor.Apply(1));
  end;
end;

function TcxPCSlantedPainter.DoGetDefaultTabNormalHeight: Integer;
begin
  Result := 0;
end;

function TcxPCSlantedPainter.GetDrawImageOffset(TabVisibleIndex: Integer): TRect;
begin
  if ViewInfo.ActuallyRotate then
    if (ViewInfo.TabSlants.Kind = skSlant) or (ViewInfo.TabSlants.Positions = []) then
    begin
      Result.Top := GetFrameWidth + 1;
      Result.Bottom := GetFrameWidth + 1;
    end
    else
    begin
      Result.Top := GetFrameWidth + 2;
      Result.Bottom := GetFrameWidth + 2;
    end
  else
    Result := Rect(0, SlantedPainterTabStateMarkWidth + GetFrameWidth, 0, 1);
end;

function TcxPCSlantedPainter.GetDrawTextHOffset(TabVisibleIndex: Integer): TRect;
begin
  if ViewInfo.ActuallyRotate then
    Result := Rect(0, GetFrameWidth + 3, 0, GetFrameWidth + 1)
  else
    Result := Rect(0, SlantedPainterTabStateMarkWidth + GetFrameWidth + 1, 0, 1);
end;

function TcxPCSlantedPainter.GetFillClientRect: TRect;
begin
  Result := inherited GetFillClientRect;
  if ViewInfo.IsTabsContainer then
    Result := cxRectContent(Result, RotateRect(Rect(0, -TabsContainerOffset, 0, 0), ViewInfo.TabPosition));
end;

function TcxPCSlantedPainter.GetFrameWidth: Integer;
begin
  Result := 1;
end;

function TcxPCSlantedPainter.GetImageTextDistance(ATabVisibleIndex: Integer): Integer;
begin
  Result := 3;
end;

function TcxPCSlantedPainter.GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabNormalWidth(ATabVisibleIndex);
end;

function TcxPCSlantedPainter.GetTabBodyColor(
  TabVisibleIndex: Integer): TColor;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[TabVisibleIndex];
  if ATabViewInfo.IsHighlighted then
    Result := HighlightedTabBodyColor
  else
  begin
    Result := GetTabColor(TabVisibleIndex);
    if Result = clDefault then
      if ViewInfo.IsTabsContainer then
        Result := cxPCTabBodyColor
      else
        Result := ViewInfo.Color;
  end;
end;

function TcxPCSlantedPainter.GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  Result := InternalGetTabClipRegion(ACanvas, ATabVisibleIndex, False);
  Result.Combine(ATabViewInfo.VisibleRect, roIntersect);
  if ATabViewInfo.IsMainTab then
    Result.Combine(GetTabUnderlineRect(ATabViewInfo), roAdd);
end;

function TcxPCSlantedPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
const
  AContentOffset = 2;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  if ViewInfo.ActuallyRotate then
  begin
    Result.Left := AContentOffset;
    Result.Right := AContentOffset + SlantedPainterTabStateMarkWidth + GetFrameWidth;
    if (ViewInfo.TabSlants.Kind = skCutCorner) and (ViewInfo.TabSlants.Positions <> []) then
      Inc(Result.Right);

    if ATabVisibleIndex > -1 then
    begin
      ATabViewInfo := TabViewInfo[ATabVisibleIndex];
      if ATabViewInfo.GetRelativeTextRotationAngle = raMinus90 then
        ExchangeLongWords(Result.Left, Result.Right);
    end;
  end
  else
  begin
    Result.Left := AContentOffset + GetFrameWidth;
    Result.Right := AContentOffset + GetFrameWidth;
    if spLeft in ViewInfo.TabSlants.Positions then
      Result.Left := GetCutSideOffset;
    if spRight in ViewInfo.TabSlants.Positions then
      Result.Right := GetCutSideOffset;
    if ViewInfo.GetTextRotationAngle = raMinus90 then
      ExchangeLongWords(Result.Left, Result.Right);
  end;
end;

function TcxPCSlantedPainter.GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer;
begin
  with GetTabContentWOffset(ATabVisibleIndex) do
    Result := Left + Right;
end;

function TcxPCSlantedPainter.GetTabNormalImageAreaWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetTabImageSize.cx;
end;

function TcxPCSlantedPainter.GetTabNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := Max(inherited GetTabNormalWidth(ATabVisibleIndex), GetGeometricalMinTabWidth);
end;

function TcxPCSlantedPainter.GetTabsContainerOffsets: TRect;
begin
  Result := cxEmptyRect;
  if not ViewInfo.IsTabsContainer then
    Exit;
  Result := Rect(0, TabsContainerOffset, 0, 0);
  case ViewInfo.TabPosition of
    tpTop, tpLeft:
      begin
        Result.Left := TabsContainerOffset - Integer(spLeft in SlantedSides);
        Result.Right := TabsContainerOffset - Integer(spRight in SlantedSides);
      end;
    tpBottom, tpRight:
      begin
        Result.Left := TabsContainerOffset - Integer(spRight in SlantedSides);
        Result.Right := TabsContainerOffset - Integer(spLeft in SlantedSides);
      end;
  end;
end;

function TcxPCSlantedPainter.GetTabsNormalDistance: TcxPCDistance;

  function GetSlantCount: Integer;
  begin
    Result := 0;
    if spLeft in SlantedSides then
      Inc(Result);
    if spRight in SlantedSides then
      Inc(Result);
  end;

begin
  Result.dh := -3;
  if SlantedSides = [] then
    Result.dw := 1
  else
    Result.dw := -(GetCutValue div 2) * GetSlantCount;

  if ViewInfo.ActuallyRotate then
    RotateTabsDistance(Result);
end;

function TcxPCSlantedPainter.GetTabsRectOffset: TRect;
begin
  Result := cxEmptyRect;
  if spLeft in SlantedSides then
  begin
    if ViewInfo.TabPosition in [tpTop, tpBottom] then
      Result.Left := 1
    else
      Result.Bottom := 1;
  end;
  if spRight in SlantedSides then
  begin
    if ViewInfo.TabPosition in [tpTop, tpBottom] then
      Result.Right := 1
    else
      Result.Top := 1;
  end;
end;

function TcxPCSlantedPainter.GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := inherited GetTabTextNormalWidth(ATabVisibleIndex);
  if IsTabHasImage(ATabVisibleIndex) or IsAssignedImages and ViewInfo.ActuallyRotate then
    Inc(Result, GetImageTextDistance(ATabVisibleIndex));
end;

function TcxPCSlantedPainter.GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  Result := GetTabContentWOffset(ATabVisibleIndex);
end;

procedure TcxPCSlantedPainter.DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  R: TRect;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  R := ATabViewInfo.TextRect;
  if not IsRectEmpty(R) then
  begin
    case ViewInfo.GetTextRotationAngle of
      ra0:
        R := cxRectContent(R, Rect(-3, -2, -3, -2));
      raPlus90:
        R := cxRectContent(R, Rect(-2, -3, -2, -3));
      raMinus90:
        R := cxRectContent(R, Rect(-2, -3, -2, -3));
    end;

    ACanvas.Brush.Color := GetTabBodyColor(ATabVisibleIndex);
    InternalDrawFocusRect(ACanvas, ATabVisibleIndex, R);
  end;
end;

procedure TcxPCSlantedPainter.Init;
begin
  inherited Init;
  FCutValue := InternalGetCutValue;
end;

function TcxPCSlantedPainter.InternalCalculateTabNormalHeight: Integer;

  function GetDrawImageTotalHOffset: Integer;
  begin
    Result := cxMarginsHeight(GetDrawImageOffset(-1));
  end;

  function GetDrawTextTotalHOffset: Integer;
  begin
    Result := cxMarginsHeight(GetDrawTextHOffset(-1));
  end;

begin
  Result := GetMaxTabCaptionHeight + GetDrawTextTotalHOffset;
  Result := Max(Result, GetTabImageSize.cy + GetDrawImageTotalHOffset);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxPCSlantedPainter.PtInTab(TabVisibleIndex: Integer; X, Y: Integer): Boolean;
var
  ARegion: TcxRegion;
  AFirstIndex, ALastIndex: Integer;
begin
  ARegion := GetTabClipRgn(ViewInfo.Canvas, TabVisibleIndex);
  Result := ARegion.PtInRegion(X, Y);
  ARegion.Free;

  if Result then
  begin
    ViewInfo.InitializeVisibleTabRange(AFirstIndex, ALastIndex);
    if (TabVisibleIndex + 1 <= ALastIndex) and
      TabViewInfo[TabVisibleIndex + 1].IsMainTab then
    begin
      ARegion := GetTabClipRgn(ViewInfo.Canvas, TabVisibleIndex + 1);
      Result := not ARegion.PtInRegion(X, Y);
      ARegion.Free;
    end;
  end;
end;

function TcxPCSlantedPainter.IsTabBorderThick(ATabVisibleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxPCSlantedPainter.IsCloseButtonOnSlantedSide(ATabVisibleIndex: Integer): Boolean;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  Result := False;
  if ATabVisibleIndex <> -1 then
  begin
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    Result := (ATabViewInfo.PaintingPositionIndex in [5, 8]) and (spLeft in ViewInfo.TabSlants.Positions) or
      not (ATabViewInfo.PaintingPositionIndex in [5, 8]) and (spRight in ViewInfo.TabSlants.Positions);
  end;
end;

procedure TcxPCSlantedPainter.PaintButtonFrame(
  ACanvas: TcxCanvas; var ARect: TRect; AButtonState: TcxPCNavigatorButtonState);

  function GetFrameColor: TColor;
  begin
    if AButtonState in [nbsNormal, nbsDisabled] then
      Result := clBtnShadow
    else
      Result := clBlack;
  end;

begin
  ACanvas.DrawComplexFrame(ARect, GetFrameColor, GetFrameColor);
  InflateRect(ARect, -1, -1);
end;

procedure TcxPCSlantedPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
var
  AFrameRect: TRect;
begin
  AFrameRect := GetPageClientRect;
  InflateRect(AFrameRect, 1, 1);
  if ViewInfo.IsTabsContainer then
    AFrameRect := cxRectContent(AFrameRect, RotateRect(Rect(0, -TabsContainerOffset, 0, 0), ViewInfo.TabPosition));
  ACanvas.FrameRect(AFrameRect, GetFrameColor);
end;

procedure TcxPCSlantedPainter.PaintTabShape(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  ACanvas.SaveClipRegion;
  try
    PaintTabFrame(ACanvas, ATabVisibleIndex, ATabViewInfo.FullRect);
    ACanvas.SetClipRegion(InternalGetTabClipRegion(ACanvas, ATabVisibleIndex, True), roIntersect);
    PaintTabStateMark(ACanvas, ATabVisibleIndex);
    DrawBackground(ACanvas, ATabViewInfo.FullRect, ATabVisibleIndex,
      ATabViewInfo.PaintingPosition in [tpLeft, tpRight], False);
  finally
    ACanvas.RestoreClipRegion;
  end;
  if TabViewInfo[ATabVisibleIndex].IsMainTab then
    DrawTabUnderline(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCSlantedPainter.PaintTabsRegion(ACanvas: TcxCanvas);

  procedure DrawLineBorder(ATabIndexInterval: TcxPCIndexInterval; AIsUpperLine, AIsLowerLine: Boolean);
  var
    ALineRect: TRect;
    ALeftSide, ARightSide, ABottomSide: TcxBorder;
    ATabViewInfo: TcxTabViewInfo;
  begin
    ATabViewInfo := TabViewInfo[ATabIndexInterval.Left];
    ALineRect := GetExtendedRect(GetTabsLineRect(ATabIndexInterval, AIsLowerLine),
      Rect(0, -GetTabsNormalDistance.dh, 0, -1), ATabViewInfo.PaintingPosition);

    case ATabViewInfo.PaintingPosition of
      tpTop:
        begin
          ALeftSide := bLeft;
          ARightSide := bRight;
          ABottomSide := bBottom;
        end;
      tpBottom:
        begin
          ALeftSide := bLeft;
          ARightSide := bRight;
          ABottomSide := bTop;
        end;
      tpLeft:
        begin
          ALeftSide := bBottom;
          ARightSide := bTop;
          ABottomSide := bRight;
        end;
    else
      begin
        ALeftSide := bBottom;
        ARightSide := bTop;
        ABottomSide := bLeft;
      end;
    end;

    if (spLeft in SlantedSides) and not AIsUpperLine then
      DrawBorder(ACanvas, ALineRect, ALeftSide, [GetFrameColor], True);
    if (spRight in SlantedSides) and not AIsUpperLine then
      DrawBorder(ACanvas, ALineRect, ARightSide, [GetFrameColor], True);
    DrawBorder(ACanvas, ALineRect, ABottomSide, [GetFrameColor], True);
  end;

  procedure PaintLine(ATabIndexInterval: TcxPCIndexInterval; AIsUpperLine, AIsLowerLine: Boolean);
  var
    I: Integer;
  begin
    for I := ATabIndexInterval.Left to ATabIndexInterval.Right do
    begin
      if I <> ViewInfo.MainTabVisibleIndex then
        PaintTab(ACanvas, I);
    end;
    DrawLineBorder(ATabIndexInterval, AIsUpperLine, AIsLowerLine);
  end;

var
  J: Integer;
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  AFirstIndex, AlastIndex: Integer;
begin
  ViewInfo.InitializeVisibleTabRange(AFirstIndex, AlastIndex);

  if (ViewInfo.MainTabVisibleIndex <> -1) and
    (AFirstIndex <= ViewInfo.MainTabVisibleIndex) and
    (ViewInfo.MainTabVisibleIndex <= AlastIndex)
  then
    PaintTab(ACanvas, ViewInfo.MainTabVisibleIndex);

  ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);

  for J := ViewInfo.TopOrLeftPartRowCount - 1 downto 0 do
    PaintLine(ALineIndexBoundsA[J], J = 0, J = ViewInfo.TopOrLeftPartRowCount - 1);

  for J := ViewInfo.TopOrLeftPartRowCount to ViewInfo.RowCount - 1 do
    PaintLine(ALineIndexBoundsA[J], J = ViewInfo.RowCount - 1, J = ViewInfo.TopOrLeftPartRowCount);
end;

procedure TcxPCSlantedPainter.PrepareDrawTabContentBitmapBackground(ABitmap: TcxBitmap;
  const ABitmapPos: TPoint; ATabVisibleIndex: Integer);
var
  P: TPoint;
  R: TRect;
begin
  R := TabViewInfo[ATabVisibleIndex].FullRect;
  P := ABitmapPos;
  if ViewInfo.IsVerticalText then
  begin
    R := Rect(R.Top, R.Left, R.Bottom, R.Right);
    P := Point(P.Y, P.X);
  end;

  if ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raPlus90) then
    OffsetRect(R, -R.Left + P.X + ABitmap.Width - R.Right, -P.Y)
  else if not ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raMinus90) then
    OffsetRect(R, -P.X, -R.Top - (R.Bottom - P.Y - ABitmap.Height))
  else
    OffsetRect(R, -P.X, -P.Y);

  DrawBackground(ABitmap.cxCanvas, R, ATabVisibleIndex, ViewInfo.ActuallyRotate,
    not ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raMinus90) or
    ViewInfo.ActuallyRotate and (ViewInfo.GetTextRotationAngle = raPlus90));
end;

procedure TcxPCSlantedPainter.DrawTabContentBackground(
  ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ABounds);
    DrawBackground(ACanvas, TabViewInfo[ATabVisibleIndex].FullRect,
      ATabVisibleIndex, ViewInfo.TabPosition in [tpLeft, tpRight], False);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxPCSlantedPainter.CanLightMainTab: Boolean;
begin
  Result := False;
end;

function TcxPCSlantedPainter.DirectionalGetFigureRegion(const R: TRect;
  APoints: array of TPoint; ALinePositions: array of TLinePosition;
  ATabPositon: TcxTabPosition; AForContent: Boolean): TcxRegion;

  procedure CorrectPolylineEnd(AIndex: Integer);
  begin
    with APoints[AIndex] do
      case ALinePositions[High(ALinePositions)] of
        lpL:
          Dec(Y);
        lpLT:
          begin
            Inc(X);
            Dec(Y);
          end;
        lpT:
          Inc(X);
        lpRT:
          begin
            Inc(X);
            Inc(Y);
          end;
        lpR:
          Inc(Y);
        lpRB:
          begin
            Dec(X);
            Inc(Y);
          end;
        lpB:
          Dec(X);
        lpLB:
          begin
            Dec(X);
            Dec(Y);
          end;
      end;
  end;

  procedure CorrectPolylineEnds;
  begin
    CorrectPolylineEnd(High(APoints));
    if (ATabPositon = tpLeft) and (ALinePositions[0] = lpLB) then
    begin
      Inc(APoints[0].X);
      Inc(APoints[0].Y);
    end;
  end;

  procedure ReorderPoints;
  const
    ALinePositionConversionTable: array[TcxTabPosition, TLinePosition] of TLinePosition = (
      (lpL, lpLT, lpT, lpRT, lpR, lpRB, lpB, lpLB),
      (lpL, lpLB, lpB, lpRB, lpR, lpRB, lpB, lpLB),
      (lpB, lpLB, lpL, lpLT, lpT, lpRB, lpB, lpLB),
      (lpB, lpRB, lpR, lpRT, lpT, lpRB, lpB, lpLB)
    );
  var
    ALinePosition: TLinePosition;
    I: Integer;
    P: TPoint;
  begin
    case ATabPositon of
      tpBottom, tpRight:
        begin
          for I := 0 to Length(APoints) div 2 - 1 do
          begin
            P := APoints[I];
            APoints[I] := APoints[High(APoints) - I];
            APoints[High(APoints) - I] := P;
          end;
          for I := 0 to Length(ALinePositions) div 2 - 1 do
          begin
            ALinePosition := ALinePositions[I];
            ALinePositions[I] := ALinePositions[High(ALinePositions) - I];
            ALinePositions[High(ALinePositions) - I] := ALinePosition;
          end;
        end;
    end;
    for I := 0 to High(ALinePositions) do
      ALinePositions[I] := ALinePositionConversionTable[ATabPositon, ALinePositions[I]];
  end;

begin
  RotatePolyline(R, APoints, ATabPositon);
  ReorderPoints;
  CorrectPolylineEnds;
  Result := GetFigureRegion(APoints, ALinePositions, AForContent);
end;

procedure TcxPCSlantedPainter.DrawBackground(ACanvas: TcxCanvas; R: TRect;
  ATabVisibleIndex: Integer; AHorizontalGradient, AInverseGradient: Boolean);
var
  AColor1, AColor2: TColor;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ATabViewInfo.IsMainTab and not (CanLightMainTab and ATabViewInfo.IsHotTrack) then
  begin
    ACanvas.Brush.Color := GetTabBodyColor(ATabVisibleIndex);
    ACanvas.FillRect(R);
  end
  else
  begin
    if pcoGradient in ViewInfo.Options then
      GetBackgroundGradientColors(ATabVisibleIndex, AColor1, AColor2)
    else
    begin
      AColor1 := GetTabBodyColor(ATabVisibleIndex);
      AColor2 := AColor1;
    end;
    if CanLightMainTab and ATabViewInfo.IsMainTab and ATabViewInfo.IsHotTrack and
      (ATabViewInfo.PaintingPosition in [tpBottom, tpRight]) then
        ExchangeLongWords(AColor1, AColor2);
    if AInverseGradient then
      ExchangeLongWords(AColor1, AColor2);
    FillGradientRect(ACanvas.Handle, R, AColor1, AColor2, AHorizontalGradient);
  end;
end;

procedure TcxPCSlantedPainter.DrawTabUnderline(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AFrameColor: TColor;
  ARHeight, ARWidth: Integer;
  ATabViewInfo: TcxTabViewInfo;
  P: TPoint;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AFrameColor := GetFrameColor;
  ACanvas.Brush.Color := GetTabBodyColor(ATabVisibleIndex);
  ACanvas.FillRect(GetTabUnderlineRect(ATabViewInfo));

  if ATabViewInfo.IsMainTab then
  begin
    R := ATabViewInfo.FullRect;
    if not (spLeft in SlantedSides) then
    begin
      P := Point(R.Left, R.Bottom);
      RotatePoint(R, P, ATabViewInfo.PaintingPosition);
      ACanvas.Pixels[P.X, P.Y] := AFrameColor;
    end;
    if not (spRight in SlantedSides) then
    begin
      GetRectSize(R, ATabViewInfo.PaintingPosition in [tpTop, tpBottom], ARWidth, ARHeight);
      P := Point(R.Left + ARWidth - 1, R.Bottom);
      RotatePoint(R, P, ATabViewInfo.PaintingPosition);
      ACanvas.Pixels[P.X, P.Y] := AFrameColor;
    end;
  end;
end;

procedure TcxPCSlantedPainter.GetBackgroundGradientColors(ATabVisibleIndex: Integer;
  out AColor1, AColor2: TColor);
var
  ATabColor: TColor;
begin
  ATabColor := GetTabBodyColor(ATabVisibleIndex);
  if TabViewInfo[ATabVisibleIndex].IsHotTrack then
  begin
    AColor1 := Light(ATabColor, 20);
    AColor2 := ATabColor;
  end
  else
  begin
    AColor1 := Light(ATabColor, 13);
    AColor2 := dxGetDarkerColor(ATabColor, 90);
  end;
end;

function TcxPCSlantedPainter.GetCutValue: Integer;
begin
  Result := FCutValue;
end;

function TcxPCSlantedPainter.GetGeometricalMinTabWidth: Integer;

  function GetCutSideCount: Integer;
  begin
    Result := 0;
    if spLeft in ViewInfo.TabSlants.Positions then
      Inc(Result);
    if spRight in ViewInfo.TabSlants.Positions then
      Inc(Result);
  end;

const
  MinContentWidth = 4;
  RoundedCornerSideWidth = 2;
begin
  if ViewInfo.ActuallyRotate then
  begin
    Result := 0;
    Exit;
  end;

  if ViewInfo.TabSlants.Positions = [] then
    Result := RoundedCornerSideWidth * 2
  else
  begin
    Result := 0;
    Inc(Result, GetCutValue * GetCutSideCount);
    if ViewInfo.TabSlants.Kind = skSlant then
      Inc(Result, RoundedCornerSideWidth * (2 - GetCutSideCount))
    else
      Inc(Result, GetFrameWidth * (2 - GetCutSideCount));
  end;
  Inc(Result, MinContentWidth);
end;

function TcxPCSlantedPainter.GetFrameColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxPCSlantedPainter.GetSlantedSides: TcxTabSlantPositions;
begin
  if (ViewInfo.TabSlants.Kind = skSlant) and not ViewInfo.ActuallyRotate then
    Result := ViewInfo.TabSlants.Positions
  else
    Result := [];
end;

procedure TcxPCSlantedPainter.GetTabFramePolyline(ATabVisibleIndex: Integer; out APoints: TPoints; out ALinePositions: TLinePositions);
var
  ARHeight, ARWidth: Integer;
  ATabViewInfo: TcxTabViewInfo;
  R: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  R := ATabViewInfo.FullRect;
  GetRectSize(R, ATabViewInfo.PaintingPosition in [tpTop, tpBottom], ARWidth, ARHeight);

  SetLength(APoints, 0);
  SetLength(ALinePositions, 0);
  if (spLeft in ViewInfo.TabSlantPositions) and (not ViewInfo.ActuallyRotate or (ViewInfo.TabSlants.Kind = skCutCorner)) then
  begin
    if ViewInfo.TabSlants.Kind = skSlant then
      AddPoints(APoints, [Point(R.Left, R.Bottom - 1), Point(R.Left + (ARHeight - 1),
        R.Bottom - ARHeight)], ALinePositions, [lpLT])
    else
      AddPoints(APoints, [Point(R.Left, R.Bottom - 1), Point(R.Left, R.Bottom - ARHeight + CutCornerSize),
        Point(R.Left + CutCornerSize, R.Bottom - ARHeight)], ALinePositions, [lpL, lpLT]);
  end
  else
    if (ViewInfo.TabSlants.Kind = skSlant) or (ViewInfo.TabSlantPositions = []) then
      AddPoints(APoints, [Point(R.Left, R.Bottom - 1), Point(R.Left, R.Bottom - ARHeight + 2),
        Point(R.Left + 2, R.Bottom - ARHeight)], ALinePositions, [lpL, lpLT])
    else
      AddPoints(APoints, [Point(R.Left, R.Bottom - 1), Point(R.Left, R.Bottom - ARHeight)],
        ALinePositions, [lpL]);

  if (spRight in ViewInfo.TabSlantPositions) and (not ViewInfo.ActuallyRotate or (ViewInfo.TabSlants.Kind = skCutCorner)) then
  begin
    if ViewInfo.TabSlants.Kind = skSlant then
      AddPoints(APoints, [Point(ARWidth + R.Left - 1 - (ARHeight - 1), R.Bottom - ARHeight),
        Point(ARWidth + R.Left - 1, R.Bottom - 1)], ALinePositions, [lpT, lpRT])
    else
      AddPoints(APoints, [Point(ARWidth + R.Left - CutCornerSize - 1, R.Bottom - ARHeight),
        Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + CutCornerSize), Point(ARWidth + R.Left - 1, R.Bottom - 1)],
        ALinePositions, [lpT, lpRT, lpR]);
  end
  else
    if (ViewInfo.TabSlants.Kind = skSlant) or (ViewInfo.TabSlantPositions = []) then
      AddPoints(APoints, [Point(ARWidth + R.Left - 3, R.Bottom - ARHeight),
        Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 2), Point(ARWidth + R.Left - 1, R.Bottom - 1)],
        ALinePositions, [lpT, lpRT, lpR])
    else
      AddPoints(APoints, [Point(ARWidth + R.Left - 1, R.Bottom - ARHeight),
        Point(ARWidth + R.Left - 1, R.Bottom - 1)], ALinePositions, [lpT, lpR]);
end;

function TcxPCSlantedPainter.GetTabImageSize: TSize;
begin
  Result.cx := IfThen(IsAssignedImages, GetTabImageAreaWidth);
  Result.cy := IfThen(IsAssignedImages, GetTabImageAreaHeight);
end;

function TcxPCSlantedPainter.GetVerticalTextIndent: Integer;
begin
  Result := 0;
end;

function TcxPCSlantedPainter.InternalGetCutValue: Integer;
begin
  if ViewInfo.TabSlants.Kind = skSlant then
    Result := CalculateTabNormalHeight - 1
  else
    Result := CutCornerSize;
end;

function TcxPCSlantedPainter.InternalGetTabClipRegion(
  ACanvas: TcxCanvas; ATabVisibleIndex: Integer; AForContent: Boolean): TcxRegion;
var
  ALinePositions: TLinePositions;
  APoints: TPoints;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  GetTabFramePolyline(ATabVisibleIndex, APoints, ALinePositions);
  Result := DirectionalGetFigureRegion(ATabViewInfo.FullRect, APoints, ALinePositions, ATabViewInfo.PaintingPosition, AForContent);
end;

procedure TcxPCSlantedPainter.PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer; const R: TRect);
var
  ALinePositions: TLinePositions;
  APoints: TPoints;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  GetTabFramePolyline(ATabVisibleIndex, APoints, ALinePositions);
  DirectionalPolyline(ACanvas, R, APoints, ATabViewInfo.PaintingPosition, GetFrameColor);
end;

procedure TcxPCSlantedPainter.PaintTabStateMark(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  R: TRect;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ATabViewInfo.IsHotTrack or ATabViewInfo.IsMainTab then
  begin
    SubtractRect(R, ATabViewInfo.FullRect,
      GetExtendedRect(ATabViewInfo.FullRect, Rect(0, GetFrameWidth + SlantedPainterTabStateMarkWidth, 0, 0), ATabViewInfo.PaintingPosition));
    ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(R);
    ACanvas.ExcludeClipRect(R);
  end;
end;

function TcxPCSlantedPainter.GetTabsLineRect(ATabIndexInterval: TcxPCIndexInterval; AFullRect: Boolean): TRect;

  procedure ExchangeTabs(var ALeftTabVeiewInfo, ARightTabVeiwInfo: TcxTabViewInfo);
  var
    AExchangeTabViewInfo: TcxTabViewInfo;
  begin
    AExchangeTabViewInfo := ALeftTabVeiewInfo;
    ALeftTabVeiewInfo := ARightTabVeiwInfo;
    ARightTabVeiwInfo := AExchangeTabViewInfo;
  end;

var
  ALeftTabViewInfo, ARightTabViewInfo: TcxTabViewInfo;
begin
  ALeftTabViewInfo := TabViewInfo[ATabIndexInterval.Left];
  ARightTabViewInfo := TabViewInfo[ATabIndexInterval.Right];
  Result := ALeftTabViewInfo.VisibleRect;
  case ViewInfo.TabPosition of
    tpTop, tpBottom:
      begin
        if ViewInfo.IsRightToLeftAlignment then
          ExchangeTabs(ALeftTabViewInfo, ARightTabViewInfo);

        if not AFullRect and not (spLeft in SlantedSides) then
          Result.Left := ALeftTabViewInfo.VisibleRect.Left
        else
          Result.Left := 0;
        if not AFullRect and not (spRight in SlantedSides) then
          Result.Right := ARightTabViewInfo.VisibleRect.Right
        else
          Result.Right := ViewInfo.Width;
      end;
    tpLeft, tpRight:
      begin
        if not ViewInfo.IsBottomToTopAlignment then
          ExchangeTabs(ALeftTabViewInfo, ARightTabViewInfo);

        if not AFullRect and not (spRight in SlantedSides) then
          Result.Top := ARightTabViewInfo.VisibleRect.Top
        else
          Result.Top := 0;
        if not AFullRect and not (spLeft in SlantedSides) then
          Result.Bottom := ALeftTabViewInfo.VisibleRect.Bottom
        else
          Result.Bottom := ViewInfo.Height;
      end;
  end;
end;

function TcxPCSlantedPainter.GetTabUnderlineRect(ATabViewInfo: TcxTabViewInfo): TRect;
begin
  SubtractRect(Result,
    GetExtendedRect(ATabViewInfo.VisibleRect, Rect(0, 0, 0, -1), ATabViewInfo.PaintingPosition),
    ATabViewInfo.VisibleRect);
end;

{ TcxPCOneNotePainter }

class function TcxPCOneNotePainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCOneNoteStyle;
end;

class function TcxPCOneNotePainter.GetStyleName: TCaption;
begin
  Result := 'OneNote';
end;

class function TcxPCOneNotePainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

class function TcxPCOneNotePainter.AllowRotate: Boolean;
begin
  Result := False;
end;

function TcxPCOneNotePainter.CanLightMainTab: Boolean;
begin
  Result := True;
end;

procedure TcxPCOneNotePainter.DrawTabUnderline(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AColor: TColor;
  ARWidth, ARHeight: Integer;
  R: TRect;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ATabViewInfo.IsMainTab then
    AColor := GetTabBodyColor(ATabVisibleIndex)//ViewInfo.PageColors[ATabVisibleIndex]
  else
    AColor := GetFrameColor;
  R := ATabViewInfo.FullRect;
  GetRectSize(R, ATabViewInfo.PaintingPosition in [tpTop, tpBottom], ARWidth, ARHeight);
  DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom), Point(R.Left, R.Bottom)],
    ATabViewInfo.PaintingPosition, dxGetMiddleRGB(GetFrameColor, AColor, 50));
  DirectionalPolyline(ACanvas, R, [Point(R.Left + 1, R.Bottom), Point(R.Left + ARWidth - 1, R.Bottom)],
    ATabViewInfo.PaintingPosition, AColor);
end;

procedure TcxPCOneNotePainter.GetBackgroundGradientColors(ATabVisibleIndex: Integer; out AColor1, AColor2: TColor);
var
  ATabColor: TColor;
begin
  ATabColor := GetTabBodyColor(ATabVisibleIndex);
  if TabViewInfo[ATabVisibleIndex].IsHotTrack then
  begin
    AColor1 := Light(ATabColor, 20);
    AColor2 := ATabColor;
  end
  else
  begin
    AColor1 := Light(ATabColor, 13);
    AColor2 := dxGetDarkerColor(ATabColor, 90);
  end;
end;

function TcxPCOneNotePainter.GetButtonsRegionHOffset: Integer;
begin
  Result := 0;
  if ViewInfo.IsTabsContainer then
    Result := TabsContainerOffset;
end;

function TcxPCOneNotePainter.GetDrawImageOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := ScaleFactor.Apply(Rect(0, OneNotePainterTabFrameWidth + 1, 0, 1));
end;

function TcxPCOneNotePainter.GetDrawTextHOffset(TabVisibleIndex: Integer): TRect;
begin
  Result := ScaleFactor.Apply(Rect(0, OneNotePainterTabFrameWidth + 2, 0, 1));
end;

function TcxPCOneNotePainter.GetFrameColor: TColor;
begin
  Result := OneNoteMainTabBorderColor;
end;

function TcxPCOneNotePainter.GetGeometricalMinTabWidth: Integer;
const
  MinContentWidth = 2;
begin
  Result := GetCutValue + MinContentWidth + OneNotePainterTabFrameWidth;
end;

function TcxPCOneNotePainter.GetSlantedSides: TcxTabSlantPositions;
begin
  Result := [spLeft];
end;

function TcxPCOneNotePainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
const
  AContentOffset = 2;
begin
  Result.Left := GetCutValue;
  Result.Right := AContentOffset + OneNotePainterTabFrameWidth;
  if ViewInfo.GetTextRotationAngle = raMinus90 then
    ExchangeLongWords(Result.Left, Result.Right);
end;

function TcxPCOneNotePainter.GetTabsContainerOffsets: TRect;
begin
  Result := cxEmptyRect;
  if ViewInfo.IsTabsContainer then
    Result.Top := TabsContainerOffset;
end;

function TcxPCOneNotePainter.GetTabsNormalDistance: TcxPCDistance;
begin
  Result.dh := -3;
  Result.dw := -GetCutValue + 5;
end;

function TcxPCOneNotePainter.GetTabsRectOffset: TRect;
begin
  Result := RotateRect(Rect(4, 0, 4, 0), ViewInfo.TabPosition);
end;

function TcxPCOneNotePainter.InternalGetCutValue: Integer;
begin
  Result := CalculateTabNormalHeight + 2;
end;

function TcxPCOneNotePainter.InternalGetTabClipRegion(ACanvas: TcxCanvas; ATabVisibleIndex: Integer;
  AForContent: Boolean): TcxRegion;
var
  APaintingPosition: TcxTabPosition;
  ARHeight, ARWidth: Integer;
  R: TRect;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  ACanvas.BeginPath;
  try
    R := ATabViewInfo.FullRect;
    APaintingPosition := ATabViewInfo.PaintingPosition;
    if AForContent then
      case APaintingPosition of
        tpTop:
          begin
            Inc(R.Top);
            Inc(R.Bottom);
            GetRectSize(R, True, ARWidth, ARHeight);
            DirectionalPolyline(ACanvas, R, [Point(R.Left + 2, R.Bottom - 1), Point(R.Left + (R.Bottom - R.Top - 2), R.Top + 3),
              Point(R.Left + (R.Bottom - R.Top + 2), R.Top + 1),
              Point(R.Right - 2, R.Top + 1), Point(R.Right - 2, R.Bottom - 1)], tpTop, clWhite);
            end;
          tpBottom:
            begin
              Inc(R.Left);
              DirectionalPolyline(ACanvas, R, [Point(R.Left + 2, R.Bottom - 1), Point(R.Left + (R.Bottom - R.Top - 3), R.Top + 4),
                Point(R.Left + (R.Bottom - R.Top + 2), R.Top + 1),
                Point(R.Right - 2, R.Top + 1), Point(R.Right - 2, R.Bottom - 1)], tpBottom, clWhite);
            end;
          tpLeft:
            begin
              Inc(R.Top);
              Inc(R.Left);
              Inc(R.Right);
              GetRectSize(R, False, ARWidth, ARHeight);
              DirectionalPolyline(ACanvas, R, [Point(R.Left + 2, R.Bottom - 1), Point(R.Left + (ARHeight - 2), R.Bottom - ARHeight + 3),
                Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight + 1),
                Point(ARWidth + R.Left - 2, R.Bottom - ARHeight + 1), Point(ARWidth + R.Left - 2, R.Bottom - 1)], tpLeft, clWhite);
            end;
          tpRight:
            begin
              Inc(R.Top);
              GetRectSize(R, False, ARWidth, ARHeight);
              DirectionalPolyline(ACanvas, R, [Point(R.Left + 2, R.Bottom - 1), Point(R.Left + (ARHeight - 1), R.Bottom - ARHeight + 3),
                Point(R.Left + (ARHeight + 3), R.Bottom - ARHeight + 1),
                Point(ARWidth + R.Left - 2, R.Bottom - ARHeight + 1), Point(ARWidth + R.Left - 2, R.Bottom - 1)], tpRight, clWhite);
            end;
        end
      else
      begin
        InflateRect(R, 1, 1);
        GetRectSize(R, APaintingPosition in [tpTop, tpBottom], ARWidth, ARHeight);
        case APaintingPosition of
          tpTop:
            DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom - 2), Point(R.Left + 1, R.Bottom - 2),
              Point(R.Left + (ARHeight - 3), R.Bottom - ARHeight + 3), Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
              Point(ARWidth + R.Left - 4, R.Bottom - ARHeight), Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 3),
              Point(ARWidth + R.Left - 1, R.Bottom - 1)], tpTop, $9C613B);
          tpBottom:
            DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom - 2), Point(R.Left + 1, R.Bottom - 2),
              Point(R.Left + (ARHeight - 3), R.Bottom - ARHeight + 3), Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
              Point(ARWidth + R.Left - 4, R.Bottom - ARHeight), Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 3),
              Point(ARWidth + R.Left - 1, R.Bottom - 1)], tpBottom, $9C613B);
          tpLeft:
            DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom - 2), Point(R.Left + 1, R.Bottom - 2),
              Point(R.Left + (ARHeight - 3), R.Bottom - ARHeight + 3), Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
              Point(ARWidth + R.Left - 5, R.Bottom - ARHeight), Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 4),
              Point(ARWidth + R.Left - 1, R.Bottom - 1)], tpLeft, $9C613B);
          tpRight:
            DirectionalPolyline(ACanvas, R, [Point(R.Left - 1, R.Bottom - 1), Point(R.Left - 1, R.Bottom - 3), Point(R.Left + 2, R.Bottom - 3),
              Point(R.Left + (ARHeight - 2), R.Bottom - ARHeight + 2), Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
              Point(ARWidth + R.Left - 4, R.Bottom - ARHeight), Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 3),
              Point(ARWidth + R.Left - 1, R.Bottom - 1)], tpRight, $9C613B);
        end;
      end;
  finally
    ACanvas.EndPath;
    Result := ACanvas.PathToRegion;
  end;
end;

function TcxPCOneNotePainter.IsCloseButtonOnSlantedSide(ATabVisibleIndex: Integer): Boolean;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  Result := False;
  if ATabVisibleIndex <> -1 then
  begin
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    Result := (ATabViewInfo.PaintingPositionIndex in [5, 8]);
  end;
end;

procedure TcxPCOneNotePainter.PaintTabFrame(ACanvas: TcxCanvas; ATabVisibleIndex: Integer;
  const R: TRect);

  procedure PrepareColors(out ABorderColor, AInnerBorderColor1, AInnerBorderColor2: TColor);
  var
    ATabViewInfo: TcxTabViewInfo;
  begin
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    if ATabViewInfo.IsHotTrack then
    begin
      ABorderColor := OneNoteTabHotBorderColor;
      AInnerBorderColor1 := OneNoteTabHotBorderColor;
      AInnerBorderColor2 := OneNoteTabHotBorderColor;
    end
    else
      if ATabViewInfo.IsMainTab then
      begin
        ABorderColor := OneNoteMainTabBorderColor;
        AInnerBorderColor1 := dxGetMiddleRGB(clWhite, GetTabBodyColor(ATabVisibleIndex), 60);
        AInnerBorderColor2 := dxGetMiddleRGB(AInnerBorderColor1, GetTabBodyColor(ATabVisibleIndex), 70);
      end
      else
      begin
        ABorderColor := OneNoteTabBorderColor;
        AInnerBorderColor1 := dxGetMiddleRGB(clWhite, OneNoteTabInnerBorderColor2, 50);
        AInnerBorderColor1 := dxGetMiddleRGB(AInnerBorderColor1, GetTabBodyColor(ATabVisibleIndex), 50);
        AInnerBorderColor2 := dxGetMiddleRGB(OneNoteTabInnerBorderColor2, GetTabBodyColor(ATabVisibleIndex), 50);
      end;
  end;

  function GetColorAt(P: TPoint): TColor;
  var
    ANeighbourTabVisibleIndex: Integer;
  begin
    RotatePoint(R, P, TabViewInfo[ATabVisibleIndex].PaintingPosition);
    ANeighbourTabVisibleIndex := ViewInfo.VisibleIndexOfTabAt(P.X, P.Y);
    if ANeighbourTabVisibleIndex = -1 then
      Result := ViewInfo.Color
    else
      Result := GetTabBodyColor(ANeighbourTabVisibleIndex);
  end;

var
  ABorderColor, AInnerBorderColor1, AInnerBorderColor2: TColor;
  ARHeight, ARWidth: Integer;
  ATabPaintingPosition: TcxTabPosition;
begin
  ATabPaintingPosition := TabViewInfo[ATabVisibleIndex].PaintingPosition;
  GetRectSize(R, ATabPaintingPosition in [tpTop, tpBottom], ARWidth, ARHeight);
  PrepareColors(ABorderColor, AInnerBorderColor1, AInnerBorderColor2);

  DirectionalPolyline(ACanvas, R, [Point(R.Left, R.Bottom - 1), Point(R.Left, R.Bottom - 1)],
    ATabPaintingPosition, dxGetMiddleRGB(ABorderColor, GetColorAt(Point(R.Left - 1, R.Bottom - 1)), 25));
  DirectionalPolyline(ACanvas, R, [Point(R.Left + 1, R.Bottom - 1), Point(R.Left + 1, R.Bottom - 1),
    Point(R.Left + (ARHeight - 3), R.Bottom - ARHeight + 3), Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
    Point(ARWidth + R.Left - 3, R.Bottom - ARHeight), Point(ARWidth + R.Left - 1, R.Bottom - ARHeight + 2),
    Point(ARWidth + R.Left - 1, R.Bottom - 1)], ATabPaintingPosition, ABorderColor);
  DirectionalPolyline(ACanvas, R, [Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight),
    Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight)], ATabPaintingPosition, dxGetMiddleRGB(ABorderColor, GetColorAt(Point(R.Left + (ARHeight + 1), R.Bottom - ARHeight)), 60));
  DirectionalPolyline(ACanvas, R, [Point(R.Left + 2, R.Bottom - 1), Point(R.Left + (ARHeight - 3), R.Bottom - ARHeight + 4),
    Point(R.Left + (ARHeight + 2), R.Bottom - ARHeight + 1),
    Point(ARWidth + R.Left - 4, R.Bottom - ARHeight + 1)], ATabPaintingPosition, AInnerBorderColor1);
  DirectionalPolyline(ACanvas, R, [Point(ARWidth + R.Left - 3, R.Bottom - ARHeight + 1), Point(ARWidth + R.Left - 2, R.Bottom - ARHeight + 2),
    Point(ARWidth + R.Left - 2, R.Bottom - 1)], ATabPaintingPosition, AInnerBorderColor2);
end;

procedure TcxPCOneNotePainter.PaintTabStateMark(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
begin
  // do nothing
end;

{ TcxPaletteChangedNotifier }

procedure TcxPaletteChangedNotifier.DoChanged;
begin
  PrepareOneNoteStyleColors;
end;

{ TdxScaleFactorHelper }

function TdxScaleFactorHelper.Apply(const P: TcxPCWOffset): TcxPCWOffset;
begin
  Result.Right := Apply(P.Right);
  Result.Left := Apply(P.Left);
end;

initialization
  CalculateLightBrushColor;
  PrepareOneNoteStyleColors;
  FPaletteChangedNotifier := TcxPaletteChangedNotifier.Create(True);
  RegisterPCPainterClass(TcxPCTabsPainter);
  RegisterPCPainterClass(TcxPCButtonsPainter);
  RegisterPCPainterClass(TcxPCFlatButtonsPainter);
  RegisterPCPainterClass(TcxPCExtraFlatPainter);
  RegisterPCPainterClass(TcxPCUltraFlatPainter);
  RegisterPCPainterClass(TcxPCFlatPainter);
  RegisterPCPainterClass(TcxPCOffice11Painter);
  RegisterPCPainterClass(TcxPCSlantedPainter);
  RegisterPCPainterClass(TcxPCOneNotePainter);

finalization
  UnregisterPCPainterClass(TcxPCTabsPainter);
  UnregisterPCPainterClass(TcxPCButtonsPainter);
  UnregisterPCPainterClass(TcxPCFlatButtonsPainter);
  UnregisterPCPainterClass(TcxPCExtraFlatPainter);
  UnregisterPCPainterClass(TcxPCUltraFlatPainter);
  UnregisterPCPainterClass(TcxPCFlatPainter);
  UnregisterPCPainterClass(TcxPCOffice11Painter);
  UnregisterPCPainterClass(TcxPCSlantedPainter);
  UnregisterPCPainterClass(TcxPCOneNotePainter);
  FreeAndNil(FPaletteChangedNotifier);

end.

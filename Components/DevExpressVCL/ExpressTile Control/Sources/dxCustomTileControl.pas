{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressTileControl                                       }
{                                                                    }
{           Copyright (c) 2011-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSTILECONTROL AND ALL            }
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

unit dxCustomTileControl;

{$I cxVer.inc}
interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Forms, SysUtils, Classes, Controls, StdCtrls, Graphics, Math, ImgList, Dialogs, Types, Menus,
  dxCoreClasses, dxCore, dxCoreGraphics, cxGeometry, cxClasses, cxGraphics, cxControls, cxLookAndFeels, dxSkinInfo,
  cxLookAndFeelPainters, dxGDIPlusClasses, dxGDIPlusAPI, cxInplaceContainer, cxStorage, dxSkinsCore, cxDrawTextUtils,
  cxLibraryConsts, dxHooks, dxAnimation, cxButtonEdit, cxEdit, dxMessages;


resourcestring
  dxTileControlDefaultGroupCaptionHint = 'Name group';

const
  // appearances
  dxTileControlDefaultActionBarsIndentHorz = 24;
  dxTileControlDefaultActionBarsIndentVert = 14;
  dxTileControlDefaultActionBarsItemIndent = 30;
  dxTileControlDefaultAnimateText = True;
  dxTileControlDefaultIndentHorz = 74;
  dxTileControlDefaultIndentVert = 40;
  dxTileControlDefaultItemIndent = 8;
  dxTileControlDefaultItemSize = 120;
  dxTileControlDefaultGroupIndent = 56;
  dxTileControlDefaultGroupBlockColumnCount = 2;
  dxTileControlDefaultRowCount = 4;
  dxTileControlDefaultItemRowCount = 1;
  dxTileControlItemMaxRowCount = 10;
  dxTileControlSizeGroupBorder = 1;

  dxTileFocusItemFrameSize = 3;
  dxTileFocusItemOuterFrameSize = 4;
  dxTileItemObjectDefaultIndent = 6;

  dxTileControlInvisiblePosition = 15000;

  dxTileControlDefaultTabFontSize = 12;
  dxTileControlDefaultTitleFontSize = 32;
  dxTileControlDefaultGroupCaptionFontSize = 16;

  dxTileDefaultActionBarColor = $D2D2D2;
  dxTileDefaultActionBarTextColor = clBlack;
  dxTileControlDropArrowColor = $00FF00;
  dxTileItemDisabledMaskColor = $7FFFFFFF;
  dxTileItemLightColor = $30A965;
  dxTileItemShadowColor = $17934B;
  dxTileVirtualGroupBackgroundColor = $444444;
  dxTileItemPlaceColor = $444444;

  dxTileCheckedItemCheckMarkAreaSize = 45;
  dxTileCheckedItemCheckMarkPenSize = dxTileCheckedItemCheckMarkAreaSize div 15;
  dxTileCheckedItemFrameBorderSize = 4;

  // customizing
  dxTileControlAutoScrollWidth = 20;
  dxTileControlResizeDelta = 20;
  dxTileControlScrollStep = 20;
  dxTileControlItemHotTrackHighlightDefaultColor = $D1FA9729;

  dxTileControlDefaultScrollSync = 15;

  // animation
  dxTileControlInflateAnimationStepsCount = 7;
  dxTileControlDefaultAnimationInterval = 3000;
  dxTileControlDefaultAnimationMode = amScrollUp;
  dxTileDropAnimationStepCount = 10000;
  //
  dxTileControlActionBarsAnimationTime = 200;
  dxTileControlActivateDetailAnimationTime = 500;
  dxTileControlHotTrackItemAnimationTime = 500;
  dxTileControlInflateAnimationTime = 150;
  dxTileControlCenterContentAnimationTime = 150;
  dxTileControlFramesAnimationTime = 1000;
  dxTileControlGalleryAnimationTime = 750;
  dxTileControlRubberAnimationTime = 400;
  dxTileControlTabScrollAnimationTime = 200;

  // base index for hit test bits
  tchtGroup        = 1;
  tchtItem         = 2;
  tchtPageTab      = 3;
  tchtBackButton   = 4;
  tchtActionBar    = 5;
  tchtActionButton = 6;
  tchtScrollButton = 7;
  tchtScrollButtonArea = 8;
  tchtTitle = 9;
  tchtGroupCaption = 10;
  tchtActionBarTop    = 11;
  tchtActionBarBottom = 12;
  tchtTabScrollButton = 13;

  dxTileControlStoringVersion = 2;

type
  TdxCustomTileControl = class;
  TdxTileControlActionBarController = class;
  TdxTileControlActionBarItem = class;
  TdxTileControlActionBars = class;
  TdxTileControlCustomActionBar = class;
  TdxTileControlViewInfo = class;
  TdxTileControlGroupCollection = class;
  TdxTileControlGroup = class;
  TdxTileControlItemCollection = class;
  TdxTileControlCustomItem = class;
  TdxTileControlItem = class;
  TdxTileControlItemFrame = class;
  TdxTileControlHintController = class;
  TdxTileControlController = class;
  TdxTileControlHitTest = class;
  TdxTileControlCustomActionBarViewInfo = class;
  TdxTileControlCustomCellViewInfo = class;
  TdxTileControlCustomItemViewData = class;
  TdxTileControlPainter = class;
  TdxTileControlItemNavigator = class;
  TdxTileControlTitle = class;
  TdxTileControlDetailSite = class;
  TdxTileControlDetailSiteTitleViewInfo = class;
  TdxTileControlDragAndDropChanges = class;
  TdxTileControlDragDropCustomObject = class;
  TdxTileControlHandScroll = class;
  TdxTileControlScrollButtonViewInfo = class;
  TdxTileControlGroupCaption = class;
  TdxTileControlItemGalleryCell = class;
  TdxTileControlGalleryCellController = class;

  TdxTileControlOptionsBehaviorClass = class of TdxTileControlOptionsBehavior;
  TdxTileControlOptionsViewClass = class of TdxTileControlOptionsView;
  TdxTileControlScrollButtonViewInfoClass = class of TdxTileControlScrollButtonViewInfo;
  TdxTileControlItemDetailOptionsClass = class of TdxTileControlItemDetailOptions;
  TdxTileControlDetailSiteTitleViewInfoClass = class of TdxTileControlDetailSiteTitleViewInfo;

  TdxTileControlAutoScrollSide = (sLeft, sRight, sTop, sBottom);
  TdxTileControlAutoScrollSides = set of TdxTileControlAutoScrollSide;

  TdxTileControlDetailAnimationMode = (damScroll, damFade, damSegmentedFade, damRandomSegmentedFade, damScrollFade);

  TdxTileControlRectIntersectedBounds = (rcNone, rcFull,
    rcFullLeft, rcFullTop, rcFullRight, rcFullBottom, rcLeft, rcRight);

  TdxTileControlActionBarVisibilityChangeReason = (abvcrManualSwitch, abvcrActionButtonClick,
    abvcrActiveDetailChange, abvcrMouseLeftClick, abvcrMouseMiddleClick, abvcrMouseRightClick,
    abvcrTileElementInteraction, abvcrTileItemCheckedStateChange);

  TdxTileControlItemCheckMode = (tcicmNone, tcicmMultiple);
  TdxTileControlItemSize = (tcisSmall, tcisRegular, tcisLarge, tcisExtraLarge);
  TdxTileControlItemType = (tcitFrame, tcitGallery);
  TdxTileControlItemFocusMode = (tcifmDefault, tcifmOuterFrame);
  TdxTileControlItemHotTrackMode = (tcihtmNone, tcihtmFrame, tcihtmHighlight, tcihtmOuterFrame);
  TdxTileControlGroupLayout = (glHorizontal, glVertical);
  TdxTileControlScrollMode = (smDefault, smScrollbars, smScrollButtons);

  TdxTileControlVirtualGroupState = (vgsNoVisible, vgsVisible);

  TdxTileControlDesignTimeWhatSelected = (tcdtwsNone, tcdtwsItem, tcdtwsItems, tcdtwsGroup, tcdtwsGroups, tcdtwsMixed);

  TdxLayoutItemPosition = class
  private
    FColumn: Integer;
    FRow: Integer;
  protected
    procedure Assign(ASource: TdxLayoutItemPosition);
    function IsEqual(APosition: TdxLayoutItemPosition): Boolean;
  public
    property Column: Integer read FColumn write FColumn;
    property Row: Integer read FRow write FRow;
  end;

  { TdxTileControlCells }

  TdxTileControlCells = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxTileControlCustomCellViewInfo;
  protected
    procedure DrawWithoutClipping(ACanvas: TcxCanvas);
  public
    function CalculateHitTest(AHitTest: TdxTileControlHitTest): Boolean; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure RefreshState; virtual;

    property Items[AIndex: Integer]: TdxTileControlCustomCellViewInfo read GetItem; default;
  end;

  { TdxTileControlCustomViewInfo }

  TdxTileControlCustomViewInfo = class(TcxIUnknownObject)
  private
    FBounds: TRect;
  protected
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; virtual; abstract;
    function GetScaleFactor: TdxScaleFactor; virtual; abstract;
    function IsEqual(ACompare: TdxTileControlCustomViewInfo): Boolean; virtual;
    //
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    procedure Draw(ACanvas: TcxCanvas); virtual; abstract;
    procedure RefreshState; virtual;
    //
    property Bounds: TRect read FBounds write FBounds;
  end;

  { TdxTileControlCustomCellViewInfo }

  TdxTileControlCustomCellViewInfo = class(TdxTileControlCustomViewInfo)
  private
    FCalculated: Boolean;
    FClipRect: TRect;
    FVisibleBounds: TRect;
    FVisible: Boolean;
    procedure SetCalculate(AValue: Boolean);
  protected
    procedure CalculateClipping; virtual;
    procedure DoCalculate; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); virtual;
    function GetHeight: Integer; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetVisibleBounds: TRect; virtual;
    procedure Offset(const DX, DY: Integer); virtual;
    procedure Scroll(const DX, DY: Integer); virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
  public
    procedure Calculate;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure Invalidate; virtual;
    procedure Recalculate;
    //
    property Calculated: Boolean read FCalculated write SetCalculate;
    property ClipRect: TRect read FClipRect;
    property Visible: Boolean read FVisible;
    property VisibleBounds: TRect read FVisibleBounds write FVisibleBounds;
  end;

  { TdxTileControlAnimatedDragAndDropCustomCellViewInfo }

  TdxTileControlAnimatedDragAndDropCustomCellViewInfo = class(TdxTileControlCustomCellViewInfo)
  protected
    procedure DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect); virtual;
    function GetBaseDrawBounds: TRect; virtual;
    function GetDrawBounds(AAbsolutePosition: Boolean = False): TRect;
    function GetIsAnimatedOnDragDrop: Boolean; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTileControl: TdxCustomTileControl; virtual;

    property IsAnimatedOnDragDrop: Boolean read GetIsAnimatedOnDragDrop;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlItemViewInfo }

  TdxTileControlItemViewInfo = class(TdxTileControlAnimatedDragAndDropCustomCellViewInfo)
  private
    FCache: TcxBitmap;
    FContentAnimation: TdxAnimationTransition;
    FHottrackValue: Integer;
    FInflateDelta: Integer;
    FIsDirty: Boolean;
    FIsDrawnAsEnabled: Boolean;
    FItem: TdxTileControlItem;
    FPosition: TdxLayoutItemPosition;
    FViewData: TdxTileControlCustomItemViewData;
    function CanHottracked: Boolean;
    function GetController: TdxTileControlController;
    function GetDragItemPlaceBounds: TRect;
    function GetFocused: Boolean;
    function GetGalleryCellController: TdxTileControlGalleryCellController;
    function GetGroup: TdxTileControlGroup;
    function GetHottracked: Boolean;
    function GetItemIndent: Integer;
    function GetNeedDrawDragItemPlace: Boolean;
    function GetPainter: TdxTileControlPainter;
    function GetSelected: Boolean;
    function GetViewData: TdxTileControlCustomItemViewData;
    procedure SetHottrackValue(AValue: Integer);
    procedure SetIsDirty(AValue: Boolean);
    procedure SetViewData(AValue: TdxTileControlCustomItemViewData);
  protected
    procedure CalculateBounds; virtual;
    function CanFocusInDesigning: Boolean; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DirtyChanged; virtual;
    procedure DrawChecked(ACanvas: TcxCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect); override;
    procedure DrawFocusRect(ACanvas: TcxCanvas); virtual;
    procedure DrawFrameRect(ACanvas: TcxCanvas; AColor: TColor; AIsOuter: Boolean; AColorAlpha: Byte = 255); virtual;
    procedure DrawHighlightRect(ACanvas: TcxCanvas; const R: TRect; const AHighlightColor: TdxAlphaColor); virtual;
    procedure DrawHottrackRect(ACanvas: TcxCanvas); virtual;
    procedure DrawSelection(ACanvas: TcxCanvas); virtual;
    function GetActualFocusFrameSize: Integer; virtual;
    function GetActualVisualBounds: TRect; virtual;
    function GetBaseDrawBounds: TRect; override;
    function GetHeight: Integer; override;
    function GetHighlightBounds(const ABounds: TRect): TRect; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetLastOccupiedColumn(AStartColumn: Integer): Integer;
    function GetLastOccupiedRow(AStartRow: Integer): Integer;
    function GetLeft: Integer; virtual;
    function GetTileControl: TdxCustomTileControl; override;
    function GetTop: Integer; virtual;
    function GetVisibleBounds: TRect; override;
    function GetWidth: Integer; virtual;
    procedure NotifyBiDiModeChanged;
    procedure Scroll(const DX, DY: Integer); override;
    procedure SetInflateDelta(AValue: Integer); virtual;
    procedure ValidateCache(const ADrawRect: TRect); virtual;
    procedure ValidateImage; virtual;

    property Cache: TcxBitmap read FCache;
    property ContentAnimation: TdxAnimationTransition read FContentAnimation;
    property Controller: TdxTileControlController read GetController;
    property GalleryCellController: TdxTileControlGalleryCellController read GetGalleryCellController;
    property DragItemPlaceBounds: TRect read GetDragItemPlaceBounds;
    property HottrackValue: Integer read FHottrackValue write SetHottrackValue;
    property InflateDelta: Integer read FInflateDelta write SetInflateDelta;
    property IsDirty: Boolean read FIsDirty write SetIsDirty;
    property IsDrawnAsEnabled: Boolean read FIsDrawnAsEnabled write FIsDrawnAsEnabled;
    property ItemIndent: Integer read GetItemIndent;
    property NeedDrawDragItemPlace: Boolean read GetNeedDrawDragItemPlace;
  public
    constructor Create(AOwner: TdxTileControlItem); virtual;
    destructor Destroy; override;
    procedure Invalidate; override;

    property Focused: Boolean read GetFocused;
    property Group: TdxTileControlGroup read GetGroup;
    property Hottracked: Boolean read GetHottracked;
    property Item: TdxTileControlItem read FItem;
    property Painter: TdxTileControlPainter read GetPainter;
    property Position: TdxLayoutItemPosition read FPosition write FPosition;
    property Selected: Boolean read GetSelected;
    property ViewData: TdxTileControlCustomItemViewData read GetViewData write SetViewData;
  end;

  { TdxTileControlCustomGroupViewInfo }

  TdxTileControlCustomGroupViewInfo = class(TdxTileControlCustomCellViewInfo)
  private
    FGroup: TdxTileControlGroup;
    function GetTileControl: TdxCustomTileControl;
    function GetPainter: TdxTileControlPainter;
  protected
    procedure CalculateBounds; virtual;
    procedure CalculateSize(var AWidth, AHeight: Integer); virtual;
    function GetBorderWidth: Integer; virtual;
    function GetLeftPos: Integer; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTopPos: Integer; virtual;
    function GetVisibleBounds: TRect; override;
  public
    constructor Create(AOwner: TdxTileControlGroup); virtual;

    property Group: TdxTileControlGroup read FGroup;
    property Painter: TdxTileControlPainter read GetPainter;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlVirtualGroupViewInfo }

  TdxTileControlVirtualGroupViewInfo = class(TdxTileControlCustomGroupViewInfo)
  private
    FDrawingBounds: TRect;
    FBasisWidth: Integer;
    FSizeMustFromDragItem: Boolean;
    FState: TdxTileControlVirtualGroupState;
    procedure SetState(const AValue: TdxTileControlVirtualGroupState);

    procedure CalculateSizeAtHorizontalGroupLayout(var AWidth, AHeight: Integer);
    procedure CalculateSizeAtVerticalGroupLayout(var AWidth, AHeight: Integer);
    procedure GetDrawingBoundsParamsAtHorizontalGroupLayout(var ALeft, ATop, AWidth, AHeight: Integer);
    procedure GetDrawingBoundsParamsAtVerticalGroupLayout(var ALeft, ATop, AWidth, AHeight: Integer);
    function GetGroupLayout: TdxTileControlGroupLayout;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
  protected
    procedure CalculateDrawingBounds; virtual;
    procedure CalculateSize(var AWidth, AHeight: Integer); override;
    procedure DoCalculate; override;
    procedure ExpandBounds(const DW, DH: Integer);
    function GetLeftPos: Integer; override;
    function GetTopPos: Integer; override;
    procedure Offset(const DX, DY: Integer); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure Invalidate; override;
    procedure RecalculateDrawingBounds;

    property DrawingBounds: TRect read FDrawingBounds;
    property BasisWidth: Integer read FBasisWidth;
    property State: TdxTileControlVirtualGroupState read FState write SetState;
    property SizeMustFromDragItem: Boolean read FSizeMustFromDragItem write FSizeMustFromDragItem;
  end;

  { TdxTileControlGroupViewInfo }

  TdxTileControlGroupViewInfo = class(TdxTileControlCustomGroupViewInfo)
  private
    FColumnCount: Integer;
    FExpandedBounds: TRect;
    FItemsBottom: Integer;
    FItemsRight: Integer;
    FItemsLocalBottom: Integer;
    FItemsLocalRight: Integer;
    FMaximizedAreaBounds: TRect;
    FNewGroupDrawingBounds: TRect;
    FRowCount: Integer;
    function GetSelected: Boolean;
    // used in calculations
    procedure ExpandBoundsToBottomControl(var ABounds: TRect);
    procedure ExpandBoundsToLeftControl(var ABounds: TRect);
    procedure ExpandBoundsToRightControl(var ABounds: TRect);
    procedure ExpandBoundsToTopControl(var ABounds: TRect);
    function GetSmallColumnCountInLogicalRow: Integer;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetLogicalRowSize: Integer;
    function GetTilesMaximizedAreaHeight: Integer;
    function GetTilesMaximizedAreaWidth: Integer;
    function IsAreasIntersect(const AArea1, AArea2: TRect): Boolean;
    function IsGroupEmptyAndDragItemOverIt: Boolean;
    function IsItemOccupyPosition(AItem: TdxTileControlItem; APosition: TdxLayoutItemPosition): Boolean;
    function IsOccupiedByPriorItems(ACurrentItem: TdxTileControlItem; ACurrentRow, ACurrentColumn: Integer): Boolean;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
  protected
    procedure CalculateExpandedBounds; virtual;
    procedure CalculateMaximizedAreaBounds; virtual;
    procedure CalculateDimensionAndItemsPositions;
    procedure CalculateItemPosition(AItem, APrevItem: TdxTileControlItem);
    procedure CalculateItemsEdges;
    procedure CalculateSize(var AWidth, AHeight: Integer); override;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetLeftPos: Integer; override;
    function GetOccupiedItem(APosition: TdxLayoutItemPosition): TdxTileControlItem;
    class function GetStartOfCurrentLogicalColumn(const ASmallColumn, ASmallColumnCountInLogicalRow: Integer): Integer;
    class function GetStartOfCurrentRegularColumn(const ASmallColumn: Integer): Integer;
    class function GetStartOfCurrentRegularRow(const ASmallRow: Integer): Integer;
    function GetTopPos: Integer; override;
    function GetZIndex(AColumn, ARow, ADragItemRow: Integer): Integer; overload;
    function GetZIndex(APosition, ADragItemPosition: TdxLayoutItemPosition): Integer; overload;
    procedure Offset(const DX, DY: Integer); override;

    property ItemsBottom: Integer read FItemsBottom;
    property ItemsRight: Integer read FItemsRight;
    property SmallColumnCountInLogicalRow: Integer read GetSmallColumnCountInLogicalRow;
    property LogicalRowSize: Integer read GetLogicalRowSize;
  public
    function GetContentRect: TRect;
    procedure Invalidate; override;

    property ColumnCount: Integer read FColumnCount;
    property ExpandedBounds: TRect read FExpandedBounds;
    property MaximizedAreaBounds: TRect read FMaximizedAreaBounds;
    property RowCount: Integer read FRowCount;
    property Selected: Boolean read GetSelected;
  end;

  { TdxTileControlTitleViewInfo }

  TdxTileControlTitleViewInfo = class(TdxTileControlCustomCellViewInfo)
  private
    FFont: TFont;
    FGlyphBounds: TRect;
    FTextBounds: TRect;
    FTextOffset: Integer;
    FTitle: TdxTileControlTitle;
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftScrollBar: Boolean;
    function GetPainter: TdxTileControlPainter;
    function GetTextHeight: Integer;
    function GetTileControl: TdxCustomTileControl;
  protected
    procedure CheckBiDiModeFlags; virtual;
    procedure Clear; virtual;
    procedure DoCalculate; override;
    procedure DoCalculateGlyphBounds; virtual;
    procedure DoCalculateTextBounds; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHeight: Integer; override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetLeftOffset: Integer; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTextColor: TColor;
    function GetTextOutFlags: Integer;
    function GetVisibleBounds: TRect; override;
    function MeasureTextBounds(const AVisibleBounds: TRect): TRect;
    procedure Scroll(const DX, DY: Integer); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure SetTextBounds(const ABounds: TRect);

    property TileControl: TdxCustomTileControl read GetTileControl;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment;
    property UseRightToLeftScrollBar: Boolean read FUseRightToLeftScrollBar;
  public
    constructor Create;
    destructor Destroy; override;

    property Font: TFont read FFont;
    property GlyphBounds: TRect read FGlyphBounds;
    property Painter: TdxTileControlPainter read GetPainter;
    property TextBounds: TRect read FTextBounds;
    property TextColor: TColor read GetTextColor;
    property TextOffset: Integer read FTextOffset;
    property Title: TdxTileControlTitle read FTitle;
  end;

  { TdxTileControlCustomDesignHelper }

  TdxTileControlCustomDesignHelper = class(TPersistent)
  protected
    function GetControl: TdxCustomTileControl; virtual;
  public
    constructor Create(AControl: TdxCustomTileControl); virtual;
    procedure CreateItemClickHandler(AItem: TdxTileControlItem); virtual; abstract;
    function IsObjectSelected(AObject: TPersistent): Boolean; virtual; abstract;
    procedure Select(AObject: TPersistent; AShift: TShiftState); virtual; abstract;
    procedure SetSelection(AList: TList); virtual; abstract;
    procedure UnselectObject(AObject: TPersistent); virtual; abstract;

    property Control: TdxCustomTileControl read GetControl;
  end;

  TdxTileControlCustomDesignHelperClass = class of TdxTileControlCustomDesignHelper;

  { TdxTileControlHintController }

  TdxTileControlHintController = class
  private
    FOwner: TdxTileControlController;
    function GetTileControl: TdxCustomTileControl;
  protected
  public
    constructor Create(AOwner: TdxTileControlController); virtual;

    property Owner: TdxTileControlController read FOwner;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlInflateItemAnimation }

  TdxTileControlInflateItemAnimation = class(TdxAnimationTransition)
  private
    FDelta: Integer;
    FItem: TdxTileControlItem;
    FTileControl: TdxCustomTileControl;
  protected
    procedure DoAnimate; override;
    procedure InflateItem(AItem: TdxTileControlItem; const AData: Pointer);
  public
    constructor Create(ATileControl: TdxCustomTileControl;
      AItem: TdxTileControlItem; const ADelta, ACount: Integer); reintroduce;
    destructor Destroy; override;

    property Item: TdxTileControlItem read FItem;
    property TileControl: TdxCustomTileControl read FTileControl;
  end;

  { TdxTileControlFramesAnimation }

  TdxTileControlFramesAnimation = class(TdxImageAnimationTransition)
  private
    FDestViewData: TdxTileControlCustomItemViewData;
    FItem: TdxTileControlItem;
    FSourceViewData: TdxTileControlCustomItemViewData;
  protected
    procedure InitializeViewData; virtual;
    //
    property DestViewData: TdxTileControlCustomItemViewData read FDestViewData;
    property SourceViewData: TdxTileControlCustomItemViewData read FSourceViewData;
  public
    constructor Create(AItem: TdxTileControlItem); reintroduce; virtual;
    destructor Destroy; override;
    procedure RefreshAnimatedObject; override;

    property Item: TdxTileControlItem read FItem;
  end;

  { TdxTileControlGalleryCellAnimation }

  TdxTileControlGalleryCellAnimation = class(TdxAnimationTransition)
  private
    FAssistImage: TcxBitmap;
    FBackgroundImage: TcxBitmap;
    FCell: TdxTileControlItemGalleryCell;
    FCurrentImage: TdxSmartImage;
    FItem: TdxTileControlItem;
    FNextImage: TdxSmartImage;
  public
    constructor Create(AItem: TdxTileControlItem; ACell: TdxTileControlItemGalleryCell;
      const ABackgroundImage: TcxBitmap); reintroduce; virtual;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; const ADestRect: TRect);
    procedure RefreshAnimatedObject; override;

    property Cell: TdxTileControlItemGalleryCell read FCell;
    property Item: TdxTileControlItem read FItem;
  end;

  { TdxTileControlDragAndDropAnimation }

  TdxTileControlDragAndDropAnimation = class(TdxAnimationTransition)
  private
    FChanges: TdxTileControlDragAndDropChanges;
    FDragImageOrigin: TPoint;
    function GetTileControl: TdxCustomTileControl;
  protected
    function AnimationLength: Integer;
    procedure DoAnimate; override;
  public
    constructor Create(AChanges: TdxTileControlDragAndDropChanges); reintroduce;

    property Changes: TdxTileControlDragAndDropChanges read FChanges;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlHottrackItemAnimation }

  TdxTileControlHottrackItemAnimation = class(TdxAnimationTransition)
  private
    FItem: TdxTileControlItem;
    FShowFrame: Boolean;
  protected
    procedure DoAnimate; override;
    function IsCompatible(AAnimation: TdxAnimationTransition): Boolean; override;
  public
    constructor Create(AItem: TdxTileControlItem; AShowFrame: Boolean = True); reintroduce;
    destructor Destroy; override;

    property Item: TdxTileControlItem read FItem;
    property ShowFrame: Boolean read FShowFrame;
  end;

  { TdxTileControlActionBarsAnimation }

  TdxTileControlActionBarsAnimationMode = (tcabamShow, tcabamHide);

  TdxTileControlActionBarsAnimation = class(TdxAnimationTransition)
  private
    FTileControl: TdxCustomTileControl;
    FMode: TdxTileControlActionBarsAnimationMode;
    FPrevPosition: Integer;
    function GetActionBarTop: TdxTileControlCustomActionBar;
    function GetActionBarBottom: TdxTileControlCustomActionBar;
  protected
    function CalculateActionBarBottomBounds: TRect; virtual;
    function CalculateActionBarTopBounds: TRect; virtual;
    procedure DoAnimate; override;
    function IsCompatible(Animation: TdxAnimationTransition): Boolean; override;
  public
    constructor Create(ATileControl: TdxCustomTileControl; AMode: TdxTileControlActionBarsAnimationMode); reintroduce; virtual;
    //
    property ActionBarBottom: TdxTileControlCustomActionBar read GetActionBarBottom;
    property ActionBarTop: TdxTileControlCustomActionBar read GetActionBarTop;
    property Mode: TdxTileControlActionBarsAnimationMode read FMode;
    property PrevPosition: Integer read FPrevPosition;
    property TileControl: TdxCustomTileControl read FTileControl;
  end;

  { TdxTileControlContentAnimation }

  TdxTileControlContentAnimation  = class(TdxAnimationTransition)
  private
    FLengthX: Integer;
    FLengthY: Integer;
    FPrevPositionX: Integer;
    FPrevPositionY: Integer;
    FTileControl: TdxCustomTileControl;
    function GetPositionX: Integer;
    function GetPositionY: Integer;
    function GetViewInfo: TdxTileControlViewInfo;
  protected
    procedure DoAnimate; override;
    procedure DoMoveContent(const DX, DY: Integer); virtual;

    property LengthX: Integer read FLengthX;
    property LengthY: Integer read FLengthY;
    property PrevPositionX: Integer read FPrevPositionX write FPrevPositionX;
    property PrevPositionY: Integer read FPrevPositionY write FPrevPositionY;
  public
    constructor Create(ATileControl: TdxCustomTileControl;
      ATime: Cardinal; const ALengthX, ALengthY: Integer); reintroduce; virtual;

    property TileControl: TdxCustomTileControl read FTileControl;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  end;

  { TdxTileControlRubberAnimation }

  TdxTileControlRubberAnimation  = class(TdxTileControlContentAnimation)
  private
    FHandScrollObject: TdxTileControlHandScroll;
    function GetController: TdxTileControlController;
  protected
    procedure DoMoveContent(const DX, DY: Integer); override;
  public
    constructor Create(ATileControl: TdxCustomTileControl;
      AHandScrollObject: TdxTileControlHandScroll); reintroduce; virtual;
    destructor Destroy; override;

    property HandScrollObject: TdxTileControlHandScroll read FHandScrollObject;
    property Controller: TdxTileControlController read GetController;
  end;

  { TdxTileControlDragAndDropChanges }

  TdxTileControlDragAndDropChangesCellInfo = class
  private
    function GetGroup: TdxTileControlGroup;
    function GetTileControl: TdxCustomTileControl;
    function GetViewInfo: TdxTileControlViewInfo;
  protected
    AreaSizeX, AreaSizeY: Integer;
    BeforeUpdate: Boolean;
    BoundsBefore, BoundsAfter: TRect;
    Cell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo;
    ClipRgn: TcxRegion;
    ColumnBefore, ColumnAfter: Integer;
    GroupCaption: TdxTileControlGroupCaption;
    GroupOriginBefore, GroupOriginAfter: TPoint;
    IsSimpleMoving: Boolean;
    Item: TdxTileControlItem;
    RowBefore, RowAfter: Integer;
    procedure CalculateComplexDrawingAreaBounds(var ADest, ASource: TRect; const AOffsetX, AOffsetY: Integer);
    procedure CalculateSimpleDrawingAreaBounds(var ADest: TRect; const AOffsetX, AOffsetY: Integer);
    function CompareTileControlGroupCaptions(AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
    function CompareTileControlItems(AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
    function IsEqualGroupCaptions(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
    function IsEqualItems(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;

    property Group: TdxTileControlGroup read GetGroup;
  public
    constructor Create(ACell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo; ABeforeUpdate: Boolean);
    destructor Destroy; override;
    function Compare(AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
    procedure Draw(ACanvas: TcxCanvas; const AProgress: Integer);
    procedure InvalidateAnimatedArea;
    function IsEqual(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
    function Merge(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;

    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  end;

  TdxTileControlDragAndDropChanges = class
  private
    FDragFinished: Boolean;
    FDragObject: TdxTileControlCustomCellViewInfo;
    FDragImageBounds: TRect;
    FDragImagePosition: TPoint;
    FItemsInfo: TcxObjectList;
    FLock: Integer;
    FProgress: Integer;
    FViewInfo: TdxTileControlViewInfo;
    function GetCount: Integer;
    function GetDragAndDropObject: TdxTileControlDragDropCustomObject;
    function GetDragObjectFinished: Boolean;
    function GetItem(AIndex: Integer): TdxTileControlDragAndDropChangesCellInfo;
    function GetTileControl: TdxCustomTileControl;
  protected
    procedure AnimateDragFinished(const AProgress: Integer); virtual;
    procedure DrawItems(ACanvas: TcxCanvas); virtual;
    procedure PopulateItemsInfo(ABeforeUpdate: Boolean);
    procedure SetAnimationProgress(const AProgress: Integer);

    property DragFinished: Boolean read FDragFinished write FDragFinished;
    property ItemsInfo: TcxObjectList read FItemsInfo;
    property Progress: Integer read FProgress;
  public
    constructor Create(AViewInfo: TdxTileControlViewInfo);
    destructor Destroy; override;

    procedure AfterUpdate; virtual;
    procedure BeforeUpdate; virtual;
    function IndexOf(ACell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo): Integer;

    property Count: Integer read GetCount;
    property DragAndDropObject: TdxTileControlDragDropCustomObject read GetDragAndDropObject;
    property DragObject: TdxTileControlCustomCellViewInfo read FDragObject;
    property DragObjectFinished: Boolean read GetDragObjectFinished;
    property Items[Index: Integer]: TdxTileControlDragAndDropChangesCellInfo read GetItem;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlViewInfo read FViewInfo;
  end;

  { TdxTileControlAutoScrollingObject }

  TdxTileControlAutoScrollingObject = class(TcxCustomAutoScrollingObject)
  protected
    function GetControl: TcxControl; override;
    function GetHasScrollBar: Boolean; override;
    procedure GetScrollBarParams(var AMin, AMax, APos: Integer); override;
    function GetTileControl: TdxCustomTileControl;
    procedure Scroll(AKind: TScrollBarKind; ACode: TScrollCode; var APosition: Integer); override;

    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlHandScroll }

  TdxTileControlHandScroll = class(TcxDragAndDropObject)
  private
    FFinishAnimationPosX: Integer;
    FFinishAnimationPosY: Integer;
    FStartAnimationPosX: Integer;
    FStartAnimationPosY: Integer;

    procedure DoScrollHorizontally;
    procedure DoScrollVertically;
    function GetSlowdownScrollFactor(APos, AScrollPageSize, AContentSize, ANewDelta: Integer): Integer;
    function GetTileControl: TdxCustomTileControl;
    function GetViewInfo: TdxTileControlViewInfo;
  protected
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;

    property FinishAnimationPosX: Integer read FFinishAnimationPosX;
    property FinishAnimationPosY: Integer read FFinishAnimationPosY;
    property StartAnimationPosX: Integer read FStartAnimationPosX;
    property StartAnimationPosY: Integer read FStartAnimationPosY;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  end;

  { TdxTileControlDragDropCustomObject }

  TdxTileControlDragDropState = (tcddsPulling, tcddsMoving);

  TdxTileControlDragDropCustomObject = class(TcxDragAndDropObject)
  private
    FDragCell: TdxTileControlCustomCellViewInfo;
    FDragCellIsHided: Boolean;
    FHotSpot: TPoint;
    FHotSpotCalculated: Boolean;
    FIsTouching: Boolean;
    FPrevAccepted: Boolean;
    FPullProgress: Integer;
    FState: TdxTileControlDragDropState;
    procedure CalculateHotSpot;
    function CreateAutoScrollObject(Kind: TScrollBarKind; const ARect: TRect;
      ACode: TScrollCode): TdxTileControlAutoScrollingObject;
    procedure CreateAutoScrollObjects;
    procedure DestroyAutoScrollObjects;
    function GetController: TdxTileControlController;
    function GetDragBounds: TRect;
    function GetDragCellAsGroup: TdxTileControlGroup;
    function GetDragCellAsItem: TdxTileControlItem;
    function GetDragCellIsItem: Boolean;
    function GetDragDropChanges: TdxTileControlDragAndDropChanges;
    function GetHitTest: TdxTileControlHitTest;
    function GetIsPullDownGesture: Boolean;
    function GetMaxPullSize: Integer;
    function GetTileControl: TdxCustomTileControl;
    function GetViewInfo: TdxTileControlViewInfo;
    procedure SetPullProgress(AValue: Integer);
    procedure HideOriginalDragCell;
    function IsRectInControl(const ARect: TRect): Boolean;
    function MouseClientToScreen(const P: TPoint): TPoint;
  protected
    DragImage: TcxDragImage;
    ScrollControllers: array [TdxTileControlAutoScrollSide] of TdxTileControlAutoScrollingObject;
    procedure AfterDragAndDrop(Accepted: Boolean); override;
    procedure BeginDragAndDrop; override;
    procedure BeginDragAndDropGroup; virtual;
    procedure BeginDragAndDropItem; virtual;
    procedure BeginDragMoving; virtual;
    function CheckAutoScrolling: Boolean; virtual;
    procedure DoAnimateContent(const AStart, AFinish: TPoint);
    procedure DoDragEnd; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure DragAndDropGroup(var Accepted: Boolean); virtual;
    procedure DragAndDropItem(var Accepted: Boolean); virtual;
    procedure DragMoving(const P: TPoint; var Accepted: Boolean); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    procedure EndDragAndDropGroup(Accepted: Boolean); virtual;
    procedure EndDragAndDropItem(Accepted: Boolean; var ANewGroup: TdxTileControlGroup); virtual;
    procedure EndDragMoving(Accepted: Boolean); virtual;
    procedure InitializeDragImage; virtual;
    function GetActualDragBounds: TRect;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    procedure PullDown(Accepted: Boolean); virtual;
    procedure Pulling(const P: TPoint); virtual;
    procedure ShowDragImage(APos: TPoint); virtual; // virtual only for debug!!!
    procedure StopAutoScrollingTimers;
    function UseRightToLeftAlignment: Boolean;

    property Controller: TdxTileControlController read GetController;
    property DragBounds: TRect read GetDragBounds;
    property DragCell: TdxTileControlCustomCellViewInfo read FDragCell;
    property DragCellAsGroup: TdxTileControlGroup read GetDragCellAsGroup;
    property DragCellAsItem: TdxTileControlItem read GetDragCellAsItem;
    property DragCellIsItem: Boolean read GetDragCellIsItem;
    property DragDropChanges: TdxTileControlDragAndDropChanges read GetDragDropChanges;
    property HitTest: TdxTileControlHitTest read GetHitTest;
    property IsPullDownGesture: Boolean read GetIsPullDownGesture;
    property IsTouching: Boolean read FIsTouching;
    property MaxPullSize: Integer read GetMaxPullSize;
    property PullProgress: Integer read FPullProgress write SetPullProgress;
    property State: TdxTileControlDragDropState read FState;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;
  end;

  { TdxTileControlDragGroupInfo }

  TdxTileControlDragGroupInfo = class
  private
    FBeginIndex: Integer;
    FCurrentIndex: Integer;
    FGroup: TdxTileControlGroup;
    function GetTileControl: TdxCustomTileControl;
  protected
    procedure Initialize(AGroup: TdxTileControlGroup);
    procedure SetCurrentIndex(AIndex: Integer);

    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    property BeginIndex: Integer read FBeginIndex;
    property CurrentIndex: Integer read FCurrentIndex;
    property Group: TdxTileControlGroup read FGroup;
  end;

  { TdxTileControlDragDropGroup }

  TdxTileControlDragDropGroup = class(TdxTileControlDragDropCustomObject)
  private
    FDragGroup: TdxTileControlGroup;
    FPrevIndex: Integer;
    function GetDragGroupInfo: TdxTileControlDragGroupInfo;
    function GetPotentialIndexForDraggedGroup: Integer;
    function GetPotentialIndexForNewlyInsertedGroup: Integer;
    function GetPotentialIndexForMovedGroup: Integer;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetGroups: TdxTileControlGroupCollection;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
    property Groups: TdxTileControlGroupCollection read GetGroups;
  protected
    procedure BeginDragAndDropGroup; override;
    procedure DoDragEnd; override;
    procedure DragAndDropGroup(var Accepted: Boolean); override;
    procedure EndDragAndDropGroup(Accepted: Boolean); override;
    procedure InitializeDragImage; override;

    property DragGroupInfo: TdxTileControlDragGroupInfo read GetDragGroupInfo;
  public
    property DragGroup: TdxTileControlGroup read FDragGroup;
  end;

  { TdxTileControlCheckedItems }

  TdxTileControlCheckedItems = class(TList)
  private
    function GetItem(Index: Integer): TdxTileControlItem;
  protected
    procedure SortByIndexInGroupBeforeDrag;
  public
    property Items[Index: Integer]: TdxTileControlItem read GetItem; default;
  end;

  { TdxTileControlDragItemInfo }

  TdxTileControlDragItemInfo = class
  private
    FCheckedItems: TdxTileControlCheckedItems;
    FItem: TdxTileControlItem;
    FGroup: TdxTileControlGroup;
    function GetSourceGroup: TdxTileControlGroup;
    function GetTileControl: TdxCustomTileControl;
  protected
    procedure CreateCheckedItems;
    procedure DestroyCheckedItems;
    procedure Initialize(ADragItem: TdxTileControlItem);
    procedure SetGroup(AGroup: TdxTileControlGroup);

    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    destructor Destroy; override;

    property CheckedItems: TdxTileControlCheckedItems read FCheckedItems;
    property Item: TdxTileControlItem read FItem;
    property Group: TdxTileControlGroup read FGroup;
    property SourceGroup: TdxTileControlGroup read GetSourceGroup;
  end;

  { TdxTileControlDragItemPlace }

  TdxTileControlDragItemPlace = class
  private
    FGroup: TdxTileControlGroupViewInfo;
    FPosition: TdxLayoutItemPosition;
    FVirtualGroup: TdxTileControlVirtualGroupViewInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TdxTileControlDragItemPlace);
    function GetGroup: TdxTileControlGroup;
    function IsEqual(APlace: TdxTileControlDragItemPlace): Boolean;

    property Group: TdxTileControlGroupViewInfo read FGroup write FGroup;
    property Position: TdxLayoutItemPosition read FPosition write FPosition;
    property VirtualGroup: TdxTileControlVirtualGroupViewInfo read FVirtualGroup write FVirtualGroup;
  end;

  { TdxTileControlDragDropItem }

  TdxTileControlDragDropItem = class(TdxTileControlDragDropCustomObject)
  private
    FCountInfoFont: TFont;
    FDragOverIsAccepted: Boolean;
    FDragItem: TdxTileControlItem;
    FWasTryDecreaseBeginGroup: Boolean;
    FGroupItemsPositions: TcxObjectList;
    FNextPosition: TdxLayoutItemPosition;
    FPrevPlace, FPlace: TdxTileControlDragItemPlace;
    FTemporaryGroup: TdxTileControlGroup;
    procedure CalculateDragAndDropItemInGroup;
    procedure CalculateDragAndDropItemInAnotherGroup;
    procedure CalculateDragAndDropItemInPrevGroup;
    procedure CalculateDragAndDropItemInVirtualGroup(var AAccepted: Boolean);
    function CanDecreaseBeginGroup: Boolean;
    procedure CheckGroupDragItem;
    procedure CheckPlaceDragItem(var AAccepted: Boolean);
    procedure CheckPositionDragItem;
    procedure CheckPositionDragPoint(ADragPoint: TPoint);
    function CreateNewGroupAndMoveDragItemTo(AVirtualGroup: TdxTileControlVirtualGroupViewInfo): TdxTileControlGroup;
    procedure DoCollapseBeginGroupWhenItemIsLast;
    procedure DoItemDragOver(var AAccepted: Boolean);
    procedure FindGroupOccupiedCenterOfDragItem;
    procedure FindVirtualGroupOccupiedCenterOfDragItem;
    procedure FindSomethingOccupiedPartiallyOfDragItem;
    function GetBeginGroup: TdxTileControlGroup;
    function GetBeginIndex: Integer;
    function GetCheckedItems: TdxTileControlCheckedItems;
    function GetDragItemInfo: TdxTileControlDragItemInfo;
    function GetDragPoint: TPoint;
    function GetGroupCrossedOnTheRight: TdxTileControlGroup;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetMaximizedAreaBoundsChanging(AGroup: TdxTileControlGroup): TSize;
    function GetNewIndexOfDragItem(AGroup: TdxTileControlGroup): Integer;
    function GetSecondaryDragPoint(ABounds: TRect): TPoint;
    function GetSpecialPointX: Integer;
    function GetSpecialPointY: Integer;
    function GetStayInPlaceVirtualGroup: Boolean;
    function GetVirtualGroups: TcxObjectList;
    procedure InitializePrevPlace;
    function IsDragBoundsCrossesOnTheRight(AGroup: TdxTileControlGroup): Boolean;
    function IsDragItemsGroupHasMaxExpanded: Boolean;
    function IsDragItemsGroupStayMoreRight: Boolean;
    function IsExitFromVirtualGroup: Boolean;
    function IsPossibleIncreaseGroup(AGroup: TdxTileControlGroup; var AAccepted: Boolean): Boolean;
    procedure MoveDragItemFromControl;
    procedure MoveDragItemToGroup(AGroup: TdxTileControlGroup; AIndexInGroup: Integer);
    procedure RecalculateGroup(AGroup: TdxTileControlGroup);
    procedure RemoveCheckedItemsFromGroups;
    procedure RestoreCheckedItemsSourceLayout;
    procedure RestoreDragItemSourcePosition;
    procedure RestoreSourcePositions;
    procedure RestoreGroupItemsPositions(AGroup: TdxTileControlGroup);
    procedure SetNoVisibleAllVirtualGroups;
    procedure SwitchOff(AVirtualGroup: TdxTileControlVirtualGroupViewInfo);
    procedure StoreCheckedItemsSourceLayout;
    procedure StoreGroupItemsPositions(AGroup: TdxTileControlGroup);
    procedure TryDecreaseBeginGroup;
  protected
    procedure BeginDragAndDropItem; override;
    procedure BeginDragMoving; override;
    procedure DoDragEnd; override;
    procedure DragAndDropItem(var Accepted: Boolean); override;
    procedure EndDragAndDropItem(Accepted: Boolean; var ANewGroup: TdxTileControlGroup); override;
    procedure InitializeDragImage; override;
    procedure PullDown(Accepted: Boolean); override;

    property BeginGroup: TdxTileControlGroup read GetBeginGroup;
    property BeginIndex: Integer read GetBeginIndex;
    property CheckedItems: TdxTileControlCheckedItems read GetCheckedItems;
    property DragItemInfo: TdxTileControlDragItemInfo read GetDragItemInfo;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
    property Place: TdxTileControlDragItemPlace read FPlace;
    property VirtualGroups: TcxObjectList read GetVirtualGroups;
    property DragItem: TdxTileControlItem read FDragItem;
  end;

  { TdxTileControlGroupCaptionInplaceEdit }

  TdxTileControlGroupCaptionInplaceEdit = class(TcxButtonEdit)
  private
    FIniBoundsPos: TPoint;
    FIniCaptionPos: TPoint;
    function GetCaption: TdxTileControlGroupCaption;
    function GetClearButton: TcxEditButton;
    function GetGroup: TdxTileControlGroup;
    function GetTextHint: string;
    function GetTileControl: TdxCustomTileControl;
    procedure SetupBounds;
    procedure SetupFont;
    procedure SetupClearButton;
    procedure SetupProperties;
    procedure SetupStyles;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure ClearEditor(Sender: TObject; AButtonIndex: Integer);
    procedure ChangeScaleEx(M: Integer; D: Integer; IsDPIChanged: Boolean); override;
    procedure CheckEmptyOnExit(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawClearButtonGlyph; virtual;
    function GetCaptionTextBounds: TRect;
    function IsUseSkins: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    property Caption: TdxTileControlGroupCaption read GetCaption;
    property ClearButton: TcxEditButton read GetClearButton;
    property Group: TdxTileControlGroup read GetGroup;
    property IniBoundsPos: TPoint read FIniBoundsPos;
    property IniCaptionPos: TPoint read FIniCaptionPos;
    property TextHint: string read GetTextHint;
    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure SynchronizePos;
  end;

  { TdxThreadScalerTaskItem }

  TdxThreadScalerTaskItem = class
  private
    FImage: TdxSmartImage;
    FCallBackProc: TNotifyEvent;
    FImageItem: TcxImageCollectionItem;
    FOutputSize: TSize;
    procedure LoadGraphicToImage(AGraphic: TGraphic; AImage: TdxSmartImage);
  public
    constructor Create(AImageItem: TcxImageCollectionItem; AOutputSize: TSize; ACallBackProc: TNotifyEvent);
    destructor Destroy; override;
    procedure ClearCallBack;
    procedure Scale;

    property CallBackProc: TNotifyEvent read FCallBackProc;
    property Image: TdxSmartImage read FImage;
    property ImageItem: TcxImageCollectionItem read FImageItem;
  end;

  { TdxThreadScaler }

  TdxThreadScaler  = class(TcxThread)
  private
    FTasks: TThreadList;
    FCurrentTask: TdxThreadScalerTaskItem;
    procedure ClearTasks;
  protected
    procedure CancelCurrentTask;
    procedure DoCallBack;
    procedure Execute; override;
    procedure ProcessTask;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(AImageItem: TcxImageCollectionItem; AOutputSize: TSize; ACallBackProc: TNotifyEvent);
    procedure CancelTasks(ACallBackProc: TNotifyEvent); overload;
    procedure CancelTasks(AImageItem: TcxImageCollectionItem); overload;
    function HasTaskForProc(ACallBackProc: TNotifyEvent): Boolean;
    procedure Stop;
  end;

  { TdxTileControlController }

  TdxTileControlDesignTimeMenuItemCheckedCondition = function(AItem: TdxTileControlItem): Boolean of object;

  TdxTileControlController = class(TcxIUnknownObject, IdxAnimationListener, IcxMouseTrackingCaller, IcxMouseTrackingCaller2)
  private
    FAnimationItems: TList;
    FAnimations: TList;
    FCanItemDrawing: Boolean;
    FContextMenuHandled: Boolean;
    FDragCell: TObject;
    FDesignHelper: TdxTileControlCustomDesignHelper;
    FDesignPopupMenu: TPopupMenu;
    FDesignPopupMenuCaller: TcxComponentCollectionItem;
    FDesignWhatObjectsSelected: TdxTileControlDesignTimeWhatSelected;
    FFocusedItem: TdxTileControlItem;
    FHasRubberAnimation: Boolean;
    FHintController: TdxTileControlHintController;
    FHottrackedItem: TdxTileControlItem;
    FItemsContentAnimationLockCount: Integer;
    FMouseDownItem: TdxTileControlItem;
    FMouseDownWasHandled: Boolean;
    FNavigator: TdxTileControlItemNavigator;
    FPressedScrollButton: TdxTileControlScrollButtonViewInfo;
    FRestoreItemsBoundsOnMouseUp: Boolean;
    FStartDragPos: TPoint;
    FTileControl: TdxCustomTileControl;
    function EnabledGroupMoving: Boolean;
    function EnabledItemMoving: Boolean;
    function GetCanGroupMoving: Boolean;
    function GetCanItemMoving: Boolean;
    function GetCheckedItems: TdxTileControlCheckedItems;
    function GetHasAnimation_: Boolean;
    function GetHitTest: TdxTileControlHitTest;
    function GetItemCheckMode: TdxTileControlItemCheckMode;
    function GetViewInfo: TdxTileControlViewInfo;
    procedure RestoreItemBounds(AItem: TdxTileControlItem; const AData: Pointer);
    procedure SetHottrackedItem(AValue: TdxTileControlItem);
    procedure SetPressedScrollButton(AValue: TdxTileControlScrollButtonViewInfo);
    //design PopupMenu
    function CheckCallerGroup: TdxTileControlGroup;
    function CreatePopupMenuItem(const ACaption: string; const ATag: Integer;
      const AGroupIndex: Byte; const AChecked, ARadioItem: Boolean; AOnClick: TNotifyEvent): TMenuItem;
    function CheckDesignPopupMenu(AShift: TShiftState): Boolean;
    procedure DeleteSelectedObjects(Sender: TObject);
    procedure DeleteTileControlGroups(const ASelectNextObjectAfterDelete: Boolean);
    procedure DeleteTileControlItems(const ASelectNextObjectAfterDelete: Boolean);
    procedure DeleteTileControlSelectedGroups;
    procedure DeleteTileControlSelectedItems;
    function GetDesignTimeWhatSelected: TdxTileControlDesignTimeWhatSelected;
    function GetNextObject(ADeletedGroupIndex: Integer): TPersistent; overload;
    function GetNextObject(ADeletedItemIndex: Integer; ADeletedItemGroup: TdxTileControlGroup): TPersistent; overload;
  protected
    procedure AddAnimation(Animation: TdxAnimationTransition);
    procedure AnimateItemContent(AItem: TdxTileControlItem);
    // IdxAnimationListener
    procedure AfterAnimation(Sender: TdxAnimationController);
    procedure BeforeAnimation(Sender: TdxAnimationController);
    procedure DestroyAnimation(Animation: TdxAnimationTransition);
    //
    function CanShowActionBarsOnRightClick: Boolean; virtual;
    function CanToggleItemCheckOnRightClick: Boolean; virtual;
    procedure ChangePressedScrollButtonState;
    procedure CheckActionBarsState(ACheckingItem: TdxTileControlItem);
    procedure CheckHottrackItemAfterMouseUp;
    procedure FrameDestroyed(AFrame: TdxTileControlItemFrame);
    procedure HideGroupCaptionEditors;
    procedure HideScrollButtons;
    function HitAtGroup: Boolean;
    function HitAtItem: Boolean;
    function HitAtItemOrGroup: Boolean;
    procedure ImmediateAnimation(Animation: TdxAnimationTransition);
    function IsDesignPopupMenuPresent: Boolean;
    function IsDetectedDragItem: Boolean;
    function IsDetectedDragGroup: Boolean;
    function IsEnableDrawDragItemPlace: Boolean;
    function IsHandScrolling(ACheckedKind: TScrollBarKind): Boolean;
    procedure ItemChecked(AItem: TdxTileControlItem);
    procedure ItemDestroyed(AItem: TdxTileControlItem);
    procedure RefreshItems;
    procedure RestoreItemsBounds;
    procedure ShowGroupCaptionEditors;
    procedure SetFocusedItem(AValue: TdxTileControlItem); virtual;
    procedure TerminateAnimations(ATerminateContentAnimations: Boolean);
    procedure UncheckAllItems;
    // design selection
    function CanCreateSelectionHelper: Boolean;
    procedure CheckCreateDesignHelper;
    function CreateDesignHelper: TdxTileControlCustomDesignHelper; virtual;
    procedure DestroyDesignHelper;
    function IsDesignHelperClassInitialized: Boolean; virtual;
    function IsObjectSelected(AObject: TPersistent): Boolean; virtual;
    procedure SelectObject(AObject: TPersistent; AShift: TShiftState); virtual;
    procedure UnselectObject(AObject: TPersistent); virtual;
    // design popup menu
    procedure AddDesignPopupMenuItem(const ACaption: string; const ATag: Integer; const AGroupIndex: Byte;
      const AChecked, ARadioItem: Boolean; AOnClick: TNotifyEvent);
    function CreateRegularItem(AGroup: TdxTileControlGroup): TdxTileControlItem; virtual;
    procedure CreateTileControlItem(Sender: TObject);
    function GetIsExtraLargeItem(AItem: TdxTileControlItem): Boolean;
    function GetIsLargeItem(AItem: TdxTileControlItem): Boolean; virtual;
    function GetIsRegularItem(AItem: TdxTileControlItem): Boolean; virtual;
    function GetIsSmallItem(AItem: TdxTileControlItem): Boolean;
    function IsDesignerMenuItemChecked(const AItemSizeIndex: Integer; AItem: TdxTileControlItem): Boolean; overload; virtual;
    function IsDesignerMenuItemChecked(AItem: TdxTileControlItem;
      AFunc: TdxTileControlDesignTimeMenuItemCheckedCondition): Boolean; overload;
    procedure PopulateDesignPopupMenu;
    procedure PopulateOneGroupDesignPopupMenu; virtual;
    procedure PopulateOneItemDesignPopupMenu; virtual;
    procedure ProcessDesignPopupMenu(ACaller: TcxComponentCollectionItem; AShift: TShiftState; APopupPoint: TPoint);
    procedure SetDesignItemSize(AItem: TdxTileControlItem; AMenuItemTag: Longint); virtual;
    procedure SwitchItemsProperty(Sender: TObject);

    // mouse
    procedure InternalMouseLeftDown(Shift: TShiftState; X, Y: Integer); virtual;
    procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual; // IcxMouseTrackingCaller
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    //
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    // action bars
    procedure HideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
    procedure ShowActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
    procedure ToggleActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
    //
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    //
    property AnimationItems: TList read FAnimationItems;
    property Animations: TList read FAnimations;
    property CanGroupMoving: Boolean read GetCanGroupMoving;
    property CanItemMoving: Boolean read GetCanItemMoving;
    property CanItemDrawing: Boolean read FCanItemDrawing write FCanItemDrawing;
    property CheckedItems: TdxTileControlCheckedItems read GetCheckedItems;
    property ContextMenuHandled: Boolean read FContextMenuHandled;
    property DesignHelper: TdxTileControlCustomDesignHelper read FDesignHelper;
    property DesignPopupMenuCaller: TcxComponentCollectionItem read FDesignPopupMenuCaller;
    property DragCell: TObject read FDragCell write FDragCell;
    property HasAnimation_: Boolean read GetHasAnimation_;
    property HasRubberAnimation: Boolean read FHasRubberAnimation;
    property HintController: TdxTileControlHintController read FHintController;
    property HitTest: TdxTileControlHitTest read GetHitTest;
    property ItemCheckMode: TdxTileControlItemCheckMode read GetItemCheckMode;
    property ItemsContentAnimationLockCount: Integer read FItemsContentAnimationLockCount write FItemsContentAnimationLockCount;
    property MouseDownWasHandled: Boolean read FMouseDownWasHandled write FMouseDownWasHandled;
    property Navigator: TdxTileControlItemNavigator read FNavigator;
    property RestoreItemsBoundsOnMouseUp: Boolean read FRestoreItemsBoundsOnMouseUp;
    property StartDragPos: TPoint read FStartDragPos write FStartDragPos;
    property TileControl: TdxCustomTileControl read FTileControl;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TdxCustomTileControl); virtual;
    destructor Destroy; override;
    procedure PrepareToDragAndDrop;
    procedure ProcessContextMenuMessage(var Message: TWMContextMenu); virtual;
    procedure StartAnimations;
    procedure StartItemContentAnimation;
    procedure StopAnimations;
    procedure StopItemContentAnimation;

    property FocusedItem: TdxTileControlItem read FFocusedItem write SetFocusedItem;
    property HottrackedItem: TdxTileControlItem read FHottrackedItem write SetHottrackedItem;
    property PressedScrollButton: TdxTileControlScrollButtonViewInfo read FPressedScrollButton write SetPressedScrollButton;
  end;

  { TdxTileControlHitTest }

  TdxTileControlHitTest = class
  private
    FHitObject: TObject;
    FHitPoint: TPoint;
    FOwner: TdxCustomTileControl;
    function GetController: TdxTileControlController;
    function GetGroup: TdxTileControlGroup;
    function GetItem: TdxTileControlItem;
    function GetPosValue(const AIndex: Integer): Integer;
    function GetScrollButton: TdxTileControlScrollButtonViewInfo;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
    procedure SetHitObject(AValue: TObject);
    procedure SetHitPoint(const AValue: TPoint);
    procedure SetPosValue(const AIndex, AValue: Integer);
  protected
    Flags: Int64;
    function GetActiveViewInfo: TdxTileControlCustomViewInfo; virtual;
    function GetBitState(AIndex: Integer): Boolean;

    property ActiveViewInfo: TdxTileControlCustomViewInfo read GetActiveViewInfo;
    property BitState[AIndex: Integer]: Boolean read GetBitState write SetBitState;
    property Controller: TdxTileControlController read GetController;
    property ScrollButton: TdxTileControlScrollButtonViewInfo read GetScrollButton;
  public
    constructor Create(AOwner: TdxCustomTileControl);
    procedure Calculate(const X, Y: Integer);
    procedure Clear;
    procedure Recalculate; virtual;

    property Group: TdxTileControlGroup read GetGroup;
    property HitAtActionBarBottom: Boolean index tchtActionBarBottom read GetBitState;
    property HitAtActionBarTop: Boolean index tchtActionBarTop read GetBitState;
    property HitAtActionButton: Boolean index tchtActionButton read GetBitState;
    property HitAtBackButton: Boolean index tchtBackButton read GetBitState;
    property HitAtGroup: Boolean index tchtGroup read GetBitState;
    property HitAtGroupCaption: Boolean index tchtGroupCaption read GetBitState;
    property HitAtItem: Boolean index tchtItem read GetBitState;
    property HitAtPageTab: Boolean index tchtPageTab read GetBitState;
    property HitAtScrollButton: Boolean index tchtScrollButton read GetBitState;
    property HitAtScrollButtonArea: Boolean index tchtScrollButtonArea read GetBitState;
    property HitAtTabScrollButton: Boolean index tchtTabScrollButton read GetBitState;
    property HitAtTitle: Boolean index tchtTitle read GetBitState;
    property HitPoint: TPoint read FHitPoint write SetHitPoint;
    property HitObject: TObject read FHitObject write SetHitObject;
    property HitX: Integer index 0 read GetPosValue write SetPosValue;
    property HitY: Integer index 1 read GetPosValue write SetPosValue;
    property Item: TdxTileControlItem read GetItem;
    property TileControl: TdxCustomTileControl read FOwner;
  end;

  { TdxTileControlItemNavigator }

  TdxTileControlItemNavigator = class
  private
    FAssistPosition: TdxLayoutItemPosition;
    FController: TdxTileControlController;
    FCurrentColumn: Integer;
    FCurrentRow: Integer;
    FFocusItemOnCycle: Boolean;
    FIsInitializingCurrentPositionDisabled: Boolean;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetGroups: TdxTileControlGroupCollection;
    function CheckGroupOnExistenceOfItemsStartingFromRow(AGroup: TdxTileControlGroup; const ARow: Integer): Boolean;
    function GetCurrentItem: TdxTileControlItem;
    function GetCurrentItemLastColumn: Integer;
    function GetCurrentItemLastRow: Integer;
    function GetItemByBackSearchOnColumns(AGroup: TdxTileControlGroup; const AStartColumn, AEndColumn: Integer): TdxTileControlItem;
    function GetItemByBackSearchOnRows(AGroup: TdxTileControlGroup; const AStartRow, AEndRow: Integer): TdxTileControlItem;
    function GetItemByForwardSearchOnColumns(AGroup: TdxTileControlGroup; const AStartColumn, AEndColumn: Integer): TdxTileControlItem;
    function GetItemByForwardSearchOnRows(AGroup: TdxTileControlGroup; const AStartRow, AEndRow: Integer): TdxTileControlItem;
    function GetNextItemInColumnAboveTheCurrentWhenHorizontalLayout(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextItemInColumnAboveTheCurrentWhenVerticalLayout(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextItemInColumnBelowTheCurrent(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextItemInNextRowWhenHorizontalLayout(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextItemInNextRowWhenVerticalLayout(const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextItemInRowToRightHand(AStartColumn, ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInColumnBelowTheCurrentWhenHorizontalLayout(const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInColumnBelowTheCurrentWhenVerticalLayout(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInColumnAboveTheCurrent(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInPrevRowWhenHorizontalLayout(const ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInPrevRowWhenVerticalLayout(const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetPrevItemInRowToLeftHand(AStartColumn, ACurrentGroupIndex: Integer): TdxTileControlItem;
    function GetNextRow(const AIncrement: Integer; var AGroupIndex: Integer): Integer;
    function GetNextRowWhenHorizontalLayout(const AIncrement: Integer): Integer;
    function GetNextRowWhenVerticalLayout(const AIncrement: Integer; var AGroupIndex: Integer): Integer;
    procedure SetCurrentItem(AValue: TdxTileControlItem);
  protected
    function GetFirstItem: TdxTileControlItem; virtual;
    function GetFirstItemInRow: TdxTileControlItem; virtual;
    function GetLastItem: TdxTileControlItem; virtual;
    function GetLastItemInRow: TdxTileControlItem; virtual;
    function GetNextItemInColumn: TdxTileControlItem; virtual;
    function GetNextItemInRow: TdxTileControlItem; virtual;
    function GetPrevItemInColumn: TdxTileControlItem; virtual;
    function GetPrevItemInRow: TdxTileControlItem; virtual;
    procedure InitializeCurrentPosition;

    property Controller: TdxTileControlController read FController;
    property CurrentItem: TdxTileControlItem read GetCurrentItem write SetCurrentItem;
    property CurrentItemLastColumn: Integer read GetCurrentItemLastColumn;
    property CurrentItemLastRow: Integer read GetCurrentItemLastRow;
    property CurrentColumn: Integer read FCurrentColumn write FCurrentColumn;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
    property Groups: TdxTileControlGroupCollection read GetGroups;
  public
    constructor Create(AController: TdxTileControlController);
    destructor Destroy; override;
    function IsAcceptedKey(Key: Word): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure SaveCurrentNavigationItem;
  end;

  TdxTileItemInnerObjectAlignment = (oaDefault, oaTopLeft, oaTopCenter, oaTopRight, oaMiddleLeft,
    oaMiddleCenter, oaMiddleRight, oaBottomLeft, oaBottomCenter, oaBottomRight);
  TdxTileControlImageWithTextAlignment = (itaNone, itaLeft, itaTop, itaRight, itaBottom);

  { TdxTileControlItemPersistent }

  TdxTileControlItemPersistent = class(TcxOwnedPersistent)
  private
    function GetOwnerItem: TdxTileControlCustomItem;
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function IsChanged: Boolean; virtual;
    //
    property Owner: TdxTileControlCustomItem read GetOwnerItem;
  end;

  { TdxTileControlItemText }

  TdxTileItemTextAssignedValue = (avColor, avTextColor, avFont);
  TdxTileItemTextAssignedValues = set of TdxTileItemTextAssignedValue;

  TdxTileControlItemText = class(TdxTileControlItemPersistent)
  private
    FAlign: TdxTileItemInnerObjectAlignment;
    FAlignment: TAlignment;
    FAssignedValues: TdxTileItemTextAssignedValues;
    FColor: TColor;
    FFont: TFont;
    FIndentHorz: Integer;
    FIndentVert: Integer;
    FTextColor: TColor;
    FTransparent: Boolean;
    FValue: string;
    FWordWrap: Boolean;

    function IsFontStored: Boolean;
    procedure ResetFont;
    procedure SetAlign(AValue: TdxTileItemInnerObjectAlignment);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAssignedValues(AValue: TdxTileItemTextAssignedValues);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetIndentHorz(AValue: Integer);
    procedure SetIndentVert(AValue: Integer);
    procedure SetTextColor(AValue: TColor);
    procedure SetTransparent(AValue: Boolean);
    procedure SetValue(const AValue: string);
    procedure SetWordWrap(AValue: Boolean);
  protected
    FDefaultAlign: TdxTileItemInnerObjectAlignment;
    procedure CalculateBounds(var ABounds: TRect); virtual;
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure FontChanged(Sender: TObject);
    function GetActualAlign: TdxTileItemInnerObjectAlignment;
    function GetActualColor: TColor;
    function GetActualFont: TFont;
    function GetActualFontColor: TColor;
    function GetActualValue: string;
    function GetTextOutFlags: Integer;
    function HasValue: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Align: TdxTileItemInnerObjectAlignment read FAlign write SetAlign default oaDefault;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AssignedValues: TdxTileItemTextAssignedValues read FAssignedValues write SetAssignedValues;
    property Color: TColor read FColor write SetColor default clDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IndentHorz: Integer read FIndentHorz write SetIndentHorz default dxTileItemObjectDefaultIndent;
    property IndentVert: Integer read FIndentVert write SetIndentVert default dxTileItemObjectDefaultIndent;
    property Value: string read FValue write SetValue;
    property TextColor: TColor read FTextColor write SetTextColor default clDefault;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  { TdxTileControlItemCustomGlyph }

  TdxTileControlItemCustomGlyph = class(TdxTileControlItemPersistent)
  private
    FAlign: TdxTileItemInnerObjectAlignment;
    FAlignWithText: TdxTileControlImageWithTextAlignment;
    FImage: TdxSmartGlyph;
    FImageIndex: TcxImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FImagesNotifyComponent: TcxFreeNotificator;
    FIndentHorz: Integer;
    FIndentVert: Integer;
    FMode: TcxImageFitMode;
    procedure ImageChanged(Sender: TObject);
    procedure SetAlign(AValue: TdxTileItemInnerObjectAlignment);
    procedure SetAlignWithText(AValue: TdxTileControlImageWithTextAlignment);
    procedure SetImage(AValue: TdxSmartGlyph);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetIndentHorz(AValue: Integer);
    procedure SetIndentVert(AValue: Integer);
    procedure SetMode(AValue: TcxImageFitMode);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function GetActualAlign: TdxTileItemInnerObjectAlignment;
    function GetClientRect(const ABounds: TRect): TRect;
    function GetImages: TCustomImageList;
    procedure ImagesChanged(Sender: TObject);
    procedure ImagesFreeNotification(AComponent: TComponent);
    function IsChanged: Boolean; override;
    function IsImagesUse: Boolean;

    property Images: TCustomImageList read FImages write SetImages;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Align: TdxTileItemInnerObjectAlignment read FAlign write SetAlign default oaDefault;
    property AlignWithText: TdxTileControlImageWithTextAlignment read FAlignWithText write SetAlignWithText default itaNone;
    property Image: TdxSmartGlyph read FImage write SetImage;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property IndentHorz: Integer read FIndentHorz write SetIndentHorz default dxTileItemObjectDefaultIndent;
    property IndentVert: Integer read FIndentVert write SetIndentVert default dxTileItemObjectDefaultIndent;
    property Mode: TcxImageFitMode read FMode write SetMode default ifmNormal;
  end;

  { TdxTileControlItemFrameGlyph }

  TdxTileControlItemFrameGlyph = class(TdxTileControlItemCustomGlyph)
  end;

  { TdxTileControlTileItemGlyph }

  TdxTileControlItemGlyph = class(TdxTileControlItemCustomGlyph)
  published
    property Images;
  end;

  { TdxTileControlCustomStyle }

  TdxTileControlCustomStyle = class(TcxOwnedPersistent)
  private
    FBorderColor: TColor;
    FFont: TFont;
    FFontChanged: Boolean;
    FGradient: TdxSkinGradientMode;
    FGradientBeginColor: TColor;
    FGradientEndColor: TColor;
    FStretch: TdxSkinStretchMode;
    FTexture: TdxSmartImage;
    FOnChange: TNotifyEvent;
    procedure ImageChanged(Sender: TObject);
    function IsFontStored: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetGradientBeginColor(AValue: TColor);
    procedure SetGradientEndColor(AValue: TColor);
    procedure SetGradientMode(AValue: TdxSkinGradientMode);
    procedure SetStretch(AValue: TdxSkinStretchMode);
    procedure SetTexture(AValue: TdxSmartImage);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure FontChanged(Sender: TObject);
    function IsChanged: Boolean; virtual;
    function IsDefaultBackgroundColors: Boolean; virtual;
    function IsEmpty: Boolean; virtual;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Gradient: TdxSkinGradientMode read FGradient write SetGradientMode default gmHorizontal;
    property GradientBeginColor: TColor read FGradientBeginColor write SetGradientBeginColor default clDefault;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor default clDefault;
    property Stretch: TdxSkinStretchMode read FStretch write SetStretch default smStretch;
    property Texture: TdxSmartImage read FTexture write SetTexture;
  end;

  { TdxTileControlDetailSiteOptionsAnimate }

  TdxTileControlDetailOptionsAnimate = class(TcxOwnedPersistent)
  private
    FAnimationInterval: Integer;
    FAnimationMode: TdxTileControlDetailAnimationMode;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AnimationInterval: Integer read FAnimationInterval write FAnimationInterval default dxTileControlActivateDetailAnimationTime;
    property AnimationMode: TdxTileControlDetailAnimationMode read FAnimationMode write FAnimationMode default damScroll;
  end;

  { TdxTileControlCustomItemOptionsAnimate }

  TdxTileControlItemOptionsAnimateAssignedValue = (ioaavAnimateText);
  TdxTileControlItemOptionsAnimateAssignedValues = set of TdxTileControlItemOptionsAnimateAssignedValue;

  TdxTileControlCustomItemOptionsAnimate = class(TcxOwnedPersistent)
  private
    FAnimateText: Boolean;
    FAssignedValues: TdxTileControlItemOptionsAnimateAssignedValues;
    function GetAnimateText: Boolean;
    function IsAnimateTextStored: Boolean;
    procedure SetAnimateText(AValue: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
    function GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate; virtual;
    //
    property AssignedValues: TdxTileControlItemOptionsAnimateAssignedValues read FAssignedValues write FAssignedValues stored False;
    property ParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate read GetParentOptionsAnimate;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property AnimateText: Boolean read GetAnimateText write SetAnimateText stored IsAnimateTextStored;
  end;

  { TdxTileControlCustomItemViewData }

  TdxTileControlCustomItemViewData = class
  private
    FClientRect: TRect;
    FImage: TdxGPImage;
    FImageBounds: TRect;
    FIsDirty: Boolean;
    FItem: TdxTileControlCustomItem;
    FNeedDrawTextOnImage: Boolean;
    FTextBounds: array[0..3] of TRect;
    function GetBounds: TRect;
    function GetGalleryCellController: TdxTileControlGalleryCellController;
    function GetImage: TdxGPImage;
    function GetIsActive: Boolean;
    function GetPainter: TdxTileControlPainter;
    function GetTextBounds(AIndex: Integer): TRect;
    function GetTileItem: TdxTileControlItem;
    procedure SetIsDirty(AValue: Boolean);
    procedure SetNeedDrawTextOnImage(AValue: Boolean);
  protected
    procedure AdjustObjectBounds(const ABounds: TRect; var AObjectBounds: TRect;
      AAlign: TdxTileItemInnerObjectAlignment; AIndentHorz, AIndentVert: Integer);
    procedure AdjustImageBoundsWithText(const ABounds: TRect; AText: TdxTileControlItemText;
      var AImageBounds, ATextBounds: TRect);
    procedure CalculateImageAndTextLayout(const ABounds: TRect); virtual;
    procedure DrawItemBackground(const ACanvas: TcxCanvas); virtual;
    procedure DrawItemGlyph(const ACanvas: TcxCanvas); virtual;
    procedure DrawItemText(const ACanvas: TcxCanvas; const AIndex: Integer); virtual;
    procedure DrawItemTexts(const ACanvas: TcxCanvas); virtual;
    procedure PrepareViewData; virtual;

    property GalleryCellController: TdxTileControlGalleryCellController read GetGalleryCellController;
  public
    constructor Create(AItem: TdxTileControlCustomItem); virtual;
    destructor Destroy; override;
    procedure ValidateViewData;
    //
    property Bounds: TRect read GetBounds;
    property ClientRect: TRect read FClientRect;
    property Image: TdxGPImage read GetImage;
    property ImageBounds: TRect read FImageBounds;
    property IsActive: Boolean read GetIsActive;
    property IsDirty: Boolean read FIsDirty write SetIsDirty;
    property Item: TdxTileControlCustomItem read FItem;
    property NeedDrawTextOnImage: Boolean read FNeedDrawTextOnImage write SetNeedDrawTextOnImage;
    property Painter: TdxTileControlPainter read GetPainter;
    property TextBounds[Index: Integer]: TRect read GetTextBounds;
    property TileItem: TdxTileControlItem read GetTileItem;
  end;

  { TdxTileControlItemOptionsAnimate }

  TdxTileControlItemOptionsAnimate = class(TdxTileControlCustomItemOptionsAnimate)
  protected
    function GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate; override;
  published
    property AssignedValues;
  end;

  { TdxTileControlCustomItem }

  TdxTileControlCustomItem = class(TcxComponentCollectionItem)
  private
    FGlyph: TdxTileControlItemCustomGlyph;
    FMargins: TcxMargin;
    FOptionsAnimate: TdxTileControlItemOptionsAnimate;
    FParentStyle: Boolean;
    FStyle: TdxTileControlCustomStyle;
    FTexts: array [0 .. 3] of TdxTileControlItemText;
    FViewData: TdxTileControlCustomItemViewData;
    FVisible: Boolean;
    function GetText(AIndex: Integer): TdxTileControlItemText;
    function IsMarginsStored: Boolean;
    function IsStyleStored: Boolean;
    procedure MarginsChangeHandler(Sender: TObject);
    procedure SetMargins(AValue: TcxMargin);
    procedure SetOptionsAnimate(AValue: TdxTileControlItemOptionsAnimate);
    procedure SetParentStyle(AValue: Boolean);
    procedure SetStyle(AValue: TdxTileControlCustomStyle);
    procedure SetText(AIndex: Integer; AValue: TdxTileControlItemText);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateGlyph: TdxTileControlItemCustomGlyph; virtual; abstract;
    function CreateOptionsAnimate: TdxTileControlItemOptionsAnimate; virtual; abstract;
    function CreateText(const AAlign: TdxTileItemInnerObjectAlignment): TdxTileControlItemText; virtual;
    function CreateViewData: TdxTileControlCustomItemViewData; virtual;
    function IsGlyphStored: Boolean; virtual;
    function GetTileItem: TdxTileControlItem; virtual;
    procedure StyleChanged(Sender: TObject);
    //
    property Glyph: TdxTileControlItemCustomGlyph read FGlyph write FGlyph;
    property OptionsAnimate: TdxTileControlItemOptionsAnimate read FOptionsAnimate write SetOptionsAnimate;
    property ParentStyle: Boolean read FParentStyle write SetParentStyle default True;
    property ViewData: TdxTileControlCustomItemViewData read FViewData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    //
    property Margins: TcxMargin read FMargins write SetMargins stored IsMarginsStored;
    property Style: TdxTileControlCustomStyle read FStyle write SetStyle stored IsStyleStored;
    property Text1: TdxTileControlItemText index 0 read GetText write SetText;
    property Text2: TdxTileControlItemText index 1 read GetText write SetText;
    property Text3: TdxTileControlItemText index 2 read GetText write SetText;
    property Text4: TdxTileControlItemText index 3 read GetText write SetText;
    property TileItem: TdxTileControlItem read GetTileItem;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxTileControlItemFrameOptionsAnimate }

  TdxTileControlItemFrameOptionsAnimate = class(TdxTileControlItemOptionsAnimate)
  protected
    function GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate; override;
  end;

  { TdxTileControlItemFrame }

  TdxTileControlItemFrame = class(TdxTileControlCustomItem)
  private
    FData: Pointer;
    function GetGlyph: TdxTileControlItemFrameGlyph;
    procedure SetGlyph(AValue: TdxTileControlItemFrameGlyph);
  protected
    procedure Changed; override;
    function CreateGlyph: TdxTileControlItemCustomGlyph; override;
    function CreateOptionsAnimate: TdxTileControlItemOptionsAnimate; override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetTileItem: TdxTileControlItem; override;
  public
    destructor Destroy; override;

    property Data: Pointer read FData write FData;
  published
    property Glyph: TdxTileControlItemFrameGlyph read GetGlyph write SetGlyph stored IsGlyphStored;

    property ParentStyle;
    property Style;
    property Margins;
    property OptionsAnimate;
    property Text1;
    property Text2;
    property Text3;
    property Text4;
    property Visible;
  end;

  { TdxTileControlItemFrames }

  TdxTileControlForEachItemFrameProc = procedure(AItem: TdxTileControlItemFrame; const AData: Pointer) of object;

  TdxTileControlItemFrames = class(TcxComponentCollection)
  private
    function GetItem(AIndex: Integer): TdxTileControlItemFrame;
    function GetOwnerItem: TdxTileControlItem;
    procedure SetItem(AIndex: Integer; const AValue: TdxTileControlItemFrame);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure ForEach(AProc: TdxTileControlForEachItemFrameProc; const AData: Pointer);
  public
    function Add: TdxTileControlItemFrame;
    //
    property Owner: TdxTileControlItem read GetOwnerItem;
    property Items[Index: Integer]: TdxTileControlItemFrame read GetItem write SetItem; default;
  end;

  { TdxTileControlItemDetailOptions }

  TdxTileControlItemDetailOptions = class(TcxOwnedPersistent)
  private
    FCaption: string;
    FDetailControl: TWinControl;
    FDetailControlPrevParent: TWinControl;
    FDetailSite: TdxTileControlDetailSite;
    FShowTab: Boolean;
    function GetDetailControl: TWinControl;
    function GetHasDetail: Boolean;
    function GetDetailSite: TdxTileControlDetailSite;
    function GetTileControl: TdxCustomTileControl;
    function GetTileItem: TdxTileControlItem;
    procedure SetCaption(AValue: string);
  protected
    procedure Changed; virtual;
    function GetDetailSiteHeight: Integer; virtual;
    function GetDetailSiteWidth: Integer; virtual;
    function IsChanged: Boolean; virtual;
    procedure RestoreDetailControlPrevParent;
    procedure SetDetailControl(AValue: TWinControl); virtual;
    procedure StoreDetailControlPrevParent(AParent: TWinControl);
  public
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;

    property HasDetail: Boolean read GetHasDetail;
    property DetailSite: TdxTileControlDetailSite read GetDetailSite;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property TileItem: TdxTileControlItem read GetTileItem;
  published
    property Caption: string read FCaption write SetCaption;
    property DetailControl: TWinControl read GetDetailControl write SetDetailControl;
    property ShowTab: Boolean read FShowTab write FShowTab default False;
  end;

  { TdxTileControlItemGalleryCell }

  TdxTileControlItemGalleryCell = class(TdxTileControlCustomCellViewInfo)
  private
    FColumn: Byte;
    FController: TdxTileControlGalleryCellController;
    FCurrentImage: TdxSmartImage;
    FNextImage: TdxSmartImage;
    FTileItem: TdxTileControlItem;
    FRow: Byte;
    function GetAnimation: TdxTileControlGalleryCellAnimation;
  protected
    function CreateResultingImage(AImage: TdxSmartImage): TdxSmartImage;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawEmptyCell(AImage: TdxSmartImage);
    function GetEmptyCellBasicColor: TColor;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetVisibleBounds: TRect; override;
    function IsAnimated: Boolean;
    procedure ReplaceCurrentImageByNextAndCleanNextImage;

    property Controller: TdxTileControlGalleryCellController read FController;
    property TileItem: TdxTileControlItem read FTileItem;
    property Animation: TdxTileControlGalleryCellAnimation read GetAnimation;
  public
    constructor Create(ATileItem: TdxTileControlItem; AController: TdxTileControlGalleryCellController;
      const ARow, AColumn: Byte; const ABounds: TRect); virtual;
    destructor Destroy; override;

    property Column: Byte read FColumn;
    property CurrentImage: TdxSmartImage read FCurrentImage;
    property NextImage: TdxSmartImage read FNextImage;
    property Row: Byte read FRow;
  end;

  { TdxTileControlGalleryCells }

  TdxTileControlGalleryCells = class(TdxTileControlCells)
  private
    function GetCell(ARow, AColumn: Integer): TdxTileControlItemGalleryCell;
  public
    function CalculateHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;

    property Cells[ARow, AColumn: Integer]: TdxTileControlItemGalleryCell read GetCell;
  end;

  { TdxTileControlGalleryCellController }

  TdxTileControlGalleryCellController = class
  private
    FActualCellsHeight: Integer;
    FActualCellsWidth: Integer;
    FColumnCount: Byte;
    FGalleryCells: TdxTileControlGalleryCells;
    FImageColumnCount: Byte;
    FImageRowCount: Byte;
    FTileItem: TdxTileControlItem;
    FImageIndex: Integer;
    FLastImageIndex: Integer;
    FPreparedCells: TdxTileControlGalleryCells;
    FPreparedCell: TdxTileControlItemGalleryCell;
    FRowCount: Byte;
    FUntouchableCells: TdxTileControlGalleryCells;
    procedure CalculateDimensions;
    procedure CalculateImageIndexAndSize;
    function CanPopulatePreparedCellsAt(const ARow, AColumn: Byte): Boolean;
    procedure ChoosePreparedCell;
    function GetCellsBounds: TRect;
    function GetImagesCount: Integer;
    function GetGalleryImages: TcxImageCollection;
    function GetTileControl: TdxCustomTileControl;
    function IsEmptyImage(AImageIndex: Integer): Boolean;
    function IsThereEmptyCells: Boolean;
    procedure PopulatePreparedCells;
    procedure RemoveFromUntouchableCells;
    procedure SeparateImageIntoPreparedCells(AImage: TdxSmartImage);
    procedure SetPreparedCellsBackImages;
  protected
    procedure AddAnimation;
    function GetPreparedCellsCombinedBounds: TRect;
    procedure InitializeCells;
    function IsOnlyOneCellPrepared: Boolean;
    procedure OnEndBitmapRescale(Sender: TObject);

    property ActualCellsHeight: Integer read FActualCellsHeight;
    property ActualCellsWidth: Integer read FActualCellsWidth;
    property CellsBounds: TRect read GetCellsBounds;
    property GalleryImages: TcxImageCollection read GetGalleryImages;
    property ImagesCount: Integer read GetImagesCount;
    property ImageColumnCount: Byte read FImageColumnCount;
    property ImageRowCount: Byte read FImageRowCount;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property UntouchableCells: TdxTileControlGalleryCells read FUntouchableCells;
  public
    constructor Create(ATileItem: TdxTileControlItem); virtual;
    destructor Destroy; override;
    function AreCellsDirty: Boolean;
    procedure CheckPreparedCells;
    procedure DrawCells(ACanvas: TcxCanvas; AWithoutClipping: Boolean); virtual;
    function IsPreparedCellReady: Boolean;
    procedure PrepareCells;
    procedure RecalculateCells;

    property ColumnCount: Byte read FColumnCount;
    property GalleryCells: TdxTileControlGalleryCells read FGalleryCells;
    property PreparedCell: TdxTileControlItemGalleryCell read FPreparedCell;
    property PreparedCells: TdxTileControlGalleryCells read FPreparedCells;
    property TileItem: TdxTileControlItem read FTileItem;
    property RowCount: Byte read FRowCount;
  end;

  { TdxTileControlItem }

  TdxTileControlItemAllowOperationEvent = procedure(Sender: TdxCustomTileControl; AItem: TdxTileControlItem; var AAllow: Boolean) of object;
  TdxTileControlItemFrameEvent = procedure(Sender: TdxTileControlItemFrame) of object;
  TdxTileControlItemEvent = procedure(Sender: TdxTileControlItem) of object;
  TdxTileControlItemOperationEvent = procedure(Sender: TdxCustomTileControl; AItem: TdxTileControlItem) of object;

  TdxTileControlItem = class(TdxTileControlCustomItem, IcxStoredObject, IcxImageCollectionListener)
  private
    FActiveFrameIndex: Integer;
    FActiveTransitions: TcxObjectList;
    FAnimationInterval: Integer;
    FAnimationMode: TdxDrawAnimationMode;
    FAnimationTimer: TcxTimer;
    FChecked: Boolean;
    FDetailOptions: TdxTileControlItemDetailOptions;
    FEnabled: Boolean;
    FFrames: TdxTileControlItemFrames;
    FGalleryCellController: TdxTileControlGalleryCellController;
    FGalleryImages: TcxImageCollection;
    FGroup: TdxTileControlGroup;
    FGroupBeforeDrag: TdxTileControlGroup;
    FIndexInGroupAtLoaded: Integer;
    FIndexInGroupBeforeDrag: Integer;
    FIsActive: Boolean;
    FRowCount: Integer;
    FRowCountAtLoaded: Integer;
    FSize: TdxTileControlItemSize;
    FViewInfo: TdxTileControlItemViewInfo;
    FOnActivateDetail: TdxTileControlItemEvent;
    FOnActiveFrameChanged: TdxTileControlItemEvent;
    FOnDeactivateDetail: TdxTileControlItemEvent;
    FOnDeactivatingDetail: TdxTileControlItemAllowOperationEvent;
    FOnClick: TdxTileControlItemEvent;
    FOnFrameDestroy: TdxTileControlItemFrameEvent;
    FOnFrameInitialize: TdxTileControlItemFrameEvent;
    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;

    function GetActiveFrame: TdxTileControlItemFrame;
    function GetActuallyVisible: Boolean;
    function GetController: TdxTileControlController;
    function GetGlyph: TdxTileControlItemGlyph;
    function GetGroupIndex: Integer;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetIndexInGroup: Integer;
    function GetIsAnimationActive: Boolean;
    function GetIsLarge: Boolean;
    function GetIsMostLeft: Boolean;
    function GetIsMostRight: Boolean;
    function GetItemType: TdxTileControlItemType;
    function GetTileControl: TdxCustomTileControl;
    function GetVisibleFramesCount: Integer;
    procedure ReadItemIsLarge(AReader: TReader);
    procedure RefreshFrame(AItem: TdxTileControlItemFrame; const AData: Pointer);
    procedure RestoreAnimationInterval;
    procedure SetActiveFrame(AValue: TdxTileControlItemFrame);
    procedure SetActiveFrameIndex(AValue: Integer);
    procedure SetAnimationInterval(AValue: Integer);
    procedure SetChecked(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFrames(AValue: TdxTileControlItemFrames);
    procedure SetGalleryImages(AValue: TcxImageCollection);
    procedure SetGlyph(AValue: TdxTileControlItemGlyph);
    procedure SetGroup(AValue: TdxTileControlGroup);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetIndexInGroup(AValue: Integer);
    procedure SetIsLarge(AValue: Boolean);
    procedure SetOnActivateDetail(AValue: TdxTileControlItemEvent);
    procedure SetRowCount(AValue: Integer);
    procedure SetTimerHandlerFastestRepeat;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
  protected
    procedure ActiveFrameChanged; virtual;
    procedure AddTransition(ATransition: TdxAnimationTransition);
    procedure AnimationTimerHandler(Sender: TObject);
    function CanDisplayGallery: Boolean;
    function CanFocused: Boolean;
    function CanGalleryAnimation: Boolean;
    procedure Changed; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure CheckGalleryCellController;
    procedure CheckTimerEnabled;
    function CheckViewDataInitialized(AViewData: TdxTileControlCustomItemViewData): TdxTileControlCustomItemViewData;
    function ConvertToSize(AIsLarge: Boolean): TdxTileControlItemSize;
    function CreateDetailOptions: TdxTileControlItemDetailOptions; virtual;
    function CreateDetailSite: TdxTileControlDetailSite; virtual;
    function CreateFrames: TdxTileControlItemFrames; virtual;
    function CreateGlyph: TdxTileControlItemCustomGlyph; override;
    function CreateOptionsAnimate: TdxTileControlItemOptionsAnimate; override;
    function CreateViewInfo: TdxTileControlItemViewInfo; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoActivateDetail(ADetail: TdxTileControlDetailSite); virtual;
    procedure DoActiveFrameChanged; virtual;
    procedure DoClick; virtual;
    procedure DoDeactivateDetail; virtual;
    procedure DoFrameDestroy(AFrame: TdxTileControlItemFrame); virtual;
    procedure DoFrameInitialize(AFrame: TdxTileControlItemFrame); virtual;
    procedure FrameChanged(AFrame: TdxTileControlCustomItem); virtual;
    function GetActualViewData: TdxTileControlCustomItemViewData;
    function GetClientBounds: TRect;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetDetailOptionsClass: TdxTileControlItemDetailOptionsClass; virtual;
    function GetTileItem: TdxTileControlItem; override;
    function HasAnimatedContent: Boolean;
    function HasGalleryImages: Boolean;
    procedure Invalidate;
    function IsDetailOptionsStored: Boolean; virtual;
    function IsDragged: Boolean;
    function IsEnabled: Boolean;
    function IsGroupEnabled: Boolean;
    function IsGroupVisible: Boolean;
    function IsVisible: Boolean; virtual;
    procedure OnEndGalleryCellAnimation(AAnimation: TdxTileControlGalleryCellAnimation);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyBiDiModeChanged; virtual;
    procedure Refresh;
    procedure RemoveTransition(ATransition: TdxAnimationTransition);
    procedure RestoreBeforeDragLayout;
    procedure SetDetailOptions(AValue: TdxTileControlItemDetailOptions); virtual;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetSize(AValue: TdxTileControlItemSize); virtual;
    procedure StopTransitions;
    procedure StoreLayoutBeforeDrag;
    procedure ToggleChecked;
    function UseRightToLeftAlignment: Boolean;

    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    function GetPropertyIndex(const AName: string): Integer;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    //IcxImageCollectionListener
    procedure ImageCollectionChanged;
    procedure ImageCollectionDestroyed;

    property ActiveTransitions: TcxObjectList read FActiveTransitions;
    property AnimationTimer: TcxTimer read FAnimationTimer;
    property Controller: TdxTileControlController read GetController;
    property GalleryCellController: TdxTileControlGalleryCellController read FGalleryCellController;
    property IsAnimationActive: Boolean read GetIsAnimationActive;
    property IsActive: Boolean read FIsActive write FIsActive;
    property ItemType: TdxTileControlItemType read GetItemType;
    property VisibleFramesCount: Integer read GetVisibleFramesCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure ActivateDetail;
    procedure Click;
    procedure DeactivateDetail;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MakeVisible;
    procedure Move(AGroupDest: TdxTileControlGroup; AIndexDest: Integer);
    procedure RemoveFromGroup;

    property ActiveFrame: TdxTileControlItemFrame read GetActiveFrame write SetActiveFrame;
    property ActiveFrameIndex: Integer read FActiveFrameIndex write SetActiveFrameIndex;
    property ActuallyVisible: Boolean read GetActuallyVisible;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Group: TdxTileControlGroup read FGroup write SetGroup;
    property GroupBeforeDrag: TdxTileControlGroup read FGroupBeforeDrag;
    property IndexInGroupBeforeDrag: Integer read FIndexInGroupBeforeDrag;
    property IsLarge: Boolean read GetIsLarge write SetIsLarge default False;
    property IsMostLeft: Boolean read GetIsMostLeft;
    property IsMostRight: Boolean read GetIsMostRight;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlItemViewInfo read FViewInfo;
  published
    property AnimationInterval: Integer read FAnimationInterval write SetAnimationInterval default dxTileControlDefaultAnimationInterval;
    property AnimationMode: TdxDrawAnimationMode read FAnimationMode write FAnimationMode default dxTileControlDefaultAnimationMode;
    property DetailOptions: TdxTileControlItemDetailOptions read FDetailOptions write SetDetailOptions stored IsDetailOptionsStored;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Frames: TdxTileControlItemFrames read FFrames write SetFrames;
    property GalleryImages: TcxImageCollection read FGalleryImages write SetGalleryImages;
    property Glyph: TdxTileControlItemGlyph read GetGlyph write SetGlyph stored IsGlyphStored;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex;
    property IndexInGroup: Integer read GetIndexInGroup write SetIndexInGroup;
    property RowCount: Integer read FRowCount write SetRowCount default dxTileControlDefaultItemRowCount;
    property Size: TdxTileControlItemSize read FSize write SetSize default tcisRegular;

    property Style;
    property Margins;
    property OptionsAnimate;
    property Text1;
    property Text2;
    property Text3;
    property Text4;
    property Visible;
    // Events
    property OnActivateDetail: TdxTileControlItemEvent read FOnActivateDetail write SetOnActivateDetail;
    property OnActiveFrameChanged: TdxTileControlItemEvent read FOnActiveFrameChanged write FOnActiveFrameChanged;
    property OnClick: TdxTileControlItemEvent read FOnClick write FOnClick;
    property OnDeactivateDetail: TdxTileControlItemEvent read FOnDeactivateDetail write FOnDeactivateDetail;
    property OnDeactivatingDetail: TdxTileControlItemAllowOperationEvent read FOnDeactivatingDetail write FOnDeactivatingDetail;
    property OnFrameDestroy: TdxTileControlItemFrameEvent read FOnFrameDestroy write FOnFrameDestroy;
    property OnFrameInitialize: TdxTileControlItemFrameEvent read FOnFrameInitialize write FOnFrameInitialize;
    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;
  end;

  { TdxTileControlCustomStoredCollection }

  TdxTileControlCustomStoredCollection = class(TcxComponentCollection,
    IcxStoredObject, IcxStoredParent)
  private
    function GetTileControl: TdxCustomTileControl;
    // IcxStoredParent
    function IcxStoredParent.CreateChild = StoredCreateChild;
    procedure IcxStoredParent.DeleteChild = StoredDeleteChild;
    procedure IcxStoredParent.GetChildren = StoredChildren;
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetItemPrefixName: string; override;
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject }
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent implementation
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure StoredDeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure StoredChildren(AChildren: TStringList); virtual;
  public
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlItemCollection }

  TdxTileControlForEachItemProc = procedure(AItem: TdxTileControlItem; const AData: Pointer) of object;

  TdxTileControlItemCollection = class(TdxTileControlCustomStoredCollection)
  private
    function GetItem(AIndex: Integer): TdxTileControlItem;
    procedure SetItem(AIndex: Integer; const AValue: TdxTileControlItem);
  protected
    procedure ChangeScale(M, D: Integer); override;

    procedure ForEach(AProc: TdxTileControlForEachItemProc; const AData: Pointer; AVisibleOnly: Boolean = True);
    procedure Loaded; virtual;
    // IcxStoredObject
    function GetObjectName: string; override;
    // IcxStoredParent implementation
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; override;
    procedure StoredChildren(AChildren: TStringList); override;
  public
    function Add: TdxTileControlItem;

    property Items[Index: Integer]: TdxTileControlItem read GetItem write SetItem; default;
  end;

  TdxTileControlItemCollectionClass = class of TdxTileControlItemCollection;

  { TdxTileControlGroup }

  TdxTileControlGroup = class(TcxComponentCollectionItem, IcxStoredObject, IcxStoredParent)
  private
    FCaption: TdxTileControlGroupCaption;
    FCaptionEditor: TdxTileControlGroupCaptionInplaceEdit;
    FEnabled: Boolean;
    FIndent: Integer;
    FItems: TcxObjectList;
    FViewInfo: TdxTileControlGroupViewInfo;
    FVirtualGroupBefore: TdxTileControlVirtualGroupViewInfo;
    FVirtualGroupAfter: TdxTileControlVirtualGroupViewInfo;
    FVisible: Boolean;
    FTileControl: TdxCustomTileControl;
    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;

    function GetBounds: TRect;
    function GetExpandedBounds: TRect;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetGroups: TdxTileControlGroupCollection;
    function GetItem(AIndex: Integer): TdxTileControlItem;
    function GetItemCount: Integer;
    function GetIsMostLeft: Boolean;
    function GetIsMostRight: Boolean;
    function GetMaximizedAreaBounds: TRect;
    function GetRealColumnCount: Integer;
    function GetRealRowCount: Integer;
    function IsCaptionStored: Boolean;
    procedure SetIndent(AValue: Integer);
    procedure SetCaption(AValue: TdxTileControlGroupCaption);
    procedure SetEnabled(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    // IcxStoredParent
    function IcxStoredParent.CreateChild = StoredCreateChild;
    procedure IcxStoredParent.DeleteChild = StoredDeleteChild;
    procedure IcxStoredParent.GetChildren = StoredChildren;

    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
  protected
    procedure AssignFrom(AGroup: TdxTileControlGroup; AExcludeItem: TdxTileControlItem = nil);
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CreateCaptionEditor;
    procedure GroupChanged; virtual;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetCustomIndent(AVirtualGroup: TdxTileControlVirtualGroupViewInfo): Integer;
    procedure HideCaptionEditor;
    procedure Invalidate;
    function IsAvailableForNavigation: Boolean;
    function IsCaptionEditorPresent: Boolean;
    function IsDragged: Boolean;
    procedure SetCollection(AValue: TcxComponentCollection); override;
    procedure SetIndex(AValue: Integer); override;
    procedure SetIndexForHorzLayout(AValue: Integer);
    procedure SetIndexForVertLayout(AValue: Integer);
    procedure ShowCaptionEditor;
    procedure SynchronizeEditorPos;
    procedure UpdateTileControlLink;
    function UseRightToLeftAlignment: Boolean;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent implementation
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure StoredDeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure StoredChildren(AChildren: TStringList); virtual;

    property Bounds: TRect read GetBounds;
    property CaptionEditor: TdxTileControlGroupCaptionInplaceEdit read FCaptionEditor;
    property ExpandedBounds: TRect read GetExpandedBounds;
    property MaximizedAreaBounds: TRect read GetMaximizedAreaBounds;
    property ItemsList: TcxObjectList read FItems;
    property IsMostLeft: Boolean read GetIsMostLeft;
    property IsMostRight: Boolean read GetIsMostRight;
    property RealColumnCount: Integer read GetRealColumnCount;
    property RealRowCount: Integer read GetRealRowCount;
    property ViewInfo: TdxTileControlGroupViewInfo read FViewInfo;
    property VirtualGroupBefore: TdxTileControlVirtualGroupViewInfo read FVirtualGroupBefore;
    property VirtualGroupAfter: TdxTileControlVirtualGroupViewInfo read FVirtualGroupAfter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AItem: TdxTileControlItem);
    procedure DeleteItem(AItem: TdxTileControlItem);
    procedure DeleteItems;
    function IndexOfItem(AItem: TdxTileControlItem): Integer;
    procedure MakeVisible;
    procedure MoveItem(AItem: TdxTileControlItem; AIndexDest: Integer);
    procedure RemoveItem(AItem: TdxTileControlItem);
    procedure RemoveItems;

    property Groups: TdxTileControlGroupCollection read GetGroups;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TdxTileControlItem read GetItem;
    property TileControl: TdxCustomTileControl read FTileControl;
  published
    property Indent: Integer read FIndent write SetIndent default 0;
    property Caption: TdxTileControlGroupCaption read FCaption write SetCaption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Index;
    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent
      read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent
      read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent
      read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;
  end;

  { TdxTileControlGroupCaptionViewInfo }

  TdxTileControlGroupCaptionViewInfo = class(TdxTileControlAnimatedDragAndDropCustomCellViewInfo)
  private
    FCaption: TdxTileControlGroupCaption;
    FFont: TFont;
    FHeight: Integer;
    FIndent: Integer;
    FTextBounds: TRect;
    function GetGroup: TdxTileControlGroup;
    function GetPainter: TdxTileControlPainter;
    procedure SetHeight(AValue: Integer);
  protected
    procedure CalculateBounds; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect); override;
    function GetBaseDrawBounds: TRect; override;
    function GetHeight: Integer; override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetTextBounds(ABounds: TRect): TRect;
    function GetTextColor: TColor;
    function GetTextOutFlags: Integer;
    function GetTileControl: TdxCustomTileControl; override;
    function GetVisibleBounds: TRect; override;
    procedure MeasureHeight; virtual;
    procedure Offset(const DX, DY: Integer);  override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;

    property Caption: TdxTileControlGroupCaption read FCaption;
    property Height: Integer read GetHeight write SetHeight;
    property Painter: TdxTileControlPainter read GetPainter;
  public
    constructor Create(AOwner: TdxTileControlGroupCaption); virtual;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas); override;

    property Font: TFont read FFont;
    property Group: TdxTileControlGroup read GetGroup;
    property TextBounds: TRect read FTextBounds;
    property TextColor: TColor read GetTextColor;
  end;

  { TdxTileControlGroupCaption }

  TdxTileControlGroupCaption = class(TcxOwnedPersistent)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FFont: TFont;
    FFontChanged: Boolean;
    FGroup: TdxTileControlGroup;
    FText: string;
    FViewInfo: TdxTileControlGroupCaptionViewInfo;
    function GetBounds: TRect;
    function GetHeight: Integer;
    function GetTileControl: TdxCustomTileControl;
    function IsFontStored: Boolean;
    procedure MeasureHeight;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetHeight(AHeight: Integer);
    procedure SetText(AValue: string);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function IsChanged: Boolean; virtual;

    property Bounds: TRect read GetBounds;
    property Group: TdxTileControlGroup read FGroup;
    property Height: Integer read GetHeight;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlGroupCaptionViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Text: string read FText write SetText;
  end;

  { TdxTileControlGroupCollection }

  TdxTileControlGroupCollection = class(TdxTileControlCustomStoredCollection)
  private
    function GetItem(AIndex: Integer): TdxTileControlGroup;
    procedure SetItem(AIndex: Integer; const AValue: TdxTileControlGroup);
  protected
    procedure ChangeScale(M, D: Integer); override;

    function GetNextVisibleGroup(AGroup: TdxTileControlGroup): TdxTileControlGroup;
    function GetPrevVisibleGroup(AGroup: TdxTileControlGroup): TdxTileControlGroup;

    procedure Loaded; virtual;
    procedure Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
    // IcxStoredObject }
    function GetObjectName: string; override;
    // IcxStoredParent implementation
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; override;
    procedure StoredDeleteChild(const AObjectName: string; AObject: TObject); override;
  public
    function Add: TdxTileControlGroup;
    function Insert(AIndex: Integer): TdxTileControlGroup;

    property Items[Index: Integer]: TdxTileControlGroup read GetItem write SetItem; default;
  end;

  TdxTileControlGroupCollectionClass = class of TdxTileControlGroupCollection;

  { TdxTileControlStyle }

  TdxTileControlStyle = class(TdxTileControlCustomStyle)
  private
    FCheckedItemCheckMarkColor: TColor;
    FCheckedItemFrameColor: TColor;
    FFocusedColor: TColor;
    procedure SetCheckedItemCheckMarkColor(AValue: TColor);
    procedure SetCheckedItemFrameColor(AValue: TColor);
    procedure SetFocusedColor(AValue: TColor);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property CheckedItemCheckMarkColor: TColor read FCheckedItemCheckMarkColor write SetCheckedItemCheckMarkColor default clDefault;
    property CheckedItemFrameColor: TColor read FCheckedItemFrameColor write SetCheckedItemFrameColor default clDefault;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor default clDefault;
  end;

  { TdxTileControlOptionsBehavior }

  TdxTileControlOptionsBehavior = class(TcxOwnedPersistent)
  private
    FBackgroundScrollSync: Byte;
    FFocusItemOnCycle: Boolean;
    FGroupCaptionHint: string;
    FGroupMoving: Boolean;
    FGroupRenaming: Boolean;
    FHideFocusOnItemHotTrack: Boolean;
    FItemCheckMode: TdxTileControlItemCheckMode;
    FItemHotTrackHighlightColor: TdxAlphaColor;
    FItemFocusMode: TdxTileControlItemFocusMode;
    FItemHotTrackMode: TdxTileControlItemHotTrackMode;
    FItemMoving: Boolean;
    FItemOuterFrameColor: TColor;
    FItemPressAnimation: Boolean;
    FScrollMode: TdxTileControlScrollMode;
    function GetDefaultGroupCaptionHint: string;
    function GetTileControl: TdxCustomTileControl;
    function IsGroupCaptionHintStored: Boolean;
    function IsItemCheckModeStored: Boolean;
    function IsItemFocusModeStored: Boolean;
    function IsItemHotTrackHighlightColorStored: Boolean;
    function IsItemHotTrackModeStored: Boolean;
    function IsItemMovingStored: Boolean;
    function IsItemPressAnimationStored: Boolean;
    function IsScrollModeStored: Boolean;
    procedure ReadItemHotTrack(AReader: TReader);
    procedure SetBackgroundScrollSync(AValue: Byte);
    procedure SetGroupRenaming(AValue: Boolean);
    procedure SetItemCheckMode(AValue: TdxTileControlItemCheckMode);
    procedure SetItemFocusMode(AValue: TdxTileControlItemFocusMode);
    procedure SetItemHotTrackHighlightColor(AValue: TdxAlphaColor);
    procedure SetItemHotTrackMode(AValue: TdxTileControlItemHotTrackMode);
    procedure SetItemMoving(AValue: Boolean);
    procedure SetItemOuterFrameColor(AValue: TColor);
    procedure SetItemPressAnimation(AValue: Boolean);
    procedure SetScrollMode(AValue: TdxTileControlScrollMode);
  protected
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;

    function GetDefaultItemCheckMode: TdxTileControlItemCheckMode; virtual;
    function GetDefaultItemFocusMode: TdxTileControlItemFocusMode; virtual;
    function GetDefaultItemHotTrackHighlightColor: TdxAlphaColor; virtual;
    function GetDefaultItemHotTrackMode: TdxTileControlItemHotTrackMode; virtual;
    function GetDefaultItemMoving: Boolean; virtual;
    function GetDefaultItemPressAnimation: Boolean; virtual;
    function GetDefaultScrollMode: TdxTileControlScrollMode; virtual;

    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property BackgroundScrollSync: Byte read FBackgroundScrollSync write SetBackgroundScrollSync default dxTileControlDefaultScrollSync;
    property FocusItemOnCycle: Boolean read FFocusItemOnCycle write FFocusItemOnCycle default True;
    property GroupCaptionHint: string read FGroupCaptionHint write FGroupCaptionHint stored IsGroupCaptionHintStored;
    property GroupMoving: Boolean read FGroupMoving write FGroupMoving default False;
    property GroupRenaming: Boolean read FGroupRenaming write SetGroupRenaming default False;
    property HideFocusOnItemHotTrack: Boolean read FHideFocusOnItemHotTrack write FHideFocusOnItemHotTrack default False;
    property ItemCheckMode: TdxTileControlItemCheckMode read FItemCheckMode write SetItemCheckMode stored IsItemCheckModeStored;
    property ItemFocusMode: TdxTileControlItemFocusMode read FItemFocusMode write SetItemFocusMode stored IsItemFocusModeStored;
    property ItemHotTrackHighlightColor: TdxAlphaColor read FItemHotTrackHighlightColor write SetItemHotTrackHighlightColor stored IsItemHotTrackHighlightColorStored;
    property ItemHotTrackMode: TdxTileControlItemHotTrackMode read FItemHotTrackMode write SetItemHotTrackMode stored IsItemHotTrackModeStored;
    property ItemMoving: Boolean read FItemMoving write SetItemMoving  stored IsItemMovingStored;
    property ItemOuterFrameColor: TColor read FItemOuterFrameColor write SetItemOuterFrameColor default clDefault;
    property ItemPressAnimation: Boolean read FItemPressAnimation write SetItemPressAnimation stored IsItemPressAnimationStored;
    property ScrollMode: TdxTileControlScrollMode read FScrollMode write SetScrollMode stored IsScrollModeStored;
  end;

  { TdxTileControlOptionsView }

  TdxTileControlOptionsView = class(TcxOwnedPersistent)
  private
    FCenterContentHorz: Boolean;
    FCenterContentVert: Boolean;
    FFixedIndentHorz: Boolean;
    FFixedIndentVert: Boolean;
    FGroupBlockMaxColumnCount: Integer;
    FGroupIndent: Integer;
    FGroupLayout: TdxTileControlGroupLayout;
    FGroupMaxRowCount: Integer;
    FIndentHorz: Integer;
    FIndentVert: Integer;
    FItemHeight: Integer;
    FItemIndent: Integer;
    FItemWidth: Integer;

    function GetMaxItemRowCount: Integer;
    function GetTileControl: TdxCustomTileControl;
    procedure ReadItemSize(AReader: TReader);
    procedure SetCenterContentHorz(AValue: Boolean);
    procedure SetCenterContentVert(AValue: Boolean);
    procedure SetFixedIndentHorz(AValue: Boolean);
    procedure SetFixedIndentVert(AValue: Boolean);
    procedure SetGroupBlockMaxColumnCount(AValue: Integer);
    procedure SetGroupIndent(AValue: Integer);
    procedure SetGroupMaxRowCount(AValue: Integer);
    procedure SetIndentHorz(AValue: Integer);
    procedure SetIndentVert(AValue: Integer);
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemIndent(AValue: Integer);
    procedure SetItemWidth(AValue: Integer);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;

    function GetDefaultFixedIndentHorz: Boolean; virtual;
    function GetDefaultFixedIndentVert: Boolean; virtual;
    function GetDefaultGroupBlockMaxColumnCount: Integer; virtual;
    function GetDefaultGroupIndent: Integer; virtual;
    function GetDefaultGroupMaxRowCount: Integer; virtual;
    function GetDefaultIndentHorz: Integer; virtual;
    function GetDefaultIndentVert: Integer; virtual;
    function GetDefaultItemHeight: Integer; virtual;
    function GetDefaultItemIndent: Integer; virtual;
    function GetDefaultItemWidth: Integer; virtual;

    function IsAllowableWidth(const AWidth: Integer): Boolean; virtual;
    function IsFixedIndentHorzStored: Boolean;
    function IsFixedIndentVertStored: Boolean;
    function IsGroupBlockMaxColumnCountStored: Boolean;
    function IsGroupIndentStored: Boolean;
    function IsGroupMaxRowCountStored: Boolean;
    function IsIndentHorzStored: Boolean;
    function IsIndentVertStored: Boolean;
    function IsItemHeightStored: Boolean;
    function IsItemIndentStored: Boolean;
    function IsItemWidthStored: Boolean;

    procedure SetGroupLayout(AValue: TdxTileControlGroupLayout); virtual;

    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property CenterContentHorz: Boolean read FCenterContentHorz write SetCenterContentHorz default False;
    property CenterContentVert: Boolean read FCenterContentVert write SetCenterContentVert default False;
    property FixedIndentHorz: Boolean read FFixedIndentHorz write SetFixedIndentHorz stored IsFixedIndentHorzStored;
    property FixedIndentVert: Boolean read FFixedIndentVert write SetFixedIndentVert stored IsFixedIndentVertStored;
    property GroupBlockMaxColumnCount: Integer read FGroupBlockMaxColumnCount write SetGroupBlockMaxColumnCount stored IsGroupBlockMaxColumnCountStored;
    property GroupIndent: Integer read FGroupIndent write SetGroupIndent stored IsGroupIndentStored;
    property GroupLayout: TdxTileControlGroupLayout read FGroupLayout write SetGroupLayout default glHorizontal;
    property GroupMaxRowCount: Integer read FGroupMaxRowCount write SetGroupMaxRowCount stored IsGroupMaxRowCountStored;
    property IndentHorz: Integer read FIndentHorz write SetIndentHorz stored IsIndentHorzStored;
    property IndentVert: Integer read FIndentVert write SetIndentVert stored IsIndentVertStored;
    property ItemHeight: Integer read FItemHeight write SetItemHeight stored IsItemHeightStored;
    property ItemIndent: Integer read FItemIndent write SetItemIndent stored IsItemIndentStored;
    property ItemWidth: Integer read FItemWidth write SetItemWidth stored IsItemWidthStored;
  end;

  { TdxTileControlViewInfo }

  TdxTileControlViewInfo = class(TdxTileControlCustomViewInfo)
  private
    FBackground: TcxBitmap;
    FCalculationCount: Integer;
    FCells: TdxTileControlCells;
    FClientBounds: TRect;
    FContentBottom: Integer;
    FContentHeight: Integer;
    FContentRight: Integer;
    FContentWidth: Integer;
    FFixedGroupsOrigin: Boolean;
    FItemHalfIndent: Integer;
    FItemLargeWidth: Integer;
    FItemSmallHeight: Integer;
    FItemSmallWidth: Integer;
    FRealContentHeight: Integer;
    FRealContentWidth: Integer;
    FTilesArea: TRect;
    FTilesZone: TRect;
    FRowCount: Integer;
    FLeftScrollPos: Integer;
    FScrollButtonViewInfo: array[TcxArrowDirection] of TdxTileControlScrollButtonViewInfo;
    FTileControl: TdxCustomTileControl;
    FTopScrollPos: Integer;
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftScrollBar: Boolean;
    FVisibleGroupsOrigin: TPoint;
    procedure CalculateDerivedItemSizes;
    function GetGroupCount: Integer;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetGroups: TdxTileControlGroupCollection;
    function GetItems: TdxTileControlItemCollection;
    function GetPainter: TdxTileControlPainter;
    function GetSubColumnSize: Integer;
    function GetTitle: TdxTileControlTitle;

    procedure CalculateClientBounds;
    function CalculateTilesArea: TRect;
    procedure DestroyScrollButtonsViewInfo;
    procedure DoCenterContent;
    procedure DoCenterContentHorz;
    procedure DoCenterContentHorzAtHorizontalGroupLayout(const AShift, AAreaRight: Integer);
    procedure DoCenterContentHorzAtVerticalGroupLayout(const AShift: Integer);
    procedure DoCenterContentVert;
    procedure DoCenterContentVertAtHorizontalGroupLayout(const AShift: Integer);
    procedure DoCenterContentVertAtVerticalGroupLayout(const AShift, AAreaBottom: Integer);
    procedure DrawDesignTimeBorder(ACanvas: TcxCanvas);
    function GetContentBounds: TRect;
    function GetTitleHeight: Integer;
    function GetVisibleGroupsBounds: TRect;
    function IsFixedContentLeftSide: Boolean;
    function IsFixedContentTopSide: Boolean;
    function IsScrollBarsParametersWasChanged: Boolean;
    procedure MakeVisibleOnHorz(const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
    procedure MakeVisibleOnVert(const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
    procedure SetLeftScrollPos(AValue: Integer);
    procedure SetTopScrollPos(AValue: Integer);
  protected
    DragDropChanges: TdxTileControlDragAndDropChanges;
    FVirtualGroups: TcxObjectList;

    procedure AddGroup(AGroup: TdxTileControlGroup);
    procedure AddGroups; virtual;
    procedure AddItem(AItem: TdxTileControlItem); virtual;
    procedure AddItems; virtual;
    procedure AddScrollButtons;
    procedure AddVirtualGroupAfter(AGroup: TdxTileControlGroup);
    procedure AddVirtualGroupBefore(AGroup: TdxTileControlGroup);
    procedure CalculateRowCount;
    function CalculateTilesZone: TRect; virtual;
    procedure CalculateTitle;
    procedure CheckBiDiModeAlignment; virtual;
    procedure ClearGroupsOrigin;
    procedure CreateScrollButtonsViewInfo; virtual;
    procedure DoCalculate; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawTileControlPart(ADest: TcxBitmap32; const AArea: TRect);
    function GetBackgroundPart(const ARect: TRect): TcxBitmap;
    function GetDetailSiteArea: TRect; virtual;
    function GetGroupsCaptionsMaxHeight: Integer;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetHorzScrollPage: Integer; virtual;
    function GetItemsMaxWidth: Integer;
    function GetMaxPossibilityFocusItemFrameSize: Integer; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetScrollButtonViewInfoClass: TdxTileControlScrollButtonViewInfoClass; virtual;
    function GetVertScrollPage: Integer; virtual;
    procedure HideScrollButtons;
    function IsContentCentredHorz: Boolean;
    function IsContentCentredVert: Boolean;
    function IsContentNotCentredHorz: Boolean;
    function IsContentNotCentredVert: Boolean;
    function IsScrollAvailable: Boolean; overload;
    function IsScrollAvailable(ADirection: TcxArrowDirection): Boolean; overload;
    procedure MakeVisible(const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
    procedure MeasureGroupsCaptionsHeights;
    procedure Scroll(const DX, DY: Integer); virtual;
    procedure StoreGroupsOrigin;
  public
    constructor Create(AOwner: TdxCustomTileControl); virtual;
    destructor Destroy; override;
    procedure Calculate;
    procedure Clear;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure RefreshState; override;

    property Background: TcxBitmap read FBackground;
    property Cells: TdxTileControlCells read FCells write FCells;
    property ClientBounds: TRect read FClientBounds;
    property ContentHeight: Integer read FContentHeight write FContentHeight;
    property ContentWidth: Integer read FContentWidth write FContentWidth;
    property DetailSiteArea: TRect read GetDetailSiteArea;
    property Groups: TdxTileControlGroupCollection read GetGroups;
    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
    property GroupsCount: Integer read GetGroupCount;
    property ItemHalfIndent: Integer read FItemHalfIndent;
    property ItemLargeWidth: Integer read FItemLargeWidth;
    property ItemSmallHeight: Integer read FItemSmallHeight;
    property ItemSmallWidth: Integer read FItemSmallWidth;
    property Items: TdxTileControlItemCollection read GetItems;
    property Painter: TdxTileControlPainter read GetPainter;
    property RowCount: Integer read FRowCount;
    property HorzScrollPage: Integer read GetHorzScrollPage;
    property LeftScrollPos: Integer read FLeftScrollPos write SetLeftScrollPos;
    property SubColumnSize: Integer read GetSubColumnSize;
    property TileControl: TdxCustomTileControl read FTileControl;
    property TilesArea: TRect read FTilesArea;
    property TilesZone: TRect read FTilesZone;
    property Title: TdxTileControlTitle read GetTitle;
    property TitleHeight: Integer read GetTitleHeight;
    property TopScrollPos: Integer read FTopScrollPos write SetTopScrollPos;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment;
    property UseRightToLeftScrollBar: Boolean read FUseRightToLeftScrollBar;
    property VertScrollPage: Integer read GetVertScrollPage;
    property VirtualGroups: TcxObjectList read FVirtualGroups;
    property VisibleGroupsOrigin: TPoint read FVisibleGroupsOrigin;
  end;

  { TdxTileControlAssets }

  TdxTileControlAssets = class(TObject)
  private
    FBackButton: TdxSkinImage;
    FCustomButton: TdxSkinImage;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    property BackButton: TdxSkinImage read FBackButton;
    property CustomButton: TdxSkinImage read FCustomButton;
  end;

  { TdxTileControlPainter }

  TdxTileControlPainter = class(TcxIUnknownObject)
  private
    FSkinInfo: TdxSkinInfo;
    FTileControl: TdxCustomTileControl;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetUserSkins: Boolean;
  protected
    procedure DrawDefaultItemBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawDefaultItemCheck(ACanvas: TcxCanvas; const R: TRect); overload; virtual;
    procedure DrawDefaultItemCheck(ACanvas: TcxCanvas; const R: TRect;
      ABackgroundColor, ACheckMarkColor: TColor; AAlpha: Byte = MaxByte); overload; virtual;
    procedure DrawHighlightRect(ACanvas: TcxCanvas; const R: TRect; const AHighlightColor: TdxAlphaColor;
      const ABackgroundColor: TColor = clNone); virtual;
    procedure DrawItemCheck(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawStyleRect(ACanvas: TcxCanvas; const R: TRect; AStyle: TdxTileControlCustomStyle;
      const ATextureOffsetX, ATextureOffsetY: Integer; ABorders: TcxBorders);
    procedure DrawStyleTexture(ACanvas: TdxGPCanvas; const R: TRect; AStyle: TdxTileControlCustomStyle;
      const ATextureOffsetX, ATextureOffsetY: Integer);
    procedure DrawTransparentBackground(ACanvas: TcxCanvas; const R: TRect);
    procedure ExcludeInvisibleOuterFrameParts(ACanvas: TcxCanvas; const R: TRect; const AFrameSize: Integer);
    function GetActionBarDefaultBackgroundColor: TColor; virtual;
    function GetActionBarDefaultTextColor: TColor; virtual;
    function GetCheckedItemCheckMarkColor: TColor; virtual;
    function GetCheckedItemFrameColor: TColor; virtual;
    function GetDefaultItemTextBackgroundColor: TColor; virtual;
    function GetDefaultItemTextColor: TColor; virtual;
    function GetGroupCaptionDefaultFontSize: Integer; virtual;
    function GetGroupCaptionDefaultTextColor: TColor; virtual;
    function GetItemOuterFrameColor(AItem: TdxTileControlCustomItem): TColor; virtual;
    function GetPageTabContentOffset: TRect; virtual;
    function GetPageTabDefaultFontSize: Integer; virtual;
    function GetPageTabTextColor(AState: TcxButtonState): TColor; virtual;
    function GetSelectionSelectedColor: TColor; virtual;
    function GetTitleDefaultFontSize: Integer; virtual;
    function GetTitleDefaultTextColor: TColor; virtual;
  public
    constructor Create(ATileControl: TdxCustomTileControl); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AGlyph: TdxSkinImage; AGlyphType: TImageType; AColor: TColor; const AIsRTL: Boolean = False); virtual;
    procedure DrawItemBackground(ACanvas: TcxCanvas; const R: TRect; AStyle: TdxTileControlCustomStyle); virtual;
    procedure DrawItemPlace(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawPageTab(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawVirtualGroup(ACanvas: TcxCanvas; const R: TRect); virtual;
    class function GetColorAsItemBackground(AItem: TdxTileControlCustomItem): TColor;
    function GetSelectionFocusedColor(AItem: TdxTileControlCustomItem = nil): TColor; virtual;
    function GetSelectionHottrackedColor(AItem: TdxTileControlCustomItem = nil): TColor; virtual;
    procedure ValidatePainterData; virtual;
    //
    property ActionBarDefaultBackgroundColor: TColor read GetActionBarDefaultBackgroundColor;
    property ActionBarDefaultTextColor: TColor read GetActionBarDefaultTextColor;
    property CheckedItemCheckMarkColor: TColor read GetCheckedItemCheckMarkColor;
    property CheckedItemFrameColor: TColor read GetCheckedItemFrameColor;
    property DefaultItemTextBackgroundColor: TColor read GetDefaultItemTextBackgroundColor;
    property DefaultItemTextColor: TColor read GetDefaultItemTextColor;
    property GroupCaptionDefaultFontSize: Integer read GetGroupCaptionDefaultFontSize;
    property GroupCaptionDefaultTextColor: TColor read GetGroupCaptionDefaultTextColor;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property PageTabContentOffset: TRect read GetPageTabContentOffset;
    property PageTabDefaultFontSize: Integer read GetPageTabDefaultFontSize;
    property PageTabTextColor[AState: TcxButtonState]: TColor read GetPageTabTextColor;
    property SkinInfo: TdxSkinInfo read FSkinInfo;
    property SelectionSelectedColor: TColor read GetSelectionSelectedColor;
    property TileControl: TdxCustomTileControl read FTileControl;
    property TitleDefaultFontSize: Integer read GetTitleDefaultFontSize;
    property TitleDefaultTextColor: TColor read GetTitleDefaultTextColor;
    property UseSkins: Boolean read GetUserSkins;
  end;

  { TdxTileControlTitle }

  TdxTileControlTitle = class(TcxOwnedPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FFontChanged: Boolean;
    FGlyph: TdxSmartGlyph;
    FGlyphAlignHorz: TAlignment;
    FGlyphAlignVert: TcxAlignmentVert;
    FIndentHorz: Integer;
    FIndentVert: Integer;
    FTabsActiveTextColor: TColor;
    FTabsDisabledTextColor: TColor;
    FTabsFontSize: Integer;
    FTabsHotTextColor: TColor;
    FTabsTextColor: TColor;
    FText: string;
    FTextAlignHorz: TAlignment;
    FTextAlignVert: TcxAlignmentVert;
    FViewInfo: TdxTileControlTitleViewInfo;
    FWordWrap: Boolean;
    function IsFontStored: Boolean;
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetGlyphAlignHorz(AValue: TAlignment);
    procedure SetGlyphAlignVert(AValue: TcxAlignmentVert);
    procedure SetIndentHorz(AValue: Integer);
    procedure SetIndentVert(AValue: Integer);
    procedure SetTabsFontSize(AValue: Integer);
    procedure SetTabsActiveTextColor(AValue: TColor);
    procedure SetTabsDisabledTextColor(AValue: TColor);
    procedure SetTabsHotTextColor(AValue: TColor);
    procedure SetTabsTextColor(AValue: TColor);
    procedure SetText(AValue: string);
    procedure SetTextAlignHorz(AValue: TAlignment);
    procedure SetTextAlignVert(AValue: TcxAlignmentVert);
    procedure SetWordWrap(AValue: Boolean);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function CreateViewInfo: TdxTileControlTitleViewInfo; virtual;
    procedure FontChanged(Sender: TObject);
    function GetTileControl: TdxCustomTileControl; virtual;
    procedure GlyphChanged(Sender: TObject);
    function IsChanged: Boolean; virtual;

    property GlyphAlignHorz: TAlignment read FGlyphAlignHorz write SetGlyphAlignHorz default taRightJustify;
    property GlyphAlignVert: TcxAlignmentVert read FGlyphAlignVert write SetGlyphAlignVert default vaBottom;
    property TextAlignHorz: TAlignment read FTextAlignHorz write SetTextAlignHorz default taLeftJustify;
    property TextAlignVert: TcxAlignmentVert read FTextAlignVert write SetTextAlignVert default vaBottom;
    property TileControl: TdxCustomTileControl read GetTileControl;
    property ViewInfo: TdxTileControlTitleViewInfo read FViewInfo;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property IndentHorz: Integer read FIndentHorz write SetIndentHorz default dxTileItemObjectDefaultIndent;
    property IndentVert: Integer read FIndentVert write SetIndentVert default dxTileItemObjectDefaultIndent;
    property TabsActiveTextColor: TColor read FTabsActiveTextColor write SetTabsActiveTextColor default clDefault;
    property TabsDisabledTextColor: TColor read FTabsDisabledTextColor write SetTabsDisabledTextColor default clDefault;
    property TabsFontSize: Integer read FTabsFontSize write SetTabsFontSize default dxTileControlDefaultTabFontSize;
    property TabsHotTextColor: TColor read FTabsHotTextColor write SetTabsHotTextColor default clDefault;
    property TabsTextColor: TColor read FTabsTextColor write SetTabsTextColor default clDefault;
    property Text: string read FText write SetText;
  end;

  { TdxTileControlPageTabCellViewInfo }

  TdxTileControlPageTabCellViewInfo = class(TdxTileControlCustomCellViewInfo)
  private
    FItem: TdxTileControlItem;
    FOwner: TdxTileControlDetailSiteTitleViewInfo;
    FState: TcxButtonState;
    function GetActive: Boolean;
    function GetCaption: string;
    function GetFont: TFont;
    function GetPainter: TdxTileControlPainter;
    procedure SetState(AValue: TcxButtonState);
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetContentOffsets: TRect; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
    procedure SetBounds(const ABounds, AVisibleBounds: TRect);
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    constructor Create(AOwner: TdxTileControlDetailSiteTitleViewInfo; AItem: TdxTileControlItem);
    function GetMarginsWidth: Integer; virtual;
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;
    procedure RefreshState; override;

    property Active: Boolean read GetActive;
    property Caption: string read GetCaption;
    property Font: TFont read GetFont;
    property Item: TdxTileControlItem read FItem;
    property Owner: TdxTileControlDetailSiteTitleViewInfo read FOwner;
    property Painter: TdxTileControlPainter read GetPainter;
    property State: TcxButtonState read FState write SetState;
  end;

  { TdxTileControlCustomButtonViewInfo }

  TdxTileControlCustomButtonViewInfo = class(TdxTileControlCustomCellViewInfo)
  private
    FState: TcxButtonState;
    procedure SetState(const Value: TcxButtonState);
  protected
    function GetTexture: TdxSkinImage; virtual; abstract;
    function GetTextureType: TImageType; virtual;
  public
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;
    procedure SetBounds(const ABounds, AVisibleBounds: TRect);
    //
    property State: TcxButtonState read FState write SetState;
    property Texture: TdxSkinImage read GetTexture;
    property TextureType: TImageType read GetTextureType;
  end;

  { TdxTileControlDetailSiteBackButtonViewInfo }

  TdxTileControlDetailSiteBackButtonViewInfo = class(TdxTileControlCustomButtonViewInfo)
  private
    FOwner: TdxTileControlDetailSite;
  protected
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTexture: TdxSkinImage; override;
  public
    constructor Create(AOwner: TdxTileControlDetailSite); virtual;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure Invalidate; override;
    procedure RefreshState; override;

    property Owner: TdxTileControlDetailSite read FOwner;
  end;

  { TdxTileControlScrollButtonViewInfo }

  TdxTileControlScrollButtonViewInfo = class(TdxTileControlCustomCellViewInfo,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  private
    FDirection: TcxArrowDirection;
    FOwner: TdxCustomTileControl;
    FState: TcxButtonState;
    FTimer: TcxTimer;

    function GetController: TdxTileControlController;
    function GetViewInfo: TdxTileControlViewInfo;
    procedure SetState(const AValue: TcxButtonState);
    procedure TimerHandler(Sender: TObject);
  protected
    procedure DoCalculate; override;
    procedure DoCalculateTopLeft(const ASizeX, ASizeY: Integer; var ATop, ALeft: Integer); virtual;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoScrollContent; virtual;
    function GetArea: TRect; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetVisibleBounds: TRect; override;
    procedure Scroll(const DX, DY: Integer); override;
    // IcxMouseTrackingCaller
    procedure MouseLeave; virtual;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;

    property Timer: TcxTimer read FTimer;
  public
    constructor Create(AOwner: TdxCustomTileControl; ADirection: TcxArrowDirection); virtual;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure RefreshState; override;

    property Area: TRect read GetArea;
    property Controller: TdxTileControlController read GetController;
    property Direction: TcxArrowDirection read FDirection;
    property Owner: TdxCustomTileControl read FOwner;
    property State: TcxButtonState read FState write SetState;
    property ViewInfo: TdxTileControlViewInfo read GetViewInfo;
  end;

  { TdxTileControlDetailSiteTabsScrollButtonViewInfo }

  TdxTileControlDetailSiteTabsScrollButtonViewInfo = class(TdxTileControlCustomCellViewInfo,
    IcxMouseTrackingCaller, IcxMouseTrackingCaller2)
  private
    FDirection: TcxArrowDirection;
    FOwner: TdxTileControlDetailSiteTitleViewInfo;
    FState: TcxButtonState;
    FTimer: TcxTimer;
    FTimerIsHandled : Boolean;
    function GetDetailSite: TdxTileControlDetailSite;
    procedure SetState(const AValue: TcxButtonState);
    procedure TimerHandler(Sender: TObject);
  protected
    function CanClick: Boolean; virtual;
    procedure Click;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoScrollTabs; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetNearestTabBounds: TRect; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetVisibleBounds: TRect; override;
    procedure Scroll(const DX, DY: Integer); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    // IcxMouseTrackingCaller
    procedure MouseLeave; virtual;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;

    property Timer: TcxTimer read FTimer;
  public
    constructor Create(AOwner: TdxTileControlDetailSiteTitleViewInfo; ADirection: TcxArrowDirection); virtual;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure RefreshState; override;

    property DetailSite: TdxTileControlDetailSite read GetDetailSite;
    property Direction: TcxArrowDirection read FDirection;
    property Owner: TdxTileControlDetailSiteTitleViewInfo read FOwner;
    property State: TcxButtonState read FState write SetState;
  end;

  { TdxTileControlTabsScrollingAnimation }

  TdxTileControlTabsScrollingAnimation  = class(TdxAnimationTransition)
  private
    FOwner: TdxTileControlDetailSiteTitleViewInfo;
    FSign: Integer;
    FPrevPosition: Integer;
    function  GetDetailSite: TdxTileControlDetailSite;
  protected
    procedure DoAnimate; override;

    property DetailSite: TdxTileControlDetailSite read GetDetailSite;
    property Owner: TdxTileControlDetailSiteTitleViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxTileControlDetailSiteTitleViewInfo; ATime: Cardinal; const ALength: Integer); reintroduce; virtual;
  end;

  { TdxTileControlDetailSiteTitleViewInfo }

  TdxTileControlDetailSiteTitleViewInfo = class(TdxTileControlTitleViewInfo)
  private
    FBackButtonViewInfo: TdxTileControlDetailSiteBackButtonViewInfo;
    FOwner: TdxTileControlDetailSite;
    FScrollTabsLeftButton: TdxTileControlDetailSiteTabsScrollButtonViewInfo;
    FScrollTabsRightButton: TdxTileControlDetailSiteTabsScrollButtonViewInfo;
    FTabs: TdxTileControlCells;
    FTabsBounds: TRect;
    FTabsFont: TFont;
    FTabsBottom: Integer;
    FTabsLeftScrollPos: Integer;
    FTabsTop: Integer;

    function GetItem: TdxTileControlItem;
    function GetTileControl: TdxCustomTileControl;
    procedure SetTabsLeftScrollPos(AValue: Integer);
  protected
    procedure CalculateBackButton; virtual;
    procedure CalculateTabs; virtual;
    function CanScrollTabToLeft: Boolean;
    function CanScrollTabToRight: Boolean;
    procedure CheckBiDiModeAlignment; virtual;
    function CreateBackButtonViewInfo: TdxTileControlDetailSiteBackButtonViewInfo; virtual;
    procedure CreateScrollTabsButtons; virtual;
    function CreateTabCellViewInfo(AItem: TdxTileControlItem): TdxTileControlPageTabCellViewInfo; virtual;
    procedure DestroyScrollTabsButtons; virtual;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoDrawScrollTabsButtons(ACanvas: TcxCanvas); virtual;
    function GetHeight: Integer; override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetTabsScrollButtonsHitTest(AHitTest: TdxTileControlHitTest): Boolean; virtual;
    function GetVisibleBounds: TRect; override;
    procedure ScrollTabs(const DX: Integer); virtual;
    procedure ScrollTabsButtonsRefreshState;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;

    property TabsLeftScrollPos: Integer read FTabsLeftScrollPos write SetTabsLeftScrollPos;
  public
    constructor Create(AOwner: TdxTileControlDetailSite); virtual;
    destructor Destroy; override;
    procedure RefreshState; override;

    property BackButtonViewInfo: TdxTileControlDetailSiteBackButtonViewInfo read FBackButtonViewInfo;
    property Item: TdxTileControlItem read GetItem;
    property Owner: TdxTileControlDetailSite read FOwner;
    property ScrollTabsLeftButton: TdxTileControlDetailSiteTabsScrollButtonViewInfo read FScrollTabsLeftButton;
    property ScrollTabsRightButton: TdxTileControlDetailSiteTabsScrollButtonViewInfo read FScrollTabsRightButton;
    property Tabs: TdxTileControlCells read FTabs;
    property TabsBounds: TRect read FTabsBounds;
    property TabsFont: TFont read FTabsFont;
    property TabsBottom: Integer read FTabsBottom;
    property TabsTop: Integer read FTabsTop;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlCustomPopupControl }

  TdxTileControlCustomPopupControl = class(TcxControl, IcxMouseTrackingCaller, IcxMouseTrackingCaller2)
  private
    FActivatingInProgress: Boolean;
  protected
    procedure BoundsChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateSubClasses; virtual; abstract;
    procedure DestroySubClasses; virtual; abstract;
    function GetActiveControl: TWinControl; virtual;
    function GetHitTest: TdxTileControlHitTest; virtual;
    function GetTileControl: TdxCustomTileControl; virtual;
    procedure RecalculateSubClasses; virtual; abstract;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = ReallyMouseLeave;
    procedure IcxMouseTrackingCaller2.MouseLeave = ReallyMouseLeave;
    procedure ReallyMouseLeave; virtual;

    procedure ExecuteActivating; virtual;
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    //
    property ActivatingInProgress: Boolean read FActivatingInProgress;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActiveControl: TWinControl read GetActiveControl;
    property HitTest: TdxTileControlHitTest read GetHitTest;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlDetailSite }

  TdxTileControlDetailSite = class(TdxTileControlCustomPopupControl)
  private
    FPressedCell: TdxTileControlCustomCellViewInfo;
    FTitleViewInfo: TdxTileControlDetailSiteTitleViewInfo;

    function GetTileItem: TdxTileControlItem;
    procedure SetPressedCell(AValue: TdxTileControlCustomCellViewInfo);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    procedure DoPaint; override;
    function GetActiveControl: TWinControl; override;
    function GetHitTest: TdxTileControlHitTest; override;
    function GetIndex: Integer; virtual;
    function GetTileControl: TdxCustomTileControl; override;
    function GetTitleViewInfoClass: TdxTileControlDetailSiteTitleViewInfoClass; virtual;
    procedure InitializeSite(ALeft, ATop: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RecalculateSubClasses; override;
    // Gesture
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer); override;
    function GetCheckedDeltaX(const ADelta: Integer): Integer;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    //
    property PressedCell: TdxTileControlCustomCellViewInfo read FPressedCell write SetPressedCell;
    property TitleViewInfo: TdxTileControlDetailSiteTitleViewInfo read FTitleViewInfo;
  public
    property Index: Integer read GetIndex;
    property TileItem: TdxTileControlItem read GetTileItem;
  end;

  { TdxTileControlActionBarItem }

  TdxTileControlActionBarItemAlignment = (abiaLeft, abiaRight);
  TdxTileControlActionBarItemPosition = (abipTopBar, abipBottomBar);

  TdxTileControlActionBarItemEvent = procedure (Sender: TdxTileControlActionBarItem) of object;
  TdxTileControlActionBarVisibilityChangeEvent = procedure (Sender: TdxCustomTileControl;
    AReason: TdxTileControlActionBarVisibilityChangeReason; var AHandled: Boolean) of object;

  TdxTileControlActionBarItem = class(TcxComponentCollectionItem)
  private
    FAlign: TdxTileControlActionBarItemAlignment;
    FCaption: string;
    FGlyphType: TImageType;
    FImage: TdxSkinImage;
    FPosition: TdxTileControlActionBarItemPosition;
    FVisible: Boolean;

    FOnClick: TdxTileControlActionBarItemEvent;

    function GetGlyph: TdxSmartImage;
    function GetGlyphFrameCount: Integer;
    procedure GlyphChanged(Sender: TObject);
    procedure SetAlign(AValue: TdxTileControlActionBarItemAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetGlyph(AValue: TdxSmartImage);
    procedure SetGlyphFrameCount(AValue: Integer);
    procedure SetGlyphType(AValue: TImageType);
    procedure SetPosition(AValue: TdxTileControlActionBarItemPosition);
    procedure SetVisible(AValue: Boolean);
  protected
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    //
    property Image: TdxSkinImage read FImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Click;
  published
    property Align: TdxTileControlActionBarItemAlignment read FAlign write SetAlign default abiaLeft;
    property Caption: string read FCaption write SetCaption;
    property Glyph: TdxSmartImage read GetGlyph write SetGlyph;
    property GlyphFrameCount: Integer read GetGlyphFrameCount write SetGlyphFrameCount default 1;
    property GlyphType: TImageType read FGlyphType write SetGlyphType default itMask;
    property Position: TdxTileControlActionBarItemPosition read FPosition write SetPosition default abipBottomBar;
    property Visible: Boolean read FVisible write SetVisible default True;
    //
    property OnClick: TdxTileControlActionBarItemEvent read FOnClick write FOnClick;
  end;

  { TdxTileControlActionBarItems }

  TdxTileControlActionBarItems = class(TcxComponentCollection)
  private
    function GetItem(AIndex: Integer): TdxTileControlActionBarItem;
    procedure SetItem(AIndex: Integer; AValue: TdxTileControlActionBarItem);
  protected
    function GetItemPrefixName: string; override;
  public
    function Add: TdxTileControlActionBarItem;
    //
    property Items[Index: Integer]: TdxTileControlActionBarItem read GetItem write SetItem; default;
  end;

  { TdxTileControlActionBarCustomItemViewInfo }

  TdxTileControlActionBarCustomItemViewInfo = class(TdxTileControlCustomButtonViewInfo)
  private
    FOwner: TdxTileControlCustomActionBarViewInfo;
    function GetController: TdxTileControlActionBarController;
    function GetPainter: TdxTileControlPainter;
  protected
    FGlyphBounds: TRect;
    function GetFont: TFont; virtual;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetText: string; virtual;
    function GetTextBounds: TRect; virtual;
    function GetTextColor: TColor; virtual;
    procedure DoCalculate; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    constructor Create(AOwner: TdxTileControlCustomActionBarViewInfo);
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure Invalidate; override;
    procedure RefreshState; override;
    //
    property Controller: TdxTileControlActionBarController read GetController;
    property Font: TFont read GetFont;
    property Owner: TdxTileControlCustomActionBarViewInfo read FOwner;
    property Painter: TdxTileControlPainter read GetPainter;
    property Text: string read GetText;
    property TextBounds: TRect read GetTextBounds;
    property TextColor: TColor read GetTextColor;
  end;

  { TdxTileControlCustomActionBarViewInfo }

  TdxTileControlCustomActionBarViewInfo = class(TdxTileControlCustomViewInfo)
  private
    FCells: TdxTileControlCells;
    FOwner: TdxTileControlCustomActionBar;
    function GetActionBars: TdxTileControlActionBars;
    function GetItems: TdxTileControlActionBarItems;
    function GetPainter: TdxTileControlPainter;
  protected
    procedure CalculateItems(ABounds: TRect); virtual;
    procedure CheckBiDiModeAlignment; virtual;
    function CreateItemViewInfo(AItem: TdxTileControlActionBarItem): TdxTileControlActionBarCustomItemViewInfo; virtual;
    function GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean; virtual;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
  public
    constructor Create(AOwner: TdxTileControlCustomActionBar); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Clear; virtual;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    function MeasureHeight: Integer; virtual;
    procedure RefreshState; override;
    //
    property ActionBars: TdxTileControlActionBars read GetActionBars;
    property Cells: TdxTileControlCells read FCells;
    property Items: TdxTileControlActionBarItems read GetItems;
    property Owner: TdxTileControlCustomActionBar read FOwner;
    property Painter: TdxTileControlPainter read GetPainter;
  end;

  { TdxTileControlActionBarItemViewInfo }

  TdxTileControlActionBarItemViewInfo = class(TdxTileControlActionBarCustomItemViewInfo)
  private
    FItem: TdxTileControlActionBarItem;
  protected
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetText: string; override;
    function GetTexture: TdxSkinImage; override;
    function GetTextureType: TImageType; override;
  public
    constructor Create(AOwner: TdxTileControlCustomActionBarViewInfo; AItem: TdxTileControlActionBarItem);
    //
    property Item: TdxTileControlActionBarItem read FItem;
  end;

  { TdxTileControlActionBarHitTest }

  TdxTileControlActionBarHitTest = class(TdxTileControlHitTest)
  private
    FOwner: TdxTileControlCustomActionBar;
  protected
    function GetActiveViewInfo: TdxTileControlCustomViewInfo; override;
  public
    constructor Create(AOwner: TdxTileControlCustomActionBar);
    //
    property Owner: TdxTileControlCustomActionBar read FOwner;
  end;

  { TdxTileControlActionBarController }

  TdxTileControlActionBarController = class(TcxIUnknownObject, IcxMouseTrackingCaller, IcxMouseTrackingCaller2)
  private
    FHotCell: TObject;
    FOwner: TdxTileControlCustomActionBar;
    FPressedCell: TObject;
    function GetHitTest: TdxTileControlActionBarHitTest;
    function GetTileControl: TdxCustomTileControl;
    procedure SetHotCell(AValue: TObject);
    procedure SetPressedCell(AValue: TObject);
  protected
    procedure ProcessMouseLeftButtonClick; virtual;
    procedure UncheckAllItems;
  public
    constructor Create(AOwner: TdxTileControlCustomActionBar); virtual;
    destructor Destroy; override;
    procedure HideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
    procedure RefreshStates;
    // Mouse
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual; // IcxMouseTrackingCaller
    procedure MouseMove(AShift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    function PtInCaller(const P: TPoint): Boolean;  // IcxMouseTrackingCaller2
    //
    property HitTest: TdxTileControlActionBarHitTest read GetHitTest;
    property HotCell: TObject read FHotCell write SetHotCell;
    property Owner: TdxTileControlCustomActionBar read FOwner;
    property PressedCell: TObject read FPressedCell write SetPressedCell;
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileControlCustomActionBar }

  TdxTileControlCustomActionBar = class(TcxControl)
  private
    FController: TdxTileControlActionBarController;
    FHitTest: TdxTileControlActionBarHitTest;
    FTileControl: TdxCustomTileControl;
    FViewInfo: TdxTileControlCustomActionBarViewInfo;
  protected
    procedure BoundsChanged; override;
    function CreateController: TdxTileControlActionBarController; virtual;
    function CreateHitTest: TdxTileControlActionBarHitTest; virtual;
    function CreateViewInfo: TdxTileControlCustomActionBarViewInfo; virtual; abstract;
    procedure DoPaint; override;
    procedure Recalculate;
    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(ATileControl: TdxCustomTileControl); reintroduce; virtual;
    destructor Destroy; override;
    //
    property Controller: TdxTileControlActionBarController read FController;
    property HitTest: TdxTileControlActionBarHitTest read FHitTest;
    property TileControl: TdxCustomTileControl read FTileControl;
    property ViewInfo: TdxTileControlCustomActionBarViewInfo read FViewInfo;
  end;

  { TdxTileControlBottomActionBar }

  TdxTileControlBottomActionBar = class(TdxTileControlCustomActionBar)
  protected
    function CreateViewInfo: TdxTileControlCustomActionBarViewInfo; override;
  end;

  { TdxTileControlBottomActionBarViewInfo }

  TdxTileControlBottomActionBarViewInfo = class(TdxTileControlCustomActionBarViewInfo)
  protected
    function GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean; override;
  end;

  { TdxTileControlTopActionBar }

  TdxTileControlTopActionBar = class(TdxTileControlCustomActionBar)
  protected
    function CreateViewInfo: TdxTileControlCustomActionBarViewInfo; override;
  end;

  { TdxTileControlTopActionBarViewInfo }

  TdxTileControlTopActionBarViewInfo = class(TdxTileControlCustomActionBarViewInfo)
  protected
    function CreateBackButtonViewInfo: TdxTileControlActionBarCustomItemViewInfo; virtual;
    function GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean; override;
    procedure CalculateItems(ABounds: TRect); override;
  end;

  { TdxTileControlTopActionBarBackButtonViewInfo }

  TdxTileControlTopActionBarBackButtonViewInfo = class(TdxTileControlActionBarCustomItemViewInfo)
  protected
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetTexture: TdxSkinImage; override;
  end;

  { TdxTileControlActionBars }

  TdxTileControlActionBars = class(TcxOwnedPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FFontChanged: Boolean;
    FIndentHorz: Integer;
    FIndentVert: Integer;
    FItemIndent: Integer;
    FItems: TdxTileControlActionBarItems;
    procedure FontChanged(Sender: TObject);
    function GetTileControl: TdxCustomTileControl;
    function IsFontStored: Boolean;
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetIndentHorz(AValue: Integer);
    procedure SetIndentVert(AValue: Integer);
    procedure SetItemIndent(AValue: Integer);
    procedure SetItems(AValue: TdxTileControlActionBarItems);
  protected
    function CreateItems: TdxTileControlActionBarItems; virtual;
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure ItemsChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); virtual;
    //
    property TileControl: TdxCustomTileControl read GetTileControl;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IndentHorz: Integer read FIndentHorz write SetIndentHorz default dxTileControlDefaultActionBarsIndentHorz;
    property IndentVert: Integer read FIndentVert write SetIndentVert default dxTileControlDefaultActionBarsIndentVert;
    property ItemIndent: Integer read FItemIndent write SetItemIndent default dxTileControlDefaultActionBarsItemIndent;
    property Items: TdxTileControlActionBarItems read FItems write SetItems;
  end;

  { TdxCustomTileControl }

  TdxTileControlChange = (tccLayout, tccSelection, tccItems);
  TdxTileControlChanges = set of TdxTileControlChange;

  TdxTileControlGroupDragBeginEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragGroupInfo; var AAllow: Boolean) of object;
  TdxTileControlGroupDragEndEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragGroupInfo) of object;
  TdxTileControlGroupDragOverEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragGroupInfo; var AAccept: Boolean) of object;

  TdxTileControlItemDragBeginEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragItemInfo; var AAllow: Boolean) of object;
  TdxTileControlItemDragEndEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragItemInfo) of object;
  TdxTileControlItemDragOverEvent = procedure(Sender: TdxCustomTileControl; AInfo: TdxTileControlDragItemInfo; var AAccept: Boolean) of object;

  TdxTileControlItemFocusChange = procedure(Sender: TdxCustomTileControl; AFocusedItem, ANewFocusedItem: TdxTileControlItem) of object;

  TdxCustomTileControl = class(TcxControl, IcxStoredParent, IcxStoredObject, IdxSkinSupport)
  private
    FActionBarBottom: TdxTileControlCustomActionBar;
    FActionBars: TdxTileControlActionBars;
    FActionBarTop: TdxTileControlCustomActionBar;
    FActiveDetail: TdxTileControlDetailSite;
    FActiveHitTestPos: TPoint;
    FAnimation: TdxImageAnimationTransition;
    FAssets: TdxTileControlAssets;
    FChanges: TdxTileControlChanges;
    FController: TdxTileControlController;
    FDetailsAnimationInProcess: Boolean;
    FDragGroupInfo: TdxTileControlDragGroupInfo;
    FDragItemInfo: TdxTileControlDragItemInfo;
    FForceCalculate: Boolean;
    FGroups: TdxTileControlGroupCollection;
    FHitTest: TdxTileControlHitTest;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FImagesNotifyComponent: TcxFreeNotificator;
    FIsRestoring: Boolean;
    FItems: TdxTileControlItemCollection;
    FJustLoaded: Boolean;
    FLockCount: Integer;
    FOptionsBehavior: TdxTileControlOptionsBehavior;
    FOptionsDetailAnimate: TdxTileControlDetailOptionsAnimate;
    FOptionsItemAnimate: TdxTileControlCustomItemOptionsAnimate;
    FOptionsView: TdxTileControlOptionsView;
    FPainter: TdxTileControlPainter;
    FRightButtonPressed: Boolean;
    FScalingInProcess: Boolean;
    FStoredVersion: Integer;
    FStoringName: string;
    FStyle: TdxTileControlStyle;
    FTitle: TdxTileControlTitle;
    FTitleChanged: Boolean;
    FViewInfo: TdxTileControlViewInfo;

    FOnActionBarsHide: TdxTileControlActionBarVisibilityChangeEvent;
    FOnActionBarsShow: TdxTileControlActionBarVisibilityChangeEvent;
    FOnGroupDragBegin: TdxTileControlGroupDragBeginEvent;
    FOnGroupDragEnd: TdxTileControlGroupDragEndEvent;
    FOnGroupDragOver: TdxTileControlGroupDragOverEvent;
    FOnItemActivateDetail: TdxTileControlItemOperationEvent;
    FOnItemBeforeCheck: TdxTileControlItemAllowOperationEvent;
    FOnItemCheck: TdxTileControlItemOperationEvent;
    FOnItemDeactivateDetail: TdxTileControlItemOperationEvent;
    FOnItemDeactivatingDetail: TdxTileControlItemAllowOperationEvent;
    FOnItemDragBegin: TdxTileControlItemDragBeginEvent;
    FOnItemDragEnd: TdxTileControlItemDragEndEvent;
    FOnItemDragOver: TdxTileControlItemDragOverEvent;
    FOnItemFocusChanging: TdxTileControlItemAllowOperationEvent;
    FOnItemFocusChange: TdxTileControlItemFocusChange;

    // IcxStoredObject events
    FOnGetStoredProperties: TcxGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent;
    FOnInitStoredObject: TcxInitStoredObjectEvent;
    FOnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent;

    procedure AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure BeforeActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    function GetCheckedItem(Index: Integer): TdxTileControlItem;
    function GetCheckedItemCount: Integer;
    function GetDraggedGroup: TdxTileControlGroup;
    function GetDraggedItem: TdxTileControlItem;
    function GetIsLocked: Boolean;
    procedure InitializeDragGroupInfo;
    procedure InitializeDragItemInfo;
    function IsGroupDragged: Boolean;
    function IsItemDragged: Boolean;
    function IsStyleStored: Boolean;
    function IsTitleStored: Boolean;
    procedure RefreshItem(AItem: TdxTileControlItem; const AData: Pointer);
    procedure SetActionBars(AValue: TdxTileControlActionBars);
    procedure SetActiveDetail(AValue: TdxTileControlDetailSite);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetOptionsDetailAnimate(AValue: TdxTileControlDetailOptionsAnimate);
    procedure SetOptionsItemAnimate(AValue: TdxTileControlCustomItemOptionsAnimate);
    procedure SetStyle(AValue: TdxTileControlStyle);
    procedure SetTitle(AValue: TdxTileControlTitle);
    // scroll
    procedure ScrollHorz(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
    procedure ScrollVert(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxStoredParent
    function IcxStoredParent.CreateChild = StoredCreateChild;
    procedure IcxStoredParent.DeleteChild = StoredDeleteChild;
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    // Messages
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMHideGroupCaptionEditors(var Message: TMessage); message DXM_TILECONTROL_HIDEGROUPCAPTIONEDITORS;
  protected
    FCheckedItems: TdxTileControlCheckedItems;
    function ActiveDetailOccupiesAllTileControl: Boolean; virtual;
    procedure AddChanges(AChanges: TdxTileControlChanges);
    procedure AfterActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite); virtual;
    function AllowTouchScrollUIMode: Boolean; override;
    procedure AssignActiveDetail(ANewDetail: TdxTileControlDetailSite);
    procedure BoundsChanged; override;
    procedure BringInternalControlsToFront; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckChanges; virtual;
    function CreateActionBarBottom: TdxTileControlCustomActionBar; virtual;
    function CreateActionBars: TdxTileControlActionBars; virtual;
    function CreateActionBarTop: TdxTileControlCustomActionBar; virtual;
    function CreateAssets: TdxTileControlAssets; virtual;
    function CreateController: TdxTileControlController; virtual;
    function CreateGroupsCollection: TdxTileControlGroupCollection; virtual;
    function CreateHitTest: TdxTileControlHitTest; virtual;
    function CreateImagesChangeLink(AOnChange: TNotifyEvent): TChangeLink;
    function CreateImagesFreeNotificator(AOnFree: TcxFreeNotificationEvent): TcxFreeNotificator;
    function CreateItemsCollection: TdxTileControlItemCollection; virtual;
    function CreatePainter: TdxTileControlPainter; virtual;
    function CreateStyle: TdxTileControlStyle; virtual;
    procedure CreateSubClasses; virtual;
    function CreateTitle: TdxTileControlTitle; virtual;
    function CreateViewInfo: TdxTileControlViewInfo; virtual;
    procedure CreateWnd; override;
    procedure DestroySubClasses; virtual;
    procedure DblClick; override;

    function GetAnimationScrollFadeMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode; virtual;
    function GetAnimationScrollMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode; virtual;

    function CanDeactivateDetail(ADetail: TdxTileControlDetailSite): Boolean; virtual;
    function DoActionBarsHide(AReason: TdxTileControlActionBarVisibilityChangeReason): Boolean; virtual;
    function DoActionBarsShow(AReason: TdxTileControlActionBarVisibilityChangeReason): Boolean; virtual;
    procedure DoActivateDetail(ADetail: TdxTileControlDetailSite); virtual;
    procedure DoCancelMode; override;
    procedure DoDeactivateDetail(ADetail: TdxTileControlDetailSite); virtual;
    procedure DoGroupDragBegin(var AAllow: Boolean); dynamic;
    procedure DoGroupDragEnd; dynamic;
    procedure DoGroupDragOver(var AAccept: Boolean); dynamic;
    procedure DoInitStoredObject(AObject: TObject); virtual;
    function DoItemBeforeCheck(AItem: TdxTileControlItem): Boolean; dynamic;
    procedure DoItemCheck(AItem: TdxTileControlItem); dynamic;
    procedure DoItemDragBegin(var AAllow: Boolean); dynamic;
    procedure DoItemDragEnd; dynamic;
    procedure DoItemDragOver(var AAccept: Boolean); dynamic;
    procedure ExecuteDetailDeactivating(ADetail: TdxTileControlDetailSite); virtual;

    procedure DoCustomPaint; virtual;
    procedure DoPaint; override;

    procedure SetOptionsBehavior(AValue: TdxTileControlOptionsBehavior); virtual;
    procedure SetOptionsView(AValue: TdxTileControlOptionsView); virtual;

    // ActionBars
    function IsActionBarTopVisible: Boolean; virtual;
    function IsActionBarBottomVisible: Boolean; virtual;
    procedure DoHideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason); virtual;
    procedure DoShowActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason); virtual;

    procedure CheckDetailLayout(ADetail: TdxTileControlDetailSite); virtual;
    procedure CheckTitleChanges; virtual;
    procedure DrawScrollBars(ACanvas: TcxCanvas); override;
    function FindGroup(const AName: string): TdxTileControlGroup;
    procedure FocusChanged; override;
    procedure ForceUpdate(AUpdateAll: Boolean = False);
    function GetActionBarBottomBounds: TRect; virtual;
    function GetActionBarTopBounds: TRect; virtual;
    function GetActiveHitTest: TdxTileControlHitTest; virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetHScrollBarBounds: TRect; override;
    function GetVScrollBarBounds: TRect; override;
    function GetScrollStep: Integer; virtual;
    function GetOptionsBehaviorClass: TdxTileControlOptionsBehaviorClass; virtual;
    function GetOptionsViewClass: TdxTileControlOptionsViewClass; virtual;

    function GetLastVisibleGroup: TdxTileControlGroup;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    procedure ImagesChanged(Sender: TObject); virtual;
    procedure ImagesFreeNotification(AComponent: TComponent);
    procedure InvalidateChanges; virtual;
    function IsDestroying: Boolean;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsLoading: Boolean;
    procedure InitializeAlign; virtual;
    procedure InitScrollBarsParameters; override;
    function InsertGroup(AIndex: Integer): TdxTileControlGroup;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoLoaded; virtual;
    procedure Loaded; override;
    procedure LockTimers;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedStopAnimationsBeforeActiveDetailChanging(AActiveDetail: TdxTileControlDetailSite): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReCreateViewInfo;
    procedure RefreshActionBars;
    procedure RefreshItems;
    procedure RemoveGroupIfEmpty(AGroup: TdxTileControlGroup);
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetAutoSize(Value: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure StyleChanged(Sender: TObject);
    procedure TitleChanged;
    procedure TransparentChanged; override;
    procedure UnlockTimers;
    procedure ValidateChanges;

    // used classes
    function GetGroupsClass: TdxTileControlGroupCollectionClass;
    function GetItemsClass: TdxTileControlItemCollectionClass;

    // store-restore layout
    procedure RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string;
      AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreName: string);
    procedure StoreTo(AStorageType: TcxStorageType; const AStorageName: string;
      AStorageStream: TStream; AReCreate: Boolean; const ASaveName: string);

    // cxStorage implementation
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    function GetStoredObjectName: string; virtual;
    function GetStoredObjectProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;
    function StoredCreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure StoredDeleteChild(const AObjectName: string; AObject: TObject); virtual;

    property ActionBarBottom: TdxTileControlCustomActionBar read FActionBarBottom;
    property ActionBarTop: TdxTileControlCustomActionBar read FActionBarTop;
    property ActiveHitTestPos: TPoint read FActiveHitTestPos;
    property Animation: TdxImageAnimationTransition read FAnimation;
    property Assets: TdxTileControlAssets read FAssets;
    property Changes: TdxTileControlChanges read FChanges write FChanges;
    property DetailsAnimationInProcess: Boolean read FDetailsAnimationInProcess;
    property DraggedGroup: TdxTileControlGroup read GetDraggedGroup;
    property DraggedItem: TdxTileControlItem read GetDraggedItem;
    property DragGroupInfo: TdxTileControlDragGroupInfo read FDragGroupInfo;
    property DragItemInfo: TdxTileControlDragItemInfo read FDragItemInfo;
    property ForceCalculate: Boolean read FForceCalculate write FForceCalculate;
    property HitTest: TdxTileControlHitTest read FHitTest;
    property IsLocked: Boolean read GetIsLocked;
    property IsRestoring: Boolean read FIsRestoring write FIsRestoring;
    property JustLoaded: Boolean read FJustLoaded;
    property LockCount: Integer read FLockCount write FLockCount;
    property Painter: TdxTileControlPainter read FPainter;
    property StoringName: string read FStoringName write FStoringName;
    property ViewInfo: TdxTileControlViewInfo read FViewInfo;
    // IcxStoredObject events
    property OnGetStoredProperties: TcxGetStoredPropertiesEvent read FOnGetStoredProperties write FOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGetStoredPropertyValueEvent read FOnGetStoredPropertyValue write FOnGetStoredPropertyValue;
    property OnInitStoredObject: TcxInitStoredObjectEvent read FOnInitStoredObject write FOnInitStoredObject;
    property OnSetStoredPropertyValue: TcxSetStoredPropertyValueEvent read FOnSetStoredPropertyValue write FOnSetStoredPropertyValue;

    property OnActionBarsHide: TdxTileControlActionBarVisibilityChangeEvent read FOnActionBarsHide write FOnActionBarsHide;
    property OnActionBarsShow: TdxTileControlActionBarVisibilityChangeEvent read FOnActionBarsShow write FOnActionBarsShow;
    property OnGroupDragBegin: TdxTileControlGroupDragBeginEvent read FOnGroupDragBegin write FOnGroupDragBegin;
    property OnGroupDragEnd: TdxTileControlGroupDragEndEvent read FOnGroupDragEnd write FOnGroupDragEnd;
    property OnGroupDragOver: TdxTileControlGroupDragOverEvent read FOnGroupDragOver write FOnGroupDragOver;
    property OnItemActivateDetail: TdxTileControlItemOperationEvent read FOnItemActivateDetail write FOnItemActivateDetail;
    property OnItemBeforeCheck: TdxTileControlItemAllowOperationEvent read FOnItemBeforeCheck write FOnItemBeforeCheck;
    property OnItemCheck: TdxTileControlItemOperationEvent read FOnItemCheck write FOnItemCheck;
    property OnItemDeactivateDetail: TdxTileControlItemOperationEvent read FOnItemDeactivateDetail write FOnItemDeactivateDetail;
    property OnItemDeactivatingDetail: TdxTileControlItemAllowOperationEvent read FOnItemDeactivatingDetail write FOnItemDeactivatingDetail;
    property OnItemDragBegin: TdxTileControlItemDragBeginEvent read FOnItemDragBegin write FOnItemDragBegin;
    property OnItemDragEnd: TdxTileControlItemDragEndEvent read FOnItemDragEnd write FOnItemDragEnd;
    property OnItemDragOver: TdxTileControlItemDragOverEvent read FOnItemDragOver write FOnItemDragOver;
    property OnItemFocusChange: TdxTileControlItemFocusChange read FOnItemFocusChange write FOnItemFocusChange;
    property OnItemFocusChanging: TdxTileControlItemAllowOperationEvent read FOnItemFocusChanging write FOnItemFocusChanging;

    property StoredVersion: Integer read FStoredVersion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CancelUpdate;
    function CreateGroup: TdxTileControlGroup;
    function CreateItem(AIsLarge: Boolean = False; AGroup: TdxTileControlGroup = nil): TdxTileControlItem; overload;
    function CreateItem(ASize: TdxTileControlItemSize; AGroup: TdxTileControlGroup = nil): TdxTileControlItem; overload;
    procedure DeleteItem(AItem: TdxTileControlItem);
    procedure EndUpdate;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsMouseInPressedArea(X, Y: Integer): Boolean; override;
    procedure LayoutChanged; virtual;
    procedure RemoveGroup(AGroup: TdxTileControlGroup);
    // ActionBars
    function IsAnyActionBarVisible: Boolean; virtual;
    procedure HideActionBars; virtual;
    procedure ShowActionBars; virtual;
    // storing layout
    procedure RestoreFromIniFile(const AStorageName: string; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreName: string = '');
    procedure RestoreFromRegistry(const AStorageName: string; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreName: string = '');
    procedure RestoreFromStream(AStream: TStream; AChildrenCreating: Boolean = False;
      AChildrenDeleting: Boolean = False; const ARestoreName: string = '');
    procedure StoreToIniFile(AStorageName: string; AReCreate: Boolean = True;
      const ASaveName: string = '');
    procedure StoreToRegistry(AStorageName: string; AReCreate: Boolean = True;
      const ASaveName: string = '');
    procedure StoreToStream(AStream: TStream; const ASaveName: string = '');

    property ActionBars: TdxTileControlActionBars read FActionBars write SetActionBars;
    property ActiveDetail: TdxTileControlDetailSite read FActiveDetail write SetActiveDetail;
    property ActiveHitTest: TdxTileControlHitTest read GetActiveHitTest;
    property CheckedItems[Index: Integer]: TdxTileControlItem read GetCheckedItem;
    property CheckedItemCount: Integer read GetCheckedItemCount;
    property Controller: TdxTileControlController read FController;
    property Groups: TdxTileControlGroupCollection read FGroups;
    property Items: TdxTileControlItemCollection read FItems;
    property Images: TCustomImageList read FImages write SetImages;
    property OptionsBehavior: TdxTileControlOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsDetailAnimate: TdxTileControlDetailOptionsAnimate read FOptionsDetailAnimate write SetOptionsDetailAnimate;
    property OptionsItemAnimate: TdxTileControlCustomItemOptionsAnimate read FOptionsItemAnimate write SetOptionsItemAnimate;
    property OptionsView: TdxTileControlOptionsView read FOptionsView write SetOptionsView;
    property Style: TdxTileControlStyle read FStyle write SetStyle stored IsStyleStored;
    property Title: TdxTileControlTitle read FTitle write SetTitle stored IsTitleStored;
    property Transparent;
  end;

var
  dxDesignHelperClass: TdxTileControlCustomDesignHelperClass = nil;

implementation

uses
  dxOffice11, RTLConsts, Variants, cxTextEdit, dxDPIAwareUtils;

{$R dxTileControl.res}


const
  dxTileControlActionBarsAnimationMaxStepCount = 100;
  dxTileControlPullDownThreshold = 35;

const
  DefaultFontName: string = 'Segoe UI';

  SubColumnSizes: array [Boolean] of Integer = (1, 2);

  DefaultTextAlignment: array [0..3] of TdxTileItemInnerObjectAlignment =
    (oaTopLeft, oaTopRight, oaBottomLeft, oaBottomRight);

  dxTileControlStoredItemPropertiesNames: array [0..5] of string =
    ('Visible', 'Size', 'GroupIndex', 'IndexInGroup', 'RowCount', 'Enabled');

  dxTileControlStoredGroupPropertiesNames: array [0..3] of string = ('Visible', 'Index', 'Enabled', 'Caption');

  dxMouseButtonToActionBarVisibilityChangeReason: array[TMouseButton] of TdxTileControlActionBarVisibilityChangeReason = (
    abvcrMouseLeftClick, abvcrMouseRightClick, abvcrMouseMiddleClick
  );

function dxGetBiDiModeTileItemInnerObjectAlignment(AAlign: TdxTileItemInnerObjectAlignment;
  AUseRightToLeftAlignment: Boolean): TdxTileItemInnerObjectAlignment;
begin
  Result := AAlign;
  if AUseRightToLeftAlignment then
    case AAlign of
      oaTopLeft     : Result := oaTopRight;
      oaTopRight    : Result := oaTopLeft;
      oaMiddleLeft  : Result := oaMiddleRight;
      oaMiddleRight : Result := oaMiddleLeft;
      oaBottomLeft  : Result := oaBottomRight;
      oaBottomRight : Result := oaBottomLeft;
    end;
end;

type
  TdxSmartImageAccess = class(TdxSmartImage);

  { TdxDetailControlInfo }

  TdxDetailControlInfo = class
  public
    Align: TAlign;
    Bounds: TRect;
    Control: TWinControl;
    Parent: TWinControl;
    RefCount: Integer;
  end;

  { TdxDetailControlsInfo }

  TdxDetailControlsInfo = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxDetailControlInfo;
  public
    procedure AddInfo(AControl: TWinControl; ATileItem: TdxTileControlItem);
    function FindByControl(AControl: TWinControl): TdxDetailControlInfo;
    procedure RemoveInfo(AControl: TWinControl; ATileItem: TdxTileControlItem);

    property Items[AIndex: Integer]: TdxDetailControlInfo read GetItem;
  end;

  { TdxTileControlActionsBarHelper }

  TdxTileControlActionsBarHelper = class(TObject)
  private
    FTileControls: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ProcessClickOnControl(AControl: TWinControl; AButton: TMouseButton);
    class procedure Register(ATileControl: TdxCustomTileControl);
    class procedure Unregister(ATileControl: TdxCustomTileControl);

    property TileControls: TList read FTileControls;
  end;

  { TdxTileControlDesignKeyDownHookHelper }

  TdxTileControlDesignKeyDownHookHelper = class(TObject)
  private
    FTileControls: TList;
    function CanCustomize(ATileControl: TdxCustomTileControl): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CanProcessKeyboardHook: Boolean;
    procedure ProcessKeyDownOnControl(ATileControl: TdxCustomTileControl; AKey: Word);
    class procedure Register(ATileControl: TdxCustomTileControl);
    class procedure Unregister(ATileControl: TdxCustomTileControl);

    property TileControls: TList read FTileControls;
  end;

  { TdxTileControlDragAndDropSecondaryDragPointHelper }

  TdxTileControlDragAndDropSecondaryDragPointHelper = class
  private
    FCount: Integer;
    FDragBounds: TRect;
    FGroupExpandedBounds: TRect;
    FIncludedSideLength: Integer;
    FIntersections: array of TRect;
    FSegments: array of TRect;
    FTileControl: TdxCustomTileControl;
  protected
    procedure CalculateIntersections;
    procedure CalculateSegmentsBounds;
    procedure CalculateSegmentsCountAndLength;
    function GetMaxIntersection: TRect;
  public
    constructor Create(ATileControl: TdxCustomTileControl; const ADragBounds, AGroupExpandedBounds: TRect); virtual;
    function GetSecondaryDragPoint: TPoint;
  end;

var
  DetailsInfo: TdxDetailControlsInfo;
  ActionsBarHelper: TdxTileControlActionsBarHelper = nil;
  DesignKeyDownHookHelper: TdxTileControlDesignKeyDownHookHelper = nil;
  ThreadScaler: TdxThreadScaler = nil;

procedure DestroyThreadScaler;
begin
  if Assigned(ThreadScaler) then
  begin
    ThreadScaler.Stop;
    FreeAndNil(ThreadScaler);
  end;
end;

procedure AddTileControlResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('dxTileControlDefaultGroupCaptionHint', @dxTileControlDefaultGroupCaptionHint);
end;

function GetDetailsInfo: TdxDetailControlsInfo;
begin
  if DetailsInfo = nil then
    DetailsInfo := TdxDetailControlsInfo.Create();
  Result := DetailsInfo;
end;

function GetThreadScaler: TdxThreadScaler;
begin
  if ThreadScaler = nil then
    ThreadScaler := TdxThreadScaler.Create;
  Result := ThreadScaler;
end;

function IsTouchEvent: Boolean;
const
  MouseOriginMask  = $FFFFFF80;
  MouseOriginTouch = $FF515780;
begin
  Result := GetMessageExtraInfo and MouseOriginMask = MouseOriginTouch;
end;

procedure dxTileControlKeyboardHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  I: Integer;
begin
  if (lParam > 0) and (DesignKeyDownHookHelper <> nil) and (DesignKeyDownHookHelper.CanProcessKeyboardHook) then
    for I := 0 to DesignKeyDownHookHelper.TileControls.Count - 1 do
      DesignKeyDownHookHelper.ProcessKeyDownOnControl(TdxCustomTileControl(DesignKeyDownHookHelper.TileControls[I]), wParam);
end;

procedure dxTileControlMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);

  procedure DoProcessClick(AButton: TMouseButton);
  begin
    if ActionsBarHelper <> nil then
      ActionsBarHelper.ProcessClickOnControl(FindControl(PMouseHookStruct(lParam).hwnd), AButton);
  end;

begin
  case wParam of
    WM_LBUTTONDOWN:
      DoProcessClick(mbLeft);
    WM_RBUTTONDOWN:
      DoProcessClick(mbRight);
    WM_MBUTTONDOWN:
      DoProcessClick(mbMiddle);
  end;
end;

procedure AdjustFontHeight(AFont: TFont; AScaleFactor: TdxScaleFactor; AStoredFontSize, ADefaultFontSize: Integer);
begin
  if AFont.Height = AScaleFactor.Apply(dxGetFontHeightForDefaultDPI(AStoredFontSize)) then
    AFont.Height := AScaleFactor.Apply(dxGetFontHeightForDefaultDPI(ADefaultFontSize));
end;

{ TdxTileControlActionsBarHelper }

constructor TdxTileControlActionsBarHelper.Create;
begin
  inherited Create;
  FTileControls := TList.Create;
  dxSetHook(htMouse, dxTileControlMouseHook);
end;

destructor TdxTileControlActionsBarHelper.Destroy;
begin
  dxReleaseHook(dxTileControlMouseHook);
  FreeAndNil(FTileControls);
  inherited Destroy;
end;

procedure TdxTileControlActionsBarHelper.ProcessClickOnControl(AControl: TWinControl; AButton: TMouseButton);

  function GetCurrentTileControl: TdxCustomTileControl;
  begin
    Result := nil;
    if AControl is TdxTileControlDetailSite then
      Result := TdxTileControlDetailSite(AControl).TileControl;
    if AControl is TdxCustomTileControl then
      Result := TdxCustomTileControl(AControl);
  end;

  procedure HideActionBarsForAnotherTileControls(ACurrentTileControl: TdxCustomTileControl);
  var
    I: Integer;
  begin
    for I := FTileControls.Count - 1 downto 0 do
    begin
      if FTileControls[I] <> ACurrentTileControl then
        TdxCustomTileControl(FTileControls[I]).DoHideActionBars(dxMouseButtonToActionBarVisibilityChangeReason[AButton]);
    end;
  end;

begin
  if not (AControl is TdxTileControlCustomActionBar) then
    HideActionBarsForAnotherTileControls(GetCurrentTileControl);
end;

class procedure TdxTileControlActionsBarHelper.Register(ATileControl: TdxCustomTileControl);
begin
  if ActionsBarHelper = nil then
    ActionsBarHelper := TdxTileControlActionsBarHelper.Create;
  ActionsBarHelper.TileControls.Add(ATileControl);
end;

class procedure TdxTileControlActionsBarHelper.Unregister(ATileControl: TdxCustomTileControl);
begin
  if ActionsBarHelper <> nil then
  begin
    ActionsBarHelper.TileControls.Remove(ATileControl);
    if ActionsBarHelper.TileControls.Count = 0 then
      FreeAndNil(ActionsBarHelper);
  end;
end;

{ TdxTileControlDesignKeyDownHookHelper }

constructor TdxTileControlDesignKeyDownHookHelper.Create;
begin
  inherited Create;
  FTileControls := TList.Create;
  dxSetHook(htKeyboard, dxTileControlKeyboardHook);
end;

destructor TdxTileControlDesignKeyDownHookHelper.Destroy;
begin
  dxReleaseHook(dxTileControlKeyboardHook);
  FreeAndNil(FTileControls);
  inherited Destroy;
end;

function TdxTileControlDesignKeyDownHookHelper.CanCustomize(ATileControl: TdxCustomTileControl): Boolean;
var
  AOwner: TWinControl;
begin
  AOwner := TWinControl(ATileControl.Owner);
  Result := AOwner.HandleAllocated and cxIsParentFocused(AOwner.Handle);
end;

function TdxTileControlDesignKeyDownHookHelper.CanProcessKeyboardHook: Boolean;
var
  I: Integer;
  ATileControl: TdxCustomTileControl;
begin
  Result := False;
  for I := 0 to DesignKeyDownHookHelper.TileControls.Count - 1 do
  begin
    ATileControl := TdxCustomTileControl(DesignKeyDownHookHelper.TileControls[I]);
    Result := CanCustomize(ATileControl) and
      not ATileControl.Controller.IsDesignPopupMenuPresent;
    if not Result then
      Break;
  end;
end;

procedure TdxTileControlDesignKeyDownHookHelper.ProcessKeyDownOnControl(ATileControl: TdxCustomTileControl; AKey: Word);
begin
  if AKey = VK_DELETE then
    ATileControl.Controller.DeleteSelectedObjects(nil);
end;

class procedure TdxTileControlDesignKeyDownHookHelper.Register(ATileControl: TdxCustomTileControl);
begin
  if DesignKeyDownHookHelper = nil then
    DesignKeyDownHookHelper := TdxTileControlDesignKeyDownHookHelper.Create;
  DesignKeyDownHookHelper.TileControls.Add(ATileControl);
end;

class procedure TdxTileControlDesignKeyDownHookHelper.Unregister(ATileControl: TdxCustomTileControl);
begin
  if DesignKeyDownHookHelper <> nil then
  begin
    DesignKeyDownHookHelper.TileControls.Remove(ATileControl);
    if DesignKeyDownHookHelper.TileControls.Count = 0 then
      FreeAndNil(DesignKeyDownHookHelper);
  end;
end;

{ TdxDetailControlsInfo }

procedure TdxDetailControlsInfo.AddInfo(AControl: TWinControl; ATileItem: TdxTileControlItem);
var
  AInfo: TdxDetailControlInfo;
begin
  AControl.FreeNotification(ATileItem);
  AInfo := FindByControl(AControl);
  try
    if AInfo = nil then
    begin
      AInfo := TdxDetailControlInfo.Create;
      Add(AInfo);
      AInfo.Control := AControl;
      AInfo.Align := AControl.Align;
      AInfo.Parent := AControl.Parent;
      AInfo.Bounds := AControl.BoundsRect;
    end;
  finally
    Inc(AInfo.RefCount);
  end;
end;

function TdxDetailControlsInfo.FindByControl(AControl: TWinControl): TdxDetailControlInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Control = AControl then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

procedure TdxDetailControlsInfo.RemoveInfo(AControl: TWinControl; ATileItem: TdxTileControlItem);
var
  AInfo: TdxDetailControlInfo;
begin
  AControl.RemoveFreeNotification(ATileItem);
  AInfo := FindByControl(AControl);
  if AInfo = nil then Exit;
  Dec(AInfo.RefCount);
  if AInfo.RefCount = 0 then
  try
    Remove(AInfo);
    if not (csDestroying in AControl.ComponentState) then
    begin
      AControl.Parent := AInfo.Parent;
      AControl.Align := AInfo.Align;
      AControl.BoundsRect := AInfo.Bounds;
    end;
  finally
    AInfo.Free;
  end;
end;

function TdxDetailControlsInfo.GetItem(AIndex: Integer): TdxDetailControlInfo;
begin
  Result := TdxDetailControlInfo(inherited Items[AIndex]);
end;

{ TdxTileControlDragAndDropSecondaryDragPointHelper }

constructor TdxTileControlDragAndDropSecondaryDragPointHelper.Create(ATileControl: TdxCustomTileControl;
  const ADragBounds, AGroupExpandedBounds: TRect);
begin
  inherited Create;
  FTileControl := ATileControl;
  FDragBounds := ADragBounds;
  FGroupExpandedBounds := AGroupExpandedBounds;
end;

procedure TdxTileControlDragAndDropSecondaryDragPointHelper.CalculateIntersections;
var
  I: Integer;
begin
  SetLength(FIntersections, FCount);
  for I := 0 to FCount - 1 do
    cxRectIntersect(FIntersections[I], FSegments[I], FDragBounds);
end;

procedure TdxTileControlDragAndDropSecondaryDragPointHelper.CalculateSegmentsCountAndLength;
begin
  if FTileControl.OptionsView.GroupLayout = glHorizontal then
  begin
    FIncludedSideLength := cxRectWidth(FDragBounds);
    FCount := Round(cxRectWidth(FGroupExpandedBounds) / FIncludedSideLength);
  end
  else
  begin
    FIncludedSideLength := cxRectHeight(FDragBounds);
    FCount := Round(cxRectHeight(FGroupExpandedBounds) / FIncludedSideLength);
  end;
  if FCount = 0 then
  begin
    FCount := 1;
    FIncludedSideLength := 0;
  end;
end;

procedure TdxTileControlDragAndDropSecondaryDragPointHelper.CalculateSegmentsBounds;
var
  I: Integer;
begin
  SetLength(FSegments, FCount);
  for I := 0 to FCount - 1 do
    if FTileControl.OptionsView.GroupLayout = glHorizontal then
    begin
      FSegments[I].TopLeft := cxPointOffset(FGroupExpandedBounds.TopLeft, FIncludedSideLength * I, 0);
      FSegments[I].BottomRight := cxPointOffset(FGroupExpandedBounds.BottomRight, -FIncludedSideLength * (FCount - I - 1), 0);
    end
    else
    begin
      FSegments[I].TopLeft := cxPointOffset(FGroupExpandedBounds.TopLeft, 0, FIncludedSideLength * I);
      FSegments[I].BottomRight := cxPointOffset(FGroupExpandedBounds.BottomRight, 0, -FIncludedSideLength * (FCount - I - 1));
    end;
end;

function TdxTileControlDragAndDropSecondaryDragPointHelper.GetMaxIntersection: TRect;
var
  I: Integer;
  ASquare: Integer;
begin
  Result := FIntersections[0];
  ASquare := cxRectSquare(Result);
  for I := 1 to FCount - 1 do
    if not cxRectIsEqual(FIntersections[I], cxNullRect) and (cxRectSquare(FIntersections[I]) > ASquare) then
    begin
      Result := FIntersections[I];
      ASquare := cxRectSquare(Result);
    end;
end;

function TdxTileControlDragAndDropSecondaryDragPointHelper.GetSecondaryDragPoint: TPoint;
begin
  CalculateSegmentsCountAndLength;
  CalculateSegmentsBounds;
  CalculateIntersections;
  Result := cxRectCenter(GetMaxIntersection);
end;

{ TdxTileControlActionBarCustomItemViewInfo }

constructor TdxTileControlActionBarCustomItemViewInfo.Create(AOwner: TdxTileControlCustomActionBarViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxTileControlActionBarCustomItemViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  FGlyphBounds := cxRectSetHeight(cxRectCenterHorizontally(Bounds, ScaleFactor.Apply(Texture.Size.cx)),
    ScaleFactor.Apply(Texture.Size.cy));
end;

procedure TdxTileControlActionBarCustomItemViewInfo.Draw(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  Painter.DrawGlyph(ACanvas, FGlyphBounds, State, Texture, TextureType, TextColor);
  if Text <> '' then
  begin
    R := TextBounds;
    cxTextOut(ACanvas.Canvas, Text, R, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY, Font, 0, 0, 0, TextColor);
  end;
end;

procedure TdxTileControlActionBarCustomItemViewInfo.Invalidate;
begin
  if Visible then
    Owner.Owner.InvalidateRect(Bounds, False);
end;

function TdxTileControlActionBarCustomItemViewInfo.MeasureHeight: Integer;
begin
  Result := ScaleFactor.Apply(Texture.Size.cy);
  if Text <> '' then
    Inc(Result, cxTextHeight(Font, Text) + 2 * ScaleFactor.Apply(cxTextOffset));
end;

function TdxTileControlActionBarCustomItemViewInfo.MeasureWidth: Integer;
begin
  Result := ScaleFactor.Apply(Texture.Size.cx);
  if Text <> '' then
    Result := Max(Result, cxTextWidth(Font, Text));
end;

procedure TdxTileControlActionBarCustomItemViewInfo.RefreshState;
begin
  if (Controller.PressedCell = Self) and (Controller.HotCell = Self) then
    State := cxbsPressed
  else
    if (Controller.HotCell = Self) and (Controller.PressedCell = nil) then
      State := cxbsHot
    else
      State := cxbsNormal;
end;

procedure TdxTileControlActionBarCustomItemViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphBounds, AClientBounds);
  inherited DoRightToLeftConversion(AClientBounds);
end;

function TdxTileControlActionBarCustomItemViewInfo.GetController: TdxTileControlActionBarController;
begin
  Result := Owner.Owner.Controller;
end;

function TdxTileControlActionBarCustomItemViewInfo.GetFont: TFont;
begin
  Result := Owner.ActionBars.Font;
end;

function TdxTileControlActionBarCustomItemViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := Owner.Owner.TileControl.Painter;
end;

function TdxTileControlActionBarCustomItemViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxTileControlActionBarCustomItemViewInfo.GetText: string;
begin
  Result := '';
end;

function TdxTileControlActionBarCustomItemViewInfo.GetTextBounds: TRect;
begin
  Result := Bounds;
  Result.Top := FGlyphBounds.Bottom;
end;

function TdxTileControlActionBarCustomItemViewInfo.GetTextColor: TColor;
begin
  Result := cxGetActualColor(Font.Color, Painter.ActionBarDefaultTextColor);
end;

{ TdxTileControlActionBarItemViewInfo }

constructor TdxTileControlActionBarItemViewInfo.Create(
  AOwner: TdxTileControlCustomActionBarViewInfo; AItem: TdxTileControlActionBarItem);
begin
  inherited Create(AOwner);
  FItem := AItem;
end;

function TdxTileControlActionBarItemViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtActionButton] := True;
end;

function TdxTileControlActionBarItemViewInfo.GetText: string;
begin
  Result := Item.Caption;
end;

function TdxTileControlActionBarItemViewInfo.GetTexture: TdxSkinImage;
begin
  if Item.Image.Empty then
    Result := Owner.Owner.TileControl.Assets.CustomButton
  else
    Result := Item.Image;
end;

function TdxTileControlActionBarItemViewInfo.GetTextureType: TImageType;
begin
  if Item.Image.Empty then
    Result := inherited GetTextureType
  else
    Result := Item.GlyphType;
end;

{ TdxLayoutItemPosition }

procedure TdxLayoutItemPosition.Assign(ASource: TdxLayoutItemPosition);
begin
  FColumn := ASource.Column;
  FRow := ASource.Row;
end;

function TdxLayoutItemPosition.IsEqual(APosition: TdxLayoutItemPosition): Boolean;
begin
  Result := (APosition <> nil) and (Row = APosition.Row) and (Column = APosition.Column);
end;

{ TdxTileControlCells }
function TdxTileControlCells.CalculateHitTest(AHitTest: TdxTileControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if (Items[I] is TdxTileControlTitleViewInfo) and Items[I].GetHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
  if not Result then
    for I := Count - 1 downto 0 do
      if Items[I].GetHitTest(AHitTest) then
      begin
        Result := True;
        Break;
      end;
end;

function TdxTileControlCells.GetItem(AIndex: Integer): TdxTileControlCustomCellViewInfo;
begin
  Result := TdxTileControlCustomCellViewInfo(inherited Items[AIndex]);
end;

procedure TdxTileControlCells.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas);
end;

procedure TdxTileControlCells.RefreshState;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RefreshState;
end;

procedure TdxTileControlCells.DrawWithoutClipping(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoDraw(ACanvas);
end;

{ TdxTileControlAnimatedDragAndDropCustomCellViewInfo }

procedure TdxTileControlAnimatedDragAndDropCustomCellViewInfo.DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect);
begin
end;

function TdxTileControlAnimatedDragAndDropCustomCellViewInfo.GetBaseDrawBounds: TRect;
begin
  Result := Bounds;
end;

function TdxTileControlAnimatedDragAndDropCustomCellViewInfo.GetDrawBounds(AAbsolutePosition: Boolean = False): TRect;
begin
  Result := GetBaseDrawBounds;
  if AAbsolutePosition then
    OffsetRect(Result, -TileControl.ViewInfo.LeftScrollPos, -TileControl.ViewInfo.TopScrollPos);
end;

function TdxTileControlAnimatedDragAndDropCustomCellViewInfo.GetIsAnimatedOnDragDrop: Boolean;
begin
  with TileControl.ViewInfo do
    Result := (DragDropChanges <> nil) and (DragDropChanges.IndexOf(Self) >= 0);
end;

function TdxTileControlAnimatedDragAndDropCustomCellViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileControl.ScaleFactor;
end;

function TdxTileControlAnimatedDragAndDropCustomCellViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := nil;
end;

{ TdxTileControlItemViewInfo }

constructor TdxTileControlItemViewInfo.Create(AOwner: TdxTileControlItem);
begin
  inherited Create;
  FItem := AOwner;
  FPosition := TdxLayoutItemPosition.Create;
  FIsDirty := True;
end;

destructor TdxTileControlItemViewInfo.Destroy;
begin
  FreeAndNil(FCache);
  FreeAndNil(FPosition);
  inherited Destroy;
end;

procedure TdxTileControlItemViewInfo.CalculateBounds;
var
  ABounds: TRect;
begin
  ABounds.Left := GetLeft;
  ABounds.Top := GetTop;
  ABounds.Right := ABounds.Left + GetWidth;
  ABounds.Bottom := ABounds.Top + GetHeight;
  ABounds := cxRectOffset(ABounds, ScaleFactor.Apply(1), ScaleFactor.Apply(1));
  if Item.UseRightToLeftAlignment then
    ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds, Item.Group.Bounds);
  Bounds := ABounds;
end;

function TdxTileControlItemViewInfo.CanFocusInDesigning: Boolean;
begin
  Result := False;
end;

procedure TdxTileControlItemViewInfo.DoCalculate;
begin
  if FItem.Visible then
  begin
    CalculateBounds;
    inherited DoCalculate;
    if Item.CanGalleryAnimation then
      if GalleryCellController.AreCellsDirty then
        GalleryCellController.InitializeCells
      else
        GalleryCellController.RecalculateCells;
  end;
end;

procedure TdxTileControlItemViewInfo.DoDraw(ACanvas: TcxCanvas);

  function CalculateCheckMarkAlpha: Byte;
  begin
    if (TileControl.DragAndDropObject as TdxTileControlDragDropItem).IsPullDownGesture then
      Result := MaxByte
    else
      Result := MulDiv(150, (TileControl.DragAndDropObject as TdxTileControlDragDropItem).PullProgress, 100);
  end;

begin
  if (Group = nil) or Group.IsDragged or not ACanvas.RectVisible(GetActualVisualBounds) then Exit;

  Calculated := True;

  if Item.IsDragged or (not Controller.CanItemDrawing and (Controller.DragCell = Self)) then
  begin
    if NeedDrawDragItemPlace then
      Painter.DrawItemPlace(ACanvas, DragItemPlaceBounds);
    if (TileControl.DragAndDropObject as TdxTileControlDragDropItem).State = tcddsPulling then
    begin
      Painter.DrawDefaultItemCheck(ACanvas, Bounds, clNone,
        Painter.GroupCaptionDefaultTextColor, CalculateCheckMarkAlpha);
    end;
    Exit;
  end;
  if IsAnimatedOnDragDrop then Exit;

  if ACanvas.RectVisible(Bounds) then
  begin
    DrawContent(ACanvas, GetDrawBounds);
    if Selected then
      DrawSelection(ACanvas);
    if Item.Checked then
      DrawChecked(ACanvas);
  end;

  if Item.CanFocused then
  begin
    if Focused then
      DrawFocusRect(ACanvas);
    if Hottracked then
      DrawHottrackRect(ACanvas);
  end;
end;

procedure TdxTileControlItemViewInfo.DirtyChanged;
begin
  FreeAndNil(FCache);
  ValidateImage;
end;

procedure TdxTileControlItemViewInfo.DrawChecked(ACanvas: TcxCanvas);
begin
  Painter.DrawItemCheck(ACanvas, Bounds, ScaleFactor);
end;

procedure TdxTileControlItemViewInfo.DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect);
begin
  ValidateCache(ADrawRect);
  cxBitBlt(ACanvas.Handle, Cache.Canvas.Handle, ADrawRect, cxNullPoint, SRCCOPY);
end;

procedure TdxTileControlItemViewInfo.DrawFocusRect(ACanvas: TcxCanvas);
begin
  DrawFrameRect(ACanvas, Painter.GetSelectionFocusedColor(Item), TileControl.OptionsBehavior.ItemFocusMode = tcifmOuterFrame);
end;

procedure TdxTileControlItemViewInfo.DrawFrameRect(ACanvas: TcxCanvas; AColor: TColor; AIsOuter: Boolean; AColorAlpha: Byte = 255);
var
  R: TRect;
  ARegion: TcxRegion;
  AFrameSize: Integer;
begin
  R := GetActualVisualBounds;
  ACanvas.SaveClipRegion;
  try
    ARegion := TcxRegion.Create(R);
    ACanvas.SetClipRegion(ARegion, roSet);
    AFrameSize := GetActualFocusFrameSize;
    if not AIsOuter then
      ACanvas.ExcludeClipRect(cxRectInflate(R, -AFrameSize))
    else
      ACanvas.ExcludeClipRect(cxRectInflate(R, -ScaleFactor.Apply(2)));
    Painter.ExcludeInvisibleOuterFrameParts(ACanvas, R, AFrameSize);
    dxGpFillRect(ACanvas.Handle, R, AColor, AColorAlpha);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxTileControlItemViewInfo.DrawHighlightRect(ACanvas: TcxCanvas; const R: TRect; const AHighlightColor: TdxAlphaColor);
begin
  TileControl.Painter.DrawHighlightRect(ACanvas, R, AHighlightColor, Item.Style.GradientBeginColor);
end;

procedure TdxTileControlItemViewInfo.DrawHottrackRect(ACanvas: TcxCanvas);
begin
  if TileControl.OptionsBehavior.ItemHotTrackMode = tcihtmHighlight then
    TileControl.Painter.DrawHighlightRect(ACanvas, GetHighlightBounds(Bounds), TileControl.OptionsBehavior.ItemHotTrackHighlightColor,
      Item.Style.GradientBeginColor)
  else
    DrawFrameRect(ACanvas, Painter.GetSelectionHottrackedColor(Item),
      TileControl.OptionsBehavior.ItemHotTrackMode = tcihtmOuterFrame, HottrackValue);
end;

procedure TdxTileControlItemViewInfo.DrawSelection(ACanvas: TcxCanvas);
var
  C30A71AAD3D24F77897BC4987A1C9D88: TRect;
begin
  C30A71AAD3D24F77897BC4987A1C9D88 := cxRectInflate(Bounds, InflateDelta);
  dxGpFillRect(ACanvas.Handle, C30A71AAD3D24F77897BC4987A1C9D88, Painter.SelectionSelectedColor, 127);

  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psDashDot;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(C30A71AAD3D24F77897BC4987A1C9D88);
end;

function TdxTileControlItemViewInfo.GetActualFocusFrameSize: Integer;
var
  AOptionsBehavior: TdxTileControlOptionsBehavior;
begin
  Result := 0;
  AOptionsBehavior := TileControl.OptionsBehavior;
  if (Item = Controller.HottrackedItem) and (AOptionsBehavior.ItemHotTrackMode in [tcihtmFrame, tcihtmOuterFrame]) then
    if AOptionsBehavior.ItemHotTrackMode = tcihtmOuterFrame then
      Result := dxTileFocusItemOuterFrameSize
    else
      Result := dxTileFocusItemFrameSize;

  if Focused and (AOptionsBehavior.ItemFocusMode = tcifmOuterFrame) then
      Result := Max(Result, dxTileFocusItemOuterFrameSize)
    else
      Result := Max(Result, dxTileFocusItemFrameSize);
  Result := ScaleFactor.Apply(Result);
end;

function TdxTileControlItemViewInfo.GetActualVisualBounds: TRect;
var
  AFocusSize: Integer;
begin
  Result := Bounds;
  AFocusSize := GetActualFocusFrameSize;
  if AFocusSize > 0 then
    InflateRect(Result, AFocusSize, AFocusSize);
end;

function TdxTileControlItemViewInfo.GetDragItemPlaceBounds: TRect;
begin
  Result := cxRectInflate(Bounds, -dxTileControlInflateAnimationStepsCount, -dxTileControlInflateAnimationStepsCount);
end;

function TdxTileControlItemViewInfo.GetBaseDrawBounds: TRect;
begin
  Result := cxRectInflate(inherited GetBaseDrawBounds, InflateDelta, InflateDelta);
end;

function TdxTileControlItemViewInfo.GetLeft: Integer;
var
  ARegularItemWidth: Integer;
begin
  ARegularItemWidth := TileControl.OptionsView.ItemWidth;
  Result := Item.Group.Bounds.Left + (Position.Column div 2) * (ARegularItemWidth + ItemIndent);
  if Odd(Position.Column) then
    Result := Result + ARegularItemWidth - TileControl.ViewInfo.ItemSmallWidth;
end;

function TdxTileControlItemViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Item.TileControl;
end;

function TdxTileControlItemViewInfo.GetTop: Integer;
var
  ARegularItemHeight: Integer;
begin
  ARegularItemHeight := TileControl.OptionsView.ItemHeight;
  Result := Item.Group.Bounds.Top + Item.Group.Caption.Height + (Position.Row div 2) * (ARegularItemHeight + ItemIndent);
  if Odd(Position.Row) then
    Result := Result + ARegularItemHeight - TileControl.ViewInfo.ItemSmallHeight;
end;

function TdxTileControlItemViewInfo.GetVisibleBounds: TRect;
begin
  if Group = nil then
    Result := cxNullRect
  else
    Result := Group.ViewInfo.ClipRect;
end;

function TdxTileControlItemViewInfo.GetWidth: Integer;
begin
  case Item.Size of
    tcisSmall:
      Result := TileControl.ViewInfo.ItemSmallWidth;
    tcisRegular:
      Result := TileControl.OptionsView.ItemWidth
  else
    Result := TileControl.ViewInfo.ItemLargeWidth;
  end;
end;

procedure TdxTileControlItemViewInfo.Invalidate;
begin
  Item.Invalidate;
end;

procedure TdxTileControlItemViewInfo.NotifyBiDiModeChanged;
begin
  FIsDirty := True;
  ViewData.FIsDirty := True;
end;

procedure TdxTileControlItemViewInfo.Scroll(const DX, DY: Integer);
begin
  inherited Scroll(DX, DY);
  if Item.CanGalleryAnimation then
    Item.GalleryCellController.RecalculateCells;
end;

procedure TdxTileControlItemViewInfo.ValidateCache(const ADrawRect: TRect);

  function CheckCacheSize(const ADrawRect: TRect): Boolean;
  begin
    Result := (Cache <> nil) and
      (Cache.Width = cxRectWidth(ADrawRect)) and
      (Cache.Height = cxRectHeight(ADrawRect));
  end;

  procedure InternalDraw(ACanvas: TcxCanvas; const R: TRect);
  begin
    if (ContentAnimation = nil) and not Item.CanGalleryAnimation then
      ViewData.Image.StretchDraw(ACanvas.Handle, R)
    else
      if ContentAnimation is TdxTileControlFramesAnimation then
        TdxTileControlFramesAnimation(ContentAnimation).Draw(ACanvas.Canvas, R)
      else
        GalleryCellController.DrawCells(ACanvas, False);

    if not ViewData.NeedDrawTextOnImage or Item.CanDisplayGallery then
    begin
      ACanvas.SaveDC;
      try
        ACanvas.IntersectClipRect(R);
        MoveWindowOrg(ACanvas.Handle, R.Left, R.Top);
        ViewData.DrawItemTexts(ACanvas);
      finally
        ACanvas.RestoreDC;
      end;
    end;
  end;

var
  ACanvas: TdxGPCanvas;
  ATempImage: TdxGPImage;
  ATempImageDC: HDC;
begin
  IsDirty := False;
  if not CheckCacheSize(ADrawRect) or Item.CanGalleryAnimation or (Item.IsEnabled <> IsDrawnAsEnabled) then
  begin
    FreeAndNil(FCache);
    FCache := TcxBitmap.CreateSize(ADrawRect);
    if (cxRectWidth(Bounds) = Cache.Width) and (cxRectHeight(Bounds) = Cache.Height) then
      InternalDraw(Cache.cxCanvas, Cache.ClientRect)
    else
      if (ContentAnimation <> nil) or not ViewData.NeedDrawTextOnImage then
      begin
        ATempImage := TdxGPImage.CreateSize(ViewData.Bounds, 0);
        try
          ACanvas := ATempImage.CreateCanvas;
          try
            ATempImageDC := ACanvas.GetHDC;
            try
              cxPaintCanvas.BeginPaint(ATempImageDC);
              InternalDraw(cxPaintCanvas, ATempImage.ClientRect);
              cxPaintCanvas.EndPaint;
            finally
              ACanvas.ReleaseHDC(ATempImageDC);
            end;
          finally
            ACanvas.Free;
          end;
          ATempImage.StretchDraw(Cache.Canvas.Handle, Cache.ClientRect);
        finally
          ATempImage.Free;
        end;
      end
      else
        ViewData.Image.StretchDraw(Cache.Canvas.Handle, Cache.ClientRect);
    if not Item.IsEnabled then
      DrawHighlightRect(Cache.cxCanvas, Cache.ClientRect, dxTileItemDisabledMaskColor);
    IsDrawnAsEnabled := Item.IsEnabled;
  end;
end;

procedure TdxTileControlItemViewInfo.ValidateImage;
begin
  ViewData.ValidateViewData;
end;

function TdxTileControlItemViewInfo.CanHottracked: Boolean;
var
  AHotTrackMode: TdxTileControlItemHotTrackMode;
  AFocusedMode: TdxTileControlItemFocusMode;
begin
  AHotTrackMode := TileControl.OptionsBehavior.ItemHotTrackMode;
  AFocusedMode := TileControl.OptionsBehavior.ItemFocusMode;
  Result := not(Focused and (
    ((AHotTrackMode = tcihtmFrame) and (AFocusedMode = tcifmDefault)) or
    ((AHotTrackMode = tcihtmOuterFrame) and (AFocusedMode = tcifmOuterFrame))
    ));
end;

function TdxTileControlItemViewInfo.GetController: TdxTileControlController;
begin
  Result := TileControl.Controller;
end;

function TdxTileControlItemViewInfo.GetFocused: Boolean;
begin
  Result := (CanFocusInDesigning or not TileControl.IsDesigning) and (InflateDelta = 0) and
    (Controller.FocusedItem = Item) and ((Controller.HottrackedItem = nil) or
    not TileControl.OptionsBehavior.HideFocusOnItemHotTrack);
end;

function TdxTileControlItemViewInfo.GetGalleryCellController: TdxTileControlGalleryCellController;
begin
  Result := Item.GalleryCellController;
end;

function TdxTileControlItemViewInfo.GetGroup: TdxTileControlGroup;
begin
  Result := Item.Group;
end;

function TdxTileControlItemViewInfo.GetHottracked: Boolean;
begin
  Result := not TileControl.IsDesigning and (TileControl.OptionsBehavior.ItemHotTrackMode <> tcihtmNone) and
    (InflateDelta = 0) and ((HottrackValue <> 0) or (Controller.HottrackedItem = Item)) and CanHottracked;
end;

function TdxTileControlItemViewInfo.GetItemIndent: Integer;
begin
  Result := TileControl.OptionsView.ItemIndent;
end;

function TdxTileControlItemViewInfo.GetNeedDrawDragItemPlace: Boolean;
begin
  Result := Controller.IsEnableDrawDragItemPlace;
end;

function TdxTileControlItemViewInfo.GetHeight: Integer;
var
  ARegularItemHeight, AItemIndent, ARowCount: Integer;
begin
  ARegularItemHeight := TileControl.OptionsView.ItemHeight;
  AItemIndent := TileControl.OptionsView.ItemIndent;
  ARowCount := Item.RowCount;
  if Item.Size = tcisSmall then
  begin
    Result := ARowCount div 2 * (ARegularItemHeight + AItemIndent);
    if Odd(ARowCount) then
      Inc(Result, TileControl.ViewInfo.ItemSmallHeight)
    else
      Dec(Result, AItemIndent);
  end
  else
    Result := ARowCount * (ARegularItemHeight + AItemIndent) - AItemIndent;
end;

function TdxTileControlItemViewInfo.GetHighlightBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

function TdxTileControlItemViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtItem] := True;
end;

function TdxTileControlItemViewInfo.GetLastOccupiedColumn(AStartColumn: Integer): Integer;
begin
  case Item.Size of
    tcisSmall:
      Result := AStartColumn;
    tcisRegular:
      Result := AStartColumn + 1;
  else
    Result := AStartColumn + 3;
  end;
end;

function TdxTileControlItemViewInfo.GetLastOccupiedRow(AStartRow: Integer): Integer;
begin
  Result := AStartRow;
  if Item.Size = tcisSmall then
    Inc(Result, Item.RowCount - 1)
  else
    Inc(Result, 2 * Item.RowCount - 1);
end;

function TdxTileControlItemViewInfo.GetSelected: Boolean;
var
  I: Integer;
begin
  Result := Controller.IsObjectSelected(FItem);
  if not Result then
    for I := 0 to FItem.Frames.Count - 1 do
      if Controller.IsObjectSelected(FItem.Frames[I]) then
      begin
        Result := True;
        Break;
      end;
end;

function TdxTileControlItemViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := TileControl.Painter;
end;

function TdxTileControlItemViewInfo.GetViewData: TdxTileControlCustomItemViewData;
begin
  if FViewData = nil then
    FViewData := Item.GetActualViewData;
  Result := FViewData;
end;

procedure TdxTileControlItemViewInfo.SetHottrackValue(AValue: Integer);
begin
  if AValue <> FHottrackValue then
  begin
    if Visible then
      Item.Invalidate;
    FHottrackValue := AValue;
  end;
end;

procedure TdxTileControlItemViewInfo.SetInflateDelta(AValue: Integer);
begin
  if AValue <> FInflateDelta then
  begin
    if (AValue <> 0) and (HottrackValue <> 0) then
    begin
      FHottrackValue := 0;
      Item.Invalidate;
    end;
    FInflateDelta := AValue;
    if Visible then
      Item.Invalidate;
  end;
end;

procedure TdxTileControlItemViewInfo.SetIsDirty(AValue: Boolean);
begin
  if AValue <> FIsDirty then
  begin
    FIsDirty := AValue;
    if not IsDirty then
      DirtyChanged;
  end;
end;

procedure TdxTileControlItemViewInfo.SetViewData(AValue: TdxTileControlCustomItemViewData);
begin
  if ContentAnimation <> nil then
    ContentAnimation.Terminate;
  FViewData := AValue;
  IsDirty := True;
  Calculated := False;
  Item.Invalidate;
end;

{ TdxTileControlGroupViewInfo }

procedure TdxTileControlGroupViewInfo.CalculateExpandedBounds;
begin
  if not Group.Visible then
    FExpandedBounds := cxNullRect
  else
  begin
    FExpandedBounds := FMaximizedAreaBounds;
    if GroupLayout = glHorizontal then
    begin
      ExpandBoundsToTopControl(FExpandedBounds);
      ExpandBoundsToBottomControl(FExpandedBounds);
    end
    else
    begin
      ExpandBoundsToLeftControl(FExpandedBounds);
      ExpandBoundsToRightControl(FExpandedBounds);
    end;
  end;
end;

procedure TdxTileControlGroupViewInfo.CalculateMaximizedAreaBounds;
var
  AItemHalfIndent: Integer;
begin
  if not Group.Visible then
    FMaximizedAreaBounds := cxNullRect
  else
  begin
    AItemHalfIndent := TileControl.ViewInfo.ItemHalfIndent;
    FMaximizedAreaBounds := cxRectOffset(Bounds, -AItemHalfIndent, -AItemHalfIndent + Group.Caption.Height);
    FMaximizedAreaBounds.Bottom := FMaximizedAreaBounds.Top + GetTilesMaximizedAreaHeight;
    FMaximizedAreaBounds.Right := FMaximizedAreaBounds.Left + GetTilesMaximizedAreaWidth;
  end;
end;

procedure TdxTileControlGroupViewInfo.CalculateDimensionAndItemsPositions;
var
  I: Integer;
  AItem, AItemPrev: TdxTileControlItem;
begin
  AItemPrev := nil;
  FRowCount := 0;
  FColumnCount := 0;
  for I := 0 to Group.ItemCount - 1 do
  begin
    AItem := Group.Items[I];
    if not AItem.Visible then
      Continue;
    CalculateItemPosition(AItem, AItemPrev);
    if GroupLayout = glHorizontal then
    begin
      FRowCount := Max(FRowCount,
        2 * (GetStartOfCurrentRegularRow(AItem.ViewInfo.GetLastOccupiedRow(AItem.ViewInfo.Position.Row)) div 2 + 1));
      FColumnCount := Max(FColumnCount, AItem.ViewInfo.GetLastOccupiedColumn(AItem.ViewInfo.Position.Column) + 1);
    end
    else
    begin
      FRowCount := Max(FRowCount, AItem.ViewInfo.GetLastOccupiedRow(AItem.ViewInfo.Position.Row) + 1);
      FColumnCount := Max(FColumnCount,
        2 * (GetStartOfCurrentRegularColumn(AItem.ViewInfo.GetLastOccupiedColumn(AItem.ViewInfo.Position.Column)) div 2 + 1));
    end;
    if IsGroupEmptyAndDragItemOverIt then
    begin
      FRowCount := Max(FRowCount, 2);
      FColumnCount := Max(FColumnCount, 2);
    end;
    AItemPrev := AItem;
  end;
end;

procedure TdxTileControlGroupViewInfo.CalculateItemPosition(AItem, APrevItem: TdxTileControlItem);

  procedure MoveToNextRegularPosition(var ANewRow, ANewColumn: Integer; const AIsLarge: Boolean);
  var
    ASmallColumnCountInLogicalRow: Integer;
  begin
    ASmallColumnCountInLogicalRow := SmallColumnCountInLogicalRow;
    if ANewColumn mod ASmallColumnCountInLogicalRow <  ASmallColumnCountInLogicalRow - 2 * (1 + Integer(AIsLarge)) then
    begin
      ANewColumn := GetStartOfCurrentRegularColumn(ANewColumn) + 2;
      ANewRow := ANewRow div 2 * 2;
    end
    else
    begin
      ANewColumn := GetStartOfCurrentLogicalColumn(ANewColumn, ASmallColumnCountInLogicalRow);
      ANewRow := ANewRow div 2 * 2 + 2;
    end;
  end;

  procedure MoveToNextSmallPosition(var ANewRow, ANewColumn: Integer);
  var
    ASmallColumnCountInLogicalRow: Integer;
  begin
    ASmallColumnCountInLogicalRow := SmallColumnCountInLogicalRow;
    if not Odd(ANewColumn mod ASmallColumnCountInLogicalRow) then
      Inc(ANewColumn)
    else
      if not Odd(ANewRow) then
      begin
        Inc(ANewRow);
        Dec(ANewColumn);
      end
      else
        MoveToNextRegularPosition(ANewRow, ANewColumn, False);
  end;

var
  AColumn, ARow, ASmallColumnCountInLogicalRow: Integer;
begin
  AColumn := 0;
  ARow := 0;
  if APrevItem <> nil then
  begin
    AColumn := APrevItem.ViewInfo.Position.Column;
    ARow := APrevItem.ViewInfo.Position.Row;
    repeat
      repeat
        if AItem.Size = tcisSmall then
          MoveToNextSmallPosition(ARow, AColumn)
        else
          MoveToNextRegularPosition(ARow, AColumn, AItem.Size <> tcisRegular);
      until not IsOccupiedByPriorItems(AItem, ARow, AColumn);

      if (ARow > 0) and (AItem.ViewInfo.GetLastOccupiedRow(ARow) >= 2 * TileControl.ViewInfo.RowCount) then
      begin
        ARow := 0;
        ASmallColumnCountInLogicalRow := SmallColumnCountInLogicalRow;
        AColumn := GetStartOfCurrentLogicalColumn(AColumn, ASmallColumnCountInLogicalRow) + ASmallColumnCountInLogicalRow;
      end;
    until not IsOccupiedByPriorItems(AItem, ARow, AColumn);
  end;
  AItem.ViewInfo.Position.Column := AColumn;
  AItem.ViewInfo.Position.Row := ARow;
end;

procedure TdxTileControlGroupViewInfo.CalculateItemsEdges;
var
  ABorderWidth: Integer;
begin
  ABorderWidth := GetBorderWidth;
  FItemsRight := GetLeftPos + ABorderWidth + FItemsLocalRight;
  FItemsBottom := GetTopPos + ABorderWidth + FItemsLocalBottom;
end;

procedure TdxTileControlGroupViewInfo.CalculateSize(var AWidth, AHeight: Integer);
var
  AItemHeight, AItemIndent, AItemWidth: Integer;
  ABorderWidth: Integer;
begin
  inherited CalculateSize(AWidth, AHeight);
  if not Group.Visible then Exit;
  AItemHeight := TileControl.OptionsView.ItemHeight;
  AItemWidth := TileControl.OptionsView.ItemWidth;
  AItemIndent := TileControl.OptionsView.ItemIndent;
  if FRowCount = 0 then
  begin
    AHeight := AItemHeight;
    AWidth := AItemWidth;
  end
  else
  begin
    AHeight := (AItemHeight + AItemIndent) * (RowCount div 2);
    if Odd(RowCount) then
      Inc(AHeight, TileControl.ViewInfo.ItemSmallHeight)
    else
      Dec(AHeight, AItemIndent);
    AWidth := (AItemWidth + AItemIndent) * (ColumnCount div 2);
    if Odd(ColumnCount) then
      Inc(AWidth, TileControl.ViewInfo.ItemSmallWidth)
    else
      Dec(AWidth, AItemIndent);
  end;
  FItemsLocalRight := AWidth;
  FItemsLocalBottom := AHeight + Group.Caption.Height;
  ABorderWidth := GetBorderWidth;
  AWidth := FItemsLocalRight + 2 * ABorderWidth;
  AHeight := FItemsLocalBottom + 2 * ABorderWidth;
end;

procedure TdxTileControlGroupViewInfo.DoCalculate;
var
  I: Integer;
begin
  CalculateDimensionAndItemsPositions;
  CalculateBounds;
  CalculateItemsEdges;
  CalculateMaximizedAreaBounds;
  CalculateExpandedBounds;
  inherited DoCalculate;
  Group.Caption.ViewInfo.Recalculate;
  for I := 0 to FGroup.ItemCount - 1 do
    Group.Items[I].ViewInfo.Calculated := False;
end;

procedure TdxTileControlGroupViewInfo.DoDraw(ACanvas: TcxCanvas);
const
  AStyles: array [Boolean] of TPenStyle = (psDash, psDashDot);
  AColors: array [Boolean] of TColor = (dxTileVirtualGroupBackgroundColor, clBlue);
var
  ARegion: TcxRegion;
  AWidth: Integer;
begin
  if not cxRectIsNull(FNewGroupDrawingBounds) then
  begin
    Painter.DrawVirtualGroup(ACanvas, FNewGroupDrawingBounds);
    Exit;
  end;
  if Group.IsDragged then
  begin
    if (Group.Collection <> nil) and ((TileControl.ViewInfo.DragDropChanges = nil) or
      not TileControl.ViewInfo.DragDropChanges.DragObjectFinished) then
      Painter.DrawItemPlace(ACanvas, Bounds);
    Exit;
  end;
  if not TileControl.IsDesigning then Exit;

  AWidth := GetBorderWidth;
  ACanvas.SaveClipRegion;
  try
    ARegion := TcxRegion.Create(Bounds);
    ACanvas.SetClipRegion(ARegion, roSet);
    Painter.ExcludeInvisibleOuterFrameParts(ACanvas, Bounds, AWidth);
    ACanvas.Pen.Width := AWidth;
    ACanvas.Pen.Style := AStyles[Selected];
    ACanvas.Pen.Color := AColors[Selected];
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(Bounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxTileControlGroupViewInfo.ExpandBoundsToBottomControl(var ABounds: TRect);
begin
  if ABounds.Bottom < TileControl.ViewInfo.ClientBounds.Bottom then
    ABounds.Bottom := TileControl.ViewInfo.ClientBounds.Bottom;
end;

procedure TdxTileControlGroupViewInfo.ExpandBoundsToLeftControl(var ABounds: TRect);
begin
  if ABounds.Left > TileControl.ViewInfo.ClientBounds.Left then
    ABounds.Left := TileControl.ViewInfo.ClientBounds.Left;
end;

procedure TdxTileControlGroupViewInfo.ExpandBoundsToRightControl(var ABounds: TRect);
begin
  if ABounds.Right < TileControl.ViewInfo.ClientBounds.Right then
    ABounds.Right := TileControl.ViewInfo.ClientBounds.Right;
end;

procedure TdxTileControlGroupViewInfo.ExpandBoundsToTopControl(var ABounds: TRect);
begin
  if ABounds.Top > TileControl.ViewInfo.ClientBounds.Top then
    ABounds.Top := TileControl.ViewInfo.ClientBounds.Top;
end;

function TdxTileControlGroupViewInfo.GetSmallColumnCountInLogicalRow: Integer;
begin
  Result := 2 * LogicalRowSize;
end;

function TdxTileControlGroupViewInfo.GetContentRect: TRect;
var
  I: Integer;
  ABounds: TRect;
begin
  if Group.ItemCount > 0 then
  begin
    Result.Left := Group.Items[0].ViewInfo.Bounds.Left;
    Result.Right := Group.Items[0].ViewInfo.Bounds.Right;
    Result.Top := Group.Items[0].ViewInfo.Bounds.Top;
    Result.Bottom := Group.Items[0].ViewInfo.Bounds.Bottom;
  end
  else
  begin
    Result.Left := Bounds.Left;
    Result.Right := Bounds.Right;
    Result.Bottom := Bounds.Bottom;
    Result.Top := Bounds.Top;
  end;
  for I := 1 to Group.ItemCount - 1 do
  begin      // left and top can't any more to change!
    ABounds := Group.Items[I].ViewInfo.Bounds;
    if ABounds.Right > Result.Right then
      Result.Right := ABounds.Right;
    if ABounds.Bottom > Result.Bottom then
      Result.Bottom := ABounds.Bottom;
  end;
end;

function TdxTileControlGroupViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtGroup] := True;
end;

function TdxTileControlGroupViewInfo.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlGroupViewInfo.GetLeftPos: Integer;
var
  APrevGroup: TdxTileControlGroup;
begin
  Result := Bounds.Left;
  if Group.Groups = nil then Exit;
  Result := inherited GetLeftPos;
  if not Group.Visible then Exit;
  APrevGroup := Group.Groups.GetPrevVisibleGroup(Group);
  if GroupLayout = glHorizontal then
  begin
    if APrevGroup <> nil then
      Result := APrevGroup.VirtualGroupAfter.Bounds.Right
    else
      Result := Group.VirtualGroupBefore.Bounds.Right;
  end
  else
  begin
    if APrevGroup <> nil then
      Result := APrevGroup.Bounds.Left
    else
    begin
      Result := TileControl.ViewInfo.TilesArea.Left - TileControl.ViewInfo.LeftScrollPos - GetBorderWidth;
      if not TileControl.OptionsView.FixedIndentHorz then
        Inc(Result, TileControl.OptionsView.IndentHorz);
      if TileControl.ViewInfo.IsFixedContentLeftSide then
        Result := Max(Result, TileControl.ViewInfo.VisibleGroupsOrigin.X);
    end;
  end;
end;

function TdxTileControlGroupViewInfo.GetLogicalRowSize: Integer;
begin
  Result := TileControl.OptionsView.GroupBlockMaxColumnCount;
end;

function TdxTileControlGroupViewInfo.GetOccupiedItem(APosition: TdxLayoutItemPosition): TdxTileControlItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Group.ItemCount - 1 do
    if IsItemOccupyPosition(Group.Items[I], APosition) then
    begin
      Result := Group.Items[I];
      Break;
    end;
end;

class function TdxTileControlGroupViewInfo.GetStartOfCurrentLogicalColumn(
  const ASmallColumn, ASmallColumnCountInLogicalRow: Integer): Integer;
begin
  Result := ASmallColumn div ASmallColumnCountInLogicalRow * ASmallColumnCountInLogicalRow;
end;

class function TdxTileControlGroupViewInfo.GetStartOfCurrentRegularColumn(const ASmallColumn: Integer): Integer;
begin
  Result := ASmallColumn div 2 * 2;
end;

class function TdxTileControlGroupViewInfo.GetStartOfCurrentRegularRow(const ASmallRow: Integer): Integer;
begin
  Result := GetStartOfCurrentRegularColumn(ASmallRow);
end;

function TdxTileControlGroupViewInfo.GetTilesMaximizedAreaHeight: Integer;
var
  AItemIndent: Integer;
begin
  AItemIndent := TileControl.OptionsView.ItemIndent;
  Result := TileControl.OptionsView.ItemHeight + AItemIndent;
  if GroupLayout = glHorizontal then
    Result := Result * TileControl.ViewInfo.RowCount
  else
    if RowCount > 0 then
      Result := Result * (RowCount div 2);
end;

function TdxTileControlGroupViewInfo.GetTilesMaximizedAreaWidth: Integer;
var
  AItemIndent: Integer;
begin
  AItemIndent := TileControl.OptionsView.ItemIndent;
  Result := TileControl.OptionsView.ItemWidth + AItemIndent;
  if ColumnCount > 0 then
    Result := Result * (ColumnCount div 2);
  if Odd(ColumnCount) then
    Inc(Result, TileControl.ViewInfo.ItemSmallWidth + AItemIndent);
end;

function TdxTileControlGroupViewInfo.GetTopPos: Integer;
var
  APrevGroup: TdxTileControlGroup;
begin
  Result := Bounds.Top;
  if not Group.Visible or (Group.Groups = nil) then Exit;
  if GroupLayout = glHorizontal then
  begin
    Result := TileControl.ViewInfo.TilesArea.Top - TileControl.ViewInfo.TopScrollPos - GetBorderWidth;
    if not TileControl.OptionsView.FixedIndentVert then
      Inc(Result, TileControl.OptionsView.IndentVert);
    if TileControl.ViewInfo.IsFixedContentTopSide then
      Result := Max(Result, TileControl.ViewInfo.VisibleGroupsOrigin.Y);
  end
  else
  begin
    APrevGroup := Group.Groups.GetPrevVisibleGroup(Group);
    if APrevGroup <> nil then
      Result := APrevGroup.VirtualGroupAfter.Bounds.Bottom
    else
      Result := Group.VirtualGroupBefore.Bounds.Bottom;
  end;
end;

function TdxTileControlGroupViewInfo.GetSelected: Boolean;
begin
  Result := TileControl.Controller.IsObjectSelected(FGroup);
end;

function TdxTileControlGroupViewInfo.GetZIndex(AColumn, ARow, ADragItemRow: Integer): Integer;
var
  ASmallColumnCountInLogicalRow, AStartLogicalColumn: Integer;
begin
  ASmallColumnCountInLogicalRow := SmallColumnCountInLogicalRow;
  AStartLogicalColumn := GetStartOfCurrentLogicalColumn(AColumn, ASmallColumnCountInLogicalRow);
  Result := AStartLogicalColumn * Max(RowCount, (ADragItemRow div 2 + 1) * 2) +
    ARow div 2 * (2 * ASmallColumnCountInLogicalRow) +
   (AColumn - AStartLogicalColumn) div 2 * 4 +
    ARow mod 2 * 2 + AColumn mod 2;
end;

function TdxTileControlGroupViewInfo.GetZIndex(APosition, ADragItemPosition: TdxLayoutItemPosition): Integer;
begin
  Result := GetZIndex(APosition.Column, APosition.Row, ADragItemPosition.Row);
end;

procedure TdxTileControlGroupViewInfo.Invalidate;
begin
  Group.Invalidate;
end;

function TdxTileControlGroupViewInfo.IsGroupEmptyAndDragItemOverIt: Boolean;
begin
  Result := (Group.ItemCount = 1) and Group.Items[0].IsDragged;
end;

function TdxTileControlGroupViewInfo.IsItemOccupyPosition(AItem: TdxTileControlItem; APosition: TdxLayoutItemPosition): Boolean;
var
  AItemPosition: TdxLayoutItemPosition;
  AItemLastRow, AItemLastColumn: Integer;
begin
  Result := AItem.Visible;
  if not Result then Exit;
  AItemPosition := AItem.ViewInfo.Position;
  AItemLastColumn := AItem.ViewInfo.GetLastOccupiedColumn(AItemPosition.Column);
  AItemLastRow := AItem.ViewInfo.GetLastOccupiedRow(AItemPosition.Row);
  Result := (APosition.Row >= AItemPosition.Row) and (APosition.Row <= AItemLastRow) and
    (APosition.Column >= AItemPosition.Column) and (APosition.Column <= AItemLastColumn);
end;

function TdxTileControlGroupViewInfo.IsAreasIntersect(const AArea1, AArea2: TRect): Boolean;
var
  AArea: TRect;
begin
  AArea.Left := Max(AArea2.Left, AArea1.Left);
  AArea.Top := Max(AArea2.Top, AArea1.Top);
  AArea.Right := Min(AArea2.Right, AArea1.Right);
  AArea.Bottom := Min(AArea2.Bottom, AArea1.Bottom);
  Result := not ((AArea.Right < AArea.Left) or (AArea.Bottom < AArea.Top));
end;

function TdxTileControlGroupViewInfo.IsOccupiedByPriorItems(ACurrentItem: TdxTileControlItem;
  ACurrentRow, ACurrentColumn: Integer): Boolean;
var
  I, ACurrentLastColumn, ACurrentLastRow: Integer;
  AItem: TdxTileControlItem;
  APosition: TdxLayoutItemPosition;
begin
  Result := False;
  ACurrentLastColumn := ACurrentItem.ViewInfo.GetLastOccupiedColumn(ACurrentColumn);
  ACurrentLastRow := ACurrentItem.ViewInfo.GetLastOccupiedRow(ACurrentRow);
  for I := 0 to Group.FItems.IndexOf(ACurrentItem) - 1 do
  begin
    AItem := Group.Items[I];
    APosition := AItem.ViewInfo.Position;
    if AItem.Visible then
      Result := IsAreasIntersect(
        Rect(ACurrentColumn, ACurrentRow, ACurrentLastColumn, ACurrentLastRow),
        Rect(APosition.Column, APosition.Row,
          AItem.ViewInfo.GetLastOccupiedColumn(APosition.Column), AItem.ViewInfo.GetLastOccupiedRow(APosition.Row)));
    if Result then
      Break;
  end;
end;

procedure TdxTileControlGroupViewInfo.Offset(const DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  FMaximizedAreaBounds := cxRectOffset(FMaximizedAreaBounds, DX, DY);
  FExpandedBounds := cxRectOffset(FExpandedBounds, DX, DY);
  if not cxRectIsEqual(FNewGroupDrawingBounds, cxNullRect) then
    FNewGroupDrawingBounds := cxRectOffset(FNewGroupDrawingBounds, DX, DY);
end;

{ TdxTileControlTitleViewInfo }

constructor TdxTileControlTitleViewInfo.Create;
begin
  inherited Create;
  FFont := TFont.Create;
end;

destructor TdxTileControlTitleViewInfo.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxTileControlTitleViewInfo.CheckBiDiModeFlags;
begin
  FUseRightToLeftAlignment := TileControl.UseRightToLeftAlignment;
  FUseRightToLeftScrollBar := TileControl.UseRightToLeftScrollBar;
end;

procedure TdxTileControlTitleViewInfo.Clear;
begin
  FUseRightToLeftAlignment := False;
  FUseRightToLeftScrollBar := False;
end;

procedure TdxTileControlTitleViewInfo.DoCalculate;
begin
  Clear;
  TileControl.Painter.ValidatePainterData;
  Font.Assign(Title.Font);
  AdjustFontHeight(Font, ScaleFactor, dxTileControlDefaultTitleFontSize, Painter.TitleDefaultFontSize);
  FTextOffset := cxTextWidth(Font, ' ');
  FVisibleBounds := GetVisibleBounds;
  FBounds := FVisibleBounds;
  FClipRect := FBounds;
  FVisible := not cxRectIsEmpty(FBounds);
  DoCalculateGlyphBounds;
  DoCalculateTextBounds;
end;

procedure TdxTileControlTitleViewInfo.DoCalculateGlyphBounds;
begin
  FGlyphBounds.Left := Bounds.Right;
  FGlyphBounds.Right := FGlyphBounds.Left;
  if not Title.Glyph.Empty then
  begin
    FGlyphBounds := Rect(Bounds.Right - Title.Glyph.Width, Bounds.Bottom - Title.Glyph.Height,
      Bounds.Right, Bounds.Bottom);
    OffsetRect(FGlyphBounds, -Title.IndentHorz, -FGlyphBounds.Top div 2);
  end;
end;

procedure TdxTileControlTitleViewInfo.DoCalculateTextBounds;
begin
  FTextBounds.Left := GetLeftOffset;
  if TileControl.BiDiMode = bdRightToLeftNoAlign then
    Inc(FTextBounds.Left, TileControl.ClientBounds.Left);
  FTextBounds.Right := FTextBounds.Left;
  if Title.Text <> '' then
    FTextBounds := cxRectSetTop(MeasureTextBounds(FTextBounds),
      Bounds.Top + (cxRectHeight(Bounds) - GetTextHeight) div 2);
end;

procedure TdxTileControlTitleViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if Title.Color <> clDefault then
    ACanvas.FillRect(Bounds, Title.Color);
  if Title.Text <> '' then
    cxTextOut(ACanvas.Canvas, Title.Text, FTextBounds,
      GetTextOutFlags, 0, 0, Font, clNone, clNone, 0, 0, 0, TextColor);
  if not Title.Glyph.Empty then
    cxDrawImage(ACanvas, GlyphBounds, Title.Glyph, ifmNormal, nil, ScaleFactor);
  ACanvas.ExcludeClipRect(Bounds);
end;

function TdxTileControlTitleViewInfo.GetHeight: Integer;
var
  AIndentVert: Integer;
begin
  Result := inherited GetHeight;
  AIndentVert := Title.IndentVert;
  if not Title.Glyph.Empty then
    Result := cxRectHeight(Title.Glyph.ClientRect) + 2 * AIndentVert;
  if Title.Text <> '' then
    Result := Max(Result, GetTextHeight + 2 * AIndentVert);
end;

function TdxTileControlTitleViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtTitle] := True;
end;

function TdxTileControlTitleViewInfo.GetLeftOffset: Integer;
begin
  Result := TileControl.OptionsView.IndentHorz;
end;

function TdxTileControlTitleViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := TileControl.Painter;
end;

function TdxTileControlTitleViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileControl.ScaleFactor;
end;

function TdxTileControlTitleViewInfo.GetTextColor: TColor;
begin
  Result := cxGetActualColor(Font.Color, Painter.TitleDefaultTextColor);
end;

function TdxTileControlTitleViewInfo.GetTextHeight: Integer;
begin
  if Title.Text <> '' then
    Result := cxRectHeight(MeasureTextBounds(cxNullRect))
  else
    Result := 0;
end;

function TdxTileControlTitleViewInfo.GetTextOutFlags: Integer;
const
  HorzAlign: array[TAlignment] of TcxTextAlignX =
    (taLeft, taRight, taCenterX);
  VertAlign: array[TcxAlignmentVert] of TcxTextAlignY =
    (taTop, taBottom, taCenterY);
begin
  Result := cxMakeFormat(HorzAlign[Title.TextAlignHorz], VertAlign[Title.TextAlignVert]) or CXTO_EXPANDTABS;
  if Title.WordWrap then
    Result := Result or CXTO_WORDBREAK;
end;

function TdxTileControlTitleViewInfo.GetVisibleBounds: TRect;
var
  R: TRect;
begin
  R := TileControl.ClientBounds;
  if TileControl.IsScrollBarActive(sbVertical) then
    if TileControl.UseRightToLeftScrollBar then
      Dec(R.Left, TileControl.VScrollBar.Width)
    else
      Inc(R.Right, TileControl.VScrollBar.Width);
  Result := cxRectSetHeight(R, GetHeight);
end;

function TdxTileControlTitleViewInfo.MeasureTextBounds(const AVisibleBounds: TRect): TRect;
var
  DC: HDC;
  AFontHandle: HFONT;
  ATextRows: TcxTextRows;
  ATextParams: TcxTextParams;
  ALineCount: Integer;
const
  CalcFlags = CXTO_CALCROWCOUNT or CXTO_AUTOINDENTS or CXTO_CHARBREAK;
begin
  Result := AVisibleBounds;
  if Title.Text = '' then
  begin
    Result.Bottom := Result.Top;
    Exit;
  end;
  DC := GetDC(0);
  AFontHandle := SelectObject(DC, Font.Handle);
  try
    ATextParams := cxCalcTextParams(DC, GetTextOutFlags or CalcFlags);
    cxMakeTextRows(DC, PChar(Title.Text), Length(Title.Text), Result,
      ATextParams, ATextRows, ALineCount);
    Result := cxRectSetSize(Result, cxGetLongestTextRowWidth(ATextRows, ALineCount),
      ALineCount * ATextParams.FullRowHeight);
    cxResetTextRows(ATextRows);
  finally
    SelectObject(DC, AFontHandle);
    ReleaseDC(0, DC)
  end;
end;

procedure TdxTileControlTitleViewInfo.Scroll(const DX, DY: Integer);
begin
end;

procedure TdxTileControlTitleViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, Bounds);
  FGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphBounds, Bounds);
end;

procedure TdxTileControlTitleViewInfo.SetTextBounds(const ABounds: TRect);
begin
  FTextBounds := ABounds;
end;

function TdxTileControlTitleViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Title.TileControl;
end;

{ TdxTileControlItemCustomGlyph }

constructor TdxTileControlItemCustomGlyph.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FImage := TdxSmartGlyph.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChanged;
  FImagesNotifyComponent := TcxFreeNotificator.Create(nil);
  FImagesNotifyComponent.OnFreeNotification := ImagesFreeNotification;
  FIndentHorz := dxTileItemObjectDefaultIndent;
  FIndentVert := dxTileItemObjectDefaultIndent;
  FMode := ifmNormal;
end;

destructor TdxTileControlItemCustomGlyph.Destroy;
begin
  FImagesChangeLink.OnChange := nil;
  FImagesNotifyComponent.OnFreeNotification := nil;
  Images := nil;
  FreeAndNil(FImagesChangeLink);
  FreeAndNil(FImagesNotifyComponent);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxTileControlItemCustomGlyph.Assign(ASource: TPersistent);
var
  ASourceGlyph: TdxTileControlItemCustomGlyph;
begin
  if ASource is TdxTileControlItemCustomGlyph then
  begin
    ASourceGlyph := TdxTileControlItemCustomGlyph(ASource);
    Align := ASourceGlyph.Align;
    Image := ASourceGlyph.Image;
    ImageIndex := ASourceGlyph.ImageIndex;
    FImages := ASourceGlyph.Images;
    IndentHorz := ASourceGlyph.IndentHorz;
    IndentVert := ASourceGlyph.IndentVert;
    AlignWithText := ASourceGlyph.AlignWithText;
    Mode := ASourceGlyph.Mode;
  end
  else
    inherited Assign(ASource);
end;

function TdxTileControlItemCustomGlyph.GetClientRect(const ABounds: TRect): TRect;
var
  AImageSize, ARectSize, AGlyphSize: TSize;
  K: double;
  AImages: TCustomImageList;
begin
  AImageSize := cxSize(Image.ClientRect);
  if Image.Empty and IsImagesUse then
  begin
    AImages := GetImages;
    AImageSize.cx := AImages.Width;
    AImageSize.cy := AImages.Height;
  end;
  AImageSize := Owner.TileItem.TileControl.ScaleFactor.Apply(AImageSize);
  AGlyphSize := AImageSize;
  if not cxSizeIsEmpty(AImageSize) then
  begin
    ARectSize := cxSize(ABounds);
    K := Min(ARectSize.cx / AImageSize.cx, ARectSize.cy / AImageSize.cy);
    case Mode of
      ifmStretch:
        AGlyphSize := ARectSize;
      ifmProportionalStretch:
        AGlyphSize := cxSize(Trunc(AImageSize.cx * K), Trunc(AImageSize.cy * K));
      ifmFit:
        if K < 1 then
          AGlyphSize := cxSize(Trunc(AImageSize.cx * K), Trunc(AImageSize.cy * K));
    end;
  end;
  Result := cxRectCenter(ABounds, AGlyphSize);
end;

procedure TdxTileControlItemCustomGlyph.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  FIndentHorz := MulDiv(FIndentHorz, M, D);
  FIndentVert := MulDiv(FIndentVert, M, D);
end;

function TdxTileControlItemCustomGlyph.GetActualAlign: TdxTileItemInnerObjectAlignment;
begin
  Result := Align;
  if Result = oaDefault then
    Result := oaMiddleCenter;
  Result := dxGetBiDiModeTileItemInnerObjectAlignment(Result, Owner.TileItem.UseRightToLeftAlignment);
end;

function TdxTileControlItemCustomGlyph.GetImages: TCustomImageList;
begin
  Result := FImages;
  if Result <> nil then Exit;
  if Owner is TdxTileControlItem then
    Result := TdxTileControlItem(Owner).TileControl.Images
  else
    Result := (Owner as TdxTileControlItemFrame).TileItem.Glyph.GetImages;
end;

procedure TdxTileControlItemCustomGlyph.ImagesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxTileControlItemCustomGlyph.ImagesFreeNotification(AComponent: TComponent);
begin
  Images := nil;
end;

function TdxTileControlItemCustomGlyph.IsChanged: Boolean;
begin
  Result := (Align <> oaDefault) or not Image.Empty or IsImagesUse or (IndentHorz <> 0) or (IndentVert <> 0);
end;

function TdxTileControlItemCustomGlyph.IsImagesUse: Boolean;
var
  AImages: TCustomImageList;
begin
  AImages := GetImages;
  Result := (AImages <> nil) and (ImageIndex > -1) and (ImageIndex < AImages.Count);
end;

procedure TdxTileControlItemCustomGlyph.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxTileControlItemCustomGlyph.SetAlign(AValue: TdxTileItemInnerObjectAlignment);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemCustomGlyph.SetAlignWithText(AValue: TdxTileControlImageWithTextAlignment);
begin
  if FAlignWithText <> AValue then
  begin
    FAlignWithText := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemCustomGlyph.SetImage(AValue: TdxSmartGlyph);
begin
  FImage.Assign(AValue);
  Changed;
end;

procedure TdxTileControlItemCustomGlyph.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemCustomGlyph.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FImagesChangeLink, FImagesNotifyComponent);
end;

procedure TdxTileControlItemCustomGlyph.SetIndentHorz(AValue: Integer);
begin
  if FIndentHorz <> AValue then
  begin
    FIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemCustomGlyph.SetIndentVert(AValue: Integer);
begin
  if FIndentVert <> AValue then
  begin
    FIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemCustomGlyph.SetMode(AValue: TcxImageFitMode);
begin
  if FMode <> AValue then
  begin
    FMode := AValue;
    Changed;
  end;
end;

{ TdxTileControlCustomStyle }

constructor TdxTileControlCustomStyle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Height := dxGetFontHeightForDefaultDPI(FFont.Size);
  FFont.Name := DefaultFontName;
  FFont.Color := clDefault;
  FFont.OnChange := FontChanged;
  FBorderColor := clDefault;
  FTexture := TdxSmartImage.Create;
  FTexture.OnChange := ImageChanged;
  FGradientBeginColor := clDefault;
  FGradientEndColor := clDefault;
end;

destructor TdxTileControlCustomStyle.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FTexture);
  inherited Destroy;
end;

procedure TdxTileControlCustomStyle.Assign(ASource: TPersistent);
var
  ASourceStyle: TdxTileControlCustomStyle;
begin
  if ASource is TdxTileControlCustomStyle then
  begin
    ASourceStyle := TdxTileControlCustomStyle(ASource);
    Font := ASourceStyle.Font;
    FBorderColor := ASourceStyle.FBorderColor;
    Gradient := ASourceStyle.Gradient;
    GradientBeginColor := ASourceStyle.GradientBeginColor;
    GradientEndColor := ASourceStyle.GradientEndColor;
    Stretch := ASourceStyle.Stretch;
    Texture := ASourceStyle.Texture;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlCustomStyle.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TdxTileControlCustomStyle.ChangeScale(M, D: Integer);
var
  APrevFontChanged: Boolean;
begin
  APrevFontChanged := IsFontStored;
  Font.Height := MulDiv(Font.Height, M, D);
  FFontChanged := APrevFontChanged;
end;

procedure TdxTileControlCustomStyle.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
  Changed;
end;

function TdxTileControlCustomStyle.IsChanged: Boolean;
begin
  Result := (Gradient <> gmHorizontal) or (GradientBeginColor <> clNone) or
    (GradientEndColor <> clNone) or (Stretch <> smStretch) or not Texture.Empty or
    (BorderColor <> clDefault) or FFontChanged;
end;

function TdxTileControlCustomStyle.IsDefaultBackgroundColors: Boolean;
begin
  Result := (GradientBeginColor = clDefault) and (GradientEndColor = clDefault);
end;

function TdxTileControlCustomStyle.IsEmpty: Boolean;
begin
  Result := IsDefaultBackgroundColors and Texture.Empty;
end;

procedure TdxTileControlCustomStyle.ImageChanged(Sender: TObject);
begin
  Changed;
end;

function TdxTileControlCustomStyle.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxTileControlCustomStyle.SetBorderColor(AValue: TColor);
begin
  if FBorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomStyle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxTileControlCustomStyle.SetGradientBeginColor(AValue: TColor);
begin
  if FGradientBeginColor <> AValue then
  begin
    FGradientBeginColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomStyle.SetGradientEndColor(AValue: TColor);
begin
  if FGradientEndColor <> AValue then
  begin
    FGradientEndColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomStyle.SetGradientMode(AValue: TdxSkinGradientMode);
begin
  if FGradient <> AValue then
  begin
    FGradient := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomStyle.SetStretch(AValue: TdxSkinStretchMode);
begin
  if FStretch <> AValue then
  begin
    FStretch := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomStyle.SetTexture(AValue: TdxSmartImage);
begin
  if AValue <> nil then
    FTexture.Assign(AValue)
  else
    FTexture.Clear;
  Changed;
end;

{ TdxTileControlItemOptionsAnimate }

function TdxTileControlItemOptionsAnimate.GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate;
begin
  if Owner is TdxTileControlItem then
    Result := TdxTileControlItem(Owner).TileControl.OptionsItemAnimate
  else
    Result := nil;
end;

{ TdxTileControlItemFrameOptionsAnimate }

function TdxTileControlItemFrameOptionsAnimate.GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate;
begin
  if Owner is TdxTileControlItemFrame then
    Result := TdxTileControlItemFrame(Owner).TileItem.OptionsAnimate
  else
    Result := nil;
end;

{ TdxTileControlCustomItem }

constructor TdxTileControlCustomItem.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FViewData := CreateViewData;
  FVisible := True;
  FGlyph := CreateGlyph;
  FStyle := TdxTileControlCustomStyle.Create(Self);
  FStyle.OnChange := StyleChanged;
  FOptionsAnimate := CreateOptionsAnimate;
  FMargins := TcxMargin.Create(Self, 1);
  FMargins.Margin := cxSimpleRect;
  FMargins.OnChange := MarginsChangeHandler;
  FParentStyle := True;
  for I := 0 to 3 do
    FTexts[I] := CreateText(DefaultTextAlignment[I]);
  FVisible := True;
end;

destructor TdxTileControlCustomItem.Destroy;
begin
  FreeAndNil(FStyle);
  FreeAndNil(FGlyph);
  FreeAndNil(FMargins);
  FreeAndNil(FTexts[0]);
  FreeAndNil(FTexts[1]);
  FreeAndNil(FTexts[2]);
  FreeAndNil(FTexts[3]);
  FreeAndNil(FOptionsAnimate);
  FreeAndNil(FViewData);
  inherited Destroy;
end;

procedure TdxTileControlCustomItem.Assign(ASource: TPersistent);
var
  ACustomItem: TdxTileControlCustomItem;
begin
  if ASource is TdxTileControlCustomItem then
  begin
    ACustomItem := TdxTileControlCustomItem(ASource);
    Style := ACustomItem.Style;
    Glyph := ACustomItem.FGlyph;
    Text1 := ACustomItem.Text1;
    Text2 := ACustomItem.Text2;
    Text3 := ACustomItem.Text3;
    Text4 := ACustomItem.Text4;
    Margins := ACustomItem.Margins;
    OptionsAnimate := ACustomItem.OptionsAnimate;
    ParentStyle := ACustomItem.ParentStyle;
    Visible := ACustomItem.Visible;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlCustomItem.Changed;
begin
  TileItem.Invalidate();
end;

procedure TdxTileControlCustomItem.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  Glyph.ChangeScale(M, D);
  Style.ChangeScale(M, D);
  for I := 0 to 3 do
    FTexts[I].ChangeScale(M, D);
end;

function TdxTileControlCustomItem.CreateText(
  const AAlign: TdxTileItemInnerObjectAlignment): TdxTileControlItemText;
begin
  Result := TdxTileControlItemText.Create(Self);
  Result.FDefaultAlign := AAlign;
end;

function TdxTileControlCustomItem.CreateViewData: TdxTileControlCustomItemViewData;
begin
  Result := TdxTileControlCustomItemViewData.Create(Self);
end;

function TdxTileControlCustomItem.GetTileItem: TdxTileControlItem;
begin
  Result := nil;
end;

procedure TdxTileControlCustomItem.StyleChanged(Sender: TObject);
begin
  Changed;
end;

function TdxTileControlCustomItem.GetText(AIndex: Integer): TdxTileControlItemText;
begin
  Result := FTexts[AIndex];
end;

function TdxTileControlCustomItem.IsGlyphStored: Boolean;
begin
  Result := Glyph.IsChanged;
end;

function TdxTileControlCustomItem.IsMarginsStored: Boolean;
begin
  Result := not cxRectIsEqual(Margins.Margin, cxSimpleRect);
end;

function TdxTileControlCustomItem.IsStyleStored: Boolean;
begin
  Result := Style.IsChanged;
end;

procedure TdxTileControlCustomItem.MarginsChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxTileControlCustomItem.SetStyle(AValue: TdxTileControlCustomStyle);
begin
  FStyle.Assign(AValue);
  Changed;
end;

procedure TdxTileControlCustomItem.SetMargins(AValue: TcxMargin);
begin
  FMargins.Assign(AValue);
  Changed;
end;

procedure TdxTileControlCustomItem.SetOptionsAnimate(AValue: TdxTileControlItemOptionsAnimate);
begin
  FOptionsAnimate.Assign(AValue);
end;

procedure TdxTileControlCustomItem.SetParentStyle(AValue: Boolean);
begin
  if FParentStyle <> AValue then
  begin
    FParentStyle := AValue;
    Changed;
  end;
end;

procedure TdxTileControlCustomItem.SetText(AIndex: Integer; AValue: TdxTileControlItemText);
begin
  GetText(AIndex).Assign(AValue);
end;

procedure TdxTileControlCustomItem.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed;
    TileItem.TileControl.LayoutChanged;
  end;
end;

{ TdxTileControlItemFrames }

function TdxTileControlItemFrames.Add: TdxTileControlItemFrame;
begin
  Result := TdxTileControlItemFrame(inherited Add);
end;

procedure TdxTileControlItemFrames.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

procedure TdxTileControlItemFrames.ForEach(
  AProc: TdxTileControlForEachItemFrameProc; const AData: Pointer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AProc(Items[I], AData);
end;

function TdxTileControlItemFrames.GetItem(AIndex: Integer): TdxTileControlItemFrame;
begin
  Result := TdxTileControlItemFrame(inherited Items[AIndex]);
end;

function TdxTileControlItemFrames.GetOwnerItem: TdxTileControlItem;
begin
  Result := TdxTileControlItem(ParentComponent);
end;

procedure TdxTileControlItemFrames.SetItem(AIndex: Integer; const AValue: TdxTileControlItemFrame);
begin
  GetItem(AIndex).Assign(AValue);
end;

{ TdxTileControlItemDetailOptions }

destructor TdxTileControlItemDetailOptions.Destroy;
begin
  DetailControl := nil;
  FreeAndNil(FDetailSite);
  inherited Destroy;
end;

procedure TdxTileControlItemDetailOptions.Assign(ASource: TPersistent);
begin
  if ASource is TdxTileControlItemDetailOptions then
  begin
    Caption := TdxTileControlItemDetailOptions(ASource).Caption;
    DetailControl := TdxTileControlItemDetailOptions(ASource).DetailControl;
  end;
end;

procedure TdxTileControlItemDetailOptions.Changed;
begin
  TileItem.TileControl.LayoutChanged;
end;

function TdxTileControlItemDetailOptions.IsChanged: Boolean;
begin
  Result := (Caption <> '') or (DetailControl <> nil)
end;

function TdxTileControlItemDetailOptions.GetHasDetail: Boolean;
begin
  Result := (DetailControl <> nil) or Assigned(TileItem.OnActivateDetail) or ShowTab;
end;

function TdxTileControlItemDetailOptions.GetDetailControl: TWinControl;
begin
  Result := FDetailControl;
end;

function TdxTileControlItemDetailOptions.GetDetailSite: TdxTileControlDetailSite;
begin
  if FDetailSite = nil then
    FDetailSite := TileItem.CreateDetailSite;
  Result := FDetailSite;
  if not FDetailSite.HandleAllocated then
  begin
    FDetailSite.SetBounds(dxTileControlInvisiblePosition, 0, GetDetailSiteWidth, GetDetailSiteHeight);
    FDetailSite.Parent := TileItem.TileControl;
  end;
end;

function TdxTileControlItemDetailOptions.GetDetailSiteHeight: Integer;
begin
  Result := TileControl.Height;
end;

function TdxTileControlItemDetailOptions.GetDetailSiteWidth: Integer;
begin
  Result := TileControl.Width;
end;

function TdxTileControlItemDetailOptions.GetTileControl: TdxCustomTileControl;
begin
  Result := TileItem.TileControl;
end;

function TdxTileControlItemDetailOptions.GetTileItem: TdxTileControlItem;
begin
  Result := GetOwner as TdxTileControlItem;
end;

procedure TdxTileControlItemDetailOptions.SetCaption(AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemDetailOptions.SetDetailControl(AValue: TWinControl);
begin
  if AValue = TileItem.TileControl then
    AValue := nil;
  if FDetailControl = AValue then Exit;
  if not TileControl.IsDesigning then
  begin
    if FDetailControl <> nil then
      GetDetailsInfo.RemoveInfo(FDetailControl, TileItem);
    FDetailControl := AValue;
    if FDetailControl <> nil then
    begin
      GetDetailsInfo.AddInfo(FDetailControl, TileItem);
      if not TileControl.IsLoading then
        StoreDetailControlPrevParent(FDetailControl.Parent);
      FDetailControl.Parent := DetailSite;
    end;
  end
  else
    FDetailControl := AValue;
end;

procedure TdxTileControlItemDetailOptions.StoreDetailControlPrevParent(AParent: TWinControl);
begin
  FDetailControlPrevParent := AParent;
end;

procedure TdxTileControlItemDetailOptions.RestoreDetailControlPrevParent;
var
  AInfo: TdxDetailControlInfo;
begin
  if FDetailControl <> nil then
  begin
     FDetailControl.Parent := FDetailControlPrevParent;
     AInfo := GetDetailsInfo.FindByControl(FDetailControl);
     if AInfo <> nil then
     begin
       FDetailControl.Align := AInfo.Align;
       FDetailControl.BoundsRect := AInfo.Bounds;
     end;
  end;
end;

{ TdxTileControlItemGalleryCell }

constructor TdxTileControlItemGalleryCell.Create(ATileItem: TdxTileControlItem;
  AController: TdxTileControlGalleryCellController; const ARow, AColumn: Byte; const ABounds: TRect);
begin
  inherited Create;
  FTileItem := ATileItem;
  FController := AController;
  FColumn := AColumn;
  FRow := ARow;
  FBounds := ABounds;
  FCurrentImage := TdxSmartImage.Create;
  FNextImage := TdxSmartImage.Create;
end;

destructor TdxTileControlItemGalleryCell.Destroy;
begin
  FreeAndNil(FCurrentImage);
  FreeAndNil(FNextImage);
  inherited Destroy;
end;

function TdxTileControlItemGalleryCell.CreateResultingImage(AImage: TdxSmartImage): TdxSmartImage;
var
  ACanvas: TdxGPCanvas;
begin
  Result := TdxSmartImage.CreateSize(cxRectSize(Bounds));
  DrawEmptyCell(Result);
  if not AImage.Empty then
  begin
    ACanvas := Result.CreateCanvas;
    try
      ACanvas.Draw(AImage, Result.ClientRect, AImage.ClientRect);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TdxTileControlItemGalleryCell.DoDraw(ACanvas: TcxCanvas);
var
  AImage: TdxSmartImage;
begin
  if IsAnimated then
    Animation.Draw(ACanvas, Bounds)
  else
  begin
    AImage := CreateResultingImage(CurrentImage);
    try
      ACanvas.Draw(Bounds.Left, Bounds.Top, AImage);
    finally
      AImage.Free;
    end;
  end;
end;

procedure TdxTileControlItemGalleryCell.DrawEmptyCell(AImage: TdxSmartImage);
var
  ABitmap: TcxBitmap;
  AColor: TColor;
begin
  ABitmap := TcxBitmap.CreateSize(Bounds, pf24bit);
  try
    AColor := GetEmptyCellBasicColor;
    FillGradientRect(ABitmap.cxCanvas.Handle, cxRectOffset(Bounds, Bounds.TopLeft, False), AColor, AColor, True);
    AImage.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

function TdxTileControlItemGalleryCell.GetAnimation: TdxTileControlGalleryCellAnimation;
begin
  Result := TileItem.ViewInfo.ContentAnimation as TdxTileControlGalleryCellAnimation;
end;

function TdxTileControlItemGalleryCell.GetEmptyCellBasicColor: TColor;
begin
  Result := TileItem.Style.GradientBeginColor;
  if Result = clDefault then
    Result := dxTileItemShadowColor;
  if Odd(Row + Column) then
    Result := dxGetDarkerColor(Result, 80)
  else
    Result := dxGetLighterColor(Result, 80);
end;

function TdxTileControlItemGalleryCell.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := False;
end;

function TdxTileControlItemGalleryCell.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileItem.TileControl.ScaleFactor;
end;

function TdxTileControlItemGalleryCell.GetVisibleBounds: TRect;
begin
  Result := cxRectOffset(TileItem.ViewInfo.VisibleBounds, TileItem.ViewInfo.Bounds.TopLeft, False);
end;

function TdxTileControlItemGalleryCell.IsAnimated: Boolean;
begin
  Result := (Animation <> nil) and (Animation.Cell = Self);
end;

procedure TdxTileControlItemGalleryCell.ReplaceCurrentImageByNextAndCleanNextImage;
begin
  CurrentImage.Assign(NextImage);
  NextImage.Clear;
end;

{ TdxTileControlItem }

constructor TdxTileControlItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveTransitions := TcxObjectList.Create;
  FFrames := CreateFrames;
  FViewInfo := CreateViewInfo;
  FAnimationTimer := TcxTimer.Create(Self);
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := AnimationTimerHandler;
  FAnimationMode := dxTileControlDefaultAnimationMode;
  FRowCount := dxTileControlDefaultItemRowCount;
  FActiveFrameIndex := -1;
  AnimationInterval := dxTileControlDefaultAnimationInterval;
  FEnabled := True;
  FDetailOptions := CreateDetailOptions;
  FSize := tcisRegular;
end;

destructor TdxTileControlItem.Destroy;
begin
  StopTransitions;
  Controller.ItemDestroyed(Self);
  Group := nil;
  GalleryImages := nil;
  FreeAndNil(FGalleryCellController);
  FreeAndNil(FDetailOptions);
  FreeAndNil(FFrames);
  FreeAndNil(FAnimationTimer);
  FreeAndNil(FViewInfo);
  FreeAndNil(FActiveTransitions);
  inherited Destroy;
end;

procedure TdxTileControlItem.Assign(ASource: TPersistent);
var
  AItem: TdxTileControlItem;
begin
  inherited Assign(ASource);
  if ASource is TdxTileControlItem then
  begin
    AItem := TdxTileControlItem(ASource);
    FAnimationInterval := AItem.AnimationInterval;
    FAnimationMode := AItem.AnimationMode;
    FDetailOptions := AItem.DetailOptions;
    FEnabled := AItem.Enabled;
    FSize := AItem.Size;
    FRowCount := AItem.RowCount;
    FGalleryImages := AItem.GalleryImages;
    GroupIndex := AItem.GroupIndex;
    IndexInGroup := AItem.IndexInGroup;
  end;
end;

procedure TdxTileControlItem.ActivateDetail;
begin
  if TileControl.CanDeactivateDetail(TileControl.ActiveDetail) then
    DetailOptions.DetailSite.ExecuteActivating;
end;

procedure TdxTileControlItem.ActiveFrameChanged;
begin
  if ActiveFrame = nil then Exit;
  if not IsAnimationActive then
    ViewInfo.ViewData := CheckViewDataInitialized(ActiveFrame.ViewData)
  else
    Controller.AnimateItemContent(Self);
  DoActiveFrameChanged;
end;

procedure TdxTileControlItem.AddTransition(ATransition: TdxAnimationTransition);
begin
  ActiveTransitions.Add(ATransition);
end;

procedure TdxTileControlItem.AnimationTimerHandler(Sender: TObject);

  procedure DoGalleryAnimation;
  begin
    if GalleryCellController.IsPreparedCellReady then
    begin
      Controller.AnimateItemContent(Self);
      RestoreAnimationInterval;
    end
    else
    begin
      GalleryCellController.CheckPreparedCells;
      SetTimerHandlerFastestRepeat;
    end;
  end;

begin
  if IsAnimationActive and HasAnimatedContent then
    if CanGalleryAnimation then
      DoGalleryAnimation
    else
    begin
      FActiveFrameIndex := Max(-1, Min(VisibleFramesCount - 1, FActiveFrameIndex));
      if FActiveFrameIndex = VisibleFramesCount - 1 then
        ActiveFrameIndex := 0
      else
        ActiveFrameIndex := ActiveFrameIndex + 1;
    end;
end;

function TdxTileControlItem.CanDisplayGallery: Boolean;
begin
  Result := ItemType = tcitGallery;
end;

function TdxTileControlItem.CanFocused: Boolean;
begin
  Result := IsGroupEnabled;
end;

function TdxTileControlItem.CanGalleryAnimation: Boolean;
begin
  Result := not(csDestroying in ComponentState) and not(csDestroying in TileControl.ComponentState) and
    CanDisplayGallery and (GalleryCellController <> nil);
end;

procedure TdxTileControlItem.Changed;
begin
  ViewInfo.Calculated := False;
  ViewInfo.IsDirty := True;
  ViewData.IsDirty := True;
  CheckGalleryCellController;
  Frames.ForEach(RefreshFrame, nil);
  inherited Changed;
end;

procedure TdxTileControlItem.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Frames.ChangeScale(M, D);
end;

procedure TdxTileControlItem.CheckTimerEnabled;
begin
  AnimationTimer.Enabled := IsVisible and (AnimationInterval > 0) and (TileControl <> nil) and
    not TileControl.IsDesigning and not TileControl.IsLocked and
    not(csDestroying in TileControl.ComponentState) and (Controller.AnimationItems.IndexOf(Self) < 0);
end;

function TdxTileControlItem.CheckViewDataInitialized(
  AViewData: TdxTileControlCustomItemViewData): TdxTileControlCustomItemViewData;
begin
  if (AViewData <> ViewData) and (AViewData <> nil) then
    DoFrameInitialize(AViewData.Item as TdxTileControlItemFrame);
  Result := AViewData;
end;

function TdxTileControlItem.ConvertToSize(AIsLarge: Boolean): TdxTileControlItemSize;
begin
  if not AIsLarge then
    Result := tcisRegular
  else
    if RowCount = 2 then
      Result := tcisExtraLarge
    else
      Result := tcisLarge;
end;

function TdxTileControlItem.CreateDetailOptions: TdxTileControlItemDetailOptions;
begin
  Result := GetDetailOptionsClass.Create(Self);
end;

function TdxTileControlItem.CreateDetailSite: TdxTileControlDetailSite;
begin
  Result := TdxTileControlDetailSite.Create(Self);
end;

function TdxTileControlItem.CreateFrames: TdxTileControlItemFrames;
begin
  Result := TdxTileControlItemFrames.Create(Self, TdxTileControlItemFrame);
end;

function TdxTileControlItem.CreateGlyph: TdxTileControlItemCustomGlyph;
begin
  Result := TdxTileControlItemGlyph.Create(Self);
end;

function TdxTileControlItem.CreateOptionsAnimate: TdxTileControlItemOptionsAnimate;
begin
  Result := TdxTileControlItemOptionsAnimate.Create(Self);
end;

function TdxTileControlItem.CreateViewInfo: TdxTileControlItemViewInfo;
begin
  Result := TdxTileControlItemViewInfo.Create(Self);
end;

procedure TdxTileControlItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsLarge', ReadItemIsLarge, nil, True);
end;

procedure TdxTileControlItem.DoActivateDetail(ADetail: TdxTileControlDetailSite);
var
  AActiveControl: TWinControl;
begin
  AActiveControl := ADetail.ActiveControl;
  if AActiveControl <> nil then
  begin
    if (AActiveControl.Parent <> ADetail) or (DetailOptions.FDetailControlPrevParent = nil) then
      DetailOptions.StoreDetailControlPrevParent(AActiveControl.Parent);
    AActiveControl.Parent := ADetail;
  end;
  if Assigned(TileControl.OnItemActivateDetail) then
    TileControl.OnItemActivateDetail(TileControl, Self);
  if Assigned(FOnActivateDetail) then
    FOnActivateDetail(Self);
end;

procedure TdxTileControlItem.DoActiveFrameChanged;
begin
  if Assigned(FOnActiveFrameChanged) then
    FOnActiveFrameChanged(Self);
end;

procedure TdxTileControlItem.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
  if not TileControl.IsDesigning and DetailOptions.HasDetail then
    ActivateDetail;
end;

procedure TdxTileControlItem.DoDeactivateDetail;
begin
  if Assigned(TileControl.OnItemDeactivateDetail) then
    TileControl.OnItemDeactivateDetail(TileControl, Self);
  if Assigned(FOnDeactivateDetail) then
    FOnDeactivateDetail(Self);
end;

procedure TdxTileControlItem.DoFrameDestroy(AFrame: TdxTileControlItemFrame);
begin
  StopTransitions;
  if (ViewInfo <> nil) and (ViewInfo.ViewData = AFrame.ViewData) then
    ViewInfo.ViewData := nil;
  if Assigned(FOnFrameDestroy) then
    FOnFrameDestroy(AFrame);
end;

procedure TdxTileControlItem.DoFrameInitialize(AFrame: TdxTileControlItemFrame);
begin
  if Assigned(FOnFrameInitialize) then
    FOnFrameInitialize(AFrame);
end;

procedure TdxTileControlItem.FrameChanged(AFrame: TdxTileControlCustomItem);
begin
  if AFrame = ActiveFrame then
    Invalidate;
end;

function TdxTileControlItem.GetActualViewData: TdxTileControlCustomItemViewData;
begin
  Result := ViewData;
  if TileControl.IsDesigning or (ActiveFrame = nil) or CanDisplayGallery then Exit;
  FActiveFrameIndex := ActiveFrame.Index;
  Result := CheckViewDataInitialized(ActiveFrame.ViewData);
end;

function TdxTileControlItem.GetClientBounds: TRect;
begin
  Result := cxRectOffset(ViewInfo.Bounds, ViewInfo.Bounds.TopLeft, False);
end;

function TdxTileControlItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := TdxCustomTileControl(AParent).Items;
end;

function TdxTileControlItem.GetDetailOptionsClass: TdxTileControlItemDetailOptionsClass;
begin
  Result := TdxTileControlItemDetailOptions;
end;

function TdxTileControlItem.GetTileItem: TdxTileControlItem;
begin
  Result := Self;
end;

function TdxTileControlItem.HasAnimatedContent: Boolean;
begin
  Result := (VisibleFramesCount > 0) or CanGalleryAnimation;
end;

function TdxTileControlItem.HasGalleryImages: Boolean;
begin
  Result := (GalleryImages <> nil) and (GalleryImages.Count > 0);
end;

procedure TdxTileControlItem.Invalidate;
begin
  if ViewInfo.Visible then
    TileControl.InvalidateRect(cxRectInflate(ViewInfo.GetActualVisualBounds, 1, 1), False);
end;

function TdxTileControlItem.IsDragged: Boolean;
begin
  if (TileControl.DragAndDropState = ddsInProcess) and
    (TileControl.IsItemDragged or (TileControl.ViewInfo.DragDropChanges <> nil)) then
    Result := ViewInfo = (TileControl.DragAndDropObject as TdxTileControlDragDropCustomObject).DragCell
  else
    Result := False;
end;

procedure TdxTileControlItem.OnEndGalleryCellAnimation(AAnimation: TdxTileControlGalleryCellAnimation);
begin
  ViewInfo.FContentAnimation := nil;
  Controller.AnimationItems.Remove(Self);
  RemoveTransition(AAnimation);
  if not(csDestroying in ComponentState) then
    ViewInfo.ViewData.PrepareViewData;
  GalleryCellController.FPreparedCell := nil;
  CheckTimerEnabled;
end;

procedure TdxTileControlItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (DetailOptions <> nil) and (Operation = opRemove) and (DetailOptions.DetailControl = AComponent) then
    DetailOptions.DetailControl := nil;
  if (AComponent is TcxImageCollectionItem) and (Operation = opRemove) and Assigned(ThreadScaler) then
    ThreadScaler.CancelTasks(AComponent as TcxImageCollectionItem);
end;

procedure TdxTileControlItem.NotifyBiDiModeChanged;
begin
  ViewInfo.NotifyBiDiModeChanged;
  Frames.ForEach(RefreshFrame, nil);
end;

procedure TdxTileControlItem.RemoveTransition(ATransition: TdxAnimationTransition);
begin
  ActiveTransitions.Remove(ATransition);
end;

procedure TdxTileControlItem.Refresh;
begin
  ViewInfo.IsDirty := True;
  Invalidate;
end;

procedure TdxTileControlItem.Click;
begin
  Controller.FocusedItem := Self;
  if IsEnabled then
    DoClick;
end;

procedure TdxTileControlItem.DeactivateDetail;
begin
  DetailOptions.DetailSite.DoDeactivate;
end;

procedure TdxTileControlItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to Frames.Count - 1 do
    if Frames[I].Owner = Root then
      Proc(Frames[I]);
end;

procedure TdxTileControlItem.MakeVisible;
begin
  if not Visible or TileControl.IsLocked then Exit;
  TileControl.ViewInfo.MakeVisible(ViewInfo.GetActualVisualBounds, IsMostLeft, IsMostRight);
  Invalidate;
end;

procedure TdxTileControlItem.Move(AGroupDest: TdxTileControlGroup; AIndexDest: Integer);
begin
  TileControl.BeginUpdate;
  try
    if Group <> AGroupDest then
    begin
      Group.RemoveItem(Self);
      AGroupDest.Add(Self);
    end;
    AGroupDest.MoveItem(Self, AIndexDest);
  finally
    TileControl.EndUpdate;
  end;
end;

function TdxTileControlItem.GetActiveFrame: TdxTileControlItemFrame;
var
  I, AIndex: Integer;
begin
  Result := nil;
  AIndex := Min(VisibleFramesCount - 1, Max(0, FActiveFrameIndex));
  if AIndex < 0 then Exit;
  for I := 0 to Frames.Count - 1 do
    if Frames[I].Visible then
    begin
      Result := Frames[I];
      Dec(AIndex);
      if AIndex < 0 then Break;
    end;
end;

function TdxTileControlItem.GetActuallyVisible: Boolean;
begin
  Result := FVisible and (Group <> nil) and Group.Visible;
end;

function TdxTileControlItem.GetController: TdxTileControlController;
begin
  if TileControl = nil then
    Result := nil
  else
    Result := TileControl.Controller;
end;

function TdxTileControlItem.GetGlyph: TdxTileControlItemGlyph;
begin
  Result := inherited Glyph as TdxTileControlItemGlyph;
end;

function TdxTileControlItem.GetGroupIndex: Integer;
begin
  Result := -1;
  if Group <> nil then
    Result := Group.Index;
end;

function TdxTileControlItem.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlItem.GetIndexInGroup: Integer;
begin
  if TileControl.IsLoading then
    Result := FIndexInGroupAtLoaded
  else
  begin
    Result := -1;
    if Group <> nil then
      Result := Group.IndexOfItem(Self);
  end;
end;

function TdxTileControlItem.GetIsAnimationActive: Boolean;
begin
  Result := IsEnabled and (Controller.ItemsContentAnimationLockCount = 0);
end;

function TdxTileControlItem.GetIsLarge: Boolean;
begin
  Result := Size in [tcisLarge, tcisExtraLarge];
end;

function TdxTileControlItem.GetIsMostLeft: Boolean;
begin
  Result := (Group <> nil) and Group.IsMostLeft;
  if Result then
    if GroupLayout = glHorizontal then
      Result := Result and (ViewInfo.Position.Column = 0)
    else
      Result := Result and (ViewInfo.Position.Row = 0);
end;

function TdxTileControlItem.GetIsMostRight: Boolean;
begin
  Result := (Group <> nil) and Group.IsMostRight;
  if Result then
    if GroupLayout = glHorizontal then
      Result := ViewInfo.GetLastOccupiedColumn(ViewInfo.Position.Column) = Group.RealColumnCount - 1
    else
      Result := ViewInfo.GetLastOccupiedRow(ViewInfo.Position.Row) = Group.RealRowCount - 1;
end;

function TdxTileControlItem.GetItemType: TdxTileControlItemType;
begin
  Result := tcitFrame;
  if HasGalleryImages and (Size <> tcisSmall) then
    Result := tcitGallery;
end;

function TdxTileControlItem.GetTileControl: TdxCustomTileControl;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TdxCustomTileControl(Collection.ParentComponent);
end;

function TdxTileControlItem.GetVisibleFramesCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  if Frames = nil then Exit;
  for I := 0 to Frames.Count - 1 do
    if Frames[I].Visible then
      Inc(Result);
end;

function TdxTileControlItem.IsDetailOptionsStored: Boolean;
begin
  Result := DetailOptions.IsChanged;
end;

function TdxTileControlItem.IsEnabled: Boolean;
begin
  Result := Enabled and IsGroupEnabled and TileControl.Enabled;
end;

function TdxTileControlItem.IsGroupEnabled: Boolean;
begin
  Result := (Group <> nil) and Group.Enabled;
end;

function TdxTileControlItem.IsGroupVisible: Boolean;
begin
  Result := (Group <> nil) and Group.Visible;
end;

function TdxTileControlItem.IsVisible: Boolean;
begin
  Result := Visible and IsGroupVisible and TileControl.Visible;
end;

procedure TdxTileControlItem.ReadItemIsLarge(AReader: TReader);
begin
  IsLarge := AReader.ReadBoolean;
end;

procedure TdxTileControlItem.RemoveFromGroup;
begin
  if Group <> nil then
    Group.RemoveItem(Self);
end;

procedure TdxTileControlItem.RefreshFrame(AItem: TdxTileControlItemFrame; const AData: Pointer);
begin
  AItem.Changed;
end;

procedure TdxTileControlItem.RestoreAnimationInterval;
begin
  AnimationTimer.Interval := AnimationInterval;
end;

procedure TdxTileControlItem.RestoreBeforeDragLayout;
begin
  Group := FGroupBeforeDrag;
  IndexInGroup := FIndexInGroupBeforeDrag;
end;

procedure TdxTileControlItem.SetOnActivateDetail(AValue: TdxTileControlItemEvent);
begin
  FOnActivateDetail := AValue;
  if Assigned(AValue) then
    DetailOptions.ShowTab := True;
end;

procedure TdxTileControlItem.SetActiveFrame(AValue: TdxTileControlItemFrame);
begin
  if AValue <> nil then
    ActiveFrameIndex := AValue.Index
  else
    ActiveFrameIndex := -1;
end;

procedure TdxTileControlItem.SetActiveFrameIndex(AValue: Integer);
begin
  AValue := Max(-1, Min(VisibleFramesCount - 1, AValue));
  if AValue <> FActiveFrameIndex then
    FActiveFrameIndex := AValue;
  ActiveFrameChanged;
end;

procedure TdxTileControlItem.SetAnimationInterval(AValue: Integer);
begin
  AnimationTimer.Enabled := False;
  FAnimationInterval := AValue;
  AnimationTimer.Interval := AValue;
  CheckTimerEnabled;
end;

procedure TdxTileControlItem.SetChecked(AValue: Boolean);
begin
  if (Controller.ItemCheckMode = tcicmNone) or not Enabled then
    AValue := False;
  if FChecked <> AValue then
  begin
    if TileControl.DoItemBeforeCheck(Self) then
    begin
      FChecked := AValue;
      Controller.ItemChecked(Self);
    end;
  end;
end;

procedure TdxTileControlItem.SetDetailOptions(AValue: TdxTileControlItemDetailOptions);
begin
  DetailOptions.Assign(AValue);
end;

procedure TdxTileControlItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Invalidate;
  end;
end;

procedure TdxTileControlItem.SetFrames(AValue: TdxTileControlItemFrames);
begin
  Frames.Assign(AValue);
end;

procedure TdxTileControlItem.SetGalleryImages(AValue: TcxImageCollection);
begin
  if GalleryImages <> AValue then
  begin
    if GalleryImages <> nil then
      GalleryImages.RemoveListener(Self);
    FGalleryImages := AValue;
    if GalleryImages <> nil then
      GalleryImages.AddListener(Self);
    ImageCollectionChanged;
  end;
end;

procedure TdxTileControlItem.SetGlyph(AValue: TdxTileControlItemGlyph);
begin
  inherited Glyph.Assign(AValue);
  Changed;
end;

procedure TdxTileControlItem.SetGroup(AValue: TdxTileControlGroup);
begin
  if FGroup <> AValue then
  begin
    TileControl.BeginUpdate;
    try
      if Group <> nil then
        Group.RemoveItem(Self);
      if AValue <> nil then
        AValue.Add(Self);
    finally
      TileControl.EndUpdate;
    end;
  end;
end;

procedure TdxTileControlItem.SetGroupIndex(AValue: Integer);
begin
  AValue := Min(Max(-1, AValue), TileControl.Groups.Count - 1);
  if AValue < 0 then
    SetGroup(nil)
  else
    SetGroup(TileControl.Groups[AValue]);
end;

procedure TdxTileControlItem.SetIndexInGroup(AValue: Integer);
begin
  if TileControl.IsLoading or TileControl.IsRestoring then
    FIndexInGroupAtLoaded := AValue
  else
    if Group <> nil then
    begin
      AValue := Min(Max(AValue, 0), Group.ItemCount - 1);
      if IndexInGroup <> AValue then
      begin
        TileControl.BeginUpdate;
        try
          Move(Group, AValue);
        finally
          TileControl.EndUpdate;
        end;
      end;
    end;
end;

procedure TdxTileControlItem.SetIsLarge(AValue: Boolean);
begin
  if IsLarge <> AValue then
  begin
    FSize := ConvertToSize(AValue);
    TileControl.LayoutChanged;
    Changed;
  end;
end;

procedure TdxTileControlItem.SetRowCount(AValue: Integer);
begin
  if TileControl.IsLoading then
    FRowCountAtLoaded := AValue
  else
  begin
    AValue := Min(Max(AValue, 1), dxTileControlItemMaxRowCount);
    if FRowCount <> AValue then
    begin
      FRowCount := AValue;
      if (FRowCount = 1) and (Size = tcisExtraLarge) then
        FSize := tcisLarge;
      if (FRowCount = 2) and (Size = tcisLarge) then
        FSize := tcisExtraLarge;
      TileControl.LayoutChanged;
      Changed;
    end;
  end;
end;

procedure TdxTileControlItem.SetSize(AValue: TdxTileControlItemSize);
const
  ARowCount: array[Boolean] of Integer = (1, 2);
begin
  if FSize <> AValue then
  begin
    FSize := AValue;
    FRowCount := ARowCount[FSize = tcisExtraLarge];
    Changed;
    TileControl.LayoutChanged;
  end;
end;

procedure TdxTileControlItem.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if TileControl.IsDesigning then
    Changed;
end;

procedure TdxTileControlItem.SetTimerHandlerFastestRepeat;
begin
  AnimationTimer.Interval := 60;
end;

procedure TdxTileControlItem.StopTransitions;
var
  I: Integer;
begin
  for I := ActiveTransitions.Count - 1 downto 0 do
    (FActiveTransitions[I] as TdxAnimationTransition).Terminate;
  ActiveTransitions.Clear;
end;

procedure TdxTileControlItem.StoreLayoutBeforeDrag;
begin
  FGroupBeforeDrag := Group;
  FIndexInGroupBeforeDrag := IndexInGroup;
end;

procedure TdxTileControlItem.ToggleChecked;
begin
  Checked := not Checked;
end;

function TdxTileControlItem.UseRightToLeftAlignment: Boolean;
begin
  Result := TileControl.ViewInfo.UseRightToLeftAlignment;
end;

// IcxStoredObject

function TdxTileControlItem.GetObjectName: string;
begin
  Result := Name;
end;

function TdxTileControlItem.GetProperties(AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  for I := Low(dxTileControlStoredItemPropertiesNames) to High(dxTileControlStoredItemPropertiesNames) do
    AProperties.Add(dxTileControlStoredItemPropertiesNames[I]);
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

function TdxTileControlItem.GetPropertyIndex(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(dxTileControlStoredItemPropertiesNames) to High(dxTileControlStoredItemPropertiesNames) do
  begin
    if AnsiCompareText(dxTileControlStoredItemPropertiesNames[I], AName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TdxTileControlItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if Assigned(OnGetStoredPropertyValue) then
    OnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TdxTileControlItem.SetPropertyValue(const AName: string; const AValue: Variant);
var
  AGroup: TdxTileControlGroup;
begin
  if (AName = 'IndexGroup') or (AName = 'IsLarge') then
  begin
    if AName = 'IndexGroup' then
    begin
      AGroup := TileControl.FindGroup(AValue);
      if AGroup <> nil then
        GroupIndex := AGroup.Index;
    end
    else
      Size := ConvertToSize(AValue);
  end
  else
    if Assigned(OnSetStoredPropertyValue) then
      OnSetStoredPropertyValue(Self, AName, AValue);
end;

procedure TdxTileControlItem.CheckGalleryCellController;
begin
  StopTransitions;
  if not CanDisplayGallery then
    FreeAndNil(FGalleryCellController)
  else
    if FGalleryCellController = nil then
      FGalleryCellController := TdxTileControlGalleryCellController.Create(Self);
end;

// IcxImageCollectionListener
procedure TdxTileControlItem.ImageCollectionChanged;
begin
  if not (csDestroying in ComponentState) then
    Changed;
end;

procedure TdxTileControlItem.ImageCollectionDestroyed;
begin
  GalleryImages := nil;
end;

{ TdxTileControlGalleryCells }

function TdxTileControlGalleryCells.CalculateHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := False;
end;

function TdxTileControlGalleryCells.GetCell(ARow, AColumn: Integer): TdxTileControlItemGalleryCell;
var
  I: Integer;
  ACell: TdxTileControlItemGalleryCell;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    ACell := Items[I] as TdxTileControlItemGalleryCell;
    if (ACell.Row = ARow) and (ACell.Column = AColumn) then
    begin
      Result := ACell;
      Break;
    end;
  end;
end;

{ TdxTileControlGalleryCellController }

constructor TdxTileControlGalleryCellController.Create(ATileItem: TdxTileControlItem);
begin
  inherited Create;
  FTileItem := ATileItem;
  FImageIndex := -1;
  FLastImageIndex := -1;
  FGalleryCells := TdxTileControlGalleryCells.Create(True);
  FPreparedCells := TdxTileControlGalleryCells.Create(False);
  FUntouchableCells := TdxTileControlGalleryCells.Create(False);
end;

destructor TdxTileControlGalleryCellController.Destroy;
begin
  if Assigned(ThreadScaler) then
    ThreadScaler.CancelTasks(OnEndBitmapRescale);
  FUntouchableCells.Clear;
  FreeAndNil(FUntouchableCells);
  FPreparedCells.Clear;
  FreeAndNil(FPreparedCells);
  FreeAndNil(FGalleryCells);
  inherited Destroy;
end;

procedure TdxTileControlGalleryCellController.AddAnimation;
var
  ABounds: TRect;
begin
  ABounds := cxRectOffset(PreparedCell.Bounds, CellsBounds.TopLeft);
  TileItem.Controller.AddAnimation(TdxTileControlGalleryCellAnimation.Create(TileItem, PreparedCell,
    TileControl.ViewInfo.GetBackgroundPart(ABounds)));
  PreparedCells.Remove(PreparedCell);
  UntouchableCells.Add(PreparedCell);
end;

function TdxTileControlGalleryCellController.AreCellsDirty: Boolean;
var
  R: TRect;
begin
  R := CellsBounds;
  Result := (ActualCellsHeight <> cxRectHeight(R)) or (ActualCellsWidth <> cxRectWidth(R));
end;

procedure TdxTileControlGalleryCellController.CalculateDimensions;
const
  AColumnCountPerRegularItem = 3;
  ARowCountPerRegularItem = 3;
var
  AColumnWidth, ARowHeight: Integer;
  R: TRect;
begin
  AColumnWidth := TileControl.OptionsView.ItemWidth div AColumnCountPerRegularItem;
  ARowHeight := TileControl.OptionsView.ItemHeight div ARowCountPerRegularItem;
  R := CellsBounds;
  FActualCellsHeight := cxRectHeight(R);
  FActualCellsWidth := cxRectWidth(R);
  FColumnCount := FActualCellsWidth div AColumnWidth;
  FRowCount := ActualCellsHeight div ARowHeight;
end;

procedure TdxTileControlGalleryCellController.CalculateImageIndexAndSize;

   function GetNextImageIndex: Integer;
   begin
     if FImageIndex > -1 then
       FLastImageIndex := FImageIndex;
     Result := FLastImageIndex + 1;
     if Random(100) >= 90 then
       Result := -1;
     if Result > GalleryImages.Count - 1 then
       Result := 0;
   end;

   procedure CalculateImageSize;
    const
      AImageSizes: array [Boolean] of Integer = (1, 2);
   begin
    if IsEmptyImage(FImageIndex) then
      FImageRowCount := 1
    else
    begin
      FImageRowCount := AImageSizes[Random(100) > 85];
    end;
    FImageColumnCount := FImageRowCount;
   end;

begin
  FImageIndex := GetNextImageIndex;
  CalculateImageSize;
end;

function TdxTileControlGalleryCellController.CanPopulatePreparedCellsAt(const ARow, AColumn: Byte): Boolean;
var
  I: Integer;
  ACell: TdxTileControlItemGalleryCell;
  ACellRow, ACellColumn: Byte;
begin
  Result := (ARow + ImageRowCount <= RowCount) and (AColumn + ImageColumnCount <= ColumnCount) and IsThereEmptyCells;
  if Result then
    for I := 0 to UntouchableCells.Count - 1 do
    begin
      ACell := UntouchableCells[I] as TdxTileControlItemGalleryCell;
      ACellRow := ACell.Row;
      ACellColumn := ACell.Column;
      Result := not((ACellRow in [ARow .. ARow + ImageRowCount - 1]) and
        (ACellColumn in [AColumn .. AColumn + ImageColumnCount - 1]));
      if not Result then
        Break;
    end;
end;

procedure TdxTileControlGalleryCellController.CheckPreparedCells;
begin
  if not GetThreadScaler.HasTaskForProc(OnEndBitmapRescale) then
    PrepareCells
end;

procedure TdxTileControlGalleryCellController.ChoosePreparedCell;
begin
  FPreparedCell := TdxTileControlItemGalleryCell(PreparedCells[Random(PreparedCells.Count)]);
end;

procedure TdxTileControlGalleryCellController.DrawCells(ACanvas: TcxCanvas; AWithoutClipping: Boolean);
begin
  if AWithoutClipping then
    GalleryCells.DrawWithoutClipping(ACanvas)
  else
    GalleryCells.Draw(ACanvas);
end;

function TdxTileControlGalleryCellController.GetCellsBounds: TRect;
begin
  Result := TileItem.ViewInfo.Bounds;
end;

function TdxTileControlGalleryCellController.GetGalleryImages: TcxImageCollection;
begin
  Result := TileItem.GalleryImages;
end;

function TdxTileControlGalleryCellController.GetImagesCount: Integer;
begin
  Result := ImageRowCount * ImageColumnCount;
end;

function TdxTileControlGalleryCellController.GetPreparedCellsCombinedBounds: TRect;
var
  I: Integer;
  ACell: TdxTileControlItemGalleryCell;
begin
  Result := PreparedCells[0].Bounds;
  for I := 1 to PreparedCells.Count - 1 do
  begin
    ACell := PreparedCells[I] as TdxTileControlItemGalleryCell;
    Result.Left := Min(Result.Left, ACell.Bounds.Left);
    Result.Top := Min(Result.Top, ACell.Bounds.Top);
    Result.Right := Max(Result.Right, ACell.Bounds.Right);
    Result.Bottom := Max(Result.Bottom, ACell.Bounds.Bottom);
  end;
end;

function TdxTileControlGalleryCellController.GetTileControl: TdxCustomTileControl;
begin
  Result := TileItem.TileControl;
end;

procedure TdxTileControlGalleryCellController.InitializeCells;
var
  ALeft, ATop, ARight, ABottom: Integer;
  ARow, AColumn: Byte;
  ACell: TdxTileControlItemGalleryCell;
begin
  if Assigned(ThreadScaler) then
    ThreadScaler.CancelTasks(OnEndBitmapRescale);
  TileItem.StopTransitions;
  GalleryCells.Clear;
  FPreparedCell := nil;
  PreparedCells.Clear;
  UntouchableCells.Clear;
  CalculateDimensions;
  for ARow := 0 to RowCount - 1 do
    for AColumn := 0 to ColumnCount - 1 do
    begin
      ALeft := MulDiv(ActualCellsWidth, AColumn, ColumnCount);
      ATop := MulDiv(ActualCellsHeight, ARow, RowCount);
      ARight := MulDiv(ActualCellsWidth, AColumn + 1, ColumnCount);
      ABottom := MulDiv(ActualCellsHeight, ARow + 1, RowCount);
      ACell := TdxTileControlItemGalleryCell.Create(TileItem, Self, ARow, AColumn, cxRect(ALeft, ATop, ARight, ABottom));
      ACell.Calculated := True;
      GalleryCells.Add(ACell);
    end;
  if TileItem.IsAnimationActive then
    PrepareCells;
end;

function TdxTileControlGalleryCellController.IsEmptyImage(AImageIndex: Integer): Boolean;
begin
  Result := (AImageIndex = -1) or not Assigned(GalleryImages.Items[FImageIndex].Picture.Graphic);
end;

function TdxTileControlGalleryCellController.IsOnlyOneCellPrepared: Boolean;
begin
  Result := PreparedCells.Count = 1;
end;

function TdxTileControlGalleryCellController.IsPreparedCellReady: Boolean;
begin
  Result := PreparedCell <> nil;
end;

function TdxTileControlGalleryCellController.IsThereEmptyCells: Boolean;
begin
  Result := UntouchableCells.Count + ImagesCount <= RowCount * ColumnCount;
end;

procedure TdxTileControlGalleryCellController.OnEndBitmapRescale(Sender: TObject);
var
  ATask: TdxThreadScalerTaskItem absolute Sender;
  AImage: TdxSmartImage;
begin
  ATask.ImageItem.RemoveFreeNotification(TileItem);
  AImage := ATask.Image;
  if IsOnlyOneCellPrepared then
    (PreparedCells[0] as TdxTileControlItemGalleryCell).NextImage.Assign(AImage)
  else
    SeparateImageIntoPreparedCells(AImage);
  ChoosePreparedCell;
end;

procedure TdxTileControlGalleryCellController.PopulatePreparedCells;
var
  ARow, AColumn, I, J, P: Byte;
begin
  P := 0;
  repeat
    if (P > 10) or (not IsThereEmptyCells) then
    begin
      RemoveFromUntouchableCells;
      P := 0;
    end;
    ARow := Random(RowCount);
    AColumn := Random(ColumnCount);
    Inc(P);
  until CanPopulatePreparedCellsAt(ARow, AColumn);
  for I := ARow to ARow + ImageRowCount - 1 do
    for J := AColumn to AColumn + ImageColumnCount - 1 do
      PreparedCells.Add(GalleryCells.Cells[I, J]);
end;

procedure TdxTileControlGalleryCellController.PrepareCells;
begin
  if PreparedCells.Count = 0 then
  begin
    FPreparedCell := nil;
    CalculateImageIndexAndSize;
    PopulatePreparedCells;
    SetPreparedCellsBackImages;
  end
  else
    ChoosePreparedCell;
end;

procedure TdxTileControlGalleryCellController.RecalculateCells;
var
  I: Integer;
begin
  for I := 0 to GalleryCells.Count - 1 do
    GalleryCells[I].Recalculate;
end;

procedure TdxTileControlGalleryCellController.RemoveFromUntouchableCells;
begin
  if ImagesCount = 1 then
    UntouchableCells.Delete(0)
  else
    UntouchableCells.Clear;
end;

procedure TdxTileControlGalleryCellController.SeparateImageIntoPreparedCells(AImage: TdxSmartImage);
var
  ACanvas: TdxGPCanvas;
  ACell: TdxTileControlItemGalleryCell;
  ACellImage: TdxSmartImage;
  AOrigin: TPoint;
  I: Integer;
begin
  AOrigin := PreparedCells[0].Bounds.TopLeft;
  for I := 0 to PreparedCells.Count - 1 do
  begin
    ACell := PreparedCells[I] as TdxTileControlItemGalleryCell;
    ACellImage := TdxSmartImage.CreateSize(ACell.Bounds);
    try
      ACanvas := ACellImage.CreateCanvas;
      try
        ACanvas.Clear(clNone);
        ACanvas.Draw(AImage, ACellImage.ClientRect, cxRectOffset(ACell.Bounds, AOrigin, False));
      finally
        ACanvas.Free;
      end;
      ACell.NextImage.Assign(ACellImage);
    finally
      ACellImage.Free;
    end;
  end;
end;

procedure TdxTileControlGalleryCellController.SetPreparedCellsBackImages;
var
  AImageItem: TcxImageCollectionItem;
  ACell: TdxTileControlItemGalleryCell;
begin
  if IsEmptyImage(FImageIndex) then
  begin
    ACell := PreparedCells[0] as TdxTileControlItemGalleryCell;
    ACell.NextImage.Clear;
    FPreparedCell := ACell;
  end
  else
  begin
    AImageItem := GalleryImages.Items[FImageIndex];
    AImageItem.FreeNotification(TileItem);
    GetThreadScaler.AddTask(AImageItem, cxRectSize(GetPreparedCellsCombinedBounds), OnEndBitmapRescale);
  end;
end;

{ TdxTileControlCustomStoredCollection }

procedure TdxTileControlCustomStoredCollection.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TdxTileControlCustomStoredCollection.GetItemPrefixName: string;
begin
  Result := 'TdxTileControl';
end;

function TdxTileControlCustomStoredCollection.GetTileControl: TdxCustomTileControl;
begin
  Result := TdxCustomTileControl(ParentComponent);
end;

// IInterface
function TdxTileControlCustomStoredCollection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

function TdxTileControlCustomStoredCollection._AddRef: Integer;
begin
  Result := -1;
end;

function TdxTileControlCustomStoredCollection._Release: Integer;
begin
  Result := -1;
end;

// IcxStoredObject }

function TdxTileControlCustomStoredCollection.GetObjectName: string;
begin
  Result := '';
end;

function TdxTileControlCustomStoredCollection.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
end;

procedure TdxTileControlCustomStoredCollection.GetPropertyValue(const AName: string; var AValue: Variant);
begin
end;

procedure TdxTileControlCustomStoredCollection.SetPropertyValue
  (const AName: string; const AValue: Variant);
begin
end;

// IcxStoredParent
function TdxTileControlCustomStoredCollection.StoredCreateChild(const AObjectName, AClassName: string): TObject;
begin
  Result := nil;
end;

procedure TdxTileControlCustomStoredCollection.StoredDeleteChild(const AObjectName: string; AObject: TObject);
begin
  Remove(AObject as TcxComponentCollectionItem);
end;

procedure TdxTileControlCustomStoredCollection.StoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AChildren.AddObject('', Items[I]);
end;

{ TdxTileControlItemCollection }

function TdxTileControlItemCollection.Add: TdxTileControlItem;
begin
  Result := inherited Add as TdxTileControlItem;
end;

procedure TdxTileControlItemCollection.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

procedure TdxTileControlItemCollection.ForEach(AProc: TdxTileControlForEachItemProc; const AData: Pointer;
  AVisibleOnly: Boolean = True);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ActuallyVisible or not AVisibleOnly then
      AProc(Items[I], AData);
end;

procedure TdxTileControlItemCollection.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RowCount := Items[I].FRowCountAtLoaded;
end;

function TdxTileControlItemCollection.GetItem(AIndex: Integer): TdxTileControlItem;
begin
  Result := TdxTileControlItem(inherited GetItem(AIndex));
end;

procedure TdxTileControlItemCollection.SetItem(AIndex: Integer; const AValue: TdxTileControlItem);
begin
  inherited SetItem(AIndex, AValue);
end;

// IcxStoredObject }
function TdxTileControlItemCollection.GetObjectName: string;
begin
  Result := 'Items';
end;

// IcxStoredParent implementation
function TdxTileControlItemCollection.StoredCreateChild(const AObjectName, AClassName: string): TObject;
var
  AItem: TdxTileControlItem;
begin
  if AClassName = 'TdxTileControlItem' then
  begin
    AItem := (ParentComponent as TdxCustomTileControl).Items.Add;
    AItem.Group := nil;
    Result := AItem;
    TileControl.DoInitStoredObject(Result);
  end
  else
    Result := inherited StoredCreateChild(AObjectName, AClassName);
end;

procedure TdxTileControlItemCollection.StoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  if TileControl.StoredVersion > 0 then
    inherited StoredChildren(AChildren)
  else
    for I := 0 to Count - 1 do
      if Items[I].Group = nil then
        AChildren.AddObject('', Items[I]);
end;

{ TdxTileControlGroupCaptionViewInfo }

constructor TdxTileControlGroupCaptionViewInfo.Create(AOwner: TdxTileControlGroupCaption);
begin
  inherited Create;
  FCaption := AOwner;
  FFont := TFont.Create;
end;

destructor TdxTileControlGroupCaptionViewInfo.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxTileControlGroupCaptionViewInfo.CalculateBounds;
begin
  Bounds := cxRectInflate(Group.Bounds, -ScaleFactor.Apply(1));
  FBounds.Bottom := Bounds.Top + Height;
  if Height = 0 then Exit;
  FTextBounds := GetTextBounds(Bounds);
end;

procedure TdxTileControlGroupCaptionViewInfo.DoCalculate;
begin
  CalculateBounds;
  inherited DoCalculate;
end;

procedure TdxTileControlGroupCaptionViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if not Group.Visible or (Group = TileControl.DraggedGroup) or IsAnimatedOnDragDrop then Exit;
  DrawContent(ACanvas, TextBounds);
end;

procedure TdxTileControlGroupCaptionViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Group.SynchronizeEditorPos;
  inherited Draw(ACanvas);
end;

procedure TdxTileControlGroupCaptionViewInfo.DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect);
var
  ABounds: TRect;
  ATextColor: TColor;
begin
  ABounds := ADrawRect;
  if Caption.Color <> clDefault then
    ACanvas.FillRect(ABounds, Caption.Color);
  if Caption.Text <> '' then
  begin
    ATextColor := TextColor;
    if not Group.Enabled then
      ATextColor := dxGetLighterColor(ATextColor, 50);
    cxTextOut(ACanvas.Canvas, Caption.Text, ABounds, GetTextOutFlags, 0, 0, Font, clNone, clNone, 0, 0, 0, ATextColor);
  end;
end;

function TdxTileControlGroupCaptionViewInfo.GetGroup: TdxTileControlGroup;
begin
  Result := Caption.Group;
end;

function TdxTileControlGroupCaptionViewInfo.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TdxTileControlGroupCaptionViewInfo.GetBaseDrawBounds: TRect;
begin
  Result := GetTextBounds(inherited GetBaseDrawBounds);
end;

function TdxTileControlGroupCaptionViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtGroupCaption] := True;
end;

function TdxTileControlGroupCaptionViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := TileControl.Painter;
end;

function TdxTileControlGroupCaptionViewInfo.GetTextBounds(ABounds: TRect): TRect;
begin
  Result := ABounds;
  Result.Bottom := Result.Bottom - FIndent;
end;

function TdxTileControlGroupCaptionViewInfo.GetTextColor: TColor;
begin
  Result := cxGetActualColor(Font.Color, Painter.GroupCaptionDefaultTextColor);
end;

function TdxTileControlGroupCaptionViewInfo.GetTextOutFlags: Integer;
const
  AHorzAlign: array[TAlignment] of TcxTextAlignX = (taLeft, taRight, taCenterX);
var
  AHorzAlignment: TcxTextAlignX;
begin
  AHorzAlignment := AHorzAlign[Caption.Alignment];
  if TileControl.UseRightToLeftAlignment then
    AHorzAlignment := TdxRightToLeftLayoutConverter.ConvertcxTextAlignX(AHorzAlignment);
  Result := cxMakeFormat(AHorzAlignment, taBottom) or CXTO_PREVENT_TOP_EXCEED or CXTO_END_ELLIPSIS or CXTO_EXPANDTABS;
end;

function TdxTileControlGroupCaptionViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Caption.TileControl;
end;

function TdxTileControlGroupCaptionViewInfo.GetVisibleBounds: TRect;
begin
  Result := TileControl.ViewInfo.TilesArea;
end;

procedure TdxTileControlGroupCaptionViewInfo.MeasureHeight;
var
  ATextHeight: Integer;
begin
  FHeight := 0;
  FIndent := TileControl.OptionsView.ItemIndent;
  TileControl.Painter.ValidatePainterData;
  Font.Assign(Caption.Font);
  AdjustFontHeight(Font, ScaleFactor, dxTileControlDefaultGroupCaptionFontSize, Painter.GroupCaptionDefaultFontSize);
  if TileControl.OptionsBehavior.GroupRenaming then
    ATextHeight := Max(cxTextHeight(Font, 'I'), cxTextHeight(Font, Caption.Text))
  else
    ATextHeight := cxTextHeight(Font, Caption.Text);
  if ATextHeight > 0 then
    Height := FIndent + ATextHeight;
end;

procedure TdxTileControlGroupCaptionViewInfo.Offset(const DX, DY: Integer);
begin
  FTextBounds := cxRectOffset(FTextBounds, DX, DY);
  inherited Offset(DX, DY);
end;

procedure TdxTileControlGroupCaptionViewInfo.SetHeight(AValue: Integer);
begin
  FHeight := Max(FHeight, AValue);
end;

procedure TdxTileControlGroupCaptionViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, AClientBounds);
end;

{ TdxTileControlGroupCaption }

constructor TdxTileControlGroupCaption.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGroup := AOwner as TdxTileControlGroup;
  FAlignment := taLeftJustify;
  FColor := clDefault;
  FFont := TFont.Create;
  FFont.Color := clDefault;
  FFont.Name := DefaultFontName;
  FFont.Height := dxGetFontHeightForDefaultDPI(dxTileControlDefaultGroupCaptionFontSize);
  FFont.OnChange := FontChanged;
  FViewInfo := TdxTileControlGroupCaptionViewInfo.Create(Self);
end;

destructor TdxTileControlGroupCaption.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxTileControlGroupCaption.Assign(ASource: TPersistent);
var
  ACaption: TdxTileControlGroupCaption;
begin
  if ASource is TdxTileControlGroupCaption then
  begin
    ACaption := TdxTileControlGroupCaption(ASource);
    FAlignment := ACaption.Alignment;
    Font := ACaption.Font;
    FText := ACaption.Text;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlGroupCaption.Changed;
begin
  TileControl.LayoutChanged;
end;

procedure TdxTileControlGroupCaption.ChangeScale(M, D: Integer);
var
  APrevFontChanged: Boolean;
begin
  APrevFontChanged := IsFontStored;
  Font.Height := MulDiv(Font.Height, M, D);
  FFontChanged := APrevFontChanged;
end;

procedure TdxTileControlGroupCaption.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
  Changed;
end;

function TdxTileControlGroupCaption.GetBounds: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TdxTileControlGroupCaption.GetHeight: Integer;
begin
  Result := ViewInfo.Height;
end;

function TdxTileControlGroupCaption.GetTileControl: TdxCustomTileControl;
begin
  Result := FGroup.TileControl;
end;

function TdxTileControlGroupCaption.IsChanged: Boolean;
begin
  Result := IsFontStored or (Alignment <> taLeftJustify) or (Text <> '') or (Color <> clDefault);
end;

function TdxTileControlGroupCaption.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxTileControlGroupCaption.MeasureHeight;
begin
  ViewInfo.MeasureHeight;
end;

procedure TdxTileControlGroupCaption.SetAlignment(AValue: TAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    Changed;
  end;
end;

procedure TdxTileControlGroupCaption.SetColor(AValue: TColor);
begin
  if AValue <> FColor then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlGroupCaption.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue)
end;

procedure TdxTileControlGroupCaption.SetHeight(AHeight: Integer);
begin
  ViewInfo.Height := AHeight;
end;

procedure TdxTileControlGroupCaption.SetText(AValue: string);
begin
  AValue := Trim(AValue);
  if AValue <> FText then
  begin
    FText := AValue;
    Changed;
  end;
end;

{ TdxTileControlGroupCollection }

function TdxTileControlGroupCollection.Add: TdxTileControlGroup;
begin
  Result := inherited Add as TdxTileControlGroup;
end;

function TdxTileControlGroupCollection.Insert(AIndex: Integer): TdxTileControlGroup;
begin
  Result := inherited Insert(AIndex) as TdxTileControlGroup;
end;

procedure TdxTileControlGroupCollection.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

function TdxTileControlGroupCollection.GetNextVisibleGroup(AGroup: TdxTileControlGroup): TdxTileControlGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := AGroup.Index + 1 to Count - 1 do
    if Items[I].Visible then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxTileControlGroupCollection.GetPrevVisibleGroup(AGroup: TdxTileControlGroup): TdxTileControlGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := AGroup.Index - 1 downto 0 do
    if Items[I].Visible then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxTileControlGroupCollection.GetItem(AIndex: Integer): TdxTileControlGroup;
begin
  Result := TdxTileControlGroup(inherited GetItem(AIndex));
end;

function dxCompareItemsByIndexInGroup(AItem1, AItem2: TdxTileControlItem): Integer;
begin
  Result := AItem1.FIndexInGroupAtLoaded - AItem2.FIndexInGroupAtLoaded;
end;

procedure TdxTileControlGroupCollection.Loaded;
var
  I: Integer;
begin
  // sort and reindex after loaded
  for I := 0 to Count - 1 do
    with Items[I] do
      FItems.Sort(@dxCompareItemsByIndexInGroup);
end;

procedure TdxTileControlGroupCollection.SetItem(AIndex: Integer;
  const AValue: TdxTileControlGroup);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TdxTileControlGroupCollection.Update(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  inherited Update(AItem, AAction);
  TileControl.LayoutChanged;
end;

// IcxStoredObject }

function TdxTileControlGroupCollection.GetObjectName: string;
begin
  Result := 'Groups';
end;

// IcxStoredParent
function TdxTileControlGroupCollection.StoredCreateChild(const AObjectName,
  AClassName: string): TObject;
begin
  if AClassName = 'TdxTileControlGroup' then
  begin
    Result := Add;
    TileControl.DoInitStoredObject(Result);
  end
  else
    Result := inherited StoredCreateChild(AObjectName, AClassName);
end;

procedure TdxTileControlGroupCollection.StoredDeleteChild(const AObjectName: string;
  AObject: TObject);
begin
  AObject.Free;
end;

{ TdxTileControlStyle }

constructor TdxTileControlStyle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFocusedColor := clDefault;
  FCheckedItemCheckMarkColor := clDefault;
  FCheckedItemFrameColor := clDefault;
end;

procedure TdxTileControlStyle.Assign(ASource: TPersistent);
var
  AStyle: TdxTileControlStyle;
begin
  if ASource is TdxTileControlStyle then
  begin
    AStyle := TdxTileControlStyle(ASource);
    FocusedColor := AStyle.FocusedColor;
    CheckedItemCheckMarkColor := AStyle.CheckedItemCheckMarkColor;
    CheckedItemFrameColor := AStyle.CheckedItemFrameColor;
  end;
  inherited Assign(ASource);
end;

procedure TdxTileControlStyle.SetCheckedItemCheckMarkColor(AValue: TColor);
begin
  if AValue <> FCheckedItemCheckMarkColor then
  begin
    FCheckedItemCheckMarkColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlStyle.SetCheckedItemFrameColor(AValue: TColor);
begin
  if AValue <> FCheckedItemFrameColor then
  begin
    FCheckedItemFrameColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlStyle.SetFocusedColor(AValue: TColor);
begin
  if FFocusedColor <> AValue then
  begin
    FFocusedColor := AValue;
    Changed;
  end;
end;

{ TdxTileControlCustomDesignHelper }

constructor TdxTileControlCustomDesignHelper.Create(AControl: TdxCustomTileControl);
begin
  inherited Create;
end;

function TdxTileControlCustomDesignHelper.GetControl: TdxCustomTileControl;
begin
  Result := nil;
end;

{ TdxTileControlHintController }

constructor TdxTileControlHintController.Create(AOwner: TdxTileControlController);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxTileControlHintController.GetTileControl: TdxCustomTileControl;
begin
  Result := FOwner.TileControl;
end;

{ TdxTileControlInflateItemAnimation }

constructor TdxTileControlInflateItemAnimation.Create(ATileControl: TdxCustomTileControl; AItem: TdxTileControlItem;
  const ADelta, ACount: Integer);
begin
  inherited Create(dxTileControlInflateAnimationTime, ateLinear, ACount);
  FTileControl := ATileControl;
  FItem := AItem;
  FDelta := ADelta;
  if Item <> nil then
    Item.AddTransition(Self);
end;

destructor TdxTileControlInflateItemAnimation.Destroy;
begin
  if Item <> nil then
    Item.RemoveTransition(Self);
  inherited Destroy;
end;

procedure TdxTileControlInflateItemAnimation.InflateItem(AItem: TdxTileControlItem; const AData: Pointer);
begin
  if AItem.IsGroupEnabled then
    AItem.ViewInfo.InflateDelta := Integer(AData);
end;

procedure TdxTileControlInflateItemAnimation.DoAnimate;
begin
  if Item = nil then
    TileControl.Items.ForEach(InflateItem, Pointer(Position * FDelta))
  else
    InflateItem(Item, Pointer(Position * FDelta));
  TileControl.Update;
end;

{ TdxTileControlFramesAnimation }

constructor TdxTileControlFramesAnimation.Create(AItem: TdxTileControlItem);
begin
  FItem := AItem;
  InitializeViewData;
  inherited Create(SourceViewData.Image, DestViewData.Image, dxTileControlFramesAnimationTime,
    Item.AnimationMode, ateAccelerateDecelerate);
  Item.ViewInfo.ViewData := DestViewData;
  Item.ViewInfo.FContentAnimation := Self;
  Item.AddTransition(Self);
end;

destructor TdxTileControlFramesAnimation.Destroy;
begin
  Item.ViewInfo.FContentAnimation := nil;
  Item.ViewInfo.ViewData.NeedDrawTextOnImage := True;
  Item.Controller.AnimationItems.Remove(Item);
  Item.CheckTimerEnabled;
  Item.RemoveTransition(Self);
  inherited Destroy;
end;

procedure TdxTileControlFramesAnimation.InitializeViewData;
begin
  FDestViewData := Item.CheckViewDataInitialized(Item.ActiveFrame.ViewData);
  FDestViewData.NeedDrawTextOnImage := DestViewData.Item.OptionsAnimate.AnimateText;
  FSourceViewData := Item.CheckViewDataInitialized(Item.ViewInfo.ViewData);
  FSourceViewData.NeedDrawTextOnImage := DestViewData.NeedDrawTextOnImage;
end;

procedure TdxTileControlFramesAnimation.RefreshAnimatedObject;
begin
  Item.Refresh;
end;

{ TdxTileControlGalleryCellAnimation }

constructor TdxTileControlGalleryCellAnimation.Create(AItem: TdxTileControlItem; ACell: TdxTileControlItemGalleryCell;
  const ABackgroundImage: TcxBitmap);
begin
  FItem := AItem;
  FBackgroundImage := ABackgroundImage;
  FCell := ACell;
  FAssistImage := TcxBitmap.CreateSize(FBackgroundImage.Width, FBackgroundImage.Height);
  FCurrentImage := Cell.CreateResultingImage(Cell.CurrentImage);
  FNextImage := Cell.CreateResultingImage(Cell.NextImage);
  Cell.ReplaceCurrentImageByNextAndCleanNextImage;
  inherited Create(dxTileControlGalleryAnimationTime, ateAccelerateDecelerate, 180);
  Item.ViewInfo.FContentAnimation := Self;
  Item.AddTransition(Self);
end;

destructor TdxTileControlGalleryCellAnimation.Destroy;
begin
  Item.OnEndGalleryCellAnimation(Self);
  FreeAndNil(FAssistImage);
  FreeAndNil(FCurrentImage);
  FreeAndNil(FNextImage);
  FreeAndNil(FBackgroundImage);
  inherited Destroy;
end;

procedure TdxTileControlGalleryCellAnimation.Draw(ACanvas: TcxCanvas; const ADestRect: TRect);
var
  AHeight, AActualHeight, AMiddle: Integer;
  ARect: TRect;
begin
  FAssistImage.Assign(FBackgroundImage);
  AHeight := cxRectHeight(FAssistImage.ClientRect);
  AMiddle := Length div 2;
  AActualHeight := MulDiv(AHeight, Abs(AMiddle - Position), Length div 2);
  ARect := cxRectSetHeight(cxRectOffset(FAssistImage.ClientRect, 0, (AHeight - AActualHeight) div 2), AActualHeight);
  if Position < AMiddle then
    FAssistImage.Canvas.StretchDraw(ARect, FCurrentImage)
  else
    FAssistImage.Canvas.StretchDraw(ARect, FNextImage);
  ACanvas.Draw(ADestRect.Left, ADestRect.Top, FAssistImage);
end;

procedure TdxTileControlGalleryCellAnimation.RefreshAnimatedObject;
begin
  Item.Refresh;
end;

{ TdxTileControlDragAndDropAnimation }

constructor TdxTileControlDragAndDropAnimation.Create(AChanges: TdxTileControlDragAndDropChanges);
begin
  FChanges := AChanges;
  with FChanges.DragAndDropObject.DragImage do
    FDragImageOrigin := cxPointOffset(BoundsRect.TopLeft, PositionOffset);
  inherited Create(400, ateAccelerateDecelerate, AnimationLength);
end;

function TdxTileControlDragAndDropAnimation.GetTileControl: TdxCustomTileControl;
begin
  Result := Changes.TileControl;
end;

function TdxTileControlDragAndDropAnimation.AnimationLength: Integer;
begin
  Result := dxTileDropAnimationStepCount;
end;

procedure TdxTileControlDragAndDropAnimation.DoAnimate;
begin
  if ((Changes.ItemsInfo.Count = 0) and not Changes.DragObjectFinished) then
    Finished := True
  else
  begin
    Changes.SetAnimationProgress(Position div 100);
    if not Changes.DragObjectFinished and
        not cxPointIsEqual(GetMouseCursorPos, Changes.DragAndDropObject.PrevMousePos) then
      Changes.DragAndDropObject.ShowDragImage(GetMouseCursorPos);
    TileControl.ForceUpdate();
  end;
end;

{ TdxTileControlHottrackItemAnimation }

constructor TdxTileControlHottrackItemAnimation.Create(AItem: TdxTileControlItem; AShowFrame: Boolean = True);
begin
  FShowFrame := AShowFrame;
  FItem := AItem;
  AItem.AddTransition(Self);
  inherited Create(dxTileControlHotTrackItemAnimationTime, ateLinear, 255);
  AItem.Invalidate;
end;

destructor TdxTileControlHottrackItemAnimation.Destroy;
begin
  Item.RemoveTransition(Self);
  inherited Destroy;
end;

procedure TdxTileControlHottrackItemAnimation.DoAnimate;
begin
  if ShowFrame then
    Item.ViewInfo.HottrackValue := Position
  else
    Item.ViewInfo.HottrackValue := 255 - Position;
end;

function TdxTileControlHottrackItemAnimation.IsCompatible(AAnimation: TdxAnimationTransition): Boolean;
begin
  Result := (AAnimation.ClassType <> ClassType) or
    ((AAnimation as TdxTileControlHottrackItemAnimation).Item <> Item);
end;

{ TdxTileControlHandScroll }

procedure TdxTileControlHandScroll.DoScrollHorizontally;
var
  dX: Integer;
begin
  dX := PrevMousePos.X - CurMousePos.X;
  dX := dX div GetSlowdownScrollFactor(ViewInfo.LeftScrollPos, ViewInfo.HorzScrollPage, ViewInfo.ContentWidth, dX);
  if dX <> 0 then
    ViewInfo.LeftScrollPos := ViewInfo.LeftScrollPos + dX;
end;

procedure TdxTileControlHandScroll.DoScrollVertically;
var
  dY: Integer;
begin
  dY := PrevMousePos.Y - CurMousePos.Y;
  dY := dY div GetSlowdownScrollFactor(ViewInfo.TopScrollPos, ViewInfo.VertScrollPage, ViewInfo.ContentHeight, dY);
  if dY <> 0 then
    ViewInfo.TopScrollPos := ViewInfo.TopScrollPos + dY;
end;

procedure TdxTileControlHandScroll.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  ALeftPos, ATopPos: Integer;
begin
  Accepted := True;
  ALeftPos := ViewInfo.LeftScrollPos;
  ATopPos := ViewInfo.TopScrollPos;
  DoScrollHorizontally;
  DoScrollVertically;
  if (ALeftPos <> ViewInfo.LeftScrollPos) or (ATopPos <> ViewInfo.TopScrollPos) then
    TileControl.Update;
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxTileControlHandScroll.EndDragAndDrop(Accepted: Boolean);
begin
  FStartAnimationPosX := ViewInfo.LeftScrollPos;
  FFinishAnimationPosX := Max(0, Min(StartAnimationPosX, ViewInfo.ContentWidth - ViewInfo.HorzScrollPage));
  FStartAnimationPosY := ViewInfo.TopScrollPos;
  FFinishAnimationPosY := Max(0, Min(FStartAnimationPosY, ViewInfo.ContentHeight - ViewInfo.VertScrollPage));
  if (StartAnimationPosX <> FinishAnimationPosX) or (StartAnimationPosY <> FinishAnimationPosY) then
    TileControl.Controller.ImmediateAnimation(TdxTileControlRubberAnimation.Create(TileControl, Self));
  inherited EndDragAndDrop(Accepted);
end;

function TdxTileControlHandScroll.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crDefault, crcxHandDrag);
begin
  Result := Cursors[Accepted];
end;

function TdxTileControlHandScroll.GetImmediateStart: Boolean;
begin
  Result := False;
end;

function TdxTileControlHandScroll.GetSlowdownScrollFactor(APos, AScrollPageSize, AContentSize, ANewDelta: Integer): Integer;

  function IsNeedToSlowdown: Boolean;
  begin
    Result := ((APos * ANewDelta) > 0) and ((APos < 0) or (APos > (AContentSize - AScrollPageSize)));
  end;

var
  ARange: Integer;
begin
  Result := 1;
  if IsNeedToSlowdown then
  begin
    ARange := AScrollPageSize div 10;
    if APos < 0 then
      APos := Abs(APos)
    else
      APos := APos - (AContentSize - AScrollPageSize);
    Result := APos div ARange + 1;
  end;
end;

function TdxTileControlHandScroll.GetTileControl: TdxCustomTileControl;
begin
  Result := TdxCustomTileControl(Control);
end;

function TdxTileControlHandScroll.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := TileControl.ViewInfo;
end;

{ TdxTileControlDragDropCustomObject }

constructor TdxTileControlDragDropCustomObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  ViewInfo.DragDropChanges := TdxTileControlDragAndDropChanges.Create(ViewInfo);
  FDragCell := Controller.DragCell as TdxTileControlCustomCellViewInfo;
end;

destructor TdxTileControlDragDropCustomObject.Destroy;
begin
  FreeAndNil(DragImage);
  FreeAndNil(ViewInfo.DragDropChanges);
  inherited Destroy;
end;

procedure TdxTileControlDragDropCustomObject.AfterDragAndDrop(Accepted: Boolean);
begin
  inherited AfterDragAndDrop(Accepted);
  if Accepted and TileControl.AutoSize then
    TileControl.AdjustSize;
end;

procedure TdxTileControlDragDropCustomObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  CreateAutoScrollObjects;
  FIsTouching := IsTouchEvent;
  Controller.PrepareToDragAndDrop;
  if DragCell is TdxTileControlGroupCaptionViewInfo then
    FDragCell := TdxTileControlGroupCaptionViewInfo(DragCell).Group.ViewInfo;
  if IsTouching and (Controller.ItemCheckMode <> tcicmNone) and DragCellIsItem then
  begin
    FState := tcddsPulling;
    InitializeDragImage;
  end
  else
    BeginDragMoving;
end;

procedure TdxTileControlDragDropCustomObject.BeginDragAndDropGroup;
begin
end;

procedure TdxTileControlDragDropCustomObject.BeginDragAndDropItem;
begin
  Controller.CanItemDrawing := False;
end;

procedure TdxTileControlDragDropCustomObject.BeginDragMoving;
begin
  FState := tcddsMoving;
  FPrevAccepted := True;
  ViewInfo.StoreGroupsOrigin;
  InitializeDragImage;
  Controller.UncheckAllItems;
  ShowDragImage(TileControl.ClientToScreen(DragBounds.TopLeft));
  if DragCellIsItem then
    BeginDragAndDropItem
  else
    BeginDragAndDropGroup;
end;

procedure TdxTileControlDragDropCustomObject.CalculateHotSpot;
var
  P: TPoint;
begin
  P := Controller.StartDragPos;
  if DragCell.Visible or TileControl.IsDesigning then
    FHotSpot := Point(P.X - DragCell.Bounds.Left, P.Y - DragCell.Bounds.Top)
  else
    FHotSpot := cxPointInvert(P);
end;

function TdxTileControlDragDropCustomObject.CheckAutoScrolling: Boolean;
var
  R: TRect;
begin
  R := DragBounds;
  Result := ScrollControllers[sLeft].Check(R.TopLeft) or
    ScrollControllers[sLeft].Check(cxRectLeftBottom(R));
  if ScrollControllers[sLeft].Timer.Enabled then
    ScrollControllers[sRight].Stop
  else
    Result := ScrollControllers[sRight].Check(R.BottomRight) or
      ScrollControllers[sRight].Check(cxRectRightTop(R));
  Result := Result or ScrollControllers[sTop].Check(R.TopLeft) or
    ScrollControllers[sTop].Check(cxRectRightTop(R));
  if ScrollControllers[sTop].Timer.Enabled then
    ScrollControllers[sBottom].Stop
  else
    Result := Result or ScrollControllers[sBottom].Check(R.BottomRight) or
      ScrollControllers[sBottom].Check(cxRectLeftBottom(R));
end;

function TdxTileControlDragDropCustomObject.CreateAutoScrollObject(Kind: TScrollBarKind;
  const ARect: TRect; ACode: TScrollCode): TdxTileControlAutoScrollingObject;
begin
  Result := TdxTileControlAutoScrollingObject.Create(Self);
  Result.SetParams(ARect, Kind, ACode, TileControl.GetScrollStep);
end;

procedure TdxTileControlDragDropCustomObject.CreateAutoScrollObjects;
const
  ALineUpScrollCodes: array[Boolean] of TScrollCode = (scLineDown, scLineUp);
  ALineDownScrollCodes: array[Boolean] of TScrollCode = (scLineUp, scLineDown);
var
  R, AScrollRect: TRect;
  AAutoScrollWidth: Integer;
begin
  R := ViewInfo.TilesArea;
  AAutoScrollWidth := TileControl.ScaleFactor.Apply(dxTileControlAutoScrollWidth);
  AScrollRect := cxRectSetWidth(R, AAutoScrollWidth);
  AScrollRect.Left := AScrollRect.Left - Screen.Width;
  ScrollControllers[sLeft] := CreateAutoScrollObject(sbHorizontal, AScrollRect, ALineUpScrollCodes[not UseRightToLeftAlignment]);

  AScrollRect := R;
  AScrollRect.Left := AScrollRect.Right - AAutoScrollWidth;
  AScrollRect := cxRectSetWidth(AScrollRect, Screen.Width);
  ScrollControllers[sRight] := CreateAutoScrollObject(sbHorizontal, AScrollRect, ALineDownScrollCodes[not UseRightToLeftAlignment]);

  AScrollRect := R;
  AScrollRect.Bottom := AScrollRect.Top + AAutoScrollWidth;
  AScrollRect.Top := AScrollRect.Top - Screen.Height;
  ScrollControllers[sTop] := CreateAutoScrollObject(sbVertical, AScrollRect, ALineUpScrollCodes[not UseRightToLeftAlignment]);

  AScrollRect := R;
  AScrollRect.Top := AScrollRect.Bottom - AAutoScrollWidth;
  AScrollRect := cxRectSetHeight(AScrollRect, Screen.Height);
  ScrollControllers[sBottom] := CreateAutoScrollObject(sbVertical, AScrollRect, ALineDownScrollCodes[not UseRightToLeftAlignment]);
end;

procedure TdxTileControlDragDropCustomObject.DestroyAutoScrollObjects;
var
  ASide: TdxTileControlAutoScrollSide;
begin
  for ASide := Low(TdxTileControlAutoScrollSide) to High(TdxTileControlAutoScrollSide) do
    FreeAndNil(ScrollControllers[ASide]);
end;

procedure TdxTileControlDragDropCustomObject.InitializeDragImage;
var
  AWinOrg: TPoint;
begin
  AWinOrg := DragCell.Bounds.TopLeft;
  if DragImage = nil then
  begin
    DragImage := TcxDragImage.Create;
    DragImage.BoundsRect := DragCell.Bounds;
    DragImage.MoveTo(TileControl.ClientToScreen(AWinOrg));
  end;
  DragImage.Canvas.WindowOrg := AWinOrg;
end;

function TdxTileControlDragDropCustomObject.GetController: TdxTileControlController;
begin
  Result := TileControl.Controller;
end;

function TdxTileControlDragDropCustomObject.GetDragBounds: TRect;
begin
  Result.TopLeft := TileControl.ScreenToClient(DragImage.BoundsRect.TopLeft);
  Result := cxRectSetSize(Result, DragImage.Width, DragImage.Height);
end;

function TdxTileControlDragDropCustomObject.GetDragCellAsGroup: TdxTileControlGroup;
begin
  Result := TdxTileControlGroupViewInfo(DragCell).Group;
end;

function TdxTileControlDragDropCustomObject.GetDragCellAsItem: TdxTileControlItem;
begin
  Result := TdxTileControlItemViewInfo(DragCell).Item;
end;

function TdxTileControlDragDropCustomObject.GetDragCellIsItem: Boolean;
begin
  Result := DragCell is TdxTileControlItemViewInfo;
end;

procedure TdxTileControlDragDropCustomObject.DoAnimateContent(const AStart, AFinish: TPoint);
var
  I: Integer;
begin
  if TileControl.IsDesigning or cxPointIsEqual(AStart, AFinish) then Exit;
  for I := 0 to ViewInfo.Cells.Count - 1 do
    ViewInfo.Cells[I].Scroll(AStart.X - AFinish.X, AStart.Y - AFinish.Y);
  TileControl.Controller.ImmediateAnimation(TdxTileControlContentAnimation.Create(
    TileControl, dxTileControlCenterContentAnimationTime, AFinish.X - AStart.X, AFinish.Y - AStart.Y));
end;

procedure TdxTileControlDragDropCustomObject.DoDragEnd;
begin
//
end;

procedure TdxTileControlDragDropCustomObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  if not cxPointIsEqual(P, PrevMousePos) then
  begin
    HideOriginalDragCell;
    if not FHotSpotCalculated then
    begin
      CalculateHotSpot;
      FHotSpotCalculated := True;
    end;

    case State of
      tcddsPulling:
        Pulling(P);
      tcddsMoving:
        DragMoving(P, Accepted);
    end;
    inherited DragAndDrop(P, Accepted);
  end;
end;

procedure TdxTileControlDragDropCustomObject.DragAndDropGroup(var Accepted: Boolean);
begin
end;

procedure TdxTileControlDragDropCustomObject.DragAndDropItem(var Accepted: Boolean);
begin
end;

procedure TdxTileControlDragDropCustomObject.DragMoving(const P: TPoint; var Accepted: Boolean);
begin
  ShowDragImage(TileControl.ClientToScreen(P));
  if DragCellIsItem then
    DragAndDropItem(Accepted)
  else
    DragAndDropGroup(Accepted);
  StopAutoScrollingTimers;
  CheckAutoScrolling;
  FPrevAccepted := Accepted;
end;

procedure TdxTileControlDragDropCustomObject.EndDragAndDrop(Accepted: Boolean);
begin
  DestroyAutoScrollObjects;
  Accepted := Accepted and (FPrevAccepted or (State = tcddsPulling));
  inherited EndDragAndDrop(Accepted);
  if DragCell <> nil then
  begin
    DragImage.Hide;
    case State of
      tcddsMoving:
        EndDragMoving(Accepted);
      tcddsPulling:
        if IsPullDownGesture then
          PullDown(Accepted);
    end;
    TileControl.AddChanges([tccItems]);
  end;
end;

procedure TdxTileControlDragDropCustomObject.EndDragAndDropGroup(Accepted: Boolean);
begin
end;

procedure TdxTileControlDragDropCustomObject.EndDragAndDropItem(Accepted: Boolean; var ANewGroup: TdxTileControlGroup);
begin
end;

procedure TdxTileControlDragDropCustomObject.EndDragMoving(Accepted: Boolean);
var
  ANewGroup: TdxTileControlGroup;
begin
  ANewGroup := nil;
  DragDropChanges.BeforeUpdate;
  try
    DragDropChanges.DragFinished := True;
    if DragCellIsItem then
      EndDragAndDropItem(Accepted, ANewGroup)
    else
      EndDragAndDropGroup(Accepted);
  finally
    DragDropChanges.AfterUpdate;
  end;
  if ANewGroup <> nil then
    ANewGroup.ViewInfo.FNewGroupDrawingBounds := cxNullRect;
  Controller.RestoreItemsBounds;
  if TileControl.IsDesigning then
    SetDesignerModified(TileControl);
  TileControl.ForceUpdate(True);
  ViewInfo.ClearGroupsOrigin;
  DoDragEnd;
  Controller.DragCell := nil;
end;

procedure TdxTileControlDragDropCustomObject.PullDown(Accepted: Boolean);
begin
  //nothing
end;

procedure TdxTileControlDragDropCustomObject.Pulling(const P: TPoint);
var
  ADragImagePos: TPoint;
begin
  ADragImagePos := Controller.StartDragPos;
  ADragImagePos.Y := ADragImagePos.Y + Min(MulDiv(Max(P.Y - ADragImagePos.Y, 0), 2, 5), MaxPullSize);
  PullProgress := MulDiv(100, ADragImagePos.Y - Controller.StartDragPos.Y, MaxPullSize);
  ShowDragImage(TileControl.ClientToScreen(ADragImagePos));
  if not PtInRect(cxRectInflate(DragBounds, MaxPullSize div 2, MaxPullSize div 2), P) then
    BeginDragMoving;
end;

function TdxTileControlDragDropCustomObject.GetActualDragBounds: TRect;
begin
  Result := DragBounds;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, TileControl.ViewInfo.ClientBounds);
end;

function TdxTileControlDragDropCustomObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array [Boolean] of TCursor = (crNoDrop, crArrow);
begin
  if State = tcddsPulling then
    Result := crDefault
  else
    Result := Cursors[Accepted];
end;

function TdxTileControlDragDropCustomObject.GetDragDropChanges: TdxTileControlDragAndDropChanges;
begin
  Result := ViewInfo.DragDropChanges;
end;

function TdxTileControlDragDropCustomObject.GetHitTest: TdxTileControlHitTest;
begin
  Result := TileControl.HitTest;
end;

function TdxTileControlDragDropCustomObject.GetImmediateStart: Boolean;
begin
  Result := False;
end;

function TdxTileControlDragDropCustomObject.GetIsPullDownGesture: Boolean;
begin
  Result := PullProgress >= dxTileControlPullDownThreshold;
end;

function TdxTileControlDragDropCustomObject.GetMaxPullSize: Integer;
begin
  Result := DragImage.Height div 2;
end;

function TdxTileControlDragDropCustomObject.GetTileControl: TdxCustomTileControl;
begin
  Result := TdxCustomTileControl(Control);
end;

function TdxTileControlDragDropCustomObject.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := TileControl.ViewInfo;
end;

procedure TdxTileControlDragDropCustomObject.HideOriginalDragCell;
begin
  if FDragCellIsHided then Exit;
  FDragCellIsHided := True;
  TileControl.ForceUpdate(True);
end;

function TdxTileControlDragDropCustomObject.IsRectInControl(const ARect: TRect): Boolean;
begin
  Result := cxRectIntersect(TileControl.Bounds, ARect);
end;

function TdxTileControlDragDropCustomObject.MouseClientToScreen(const P: TPoint): TPoint;
begin
  Result.X := P.X - FHotSpot.X;
  Result.Y := P.Y - FHotSpot.Y;
end;

procedure TdxTileControlDragDropCustomObject.ShowDragImage(APos: TPoint);
begin
  DragImage.MoveTo(MouseClientToScreen(APos));
  DragImage.Visible := True;
  DragImage.Refresh;
end;

procedure TdxTileControlDragDropCustomObject.SetPullProgress(AValue: Integer);
var
  APrevIsPullDownGesture: Boolean;
begin
  if FPullProgress <> AValue then
  begin
    APrevIsPullDownGesture := IsPullDownGesture;
    FPullProgress := AValue;
    if DragCellIsItem then
    begin
      if IsPullDownGesture <> APrevIsPullDownGesture then
        InitializeDragImage;
      DragCellAsItem.Invalidate;
    end;
  end;
end;

procedure TdxTileControlDragDropCustomObject.StopAutoScrollingTimers;
var
  ASide: TdxTileControlAutoScrollSide;
begin
  for ASide := Low(TdxTileControlAutoScrollSide) to High(TdxTileControlAutoScrollSide) do
    ScrollControllers[ASide].Stop;
end;

function TdxTileControlDragDropCustomObject.UseRightToLeftAlignment: Boolean;
begin
  Result := TileControl.ViewInfo.UseRightToLeftAlignment;
end;

{ TdxTileControlDragDropGroup }

procedure TdxTileControlDragDropGroup.BeginDragAndDropGroup;
begin
  if FDragCell is TdxTileControlGroupViewInfo then
    FDragGroup := TdxTileControlGroupViewInfo(DragCell).Group
  else
    FDragGroup := (DragCell as TdxTileControlGroupCaptionViewInfo).Group;
  FPrevIndex := DragGroupInfo.BeginIndex;
end;

procedure TdxTileControlDragDropGroup.DoDragEnd;
begin
  DragGroup.MakeVisible;
  TileControl.DoGroupDragEnd;
end;

procedure TdxTileControlDragDropGroup.DragAndDropGroup(var Accepted: Boolean);
var
  AIndex: Integer;
begin
  DragDropChanges.BeforeUpdate;
  try
    AIndex := GetPotentialIndexForDraggedGroup;
    Accepted := AIndex >= 0;
    if Accepted then
    begin
      DragGroupInfo.SetCurrentIndex(AIndex);
      TileControl.DoGroupDragOver(Accepted);
    end;
    if Accepted then
    begin
      if DragGroup.Collection = nil then
        DragGroup.AddToCollection(Groups);
      DragGroup.Index := AIndex;
      if AIndex <> FPrevIndex then
        ViewInfo.Calculate;
      if FPrevIndex = -1 then
      begin
        FPrevIndex := DragGroup.Index;
        FPrevAccepted := Accepted;
        DragAndDropGroup(Accepted);
      end;
    end
    else
      if DragGroup.Collection <> nil then
      begin
        DragGroup.RemoveFromCollection(Groups);
        ViewInfo.Calculate;
      end;
  finally
    DragDropChanges.AfterUpdate;
  end;
  FPrevIndex := DragGroup.Index;
end;

procedure TdxTileControlDragDropGroup.EndDragAndDropGroup(Accepted: Boolean);
begin
  if DragGroup.Collection = nil then
    DragGroup.AddToCollection(Groups);
  if not Accepted then
    DragGroupInfo.SetCurrentIndex(DragGroupInfo.BeginIndex);
  DragGroup.Index := DragGroupInfo.CurrentIndex;
end;

function TdxTileControlDragDropGroup.GetDragGroupInfo: TdxTileControlDragGroupInfo;
begin
  Result := TileControl.DragGroupInfo;
end;

function TdxTileControlDragDropGroup.GetPotentialIndexForDraggedGroup: Integer;
begin
  if not IsRectInControl(DragBounds) then
    Result := -1
  else
    if DragGroup.Collection = nil then
      Result := GetPotentialIndexForNewlyInsertedGroup
    else
      Result := GetPotentialIndexForMovedGroup;
end;

function TdxTileControlDragDropGroup.GetPotentialIndexForNewlyInsertedGroup: Integer;
var
  ADragCenter, AGroupCenter: TPoint;
  I: Integer;
  AGroup: TdxTileControlGroup;
  ASuccess: Boolean;
begin
  Result := Groups.Count;
  ADragCenter := cxRectCenter(GetActualDragBounds);
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if not AGroup.Visible then Continue;
    AGroupCenter := cxRectCenter(AGroup.ViewInfo.FExpandedBounds);
    if GroupLayout = glHorizontal then
      ASuccess := ADragCenter.X <= AGroupCenter.X
    else
      ASuccess := ADragCenter.Y <= AGroupCenter.Y;
    if ASuccess then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TdxTileControlDragDropGroup.GetPotentialIndexForMovedGroup: Integer;

  function GetIndex(const ACurrentGroupIndex, ACurrentGroupCenter, ADragLeft, ADragRight: Integer): Integer;
  begin
    Result := -1;
    if ADragRight < ACurrentGroupCenter then
      Result := Max(0, ACurrentGroupIndex - 1 + Ord(ACurrentGroupIndex = FPrevIndex))
    else
      if (ADragLeft < ACurrentGroupCenter) and (ACurrentGroupCenter <= ADragRight) then
        Result := ACurrentGroupIndex;
  end;

var
  AGroupCenter: TPoint;
  AGroup: TdxTileControlGroup;
  I, AIndex: Integer;
  ADragBounds: TRect;
begin
  Result := FPrevIndex;
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if not AGroup.Visible then Continue;
    AGroupCenter := cxRectCenter(AGroup.ViewInfo.FExpandedBounds);
    ADragBounds := GetActualDragBounds;
    if GroupLayout = glHorizontal then
      AIndex := GetIndex(I, AGroupCenter.X, ADragBounds.Left, ADragBounds.Right)
    else
      AIndex := GetIndex(I, AGroupCenter.Y, ADragBounds.Top, ADragBounds.Bottom);
    if AIndex >= 0 then
    begin
      Result := AIndex;
      Break;
    end;
  end;
end;

function TdxTileControlDragDropGroup.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlDragDropGroup.GetGroups: TdxTileControlGroupCollection;
begin
  Result := TileControl.Groups;
end;

procedure TdxTileControlDragDropGroup.InitializeDragImage;
begin
  inherited InitializeDragImage;
  ViewInfo.Painter.DrawBackground(DragImage.Canvas, DragImage.BoundsRect);
  ViewInfo.Cells.DrawWithoutClipping(DragImage.Canvas);
end;


{ TdxTileControlDragGroupInfo }

function TdxTileControlDragGroupInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := FGroup.TileControl;
end;

procedure TdxTileControlDragGroupInfo.Initialize(AGroup: TdxTileControlGroup);
begin
  FGroup := AGroup;
  FBeginIndex := FGroup.Index;
end;

procedure TdxTileControlDragGroupInfo.SetCurrentIndex(AIndex: Integer);
begin
  FCurrentIndex := AIndex;
end;

{ TdxTileControlDragItemInfo }

destructor TdxTileControlDragItemInfo.Destroy;
begin
  DestroyCheckedItems;
  inherited Destroy;
end;

procedure TdxTileControlDragItemInfo.CreateCheckedItems;
var
  I, J: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  FCheckedItems := TdxTileControlCheckedItems.Create;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    if AGroup.Visible then
      for J := 0 to AGroup.ItemCount - 1 do
      begin
        AItem := AGroup.Items[J];
        if AItem.Checked and (AItem <> Item) then
          FCheckedItems.Add(AItem);
      end;
  end;
end;

procedure TdxTileControlDragItemInfo.DestroyCheckedItems;
begin
  FreeAndNil(FCheckedItems);
end;

function TdxTileControlDragItemInfo.GetSourceGroup: TdxTileControlGroup;
begin
  Result := Item.GroupBeforeDrag;
end;

function TdxTileControlDragItemInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Item.TileControl;
end;

procedure TdxTileControlDragItemInfo.Initialize(ADragItem: TdxTileControlItem);
begin
  FItem := ADragItem;
  Item.StoreLayoutBeforeDrag;
  SetGroup(SourceGroup);
  if Item.Checked and (TileControl.CheckedItemCount > 1) then
    CreateCheckedItems;
end;

procedure TdxTileControlDragItemInfo.SetGroup(AGroup: TdxTileControlGroup);
begin
  FGroup := AGroup;
end;

{ TdxTileControlDragItemPlace }

constructor TdxTileControlDragItemPlace.Create;
begin
  FPosition := TdxLayoutItemPosition.Create;
end;

destructor TdxTileControlDragItemPlace.Destroy;
begin
  FreeAndNil(FPosition);
  inherited Destroy;
end;

procedure TdxTileControlDragItemPlace.Assign(ASource: TdxTileControlDragItemPlace);
begin
  FGroup := ASource.Group;
  FVirtualGroup := ASource.VirtualGroup;
  FPosition.Assign(ASource.Position);
end;

function TdxTileControlDragItemPlace.GetGroup: TdxTileControlGroup;
begin
  if FGroup = nil then
    Result := nil
  else
    Result := FGroup.Group;
end;

function TdxTileControlDragItemPlace.IsEqual(APlace: TdxTileControlDragItemPlace): Boolean;
begin
  Result := (FGroup = APlace.Group) and (FVirtualGroup = APlace.FVirtualGroup) and
    FPosition.IsEqual(APlace.Position);
end;

{ TdxTileControlGroup }

constructor TdxTileControlGroup.Create(AOwner: TComponent);
begin
  if AOwner is TdxCustomTileControl then
  begin
    inherited Create(nil);
    FTileControl := AOwner as TdxCustomTileControl;
  end
  else
    inherited Create(AOwner);
  FViewInfo := TdxTileControlGroupViewInfo.Create(Self);
  FVirtualGroupBefore := TdxTileControlVirtualGroupViewInfo.Create(Self);
  FVirtualGroupAfter := TdxTileControlVirtualGroupViewInfo.Create(Self);
  FItems := TcxObjectList.Create(False);
  FIndent := 0;
  FCaption := TdxTileControlGroupCaption.Create(Self);
  FEnabled := True;
  FVisible := True;
end;

destructor TdxTileControlGroup.Destroy;
begin
  RemoveItems;
  FreeAndNil(FCaption);
  FreeAndNil(FItems);
  FreeAndNil(FVirtualGroupAfter);
  FreeAndNil(FVirtualGroupBefore);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxTileControlGroup.Add(AItem: TdxTileControlItem);
begin
  if AItem.Group = Self then Exit;
  FItems.Add(AItem);
  AItem.FGroup := Self;
  GroupChanged;
end;

procedure TdxTileControlGroup.AssignFrom(AGroup: TdxTileControlGroup; AExcludeItem: TdxTileControlItem = nil);
var
  I: Integer;
begin
  ViewInfo.FBounds := AGroup.ViewInfo.FBounds;
  ViewInfo.FMaximizedAreaBounds := AGroup.ViewInfo.FMaximizedAreaBounds;
  ViewInfo.FExpandedBounds := AGroup.ViewInfo.FExpandedBounds;
  ViewInfo.FColumnCount := AGroup.ViewInfo.FColumnCount;
  ViewInfo.FRowCount := AGroup.ViewInfo.FRowCount;
  FItems.Clear;
  for I := 0 to AGroup.ItemCount - 1 do
    if AGroup.Items[I] <> AExcludeItem then
      FItems.Add(AGroup.Items[I]);
end;

procedure TdxTileControlGroup.ChangeScale(M, D: Integer);
begin
  Caption.ChangeScale(M, D);
  FIndent := MulDiv(FIndent, M, D);
end;

procedure TdxTileControlGroup.CreateCaptionEditor;
begin
  FCaptionEditor := TdxTileControlGroupCaptionInplaceEdit.Create(Self);
end;

procedure TdxTileControlGroup.DeleteItem(AItem: TdxTileControlItem);
var
  AIndex: Integer;
begin
  AIndex := IndexOfItem(AItem);
  if AIndex >= 0 then
  begin
    FItems.Items[AIndex].Free;
    GroupChanged;
  end;
end;

procedure TdxTileControlGroup.DeleteItems;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FItems.Items[I].Free;
end;

procedure TdxTileControlGroup.GroupChanged;
begin
  FViewInfo.Calculated := False;
  Changed(True);
end;

function TdxTileControlGroup.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := TdxCustomTileControl(AParent).Groups;
end;

procedure TdxTileControlGroup.Invalidate;
begin
  if FViewInfo.Visible then
    TileControl.InvalidateRect(FViewInfo.Bounds, False);
end;

function TdxTileControlGroup.GetCustomIndent(AVirtualGroup: TdxTileControlVirtualGroupViewInfo): Integer;
begin
  if (AVirtualGroup = VirtualGroupAfter) and not IsMostRight then
    Result := Indent
  else
    Result := 0;
end;

function TdxTileControlGroup.GetBounds: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TdxTileControlGroup.GetExpandedBounds: TRect;
begin
  Result := ViewInfo.ExpandedBounds;
end;

function TdxTileControlGroup.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlGroup.GetGroups: TdxTileControlGroupCollection;
begin
  Result := TdxTileControlGroupCollection(Collection);
end;

function TdxTileControlGroup.GetItem(AIndex: Integer): TdxTileControlItem;
begin
  Result := TdxTileControlItem(FItems[AIndex]);
end;

function TdxTileControlGroup.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxTileControlGroup.GetIsMostLeft: Boolean;
begin
  Result := Groups.GetPrevVisibleGroup(Self) = nil;
end;

function TdxTileControlGroup.GetIsMostRight: Boolean;
begin
  Result := Groups.GetNextVisibleGroup(Self) = nil;
end;

function TdxTileControlGroup.GetMaximizedAreaBounds: TRect;
begin
  Result := ViewInfo.MaximizedAreaBounds;
end;

function TdxTileControlGroup.GetRealColumnCount: Integer;
begin
  Result := ViewInfo.ColumnCount;
end;

function TdxTileControlGroup.GetRealRowCount: Integer;
begin
  Result := ViewInfo.RowCount;
end;

procedure TdxTileControlGroup.HideCaptionEditor;
begin
  if CaptionEditor.EditModified then
    Caption.Text := CaptionEditor.Text;
  CaptionEditor.Hide;
  FreeAndNil(FCaptionEditor);
end;

function TdxTileControlGroup.IsCaptionStored: Boolean;
begin
  Result := FCaption.IsChanged;
end;

function TdxTileControlGroup.IsAvailableForNavigation: Boolean;
begin
  Result := Visible and Enabled;
end;

function TdxTileControlGroup.IsCaptionEditorPresent: Boolean;
begin
  Result := (CaptionEditor <> nil) and CaptionEditor.Visible;
end;

function TdxTileControlGroup.IsDragged: Boolean;
begin
  if TileControl.IsGroupDragged then
    Result := ViewInfo = (TileControl.DragAndDropObject as TdxTileControlDragDropCustomObject).DragCell
  else
    Result := False;
end;

function TdxTileControlGroup.IndexOfItem(AItem: TdxTileControlItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

procedure TdxTileControlGroup.MakeVisible;
begin
  if not Visible or TileControl.IsLocked then Exit;
  TileControl.ViewInfo.MakeVisible(ViewInfo.Bounds, IsMostLeft, IsMostRight);
  Invalidate;
end;

procedure TdxTileControlGroup.MoveItem(AItem: TdxTileControlItem; AIndexDest: Integer);
var
  ANowIndex: Integer;
begin
  ANowIndex := IndexOfItem(AItem);
  if AIndexDest > ItemCount - 1 then
    AIndexDest := ItemCount - 1;
  if ANowIndex <> AIndexDest then
    FItems.Move(ANowIndex, AIndexDest);
end;

procedure TdxTileControlGroup.RemoveItem(AItem: TdxTileControlItem);
begin
  if Self = nil then Exit;
  TileControl.BeginUpdate;
  try
    if FItems.Remove(AItem) >= 0 then
    begin
      AItem.FGroup := nil;
        GroupChanged;
    end;
  finally
    TileControl.EndUpdate;
  end;
end;

procedure TdxTileControlGroup.RemoveItems;
var
  I: Integer;
begin
  if Owner = nil then
    for I := FItems.Count - 1 downto 0 do
      FItems.Remove(FItems[I])
  else
  begin
    TileControl.BeginUpdate;
    try
      for I := FItems.Count - 1 downto 0 do
        RemoveItem(FItems.Items[I] as TdxTileControlItem);
      GroupChanged;
    finally
      TileControl.EndUpdate;
    end;
  end;
end;

procedure TdxTileControlGroup.SetCollection(AValue: TcxComponentCollection);
begin
  if AValue <> nil then
    FTileControl := (AValue as TdxTileControlGroupCollection).TileControl;
  inherited SetCollection(AValue);
  UpdateTileControlLink;
end;

procedure TdxTileControlGroup.SetIndex(AValue: Integer);
begin
  AValue := Min(Max(AValue, 0), Collection.Count - 1);
  if Index <> AValue then
    if GroupLayout = glHorizontal then
      SetIndexForHorzLayout(AValue)
    else
      SetIndexForVertLayout(AValue);
end;

procedure TdxTileControlGroup.SetIndexForHorzLayout(AValue: Integer);
var
  ALeftOfFirstGroup: Integer;
  ANewFirstGroup: TdxTileControlGroupViewInfo;
begin
  ALeftOfFirstGroup := 0;
  if ((Index = 0) or (AValue = 0)) and (Collection.Items[0] <> nil) then
    ALeftOfFirstGroup := TdxTileControlGroup(Collection.Items[0]).ViewInfo.Bounds.Left;
  TileControl.BeginUpdate;
  try
    inherited SetIndex(AValue);
    if ALeftOfFirstGroup <> 0 then
    begin
      ANewFirstGroup := TdxTileControlGroup(Collection.Items[0]).ViewInfo;
      ANewFirstGroup.Scroll(ALeftOfFirstGroup - ANewFirstGroup.Bounds.Left, 0);
    end;
  finally
    TileControl.EndUpdate;
  end;
end;

procedure TdxTileControlGroup.SetIndexForVertLayout(AValue: Integer);
var
  ATopOfFirstGroup: Integer;
  ANewFirstGroup: TdxTileControlGroupViewInfo;
begin
  ATopOfFirstGroup := 0;
  if ((Index = 0) or (AValue = 0)) and (Collection.Items[0] <> nil) then
    ATopOfFirstGroup := TdxTileControlGroup(Collection.Items[0]).ViewInfo.Bounds.Top;
  TileControl.BeginUpdate;
  try
    inherited SetIndex(AValue);
    if ATopOfFirstGroup <> 0 then
    begin
      ANewFirstGroup := TdxTileControlGroup(Collection.Items[0]).ViewInfo;
      ANewFirstGroup.Scroll(0, ATopOfFirstGroup - ANewFirstGroup.Bounds.Top);
    end;
  finally
    TileControl.EndUpdate;
  end;
end;

procedure TdxTileControlGroup.ShowCaptionEditor;
begin
  if CaptionEditor = nil then
    CreateCaptionEditor;
  CaptionEditor.Show;
end;

procedure TdxTileControlGroup.SynchronizeEditorPos;
begin
  if IsCaptionEditorPresent then
    CaptionEditor.SynchronizePos;
end;

procedure TdxTileControlGroup.UpdateTileControlLink;
begin
  if Groups <> nil then
    FTileControl := Groups.TileControl;
end;

function TdxTileControlGroup.UseRightToLeftAlignment: Boolean;
begin
  Result := TileControl.ViewInfo.UseRightToLeftAlignment;
end;

procedure TdxTileControlGroup.SetIndent(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FIndent <> AValue then
  begin
    FIndent := AValue;
    GroupChanged;
  end;
end;

procedure TdxTileControlGroup.SetCaption(AValue: TdxTileControlGroupCaption);
begin
  FCaption.Assign(AValue);
end;

procedure TdxTileControlGroup.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Invalidate;
  end;
end;

procedure TdxTileControlGroup.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    GroupChanged;
  end;
end;

// IcxStoredObject

function TdxTileControlGroup.GetObjectName: string;
begin
  Result := Name;
end;

function TdxTileControlGroup.GetProperties(AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  for I := Low(dxTileControlStoredGroupPropertiesNames) to High(dxTileControlStoredGroupPropertiesNames) do
    AProperties.Add(dxTileControlStoredGroupPropertiesNames[I]);
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TdxTileControlGroup.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = dxTileControlStoredGroupPropertiesNames[3] then
    AValue := Caption.Text
  else
    if Assigned(OnGetStoredPropertyValue) then
      OnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TdxTileControlGroup.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = dxTileControlStoredGroupPropertiesNames[3] then
    Caption.Text := AValue
  else
    if Assigned(OnSetStoredPropertyValue) then
      OnSetStoredPropertyValue(Self, AName, AValue);
end;

// IcxStoredParent
function TdxTileControlGroup.StoredCreateChild(const AObjectName, AClassName: string): TObject;
var
  AItem: TdxTileControlItem;
begin
  if AClassName = 'TdxTileControlItem' then
  begin
    AItem := TileControl.Items.Add;
    FItems.Add(AItem);
    AItem.FGroup := Self;
    Result := AItem;
    TileControl.DoInitStoredObject(Result);
  end
  else
    Result := nil;
end;

procedure TdxTileControlGroup.StoredDeleteChild(const AObjectName: string; AObject: TObject);
begin
  DeleteItem(AObject as TdxTileControlItem);
end;

procedure TdxTileControlGroup.StoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  if TileControl.StoredVersion = 0 then
    for I := 0 to TileControl.Items.Count - 1 do
      if TileControl.IsRestoring or (TileControl.Items[I].Group = Self) then
        AChildren.AddObject('', TileControl.Items[I])
end;

{ TdxTileControlDragAndDropChanges }

function dxCompareAnimationInfoCells(AInfo1, AInfo2: TdxTileControlDragAndDropChangesCellInfo): Integer;
begin
  Result := AInfo1.Compare(AInfo2);
end;

constructor TdxTileControlDragAndDropChanges.Create(AViewInfo: TdxTileControlViewInfo);
begin
  FViewInfo := AViewInfo;
  FItemsInfo := TcxObjectList.Create;
end;

destructor TdxTileControlDragAndDropChanges.Destroy;
begin
  FreeAndNil(FItemsInfo);
  inherited Destroy;
end;


procedure TdxTileControlDragAndDropChanges.AfterUpdate;
var
  I: Integer;
  AInfo: TdxTileControlDragAndDropChangesCellInfo;
begin
  Dec(FLock);
  if FLock > 0 then Exit;
  TileControl.CancelUpdate;
  PopulateItemsInfo(False);
  ItemsInfo.Sort(@dxCompareAnimationInfoCells);
  I := 0;
  while I < Count do
  begin
    if I + 1 < ItemsInfo.Count then
      AInfo := Items[I + 1]
    else
      AInfo := nil;
    if Items[I].IsEqual(AInfo) then
    begin
      ItemsInfo.FreeAndDelete(I);
      ItemsInfo.FreeAndDelete(I);
    end
    else
      if (AInfo = nil) or (Items[I].Item <> AInfo.Item) or
        (Items[I].GroupCaption <> AInfo.GroupCaption) then
        ItemsInfo.FreeAndDelete(I)
      else
      begin
        if not Items[I].Merge(AInfo) then
          ItemsInfo.FreeAndDelete(I)
        else
          Inc(I);
        ItemsInfo.FreeAndDelete(I);
      end;
  end;
  if not TileControl.IsDestroying then
  begin
    if DragObjectFinished then
      FDragImageBounds := dxMapWindowRect(DragAndDropObject.DragImage.Handle, TileControl.Handle, DragAndDropObject.DragImage.ClientRect);
    if not TileControl.IsDesigning then
    begin
      if (ItemsInfo.Count > 0) or DragObjectFinished or DragFinished then
        TileControl.Controller.ImmediateAnimation(TdxTileControlDragAndDropAnimation.Create(Self));
      dxMessagesController.KillMessages(TileControl.Handle, WM_MOUSEMOVE);
    end
    else
      TileControl.Invalidate;
  end;
  ItemsInfo.Clear;
end;

procedure TdxTileControlDragAndDropChanges.BeforeUpdate;
begin
  Inc(FLock);
  if FLock = 1 then
  begin
    FDragImagePosition := cxInvisiblePoint;
    TileControl.BeginUpdate;
    FDragObject := nil;
    ItemsInfo.Clear;
    PopulateItemsInfo(True);
  end;
end;

function TdxTileControlDragAndDropChanges.IndexOf(ACell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Cell = ACell then Exit;
  Result := -1;
end;

procedure TdxTileControlDragAndDropChanges.AnimateDragFinished(const AProgress: Integer);
var
  APos: TPoint;
  R: TRect;
begin
  R := DragAndDropObject.DragCell.Bounds;
  APos := DragAndDropObject.DragCell.Bounds.TopLeft;
  APos.X := FDragImageBounds.Left + MulDiv(APos.X - FDragImageBounds.Left,  AProgress, dxTileDropAnimationStepCount);
  APos.Y := FDragImageBounds.Top + MulDiv(APos.Y - FDragImageBounds.Top, AProgress, dxTileDropAnimationStepCount);
  if not cxPointIsEqual(APos, DragAndDropObject.DragImage.BoundsRect.TopLeft) then
  begin
    if not cxPointIsEqual(FDragImagePosition, cxInvisiblePoint) then
      TileControl.InvalidateRect(cxRectSetOrigin(R, FDragImagePosition), False);
    FDragImagePosition := APos;
    TileControl.InvalidateRect(cxRectSetOrigin(R, FDragImagePosition), False);
    TileControl.ForceUpdate();
  end;
  if AProgress = dxTileDropAnimationStepCount then
    FDragObject := nil;
end;

procedure TdxTileControlDragAndDropChanges.DrawItems(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas, FProgress);
  if FDragFinished and (DragAndDropObject is TdxTileControlDragDropItem) then
    DragAndDropObject.DragCellAsItem.ViewInfo.DrawContent(ACanvas, cxRectSetOrigin(FDragImageBounds, FDragImagePosition));
end;

procedure TdxTileControlDragAndDropChanges.PopulateItemsInfo(
  ABeforeUpdate: Boolean);
var
  I: Integer;
  ACell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo;
  AItem: TdxTileControlItem;
  AGroupCaption: TdxTileControlGroupCaption;
  ACellIsGroupCaption: Boolean;
begin
  if TileControl.Changes * [tccItems, tccLayout] <> [] then
    ViewInfo.Calculate;
  if ABeforeUpdate then
    ItemsInfo.Capacity := ViewInfo.Cells.Count * 2;
  for I := 0 to ViewInfo.Cells.Count - 1 do
  begin
    if (ViewInfo.Cells[I] is TdxTileControlGroupViewInfo) and
      TdxTileControlGroupViewInfo(ViewInfo.Cells[I]).Group.IsDragged then
        FDragObject := TdxTileControlGroupViewInfo(ViewInfo.Cells[I]);

    ACellIsGroupCaption := ViewInfo.Cells[I] is TdxTileControlGroupCaptionViewInfo;
    if (ViewInfo.Cells[I] is TdxTileControlItemViewInfo) or ACellIsGroupCaption then
    begin
      ACell := TdxTileControlAnimatedDragAndDropCustomCellViewInfo(ViewInfo.Cells[I]);
      if ACellIsGroupCaption then
      begin
        AGroupCaption := TdxTileControlGroupCaptionViewInfo(ACell).Caption;
        if not ((AGroupCaption.Group = nil) or AGroupCaption.Group.IsDragged or (Trim(AGroupCaption.Text) = '')) then
          ItemsInfo.Add(TdxTileControlDragAndDropChangesCellInfo.Create(ACell, ABeforeUpdate));
      end
      else
      begin
        AItem := TdxTileControlItemViewInfo(ACell).Item;
        if AItem.IsDragged then
          FDragObject := AItem.ViewInfo;
        if not (AItem.IsDragged or (AItem.Group = nil) or AItem.Group.IsDragged) then
          ItemsInfo.Add(TdxTileControlDragAndDropChangesCellInfo.Create(ACell, ABeforeUpdate));
      end;
    end;
  end;
end;

procedure TdxTileControlDragAndDropChanges.SetAnimationProgress(const AProgress: Integer);
var
  I: Integer;
begin
  FProgress := AProgress;
  for I := 0 to Count - 1 do
    Items[I].InvalidateAnimatedArea;
  if DragObjectFinished then
  begin
    if DragObject <> nil then
      DragObject.Invalidate;
    AnimateDragFinished(Min(AProgress * 100, dxTileDropAnimationStepCount));
  end;
end;

function TdxTileControlDragAndDropChanges.GetCount: Integer;
begin
  Result := ItemsInfo.Count;
end;

function TdxTileControlDragAndDropChanges.GetDragAndDropObject: TdxTileControlDragDropCustomObject;
begin
  Result := TileControl.DragAndDropObject as TdxTileControlDragDropCustomObject;
end;

function TdxTileControlDragAndDropChanges.GetDragObjectFinished: Boolean;
begin
  Result := ((TileControl.DragAndDropState = ddsNone) and
    (DragObject <> nil)) or DragFinished;
end;

function TdxTileControlDragAndDropChanges.GetItem(AIndex: Integer): TdxTileControlDragAndDropChangesCellInfo;
begin
  Result := TdxTileControlDragAndDropChangesCellInfo(ItemsInfo.List[AIndex]);
end;

function TdxTileControlDragAndDropChanges.GetTileControl: TdxCustomTileControl;
begin
  Result := ViewInfo.TileControl;
end;

{ TdxTileControlDragAndDropChangesCellInfo }

constructor TdxTileControlDragAndDropChangesCellInfo.Create(ACell: TdxTileControlAnimatedDragAndDropCustomCellViewInfo;
  ABeforeUpdate: Boolean);
begin
  GroupCaption := nil;
  Item := nil;
  Cell := ACell;
  BeforeUpdate := ABeforeUpdate;
  if Cell is TdxTileControlGroupCaptionViewInfo then
  begin
    GroupCaption := TdxTileControlGroupCaptionViewInfo(ACell).Caption;
    if BeforeUpdate then
      GroupOriginBefore := GroupCaption.Group.Bounds.TopLeft
    else
      GroupOriginAfter := GroupCaption.Group.Bounds.TopLeft;
  end
  else
  begin
    Item := TdxTileControlItemViewInfo(ACell).Item;
    if BeforeUpdate then
    begin
      GroupOriginBefore := Item.Group.Bounds.TopLeft;
      ColumnBefore := Item.ViewInfo.Position.Column;
      RowBefore := Item.ViewInfo.Position.Row;
    end
    else
    begin
      GroupOriginAfter := Item.Group.Bounds.TopLeft;
      ColumnAfter := Item.ViewInfo.Position.Column;
      RowAfter := Item.ViewInfo.Position.Row;
    end;
  end;
  if BeforeUpdate then
    BoundsBefore := Cell.GetDrawBounds(True)
  else
    BoundsAfter := Cell.GetDrawBounds;
end;

destructor TdxTileControlDragAndDropChangesCellInfo.Destroy;
begin
  FreeAndNil(ClipRgn);
  inherited Destroy;
end;

procedure TdxTileControlDragAndDropChangesCellInfo.CalculateComplexDrawingAreaBounds(var ADest, ASource: TRect;
  const AOffsetX, AOffsetY: Integer);

  function IsMovingToForward: Boolean;
  begin
    Result :=
      Group.ViewInfo.GetZIndex(ColumnBefore, RowBefore, RowBefore) <
      Group.ViewInfo.GetZIndex(ColumnAfter, RowAfter, RowAfter);
  end;

  procedure CalculateForVerticalMoving;
  begin
    if not IsMovingToForward then
    begin
      ADest := cxRectSetTop(BoundsAfter, BoundsAfter.Bottom - AOffsetY);
      ASource := cxRectSetTop(BoundsBefore, BoundsBefore.Top - AOffsetY);
    end
    else
    begin
      ADest := cxRectSetBottom(BoundsAfter, BoundsAfter.Top + AOffsetY);
      ASource := cxRectSetTop(BoundsBefore, BoundsBefore.Top + AOffsetY);
    end;
  end;

  procedure CalculateForHorizontalMoving;
  begin
    if IsMovingToForward then
    begin
      ADest := cxRectSetRight(BoundsAfter, BoundsAfter.Left + AOffsetX);
      ASource := cxRectSetLeft(BoundsBefore, BoundsBefore.Left + AOffsetX);
    end
    else
    begin
      ADest := cxRectSetLeft(BoundsAfter, BoundsAfter.Right - AOffsetX);
      ASource := cxRectSetLeft(BoundsBefore, BoundsBefore.Left - AOffsetX);
    end;
  end;

  function IsMovingFromLastRowInFirstOrReverse: Boolean;
  begin
    Result := (((RowBefore div 2 = TileControl.ViewInfo.RowCount - 1) and (RowAfter div 2 = 0)) or
               ((RowBefore div 2 = 0) and (RowAfter div 2 = TileControl.ViewInfo.RowCount - 1)));
  end;

  function IsDraggedItemNotPlaceSameBeforePosition(ADraggedItem: TdxTileControlItem): Boolean;
  begin
    Result := (ADraggedItem <> nil) and
      ((ADraggedItem.Group = nil) or
       ((ADraggedItem.Group <> nil) and (ADraggedItem.ViewInfo.Position.Row div 2 <> RowBefore div 2) and
        (ADraggedItem.ViewInfo.Position.Column div 2 <> RowBefore div 2)));
  end;

  function IsLargeSizeCategory(AItem: TdxTileControlItem): Boolean;
  begin
    Result := (AItem <> nil) and (AItem.Size in [tcisLarge, tcisExtraLarge]);
  end;

var
  ADraggedItem: TdxTileControlItem;
begin
  ADraggedItem := TileControl.DraggedItem;
  if IsMovingFromLastRowInFirstOrReverse and
    (IsLargeSizeCategory(Item) or IsLargeSizeCategory(ADraggedItem) or
     IsDraggedItemNotPlaceSameBeforePosition(ADraggedItem)) then
    CalculateForVerticalMoving
  else
    CalculateForHorizontalMoving;
  if not cxPointIsEqual(GroupOriginBefore, GroupOriginAfter) then
    ASource := cxNullRect;
end;

procedure TdxTileControlDragAndDropChangesCellInfo.CalculateSimpleDrawingAreaBounds(var ADest: TRect;
  const AOffsetX, AOffsetY: Integer);
begin
  ADest := cxRectOffset(ADest, AOffsetX, AOffsetY);
end;

function TdxTileControlDragAndDropChangesCellInfo.Compare(AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
begin
  if (Item <> nil) and (AInfo.Item <> nil) then
    Result := CompareTileControlItems(AInfo)
  else
    if (GroupCaption <> nil) and (AInfo.GroupCaption <> nil) then
      Result := CompareTileControlGroupCaptions(AInfo)
    else
      if Item <> nil then
        Result := 1
      else
        Result := -1;
end;

function TdxTileControlDragAndDropChangesCellInfo.CompareTileControlGroupCaptions(
  AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
begin
  if GroupCaption = AInfo.GroupCaption then
    Result := Ord(AInfo.BeforeUpdate) - Ord(BeforeUpdate)
  else
    Result := TdxNativeInt(GroupCaption) - TdxNativeInt(AInfo.GroupCaption);
end;

function TdxTileControlDragAndDropChangesCellInfo.CompareTileControlItems(
  AInfo: TdxTileControlDragAndDropChangesCellInfo): Integer;
begin
  if Item = AInfo.Item then
    Result := Ord(AInfo.BeforeUpdate) - Ord(BeforeUpdate)
  else
    Result := TdxNativeInt(Item) - TdxNativeInt(AInfo.Item);
end;

procedure TdxTileControlDragAndDropChangesCellInfo.Draw(ACanvas: TcxCanvas; const AProgress: Integer);
var
  ASource, ADest: TRect;
  AOffsetX, AOffsetY: Integer;
begin
  ASource := cxNullRect;
  ADest := BoundsBefore;

  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(ClipRgn, roSet, False);

    AOffsetX := MulDiv(AreaSizeX, AProgress, 100);
    AOffsetY := MulDiv(AreaSizeY, AProgress, 100);
    if IsSimpleMoving then
      CalculateSimpleDrawingAreaBounds(ADest, AOffsetX, AOffsetY)
    else
      CalculateComplexDrawingAreaBounds(ADest, ASource, AOffsetX, AOffsetY);

    if not cxRectIsNull(ASource) then
      Cell.DrawContent(ACanvas, ASource);
    Cell.DrawContent(ACanvas, ADest);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxTileControlDragAndDropChangesCellInfo.GetGroup: TdxTileControlGroup;
begin
  if Item <> nil then
    Result := Item.Group
  else
    Result := GroupCaption.Group;
end;

function TdxTileControlDragAndDropChangesCellInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Cell.TileControl;
end;

function TdxTileControlDragAndDropChangesCellInfo.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := TileControl.ViewInfo;
end;

procedure TdxTileControlDragAndDropChangesCellInfo.InvalidateAnimatedArea;
begin
  TileControl.InvalidateRgn(ClipRgn, False);
end;

function TdxTileControlDragAndDropChangesCellInfo.IsEqual(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
begin
  Result := (AInfo <> nil) and cxRectIsEqual(BoundsBefore, AInfo.BoundsAfter);
  if Result then
    if Item <> nil then
      Result := IsEqualItems(AInfo)
    else
      Result := IsEqualGroupCaptions(AInfo);
end;

function TdxTileControlDragAndDropChangesCellInfo.IsEqualGroupCaptions(
  AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
begin
  Result := GroupCaption = AInfo.GroupCaption;
end;

function TdxTileControlDragAndDropChangesCellInfo.IsEqualItems(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
begin
  Result := Item = AInfo.Item;
end;

function TdxTileControlDragAndDropChangesCellInfo.Merge(AInfo: TdxTileControlDragAndDropChangesCellInfo): Boolean;
begin
  BoundsBefore := cxRectOffset(BoundsBefore, ViewInfo.LeftScrollPos, ViewInfo.TopScrollPos);
  Result := cxRectIntersect(BoundsBefore, ViewInfo.TilesArea) or Cell.Visible;
  BoundsAfter := AInfo.BoundsAfter;
  GroupOriginAfter := AInfo.GroupOriginAfter;
  ColumnAfter := AInfo.ColumnAfter;
  RowAfter := AInfo.RowAfter;

  IsSimpleMoving := (GroupCaption <> nil) or
    ((Item <> nil) and (RowBefore = AInfo.RowAfter) or (ColumnBefore = AInfo.ColumnAfter));
  if IsSimpleMoving then
    ClipRgn := TcxRegion.Create(cxRectUnion(BoundsBefore, BoundsAfter))
  else
  begin
    ClipRgn := TcxRegion.Create(BoundsBefore);
    ClipRgn.Combine(BoundsAfter, roAdd);
  end;
  ClipRgn.Combine(ViewInfo.TilesArea, roIntersect);
  AreaSizeX := BoundsAfter.Left - BoundsBefore.Left;
  AreaSizeY := BoundsAfter.Top - BoundsBefore.Top;
  if (Item <> nil) and not IsSimpleMoving then
  begin
    AreaSizeX := cxRectWidth(BoundsAfter);
    AreaSizeY := cxRectHeight(BoundsAfter);
  end;
end;

{ TdxTileControlDragDropItem }

constructor TdxTileControlDragDropItem.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FCountInfoFont := TFont.Create;
  FCountInfoFont.Style := [fsBold];
  FCountInfoFont.Color := clWhite;
  FCountInfoFont.Name := DefaultFontName;
  FCountInfoFont.Height := dxGetFontHeightForDefaultDPI(dxTileControlDefaultGroupCaptionFontSize);
  FGroupItemsPositions := TcxObjectList.Create(True);
  FPrevPlace := TdxTileControlDragItemPlace.Create;
  FPlace := TdxTileControlDragItemPlace.Create;
  FTemporaryGroup := TdxTileControlGroup.Create(TileControl);
  FNextPosition := TdxLayoutItemPosition.Create;
end;

destructor TdxTileControlDragDropItem.Destroy;
begin
  FreeAndNil(FPlace);
  FreeAndNil(FPrevPlace);
  FGroupItemsPositions.Clear;
  FreeAndNil(FGroupItemsPositions);
  FreeAndNil(FTemporaryGroup);
  FreeAndNil(FNextPosition);
  FreeAndNil(FCountInfoFont);
  DragItemInfo.DestroyCheckedItems;
  inherited Destroy;
end;

procedure TdxTileControlDragDropItem.BeginDragAndDropItem;
begin
  inherited BeginDragAndDropItem;
  if not TileControl.IsDesigning then
    Controller.ImmediateAnimation(TdxTileControlInflateItemAnimation.Create(TileControl, nil, -1,
      dxTileControlInflateAnimationStepsCount));
  DragItem.StoreLayoutBeforeDrag;
  FWasTryDecreaseBeginGroup := False;
  InitializePrevPlace;
end;

procedure TdxTileControlDragDropItem.BeginDragMoving;
begin
  FDragItem := DragCellAsItem;
  inherited BeginDragMoving;
end;

procedure TdxTileControlDragDropItem.DoCollapseBeginGroupWhenItemIsLast;
begin
  BeginGroup.Visible := False;
  ViewInfo.Calculate;
end;

procedure TdxTileControlDragDropItem.DoDragEnd;
var
  AStart, AFinish: TPoint;
begin
  Controller.CanItemDrawing := True;
  DragItem.MakeVisible;
  DragImage.Hide;
  TileControl.Refresh;
  AStart := DragCellAsItem.ViewInfo.Bounds.TopLeft;
  ViewInfo.Calculate;
  AFinish := DragCellAsItem.ViewInfo.Bounds.TopLeft;
  DoAnimateContent(AStart, AFinish);
  TileControl.DoItemDragEnd;
end;

procedure TdxTileControlDragDropItem.DoItemDragOver(var AAccepted: Boolean);
begin
  if AAccepted then
  begin
    if not TileControl.IsDesigning and (FPlace.Group <> nil) then
      AAccepted := FPlace.Group.Group.Enabled;
    if AAccepted then
      TileControl.DoItemDragOver(AAccepted);
  end;
  FDragOverIsAccepted := AAccepted;
end;

procedure TdxTileControlDragDropItem.DragAndDropItem(var Accepted: Boolean);
begin
  Accepted := True;
  if (CheckedItems <> nil) and (CheckedItems[0].GroupIndex > -1) then
    RemoveCheckedItemsFromGroups;
  DragDropChanges.BeforeUpdate;
  try
    TryDecreaseBeginGroup;
    CheckPlaceDragItem(Accepted);
    if not Accepted then
      MoveDragItemFromControl
    else
    begin
      if FPlace.Group <> nil then
        CalculateDragAndDropItemInGroup
      else
        CalculateDragAndDropItemInVirtualGroup(Accepted);
      if FPlace.VirtualGroup <> nil then
      begin
        if not FPlace.VirtualGroup.SizeMustFromDragItem  then
        begin
          FPlace.VirtualGroup.SizeMustFromDragItem := True;
          ViewInfo.Calculate;
        end;
        FPlace.VirtualGroup.State := vgsVisible;
      end;
    end;
    FPrevPlace.Assign(FPlace);
  finally
    DragDropChanges.AfterUpdate;
  end;
end;

procedure TdxTileControlDragDropItem.EndDragAndDropItem(Accepted: Boolean; var ANewGroup: TdxTileControlGroup);
var
  I: Integer;
begin
  if not Accepted or ((FPrevPlace.Group = nil) and (FPrevPlace.VirtualGroup = nil)) then
    RestoreSourcePositions
  else
  begin
    if FPrevPlace.VirtualGroup <> nil then
    begin
      FPrevPlace.VirtualGroup.State := vgsNoVisible;
      ANewGroup := CreateNewGroupAndMoveDragItemTo(FPrevPlace.VirtualGroup);
      TileControl.RemoveGroupIfEmpty(BeginGroup);
    end
    else
    if not BeginGroup.Visible then
      TileControl.RemoveGroupIfEmpty(BeginGroup);
    if CheckedItems <> nil then
      for I := 0 to CheckedItems.Count - 1 do
      begin
        CheckedItems[I].Group := DragItem.Group;
        CheckedItems[I].IndexInGroup := DragItem.IndexInGroup + I + 1;
      end;
  end;
end;

procedure TdxTileControlDragDropItem.CalculateDragAndDropItemInGroup;
begin
  if FPlace.Group = FPrevPlace.Group then
    CalculateDragAndDropItemInPrevGroup
  else
    CalculateDragAndDropItemInAnotherGroup
end;

procedure TdxTileControlDragDropItem.CalculateDragAndDropItemInAnotherGroup;
var
  AGroup: TdxTileControlGroup;
  I: Integer;
  AChanging: TSize;
begin
  AGroup := FPlace.Group.Group;
  MoveDragItemToGroup(AGroup, GetNewIndexOfDragItem(AGroup));
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    AChanging := GetMaximizedAreaBoundsChanging(AGroup);
    if (AChanging.cx <> 0) or (AChanging.cy <> 0) then
    begin
      ViewInfo.Calculate;
      Break;
    end;
  end;
end;

procedure TdxTileControlDragDropItem.CalculateDragAndDropItemInPrevGroup;
var
  AOldIndex, ANewIndex: Integer;
  APrevGroup: TdxTileControlGroup;
  AChanging: TSize;
begin
  APrevGroup := FPrevPlace.Group.Group;
  AOldIndex := DragItem.IndexInGroup;
  FTemporaryGroup.AssignFrom(APrevGroup, DragItem);
  StoreGroupItemsPositions(APrevGroup);
  try
    FTemporaryGroup.ViewInfo.CalculateDimensionAndItemsPositions;
    ANewIndex := GetNewIndexOfDragItem(FTemporaryGroup);
  finally
    RestoreGroupItemsPositions(APrevGroup);
  end;
  if ANewIndex <> AOldIndex then
  begin
    DragItem.Move(APrevGroup, ANewIndex);
    AChanging := GetMaximizedAreaBoundsChanging(APrevGroup);
    if (AChanging.cx <> 0) or (AChanging.cy <> 0) then
      ViewInfo.Calculate;
  end;
end;

procedure TdxTileControlDragDropItem.CalculateDragAndDropItemInVirtualGroup(var AAccepted: Boolean);
var
  AGroup, ATemporaryGroup: TdxTileControlGroup;
begin
  if GetStayInPlaceVirtualGroup then Exit;
  AGroup := GetGroupCrossedOnTheRight;
  ATemporaryGroup := DragItemInfo.Group;
  DragItemInfo.SetGroup(AGroup);
  if IsPossibleIncreaseGroup(AGroup, AAccepted) then
  begin
    MoveDragItemToGroup(AGroup, AGroup.ItemCount);
    ViewInfo.Calculate;
    CheckPlaceDragItem(AAccepted);
    if FPlace.Group <> nil then
      CalculateDragAndDropItemInGroup
    else
      DragItemInfo.SetGroup(ATemporaryGroup);
  end
  else
    DragItemInfo.SetGroup(ATemporaryGroup);
end;

function TdxTileControlDragDropItem.CanDecreaseBeginGroup: Boolean;
var
  AChanging: TSize;
  ANextGroupExpandedBounds: TRect;
begin
  Result := not BeginGroup.IsMostRight;
  if not Result then
    Exit;
  if GroupLayout = glVertical then
    Result := DragItem.ViewInfo.GetLastOccupiedRow(DragItem.ViewInfo.Position.Row) = BeginGroup.RealRowCount - 1
  else
    Result := DragItem.ViewInfo.GetLastOccupiedColumn(DragItem.ViewInfo.Position.Column) = BeginGroup.RealColumnCount - 1;
  if Result then
  begin
    FTemporaryGroup.AssignFrom(BeginGroup, DragItem);
    StoreGroupItemsPositions(BeginGroup);
    try
      AChanging := GetMaximizedAreaBoundsChanging(FTemporaryGroup);
      if (AChanging.cx <> 0) or (AChanging.cy <> 0) then
      begin
        ANextGroupExpandedBounds := BeginGroup.Groups.GetNextVisibleGroup(BeginGroup).ExpandedBounds;
        ANextGroupExpandedBounds := cxRectOffset(ANextGroupExpandedBounds, Point(AChanging.cx, AChanging.cy));
        Result := PtInRect(ANextGroupExpandedBounds, GetDragPoint);
      end
      else
        Result := False;
    finally
      RestoreGroupItemsPositions(BeginGroup);
    end;
  end;
end;

procedure TdxTileControlDragDropItem.CheckGroupDragItem;
begin
  FPlace.Group := nil;
  FPlace.VirtualGroup := nil;
  if not IsRectInControl(DragBounds) then Exit;
  FindGroupOccupiedCenterOfDragItem;
  if FPlace.Group = nil then
  begin
    FindVirtualGroupOccupiedCenterOfDragItem;
    if FPlace.VirtualGroup = nil then
      FindSomethingOccupiedPartiallyOfDragItem;
  end;
end;

procedure TdxTileControlDragDropItem.CheckPlaceDragItem(var AAccepted: Boolean);
begin
  CheckGroupDragItem;
  if FPlace.Group <> nil then
    CheckPositionDragItem
  else
  begin
    FPlace.Position.Row := -1;
    FPlace.Position.Column := -1;
  end;
  AAccepted := (FPlace.Group <> nil) or (FPlace.VirtualGroup <> nil);
  DragItemInfo.FGroup := FPlace.GetGroup;
  DoItemDragOver(AAccepted);
  if AAccepted and IsExitFromVirtualGroup then
  begin
    SwitchOff(FPrevPlace.VirtualGroup);
    FPrevPlace.Assign(FPlace);
    CheckPlaceDragItem(AAccepted);
  end;
end;

procedure TdxTileControlDragDropItem.CheckPositionDragItem;
var
  ACheckedBounds: TRect;
  ADragPoint: TPoint;
begin
  ADragPoint := GetDragPoint;
  ACheckedBounds := FPlace.Group.ExpandedBounds;
  if PtInRect(ACheckedBounds, ADragPoint) then
    CheckPositionDragPoint(ADragPoint)
  else
    CheckPositionDragPoint(GetSecondaryDragPoint(ACheckedBounds));
end;

procedure TdxTileControlDragDropItem.CheckPositionDragPoint(ADragPoint: TPoint);
var
  ABounds: TRect;
  AOffset: TPoint;
  ASizeX, ASizeY: Integer;
  ARow, AColumn: Integer;
begin
  AOffset := FPlace.Group.MaximizedAreaBounds.TopLeft;
  ABounds := cxRectOffset(FPlace.Group.MaximizedAreaBounds, AOffset, False);
  ADragPoint := cxPointOffset(ADragPoint, AOffset, False);
  ASizeX := TileControl.ViewInfo.ItemSmallWidth + TileControl.OptionsView.ItemIndent;
  ASizeY := TileControl.ViewInfo.ItemSmallHeight + TileControl.OptionsView.ItemIndent;
  ARow := Max(0, ADragPoint.Y div ASizeY);
  AColumn := Max(0, ADragPoint.X div ASizeX);

  case DragItem.Size of
    tcisRegular:
      begin
        ARow := TdxTileControlGroupViewInfo.GetStartOfCurrentRegularRow(ARow);
        AColumn := TdxTileControlGroupViewInfo.GetStartOfCurrentRegularColumn(AColumn);
      end;
    tcisLarge, tcisExtraLarge:
      begin
        ARow := TdxTileControlGroupViewInfo.GetStartOfCurrentRegularRow(ARow);
        AColumn := TdxTileControlGroupViewInfo.GetStartOfCurrentLogicalColumn(AColumn, 2 * TileControl.OptionsView.GroupBlockMaxColumnCount);
      end;
  end;

  FPlace.Position.FRow := ARow;
  FPlace.Position.FColumn := AColumn;
end;

function TdxTileControlDragDropItem.CreateNewGroupAndMoveDragItemTo(
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo): TdxTileControlGroup;
var
  AIndex: Integer;
begin
  AIndex := AVirtualGroup.Group.Index + Ord(AVirtualGroup = AVirtualGroup.Group.VirtualGroupAfter);
  Result := TileControl.InsertGroup(AIndex);
  Result.ViewInfo.FNewGroupDrawingBounds := AVirtualGroup.DrawingBounds;
  MoveDragItemToGroup(Result, 0);
end;

procedure TdxTileControlDragDropItem.InitializePrevPlace;
begin
  FPrevPlace.Group := BeginGroup.ViewInfo;
  FPrevPlace.VirtualGroup := nil;
  FPrevPlace.Position.Assign(DragItem.ViewInfo.Position);
end;

function TdxTileControlDragDropItem.IsDragItemsGroupHasMaxExpanded: Boolean;
var
  AGroup: TdxTileControlGroup;
begin
  AGroup := DragItem.Group;
  Result := False;
  if AGroup = nil then Exit;
  Result := True;
  if not IsDragBoundsCrossesOnTheRight(AGroup) then Exit;
  FTemporaryGroup.AssignFrom(AGroup, DragItem);
  StoreGroupItemsPositions(AGroup);
  try
    FTemporaryGroup.FItems.Add(DragItem);
    FTemporaryGroup.ViewInfo.Recalculate;
    if GroupLayout = glHorizontal then
      Result := cxRectWidth(AGroup.ExpandedBounds) = cxRectWidth(FTemporaryGroup.ExpandedBounds)
    else
      Result := cxRectHeight(AGroup.ExpandedBounds) = cxRectHeight(FTemporaryGroup.ExpandedBounds);
  finally
    RestoreGroupItemsPositions(AGroup);
  end;
end;

function TdxTileControlDragDropItem.IsDragItemsGroupStayMoreRight: Boolean;
var
  AGroup: TdxTileControlGroup;
begin
  AGroup := DragItem.Group;
  Result := (AGroup <> nil) and (AGroup.Bounds.Left > FPlace.VirtualGroup.Bounds.Left);
end;

procedure TdxTileControlDragDropItem.FindGroupOccupiedCenterOfDragItem;
var
  I: Integer;
  AGroup: TdxTileControlGroupViewInfo;
  ADragPoint: TPoint;
begin
  ADragPoint := GetDragPoint;
  for I := 0 to TileControl.Groups.Count - 1 do
    if TileControl.Groups[I].Visible then
    begin
      AGroup := TileControl.Groups[I].ViewInfo;
      if PtInRect(AGroup.ExpandedBounds, ADragPoint) then
      begin
        FPlace.Group := AGroup;
        Break;
      end;
    end;
end;

procedure TdxTileControlDragDropItem.FindVirtualGroupOccupiedCenterOfDragItem;
var
  I: Integer;
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo;
  ADragPoint: TPoint;
begin
  ADragPoint := GetDragPoint;
  for I := 0 to VirtualGroups.Count - 1 do
  begin
    AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[I]);
    if PtInRect(AVirtualGroup.Bounds, ADragPoint) then
    begin
      FPlace.VirtualGroup := AVirtualGroup;
      Break;
    end;
  end;
end;

procedure TdxTileControlDragDropItem.FindSomethingOccupiedPartiallyOfDragItem;
var
  I: Integer;
  AGroup: TdxTileControlGroupViewInfo;
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo;
  R, ADragBounds: TRect;
  ASquare, AMaxSquare: Integer;
  AResult: TdxTileControlCustomGroupViewInfo;
begin
  AMaxSquare := 0;
  AResult := nil;
  ADragBounds := GetActualDragBounds;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I].ViewInfo;
    if AGroup.Group.Visible and cxRectIntersect(R, ADragBounds, AGroup.ExpandedBounds) then
    begin
      ASquare := cxRectSquare(R);
      if ASquare > AMaxSquare then
      begin
        AMaxSquare := ASquare;
        AResult := AGroup;
      end;
    end;
  end;
  for I := 0 to VirtualGroups.Count - 1 do
  begin
    AVirtualGroup := VirtualGroups[I] as TdxTileControlVirtualGroupViewInfo;
    if cxRectIntersect(R, ADragBounds, AVirtualGroup.Bounds) then
    begin
      ASquare := cxRectSquare(R);
      if ASquare > AMaxSquare then
      begin
        AMaxSquare := ASquare;
        AResult := AVirtualGroup;
      end;
    end;
  end;
  if AResult is TdxTileControlGroupViewInfo then
    FPlace.Group := AResult as TdxTileControlGroupViewInfo
  else
    if AResult is TdxTileControlVirtualGroupViewInfo then
      FPlace.VirtualGroup := AResult as TdxTileControlVirtualGroupViewInfo;
end;

function TdxTileControlDragDropItem.GetBeginGroup: TdxTileControlGroup;
begin
  Result := DragItem.GroupBeforeDrag;
end;

function TdxTileControlDragDropItem.GetBeginIndex: Integer;
begin
  Result := DragItem.IndexInGroupBeforeDrag;
end;

function TdxTileControlDragDropItem.GetCheckedItems: TdxTileControlCheckedItems;
begin
  Result := DragItemInfo.CheckedItems;
end;

function TdxTileControlDragDropItem.GetGroupCrossedOnTheRight: TdxTileControlGroup;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
begin
  Result := nil;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    if IsDragBoundsCrossesOnTheRight(AGroup) then
    begin
      Result := AGroup;
      Break;
    end;
  end;
end;

function TdxTileControlDragDropItem.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlDragDropItem.GetDragItemInfo: TdxTileControlDragItemInfo;
begin
  Result := TileControl.DragItemInfo;
end;

function TdxTileControlDragDropItem.GetDragPoint: TPoint;
var
  ABounds: TRect;
begin
  ABounds := DragBounds;
  if DragItem.RowCount > 1 then
    ABounds.Bottom := ABounds.Top + DragItem.ViewInfo.GetHeight;
  Result := cxRectCenter(ABounds);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertPoint(Result, ViewInfo.ClientBounds);
end;

function TdxTileControlDragDropItem.GetMaximizedAreaBoundsChanging(AGroup: TdxTileControlGroup): TSize;
var
  AMaximizedAreaBounds: TRect;
begin
  AMaximizedAreaBounds := AGroup.MaximizedAreaBounds;
  RecalculateGroup(AGroup);
  Result.cx := cxRectWidth(AGroup.MaximizedAreaBounds) - cxRectWidth(AMaximizedAreaBounds);
  Result.cy := cxRectHeight(AGroup.MaximizedAreaBounds) - cxRectHeight(AMaximizedAreaBounds);
end;

function TdxTileControlDragDropItem.GetNewIndexOfDragItem(AGroup: TdxTileControlGroup): Integer;

  function GetPositionCountOccupiedWithHeadOfItem(AItem: TdxTileControlItem): Integer;
  var
    AColumn: Integer;
    ASize: TdxTileControlItemSize;
  begin
    AColumn := AItem.ViewInfo.Position.Column;
    Result := AItem.ViewInfo.GetLastOccupiedColumn(AColumn) - AColumn + 1;
    ASize := AItem.Size;
    if ASize in [tcisRegular, tcisLarge, tcisExtraLarge] then
      Result := 2 * Result;
  end;

var
  AItem: TdxTileControlItem;
  I, AIndex, ADragItemIndex: Integer;
begin
  ADragItemIndex := AGroup.ViewInfo.GetZIndex(Place.Position, Place.Position);
  Result := 0;
  for I := AGroup.ItemCount - 1 downto 0 do
  begin
    AItem := AGroup.Items[I];
    AIndex := AGroup.ViewInfo.GetZIndex(AItem.ViewInfo.Position, Place.Position);
    if AIndex + GetPositionCountOccupiedWithHeadOfItem(AItem) - 1 < ADragItemIndex then
    begin
      Result := I + 1;
      Break;
    end;
  end;
end;

function TdxTileControlDragDropItem.GetSecondaryDragPoint(ABounds: TRect): TPoint;
var
  AHelper: TdxTileControlDragAndDropSecondaryDragPointHelper;
begin
  AHelper := TdxTileControlDragAndDropSecondaryDragPointHelper.Create(TileControl, DragBounds, ABounds);
  try
    Result := AHelper.GetSecondaryDragPoint;
  finally
    FreeAndNil(AHelper);
  end;
end;

function TdxTileControlDragDropItem.GetSpecialPointX: Integer;
begin
  Result := cxRectCenter(GetActualDragBounds).X - TileControl.OptionsView.ItemWidth div 4;
end;

function TdxTileControlDragDropItem.GetSpecialPointY: Integer;
begin
  Result := DragBounds.Top + TileControl.OptionsView.ItemHeight div 4;
end;

function TdxTileControlDragDropItem.GetStayInPlaceVirtualGroup: Boolean;
begin
  Result := False;
  if not FPrevAccepted or IsDragItemsGroupStayMoreRight or IsDragItemsGroupHasMaxExpanded then
  begin
    DragItem.RemoveFromGroup;
    FPlace.VirtualGroup.SizeMustFromDragItem := True;
    ViewInfo.Calculate;
    Result := True;
  end;
end;

function TdxTileControlDragDropItem.GetVirtualGroups: TcxObjectList;
begin
  Result := ViewInfo.VirtualGroups;
end;

procedure TdxTileControlDragDropItem.InitializeDragImage;

  procedure DrawCheckedCountInfo;
  var
    ABounds: TRect;
  begin
    ABounds := DragCell.Bounds;
    ABounds.Left := ABounds.Right - 3 * TileControl.ViewInfo.ItemSmallWidth div 4;
    ABounds.Bottom := ABounds.Top + 3 * TileControl.ViewInfo.ItemSmallHeight div 4;
    DragImage.Canvas.FillRect(ABounds, dxGetColorTint(FCountInfoFont.Color, -70));
    cxTextOut(DragImage.Canvas.Handle, IntToStr(CheckedItems.Count + 1), ABounds, cxMakeFormat(taCenterX, taCenterY),
      0, 0, FCountInfoFont, clNone, clNone, 0, 0, 0, FCountInfoFont.Color);
  end;

begin
  inherited InitializeDragImage;
  DragCellAsItem.ViewInfo.DrawContent(DragImage.Canvas, DragCell.Bounds);
  if State = tcddsPulling then
  begin
    if DragCellAsItem.Checked <> IsPullDownGesture then
      DragCellAsItem.ViewInfo.DrawChecked(DragImage.Canvas);
  end
  else
    if CheckedItems <> nil then
      DrawCheckedCountInfo;
end;

function TdxTileControlDragDropItem.IsDragBoundsCrossesOnTheRight(AGroup: TdxTileControlGroup): Boolean;
var
  ABounds, ADragBounds: TRect;
begin
  Result := False;
  if not AGroup.Visible then Exit;
  ABounds := AGroup.MaximizedAreaBounds;
  ADragBounds := GetActualDragBounds;
  if GroupLayout = glHorizontal then
    Result := (PtInRect(ABounds, ADragBounds.TopLeft) or PtInRect(ABounds, cxRectLeftBottom(ADragBounds))) and
     (GetSpecialPointX <= ABounds.Right)
  else
    Result := (PtInRect(ABounds, ADragBounds.TopLeft) or PtInRect(ABounds, cxRectRightTop(ADragBounds))) and
     (GetSpecialPointY <= ABounds.Bottom);
end;

function TdxTileControlDragDropItem.IsExitFromVirtualGroup: Boolean;
begin
  Result := (FPrevPlace.VirtualGroup <> nil) and
    not FPrevPlace.VirtualGroup.IsEqual(FPlace.VirtualGroup);
end;

function TdxTileControlDragDropItem.IsPossibleIncreaseGroup(AGroup: TdxTileControlGroup;
  var AAccepted: Boolean): Boolean;
var
  ARowCountBefore, AColumnCountBefore: Integer;
begin
  DoItemDragOver(AAccepted);
  Result := (AGroup <> nil) and AAccepted and (TileControl.IsDesigning or AGroup.Enabled) and (AGroup.ItemCount > 0) and
    (FPlace.VirtualGroup.IsEqual(FPrevPlace.VirtualGroup) or (AGroup.ViewInfo = FPrevPlace.Group));
  if not Result then Exit;
  FTemporaryGroup.AssignFrom(AGroup, DragItem);
  StoreGroupItemsPositions(AGroup);
  try
    FTemporaryGroup.ViewInfo.CalculateDimensionAndItemsPositions;
    ARowCountBefore := FTemporaryGroup.RealRowCount;
    AColumnCountBefore := FTemporaryGroup.RealColumnCount;
    FTemporaryGroup.FItems.Add(DragItem);
    FTemporaryGroup.ViewInfo.CalculateDimensionAndItemsPositions;
    if GroupLayout = glHorizontal then
      Result := AColumnCountBefore < FTemporaryGroup.RealColumnCount
    else
      Result := ARowCountBefore < FTemporaryGroup.RealRowCount;
  finally
    RestoreGroupItemsPositions(AGroup);
  end;
end;

procedure TdxTileControlDragDropItem.MoveDragItemFromControl;
var
  AGroup: TdxTileControlGroup;
  AOldBounds: TRect;
begin
  if FPrevPlace.Group <> nil then
  begin
    AGroup := FPrevPlace.Group.Group;
    AOldBounds := AGroup.MaximizedAreaBounds;
    DragItem.RemoveFromGroup;
    RecalculateGroup(AGroup);
    if not cxRectIsEqual(AOldBounds, AGroup.MaximizedAreaBounds) then
      ViewInfo.Calculate;
  end
  else
    if FPrevPlace.VirtualGroup <> nil then
    begin
      SwitchOff(FPrevPlace.VirtualGroup);
      ViewInfo.Calculate;
      FPrevPlace.VirtualGroup := nil;
    end;
end;

procedure TdxTileControlDragDropItem.MoveDragItemToGroup(AGroup: TdxTileControlGroup; AIndexInGroup: Integer);
begin
  if AIndexInGroup < 0 then Exit;
  DragItem.Move(AGroup, AIndexInGroup);
  if (BeginGroup <> AGroup) and (BeginGroup.ItemCount = 0) and BeginGroup.Visible then
    DoCollapseBeginGroupWhenItemIsLast
  else
    RecalculateGroup(AGroup);
end;

procedure TdxTileControlDragDropItem.PullDown(Accepted: Boolean);
begin
  if Accepted then
    (DragCell as TdxTileControlItemViewInfo).Item.ToggleChecked;
end;

procedure TdxTileControlDragDropItem.RestoreGroupItemsPositions(AGroup: TdxTileControlGroup);
var
  I: Integer;
begin
  for I := 0 to FGroupItemsPositions.Count - 1 do
    AGroup.Items[I].ViewInfo.Position.Assign(TdxLayoutItemPosition(FGroupItemsPositions[I]));
end;

procedure TdxTileControlDragDropItem.RecalculateGroup(AGroup: TdxTileControlGroup);
begin
  AGroup.ViewInfo.Recalculate;
end;

procedure TdxTileControlDragDropItem.RemoveCheckedItemsFromGroups;
var
  I: Integer;
begin
  StoreCheckedItemsSourceLayout;
  DragDropChanges.BeforeUpdate;
  try
    for I := 0 to CheckedItems.Count - 1 do
      CheckedItems[I].GroupIndex := -1;
    for I := 0 to TileControl.Groups.Count - 1 do
      TileControl.Groups[I].Visible := TileControl.Groups[I].ItemCount > 0;
  finally
    DragDropChanges.AfterUpdate;
  end;
end;

procedure TdxTileControlDragDropItem.RestoreCheckedItemsSourceLayout;
var
  I: Integer;
  AItem: TdxTileControlItem;
begin
  for I := 0 to CheckedItems.Count - 1 do
  begin
    AItem := CheckedItems[I];
    if AItem = DragItem then
      RestoreDragItemSourcePosition
    else
    begin
      AItem.RestoreBeforeDragLayout;
      AItem.Group.Visible := True;
    end;
  end;
end;

procedure TdxTileControlDragDropItem.RestoreDragItemSourcePosition;
begin
  DragItem.Move(BeginGroup, BeginIndex);
  FPlace.Group := BeginGroup.ViewInfo;
  DragItem.ViewInfo.Recalculate;
  if not BeginGroup.Visible then
    BeginGroup.Visible := True;
  ViewInfo.Calculate;
end;

procedure TdxTileControlDragDropItem.RestoreSourcePositions;

  function IsLayoutChanged: Boolean;
  begin
    Result := (DragItem.Group = nil) or (DragItem.Group <> BeginGroup) or (DragItem.IndexInGroup <> BeginIndex) or
      (CheckedItems <> nil);
  end;

begin
  if IsLayoutChanged then
    if CheckedItems <> nil then
    begin
      CheckedItems.Add(DragItem);
      CheckedItems.SortByIndexInGroupBeforeDrag;
      RestoreCheckedItemsSourceLayout;
    end
    else
      RestoreDragItemSourcePosition;
  SetNoVisibleAllVirtualGroups;
end;

procedure TdxTileControlDragDropItem.SetNoVisibleAllVirtualGroups;
var
  I: Integer;
begin
  for I := 0 to VirtualGroups.Count - 1 do
    TdxTileControlVirtualGroupViewInfo(VirtualGroups[I]).State := vgsNoVisible;
end;

procedure TdxTileControlDragDropItem.SwitchOff(AVirtualGroup: TdxTileControlVirtualGroupViewInfo);
begin
  AVirtualGroup.State := vgsNoVisible;
  if FPlace.Group = nil then
    ViewInfo.Calculate;
end;

procedure TdxTileControlDragDropItem.StoreCheckedItemsSourceLayout;
var
  I: Integer;
begin
  for I := 0 to CheckedItems.Count - 1 do
    CheckedItems[I].StoreLayoutBeforeDrag;
end;

procedure TdxTileControlDragDropItem.StoreGroupItemsPositions(AGroup: TdxTileControlGroup);
var
  APosition: TdxLayoutItemPosition;
  I: Integer;
begin
  FGroupItemsPositions.Clear;
  for I := 0 to AGroup.ItemCount - 1 do
  begin
    APosition := TdxLayoutItemPosition.Create;
    APosition.Assign(AGroup.Items[I].ViewInfo.Position);
    FGroupItemsPositions.Add(APosition);
  end;
end;

procedure TdxTileControlDragDropItem.TryDecreaseBeginGroup;
begin
  if FWasTryDecreaseBeginGroup then
    Exit;
  ViewInfo.Calculate;
  FWasTryDecreaseBeginGroup := True;
  if BeginGroup.ItemCount = 1 then
    DoCollapseBeginGroupWhenItemIsLast
  else
    if CanDecreaseBeginGroup then
    begin
      DragItem.RemoveFromGroup;
      FPrevPlace.Group := nil;
      ViewInfo.Calculate;
    end;
end;

{ TdxThreadScalerTaskItem }

constructor TdxThreadScalerTaskItem.Create(AImageItem: TcxImageCollectionItem; AOutputSize: TSize; ACallBackProc: TNotifyEvent);
begin
  inherited Create;
  FImageItem := AImageItem;
  FOutputSize := AOutputSize;
  FCallBackProc := ACallBackProc;
end;

destructor TdxThreadScalerTaskItem.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxThreadScalerTaskItem.ClearCallBack;
begin
  FCallBackProc := nil;
end;

procedure TdxThreadScalerTaskItem.LoadGraphicToImage(AGraphic: TGraphic; AImage: TdxSmartImage);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AGraphic.SaveToStream(AStream);
    AStream.Position := 0;
    AImage.LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxThreadScalerTaskItem.Scale;
const
  AMode: array[Boolean] of TcxImageFitMode = (ifmFit, ifmFill);

  procedure CleanRoundingError(var ARect: TRect);
  begin
    if cxRectWidth(ARect) - FOutputSize.cx = -1 then
      ARect.Right := ARect.Right + 1;
    if cxRectHeight(ARect) - FOutputSize.cy = -1 then
      ARect.Bottom := ARect.Bottom + 1;
  end;

var
  AGraphic: TGraphic;
  AImage: TdxSmartImage;
  ACanvas: TdxGPCanvas;
  R: TRect;
begin
  FImage := TdxSmartImage.CreateSize(FOutputSize);
  AGraphic := FImageItem.Picture.Graphic;
  AImage := TdxSmartImage.CreateSize(AGraphic.Width, AGraphic.Height);
  try
    if AGraphic is TdxGPImage then
      AImage.Assign(AGraphic)
    else
      LoadGraphicToImage(AGraphic, AImage);
    R := cxGetImageRect(cxRect(FOutputSize), Size(AGraphic.Width, AGraphic.Height),
      AMode[(FOutputSize.cx < AGraphic.Width) and (FOutputSize.cy < AGraphic.Height)]);
    CleanRoundingError(R);
    AImage.Resize(cxRectWidth(R), cxRectHeight(R));

    ACanvas := FImage.CreateCanvas;
    try
      ACanvas.Clear(clNone);
      ACanvas.Draw(AImage, R);
    finally
      ACanvas.Free;
    end;
  finally
    AImage.Free;
  end;
end;

{ TdxThreadScaler }

constructor TdxThreadScaler.Create;
begin
  inherited Create(False, True);
  FTasks := TThreadList.Create;
end;

destructor TdxThreadScaler.Destroy;
begin
  ClearTasks;
  FreeAndNil(FTasks);
  inherited Destroy;
end;

procedure TdxThreadScaler.AddTask(AImageItem: TcxImageCollectionItem; AOutputSize: TSize; ACallBackProc: TNotifyEvent);
begin
  FTasks.Add(TdxThreadScalerTaskItem.Create(AImageItem, AOutputSize, ACallBackProc));
  Unpause;
end;

procedure TdxThreadScaler.CancelCurrentTask;
begin
  FCurrentTask.ClearCallBack;
  Pause(True);
  Unpause;
end;

procedure TdxThreadScaler.CancelTasks(ACallBackProc: TNotifyEvent);
var
  L: TList;
  I: Integer;
begin
  L := FTasks.LockList;
  try
    if Assigned(FCurrentTask) and dxSameMethods(FCurrentTask.CallBackProc, ACallBackProc) then
      CancelCurrentTask;
    for I := L.Count - 1 downto 0 do
      if dxSameMethods((TObject(L[I]) as TdxThreadScalerTaskItem).CallBackProc, ACallBackProc) then
      begin
        (TObject(L[I]) as TdxThreadScalerTaskItem).Free;
        L.Delete(I);
      end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TdxThreadScaler.CancelTasks(AImageItem: TcxImageCollectionItem);
var
  L: TList;
  I: Integer;
begin
  L := FTasks.LockList;
  try
    if Assigned(FCurrentTask) and (FCurrentTask.ImageItem = AImageItem) then
      CancelCurrentTask;
    for I := L.Count - 1 downto 0 do
      if (TObject(L[I]) as TdxThreadScalerTaskItem).ImageItem = AImageItem then
      begin
        (TObject(L[I]) as TdxThreadScalerTaskItem).Free;
        L.Delete(I);
      end;
  finally
    FTasks.UnlockList;
  end;
end;

function TdxThreadScaler.HasTaskForProc(ACallBackProc: TNotifyEvent): Boolean;
var
  L: TList;
  I: Integer;
begin
  L := FTasks.LockList;
  try
    Result := Assigned(FCurrentTask) and dxSameMethods(FCurrentTask.CallBackProc, ACallBackProc);
    if not Result then
      for I := 0 to L.Count - 1 do
      begin
        Result := dxSameMethods((TObject(L[I]) as TdxThreadScalerTaskItem).CallBackProc, ACallBackProc);
        if Result then
          Break;
      end;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TdxThreadScaler.Stop;
begin
  if Assigned(FCurrentTask) then
    FCurrentTask.ClearCallBack;
  Terminate;
  Unpause;
  WaitFor;
end;

procedure TdxThreadScaler.ClearTasks;
var
  I: Integer;
  L: TList;
begin
  L := FTasks.LockList;
  try
    for I := L.Count - 1 downto 0 do
      TdxThreadScalerTaskItem(L[I]).Free;
  finally
    FTasks.UnlockList;
  end;
end;

procedure TdxThreadScaler.DoCallBack;
begin
  if Assigned(FCurrentTask.CallBackProc) then
    FCurrentTask.CallBackProc(FCurrentTask);
end;

procedure TdxThreadScaler.Execute;
var
  L: TList;
begin
  while not (Terminated or Destroying) do
  begin
    CheckForPause;
    L := FTasks.LockList;
    try
      if L.Count = 0 then
        FCurrentTask := nil
      else
      begin
        FCurrentTask := L[0];
        L.Delete(0);
      end;
    finally
      FTasks.UnlockList;
    end;
    if Assigned(FCurrentTask) then
      ProcessTask
    else
      Pause;
  end;
end;

procedure TdxThreadScaler.ProcessTask;
begin
  if not Assigned(FCurrentTask.CallBackProc) then
  begin
    FreeAndNil(FCurrentTask);
    Exit;
  end;
  FCurrentTask.Scale;
  if Assigned(FCurrentTask.CallBackProc) then
    Synchronize(DoCallBack);
  FreeAndNil(FCurrentTask);
end;

{ TdxTileControlController }

constructor TdxTileControlController.Create(AOwner: TdxCustomTileControl);
begin
  FTileControl := AOwner;
  FRestoreItemsBoundsOnMouseUp := True;
  FCanItemDrawing := True;
  FItemsContentAnimationLockCount := 1;
  dxAnimationController.AddListener(Self);
  FAnimationItems := TList.Create;
  FAnimations := TList.Create;
  FHintController := TdxTileControlHintController.Create(Self);
  FNavigator := TdxTileControlItemNavigator.Create(Self);
end;

destructor TdxTileControlController.Destroy;
begin
  EndMouseTracking(Self);
  FreeAndNil(FAnimations);
  FreeAndNil(FHintController);
  FreeAndNil(FAnimationItems);
  FreeAndNil(FNavigator);
  DestroyDesignHelper;
  dxAnimationController.RemoveListener(Self);
  inherited Destroy;
end;

procedure TdxTileControlController.ChangePressedScrollButtonState;
const
  AState: array[Boolean] of TcxButtonState = (cxbsDisabled, cxbsDefault);
begin
  if PressedScrollButton = nil then Exit;
  PressedScrollButton.State := AState[(PressedScrollButton.State = cxbsPressed) and
    ViewInfo.IsScrollAvailable(PressedScrollButton.Direction)];
  PressedScrollButton := nil;
end;

procedure TdxTileControlController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not TileControl.IsDesigning and FNavigator.IsAcceptedKey(Key) then
    FNavigator.KeyDown(Key, Shift);

  case Key of
    VK_RETURN:
      if FocusedItem <> nil then
        FocusedItem.Click;
    VK_SPACE:
      if FocusedItem <> nil then
        FocusedItem.ToggleChecked;
    VK_ESCAPE:
      UncheckAllItems;
  end;
end;

procedure TdxTileControlController.PrepareToDragAndDrop;
begin
  HideActionBars(abvcrTileElementInteraction);
  HideScrollButtons;
  RestoreItemsBounds;
  FMouseDownItem := nil;
end;

procedure TdxTileControlController.ProcessContextMenuMessage(var Message: TWMContextMenu);
begin
  if Message.Result = 1 then
    FContextMenuHandled := True;
end;

procedure TdxTileControlController.StartAnimations;
begin
  StartItemContentAnimation;
end;

procedure TdxTileControlController.StartItemContentAnimation;
begin
  Dec(FItemsContentAnimationLockCount);
  if ItemsContentAnimationLockCount = 0 then
    TileControl.UnlockTimers;
end;

procedure TdxTileControlController.StopAnimations;
begin
  StopItemContentAnimation;
  TerminateAnimations(False);
end;

procedure TdxTileControlController.StopItemContentAnimation;
begin
  Inc(FItemsContentAnimationLockCount);
  if ItemsContentAnimationLockCount = 1 then
  begin
    if FAnimationItems.Count > 0 then
    begin
      TerminateAnimations(True);
      TileControl.Update;
    end;
    TileControl.LockTimers;
  end;
end;

procedure TdxTileControlController.HideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  TileControl.DoHideActionBars(AReason);
end;

procedure TdxTileControlController.ShowActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  TileControl.DoShowActionBars(AReason);
end;

procedure TdxTileControlController.ToggleActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  if TileControl.IsAnyActionBarVisible then
    HideActionBars(AReason)
  else
    ShowActionBars(AReason);
end;

procedure TdxTileControlController.RestoreItemBounds(AItem: TdxTileControlItem; const AData: Pointer);
begin
  AItem.ViewInfo.InflateDelta := 0;
end;

procedure TdxTileControlController.SetFocusedItem(AValue: TdxTileControlItem);

  function InternalCanFocus: Boolean;
  begin
    Result := True;
    if not(csDestroying in TileControl.ComponentState) and Assigned(TileControl.OnItemFocusChanging) then
      TileControl.OnItemFocusChanging(TileControl, AValue, Result);
  end;

var
  AOldItem: TdxTileControlItem;
begin
  AOldItem := FFocusedItem;
  if AOldItem = AValue then
    Exit;
  if InternalCanFocus then
  begin
    if FFocusedItem <> nil then
      FFocusedItem.Invalidate;
    FFocusedItem := AValue;
    Navigator.InitializeCurrentPosition;
    if FFocusedItem <> nil then
    begin
      FFocusedItem.Invalidate;
      RefreshItems;
    end;
    if not(csDestroying in TileControl.ComponentState) and Assigned(TileControl.OnItemFocusChange) then
      TileControl.OnItemFocusChange(TileControl, AOldItem, FFocusedItem);
  end;
end;

procedure TdxTileControlController.SetHottrackedItem(AValue: TdxTileControlItem);

  function IsItemHottrackVisible(AMode: TdxTileControlItemHotTrackMode): Boolean;
  begin
    Result := (FHottrackedItem <> nil) and (AMode <> tcihtmNone);
  end;

var
  AMode: TdxTileControlItemHotTrackMode;
  AOldItem: TdxTileControlItem;
begin
  if FHottrackedItem = AValue then Exit;
  if (AValue <> nil) and (not AValue.IsGroupEnabled) then
    AValue := nil;
  if AValue = FHottrackedItem then Exit;
  AMode := TileControl.OptionsBehavior.ItemHotTrackMode;
  AOldItem := FHottrackedItem;
  if IsItemHottrackVisible(AMode) then
    if AMode = tcihtmHighlight then
      FHottrackedItem.ViewInfo.HottrackValue := 0
    else
      AddAnimation(TdxTileControlHottrackItemAnimation.Create(FHottrackedItem, False));
  FHottrackedItem := AValue;
  if IsItemHottrackVisible(AMode) then
    if AMode = tcihtmHighlight then
      FHottrackedItem.ViewInfo.HottrackValue := 1
    else
      AddAnimation(TdxTileControlHottrackItemAnimation.Create(FHottrackedItem, True));
  if AOldItem <> nil then
    AOldItem.Invalidate;
  if (FocusedItem <> nil) and TileControl.OptionsBehavior.HideFocusOnItemHotTrack then
    FocusedItem.Invalidate;
end;

procedure TdxTileControlController.SetPressedScrollButton(AValue: TdxTileControlScrollButtonViewInfo);
begin
  if FPressedScrollButton <> AValue then
  begin
    if FPressedScrollButton <> nil then
      FPressedScrollButton.Invalidate;
    FPressedScrollButton := AValue;
    if FPressedScrollButton <> nil then
      FPressedScrollButton.Invalidate;
  end;
end;

// design PopupMenu
function TdxTileControlController.CreatePopupMenuItem(const ACaption: string; const ATag: Integer;
  const AGroupIndex: Byte; const AChecked, ARadioItem: Boolean; AOnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := ACaption;
  Result.Checked := AChecked;
  Result.RadioItem := ARadioItem;
  Result.GroupIndex := AGroupIndex;
  Result.Tag := ATag;
  Result.OnClick := AOnClick;
end;

function TdxTileControlController.CheckDesignPopupMenu(AShift: TShiftState): Boolean;
var
  ACaller: TcxComponentCollectionItem;
begin
  if HitAtItem then
    ACaller := HitTest.Item
  else
    if HitAtGroup then
      ACaller := HitTest.Group
    else
      ACaller := nil;
  Result := ACaller <> nil;
  if Result then
    ProcessDesignPopupMenu(ACaller, AShift, GetMouseCursorPos);
end;

function TdxTileControlController.CheckCallerGroup: TdxTileControlGroup;
begin
  if FDesignPopupMenuCaller is TdxTileControlGroup then
    Result := FDesignPopupMenuCaller as TdxTileControlGroup
  else
    Result := (FDesignPopupMenuCaller as TdxTileControlItem).Group;
end;

procedure TdxTileControlController.CheckHottrackItemAfterMouseUp;
begin
  FHottrackedItem := nil;
  HottrackedItem := HitTest.Item;
end;

function TdxTileControlController.CreateRegularItem(AGroup: TdxTileControlGroup): TdxTileControlItem;
begin
  Result := TileControl.CreateItem(tcisRegular, AGroup);
end;

procedure TdxTileControlController.CreateTileControlItem(Sender: TObject);
var
  AItem: TdxTileControlItem;
begin
  AItem := CreateRegularItem(CheckCallerGroup);
  SetDesignItemSize(AItem, (Sender as TMenuItem).Tag);
  SelectObject(AItem, []);
end;

procedure TdxTileControlController.DeleteSelectedObjects(Sender: TObject);
begin
  if (Sender = nil) and IsDesignPopupMenuPresent then
    Exit;
  FDesignWhatObjectsSelected := GetDesignTimeWhatSelected;
  case FDesignWhatObjectsSelected of
    tcdtwsItem, tcdtwsItems:
      DeleteTileControlItems(Sender <> nil);
    tcdtwsGroup, tcdtwsGroups:
      DeleteTileControlGroups(Sender <> nil);
    tcdtwsMixed:
      begin
        DeleteTileControlItems(False);
        DeleteTileControlGroups(False);
      end;
  end;
end;

procedure TdxTileControlController.DeleteTileControlGroups(const ASelectNextObjectAfterDelete: Boolean);
var
  AIndex: Integer;
  AGroup: TdxTileControlGroup;
begin
  if not ASelectNextObjectAfterDelete then
  begin
    DeleteTileControlSelectedGroups;
    Exit;
  end;
  AGroup := FDesignPopupMenuCaller as TdxTileControlGroup;
  AIndex := AGroup.Index;
  DeleteTileControlSelectedGroups;
  SelectObject(GetNextObject(AIndex), []);
end;

procedure TdxTileControlController.DeleteTileControlItems(const ASelectNextObjectAfterDelete: Boolean);
var
  AItemIndex: Integer;
  AOwnerGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  if not ASelectNextObjectAfterDelete then
  begin
    DeleteTileControlSelectedItems;
    Exit;
  end;
  AItem := FDesignPopupMenuCaller as TdxTileControlItem;
  AItemIndex := AItem.IndexInGroup;
  AOwnerGroup := AItem.Group;
  DeleteTileControlSelectedItems;
  SelectObject(GetNextObject(AItemIndex, AOwnerGroup), []);
end;

procedure TdxTileControlController.DeleteTileControlSelectedGroups;
var
  AGroup: TdxTileControlGroup;
  I: Integer;
begin
  for I := TileControl.Groups.Count - 1 downto 0 do
  begin
    AGroup := TileControl.Groups[I];
    if IsObjectSelected(AGroup) then
      AGroup.Free;
  end;
end;

procedure TdxTileControlController.DeleteTileControlSelectedItems;
var
  AItem: TdxTileControlItem;
  I: Integer;
begin
  for I := TileControl.Items.Count - 1 downto 0 do
  begin
    AItem := TileControl.Items[I];
    if IsObjectSelected(AItem) then
      AItem.Free;
  end;
end;

function TdxTileControlController.GetIsExtraLargeItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := (AItem.Size = tcisExtraLarge) and (AItem.RowCount = 2);
end;

function TdxTileControlController.GetIsLargeItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := (AItem.Size = tcisLarge) and (AItem.RowCount = 1);
end;

function TdxTileControlController.GetIsRegularItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := (AItem.Size = tcisRegular) and (AItem.RowCount = 1);
end;

function TdxTileControlController.GetIsSmallItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := (AItem.Size = tcisSmall) and (AItem.RowCount = 1);
end;

function TdxTileControlController.GetDesignTimeWhatSelected: TdxTileControlDesignTimeWhatSelected;
var
  I: Integer;
begin
  Result := tcdtwsNone;
  for I := 0 to TileControl.Items.Count - 1 do
    if IsObjectSelected(TileControl.Items[I]) then
      if Result = tcdtwsItem then
      begin
        Result := tcdtwsItems;
        Break;
      end
      else
        Result := tcdtwsItem;
  for I := 0 to TileControl.Groups.Count - 1 do
    if IsObjectSelected(TileControl.Groups[I]) then
      if Result in [tcdtwsItem, tcdtwsItems] then
      begin
        Result := tcdtwsMixed;
        Break;
      end
      else
      if Result = tcdtwsGroup then
      begin
        Result := tcdtwsGroups;
        Break;
      end
      else
        Result := tcdtwsGroup;
end;

function TdxTileControlController.GetNextObject(ADeletedGroupIndex: Integer): TPersistent;
var
  I: Integer;
begin
  Result := TileControl;
  ADeletedGroupIndex := Min(ADeletedGroupIndex, TileControl.Groups.Count - 1);
  for I := 0 to TileControl.Groups.Count - 1 do
    if TileControl.Groups[I].Visible then
    begin
      Result := TileControl.Groups[I];
      if I >= ADeletedGroupIndex then
        Break;
    end;
end;

function TdxTileControlController.GetNextObject(ADeletedItemIndex: Integer; ADeletedItemGroup: TdxTileControlGroup): TPersistent;
var
  I: Integer;
begin
  Result := TileControl;
  if ADeletedItemGroup = nil then Exit;
  Result := ADeletedItemGroup;
  ADeletedItemIndex := Min(ADeletedItemIndex, ADeletedItemGroup.ItemCount - 1);
  for I := 0 to ADeletedItemGroup.ItemCount - 1 do
    if ADeletedItemGroup.Items[I].Visible then
    begin
      Result := ADeletedItemGroup.Items[I];
      if I >= ADeletedItemIndex then
        Break;
    end;
end;

function TdxTileControlController.IsDesignerMenuItemChecked(const AItemSizeIndex: Integer; AItem: TdxTileControlItem): Boolean;
begin
  case AItemSizeIndex of
    Integer(tcisSmall):
      Result := IsDesignerMenuItemChecked(AItem, GetIsSmallItem);
    Integer(tcisRegular):
      Result := IsDesignerMenuItemChecked(AItem, GetIsRegularItem);
    Integer(tcisLarge):
      Result := IsDesignerMenuItemChecked(AItem, GetIsLargeItem);
  else
    Result := IsDesignerMenuItemChecked(AItem, GetIsExtraLargeItem);
  end;
end;

function TdxTileControlController.IsDesignerMenuItemChecked(AItem: TdxTileControlItem;
  AFunc: TdxTileControlDesignTimeMenuItemCheckedCondition): Boolean;
var
  I: Integer;
begin
  Result := AFunc(AItem);
  if FDesignWhatObjectsSelected = tcdtwsItem then Exit;
  for I := 0 to TileControl.Items.Count - 1 do
  begin
    AItem := TileControl.Items[I];
    if IsObjectSelected(AItem) then
      if Result <> AFunc(AItem) then
      begin
        Result := False;
        Break;
      end;
  end;
end;

procedure TdxTileControlController.AddDesignPopupMenuItem(const ACaption: string; const ATag: Integer; const AGroupIndex: Byte;
  const AChecked, ARadioItem: Boolean; AOnClick: TNotifyEvent);
begin
  FDesignPopupMenu.Items.Add(CreatePopupMenuItem(ACaption, ATag, AGroupIndex, AChecked, ARadioItem, AOnClick));
end;

function TdxTileControlController.IsDesignPopupMenuPresent: Boolean;
begin
  Result := FDesignPopupMenu <> nil;
end;

function TdxTileControlController.IsDetectedDragItem: Boolean;
begin
  Result := HitAtItem and (TileControl.IsDesigning or HitTest.Item.CanFocused) and EnabledItemMoving;
end;

function TdxTileControlController.IsDetectedDragGroup: Boolean;
begin
  Result := HitAtGroup and EnabledGroupMoving;
end;

procedure TdxTileControlController.PopulateDesignPopupMenu;
var
  AItem: TMenuItem;
begin
  FDesignWhatObjectsSelected := GetDesignTimeWhatSelected;
  case FDesignWhatObjectsSelected of
    tcdtwsItem, tcdtwsItems:
      begin
        PopulateOneItemDesignPopupMenu;
        FDesignPopupMenu.Items.Add(NewLine);
        if FDesignWhatObjectsSelected = tcdtwsItem then
        begin
          PopulateOneGroupDesignPopupMenu;
          FDesignPopupMenu.Items.Add(NewLine);
        end;
      end;
    tcdtwsGroup:
      begin
        PopulateOneGroupDesignPopupMenu;
        FDesignPopupMenu.Items.Add(NewLine);
      end;
  end;
  AItem := CreatePopupMenuItem('Delete', 0, 0, False, False, DeleteSelectedObjects);
  AItem.ShortCut := VK_DELETE;
  FDesignPopupMenu.Items.Add(AItem);
end;

procedure TdxTileControlController.PopulateOneGroupDesignPopupMenu;
const
  AMenuItemCaption: array[Low(TdxTileControlItemSize)..High(TdxTileControlItemSize)] of string =
    ('Add Small Item', 'Add Regular Item', 'Add Large Item', 'Add Extra-Large Item');
var
  I: TdxTileControlItemSize;
begin
  for I := Low(TdxTileControlItemSize) to High(TdxTileControlItemSize) do
    AddDesignPopupMenuItem(AMenuItemCaption[I], Integer(I), 0, False, False, CreateTileControlItem);
end;

procedure TdxTileControlController.PopulateOneItemDesignPopupMenu;
const
  AMenuItemCaption: array[Low(TdxTileControlItemSize)..High(TdxTileControlItemSize)] of string =
    ('Small', 'Regular', 'Large', 'Extra-Large');
var
  I: TdxTileControlItemSize;
  AItem: TdxTileControlItem;
begin
  AItem := FDesignPopupMenuCaller as TdxTileControlItem;
  for I := Low(TdxTileControlItemSize) to High(TdxTileControlItemSize) do
    AddDesignPopupMenuItem(AMenuItemCaption[I], Integer(I), 1, IsDesignerMenuItemChecked(Integer(I), AItem), True, SwitchItemsProperty);
end;

procedure TdxTileControlController.ProcessDesignPopupMenu(
  ACaller: TcxComponentCollectionItem; AShift: TShiftState; APopupPoint: TPoint);
begin
  FreeAndNil(FDesignPopupMenu);
  if ssCtrl in AShift then
    AShift := AShift - [ssCtrl];
  if not IsObjectSelected(ACaller) then
    SelectObject(ACaller, AShift);
  if IsObjectSelected(ACaller) then
  begin
    FDesignPopupMenuCaller := ACaller;
    FDesignPopupMenu := TPopupMenu.Create(nil);
    try
      PopulateDesignPopupMenu;
      FDesignPopupMenu.Popup(APopupPoint.X, APopupPoint.Y);
      Application.ProcessMessages;
    finally
      FreeAndNil(FDesignPopupMenu);
    end;
  end;
end;

procedure TdxTileControlController.SetDesignItemSize(AItem: TdxTileControlItem; AMenuItemTag: Longint);
begin
  AItem.Size := TdxTileControlItemSize(AMenuItemTag);
end;

procedure TdxTileControlController.SwitchItemsProperty(Sender: TObject);
var
  AItem: TdxTileControlItem;
  I: Integer;
  AHasChanges: Boolean;
  AOldSize: TdxTileControlItemSize;
  AOldRowCount: Integer;
begin
  TileControl.BeginUpdate;
  AHasChanges := False;
  try
    for I := 0 to TileControl.Items.Count - 1 do
    begin
      AItem := TileControl.Items[I];
      if IsObjectSelected(AItem) then
      begin
        AOldSize := AItem.Size;
        AOldRowCount := AItem.RowCount;
        SetDesignItemSize(AItem, (Sender as TMenuItem).Tag);
        AHasChanges := AHasChanges or
          (AOldSize <> AItem.Size) or (AOldRowCount <> AItem.RowCount);
      end;
    end;
  finally
    TileControl.EndUpdate;
  end;
  if AHasChanges then
    SetDesignerModified(TileControl);
end;

//
procedure TdxTileControlController.AddAnimation(Animation: TdxAnimationTransition);
begin
  FAnimations.Add(Animation);
  Animation.Resume;
end;

procedure TdxTileControlController.AfterAnimation(Sender: TdxAnimationController);

  procedure InvalidateItem(AItem: TdxTileControlItem);
  begin
    if AItem.ViewInfo.Visible and AItem.ViewInfo.ContentAnimation.PositionChanged then
      AItem.Refresh;
  end;

var
  I: Integer;
begin
  for I := 0 to AnimationItems.Count - 1 do
    InvalidateItem(TdxTileControlItem(AnimationItems[I]));
end;

procedure TdxTileControlController.AnimateItemContent(AItem: TdxTileControlItem);
begin
  if (AnimationItems.IndexOf(AItem) < 0) and
    (AItem.CanGalleryAnimation or (AItem.ViewInfo.ViewData <> AItem.ActiveFrame.ViewData)) then
  begin
    AnimationItems.Add(AItem);
    AItem.CheckTimerEnabled;
    AItem.ViewInfo.ViewData.IsDirty := False;
    if AItem.CanGalleryAnimation then
      AItem.GalleryCellController.AddAnimation
    else
    begin
      AItem.ActiveFrame.ViewData.IsDirty := False;
      AddAnimation(TdxTileControlFramesAnimation.Create(AItem));
    end;
  end;
end;

procedure TdxTileControlController.BeforeAnimation(Sender: TdxAnimationController);
begin
end;

procedure TdxTileControlController.DestroyAnimation(Animation: TdxAnimationTransition);
begin
  FAnimations.Remove(Animation);
end;

function TdxTileControlController.EnabledGroupMoving: Boolean;
begin
  Result := (TileControl.OptionsBehavior.GroupMoving or TileControl.IsDesigning) and
    (TileControl.Groups.Count > 1);
end;

function TdxTileControlController.EnabledItemMoving: Boolean;
begin
  Result := (TileControl.OptionsBehavior.ItemMoving or TileControl.IsDesigning) and
    (TileControl.Items.Count > 1);
end;

procedure TdxTileControlController.FrameDestroyed(AFrame: TdxTileControlItemFrame);
var
  AItem: TdxTileControlItem;
begin
  AItem := AFrame.TileItem;
  if AnimationItems.IndexOf(AItem) < 0 then Exit;
  StopItemContentAnimation;
  if AItem.ViewInfo.ViewData = AFrame.ViewData then
    AItem.ViewInfo.ViewData := nil;
  AnimationItems.Remove(AItem);
  StartItemContentAnimation;
end;

function TdxTileControlController.HitAtGroup: Boolean;
begin
  Result := HitTest.HitAtGroup or HitTest.HitAtGroupCaption;
end;

function TdxTileControlController.HitAtItem: Boolean;
begin
  Result := HitTest.HitAtItem;
end;

function TdxTileControlController.HitAtItemOrGroup: Boolean;
begin
  Result := HitAtItem or HitAtGroup;
end;

function TdxTileControlController.GetCanGroupMoving: Boolean;
begin
  Result := EnabledGroupMoving;
  if Result then
    TileControl.DoGroupDragBegin(Result);
end;

function TdxTileControlController.GetCanItemMoving: Boolean;
begin
  Result := EnabledItemMoving;
  if Result then
    TileControl.DoItemDragBegin(Result);
end;

function TdxTileControlController.GetCheckedItems: TdxTileControlCheckedItems;
begin
  Result := TileControl.FCheckedItems;
end;

function TdxTileControlController.GetItemCheckMode: TdxTileControlItemCheckMode;
begin
  Result := TileControl.OptionsBehavior.ItemCheckMode;
end;

procedure TdxTileControlController.ImmediateAnimation(Animation: TdxAnimationTransition);
begin
  Animation.ImmediateAnimation;
end;

function TdxTileControlController.IsEnableDrawDragItemPlace: Boolean;
begin
  Result := ((TileControl.DraggedItem <> nil) or (ViewInfo.DragDropChanges <> nil)) and
    (((TileControl.DragAndDropObject as TdxTileControlDragDropItem).Place.Group <> nil) or
      not (TileControl.DragAndDropObject as TdxTileControlDragDropItem).FDragOverIsAccepted);
end;

function TdxTileControlController.IsHandScrolling(ACheckedKind: TScrollBarKind): Boolean;
begin
  Result := TileControl.GetScrollBar(ACheckedKind).Visible and
    (TileControl.DragAndDropState = ddsInProcess) and
    (TileControl.DragAndDropObject is TdxTileControlHandScroll);
end;

procedure TdxTileControlController.ItemChecked(AItem: TdxTileControlItem);
begin
  if AItem.Checked then
    CheckedItems.Add(AItem)
  else
    CheckedItems.Remove(AItem);

  TileControl.DoItemCheck(AItem);
  AItem.Invalidate;

  CheckActionBarsState(AItem);
end;

procedure TdxTileControlController.ItemDestroyed(AItem: TdxTileControlItem);
begin
  AnimationItems.Remove(AItem);
  CheckedItems.Remove(AItem);
  CheckActionBarsState(AItem);
  if FFocusedItem = AItem then
    FocusedItem := nil;
  if FHottrackedItem = AItem then
    FHottrackedItem := nil;
end;

procedure TdxTileControlController.RefreshItems;
begin
  HitTest.HitPoint := TileControl.ScreenToClient(GetMouseCursorPos);
  HottrackedItem := HitTest.Item;
end;

procedure TdxTileControlController.RestoreItemsBounds;
begin
  TerminateAnimations(False);
  TileControl.Items.ForEach(RestoreItemBounds, nil);
  TileControl.OptionsBehavior.GroupRenaming := False;
end;

procedure TdxTileControlController.ShowGroupCaptionEditors;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
begin
  ViewInfo.Calculate;
  TileControl.Invalidate;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    if AGroup.Visible and AGroup.Enabled then
      AGroup.ShowCaptionEditor;
  end;
  ImmediateAnimation(TdxTileControlInflateItemAnimation.Create(TileControl, nil, -1, 3));
end;

procedure TdxTileControlController.TerminateAnimations(ATerminateContentAnimations: Boolean);
var
  I: Integer;
  AAnimation: TdxAnimationTransition;
begin
  for I := Animations.Count - 1 downto 0 do
  begin
    AAnimation := TObject(Animations[I]) as TdxAnimationTransition;
    if ((AAnimation is TdxTileControlFramesAnimation) or
        (AAnimation is TdxTileControlGalleryCellAnimation)) <> ATerminateContentAnimations then Continue;

    if ATerminateContentAnimations then
      AAnimation.RefreshAnimatedObject;

    AAnimation.Terminate;
  end;
end;

function TdxTileControlController.GetHasAnimation_: Boolean;
begin
  Result := Animations.Count > 0;
end;

function TdxTileControlController.GetHitTest: TdxTileControlHitTest;
begin
  Result := FTileControl.HitTest;
end;

function TdxTileControlController.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := TileControl.ViewInfo;
end;

procedure TdxTileControlController.HideGroupCaptionEditors;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
begin
  TileControl.BeginUpdate;
  try
    for I := 0 to TileControl.Groups.Count - 1 do
    begin
      AGroup := TileControl.Groups[I];
      if AGroup.CaptionEditor <> nil then
        AGroup.HideCaptionEditor;
    end;
    RestoreItemsBounds;
  finally
    TileControl.EndUpdate;
  end;
end;

procedure TdxTileControlController.HideScrollButtons;
begin
  ViewInfo.HideScrollButtons;
end;

function TdxTileControlController.CanCreateSelectionHelper: Boolean;  // design selection
begin
  Result := TileControl.IsDesigning and not TileControl.IsDestroying and
    not TileControl.IsLoading and IsDesignHelperClassInitialized and TileControl.HandleAllocated;
end;

function TdxTileControlController.CanShowActionBarsOnRightClick: Boolean;
begin
  Result := not ContextMenuHandled and (not HitAtItem or (ItemCheckMode = tcicmNone));
end;

function TdxTileControlController.CanToggleItemCheckOnRightClick: Boolean;
begin
  Result := not ContextMenuHandled;
end;

procedure TdxTileControlController.CheckActionBarsState(ACheckingItem: TdxTileControlItem);
begin
  if not (TileControl.IsDesigning or TileControl.IsDestroying) then
  begin
    if CheckedItems.IndexOf(ACheckingItem) >= 0 then
      ShowActionBars(abvcrTileItemCheckedStateChange)
    else
      if CheckedItems.Count = 0 then
        HideActionBars(abvcrTileItemCheckedStateChange);
  end;
end;

procedure TdxTileControlController.CheckCreateDesignHelper;
begin
  if CanCreateSelectionHelper then
    FDesignHelper := CreateDesignHelper;
end;

function TdxTileControlController.CreateDesignHelper: TdxTileControlCustomDesignHelper;
begin
  Result := dxDesignHelperClass.Create(TileControl);
end;

procedure TdxTileControlController.DestroyDesignHelper;
begin
  FreeAndNil(FDesignHelper);
end;

function TdxTileControlController.IsDesignHelperClassInitialized: Boolean;
begin
  Result := dxDesignHelperClass <> nil;
end;

function TdxTileControlController.IsObjectSelected(AObject: TPersistent): Boolean;
begin
  Result := (DesignHelper <> nil) and DesignHelper.IsObjectSelected(AObject);
end;

procedure TdxTileControlController.SelectObject(AObject: TPersistent; AShift: TShiftState);
begin
  if DesignHelper <> nil then
    DesignHelper.Select(AObject, AShift);
end;

procedure TdxTileControlController.UncheckAllItems;
var
  I: Integer;
begin
  for I := TileControl.CheckedItemCount - 1 downto 0 do
    TileControl.CheckedItems[I].Checked := False;
end;

procedure TdxTileControlController.UnselectObject(AObject: TPersistent);
begin
  if DesignHelper <> nil then
    DesignHelper.UnselectObject(AObject);
end;

procedure TdxTileControlController.InternalMouseLeftDown(Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TdxTileControlController.InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TdxTileControlController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDownWasHandled := True;
  FRestoreItemsBoundsOnMouseUp := TileControl.OptionsBehavior.GroupRenaming;
  HitTest.Calculate(X, Y);
  FContextMenuHandled := False;
  if ItemsContentAnimationLockCount = 0 then
    StopItemContentAnimation;
  FMouseDownItem := HitTest.Item;
  if not TileControl.IsDesigning and (FMouseDownItem <> nil) and not FMouseDownItem.CanFocused then
    FMouseDownItem := nil;
  if TileControl.IsDesigning and TileControl.FRightButtonPressed then
    CheckDesignPopupMenu(Shift)
  else
    if Button = mbLeft then
    begin
      if HitAtGroup then
        SelectObject(HitTest.Group, Shift)
      else
        if HitAtItem then
        begin
          if not (TileControl.IsDesigning or IsTouchEvent or not TileControl.OptionsBehavior.ItemPressAnimation) and
             (FMouseDownItem <> nil) then
            TdxTileControlInflateItemAnimation.Create(TileControl, HitTest.Item, -1, 3).ImmediateAnimation;
          SelectObject(HitTest.Item, Shift);
        end;

      InternalMouseLeftDown(Shift, X, Y);

      if FMouseDownItem <> nil then
        FocusedItem := HitTest.Item;
      if HitTest.HitAtScrollButton then
        PressedScrollButton := HitTest.ScrollButton;
    end;
end;

procedure TdxTileControlController.MouseLeave;
begin
  HottrackedItem := nil;
  EndMouseTracking(Self);
end;

procedure TdxTileControlController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  APrevPoint: TPoint;
begin
  if TileControl.DragAndDropState = ddsNone then
  begin
    APrevPoint := HitTest.HitPoint;
    HitTest.Calculate(X, Y);
    if not cxPointIsEqual(APrevPoint, Point(X, Y)) then
      HottrackedItem := HitTest.Item;
    BeginMouseTracking(TileControl, TileControl.ClientBounds, Self);
  end;
end;

procedure TdxTileControlController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not MouseDownWasHandled then Exit;
  HitTest.Calculate(X, Y);
  if FRestoreItemsBoundsOnMouseUp or not TileControl.OptionsBehavior.GroupRenaming then
    RestoreItemsBounds;
  FRestoreItemsBoundsOnMouseUp := True;
  if HitAtItem and (HitTest.Item = FMouseDownItem) then
  begin
    if Button = mbLeft then
      HitTest.Item.Click;
    if (Button = mbRight) and CanToggleItemCheckOnRightClick then
      HitTest.Item.ToggleChecked;
  end;

  InternalMouseUp(Button, Shift, X, Y);

  if CheckedItems.Count = 0 then
  begin
    if (Button = mbRight) and CanShowActionBarsOnRightClick then
      ToggleActionBars(abvcrMouseRightClick)
    else
      HideActionBars(dxMouseButtonToActionBarVisibilityChangeReason[Button]);
  end;

  if Button = mbLeft  then
    ChangePressedScrollButtonState;
  if ItemsContentAnimationLockCount <> 0 then
    StartItemContentAnimation;
  MouseDownWasHandled := False;
  CheckHottrackItemAfterMouseUp;
end;

function TdxTileControlController.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(TileControl.ClientBounds, P);
end;

{ TdxTileControlHitTest }

constructor TdxTileControlHitTest.Create(AOwner: TdxCustomTileControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxTileControlHitTest.Calculate(const X, Y: Integer);
begin
  HitPoint := Point(X, Y);
end;

procedure TdxTileControlHitTest.Clear;
begin
  HitObject := nil;
  Flags := 0;
end;

function TdxTileControlHitTest.GetActiveViewInfo: TdxTileControlCustomViewInfo;
begin
  Result := TileControl.ViewInfo;
  if (TileControl.ActiveDetail <> nil) and (TileControl.ActiveDetail is TdxTileControlDetailSite) then
    Result := TdxTileControlDetailSite(TileControl.ActiveDetail).TitleViewInfo;
end;

function TdxTileControlHitTest.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (Flags and (1 shl AIndex)) <> 0;
end;

function TdxTileControlHitTest.GetController: TdxTileControlController;
begin
  Result := TileControl.Controller;
end;

function TdxTileControlHitTest.GetGroup: TdxTileControlGroup;
begin
  if (HitObject is TdxTileControlGroupViewInfo) or (HitObject is TdxTileControlGroupCaptionViewInfo) then
    if HitObject is TdxTileControlGroupViewInfo then
      Result := TdxTileControlGroupViewInfo(HitObject).Group
    else
      Result := TdxTileControlGroupCaptionViewInfo(HitObject).Group
  else
    if Item <> nil then
      Result := Item.Group
    else
      Result := nil;
end;

function TdxTileControlHitTest.GetItem: TdxTileControlItem;
begin
  if HitObject is TdxTileControlItemViewInfo then
    Result := TdxTileControlItemViewInfo(HitObject).Item
  else
    Result := nil;
end;

function TdxTileControlHitTest.GetPosValue(const AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := FHitPoint.X
  else
    Result := FHitPoint.Y
end;

function TdxTileControlHitTest.GetScrollButton: TdxTileControlScrollButtonViewInfo;
begin
  if HitObject is TdxTileControlScrollButtonViewInfo then
    Result := TdxTileControlScrollButtonViewInfo(HitObject)
  else
    Result := nil;
end;

procedure TdxTileControlHitTest.SetBitState(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    Flags := Flags or (1 shl AIndex)
  else
    Flags := Flags and not (1 shl AIndex);
end;

procedure TdxTileControlHitTest.SetHitObject(AValue: TObject);
begin
  if FHitObject <> AValue then
  begin
    Flags := 0;
    FHitObject := AValue;
  end;
end;

procedure TdxTileControlHitTest.Recalculate;
var
  APrevHitObject: TObject;
begin
  APrevHitObject := HitObject;
  Clear;
  ActiveViewInfo.GetHitTest(Self);
  if APrevHitObject <> HitObject then
    ActiveViewInfo.RefreshState;
end;

procedure TdxTileControlHitTest.SetHitPoint(const AValue: TPoint);
begin
  FHitPoint := AValue;
  Recalculate;
end;

procedure TdxTileControlHitTest.SetPosValue(const AIndex, AValue: Integer);
begin
  if AIndex = 0 then
    FHitPoint.X := AValue
  else
    FHitPoint.Y := AValue;
  Recalculate;
end;

{ TdxTileControlCellNavigator }

constructor TdxTileControlItemNavigator.Create(AController: TdxTileControlController);
begin
  FController := AController;
  FAssistPosition := TdxLayoutItemPosition.Create;
end;

destructor TdxTileControlItemNavigator.Destroy;
begin
  FreeAndNil(FAssistPosition);
  inherited Destroy;
end;

function TdxTileControlItemNavigator.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := Controller.TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlItemNavigator.GetGroups: TdxTileControlGroupCollection;
begin
  Result := Controller.TileControl.Groups;
end;

function TdxTileControlItemNavigator.CheckGroupOnExistenceOfItemsStartingFromRow(AGroup: TdxTileControlGroup;
  const ARow: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AGroup.ItemCount - 1 do
  begin
    Result := AGroup.Items[I].ViewInfo.Position.Row = ARow;
    if Result then
      Break;
  end;
end;

function TdxTileControlItemNavigator.GetCurrentItem: TdxTileControlItem;
begin
  Result := Controller.FocusedItem;
end;

function TdxTileControlItemNavigator.GetCurrentItemLastColumn: Integer;
begin
  Result := CurrentItem.ViewInfo.GetLastOccupiedColumn(CurrentItem.ViewInfo.Position.Column);
end;

function TdxTileControlItemNavigator.GetCurrentItemLastRow: Integer;
begin
  Result := CurrentItem.ViewInfo.GetLastOccupiedRow(CurrentItem.ViewInfo.Position.Row);
end;

function TdxTileControlItemNavigator.GetFirstItem: TdxTileControlItem;
var
  I, J: Integer;
  AItem: TdxTileControlItem;
  AGroup: TdxTileControlGroup;
begin
  Result := CurrentItem;
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.IsAvailableForNavigation then
      for J := 0 to AGroup.ItemCount - 1 do
      begin
        AItem := AGroup.Items[J];
        if AItem.Visible then
        begin
          Result := AItem;
          Break;
        end;
      end;
    if Result <> CurrentItem then Break;
  end;
end;

function TdxTileControlItemNavigator.GetFirstItemInRow: TdxTileControlItem;
var
  I, AEndColumn: Integer;
  AGroup, ACurrentGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := CurrentItem;
  ACurrentGroup := CurrentItem.Group;
  if GroupLayout = glVertical then
  begin
    AItem := GetItemByForwardSearchOnColumns(ACurrentGroup, 0, CurrentColumn - 1);
    if AItem <> nil then
      Result := AItem;
  end
  else
    for I := 0 to ACurrentGroup.Index do
    begin
      AGroup := Groups[I];
      if AGroup.IsAvailableForNavigation then
      begin
        if I = ACurrentGroup.Index then
          AEndColumn := CurrentColumn - 1
        else
          AEndColumn := AGroup.RealColumnCount - 1;
        AItem := GetItemByForwardSearchOnColumns(AGroup, 0, AEndColumn);
        if AItem <> nil then
        begin
          Result := AItem;
          Break;
        end;
      end;
    end;
end;

function TdxTileControlItemNavigator.GetItemByBackSearchOnColumns(AGroup: TdxTileControlGroup;
  const AStartColumn, AEndColumn: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
  AColumn: Integer;
begin
  Result := nil;
  for AColumn := AStartColumn downto AEndColumn do
  begin
    FAssistPosition.Row := CurrentRow;
    FAssistPosition.Column := AColumn;
    AItem := AGroup.ViewInfo.GetOccupiedItem(FAssistPosition);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetItemByBackSearchOnRows(AGroup: TdxTileControlGroup;
  const AStartRow, AEndRow: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
  ARow: Integer;
begin
  Result := nil;
  for ARow := AStartRow downto AEndRow do
  begin
    FAssistPosition.Row := ARow;
    FAssistPosition.Column := CurrentColumn;
    AItem := AGroup.ViewInfo.GetOccupiedItem(FAssistPosition);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetItemByForwardSearchOnColumns(AGroup: TdxTileControlGroup;
  const AStartColumn, AEndColumn: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
  AColumn: Integer;
begin
  Result := nil;
  for AColumn := AStartColumn to AEndColumn do
  begin
    FAssistPosition.Row := CurrentRow;
    FAssistPosition.Column := AColumn;
    AItem := AGroup.ViewInfo.GetOccupiedItem(FAssistPosition);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetItemByForwardSearchOnRows(AGroup: TdxTileControlGroup;
  const AStartRow, AEndRow: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
  ARow: Integer;
begin
  Result := nil;
  for ARow := AStartRow to AEndRow do
  begin
    FAssistPosition.Row := ARow;
    FAssistPosition.Column := CurrentColumn;
    AItem := AGroup.ViewInfo.GetOccupiedItem(FAssistPosition);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetLastItem: TdxTileControlItem;
var
  I, J: Integer;
  AItem: TdxTileControlItem;
  AGroup: TdxTileControlGroup;
begin
  Result := CurrentItem;
  for I := Groups.Count - 1 downto 0 do
  begin
    AGroup := Groups[I];
    if AGroup.IsAvailableForNavigation then
      for J := AGroup.ItemCount - 1 downto 0 do
      begin
        AItem := AGroup.Items[J];
        if AItem.Visible then
        begin
          Result := AItem;
          Break;
        end;
      end;
    if Result <> CurrentItem then Break;
  end;
end;

function TdxTileControlItemNavigator.GetLastItemInRow: TdxTileControlItem;
var
  I, AEndColumn: Integer;
  AGroup, ACurrentGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := CurrentItem;
  ACurrentGroup := CurrentItem.Group;
  if GroupLayout = glVertical then
  begin
    AItem := GetItemByBackSearchOnColumns(ACurrentGroup, CurrentItemLastColumn + 1, ACurrentGroup.ViewInfo.ColumnCount - 1);
    if AItem <> nil then
      Result := AItem;
  end
  else
    for I := Groups.Count - 1 downto ACurrentGroup.Index do
    begin
      AGroup := Groups[I];
      if AGroup.IsAvailableForNavigation then
      begin
        if I = ACurrentGroup.Index then
          AEndColumn := CurrentItemLastColumn + 1
        else
          AEndColumn := 0;
        AItem := GetItemByBackSearchOnColumns(AGroup, AGroup.RealColumnCount - 1, AEndColumn);
        if AItem <> nil then
        begin
          Result := AItem;
          Break;
        end;
      end;
    end;
end;

function TdxTileControlItemNavigator.GetNextItemInColumnAboveTheCurrentWhenHorizontalLayout(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
begin
  Result := nil;
  AItem := GetItemByForwardSearchOnRows(Groups[ACurrentGroupIndex], 0, CurrentRow - 1);
  if AItem <> nil then
    Result := AItem;
end;

function TdxTileControlItemNavigator.GetNextItemInColumnAboveTheCurrentWhenVerticalLayout(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I, AEndRow: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := 0 to ACurrentGroupIndex do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I = ACurrentGroupIndex then
      AEndRow := CurrentRow - 1
    else
      AEndRow := AGroup.RealRowCount - 1;
    AItem := GetItemByForwardSearchOnRows(AGroup, 0, AEndRow);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetNextItemInColumnBelowTheCurrent(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
  AStartRow: Integer;
begin
  Result := nil;
  for I := ACurrentGroupIndex to Groups.Count - 1 do
  begin
    if (GroupLayout = glHorizontal) and (I <> ACurrentGroupIndex) then Break;
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I = ACurrentGroupIndex then
      AStartRow := CurrentItemLastRow + 1
    else
      AStartRow := 0;
    AItem := GetItemByForwardSearchOnRows(AGroup, AStartRow, AGroup.RealRowCount - 1);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetNextItemInColumn: TdxTileControlItem;
var
  ACurrentGroupIndex: Integer;
  AItem: TdxTileControlItem;
begin
  Result := CurrentItem;
  ACurrentGroupIndex := CurrentItem.Group.Index;
  AItem := GetNextItemInColumnBelowTheCurrent(ACurrentGroupIndex);
  if AItem <> nil then
    Result := AItem;
  if (Result = CurrentItem) and FFocusItemOnCycle then
  begin
    if GroupLayout = glHorizontal then
      AItem := GetNextItemInColumnAboveTheCurrentWhenHorizontalLayout(ACurrentGroupIndex)
    else
      AItem := GetNextItemInColumnAboveTheCurrentWhenVerticalLayout(ACurrentGroupIndex);
    if AItem <> nil then
      Result := AItem;
  end;
  if Result <> CurrentItem then
    CurrentRow := Result.ViewInfo.Position.Row;
end;

function TdxTileControlItemNavigator.GetNextItemInNextRowWhenHorizontalLayout(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I: Integer;
  AEndColumn: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := 0 to ACurrentGroupIndex do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I = ACurrentGroupIndex then
      AEndColumn := CurrentColumn - 1
    else
      AEndColumn := AGroup.RealColumnCount - 1;
    AItem := GetItemByForwardSearchOnColumns(AGroup, 0, AEndColumn);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetNextItemInNextRowWhenVerticalLayout(
  const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I, ARow: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := ACurrentGroupIndex to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    for ARow := 0 to AGroup.RealRowCount - 1 do
    begin
      if (ARow < AStartRow) and (I = ACurrentGroupIndex) then Continue;
      AItem := GetItemByForwardSearchOnColumns(AGroup, 0, AGroup.RealColumnCount - 1);
      if AItem <> nil then
      begin
        Result := AItem;
        Break;
      end;
    end;
    if Result <> nil then Break;
  end;
end;

function TdxTileControlItemNavigator.GetNextItemInRow: TdxTileControlItem;
var
  ACurrentGroupIndex: Integer;
  AItem: TdxTileControlItem;
  ARow: Integer;
begin
  Result := CurrentItem;
  ACurrentGroupIndex := CurrentItem.Group.Index;
  AItem := GetNextItemInRowToRightHand(CurrentItemLastColumn + 1, ACurrentGroupIndex);
  if AItem <> nil then
    Result := AItem;
  if (Result = CurrentItem) and FFocusItemOnCycle then
  begin
    ARow := GetNextRow(1, ACurrentGroupIndex);
    if ARow > -1 then
    begin
      CurrentRow := ARow;
      if GroupLayout = glHorizontal then
        AItem := GetNextItemInNextRowWhenHorizontalLayout(ACurrentGroupIndex)
      else
        AItem := GetNextItemInNextRowWhenVerticalLayout(CurrentRow, ACurrentGroupIndex);
      if AItem <> nil then
        Result := AItem;
    end;
  end;
  if Result <> CurrentItem then
    CurrentColumn := Result.ViewInfo.Position.Column;
end;

function TdxTileControlItemNavigator.GetNextItemInRowToRightHand(AStartColumn, ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I: Integer;
  AEndColumn: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := ACurrentGroupIndex to Groups.Count - 1 do
  begin
    if (GroupLayout = glVertical) and (I <> ACurrentGroupIndex) then Break;
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I <> ACurrentGroupIndex then
      AStartColumn := 0;
    AEndColumn := AGroup.RealColumnCount - 1;
    AItem := GetItemByForwardSearchOnColumns(AGroup, AStartColumn, AEndColumn);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetPrevItemInColumn: TdxTileControlItem;
var
  ACurrentGroupIndex: Integer;
  AItem: TdxTileControlItem;
begin
  Result := CurrentItem;
  ACurrentGroupIndex := CurrentItem.Group.Index;
  AItem := GetPrevItemInColumnAboveTheCurrent(ACurrentGroupIndex);
  if AItem <> nil then
    Result := AItem;
  if (Result = CurrentItem) and FFocusItemOnCycle then
  begin
    if GroupLayout = glHorizontal then
      AItem := GetPrevItemInColumnBelowTheCurrentWhenHorizontalLayout(CurrentItem.Group.RealRowCount - 1, ACurrentGroupIndex)
    else
      AItem := GetPrevItemInColumnBelowTheCurrentWhenVerticalLayout(ACurrentGroupIndex);
    if AItem <> nil then
      Result := AItem;
  end;
  if Result <> CurrentItem then
    CurrentRow := Result.ViewInfo.Position.Row;
end;

function TdxTileControlItemNavigator.GetPrevItemInColumnBelowTheCurrentWhenHorizontalLayout(
  const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  AItem: TdxTileControlItem;
  AGroup: TdxTileControlGroup;
begin
  Result := nil;
  AGroup := Groups[ACurrentGroupIndex];
  AItem := GetItemByBackSearchOnRows(AGroup, AStartRow, CurrentRow + 1);
  if AItem <> nil then
    Result := AItem;
end;

function TdxTileControlItemNavigator.GetPrevItemInColumnBelowTheCurrentWhenVerticalLayout(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I, AEndRow: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := Groups.Count - 1 downto ACurrentGroupIndex do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I = ACurrentGroupIndex then
      AEndRow := CurrentItemLastRow + 1
    else
      AEndRow := 0;
    AItem := GetItemByBackSearchOnRows(AGroup, AGroup.RealRowCount - 1, AEndRow);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetPrevItemInColumnAboveTheCurrent(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I, AStartRow: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := ACurrentGroupIndex downto 0 do
  begin
    if (GroupLayout = glHorizontal) and (I <> ACurrentGroupIndex) then Break;
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I = ACurrentGroupIndex then
      AStartRow := CurrentRow - 1
    else
      AStartRow := AGroup.RealColumnCount - 1;
    AItem := GetItemByBackSearchOnRows(AGroup, AStartRow, 0);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetPrevItemInPrevRowWhenHorizontalLayout(
  const ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I: Integer;
  AStartColumn, AEndColumn: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := Groups.Count - 1 downto ACurrentGroupIndex do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    AStartColumn := AGroup.RealColumnCount - 1;
    if I = ACurrentGroupIndex then
      AEndColumn := CurrentItemLastColumn + 1
    else
      AEndColumn := 0;
    AItem := GetItemByBackSearchOnColumns(AGroup, AStartColumn, AEndColumn);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetPrevItemInPrevRowWhenVerticalLayout(
  const AStartRow, ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I, ARow: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := ACurrentGroupIndex downto 0 do
  begin
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    for ARow := AGroup.RealRowCount - 1 downto 0 do
    begin
      if (ARow > AStartRow) and (I = ACurrentGroupIndex) then Continue;
      AItem := GetItemByBackSearchOnColumns(AGroup, AGroup.RealColumnCount - 1, 0);
      if AItem <> nil then
      begin
        Result := AItem;
        Break;
      end;
    end;
    if Result <> nil then Break;
  end;
end;

function TdxTileControlItemNavigator.GetPrevItemInRow: TdxTileControlItem;
var
  ACurrentGroupIndex: Integer;
  AItem: TdxTileControlItem;
  ARow: Integer;
begin
  Result := CurrentItem;
  ACurrentGroupIndex := CurrentItem.Group.Index;
  AItem := GetPrevItemInRowToLeftHand(CurrentItem.ViewInfo.Position.Column - 1, ACurrentGroupIndex);
  if AItem <> nil then
    Result := AItem;
  if (Result = CurrentItem) and FFocusItemOnCycle then
  begin
    ARow := GetNextRow(-1, ACurrentGroupIndex);
    if ARow > -1 then
    begin
      CurrentRow := ARow;
      if GroupLayout = glHorizontal then
        AItem := GetPrevItemInPrevRowWhenHorizontalLayout(ACurrentGroupIndex)
      else
        AItem := GetPrevItemInPrevRowWhenVerticalLayout(CurrentRow, ACurrentGroupIndex);
      if AItem <> nil then
        Result := AItem;
    end;
  end;
  if Result <> CurrentItem then
    CurrentColumn := Result.ViewInfo.Position.Column;
end;


function TdxTileControlItemNavigator.GetPrevItemInRowToLeftHand(AStartColumn, ACurrentGroupIndex: Integer): TdxTileControlItem;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
  AItem: TdxTileControlItem;
begin
  Result := nil;
  for I := ACurrentGroupIndex downto 0 do
  begin
    if (GroupLayout = glVertical) and (I <> ACurrentGroupIndex) then Break;
    AGroup := Groups[I];
    if not AGroup.IsAvailableForNavigation then Continue;
    if I <> ACurrentGroupIndex then
      AStartColumn := AGroup.RealColumnCount - 1;
    AItem := GetItemByBackSearchOnColumns(AGroup, AStartColumn, 0);
    if AItem <> nil then
    begin
      Result := AItem;
      Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetNextRow(const AIncrement: Integer; var AGroupIndex: Integer): Integer;
begin
  if GroupLayout = glHorizontal then
    Result := GetNextRowWhenHorizontalLayout(AIncrement)
  else
    Result := GetNextRowWhenVerticalLayout(AIncrement, AGroupIndex);
end;

function TdxTileControlItemNavigator.GetNextRowWhenHorizontalLayout(const AIncrement: Integer): Integer;
var
  ASkipRow: Boolean;
  I: Integer;
begin
  ASkipRow := True;
  Result := CurrentRow;
  while ASkipRow do
  begin
    Inc(Result, AIncrement);
    if (Result < 0) or (Result >= Controller.ViewInfo.RowCount * 2) then
    begin
      Result := -1;
      Break;
    end;
    for I := 0 to Groups.Count - 1 do
    begin
      if Groups[I].IsAvailableForNavigation then
        ASkipRow := not CheckGroupOnExistenceOfItemsStartingFromRow(Groups[I], Result);
      if not ASkipRow then
        Break;
    end;
  end;
end;

function TdxTileControlItemNavigator.GetNextRowWhenVerticalLayout(
  const AIncrement: Integer; var AGroupIndex: Integer): Integer;
var
  ASkipRow: Boolean;
  AGroup: TdxTileControlGroup;
begin
  AGroupIndex := CurrentItem.GroupIndex;
  Result := CurrentRow + AIncrement;
  while (AGroupIndex >= 0) and (AGroupIndex <= Groups.Count - 1) do
  begin
    AGroup := Groups[AGroupIndex];
    ASkipRow := True;
    if AGroup.IsAvailableForNavigation then
    begin
      if AGroupIndex <> CurrentItem.GroupIndex then
        if AIncrement > 0 then
          Result := 0
        else
          Result := AGroup.RealRowCount - 1;
      while ASkipRow do
      begin
        if (Result < 0) or (Result >= AGroup.RealRowCount) then
        begin
          Result := -1;
          Break;
        end;
        ASkipRow := not CheckGroupOnExistenceOfItemsStartingFromRow(AGroup, Result);
        if ASkipRow then
          Inc(Result, AIncrement);
      end;
    end;
    if not ASkipRow then
      Break;
    Inc(AGroupIndex, Sign(AIncrement));
  end;
end;

procedure TdxTileControlItemNavigator.InitializeCurrentPosition;
begin
  if FIsInitializingCurrentPositionDisabled then Exit;
  if CurrentItem <> nil then
  begin
    CurrentRow := CurrentItem.ViewInfo.Position.Row;
    CurrentColumn := CurrentItem.ViewInfo.Position.Column;
  end
  else
  begin
    CurrentRow := 0;
    CurrentColumn := 0;
  end;
end;

function TdxTileControlItemNavigator.IsAcceptedKey(Key: Word): Boolean;
begin
  Result := Key in [VK_END, VK_HOME, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN];
end;

procedure TdxTileControlItemNavigator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FFocusItemOnCycle := Controller.TileControl.OptionsBehavior.FocusItemOnCycle;
  if CurrentItem = nil then
    SaveCurrentNavigationItem;
  if Controller.ViewInfo.UseRightToLeftAlignment then
    Key := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key);
  if CurrentItem <> nil then
  case Key of
    VK_HOME:
      if [ssCtrl] * Shift <> [] then
        CurrentItem := GetFirstItem
      else
        CurrentItem := GetFirstItemInRow;
    VK_LEFT:
      CurrentItem := GetPrevItemInRow;
    VK_UP:
      CurrentItem := GetPrevItemInColumn;
    VK_RIGHT:
      CurrentItem := GetNextItemInRow;
    VK_DOWN:
      CurrentItem := GetNextItemInColumn;
  else
    if [ssCtrl] * Shift <> [] then
      CurrentItem := GetLastItem
    else
      CurrentItem := GetLastItemInRow;
  end;
  Key := 0;
  if CurrentItem <> nil then
    CurrentItem.MakeVisible;
end;

procedure TdxTileControlItemNavigator.SaveCurrentNavigationItem;
var
  I, J:  Integer;
  AGroup: TdxTileControlGroup;
begin
  if Controller.TileControl.IsDesigning then Exit;
  if CurrentItem = nil then
    for I := 0 to Groups.Count - 1 do
    begin
      AGroup := Groups[I];
      if AGroup.Visible and AGroup.Enabled then
        for J := 0 to AGroup.ItemCount - 1 do
          if AGroup.Items[J].Visible then
          begin
            CurrentItem := AGroup.Items[J];
            Break;
          end;
      if CurrentItem <> nil then Break;
    end;
end;

procedure TdxTileControlItemNavigator.SetCurrentItem(AValue: TdxTileControlItem);
begin
  FIsInitializingCurrentPositionDisabled := True;
  try
    Controller.FocusedItem := AValue;
  finally
    FIsInitializingCurrentPositionDisabled := False;
  end;
end;

{ TdxTileControlOptionsBehavior }

constructor TdxTileControlOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGroupMoving := False;
  FGroupRenaming := False;
  FGroupCaptionHint := GetDefaultGroupCaptionHint;
  FFocusItemOnCycle := True;
  FItemMoving := GetDefaultItemMoving;
  FBackgroundScrollSync := dxTileControlDefaultScrollSync;
  FItemHotTrackHighlightColor :=GetDefaultItemHotTrackHighlightColor;
  FItemFocusMode := GetDefaultItemFocusMode;
  FItemHotTrackMode := GetDefaultItemHotTrackMode;
  FItemOuterFrameColor := clDefault;
  FItemPressAnimation := GetDefaultItemPressAnimation;
  FScrollMode := GetDefaultScrollMode;
  FItemCheckMode := GetDefaultItemCheckMode;
end;

procedure TdxTileControlOptionsBehavior.Assign(ASource: TPersistent);
var
  ABehavior: TdxTileControlOptionsBehavior;
begin
  if ASource is TdxTileControlOptionsBehavior then
  begin
    ABehavior := TdxTileControlOptionsBehavior(ASource);
    FBackgroundScrollSync := ABehavior.BackgroundScrollSync;
    FFocusItemOnCycle := ABehavior.FocusItemOnCycle;
    FGroupCaptionHint := ABehavior.GroupCaptionHint;
    FGroupMoving := ABehavior.GroupMoving;
    FGroupRenaming := ABehavior.GroupRenaming;
    FHideFocusOnItemHotTrack := ABehavior.HideFocusOnItemHotTrack;
    FItemCheckMode := ABehavior.ItemCheckMode;
    FItemFocusMode := ABehavior.ItemFocusMode;
    FItemHotTrackHighlightColor := ABehavior.ItemHotTrackHighlightColor;
    FItemHotTrackMode := ABehavior.ItemHotTrackMode;
    FItemMoving := ABehavior.ItemMoving;
    FItemOuterFrameColor := ABehavior.ItemOuterFrameColor;
    FScrollMode := ABehavior.ScrollMode;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlOptionsBehavior.Changed;
begin
  TileControl.LayoutChanged;
end;

procedure TdxTileControlOptionsBehavior.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ItemHotTrack', ReadItemHotTrack, nil, True);
end;

function TdxTileControlOptionsBehavior.GetDefaultItemCheckMode: TdxTileControlItemCheckMode;
begin
  Result := tcicmMultiple;
end;

function TdxTileControlOptionsBehavior.GetDefaultItemFocusMode: TdxTileControlItemFocusMode;
begin
  Result := tcifmDefault;
end;

function TdxTileControlOptionsBehavior.GetDefaultItemHotTrackHighlightColor: TdxAlphaColor;
begin
  Result := dxTileControlItemHotTrackHighlightDefaultColor;
end;

function TdxTileControlOptionsBehavior.GetDefaultItemHotTrackMode: TdxTileControlItemHotTrackMode;
begin
  Result := tcihtmFrame;
end;

function TdxTileControlOptionsBehavior.GetDefaultItemMoving: Boolean;
begin
  Result := True;
end;

function TdxTileControlOptionsBehavior.GetDefaultItemPressAnimation: Boolean;
begin
  Result := True;
end;

function TdxTileControlOptionsBehavior.GetDefaultGroupCaptionHint: string;
begin
  Result := cxGetResourceString(@dxTileControlDefaultGroupCaptionHint);
end;

function TdxTileControlOptionsBehavior.GetDefaultScrollMode: TdxTileControlScrollMode;
begin
  Result := smScrollbars;
end;

function TdxTileControlOptionsBehavior.IsItemCheckModeStored: Boolean;
begin
  Result := FItemCheckMode <> GetDefaultItemCheckMode;
end;

function TdxTileControlOptionsBehavior.IsItemFocusModeStored: Boolean;
begin
  Result := FItemFocusMode <> GetDefaultItemFocusMode;
end;

function TdxTileControlOptionsBehavior.IsItemHotTrackHighlightColorStored: Boolean;
begin
  Result := FItemHotTrackHighlightColor <> GetDefaultItemHotTrackHighlightColor;
end;

function TdxTileControlOptionsBehavior.IsItemHotTrackModeStored: Boolean;
begin
  Result := FItemHotTrackMode <> GetDefaultItemHotTrackMode;
end;

function TdxTileControlOptionsBehavior.IsItemMovingStored: Boolean;
begin
  Result := FItemMoving <> GetDefaultItemMoving;
end;

function TdxTileControlOptionsBehavior.IsItemPressAnimationStored: Boolean;
begin
  Result := FItemPressAnimation <> GetDefaultItemPressAnimation;
end;

function TdxTileControlOptionsBehavior.IsScrollModeStored: Boolean;
begin
  Result := FScrollMode <> GetDefaultScrollMode;
end;

function TdxTileControlOptionsBehavior.GetTileControl: TdxCustomTileControl;
begin
  Result := GetOwner as TdxCustomTileControl;
end;

function TdxTileControlOptionsBehavior.IsGroupCaptionHintStored: Boolean;
begin
  Result := FGroupCaptionHint <> GetDefaultGroupCaptionHint;
end;

procedure TdxTileControlOptionsBehavior.ReadItemHotTrack(AReader: TReader);
const
  AMode: array [Boolean] of TdxTileControlItemHotTrackMode = (tcihtmNone, tcihtmFrame);
begin
  FItemHotTrackMode := AMode[AReader.ReadBoolean];
end;

procedure TdxTileControlOptionsBehavior.SetBackgroundScrollSync(AValue: Byte);
begin
  AValue := Min(100, Max(0, AValue));
  if AValue <> FBackgroundScrollSync then
  begin
    FBackgroundScrollSync := AValue;
    TileControl.Invalidate;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetGroupRenaming(AValue: Boolean);
begin
  if FGroupRenaming <> AValue then
  begin
    FGroupRenaming := AValue;
    if not TileControl.IsDesigning then
      if FGroupRenaming then
        TileControl.Controller.ShowGroupCaptionEditors
      else
        TileControl.Controller.HideGroupCaptionEditors;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetItemCheckMode(AValue: TdxTileControlItemCheckMode);
begin
  if FItemCheckMode <> AValue then
  begin
    FItemCheckMode := AValue;
    TileControl.Controller.UncheckAllItems;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetItemFocusMode(AValue: TdxTileControlItemFocusMode);
begin
  if FItemFocusMode <> AValue then
  begin
    FItemFocusMode := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetItemHotTrackHighlightColor(AValue: TdxAlphaColor);
begin
  if FItemHotTrackHighlightColor <> AValue then
  begin
    FItemHotTrackHighlightColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetItemHotTrackMode(AValue: TdxTileControlItemHotTrackMode);
begin
  if FItemHotTrackMode <> AValue then
  begin
    FItemHotTrackMode := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetItemMoving(AValue: Boolean);
begin
  FItemMoving := AValue;
end;

procedure TdxTileControlOptionsBehavior.SetItemPressAnimation(AValue: Boolean);
begin
  FItemPressAnimation := AValue;
end;

procedure TdxTileControlOptionsBehavior.SetItemOuterFrameColor(AValue: TColor);
begin
  if FItemOuterFrameColor <> AValue then
  begin
    FItemOuterFrameColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsBehavior.SetScrollMode(AValue: TdxTileControlScrollMode);
begin
  if FScrollMode <> AValue then
  begin
    FScrollMode := AValue;
    Changed;
  end;
end;

{ TdxTileControlAutoScrollingObject }

function TdxTileControlAutoScrollingObject.GetControl: TcxControl;
begin
  Result := TdxTileControlDragDropCustomObject(Owner).TileControl;
end;

function TdxTileControlAutoScrollingObject.GetHasScrollBar: Boolean;
begin
  if TileControl.OptionsBehavior.ScrollMode = smScrollbars then
    Result := inherited GetHasScrollBar
  else
    Result := TileControl.OptionsBehavior.ScrollMode = smScrollButtons;
end;

procedure TdxTileControlAutoScrollingObject.GetScrollBarParams(var AMin, AMax, APos: Integer);
var
  AViewInfo: TdxTileControlViewInfo;
begin
  AViewInfo := TdxCustomTileControl(GetControl).ViewInfo;
  AMin := 0;
  if Kind = sbHorizontal then
  begin
    APos := AViewInfo.LeftScrollPos;
    AMax := AViewInfo.ContentWidth - AViewInfo.HorzScrollPage;
  end
  else
  begin
    APos := AViewInfo.TopScrollPos;
    AMax := AViewInfo.ContentHeight - AViewInfo.VertScrollPage;
  end;
end;

function TdxTileControlAutoScrollingObject.GetTileControl: TdxCustomTileControl;
begin
  Result := TdxCustomTileControl(GetControl);
end;

procedure TdxTileControlAutoScrollingObject.Scroll(AKind: TScrollBarKind;
  ACode: TScrollCode; var APosition: Integer);
const
  ALayouts: array [TScrollBarKind] of TdxTileControlGroupLayout = (glHorizontal, glVertical);
var
  P: TPoint;
  AAccepted: Boolean;
begin
  if ACode <> scEndScroll then
    TileControl.Scroll(AKind, ACode, APosition)
  else
    if TileControl.OptionsView.GroupLayout = ALayouts[AKind] then
    begin
      P := TileControl.ScreenToClient(GetMouseCursorPos);
      TileControl.HitTest.HitPoint := P;
      TileControl.DragAndDropObject.PrevMousePos := cxNullPoint;
      TdxTileControlDragDropCustomObject(TileControl.DragAndDropObject).DragAndDrop(P, AAccepted);
    end;
end;

{ TdxTileControlGroupCaptionInplaceEdit }

constructor TdxTileControlGroupCaptionInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TileControl;
  Hide;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  TabStop := False;
  AutoSize := False;
  ParentCtl3D := False;
  Ctl3D := False;
  Enabled := Group.Enabled;
  Text := Caption.Text;
  OnExit := CheckEmptyOnExit;
  SetupFont;
  SetupStyles;
  SetupBounds;
  SetupProperties;
  SetupClearButton;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.ClearEditor(Sender: TObject; AButtonIndex: Integer);
begin
  Clear;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.ChangeScaleEx(M: Integer; D: Integer; IsDPIChanged: Boolean);
begin
  inherited ChangeScaleEx(M, D, IsDPIChanged);
  SetupClearButton;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.CheckEmptyOnExit(Sender: TObject);
var
  AText: string;
begin
  AText :=  Trim(VarToStr(EditValue));
  Caption.Text := AText;
  if AText = '' then
    Clear
  else
    Text := AText;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_LEFT;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.DrawClearButtonGlyph;
var
  ACenter: TPoint;
  AHorn: Integer;
  AImage: TdxSmartImage;
  AImageCanvas: TdxGPCanvas;
  APrevSmoothingMode: TdxGPSmoothingMode;
  R: TRect;
begin
  R := cxRectSetSize(cxNullRect, ClearButton.Width, Height);
  AImage := TdxSmartImage.CreateSize(R);
  try
    AImageCanvas := AImage.CreateCanvas;
    try
      APrevSmoothingMode := AImageCanvas.SmoothingMode;
      try
        AImageCanvas.SmoothingMode := smAntiAlias;
        ACenter := cxRectCenter(R);
        AHorn := cxRectWidth(R) div 4;
        AImageCanvas.Line(ACenter.X - AHorn, ACenter.Y - AHorn, ACenter.X + AHorn, ACenter.Y + AHorn, $FF000000,
          ScaleFactor.Apply(2));
        AImageCanvas.Line(ACenter.X - AHorn, ACenter.Y + AHorn, ACenter.X + AHorn, ACenter.Y - AHorn, $FF000000,
          ScaleFactor.Apply(2));
      finally
        AImageCanvas.SmoothingMode := APrevSmoothingMode;
      end;
    finally
      AImageCanvas.Free;
    end;
    ClearButton.Glyph.Assign(AImage);
  finally
    AImage.Free;
  end;
end;

function TdxTileControlGroupCaptionInplaceEdit.GetCaptionTextBounds: TRect;
begin
  Result := Caption.ViewInfo.TextBounds;
end;

function TdxTileControlGroupCaptionInplaceEdit.GetCaption: TdxTileControlGroupCaption;
begin
  Result := Group.Caption;
end;

function TdxTileControlGroupCaptionInplaceEdit.GetClearButton: TcxEditButton;
begin
  Result := Properties.Buttons[0];
end;

function TdxTileControlGroupCaptionInplaceEdit.GetGroup: TdxTileControlGroup;
begin
  Result := Owner as TdxTileControlGroup;
end;

function TdxTileControlGroupCaptionInplaceEdit.GetTextHint;
begin
  Result := TileControl.OptionsBehavior.GroupCaptionHint;
end;

function TdxTileControlGroupCaptionInplaceEdit.GetTileControl: TdxCustomTileControl;
begin
  Result := Group.TileControl;
end;

function TdxTileControlGroupCaptionInplaceEdit.IsUseSkins: Boolean;
begin
  Result := Caption.ViewInfo.Painter.UseSkins;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure EndEdit;
  begin
    if Key = VK_ESCAPE then
      Text := Caption.Text
    else
      Caption.Text := Trim(Text);
    TileControl.SetFocus;
    Key := 0;
    PostMessage(TileControl.Handle, DXM_TILECONTROL_HIDEGROUPCAPTIONEDITORS, 0, 0);
  end;

begin
  if Key in [VK_RETURN, VK_ESCAPE] then
    EndEdit;
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SetupBounds;
var
  ACaptionTextBounds, ABoundsRect: TRect;
begin
  ACaptionTextBounds := GetCaptionTextBounds;
  FIniCaptionPos := ACaptionTextBounds.TopLeft;
  ABoundsRect := cxRectInflate(ACaptionTextBounds, TileControl.ScaleFactor.Apply(cxTextSpace));
  FIniBoundsPos := ABoundsRect.TopLeft;
  SetBounds(ABoundsRect.Left, ABoundsRect.Top, cxRectWidth(ABoundsRect), cxRectHeight(ABoundsRect));
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SetupClearButton;
var
  AWidth: Integer;
begin
  AWidth := 7 * Height div 10;
  dxAdjustToTouchableSize(AWidth, ScaleFactor);
  ClearButton.Width := AWidth;
  ClearButton.Kind := bkGlyph;
  DrawClearButtonGlyph;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SetupFont;
begin
  ParentFont := False;
  Style.Font := Caption.Font;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SetupProperties;
begin
  Properties.Alignment.Vert := taVCenter;
  Properties.UseNullString := True;
  Properties.Nullstring := TextHint;
  Properties.OnButtonClick := ClearEditor;
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SetupStyles;
begin
  Style.LookAndFeel.MasterLookAndFeel := TileControl.LookAndFeel;
  if IsUseSkins then Exit;
  Style.LookAndFeel.Kind := lfUltraFlat;
  Style.LookAndFeel.NativeStyle := False;
  Style.LookAndFeel.AssignedValues := [lfvKind,lfvNativeStyle];
  Style.ButtonStyle := btsUltraFlat;
  Style.Color := dxGetLighterColor(
    cxGetActualColor(TileControl.Style.GradientBeginColor, LookAndFeelPainter.DefaultContentColor), 90);
  Style.BorderStyle := ebsNone;
  Style.ButtonTransparency := ebtInactive;

  StyleFocused.BorderStyle := Style.BorderStyle;
  StyleFocused.ButtonStyle := btsUltraFlat;
  StyleFocused.Color := clWindow;
  StyleFocused.TextColor := clWindowText;

  StyleHot.BorderStyle := Style.BorderStyle;
  StyleHot.ButtonStyle := btsUltraFlat;
  StyleHot.Color := dxGetLighterColor(cxGetActualColor(Style.Color, LookAndFeelPainter.DefaultContentColor), 90);
end;

procedure TdxTileControlGroupCaptionInplaceEdit.SynchronizePos;

  function IsNeedPosChange(const ADelta, AActualPos, AIniPos: Integer): Boolean;
  begin
    Result := (ADelta <> 0) or ((ADelta = 0) and (AActualPos <> AIniPos));
  end;

var
  DX, DY: Integer;
  AActualCaptionPos: TPoint;
begin
  AActualCaptionPos := GetCaptionTextBounds.TopLeft;
  DX := AActualCaptionPos.X - IniCaptionPos.X;
  DY := AActualCaptionPos.Y - IniCaptionPos.Y;
  if IsNeedPosChange(DX, Left, IniBoundsPos.X) or IsNeedPosChange(DY, Top, IniBoundsPos.Y) then
    SetBounds(IniBoundsPos.X + DX, IniBoundsPos.Y + DY, Width, Height);
end;

procedure TdxTileControlGroupCaptionInplaceEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

{ TdxTileControlTitle }

constructor TdxTileControlTitle.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Color := clDefault;
  FFont.Name := DefaultFontName;
  FFont.Height := dxGetFontHeightForDefaultDPI(dxTileControlDefaultTitleFontSize);
  FFont.OnChange := FontChanged;
  FTabsFontSize := dxTileControlDefaultTabFontSize;
  FTabsActiveTextColor := clDefault;
  FTabsDisabledTextColor := clDefault;
  FTabsHotTextColor := clDefault;
  FTabsTextColor := clDefault;
  FTextAlignVert := vaBottom;
  FGlyphAlignHorz := taRightJustify;
  FGlyphAlignVert := vaBottom;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FIndentHorz := dxTileItemObjectDefaultIndent;
  FIndentVert := dxTileItemObjectDefaultIndent;
  FColor := clDefault;
  FViewInfo := CreateViewInfo;
  FViewInfo.FTitle := Self;
end;

destructor TdxTileControlTitle.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FFont);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxTileControlTitle.Assign(ASource: TPersistent);
var
  ATitle: TdxTileControlTitle;
begin
  if ASource is TdxTileControlTitle then
  begin
    ATitle := TdxTileControlTitle(ASource);
    FColor := ATitle.Color;
    FIndentHorz := ATitle.IndentHorz;
    FIndentVert := ATitle.IndentVert;
    FText := ATitle.Text;
    FTextAlignHorz := ATitle.TextAlignHorz;
    FTextAlignVert := ATitle.TextAlignVert;
    FWordWrap := ATitle.WordWrap;
    FGlyphAlignHorz  := ATitle.GlyphAlignHorz;
    FGlyphAlignVert := ATitle.GlyphAlignVert;
    Glyph := ATitle.Glyph;
    Font := ATitle.Font;
    FTabsActiveTextColor := ATitle.TabsActiveTextColor;
    FTabsDisabledTextColor := ATitle.TabsDisabledTextColor;
    FTabsFontSize := ATitle.TabsFontSize;
    FTabsHotTextColor := ATitle.TabsHotTextColor;
    FTabsTextColor := ATitle.TabsTextColor;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlTitle.Changed;
begin
  TileControl.TitleChanged;
end;

procedure TdxTileControlTitle.ChangeScale(M, D: Integer);
var
  APrevFontChanged: Boolean;
begin
  APrevFontChanged := IsFontStored;
  Font.Height := MulDiv(Font.Height, M, D);
  FFontChanged := APrevFontChanged;
  FIndentHorz := MulDiv(FIndentHorz, M, D);
  FIndentVert := MulDiv(FIndentVert, M, D);
  FTabsFontSize := MulDiv(FTabsFontSize, M, D);
end;

function TdxTileControlTitle.CreateViewInfo: TdxTileControlTitleViewInfo;
begin
  Result := TdxTileControlTitleViewInfo.Create;
end;

procedure TdxTileControlTitle.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
  Changed;
end;

procedure TdxTileControlTitle.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

function TdxTileControlTitle.IsChanged: Boolean;
begin
  Result := IsFontStored or (TextAlignHorz <> taLeftJustify) or (TextAlignVert <>  vaBottom) or
    (Color <> clDefault) or (IndentHorz <> dxTileItemObjectDefaultIndent) or
    (IndentVert <> dxTileItemObjectDefaultIndent) or (Text <> '') or WordWrap or
    not Glyph.Empty or (GlyphAlignHorz <> taRightJustify) or (GlyphAlignVert <> vaBottom) or
    (TabsFontSize <> dxTileControlDefaultTabFontSize) or (TabsActiveTextColor <> clDefault) or
    (TabsDisabledTextColor <> clDefault) or (TabsHotTextColor <> clDefault) or (TabsTextColor <> clDefault);
end;

function TdxTileControlTitle.GetTileControl: TdxCustomTileControl;
begin
  Result := GetOwner as TdxCustomTileControl;
end;

function TdxTileControlTitle.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxTileControlTitle.SetColor(AValue: TColor);
begin
  if AValue <> FColor then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue)
end;

procedure TdxTileControlTitle.SetGlyph(AValue: TdxSmartGlyph);
begin
  if AValue <> nil then
    FGlyph.Assign(AValue)
  else
    FGlyph.Clear;
  Changed;
end;

procedure TdxTileControlTitle.SetGlyphAlignHorz(AValue: TAlignment);
begin
  if AValue <> FGlyphAlignHorz then
  begin
    FGlyphAlignHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetGlyphAlignVert(AValue: TcxAlignmentVert);
begin
  if AValue <> FGlyphAlignVert then
  begin
    FGlyphAlignVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetIndentHorz(AValue: Integer);
begin
  if AValue <> FIndentHorz then
  begin
    FIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetIndentVert(AValue: Integer);
begin
  if AValue <> FIndentVert then
  begin
    FIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTabsFontSize(AValue: Integer);
begin
  if FTabsFontSize <> AValue then
  begin
    FTabsFontSize := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTabsActiveTextColor(AValue: TColor);
begin
  if FTabsActiveTextColor <> AValue then
  begin
    FTabsActiveTextColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTabsDisabledTextColor(AValue: TColor);
begin
  if FTabsDisabledTextColor <> AValue then
  begin
    FTabsDisabledTextColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTabsHotTextColor(AValue: TColor);
begin
  if FTabsHotTextColor <> AValue then
  begin
    FTabsHotTextColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTabsTextColor(AValue: TColor);
begin
  if FTabsTextColor <> AValue then
  begin
    FTabsTextColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetText(AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTextAlignHorz(AValue: TAlignment);
begin
  if AValue <> FTextAlignHorz then
  begin
    FTextAlignHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetTextAlignVert(AValue: TcxAlignmentVert);
begin
  if AValue <> FTextAlignVert then
  begin
    FTextAlignVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlTitle.SetWordWrap(AValue: Boolean);
begin
  if AValue <> FWordWrap then
  begin
    FWordWrap := AValue;
    Changed;
  end;
end;

{ TdxTileControlActionBars }

constructor TdxTileControlActionBars.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FColor := clDefault;
  FFont := TFont.Create;
  FFont.Color := clDefault;
  FFont.Height := dxGetFontHeightForDefaultDPI(FFont.Size);
  FFont.OnChange := FontChanged;
  FItems := CreateItems;
  FItems.OnChange := ItemsChangeHandler;
  FIndentHorz := dxTileControlDefaultActionBarsIndentHorz;
  FIndentVert := dxTileControlDefaultActionBarsIndentVert;
  FItemIndent := dxTileControlDefaultActionBarsItemIndent;
end;

destructor TdxTileControlActionBars.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxTileControlActionBars.Assign(Source: TPersistent);
var
  ABars: TdxTileControlActionBars;
begin
  if Source is TdxTileControlActionBars then
  begin
    ABars := TdxTileControlActionBars(Source);
    Font := ABars.Font;
    Color := ABars.Color;
    Items := ABars.Items;
    IndentHorz := ABars.IndentHorz;
    IndentVert := ABars.IndentVert;
    ItemIndent := ABars.ItemIndent;
  end;
end;

procedure TdxTileControlActionBars.Changed;
begin
  TileControl.RefreshActionBars;
end;

procedure TdxTileControlActionBars.ChangeScale(M, D: Integer);
var
  APrevFontChanged: Boolean;
begin
  APrevFontChanged := IsFontStored;
  Font.Height := MulDiv(Font.Height, M, D);
  FFontChanged := APrevFontChanged;
  FIndentHorz := MulDiv(FIndentHorz, M, D);
  FIndentVert := MulDiv(FIndentVert, M, D);
  FItemIndent := MulDiv(FItemIndent, M, D);
end;

function TdxTileControlActionBars.CreateItems: TdxTileControlActionBarItems;
begin
  Result := TdxTileControlActionBarItems.Create(TileControl, TdxTileControlActionBarItem);
end;

procedure TdxTileControlActionBars.FontChanged(Sender: TObject);
begin
  FFontChanged := True;
  Changed;
end;

function TdxTileControlActionBars.IsFontStored: Boolean;
begin
  Result := FFontChanged;
end;

procedure TdxTileControlActionBars.ItemsChangeHandler(Sender: TObject;
  AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  Changed;
end;

function TdxTileControlActionBars.GetTileControl: TdxCustomTileControl;
begin
  Result := Owner as TdxCustomTileControl;
end;

procedure TdxTileControlActionBars.SetColor(AValue: TColor);
begin
  if AValue <> FColor then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxTileControlActionBars.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxTileControlActionBars.SetIndentHorz(AValue: Integer);
begin
  if AValue <> FIndentHorz then
  begin
    FIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlActionBars.SetIndentVert(AValue: Integer);
begin
  if AValue <> FIndentVert then
  begin
    FIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlActionBars.SetItemIndent(AValue: Integer);
begin
  if AValue <> FItemIndent then
  begin
    FItemIndent := AValue;
    Changed;
  end;
end;

procedure TdxTileControlActionBars.SetItems(AValue: TdxTileControlActionBarItems);
begin
  FItems.Assign(AValue);
end;

{ TdxTileControlOptionsView }

constructor TdxTileControlOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFixedIndentHorz := GetDefaultFixedIndentHorz;
  FFixedIndentVert := GetDefaultFixedIndentVert;
  FGroupIndent := GetDefaultGroupIndent;
  FIndentHorz := GetDefaultIndentHorz;
  FIndentVert := GetDefaultIndentVert;
  FItemHeight := GetDefaultItemHeight;
  FItemIndent := GetDefaultItemIndent;
  FItemWidth := GetDefaultItemWidth;
  FGroupLayout := glHorizontal;
  FGroupBlockMaxColumnCount := GetDefaultGroupBlockMaxColumnCount;
  FGroupMaxRowCount := GetDefaultGroupMaxRowCount;
end;

procedure TdxTileControlOptionsView.Assign(ASource: TPersistent);
var
  AView: TdxTileControlOptionsView;
begin
  if ASource is TdxTileControlOptionsView then
  begin
    AView := TdxTileControlOptionsView(ASource);
    FCenterContentHorz := AView.CenterContentHorz;
    FCenterContentVert := AView.CenterContentVert;
    FFixedIndentHorz := AView.FixedIndentHorz;
    FFixedIndentVert := AView.FixedIndentVert;
    FGroupBlockMaxColumnCount := AView.GroupBlockMaxColumnCount;
    FGroupIndent := AView.GroupIndent;
    FGroupLayout := AView.GroupLayout;
    FGroupMaxRowCount := AView.GroupMaxRowCount;
    FIndentHorz := AView.IndentHorz;
    FIndentVert := AView.IndentVert;
    FItemHeight := AView.ItemHeight;
    FItemIndent := AView.ItemIndent;
    FItemWidth := AView.ItemWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlOptionsView.Changed;
begin
  TileControl.LayoutChanged;
end;

procedure TdxTileControlOptionsView.ChangeScale(M, D: Integer);
begin
  FGroupIndent := MulDiv(FGroupIndent, M, D);
  FIndentHorz := MulDiv(FIndentHorz, M, D);
  FIndentVert := MulDiv(FIndentVert, M, D);
  FItemHeight := MulDiv(FItemHeight, M, D);
  FItemIndent := MulDiv(FItemIndent, M, D);
  FItemWidth := MulDiv(FItemWidth, M, D);
end;

procedure TdxTileControlOptionsView.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ItemSize', ReadItemSize, nil, True);
end;

function TdxTileControlOptionsView.GetDefaultFixedIndentHorz: Boolean;
begin
  Result := False;
end;

function TdxTileControlOptionsView.GetDefaultFixedIndentVert: Boolean;
begin
  Result := False;
end;

function TdxTileControlOptionsView.GetDefaultGroupBlockMaxColumnCount: Integer;
begin
  Result := dxTileControlDefaultGroupBlockColumnCount;
end;

function TdxTileControlOptionsView.GetDefaultGroupIndent: Integer;
begin
  Result := dxTileControlDefaultGroupIndent;
end;

function TdxTileControlOptionsView.GetDefaultGroupMaxRowCount: Integer;
begin
  Result := dxTileControlDefaultRowCount;
end;

function TdxTileControlOptionsView.GetDefaultIndentHorz: Integer;
begin
  Result := dxTileControlDefaultIndentHorz;
end;

function TdxTileControlOptionsView.GetDefaultIndentVert: Integer;
begin
  Result := dxTileControlDefaultIndentVert;
end;

function TdxTileControlOptionsView.GetDefaultItemHeight: Integer;
begin
  Result := dxTileControlDefaultItemSize;
end;

function TdxTileControlOptionsView.GetDefaultItemIndent: Integer;
begin
  Result := dxTileControlDefaultItemIndent;
end;

function TdxTileControlOptionsView.GetDefaultItemWidth: Integer;
begin
  Result := dxTileControlDefaultItemSize;
end;

function TdxTileControlOptionsView.GetMaxItemRowCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to TileControl.Items.Count - 1 do
    if TileControl.Items[I].RowCount > Result then
      Result := TileControl.Items[I].RowCount;
end;

function TdxTileControlOptionsView.GetTileControl: TdxCustomTileControl;
begin
  Result := Owner as TdxCustomTileControl;
end;

function TdxTileControlOptionsView.IsAllowableWidth(const AWidth: Integer): Boolean;
begin
  Result := AWidth > 0;
end;

function TdxTileControlOptionsView.IsFixedIndentHorzStored: Boolean;
begin
  Result := FFixedIndentHorz <> GetDefaultFixedIndentHorz;
end;

function TdxTileControlOptionsView.IsFixedIndentVertStored: Boolean;
begin
  Result := FFixedIndentVert <> GetDefaultFixedIndentVert;
end;

function TdxTileControlOptionsView.IsGroupBlockMaxColumnCountStored: Boolean;
begin
  Result := FGroupBlockMaxColumnCount <> GetDefaultGroupBlockMaxColumnCount;
end;

function TdxTileControlOptionsView.IsGroupIndentStored: Boolean;
begin
  Result := FGroupIndent <> GetDefaultGroupIndent;
end;

function TdxTileControlOptionsView.IsGroupMaxRowCountStored: Boolean;
begin
  Result := FGroupMaxRowCount <> GetDefaultGroupMaxRowCount;
end;

function TdxTileControlOptionsView.IsIndentHorzStored: Boolean;
begin
  Result := FIndentHorz <> GetDefaultIndentHorz;
end;

function TdxTileControlOptionsView.IsIndentVertStored: Boolean;
begin
  Result := FIndentVert <> GetDefaultIndentVert;
end;

function TdxTileControlOptionsView.IsItemHeightStored: Boolean;
begin
  Result := FItemHeight <> GetDefaultItemHeight;
end;

function TdxTileControlOptionsView.IsItemIndentStored: Boolean;
begin
  Result := FItemIndent <> GetDefaultItemIndent;
end;

function TdxTileControlOptionsView.IsItemWidthStored: Boolean;
begin
  Result := FItemWidth <> GetDefaultItemWidth;
end;

procedure TdxTileControlOptionsView.ReadItemSize(AReader: TReader);
var
  AValue: Integer;
begin
  AValue := AReader.ReadInteger;
  FItemHeight := AValue;
  FItemWidth := AValue;
end;

procedure TdxTileControlOptionsView.SetCenterContentHorz(AValue: Boolean);
begin
  if FCenterContentHorz <> AValue then
  begin
    FCenterContentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetCenterContentVert(AValue: Boolean);
begin
  if FCenterContentVert <> AValue then
  begin
    FCenterContentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetFixedIndentHorz(AValue: Boolean);
begin
  if FFixedIndentHorz <> AValue then
  begin
    FFixedIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetFixedIndentVert(AValue: Boolean);
begin
  if FFixedIndentVert <> AValue then
  begin
    FFixedIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetGroupIndent(AValue: Integer);
begin
  if FGroupIndent <> AValue then
  begin
    FGroupIndent := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetIndentHorz(AValue: Integer);
begin
  if FIndentHorz <> AValue then
  begin
    FIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetIndentVert(AValue: Integer);
begin
  if FIndentVert <> AValue then
  begin
    FIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetGroupBlockMaxColumnCount(AValue: Integer);
begin
  AValue := Max(1, AValue);
  if FGroupBlockMaxColumnCount <> AValue then
  begin
    FGroupBlockMaxColumnCount := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetItemHeight(AValue: Integer);
begin
  if FItemHeight <> AValue then
  begin
    FItemHeight := AValue;
    TileControl.AddChanges([tccItems]);
    TileControl.LayoutChanged;
  end;
end;

procedure TdxTileControlOptionsView.SetItemIndent(AValue: Integer);
begin
  if FItemIndent <> AValue then
  begin
    FItemIndent := AValue;
    TileControl.AddChanges([tccItems]);
    TileControl.LayoutChanged;
  end;
end;

procedure TdxTileControlOptionsView.SetItemWidth(AValue: Integer);
begin
  if (FItemWidth <> AValue) and IsAllowableWidth(AValue) then
  begin
    FItemWidth := AValue;
    TileControl.AddChanges([tccItems]);
    TileControl.LayoutChanged;
  end;
end;

procedure TdxTileControlOptionsView.SetGroupLayout(AValue: TdxTileControlGroupLayout);
begin
  if FGroupLayout <> AValue then
  begin
    FGroupLayout := AValue;
    Changed;
  end;
end;

procedure TdxTileControlOptionsView.SetGroupMaxRowCount(AValue: Integer);
begin
  AValue := Max(AValue, GetMaxItemRowCount);
  if FGroupMaxRowCount <> AValue then
  begin
    FGroupMaxRowCount := AValue;
    Changed;
  end;
end;

{ TdxTileControlViewInfo }

constructor TdxTileControlViewInfo.Create(AOwner: TdxCustomTileControl);
begin
  inherited Create;
  FCalculationCount := 0;
  FTileControl := AOwner;
  FCells := TdxTileControlCells.Create(False);
  FVirtualGroups := TcxObjectList.Create(False);
  CreateScrollButtonsViewInfo;
end;

destructor TdxTileControlViewInfo.Destroy;
begin
  DestroyScrollButtonsViewInfo;
  FreeAndNil(FVirtualGroups);
  FreeAndNil(FCells);
  FreeAndNil(FBackground);
  Inherited Destroy;
end;

procedure TdxTileControlViewInfo.Calculate;
begin
  FBounds := TileControl.ClientBounds;
  DoCalculate;
end;

function TdxTileControlViewInfo.GetBackgroundPart(const ARect: TRect): TcxBitmap;
begin
  Result := TcxBitmap.CreateSize(ARect);
  cxBitBlt(Result.Canvas.Handle, Background.Canvas.Handle, Result.ClientRect, ARect.TopLeft, SRCCOPY);
end;

function TdxTileControlViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := Cells.CalculateHitTest(AHitTest);
end;

procedure TdxTileControlViewInfo.AddGroup(AGroup: TdxTileControlGroup);
begin
  AddVirtualGroupBefore(AGroup);
  AGroup.ViewInfo.Recalculate;
  FCells.Add(AGroup.ViewInfo);
  FCells.Add(AGroup.Caption.ViewInfo);
  AddVirtualGroupAfter(AGroup);
end;

procedure TdxTileControlViewInfo.AddGroups;
var
  I: Integer;
begin
  MeasureGroupsCaptionsHeights;
  VirtualGroups.Clear;
  for I := 0 to GroupsCount - 1 do
    AddGroup(Groups.Items[I]);
end;

procedure TdxTileControlViewInfo.AddItem(AItem: TdxTileControlItem);
begin
  if not AItem.ActuallyVisible then Exit;
  AItem.ViewInfo.Calculate;
  FCells.Add(AItem.ViewInfo);
end;

procedure TdxTileControlViewInfo.AddItems;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    AddItem(Items[I]);
end;

procedure TdxTileControlViewInfo.AddScrollButtons;
var
  I: TcxArrowDirection;
begin
  if TileControl.OptionsBehavior.ScrollMode <> smScrollButtons then Exit;
  for I := Low(TcxArrowDirection) to High(TcxArrowDirection) do
  begin
    FScrollButtonViewInfo[I].Recalculate;
    FCells.Add(FScrollButtonViewInfo[I]);
  end;
end;

procedure TdxTileControlViewInfo.AddVirtualGroupAfter(AGroup: TdxTileControlGroup);
begin
  if AGroup.Visible then
  begin
    AGroup.VirtualGroupAfter.Recalculate;
    FCells.Add(AGroup.VirtualGroupAfter);
    FVirtualGroups.Add(AGroup.VirtualGroupAfter);
  end;
end;

procedure TdxTileControlViewInfo.AddVirtualGroupBefore(AGroup: TdxTileControlGroup);
begin
  if AGroup.Visible and AGroup.IsMostLeft then
  begin
    AGroup.VirtualGroupBefore.Recalculate;
    FCells.Add(AGroup.VirtualGroupBefore);
    FVirtualGroups.Add(AGroup.VirtualGroupBefore);
  end;
end;

procedure TdxTileControlViewInfo.CalculateClientBounds;
begin
  FClientBounds := TileControl.ClientBounds;
end;

procedure TdxTileControlViewInfo.CalculateDerivedItemSizes;
begin
  FItemHalfIndent := TileControl.OptionsView.ItemIndent div 2;
  FItemSmallHeight := (TileControl.OptionsView.ItemHeight - TileControl.OptionsView.ItemIndent) div 2;
  FItemSmallWidth := (TileControl.OptionsView.ItemWidth - TileControl.OptionsView.ItemIndent) div 2;
  FItemLargeWidth := 2 * TileControl.OptionsView.ItemWidth + TileControl.OptionsView.ItemIndent;
end;

function TdxTileControlViewInfo.CalculateTilesArea: TRect;
begin
  Result := TilesZone;
  if TileControl.OptionsView.FixedIndentHorz then
    Result := cxRectInflate(Result, -TileControl.OptionsView.IndentHorz, 0);
  if TileControl.OptionsView.FixedIndentVert then
    Result := cxRectInflate(Result, 0, -TileControl.OptionsView.IndentVert);
end;

function TdxTileControlViewInfo.CalculateTilesZone: TRect;
begin
  Result := FClientBounds;
  Result.Top := Result.Top + TitleHeight;
end;

procedure TdxTileControlViewInfo.CalculateTitle;
begin
  Title.ViewInfo.Recalculate;
  if Title.ViewInfo.Visible then
    Cells.Add(Title.ViewInfo);
end;

procedure TdxTileControlViewInfo.CalculateRowCount;
var
  AUsefulHeight: Integer;
  ARegularItemHeight, AItemIndent: Integer;
begin
  if (GroupLayout = glVertical) or TileControl.AutoSize then
    FRowCount := TileControl.OptionsView.GroupMaxRowCount
  else
  begin
    AUsefulHeight := cxRectHeight(TilesArea) - GetGroupsCaptionsMaxHeight - 1;
    if not TileControl.OptionsView.FixedIndentVert then
      Dec(AUsefulHeight, 2 * TileControl.OptionsView.IndentVert);
    AItemIndent := TileControl.OptionsView.ItemIndent;
    ARegularItemHeight := TileControl.OptionsView.ItemHeight + AItemIndent;
    FRowCount := AUsefulHeight div ARegularItemHeight;
    Inc(FRowCount, Ord(AUsefulHeight > (FRowCount + 1) * ARegularItemHeight - AItemIndent));
    FRowCount := Min(FRowCount, TileControl.OptionsView.GroupMaxRowCount);
    Inc(FRowCount, Ord(FRowCount = 0));
  end;
end;

procedure TdxTileControlViewInfo.CheckBiDiModeAlignment;
var
  I: Integer;
begin
  FUseRightToLeftAlignment := TileControl.UseRightToLeftAlignment;
  FUseRightToLeftScrollBar := TileControl.UseRightToLeftScrollBar;
  if UseRightToLeftAlignment then
  begin
    FTilesZone := TdxRightToLeftLayoutConverter.ConvertRect(FTilesZone, ClientBounds);
    FTilesArea := TdxRightToLeftLayoutConverter.ConvertRect(FTilesArea, ClientBounds);
    for I := 0 to FCells.Count - 1 do
      FCells[I].DoRightToLeftConversion(ClientBounds);
  end;
end;

procedure TdxTileControlViewInfo.Clear;
begin
  Cells.Clear;
  FUseRightToLeftAlignment := False;
  FUseRightToLeftScrollBar := False;
end;

procedure TdxTileControlViewInfo.ClearGroupsOrigin;
begin
  FFixedGroupsOrigin := False;
end;

procedure TdxTileControlViewInfo.CreateScrollButtonsViewInfo;
var
  I: TcxArrowDirection;
begin
  for I := Low(TcxArrowDirection) to High(TcxArrowDirection) do
    FScrollButtonViewInfo[I] := GetScrollButtonViewInfoClass.Create(TileControl, I);
end;

function TdxTileControlViewInfo.GetScrollButtonViewInfoClass: TdxTileControlScrollButtonViewInfoClass;
begin
  Result := TdxTileControlScrollButtonViewInfo;
end;

procedure TdxTileControlViewInfo.DestroyScrollButtonsViewInfo;
var
  I: TcxArrowDirection;
begin
  for I := Low(TcxArrowDirection) to High(TcxArrowDirection) do
    FreeAndNil(FScrollButtonViewInfo[I]);
end;

procedure TdxTileControlViewInfo.DoCalculate;
begin
  Inc(FCalculationCount);
  Clear;
  CalculateTitle;
  CalculateDerivedItemSizes;
  CalculateClientBounds;
  FTilesZone := CalculateTilesZone;
  FTilesArea := CalculateTilesArea;
  if (TileControl.ActiveDetail <> nil) and not TileControl.ForceCalculate then Exit;
  CalculateRowCount;
  AddGroups;
  if IsScrollBarsParametersWasChanged then
    DoCalculate
  else
  begin
    DoCenterContent;
    AddItems;
    AddScrollButtons;
    if not TileControl.UpdatingScrollBars then
      TileControl.UpdateScrollBars;
  end;
  Dec(FCalculationCount);
  if FCalculationCount = 0 then
    CheckBiDiModeAlignment;
end;

procedure TdxTileControlViewInfo.DoCenterContent;
begin
  DoCenterContentHorz;
  DoCenterContentVert;
end;

procedure TdxTileControlViewInfo.DoCenterContentHorz;
var
  AShift, AAreaRight: Integer;
begin
  AAreaRight := TilesArea.Right;
  if IsContentNotCentredHorz or (VirtualGroups.Count = 0) or (FContentRight > AAreaRight) then Exit;
  AShift := (AAreaRight - FContentRight) div 2;
  if GroupLayout = glHorizontal then
    DoCenterContentHorzAtHorizontalGroupLayout(AShift, AAreaRight)
  else
    DoCenterContentHorzAtVerticalGroupLayout(AShift);
end;

procedure TdxTileControlViewInfo.DoCenterContentHorzAtHorizontalGroupLayout(const AShift, AAreaRight: Integer);
var
  I: Integer;
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo;
begin
  AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[0]);
  AVirtualGroup.ExpandBounds(AShift, 0);
  for I := 0 to Cells.Count - 1 do
    if Cells[I] <> AVirtualGroup then
      Cells[I].Scroll(AShift, 0);
  AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[VirtualGroups.Count - 1]);
  AVirtualGroup.ExpandBounds(AAreaRight - AVirtualGroup.Bounds.Right, 0);
end;

procedure TdxTileControlViewInfo.DoCenterContentHorzAtVerticalGroupLayout(const AShift: Integer);
var
  I: Integer;
begin
  for I := 0 to Cells.Count - 1 do
    if not(Cells[I] is TdxTileControlVirtualGroupViewInfo) then
      Cells[I].Scroll(AShift, 0);
end;

procedure TdxTileControlViewInfo.DoCenterContentVert;
var
  AShift, AControlBottom: Integer;
  R: TRect;
begin
  AControlBottom := TilesArea.Bottom;
  if IsContentNotCentredVert or (VirtualGroups.Count = 0) or (FContentBottom > AControlBottom) then Exit;
  R := GetVisibleGroupsBounds;
  AShift := (AControlBottom - R.Bottom - R.Top + TilesArea.Top) div 2;
  if AShift > 0 then
    if GroupLayout = glHorizontal then
      DoCenterContentVertAtHorizontalGroupLayout(AShift)
    else
      DoCenterContentVertAtVerticalGroupLayout(AShift, AControlBottom);
end;

procedure TdxTileControlViewInfo.DoCenterContentVertAtHorizontalGroupLayout(const AShift: Integer);

  function IsCellIsVirtualGroupViewInfo(ACell: TdxTileControlCustomCellViewInfo): Boolean;
  begin
    Result := ACell is TdxTileControlVirtualGroupViewInfo;
  end;

var
  I: Integer;
begin
  for I := 0 to Cells.Count - 1 do
    if not IsCellIsVirtualGroupViewInfo(Cells[I]) then
      Cells[I].Scroll(0, AShift);
  for I := 0 to Cells.Count - 1 do
    if IsCellIsVirtualGroupViewInfo(Cells[I]) then
      TdxTileControlVirtualGroupViewInfo(Cells[I]).RecalculateDrawingBounds;
end;

procedure TdxTileControlViewInfo.DoCenterContentVertAtVerticalGroupLayout(const AShift, AAreaBottom: Integer);
var
  I: Integer;
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo;
begin
  AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[0]);
  AVirtualGroup.ExpandBounds(0, AShift);
  for I := 0 to Cells.Count - 1 do
    if Cells[I] <> AVirtualGroup then
      Cells[I].Scroll(0, AShift);
  AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[VirtualGroups.Count - 1]);
  AVirtualGroup.ExpandBounds(0, AAreaBottom - AVirtualGroup.Bounds.Bottom);
end;

function TdxTileControlViewInfo.GetContentBounds: TRect;
var
  I: Integer;
  AVirtualGroup: TdxTileControlVirtualGroupViewInfo;
  AGroup: TdxTileControlGroup;
  ABounds: TRect;
  ARealContentRight, AWidth, AHeight: Integer;
begin
  FRealContentHeight := 0;
  FRealContentWidth := 0;
  Result.Left := MaxInt;
  Result.Right := 0;
  Result.Top := MaxInt;
  Result.Bottom := 0;
  for I := 0 to VirtualGroups.Count - 1 do
  begin
    AVirtualGroup := TdxTileControlVirtualGroupViewInfo(VirtualGroups[I]);
    AGroup := AVirtualGroup.Group;
    ABounds := AVirtualGroup.Bounds;

    ARealContentRight := Max(Result.Right, AGroup.ViewInfo.ItemsRight + TileControl.OptionsView.IndentHorz);
    Result.Left := Min(Result.Left, ABounds.Left);
    Result.Bottom := Max(Result.Bottom, AGroup.ViewInfo.ItemsBottom + TileControl.OptionsView.IndentVert);
    if GroupLayout = glHorizontal then
    begin
      Result.Top := Min(Result.Top, AGroup.Caption.Bounds.Top - TileControl.OptionsView.IndentVert);
      if TileControl.IsItemDragged and (AVirtualGroup = AGroup.VirtualGroupAfter) then
        ARealContentRight := Max(ARealContentRight, AGroup.Bounds.Right + AVirtualGroup.BasisWidth);
    end
    else
    begin
      Result.Top := Min(Result.Top, ABounds.Top);
      if TileControl.IsItemDragged then
        Result.Bottom := Max(Result.Bottom, ABounds.Bottom);
    end;
    Result.Right := Max(Result.Right, ARealContentRight);
    if TileControl.IsDesigning then
    begin
      if TileControl.OptionsView.FixedIndentHorz then
        Dec(Result.Right, ScaleFactor.Apply(1));
      if TileControl.OptionsView.FixedIndentVert then
        Dec(Result.Bottom, ScaleFactor.Apply(1));
    end;
  end;
  AWidth := Result.Right - Result.Left;
  if TileControl.OptionsView.FixedIndentHorz and (AWidth > 0) and (AWidth > TileControl.OptionsView.IndentHorz) then
    Result := cxRectInflate(Result, -TileControl.OptionsView.IndentHorz, 0);
  AHeight := Result.Bottom - Result.Top;
  if TileControl.OptionsView.FixedIndentVert and (AHeight > 0) and (AHeight > TileControl.OptionsView.IndentVert) then
    Result := cxRectInflate(Result, 0, -TileControl.OptionsView.IndentVert);
  FRealContentHeight := Max(0, cxRectHeight(Result));
  FRealContentWidth := Max(0, cxRectWidth(Result));
end;

function TdxTileControlViewInfo.GetDetailSiteArea: TRect;
begin
  Result := TileControl.ClientRect;
end;

function TdxTileControlViewInfo.GetGroupsCaptionsMaxHeight: Integer;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
begin
  Result := 0;
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.Visible then
      Result := Max(Result, AGroup.Caption.Height);
  end;
end;

function TdxTileControlViewInfo.GetTitleHeight: Integer;
begin
  Result := 0;
  if Title.ViewInfo.Visible then
    Result := Title.ViewInfo.Bounds.Bottom;
end;

function TdxTileControlViewInfo.GetVertScrollPage: Integer;
begin
  Result := cxRectHeight(TilesArea);
end;

function TdxTileControlViewInfo.GetVisibleGroupsBounds: TRect;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
  ABounds: TRect;
begin
  Result := cxNullRect;
  Result.Left := MaxInt;
  Result.Top := MaxInt;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    if AGroup.Visible then
      begin
        ABounds := AGroup.Bounds;
        Result.Left := Min(Result.Left, ABounds.Left);
        Result.Top := Min(Result.Top, AGroup.Caption.Bounds.Top);
        Result.Right := Max(Result.Right, ABounds.Right);
        Result.Bottom := Max(Result.Bottom, ABounds.Bottom);
      end;
  end;
end;

function TdxTileControlViewInfo.IsFixedContentLeftSide: Boolean;
begin
  Result := TileControl.OptionsView.CenterContentHorz and FFixedGroupsOrigin;
end;

function TdxTileControlViewInfo.IsFixedContentTopSide: Boolean;
begin
  Result := TileControl.OptionsView.CenterContentVert and FFixedGroupsOrigin;
end;

function TdxTileControlViewInfo.IsScrollBarsParametersWasChanged: Boolean;
var
  ABounds: TRect;
  APos: Integer;
begin
  ABounds := GetContentBounds;
  FContentHeight := Max(ABounds.Bottom - ABounds.Top, VertScrollPage);
  FContentWidth := Max(ABounds.Right - ABounds.Left, HorzScrollPage);
  FContentRight := ABounds.Right;
  FContentBottom := ABounds.Bottom;
  Result := False;
  if VertScrollPage + TopScrollPos > FContentHeight then
  begin
    APos := TopScrollPos;
    TopScrollPos := Max(0, FContentHeight - VertScrollPage);
    Result := TopScrollPos <> APos;
  end;
  if HorzScrollPage + LeftScrollPos > FRealContentWidth then
  begin
    APos := LeftScrollPos;
    LeftScrollPos := Max(0, FRealContentWidth - HorzScrollPage);
    Result := Result or (LeftScrollPos <> APos);
  end;
end;

procedure TdxTileControlViewInfo.MakeVisible(
  const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
begin
  if GroupLayout = glHorizontal then
    MakeVisibleOnHorz(ABounds, AIsLeftMost, AIsRightMost)
  else
    MakeVisibleOnVert(ABounds, AIsLeftMost, AIsRightMost);
end;

procedure TdxTileControlViewInfo.MakeVisibleOnHorz(
  const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
var
  APos, AGap: Integer;
begin
  APos := LeftScrollPos;
  AGap := ItemHalfIndent + TileControl.GetBorderSize * 2;
  if AIsLeftMost then
    APos := 0
  else
    if AIsRightMost then
      APos := FContentWidth - HorzScrollPage
    else
      if ABounds.Left < TilesArea.Left then
        Dec(APos, TilesArea.Left - ABounds.Left + AGap)
      else
        if ABounds.Right > TilesArea.Right then
          Inc(APos, ABounds.Right - TilesArea.Right + AGap);
  if LeftScrollPos <> APos then
    Scroll(APos - LeftScrollPos, 0);
end;

procedure TdxTileControlViewInfo.MakeVisibleOnVert(
  const ABounds: TRect; AIsLeftMost, AIsRightMost: Boolean);
var
  APos, AGap: Integer;
begin
  APos := TopScrollPos;
  AGap := ItemHalfIndent + TileControl.GetBorderSize * 2;
  if AIsLeftMost then
    APos := 0
  else
    if AIsRightMost then
      APos := FContentHeight - VertScrollPage
    else
      if ABounds.Top < TilesArea.Top then
        Dec(APos, TilesArea.Top - ABounds.Top + AGap)
      else
        if ABounds.Bottom > TilesArea.Bottom then
          Inc(APos, ABounds.Bottom - TilesArea.Bottom + AGap);
  if TopScrollPos <> APos then
    Scroll(0, APos - TopScrollPos);
end;

procedure TdxTileControlViewInfo.MeasureGroupsCaptionsHeights;
var
  I, AMaxHeight: Integer;
  ACaption: TdxTileControlGroupCaption;
begin
  AMaxHeight := 0;
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Visible then
    begin
      ACaption := Groups[I].Caption;
      ACaption.MeasureHeight;
      AMaxHeight := Max(AMaxHeight, ACaption.Height);
    end;
  if (AMaxHeight > 0) and (TileControl.OptionsView.GroupLayout = glHorizontal) then
    for I := 0 to Groups.Count - 1 do
      if Groups[I].Visible then
        Groups[I].Caption.SetHeight(AMaxHeight);
end;

procedure TdxTileControlViewInfo.StoreGroupsOrigin;
var
  I: Integer;
  AGroup: TdxTileControlGroup;
begin
  FFixedGroupsOrigin := True;
  FVisibleGroupsOrigin.X := 0;
  FVisibleGroupsOrigin.Y := 0;
  for I := 0 to TileControl.Groups.Count - 1 do
  begin
    AGroup := TileControl.Groups[I];
    if AGroup.Visible then
    begin
      FVisibleGroupsOrigin.X := AGroup.Bounds.Left;
      FVisibleGroupsOrigin.Y := AGroup.Bounds.Top;
      Break;
    end;
  end;
end;

procedure TdxTileControlViewInfo.Scroll(const DX, DY: Integer);
const
  ASign: array[Boolean] of Integer = (1, -1);
var
  I: Integer;
begin
  TileControl.ShowTouchScrollUI(TileControl, True);
  Inc(FLeftScrollPos, DX);
  Inc(FTopScrollPos, DY);
  for I := 0 to Cells.Count - 1 do
    Cells[I].Scroll(ASign[UseRightToLeftAlignment] * -DX, -DY);
  TileControl.HitTest.HitPoint := TileControl.ScreenToClient(GetMouseCursorPos);
  TileControl.Controller.HottrackedItem := TileControl.HitTest.Item;
  TileControl.Invalidate;
  if not TileControl.Controller.HasRubberAnimation then
    TileControl.UpdateScrollBars;
end;

procedure TdxTileControlViewInfo.Draw(ACanvas: TcxCanvas);

  procedure InternalDrawScrollButtons;
  var
    I: Integer;
    AClipRect: TRect;
  begin
    for I := 0 to Cells.Count - 1 do
      if Cells[I] is TdxTileControlScrollButtonViewInfo and Cells[I].Visible then
      begin
        cxRectIntersect(AClipRect, TilesArea, Cells[I].Bounds);
        if not cxRectIsEqual(Cells[I].Bounds, AClipRect) then
          Cells[I].Draw(ACanvas);
      end;
  end;

var
  ARegion: TcxRegion;
begin
  DrawBackground(ACanvas);
  if TileControl.IsDesigning and TileControl.Transparent then
    DrawDesignTimeBorder(ACanvas);
  ACanvas.SaveClipRegion;
  try
    ARegion := TcxRegion.Create(TilesArea);
    ARegion.Combine(Title.ViewInfo.Bounds, roAdd);
    ACanvas.SetClipRegion(ARegion, roIntersect);
    Cells.Draw(ACanvas);
    if DragDropChanges <> nil then
      DragDropChanges.DrawItems(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
  if (TileControl.OptionsBehavior.ScrollMode = smScrollButtons) and
    (TileControl.OptionsView.FixedIndentHorz or TileControl.OptionsView.FixedIndentVert) then
    InternalDrawScrollButtons;
end;

procedure TdxTileControlViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if (Background = nil) or not cxRectIsEqual(Background.ClientRect, TileControl.ClientRect) then
  begin
    FreeAndNil(FBackGround);
    FBackground := TcxBitmap.CreateSize(TileControl.ClientRect);
  end;
  Painter.DrawBackground(Background.cxCanvas, Background.ClientRect);
  cxBitBlt(ACanvas.Handle, Background.cxCanvas.Handle, TileControl.ClientRect, cxNullPoint, SRCCOPY);
end;

procedure TdxTileControlViewInfo.DrawDesignTimeBorder(ACanvas: TcxCanvas);
const
  AStyles: array [Boolean] of TPenStyle = (psDash, psDashDot);
  AColors: array [Boolean] of TColor = (dxTileVirtualGroupBackgroundColor, clBlue);
begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := AStyles[TileControl.Controller.IsObjectSelected(TileControl)];
  ACanvas.Pen.Color := AColors[TileControl.Controller.IsObjectSelected(TileControl)];
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(Bounds);
end;

procedure TdxTileControlViewInfo.DrawTileControlPart(ADest: TcxBitmap32; const AArea: TRect);
var
  ABuffer: TcxBitmap32;
begin
  if cxRectIsEqual(Bounds, AArea) then
    Draw(ADest.cxCanvas)
  else
  begin
    ABuffer := TcxBitmap32.CreateSize(Bounds);
    try
      Draw(ABuffer.cxCanvas);
      cxBitBlt(ADest.Canvas.Handle, ABuffer.Canvas.Handle, ADest.ClientRect, AArea.TopLeft, SRCCOPY);
    finally
      FreeAndNil(ABuffer);
    end;
  end;
end;

function TdxTileControlViewInfo.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlViewInfo.GetGroups: TdxTileControlGroupCollection;
begin
  Result := TileControl.Groups;
end;

function TdxTileControlViewInfo.GetGroupCount: Integer;
begin
  Result := TileControl.Groups.Count;
end;

function TdxTileControlViewInfo.GetHorzScrollPage: Integer;
begin
  Result := cxRectWidth(TilesArea);
end;

function TdxTileControlViewInfo.GetItemsMaxWidth: Integer;
var
  I, AWidth, ARegularWidth: Integer;
begin
  Result := 0;
  ARegularWidth := TileControl.OptionsView.ItemWidth;
  for I := 0 to TileControl.Items.Count - 1 do
  begin
    AWidth := TileControl.Items[I].ViewInfo.GetWidth;
    if AWidth > Result then
      Result := AWidth;
    if Result > ARegularWidth then
      Break;
  end;
end;

function TdxTileControlViewInfo.GetMaxPossibilityFocusItemFrameSize: Integer;
begin
  if TileControl.OptionsBehavior.ItemFocusMode = tcifmDefault then
    Result := ScaleFactor.Apply(dxTileFocusItemFrameSize)
  else
    Result := ScaleFactor.Apply(dxTileFocusItemOuterFrameSize);
end;

function TdxTileControlViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileControl.ScaleFactor;
end;

function TdxTileControlViewInfo.GetSubColumnSize: Integer;
begin
  Result := SubColumnSizes[RowCount > 1];
end;

function TdxTileControlViewInfo.GetTitle: TdxTileControlTitle;
begin
  Result := TileControl.Title;
end;

function TdxTileControlViewInfo.GetItems: TdxTileControlItemCollection;
begin
  Result := TileControl.Items;
end;

function TdxTileControlViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := TileControl.Painter;
end;

procedure TdxTileControlViewInfo.HideScrollButtons;
var
  I: TcxArrowDirection;
begin
  for I := Low(TcxArrowDirection) to High(TcxArrowDirection) do
    FScrollButtonViewInfo[I].State := cxbsDisabled;
end;

function TdxTileControlViewInfo.IsContentCentredHorz: Boolean;
begin
  Result := TileControl.OptionsView.CenterContentHorz and
    (ContentWidth <= HorzScrollPage) and (VirtualGroups.Count > 0);
end;

function TdxTileControlViewInfo.IsContentCentredVert: Boolean;
begin
  Result := TileControl.OptionsView.CenterContentVert and
    (ContentHeight <= VertScrollPage) and (VirtualGroups.Count > 0);
end;

function TdxTileControlViewInfo.IsContentNotCentredHorz: Boolean;
begin
  Result := not IsContentCentredHorz or IsFixedContentLeftSide;
end;

function TdxTileControlViewInfo.IsContentNotCentredVert: Boolean;
begin
  Result := not IsContentCentredVert or IsFixedContentTopSide;
end;

function TdxTileControlViewInfo.IsScrollAvailable: Boolean;
begin
  Result := IsScrollAvailable(adUp) or IsScrollAvailable(adDown) or
    IsScrollAvailable(adLeft) or IsScrollAvailable(adRight);
end;

function TdxTileControlViewInfo.IsScrollAvailable(ADirection: TcxArrowDirection): Boolean;
begin
  case ADirection of
    adUp:
      Result := TopScrollPos <> 0;
    adDown:
      Result := TopScrollPos < ContentHeight - 1 - VertScrollPage;
    adLeft:
      Result := LeftScrollPos <> 0;
  else
    Result := LeftScrollPos < ContentWidth - 1 - HorzScrollPage;
  end;
end;

procedure TdxTileControlViewInfo.RefreshState;
begin
  inherited RefreshState;
  Cells.RefreshState;
end;

procedure TdxTileControlViewInfo.SetLeftScrollPos(AValue: Integer);
begin
  if not TileControl.Controller.IsHandScrolling(sbHorizontal) then
    AValue := Max(0, Min(AValue, ContentWidth - HorzScrollPage));
  if AValue <> FLeftScrollPos then
    Scroll(AValue - FLeftScrollPos, 0);
end;

procedure TdxTileControlViewInfo.SetTopScrollPos(AValue: Integer);
begin
  if not TileControl.Controller.IsHandScrolling(sbVertical) then
    AValue := Max(0, Min(AValue, ContentHeight - VertScrollPage));
  if AValue <> FTopScrollPos then
    Scroll(0, AValue - FTopScrollPos);
end;

{ TdxTileControlItemPersistent }

procedure TdxTileControlItemPersistent.Changed;
begin
  if Owner <> nil then
    Owner.Changed;
end;

procedure TdxTileControlItemPersistent.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TdxTileControlItemPersistent.IsChanged: Boolean;
begin
  Result := False;
end;

function TdxTileControlItemPersistent.GetOwnerItem: TdxTileControlCustomItem;
begin
  Result := TdxTileControlCustomItem(GetOwner);
end;

{ TdxTileControlPainter }

constructor TdxTileControlPainter.Create(ATileControl: TdxCustomTileControl);
begin
  inherited Create;
  FTileControl := ATileControl;
end;

procedure TdxTileControlPainter.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
var
  ATextureOffsetX, ATextureOffsetY: Integer;
begin
  if TileControl.Transparent then
  begin
    DrawTransparentBackground(ACanvas, R);
    Exit;
  end;
  ATextureOffsetX := MulDiv(TileControl.ViewInfo.LeftScrollPos, TileControl.OptionsBehavior.BackgroundScrollSync, 100);
  ATextureOffsetY := MulDiv(TileControl.ViewInfo.TopScrollPos, TileControl.OptionsBehavior.BackgroundScrollSync, 100);
  if UseSkins then
  begin
    SkinInfo.TileControlBackground.UseCache := True;
    SkinInfo.TileControlBackground.Draw(ACanvas.Handle, R);
    if not TileControl.Style.Texture.Empty then
    begin
      dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
      try
        DrawStyleTexture(dxGPPaintCanvas, R, TileControl.Style, ATextureOffsetX, ATextureOffsetY);
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    end;
  end
  else
  begin
    if TileControl.Style.IsEmpty then
      LookAndFeelPainter.DrawPanelContent(ACanvas, R, False);
    DrawStyleRect(ACanvas, R, TileControl.Style, ATextureOffsetX, ATextureOffsetY, []);
  end;
end;

procedure TdxTileControlPainter.DrawGlyph(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AGlyph: TdxSkinImage; AGlyphType: TImageType; AColor: TColor; const AIsRTL: Boolean = False);
const
  StateMap: array[TcxButtonState] of Integer = (0, 0, 1, 2, 3);
var
  ASkinImage: TdxSkinImage;
begin
  if (AGlyphType = itMask) and cxColorIsValid(AColor) then
  begin
    ASkinImage := TdxSkinImage.Create(nil);
    try
      ASkinImage.Assign(AGlyph);
      ASkinImage.Texture.ChangeColor(AColor);
      if AIsRTL then
        ASkinImage.DrawRTL(ACanvas.Handle, R, TileControl.ScaleFactor, StateMap[AState])
      else
        ASkinImage.Draw(ACanvas.Handle, R, TileControl.ScaleFactor, StateMap[AState]);
    finally
      ASkinImage.Free;
    end;
  end
  else
    AGlyph.Draw(ACanvas.Handle, R, StateMap[AState]);
end;

procedure TdxTileControlPainter.DrawDefaultItemBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if UseSkins then
    SkinInfo.TileControlItem.Draw(ACanvas.Handle, R)
  else
    dxGpFillRectByGradient(ACanvas.Handle, R, dxTileItemShadowColor, dxTileItemLightColor, LinearGradientModeForwardDiagonal);
end;

procedure TdxTileControlPainter.DrawDefaultItemCheck(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawDefaultItemCheck(ACanvas, R, CheckedItemFrameColor, CheckedItemCheckMarkColor);
end;

procedure TdxTileControlPainter.DrawDefaultItemCheck(ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor, ACheckMarkColor: TColor; AAlpha: Byte = MaxByte);

  function CreateRegion: TcxRegion;
  var
    APoints: array[0..2] of TPoint;
    AScaleFactor: TdxScaleFactor;
  begin
    AScaleFactor := TileControl.ScaleFactor;

    APoints[0] := cxPoint(R.Right - AScaleFactor.Apply(dxTileCheckedItemCheckMarkAreaSize), R.Top);
    APoints[1] := cxPoint(R.Right, R.Top);
    APoints[2] := cxPoint(R.Right, R.Top + AScaleFactor.Apply(dxTileCheckedItemCheckMarkAreaSize));

    Result := TcxRegion.Create(R);
    Result.Combine(cxRectInflate(R, -AScaleFactor.Apply(dxTileCheckedItemFrameBorderSize)), roSubtract);
    Result.Combine(TcxRegion.Create(CreatePolygonRgn(APoints[0], Length(APoints), ALTERNATE)), roAdd);
  end;

  function CalculateCheckMarkArea(const R: TRect): TRect;
  var
    ACheckMarkAreaHalfSize: Integer;
    AScaleFactor: TdxScaleFactor;
  begin
    AScaleFactor := TileControl.ScaleFactor;
    ACheckMarkAreaHalfSize := AScaleFactor.Apply(dxTileCheckedItemCheckMarkAreaSize) div 2;
    Result := cxRectSetHeight(R, MulDiv(ACheckMarkAreaHalfSize, AScaleFactor.Apply(8), AScaleFactor.Apply(9)));
    Result := cxRectSetRight(Result, Result.Right, ACheckMarkAreaHalfSize);
    Result := cxRectInflate(Result, 0, -2 * AScaleFactor.Apply(dxTileCheckedItemFrameBorderSize),
      -2 * AScaleFactor.Apply(dxTileCheckedItemFrameBorderSize), 0);
  end;

  procedure DrawCheckMark(AGraphics: TdxGPGraphics; AColor: TColor);
  var
    ACheckMarkArea: TRect;
    APoints: array[0..2] of TPoint;
    APrevSmoothingMode: TdxGPSmoothingMode;
  begin
    ACheckMarkArea := CalculateCheckMarkArea(R);
    APoints[0] := cxPoint(ACheckMarkArea.Right, ACheckMarkArea.Top);
    APoints[1] := cxPoint((ACheckMarkArea.Left + ACheckMarkArea.Right) div 2, ACheckMarkArea.Bottom);
    APoints[2] := cxPoint(ACheckMarkArea.Left, (ACheckMarkArea.Bottom + ACheckMarkArea.Top) div 2);

    APrevSmoothingMode := dxGPPaintCanvas.SmoothingMode;
    try
      dxGPPaintCanvas.SmoothingMode := smAntiAlias;
      dxGPPaintCanvas.Polyline(APoints, AColor, TileControl.ScaleFactor.Apply(dxTileCheckedItemCheckMarkPenSize),
        psSolid, AAlpha);
    finally
      dxGPPaintCanvas.SmoothingMode := APrevSmoothingMode;
    end;
  end;

begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(CreateRegion, roIntersect);
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      if ABackgroundColor <> clNone then
        dxGPPaintCanvas.FillRectangle(R, dxColorToAlphaColor(ABackgroundColor, AAlpha));
      if ACheckMarkColor <> clNone then
        DrawCheckMark(dxGPPaintCanvas, ACheckMarkColor);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxTileControlPainter.DrawHighlightRect(ACanvas: TcxCanvas; const R: TRect;
  const AHighlightColor: TdxAlphaColor; const ABackgroundColor: TColor = clNone);

  function GetSelectionColor(ADestColor: TColor; const AAlpha: Byte): TColor;

    function GetValue(ASValue, ADValue: Byte): Byte;
    begin
      Result := Round((ADValue - ASValue * (255 - AAlpha) / 255));
    end;

  begin
    Result := RGB(GetValue(GetRValue(ABackgroundColor), GetRValue(ADestColor)),
                  GetValue(GetGValue(ABackgroundColor), GetGValue(ADestColor)),
                  GetValue(GetBValue(ABackgroundColor), GetBValue(ADestColor)));
  end;

var
  AColor: TColor;
  AAlpha: Byte;
begin
  AColor := dxAlphaColorToColor(AHighlightColor, AAlpha);
  if ABackgroundColor <> clNone then
    AColor := GetSelectionColor(AColor, AAlpha);
  dxGpFillRect(ACanvas.Handle, R, AColor, AAlpha);
end;

procedure TdxTileControlPainter.DrawItemCheck(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  if UseSkins then
    SkinInfo.TileControlItemCheck.Draw(ACanvas.Handle, R, AScaleFactor)
  else
    DrawDefaultItemCheck(ACanvas, R);
end;

procedure TdxTileControlPainter.DrawStyleRect(ACanvas: TcxCanvas; const R: TRect;
  AStyle: TdxTileControlCustomStyle; const ATextureOffsetX, ATextureOffsetY: Integer; ABorders: TcxBorders);

  procedure DrawFrame(ACanvas: TdxGpCanvas; const R: TRect; AColor: TdxAlphaColor; ALineWidth: Integer; ABorders: TcxBorders);
  begin
    if cxColorIsValid(AColor) and (ALineWidth > 0) and (ABorders <> []) then
    begin
      if bLeft in ABorders then
        ACanvas.FillRectangle(cxRect(R.Left, R.Top, Min(R.Left + ALineWidth, R.Right), R.Bottom), AColor);
      if bRight in ABorders then
        ACanvas.FillRectangle(cxRect(Max(R.Right - ALineWidth, R.Left), R.Top, R.Right, R.Bottom), AColor);
      if bTop in ABorders then
        ACanvas.FillRectangle(cxRect(R.Left, R.Top, R.Right, Min(R.Top + ALineWidth, R.Bottom)), AColor);
      if bBottom in ABorders then
        ACanvas.FillRectangle(cxRect(R.Left, Max(R.Bottom - ALineWidth, R.Top), R.Right, R.Bottom), AColor);
    end;
  end;

begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGPPaintCanvas.FillRectangleByGradient(R,
      dxColorToAlphaColor(cxGetActualColor(AStyle.GradientBeginColor, LookAndFeelPainter.DefaultContentColor)),
      dxColorToAlphaColor(AStyle.GradientEndColor), dxSkinsGradientModeMap[AStyle.Gradient]);
    DrawStyleTexture(dxGPPaintCanvas, R, AStyle, ATextureOffsetX, ATextureOffsetY);

    if (AStyle.BorderColor <> clNone) and (ABorders <> []) then
      DrawFrame(dxGPPaintCanvas, R, dxColorToAlphaColor(cxGetActualColor(AStyle.BorderColor, clBtnShadow)), 1, ABorders);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxTileControlPainter.DrawStyleTexture(ACanvas: TdxGPCanvas;
  const R: TRect; AStyle: TdxTileControlCustomStyle; const ATextureOffsetX, ATextureOffsetY: Integer);
var
  ATextureRect: TRect;
begin
  ATextureRect := R;
  if AStyle.Stretch <> smStretch then
  begin
    Dec(ATextureRect.Left, ATextureOffsetX);
    Dec(ATextureRect.Top, ATextureOffsetY);
  end;
  ACanvas.InterpolationMode := imHighQualityBicubic;
  case AStyle.Stretch of
    smStretch:
      ACanvas.Draw(AStyle.Texture, R);
    smTile:
      ACanvas.DrawTile(AStyle.Texture, R);
    smNoResize:
      ACanvas.Draw(AStyle.Texture, cxRectCenter(R, AStyle.Texture.Width, AStyle.Texture.Height));
  end;
end;

procedure TdxTileControlPainter.DrawItemBackground(
  ACanvas: TcxCanvas; const R: TRect; AStyle: TdxTileControlCustomStyle);
begin
  if AStyle.IsEmpty then
    DrawDefaultItemBackground(ACanvas, R)
  else
    DrawStyleRect(ACanvas, R, AStyle, 0, 0, cxBordersAll)
end;

procedure TdxTileControlPainter.DrawItemPlace(ACanvas: TcxCanvas; const R: TRect);
begin
  if UseSkins then
    SkinInfo.TileControlVirtualGroup.Draw(ACanvas.Handle, R)
  else
    ACanvas.FillRect(R, dxTileItemPlaceColor);
end;

procedure TdxTileControlPainter.DrawPageTab(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
const
  ButtonStateToSkinState: array[TcxButtonState] of TdxSkinElementState = (
    esNormal, esNormal, esHot, esPressed, esDisabled
  );
begin
  if UseSkins then
    SkinInfo.TileControlTabHeader.Draw(ACanvas.Handle, R, 0, ButtonStateToSkinState[AState]);
end;

procedure TdxTileControlPainter.DrawTransparentBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(R), roIntersect);
    cxDrawTransparentControlBackground(TileControl, ACanvas, R);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxTileControlPainter.DrawVirtualGroup(ACanvas: TcxCanvas; const R: TRect);
begin
  if UseSkins then
    SkinInfo.TileControlVirtualGroup.Draw(ACanvas.Handle, R)
  else
    ACanvas.FillRect(R, dxTileVirtualGroupBackgroundColor);
end;

procedure TdxTileControlPainter.ExcludeInvisibleOuterFrameParts(ACanvas: TcxCanvas; const R: TRect; const AFrameSize: Integer);
var
  ATilesArea: TRect;
  AHasScrollBar: Boolean;
begin
  ATilesArea := TileControl.ViewInfo.TilesArea;
  AHasScrollBar := TileControl.OptionsBehavior.ScrollMode = smScrollbars;
  if R.Left + AFrameSize < ATilesArea.Left then
    ACanvas.ExcludeClipRect(cxRect(R.Left, R.Top, ATilesArea.Left, R.Bottom));
  if (R.Right - AFrameSize - 1 >= ATilesArea.Right) or
    (AHasScrollBar and TileControl.IsScrollBarActive(sbVertical) and (R.Right >= TileControl.GetVScrollBarBounds.Left - 1)) then
    ACanvas.ExcludeClipRect(cxRect(ATilesArea.Right, R.Top, R.Right, R.Bottom));
  if R.Top + AFrameSize < ATilesArea.Top then
    ACanvas.ExcludeClipRect(cxRect(R.Left, R.Top, R.Right, ATilesArea.Top));
  if (R.Bottom - AFrameSize - 1 >= ATilesArea.Bottom) or
    (AHasScrollBar and TileControl.IsScrollBarActive(sbHorizontal) and (R.Bottom >= TileControl.GetHScrollBarBounds.Top - 1)) then
    ACanvas.ExcludeClipRect(cxRect(R.Left, ATilesArea.Bottom, R.Right, R.Bottom));
end;

function TdxTileControlPainter.GetActionBarDefaultBackgroundColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlActionBar.Color;
  if Result = clDefault then
    Result := dxTileDefaultActionBarColor;
end;

function TdxTileControlPainter.GetActionBarDefaultTextColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlActionBar.TextColor;
  if Result = clDefault then
    Result := dxTileDefaultActionBarTextColor;
end;

function TdxTileControlPainter.GetCheckedItemCheckMarkColor: TColor;
begin
  Result := cxGetActualColor(TileControl.Style.CheckedItemCheckMarkColor, clHighlightText);
end;

function TdxTileControlPainter.GetCheckedItemFrameColor: TColor;
begin
  Result := cxGetActualColor(TileControl.Style.CheckedItemFrameColor, clHighlight);
end;

function TdxTileControlPainter.GetDefaultItemTextBackgroundColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlItem.Color;
  if Result = clDefault then
    Result := clBlack;
end;

function TdxTileControlPainter.GetDefaultItemTextColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlItem.TextColor;
  if Result = clDefault then
    Result := clWhite;
end;

function TdxTileControlPainter.GetGroupCaptionDefaultTextColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlGroupCaption.TextColor;
  if Result = clDefault then
    Result := clWindowText;
end;

function TdxTileControlPainter.GetGroupCaptionDefaultFontSize: Integer;
begin
  Result := dxTileControlDefaultGroupCaptionFontSize;
  if UseSkins then
    Inc(Result, SkinInfo.TileControlGroupCaptionFontDelta);
end;

function TdxTileControlPainter.GetItemOuterFrameColor(AItem: TdxTileControlCustomItem): TColor;
begin
  if (AItem <> nil) and (TileControl.OptionsBehavior.ItemOuterFrameColor = clDefault) then
    Result := GetColorAsItemBackground(AItem)
  else
    Result := TileControl.OptionsBehavior.ItemOuterFrameColor;
end;

function TdxTileControlPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TileControl.LookAndFeelPainter;
end;

function TdxTileControlPainter.GetPageTabContentOffset: TRect;
begin
  if UseSkins then
    Result := SkinInfo.TileControlTabHeader.ContentOffset.Rect
  else
    Result := cxRect(15, cxTextOffset, 15, cxTextOffset);
end;

function TdxTileControlPainter.GetPageTabDefaultFontSize: Integer;
begin
  if UseSkins then
    Result := 8 + SkinInfo.TileControlTabHeaderFontDelta
  else
    Result := dxTileControlDefaultTabFontSize;
end;

function TdxTileControlPainter.GetPageTabTextColor(AState: TcxButtonState): TColor;
var
  ATitle: TdxTileControlTitle;
begin
  ATitle := TileControl.Title;
  case AState of
    cxbsHot:
      Result := ATitle.TabsHotTextColor;
    cxbsPressed:
      Result := ATitle.TabsActiveTextColor;
    cxbsDisabled:
      Result := ATitle.TabsDisabledTextColor;
    else
      Result := ATitle.TabsTextColor;
  end;

  if (Result = clDefault) and UseSkins then
    Result := SkinInfo.TileControlTabHeader.GetTextColor(AState);

  case AState of
    cxbsHot, cxbsPressed:
      Result := cxGetActualColor(Result, clHighlight);
    cxbsDisabled:
      Result := cxGetActualColor(Result, clGrayText);
    else
      Result := cxGetActualColor(Result, clWindowText);
  end;
end;

class function TdxTileControlPainter.GetColorAsItemBackground(AItem: TdxTileControlCustomItem): TColor;
begin
  Result := AItem.Style.GradientBeginColor;
  if AItem.Style.IsDefaultBackgroundColors then
    Result := dxTileItemLightColor;
end;

function TdxTileControlPainter.GetSelectionFocusedColor(AItem: TdxTileControlCustomItem = nil): TColor;
begin
  if TileControl.OptionsBehavior.ItemFocusMode = tcifmOuterFrame then
    Result := GetItemOuterFrameColor(AItem)
  else
  begin
    Result := TileControl.Style.FocusedColor;
    if (Result = clDefault) and UseSkins then
      Result := SkinInfo.TileControlSelectionFocusedColor;
    if (Result = clDefault) then
      Result := dxGetLighterColor(clHighlight, 70);
  end;
end;

function TdxTileControlPainter.GetSelectionHottrackedColor(AItem: TdxTileControlCustomItem = nil): TColor;
begin
  if TileControl.OptionsBehavior.ItemHotTrackMode = tcihtmOuterFrame then
    Result := GetItemOuterFrameColor(AItem)
  else
  begin
    Result := TileControl.Style.FocusedColor;
    if (Result = clDefault) and UseSkins then
      Result := SkinInfo.TileControlSelectionHotColor;
    if (Result = clDefault) then
      Result := dxGetLighterColor(clHighlight, 70);
  end;
end;

function TdxTileControlPainter.GetSelectionSelectedColor: TColor;
begin
  if UseSkins then
    Result := SkinInfo.TileControlSelectionFocusedColor
  else
    Result := clDefault;
  if Result = clDefault then
    Result := ColorToRGB(dxGetLighterColor(clHighlight, 70));
end;

function TdxTileControlPainter.GetTitleDefaultFontSize: Integer;
begin
  if UseSkins then
    Result := 8 + SkinInfo.TileControlTitleFontDelta
  else
    Result := dxTileControlDefaultTitleFontSize;
end;

function TdxTileControlPainter.GetTitleDefaultTextColor: TColor;
begin
  Result := clDefault;
  if UseSkins then
    Result := SkinInfo.TileControlTitle.TextColor;
  if Result = clDefault then
    Result := clWindowText;
end;

function TdxTileControlPainter.GetUserSkins: Boolean;
begin
  Result := SkinInfo <> nil;
end;

procedure TdxTileControlPainter.ValidatePainterData;
begin
  if not LookAndFeelPainter.GetPainterData(FSkinInfo) then
    FSkinInfo := nil;
end;

{ TdxTileControlPageTabCellViewInfo }

constructor TdxTileControlPageTabCellViewInfo.Create(AOwner: TdxTileControlDetailSiteTitleViewInfo; AItem: TdxTileControlItem);
begin
  FOwner := AOwner;
  FItem := AItem;
end;

procedure TdxTileControlPageTabCellViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  ATextRect: TRect;
begin
  Painter.DrawPageTab(ACanvas, Bounds, State);
  ATextRect := cxRectContent(Bounds, GetContentOffsets);
  cxTextOut(ACanvas.Canvas, Caption, ATextRect, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_EXPANDTABS,
    0, 0, Font, clNone, clNone, 0, 0, 0, Painter.PageTabTextColor[State]);
end;

function TdxTileControlPageTabCellViewInfo.GetActive: Boolean;
begin
  Result := False;
end;

function TdxTileControlPageTabCellViewInfo.GetCaption: string;
var
  I: Integer;
begin
  Result := Item.DetailOptions.Caption;
  if Result <> '' then Exit;
  for I := 0 to 3 do
    if Item.GetText(I).HasValue then
    begin
      Result := Item.GetText(I).GetActualValue;
      Break;
    end;
  if Result = '' then
    Result := Item.Name;
end;

function TdxTileControlPageTabCellViewInfo.GetContentOffsets: TRect;
begin
  Result := ScaleFactor.Apply(Painter.PageTabContentOffset);
end;

function TdxTileControlPageTabCellViewInfo.GetFont: TFont;
begin
  Result := Owner.TabsFont;
end;

function TdxTileControlPageTabCellViewInfo.GetHitTest(
  AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtPageTab] := True;
end;

function TdxTileControlPageTabCellViewInfo.GetMarginsWidth: Integer;
begin
  Result := cxMarginsWidth(GetContentOffsets);
end;

function TdxTileControlPageTabCellViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := Owner.TileControl.Painter;
end;

function TdxTileControlPageTabCellViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxTileControlPageTabCellViewInfo.MeasureHeight: Integer;
begin
  Result := cxTextHeight(Font, Caption) + cxMarginsHeight(GetContentOffsets);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxTileControlPageTabCellViewInfo.MeasureWidth: Integer;
begin
  Result := cxTextWidth(Font, Caption) + GetMarginsWidth;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

procedure TdxTileControlPageTabCellViewInfo.RefreshState;
begin
  if not Item.IsEnabled then
    State := cxbsDisabled
  else
    if Item.IsActive then
      State := cxbsPressed
    else
      if Owner.TileControl.HitTest.HitObject = Self then
        State := cxbsHot
      else
        State := cxbsNormal;
end;

procedure TdxTileControlPageTabCellViewInfo.SetBounds(
  const ABounds, AVisibleBounds: TRect);
begin
  FBounds := ABounds;
  FVisibleBounds := AVisibleBounds;
  Recalculate;
end;

procedure TdxTileControlPageTabCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  VisibleBounds := TdxRightToLeftLayoutConverter.ConvertRect(VisibleBounds, AClientBounds);
  inherited DoRightToLeftConversion(AClientBounds);
end;

procedure TdxTileControlPageTabCellViewInfo.SetState(AValue: TcxButtonState);
begin
  if AValue <> FState then
  begin
    FState := AValue;
    Owner.FOwner.InvalidateRect(Bounds, False);
  end;
end;

{ TdxTileControlItemFrame }

destructor TdxTileControlItemFrame.Destroy;
begin
  TileItem.DoFrameDestroy(Self);
  TileItem.Controller.FrameDestroyed(Self);
  inherited Destroy;
end;

procedure TdxTileControlItemFrame.Changed;
begin
  TileItem.ViewInfo.IsDirty := True;
  ViewData.IsDirty := True;
  TileItem.FrameChanged(Self);
end;

function TdxTileControlItemFrame.CreateGlyph: TdxTileControlItemCustomGlyph;
begin
  Result := TdxTileControlItemFrameGlyph.Create(Self);
end;

function TdxTileControlItemFrame.CreateOptionsAnimate: TdxTileControlItemOptionsAnimate;
begin
  Result := TdxTileControlItemFrameOptionsAnimate.Create(Self);
end;

function TdxTileControlItemFrame.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := TdxTileControlItem(AParent).Frames;
end;

function TdxTileControlItemFrame.GetGlyph: TdxTileControlItemFrameGlyph;
begin
  Result := TdxTileControlItemFrameGlyph(inherited Glyph);
end;

function TdxTileControlItemFrame.GetTileItem: TdxTileControlItem;
begin
  Result := TdxTileControlItem(Collection.ParentComponent);
end;

procedure TdxTileControlItemFrame.SetGlyph(AValue: TdxTileControlItemFrameGlyph);
begin
  inherited Glyph.Assign(AValue);
  Changed;
end;

{ TdxTileControlCustomButtonViewInfo }

function TdxTileControlCustomButtonViewInfo.GetTextureType: TImageType;
begin
  Result := itMask;
end;

function TdxTileControlCustomButtonViewInfo.MeasureHeight: Integer;
begin
  Result := ScaleFactor.Apply(Texture.Size.cy);
end;

function TdxTileControlCustomButtonViewInfo.MeasureWidth: Integer;
begin
  Result := ScaleFactor.Apply(Texture.Size.cx);
end;

procedure TdxTileControlCustomButtonViewInfo.SetBounds(const ABounds, AVisibleBounds: TRect);
begin
  Bounds := ABounds;
  VisibleBounds := AVisibleBounds;
  Recalculate;
end;

procedure TdxTileControlCustomButtonViewInfo.SetState(const Value: TcxButtonState);
begin
  if Value <> FState then
  begin
    FState := Value;
    Invalidate;
  end;
end;

{ TdxTileControlDetailSiteBackButtonViewInfo }

constructor TdxTileControlDetailSiteBackButtonViewInfo.Create(AOwner: TdxTileControlDetailSite);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxTileControlDetailSiteBackButtonViewInfo.Invalidate;
begin
  if Visible then
    Owner.InvalidateRect(Bounds, False);
end;

procedure TdxTileControlDetailSiteBackButtonViewInfo.RefreshState;
begin
  if (Owner.PressedCell = Self) and (Owner.HitTest.HitObject = Self) then
    State := cxbsPressed
  else
    if (Owner.HitTest.HitObject = Self) and (Owner.PressedCell = nil) then
      State := cxbsHot
    else
      State := cxbsNormal;
end;

procedure TdxTileControlDetailSiteBackButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Owner.TileControl.Painter.DrawGlyph(ACanvas, Bounds, State, Texture, TextureType, Owner.TitleViewInfo.TextColor,
    Owner.TileControl.ViewInfo.UseRightToLeftAlignment);
end;

function TdxTileControlDetailSiteBackButtonViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtBackButton] := True;
end;

function TdxTileControlDetailSiteBackButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.TileControl.ScaleFactor;
end;

function TdxTileControlDetailSiteBackButtonViewInfo.GetTexture: TdxSkinImage;
begin
  Result := Owner.TileControl.Assets.BackButton;
end;

{ TdxTileControlScrollButtonViewInfo }

constructor TdxTileControlScrollButtonViewInfo.Create(
  AOwner: TdxCustomTileControl; ADirection: TcxArrowDirection);
begin
  inherited Create;
  FOwner := AOwner;
  FDirection := ADirection;
  FState := cxbsDisabled;
  FTimer := TcxTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerHandler;
end;

destructor TdxTileControlScrollButtonViewInfo.Destroy;
begin
  EndMouseTracking(Self);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TdxTileControlScrollButtonViewInfo.DoCalculate;
var
  ASizeX, ASizeY, ALeft, ATop: Integer;
begin
  ASizeX := Owner.LookAndFeelPainter.ScaledSliderButtonSize(FDirection, ScaleFactor).cx;
  ASizeY := Owner.LookAndFeelPainter.ScaledSliderButtonSize(FDirection, ScaleFactor).cy;
  DoCalculateTopLeft(ASizeX, ASizeY, ATop, ALeft);
  Bounds := cxRectBounds(ALeft, ATop, ASizeX, ASizeY);
  inherited DoCalculate;
end;

procedure TdxTileControlScrollButtonViewInfo.DoCalculateTopLeft(const ASizeX, ASizeY: Integer; var ATop, ALeft: Integer);

  procedure CalculateOffsetsAndPlacements(var AOffsetX, AOffsetY: Integer;
    var AIsNeedUseFixedIndentHorz, AIsNeedUseFixedIndentVert: Boolean);
  var
    AOwnerIndentHorz, AOwnerIndentVert, AFrameSize: Integer;
  begin
    AOffsetX := ASizeX div 2;
    AOffsetY := ASizeY div 2;
    AOwnerIndentHorz := Owner.OptionsView.IndentHorz;
    AFrameSize := Owner.ViewInfo.GetMaxPossibilityFocusItemFrameSize;
    AIsNeedUseFixedIndentHorz := Owner.OptionsView.FixedIndentHorz and (AOwnerIndentHorz > ASizeX + 2 * AFrameSize);
    if AIsNeedUseFixedIndentHorz and (AOwnerIndentHorz - ASizeX < AOffsetX) then
      AOffsetX := 2 * AFrameSize;
    AOwnerIndentVert := Owner.OptionsView.IndentVert;
    AIsNeedUseFixedIndentVert := Owner.OptionsView.FixedIndentVert and (AOwnerIndentVert > ASizeY + 2 * AFrameSize);
    if AIsNeedUseFixedIndentVert and (AOwnerIndentVert - ASizeY < AOffsetY)  then
      AOffsetY := 2 * AFrameSize;
  end;

var
  AOffsetX, AOffsetY: Integer;
  AWidth, AHeight: Integer;
  R: TRect;
  AIsNeedUseFixedIndentHorz, AIsNeedUseFixedIndentVert: Boolean;
begin
  R := ViewInfo.TilesArea;
  AWidth := cxRectWidth(R);
  AHeight := cxRectHeight(R);
  CalculateOffsetsAndPlacements(AOffsetX, AOffsetY, AIsNeedUseFixedIndentHorz, AIsNeedUseFixedIndentVert);
  case Direction of
    adUp:
      begin
        ALeft := R.Left + (AWidth - ASizeX) div 2;
        if AIsNeedUseFixedIndentVert then
          ATop := R.Top - AOffsetY - ASizeY
        else
          ATop := R.Top + AOffsetY;
      end;
    adDown:
      begin
        ALeft := R.Left + (AWidth - ASizeX) div 2;
        if AIsNeedUseFixedIndentVert then
          ATop := R.Bottom + AOffsetY
        else
          ATop := R.Bottom - AOffsetY - ASizeY;
      end;
    adLeft:
      begin
        if AIsNeedUseFixedIndentHorz then
          ALeft := R.Left - AOffsetX - ASizeX
        else
          ALeft := R.Left + AOffsetX;
        ATop := R.Top + (AHeight - ASizeY) div 2;
      end;
  else
    begin
      if AIsNeedUseFixedIndentHorz then
        ALeft := R.Right + AOffsetX
      else
        ALeft := R.Right - AOffsetX - ASizeX;
      ATop := R.Top + (AHeight - ASizeY) div 2;
    end;
  end;
end;

procedure TdxTileControlScrollButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if State <> cxbsDisabled then
    Owner.LookAndFeelPainter.DrawScaledSliderButton(ACanvas, Bounds, FDirection, State, ScaleFactor);
end;

procedure TdxTileControlScrollButtonViewInfo.DoScrollContent;
const
  AScrollBarKind: array[TcxArrowDirection] of TScrollBarKind =
    (sbVertical, sbVertical, sbHorizontal, sbHorizontal);
  AScrollCode: array[TcxArrowDirection] of TScrollCode =
    (scLineUp, scLineDown, scLineUp, scLineDown);
var
  AScrollPos: Integer;
begin
  if FDirection in [adUp, adDown] then
    AScrollPos := ViewInfo.TopScrollPos
  else
    AScrollPos := ViewInfo.LeftScrollPos;
  Owner.Scroll(AScrollBarKind[FDirection], AScrollCode[FDirection], AScrollPos);
end;

function TdxTileControlScrollButtonViewInfo.GetArea: TRect;
begin
  Result := cxRectInflate(Bounds, ScaleFactor.Apply(100));
end;

function TdxTileControlScrollButtonViewInfo.GetController: TdxTileControlController;
begin
  Result := Owner.Controller;
end;

function TdxTileControlScrollButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxTileControlScrollButtonViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := False;
  if (Owner.DragAndDropState <> ddsNone) or not ViewInfo.IsScrollAvailable(FDirection) then
  begin
    if Owner.DragAndDropState = ddsNone then
      State := cxbsDisabled;
    Exit;
  end;
  Result := PtInRect(Bounds, AHitTest.HitPoint);
  if Result then
  begin
    AHitTest.HitObject := Self;
    AHitTest.BitState[tchtScrollButton] := True;
    if Controller.PressedScrollButton = Self then
      State := cxbsPressed
    else
      State := cxbsHot;
  end
  else
    if PtInCaller(AHitTest.HitPoint) then
    begin
      AHitTest.HitObject := Self;
      AHitTest.BitState[tchtScrollButtonArea] := True;
      State := cxbsNormal;
    end;
  if AHitTest.HitObject = Self then
    BeginMouseTracking(Owner, Area, Self);
end;

function TdxTileControlScrollButtonViewInfo.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := Owner.ViewInfo;
end;

function TdxTileControlScrollButtonViewInfo.GetVisibleBounds: TRect;
begin
  Result := ViewInfo.ClientBounds;
end;

procedure TdxTileControlScrollButtonViewInfo.Invalidate;
begin
  if Visible then
    if Owner.LookAndFeelPainter is TcxStandardLookAndFeelPainter then
      Owner.InvalidateRect(cxRectInflate(Bounds, 1, 1), False)
    else
      Owner.InvalidateRect(Bounds, False);
end;

procedure TdxTileControlScrollButtonViewInfo.RefreshState;
begin
end;

procedure TdxTileControlScrollButtonViewInfo.Scroll(const DX, DY: Integer);
begin
end;

procedure TdxTileControlScrollButtonViewInfo.SetState(const AValue: TcxButtonState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
    FTimer.Enabled := FState = cxbsPressed;
  end;
end;

procedure TdxTileControlScrollButtonViewInfo.TimerHandler(Sender: TObject);
begin
  if Controller.PressedScrollButton = Self then
    DoScrollContent;
end;

// IcxMouseTrackingCaller2
procedure TdxTileControlScrollButtonViewInfo.MouseLeave;
begin
  EndMouseTracking(Self);
  if Owner.DragAndDropState = ddsNone then
    State := cxbsDisabled;
end;

function TdxTileControlScrollButtonViewInfo.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(Area, P);
end;

{ TdxTileControlDetailSiteTabsScrollButtonViewInfo }

constructor TdxTileControlDetailSiteTabsScrollButtonViewInfo.Create(AOwner: TdxTileControlDetailSiteTitleViewInfo;
  ADirection: TcxArrowDirection);
begin
  inherited Create;
  FOwner := AOwner;
  FDirection := ADirection;
  FState := cxbsNormal;
  FTimer := TcxTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerHandler;
end;

destructor TdxTileControlDetailSiteTabsScrollButtonViewInfo.Destroy;
begin
  if Self <> nil then
    EndMouseTracking(Self);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.CanClick: Boolean;
begin
  Result := State <> cxbsDisabled;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.Click;
begin
  if CanClick and not FTimerIsHandled then
    DoScrollTabs;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.DoCalculate;
var
  ASize, ALeft: Integer;
  R: TRect;
begin
  R := Owner.TabsBounds;
  R.Top := Owner.TabsTop;
  R.Bottom := Owner.TabsBottom;
  ASize := Owner.TabsBottom - Owner.TabsTop;
  if FDirection = adLeft then
    ALeft := R.Left
  else
    ALeft := R.Right - ASize;
  Bounds := cxRectBounds(ALeft, R.Top, ASize, ASize);
  inherited DoCalculate;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  Owner.TileControl.LookAndFeelPainter.DrawArrow(ACanvas, Bounds, FDirection, Owner.Painter.PageTabTextColor[State]);
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.DoScrollTabs;
var
  DX: Integer;
  ATabBounds: TRect;
begin
  ATabBounds := GetNearestTabBounds;
  if FDirection = adLeft then
    DX := Owner.TabsBounds.Left - ATabBounds.Left
  else
    DX := Owner.TabsBounds.Right - ATabBounds.Right;
  Owner.TabsLeftScrollPos := Owner.TabsLeftScrollPos + DX;
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.GetDetailSite: TdxTileControlDetailSite;
begin
  Result := Owner.Owner as TdxTileControlDetailSite;
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if not Result or (State = cxbsDisabled) then
    Exit;
  AHitTest.BitState[tchtTabScrollButton] := True;
  State := cxbsHot;
  if DetailSite.PressedCell = Self then
    State := cxbsPressed
  else
    State := cxbsHot;
  BeginMouseTracking(DetailSite, Bounds, Self);
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.GetNearestTabBounds: TRect;

  function GetNearestTabBoundsToLeftScrollButton(ATabs: TdxTileControlCells): TRect;
  var
    I: Integer;
    ATab: TdxTileControlPageTabCellViewInfo;
  begin
    Result := ATabs[0].Bounds;
    for I := ATabs.Count - 1 downto 1 do
    begin
      ATab := ATabs[I] as TdxTileControlPageTabCellViewInfo;
      if Bounds.Right - ATab.Bounds.Left > ATab.GetMarginsWidth div 2 then
      begin
        Result := ATab.Bounds;
        Break;
      end;
    end;
  end;

  function GetNearestTabBoundsToRightScrollButton(ATabs: TdxTileControlCells): TRect;
  var
    I: Integer;
    ATab: TdxTileControlPageTabCellViewInfo;
  begin
    Result := ATabs[ATabs.Count - 1].Bounds;
    for I := 0 to ATabs.Count - 2 do
    begin
      ATab := ATabs[I] as TdxTileControlPageTabCellViewInfo;
      if ATab.Bounds.Right - Bounds.Left > ATab.GetMarginsWidth div 2 then
      begin
        Result := ATab.Bounds;
        Break;
      end;
    end;
  end;

begin
  if FDirection = adLeft then
    Result := GetNearestTabBoundsToLeftScrollButton(Owner.Tabs)
  else
    Result := GetNearestTabBoundsToRightScrollButton(Owner.Tabs);
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.GetVisibleBounds: TRect;
begin
  Result := Owner.Bounds;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.Invalidate;
begin
  if Owner.TileControl.LookAndFeelPainter is TcxStandardLookAndFeelPainter then
    DetailSite.InvalidateRect(cxRectInflate(Bounds, 1, 1), False)
  else
    DetailSite.InvalidateRect(Bounds, False);
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.RefreshState;
var
  AOldState: TcxButtonState;
begin
  AOldState := State;
  if (FDirection = adLeft) and not Owner.CanScrollTabToLeft then
    State := cxbsDisabled
  else
    if (FDirection = adRight) and not Owner.CanScrollTabToRight then
      State := cxbsDisabled
    else
      if (DetailSite.PressedCell = Self) and (DetailSite.HitTest.HitObject = Self) then
        State := cxbsPressed
      else
        if (DetailSite.HitTest.HitObject = Self) and (DetailSite.PressedCell = nil) then
          State := cxbsHot
        else
          State := cxbsNormal;
  if AOldState <> State then
    FTimerIsHandled := False;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.Scroll(const DX, DY: Integer);
begin
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  if FDirection = adLeft then
    FDirection := adRight
  else
    FDirection := adLeft;
  RefreshState;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.SetState(const AValue: TcxButtonState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
    FTimer.Enabled := FState = cxbsPressed;
  end;
end;

procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.TimerHandler(Sender: TObject);
begin
  if DetailSite.PressedCell = Self then
  begin
    DoScrollTabs;
    FTimerIsHandled := True;
  end;
end;

// IcxMouseTrackingCaller2
procedure TdxTileControlDetailSiteTabsScrollButtonViewInfo.MouseLeave;
begin
  EndMouseTracking(Self);
  if State <> cxbsDisabled then
    State := cxbsNormal;
end;

function TdxTileControlDetailSiteTabsScrollButtonViewInfo.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, P);
end;

{ TdxTileControlCustomItemViewData }

constructor TdxTileControlCustomItemViewData.Create(AItem: TdxTileControlCustomItem);
begin
  inherited Create;
  FItem := AItem;
  FNeedDrawTextOnImage := True;
  IsDirty := True;
end;

destructor TdxTileControlCustomItemViewData.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxTileControlCustomItemViewData.AdjustObjectBounds(const ABounds: TRect;
  var AObjectBounds: TRect; AAlign: TdxTileItemInnerObjectAlignment;
  AIndentHorz, AIndentVert: Integer);
begin
  if AAlign in [oaTopCenter, oaMiddleCenter, oaBottomCenter] then
  begin
    AObjectBounds := cxRectSetLeft(AObjectBounds, (ABounds.Left + ABounds.Right - cxRectWidth(AObjectBounds)) div 2);
    AIndentHorz := 0;
  end
  else
    if AAlign in [oaTopRight, oaMiddleRight, oaBottomRight] then
    begin
      AObjectBounds := cxRectSetRight(AObjectBounds, ABounds.Right);
      AIndentHorz  := -AIndentHorz;
    end
    else
      AObjectBounds := cxRectSetLeft(AObjectBounds, ABounds.Left);
  if AAlign in [oaTopLeft, oaTopCenter, oaTopRight] then
    AObjectBounds := cxRectSetTop(AObjectBounds, ABounds.Top)
  else
    if AAlign in [oaMiddleLeft..oaMiddleRight] then
    begin
      AObjectBounds := cxRectSetTop(AObjectBounds,
        (ABounds.Top + ABounds.Bottom - cxRectHeight(AObjectBounds)) div 2);
       AIndentVert := 0;
    end
    else
    begin
      AObjectBounds := cxRectSetBottom(AObjectBounds, ABounds.Bottom);
      AIndentVert := -AIndentVert
    end;
  OffsetRect(AObjectBounds, AIndentHorz, AIndentVert);
end;

procedure TdxTileControlCustomItemViewData.AdjustImageBoundsWithText(const ABounds: TRect;
  AText: TdxTileControlItemText; var AImageBounds, ATextBounds: TRect);
var
  R: TRect;
  DW, DH: Integer;
  AGlyph: TdxTileControlItemCustomGlyph;
  AAlign: TdxTileControlImageWithTextAlignment;
  AImageAlign: TdxTileItemInnerObjectAlignment;
begin
  AGlyph := Item.Glyph;
  AAlign := AGlyph.AlignWithText;
  AImageAlign := AGlyph.GetActualAlign;
  R := AImageBounds;
  if AAlign in [itaLeft, itaRight] then
    Inc(R.Right, cxRectWidth(ATextBounds) + AText.IndentHorz)
  else
    Inc(R.Bottom, cxRectHeight(ATextBounds) + AText.IndentVert);
  AdjustObjectBounds(ABounds, R, AImageAlign, AGlyph.IndentHorz, AGlyph.IndentVert);
  //
  case AAlign of
    itaLeft:
    begin
      AImageBounds := cxRectSetLeft(AImageBounds, R.Left);
      ATextBounds := cxRectSetLeft(ATextBounds,  AImageBounds.Right + AText.IndentHorz);
    end;
    itaTop:
    begin
      AImageBounds := cxRectSetTop(AImageBounds, R.Top);
      ATextBounds := cxRectSetTop(ATextBounds,  AImageBounds.Bottom + AText.IndentVert);
    end;
    itaRight:
    begin
      AImageBounds := cxRectSetRight(AImageBounds, R.Right);
      ATextBounds := cxRectSetRight(ATextBounds,  AImageBounds.Left - AText.IndentHorz);
    end;
    itaBottom:
    begin
      AImageBounds := cxRectSetBottom(AImageBounds, R.Bottom);
      ATextBounds := cxRectSetBottom(ATextBounds,  AImageBounds.Top - AText.IndentVert);
    end;
  end;
   // correct image bounds for long text and center text with image bounds
  if not (AImageAlign in [oaTopCenter, oaMiddleCenter, oaBottomCenter]) then
  begin
    DW := 0;
    if cxRectWidth(ATextBounds) <= cxRectWidth(AImageBounds) then
      DW := (cxRectWidth(ATextBounds) - cxRectWidth(AImageBounds)) div 2;
    DH := 0;
    if cxRectHeight(ATextBounds) <= cxRectHeight(AImageBounds) then
      DH := (cxRectHeight(ATextBounds) - cxRectHeight(AImageBounds)) div 2;
    if (AAlign in [itaTop, itaBottom]) and (DW > 0) then
      OffsetRect(AImageBounds, DW * (Ord(AImageAlign in [oaTopLeft, oaMiddleLeft, oaBottomLeft]) * 2 - 1), 0)
    else
      if (AAlign in [itaLeft, itaRight]) and (DH > 0) then
        OffsetRect(AImageBounds, 0, DH * (Ord(AImageAlign in [oaTopLeft, oaTopCenter, oaTopRight]) * 2 - 1));
  end;
  if (AAlign in [itaTop, itaBottom]) then
  begin
    if cxRectWidth(ATextBounds) <= cxRectWidth(AImageBounds) then
      ATextBounds := cxRectSetLeft(ATextBounds, (AImageBounds.Left + AImageBounds.Right - cxRectWidth(ATextBounds)) div 2)
  end
  else
    if cxRectHeight(ATextBounds) <= cxRectHeight(AImageBounds) then
      ATextBounds := cxRectSetTop(ATextBounds, (AImageBounds.Top + AImageBounds.Bottom - cxRectHeight(ATextBounds)) div 2);
  // check clipping
  cxRectIntersect(AImageBounds, AImageBounds, ABounds);
  cxRectIntersect(ATextBounds, ATextBounds, ABounds);
end;

procedure TdxTileControlCustomItemViewData.CalculateImageAndTextLayout(const ABounds: TRect);

  function GetTextBounds(AText: TdxTileControlItemText; ANum: Integer; AGlyph: TdxTileControlItemCustomGlyph): TRect;
  var
    AImageSize: TSize;
    AImageAlign: TdxTileControlImageWithTextAlignment;
  begin
    Result := ABounds;
    Result.BottomRight := cxPointOffset(Result.BottomRight, -AText.IndentHorz, -AText.IndentVert);
    AImageAlign := AGlyph.AlignWithText;
    if (ANum = 0) and (not AGlyph.Image.Empty or Item.Glyph.IsImagesUse) and (AImageAlign <> itaNone) then
    begin
      AImageSize := cxSize(FImageBounds);
      if AImageAlign in [itaLeft, itaRight] then
        Dec(Result.Right, AImageSize.cx + AGlyph.IndentHorz)
      else
        Dec(Result.Bottom, AImageSize.cy + AGlyph.IndentVert);
    end;
  end;

var
  I: Integer;
  AText: TdxTileControlItemText;
  AGlyph: TdxTileControlItemCustomGlyph;
begin
  AGlyph := Item.Glyph;
  FImageBounds := cxRectScaleSize(AGlyph.GetClientRect(ABounds), dxDefaultDPI, AGlyph.Image.SourceDPI);
  for I := 0 to 3 do
  begin
    AText := Item.GetText(I);
    if AText.GetActualValue = '' then Continue;
    FTextBounds[I] := GetTextBounds(AText, I, AGlyph);
    AText.CalculateBounds(FTextBounds[I]);
    AdjustObjectBounds(ABounds, FTextBounds[I], AText.GetActualAlign, AText.IndentHorz, AText.IndentVert);
  end;
  if not cxRectIsEmpty(FImageBounds) then
  begin
    AdjustObjectBounds(ABounds, FImageBounds, AGlyph.GetActualAlign, AGlyph.IndentHorz, AGlyph.IndentVert);
    if (AGlyph.AlignWithText <> itaNone) and Item.GetText(0).HasValue then
      AdjustImageBoundsWithText(ABounds, Item.GetText(0), FImageBounds, FTextBounds[0]);
  end;
end;

procedure TdxTileControlCustomItemViewData.DrawItemBackground(const ACanvas: TcxCanvas);
var
  AStyle: TdxTileControlCustomStyle;
begin
  AStyle := Item.Style;
  if Item.ParentStyle then
    AStyle := TileItem.Style;
  Painter.DrawItemBackground(ACanvas, Bounds, AStyle);
end;

procedure TdxTileControlCustomItemViewData.DrawItemGlyph(const ACanvas: TcxCanvas);
var
  AGlyph: TdxTileControlItemCustomGlyph;
begin
  AGlyph := Item.Glyph;
  cxDrawImage(ACanvas.Handle, ImageBounds, Bounds, AGlyph.Image, AGlyph.GetImages, AGlyph.ImageIndex, idmNormal,
    True, 0, clNone, False);
end;

procedure TdxTileControlCustomItemViewData.DrawItemText(const ACanvas: TcxCanvas; const AIndex: Integer);
var
  R: TRect;
  AText: TdxTileControlItemText;
  ATextColor: TColor;
begin
  AText := Item.GetText(AIndex);
  if (Length(AText.GetActualValue) = 0) or not cxRectIntersect(R, FClientRect, FTextBounds[AIndex]) then
    Exit;

  ACanvas.SaveClipRegion;
  try
    if not cxRectIsEqual(R, FTextBounds[AIndex]) then
      ACanvas.SetClipRegion(TcxRegion.Create(R), roSet);
    ATextColor := cxGetActualColor(AText.GetActualFontColor, Painter.DefaultItemTextColor);
    if not AText.Transparent then
      ACanvas.FillRect(FTextBounds[AIndex], cxGetActualColor(AText.GetActualColor, Painter.DefaultItemTextBackgroundColor));
    cxTextOut(ACanvas.Canvas, AText.GetActualValue, FTextBounds[AIndex],
      AText.GetTextOutFlags, 0, 0, AText.GetActualFont, clNone, clNone, 0, 0, 0, ATextColor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxTileControlCustomItemViewData.DrawItemTexts(const ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to 3 do
    DrawItemText(ACanvas, I);
end;

procedure TdxTileControlCustomItemViewData.PrepareViewData;
var
  ADestBitmap: TcxBitmap;
  ABounds: TRect;
begin
  ABounds := TileItem.ViewInfo.GetHighlightBounds(Bounds);
  FClientRect := cxRectInflate(ABounds, cxRectInvert(Item.Margins.Margin));
  CalculateImageAndTextLayout(FClientRect);
  ADestBitmap := TcxBitmap.CreateSize(Bounds, pf24bit);
  try
    if TileItem.CanGalleryAnimation then
      GalleryCellController.DrawCells(ADestBitmap.cxCanvas, True)
    else
    begin
      DrawItemBackground(ADestBitmap.cxCanvas);
      DrawItemGlyph(ADestBitmap.cxCanvas);
    end;
    if NeedDrawTextOnImage then
      DrawItemTexts(ADestBitmap.cxCanvas);
    if FImage = nil then
      FImage := TdxGPImage.CreateFromBitmap(ADestBitmap)
    else
      FImage.Assign(ADestBitmap);
  finally
    ADestBitmap.Free;
  end;
end;

procedure TdxTileControlCustomItemViewData.ValidateViewData;
begin
  if IsDirty then
  begin
    IsDirty := False;
    if IsActive then
      TileItem.ViewInfo.Calculated := False;
  end;
end;

function TdxTileControlCustomItemViewData.GetBounds: TRect;
begin
  Result := TileItem.GetClientBounds;
end;

function TdxTileControlCustomItemViewData.GetGalleryCellController: TdxTileControlGalleryCellController;
begin
  Result := TileItem.GalleryCellController;
end;

function TdxTileControlCustomItemViewData.GetImage: TdxGPImage;
begin
  ValidateViewData;
  Result := FImage;
end;

function TdxTileControlCustomItemViewData.GetIsActive: Boolean;
begin
  Result := TileItem.ViewInfo.ViewData = Self;
end;

function TdxTileControlCustomItemViewData.GetPainter: TdxTileControlPainter;
begin
  Result := TileItem.TileControl.Painter;
end;

function TdxTileControlCustomItemViewData.GetTextBounds(AIndex: Integer): TRect;
begin
  Result := FTextBounds[AIndex]
end;

function TdxTileControlCustomItemViewData.GetTileItem: TdxTileControlItem;
begin
  Result := Item.GetTileItem;
end;

procedure TdxTileControlCustomItemViewData.SetIsDirty(AValue: Boolean);
begin
  if FIsDirty <> AValue then
  begin
    FIsDirty := AValue;
    if not IsDirty then
      PrepareViewData;
  end;
end;

procedure TdxTileControlCustomItemViewData.SetNeedDrawTextOnImage(AValue: Boolean);
begin
  if NeedDrawTextOnImage <> AValue then
  begin
    FNeedDrawTextOnImage := AValue;
    IsDirty := True;
    if IsActive then
      TileItem.ViewInfo.IsDirty := True;
  end;
end;

{ TdxTileControlDetailSiteOptionsAnimate }

constructor TdxTileControlDetailOptionsAnimate.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAnimationInterval := dxTileControlActivateDetailAnimationTime;
  FAnimationMode := damScroll;
end;

procedure TdxTileControlDetailOptionsAnimate.Assign(ASource: TPersistent);
begin
  if ASource is TdxTileControlDetailOptionsAnimate then
  begin
    FAnimationInterval := TdxTileControlDetailOptionsAnimate(ASource).AnimationInterval;
    FAnimationMode := TdxTileControlDetailOptionsAnimate(ASource).AnimationMode;
  end
  else
    inherited Assign(ASource);
end;

{ TdxTileControlCustomItemOptionsAnimate }

constructor TdxTileControlCustomItemOptionsAnimate.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAnimateText := dxTileControlDefaultAnimateText;
end;

procedure TdxTileControlCustomItemOptionsAnimate.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxTileControlCustomItemOptionsAnimate then
  begin
    AnimateText := TdxTileControlCustomItemOptionsAnimate(Source).AnimateText;
    AssignedValues := TdxTileControlCustomItemOptionsAnimate(Source).AssignedValues;
  end;
end;

function TdxTileControlCustomItemOptionsAnimate.IsAnimateTextStored: Boolean;
begin
  if ParentOptionsAnimate = nil then
    Result := FAnimateText <> dxTileControlDefaultAnimateText
  else
    Result := ioaavAnimateText in AssignedValues;
end;

function TdxTileControlCustomItemOptionsAnimate.GetAnimateText: Boolean;
begin
  if (ioaavAnimateText in AssignedValues) or (ParentOptionsAnimate = nil) then
    Result := FAnimateText
  else
    Result := ParentOptionsAnimate.AnimateText;
end;

function TdxTileControlCustomItemOptionsAnimate.GetParentOptionsAnimate: TdxTileControlCustomItemOptionsAnimate;
begin
  Result := nil;
end;

procedure TdxTileControlCustomItemOptionsAnimate.SetAnimateText(AValue: Boolean);
begin
  AssignedValues := AssignedValues + [ioaavAnimateText];
  FAnimateText := AValue;
end;

{ TdxTileControlTabsScrollingAnimation }

constructor TdxTileControlTabsScrollingAnimation.Create(AOwner: TdxTileControlDetailSiteTitleViewInfo; ATime: Cardinal;
  const ALength: Integer);
begin
  FOwner := AOwner;
  FSign := Sign(ALength);
  inherited Create(ATime, ateLinear, Abs(ALength));
end;

procedure TdxTileControlTabsScrollingAnimation.DoAnimate;
begin
  Owner.ScrollTabs(FSign * (Position - FPrevPosition));
  FPrevPosition := Position;
  DetailSite.InvalidateRect(Owner.TabsBounds, True);
  DetailSite.TileControl.ForceUpdate();
end;

function TdxTileControlTabsScrollingAnimation.GetDetailSite: TdxTileControlDetailSite;
begin
  Result := Owner.Owner as TdxTileControlDetailSite;
end;

{ TdxTileControlDetailSiteTitleViewInfo }

constructor TdxTileControlDetailSiteTitleViewInfo.Create(AOwner: TdxTileControlDetailSite);
begin
  inherited Create;
  FOwner := AOwner;
  FTabs := TdxTileControlCells.Create;
  FTabsFont := TFont.Create;
  FBackButtonViewInfo := CreateBackButtonViewInfo;
end;

destructor TdxTileControlDetailSiteTitleViewInfo.Destroy;
begin
  FreeAndNil(FBackButtonViewInfo);
  FreeAndNil(FTabsFont);
  FreeAndNil(FTabs);
  DestroyScrollTabsButtons;
  inherited Destroy;
end;

procedure TdxTileControlDetailSiteTitleViewInfo.CalculateBackButton;
var
  ABounds: TRect;
begin
  ABounds := cxRect(0, 0, BackButtonViewInfo.MeasureWidth, BackButtonViewInfo.MeasureHeight);
  ABounds := cxRectSetRight(ABounds, Max(Bounds.Left + ABounds.Right + TextOffset, GetLeftOffset - TextOffset));
  ABounds := cxRectSetTop(ABounds, (VisibleBounds.Top + VisibleBounds.Bottom - cxRectHeight(ABounds)) div 2);
  BackButtonViewInfo.SetBounds(ABounds, VisibleBounds);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.CalculateTabs;

  procedure InternalScrollTabs(const DX: Integer);
  begin
    ScrollTabs(DX);
    FTabsLeftScrollPos := FTabsLeftScrollPos + DX;
  end;

  procedure CheckTabsRightSide;
  var
    AOffset: Integer;
  begin
    AOffset := TabsBounds.Right - Tabs[Tabs.Count - 1].Bounds.Right;
    if AOffset > 0 then
      InternalScrollTabs(AOffset);
  end;

  procedure CreateTabsViewInfo(var AActiveIndex: Integer);
  var
    AItem: TdxTileControlItem;
    I: Integer;
  begin
    for I := 0 to TileControl.Items.Count - 1 do
    begin
      AItem := TileControl.Items[I];
      if AItem.ActuallyVisible and AItem.DetailOptions.ShowTab then
      begin
        FTabs.Add(CreateTabCellViewInfo(AItem));
        if AItem.IsActive then
          AActiveIndex := FTabs.Count - 1;
      end;
    end;
  end;

  function IsNeedMakeTabVisible(const ATabIndex: Integer): Boolean;
  begin
    Result := (ATabIndex > -1) and (ATabIndex < Tabs.Count) and
      Owner.ActivatingInProgress and (ScrollTabsLeftButton <> nil) and
    ((Tabs[ATabIndex].Bounds.Right > TabsBounds.Right) or
     (Tabs[ATabIndex].Bounds.Left < TabsBounds.Left));
  end;

  procedure MakeTabVisible(const ATabIndex: Integer);
  var
    ABounds: TRect;
    ALeft: Integer;
  begin
    ABounds := Tabs[ATabIndex].Bounds;
    ALeft := ABounds.Left;
    if ABounds.Right > TabsBounds.Right then
      ABounds := cxRectOffset(ABounds, TabsBounds.Right - ABounds.Right, 0);
    if ABounds.Left < TabsBounds.Left then
      ABounds := cxRectOffset(ABounds, TabsBounds.Left - ABounds.Left, 0);
    InternalScrollTabs(ABounds.Left - ALeft);
  end;

  procedure PlaceTabs(const R: TRect);
  var
    ABounds: TRect;
    ACell: TdxTileControlPageTabCellViewInfo;
    AOffset: Integer;
    I: Integer;
  begin
    FTabsBottom := R.Top;
    FTabsTop := R.Bottom;
    AOffset := TabsLeftScrollPos + R.Left;
    for I := 0 to Tabs.Count - 1 do
    begin
      ACell := Tabs[I] as TdxTileControlPageTabCellViewInfo;
      ABounds := cxRectSetBottom(R, R.Bottom, Min(cxRectHeight(R), ACell.MeasureHeight));
      ABounds := cxRectSetWidth(ABounds, AOffset, ACell.MeasureWidth);
      ACell.SetBounds(ABounds, R);
      FTabsTop := Min(ABounds.Top, FTabsTop);
      FTabsBottom := Max(ABounds.Bottom, FTabsBottom);
      AOffset := ABounds.Right + ScaleFactor.Apply(cxTextOffset);
    end;
  end;

  procedure RestorePriorTabsLeftScrollPos;
  begin
    if TileControl.ActiveDetail <> nil then
      FTabsLeftScrollPos := Min(0, TabsLeftScrollPos);
  end;

var
  AActiveIndex, ALeft, AScrollButtonWidth: Integer;
begin
  Tabs.Clear;
  DestroyScrollTabsButtons;
  AActiveIndex := -1;
  CreateTabsViewInfo(AActiveIndex);
  if Tabs.Count > 0 then
  begin
    ALeft := TextBounds.Right;
    if Title.Text <> '' then
      Inc(ALeft, TextOffset);
    FTabsBounds := cxRectSetXPos(Bounds,
      Max(ALeft, BackButtonViewInfo.Bounds.Right),
      Min(FGlyphBounds.Left, Bounds.Right - TileControl.OptionsView.IndentHorz));
    PlaceTabs(TabsBounds);
    ALeft := TabsBounds.Left;
    if CanScrollTabToLeft or CanScrollTabToRight then
    begin
      CreateScrollTabsButtons;
      AScrollButtonWidth := cxRectWidth(ScrollTabsLeftButton.Bounds);
      FTabsBounds := cxRectInflate(FTabsBounds, -(AScrollButtonWidth + ScaleFactor.Apply(cxTextOffset)), 0);
      RestorePriorTabsLeftScrollPos;
      if cxRectWidth(FTabsBounds) <= 2 * ScaleFactor.Apply(cxTextOffset) then
        DestroyScrollTabsButtons;
    end
    else
      Inc(FTabsBounds.Left, TabsBounds.Right - Tabs[Tabs.Count - 1].Bounds.Right);
    if ALeft <> TabsBounds.Left then
      PlaceTabs(TabsBounds);
    CheckTabsRightSide;
    if IsNeedMakeTabVisible(AActiveIndex) then
      MakeTabVisible(AActiveIndex);
    ScrollTabsButtonsRefreshState;
  end;
end;

function TdxTileControlDetailSiteTitleViewInfo.CanScrollTabToLeft: Boolean;
var
  AActualTab: TdxTileControlCustomCellViewInfo;
begin
  AActualTab := Tabs[0];
  if UseRightToLeftAlignment then
    AActualTab := Tabs[Tabs.Count - 1];
  Result := AActualTab.Bounds.Left < TabsBounds.Left - ScaleFactor.Apply(cxTextOffset);
end;

function TdxTileControlDetailSiteTitleViewInfo.CanScrollTabToRight: Boolean;
var
  AActualTab: TdxTileControlCustomCellViewInfo;
begin
  AActualTab := Tabs[Tabs.Count - 1];
  if UseRightToLeftAlignment then
    AActualTab := Tabs[0];
  Result := AActualTab.Bounds.Right > TabsBounds.Right + ScaleFactor.Apply(cxTextOffset);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.CheckBiDiModeAlignment;
begin
  CheckBiDiModeFlags;
  if UseRightToLeftAlignment then
    DoRightToLeftConversion(Bounds);
end;

function TdxTileControlDetailSiteTitleViewInfo.CreateBackButtonViewInfo: TdxTileControlDetailSiteBackButtonViewInfo;
begin
  Result := TdxTileControlDetailSiteBackButtonViewInfo.Create(FOwner);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.CreateScrollTabsButtons;

  function CreateButton(ADirection: TcxArrowDirection): TdxTileControlDetailSiteTabsScrollButtonViewInfo;
  begin
    Result := TdxTileControlDetailSiteTabsScrollButtonViewInfo.Create(Self, ADirection);
    Result.Recalculate;
  end;

begin
  FScrollTabsLeftButton := CreateButton(adLeft);
  FScrollTabsRightButton := CreateButton(adRight);
end;

function TdxTileControlDetailSiteTitleViewInfo.CreateTabCellViewInfo(
  AItem: TdxTileControlItem): TdxTileControlPageTabCellViewInfo;
begin
  Result := TdxTileControlPageTabCellViewInfo.Create(Self, AItem);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.DestroyScrollTabsButtons;
begin
  FreeAndNil(FScrollTabsLeftButton);
  FreeAndNil(FScrollTabsRightButton);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.DoCalculate;
begin
  inherited DoCalculate;
  CalculateBackButton;
  SetTextBounds(cxRectOffsetHorz(TextBounds, BackButtonViewInfo.Bounds.Right + TextOffset - TextBounds.Left));
  TabsFont.Assign(Title.Font);
  TabsFont.Height := dxGetFontHeightForDefaultDPI(Title.TabsFontSize);
  AdjustFontHeight(TabsFont, ScaleFactor, dxTileControlDefaultTabFontSize, Painter.PageTabDefaultFontSize);
  CalculateTabs;
  CheckBiDiModeAlignment;
end;

procedure TdxTileControlDetailSiteTitleViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  ACanvas.SaveClipRegion;
  inherited DoDraw(ACanvas);
  ACanvas.RestoreClipRegion;
  BackButtonViewInfo.Draw(ACanvas);
  DoDrawScrollTabsButtons(ACanvas);
  if Tabs.Count > 0 then
    ACanvas.SetClipRegion(TcxRegion.Create(TabsBounds), roSet);
  for I := 0 to Tabs.Count - 1 do
    Tabs[I].Draw(ACanvas);
  ACanvas.ExcludeClipRect(GetVisibleBounds);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.DoDrawScrollTabsButtons(ACanvas: TcxCanvas);
begin
  if ScrollTabsLeftButton <> nil then
    ScrollTabsLeftButton.Draw(ACanvas);
  if ScrollTabsRightButton <> nil then
    ScrollTabsRightButton.Draw(ACanvas);
end;

function TdxTileControlDetailSiteTitleViewInfo.GetHeight: Integer;
begin
  Result := Max(inherited GetHeight, BackButtonViewInfo.MeasureHeight + 2 * Title.IndentVert);
end;

function TdxTileControlDetailSiteTitleViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
  begin
    if not BackButtonViewInfo.GetHitTest(AHitTest) and not GetTabsScrollButtonsHitTest(AHitTest) and
      PtInRect(TabsBounds, AHitTest.HitPoint) then
      Tabs.CalculateHitTest(AHitTest);
  end;
end;

function TdxTileControlDetailSiteTitleViewInfo.GetItem: TdxTileControlItem;
begin
  Result := FOwner.TileItem;
end;

function TdxTileControlDetailSiteTitleViewInfo.GetTabsScrollButtonsHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := False;
  if ScrollTabsLeftButton <> nil then
    Result := ScrollTabsLeftButton.GetHitTest(AHitTest);
  if not Result and (ScrollTabsRightButton <> nil) then
    Result := ScrollTabsRightButton.GetHitTest(AHitTest);
end;

function TdxTileControlDetailSiteTitleViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Item.TileControl;
end;

function TdxTileControlDetailSiteTitleViewInfo.GetVisibleBounds: TRect;
begin
  Result := inherited GetVisibleBounds;
  Result.Bottom := Max(Result.Bottom,
    Result.Top + BackButtonViewInfo.MeasureHeight + Title.IndentVert * 2);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.RefreshState;
begin
  inherited RefreshState;
  BackButtonViewInfo.RefreshState;
  Tabs.RefreshState;
  ScrollTabsButtonsRefreshState;
end;

procedure TdxTileControlDetailSiteTitleViewInfo.ScrollTabs(const DX: Integer);
var
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
    Tabs[I].Scroll(DX, 0);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.ScrollTabsButtonsRefreshState;
begin
  if ScrollTabsLeftButton <> nil then
    ScrollTabsLeftButton.RefreshState;
  if ScrollTabsRightButton <> nil then
    ScrollTabsRightButton.RefreshState;
end;

procedure TdxTileControlDetailSiteTitleViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FBackButtonViewInfo.DoRightToLeftConversion(AClientBounds);
  FTabsBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTabsBounds, AClientBounds);
  for I := 0 to FTabs.Count - 1 do
    FTabs[I].DoRightToLeftConversion(AClientBounds);
  if ScrollTabsLeftButton <> nil then
    ScrollTabsLeftButton.DoRightToLeftConversion(AClientBounds);
  if ScrollTabsRightButton <> nil then
    ScrollTabsRightButton.DoRightToLeftConversion(AClientBounds);
end;

procedure TdxTileControlDetailSiteTitleViewInfo.SetTabsLeftScrollPos(AValue: Integer);
begin
  if FTabsLeftScrollPos <> AValue then
  begin
    TileControl.Controller.ImmediateAnimation(TdxTileControlTabsScrollingAnimation.Create(Self,
      dxTileControlTabScrollAnimationTime, AValue - FTabsLeftScrollPos));
    FTabsLeftScrollPos := AValue;
    ScrollTabsButtonsRefreshState;
  end;
end;

{ TdxTileControlCustomPopupControl }

constructor TdxTileControlCustomPopupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubClasses;
  LookAndFeel.MasterLookAndFeel := TileControl.LookAndFeel;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  TabStop := False;
end;

destructor TdxTileControlCustomPopupControl.Destroy;
begin
  EndMouseTracking(Self);
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxTileControlCustomPopupControl.BoundsChanged;
begin
  inherited BoundsChanged;
  RecalculateSubClasses;
  if ActiveControl <> nil then
    ActiveControl.Realign;
end;

procedure TdxTileControlCustomPopupControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    ExStyle := WS_EX_CONTROLPARENT;
    WindowClass.Style := CS_DBLCLKS;
  end;
end;

procedure TdxTileControlCustomPopupControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  HitTest.Calculate(X, Y);
end;

procedure TdxTileControlCustomPopupControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  BeginMouseTracking(Self, ClientBounds, Self);
  HitTest.Calculate(X, Y);
end;

procedure TdxTileControlCustomPopupControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  HitTest.Calculate(X, Y);
end;

function TdxTileControlCustomPopupControl.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(ClientRect, P);
  if Result and (ActiveControl <> nil) then
    Result := not PtInRect(ActiveControl.BoundsRect, P);
end;

procedure TdxTileControlCustomPopupControl.ReallyMouseLeave;
var
  P: TPoint;
begin
  EndMouseTracking(Self);
  P := TileControl.ScreenToClient(GetMouseCursorPos);
  HitTest.Calculate(P.X, P.Y);
end;

procedure TdxTileControlCustomPopupControl.ExecuteActivating;
begin
  FActivatingInProgress := True;
  try
    DoActivate;
  finally
    FActivatingInProgress := False;
  end;
end;

function TdxTileControlCustomPopupControl.GetActiveControl: TWinControl;
begin
  Result := nil;
end;

function TdxTileControlCustomPopupControl.GetHitTest: TdxTileControlHitTest;
begin
  Result := nil;
end;

function TdxTileControlCustomPopupControl.GetTileControl: TdxCustomTileControl;
begin
  Result := nil;
end;

{ TdxTileControlDetailSite }

procedure TdxTileControlDetailSite.CreateSubClasses;
begin
  FTitleViewInfo := GetTitleViewInfoClass.Create(Self);
  FTitleViewInfo.FTitle := TileControl.Title;
end;

procedure TdxTileControlDetailSite.DestroySubClasses;
begin
  FreeAndNil(FTitleViewInfo);
end;

procedure TdxTileControlDetailSite.DoActivate;
begin
  if (TileControl.ActiveDetail = nil) or (TileControl.ActiveDetail.ActiveControl <> Self.ActiveControl) then
  begin
    TileItem.DoActivateDetail(Self);
    TileControl.DoActivateDetail(Self);
  end;
end;

procedure TdxTileControlDetailSite.DoDeactivate;
begin
  TileControl.DoDeactivateDetail(Self);
end;

procedure TdxTileControlDetailSite.DoPaint;
begin
  inherited DoPaint;
  TileControl.Painter.DrawBackground(Canvas, ClientBounds);
  if TitleViewInfo.Visible then
    TitleViewInfo.Draw(Canvas);
end;

function TdxTileControlDetailSite.GetActiveControl: TWinControl;
begin
  Result := TileItem.DetailOptions.DetailControl;
end;

function TdxTileControlDetailSite.GetHitTest: TdxTileControlHitTest;
begin
  Result := TileControl.HitTest;
end;

function TdxTileControlDetailSite.GetIndex: Integer;
begin
  Result := (UInt32(TileItem.GroupIndex) shl 16) or UInt32(TileItem.IndexInGroup);
end;

function TdxTileControlDetailSite.GetTileControl: TdxCustomTileControl;
begin
  Result := TileItem.TileControl;
end;

function TdxTileControlDetailSite.GetTitleViewInfoClass: TdxTileControlDetailSiteTitleViewInfoClass;
begin
  Result := TdxTileControlDetailSiteTitleViewInfo;
end;

function TdxTileControlDetailSite.GetTileItem: TdxTileControlItem;
begin
  Result := GetOwner as TdxTileControlItem;
end;

procedure TdxTileControlDetailSite.InitializeSite(ALeft, ATop: Integer);
var
  R: TRect;
  AIndentHorz, AIndentVert: Integer;
begin
  Align := alNone;
  Parent := TileControl;
  SetBounds(0, 0, 0, 0);
  SetBounds(ALeft, 0, TileControl.Width, TileControl.Height);
  TitleViewInfo.Recalculate;
  if ActiveControl <> nil then
  begin
    ActiveControl.Align := alNone;
    ActiveControl.Anchors := [akLeft..akBottom];
  end;
  AIndentHorz := TileControl.OptionsView.IndentHorz;
  AIndentVert := TileControl.OptionsView.IndentVert;
  R := cxRectInflate(TileControl.ViewInfo.ClientBounds,
    -AIndentHorz, -TitleViewInfo.Bounds.Bottom -AIndentVert, -AIndentHorz, -AIndentVert);
  if ActiveControl <> nil then
  begin
    ActiveControl.BoundsRect := R;
    ActiveControl.Visible := True;
  end;
  Visible := True;
  BringToFront;
  Realign;
end;

procedure TdxTileControlDetailSite.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if HitTest.HitObject is TdxTileControlCustomCellViewInfo then
    PressedCell := TdxTileControlCustomCellViewInfo(HitTest.HitObject)
  else
    PressedCell := nil;
end;

procedure TdxTileControlDetailSite.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbRight then
    TileControl.Controller.ToggleActionBars(abvcrMouseRightClick)
  else
    TileControl.Controller.HideActionBars(dxMouseButtonToActionBarVisibilityChangeReason[Button]);

  if (PressedCell = HitTest.HitObject) and (Button = mbLeft) then
  begin
    if HitTest.HitAtPageTab then
      (HitTest.HitObject as TdxTileControlPageTabCellViewInfo).Item.Click
    else
      if HitTest.HitAtBackButton then
        TileControl.DoDeactivateDetail(Self)
      else
        if PressedCell is TdxTileControlDetailSiteTabsScrollButtonViewInfo then
          TdxTileControlDetailSiteTabsScrollButtonViewInfo(PressedCell).Click;
    HitTest.HitObject := nil;
  end;
  PressedCell := nil;
end;

procedure TdxTileControlDetailSite.RecalculateSubClasses;
begin
  TitleViewInfo.Recalculate;
end;

procedure TdxTileControlDetailSite.SetPressedCell(AValue: TdxTileControlCustomCellViewInfo);
begin
  if AValue <> FPressedCell then
  begin
    FPressedCell := AValue;
    TitleViewInfo.RefreshState;
  end;
end;

function TdxTileControlDetailSite.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbHorizontal;
end;

procedure TdxTileControlDetailSite.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  ADeltaX := GetCheckedDeltaX(ADeltaX);
  if ADeltaX <> 0 then
  begin
    TitleViewInfo.ScrollTabs(ADeltaX);
    InvalidateRect(TitleViewInfo.TabsBounds, True);
    TitleViewInfo.ScrollTabsButtonsRefreshState;
    TileControl.ForceUpdate();
  end;
end;

function TdxTileControlDetailSite.GetCheckedDeltaX(const ADelta: Integer): Integer;
begin
  if ADelta > 0 then
    Result := Min(ADelta, TitleViewInfo.TabsBounds.Left - TitleViewInfo.Tabs[0].Bounds.Left)
  else
    Result := Max(ADelta, TitleViewInfo.TabsBounds.Right - TitleViewInfo.Tabs[TitleViewInfo.Tabs.Count - 1].Bounds.Right);
end;

function TdxTileControlDetailSite.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(TitleViewInfo.TabsBounds, APoint);
end;

{ TdxTileControlCheckedItems }

function TdxTileControlCheckedItems.GetItem(Index: Integer): TdxTileControlItem;
begin
  Result := TdxTileControlItem(inherited Items[Index]);
end;

procedure TdxTileControlCheckedItems.SortByIndexInGroupBeforeDrag;
var
  I, P: Integer;
begin
  for P := 1 to Count - 1 do
    for I := 0 to Count - 1 - P do
      if Items[I].IndexInGroupBeforeDrag > Items[I + 1].IndexInGroupBeforeDrag then
        Exchange(I, I + 1);
end;

{ TdxTileControlItemText }

constructor TdxTileControlItemText.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FColor := clDefault;
  FFont := TFont.Create;
  ResetFont;
  FTextColor := clDefault;
  FDefaultAlign := oaTopLeft;
  FTransparent := True;
  FIndentHorz := dxTileItemObjectDefaultIndent;
  FIndentVert := dxTileItemObjectDefaultIndent;
end;

destructor TdxTileControlItemText.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxTileControlItemText.Assign(ASource: TPersistent);
var
  ASourceText: TdxTileControlItemText;
begin
  if ASource is TdxTileControlItemText then
  begin
    ASourceText := TdxTileControlItemText(ASource);
    Align := ASourceText.Align;
    IndentHorz := ASourceText.IndentHorz;
    IndentVert := ASourceText.IndentVert;
    Value := ASourceText.Value;
    Font := ASourceText.Font;
    Transparent := ASourceText.Transparent;
    TextColor := ASourceText.TextColor;
    Color := ASourceText.Color;
    AssignedValues := ASourceText.AssignedValues;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileControlItemText.CalculateBounds(var ABounds: TRect);
var
  DC: HDC;
  AFontHandle: HFONT;
  ATextRows: TcxTextRows;
  ATextParams: TcxTextParams;
  ALineCount: Integer;
begin
  if not HasValue then
  begin
    ABounds := cxNullRect;
    Exit;
  end;
  DC := GetDC(0);
  try
    AFontHandle := SelectObject(DC, GetActualFont.Handle);
    ABounds := cxRectInflate(ABounds, -Owner.TileItem.TileControl.ScaleFactor.Apply(cxTextSpace));
    ATextParams := cxCalcTextParams(DC, GetTextOutFlags or CXTO_CALCROWCOUNT);
    cxMakeTextRows(DC, PChar(GetActualValue), Length(GetActualValue), ABounds, ATextParams, ATextRows, ALineCount);
    SelectObject(DC, AFontHandle);
    ALineCount := Min(ALineCount, (ABounds.Bottom - ABounds.Top) div ATextParams.FullRowHeight);
    ABounds.Bottom := ABounds.Top + ALineCount * ATextParams.FullRowHeight;
    ABounds.Right := Min(ABounds.Right, ABounds.Left + cxGetLongestTextRowWidth(ATextRows, ALineCount));
    cxResetTextRows(ATextRows);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TdxTileControlItemText.ChangeScale(M: Integer; D: Integer);
var
  APrevAssignedValues: TdxTileItemTextAssignedValues;
begin
  inherited ChangeScale(M, D);
  APrevAssignedValues := FAssignedValues;
  Font.Height := MulDiv(Font.Height, M, D);
  FAssignedValues := APrevAssignedValues;
  FIndentHorz := MulDiv(FIndentHorz, M, D);
  FIndentVert := MulDiv(FIndentVert, M, D);
end;

procedure TdxTileControlItemText.FontChanged(Sender: TObject);
begin
  FAssignedValues := FAssignedValues + [avFont];
  Changed;
end;

function TdxTileControlItemText.GetActualAlign: TdxTileItemInnerObjectAlignment;
begin
  Result := Align;
  if Result = oaDefault then
    Result := FDefaultAlign;
  Result := dxGetBiDiModeTileItemInnerObjectAlignment(Result, Owner.TileItem.UseRightToLeftAlignment);
end;

function TdxTileControlItemText.GetActualColor: TColor;
begin
  Result := Color;
  if not (avColor in AssignedValues) then
    Result := Owner.TileItem.TileControl.Painter.DefaultItemTextBackgroundColor;
end;

function TdxTileControlItemText.GetActualFont: TFont;
begin
  if avFont in AssignedValues then
    Result := Font
  else
    Result := Owner.Style.Font;
end;

function TdxTileControlItemText.GetActualFontColor: TColor;
begin
  Result := TextColor;
  if not (avTextColor in AssignedValues) then
    Result := GetActualFont.Color;
end;

function TdxTileControlItemText.GetActualValue: string;
begin
  Result := Value;
  if (Result = '') and Owner.TileItem.TileControl.IsDesigning and (FDefaultAlign = oaTopLeft) then
    Result := Owner.TileItem.Name;
end;

function TdxTileControlItemText.GetTextOutFlags: Integer;
const
  AFlags: array[TAlignment] of Integer =
   (CXTO_LEFT or CXTO_TOP,
    CXTO_RIGHT or CXTO_TOP,
    CXTO_CENTER_HORIZONTALLY or CXTO_TOP);
var
  AAlignment: TAlignment;
begin
  AAlignment := Alignment;
  if Owner.TileItem.TileControl.UseRightToLeftReading then
    ChangeBiDiModeAlignment(AAlignment);
  Result := AFlags[AAlignment] or CXTO_EXCELSTYLE or CXTO_END_ELLIPSIS or CXTO_EDITCONTROL;
  if WordWrap then
    Result := Result or CXTO_WORDBREAK;
  if Owner.TileItem.TileControl.UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING or CXTO_EXPANDTABS;
end;

function TdxTileControlItemText.HasValue: Boolean;
begin
  Result := (GetActualValue <> '') and (GetActualValue[1] <> #0);
end;

function TdxTileControlItemText.IsFontStored: Boolean;
begin
  Result := avFont in AssignedValues;
end;

procedure TdxTileControlItemText.ResetFont;
begin
  FFont.OnChange := nil;
  FFont.Name := DefaultFontName;
  FFont.Height := dxGetFontHeightForDefaultDPI(10);
  FFont.Color := clDefault;
  FFont.OnChange := FontChanged;
end;

procedure TdxTileControlItemText.SetAlign(AValue: TdxTileItemInnerObjectAlignment);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetAlignment(AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetAssignedValues(AValue: TdxTileItemTextAssignedValues);
begin
  if FAssignedValues <> AValue then
  begin
    FAssignedValues := AValue;
    if not (avColor in FAssignedValues) then
      FColor := clDefault;
    if not (avTextColor in FAssignedValues) then
      FTextColor := clDefault;
    if not (avFont in FAssignedValues) then
      ResetFont;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    FAssignedValues := FAssignedValues + [avColor];
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxTileControlItemText.SetIndentHorz(AValue: Integer);
begin
  if FIndentHorz <> AValue then
  begin
    FIndentHorz := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetIndentVert(AValue: Integer);
begin
  if FIndentVert <> AValue then
  begin
    FIndentVert := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetTextColor(AValue: TColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    FAssignedValues := FAssignedValues + [avTextColor];
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetValue(const AValue: string);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetTransparent(AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    Changed;
  end;
end;

procedure TdxTileControlItemText.SetWordWrap(AValue: Boolean);
begin
  if FWordWrap <> AValue then
  begin
    FWordWrap := AValue;
    Changed;
  end;
end;

{ TdxCustomTileControl }

constructor TdxCustomTileControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Keys := [kArrows, kChars];
  FLockCount := 0;
  FChanges := [];
  Height := 300;
  Width := 400;
  InitializeAlign;
  CreateSubClasses;
end;

destructor TdxCustomTileControl.Destroy;
begin
  CancelMouseOperations;
  ActiveDetail := nil;
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxCustomTileControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomTileControl.CancelUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    CheckChanges;
end;

function TdxCustomTileControl.CreateGroup: TdxTileControlGroup;
begin
  BeginUpdate;
  try
    Result := Groups.Add;
  finally
    EndUpdate;
  end;
  Result.MakeVisible;
end;

function TdxCustomTileControl.CreateItem(AIsLarge: Boolean = False; AGroup: TdxTileControlGroup = nil): TdxTileControlItem;
begin
  if AIsLarge then
    Result := CreateItem(tcisLarge, AGroup)
  else
    Result := CreateItem(tcisRegular, AGroup);
end;

function TdxCustomTileControl.CreateItem(ASize: TdxTileControlItemSize; AGroup: TdxTileControlGroup = nil): TdxTileControlItem;
begin
  BeginUpdate;
  try
    Result := Items.Add;
    Result.Size := ASize;
    if AGroup = nil then
    begin
      AGroup := GetLastVisibleGroup;
      if AGroup = nil then
        AGroup := CreateGroup;
    end;
    Result.Group := AGroup;
  finally
    EndUpdate;
  end;
  Result.MakeVisible;
end;

procedure TdxCustomTileControl.DblClick;
begin
  if (Controller.DesignHelper <> nil) and Controller.HitAtItem then
    Controller.DesignHelper.CreateItemClickHandler(HitTest.Item)
  else
    inherited DblClick;
end;

procedure TdxCustomTileControl.DeleteItem(AItem: TdxTileControlItem);
var
  AGroup: TdxTileControlGroup;
begin
  if AItem = nil then Exit;
  BeginUpdate;
  try
    AGroup := AItem.Group;
    if AGroup <> nil then
    begin
      AGroup.DeleteItem(AItem);
      RemoveGroupIfEmpty(AGroup);
    end
    else
      FreeAndNil(AItem);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTileControl.EndUpdate;
begin
  Dec(FLockCount);
  LayoutChanged;
end;

function TdxCustomTileControl.GetActionBarBottomBounds: TRect;
begin
  Result := cxRectSetBottom(ClientRect, ClientRect.Bottom, ActionBarBottom.ViewInfo.MeasureHeight);
end;

function TdxCustomTileControl.GetActionBarTopBounds: TRect;
begin
  Result := cxRectSetHeight(ClientRect, ActionBarTop.ViewInfo.MeasureHeight);
end;

function TdxCustomTileControl.GetCheckedItem(Index: Integer): TdxTileControlItem;
begin
  Result := TdxTileControlItem(FCheckedItems[Index]);
end;

function TdxCustomTileControl.GetCheckedItemCount: Integer;
begin
  Result := FCheckedItems.Count;
end;

procedure TdxCustomTileControl.GetChildren(Proc: TGetChildProc; Root: TComponent);

  procedure ProcessCollection(ACollection: TcxComponentCollection);
  var
    I: Integer;
  begin
    for I := 0 to ACollection.Count - 1 do
    begin
      if ACollection[I].Owner = Root then
        Proc(ACollection[I]);
    end;
  end;

begin
  inherited GetChildren(Proc, Root);
  ProcessCollection(ActionBars.Items);
  ProcessCollection(Groups);
  ProcessCollection(Items);
end;

function TdxCustomTileControl.GetLastVisibleGroup: TdxTileControlGroup;
var
  I: Integer;
begin
  Result := nil;
  if Groups.Count = 0 then Exit;
  begin
    I := Groups.Count - 1;
    while (I >= 0) and (Result = nil) do
    begin
      Result := Groups.Items[I];
      if not Result.Visible then
        Result := nil;
      Dec(I);
    end;
  end
end;

function TdxCustomTileControl.IsMouseInPressedArea(X, Y: Integer): Boolean;
begin
  Result :=(Abs(X - MouseDownPos.X) < 2) and (Abs(Y - MouseDownPos.Y) < 2);
end;

procedure TdxCustomTileControl.AfterActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite);
begin
  if AActiveDetail <> nil then
  begin
    AActiveDetail.Parent := nil;
    AActiveDetail.RemoveFreeNotification(Self);
  end;
  AssignActiveDetail(ANewDetail);
  if ANewDetail <> nil then
  begin
    ANewDetail.FreeNotification(Self);
//    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    ANewDetail.BorderStyle := BorderStyle;
    ANewDetail.Align := alClient;
    ANewDetail.BringToFront;
    if ANewDetail.ActiveControl <> nil then
      if ANewDetail.ActiveControl.CanFocus then
        ANewDetail.ActiveControl.SetFocus
      else
    else
      if ANewDetail.CanFocus then
        ANewDetail.SetFocus;
//    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    ANewDetail.InvalidateWithChildren;
  end
  else
  begin
    LayoutChanged;
    Controller.StartItemContentAnimation;
    if CanFocus then
      SetFocus;
  end;
  Controller.RefreshItems;
  RefreshActionBars;
end;

procedure TdxCustomTileControl.AnimationHandler(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  Refresh;
  if AFinished then
  begin
    FAnimation := nil;
    FDetailsAnimationInProcess := False;
  end;
end;

procedure TdxCustomTileControl.AssignActiveDetail(ANewDetail: TdxTileControlDetailSite);
begin
  FActiveDetail := ANewDetail;
end;

procedure TdxCustomTileControl.BeforeActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite);

  function GetIndex(AValue: TdxTileControlDetailSite): Integer;
  begin
    Result := -1;
    if AValue <> nil then
      Result := AValue.Index;
  end;

  function GetAnimationMode: TdxDrawAnimationMode;
  begin
    case OptionsDetailAnimate.AnimationMode of
      damFade:
        Result := amFade;
      damSegmentedFade:
        Result := amSegmentedFade;
      damRandomSegmentedFade:
        Result := amRandomSegmentedFade;
      damScrollFade:
        Result := GetAnimationScrollFadeMode(GetIndex(AActiveDetail), GetIndex(ANewDetail));
    else
      Result := GetAnimationScrollMode(GetIndex(AActiveDetail), GetIndex(ANewDetail));
    end;
  end;

  function CreateImage(AValue: TdxTileControlDetailSite): TcxBitmap32;
  var
    AArea: TRect;
  begin
    AArea := ViewInfo.DetailSiteArea;
    Result := TcxBitmap32.CreateSize(AArea);
    if AValue <> nil then
      cxPaintTo(AValue, Result.cxCanvas, cxNullPoint, Rect(0, 0, AValue.Width, AValue.Height))
    else
      ViewInfo.DrawTileControlPart(Result, AArea);
    Result.MakeOpaque;
  end;

var
  AStartImage, AFinishImage: TcxBitmap32;
  AActualInterval: Integer;
begin
  FDetailsAnimationInProcess := True;
  if NeedStopAnimationsBeforeActiveDetailChanging(AActiveDetail) then
    Controller.StopAnimations;
  HitTest.Clear;
  Repaint;
  if AActiveDetail <> nil then
  begin
    AActiveDetail.TileItem.FIsActive := False;
    AActiveDetail.RecalculateSubClasses;
    AActiveDetail.BorderStyle := cxcbsNone;
  end;
  if ANewDetail <> nil then
  begin
    ANewDetail.TileItem.FIsActive := True;
    ANewDetail.RecalculateSubClasses;
    ANewDetail.InitializeSite(Width, Height);
  end;
  AStartImage := CreateImage(AActiveDetail);
  try
    AFinishImage := CreateImage(ANewDetail);
    try
      if AActiveDetail <> nil then
        AActiveDetail.Align := alNone;
      AActualInterval := OptionsDetailAnimate.AnimationInterval;
      if not CanFocus then
        AActualInterval := 0;
      FAnimation := TdxImageAnimationTransition.Create(AStartImage, AFinishImage, AActualInterval, GetAnimationMode);
      FAnimation.OnAnimate := AnimationHandler;
    finally
      AFinishImage.Free;
    end;
  finally
    AStartImage.Free;
  end;
end;

procedure TdxCustomTileControl.CMBiDiModeChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Items.Count - 1 do
    Items[I].NotifyBiDiModeChanged;
end;

function TdxCustomTileControl.GetActiveHitTest: TdxTileControlHitTest;

  function CheckActionBarHitTest(AActionBar: TdxTileControlCustomActionBar): TdxTileControlHitTest;
  const
    IndexMap: array[Boolean] of Integer = (tchtActionBarBottom, tchtActionBarTop);
  begin
    Result := HitTest;
    AActionBar.HitTest.HitPoint := AActionBar.ScreenToClient(ActiveHitTestPos);
    if AActionBar.HitTest.HitObject <> nil then
    begin
      Result := AActionBar.HitTest;
      Result.BitState[IndexMap[AActionBar = ActionBarTop]] := True;
    end;
  end;

begin
  FActiveHitTestPos := GetMouseCursorPos;
  Result := HitTest;
  if IsActionBarTopVisible then
    Result := CheckActionBarHitTest(ActionBarTop);
  if (Result = HitTest) and IsActionBarBottomVisible then
    Result := CheckActionBarHitTest(ActionBarBottom);
  if (Result = HitTest) and (ActiveDetail <> nil) and ActiveDetailOccupiesAllTileControl then
    Result := ActiveDetail.HitTest;
end;

function TdxCustomTileControl.GetAnimationScrollFadeMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode;
const
  AMode: array[Boolean] of TdxDrawAnimationMode = (amScrollLeftFade, amScrollRightFade);
begin
  Result := AMode[GetAnimationScrollMode(AIndex1, AIndex2) in [amScrollDown, amScrollRight]];
end;

function TdxCustomTileControl.GetAnimationScrollMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode;
var
  ASign: TValueSign;
begin
  ASign := Sign(AIndex1 - AIndex2);
  if UseRightToLeftAlignment then
    ASign := Sign(AIndex2 - AIndex1);
  Result := TdxDrawAnimationMode(ASign + 1);
end;

function TdxCustomTileControl.GetDraggedGroup: TdxTileControlGroup;
begin
  if IsGroupDragged then
    Result := TdxTileControlDragDropGroup(DragAndDropObject).DragGroup
  else
    Result := nil;
end;

function TdxCustomTileControl.GetDraggedItem: TdxTileControlItem;
begin
  if IsItemDragged then
    Result := TdxTileControlDragDropItem(DragAndDropObject).DragItem
  else
    Result := nil;
end;

function TdxCustomTileControl.GetIsLocked: Boolean;
begin
  Result := IsLoading or IsDestroying;
  if not Result then
    Result := FLockCount <> 0;
end;

procedure TdxCustomTileControl.InitializeAlign;
begin
  Align := alClient; // by design must accept all content area
end;

procedure TdxCustomTileControl.InitScrollBarsParameters;
var
  AMin: Integer;
begin
  if ([csLoading, csDestroying, csUpdating, csReading] * ComponentState <> []) or (ViewInfo = nil) then
    Exit;

  AMin := 0;
  if Controller.IsHandScrolling(sbHorizontal) then
    AMin := Min(0, ViewInfo.LeftScrollPos);
  SetScrollBarInfo(sbHorizontal, AMin, ViewInfo.ContentWidth - 1 + AMin,
    GetScrollStep, ViewInfo.HorzScrollPage, ViewInfo.LeftScrollPos,
   (OptionsBehavior.ScrollMode = smScrollbars), True);

  AMin := 0;
  if Controller.IsHandScrolling(sbVertical) then
    AMin := Min(0, ViewInfo.TopScrollPos);
  SetScrollBarInfo(sbVertical, AMin, ViewInfo.ContentHeight - 1 + AMin,
    GetScrollStep, ViewInfo.VertScrollPage, ViewInfo.TopScrollPos,
   (OptionsBehavior.ScrollMode = smScrollbars), True);
end;

function TdxCustomTileControl.InsertGroup(AIndex: Integer): TdxTileControlGroup;
begin
  BeginUpdate;
  try
    Result := Groups.Insert(AIndex);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTileControl.ImagesChanged(Sender: TObject);
begin
  Include(FChanges, tccItems);
  CheckChanges;
end;

procedure TdxCustomTileControl.ImagesFreeNotification(AComponent: TComponent);
begin
  Images := nil;
end;

procedure TdxCustomTileControl.InvalidateChanges;
begin
  if ActiveDetail <> nil then
    ActiveDetail.Invalidate
  else
    Invalidate;
end;

function TdxCustomTileControl.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxCustomTileControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomTileControl.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxCustomTileControl.CanDeactivateDetail(ADetail: TdxTileControlDetailSite): Boolean;
var
  AItem: TdxTileControlItem;
begin
  Result := True;
  if ADetail = nil then
    Exit;
  AItem := ADetail.TileItem;
  if Assigned(OnItemDeactivatingDetail) then
    OnItemDeactivatingDetail(Self, AItem, Result);
  if Result and Assigned(AItem.OnDeactivatingDetail) then
    AItem.OnDeactivatingDetail(Self, AItem, Result);
end;

function TdxCustomTileControl.DoActionBarsHide(AReason: TdxTileControlActionBarVisibilityChangeReason): Boolean;
begin
  Result := False;
  if Assigned(OnActionBarsHide) then
    OnActionBarsHide(Self, AReason, Result);
end;

function TdxCustomTileControl.DoActionBarsShow(AReason: TdxTileControlActionBarVisibilityChangeReason): Boolean;
begin
  Result := False;
  if Assigned(OnActionBarsShow) then
    OnActionBarsShow(Self, AReason, Result);
end;

procedure TdxCustomTileControl.DoActivateDetail(ADetail: TdxTileControlDetailSite);
begin
  ActiveDetail := ADetail;
end;

procedure TdxCustomTileControl.DoItemCheck(AItem: TdxTileControlItem);
begin
  if Assigned(OnItemCheck) then
    OnItemCheck(Self, AItem);
end;

function TdxCustomTileControl.DoItemBeforeCheck(AItem: TdxTileControlItem): Boolean;
begin
  Result := True;
  if Assigned(OnItemBeforeCheck) then
    OnItemBeforeCheck(Self, AItem, Result);
end;

procedure TdxCustomTileControl.DoCancelMode;
begin
  inherited DoCancelMode;
  Controller.RefreshItems;
end;

procedure TdxCustomTileControl.DoDeactivateDetail(ADetail: TdxTileControlDetailSite);
begin
  if CanDeactivateDetail(ADetail) then
    ExecuteDetailDeactivating(ADetail);
end;

procedure TdxCustomTileControl.DoGroupDragBegin(var AAllow: Boolean);
begin
  InitializeDragGroupInfo;
  if Assigned(FOnGroupDragBegin) then
    FOnGroupDragBegin(Self, DragGroupInfo, AAllow);
end;

procedure TdxCustomTileControl.DoGroupDragEnd;
begin
  if Assigned(FOnGroupDragEnd) then
    FOnGroupDragEnd(Self, DragGroupInfo);
end;

procedure TdxCustomTileControl.DoGroupDragOver(var AAccept: Boolean);
begin
  if Assigned(FOnGroupDragOver) then
    FOnGroupDragOver(Self, DragGroupInfo, AAccept);
end;

procedure TdxCustomTileControl.DoInitStoredObject(AObject: TObject);
begin
  if (AObject <> nil) and Assigned(OnInitStoredObject) then
    FOnInitStoredObject(Self, AObject);
end;

procedure TdxCustomTileControl.DoItemDragBegin(var AAllow: Boolean);
begin
  InitializeDragItemInfo;
  if Assigned(FOnItemDragBegin) then
    FOnItemDragBegin(Self, DragItemInfo, AAllow);
end;

procedure TdxCustomTileControl.DoItemDragEnd;
begin
  if Assigned(FOnItemDragEnd) then
    FOnItemDragEnd(Self, DragItemInfo);
end;

procedure TdxCustomTileControl.DoItemDragOver(var AAccept: Boolean);
begin
  if Assigned(FOnItemDragOver) then
    FOnItemDragOver(Self, DragItemInfo, AAccept);
end;

procedure TdxCustomTileControl.DoCustomPaint;
begin
  if Animation <> nil then
    Animation.Draw(Canvas.Canvas, ViewInfo.DetailSiteArea)
  else
    if ActiveDetail = nil then
      ViewInfo.Draw(Canvas);
end;

procedure TdxCustomTileControl.DoPaint;
begin
  inherited DoPaint;
  DoCustomPaint;
end;

procedure TdxCustomTileControl.ExecuteDetailDeactivating(ADetail: TdxTileControlDetailSite);
begin
  ActiveDetail := nil;
end;

function TdxCustomTileControl.IsActionBarTopVisible: Boolean;
begin
  Result := (ActionBarTop <> nil) and ActionBarTop.Visible;
end;

function TdxCustomTileControl.IsActionBarBottomVisible: Boolean;
begin
  Result := (ActionBarBottom <> nil) and ActionBarBottom.Visible;
end;

procedure TdxCustomTileControl.DoHideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  if IsAnyActionBarVisible and not DoActionBarsHide(AReason) then
  begin
    TdxTileControlActionsBarHelper.Unregister(Self);
    TdxTileControlActionBarsAnimation.Create(Self, tcabamHide).ImmediateAnimation;
  end;
end;

procedure TdxCustomTileControl.DoShowActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  if not IsAnyActionBarVisible and not DoActionBarsShow(AReason) then
  begin
    if ActionBarTop = nil then
    begin
      FActionBarTop := CreateActionBarTop;
      FActionBarTop.Parent := Self;
    end;
    if ActionBarBottom = nil then
    begin
      FActionBarBottom := CreateActionBarBottom;
      FActionBarBottom.Parent := Self;
    end;
    BringInternalControlsToFront;
    TdxTileControlActionBarsAnimation.Create(Self, tcabamShow).ImmediateAnimation;
    TdxTileControlActionsBarHelper.Register(Self);
  end;
end;

procedure TdxCustomTileControl.CheckDetailLayout(ADetail: TdxTileControlDetailSite);
begin
//
end;

procedure TdxCustomTileControl.CheckTitleChanges;
begin
//
end;

procedure TdxCustomTileControl.DrawScrollBars(ACanvas: TcxCanvas);
begin
  if not DetailsAnimationInProcess then
    inherited DrawScrollBars(ACanvas);
end;

function TdxCustomTileControl.FindGroup(const AName: string): TdxTileControlGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Groups.Count - 1 do
    if Groups[I].Name = AName then
    begin
      Result := Groups[I];
      Break;
    end;
end;

procedure TdxCustomTileControl.FocusChanged;
begin
  inherited FocusChanged;
  Controller.RefreshItems;
end;

procedure TdxCustomTileControl.ForceUpdate(AUpdateAll: Boolean = False);
begin
  dxAnimationController.Update;
  if AUpdateAll then
    Invalidate;
  Update;
end;

function TdxCustomTileControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := (DragAndDropState <> ddsNone);
  if not Result then
    HitTest.Calculate(X, Y);
  FRightButtonPressed := (ssRight in Shift) and Controller.HitAtItemOrGroup and
    (DragAndDropState = ddsNone);
  if FRightButtonPressed then
    Result := True
  else
    Result := False or (DragAndDropState <> ddsNone);
  if not Result and (ssLeft in Shift) then
    Result := not FRightButtonPressed and
      (Controller.HitAtItemOrGroup or HitTest.HitAtScrollButton or (DragAndDropState <> ddsNone));
end;

function TdxCustomTileControl.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  if Controller.IsDetectedDragItem then
    Result := TdxTileControlDragDropItem
  else
    if Controller.IsDetectedDragGroup then
      Result := TdxTileControlDragDropGroup
    else
      Result := TdxTileControlHandScroll;
end;

function TdxCustomTileControl.GetHScrollBarBounds: TRect;
begin
  if not OptionsView.FixedIndentHorz then
    Result := inherited GetHScrollBarBounds
  else
  begin
    Result := ClientBounds;
    if IsPopupScrollBars then
      Result.Top := Result.Bottom - HScrollBar.Height
    else
    begin
      Result.Top := Result.Bottom;
      Result.Bottom := Result.Top + HScrollBar.Height;
    end;
    InflateRect(Result, -OptionsView.IndentHorz, 0);
  end;
end;

function TdxCustomTileControl.GetOptionsBehaviorClass: TdxTileControlOptionsBehaviorClass;
begin
  Result := TdxTileControlOptionsBehavior;
end;

function TdxCustomTileControl.GetOptionsViewClass: TdxTileControlOptionsViewClass;
begin
  Result := TdxTileControlOptionsView;
end;

function TdxCustomTileControl.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  if IsPopupScrollBars then
    Result := inherited GetMainScrollBarsClass
  else
    Result := TcxControlScrollBars;
end;

function TdxCustomTileControl.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := inherited GetMouseWheelScrollingKind;
  if OptionsView.GroupLayout = glHorizontal then
  begin
    if (Result =  mwskNone) and (IsScrollBarActive(sbHorizontal) or (OptionsBehavior.ScrollMode = smScrollButtons)) then
      Result := mwskHorizontal
  end
  else
    if (Result =  mwskNone) and (IsScrollBarActive(sbVertical) or (OptionsBehavior.ScrollMode = smScrollButtons)) then
      Result := mwskVertical
end;

function TdxCustomTileControl.GetVScrollBarBounds: TRect;
begin
  Result := inherited GetVScrollBarBounds;
  Result.Top := Title.ViewInfo.Bounds.Bottom;
  if OptionsView.FixedIndentVert then
    InflateRect(Result, 0, -OptionsView.IndentVert);
  if UseRightToLeftScrollBar then
    Result := cxRectOffsetHorz(Result, -Result.Left);
end;

function TdxCustomTileControl.GetScrollStep: Integer;
begin
  Result := ScaleFactor.Apply(dxTileControlScrollStep);
end;

function TdxCustomTileControl.ActiveDetailOccupiesAllTileControl: Boolean;
begin
  Result := True;
end;

procedure TdxCustomTileControl.AddChanges(AChanges: TdxTileControlChanges);
begin
  FChanges := FChanges + AChanges;
  CheckChanges;
end;

function TdxCustomTileControl.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomTileControl.BoundsChanged;
begin
  FForceCalculate := True;
  try
    inherited BoundsChanged;
    AddChanges([tccLayout]);
    RefreshActionBars;
  finally
    FForceCalculate := False;
  end;
end;

procedure TdxCustomTileControl.BringInternalControlsToFront;
begin
  inherited BringInternalControlsToFront;
  if ActionBarBottom <> nil then
    ActionBarBottom.BringToFront;
  if ActionBarTop <> nil then
    ActionBarTop.BringToFront;
end;

function TdxCustomTileControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;

  function GetMinHeight: Integer;
  begin
    Result := ViewInfo.TitleHeight + ViewInfo.FRealContentHeight + BorderSize * 2;
  end;

  function GetMinWidth: Integer;
  begin
    Result := ViewInfo.FRealContentWidth + BorderSize * 2;
  end;

var
  ANewHeight, ANewWidth: Integer;
begin
  Result := AutoSize and (Align <> alClient);
  if not Result then
  begin
    NewHeight := Height;
    NewWidth := Width;
  end
  else
    if not FScalingInProcess then
    begin
      HScrollBar.Visible := False;
      VScrollBar.Visible := False;
      ViewInfo.Calculate;
      if Align in [alLeft, alRight] then
        ANewHeight := Height
      else
        if ViewInfo.FRealContentHeight = 0 then
          ANewHeight := NewHeight
        else
          ANewHeight := GetMinHeight;

      if Align in [alTop, alBottom] then
        ANewWidth := Width
      else
        if ViewInfo.FRealContentWidth = 0 then
          ANewWidth := NewWidth
        else
          ANewWidth := GetMinWidth;

      NewHeight := IfThen(not IsTouchScrollUIMode and (GetMinWidth > NewWidth),
        ANewHeight + HScrollBar.Height,  ANewHeight);
      NewWidth := IfThen(not IsTouchScrollUIMode and (GetMinHeight > NewHeight),
        ANewWidth + VScrollBar.Width,  ANewWidth);
    end;
end;

procedure TdxCustomTileControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  FScalingInProcess := True;
  BeginUpdate;
  try
    inherited ChangeScaleEx(M, D, isDpiChange);
    ActionBars.ChangeScale(M, D);
    Groups.ChangeScale(M, D);
    Items.ChangeScale(M, D);
    OptionsView.ChangeScale(M, D);
    Style.ChangeScale(M, D);
    Title.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
  FScalingInProcess := False;
end;

procedure TdxCustomTileControl.CheckChanges;
begin
  if IsLocked or (Changes = []) then Exit;
  ValidateChanges;
end;

function TdxCustomTileControl.CreateGroupsCollection: TdxTileControlGroupCollection;
begin
  Result := TdxTileControlGroupCollection.Create(Self, TdxTileControlGroup);
end;

function TdxCustomTileControl.CreateImagesChangeLink(AOnChange: TNotifyEvent): TChangeLink;
begin
  Result := TChangeLink.Create;
  Result.OnChange := AOnChange;
end;

function TdxCustomTileControl.CreateImagesFreeNotificator(AOnFree: TcxFreeNotificationEvent): TcxFreeNotificator;
begin
  Result := TcxFreeNotificator.Create(nil);
  Result.OnFreeNotification := AOnFree;
end;

function TdxCustomTileControl.CreateItemsCollection: TdxTileControlItemCollection;
begin
  Result := TdxTileControlItemCollection.Create(Self, TdxTileControlItem);
end;

function TdxCustomTileControl.CreatePainter: TdxTileControlPainter;
begin
  Result := TdxTileControlPainter.Create(Self);
end;

function TdxCustomTileControl.CreateStyle: TdxTileControlStyle;
begin
  Result := TdxTileControlStyle.Create(Self);
  Result.OnChange := StyleChanged;
end;

function TdxCustomTileControl.CreateTitle: TdxTileControlTitle;
begin
  Result := TdxTileControlTitle.Create(Self);
end;

procedure TdxCustomTileControl.CreateSubClasses;
begin
  FCheckedItems := TdxTileControlCheckedItems.Create;
  FStyle := CreateStyle;
  FGroups := CreateGroupsCollection;
  FItems := CreateItemsCollection;
  FViewInfo := CreateViewInfo;
  FHitTest := CreateHitTest;
  FAssets := CreateAssets;
  FActionBars := CreateActionBars;
  FImagesChangeLink := CreateImagesChangeLink(ImagesChanged);
  FImagesNotifyComponent := CreateImagesFreeNotificator(ImagesFreeNotification);
  FOptionsView := GetOptionsViewClass.Create(Self);
  FOptionsBehavior := GetOptionsBehaviorClass.Create(Self);
  FOptionsDetailAnimate:= TdxTileControlDetailOptionsAnimate.Create(Self);
  FOptionsItemAnimate := TdxTileControlCustomItemOptionsAnimate.Create(Self);
  FController := CreateController;
  FPainter := CreatePainter;
  FTitle := CreateTitle;
  FDragGroupInfo := TdxTileControlDragGroupInfo.Create;
  FDragItemInfo := TdxTileControlDragItemInfo.Create;
  if IsDesigning then
    TdxTileControlDesignKeyDownHookHelper.Register(Self);
end;

function TdxCustomTileControl.CreateActionBarBottom: TdxTileControlCustomActionBar;
begin
  Result := TdxTileControlBottomActionBar.Create(Self);
end;

function TdxCustomTileControl.CreateActionBars: TdxTileControlActionBars;
begin
  Result := TdxTileControlActionBars.Create(Self);
end;

function TdxCustomTileControl.CreateActionBarTop: TdxTileControlCustomActionBar;
begin
  Result := TdxTileControlTopActionBar.Create(Self);
end;

function TdxCustomTileControl.CreateAssets: TdxTileControlAssets;
begin
  Result := TdxTileControlAssets.Create;
end;

function TdxCustomTileControl.CreateController: TdxTileControlController;
begin
  Result := TdxTileControlController.Create(Self);
end;

function TdxCustomTileControl.CreateHitTest: TdxTileControlHitTest;
begin
  Result := TdxTileControlHitTest.Create(Self);
end;

function TdxCustomTileControl.CreateViewInfo: TdxTileControlViewInfo;
begin
  Result := TdxTileControlViewInfo.Create(Self);
end;

procedure TdxCustomTileControl.CreateWnd;
begin
  Controller.DestroyDesignHelper;
  inherited CreateWnd;
  Controller.CheckCreateDesignHelper;
end;

procedure TdxCustomTileControl.DestroySubClasses;
begin
  FController.StopAnimations;
  TdxTileControlActionsBarHelper.Unregister(Self);
  TdxTileControlDesignKeyDownHookHelper.Unregister(Self);
  FreeAndNil(FItems);
  FreeAndNil(FGroups);
  FreeAndNil(FHitTest);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FActionBars);
  FreeAndNil(FPainter);
  FreeAndNil(FStyle);
  FreeAndNil(FAssets);
  FreeAndNil(FActionBarTop);
  FreeAndNil(FActionBarBottom);
  Images := nil;
  FreeAndNil(FImagesChangeLink);
  FreeAndNil(FImagesNotifyComponent);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FOptionsDetailAnimate);
  FreeAndNil(FOptionsItemAnimate);
  FreeAndNil(FOptionsView);
  FreeAndNil(FCheckedItems);
  FreeAndNil(FTitle);
  FreeAndNil(FDragItemInfo);
  FreeAndNil(FDragGroupInfo);
end;

procedure TdxCustomTileControl.LayoutChanged;
begin
  Changes := Changes + [tccLayout];
  if IsLocked then Exit;
  CheckChanges;
  if AutoSize then
    AdjustSize;
end;

procedure TdxCustomTileControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
end;

procedure TdxCustomTileControl.DoLoaded;
begin
  Items.Loaded;
  Groups.Loaded;
  LayoutChanged;
  Controller.StartAnimations;
  Controller.CheckCreateDesignHelper;
end;

procedure TdxCustomTileControl.Loaded;
begin
  inherited Loaded;
  FJustLoaded := True;
  try
    DoLoaded;
  finally
    FJustLoaded := False;
  end;
end;

procedure TdxCustomTileControl.LockTimers;
var
  I: Integer;
begin
  if Items = nil then Exit;
  for I := 0 to Items.Count - 1 do
    Items[I].FAnimationTimer.Enabled := False;
end;

procedure TdxCustomTileControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  AddChanges([tccItems]);
  Painter.ValidatePainterData;
  if ActiveDetail <> nil then
    ActiveDetail.Invalidate;
  Invalidate;
end;

procedure TdxCustomTileControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseDown(Button, Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomTileControl.MouseLeave(AControl: TControl);
begin
  if AControl = Self then
    Controller.MouseLeave;
  inherited MouseLeave(AControl);
end;

procedure TdxCustomTileControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseMove(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxCustomTileControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, X, Y);
end;

function TdxCustomTileControl.NeedStopAnimationsBeforeActiveDetailChanging(AActiveDetail: TdxTileControlDetailSite): Boolean;
begin
  Result := AActiveDetail = nil;
end;

procedure TdxCustomTileControl.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not IsDestroying and (Operation = opRemove) and (AComponent = ActiveDetail) then
  begin
    FActiveDetail := nil;
    Repaint;
  end;
end;

procedure TdxCustomTileControl.ReCreateViewInfo;
begin
  FreeAndNil(FViewInfo);
  FViewInfo := CreateViewInfo;
  LayoutChanged;
end;

procedure TdxCustomTileControl.RemoveGroup(AGroup: TdxTileControlGroup);
begin
  if AGroup = nil then Exit;
  FGroups.Remove(AGroup);
end;

procedure TdxCustomTileControl.RemoveGroupIfEmpty(AGroup: TdxTileControlGroup);
begin
  if (AGroup = nil) or (AGroup.ItemCount > 0) then Exit;
  BeginUpdate;
  try
    RemoveGroup(AGroup);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomTileControl.RefreshItems;
begin
  Controller.StopAnimations;
  try
    Items.ForEach(RefreshItem, nil, False);
  finally
    Controller.StartAnimations;
  end;
end;

function TdxCustomTileControl.IsAnyActionBarVisible: Boolean;
begin
  Result := IsActionBarTopVisible or IsActionBarBottomVisible;
end;

procedure TdxCustomTileControl.HideActionBars;
begin
  DoHideActionBars(abvcrManualSwitch);
end;

procedure TdxCustomTileControl.ShowActionBars;
begin
  DoShowActionBars(abvcrManualSwitch);
end;

procedure TdxCustomTileControl.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollCode <> scEndScroll then
  begin
    if AScrollBarKind = sbHorizontal then
      ScrollHorz(AScrollBarKind, AScrollCode, AScrollPos)
    else
      ScrollVert(AScrollBarKind, AScrollCode, AScrollPos);

    ForceUpdate;
  end;
end;

procedure TdxCustomTileControl.ScrollHorz(
  AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if not (AScrollCode in [scTrack, scPosition]) then
    AScrollPos := ViewInfo.LeftScrollPos;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, GetScrollStep);
    scLineDown:
      Inc(AScrollPos, GetScrollStep);
    scPageUp:
      Dec(AScrollPos, ViewInfo.HorzScrollPage);
    scPageDown:
      Inc(AScrollPos, ViewInfo.HorzScrollPage);
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := ViewInfo.ContentWidth;
  end;
  ViewInfo.LeftScrollPos := AScrollPos;
end;

procedure TdxCustomTileControl.ScrollVert(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  AGroupLayout: TdxTileControlGroupLayout;
begin
  if not (AScrollCode in [scTrack, scPosition]) then
    AScrollPos := ViewInfo.TopScrollPos;
  AGroupLayout := OptionsView.GroupLayout;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, GetScrollStep);
    scLineDown:
      Inc(AScrollPos, GetScrollStep);
    scPageUp:
      begin
        if AGroupLayout = glHorizontal then
          Dec(AScrollPos, ViewInfo.HorzScrollPage)
        else
          Dec(AScrollPos, ViewInfo.VertScrollPage);
      end;
    scPageDown:
      begin
        if AGroupLayout = glHorizontal then
          Inc(AScrollPos, ViewInfo.HorzScrollPage)
        else
          Inc(AScrollPos, ViewInfo.VertScrollPage);
      end;
    scTop:
      AScrollPos := 0;
    scBottom:
      begin
        if AGroupLayout = glHorizontal then
          AScrollPos := ViewInfo.ContentWidth
        else
          AScrollPos := ViewInfo.ContentHeight;
      end;
  end;
  ViewInfo.TopScrollPos := AScrollPos;
end;

procedure TdxCustomTileControl.StyleChanged(Sender: TObject);
begin
  LayoutChanged;
end;

procedure TdxCustomTileControl.TitleChanged;
begin
  FTitleChanged := True;
  LayoutChanged;
end;

procedure TdxCustomTileControl.TransparentChanged;
begin
  if Transparent then
    ControlStyle := ControlStyle + [csParentBackground]
  else
    ControlStyle := ControlStyle - [csParentBackground];
  inherited;
end;

procedure TdxCustomTileControl.UnlockTimers;
var
  I: Integer;
begin
  if Items = nil then Exit;
  for I := 0 to Items.Count - 1 do
    Items[I].CheckTimerEnabled;
end;

procedure TdxCustomTileControl.ValidateChanges;
begin
  LockTimers;
  try
    if tccItems in Changes then
    begin
      Include(FChanges, tccLayout);
      RefreshItems;
    end;
    if tccLayout in Changes then
    begin
      ViewInfo.Calculate;
      if ActiveDetail <> nil then
      begin
        ActiveDetail.RecalculateSubClasses;
        CheckDetailLayout(ActiveDetail);
      end;
      if FTitleChanged then
      begin
        CheckTitleChanges;
        FTitleChanged := False;
      end;
    end;
    InvalidateChanges;
  finally
    Changes := [];
    UnlockTimers;
  end;
end;

procedure TdxCustomTileControl.WMContextMenu(var Message: TWMContextMenu);
begin
  inherited;
  Controller.ProcessContextMenuMessage(Message);
end;

procedure TdxCustomTileControl.WMHideGroupCaptionEditors(var Message: TMessage);
begin
  Controller.HideGroupCaptionEditors;
end;

procedure TdxCustomTileControl.InitializeDragGroupInfo;
begin
  DragGroupInfo.Initialize(HitTest.Group);
end;

procedure TdxCustomTileControl.InitializeDragItemInfo;
begin
  DragItemInfo.Initialize(HitTest.Item);
end;

function TdxCustomTileControl.IsGroupDragged: Boolean;
begin
  Result := (DragAndDropState = ddsInProcess) and
    (DragAndDropObject is TdxTileControlDragDropGroup);
end;

function TdxCustomTileControl.IsItemDragged: Boolean;
begin
  Result := (DragAndDropState = ddsInProcess) and
    (DragAndDropObject is TdxTileControlDragDropItem);
end;

function TdxCustomTileControl.IsStyleStored: Boolean;
begin
  Result := Style.IsChanged;
end;

function TdxCustomTileControl.IsTitleStored: Boolean;
begin
  Result := FTitle.IsChanged;
end;

procedure TdxCustomTileControl.RefreshActionBars;

  procedure RecalculateActionBarsBoundsRect;
  begin
    ActionBarBottom.BoundsRect := GetActionBarBottomBounds;
    ActionBarTop.BoundsRect := GetActionBarTopBounds;
  end;

begin
  if (ActionBarTop <> nil) and (ActionBarBottom <> nil) then
  begin
    ActionBarBottom.ViewInfo.Clear;
    ActionBarTop.ViewInfo.Clear;
    RecalculateActionBarsBoundsRect;
    BringInternalControlsToFront;
    ActionBarBottom.Recalculate;
    ActionBarTop.Recalculate;
    RecalculateActionBarsBoundsRect;
  end;
end;

procedure TdxCustomTileControl.RefreshItem(AItem: TdxTileControlItem; const AData: Pointer);
begin
  AItem.Changed;
end;

procedure TdxCustomTileControl.SetActionBars(AValue: TdxTileControlActionBars);
begin
  FActionBars.Assign(AValue);
end;

procedure TdxCustomTileControl.SetActiveDetail(AValue: TdxTileControlDetailSite);
var
  APrevItem: TdxTileControlItem;
begin
  if IsDesigning then
    AValue := nil;
  APrevItem := nil;
  if ActiveDetail <> nil then
  begin
    APrevItem := ActiveDetail.TileItem;
    APrevItem.DoDeactivateDetail;
  end;
  if IsDestroying then
  begin
    FActiveDetail := nil;
    Exit;
  end;
  if FActiveDetail <> AValue then
  begin
    Controller.HideActionBars(abvcrActiveDetailChange);
    Controller.UncheckAllItems;
    BeforeActiveDetailChangingAnimation(FActiveDetail, AValue);
    try
      if ActiveDetail <> nil then
      begin
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
        ActiveDetail.Parent := nil;
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
      end;
      if IsLoading or JustLoaded or IsDestroying then
        FreeAndNil(FAnimation)
      else
        FAnimation.ImmediateAnimation;
    finally
      AfterActiveDetailChangingAnimation(FActiveDetail, AValue);
    end;
    if APrevItem <> nil then
      APrevItem.DetailOptions.RestoreDetailControlPrevParent;
  end;
end;

procedure TdxCustomTileControl.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  LayoutChanged;
end;

procedure TdxCustomTileControl.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FImagesChangeLink, FImagesNotifyComponent);
end;

procedure TdxCustomTileControl.SetOptionsDetailAnimate(AValue: TdxTileControlDetailOptionsAnimate);
begin
  FOptionsDetailAnimate.Assign(AValue);
end;

procedure TdxCustomTileControl.SetOptionsItemAnimate(AValue: TdxTileControlCustomItemOptionsAnimate);
begin
  FOptionsItemAnimate.Assign(AValue);
end;

procedure TdxCustomTileControl.SetOptionsBehavior(AValue: TdxTileControlOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxCustomTileControl.SetOptionsView(AValue: TdxTileControlOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TdxCustomTileControl.SetStyle(AValue: TdxTileControlStyle);
begin
  FStyle.Assign(AValue);
end;

procedure TdxCustomTileControl.SetTitle(AValue: TdxTileControlTitle);
begin
  FTitle.Assign(AValue);
end;

function TdxCustomTileControl.StartDragAndDrop(const P: TPoint): Boolean;
var
  AHitAtItem, AHitAtGroup: Boolean;
  ADragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  if ViewInfo.DragDropChanges <> nil then
  begin
    Result := False;
    Exit;
  end;
  HitTest.HitPoint := P;
  AHitAtItem := Controller.HitAtItem;
  AHitAtGroup := Controller.HitAtGroup;
  ADragAndDropObjectClass := GetDragAndDropObjectClass;
  Result := not FRightButtonPressed and not HitTest.HitAtScrollButton and
      not HitTest.HitAtTitle and
    ((AHitAtItem and Controller.CanItemMoving) or (AHitAtGroup and Controller.CanGroupMoving) or
     (ViewInfo.IsScrollAvailable and not IsDesigning and (ADragAndDropObjectClass = TdxTileControlHandScroll)));
  Controller.DragCell := HitTest.HitObject;
  Controller.StartDragPos := P;
end;

// IcxStoredObject

function TdxCustomTileControl.GetObjectName: string;
begin
  Result := GetStoredObjectName;
end;

function TdxCustomTileControl.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := GetStoredObjectProperties(AProperties);
end;

procedure TdxCustomTileControl.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  GetStoredPropertyValue(AName, AValue);
end;

procedure TdxCustomTileControl.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  SetStoredPropertyValue(AName, AValue);
end;

// used classes

function TdxCustomTileControl.GetGroupsClass: TdxTileControlGroupCollectionClass;
begin
  Result := TdxTileControlGroupCollection;
end;

function TdxCustomTileControl.GetItemsClass: TdxTileControlItemCollectionClass;
begin
  Result := TdxTileControlItemCollection;
end;

// store-restore layout

procedure TdxCustomTileControl.RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string;
  AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreName: string);
var
  AStorage: TcxStorage;
  AModes: TcxStorageModes;
begin
  FStoringName := ARestoreName;
  AStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    AModes := [];
    if ACreateChildren then
      Include(AModes, smChildrenCreating);
    if ADeleteChildren then
      Include(AModes, smChildrenDeleting);
    AStorage.Modes := AModes;
    AStorage.UseInterfaceOnly := True;
    if ARestoreName = '' then
      AStorage.NamePrefix := Owner.Name;
    BeginUpdate;
    try
      IsRestoring := True;
      try
        case AStorageType of
          stIniFile:
            AStorage.RestoreFromIni(Self);
          stRegistry:
            AStorage.RestoreFromRegistry(Self);
          stStream:
            AStorage.RestoreFromStream(Self);
        end;
      finally
        IsRestoring := False;
      end;
      Groups.Loaded;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TdxCustomTileControl.StoreTo(AStorageType: TcxStorageType; const AStorageName: string;
  AStorageStream: TStream; AReCreate: Boolean; const ASaveName: string);
var
  AStorage: TcxStorage;
begin
  FStoringName := ASaveName;
  AStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    AStorage.UseInterfaceOnly := True;
    if ASaveName = '' then
      AStorage.NamePrefix := Owner.Name;
    AStorage.ReCreate := AReCreate;
    case AStorageType of
      stIniFile:
        AStorage.StoreToIni(Self);
      stRegistry:
        AStorage.StoreToRegistry(Self);
      stStream:
        AStorage.StoreToStream(Self);
    end;
    AStorage.ReCreate := False;
  finally
    AStorage.Free;
  end;
end;

// cxStorage implementation

procedure TdxCustomTileControl.GetStoredChildren(AChildren: TStringList);
begin
  if StoredVersion > 0 then
  begin
    if Groups <> nil then
      AChildren.AddObject('', Groups);
    if Items <> nil then
      AChildren.AddObject('', Items);
  end
  else
  begin
    if Items <> nil then
      AChildren.AddObject('', Items);
    if Groups <> nil then
      AChildren.AddObject('', Groups);
  end
end;

function TdxCustomTileControl.GetStoredObjectName: string;
begin
  if FStoringName = '' then
    Result := Name
  else
    Result := FStoringName;
end;

function TdxCustomTileControl.GetStoredObjectProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Version');
  if Assigned(OnGetStoredProperties) then
    OnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TdxCustomTileControl.GetStoredPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Version' then
  begin
    AValue := dxTileControlStoringVersion;
    FStoredVersion := AValue;
  end
  else
    FStoredVersion := 0;
  if Assigned(OnGetStoredPropertyValue) then
    OnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TdxCustomTileControl.SetStoredPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Version' then
    FStoredVersion := AValue;
  if Assigned(OnSetStoredPropertyValue) then
    OnSetStoredPropertyValue(Self, AName, AValue);
end;

function TdxCustomTileControl.StoredCreateChild(const AObjectName, AClassName: string): TObject;
begin
  if (AClassName = GetGroupsClass.ClassName) or (AClassName = GetItemsClass.ClassName) then
  begin
    if AClassName = GetGroupsClass.ClassName then
      Result := CreateGroupsCollection
    else
      Result := CreateItemsCollection;
    DoInitStoredObject(Result);
  end
  else
    Result := nil;
end;

procedure TdxCustomTileControl.StoredDeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

// storing layout

procedure TdxCustomTileControl.RestoreFromIniFile(const AStorageName: string;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreName: string = '');
begin
  RestoreFrom(stIniFile, AStorageName, nil, AChildrenCreating, AChildrenDeleting, ARestoreName);
end;

procedure TdxCustomTileControl.RestoreFromRegistry(const AStorageName: string;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreName: string = '');
begin
  RestoreFrom(stRegistry, AStorageName, nil, AChildrenCreating, AChildrenDeleting, ARestoreName);
end;

procedure TdxCustomTileControl.RestoreFromStream(AStream: TStream;
  AChildrenCreating: Boolean = False; AChildrenDeleting: Boolean = False;
  const ARestoreName: string = '');
begin
  RestoreFrom(stStream, '', AStream, AChildrenCreating, AChildrenDeleting, ARestoreName);
end;

procedure TdxCustomTileControl.StoreToIniFile(AStorageName: string;
  AReCreate: Boolean = True; const ASaveName: string = '');
begin
  StoreTo(stIniFile, AStorageName, nil, AReCreate, ASaveName);
end;

procedure TdxCustomTileControl.StoreToRegistry(AStorageName: string;
  AReCreate: Boolean = True; const ASaveName: string = '');
begin
  StoreTo(stRegistry, AStorageName, nil, AReCreate, ASaveName);
end;

procedure TdxCustomTileControl.StoreToStream(AStream: TStream; const ASaveName: string = '');
begin
  StoreTo(stStream, '', AStream, True, ASaveName);
end;

{ TdxTileControlAssets }

constructor TdxTileControlAssets.Create;
begin
  inherited Create;

  FBackButton := TdxSkinImage.Create(nil);
  FBackButton.LoadFromResource(HInstance, 'dxTileControlBackButton', RT_RCDATA);
  FBackButton.ImageCount := 3;

  FCustomButton := TdxSkinImage.Create(nil);
  FCustomButton.LoadFromResource(HInstance, 'dxTileControlCustomButton', RT_RCDATA);
  FCustomButton.ImageCount := 3;
end;

destructor TdxTileControlAssets.Destroy;
begin
  FreeAndNil(FCustomButton);
  FreeAndNil(FBackButton);
  inherited Destroy;
end;

{ TdxTileControlVirtualGroupViewInfo }

procedure TdxTileControlVirtualGroupViewInfo.CalculateDrawingBounds;
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if not SizeMustFromDragItem then Exit;
  if GroupLayout = glHorizontal then
    GetDrawingBoundsParamsAtHorizontalGroupLayout(ALeft, ATop, AWidth, AHeight)
  else
    GetDrawingBoundsParamsAtVerticalGroupLayout(ALeft, ATop, AWidth, AHeight);
  FDrawingBounds := cxRectBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxTileControlVirtualGroupViewInfo.CalculateSize(var AWidth, AHeight: Integer);
begin
  inherited CalculateSize(AWidth, AHeight);
  if not Group.Visible then Exit;
  if GroupLayout = glHorizontal then
    CalculateSizeAtHorizontalGroupLayout(AWidth, AHeight)
  else
    CalculateSizeAtVerticalGroupLayout(AWidth, AHeight);
end;

procedure TdxTileControlVirtualGroupViewInfo.CalculateSizeAtHorizontalGroupLayout(
  var AWidth, AHeight: Integer);

  function WeightOutermostGroup(const AAdvanceWidth: Integer): Integer;
  begin
    if not TileControl.IsItemDragged then
      Result := AAdvanceWidth
    else
      Result := Max(TileControl.ViewInfo.ItemHalfIndent + ScaleFactor.Apply(1), AAdvanceWidth);
  end;

var
  AGroupIndent, ABorderWidth: Integer;
begin
  AGroupIndent := TileControl.OptionsView.GroupIndent;
  ABorderWidth := GetBorderWidth;
  AWidth := AGroupIndent + Group.GetCustomIndent(Self);
  if ((Self = Group.VirtualGroupBefore) and Group.IsMostLeft) or
     ((Self = Group.VirtualGroupAfter) and Group.IsMostRight) then
    AWidth := WeightOutermostGroup(TileControl.OptionsView.IndentHorz - ABorderWidth);
  if SizeMustFromDragItem then
    AWidth := AWidth + AGroupIndent + ABorderWidth * 2 +
      cxRectWidth((TileControl.DragAndDropObject as TdxTileControlDragDropItem).DragItem.ViewInfo.Bounds);
  FBasisWidth := AWidth;
  if TileControl.ViewInfo.IsFixedContentLeftSide and (Self = Group.VirtualGroupBefore) and Group.IsMostLeft then
    AWidth := Max(AWidth, TileControl.ViewInfo.VisibleGroupsOrigin.X - TileControl.ViewInfo.TilesArea.Left);
  if (not TileControl.OptionsView.CenterContentHorz or TileControl.ViewInfo.IsFixedContentLeftSide) and
    (Self = Group.VirtualGroupAfter) and Group.IsMostRight then
    AWidth := Max(AWidth, TileControl.ViewInfo.ClientBounds.Right - Group.Bounds.Right);
  AHeight := Max(TileControl.ViewInfo.ContentHeight, TileControl.ClientBounds.Bottom);
end;

procedure TdxTileControlVirtualGroupViewInfo.CalculateSizeAtVerticalGroupLayout(
  var AWidth, AHeight: Integer);

  function HeightOutermostGroup(const AAdvanceHeight: Integer): Integer;
  begin
    if not TileControl.IsItemDragged then
      Result := AAdvanceHeight
    else
      Result := Max(TileControl.ViewInfo.ItemHalfIndent + ScaleFactor.Apply(1), AAdvanceHeight);
  end;

var
  AGroupIndent, ABorderWidth: Integer;
begin
  AGroupIndent := TileControl.OptionsView.GroupIndent;
  ABorderWidth := GetBorderWidth;
  AWidth := Max(TileControl.ViewInfo.ContentWidth, TileControl.ClientBounds.Right);
  AHeight := AGroupIndent + Group.GetCustomIndent(Self);
  if ((Self = Group.VirtualGroupBefore) and Group.IsMostLeft) or
     ((Self = Group.VirtualGroupAfter) and Group.IsMostRight) then
    AHeight := HeightOutermostGroup(TileControl.OptionsView.IndentVert - ABorderWidth);
  if SizeMustFromDragItem then
    AHeight := AHeight + AGroupIndent + ABorderWidth * 2 +
      cxRectHeight((TileControl.DragAndDropObject as TdxTileControlDragDropItem).DragItem.ViewInfo.Bounds);
  if TileControl.ViewInfo.IsFixedContentTopSide and (Self = Group.VirtualGroupBefore) and Group.IsMostLeft then
    AHeight := Max(AHeight, TileControl.ViewInfo.VisibleGroupsOrigin.Y - TileControl.ViewInfo.TilesArea.Top);
  if (not TileControl.OptionsView.CenterContentVert or TileControl.ViewInfo.IsFixedContentTopSide) and
    (Self = Group.VirtualGroupAfter) and Group.IsMostRight then
    AHeight := Max(AHeight, TileControl.ViewInfo.ClientBounds.Bottom - Group.Bounds.Bottom);
end;

procedure TdxTileControlVirtualGroupViewInfo.DoCalculate;
begin
  CalculateBounds;
  CalculateDrawingBounds;
  inherited DoCalculate;
end;

procedure TdxTileControlVirtualGroupViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if State = vgsVisible then
    Painter.DrawVirtualGroup(ACanvas, DrawingBounds)
end;

procedure TdxTileControlVirtualGroupViewInfo.ExpandBounds(const DW, DH: Integer);
begin
  Bounds := cxRectSetWidth(Bounds, Bounds.Right + DW);
  Bounds := cxRectSetHeight(Bounds,  Bounds.Bottom + DH);
  RecalculateDrawingBounds;
end;

procedure TdxTileControlVirtualGroupViewInfo.GetDrawingBoundsParamsAtHorizontalGroupLayout(
  var ALeft, ATop, AWidth, AHeight: Integer);
var
  ABounds, ATilesArea: TRect;
  AItemIndent, AGroupIndent: Integer;
begin
  AItemIndent := TileControl.OptionsView.ItemIndent;
  AGroupIndent := TileControl.OptionsView.GroupIndent;
  ATilesArea := TileControl.ViewInfo.TilesArea;
  ABounds := Group.MaximizedAreaBounds;
  ATop := ABounds.Top;
  AWidth := AGroupIndent;
  AHeight := TileControl.ViewInfo.TopScrollPos;
  if TileControl.OptionsView.FixedIndentVert then
    Inc(AHeight, cxRectHeight(ATilesArea))
  else
  begin
    Inc(AHeight, cxRectHeight(ABounds));
    if (ATop + AHeight) > (ATilesArea.Bottom - AItemIndent) then
      AHeight := ATilesArea.Bottom - AItemIndent - ATop;
  end;

  if Self = Group.VirtualGroupBefore then
    ALeft := Bounds.Right - AItemIndent - AGroupIndent - AWidth
  else
    if Group.IsMostRight then
      ALeft := Bounds.Left + AItemIndent + AGroupIndent
    else
      ALeft := cxRectCenter(Bounds).X - AWidth div 2;
end;

procedure TdxTileControlVirtualGroupViewInfo.GetDrawingBoundsParamsAtVerticalGroupLayout(
  var ALeft, ATop, AWidth, AHeight: Integer);
var
  ABounds, ATilesArea: TRect;
  AItemIndent, AGroupIndent: Integer;
begin
  AItemIndent := TileControl.OptionsView.ItemIndent;
  AGroupIndent := TileControl.OptionsView.GroupIndent;
  ATilesArea := TileControl.ViewInfo.TilesArea;
  ABounds := Group.ExpandedBounds;
  AHeight := AGroupIndent;
  ALeft := Max(AItemIndent, TileControl.OptionsView.IndentHorz - AItemIndent);
  if TileControl.OptionsView.FixedIndentHorz then
    AWidth := cxRectWidth(ATilesArea) + TileControl.ViewInfo.LeftScrollPos
  else
    AWidth := TileControl.ViewInfo.ContentWidth - 2 * ALeft;
  ALeft := ALeft - TileControl.ViewInfo.LeftScrollPos;
  if Self = Group.VirtualGroupBefore then
    ATop := (ATilesArea.Top + Bounds.Bottom - AHeight) div 2
  else
    if Group.IsMostRight then
      ATop := Bounds.Top + AItemIndent + AGroupIndent
    else
      ATop := cxRectCenter(Bounds).Y - AHeight div 2;
end;

function TdxTileControlVirtualGroupViewInfo.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := TileControl.OptionsView.GroupLayout;
end;

function TdxTileControlVirtualGroupViewInfo.GetLeftPos: Integer;
begin
  Result := inherited GetLeftPos;
  if Group.Visible then
    if (Self = Group.VirtualGroupAfter) and (GroupLayout = glHorizontal) then
      Result := Group.ViewInfo.ItemsRight - GetBorderWidth
    else
    begin
      Result := TileControl.ViewInfo.TilesArea.Left - TileControl.ViewInfo.LeftScrollPos;
      if TileControl.OptionsView.FixedIndentHorz then
        Dec(Result, TileControl.OptionsView.IndentHorz);
    end;
end;

function TdxTileControlVirtualGroupViewInfo.GetTopPos: Integer;
begin
  Result := inherited GetTopPos;
  if Group.Visible and (GroupLayout = glVertical) then
    if Self = Group.VirtualGroupBefore then
    begin
      Result := TileControl.ViewInfo.TilesArea.Top - TileControl.ViewInfo.TopScrollPos;
      if TileControl.OptionsView.FixedIndentVert then
        Dec(Result, TileControl.OptionsView.IndentVert);
    end
    else
      Result := Group.ViewInfo.ItemsBottom - GetBorderWidth;
end;

procedure TdxTileControlVirtualGroupViewInfo.Invalidate;
begin
  if Visible then
    if not Group.UseRightToLeftAlignment then
      TileControl.InvalidateRect(Bounds, False)
    else
      TileControl.InvalidateRect(TdxRightToLeftLayoutConverter.ConvertRect(Bounds, TileControl.ViewInfo.ClientBounds), False);
end;

procedure TdxTileControlVirtualGroupViewInfo.RecalculateDrawingBounds;
begin
  CalculateClipping;
  CalculateDrawingBounds;
end;

procedure TdxTileControlVirtualGroupViewInfo.Offset(const DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  FDrawingBounds := cxRectOffset(DrawingBounds, DX, DY);
  if TileControl.OptionsView.FixedIndentHorz and (TileControl.OptionsView.GroupLayout = glVertical) then
    Inc(FDrawingBounds.Right, -DX);
  if TileControl.OptionsView.GroupLayout = glHorizontal then
    Inc(FDrawingBounds.Bottom, -DY);
end;

procedure TdxTileControlVirtualGroupViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FDrawingBounds := TdxRightToLeftLayoutConverter.ConvertRect(FDrawingBounds, AClientBounds);
  FClipRect := TdxRightToLeftLayoutConverter.ConvertRect(FClipRect, AClientBounds);
end;

procedure TdxTileControlVirtualGroupViewInfo.SetState(const AValue: TdxTileControlVirtualGroupState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
    if AValue <> vgsVisible then
      SizeMustFromDragItem := False;
  end;
end;

{ TdxTileControlCustomActionBar }

constructor TdxTileControlCustomActionBar.Create(ATileControl: TdxCustomTileControl);
begin
  inherited Create(nil);
  FTileControl := ATileControl;
  FHitTest := CreateHitTest;
  FController := CreateController;
  FViewInfo := CreateViewInfo;
end;

destructor TdxTileControlCustomActionBar.Destroy;
begin
  FreeAndNil(FController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

procedure TdxTileControlCustomActionBar.BoundsChanged;
begin
  inherited BoundsChanged;
  Recalculate;
end;

function TdxTileControlCustomActionBar.CreateController: TdxTileControlActionBarController;
begin
  Result := TdxTileControlActionBarController.Create(Self);
end;

function TdxTileControlCustomActionBar.CreateHitTest: TdxTileControlActionBarHitTest;
begin
  Result := TdxTileControlActionBarHitTest.Create(Self);
end;

procedure TdxTileControlCustomActionBar.DoPaint;
begin
  ViewInfo.Draw(Canvas);
end;

procedure TdxTileControlCustomActionBar.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxTileControlCustomActionBar.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

procedure TdxTileControlCustomActionBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, X, Y);
end;

procedure TdxTileControlCustomActionBar.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TdxTileControlCustomActionBar.Recalculate;
begin
  ViewInfo.Bounds := ClientRect;
  ViewInfo.Calculate;
  Invalidate;
end;

{ TdxTileControlActionBarsAnimation }

constructor TdxTileControlActionBarsAnimation.Create(
  ATileControl: TdxCustomTileControl; AMode: TdxTileControlActionBarsAnimationMode);
begin
  FTileControl := ATileControl;
  FTileControl.RefreshActionBars;
  inherited Create(dxTileControlActionBarsAnimationTime, ateAccelerateDecelerate,
    Max(ActionBarBottom.ViewInfo.MeasureHeight, ActionBarTop.ViewInfo.MeasureHeight));
  FMode := AMode;
end;

function TdxTileControlActionBarsAnimation.CalculateActionBarBottomBounds: TRect;
var
  ADelta: Integer;
begin
  Result := TileControl.GetActionBarBottomBounds;
  ADelta := MulDiv(cxRectHeight(Result), Position, Length);
  if Mode = tcabamShow then
    ADelta := cxRectHeight(Result) - ADelta;
  Result := cxRectOffset(Result, 0, ADelta);
end;

function TdxTileControlActionBarsAnimation.CalculateActionBarTopBounds: TRect;
var
  ADelta: Integer;
begin
  Result := TileControl.GetActionBarTopBounds;
  ADelta := MulDiv(cxRectHeight(Result), Position, Length);
  if Mode = tcabamShow then
    Dec(ADelta, cxRectHeight(Result))
  else
    ADelta := -ADelta;
  Result := cxRectOffset(Result, 0, ADelta);
end;

procedure TdxTileControlActionBarsAnimation.DoAnimate;

  procedure DeferWindowPosition(AInfoHandle: HDWP; AWindow: TWinControl; const R: TRect);
  begin
    DeferWindowPos(AInfoHandle, AWindow.Handle, 0,
      R.Left, R.Top, cxRectWidth(R), cxRectHeight(R), SWP_NOZORDER)
  end;

  procedure UpdateVisibility(AValue: Boolean);
  begin
    ActionBarTop.Visible := AValue;
    ActionBarBottom.Visible := AValue;
  end;

var
  ADeferHandle: HDWP;
begin
  if Position <> FPrevPosition then
  begin
    ADeferHandle := BeginDeferWindowPos(2);
    try
      DeferWindowPosition(ADeferHandle, ActionBarTop, CalculateActionBarTopBounds);
      DeferWindowPosition(ADeferHandle, ActionBarBottom, CalculateActionBarBottomBounds);
    finally
      EndDeferWindowPos(ADeferHandle);
    end;
  end;

  if Finished then
    UpdateVisibility(Mode = tcabamShow)
  else
    UpdateVisibility(True);
  TileControl.ForceUpdate;
end;

function TdxTileControlActionBarsAnimation.GetActionBarBottom: TdxTileControlCustomActionBar;
begin
  Result := TileControl.ActionBarBottom;
end;

function TdxTileControlActionBarsAnimation.GetActionBarTop: TdxTileControlCustomActionBar;
begin
  Result := TileControl.ActionBarTop;
end;

function TdxTileControlActionBarsAnimation.IsCompatible(Animation: TdxAnimationTransition): Boolean;
begin
  Result := not (Animation is TdxTileControlActionBarsAnimation);
end;

{ TdxTileControlContentAnimation }

constructor TdxTileControlContentAnimation.Create(ATileControl: TdxCustomTileControl;
  ATime: Cardinal; const ALengthX, ALengthY: Integer);
begin
  FTileControl := ATileControl;
  FLengthX := ALengthX;
  FLengthY := ALengthY;
  inherited Create(ATime, ateAccelerateDecelerate, Max(Abs(LengthX), Abs(LengthY)));
end;

procedure TdxTileControlContentAnimation.DoAnimate;
var
  DX, DY: Integer;
  APositionX, APositionY: Integer;
begin
  APositionX := GetPositionX;
  APositionY := GetPositionY;
  DX := (APositionX - PrevPositionX) * Sign(LengthX);
  DY := (APositionY - PrevPositionY) * Sign(LengthY);
  DoMoveContent(DX, DY);
  FPrevPositionX := APositionX;
  FPrevPositionY := APositionY;
  TileControl.ForceUpdate();
end;

procedure TdxTileControlContentAnimation.DoMoveContent(const DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to ViewInfo.Cells.Count - 1 do
    ViewInfo.Cells[I].Scroll(DX, DY);
end;

function TdxTileControlContentAnimation.GetPositionX: Integer;
begin
  Result := MulDiv(Abs(LengthX), Position, Length);
end;

function TdxTileControlContentAnimation.GetPositionY: Integer;
begin
  Result := MulDiv(Abs(LengthY), Position, Length);
end;

function TdxTileControlContentAnimation.GetViewInfo: TdxTileControlViewInfo;
begin
  Result := TileControl.ViewInfo;
end;

{ TdxTileControlRubberAnimation }

constructor TdxTileControlRubberAnimation.Create(ATileControl: TdxCustomTileControl;
  AHandScrollObject: TdxTileControlHandScroll);
begin
  FHandScrollObject := AHandScrollObject;
  inherited Create(ATileControl, dxTileControlRubberAnimationTime,
    HandScrollObject.FinishAnimationPosX - HandScrollObject.StartAnimationPosX,
    HandScrollObject.FinishAnimationPosY - HandScrollObject.StartAnimationPosY);
  Controller.FHasRubberAnimation := True;
end;

destructor TdxTileControlRubberAnimation.Destroy;
begin
  Controller.FHasRubberAnimation := False;
  TileControl.UpdateScrollBars;
  inherited Destroy;
end;

procedure TdxTileControlRubberAnimation.DoMoveContent(const DX, DY: Integer);
begin
  if (DX <> 0) or (DY <> 0) then
    TileControl.ViewInfo.Scroll(DX, DY);
end;

function TdxTileControlRubberAnimation.GetController: TdxTileControlController;
begin
  Result := TileControl.Controller;
end;

{ TdxTileControlActionBarController }

constructor TdxTileControlActionBarController.Create(AOwner: TdxTileControlCustomActionBar);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TdxTileControlActionBarController.Destroy;
begin
  EndMouseTracking(Self);
  inherited Destroy;
end;

procedure TdxTileControlActionBarController.HideActionBars(AReason: TdxTileControlActionBarVisibilityChangeReason);
begin
  TileControl.DoHideActionBars(AReason);
end;

procedure TdxTileControlActionBarController.MouseDown(
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  HitTest.Calculate(X, Y);
  if AButton = mbLeft then
    PressedCell := HitTest.HitObject;
end;

procedure TdxTileControlActionBarController.MouseLeave;
begin
  MouseMove([], -1, -1);
  EndMouseTracking(Self);
end;

procedure TdxTileControlActionBarController.MouseMove(AShift: TShiftState; X, Y: Integer);
begin
  HitTest.Calculate(X, Y);
  HotCell := HitTest.HitObject;
  BeginMouseTracking(Owner, Owner.Bounds, Self);
end;

procedure TdxTileControlActionBarController.MouseUp(
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  HitTest.Calculate(X, Y);
  case AButton of
    mbLeft:
      ProcessMouseLeftButtonClick;
    mbRight:
      begin
        UncheckAllItems;
        HideActionBars(abvcrMouseRightClick);
      end;
  end;
  PressedCell := nil;
end;

function TdxTileControlActionBarController.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(Owner.Bounds, P);
end;

procedure TdxTileControlActionBarController.ProcessMouseLeftButtonClick;
var
  AItem: TdxTileControlActionBarItem;
begin
  if HitTest.HitObject = PressedCell then
  begin
    if HitTest.HitAtBackButton then
    begin
      UncheckAllItems;
      HideActionBars(abvcrActiveDetailChange);
      if TileControl.ActiveDetail <> nil then
        TileControl.ActiveDetail.TileItem.DeactivateDetail;
    end;
    if HitTest.HitAtActionButton then
    begin
      AItem := (HitTest.HitObject as TdxTileControlActionBarItemViewInfo).Item;
      HideActionBars(abvcrActionButtonClick);
      AItem.Click;
      UncheckAllItems;
    end;
  end;
end;

procedure TdxTileControlActionBarController.RefreshStates;
begin
  Owner.ViewInfo.RefreshState;
end;

procedure TdxTileControlActionBarController.SetHotCell(AValue: TObject);
begin
  if FHotCell <> AValue then
  begin
    FHotCell := AValue;
    RefreshStates;
  end;
end;

procedure TdxTileControlActionBarController.SetPressedCell(AValue: TObject);
begin
  if FPressedCell <> AValue then
  begin
    FPressedCell := AValue;
    RefreshStates;
  end;
end;

procedure TdxTileControlActionBarController.UncheckAllItems;
begin
  TileControl.Controller.UncheckAllItems;
end;

function TdxTileControlActionBarController.GetHitTest: TdxTileControlActionBarHitTest;
begin
  Result := Owner.HitTest;
end;

function TdxTileControlActionBarController.GetTileControl: TdxCustomTileControl;
begin
  Result := Owner.TileControl;
end;

{ TdxTileControlCustomActionBarViewInfo }

constructor TdxTileControlCustomActionBarViewInfo.Create(AOwner: TdxTileControlCustomActionBar);
begin
  inherited Create;
  FOwner := AOwner;
  FCells := TdxTileControlCells.Create;
end;

destructor TdxTileControlCustomActionBarViewInfo.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TdxTileControlCustomActionBarViewInfo.Calculate;
begin
  Clear;
  CalculateItems(Bounds);
  CheckBiDiModeAlignment;
end;

procedure TdxTileControlCustomActionBarViewInfo.CalculateItems(ABounds: TRect);
var
  AItem: TdxTileControlActionBarItem;
  AItemViewInfo: TdxTileControlActionBarCustomItemViewInfo;
  AItemWidth: Integer;
  I: Integer;
begin
  ABounds := cxRectInflate(ABounds, -ActionBars.IndentHorz, -ActionBars.IndentVert);

  for I := 0 to Items.Count - 1 do
  begin
    AItem := Items[I];
    if GetCanDisplayItem(AItem) and (AItem.Align = abiaLeft) then
    begin
      AItemViewInfo := CreateItemViewInfo(AItem);
      AItemWidth := AItemViewInfo.MeasureWidth;
      if AItemWidth > cxRectWidth(ABounds) then
      begin
        AItemViewInfo.Free;
        Break;
      end;
      Cells.Add(AItemViewInfo);
      AItemViewInfo.SetBounds(cxRectSetWidth(ABounds, AItemWidth), Bounds);
      ABounds.Left := AItemViewInfo.Bounds.Right + ActionBars.ItemIndent;
    end;
  end;

  for I := 0 to Items.Count - 1 do
  begin
    AItem := Items[I];
    if GetCanDisplayItem(AItem) and (AItem.Align = abiaRight) then
    begin
      AItemViewInfo := CreateItemViewInfo(AItem);
      AItemWidth := AItemViewInfo.MeasureWidth;
      if AItemWidth > cxRectWidth(ABounds) then
      begin
        AItemViewInfo.Free;
        Break;
      end;
      Cells.Add(AItemViewInfo);
      AItemViewInfo.SetBounds(cxRectSetRight(ABounds, ABounds.Right, AItemWidth), Bounds);
      ABounds.Right := AItemViewInfo.Bounds.Left - ActionBars.ItemIndent;
    end;
  end;
end;

procedure TdxTileControlCustomActionBarViewInfo.CheckBiDiModeAlignment;
var
  I: Integer;
begin
  if Owner.TileControl.ViewInfo.UseRightToLeftAlignment then
    for I := 0 to Cells.Count - 1 do
      Cells[I].DoRightToLeftConversion(Bounds);
end;

procedure TdxTileControlCustomActionBarViewInfo.Clear;
begin
  Cells.Clear;
end;

function TdxTileControlCustomActionBarViewInfo.CreateItemViewInfo(
  AItem: TdxTileControlActionBarItem): TdxTileControlActionBarCustomItemViewInfo;
begin
  Result := TdxTileControlActionBarItemViewInfo.Create(Self, AItem);
end;

procedure TdxTileControlCustomActionBarViewInfo.Draw(ACanvas: TcxCanvas);
begin
  DrawBackground(ACanvas);
  Cells.Draw(ACanvas);
end;

procedure TdxTileControlCustomActionBarViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  ACanvas.FillRect(Bounds, cxGetActualColor(
    ActionBars.Color, Painter.ActionBarDefaultBackgroundColor));
end;

function TdxTileControlCustomActionBarViewInfo.MeasureHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Cells.Count - 1 do
    Result := Max(Result, TdxTileControlCustomButtonViewInfo(Cells[I]).MeasureHeight);
  if Result > 0 then
    Inc(Result, 2 * ActionBars.IndentVert);
end;

procedure TdxTileControlCustomActionBarViewInfo.RefreshState;
begin
  inherited RefreshState;
  Cells.RefreshState;
end;

function TdxTileControlCustomActionBarViewInfo.GetActionBars: TdxTileControlActionBars;
begin
  Result := Owner.TileControl.ActionBars;
end;

function TdxTileControlCustomActionBarViewInfo.GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean;
begin
  Result := AItem.Visible;
end;

function TdxTileControlCustomActionBarViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := PtInRect(Bounds, AHitTest.HitPoint);
  if Result then
  begin
    AHitTest.BitState[tchtActionBar] := True;
    AHitTest.HitObject := Self;
    Cells.CalculateHitTest(AHitTest);
  end;
end;

function TdxTileControlCustomActionBarViewInfo.GetItems: TdxTileControlActionBarItems;
begin
  Result := ActionBars.Items;
end;

function TdxTileControlCustomActionBarViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := Owner.TileControl.Painter;
end;

function TdxTileControlCustomActionBarViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.TileControl.ScaleFactor;
end;

{ TdxTileControlActionBarItem }

constructor TdxTileControlActionBarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TdxSkinImage.Create(nil);
  FImage.OnChange := GlyphChanged;
  FPosition := abipBottomBar;
  FGlyphType := itMask;
  FVisible := True;
end;

destructor TdxTileControlActionBarItem.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxTileControlActionBarItem.Assign(Source: TPersistent);
var
  ABarItem: TdxTileControlActionBarItem;
begin
  if Source is TdxTileControlActionBarItem then
  begin
    ABarItem := TdxTileControlActionBarItem(Source);
    Align := ABarItem.Align;
    Caption := ABarItem.Caption;
    Glyph := ABarItem.Glyph;
    GlyphFrameCount := ABarItem.GlyphFrameCount;
    GlyphType := ABarItem.GlyphType;
    Position := ABarItem.Position;
    Visible := ABarItem.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TdxTileControlActionBarItem.Click;
begin
  if Assigned(OnClick) then OnClick(Self);
end;

procedure TdxTileControlActionBarItem.GlyphChanged(Sender: TObject);
begin
  Changed(True);
end;

function TdxTileControlActionBarItem.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomTileControl).ActionBars.Items;
end;

function TdxTileControlActionBarItem.GetGlyph: TdxSmartImage;
begin
  Result := Image.Texture;
end;

function TdxTileControlActionBarItem.GetGlyphFrameCount: Integer;
begin
  Result := Image.ImageCount;
end;

procedure TdxTileControlActionBarItem.SetAlign(AValue: TdxTileControlActionBarItemAlignment);
begin
  if AValue <> Align then
  begin
    FAlign := AValue;
    Changed(True);
  end;
end;

procedure TdxTileControlActionBarItem.SetCaption(const AValue: string);
begin
  if AValue <> Caption then
  begin
    FCaption := AValue;
    Changed(True);
  end;
end;

procedure TdxTileControlActionBarItem.SetGlyph(AValue: TdxSmartImage);
begin
  Image.Texture.Assign(AValue);
  Changed(True);
end;

procedure TdxTileControlActionBarItem.SetGlyphFrameCount(AValue: Integer);
begin
  Image.ImageCount := Min(AValue, 3);
end;

procedure TdxTileControlActionBarItem.SetGlyphType(AValue: TImageType);
begin
  if FGlyphType <> AValue then
  begin
    FGlyphType := AValue;
    Changed(False);
  end;
end;

procedure TdxTileControlActionBarItem.SetPosition(AValue: TdxTileControlActionBarItemPosition);
begin
  if AValue <> FPosition then
  begin
    FPosition := AValue;
    Changed(True);
  end;
end;

procedure TdxTileControlActionBarItem.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

{ TdxTileControlActionBarItems }

function TdxTileControlActionBarItems.Add: TdxTileControlActionBarItem;
begin
  Result := inherited Add as TdxTileControlActionBarItem;
end;

function TdxTileControlActionBarItems.GetItem(AIndex: Integer): TdxTileControlActionBarItem;
begin
  Result := inherited Items[AIndex] as TdxTileControlActionBarItem;
end;

function TdxTileControlActionBarItems.GetItemPrefixName: string;
begin
  Result := 'TdxTileControl';
end;

procedure TdxTileControlActionBarItems.SetItem(AIndex: Integer; AValue: TdxTileControlActionBarItem);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxTileControlCustomGroupViewInfo }

constructor TdxTileControlCustomGroupViewInfo.Create(AOwner: TdxTileControlGroup);
begin
  inherited Create;
  FGroup := AOwner;
end;

procedure TdxTileControlCustomGroupViewInfo.CalculateBounds;
var
  AWidth, AHeight: Integer;
begin
  CalculateSize(AWidth, AHeight);
  Bounds := cxRectBounds(GetLeftPos, GetTopPos, AWidth, AHeight);
end;

procedure TdxTileControlCustomGroupViewInfo.CalculateSize(var AWidth, AHeight: Integer);
begin
  AWidth := 0;
  AHeight := 0;
end;

function TdxTileControlCustomGroupViewInfo.GetBorderWidth: Integer;
begin
  Result := ScaleFactor.Apply(dxTileControlSizeGroupBorder);
end;

function TdxTileControlCustomGroupViewInfo.GetLeftPos: Integer;
begin
  Result := 0;
end;

function TdxTileControlCustomGroupViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileControl.ScaleFactor;
end;

function TdxTileControlCustomGroupViewInfo.GetTopPos: Integer;
begin
  Result := 0;
end;

function TdxTileControlCustomGroupViewInfo.GetVisibleBounds: TRect;
begin
  Result := TileControl.ViewInfo.TilesArea;
end;

function TdxTileControlCustomGroupViewInfo.GetPainter: TdxTileControlPainter;
begin
  Result := TileControl.Painter;
end;

function TdxTileControlCustomGroupViewInfo.GetTileControl: TdxCustomTileControl;
begin
  Result := Group.TileControl;
end;

{ TdxTileControlTopActionBarBackButtonViewInfo }

function TdxTileControlTopActionBarBackButtonViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest);
  if Result then
    AHitTest.BitState[tchtBackButton] := True;
end;

function TdxTileControlTopActionBarBackButtonViewInfo.GetTexture: TdxSkinImage;
begin
  Result := Owner.Owner.TileControl.Assets.BackButton;
end;

{ TdxTileControlBottomActionBar }

function TdxTileControlBottomActionBar.CreateViewInfo: TdxTileControlCustomActionBarViewInfo;
begin
  Result := TdxTileControlBottomActionBarViewInfo.Create(Self);
end;

{ TdxTileControlBottomActionBarViewInfo }

function TdxTileControlBottomActionBarViewInfo.GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean;
begin
  Result := inherited GetCanDisplayItem(AItem) and (AItem.Position = abipBottomBar);
end;

{ TdxTileControlTopActionBar }

function TdxTileControlTopActionBar.CreateViewInfo: TdxTileControlCustomActionBarViewInfo;
begin
  Result := TdxTileControlTopActionBarViewInfo.Create(Self);
end;

{ TdxTileControlTopActionBarViewInfo }

procedure TdxTileControlTopActionBarViewInfo.CalculateItems(ABounds: TRect);
var
  ABackButtonBounds: TRect;
  ABackButtonViewInfo: TdxTileControlActionBarCustomItemViewInfo;
begin
  if Owner.TileControl.ActiveDetail <> nil then
  begin
    ABackButtonViewInfo := CreateBackButtonViewInfo;
    ABackButtonBounds := cxRectInflate(ABounds, -ActionBars.IndentHorz, -ActionBars.IndentVert);
    ABackButtonBounds := cxRectSetWidth(ABackButtonBounds, ABackButtonViewInfo.MeasureWidth);
    ABackButtonViewInfo.SetBounds(ABackButtonBounds, ABounds);
    Cells.Add(ABackButtonViewInfo);
    ABounds.Left := ABackButtonBounds.Right;
  end;
  inherited CalculateItems(ABounds);
end;

function TdxTileControlTopActionBarViewInfo.CreateBackButtonViewInfo: TdxTileControlActionBarCustomItemViewInfo;
begin
  Result := TdxTileControlTopActionBarBackButtonViewInfo.Create(Self);
end;

function TdxTileControlTopActionBarViewInfo.GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean;
begin
  Result := inherited GetCanDisplayItem(AItem) and (AItem.Position = abipTopBar);
end;

{ TdxTileControlCustomViewInfo }

procedure TdxTileControlCustomViewInfo.RefreshState;
begin
end;

function TdxTileControlCustomViewInfo.IsEqual(ACompare: TdxTileControlCustomViewInfo): Boolean;
begin
  Result := (Self <> nil) and (Self = ACompare);
end;

{ TdxTileControlCustomCellViewInfo }

procedure TdxTileControlCustomCellViewInfo.Calculate;
begin
  if not Calculated then
  begin
    DoCalculate;
    FCalculated := True;
  end;
end;

procedure TdxTileControlCustomCellViewInfo.CalculateClipping;
begin
  FVisibleBounds := GetVisibleBounds;
  FVisible := cxRectIntersect(FClipRect, FVisibleBounds, FBounds);
end;

procedure TdxTileControlCustomCellViewInfo.DoCalculate;
begin
  CalculateClipping;
  RefreshState;
end;

procedure TdxTileControlCustomCellViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
end;

procedure TdxTileControlCustomCellViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if Visible then
    DoDraw(ACanvas);
end;

function TdxTileControlCustomCellViewInfo.GetHeight: Integer;
begin
  Result := 0;
end;

function TdxTileControlCustomCellViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := Visible and PtInRect(ClipRect, AHitTest.HitPoint);
  if Result then
    AHitTest.HitObject := Self;
end;

function TdxTileControlCustomCellViewInfo.GetVisibleBounds: TRect;
begin
  Result := FVisibleBounds;
end;

procedure TdxTileControlCustomCellViewInfo.Invalidate;
begin
end;

procedure TdxTileControlCustomCellViewInfo.Recalculate;
begin
  Calculated := False;
  Calculate;
end;

procedure TdxTileControlCustomCellViewInfo.Offset(const DX, DY: Integer);
begin
  FBounds := cxRectOffset(FBounds, DX, DY);
end;

procedure TdxTileControlCustomCellViewInfo.Scroll(const DX, DY: Integer);
begin
  Offset(DX, DY);
  CalculateClipping;
end;

procedure TdxTileControlCustomCellViewInfo.SetCalculate(AValue: Boolean);
begin
  if FCalculated <> AValue then
  begin
    if AValue then
      Calculate
    else
      FCalculated := AValue;
  end;
end;

procedure TdxTileControlCustomCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, AClientBounds);
  CalculateClipping;
end;

{ TdxTileControlActionBarHitTest }

constructor TdxTileControlActionBarHitTest.Create(AOwner: TdxTileControlCustomActionBar);
begin
  inherited Create(AOwner.TileControl);
  FOwner := AOwner;
end;

function TdxTileControlActionBarHitTest.GetActiveViewInfo: TdxTileControlCustomViewInfo;
begin
  Result := Owner.ViewInfo;
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressTileControl', @AddTileControlResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressTileControl');
  FreeAndNil(DetailsInfo);
  DestroyThreadScaler;
end.

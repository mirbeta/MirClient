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

unit cxPC;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms, Graphics, ImgList, SysUtils, Contnrs, Menus,
  dxCore, dxCoreGraphics, dxCoreClasses, cxClasses, cxControls, cxContainer, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, dxCustomHint, cxGeometry, dxGDIPlusClasses;

type
  TcxPCOption = (pcoAlwaysShowGoDialogButton, pcoCloseButton,
    pcoFixedTabWidthWhenRotated, pcoGoDialog, pcoGradient,
    pcoGradientClientArea, pcoNoArrows, pcoRedrawOnResize, pcoSort,
    pcoTopToBottomText, pcoUsePageColorForTab);
  TcxPCOptions = set of TcxPCOption;
  TcxPCButtonMode = (cbmNone, cbmActiveTab, cbmEveryTab, cbmActiveAndHoverTabs);
  TcxPCButtonPosition = (pcbpTabs, pcbpHeader, pcbpTabsAndHeader);
  TcxPCNewButtonMode = (nbmNone, nbmTab);

  TcxTabSlantKind = (skCutCorner, skSlant);
  TcxTabSlantPosition = (spLeft, spRight);
  TcxTabSlantPositions = set of TcxTabSlantPosition;

const
  TabsContainerOffset = 2;
  TabsContainerBaseWidth = 3;

  cxPCNoStyle = -1;
  cxPCDefaultStyle = 0;
  cxPCDefaultStyleName = 'Default';

  cxPCNewButtonIndex = -2;

  cxPCDefaultOptions = [pcoAlwaysShowGoDialogButton, pcoGradient,
    pcoGradientClientArea, pcoRedrawOnResize];

  cxTabSlantDefaultPositions = [spLeft];

  // hit test constants
  pchtNavigatorButton = 1;
  pchtTab = 2;
  pchtTabCloseButton = 3;
  pchtTabButton = 4;
  pchtHeaderButton = 5;

type
  TcxPCStyleID = -1 .. High(Integer);
  TcxPCStandardStyle = (tsTabs, tsButtons, tsFlatButtons);

  TcxTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

  TcxCustomTabControl = class;
  TcxCustomTabControlProperties = class;
  TcxCustomTabControlController = class;
  TcxCustomTabControlHitTest = class;
  TcxTab = class;
  TcxPCNewButton = class;
  TcxPCButton = class;
  TcxPageControl = class;
  TcxPCImageList = class;
  TcxTabSheet = class;
  TcxTabSheetClass = class of TcxTabSheet;
  TcxTabs = class;
  TcxTabSlants = class;

  // View Infos
  TcxTabViewInfo = class;
  TcxTabViewInfoClass = class of TcxTabViewInfo;
  TcxTabsViewInfo = class;
  TcxTabsViewInfoClass = class of TcxTabsViewInfo;
  TcxCustomTabControlViewInfo = class;
  TcxCustomTabControlViewInfoClass = class of TcxCustomTabControlViewInfo;

  // painters
  TcxPCCustomPainter = class;
  TcxPCPainterClass = class of TcxPCCustomPainter;

  // for cxGrid6 compatibility
  TcxPCOutTabImageAndTextData = record
    TabImageRect: TRect;
    TabTextRect: TRect;
    TabVisibleIndex: Integer;
  end;

  TcxPCPropertiesDrawTabEvent = procedure(AProperties: TcxCustomTabControlProperties; ATab: TcxTab;
    var DefaultDraw: Boolean) of object;
  TcxPCPropertiesDrawTabExEvent = procedure(AProperties: TcxCustomTabControlProperties; ATab: TcxTab;
    Font: TFont) of object;
  TcxGetTabImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;
  TcxGetTabHintEvent = procedure(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean) of object;
  TcxPageChangingEvent = procedure(Sender: TObject; NewPage: TcxTabSheet; var AllowChange: Boolean) of object;
  TcxPCCanCloseEventEx = procedure(Sender: TObject; ATabIndex: Integer; var ACanClose: Boolean) of object;
  TcxPCCloseEvent = procedure(Sender: TObject; ATabIndex: Integer) of object;
  TcxPCTabClickEvent = procedure(Sender: TObject; ATabVisibleIndex: Integer; AShift: TShiftState) of object;
  TcxTabChangedEvent = procedure(Sender: TObject; TabID: Integer) of object;
  TcxTabChangingEvent = procedure(Sender: TObject; var AllowChange: Boolean) of object;

  TcxTabAfterPaintEvent = procedure(ACanvas: TcxCanvas; ATab: TcxTab;
    AImageAndTextData: TcxPCOutTabImageAndTextData) of object; // for cxGrid6 compatibility

  TcxPCNavigatorButton = (nbTopLeft, nbBottomRight, nbGoDialog, nbClose);
  TcxPCNavigatorButtons = set of TcxPCNavigatorButton;
  TcxPCNavigatorButtonIndex = TcxPCNavigatorButtons;
  TcxPCNavigatorButtonState = (nbsNormal, nbsPressed, nbsHotTrack, nbsDisabled);
  TcxPCNavigatorPosition = (npLeftTop, npLeftBottom, npRightTop, npRightBottom);

  TcxPCTabsPosition = record
    ExtendedTabsRect: TRect;
    ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset: Integer;
    ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset: Integer;
    MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects: Integer;
    NormalRowWidth: Integer;
    NormalTabsRect: TRect;
  end;

  TcxPCWOffset = record
    Left, Right: Integer;
  end;

  TcxPCDistance = record
    dw, dh: Integer;
  end;

  TcxPCIndexInterval = record
    Left, Right: Integer;
  end;
  TcxPCLineIndexBoundsArray = array of TcxPCIndexInterval;

  TcxPCTabIndex = Integer;

  TcxPCTabPropertyChanged = (tpcNotSpecified, tpcColor, tpcEnabled, tpcFocused,
    tpcHighlighted, tpcHotTrack, tpcIsMainTab, tpcLayout, tpcPressed,
    tpcSelected, tpcTracking, tpcVisible);

  TcxPCTabNotification = (tnDeleting);

  TcxPCImageListRotatedImagesElement = record
    BackgroundColor: TColor;
    Bitmap: TcxBitmap;
    IsBackgroundColorSpecified: Boolean;
  end;
  TcxPCImageListRotatedImagesElementArray = array of TcxPCImageListRotatedImagesElement;

  { IcxTabControl }

  IcxTabControl = interface(IcxControlComponentState)
  ['{8C8C2262-5419-46E1-BCE0-5A5311C330A0}']
    function GetController: TcxCustomTabControlController;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;

    function CanDrawParentBackground: Boolean;
    function GetBoundsRect: TRect;
    function GetCanvas: TcxCanvas;
    function GetControl: TWinControl;
    function GetColor: TColor;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetDragAndDropState: TcxDragAndDropState;
    function GetFont: TFont;
    function GetLookAndFeel: TcxLookAndFeel;
    procedure InvalidateRect(const R: TRect; AEraseBackground: Boolean);
    procedure SetModified;

    function IsEnabled: Boolean;
    function IsFocused: Boolean;
    function IsParentBackground: Boolean;

    procedure RequestLayout;

    property Controller: TcxCustomTabControlController read GetController;
    property Painter: TcxPCCustomPainter read GetPainter;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property ViewInfo: TcxCustomTabControlViewInfo read GetViewInfo;
  end;

  { TcxPCCustomElementViewInfo }

  TcxPCCustomElementViewInfo = class(TcxIUnknownObject, IcxHintableObject)
  strict private
    FBounds: TRect;

    function GetScaleFactor: TdxScaleFactor; inline;
  protected
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function GetAbsoluteBounds: TRect; virtual;
    function GetControlViewInfo: TcxCustomTabControlViewInfo; virtual; abstract;
    function GetHint: string; virtual;
    function GetHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean; virtual;
    function GetHitTestIndex: Integer; virtual; abstract;
    function PtInElement(const APoint: TPoint): Boolean; virtual;
    procedure SetHitTest(AHitTest: TcxCustomTabControlHitTest); virtual;
  public
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean; virtual;
    function IsHintAtMousePos: Boolean; virtual;
    function UseHintHidePause: Boolean; virtual;

    property AbsoluteBounds: TRect read GetAbsoluteBounds;
    property Bounds: TRect read FBounds write FBounds;
    property ControlViewInfo: TcxCustomTabControlViewInfo read GetControlViewInfo;
    property Hint: string read GetHint;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  {TcxPCCustomButtonViewInfo}

  TcxPCCustomButtonViewInfo = class(TcxPCCustomElementViewInfo)
  strict private
    FState: TcxPCNavigatorButtonState;
  protected
    function CanClick: Boolean; virtual;
    procedure Click;
    procedure DoClick; virtual;
    procedure Invalidate; virtual;
    function GetHint: string; override;
    function GetImageIndex: TcxImageIndex; virtual;
    function PtInElement(const APoint: TPoint): Boolean; override;
    function GetState: TcxPCNavigatorButtonState; virtual;
    procedure SetState(AValue: TcxPCNavigatorButtonState); virtual;
  public
    destructor Destroy; override;
    function GetWidth: Integer; virtual; abstract;
    function HasImage: Boolean; virtual;

    property ImageIndex: TcxImageIndex read GetImageIndex;
    property State: TcxPCNavigatorButtonState read GetState write SetState;
  end;

  {TcxPCCustomTabButtonViewInfo}

  TcxPCCustomTabButtonViewInfo = class(TcxPCCustomButtonViewInfo)
  strict private
    FTabViewInfo: TcxTabViewInfo;
  protected
    function GetAbsoluteBounds: TRect; override;
    function GetControlViewInfo: TcxCustomTabControlViewInfo; override;
    function GetState: TcxPCNavigatorButtonState; override;
  public
    constructor Create(ATabViewInfo: TcxTabViewInfo);
    //
    property TabViewInfo: TcxTabViewInfo read FTabViewInfo;
  end;

  { TcxPCTabButtonViewInfo }

  TcxPCTabButtonViewInfo = class(TcxPCCustomTabButtonViewInfo)
  strict private
    FButton: TcxPCButton;
  protected
    procedure DoClick; override;
    function GetHitTestIndex: Integer; override;
    function GetImageIndex: TcxImageIndex; override;
    function GetState: TcxPCNavigatorButtonState; override;
  public
    constructor Create(ATabViewInfo: TcxTabViewInfo; AButton: TcxPCButton);
    function GetWidth: Integer; override;
    //
    property Button: TcxPCButton read FButton;
  end;

  {TcxPCTabCloseButtonViewInfo}

  TcxPCTabCloseButtonViewInfo = class(TcxPCCustomTabButtonViewInfo)
  protected
    procedure DoClick; override;
    function GetHitTestIndex: Integer; override;
  public
    function GetWidth: Integer; override;
  end;

  { TcxPCCustomHeaderButtonViewInfo }

  TcxPCCustomHeaderButtonViewInfo = class(TcxPCCustomButtonViewInfo)
  private
    FTabControlViewInfo: TcxCustomTabControlViewInfo;
  protected
    function AllowHotTrack: Boolean; virtual;
    function GetHitTestIndex: Integer; override;
    function GetControlViewInfo: TcxCustomTabControlViewInfo; override;
  public
    constructor Create(ATabControlViewInfo: TcxCustomTabControlViewInfo);
    function IsNavigatorButton: Boolean; overload; virtual;
    function IsNavigatorButton(AButtonTypes: TcxPCNavigatorButtons): Boolean; overload; virtual;
  end;

  { TcxPCHeaderButtonViewInfo }

  TcxPCHeaderButtonViewInfo = class(TcxPCCustomHeaderButtonViewInfo)
  private
    FButton: TcxPCButton;
  protected
    procedure DoClick; override;
    function GetImageIndex: TcxImageIndex; override;
    function GetState: TcxPCNavigatorButtonState; override;
  public
    constructor Create(ATabControlViewInfo: TcxCustomTabControlViewInfo; AButton: TcxPCButton);
    function GetWidth: Integer; override;
    property Button: TcxPCButton read FButton;
  end;


  { TcxPCNavigatorButtonViewInfo }

  TcxPCNavigatorButtonViewInfo = class(TcxPCCustomHeaderButtonViewInfo)
  private
    FButtonType: TcxPCNavigatorButton;
  protected
    function AllowHotTrack: Boolean; override;
    procedure DoClick; override;
    function GetHitTestIndex: Integer; override;
  public
    constructor Create(ATabControlViewInfo: TcxCustomTabControlViewInfo;
      AButtonType: TcxPCNavigatorButton);
    function GetWidth: Integer; override;
    function IsNavigatorButton(AButtonTypes: TcxPCNavigatorButtons): Boolean; override;

    property ButtonType: TcxPCNavigatorButton read FButtonType;
  end;

  { TcxPCButtonViewInfos }

  TcxPCButtonViewInfos = class
  private
    FItems: TcxObjectList;
    FTabViewInfo: TcxTabViewInfo;
    function GetCount: Integer;
    function GetItems(Index: Integer): TcxPCCustomTabButtonViewInfo;
  protected
    function GetCloseButtonInfo: TcxPCTabCloseButtonViewInfo;
  public
    constructor Create(ATabViewInfo: TcxTabViewInfo);
    destructor Destroy; override;
    procedure Add(AButtonViewInfo: TcxPCCustomTabButtonViewInfo);
    function GetHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean;
    property Count: Integer read GetCount;
    property Items [Index: Integer]: TcxPCCustomTabButtonViewInfo read GetItems; default;
  end;

  { TcxTabViewInfo }

  TcxTabViewInfo = class(TcxPCCustomElementViewInfo)
  private
    FButtonInfos: TcxPCButtonViewInfos;
    FTab: TcxTab;
    FTabsViewInfo: TcxTabsViewInfo;

    FNormalPosition: TPoint;
    FNormalWidth: Integer; // Height is in TcxTabs
    FRow: Integer;
    FVisibleRow: Integer;

    FContentRect: TRect;
    FImageRect: TRect;
    FIsMultiline: Boolean;
    FIsTextTooLong: Boolean;
    FTextRect: TRect;
    FTextSize: TSize;

    procedure TabDestroyHandler(Sender: TObject; const AEventArgs);

    function CheckHasButton(AShowMode: TcxPCButtonMode): Boolean;
    function GetAbsolutePartRect(const APartRect: TRect): TRect;
    function GetButtonsCount: Integer;
    function GetCanvas: TcxCanvas;
    function GetCaption: string;
    function GetCloseButtonInfo: TcxPCTabCloseButtonViewInfo;
    function GetColor: TColor;
    function GetContentOffset: TcxPCWOffset;
    function GetFullRect: TRect;
    function GetEnabled: Boolean;
    function GetImageAreaWidth: Integer;
    function GetIndex: Integer;
    function GetImageIndex: Integer;
    function GetNormalLongitudinalSize: Integer;
    function GetNormalRect: TRect;
    function GetPainter: TcxPCCustomPainter;
    function GetPaintingPosition: TcxTabPosition;
    function GetPaintingPositionIndex: Integer;
    function GetProperties: TcxCustomTabControlProperties;
    function GetShowAccelChar: Boolean;
    function GetTabRect: TRect;
    function GetTabImageAndTextRect: TRect;
    function GetTextSize: TSize;
    function GetTextHeight: Integer;
    function GetTextWidth: Integer;
    function GetVisibleIndex: Integer;
    function GetVisibleRect: TRect;
    function IsImagesAssigned: Boolean;
  protected
    procedure PrepareCanvasFont(ACanvas: TcxCanvas);
    procedure RecreateButtonViewInfos;

    procedure CalculateAngleDependentPartBounds(var ACalcRect: TRect;
      const APartHeight: Integer; ADrawOffset: TRect);
    procedure CalculateButtonHorizontalPositions;
    procedure CalculateButtonVerticalPositions;
    procedure CalculateContentRect;
    procedure CalculateImageAndTextHorizontalPositions;
    procedure CalculateImageHorizontalPosition;
    procedure CalculateImageVerticalPosition;
    procedure CalculateIsTextTooLong;
    procedure CalculateHorizontalPositions;
    procedure CalculateTextVerticalPosition;
    function GetContentHorizontalOffset(ATabWidth, AContentWidth: Integer): Integer;

    function GetControlViewInfo: TcxCustomTabControlViewInfo; override;
    function GetScrollingArea: TRect;
    function GetHint: string; override;
    function GetHitTestIndex: Integer; override;
    function PtInElement(const APoint: TPoint): Boolean; override;
    procedure SetHitTest(AHitTest: TcxCustomTabControlHitTest); override;

    function CanClick: Boolean; virtual;
    function CanDrag: Boolean; virtual;
    function CanFocus: Boolean; virtual;
    function CanMultiSelect: Boolean; virtual;
    function CanSelect: Boolean; virtual;
    procedure Click(AShift: TShiftState);
    procedure DoHandleDialogChar(Key: Integer); virtual;
    procedure DoSelect(AAddToSelected: Boolean); virtual;
    procedure DoSetFocus; virtual;
    procedure DoClick(AShift: TShiftState); virtual;
    procedure DoRightToLeftContentConversion(const R: TRect);
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function HandleDialogChar(Key: Integer): Boolean;
    procedure Select(AAddToSelected: Boolean);
    procedure SetFocus;

    function IsCompactMode: Boolean;
    function IsNewButton: Boolean;
    function IsPressable: Boolean; virtual;

    property Canvas: TcxCanvas read GetCanvas;
    property NormalLongitudinalSize: Integer read GetNormalLongitudinalSize;
    property NormalPosition: TPoint read FNormalPosition write FNormalPosition;
    property NormalWidth: Integer read FNormalWidth write FNormalWidth;
    property TextHeight: Integer read GetTextHeight;
    property TextWidth: Integer read GetTextWidth;
  public
    constructor Create(ATab: TcxTab; ATabsViewInfo: TcxTabsViewInfo); virtual;
    destructor Destroy; override;

    function ActuallyEnabled: Boolean;
    function ActuallyVisible: Boolean;
    function AllowMultilineCaption: Boolean;
    function HasButton(AButton: TcxPCButton): Boolean; virtual;
    function HasButtons: Boolean;
    function HasCaption: Boolean;
    function HasCloseButton: Boolean; virtual;
    function HasImage: Boolean;
    function IsActive: Boolean;
    function IsFocused: Boolean;
    function IsHighlighted: Boolean;
    function IsHotTrack: Boolean;
    function IsMainTab: Boolean;
    function IsPressed: Boolean;
    function IsSelected: Boolean;
    function IsTracking: Boolean;
    function IsVisibleForGoDialog: Boolean; virtual;

    function IsButtonsPlacedFirst: Boolean;
    function GetDefinedWidth: Integer; virtual;
    function GetLimitedWidth: Integer;
    function GetRelativeTextRotationAngle: TcxRotationAngle;

    procedure CalculateNormalWidth;
    procedure CalculateParts;
    procedure CorrectTabNormalWidth(var AValue: Integer); virtual;

    property ButtonInfos: TcxPCButtonViewInfos read FButtonInfos;
    property ButtonsCount: Integer read GetButtonsCount;
    property Caption: string read GetCaption;
    property CloseButtonInfo: TcxPCTabCloseButtonViewInfo read GetCloseButtonInfo;
    property Color: TColor read GetColor;
    property Enabled: Boolean read GetEnabled;
    property FullRect: TRect read GetFullRect;
    property ImageIndex: Integer read GetImageIndex;
    property ImageRect: TRect read FImageRect;
    property Index: Integer read GetIndex;
    property NormalRect: TRect read GetNormalRect;
    property Painter: TcxPCCustomPainter read GetPainter;
    property PaintingPosition: TcxTabPosition read GetPaintingPosition;
    property PaintingPositionIndex: Integer read GetPaintingPositionIndex;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property ShowAccelChar: Boolean read GetShowAccelChar;
    property Tab: TcxTab read FTab;
    property TabsViewInfo: TcxTabsViewInfo read FTabsViewInfo;
    property TextRect: TRect read FTextRect;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleRect: TRect read GetVisibleRect;
    property VisibleRow: Integer read FVisibleRow;
  end;

  { TcxPCNewButtonViewInfo }

  TcxPCNewButtonViewInfo = class(TcxTabViewInfo)
  private
    function GetButton: TcxPCNewButton;
  protected
    function CanClick: Boolean; override;
    function CanDrag: Boolean; override;
    function CanFocus: Boolean; override;
    function CanMultiSelect: Boolean; override;
    function CanSelect: Boolean; override;
    procedure DoClick(AShift: TShiftState); override;

    property Button: TcxPCNewButton read GetButton;
  public
    procedure CorrectTabNormalWidth(var AValue: Integer); override;
    function GetDefinedWidth: Integer; override;
    function HasButton(AButton: TcxPCButton): Boolean; override;
    function HasCloseButton: Boolean; override;
    function IsVisibleForGoDialog: Boolean; override;
  end;

  { TcxTabsViewInfo }

  TcxTabsViewInfo = class
  private
    FControlViewInfo: TcxCustomTabControlViewInfo;
    FTabs: TcxTabs;
    FViewInfos: TObjectList;

    FNeedCheckTabIndex: Boolean;
    FNeedRecreateViewInfos: Boolean;

    FTabNormalHeight: Integer;

    procedure TabsChangedHandler(Sender: TObject; const AEventArgs);
    procedure TabsDestroyHandler(Sender: TObject; const AEventArgs);

    function GetViewInfoCount: Integer;
    function GetViewInfo(Index: Integer): TcxTabViewInfo;
  protected
    procedure AddViewInfo(AViewInfo: TcxTabViewInfo);
    function GetMaxMainTabIndex: Integer;
    function IndexOf(AViewInfo: TcxTabViewInfo): Integer;

    procedure CreateViewInfos; virtual;
    procedure DestroyViewInfos; virtual;
    procedure RecreateButtonViewInfos;
    procedure RecreateViewInfos;
    procedure ResetCachedValues;

    procedure CheckTabIndex;

    function CalculateHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    procedure RepaintTab(ATabVisibleIndex: Integer; ATabPropertyChanged: TcxPCTabPropertyChanged);

    function HandleDialogChar(Key: Integer): Boolean;

    property TabNormalHeight: Integer read FTabNormalHeight;
  public
    constructor Create(ATabs: TcxTabs; AControlViewInfo: TcxCustomTabControlViewInfo); virtual;
    destructor Destroy; override;

    procedure CalculateNormalSizes;
    procedure CalculateTabParts;
    function GetTabDefaultHeight: Integer; virtual;

    property ControlViewInfo: TcxCustomTabControlViewInfo read FControlViewInfo;
    property Tabs: TcxTabs read FTabs;
    property ViewInfoCount: Integer read GetViewInfoCount;
    property ViewInfos[Index: Integer]: TcxTabViewInfo read GetViewInfo; default;
  end;

  { TcxCustomTabControlViewInfo }

  TcxCustomTabControlViewInfo = class
  private
    FExtendedBottomOrRightTabsRect: TRect;
    FExtendedTopOrLeftTabsRect: TRect;
    FFirstTabVisibleIndex: Integer;
    FFocusedTabVisibleIndex: Integer;
    FHotTrackNavigatorButton: TcxPCCustomHeaderButtonViewInfo;
    FHotTrackTabButton: TcxPCCustomTabButtonViewInfo;
    FHotTrackTabVisibleIndex: Integer;
    FIControl: IcxTabControl;
    FIsCustomTextColorAssigned: Boolean;
    FIsLastTabFullyVisible: Boolean;
    FLastTabVisibleIndex: Integer;
    FMainTabVisibleIndex: Integer;
    FMaxTabCaptionWidth: Integer;
    FOwner: TObject;
    FPressedNavigatorButton: TcxPCCustomHeaderButtonViewInfo;
    FPressedTabButton: TcxPCCustomTabButtonViewInfo;
    FPressedTabVisibleIndex: Integer;
    FRowCount: Integer;
    FRowHeight: Integer;
    FScaleFactor: TdxScaleFactor;
    FTabButtonHeight: Integer;
    FTabCalculatingIndex: Integer;
    FTabsPosition: TcxPCTabsPosition;
    FTabsViewInfo: TcxTabsViewInfo;
    FTopOrLeftPartRowCount: Integer;
    FTrackingTabVisibleIndex: Integer;
    FUpdating: Boolean;

    FOnAfterPaintTab: TcxTabAfterPaintEvent;

    FHeaderButtonHeight: Integer;
    FNavigatorButtons: TcxPCNavigatorButtons;
    FNavigatorButtonInfos: TObjectList;

    function CanPressButton(AButton: TcxPCNavigatorButton): Boolean;
    function GetBoundsRect: TRect;
    function GetCanvas: TcxCanvas;
    function GetClientRect: TRect;
    function GetColor: TColor;
    function GetControlBounds: TRect;
    function GetController: TcxCustomTabControlController;
    function GetFocusedTabVisibleIndex: Integer;
    function GetFont: TFont;
    function GetHeight: Integer;
    function GetHideTabs: Boolean;
    function GetImageBorder: Integer;
    function GetLineWidth(const ALineIndexBoundsA: TcxPCLineIndexBoundsArray; ALineNumber, ATabsDistance: Integer): Integer;
    function GetMainTabIndex: Integer;
    function GetMultiLine: Boolean;
    function GetMultiLineTabCaptions: Boolean;
    function GetNavigatorButtonCount: Integer;
    function GetNavigatorButtonInfos(Index: Integer): TcxPCCustomHeaderButtonViewInfo;
    function GetNavigatorButtons: TcxPCNavigatorButtons;
    function GetNavigatorPosition: TcxPCNavigatorPosition;
    function GetOptions: TcxPCOptions;
    function GetPageClientRect: TRect;
    function GetPageClientRectOffset: TRect;
    function GetPageFrameRect: TRect;
    function GetPageFrameRectOffset: TRect;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetRaggedRight: Boolean;
    function GetShowFrame: Boolean;
    function GetTabHeight: Smallint;
    function GetTabPosition: TcxTabPosition;
    function GetTabsAreaRect: TRect;
    function GetTabSlantPositions: TcxTabSlantPositions;
    function GetTabSlants: TcxTabSlants;
    function GetTabsScroll: Boolean;
    function GetTabWidth: Smallint;
    function GetWidth: Integer;
    function IsInverseNavigatorButtonsOrder: Boolean;
    procedure RearrangeRows;
    procedure ResetHotTrack;
    procedure SetFocusedTabVisibleIndex(Value: Integer);
    procedure SetHotTrackTabButton(const Value: TcxPCCustomTabButtonViewInfo);
    procedure SetHotTrackNavigatorButton(const Value: TcxPCCustomHeaderButtonViewInfo);
    procedure SetHotTrackTabVisibleIndex(Value: Integer);
    procedure SetMainTabVisibleIndex(Value: Integer);
    procedure CreateHeaderButtons;
    procedure SetPressedNavigatorButton(const Value: TcxPCCustomHeaderButtonViewInfo);
    procedure SetPressedTabButton(const Value: TcxPCCustomTabButtonViewInfo);
    procedure SetPressedTabVisibleIndex(Value: Integer);
    procedure SetTrackingTabVisibleIndex(Value: Integer);
    procedure SynchronizeHotTrackStates(Shift: TShiftState);
    procedure UpdateButtonsState;
    procedure UpdateNavigatorButtons(AOnlyObligatoryButtons: Boolean);
    procedure UpdateTabPosition(AShowButtons: Boolean);
  protected
    procedure BeginCalculate;
    procedure EndCalculate;
    function IsUpdating: Boolean;

    function GetTabViewInfo(ATab: TcxTab): TcxTabViewInfo;
    function GetTabVisibleIndex(ATab: TcxTab): Integer;

    function CanFocusOnClick(X, Y: Integer): Boolean; virtual;
    function GetPainterClass: TcxPCPainterClass; virtual;

    procedure CalculateHitTest(AHitTest: TcxCustomTabControlHitTest);
    procedure CalculateLongitudinalTabPositions;
    procedure CalculateRowHeight;
    procedure CalculateRowPositions;
    procedure CalculateTabsPositions;
    procedure DoCalculate; virtual;

    function DoGetTabIndex: Integer; virtual;
    procedure DoSetTabIndex(Value: Integer); virtual;
    function GetTabIndex: Integer;
    procedure SetTabIndex(Value: Integer);

    procedure AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab; AImageAndTextData: TcxPCOutTabImageAndTextData); virtual;
    function ArrowButtonClick(ANavigatorButton: TcxPCNavigatorButton): Boolean;
    procedure ButtonDestroying(AElementInfo: TcxPCCustomElementViewInfo);
    procedure ElementDestroying(AElementInfo: TcxPCCustomElementViewInfo);
    procedure CorrectFirstTabVisibleIndex(ATabVisibleIndex: Integer);
    function GetActivePageColor: TColor; virtual;
    function GetNavigatorButtonInfoByType(AType: TcxPCNavigatorButton): TcxPCNavigatorButtonViewInfo;
    function GetNextFocusedTabVisibleIndex(ACurrentTabVisibleIndex, ADelta: Integer; ACycle: Boolean; AOnlyAllowSelectedTabs: Boolean): Integer;
    function GetTabColor(ATabVisibleIndex: Integer): TColor; virtual;
    function GetTabExtendedTabsRect(ATabViewInfo: TcxTabViewInfo): TRect; overload;
    function GetTabExtendedTabsRect(ATabVisibleIndex: Integer): TRect; overload;
    function GetTabImageAreaWidth: Integer;
    function GetTabImageAreaHeight: Integer;
    procedure InitializeLineBoundsA(var ALineIndexBoundsA: TcxPCLineIndexBoundsArray; AFirstIndex, ALastIndex: Integer);
    function HasActivePage: Boolean; virtual;
    function HasBorders: Boolean; virtual;
    procedure MakeTabVisible(ATabVisibleIndex: Integer);
    procedure PlaceVisibleTabsOnRows(ATabsWidth, ATabsDistance: Integer);
    procedure RepaintTab(ATabVisibleIndex: Integer; ATabPropertyChanged: TcxPCTabPropertyChanged);
    procedure SetMainTab;
    procedure TabDestroying(ATabViewInfo: TcxTabViewInfo);
    procedure TabDown(ATabVisibleIndex: Integer; AShift: TShiftState);
    procedure TabUp(ATabVisibleIndex: Integer; AShift: TShiftState);
    function UseActivePageColor: Boolean; virtual;
    procedure DestroyTabs;

    function ActuallyRotatedTabsMaxWidth: Integer;
    function AllowHotTrack: Boolean;
    function AllowMultiSelect: Boolean;
    function AllowDisabledTabAccess: Boolean;
    function CanMouseWheel(const AMousePos: TPoint): Boolean;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function HasNavigatorButton(AType: TcxPCNavigatorButton): Boolean;
    function PtInTab(ATabVisibleIndex: Integer; X, Y: Integer): Boolean;
    function IsCustomTextColorAssigned: Boolean;
    function IsHeaderButtonImagesAssigned: Boolean;
    function IsTabFullVisible(ATabVisibleIndex: Integer): Boolean;
    function IsSpecialAlignment: Boolean;
    function IsTooSmallControlSize: Boolean;
    function IsTabActuallyVisible(ATabViewInfo: TcxTabViewInfo): Boolean;
    function IsTabAccessible(AIndex: Integer): Boolean;
    function IsTabButtonImagesAssigned: Boolean;
    function IsTabImagesAssigned: Boolean;
    function IsTabVisibleIndexValid(AIndex: Integer): Boolean;
    function PtInScrollingArea(const P: TPoint; var ADirection: Integer): Boolean;

    property Controller: TcxCustomTabControlController read GetController;
    property HotTrackNavigatorButton: TcxPCCustomHeaderButtonViewInfo read FHotTrackNavigatorButton write SetHotTrackNavigatorButton;
    property HotTrackTabButton: TcxPCCustomTabButtonViewInfo read FHotTrackTabButton write SetHotTrackTabButton;
    property HotTrackTabVisibleIndex: Integer read FHotTrackTabVisibleIndex write SetHotTrackTabVisibleIndex;
    property LastTabVisibleIndex: Integer read FLastTabVisibleIndex write FLastTabVisibleIndex;
    property MainTabIndex: Integer read GetMainTabIndex;
    property Owner: TObject read FOwner;
    property PageFrameRect: TRect read GetPageFrameRect;
    property PageFrameRectOffset: TRect read GetPageFrameRectOffset;
    property Painter: TcxPCCustomPainter read GetPainter;
    property PressedNavigatorButton: TcxPCCustomHeaderButtonViewInfo read FPressedNavigatorButton write SetPressedNavigatorButton;
    property PressedTabButton: TcxPCCustomTabButtonViewInfo read FPressedTabButton write SetPressedTabButton;
    property PressedTabVisibleIndex: Integer read FPressedTabVisibleIndex write SetPressedTabVisibleIndex;
    property RowHeight: Integer read FRowHeight;
    property TrackingTabVisibleIndex: Integer read FTrackingTabVisibleIndex write SetTrackingTabVisibleIndex;
    property OnAfterPaintTab: TcxTabAfterPaintEvent read FOnAfterPaintTab write FOnAfterPaintTab; // for cxGrid6 compatibility
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    procedure Calculate;

    function ActuallyRotate: Boolean;
    function CanDrawParentBackground: Boolean;
    function GetHeaderButtonsDistance(AButtonInfo1, AButtonInfo2: TcxPCCustomHeaderButtonViewInfo): Integer;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetOptimalSize: Integer;
    function GetPopupOwner: TComponent; virtual;
    function GetTabImageSize: TSize;
    function GetTextRotationAngle: TcxRotationAngle;
    function GetSizeDeficit: Integer;
    function HasTabCloseButtons: Boolean;
    function HasTabButtons: Boolean;
    function IndexOfTabAt(X, Y: Integer): Integer;
    procedure InitializeLineBoundsArray(var ALineIndexBoundsA: TcxPCLineIndexBoundsArray);
    procedure InitializeVisibleTabRange(var AFirstIndex, ALastIndex: Integer);
    procedure InvalidateRect(const R: TRect; EraseBackground: Boolean);
    function IsBottomToTopAlignment: Boolean;
    function IsEnabled: Boolean;
    function IsFocused: Boolean;
    function IsNativePainting: Boolean;
    function IsRightToLeftAlignment: Boolean;
    function IsTabsContainer: Boolean;
    function IsTabsOnBothSides: Boolean;
    function IsTabsVisible: Boolean;
    function IsTransparent: Boolean; virtual;
    function IsVerticalText: Boolean;
    function ParentBackground: Boolean;
    procedure PrepareTabCanvasFont(ATabViewInfo: TcxTabViewInfo; ACanvas: TcxCanvas); virtual;
    function VisibleIndexOfTabAt(X, Y: Integer): Integer;
    function UseRightToLeftAlignment: Boolean; virtual;
    function UseRightToLeftReading: Boolean; virtual;

    property BoundsRect: TRect read GetBoundsRect;
    property Canvas: TcxCanvas read GetCanvas;
    property ClientRect: TRect read GetClientRect;
    property Color: TColor read GetColor;
    property ControlBounds: TRect read GetControlBounds;
    property FirstTabVisibleIndex: Integer read FFirstTabVisibleIndex write FFirstTabVisibleIndex;
    property FocusedTabVisibleIndex: Integer read GetFocusedTabVisibleIndex write SetFocusedTabVisibleIndex;
    property Font: TFont read GetFont;
    property HeaderButtonHeight: Integer read FHeaderButtonHeight;
    property Height: Integer read GetHeight;
    property HideTabs: Boolean read GetHideTabs;
    property IControl: IcxTabControl read FIControl;
    property ImageBorder: Integer read GetImageBorder;
    property MainTabVisibleIndex: Integer read FMainTabVisibleIndex write SetMainTabVisibleIndex;
    property MultiLine: Boolean read GetMultiLine;
    property MultiLineTabCaptions: Boolean read GetMultiLineTabCaptions;
    property NavigatorButtonCount: Integer read GetNavigatorButtonCount;
    property NavigatorButtonInfoByType[AType: TcxPCNavigatorButton]: TcxPCNavigatorButtonViewInfo read GetNavigatorButtonInfoByType;
    property NavigatorButtonInfos [Index: Integer]: TcxPCCustomHeaderButtonViewInfo read GetNavigatorButtonInfos;
    property NavigatorButtons: TcxPCNavigatorButtons read GetNavigatorButtons;
    property NavigatorPosition: TcxPCNavigatorPosition read GetNavigatorPosition;
    property Options: TcxPCOptions read GetOptions;
    property PageClientRect: TRect read GetPageClientRect;
    property PageClientRectOffset: TRect read GetPageClientRectOffset;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property RaggedRight: Boolean read GetRaggedRight;
    property RowCount: Integer read FRowCount;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property ShowFrame: Boolean read GetShowFrame;
    property TabButtonHeight: Integer read FTabButtonHeight;
    property TabColors[ATabVisibleIndex: Integer]: TColor read GetTabColor;
    property TabHeight: Smallint read GetTabHeight;
    property TabIndex: Integer read GetTabIndex write SetTabIndex;
    property TabPosition: TcxTabPosition read GetTabPosition;
    property TabsAreaRect: TRect read GetTabsAreaRect;
    property TabSlants: TcxTabSlants read GetTabSlants;
    property TabSlantPositions: TcxTabSlantPositions read GetTabSlantPositions;
    property TabsScroll: Boolean read GetTabsScroll;
    property TabsViewInfo: TcxTabsViewInfo read FTabsViewInfo;
    property TabWidth: Smallint read GetTabWidth;
    property TopOrLeftPartRowCount: Integer read FTopOrLeftPartRowCount;
    property Width: Integer read GetWidth;
  end;

  TcxTab = class(TPersistent,
    IUnknown,
    IdxAdornerTargetElement)
  private
    FAllowCloseButton: Boolean;
    FCaption: string; // type of TStrings' item
    FColor: TColor;
    FDestroyHandlers: TcxEventHandlerCollection;
    FEnabled: Boolean;
    FImageIndex: TcxImageIndex;
    FObject: TObject;
    FShowAccelChar: Boolean;

    FSelected: Boolean;
    FHighlighted: Boolean;

    FTabs: TcxTabs;
    FVisible: Boolean;
    function GetFullRect: TRect;
    function GetHotTrack: Boolean;
    function GetImageIndex: TcxImageIndex;
    function GetIsMainTab: Boolean;
    function GetPressed: Boolean;
    function GetProperties: TcxCustomTabControlProperties;
    function GetRealVisible: Boolean;
    function GetTracking: Boolean;

    function GetControlViewInfo: TcxCustomTabControlViewInfo;
    function GetViewInfo: TcxTabViewInfo;

    function GetVisibleIndex: Integer;
    function GetVisibleRect: TRect;
    function GetVisibleRow: Integer;
    procedure InternalSetCaption(const Value: string);
    procedure SetAllowCloseButton(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetColor(Value: TColor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetHighlighted(const Value: Boolean);
    procedure SetImageIndex(Value: TcxImageIndex);
    procedure SetSelected(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(ATabPropertyChanged: TcxPCTabPropertyChanged);
    procedure DoDestroy; virtual;
    function GetViewInfoClass: TcxTabViewInfoClass; virtual;
    function GetIndex: Integer; virtual;
    function IsNewButton: Boolean; virtual;

    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;

    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    property DestroyHandlers: TcxEventHandlerCollection read FDestroyHandlers;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property Tabs: TcxTabs read FTabs;
    property ShowAccelChar: Boolean read FShowAccelChar write FShowAccelChar;
  public
    constructor Create(ATabs: TcxTabs); virtual;
    destructor Destroy; override;

    property AllowCloseButton: Boolean read FAllowCloseButton write SetAllowCloseButton;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Data: TObject read FObject;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property FullRect: TRect read GetFullRect;
    property Highlighted: Boolean read FHighlighted write SetHighlighted;
    property HotTrack: Boolean read GetHotTrack;
    property ImageIndex: TcxImageIndex read GetImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property IsMainTab: Boolean read GetIsMainTab;
    property Pressed: Boolean read GetPressed;
    property RealVisible: Boolean read GetRealVisible;
    property Selected: Boolean read FSelected write SetSelected;
    property Tracking: Boolean read GetTracking;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleRect: TRect read GetVisibleRect;
    property VisibleRow: Integer read GetVisibleRow;
  end;

  { TcxPCNewButton }

  TcxPCNewButton = class(TcxTab)
  private
    FWidth: Integer;
    procedure SetWidth(Value: Integer);
  protected
    function GetViewInfoClass: TcxTabViewInfoClass; override;
    function GetIndex: Integer; override;
    procedure DoDestroy; override;
    function IsNewButton: Boolean; override;
  public
    constructor Create(ATabs: TcxTabs); override;

    property Width: Integer read FWidth write SetWidth;
  end;
  TcxPCNewButtonClass = class of TcxPCNewButton;

  TcxTabs = class(TStrings)
  private
    FChangedHandlers: TcxEventHandlerCollection;
    FDestroyHandlers: TcxEventHandlerCollection;
    FIsTabsCleaning: Boolean;
    FNewButton: TcxPCNewButton;
    FProperties: TcxCustomTabControlProperties;
    FTabItems: TObjectList;
    function GetTab(TabIndex: Integer): TcxTab;
    function GetVisibleTab(ATabVisibleIndex: Integer): TcxTab;
    procedure SetTab(Index: Integer; const Value: TcxTab);
    function GetVisibleTabsCount: Integer;

    function GetControlViewInfo: TcxCustomTabControlViewInfo;
    function GetViewInfo: TcxTabsViewInfo;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;

    procedure Changed(ATab: TcxTab = nil; ATabPropertyChanged: TcxPCTabPropertyChanged = tpcLayout);
    function GetViewInfoClass: TcxTabsViewInfoClass; virtual;
    function GetNewButtonClass: TcxPCNewButtonClass; virtual;

    procedure CreateNewButton; virtual;
    procedure DestroyNewButton; virtual;
    procedure RemoveTab(ATab: TcxTab);

    property ChangedHandlers: TcxEventHandlerCollection read FChangedHandlers;
    property DestroyHandlers: TcxEventHandlerCollection read FDestroyHandlers;
    property NewButton: TcxPCNewButton read FNewButton;
    property Properties: TcxCustomTabControlProperties read FProperties;
    property TabItems: TObjectList read FTabItems;
  public
    constructor Create(AProperties: TcxCustomTabControlProperties);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;

    property Tabs[TabIndex: Integer]: TcxTab read GetTab write SetTab; default;
    property VisibleTabsCount: Integer read GetVisibleTabsCount;
    property VisibleTabs[TabVisibleIndex: Integer]: TcxTab read GetVisibleTab;
  end;

  TcxPCCustomPainter = class
  strict private
    FIsDragImagePainted: Boolean;
    FViewInfo: TcxCustomTabControlViewInfo;

    function GetDisabledTextFaceColor: TColor;
    function GetDisabledTextShadowColor: TColor;
    function GetHeaderButtonImagePainter: TcxPCImageList;
    function GetHighlightedTabBodyColor: TColor;
    function GetImagePainter: TcxPCImageList;
    function GetScaleFactor: TdxScaleFactor;
    function GetTabButtonImagePainter: TcxPCImageList;
    function GetTabViewInfo(Index: Integer): TcxTabViewInfo;
    function GetTabsContainerOffset: Integer;
  protected
    class function AllowRotate: Boolean; virtual;
    class function AllowMultiLineTabCaptions: Boolean; virtual;
    function CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer; virtual; abstract;
    function CanDrawParentBackground: Boolean; virtual;
    procedure CorrectTabContentVerticalOffset(ATabVisibleIndex: Integer; var ADrawOffset: TRect); virtual;
    procedure CorrectTabNormalWidth(var AValue: Integer); virtual;
    function GetButtonDrawOffsets: TRect; virtual;
    function GetDropArrowRects(ADragTabVisibleIndex, AHitTabVisibleIndex, ADestinationTabVisibleIndex: Integer): TRects;
    function GetTabCorrection(ATabVisibleIndex: Integer): TRect; virtual;
    procedure DirectionalPolyline(ACanvas: TcxCanvas; const R: TRect; APoints: array of TPoint; ATabPosition: TcxTabPosition; AColor: TColor);
    function DoCustomDraw(TabVisibleIndex: Integer): Boolean;
    procedure DoDrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DoDrawTabButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); virtual; abstract;
    procedure DoDrawTabCloseButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState); virtual; abstract;
    function DoGetButtonHeight: Integer; virtual; abstract;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; virtual; abstract;
    function DoGetCloseButtonSize: TSize; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawHeaderButtonGlyph(ACanvas: TcxCanvas; AHeaderButtonInfo: TcxPCHeaderButtonViewInfo);
    procedure DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet); virtual;
    procedure DrawTabButtons(ACanvas: TcxCanvas; TabVisibleIndex: Integer);
    procedure DrawTabImage(ACanvas: TcxCanvas; const ARect: TRect; AImageIndex: Integer;
      AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer); virtual;
    procedure DrawTabImageAndText(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    procedure DrawTabText(ACanvas: TcxCanvas; const ARect: TRect; const AText: string;
      AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer); virtual;
    procedure ExcludeTabContentClipRegion(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
    procedure FillFreeSpaceArea(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure FillPageClientRect(ACanvas: TcxCanvas); virtual;
    procedure FillTabPaneContent(ACanvas: TcxCanvas); virtual;
    function GetButtonHeight: Integer; virtual;
    function GetButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; virtual;
    function GetButtonsRegionFromTabsOffset: Integer; virtual;
    function GetButtonsRegionHOffset: Integer; virtual;
    function GetButtonsRegionWOffset: Integer; virtual;
    function GetButtonWidth(Button: TcxPCNavigatorButton): Integer; virtual;
    function GetClientColor: TColor; virtual;
    function GetCloseButtonAreaHeight(ATabVisibleIndex: Integer): Integer; virtual;
    function GetCloseButtonAreaWidth(ATabVisibleIndex: Integer): Integer; virtual;
    function GetCloseButtonSize: TSize; virtual;
    function GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect; virtual;
    function GetFillClientRect: TRect; virtual;
    function GetFreeSpaceColor: TColor; virtual;
    function GetHeaderButtonGlyphOffset: TRect; virtual;
    function GetHeaderButtonGlyphPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette; virtual;
    function GetPageBorders: TcxBorders;
    function GetPageClientRect: TRect;
    function GetPageClientRectOffset: TRect; virtual;
    function GetDefaultClientColor: TColor; virtual;
    function GetPageFrameRect: TRect; virtual;
    function GetPageFrameRectOffset: TRect; virtual;
    function GetDrawImageOffset(TabVisibleIndex: Integer): TRect; virtual; abstract;
    function GetDrawImageWithoutTextWOffset(TabVisibleIndex: Integer): TcxPCWOffset; virtual; abstract;
    function GetDrawTextHOffset(TabVisibleIndex: Integer): TRect; virtual; abstract;
    function GetDTFlags(ATabVisibleIndex: Integer): Integer;
    function GetCTFlags(ATabVisibleIndex: Integer): Integer;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual;

    function GetBorderWidths: TRect; virtual;
    function GetExtendedRect(const ARect, AExtension: TRect; ATabPosition: TcxTabPosition): TRect;
    function GetFrameWidth: Integer; virtual;

    function GetTabButtonColorPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette; virtual;
    function GetTabButtonDistance: Integer; virtual;
    function GetTabButtonGlyphOffset: TRect; virtual;
    function GetTabButtonsAreaWidth(ATabVisibleIndex: Integer): Integer; virtual;

    procedure AfterPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual;
    function AlwaysColoredTabs: Boolean; virtual;
    function GetGoDialogPosition(GoDialogSize: TSize): TPoint; virtual; abstract;
    function GetHighlightedTextColor(ATabVisibleIndex: Integer; ATextColor: TColor): TColor;
    function GetHotTrackColor: TColor;
    function GetImageTextDistance(ATabVisibleIndex: Integer): Integer; virtual; abstract;
    function GetMaxTabCaptionHeight: Integer;
    function GetMinTabNormalWidth(ATabVisibleIndex: Integer): Integer; virtual; abstract;
    function GetMinTabSelectionDistance: TcxPCDistance; virtual; abstract;
    function GetNativeContentOffset: TRect; virtual;
    function GetTabBaseImageSize: TSize;
    function GetTabBodyColor(TabVisibleIndex: Integer): TColor; virtual; abstract;
    function GetTabColor(ATabVisibleIndex: Integer): TColor; virtual;
    function GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion; virtual;
    function GetTabClipRgnOperation(ATabVisibleIndex: Integer): TcxRegionOperation; virtual;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; virtual; abstract;
    function GetTabImageAreaHeight: Integer;
    function GetTabImageAreaWidth: Integer;
    function GetTabImageColorPalette: IdxColorPalette; virtual;
    procedure GetTabNativePartAndState(ATabVisibleIndex: Integer; out PartId, StateId: Integer); virtual;
    function GetTabNativeState(ATabVisibleIndex: Integer): Integer;
    function GetTabRotatedImageSize: TSize;
    function GetTabsContainerOffsets: TRect; virtual;
    function GetTabsNormalDistance: TcxPCDistance; virtual;
    function GetTabsPosition: TcxPCTabsPosition; virtual; abstract;
    function GetTextColor(ATabVisibleIndex: Integer): TColor; virtual;
    function GetTooNarrowTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; virtual; abstract;
    function HasActivePage: Boolean;
    procedure Init; virtual;
    procedure InternalDrawText(ACanvas: TcxCanvas; const ACaption: string; ARect: TRect; ATabVisibleIndex: Integer); virtual;
    function InternalGetPageFrameRectOffset: TRect; virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); virtual;
    procedure InternalPaintDragImage(ACanvas: TcxCanvas; ATabVisibleIndex: Integer); virtual; abstract;
    procedure InternalPolyLine(const APoints: array of TPoint; AColor: TColor; ACanvas: TcxCanvas);
    procedure InternalResetClipRegion(ACanvas: TcxCanvas);
    function InternalSetClipRect(ACanvas: TcxCanvas; ClipR: TRect; IntersectWithCurrentClipRegion: Boolean = True): Boolean;
    procedure InvalidateTabExtendedTabsRect(TabVisibleIndex: Integer);
    procedure InvalidateTabRect(ATabVisibleIndex: Integer); virtual;
    function IsAssignedImages: Boolean;
    function IsEnableHotTrack: Boolean; virtual;
    function IsEnouphSpaceForClientPage: Boolean;
    function IsNativePainting: Boolean; virtual;
    function PtInTab(TabVisibleIndex: Integer; X, Y: Integer): Boolean; virtual;
    function IsPaintHeadersAreaFirst: Boolean; virtual;
    function IsTabHasImage(ATabVisibleIndex: Integer): Boolean;
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; virtual; abstract;
    function IsTabTransparent(ATabVisibleIndex: Integer): Boolean; virtual;
    function IsTabsRectVisible(ACanvas: TcxCanvas): Boolean; virtual;
    function NeedDisabledTextShadow: Boolean; virtual;
    function NeedDoubleBuffer: Boolean; virtual;
    function NeedRedrawOnResize: Boolean; virtual;
    function NeedShowFrame: Boolean; virtual;
    procedure PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton); virtual; abstract;
    procedure PaintButtonsRegion(ACanvas: TcxCanvas); virtual; abstract;
    procedure PaintDragImage(ACanvas: TcxCanvas; const R: TRect; ATabVisibleIndex: Integer);
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); virtual;
    procedure PaintHeadersArea(ACanvas: TcxCanvas); virtual;
    procedure PaintPageFrame(ACanvas: TcxCanvas);
    procedure PaintTab(ACanvas: TcxCanvas; TabVisibleIndex: Integer); virtual;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); virtual; abstract;
    procedure PrepareDrawTabContentBitmapBackground(ABitmap: TcxBitmap;
      const ABitmapPos: TPoint; ATabVisibleIndex: Integer); virtual;
    procedure DrawTabContentBackground(ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer); virtual;
    procedure RepaintButtonsRegion; virtual;
    procedure RepaintTab(TabVisibleIndex: Integer; TabPropertyChanged: TcxPCTabPropertyChanged); virtual;
    procedure RotatePoint(const R: TRect; var P: TPoint;
      ATabPosition: TcxTabPosition);
    procedure RotatePolyline(const R: TRect; var APoints: array of TPoint;
      ATabPosition: TcxTabPosition);
    function UseActivePageColor: Boolean;
    function UseLookAndFeelTabButton: Boolean; virtual;

    function IsDragImagePainted: Boolean;
    procedure StartDragImagePainted;
    procedure StopDragImagePainted;

    property TabsContainerOffset: Integer read GetTabsContainerOffset;
    property DisabledTextFaceColor: TColor read GetDisabledTextFaceColor;
    property DisabledTextShadowColor: TColor read GetDisabledTextShadowColor;
    property HighlightedTabBodyColor: TColor read GetHighlightedTabBodyColor;
  public
    constructor Create(AViewInfo: TcxCustomTabControlViewInfo); virtual;

    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure PaintPageClientArea(ACanvas: TcxCanvas); virtual;

    function CalculateTabNormalHeight: Integer; virtual; abstract;
    class function GetStandardStyle: TcxPCStandardStyle; virtual;
    class function GetStyleID: TcxPCStyleID; virtual;
    class function GetStyleName: TCaption; virtual;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; virtual;
    class function IsDefault(ALookAndFeel: TcxLookAndFeel): Boolean; virtual;
    class function IsMainTabBoundWithClient: Boolean; virtual;
    class function IsMultiSelectionAccepted: Boolean; virtual;
    class function IsStandardStyle: Boolean; virtual;
    class function IsTabPressable: Boolean; virtual;

    property HeaderButtonImagePainter: TcxPCImageList read GetHeaderButtonImagePainter;
    property ImagePainter: TcxPCImageList read GetImagePainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property TabButtonImagePainter: TcxPCImageList read GetTabButtonImagePainter;
    property TabViewInfo [Index: Integer]: TcxTabViewInfo read GetTabViewInfo;
    property ViewInfo: TcxCustomTabControlViewInfo read FViewInfo;
  end;

  { TcxPCImageList }

  TcxPCImageList = class
  strict private
    FBaseHotImageChangeLink: TChangeLink;
    FBaseHotImages: TCustomImageList;
    FBaseImageChangeLink: TChangeLink;
    FBaseImages: TCustomImageList;
    FFreeNotificator: TcxFreeNotificator;
    FImageRotationAngle: TcxRotationAngle;
    FProperties: TcxCustomTabControlProperties;

    FOnChange: TNotifyEvent;

    procedure BaseImageListChange(Sender: TObject);
    procedure Change;
    procedure FreeNotification(AComponent: TComponent);
    class procedure OutError(SourceMethodName, Msg: TCaption);
    procedure SetBaseHotImages(const Value: TCustomImageList);
    procedure SetBaseImages(const Value: TCustomImageList);
    function GetBaseImageSize: TSize;
    function GetImages(AHot: Boolean): TCustomImageList;
    function GetRotatedImageSize: TSize;
  public
    constructor Create(AProperties: TcxCustomTabControlProperties);
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; const R: TRect; AIndex: Integer;
      ABackgroundColor: TColor; AIsNativePainting, AEnabled, AHot: Boolean; APalette: IdxColorPalette);
    function IsImagesAssigned: Boolean;

    property BaseHotImages: TCustomImageList read FBaseHotImages write SetBaseHotImages;
    property BaseImages: TCustomImageList read FBaseImages write SetBaseImages;
    property BaseImageSize: TSize read GetBaseImageSize;
    property ImageRotationAngle: TcxRotationAngle read FImageRotationAngle write FImageRotationAngle default ra0;
    property RotatedImageSize: TSize read GetRotatedImageSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TcxTabSlants }

  TcxTabSlants = class(TPersistent)
  private
    FKind: TcxTabSlantKind;
    FOwner: TPersistent;
    FPositions: TcxTabSlantPositions;
    FOnChange: TNotifyEvent;
    procedure Changed;
    procedure SetKind(Value: TcxTabSlantKind);
    procedure SetPositions(Value: TcxTabSlantPositions);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Kind: TcxTabSlantKind read FKind write SetKind default skSlant;
    property Positions: TcxTabSlantPositions read FPositions write SetPositions default cxTabSlantDefaultPositions;
  end;

  TcxPCGoDialogClickEvent = procedure(ATabVisibleIndex: Integer) of object;

  TcxPCCustomGoDialog = class
  private
    FTabControl: IcxTabControl;
    FOnClick: TcxPCGoDialogClickEvent;
    function GetViewInfo: TcxCustomTabControlViewInfo;
  public
    constructor Create(ATabControl: IcxTabControl); virtual;
    function Popup(X, Y: Integer): Boolean; virtual; abstract;

    property TabControl: IcxTabControl read FTabControl;
    property ViewInfo: TcxCustomTabControlViewInfo read GetViewInfo;
    property OnClick: TcxPCGoDialogClickEvent read FOnClick write FOnClick;
  end;

  { TcxTabControlDragAndDropObject }

  TcxTabControlDragAndDropObject = class(TcxDragAndDropObject)
  private
    FHitTabVisibleIndex: Integer;
    FDragImage: TcxDragImage;
    FDestinationArrowFirst: TcxDragAndDropArrow;
    FDestinationArrowSecond: TcxDragAndDropArrow;
    FDestinationTabVisibleIndex: Integer;
    FIControl: IcxTabControl;
    FScrollingDirection: Integer;
    FScrollingTimer: TcxTimer;
    FDragPointOffset: TPoint;
    FTabVisibleIndex: Integer;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    procedure ScrollingTimerHandler(Sender: TObject);
    procedure SetDestinationTabVisibleIndex(Value: Integer);
  protected
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    procedure CheckScrolling(const P: TPoint);
    procedure CreateDestinationImage;
    procedure CreateDragImage;
    procedure CreateScrollingTimer;
    procedure DestroyDestinationImage;
    procedure DestroyDragImage;
    procedure DestroyScrollingTimer;
    procedure DoPaintDragImage; virtual;
    procedure Drop(Accepted: Boolean); virtual;
    function GetClientCursorPos: TPoint; override;
    function GetDragDestinationTabIndex: Integer;
    function GetDragTabIndex: Integer;
    procedure PaintDragImage;
    procedure ShowDragImage;
    property DestinationTabVisibleIndex: Integer read FDestinationTabVisibleIndex write SetDestinationTabVisibleIndex;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property TabVisibleIndex: Integer read FTabVisibleIndex;
    property ViewInfo: TcxCustomTabControlViewInfo read GetViewInfo;
  public
    constructor Create(AIControl: IcxTabControl); reintroduce; virtual;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    procedure Init(ATabVisibleIndex: Integer; const P: TPoint); virtual;
  end;
  TcxTabControlDragAndDropObjectClass = class of TcxTabControlDragAndDropObject;

  {TcxCustomTabControlHitTest}

  TcxCustomTabControlHitTest = class
  private
    FController: TcxCustomTabControlController;
    FHitObject: TObject;
    FFlags: Int64;
    FHitPoint: TPoint;
    FHitTab: TcxTabViewInfo;
    FShift: TShiftState;
    function GetBitState(AIndex: Integer): Boolean;
    function GetHitAtHeaderButton: Boolean;
    function GetHitAtTabButton: Boolean;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
  protected
    procedure Clear;
    procedure Recalculate;
    procedure Update(AShift: TShiftState; const APoint: TPoint);
  public
    constructor Create(AOwner: TcxCustomTabControlController);

    property HitAtHeaderButton: Boolean read GetHitAtHeaderButton;
    property HitAtNavigatorButton: Boolean index pchtNavigatorButton read GetBitState;
    property HitAtTab: Boolean index pchtTab read GetBitState;
    property HitAtTabCloseButton: Boolean index pchtTabCloseButton read GetBitState;
    property HitAtTabButton: Boolean read GetHitAtTabButton;
    property HitObject: TObject read FHitObject;
    property HitPoint: TPoint read FHitPoint;
    property HitTab: TcxTabViewInfo read FHitTab;
    property Shift: TShiftState read FShift;
  end;

  { TcxCustomTabControlHintHelper }

  TcxCustomTabControlHintHelper = class(TcxCustomHintHelper)
  private
    FController: TcxCustomTabControlController;
  protected
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerWinControl: TWinControl; override;
    property Controller: TcxCustomTabControlController read FController;
  public
    constructor Create(AController: TcxCustomTabControlController);
  end;

  TcxCustomTabControlHintHelperClass = class of TcxCustomTabControlHintHelper;

  { TcxCustomTabControlController }

  TcxCustomTabControlControllerClass = class of TcxCustomTabControlController;
  TcxCustomTabControlController = class(TcxIUnknownObject)
  private
    FHintHelper: TcxCustomTabControlHintHelper;
    FHintObject: TcxPCCustomElementViewInfo;
    FHitTest: TcxCustomTabControlHitTest;
    FIControl: IcxTabControl;
    FOwner: TObject;
    FTimer: TcxTimer;
    FMouseDownTabVisibleIndex: Integer;

    FGoDialog: TcxPCCustomGoDialog;
    FIsGoDialogShowing: Boolean;
    FGoDialogJustClosed: Boolean;

    procedure ArrowButtonMouseDown(ANavigatorButton: TcxPCNavigatorButton);
    procedure ArrowButtonClick(ANavigatorButton: TcxPCNavigatorButton);
    procedure CloseButtonClick;
    procedure CreateScrollingTimer;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    function IsScrollTimerStarted: Boolean;
    function IsTabPressable: Boolean;
    procedure StartScrollTimer;
    procedure StopScrollTimer;
    procedure TimerEventHandler(Sender: TObject);
  protected
    procedure CheckHint; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure FocusChanged; virtual;
    function HandleDialogChar(Key: Integer): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;

    procedure DoTabClick(ATabVisibleIndex: Integer; AShift: TShiftState); virtual;
    procedure TabClick(ATabVisibleIndex: Integer; AShift: TShiftState; AMouseEvent: Boolean); virtual;
    procedure TabDown(ATabVisibleIndex: Integer; AShift: TShiftState); virtual;
    procedure TabUp(ATabVisibleIndex: Integer; AShift: TShiftState); virtual;
    procedure TabButtonDown(ATabButtonInfo: TcxPCCustomTabButtonViewInfo); virtual;

    //Drag and drop
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function GetDragAndDropObjectClass: TcxTabControlDragAndDropObjectClass; virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; virtual;

    procedure CreateGoDialog;
    procedure DoShowGoDialog; virtual;
    function GetClientToScreen(const APoint: TPoint): TPoint; virtual;
    function GetMouseCursorPos: TPoint; virtual;
    function GetScreenToClient(const APoint: TPoint): TPoint; virtual;
    procedure GoDialogClickEventHandler(ATabVisibleIndex: Integer); virtual;
    procedure HideGoDialog(ATabVisibleIndex: Integer);
    procedure ShowGoDialog;

    function GetControlHandle: THandle; virtual;
    function GetHintHelperClass: TcxCustomTabControlHintHelperClass; virtual;

    property HintHelper: TcxCustomTabControlHintHelper read FHintHelper;
    property IControl: IcxTabControl read FIControl;
    property MouseDownTabVisibleIndex: Integer read FMouseDownTabVisibleIndex write FMouseDownTabVisibleIndex;
    property Owner: TObject read FOwner;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property ViewInfo: TcxCustomTabControlViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    procedure ScrollTabs(ADelta: Integer);
    property HitTest: TcxCustomTabControlHitTest read FHitTest;
  end;

  { TcxCustomTabControlProperties }

  TcxCustomTabControlPropertiesChangedType = (pctHard, pctMedium, pctSimple, pctLight);

  TcxCustomTabControlPropertiesChangedEvent = procedure(Sender: TObject; AType: TcxCustomTabControlPropertiesChangedType) of object;
  TcxPCPrepareTabCanvasFontEvent = procedure(ATab: TcxTab; ACanvas: TcxCanvas) of object;

  TcxPCNewTabButtonClickEvent = procedure (Sender: TObject; var AHandled: Boolean) of object;
  TcxPCNewTabCreateEvent = procedure(Sender: TObject; AIndex: Integer) of object;
  TcxPCPaintDragImageEvent = procedure(Sender: TObject; ABitmap: TBitmap; var ADone: Boolean) of object;

  TcxPCNewEvent = procedure(Sender: TObject; AIndex: Integer) of object; // decrecated

  TcxPCButtonCanShowEvent = procedure(AButton: TcxPCButton; ATabIndex: Integer; var ACanShow: Boolean) of object;

  { TcxPCButton }

  TcxPCButton = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FHeaderImageIndex: TcxImageIndex;
    FHint: string;
    FPosition: TcxPCButtonPosition;
    FTabImageIndex: TcxImageIndex;
    FVisible: Boolean;
    FOnCanShow: TcxPCButtonCanShowEvent;
    FOnClick: TNotifyEvent;
    procedure SetEnabled(AValue: Boolean);
    procedure SetHeaderImageIndex(AValue: TcxImageIndex);
    procedure SetHint(const AValue: string);
    procedure SetPosition(AValue: TcxPCButtonPosition);
    procedure SetTabImageIndex(AValue: TcxImageIndex);
    procedure SetVisible(AValue: Boolean);
  protected
    function DoCanShow(ATabIndex: Integer): Boolean;
    procedure DoClick;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property HeaderImageIndex: TcxImageIndex read FHeaderImageIndex write SetHeaderImageIndex default -1;
    property Hint: string read FHint write SetHint;
    property Position: TcxPCButtonPosition read FPosition write SetPosition default pcbpHeader;
    property TabImageIndex: TcxImageIndex read FTabImageIndex write SetTabImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnCanShow: TcxPCButtonCanShowEvent read FOnCanShow write FOnCanShow;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TcxPCButtons }

  TcxPCButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TcxPCButton;
    procedure SetItem(Index: Integer; Value: TcxPCButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TcxPCButton;

    property Items[Index: Integer]: TcxPCButton read GetItem write SetItem; default;
  end;

  { TcxPCCustomButtons }

  TcxPCCustomButtons = class(TcxOwnedPersistent)
  private
    FButtons: TcxPCButtons;
    FHeaderImages: TcxPCImageList;
    FMode: TcxPCButtonMode;
    FProperties: TcxCustomTabControlProperties;
    FTabImages: TcxPCImageList;
    function GetCount: Integer;
    function GetHeaderImages: TCustomImageList;
    function GetTabImages: TCustomImageList;
    procedure SetButtons(AValue: TcxPCButtons);
    procedure SetHeaderImages(AValue: TCustomImageList);
    procedure SetMode(AValue: TcxPCButtonMode);
    procedure SetTabImages(AValue: TCustomImageList);
  protected
    procedure Changed;
    function CreateButtons: TcxPCButtons; virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
  published
    property Buttons: TcxPCButtons read FButtons write SetButtons;
    property HeaderImages: TCustomImageList read GetHeaderImages write SetHeaderImages;
    property Mode: TcxPCButtonMode read FMode write SetMode default cbmNone;
    property TabImages: TCustomImageList read GetTabImages write SetTabImages;
  end;

  { TcxCustomTabControlProperties }

  TcxCustomTabControlProperties = class(TcxOwnedPersistent)
  private
    FActivateFocusedTab: Boolean;
    FAllowDisabledTabAccess: Boolean;
    FAllowTabDragDrop: Boolean;
    FCloseButtonMode: TcxPCButtonMode;
    FCloseTabWithMiddleClick: Boolean;
    FCustomButtons: TcxPCCustomButtons;
    FDragImage: TdxSmartGlyph;
    FHideTabs: Boolean;
    FHotTrack: Boolean;
    FIsTabsContainer: Boolean;
    FImages: TcxPCImageList;
    FImageBorder: Integer;
    FMultiLine: Boolean;
    FMultiLineTabCaptions: Boolean;
    FMultiSelect: Boolean;
    FNavigatorPosition: TcxPCNavigatorPosition;
    FNewTabIndex: Integer;
    FOptions: TcxPCOptions;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FRotate: Boolean;
    FRotatedTabsMaxWidth: Integer;
    FScrollOpposite: Boolean;
    FShowFrame: Boolean;
    FShowButtonHints: Boolean;
    FShowTabHints: Boolean;
    FStyle: TcxPCStyleID;
    FTabCaptionAlignment: TAlignment;
    FTabChanging: Boolean;
    FTabIndex: Integer;
    FTabPosition: TcxTabPosition;
    FTabs: TcxTabs;
    FTabsScroll: Boolean;
    FTabSize: TSmallPoint;
    FTabSlants: TcxTabSlants;

    // Locking
    FLockedEvents: TList;
    FNotifyEventLockCount: Integer;
    FChangingEventLockCount: Integer;
    FChangeEventLockCount: Integer;
    FUpdateLockCount: Integer;

    FOnChange: TNotifyEvent;
    FOnChanged: TcxCustomTabControlPropertiesChangedEvent;
    FOnChanging: TcxTabChangingEvent;
    FOnStyleChanged: TNotifyEvent;

    FOnCanClose: TcxPCCanCloseEventEx;
    FOnClose: TcxPCCloseEvent;
    FOnDrawTab: TcxPCPropertiesDrawTabEvent;
    FOnDrawTabEx: TcxPCPropertiesDrawTabExEvent;
    FOnGetImageIndex: TcxGetTabImageEvent;
    FOnGetTabHint: TcxGetTabHintEvent;
    FOnNewTabButtonClick: TcxPCNewTabButtonClickEvent;
    FOnNewTabCreate: TcxPCNewTabCreateEvent;
    FOnPaintDragImage: TcxPCPaintDragImageEvent;
    FOnPrepareTabCanvasFont: TcxPCPrepareTabCanvasFontEvent;
    FOnTabClick: TcxPCTabClickEvent;

    procedure TabSlantsChangedHandler(Sender: TObject);

    function GetHeaderButtonImages: TCustomImageList;
    function GetHotImages: TCustomImageList;
    function GetImages: TCustomImageList;
    function GetNewButton: TcxPCNewButton;
    function GetNewButtonMode: TcxPCNewButtonMode;
    function GetOptions: TcxPCOptions;
    function GetTabButtonImages: TCustomImageList;
    procedure SetActivateFocusedTab(Value: Boolean);
    procedure SetAllowDisabledTabAccess(const Value: Boolean);
    procedure SetCloseButtonMode(const Value: TcxPCButtonMode);
    procedure SetCustomButtons(AValue: TcxPCCustomButtons);
    procedure SetDragImage(const Value: TdxSmartGlyph);
    procedure SetHideTabs(Value: Boolean);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetHotTrack(Value: Boolean);
    procedure SetImageBorder(Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetIsTabsContainer(Value: Boolean);
    procedure SetRotatedTabsMaxWidth(Value: Integer);
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiLineTabCaptions(Value: Boolean);
    procedure SetNavigatorPosition(const Value: TcxPCNavigatorPosition);
    procedure SetNewButtonMode(Value: TcxPCNewButtonMode);
    procedure SetOptions(Value: TcxPCOptions);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetRotate(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetShowFrame(Value: Boolean);
    procedure SetStyle(const Value: TcxPCStyleID);
    procedure SetTabCaptionAlignment(Value: TAlignment);
    procedure SetTabHeight(Value: Smallint);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(Value: TcxTabPosition);
    procedure SetTabs(const Value: TcxTabs);
    procedure SetTabsScroll(Value: Boolean);
    procedure SetTabSlants(Value: TcxTabSlants);
    procedure SetTabWidth(const Value: Smallint);
  protected
    procedure DoAssign(Source: TPersistent); override;

    procedure AddDeferredEvent(AEvent: TNotifyEvent; ASender: TObject);
    procedure CallLockedEvents;
    function IsChangeEventLocked: Boolean;
    function IsChangingEventLocked: Boolean;
    function IsNotifyEventLocked: Boolean;
    function IsUpdateLocked: Boolean;
    procedure LockChangeEvent;
    procedure UnlockChangeEvent;
    procedure LockChangingEvent;
    procedure UnlockChangingEvent;
    procedure LockNotifyEvents;
    procedure UnLockNotifyEvents;

    function CanChange(NewTabIndex: Integer): Boolean;
    function CanCloseButtonShow(ATab: TcxTab): Boolean; virtual;
    procedure Changed(AType: TcxCustomTabControlPropertiesChangedType = pctHard);
    procedure ChangeScale(M, D: Integer); virtual;
    function DoCanChange(ANewTabIndex: Integer): Boolean; virtual;
    procedure DoChange; dynamic;
    procedure DoChanging(ANewTabIndex: Integer; var AAllowChange: Boolean); virtual;
    procedure DoChanged(AType: TcxCustomTabControlPropertiesChangedType = pctHard); virtual;
    function DoPaintDragImage(AImage: TcxDragImage): Boolean; virtual;
    procedure DoPrepareTabCanvasFont(ATab: TcxTab; ACanvas: TcxCanvas); virtual;
    procedure DoStyleChanged;
    procedure DoSetTabIndex(Value: Integer); virtual;
    function GetTabIndex: Integer; virtual;
    procedure TabIndexChanged; virtual;

    function CanProcessChanged: Boolean; virtual;
    procedure CloseActiveTab; virtual;
    procedure CloseTab(AIndex: Integer); virtual;
    function CreateCustomButtons: TcxPCCustomButtons; virtual;
    function CreateNewTab: Integer; virtual;
    function DoCanClose(AIndex: Integer): Boolean; virtual;
    procedure DoClose(ATabIndex: Integer); virtual;
    procedure DoCloseTab(AIndex: Integer); virtual;
    procedure DoCloseButtonClick(ATabIndex: Integer); virtual;
    procedure DoGetTabHint(ATab: TcxTab; var AHint: string; var ACanShow: Boolean); virtual;
    function DoNewTabButtonClick: Boolean; virtual;
    procedure DoNewTabCreate(AIndex: Integer); virtual;
    procedure DoTabClick(ATabVisibleIndex: Integer; AShift: TShiftState); virtual;
    procedure NewButtonClick;

    procedure MoveTab(ACurrentIndex, ANewIndex: Integer); virtual;

    procedure SetModified;

    function DoCustomDraw(TabVisibleIndex: Integer): Boolean; dynamic;
    procedure DoDrawTabEx(ATabVisibleIndex: Integer; AFont: TFont); virtual;

    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    function GetTabControl: IcxTabControl; virtual;
    function GetViewInfo: TcxCustomTabControlViewInfo;

    procedure ImageListChange(Sender: TObject); virtual;
    function InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string; virtual;
    function GetButtonHint(AButtonViewInfo: TcxPCCustomButtonViewInfo): string; virtual;
    function GetImageIndex(ATab: TcxTab): Integer; virtual;
    function GetNavigatorButtons(AOnlyObligatoryButtons: Boolean): TcxPCNavigatorButtons;
    function GetTabHint(ATab: TcxTab): string; virtual;
    function TabIndexTabMustBeVisible: Boolean; virtual;

    property ActivateFocusedTab: Boolean read FActivateFocusedTab write SetActivateFocusedTab default True;
    property NewButtonMode: TcxPCNewButtonMode read GetNewButtonMode write SetNewButtonMode default nbmNone;
    property AllowDisabledTabAccess: Boolean read FAllowDisabledTabAccess write SetAllowDisabledTabAccess default False;
    property AllowTabDragDrop: Boolean read FAllowTabDragDrop write FAllowTabDragDrop default False;
    property CloseButtonMode: TcxPCButtonMode read FCloseButtonMode write SetCloseButtonMode default cbmNone;
    property CloseTabWithMiddleClick: Boolean read FCloseTabWithMiddleClick write FCloseTabWithMiddleClick default False;
    property CustomButtons: TcxPCCustomButtons read FCustomButtons write SetCustomButtons;
    property DragImage: TdxSmartGlyph read FDragImage write SetDragImage;
    property HeaderButtonImages: TCustomImageList read GetHeaderButtonImages;
    property HideTabs: Boolean read FHideTabs write SetHideTabs default False;
    property HotImages: TCustomImageList read GetHotImages write SetHotImages;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property ImageBorder: Integer read FImageBorder write SetImageBorder default 0;
    property Images: TCustomImageList read GetImages write SetImages;
    property IsTabsContainer: Boolean read FIsTabsContainer write SetIsTabsContainer default False;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiLineTabCaptions: Boolean read FMultiLineTabCaptions write SetMultiLineTabCaptions default False;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
    property NavigatorPosition: TcxPCNavigatorPosition read FNavigatorPosition write SetNavigatorPosition default npRightTop;
    property NewTabIndex: Integer read FNewTabIndex;
    property Options: TcxPCOptions read GetOptions write SetOptions default cxPCDefaultOptions;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property Rotate: Boolean read FRotate write SetRotate default False;
    property RotatedTabsMaxWidth: Integer read FRotatedTabsMaxWidth write SetRotatedTabsMaxWidth default 0;
    property ScrollOpposite: Boolean read FScrollOpposite write SetScrollOpposite default False;
    property ShowButtonHints: Boolean read FShowButtonHints write FShowButtonHints default False;
    property ShowFrame: Boolean read FShowFrame write SetShowFrame default False;
    property ShowTabHints: Boolean read FShowTabHints write FShowTabHints default False;
    property Style: TcxPCStyleID read FStyle write SetStyle default cxPCDefaultStyle;
    property TabButtonImages: TCustomImageList read GetTabButtonImages;
    property TabCaptionAlignment: TAlignment read FTabCaptionAlignment write SetTabCaptionAlignment default taCenter;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TcxTabPosition read FTabPosition write SetTabPosition default tpTop;
    property Tabs: TcxTabs read FTabs write SetTabs;
    property TabsScroll: Boolean read FTabsScroll write SetTabsScroll default True;
    property TabSlants: TcxTabSlants read FTabSlants write SetTabSlants;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;

    property OnCanClose: TcxPCCanCloseEventEx read FOnCanClose write FOnCanClose;
    property OnClose: TcxPCCloseEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanged: TcxCustomTabControlPropertiesChangedEvent read FOnChanged write FOnChanged;
    property OnChanging: TcxTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TcxPCPropertiesDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnDrawTabEx: TcxPCPropertiesDrawTabExEvent read FOnDrawTabEx write FOnDrawTabEx;
    property OnGetImageIndex: TcxGetTabImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetTabHint: TcxGetTabHintEvent read FOnGetTabHint write FOnGetTabHint;
    property OnNewTabButtonClick: TcxPCNewTabButtonClickEvent read FOnNewTabButtonClick write FOnNewTabButtonClick;
    property OnNewTabCreate: TcxPCNewTabCreateEvent read FOnNewTabCreate write FOnNewTabCreate;
    property OnStyleChanged: TNotifyEvent read FOnStyleChanged write FOnStyleChanged;
    property OnPaintDragImage: TcxPCPaintDragImageEvent read FOnPaintDragImage write FOnPaintDragImage;
    property OnPrepareTabCanvasFont: TcxPCPrepareTabCanvasFontEvent read FOnPrepareTabCanvasFont write FOnPrepareTabCanvasFont;
    property OnTabClick: TcxPCTabClickEvent read FOnTabClick write FOnTabClick;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    property NewButton: TcxPCNewButton read GetNewButton;
  end;
  TcxCustomTabControlPropertiesClass = class of TcxCustomTabControlProperties;

  { TcxTabControlProperties }

  TcxTabControlProperties = class(TcxCustomTabControlProperties)
  published
    property ActivateFocusedTab;
    property AllowTabDragDrop;
    property CloseButtonMode;
    property CloseTabWithMiddleClick;
    property CustomButtons;
    property DragImage;
    property HideTabs;
    property HotImages;
    property HotTrack;
    property ImageBorder;
    property Images;
    property MultiLine;
    property MultiLineTabCaptions;
    property MultiSelect;
    property NavigatorPosition;
    property NewButtonMode;
    property Options;
    property OwnerDraw;
    property RaggedRight;
    property Rotate;
    property RotatedTabsMaxWidth;
    property ScrollOpposite;
    property ShowButtonHints;
    property ShowFrame;
    property ShowTabHints;
    property Style;
    property TabCaptionAlignment;
    property TabHeight;
    property TabIndex;
    property TabPosition;
    property Tabs;
    property TabsScroll;
    property TabSlants;
    property TabWidth;
  end;

  TcxDrawTabEvent = procedure(AControl: TcxCustomTabControl; ATab: TcxTab;  var DefaultDraw: Boolean) of object;
  TcxDrawTabExEvent = procedure(AControl: TcxCustomTabControl; ATab: TcxTab; Font: TFont) of object;
  TcxPCCanCloseEvent = procedure(Sender: TObject; var ACanClose: Boolean) of object;
  TcxTabControlTabDragAndDropEvent = procedure(AControl: TcxCustomTabControl; AIndex, ADestinationIndex: Integer; var AAccept: Boolean) of object;
  TcxTabControlTabStartDragEvent = procedure(AControl: TcxCustomTabControl; AIndex: Integer) of object;
  TcxTabControlTabEndDragEvent = procedure(AControl: TcxCustomTabControl; ANewIndex: Integer) of object;

  { TcxCustomTabControl }

  TcxCustomTabControl = class(TcxControl,
    IcxTabControl,
    IcxControlComponentState,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2,
    IdxSkinSupport)
  private
    FController: TcxCustomTabControlController;
    FFocusable: Boolean;
    FPainter: TcxPCCustomPainter;
    FProperties: TcxCustomTabControlProperties;
    FViewInfo: TcxCustomTabControlViewInfo;

    FClientRect: TRect;
    FIsClientRectLoaded: Boolean;
    FScalingFlags: TScalingFlags;

    FOnCanClose: TcxPCCanCloseEvent;
    FOnCanCloseEx: TcxPCCanCloseEventEx;
    FOnChange: TNotifyEvent;
    FOnChanging: TcxTabChangingEvent;
    FOnDrawTab: TcxDrawTabEvent;
    FOnDrawTabEx: TcxDrawTabExEvent;
    FOnGetImageIndex: TcxGetTabImageEvent;
    FOnGetTabHint: TcxGetTabHintEvent;
    FOnNewTabButtonClick: TcxPCNewTabButtonClickEvent;
    FOnNewTabCreate: TcxPCNewTabCreateEvent;
    FOnPaintDragImage: TcxPCPaintDragImageEvent;
    FOnTabDragAndDrop: TcxTabControlTabDragAndDropEvent;
    FOnTabEndDrag: TcxTabControlTabEndDragEvent;
    FOnTabStartDrag: TcxTabControlTabStartDragEvent;

    function GetFirstVisibleTab: Integer;
    function GetHideTabs: Boolean;
    function GetHotTrack: Boolean;
    function GetImageBorder: Integer;
    function GetImages: TCustomImageList;
    function GetIsTabsContainer: Boolean;
    function GetMultiLine: Boolean;
    function GetMultiSelect: Boolean;
    function GetNavigatorPosition: TcxPCNavigatorPosition;
    function GetOptions: TcxPCOptions;
    function GetOwnerDraw: Boolean;
    function GetRaggedRight: Boolean;
    function GetRotate: Boolean;
    function GetMaxRotatedTabWidth: Integer;
    function GetScrollOpposite: Boolean;
    function GetShowFrame: Boolean;
    function GetTabCaptionAlignment: TAlignment;
    function GetTabHeight: Smallint;
    function GetTabIndex: Integer;
    function GetTabPosition: TcxTabPosition;
    function GetTabSlants: TcxTabSlants;
    function GetTabWidth: Smallint;
    function GetOnNew: TcxPCNewEvent;
    procedure SetFirstVisibleTab(Value: Integer);
    procedure SetHideTabs(const Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetIsTabsContainer(Value: Boolean);
    procedure SetImageBorder(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetNavigatorPosition(const Value: TcxPCNavigatorPosition);
    procedure SetOptions(Value: TcxPCOptions);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetRaggedRight(const Value: Boolean);
    procedure SetRotate(const Value: Boolean);
    procedure SetMaxRotatedTabWidth(Value: Integer);
    procedure SetScrollOpposite(const Value: Boolean);
    procedure SetShowFrame(const Value: Boolean);
    procedure SetTabCaptionAlignment(Value: TAlignment);
    procedure SetTabHeight(const Value: Smallint);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(const Value: TcxTabPosition);
    procedure SetTabSlants(Value: TcxTabSlants);
    procedure SetTabWidth(const Value: Smallint);
    procedure SetOnNew(const Value: TcxPCNewEvent);

    // other
    function GetPageClientRect: TRect;
    function GetPageClientRectOffset: TRect;
    function GetMainTabIndex: Integer;
    function GetStyle: TcxPCStyleID;
    function GetTabs: TcxTabs;
    function InternalGetClientRect: TRect;
    procedure ReadClientRectBottom(Reader: TReader);
    procedure ReadClientRectLeft(Reader: TReader);
    procedure ReadClientRectRight(Reader: TReader);
    procedure ReadClientRectTop(Reader: TReader);
    procedure SetProperties(const Value: TcxCustomTabControlProperties);
    procedure SetStyle(const Value: TcxPCStyleID);
    procedure SetTabs(const Value: TcxTabs);
    procedure WriteClientRectBottom(Writer: TWriter);
    procedure WriteClientRectLeft(Writer: TWriter);
    procedure WriteClientRectRight(Writer: TWriter);
    procedure WriteClientRectTop(Writer: TWriter);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPrintClient(var Message: TWMPrintClient); message WM_PRINTCLIENT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    // CustomControl inherited methods
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Paint; override;

    // cxControls inherited methods
    function CreateDragAndDropObject: TcxDragAndDropObject; override;
    function CanFocusOnClick(X, Y: Integer): Boolean; override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    procedure EraseBackground(DC: HDC); override;
    function HasBackground: Boolean; override;
    procedure FocusChanged; override;
    procedure FontChanged; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function NeedsScrollBars: Boolean; override;
    function NeedRedrawOnResize: Boolean; override;

    // mouse inherited methods
    function DoMouseWheelDown(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;  MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // Event Handlers
    procedure PropertiesCanCloseHandler(Sender: TObject; AIndex: Integer; var ACanClose: Boolean);
    procedure PropertiesChangeHandler(Sender: TObject);
    procedure PropertiesChangedHandler(Sender: TObject; AType: TcxCustomTabControlPropertiesChangedType);
    procedure PropertiesChangingHandler(Sender: TObject; var AllowChange: Boolean);
    procedure PropertiesDrawTabExHandler(AControl: TcxCustomTabControlProperties; ATab: TcxTab; Font: TFont);
    procedure PropertiesDrawTabHandler(AControl: TcxCustomTabControlProperties; ATab: TcxTab; var DefaultDraw: Boolean);
    procedure PropertiesGetImageIndexHandler(Sender: TObject; TabIndex: Integer;  var ImageIndex: Integer);
    procedure PropertiesGetTabHintHandler(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
    procedure PropertiesNewTabButtonClickHandler(Sender: TObject; var AHandled: Boolean);
    procedure PropertiesNewTabCreateHandler(Sender: TObject; AIndex: Integer);
    procedure PropertiesPaintDragImageHandler(Sender: TObject; ABitmap: TBitmap; var ADone: Boolean);
    procedure PropertiesPrepareTabCanvasFontHandler(ATab: TcxTab; ACanvas: TcxCanvas); virtual;
    procedure PropertiesTabClickHandler(Sender: TObject; ATabVisibleIndex: Integer; AShift: TShiftState); virtual;
    function PropertiesTabDragAndDrop(AIndex, ADestinationIndex: Integer): Boolean;
    procedure PropertiesTabEndDrag(ANewIndex: Integer);
    procedure PropertiesTabStartDrag(AIndex: Integer);

    procedure AfterLoaded; virtual;
    procedure AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab; AImageAndTextData: TcxPCOutTabImageAndTextData); virtual;
    procedure Change; virtual;
    function HandleDialogChar(Key: Integer): Boolean; virtual;
    function InternalKeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    class procedure OutError(SourceMethodName: TCaption; Msg: TCaption);
    procedure LayoutChanged(ANeedRealign: Boolean); virtual;
    procedure RecreatePainter;
    procedure RequestLayout; dynamic;
    procedure SetModified;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateTabImages;

    // ChildClasses
    function GetControllerClass: TcxCustomTabControlControllerClass; virtual;
    function GetPainterClass: TcxPCPainterClass;
    function GetPropertiesClass: TcxCustomTabControlPropertiesClass; virtual;
    function GetViewInfoClass: TcxCustomTabControlViewInfoClass; virtual;

    // Drag and drop
    function AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function AllowDragAndDropWithoutFocus: Boolean; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    // IcxTabControl
    function CanDrawParentBackground: Boolean;
    function GetBoundsRect: TRect;
    function GetCanvas: TcxCanvas;
    function GetControl: TWinControl;
    function GetController: TcxCustomTabControlController;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetDragAndDropState: TcxDragAndDropState;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    function IsEnabled: Boolean;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsFocused: Boolean;
    function IsLoading: Boolean;
    function IsParentBackground: Boolean;

    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingCallerMouseLeave;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    procedure IcxMouseTrackingCaller2.MouseLeave = MouseTrackingCallerMouseLeave;
    procedure MouseTrackingCallerMouseLeave;

    // Adorners
    procedure GetAdornerTargetElements(AList: TStrings); override;

    property FirstVisibleTab: Integer read GetFirstVisibleTab write SetFirstVisibleTab;
    property Focusable: Boolean read FFocusable write FFocusable default True;
    property HideTabs: Boolean read GetHideTabs write SetHideTabs stored False;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack stored False;
    property IsTabsContainer: Boolean read GetIsTabsContainer write SetIsTabsContainer stored False;
    property ImageBorder: Integer read GetImageBorder write SetImageBorder stored False;
    property Images: TCustomImageList read GetImages write SetImages stored False;
    property MainTabIndex: Integer read GetMainTabIndex;
    property MultiLine: Boolean read GetMultiLine write SetMultiLine stored False;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect stored False;
    property NavigatorPosition: TcxPCNavigatorPosition read GetNavigatorPosition write SetNavigatorPosition stored False;
    property Options: TcxPCOptions read GetOptions write SetOptions stored False;
    property OwnerDraw: Boolean read GetOwnerDraw write SetOwnerDraw stored False;
    property PageClientRect: TRect read GetPageClientRect;
    property PageClientRectOffset: TRect read GetPageClientRectOffset;
    property RaggedRight: Boolean read GetRaggedRight write SetRaggedRight stored False;
    property Rotate: Boolean read GetRotate write SetRotate stored False;
    property MaxRotatedTabWidth: Integer read GetMaxRotatedTabWidth write SetMaxRotatedTabWidth stored False;
    property ScrollOpposite: Boolean read GetScrollOpposite write SetScrollOpposite stored False;
    property ShowFrame: Boolean read GetShowFrame write SetShowFrame stored False;
    property Style: TcxPCStyleID read GetStyle write SetStyle stored False;
    property TabCaptionAlignment: TAlignment read GetTabCaptionAlignment write SetTabCaptionAlignment stored False;
    property TabHeight: Smallint read GetTabHeight write SetTabHeight stored False;
    property TabIndex: Integer read GetTabIndex write SetTabIndex stored False;
    property TabPosition: TcxTabPosition read GetTabPosition write SetTabPosition stored False;
    property TabSlants: TcxTabSlants read GetTabSlants write SetTabSlants;
    property TabWidth: Smallint read GetTabWidth write SetTabWidth stored False;
    property Tabs: TcxTabs read GetTabs write SetTabs stored False;

    property OnCanClose: TcxPCCanCloseEvent read FOnCanClose write FOnCanClose;
    property OnCanCloseEx: TcxPCCanCloseEventEx read FOnCanCloseEx write FOnCanCloseEx;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TcxTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TcxDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnDrawTabEx: TcxDrawTabExEvent read FOnDrawTabEx write FOnDrawTabEx;
    property OnGetImageIndex: TcxGetTabImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetTabHint: TcxGetTabHintEvent read FOnGetTabHint write FOnGetTabHint;
    property OnPaintDragImage: TcxPCPaintDragImageEvent read FOnPaintDragImage write FOnPaintDragImage;
    property OnNewTabButtonClick: TcxPCNewTabButtonClickEvent read FOnNewTabButtonClick write FOnNewTabButtonClick;
    property OnNewTabCreate: TcxPCNewTabCreateEvent read FOnNewTabCreate write FOnNewTabCreate;
    property OnNew: TcxPCNewEvent read GetOnNew write SetOnNew stored False; // deprecated
    property OnTabDragAndDrop: TcxTabControlTabDragAndDropEvent read FOnTabDragAndDrop write FOnTabDragAndDrop;
    property OnTabEndDrag: TcxTabControlTabEndDragEvent read FOnTabEndDrag write FOnTabEndDrag;
    property OnTabStartDrag: TcxTabControlTabStartDragEvent read FOnTabStartDrag write FOnTabStartDrag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanFocus: Boolean; override;
    procedure CloseTab(AIndex: Integer);
    function GetOptimalSize: Integer;
    procedure GetTabOrderList(List: TList); override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function VisibleIndexOfTabAt(X, Y: Integer): Integer;
    procedure ScrollTabs(ADelta: Integer);
    procedure SetStandardStyle(StandardStyle: TcxPCStandardStyle);
    procedure SetStyleByStyleName(StyleName: TCaption);

    property Controller: TcxCustomTabControlController read FController;
    property LookAndFeel;
    property Painter: TcxPCCustomPainter read FPainter;
    property ParentBackground;
    property Properties: TcxCustomTabControlProperties read FProperties write SetProperties;
    property TabStop default True;
    property ViewInfo: TcxCustomTabControlViewInfo read FViewInfo;
  end;

  { TcxPageControlProperties }

  TcxPageControlProperties = class(TcxCustomTabControlProperties)
  private
    FActivePage: TcxTabSheet;
    FActivePageSetting: Boolean;
    FPageInserting: Boolean;
    FPages: TList;
    FTabSheetClass: TcxTabSheetClass;
    FOnPageChanging: TcxPageChangingEvent;

    procedure ChangeActivePage(APage: TcxTabSheet);
    procedure UpdateTabOrders;

    function GetControl: TcxPageControl;
    function GetPageCount: Integer;
    procedure SetActivePage(APage: TcxTabSheet);
  protected
    function CanCloseButtonShow(ATab: TcxTab): Boolean; override;
    function CreateNewTab: Integer; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoChanging(ANewTabIndex: Integer; var AAllowChange: Boolean); override;
    procedure TabIndexChanged; override;
    procedure ImageListChange(Sender: TObject); override;
    procedure DoCloseTab(AIndex: Integer); override;
    function InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string; override;
    function TabIndexTabMustBeVisible: Boolean; override;
    procedure MoveTab(ACurrentIntex, ANewIndex: Integer); override;

    function CanChangeActivePage(ANewPage: TcxTabSheet): Boolean; dynamic;
    function GetActivePage: TcxTabSheet; virtual;
    function GetPage(ATabIndex: Integer): TcxTabSheet; virtual;
    procedure DoPageChange; dynamic;
    procedure DoPageChanging(NewPage: TcxTabSheet; var AllowChange: Boolean); dynamic;

    procedure InsertPage(APage: TcxTabSheet);
    procedure RemovePage(APage: TcxTabSheet);

    procedure UpdateActivePage; virtual;
    procedure UpdateTab(APage: TcxTabSheet);
    procedure UpdateTabs;

    property Control: TcxPageControl read GetControl;
    property OnPageChanging: TcxPageChangingEvent read FOnPageChanging write FOnPageChanging;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    function FindNextPage(ACurrentPage: TcxTabSheet; AGoForward, ACheckTabAccessibility, ACircular: Boolean): TcxTabSheet;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TcxTabSheet read GetPage;
    property TabIndex;
    property TabSheetClass: TcxTabSheetClass read FTabSheetClass write FTabSheetClass;
  published
    property ActivePage: TcxTabSheet read GetActivePage write SetActivePage;

    property ActivateFocusedTab;
    property AllowDisabledTabAccess;
    property AllowTabDragDrop;
    property CloseButtonMode;
    property CloseTabWithMiddleClick;
    property CustomButtons;
    property DragImage;
    property HideTabs;
    property HotImages;
    property HotTrack;
    property ImageBorder;
    property Images;
    property MultiLine;
    property MultiLineTabCaptions;
    property NavigatorPosition;
    property NewButtonMode;
    property Options; //!!! #DG
    property OwnerDraw;
    property RaggedRight;
    property Rotate;
    property RotatedTabsMaxWidth;
    property ScrollOpposite;
    property ShowButtonHints;
    property ShowFrame;
    property ShowTabHints;
    property Style;
    property TabCaptionAlignment;
    property TabHeight;
    property TabPosition;
    property TabsScroll;
    property TabSlants;
    property TabWidth;
  end;

  TcxPageControlViewInfo = class(TcxCustomTabControlViewInfo)
  private
    function GetActivePage: TcxTabSheet;
    function GetProperties: TcxPageControlProperties;
  protected
    function GetActivePageColor: TColor; override;
    function GetTabColor(ATabVisibleIndex: Integer): TColor; override;
    function HasActivePage: Boolean; override;
    function UseActivePageColor: Boolean; override;
  public
    property ActivePage: TcxTabSheet read GetActivePage;
    property Properties: TcxPageControlProperties read GetProperties;
  end;

  { TcxPageControl }

  TcxPageControl = class(TcxCustomTabControl)
  private
    FNewDockSheet: TcxTabSheet;
    FUndockingPage: TcxTabSheet;
    FOnPageChanging: TcxPageChangingEvent;

    // handlers
    procedure PropertiesPageChangingHandler(Sender: TObject; NewPage: TcxTabSheet; var AllowChange: Boolean);

    function GetActivePage: TcxTabSheet;
    procedure SetActivePage(APage: TcxTabSheet);

    function GetActivePageIndex: Integer;
    function GetDockClientFromPoint(P: TPoint): TControl;
    function GetPage(Index: Integer): TcxTabSheet;
    function GetPageCount: Integer;
    function GetProperties: TcxPageControlProperties;
    function GetTabCount: Integer;
    procedure SetActivePageIndex(Value: Integer);
    procedure SetProperties(Value: TcxPageControlProperties);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    // TCustomControl
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

    // TcxCustomTabControl
    procedure AfterLoaded; override;
    function GetPropertiesClass: TcxCustomTabControlPropertiesClass; override;
    function GetViewInfoClass: TcxCustomTabControlViewInfoClass; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;

    // Dock
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    function DockClient(DockSource: TDragDockObject; MousePos: TPoint): Integer; virtual;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    function GetPageFromDockClient(Client: TControl): TcxTabSheet;
    function UndockClient(NewTarget, Client: TControl): Boolean; virtual;

    procedure ControlChange(Inserting: Boolean; Child: TControl); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    function FindNextPage(ACurrentPage: TcxTabSheet;
      AGoForward, ACheckTabAccessibility: Boolean): TcxTabSheet;
    function FindNextPageEx(ACurrentPage: TcxTabSheet;
      AGoForward, ACheckTabAccessibility, ACircular: Boolean): TcxTabSheet;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TcxTabSheet read GetPage;
    property TabCount: Integer read GetTabCount;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Focusable;
    property Font;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property Properties: TcxPageControlProperties read GetProperties write SetProperties;
    property ActivePage: TcxTabSheet read GetActivePage write SetActivePage stored False;
    property HideTabs;
    property HotTrack;
    property ImageBorder;
    property Images;
    property LookAndFeel;
    property MultiLine;
    property NavigatorPosition;
    property Options;
    property OwnerDraw;
    property RaggedRight;
    property Rotate;
    property MaxRotatedTabWidth;
    property ScrollOpposite;
    property ShowFrame;
    property Style;
    property TabHeight;
    property TabPosition;
    property TabSlants stored False;
    property TabWidth;

    property OnCanClose;
    property OnCanCloseEx;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnDrawTabEx;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetTabHint;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnNew; // deprecated
    property OnNewTabButtonClick;
    property OnNewTabCreate;
    property OnPageChanging: TcxPageChangingEvent read FOnPageChanging write FOnPageChanging;
    property OnPaintDragImage;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTabDragAndDrop;
    property OnTabEndDrag;
    property OnTabStartDrag;
    property OnUnDock;
  end;

  TcxTabSheet = class(TCustomControl)
  strict private
  private
    FAllowCloseButton: Boolean;
    FHighlighted: Boolean;
    FImageIndex: TcxImageIndex;
    FPageControl: TcxPageControl;
    FTab: TcxTab;
    FTabHint: string;
    FTabVisible: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;

    function GetPageIndex: Integer;
    function GetPainter: TcxPCCustomPainter;
    function GetTabIndex: Integer;
    procedure InternalColorChanged;
    procedure SetAllowCloseButton(const Value: Boolean);
    procedure SetHighlighted(const Value: Boolean);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetPageControl(const Value: TcxPageControl);
    procedure SetPageIndex(const Value: Integer);
    procedure SetTabVisible(const Value: Boolean);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure EnabledChanged; dynamic;
    procedure PagePropertyChanged;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetParentPageControl(AParentPageControl: TcxPageControl); virtual;
    procedure ShowingChanged; dynamic;
    procedure TextChanged; dynamic;
    //
    property Painter: TcxPCCustomPainter read GetPainter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TcxPageControl read FPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;
  published
    property AllowCloseButton: Boolean read FAllowCloseButton write SetAllowCloseButton default True;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Left stored False;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabHint: string read FTabHint write FTabHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;
  end;

  { TcxTabControl }

  TcxTabControl = class(TcxCustomTabControl)
  private
    function GetProperties: TcxTabControlProperties;
    procedure SetProperties(Value: TcxTabControlProperties);
  protected
    function GetPropertiesClass: TcxCustomTabControlPropertiesClass; override;
  public
    property PageClientRect;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Focusable;
    property Font;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property Properties: TcxTabControlProperties read GetProperties write SetProperties;
    property HideTabs;
    property HotTrack;
    property ImageBorder;
    property Images;
    property LookAndFeel;
    property MultiLine;
    property MultiSelect;
    property NavigatorPosition;
    property Options;
    property OwnerDraw;
    property RaggedRight;
    property Rotate;
    property MaxRotatedTabWidth;
    property ScrollOpposite;
    property ShowFrame;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabPosition;
    property Tabs;
    property TabSlants;
    property TabWidth;

    property OnCanClose;
    property OnCanCloseEx;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnDrawTabEx;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnGetTabHint;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnNew; // deprecated
    property OnNewTabButtonClick;
    property OnNewTabCreate;
    property OnPaintDragImage;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTabDragAndDrop;
    property OnTabEndDrag;
    property OnTabStartDrag;
    property OnUnDock;
  end;

function DistanceGetter(const Distance: TcxPCDistance; const LongitudinalDistance: Boolean): Integer;
function InternalGetTextRotationAngle(AViewInfo: TcxCustomTabControlViewInfo): TcxRotationAngle;
function InternalIsVerticalText(AViewInfo: TcxCustomTabControlViewInfo): Boolean;
function IsBottomToTopAlignment(TabControl: TcxCustomTabControl): Boolean;
function IsOneOfButtons(AButton1, AButton2, AButton: TcxPCNavigatorButton): Boolean;
function IsRightToLeftAlignment(TabControl: TcxCustomTabControl): Boolean;
function IsVerticalText(TabControl: TcxCustomTabControl): Boolean;
function PointGetter(const APoint: TPoint; AIsY: Boolean): Longint;
procedure PointSetter(var APoint: TPoint; AIsY: Boolean; AValue: Longint);
procedure RectSetter(var ARect: TRect; AIsLeftTop, AIsY: Boolean; AValue: Longint);
function RotateRect(const ARect: TRect; ATabPosition: TcxTabPosition): TRect;
function RotateRectBack(const ARect: TRect; ATabPosition: TcxTabPosition): TRect;
procedure ValidateRect(var R: TRect);

function cxPCGetRightToLeftTabPosition(ATabPosition: TcxTabPosition): TcxTabPosition;

function GetPCStyleName(AStyleID: TcxPCStyleID): string;

const
  NavigatorBtnStateToLookAndFeelBtnState: array [TcxPCNavigatorButtonState] of TcxButtonState = (
    cxbsNormal, cxbsPressed, cxbsHot, cxbsDisabled);
  dxPCOptionsNames: array[TcxPCOption] of string = (
    'AlwaysShowGoDialogButton', 'CloseButton', 'FixedTabWidthWhenRotated',
    'GoDialog', 'Gradient', 'GradientClientArea', 'NoArrows', 'RedrawOnResize',
    'Sort', 'TopToBottomText', 'UsePageColorForTab'
  );

var
  TabScrollingDelay: Integer = 150;
  TabScrollingStartDelay: Integer = 300;

implementation

uses
  Math, StrUtils, Types,
  dxThemeConsts, dxThemeManager, dxUxTheme, cxPCConsts, cxPCPainters, cxPCPaintersFactory, cxPCGoDialog, dxDPIAwareUtils,
  dxSkinscxPCPainter, dxMessages;

type
  TcxDragImageAccess = class(TcxDragImage);

  TcxPCTabsChangedEventArgs = record
    Tab: TcxTab;
    PropertyChanged: TcxPCTabPropertyChanged;
  end;

var
  FBackgroundBitmap: TBitmap = nil;

function DistanceGetter(const Distance: TcxPCDistance; const LongitudinalDistance: Boolean): Integer;
begin
  if LongitudinalDistance then
    Result := Distance.dw
  else
    Result := Distance.dh;
end;

function GetControlRect(AControl: TControl): TRect;
begin
  Result := Rect(0, 0, AControl.Width, AControl.Height);
end;

function InternalGetTextRotationAngle(AViewInfo: TcxCustomTabControlViewInfo): TcxRotationAngle;
begin
  if AViewInfo.IsNativePainting then
    Result := ra0
  else
    Result := AViewInfo.GetTextRotationAngle;
end;

function InternalIsVerticalText(AViewInfo: TcxCustomTabControlViewInfo): Boolean;
begin
  Result := not AViewInfo.IsNativePainting and AViewInfo.IsVerticalText;
end;

function IsBottomToTopAlignment(TabControl: TcxCustomTabControl): Boolean;
begin
  with TabControl do
  begin
    Result := (TabPosition in [tpLeft, tpRight]) and (not ViewInfo.ActuallyRotate) and
      not (pcoTopToBottomText in Options);
  end;
end;

function IsOneOfButtons(AButton1, AButton2, AButton: TcxPCNavigatorButton): Boolean;
begin
  Result := (AButton = AButton1) or (AButton = AButton2);
end;

function IsRightToLeftAlignment(TabControl: TcxCustomTabControl): Boolean;
begin
  with TabControl do
  begin
    Result := (TabPosition in [tpTop, tpBottom]) and ViewInfo.ActuallyRotate and
      (pcoTopToBottomText in Options);
  end;
end;

function IsVerticalText(TabControl: TcxCustomTabControl): Boolean;
begin
  with TabControl do
  begin
    Result := (TabPosition in [tpLeft, tpRight]) and (not ViewInfo.ActuallyRotate);
    Result := Result or (TabPosition in [tpTop, tpBottom]) and ViewInfo.ActuallyRotate;
  end;
end;

function PointGetter(const APoint: TPoint; AIsY: Boolean): Longint;
begin
  if AIsY then
    Result := APoint.Y
  else
    Result := APoint.X;
end;

procedure PointSetter(var APoint: TPoint; AIsY: Boolean; AValue: Longint);
begin
  if AIsY then
    APoint.Y := AValue
  else
    APoint.X := AValue;
end;

procedure PrepareBitmap(ABitmap: TcxBitmap; AParametersSource: TcxCanvas;
  ASize: TSize; ABackgroundColor: TColor);
begin
  ABitmap.SetSize(cxRect(ASize));
  with ABitmap.cxCanvas do
  begin
    Font.Assign(AParametersSource.Font);
    Pen := AParametersSource.Pen;

    Brush := AParametersSource.Brush;
    Brush.Color := ABackgroundColor;
    Brush.Style := bsSolid;
    FillRect(ABitmap.ClientRect);
    Brush := AParametersSource.Brush;
  end;
end;

procedure RectSetter(var ARect: TRect; AIsLeftTop, AIsY: Boolean;
  AValue: Longint);
begin
  if AIsLeftTop then
  begin
    if AIsY then
      ARect.Top := AValue
    else
      ARect.Left := AValue;
  end
  else
  begin
    if AIsY then
      ARect.Bottom := AValue
    else
      ARect.Right := AValue;
  end;
end;

function RotateRect(const ARect: TRect; ATabPosition: TcxTabPosition): TRect;
begin
  case ATabPosition of
    tpLeft: Result := Rect(ARect.Top, ARect.Right, ARect.Bottom, ARect.Left);
    tpTop: Result := ARect;
    tpRight: Result := Rect(ARect.Bottom, ARect.Left, ARect.Top, ARect.Right);
    tpBottom: Result := Rect(ARect.Right, ARect.Bottom, ARect.Left, ARect.Top);
  end;
end;

function RotateRectBack(const ARect: TRect; ATabPosition: TcxTabPosition): TRect;
begin
  case ATabPosition of
    tpLeft: Result := RotateRect(ARect, tpRight);
    tpTop: Result := ARect;
    tpRight: Result := RotateRect(ARect, tpLeft);
    tpBottom: Result := RotateRect(ARect, tpBottom);
  end;
end;

procedure ValidateRect(var R: TRect);
begin
  R.Right := Max(R.Right, R.Left);
  R.Bottom := Max(R.Bottom, R.Top);
end;

function GetPCStyleName(AStyleID: TcxPCStyleID): string;
var
  APainterClass: TcxPCPainterClass;
begin
  if AStyleID = cxPCDefaultStyle then
    Result := cxPCDefaultStyleName
  else
  begin
    APainterClass := PaintersFactory.GetPainterClass(AStyleID);
    if APainterClass = nil then
      Result := ''
    else
      Result := APainterClass.GetStyleName;
  end;
end;

function cxPCGetRightToLeftTabPosition(ATabPosition: TcxTabPosition): TcxTabPosition;
begin
  Result := ATabPosition;
  case Result of
    tpLeft:
      Result := tpRight;
    tpRight:
      Result := tpLeft;
  end;
end;


{ TcxPCCustomPainter }

constructor TcxPCCustomPainter.Create(
  AViewInfo: TcxCustomTabControlViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

function TcxPCCustomPainter.GetPageBorders: TcxBorders;
begin
  if ViewInfo.HasBorders then
    Result := cxBordersAll
  else
  begin
    Result := [];
    if not ViewInfo.IsTabsVisible then
      Exit;
    case ViewInfo.TabPosition of
      tpTop:
        begin
          Include(Result, bTop);
          if ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount then
            Include(Result, bBottom);
        end;
      tpLeft:
        begin
          Include(Result, bLeft);
          if ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount then
            Include(Result, bRight);
        end;
      tpBottom:
        begin
          Include(Result, bBottom);
          if ViewInfo.TopOrLeftPartRowCount <> 0 then
            Include(Result, bTop);
        end;
      tpRight:
        begin
          Include(Result, bRight);
          if ViewInfo.TopOrLeftPartRowCount <> 0 then
            Include(Result, bLeft);
        end;
    end;
  end;
end;

function TcxPCCustomPainter.GetPageClientRect: TRect;
begin
  Result := cxNullRect;
  if IsEnouphSpaceForClientPage then
  begin
    Result := GetExtendedRect(ViewInfo.ControlBounds, GetPageClientRectOffset, ViewInfo.TabPosition);
    ValidateRect(Result);
  end;
end;

class function TcxPCCustomPainter.GetStandardStyle: TcxPCStandardStyle;
begin
  Result := tsTabs;
end;

class function TcxPCCustomPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCNoStyle;
end;

class function TcxPCCustomPainter.GetStyleName: TCaption;
begin
  Result := '';
end;

class function TcxPCCustomPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

class function TcxPCCustomPainter.IsDefault(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := False;
end;

class function TcxPCCustomPainter.IsMainTabBoundWithClient: Boolean;
begin
  Result := True;
end;

class function TcxPCCustomPainter.IsMultiSelectionAccepted: Boolean;
begin
  Result := False;
end;

class function TcxPCCustomPainter.IsStandardStyle: Boolean;
begin
  Result := False;
end;

class function TcxPCCustomPainter.IsTabPressable: Boolean;
begin
  Result := False;
end;

class function TcxPCCustomPainter.AllowRotate: Boolean;
begin
  Result := True;
end;

class function TcxPCCustomPainter.AllowMultiLineTabCaptions: Boolean;
begin
  Result := True;
end;

function TcxPCCustomPainter.CanDrawParentBackground: Boolean;
begin
  Result := ViewInfo.CanDrawParentBackground;
end;

procedure TcxPCCustomPainter.CorrectTabContentVerticalOffset(ATabVisibleIndex: Integer; var ADrawOffset: TRect);

  procedure CorrectNonpressableTabContentVerticalOffset;
  begin
    if ViewInfo.ActuallyRotate then
    begin
      ADrawOffset.Top := 0;
      ADrawOffset.Bottom := 0;
    end;
  end;

  procedure CorrectPressableTabContentVerticalOffset;
  begin
    if (InternalGetTextRotationAngle(ViewInfo) = raMinus90) xor
      IsTabBorderThick(ATabVisibleIndex) then
    begin
      Inc(ADrawOffset.Top);
      Dec(ADrawOffset.Bottom);
    end;
  end;

begin
  if IsTabPressable then
    CorrectPressableTabContentVerticalOffset
  else
    CorrectNonpressableTabContentVerticalOffset;
  if IsMainTabBoundWithClient and (TabViewInfo[ATabVisibleIndex].GetRelativeTextRotationAngle = ra180) then
    ExchangeLongWords(ADrawOffset.Top, ADrawOffset.Bottom);
end;

procedure TcxPCCustomPainter.CorrectTabNormalWidth(var AValue: Integer);
begin
// do nothing
end;

function TcxPCCustomPainter.GetButtonDrawOffsets: TRect;
begin
  if IsNativePainting then
    Result := cxSimpleRect
  else
    Result := cxEmptyRect;
end;

function TcxPCCustomPainter.GetButtonHeight: Integer;
begin
  Result := DoGetButtonHeight;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxPCCustomPainter.GetDropArrowRects(ADragTabVisibleIndex, AHitTabVisibleIndex,
  ADestinationTabVisibleIndex: Integer): TRects;
const
  DirectionMap: array [Boolean] of Integer = (-1, 1);
var
  AIsAfterInsert: Boolean;
  ANextTabVisibleIndex: Integer;
  P: TPoint;
  Y1, Y2: Integer;
  R: TRect;
  ANextRect: TRect;
  AIsNextRectValid: Boolean;
  AIndex: Integer;
begin
  AIndex := AHitTabVisibleIndex;
  ANextTabVisibleIndex := AIndex + DirectionMap[(AIndex < ADestinationTabVisibleIndex) xor
    ((AIndex = ADestinationTabVisibleIndex) and (ADragTabVisibleIndex < ADestinationTabVisibleIndex))];
  AIsAfterInsert := AIndex < ANextTabVisibleIndex;
  R := ViewInfo.TabsViewInfo[AIndex].VisibleRect;
  if ViewInfo.IsTabVisibleIndexValid(ANextTabVisibleIndex) and
      (ViewInfo.TabsViewInfo[AIndex].VisibleRow = ViewInfo.TabsViewInfo[ANextTabVisibleIndex].VisibleRow) then
    ANextRect := ViewInfo.TabsViewInfo[ANextTabVisibleIndex].VisibleRect
  else
    ANextRect := cxInvalidRect;
  AIsNextRectValid := not cxRectIsEmpty(ANextRect);
  if ViewInfo.TabPosition in [tpLeft, tpRight] then
  begin
    R := cxRectRotate(R);
    if AIsNextRectValid then
      ANextRect := cxRectRotate(ANextRect);
  end;
  Y1 := R.Top;
  Y2 := R.Bottom;
  if ViewInfo.IsRightToLeftAlignment or ViewInfo.IsBottomToTopAlignment then
  begin
    R := cxRectAdjust(cxRectInvert(R));
    if AIsNextRectValid then
      ANextRect := cxRectAdjust(cxRectInvert(ANextRect));
  end;
  if AIsAfterInsert then
    if ViewInfo.UseRightToLeftAlignment and (ViewInfo.TabPosition in [tpTop, tpBottom]) then
      P.X := IfThen(AIsNextRectValid, Min(R.Left, ANextRect.Right), R.Left)
    else
      P.X := IfThen(AIsNextRectValid, Max(R.Right, ANextRect.Left), R.Right)
  else
    if ViewInfo.UseRightToLeftAlignment and (ViewInfo.TabPosition in [tpTop, tpBottom]) then
      P.X := IfThen(AIsNextRectValid, Min(R.Right, ANextRect.Left), R.Right)
    else
      P.X := IfThen(AIsNextRectValid, Max(R.Left, ANextRect.Right), R.Left);
  if ViewInfo.IsRightToLeftAlignment or ViewInfo.IsBottomToTopAlignment then
    P.X := -P.X;
  P.Y := Y1;
  Result[0] := cxRect(P, P);
  InflateRect(Result[0], 1, 1);
  if ViewInfo.TabPosition in [tpLeft, tpRight] then
    Result[0] := cxRectRotate(Result[0]);
  P.Y := Y2;
  Result[1] := cxRect(P, P);
  InflateRect(Result[1], 1, 1);
  if ViewInfo.TabPosition in [tpLeft, tpRight] then
    Result[1] := cxRectRotate(Result[1]);
end;

function TcxPCCustomPainter.GetTabCorrection(ATabVisibleIndex: Integer): TRect;
begin
  Result := cxNullRect;
end;

procedure TcxPCCustomPainter.DirectionalPolyline(ACanvas: TcxCanvas; const R: TRect;
  APoints: array of TPoint; ATabPosition: TcxTabPosition; AColor: TColor);
begin
  RotatePolyline(R, APoints, ATabPosition);
  InternalPolyLine(APoints, AColor, ACanvas);
end;

function TcxPCCustomPainter.DoCustomDraw(TabVisibleIndex: Integer): Boolean;
begin
  Result := ViewInfo.Properties.DoCustomDraw(TabVisibleIndex);
end;

procedure TcxPCCustomPainter.DoDrawBackground(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := ViewInfo.ControlBounds;
  if IsNativePainting and ViewInfo.ParentBackground then
  begin
    if CanDrawParentBackground then
      cxDrawTransparentControlBackground(ViewInfo.IControl.GetControl, ACanvas, R, False)
  end
  else
    FillFreeSpaceArea(ACanvas, R);
end;

function TcxPCCustomPainter.DoGetCloseButtonSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(16, 14));
end;

procedure TcxPCCustomPainter.DrawBackground(ACanvas: TcxCanvas);
begin
  DoDrawBackground(ACanvas);
end;

procedure TcxPCCustomPainter.DrawHeaderButtonGlyph(ACanvas: TcxCanvas; AHeaderButtonInfo: TcxPCHeaderButtonViewInfo);
var
  AGlyphRect: TRect;
begin
  if IsImageAssigned(ViewInfo.Properties.HeaderButtonImages, AHeaderButtonInfo.ImageIndex) then
  begin
    AGlyphRect := cxRectCenter(AHeaderButtonInfo.Bounds, HeaderButtonImagePainter.RotatedImageSize);
    HeaderButtonImagePainter.Draw(ACanvas, AGlyphRect, AHeaderButtonInfo.ImageIndex,
      clNone, IsNativePainting, (AHeaderButtonInfo.State <> nbsDisabled), False,
      GetHeaderButtonGlyphPalette(AHeaderButtonInfo.State));
  end;
end;

procedure TcxPCCustomPainter.DrawFocusRect(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
begin
end;

procedure TcxPCCustomPainter.DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet);
begin
end;

procedure TcxPCCustomPainter.DrawTabButtons(ACanvas: TcxCanvas; TabVisibleIndex: Integer);

  function NeedDrawImage(AButtonInfo: TcxPCCustomTabButtonViewInfo): Boolean;
  begin
    Result := IsImageAssigned(ViewInfo.Properties.TabButtonImages, AButtonInfo.ImageIndex);
  end;

  procedure InternalDrawTabButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonInfo: TcxPCCustomTabButtonViewInfo);
  begin
    if AButtonInfo.GetHitTestIndex = pchtTabCloseButton then
    begin
      if UseLookAndFeelTabButton then
        GetLookAndFeelPainter.DrawScaledSmallCloseButton(ACanvas, ARect,
          NavigatorBtnStateToLookAndFeelBtnState[AButtonInfo.State], ScaleFactor)
      else
        DoDrawTabCloseButton(ACanvas, ARect, AButtonInfo.State)
    end
    else
    begin
      if UseLookAndFeelTabButton then
        GetLookAndFeelPainter.DrawScaledSmallButton(
          ACanvas, cxRectInflate(ARect, GetButtonDrawOffsets),
          NavigatorBtnStateToLookAndFeelBtnState[AButtonInfo.State], ScaleFactor)
      else
        DoDrawTabButton(ACanvas, ARect, AButtonInfo.State);

      if NeedDrawImage(AButtonInfo) then
      begin
        TabButtonImagePainter.Draw(ACanvas, cxRectCenter(ARect,
          dxGetImageSize(nil, ViewInfo.Properties.TabButtonImages, AButtonInfo.ImageIndex, ScaleFactor)),
          AButtonInfo.ImageIndex, clNone, IsNativePainting, (AButtonInfo.State <> nbsDisabled), False,
          GetTabButtonColorPalette(AButtonInfo.State));
      end;
    end;
  end;

const
  ABackRotateAngle: array [TcxRotationAngle] of TcxRotationAngle = (ra0, raMinus90, raPlus90, ra0);
var
  AButtonInfo: TcxPCCustomTabButtonViewInfo;
  ABitmap: TcxBitmap;
  AButtonRect: TRect;
  AAngle: TcxRotationAngle;
  I: Integer;
begin
  AAngle := InternalGetTextRotationAngle(ViewInfo);
  for I := 0 to TabViewInfo[TabVisibleIndex].ButtonsCount - 1 do
  begin
    AButtonInfo := TabViewInfo[TabVisibleIndex].FButtonInfos[I];
    AButtonRect := AButtonInfo.Bounds;
    if AAngle = ra0 then
      InternalDrawTabButton(ACanvas, AButtonRect, AButtonInfo)
    else
    begin
      ABitmap := TcxBitmap.CreateSize(AButtonRect);
      try
        ABitmap.Canvas.Lock;
        try
          ABitmap.Rotate(ABackRotateAngle[AAngle]);
          InternalDrawTabButton(ABitmap.cxCanvas, ABitmap.ClientRect, AButtonInfo);
          ABitmap.Rotate(AAngle);
          cxDrawBitmap(ACanvas.Handle, ABitmap, AButtonRect, cxNullPoint);
        finally
          ABitmap.Canvas.UnLock;
        end;
      finally
        ABitmap.Free;
      end;
    end;
  end;
end;

procedure TcxPCCustomPainter.DrawTabImage(ACanvas: TcxCanvas;
  const ARect: TRect; AImageIndex: Integer; AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer);
begin
  ImagePainter.Draw(ACanvas, ARect, AImageIndex, AColor, IsNativePainting,
    AEnabled, TabViewInfo[ATabVisibleIndex].IsHotTrack, GetTabImageColorPalette);
end;

procedure TcxPCCustomPainter.DrawTabImageAndText(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);

  function NeedDrawImage: Boolean;
  var
    ATabViewInfo: TcxTabViewInfo;
    AHasHotImage, AHasImage, AHotImagesAssigned: Boolean;
  begin
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    AHasImage := IsImageAssigned(ViewInfo.Properties.Images, ATabViewInfo.ImageIndex);
    AHasHotImage := IsImageAssigned(ViewInfo.Properties.HotImages, ATabViewInfo.ImageIndex);
    AHotImagesAssigned := ViewInfo.Properties.HotImages <> nil;

    Result := not ATabViewInfo.IsHotTrack and AHasImage or
      ATabViewInfo.IsHotTrack and (AHotImagesAssigned and AHasHotImage or not AHotImagesAssigned and AHasImage);
  end;

var
  ATabViewInfo: TcxTabViewInfo;
  AIsTabEnabled: Boolean;
  ABackgroundColor: TColor;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  ViewInfo.PrepareTabCanvasFont(ATabViewInfo, ACanvas);
  ABackgroundColor := GetTabBodyColor(ATabVisibleIndex);
  AIsTabEnabled := ATabViewInfo.ActuallyEnabled;
  if NeedDrawImage then
    DrawTabImage(ACanvas, ATabViewInfo.ImageRect, ATabViewInfo.ImageIndex,
      AIsTabEnabled, ABackgroundColor, ATabVisibleIndex);
  if ATabViewInfo.HasCaption then
    DrawTabText(ACanvas, ATabViewInfo.TextRect, ATabViewInfo.Caption,
      AIsTabEnabled, ABackgroundColor, ATabVisibleIndex);
  if ATabViewInfo.HasButtons then
    DrawTabButtons(ACanvas, ATabVisibleIndex);
end;

procedure TcxPCCustomPainter.DrawTabText(ACanvas: TcxCanvas; const ARect: TRect; const AText: string;
  AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer);
const
  ATextOffset: array [TcxRotationAngle] of TPoint = (
    (X: 1; Y: 1),
    (X: 1; Y: -1),
    (X: -1; Y: 1),
    (X: 1; Y: 1));
begin
  if not AEnabled then
  begin
    ACanvas.Font.Color := DisabledTextFaceColor;
    if NeedDisabledTextShadow then
    begin
      InternalDrawText(ACanvas, AText, cxRectOffset(ARect,
        ATextOffset[InternalGetTextRotationAngle(ViewInfo)]), ATabVisibleIndex);
      ACanvas.Font.Color := DisabledTextShadowColor;
    end;
  end;
  InternalDrawText(ACanvas, AText, ARect, ATabVisibleIndex);
end;

procedure TcxPCCustomPainter.ExcludeTabContentClipRegion(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AContentRgn: TcxRegion;
begin
  if IsNativePainting then
    Exit;

  AContentRgn := TcxRegion.Create(TabViewInfo[ATabVisibleIndex].FullRect);
  AContentRgn.Combine(GetTabClipRgn(ACanvas, ATabVisibleIndex), roIntersect);
  ACanvas.SetClipRegion(AContentRgn, roSubtract);
end;

procedure TcxPCCustomPainter.FillFreeSpaceArea(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if ViewInfo.IsTransparent then
    Exit;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(ARect, GetFreeSpaceColor);
end;

procedure TcxPCCustomPainter.FillPageClientRect(ACanvas: TcxCanvas);
begin
  if ViewInfo.IsTransparent then
    Exit;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(GetFillClientRect, GetClientColor);
end;

procedure TcxPCCustomPainter.FillTabPaneContent(ACanvas: TcxCanvas);
begin
  FillPageClientRect(ACanvas);
end;

function TcxPCCustomPainter.GetButtonsDistance(
  AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  Result := 0;
end;

function TcxPCCustomPainter.GetButtonsRegionFromTabsOffset: Integer;
begin
  Result := 0;
end;

function TcxPCCustomPainter.GetButtonsRegionHOffset: Integer;
begin
  Result := 0;
end;

function TcxPCCustomPainter.GetButtonsRegionWOffset: Integer;
begin
  Result := 0;
end;

function TcxPCCustomPainter.GetButtonWidth(Button: TcxPCNavigatorButton): Integer;
begin
  Result := DoGetButtonWidth(Button);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxPCCustomPainter.GetClientColor: TColor;
begin
  if UseActivePageColor then
    Result := ViewInfo.GetActivePageColor
  else
    Result := GetDefaultClientColor;
end;

function TcxPCCustomPainter.GetCloseButtonAreaHeight(ATabVisibleIndex: Integer): Integer;
begin
  Result := ViewInfo.TabButtonHeight + GetCloseButtonOffset(ATabVisibleIndex).Top +
    GetCloseButtonOffset(ATabVisibleIndex).Bottom;
end;

function TcxPCCustomPainter.GetCloseButtonAreaWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := GetCloseButtonSize.cx + GetCloseButtonOffset(ATabVisibleIndex).Left +
    GetCloseButtonOffset(ATabVisibleIndex).Right;
end;

function TcxPCCustomPainter.GetCloseButtonSize: TSize;
begin
  Result := DoGetCloseButtonSize;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxPCCustomPainter.GetCloseButtonOffset(ATabVisibleIndex: Integer): TRect;
begin
  Result := ScaleFactor.Apply(cxRect(4, 2, 0, 2));
  if ATabVisibleIndex = -1 then Exit;
  Result.Top := GetDrawImageOffset(ATabVisibleIndex).Top;
  Result.Bottom := GetDrawImageOffset(ATabVisibleIndex).Bottom;
  Dec(Result.Bottom);
end;

function TcxPCCustomPainter.GetFillClientRect: TRect;
begin
  Result := GetPageClientRect;
end;

function TcxPCCustomPainter.GetFreeSpaceColor: TColor;
begin
  Result := ViewInfo.Color;
end;

function TcxPCCustomPainter.GetHeaderButtonGlyphOffset: TRect;
begin
  Result := ScaleFactor.Apply(Rect(2, 2, 2, 2));
end;

function TcxPCCustomPainter.GetHeaderButtonGlyphPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxPCCustomPainter.GetPageClientRectOffset: TRect;
var
  ABorders: TcxBorders;
  ABorderOffsets: TRect;
begin
  Result := GetPageFrameRectOffset;
  if NeedShowFrame then
  begin
    ABorders := GetPageBorders;
    ABorderOffsets := cxEmptyRect;
    if bLeft in ABorders then
      Inc(ABorderOffsets.Left, GetBorderWidths.Left);
    if bTop in ABorders then
      Inc(ABorderOffsets.Top, GetBorderWidths.Top);
    if bRight in ABorders then
      Inc(ABorderOffsets.Right, GetBorderWidths.Right);
    if bBottom in ABorders then
      Inc(ABorderOffsets.Bottom, GetBorderWidths.Bottom);
    ABorderOffsets := RotateRectBack(ABorderOffsets, ViewInfo.TabPosition);
    Result := cxRectTransform(Result, ABorderOffsets);
  end;
end;

function TcxPCCustomPainter.GetDefaultClientColor: TColor;
begin
  Result := ViewInfo.Color;
end;

function TcxPCCustomPainter.GetPageFrameRect: TRect;
begin
  Result := cxNullRect;
  if IsEnouphSpaceForClientPage then
  begin
    Result := GetExtendedRect(ViewInfo.ControlBounds, GetPageFrameRectOffset, ViewInfo.TabPosition);
    ValidateRect(Result);
  end;
end;

function TcxPCCustomPainter.GetPageFrameRectOffset: TRect;
begin
  Result := cxEmptyRect;
  if ViewInfo.IsTabsVisible then
    Result := InternalGetPageFrameRectOffset;
end;

function TcxPCCustomPainter.GetDTFlags(ATabVisibleIndex: Integer): Integer;
begin
  if TabViewInfo[ATabVisibleIndex].FIsTextTooLong then
    Result := DT_END_ELLIPSIS
  else
    Result := 0;
  if TabViewInfo[ATabVisibleIndex].FIsMultiline then
    if TabViewInfo[ATabVisibleIndex].GetLimitedWidth > 0 then
      Result := Result or DT_WORDBREAK
    else
      Result := Result
  else
    Result := Result or DT_SINGLELINE;
  if not TabViewInfo[ATabVisibleIndex].ShowAccelChar then
    Result := Result or DT_NOPREFIX;
  if ViewInfo.UseRightToLeftReading then
    Result := Result or DT_RTLREADING;
end;

function TcxPCCustomPainter.GetCTFlags(ATabVisibleIndex: Integer): Integer;
begin
  if not TabViewInfo[ATabVisibleIndex].FIsMultiline then
    Result := DT_CALCRECT or DT_SINGLELINE
  else
    Result := DT_CALCRECT or DT_WORDBREAK;
  if not TabViewInfo[ATabVisibleIndex].ShowAccelChar then
    Result := Result or DT_NOPREFIX;
  if ViewInfo.UseRightToLeftReading then
    Result := Result or DT_RTLREADING;
end;

function TcxPCCustomPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ViewInfo.GetLookAndFeel.Painter;
end;

function TcxPCCustomPainter.GetBorderWidths: TRect;
var
  AWidth: Integer;
begin
  AWidth := GetFrameWidth;
  Result := cxRect(AWidth, AWidth, AWidth, AWidth);
end;

function TcxPCCustomPainter.GetExtendedRect(const ARect, AExtension: TRect; ATabPosition: TcxTabPosition): TRect;
begin
  Result := cxRectContent(ARect, RotateRect(AExtension, ATabPosition));
end;

function TcxPCCustomPainter.GetFrameWidth: Integer;
begin
  Result := 0;
end;

function TcxPCCustomPainter.GetDisabledTextFaceColor: TColor;
begin
  Result := clBtnHighlight;
end;

function TcxPCCustomPainter.GetDisabledTextShadowColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxPCCustomPainter.GetHeaderButtonImagePainter: TcxPCImageList;
begin
  Result := ViewInfo.Properties.CustomButtons.FHeaderImages;
end;

function TcxPCCustomPainter.GetHighlightedTabBodyColor: TColor;
begin
  Result := clHighlight;
end;

function TcxPCCustomPainter.GetImagePainter: TcxPCImageList;
begin
  Result := ViewInfo.Properties.FImages;
end;

function TcxPCCustomPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := ViewInfo.ScaleFactor;
end;

function TcxPCCustomPainter.GetTabButtonImagePainter: TcxPCImageList;
begin
  Result := ViewInfo.Properties.CustomButtons.FTabImages;
end;

function TcxPCCustomPainter.GetTabViewInfo(Index: Integer): TcxTabViewInfo;
begin
  Result := ViewInfo.TabsViewInfo[Index];
end;

function TcxPCCustomPainter.GetTabButtonGlyphOffset: TRect;
begin
  Result := ScaleFactor.Apply(Rect(2, 2, 2, 2));
end;

function TcxPCCustomPainter.GetTabButtonsAreaWidth(ATabVisibleIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TabViewInfo[ATabVisibleIndex].ButtonsCount - 1 do
    Inc(Result, TabViewInfo[ATabVisibleIndex].ButtonInfos[I].GetWidth);
  Inc(Result, GetTabButtonDistance *
    (TabViewInfo[ATabVisibleIndex].ButtonsCount - 1) +
    cxMarginsWidth(GetCloseButtonOffset(ATabVisibleIndex)));
end;

function TcxPCCustomPainter.GetTabButtonColorPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette;
begin
  if UseLookAndFeelTabButton then
    Result := GetLookAndFeelPainter.GetSmallButtonColorPalette(NavigatorBtnStateToLookAndFeelBtnState[AState])
  else
    Result := nil;
end;

function TcxPCCustomPainter.GetTabButtonDistance: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

procedure TcxPCCustomPainter.AfterPaintTab(ACanvas: TcxCanvas; ATabVisibleIndex: Integer);
var
  AImageAndTextData: TcxPCOutTabImageAndTextData;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  AImageAndTextData.TabImageRect := ATabViewInfo.GetAbsolutePartRect(ATabViewInfo.ImageRect);
  AImageAndTextData.TabTextRect := ATabViewInfo.GetAbsolutePartRect(ATabViewInfo.TextRect);
  AImageAndTextData.TabVisibleIndex := ATabVisibleIndex;
  ViewInfo.AfterPaintTab(ACanvas, TabViewInfo[ATabVisibleIndex].Tab, AImageAndTextData);
end;

function TcxPCCustomPainter.AlwaysColoredTabs: Boolean;
begin
  Result := False;
end;

function TcxPCCustomPainter.GetHighlightedTextColor(ATabVisibleIndex: Integer;
  ATextColor: TColor): TColor;
var
  AColor: TColorRef;
  ATheme: TdxTheme;
begin
  if IsNativePainting then
  begin
    ATheme := OpenTheme(totTab);
    if GetThemeColor(ATheme, TABP_TABITEM, GetTabNativeState(ATabVisibleIndex), TMT_TEXTCOLOR, AColor) = S_OK then
      Result := AColor
    else
      Result := ATextColor;
  end
  else
    Result := clHighlightText;
end;

function TcxPCCustomPainter.GetHotTrackColor: TColor;
begin
  Result := GetSysColor(COLOR_HOTLIGHT)
end;

function TcxPCCustomPainter.GetMaxTabCaptionHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ViewInfo.TabsViewInfo.ViewInfoCount - 1 do
    Result := Max(Result, TabViewInfo[I].TextHeight);
end;

function TcxPCCustomPainter.GetNativeContentOffset: TRect;
var
  R: TRect;
begin
  R := Rect(0, 0, 100, 100);
  GetThemeBackgroundContentRect(OpenTheme(totTab), 0, TABP_PANE, 0, R, Result);
  Result.Right := R.Right - Result.Right;
  Result.Bottom := R.Bottom - Result.Bottom;

  if IsStandardTheme then
  begin
    if ViewInfo.IsTabsContainer then
      Result.Left := TabsContainerBaseWidth
    else
      Result.Left := Max(Result.Left, Result.Top);
    Result.Top := Result.Left;
    Result.Right := Result.Left * 2;//Max(Result.Right, Result.Bottom);
    Result.Bottom := Result.Top * 2;//Result.Right;
  end;
end;

function TcxPCCustomPainter.GetTabBaseImageSize: TSize;
begin
  Result := ViewInfo.GetTabImageSize;
end;

function TcxPCCustomPainter.GetTabColor(ATabVisibleIndex: Integer): TColor;
begin
  Result := ViewInfo.TabColors[ATabVisibleIndex];
end;

function TcxPCCustomPainter.GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion;
begin
  Result := TcxRegion.Create(TabViewInfo[ATabVisibleIndex].VisibleRect);
end;

function TcxPCCustomPainter.GetTabClipRgnOperation(ATabVisibleIndex: Integer): TcxRegionOperation;
begin
  Result := roIntersect;
end;

function TcxPCCustomPainter.GetTabImageAreaHeight: Integer;
begin
  Result := ViewInfo.GetTabImageAreaHeight;
end;

function TcxPCCustomPainter.GetTabImageAreaWidth: Integer;
begin
  Result := ViewInfo.GetTabImageAreaWidth;
end;

function TcxPCCustomPainter.GetTabImageColorPalette: IdxColorPalette;
begin
  Result := nil;
end;

procedure TcxPCCustomPainter.GetTabNativePartAndState(ATabVisibleIndex: Integer; out PartId, StateId: Integer);
begin
  PartId := 0;
  StateId := 0;
end;

function TcxPCCustomPainter.GetTabNativeState(ATabVisibleIndex: Integer): Integer;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ATabViewInfo.IsMainTab then
    Result := TIS_SELECTED
  else
    if ATabViewInfo.IsHotTrack then
      Result := TIS_HOT
    else
      Result := TIS_NORMAL;
end;

function TcxPCCustomPainter.GetTabRotatedImageSize: TSize;
begin
  Result := ImagePainter.RotatedImageSize;
end;

function TcxPCCustomPainter.GetTabsContainerOffset: Integer;
begin
  Result := ScaleFactor.Apply(cxPC.TabsContainerOffset);
end;

function TcxPCCustomPainter.GetTabsContainerOffsets: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxPCCustomPainter.GetTabsNormalDistance: TcxPCDistance;
begin
  Result.dw := 0;
  Result.dh := 0;
end;

function TcxPCCustomPainter.GetTextColor(ATabVisibleIndex: Integer): TColor;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  if ATabViewInfo.IsHighlighted then
    Result := GetHighlightedTextColor(ATabVisibleIndex, ViewInfo.Font.Color)
  else
    if ATabViewInfo.IsHotTrack then
      Result := GetHotTrackColor
  else
    Result := clBtnText;
end;

function TcxPCCustomPainter.HasActivePage: Boolean;
begin
  Result := ViewInfo.HasActivePage;
end;

procedure TcxPCCustomPainter.Init;
begin
end;

procedure TcxPCCustomPainter.InternalPaint(ACanvas: TcxCanvas);

  procedure DoPaintHeadersArea;
  begin
    if ViewInfo.IsTabsVisible then
      PaintHeadersArea(ACanvas);
  end;

begin
  if IsPaintHeadersAreaFirst then
    DoPaintHeadersArea;
  PaintPageClientArea(ACanvas);
  PaintPageFrame(ACanvas);
  if not IsPaintHeadersAreaFirst then
    DoPaintHeadersArea;
end;

procedure TcxPCCustomPainter.InternalDrawText(ACanvas: TcxCanvas;
  const ACaption: string; ARect: TRect; ATabVisibleIndex: Integer);
var
  APartId, AStateId: Integer;
begin

  if IsNativePainting and TabViewInfo[ATabVisibleIndex].ActuallyEnabled and AreVisualStylesAvailable then
  begin
    if not ViewInfo.IsCustomTextColorAssigned then
    begin
      GetTabNativePartAndState(ATabVisibleIndex, APartId, AStateId);
      DrawThemeText(OpenTheme(totTab), ACanvas.Canvas.Handle, APartId,
        AStateId, ACaption, -1, GetDTFlags(ATabVisibleIndex), 0, ARect);
    end
    else
      cxDrawText(ACanvas.Handle, ACaption, ARect, GetDTFlags(ATabVisibleIndex));
  end
  else
    cxDrawText(ACanvas, ACaption, ARect, GetDTFlags(ATabVisibleIndex), clDefault, InternalGetTextRotationAngle(ViewInfo));
end;

function TcxPCCustomPainter.InternalGetPageFrameRectOffset: TRect;
begin
   case ViewInfo.TabPosition of
    tpTop:
      begin
        Result.Top := ViewInfo.FExtendedTopOrLeftTabsRect.Bottom;
        if ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount then
          Result.Bottom := ViewInfo.Height - ViewInfo.FExtendedBottomOrRightTabsRect.Top;
      end;
    tpLeft:
      begin
        Result.Top := ViewInfo.FExtendedTopOrLeftTabsRect.Right;
        if ViewInfo.TopOrLeftPartRowCount <> ViewInfo.RowCount then
          Result.Bottom := ViewInfo.Width - ViewInfo.FExtendedBottomOrRightTabsRect.Left;
      end;
    tpBottom:
      begin
        if ViewInfo.IsTooSmallControlSize then
          Result.Top := ViewInfo.FExtendedTopOrLeftTabsRect.Top
        else
          Result.Top := ViewInfo.Height - ViewInfo.FExtendedBottomOrRightTabsRect.Top;
        if ViewInfo.TopOrLeftPartRowCount <> 0 then
          Result.Bottom := ViewInfo.FExtendedTopOrLeftTabsRect.Bottom;
      end;
    tpRight:
      begin
        if ViewInfo.IsTooSmallControlSize then
          Result.Top := ViewInfo.FExtendedTopOrLeftTabsRect.Left
        else
          Result.Top := ViewInfo.Width - ViewInfo.FExtendedBottomOrRightTabsRect.Left;
        if ViewInfo.TopOrLeftPartRowCount <> 0 then
          Result.Bottom := ViewInfo.FExtendedTopOrLeftTabsRect.Right;
      end;
  end;
end;

procedure TcxPCCustomPainter.InternalPolyLine(const APoints: array of TPoint; AColor: TColor; ACanvas: TcxCanvas);
var
  ALastPoint: TPoint;
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.Polyline(APoints);
  ALastPoint := APoints[High(APoints)];
  ACanvas.Polyline([ALastPoint, Point(ALastPoint.X + 1, ALastPoint.Y + 1)]);
end;

procedure TcxPCCustomPainter.InternalResetClipRegion(ACanvas: TcxCanvas);
begin
  ACanvas.RestoreClipRegion;
end;

function TcxPCCustomPainter.InternalSetClipRect(ACanvas: TcxCanvas; ClipR: TRect;
  IntersectWithCurrentClipRegion: Boolean = True): Boolean;
begin
  Result := False;
  if IsRectEmpty(ClipR) then Exit;
  with ACanvas do
  begin
    if IntersectWithCurrentClipRegion and
      (not Windows.RectVisible(Handle, ClipR)) then Exit;
    SaveClipRegion;
    if IntersectWithCurrentClipRegion then
      IntersectClipRect(ClipR)
    else
      SetClipRegion(TcxRegion.Create(ClipR), roSet);
  end;
  Result := True;
end;

procedure TcxPCCustomPainter.InvalidateTabExtendedTabsRect(TabVisibleIndex: Integer);
var
  ATabExtendedRect: TRect;
begin
  with ViewInfo do
    ATabExtendedRect := GetTabExtendedTabsRect(TabVisibleIndex);
  ViewInfo.InvalidateRect(ATabExtendedRect, False);
end;

procedure TcxPCCustomPainter.InvalidateTabRect(ATabVisibleIndex: Integer);
begin
  ViewInfo.InvalidateRect(TabViewInfo[ATabVisibleIndex].VisibleRect, False);
end;

function TcxPCCustomPainter.IsAssignedImages: Boolean;
begin
  Result := ViewInfo.IsTabImagesAssigned;
end;

function TcxPCCustomPainter.IsEnableHotTrack: Boolean;
begin
  Result := IsNativePainting;
end;

function TcxPCCustomPainter.IsEnouphSpaceForClientPage: Boolean;

  procedure CheckRect(var ARect: TRect);
  begin
    if cxRectIsEmpty(ARect) then
      ARect := cxNullRect;
  end;

var
  ARect1, ARect2: TRect;
begin
   Result := False;
   ARect1 := ViewInfo.FExtendedTopOrLeftTabsRect;
   ARect2 := ViewInfo.FExtendedBottomOrRightTabsRect;
   CheckRect(ARect1);
   CheckRect(ARect2);

   case ViewInfo.TabPosition of
    tpTop, tpBottom:
      Result := cxRectHeight(ARect1) + cxRectHeight(ARect2) < ViewInfo.Height;
    tpLeft, tpRight:
      Result := cxRectWidth(ARect1) + cxRectWidth(ARect2) < ViewInfo.Width;
  end;
end;

function TcxPCCustomPainter.IsNativePainting: Boolean;
begin
  Result := False;
end;

function TcxPCCustomPainter.PtInTab(TabVisibleIndex: Integer; X,
  Y: Integer): Boolean;
begin
  Result := True;
end;

function TcxPCCustomPainter.IsPaintHeadersAreaFirst: Boolean;
begin
  Result := True;
end;

function TcxPCCustomPainter.IsTabHasImage(ATabVisibleIndex: Integer): Boolean;
begin
  Result := TabViewInfo[ATabVisibleIndex].HasImage;
end;

function TcxPCCustomPainter.IsTabTransparent(ATabVisibleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxPCCustomPainter.IsTabsRectVisible(ACanvas: TcxCanvas): Boolean;
var
  R1, R2, R3, R4: TRect;
  AControlRect, AClientRect: TRect;
begin
  Result := not ViewInfo.IsTooSmallControlSize;
  if Result then
  begin
    AControlRect := ViewInfo.ControlBounds;
    AClientRect := GetPageClientRect;
    R1 := AControlRect;
    R1.Right := AClientRect.Left;
    R2 := AControlRect;
    R2.Bottom := AClientRect.Top;
    R3 := AControlRect;
    R3.Left := AClientRect.Right;
    R4 := AControlRect;
    R4.Top := AClientRect.Bottom;
    Result := ACanvas.RectVisible(R1) or
      ACanvas.RectVisible(R2) or
      ACanvas.RectVisible(R3) or
      ACanvas.RectVisible(R4);
  end;
end;

function TcxPCCustomPainter.NeedDisabledTextShadow: Boolean;
begin
  Result := True;
end;

function TcxPCCustomPainter.NeedDoubleBuffer: Boolean;
begin
  Result := False;
end;

function TcxPCCustomPainter.NeedRedrawOnResize: Boolean;
begin
  Result := IsNativePainting;
end;

function TcxPCCustomPainter.NeedShowFrame: Boolean;
begin
  Result := ViewInfo.ShowFrame;
end;

procedure TcxPCCustomPainter.PaintDragImage(ACanvas: TcxCanvas; const R: TRect; ATabVisibleIndex: Integer);
begin
  StartDragImagePainted;
  try
    InternalPaintDragImage(ACanvas, ATabVisibleIndex);
  finally
    StopDragImagePainted;
  end;
end;

procedure TcxPCCustomPainter.Paint(ACanvas: TcxCanvas);
begin
  InternalPaint(ACanvas);
  DrawBackground(ACanvas);
end;

procedure TcxPCCustomPainter.PaintPageClientArea(ACanvas: TcxCanvas);
begin
  FillPageClientRect(ACanvas);
  ACanvas.ExcludeClipRect(GetPageClientRect);
end;

procedure TcxPCCustomPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
end;

procedure TcxPCCustomPainter.PaintHeadersArea(ACanvas: TcxCanvas);

  function IsButtonsRegionVisible: Boolean;
  begin
    Result := (ViewInfo.TabPosition in [tpTop, tpBottom]) or not ViewInfo.IsTooSmallControlSize;
  end;

begin
  if IsButtonsRegionVisible then
    PaintButtonsRegion(ACanvas);
  if IsTabsRectVisible(ACanvas) then
    PaintTabsRegion(ACanvas);
end;

procedure TcxPCCustomPainter.PaintPageFrame(ACanvas: TcxCanvas);
begin
  if NeedShowFrame then
    DoPaintPageFrame(ACanvas);
  ACanvas.ExcludeClipRect(GetPageFrameRect);
end;

procedure TcxPCCustomPainter.PaintTab(ACanvas: TcxCanvas; TabVisibleIndex: Integer);
begin
  // do nothing
end;

procedure TcxPCCustomPainter.PrepareDrawTabContentBitmapBackground(
  ABitmap: TcxBitmap; const ABitmapPos: TPoint; ATabVisibleIndex: Integer);
begin
end;

procedure TcxPCCustomPainter.DrawTabContentBackground(ACanvas: TcxCanvas;
  const ABounds: TRect; ABackgroundColor: TColor; ATabVisibleIndex: Integer);
begin
  ACanvas.FillRect(ABounds, ABackgroundColor);
end;

procedure TcxPCCustomPainter.RepaintButtonsRegion;
begin
end;

procedure TcxPCCustomPainter.RepaintTab(TabVisibleIndex: Integer;
  TabPropertyChanged: TcxPCTabPropertyChanged);
begin
end;

procedure TcxPCCustomPainter.RotatePoint(const R: TRect; var P: TPoint;
  ATabPosition: TcxTabPosition);
begin
  case ATabPosition of
    tpBottom:
      P.Y := R.Bottom - 1 - (P.Y - R.Top);
    tpLeft:
      P := Point(R.Right - 1 - (R.Bottom - 1 - P.Y), R.Bottom - 1 - (P.X - R.Left));
    tpRight:
      P := Point(R.Bottom - 1 - P.Y + R.Left, R.Bottom - 1 - (P.X - R.Left));
  end;
end;

procedure TcxPCCustomPainter.RotatePolyline(const R: TRect; var APoints: array of TPoint;
  ATabPosition: TcxTabPosition);
var
  I: Integer;
begin
  for I := Low(APoints) to High(APoints) do
    RotatePoint(R, APoints[I], ATabPosition);
end;

function TcxPCCustomPainter.UseActivePageColor: Boolean;
begin
  Result := ViewInfo.UseActivePageColor;
end;

function TcxPCCustomPainter.UseLookAndFeelTabButton: Boolean;
begin
  Result := False;
end;

function TcxPCCustomPainter.IsDragImagePainted: Boolean;
begin
  Result := FIsDragImagePainted;
end;

procedure TcxPCCustomPainter.StartDragImagePainted;
begin
  FIsDragImagePainted := True;
end;

procedure TcxPCCustomPainter.StopDragImagePainted;
begin
  FIsDragImagePainted := False;
end;

{ TcxPCCustomElementViewInfo }

function TcxPCCustomElementViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
 // Result := PtInElement(P);
  Result := ControlViewInfo.Controller.HitTest.HitObject = Self;
end;

function TcxPCCustomElementViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxPCCustomElementViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

procedure TcxPCCustomElementViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
end;

function TcxPCCustomElementViewInfo.GetAbsoluteBounds: TRect;
begin
  Result := Bounds;
end;

function TcxPCCustomElementViewInfo.GetHint: string;
begin
  Result := '';
end;

function TcxPCCustomElementViewInfo.GetHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean;
begin
  Result := PtInElement(AHitTest.HitPoint);
  if Result then
    SetHitTest(AHitTest);
end;

function TcxPCCustomElementViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := ControlViewInfo.ScaleFactor;
end;

function TcxPCCustomElementViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(FBounds, APoint);
end;

procedure TcxPCCustomElementViewInfo.SetHitTest(AHitTest: TcxCustomTabControlHitTest);
begin
  AHitTest.SetBitState(GetHitTestIndex, True);
  AHitTest.FHitObject := Self;
end;

{ TcxPCCustomButtonViewInfo }

destructor TcxPCCustomButtonViewInfo.Destroy;
begin
  ControlViewInfo.ButtonDestroying(Self);
  inherited Destroy;
end;

function TcxPCCustomButtonViewInfo.HasImage: Boolean;
begin
  Result := ImageIndex <> -1;
end;

function TcxPCCustomButtonViewInfo.CanClick: Boolean;
begin
  Result := State <> nbsDisabled;
end;

procedure TcxPCCustomButtonViewInfo.Click;
begin
  if CanClick then
    DoClick;
end;

procedure TcxPCCustomButtonViewInfo.DoClick;
begin
// do nothing
end;

procedure TcxPCCustomButtonViewInfo.Invalidate;
begin
  ControlViewInfo.InvalidateRect(AbsoluteBounds, False);
end;

function TcxPCCustomButtonViewInfo.GetHint: string;
begin
  Result := ControlViewInfo.Properties.GetButtonHint(Self);
end;

function TcxPCCustomButtonViewInfo.GetImageIndex: TcxImageIndex;
begin
  Result := -1;
end;

function TcxPCCustomButtonViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(AbsoluteBounds, APoint);
end;

function TcxPCCustomButtonViewInfo.GetState: TcxPCNavigatorButtonState;
begin
  Result := FState;
end;

procedure TcxPCCustomButtonViewInfo.SetState(AValue: TcxPCNavigatorButtonState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

{ TcxPCCustomTabButtonViewInfo }

constructor TcxPCCustomTabButtonViewInfo.Create(ATabViewInfo: TcxTabViewInfo);
begin
  inherited Create;
  FTabViewInfo := ATabViewInfo;
end;

function TcxPCCustomTabButtonViewInfo.GetAbsoluteBounds: TRect;
begin
  Result := FTabViewInfo.GetAbsolutePartRect(Bounds);
end;

function TcxPCCustomTabButtonViewInfo.GetControlViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := FTabViewInfo.ControlViewInfo;
end;

function TcxPCCustomTabButtonViewInfo.GetState: TcxPCNavigatorButtonState;
begin
  if ControlViewInfo.IsTabAccessible(FTabViewInfo.VisibleIndex) then
    Result := inherited GetState
  else
    Result := nbsDisabled;
end;

{ TcxPCTabButtonViewInfo }

constructor TcxPCTabButtonViewInfo.Create(ATabViewInfo: TcxTabViewInfo;
  AButton: TcxPCButton);
begin
  inherited Create(ATabViewInfo);
  FButton := AButton;
end;

procedure TcxPCTabButtonViewInfo.DoClick;
begin
  Button.DoClick;
end;

function TcxPCTabButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := pchtTabButton;
end;

function TcxPCTabButtonViewInfo.GetImageIndex: TcxImageIndex;
begin
  Result := Button.TabImageIndex;
end;

function TcxPCTabButtonViewInfo.GetState: TcxPCNavigatorButtonState;
begin
  if FButton.Enabled then
    Result := inherited GetState
  else
    Result := nbsDisabled;
end;

function TcxPCTabButtonViewInfo.GetWidth: Integer;
begin
  if ControlViewInfo.IsTabButtonImagesAssigned then
  begin
    Result := cxMarginsWidth(ControlViewInfo.Painter.GetTabButtonGlyphOffset) +
      dxGetImageSize(ControlViewInfo.Properties.TabButtonImages, ScaleFactor).cx;
  end
  else
    Result := ControlViewInfo.Painter.GetCloseButtonSize.cx;
end;

{ TcxPCTabCloseButtonViewInfo }

function TcxPCTabCloseButtonViewInfo.GetWidth: Integer;
begin
  Result := ControlViewInfo.Painter.GetCloseButtonSize.cx;
  if ControlViewInfo.TabButtonHeight > ControlViewInfo.Painter.GetCloseButtonSize.cy then
    Result := MulDiv(Result, ControlViewInfo.TabButtonHeight, ControlViewInfo.Painter.GetCloseButtonSize.cy);
end;

procedure TcxPCTabCloseButtonViewInfo.DoClick;
begin
  ControlViewInfo.Properties.DoCloseButtonClick(TabViewInfo.Index);
end;

function TcxPCTabCloseButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := pchtTabCloseButton;
end;

{TcxPCCustomHeaderButtonViewInfo}

constructor TcxPCCustomHeaderButtonViewInfo.Create(ATabControlViewInfo: TcxCustomTabControlViewInfo);
begin
  inherited Create;
  FTabControlViewInfo := ATabControlViewInfo;
end;

function TcxPCCustomHeaderButtonViewInfo.IsNavigatorButton: Boolean;
begin
  Result := GetHitTestIndex = pchtNavigatorButton;
end;

function TcxPCCustomHeaderButtonViewInfo.IsNavigatorButton(AButtonTypes: TcxPCNavigatorButtons): Boolean;
begin
  Result := False;
end;

function TcxPCCustomHeaderButtonViewInfo.AllowHotTrack: Boolean;
begin
  Result := not ControlViewInfo.Properties.IsDesigning;
end;

function TcxPCCustomHeaderButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := pchtHeaderButton;
end;

function TcxPCCustomHeaderButtonViewInfo.GetControlViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := FTabControlViewInfo;
end;

{TcxPCHeaderButtonViewInfo}

constructor TcxPCHeaderButtonViewInfo.Create(ATabControlViewInfo: TcxCustomTabControlViewInfo;
  AButton: TcxPCButton);
begin
  inherited Create(ATabControlViewInfo);
  FButton := AButton;
end;

function TcxPCHeaderButtonViewInfo.GetWidth: Integer;
begin
  if ControlViewInfo.IsHeaderButtonImagesAssigned then
  begin
    Result := cxMarginsWidth(ControlViewInfo.Painter.GetHeaderButtonGlyphOffset) +
      dxGetImageSize(ControlViewInfo.Properties.HeaderButtonImages, ScaleFactor).cx;
  end
  else
    Result := ControlViewInfo.Painter.GetButtonWidth(nbClose);
end;

procedure TcxPCHeaderButtonViewInfo.DoClick;
begin
  Button.DoClick;
end;

function TcxPCHeaderButtonViewInfo.GetImageIndex: TcxImageIndex;
begin
  Result := Button.HeaderImageIndex;
end;

function TcxPCHeaderButtonViewInfo.GetState: TcxPCNavigatorButtonState;
begin
  if FButton.Enabled then
    Result := inherited GetState
  else
    Result := nbsDisabled;
end;

{ TcxPCNavigatorButtonViewInfo }

constructor TcxPCNavigatorButtonViewInfo.Create(ATabControlViewInfo: TcxCustomTabControlViewInfo;
  AButtonType: TcxPCNavigatorButton);
begin
  inherited Create(ATabControlViewInfo);
  FButtonType := AButtonType;
end;

function TcxPCNavigatorButtonViewInfo.GetWidth: Integer;
begin
  Result := ControlViewInfo.Painter.GetButtonWidth(ButtonType);
  if ControlViewInfo.HeaderButtonHeight > ControlViewInfo.Painter.GetButtonHeight then
    Result := MulDiv(Result, ControlViewInfo.HeaderButtonHeight, ControlViewInfo.Painter.GetButtonHeight);
end;

function TcxPCNavigatorButtonViewInfo.IsNavigatorButton(AButtonTypes: TcxPCNavigatorButtons): Boolean;
begin
  Result := ButtonType in AButtonTypes;
end;

function TcxPCNavigatorButtonViewInfo.AllowHotTrack: Boolean;
begin
  Result := inherited AllowHotTrack or (ButtonType in [nbTopLeft, nbBottomRight]);
end;

procedure TcxPCNavigatorButtonViewInfo.DoClick;
begin
  case ButtonType of
    nbGoDialog:
      begin
        ControlViewInfo.PressedNavigatorButton := Self;
        ControlViewInfo.Controller.ShowGoDialog;
      end;
    nbClose:
      ControlViewInfo.Controller.CloseButtonClick;
  end;
end;

function TcxPCNavigatorButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := pchtNavigatorButton;
end;

{ TcxPCButtonViewInfos }

constructor TcxPCButtonViewInfos.Create(ATabViewInfo: TcxTabViewInfo);
begin
  inherited Create;
  FTabViewInfo := ATabViewInfo;
  FItems := TcxObjectList.Create;
end;

destructor TcxPCButtonViewInfos.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxPCButtonViewInfos.Add(AButtonViewInfo: TcxPCCustomTabButtonViewInfo);
begin
  FItems.Add(AButtonViewInfo);
end;

function TcxPCButtonViewInfos.GetHitTest(
  AHitTest: TcxCustomTabControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitTest(AHitTest);
    if Result then
      Break;
  end;
end;

function TcxPCButtonViewInfos.GetCloseButtonInfo: TcxPCTabCloseButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].GetHitTestIndex = pchtTabCloseButton then
    begin
      Result := Items[I] as TcxPCTabCloseButtonViewInfo;
      Break;
    end;
end;

function TcxPCButtonViewInfos.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxPCButtonViewInfos.GetItems(Index: Integer): TcxPCCustomTabButtonViewInfo;
begin
  Result := TcxPCCustomTabButtonViewInfo(FItems[Index]);
end;

{ TcxTabViewInfo }

constructor TcxTabViewInfo.Create(ATab: TcxTab; ATabsViewInfo: TcxTabsViewInfo);
begin
  inherited Create;
  FTab := ATab;
  FTabsViewInfo := ATabsViewInfo;
  FButtonInfos := TcxPCButtonViewInfos.Create(Self);
  Tab.DestroyHandlers.Add(TabDestroyHandler);
end;

destructor TcxTabViewInfo.Destroy;
begin
  ControlViewInfo.TabDestroying(Self);
  if Tab <> nil then
    Tab.DestroyHandlers.Remove(TabDestroyHandler);
  FreeAndNil(FButtonInfos);
  inherited;
end;

function TcxTabViewInfo.ActuallyEnabled: Boolean;
begin
  Result := ControlViewInfo.IsEnabled and Tab.Enabled;
end;

function TcxTabViewInfo.ActuallyVisible: Boolean;
begin
  Result := ControlViewInfo.IsTabActuallyVisible(Self);
end;

function TcxTabViewInfo.AllowMultilineCaption: Boolean;
begin
  Result := ControlViewInfo.MultiLineTabCaptions and Painter.AllowMultiLineTabCaptions;
end;

function TcxTabViewInfo.CanFocus: Boolean;
begin
  Result := True;
end;

function TcxTabViewInfo.HasButton(AButton: TcxPCButton): Boolean;
begin
  Result := CheckHasButton(Properties.CustomButtons.Mode) and AButton.DoCanShow(Tab.Index);
end;

function TcxTabViewInfo.HasButtons: Boolean;
begin
  Result := ButtonInfos.Count > 0;
end;

function TcxTabViewInfo.HasCaption: Boolean;
begin
  Result := Caption <> '';
end;

function TcxTabViewInfo.HasCloseButton: Boolean;
begin
  Result := Properties.CanCloseButtonShow(Tab) and
    CheckHasButton(Properties.CloseButtonMode);
end;

function TcxTabViewInfo.HasImage: Boolean;
begin
  Result := ImageIndex <> -1;
end;

function TcxTabViewInfo.IsActive: Boolean;
begin
  Result := ControlViewInfo.TabIndex = Index;
end;

function TcxTabViewInfo.IsFocused: Boolean;
begin
  Result := ControlViewInfo.FocusedTabVisibleIndex = VisibleIndex;
end;

function TcxTabViewInfo.IsHighlighted: Boolean;
begin
  Result := Tab.Highlighted;
end;

function TcxTabViewInfo.IsHotTrack: Boolean;
begin
  Result := ControlViewInfo.HotTrackTabVisibleIndex = VisibleIndex;
end;

function TcxTabViewInfo.IsMainTab: Boolean;
begin
  Result := ControlViewInfo.MainTabVisibleIndex = VisibleIndex;
end;

function TcxTabViewInfo.IsPressed: Boolean;
begin
  Result := ControlViewInfo.PressedTabVisibleIndex = VisibleIndex;
end;

function TcxTabViewInfo.IsSelected: Boolean;
begin
  Result := Tab.Selected;
end;

function TcxTabViewInfo.IsTracking: Boolean;
begin
  Result := ControlViewInfo.TrackingTabVisibleIndex = VisibleIndex;
end;

function TcxTabViewInfo.IsVisibleForGoDialog: Boolean;
begin
  Result := ControlViewInfo.IsTabAccessible(VisibleIndex);
end;

function TcxTabViewInfo.IsButtonsPlacedFirst: Boolean;
begin
  Result := GetRelativeTextRotationAngle = raMinus90;
end;

function TcxTabViewInfo.GetDefinedWidth: Integer;
begin
  Result := ControlViewInfo.TabWidth;
end;

function TcxTabViewInfo.GetLimitedWidth: Integer;
begin
  Result := GetDefinedWidth;
  if ControlViewInfo.ActuallyRotate then
    Result := Max(Result, ControlViewInfo.ActuallyRotatedTabsMaxWidth);
end;

function TcxTabViewInfo.GetRelativeTextRotationAngle: TcxRotationAngle;
begin
  case PaintingPositionIndex of
    2, 9, 12: Result := raPlus90;
    3, 6, 11: Result := raMinus90;
    5, 7, 10: Result := ra180;
  else
    Result := ra0;
  end;
end;

procedure TcxTabViewInfo.CalculateNormalWidth;
begin
  FNormalWidth := Painter.CalculateTabNormalWidth(Self);
end;

procedure TcxTabViewInfo.CorrectTabNormalWidth(var AValue: Integer);
begin
  Painter.CorrectTabNormalWidth(AValue);
end;

procedure TcxTabViewInfo.CalculateParts;

  procedure ResetPartParams;
  begin
    FImageRect := cxEmptyRect;
    FTextRect := cxEmptyRect;
    FIsTextTooLong := False;
  end;

begin
  if ControlViewInfo.IsTabsVisible then
  begin
    ResetPartParams;
    PrepareCanvasFont(Canvas);
    CalculateIsTextTooLong;
    CalculateContentRect;

    if HasButtons then
      CalculateButtonVerticalPositions;
    if Painter.IsTabHasImage(VisibleIndex) then
      CalculateImageVerticalPosition;
    if HasCaption then
      CalculateTextVerticalPosition;

    CalculateHorizontalPositions;
  end;
end;

procedure TcxTabViewInfo.PrepareCanvasFont(ACanvas: TcxCanvas);
begin
  ControlViewInfo.PrepareTabCanvasFont(Self, ACanvas);
end;

procedure TcxTabViewInfo.RecreateButtonViewInfos;
var
  I: Integer;
  AButton: TcxPCButton;
begin
  FButtonInfos.FItems.Clear;
  if HasCloseButton then
    FButtonInfos.FItems.Add(TcxPCTabCloseButtonViewInfo.Create(Self));
  for I := 0 to Properties.CustomButtons.Count - 1 do
  begin
    AButton := Properties.CustomButtons.Buttons[I];
    if AButton.Visible and (AButton.Position in [pcbpTabs, pcbpTabsAndHeader]) and HasButton(AButton) then
      FButtonInfos.Add(TcxPCTabButtonViewInfo.Create(Self, AButton));
  end;
end;

procedure TcxTabViewInfo.CalculateAngleDependentPartBounds(var ACalcRect: TRect;
  const APartHeight: Integer; ADrawOffset: TRect);
begin
  Painter.CorrectTabContentVerticalOffset(VisibleIndex, ADrawOffset);
  with ACalcRect do
    case InternalGetTextRotationAngle(ControlViewInfo) of
      ra0:
        begin
          Inc(Top, ADrawOffset.Top);
          Dec(Bottom, ADrawOffset.Bottom);
          Top := Top + (Bottom - Top - APartHeight) div 2;
          Bottom := Top + APartHeight;
          if ControlViewInfo.IsNativePainting and IsMainTab and
            (GetRelativeTextRotationAngle = ra180) then
            OffsetRect(ACalcRect, 0, 1);
        end;
      raPlus90:
        begin
          Inc(Left, ADrawOffset.Top);
          Dec(Right, ADrawOffset.Bottom);
          Left := Left + (Right - Left - APartHeight) div 2;
          Right := Left + APartHeight;
        end;
      raMinus90:
        begin
          Inc(Left, ADrawOffset.Bottom);
          Dec(Right, ADrawOffset.Top);
          Right := Right - (Right - Left - APartHeight) div 2;
          Left := Right - APartHeight;
        end;
    end;
end;

procedure TcxTabViewInfo.CalculateButtonHorizontalPositions;
type
  TcxRectCoordinate = (rtcLeft, rtcTop, rtcRight, rtcBottom);
const
  ARotatePosition: array [TcxRotationAngle] of TcxTabPosition =
    (tpTop, tpRight, tpLeft, tpBottom);

  function NextPosition(APosition: Integer; AAngle: TcxRotationAngle;
    ADisplacement: Integer): Integer;
  begin
    case AAngle of
      ra0, raMinus90:
        Result := APosition - ADisplacement
    else // raPlus90, ra180
      Result := APosition + ADisplacement;
    end;
  end;

  function SetRectCoordinate(const ARect: TRect; AAngle: TcxRotationAngle;
    ACoordinate: TcxRectCoordinate; AValue: Integer): TRect;
  begin
    Result := RotateRect(ARect, ARotatePosition[AAngle]);
    case ACoordinate of
      rtcLeft: Result.Left := AValue;
      rtcTop: Result.Top := AValue;
      rtcRight: Result.Right := AValue;
    else // rtcBottom
      Result.Bottom := AValue;
    end;
    Result := RotateRectBack(Result, ARotatePosition[AAngle]);
  end;

  function GetRectCoordinate(const ARect: TRect; AAngle: TcxRotationAngle;
    ACoordinate: TcxRectCoordinate): Integer;
  var
    R: TRect;
  begin
    R := RotateRect(ARect, ARotatePosition[AAngle]);
    case ACoordinate of
      rtcLeft: Result := R.Left;
      rtcTop: Result := R.Top;
      rtcRight: Result := R.Right;
    else // rtcBottom
      Result := R.Bottom;
    end;
  end;

  procedure InternalCalculateButtonHorizontalPositions(APosition: Integer; AAngle: TcxRotationAngle);
  var
    I: Integer;
  begin
    for I := 0 to ButtonsCount - 1 do
    begin
      APosition := NextPosition(APosition, AAngle, IfThen(I > 0, Painter.GetTabButtonDistance));
      FButtonInfos[I].Bounds := SetRectCoordinate(FButtonInfos[I].Bounds, AAngle, rtcRight, APosition);
      APosition := NextPosition(APosition, AAngle, FButtonInfos[I].GetWidth);
      FButtonInfos[I].Bounds := SetRectCoordinate(FButtonInfos[I].Bounds, AAngle, rtcLeft, APosition);
    end;
  end;

  function GetAbsoluteAngle(AAngle: TcxRotationAngle): TcxRotationAngle;
  begin
    if InternalIsVerticalText(ControlViewInfo) then
      if (AAngle = raPlus90) xor IsButtonsPlacedFirst then
        Result := raPlus90
      else
        Result := raMinus90
    else
      if IsButtonsPlacedFirst then
        Result := ra180
      else
        Result := ra0;
  end;

var
  AOffsets: TRect;
  AAbsoluteAngle: TcxRotationAngle;
  APosition: Integer;
begin
  AAbsoluteAngle := GetAbsoluteAngle(InternalGetTextRotationAngle(ControlViewInfo));
  APosition := GetRectCoordinate(FContentRect, AAbsoluteAngle, rtcRight);
  AOffsets := Painter.GetCloseButtonOffset(VisibleIndex);
  APosition := NextPosition(APosition, AAbsoluteAngle, AOffsets.Right);
  InternalCalculateButtonHorizontalPositions(APosition, AAbsoluteAngle);
end;

procedure TcxTabViewInfo.CalculateButtonVerticalPositions;
var
  AButtonRect: TRect;
  I: Integer;
begin
  for I := 0 to ButtonsCount - 1 do
  begin
    AButtonRect := FContentRect;
    CalculateAngleDependentPartBounds(AButtonRect, ControlViewInfo.TabButtonHeight,
      Painter.GetCloseButtonOffset(VisibleIndex));
    FButtonInfos[I].Bounds := AButtonRect;
  end;
end;

procedure TcxTabViewInfo.CalculateContentRect;
var
  AContentOffset: TcxPCWOffset;
begin
  FContentRect := GetTabRect;
  AContentOffset := GetContentOffset;

  case InternalGetTextRotationAngle(ControlViewInfo) of
    ra0:
      begin
        Inc(FContentRect.Left, AContentOffset.Left);
        Dec(FContentRect.Right, AContentOffset.Right);
      end;
    raMinus90:
      begin
        Inc(FContentRect.Top, AContentOffset.Left);
        Dec(FContentRect.Bottom, AContentOffset.Right);
      end;
  else // raPlus90
    Dec(FContentRect.Bottom, AContentOffset.Left);
    Inc(FContentRect.Top, AContentOffset.Right);
  end;
end;

procedure TcxTabViewInfo.CalculateImageAndTextHorizontalPositions;

  function GetVisibleTextWidth(const AAccessibleWidth: Integer): Integer;
  begin
    if GetTextSize.cx < AAccessibleWidth then
      Result := GetTextSize.cx
    else
      Result := AAccessibleWidth;
  end;

var
  AIsAssignedImagesAndRotate, AIsTabHasImage, AIsTabHasImageArea: Boolean;
  AImageAreaWidth, AImageTextDistance: Integer;
  AContentHorizontalOffset: Integer;
  AContentWidth: Integer;
  ATextAndImageAreaWidth: Integer;
  ATextWidth: Integer;
  AContentRect: TRect;
begin
  AContentRect := GetTabImageAndTextRect;
  AContentWidth := IfThen(InternalIsVerticalText(ControlViewInfo),
    cxRectHeight(AContentRect), cxRectWidth(AContentRect));

  AIsAssignedImagesAndRotate := IsImagesAssigned and ControlViewInfo.ActuallyRotate;
  AIsTabHasImage := HasImage;
  AIsTabHasImageArea := AIsAssignedImagesAndRotate or AIsTabHasImage;
  AImageTextDistance := IfThen(AIsTabHasImageArea, Painter.GetImageTextDistance(VisibleIndex));
  AImageAreaWidth := IfThen(AIsTabHasImageArea, GetImageAreaWidth);

  ATextWidth := GetVisibleTextWidth(AContentWidth - AImageAreaWidth - AImageTextDistance);
  ATextAndImageAreaWidth := AImageAreaWidth + AImageTextDistance + ATextWidth;

  if ATextAndImageAreaWidth <= 0 then
  begin
    FImageRect := cxEmptyRect;
    FTextRect := cxEmptyRect;
    Exit;
  end;
  AContentHorizontalOffset := IfThen(not FIsTextTooLong and
    not IsCompactMode and not AIsAssignedImagesAndRotate,
    GetContentHorizontalOffset(AContentWidth, ATextAndImageAreaWidth));

  case InternalGetTextRotationAngle(ControlViewInfo) of
    ra0:
      begin
        Inc(AContentRect.Left, AContentHorizontalOffset);
        if AIsTabHasImage then
        begin
          FImageRect.Left := AContentRect.Left + ControlViewInfo.ImageBorder;
          FImageRect.Right := FImageRect.Left + ControlViewInfo.GetTabImageSize.cx;
        end;
        Inc(AContentRect.Left, AImageAreaWidth + AImageTextDistance);

        if not FIsTextTooLong and AIsAssignedImagesAndRotate then
          Inc(AContentRect.Left,
            GetContentHorizontalOffset(cxRectWidth(AContentRect), ATextWidth));
        FTextRect.Left := AContentRect.Left;
        FTextRect.Right := FTextRect.Left + ATextWidth;
      end;
    raPlus90:
      begin
        Dec(AContentRect.Bottom, AContentHorizontalOffset);
        if AIsTabHasImage then
        begin
          FImageRect.Bottom := AContentRect.Bottom - ControlViewInfo.ImageBorder;
          FImageRect.Top := FImageRect.Bottom - ControlViewInfo.GetTabImageSize.cx;
        end;
        Dec(AContentRect.Bottom, AImageAreaWidth + AImageTextDistance);

        if not FIsTextTooLong and AIsAssignedImagesAndRotate then
          Dec(AContentRect.Bottom,
            GetContentHorizontalOffset(cxRectHeight(AContentRect), ATextWidth));
        FTextRect.Bottom := AContentRect.Bottom;
        FTextRect.Top := FTextRect.Bottom - ATextWidth;
      end;
    raMinus90:
      begin
        Inc(AContentRect.Top, AContentHorizontalOffset);
        if AIsTabHasImage then
        begin
          FImageRect.Top := AContentRect.Top + ControlViewInfo.ImageBorder;
          FImageRect.Bottom := FImageRect.Top + ControlViewInfo.GetTabImageSize.cx;
        end;
        Inc(AContentRect.Top, AImageAreaWidth + AImageTextDistance);

        if not FIsTextTooLong and AIsAssignedImagesAndRotate then
          Inc(AContentRect.Top,
            GetContentHorizontalOffset(cxRectHeight(AContentRect), ATextWidth));
        FTextRect.Top := AContentRect.Top;
        FTextRect.Bottom := FTextRect.Top + ATextWidth;
      end;
  end;
end;

procedure TcxTabViewInfo.CalculateImageHorizontalPosition;
var
  ADrawImageOffset: TcxPCWOffset;
  AImageSize: TSize;
  AImageRect: TRect;

begin
  AImageSize := ControlViewInfo.GetTabImageSize;
  AImageRect := GetTabImageAndTextRect;
  ADrawImageOffset := Painter.GetDrawImageWithoutTextWOffset(VisibleIndex);
  with AImageRect do
    case InternalGetTextRotationAngle(ControlViewInfo) of
      ra0:
        begin
          Inc(AImageRect.Left, ADrawImageOffset.Left);
          Dec(AImageRect.Right, ADrawImageOffset.Right);
          begin
            FImageRect.Left := Left + (Right - Left - AImageSize.cx) div 2;
            FImageRect.Right := FImageRect.Left + AImageSize.cx;
          end;
        end;
      raPlus90:
        begin
          Dec(AImageRect.Bottom, ADrawImageOffset.Left);
          Inc(AImageRect.Top, ADrawImageOffset.Right);
          if Painter.IsTabPressable then
          begin
            FImageRect.Top := Top + (Bottom - Top - AImageSize.cx) div 2;
            FImageRect.Bottom := FImageRect.Top + AImageSize.cx;
          end
          else
          begin
            FImageRect.Bottom := Bottom - (Bottom - Top - AImageSize.cx) div 2;
            FImageRect.Top := FImageRect.Bottom - AImageSize.cx;
          end;
        end;
      raMinus90:
        begin
          Inc(AImageRect.Top, ADrawImageOffset.Left);
          Dec(AImageRect.Bottom, ADrawImageOffset.Right);
          FImageRect.Top := Top + (Bottom - Top - AImageSize.cx) div 2;
          FImageRect.Bottom := FImageRect.Top + AImageSize.cx;
        end;
    end;
end;

procedure TcxTabViewInfo.CalculateImageVerticalPosition;
begin
  FImageRect := FContentRect;
  CalculateAngleDependentPartBounds(FImageRect, ControlViewInfo.GetTabImageSize.cy,
    Painter.GetDrawImageOffset(VisibleIndex));
end;

procedure TcxTabViewInfo.CalculateIsTextTooLong;
var
  AContentWidth: Integer;
  AOffset: TcxPCWOffset;
  AButtonsAreaWidth, AImageAreaWidth, AImageTextDistance: Integer;
  AIsTabHasImageArea: Boolean;
  ATabRect: TRect;
begin
  ATabRect := GetTabRect;
  AOffset := GetContentOffset;
  AContentWidth := IfThen(InternalIsVerticalText(ControlViewInfo),
    cxRectHeight(ATabRect), cxRectWidth(ATabRect));
  Dec(AContentWidth, AOffset.Left + AOffset.Right);

  AIsTabHasImageArea := HasImage or IsImagesAssigned and ControlViewInfo.ActuallyRotate;
  AImageTextDistance := IfThen(AIsTabHasImageArea, Painter.GetImageTextDistance(VisibleIndex));
  AImageAreaWidth := IfThen(AIsTabHasImageArea, GetImageAreaWidth);
  AButtonsAreaWidth := IfThen(HasButtons, Painter.GetTabButtonsAreaWidth(VisibleIndex));
  FIsTextTooLong := TextWidth > (AContentWidth - AImageAreaWidth - AImageTextDistance - AButtonsAreaWidth);
end;

procedure TcxTabViewInfo.CalculateHorizontalPositions;
begin
  if HasButtons then
    CalculateButtonHorizontalPositions;
  if HasImage and not HasCaption then
    CalculateImageHorizontalPosition;
  if HasCaption then
    CalculateImageAndTextHorizontalPositions;
end;

procedure TcxTabViewInfo.CalculateTextVerticalPosition;
begin
  FTextRect := FContentRect;
  CalculateAngleDependentPartBounds(FTextRect, TextHeight, Painter.GetDrawTextHOffset(VisibleIndex));
end;

function TcxTabViewInfo.GetContentHorizontalOffset(ATabWidth, AContentWidth: Integer): Integer;
var
  ACaptionAlignment: TAlignment;
  AMaxTabCaptionWidth: Integer;
begin
  ACaptionAlignment := Properties.TabCaptionAlignment;
  if ACaptionAlignment = taCenter then
    Result := (ATabWidth - AContentWidth) div 2
  else
    if not ControlViewInfo.ActuallyRotate or (GetDefinedWidth > 0) or
      (ControlViewInfo.ActuallyRotatedTabsMaxWidth > 0) then
    begin
      case ACaptionAlignment of
        taLeftJustify:
          Result := 0;//(ATabWidth - AMaxTabCaptionWidth) div 2;
      else //taRightJustify
        Result := (ATabWidth - AContentWidth);
      end;
    end
    else
    begin
      AMaxTabCaptionWidth := ControlViewInfo.FMaxTabCaptionWidth;
      case ACaptionAlignment of
        taLeftJustify:
          Result := (ATabWidth - AMaxTabCaptionWidth) div 2;
      else //taRightJustify
        Result := (ATabWidth - AMaxTabCaptionWidth) div 2 +
          AMaxTabCaptionWidth - AContentWidth;
      end;
    end;
end;

function TcxTabViewInfo.GetControlViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := TabsViewInfo.ControlViewInfo;
end;

function TcxTabViewInfo.GetScrollingArea: TRect;
const
  AMinScrollingAreaWidth = 4;
var
  AWidth: Integer;
begin
  Result := NormalRect;
  if ControlViewInfo.FIsLastTabFullyVisible or (VisibleIndex <> ControlViewInfo.LastTabVisibleIndex) then
  begin
    case ControlViewInfo.TabPosition of
      tpTop, tpBottom:
        begin
          AWidth := Max(cxRectWidth(Result) div 4, AMinScrollingAreaWidth);
          if ControlViewInfo.IsRightToLeftAlignment then
            Result.Left := Result.Right - AWidth
          else
            Result.Right := Result.Left + AWidth;
        end;
      tpLeft, tpRight:
        begin
          AWidth := Max(cxRectHeight(Result) div 4, AMinScrollingAreaWidth);
          if ControlViewInfo.IsBottomToTopAlignment then
            Result.Top := Result.Bottom - AWidth
          else
            Result.Bottom := Result.Top + AWidth;
        end;
    end;
  end;
end;

function TcxTabViewInfo.GetHint: string;
begin
  Result := Properties.GetTabHint(Tab);
end;

function TcxTabViewInfo.GetHitTestIndex: Integer;
begin
  Result := pchtTab;
end;

function TcxTabViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(VisibleRect, APoint) and
    ControlViewInfo.PtInTab(VisibleIndex, APoint.X, APoint.Y);
end;

procedure TcxTabViewInfo.SetHitTest(AHitTest: TcxCustomTabControlHitTest);
begin
  inherited SetHitTest(AHitTest);
  AHitTest.FHitTab := Self;
  if not Properties.IsDesigning then
    FButtonInfos.GetHitTest(AHitTest);
end;

function TcxTabViewInfo.CanClick: Boolean;
begin
  Result := True;
end;

function TcxTabViewInfo.CanDrag: Boolean;
begin
  Result := True;
end;

function TcxTabViewInfo.CanMultiSelect: Boolean;
begin
  Result := ControlViewInfo.AllowMultiSelect;
end;

function TcxTabViewInfo.CanSelect: Boolean;
begin
  Result := True;
end;

procedure TcxTabViewInfo.DoHandleDialogChar(Key: Integer);
begin
  ControlViewInfo.Controller.TabClick(VisibleIndex, [], False);
end;

procedure TcxTabViewInfo.DoSelect(AAddToSelected: Boolean);
begin
  if AAddToSelected and CanMultiSelect then
  begin
    if VisibleIndex = ControlViewInfo.MainTabVisibleIndex then
      ControlViewInfo.TabIndex := -1
    else
      Tab.Selected := not Tab.Selected;
  end
  else
    ControlViewInfo.TabIndex := Index;
end;

procedure TcxTabViewInfo.DoSetFocus;
begin
  ControlViewInfo.FocusedTabVisibleIndex := VisibleIndex;
end;

procedure TcxTabViewInfo.DoClick(AShift: TShiftState);
begin
  ControlViewInfo.Controller.DoTabClick(VisibleIndex, AShift);
end;

procedure TcxTabViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FNormalPosition := TdxRightToLeftLayoutConverter.ConvertRect(NormalRect, AClientBounds).TopLeft;
  if not ControlViewInfo.IsNativePainting then
    FContentRect := TdxRightToLeftLayoutConverter.ConvertRect(FContentRect, AClientBounds);
  DoRightToLeftContentConversion(AClientBounds);
end;

function TcxTabViewInfo.HandleDialogChar(Key: Integer): Boolean;
begin
  Result := ShowAccelChar and IsAccel(Key, Caption) and ControlViewInfo.IsTabAccessible(VisibleIndex);
  if Result then
    DoHandleDialogChar(Key);
end;

procedure TcxTabViewInfo.Select(AAddToSelected: Boolean);
begin
  if CanSelect then
    DoSelect(AAddToSelected);
end;

procedure TcxTabViewInfo.SetFocus;
begin
  if CanFocus then
    DoSetFocus;
end;

procedure TcxTabViewInfo.Click(AShift: TShiftState);
begin
  if CanClick then
    DoClick(AShift);
end;

function TcxTabViewInfo.IsCompactMode: Boolean;
begin
  Result := (Properties.CloseButtonMode = cbmActiveAndHoverTabs) or
    (Properties.CustomButtons.Mode = cbmActiveAndHoverTabs);
end;

function TcxTabViewInfo.IsNewButton: Boolean;
begin
  Result := Tab.IsNewButton;
end;

function TcxTabViewInfo.IsPressable: Boolean;
begin
  Result := ControlViewInfo.Painter.IsTabPressable;
end;

procedure TcxTabViewInfo.TabDestroyHandler(Sender: TObject; const AEventArgs);
begin
  FTab := nil;
  if IsMainTab then
    ControlViewInfo.FMainTabVisibleIndex := -1;
  if IsTracking then
    ControlViewInfo.FTrackingTabVisibleIndex := -1;
  if IsFocused then
    ControlViewInfo.FFocusedTabVisibleIndex := -1;
  if IsHotTrack then
    ControlViewInfo.FHotTrackTabVisibleIndex := -1;
  if IsPressed then
    ControlViewInfo.FPressedTabVisibleIndex := -1;
  TabsViewInfo.FViewInfos.Delete(VisibleIndex);
end;

function TcxTabViewInfo.CheckHasButton(AShowMode: TcxPCButtonMode): Boolean;
begin
  Result := ((AShowMode = cbmEveryTab) or
    (AShowMode = cbmActiveTab) and IsActive or
    (AShowMode = cbmActiveAndHoverTabs) and (IsActive or IsHotTrack));
end;

procedure TcxTabViewInfo.DoRightToLeftContentConversion(const R: TRect);
var
  I: Integer;
  AParentRect: TRect;
begin
  if ControlViewInfo.IsNativePainting then
    AParentRect := FContentRect
  else
    AParentRect := R;
  if not ControlViewInfo.IsNativePainting or (GetRelativeTextRotationAngle in [ra0, ra180]) then
  begin
    FImageRect := TdxRightToLeftLayoutConverter.ConvertRect(FImageRect, AParentRect);
    FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, AParentRect);
    for I := 0 to FButtonInfos.Count - 1 do
      FButtonInfos.Items[I].DoRightToLeftConversion(AParentRect);
  end;
end;

function TcxTabViewInfo.GetButtonsCount: Integer;
begin
  Result := ButtonInfos.Count;
end;

function TcxTabViewInfo.GetCanvas: TcxCanvas;
begin
  Result := ControlViewInfo.Canvas;
end;

function TcxTabViewInfo.GetCaption: string;
begin
  Result := Tab.Caption;
end;

function TcxTabViewInfo.GetCloseButtonInfo: TcxPCTabCloseButtonViewInfo;
begin
  Result := FButtonInfos.GetCloseButtonInfo;
end;

function TcxTabViewInfo.GetColor: TColor;
begin
  Result := Tab.Color;
end;

function TcxTabViewInfo.GetContentOffset: TcxPCWOffset;
begin
  if FIsTextTooLong then
    Result := Painter.GetTooNarrowTabContentWOffset(VisibleIndex)
  else
    Result := Painter.GetTabContentWOffset(VisibleIndex);
end;

function TcxTabViewInfo.GetIndex: Integer;
begin
  Result := Tab.Index;
end;

function TcxTabViewInfo.GetImageIndex: Integer;
begin
  Result := Tab.ImageIndex;
end;

function TcxTabViewInfo.GetFullRect: TRect;
begin
  Result := cxRectTransform(NormalRect, Painter.GetTabCorrection(VisibleIndex));
end;

function TcxTabViewInfo.GetEnabled: Boolean;
begin
  Result := Tab.Enabled;
end;

function TcxTabViewInfo.GetImageAreaWidth: Integer;
begin
  Result := ControlViewInfo.GetTabImageAreaWidth;
end;

function TcxTabViewInfo.GetNormalLongitudinalSize: Integer;
begin
  if ControlViewInfo.ActuallyRotate then
    Result := TabsViewinfo.TabNormalHeight
  else
    Result := FNormalWidth;
end;

function TcxTabViewInfo.GetNormalRect: TRect;
begin
  Result.Left := FNormalPosition.X;
  Result.Top := FNormalPosition.Y;
  if ControlViewInfo.IsVerticalText then
  begin
    Result.Right := Result.Left + TabsViewInfo.TabNormalHeight;
    if ControlViewInfo.ActuallyRotate then
      Result.Bottom := Result.Top + ControlViewInfo.RowHeight
    else
      Result.Bottom := Result.Top + FNormalWidth;
  end
  else
  begin
    if ControlViewInfo.ActuallyRotate then
      Result.Right := Result.Left + ControlViewInfo.RowHeight
    else
      Result.Right := Result.Left + FNormalWidth;
    Result.Bottom := Result.Top + TabsViewinfo.TabNormalHeight;
  end;
end;

function TcxTabViewInfo.GetPainter: TcxPCCustomPainter;
begin
  Result := ControlViewInfo.Painter;
end;

function TcxTabViewInfo.GetPaintingPosition: TcxTabPosition;
var
  AIsY: Boolean;
begin
  AIsY := ControlViewInfo.TabPosition in [tpTop, tpBottom];
  if VisibleRow < ControlViewInfo.TopOrLeftPartRowCount then
    if AIsY then
      Result := tpTop
    else
      Result := tpLeft
  else
    if AIsY then
      Result := tpBottom
    else
      Result := tpRight;
end;

function TcxTabViewInfo.GetPaintingPositionIndex: Integer;
const
  PaintingPositionIndexMap: array [TcxTabPosition, TcxRotationAngle] of Integer = (
//  0  +90 -90 180
    (1,  2,  3,  0),     // Top
    (10, 11, 12, 0),     // Bottom
    (6,  4,  5,  0),     // Left
    (9,  7,  8,  0)      // Right
  );
begin
  Result := PaintingPositionIndexMap[PaintingPosition, ControlViewInfo.GetTextRotationAngle];
end;

function TcxTabViewInfo.GetProperties: TcxCustomTabControlProperties;
begin
  Result := ControlViewInfo.Properties;
end;

function TcxTabViewInfo.GetShowAccelChar: Boolean;
begin
  Result := Tab.ShowAccelChar;
end;

function TcxTabViewInfo.GetAbsolutePartRect(const APartRect: TRect): TRect;
var
  ATabRect: TRect;
begin
  Result := APartRect;
  if ControlViewInfo.IsNativePainting then
  begin
    ATabRect := FullRect;
    if IsMainTab then
      ATabRect := Painter.GetExtendedRect(ATabRect, Rect(0, 0, 0, -1), PaintingPosition);
    case ControlViewInfo.GetTextRotationAngle of
      raPlus90:
        with Result do
          Result := Rect(Top, cxRectHeight(FullRect) - Right, Bottom, cxRectHeight(FullRect) - Left);
      raMinus90:
        with Result do
          Result := Rect(cxRectWidth(FullRect) - Bottom, Left, cxRectWidth(FullRect) - Top, Right);
    end;
    Result := cxRectOffset(Result, ATabRect.TopLeft);
  end;
end;

function TcxTabViewInfo.GetTabRect: TRect;
begin
  Result := FullRect;
  if ControlViewInfo.IsNativePainting then
    if ControlViewInfo.IsVerticalText then
      Result := Rect(0, 0, cxRectHeight(Result), cxRectWidth(Result))
    else
      Result := Rect(0, 0, cxRectWidth(Result), cxRectHeight(Result));
end;

function TcxTabViewInfo.GetTabImageAndTextRect: TRect;
var
  ACloseButtonAreaWidth: Integer;
  AAngle: TcxRotationAngle;
begin
  Result := FContentRect;
  ACloseButtonAreaWidth := IfThen(HasButtons, Painter.GetTabButtonsAreaWidth(VisibleIndex));
  AAngle := InternalGetTextRotationAngle(ControlViewInfo);
  if InternalIsVerticalText(ControlViewInfo) then
  begin
    if (AAngle = raPlus90) xor IsButtonsPlacedFirst then
      Inc(Result.Top, ACloseButtonAreaWidth)
    else
      Dec(Result.Bottom, ACloseButtonAreaWidth);
  end
  else
  begin
    if IsButtonsPlacedFirst then
      Inc(Result.Left, ACloseButtonAreaWidth)
    else
      Dec(Result.Right, ACloseButtonAreaWidth);
  end;
end;

function TcxTabViewInfo.GetTextSize: TSize;
var
  I, ALineCount: Integer;
  R: TRect;
  ATabWidthCorrection: Integer;
  AContentOffset: TcxPCWOffset;
  ATabCorrection: TRect;
begin
  if cxSizeIsEmpty(FTextSize) then
  begin
    PrepareCanvasFont(cxScreenCanvas);
    FIsMultiline := AllowMultilineCaption;
    if FIsMultiline then
    begin
      ALineCount := 1;
      for I := 1 to Length(Caption) do
        if Caption[I] = #13 then
          Inc(ALineCount);
      FIsMultiline := (GetLimitedWidth > 0) or (ALineCount > 1);
      if GetLimitedWidth > 0 then
      begin
        AContentOffset := GetContentOffset;
        ATabCorrection := Painter.GetTabCorrection(VisibleIndex);
        ATabWidthCorrection := AContentOffset.Left + AContentOffset.Right + ATabCorrection.Left - ATabCorrection.Right;
        R := Rect(0, 0, GetLimitedWidth - ATabWidthCorrection, 1);
        cxDrawText(cxScreenCanvas.Handle, Caption, R, Painter.GetCTFlags(VisibleIndex));
        FTextSize := cxSize(R);
      end
      else
        FTextSize := cxSize(cxGetTextRect(cxScreenCanvas.Font, Caption, ALineCount, True))
    end
    else
      FTextSize := cxTextSize(cxScreenCanvas.Font, Caption, Painter.GetCTFlags(VisibleIndex));
    cxScreenCanvas.Dormant;
  end;
  Result := FTextSize;
end;

function TcxTabViewInfo.GetTextHeight: Integer;
begin
  Result := GetTextSize.cy;
end;

function TcxTabViewInfo.GetTextWidth: Integer;
begin
  Result := GetTextSize.cx;
end;

function TcxTabViewInfo.GetVisibleIndex: Integer;
begin
  Result := TabsViewInfo.IndexOf(Self);
end;

function TcxTabViewInfo.GetVisibleRect: TRect;
begin
  if not ActuallyVisible then
    Result := cxEmptyRect
  else
    IntersectRect(Result, FullRect, ControlViewInfo.GetTabExtendedTabsRect(Self));
end;

function TcxTabViewInfo.IsImagesAssigned: Boolean;
begin
  Result := ControlViewInfo.IsTabImagesAssigned;
end;

{ TcxPCNewButtonViewInfo }

function TcxPCNewButtonViewInfo.CanFocus: Boolean;
begin
  Result := not Properties.ActivateFocusedTab and ActuallyEnabled;
end;

procedure TcxPCNewButtonViewInfo.CorrectTabNormalWidth(var AValue: Integer);
begin
// do nothing
end;

function TcxPCNewButtonViewInfo.GetDefinedWidth: Integer;
begin
  Result := Button.Width;
end;

function TcxPCNewButtonViewInfo.HasButton(AButton: TcxPCButton): Boolean;
begin
  Result := False;
end;

function TcxPCNewButtonViewInfo.HasCloseButton: Boolean;
begin
  Result := False;
end;

function TcxPCNewButtonViewInfo.IsVisibleForGoDialog: Boolean;
begin
  Result := False;
end;

function TcxPCNewButtonViewInfo.CanClick: Boolean;
begin
  Result := ActuallyEnabled;
end;

function TcxPCNewButtonViewInfo.CanDrag: Boolean;
begin
  Result := False;
end;

function TcxPCNewButtonViewInfo.CanMultiSelect: Boolean;
begin
  Result := False;
end;

function TcxPCNewButtonViewInfo.CanSelect: Boolean;
begin
  Result := False;
end;

procedure TcxPCNewButtonViewInfo.DoClick(AShift: TShiftState);
begin
  Properties.NewButtonClick;
end;

function TcxPCNewButtonViewInfo.GetButton: TcxPCNewButton;
begin
  Result := TcxPCNewButton(Tab);
end;

{ TcxTabsViewInfo }

constructor TcxTabsViewInfo.Create(ATabs: TcxTabs; AControlViewInfo: TcxCustomTabControlViewInfo);
begin
  inherited Create;
  FControlViewInfo := AControlViewInfo;
  FTabs := ATabs;
  FViewInfos := TObjectList.Create;
  CreateViewInfos;
  Tabs.ChangedHandlers.Add(TabsChangedHandler);
  Tabs.DestroyHandlers.Add(TabsDestroyHandler);
end;

destructor TcxTabsViewInfo.Destroy;
begin
  if Tabs <> nil then
  begin
    Tabs.DestroyHandlers.Remove(TabsDestroyHandler);
    Tabs.ChangedHandlers.Remove(TabsChangedHandler);
  end;
  DestroyViewInfos;
  FreeAndNil(FViewInfos);
  inherited;
end;

procedure TcxTabsViewInfo.CalculateNormalSizes;
var
  I: Integer;
begin
  FTabNormalHeight := ControlViewInfo.Painter.CalculateTabNormalHeight;
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].CalculateNormalWidth;
end;

procedure TcxTabsViewInfo.CalculateTabParts;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].CalculateParts;
end;

function TcxTabsViewInfo.GetTabDefaultHeight: Integer;
begin
  Result := ControlViewInfo.Properties.TabHeight;
end;

procedure TcxTabsViewInfo.AddViewInfo(AViewInfo: TcxTabViewInfo);
begin
  FViewInfos.Add(AViewInfo);
end;

function TcxTabsViewInfo.GetMaxMainTabIndex: Integer;
begin
  Result := ViewInfoCount - 1;
  while (Result > 0) and (ViewInfos[Result].Index < 0) do
    Dec(Result);
end;

function TcxTabsViewInfo.IndexOf(AViewInfo: TcxTabViewInfo): Integer;
begin
  Result := FViewInfos.IndexOf(AViewInfo);
end;

procedure TcxTabsViewInfo.CreateViewInfos;

  function CreateTabViewInfo(ATab: TcxTab): TcxTabViewInfo;
  begin
    Result := ATab.GetViewInfoClass.Create(ATab, Self);
  end;

var
  I: Integer;
  ATab: TcxTab;
begin
  for I := 0 to ControlViewInfo.Properties.Tabs.Count - 1 do
  begin
    ATab := ControlViewInfo.Properties.Tabs[I];
    if ATab.Visible then
      AddViewInfo(CreateTabViewInfo(ATab));
  end;
  if not ControlViewInfo.Properties.IsDesigning and ControlViewInfo.Properties.NewButton.Visible then
    AddViewInfo(CreateTabViewInfo(Tabs.NewButton));
end;

procedure TcxTabsViewInfo.DestroyViewInfos;
begin
  FViewInfos.Clear;
end;

procedure TcxTabsViewInfo.RecreateButtonViewInfos;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].RecreateButtonViewInfos;
end;

procedure TcxTabsViewInfo.RecreateViewInfos;
begin
  if ControlViewInfo.Properties.IsUpdateLocked then
    FNeedRecreateViewInfos := True
  else
    if FNeedRecreateViewInfos then
    begin
      DestroyViewInfos;
      CreateViewInfos;
      FNeedRecreateViewInfos := False;
    end;
end;

procedure TcxTabsViewInfo.ResetCachedValues;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].FTextSize := cxNullSize;
end;

procedure TcxTabsViewInfo.CheckTabIndex;
begin
  if FNeedCheckTabIndex then
  begin
    if ControlViewInfo.MainTabVisibleIndex <> -1 then
      ControlViewInfo.TabIndex := ViewInfos[ControlViewInfo.MainTabVisibleIndex].Index;
    FNeedCheckTabIndex := False;
  end;
end;

function TcxTabsViewInfo.CalculateHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ViewInfoCount - 1 do
    if ViewInfos[I].GetHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TcxTabsViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to FViewInfos.Count - 1 do
    ViewInfos[I].DoRightToLeftConversion(AClientBounds);
end;

procedure TcxTabsViewInfo.RepaintTab(ATabVisibleIndex: Integer;
  ATabPropertyChanged: TcxPCTabPropertyChanged);
begin
  ControlViewInfo.RepaintTab(ATabVisibleIndex, ATabPropertyChanged);
end;

function TcxTabsViewInfo.HandleDialogChar(Key: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ViewInfoCount - 1 do
  begin
    Result := ViewInfos[I].HandleDialogChar(Key);
    if Result then
      Break;
  end;
end;

procedure TcxTabsViewInfo.TabsChangedHandler(Sender: TObject; const AEventArgs);
var
  ATab: TcxTab;
  APropertyChanged: TcxPCTabPropertyChanged;
  AVisibleIndex: Integer;
begin
  ATab := TcxPCTabsChangedEventArgs(AEventArgs).Tab;
  APropertyChanged := TcxPCTabsChangedEventArgs(AEventArgs).PropertyChanged;

  FNeedRecreateViewInfos := FNeedRecreateViewInfos or
    (APropertyChanged in [tpcLayout, tpcVisible]);

  AVisibleIndex := ControlViewInfo.GetTabVisibleIndex(ATab);
  if (APropertyChanged = tpcVisible) and (AVisibleIndex <> -1) then
  begin
    FNeedCheckTabIndex := FNeedCheckTabIndex or
      (ATab.Properties.TabIndexTabMustBeVisible and ViewInfos[AVisibleIndex].IsMainTab);
    FViewInfos.Delete(AVisibleIndex);
  end;

  if not Tabs.Properties.IsUpdateLocked then
  begin
    if APropertyChanged in [tpcLayout, tpcVisible] then
      ControlViewInfo.IControl.RequestLayout
    else
      if AVisibleIndex <> -1 then
        RepaintTab(AVisibleIndex, APropertyChanged);
  end;
end;

procedure TcxTabsViewInfo.TabsDestroyHandler(Sender: TObject; const AEventArgs);
begin
  FTabs := nil;
  ControlViewInfo.DestroyTabs;
end;

function TcxTabsViewInfo.GetViewInfoCount: Integer;
begin
  Result := FViewInfos.Count;
end;

function TcxTabsViewInfo.GetViewInfo(Index: Integer): TcxTabViewInfo;
begin
  Result := FViewInfos[Index] as TcxTabViewInfo;
end;

{ TcxCustomTabControlViewInfo }

constructor TcxCustomTabControlViewInfo.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  Supports(Owner, IcxTabControl, FIControl);
  FNavigatorButtonInfos := TObjectList.Create;

  FFirstTabVisibleIndex := -1;
  FLastTabVisibleIndex := -1;
  FTrackingTabVisibleIndex := -1;
  FFocusedTabVisibleIndex := -1;
  FHotTrackTabVisibleIndex := -1;
  FMainTabVisibleIndex := -1;
  FPressedTabVisibleIndex := -1;

  FTabsViewInfo := Properties.Tabs.GetViewInfoClass.Create(Properties.Tabs, Self);
  FScaleFactor := dxGetScaleFactor(Owner);
end;

destructor TcxCustomTabControlViewInfo.Destroy;
begin
  DestroyTabs;
  FreeAndNil(FNavigatorButtonInfos);
  FIControl := nil;
  inherited;
end;

procedure TcxCustomTabControlViewInfo.Calculate;

  procedure SynchronizeTabImagesRotationAngle;
  var
    AHeaderImagesAngle: TcxRotationAngle;
  begin
    Properties.FImages.ImageRotationAngle := InternalGetTextRotationAngle(Self);
    if TabPosition in [tpTop, tpBottom] then
      AHeaderImagesAngle := ra0
    else
      if pcoTopToBottomText in Properties.Options then
        AHeaderImagesAngle := raMinus90
      else
        AHeaderImagesAngle := raPlus90;
    Properties.CustomButtons.FHeaderImages.ImageRotationAngle := AHeaderImagesAngle;
  end;

begin
  if IsUpdating then
    Exit;

  BeginCalculate;
  try
    DoCalculate;
    if UseRightToLeftAlignment and (TabPosition in [tpTop, tpBottom]) then
      DoRightToLeftConversion(ClientRect);
    SynchronizeTabImagesRotationAngle;
    SynchronizeHotTrackStates(KeyboardStateToShiftState);
  finally
    EndCalculate;
  end;
end;

function TcxCustomTabControlViewInfo.GetPainterClass: TcxPCPainterClass;

  function GetRealStyle: TcxPCStyleID;
  var
    AStyleID: TcxPCStyleID;
  begin
    if Properties.Style <> cxPCDefaultStyle then
      Result := Properties.Style
    else
    begin
      AStyleID := PaintersFactory.GetStyleID(GetLookAndFeel);
      if AStyleID = cxPCNoStyle then
        Result := PaintersFactory.GetDefaultStyleID(GetLookAndFeel)
      else
        Result := AStyleID;
    end;
  end;

begin
  Result := PaintersFactory.GetPainterClass(GetRealStyle);
end;

procedure TcxCustomTabControlViewInfo.CalculateHitTest(AHitTest: TcxCustomTabControlHitTest);

  function CalculateNavigatorButtonsHitTest(AHitTest: TcxCustomTabControlHitTest): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to NavigatorButtonCount - 1 do
      if NavigatorButtonInfos[I].GetHitTest(AHitTest) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if not CalculateNavigatorButtonsHitTest(AHitTest) then
    TabsViewInfo.CalculateHitTest(AHitTest);
end;

function TcxCustomTabControlViewInfo.CanFocusOnClick(X, Y: Integer): Boolean;
var
  ATabIndex: Integer;
begin
  ATabIndex := VisibleIndexOfTabAt(X, Y);
  Result := (ATabIndex <> -1) and (TabsViewInfo[ATabIndex].Index = TabIndex);
end;

procedure TcxCustomTabControlViewInfo.CalculateLongitudinalTabPositions;

  procedure InternalCalculateLongitudinalTabPositions(
    AFirstIndex, ALastIndex: Integer; ACalculateAll: Boolean = False; Row: Integer = 0); overload;
  var
    I: Integer;
    ALineStartPosition, ALineFinishPosition: Integer;
    ATabStartPosition, ATabFinishPosition, ATabWidth: Integer;
    ADistanceBetweenTabs: Integer;
    AIsY: Boolean;
    ASign: Integer;
    APos: TPoint;
  begin
    AIsY := TabPosition in [tpLeft, tpRight];
    ALineStartPosition := PointGetter(FTabsPosition.NormalTabsRect.TopLeft, AIsY);
    ASign := 1;
    if IsRightToLeftAlignment or IsBottomToTopAlignment then
    begin
      ALineFinishPosition := -ALineStartPosition;
      Inc(ALineStartPosition, FTabsPosition.NormalRowWidth - 1);
      ASign := -1;
    end
    else
      ALineFinishPosition := ALineStartPosition + FTabsPosition.NormalRowWidth - 1;
    ADistanceBetweenTabs := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);

    ATabStartPosition := ALineStartPosition;
    ATabFinishPosition := ATabStartPosition;
    for I := AFirstIndex to ALastIndex do
    begin
      FLastTabVisibleIndex := I;
      ATabWidth := TabsViewInfo[I].NormalLongitudinalSize;
      ATabFinishPosition := ATabStartPosition + (ATabWidth - 1) * ASign;
      APos := TabsViewInfo[I].NormalPosition;
      if ASign > 0 then
        PointSetter(APos, AIsY, ATabStartPosition)
      else
        PointSetter(APos, AIsY, ATabFinishPosition);
      TabsViewInfo[I].NormalPosition := APos;

      ATabStartPosition := ATabFinishPosition + (1 + ADistanceBetweenTabs) * ASign;
      if (ATabStartPosition * ASign > ALineFinishPosition) and (not ACalculateAll) then
        Break;
    end;
    FIsLastTabFullyVisible := (ATabFinishPosition * ASign <= ALineFinishPosition)
      and (FLastTabVisibleIndex = ALastIndex);
  end;

  procedure InternalCalculateLongitudinalTabPositions(const ABounds: TcxPCIndexInterval; ARow: Integer = 0); overload;
  begin
    InternalCalculateLongitudinalTabPositions(ABounds.Left, ABounds.Right, False, ARow);
  end;

  procedure SingleLineCalculateLongitudinalTabPositions;
  var
    APrevFirstTabVisibleIndex: Integer;
  begin
    APrevFirstTabVisibleIndex := FFirstTabVisibleIndex;
    FFirstTabVisibleIndex := 0;

    UpdateNavigatorButtons(True);
    UpdateTabPosition(True);
    if IsTooSmallControlSize then Exit;
    InternalCalculateLongitudinalTabPositions(FFirstTabVisibleIndex, TabsViewInfo.ViewInfoCount - 1, True);

    if not FIsLastTabFullyVisible then
    begin
      FFirstTabVisibleIndex := APrevFirstTabVisibleIndex;
      UpdateNavigatorButtons(False);
      UpdateTabPosition(True);
      if IsTooSmallControlSize then Exit;

      InternalCalculateLongitudinalTabPositions(FFirstTabVisibleIndex, TabsViewInfo.ViewInfoCount - 1);

      if FIsLastTabFullyVisible then
        while FFirstTabVisibleIndex > 0 do
        begin
          Dec(FFirstTabVisibleIndex);
          InternalCalculateLongitudinalTabPositions(FFirstTabVisibleIndex, TabsViewInfo.ViewInfoCount - 1);
          if not FIsLastTabFullyVisible then
          begin
            Inc(FFirstTabVisibleIndex);
            InternalCalculateLongitudinalTabPositions(FFirstTabVisibleIndex, TabsViewInfo.ViewInfoCount - 1);
            Break;
          end;
        end;
    end;
    UpdateButtonsState;
  end;

  procedure StretchTabWidths(const ABounds: TcxPCIndexInterval; AFreeSpace, ATotalTabsNormalWidth: Integer);
  var
    AFreeSpaceRest: Integer;
    ATabViewInfo: TcxTabViewInfo;
    ATabWidthDelta: Integer;
    I: Integer;
  begin
    AFreeSpaceRest := AFreeSpace;
    for I := ABounds.Left to ABounds.Right do
    begin
      ATabViewInfo := TabsViewInfo[I];
      if I <> ABounds.Right then
        ATabWidthDelta := ATabViewInfo.NormalWidth * AFreeSpace div ATotalTabsNormalWidth
      else
        ATabWidthDelta := AFreeSpaceRest;

      ATabViewInfo.NormalWidth := ATabViewInfo.NormalWidth + ATabWidthDelta;
      Dec(AFreeSpaceRest, ATabWidthDelta);
    end;
  end;

  procedure SingleLineNoScrollCalculateLongitudinalTabPositions;
  var
    ADistanceBetweenTabs: Integer;
    ALineFreeSpaceWidth: Integer;
    ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  begin
    UpdateNavigatorButtons(True);
    UpdateTabPosition(True);
    if IsTooSmallControlSize then Exit;
    InitializeLineBoundsA(ALineIndexBoundsA, 0, TabsViewInfo.ViewInfoCount - 1);
    ADistanceBetweenTabs := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);
    ALineFreeSpaceWidth := FTabsPosition.NormalRowWidth - GetLineWidth(ALineIndexBoundsA, 0, ADistanceBetweenTabs);
    if ActuallyRotate or (ALineFreeSpaceWidth >= 0) then
      SingleLineCalculateLongitudinalTabPositions
    else
    begin
      FFirstTabVisibleIndex := 0;
      StretchTabWidths(ALineIndexBoundsA[0], ALineFreeSpaceWidth, GetLineWidth(ALineIndexBoundsA, 0, 0));
      InternalCalculateLongitudinalTabPositions(ALineIndexBoundsA[0]);
    end;
  end;

  procedure MultiLineCalculateLongitudinalTabPositions;
  var
    ADistanceBetweenTabs: Integer;
    ALineFreeSpaceWidth, ATotalTabsNormalWidth: Integer;
    ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
    ARow: Integer;
    AToStretchTabs: Boolean;
  begin
    UpdateNavigatorButtons(True);
    UpdateTabPosition(False);
    if IsTooSmallControlSize then Exit;
    InitializeLineBoundsA(ALineIndexBoundsA, 0, TabsViewInfo.ViewInfoCount - 1);
    AToStretchTabs := not(ActuallyRotate or Properties.RaggedRight);
    ADistanceBetweenTabs := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);
    for ARow := 0 to RowCount - 1 do
    begin
      if AToStretchTabs then
      begin
        ATotalTabsNormalWidth := GetLineWidth(ALineIndexBoundsA, ARow, 0);
        ALineFreeSpaceWidth := FTabsPosition.NormalRowWidth - GetLineWidth(ALineIndexBoundsA, ARow, ADistanceBetweenTabs);
        if ALineFreeSpaceWidth > 0 then
          StretchTabWidths(ALineIndexBoundsA[ARow], ALineFreeSpaceWidth, ATotalTabsNormalWidth);
      end;
      InternalCalculateLongitudinalTabPositions(ALineIndexBoundsA[ARow], ARow);
    end;
  end;

  procedure SetLongitudinalExtendedTabsRectsBounds;
  begin
    if TabPosition in [tpTop, tpBottom] then
    begin
      FExtendedBottomOrRightTabsRect.Left := FTabsPosition.ExtendedTabsRect.Left;
      FExtendedBottomOrRightTabsRect.Right := FTabsPosition.ExtendedTabsRect.Right;
      FExtendedTopOrLeftTabsRect.Left := FTabsPosition.ExtendedTabsRect.Left;
      FExtendedTopOrLeftTabsRect.Right := FTabsPosition.ExtendedTabsRect.Right;
    end
    else
    begin
      FExtendedBottomOrRightTabsRect.Top := FTabsPosition.ExtendedTabsRect.Top;
      FExtendedBottomOrRightTabsRect.Bottom := FTabsPosition.ExtendedTabsRect.Bottom;
      FExtendedTopOrLeftTabsRect.Top := FTabsPosition.ExtendedTabsRect.Top;
      FExtendedTopOrLeftTabsRect.Bottom := FTabsPosition.ExtendedTabsRect.Bottom;
    end;
  end;

begin
  if TabsViewInfo.ViewInfoCount > 0 then
  begin
    if FRowCount > 1 (*MultiLine*) then
      MultiLineCalculateLongitudinalTabPositions
    else
      if TabsScroll then
        SingleLineCalculateLongitudinalTabPositions
      else
        SingleLineNoScrollCalculateLongitudinalTabPositions;

    if not IsTooSmallControlSize then
      SetLongitudinalExtendedTabsRectsBounds;
  end;
end;

procedure TcxCustomTabControlViewInfo.CalculateRowHeight;

  function GetMaxWidthTabVisibleIndex: Integer;
  var
    AFirstIndex, ALastIndex, AMaxTabWidth, I: Integer;
  begin
    Result := -1;
    AMaxTabWidth := 0;
    if pcoFixedTabWidthWhenRotated in Properties.Options then
    begin
      AFirstIndex := 0;
      ALastIndex := TabsViewInfo.ViewInfoCount - 1;
    end
    else
      InitializeVisibleTabRange(AFirstIndex, ALastIndex);
    for I := AFirstIndex to ALastIndex do
      if TabsViewInfo[I].NormalWidth > AMaxTabWidth then
      begin
        AMaxTabWidth := TabsViewInfo[I].NormalWidth;
        Result := I;
      end;
  end;

var
  AMaxWidthTabVisibleIndex: Integer;
  ATabViewInfo: TcxTabViewInfo;
begin
  if ActuallyRotate then
  begin
    AMaxWidthTabVisibleIndex := GetMaxWidthTabVisibleIndex;
    if AMaxWidthTabVisibleIndex <> -1 then
    begin
      ATabViewInfo := TabsViewInfo[AMaxWidthTabVisibleIndex];
      FMaxTabCaptionWidth := ATabViewInfo.TextWidth;
      FRowHeight := ATabViewInfo.NormalWidth;
    end
    else
    begin
      FMaxTabCaptionWidth := 0;
      FRowHeight := 0;
    end;
    if (ActuallyRotatedTabsMaxWidth > 0) and
      (ActuallyRotatedTabsMaxWidth >= Painter.GetMinTabNormalWidth(-1)) and
      (RowHeight > ActuallyRotatedTabsMaxWidth) then
        FRowHeight := ActuallyRotatedTabsMaxWidth;
  end
  else
    FRowHeight := TabsViewinfo.FTabNormalHeight;
end;

procedure TcxCustomTabControlViewInfo.CalculateRowPositions;
var
  ANormalTopBorder, ANormalBottomBorder: Integer;
  AExtendedTopBorder, AExtendedBottomBorder: Integer;
  ATabsDistance: Integer;
  AIsY: Boolean;

  procedure InitializeVariables;
  begin
    ATabsDistance := DistanceGetter(Painter.GetTabsNormalDistance, ActuallyRotate);
    AIsY := TabPosition in [tpTop, tpBottom];
    with FTabsPosition.NormalTabsRect do
    begin
      ANormalTopBorder := PointGetter(TopLeft, AIsY);
      ANormalBottomBorder := PointGetter(BottomRight, AIsY);
    end;
    with FTabsPosition.ExtendedTabsRect do
    begin
      AExtendedTopBorder := PointGetter(TopLeft, AIsY);
      AExtendedBottomBorder := PointGetter(BottomRight, AIsY);
    end;
  end;

  procedure SetDiametricalExtendedTabsRectsBorders;

    procedure SetInternalBorders;
    var
      ABorder: Integer;
      ABottomOrRightPartRowCount: Integer;
    begin
// ExtendedTopOrLeftTabsRectBottomOrRightBorder
      ABorder := ANormalTopBorder + FTopOrLeftPartRowCount * (RowHeight + ATabsDistance);
      Dec(ABorder, ATabsDistance);
      Inc(ABorder, FTabsPosition.ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset);

      if ABorder > AExtendedBottomBorder then
        ABorder := AExtendedBottomBorder;
      RectSetter(FExtendedTopOrLeftTabsRect, False, AIsY, ABorder);

// ExtendedBottomOrRightTabsRectTopOrLeftBorder
      ABottomOrRightPartRowCount := RowCount - FTopOrLeftPartRowCount;
      ABorder := ANormalBottomBorder - ABottomOrRightPartRowCount * (RowHeight + ATabsDistance);
      Inc(ABorder, ATabsDistance);
      Inc(ABorder, FTabsPosition.ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset);

      if ABorder < AExtendedTopBorder then
        ABorder := AExtendedTopBorder;
      RectSetter(FExtendedBottomOrRightTabsRect, True, AIsY, ABorder);
    end;

    procedure CorrectSecondaryBorder;
    var
      ASecondaryBorderBound: Integer;
    begin
      if TabPosition in [tpTop, tpLeft] then
      begin
        ASecondaryBorderBound := PointGetter(FExtendedTopOrLeftTabsRect.BottomRight, AIsY) + FTabsPosition.MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects;
        ASecondaryBorderBound := Min(ASecondaryBorderBound, AExtendedBottomBorder);
        if PointGetter(FExtendedBottomOrRightTabsRect.TopLeft, AIsY) < ASecondaryBorderBound then
          RectSetter(FExtendedBottomOrRightTabsRect, True, AIsY, ASecondaryBorderBound);
      end
      else
      begin
        ASecondaryBorderBound := PointGetter(FExtendedBottomOrRightTabsRect.TopLeft, AIsY) - FTabsPosition.MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects;
        ASecondaryBorderBound := Max(ASecondaryBorderBound, AExtendedTopBorder);
        if PointGetter(FExtendedTopOrLeftTabsRect.BottomRight, AIsY) > ASecondaryBorderBound then
          RectSetter(FExtendedTopOrLeftTabsRect, False, AIsY, ASecondaryBorderBound);
      end;
    end;

  begin
    RectSetter(FExtendedTopOrLeftTabsRect, True, AIsY, AExtendedTopBorder);
    RectSetter(FExtendedBottomOrRightTabsRect, False, AIsY, AExtendedBottomBorder);
    SetInternalBorders;
    if (FTopOrLeftPartRowCount <> 0) and (FTopOrLeftPartRowCount <> FRowCount) then
      CorrectSecondaryBorder;
  end;

var
  I: Integer;
  C: Integer;
  APos: TPoint;
begin
  InitializeVariables;
  for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
  begin
    if TabsViewInfo[I].VisibleRow < FTopOrLeftPartRowCount then
      C := ANormalTopBorder + TabsViewInfo[I].VisibleRow * (RowHeight + ATabsDistance)
    else
    begin
      C := ANormalBottomBorder - (RowCount - TabsViewInfo[I].VisibleRow) * RowHeight;
      Dec(C, (RowCount - 1 - TabsViewInfo[I].VisibleRow) * ATabsDistance);
    end;
    APos := TabsViewInfo[I].NormalPosition;
    PointSetter(APos, AIsY, C);
    TabsViewInfo[I].NormalPosition := APos;
  end;
  SetDiametricalExtendedTabsRectsBorders;
end;

procedure TcxCustomTabControlViewInfo.CalculateTabsPositions;
var
  ATabsDistance: Integer;

  function InitializeVariables: Boolean;
  begin
    UpdateNavigatorButtons(True);
    UpdateTabPosition(False);
    Result := not IsTooSmallControlSize;
    if Result then
      ATabsDistance := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);
  end;

  procedure MultiLineCalculate;
  begin
    if not InitializeVariables then Exit;

    PlaceVisibleTabsOnRows(FTabsPosition.NormalRowWidth, ATabsDistance);
    CalculateLongitudinalTabPositions;
    CalculateRowHeight;
    RearrangeRows;
  end;

  procedure NotMultiLineCalculate;

    procedure SetTabRows;
    var
      AFirstIndex, ALastIndex, I: Integer;
    begin
      InitializeVisibleTabRange(AFirstIndex, ALastIndex);
      for I := AFirstIndex to ALastIndex do
        with TabsViewInfo[I] do
        begin
          FRow := 0;
          FVisibleRow := 0;
        end;
    end;

  begin
    FRowCount := 1;
    FTopOrLeftPartRowCount := IfThen(TabPosition in [tpTop, tpLeft], 1);
    CalculateLongitudinalTabPositions;
    if not IsTooSmallControlSize then
    begin
      SetTabRows;
      CalculateRowHeight;
      CalculateRowPositions;
    end;
  end;

begin
  if MultiLine then
    MultiLineCalculate
  else
    NotMultiLineCalculate;
end;

procedure TcxCustomTabControlViewInfo.DoCalculate;
var
  ATabsDistance: Integer;
  APrevTabIndex: Integer;
  APrevMainTabVisibleIndex: Integer;

  function InitializeVariables: Boolean;
  begin
    UpdateNavigatorButtons(True);
    UpdateTabPosition(False);
    Result := not IsTooSmallControlSize;
    if Result then
      ATabsDistance := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);
  end;

  procedure ValidateTabVisibleIndex(var ATabVisibleIndex: Integer);
  begin
    if ATabVisibleIndex >= TabsViewInfo.ViewInfoCount then
      ATabVisibleIndex := -1;
  end;

  procedure ValidateTabIndex;
  begin
    if TabIndex >= Properties.Tabs.Count then
      TabIndex := Properties.Tabs.Count - 1;
  end;

  procedure ValidateTabVisibleIndexs;
  begin
    ValidateTabVisibleIndex(FMainTabVisibleIndex);
    ValidateTabVisibleIndex(FHotTrackTabVisibleIndex);
    ValidateTabVisibleIndex(FPressedTabVisibleIndex);
    if Properties.ActivateFocusedTab then
      FFocusedTabVisibleIndex := -1;
    ValidateTabVisibleIndex(FFocusedTabVisibleIndex);
    if TabsViewInfo.ViewInfoCount > 0 then
    begin
      if FFirstTabVisibleIndex = -1 then
        FFirstTabVisibleIndex := 0
      else
        FFirstTabVisibleIndex := Min(FFirstTabVisibleIndex, TabsViewInfo.ViewInfoCount - 1);
    end
    else
      FFirstTabVisibleIndex := -1;
    FLastTabVisibleIndex := Max(FLastTabVisibleIndex, FFirstTabVisibleIndex);
    ValidateTabVisibleIndex(FLastTabVisibleIndex);
    ValidateTabIndex;
  end;

  procedure ResetControlInternalVariables;
  begin
    FExtendedBottomOrRightTabsRect := cxEmptyRect;
    FExtendedTopOrLeftTabsRect := cxEmptyRect;
    FRowCount := 0;
    FTopOrLeftPartRowCount := 0;
  end;

  procedure CheckTabButtonHeight;
  begin
    FTabButtonHeight := 0;
    if HasTabButtons then
    begin
      FTabButtonHeight := Painter.GetCloseButtonSize.cy;
      if IsTabButtonImagesAssigned then
      begin
        FTabButtonHeight := Max(FTabButtonHeight,
          dxGetImageSize(Properties.TabButtonImages, ScaleFactor).cy) +
          cxMarginsHeight(Painter.GetTabButtonGlyphOffset);
      end;
    end;
  end;

  procedure CheckViewInfos;
  begin
    CheckTabButtonHeight;
    TabsViewInfo.RecreateViewInfos;
  end;

begin
  CheckViewInfos;

  APrevTabIndex := TabIndex;
  ResetControlInternalVariables;
  ValidateTabVisibleIndexs;
  APrevMainTabVisibleIndex := MainTabVisibleIndex;
  if TabsViewInfo.ViewInfoCount = 0 then
  begin
    InitializeVariables;
    TabsViewInfo.CheckTabIndex;
    Exit;
  end;
  Painter.Init;
  SetMainTab;
  TabsViewInfo.RecreateButtonViewInfos;
  TabsViewInfo.ResetCachedValues;
  TabsViewInfo.CalculateNormalSizes;
  CalculateTabsPositions;
  if (APrevTabIndex = TabIndex) and (APrevMainTabVisibleIndex <> MainTabVisibleIndex) and
      (MainTabVisibleIndex <> -1) then
    MakeTabVisible(MainTabVisibleIndex);
  TabsViewInfo.CheckTabIndex;
  TabsViewInfo.CalculateTabParts;
end;

procedure TcxCustomTabControlViewInfo.AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab;
  AImageAndTextData: TcxPCOutTabImageAndTextData);
begin
end;

function TcxCustomTabControlViewInfo.ArrowButtonClick(ANavigatorButton: TcxPCNavigatorButton): Boolean;

  function GetDirection: Integer;
  begin
    if (IsSpecialAlignment and (ANavigatorButton = nbTopLeft)) or
       ((not IsSpecialAlignment) and (ANavigatorButton = nbBottomRight)) then
      Result := 1
    else
      Result := -1;
  end;

begin
  Result := NavigatorButtonInfoByType[ANavigatorButton].State <> nbsDisabled;
  if Result then
    Inc(FFirstTabVisibleIndex, GetDirection);
end;

procedure TcxCustomTabControlViewInfo.ButtonDestroying(AElementInfo: TcxPCCustomElementViewInfo);
begin
  ElementDestroying(AElementInfo);
  if FPressedNavigatorButton = AElementInfo then
    FPressedNavigatorButton := nil;
  if FPressedTabButton = AElementInfo then
    FPressedTabButton := nil;
  if FHotTrackTabButton = AElementInfo then
    FHotTrackTabButton := nil;
  if FHotTrackNavigatorButton = AElementInfo then
    FHotTrackNavigatorButton := nil;
end;

procedure TcxCustomTabControlViewInfo.ElementDestroying(AElementInfo: TcxPCCustomElementViewInfo);
begin
  if Controller.HitTest.HitObject = AElementInfo then
    Controller.HitTest.Clear;
  if Controller.FHintHelper.HintableObject = AElementInfo then
  begin
    Controller.FHintObject := nil;
    Controller.FHintHelper.CancelHint;
  end;
end;

procedure TcxCustomTabControlViewInfo.CorrectFirstTabVisibleIndex(ATabVisibleIndex: Integer);
var
  C: Integer;
  ADistanceBetweenTabs: Integer;
begin
  if ATabVisibleIndex < FFirstTabVisibleIndex then
    FFirstTabVisibleIndex := ATabVisibleIndex
  else
    if (ATabVisibleIndex > FLastTabVisibleIndex) or
      ((ATabVisibleIndex = FLastTabVisibleIndex) and not FIsLastTabFullyVisible) then
    begin
      UpdateTabPosition(True);
      ADistanceBetweenTabs := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);
      if IsTooSmallControlSize then
        FFirstTabVisibleIndex := ATabVisibleIndex
      else
      begin
        C := TabsViewInfo[ATabVisibleIndex].NormalLongitudinalSize;
        FFirstTabVisibleIndex := ATabVisibleIndex;
        while (C + ADistanceBetweenTabs < FTabsPosition.NormalRowWidth) and (FFirstTabVisibleIndex > 0) do
        begin
          Dec(FFirstTabVisibleIndex);
          Inc(C, ADistanceBetweenTabs);
          Inc(C, TabsViewInfo[FFirstTabVisibleIndex].NormalLongitudinalSize);
          if C > FTabsPosition.NormalRowWidth then
            Inc(FFirstTabVisibleIndex);
        end;
      end;
    end;
end;

function TcxCustomTabControlViewInfo.GetActivePageColor: TColor;
begin
  Result := clNone;
end;

function TcxCustomTabControlViewInfo.GetNavigatorButtonInfoByType(
  AType: TcxPCNavigatorButton): TcxPCNavigatorButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FNavigatorButtonInfos.Count - 1 do
    if NavigatorButtonInfos[I].IsNavigatorButton([AType]) then
    begin
      Result := TcxPCNavigatorButtonViewInfo(NavigatorButtonInfos[I]);
      Break;
    end;
end;

function TcxCustomTabControlViewInfo.GetNextFocusedTabVisibleIndex(ACurrentTabVisibleIndex,
  ADelta: Integer; ACycle: Boolean; AOnlyAllowSelectedTabs: Boolean): Integer;

  function IsFocusable(AIndex: Integer): Boolean;
  var
    ATabViewInfo: TcxTabViewInfo;
  begin
    ATabViewInfo := TabsViewInfo[AIndex];
    Result := (AllowDisabledTabAccess or ATabViewInfo.ActuallyEnabled) and
      ATabViewInfo.CanFocus and (not AOnlyAllowSelectedTabs or ATabViewInfo.CanSelect);
  end;

  function GetNextIndex(var I: Integer): Boolean;
  begin
    Inc(I, ADelta);
    Result := IsTabVisibleIndexValid(I);
    if not Result and ACycle then
    begin
      if I < 0 then
        Inc(I, TabsViewInfo.ViewInfoCount)
      else
        Dec(I, TabsViewInfo.ViewInfoCount);
      Result := IsTabVisibleIndexValid(I);
    end;
  end;

var
  I: Integer;
  AVisibleTabCount: Integer;
begin
  Result := -1;
  AVisibleTabCount := TabsViewInfo.ViewInfoCount;
  if AVisibleTabCount > 1 then
  begin
    I := ACurrentTabVisibleIndex;
    while GetNextIndex(I) and not IsFocusable(I) and (I <> ACurrentTabVisibleIndex) do;
    if IsTabVisibleIndexValid(I) and IsFocusable(I) then
      Result := I;
  end;
end;

function TcxCustomTabControlViewInfo.GetTabColor(ATabVisibleIndex: Integer): TColor;
begin
  Result := TabsViewInfo[ATabVisibleIndex].Color;
end;

function TcxCustomTabControlViewInfo.GetTabExtendedTabsRect(ATabViewInfo: TcxTabViewInfo): TRect;
begin
  if ATabViewInfo.VisibleRow < FTopOrLeftPartRowCount then
    Result := FExtendedTopOrLeftTabsRect
  else
    Result := FExtendedBottomOrRightTabsRect
end;

function TcxCustomTabControlViewInfo.GetTabExtendedTabsRect(ATabVisibleIndex: Integer): TRect;
begin
  Result := GetTabExtendedTabsRect(TabsViewInfo[ATabVisibleIndex]);
end;

function TcxCustomTabControlViewInfo.GetTabImageAreaWidth: Integer;
begin
  Result := GetTabImageSize.cx + 2 * ImageBorder;
end;

function TcxCustomTabControlViewInfo.GetTabImageAreaHeight: Integer;
begin
  Result := GetTabImageSize.cy + 2 * ImageBorder;
end;

procedure TcxCustomTabControlViewInfo.InitializeLineBoundsA(var ALineIndexBoundsA:
  TcxPCLineIndexBoundsArray; AFirstIndex, ALastIndex: Integer);
var
  I, ALineIndex: Integer;
  AFirstRow, ARowCount: Integer;
begin
  AFirstRow := TabsViewInfo[AFirstIndex].FRow;
  ARowCount := TabsViewInfo[ALastIndex].FRow - AFirstRow + 1;
  SetLength(ALineIndexBoundsA, ARowCount);
  ALineIndex := 0;
  ALineIndexBoundsA[0].Left := AFirstIndex;
  for I := AFirstIndex to ALastIndex do
    if TabsViewInfo[I].FRow - AFirstRow > ALineIndex then
    begin
      ALineIndexBoundsA[ALineIndex].Right := I - 1;
      Inc(ALineIndex);
      ALineIndexBoundsA[ALineIndex].Left := I;
    end;
  ALineIndexBoundsA[ALineIndex].Right := ALastIndex;
end;

function TcxCustomTabControlViewInfo.HasActivePage: Boolean;
begin
  Result := False;
end;

function TcxCustomTabControlViewInfo.HasBorders: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTabControlViewInfo.MakeTabVisible(ATabVisibleIndex: Integer);
var
  APrevFirstTabVisibleIndex: Integer;
begin
  if MultiLine or IsTabFullVisible(ATabVisibleIndex) then
    Exit;
  APrevFirstTabVisibleIndex := FirstTabVisibleIndex;
  CorrectFirstTabVisibleIndex(ATabVisibleIndex);
  if APrevFirstTabVisibleIndex <> FirstTabVisibleIndex then
  begin
    CalculateTabsPositions;
    IControl.RequestLayout;
  end;
end;

procedure TcxCustomTabControlViewInfo.PlaceVisibleTabsOnRows(ATabsWidth, ATabsDistance: Integer);

  procedure InternalImproveTabsLayout(var ALineBoundsA: TcxPCLineIndexBoundsArray);

    function Deviation(const ALineBoundsA: TcxPCLineIndexBoundsArray; ALineIndex: Integer): Double;
    begin
      Result := Power(ATabsWidth - GetLineWidth(ALineBoundsA, ALineIndex, ATabsDistance), 2);
    end;

    function TotalDeviation(const ALineBoundsA: TcxPCLineIndexBoundsArray): Double;
    var
      I: Integer;
    begin
      Result := 0;
      for I := 0 to Length(ALineBoundsA) - 1 do
        Result := Result + Deviation(ALineBoundsA, I);
    end;

    procedure CopyBounds(var ASource, ADestination: TcxPCLineIndexBoundsArray);
    var
      I: Integer;
    begin
      if Length(ASource) <> Length(ADestination) then
        SetLength(ADestination, Length(ASource));
      for I := 0 to Length(ASource) - 1 do
        ADestination[I] := ASource[I];
    end;

    function DoBest(var ALineBoundsA: TcxPCLineIndexBoundsArray; ALineIndex: Integer; ADirection: Integer): Boolean;

      procedure ChangeLineIndex(var ALineBoundsA: TcxPCLineIndexBoundsArray; ALineIndex: Integer);
      begin
        case ADirection of
          -1:
            if ALineIndex > 0 then
            begin
              Dec(ALineBoundsA[ALineIndex].Left);
              Dec(ALineBoundsA[ALineIndex - 1].Right);
            end;
          1:
            if ALineIndex < Length(ALineBoundsA) - 1 then
            begin
              Inc(ALineBoundsA[ALineIndex].Right);
              Inc(ALineBoundsA[ALineIndex + 1].Left);
            end;
        end;
      end;

    var
      APrevError, ANewError: Double;
      ATempLineBoundsA: TcxPCLineIndexBoundsArray;
    begin
      CopyBounds(ALineBoundsA, ATempLineBoundsA);
      APrevError := Deviation(ATempLineBoundsA, ALineIndex);

      ChangeLineIndex(ATempLineBoundsA, ALineIndex);

      ANewError := Deviation(ATempLineBoundsA, ALineIndex);
      Result := (ANewError < APrevError) and (GetLineWidth(ATempLineBoundsA, ALineIndex, ATabsDistance) <= ATabsWidth);
      if Result then
        CopyBounds(ATempLineBoundsA, ALineBoundsA);
    end;

    function DoComplexBest(ACurrentError: Double; var ALineBoundsA: TcxPCLineIndexBoundsArray): Boolean;
    var
      I: Integer;
      ATempLineBoundsA: TcxPCLineIndexBoundsArray;
      APrevDeviation: Double;
    begin
      Result := False;
      CopyBounds(ALineBoundsA, ATempLineBoundsA);
      for I := Length(ATempLineBoundsA) - 1 downto 0 do
      begin
        repeat
          APrevDeviation := TotalDeviation(ATempLineBoundsA);
        until not DoBest(ATempLineBoundsA, I, -1) or (APrevDeviation < TotalDeviation(ATempLineBoundsA));

        if TotalDeviation(ATempLineBoundsA) < ACurrentError then
        begin
          Result := True;
          CopyBounds(ATempLineBoundsA, ALineBoundsA);
          Break;
        end;
      end;
    end;

    function DoSimpleBest(var ALineBoundsA: TcxPCLineIndexBoundsArray; ADirection: Integer): Boolean;
    var
      I: Integer;
      ATempLineBoundsA: TcxPCLineIndexBoundsArray;
      ACurrentError: Double;
    begin
      Result := False;
      ACurrentError := TotalDeviation(ALineBoundsA);
      for I := 0 to Length(ALineBoundsA) - 1 do
      begin
        CopyBounds(ALineBoundsA, ATempLineBoundsA);
        DoBest(ATempLineBoundsA, I, ADirection);
        if TotalDeviation(ATempLineBoundsA) < ACurrentError then
        begin
          Result := True;
          CopyBounds(ATempLineBoundsA, ALineBoundsA);
          Break;
        end;
      end;
    end;

    function DoTotalBest(var ALineBoundsA: TcxPCLineIndexBoundsArray): Boolean;
    var
      ACurrentError: Double;
    begin
      ACurrentError := TotalDeviation(ALineBoundsA);
      Result := DoComplexBest(ACurrentError, ALineBoundsA) or
        DoSimpleBest(ALineBoundsA, 1) or
        DoSimpleBest(ALineBoundsA, -1);
    end;

  begin
    while DoTotalBest(ALineBoundsA) do {loop};
  end;

  procedure AcceptImprovements(const ALineBoundsA: TcxPCLineIndexBoundsArray);
  var
    I, ARow, ACurrentRow: Integer;
  begin
    ACurrentRow := TabsViewInfo[ALineBoundsA[0].Left].FRow;
    for ARow := 0 to Length(ALineBoundsA) - 1 do
    begin
      for I := ALineBoundsA[ARow].Left to ALineBoundsA[ARow].Right do
        TabsViewInfo[I].FRow := ACurrentRow;
      Inc(ACurrentRow);
    end;
  end;

  procedure ImproveTabsLayout(AFirstIndex, ALastIndex: Integer);
  var
    ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  begin
    if ActuallyRotate and Properties.RaggedRight then Exit;
    InitializeLineBoundsA(ALineIndexBoundsA, AFirstIndex, ALastIndex);
    InternalImproveTabsLayout(ALineIndexBoundsA);
    AcceptImprovements(ALineIndexBoundsA);
  end;

var
  AFirstIndex: Integer;
  C: Integer;
  I: Integer;
  ATabViewInfo: TcxTabViewInfo;
begin
  FRowCount := 1;
  AFirstIndex := 0;
  C := 0;
  for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
  begin
    ATabViewInfo := TabsViewInfo[I];
    if ATabViewInfo.NormalLongitudinalSize >= ATabsWidth then
    begin
      if C <> 0 then
      begin
        Inc(FRowCount);
        C := 0;
      end;
      if (I > AFirstIndex{guarantees that FVisibleTabList[AFirstIndex].FRow had been set}) and
        ((FRowCount - 1) - TabsViewInfo[AFirstIndex].FRow > 1) then
        ImproveTabsLayout(AFirstIndex, I - 1);
      AFirstIndex := I + 1;
      if (not ActuallyRotate) and (ATabsWidth > Painter.GetMinTabNormalWidth(I)) then
        ATabViewInfo.NormalWidth := ATabsWidth;
      ATabViewInfo.FRow := FRowCount - 1;
      if I <> TabsViewInfo.ViewInfoCount - 1 then
        Inc(FRowCount);
    end
    else
      if C + ATabViewInfo.NormalLongitudinalSize > ATabsWidth then
      begin
        Inc(FRowCount);
        ATabViewInfo.FRow := FRowCount - 1;
        C := ATabViewInfo.NormalLongitudinalSize + ATabsDistance;
      end
      else
      begin
        ATabViewInfo.FRow := FRowCount - 1;
        Inc(C, ATabViewInfo.NormalLongitudinalSize + ATabsDistance);
      end;
  end;
  if (TabsViewInfo.ViewInfoCount - 1 > AFirstIndex) and ((FRowCount - 1) - TabsViewInfo[AFirstIndex].FRow > 0) then
    ImproveTabsLayout(AFirstIndex, TabsViewInfo.ViewInfoCount - 1);
end;

procedure TcxCustomTabControlViewInfo.RepaintTab(ATabVisibleIndex: Integer; ATabPropertyChanged: TcxPCTabPropertyChanged);
begin
  Painter.RepaintTab(ATabVisibleIndex, ATabPropertyChanged);
end;

procedure TcxCustomTabControlViewInfo.SetMainTab;
var
  AVisibleIndex: Integer;
begin
  if TabIndex = -1 then
    MainTabVisibleIndex := -1
  else
  begin
    AVisibleIndex := GetTabVisibleIndex(Properties.Tabs[TabIndex]);
    if (AVisibleIndex = -1) then
    begin
      if FMainTabVisibleIndex > TabsViewInfo.GetMaxMainTabIndex then
        MainTabVisibleIndex := TabsViewInfo.GetMaxMainTabIndex;
    end
    else
      MainTabVisibleIndex := AVisibleIndex;
  end;
end;

procedure TcxCustomTabControlViewInfo.TabDestroying(ATabViewInfo: TcxTabViewInfo);
begin
  if Controller.HitTest.HitTab = ATabViewInfo then
    Controller.HitTest.Clear;
  ElementDestroying(ATabViewInfo);
end;

procedure TcxCustomTabControlViewInfo.TabDown(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  if ATabVisibleIndex <> -1 then
  begin
    if TabsViewInfo[ATabVisibleIndex].IsPressable and not IControl.IsDesigning then
      PressedTabVisibleIndex := ATabVisibleIndex
    else
      TabsViewInfo[ATabVisibleIndex].Select(ssCtrl in AShift);
  end;
end;

procedure TcxCustomTabControlViewInfo.TabUp(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  if (ATabVisibleIndex <> -1) and
    TabsViewInfo[ATabVisibleIndex].IsPressable and (ATabVisibleIndex = FPressedTabVisibleIndex) then
    PressedTabVisibleIndex := -1;
end;

function TcxCustomTabControlViewInfo.UseActivePageColor: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTabControlViewInfo.DestroyTabs;
begin
  FreeAndNil(FTabsViewInfo);
end;

function TcxCustomTabControlViewInfo.ActuallyRotatedTabsMaxWidth: Integer;
begin
  Result := Properties.RotatedTabsMaxWidth;
  if Result > 0 then
    Result := Max(Result, Painter.GetMinTabNormalWidth(-1));
end;

function TcxCustomTabControlViewInfo.ActuallyRotate: Boolean;
begin
  Result := Properties.Rotate and Painter.AllowRotate;
end;

function TcxCustomTabControlViewInfo.CanDrawParentBackground: Boolean;
begin
  Result := IControl.CanDrawParentBackground;
end;

function TcxCustomTabControlViewInfo.GetHeaderButtonsDistance(
  AButtonInfo1, AButtonInfo2: TcxPCCustomHeaderButtonViewInfo): Integer;

  function GetButtonTypeForDistance(AButtonInfo: TcxPCCustomHeaderButtonViewInfo): TcxPCNavigatorButton;
  begin
    if AButtonInfo.GetHitTestIndex = pchtNavigatorButton then
      Result := (AButtonInfo as TcxPCNavigatorButtonViewInfo).ButtonType
    else
      Result := nbGoDialog;
  end;

begin
  Result := Painter.GetButtonsDistance(GetButtonTypeForDistance(AButtonInfo1), GetButtonTypeForDistance(AButtonInfo2));
end;

function TcxCustomTabControlViewInfo.GetPopupOwner: TComponent;
var
  AComponentRefrence: IInterfaceComponentReference;
begin
  if Supports(IControl, IInterfaceComponentReference, AComponentRefrence) and
    (AComponentRefrence.GetComponent is TWinControl)
  then
    Result := AComponentRefrence.GetComponent
  else
    Result := nil;
end;

function TcxCustomTabControlViewInfo.GetTabImageSize: TSize;
begin
  Result := Properties.FImages.BaseImageSize;
end;

function TcxCustomTabControlViewInfo.AllowHotTrack: Boolean;
begin
  Result := Properties.HotTrack or Painter.IsEnableHotTrack;
end;

function TcxCustomTabControlViewInfo.AllowMultiSelect: Boolean;
begin
  Result := Properties.MultiSelect and Painter.IsMultiSelectionAccepted;
end;

function TcxCustomTabControlViewInfo.AllowDisabledTabAccess: Boolean;
begin
  Result := Properties.AllowDisabledTabAccess;
end;

function TcxCustomTabControlViewInfo.CanMouseWheel(const AMousePos: TPoint): Boolean;
var
  R: TRect;
begin
  Result := False;
  if not MultiLine and (FFirstTabVisibleIndex >= 0) then
  begin
    R := TabsViewInfo[FFirstTabVisibleIndex].FullRect;
    Result := (TabPosition in [tpTop, tpBottom]) and (AMousePos.Y >= R.Top) and (AMousePos.Y < R.Bottom) or
      (TabPosition in [tpLeft, tpRight]) and (AMousePos.X >= R.Left) and (AMousePos.X < R.Right);
  end;
end;

function TcxCustomTabControlViewInfo.HasNavigatorButton(AType: TcxPCNavigatorButton): Boolean;
begin
  Result := NavigatorButtonInfoByType[AType] <> nil;
end;

function TcxCustomTabControlViewInfo.IsEnabled: Boolean;
begin
  Result := IControl.IsEnabled;
end;

function TcxCustomTabControlViewInfo.IsFocused: Boolean;
begin
  Result := IControl.IsFocused;
end;

function TcxCustomTabControlViewInfo.PtInTab(ATabVisibleIndex: Integer; X, Y: Integer): Boolean;
begin
  Result := Painter.PtInTab(ATabVisibleIndex, X, Y);
end;

function TcxCustomTabControlViewInfo.IsHeaderButtonImagesAssigned: Boolean;
begin
  Result := Properties.CustomButtons.FHeaderImages.IsImagesAssigned;
end;

function TcxCustomTabControlViewInfo.IsTabFullVisible(ATabVisibleIndex: Integer): Boolean;
begin
  with TabsViewInfo[ATabVisibleIndex] do
    Result := cxRectIsEqual(FullRect, VisibleRect);
end;

function TcxCustomTabControlViewInfo.IsSpecialAlignment: Boolean;
begin
  Result := IsRightToLeftAlignment or IsBottomToTopAlignment;
end;

function TcxCustomTabControlViewInfo.IsTooSmallControlSize: Boolean;
begin
  Result := FTabsPosition.NormalRowWidth <= 0;
end;

function TcxCustomTabControlViewInfo.IsTabActuallyVisible(ATabViewInfo: TcxTabViewInfo): Boolean;
begin
  Result := (Properties.MultiLine or
    ((ATabViewInfo.VisibleIndex >= FFirstTabVisibleIndex) and
    (ATabViewInfo.VisibleIndex <= FLastTabVisibleIndex)));
end;

function TcxCustomTabControlViewInfo.IsTabAccessible(AIndex: Integer): Boolean;
begin
  Result := IsTabVisibleIndexValid(AIndex) and
    (TabsViewInfo[AIndex].ActuallyEnabled or AllowDisabledTabAccess or IControl.IsDesigning);
end;

function TcxCustomTabControlViewInfo.IsTabButtonImagesAssigned: Boolean;
begin
  Result := Properties.CustomButtons.FTabImages.IsImagesAssigned;
end;

function TcxCustomTabControlViewInfo.IsTabImagesAssigned: Boolean;
begin
  Result := Properties.FImages.IsImagesAssigned;
end;

function TcxCustomTabControlViewInfo.IsTabVisibleIndexValid(AIndex: Integer): Boolean;
begin
  Result := InRange(AIndex, 0, TabsViewInfo.ViewInfoCount - 1);
end;

function TcxCustomTabControlViewInfo.PtInScrollingArea(const P: TPoint; var ADirection: Integer): Boolean;
var
  ATabVisibleIndex: Integer;
begin
  ATabVisibleIndex := VisibleIndexOfTabAt(P.X, P.Y);
  Result := (ATabVisibleIndex > -1);
  if Result then
  begin
    ADirection := 0;
    if (ATabVisibleIndex = FirstTabVisibleIndex) and (FirstTabVisibleIndex > 0) then
      ADirection := -1;
    if (ATabVisibleIndex = LastTabVisibleIndex) and
        ((LastTabVisibleIndex < TabsViewInfo.ViewInfoCount - 1) or not FIsLastTabFullyVisible) then
      ADirection := 1;
    Result := (ADirection <> 0) and PtInRect(TabsViewInfo[ATabVisibleIndex].GetScrollingArea, P);
  end;
end;

function TcxCustomTabControlViewInfo.GetOptimalSize: Integer;
var
  ADistanceBetweenTabs, ATabFinishPosition, ATabStartPosition, ATabWidth, I: Integer;
  AIsY: Boolean;
  ALineFinishPosition, ALineStartPosition: Integer;
begin
  Result := 0;
  if TabsViewInfo.ViewInfoCount = 0 then
    Exit;
  TabsViewInfo.CalculateNormalSizes;
  UpdateNavigatorButtons(True);
  UpdateTabPosition(True);

  AIsY := TabPosition in [tpLeft, tpRight];
  ALineStartPosition := PointGetter(FTabsPosition.NormalTabsRect.TopLeft, AIsY);
  ALineFinishPosition := ALineStartPosition + FTabsPosition.NormalRowWidth - 1;
  ADistanceBetweenTabs := DistanceGetter(Painter.GetTabsNormalDistance, not ActuallyRotate);

  ATabStartPosition := ALineStartPosition;
  ATabFinishPosition := ATabStartPosition;
  for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
  begin
    ATabWidth := TabsViewInfo[I].NormalLongitudinalSize;
    ATabFinishPosition := ATabStartPosition + ATabWidth - 1;
    ATabStartPosition := ATabFinishPosition + 1 + ADistanceBetweenTabs;
  end;
  FIsLastTabFullyVisible := (ATabFinishPosition <= ALineFinishPosition);

  if TabPosition in [tpTop, tpBottom] then
    Result := ATabFinishPosition + 1 + (Width - 1 - ALineFinishPosition)
  else
    Result := ATabFinishPosition + 1 + (Height - 1 - ALineFinishPosition);
end;

function TcxCustomTabControlViewInfo.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := IControl.GetLookAndFeel;
end;

function TcxCustomTabControlViewInfo.GetTextRotationAngle: TcxRotationAngle;
begin
  if IsVerticalText then
    if pcoTopToBottomText in Properties.Options then
      Result := raMinus90
    else
      Result := raPlus90
  else
    Result := ra0;
end;

function TcxCustomTabControlViewInfo.GetSizeDeficit: Integer;
begin
  Result := -FTabsPosition.NormalRowWidth + 1;
end;

function TcxCustomTabControlViewInfo.HasTabCloseButtons: Boolean;
begin
  Result := Properties.CloseButtonMode <> cbmNone;
end;

function TcxCustomTabControlViewInfo.HasTabButtons: Boolean;
begin
  Result := HasTabCloseButtons or (Properties.CustomButtons.Mode <> cbmNone);
end;

function TcxCustomTabControlViewInfo.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result := VisibleIndexOfTabAt(X, Y);
  if Result <> -1 then
    Result := TabsViewInfo[Result].Index;
end;

procedure TcxCustomTabControlViewInfo.InitializeLineBoundsArray(var ALineIndexBoundsA: TcxPCLineIndexBoundsArray);
var
  ALineCount, I: Integer;
  AFirstIndex, ALastIndex: Integer;
begin
  SetLength(ALineIndexBoundsA, RowCount);
  if RowCount = 0 then
    Exit;
  for ALineCount := 0 to RowCount - 1 do
    ALineIndexBoundsA[ALineCount].Left := -1;
  InitializeVisibleTabRange(AFirstIndex, ALastIndex);
  for I := AFirstIndex to ALastIndex do
    with ALineIndexBoundsA[TabsViewInfo[I].VisibleRow] do
      if Left = -1 then
      begin
        Left := I;
        Right := I;
      end
      else
      begin
        if I < Left then
          Left := I;
        if I > Right then
          Right := I;
      end;
end;

procedure TcxCustomTabControlViewInfo.InitializeVisibleTabRange(var AFirstIndex, ALastIndex: Integer);
begin
  if MultiLine then
  begin
    AFirstIndex := 0;
    ALastIndex := TabsViewInfo.ViewInfoCount - 1;
  end
  else
  begin
    AFirstIndex := FFirstTabVisibleIndex;
    if FFirstTabVisibleIndex = -1 then
      ALastIndex := -2
    else
      ALastIndex := FLastTabVisibleIndex;
  end;
end;

procedure TcxCustomTabControlViewInfo.InvalidateRect(const R: TRect; EraseBackground: Boolean);
begin
  IControl.InvalidateRect(R, EraseBackground);
end;

function TcxCustomTabControlViewInfo.IsBottomToTopAlignment: Boolean;
begin
  Result := (TabPosition in [tpLeft, tpRight]) and (not ActuallyRotate) and
    not(pcoTopToBottomText in Properties.Options);
end;

function TcxCustomTabControlViewInfo.IsCustomTextColorAssigned: Boolean;
begin
  Result := FIsCustomTextColorAssigned;
end;

function TcxCustomTabControlViewInfo.IsNativePainting: Boolean;
begin
  Result := Painter.IsNativePainting;
end;

function TcxCustomTabControlViewInfo.IsRightToLeftAlignment: Boolean;
begin
  Result := (TabPosition in [tpTop, tpBottom]) and ActuallyRotate and
    (pcoTopToBottomText in Properties.Options);
end;

function TcxCustomTabControlViewInfo.IsTabsContainer: Boolean;
begin
  Result := Properties.IsTabsContainer;
end;

function TcxCustomTabControlViewInfo.IsTabsOnBothSides: Boolean;
begin
  Result := (TabPosition in [tpTop, tpLeft]) and (TopOrLeftPartRowCount <> RowCount) or
    (TabPosition in [tpBottom, tpRight]) and (TopOrLeftPartRowCount <> 0);
end;

function TcxCustomTabControlViewInfo.IsTabsVisible: Boolean;
begin
  Result := not HideTabs and (RowCount > 0);
end;

function TcxCustomTabControlViewInfo.IsTransparent: Boolean;
begin
  Result := False;
end;

function TcxCustomTabControlViewInfo.IsVerticalText: Boolean;
begin
  Result := (TabPosition in [tpLeft, tpRight]) and not ActuallyRotate or
    (TabPosition in [tpTop, tpBottom]) and ActuallyRotate;
end;

function TcxCustomTabControlViewInfo.ParentBackground: Boolean;
begin
  Result := IControl.IsParentBackground;
end;

procedure TcxCustomTabControlViewInfo.PrepareTabCanvasFont(ATabViewInfo: TcxTabViewInfo; ACanvas: TcxCanvas);
var
  AColor: TColor;
begin
  ACanvas.Lock;
  try
    ACanvas.Font := IControl.GetFont;
    ACanvas.Font.Color := Painter.GetTextColor(ATabViewInfo.VisibleIndex);
    AColor := ACanvas.Font.Color;
    Properties.DoDrawTabEx(ATabViewInfo.VisibleIndex, ACanvas.Font);
    FIsCustomTextColorAssigned := AColor <> ACanvas.Font.Color;
    Properties.DoPrepareTabCanvasFont(ATabViewInfo.Tab, ACanvas);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxCustomTabControlViewInfo.VisibleIndexOfTabAt(X, Y: Integer): Integer;
var
  AFirstIndex, ALastIndex, I: Integer;
begin
  Result := -1;
  if TabsViewInfo.ViewInfoCount = 0 then
    Exit;
  InitializeVisibleTabRange(AFirstIndex, ALastIndex);
  for I := AFirstIndex to ALastIndex do
    with TabsViewInfo[I] do
      if PtInRect(VisibleRect, Point(X, Y)) and PtInTab(I, X, Y) then
      begin
        Result := I;
        Break;
      end;
end;

function TcxCustomTabControlViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := IControl.GetControl.UseRightToLeftAlignment;
end;

function TcxCustomTabControlViewInfo.UseRightToLeftReading: Boolean;
begin
  Result := IControl.GetControl.UseRightToLeftReading;
end;

function TcxCustomTabControlViewInfo.CanPressButton(AButton: TcxPCNavigatorButton): Boolean;
begin
  Result := True;
  case AButton of
    nbTopLeft, nbBottomRight:
      if IsSpecialAlignment and (AButton = nbTopLeft) or not IsSpecialAlignment and (AButton = nbBottomRight) then
        Result := (FLastTabVisibleIndex < TabsViewInfo.ViewInfoCount - 1) or
          not FIsLastTabFullyVisible and (FLastTabVisibleIndex <> FFirstTabVisibleIndex)
      else
        Result := FFirstTabVisibleIndex > 0;
    nbClose:
      Result := FMainTabVisibleIndex >= 0;
  end;
end;

function TcxCustomTabControlViewInfo.GetBoundsRect: TRect;
begin
  Result := IControl.GetBoundsRect;
end;

function TcxCustomTabControlViewInfo.GetCanvas: TcxCanvas;
begin
  Result := IControl.GetCanvas;
end;

function TcxCustomTabControlViewInfo.GetClientRect: TRect;
begin
  Result := cxRectOffset(BoundsRect, BoundsRect.TopLeft, False);
end;

function TcxCustomTabControlViewInfo.GetControlBounds: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TcxCustomTabControlViewInfo.GetController: TcxCustomTabControlController;
begin
  Result := IControl.Controller;
end;

function TcxCustomTabControlViewInfo.GetColor: TColor;
begin
  Result := IControl.GetColor;
  if Result = clNone then
    Result := clBtnFace;
end;

function TcxCustomTabControlViewInfo.GetFocusedTabVisibleIndex: Integer;
begin
  if (FFocusedTabVisibleIndex = -1) or Properties.ActivateFocusedTab then
    Result := MainTabVisibleIndex
  else
    Result := FFocusedTabVisibleIndex;
end;

function TcxCustomTabControlViewInfo.GetFont: TFont;
begin
  Result := IControl.GetFont;
end;

function TcxCustomTabControlViewInfo.GetHideTabs: Boolean;
begin
  Result := Properties.HideTabs;
end;

function TcxCustomTabControlViewInfo.GetHeight: Integer;
begin
  Result := cxRectHeight(BoundsRect);
end;

function TcxCustomTabControlViewInfo.GetImageBorder: Integer;
begin
  Result := Properties.ImageBorder;
end;

function TcxCustomTabControlViewInfo.GetLineWidth(const ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  ALineNumber, ATabsDistance: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  with ALineIndexBoundsA[ALineNumber] do
  begin
    for I := Left to Right do
      Inc(Result, TabsViewInfo[I].NormalLongitudinalSize);
    Inc(Result, (Right - Left) * ATabsDistance);
  end;
end;

function TcxCustomTabControlViewInfo.GetMainTabIndex: Integer;
begin
  Result := -1;
  if FMainTabVisibleIndex <> -1 then
    Result := TabsViewInfo[FMainTabVisibleIndex].Index;
end;

function TcxCustomTabControlViewInfo.GetMultiLine: Boolean;
begin
  Result := Properties.MultiLine;
end;

function TcxCustomTabControlViewInfo.GetMultiLineTabCaptions: Boolean;
begin
  Result := Properties.MultiLineTabCaptions;
end;

function TcxCustomTabControlViewInfo.GetNavigatorButtonCount: Integer;
begin
  Result := FNavigatorButtonInfos.Count;
end;

function TcxCustomTabControlViewInfo.GetNavigatorButtonInfos(
  Index: Integer): TcxPCCustomHeaderButtonViewInfo;
begin
  Result := TcxPCCustomHeaderButtonViewInfo(FNavigatorButtonInfos[Index]);
end;

function TcxCustomTabControlViewInfo.GetNavigatorButtons: TcxPCNavigatorButtons;
begin
  Result := FNavigatorButtons;
end;

function TcxCustomTabControlViewInfo.GetNavigatorPosition: TcxPCNavigatorPosition;
begin
  Result := Properties.NavigatorPosition;
end;

function TcxCustomTabControlViewInfo.GetOptions: TcxPCOptions;
begin
  Result := Properties.Options;
end;

function TcxCustomTabControlViewInfo.GetPainter: TcxPCCustomPainter;
begin
  Result := IControl.Painter;
end;

function TcxCustomTabControlViewInfo.GetProperties: TcxCustomTabControlProperties;
begin
  Result := IControl.Properties;
end;

function TcxCustomTabControlViewInfo.GetPageClientRect: TRect;
begin
  Result := Painter.GetPageClientRect;
end;

function TcxCustomTabControlViewInfo.GetPageClientRectOffset: TRect;
begin
  Result := RotateRect(Painter.GetPageClientRectOffset, TabPosition);
end;

function TcxCustomTabControlViewInfo.GetPageFrameRect: TRect;
begin
  Result := Painter.GetPageFrameRect;
end;

function TcxCustomTabControlViewInfo.GetPageFrameRectOffset: TRect;
begin
  Result := RotateRect(Painter.GetPageFrameRectOffset, TabPosition);
end;

function TcxCustomTabControlViewInfo.GetRaggedRight: Boolean;
begin
  Result := Properties.RaggedRight;
end;

function TcxCustomTabControlViewInfo.GetShowFrame: Boolean;
begin
  Result := Properties.ShowFrame;
end;

function TcxCustomTabControlViewInfo.GetTabsAreaRect: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTop:
      Result.Bottom := PageFrameRect.Top;
    tpBottom:
      Result.Top := PageFrameRect.Bottom;
    tpLeft:
      Result.Right := PageFrameRect.Left;
    tpRight:
      Result.Left := PageFrameRect.Right;
  end;
end;

function TcxCustomTabControlViewInfo.GetTabHeight: Smallint;
begin
  Result := Properties.TabHeight;
end;

function TcxCustomTabControlViewInfo.DoGetTabIndex: Integer;
begin
  Result := Properties.TabIndex;
end;

procedure TcxCustomTabControlViewInfo.DoSetTabIndex(Value: Integer);
begin
  Properties.TabIndex := Value;
end;

function TcxCustomTabControlViewInfo.GetTabIndex: Integer;
begin
  if IsUpdating then
    Result := FTabCalculatingIndex
  else
    Result := DoGetTabIndex;
end;

function TcxCustomTabControlViewInfo.GetTabPosition: TcxTabPosition;
begin
  Result := Properties.TabPosition;
  if UseRightToLeftAlignment then
    Result := cxPCGetRightToLeftTabPosition(Result);
end;

function TcxCustomTabControlViewInfo.GetTabsScroll: Boolean;
begin
  Result := Properties.TabsScroll;
end;

function TcxCustomTabControlViewInfo.GetTabSlantPositions: TcxTabSlantPositions;
begin
  Result := Properties.TabSlants.Positions;
  if UseRightToLeftAlignment and (TabPosition in [tpTop, tpBottom]) then
    if Result = [spLeft] then
      Result := [spRight]
    else
      if Result = [spRight] then
        Result := [spLeft];
end;

function TcxCustomTabControlViewInfo.GetTabSlants: TcxTabSlants;
begin
  Result := Properties.TabSlants;
end;

function TcxCustomTabControlViewInfo.GetTabWidth: Smallint;
begin
  Result := Properties.TabWidth;
end;

function TcxCustomTabControlViewInfo.GetWidth: Integer;
begin
  Result := cxRectWidth(BoundsRect);
end;

function TcxCustomTabControlViewInfo.IsInverseNavigatorButtonsOrder: Boolean;
var
  AIsVertical: Boolean;
begin
  AIsVertical := TabPosition in [tpLeft, tpRight];
  Result := (AIsVertical and (NavigatorPosition in [npLeftTop, npRightTop])) or
    (not AIsVertical and (NavigatorPosition in [npLeftTop, npLeftBottom]));
end;

procedure TcxCustomTabControlViewInfo.RearrangeRows;

  function IsRowNumbersCorrectionNeeded: Boolean;
  begin
    if TabPosition in [tpBottom, tpTop] then
    begin
      Result := ActuallyRotate and not(pcoTopToBottomText in Properties.Options);
      if TabPosition = tpBottom then
        Result := not Result;
    end
    else
    begin
      Result := (not ActuallyRotate) and (pcoTopToBottomText in Properties.Options);
      if TabPosition = tpRight then
        Result := not Result;
    end;
  end;

  // tpTop: top to bottom
  // tpLeft: left to right
  // tpRight: right to left
  // tpBottom: bottom to top
  procedure ConvertRowNumbersToNumbersRelativeToTabPosition;
  var
    I: Integer;
  begin
    if IsRowNumbersCorrectionNeeded then
      for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
        with TabsViewInfo[I] do
          FVisibleRow := RowCount - 1 - FRow
    else
      for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
        with TabsViewInfo[I] do
          FVisibleRow := FRow;
  end;

  procedure ConvertRelativeNumbersToConvenientNumbers;
  var
    I: Integer;
  begin
    if TabPosition in [tpRight, tpBottom] then
    begin
      for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
        with TabsViewInfo[I] do
          FVisibleRow := RowCount - 1 - FVisibleRow;
      FTopOrLeftPartRowCount := FRowCount - FTopOrLeftPartRowCount;
    end
  end;

var
  I: Integer;
  ARow: Integer;
  ARelativeTopPartRowCount: Integer;
begin
  ConvertRowNumbersToNumbersRelativeToTabPosition;
  ARelativeTopPartRowCount := RowCount;

  if FMainTabVisibleIndex <> -1 then
    if Properties.ScrollOpposite then
      ARelativeTopPartRowCount := TabsViewInfo[FMainTabVisibleIndex].FVisibleRow + 1
    else
      if Painter.IsMainTabBoundWithClient then
      begin
        ARow := RowCount - 1 - TabsViewInfo[FMainTabVisibleIndex].FVisibleRow;
        for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
          with TabsViewInfo[I] do
            if RowCount = 0 then
              FVisibleRow := 0
            else
              FVisibleRow := (FVisibleRow + ARow) mod RowCount;
      end;

  FTopOrLeftPartRowCount := ARelativeTopPartRowCount;
  ConvertRelativeNumbersToConvenientNumbers;
  CalculateRowPositions;
end;

procedure TcxCustomTabControlViewInfo.ResetHotTrack;
begin
  HotTrackTabVisibleIndex := -1;
  HotTrackTabButton := nil;
  HotTrackNavigatorButton := nil;
end;

procedure TcxCustomTabControlViewInfo.SetFocusedTabVisibleIndex(Value: Integer);
begin
  if FocusedTabVisibleIndex <> Value then
  begin
    if IsTabVisibleIndexValid(FocusedTabVisibleIndex) then
      RepaintTab(FocusedTabVisibleIndex, tpcFocused);
    FFocusedTabVisibleIndex := Value;
    if IsTabVisibleIndexValid(FFocusedTabVisibleIndex) then
    begin
      if IsTabFullVisible(FFocusedTabVisibleIndex) then
        RepaintTab(FFocusedTabVisibleIndex, tpcFocused)
      else
        MakeTabVisible(FFocusedTabVisibleIndex);
    end;
  end;
end;

procedure TcxCustomTabControlViewInfo.SetHotTrackTabButton(
  const Value: TcxPCCustomTabButtonViewInfo);
begin
  if FHotTrackTabButton <> Value then
  begin
    if FHotTrackTabButton <> nil then
      FHotTrackTabButton.State := nbsNormal;
    FHotTrackTabButton := Value;
    if FHotTrackTabButton <> nil then
      if (FHotTrackTabButton = PressedTabButton) then
        FHotTrackTabButton.State := nbsPressed
      else
        if AllowHotTrack then
          FHotTrackTabButton.State := nbsHotTrack
        else
          FHotTrackTabButton.State := nbsNormal;
  end;
end;

procedure TcxCustomTabControlViewInfo.SetHotTrackNavigatorButton(
  const Value: TcxPCCustomHeaderButtonViewInfo);
begin
  if FHotTrackNavigatorButton <> Value then
  begin
    if HotTrackNavigatorButton <> nil then
      HotTrackNavigatorButton.State := nbsNormal;
    FHotTrackNavigatorButton := Value;
    if FHotTrackNavigatorButton <> nil then
      if (FHotTrackNavigatorButton = PressedNavigatorButton) then
        FHotTrackNavigatorButton.State := nbsPressed
      else
        if AllowHotTrack then
          FHotTrackNavigatorButton.State := nbsHotTrack
        else
          FHotTrackNavigatorButton.State := nbsNormal
  end;
end;

procedure TcxCustomTabControlViewInfo.SetHotTrackTabVisibleIndex(Value: Integer);

   procedure UpdateTabLayout(ATabIndex: Integer);
   begin
      TabsViewInfo[ATabIndex].RecreateButtonViewInfos;
      TabsViewInfo[ATabIndex].CalculateParts;
      if UseRightToLeftAlignment and (TabPosition in [tpTop, tpBottom])  then
        TabsViewInfo[ATabIndex].DoRightToLeftContentConversion(TabsViewInfo[ATabIndex].FContentRect);
      RepaintTab(ATabIndex, tpcHotTrack);
   end;

var
  APrevHotTrackTabVisibleIndex: Integer;
begin
  if HotTrackTabVisibleIndex <> Value then
  begin
    APrevHotTrackTabVisibleIndex := FHotTrackTabVisibleIndex;
    FHotTrackTabVisibleIndex := Value;
    if IsTabVisibleIndexValid(APrevHotTrackTabVisibleIndex) then
      UpdateTabLayout(APrevHotTrackTabVisibleIndex);
    if IsTabVisibleIndexValid(FHotTrackTabVisibleIndex) then
      UpdateTabLayout(FHotTrackTabVisibleIndex);
  end;
end;

procedure TcxCustomTabControlViewInfo.SetMainTabVisibleIndex(Value: Integer);
begin
  if FMainTabVisibleIndex <> Value then
  begin
    TrackingTabVisibleIndex := -1;
    FocusedTabVisibleIndex := -1;
    if IsTabVisibleIndexValid(FMainTabVisibleIndex) then
      RepaintTab(FMainTabVisibleIndex, tpcIsMainTab);
    FMainTabVisibleIndex := Value;
    if FMainTabVisibleIndex <> -1 then
      RepaintTab(FMainTabVisibleIndex, tpcIsMainTab);
    UpdateButtonsState;
  end;
end;

procedure TcxCustomTabControlViewInfo.CreateHeaderButtons;

  procedure AddCustomButtonViewInfos;
  var
    AButton: TcxPCButton;
    I: Integer;
  begin
    for I := 0 to Properties.CustomButtons.Count - 1 do
    begin
      AButton := Properties.CustomButtons.Buttons[I];
      if AButton.Visible and (AButton.Position in [pcbpHeader, pcbpTabsAndHeader]) then
        FNavigatorButtonInfos.Add(TcxPCHeaderButtonViewInfo.Create(Self, AButton));
    end;

    if IsHeaderButtonImagesAssigned then
    begin
      FHeaderButtonHeight := Max(FHeaderButtonHeight,
        dxGetImageSize(Properties.HeaderButtonImages, ScaleFactor).cy +
        cxMarginsHeight(Painter.GetHeaderButtonGlyphOffset));
    end;
  end;

  procedure AddNavigatorButtonViewInfos(AInverseOrder: Boolean);
  const
    ButtonsOrder: array [Boolean, TcxPCNavigatorButton] of TcxPCNavigatorButton = (
      (nbGoDialog, nbTopLeft, nbBottomRight, nbClose), (nbClose, nbTopLeft, nbBottomRight, nbGoDialog)
    );
  var
    I, ANextButton: TcxPCNavigatorButton;
  begin
    for I := Low(TcxPCNavigatorButton) to High(TcxPCNavigatorButton) do
    begin
      ANextButton := ButtonsOrder[AInverseOrder, I];
      if ANextButton in FNavigatorButtons then
        FNavigatorButtonInfos.Add(TcxPCNavigatorButtonViewInfo.Create(Self, ANextButton));
    end;
  end;

var
  AInverseOrder: Boolean;
begin
  AInverseOrder := IsInverseNavigatorButtonsOrder;
  if not AInverseOrder then
    AddCustomButtonViewInfos;
  AddNavigatorButtonViewInfos(AInverseOrder);
  if AInverseOrder then
    AddCustomButtonViewInfos;
  UpdateButtonsState;
end;

procedure TcxCustomTabControlViewInfo.SetPressedNavigatorButton(
  const Value: TcxPCCustomHeaderButtonViewInfo);
begin
  if FPressedNavigatorButton <> Value then
  begin
    if FPressedNavigatorButton <> nil then
      if AllowHotTrack and (FPressedNavigatorButton = FHotTrackNavigatorButton) then
        FPressedNavigatorButton.State := nbsHotTrack
      else
        FPressedNavigatorButton.State := nbsNormal;
    FPressedNavigatorButton := Value;
    if FPressedNavigatorButton <> nil then
      FPressedNavigatorButton.State := nbsPressed;
  end;
end;

procedure TcxCustomTabControlViewInfo.SetPressedTabButton(
  const Value: TcxPCCustomTabButtonViewInfo);
begin
  if FPressedTabButton <> Value then
  begin
    if FPressedTabButton <> nil then
      if AllowHotTrack and (FPressedTabButton = FHotTrackTabButton) then
        FPressedTabButton.State := nbsHotTrack
      else
        FPressedTabButton.State := nbsNormal;
    FPressedTabButton := Value;
    if FPressedTabButton <> nil then
      FPressedTabButton.State := nbsPressed;
  end;
end;

procedure TcxCustomTabControlViewInfo.SetPressedTabVisibleIndex(Value: Integer);
begin
  if PressedTabVisibleIndex <> Value then
  begin
    if PressedTabVisibleIndex <> -1 then
      RepaintTab(PressedTabVisibleIndex, tpcPressed);
    FPressedTabVisibleIndex := Value;
    if PressedTabVisibleIndex <> -1 then
      RepaintTab(PressedTabVisibleIndex, tpcPressed);
  end;
end;

procedure TcxCustomTabControlViewInfo.SetTabIndex(Value: Integer);
begin
  if IsUpdating then
    FTabCalculatingIndex := Value
  else
    DoSetTabIndex(Value);
end;

procedure TcxCustomTabControlViewInfo.SetTrackingTabVisibleIndex(Value: Integer);
begin
  if TrackingTabVisibleIndex <> Value then
  begin
    if IsTabVisibleIndexValid(TrackingTabVisibleIndex) then
      RepaintTab(TrackingTabVisibleIndex, tpcTracking);
    FTrackingTabVisibleIndex := Value;
    if IsTabVisibleIndexValid(TrackingTabVisibleIndex) then
    begin
      if IsTabFullVisible(TrackingTabVisibleIndex) then
        RepaintTab(TrackingTabVisibleIndex, tpcTracking)
      else
        MakeTabVisible(TrackingTabVisibleIndex);
    end;
  end;
end;

procedure TcxCustomTabControlViewInfo.SynchronizeHotTrackStates(Shift: TShiftState);
var
  AMousePos: TPoint;
  ANewHotTrackTabVisibleIndex, APrevHotTrackVisibleIndex: Integer;
  ANewHotTrackNavigatorButton: TcxPCCustomHeaderButtonViewInfo;
  AHitObject: TObject;
begin
  if Controller.FIsGoDialogShowing then Exit;
  AMousePos := Controller.GetMouseCursorPos;
  Controller.HitTest.Update(Shift, AMousePos);

  ANewHotTrackTabVisibleIndex := -1;
  ANewHotTrackNavigatorButton := nil;
  AHitObject := Controller.HitTest.HitObject;
  if IsEnabled and (IControl.GetDragAndDropState = ddsNone) then
    if Controller.HitTest.HitAtHeaderButton and
       (TcxPCCustomHeaderButtonViewInfo(AHitObject).State <> nbsDisabled) and
        (TcxPCCustomHeaderButtonViewInfo(AHitObject).AllowHotTrack) then
      ANewHotTrackNavigatorButton := TcxPCCustomHeaderButtonViewInfo(AHitObject)
    else
    begin
      if AllowHotTrack then
      begin
        ANewHotTrackTabVisibleIndex := VisibleIndexOfTabAt(AMousePos.X, AMousePos.Y);
        if (ANewHotTrackTabVisibleIndex <> -1) and (not TabsViewInfo[ANewHotTrackTabVisibleIndex].Enabled) then
          ANewHotTrackTabVisibleIndex := -1;
      end;
    end;
  APrevHotTrackVisibleIndex := HotTrackTabVisibleIndex;
  HotTrackTabVisibleIndex := ANewHotTrackTabVisibleIndex;
  HotTrackNavigatorButton := ANewHotTrackNavigatorButton;
  if APrevHotTrackVisibleIndex <> ANewHotTrackTabVisibleIndex then
    Controller.HitTest.Recalculate;
  if Controller.HitTest.HitAtTabButton then
    HotTrackTabButton := Controller.HitTest.HitObject as TcxPCCustomTabButtonViewInfo
  else
    HotTrackTabButton := nil;
end;

procedure TcxCustomTabControlViewInfo.UpdateButtonsState;

  procedure InternalUpdateButtonState(AButtonInfo: TcxPCNavigatorButtonViewInfo);
  var
    ANewButtonState, APrevButtonState: TcxPCNavigatorButtonState;
  begin
    APrevButtonState := AButtonInfo.State;
    ANewButtonState := APrevButtonState;
    if CanPressButton(AButtonInfo.ButtonType) then
    begin
      if APrevButtonState = nbsDisabled then
        ANewButtonState := nbsNormal;
    end
    else
      ANewButtonState := nbsDisabled;
    if ANewButtonState <> APrevButtonState then
    begin
      if (ANewButtonState = nbsDisabled) and (AButtonInfo = FHotTrackNavigatorButton) then
        FHotTrackNavigatorButton := nil;
      if (ANewButtonState = nbsDisabled) and (AButtonInfo = FPressedNavigatorButton) then
        FPressedNavigatorButton := nil;
      AButtonInfo.State := ANewButtonState;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    if NavigatorButtonInfos[I].IsNavigatorButton([nbTopLeft, nbBottomRight, nbClose]) then
      InternalUpdateButtonState(TcxPCNavigatorButtonViewInfo(NavigatorButtonInfos[I]));
end;

procedure TcxCustomTabControlViewInfo.UpdateNavigatorButtons(AOnlyObligatoryButtons: Boolean);
begin
  FNavigatorButtons := Properties.GetNavigatorButtons(AOnlyObligatoryButtons);
end;

procedure TcxCustomTabControlViewInfo.UpdateTabPosition(AShowButtons: Boolean);
begin
  FNavigatorButtonInfos.Clear;
  FHeaderButtonHeight := Painter.GetButtonHeight;
  if AShowButtons and not Properties.MultiLine then
    CreateHeaderButtons;
  FTabsPosition := Painter.GetTabsPosition;
end;

procedure TcxCustomTabControlViewInfo.BeginCalculate;
begin
  FTabCalculatingIndex := TabIndex;
  FUpdating := True;
end;

procedure TcxCustomTabControlViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  FExtendedBottomOrRightTabsRect := TdxRightToLeftLayoutConverter.ConvertRect(FExtendedBottomOrRightTabsRect, AClientBounds);
  FExtendedTopOrLeftTabsRect := TdxRightToLeftLayoutConverter.ConvertRect(FExtendedTopOrLeftTabsRect, AClientBounds);
  for I := 0 to FNavigatorButtonInfos.Count - 1 do
    NavigatorButtonInfos[I].DoRightToLeftConversion(AClientBounds);
  FTabsViewInfo.DoRightToLeftConversion(AClientBounds)
end;

procedure TcxCustomTabControlViewInfo.EndCalculate;
begin
  FUpdating := False;
  TabIndex := FTabCalculatingIndex;
end;

function TcxCustomTabControlViewInfo.IsUpdating: Boolean;
begin
  Result := FUpdating;
end;

function TcxCustomTabControlViewInfo.GetTabViewInfo(ATab: TcxTab): TcxTabViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to TabsViewInfo.ViewInfoCount - 1 do
    if TabsViewInfo.ViewInfos[I].Tab = ATab then
       Result := TabsViewInfo.ViewInfos[I];
end;

function TcxCustomTabControlViewInfo.GetTabVisibleIndex(ATab: TcxTab): Integer;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := GetTabViewInfo(ATab);
  if ATabViewInfo <> nil then
    Result := ATabViewInfo.VisibleIndex
  else
    Result := -1;
end;

{ TcxPageControlProperties }

constructor TcxPageControlProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FPages := TList.Create;
  FTabSheetClass := TcxTabSheet;
end;

destructor TcxPageControlProperties.Destroy;
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    with Pages[I] do
    begin
      FPageControl := nil;
      FTab := nil;
    end;
  FreeAndNil(FPages);
  inherited;
end;

function TcxPageControlProperties.FindNextPage(ACurrentPage: TcxTabSheet; AGoForward, ACheckTabAccessibility, ACircular: Boolean): TcxTabSheet;

  function GetDefaultStartPageIndex: Integer;
  begin
    if AGoForward then
      Result := 0
    else
      Result := PageCount - 1;
  end;

  function IncrementIndex(var APageIndex: Integer): Boolean;
  const
    AStep: array [Boolean] of Integer = (-1, 1);
  begin
    Result := True;
    Inc(APageIndex, AStep[AGoForward]);
    if (APageIndex < 0) or (PageCount - 1 < APageIndex) then
      if ACircular then
        APageIndex := GetDefaultStartPageIndex
      else
        Result := False;
  end;

  function InternalGetNextPage(APagesCount: Integer; APageIndex: Integer): TcxTabSheet;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to APagesCount - 1 do
    begin
      if not ACheckTabAccessibility or
        Pages[APageIndex].TabVisible and
        (Pages[APageIndex].Enabled or AllowDisabledTabAccess) then
      begin
        Result := Pages[APageIndex];
        Break;
      end
      else
        if not IncrementIndex(APageIndex) then
          Break;
    end;
  end;

var
  APageIndex: Integer;
begin
  Result := nil;
  if PageCount <> 0 then
  begin
    APageIndex := FPages.IndexOf(ACurrentPage);
    if APageIndex = -1 then
      Result := InternalGetNextPage(PageCount, GetDefaultStartPageIndex)
    else
      if IncrementIndex(APageIndex) then
        Result := InternalGetNextPage(PageCount - 1, APageIndex);
  end;
end;

function TcxPageControlProperties.CanCloseButtonShow(ATab: TcxTab): Boolean;
begin
  Result := Pages[ATab.Index].AllowCloseButton;
end;

function TcxPageControlProperties.CreateNewTab: Integer;
var
  APage: TcxTabSheet;
begin
  APage := TabSheetClass.Create(nil);
  APage.PageControl := Control;
  APage.ImageIndex := APage.PageIndex;
  Result := APage.PageIndex;
end;

procedure TcxPageControlProperties.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TcxPageControlProperties then
  begin
    ActivePage := TcxPageControlProperties(Source).ActivePage;
    OnPageChanging := TcxPageControlProperties(Source).OnPageChanging;
  end;
end;

procedure TcxPageControlProperties.DoChanging(ANewTabIndex: Integer; var AAllowChange: Boolean);
begin
  inherited;
  if AAllowChange and (ANewTabIndex <> -1) then
    AAllowChange := CanChangeActivePage(Pages[ANewTabIndex]);
end;

procedure TcxPageControlProperties.TabIndexChanged;
var
  APage: TcxTabSheet;
begin
  BeginUpdate;
  try
    APage := ActivePage;
    UpdateActivePage;
  finally
    CancelUpdate;
  end;
  if APage <> ActivePage then
    inherited;
end;

function TcxPageControlProperties.GetActivePage: TcxTabSheet;
begin
  Result := FActivePage;
end;

function TcxPageControlProperties.GetPage(ATabIndex: Integer): TcxTabSheet;
begin
  Result := TcxTabSheet(FPages[ATabIndex]);
end;

procedure TcxPageControlProperties.DoPageChange;
begin
  inherited DoChange;
end;

procedure TcxPageControlProperties.ImageListChange(Sender: TObject);
var
  I: Integer;
begin
  if FPageInserting or IsDestroying then
    Exit;

  for I := 0 to PageCount - 1 do
    Pages[I].FTab.ImageIndex := Pages[I].ImageIndex;
  inherited ImageListChange(Sender);
end;

procedure TcxPageControlProperties.DoCloseTab(AIndex: Integer);
begin
  Pages[AIndex].Free;
end;

function TcxPageControlProperties.InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string;
begin
  Result := Pages[ATab.Index].TabHint;
  if Result = '' then
    Result := inherited InternalGetTabHint(ATab, ACanShow);
end;

function TcxPageControlProperties.TabIndexTabMustBeVisible: Boolean;
begin
  Result := True;
end;

procedure TcxPageControlProperties.MoveTab(ACurrentIntex, ANewIndex: Integer);
begin
  Pages[ACurrentIntex].PageIndex := ANewIndex;
end;

procedure TcxPageControlProperties.InsertPage(APage: TcxTabSheet);
var
  PageIndex: Integer;
begin
  if CanAllocateHandle(Control) then
    Control.HandleNeeded;
  FPageInserting := True;
  try
    PageIndex := FPages.Add(APage);
    APage.SetParentPageControl(Control);
    Tabs.Add(APage.Caption);
  finally
    FPageInserting := False;
  end;
  APage.FTab := Tabs[PageIndex];
  UpdateTab(APage);
end;

procedure TcxPageControlProperties.RemovePage(APage: TcxTabSheet);

  function InternalFindNextPage(ACurrentPage: TcxTabSheet; AGoForward,
    ACheckTabAccessibility: Boolean): TcxTabSheet;
  begin
    Result := FindNextPage(ACurrentPage, AGoForward, ACheckTabAccessibility, False);
    if Result = nil then
      Result := FindNextPage(ACurrentPage, not AGoForward, ACheckTabAccessibility, False);
  end;

var
  ANextPage: TcxTabSheet;
begin
  LockChangingEvent;
  try
    if APage = ActivePage then
    begin
      ANextPage := InternalFindNextPage(APage, True, not Control.IsDesigning);
      if ANextPage = APage then
        ANextPage := nil;
    end
    else
      ANextPage := ActivePage;

    Tabs.Delete(FPages.Remove(APage));
    APage.FTab := nil;
    FTabIndex := -1;
    ActivePage := ANextPage;
    APage.SetParentPageControl(nil);
  finally
    UnlockChangingEvent;
  end;
end;

procedure TcxPageControlProperties.UpdateActivePage;
begin
  if TabIndex = -1 then
    ActivePage := nil
  else
    ActivePage := Pages[TabIndex];
end;

procedure TcxPageControlProperties.UpdateTab(APage: TcxTabSheet);
begin
  if Control.IsLoading then
    Exit;
  with APage.FTab do
  begin
    AllowCloseButton := APage.AllowCloseButton;
    Caption := APage.Caption;
    Enabled := APage.Enabled;
    ImageIndex := APage.ImageIndex;
    Highlighted := APage.Highlighted;
    Visible := APage.TabVisible;
  end;
  if FActivePage = APage then
    if APage.TabVisible then
      TabIndex := APage.FTab.Index
    else
      TabIndex := -1;
end;

procedure TcxPageControlProperties.UpdateTabs;
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    UpdateTab(Pages[I]);
end;

function TcxPageControlProperties.CanChangeActivePage(ANewPage: TcxTabSheet): Boolean;
begin
  Result := FNotifyEventLockCount = 0;
  if Result then
    DoPageChanging(ANewPage, Result)
end;

procedure TcxPageControlProperties.DoPageChanging(NewPage: TcxTabSheet; var AllowChange: Boolean);
begin
  if not IsChangingEventLocked and Assigned(FOnPageChanging) then
    FOnPageChanging(Self, NewPage, AllowChange);
end;

procedure TcxPageControlProperties.ChangeActivePage(APage: TcxTabSheet);

  procedure SetActivePage(APage: TcxTabSheet);
  begin
    if FActivePage <> nil then
      FActivePage.Visible := False;
    FActivePage := APage;
  end;

  procedure MakePageVisible(APage: TcxTabSheet);
  begin
    if APage <> nil then
    begin
      APage.BringToFront;
      APage.Visible := True;
    end;
  end;

  function DoFindActiveControl(AParentForm: TCustomForm): TWinControl;
  begin
    if (APage <> nil) and (AParentForm <> nil) and (FActivePage <> nil) and
      (AParentForm.ActiveControl = FActivePage) then
    begin
      if APage.Enabled and Control.Visible and Control.Enabled and Control.Parent.CanFocus then
        Result := APage
      else
        Result := Control;
    end
    else
      Result := nil;
  end;

  procedure DoSetActiveControl(AParentForm: TCustomForm; AActiveControl: TWinControl);
  begin
    if AActiveControl <> nil then
      AParentForm.ActiveControl := AActiveControl;
    if (AParentForm <> nil) and (APage <> nil) and (AParentForm.ActiveControl = APage) then
      APage.SelectFirst;
  end;

  procedure SynchronizeTabIndex;
  begin
    if ActivePage = APage then
    begin
      BeginUpdate;
      FActivePageSetting := True;
      try
        if (APage <> nil) and APage.TabVisible then
          TabIndex := APage.FTab.Index
        else
          TabIndex := -1;
      finally
        FActivePageSetting := False;
        EndUpdate;
      end;
    end;
  end;

var
  ANewActiveControl: TWinControl;
  AParentForm: TCustomForm;
  AModified: Boolean;
begin
  if FActivePage <> APage then
  begin
    if not CanChangeActivePage(APage) then
      Exit;

    LockNotifyEvents;
    try
      AParentForm := GetParentForm(Control);
      if (AParentForm <> nil) and (FActivePage <> nil) and
        FActivePage.ContainsControl(AParentForm.ActiveControl) then
      begin
        AParentForm.ActiveControl := FActivePage;
        if AParentForm.ActiveControl <> FActivePage then
        begin
          TabIndex := FActivePage.FTab.Index;
          Exit;
        end;
      end;

      ANewActiveControl := DoFindActiveControl(AParentForm);
      MakePageVisible(APage);
      DoSetActiveControl(AParentForm, ANewActiveControl);
      SetActivePage(APage);
      if APage <> nil then
        APage.Invalidate;
    finally
      UnLockNotifyEvents;
    end;
    AModified := True;
  end
  else
    AModified := False;
  SynchronizeTabIndex;
  if AModified then
  begin
    DoPageChange;
    Control.SetModified;
  end;
end;

procedure TcxPageControlProperties.UpdateTabOrders;
var
  I: Integer;
begin
  if not IsDestroying then
  begin
    for I := 0 to PageCount - 1 do
      Pages[I].TabOrder := I;
  end;
end;

function TcxPageControlProperties.GetControl: TcxPageControl;
begin
  Result := GetOwner as TcxPageControl;
end;

function TcxPageControlProperties.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TcxPageControlProperties.SetActivePage(APage: TcxTabSheet);
begin
  if FActivePageSetting or Control.IsDestroying or (APage <> nil) and (APage.PageControl <> Control) then
    Exit;
  ChangeActivePage(APage);
end;

{ TcxPageControlViewInfo }

function TcxPageControlViewInfo.GetActivePageColor: TColor;
begin
  Result := ActivePage.Color;
end;

function TcxPageControlViewInfo.GetTabColor(ATabVisibleIndex: Integer): TColor;
var
  ATabSheet: TcxTabSheet;
begin
  Result := inherited GetTabColor(ATabVisibleIndex);
  if TabsViewInfo[ATabVisibleIndex].IsNewButton then
    Exit;
  ATabSheet := Properties.Pages[TabsViewInfo[ATabVisibleIndex].Index];
  if (Result = clDefault) and (ATabSheet <> nil) and
    ((pcoUsePageColorForTab in Properties.Options) or Painter.AlwaysColoredTabs) and
    not ATabSheet.ParentColor then
      Result := ATabSheet.Color;
end;

function TcxPageControlViewInfo.HasActivePage: Boolean;
begin
  Result := ActivePage <> nil;
end;

function TcxPageControlViewInfo.UseActivePageColor: Boolean;
begin
  Result := HasActivePage and not ActivePage.ParentColor;
end;

function TcxPageControlViewInfo.GetActivePage: TcxTabSheet;
begin
  Result := Properties.ActivePage;
end;

function TcxPageControlViewInfo.GetProperties: TcxPageControlProperties;
begin
  Result := TcxPageControlProperties(inherited GetProperties);
end;

{ TcxPageControl }

constructor TcxPageControl.Create(AOwner: TComponent);
begin
  inherited;
  Properties.OnPageChanging := PropertiesPageChangingHandler;
end;

function TcxPageControl.FindNextPage(ACurrentPage: TcxTabSheet; AGoForward,
  ACheckTabAccessibility: Boolean): TcxTabSheet;
begin
  Result := FindNextPageEx(ACurrentPage, AGoForward, ACheckTabAccessibility, True);
end;

function TcxPageControl.FindNextPageEx(ACurrentPage: TcxTabSheet;
  AGoForward, ACheckTabAccessibility, ACircular: Boolean): TcxTabSheet;
begin
  Result := Properties.FindNextPage(ACurrentPage, AGoForward, ACheckTabAccessibility, ACircular);
end;

procedure TcxPageControl.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if (DockClient(Source, Point(X, Y)) >= 0) and Assigned(OnDockDrop) then
    OnDockDrop(Self, Source, X, Y);
end;

procedure TcxPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
var
  Page: TcxTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) then
    ActivePage := Page;
end;

procedure TcxPageControl.AlignControls(AControl: TControl; var Rect: TRect);
var
  ARgn: TcxRegion;
begin
  inherited AlignControls(AControl, Rect);
  if (ActivePage <> nil) and ActivePage.HandleAllocated and
    (ActivePage.BorderWidth > 0) and Painter.IsNativePainting then
  begin
    ARgn := TcxRegion.Create(GetControlRect(ActivePage));
    try
      SendMessage(ActivePage.Handle, WM_NCPAINT, ARgn.Handle, 0);
    finally
      ARgn.Free;
    end;
  end;
end;

procedure TcxPageControl.DoAddDockClient(Client: TControl;
  const ARect: TRect);
begin
  if FNewDockSheet <> nil then Client.Parent := FNewDockSheet;
end;

function TcxPageControl.DockClient(DockSource: TDragDockObject;
  MousePos: TPoint): Integer;

  function CheckDockingControl: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to PageCount - 1 do
      if DockSource.Control.Parent = Pages[I] then
      begin
        Pages[I].PageIndex := PageCount - 1;
        Result := False;
        Break;
      end;
  end;

var
  ADockingControl: TControl;
begin
  Result := 0;
  if CheckDockingControl then
  begin
    FNewDockSheet := Pages[Properties.CreateNewTab];
    try
      try
        ADockingControl := DockSource.Control;
        if ADockingControl is TCustomForm then
          FNewDockSheet.Caption := TCustomForm(ADockingControl).Caption;
        ADockingControl.Dock(Self, DockSource.DockRect);
      except
        FNewDockSheet.Free;
        raise;
      end;
      FNewDockSheet.TabVisible := ADockingControl.Visible;
      if ADockingControl.Visible then
        ActivePage := FNewDockSheet;
      ADockingControl.Align := alClient;
    finally
      FNewDockSheet := nil;
    end;
  end;
end;

procedure TcxPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  R := cxGetWindowRect(Self);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TcxPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not IsDestroying then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TcxPageControl.GetActivePage: TcxTabSheet;
begin
  Result := Properties.ActivePage;
end;

procedure TcxPageControl.AfterLoaded;
begin
  Properties.BeginUpdate;
  try
    inherited;
    Properties.UpdateTabs;
    if not IsDesigning and (ActivePage <> nil) and
      not (ActivePage.Enabled or Properties.AllowDisabledTabAccess) then
      ActivePage := nil;
  finally
    Properties.EndUpdate;
  end;
end;

function TcxPageControl.GetPropertiesClass: TcxCustomTabControlPropertiesClass;
begin
  Result := TcxPageControlProperties;
end;

procedure TcxPageControl.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

function TcxPageControl.GetViewInfoClass: TcxCustomTabControlViewInfoClass;
begin
  Result := TcxPageControlViewInfo;
end;

procedure TcxPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TcxTabSheet(Child).PageIndex := Order;
end;

procedure TcxPageControl.ShowControl(AControl: TControl);
var
  Page: TcxTabSheet;
begin
  if (AControl is TcxTabSheet) then
  begin
    Page := TcxTabSheet(AControl);
    if Page.PageControl = Self then
      ActivePage := Page;
  end;
  inherited ShowControl(AControl);
end;

function TcxPageControl.UndockClient(NewTarget, Client: TControl): Boolean;
var
  APage: TcxTabSheet;
begin
  Result := True;
  if not IsDestroying then
  begin
    APage := GetPageFromDockClient(Client);
    if APage <> nil then
    begin
      FUndockingPage := APage;
      Client.Align := alNone;
    end;
  end;
end;

procedure TcxPageControl.PropertiesPageChangingHandler(Sender: TObject; NewPage: TcxTabSheet; var AllowChange: Boolean);
begin
  if Assigned(FOnPageChanging) and not Properties.IsChangeEventLocked then
    FOnPageChanging(Self, NewPage, AllowChange);
end;

function TcxPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := True;
  if Assigned(OnUnDock) then
    OnUnDock(Self, Client, NewTarget, Result);
  Result := Result and UndockClient(NewTarget, Client);
end;

procedure TcxPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    Proc(Pages[I]);
end;

procedure TcxPageControl.ControlChange(Inserting: Boolean; Child: TControl);
begin
  if IsDesigning and Inserting and not (Child is TcxTabSheet) then
  begin
    Child.SetBounds(Child.Left + Left, Child.Top + Top, Child.Width, Child.Height);
    Child.Parent := Parent;
  end;
end;

function TcxPageControl.GetPageFromDockClient(Client: TControl): TcxTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TcxPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage = nil then
    Result := -1
  else
    Result := ActivePage.PageIndex;
end;

function TcxPageControl.GetDockClientFromPoint(P: TPoint): TControl;
var
  APage: TcxTabSheet;
  ATabVisibleIndex, I: Integer;
begin
  Result := nil;
  if DockSite then
  begin
    ATabVisibleIndex := VisibleIndexOfTabAt(P.X, P.Y);
    if (ATabVisibleIndex >= 0) and not Controller.HitTest.HitAtTabButton then
    begin
      APage := nil;
      for I := 0 to PageCount - 1 do
        if Pages[I].TabIndex = ATabVisibleIndex then
        begin
          APage := Pages[I];
          Break;
        end;
      if (APage <> nil) and (APage.ControlCount > 0) then
      begin
        Result := APage.Controls[0];
        if Result.HostDockSite <> Self then
          Result := nil;
      end;
    end;
  end;
end;

function TcxPageControl.GetPage(Index: Integer): TcxTabSheet;
begin
  Result := Properties.Pages[Index];
end;

function TcxPageControl.GetPageCount: Integer;
begin
  Result := Properties.PageCount;
end;

function TcxPageControl.GetProperties: TcxPageControlProperties;
begin
  Result := inherited Properties as TcxPageControlProperties;
end;

function TcxPageControl.GetTabCount: Integer;
begin
  Result := ViewInfo.TabsViewInfo.ViewInfoCount;
end;

procedure TcxPageControl.SetActivePage(APage: TcxTabSheet);
begin
  Properties.ActivePage := APage;
end;

procedure TcxPageControl.SetActivePageIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TcxPageControl.SetProperties(Value: TcxPageControlProperties);
begin
  inherited Properties := Value;
end;

procedure TcxPageControl.WMLButtonDown(var Message: TWMLButtonDown);
var
  ADockClient: TControl;
begin
  inherited;
  ADockClient := GetDockClientFromPoint(SmallPointToPoint(Message.Pos));
  if ADockClient <> nil then
    ADockClient.BeginDrag(False);
end;

procedure TcxPageControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  ADockClient: TControl;
begin
  inherited;
  ADockClient := GetDockClientFromPoint(SmallPointToPoint(Message.Pos));
  if ADockClient <> nil then
    ADockClient.ManualDock(nil, nil, alNone);
end;

procedure TcxPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  APage: TcxTabSheet;
  I: Integer;
  S: string;
begin
  APage := GetPageFromDockClient(Message.Client);
  if APage <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          for I := 1 to Length(S) do
            if (S[I] = #10) or (S[I] = #13) then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          APage.Caption := S;
        end;
      CM_VISIBLECHANGED:
        APage.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;

procedure TcxPageControl.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  ControlChange(Message.Inserting, Message.Control);
end;

procedure TcxPageControl.CMUnDockClient(var Message: TCMUnDockClient);
begin
  UndockClient(Message.NewTarget, Message.Client);
end;

{ TcxTab }

constructor TcxTab.Create(ATabs: TcxTabs);
begin
  inherited Create;
  FTabs := ATabs;
  FColor := clDefault;
  FEnabled := True;
  FVisible := True;
  FImageIndex := -1;
  FDestroyHandlers := TcxEventHandlerCollection.Create;
  FAllowCloseButton := True;
  FShowAccelChar := True;
end;

destructor TcxTab.Destroy;
begin
  DestroyHandlers.CallEvents(Self, []);
  FreeAndNil(FDestroyHandlers);
  DoDestroy;
  inherited;
end;

procedure TcxTab.AssignTo(Dest: TPersistent);
var
  DestTab: TcxTab;
begin
  if Dest is TcxTab then
  begin
    DestTab := TcxTab(Dest);
    DestTab.FCaption := Caption;
    DestTab.FEnabled := Enabled;
    DestTab.FHighlighted := Highlighted;
    DestTab.ImageIndex := ImageIndex;
    DestTab.FObject := FObject;
    DestTab.Visible := Visible;
    DestTab.Selected := Selected;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TcxTab.Changed(ATabPropertyChanged: TcxPCTabPropertyChanged);
begin
  Tabs.Changed(Self, ATabPropertyChanged);
end;

procedure TcxTab.DoDestroy;
begin
  if not Tabs.FIsTabsCleaning then
    Tabs.RemoveTab(Self);
end;

function TcxTab.GetViewInfoClass: TcxTabViewInfoClass;
begin
  Result := TcxTabViewInfo;
end;

function TcxTab.IsNewButton: Boolean;
begin
  Result := False;
end;

function TcxTab._AddRef: Integer;
begin
  Result := -1;
end;

function TcxTab._Release: Integer;
begin
  Result := -1;
end;

function TcxTab.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TcxTab.GetAdornerTargetElementBounds: TRect;
begin
  Result := VisibleRect;
end;

function TcxTab.GetAdornerTargetElementControl: TWinControl;
begin
  Result := TcxTabControl(Tabs.Properties.Owner);
end;

function TcxTab.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible and not cxRectIsEmpty(VisibleRect);
end;

function TcxTab.GetFullRect: TRect;
begin
  if GetViewInfo <> nil then
    Result := GetViewInfo.FullRect
  else
    Result := cxEmptyRect;
end;

function TcxTab.GetHotTrack: Boolean;
begin
  Result := (GetViewInfo <> nil) and GetViewInfo.IsHotTrack;
end;

function TcxTab.GetIndex: Integer;
begin
  Result := Tabs.TabItems.IndexOf(Self);
end;

function TcxTab.GetImageIndex: TcxImageIndex;
begin
  Result := Properties.GetImageIndex(Self);
end;

function TcxTab.GetIsMainTab: Boolean;
begin
  Result := (GetViewInfo <> nil) and GetViewInfo.IsMainTab;
end;

function TcxTab.GetPressed: Boolean;
begin
  Result := (GetViewInfo <> nil) and GetViewInfo.IsPressed;
end;

function TcxTab.GetProperties: TcxCustomTabControlProperties;
begin
  Result := Tabs.Properties;
end;

function TcxTab.GetRealVisible: Boolean;
begin
  Result := not IsRectEmpty(VisibleRect)
end;

function TcxTab.GetTracking: Boolean;
begin
  Result := (GetViewInfo <> nil) and GetViewInfo.IsTracking;
end;

function TcxTab.GetControlViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := Tabs.GetControlViewInfo;
end;

function TcxTab.GetViewInfo: TcxTabViewInfo;
begin
  if GetControlViewInfo <> nil  then
    Result := GetControlViewInfo.GetTabViewInfo(Self)
  else
    Result := nil;
end;

function TcxTab.GetVisibleIndex: Integer;
begin
  if GetViewInfo = nil then
    Result := -1
  else
    Result := GetViewInfo.VisibleIndex;
end;

function TcxTab.GetVisibleRect: TRect;
begin
  if GetViewInfo <> nil then
    Result := GetViewInfo.VisibleRect
  else
    Result := cxEmptyRect;
end;

function TcxTab.GetVisibleRow: Integer;
begin
  Result := GetViewInfo.VisibleRow;
end;

procedure TcxTab.InternalSetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TcxTab.SetAllowCloseButton(const Value: Boolean);
begin
  if FAllowCloseButton <> Value then
  begin
    FAllowCloseButton := Value;
    Changed(tpcLayout);
  end;
end;

procedure TcxTab.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    InternalSetCaption(Value);
    Changed(tpcLayout);
  end;
end;

procedure TcxTab.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed(tpcColor);
  end;
end;

procedure TcxTab.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(tpcEnabled);
  end;
end;

procedure TcxTab.SetHighlighted(const Value: Boolean);
begin
  if Value <> FHighlighted then
  begin
    FHighlighted := Value;
    Changed(tpcHighlighted);
  end;
end;

procedure TcxTab.SetImageIndex(Value: TcxImageIndex);
begin
  if Value < 0 then
    Value := -1;
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(tpcLayout);
  end;
end;

procedure TcxTab.SetSelected(const Value: Boolean);

  function GetSelectedTabCount: Integer;
  var
    I, TabCount: Integer;
  begin
    Result := 0;
    TabCount := Tabs.Count;
    for I := 0 to TabCount - 1 do
      if Tabs[I].Selected then
        Inc(Result);
  end;

begin
  if FSelected <> Value then
  begin
    if GetViewInfo <> nil then
    begin
      if not GetViewInfo.CanMultiSelect then
      begin
        if Value and (Properties.TabIndex <> Index) then
          Exit;
      end
      else
        if Value and (GetSelectedTabCount > 0) and (not Properties.MultiSelect) then
          Properties.MultiSelect := True;

      FSelected := Value;
      Changed(tpcSelected);
    end
    else
      FSelected := Value;
  end;
end;

procedure TcxTab.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(tpcVisible);
  end;
end;

{ TcxPCNewButton }

constructor TcxPCNewButton.Create(ATabs: TcxTabs);
begin
  inherited;
  Visible := False;
end;

function TcxPCNewButton.GetViewInfoClass: TcxTabViewInfoClass;
begin
  Result := TcxPCNewButtonViewInfo;
end;

function TcxPCNewButton.GetIndex: Integer;
begin
  Result := cxPCNewButtonIndex;
end;

procedure TcxPCNewButton.DoDestroy;
begin
// do nothing
end;

function TcxPCNewButton.IsNewButton: Boolean;
begin
  Result := True;
end;

procedure TcxPCNewButton.SetWidth(Value: Integer);
begin
  if Width <> Value then
  begin
    FWidth := Value;
    Changed(tpcLayout);
  end;
end;

{ TcxPCImageList }

constructor TcxPCImageList.Create(AProperties: TcxCustomTabControlProperties);
begin
  inherited Create;
  FProperties := AProperties;

  FBaseHotImageChangeLink := TChangeLink.Create;
  FBaseHotImageChangeLink.OnChange := BaseImageListChange;
  FBaseImageChangeLink := TChangeLink.Create;
  FBaseImageChangeLink.OnChange := BaseImageListChange;
  FImageRotationAngle := ra0;

  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
end;

destructor TcxPCImageList.Destroy;
begin
  FFreeNotificator.Free;
  FBaseImageChangeLink.Free;
  FBaseHotImageChangeLink.Free;
  inherited Destroy;
end;

procedure TcxPCImageList.Draw(ACanvas: TcxCanvas; const R: TRect; AIndex: Integer;
  ABackgroundColor: TColor; AIsNativePainting, AEnabled, AHot: Boolean; APalette: IdxColorPalette);

  procedure DrawBitmap(ACanvas: TcxCanvas; const R: TRect);
  begin
    cxDrawImage(ACanvas.Handle, R, R, nil, GetImages(AHot), AIndex,
      EnabledImageDrawModeMap[AEnabled], True, 0, clNone, True, APalette);
  end;

  function CreateRotatedBitmap: TcxBitmap;
  const
    BackRotateAngle: array [TcxRotationAngle] of TcxRotationAngle = (ra0, raMinus90, raPlus90, ra0);
  begin
    Result:= TcxBitmap.CreateSize(R, pf32bit);
    Result.HandleType := bmDDB;
    if AIsNativePainting then
      Result.Transparent := True;
    Result.cxCanvas.CopyRect(Result.ClientRect, ACanvas.Canvas, R);
    Result.Rotate(BackRotateAngle[ImageRotationAngle]);
    DrawBitmap(Result.cxCanvas, Result.ClientRect);
    Result.Rotate(ImageRotationAngle);
  end;

  procedure CheckImageIndex;
  var
    AImages: TCustomImageList;
  begin
    AImages := GetImages(AHot);
    if AImages = nil then
      OutError('Draw', cxGetResourceString(@scxPCNoBaseImages));
    if (AIndex < 0) or (AIndex >= AImages.Count) then
      OutError('Draw', Format(cxGetResourceString(@scxPCImageListIndexError), [AIndex, AImages.Count - 1]));
  end;

var
  ARotatedBitmap: TcxBitmap;
begin
  CheckImageIndex;
  if ImageRotationAngle = ra0 then
    DrawBitmap(ACanvas, R)
  else
  begin
    ARotatedBitmap := CreateRotatedBitmap;
    try
      ACanvas.Draw(R.Left, R.Top, ARotatedBitmap);
    finally
      ARotatedBitmap.Free;
    end;
  end;
end;

function TcxPCImageList.IsImagesAssigned: Boolean;
begin
  Result := (FBaseImages <> nil) or (FBaseHotImages <> nil);
end;

procedure TcxPCImageList.BaseImageListChange(Sender: TObject);
begin
  Change;
end;

procedure TcxPCImageList.Change;
begin
  CallNotify(OnChange, Self);
end;

function TcxPCImageList.GetBaseImageSize: TSize;
begin
  Result := dxGetImageSize(GetImages(FBaseImages = nil), dxGetScaleFactor(FProperties.Owner));
end;

function TcxPCImageList.GetImages(AHot: Boolean): TCustomImageList;
begin
  if AHot and (FBaseHotImages <> nil) then
    Result := FBaseHotImages
  else
    Result := FBaseImages;
end;

function TcxPCImageList.GetRotatedImageSize: TSize;
begin
  Result := GetBaseImageSize;
  if ImageRotationAngle <> ra0 then
    Result := cxSize(Result.cy, Result.cx);
end;

procedure TcxPCImageList.FreeNotification(AComponent: TComponent);
begin
  if AComponent = BaseImages then
    BaseImages := nil
  else
    if AComponent = BaseHotImages then
      BaseHotImages := nil;
end;

class procedure TcxPCImageList.OutError(SourceMethodName, Msg: TCaption);
begin
  raise EdxException.Create('TcxPCImageList.' + SourceMethodName + ': ' + Msg);
end;

procedure TcxPCImageList.SetBaseHotImages(const Value: TCustomImageList);
begin
  cxSetImageList(Value, FBaseHotImages, FBaseHotImageChangeLink, FFreeNotificator);
end;

procedure TcxPCImageList.SetBaseImages(
  const Value: TCustomImageList);
begin
  cxSetImageList(Value, FBaseImages, FBaseImageChangeLink, FFreeNotificator);
end;

{ TcxCustomTabControlController }

constructor TcxCustomTabControlController.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FHitTest := TcxCustomTabControlHitTest.Create(Self);
  FMouseDownTabVisibleIndex := -1;
  Supports(Owner, IcxTabControl, FIControl);
  CreateGoDialog;
  FHintHelper := GetHintHelperClass.Create(Self);
end;

destructor TcxCustomTabControlController.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FTimer);
  FreeAndNil(FHintHelper);
  FreeAndNil(FGoDialog);
  FreeAndNil(FHitTest);
  FIControl := nil;
  inherited;
end;

procedure TcxCustomTabControlController.ScrollTabs(ADelta: Integer);
var
  I: Integer;
begin
  if not Properties.MultiLine and ViewInfo.HasNavigatorButton(nbTopLeft) and
    ViewInfo.HasNavigatorButton(nbBottomRight)
  then
    for I := 0 to Abs(ADelta) - 1 do
    begin
      if ADelta < 0 then
        ArrowButtonClick(nbTopLeft)
      else
        ArrowButtonClick(nbBottomRight);
    end;
end;

procedure TcxCustomTabControlController.CheckHint;

  function NeedShowHint(AHint: string): Boolean;
  begin
    Result := AHint <> '';
  end;

var
  AHintAreaBounds: TRect;
  AHint: string;
  APrevHintObject: TcxPCCustomElementViewInfo;
begin
  AHint := '';
  APrevHintObject := FHintObject;
  FHintObject := HitTest.HitObject as TcxPCCustomElementViewInfo;
  AHintAreaBounds := cxNullRect;
  if FHintObject <> APrevHintObject then
    FHintHelper.ResetLastHintElement;

  if FHintObject <> nil then
  begin
    AHint := FHintObject.Hint;
    AHintAreaBounds := FHintObject.AbsoluteBounds;
  end;

  if NeedShowHint(AHint) then
    FHintHelper.ShowHint(AHintAreaBounds, AHintAreaBounds, AHint, False, FHintObject);
end;

function TcxCustomTabControlController.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := ViewInfo.CanMouseWheel(MousePos);
  if Result then
    ScrollTabs(1);
end;

function TcxCustomTabControlController.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := ViewInfo.CanMouseWheel(MousePos);
  if Result then
    ScrollTabs(-1);
end;

procedure TcxCustomTabControlController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure NavigatorButtonDown(AButtonInfo: TcxPCCustomHeaderButtonViewInfo);
  begin
    ViewInfo.PressedNavigatorButton := AButtonInfo;
    if AButtonInfo.IsNavigatorButton([nbTopLeft, nbBottomRight]) then
      ArrowButtonMouseDown((AButtonInfo as TcxPCNavigatorButtonViewInfo).ButtonType);
  end;

var
  AButtonInfo: TcxPCCustomHeaderButtonViewInfo;
  AGoDialogJustClosed: Boolean;
begin
  FHintHelper.MouseDown;
  AGoDialogJustClosed := FGoDialogJustClosed;
  FGoDialogJustClosed := False;
  if IControl.IsDesigning or ViewInfo.IsTabsVisible then
    if (Button = mbLeft) then
      if FHitTest.HitAtHeaderButton then
      begin
        AButtonInfo := FHitTest.HitObject as TcxPCCustomHeaderButtonViewInfo;
        if (not AGoDialogJustClosed or not AButtonInfo.IsNavigatorButton([nbGoDialog])) and
          (AButtonInfo.State <> nbsDisabled) then
          NavigatorButtonDown(AButtonInfo)
      end
      else
        if HitTest.HitAtTabButton then
          TabButtonDown(TcxPCCustomTabButtonViewInfo(HitTest.HitObject))
        else
          TabDown(ViewInfo.VisibleIndexOfTabAt(X, Y), Shift)
    else
      if not IControl.IsDesigning and Properties.CloseTabWithMiddleClick and
        (Button = mbMiddle) and FHitTest.HitAtTab then
        Properties.CloseTab(FHitTest.HitTab.Index);
end;

procedure TcxCustomTabControlController.MouseLeave;
begin
  HitTest.Clear;
  if not FIsGoDialogShowing then
    ViewInfo.ResetHotTrack;
  FHintHelper.MouseLeave;
end;

procedure TcxCustomTabControlController.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure CheckTabsScrolling(APrevButtonInfo: TcxPCCustomHeaderButtonViewInfo);
  var
    AButtonInfo: TcxPCCustomHeaderButtonViewInfo;
  begin
    AButtonInfo := ViewInfo.HotTrackNavigatorButton;
    if not IsScrollTimerStarted and (APrevButtonInfo <> AButtonInfo) and
      (AButtonInfo <> nil) and (AButtonInfo.IsNavigatorButton([nbTopLeft, nbBottomRight])) and
      (AButtonInfo.State = nbsPressed) then
        ArrowButtonMouseDown((AButtonInfo as TcxPCNavigatorButtonViewInfo).ButtonType)
  end;

var
  ATabVisibleIndex: Integer;
  APrevButtonInfo: TcxPCCustomHeaderButtonViewInfo;
begin
  FHitTest.Update(Shift, cxPoint(X, Y));
  if not ViewInfo.IsTabsVisible or FIsGoDialogShowing then Exit;
  ATabVisibleIndex := ViewInfo.VisibleIndexOfTabAt(X, Y);
  if (ATabVisibleIndex <> ViewInfo.FPressedTabVisibleIndex) then
    ViewInfo.PressedTabVisibleIndex := -1;
  if IsTabPressable and (ATabVisibleIndex = FMouseDownTabVisibleIndex) then
    ViewInfo.PressedTabVisibleIndex := ATabVisibleIndex;
  APrevButtonInfo := ViewInfo.HotTrackNavigatorButton;
  ViewInfo.SynchronizeHotTrackStates(Shift);
  CheckTabsScrolling(APrevButtonInfo);
  if not IControl.IsDesigning then
    CheckHint;
end;

procedure TcxCustomTabControlController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AButtonClick: Boolean;
begin
  if not ViewInfo.IsTabsVisible or FIsGoDialogShowing then
    Exit;

  if Button = mbLeft then
  begin
    StopScrollTimer;
    AButtonClick := (FHitTest.HitAtHeaderButton and (FHitTest.HitObject = ViewInfo.PressedNavigatorButton)) or
      (HitTest.HitAtTabButton and (FHitTest.HitObject = ViewInfo.PressedTabButton));
    ViewInfo.PressedTabButton := nil;
    ViewInfo.PressedNavigatorButton := nil;
    if AButtonClick then
      (FHitTest.HitObject as TcxPCCustomButtonViewInfo).Click
    else
      TabUp(ViewInfo.VisibleIndexOfTabAt(X, Y), Shift);
  end;
end;

procedure TcxCustomTabControlController.FocusChanged;
begin
  if not IControl.IsDestroying and (ViewInfo.FocusedTabVisibleIndex <> -1) and
    ViewInfo.TabsViewInfo[ViewInfo.FocusedTabVisibleIndex].ActuallyVisible then
      ViewInfo.RepaintTab(ViewInfo.FocusedTabVisibleIndex, tpcFocused);
end;

function TcxCustomTabControlController.HandleDialogChar(Key: Integer): Boolean;
begin
  Result := ViewInfo.IsTabsVisible and ViewInfo.TabsViewInfo.HandleDialogChar(Key);
end;

function TcxCustomTabControlController.KeyDown(var Key: Word; Shift: TShiftState): Boolean;

  function GetCorrectedDelta(ADelta: Integer): Integer;
  const
    DirectionMap: array[Boolean] of Integer = (-1, 1);
  begin
    Result := ADelta * DirectionMap[not ViewInfo.IsSpecialAlignment];
  end;

  function GetNextTabVisibleIndex: Integer;
  const
    DeltaMap: array[Boolean] of Integer = (-1, 1);
  var
    ADelta: Integer;
    ACycle: Boolean;
  begin
    ACycle := False;
    ADelta := 0;
    Result := -1;
    case Key of
      VK_TAB:
        if ssCtrl in Shift then
        begin
          ACycle := True;
          ADelta := DeltaMap[not (ssShift in Shift)];
        end;
      VK_RIGHT:
        ADelta := DeltaMap[not ViewInfo.UseRightToLeftAlignment];
      VK_LEFT:
        ADelta := DeltaMap[ViewInfo.UseRightToLeftAlignment];
      VK_DOWN:
        ADelta := 1;
      VK_UP:
        ADelta := -1;
      VK_HOME, VK_END:
        begin
          ADelta := DeltaMap[Key = VK_END];
          ADelta := GetCorrectedDelta(ADelta);
          if ADelta = 1 then
            Result := ViewInfo.TabsViewInfo.ViewInfoCount - 1
          else
            Result := 0;
          ADelta := -ADelta;
        end;
      VK_RETURN, VK_SPACE:
      begin
        if ViewInfo.TrackingTabVisibleIndex <> -1 then
          Result := ViewInfo.TrackingTabVisibleIndex
        else
          if ViewInfo.FocusedTabVisibleIndex <> ViewInfo.MainTabVisibleIndex then
          begin
            Result := ViewInfo.FocusedTabVisibleIndex;
            ViewInfo.FocusedTabVisibleIndex := -1;
          end;
      end;
    end;
    if ADelta <> 0 then
    begin
      if Result = -1 then
      begin
        ADelta := GetCorrectedDelta(ADelta);
        if ViewInfo.TrackingTabVisibleIndex <> -1 then
          Result := ViewInfo.TrackingTabVisibleIndex
        else
          if ViewInfo.FocusedTabVisibleIndex <> -1 then
            Result := ViewInfo.FocusedTabVisibleIndex
          else
            if ADelta = 1 then
              Result := -1
            else
              Result := ViewInfo.TabsViewInfo.ViewInfoCount;
        Result := ViewInfo.GetNextFocusedTabVisibleIndex(Result, ADelta, ACycle,
          Key in [VK_TAB, VK_RETURN, VK_SPACE]);
      end
      else
        if not ViewInfo.IsTabAccessible(Result) then
          Result := ViewInfo.GetNextFocusedTabVisibleIndex(Result, ADelta, ACycle,
            Key in [VK_TAB, VK_RETURN, VK_SPACE]);
    end;
  end;

var
  ATabVisibleIndex: Integer;
begin
  Result := False;
  if not ViewInfo.IsTabsVisible or
      (Shift = [ssAlt]) or (Shift = [ssAlt, ssShift]) then
    Exit;

  ATabVisibleIndex := GetNextTabVisibleIndex;

  if ATabVisibleIndex <> -1 then
  begin
    if not (IsTabPressable or not Properties.ActivateFocusedTab) or
      (Key in [VK_TAB, VK_RETURN, VK_SPACE]) then
      TabClick(ATabVisibleIndex, [], False)
    else
    begin
      if not IsTabPressable then
        ViewInfo.TabsViewInfo[ATabVisibleIndex].SetFocus
      else
        ViewInfo.TrackingTabVisibleIndex := ATabVisibleIndex;
    end;
    Result := True;
  end;
end;

procedure TcxCustomTabControlController.TabClick(ATabVisibleIndex: Integer; AShift: TShiftState; AMouseEvent: Boolean);
var
  ATabViewInfo: TcxTabViewInfo;
  ATab: TcxTab;
  ALink: TcxObjectLink;
begin
  if ViewInfo.IsTabAccessible(ATabVisibleIndex) then
  begin
    ATab := ViewInfo.TabsViewInfo[ATabVisibleIndex].Tab;
    ALink := cxAddObjectLink(Self);
    try
      if not AMouseEvent or IsTabPressable then
        ViewInfo.TabsViewInfo[ATabVisibleIndex].Select(ssCtrl in AShift);
      if ALink.Ref <> nil then
      begin
        ATabViewInfo := IControl.ViewInfo.GetTabViewInfo(ATab);
        if ATabViewInfo <> nil then
        begin
          ATabViewInfo.SetFocus;
          ATabViewInfo.Click(AShift);
        end;
      end;
    finally
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

procedure TcxCustomTabControlController.DoTabClick(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  Properties.DoTabClick(ATabVisibleIndex, AShift);
end;

procedure TcxCustomTabControlController.TabDown(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  if ViewInfo.IsTabAccessible(ATabVisibleIndex) then
  begin
    FMouseDownTabVisibleIndex := ATabVisibleIndex;
    ViewInfo.TabDown(ATabVisibleIndex, AShift);
  end;
end;

procedure TcxCustomTabControlController.TabUp(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  if (FMouseDownTabVisibleIndex = ATabVisibleIndex) and ViewInfo.IsTabAccessible(ATabVisibleIndex) then
  begin
    FMouseDownTabVisibleIndex := -1;
    if IsTabPressable and not IControl.IsDesigning then
      ViewInfo.TabUp(ATabVisibleIndex, AShift);
    TabClick(ATabVisibleIndex, AShift, True);
  end
  else
    FMouseDownTabVisibleIndex := -1;
end;

procedure TcxCustomTabControlController.TabButtonDown(ATabButtonInfo: TcxPCCustomTabButtonViewInfo);
begin
  if ATabButtonInfo.State <> nbsDisabled then
    ViewInfo.PressedTabButton := ATabButtonInfo
  else
    ViewInfo.PressedTabButton := nil;
end;

function TcxCustomTabControlController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := Properties.AllowTabDragDrop and HitTest.HitAtTab and not HitTest.HitAtTabButton and HitTest.HitTab.CanDrag;
  if Result then
    (IControl.GetDragAndDropObject as TcxTabControlDragAndDropObject).Init(
      TcxTabViewInfo(HitTest.HitObject).VisibleIndex, P);
end;

procedure TcxCustomTabControlController.EndDragAndDrop(Accepted: Boolean);
begin
end;

function TcxCustomTabControlController.GetDragAndDropObjectClass: TcxTabControlDragAndDropObjectClass;
begin
  Result := TcxTabControlDragAndDropObject;
end;

procedure TcxCustomTabControlController.CreateGoDialog;
begin
  if not IControl.IsDesigning then
  begin
    FGoDialog := TcxPCGoDialog.Create(IControl);
    FGoDialog.OnClick := GoDialogClickEventHandler;
  end;
end;

procedure TcxCustomTabControlController.DoShowGoDialog;
var
  P: TPoint;
begin
  P := GetClientToScreen(cxRectLeftBottom(ViewInfo.NavigatorButtonInfoByType[nbGoDialog].Bounds));
  FGoDialog.Popup(P.X, P.Y);
  HideGoDialog(-1);
end;

function TcxCustomTabControlController.GetClientToScreen(const APoint: TPoint): TPoint;
begin
  Result := APoint;
  ClientToScreen(GetControlHandle, Result);
end;

function TcxCustomTabControlController.GetMouseCursorPos: TPoint;
begin
  if GetControlHandle <> 0 then
    Result := GetScreenToClient(cxControls.GetMouseCursorPos)
  else
    Result := cxInvalidPoint;
end;

function TcxCustomTabControlController.GetScreenToClient(const APoint: TPoint): TPoint;
begin
  Result := APoint;
  ScreenToClient(GetControlHandle, Result);
end;

procedure TcxCustomTabControlController.GoDialogClickEventHandler(ATabVisibleIndex: Integer);
begin
  HideGoDialog(ATabVisibleIndex);
end;

procedure TcxCustomTabControlController.HideGoDialog(ATabVisibleIndex: Integer);

  function NeedGoDialogClosed: Boolean;
  var
    AOwnerWnd: THandle;
  begin
    AOwnerWnd := GetControlHandle;
    Result := (AOwnerWnd <> 0) and (dxMessagesController.IsMessageInQueue(AOwnerWnd, WM_LBUTTONDOWN) or
      dxMessagesController.IsMessageInQueue(AOwnerWnd, WM_LBUTTONDBLCLK));
  end;

begin
  FGoDialogJustClosed := NeedGoDialogClosed;
  FIsGoDialogShowing := False;
  TabClick(ATabVisibleIndex, [], False);
  ViewInfo.PressedNavigatorButton := nil;
  ViewInfo.HotTrackNavigatorButton := nil;
  ViewInfo.SynchronizeHotTrackStates(KeyboardStateToShiftState);
end;

procedure TcxCustomTabControlController.ShowGoDialog;
begin
  FIsGoDialogShowing := True;
  DoShowGoDialog;
end;

function TcxCustomTabControlController.GetControlHandle: THandle;
var
  AControl: TWinControl;
begin
  AControl := IControl.GetControl;
  if AControl.HandleAllocated then
    Result := AControl.Handle
  else
    Result := 0;
end;

function TcxCustomTabControlController.GetHintHelperClass: TcxCustomTabControlHintHelperClass;
begin
  Result := TcxCustomTabControlHintHelper;
end;

procedure TcxCustomTabControlController.ArrowButtonMouseDown(ANavigatorButton: TcxPCNavigatorButton);
begin
  ArrowButtonClick(ANavigatorButton);
  if (ViewInfo.PressedNavigatorButton <> nil) and
    ViewInfo.PressedNavigatorButton.IsNavigatorButton([ANavigatorButton]) then
    CreateScrollingTimer;
end;

procedure TcxCustomTabControlController.ArrowButtonClick(ANavigatorButton: TcxPCNavigatorButton);
var
  AButtonInfo: TcxPCNavigatorButtonViewInfo;
  AWasPressed: Boolean;
begin
  if ViewInfo.ArrowButtonClick(ANavigatorButton) then
  begin
    AWasPressed := ViewInfo.PressedNavigatorButton <> nil;
    IControl.RequestLayout;
    if AWasPressed then
    begin
      AButtonInfo := ViewInfo.NavigatorButtonInfoByType[ANavigatorButton];
      if (AButtonInfo <> nil) and (AButtonInfo.State <> nbsDisabled) then
      begin
        ViewInfo.FHotTrackNavigatorButton := nil;
        ViewInfo.FPressedNavigatorButton := AButtonInfo;
      end;
      ViewInfo.SynchronizeHotTrackStates(KeyboardStateToShiftState);
    end;
  end;
end;

procedure TcxCustomTabControlController.CloseButtonClick;
begin
  Properties.CloseActiveTab;
end;

procedure TcxCustomTabControlController.CreateScrollingTimer;
begin
  if FTimer = nil then
    FTimer := cxCreateTimer(TimerEventHandler, TabScrollingStartDelay);
  FTimer.Interval := TabScrollingStartDelay;
  StartScrollTimer;
end;

function TcxCustomTabControlController.GetProperties: TcxCustomTabControlProperties;
begin
  Result := IControl.Properties;
end;

function TcxCustomTabControlController.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := IControl.ViewInfo;
end;

function TcxCustomTabControlController.IsScrollTimerStarted: Boolean;
begin
  Result := (FTimer <> nil) and FTimer.Enabled;
end;

function TcxCustomTabControlController.IsTabPressable: Boolean;
begin
  Result := ViewInfo.Painter.IsTabPressable;
end;

procedure TcxCustomTabControlController.StartScrollTimer;
begin
  if FTimer <> nil then
    FTimer.Enabled := True;
end;

procedure TcxCustomTabControlController.StopScrollTimer;
begin
  if FTimer <> nil then
    FTimer.Enabled := False;
end;

procedure TcxCustomTabControlController.TimerEventHandler(Sender: TObject);
var
  AArrowButtonInfo: TcxPCNavigatorButtonViewInfo;
begin
  FTimer.Interval := TabScrollingDelay;
  AArrowButtonInfo := ViewInfo.FPressedNavigatorButton as TcxPCNavigatorButtonViewInfo;
  if (AArrowButtonInfo <> nil) and (AArrowButtonInfo.State = nbsPressed) then
    ArrowButtonClick(AArrowButtonInfo.ButtonType)
  else
    StopScrollTimer;
end;

{ TcxPCCustomButton }

constructor TcxPCButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FPosition := pcbpHeader;
  FVisible := True;
  FTabImageIndex := -1;
  FHeaderImageIndex := -1;
end;

procedure TcxPCButton.Assign(Source: TPersistent);
var
  ASource: TcxPCButton;
begin
  if Source is TcxPCButton then
  begin
    ASource := TcxPCButton(Source);
    Enabled := ASource.Enabled;
    Hint := ASource.Hint;
    TabImageIndex := ASource.TabImageIndex;
    HeaderImageIndex := ASource.HeaderImageIndex;
    Position := ASource.Position;
    Visible := ASource.Visible;
  end
  else
    inherited Assign(Source);
end;

function TcxPCButton.DoCanShow(ATabIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanShow) then
    FOnCanShow(Self, ATabIndex, Result);
end;

procedure TcxPCButton.DoClick;
begin
  dxCallNotify(FOnClick, Self);
end;

procedure TcxPCButton.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Changed(False);
  end;
end;

procedure TcxPCButton.SetHeaderImageIndex(AValue: TcxImageIndex);
begin
  if FHeaderImageIndex <> AValue then
  begin
    FHeaderImageIndex := AValue;
    Changed(True);
  end;
end;

procedure TcxPCButton.SetHint(const AValue: string);
begin
  if FHint <> AValue then
  begin
    FHint := AValue;
    Changed(False);
  end;
end;

procedure TcxPCButton.SetPosition(AValue: TcxPCButtonPosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    Changed(True);
  end;
end;

procedure TcxPCButton.SetTabImageIndex(AValue: TcxImageIndex);
begin
  if FTabImageIndex <> AValue then
  begin
    FTabImageIndex := AValue;
    Changed(True);
  end;
end;

procedure TcxPCButton.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

{ TcxPCCustomButtons }

function TcxPCButtons.Add: TcxPCButton;
begin
  BeginUpdate;
  try
    Result := TcxPCButton(inherited Add);
  finally
    EndUpdate;
  end;
end;

procedure TcxPCButtons.Update(Item: TCollectionItem);
begin
  (Owner as TcxPCCustomButtons).Changed;
end;

function TcxPCButtons.GetItem(Index: Integer): TcxPCButton;
begin
  Result := TcxPCButton(inherited GetItem(Index));
end;

procedure TcxPCButtons.SetItem(Index: Integer;
  Value: TcxPCButton);
begin
  inherited SetItem(Index, Value);
end;

{ TcxPCCustomButtons }

constructor TcxPCCustomButtons.Create(AOwner: TPersistent);
begin
  inherited;
  FProperties := AOwner as TcxCustomTabControlProperties;
  FButtons := CreateButtons;
  FTabImages := TcxPCImageList.Create(FProperties);
  FTabImages.OnChange := FProperties.ImageListChange;
  FHeaderImages := TcxPCImageList.Create(FProperties);
  FHeaderImages.OnChange := FProperties.ImageListChange;
end;

destructor TcxPCCustomButtons.Destroy;
begin
  FreeAndNil(FHeaderImages);
  FreeAndNil(FTabImages);
  FreeAndNil(FButtons);
  inherited;
end;

procedure TcxPCCustomButtons.Changed;
begin
  FProperties.Changed;
end;

function TcxPCCustomButtons.CreateButtons: TcxPCButtons;
begin
  Result := TcxPCButtons.Create(Self, TcxPCButton);
end;

procedure TcxPCCustomButtons.DoAssign(Source: TPersistent);
var
  ASource: TcxPCCustomButtons;
begin
  inherited DoAssign(Source);
  if Source is TcxPCCustomButtons then
  begin
    ASource := TcxPCCustomButtons(Source);
    FProperties.BeginUpdate;
    try
      Buttons := ASource.Buttons;
      HeaderImages := ASource.HeaderImages;
      TabImages := ASource.TabImages;
      Mode := ASource.Mode;
    finally
      FProperties.EndUpdate;
    end;
  end;
end;

function TcxPCCustomButtons.GetCount: Integer;
begin
  Result := Buttons.Count;
end;

function TcxPCCustomButtons.GetHeaderImages: TCustomImageList;
begin
  Result := FHeaderImages.BaseImages;
end;

function TcxPCCustomButtons.GetTabImages: TCustomImageList;
begin
  Result := FTabImages.BaseImages;
end;

procedure TcxPCCustomButtons.SetButtons(AValue: TcxPCButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TcxPCCustomButtons.SetHeaderImages(AValue: TCustomImageList);
begin
  FHeaderImages.BaseImages := AValue;
end;

procedure TcxPCCustomButtons.SetMode(AValue: TcxPCButtonMode);
begin
  if FMode <> AValue then
  begin
    FMode := AValue;
    Changed;
  end;
end;

procedure TcxPCCustomButtons.SetTabImages(AValue: TCustomImageList);
begin
 FTabImages.BaseImages := AValue;
end;

{ TcxCustomTabControlProperties }

constructor TcxCustomTabControlProperties.Create(AOwner: TPersistent);
begin
  inherited;

  FOptions := cxPCDefaultOptions;
  FTabIndex := -1;
  FImageBorder := 0;
  FNavigatorPosition := npRightTop;
  FTabCaptionAlignment := taCenter;
  FActivateFocusedTab := True;
  FTabsScroll := True;

  FTabs := TcxTabs.Create(Self);

  FImages := TcxPCImageList.Create(Self);
  FImages.OnChange := ImageListChange;

  FTabSlants := TcxTabSlants.Create(Self);
  FTabSlants.OnChange := TabSlantsChangedHandler;

  FDragImage := TdxSmartGlyph.Create;
  FCustomButtons := CreateCustomButtons;
  FLockedEvents := TObjectList.Create;
end;

destructor TcxCustomTabControlProperties.Destroy;
begin
  FreeAndNil(FLockedEvents);
  FreeAndNil(FCustomButtons);
  FreeAndNil(FDragImage);
  FreeAndNil(FTabSlants);
  FreeAndNil(FImages);
  FreeAndNil(FTabs);
  inherited;
end;

procedure TcxCustomTabControlProperties.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomTabControlProperties.BeginUpdate;
begin
  Inc(FUpdateLockCount);
end;

procedure TcxCustomTabControlProperties.CancelUpdate;
begin
  Dec(FUpdateLockCount);
end;

procedure TcxCustomTabControlProperties.EndUpdate;
begin
  Dec(FUpdateLockCount);
  Changed;
end;

procedure TcxCustomTabControlProperties.DoAssign(Source: TPersistent);
var
  ASourceProperties: TcxCustomTabControlProperties;
begin
  inherited DoAssign(Source);
  if Source is TcxCustomTabControlProperties then
  begin
    ASourceProperties :=  TcxCustomTabControlProperties(Source);
    ActivateFocusedTab := ASourceProperties.ActivateFocusedTab;
    AllowDisabledTabAccess := ASourceProperties.AllowDisabledTabAccess;
    AllowTabDragDrop := ASourceProperties.AllowTabDragDrop;
    CloseButtonMode := ASourceProperties.CloseButtonMode;
    CloseTabWithMiddleClick := ASourceProperties.CloseTabWithMiddleClick;
    CustomButtons := ASourceProperties.CustomButtons;
    HideTabs := ASourceProperties.HideTabs;
    HotTrack := ASourceProperties.HotTrack;
    ImageBorder := ASourceProperties.ImageBorder;
    IsTabsContainer := ASourceProperties.IsTabsContainer;
    MultiLine := ASourceProperties.MultiLine;
    NavigatorPosition := ASourceProperties.NavigatorPosition;
    NewButtonMode := ASourceProperties.NewButtonMode;
    Options := ASourceProperties.Options;
    OwnerDraw := ASourceProperties.OwnerDraw;
    RaggedRight := ASourceProperties.RaggedRight;
    Rotate := ASourceProperties.Rotate;
    RotatedTabsMaxWidth := ASourceProperties.RotatedTabsMaxWidth;
    ScrollOpposite := ASourceProperties.ScrollOpposite;
    ShowFrame := ASourceProperties.ShowFrame;
    Style := ASourceProperties.Style;
    TabCaptionAlignment := ASourceProperties.TabCaptionAlignment;
    TabHeight := ASourceProperties.TabHeight;
    TabIndex := ASourceProperties.TabIndex;
    TabPosition := ASourceProperties.TabPosition;
    TabSlants := ASourceProperties.TabSlants;
    TabsScroll := ASourceProperties.TabsScroll;
    TabWidth := ASourceProperties.TabWidth;
  end;
end;

type
  TEventInfo = class
  public
    Event: TNotifyEvent;
    Sender: TObject;
  end;

procedure TcxCustomTabControlProperties.AddDeferredEvent(AEvent: TNotifyEvent; ASender: TObject);
var
  AEventInfo: TEventInfo;
begin
  AEventInfo := TEventInfo.Create;
  AEventInfo.Event := AEvent;
  AEventInfo.Sender := ASender;
  FLockedEvents.Add(AEventInfo)
end;

procedure TcxCustomTabControlProperties.CallLockedEvents;
var
  I: Integer;
begin
  for I := 0 to FLockedEvents.Count - 1 do
    CallNotify(TEventInfo(FLockedEvents[I]).Event, TEventInfo(FLockedEvents[I]).Sender);
  FLockedEvents.Clear;
end;

function TcxCustomTabControlProperties.IsChangeEventLocked: Boolean;
begin
  Result := FChangeEventLockCount > 0;
end;

function TcxCustomTabControlProperties.IsChangingEventLocked: Boolean;
begin
  Result := (FChangingEventLockCount > 0) or IsUpdateLocked;
end;

function TcxCustomTabControlProperties.IsNotifyEventLocked: Boolean;
begin
  Result := FNotifyEventLockCount > 0;
end;

procedure TcxCustomTabControlProperties.LockChangingEvent;
begin
  Inc(FChangingEventLockCount);
end;

procedure TcxCustomTabControlProperties.UnlockChangingEvent;
begin
  Dec(FChangingEventLockCount);
end;

procedure TcxCustomTabControlProperties.LockNotifyEvents;
begin
  Inc(FNotifyEventLockCount)
end;

procedure TcxCustomTabControlProperties.UnLockNotifyEvents;
begin
  if FNotifyEventLockCount = 1 then
    CallLockedEvents;
  Dec(FNotifyEventLockCount);
end;

function TcxCustomTabControlProperties.CanChange(NewTabIndex: Integer): Boolean;
begin
  Result := not FTabChanging;
  if Result then
  begin
    FTabChanging := True;
    try
      Result := DoCanChange(NewTabIndex);
    finally
      FTabChanging := False;
    end;
  end;
end;

function TcxCustomTabControlProperties.CanCloseButtonShow(ATab: TcxTab): Boolean;
begin
  Result := ATab.AllowCloseButton;
end;

procedure TcxCustomTabControlProperties.Changed(AType: TcxCustomTabControlPropertiesChangedType = pctHard);
begin
  if not IsUpdateLocked then
    DoChanged(AType);
end;

procedure TcxCustomTabControlProperties.ChangeScale(M, D: Integer);
begin
  ImageBorder := MulDiv(ImageBorder, M, D);
  TabHeight := MulDiv(TabHeight, M, D);
  TabWidth := MulDiv(TabWidth, M, D);
end;

function TcxCustomTabControlProperties.DoCanChange(ANewTabIndex: Integer): Boolean;
begin
  Result := True;
  if not IsChangingEventLocked then
    DoChanging(ANewTabIndex, Result);
end;

procedure TcxCustomTabControlProperties.DoChange;
begin
  if Assigned(FOnChange) and not IsUpdateLocked then
    FOnChange(Self);
end;

procedure TcxCustomTabControlProperties.DoChanging(ANewTabIndex: Integer; var AAllowChange: Boolean);
begin
  FNewTabIndex := ANewTabIndex;
  if Assigned(FOnChanging) then
    FOnChanging(Self, AAllowChange);
end;

procedure TcxCustomTabControlProperties.NewButtonClick;
var
  AIndex: Integer;
begin
  if not DoNewTabButtonClick then
  begin
    AIndex := CreateNewTab;
    TabIndex := AIndex;
    DoNewTabCreate(AIndex);
  end;
end;

procedure TcxCustomTabControlProperties.CloseActiveTab;
begin
  CloseTab(TabIndex);
end;

function TcxCustomTabControlProperties.CanProcessChanged: Boolean;
begin
  Result := not (IsDestroying or IsLoading);
end;

procedure TcxCustomTabControlProperties.CloseTab(AIndex: Integer);
begin
  if DoCanClose(AIndex) then
  begin
    LockChangingEvent;
    try
      DoCloseTab(AIndex)
    finally
      UnlockChangingEvent;
    end;
  end;
end;

function TcxCustomTabControlProperties.CreateCustomButtons: TcxPCCustomButtons;
begin
  Result := TcxPCCustomButtons.Create(Self);
end;

function TcxCustomTabControlProperties.CreateNewTab: Integer;
begin
  Result := Tabs.Add('');
end;

procedure TcxCustomTabControlProperties.DoChanged(AType: TcxCustomTabControlPropertiesChangedType = pctHard);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, AType);
end;

function TcxCustomTabControlProperties.DoPaintDragImage(AImage: TcxDragImage): Boolean;
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    Result := False;
    if Assigned(FOnPaintDragImage) then
      FOnPaintDragImage(Self, ABitmap, Result);
    if not Result and IsGlyphAssigned(FDragImage) then
    begin
      ABitmap.Assign(FDragImage);
      Result := True;
    end;
    if Result then
    begin
      AImage.SetBounds(0, 0, ABitmap.Width, ABitmap.Height);
      TcxDragImageAccess(AImage).TransparentColorValue := ABitmap.TransparentColor;
      TcxDragImageAccess(AImage).TransparentColor := True;
      cxBitBlt(AImage.Canvas.Handle, ABitmap.Canvas.Handle, AImage.ClientRect, cxNullPoint, SRCCOPY);
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure TcxCustomTabControlProperties.DoPrepareTabCanvasFont(ATab: TcxTab; ACanvas: TcxCanvas);
begin
  if Assigned(OnPrepareTabCanvasFont) then
    OnPrepareTabCanvasFont(ATab, ACanvas);
end;

procedure TcxCustomTabControlProperties.DoClose(ATabIndex: Integer);
begin
  if Assigned(FOnClose) then
    FOnClose(Self, ATabIndex);
end;

procedure TcxCustomTabControlProperties.DoCloseButtonClick(ATabIndex: Integer);
begin
  CloseTab(ATabIndex);
end;

procedure TcxCustomTabControlProperties.DoGetTabHint(ATab: TcxTab;
  var AHint: string; var ACanShow: Boolean);
begin
  if Assigned(OnGetTabHint) then
    OnGetTabHint(Self, ATab.Index, AHint, ACanShow);
end;

function TcxCustomTabControlProperties.DoCustomDraw(TabVisibleIndex: Integer): Boolean;
begin
  Result := OwnerDraw and Assigned(OnDrawTab);
  if Result then
  begin
    OnDrawTab(Self, Tabs.VisibleTabs[TabVisibleIndex], Result);
    Result := not Result;
  end;
end;

procedure TcxCustomTabControlProperties.DoStyleChanged;
begin
  if Assigned(FOnStyleChanged) then
    FOnStyleChanged(Self);
end;

procedure TcxCustomTabControlProperties.DoCloseTab(AIndex: Integer);
var
  ATab: TcxTab;
begin
  BeginUpdate;
  try
    if AIndex <> TabIndex then
      ATab := Tabs[TabIndex]
    else
      ATab := nil;
    Tabs.Delete(AIndex);
    if ATab <> nil then
      TabIndex := ATab.Index
    else
      TabIndex := Min(AIndex, Tabs.Count - 1);
    DoClose(AIndex);
  finally
    EndUpdate;
  end;
  if ATab = nil then
    DoChange;
end;

procedure TcxCustomTabControlProperties.MoveTab(ACurrentIndex, ANewIndex: Integer);
begin
  Tabs.Move(ACurrentIndex, ANewIndex);
end;

procedure TcxCustomTabControlProperties.SetModified;
var
  AIntf: IcxTabControl;
begin
  if Supports(Owner, IcxTabControl, AIntf) then
    AIntf.SetModified;
end;

function TcxCustomTabControlProperties.DoCanClose(AIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanClose) then
    FOnCanClose(Self, AIndex, Result);
end;

function TcxCustomTabControlProperties.DoNewTabButtonClick: Boolean;
begin
  Result := False;
  if Assigned(FOnNewTabButtonClick) then
    FOnNewTabButtonClick(Self, Result);
end;

procedure TcxCustomTabControlProperties.DoNewTabCreate(AIndex: Integer);
begin
  if Assigned(FOnNewTabCreate) then
    FOnNewTabCreate(Self, AIndex);
end;

procedure TcxCustomTabControlProperties.DoSetTabIndex(Value: Integer);
begin
  FTabIndex := Value;
end;

procedure TcxCustomTabControlProperties.DoDrawTabEx(ATabVisibleIndex: Integer; AFont: TFont);
begin
  if Assigned(FOnDrawTabEx) then
    FOnDrawTabEx(Self, Tabs.VisibleTabs[ATabVisibleIndex], AFont);
end;

procedure TcxCustomTabControlProperties.DoTabClick(ATabVisibleIndex: Integer; AShift: TShiftState);
begin
  if Assigned(OnTabClick) then
    OnTabClick(Self, ATabVisibleIndex, AShift);
end;

function TcxCustomTabControlProperties.GetTabIndex: Integer;
begin
  Result := FTabIndex;
end;

procedure TcxCustomTabControlProperties.TabIndexChanged;
begin
  DoChange;
end;

function TcxCustomTabControlProperties.IsUpdateLocked: Boolean;
begin
  Result := (FUpdateLockCount > 0) or IsLoading;
end;

procedure TcxCustomTabControlProperties.LockChangeEvent;
begin
  Inc(FChangeEventLockCount);
end;

procedure TcxCustomTabControlProperties.UnlockChangeEvent;
begin
  Dec(FChangeEventLockCount);
end;

function TcxCustomTabControlProperties.IsDesigning: Boolean;
var
  AIntf: IcxControlComponentState;
begin
  Result := Supports(Owner, IcxControlComponentState, AIntf) and AIntf.IsDesigning;
end;

function TcxCustomTabControlProperties.IsDestroying: Boolean;
var
  AIntf: IcxControlComponentState;
begin
  Result := Supports(Owner, IcxControlComponentState, AIntf) and AIntf.IsDestroying;
end;

function TcxCustomTabControlProperties.IsLoading: Boolean;
var
  AIntf: IcxControlComponentState;
begin
  Result := Supports(Owner, IcxControlComponentState, AIntf) and AIntf.IsLoading;
end;

function TcxCustomTabControlProperties.GetTabControl: IcxTabControl;
begin
  Supports(Owner, IcxTabControl, Result);
end;

function TcxCustomTabControlProperties.GetViewInfo: TcxCustomTabControlViewInfo;
var
  AIntf: IcxTabControl;
begin
  AIntf := GetTabControl;
  if AIntf <> nil then
    Result := AIntf.ViewInfo
  else
    Result := nil;
end;

procedure TcxCustomTabControlProperties.ImageListChange(Sender: TObject);
begin
  Changed;
end;

function TcxCustomTabControlProperties.InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string;
begin
  Result := ATab.Caption;
  if ATab.GetViewInfo <> nil then
    ACanShow := ATab.GetViewInfo.FIsTextTooLong
  else
    ACanShow := False;
end;

function TcxCustomTabControlProperties.GetButtonHint(AButtonViewInfo: TcxPCCustomButtonViewInfo): string;
var
  AHitTestIndex: Integer;
begin
  if not ShowButtonHints then
  begin
    Result := '';
    Exit;
  end;
  AHitTestIndex := AButtonViewInfo.GetHitTestIndex;
  case AHitTestIndex of
    pchtTabCloseButton:
      Result := cxGetResourceString(@scxPCDefaultHintTabCloseButton);
    pchtTabButton:
      Result := TcxPCTabButtonViewInfo(AButtonViewInfo).Button.Hint;
    pchtHeaderButton:
      Result := TcxPCHeaderButtonViewInfo(AButtonViewInfo).Button.Hint;
  else  // pchtNavigatorButton
    case TcxPCNavigatorButtonViewInfo(AButtonViewInfo).ButtonType of
      nbTopLeft:
        Result := cxGetResourceString(@scxPCDefaultHintTopLeftButton);
      nbBottomRight:
        Result := cxGetResourceString(@scxPCDefaultHintBottomRightButton);
      nbGoDialog:
        Result := cxGetResourceString(@scxPCDefaultHintGoDialogButton);
    else  // nbClose
      Result := cxGetResourceString(@scxPCDefaultHintCloseButton);
    end;
  end;
end;

function TcxCustomTabControlProperties.GetImageIndex(ATab: TcxTab): Integer;
begin
  Result := ATab.FImageIndex;
  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self, ATab.Index, Result);
  if not IsImageAssigned(Images, Result) and not IsImageAssigned(HotImages, Result) then
    Result := -1;
end;

function TcxCustomTabControlProperties.GetNavigatorButtons(AOnlyObligatoryButtons: Boolean): TcxPCNavigatorButtons;
begin
  Result := [];
  if not MultiLine then
  begin
    if pcoCloseButton in Options then
      Include(Result, nbClose);
    if not AOnlyObligatoryButtons and not (pcoNoArrows in Options) then
      Result := Result + [nbTopLeft, nbBottomRight];
    if (pcoGoDialog in Options) and
       (not AOnlyObligatoryButtons or (pcoAlwaysShowGoDialogButton in Options))
    then
      Include(Result, nbGoDialog);
  end;
end;

function TcxCustomTabControlProperties.GetTabHint(ATab: TcxTab): string;
var
  ACanShow: Boolean;
begin
  if not ShowTabHints then
  begin
    Result := '';
    Exit;
  end;
  ACanShow := True;
  if ATab.IsNewButton then
    Result := cxGetResourceString(@scxPCDefaultHintNewButton)
  else
    Result := InternalGetTabHint(ATab, ACanShow);
  DoGetTabHint(ATab, Result, ACanShow);
  if not ACanShow then
    Result := '';
end;

function TcxCustomTabControlProperties.TabIndexTabMustBeVisible: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTabControlProperties.TabSlantsChangedHandler(Sender: TObject);
begin
  Changed;
end;

function TcxCustomTabControlProperties.GetNewButton: TcxPCNewButton;
begin
  Result := Tabs.NewButton;
end;

function TcxCustomTabControlProperties.GetNewButtonMode: TcxPCNewButtonMode;
const
  ResultMap: array[Boolean] of TcxPCNewButtonMode = (nbmNone, nbmTab);
begin
  Result := ResultMap[NewButton.Visible];
end;

function TcxCustomTabControlProperties.GetHeaderButtonImages: TCustomImageList;
begin
  Result := FCustomButtons.HeaderImages;
end;

function TcxCustomTabControlProperties.GetHotImages: TCustomImageList;
begin
  Result := FImages.BaseHotImages;
end;

function TcxCustomTabControlProperties.GetImages: TCustomImageList;
begin
  Result := FImages.BaseImages;
end;

function TcxCustomTabControlProperties.GetOptions: TcxPCOptions;
begin
  Result := FOptions;
end;

function TcxCustomTabControlProperties.GetTabButtonImages: TCustomImageList;
begin
  Result := FCustomButtons.TabImages;
end;

procedure TcxCustomTabControlProperties.SetActivateFocusedTab(Value: Boolean);
begin
  if FActivateFocusedTab <> Value then
  begin
    FActivateFocusedTab := Value;
    if FActivateFocusedTab then
      Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetAllowDisabledTabAccess(
  const Value: Boolean);
begin
  if FAllowDisabledTabAccess <> Value then
  begin
    FAllowDisabledTabAccess := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetCloseButtonMode(
  const Value: TcxPCButtonMode);
begin
  if FCloseButtonMode <> Value then
  begin
    FCloseButtonMode := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetCustomButtons(
  AValue: TcxPCCustomButtons);
begin
  FCustomButtons.Assign(AValue);
end;

procedure TcxCustomTabControlProperties.SetDragImage(const Value: TdxSmartGlyph);
begin
  FDragImage.Assign(Value);
end;

procedure TcxCustomTabControlProperties.SetHideTabs(Value: Boolean);
begin
  if FHideTabs <> Value then
  begin
    FHideTabs := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetHotImages(Value: TCustomImageList);
begin
  FImages.BaseHotImages := Value;
end;

procedure TcxCustomTabControlProperties.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetImageBorder(Value: Integer);
begin
  if (Value >= 0) and (FImageBorder <> Value) then
  begin
    FImageBorder := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetImages(const Value: TCustomImageList);
begin
  FImages.BaseImages := Value;
end;

procedure TcxCustomTabControlProperties.SetIsTabsContainer(Value: Boolean);
begin
  if FIsTabsContainer <> Value then
  begin
    FIsTabsContainer := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetRotatedTabsMaxWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FRotatedTabsMaxWidth <> Value then
  begin
    FRotatedTabsMaxWidth := Value;
    Changed(pctMedium);
  end;
end;

procedure TcxCustomTabControlProperties.SetMultiLine(Value: Boolean);
begin
  if MultiLine <> Value then
  begin
    FMultiLine := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetMultiLineTabCaptions(Value: Boolean);
begin
  if MultiLineTabCaptions <> Value then
  begin
    FMultiLineTabCaptions := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetNavigatorPosition(const Value: TcxPCNavigatorPosition);
begin
  if FNavigatorPosition <> Value then
  begin
    FNavigatorPosition := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetNewButtonMode(Value: TcxPCNewButtonMode);
begin
  NewButton.Visible := Value = nbmTab;
end;

procedure TcxCustomTabControlProperties.SetOptions(Value: TcxPCOptions);
const
  GraphicOptionsMap: TcxPCOptions = [pcoGradient, pcoGradientClientArea,
    pcoUsePageColorForTab];
  LayoutOptionsMap: TcxPCOptions = [
    pcoAlwaysShowGoDialogButton, pcoCloseButton, pcoFixedTabWidthWhenRotated,
    pcoGoDialog, pcoGradient, pcoGradientClientArea, pcoNoArrows,
    pcoTopToBottomText
  ];
var
  AAddOptions, AChangedOptions, ASubOptions: TcxPCOptions;
begin
  AAddOptions := Value - FOptions;
  ASubOptions := FOptions - Value;
  AChangedOptions := AAddOptions + ASubOptions;
  if AChangedOptions <> [] then
  begin
    FOptions := Value;
    if AChangedOptions * LayoutOptionsMap <> [] then
      Changed
    else
      if AChangedOptions * GraphicOptionsMap <> [] then
        Changed(pctLight);
  end;
end;

procedure TcxCustomTabControlProperties.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    Changed(pctLight);
  end;
end;

procedure TcxCustomTabControlProperties.SetRaggedRight(Value: Boolean);
begin
  if FRaggedRight <> Value then
  begin
    FRaggedRight := Value;
    if MultiLine then
      Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetRotate(Value: Boolean);
begin
  if Rotate <> Value then
  begin
    FRotate := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetScrollOpposite(Value: Boolean);
begin
  if FScrollOpposite <> Value then
  begin
    FScrollOpposite := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetShowFrame(Value: Boolean);
begin
  if FShowFrame <> Value then
  begin
    FShowFrame := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetStyle(
  const Value: TcxPCStyleID);
begin
  if FStyle <> Value then
  begin
    if PaintersFactory.GetPainterClass(Value) = nil then
      FStyle := cxPCDefaultStyle
    else
      FStyle := Value;
    DoStyleChanged;
  end;
end;

procedure TcxCustomTabControlProperties.SetTabCaptionAlignment(Value: TAlignment);
begin
  if FTabCaptionAlignment <> Value then
  begin
    FTabCaptionAlignment := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetTabHeight(Value: Smallint);
begin
  if FTabSize.Y <> Value then
  begin
    FTabSize.Y := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetTabIndex(Value: Integer);

  procedure UnselectTabs;
  var
    I: Integer;
  begin
    for I := 0 to Tabs.Count - 1 do
      Tabs[I].Selected := False;
  end;

begin
  if IsLoading then
  begin
    FTabIndex := Value;
    Exit;
  end;
  if (Value <> -1) and (Value < 0) or (Value >= Tabs.Count) then
    Exit;
  if (Value <> -1) and TabIndexTabMustBeVisible and (not Tabs[Value].Visible) then
    Value := -1;
  if Value = TabIndex then
    Exit;
  if not CanChange(Value) then
    FTabIndex := Min(FTabIndex, Tabs.Count - 1)
  else
  begin
    UnselectTabs;
    DoSetTabIndex(Value);
    SetModified;
    TabIndexChanged;
    Changed(pctMedium);
  end;
end;

procedure TcxCustomTabControlProperties.SetTabs(const Value: TcxTabs);
begin
  FTabs.Assign(Value);
end;

procedure TcxCustomTabControlProperties.SetTabsScroll(Value: Boolean);
begin
  if Value <> TabsScroll then
  begin
    FTabsScroll := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetTabSlants(Value: TcxTabSlants);
begin
  FTabSlants.Assign(Value);
end;

procedure TcxCustomTabControlProperties.SetTabPosition(Value: TcxTabPosition);
begin
  if TabPosition <> Value then
  begin
    FTabPosition := Value;
    Changed;
  end;
end;

procedure TcxCustomTabControlProperties.SetTabWidth(const Value: Smallint);
begin
  if FTabSize.X <> Value then
  begin
    FTabSize.X := Value;
    Changed;
  end;
end;

{ TcxCustomTabControl }

function TcxCustomTabControl.GetPageClientRect: TRect;
begin
  Result := FPainter.GetPageClientRect;
end;

function TcxCustomTabControl.GetPageClientRectOffset: TRect;
begin
  Result := Painter.GetPageClientRectOffset;
end;

function TcxCustomTabControl.GetImages: TCustomImageList;
begin
  Result := Properties.Images;
end;

function TcxCustomTabControl.GetIsTabsContainer: Boolean;
begin
  Result := Properties.IsTabsContainer;
end;

function TcxCustomTabControl.GetMainTabIndex: Integer;
begin
  Result := ViewInfo.MainTabIndex;
end;

function TcxCustomTabControl.GetHotTrack: Boolean;
begin
  Result := Properties.HotTrack;
end;

function TcxCustomTabControl.GetImageBorder: Integer;
begin
  Result := Properties.ImageBorder;
end;

function TcxCustomTabControl.GetFirstVisibleTab: Integer;
begin
  Result := ViewInfo.FirstTabVisibleIndex;
end;

function TcxCustomTabControl.GetHideTabs: Boolean;
begin
  Result := Properties.HideTabs;
end;

function TcxCustomTabControl.GetMaxRotatedTabWidth: Integer;
begin
  Result := Properties.RotatedTabsMaxWidth;
end;

function TcxCustomTabControl.GetOptions: TcxPCOptions;
begin
  Result := Properties.Options;
end;

function TcxCustomTabControl.GetOwnerDraw: Boolean;
begin
  Result := Properties.OwnerDraw;
end;

function TcxCustomTabControl.GetRaggedRight: Boolean;
begin
  Result := Properties.RaggedRight;
end;

function TcxCustomTabControl.GetStyle: TcxPCStyleID;
begin
  Result := Properties.Style;
end;

function TcxCustomTabControl.GetTabs: TcxTabs;
begin
  Result := Properties.Tabs;
end;

function TcxCustomTabControl.InternalGetClientRect: TRect;
begin
  if IsLoading then
    if FIsClientRectLoaded then
      Result := FClientRect
    else
      Result := GetControlRect(Self)
  else
  begin
    Result := PageClientRect;
    ValidateRect(Result);
  end;
end;

procedure TcxCustomTabControl.ReadClientRectBottom(Reader: TReader);
begin
  FClientRect.Bottom := Reader.ReadInteger;
  Include(FScalingFlags, sfHeight);
end;

procedure TcxCustomTabControl.ReadClientRectLeft(Reader: TReader);
begin
  FClientRect.Left := Reader.ReadInteger;
  Include(FScalingFlags, sfLeft);
end;

procedure TcxCustomTabControl.ReadClientRectRight(Reader: TReader);
begin
  FClientRect.Right := Reader.ReadInteger;
  Include(FScalingFlags, sfWidth);
end;

procedure TcxCustomTabControl.ReadClientRectTop(Reader: TReader);
begin
  FClientRect.Top := Reader.ReadInteger;
  Include(FScalingFlags, sfTop);
end;

procedure TcxCustomTabControl.SetFirstVisibleTab(Value: Integer);
begin
  ViewInfo.FirstTabVisibleIndex := Value;
end;

procedure TcxCustomTabControl.SetHideTabs(const Value: Boolean);
begin
  Properties.HideTabs := Value;
end;

procedure TcxCustomTabControl.SetHotTrack(Value: Boolean);
begin
  Properties.HotTrack := Value;
end;

procedure TcxCustomTabControl.SetImageBorder(const Value: Integer);
begin
  Properties.ImageBorder := Value;
end;

procedure TcxCustomTabControl.SetImages(const Value: TCustomImageList);
begin
  Properties.Images := Value;
end;

procedure TcxCustomTabControl.SetIsTabsContainer(Value: Boolean);
begin
  Properties.IsTabsContainer := Value;
end;

procedure TcxCustomTabControl.SetMultiLine(const Value: Boolean);
begin
  Properties.MultiLine := Value;
end;

procedure TcxCustomTabControl.SetMultiSelect(const Value: Boolean);
begin
  Properties.MultiSelect := Value;
end;

procedure TcxCustomTabControl.SetNavigatorPosition(
  const Value: TcxPCNavigatorPosition);
begin
  Properties.NavigatorPosition := Value;
end;

procedure TcxCustomTabControl.SetOnNew(const Value: TcxPCNewEvent);
begin
  FOnNewTabCreate := Value;
end;

procedure TcxCustomTabControl.SetOptions(Value: TcxPCOptions);
begin
  Properties.Options := Value;
end;

procedure TcxCustomTabControl.SetOwnerDraw(const Value: Boolean);
begin
  Properties.OwnerDraw := Value;
end;

procedure TcxCustomTabControl.SetProperties(const Value: TcxCustomTabControlProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomTabControl.SetRaggedRight(const Value: Boolean);
begin
  Properties.RaggedRight := Value;
end;

procedure TcxCustomTabControl.SetRotate(const Value: Boolean);
begin
  Properties.Rotate := Value;
end;

procedure TcxCustomTabControl.SetMaxRotatedTabWidth(Value: Integer);
begin
  Properties.RotatedTabsMaxWidth := Value;
end;

procedure TcxCustomTabControl.SetScrollOpposite(const Value: Boolean);
begin
  Properties.ScrollOpposite := Value;
end;

procedure TcxCustomTabControl.SetShowFrame(const Value: Boolean);
begin
  Properties.ShowFrame := Value;
end;

procedure TcxCustomTabControl.SetStyle(const Value: TcxPCStyleID);
begin
  Properties.Style := Value;
end;

procedure TcxCustomTabControl.SetTabCaptionAlignment(Value: TAlignment);
begin
  Properties.TabCaptionAlignment := Value;
end;

procedure TcxCustomTabControl.SetTabHeight(const Value: Smallint);
begin
  Properties.TabHeight := Value;
end;

procedure TcxCustomTabControl.SetTabIndex(Value: Integer);
begin
  Properties.TabIndex := Value;
end;

function TcxCustomTabControl.GetMultiLine: Boolean;
begin
  Result := Properties.MultiLine;
end;

function TcxCustomTabControl.GetMultiSelect: Boolean;
begin
  Result := Properties.MultiSelect;
end;

function TcxCustomTabControl.GetNavigatorPosition: TcxPCNavigatorPosition;
begin
  Result := Properties.NavigatorPosition;
end;

function TcxCustomTabControl.GetRotate: Boolean;
begin
  Result := Properties.Rotate;
end;

function TcxCustomTabControl.GetScrollOpposite: Boolean;
begin
  Result := Properties.ScrollOpposite;
end;

function TcxCustomTabControl.GetShowFrame: Boolean;
begin
  Result := Properties.ShowFrame;
end;

function TcxCustomTabControl.GetTabCaptionAlignment: TAlignment;
begin
  Result := Properties.TabCaptionAlignment;
end;

function TcxCustomTabControl.GetTabHeight: Smallint;
begin
  Result := Properties.TabHeight;
end;

function TcxCustomTabControl.GetTabIndex: Integer;
begin
  Result := Properties.TabIndex;
end;

function TcxCustomTabControl.GetTabPosition: TcxTabPosition;
begin
  Result := Properties.TabPosition;
end;

function TcxCustomTabControl.GetTabSlants: TcxTabSlants;
begin
  Result := Properties.TabSlants;
end;

function TcxCustomTabControl.GetTabWidth: Smallint;
begin
  Result := Properties.TabWidth;
end;

function TcxCustomTabControl.GetOnNew: TcxPCNewEvent;
begin
  Result := FOnNewTabCreate;
end;

procedure TcxCustomTabControl.SetTabPosition(const Value: TcxTabPosition);
begin
  Properties.TabPosition := Value;
end;

procedure TcxCustomTabControl.SetTabs(const Value: TcxTabs);
begin
  Properties.Tabs := Value;
end;

procedure TcxCustomTabControl.SetTabSlants(Value: TcxTabSlants);
begin
  Properties.TabSlants := Value;
end;

procedure TcxCustomTabControl.SetTabWidth(const Value: Smallint);
begin
  Properties.TabWidth := Value;
end;

procedure TcxCustomTabControl.WriteClientRectBottom(Writer: TWriter);
begin
  Writer.WriteInteger(InternalGetClientRect.Bottom);
end;

procedure TcxCustomTabControl.WriteClientRectLeft(Writer: TWriter);
begin
  Writer.WriteInteger(InternalGetClientRect.Left);
end;

procedure TcxCustomTabControl.WriteClientRectRight(Writer: TWriter);
begin
  Writer.WriteInteger(InternalGetClientRect.Right);
end;

procedure TcxCustomTabControl.WriteClientRectTop(Writer: TWriter);
begin
  Writer.WriteInteger(InternalGetClientRect.Top);
end;

procedure TcxCustomTabControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TcxCustomTabControl.WMPrintClient(var Message: TWMPrintClient);
begin
  inherited
end;

procedure TcxCustomTabControl.WMSize(var Message: TWMSize);
begin
  LayoutChanged(False);
  inherited;
end;

procedure TcxCustomTabControl.CMDialogChar(var Message: TCMDialogChar);
begin
  if HandleDialogChar(Message.CharCode) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxCustomTabControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if Focused or HandleAllocated and Windows.IsChild(Handle, Windows.GetFocus) then
  begin
    if Controller.KeyDown(Message.CharCode, KeyDataToShiftState(Message.KeyData)) then
    begin
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TcxCustomTabControl.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TcxCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque, csParentBackground];
  Width := 289;
  Height := 193;

  FProperties := GetPropertiesClass.Create(Self);
  FProperties.OnChanged := PropertiesChangedHandler;
  FProperties.OnStyleChanged := StyleChanged;

  FProperties.OnCanClose := PropertiesCanCloseHandler;
  FProperties.OnChange := PropertiesChangeHandler;
  FProperties.OnChanging := PropertiesChangingHandler;
  FProperties.OnDrawTab := PropertiesDrawTabHandler;
  FProperties.OnDrawTabEx := PropertiesDrawTabExHandler;
  FProperties.OnGetImageIndex := PropertiesGetImageIndexHandler;
  FProperties.OnGetTabHint := PropertiesGetTabHintHandler;
  FProperties.OnNewTabButtonClick := PropertiesNewTabButtonClickHandler;
  FProperties.OnNewTabCreate := PropertiesNewTabCreateHandler;
  FProperties.OnTabClick := PropertiesTabClickHandler;
  FProperties.OnPaintDragImage := PropertiesPaintDragImageHandler;
  FProperties.OnPrepareTabCanvasFont := PropertiesPrepareTabCanvasFontHandler;

  FController := GetControllerClass.Create(Self);
  FFocusable := True;
  TabStop := True;
  FViewInfo := GetViewInfoClass.Create(Self);
  FViewInfo.OnAfterPaintTab := AfterPaintTab;
  RecreatePainter;
end;

destructor TcxCustomTabControl.Destroy;
begin
  cxControls.EndMouseTracking(Self as IcxMouseTrackingCaller2);
  FreeAndNil(FPainter);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TcxCustomTabControl.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FFocusable;
end;

procedure TcxCustomTabControl.CloseTab(AIndex: Integer);
begin
  Properties.CloseTab(AIndex);
end;

function TcxCustomTabControl.GetOptimalSize: Integer;
begin
  Result := ViewInfo.GetOptimalSize;
end;

procedure TcxCustomTabControl.GetTabOrderList(List: TList);
begin
  inherited GetTabOrderList(List);
  if not FFocusable then
    List.Remove(Self);
end;

function TcxCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result := ViewInfo.IndexOfTabAt(X, Y);
end;

function TcxCustomTabControl.VisibleIndexOfTabAt(X, Y: Integer): Integer;
begin
  Result := ViewInfo.VisibleIndexOfTabAt(X, Y);
end;

procedure TcxCustomTabControl.ScrollTabs(ADelta: Integer);
begin
  Controller.ScrollTabs(ADelta);
end;

procedure TcxCustomTabControl.SetStandardStyle(StandardStyle: TcxPCStandardStyle);
const
  StandardStyleNameMap: array[TcxPCStandardStyle] of string = (
    'tsTabs', 'tsButtons', 'tsFlatButtons');
var
  NewPainterClass: TcxPCPainterClass;
begin
  NewPainterClass := PaintersFactory.GetPainterClass(StandardStyle);
  if NewPainterClass = nil then
    OutError('SetStandardStyle', Format(cxGetResourceString(@scxPCStandardStyleError),
      [StandardStyleNameMap[StandardStyle]]))
  else
    Style := NewPainterClass.GetStyleID;
end;

procedure TcxCustomTabControl.SetStyleByStyleName(StyleName: TCaption);
var
  NewPainterClass: TcxPCPainterClass;
begin
  if StyleName = cxPCDefaultStyleName then
    Style := cxPCDefaultStyle
  else
  begin
    NewPainterClass := PaintersFactory.GetPainterClass(StyleName);
    if NewPainterClass = nil then
      OutError('SetStyleByName', Format(cxGetResourceString(@scxPCStyleNameError), [StyleName]))
    else
      Style := NewPainterClass.GetStyleID;
  end;
end;

procedure TcxCustomTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := InternalGetClientRect;
end;

function TcxCustomTabControl.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := inherited CanFocusOnClick(X, Y) and ViewInfo.CanFocusOnClick(X, Y);
end;

procedure TcxCustomTabControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  if (M <> D) and (not (csLoading in ComponentState) or (FScalingFlags <> [])) then
  begin
    if sfLeft in FScalingFlags then
      FClientRect.Left := MulDiv(FClientRect.Left, M, D);
    if sfTop in FScalingFlags then
      FClientRect.Top := MulDiv(FClientRect.Top, M, D);
    if sfWidth in FScalingFlags then
      FClientRect.Right := MulDiv(FClientRect.Right, M, D);
    if sfHeight in FScalingFlags then
      FClientRect.Bottom := MulDiv(FClientRect.Bottom, M, D);
  end;
  FProperties.ChangeScale(M, D);
  FScalingFlags := [];
  inherited;
end;

function TcxCustomTabControl.CreateDragAndDropObject: TcxDragAndDropObject;
begin
  Result := Controller.GetDragAndDropObjectClass.Create(Self);
end;

procedure TcxCustomTabControl.DefineProperties(Filer: TFiler);

  function IsClientRectBottomStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := InternalGetClientRect.Bottom <>
        TcxCustomTabControl(Filer.Ancestor).InternalGetClientRect.Bottom
    else
      Result := InternalGetClientRect.Bottom <> 0;
  end;

  function IsClientRectLeftStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := InternalGetClientRect.Left <>
        TcxCustomTabControl(Filer.Ancestor).InternalGetClientRect.Left
    else
      Result := InternalGetClientRect.Left <> 0;
  end;

  function IsClientRectRightStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := InternalGetClientRect.Right <>
        TcxCustomTabControl(Filer.Ancestor).InternalGetClientRect.Right
    else
      Result := True;
  end;

  function IsClientRectTopStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := InternalGetClientRect.Top <>
        TcxCustomTabControl(Filer.Ancestor).InternalGetClientRect.Top
    else
      Result := True;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ClientRectBottom', ReadClientRectBottom,
    WriteClientRectBottom, IsClientRectBottomStored);
  Filer.DefineProperty('ClientRectLeft', ReadClientRectLeft,
    WriteClientRectLeft, IsClientRectLeftStored);
  Filer.DefineProperty('ClientRectRight', ReadClientRectRight,
    WriteClientRectRight, IsClientRectRightStored);
  Filer.DefineProperty('ClientRectTop', ReadClientRectTop,
    WriteClientRectTop, IsClientRectTopStored);
  FIsClientRectLoaded := True;
end;

function TcxCustomTabControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift);
  if not Result then
  begin
    ViewInfo.SynchronizeHotTrackStates(Shift);
    Result := Controller.HitTest.HitAtNavigatorButton and
      not (TcxPCNavigatorButtonViewInfo(Controller.HitTest.HitObject).ButtonType in [nbGoDialog, nbClose]) or
      Controller.HitTest.HitAtTab and (TcxTabViewInfo(Controller.HitTest.HitObject).Index <> TabIndex);
  end;
end;

function TcxCustomTabControl.DoMouseWheelDown(Shift: TShiftState;
   MousePos: TPoint): Boolean;
begin
  Result := Controller.DoMouseWheelDown(Shift, ScreenToClient(MousePos));
end;

function TcxCustomTabControl.DoMouseWheelUp(Shift: TShiftState;
   MousePos: TPoint): Boolean;
begin
  Result := Controller.DoMouseWheelUp(Shift, ScreenToClient(MousePos));
end;

procedure TcxCustomTabControl.AfterLoaded;
var
  AOldTabIndex: Integer;
begin
  Tabs.Changed;
  if TabIndex <> -1 then
  begin
    AOldTabIndex := TabIndex;
    Properties.FTabIndex := -1;
    Properties.LockChangeEvent;
    try
      TabIndex := AOldTabIndex;
    finally
      Properties.UnlockChangeEvent;
    end;
  end;
end;

procedure TcxCustomTabControl.AfterPaintTab(ACanvas: TcxCanvas; ATab: TcxTab;
  AImageAndTextData: TcxPCOutTabImageAndTextData);
begin
end;

procedure TcxCustomTabControl.Change;
begin
  if not Properties.IsChangeEventLocked then
    CallNotify(FOnChange, Self);
end;

procedure TcxCustomTabControl.EraseBackground(DC: HDC);
begin
  Canvas.SaveDC;
  try
    Canvas.Canvas.Handle := DC;
    Painter.FillTabPaneContent(Canvas);
  finally
    Canvas.RestoreDC;
  end;
end;

procedure TcxCustomTabControl.FocusChanged;
begin
  inherited FocusChanged;
  Controller.FocusChanged;
end;

procedure TcxCustomTabControl.FontChanged;
begin
  inherited FontChanged;
  RequestLayout;
end;

function TcxCustomTabControl.HandleDialogChar(Key: Integer): Boolean;
begin
  Result := inherited CanFocus and Controller.HandleDialogChar(Key);
end;

function TcxCustomTabControl.HasBackground: Boolean;
begin
  Result := False;
end;

procedure TcxCustomTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
end;

procedure TcxCustomTabControl.Loaded;
begin
  inherited Loaded;
  AfterLoaded;
end;

procedure TcxCustomTabControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  RecreatePainter;
  RequestLayout;
  if lfvNativeStyle in AChangedValues then
    InvalidateWithChildren;
end;

procedure TcxCustomTabControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomTabControl.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  cxControls.BeginMouseTracking(Self, Bounds, Self as IcxMouseTrackingCaller2);
end;

procedure TcxCustomTabControl.MouseLeave(AControl: TControl);
begin
  cxControls.EndMouseTracking(Self as IcxMouseTrackingCaller2);
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

procedure TcxCustomTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, X, Y);
  cxControls.BeginMouseTracking(Self, Bounds, Self as IcxMouseTrackingCaller2);
end;

procedure TcxCustomTabControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomTabControl.PropertiesTabClickHandler(Sender: TObject; ATabVisibleIndex: Integer; AShift: TShiftState);
begin
// do nothing
end;

function TcxCustomTabControl.PropertiesTabDragAndDrop(AIndex, ADestinationIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnTabDragAndDrop) then
    FOnTabDragAndDrop(Self, AIndex, ADestinationIndex, Result);
end;

procedure TcxCustomTabControl.PropertiesTabEndDrag(ANewIndex: Integer);
begin
  if Assigned(FOnTabEndDrag) then
    FOnTabEndDrag(Self, ANewIndex);
end;

procedure TcxCustomTabControl.PropertiesTabStartDrag(AIndex: Integer);
begin
  if Assigned(FOnTabStartDrag) then
    FOnTabStartDrag(Self, AIndex);
end;

procedure TcxCustomTabControl.PropertiesCanCloseHandler(Sender: TObject;
   AIndex: Integer; var ACanClose: Boolean);
begin
  if Assigned(FOnCanCloseEx) then
    FOnCanCloseEx(Self, AIndex, ACanClose)
  else
    if (AIndex = TabIndex) and Assigned(FOnCanClose) then
      FOnCanClose(Self, ACanClose);
end;

procedure TcxCustomTabControl.PropertiesChangeHandler(Sender: TObject);
begin
  Change;
end;

procedure TcxCustomTabControl.PropertiesChangedHandler(Sender: TObject; AType: TcxCustomTabControlPropertiesChangedType);
begin
  case AType of
    pctLight:
      Invalidate;
    pctSimple:
      begin
        ViewInfo.RearrangeRows;
        Invalidate;
        Realign;
      end;
  else //pctMedium, pctHard
    RequestLayout;
  end;
end;

procedure TcxCustomTabControl.PropertiesChangingHandler(Sender: TObject;
  var AllowChange: Boolean);
begin
  if Assigned(FOnChanging) and not Properties.IsChangeEventLocked then
    FOnChanging(Self, AllowChange);
end;

procedure TcxCustomTabControl.PropertiesDrawTabExHandler(
  AControl: TcxCustomTabControlProperties; ATab: TcxTab; Font: TFont);
begin
  if Assigned(FOnDrawTabEx) then
    FOnDrawTabEx(Self, ATab, Font);
end;

procedure TcxCustomTabControl.PropertiesDrawTabHandler(
  AControl: TcxCustomTabControlProperties; ATab: TcxTab;
  var DefaultDraw: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, ATab, DefaultDraw);
end;

procedure TcxCustomTabControl.PropertiesGetImageIndexHandler(
  Sender: TObject; TabIndex: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, TabIndex, ImageIndex);
end;

procedure TcxCustomTabControl.PropertiesGetTabHintHandler(Sender: TObject;
  ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
begin
  if Assigned(FOnGetTabHint) then
    FOnGetTabHint(Self, ATabIndex, AHint, ACanShow);
end;

procedure TcxCustomTabControl.PropertiesNewTabButtonClickHandler(Sender: TObject; var AHandled: Boolean);
begin
  if Assigned(FOnNewTabButtonClick) then
    FOnNewTabButtonClick(Self, AHandled);
end;

procedure TcxCustomTabControl.PropertiesNewTabCreateHandler(Sender: TObject; AIndex: Integer);
begin
  if Assigned(FOnNewTabCreate) then
    FOnNewTabCreate(Self, AIndex);
end;

procedure TcxCustomTabControl.PropertiesPaintDragImageHandler(Sender: TObject; ABitmap: TBitmap; var ADone: Boolean);
begin
  if Assigned(FOnPaintDragImage) then
    FOnPaintDragImage(Self, ABitmap, ADone);
end;

function TcxCustomTabControl.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

function TcxCustomTabControl.NeedRedrawOnResize: Boolean;
begin
  Result := (pcoRedrawOnResize in Options) and
    (Painter.NeedRedrawOnResize or cxIsVCLThemesEnabled);
end;

procedure TcxCustomTabControl.Paint;
begin
  if IsDestroying then
    Exit;
  Canvas.SaveClipRegion;
  try
    FPainter.Paint(Canvas);
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TcxCustomTabControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
end;

function TcxCustomTabControl.GetControllerClass: TcxCustomTabControlControllerClass;
begin
  Result := TcxCustomTabControlController;
end;

function TcxCustomTabControl.GetPainterClass: TcxPCPainterClass;
begin
  Result := ViewInfo.GetPainterClass;
end;

function TcxCustomTabControl.GetPropertiesClass: TcxCustomTabControlPropertiesClass;
begin
  Result := TcxCustomTabControlProperties;
end;

function TcxCustomTabControl.GetViewInfoClass: TcxCustomTabControlViewInfoClass;
begin
  Result := TcxCustomTabControlViewInfo;
end;

class procedure TcxCustomTabControl.OutError(SourceMethodName: TCaption; Msg: TCaption);
begin
  raise EdxException.Create('TcxCustomTabControl.' + SourceMethodName + ': ' + Msg);
end;

procedure TcxCustomTabControl.LayoutChanged(ANeedRealign: Boolean);
begin
  ViewInfo.Calculate;
  if ANeedRealign then
    Realign;
  Invalidate;
  if HandleAllocated then
    SendMessage(Handle, DXM_UIADORNERMANAGERUPDATE, 0, 0);
end;

procedure TcxCustomTabControl.PropertiesPrepareTabCanvasFontHandler(ATab: TcxTab; ACanvas: TcxCanvas);
begin
// do nothing
end;

procedure TcxCustomTabControl.RecreatePainter;
var
  ANewPainterClass: TcxPCPainterClass;
begin
  ANewPainterClass := GetPainterClass;
  if (FPainter = nil) or (ANewPainterClass <> FPainter.ClassType) then
  begin
    FreeAndNil(FPainter);
    if ANewPainterClass = nil then
    begin
      FPainter := nil;
      OutError('GetPainterInstance', cxGetResourceString(@scxPCPainterClassError));
    end
    else
      FPainter := ANewPainterClass.Create(FViewInfo);
    DoubleBuffered := FPainter.NeedDoubleBuffer;
    RequestLayout;
    InvalidateWithChildren;
  end;
end;

procedure TcxCustomTabControl.RequestLayout;
begin
  if IsLoading or IsDestroying or ViewInfo.IsUpdating then Exit;
  LayoutChanged(True);
end;

procedure TcxCustomTabControl.SetModified;
begin
  if not IsLoading then
    Modified;
end;

procedure TcxCustomTabControl.StyleChanged(Sender: TObject);
begin
  RecreatePainter;
end;

procedure TcxCustomTabControl.UpdateTabImages;
var
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
    Tabs[I].ImageIndex := Properties.GetImageIndex(Tabs[I]);
  Invalidate;
end;

function TcxCustomTabControl.InternalKeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := Controller.KeyDown(Key, Shift);
end;

function TcxCustomTabControl.AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxCustomTabControl.AllowDragAndDropWithoutFocus: Boolean;
begin
  Result := True;
end;

procedure TcxCustomTabControl.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  ADragAndDropObject: TcxTabControlDragAndDropObject;
begin
  ADragAndDropObject := TcxTabControlDragAndDropObject(DragAndDropObject);
  Accepted := (ADragAndDropObject.DestinationTabVisibleIndex <> -1) and
    PropertiesTabDragAndDrop(ADragAndDropObject.GetDragTabIndex, ADragAndDropObject.GetDragDestinationTabIndex);
  inherited DragAndDrop(P, Accepted);
end;

procedure TcxCustomTabControl.EndDragAndDrop(Accepted: Boolean);
var
  ANewIndex: Integer;
  ADragAndDropObject: TcxTabControlDragAndDropObject;
begin
  ADragAndDropObject := TcxTabControlDragAndDropObject(DragAndDropObject);
  Controller.EndDragAndDrop(Accepted);
  Accepted := (ADragAndDropObject.DestinationTabVisibleIndex <> -1) and (ADragAndDropObject.TabVisibleIndex <> -1) and
    (ADragAndDropObject.DestinationTabVisibleIndex <> ADragAndDropObject.TabVisibleIndex) and
    PropertiesTabDragAndDrop(ADragAndDropObject.GetDragTabIndex, ADragAndDropObject.GetDragDestinationTabIndex);
  inherited EndDragAndDrop(Accepted);
  ANewIndex := ADragAndDropObject.GetDragTabIndex;
  if Accepted and (ADragAndDropObject.DestinationTabVisibleIndex <> -1) then
    ANewIndex := ADragAndDropObject.GetDragDestinationTabIndex;
  PropertiesTabEndDrag(ANewIndex);
end;

function TcxCustomTabControl.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := not DockSite and Controller.StartDragAndDrop(P);
  if Result then
    PropertiesTabStartDrag(TcxTabControlDragAndDropObject(DragAndDropObject).GetDragTabIndex);
end;

function TcxCustomTabControl.IsLoading: Boolean;
begin
  Result := inherited IsLoading;
end;

function TcxCustomTabControl.IsParentBackground: Boolean;
begin
  Result := ParentBackground;
end;

function TcxCustomTabControl.PtInCaller(const P: TPoint): Boolean;
begin
  Result := WindowFromPoint(GetMouseCursorPos) = Handle;
end;

procedure TcxCustomTabControl.MouseTrackingCallerMouseLeave;
begin
  MouseLeave(nil);
end;

procedure TcxCustomTabControl.GetAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Tabs.Count - 1 do
    AList.AddObject(Tabs[I].Caption, Tabs[I]);
end;

function TcxCustomTabControl.IsEnabled: Boolean;
begin
  Result := Enabled;
end;

function TcxCustomTabControl.IsDesigning: Boolean;
begin
  Result := inherited IsDesigning;
end;

function TcxCustomTabControl.IsDestroying: Boolean;
begin
  Result := inherited IsDestroying;
end;

function TcxCustomTabControl.IsFocused: Boolean;
begin
  Result := Focused;
end;

function TcxCustomTabControl.CanDrawParentBackground: Boolean;
begin
  Result := True;
end;

function TcxCustomTabControl.GetCanvas: TcxCanvas;
begin
  Result := ActiveCanvas;
end;

function TcxCustomTabControl.GetBoundsRect: TRect;
begin
  Result := BoundsRect;
end;

function TcxCustomTabControl.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomTabControl.GetController: TcxCustomTabControlController;
begin
  Result := Controller;
end;

function TcxCustomTabControl.GetDragAndDropObject: TcxDragAndDropObject;
begin
  Result := DragAndDropObject;
end;

function TcxCustomTabControl.GetDragAndDropState: TcxDragAndDropState;
begin
  Result := DragAndDropState;
end;

function TcxCustomTabControl.GetColor: TColor;
begin
  Result := Color;
end;

function TcxCustomTabControl.GetFont: TFont;
begin
  Result := Font;
end;

function TcxCustomTabControl.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

function TcxCustomTabControl.GetPainter: TcxPCCustomPainter;
begin
  Result := Painter;
end;

function TcxCustomTabControl.GetProperties: TcxCustomTabControlProperties;
begin
  Result := Properties;
end;

function TcxCustomTabControl.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := ViewInfo;
end;

procedure TcxCustomTabControl.CreateHandle;
begin
  inherited CreateHandle;
  RequestLayout;
end;

procedure TcxCustomTabControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not(CS_HREDRAW or CS_VREDRAW);
end;

{ TcxTabs }

constructor TcxTabs.Create(AProperties: TcxCustomTabControlProperties);
begin
  inherited Create;
  FChangedHandlers := TcxEventHandlerCollection.Create;
  FDestroyHandlers := TcxEventHandlerCollection.Create;
  FTabItems := TObjectList.Create;
  FProperties := AProperties;
  CreateNewButton;
end;

destructor TcxTabs.Destroy;
begin
  FDestroyHandlers.CallEvents(Self, []);
  FreeAndNil(FDestroyHandlers);
  DestroyNewButton;
  FIsTabsCleaning := True;
  FreeAndNil(FTabItems);
  FreeAndNil(FChangedHandlers);
  inherited;
end;

procedure TcxTabs.Delete(Index: Integer);
begin
  TabItems.Delete(Index);
end;

function TcxTabs.Get(Index: Integer): string;
begin
  Result := Tabs[Index].Caption;
end;

function TcxTabs.GetCount: Integer;
begin
  Result := TabItems.Count;
end;

function TcxTabs.GetObject(Index: Integer): TObject;
begin
  Result := Tabs[Index].FObject;
end;

procedure TcxTabs.Put(Index: Integer; const S: string);
begin
  Tabs[Index].Caption := S;
end;

procedure TcxTabs.PutObject(Index: Integer; AObject: TObject);
begin
  Tabs[Index].FObject := AObject;
end;

procedure TcxTabs.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    Changed;
end;

function TcxTabs.GetTab(TabIndex: Integer): TcxTab;
begin
  Result := TcxTab(TabItems[TabIndex]);
end;

function TcxTabs.GetVisibleTab(ATabVisibleIndex: Integer): TcxTab;
begin
  Result := GetViewInfo[ATabVisibleIndex].Tab;
end;

procedure TcxTabs.Insert(Index: Integer; const S: string);
begin
  BeginUpdate;
  try
    TabItems.Insert(Index, TcxTab.Create(Self));
    Tabs[Index].FImageIndex := Index;
    Tabs[Index].InternalSetCaption(S);

    if not Properties.IsLoading and (Properties.TabIndex = -1) and (Count = 1) then
      Properties.FTabIndex := 0;
  finally
    EndUpdate;
  end;
end;

procedure TcxTabs.Move(CurIndex, NewIndex: Integer);
begin
  TabItems.Move(CurIndex, NewIndex);
  Changed;
end;

procedure TcxTabs.Changed(ATab: TcxTab = nil; ATabPropertyChanged: TcxPCTabPropertyChanged = tpcLayout);
var
  AEventArgs: TcxPCTabsChangedEventArgs;
begin
  if Properties.CanProcessChanged then
  begin
    AEventArgs.Tab := ATab;
    AEventArgs.PropertyChanged := ATabPropertyChanged;
    ChangedHandlers.CallEvents(Self, AEventArgs);
  end;
end;

procedure TcxTabs.Clear;
begin
  FIsTabsCleaning := True;
  try
    TabItems.Clear;
  finally
    FIsTabsCleaning := False;
    Changed;
  end;
end;

function TcxTabs.GetViewInfoClass: TcxTabsViewInfoClass;
begin
  Result := TcxTabsViewInfo;
end;

function TcxTabs.GetNewButtonClass: TcxPCNewButtonClass;
begin
  Result := TcxPCNewButton;
end;

procedure TcxTabs.SetTab(Index: Integer; const Value: TcxTab);
begin
  Tabs[Index].Assign(Value);
end;

function TcxTabs.GetVisibleTabsCount: Integer;
begin
  Result := GetViewInfo.ViewInfoCount;
end;

function TcxTabs.GetControlViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := Properties.GetViewInfo;
end;

function TcxTabs.GetViewInfo: TcxTabsViewInfo;
begin
  if GetControlViewInfo <> nil  then
    Result := GetControlViewInfo.TabsViewInfo
  else
    Result := nil;
end;

procedure TcxTabs.CreateNewButton;
begin
  FNewButton := GetNewButtonClass.Create(Self);
end;

procedure TcxTabs.DestroyNewButton;
begin
  FreeAndNil(FNewButton);
end;

procedure TcxTabs.RemoveTab(ATab: TcxTab);
begin
  FProperties.LockChangingEvent;
  try
    TabItems.Extract(ATab);
    Changed;
  finally
    FProperties.UnlockChangingEvent;
  end;
end;

{ TcxTabSlants }

constructor TcxTabSlants.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FKind := skSlant;
  FPositions := [spLeft];
end;

procedure TcxTabSlants.Assign(Source: TPersistent);
begin
  if Source is TcxTabSlants then
  begin
    Kind := TcxTabSlants(Source).Kind;
    Positions := TcxTabSlants(Source).Positions;
  end
  else
    inherited Assign(Source);
end;

function TcxTabSlants.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxTabSlants.Changed;
begin
  CallNotify(FOnChange, Self);
end;

procedure TcxTabSlants.SetKind(Value: TcxTabSlantKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TcxTabSlants.SetPositions(Value: TcxTabSlantPositions);
begin
  if Value <> FPositions then
  begin
    FPositions := Value;
    Changed;
  end;
end;

{ TcxPCCustomGoDialog }

constructor TcxPCCustomGoDialog.Create(ATabControl: IcxTabControl);
begin
  inherited Create;
  FTabControl := ATabControl;
end;

function TcxPCCustomGoDialog.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := TabControl.ViewInfo;
end;

{ TcxTabControlDragAndDropObject }

constructor TcxTabControlDragAndDropObject.Create(AIControl: IcxTabControl);
begin
  inherited Create(nil);
  FIControl := AIControl;
  Canvas := FIControl.GetCanvas;
  FDestinationTabVisibleIndex := -1;
  FHitTabVisibleIndex := -1;
end;

procedure TcxTabControlDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  CreateDragImage;
end;

procedure TcxTabControlDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);

  function CalculateDestinationTabVisibleIndex: Integer;
  var
    AIsTop, AIsLeft: Boolean;
    AIsAfter: Boolean;
    AIsBefore: Boolean;
  begin
    FHitTabVisibleIndex := ViewInfo.VisibleIndexOfTabAt(P.X, P.Y);
    Result := FHitTabVisibleIndex;
    if (Result <> -1) and ((Result = FTabVisibleIndex) or ViewInfo.TabsViewInfo[Result].IsNewButton) then
      Result := -1;
    if (Result <> -1) then
    begin
      with ViewInfo.TabsViewInfo[Result].NormalRect do
      begin
        AIsTop := ((Top + Bottom) div 2) < P.Y;
        if ViewInfo.UseRightToLeftAlignment then
          AIsLeft := ((Left + Right) div 2) > P.X
        else
          AIsLeft := ((Left + Right) div 2) < P.X;
      end;

      AIsAfter := (Result < FTabVisibleIndex) and (
        ((Properties.TabPosition in [tpLeft, tpRight]) and (ViewInfo.IsBottomToTopAlignment xor AIsTop)) or
        ((Properties.TabPosition in [tpTop, tpBottom]) and (ViewInfo.IsRightToLeftAlignment xor AIsLeft)));
      if AIsAfter then
        Inc(Result);

      AIsBefore := (Result > FTabVisibleIndex) and (
        ((Properties.TabPosition in [tpLeft, tpRight]) and not (ViewInfo.IsBottomToTopAlignment xor AIsTop)) or
        ((Properties.TabPosition in [tpTop, tpBottom]) and not (ViewInfo.IsRightToLeftAlignment xor AIsLeft)));
      if AIsBefore then
        Dec(Result);

      if Result = FTabVisibleIndex then
        Result := -1;
    end;
  end;
begin
  ShowDragImage;
  DestinationTabVisibleIndex := CalculateDestinationTabVisibleIndex;
  CheckScrolling(P);
  Accepted := Accepted and (DestinationTabVisibleIndex <> -1) and (DestinationTabVisibleIndex <> FTabVisibleIndex);
  inherited DragAndDrop(P, Accepted);
end;

procedure TcxTabControlDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  DestroyDragImage;
  DestroyDestinationImage;
  DestroyScrollingTimer;
  Drop(Accepted);
  inherited EndDragAndDrop(Accepted);
end;

procedure TcxTabControlDragAndDropObject.Init(ATabVisibleIndex: Integer; const P: TPoint);
begin
  FTabVisibleIndex := ATabVisibleIndex;
  FDragPointOffset := cxPointOffset(ViewInfo.TabsViewInfo[FTabVisibleIndex].FullRect.TopLeft, P, False);
end;

function TcxTabControlDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crDefault
  else
    Result := crNoDrop;
end;

procedure TcxTabControlDragAndDropObject.CheckScrolling(const P: TPoint);

  function NeedScrolling: Boolean;
  begin
    Result := not ViewInfo.MultiLine and ViewInfo.PtInScrollingArea(P, FScrollingDirection);
  end;

  procedure CheckTimer;
  begin
    if FScrollingTimer = nil then
      CreateScrollingTimer;
  end;

begin
  if NeedScrolling then
    CheckTimer
  else
    DestroyScrollingTimer;
end;

procedure TcxTabControlDragAndDropObject.CreateDestinationImage;
const
  ArrowPlaceMap: array[Boolean, Boolean] of TcxArrowPlace = ((apRight, apBottom), (apLeft, apTop));
var
  AControlOrigin: TPoint;
  R: TRects;
begin
  AControlOrigin := FIControl.Controller.GetClientToScreen(cxNullPoint);
  SetLength(R, 2);
  try
    R := ViewInfo.Painter.GetDropArrowRects(FTabVisibleIndex, FHitTabVisibleIndex, DestinationTabVisibleIndex);
    FDestinationArrowFirst := TcxDragAndDropArrow.Create(True);
    FDestinationArrowFirst.Init(AControlOrigin, cxNullRect, R[0], ArrowPlaceMap[True, ViewInfo.TabPosition in [tpTop, tpBottom]]);
    FDestinationArrowSecond := TcxDragAndDropArrow.Create(True);
    FDestinationArrowSecond.Init(AControlOrigin, cxNullRect, R[1], ArrowPlaceMap[False, ViewInfo.TabPosition in [tpTop, tpBottom]]);
    FDestinationArrowFirst.Visible := True;
    FDestinationArrowSecond.Visible := True;
  finally
    SetLength(R, 0);
  end;
end;

procedure TcxTabControlDragAndDropObject.CreateDragImage;
begin
  FDragImage := TcxDragImage.Create;
  PaintDragImage;
end;

procedure TcxTabControlDragAndDropObject.CreateScrollingTimer;
begin
  FScrollingTimer := cxCreateTimer(ScrollingTimerHandler, TabScrollingStartDelay);
end;

procedure TcxTabControlDragAndDropObject.DestroyDestinationImage;
begin
  FreeAndNil(FDestinationArrowFirst);
  FreeAndNil(FDestinationArrowSecond);
end;

procedure TcxTabControlDragAndDropObject.DestroyDragImage;
begin
  FreeAndNil(FDragImage);
end;

procedure TcxTabControlDragAndDropObject.DestroyScrollingTimer;
begin
  FreeAndNil(FScrollingTimer);
end;

procedure TcxTabControlDragAndDropObject.DoPaintDragImage;
var
  R: TRect;
begin
  R := ViewInfo.TabsViewInfo[FTabVisibleIndex].FullRect;
  FDragImage.SetBounds(0, 0, cxRectWidth(R), cxRectHeight(R));
  FDragImage.Canvas.WindowOrg := R.TopLeft;
  FDragImage.Canvas.Lock;
  try
    ViewInfo.Painter.PaintDragImage(FDragImage.Canvas, R, FTabVisibleIndex);
  finally
    FDragImage.Canvas.Unlock;
  end;
end;

procedure TcxTabControlDragAndDropObject.Drop(Accepted: Boolean);
var
  AIndex: Integer;
  ADestinationIndex: Integer;
begin
  if Accepted and (DestinationTabVisibleIndex <> -1) then
  begin
    AIndex := ViewInfo.TabsViewInfo[FTabVisibleIndex].Index;
    ADestinationIndex := ViewInfo.TabsViewInfo[DestinationTabVisibleIndex].Index;
    FIControl.Properties.BeginUpdate;
    try
      FIControl.Properties.MoveTab(AIndex, ADestinationIndex);
      FIControl.Properties.TabIndex := ADestinationIndex;
    finally
      FIControl.Properties.EndUpdate;
    end;
  end;
end;

function TcxTabControlDragAndDropObject.GetClientCursorPos: TPoint;
begin
  Result := FIControl.Controller.GetMouseCursorPos;
end;

function TcxTabControlDragAndDropObject.GetDragDestinationTabIndex: Integer;
begin
  Result := ViewInfo.TabsViewInfo[DestinationTabVisibleIndex].Index;
end;

function TcxTabControlDragAndDropObject.GetDragTabIndex: Integer;
begin
  Result := ViewInfo.TabsViewInfo[TabVisibleIndex].Index;
end;

procedure TcxTabControlDragAndDropObject.PaintDragImage;
begin
  if not Properties.DoPaintDragImage(FDragImage) then
    DoPaintDragImage
  else
    FDragPointOffset := cxNullPoint;
end;

procedure TcxTabControlDragAndDropObject.ShowDragImage;
var
  P: TPoint;
begin
  P := cxPointOffset(GetMouseCursorPos, FDragPointOffset);
  FDragImage.MoveTo(P);
  if not FDragImage.Visible then
    FDragImage.Show;
end;

function TcxTabControlDragAndDropObject.GetProperties: TcxCustomTabControlProperties;
begin
  Result := FIControl.Properties;
end;

function TcxTabControlDragAndDropObject.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := FIControl.ViewInfo;
end;

procedure TcxTabControlDragAndDropObject.ScrollingTimerHandler(Sender: TObject);
begin
  ViewInfo.FirstTabVisibleIndex := ViewInfo.FirstTabVisibleIndex + FScrollingDirection;
  FIControl.RequestLayout;

  FScrollingTimer.Interval := TabScrollingDelay;
end;

procedure TcxTabControlDragAndDropObject.SetDestinationTabVisibleIndex(Value: Integer);
begin
  if DestinationTabVisibleIndex <> Value then
  begin
    if DestinationTabVisibleIndex <> -1 then
      DestroyDestinationImage;
    FDestinationTabVisibleIndex := Value;
    if DestinationTabVisibleIndex <> -1 then
      CreateDestinationImage;
  end;
end;

{ TcxCustomTabControlHitTest }

constructor TcxCustomTabControlHitTest.Create(AOwner: TcxCustomTabControlController);
begin
  inherited Create;
  FController := AOwner;
end;

procedure TcxCustomTabControlHitTest.Clear;
begin
  FFlags := 0;
  FHitObject := nil;
  FHitTab := nil;
end;

procedure TcxCustomTabControlHitTest.Recalculate;
begin
  Clear;
  FController.ViewInfo.CalculateHitTest(Self);
end;

procedure TcxCustomTabControlHitTest.Update(AShift: TShiftState; const APoint: TPoint);
begin
  FShift := AShift;
  FHitPoint := APoint;
  Recalculate;
end;

function TcxCustomTabControlHitTest.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

function TcxCustomTabControlHitTest.GetHitAtHeaderButton: Boolean;
begin
  Result := GetBitState(pchtNavigatorButton) or GetBitState(pchtHeaderButton);
end;

function TcxCustomTabControlHitTest.GetHitAtTabButton: Boolean;
begin
  Result := GetBitState(pchtTabButton) or GetBitState(pchtTabCloseButton);
end;

procedure TcxCustomTabControlHitTest.SetBitState(AIndex: Integer;
  AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

{ TcxCustomTabControlHintHelper }

constructor TcxCustomTabControlHintHelper.Create(
  AController: TcxCustomTabControlController);
begin
  inherited Create;
  FController := AController;
end;

procedure TcxCustomTabControlHintHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited CorrectHintWindowRect(ARect);
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

function TcxCustomTabControlHintHelper.GetOwnerWinControl: TWinControl;
begin
  Result := FController.IControl.GetControl;
end;

{ TcxTabSheet }

constructor TcxTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  FImageIndex := -1;
  FTab := nil;
  FTabVisible := True;
  TabStop := False;
  FAllowCloseButton := True;
end;

destructor TcxTabSheet.Destroy;
begin
  if PageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then FPageControl.FUndockingPage := nil;
    FPageControl.Properties.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TcxTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not(CS_HREDRAW or CS_VREDRAW);
end;

procedure TcxTabSheet.DoHide;
begin
  if (PageControl <> nil) and PageControl.Properties.IsNotifyEventLocked then
    PageControl.Properties.AddDeferredEvent(FOnHide, Self)
  else
    CallNotify(FOnHide, Self);
end;

procedure TcxTabSheet.DoShow;
begin
  if (PageControl <> nil) and PageControl.Properties.IsNotifyEventLocked then
    PageControl.Properties.AddDeferredEvent(FOnShow, Self)
  else
    CallNotify(FOnShow, Self);
end;

procedure TcxTabSheet.EnabledChanged;
begin
  PagePropertyChanged;
end;

procedure TcxTabSheet.PagePropertyChanged;
begin
  if PageControl <> nil then
    PageControl.Properties.UpdateTab(Self);
end;

procedure TcxTabSheet.SetParent(AParent: TWinControl);
begin
  if (AParent is TcxPageControl) or (AParent = nil) then
    if AParent <> FPageControl then
      PageControl := TcxPageControl(AParent)
    else
      inherited SetParent(AParent)
  else
    Abort;
end;

procedure TcxTabSheet.SetParentPageControl(AParentPageControl: TcxPageControl);
begin
  FPageControl := AParentPageControl;
  SetParent(AParentPageControl);
end;

procedure TcxTabSheet.ShowingChanged;
begin
  try
    if Showing then
      DoShow
    else
      DoHide;
  except
    Application.HandleException(Self);
  end;
end;

procedure TcxTabSheet.TextChanged;
begin
  PagePropertyChanged;
end;

procedure TcxTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  ASavedDC: Integer;
  APageOffset: TPoint;
begin
  if PageControl <> nil then
  begin
    if Painter.IsNativePainting then
      Painter.DrawNativeTabBackground(Message.DC, Self)
    else
    begin
      ASavedDC := SaveDC(Message.DC);
      try
        APageOffset := PageControl.PageClientRect.TopLeft;
        OffsetWindowOrgEx(Message.DC, APageOffset.X, APageOffset.Y, nil); // B158825
        PageControl.EraseBackground(Message.DC);
      finally
        RestoreDC(Message.DC, ASavedDC);
      end;
    end;
  end;
  Message.Result := 1;
end;

procedure TcxTabSheet.WMNCPaint(var Message: TWMNCPaint);
var
  ASavedDC: Integer;
  DC: HDC;
  R: TRect;
begin
  if (PageControl = nil) or not Painter.IsNativePainting then
    inherited
  else
  begin
    DC := GetWindowDC(Handle);
    try
      ASavedDC := SaveDC(DC);
      try
        R := GetControlRect(Self);
        InflateRect(R, -BorderWidth, -BorderWidth);
        with R do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
        Painter.DrawNativeTabBackground(DC, Self);
      finally
        RestoreDC(DC, ASavedDC);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TcxTabSheet.CMColorChanged(var Message: TMessage);
begin
  inherited;
  InternalColorChanged;
end;

procedure TcxTabSheet.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  EnabledChanged;
end;

procedure TcxTabSheet.CMInvalidate(var Message: TMessage);
begin
  if (PageControl <> nil) then
    PageControl.InvalidateControl(Self, Message.WParam = 0, PageControl.NeedRedrawOnResize);
end;

procedure TcxTabSheet.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  InternalColorChanged;
end;

procedure TcxTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  ShowingChanged;
end;

procedure TcxTabSheet.CMTextChanged(var Message: TMessage);
begin
  inherited;
  TextChanged;
end;

function TcxTabSheet.GetPageIndex: Integer;
begin
  if PageControl = nil then
    Result := -1
  else
    Result := PageControl.Properties.FPages.IndexOf(Self);
end;

function TcxTabSheet.GetPainter: TcxPCCustomPainter;
begin
  Result := PageControl.Painter;
end;

function TcxTabSheet.GetTabIndex: Integer;
begin
  if FTab = nil then
    Result := -1
  else
    Result := FTab.VisibleIndex;
end;

procedure TcxTabSheet.InternalColorChanged;
begin
  if FTab <> nil then
    FTab.Changed(tpcLayout);
  if (BorderWidth > 0) and HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 1, 0);
  Invalidate;
end;

procedure TcxTabSheet.SetAllowCloseButton(const Value: Boolean);
begin
  if Value <> FAllowCloseButton then
  begin
    FAllowCloseButton := Value;
    PagePropertyChanged;
  end;
end;

procedure TcxTabSheet.SetHighlighted(const Value: Boolean);
begin
  if Value <> FHighlighted then
  begin
    FHighlighted := Value;
    PagePropertyChanged;
  end;
end;

procedure TcxTabSheet.SetImageIndex(const Value: TcxImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    PagePropertyChanged;
  end;
end;

procedure TcxTabSheet.SetPageControl(const Value: TcxPageControl);
begin
  if Value <> FPageControl then
  begin
    if FPageControl <> nil then FPageControl.Properties.RemovePage(Self);
    if Value <> nil then
    begin
      Value.Properties.InsertPage(Self);
      if not(csLoading in Value.ComponentState) and (Value.ActivePage = nil) and TabVisible then
        Value.ActivePage := Self;
    end;
  end;
end;

procedure TcxTabSheet.SetPageIndex(const Value: Integer);
var
  AOldPageIndex: Integer;
begin
  if PageControl <> nil then
  begin
    if Value > PageControl.PageCount - 1 then
      raise EListError.CreateFmt(cxGetResourceString(@scxPCPageIndexError), [Value, PageControl.PageCount - 1]);
    AOldPageIndex := PageIndex;
    PageControl.Properties.BeginUpdate;
    try
      PageControl.Properties.FPages.Move(AOldPageIndex, Value);
      PageControl.Tabs.Move(AOldPageIndex, Value);
      FTab := PageControl.Tabs[Value];
      PageControl.Properties.UpdateTab(Self);
      PageControl.Properties.LockChangeEvent;
      try
        PageControl.TabIndex := PageControl.ActivePageIndex;
        PageControl.Properties.UpdateTabOrders;
      finally
        PageControl.Properties.UnLockChangeEvent;
      end;
    finally
      PageControl.Properties.EndUpdate;
    end;
  end;
end;

procedure TcxTabSheet.SetTabVisible(const Value: Boolean);
begin
  if Value <> FTabVisible then
  begin
    FTabVisible := Value;
    PagePropertyChanged;
    if Value and (PageControl <> nil) and (PageControl.PageCount = 1) and (PageControl.ActivePage = nil) then
      PageControl.ActivePage := Self;
  end;
end;

function TcxTabControl.GetPropertiesClass: TcxCustomTabControlPropertiesClass;
begin
  Result := TcxTabControlProperties;
end;

function TcxTabControl.GetProperties: TcxTabControlProperties;
begin
  Result := TcxTabControlProperties(inherited Properties);
end;

procedure TcxTabControl.SetProperties(Value: TcxTabControlProperties);
begin
  inherited Properties := Value;
end;

initialization

finalization
  FreeAndNil(FBackgroundBitmap);

end.

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

unit dxRibbon;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Forms, Messages, SysUtils, Graphics, Controls, ExtCtrls, ImgList, IniFiles, Contnrs,
  Generics.Collections, Generics.Defaults, Classes,
  dxCore, dxCoreClasses, dxMessages, cxClasses, cxGraphics, dxTouch, cxControls, dxBar, cxContainer, cxLookAndFeels,
  dxScreenTip, dxBarSkin, dxFading, cxAccessibility, dxBarAccessibility, cxLookAndFeelPainters, dxRibbonSkins, dxForms,
  dxRibbonForm, dxRibbonFormCaptionHelper, dxBarApplicationMenu, dxShadowWindow, cxScrollBar, dxGdiPlusClasses,
  cxGeometry, dxCoreGraphics, dxAnimation;

const
  dxRibbonGroupMinContentWidth        = 30;
  dxRibbonHintOffset                  = 20;
  dxRibbonFormCaptionMinWidth         = 50;
  dxRibbonFormContextsMinWidth        = 8;
  dxRibbonFormCaptionTextSpace        = 4;
  dxRibbonTabIndent                   = 17;
  dxRibbonTabMinWidth                 = 28;
  dxRibbonTabTextOffset               = 5;
  dxRibbonOptimalTabSpace             = dxRibbonTabTextOffset * 2 + dxRibbonTabIndent;
  dxRibbonOwnerMinimalWidth: Integer  = 300;
  dxRibbonOwnerMinimalHeight: Integer = 250;

  dxRibbonGroupRowCount = 3;
  dxRibbonTabletGroupRowCount = 1;

  dxRibbonScrollDelay = 400;
  dxRibbonScrollInterval = 20;

  dxRibbonContextualTabLabelShowAnimationTime: Integer = 300;
  dxRibbonTabGroupContentShowAnimationTime: Integer = 200;
  dxRibbonTabGroupsPopupWindowHideAnimationTime: Integer = 100;
  dxRibbonTabGroupsPopupWindowShowAnimationTime: Integer = 230;
  dxRibbonTabStateChangeAnimationTime: Integer = 200;

type
  TdxRibbonMergeOption = (rmoCanCreateNewTab, rmoCanCreateNewGroup,
    rmoCanCreateQATToolbar, rmoCanCreateTabAreaToolbar, rmoCanCreateTabAreaSearchToolbar);
  TdxRibbonMergeOptions = set of TdxRibbonMergeOption;

  TdxRibbonPopupMenuItem = (rpmiItems, rpmiMoreCommands, rpmiQATPosition,
    rpmiQATAddRemoveItem, rpmiMinimizeRibbon, rpmiCustomizeRibbon, rpmiCustomizeQAT);
  TdxRibbonPopupMenuItems = set of TdxRibbonPopupMenuItem;

const
  dxRibbonDefaultMergeOptions = [rmoCanCreateNewTab, rmoCanCreateNewGroup, rmoCanCreateQATToolbar];
  dxRibbonDefaultPopupMenuItems = [rpmiItems, rpmiMoreCommands, rpmiQATPosition,
    rpmiQATAddRemoveItem, rpmiMinimizeRibbon, rpmiCustomizeRibbon, rpmiCustomizeQAT];

type
  TdxBarApplicationMenu = class;
  TdxCustomRibbon = class;
  TdxCustomRibbonDockControl = class;
  TdxRibbonApplicationButton = class;
  TdxRibbonApplicationButtonViewInfo = class;
  TdxRibbonButtonsContainerViewInfo = class;
  TdxRibbonCollapsedGroupPopupBarControl = class;
  TdxRibbonContext = class;
  TdxRibbonContexts = class;
  TdxRibbonContextsViewInfo = class;
  TdxRibbonController = class;
  TdxRibbonCustomButtonViewInfo = class;
  TdxRibbonCustomToolbar = class;
  TdxRibbonGroupBarControl = class;
  TdxRibbonGroupBarControlViewInfo = class;
  TdxRibbonGroupsDockControl = class;
  TdxRibbonGroupsDockControlSite = class;
  TdxRibbonGroupsDockControlSiteViewInfo = class;
  TdxRibbonGroupsDockControlViewInfo = class;
  TdxRibbonGroupsDockControlViewInfoClass = class of TdxRibbonGroupsDockControlViewInfo;
  TdxRibbonHelpButton = class;
  TdxRibbonPopupMenu = class;
  TdxRibbonQuickAccessGroupButton = class;
  TdxRibbonQuickAccessToolbar = class;
  TdxRibbonQuickAccessToolbarBarControlViewInfo = class;
  TdxRibbonQuickAccessToolbarDockControl = class;
  TdxRibbonQuickAccessToolbarViewInfo = class;
  TdxRibbonTab = class;
  TdxRibbonTabAreaSearchToolbar = class;
  TdxRibbonTabAreaToolbar = class;
  TdxRibbonTabGroup = class;
  TdxRibbonTabViewInfo = class;
  TdxRibbonTabsViewInfo = class;
  TdxRibbonViewInfo = class;

  TdxRibbonApplicationMenuDisplayMode = (amdmPopup, amdmFrame, amdmFrameFullScreen);
  TdxRibbonMergeKind = (rmkMerge, rmkAdd, rmkNone);

  TdxRibbonScrollButton = (rsbLeft, rsbRight);
  TdxRibbonScrollButtons = set of TdxRibbonScrollButton;

  TdxRibbonCustomizationFormMode = (rcfmCustomizeRibbon, rcfmCustomizeQuickAccessToolbar);
  TdxRibbonShowCustomizationFormFunc = function(ARibbon: TdxCustomRibbon; const AMode: TdxRibbonCustomizationFormMode): Boolean;

  TdxRibbonAddAccessibilityHelperProc = reference to procedure (AAccessibilityHelper: IdxBarAccessibilityHelper);

  { IdxRibbonApplicationMenu }

  IdxRibbonApplicationMenu = interface
  ['{DF34053B-F30D-4FC6-94D5-5863620E8F28}']
    function CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
    function ClosePopup: Boolean;
    function GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
    function GetOrigin(AIsClientArea: Boolean): TPoint;
    function GetRootAccessibilityHelper: IdxBarAccessibilityHelper;
    procedure GetTabOrderList(List: TList);
    function IsVisible: Boolean;
    function Popup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean;
    procedure RibbonFormResized;
    procedure SelectAppMenuFirstItemControl;
    procedure ShowKeyTips;
    procedure UpdateNonClientArea;
  end;

  { IdxRibbonListener }

  IdxRibbonListener = interface
  ['{E3CD1F8D-4F7F-4448-A877-28726BDDD97F}']
    procedure AfterBarManagerChange;
    procedure BeforeBarManagerChange;
  end;

  { IdxRibbonMouseWheelReceiver }

  IdxRibbonMouseWheelReceiver = interface
  ['{A5D2167B-0343-4525-915F-D41B49832045}']
    function CanProcessMouseWheel: Boolean;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
  end;

  { IdxRibbonToolbarContainer }

  IdxRibbonToolbarContainer = interface
  ['{4C6EF60C-C784-44B7-A40E-382ADDB36F61}']
    function GetRibbon: TdxCustomRibbon;
    function GetToolbar: TdxBar;
    procedure SetToolbar(AValue: TdxBar);
    //
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property Toolbar: TdxBar read GetToolbar write SetToolbar;
  end;

  { IdxRibbonFormNonClientDraw }

  IdxRibbonFormNonClientDraw = interface
  ['{0A28260B-C352-4704-A88B-44DD8461955C}']
    procedure Add(AObject: TObject);
    procedure Changed(AObject: TObject);
    procedure Remove(AObject: TObject);
  end;

  { IdxRibbonFormStatusBar }

  IdxRibbonFormStatusBar = interface
  ['{E6AA56DF-B87A-4D98-98CF-B41BA751594D}']
    function GetActive(AForm: TCustomForm): Boolean;
    function GetControl: TWinControl;
    function GetHeight: Integer;
    function GetIsRaised(ALeft: Boolean): Boolean;
  end;

  { TdxRibbonCustomMergeData }

  TdxRibbonCustomMergeData = class(TdxBarCustomMergeData)
  private
    FChildren: TcxComponentList;
    procedure ListChangeHandler(Sender: TObject; AComponent: TComponent; AAction: TListNotification);
  protected
    procedure FreeNotification(Sender: TComponent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    //
    property Children: TcxComponentList read FChildren;
  end;

  { TdxCustomDesignSelectionHelper }

  TdxCustomDesignSelectionHelper = class(TInterfacedObject, IdxBarSelectableItem)
  private
    FComponent: TComponent;
    FOwner: TPersistent;
    FParent: TPersistent;
  protected
    //IdxBarSelectableItem
    function CanDelete(ADestruction: Boolean = False): Boolean;
    procedure DeleteSelection(var AReference: IdxBarSelectableItem; ADestruction: Boolean);
    procedure ExecuteCustomizationAction(ABasicAction: TdxBarCustomizationAction);
    function GetBarManager: TdxBarManager; virtual;
    function GetInstance: TPersistent;
    procedure GetMasterObjects(AList: TdxObjectList);
    function GetNextSelectableItem: IdxBarSelectableItem;
    function GetSelectableParent: TPersistent;
    function GetSelectionStatus: TdxBarSelectionStatus;
    function GetSupportedActions: TdxBarCustomizationActions;
    procedure Invalidate; virtual;
    function IsComplex: Boolean;
    function IsComponentSelected: Boolean;
    procedure SelectComponent(ASelectionOperation: TdxBarSelectionOperation = soExclusive);
    function SelectParentComponent: Boolean;
    procedure SelectionChanged;
    //
    property BarManager: TdxBarManager read GetBarManager;
    property Component: TComponent read FComponent;
    property Owner: TPersistent read FOwner;
    property Parent: TPersistent read FParent;
  public
    constructor Create(AComponent: TComponent; AOwner, AParent: TPersistent);
  end;

  { TdxDesignSelectionHelper }

  TdxDesignSelectionHelper = class(TdxCustomDesignSelectionHelper)
  protected
    function GetBarManager: TdxBarManager; override;
    function GetRibbon: TdxCustomRibbon;
    procedure Invalidate; override;
    //
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonPainter }

  TdxRibbonPainter = class(TcxIUnknownObject, IdxRibbonPaintData)
  strict private const
    MaxFormIconCacheSize = 2;
  strict private
    FFormIconCache: TcxObjectList;
    FRibbon: TdxCustomRibbon;

    function GetColorScheme: TdxCustomRibbonSkin;
    function GetColorSchemeAccent: TdxRibbonColorSchemeAccent;
    function GetIsFormZoomed: Boolean;
    function GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
    function GetViewInfo: TdxRibbonViewInfo;
  protected
    procedure DrawEmptyRibbon(ACanvas: TcxCanvas);

    // IdxRibbonPaintData
    function GetApplicationMenuState: TdxRibbonApplicationMenuState;
    function GetCaptionAreaExtension: Integer;
    function GetCaptionHeight: Integer;
    function GetRibbonFormPaintData: IdxRibbonFormPaintData;
    function GetRibbonHeight: Integer;
    function GetTabsHeight: Integer;
    function HasStatusBar: Boolean;
    function IsRibbonHidden: Boolean;
    function IsQuickAccessToolbarBelowRibbon: Boolean;
  public
    constructor Create(ARibbon: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    procedure AdjustContextFont(AFont: TFont; AContextColor: TColor);
    procedure FlushCache;

    function GetGroupCaptionBottomOffset(AInPopup: Boolean = False): Integer; virtual;

    //non-client routines
    procedure DrawRibbonFormCaption(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawRibbonFormCaptionFrameArea(ACanvas: TcxCanvas; R: TRect); virtual;
    procedure DrawRibbonFormBorderIcon(ACanvas: TcxCanvas; const ABounds: TRect;
      AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState); virtual;
    procedure DrawRibbonFormBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect); virtual;

    // Form Icon
    procedure DrawDefaultFormIcon(ACanvas: TcxCanvas; const ABounds: TRect);
    function GetDefaultFormIcon(const ASize: TSize): TdxGPImage;
    function GetDefaultFormIconHandle: HICON;

    //client routines
    procedure DrawApplicationButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TdxRibbonApplicationButtonState); virtual;
    procedure DrawApplicationButtonGlyph(ACanvas: TcxCanvas; const ABounds: TRect; AGlyph: TdxSmartGlyph); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawBackgroundImage(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawContextGroupsArea(ACanvas: TcxCanvas; const ABounds: TRect; AIsQuickAccessToolbarAtBottom, AIsInPopup: Boolean); virtual;
    procedure DrawGlyph(ACanvas: TcxCanvas; AGlyph: TdxSmartGlyph; const ABounds: TRect); virtual;
    procedure DrawGlowingText(DC: HDC; const AText: string; AFont: TFont;
      const ABounds: TRect; AColor: TColor; AFlags: DWORD; ATransparent: Boolean = False);
    procedure DrawGroupsArea(ACanvas: TcxCanvas; const ABounds: TRect;
      AIsAllowContextPaint: Boolean = True; AIsInPopup: Boolean = False); virtual;
    procedure DrawGroupsScrollButton(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean; AState: TcxButtonState); virtual;
    procedure DrawGroupsScrollButtonArrow(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean); virtual;
    procedure DrawRibbonFormCaptionText(ACanvas: TcxCanvas; const ABounds: TRect; const ADocumentName, ACaption: string); virtual;
    procedure DrawRibbonGlassFormCaptionText(ACanvas: TcxCanvas; const ABounds: TRect;
      const ADocumentName, ACaption: string; AIsActive: Boolean); virtual;
    procedure DrawRibbonTopFrameAreaSeparator(ACanvas: TcxCanvas); virtual;
    procedure DrawTabAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawTabBase(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawTabScrollButton(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean; AState: TcxButtonState); virtual;
    procedure DrawTabScrollButtonGlyph(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean); virtual;
    procedure DrawHelpButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState); virtual;
    procedure DrawHelpButtonGlyph(ACanvas: TcxCanvas; AGlyph: TdxSmartGlyph; const ABounds: TRect; AState: TcxButtonState); virtual;
    procedure DrawMDIButton(ACanvas: TcxCanvas; const ABounds: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); virtual;
    procedure DrawMDIButtonGlyph(ACanvas: TcxCanvas; const ABounds: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState); virtual;
    procedure DrawMinimizeButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AMinimized: Boolean);
    procedure DrawMinimizeButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
    function GetTabContentOffset: TRect;

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property ColorSchemeAccent: TdxRibbonColorSchemeAccent read GetColorSchemeAccent;
    property IsFormZoomed: Boolean read GetIsFormZoomed;
    property QuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo read GetQuickAccessToolbarViewInfo;
    property Ribbon: TdxCustomRibbon read FRibbon;
    property ViewInfo: TdxRibbonViewInfo read GetViewInfo;
  end;

  { TdxRibbonHitInfo }

  TdxRibbonHitTest = (rhtNone, rhtTab, rhtApplicationMenu, rhtContext, rhtTabScrollLeft, rhtTabScrollRight,
    rhtGroupScrollLeft, rhtGroupScrollRight, rhtHelpButton, rhtCustomButton, rhtFormButton); //keep order

  TdxRibbonHitInfo = class
  private
    FOwner: TdxCustomRibbon;
    FHitTest: TdxRibbonHitTest;
    FHitObject: TObject;
    FHitPoint: TPoint;

    function GetHitObjectAsButton: TdxRibbonCustomButtonViewInfo;
    function GetHitObjectAsContext: TdxRibbonContext;
    function GetHitObjectAsTab: TdxRibbonTab;
  public
    constructor Create(AOwner: TdxCustomRibbon); virtual;
    procedure Calculate(const P: TPoint); virtual;
    function Compare(const AHitTest: TdxRibbonHitInfo): Boolean;
    procedure Reset; virtual;
    //
    property HitObject: TObject read FHitObject write FHitObject;
    property HitObjectAsButton: TdxRibbonCustomButtonViewInfo read GetHitObjectAsButton;
    property HitObjectAsContext: TdxRibbonContext read GetHitObjectAsContext;
    property HitObjectAsTab: TdxRibbonTab read GetHitObjectAsTab;
    property HitPoint: TPoint read FHitPoint write Calculate;
    property HitTest: TdxRibbonHitTest read FHitTest write FHitTest;
    property Owner: TdxCustomRibbon read FOwner;
  end;

  { TdxRibbonCustomViewInfo }

  TdxRibbonCustomViewInfo = class(TcxIUnknownObject)
  strict private
    FOwner: TdxRibbonViewInfo;

    function GetRibbon: TdxCustomRibbon;
    function GetScaleFactor: TdxScaleFactor;
  protected
    FBounds: TRect;
    FIsRightToLeftConverted: Boolean;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
  public
    constructor Create(AOwner: TdxRibbonViewInfo); virtual;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateMetrics(ACanvas: TcxCanvas); virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual; abstract;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; virtual;
    procedure Invalidate;
    procedure InvalidateRect(const R: TRect); virtual;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    //
    property Bounds: TRect read FBounds;
    property Owner: TdxRibbonViewInfo read FOwner;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxRibbonCustomContainerViewInfo }

  TdxRibbonCustomContainerViewInfo = class(TdxRibbonCustomViewInfo)
  private
    FCells: TcxObjectList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TObject;
  public
    constructor Create(AOwner: TdxRibbonViewInfo); override;
    destructor Destroy; override;
    function AddItem(AObject: TObject): Integer;
    procedure Clear;
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read GetItem; default;
  end;

  { TdxRibbonCustomButtonAccessibilityHelper }

  TdxRibbonCustomButtonAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    FButtonViewInfo: TdxRibbonCustomButtonViewInfo;
    function GetRibbon: TdxCustomRibbon;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function GetNextAccessibleObject(
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure GetKeyTipData(AKeyTipsData: TList); override;

    property ButtonViewInfo: TdxRibbonCustomButtonViewInfo read FButtonViewInfo;
    property Ribbon: TdxCustomRibbon read GetRibbon;
  public
    constructor CreateEx(AButtonViewInfo: TdxRibbonCustomButtonViewInfo); virtual;
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxRibbonCustomButtonViewInfo }

  TdxRibbonCustomButtonViewInfoClass = class of TdxRibbonCustomButtonViewInfo;
  TdxRibbonCustomButtonViewInfo = class(TcxIUnknownObject,
    IdxFadingObject,
    IdxFadingObjectFadingOptions)
  private
    FAccessibilityHelper: IdxBarAccessibilityHelper;
    FBounds: TRect;
    FOwner: TdxRibbonCustomContainerViewInfo;
    FState: TcxButtonState;

    function GetAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetPainter: TdxRibbonPainter;
    function GetRibbon: TdxCustomRibbon;
    function GetState: TcxButtonState;
    function GetViewInfo: TdxRibbonViewInfo;
    procedure SetState(AValue: TcxButtonState);
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateAccessibilityHelper: TdxRibbonCustomButtonAccessibilityHelper; virtual;
    function GetHint: string; virtual;
    function GetIsEnabled: Boolean; virtual;
    procedure CalculateState; virtual;
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    // IdxFadingObject
    function CanFade: Boolean;
    procedure DrawFadeImage;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    // IdxFadingObjectFadingOptions
    function GetFadingOptions: TdxFadingOptions;
  public
    constructor Create(AOwner: TdxRibbonCustomContainerViewInfo); virtual;
    destructor Destroy; override;
    procedure Click; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure Invalidate;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; virtual;
    //
    property AccessibilityHelper: IdxBarAccessibilityHelper read GetAccessibilityHelper;
    property Bounds: TRect read FBounds write FBounds;
    property Enabled: Boolean read GetIsEnabled;
    property Hint: string read GetHint;
    property Owner: TdxRibbonCustomContainerViewInfo read FOwner;
    property Painter: TdxRibbonPainter read GetPainter;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxButtonState read GetState write SetState;
    property ViewInfo: TdxRibbonViewInfo read GetViewInfo;
  end;

  { TdxRibbonCustomScrollButtonViewInfo }

  TdxRibbonCustomScrollButtonViewInfo = class(TdxRibbonCustomButtonViewInfo)
  protected
    FScrollButton: TdxRibbonScrollButton;
  end;

  { TdxRibbonTabViewInfo }

  TdxRibbonTabViewInfoClass = class of TdxRibbonTabViewInfo;
  TdxRibbonTabViewInfo = class
  strict private
    FIsRightToLeftConverted: Boolean;
    FOwner: TdxRibbonTabsViewInfo;
    FTab: TdxRibbonTab;

    function GetColorScheme: TdxCustomRibbonSkin;
    function GetContextTabSeparatorWidth: Integer;
    function GetFont: TFont;
    function GetRibbon: TdxCustomRibbon;
    function GetScaleFactor: TdxScaleFactor;
  protected
    FBounds: TRect;
    FCanHasSeparator: Boolean;
    FContextBegin: Boolean;
    FContextEnd: Boolean;
    FMinWidth: Integer;
    FOptimalWidth: Integer;
    FSeparatorAlphaValue: Integer;
    FSeparatorBounds: TRect;
    FTextBounds: TRect;
    FWidth: Integer;

    procedure CalculateWidths(ACanvas: TcxCanvas; AViewInfo: TdxRibbonViewInfo); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect; AState: TdxRibbonTabState);
    procedure DrawText(ACanvas: TcxCanvas; AHasSeparator: Boolean);
    function GetState: TdxRibbonTabState; virtual;
    function IsSelected: Boolean;
    function IsTabActive: Boolean;
    function PrepareFadeImage(ADrawHot: Boolean): TcxBitmap;

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property ContextTabSeparatorWidth: Integer read GetContextTabSeparatorWidth;
    property Owner: TdxRibbonTabsViewInfo read FOwner;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property Width: Integer read FWidth;
  public
    constructor Create(ATab: TdxRibbonTab; AOwner: TdxRibbonTabsViewInfo); virtual;
    procedure Calculate(const ABounds: TRect; ASeparatorAlpha: Byte); virtual;
    procedure Draw(ACanvas: TcxCanvas);
    function HasSeparator: Boolean;
    function IsPaintOnGlass: Boolean;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Bounds: TRect read FBounds;
    property ContextBegin: Boolean read FContextBegin write FContextBegin;
    property ContextEnd: Boolean read FContextEnd write FContextEnd;
    property Font: TFont read GetFont;
    property MinWidth: Integer read FMinWidth;
    property OptimalWidth: Integer read FOptimalWidth;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SeparatorAlphaValue: Integer read FSeparatorAlphaValue;
    property SeparatorBounds: TRect read FSeparatorBounds;
    property State: TdxRibbonTabState read GetState;
    property Tab: TdxRibbonTab read FTab;
    property TextBounds: TRect read FTextBounds;
  end;

  { TdxRibbonTabsDropDownButtonViewInfo }

  TdxRibbonTabsDropDownButtonViewInfo = class(TdxRibbonCustomButtonViewInfo)
  private
    FMenu: TdxRibbonPopupMenu;

    procedure ActivateMenuItem(Sender: TObject);
    procedure ClearMenu(AItemLinks: TdxBarItemLinks);
    procedure PopulateMenuItemContent(AItemLinks: TdxBarItemLinks; ASource: TdxRibbonTab);
    procedure PopulateMenuItems(AItemLinks: TdxBarItemLinks);
    procedure ShowMenu;
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;

    property Menu: TdxRibbonPopupMenu read FMenu;
  public
    procedure Click; override;
  end;

  { TdxRibbonTabsScrollButtonViewInfo }

  TdxRibbonTabsScrollButtonViewInfo = class(TdxRibbonCustomScrollButtonViewInfo)
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
  end;

  { TdxRibbonTabsViewInfo }

  TdxRibbonTabsViewInfo = class(TdxRibbonCustomContainerViewInfo)
  strict private
    FBitmap: TcxBitmap32;

    procedure CheckScrollPosition(var Value: Integer);
    function GetController: TdxRibbonController;
    function GetPainter: TdxRibbonPainter;
    function GetRealMinItemWidth(Index: Integer): Integer;
    function GetScrollWidth: Integer;
    function GetTabViewInfo(Index: Integer): TdxRibbonTabViewInfo;
    procedure RemoveScrolling;
    procedure SetScrollPosition(Value: Integer);
  protected
    FNeedShowHint: Boolean;
    FScrollButtonLeft: TdxRibbonTabsScrollButtonViewInfo;
    FScrollButtonRight: TdxRibbonTabsScrollButtonViewInfo;
    FScrollButtons: TdxRibbonScrollButtons;
    FScrollPosition: Integer;
    FScrollWidth: Integer;
    FSeparatorAlpha: Byte;
    FTabContentOffset: TRect;
    FTotalMinimalWidth: Integer;
    FTotalOptimalWidth: Integer;

    function CreateTabViewInfo(ATab: TdxRibbonTab): TdxRibbonTabViewInfo; virtual;

    procedure BalancedReduce(ATotalDelta: Integer);
    procedure SimpleReduce(ATotalDelta: Integer);
    procedure CalculateLayout; virtual;
    procedure CalculateComplexTabLayout; virtual;
    procedure CalculateScrollButtons; virtual;
    procedure CalculateScrollingTabLayout; virtual;
    procedure CalculateSimpleTabLayout; virtual;
    procedure CalculateSimpleTabLayoutCore; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;

    procedure CreateContextsViewInfoCells; virtual;
    procedure CreateTabsViewInfoCells; virtual;
    procedure DrawCore(ACanvas: TcxCanvas); virtual;

    property Controller: TdxRibbonController read GetController;
  public
    constructor Create(AOwner: TdxRibbonViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateMetrics(ACanvas: TcxCanvas); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function Find(ATab: TdxRibbonTab): TdxRibbonTabViewInfo;
    function GetDesignHitTest(const P: TPoint): Boolean; virtual;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    function GetRealBounds: TRect; virtual;
    function GetTabAtPos(X, Y: Integer): TdxRibbonTab;
    function IndexOf(ATab: TdxRibbonTab): Integer;
    function Last: TdxRibbonTabViewInfo;
    procedure MakeTabVisible(ATab: TdxRibbonTab);
    function MeasureHeight(ACanvas: TcxCanvas): Integer; virtual;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
    procedure RecreateViewInfoCells;
    procedure UpdateButtonsStates; virtual;
    procedure UpdateDockControls;

    property Bitmap: TcxBitmap32 read FBitmap;
    property Items[Index: Integer]: TdxRibbonTabViewInfo read GetTabViewInfo; default;
    property NeedShowHint: Boolean read FNeedShowHint;
    property Painter: TdxRibbonPainter read GetPainter;
    property ScrollButtons: TdxRibbonScrollButtons read FScrollButtons;
    property ScrollPosition: Integer read FScrollPosition write SetScrollPosition;
  end;

  { TdxRibbonTabsViewInfo2016Tablet }

  TdxRibbonTabsViewInfo2016Tablet = class(TdxRibbonTabsViewInfo)
  protected
    FDropDownButton: TdxRibbonTabsDropDownButtonViewInfo;

    procedure CalculateLayout; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawCore(ACanvas: TcxCanvas); override;
  public
    constructor Create(AOwner: TdxRibbonViewInfo); override;
    destructor Destroy; override;
    function GetDesignHitTest(const P: TPoint): Boolean; override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    function GetRealBounds: TRect; override;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
    procedure UpdateButtonsStates; override;

    property DropDownButton: TdxRibbonTabsDropDownButtonViewInfo read FDropDownButton;
  end;

  { TdxRibbonTabsViewInfo2019 }

  TdxRibbonTabsViewInfo2019 = class(TdxRibbonTabsViewInfo)
  strict private const
    TabBaseHeight = 3;
  strict private
    FTabBaseRectLTR: TRect;
    FTabBaseRectAnimation: TdxRectAnimationTransition;

    function GetActiveTabViewInfo: TdxRibbonTabViewInfo;
    procedure HandlerAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure HandlerAnimationTerminated(Sender: TObject);
  protected
    FTabBaseRect: TRect;

    procedure DrawCore(ACanvas: TcxCanvas); override;
    // TabBase
    procedure CalculateTabBaseRect; virtual;
    function CanAnimateTabBaseRectChange: Boolean; virtual;
    function CreateTabBaseAnimation(const AOldRect, ANewRect: TRect): TdxRectAnimationTransition; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure UpdateTabBaseRect(const AArea: TRect); virtual;
  public
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    function MeasureHeight(ACanvas: TcxCanvas): Integer; override;
    procedure UpdateButtonsStates; override;
    //
    property ActiveTabViewInfo: TdxRibbonTabViewInfo read GetActiveTabViewInfo;
  end;

  { TdxRibbonHelpButtonViewInfo }

  TdxRibbonHelpButtonViewInfo = class(TdxRibbonCustomButtonViewInfo)
  strict private
    function GetGlyphBounds: TRect;
    function GetHelpButton: TdxRibbonHelpButton;
  protected
    function GetIsEnabled: Boolean; override;
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
  public
    procedure Click; override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;

    property HelpButton: TdxRibbonHelpButton read GetHelpButton;
    property GlyphBounds: TRect read GetGlyphBounds;
  end;

  { TdxRibbonMinimizeButtonViewInfo }

  TdxRibbonMinimizeButtonViewInfo = class(TdxRibbonCustomButtonViewInfo)
  strict private
    function GetState: TdxRibbonMinimizeButtonGlyph;
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    function GetHint: string; override;
  public
    procedure Click; override;
    //
    property State: TdxRibbonMinimizeButtonGlyph read GetState;
  end;

  { TdxRibbonMDIButtonViewInfo }

  TdxRibbonMDIButtonViewInfo = class(TdxRibbonCustomButtonViewInfo)
  protected
    FButtonType: TdxBarMDIButton;
    function GetHint: string; override;
    function GetIsEnabled: Boolean; override;
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
  public
    procedure Click; override;
    //
    property ButtonType: TdxBarMDIButton read FButtonType;
  end;

  { TdxRibbonButtonsContainerViewInfo }

  TdxRibbonButtonsContainerViewInfo = class(TdxRibbonCustomContainerViewInfo)
  strict private
    function GetItem(Index: Integer): TdxRibbonCustomButtonViewInfo;
    function GetSelectedButton: TdxRibbonCustomButtonViewInfo;
  protected
    procedure CalculateButtonsBounds(const ABounds: TRect); virtual; abstract;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure UpdateButtonsState;
  public
    procedure Add(AClass: TdxRibbonCustomButtonViewInfoClass);
    procedure Calculate(const ABounds: TRect); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
    //
    property SelectedButton: TdxRibbonCustomButtonViewInfo read GetSelectedButton;
    property Items[Index: Integer]: TdxRibbonCustomButtonViewInfo read GetItem; default;
  end;

  { TdxRibbonTabsAreaButtonsViewInfo }

  TdxRibbonTabsAreaButtonsViewInfo = class(TdxRibbonButtonsContainerViewInfo)
  protected
    procedure CalculateButtonsBounds(const ABounds: TRect); override;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
  end;

  { TdxRibbonContextViewInfo }

  TdxRibbonContextViewInfo = class
  strict private
    FContext: TdxRibbonContext;
    FIsRightToLeftConverted: Boolean;
    FOwner: TdxRibbonContextsViewInfo;
    FText: string;

    function GetColorScheme: TdxCustomRibbonSkin;
    function GetFont: TFont;
    function GetFontShadowColor: TColor;
    procedure InternalDrawText(ACanvas: TcxCanvas; const R: TRect; ATextColor: TColor; AHasGlowing: Boolean);
  protected
    FBounds: TRect;
    FNeedShowHint: Boolean;
    FTextBounds: TRect;
    FTextWidth: Integer;

    function CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawText(ACanvas: TcxCanvas);

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property Owner: TdxRibbonContextsViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxRibbonContextsViewInfo; AContext: TdxRibbonContext); virtual;
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure Paint(ACanvas: TcxCanvas);
    function IsPaintOnGlass: Boolean;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Bounds: TRect read FBounds;
    property Context: TdxRibbonContext read FContext;
    property Font: TFont read GetFont;
    property FontShadowColor: TColor read GetFontShadowColor;
    property NeedShowHint: Boolean read FNeedShowHint;
    property Text: string read FText;
    property TextBounds: TRect read FTextBounds;
  end;

  { TdxRibbonContextViewInfo2007 }

  TdxRibbonContextViewInfo2007 = class(TdxRibbonContextViewInfo)
  protected
    function CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect; override;
  end;

  { TdxRibbonContextViewInfo2010OrNewer }

  TdxRibbonContextViewInfo2010OrNewer = class(TdxRibbonContextViewInfo)
  protected
    function CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect; override;
  end;

  { TdxRibbonContextsViewInfo }

  TdxRibbonContextsViewInfo = class(TdxRibbonCustomContainerViewInfo)
  strict private
    function GetItem(Index: Integer): TdxRibbonContextViewInfo;
  protected
    function CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
  public
    procedure Calculate(const ABounds: TRect); override;
    function CalculateCaptionWidth(ACanvas: TcxCanvas; const ACaption: string): Integer;
    procedure Draw(ACanvas: TcxCanvas); override;
    function Find(AContext: TdxRibbonContext): TdxRibbonContextViewInfo;
    function GetContextFont(AColor: TColor = clDefault): TFont;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    function NeedShowHint(AContext: TdxRibbonContext): Boolean;
    //
    property Items[Index: Integer]: TdxRibbonContextViewInfo read GetItem; default;
  end;

  { TdxRibbonContextsViewInfo2007 }

  TdxRibbonContextsViewInfo2007 = class(TdxRibbonContextsViewInfo)
  protected
    function CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo; override;
  end;

  { TdxRibbonContextsViewInfo2010OrNewer }

  TdxRibbonContextsViewInfo2010OrNewer = class(TdxRibbonContextsViewInfo)
  protected
    function CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo; override;
  end;

  { TdxRibbonApplicationButtonViewInfo }

  TdxRibbonApplicationButtonViewInfo = class(TdxRibbonCustomViewInfo,
    IdxFadingObject,
    IdxFadingObjectFadingOptions)
  strict private
    FIsHot: Boolean;
    FIsPressed: Boolean;

    function GetApplicationButton: TdxRibbonApplicationButton;
    function GetClientBounds: TRect;
    function GetFont: TFont;
    function GetPainter: TdxRibbonPainter;
    function GetState: TdxRibbonApplicationButtonState;
    procedure SetIsHot(AValue: Boolean);
    procedure SetIsPressed(AValue: Boolean);
  protected
    FClientOffsets: TRect;
    FGlyphBounds: TRect;
    FGlyphOffsets: TRect;

    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    function MeasureSizeCore: TSize; virtual;
    // IdxFadingObject
    function CanFade: Boolean;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    procedure IdxFadingObject.DrawFadeImage = Invalidate;
    // IdxFadingObjectFadingOptions
    function GetFadingOptions: TdxFadingOptions;

    property IsHot: Boolean read FIsHot write SetIsHot;
    property IsPressed: Boolean read FIsPressed write SetIsPressed;
    property Painter: TdxRibbonPainter read GetPainter;
  public
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateMetrics(ACanvas: TcxCanvas); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    procedure InvalidateRect(const R: TRect); override;
    function IsPaintOnGlass: Boolean;
    function IsVisible: Boolean; virtual;
    function MeasureSize: TSize; virtual;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
    //
    property ApplicationButton: TdxRibbonApplicationButton read GetApplicationButton;
    property Bounds: TRect read FBounds;
    property ClientBounds: TRect read GetClientBounds;
    property ClientOffsets: TRect read FClientOffsets;
    property Font: TFont read GetFont;
    property GlyphBounds: TRect read FGlyphBounds;
    property GlyphOffsets: TRect read FGlyphOffsets;
    property State: TdxRibbonApplicationButtonState read GetState;
  end;

  { TdxRibbonApplicationButtonViewInfo2010OrNewer }

  TdxRibbonApplicationButtonViewInfo2010OrNewer = class(TdxRibbonApplicationButtonViewInfo)
  strict private
    function GetTextBounds: TRect;
    function GetTextOffsets: TRect;
  protected
    FTextSize: TSize;

    procedure DrawContent(ACanvas: TcxCanvas); override;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    function HasText: Boolean;
    function MeasureSizeCore: TSize; override;
  public
    procedure CalculateMetrics(ACanvas: TcxCanvas); override;
    //
    property TextBounds: TRect read GetTextBounds;
    property TextOffsets: TRect read GetTextOffsets;
  end;

  { TdxRibbonTabsAreaViewInfo }

  TdxRibbonTabsAreaViewInfo = class(TdxRibbonCustomViewInfo)
  strict private
    FButtonsViewInfo: TdxRibbonTabsAreaButtonsViewInfo;
    FMeasuredHeight: Integer;
    FTabs: TdxRibbonTabsViewInfo;

    function GetSearchToolbar: TdxRibbonCustomToolbar;
    function GetToolbar: TdxRibbonCustomToolbar;
  protected
    FExtendedCaptionAreaRect: TRect;
    FSearchToolbarBounds: TRect;
    FToolbarBounds: TRect;

    procedure CalculateContent(var ARect: TRect); virtual;
    procedure CalculateSearchToolbar(var ARect: TRect); virtual;
    procedure CalculateTabs(var ARect: TRect); virtual;
    procedure CalculateTabsAreaButtons(var ARect: TRect); virtual;
    procedure CalculateTabsAreaToolbar(var ARect: TRect); virtual;
    function CalculateToolbarSize(ABar: TdxBar; AAvailableWidth: Integer): TSize;
    procedure CreateButtons; virtual;
    function CreateButtonsViewInfo: TdxRibbonTabsAreaButtonsViewInfo; virtual;
    function CreateTabsViewInfo: TdxRibbonTabsViewInfo; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function MeasureHeight(ACanvas: TcxCanvas): Integer; virtual;
    procedure UpdateToolbarDockControls;
  public
    constructor Create(AOwner: TdxRibbonViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateMetrics(ACanvas: TcxCanvas); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetDesignHitTest(const P: TPoint): Boolean; virtual;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
    function IsSearchToolbarVisible: Boolean; virtual;
    function IsTabsVisible: Boolean; virtual;
    function IsToolbarVisible: Boolean; virtual;
    function IsToolbarVisibleOnBackstageView: Boolean; virtual;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
    procedure RecreateViewInfoCells; virtual;
    procedure UpdateButtonsState; virtual;
    //
    property ButtonsViewInfo: TdxRibbonTabsAreaButtonsViewInfo read FButtonsViewInfo;
    property ExtendedCaptionAreaRect: TRect read FExtendedCaptionAreaRect;
    property MeasuredHeight: Integer read FMeasuredHeight;
    property SearchToolbar: TdxRibbonCustomToolbar read GetSearchToolbar;
    property SearchToolbarBounds: TRect read FSearchToolbarBounds;
    property TabsViewInfo: TdxRibbonTabsViewInfo read FTabs;
    property Toolbar: TdxRibbonCustomToolbar read GetToolbar;
    property ToolbarBounds: TRect read FToolbarBounds;
  end;

  { TdxRibbonTabsAreaViewInfo2010OrNewer }

  TdxRibbonTabsAreaViewInfo2010OrNewer = class(TdxRibbonTabsAreaViewInfo)
  strict private
    function GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo2010OrNewer;
  protected
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function MeasureHeight(ACanvas: TcxCanvas): Integer; override;
  public
    procedure Calculate(const ABounds: TRect); override;
    //
    property ApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo2010OrNewer read GetApplicationButtonViewInfo;
  end;

  { TdxRibbonTabsAreaViewInfo2010 }

  TdxRibbonTabsAreaViewInfo2010 = class(TdxRibbonTabsAreaViewInfo2010OrNewer)
  protected
    procedure CreateButtons; override;
  end;

  { TdxRibbonTabsAreaViewInfo2016Tablet }

  TdxRibbonTabsAreaViewInfo2016Tablet = class(TdxRibbonTabsAreaViewInfo2010OrNewer)
  strict private
    function GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
  protected
    procedure CalculateContent(var ARect: TRect); override;
    function CreateTabsViewInfo: TdxRibbonTabsViewInfo; override;
  public
    property QuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo read GetQuickAccessToolbarViewInfo;
  end;

  { TdxRibbonTabsAreaViewInfo2019 }

  TdxRibbonTabsAreaViewInfo2019 = class(TdxRibbonTabsAreaViewInfo2010OrNewer)
  protected
    function CreateTabsViewInfo: TdxRibbonTabsViewInfo; override;
  public
    function IsToolbarVisibleOnBackstageView: Boolean; override;
  end;

  { TdxRibbonQuickAccessToolbarViewInfo }

  TdxRibbonQuickAccessToolbarViewInfo = class(TdxRibbonCustomViewInfo)
  strict private
    function GetColorScheme: TdxCustomRibbonSkin;
    function GetDockControl: TdxRibbonQuickAccessToolbarDockControl;
    function GetMeasuredHeight: Integer;
    function GetToolbar: TdxRibbonQuickAccessToolbar;
  protected
    FAtBottom: Boolean;
    FAtNonClientArea: Boolean;
    FAtTop: Boolean;
    FControlSize: TSize;
    FDockControlBounds: TRect;
    FDockControlOffsets: TRect;
    FVisible: Boolean;

    procedure CalculateControlSize(AAvailableWidth: Integer);
    procedure CalculateDockControlOffsets; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
  public
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateMetrics(ACanvas: TcxCanvas); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    function IsActive: Boolean;
    function IsNonClientDrawingSupported: Boolean; virtual;
    function IsPaintOnGlass: Boolean;

    property AtBottom: Boolean read FAtBottom;
    property AtNonClientArea: Boolean read FAtNonClientArea;
    property AtTop: Boolean read FAtTop;
    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property ControlSize: TSize read FControlSize;
    property DockControl: TdxRibbonQuickAccessToolbarDockControl read GetDockControl;
    property DockControlBounds: TRect read FDockControlBounds;
    property DockControlOffsets: TRect read FDockControlOffsets;
    property MeasuredHeight: Integer read GetMeasuredHeight;
    property Toolbar: TdxRibbonQuickAccessToolbar read GetToolbar;
    property Visible: Boolean read FVisible;
  end;

  { TdxRibbonQuickAccessToolbarViewInfo2016Tablet }

  TdxRibbonQuickAccessToolbarViewInfo2016Tablet = class(TdxRibbonQuickAccessToolbarViewInfo)
  protected
    procedure CalculateDockControlOffsets; override;
  public
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    function IsNonClientDrawingSupported: Boolean; override;
  end;

  { TdxRibbonViewInfo }

  TdxRibbonViewInfoClass = class of TdxRibbonViewInfo;
  TdxRibbonViewInfo = class
  private
    FApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
    FBounds: TRect;
    FContextsViewInfo: TdxRibbonContextsViewInfo;
    FDrawEmptyRibbon: Boolean;
    FFont: TFont;
    FFormCaptionBounds: TRect;
    FFormCaptionTheLeftOfContext: Boolean;
    FGroupsDockControlSiteBounds: TRect;
    FGroupsDockControlSiteViewInfo: TdxRibbonGroupsDockControlSiteViewInfo;
    FHidden: Boolean;
    FIsRightToLeftConverted: Boolean;
    FQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
    FRibbon: TdxCustomRibbon;
    FSupportNonClientDrawing: Boolean;
    FTabGroupsDockControlBounds: TRect;
    FTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
    FUseGlass: Boolean;

    function GetColorScheme: TdxCustomRibbonSkin;
    function GetHasActiveContextTab: Boolean;
    function GetIsFormCaptionActive: Boolean;
    function GetIsTabsOnGlass: Boolean;
    function GetPainter: TdxRibbonPainter; inline;
    function GetScaleFactor: TdxScaleFactor;
    function GetScrollButtonWidth: Integer;
    procedure UpdateGroupsDockControlSite;
  protected
    FBackgroundImageBounds: TRect;
    FBackgroundImageClipBounds: TRect;

    procedure CalculateBounds; virtual;
    procedure CalculateMetrics;
    procedure CalculateMetricsCore(ACanvas: TcxCanvas); virtual;
    procedure RecreateViewInfoCells; virtual;

    procedure CalculateContexts; virtual;
    procedure CalculateQuickAccessToolbar; virtual;
    procedure CalculateRibbonFormCaption; virtual;
    procedure CalculateTabGroups; virtual;
    procedure CalculateTabsArea; virtual;

    function CreateApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo; virtual;
    function CreateContextsViewInfo: TdxRibbonContextsViewInfo; virtual;
    function CreateQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo; virtual;
    function CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo; virtual;

    function GetBounds: TRect; virtual;
    function GetNonClientAreaHeight: Integer; virtual;
    function GetNonClientAreaObjectsRegion: HRGN; virtual;
    function GetRibbonHeight: Integer; virtual;
    function GetTabsVerticalOffset: Integer; virtual;

    //form caption
    function GetCaption: string; virtual;
    function GetDocumentName: string; virtual;
    function GetRibbonFormCaptionClientBounds: TRect; virtual;
    function GetRibbonFormCaptionTextBounds: TRect; virtual;
    function GetRibbonFormCaptionTextBoundsWithContext(const ABounds, ACenterRect: TRect; ATextWidth: Integer): TRect; virtual;

    //TabGroups
    function GetGroupsDockControlSiteBounds: TRect; virtual;
    function GetTabGroupsContentOffset: TRect; virtual;
    function GetTabGroupsDockControlBounds: TRect; virtual;
    function GetTabGroupsDockControlOffset: TRect; virtual;
    function GetTabGroupsHeight(AIgnoreHidden: Boolean = False): Integer; virtual;

    //MDI support
    function HasMDIButtons: Boolean;
    function IsMDIButtonEnabled(AButton: TdxBarMDIButton; AState: Integer): Boolean;

    function CanShowBarControls(AIgnoreHidden: Boolean = False): Boolean;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DrawRibbonBackground(ACanvas: TcxCanvas);
    function IsNeedDrawBottomLine: Boolean;
    function IsNeedHideControl: Boolean;
    procedure UpdateButtonsStates;
    procedure UpdateNonClientParams;
    procedure UpdateToolbarDockControl(ADockControl: TdxCustomRibbonDockControl; AIsVisible: Boolean; const ABounds: TRect);

    function Style: TdxRibbonStyle; deprecated;

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property DrawEmptyRibbon: Boolean read FDrawEmptyRibbon;
    property IsTabsOnGlass: Boolean read GetIsTabsOnGlass;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ScrollButtonWidth: Integer read GetScrollButtonWidth;
    property UseGlass: Boolean read FUseGlass;
  public
    constructor Create(ARibbon: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure CalculateHitInfo(AHitTest: TdxRibbonHitInfo); virtual;
    function AdjustCaptionFontSize(ASize: Integer): Integer;
    function CanDrawDefaultFormIcon: Boolean; virtual;
    function GetDesignHitTest(const P: TPoint): Boolean; virtual;
    function GetDocumentNameTextColor(AIsActive: Boolean): TColor; virtual;
    function GetFormCaptionFont(AIsActive: Boolean): TFont; virtual;
    function GetFormCaptionText: TCaption;
    function GetTabAtPos(X, Y: Integer): TdxRibbonTab;
    procedure RightToLeftConversion(const ABounds: TRect);

    function IsApplicationButtonNearQAT: Boolean; virtual;
    function IsContextsVisible: Boolean;
    function IsTabGroupsVisible(AIgnoreHidden: Boolean = False): Boolean;
    function IsTabsVisible: Boolean;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); virtual;

    property ApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo read FApplicationButtonViewInfo;
    property BackgroundImageBounds: TRect read FBackgroundImageBounds;
    property BackgroundImageClipBounds: TRect read FBackgroundImageClipBounds;
    property Bounds: TRect read FBounds;
    property Caption: string read GetCaption;
    property DocumentName: string read GetDocumentName;
    property FormCaptionBounds: TRect read FFormCaptionBounds;
    property Hidden: Boolean read FHidden;

    property HasActiveContextTab: Boolean read GetHasActiveContextTab;
    property IsFormCaptionActive: Boolean read GetIsFormCaptionActive;
    property Painter: TdxRibbonPainter read GetPainter;
    property Ribbon: TdxCustomRibbon read FRibbon;
    property SupportNonClientDrawing: Boolean read FSupportNonClientDrawing;

    property ContextsViewInfo: TdxRibbonContextsViewInfo read FContextsViewInfo;
    property GroupsDockControlSiteBounds: TRect read FGroupsDockControlSiteBounds;
    property GroupsDockControlSiteViewInfo: TdxRibbonGroupsDockControlSiteViewInfo read FGroupsDockControlSiteViewInfo;
    property QuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo read FQuickAccessToolbarViewInfo;
    property TabGroupsDockControlBounds: TRect read FTabGroupsDockControlBounds;
    property TabsAreaViewInfo: TdxRibbonTabsAreaViewInfo read FTabsAreaViewInfo;
  end;

  { TdxRibbonViewInfo2007OrNewer }

  TdxRibbonViewInfo2007OrNewer = class(TdxRibbonViewInfo);

  { TdxRibbonViewInfo2007 }

  TdxRibbonViewInfo2007 = class(TdxRibbonViewInfo2007OrNewer)
  protected
    procedure CalculateBounds; override;
    function CreateContextsViewInfo: TdxRibbonContextsViewInfo; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetRibbonFormCaptionClientBounds: TRect; override;
  public
    function IsApplicationButtonNearQAT: Boolean; override;
  end;

  { TdxRibbonViewInfo2010OrNewer }

  TdxRibbonViewInfo2010OrNewer = class(TdxRibbonViewInfo2007OrNewer)
  protected
    function CreateApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo; override;
    function CreateContextsViewInfo: TdxRibbonContextsViewInfo; override;
    function CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo; override;
  public
    function CanDrawDefaultFormIcon: Boolean; override;
  end;

  { TdxRibbonViewInfo2010 }

  TdxRibbonViewInfo2010 = class(TdxRibbonViewInfo2010OrNewer)
  protected
    function CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo; override;
  end;

  { TdxRibbonViewInfo2013OrNewer }

  TdxRibbonViewInfo2013OrNewer = class(TdxRibbonViewInfo2010OrNewer)
  protected
    procedure CalculateBackgroundImage; virtual;
    procedure CalculateBounds; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetTabGroupsDockControlOffset: TRect; override;
  end;

  { TdxRibbonViewInfo2016OrNewer }

  TdxRibbonViewInfo2016OrNewer = class(TdxRibbonViewInfo2013OrNewer);

  { TdxRibbonViewInfo2016Tablet }

  TdxRibbonViewInfo2016Tablet = class(TdxRibbonViewInfo2016OrNewer)
  protected
    procedure CalculateQuickAccessToolbar; override;
    function CreateQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo; override;
    function CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo; override;
    function GetTabsVerticalOffset: Integer; override;
  public
    procedure PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc); override;
  end;

  { TdxRibbonViewInfo2019 }

  TdxRibbonViewInfo2019 = class(TdxRibbonViewInfo2016OrNewer)
  protected
    procedure CalculateBackgroundImage; override;
    function CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo; override;
  public
    function CanDrawDefaultFormIcon: Boolean; override;
  end;

  { TdxRibbonBarControlAccessibilityHelper }

  TdxRibbonBarControlAccessibilityHelper = class(TdxBarControlAccessibilityHelper)
  protected
    function LogicalNavigationGetNextChild(AChildIndex: Integer; AShift: TShiftState): TdxBarAccessibilityHelper; override;
  end;

  { TdxRibbonBarButtonLikeControlDrawParams }

  TdxRibbonBarButtonLikeControlDrawParams = class(TdxBarButtonLikeControlDrawParams)
  strict private
    FRibbon: TdxCustomRibbon;
  protected
    function GetScaleFactor: TdxScaleFactor; override;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce;
  end;

  { TdxRibbonBarPainter }

  TdxRibbonBarPainter = class(TdxBarSkinnedPainter)
  strict private
    FDrawParams: TdxRibbonBarButtonLikeControlDrawParams;
    FRibbon: TdxCustomRibbon;

    // Cached Values
    FCollapsedGroupElementSizeDenominator: Integer;
    FCollapsedGroupElementSizeNumerator: Integer;
    FCollapsedGroupWidthCache: TDictionary<string, Integer>;
    FGroupCaptionTextHeight: Integer;
    FGroupRowTextHeight: Integer;

    function GetCollapsedGroupGlyph(ABarControl: TdxBarControl): TdxSmartGlyph;
    function GetCollapsedGroupGlyphBackgroundSize(ABarControl: TdxBarControl): TSize;
    function GetCollapsedGroupGlyphSize(ABarControl: TdxBarControl): TSize;
    function GetGroupState(ABarControl: TdxBarControl): Integer;
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure CalculateDrawParams;
    function CalculateRowHeight(AIconSize, ATextHeight: Integer; AScaleFactor: TdxScaleFactor): Integer; virtual;
    procedure DrawCollapsedToolbarBackgroundPart(ABarControl: TdxRibbonGroupBarControl; ACanvas: TcxCanvas; AGroupState: Integer);
    procedure DrawCollapsedToolbarContentPart(ABarControl: TdxRibbonGroupBarControl; ACanvas: TcxCanvas; AGroupState: Integer);
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    procedure DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC); override;
    function GetButtonPartState(const ADrawParams: TdxBarButtonLikeControlDrawParams; AControlPart: Integer): Integer; override;
    function GetCollapsedGroupCaptionRect(const AGroupRect: TRect): TRect; virtual;
    function GetCollapsedGroupWidth(ABarControl: TdxRibbonGroupBarControl): Integer; virtual;
    function GetGroupCaptionHeight: Integer; overload;
    function GetGroupCaptionHeight(ATextHeight: Integer): Integer; overload; virtual;
    function GetGroupMinWidth(ABarControl: TdxRibbonGroupBarControl): Integer; virtual;
    function GetSmallIconSize: Integer; overload;
    function SubMenuControlUseSingleMenuWindowMode: Boolean; override;
    //
    property DrawParams: TdxRibbonBarButtonLikeControlDrawParams read FDrawParams;
  public
    constructor Create(AData: TdxNativeUInt); override;
    destructor Destroy; override;
    procedure BarDrawBackground(ABarControl: TdxBarControl; ADC: HDC;
      const ADestRect: TRect; const ASourceRect: TRect; ABrush: HBRUSH; AColor: TColor); override;
    function BarMarkItemRect(ABarControl: TdxBarControl): TRect; override;
    function BarMarkRect(ABarControl: TdxBarControl): TRect; override;
    function DropDownGalleryGetScrollBarWidth(AControlHeight: Integer; const AControlMargins: TRect): Integer; override;
    function DropDownGalleryGetSizingBandHeight(AIconSize, ATextSize: Integer; AScaleFactor: TdxScaleFactor): Integer; override;
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette; override;
    function GetGroupRowCount: Integer; virtual;
    function GetGroupRowHeight(AIconSize: Integer): Integer; virtual;
    function GetToolbarContentOffsets(ABar: TdxBar; ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect; override;
    function IsSimplifiedGroupsLayout: Boolean;
    function SubMenuControlBeginGroupSize: Integer; override;
    function SubMenuGetSeparatorSize: Integer; override;
    //
    property Ribbon: TdxCustomRibbon read FRibbon;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  { TdxCustomRibbonDockControl }

  TdxCustomRibbonDockControl = class(TdxBarDockControl)
  strict private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure AdjustSize; override;
    function AllowUndockWhenLoadFromIni: Boolean; override;
    procedure FillBackground(DC: HDC; const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    function GetSunkenBorder: Boolean; override;
    function IsDrawDesignBorder: Boolean; override;
    function IsTransparent: Boolean; override;
    procedure VisibleChanged; virtual;
  public
    procedure UpdateBarControlsFont;
    procedure UpdateColorScheme; virtual;
  end;

  { IdxRibbonGroupViewInfo }

  TdxRibbonGroupOffsetsInfo = record
    ButtonGroupOffset: Integer;
    ContentLeftOffset: Integer;
    ContentRightOffset: Integer;
  end;

  IdxRibbonGroupViewInfo = interface
  ['{A2CAD367-1836-4FA7-8730-8E7531463C8C}']
    procedure AddSeparator(const Value: TdxBarItemSeparatorInfo);
    procedure DeleteSeparators;
    function GetContentSize: TSize;
    function GetItemControlCount: Integer;
    function GetItemControlViewInfo(AIndex: Integer): IdxBarItemControlViewInfo;
    function GetMinContentWidth: Integer;
    function GetOffsetsInfo: TdxRibbonGroupOffsetsInfo;
    function GetSeparatorCount: Integer;
    function GetSeparatorInfo(AIndex: Integer): TdxBarItemSeparatorInfo;
    procedure SetContentSize(const Value: TSize);
    procedure SetSeparatorInfo(AIndex: Integer; const Value: TdxBarItemSeparatorInfo);
  end;

  { IdxRibbonGroupLayoutCalculator }

  IdxRibbonGroupLayoutCalculator = interface
  ['{894AC146-F69A-4ED2-9293-AA54AAAE1189}']
    procedure CalcInit(AGroupViewInfo: IdxRibbonGroupViewInfo);
    procedure CalcLayout(AGroupViewInfo: IdxRibbonGroupViewInfo);
    function CollapseMultiColumnItemControls(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
    function DecreaseMultiColumnItemControlsColumnCount(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
    function Reduce(AGroupViewInfo: IdxRibbonGroupViewInfo; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
    procedure ReduceInit(AGroupViewInfo: IdxRibbonGroupViewInfo);
  end;

  { TdxRibbonGroupsDockControlContentChangeAnimation }

  TdxRibbonGroupsDockControlContentChangeAnimation = class(TdxAnimationTransition)
  strict private
    FImage: TcxBitmap32;
  public
    constructor Create(ADockControl: TdxCustomRibbonDockControl;
      ATime: Cardinal; ATransitionEffect: TdxAnimationTransitionEffect = ateLinear); reintroduce;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; const R: TRect);
  end;

  { TdxRibbonGroupsDockControl }

  TdxRibbonGroupsDockControl = class(TdxCustomRibbonDockControl)
  strict private
    FContentChangeAnimation: TdxRibbonGroupsDockControlContentChangeAnimation;
    FTab: TdxRibbonTab;

    function CanAnimateContentChanging: Boolean;
    function GetRibbon: TdxCustomRibbon;
    function GetSiteViewInfo: TdxRibbonGroupsDockControlSiteViewInfo;
    procedure HandlerAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure HandlerAnimationTerminated(Sender: TObject);
    //
    procedure DesignMenuClick(Sender: TObject);
    procedure InitDesignMenu(AItemLinks: TdxBarItemLinks);
    procedure ShowDesignMenu;
    //
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    FViewInfo: TdxRibbonGroupsDockControlViewInfo;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CalcRowToolbarPositions(ARowIndex: Integer; AClientSize: Integer); override;
    procedure DblClick; override;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure FillBackground(DC: HDC; const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    procedure FullInvalidate;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
    function GetPainter: TdxBarPainter; override;
    function GetViewInfoClass: TdxRibbonGroupsDockControlViewInfoClass; virtual;
    function IsDoubleBufferedNeeded: Boolean;
    function IsMultiRow: Boolean; override;
    procedure MakeRectFullyVisible(const R: TRect); virtual;
    procedure Paint; override;
    procedure RecalculateBars;
    procedure SetSize; override;
    procedure ShowCustomizePopup; override;
    procedure StopContentChangeAnimation;
    procedure UpdateGroupPositions(AForceUpdate: Boolean = False);
    procedure VisibleChanged; override;

    property Ribbon: TdxCustomRibbon read GetRibbon;
    property SiteViewInfo: TdxRibbonGroupsDockControlSiteViewInfo read GetSiteViewInfo;
    property ViewInfo: TdxRibbonGroupsDockControlViewInfo read FViewInfo;
  public
    constructor Create(ATab: TdxRibbonTab); reintroduce; virtual;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Tab: TdxRibbonTab read FTab;
  end;

  TdxRibbonGroupsReduceStage = (rgrsMultiColumnItemControlsColumnCount,
    rgrsMultiColumnItemControlsCollapsing, rgrsItemControlsViewLevel,
    rgrsGroupsCollapsing);

  { TdxRibbonGroupsDockControlViewInfo }

  TdxRibbonGroupsDockControlViewInfo = class
  strict private
    FIsRightToLeftConverted: Boolean;
  private
    FScrollButtons: TdxRibbonScrollButtons;
    FScrollPosition: Integer;
    procedure CheckGroupsCollapsedStates;
    function GetFirstGroupPosition: Integer;
    function GetGroupCount: Integer;
    function GetGroupViewInfo(AIndex: Integer): TdxRibbonGroupBarControlViewInfo;
    function IsValidToolbar(AToolbar: TdxBar): Boolean;
    function TotalGroupsWidth: Integer;
    function TryPlaceGroups(AMaxContentWidth: Integer): Boolean;
  protected
    FDockControl: TdxRibbonGroupsDockControl;
    procedure CalculateGroupsScrollInfo(AMaxContentWidth: Integer); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure InternalScrollGroups(ADelta: Integer; AMaxContentWidth: Integer); virtual;
  public
    constructor Create(ADockControl: TdxRibbonGroupsDockControl); virtual;
    procedure Calculate(const ABoundsRect: TRect); virtual;
    procedure ResetScrollInfo;
    procedure RightToLeftConversion(const ABounds: TRect);
    procedure ScrollGroups(AScrollLeft: Boolean; AMaxContentWidth: Integer); virtual;

    property DockControl: TdxRibbonGroupsDockControl read FDockControl;
    property FirstGroupPosition: Integer read GetFirstGroupPosition;
    property GroupCount: Integer read GetGroupCount;
    property GroupViewInfos[AIndex: Integer]: TdxRibbonGroupBarControlViewInfo read GetGroupViewInfo;
    property ScrollButtons: TdxRibbonScrollButtons read FScrollButtons;
  end;

  { TdxRibbonTabGroupsPopupWindow }

  TdxRibbonTabGroupsPopupWindow = class(TcxCustomPopupWindow)
  strict private
    FAllowShowHideAnimation: Boolean;
    FRibbon: TdxCustomRibbon;
    FShadow: TdxBarShadow;
    FSwitchingBetweenTabs: Boolean;

    function CanShowShadow: Boolean;
    function GetBounds: TRect;
    function GetGroupsDockControlSite: TdxRibbonGroupsDockControlSite;
    // Messages
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    function CalculatePosition(const ASize: TSize): TPoint; override;
    function CalculateSize: TSize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Deactivate; override;
    procedure DoClosed; override;
    procedure DoShowed; override;
    procedure DoShowing; override;
    procedure HandleNavigationKey(AKey: Word);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function NeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle; AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean; override;
    procedure SetGroupsDockControlSite;

    property SwitchingBetweenTabs: Boolean read FSwitchingBetweenTabs;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce; virtual;
    destructor Destroy; override;
    //
    property AllowShowHideAnimation: Boolean read FAllowShowHideAnimation write FAllowShowHideAnimation;
    property GroupsDockControlSite: TdxRibbonGroupsDockControlSite read GetGroupsDockControlSite;
    property Ribbon: TdxCustomRibbon read FRibbon;
  end;

  { TdxRibbonCustomBarControl }

  TdxRibbonCustomBarControl = class(TdxBarPopupControl)
  strict private
    function GetQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function AllowSelectionFrame: Boolean; override;
    function AllowFade: Boolean; override;
    procedure InitializeForDock(ABar: TdxBar); override;

    function AllowQuickCustomizing: Boolean; override;
    function CanAlignControl(AControl: TdxBarItemControl): Boolean; override;
    function CanMoving: Boolean; override;
    procedure DrawBarParentBackground(ACanvas: TcxCanvas); virtual;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function GetEditFont: TFont; override;
    function GetFont: TFont; override;
    function GetFullItemRect(Item: TdxBarItemControl): TRect; override;
    function GetIsMainMenu: Boolean; override;
    function GetMultiLine: Boolean; override;
    function GetRibbon: TdxCustomRibbon; virtual; abstract;
    function GetScaleFactor: TdxScaleFactor; override;
    function MarkExists: Boolean; override;
    function NotHandleMouseMove(ACheckLastMousePos: Boolean = True): Boolean; override;
    function RealMDIButtonsOnBar: Boolean; override;
    //
    function ClickAtHeader: Boolean; virtual;
    procedure DoPopupMenuClick(Sender: TObject); virtual;
    function GetPopupMenuItems: TdxRibbonPopupMenuItems; virtual;
    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;
    procedure PopupMenuClick(Sender: TObject);
    procedure ShowPopup(AItem: TdxBarItemControl); override;
    //
    property QuickAccessToolbar: TdxRibbonQuickAccessToolbar read GetQuickAccessToolbar;
  public
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonCustomToolbarBarControl }

  TdxRibbonCustomToolbarBarControl = class(TdxRibbonCustomBarControl)
  private
    FBitmap: TcxBitmap32;
    FIsWindowCreation: Boolean;

    function GetSeparatorWidth(AItemControl: TdxBarItemControl): Integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    function AllItemsVisible: Boolean;
    procedure CalcControlsPositions; override;
    function CalcSize(AMaxWidth: Integer): TSize; virtual;
    function CanHideAllItemsInSingleLine: Boolean; override;
    procedure CreateWnd; override;
    procedure DoPaintItem(AControl: TdxBarItemControl; ACanvas: TcxCanvas; const AItemRect: TRect); override;
    procedure InitializeForDock(ABar: TdxBar); override;
    function GetClientOffset: TRect; virtual;
    function GetItemControlDefaultViewLevel(AItemControl: TdxBarItemControl): TdxBarItemViewLevel; override;
    function GetMarkSize: Integer; override;
    function GetMinHeight(AStyle: TdxBarDockingStyle): Integer; override;
    function GetMinWidth(AStyle: TdxBarDockingStyle): Integer; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetSize(AMaxWidth: Integer): TSize;
    function GetSizeForWidth(AStyle: TdxBarDockingStyle; AWidth: Integer): TSize; override;
    procedure UpdateDoubleBuffered; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxRibbonCustomToolbarPainter }

  TdxRibbonCustomToolbarPainter = class(TdxRibbonBarPainter)
  protected
    procedure BarDrawMarkBackground(ABarControl: TdxBarControl; DC: HDC; ItemRect: TRect; AToolbarBrush: HBRUSH); override;
    procedure DrawBarMarkState(ABarControl: TdxBarControl; DC: HDC; const R: TRect; AState: TdxBarMarkState); override;
    procedure DrawButtonBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams); override;
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    // Skin Consts
    function SkinGetButtonDropDownPartID: Integer; virtual; abstract;
    function SkinGetButtonID: Integer; virtual; abstract;
    function SkinGetButtonMainPartID: Integer; virtual; abstract;
  public
    function BarMarkRect(ABarControl: TdxBarControl): TRect; override;
    function BarMarkItemRect(ABarControl: TdxBarControl): TRect; override;
    procedure ComboControlDrawArrowButton(const ADrawParams: TdxBarEditLikeControlDrawParams; ARect: TRect; AInClientArea: Boolean); override;
    function GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer; override;
    function GetButtonBorderWidth(AScaleFactor: TdxScaleFactor): Integer; override;
    function GetToolbarContentOffsets(ABar: TdxBar; ADockingStyle: TdxBarDockingStyle;
      AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect; override;
    function MarkButtonOffset: Integer; virtual;
    function MarkButtonWidth: Integer; virtual;
    function MarkSizeX(ABarControl: TdxBarControl): Integer; override;
  end;

  { TdxRibbonQuickAccessToolbarBarControl }

  TdxRibbonQuickAccessToolbarBarControl = class(TdxRibbonCustomToolbarBarControl)
  strict private
    function GetViewInfo: TdxRibbonQuickAccessToolbarBarControlViewInfo;
  protected
    FDefaultGlyph: TdxSmartGlyph;

    function CalcSize(AMaxWidth: Integer): TSize; override;
    procedure CreateWnd; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetDefaultItemGlyph: TdxSmartGlyph; override;
    function GetMarkAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetPopupMenuItems: TdxRibbonPopupMenuItems; override;
    function GetQuickControlClass: TdxBarPopupControlClass; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;

    function AllowQuickCustomizing: Boolean; override;
    procedure InitQuickCustomizeItemLinks(AQuickControl: TdxBarPopupControl); override;
    procedure InitAddRemoveSubItemPopup(AItemLinks: TdxBarItemLinks); override;

    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;
    function MarkExists: Boolean; override;
    procedure RemoveItemFromQAT;
    procedure ShowPopup(AItem: TdxBarItemControl); override;
    procedure UpdateDefaultGlyph(AGlyph: TdxSmartGlyph); virtual;

    property ViewInfo: TdxRibbonQuickAccessToolbarBarControlViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsOnGlass: Boolean; override;
  end;

  { TdxRibbonQuickAccessToolbarBarControlViewInfo }

  TdxRibbonQuickAccessToolbarBarControlViewInfo = class(TdxBarControlViewInfo)
  protected
    function IsLastVisibleItemControl(AItemControl: TdxBarItemControl): Boolean; override;
  end;

  { TdxRibbonQuickAccessToolbarPainter }

  TdxRibbonQuickAccessToolbarPainter = class(TdxRibbonCustomToolbarPainter)
  strict private
    function GetColorScheme: TdxCustomRibbonSkin;
    function GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
  protected
    procedure DrawGroupButtonControl(ADrawParams: TdxBarButtonLikeControlDrawParams; const ARect: TRect); virtual;
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;

    function SkinGetArrowID: Integer; override;
    function SkinGetButtonDropDownPartID: Integer; override;
    function SkinGetButtonID: Integer; override;
    function SkinGetButtonMainPartID: Integer; override;
    function SkinGetMarkArrowID: Integer; override;
    function SkinGetTruncateMarkID: Integer; override;
  public
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette; override;
    function MarkButtonOffset: Integer; override;
    //
    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property QuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo read GetQuickAccessToolbarViewInfo;
  end;

  { TdxRibbonCustomToolbarDockControl }

  TdxRibbonCustomToolbarDockControl = class(TdxCustomRibbonDockControl)
  strict private
    FPainter: TdxRibbonCustomToolbarPainter;
    FRibbon: TdxCustomRibbon;
  protected
    procedure CalcLayout; override;
    function CreatePainter: TdxRibbonCustomToolbarPainter; virtual; abstract;
    function GetPainter: TdxBarPainter; override;
    procedure RecalculateRibbon;
    procedure ShowCustomizePopup; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TdxCustomRibbon); reintroduce; virtual;
    destructor Destroy; override;
    //
    property Ribbon: TdxCustomRibbon read FRibbon;
  end;

  { TdxRibbonQuickAccessToolbarDockControl }

  TdxRibbonQuickAccessToolbarDockControl = class(TdxRibbonCustomToolbarDockControl)
  protected
    function CreatePainter: TdxRibbonCustomToolbarPainter; override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
  end;

  { TdxRibbonQuickAccessToolbarBarControlDesignHelper }

  TdxRibbonQuickAccessToolbarBarControlDesignHelper = class(TCustomdxBarControlDesignHelper)
  public
    class procedure GetEditors(AEditors: TList); override;
  end;

  { TdxRibbonQuickAccessPopupBarControl }

  TdxRibbonQuickAccessPopupBarControl = class(TdxRibbonQuickAccessToolbarBarControl)
  strict private
    FPainter: TdxBarPainter;

    function GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
    function GetMarkLink: TdxBarItemLink;
    function GetMarkSubItem: TCustomdxBarSubItem;
  protected
    function CreatePainter: TdxBarPainter; virtual;
    procedure InitializeForDock(ABar: TdxBar); override;

    function GetClientOffset: TRect; override;
    function GetPainter: TdxBarPainter; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetPopupSize: TSize; override;
    function HasShadow: Boolean; override;
    function IsPopup: Boolean; override;

    property BarControl: TdxRibbonQuickAccessToolbarBarControl read GetBarControl;
  public
    destructor Destroy; override;
    procedure CloseUp; override;
    procedure Popup(const AOwnerRect: TRect); override;
  end;

  { TdxRibbonQuickAccessPopupItemControlPainter }

  TdxRibbonQuickAccessPopupPainter = class(TdxRibbonQuickAccessToolbarPainter)
  protected
    procedure DrawQuickAccessPopupSubItem(DC: HDC; const ARect: TRect; AState: Integer); virtual;
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    function SkinGetArrowID: Integer; override;
    function SkinGetButtonDropDownPartID: Integer; override;
    function SkinGetButtonID: Integer; override;
    function SkinGetButtonMainPartID: Integer; override;
    function SkinGetMarkArrowID: Integer; override;
    function SkinGetTruncateMarkID: Integer; override;
  public
    function MarkButtonOffset: Integer; override;
    function MarkSizeX(ABarControl: TdxBarControl): Integer; override;
  end;

  { TdxRibbonQuickAccessPopupSubItem }

  TdxRibbonQuickAccessPopupSubItem = class(TdxBarSubItem)
  protected
    function CreateBarControl: TCustomdxBarControl; override;
  end;

  { TdxRibbonQuickAccessPopupSubMenuControl }

  TdxRibbonQuickAccessPopupSubMenuControl = class(TdxBarSubMenuControl)
  protected
    procedure ShowPopup(AItem: TdxBarItemControl); override;
  end;

  { TdxRibbonQuickAccessPopupSubItemControl }

  TdxRibbonQuickAccessPopupSubItemControl = class(TdxBarSubItemControl)
  protected
    procedure DoCloseUp(AHadSubMenuControl: Boolean); override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    function GetDefaultWidth: Integer; override;
  end;

  { TdxRibbonQuickAccessPopupSubItemButton }

  TdxRibbonQuickAccessPopupSubItemButton = class(TdxBarButton)
  public
    procedure DoClick; override;
  end;

  { TdxRibbonQuickAccessPopupSubItemButtonControl }

  TdxRibbonQuickAccessPopupSubItemButtonControl = class(TdxBarButtonControl);

  { TdxRibbonTabAreaToolbarBarControl }

  TdxRibbonTabAreaToolbarBarControl = class(TdxRibbonCustomToolbarBarControl)
  protected
    function AllowQuickCustomizing: Boolean; override;
    function CalcSize(AMaxWidth: Integer): TSize; override;
    function MarkExists: Boolean; override;
    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetItemControlDefaultViewLevel(AItemControl: TdxBarItemControl): TdxBarItemViewLevel; override;
    function GetQuickControlClass: TdxBarPopupControlClass; override;
  public
    function IsOnGlass: Boolean; override;
  end;

  { TdxRibbonTabAreaToolbarBarControlAccessibilityHelper }

  TdxRibbonTabAreaToolbarBarControlAccessibilityHelper = class(TdxRibbonBarControlAccessibilityHelper)
  protected
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
  end;

  { TdxRibbonTabAreaToolbarDockControl }

  TdxRibbonTabAreaToolbarDockControl = class(TdxRibbonCustomToolbarDockControl)
  protected
    function CreatePainter: TdxRibbonCustomToolbarPainter; override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
  end;

  { TdxRibbonTabAreaToolbarPainter }

  TdxRibbonTabAreaToolbarPainter = class(TdxRibbonCustomToolbarPainter)
  protected
    procedure DoDrawText(ADC: HDC; const AText: string; var ARect: TRect; AFormat: Cardinal); override;
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    function GetDefaultEnabledTextColor(ABarItemControl: TdxBarItemControl; ASelected: Boolean; AFlat: Boolean): TColor; override;

    function SkinGetArrowID: Integer; override;
    function SkinGetButtonDropDownPartID: Integer; override;
    function SkinGetButtonID: Integer; override;
    function SkinGetButtonMainPartID: Integer; override;
    function SkinGetMarkArrowID: Integer; override;
    function SkinGetTruncateMarkID: Integer; override;
  public
    procedure BarDrawBackground(ABarControl: TdxBarControl; ADC: HDC;
      const ADestRect, ASourceRect: TRect; ABrush: HBRUSH; AColor: TColor); override;
    procedure EditControlDrawBorder(const ADrawParams: TdxBarEditLikeControlDrawParams; var ARect: TRect); override;
    function EditControlGetBackgroundColor(const ADrawParams: TdxBarItemControlDrawParams): TColor; override;
    function EditControlGetTextColor(const ADrawParams: TdxBarItemControlDrawParams): TColor; override;
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette; override;
  end;

  { TdxRibbonTabAreaToolbarPopupBarControl }

  TdxRibbonTabAreaToolbarPopupBarControl = class(TdxRibbonTabAreaToolbarBarControl)
  strict private
    FPainter: TdxBarPainter;

    function GetBarControl: TdxRibbonTabAreaToolbarBarControl;
  protected
    procedure InitializeForDock(ABar: TdxBar); override;

    function GetClientOffset: TRect; override;
    function GetPainter: TdxBarPainter; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetPopupSize: TSize; override;
    function HasShadow: Boolean; override;
    function IsPopup: Boolean; override;
  public
    property BarControl: TdxRibbonTabAreaToolbarBarControl read GetBarControl;
  end;

  { TdxRibbonGroupBarControl }

  TdxRibbonGroupBarControl = class(TdxRibbonCustomBarControl, IdxFadingObject, IdxFadingObjectFadingOptions)
  private
    FGroup: TdxRibbonTabGroup;
    FRibbon: TdxCustomRibbon;

    procedure DesignMenuClick(Sender: TObject);
    procedure DrawCaptionButton(ACanvas: TcxCanvas; AButton: TdxBarCaptionButton);
    procedure DrawCaptionButtons(ACanvas: TcxCanvas);
    procedure DrawSelectedFrame(DC: HDC; const R: TRect);
    function GetCollapsed: Boolean;
    function GetGroupCaptionRect: TRect;
    function GetGroupDesignRect: TRect;
    function GetIsComponentSelected: Boolean;
    function GetIsDesignObjectsOnClientArea: Boolean;
    function GetViewInfo: TdxRibbonGroupBarControlViewInfo;
    procedure InitDesignMenu(AItemLinks: TdxBarItemLinks);
    procedure PaintDesignObjects(ACanvas: TcxCanvas);
    procedure PaintGroupBackground(ACanvas: TcxCanvas);
    procedure PaintGroupCaptionText(ACanvas: TcxCanvas);
    procedure ShowGroupDesignMenu;
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure InitializeForDock(ABar: TdxBar); override;

    //IdxFadingObject
    function IdxFadingObject.CanFade = FadingCanFade;
    procedure IdxFadingObject.DrawFadeImage = FadingDrawFadeImage;
    procedure IdxFadingObject.GetFadingImages = FadingGetFadingImages;

    //IdxFadingObjectFadingOptions
    function GetFadingOptions: TdxFadingOptions;

    function FadingCanFade: Boolean;
    procedure FadingDrawFadeImage;
    procedure FadingGetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

    //methods
    procedure AdjustHintWindowPosition(var APos: TPoint; const ABoundsRect: TRect; AHeight: Integer); override;
    procedure CalcLayout; override;
    function CanProcessShortCut: Boolean; override;
    procedure CaptionChanged; override;
    procedure DoHideAll(AReason: TdxBarCloseUpReason); override;
    procedure DoNCPaint(DC: HDC); override;
    procedure DoOpaqueNCPaint(DC: HDC);
    procedure DoPaint; override;
    procedure DoTransparentNCPaint(DC: HDC);
    procedure DrawBarParentBackground(ACanvas: TcxCanvas); override;
    procedure DrawContentBackground; override;
    procedure DrawGroupsArea(ACanvas: TcxCanvas; const ABounds: TRect);
    function IsAllowContextPaint: Boolean; virtual;

    procedure DoBarMouseDown(Button: TMouseButton; Shift: TShiftState;
      const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean); override;

    function ClickAtHeader: Boolean; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetCaption: TCaption; override;
    function GetMarkDrawState: TdxBarMarkState; override;
    function GetMoreButtonsHint: string; override;
    function GetQuickControlClass: TdxBarPopupControlClass; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    procedure GlyphChanged; override;
    function HasCaptionButtons: Boolean; override;
    procedure InitQuickControl(AQuickControl: TdxBarPopupControl); override;
    procedure MakeItemControlFullyVisible(AItemControl: TdxBarItemControl); override;
    function MarkExists: Boolean; override;
    procedure ViewStateChanged(APrevValue: TdxBarViewState); override;
    procedure UpdateCaptionButton(ACaptionButton: TdxBarCaptionButton); override;
    procedure WindowPosChanged(var Message: TWMWindowPosMsg); override;

    property GroupCaptionRect: TRect read GetGroupCaptionRect;
    property GroupDesignRect: TRect read GetGroupDesignRect;
    property IsComponentSelected: Boolean read GetIsComponentSelected;
    property IsDesignObjectsOnClientArea: Boolean read GetIsDesignObjectsOnClientArea;
    property ViewInfo: TdxRibbonGroupBarControlViewInfo read GetViewInfo;
  public
    destructor Destroy; override;
    procedure CloseUp; override;
    //
    property Collapsed: Boolean read GetCollapsed;
    property Group: TdxRibbonTabGroup read FGroup;
  end;

  TdxRibbonGroupKeyTipsBaseLinePositions = record
    BottomKeyTipsBaseLinePosition: Integer;
    Calculated: Boolean;
    RowKeyTipsBaseLinePositions: array of Integer;
  end;

  { TdxRibbonGroupBarControlViewInfo }

  TdxRibbonGroupBarControlViewInfo = class(TCustomdxBarControlViewInfo)
  private
    FCollapsed: Boolean;
    FContentSize: TSize;
    FGroupRowHeight: Integer;
    FKeyTipsBaseLinePositions: TdxRibbonGroupKeyTipsBaseLinePositions;
    FLayoutCalculator: IdxRibbonGroupLayoutCalculator;
    FNonContentAreaSize: TSize;
    FPrevCollapsedState: Boolean;
    function CreateCalculateHelper: IdxRibbonGroupViewInfo;
    function GetBarControl: TdxRibbonGroupBarControl;
    function GetBottomKeyTipsBaseLinePosition: Integer;
    function GetRibbon: TdxCustomRibbon;
    function GetRowKeyTipsBaseLinePosition(ARowIndex: Integer): Integer;
    function GetSize: TSize;
  protected
    procedure CalculateKeyTipsBaseLinePositions;
    function CreateLayoutCalculator: IdxRibbonGroupLayoutCalculator; virtual;
    procedure DoCalculateKeyTipsBaseLinePositions; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetNonContentAreaSize: TSize; virtual;
    procedure RecreateItemControlViewInfos;
    procedure UpdateItemRects;
    //
    property ContentSize: TSize read FContentSize write FContentSize;
    property LayoutCalculator: IdxRibbonGroupLayoutCalculator read FLayoutCalculator;
  public
    procedure AfterCalculate; virtual;
    procedure BeforeCalculate(ACollapsed: Boolean); virtual;
    procedure Calculate; override;
    procedure CheckGroupCollapsedStates;
    function Reduce(AStage: TdxRibbonGroupsReduceStage; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
    procedure ReduceInit;
    //
    property BarControl: TdxRibbonGroupBarControl read GetBarControl;
    property Collapsed: Boolean read FCollapsed write FCollapsed;
    property Size: TSize read GetSize;
    property BottomKeyTipsBaseLinePosition: Integer read GetBottomKeyTipsBaseLinePosition;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property RowKeyTipsBaseLinePositions[ARowIndex: Integer]: Integer read GetRowKeyTipsBaseLinePosition;
  end;

  { TdxRibbonGroupBarControlDesignHelper }

  TdxRibbonGroupBarControlDesignHelper = class(TCustomdxBarControlDesignHelper)
  public
    class function CanDynamicallyChangeViewLevels: Boolean; override;
    class function GetForbiddenActions: TdxBarCustomizationActions; override;
  end;

  { TdxRibbonCollapsedGroupPopupBarControl }

  TdxRibbonCollapsedGroupPopupBarControl = class(TdxRibbonGroupBarControl)
  private
    FAllowNCPaint: Boolean;
  protected
    function AllowNCPaint: Boolean; override;
    procedure DoBarMouseRightButtonDown(Shift: TShiftState; const APoint: TPoint;
      AItemControl, APrevSelectedControl: TdxBarItemControl; APointInClientRect: Boolean); override;
    function GetCaption: TCaption; override;
    function GetPainter: TdxBarPainter; override;
    function GetPopupSize: TSize; override;
    function GetSizeForWidth(AStyle: TdxBarDockingStyle; AWidth: Integer): TSize; override;
    function IgnoreClickAreaWhenHidePopup: TRect; override;
    procedure InitializeForPopup(AParentBarControl: TdxBarControl; ABar: TdxBar); override;
    function IsPopup: Boolean; override;
    function NeedHideOnNCMouseClick: Boolean; override;
  public
    destructor Destroy; override;
    procedure Hide; override;
    procedure Popup(const AOwnerRect: TRect); override;
  end;

  { TdxRibbonTabGroup }

  TdxRibbonTabGroupClass = class of TdxRibbonTabGroup;

  TdxRibbonTabGroupAssignedValue = (rtgavCaption);
  TdxRibbonTabGroupAssignedValues = set of TdxRibbonTabGroupAssignedValue;
  TdxRibbonTabGroupRestriction = (rtgrNone, rtgrNoExpand, rtgrNoCollapse);

  TdxRibbonTabGroup = class(TcxInterfacedCollectionItem,
    IdxBarSelectableItem,
    IdxRibbonToolbarContainer,
    IdxAdornerTargetElement,
    IdxAdornerTargetElementCollection)
  strict private
    FAssignedValues: TdxRibbonTabGroupAssignedValues;
    FCaption: string;
    FDesignSelectionHelper: IdxBarSelectableItem;
    FLoadedToolbarName: string;
    FMergeKind: TdxRibbonMergeKind;
    FMergeOrder: Integer;
    FRestriction: TdxRibbonTabGroupRestriction;
    FToolbar: TdxBar;

    procedure CheckUndockToolbar;
    function GetAnotherTabWithOurToolbar(out ATab: TdxRibbonTab): Boolean;
    function GetCanCollapse: Boolean;
    function GetCaption: string;
    function GetIsCaptionAssigned: Boolean;
    function GetIsToolBarShared: Boolean;
    function GetRibbon: TdxCustomRibbon;
    function GetTab: TdxRibbonTab;
    function GetToolbar: TdxBar;
    function GetVisible: Boolean;
    procedure ReadCaption(AReader: TReader);
    procedure ReadToolbarName(AReader: TReader);
    procedure SetAssignedValues(const AValue: TdxRibbonTabGroupAssignedValues);
    procedure SetCanCollapse(Value: Boolean);
    procedure SetCaption(const AValue: string);
    procedure SetMergeOrder(const Value: Integer);
    procedure SetRestriction(AValue: TdxRibbonTabGroupRestriction);
    procedure SetToolbar(Value: TdxBar);
    procedure ValidateToolbar(Value: TdxBar);
    procedure WriteCaption(AWriter: TWriter);
    procedure WriteToolbarName(AWriter: TWriter);
  protected
    function CreatedByMerging: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DockToolbar(AToolbar: TdxBar); virtual;
    function IsActuallyVisible: Boolean;
    function IsToolbarAcceptable(AToolbar: TdxBar): Boolean;
    procedure MoveToTab(ATab: TdxRibbonTab);
    procedure SetCollection(Value: TCollection); override;
    procedure UpdateBarManager(ABarManager: TdxBarManager);
    procedure UpdateToolbarValue;
    function VisibleBeforeMerging: Boolean;

    function GetIniSection(const ABaseSection, ADelimiter: string): string;
    procedure LoadFromIni(ASource: TCustomIniFile; const ABaseSection, ADelimiter: string);
    procedure SaveToIni(ADestination: TCustomIniFile; const ABaseSection, ADelimiter: string);

    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    // IdxAdornerTargetElementCollection
    procedure IdxAdornerTargetElementCollection.GetElements = GetAdornerTargetElements;
    procedure GetAdornerTargetElements(AList: TStrings); virtual;

    property DesignSelectionHelper: IdxBarSelectableItem read FDesignSelectionHelper implements IdxBarSelectableItem;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function IsMergeAllowed: Boolean; virtual;
    function IsMerged: Boolean; virtual;
    procedure Merge(AGroup: TdxRibbonTabGroup;
      AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(AGroup: TdxRibbonTabGroup = nil); virtual;

    property IsToolBarShared: Boolean read GetIsToolBarShared;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property Tab: TdxRibbonTab read GetTab;
    property Visible: Boolean read GetVisible;
  published
    property AssignedValues: TdxRibbonTabGroupAssignedValues read FAssignedValues write SetAssignedValues stored False;
    property Caption: string read GetCaption write SetCaption stored GetIsCaptionAssigned;
    property MergeKind: TdxRibbonMergeKind read FMergeKind write FMergeKind default rmkMerge;
    property MergeOrder: Integer read FMergeOrder write SetMergeOrder default 0;
    property Restriction: TdxRibbonTabGroupRestriction read FRestriction write SetRestriction default rtgrNone;
    property ToolBar: TdxBar read GetToolbar write SetToolbar stored False;

    property CanCollapse: Boolean read GetCanCollapse write SetCanCollapse default True; //Obsolete
  end;

  { TdxRibbonTabGroups }

  TdxRibbonTabGroups = class(TCollection)
  private
    FTab: TdxRibbonTab;
    function GetItem(Index: Integer): TdxRibbonTabGroup;
    procedure SetItem(Index: Integer; const Value: TdxRibbonTabGroup);
  protected
    function GetIniSection(const AGroupIndex: Integer; ASource: TCustomIniFile): string;
    function GetOwner: TPersistent; override;
    procedure FreeNotification(AComponent: TComponent);
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Unmerge(AGroup: TdxRibbonTabGroup = nil);
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateGroupToolbarValues;
  public
    constructor Create(ATab: TdxRibbonTab);
    function Add: TdxRibbonTabGroup;
    function ContainsToolBar(AToolBar: TdxBar): Boolean;
    function Find(const ACaption: string; out AGroup: TdxRibbonTabGroup): Boolean;
    function FindByToolBar(AToolBar: TdxBar): TdxRibbonTabGroup;
    function IndexOf(AGroup: TdxRibbonTabGroup): Integer;
    function Insert(AIndex: Integer): TdxRibbonTabGroup;
    //
    property Items[Index: Integer]: TdxRibbonTabGroup read GetItem write SetItem; default;
    property Tab: TdxRibbonTab read FTab;
  end;

  { TdxRibbonCustomToolbar }

  TdxRibbonCustomToolbar = class(TInterfacedPersistent, IdxRibbonToolbarContainer)
  private
    FDockControl: TdxCustomRibbonDockControl;
    FRibbon: TdxCustomRibbon;
    FToolbar: TdxBar;
    FVisible: Boolean;

    procedure CheckUndockGroupToolbar(AValue: TdxBar);
    function GetActualVisible: Boolean;
    function GetIAccessibilityHelper: IdxBarAccessibilityHelper;
    procedure SetVisible(AValue: Boolean);
  protected
    function Contains(AItemLink: TdxBarItemLink): Boolean;
    function CreateDockControl: TdxCustomRibbonDockControl; virtual; abstract;
    procedure DoAssign(Source: TdxRibbonCustomToolbar); virtual;
    procedure DoToolbarRemoving; virtual;
    procedure UpdateColorScheme; virtual;
    procedure UpdateRibbon;

    // IdxRibbonToolbarContainer
    function GetRibbon: TdxCustomRibbon;
    function GetToolbar: TdxBar;
    procedure SetToolbar(AValue: TdxBar);

    property ActualVisible: Boolean read GetActualVisible;
    property DockControl: TdxCustomRibbonDockControl read FDockControl;
    property IAccessibilityHelper: IdxBarAccessibilityHelper read GetIAccessibilityHelper;
  public
    constructor Create(ARibbon: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Ribbon: TdxCustomRibbon read GetRibbon;
  published
    property Toolbar: TdxBar read GetToolbar write SetToolbar;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxRibbonTabAreaToolbar }

  TdxRibbonTabAreaToolbar = class(TdxRibbonCustomToolbar)
  protected
    function CreateDockControl: TdxCustomRibbonDockControl; override;
  public
    function IsMerged: Boolean; virtual;
    procedure Merge(ATabAreaToolbar: TdxRibbonTabAreaToolbar;
      AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(ATabAreaToolbar: TdxRibbonTabAreaToolbar = nil); virtual;
  end;

  { TdxRibbonTabAreaSearchToolbar }

  TdxRibbonTabAreaSearchToolbar = class(TdxRibbonCustomToolbar)
  protected
    function CreateDockControl: TdxCustomRibbonDockControl; override;
  public
    function IsMerged: Boolean; virtual;
    procedure Merge(ATabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar;
      AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(ATabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar = nil); virtual;
  end;

  { TdxRibbonTabAreaSearchToolbarDockControl }

  TdxRibbonTabAreaSearchToolbarDockControl = class(TdxRibbonTabAreaToolbarDockControl);

  { TdxRibbonQuickAccessToolbar }

  TdxQuickAccessToolbarPosition = (qtpAboveRibbon, qtpBelowRibbon);

  TdxRibbonQuickAccessToolbar = class(TdxRibbonCustomToolbar)
  strict private
    FPosition: TdxQuickAccessToolbarPosition;

    procedure SetPosition(AValue: TdxQuickAccessToolbarPosition);
  protected
    procedure BarManagerLoadIni(const AEventData: TdxBarIniFileEventData);
    procedure BarManagerSaveIni(const AEventData: TdxBarIniFileEventData);
    procedure DoAssign(Source: TdxRibbonCustomToolbar); override;
    procedure DoToolbarRemoving; override;

    function CreateDockControl: TdxCustomRibbonDockControl; override;
    function GetMenuItemsForMark: TdxRibbonPopupMenuItems; virtual;
    procedure UpdateGroupButton(AForToolbar: TdxBar; ABeforeUndock: Boolean);
    procedure UpdateMenuItems(AItems: TdxBarItemLinks);
  public
    constructor Create(ARibbon: TdxCustomRibbon); override;
    function AddGroupButton(AToolbar: TdxBar): TdxRibbonQuickAccessGroupButton;
    function HasGroupButtonForToolbar(AToolbar: TdxBar): Boolean;

    function IsMerged: Boolean; virtual;
    procedure Merge(AQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
      AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(AQuickAccessToolbar: TdxRibbonQuickAccessToolbar = nil); virtual;
  published
    property Position: TdxQuickAccessToolbarPosition read FPosition write SetPosition default qtpAboveRibbon;
  end;

  { TdxRibbonCustomButtonPersistent }

  TdxRibbonCustomButtonPersistent = class(TPersistent)
  private
    FGlyph: TdxSmartGlyph;
    FRibbon: TdxCustomRibbon;
    FScreenTip: TdxScreenTip;
    FStretchGlyph: Boolean;

    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetScreenTip(const Value: TdxScreenTip);
    procedure SetStretchGlyph(const Value: Boolean);
  protected
    procedure Update;
  public
    constructor Create(ARibbon: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Ribbon: TdxCustomRibbon read FRibbon;
  published
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property ScreenTip: TdxScreenTip read FScreenTip write SetScreenTip;
    property StretchGlyph: Boolean read FStretchGlyph write SetStretchGlyph default True;
  end;

  { TdxRibbonHelpButton }

  TdxRibbonHelpButton = class(TdxRibbonCustomButtonPersistent);

  { TdxRibbonApplicationButton }

  TdxRibbonApplicationButton = class(TdxRibbonCustomButtonPersistent)
  private
    FIAccessibilityHelper: IdxBarAccessibilityHelper;
    FIMenu: IdxRibbonApplicationMenu;
    FKeyTip: string;
    FMenu: TComponent;
    FText: string;
    FVisible: Boolean;
    function GetIAccessibilityHelper: IdxBarAccessibilityHelper;
    procedure SetMenu(const Value: TComponent);
    procedure SetText(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    function CanShowPopup: Boolean;
    function ClosePopup: Boolean;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; virtual;
    function Popup(var AClosedByEscape: Boolean): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation);

    property IAccessibilityHelper: IdxBarAccessibilityHelper read GetIAccessibilityHelper;
    property IMenu: IdxRibbonApplicationMenu read FIMenu;
  public
    constructor Create(ARibbon: TdxCustomRibbon); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property KeyTip: string read FKeyTip write FKeyTip;
    property Menu: TComponent read FMenu write SetMenu;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxRibbonContext }

  TdxRibbonContext = class(TCollectionItem)
  strict private
    FAlpha: Byte;
    FAlphaAnimation: TdxAnimationTransition;
    FCaption: string;
    FColor: TColor;
    FMergeOrder: Integer;
    FVisible: Boolean;

    function GetCollection: TdxRibbonContexts;
    function GetMergedWithContext: TdxRibbonContext;
    function GetRibbon: TdxCustomRibbon;
    function GetTab(Index: Integer): TdxRibbonTab;
    function GetTabCount: Integer;
    procedure SetAlpha(AValue: Byte);
    procedure SetCaption(const AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetMergeOrder(const AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  protected
    FCreatedByMerging: Boolean;
    FCreatedByMergingWith: TdxRibbonContext;

    function CanAnimateContextShowing: Boolean;
    function GetDisplayName: string; override;
    procedure ColorChanged;
    procedure UpdateMergeTargetVisibility;
    procedure VisibleChanged;

    property Alpha: Byte read FAlpha write SetAlpha;
    property Collection: TdxRibbonContexts read GetCollection;
    property MergedWithContext: TdxRibbonContext read GetMergedWithContext;
    property Ribbon: TdxCustomRibbon read GetRibbon;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Activate(AActivateFirstTab: Boolean = True);
    procedure Assign(Source: TPersistent); override;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TdxRibbonTab read GetTab;
  published
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clWhite;
    property MergeOrder: Integer read FMergeOrder write SetMergeOrder default 0;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

  { TdxRibbonContextAnimation }

  TdxRibbonContextAnimation = class(TdxAnimationTransition)
  strict private
    FContext: TdxRibbonContext;
  protected
    procedure DoAnimate; override;
  public
    constructor Create(ATime: Cardinal; AContext: TdxRibbonContext); reintroduce;
  end;

  { TdxRibbonContexts }

  TdxRibbonContexts = class(TCollection)
  strict private
    FRibbon: TdxCustomRibbon;

    function GetActiveContext: TdxRibbonContext;
    function GetItem(Index: Integer): TdxRibbonContext;
    procedure SetItem(Index: Integer; const Value: TdxRibbonContext);
  protected
    function GetItemFromIndex(AIndex: Integer): TdxRibbonContext;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce; virtual;
    function Add: TdxRibbonContext;
    function Find(const ACaption: string): TdxRibbonContext;
    function IndexOf(AContext: TdxRibbonContext): Integer;
    function Insert(AIndex: Integer): TdxRibbonContext;

    property ActiveContext: TdxRibbonContext read GetActiveContext;
    property Items[Index: Integer]: TdxRibbonContext read GetItem write SetItem; default;
    property Ribbon: TdxCustomRibbon read FRibbon;
  end;

  { TdxRibbonTabMergeData }

  TdxRibbonTabMergeData = class(TdxRibbonCustomMergeData)
  public
    MergedWith: TdxRibbonTab;
  end;

  { TdxRibbonTab }

  TdxRibbonTabClass = class of TdxRibbonTab;

  TdxRibbonTab = class(
    TcxComponentCollectionItem,
    IdxBarSelectableItem,
    IdxFadingObject,
    IdxFadingObjectFadingOptions,
    IdxAdornerTargetElement)
  private
    FCaption: string;
    FContext: TdxRibbonContext;
    FContextIndex: Integer;
    FDesignSelectionHelper: IdxBarSelectableItem;
    FDockControl: TdxRibbonGroupsDockControl;
    FGroups: TdxRibbonTabGroups;
    FIAccessibilityHelper: IdxBarAccessibilityHelper;
    FKeyTip: string;
    FLastIndex: Integer;
    FLoadedIndex: Integer;
    FLocked: Boolean;
    FMergeData: TdxRibbonTabMergeData;
    FMergeKind: TdxRibbonMergeKind;
    FMergeOrder: Integer;
    FIsPredefined: Boolean;
    FRibbon: TdxCustomRibbon;
    FSaveContextIndex: Integer;
    FVisible: Boolean;

    function GetActive: Boolean;
    function GetDisplayCaption: string;
    function GetFocused: Boolean;
    function GetHighlighted: Boolean;
    function GetIAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetIsDestroying: Boolean;
    function GetViewInfo: TdxRibbonTabViewInfo;
    function GetVisibleIndex: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetContext(AValue: TdxRibbonContext);
    procedure SetGroups(const Value: TdxRibbonTabGroups);
    procedure SetHighlighted(Value: Boolean);
    procedure SetMergeOrder(const AValue: Integer);
    procedure SetRibbon(Value: TdxCustomRibbon);
    procedure SetVisible(Value: Boolean);
    procedure ReadContextIndex(Reader: TReader);
    procedure ReadIndex(Reader: TReader);
    procedure WriteContextIndex(Writer: TWriter);
    procedure WriteIndex(Writer: TWriter);
  protected
    // IdxFadingObject
    procedure IdxFadingObject.DrawFadeImage = FadingDrawFadeImage;
    procedure IdxFadingObject.GetFadingImages = FadingGetFadingImages;
    function CanFade: Boolean;
    procedure FadingDrawFadeImage;
    procedure FadingGetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    // IdxFadingObjectFadingOptions
    function GetFadingOptions: TdxFadingOptions;
    // Merging
    function InternalUnmerge(ATab: TdxRibbonTab): Boolean; virtual;
    procedure UnmergeBeforeDestroy;
    //inherited
    procedure DefineProperties(Filer: TFiler); override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    function GetDisplayName: string; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    //methods
    procedure Activate; virtual;
    procedure AssignCommonProperties(ASource: TdxRibbonTab); virtual;
    procedure ChangeAllGroupsVisibility(const AValue: Boolean);
    procedure CheckGroupToolbarsDockControl;
    procedure Deactivate; virtual;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; virtual;
    function GetDockControlBounds: TRect; virtual;
    function GetGroupClass: TdxRibbonTabGroupClass; virtual;
    function IsVisible: Boolean;
    procedure RecalculateBars;
    procedure RestoreContext;
    procedure RestoreIndex;
    procedure SaveContext;
    procedure ScrollDockControlGroups(AScrollLeft, AOnTimer: Boolean);
    procedure UpdateBarManager(ABarManager: TdxBarManager);
    procedure UpdateColorScheme; virtual;
    procedure UpdateContextIndex;
    procedure UpdateDockControl;
    procedure UpdateDockControlBounds;
    procedure UpdateGroupsFont;
    function VisibleBeforeMerging: Boolean;

    function GetIniSection(const ABaseSection, ADelimiter: string): string;
    procedure LoadFromIni(ASource: TCustomIniFile; const ABaseSection, ADelimiter: string);
    procedure SaveToIni(ADestination: TCustomIniFile; const ABaseSection, ADelimiter: string);

    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    procedure GetAdornerTargetElements(AList: TStrings); override;

    property DesignSelectionHelper: IdxBarSelectableItem read FDesignSelectionHelper implements IdxBarSelectableItem;
    property DisplayCaption: string read GetDisplayCaption;
    property Focused: Boolean read GetFocused;
    property Highlighted: Boolean read GetHighlighted write SetHighlighted;
    property IsDestroying: Boolean read GetIsDestroying;
    property LastIndex: Integer read FLastIndex;
    property LoadedIndex: Integer read FLoadedIndex;
    property Locked: Boolean read FLocked;
    property MergeData: TdxRibbonTabMergeData read FMergeData;
    property ViewInfo: TdxRibbonTabViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddToolBar(AToolBar: TdxBar);
    procedure Invalidate;
    procedure MakeVisible;

    function IsMergeAllowed: Boolean; virtual;
    function IsMerged: Boolean; virtual;
    procedure Merge(ATab: TdxRibbonTab; AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(ATab: TdxRibbonTab = nil); virtual;

    property DockControl: TdxRibbonGroupsDockControl read FDockControl;
    property IAccessibilityHelper: IdxBarAccessibilityHelper read GetIAccessibilityHelper;
    property IsPredefined: Boolean read FIsPredefined;
    property Ribbon: TdxCustomRibbon read FRibbon write SetRibbon;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property Caption: string read FCaption write SetCaption;
    property Context: TdxRibbonContext read FContext write SetContext stored False;
    property Groups: TdxRibbonTabGroups read FGroups write SetGroups;
    property KeyTip: string read FKeyTip write FKeyTip;
    property MergeKind: TdxRibbonMergeKind read FMergeKind write FMergeKind default rmkMerge;
    property MergeOrder: Integer read FMergeOrder write SetMergeOrder default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleIndex: Integer read GetVisibleIndex;
  end;

  { TdxRibbonTabCollection }

  TdxRibbonTabCollection = class(TcxComponentCollection)
  private
    FOwner: TdxCustomRibbon;
    // Accessibility
    FIAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetIAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetItem(Index: Integer): TdxRibbonTab;
    procedure SetItem(Index: Integer; const Value: TdxRibbonTab);
  protected
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; virtual;
    function GetIniSection(const ABaseSection, ADelimiter: string; const ATabIndex: Integer): string; overload;
    function GetIniSection(const ATabName: string; ASource: TCustomIniFile): string; overload;
    function FindByComponentName(const AName: string): TdxRibbonTab;
    function FindByLoadedIndex(AIndex: Integer): TdxRibbonTab;
    procedure InternalUnmerge(ATab: TdxRibbonTab = nil);
    procedure Notify(AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); override;
    procedure RemoveContext(AContext: TdxRibbonContext);
    procedure RestoreContexts;
    procedure RestoreOrder;
    procedure SaveContexts;
    procedure SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1); override;
    procedure Update(AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification); override;
    procedure UpdateBarManager(ABarManager: TdxBarManager);
    procedure UpdateContexts;

    property IAccessibilityHelper: IdxBarAccessibilityHelper read GetIAccessibilityHelper;
    property Owner: TdxCustomRibbon read FOwner;
  public
    constructor Create(AOwner: TdxCustomRibbon); reintroduce;
    destructor Destroy; override;
    function Add: TdxRibbonTab;
    function ContainsToolBar(AToolBar: TdxBar): Boolean;
    function Find(const ACaption: string): TdxRibbonTab;
    function Insert(AIndex: Integer): TdxRibbonTab;
    function IsMerged: Boolean;
    //
    property Items[Index: Integer]: TdxRibbonTab read GetItem write SetItem; default;
  end;

  { TdxRibbonFonts }

  TdxRibbonAssignedFont = (afTabHeader, afGroup, afGroupHeader, afApplicationButton);
  TdxRibbonAssignedFonts = set of TdxRibbonAssignedFont;

  TdxRibbonFonts = class(TPersistent)
  strict private
    FAssignedFonts: TdxRibbonAssignedFonts;
    FCaptionFont: TFont;
    FDocumentNameColor: TColor;
    FFonts: array[TdxRibbonAssignedFont] of TFont;
    FInternalCaptionFont: TFont;
    FInternalFonts: array[TdxRibbonAssignedFont] of TFont;
    FLocked: Boolean;
    FRibbon: TdxCustomRibbon;

    procedure FontChanged(Sender: TObject);
    function GetDefaultCaptionTextColor(AIsActive: Boolean): TColor;
    function GetFont(const Index: Integer): TFont;
    function GetInternalFont(AIndex: TdxRibbonAssignedFont): TFont;
    function IsFontStored(const Index: Integer): Boolean;
    procedure SetAssignedFonts(const Value: TdxRibbonAssignedFonts);
    procedure SetDocumentNameColor(const Value: TColor);
    procedure SetFont(const Index: Integer; const Value: TFont);
  protected
    procedure ChangeScale(M, D: Integer);
    function GetFormCaptionFont: TFont; overload; virtual;
    function GetPartColor(APart: Integer; AState: Integer = 0): TColor;
    procedure Invalidate;
    procedure UpdateFonts;

    property Locked: Boolean read FLocked;
  public
    constructor Create(AOwner: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function GetApplicationButtonFont(AState: TdxRibbonApplicationButtonState): TFont; virtual;
    function GetContextFont(AContextColor: TColor = clDefault): TFont; virtual;
    function GetFormCaptionFont(AIsActive: Boolean): TFont; overload; virtual;
    function GetGroupFont: TFont; virtual;
    function GetGroupHeaderFont: TFont; virtual;
    function GetTabHeaderFont(AState: Integer; AContext: TdxRibbonContext): TFont;

    property Ribbon: TdxCustomRibbon read FRibbon;
  published
    property ApplicationButton: TFont index Ord(afApplicationButton) read GetFont write SetFont stored IsFontStored;
    property AssignedFonts: TdxRibbonAssignedFonts read FAssignedFonts write SetAssignedFonts default [];
    property DocumentNameColor: TColor read FDocumentNameColor write SetDocumentNameColor default clDefault;
    property Group: TFont index Ord(afGroup) read GetFont write SetFont stored IsFontStored;
    property GroupHeader: TFont index Ord(afGroupHeader) read GetFont write SetFont stored IsFontStored;
    property TabHeader: TFont index Ord(afTabHeader) read GetFont write SetFont stored IsFontStored;
  end;

  { TdxRibbonHolderComponent }

  TdxRibbonHolderComponent = class(TcxComponentHolder)
  strict private
    procedure CheckAssignRibbon(AOwner: TComponent);
    function GetRibbon: TdxCustomRibbon;
    procedure SetRibbon(Value: TdxCustomRibbon);
  public
    constructor Create(AOwner: TComponent);

    property Ribbon: TdxCustomRibbon read GetRibbon write SetRibbon;
  end;

  { TdxRibbonCustomPopupComponent }

  TdxRibbonCustomPopupComponent = class(TdxBarCustomPopupComponent)
  strict private
    FRibbonHolder: TdxRibbonHolderComponent;

    function GetRibbon: TdxCustomRibbon;
    procedure SetRibbon(const Value: TdxCustomRibbon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Ribbon: TdxCustomRibbon read GetRibbon write SetRibbon;
  end;

  { TdxRibbonPopupMenu }

  TdxRibbonCustomPopupMenu = class(TdxBarCustomPopupMenu)
  strict private
    FRibbonHolder: TdxRibbonHolderComponent;

    procedure SetRibbon(Value: TdxCustomRibbon);
    function GetRibbon: TdxCustomRibbon;
  protected
    function CreateBarControl: TCustomdxBarControl; override;
    function GetControlClass: TCustomdxBarControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Ribbon: TdxCustomRibbon read GetRibbon write SetRibbon;
  end;

  { TdxRibbonPopupMenu }

  TdxRibbonPopupMenu = class(TdxRibbonCustomPopupMenu)
  published
    property BarManager;
    property BarSize;
    property Font;
    property ItemLinks;
    property ItemOptions;
    property PopupAlignment;
    property Ribbon;
    property UseOwnFont;
    property UseRecentItems;
    property OnCloseUp;
    property OnPaintBar;
    property OnPopup;
    property OnShow;
  end;

  { TdxRibbonPopupMenuControl }

  TdxRibbonPopupMenuControl = class(TdxBarSubMenuControl)
  protected
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
  end;

  { TdxBarApplicationMenu }

  TdxBarApplicationMenu = class(TdxBarCustomApplicationMenu, IdxRibbonApplicationMenu)
  strict private
    FClosedByEscape: Boolean;
    FRibbonPainter: TdxBarPainter;
  protected
    procedure DoCloseUp; override;
    procedure DoPopup; override;
    function GetControlClass: TCustomdxBarControlClass; override;
    // IdxRibbonApplicationMenu
    function ApplicationMenuPopup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean;
    function CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
    function ClosePopup: Boolean;
    function GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
    function GetOrigin(AIsClientArea: Boolean): TPoint;
    procedure GetTabOrderList(List: TList);
    function GetRootAccessibilityHelper: IdxBarAccessibilityHelper;
    function IsVisible: Boolean;
    function IdxRibbonApplicationMenu.Popup = ApplicationMenuPopup;
    procedure RibbonFormResized;
    procedure SelectAppMenuFirstItemControl;
    procedure ShowKeyTips;
    procedure UpdateNonClientArea;
  public
    procedure Popup(X, Y: Integer); override;
  published
    property BackgroundBitmap;
    property BarManager;
    property BarSize;
    property Buttons;
    property ExtraPane;
    property ExtraPaneEvents;
    property Font;
    property ItemLinks;
    property ItemOptions;
    property UseOwnFont;

    property OnCloseUp;
    property OnPaintBar;
    property OnPopup;

    // obsolete
    property ExtraPaneWidthRatio stored False;
    property ExtraPaneSize stored False;
    property ExtraPaneItems stored False;
    property ExtraPaneHeader stored False;
    property OnExtraPaneItemClick stored False;
  end;

  { TdxRibbonApplicationMenuControl }

  TdxRibbonApplicationMenuControl = class(TdxBarApplicationMenuControl)
  private
    function GetRibbon: TdxCustomRibbon;
    procedure DoPopupMenuClick(Sender: TObject);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;

    function GetPopupMenuItems: TdxRibbonPopupMenuItems;
    procedure PopupMenuClick(Sender: TObject);
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonController }

  TdxRibbonController = class(TcxIUnknownObject, IdxBarHintKeeper)
  strict private
    FHintInfo: TdxRibbonHitInfo;
    FHitInfo: TdxRibbonHitInfo;
    FHotButton: TdxRibbonCustomButtonViewInfo;
    FHotObject: TdxRibbonHitTest;
    FPressedButton: TdxRibbonCustomButtonViewInfo;
    FPressedContext: TdxRibbonContext;
    FPressedObject: TdxRibbonHitTest;
    FRibbon: TdxCustomRibbon;
    FScrollKind: TdxRibbonHitTest;
    FScrollTimer: TcxTimer;
    FSkipDblClick: Boolean;

    procedure CancelScroll;
    function CanProcessDesignTime: Boolean;
    procedure CreateTimer;
    function GetApplicationButton: TdxRibbonApplicationButton;
    function GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
    function GetApplicationMenuState: TdxRibbonApplicationMenuState;
    function GetViewInfo: TdxRibbonViewInfo;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SetHintInfo(const Value: TdxRibbonHitInfo);
    procedure SetHotButton(AValue: TdxRibbonCustomButtonViewInfo);
    procedure SetPressedButton(AValue: TdxRibbonCustomButtonViewInfo);
    procedure SetPressedObject(const Value: TdxRibbonHitTest);
  protected
    function NextTabCore(ATab: TdxRibbonTab; ADirection: Integer): TdxRibbonTab;

    // IdxBarHintKeeper
    function DoHint(var ANeedDeactivate: Boolean; out AHintText: string; out AShortCut: string): Boolean;
    function CreateHintViewInfo(const AHintText, AShortCut: string): TdxBarCustomHintViewInfo;
    function GetEnabled: Boolean;
    function GetHintPosition(const ACursorPos: TPoint; AHeight: Integer): TPoint;

    procedure CancelHint;
    procedure CancelMode; virtual;
    function CanSwitchMinimizedOnDblClick: Boolean; virtual;
    function CloseApplicationMenu: Boolean;
    procedure DesignTabMenuClick(Sender: TObject);
    procedure DoScroll(AOnTimer: Boolean);
    procedure HideHint; virtual;
    procedure InitTabDesignMenu(AItemLinks: TdxBarItemLinks); virtual;
    function IsApplicationMenuDropped: Boolean;
    function IsNeedShowHint(AObject: TdxRibbonHitTest): Boolean; virtual;
    function IsOwnerForHintObject(AObject: TdxRibbonHitTest): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function NotHandleMouseMove(P: TPoint): Boolean; virtual;
    function ProcessApplicationButtonMouseDown(Button: TMouseButton; AShift: TShiftState): Boolean;
    procedure ProcessContextsOnMouseDown(AIsDoubleClick: Boolean);
    procedure ProcessTabClick(ATab: TdxRibbonTab; Button: TMouseButton; Shift: TShiftState);
    procedure ProcessTabOnMouseDown(AButton: TMouseButton; AShift: TShiftState);
    procedure ScrollGroups(AScrollLeft, AOnTimer: Boolean);
    procedure ScrollTabs(AScrollLeft, AOnTimer: Boolean);
    procedure ShowTabDesignMenu; virtual;
    procedure StartScroll(AScrollKind: TdxRibbonHitTest);
    procedure UpdateButtonsStates; virtual;

    property HintInfo: TdxRibbonHitInfo read FHintInfo write SetHintInfo;
    property HitInfo: TdxRibbonHitInfo read FHitInfo;
    property HotObject: TdxRibbonHitTest read FHotObject write FHotObject;
    property PressedObject: TdxRibbonHitTest read FPressedObject write SetPressedObject;
  public
    constructor Create(ARibbon: TdxCustomRibbon); virtual;
    destructor Destroy; override;
    function NextTab(ATab: TdxRibbonTab): TdxRibbonTab;
    function PrevTab(ATab: TdxRibbonTab): TdxRibbonTab;

    property ApplicationButton: TdxRibbonApplicationButton read GetApplicationButton;
    property ApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo read GetApplicationButtonViewInfo;
    property ApplicationMenuState: TdxRibbonApplicationMenuState read GetApplicationMenuState;
    property HotButton: TdxRibbonCustomButtonViewInfo read FHotButton write SetHotButton;
    property PressedButton: TdxRibbonCustomButtonViewInfo read FPressedButton write SetPressedButton;
    property Ribbon: TdxCustomRibbon read FRibbon;
    property ScrollKind: TdxRibbonHitTest read FScrollKind;
    property ViewInfo: TdxRibbonViewInfo read GetViewInfo;
  end;

  { TdxRibbonGroupsDockControlScrollButtonViewInfo }

  TdxRibbonGroupsDockControlScrollButtonViewInfo = class(TdxRibbonCustomScrollButtonViewInfo)
  protected
    procedure DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean; override;
  end;

  { TdxRibbonGroupsDockControlSiteViewInfo }

  TdxRibbonGroupsDockControlSiteViewInfo = class(TdxRibbonButtonsContainerViewInfo)
  private
    FMinimizeRibbonButton: TdxRibbonMinimizeButtonViewInfo;
    FSite: TdxRibbonGroupsDockControlSite;
    FTabGroupsScrollButtonLeft: TdxRibbonGroupsDockControlScrollButtonViewInfo;
    FTabGroupsScrollButtonRight: TdxRibbonGroupsDockControlScrollButtonViewInfo;
    FTabGroupsScrollButtons: TdxRibbonScrollButtons;
    function GetRibbonActiveTab: TdxRibbonTab;
    function GetRibbonViewInfo: TdxRibbonViewInfo;
  protected
    procedure CalculateButtonsBounds(const ABounds: TRect); override;
    procedure DrawRibbonParts(ACanvas: TcxCanvas);
  public
    constructor Create(AOwner: TdxRibbonViewInfo); override;
    procedure Calculate(const ABounds: TRect); override;
    procedure Draw(ACanvas: TcxCanvas); override;
    procedure InvalidateRect(const R: TRect); override;
    function GetMinimizeRibbonButtonSize: TSize; virtual;
    //
    property RibbonActiveTab: TdxRibbonTab read GetRibbonActiveTab;
    property RibbonViewInfo: TdxRibbonViewInfo read GetRibbonViewInfo;
    property TabGroupsScrollButtons: TdxRibbonScrollButtons read FTabGroupsScrollButtons;
    property TabGroupsScrollButtonLeft: TdxRibbonGroupsDockControlScrollButtonViewInfo read FTabGroupsScrollButtonLeft;
    property TabGroupsScrollButtonRight: TdxRibbonGroupsDockControlScrollButtonViewInfo read FTabGroupsScrollButtonRight;
  end;

  { TdxRibbonGroupsDockControlSite }

  TdxRibbonGroupsDockControlSite = class(TcxControl)
  strict private
    FRibbon: TdxCustomRibbon;

    function GetDockControl: TdxRibbonGroupsDockControl;
  protected
    // IdxGestureClient
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer); override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    // IdxGestureOwner
    function IsGestureTarget(AWnd: THandle): Boolean; override;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCancelMode; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function MayFocus: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Paint; override;
    procedure SetRedraw(ARedraw: Boolean);

    property Ribbon: TdxCustomRibbon read FRibbon;
    property DockControl: TdxRibbonGroupsDockControl read GetDockControl;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce;
    function CanFocus: Boolean; override;
  end;

  { TdxRibbonFadingOptions }

  TdxRibbonOptionsFading = class(TPersistent)
  private
    FApplicationButton: TdxFadingOptions;
    FTabs: TdxFadingOptions;
    FTabGroups: TdxFadingOptions;
    FBarItems: TdxFadingOptions;

    procedure SetApplicationButton(AValue: TdxFadingOptions);
    procedure SetBarItems(AValue: TdxFadingOptions);
    procedure SetTabGroups(AValue: TdxFadingOptions);
    procedure SetTabs(AValue: TdxFadingOptions);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ApplicationButton: TdxFadingOptions read FApplicationButton write SetApplicationButton;
    property BarItems: TdxFadingOptions read FBarItems write SetBarItems;
    property TabGroups: TdxFadingOptions read FTabGroups write SetTabGroups;
    property Tabs: TdxFadingOptions read FTabs write SetTabs;
  end;

  { TdxRibbonMergeData }

  TdxRibbonMergeData = class(TdxRibbonCustomMergeData)
  public
    ActiveTabBeforeMerging: TdxRibbonTab;
    MergedWith: TdxCustomRibbon;
  end;

  { TdxRibbonTouchModeHelper }

  TdxRibbonTouchModeHelper = class
  private
    FRibbon: TdxCustomRibbon;
  public
    constructor Create(ARibbon: TdxCustomRibbon);
    procedure AdjustFormBorderIconSize(AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; var ASize: TSize); virtual;
    procedure AdjustFormCaptionHeight(var AValue: Integer); virtual;
    procedure AdjustMargins(var AMargins: TRect; AMaxTargetValue: Integer); virtual;
    procedure AdjustPartOffsets(APart: Integer; var AValue: TRect); virtual;
    procedure AdjustPartSize(APart: Integer; var AValue: Integer); virtual;
    function IsEnabled: Boolean;
    //
    property Ribbon: TdxCustomRibbon read FRibbon;
  end;

  { TdxCustomRibbon }

  TdxRibbonStates = (rsTabsLoaded, rsCancelHintLocked, rsModifiedLocked, rsUpdatingColorScheme, rsSizing);
  TdxRibbonState = set of TdxRibbonStates;

  TdxRibbonEvent = procedure(Sender: TdxCustomRibbon) of object;
  TdxRibbonApplicationMenuClickEvent = procedure (Sender: TdxCustomRibbon; var AHandled: Boolean) of object;
  TdxRibbonHideMinimizedByClickEvent = procedure(Sender: TdxCustomRibbon; AWnd: HWND;
    AShift: TShiftState; const APos: TPoint; var AAllowProcessing: Boolean) of object;
  TdxRibbonTabChangingEvent = procedure(Sender: TdxCustomRibbon; ANewTab: TdxRibbonTab; var Allow: Boolean) of object;
  TdxRibbonTabGroupNotifyEvent = procedure(Sender: TdxCustomRibbon; ATab: TdxRibbonTab; AGroup: TdxRibbonTabGroup) of object;

  TdxRibbonInternalState = (risCreating, risAppMenuActive);
  TdxRibbonInternalStates = set of TdxRibbonInternalState;

  TdxCustomRibbon = class(TcxControl,
    IdxSkin,
    IdxRibbonFormClient,
    IdxRibbonFormNonClientPart,
    IdxRibbonFormNonClientHelper,
    IdxRibbonFormNonClientDraw,
    IdxRibbonMouseWheelReceiver,
    IdxFormKeyPreviewListener,
    IdxBarManagerMergeOperationHandler,
    IdxBarAccessibleObject)
  private
    FActiveTab: TdxRibbonTab;
    FApplicationButton: TdxRibbonApplicationButton;
    FBackgroundImage: TdxSmartGlyph;
    FBarManager: TdxBarManager;
    FCapitalizeTabCaptions: TdxDefaultBoolean;
    FColorScheme: TdxCustomRibbonSkin;
    FColorSchemeAccent: TdxRibbonColorSchemeAccent;
    FColorSchemeHandlers: TcxEventHandlerCollection;
    FContexts: TdxRibbonContexts;
    FController: TdxRibbonController;
    FDocumentName: TCaption;
    FEnableTabAero: Boolean;
    FFading: Boolean;
    FFonts: TdxRibbonFonts;
    FFormCaptionHelper: TdxRibbonFormCaptionHelper;
    FGroupsDockControlSite: TdxRibbonGroupsDockControlSite;
    FGroupsPainter: TdxRibbonBarPainter;
    FHelpButton: TdxRibbonHelpButton;
    FHighlightedTab: TdxRibbonTab;
    FHorizontalNavigationList: TInterfaceList;
    FInternalItems: TComponentList;
    FInternalState: TdxRibbonInternalStates;
    FListeners: TInterfaceList;
    FLoadedActiveTab: TdxRibbonTab;
    FLoadedHeight: Integer;
    FLockCount: Integer;
    FMergeData: TdxRibbonMergeData;
    FMinimizeOnTabDblClick: Boolean;
    FOptionsFading: TdxRibbonOptionsFading;
    FPainter: TdxRibbonPainter;
    FPopupMenuItems: TdxRibbonPopupMenuItems;
    FPreviousActiveTab: TdxRibbonTab;
    FQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
    FRibbonFormNonClientParts: TObjectList;
    FShowMinimizeButton: Boolean;
    FShowTabGroups: Boolean;
    FShowTabHeaders: Boolean;
    FSupportNonClientDrawing: Boolean;
    FTabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar;
    FTabAreaToolbar: TdxRibbonTabAreaToolbar;
    FTabGroupsPopupWindow: TdxRibbonTabGroupsPopupWindow;
    FTabs: TdxRibbonTabCollection;
    FTouchModeHelper: TdxRibbonTouchModeHelper;
    FViewInfo: TdxRibbonViewInfo;

    // Accessibility
    FAffiliatedBarControlsForAccessibility: TComponentList;
    FIAccessibilityHelper: IdxBarAccessibilityHelper;

    FOnApplicationMenuClick: TdxRibbonApplicationMenuClickEvent;
    FOnHelpButtonClick: TdxRibbonEvent;
    FOnHideMinimizedByClick: TdxRibbonHideMinimizedByClickEvent;
    FOnMinimizedChanged: TdxRibbonEvent;
    FOnMoreCommandsExecute: TdxRibbonEvent;
    FOnTabChanged: TdxRibbonEvent;
    FOnTabChanging: TdxRibbonTabChangingEvent;
    FOnTabGroupCollapsed: TdxRibbonTabGroupNotifyEvent;
    FOnTabGroupExpanded: TdxRibbonTabGroupNotifyEvent;

    procedure BackgroundImageChangeHandler(Sender: TObject);
    procedure CheckDrawRibbonFormStatusBarBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect);
    procedure DrawApplicationMenuHeader(ADC: THandle; AIsClientArea: Boolean);
    function GetActiveTab: TdxRibbonTab;
    function GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
    function GetApplicationMenuState: TdxRibbonApplicationMenuState;
    function GetColorSchemeName: string;
    function GetHelpButtonScreenTip: TdxScreenTip;
    function GetHidden: Boolean;
    function GetIniSection(const ADelimiter: string; const ASection: string): string;
    function GetIsPopupGroupsMode: Boolean;
    function GetNextActiveTab(ATab: TdxRibbonTab): TdxRibbonTab;
    function GetRibbonForm: TdxCustomRibbonForm;
    function GetStyle: TdxRibbonStyle;
    function GetTabCount: Integer;
    function GetVisibleTab(Index: Integer): TdxRibbonTab;
    function GetVisibleTabCount: Integer;
    procedure InitCustomizePopupMenu(AItemLinks: TdxBarItemLinks);
    procedure InitColorScheme;
    function IsOnRibbonMDIForm: Boolean;
    procedure InternalCloseTabGroupsPopupWindow(AAllowAnimation: Boolean = True);
    procedure RecreateViewInfo;
    procedure RibbonFormInvalidate;
    procedure SetActiveTab(Value: TdxRibbonTab);
    procedure SetApplicationButton(AValue: TdxRibbonApplicationButton);
    procedure SetBackgroundImage(AValue: TdxSmartGlyph);
    procedure SetBarManager(Value: TdxBarManager);
    procedure SetCapitalizeTabCaptions(const Value: TdxDefaultBoolean);
    procedure SetColorScheme(const AName: string; AStyle: TdxRibbonStyle); overload;
    procedure SetColorScheme(Value: TdxCustomRibbonSkin); overload;
    procedure SetColorSchemeAccent(AValue: TdxRibbonColorSchemeAccent);
    procedure SetColorSchemeName(const Value: string);
    procedure SetContexts(const Value: TdxRibbonContexts);
    procedure SetDocumentName(const Value: TCaption);
    procedure SetEnableTabAero(AValue: Boolean);
    procedure SetFading(const Value: Boolean);
    procedure SetFonts(const Value: TdxRibbonFonts);
    procedure SetHelpButton(const Value: TdxRibbonHelpButton);
    procedure SetHelpButtonScreenTip(const Value: TdxScreenTip);
    procedure SetHighlightedTab(const Value: TdxRibbonTab);
    procedure SetOnHelpButtonClick(const Value: TdxRibbonEvent);
    procedure SetOptionsFading(AValue: TdxRibbonOptionsFading);
    procedure SetPopupMenuItems(const Value: TdxRibbonPopupMenuItems);
    procedure SetQuickAccessToolbar(const Value: TdxRibbonQuickAccessToolbar);
    procedure SetShowMinimizeButton(const Value: Boolean);
    procedure SetShowTabGroups(const Value: Boolean);
    procedure SetShowTabHeaders(const Value: Boolean);
    procedure SetStyle(AValue: TdxRibbonStyle);
    procedure SetSupportNonClientDrawing(const Value: Boolean);
    procedure SetTabAreaSearchToolbar(const Value: TdxRibbonTabAreaSearchToolbar);
    procedure SetTabAreaToolbar(const Value: TdxRibbonTabAreaToolbar);
    procedure SetTabs(Value: TdxRibbonTabCollection);
    procedure UpdateColorScheme;
    procedure UpdateColorSchemeListeners;
    procedure UpdateNonClientDrawing;

    procedure CMSelectAppMenuFirstItemControl(var Message: TMessage); message DXM_BAR_SELECTAPPMENUFIRSTITEMCONTROL;
    procedure CMShowKeyTips(var Message: TMessage); message DXM_BAR_SHOWKEYTIPS;
    procedure DXMRecalculate(var Message: TMessage); message DXM_RIBBON_RECALCULATE;
    procedure DXMShowApplicationMenu(var Message: TMessage); message DXM_RIBBON_SHOWAPPLICATIONMENU;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure BarManagerAfterChange;
    procedure BarManagerBeforeChange;
    procedure BarManagerLoadIni(Sender: TObject; const AEventArgs);
    procedure BarManagerSaveIni(Sender: TObject; const AEventArgs);
    procedure BarManagerSaveState;
    procedure LoadTabsFromIni(AEventData: TdxBarIniFileEventData);
    procedure SaveTabsToIni(AEventData: TdxBarIniFileEventData);

    procedure MDIStateChanged(Sender: TObject; const AEventArgs);
    procedure SystemFontChanged(Sender: TObject; const AEventArgs);

    // Accessibility
    function GetIAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetTabsIAccessibilityHelper: IdxBarAccessibilityHelper;
  protected
    FState: TdxRibbonState;

    function CalculateFormCaptionHeight(AIsQuickAccessToolbarAtNonClientArea: Boolean): Integer; virtual;
    // IdxGestureClient
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer); override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    // IdxRibbonFormNonClientHelper
    procedure AdjustRibbonFormBorderIconSize(AIcon: TdxRibbonBorderDrawIcon;
      AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize); virtual;
    procedure DrawRibbonFormBackground(DC: HDC; const ARect: TRect);
    procedure DrawRibbonFormBorderIcon(ACanvas: TcxCanvas; const ABounds: TRect; AIcon: TdxRibbonBorderDrawIcon;
      AState: TdxRibbonBorderIconState);
    procedure DrawRibbonFormBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect);
    procedure DrawRibbonFormCaption(ACanvas: TcxCanvas; const ABounds: TRect; const ACaption: string);
    procedure GetApplicationMenuTabOrderList(List: TList);
    function GetBarManager: TdxBarManager;
    function GetBarPainter: TdxBarPainter;
    function GetRibbonFormCaptionAreaExtension: Integer; virtual;
    function GetRibbonFormCaptionHeight: Integer; virtual;
    function GetRibbonFormCaptionHeightForHiddenRibbon: Integer; virtual;
    function GetRibbonFormColor: TColor;
    function GetRibbonFormExtendedCaptionAreaRegion: HRGN; virtual;
    function GetRibbonLoadedHeight: Integer;
    function GetRibbonNonClientAreaObjectsRegion: HRGN;
    function GetRibbonQATNonClientAreaBounds: TRect;
    function GetRibbonQATOptionAddItemCaption: string;
    function GetRibbonQATOptionAddItemEnabled: Boolean;
    function GetRibbonQATPositionButtonCaption: string;
    function GetRibbonStyle: TdxRibbonStyle;
    function GetStatusBarInterface: IUnknown;
    function GetTaskbarCaption: TCaption; virtual;
    function GetValidPopupMenuItems: TdxRibbonPopupMenuItems; virtual;
    function GetWindowBordersWidth: TRect;
    function HasExternalRibbonFormShadow: Boolean;
    function HasHelpButton: Boolean;
    function HasStatusBar: Boolean;
    procedure UpdateNonClientArea; virtual;
    procedure UpdateToolbarsFonts;
    function UseRoundedWindowCorners: Boolean;

    // IdxRibbonFormClient
    procedure RibbonFormCaptionChanged; virtual;
    procedure RibbonFormIconChanged; virtual;
    procedure RibbonFormSized; virtual;
    procedure RibbonFormSizing; virtual;

    //IdxSkin
    procedure IdxSkin.DrawBackground = SkinDrawBackground;
    procedure IdxSkin.DrawBackgroundEx = SkinDrawBackgroundEx;
    procedure IdxSkin.DrawCaption = SkinDrawCaption;
    function IdxSkin.GetIsAlphaUsed = SkinGetIsAlphaUsed;
    function IdxSkin.GetCaptionRect = SkinGetCaptionRect;
    function IdxSkin.GetContentOffsets = SkinGetContentOffsets;
    function IdxSkin.GetName = SkinGetName;
    function IdxSkin.GetPartColor = SkinGetPartColor;
    function IdxSkin.GetPartSize = SkinGetPartSize;
    function IdxSkin.GetPartColorPalette = SkinGetPartColorPalette;
    procedure DrawTabGroupBackground(DC: HDC; const ARect: TRect; AState: Integer; AIsInPopup: Boolean);
    function GetBarItemControlFadingOptions: TdxFadingOptions;
    function GetGroupCaptionHeight: Integer;
    function GetGroupContentHeight: Integer;
    function GetGroupHeight: Integer;
    function GetGroupRowHeight: Integer;
    //
    procedure SkinDrawBackground(DC: HDC; const ARect: TRect; APart, AState: Integer);
    procedure SkinDrawBackgroundEx(DC: HDC; const ARect: TRect; const AContentRect: TRect; APart: Integer; AState: Integer = 0);
    procedure SkinDrawCaption(DC: HDC; const ACaption: string; const ARect: TRect; APart, AState: Integer);
    function SkinGetCaptionRect(const ARect: TRect; APart: Integer): TRect;
    function SkinGetContentOffsets(APart: Integer): TRect;
    function SkinGetIsAlphaUsed(APart: Integer): Boolean;
    function SkinGetName: string;
    function SkinGetPartColor(APart: Integer; AState: Integer = 0): TColor;
    function SkinGetPartColorPalette(APart: Integer; AState: Integer = 0): IdxColorPalette;
    function SkinGetPartSize(APart: Integer): Integer;
    // IdxRibbonMouseWheelReceiver
    function CanProcessMouseWheel: Boolean;
    //IdxFormKeyPreviewListener
    procedure FormKeyDown(var Key: Word; Shift: TShiftState);
    //IdxBarAccessibleObject
    function GetAccessibilityHelper: IdxBarAccessibilityHelper;
    // IdxBarManagerMergeOperationHandler
    procedure ProcessMergeOperation(ABarManager: TdxBarManager;
      AOperation: TdxBarMergeOperation; var AHandled: Boolean);
    //IdxRibbonFormNonClientDraw
    procedure IdxRibbonFormNonClientDraw.Add = RibbonFormNonClientDrawAdd;
    procedure IdxRibbonFormNonClientDraw.Changed = RibbonFormNonClientDrawChanged;
    procedure IdxRibbonFormNonClientDraw.Remove = RibbonFormNonClientDrawRemove;
    procedure RibbonFormNonClientDrawAdd(AObject: TObject);
    procedure RibbonFormNonClientDrawChanged(AObject: TObject);
    procedure RibbonFormNonClientDrawRemove(AObject: TObject);

    procedure AfterApplicationMenuPopup;
    procedure BeforeApplicationMenuPopup;
    procedure BoundsChanged; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanScrollTabs: Boolean;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DoCancelMode; override;
    procedure DoMinimizeChanged; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure FontChanged; override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    function MayFocus: Boolean; override;
    procedure Modified; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    function RecreateWndOnBiDiModeChanged: Boolean; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure VisibleChanged; override;
    procedure Updating; override;
    procedure Updated; override;

    procedure GetAdornerTargetElements(AList: TStrings); override;

    // Merging
    function CreateContextAsByMerging(ASourceContext: TdxRibbonContext): TdxRibbonContext; virtual;
    function CreateTabAsByMerging(ASourceTab: TdxRibbonTab): TdxRibbonTab; virtual;
    function CreateToolbarAsByMerging: TdxBar; virtual;
    procedure UnmergeBeforeDestroy; virtual;

    function AddGroupButtonToQAT(ABar: TdxBar): TdxRibbonQuickAccessGroupButton;
    procedure CancelUpdate;
    function CanFade: Boolean;
    function CanPaint: Boolean;
    function CreateApplicationButton: TdxRibbonApplicationButton; virtual;
    function CreateController: TdxRibbonController; virtual;
    function CreateFormCaptionHelper: TdxRibbonFormCaptionHelper; virtual;
    function CreateGroupsPainter: TdxRibbonBarPainter; virtual;
    function CreateHelpButton: TdxRibbonHelpButton; virtual;
    function CreatePainter: TdxRibbonPainter; virtual;
    function CreateQuickAccessToolbar: TdxRibbonQuickAccessToolbar; virtual;
    function CreateTabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar; virtual;
    function CreateTabAreaToolbar: TdxRibbonTabAreaToolbar; virtual;
    function CreateTouchModeHelper: TdxRibbonTouchModeHelper; virtual;
    function CreateViewInfo: TdxRibbonViewInfo; virtual;
    procedure DesignAddTabGroup(ATab: TdxRibbonTab; ANewToolbar: Boolean);
    function DoApplicationMenuClick: Boolean;
    procedure DoHelpButtonClick; virtual;
    function DoHideMinimizedByClick(AWnd: HWND; AShift: TShiftState; const APos: TPoint): Boolean; virtual;
    function DoTabChanging(ANewTab: TdxRibbonTab): Boolean; virtual;
    procedure DoMoreCommandsExecute; virtual;
    procedure DoTabChanged; virtual;
    procedure DoTabGroupCollapsed(ATab: TdxRibbonTab; AGroup: TdxRibbonTabGroup); virtual;
    procedure DoTabGroupExpanded(ATab: TdxRibbonTab; AGroup: TdxRibbonTabGroup); virtual;

    function GetBar(ACustomizingBarControl: TCustomdxBarControl): TdxBar;
    function GetNextHorizontalAccessibleObject(ACurrentObject: IdxBarAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
    function GetTabClass: TdxRibbonTabClass; virtual;
    function IsAutoHidden: Boolean;
    function IsAutoHideAnimationIsInProgress: Boolean;
    function IsAutoHideModeActive: Boolean;
    function IsBarManagerValid: Boolean;
    function IsHelpButtonPlacedOnTabsArea: Boolean;
    function IsLocked: Boolean;
    function IsQuickAccessToolbarValid: Boolean;
    function IsSimplifiedGroupsLayout: Boolean;
    procedure InitializeRibbonForm;
    procedure PopulatePopupMenuItems(ALinks: TdxBarItemLinks; AItems: TdxRibbonPopupMenuItems; AOnClick: TNotifyEvent);
    procedure PopupMenuItemClick(Sender: TObject);
    procedure UpdateFormActionControl(ASetControl: Boolean);
    procedure SetRedraw(ARedraw: Boolean);
    procedure ShowCustomizePopup; virtual;
    procedure UpdateActiveTab;
    procedure UpdateHorizontalNavigationList;
    procedure UpdateHeight; virtual;
    procedure UpdateHiddenActiveTabDockControl;

    procedure AddTab(ATab: TdxRibbonTab);
    procedure RemoveTab(ATab: TdxRibbonTab);
    procedure SetNextActiveTab(ATab: TdxRibbonTab);

    procedure Changed;
    procedure FullInvalidate;
    procedure RecalculateBars;

    // Accessibility
    procedure AddAffiliatedBarControlForAccessibility(ABarControl: TCustomdxBarControl);
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; virtual;
    property TabsIAccessibilityHelper: IdxBarAccessibilityHelper read GetTabsIAccessibilityHelper;

    property ApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo read GetApplicationButtonViewInfo;
    property FormCaptionHelper: TdxRibbonFormCaptionHelper read FFormCaptionHelper;
    property GroupsPainter: TdxRibbonBarPainter read FGroupsPainter;
    property HighlightedTab: TdxRibbonTab read FHighlightedTab write SetHighlightedTab;
    property HorizontalNavigationList: TInterfaceList read FHorizontalNavigationList;
    property PreviousActiveTab: TdxRibbonTab read FPreviousActiveTab;
    property RibbonFormNonClientParts: TObjectList read FRibbonFormNonClientParts;
    property TabGroupsPopupWindow: TdxRibbonTabGroupsPopupWindow read FTabGroupsPopupWindow;

    property ApplicationMenuState: TdxRibbonApplicationMenuState read GetApplicationMenuState;
    property Controller: TdxRibbonController read FController;
    property Fading: Boolean read FFading write SetFading default False;
    property GroupsDockControlSite: TdxRibbonGroupsDockControlSite read FGroupsDockControlSite;
    property InternalState: TdxRibbonInternalStates read FInternalState;
    property MergeData: TdxRibbonMergeData read FMergeData;
    property Painter: TdxRibbonPainter read FPainter;
    property RibbonForm: TdxCustomRibbonForm read GetRibbonForm;
    property TouchModeHelper: TdxRibbonTouchModeHelper read FTouchModeHelper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ApplicationMenuPopup: Boolean;
    function AreGroupsVisible: Boolean;
    procedure BeginUpdate;
    function CanFocus: Boolean; override;
    procedure CheckHide;
    function ContainsToolBar(AToolBar: TdxBar): Boolean;
    procedure EndUpdate;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetTabAtPos(X, Y: Integer): TdxRibbonTab;
    procedure InvalidateRect(const R: TRect); reintroduce;
    function ShowCustomizationForm(AMode: TdxRibbonCustomizationFormMode): Boolean;

    procedure CloseTabGroupsPopupWindow;
    procedure ShowTabGroupsPopupWindow;

    procedure AddListener(AListener: IdxRibbonListener);
    procedure RemoveListener(AListener: IdxRibbonListener);

    function IsMerged: Boolean; virtual;
    procedure Merge(ARibbon: TdxCustomRibbon; AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions); virtual;
    procedure Unmerge(ARibbon: TdxCustomRibbon = nil); virtual;

    property ActiveTab: TdxRibbonTab read GetActiveTab write SetActiveTab;
    property ApplicationButton: TdxRibbonApplicationButton read FApplicationButton write SetApplicationButton;
    property BackgroundImage: TdxSmartGlyph read FBackgroundImage write SetBackgroundImage;
    property BarManager: TdxBarManager read FBarManager write SetBarManager;
    property CapitalizeTabCaptions: TdxDefaultBoolean read FCapitalizeTabCaptions write SetCapitalizeTabCaptions default bFalse;
    property ColorScheme: TdxCustomRibbonSkin read FColorScheme write SetColorScheme;
    property ColorSchemeAccent: TdxRibbonColorSchemeAccent read FColorSchemeAccent write SetColorSchemeAccent default rcsaYellow;
    property ColorSchemeHandlers: TcxEventHandlerCollection read FColorSchemeHandlers;
    property ColorSchemeName: string read GetColorSchemeName write SetColorSchemeName stored True;
    property Contexts: TdxRibbonContexts read FContexts write SetContexts;
    property DocumentName: TCaption read FDocumentName write SetDocumentName;
    property EnableTabAero: Boolean read FEnableTabAero write SetEnableTabAero default True;
    property Fonts: TdxRibbonFonts read FFonts write SetFonts;
    property HelpButton: TdxRibbonHelpButton read FHelpButton write SetHelpButton;
    property Hidden: Boolean read GetHidden;
    property IAccessibilityHelper: IdxBarAccessibilityHelper read GetIAccessibilityHelper;
    property IsPopupGroupsMode: Boolean read GetIsPopupGroupsMode;
    property LockCount: Integer read FLockCount;
    property MinimizeOnTabDblClick: Boolean read FMinimizeOnTabDblClick write FMinimizeOnTabDblClick default True;
    property OptionsFading: TdxRibbonOptionsFading read FOptionsFading write SetOptionsFading;
    property PopupMenuItems: TdxRibbonPopupMenuItems read FPopupMenuItems write SetPopupMenuItems default dxRibbonDefaultPopupMenuItems;
    property QuickAccessToolbar: TdxRibbonQuickAccessToolbar read FQuickAccessToolbar write SetQuickAccessToolbar;
    property ShowMinimizeButton: Boolean read FShowMinimizeButton write SetShowMinimizeButton default True;
    property ShowTabGroups: Boolean read FShowTabGroups write SetShowTabGroups default True;
    property ShowTabHeaders: Boolean read FShowTabHeaders write SetShowTabHeaders default True;
    property Style: TdxRibbonStyle read GetStyle write SetStyle default rs2007;
    property SupportNonClientDrawing: Boolean read FSupportNonClientDrawing write SetSupportNonClientDrawing default False;
    property TabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar read FTabAreaSearchToolbar write SetTabAreaSearchToolbar;
    property TabAreaToolbar: TdxRibbonTabAreaToolbar read FTabAreaToolbar write SetTabAreaToolbar;
    property TabCount: Integer read GetTabCount;
    property Tabs: TdxRibbonTabCollection read FTabs write SetTabs;
    property ViewInfo: TdxRibbonViewInfo read FViewInfo;
    property VisibleTabCount: Integer read GetVisibleTabCount;
    property VisibleTabs[Index: Integer]: TdxRibbonTab read GetVisibleTab;

    //obsolete HelpButtonOptions
    property HelpButtonScreenTip: TdxScreenTip read GetHelpButtonScreenTip write SetHelpButtonScreenTip;

    property OnApplicationMenuClick: TdxRibbonApplicationMenuClickEvent read FOnApplicationMenuClick write FOnApplicationMenuClick;
    property OnHelpButtonClick: TdxRibbonEvent read FOnHelpButtonClick write SetOnHelpButtonClick;
    property OnHideMinimizedByClick: TdxRibbonHideMinimizedByClickEvent read FOnHideMinimizedByClick write FOnHideMinimizedByClick;
    property OnMinimizedChanged: TdxRibbonEvent read FOnMinimizedChanged write FOnMinimizedChanged;
    property OnMoreCommandsExecute: TdxRibbonEvent read FOnMoreCommandsExecute write FOnMoreCommandsExecute;
    property OnTabChanged: TdxRibbonEvent read FOnTabChanged write FOnTabChanged;
    property OnTabChanging: TdxRibbonTabChangingEvent read FOnTabChanging write FOnTabChanging;
    property OnTabGroupCollapsed: TdxRibbonTabGroupNotifyEvent read FOnTabGroupCollapsed write FOnTabGroupCollapsed;
    property OnTabGroupExpanded: TdxRibbonTabGroupNotifyEvent read FOnTabGroupExpanded write FOnTabGroupExpanded;
  end;

  { TdxRibbon }

  TdxRibbon = class(TdxCustomRibbon)
  published
    //obsolete
    property HelpButtonScreenTip;

    property OptionsFading;
    property ApplicationButton;
    property BackgroundImage;
    property BarManager;
    property CapitalizeTabCaptions;
    property Style;
    property ColorSchemeAccent;
    property ColorSchemeName;
    property DocumentName;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableTabAero;
    property Fonts;
    property HelpButton;
    property MinimizeOnTabDblClick;
    property PopupMenuItems;
    property QuickAccessToolbar;
    property ShowMinimizeButton;
    property ShowTabGroups;
    property ShowTabHeaders;
    property SupportNonClientDrawing;
    property Contexts;
    property TabAreaSearchToolbar;
    property TabAreaToolbar;
    property Tabs;
    property TabOrder;
    property TabStop;

    property OnApplicationMenuClick;
    property OnHelpButtonClick;
    property OnHideMinimizedByClick;
    property OnMinimizedChanged;
    property OnMoreCommandsExecute;
    property OnTabChanged;
    property OnTabChanging;
    property OnTabGroupCollapsed;
    property OnTabGroupExpanded;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TdxRibbonQuickAccessToolbarHelper }

  TdxRibbonQuickAccessToolbarHelper = class(TObject)
  public
    class function HasGroupButtonForToolbar(AQATLinks: TdxBarItemLinks; AToolbar: TdxBar): Boolean;
    class function IsToolbarDockedInRibbon(ARibbon: TdxCustomRibbon; AToolbar: TdxBar): Boolean;
  end;

  { TdxRibbonQuickAccessGroupButton }

  TdxRibbonQuickAccessGroupButton = class(TdxBarItem)
  private
    FToolbar: TdxBar;
    function GetActualToolbar: TdxBar;
    procedure SetToolbar(Value: TdxBar);
  protected
    function CanBePlacedOn(AParentKind: TdxBarItemControlParentKind;
      AToolbar: TdxBar; out AErrorText: string): Boolean; override;
    function GetCaption: string; override;
    function HasGroupButtonForToolbar(AParentBar, AToolbar: TdxBar): Boolean;
    function IsCaptionStored: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCaption(const Value: string); override;
    //
    property ActualToolbar: TdxBar read GetActualToolbar;
  public
    function IsToolbarAcceptable(AToolbar: TdxBar): Boolean;
  published
    property Toolbar: TdxBar read FToolbar write SetToolbar;
  end;

  { TdxRibbonQuickAccessGroupButtonControl }

  TdxRibbonQuickAccessGroupButtonControl = class(TdxBarButtonLikeControl)
  private
    FPopupBarControl: TdxBarPopupControl;
    function GetItem: TdxRibbonQuickAccessGroupButton;
  protected
    procedure CalcDrawParams(AFull: Boolean = True); override;
    function CanActivate: Boolean; override;
    function CanDestroyOnClick: Boolean; override;
    procedure ClosePopup;
    procedure ControlClick(AByMouse: Boolean; AKey: Char = #0); override;
    procedure DoCloseUp(AHadSubMenuControl: Boolean); override;
    procedure DoDropDown(AByMouse: Boolean); override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    procedure DropDown(AByMouse: Boolean); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetCurrentImage(AViewSize: TdxBarItemControlViewSize; ASelected: Boolean; out AGlyph: TdxSmartGlyph;
      out AImages: TCustomImageList; out AImageIndex: Integer; out AIsImageEnabled: TdxDefaultBoolean): Boolean; override;
    function GetHint: string; override;
    function GetViewStructure: TdxBarItemControlViewStructure; override;
    function IsDropDown: Boolean; override;
  public
    destructor Destroy; override;
    function IsDroppedDown: Boolean; override;
    property Item: TdxRibbonQuickAccessGroupButton read GetItem;
  end;

  { TdxRibbonQuickAccessGroupButtonPopupBarControl }

  TdxRibbonQuickAccessGroupButtonPopupBarControl = class(TdxRibbonCollapsedGroupPopupBarControl)
  private
    FGroupButtonControl: TdxRibbonQuickAccessGroupButtonControl;
    FIsActiveChangeLocked: Boolean;
  protected
    function CanActiveChange: Boolean; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure FocusItemControl(AItemControl: TdxBarItemControl); override;
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function IsAllowContextPaint: Boolean; override;
  public
    constructor CreateForPopup(AGroupButtonControl: TdxRibbonQuickAccessGroupButtonControl); reintroduce; virtual;
    procedure CloseUp; override;
    procedure HideAllByEscape; override;
  end;

  { TdxAddGroupButtonEditor }

  TdxAddGroupButtonEditor = class(TdxAddSubItemEditor)
  protected
    class function GetAddedItemClass(const AAddedItemName: string): TdxBarItemClass; override;
    class function GetPopupItemCaption: string; override;
  end;

  { TdxRibbonAccessibilityHelper }

  TdxRibbonAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    FAccessibilityChildren: TList;
    FKeyTipWindowsManager: IdxBarKeyTipWindowsManager;
    function GetRibbon: TdxCustomRibbon;
    function GetTabsAreaButtonsViewInfo: TdxRibbonButtonsContainerViewInfo;
  protected
    // IdxBarAccessibilityHelper
    function AreKeyTipsSupported(out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean; override;
    function GetBarManager: TdxBarManager; override;
    function GetDefaultAccessibleObject: IdxBarAccessibilityHelper; override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function LogicalNavigationGetChild(AIndex: Integer): TdxBarAccessibilityHelper; override;
    function LogicalNavigationGetChildIndex(AChild: TdxBarAccessibilityHelper): Integer; override;

    property Ribbon: TdxCustomRibbon read GetRibbon;
    property TabsAreaButtonsViewInfo: TdxRibbonButtonsContainerViewInfo read GetTabsAreaButtonsViewInfo;
  public
    constructor Create(AOwnerObject: TObject); override;
    destructor Destroy; override;

    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxRibbonTabCollectionAccessibilityHelper }

  TdxRibbonTabCollectionAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetTabCollection: TdxRibbonTabCollection;
    function GetTabsViewInfo: TdxRibbonTabsViewInfo;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function GetDefaultAccessibleObject: IdxBarAccessibilityHelper; override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

//    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
//    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
//    function GetName(AChildID: TcxAccessibleSimpleChildElementID): string; override;
//    function GetRole(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;
//    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties; override;

    function LogicalNavigationGetChild(AIndex: Integer): TdxBarAccessibilityHelper; override;
    function LogicalNavigationGetChildCount: Integer; override;
    function LogicalNavigationGetChildIndex(AChild: TdxBarAccessibilityHelper): Integer; override;

    property TabCollection: TdxRibbonTabCollection read GetTabCollection;
    property TabsViewInfo: TdxRibbonTabsViewInfo read GetTabsViewInfo;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxRibbonTabAccessibilityHelper }

  TdxRibbonTabAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetRibbon: TdxCustomRibbon;
    function GetTab: TdxRibbonTab;
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function GetNextAccessibleObject(
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    function LogicalNavigationGetNextAccessibleObject(
      AShift: TShiftState): IdxBarAccessibilityHelper; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure KeyTipHandler(Sender: TObject); override;
    procedure KeyTipsEscapeHandler; override;

    property Ribbon: TdxCustomRibbon read GetRibbon;
    property Tab: TdxRibbonTab read GetTab;
  public
    procedure CloseUpHandler(AReason: TdxBarCloseUpReason);
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxRibbonApplicationButtonAccessibilityHelper }

  TdxRibbonApplicationButtonAccessibilityHelper = class(TdxBarAccessibilityHelper)
  private
    function GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
    function GetRibbon: TdxCustomRibbon;
    procedure ShowApplicationMenu(APostMessage: UINT);
  protected
    // IdxBarAccessibilityHelper
    function GetBarManager: TdxBarManager; override;
    function GetNextAccessibleObject(
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetOwnerObjectWindow: HWND; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetSelectable: Boolean; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure KeyTipHandler(Sender: TObject); override;

    property ApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo read GetApplicationButtonViewInfo;
    property Ribbon: TdxCustomRibbon read GetRibbon;
  public
    function GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect; override;
  end;

  { TdxRibbonGroupsDockControlAccessibilityHelper }

  TdxRibbonGroupsDockControlAccessibilityHelper = class(TdxDockControlAccessibilityHelper)
  private
    function GetDockControl: TdxRibbonGroupsDockControl;
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer; override;

    function GetParentForKeyTip: TdxBarAccessibilityHelper; override;

    property DockControl: TdxRibbonGroupsDockControl read GetDockControl;
  end;

  { TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper }

  TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper = class(TdxRibbonBarControlAccessibilityHelper)
  private
    function GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function MarkExists: Boolean;

    procedure DoGetKeyTipsData(AKeyTipsData: TList); override;
    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function GetParentForKeyTip: TdxBarAccessibilityHelper; override;
    function IsKeyTipContainer: Boolean; override;
    procedure KeyTipsEscapeHandler; override;

    property BarControl: TdxRibbonQuickAccessToolbarBarControl read GetBarControl;
  end;

  { TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper }

  TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper = class(TdxBarControlMarkAccessibilityHelper)
  private
    function GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
  protected
    // IdxBarAccessibilityHelper
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    function GetKeyTip: string; override;
    procedure KeyTipHandler(Sender: TObject); override;

    property BarControl: TdxRibbonQuickAccessToolbarBarControl read GetBarControl;
  end;

  { TdxRibbonGroupBarControlAccessibilityHelper }

  TdxRibbonGroupBarControlAccessibilityHelper = class(TdxRibbonBarControlAccessibilityHelper)
  private
    function GetBarControl: TdxRibbonGroupBarControl;
    procedure ShowPopupBarControl;
  protected
    // IdxBarAccessibilityHelper
    function GetNextAccessibleObject(ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    function IsNavigationKey(AKey: Word): Boolean; override;
    procedure Select(ASetFocus: Boolean); override;
    procedure Unselect(ANextSelectedObject: IdxBarAccessibilityHelper); override;

    function GetSelectable: Boolean; override;

    function Expand: TCustomdxBarControlAccessibilityHelper; override;
    procedure GetCaptionButtonKeyTipPosition(ACaptionButton: TdxBarCaptionButton;
      out ABasePointY: Integer; out AVertAlign: TcxAlignmentVert); override;
    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      var AKeyTipInfo: TdxBarKeyTipInfo); override;

    function GetAssignedKeyTip: string; override;
    function GetDefaultKeyTip: string; override;
    procedure GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure GetKeyTipData(AKeyTipsData: TList); override;

    function GetNextAccessibleObject(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
      ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    function GetParentForKeyTip: TdxBarAccessibilityHelper; override;
    function IsCollapsed: Boolean; override;
    function IsKeyTipContainer: Boolean; override;
    procedure KeyTipHandler(Sender: TObject); override;
    procedure KeyTipsEscapeHandler; override;

    property BarControl: TdxRibbonGroupBarControl read GetBarControl;
  public
    procedure CloseUpHandler(AReason: TdxBarCloseUpReason);
  end;

  { TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper }

  TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper = class(TdxBarButtonLikeControlAccessibilityHelper)
  protected
    function IsDropDownControl: Boolean; override;
    function ShowDropDownWindow: Boolean; override;
  end;

  { TdxRibbonKeyTipWindow }

  TdxRibbonKeyTipWindow = class(TCustomControl)
  strict private
    FColorScheme: TdxCustomRibbonSkin;
    FRibbon: TdxCustomRibbon;

    function GetScaled(AValue: Integer): Integer;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    function CalcBoundsRect: TRect;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Paint; override;
    procedure UpdateBounds;
  public
    constructor Create(AColorScheme: TdxCustomRibbonSkin); reintroduce; overload; virtual;
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce; overload; virtual;
    procedure ShowKeyTip;
    property Caption;
    property Enabled;
  end;

  { TdxRibbonCustomKeyTipWindows }

  TdxRibbonCustomKeyTipWindows = class(TInterfacedObject, IdxBarKeyTipWindowsManager)
  strict private
    FNotificator: TcxFreeNotificator;
    FRibbon: TdxCustomRibbon;
    FWindowList: TcxObjectList;

    procedure FreeNotificationHandler(Sender: TComponent);
    function GetColorScheme: TdxCustomRibbonSkin;
    function GetCount: Integer;
  protected
    // IdxBarKeyTipWindowsManager
    procedure Add(const ACaption: string; const ABasePoint: TPoint; AHorzAlign: TAlignment;
      AVertAlign: TcxAlignmentVert; AEnabled: Boolean; out AWindow: TObject);
    procedure Delete(AWindow: TObject);
    procedure Initialize; virtual;
    procedure Show;

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property Count: Integer read GetCount;
    property Ribbon: TdxCustomRibbon read FRibbon;
  public
    constructor Create(ARibbon: TdxCustomRibbon); reintroduce;
    destructor Destroy; override;
  end;

  { TdxRibbonKeyTipWindows }

  TdxRibbonKeyTipWindows = class(TdxRibbonCustomKeyTipWindows)
  protected
    procedure Initialize; override;
  end;

  { TdxRibbonScrollBar }

  TdxRibbonScrollBar = class(TcxControlScrollBar)
  protected
    function GetHelperClass: TcxScrollBarHelperClass; override;
    function GetRibbon: TdxCustomRibbon; virtual;
  public
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonScrollBarHelper }

  TdxRibbonScrollBarHelper = class(TcxControlScrollBarHelper)
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
  end;

  { TdxRibbonScrollBarPainter }

  TdxRibbonScrollBarPainter = class(TcxScrollBarPainter)
  private
    function GetScrollBar: TdxRibbonScrollBar;
    function GetSkin: IdxSkin;
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState); override;
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect); override;
  public
    property ScrollBar: TdxRibbonScrollBar read GetScrollBar;
    property Skin: IdxSkin read GetSkin;
  end;

  { TdxRibbonSizeGrip }

  TdxRibbonSizeGrip = class(TcxSizeGrip)
  protected
    procedure Draw(ACanvas: TCanvas); override;
    function GetRibbon: TdxCustomRibbon; virtual;
  public
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

var
  FShowRibbonCustomizationFormFunc: TdxRibbonShowCustomizationFormFunc = nil;

procedure RibbonCheckCreateComponent(var AOwner: TComponent; AClass: TClass);
procedure RibbonDockToolBar(AToolBar: TdxBar; ADockControl: TdxBarDockControl);
procedure RibbonUndockToolBar(AToolBar: TdxBar);
function FindRibbonForComponent(AComponent: TComponent): TdxCustomRibbon;

implementation

uses
  Math, Types, dxOffice11, CommCtrl, MultiMon,
  // Colors Schemes
  dxRibbonSkins2007,
  dxRibbonSkins2010,
  dxRibbonSkins2013,
  dxRibbonSkins2016,
  dxRibbonSkins2019,
  dxSkinsdxRibbonPainter,
  //
  dxBarStrs, dxBarSkinConsts, cxDrawTextUtils, dxHooks, cxDWMApi, dxUxTheme, dxThemeConsts, dxGDIPlusAPI,
  dxDPIAwareUtils, dxRibbonGroupLayoutCalculator;


const
  dxRibbonTabSeparatorWidth = 1;
  dxRibbonTabsRightSpace    = 6;
  dxRibbonTabsLeftSpace     = 8;
  dxRibbonTabsAreaBottomIndent = 2;

  dxRibbonCollapsedGroupGlyphBackgroundOffsets: TRect = (Left: 3; Top: 3; Right: 3; Bottom: 4);
  dxRibbonEmptyHeight = 24;
  dxRibbonGroupCaptionHeightCorrection = 1;
  dxRibbonGroupContentLeftOffset = 2;
  dxRibbonGroupContentRightOffset = 2;
  dxRibbonGroupRowHeightCorrection = 3;
  dxRibbonGroupSelectionFrameSize = 2;

  dxRibbonGroupsScrollDelta = 10;
  dxCaptionGlowRadius       = 10;
  dxExtraContextGap         = 2;

  dxRibbonBarBehaviorOptions: TdxBarBehaviorOptions = [bboAllowShowHints, bboClickItemsBySpaceKey,
    bboMouseCantUnselectNavigationItem, bboUnmoved, bboItemCustomizePopup, bboSubMenuCaptureMouse,
    bboCanShowPopupMenuOnMouseClick];

type
  PMouseHookStructEx = ^TMouseHookStructEx;
  TMouseHookStructEx = record
    pt: TPoint;
    hwnd: HWND;
    wHitTestCode: UINT;
    dwExtraInfo: TdxNativeUInt;
    mouseData: DWORD;
  end;

  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxBarAccess = class(TdxBar);
  TdxBarAccessibilityHelperAccess = class(TdxBarAccessibilityHelper);
  TdxBarCaptionButtonAccessibilityHelperAccess = class(TdxBarCaptionButtonAccessibilityHelper);
  TdxBarControlAccess = class(TdxBarControl);
  TdxBarControlViewInfoAccess = class(TdxBarControlViewInfo);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarItemControlAccessibilityHelperAccess = class(TdxBarItemControlAccessibilityHelper);
  TdxBarItemLinkAccess = class(TdxBarItemLink);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TdxBarManagerAccess = class(TdxBarManager);
  TdxBarSubMenuControlAccess = class(TdxBarSubMenuControl);
  TdxCustomRibbonSkinAccess = class(TdxCustomRibbonSkin);
  TdxRibbonAutoHideModeAccess = class(TdxRibbonAutoHideMode);

const
  sdxErrorToolbarUsedAsQAT = 'This toolbar is already used as the QuickAccessToolbar';
  sdxErrorToolbarUsedInAnotherGroupInThisTab = 'At least one group in this tab already contains this toolbar';

type

  { TdxRibbonGroupBarControlViewInfoHelper }

  TdxRibbonGroupBarControlViewInfoHelper = class(TInterfacedObject, IdxRibbonGroupViewInfo)
  strict private
    FViewInfo: TdxRibbonGroupBarControlViewInfo;

    // IdxRibbonGroupViewInfo
    procedure AddSeparator(const Value: TdxBarItemSeparatorInfo);
    procedure DeleteSeparators;
    function GetContentSize: TSize;
    function GetItemControlCount: Integer;
    function GetItemControlViewInfo(AIndex: Integer): IdxBarItemControlViewInfo;
    function GetMinContentWidth: Integer;
    function GetOffsetsInfo: TdxRibbonGroupOffsetsInfo;
    function GetSeparatorCount: Integer;
    function GetSeparatorInfo(AIndex: Integer): TdxBarItemSeparatorInfo;
    procedure SetContentSize(const Value: TSize);
    procedure SetSeparatorInfo(AIndex: Integer; const Value: TdxBarItemSeparatorInfo);
  protected
    property ViewInfo: TdxRibbonGroupBarControlViewInfo read FViewInfo;
  public
    constructor Create(AViewInfo: TdxRibbonGroupBarControlViewInfo);
  end;

  { TdxRibbonMergeHelper }

  TdxRibbonMergeHelper = class
  public
    class function IsMergedWith(ATestToolbar, ATargetToolbar: TdxBar): Boolean;
    class procedure MergeToolbarContainers(ATargetContainer: TObject; ASourceContainer: TObject; ACanCreateNewToolbar: Boolean);
    class procedure UnmergeToolbarContainers(ATargetContainer, ASourceContainer: TObject);
  end;

var
  FIsMouseHookInstalled: Boolean;
  FRibbonList: TList;

function dxRibbonList: TList;
begin
  Result := FRibbonList;
end;

function dxRibbonGetGroupCaption(ABar: TdxBar): string;
begin
  if ABar.Control is TdxRibbonGroupBarControl then
    Result := TdxRibbonGroupBarControl(ABar.Control).Group.Caption
  else
    Result := ABar.Caption;
end;

procedure dxRibbonAccessibilityEnumChildren(AIntf: IdxBarAccessibilityHelper; AProc: TdxRibbonAddAccessibilityHelperProc);
var
  AHelper: TdxBarAccessibilityHelper;
  I: Integer;
begin
  if AIntf <> nil then
  begin
    AHelper := AIntf.GetBarHelper;
    for I := 0 to AHelper.ChildCount - 1 do
      AProc(AHelper.Childs[I]);
  end;
end;

procedure dxFillRectOpaque(DC: HDC; const ABounds: TRect; AColor: TColor;
  const AText: string = ''; ATextColor: TColor = clBlack);
var
  B: TcxBitmap32;
begin
  if cxRectIsEmpty(ABounds) then Exit;
  B := TcxBitmap32.CreateSize(ABounds);
  try
    B.cxCanvas.FillRect(B.ClientRect, AColor);
    if Length(AText) > 0 then
    begin
      B.cxCanvas.Brush.Style := bsClear;
      B.cxCanvas.Font.Color := ATextColor;
      B.cxCanvas.DrawTexT(AText, B.ClientRect, cxAlignCenter or cxSingleLine);
      B.cxCanvas.Brush.Style := bsSolid;
    end;
    B.MakeOpaque;
    cxBitBlt(DC, B.cxCanvas.Handle, ABounds, cxNullPoint, SRCCOPY);
  finally
    B.Free;
  end;
end;

function HasComponentOnForm(AForm: TCustomForm; AClass: TClass): Boolean;
begin
  Result := cxFindComponent(AForm, AClass) <> nil;
end;

function CloseActiveRibbonApplicationMenus: Boolean;
var
  ARibbon: TdxCustomRibbon;
  I: Integer;
begin
  Result := False;
  for I := 0 to dxRibbonList.Count - 1 do
  begin
    ARibbon := TdxCustomRibbon(dxRibbonList[I]);
    if ARibbon.Controller.CloseApplicationMenu then
      Result := True;
  end;
end;

procedure RibbonCheckCreateComponent(var AOwner: TComponent; AClass: TClass);
begin
  if not CheckGdiPlus then
    raise EdxException.CreateFmt(cxGetResourceString(@dxSBAR_GDIPLUSNEEDED), [AClass.ClassName]);
  if (AOwner = nil) and (Application.MainForm <> nil) then
    AOwner := Application.MainForm;
  if not (AOwner is TCustomForm) then
    raise EdxException.CreateFmt(cxGetResourceString(@dxSBAR_RIBBONBADOWNER), [AClass.ClassName]);
  if HasComponentOnForm(TCustomForm(AOwner), AClass) then
    raise EdxException.CreateFmt(cxGetResourceString(@dxSBAR_RIBBONMORETHANONE), [AClass.ClassName]);
end;

function FindMouseWheelReceiver(AWnd: HWND; out AReceiver: IdxRibbonMouseWheelReceiver): Boolean;
var
  AControl: TWinControl;
begin
  AReceiver := nil;
  while AWnd <> 0 do
  begin
    AControl := FindControl(AWnd);
    if Supports(AControl, IdxRibbonMouseWheelReceiver, AReceiver) then
      Break;
    if not IsChildClassWindow(AWnd) then
      Break;
    AWnd := GetParent(AWnd);
  end;
  Result := Assigned(AReceiver);
end;

procedure dxRibbonMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AMHS: PMouseHookStructEx;
  AReceiver: IdxRibbonMouseWheelReceiver;
begin
  if (ACode < 0) or (wParam <> WM_MOUSEWHEEL) or not Mouse.WheelPresent then
    Exit;

  AMHS := PMouseHookStructEx(lParam);
  case BarGetMouseWheelReceiver of
    mwrActiveBarControl:
      begin
        SendMessage(ActiveBarControl.Handle, WM_MOUSEWHEEL,
          MakeWParam(ShiftStateToKeys(KeyboardStateToShiftState), HiWord(AMHS.mouseData)),
          MakeLParam(AMHS.pt.X, AMHS.pt.Y));
        AHookResult := 1;
      end;

    mwrWindow:
      if FindMouseWheelReceiver(WindowFromPoint(AMHS.pt), AReceiver) and AReceiver.CanProcessMouseWheel then
      begin
        if AReceiver.DoMouseWheel(KeyboardStateToShiftState, SmallInt(HiWord(AMHS.mouseData)), cxInvalidPoint) then
          AHookResult := 1;
      end
  end;
end;

procedure DrawRect(DC: HDC; const R: TRect; AColor: TColor; AExclude: Boolean);
begin
  FillRectByColor(DC, R, AColor);
  if AExclude then
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
end;

function GetMonitorWorkArea(AWnd: HWND): TRect;
var
  AMonitor: HMONITOR;
begin
  if AWnd <> 0 then
    AMonitor := MonitorFromWindow(AWnd, MONITOR_DEFAULTTONEAREST)
  else
    AMonitor := MonitorFromPoint(GetMouseCursorPos, MONITOR_DEFAULTTONEAREST);

  if AMonitor <> 0 then
    Result := cxControls.GetMonitorWorkArea(AMonitor)
  else
    Result := cxRectBounds(Screen.DesktopLeft, Screen.DesktopTop, Screen.DesktopWidth, Screen.DesktopHeight);
end;

procedure SelectFirstSelectableAccessibleObject(AParentObject: TdxBarAccessibilityHelper);
begin
  BarNavigationController.ChangeSelectedObject(False, AParentObject.GetFirstSelectableObject);
end;

//routines
procedure RibbonDockToolBar(AToolBar: TdxBar; ADockControl: TdxBarDockControl);
var
  APrevVisible: Boolean;
begin
  if (AToolBar <> nil) and (AToolBar.DockControl <> ADockControl) then
  begin
    if csLoading in AToolBar.ComponentState then
      AToolBar.DockControl := ADockControl
    else
    begin
      APrevVisible := AToolBar.Visible;
      try
        AToolBar.Visible := False;
        AToolBar.DockControl := ADockControl;
      finally
        AToolBar.Visible := APrevVisible;
      end;
    end;
  end;
end;

procedure RibbonUndockToolBar(AToolBar: TdxBar);

  procedure DoUndock;
  begin
    AToolbar.DockControl := nil;
    AToolbar.DockedDockControl := nil;
    AToolbar.DockedDockingStyle := dsNone;
    AToolbar.DockingStyle := dsNone;
  end;

var
  APrevVisible: Boolean;
begin
  if (AToolbar = nil) or (csDestroying in AToolbar.ComponentState) then
    Exit;

  if csLoading in AToolbar.ComponentState then
    DoUndock
  else
  begin
    APrevVisible := AToolbar.Visible;
    try
      AToolbar.Visible := False;
      DoUndock;
    finally
      AToolbar.Visible := APrevVisible;
    end;
  end;
end;

function FindRibbonForComponent(AComponent: TComponent): TdxCustomRibbon;

  function GetOwnerForm(AComponent: TComponent): TCustomForm;
  begin
    Result := nil;
    while (AComponent <> nil) and (Result = nil) do
    begin
      if AComponent is TCustomForm then
        Result := TCustomForm(AComponent);
      AComponent := AComponent.Owner;
    end;
  end;

var
  AForm: TCustomForm;
begin
  AForm := GetOwnerForm(AComponent);
  if AForm <> nil then
    Result := TdxCustomRibbon(cxFindComponent(AForm, TdxCustomRibbon))
  else
    Result := nil;
end;

function FindRibbonForWindow(AWnd: HWND): TdxCustomRibbon;
var
  AForm: TCustomForm;
begin
  AForm := dxBarGetControlForm(dxFindVCLControl(AWND), fkMainOrMDIChild);
  if AForm <> nil then
    Result := TdxCustomRibbon(cxFindComponent(AForm, TdxCustomRibbon))
  else
    Result := nil;
end;

function GetRootRibbonAccessibilityHelper(AParentWnd: HWND): IdxBarAccessibilityHelper;
var
  ARibbon: TdxCustomRibbon;
begin
  Result := nil;
  ARibbon := FindRibbonForWindow(AParentWnd);
  if (ARibbon <> nil) and ARibbon.Visible and
    (not ARibbon.Hidden or ARibbon.IsAutoHidden) and
    (ARibbon.ShowTabHeaders or ARibbon.ShowTabGroups) then
  begin
    if ARibbon.Controller.IsApplicationMenuDropped then
      Result := ARibbon.ApplicationButton.IMenu.GetRootAccessibilityHelper;
    if Result = nil then
      Result := ARibbon.IAccessibilityHelper;
  end;
end;

procedure RibbonRecalculateBar(ABar: TdxBar);
begin
  if ABar <> nil then
  begin
    if ABar.Control <> nil then
      ABar.Control.RepaintBar;
  end;
end;

procedure RaiseMergingError(AResString: Pointer); overload;
begin
  raise EdxException.Create(cxGetResourceString(AResString));
end;

procedure RaiseMergingError(AResString: Pointer; ATargetComponent, ASourceComponent: TComponent); overload;
begin
  raise EdxException.Create(Format(cxGetResourceString(AResString),
    [cxGetFullComponentName(ATargetComponent), cxGetFullComponentName(ASourceComponent)]));
end;

{ TdxRibbonMergeHelper }

class function TdxRibbonMergeHelper.IsMergedWith(ATestToolbar, ATargetToolbar: TdxBar): Boolean;
begin
  Result := (ATestToolbar <> nil) and (ATargetToolbar <> nil) and
    (TdxBarAccess(ATestToolbar).MergeData.MergedWith <> nil) and
    (TdxBarAccess(ATestToolbar).MergeData.MergedWith = ATargetToolbar);
end;

class procedure TdxRibbonMergeHelper.MergeToolbarContainers(
  ATargetContainer, ASourceContainer: TObject; ACanCreateNewToolbar: Boolean);
var
  ASourceToolbar: IdxRibbonToolbarContainer;
  ATargetToolbar: IdxRibbonToolbarContainer;
begin
  if (ATargetContainer = ASourceContainer) or
    not Supports(ATargetContainer, IdxRibbonToolbarContainer, ATargetToolbar) or
    not Supports(ASourceContainer, IdxRibbonToolbarContainer, ASourceToolbar)
  then
    RaiseMergingError(@dxSBAR_CANTMERGETOOLBAR);

  if not (ASourceToolbar.Ribbon.IsBarManagerValid and ATargetToolbar.Ribbon.IsBarManagerValid) then
    RaiseMergingError(@dxSBAR_RIBBONCANTMERGEWITHOUTBARMANAGER);

  if (ATargetToolbar.Toolbar = nil) and ACanCreateNewToolbar then
  begin
    ATargetToolbar.Toolbar := ATargetToolbar.Ribbon.CreateToolbarAsByMerging;
    ATargetToolbar.Toolbar.Caption := ASourceToolbar.Toolbar.Caption;
    ATargetToolbar.Toolbar.Visible := True;
  end;
  if ATargetToolbar.Toolbar <> nil then
    ATargetToolbar.Toolbar.Merge(ASourceToolbar.Toolbar);
end;

class procedure TdxRibbonMergeHelper.UnmergeToolbarContainers(
  ATargetContainer, ASourceContainer: TObject);

  function GetToolbar(AContainer: TObject): TdxBar;
  var
    AContainerIntf: IdxRibbonToolbarContainer;
  begin
    if Supports(AContainer, IdxRibbonToolbarContainer, AContainerIntf) then
      Result := AContainerIntf.Toolbar
    else
      Result := nil;
  end;

var
  ATargetToolbar: TdxBar;
begin
  ATargetToolbar := GetToolbar(ATargetContainer);
  if ATargetToolbar <> nil then
    ATargetToolbar.Unmerge(GetToolbar(ASourceContainer));
end;

{ TdxRibbonGroupBarControlViewInfoHelper }

constructor TdxRibbonGroupBarControlViewInfoHelper.Create(
  AViewInfo: TdxRibbonGroupBarControlViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

// IdxRibbonGroupViewInfo
procedure TdxRibbonGroupBarControlViewInfoHelper.AddSeparator(
  const Value: TdxBarItemSeparatorInfo);
begin
  ViewInfo.AddSeparatorInfo(Value.Bounds, Value.Kind, nil);
end;

procedure TdxRibbonGroupBarControlViewInfoHelper.DeleteSeparators;
begin
  ViewInfo.RemoveSeparatorInfos;
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetContentSize: TSize;
begin
  Result := ViewInfo.ContentSize;
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetItemControlCount: Integer;
begin
  Result := ViewInfo.ItemControlCount;
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetItemControlViewInfo(
  AIndex: Integer): IdxBarItemControlViewInfo;
begin
  Result := ViewInfo.ItemControlViewInfos[AIndex];
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetMinContentWidth: Integer;
var
  ABarControl: TdxRibbonGroupBarControl;
begin
  ABarControl := ViewInfo.BarControl;
  Result := ABarControl.Ribbon.GroupsPainter.GetGroupMinWidth(ABarControl);
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetOffsetsInfo: TdxRibbonGroupOffsetsInfo;
begin
  Result.ButtonGroupOffset := ViewInfo.BarControl.Ribbon.SkinGetPartSize(DXBAR_BUTTONGROUP);
  Result.ContentLeftOffset := dxRibbonGroupContentLeftOffset;
  Result.ContentRightOffset := dxRibbonGroupContentRightOffset;
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetSeparatorCount: Integer;
begin
  Result := ViewInfo.SeparatorCount;
end;

function TdxRibbonGroupBarControlViewInfoHelper.GetSeparatorInfo(
  AIndex: Integer): TdxBarItemSeparatorInfo;
begin
  Result := ViewInfo.SeparatorInfos[AIndex];
end;

procedure TdxRibbonGroupBarControlViewInfoHelper.SetContentSize(
  const Value: TSize);
begin
  ViewInfo.ContentSize := Value;
end;

procedure TdxRibbonGroupBarControlViewInfoHelper.SetSeparatorInfo(
  AIndex: Integer; const Value: TdxBarItemSeparatorInfo);
begin
  ViewInfo.SeparatorInfos[AIndex] := Value;
end;

{ TdxRibbonCustomMergeData }

constructor TdxRibbonCustomMergeData.Create;
begin
  inherited Create;
  FChildren := TcxComponentList.Create(False);
  FChildren.OnNotify := ListChangeHandler;
end;

destructor TdxRibbonCustomMergeData.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxRibbonCustomMergeData.FreeNotification(Sender: TComponent);
begin
  Children.Remove(Sender);
end;

procedure TdxRibbonCustomMergeData.ListChangeHandler(
  Sender: TObject; AComponent: TComponent; AAction: TListNotification);
begin
  case AAction of
    lnAdded:
      FreeNotificator.AddSender(AComponent);
    lnExtracted, lnDeleted:
      FreeNotificator.RemoveSender(AComponent);
  end;
end;

{ TdxCustomDesignSelectionHelper }

constructor TdxCustomDesignSelectionHelper.Create(
  AComponent: TComponent; AOwner, AParent: TPersistent);
begin
  inherited Create;
  FComponent := AComponent;
  FParent := AParent;
  FOwner := AOwner;
end;

//IdxBarSelectableItem
function TdxCustomDesignSelectionHelper.CanDelete(ADestruction: Boolean): Boolean;
begin
  if (Owner is TComponent) and (BarManager <> nil) then
    Result := IdxBarDesigner(BarManager).CanDeleteComponent(TComponent(Owner))
  else
    Result := True;
end;

procedure TdxCustomDesignSelectionHelper.DeleteSelection(
  var AReference: IdxBarSelectableItem; ADestruction: Boolean);
begin
  if CanDelete(ADestruction) then
  begin
    AReference := nil;
    Owner.Free;
  end;
end;

procedure TdxCustomDesignSelectionHelper.ExecuteCustomizationAction(ABasicAction: TdxBarCustomizationAction);
begin
// do nothing;
end;

function TdxCustomDesignSelectionHelper.GetBarManager: TdxBarManager;
begin
  Result := nil;
end;

function TdxCustomDesignSelectionHelper.GetInstance: TPersistent;
begin
  Result := Owner;
end;

procedure TdxCustomDesignSelectionHelper.GetMasterObjects(AList: TdxObjectList);
begin
  AList.Add(Parent);
end;

function TdxCustomDesignSelectionHelper.GetNextSelectableItem: IdxBarSelectableItem;
begin
  Result := nil;
end;

function TdxCustomDesignSelectionHelper.GetSelectableParent: TPersistent;
begin
  Result := Parent;
end;

function TdxCustomDesignSelectionHelper.GetSelectionStatus: TdxBarSelectionStatus;
begin
  if BarManager <> nil then
    Result := (BarManager as IdxBarDesigner).GetSelectionStatus(Owner)
  else
    Result := ssUnselected;
end;

function TdxCustomDesignSelectionHelper.GetSupportedActions: TdxBarCustomizationActions;
begin
  Result := [];
end;

procedure TdxCustomDesignSelectionHelper.Invalidate;
begin
end;

function TdxCustomDesignSelectionHelper.IsComplex: Boolean;
begin
  Result := False;
end;

function TdxCustomDesignSelectionHelper.IsComponentSelected: Boolean;
begin
  Result := (BarManager <> nil) and (BarManager as IdxBarDesigner).IsComponentSelected(Owner);
end;

procedure TdxCustomDesignSelectionHelper.SelectComponent(ASelectionOperation: TdxBarSelectionOperation);
begin
  if BarManager <> nil then
    (BarManager as IdxBarDesigner).SelectComponent(Owner, ASelectionOperation);
end;

procedure TdxCustomDesignSelectionHelper.SelectionChanged;
begin
  Invalidate;
end;

function TdxCustomDesignSelectionHelper.SelectParentComponent: Boolean;
begin
  Result := True;
  if BarManager <> nil then
    (BarManager as IdxBarDesigner).SelectComponent(GetSelectableParent);
end;

{ TdxDesignSelectionHelper }

function TdxDesignSelectionHelper.GetBarManager: TdxBarManager;
begin
  Result := Ribbon.BarManager;
end;

function TdxDesignSelectionHelper.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxCustomRibbon(Component);
end;

procedure TdxDesignSelectionHelper.Invalidate;
begin
  Ribbon.FullInvalidate;
end;

{ TdxRibbonPainter }

constructor TdxRibbonPainter.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FFormIconCache := TcxObjectList.Create;
end;

destructor TdxRibbonPainter.Destroy;
begin
  FreeAndNil(FFormIconCache);
  inherited;
end;

procedure TdxRibbonPainter.AdjustContextFont(AFont: TFont; AContextColor: TColor);
begin
  ColorScheme.AdjustContextFont(AFont, Ribbon.ViewInfo.UseGlass, AContextColor);
end;

procedure TdxRibbonPainter.FlushCache;
begin
  FFormIconCache.Clear;
end;

procedure TdxRibbonPainter.DrawApplicationButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TdxRibbonApplicationButtonState);
begin
  if not IsRectEmpty(ABounds) then
  begin
    ColorScheme.UseRightToLeftAlignment := Ribbon.UseRightToLeftAlignment;
    ColorScheme.DrawApplicationButton(ACanvas.Handle, ABounds, AState);
    ColorScheme.UseRightToLeftAlignment := False;
  end;
end;

procedure TdxRibbonPainter.DrawApplicationButtonGlyph(
  ACanvas: TcxCanvas; const ABounds: TRect; AGlyph: TdxSmartGlyph);
begin
  if (AGlyph = nil) or AGlyph.Empty then
    DrawDefaultFormIcon(ACanvas, ABounds)
  else
    DrawGlyph(ACanvas, AGlyph, ABounds);
end;

procedure TdxRibbonPainter.DrawDefaultFormIcon(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if not cxRectIsEmpty(ABounds) then
    GetDefaultFormIcon(cxSize(ABounds)).StretchDraw(ACanvas.Handle, ABounds);
end;

function TdxRibbonPainter.GetDefaultFormIcon(const ASize: TSize): TdxGPImage;

  function CreateImage(const ASize: TSize; AIconHandle: HICON): TdxGPImage;
  var
    AImageList: TImageList;
  begin
    if AIconHandle = 0 then
      Result := TdxGPImage.CreateSize(ASize)
    else
    begin
      AImageList := TImageList.CreateSize(ASize.cx, ASize.cy);
      try
        ImageList_AddIcon(AImageList.Handle, AIconHandle);
        Result := TdxGPImage.Create;
        TdxImageListPaintCache.PrepareImage(Result, ASize,
          TdxDrawImageCacheID.Create(nil, AImageList, 0, idmNormal, True, False, clNone, nil), nil);
      finally
        AImageList.Free;
      end;
    end;
  end;

  function FindIconInCache(const ASize: TSize): TdxGPImage;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to FFormIconCache.Count - 1 do
      if cxSizeIsEqual(ASize, TdxGPImage(FFormIconCache.List[I]).Size) then
        Exit(TdxGPImage(FFormIconCache.List[I]));
  end;

begin
  Result := FindIconInCache(ASize);
  if Result = nil then
  begin
    Result := CreateImage(ASize, GetDefaultFormIconHandle);
    if FFormIconCache.Count > MaxFormIconCacheSize then
      FFormIconCache.FreeAndDelete(0);
    FFormIconCache.Add(Result);
  end;
end;

function TdxRibbonPainter.GetDefaultFormIconHandle: HICON;
var
  AForm: TCustomForm;
begin
  Result := 0;

  AForm := GetParentForm(Ribbon);
  if AForm <> nil then
  begin
    Result := SendMessage(AForm.Handle, WM_GETICON, ICON_SMALL, 0);
    if Result = 0 then
      Result := SendMessage(AForm.Handle, WM_GETICON, ICON_BIG, 0);
  end;

  if Result = 0 then
    Result := Application.Icon.Handle;
end;

procedure TdxRibbonPainter.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect);
var
  R: TRect;
begin
  R := ABounds;
  if QuickAccessToolbarViewInfo.Visible and not (QuickAccessToolbarViewInfo.AtBottom or ViewInfo.SupportNonClientDrawing) then
  begin
    R.Bottom := QuickAccessToolbarViewInfo.Bounds.Bottom;
    ColorScheme.DrawRibbonClientTopArea(ACanvas.Handle, R);
    R := ABounds;
    R.Top := QuickAccessToolbarViewInfo.Bounds.Bottom;
  end;
  ColorScheme.DrawRibbonBackground(ACanvas.Handle, R);
end;

procedure TdxRibbonPainter.DrawBackgroundImage(ACanvas: TcxCanvas; const ABounds: TRect);
var
  R: TRect;
  AIsRightToLeft: Boolean;
begin
  if not cxRectIsEmpty(ABounds) then
  begin
    R := ABounds;
    AIsRightToLeft := Ribbon.UseRightToLeftAlignment;
    cxRightToLeftDependentDraw(ACanvas.Handle, R, AIsRightToLeft,
      procedure
      begin
        Ribbon.BackgroundImage.StretchDraw(ACanvas.Handle, R);
      end);
  end;
end;

procedure TdxRibbonPainter.DrawContextGroupsArea(
  ACanvas: TcxCanvas; const ABounds: TRect; AIsQuickAccessToolbarAtBottom, AIsInPopup: Boolean);
var
  AClipRegion: TcxRegion;
  B: TcxBitmap;
begin
  B := TcxBitmap.CreateSize(ABounds, pf32bit);
  try
    AClipRegion := ACanvas.GetClipRegion;
    AClipRegion.Offset(-ABounds.Left, -ABounds.Top);
    B.cxCanvas.SetClipRegion(AClipRegion, roSet);
    B.cxCanvas.FillRect(B.ClientRect, Ribbon.ActiveTab.Context.Color);
    ColorScheme.DrawContextTabGroupsArea(B.cxCanvas.Handle, B.ClientRect,
      Ribbon.ActiveTab.Context.Color, AIsQuickAccessToolbarAtBottom, AIsInPopup);
    cxBitBlt(ACanvas.Handle, B.cxCanvas.Handle, ABounds, cxNullPoint, SRCCOPY);
  finally
    B.Free;
  end;
end;

procedure TdxRibbonPainter.DrawGlowingText(DC: HDC; const AText: string;
  AFont: TFont; const ABounds: TRect; AColor: TColor; AFlags: DWORD;
  ATransparent: Boolean = False);
begin
  dxDrawTextOnGlass(DC, AText, AFont, ABounds,
    AColor, AFlags, dxCaptionGlowRadius, ATransparent);
end;

procedure TdxRibbonPainter.DrawGlyph(ACanvas: TcxCanvas; AGlyph: TdxSmartGlyph; const ABounds: TRect);
begin
  if not cxRectIsEmpty(ABounds) then
    cxDrawImage(ACanvas.Handle, ABounds, ABounds, AGlyph, nil, -1, idmNormal, True);
end;

procedure TdxRibbonPainter.DrawGroupsArea(ACanvas: TcxCanvas; const ABounds: TRect;
  AIsAllowContextPaint: Boolean = True; AIsInPopup: Boolean = False);
begin
  if AIsAllowContextPaint and ViewInfo.HasActiveContextTab then
    DrawContextGroupsArea(ACanvas, ABounds, QuickAccessToolbarViewInfo.AtBottom, AIsInPopup)
  else
    ColorScheme.DrawTabGroupsArea(ACanvas.Handle, ABounds, QuickAccessToolbarViewInfo.AtBottom, AIsInPopup);
end;

procedure TdxRibbonPainter.DrawGroupsScrollButton(ACanvas: TcxCanvas;
  const ABounds: TRect; ALeft: Boolean; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer = (
    DXBAR_NORMAL, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_NORMAL
  );
begin
  ColorScheme.DrawGroupScrollButton(ACanvas.Handle, ABounds, ALeft, StateMap[AState]);
end;

procedure TdxRibbonPainter.DrawGroupsScrollButtonArrow(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean);
begin
  ColorScheme.DrawGroupScrollButtonGlyph(ACanvas.Handle, ABounds, ALeft);
end;

procedure TdxRibbonPainter.DrawTabAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect);
var
  ARect: TRect;
begin
  if not IsRectEmpty(ABounds) then
  begin
    ColorScheme.DrawTabAreaBackground(ACanvas.Handle, ABounds, ViewInfo.IsFormCaptionActive or
      not ViewInfo.SupportNonClientDrawing, ViewInfo.IsTabsOnGlass, Ribbon.ApplicationMenuState);

    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ABounds);
      ARect := ABounds;
      ARect.Top := 0;
      ColorScheme.DrawRibbonTopFrameArea(ACanvas.Handle, ARect, ViewInfo.IsTabsOnGlass);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxRibbonPainter.DrawTabBase(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  ColorScheme.DrawTabBase(ACanvas.Handle, ABounds);
end;

procedure TdxRibbonPainter.DrawTabScrollButton(ACanvas: TcxCanvas;
  const ABounds: TRect; ALeft: Boolean; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer = (
    DXBAR_NORMAL, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_NORMAL
  );
begin
  ColorScheme.DrawTabScrollButton(ACanvas.Handle, ABounds, ALeft, StateMap[AState]);
end;

procedure TdxRibbonPainter.DrawTabScrollButtonGlyph(ACanvas: TcxCanvas; const ABounds: TRect; ALeft: Boolean);
begin
  ColorScheme.DrawTabScrollButtonGlyph(ACanvas.Handle, ABounds, ALeft);
end;

procedure TdxRibbonPainter.DrawHelpButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState);
begin
  ColorScheme.DrawHelpButton(ACanvas.Handle, ABounds, AState);
end;

procedure TdxRibbonPainter.DrawHelpButtonGlyph(ACanvas: TcxCanvas;
  AGlyph: TdxSmartGlyph; const ABounds: TRect; AState: TcxButtonState);
begin
  if (AGlyph = nil) or AGlyph.Empty then
    ColorScheme.DrawHelpButtonGlyph(ACanvas.Handle, ABounds)
  else
    DrawGlyph(ACanvas, AGlyph, ABounds);
end;

procedure TdxRibbonPainter.DrawMDIButton(ACanvas: TcxCanvas;
  const ABounds: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
begin
  ColorScheme.DrawMDIButton(ACanvas.Handle, ABounds, AButton, AState);
end;

procedure TdxRibbonPainter.DrawMDIButtonGlyph(ACanvas: TcxCanvas;
  const ABounds: TRect; AButton: TdxBarMDIButton; AState: TcxButtonState);
begin
  ColorScheme.DrawMDIButtonGlyph(ACanvas.Handle, ABounds, AButton, AState);
end;

procedure TdxRibbonPainter.DrawMinimizeButton(ACanvas: TcxCanvas;
  const ABounds: TRect; AState: TcxButtonState; AMinimized: Boolean);
begin
  ColorScheme.DrawMinimizeRibbonButton(ACanvas.Handle, ABounds, AState, AMinimized);
end;

procedure TdxRibbonPainter.DrawMinimizeButtonGlyph(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AGlyph: TdxRibbonMinimizeButtonGlyph);
begin
  ColorScheme.DrawMinimizeRibbonButtonGlyph(ACanvas.Handle, R, AState, AGlyph);
end;

function TdxRibbonPainter.GetTabContentOffset: TRect;
begin
  Result := Ribbon.ScaleFactor.Apply(cxRect(dxRibbonTabTextOffset, 7, dxRibbonTabTextOffset, 4));
  Ribbon.TouchModeHelper.AdjustMargins(Result, 9);
end;

procedure TdxRibbonPainter.DrawRibbonFormCaptionText(ACanvas: TcxCanvas;
  const ABounds: TRect; const ADocumentName, ACaption: string);
var
  R: TRect;
  AFlags: Cardinal;
begin
  if cxRectIsEmpty(ABounds) then Exit;
  ACanvas.Font := ViewInfo.GetFormCaptionFont(GetRibbonFormPaintData.GetIsActive);
  R := ABounds;
  if UseAeroNCPaint(GetRibbonFormPaintData) then
    DrawRibbonGlassFormCaptionText(ACanvas, ABounds, ADocumentName, ACaption, True)
  else
  begin
    AFlags := CXTO_PREVENT_LEFT_EXCEED or CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or
      CXTO_END_ELLIPSIS;
    if Ribbon.UseRightToLeftReading then
      AFlags := AFlags or CXTO_RTLREADING;
    cxTextOut(ACanvas.Handle, ADocumentName + ACaption, R, AFlags, 0, Length(ADocumentName), nil, clNone,
      ViewInfo.GetDocumentNameTextColor(GetRibbonFormPaintData.GetIsActive));
  end;
end;

procedure TdxRibbonPainter.DrawRibbonGlassFormCaptionText(ACanvas: TcxCanvas;
  const ABounds: TRect; const ADocumentName, ACaption: string; AIsActive: Boolean);
var
  R: TRect;
  S: string;
  ANeedClipping: Boolean;
  AFlags: Cardinal;
begin
  if IsFormZoomed and not IsWinSevenOrLater then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clWhite;
    AFlags := cxAlignVCenter or cxSingleLine or cxShowEndEllipsis;
    if Ribbon.UseRightToLeftAlignment then
      AFlags := AFlags or cxAlignRight
    else
      AFlags := AFlags or cxAlignLeft;
    if Ribbon.UseRightToLeftReading then
      AFlags := AFlags or cxRtlReading;
    ACanvas.DrawTexT(ADocumentName + ACaption, ABounds, AFlags);
    ACanvas.Brush.Style := bsSolid;
  end
  else
  begin
    R := ABounds;
    S := cxGetStringAdjustedToWidth(ACanvas.Handle, ACanvas.Font.Handle,
      ADocumentName + ACaption, cxRectWidth(R) - 2 * dxCaptionGlowRadius);
    if Ribbon.UseRightToLeftAlignment then
      R.Left := R.Right - cxTextWidth(ACanvas.Font, S) - 2 * dxCaptionGlowRadius
    else
      R.Right := R.Left + cxTextWidth(ACanvas.Font, S) + 2 * dxCaptionGlowRadius;
    ANeedClipping := ABounds.Right < R.Right;
    if ANeedClipping then
    begin
      ACanvas.SaveClipRegion;
      ACanvas.IntersectClipRect(ABounds);
    end;
    AFlags := DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;
    if Ribbon.UseRightToLeftReading then
      AFlags := AFlags or DT_RTLREADING;
    DrawGlowingText(ACanvas.Handle, S, ACanvas.Font, R, ACanvas.Font.Color, AFlags, True);
    if ANeedClipping then
      ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxRibbonPainter.DrawRibbonFormCaption(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  ColorScheme.InitializePaintData(Self);
  if not UseAeroNCPaint(GetRibbonFormPaintData) then
    ColorScheme.DrawFormCaption(ACanvas.Handle, ABounds);
  if not Ribbon.Hidden and (GetRibbonFormPaintData.GetState <> wsMinimized) then
  begin
    if Ribbon.ShowTabHeaders then
      DrawRibbonFormCaptionFrameArea(ACanvas, ABounds);
    if Ribbon.ApplicationMenuState <> ramsShownAsFullScreenFrame then
    begin
      if QuickAccessToolbarViewInfo.AtNonClientArea then
        QuickAccessToolbarViewInfo.Draw(ACanvas);
      ViewInfo.ApplicationButtonViewInfo.Draw(ACanvas);
      if ViewInfo.IsContextsVisible then
        ViewInfo.ContextsViewInfo.Draw(ACanvas);
    end;
  end;
  if ViewInfo.CanDrawDefaultFormIcon then
    DrawDefaultFormIcon(ACanvas, Ribbon.FormCaptionHelper.SysMenuIconBounds);
  DrawRibbonFormCaptionText(ACanvas, ViewInfo.FormCaptionBounds, ViewInfo.DocumentName, ViewInfo.Caption);
end;

procedure TdxRibbonPainter.DrawRibbonFormCaptionFrameArea(ACanvas: TcxCanvas; R: TRect);
var
  ABorderWidths: TRect;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    ABorderWidths := Ribbon.GetWindowBordersWidth;
    Dec(R.Right, ABorderWidths.Right);
    Inc(R.Left, ABorderWidths.Left);
    Inc(R.Bottom, ViewInfo.TabsAreaViewInfo.MeasuredHeight);
    ColorScheme.DrawRibbonTopFrameArea(ACanvas.Handle, R, ViewInfo.IsTabsOnGlass);
    ACanvas.IntersectClipRect(ViewInfo.BackgroundImageClipBounds);
    DrawBackgroundImage(ACanvas, ViewInfo.BackgroundImageBounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxRibbonPainter.DrawRibbonTopFrameAreaSeparator(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := ViewInfo.Bounds;
  R.Top := ViewInfo.TabsAreaViewInfo.Bounds.Bottom - 1;
  R.Bottom := R.Top + ColorScheme.GetRibbonTopFrameAreaSeparatorSize;
  ColorScheme.DrawRibbonTopFrameAreaSeparator(ACanvas.Handle, R);
end;

procedure TdxRibbonPainter.DrawRibbonFormBorderIcon(ACanvas: TcxCanvas; const ABounds: TRect;
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
begin
  ColorScheme.InitializePaintData(Self);
  ColorScheme.DrawFormBorderIcon(ACanvas.Handle, ABounds, AIcon, AState);
end;

procedure TdxRibbonPainter.DrawRibbonFormBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect);
begin
  ColorScheme.InitializePaintData(Self);
  ColorScheme.UseRightToLeftAlignment := Ribbon.UseRightToLeftAlignment;
  ColorScheme.DrawFormBorders(ACanvas.Handle, ABordersWidth);
  ColorScheme.UseRightToLeftAlignment := False;
end;

procedure TdxRibbonPainter.DrawEmptyRibbon(ACanvas: TcxCanvas);
var
  ABrush: HBRUSH;
  APrevBkColor: TColor;
begin
  APrevBkColor := GetBkColor(ACanvas.Handle);
  SetBkColor(ACanvas.Handle, clSilver);
  ABrush := CreateHatchBrush(HS_BDIAGONAL, clBlack);
  FillRect(ACanvas.Handle, ViewInfo.Bounds, ABrush);
  DeleteObject(ABrush);
  SetBkColor(ACanvas.Handle, APrevBkColor);
end;

function TdxRibbonPainter.GetApplicationMenuState: TdxRibbonApplicationMenuState;
begin
  Result := Ribbon.ApplicationMenuState;
end;

function TdxRibbonPainter.GetCaptionAreaExtension: Integer;
begin
  Result := Ribbon.GetRibbonFormCaptionAreaExtension;
end;

function TdxRibbonPainter.GetCaptionHeight: Integer;
begin
  Result := Ribbon.GetRibbonFormCaptionHeight;
end;

function TdxRibbonPainter.GetRibbonFormPaintData: IdxRibbonFormPaintData;
begin
  Supports(Ribbon.RibbonForm, IdxRibbonFormPaintData, Result);
end;

function TdxRibbonPainter.GetRibbonHeight: Integer;
begin
  Result := Ribbon.Height;
end;

function TdxRibbonPainter.GetTabsHeight: Integer;
begin
  Result := ViewInfo.TabsAreaViewInfo.MeasuredHeight;
end;

function TdxRibbonPainter.HasStatusBar: Boolean;
begin
  Result := Ribbon.HasStatusBar;
end;

function TdxRibbonPainter.IsRibbonHidden: Boolean;
begin
  Result := Ribbon.Hidden;
end;

function TdxRibbonPainter.IsQuickAccessToolbarBelowRibbon: Boolean;
begin
  Result := QuickAccessToolbarViewInfo.AtBottom;
end;

function TdxRibbonPainter.GetGroupCaptionBottomOffset(AInPopup: Boolean): Integer;
begin
  Result := Ribbon.SkinGetPartSize(IfThen(AInPopup, rspTabGroupInPopupBottomOffset, rspTabGroupBottomOffset));
end;

function TdxRibbonPainter.GetIsFormZoomed: Boolean;
var
  F: TCustomForm;
begin
  F := Ribbon.RibbonForm;
  Result := (F <> nil) and F.HandleAllocated and IsZoomed(F.Handle);
end;

function TdxRibbonPainter.GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
begin
  Result := Ribbon.ViewInfo.QuickAccessToolbarViewInfo;
end;

function TdxRibbonPainter.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Ribbon.ColorScheme;
end;

function TdxRibbonPainter.GetColorSchemeAccent: TdxRibbonColorSchemeAccent;
begin
  Result := Ribbon.ColorSchemeAccent;
end;

function TdxRibbonPainter.GetViewInfo: TdxRibbonViewInfo;
begin
  Result := Ribbon.ViewInfo;
end;

{ TdxRibbonHitInfo }

constructor TdxRibbonHitInfo.Create(AOwner: TdxCustomRibbon);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxRibbonHitInfo.GetHitObjectAsButton: TdxRibbonCustomButtonViewInfo;
begin
  if HitObject is TdxRibbonCustomButtonViewInfo then
    Result := TdxRibbonCustomButtonViewInfo(HitObject)
  else
    Result := nil;
end;

function TdxRibbonHitInfo.GetHitObjectAsContext: TdxRibbonContext;
begin
  if HitObject is TdxRibbonContext then
    Result := TdxRibbonContext(HitObject)
  else
    Result := nil;
end;

function TdxRibbonHitInfo.GetHitObjectAsTab: TdxRibbonTab;
begin
  if HitObject is TdxRibbonTab then
    Result := TdxRibbonTab(HitObject)
  else
    Result := nil;
end;

procedure TdxRibbonHitInfo.Calculate(const P: TPoint);
begin
  Reset;
  FHitPoint := P;
  Owner.ViewInfo.CalculateHitInfo(Self);
end;

function TdxRibbonHitInfo.Compare(const AHitTest: TdxRibbonHitInfo): Boolean;
begin
  Result := (HitTest = AHitTest.HitTest) and (HitObject = AHitTest.HitObject);
end;

procedure TdxRibbonHitInfo.Reset;
begin
  FHitTest := rhtNone;
  FHitObject := nil;
  FHitPoint := cxInvalidPoint;
end;

{ TdxRibbonCustomViewInfo }

constructor TdxRibbonCustomViewInfo.Create(AOwner: TdxRibbonViewInfo);
begin
  inherited Create;
{$IFDEF DEBUG}
  dxTestCheck(AOwner <> nil, '');
{$ENDIF}
  FOwner := AOwner;
end;

procedure TdxRibbonCustomViewInfo.Calculate(const ABounds: TRect);
begin
  FIsRightToLeftConverted := False;
  FBounds := ABounds;
end;

procedure TdxRibbonCustomViewInfo.CalculateMetrics(ACanvas: TcxCanvas);
begin
  // do nothing
end;

function TdxRibbonCustomViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := False;
end;

procedure TdxRibbonCustomViewInfo.Invalidate;
begin
  InvalidateRect(Bounds);
end;

procedure TdxRibbonCustomViewInfo.InvalidateRect(const R: TRect);
begin
  Ribbon.InvalidateRect(R);
end;

procedure TdxRibbonCustomViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  // do nothing
end;

function TdxRibbonCustomViewInfo.GetRibbon: TdxCustomRibbon;
begin
  Result := Owner.Ribbon;
end;

function TdxRibbonCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ribbon.ScaleFactor;
end;

procedure TdxRibbonCustomViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxRibbonCustomViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
end;

{ TdxRibbonCustomContainerViewInfo }

constructor TdxRibbonCustomContainerViewInfo.Create(AOwner: TdxRibbonViewInfo);
begin
  inherited Create(AOwner);
  FCells := TcxObjectList.Create;
end;

destructor TdxRibbonCustomContainerViewInfo.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

function TdxRibbonCustomContainerViewInfo.AddItem(AObject: TObject): Integer;
begin
  Result := FCells.Add(AObject);
end;

procedure TdxRibbonCustomContainerViewInfo.Clear;
begin
  FCells.Clear;
end;

function TdxRibbonCustomContainerViewInfo.GetCount: Integer;
begin
  Result := FCells.Count;
end;

function TdxRibbonCustomContainerViewInfo.GetItem(Index: Integer): TObject;
begin
  Result := FCells[Index];
end;

{ TdxRibbonCustomButtonAccessibilityHelper }

constructor TdxRibbonCustomButtonAccessibilityHelper.CreateEx(
  AButtonViewInfo: TdxRibbonCustomButtonViewInfo);
begin
  Create(AButtonViewInfo.Ribbon);
  FButtonViewInfo := AButtonViewInfo;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Ribbon.BarManager;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetNextAccessibleObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
begin
  Result := nil;
  case ADirection of
    andLeft, andRight:
      Result := Ribbon.GetNextHorizontalAccessibleObject(Self, ADirection);
    andUp:
      Result := Self;
  end;
  if Result = nil then
    Result := inherited GetNextAccessibleObject(ADirection);
end;

function TdxRibbonCustomButtonAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if not Result then
  begin
    Result := AKey in [VK_SPACE, VK_RETURN];
    if Result then
    begin
      Unselect(nil);
      ButtonViewInfo.Click;
    end;
  end;
end;

function TdxRibbonCustomButtonAccessibilityHelper.IsNavigationKey(AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey) or (AKey in [VK_ESCAPE, VK_SPACE, VK_RETURN]);
end;

procedure TdxRibbonCustomButtonAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  ButtonViewInfo.Invalidate;
end;

procedure TdxRibbonCustomButtonAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  ButtonViewInfo.Invalidate;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := '';
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetDefaultKeyTip: string;
begin
  Result := '';
end;

procedure TdxRibbonCustomButtonAccessibilityHelper.GetKeyTipData(AKeyTipsData: TList);
begin
  //nothing
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Ribbon.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := ButtonViewInfo.Bounds;
    Result := cxRectOffset(Result, Ribbon.ClientToScreen(Result.TopLeft));
  end
  else
    Result := cxEmptyRect;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
end;

function TdxRibbonCustomButtonAccessibilityHelper.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxCustomRibbon(FOwnerObject);
end;

{ TdxRibbonCustomButtonViewInfo }

constructor TdxRibbonCustomButtonViewInfo.Create(AOwner: TdxRibbonCustomContainerViewInfo);
begin
  inherited Create;
  FState := cxbsNormal;
  FOwner := AOwner;
end;

destructor TdxRibbonCustomButtonViewInfo.Destroy;
begin
  dxFader.Remove(Self);
  BarAccessibilityHelperOwnerObjectDestroyed(FAccessibilityHelper);
  inherited Destroy;
end;

function TdxRibbonCustomButtonViewInfo.CanFade: Boolean;
begin
  Result := Assigned(Ribbon) and Ribbon.CanFade;
end;

procedure TdxRibbonCustomButtonViewInfo.CalculateState;
begin
  if not Enabled then
    State := cxbsDisabled
  else
    if Ribbon.Controller.PressedButton = Self then
      State := cxbsPressed
    else
      if Ribbon.Controller.HotButton = Self then
        State := cxbsHot
      else
        State := cxbsNormal;
end;

procedure TdxRibbonCustomButtonViewInfo.Click;
begin
end;

function TdxRibbonCustomButtonViewInfo.CreateAccessibilityHelper: TdxRibbonCustomButtonAccessibilityHelper;
begin
  Result := TdxRibbonCustomButtonAccessibilityHelper.CreateEx(Self);
end;

function TdxRibbonCustomButtonViewInfo.GetIsEnabled: Boolean;
begin
  Result := Ribbon.ApplicationMenuState <= ramsShownAsMenu;
end;

procedure TdxRibbonCustomButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if not cxRectIsEmpty(Bounds) and ACanvas.RectVisible(Bounds) then
  begin
    if not dxFader.DrawFadeImage(Self, ACanvas.Handle, Bounds) then
      DrawButton(ACanvas, Bounds, State);
    DrawButtonGlyph(ACanvas, Bounds, State);
  end;
end;

procedure TdxRibbonCustomButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
end;

procedure TdxRibbonCustomButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
end;

procedure TdxRibbonCustomButtonViewInfo.DrawFadeImage;
begin
  Invalidate;
end;

procedure TdxRibbonCustomButtonViewInfo.Invalidate;
begin
  Owner.InvalidateRect(Bounds);
end;

function TdxRibbonCustomButtonViewInfo.GetAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if FAccessibilityHelper = nil then
    FAccessibilityHelper := CreateAccessibilityHelper;
  Result := FAccessibilityHelper;
end;

function TdxRibbonCustomButtonViewInfo.GetFadingOptions: TdxFadingOptions;
begin
  Result := Ribbon.OptionsFading.BarItems;
end;

procedure TdxRibbonCustomButtonViewInfo.GetFadingImages(
  out AFadeOutImage, AFadeInImage: TcxBitmap);
begin
  AFadeInImage := TcxBitmap32.CreateSize(Bounds, True);
  AFadeOutImage := TcxBitmap32.CreateSize(Bounds, True);
  DrawButton(AFadeInImage.cxCanvas, AFadeInImage.ClientRect, cxbsHot);
  DrawButton(AFadeOutImage.cxCanvas, AFadeOutImage.ClientRect, cxbsNormal);
end;

function TdxRibbonCustomButtonViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxRibbonCustomButtonViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := cxRectPtIn(Bounds, AHitInfo.HitPoint);
  if Result then
  begin
    AHitInfo.HitTest := rhtCustomButton;
    AHitInfo.HitObject := Self;
  end;
end;

function TdxRibbonCustomButtonViewInfo.GetPainter: TdxRibbonPainter;
begin
  Result := ViewInfo.Painter;
end;

function TdxRibbonCustomButtonViewInfo.GetRibbon: TdxCustomRibbon;
begin
  Result := ViewInfo.Ribbon;
end;

function TdxRibbonCustomButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ribbon.ScaleFactor;
end;

function TdxRibbonCustomButtonViewInfo.GetState: TcxButtonState;
begin
  Result := FState;
  if (Result = cxbsNormal) and AccessibilityHelper.IsSelected then
    Result := cxbsHot;
end;

function TdxRibbonCustomButtonViewInfo.GetViewInfo: TdxRibbonViewInfo;
begin
  Result := Owner.Owner;
end;

procedure TdxRibbonCustomButtonViewInfo.SetState(AValue: TcxButtonState);
begin
  if AValue <> State then
  begin
    if CanFade then
    begin
      if (AValue = cxbsHot) and (State = cxbsNormal) then
        dxFader.FadeIn(Self);
      if (AValue = cxbsNormal) and (State = cxbsHot) then
        dxFader.FadeOut(Self);
    end;
    FState := AValue;
    Invalidate;
  end;
end;

{ TdxRibbonTabViewInfo }

constructor TdxRibbonTabViewInfo.Create(ATab: TdxRibbonTab; AOwner: TdxRibbonTabsViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  FTab := ATab;
end;

procedure TdxRibbonTabViewInfo.Calculate(const ABounds: TRect; ASeparatorAlpha: Byte);
begin
  FIsRightToLeftConverted := False;
  FBounds := ABounds;

  FSeparatorAlphaValue := ASeparatorAlpha;
  if HasSeparator then
    FSeparatorBounds := cxRect(Bounds.Right - dxRibbonTabSeparatorWidth, Bounds.Top, Bounds.Right, Bounds.Bottom - 1)
  else
    FSeparatorBounds := cxEmptyRect;

  FTextBounds := cxRectContent(Bounds, FOwner.FTabContentOffset);
end;

procedure TdxRibbonTabViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if not dxFader.DrawFadeImage(Tab, ACanvas.Handle, Bounds) then
    DrawBackground(ACanvas, Bounds, State);

  ACanvas.Font := Font;
  DrawText(ACanvas, SeparatorAlphaValue = 255);
  if HasSeparator then
    ColorScheme.DrawTabSeparator(ACanvas.Handle, SeparatorBounds, SeparatorAlphaValue);
  if ContextBegin then
    ColorScheme.DrawContextTabSeparator(ACanvas.Handle, cxRectSetWidth(Bounds, ContextTabSeparatorWidth), True);
  if ContextEnd then
    ColorScheme.DrawContextTabSeparator(ACanvas.Handle, cxRectSetRight(Bounds, Bounds.Right, ContextTabSeparatorWidth), False);
  if Tab.DesignSelectionHelper.IsComponentSelected then
    ACanvas.DrawDesignSelection(cxRectInflate(Bounds, -2));
end;

function TdxRibbonTabViewInfo.HasSeparator: Boolean;
begin
  Result := FCanHasSeparator and (FSeparatorAlphaValue > 0);
end;

function TdxRibbonTabViewInfo.IsPaintOnGlass: Boolean;
begin
  Result := Owner.Owner.IsTabsOnGlass;
end;

procedure TdxRibbonTabViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxRibbonTabViewInfo.DrawBackground(ACanvas: TcxCanvas; const R: TRect; AState: TdxRibbonTabState);
begin
  if Tab.Context <> nil then
    ColorScheme.DrawContextTabBackground(ACanvas.Handle, R, AState, Tab.Context.Color)
  else
    ColorScheme.DrawTab(ACanvas.Handle, R, AState);
end;

procedure TdxRibbonTabViewInfo.DrawText(ACanvas: TcxCanvas; AHasSeparator: Boolean);
const
  CustomFlags: array[Boolean] of Integer = (
    cxAlignVCenter or cxSingleLine or cxAlignHCenter,
    cxAlignVCenter or cxSingleLine
  );
  GlassFlags: array[Boolean] of Integer = (
    DT_VCENTER or DT_SINGLELINE or DT_CENTER,
    DT_VCENTER or DT_SINGLELINE
  );
var
  AFlags: Cardinal;
begin
  if IsPaintOnGlass then
  begin
    AFlags := DT_NOPREFIX or GlassFlags[AHasSeparator];
    if Ribbon.UseRightToLeftReading then
      AFlags := AFlags or DT_RTLREADING;
    if AHasSeparator then
      if Ribbon.UseRightToLeftAlignment then
        AFlags := AFlags or DT_RIGHT
      else
        AFlags := AFlags or DT_LEFT;
    dxDrawTextOnGlass(ACanvas.Handle, Tab.DisplayCaption, ACanvas.Font, TextBounds,
      ACanvas.Font.Color, AFlags, 0, True);
  end
  else
  begin
    ACanvas.Brush.Style := bsClear;
    AFlags := CustomFlags[AHasSeparator];
    if Ribbon.UseRightToLeftReading then
      ACanvas.TextFlags := ACanvas.TextFlags or ETO_RTLREADING;
    if AHasSeparator then
      if Ribbon.UseRightToLeftAlignment then
        AFlags := AFlags or cxAlignRight
      else
        AFlags := AFlags or cxAlignLeft;
    ACanvas.DrawText(Tab.DisplayCaption, TextBounds, AFlags);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TdxRibbonTabViewInfo.CalculateWidths(ACanvas: TcxCanvas; AViewInfo: TdxRibbonViewInfo);
var
  ATextIndent: Integer;
  ATextWidth: Integer;
begin
  ATextWidth := ACanvas.TextWidth(Tab.DisplayCaption);
  ATextIndent := ScaleFactor.Apply(dxRibbonTabTextOffset) * 2;
  FOptimalWidth := ATextWidth + ScaleFactor.Apply(dxRibbonOptimalTabSpace);
  if ContextBegin then
  begin
    Inc(FOptimalWidth, dxExtraContextGap);
    if ContextEnd then
      FOptimalWidth := Max(OptimalWidth, AViewInfo.ContextsViewInfo.CalculateCaptionWidth(ACanvas, Tab.Context.Caption) + dxExtraContextGap);
  end;
  FMinWidth := ACanvas.TextWidth(Copy(Tab.DisplayCaption, 1, 3)) + ATextIndent;
  FMinWidth := Max(FMinWidth, ScaleFactor.Apply(dxRibbonOptimalTabSpace));
  FWidth := ATextWidth + ATextIndent;
end;

procedure TdxRibbonTabViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
  FSeparatorBounds := TdxRightToLeftLayoutConverter.ConvertRect(FSeparatorBounds, ABounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, ABounds);
end;

function TdxRibbonTabViewInfo.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Ribbon.ColorScheme;
end;

function TdxRibbonTabViewInfo.GetContextTabSeparatorWidth: Integer;
begin
  Result := Ribbon.SkinGetPartSize(rspContextTabSeparatorBegin);
end;

function TdxRibbonTabViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ribbon.ScaleFactor;
end;

function TdxRibbonTabViewInfo.GetState: TdxRibbonTabState;
const
  ActiveHotMap: array[Boolean] of TdxRibbonTabState = (rtsActive, rtsActiveHot);
  FocusMap: array[Boolean] of TdxRibbonTabState = (rtsHot, rtsFocused);
  HotMap: array[Boolean] of TdxRibbonTabState = (rtsNormal, rtsHot);
begin
  if Tab.Focused then
    Result := FocusMap[Ribbon.AreGroupsVisible]
  else
    if not (Tab.Highlighted or Tab.Active) then
      Result := rtsNormal
    else
      if IsTabActive then
        Result := ActiveHotMap[Tab.Highlighted and not Ribbon.IsPopupGroupsMode]
      else
        Result := HotMap[Tab.Highlighted];
end;

function TdxRibbonTabViewInfo.IsSelected: Boolean;
begin
  Result := Tab.DesignSelectionHelper.IsComponentSelected;
end;

function TdxRibbonTabViewInfo.IsTabActive: Boolean;
begin
  Result := Tab.Active and Ribbon.AreGroupsVisible and (Ribbon.ApplicationMenuState <= ramsShownAsMenu);
end;

function TdxRibbonTabViewInfo.PrepareFadeImage(ADrawHot: Boolean): TcxBitmap;

  function GetFadeImageState: TdxRibbonTabState;
  const
    ActiveStatesMap: array[Boolean] of TdxRibbonTabState = (rtsActive, rtsActiveHot);
    StatesMap: array[Boolean] of TdxRibbonTabState = (rtsNormal, rtsHot);
  begin
    if State = rtsFocused then
      Result := State
    else
      if IsTabActive then
        Result := ActiveStatesMap[ADrawHot and not Ribbon.IsPopupGroupsMode]
      else
        Result := StatesMap[ADrawHot];
  end;

begin
  Result := TcxBitmap32.CreateSize(Bounds, True);
  Result.Canvas.Font := Font;
  Result.cxCanvas.UseRightToLeftAlignment := Ribbon.UseRightToLeftAlignment;
  DrawBackground(Result.cxCanvas, Result.ClientRect, GetFadeImageState);
end;

function TdxRibbonTabViewInfo.GetFont: TFont;

  function GetTabState: Integer;
  begin
    if IsTabActive then
      Result := DXBAR_ACTIVE
    else
      if State = rtsNormal then
        Result := DXBAR_NORMAL
      else
        Result := DXBAR_HOT;
  end;

begin
  Result := Ribbon.Fonts.GetTabHeaderFont(GetTabState, Tab.Context);
end;

function TdxRibbonTabViewInfo.GetRibbon: TdxCustomRibbon;
begin
  Result := Tab.Ribbon;
end;

{ TdxRibbonTabsDropDownButtonViewInfo }

procedure TdxRibbonTabsDropDownButtonViewInfo.Click;
begin
  if Ribbon.IsDesigning then
    BarDesignController.ShowCustomCustomizePopup(Ribbon.BarManager, PopulateMenuItems, Ribbon.GroupsPainter,
      Ribbon.ClientToScreen(Point(Bounds.Left, Bounds.Bottom)))
  else
    ShowMenu;
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
const
  TabStateMap: array [TcxButtonState] of TdxRibbonTabState = (rtsNormal, rtsNormal, rtsHot, rtsActive, rtsNormal);
begin
  Painter.ColorScheme.DrawTab(ACanvas.Handle, R, TabStateMap[AState]);
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.ColorScheme.DrawArrowDown(ACanvas.Handle, R, DXBAR_NORMAL);
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.ActivateMenuItem(Sender: TObject);
begin
  TdxRibbonTab(TdxBarButton(Sender).Tag).Active := True;
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.ClearMenu(AItemLinks: TdxBarItemLinks);
var
  I: Integer;
begin
  AItemLinks.BarManager.BeginUpdate;
  try
    AItemLinks.BeginUpdate;
    try
      for I := AItemLinks.Count - 1 downto 0 do
        AItemLinks[I].Item.Free;
    finally
      AItemLinks.EndUpdate;
    end;
  finally
    AItemLinks.BarManager.EndUpdate;
  end;
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.PopulateMenuItemContent(AItemLinks: TdxBarItemLinks; ASource: TdxRibbonTab);
var
  AButton: TdxBarButton;
begin
  AButton := BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    ASource.DisplayCaption, ActivateMenuItem, NativeInt(ASource)).Item as TdxBarButton;
  AButton.Hint := '';
  AButton.ButtonStyle := bsChecked;
  AButton.Down := ASource.Active;
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.PopulateMenuItems(AItemLinks: TdxBarItemLinks);
var
  ATab: TdxRibbonTab;
  ATabsViewInfo: TdxRibbonTabsViewInfo;
  AViewInfoIndex: Integer;
  I: Integer;
begin
  AItemLinks.BarManager.BeginUpdate;
  try
    AItemLinks.BeginUpdate;
    try
      ATabsViewInfo := ViewInfo.TabsAreaViewInfo.TabsViewInfo;
      for I := 0 to Ribbon.TabCount - 1 do
      begin
        ATab := Ribbon.Tabs[I];
        if ATab.IsVisible then
        begin
          AViewInfoIndex := ATabsViewInfo.IndexOf(ATab);
          if (AViewInfoIndex < 0) or ATab.Active and (AViewInfoIndex = ATabsViewInfo.Count - 1) and (AItemLinks.Count > 0) then
            PopulateMenuItemContent(AItemLinks, ATab);
        end;
      end;
    finally
      AItemLinks.EndUpdate;
    end;
  finally
    AItemLinks.BarManager.EndUpdate;
  end;
end;

procedure TdxRibbonTabsDropDownButtonViewInfo.ShowMenu;
var
  APoint: TPoint;
begin
  FMenu := TdxRibbonPopupMenu.Create(nil);
  try
    FMenu.Ribbon := Ribbon;
    PopulateMenuItems(FMenu.ItemLinks);
    APoint := Ribbon.ClientToScreen(Point(Bounds.Left, Bounds.Bottom));
    FMenu.Popup(APoint.X, APoint.Y);
    ClearMenu(FMenu.ItemLinks);
  finally
    FreeAndNil(FMenu);
  end;
end;

{ TdxRibbonTabsScrollButtonViewInfo }

procedure TdxRibbonTabsScrollButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  inherited;
  ACanvas.ExcludeClipRect(Bounds);
end;

procedure TdxRibbonTabsScrollButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawTabScrollButton(ACanvas, R, (FScrollButton = rsbLeft) xor Ribbon.UseRightToLeftAlignment, AState);
end;

procedure TdxRibbonTabsScrollButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawTabScrollButtonGlyph(ACanvas, R, (FScrollButton = rsbLeft) xor Ribbon.UseRightToLeftAlignment);
end;

function TdxRibbonTabsScrollButtonViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
const
  HitTestMap: array[TdxRibbonScrollButton] of TdxRibbonHitTest = (rhtTabScrollLeft, rhtTabScrollRight);
begin
  Result := inherited GetHitInfo(AHitInfo);
  if Result then
    AHitInfo.HitTest := HitTestMap[FScrollButton];
end;

{ TdxRibbonTabsViewInfo }

constructor TdxRibbonTabsViewInfo.Create(AOwner: TdxRibbonViewInfo);

  function CreateButton(AScrollButton: TdxRibbonScrollButton): TdxRibbonTabsScrollButtonViewInfo;
  begin
    Result := TdxRibbonTabsScrollButtonViewInfo.Create(Self);
    Result.FScrollButton := AScrollButton;
  end;

begin
  inherited Create(AOwner);
  FBitmap := TcxBitmap32.Create;
  FScrollButtonLeft := CreateButton(rsbLeft);
  FScrollButtonRight := CreateButton(rsbRight);
end;

destructor TdxRibbonTabsViewInfo.Destroy;
begin
  FreeAndNil(FScrollButtonRight);
  FreeAndNil(FScrollButtonLeft);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TdxRibbonTabsViewInfo.CreateTabViewInfo(ATab: TdxRibbonTab): TdxRibbonTabViewInfo;
begin
  Result := TdxRibbonTabViewInfo.Create(ATab, Self);
end;

procedure TdxRibbonTabsViewInfo.BalancedReduce(ATotalDelta: Integer);

  function GetLongestTabWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Count - 1 do
      Result := Max(Result, Items[I].Width);
  end;

var
  AHasReduce: Boolean;
  ALimit: Integer;
  ATab: TdxRibbonTabViewInfo;
  I: Integer;
begin
  FSeparatorAlpha := 255;
  ALimit := GetLongestTabWidth - 1;
  repeat
    AHasReduce := False;
    for I := 0 to Count - 1 do
    begin
      ATab := Items[I];
      if (ATab.Width > ALimit) and (ATab.Width > GetRealMinItemWidth(I)) then
      begin
        AHasReduce := True;
        Dec(ATab.FWidth);
        Dec(ATotalDelta);
        if ATotalDelta = 0 then
          Break;
      end;
    end;
    Dec(ALimit);
  until (ATotalDelta = 0) or not AHasReduce;
  FNeedShowHint := AHasReduce;
end;

procedure TdxRibbonTabsViewInfo.Calculate(const ABounds: TRect);
var
  AMaxSeparatorIndex: Integer;
  ATab: TdxRibbonTabViewInfo;
  I: Integer;
begin
  inherited Calculate(ABounds);

  FNeedShowHint := False;
  FScrollButtons := [];
  FSeparatorAlpha := 0;
  FTotalMinimalWidth := 0;
  FTotalOptimalWidth := 0;

  AMaxSeparatorIndex := Count - IfThen(Owner.TabsAreaViewInfo.ButtonsViewInfo.Count = 0, 1);
  cxScreenCanvas.Font := Ribbon.Fonts.TabHeader;
  try
    for I := 0 to Count - 1 do
    begin
      ATab := Items[I];
      ATab.CalculateWidths(cxScreenCanvas, Owner);
      Inc(FTotalMinimalWidth, ATab.MinWidth);
      Inc(FTotalOptimalWidth, ATab.OptimalWidth);
      ATab.FCanHasSeparator := I < AMaxSeparatorIndex;
    end;
  finally
    cxScreenCanvas.Dormant;
  end;

  CalculateLayout;
  CalculateScrollButtons;
  FBitmap.SetSize(Bounds);
end;

procedure TdxRibbonTabsViewInfo.CalculateMetrics(ACanvas: TcxCanvas);
begin
  FTabContentOffset := Painter.GetTabContentOffset;
end;

procedure TdxRibbonTabsViewInfo.CalculateComplexTabLayout;
var
  I, ADelta, ASimpleReduceWidth: Integer;
  R: TRect;
begin
  RemoveScrolling;
  R := Bounds;
  ADelta := FTotalOptimalWidth - (R.Right - R.Left);
  ASimpleReduceWidth := ScaleFactor.Apply(dxRibbonTabIndent) * Count;
  if ADelta <= ASimpleReduceWidth then
    SimpleReduce(ADelta)
  else
  begin
    Dec(ADelta, ASimpleReduceWidth);
    BalancedReduce(ADelta);
  end;
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      R.Right := R.Left + FWidth;
      Calculate(R, FSeparatorAlpha);
      R.Left := R.Right;
    end;
end;

procedure TdxRibbonTabsViewInfo.CalculateLayout;
begin
  if FTotalOptimalWidth <= cxRectWidth(Bounds) then
    CalculateSimpleTabLayout
  else
    if FTotalMinimalWidth <= cxRectWidth(Bounds) then
      CalculateComplexTabLayout
    else
      CalculateScrollingTabLayout;
end;

procedure TdxRibbonTabsViewInfo.CalculateScrollingTabLayout;
var
  I: Integer;
  R: TRect;
begin
  R := Bounds;
  FSeparatorAlpha := 255;
  FScrollWidth := GetScrollWidth;
  CheckScrollPosition(FScrollPosition);
  Dec(R.Left, FScrollPosition);
  for I := 0 to Count - 1 do
  begin
    R.Right := R.Left + GetRealMinItemWidth(I);
    Items[I].Calculate(R, FSeparatorAlpha);
    R.Left := R.Right;
  end;

  if FScrollPosition = 0 then
    FScrollButtons := [rsbRight]
  else
    if FScrollPosition = FScrollWidth then
      FScrollButtons := [rsbLeft]
    else
      FScrollButtons := [rsbLeft, rsbRight];

  FNeedShowHint := True;
end;

procedure TdxRibbonTabsViewInfo.CalculateScrollButtons;
var
  AButtonWidth: Integer;
begin
  AButtonWidth := Owner.ScrollButtonWidth;
  FScrollButtonLeft.Bounds := cxRectSetWidth(Bounds, IfThen(rsbLeft in ScrollButtons, AButtonWidth));
  FScrollButtonRight.Bounds := cxRectSetRight(Bounds, Bounds.Right, IfThen(rsbRight in ScrollButtons, AButtonWidth));
end;

procedure TdxRibbonTabsViewInfo.CalculateSimpleTabLayout;
begin
  RemoveScrolling;
  CalculateSimpleTabLayoutCore;
  if Count > 0 then
    FBounds.Right := Last.Bounds.Right;
end;

procedure TdxRibbonTabsViewInfo.CalculateSimpleTabLayoutCore;
var
  ATab: TdxRibbonTabViewInfo;
  I: Integer;
  R: TRect;
begin
  R := Bounds;
  for I := 0 to Count - 1 do
  begin
    ATab := Items[I];
    R.Right := R.Left + ATab.OptimalWidth;
    if ATab.ContextBegin then
      Inc(R.Left, dxExtraContextGap);
    ATab.Calculate(R, 0);
    R.Left := R.Right;
  end;
end;

procedure TdxRibbonTabsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
  FScrollButtonLeft.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(FScrollButtonLeft.Bounds, ABounds);
  FScrollButtonRight.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(FScrollButtonRight.Bounds, ABounds);
end;

procedure TdxRibbonTabsViewInfo.CheckScrollPosition(var Value: Integer);
begin
  Value := Min(Max(0, Value), FScrollWidth);
end;

procedure TdxRibbonTabsViewInfo.CreateContextsViewInfoCells;
var
  AContext: TdxRibbonContext;
  AContextBegin: Boolean;
  ATab: TdxRibbonTab;
  ATabViewInfo: TdxRibbonTabViewInfo;
  I, J: Integer;
begin
  for I := 0 to Owner.Ribbon.Contexts.Count - 1 do
  begin
    AContext := Owner.Ribbon.Contexts[I];
    if not AContext.Visible then
      Continue;
    AContextBegin := True;
    ATabViewInfo := nil;
    for J := 0 to Owner.Ribbon.TabCount - 1 do
    begin
      ATab := Owner.Ribbon.Tabs[J];
      if ATab.Visible and (ATab.Context = AContext) then
      begin
        ATabViewInfo := CreateTabViewInfo(ATab);
        if AContextBegin then
        begin
          ATabViewInfo.ContextBegin := True;
          AContextBegin := False;
        end;
        AddItem(ATabViewInfo);
      end;
    end;
    if ATabViewInfo <> nil then
      ATabViewInfo.ContextEnd := True;
  end;
end;

procedure TdxRibbonTabsViewInfo.CreateTabsViewInfoCells;
var
  ATab: TdxRibbonTab;
  I: Integer;
begin
  for I := 0 to Owner.Ribbon.TabCount - 1 do
  begin
    ATab := Owner.Ribbon.Tabs[I];
    if ATab.Visible and (ATab.Context = nil) then
      AddItem(CreateTabViewInfo(ATab));
  end;
end;

procedure TdxRibbonTabsViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Bitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
  cxBitBlt(Bitmap.Canvas.Handle, ACanvas.Handle, Bounds, Bounds.TopLeft, SRCCOPY);
  DrawCore(Bitmap.cxCanvas);
  cxBitBlt(ACanvas.Handle, Bitmap.Canvas.Handle, Bounds, Bounds.TopLeft, SRCCOPY);
end;

procedure TdxRibbonTabsViewInfo.DrawCore(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas);
  FScrollButtonRight.Draw(ACanvas);
  FScrollButtonLeft.Draw(ACanvas);
end;

function TdxRibbonTabsViewInfo.Last: TdxRibbonTabViewInfo;
begin
  Result := TdxRibbonTabViewInfo(FCells.Last);
end;

procedure TdxRibbonTabsViewInfo.MakeTabVisible(ATab: TdxRibbonTab);
var
  AIndex: Integer;
  P: Integer;
  R: TRect;
begin
  if ScrollButtons = [] then
    Exit;

  AIndex := IndexOf(ATab);
  if AIndex >= 0 then
  begin
    R := Items[AIndex].Bounds;
    P := ScrollPosition;
    if R.Left < Bounds.Left then
    begin
      Dec(P, Bounds.Left - R.Left);
      if AIndex > 0 then
        Dec(P, Owner.ScrollButtonWidth);
    end
      else
      if R.Right > Bounds.Right then
      begin
        Inc(P, R.Right - Bounds.Right);
        if AIndex < Count - 1 then
          Inc(P, Owner.ScrollButtonWidth);
      end;

    SetScrollPosition(P);
  end;
end;

function TdxRibbonTabsViewInfo.MeasureHeight(ACanvas: TcxCanvas): Integer;
begin
  Result := ACanvas.FontHeight(Ribbon.Fonts.TabHeader) + cxMarginsHeight(FTabContentOffset);
end;

procedure TdxRibbonTabsViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AProc(Items[I].Tab.IAccessibilityHelper);
end;

procedure TdxRibbonTabsViewInfo.RecreateViewInfoCells;
begin
  Clear;
  CreateTabsViewInfoCells;
  CreateContextsViewInfoCells;
end;

procedure TdxRibbonTabsViewInfo.RemoveScrolling;
begin
  FScrollPosition := 0;
  FScrollButtons := [];
  CalculateScrollButtons;
end;

procedure TdxRibbonTabsViewInfo.SimpleReduce(ATotalDelta: Integer);
var
  I, ADelta, ARemainder: Integer;
begin
  FSeparatorAlpha := MulDiv(ATotalDelta, 255, Count *
    (ScaleFactor.Apply(dxRibbonOptimalTabSpace) - ScaleFactor.Apply(dxRibbonTabTextOffset) * 2));

  ADelta := ATotalDelta div Count;
  ARemainder := ATotalDelta - (Count * ADelta);
  for I := Count - 1 downto 0 do
    with Items[I] do
    begin
      FWidth := OptimalWidth - ADelta;
      if Count - I <= ARemainder then
        Dec(FWidth);
    end;
end;

procedure TdxRibbonTabsViewInfo.UpdateButtonsStates;
begin
  FScrollButtonLeft.CalculateState;
  FScrollButtonRight.CalculateState;
end;

procedure TdxRibbonTabsViewInfo.UpdateDockControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Tab.UpdateDockControl;
end;

function TdxRibbonTabsViewInfo.GetController: TdxRibbonController;
begin
  Result := Ribbon.Controller;
end;

function TdxRibbonTabsViewInfo.Find(ATab: TdxRibbonTab): TdxRibbonTabViewInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Tab = ATab then
      Exit;
  end;
  Result := nil;
end;

function TdxRibbonTabsViewInfo.GetDesignHitTest(const P: TPoint): Boolean;
begin
  Result := GetTabAtPos(P.X, P.Y) <> nil;
end;

function TdxRibbonTabsViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
var
  I: Integer;
begin
  Result := FScrollButtonLeft.GetHitInfo(AHitInfo) or FScrollButtonRight.GetHitInfo(AHitInfo);
  if not Result and cxRectPtIn(GetRealBounds, AHitInfo.HitPoint) then
    for I := 0 to Count - 1 do
    begin
      Result := cxRectPtIn(Items[I].Bounds, AHitInfo.HitPoint);
      if Result then
      begin
        AHitInfo.HitTest := rhtTab;
        AHitInfo.HitObject := Items[I].Tab;
        Break;
      end;
    end;
end;

function TdxRibbonTabsViewInfo.GetPainter: TdxRibbonPainter;
begin
  Result := Owner.Painter;
end;

function TdxRibbonTabsViewInfo.GetRealBounds: TRect;
begin
  Result := Bounds;
  if Ribbon.UseRightToLeftAlignment then
  begin
    Result.Left := FScrollButtonRight.Bounds.Right;
    Result.Right := FScrollButtonLeft.Bounds.Left;
  end
  else
  begin
    Result.Left := FScrollButtonLeft.Bounds.Right;
    Result.Right := FScrollButtonRight.Bounds.Left;
  end;
end;

function TdxRibbonTabsViewInfo.GetRealMinItemWidth(Index: Integer): Integer;
begin
  Result := GetTabViewInfo(Index).MinWidth;
  if Index < Count - 1 then
    Inc(Result, dxRibbonTabSeparatorWidth);
end;

function TdxRibbonTabsViewInfo.GetScrollWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, GetRealMinItemWidth(I));
  Dec(Result, Bounds.Right - Bounds.Left);
end;

function TdxRibbonTabsViewInfo.GetTabAtPos(X, Y: Integer): TdxRibbonTab;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if cxRectPtIn(Items[I].Bounds, X, Y) then
      Exit(Items[I].Tab);
  Result := nil;
end;

function TdxRibbonTabsViewInfo.IndexOf(ATab: TdxRibbonTab): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].Tab = ATab then
      Exit(I);
  end;
  Result := -1;
end;

function TdxRibbonTabsViewInfo.GetTabViewInfo(Index: Integer): TdxRibbonTabViewInfo;
begin
  Result := TdxRibbonTabViewInfo(inherited Items[Index]);
end;

procedure TdxRibbonTabsViewInfo.SetScrollPosition(Value: Integer);
begin
  CheckScrollPosition(Value);
  if FScrollPosition <> Value then
  begin
    FScrollPosition := Value;
    Owner.Ribbon.Changed;
  end;
end;

{ TdxRibbonTabsViewInfo2016Tablet }

constructor TdxRibbonTabsViewInfo2016Tablet.Create(AOwner: TdxRibbonViewInfo);
begin
  inherited;
  FDropDownButton := TdxRibbonTabsDropDownButtonViewInfo.Create(Self);
end;

destructor TdxRibbonTabsViewInfo2016Tablet.Destroy;
begin
  FreeAndNil(FDropDownButton);
  inherited;
end;

procedure TdxRibbonTabsViewInfo2016Tablet.CalculateLayout;
var
  AActiveTab: TdxRibbonTab;
  ADropDownButtonWidth: Integer;
  APrevActiveTab: TdxRibbonTab;
  ATab: TdxRibbonTab;
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FCanHasSeparator := False;

  AActiveTab := Ribbon.ActiveTab;
  APrevActiveTab := Ribbon.PreviousActiveTab;
  ADropDownButtonWidth := 0;

  CalculateSimpleTabLayoutCore;
  while (Count > 1) and ((Items[Count - 1].Bounds.Right + ADropDownButtonWidth) > Bounds.Right) do
  begin
    for I := Count - 1 downto 0 do
    begin
      ATab := Items[I].Tab;
      if ((AActiveTab = nil) or (ATab <> AActiveTab)) and
        ((APrevActiveTab = nil) or (ATab <> APrevActiveTab) or (Count <= 2)) then
      begin
        FCells.FreeAndDelete(I);
        Break;
      end;
    end;
    ADropDownButtonWidth := Owner.TabsAreaViewInfo.MeasuredHeight;
    CalculateSimpleTabLayoutCore;
  end;

  FDropDownButton.Bounds := cxRectSetLeft(Bounds, Items[Count - 1].Bounds.Right, ADropDownButtonWidth);
  FBounds.Right := FDropDownButton.Bounds.Right;
end;

procedure TdxRibbonTabsViewInfo2016Tablet.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FDropDownButton.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(FDropDownButton.Bounds, ABounds);
end;

function TdxRibbonTabsViewInfo2016Tablet.GetDesignHitTest(const P: TPoint): Boolean;
begin
  Result := inherited or cxRectPtIn(DropDownButton.Bounds, P);
end;

function TdxRibbonTabsViewInfo2016Tablet.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := FDropDownButton.GetHitInfo(AHitInfo) or inherited;
end;

function TdxRibbonTabsViewInfo2016Tablet.GetRealBounds: TRect;
begin
  Result := inherited GetRealBounds;
  if Ribbon.UseRightToLeftAlignment then
    Result.Left := DropDownButton.Bounds.Right
  else
    Result.Right := DropDownButton.Bounds.Left;
end;

procedure TdxRibbonTabsViewInfo2016Tablet.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  inherited;
  AProc(DropDownButton.AccessibilityHelper);
end;

procedure TdxRibbonTabsViewInfo2016Tablet.UpdateButtonsStates;
begin
  inherited;
  DropDownButton.CalculateState;
end;

procedure TdxRibbonTabsViewInfo2016Tablet.DrawCore(ACanvas: TcxCanvas);
begin
  inherited;
  DropDownButton.Draw(ACanvas);
end;

{ TdxRibbonTabsViewInfo2019 }

destructor TdxRibbonTabsViewInfo2019.Destroy;
begin
  if FTabBaseRectAnimation <> nil then
    FTabBaseRectAnimation.Terminate;
  inherited;
end;

procedure TdxRibbonTabsViewInfo2019.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  CalculateTabBaseRect;
  if not Ribbon.UseRightToLeftAlignment then
    dxAnimationController.Update;
end;

function TdxRibbonTabsViewInfo2019.MeasureHeight(ACanvas: TcxCanvas): Integer;
begin
  Result := inherited + ScaleFactor.Apply(TabBaseHeight);
end;

procedure TdxRibbonTabsViewInfo2019.CalculateTabBaseRect;
var
  AActiveTabViewInfo: TdxRibbonTabViewInfo;
  AHeight: Integer;
  ATextWidth: Integer;
begin
  AActiveTabViewInfo := ActiveTabViewInfo;
  if AActiveTabViewInfo <> nil then
  begin
    AHeight := ScaleFactor.Apply(TabBaseHeight);
    FTabBaseRectLTR := cxRectSetBottom(Bounds, Bounds.Bottom, AHeight);
    FTabBaseRectLTR.Left := AActiveTabViewInfo.Bounds.Left;
    FTabBaseRectLTR.Right := AActiveTabViewInfo.Bounds.Right;
    if not (AActiveTabViewInfo.State in [rtsHot, rtsActiveHot]) then
    begin
      ATextWidth := cxTextWidth(AActiveTabViewInfo.Font, AActiveTabViewInfo.Tab.DisplayCaption);
      if ATextWidth < cxRectWidth(FTabBaseRectLTR) then
        FTabBaseRectLTR := cxRectCenterHorizontally(FTabBaseRectLTR, ATextWidth);
    end;
  end
  else
    FTabBaseRectLTR := cxNullRect;


  if not Ribbon.UseRightToLeftAlignment then
    UpdateTabBaseRect(FTabBaseRectLTR);
end;

function TdxRibbonTabsViewInfo2019.CanAnimateTabBaseRectChange: Boolean;
begin
  Result := (dxRibbonTabStateChangeAnimationTime > 0) and
    not (Ribbon.IsDesigning or (rsUpdatingColorScheme in Ribbon.FState));
end;

function TdxRibbonTabsViewInfo2019.CreateTabBaseAnimation(const AOldRect, ANewRect: TRect): TdxRectAnimationTransition;
begin
  if cxRectIntersect(AOldRect, ANewRect) then
    Result := TdxRectAnimationTransition.Create(AOldRect, ANewRect, dxRibbonTabStateChangeAnimationTime, ateAccelerateDecelerate)
  else
    Result := TdxRectAnimationTransition.Create(AOldRect, ANewRect, dxRibbonTabStateChangeAnimationTime);
end;

procedure TdxRibbonTabsViewInfo2019.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FTabBaseRectLTR := TdxRightToLeftLayoutConverter.ConvertRect(FTabBaseRectLTR, ABounds);
  UpdateTabBaseRect(FTabBaseRectLTR);
  dxAnimationController.Update;
end;

procedure TdxRibbonTabsViewInfo2019.DrawCore(ACanvas: TcxCanvas);
begin
  inherited;
  if Owner.IsTabGroupsVisible then
    Painter.DrawTabBase(ACanvas, FTabBaseRect);
end;

procedure TdxRibbonTabsViewInfo2019.UpdateButtonsStates;
begin
  inherited;
  CalculateTabBaseRect;
end;

procedure TdxRibbonTabsViewInfo2019.UpdateTabBaseRect(const AArea: TRect);
begin
  if not cxRectIsEqual(AArea, FTabBaseRect) then
  begin
    if FTabBaseRectAnimation <> nil then
      FTabBaseRectAnimation.TargetRect := AArea
    else
      if CanAnimateTabBaseRectChange then
      begin
        FTabBaseRectAnimation := CreateTabBaseAnimation(FTabBaseRect, AArea);
        FTabBaseRectAnimation.OnAnimate := HandlerAnimate;
        FTabBaseRectAnimation.OnTerminate := HandlerAnimationTerminated;
        FTabBaseRectAnimation.Resume;
      end
      else
        FTabBaseRect := AArea;
  end;
end;

procedure TdxRibbonTabsViewInfo2019.HandlerAnimate(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
var
  ARect: TRect;
begin
  ARect := FTabBaseRectAnimation.CurrentRect;
  if not cxRectIsEqual(ARect, FTabBaseRect) then
  begin
    InvalidateRect(ARect);
    InvalidateRect(FTabBaseRect);
    FTabBaseRect := ARect;
  end;
end;

procedure TdxRibbonTabsViewInfo2019.HandlerAnimationTerminated(Sender: TObject);
begin
  FTabBaseRectAnimation := nil;
end;

function TdxRibbonTabsViewInfo2019.GetActiveTabViewInfo: TdxRibbonTabViewInfo;
begin
  Result := Ribbon.ActiveTab.ViewInfo;
end;

{ TdxRibbonHelpButtonViewInfo }

procedure TdxRibbonHelpButtonViewInfo.Click;
begin
  Ribbon.DoHelpButtonClick;
end;

procedure TdxRibbonHelpButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawHelpButton(ACanvas, R, AState);
end;

procedure TdxRibbonHelpButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawHelpButtonGlyph(ACanvas, HelpButton.Glyph, GlyphBounds, AState);
end;

function TdxRibbonHelpButtonViewInfo.GetHelpButton: TdxRibbonHelpButton;
begin
  Result := Ribbon.HelpButton;
end;

function TdxRibbonHelpButtonViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := inherited GetHitInfo(AHitInfo);
  if Result then
    AHitInfo.HitTest := rhtHelpButton;
end;

function TdxRibbonHelpButtonViewInfo.GetGlyphBounds: TRect;
var
  AIconSize: TSize;
begin
  if (HelpButton.Glyph = nil) or HelpButton.Glyph.Empty then
    AIconSize := cxSize(dxGetSystemMetrics(SM_CXSMICON, ScaleFactor), dxGetSystemMetrics(SM_CYSMICON, ScaleFactor))
  else
    AIconSize := dxGetImageSize(HelpButton.Glyph, ScaleFactor);

  Result := Bounds;
  if HelpButton.StretchGlyph then
    AIconSize := cxSize(cxRectProportionalStretch(Result, AIconSize.cx, AIconSize.cy));
  Result := cxRectCenter(Result, AIconSize);
end;

function TdxRibbonHelpButtonViewInfo.GetIsEnabled: Boolean;
begin
  Result := True;
end;

{ TdxRibbonMinimizeButtonViewInfo }

procedure TdxRibbonMinimizeButtonViewInfo.Click;
begin
  if Enabled then
    Ribbon.ShowTabGroups := not Ribbon.ShowTabGroups;
end;

procedure TdxRibbonMinimizeButtonViewInfo.DrawButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawMinimizeButton(ACanvas, R, AState, not Ribbon.ShowTabGroups);
end;

procedure TdxRibbonMinimizeButtonViewInfo.DrawButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawMinimizeButtonGlyph(ACanvas, R, AState, State);
end;

function TdxRibbonMinimizeButtonViewInfo.GetHint: string;
begin
  case State of
    rmbRestore:
      Result := cxGetResourceString(@dxSBAR_RIBBON_RESTORERIBBON);
    rmbPin:
      Result := cxGetResourceString(@dxSBAR_RIBBON_PINRIBBON);
  else
    Result := cxGetResourceString(@dxSBAR_RIBBON_MINIMIZERIBBON);
  end;
end;

function TdxRibbonMinimizeButtonViewInfo.GetState: TdxRibbonMinimizeButtonGlyph;
begin
  if Ribbon.ShowTabGroups then
    Result := rmbMinimize
  else
    if Ribbon.IsPopupGroupsMode then
      Result := rmbPin
    else
      Result := rmbRestore;
end;

{ TdxRibbonMDIButtonViewInfo }

procedure TdxRibbonMDIButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawMDIButton(ACanvas, R, ButtonType, AState);
end;

procedure TdxRibbonMDIButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawMDIButtonGlyph(ACanvas, R, ButtonType, AState);
end;

procedure TdxRibbonMDIButtonViewInfo.Click;
begin
  if Ribbon.IsBarManagerValid and Enabled then
    Ribbon.BarManager.DoMDIButtonCommand(ButtonType);
end;

function TdxRibbonMDIButtonViewInfo.GetHint: string;
begin
  case FButtonType of
    mdibMinimize:
      Result := cxGetResourceString(@dxSBAR_MDIMINIMIZE);
    mdibRestore:
      Result := cxGetResourceString(@dxSBAR_MDIRESTORE);
    else // mdibClose:
      Result := cxGetResourceString(@dxSBAR_MDICLOSE);
  end;
end;

function TdxRibbonMDIButtonViewInfo.GetIsEnabled: Boolean;
begin
  Result := inherited GetIsEnabled and ViewInfo.IsMDIButtonEnabled(ButtonType, 0);
end;

{ TdxRibbonButtonsContainerViewInfo }

procedure TdxRibbonButtonsContainerViewInfo.Add(AClass: TdxRibbonCustomButtonViewInfoClass);
begin
  AddItem(AClass.Create(Self));
end;

procedure TdxRibbonButtonsContainerViewInfo.Calculate(const ABounds: TRect);
begin
  FIsRightToLeftConverted := False;
  FBounds := ABounds;
  CalculateButtonsBounds(FBounds);
  UpdateButtonsState;
end;

procedure TdxRibbonButtonsContainerViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Items[I].Bounds, ABounds);
end;

procedure TdxRibbonButtonsContainerViewInfo.UpdateButtonsState;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].CalculateState;
end;

procedure TdxRibbonButtonsContainerViewInfo.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Draw(ACanvas);
end;

function TdxRibbonButtonsContainerViewInfo.GetItem(Index: Integer): TdxRibbonCustomButtonViewInfo;
begin
  Result := TdxRibbonCustomButtonViewInfo(inherited Items[Index]);
end;

function TdxRibbonButtonsContainerViewInfo.GetSelectedButton: TdxRibbonCustomButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].AccessibilityHelper.IsSelected then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TdxRibbonButtonsContainerViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AProc(Items[I].AccessibilityHelper);
end;

function TdxRibbonButtonsContainerViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].GetHitInfo(AHitInfo);
    if Result then Break;
  end;
end;

{ TdxRibbonTabsAreaButtonsViewInfo }

procedure TdxRibbonTabsAreaButtonsViewInfo.CalculateButtonsBounds(const ABounds: TRect);
var
  I: Integer;
  R: TRect;
begin
  R := ABounds;
  R := cxRectSetRight(R, R.Right, 0);
  R := cxRectSetRight(R, R.Left, cxRectHeight(R));
  for I := Count - 1 downto 0 do
  begin
    Items[I].Bounds := R;
    R := cxRectOffset(R, -cxRectWidth(R), 0);
  end;
  FBounds := cxRectSetRight(ABounds, ABounds.Right, ABounds.Right - R.Right);
end;

procedure TdxRibbonTabsAreaButtonsViewInfo.Draw(ACanvas: TcxCanvas);
var
  ABitmap: TcxBitmap32;
begin
  if Owner.IsTabsOnGlass then
  begin
    ABitmap := TcxBitmap32.CreateSize(Bounds, True);
    try
      cxBitBlt(ABitmap.cxCanvas.Handle, ACanvas.Handle, ABitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
      ABitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
      inherited Draw(ABitmap.cxCanvas);
      ABitmap.cxCanvas.WindowOrg := cxNullPoint;
      cxBitBlt(ACanvas.Handle, ABitmap.cxCanvas.Handle, Bounds, cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end
  else
    inherited Draw(ACanvas);
end;

{ TdxRibbonContextViewInfo }

constructor TdxRibbonContextViewInfo.Create(AOwner: TdxRibbonContextsViewInfo; AContext: TdxRibbonContext);
begin
  inherited Create;
  FOwner := AOwner;
  FContext := AContext;
end;

procedure TdxRibbonContextViewInfo.Calculate(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  FIsRightToLeftConverted := False;
  FNeedShowHint := False;
  FBounds := ABounds;
  FTextBounds := Bounds;
  if Owner.Owner.SupportNonClientDrawing then
  begin
    FTextBounds := CalculateTextBounds(ACanvas, IfThen(IsPaintOnGlass, dxCaptionGlowRadius, dxRibbonTabsLeftSpace));
    FNeedShowHint := cxRectWidth(Bounds) < Owner.CalculateCaptionWidth(ACanvas, Context.Caption);
  end;
end;

function TdxRibbonContextViewInfo.CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect;
begin
  Result := Bounds;
  FText := cxGetStringAdjustedToWidth(ACanvas.Handle, 0, Context.Caption, cxRectWidth(Bounds) - 2 * AIndent);
end;

procedure TdxRibbonContextViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
  FTextBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTextBounds, ABounds);
end;

function TdxRibbonContextViewInfo.IsPaintOnGlass: Boolean;
begin
  Result := Owner.Owner.UseGlass;
end;

procedure TdxRibbonContextViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if IsPaintOnGlass then
    ColorScheme.DrawContextBackgroundGlass(ACanvas.Handle, Bounds, Context.Color)
  else
    ColorScheme.DrawContextBackground(ACanvas.Handle, Bounds, Context.Color);
end;

procedure TdxRibbonContextViewInfo.DrawText(ACanvas: TcxCanvas);
var
  ANeedClipping: Boolean;
begin
  ANeedClipping := TextBounds.Right > Bounds.Right;
  if ANeedClipping then
  begin
    ACanvas.SaveClipRegion;
    ACanvas.IntersectClipRect(Bounds);
  end;

  ACanvas.Font := Font;
  ACanvas.Brush.Style := bsClear;
  InternalDrawText(ACanvas, cxRectOffset(TextBounds, 1, 1), FontShadowColor, True);
  InternalDrawText(ACanvas, TextBounds, Font.Color, FontShadowColor = clNone);
  ACanvas.Brush.Style := bsSolid;

  if ANeedClipping then
    ACanvas.RestoreClipRegion;
end;

procedure TdxRibbonContextViewInfo.InternalDrawText(
  ACanvas: TcxCanvas; const R: TRect; ATextColor: TColor; AHasGlowing: Boolean);
var
  AFlags: Cardinal;
begin
  if ATextColor <> clNone then
  begin
    if IsPaintOnGlass or (Context.Alpha < MaxByte) then
    begin
      AFlags := DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER or DT_NOPREFIX;
      if FContext.Ribbon.UseRightToLeftReading then
        AFlags := AFlags or DT_RTLREADING;
      dxDrawTextOnGlass(ACanvas.Handle, Text, Font, R, ATextColor, AFlags, IfThen(AHasGlowing, dxCaptionGlowRadius), True);
    end
    else
    begin
      AFlags := cxAlignHCenter or cxShowEndEllipsis or cxAlignVCenter or cxSingleLine;
      if FContext.Ribbon.UseRightToLeftReading then
        AFlags := AFlags or cxRtlReading;
      ACanvas.Font.Color := ATextColor;
      ACanvas.DrawText(Text, R, AFlags);
    end;
  end;
end;

procedure TdxRibbonContextViewInfo.Paint(ACanvas: TcxCanvas);
var
  ABitmap: TcxBitmap32;
begin
  if Context.Alpha < MaxByte then
  begin
    ABitmap := TcxBitmap32.CreateSize(Bounds, True);
    try
      ABitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
      DrawBackground(ABitmap.cxCanvas);
      DrawText(ABitmap.cxCanvas);
      cxAlphaBlend(ACanvas.Handle, ABitmap, Bounds, ABitmap.ClientRect, False, Context.Alpha);
    finally
      ABitmap.Free;
    end;
  end
  else
  begin
    DrawBackground(ACanvas);
    DrawText(ACanvas);
  end;
end;

function TdxRibbonContextViewInfo.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Owner.Owner.ColorScheme;
end;

function TdxRibbonContextViewInfo.GetFont: TFont;
begin
  Result := Owner.Ribbon.Fonts.GetContextFont(Context.Color);
end;

function TdxRibbonContextViewInfo.GetFontShadowColor: TColor;
const
  PartsMap: array[Boolean] of Integer = (rspContextTextShadow, rspContextTextOnGlassShadow);
begin
  Result := ColorScheme.GetPartColor(PartsMap[IsPaintOnGlass]);
end;

procedure TdxRibbonContextViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TdxRibbonContextViewInfo2007 }

function TdxRibbonContextViewInfo2007.CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect;
var
  ATextSize: TSize;
begin
  Result := inherited;
  ATextSize := ACanvas.TextExtent(Text);
  Result.Right := Result.Left + ATextSize.cx + 2 * AIndent;
  Result.Top := Result.Bottom - ATextSize.cy - dxRibbonTabsLeftSpace;
end;

{ TdxRibbonContextViewInfo2010OrNewer }

function TdxRibbonContextViewInfo2010OrNewer.CalculateTextBounds(ACanvas: TcxCanvas; AIndent: Integer): TRect;
begin
  Result := inherited;
  Result.Right := Bounds.Right;
  Result.Top := Result.Bottom - ACanvas.TextHeight(Text) - 13;
end;

{ TdxRibbonContextsViewInfo }

procedure TdxRibbonContextsViewInfo.Calculate(const ABounds: TRect);
var
  ACanvas: TcxCanvas;
  AContextViewInfo: TdxRibbonContextViewInfo;
  AMinWidth: Integer;
  ATabViewInfo: TdxRibbonTabViewInfo;
  I: Integer;
  R: TRect;
begin
  FIsRightToLeftConverted := False;
  R := ABounds;
  if not Owner.UseGlass then
    R := cxRectOffset(R, 0, -1);

  ACanvas := cxScreenCanvas;
  try
    ACanvas.Font := GetContextFont;
    AMinWidth := ScaleFactor.Apply(dxRibbonFormContextsMinWidth);
    for I := 0 to Owner.TabsAreaViewInfo.TabsViewInfo.Count - 1 do
    begin
      ATabViewInfo := Owner.TabsAreaViewInfo.TabsViewInfo[I];
      if ATabViewInfo.ContextBegin then
        R.Left := ATabViewInfo.Bounds.Left;
      if R.Left >= ABounds.Right then
        Break;
      if ATabViewInfo.ContextEnd then
      begin
        R.Right := Min(ATabViewInfo.Bounds.Right, ABounds.Right);
        if cxRectWidth(R) > AMinWidth then
        begin
          AContextViewInfo := CreateContextViewInfo(ATabViewInfo.Tab.Context);
          AContextViewInfo.Calculate(ACanvas, R);
          if AddItem(AContextViewInfo) = 0 then
            FBounds.Left := R.Left;
          FBounds.Right := R.Right;
        end
        else
          Break;
      end;
    end;
  finally
    cxScreenCanvas.Dormant;
  end;

  if Count > 0 then
  begin
    FBounds.Top := ABounds.Top;
    FBounds.Bottom := ABounds.Bottom;
  end
  else
    FBounds := cxNullRect;
end;

function TdxRibbonContextsViewInfo.CalculateCaptionWidth(ACanvas: TcxCanvas; const ACaption: string): Integer;
begin
  Result := 0;
  if Owner.SupportNonClientDrawing then
  begin
    ACanvas.Font := GetContextFont;
    Result := ACanvas.TextWidth(ACaption);
    if Owner.UseGlass then
      Inc(Result, 2 * dxCaptionGlowRadius)
    else
      Inc(Result, 2 * dxRibbonTabsLeftSpace);
  end;
end;

function TdxRibbonContextsViewInfo.NeedShowHint(AContext: TdxRibbonContext): Boolean;
var
  AItem: TdxRibbonContextViewInfo;
begin
  AItem := Find(AContext);
  Result := (AItem <> nil) and AItem.NeedShowHint;
end;

procedure TdxRibbonContextsViewInfo.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Paint(ACanvas);
end;

function TdxRibbonContextsViewInfo.GetContextFont(AColor: TColor): TFont;
begin
  Result := Ribbon.Fonts.GetContextFont(AColor);
end;

function TdxRibbonContextsViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if cxRectPtIn(Items[I].Bounds, AHitInfo.HitPoint) then
    begin
      AHitInfo.HitTest := rhtContext;
      AHitInfo.HitObject := Items[I].Context;
      Exit(True);
    end;

  Result := False;
end;

function TdxRibbonContextsViewInfo.Find(AContext: TdxRibbonContext): TdxRibbonContextViewInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Context = AContext then
      Exit(Items[I]);
  Result := nil;
end;

function TdxRibbonContextsViewInfo.CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo;
begin
  Result := TdxRibbonContextViewInfo.Create(Self, AContext);
end;

procedure TdxRibbonContextsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TdxRibbonContextsViewInfo.GetItem(Index: Integer): TdxRibbonContextViewInfo;
begin
  Result := TdxRibbonContextViewInfo(inherited Items[Index]);
end;

{ TdxRibbonContextsViewInfo2007 }

function TdxRibbonContextsViewInfo2007.CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo;
begin
  Result := TdxRibbonContextViewInfo2007.Create(Self, AContext);
end;

{ TdxRibbonContextsViewInfo2010OrNewer }

function TdxRibbonContextsViewInfo2010OrNewer.CreateContextViewInfo(AContext: TdxRibbonContext): TdxRibbonContextViewInfo;
begin
  Result := TdxRibbonContextViewInfo2010OrNewer.Create(Self, AContext);
end;

{ TdxRibbonApplicationButtonViewInfo }

destructor TdxRibbonApplicationButtonViewInfo.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

procedure TdxRibbonApplicationButtonViewInfo.Calculate(const ABounds: TRect);
var
  AIconSize: TSize;
begin
  inherited Calculate(ABounds);

  if ApplicationButton.Glyph.Empty then
    AIconSize := cxSize(dxGetSystemMetrics(SM_CXSMICON, ScaleFactor), dxGetSystemMetrics(SM_CYSMICON, ScaleFactor))
  else
    AIconSize := dxGetImageSize(ApplicationButton.Glyph, ScaleFactor);

  FGlyphBounds := cxRectContent(ClientBounds, GlyphOffsets);
  if ApplicationButton.StretchGlyph then
    AIconSize := cxSize(cxRectProportionalStretch(FGlyphBounds, AIconSize));
  FGlyphBounds := cxRectCenter(FGlyphBounds, AIconSize);
end;

procedure TdxRibbonApplicationButtonViewInfo.CalculateMetrics(ACanvas: TcxCanvas);
begin
  FClientOffsets := Ribbon.SkinGetContentOffsets(DXBAR_APPLICATIONBUTTON);
  FGlyphOffsets := Ribbon.SkinGetContentOffsets(DXBAR_APPLICATIONBUTTONICONOFFSET);
end;

procedure TdxRibbonApplicationButtonViewInfo.Draw(ACanvas: TcxCanvas);
var
  B: TcxBitmap32;
begin
  if ACanvas.RectVisible(Bounds) then
  begin
    if IsPaintOnGlass then
    begin
      B := TcxBitmap32.CreateSize(Bounds, True);
      try
        B.Canvas.Lock;
        try
          cxBitBlt(B.Canvas.Handle, ACanvas.Handle, B.ClientRect, Bounds.TopLeft, SRCCOPY);
          B.cxCanvas.WindowOrg := Bounds.TopLeft;
          DrawBackground(B.cxCanvas);
          DrawContent(B.cxCanvas);
          B.cxCanvas.WindowOrg := cxNullPoint;
          cxBitBlt(ACanvas.Handle, B.Canvas.Handle, Bounds, cxNullPoint, SRCCOPY);
        finally
          B.Canvas.Unlock;
        end;
      finally
        B.Free;
      end;
    end
    else
    begin
      DrawBackground(ACanvas);
      DrawContent(ACanvas);
    end;
  end;
end;

procedure TdxRibbonApplicationButtonViewInfo.InvalidateRect(const R: TRect);
begin
  if IsVisible and not Ribbon.IsDestroying then
    inherited InvalidateRect(R);
end;

function TdxRibbonApplicationButtonViewInfo.IsPaintOnGlass: Boolean;
begin
  Result := Owner.IsTabsOnGlass;
end;

function TdxRibbonApplicationButtonViewInfo.IsVisible: Boolean;
begin
  Result := ApplicationButton.Visible and Owner.IsTabsVisible and
    (Owner.SupportNonClientDrawing or Owner.QuickAccessToolbarViewInfo.AtTop);
end;

function TdxRibbonApplicationButtonViewInfo.MeasureSize: TSize;
begin
  Result := MeasureSizeCore;
  dxAdjustToTouchableSize(Result.cx, ScaleFactor);
  Inc(Result.cx, cxMarginsWidth(ClientOffsets));
  Inc(Result.cy, cxMarginsHeight(ClientOffsets));
end;

procedure TdxRibbonApplicationButtonViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  AProc(ApplicationButton.IAccessibilityHelper);
end;

function TdxRibbonApplicationButtonViewInfo.CanFade: Boolean;
begin
  Result := Ribbon.CanFade and IsVisible and not (IsPressed or Ribbon.Controller.IsApplicationMenuDropped);
end;

function TdxRibbonApplicationButtonViewInfo.GetFadingOptions: TdxFadingOptions;
begin
  Result := Ribbon.OptionsFading.ApplicationButton;
end;

procedure TdxRibbonApplicationButtonViewInfo.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
begin
  AFadeInImage := TcxBitmap32.CreateSize(ClientBounds, True);
  AFadeOutImage := TcxBitmap32.CreateSize(ClientBounds, True);
  Painter.DrawApplicationButton(AFadeInImage.cxCanvas, AFadeInImage.ClientRect, rabsHot);
  Painter.DrawApplicationButton(AFadeOutImage.cxCanvas, AFadeOutImage.ClientRect, rabsNormal);
end;

procedure TdxRibbonApplicationButtonViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FGlyphBounds := TdxRightToLeftLayoutConverter.ConvertRect(FGlyphBounds, ABounds);
end;

procedure TdxRibbonApplicationButtonViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  if not dxFader.DrawFadeImage(Self, ACanvas.Handle, ClientBounds) then
    Painter.DrawApplicationButton(ACanvas, ClientBounds, State);
end;

procedure TdxRibbonApplicationButtonViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  Painter.DrawApplicationButtonGlyph(ACanvas, GlyphBounds, ApplicationButton.Glyph);
end;

function TdxRibbonApplicationButtonViewInfo.MeasureSizeCore: TSize;
begin
  Result := Ribbon.ColorScheme.GetApplicationMenuGlyphSize;
end;

function TdxRibbonApplicationButtonViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := IsVisible and cxRectPtIn(ClientBounds, AHitInfo.HitPoint);
  if Result then
    AHitInfo.HitTest := rhtApplicationMenu;
end;

function TdxRibbonApplicationButtonViewInfo.GetApplicationButton: TdxRibbonApplicationButton;
begin
  Result := Ribbon.ApplicationButton;
end;

function TdxRibbonApplicationButtonViewInfo.GetClientBounds: TRect;
var
  AOffsets: TRect;
begin
  if FIsRightToLeftConverted then
    AOffsets := TdxRightToLeftLayoutConverter.ConvertOffsets(ClientOffsets)
  else
    AOffsets := ClientOffsets;
  Result := cxRectContent(Bounds, AOffsets);
end;

function TdxRibbonApplicationButtonViewInfo.GetFont: TFont;
begin
  Result := Ribbon.Fonts.GetApplicationButtonFont(State);
end;

function TdxRibbonApplicationButtonViewInfo.GetPainter: TdxRibbonPainter;
begin
  Result := Ribbon.Painter;
end;

function TdxRibbonApplicationButtonViewInfo.GetState: TdxRibbonApplicationButtonState;
begin
  if ApplicationButton.IAccessibilityHelper.IsSelected then
    Result := rabsHot
  else
    if IsPressed or Ribbon.Controller.IsApplicationMenuDropped then
      Result := rabsPressed
    else
      if IsHot then
        Result := rabsHot
      else
        Result := rabsNormal;
end;

procedure TdxRibbonApplicationButtonViewInfo.SetIsHot(AValue: Boolean);
begin
  if AValue <> FIsHot then
  begin
    if CanFade then
    begin
      if AValue then
        dxFader.FadeIn(Self)
      else
        dxFader.FadeOut(Self);
    end;
    FIsHot := AValue;
    Invalidate;
  end;
end;

procedure TdxRibbonApplicationButtonViewInfo.SetIsPressed(AValue: Boolean);
begin
  if AValue <> FIsPressed then
  begin
    FIsPressed := AValue;
    Invalidate;
  end;
end;

{ TdxRibbonApplicationButtonViewInfo2010OrNewer }

procedure TdxRibbonApplicationButtonViewInfo2010OrNewer.CalculateMetrics(ACanvas: TcxCanvas);
begin
  inherited;

  if ApplicationButton.Text <> '' then
  begin
    ACanvas.Font := Font;
    FTextSize := cxTextSize(ACanvas.Handle, ApplicationButton.Text);
  end
  else
    FTextSize := cxNullSize;
end;

procedure TdxRibbonApplicationButtonViewInfo2010OrNewer.DrawContent(ACanvas: TcxCanvas);
begin
  if HasText then
    DrawText(ACanvas)
  else
    inherited;
end;

procedure TdxRibbonApplicationButtonViewInfo2010OrNewer.DrawText(ACanvas: TcxCanvas);
var
  AFlags: Cardinal;
begin
  if IsPaintOnGlass then
  begin
    AFlags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
    if Ribbon.UseRightToLeftReading then
      AFlags := AFlags or DT_RTLREADING;
    dxDrawTextOnGlass(ACanvas.Handle, ApplicationButton.Text, Font,
      ClientBounds, Font.Color, AFlags, 0, True);
  end
  else
  begin
    ACanvas.Font := Font;
    ACanvas.Brush.Style := bsClear;
    if Ribbon.UseRightToLeftReading then
      ACanvas.TextFlags := ACanvas.TextFlags or ETO_RTLREADING;
    ACanvas.DrawTexT(ApplicationButton.Text, TextBounds, taCenter, vaCenter, False, False);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

function TdxRibbonApplicationButtonViewInfo2010OrNewer.HasText: Boolean;
begin
  Result := FTextSize.cx > 0;
end;

function TdxRibbonApplicationButtonViewInfo2010OrNewer.MeasureSizeCore: TSize;
begin
  Result := inherited;
  if HasText then
  begin
    Result.cx := Max(Result.cx, FTextSize.cx + cxMarginsWidth(GlyphOffsets));
    Result.cy := Max(Result.cy, FTextSize.cy);
  end
end;

function TdxRibbonApplicationButtonViewInfo2010OrNewer.GetTextBounds: TRect;
begin
  Result := cxRectContent(ClientBounds, TextOffsets);
end;

function TdxRibbonApplicationButtonViewInfo2010OrNewer.GetTextOffsets: TRect;
begin
  Result := cxRect(0, ScaleFactor.Apply(3), 0, 0);
end;

{ TdxRibbonTabsAreaViewInfo }

constructor TdxRibbonTabsAreaViewInfo.Create(AOwner: TdxRibbonViewInfo);
begin
  inherited;
  FTabs := CreateTabsViewInfo;
  FButtonsViewInfo := CreateButtonsViewInfo;
end;

destructor TdxRibbonTabsAreaViewInfo.Destroy;
begin
  FreeAndNil(FButtonsViewInfo);
  FreeAndNil(FTabs);
  inherited;
end;

procedure TdxRibbonTabsAreaViewInfo.Calculate(const ABounds: TRect);
var
  ARect: TRect;
begin
  inherited Calculate(ABounds);

  ARect := ABounds;
  ARect.Left := Max(Owner.ApplicationButtonViewInfo.Bounds.Right, dxRibbonTabsLeftSpace);
  CalculateContent(ARect);
  FExtendedCaptionAreaRect := ARect;

  if not Ribbon.UseRightToLeftAlignment then
    UpdateToolbarDockControls;
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateMetrics(ACanvas: TcxCanvas);
begin
  TabsViewInfo.CalculateMetrics(ACanvas);
  FMeasuredHeight := MeasureHeight(ACanvas);
end;

procedure TdxRibbonTabsAreaViewInfo.Draw(ACanvas: TcxCanvas);
begin
  ButtonsViewInfo.Draw(ACanvas);
  if IsTabsVisible then
    TabsViewInfo.Draw(ACanvas);
  cxRedrawWindow(SearchToolbar.DockControl.Handle, RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

function TdxRibbonTabsAreaViewInfo.GetDesignHitTest(const P: TPoint): Boolean;
begin
  Result := TabsViewInfo.GetDesignHitTest(P);
end;

function TdxRibbonTabsAreaViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
begin
  Result := TabsViewInfo.GetHitInfo(AHitInfo) or ButtonsViewInfo.GetHitInfo(AHitInfo);
end;

function TdxRibbonTabsAreaViewInfo.IsSearchToolbarVisible: Boolean;
begin
  Result := Owner.CanShowBarControls and IsTabsVisible and SearchToolbar.ActualVisible
end;

function TdxRibbonTabsAreaViewInfo.IsTabsVisible: Boolean;
begin
  Result := Owner.IsTabsVisible;
end;

function TdxRibbonTabsAreaViewInfo.IsToolbarVisible: Boolean;
begin
  Result := Owner.CanShowBarControls and IsTabsVisible and Toolbar.ActualVisible;
end;

function TdxRibbonTabsAreaViewInfo.IsToolbarVisibleOnBackstageView: Boolean;
begin
  Result := IsToolbarVisible;
end;

procedure TdxRibbonTabsAreaViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  TabsViewInfo.PopulateHorizontalNavigationList(AProc);
  dxRibbonAccessibilityEnumChildren(SearchToolbar.IAccessibilityHelper, AProc);
  dxRibbonAccessibilityEnumChildren(Toolbar.IAccessibilityHelper, AProc);
  ButtonsViewInfo.PopulateHorizontalNavigationList(AProc);
end;

procedure TdxRibbonTabsAreaViewInfo.RecreateViewInfoCells;
begin
  ButtonsViewInfo.Clear;
  CreateButtons;
  TabsViewInfo.RecreateViewInfoCells;
end;

procedure TdxRibbonTabsAreaViewInfo.UpdateButtonsState;
begin
  ButtonsViewInfo.UpdateButtonsState;
  TabsViewInfo.UpdateButtonsStates;
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateContent(var ARect: TRect);
begin
  CalculateTabsAreaButtons(ARect);
  CalculateTabsAreaToolbar(ARect);
  CalculateTabs(ARect);
  CalculateSearchToolbar(ARect);
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateSearchToolbar(var ARect: TRect);
var
  ASize: TSize;
begin
  FSearchToolbarBounds := cxNullRect;
  if IsSearchToolbarVisible then
  begin
    ASize := CalculateToolbarSize(SearchToolbar.Toolbar, cxRectWidth(ARect));
    if ASize.cx <= cxRectWidth(ARect) then
    begin
      FSearchToolbarBounds := cxRectCenterVertically(ARect, ASize.cy);
      FSearchToolbarBounds := cxRectSetWidth(SearchToolbarBounds, ASize.cx);
      ARect.Right := SearchToolbarBounds.Left;
    end;
  end;
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateTabs(var ARect: TRect);
begin
  if IsTabsVisible and (TabsViewInfo.Count > 0) then
  begin
    TabsViewInfo.Calculate(ARect);
    ARect.Left := TabsViewInfo.Bounds.Right;
  end;
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateTabsAreaButtons(var ARect: TRect);
begin
  ButtonsViewInfo.Calculate(cxRectSetHeight(ARect, MeasuredHeight - dxRibbonTabsAreaBottomIndent));
  ARect.Right := Min(ButtonsViewInfo.Bounds.Left - 1, ARect.Right - dxRibbonTabsRightSpace);
end;

procedure TdxRibbonTabsAreaViewInfo.CalculateTabsAreaToolbar(var ARect: TRect);
var
  ASize: TSize;
begin
  FToolbarBounds := cxNullRect;
  if IsToolbarVisible then
  begin
    ASize := CalculateToolbarSize(Toolbar.Toolbar, cxRectWidth(ARect) div 2);
    FToolbarBounds := cxRectCenterVertically(ARect, ASize.cy);
    FToolbarBounds := cxRectSetRight(ToolbarBounds, ToolbarBounds.Right, ASize.cx);
    ARect.Right := ToolbarBounds.Left;
  end;
end;

function TdxRibbonTabsAreaViewInfo.CalculateToolbarSize(ABar: TdxBar; AAvailableWidth: Integer): TSize;
var
  AControl: TdxBarControl;
begin
  AControl := ABar.Control;
  if AControl is TdxRibbonCustomToolbarBarControl then
    Result := TdxRibbonCustomToolbarBarControl(AControl).GetSize(AAvailableWidth)
  else
    Result := cxNullSize;
end;

procedure TdxRibbonTabsAreaViewInfo.CreateButtons;
var
  AButton: TdxBarMDIButton;
  AButtonViewInfo: TdxRibbonMDIButtonViewInfo;
begin
  if Ribbon.IsHelpButtonPlacedOnTabsArea then
    ButtonsViewInfo.Add(TdxRibbonHelpButtonViewInfo);
  if Owner.HasMDIButtons then
    for AButton := Low(AButton) to High(AButton) do
    begin
      AButtonViewInfo := TdxRibbonMDIButtonViewInfo.Create(ButtonsViewInfo);
      AButtonViewInfo.FButtonType := AButton;
      ButtonsViewInfo.AddItem(AButtonViewInfo);
    end;
end;

function TdxRibbonTabsAreaViewInfo.CreateButtonsViewInfo: TdxRibbonTabsAreaButtonsViewInfo;
begin
  Result := TdxRibbonTabsAreaButtonsViewInfo.Create(Owner);
end;

function TdxRibbonTabsAreaViewInfo.CreateTabsViewInfo: TdxRibbonTabsViewInfo;
begin
  Result := TdxRibbonTabsViewInfo.Create(Owner);
end;

procedure TdxRibbonTabsAreaViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);

  ButtonsViewInfo.RightToLeftConversion(ABounds);
  FToolbarBounds := TdxRightToLeftLayoutConverter.ConvertRect(FToolbarBounds, ABounds);
  if IsTabsVisible and (TabsViewInfo.Count > 0) then
    TabsViewInfo.RightToLeftConversion(ABounds);
  FSearchToolbarBounds := TdxRightToLeftLayoutConverter.ConvertRect(FSearchToolbarBounds, ABounds);
  FExtendedCaptionAreaRect := TdxRightToLeftLayoutConverter.ConvertRect(FExtendedCaptionAreaRect, ABounds);
end;

function TdxRibbonTabsAreaViewInfo.MeasureHeight(ACanvas: TcxCanvas): Integer;

  function GetBarHeight(ABar: TdxRibbonCustomToolbar): Integer;
  var
    AControl: TdxBarControl;
  begin
    AControl := ABar.Toolbar.Control;
    if AControl is TdxRibbonCustomToolbarBarControl then
      Result := TdxRibbonCustomToolbarBarControl(AControl).GetMinHeight(dsTop)
    else
      Result := 0;
  end;

begin
  Result := 0;
  if IsTabsVisible then
    Result := Max(Result, TabsViewInfo.MeasureHeight(ACanvas));
  if IsSearchToolbarVisible then
    Result := Max(Result, GetBarHeight(SearchToolbar) + dxRibbonTabsAreaBottomIndent);
  if IsToolbarVisible then
    Result := Max(Result, GetBarHeight(Toolbar) + dxRibbonTabsAreaBottomIndent);
end;

procedure TdxRibbonTabsAreaViewInfo.UpdateToolbarDockControls;
begin
  Owner.UpdateToolbarDockControl(Toolbar.DockControl, not cxRectIsEmpty(ToolbarBounds), ToolbarBounds);
  Owner.UpdateToolbarDockControl(SearchToolbar.DockControl, not cxRectIsEmpty(SearchToolbarBounds), SearchToolbarBounds);
end;

function TdxRibbonTabsAreaViewInfo.GetSearchToolbar: TdxRibbonCustomToolbar;
begin
  Result := Ribbon.TabAreaSearchToolbar;
end;

function TdxRibbonTabsAreaViewInfo.GetToolbar: TdxRibbonCustomToolbar;
begin
  Result := Ribbon.TabAreaToolbar;
end;

{ TdxRibbonTabsAreaViewInfo2010OrNewer }

procedure TdxRibbonTabsAreaViewInfo2010OrNewer.Calculate(const ABounds: TRect);
var
  R: TRect;
begin
  if ApplicationButtonViewInfo.IsVisible then
    R := cxRectSetWidth(ABounds, ApplicationButtonViewInfo.MeasureSize.cx)
  else
    R := cxNullRect;

  ApplicationButtonViewInfo.Calculate(R);
  inherited Calculate(ABounds);
end;

procedure TdxRibbonTabsAreaViewInfo2010OrNewer.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  ApplicationButtonViewInfo.RightToLeftConversion(ABounds);
end;

function TdxRibbonTabsAreaViewInfo2010OrNewer.MeasureHeight(ACanvas: TcxCanvas): Integer;
begin
  Result := inherited;
  if ApplicationButtonViewInfo.IsVisible and ApplicationButtonViewInfo.HasText then
    Result := Max(Result, ApplicationButtonViewInfo.FTextSize.cy + cxMarginsHeight(Owner.Painter.GetTabContentOffset));
end;

function TdxRibbonTabsAreaViewInfo2010OrNewer.GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo2010OrNewer;
begin
  Result := Owner.ApplicationButtonViewInfo as TdxRibbonApplicationButtonViewInfo2010OrNewer;
end;

{ TdxRibbonTabsAreaViewInfo2010 }

procedure TdxRibbonTabsAreaViewInfo2010.CreateButtons;
begin
  if Ribbon.ShowMinimizeButton then
    ButtonsViewInfo.Add(TdxRibbonMinimizeButtonViewInfo);
  inherited CreateButtons;
end;

{ TdxRibbonTabsAreaViewInfo2016Tablet }

procedure TdxRibbonTabsAreaViewInfo2016Tablet.CalculateContent(var ARect: TRect);
begin
  if QuickAccessToolbarViewInfo.AtTop then
  begin
    QuickAccessToolbarViewInfo.Calculate(ARect);
    ARect.Left := Max(ARect.Left, QuickAccessToolbarViewInfo.Bounds.Right +
      Ribbon.ColorScheme.GetQuickAccessToolbarRightIndent(Ribbon.ViewInfo.IsApplicationButtonNearQAT) +
      TabsViewInfo.FTabContentOffset.Left);
  end;
  inherited;
end;

function TdxRibbonTabsAreaViewInfo2016Tablet.CreateTabsViewInfo: TdxRibbonTabsViewInfo;
begin
  Result := TdxRibbonTabsViewInfo2016Tablet.Create(Owner);
end;

function TdxRibbonTabsAreaViewInfo2016Tablet.GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
begin
  Result := Owner.QuickAccessToolbarViewInfo;
end;

{ TdxRibbonTabsAreaViewInfo2019 }

function TdxRibbonTabsAreaViewInfo2019.CreateTabsViewInfo: TdxRibbonTabsViewInfo;
begin
  Result := TdxRibbonTabsViewInfo2019.Create(Owner);
end;

function TdxRibbonTabsAreaViewInfo2019.IsToolbarVisibleOnBackstageView: Boolean;
begin
  Result := False;
end;

{ TdxRibbonQuickAccessToolbarViewInfo }

procedure TdxRibbonQuickAccessToolbarViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);

  if Visible then
  begin
    CalculateControlSize(cxRectWidth(ABounds) - cxMarginsWidth(DockControlOffsets));

    FBounds := cxRectSetHeight(Bounds, ControlSize.cy + cxMarginsHeight(DockControlOffsets));
    if AtTop then
      FBounds := cxRectSetWidth(Bounds, ControlSize.cx + cxMarginsWidth(DockControlOffsets));

    FDockControlBounds := cxRectContent(Bounds, DockControlOffsets);
    FDockControlBounds := cxRectSetWidth(DockControlBounds, ControlSize.cx);

    if Toolbar.Toolbar.Control <> nil then
      Toolbar.Toolbar.Control.Enabled := Ribbon.ApplicationMenuState <= ramsShownAsMenu;
  end
  else
  begin
    FBounds := cxNullRect;
    FDockControlBounds := cxNullRect;
  end;

  if not Ribbon.UseRightToLeftAlignment then
    Owner.UpdateToolbarDockControl(DockControl, Visible, DockControlBounds);
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.CalculateMetrics(ACanvas: TcxCanvas);
begin
  inherited;
  FVisible := Owner.CanShowBarControls and Toolbar.ActualVisible;
  FAtTop := Visible and (Toolbar.Position = qtpAboveRibbon);
  FAtBottom := Visible and (Toolbar.Position = qtpBelowRibbon);
  FAtNonClientArea := AtTop and IsNonClientDrawingSupported;

  CalculateControlSize(cxRectWidth(Owner.Bounds));
  CalculateDockControlOffsets;
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if Visible then
  begin
    ColorScheme.UseRightToLeftAlignment := Ribbon.UseRightToLeftAlignment;
    ColorScheme.DrawQuickAccessToolbar(ACanvas.Handle, Bounds, AtBottom,
      AtNonClientArea, Owner.IsApplicationButtonNearQAT, IsActive, not IsPaintOnGlass);
    ColorScheme.UseRightToLeftAlignment := False;
  end;
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(Bounds);
    Owner.Painter.DrawBackground(ACanvas, Owner.Bounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxRibbonQuickAccessToolbarViewInfo.IsActive: Boolean;
begin
  Result := not AtNonClientArea or Owner.Painter.GetRibbonFormPaintData.GetIsActive;
end;

function TdxRibbonQuickAccessToolbarViewInfo.IsNonClientDrawingSupported: Boolean;
begin
  Result := Owner.SupportNonClientDrawing;
end;

function TdxRibbonQuickAccessToolbarViewInfo.IsPaintOnGlass: Boolean;
begin
  Result := AtNonClientArea and Owner.UseGlass;
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.CalculateControlSize(AAvailableWidth: Integer);
var
  AControl: TdxBarControl;
begin
  FControlSize := cxNullSize;
  if Visible then
  begin
    AControl := Toolbar.Toolbar.Control;
    if AControl is TdxRibbonQuickAccessToolbarBarControl then
    begin
      FControlSize := TdxRibbonQuickAccessToolbarBarControl(AControl).GetSize(AAvailableWidth);
      if Ribbon.ApplicationMenuState = ramsShownAsFullScreenFrame then
        FControlSize.cx := 0
    end;
  end;
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.CalculateDockControlOffsets;
var
  AHeightDelta: Integer;
begin
  if AtTop then
  begin
    FDockControlOffsets := cxNullRect;
    FDockControlOffsets.Left := 1 + ColorScheme.GetQuickAccessToolbarOverrideWidth(Owner.IsApplicationButtonNearQAT, IsPaintOnGlass);
    if (ControlSize.cx <> 0) and (Ribbon.GetValidPopupMenuItems = []) then
    begin
      Inc(FDockControlOffsets.Right, ((ControlSize.cy + 2) div 2) or 1);
      Inc(FDockControlOffsets.Right, 12);
    end;
    if AtNonClientArea then
    begin
      AHeightDelta := Owner.GetNonClientAreaHeight - ControlSize.cy;
      FDockControlOffsets.Bottom := AHeightDelta div 2;
      FDockControlOffsets.Top := AHeightDelta - DockControlOffsets.Bottom;
    end
    else
    begin
      FDockControlOffsets.Top := 4;
      FDockControlOffsets.Bottom := 5;
    end;
  end
  else
    FDockControlOffsets := Ribbon.SkinGetContentOffsets(DXBAR_QUICKACCESSTOOLBAR);
end;

procedure TdxRibbonQuickAccessToolbarViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FDockControlBounds := TdxRightToLeftLayoutConverter.ConvertRect(FDockControlBounds, ABounds);
  if (Toolbar.Toolbar <> nil) and (Toolbar.Toolbar.Control <> nil) then
    Toolbar.Toolbar.Control.BiDiMode := bdRightToLeft;
  Owner.UpdateToolbarDockControl(DockControl, Visible, DockControlBounds);
end;

function TdxRibbonQuickAccessToolbarViewInfo.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Ribbon.ColorScheme;
end;

function TdxRibbonQuickAccessToolbarViewInfo.GetDockControl: TdxRibbonQuickAccessToolbarDockControl;
begin
  Result := TdxRibbonQuickAccessToolbarDockControl(Toolbar.DockControl);
end;

function TdxRibbonQuickAccessToolbarViewInfo.GetMeasuredHeight: Integer;
begin
  if Visible then
    Result := ControlSize.cy + cxMarginsHeight(DockControlOffsets)
  else
    Result := 0;
end;

function TdxRibbonQuickAccessToolbarViewInfo.GetToolbar: TdxRibbonQuickAccessToolbar;
begin
  Result := Ribbon.QuickAccessToolbar;
end;

{ TdxRibbonQuickAccessToolbarViewInfo2016Tablet }

procedure TdxRibbonQuickAccessToolbarViewInfo2016Tablet.DrawBackground(ACanvas: TcxCanvas);
begin
  Owner.DrawRibbonBackground(ACanvas);
end;

function TdxRibbonQuickAccessToolbarViewInfo2016Tablet.IsNonClientDrawingSupported: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonQuickAccessToolbarViewInfo2016Tablet.CalculateDockControlOffsets;
begin
  if AtTop then
    FDockControlOffsets := cxNullRect
  else
    inherited;
end;

{ TdxRibbonViewInfo }

constructor TdxRibbonViewInfo.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FFont := TFont.Create;
  FTabsAreaViewInfo := CreateTabsAreaViewInfo;
  FContextsViewInfo := CreateContextsViewInfo;
  FQuickAccessToolbarViewInfo := CreateQuickAccessToolbarViewInfo;
  FGroupsDockControlSiteViewInfo := TdxRibbonGroupsDockControlSiteViewInfo.Create(Self);
  FApplicationButtonViewInfo := CreateApplicationButtonViewInfo;
end;

destructor TdxRibbonViewInfo.Destroy;
begin
  FreeAndNil(FContextsViewInfo);
  FreeAndNil(FTabsAreaViewInfo);
  FreeAndNil(FApplicationButtonViewInfo);
  FreeAndNil(FQuickAccessToolbarViewInfo);
  FreeAndNil(FGroupsDockControlSiteViewInfo);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxRibbonViewInfo.Calculate;
begin
  FIsRightToLeftConverted := False;
  FBounds := GetBounds;
  UpdateNonClientParams;
  Ribbon.Fonts.UpdateFonts;
  if Hidden <> IsNeedHideControl then
  begin
    FHidden := IsNeedHideControl;
    if Hidden and Ribbon.IsAutoHidden then
      Ribbon.ShowTabGroups := True;
    Calculate;
    if Hidden then
    begin
      Ribbon.UpdateHiddenActiveTabDockControl;
      Ribbon.RibbonFormInvalidate;
    end;
  end
  else
  begin
    Ribbon.DisableAlign;
    try
      RecreateViewInfoCells;
      CalculateMetrics;
      CalculateBounds;
      if Ribbon.UseRightToLeftAlignment then
        RightToLeftConversion(Bounds);
      Ribbon.UpdateHeight;
    finally
      Ribbon.EnableAlign;
    end;
    Ribbon.UpdateHorizontalNavigationList;
  end;
end;

procedure TdxRibbonViewInfo.CalculateHitInfo(AHitTest: TdxRibbonHitInfo);
var
  APrevPoint: TPoint;
begin
  if TabsAreaViewInfo.GetHitInfo(AHitTest) then
    Exit;
  if ContextsViewInfo.GetHitInfo(AHitTest) then
    Exit;
  if ApplicationButtonViewInfo.GetHitInfo(AHitTest) then
    Exit;

  APrevPoint := AHitTest.FHitPoint;
  try
    AHitTest.FHitPoint := dxMapWindowPoint(Ribbon.Handle, GroupsDockControlSiteViewInfo.FSite.Handle, AHitTest.FHitPoint);
    GroupsDockControlSiteViewInfo.GetHitInfo(AHitTest);
  finally
    AHitTest.FHitPoint := APrevPoint;
  end;
end;

function TdxRibbonViewInfo.AdjustCaptionFontSize(ASize: Integer): Integer;
begin
  Result := ColorScheme.AdjustCaptionFontSize(ASize, UseGlass);
end;

function TdxRibbonViewInfo.GetDocumentNameTextColor(AIsActive: Boolean): TColor;
begin
  Result := Ribbon.Fonts.DocumentNameColor;
  if Result = clDefault then
    Result := Ribbon.SkinGetPartColor(rspDocumentNameText, Ord(not AIsActive));
end;

function TdxRibbonViewInfo.GetDesignHitTest(const P: TPoint): Boolean;
begin
  Result := TabsAreaViewInfo.GetDesignHitTest(P);
end;

function TdxRibbonViewInfo.GetFormCaptionFont(AIsActive: Boolean): TFont;
begin
  Result := Ribbon.Fonts.GetFormCaptionFont(AIsActive);
end;

function TdxRibbonViewInfo.GetFormCaptionText: TCaption;
begin
  Result := DocumentName + Caption;
end;

function TdxRibbonViewInfo.GetTabAtPos(X, Y: Integer): TdxRibbonTab;
begin
  Result := TabsAreaViewInfo.TabsViewInfo.GetTabAtPos(X, Y);
end;

procedure TdxRibbonViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxRibbonViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if DrawEmptyRibbon then
    Painter.DrawEmptyRibbon(ACanvas)
  else
  begin
    DrawRibbonBackground(ACanvas);
    TabsAreaViewInfo.Draw(ACanvas);
    if not QuickAccessToolbarViewInfo.AtNonClientArea then
      QuickAccessToolbarViewInfo.Draw(ACanvas);
    if IsTabGroupsVisible then
      Painter.DrawGroupsArea(ACanvas, GroupsDockControlSiteBounds);
    if IsNeedDrawBottomLine then
      Painter.DrawRibbonTopFrameAreaSeparator(ACanvas);
    ApplicationButtonViewInfo.Draw(ACanvas);
  end;
end;

procedure TdxRibbonViewInfo.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  TabsAreaViewInfo.PopulateHorizontalNavigationList(AProc);
  ApplicationButtonViewInfo.PopulateHorizontalNavigationList(AProc);
end;

procedure TdxRibbonViewInfo.UpdateNonClientParams;
var
  AForm: TdxCustomRibbonForm;
begin
  AForm := Ribbon.RibbonForm;
  FSupportNonClientDrawing := Ribbon.SupportNonClientDrawing and (AForm <> nil) and Ribbon.Visible;
  FUseGlass := FSupportNonClientDrawing and AForm.IsUseAeroNCPaint;
end;

procedure TdxRibbonViewInfo.UpdateToolbarDockControl(
  ADockControl: TdxCustomRibbonDockControl; AIsVisible: Boolean; const ABounds: TRect);
begin
  if not (Ribbon.IsLoading or Ribbon.IsBarManagerValid and TdxBarManagerAccess(Ribbon.BarManager).IsUpdateLocked) then
  begin
    ADockControl.Visible := AIsVisible;
    if ADockControl.Visible then
    begin
      ADockControl.BoundsRect := ABounds;
      ADockControl.UpdateColorScheme;
    end
    else
      ADockControl.UpdateBoundsRect(ABounds);
  end;
end;

function TdxRibbonViewInfo.GetBounds: TRect;
var
  AForm: TCustomForm;
begin
  AForm := GetParentForm(Ribbon, False);
  if AForm is TForm then
  begin
    if AForm.HandleAllocated and (AForm.WindowState = wsMinimized) and dxIsWindowStyleSet(AForm.Handle, WS_CHILD) then
      Result := cxRectSetNullOrigin(cxGetWindowRect(AForm.Handle))
    else
      Result := AForm.ClientRect;

    Result.Bottom := Ribbon.ClientBounds.Bottom;
  end
  else
    Result := Ribbon.ClientBounds;
end;

function TdxRibbonViewInfo.GetRibbonHeight: Integer;
begin
  if QuickAccessToolbarViewInfo.AtBottom then
    Result := QuickAccessToolbarViewInfo.Bounds.Bottom
  else
    if IsTabGroupsVisible and not Ribbon.IsPopupGroupsMode then
      Result := GetGroupsDockControlSiteBounds.Bottom
    else
    begin
      Result := TabsAreaViewInfo.Bounds.Bottom;
      if IsNeedDrawBottomLine and (TabsAreaViewInfo.MeasuredHeight > 0) then
        Inc(Result, ColorScheme.GetRibbonTopFrameAreaSeparatorSize - 1);
    end;

  FDrawEmptyRibbon := (Result < 8) and Ribbon.IsDesigning;
  if FDrawEmptyRibbon then
    Result := dxRibbonEmptyHeight
end;

function TdxRibbonViewInfo.GetTabsVerticalOffset: Integer;
begin
  if SupportNonClientDrawing then
    Result := GetNonClientAreaHeight
  else
    if QuickAccessToolbarViewInfo.AtTop then
      Result := QuickAccessToolbarViewInfo.MeasuredHeight
    else
      Result := 0;
end;

procedure TdxRibbonViewInfo.CalculateBounds;
begin
  CalculateTabsArea;
  CalculateTabGroups;
  if Ribbon.UseRightToLeftAlignment then
  begin
    if SupportNonClientDrawing then
      Ribbon.FormCaptionHelper.DoCalculate;
    CalculateQuickAccessToolbar;
    CalculateContexts;
    CalculateRibbonFormCaption;
  end
  else
  begin
    CalculateContexts;
    CalculateRibbonFormCaption;
    CalculateQuickAccessToolbar;
  end;
end;

procedure TdxRibbonViewInfo.CalculateMetrics;
begin
  CalculateMetricsCore(cxScreenCanvas);
  cxScreenCanvas.Dormant;
end;

procedure TdxRibbonViewInfo.CalculateMetricsCore(ACanvas: TcxCanvas);
begin
  QuickAccessToolbarViewInfo.CalculateMetrics(ACanvas);
  ApplicationButtonViewInfo.CalculateMetrics(ACanvas);
  TabsAreaViewInfo.CalculateMetrics(ACanvas);
end;

procedure TdxRibbonViewInfo.RecreateViewInfoCells;
begin
  TabsAreaViewInfo.RecreateViewInfoCells;
end;

procedure TdxRibbonViewInfo.CalculateContexts;
var
  R: TRect;
begin
  ContextsViewInfo.Clear;
  if SupportNonClientDrawing and IsTabsVisible and (TabsAreaViewInfo.TabsViewInfo.Count > 0) then
  begin
    R := GetRibbonFormCaptionClientBounds;
    Dec(R.Right, ScaleFactor.Apply(dxRibbonTabTextOffset));
    Inc(R.Bottom, ColorScheme.GetPartSize(rspContextTabOverlap));
    ContextsViewInfo.Calculate(R);
  end;
end;

procedure TdxRibbonViewInfo.CalculateQuickAccessToolbar;

  function GetQuickAccessToolbarAreaBounds: TRect;
  var
    AOnGlass: Boolean;
    ARightIndent: Integer;
    ARect: TRect;
  begin
    Result := Bounds;

    if QuickAccessToolbarViewInfo.AtTop then
    begin
      AOnGlass := QuickAccessToolbarViewInfo.IsPaintOnGlass;
      if IsApplicationButtonNearQAT then
        Result.Left := ApplicationButtonViewInfo.Bounds.Right - ColorScheme.GetQuickAccessToolbarOverrideWidth(True, AOnGlass)
      else
        if SupportNonClientDrawing then
          Result.Left := Ribbon.FormCaptionHelper.TextBounds.Left + 4
        else
          Result.Left := Result.Left + 2;

      Inc(Result.Left, ColorScheme.GetQuickAccessToolbarLeftIndent(IsApplicationButtonNearQAT, AOnGlass));

      if QuickAccessToolbarViewInfo.AtNonClientArea then
      begin
        ARect := Ribbon.FormCaptionHelper.TextBounds;
        if AOnGlass then
          ARightIndent := 0
        else
          ARightIndent := ColorScheme.GetQuickAccessToolbarRightIndent(IsApplicationButtonNearQAT);

        Inc(ARect.Left, ARightIndent);
        if AOnGlass then
          Dec(ARect.Right, 2 * dxCaptionGlowRadius);
        Result.Left := Max(Result.Left, ARect.Left);
        Result.Right := ARect.Right - ScaleFactor.Apply(dxRibbonFormCaptionMinWidth);

        if IsContextsVisible then
        begin
          ARect := ContextsViewInfo.Bounds;
          if FFormCaptionTheLeftOfContext then
            Dec(ARect.Left, dxRibbonFormCaptionMinWidth);
          Result.Right := Min(Result.Right, ARect.Left) - ARightIndent;
        end;
      end;
    end;

    if QuickAccessToolbarViewInfo.AtBottom then
    begin
      Result.Top := GetNonClientAreaHeight;
      if IsTabsVisible then
        Inc(Result.Top, TabsAreaViewInfo.MeasuredHeight - 1);
      if IsTabGroupsVisible and not Ribbon.IsPopupGroupsMode then
        Inc(Result.Top, GetTabGroupsHeight);
    end;
  end;

begin
  QuickAccessToolbarViewInfo.Calculate(GetQuickAccessToolbarAreaBounds);
end;

procedure TdxRibbonViewInfo.CalculateRibbonFormCaption;
begin
  if SupportNonClientDrawing then
    Ribbon.FormCaptionHelper.DoCalculate;
  FFormCaptionTheLeftOfContext := False; //reset
  if SupportNonClientDrawing then
    FFormCaptionBounds := GetRibbonFormCaptionTextBounds
  else
    FFormCaptionBounds := cxEmptyRect;
end;

procedure TdxRibbonViewInfo.CalculateTabGroups;
begin
  if IsTabGroupsVisible and not Ribbon.IsPopupGroupsMode then
  begin
    FGroupsDockControlSiteBounds := GetGroupsDockControlSiteBounds;
    FTabGroupsDockControlBounds := GetTabGroupsDockControlBounds;
  end
  else
  begin
    FGroupsDockControlSiteBounds := cxEmptyRect;
    FTabGroupsDockControlBounds := cxEmptyRect;
  end;
  if not Ribbon.UseRightToLeftAlignment then
    if not Ribbon.IsPopupGroupsMode then
    begin
      UpdateGroupsDockControlSite;
      if IsTabGroupsVisible then
        TabsAreaViewInfo.TabsViewInfo.UpdateDockControls;
    end;
end;

procedure TdxRibbonViewInfo.CalculateTabsArea;
begin
  TabsAreaViewInfo.Calculate(cxRectSetTop(Bounds, GetTabsVerticalOffset, TabsAreaViewInfo.MeasuredHeight));
end;

function TdxRibbonViewInfo.CanDrawDefaultFormIcon: Boolean;
begin
  if Hidden then
    Result := not Ribbon.IsAutoHidden
  else
    Result := not ApplicationButtonViewInfo.IsVisible or (Painter.GetRibbonFormPaintData.GetState = wsMinimized);
end;

function TdxRibbonViewInfo.CanShowBarControls(AIgnoreHidden: Boolean = False): Boolean;
begin
  Result := Ribbon.IsBarManagerValid and (not Hidden or AIgnoreHidden);
end;

function TdxRibbonViewInfo.GetNonClientAreaHeight: Integer;
begin
  if SupportNonClientDrawing then
    Result := Ribbon.GetRibbonFormCaptionHeight
  else
    Result := 0;
end;

function TdxRibbonViewInfo.GetNonClientAreaObjectsRegion: HRGN;
begin
  Result := 0;
  if ApplicationButtonViewInfo.IsVisible then
    dxCombineRectRegion(Result, ApplicationButtonViewInfo.Bounds, RGN_OR);
  if ContextsViewInfo.Count > 0 then
    dxCombineRectRegion(Result, ContextsViewInfo.Bounds, RGN_OR);
end;

function TdxRibbonViewInfo.CreateApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
begin
  Result := TdxRibbonApplicationButtonViewInfo.Create(Self);
end;

function TdxRibbonViewInfo.CreateContextsViewInfo: TdxRibbonContextsViewInfo;
begin
  Result := TdxRibbonContextsViewInfo.Create(Self);
end;

function TdxRibbonViewInfo.CreateQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
begin
  Result := TdxRibbonQuickAccessToolbarViewInfo.Create(Self);
end;

function TdxRibbonViewInfo.CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
begin
  Result := TdxRibbonTabsAreaViewInfo.Create(Self);
end;

procedure TdxRibbonViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
  FGroupsDockControlSiteBounds := TdxRightToLeftLayoutConverter.ConvertRect(FGroupsDockControlSiteBounds, ABounds);
  FTabGroupsDockControlBounds := TdxRightToLeftLayoutConverter.ConvertRect(FTabGroupsDockControlBounds, ABounds);
  TabsAreaViewInfo.RightToLeftConversion(ABounds);

  if SupportNonClientDrawing then
    Ribbon.FormCaptionHelper.RightToLeftConversion(ABounds);

  if not Ribbon.IsPopupGroupsMode then
  begin
    UpdateGroupsDockControlSite;

    if IsTabGroupsVisible then
      TabsAreaViewInfo.TabsViewInfo.UpdateDockControls;
  end;

  ContextsViewInfo.RightToLeftConversion(ABounds);
  FFormCaptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FFormCaptionBounds, ABounds);
  QuickAccessToolbarViewInfo.RightToLeftConversion(ABounds);

  TabsAreaViewInfo.UpdateToolbarDockControls;
end;

function TdxRibbonViewInfo.GetCaption: string;
begin
  if Ribbon.RibbonForm <> nil then
  begin
    Result := Ribbon.RibbonForm.Caption;
    if DocumentName <> '' then
      Result := ' - ' + Result;
  end
  else
    Result := '';
end;

function TdxRibbonViewInfo.GetDocumentName: string;
begin
  Result := Ribbon.DocumentName;
end;

function TdxRibbonViewInfo.GetRibbonFormCaptionClientBounds: TRect;
begin
  Result := Ribbon.FormCaptionHelper.TextBounds;
  if not Hidden then
  begin
    Result.Bottom := GetNonClientAreaHeight;
    if QuickAccessToolbarViewInfo.AtNonClientArea then
      Result.Left := QuickAccessToolbarViewInfo.Bounds.Right + ColorScheme.GetQuickAccessToolbarRightIndent(IsApplicationButtonNearQAT)
  end;
end;

function TdxRibbonViewInfo.GetRibbonFormCaptionTextBounds: TRect;
var
  ATextWidth, W: Integer;
  ACenterRect: TRect;
begin
  Result := GetRibbonFormCaptionClientBounds;
  ATextWidth := cxTextWidth(GetFormCaptionFont(True), GetFormCaptionText);
  if UseGlass then
    Inc(ATextWidth, 2 * dxCaptionGlowRadius) // add a glow radius around text
  else
    InflateRect(Result, -ScaleFactor.Apply(dxRibbonFormCaptionTextSpace), 0);

  ACenterRect := cxRect(Bounds.Left, Result.Top, Bounds.Right, Result.Bottom);
  if not UseGlass then
    InflateRect(ACenterRect, -ScaleFactor.Apply(dxRibbonFormCaptionTextSpace), 0);

  W := cxRectWidth(ACenterRect) - ATextWidth;
  Inc(ACenterRect.Left, W div 2);
  Dec(ACenterRect.Right, W div 2 - 1);
  if ContextsViewInfo.Count > 0 then
    Result := GetRibbonFormCaptionTextBoundsWithContext(Result, ACenterRect, ATextWidth)
  else
    if (W >= 0) and cxRectContain(Result, ACenterRect) then
      Result := ACenterRect;
end;

function TdxRibbonViewInfo.GetRibbonFormCaptionTextBoundsWithContext(
  const ABounds, ACenterRect: TRect; ATextWidth: Integer): TRect;
var
  ALeftWidth, ARightWidth: Integer;
  ALeftRect, ARightRect: TRect;
begin
  //try center
  ALeftRect := cxRect(ABounds.Left, ABounds.Top, ContextsViewInfo.Bounds.Left, ABounds.Bottom);
  if not UseGlass then
    Dec(ALeftRect.Right, ScaleFactor.Apply(dxRibbonFormCaptionTextSpace));

  if cxRectContain(ALeftRect, ACenterRect) then
  begin
    Result := ACenterRect;
    Exit;
  end;
  ARightRect := cxRect(ContextsViewInfo.Bounds.Right, ABounds.Top, ABounds.Right, ABounds.Bottom);
  if not UseGlass then
    Inc(ARightRect.Left, ScaleFactor.Apply(dxRibbonFormCaptionTextSpace));
  if cxRectContain(ARightRect, ACenterRect) then
  begin
    Result := ACenterRect;
    Exit;
  end;

  //select the best
  ALeftWidth := ALeftRect.Right - ALeftRect.Left;
  ARightWidth := ARightRect.Right - ARightRect.Left;
  if ALeftWidth >= ATextWidth then
  begin
    FFormCaptionTheLeftOfContext := True;
    Result := cxRectCenterHorizontally(ALeftRect, ATextWidth);
  end
  else if ARightWidth >= ATextWidth then
    Result := ARightRect
  else if ALeftWidth > ARightWidth then
  begin
    FFormCaptionTheLeftOfContext := True;
    Result := ALeftRect;
  end
  else
    Result := ARightRect;
end;

function TdxRibbonViewInfo.GetGroupsDockControlSiteBounds: TRect;
begin
  Result := Bounds;
  Result.Top := TabsAreaViewInfo.Bounds.Bottom;
  if IsTabsVisible then
    Dec(Result.Top, Ribbon.SkinGetPartSize(DXBAR_TABSGROUPSOVERLAPHEIGHT));
  Result.Bottom := Result.Top + GetTabGroupsHeight;
end;

function TdxRibbonViewInfo.GetTabGroupsContentOffset: TRect;
const
  Parts: array[Boolean] of Integer = (DXBAR_RIBBONTABGROUP, DXBAR_RIBBONCONTEXTTABGROUP);
begin
  Result := Ribbon.SkinGetContentOffsets(Parts[HasActiveContextTab]);
end;

function TdxRibbonViewInfo.GetTabGroupsDockControlBounds: TRect;
begin
  Result := cxRectSetNullOrigin(GetGroupsDockControlSiteBounds);
  Result := cxRectContent(Result, GetTabGroupsDockControlOffset);
end;

function TdxRibbonViewInfo.GetTabGroupsDockControlOffset: TRect;
begin
  Result := GetTabGroupsContentOffset;
end;

function TdxRibbonViewInfo.GetTabGroupsHeight(AIgnoreHidden: Boolean = False): Integer;
begin
  if IsTabGroupsVisible or AIgnoreHidden then
    Result := Ribbon.GetGroupHeight + cxMarginsHeight(GetTabGroupsDockControlOffset)
  else
    Result := 0;
end;

function TdxRibbonViewInfo.HasMDIButtons: Boolean;
begin
  if not Hidden and IsTabsVisible and Ribbon.IsBarManagerValid then
    Result := Ribbon.BarManager.IsMDIMaximized and (GetSystemMenu(Ribbon.BarManager.ActiveMDIChild, False) <> 0)
  else
    Result := False;
end;

function TdxRibbonViewInfo.IsMDIButtonEnabled(AButton: TdxBarMDIButton; AState: Integer): Boolean;
begin
  Result := ((AButton = mdibRestore) or
    (GetMenuState(GetSystemMenu(Ribbon.BarManager.ActiveMDIChild, False), MDIButtonCommands[AButton], MF_BYCOMMAND) and AState = 0));
end;

function TdxRibbonViewInfo.IsApplicationButtonNearQAT: Boolean;
begin
  Result := False;
end;

function TdxRibbonViewInfo.IsContextsVisible: Boolean;
begin
  Result := IsTabsVisible and (TabsAreaViewInfo.TabsViewInfo.Count > 0) and (ContextsViewInfo.Count > 0);
end;

function TdxRibbonViewInfo.IsTabGroupsVisible(AIgnoreHidden: Boolean = False): Boolean;
begin
  Result := CanShowBarControls(AIgnoreHidden) and
    (Ribbon.ShowTabGroups or Ribbon.IsPopupGroupsMode) and (TabsAreaViewInfo.TabsViewInfo.Count > 0);
end;

function TdxRibbonViewInfo.IsTabsVisible: Boolean;
begin
  Result := Ribbon.ShowTabHeaders and not Hidden;
end;

procedure TdxRibbonViewInfo.DrawRibbonBackground(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := Bounds;
  if SupportNonClientDrawing then
    R.Top := GetRibbonFormCaptionClientBounds.Bottom;

  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    ACanvas.SaveClipRegion;
    try
      Painter.DrawTabAreaBackground(ACanvas, TabsAreaViewInfo.Bounds);
      ACanvas.ExcludeClipRect(TabsAreaViewInfo.Bounds);
      Painter.DrawBackground(ACanvas, Bounds);
    finally
      ACanvas.RestoreClipRegion;
    end;
    ACanvas.IntersectClipRect(BackgroundImageClipBounds);
    Painter.DrawBackgroundImage(ACanvas, BackgroundImageBounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxRibbonViewInfo.IsNeedDrawBottomLine: Boolean;
begin
  Result := IsTabsVisible and not (QuickAccessToolbarViewInfo.AtBottom or IsTabGroupsVisible or IsTabsOnGlass);
end;

function TdxRibbonViewInfo.IsNeedHideControl: Boolean;
var
  AForm: TCustomForm;
begin
  Result := False;
  if not Ribbon.IsDesigning then
  begin
    AForm := GetParentForm(Ribbon);
    Result := (AForm <> nil) and ((IsIconic(AForm.Handle) and IsShowMinimizedOnDesktop(AForm.Handle)) or
      (AForm.Width < ScaleFactor.Apply(dxRibbonOwnerMinimalWidth)) or
      (AForm.Height < ScaleFactor.Apply(dxRibbonOwnerMinimalHeight))) or Ribbon.IsAutoHidden;
  end;
end;

function TdxRibbonViewInfo.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Ribbon.ColorScheme;
end;

function TdxRibbonViewInfo.GetHasActiveContextTab: Boolean;
begin
  Result := (Ribbon.ActiveTab <> nil) and (Ribbon.ActiveTab.Context <> nil);
end;

function TdxRibbonViewInfo.GetIsFormCaptionActive: Boolean;
begin
  Result := SupportNonClientDrawing and Ribbon.RibbonForm.IsActive;
end;

function TdxRibbonViewInfo.GetIsTabsOnGlass: Boolean;
begin
  Result := Ribbon.EnableTabAero and UseGlass and ColorScheme.ExtendCaptionAreaOnTabs;
end;

function TdxRibbonViewInfo.GetPainter: TdxRibbonPainter;
begin
  Result := Ribbon.Painter;
end;

function TdxRibbonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ribbon.ScaleFactor;
end;

function TdxRibbonViewInfo.GetScrollButtonWidth: Integer;
begin
  Result := (Abs(Ribbon.Fonts.TabHeader.Height) * 2 + 2) div 2 + 1;
end;

function TdxRibbonViewInfo.Style: TdxRibbonStyle;
begin
  Result := Ribbon.Style;
end;

procedure TdxRibbonViewInfo.UpdateButtonsStates;
begin
  GroupsDockControlSiteViewInfo.UpdateButtonsState;
  TabsAreaViewInfo.UpdateButtonsState;
end;

procedure TdxRibbonViewInfo.UpdateGroupsDockControlSite;
begin
  Ribbon.GroupsDockControlSite.BoundsRect := GroupsDockControlSiteBounds;
end;

{ TdxRibbonViewInfo2007 }

function TdxRibbonViewInfo2007.IsApplicationButtonNearQAT: Boolean;
begin
  Result := ApplicationButtonViewInfo.IsVisible;
end;

procedure TdxRibbonViewInfo2007.CalculateBounds;
begin
  if ApplicationButtonViewInfo.IsVisible then
    ApplicationButtonViewInfo.Calculate(cxRect(ApplicationButtonViewInfo.MeasureSize))
  else
    ApplicationButtonViewInfo.Calculate(cxNullRect);

  inherited CalculateBounds;
end;

function TdxRibbonViewInfo2007.CreateContextsViewInfo: TdxRibbonContextsViewInfo;
begin
  Result := TdxRibbonContextsViewInfo2007.Create(Self);
end;

procedure TdxRibbonViewInfo2007.DoRightToLeftConversion(const ABounds: TRect);
begin
  if ApplicationButtonViewInfo.IsVisible then
    ApplicationButtonViewInfo.RightToLeftConversion(ABounds);
  inherited DoRightToLeftConversion(ABounds);
end;

function TdxRibbonViewInfo2007.GetRibbonFormCaptionClientBounds: TRect;
begin
  Result := inherited;
  if not Hidden then
  begin
    if ApplicationButtonViewInfo.IsVisible then
      Result.Left := Max(Result.Left, ApplicationButtonViewInfo.Bounds.Right);
  end;
end;

{ TdxRibbonViewInfo2010OrNewer }

function TdxRibbonViewInfo2010OrNewer.CanDrawDefaultFormIcon: Boolean;
begin
  Result := not (Hidden and Ribbon.IsAutoHidden)
end;

function TdxRibbonViewInfo2010OrNewer.CreateApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
begin
  Result := TdxRibbonApplicationButtonViewInfo2010OrNewer.Create(Self);
end;

function TdxRibbonViewInfo2010OrNewer.CreateContextsViewInfo: TdxRibbonContextsViewInfo;
begin
  Result := TdxRibbonContextsViewInfo2010OrNewer.Create(Self);
end;

function TdxRibbonViewInfo2010OrNewer.CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
begin
  Result := TdxRibbonTabsAreaViewInfo2010OrNewer.Create(Self);
end;

{ TdxRibbonViewInfo2010 }

function TdxRibbonViewInfo2010.CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
begin
  Result := TdxRibbonTabsAreaViewInfo2010.Create(Self);
end;

{ TdxRibbonViewInfo2013OrNewer }

procedure TdxRibbonViewInfo2013OrNewer.CalculateBackgroundImage;
var
  ARect: TRect;
  AImageSize: TSize;
begin
  FBackgroundImageBounds := cxNullRect;
  if not UseGlass then
  begin
    AImageSize := dxGetImageSize(Ribbon.BackgroundImage, ScaleFactor);
    ARect := Bounds;
    Inc(ARect.Top, Ribbon.GetWindowBordersWidth.Top + 1);
    ARect := cxRectSetHeight(ARect, AImageSize.cy);
    ARect := cxRectSetRight(ARect, ARect.Right, AImageSize.cx);
    if ARect.Left >= Bounds.Left then
      FBackgroundImageBounds := ARect;
  end;
  FBackgroundImageClipBounds := BackgroundImageBounds;
end;

procedure TdxRibbonViewInfo2013OrNewer.CalculateBounds;
begin
  CalculateBackgroundImage;
  inherited;
end;

procedure TdxRibbonViewInfo2013OrNewer.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBackgroundImageBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBackgroundImageBounds, ABounds);
  FBackgroundImageClipBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBackgroundImageClipBounds, ABounds);
  inherited DoRightToLeftConversion(ABounds);
end;

function TdxRibbonViewInfo2013OrNewer.GetTabGroupsDockControlOffset: TRect;
begin
  Result := inherited;
  Inc(Result.Right, GroupsDockControlSiteViewInfo.GetMinimizeRibbonButtonSize.cx + 2);
end;

{ TdxRibbonViewInfo2016Tablet }

procedure TdxRibbonViewInfo2016Tablet.CalculateQuickAccessToolbar;
begin
  if QuickAccessToolbarViewInfo.AtBottom then
    inherited;
end;

function TdxRibbonViewInfo2016Tablet.CreateQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
begin
  Result := TdxRibbonQuickAccessToolbarViewInfo2016Tablet.Create(Self);
end;

function TdxRibbonViewInfo2016Tablet.CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
begin
  Result := TdxRibbonTabsAreaViewInfo2016Tablet.Create(Self);
end;

function TdxRibbonViewInfo2016Tablet.GetTabsVerticalOffset: Integer;
begin
  if SupportNonClientDrawing then
    Result := GetNonClientAreaHeight
  else
    Result := 0;
end;

procedure TdxRibbonViewInfo2016Tablet.PopulateHorizontalNavigationList(AProc: TdxRibbonAddAccessibilityHelperProc);
begin
  inherited;
  dxRibbonAccessibilityEnumChildren(QuickAccessToolbarViewInfo.Toolbar.IAccessibilityHelper, AProc);
end;

{ TdxRibbonViewInfo2019 }

function TdxRibbonViewInfo2019.CanDrawDefaultFormIcon: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonViewInfo2019.CalculateBackgroundImage;
begin
  inherited CalculateBackgroundImage;
  FBackgroundImageClipBounds.Bottom := FormCaptionBounds.Bottom;
end;

function TdxRibbonViewInfo2019.CreateTabsAreaViewInfo: TdxRibbonTabsAreaViewInfo;
begin
  Result := TdxRibbonTabsAreaViewInfo2019.Create(Self);
end;

{ TdxRibbonBarControlAccessibilityHelper }

function TdxRibbonBarControlAccessibilityHelper.LogicalNavigationGetNextChild(AChildIndex: Integer; AShift: TShiftState): TdxBarAccessibilityHelper;
begin
  Result := LogicalNavigationGetNextChild(AChildIndex, not(ssShift in AShift));
end;

{ TdxRibbonBarButtonLikeControlDrawParams }

constructor TdxRibbonBarButtonLikeControlDrawParams.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(nil);
  FRibbon := ARibbon;
end;

function TdxRibbonBarButtonLikeControlDrawParams.GetScaleFactor: TdxScaleFactor;
begin
  Result := FRibbon.ScaleFactor;
end;

{ TdxRibbonBarPainter }

constructor TdxRibbonBarPainter.Create(AData: TdxNativeUInt);
begin
  inherited Create(AData);
  FRibbon := TdxCustomRibbon(AData);
  FCollapsedGroupWidthCache := TDictionary<string, Integer>.Create;
  FDrawParams := TdxRibbonBarButtonLikeControlDrawParams.Create(FRibbon);
end;

destructor TdxRibbonBarPainter.Destroy;
begin
  FreeAndNil(FCollapsedGroupWidthCache);
  FreeAndNil(FDrawParams);
  inherited Destroy;
end;

procedure TdxRibbonBarPainter.BarDrawBackground(ABarControl: TdxBarControl; ADC: HDC;
  const ADestRect: TRect; const ASourceRect: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  cxPaintCanvas.BeginPaint(ADC);
  try
    cxPaintCanvas.IntersectClipRect(ADestRect);
    (ABarControl as TdxRibbonCustomBarControl).DrawBarParentBackground(cxPaintCanvas);
    DrawToolbarContentPart(ABarControl, cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxRibbonBarPainter.BarMarkRect(ABarControl: TdxBarControl): TRect;
begin
  Result := cxGetClientRect(ABarControl);
end;

function TdxRibbonBarPainter.BarMarkItemRect(ABarControl: TdxBarControl): TRect;
begin
  Result := cxGetClientRect(ABarControl);
end;

procedure TdxRibbonBarPainter.CalculateDrawParams;
const
  DefaultFontHeight = 13;
begin
  FCollapsedGroupWidthCache.Clear;

  FCollapsedGroupElementSizeNumerator := GetSmallIconSize +
    cxMarginsHeight(Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND)) +
    cxMarginsHeight(dxRibbonCollapsedGroupGlyphBackgroundOffsets);
  FCollapsedGroupElementSizeDenominator := Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR).Top +
    GetButtonHeight(GetSmallIconSize, DefaultFontHeight + dxRibbonGroupRowHeightCorrection, DrawParams.ScaleFactor) * GetGroupRowCount +
    GetGroupCaptionHeight(DefaultFontHeight) + (Ribbon.Painter.GetGroupCaptionBottomOffset + 1) -
    cxMarginsHeight(Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR));

  FGroupCaptionTextHeight := cxTextHeight(Ribbon.Fonts.GetGroupHeaderFont);
  FGroupRowTextHeight := cxTextHeight(Ribbon.Fonts.GetGroupFont);
end;

function TdxRibbonBarPainter.CalculateRowHeight(AIconSize, ATextHeight: Integer; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := GetButtonHeight(AIconSize, ATextHeight + AScaleFactor.Apply(dxRibbonGroupRowHeightCorrection), AScaleFactor);
end;

function TdxRibbonBarPainter.GetGlyphColorPalette(ABarItemControl: TdxBarItemControl;
  APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette;
begin
  if APaintType = ptMenu then
    Result := Ribbon.ColorScheme.GetMenuColorPalette(AState)
  else
    Result := Ribbon.ColorScheme.GetSmallButtonColorPalette(AState);
end;

function TdxRibbonBarPainter.GetGroupRowCount: Integer;
begin
  if IsSimplifiedGroupsLayout then
    Result := dxRibbonTabletGroupRowCount
  else
    Result := dxRibbonGroupRowCount;
end;

function TdxRibbonBarPainter.GetGroupRowHeight(AIconSize: Integer): Integer;
begin
  Result := CalculateRowHeight(AIconSize, FGroupRowTextHeight, DrawParams.ScaleFactor);
end;

function TdxRibbonBarPainter.DropDownGalleryGetScrollBarWidth(AControlHeight: Integer; const AControlMargins: TRect): Integer;
begin
  if IsSimplifiedGroupsLayout then
  begin
    Result := AControlHeight - cxMarginsHeight(AControlMargins);
    dxAdjustToTouchableSize(Result, ScaleFactor);
  end
  else
    Result := inherited DropDownGalleryGetScrollBarWidth(AControlHeight, AControlMargins)
end;

function TdxRibbonBarPainter.GetToolbarContentOffsets(ABar: TdxBar;
  ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect;
begin
  if (ABar.Control is TdxRibbonGroupBarControl) and TdxRibbonGroupBarControl(ABar.Control).Collapsed then
    Result := cxEmptyRect
  else
    Result := inherited GetToolbarContentOffsets(ABar, ADockingStyle, AScaleFactor, AHasSizeGrip);
end;

function TdxRibbonBarPainter.IsSimplifiedGroupsLayout: Boolean;
begin
  Result := Ribbon.IsSimplifiedGroupsLayout;
end;

function TdxRibbonBarPainter.SubMenuControlBeginGroupSize: Integer;
begin
  Result := Ribbon.ColorScheme.GetMenuSeparatorSize;
end;

function TdxRibbonBarPainter.SubMenuGetSeparatorSize: Integer;
begin
  Result := Ribbon.ColorScheme.GetMenuSeparatorSize;
end;

procedure TdxRibbonBarPainter.DrawCollapsedToolbarBackgroundPart(
  ABarControl: TdxRibbonGroupBarControl; ACanvas: TcxCanvas; AGroupState: Integer);
begin
  if not dxFader.DrawFadeImage(ABarControl, ACanvas.Handle, ABarControl.ClientRect) then
    Skin.DrawBackground(ACanvas.Handle, ABarControl.ClientRect, DXBAR_COLLAPSEDTOOLBAR, AGroupState);
end;

procedure TdxRibbonBarPainter.DrawCollapsedToolbarContentPart(
  ABarControl: TdxRibbonGroupBarControl; ACanvas: TcxCanvas; AGroupState: Integer);

  procedure InitDrawParams(AState: Integer);
  begin
    DrawParams.ResetCachedValues;
    case AState of
      DXBAR_NORMAL:
        DrawParams.HotPartIndex := icpNone;
      DXBAR_HOT:
        DrawParams.HotPartIndex := icpControl;
      DXBAR_PRESSED:
        begin
          DrawParams.HotPartIndex := icpControl;
          DrawParams.IsPressed := True;
        end;
    end;
    DrawParams.Canvas := ACanvas;
    DrawParams.Caption := (ABarControl as TdxRibbonGroupBarControl).GetCaption;
    DrawParams.IsDropDown := True;
    DrawParams.ViewSize := cvsLarge;
    DrawParams.Enabled := True;
    DrawParams.CanSelect := True;
  end;

var
  ACaptionRect, R: TRect;
  AGroupGlyph: TdxSmartGlyph;
  AGroupGlyphBackgroundSize: TSize;
begin
  ACaptionRect := GetCollapsedGroupCaptionRect(ABarControl.ClientRect);

  InitDrawParams(AGroupState);
  ButtonLikeControlDrawCaption(DrawParams, ACaptionRect, DT_CENTER);

  AGroupGlyphBackgroundSize := GetCollapsedGroupGlyphBackgroundSize(ABarControl);
  R := cxRectContent(ABarControl.ClientRect, Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR));
  R.Bottom := ACaptionRect.Top;
  R := cxRectCenterHorizontally(R, AGroupGlyphBackgroundSize.cx);
  Inc(R.Top, dxRibbonCollapsedGroupGlyphBackgroundOffsets.Top);
  Dec(R.Bottom, dxRibbonCollapsedGroupGlyphBackgroundOffsets.Bottom);
  R := cxRectCenterVertically(R, AGroupGlyphBackgroundSize.cy);
  Skin.DrawBackground(ACanvas.Handle, R, DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND, AGroupState);

  AGroupGlyph := GetCollapsedGroupGlyph(ABarControl);
  if AGroupGlyph <> nil then
  begin
    with Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND) do
    begin
      Inc(R.Top, Top);
      Dec(R.Bottom, Bottom);
    end;
    AGroupGlyph.StretchDraw(ACanvas.Handle, cxRectCenter(R, GetCollapsedGroupGlyphSize(ABarControl)));
  end;
end;

procedure TdxRibbonBarPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);

  function DrawFadeBackground(AGroupBarControl: TdxRibbonGroupBarControl): Boolean;
  var
    APrevWindowOrg: TPoint;
  begin
    with AGroupBarControl.NCOffset do
      OffsetWindowOrgEx(ACanvas.Handle, X, Y, APrevWindowOrg);
    try
      Result := dxFader.DrawFadeImage(AGroupBarControl, ACanvas.Handle, AGroupBarControl.NCRect);
    finally
      SetWindowOrgEx(ACanvas.Handle, APrevWindowOrg.X, APrevWindowOrg.Y, nil);
    end;
  end;

var
  AGroupBarControl: TdxRibbonGroupBarControl;
  AGroupState: Integer;
begin
  AGroupBarControl := ABarControl as TdxRibbonGroupBarControl;
  if AGroupBarControl.Collapsed then
  begin
    AGroupState := GetGroupState(ABarControl);
    DrawCollapsedToolbarBackgroundPart(AGroupBarControl, ACanvas, AGroupState);
    DrawCollapsedToolbarContentPart(AGroupBarControl, ACanvas, AGroupState);
  end
  else
    if not DrawFadeBackground(AGroupBarControl) then
      inherited DrawToolbarContentPart(ABarControl, ACanvas);
end;

procedure TdxRibbonBarPainter.DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC);
begin
  Skin.DrawBackground(DC,
    TdxBarControlAccess(ABarControl).NCRect,
    GetToolbarSkinPart(ABarControl),
    GetBarControlState(ABarControl));
end;

function TdxRibbonBarPainter.GetCollapsedGroupWidth(ABarControl: TdxRibbonGroupBarControl): Integer;
var
  ARect: TRect;
begin
  if IsSimplifiedGroupsLayout then
    Exit(ScaleFactor.Apply(dxRibbonGroupMinContentWidth));

  if not FCollapsedGroupWidthCache.TryGetValue(ABarControl.GetCaption, Result) then
  begin
    cxScreenCanvas.Font := ABarControl.Font;
    try
      DrawParams.ResetCachedValues;
      DrawParams.Canvas := cxScreenCanvas;
      DrawParams.Caption := ABarControl.GetCaption;
      DrawParams.ViewSize := cvsLarge;
      DrawParams.IsDropDown := True;
      Result := GetControlCaptionRect(DrawParams).Right;

      ARect := Rect(0, 0, 100, 100);
      with GetCollapsedGroupCaptionRect(ARect) do
        Inc(Result, (Left - ARect.Left) + (ARect.Right - Right));

      Result := Max(Result,
        GetCollapsedGroupGlyphBackgroundSize(ABarControl).cx +
        cxMarginsWidth(dxRibbonCollapsedGroupGlyphBackgroundOffsets) +
        cxMarginsWidth(Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR)));

      FCollapsedGroupWidthCache.Add(DrawParams.Caption, Result);
    finally
      cxScreenCanvas.Dormant;
    end;
  end;
end;

function TdxRibbonBarPainter.GetGroupCaptionHeight: Integer;
begin
  Result := GetGroupCaptionHeight(FGroupCaptionTextHeight);
end;

function TdxRibbonBarPainter.GetGroupCaptionHeight(ATextHeight: Integer): Integer;
begin
  if IsSimplifiedGroupsLayout then
    Result := 0
  else
    Result := ATextHeight + dxRibbonGroupCaptionHeightCorrection;
end;

function TdxRibbonBarPainter.DropDownGalleryGetSizingBandHeight(
  AIconSize, ATextSize: Integer; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := CalculateRowHeight(AIconSize, ATextSize, AScaleFactor);
end;

function TdxRibbonBarPainter.GetButtonPartState(
  const ADrawParams: TdxBarButtonLikeControlDrawParams; AControlPart: Integer): Integer;

  function GetCustomButtonPartState: Integer;
  begin
    if ADrawParams.IsPressed then
      Result := DXBAR_PRESSED
    else
      if ADrawParams.Downed then
      begin
        if ADrawParams.DroppedDown then
          Result := DXBAR_ACTIVE
        else
          if (ADrawParams.HotPartIndex = AControlPart) or ADrawParams.SelectedByKey and not ADrawParams.IsDropDown then
            Result := DXBAR_HOTCHECK
          else
            Result := DXBAR_CHECKED
      end
      else
        if ADrawParams.DroppedDown then
          Result := DXBAR_DROPPEDDOWN
        else
          Result := GetPartState(ADrawParams, AControlPart)
  end;

  function GetDropButtonPartState: Integer;
  begin
    if ADrawParams.DroppedDown then
      Result := DXBAR_DROPPEDDOWN
    else
      if ADrawParams.Downed and not ADrawParams.SelectedByKey and
        (ADrawParams.HotPartIndex <> AControlPart)
      then
        Result := DXBAR_CHECKED
      else
        Result := GetPartState(ADrawParams, AControlPart);
  end;

begin
  if (Ribbon.Style = rs2007) or ADrawParams.IsCustomizing then
    Result := inherited
  else
    case AControlPart of
      bcpButton:
        Result := GetCustomButtonPartState;
      bcpDropButton:
        Result := GetDropButtonPartState
      else
        Result := DXBAR_NORMAL;
    end;
end;

function TdxRibbonBarPainter.GetCollapsedGroupCaptionRect(const AGroupRect: TRect): TRect;
begin
  Result := cxRectContent(AGroupRect, Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR));
  Result := cxRectInflate(Result, -1, 0);
  if IsSimplifiedGroupsLayout then
    Result.Top := Result.Bottom
  else
    Inc(Result.Top, cxRectHeight(Result) * FCollapsedGroupElementSizeNumerator div FCollapsedGroupElementSizeDenominator);
end;

function TdxRibbonBarPainter.GetGroupMinWidth(ABarControl: TdxRibbonGroupBarControl): Integer;
begin
  if IsSimplifiedGroupsLayout then
    Exit(ScaleFactor.Apply(dxRibbonGroupMinContentWidth));

  Result := cxTextWidth(ABarControl.Ribbon.Fonts.GetGroupHeaderFont, ABarControl.GetCaption) + 2 * cxTextSpace;
  if ABarControl.CaptionButtons.Count > 0 then
    Inc(Result, cxRectWidth(ABarControl.CaptionButtons.Rect) + Skin.GetContentOffsets(GetToolbarSkinPart(ABarControl)).Right);
end;

function TdxRibbonBarPainter.SubMenuControlUseSingleMenuWindowMode: Boolean;
begin
  Result := Ribbon.Style = rs2016Tablet;
end;

function TdxRibbonBarPainter.GetCollapsedGroupGlyph(ABarControl: TdxBarControl): TdxSmartGlyph;
begin
  Result := ABarControl.Bar.Glyph;
  if (Result <> nil) and Result.Empty then
    Result := nil;
end;

function TdxRibbonBarPainter.GetCollapsedGroupGlyphBackgroundSize(ABarControl: TdxBarControl): TSize;
var
  AGroupContentHeight: Integer;
begin
  AGroupContentHeight := ABarControl.Height - cxMarginsWidth(Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBAR));
  Result.cy := cxMarginsHeight(Skin.GetContentOffsets(DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND)) + GetSmallIconSize;
  Result.cy := Result.cy * AGroupContentHeight div FCollapsedGroupElementSizeDenominator;
  Result.cx := Result.cy;
end;

function TdxRibbonBarPainter.GetCollapsedGroupGlyphSize(ABarControl: TdxBarControl): TSize;
var
  AGlyphSize: TSize;
  AGroupGlyph: TdxSmartGlyph;
begin
  AGroupGlyph := GetCollapsedGroupGlyph(ABarControl);
  if AGroupGlyph <> nil then
    AGlyphSize := dxGetImageSize(AGroupGlyph, ScaleFactor)
  else
    AGlyphSize := cxSize(GetSmallIconSize, GetSmallIconSize);

  if (AGlyphSize.cx <= GetSmallIconSize) and (AGlyphSize.cy <= GetSmallIconSize) then
    Result := AGlyphSize
  else
    if AGlyphSize.cx > AGlyphSize.cy then
    begin
      Result.cx := GetSmallIconSize;
      Result.cy := AGlyphSize.cy * GetSmallIconSize div AGlyphSize.cx;
    end
    else
    begin
      Result.cy := GetSmallIconSize;
      Result.cx := AGlyphSize.cx * GetSmallIconSize div AGlyphSize.cy;
    end;
end;

function TdxRibbonBarPainter.GetGroupState(ABarControl: TdxBarControl): Integer;
const
  GroupStates: array[TdxBarMarkState] of Integer = (DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED);
begin
  if ABarControl.IAccessibilityHelper.IsSelected then
    Result := DXBAR_ACTIVE
  else
    Result := GroupStates[(ABarControl as TdxRibbonGroupBarControl).MarkDrawState];
end;

function TdxRibbonBarPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Ribbon.ScaleFactor;
end;

function TdxRibbonBarPainter.GetSmallIconSize: Integer;
begin
  Result := GetSmallIconSize(DrawParams.ScaleFactor);
end;

{ TdxCustomRibbonDockControl }

procedure TdxCustomRibbonDockControl.UpdateBarControlsFont;
var
  ABarControl: TdxBarControl;
  ARow: TdxDockRow;
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    for J := 0 to ARow.ColCount - 1 do
    begin
      ABarControl := ARow.Cols[J].BarControl;
      if (ABarControl <> nil) and ABarControl.HandleAllocated then
        ABarControl.UpdateFont;
    end;
  end;
end;

procedure TdxCustomRibbonDockControl.UpdateColorScheme;
begin
  RepaintBarControls;
  Invalidate;
end;

procedure TdxCustomRibbonDockControl.AdjustSize;
begin
end;

function TdxCustomRibbonDockControl.AllowUndockWhenLoadFromIni: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbonDockControl.FillBackground(DC: HDC;
  const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor);
begin
end;

function TdxCustomRibbonDockControl.GetSunkenBorder: Boolean;
begin
  Result := False;
end;

function TdxCustomRibbonDockControl.IsDrawDesignBorder: Boolean;
begin
  Result := False;
end;

function TdxCustomRibbonDockControl.IsTransparent: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbonDockControl.VisibleChanged;
begin
end;

procedure TdxCustomRibbonDockControl.CMVisibleChanged(var Message: TMessage);
begin
  if HandleAllocated and not Visible then
    ShowWindow(Handle, SW_HIDE); // SC's bugs ID CB41787, CB47149
  VisibleChanged;
  inherited;
end;

procedure TdxCustomRibbonDockControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if BarManager <> nil then
  begin
    if BarManager.Designing then
      inherited
    else
      Message.Result := 0;
  end;
end;

{ TdxRibbonGroupsDockControlContentChangeAnimation }

constructor TdxRibbonGroupsDockControlContentChangeAnimation.Create(
  ADockControl: TdxCustomRibbonDockControl; ATime: Cardinal;
  ATransitionEffect: TdxAnimationTransitionEffect = ateLinear);
begin
  inherited Create(ATime, ATransitionEffect, 100);

  FImage := TcxBitmap32.CreateSize(ADockControl.ClientRect);
  cxPaintToBitmap(ADockControl, FImage);
  FImage.MakeOpaque;
end;

destructor TdxRibbonGroupsDockControlContentChangeAnimation.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TdxRibbonGroupsDockControlContentChangeAnimation.Draw(ACanvas: TcxCanvas; const R: TRect);
begin
  SystemAlphaBlend(ACanvas.Handle, FImage.Canvas.Handle,
    cxRectOffset(R, -MulDiv(10, Length - Position, Length), 0),
    FImage.ClientRect, MulDiv(MaxByte, Position, Length), False);
end;

{ TdxRibbonGroupsDockControl }

constructor TdxRibbonGroupsDockControl.Create(ATab: TdxRibbonTab);
begin
  inherited Create(nil);
  AllowDocking := False;
  FTab := ATab;
  FViewInfo := GetViewInfoClass.Create(Self);
end;

destructor TdxRibbonGroupsDockControl.Destroy;
begin
  FreeAndNil(FContentChangeAnimation);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxRibbonGroupsDockControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
begin
  if (Ribbon <> nil) and not Ribbon.IsLocked then
  begin
    SiteViewInfo.Calculate(GetControlRect(Ribbon.GroupsDockControlSite));
    if UseRightToLeftAlignment then
      SiteViewInfo.RightToLeftConversion(GetControlRect(Ribbon.GroupsDockControlSite));
    R := Tab.GetDockControlBounds;
    if UseRightToLeftAlignment then
    begin
      Dec(AWidth, Max(0, SiteViewInfo.TabGroupsScrollButtonRight.Bounds.Right - R.Left));
      Dec(AWidth, Max(0, R.Right - SiteViewInfo.TabGroupsScrollButtonLeft.Bounds.Left));
      Inc(ALeft, Max(0, SiteViewInfo.TabGroupsScrollButtonRight.Bounds.Right - R.Left));
    end
    else
    begin
      Dec(AWidth, Max(0, SiteViewInfo.TabGroupsScrollButtonLeft.Bounds.Right - R.Left));
      Dec(AWidth, Max(0, R.Right - SiteViewInfo.TabGroupsScrollButtonRight.Bounds.Left));
      Inc(ALeft, Max(0, SiteViewInfo.TabGroupsScrollButtonLeft.Bounds.Right - R.Left));
    end;
    if (AWidth <> Width) or (AHeight <> Height) then
      StopContentChangeAnimation;
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    UpdateGroupPositions;
    SiteViewInfo.Invalidate;
  end
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxRibbonGroupsDockControl.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if HandleAllocated and IsWindowVisible(Handle) then
    cxRedrawWindow(Handle, RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TdxRibbonGroupsDockControl.CalcRowToolbarPositions(ARowIndex, AClientSize: Integer);
begin
  if Visible then
    Tab.UpdateDockControl;
end;

procedure TdxRibbonGroupsDockControl.DblClick;
begin
  // do nothing
end;

procedure TdxRibbonGroupsDockControl.DrawBackground(ACanvas: TcxCanvas);
var
  AOffset: TPoint;
  ARect: TRect;
begin
  ARect := Ribbon.ViewInfo.GetGroupsDockControlSiteBounds;
  AOffset := cxPointOffset(ARect.TopLeft, Left, Top);

  ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, AOffset);
  Ribbon.Painter.DrawGroupsArea(ACanvas, ARect);
  ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, AOffset, False);

  if FContentChangeAnimation <> nil then
    FContentChangeAnimation.Draw(ACanvas, ClientRect);
end;

procedure TdxRibbonGroupsDockControl.FillBackground(
  DC: HDC; const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  // do nothing
end;

procedure TdxRibbonGroupsDockControl.FullInvalidate;
begin
  WinControlFullInvalidate(Self, True);
end;

function TdxRibbonGroupsDockControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonGroupsDockControlAccessibilityHelper;
end;

function TdxRibbonGroupsDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxRibbonGroupBarControl;
end;

function TdxRibbonGroupsDockControl.GetPainter: TdxBarPainter;
begin
  if Ribbon <> nil then
    Result := Ribbon.GroupsPainter
  else
    Result := inherited GetPainter;
end;

function TdxRibbonGroupsDockControl.GetViewInfoClass: TdxRibbonGroupsDockControlViewInfoClass;
begin
  Result := TdxRibbonGroupsDockControlViewInfo;
end;

function TdxRibbonGroupsDockControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := FContentChangeAnimation <> nil;
end;

function TdxRibbonGroupsDockControl.IsMultiRow: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonGroupsDockControl.MakeRectFullyVisible(const R: TRect);
var
  ANewLeft: Integer;
begin
  if (R.Left < 0) or (R.Right > ClientWidth) then
  begin
    if (cxRectWidth(R) > ClientWidth) or (R.Left < 0) then
      ANewLeft := 0
    else
      ANewLeft := ClientWidth - cxRectWidth(R);

    if ANewLeft <> R.Left then
      ViewInfo.InternalScrollGroups(ANewLeft - R.Left, cxRectWidth(Ribbon.ViewInfo.GetTabGroupsDockControlBounds));
  end;
end;

procedure TdxRibbonGroupsDockControl.Paint;
begin
  cxPaintCanvas.BeginPaint(Canvas);
  try
    DrawBackground(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxRibbonGroupsDockControl.RecalculateBars;
var
  AToolbar: TdxRibbonGroupBarControl;
  I: Integer;
begin
  for I := 0 to ViewInfo.GroupCount - 1 do
  begin
    AToolbar := ViewInfo.GroupViewInfos[I].BarControl;
    if AToolbar <> nil then
      AToolbar.RepaintBar;
  end;
end;

procedure TdxRibbonGroupsDockControl.SetSize;
begin
  // do nothing
end;

procedure TdxRibbonGroupsDockControl.ShowCustomizePopup;
begin
  if Ribbon.IsDesigning then
    ShowDesignMenu;
end;

procedure TdxRibbonGroupsDockControl.StopContentChangeAnimation;
begin
  if FContentChangeAnimation <> nil then
    FContentChangeAnimation.Terminate;
end;

procedure TdxRibbonGroupsDockControl.UpdateGroupPositions(AForceUpdate: Boolean = False);
var
  AHandle: THandle;
  AOffset: Integer;
  ARect: TRect;
  AToolbar: TdxRibbonGroupBarControl;
  AVisible: Boolean;
  I: Integer;
begin
  AVisible := FContentChangeAnimation = nil;
  AHandle := BeginDeferWindowPos(ViewInfo.GroupCount);
  try
    if Ribbon.UseRightToLeftAlignment then
      AOffset := Width - ViewInfo.FirstGroupPosition
    else
      AOffset := ViewInfo.FirstGroupPosition;
    for I := 0 to ViewInfo.GroupCount - 1 do
    begin
      AToolbar := ViewInfo.GroupViewInfos[I].BarControl;
      if Ribbon.UseRightToLeftAlignment then
        AOffset := AOffset - AToolbar.ViewInfo.GetSize.cx;
      ARect := cxRectOffset(cxRect(AToolbar.ViewInfo.GetSize), AOffset, 0);
      if AForceUpdate or not cxRectIsEqual(AToolbar.BoundsRect, ARect) then
      begin
        if AToolbar.HandleAllocated then
        begin
          DeferWindowPos(AHandle, AToolbar.Handle, 0,
            ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect),
            SWP_DRAWFRAME or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER or
            IfThen(AVisible, SWP_SHOWWINDOW, SWP_HIDEWINDOW))
        end
        else
        begin
          AToolbar.BoundsRect := ARect;
          AToolbar.Visible := AVisible;
        end;
      end;
      if Ribbon.UseRightToLeftAlignment then
        AOffset := ARect.Left - Painter.GetToolbarsOffsetForAutoAlign
      else
        AOffset := ARect.Right + Painter.GetToolbarsOffsetForAutoAlign;
    end;
  finally
    EndDeferWindowPos(AHandle);
  end;
end;

procedure TdxRibbonGroupsDockControl.VisibleChanged;
begin
  FreeAndNil(FContentChangeAnimation);
  if HandleAllocated and Visible then
  begin
    Tab.UpdateDockControl;
    RepaintBarControls;

    if CanAnimateContentChanging then
    begin
      FContentChangeAnimation := TdxRibbonGroupsDockControlContentChangeAnimation.Create(Self, dxRibbonTabGroupContentShowAnimationTime);
      FContentChangeAnimation.OnTerminate := HandlerAnimationTerminated;
      FContentChangeAnimation.OnAnimate := HandlerAnimate;
      FContentChangeAnimation.Resume;

      UpdateGroupPositions(True);
    end;
  end;
end;

function TdxRibbonGroupsDockControl.CanAnimateContentChanging: Boolean;
begin
  Result := (dxRibbonTabGroupContentShowAnimationTime > 0) and (Ribbon.Style >= rs2019) and not Ribbon.IsDesigning and
    (not Ribbon.IsPopupGroupsMode or Ribbon.TabGroupsPopupWindow.SwitchingBetweenTabs) and
     not BarNavigationController.KeyTipsHandlingMode and not Ribbon.IsAutoHideAnimationIsInProgress;
end;

function TdxRibbonGroupsDockControl.GetRibbon: TdxCustomRibbon;
begin
  Result := Tab.Ribbon;
end;

function TdxRibbonGroupsDockControl.GetSiteViewInfo: TdxRibbonGroupsDockControlSiteViewInfo;
begin
  Result := Ribbon.ViewInfo.GroupsDockControlSiteViewInfo;
end;

procedure TdxRibbonGroupsDockControl.HandlerAnimate(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  Invalidate;
end;

procedure TdxRibbonGroupsDockControl.HandlerAnimationTerminated(Sender: TObject);
begin
  FContentChangeAnimation := nil;
  UpdateGroupPositions(True);
  RepaintBarControls;
end;

procedure TdxRibbonGroupsDockControl.DesignMenuClick(Sender: TObject);
begin
  case TdxBarButton(Sender).Tag of
    0: Ribbon.Tabs.Add.DesignSelectionHelper.SelectComponent;
    1: Ribbon.DesignAddTabGroup(Tab, False);
    2: Ribbon.DesignAddTabGroup(Tab, True);
  end;
end;

procedure TdxRibbonGroupsDockControl.InitDesignMenu(AItemLinks: TdxBarItemLinks);
begin
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDTAB), DesignMenuClick, 0);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDEMPTYGROUP), DesignMenuClick, 1, True);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDGROUPWITHTOOLBAR), DesignMenuClick, 2);
end;

procedure TdxRibbonGroupsDockControl.ShowDesignMenu;
begin
  BarDesignController.ShowCustomCustomizePopup(BarManager, InitDesignMenu, Painter);
end;

procedure TdxRibbonGroupsDockControl.WMGestureNotify(var Message: TWMGestureNotify);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.Unused, LPARAM(Message.NotifyStruct));
end;

procedure TdxRibbonGroupsDockControl.WMPaint(var Message: TWMPaint);
begin
  if (Message.DC <> 0) or not IsDoubleBufferedNeeded then
    inherited
  else
    dxBufferedPaintControl(Self);
end;

{ TdxRibbonGroupsDockControlViewInfo }

constructor TdxRibbonGroupsDockControlViewInfo.Create(ADockControl: TdxRibbonGroupsDockControl);
begin
  inherited Create;
  FDockControl := ADockControl;
  FScrollButtons := [];
end;

procedure TdxRibbonGroupsDockControlViewInfo.Calculate(const ABoundsRect: TRect);

  function AllGroupsFitIn: Boolean;
  begin
    Result := TryPlaceGroups(cxRectWidth(ABoundsRect));
  end;

  procedure AfterCalculate;
  var
    I: Integer;
  begin
    for I := 0 to GroupCount - 1 do
      GroupViewInfos[I].AfterCalculate;
  end;

  procedure BeforeCalculate;
  var
    I: Integer;
  begin
    for I := 0 to GroupCount - 1 do
      GroupViewInfos[I].BeforeCalculate(GroupViewInfos[I].BarControl.Group.Restriction = rtgrNoExpand);
  end;

  procedure CalculateGroups;
  var
    I: Integer;
  begin
    for I := 0 to GroupCount - 1 do
      GroupViewInfos[I].Calculate;
  end;

  procedure ReduceGroupsInit;
  var
    I: Integer;
  begin
    for I := 0 to GroupCount - 1 do
      GroupViewInfos[I].ReduceInit;
  end;

  function ReduceGroups(AStage: TdxRibbonGroupsReduceStage; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
  var
    I: Integer;
  begin
    I := GroupCount - 1;
    repeat
      if not GroupViewInfos[I].Reduce(AStage, AUpToViewLevel) then
        Dec(I);
      Result := AllGroupsFitIn
    until Result or (I < 0);
  end;

var
  AGroupsReduceStage: TdxRibbonGroupsReduceStage;
  AUpToViewLevel: TdxBarItemRealViewLevel;
begin
  FIsRightToLeftConverted := False;
  BeforeCalculate;
  try
    CalculateGroups;
    if not AllGroupsFitIn then
    begin
      ReduceGroupsInit;
      for AGroupsReduceStage := Low(TdxRibbonGroupsReduceStage) to High(TdxRibbonGroupsReduceStage) do
      begin
        if AGroupsReduceStage = rgrsItemControlsViewLevel then
        begin
          for AUpToViewLevel := Succ(Low(TdxBarItemRealViewLevel)) to High(TdxBarItemRealViewLevel) do
            if ReduceGroups(AGroupsReduceStage, AUpToViewLevel) then
              Break;
        end
        else
          if ReduceGroups(AGroupsReduceStage, ivlLargeIconWithText) then
            Break;
        if AllGroupsFitIn then
          Break;
      end;
    end;
  finally
    AfterCalculate;
  end;
  CalculateGroupsScrollInfo(cxRectWidth(ABoundsRect));
  CheckGroupsCollapsedStates;
end;

procedure TdxRibbonGroupsDockControlViewInfo.ResetScrollInfo;
begin
  FScrollPosition := 0;
  FScrollButtons := [];
end;

procedure TdxRibbonGroupsDockControlViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxRibbonGroupsDockControlViewInfo.ScrollGroups(AScrollLeft: Boolean;
  AMaxContentWidth: Integer);
begin
  if AScrollLeft then
    InternalScrollGroups(-dxRibbonGroupsScrollDelta, AMaxContentWidth)
  else
    InternalScrollGroups(dxRibbonGroupsScrollDelta, AMaxContentWidth);
end;

procedure TdxRibbonGroupsDockControlViewInfo.CalculateGroupsScrollInfo(
  AMaxContentWidth: Integer);
var
  ATotalGroupsWidth: Integer;
begin
  ATotalGroupsWidth := TotalGroupsWidth;
  if ATotalGroupsWidth <= AMaxContentWidth then
  begin
    FScrollButtons := [];
    FScrollPosition := 0;
  end
  else
  begin
    if FScrollButtons = [] then
      FScrollButtons := [rsbRight]
    else
      if FScrollButtons = [rsbLeft] then
        FScrollPosition := AMaxContentWidth - ATotalGroupsWidth
      else
        if FScrollButtons = [rsbLeft, rsbRight] then
        begin
          if FScrollPosition + ATotalGroupsWidth <= AMaxContentWidth then
          begin
            FScrollButtons := [rsbLeft];
            FScrollPosition := AMaxContentWidth - ATotalGroupsWidth;
          end;
        end;
  end;
end;

procedure TdxRibbonGroupsDockControlViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
    GroupViewInfos[I].DoRightToLeftConversion(ABounds);
end;

procedure TdxRibbonGroupsDockControlViewInfo.InternalScrollGroups(
  ADelta: Integer; AMaxContentWidth: Integer);

  procedure CheckScrollPosition;
  begin
    if FScrollPosition > 0 then
      FScrollPosition := 0
    else
      FScrollPosition := Max(FScrollPosition, AMaxContentWidth - TotalGroupsWidth);
  end;

begin
  Inc(FScrollPosition, ADelta);
  CheckScrollPosition;
  FScrollButtons := [];
  if FScrollPosition < 0 then
    Include(FScrollButtons, rsbLeft);
  if FScrollPosition + TotalGroupsWidth > AMaxContentWidth then
    Include(FScrollButtons, rsbRight);
  DockControl.Tab.UpdateDockControlBounds;
end;

procedure TdxRibbonGroupsDockControlViewInfo.CheckGroupsCollapsedStates;
var
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
    GroupViewInfos[I].CheckGroupCollapsedStates;
end;

function TdxRibbonGroupsDockControlViewInfo.GetFirstGroupPosition: Integer;
var
  ATabGroupsDockControlOffset: TRect;
begin
  Result := FScrollPosition;
  if rsbLeft in ScrollButtons then
  begin
    ATabGroupsDockControlOffset := DockControl.Ribbon.ViewInfo.GetTabGroupsDockControlOffset;
    if DockControl.UseRightToLeftAlignment then
      Dec(Result, DockControl.Parent.ClientWidth - DockControl.BoundsRect.Right - ATabGroupsDockControlOffset.Left)
    else
      Dec(Result, DockControl.Left - ATabGroupsDockControlOffset.Left);
  end;
end;

function TdxRibbonGroupsDockControlViewInfo.GetGroupCount: Integer;
var
  AToolbar: TdxBar;
  I: Integer;
begin
  Result := 0;
  for I := 0 to DockControl.Tab.Groups.Count - 1 do
  begin
    AToolbar := DockControl.Tab.Groups[I].ToolBar;
    if IsValidToolbar(AToolbar) then
      Inc(Result);
  end;
end;

function TdxRibbonGroupsDockControlViewInfo.GetGroupViewInfo(
  AIndex: Integer): TdxRibbonGroupBarControlViewInfo;
var
  AToolbar: TdxBar;
  I: Integer;
begin
  Result := nil;
  for I := 0 to DockControl.Tab.Groups.Count - 1 do
  begin
    AToolbar := DockControl.Tab.Groups[I].ToolBar;
    if IsValidToolbar(AToolbar) then
      if AIndex = 0 then
      begin
        Result := (AToolBar.Control as TdxRibbonGroupBarControl).ViewInfo;
        Break;
      end
      else
        Dec(AIndex);
  end;
end;

function TdxRibbonGroupsDockControlViewInfo.IsValidToolbar(AToolbar: TdxBar): Boolean;
begin
  Result := (AToolbar <> nil) and (AToolbar.Control <> nil) and (AToolbar.Control.DockControl = DockControl);
end;

function TdxRibbonGroupsDockControlViewInfo.TotalGroupsWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GroupCount - 1 do
    Inc(Result, GroupViewInfos[I].GetSize.cx);
  if GroupCount > 1 then
    Inc(Result, DockControl.Painter.GetToolbarsOffsetForAutoAlign * (GroupCount - 1));
end;

function TdxRibbonGroupsDockControlViewInfo.TryPlaceGroups(
  AMaxContentWidth: Integer): Boolean;
var
  AGroupWidth, I, X: Integer;
begin
  Result := True;
  X := 0;
  for I := 0 to GroupCount - 1 do
  begin
    AGroupWidth := GroupViewInfos[I].GetSize.cx;
    Result := X + AGroupWidth <= AMaxContentWidth;
    if not Result then
      Break;
    Inc(X, AGroupWidth + DockControl.Painter.GetToolbarsOffsetForAutoAlign);
  end;
end;

{ TdxRibbonTabGroupsPopupWindow }

constructor TdxRibbonTabGroupsPopupWindow.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(ARibbon);
  FRibbon := ARibbon;
  FShadow := TdxBarShadow.Create(Self);
  ModalMode := False;
end;

destructor TdxRibbonTabGroupsPopupWindow.Destroy;
begin
  FreeAndNil(FShadow);
  inherited Destroy;
end;

function TdxRibbonTabGroupsPopupWindow.CalculatePosition(const ASize: TSize): TPoint;
begin
  Result := GetBounds.TopLeft;
end;

function TdxRibbonTabGroupsPopupWindow.CalculateSize: TSize;
var
  R: TRect;
begin
  R := GetBounds;
  Result := cxRectSize(R);
  SetBounds(R.Left, R.Top, Result.cx, Result.cy);
end;

function TdxRibbonTabGroupsPopupWindow.CanShowShadow: Boolean;
begin
  Result := (Ribbon.Style < rs2013) and dxCanUseShadows;
end;

procedure TdxRibbonTabGroupsPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS;
end;

procedure TdxRibbonTabGroupsPopupWindow.Deactivate;
begin
  if (ActiveBarControl = nil) or not (bsHideAll in TCustomdxBarControlAccess(ActiveBarControl).FState) then
    inherited Deactivate;
end;

procedure TdxRibbonTabGroupsPopupWindow.DoClosed;
begin
  Ribbon.UpdateFormActionControl(False);
  inherited DoClosed;
  FShadow.Visible := False;
  GroupsDockControlSite.BoundsRect := cxEmptyRect;
  GroupsDockControlSite.Parent := Ribbon;
  Ribbon.Invalidate;
end;

procedure TdxRibbonTabGroupsPopupWindow.DoShowed;
begin
  inherited DoShowed;
  FSwitchingBetweenTabs := False;
  if CanShowShadow then
  begin
    FShadow.SetOwnerBounds(cxEmptyRect, BoundsRect);
    FShadow.Visible := True;
  end;
end;

procedure TdxRibbonTabGroupsPopupWindow.DoShowing;
begin
  Ribbon.UpdateFormActionControl(True);
  inherited DoShowing;
  SetGroupsDockControlSite;
end;

procedure TdxRibbonTabGroupsPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  HandleNavigationKey(Key);
end;

procedure TdxRibbonTabGroupsPopupWindow.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Word(Key) = VK_ESCAPE then
    CloseUp
  else
    HandleNavigationKey(Word(Key));
end;

function TdxRibbonTabGroupsPopupWindow.NeedIgnoreMouseMessageAfterCloseUp(
  AWnd: THandle; AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean;
var
  AForm: TCustomForm;
  AHitInfo: TdxRibbonHitInfo;
begin
  Result := False;
  if AWnd = Ribbon.Handle then
  begin
    if (AMsg = WM_LBUTTONDOWN) and not (ssDouble in AShift) then
    begin
      AHitInfo := TdxRibbonHitInfo.Create(Ribbon);
      try
        AHitInfo.Calculate(Ribbon.ScreenToClient(APos));
        Result := AHitInfo.HitTest = rhtTab;
        if Result then
          FSwitchingBetweenTabs := AHitInfo.HitObjectAsTab <> Ribbon.ActiveTab;
      finally
        AHitInfo.Free;
      end;
    end;
  end
  else
  begin
    AForm := GetParentForm(Ribbon);
    if (AForm.Handle <> AWnd) and not dxHasAsParent(AWnd, Ribbon.Handle) then
      Result := not Ribbon.DoHideMinimizedByClick(AWnd, AShift, APos)
    else
      Result := False;
  end;
end;

procedure TdxRibbonTabGroupsPopupWindow.HandleNavigationKey(AKey: Word);
begin
  if BarNavigationController.IsNavigationKey(AKey) then
  begin
    BarNavigationController.SetKeyTipsShowingState(nil, '');
    SelectFirstSelectableAccessibleObject(GroupsDockControlSite.DockControl.IAccessibilityHelper.GetBarHelper);
  end;
end;

procedure TdxRibbonTabGroupsPopupWindow.SetGroupsDockControlSite;
var
  R: TRect;
  ATabGroupsDockControlOffset: TRect;
begin
  if Ribbon.ActiveTab <> nil then
  begin
    GroupsDockControlSite.Parent := Self;
    GroupsDockControlSite.BoundsRect := GetControlRect(Self);
    if Ribbon.UseRightToLeftAlignment then
      ATabGroupsDockControlOffset := TdxRightToLeftLayoutConverter.ConvertOffsets(Ribbon.ViewInfo.GetTabGroupsDockControlOffset)
    else
      ATabGroupsDockControlOffset := Ribbon.ViewInfo.GetTabGroupsDockControlOffset;
    R := cxRectContent(GroupsDockControlSite.BoundsRect, ATabGroupsDockControlOffset);
    GroupsDockControlSite.DockControl.ViewInfo.ResetScrollInfo;
    GroupsDockControlSite.DockControl.HandleNeeded;
    GroupsDockControlSite.DockControl.ViewInfo.Calculate(R);
    GroupsDockControlSite.DockControl.BoundsRect := R;
    GroupsDockControlSite.DockControl.Visible := True;
  end;
end;

function TdxRibbonTabGroupsPopupWindow.GetBounds: TRect;
var
  AMonitorRect, ARibbonRect, ATabsRect: TRect;
  ATabGroupsHeight: Integer;
begin
  ARibbonRect := dxMapWindowRect(Ribbon.Handle, 0, Ribbon.ClientRect);
  ATabsRect := dxMapWindowRect(Ribbon.Handle, 0, Ribbon.ViewInfo.TabsAreaViewInfo.TabsViewInfo.Bounds);
  Result := cxRect(ARibbonRect.Left, ATabsRect.Top - 1, ARibbonRect.Right, ATabsRect.Bottom);
  Dec(Result.Bottom, Ribbon.SkinGetPartSize(DXBAR_TABSGROUPSOVERLAPHEIGHT));
  ATabGroupsHeight := Ribbon.ViewInfo.GetTabGroupsHeight(True);
  AMonitorRect := GetMonitorWorkArea(0);
  cxRectIntersect(Result, Result, AMonitorRect);
  if (Result.Bottom + ATabGroupsHeight > AMonitorRect.Bottom) and (Ribbon.Style < rs2013) then
    Result := cxRect(Result.Left, Result.Top - ATabGroupsHeight, Result.Right, Result.Top)
  else
    Result := cxRect(Result.Left, Result.Bottom, Result.Right, Result.Bottom + ATabGroupsHeight);
end;

function TdxRibbonTabGroupsPopupWindow.GetGroupsDockControlSite: TdxRibbonGroupsDockControlSite;
begin
  Result := Ribbon.GroupsDockControlSite;
end;

procedure TdxRibbonTabGroupsPopupWindow.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  AFlags: Integer;
  ARgn: HRGN;
begin
  AFlags := DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE;
  if Message.WParam <> 1 then
  begin
    ARgn := CreateRectRgnIndirect(cxEmptyRect);
    CombineRgn(ARgn, Message.WParam, 0, RGN_COPY);
    AFlags := AFlags or DCX_INTERSECTRGN;
  end
  else
    ARgn := 0;

  DC := GetDCEx(Handle, ARgn, AFlags);
  try
    Ribbon.ColorScheme.DrawTabGroupsArea(DC, ClientRect, False, False);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TdxRibbonTabGroupsPopupWindow.WMShowWindow(var Message: TWMShowWindow);
const
  FlagsMap: array[Boolean] of Integer = (AW_SLIDE or AW_VER_NEGATIVE or AW_HIDE, AW_SLIDE or AW_VER_POSITIVE);
var
  AAnimationTime: Integer;
begin
  if AllowShowHideAnimation and not FSwitchingBetweenTabs and Assigned(AnimateWindowProc) then
  begin
    if Message.Show then
      AAnimationTime := dxRibbonTabGroupsPopupWindowShowAnimationTime
    else
      AAnimationTime := dxRibbonTabGroupsPopupWindowHideAnimationTime;

    AnimateWindowProc(Handle, Max(AAnimationTime, 1), FlagsMap[Message.Show]);
  end;
  inherited;
end;

procedure TdxRibbonTabGroupsPopupWindow.WMSize(var Message: TWMSize);
begin
  inherited;
  if Ribbon.UseRoundedWindowCorners then
    SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Message.Width + 1, Message.Height + 1, 4, 4), True);
end;

{ TdxRibbonCustomBarControl }

function TdxRibbonCustomBarControl.AllowSelectionFrame: Boolean;
begin
  Result := True;
end;

function TdxRibbonCustomBarControl.AllowFade: Boolean;
begin
  Result := (Ribbon <> nil) and Ribbon.CanFade;
end;

procedure TdxRibbonCustomBarControl.InitializeForDock(ABar: TdxBar);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    ControlStyle := ControlStyle - [csDoubleClicks];
end;

function TdxRibbonCustomBarControl.AllowQuickCustomizing: Boolean;
begin
  Result := False;
end;

function TdxRibbonCustomBarControl.CanAlignControl(AControl: TdxBarItemControl): Boolean;
begin
  Result := True;
end;

function TdxRibbonCustomBarControl.CanMoving: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonCustomBarControl.DrawBarParentBackground(ACanvas: TcxCanvas);
begin
end;

function TdxRibbonCustomBarControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonBarControlAccessibilityHelper;
end;

function TdxRibbonCustomBarControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := dxRibbonBarBehaviorOptions;
end;

function TdxRibbonCustomBarControl.GetEditFont: TFont;
var
  ARibbon: TdxCustomRibbon;
begin
  ARibbon := Ribbon;
  if (ARibbon <> nil) and not ARibbon.IsDestroying then
    Result := ARibbon.Fonts.GetGroupFont
  else
    Result := inherited GetEditFont;
end;

function TdxRibbonCustomBarControl.GetFont: TFont;
var
  ARibbon: TdxCustomRibbon;
begin
  ARibbon := Ribbon;
  if (ARibbon <> nil) and not ARibbon.IsDestroying then
    Result := ARibbon.Fonts.GetGroupFont
  else
    Result := inherited GetFont;
end;

function TdxRibbonCustomBarControl.GetFullItemRect(Item: TdxBarItemControl): TRect;
begin
  Result := GetItemRect(Item);
end;

function TdxRibbonCustomBarControl.GetIsMainMenu: Boolean;
begin
  Result := False;
end;

function TdxRibbonCustomBarControl.GetMultiLine: Boolean;
begin
  Result := False;
end;

function TdxRibbonCustomBarControl.MarkExists: Boolean;
begin
  Result := False;
end;

function TdxRibbonCustomBarControl.NotHandleMouseMove(
  ACheckLastMousePos: Boolean = True): Boolean;
begin
  Result := inherited NotHandleMouseMove(ACheckLastMousePos) or dxBarHasPopupWindowAbove(Self, True);
end;

function TdxRibbonCustomBarControl.RealMDIButtonsOnBar: Boolean;
begin
  Result := False;
end;

function TdxRibbonCustomBarControl.ClickAtHeader: Boolean;
var
  R: TRect;
begin
  R := WindowRect;
  R.Top := R.Bottom - (Height - ClientBounds.Bottom);
  Result := cxRectPtIn(R, GetMouseCursorPos);
end;

procedure TdxRibbonCustomBarControl.DoPopupMenuClick(Sender: TObject);
begin
  Ribbon.PopupMenuItemClick(Sender);
end;

function TdxRibbonCustomBarControl.GetPopupMenuItems: TdxRibbonPopupMenuItems;
begin
  Result := Ribbon.GetValidPopupMenuItems - [rpmiItems];
end;

procedure TdxRibbonCustomBarControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
begin
  Ribbon.PopulatePopupMenuItems(AItemLinks, GetPopupMenuItems, PopupMenuClick);
end;

procedure TdxRibbonCustomBarControl.PopupMenuClick(Sender: TObject);
var
  ALinkSelf: TcxObjectLink;
begin
  ALinkSelf := cxAddObjectLink(Self);
  try
    DoPopupMenuClick(Sender);
    if ALinkSelf.Ref <> nil then
      HideAll;
  finally
    cxRemoveObjectLink(ALinkSelf);
  end;
end;

procedure TdxRibbonCustomBarControl.ShowPopup(AItem: TdxBarItemControl);
var
  AItemLink: TdxBarItemLink;
begin
  if not BarManager.IsCustomizing then
  begin
    if AItem <> nil then
      AItemLink := AItem.ItemLink
    else
      AItemLink := nil;

    if (AItemLink <> nil) or ClickAtHeader then
      BarDesignController.ShowCustomCustomizePopup(BarManager, InitCustomizationPopup, Painter, Self, AItemLink);
  end
  else
    inherited;
end;

function TdxRibbonCustomBarControl.GetQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
var
  ARibbon: TdxCustomRibbon;
begin
  ARibbon := Ribbon;
  if (ARibbon <> nil) and not ARibbon.IsDestroying then
    Result := ARibbon.QuickAccessToolbar
  else
    Result := nil;
end;

function TdxRibbonCustomBarControl.GetScaleFactor: TdxScaleFactor;
var
  ARibbon: TdxCustomRibbon;
begin
  ARibbon := Ribbon;
  if (ARibbon <> nil) and not ARibbon.IsDestroying then
    Result := ARibbon.ScaleFactor
  else
    Result := inherited GetScaleFactor;
end;

procedure TdxRibbonCustomBarControl.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
begin
  R := cxRectOffset(ClientRect, ClientToScreen(cxNullPoint));
  inherited;
  if cxRectPtIn(R, SmallPointToPoint(Message.Pos)) then
  begin
    if HitTest = HTCAPTION then
      HitTest := HTCLIENT;
  end
  else
  begin
    Message.Result := HTCLIENT;
    HitTest := HTCLIENT;
  end;
end;

{ TdxRibbonCustomToolbarBarControl }

constructor TdxRibbonCustomToolbarBarControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TcxBitmap32.Create;
end;

destructor TdxRibbonCustomToolbarBarControl.Destroy;
begin
  if DockControl <> nil then
    DockControl.Visible := False;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TdxRibbonCustomToolbarBarControl.AllItemsVisible: Boolean;
var
  AItemLink: TdxBarItemLink;
  I: Integer;
begin
  Result := True;
  for I := 0 to ItemLinks.CanVisibleItemCount - 1 do
  begin
    AItemLink := ItemLinks.CanVisibleItems[I];
    if (AItemLink.VisibleIndex = -1) or (AItemLink.Control <> nil) and IsRectEmpty(AItemLink.ItemRect) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TdxRibbonCustomToolbarBarControl.CalcControlsPositions;

  procedure CalcItemControlsRealPositionInButtonGroup;
  var
    AItemControlViewInfos: TList;
    AItemLink: TdxBarItemLink;
    I: Integer;
  begin
    for I := 0 to ItemLinks.VisibleItemCount - 1 do
    begin
      AItemLink := ItemLinks.VisibleItems[I];
      if AItemLink.Control = nil then
        AItemLink.CreateControl;
    end;

    AItemControlViewInfos := TList.Create;
    try
      for I := 0 to ItemLinks.VisibleItemCount - 1 do
        AItemControlViewInfos.Add(Pointer(IdxBarItemControlViewInfo(ItemLinks.VisibleItems[I].Control.ViewInfo)));
      dxRibbonGroupLayoutCalculator.CalcItemControlsRealPositionInButtonGroup(AItemControlViewInfos);
    finally
      AItemControlViewInfos.Free;
    end;
  end;

var
  AItemControlWidth, ASeparatorWidth, I, X: Integer;
  AItemLink: TdxBarItemLink;
  R, AItemRect: TRect;
begin
  TdxBarItemLinksAccess(ItemLinks).BeginCalcItemRects;
  try
    R := GetClientOffset;
    TdxBarItemLinksAccess(ItemLinks).EmptyItemRects;
    X := R.Left;
    Truncated := False;
    AItemLink := nil;
    if TdxBarControlViewInfoAccess(ViewInfo).CanShowButtonGroups then
      CalcItemControlsRealPositionInButtonGroup;
    for I := 0 to ItemLinks.VisibleItemCount - 1 do
    begin
      AItemLink := ItemLinks.VisibleItems[I];
      if AItemLink.Control = nil then
        AItemLink.CreateControl;
      TdxBarItemControlAccess(AItemLink.Control).LastInRow := False;
      AItemControlWidth := TdxBarItemControlAccess(AItemLink.Control).Width;
      ASeparatorWidth := GetSeparatorWidth(AItemLink.Control);
      Truncated := X + ASeparatorWidth + AItemControlWidth > ClientWidth - GetMarkSize;
      if Truncated then
      begin
        if I > 0 then
          AItemLink := ItemLinks.VisibleItems[I - 1];
        Break;
      end;
      Inc(X, ASeparatorWidth);
      AItemRect := Rect(X, R.Top, X + AItemControlWidth, ClientHeight - R.Bottom);
      if UseRightToLeftAlignment then
        AItemRect := TdxRightToLeftLayoutConverter.ConvertRect(AItemRect, cxRectBounds(0, 0, ClientWidth, 0));
      AItemLink.ItemRect := AItemRect;
      TdxBarItemLinkAccess(AItemLink).RowHeight := ClientHeight;
      Inc(X, AItemControlWidth);
    end;
    if AItemLink <> nil then
      TdxBarItemControlAccess(AItemLink.Control).LastInRow := True;
  finally
    TdxBarItemLinksAccess(ItemLinks).EndCalcItemRects;
  end;
end;

function TdxRibbonCustomToolbarBarControl.CalcSize(AMaxWidth: Integer): TSize;
var
  AItem: TdxBarItemLink;
  AItemControl: TdxBarItemControlAccess;
  AItemControlWidth, I: Integer;
begin
  HandleNeeded;
  Result := cxSize(GetMarkSize, GetMinHeight(dsTop));
  for I := 0 to ItemLinks.CanVisibleItemCount - 1 do
  begin
    AItem := ItemLinks.CanVisibleItems[I];
    if AItem.Control = nil then
      AItem.CreateControl;
    AItemControl := TdxBarItemControlAccess(AItem.Control);
    AItemControlWidth := AItemControl.Width + GetSeparatorWidth(AItemControl);
    if Result.cx + AItemControlWidth > AMaxWidth then
      Break;
    Inc(Result.cx, AItemControlWidth);
    Result.cy := Max(Result.cy, AItemControl.Height);
  end;
  Inc(Result.cx, cxMarginsWidth(GetClientOffset));
  Inc(Result.cy, cxMarginsHeight(GetClientOffset));

  if (Result.cx = 0) and (ItemLinks.CanVisibleItemCount > 0) then
    Result.cx := Painter.MarkSizeX(Self);
end;

function TdxRibbonCustomToolbarBarControl.CanHideAllItemsInSingleLine: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonCustomToolbarBarControl.CreateWnd;
begin
  FIsWindowCreation := True;
  try
    inherited CreateWnd;
  finally
    FIsWindowCreation := False;
  end;
end;

procedure TdxRibbonCustomToolbarBarControl.DoPaintItem(AControl: TdxBarItemControl; ACanvas: TcxCanvas; const AItemRect: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(AControl.ViewInfo.Bounds);
    if NeedBufferedOnGlass(AControl) then
    begin
      FBitmap.SetSize(AItemRect);
      FBitmap.Clear;
      FBitmap.cxCanvas.WindowOrg := AItemRect.TopLeft;
      AControl.Paint(FBitmap.cxCanvas, AItemRect, GetPaintType);
      FBitmap.cxCanvas.WindowOrg := cxNullPoint;
      FBitmap.MakeOpaque;
      cxBitBlt(ACanvas.Handle, FBitmap.cxCanvas.Handle, AItemRect, cxNullPoint, SRCCOPY);
    end
    else
      AControl.Paint(ACanvas, AItemRect, GetPaintType);
  finally
    ACanvas.RestoreClipRegion;
  end;
  DrawSelectedItem(ACanvas.Handle, AControl, AItemRect);
end;

procedure TdxRibbonCustomToolbarBarControl.InitializeForDock(ABar: TdxBar);
begin
  inherited InitializeForDock(ABar);
  if ABar.DockControl <> nil then
    ABar.DockControl.Visible := True;
end;

function TdxRibbonCustomToolbarBarControl.GetClientOffset: TRect;
begin
  Result := cxNullRect;
end;

function TdxRibbonCustomToolbarBarControl.GetItemControlDefaultViewLevel(
  AItemControl: TdxBarItemControl): TdxBarItemViewLevel;
begin
  Result := AItemControl.ViewInfo.MinPossibleViewLevel;
end;

function TdxRibbonCustomToolbarBarControl.GetMarkSize: Integer;
begin
  if MarkExists then
    Result := Painter.MarkSizeX(Self)
  else
    Result := 0;
end;

function TdxRibbonCustomToolbarBarControl.GetMinHeight(AStyle: TdxBarDockingStyle): Integer;
begin
  Result := Ribbon.GetGroupRowHeight;
  if Visible then
    Result := Max(Result, inherited GetMinHeight(AStyle));
end;

function TdxRibbonCustomToolbarBarControl.GetMinWidth(AStyle: TdxBarDockingStyle): Integer;
begin
  Result := 0;
end;

function TdxRibbonCustomToolbarBarControl.GetRibbon: TdxCustomRibbon;
begin
  if DockControl <> nil then
    Result := (DockControl as TdxRibbonCustomToolbarDockControl).Ribbon
  else
    Result := nil;
end;

function TdxRibbonCustomToolbarBarControl.GetSize(AMaxWidth: Integer): TSize;
begin
  if not CanAllocateHandle(Self) and not IsPopup or FIsWindowCreation then
    Result := cxNullSize
  else
    Result := CalcSize(AMaxWidth);
end;

function TdxRibbonCustomToolbarBarControl.GetSizeForWidth(AStyle: TdxBarDockingStyle; AWidth: Integer): TSize;
begin
  Result := GetSize(AWidth);
end;

procedure TdxRibbonCustomToolbarBarControl.UpdateDoubleBuffered;
begin
  DoubleBuffered := True;
end;

function TdxRibbonCustomToolbarBarControl.GetSeparatorWidth(AItemControl: TdxBarItemControl): Integer;
begin
  if AItemControl.ItemLink.BeginGroup then
    Result := BeginGroupSize
  else
    Result := 0;
end;

procedure TdxRibbonCustomToolbarBarControl.WMPaint(var Message: TWMPaint);
begin
  if (Ribbon <> nil) and Ribbon.IsDestroying then
  begin
    Message.Result := 0;
    Exit;
  end;
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if (Ribbon <> nil) and Ribbon.ViewInfo.UseGlass then
      dxPaintWindowOnGlass(Handle, True)
    else
      dxBufferedPaintControl(Self);
  end;
end;

{ TdxRibbonCustomToolbarPainter }

procedure TdxRibbonCustomToolbarPainter.BarDrawMarkBackground(
  ABarControl: TdxBarControl; DC: HDC; ItemRect: TRect; AToolbarBrush: HBRUSH);
const
  StatesMap: array[TdxBarMarkState] of Integer = (DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED);
var
  AState: Integer;
begin
  AState := StatesMap[TdxRibbonCustomToolbarBarControl(ABarControl).MarkDrawState];
  if AState <> DXBAR_NORMAL then
    Skin.DrawBackground(DC, ItemRect, SkinGetButtonID, AState);
end;

procedure TdxRibbonCustomToolbarPainter.DrawBarMarkState(
  ABarControl: TdxBarControl; DC: HDC; const R: TRect; AState: TdxBarMarkState);
const
  StatesMap: array[TdxBarMarkState] of Integer = (DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED);
begin
  if AState <> msNone then
    Skin.DrawBackground(DC, R, SkinGetButtonID, StatesMap[AState]);
end;

function TdxRibbonCustomToolbarPainter.BarMarkItemRect(ABarControl: TdxBarControl): TRect;
begin
  Result := BarMarkRect(ABarControl);
end;

function TdxRibbonCustomToolbarPainter.BarMarkRect(ABarControl: TdxBarControl): TRect;
begin
  Result := cxGetClientRect(ABarControl);
  if ABarControl.UseRightToLeftAlignment then
    Result.Right := Result.Left + (MarkSizeX(ABarControl) - MarkButtonOffset)
  else
    Result.Left := Result.Right - (MarkSizeX(ABarControl) - MarkButtonOffset);
end;

procedure TdxRibbonCustomToolbarPainter.ComboControlDrawArrowButton(
  const ADrawParams: TdxBarEditLikeControlDrawParams; ARect: TRect; AInClientArea: Boolean);
var
  ABitmap: TcxBitmap32;
  ASaveCanvas: TcxCanvas;
begin
  if AInClientArea or not ADrawParams.BarEditControl.OnGlass then
    inherited ComboControlDrawArrowButton(ADrawParams, ARect, AInClientArea)
  else
  begin
    ABitmap := TcxBitmap32.CreateSize(ARect, True);
    try
      ABitmap.cxCanvas.WindowOrg := ARect.TopLeft;
      ASaveCanvas := ADrawParams.Canvas;
      ADrawParams.Canvas := ABitmap.cxCanvas;
      inherited ComboControlDrawArrowButton(ADrawParams, ARect, AInClientArea);
      ADrawParams.Canvas := ASaveCanvas;
      ABitmap.cxCanvas.WindowOrg := cxNullPoint;
      ABitmap.MakeOpaque;
      cxBitBlt(ADrawParams.Canvas.Handle, ABitmap.cxCanvas.Handle, ARect, cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end
end;

function TdxRibbonCustomToolbarPainter.GetToolbarContentOffsets(ABar: TdxBar;
  ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect;
begin
  Result := cxNullRect;
end;

function TdxRibbonCustomToolbarPainter.MarkButtonOffset: Integer;
begin
  Result := 0;
end;

function TdxRibbonCustomToolbarPainter.MarkButtonWidth: Integer;
begin
  Result := ((Ribbon.GetGroupRowHeight + 2) div 2) or 1;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxRibbonCustomToolbarPainter.MarkSizeX(ABarControl: TdxBarControl): Integer;
begin
  Result := MarkButtonWidth + MarkButtonOffset;
end;

procedure TdxRibbonCustomToolbarPainter.DrawButtonBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams);

  procedure InternalDrawBackground(AControlPart, ASkinPart: Integer);
  var
    ARect: TRect;
  begin
    if AControlPart >= 0 then
      ARect := TdxBarItemControlAccess(ADrawParams.BarItemControl).FParts[AControlPart]
    else
    begin
      ARect := ADrawParams.BarItemControl.ItemBounds;
      AControlPart := bcpButton;
    end;
    Skin.DrawBackground(ADrawParams.Canvas.Handle, ARect, ASkinPart, GetButtonPartState(ADrawParams, AControlPart));
  end;

begin
  if ADrawParams.SplitDropDown and ADrawParams.IsDropDown then
  begin
    InternalDrawBackground(bcpButton, SkinGetButtonMainPartID);
    InternalDrawBackground(bcpDropButton, SkinGetButtonDropDownPartID);
  end
  else
    InternalDrawBackground(-1, SkinGetButtonID);
end;

procedure TdxRibbonCustomToolbarPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
begin
  if Ribbon.ViewInfo.UseGlass and ABarControl.IsOnGlass then
    FillRect(ACanvas.Handle, ABarControl.ClientRect, GetStockObject(BLACK_BRUSH));
end;

function TdxRibbonCustomToolbarPainter.GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := cxMarginsHeight(Skin.GetContentOffsets(SkinGetButtonID));
end;

function TdxRibbonCustomToolbarPainter.GetButtonBorderWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := cxMarginsWidth(Skin.GetContentOffsets(SkinGetButtonID));
end;

{ TdxRibbonQuickAccessToolbarBarControl }

constructor TdxRibbonQuickAccessToolbarBarControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultGlyph := TdxSmartGlyph.CreateSize(16, 16);
end;

destructor TdxRibbonQuickAccessToolbarBarControl.Destroy;
begin
  FreeAndNil(FDefaultGlyph);
  inherited Destroy;
end;

function TdxRibbonQuickAccessToolbarBarControl.IsOnGlass: Boolean;
begin
  Result := Ribbon.ViewInfo.QuickAccessToolbarViewInfo.IsPaintOnGlass;
end;

function TdxRibbonQuickAccessToolbarBarControl.CalcSize(AMaxWidth: Integer): TSize;
begin
  Result := inherited CalcSize(AMaxWidth);
  if MarkExists and (Result.cx = GetMarkSize + cxMarginsWidth(GetClientOffset)) then
    Dec(Result.cx, TdxRibbonQuickAccessToolbarPainter(Painter).MarkButtonOffset);
end;

procedure TdxRibbonQuickAccessToolbarBarControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateDefaultGlyph(FDefaultGlyph);
end;

function TdxRibbonQuickAccessToolbarBarControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper;
end;

function TdxRibbonQuickAccessToolbarBarControl.GetDefaultItemGlyph: TdxSmartGlyph;
begin
  Result := FDefaultGlyph;
end;

function TdxRibbonQuickAccessToolbarBarControl.GetMarkAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper;
end;

function TdxRibbonQuickAccessToolbarBarControl.GetPopupMenuItems: TdxRibbonPopupMenuItems;
begin
  Result := inherited GetPopupMenuItems;
  if BarDesignController.CustomizingItemLink = nil then
    Exclude(Result, rpmiQATAddRemoveItem);
end;

function TdxRibbonQuickAccessToolbarBarControl.GetQuickControlClass: TdxBarPopupControlClass;
begin
  Result := TdxRibbonQuickAccessPopupBarControl;
end;

function TdxRibbonQuickAccessToolbarBarControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonQuickAccessToolbarBarControlViewInfo;
end;

function TdxRibbonQuickAccessToolbarBarControl.AllowQuickCustomizing: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonQuickAccessToolbarBarControl.InitQuickCustomizeItemLinks(AQuickControl: TdxBarPopupControl);
var
  ASubItem: TdxRibbonQuickAccessPopupSubItem;
begin
  InternalItems.Clear;
  if Ribbon.GetValidPopupMenuItems <> [] then
  begin
    ASubItem := TdxRibbonQuickAccessPopupSubItem(AQuickControl.ItemLinks.AddItem(TdxRibbonQuickAccessPopupSubItem).Item);
    BarDesignController.AddInternalItem(ASubItem, InternalItems);
    ASubItem.OnPopup := HandleQuickAccessSubItemPopup;
  end;
end;

procedure TdxRibbonQuickAccessToolbarBarControl.InitAddRemoveSubItemPopup(AItemLinks: TdxBarItemLinks);
var
  I: Integer;
  AItemLink: TdxBarItemLink;
  ASubItemButton: TdxRibbonQuickAccessPopupSubItemButton;
  ASeparator: TdxBarItem;
begin
  if ItemLinks.AvailableItemCount > 0 then
  begin
    ASeparator := AItemLinks.AddItem(TdxBarSeparator).Item;
    ASeparator.Caption := cxGetResourceString(@dxSBAR_CUSTOMIZEQAT);
    BarDesignController.AddInternalItem(ASeparator, InternalItems);

    for I := 0 to ItemLinks.AvailableItemCount - 1 do
    begin
      AItemLink := ItemLinks.AvailableItems[I];
      ASubItemButton := TdxRibbonQuickAccessPopupSubItemButton(AItemLinks.AddItem(TdxRibbonQuickAccessPopupSubItemButton).Item);
      ASubItemButton.Tag := TdxNativeInt(AItemLink);
      ASubItemButton.ButtonStyle := bsChecked;
      ASubItemButton.Down := AItemLink.Visible;
      BarDesignController.AddInternalItem(ASubItemButton, InternalItems);
      ASubItemButton.Caption := AItemLink.Caption;
      if AItemLink.Visible then
        ASubItemButton.Hint := RemoveAccelChars(cxGetResourceString(@dxSBAR_REMOVEFROMQAT))
      else
        ASubItemButton.Hint := RemoveAccelChars(cxGetResourceString(@dxSBAR_ADDTOQAT));
    end;
  end;
  QuickAccessToolbar.UpdateMenuItems(AItemLinks);
end;

procedure TdxRibbonQuickAccessToolbarBarControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
begin
  Ribbon.PopulatePopupMenuItems(AItemLinks, GetPopupMenuItems, PopupMenuClick);
end;

function TdxRibbonQuickAccessToolbarBarControl.MarkExists: Boolean;
begin
  Result := Truncated or (Ribbon.GetValidPopupMenuItems <> []);
end;

procedure TdxRibbonQuickAccessToolbarBarControl.RemoveItemFromQAT;
begin
  if BarDesignController.CustomizingItemLink.Item is TdxRibbonQuickAccessGroupButton then
    BarDesignController.DeleteCustomizingItem
  else
    if BarDesignController.CustomizingItemLink.OriginalItemLink <> nil then
      BarDesignController.CustomizingItemLink.OriginalItemLink.Free
    else
      BarDesignController.DeleteCustomizingItemLink;
end;

procedure TdxRibbonQuickAccessToolbarBarControl.ShowPopup(AItem: TdxBarItemControl);
var
  AItemLink: TdxBarItemLink;
begin
  if not BarManager.IsCustomizing then
  begin
    if AItem <> nil then
      AItemLink := AItem.ItemLink
    else
      AItemLink := nil;
    BarDesignController.ShowCustomCustomizePopup(BarManager, InitCustomizationPopup, Ribbon.GroupsPainter, Self, AItemLink);
  end
  else
    inherited;
end;

procedure TdxRibbonQuickAccessToolbarBarControl.UpdateDefaultGlyph(AGlyph: TdxSmartGlyph);
var
  ABitmap: TcxBitmap32;
  AGlyphSize: Integer;
begin
  if Ribbon = nil then Exit;

  AGlyphSize := Painter.GetSmallIconSize(ScaleFactor);
  ABitmap := TcxBitmap32.CreateSize(AGlyphSize, AGlyphSize, True);
  try
    Ribbon.ColorScheme.DrawQuickAccessToolbarDefaultGlyph(ABitmap.Canvas.Handle, ABitmap.ClientRect);
    AGlyph.Assign(ABitmap);
    AGlyph.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
  finally
    ABitmap.Free;
  end;
end;

function TdxRibbonQuickAccessToolbarBarControl.GetViewInfo: TdxRibbonQuickAccessToolbarBarControlViewInfo;
begin
  Result := TdxRibbonQuickAccessToolbarBarControlViewInfo(FViewInfo);
end;

{ TdxRibbonQuickAccessToolbarBarControlViewInfo }

function TdxRibbonQuickAccessToolbarBarControlViewInfo.IsLastVisibleItemControl(AItemControl: TdxBarItemControl): Boolean;
begin
  Result := TdxBarItemControlAccess(AItemControl).LastInRow;
end;

{ TdxRibbonQuickAccessItemControlPainter }

function TdxRibbonQuickAccessToolbarPainter.GetGlyphColorPalette(
  ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette;
begin
  if APaintType = ptMenu then
    Result := ColorScheme.GetMenuColorPalette(AState)
  else
    Result := ColorScheme.GetQuickAccessToolbarColorPalette(AState, QuickAccessToolbarViewInfo.AtBottom);
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetArrowID: Integer;
begin
  Result := DXRIBBON_QAT_ARROWDOWN;
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetButtonDropDownPartID: Integer;
begin
  Result := DXRIBBON_QAT_SMALLBUTTON_DROPBUTTON;
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetButtonMainPartID: Integer;
begin
  Result := DXRIBBON_QAT_SMALLBUTTON_GLYPH;
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetMarkArrowID: Integer;
begin
  Result := DXRIBBON_QAT_MARKARROW;
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetTruncateMarkID: Integer;
begin
  Result := DXRIBBON_QAT_MARKTRUNCATED;
end;

function TdxRibbonQuickAccessToolbarPainter.SkinGetButtonID: Integer;
begin
  Result := DXRIBBON_QAT_SMALLBUTTON;
end;

procedure TdxRibbonQuickAccessToolbarPainter.DrawGroupButtonControl(ADrawParams: TdxBarButtonLikeControlDrawParams; const ARect: TRect);
var
  R: TRect;
begin
  Skin.DrawBackground(ADrawParams.Canvas.Handle, ARect, DXRIBBON_QAT_GROUPBUTTON, GetButtonPartState(ADrawParams, bcpButton));
  R := cxRectContent(ARect, Skin.GetContentOffsets(DXRIBBON_QAT_GROUPBUTTON));
  DrawGlyph(ADrawParams.BarItemControl, ADrawParams.Canvas.Handle,
    R, R, ptHorz, False, False, False, False, False, True, False, False);
end;

procedure TdxRibbonQuickAccessToolbarPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
begin
  inherited DrawToolbarContentPart(ABarControl, ACanvas);

  ACanvas.SaveDC;
  try
    ACanvas.WindowOrg := dxMapWindowPoint(ABarControl.Handle, Ribbon.Handle, ACanvas.WindowOrg);
    if QuickAccessToolbarViewInfo.AtNonClientArea and Assigned(Ribbon.FormCaptionHelper) then
      Ribbon.FormCaptionHelper.UpdateCaptionArea(ACanvas)
    else
    begin
      QuickAccessToolbarViewInfo.DrawBackground(ACanvas);
      QuickAccessToolbarViewInfo.Draw(ACanvas);
    end;
  finally
    ACanvas.RestoreDC;
  end;
end;

function TdxRibbonQuickAccessToolbarPainter.MarkButtonOffset: Integer;
begin
  Result := ColorScheme.GetQuickAccessToolbarMarkButtonOffset(
    Ribbon.ApplicationButtonViewInfo.IsVisible,
    Ribbon.QuickAccessToolbar.Position = qtpBelowRibbon);
end;

function TdxRibbonQuickAccessToolbarPainter.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := Ribbon.ColorScheme;
end;

function TdxRibbonQuickAccessToolbarPainter.GetQuickAccessToolbarViewInfo: TdxRibbonQuickAccessToolbarViewInfo;
begin
  Result := Ribbon.ViewInfo.QuickAccessToolbarViewInfo;
end;

{ TdxRibbonCustomToolbarDockControl }

constructor TdxRibbonCustomToolbarDockControl.Create(AOwner: TdxCustomRibbon);
begin
  inherited Create(nil);
  FRibbon := AOwner;
  Parent := AOwner;
  AllowDocking := False;
  Align := dalNone;
  FPainter := CreatePainter;
end;

destructor TdxRibbonCustomToolbarDockControl.Destroy;
begin
  FreeAndNil(FPainter);
  inherited Destroy;
end;

procedure TdxRibbonCustomToolbarDockControl.CalcLayout;
begin
  RecalculateRibbon;
  inherited CalcLayout;
end;

function TdxRibbonCustomToolbarDockControl.GetPainter: TdxBarPainter;
begin
  Result := FPainter;
end;

procedure TdxRibbonCustomToolbarDockControl.RecalculateRibbon;
begin
  if not Ribbon.IsDestroying then
  begin
    if Ribbon.IsDesigning and Ribbon.HandleAllocated then
      PostMessage(Ribbon.Handle, DXM_RIBBON_RECALCULATE, 0, 0)
    else
      Ribbon.Changed;
  end;
end;

procedure TdxRibbonCustomToolbarDockControl.ShowCustomizePopup;
begin
  // do nothing
end;

procedure TdxRibbonCustomToolbarDockControl.VisibleChanged;
begin
  inherited VisibleChanged;
  if not Ribbon.IsDestroying and Ribbon.IsBarManagerValid then
    RecalculateRibbon;
end;

{ TdxRibbonQuickAccessToolbarDockControl }

function TdxRibbonQuickAccessToolbarDockControl.CreatePainter: TdxRibbonCustomToolbarPainter;
begin
  Result := TdxRibbonQuickAccessToolbarPainter.Create(TdxNativeUInt(Ribbon));
end;

function TdxRibbonQuickAccessToolbarDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxRibbonQuickAccessToolbarBarControl;
end;

{ TdxRibbonQuickAccessToolbarBarControlDesignHelper }

class procedure TdxRibbonQuickAccessToolbarBarControlDesignHelper.GetEditors(AEditors: TList);
begin
  inherited GetEditors(AEditors);
  AEditors.Add(TdxAddGroupButtonEditor);
end;

{ TdxRibbonQuickAccessPopupBarControl }

destructor TdxRibbonQuickAccessPopupBarControl.Destroy;
begin
  FreeAndNil(FPainter);
  IsActive := False;
  inherited Destroy;
end;

procedure TdxRibbonQuickAccessPopupBarControl.CloseUp;
var
  AAccessibilityHelper: TdxBarControlMarkAccessibilityHelper;
  AReason: TdxBarCloseUpReason;
begin
  AAccessibilityHelper := TdxBarControlMarkAccessibilityHelper(BarControl.MarkIAccessibilityHelper.GetHelper);
  AReason := CloseUpReason;
  inherited CloseUp;
  AAccessibilityHelper.CloseUpHandler(AReason);
end;

procedure TdxRibbonQuickAccessPopupBarControl.Popup(const AOwnerRect: TRect);
var
  R: TRect;
begin
  inherited Popup(AOwnerRect);

  if BarControl.AllItemsVisible then
  begin
    SetWindowRgn(Handle, CreateRectRgnIndirect(cxEmptyRect), True);

    R := TdxBarAccessibilityHelperAccess(BarControl.MarkIAccessibilityHelper.GetHelper).GetScreenBounds(cxAccessibleObjectSelfID);
    R.TopLeft := ScreenToClient(R.TopLeft);
    R.BottomRight := ScreenToClient(R.BottomRight);
    GetMarkLink.ItemRect := R;

    GetMarkSubItem.DropDown(not BarNavigationController.NavigationMode);
  end;
end;

function TdxRibbonQuickAccessPopupBarControl.CreatePainter: TdxBarPainter;
begin
  Result := TdxRibbonQuickAccessPopupPainter.Create(TdxNativeUInt(Ribbon));
end;

procedure TdxRibbonQuickAccessPopupBarControl.InitializeForDock(ABar: TdxBar);
begin
  inherited InitializeForDock(ABar);
  FPainter := CreatePainter;
  DoubleBuffered := True;
end;

function TdxRibbonQuickAccessPopupBarControl.GetClientOffset: TRect;
begin
  Result := cxRect(3, 3, 3, 3);
end;

function TdxRibbonQuickAccessPopupBarControl.GetPainter: TdxBarPainter;
begin
  Result := FPainter;
end;

function TdxRibbonQuickAccessPopupBarControl.GetRibbon: TdxCustomRibbon;
begin
  Result := BarControl.Ribbon;
end;

function TdxRibbonQuickAccessPopupBarControl.GetPopupSize: TSize;
begin
  Result := GetSize(MaxInt);
end;

function TdxRibbonQuickAccessPopupBarControl.HasShadow: Boolean;
begin
  Result := not BarControl.AllItemsVisible;
end;

function TdxRibbonQuickAccessPopupBarControl.IsPopup: Boolean;
begin
  Result := True;
end;

function TdxRibbonQuickAccessPopupBarControl.GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
begin
  if ParentBar <> nil then
    Result := TdxRibbonQuickAccessToolbarBarControl(ParentBar)
  else
    Result := TdxRibbonQuickAccessToolbarBarControl(Bar.Control);
end;

function TdxRibbonQuickAccessPopupBarControl.GetMarkLink: TdxBarItemLink;
begin
  Result := ItemLinks[ItemLinks.Count - 1];
end;

function TdxRibbonQuickAccessPopupBarControl.GetMarkSubItem: TCustomdxBarSubItem;
begin
  Result := TCustomdxBarSubItem(GetMarkLink.Item);
end;

{ TdxRibbonQuickAccessPopupPainter }

function TdxRibbonQuickAccessPopupPainter.MarkButtonOffset: Integer;
begin
  Result := 0;
end;

function TdxRibbonQuickAccessPopupPainter.MarkSizeX(ABarControl: TdxBarControl): Integer;
begin
  Result := 0;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetArrowID: Integer;
begin
  Result := DXBAR_ARROWDOWN;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetButtonDropDownPartID: Integer;
begin
  Result := DXBAR_SMALLBUTTON_DROPBUTTON;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetButtonID: Integer;
begin
  Result := DXBAR_SMALLBUTTON;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetButtonMainPartID: Integer;
begin
  Result := DXBAR_SMALLBUTTON_GLYPH;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetMarkArrowID: Integer;
begin
  Result := DXBAR_MARKARROW;
end;

function TdxRibbonQuickAccessPopupPainter.SkinGetTruncateMarkID: Integer;
begin
  Result := DXBAR_MARKTRUNCATED;
end;

procedure TdxRibbonQuickAccessPopupPainter.DrawQuickAccessPopupSubItem(DC: HDC; const ARect: TRect; AState: Integer);
begin
  if AState <> DXBAR_NORMAL then
    Skin.DrawBackground(DC, ARect, DXBAR_SMALLBUTTON, AState);
  if AState = DXBAR_ACTIVE then
    AState := DXBAR_HOT;
  Skin.DrawBackground(DC, ARect, DXBAR_MARKARROWINPOPUP, AState);
end;

procedure TdxRibbonQuickAccessPopupPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
begin
  Ribbon.ColorScheme.DrawQuickAccessToolbarPopup(ACanvas.Handle, ABarControl.ClientRect);
end;

{ TdxRibbonQuickAccessPopupSubItem }

function TdxRibbonQuickAccessPopupSubItem.CreateBarControl: TCustomdxBarControl;
begin
  Result := TdxRibbonQuickAccessPopupSubMenuControl.Create(BarManager);
end;

{ TdxRibbonQuickAccessPopupSubMenuControl }

procedure TdxRibbonQuickAccessPopupSubMenuControl.ShowPopup(AItem: TdxBarItemControl);
begin
// do nothing
end;

{ TdxRibbonQuickAccessPopupSubItemControl }

procedure TdxRibbonQuickAccessPopupSubItemControl.DoCloseUp(AHadSubMenuControl: Boolean);
var
  ABarControl: TdxRibbonCustomToolbarBarControl;
  ACloseUpReason: TdxBarCloseUpReason;
begin
  if AHadSubMenuControl then
  begin
    ACloseUpReason := TdxBarSubMenuControlAccess(Item.ItemLinks.BarControl).CloseUpReason;
    inherited;
    ABarControl := TdxRibbonQuickAccessPopupBarControl(Parent).BarControl;
    if ACloseUpReason = bcrEscape then
    begin
      if ABarControl.AllItemsVisible then
      begin
        ABarControl.MarkState := msNone;
        if BarNavigationController.NavigationMode then
          ABarControl.MarkIAccessibilityHelper.Select(False);
      end;
    end
    else
      if (BarNavigationController.AssignedSelectedObject <> nil) and
        (BarNavigationController.AssignedSelectedObject.GetHelper = ABarControl.MarkIAccessibilityHelper.GetHelper) then
          ABarControl.MarkIAccessibilityHelper.Unselect(nil);
  end
  else
    inherited;
end;

procedure TdxRibbonQuickAccessPopupSubItemControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);

  function GetState: Integer;
  begin
    if DrawParams.DroppedDown then
      Result := DXBAR_PRESSED
    else
      if DrawSelected then
        Result := DXBAR_ACTIVE
      else
        Result := DXBAR_NORMAL;
  end;

begin
  TdxRibbonQuickAccessPopupPainter(Painter).DrawQuickAccessPopupSubItem(Canvas.Handle, ARect, GetState);
end;

function TdxRibbonQuickAccessPopupSubItemControl.GetDefaultWidth: Integer;
begin
  Result := TdxRibbonQuickAccessPopupPainter(Painter).MarkButtonWidth;
end;

{ TdxRibbonQuickAccessPopupSubItemButton }

procedure TdxRibbonQuickAccessPopupSubItemButton.DoClick;
begin
  TdxBarItemLink(Tag).Visible := Down;
end;

{ TdxRibbonTabAreaToolbarBarControl }

function TdxRibbonTabAreaToolbarBarControl.IsOnGlass: Boolean;
begin
  Result := Ribbon.ViewInfo.IsTabsOnGlass;
end;

function TdxRibbonTabAreaToolbarBarControl.AllowQuickCustomizing: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonTabAreaToolbarBarControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
begin
  // do nothing
end;

function TdxRibbonTabAreaToolbarBarControl.CalcSize(AMaxWidth: Integer): TSize;
begin
  Result := inherited;
  if AMaxWidth <= 0 then
    Result.cx := 0;
end;

function TdxRibbonTabAreaToolbarBarControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonTabAreaToolbarBarControlAccessibilityHelper;
end;

function TdxRibbonTabAreaToolbarBarControl.GetItemControlDefaultViewLevel(
  AItemControl: TdxBarItemControl): TdxBarItemViewLevel;
begin
  case AItemControl.ItemLink.PaintStyle of
    psStandard:
      Result := inherited GetItemControlDefaultViewLevel(AItemControl);
    psCaption:
      Result := ivlDefault;
    psCaptionInMenu:
      Result := ivlSmallIcon;
  else //psCaptionGlyph
    Result := ivlSmallIconWithText;
  end;
end;

function TdxRibbonTabAreaToolbarBarControl.GetQuickControlClass: TdxBarPopupControlClass;
begin
  Result := TdxRibbonTabAreaToolbarPopupBarControl;
end;

function TdxRibbonTabAreaToolbarBarControl.MarkExists: Boolean;
begin
  Result := Truncated;
end;

{ TdxRibbonTabAreaToolbarBarControlAccessibilityHelper }

function TdxRibbonTabAreaToolbarBarControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
begin
  Result := TdxRibbonCustomToolbarBarControl(FOwnerObject).Ribbon.GetNextHorizontalAccessibleObject(AItemLinkHelper, ADirection)
end;

{ TdxRibbonTabAreaToolbarDockControl }

function TdxRibbonTabAreaToolbarDockControl.CreatePainter: TdxRibbonCustomToolbarPainter;
begin
  Result := TdxRibbonTabAreaToolbarPainter.Create(TdxNativeUInt(Ribbon));
end;

function TdxRibbonTabAreaToolbarDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxRibbonTabAreaToolbarBarControl;
end;

{ TdxRibbonTabAreaToolbarPainter }

procedure TdxRibbonTabAreaToolbarPainter.BarDrawBackground(ABarControl: TdxBarControl;
  ADC: HDC; const ADestRect, ASourceRect: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  // do nothing
end;

procedure TdxRibbonTabAreaToolbarPainter.EditControlDrawBorder(
  const ADrawParams: TdxBarEditLikeControlDrawParams; var ARect: TRect);
begin
  if Ribbon.Style in [rs2007, rs2010] then
    inherited EditControlDrawBorder(ADrawParams, ARect);
end;

function TdxRibbonTabAreaToolbarPainter.EditControlGetBackgroundColor(
  const ADrawParams: TdxBarItemControlDrawParams): TColor;
begin
  Result := Ribbon.ColorScheme.GetPartColor(rtatpEditBackground, GetPartState(ADrawParams, icpControl));
end;

function TdxRibbonTabAreaToolbarPainter.EditControlGetTextColor(const ADrawParams: TdxBarItemControlDrawParams): TColor;
begin
  Result := Ribbon.ColorScheme.GetPartColor(rtatpEditText, GetPartState(ADrawParams, icpControl));
end;

function TdxRibbonTabAreaToolbarPainter.GetGlyphColorPalette(
  ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette;
begin
  if APaintType = ptMenu then
    Result := Ribbon.ColorScheme.GetMenuColorPalette(AState)
  else
    Result := Ribbon.ColorScheme.GetTabAreaButtonColorPalette(AState);
end;

procedure TdxRibbonTabAreaToolbarPainter.DoDrawText(ADC: HDC; const AText: string; var ARect: TRect; AFormat: Cardinal);
begin
  if Ribbon.ViewInfo.IsTabsOnGlass and (AFormat and DT_CALCRECT = 0) then
    dxDrawTextOnGlass(ADC, AText, nil, ARect, clDefault, AFormat, 0, True)
  else
    inherited DoDrawText(ADC, AText, ARect, AFormat);
end;

procedure TdxRibbonTabAreaToolbarPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
begin
  inherited DrawToolbarContentPart(ABarControl, ACanvas);

  ACanvas.SaveDC;
  try
    ACanvas.WindowOrg := dxMapWindowPoint(ABarControl.Handle, Ribbon.Handle, ACanvas.WindowOrg);
    Ribbon.ViewInfo.DrawRibbonBackground(ACanvas);
    if Ribbon.FormCaptionHelper <> nil then
      Ribbon.FormCaptionHelper.UpdateCaptionArea(ACanvas);
  finally
    ACanvas.RestoreDC;
  end;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetArrowID: Integer;
begin
  Result := DXRIBBON_TAT_ARROWDOWN;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetButtonDropDownPartID: Integer;
begin
  Result := DXRIBBON_TAT_SMALLBUTTON_DROPBUTTON;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetButtonMainPartID: Integer;
begin
  Result := DXRIBBON_TAT_SMALLBUTTON_GLYPH;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetMarkArrowID: Integer;
begin
  Result := DXRIBBON_TAT_MARKARROW;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetTruncateMarkID: Integer;
begin
  Result := DXRIBBON_TAT_MARKTRUNCATED;
end;

function TdxRibbonTabAreaToolbarPainter.SkinGetButtonID: Integer;
begin
  Result := DXRIBBON_TAT_SMALLBUTTON;
end;

function TdxRibbonTabAreaToolbarPainter.GetDefaultEnabledTextColor(
  ABarItemControl: TdxBarItemControl; ASelected: Boolean; AFlat: Boolean): TColor;
var
  ADrawParams: TdxBarItemControlDrawParams;
begin
  ADrawParams := TdxBarItemControlAccess(ABarItemControl).FDrawParams;
  if ADrawParams.PaintType <> ptMenu then
    Result := Ribbon.ColorScheme.GetPartColor(DXRIBBON_TAT_SMALLBUTTON, GetPartState(ADrawParams, icpControl))
  else
    Result := inherited GetDefaultEnabledTextColor(ABarItemControl, ASelected, AFlat);
end;

{ TdxRibbonTabAreaToolbarPopupBarControl }

procedure TdxRibbonTabAreaToolbarPopupBarControl.InitializeForDock(ABar: TdxBar);
begin
  inherited InitializeForDock(ABar);
  FPainter := TdxRibbonQuickAccessPopupPainter.Create(TdxNativeUInt(Ribbon));
  DoubleBuffered := True;
end;

function TdxRibbonTabAreaToolbarPopupBarControl.GetClientOffset: TRect;
begin
  Result := cxRect(3, 3, 3, 3);
end;

function TdxRibbonTabAreaToolbarPopupBarControl.GetPainter: TdxBarPainter;
begin
  Result := FPainter;
end;

function TdxRibbonTabAreaToolbarPopupBarControl.GetPopupSize: TSize;
begin
  Result := GetSize(MaxInt);
end;

function TdxRibbonTabAreaToolbarPopupBarControl.GetRibbon: TdxCustomRibbon;
begin
  Result := BarControl.Ribbon;
end;

function TdxRibbonTabAreaToolbarPopupBarControl.HasShadow: Boolean;
begin
  Result := not BarControl.AllItemsVisible;
end;

function TdxRibbonTabAreaToolbarPopupBarControl.IsPopup: Boolean;
begin
  Result := True;
end;

function TdxRibbonTabAreaToolbarPopupBarControl.GetBarControl: TdxRibbonTabAreaToolbarBarControl;
begin
  if ParentBar <> nil then
    Result := TdxRibbonTabAreaToolbarBarControl(ParentBar)
  else
    Result := TdxRibbonTabAreaToolbarBarControl(Bar.Control);
end;

{ TdxRibbonGroupBarControl }

destructor TdxRibbonGroupBarControl.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

procedure TdxRibbonGroupBarControl.CloseUp;
var
  AAccessibilityHelper: TdxRibbonGroupBarControlAccessibilityHelper;
  AReason: TdxBarCloseUpReason;
begin
  if dxBarGetParentPopupWindow(Self, False) <> nil then
  begin
    TdxRibbonTabAccessibilityHelper(Group.Tab.IAccessibilityHelper.GetHelper).CloseUpHandler(CloseUpReason);
    TdxRibbonTabGroupsPopupWindow(DockControl.Parent.Parent).CloseUp;
  end
  else
  begin
    AAccessibilityHelper := TdxRibbonGroupBarControlAccessibilityHelper(ParentBar.IAccessibilityHelper.GetHelper);
    AReason := CloseUpReason;
    inherited CloseUp;
    AAccessibilityHelper.CloseUpHandler(AReason);
  end;
end;

procedure TdxRibbonGroupBarControl.InitializeForDock(ABar: TdxBar);

  function GetGroup: TdxRibbonTabGroup;
  var
    ATab: TdxRibbonTab;
    I: Integer;
  begin
    ATab := TdxRibbonGroupsDockControl(Bar.DockControl).Tab;
    for I := 0 to ATab.Groups.Count - 1 do
    begin
      if ATab.Groups[I].ToolBar = ABar then
        Exit(ATab.Groups[I]);
    end;
    Result := nil;
  end;

begin
  inherited;
  FGroup := GetGroup;
  FRibbon := FGroup.Tab.Ribbon;
end;

function TdxRibbonGroupBarControl.FadingCanFade: Boolean;
begin
  Result := HandleAllocated and not (csDestroying in ComponentState) and Ribbon.CanFade;
end;

procedure TdxRibbonGroupBarControl.FadingDrawFadeImage;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_FRAME);
end;

procedure TdxRibbonGroupBarControl.FadingGetFadingImages(
  out AFadeOutImage, AFadeInImage: TcxBitmap);

  function GetGroupViewOrg: TPoint;
  var
    R: TRect;
  begin
    R := WindowRect;
    Dec(R.Left, ClientOrigin.X);
    Dec(R.Top, ClientOrigin.Y);
    Result := R.TopLeft;
  end;

  procedure Draw(ACanvas: TcxCanvas; AState: TdxBarViewState);
  const
    CollapsedStateMap: array[TdxBarViewState] of Integer =
      (DXBAR_NORMAL, DXBAR_HOT);
  var
    APrevViewState: TdxBarViewState;
  begin
    APrevViewState := FViewState;
    try
      FViewState := AState;
      ACanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
      PaintGroupBackground(ACanvas);
      ACanvas.WindowOrg := GetGroupViewOrg;
      ACanvas.SaveClipRegion;
      try
        ACanvas.SetClipRegion(TcxRegion.Create(ClientRect), roSet);
        if Collapsed then
          Ribbon.GroupsPainter.DrawCollapsedToolbarBackgroundPart(Self, ACanvas, CollapsedStateMap[AState])
        else
          Ribbon.GroupsPainter.DrawToolbarContentPart(Self, ACanvas);
      finally
        ACanvas.RestoreClipRegion;
        ACanvas.WindowOrg := cxNullPoint;
      end;
    finally
      FViewState := APrevViewState;
    end;
  end;

begin
  AFadeInImage := TcxBitmap32.CreateSize(BoundsRect, True);
  AFadeOutImage := TcxBitmap32.CreateSize(BoundsRect, True);
  Draw(AFadeInImage.cxCanvas, bvsHot);
  Draw(AFadeOutImage.cxCanvas, bvsNormal);
end;

procedure TdxRibbonGroupBarControl.AdjustHintWindowPosition(
  var APos: TPoint; const ABoundsRect: TRect; AHeight: Integer);
const
  HintIndent = 2;
begin
  if UseRightToLeftAlignment then
    APos.X := ABoundsRect.Right
  else
    APos.X := ABoundsRect.Left;
  APos.Y := Ribbon.ClientToScreen(cxPoint(0, Ribbon.Height)).Y;
  APos.Y := Max(APos.Y, ClientToScreen(cxPoint(0, Height + HintIndent)).Y);
  if GetDesktopWorkArea(APos).Bottom - APos.Y < AHeight then
  begin
    APos.Y := Ribbon.ClientToScreen(cxNullPoint).Y - AHeight - HintIndent;
    APos.Y := Min(APos.Y, ClientToScreen(cxNullPoint).Y - AHeight - 2 * HintIndent);
  end;
end;

procedure TdxRibbonGroupBarControl.CalcLayout;
begin
  if Ribbon.CanFade then
    dxFader.Clear;
end;

function TdxRibbonGroupBarControl.CanProcessShortCut: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonGroupBarControl.CaptionChanged;
begin
  inherited CaptionChanged;
  RebuildBar;
end;

procedure TdxRibbonGroupBarControl.DoHideAll(AReason: TdxBarCloseUpReason);
var
  ALinkSelf: TcxObjectLink;
begin
  ALinkSelf := cxAddObjectLink(Self);
  try
    inherited;
    if (ALinkSelf.Ref <> nil) and (dxBarGetParentPopupWindow(Self, True) <> nil) then
    begin
      if AReason <> bcrUnknown then
        CloseUp;
    end;
  finally
    cxRemoveObjectLink(ALinkSelf);
  end;
end;

procedure TdxRibbonGroupBarControl.DoNCPaint(DC: HDC);
begin
  DoTransparentNCPaint(DC)
end;

procedure TdxRibbonGroupBarControl.DoOpaqueNCPaint(DC: HDC);
begin
  if not cxRectIsEqual(NCRect, ClientRect) then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
      if UseRightToLeftReading then
        cxPaintCanvas.TextFlags := cxPaintCanvas.TextFlags or ETO_RTLREADING;
      PaintDesignObjects(cxPaintCanvas);
      if not Collapsed then
      begin
        PaintGroupBackground(cxPaintCanvas);
        PaintGroupCaptionText(cxPaintCanvas);
        DrawCaptionButtons(cxPaintCanvas);
      end;
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxRibbonGroupBarControl.DoTransparentNCPaint(DC: HDC);
var
  P: TPoint;
  R, BR: TRect;
  AIndex: Integer;
  B: TcxBitmap32;
begin
  B := TcxBitmap32.CreateSize(WindowRect, True);
  try
    B.cxCanvas.UseRightToLeftAlignment := UseRightToLeftAlignment;
    if UseRightToLeftReading then
      B.cxCanvas.TextFlags := B.cxCanvas.TextFlags or ETO_RTLREADING;
    AIndex := SaveDC(DC);
    R := ClientRect;
    BR := Painter.GetToolbarContentOffsets(Bar, dsNone, ScaleFactor, False);
    if UseRightToLeftAlignment then
      BR := TdxRightToLeftLayoutConverter.ConvertOffsets(BR);
    OffsetRect(R, BR.Left, BR.Top);
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    if IsPopup then
    begin
      R := NCRect;
      DrawGroupsArea(B.cxCanvas, R);
    end
    else
    begin
      P := dxMapWindowPoint(Handle, Ribbon.FGroupsDockControlSite.Handle, cxNullPoint);
      R := cxGetWindowRect(Ribbon.FGroupsDockControlSite);
      OffsetRect(R, -R.Left, -R.Top);
      Dec(P.X, BR.Left);
      Dec(P.Y, BR.Top);
      SetWindowOrgEx(B.Canvas.Handle, P.X, P.Y, nil);
      DrawGroupsArea(B.cxCanvas, R);
      SetWindowOrgEx(B.Canvas.Handle, 0, 0, nil);
      R := NCRect;
    end;
    DoOpaqueNCPaint(B.Canvas.Handle);
    cxBitBlt(DC, B.Canvas.Handle, R, cxNullPoint, SRCCOPY);
    RestoreDC(DC, AIndex);
  finally
    B.Free;
  end;
end;

procedure TdxRibbonGroupBarControl.DrawBarParentBackground(ACanvas: TcxCanvas);
var
  APoint: TPoint;
  ARect, AOffsets: TRect;
  AHandle: HWND;
begin
  ACanvas.SaveState;
  if IsPopup then
  begin
    ARect := WindowRect;
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    AOffsets := Painter.GetToolbarContentOffsets(Bar, dsNone, ScaleFactor, False);
    if UseRightToLeftAlignment then
      AOffsets := TdxRightToLeftLayoutConverter.ConvertOffsets(AOffsets);
    OffsetRect(ARect, -AOffsets.Left, -AOffsets.Top);
  end
  else
  begin
    AHandle := Ribbon.FGroupsDockControlSite.Handle;
    APoint := dxMapWindowPoint(Handle, AHandle, cxNullPoint);
    Windows.GetClientRect(AHandle, ARect);
    OffsetRect(ARect, -APoint.X, -APoint.Y);
  end;
  DrawGroupsArea(ACanvas, ARect);
  ACanvas.RestoreState;
end;

procedure TdxRibbonGroupBarControl.DrawContentBackground;

  function DrawFadeContentBackground: Boolean;
  var
    R: TRect;
  begin
    R := WindowRect;
    OffsetRect(R, -R.Left, -R.Top);
    Dec(R.Top, ClientBounds.Top);
    Dec(R.Left, ClientBounds.Left);
    Result := dxFader.DrawFadeImage(Self, Canvas.Handle, R);
  end;

begin
  if not DrawFadeContentBackground then
    inherited DrawContentBackground;
end;

procedure TdxRibbonGroupBarControl.DrawGroupsArea(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  Ribbon.ViewInfo.Painter.DrawGroupsArea(ACanvas, ABounds, IsAllowContextPaint, IsPopup);
end;

function TdxRibbonGroupBarControl.IsAllowContextPaint: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonGroupBarControl.DoPaint;
var
  APrevWindowOrg: TPoint;
begin
  DrawBarParentBackground(Canvas);
  if Collapsed then
    TdxRibbonBarPainter(Painter).DrawToolbarContentPart(Self, Canvas)
  else
    inherited DoPaint;

  if IsDesignObjectsOnClientArea then
  begin
    APrevWindowOrg := Canvas.WindowOrg;
    try
      Canvas.WindowOrg := NCOffset;
      PaintDesignObjects(Canvas);
    finally
      Canvas.WindowOrg := APrevWindowOrg;
    end;
  end;
end;

procedure TdxRibbonGroupBarControl.DoBarMouseDown(Button: TMouseButton; Shift: TShiftState;
  const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean);
begin
  if cxRectPtIn(GroupDesignRect, GetWindowPoint(APoint)) then
  begin
    Group.DesignSelectionHelper.SelectComponent;
    if Button = mbRight then
      ShowGroupDesignMenu;
  end
  else
    inherited DoBarMouseDown(Button, Shift, APoint, AItemControl, APointInClientRect);
end;

function TdxRibbonGroupBarControl.ClickAtHeader: Boolean;
begin
  Result := Collapsed and cxRectPtIn(WindowRect, GetMouseCursorPos) or inherited ClickAtHeader;
end;

function TdxRibbonGroupBarControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonGroupBarControlAccessibilityHelper;
end;

function TdxRibbonGroupBarControl.GetCaption: TCaption;
begin
  Result := Group.Caption;
end;

function TdxRibbonGroupBarControl.GetMarkDrawState: TdxBarMarkState;
begin
  if IAccessibilityHelper.IsSelected then
    Result := msSelected
  else
    Result := MarkState;
end;

function TdxRibbonGroupBarControl.GetMoreButtonsHint: string;
begin
  Result := Group.Caption;
end;

function TdxRibbonGroupBarControl.GetQuickControlClass: TdxBarPopupControlClass;
begin
  Result := TdxRibbonCollapsedGroupPopupBarControl;
end;

function TdxRibbonGroupBarControl.GetRibbon: TdxCustomRibbon;
begin
  Result := FRibbon;
end;

function TdxRibbonGroupBarControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonGroupBarControlViewInfo;
end;

procedure TdxRibbonGroupBarControl.GlyphChanged;
begin
  Ribbon.QuickAccessToolbar.UpdateGroupButton(Bar, False);
end;

function TdxRibbonGroupBarControl.HasCaptionButtons: Boolean;
begin
  Result := not Collapsed and inherited HasCaptionButtons;
end;

procedure TdxRibbonGroupBarControl.InitQuickControl(AQuickControl: TdxBarPopupControl);
begin
// do nothing
end;

procedure TdxRibbonGroupBarControl.MakeItemControlFullyVisible(
  AItemControl: TdxBarItemControl);
var
  R: TRect;
begin
  if DockControl = nil then
    Exit;
  R := AItemControl.ViewInfo.Bounds;
  R.TopLeft := DockControl.ScreenToClient(ClientToScreen(R.TopLeft));
  R.BottomRight := DockControl.ScreenToClient(ClientToScreen(R.BottomRight));
  TdxRibbonGroupsDockControl(DockControl).MakeRectFullyVisible(R);
end;

function TdxRibbonGroupBarControl.MarkExists: Boolean;
begin
  Result := Collapsed;
end;

procedure TdxRibbonGroupBarControl.ViewStateChanged(APrevValue: TdxBarViewState);
begin
  if Ribbon.CanFade then
  begin
    if ViewState = bvsHot then
      dxFader.FadeIn(Self)
    else
      if MarkState <> msPressed then
        dxFader.FadeOut(Self);
  end;
  FullInvalidate;
end;

procedure TdxRibbonGroupBarControl.UpdateCaptionButton(ACaptionButton: TdxBarCaptionButton);
var
  I: Integer;
  AButtonRect, AInvalidateRect, ACaptionRect: TRect;
  AButtonWidth: Integer;
begin
  if ACaptionButton = nil then
  begin
    ACaptionRect := GroupCaptionRect;
    AButtonRect := ACaptionRect;
    AButtonWidth := cxRectHeight(AButtonRect) + 1;
    AButtonRect.Left := AButtonRect.Right - AButtonWidth;
    for I := 0 to Bar.CaptionButtons.Count - 1 do
    begin
      if Ribbon.UseRightToLeftAlignment then
        Bar.CaptionButtons[I].Rect := TdxRightToLeftLayoutConverter.ConvertRect(AButtonRect, ACaptionRect)
      else
        Bar.CaptionButtons[I].Rect := AButtonRect;
      OffsetRect(AButtonRect, -AButtonWidth, 0);
    end;
    AInvalidateRect := CaptionButtons.Rect;
  end
  else
    AInvalidateRect := ACaptionButton.Rect;
  if HandleAllocated and IsWindowVisible(Handle) then
    InvalidateNCRect(AInvalidateRect);
end;

procedure TdxRibbonGroupBarControl.WindowPosChanged(var Message: TWMWindowPosMsg);
begin
  inherited WindowPosChanged(Message);
  if Message.WindowPos^.flags and SWP_NOSIZE = 0 then
  begin
    if HasCaptionButtons then
      UpdateCaptionButton(nil);
  end;
end;

procedure TdxRibbonGroupBarControl.DesignMenuClick(Sender: TObject);
begin
  case TdxBarButton(Sender).Tag of
    0: Ribbon.DesignAddTabGroup(Group.Tab, False);
    1: Ribbon.DesignAddTabGroup(Group.Tab, True);
    2: BarDesignController.DeleteSelectedObjects(True, True)
  end;
end;

procedure TdxRibbonGroupBarControl.DrawCaptionButton(ACanvas: TcxCanvas; AButton: TdxBarCaptionButton);
var
  ADefaultGlyphSize: Integer;
  AGlyphRatio: Integer;
  AGlyphRect: TRect;
  AGlyphRectSize: Integer;
begin
  Ribbon.SkinDrawBackground(ACanvas.Handle, AButton.Rect, DXBAR_LAUNCHBUTTONBACKGROUND, AButton.State);

  ADefaultGlyphSize := Ribbon.SkinGetPartSize(DXBAR_LAUNCHBUTTONDEFAULTGLYPH);
  AGlyphRectSize := Min(cxRectWidth(AButton.Rect), cxRectHeight(AButton.Rect)) - 2 {BorderSize} * 2;
  AGlyphRatio := Round(Max(1, AGlyphRectSize / ADefaultGlyphSize));
  AGlyphRectSize := ADefaultGlyphSize * AGlyphRatio;
  if AGlyphRatio > 1 then
    Dec(AGlyphRectSize, AGlyphRatio); // GDI+ feature
  AGlyphRect := cxRectCenter(AButton.Rect, AGlyphRectSize, AGlyphRectSize);
  if IsGlyphAssigned(AButton.Glyph) then
    TransparentDraw(ACanvas.Handle, AGlyphRect, AButton.Glyph, AButton.Enabled)
  else
  begin
    OffsetRect(AGlyphRect, 1, 1); // because shadow
    if UseRightToLeftAlignment then
      AGlyphRect := TdxRightToLeftLayoutConverter.ConvertRect(AGlyphRect, AButton.Rect);
    Ribbon.SkinDrawBackground(ACanvas.Handle, AGlyphRect, DXBAR_LAUNCHBUTTONDEFAULTGLYPH, AButton.State);
  end;
end;

procedure TdxRibbonGroupBarControl.DrawCaptionButtons(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if (CaptionButtons.Count > 0) and not Ribbon.IsSimplifiedGroupsLayout then
  begin
    ACanvas.IntersectClipRect(CaptionButtons.Rect);
    for I := 0 to CaptionButtons.Count - 1 do
      DrawCaptionButton(ACanvas, CaptionButtons[I]);
  end;
end;

procedure TdxRibbonGroupBarControl.DrawSelectedFrame(DC: HDC; const R: TRect);

  procedure DrawLine(X1, Y1, X2, Y2: Integer);
  begin
    DrawRect(DC, cxRect(X1, Y1, X2, Y2), clBlack, True);
  end;

begin
  DrawLine(R.Left, R.Top, R.Right, R.Top + dxRibbonGroupSelectionFrameSize);
  DrawLine(R.Left, R.Bottom - dxRibbonGroupSelectionFrameSize, R.Right, R.Bottom);
  DrawLine(R.Left, R.Top, R.Left + dxRibbonGroupSelectionFrameSize, R.Bottom);
  DrawLine(R.Right - dxRibbonGroupSelectionFrameSize, R.Top, R.Right, R.Bottom);
end;

function TdxRibbonGroupBarControl.GetCollapsed: Boolean;
begin
  Result := ViewInfo.Collapsed;
end;

function TdxRibbonGroupBarControl.GetFadingOptions: TdxFadingOptions;
begin
  Result := Ribbon.OptionsFading.TabGroups;
end;

function TdxRibbonGroupBarControl.GetGroupCaptionRect: TRect;
begin
  Result := Ribbon.SkinGetCaptionRect(NCRect, Ribbon.GroupsPainter.GetToolbarSkinPart(Self));
end;

function TdxRibbonGroupBarControl.GetGroupDesignRect: TRect;
const
  MarkSize = 14;
begin
  if csDesigning in ComponentState then
  begin
    Result := WindowRect;
    OffsetRect(Result, -(Result.Left - ScaleFactor.Apply(3)), -(Result.Top + ScaleFactor.Apply(3)));
    Result.Top := Result.Bottom - ScaleFactor.Apply(MarkSize);
    Result.Right := Result.Left + ScaleFactor.Apply(MarkSize);
  end
  else
    Result := cxEmptyRect;
end;

function TdxRibbonGroupBarControl.GetIsDesignObjectsOnClientArea: Boolean;
var
  AClientBounds: TRect;
begin
  Result := csDesigning in ComponentState;
  if Result then
  begin
    AClientBounds := ClientBounds;
    Result := cxRectIntersect(AClientBounds, GroupDesignRect) or
      (AClientBounds.Left < dxRibbonGroupSelectionFrameSize) or
      (AClientBounds.Top < dxRibbonGroupSelectionFrameSize) or
      (NCRect.Right - AClientBounds.Right < dxRibbonGroupSelectionFrameSize) or
      (NCRect.Bottom - AClientBounds.Bottom < dxRibbonGroupSelectionFrameSize);
  end;
end;

function TdxRibbonGroupBarControl.GetIsComponentSelected: Boolean;
begin
  Result := Group.DesignSelectionHelper.IsComponentSelected;
end;

function TdxRibbonGroupBarControl.GetViewInfo: TdxRibbonGroupBarControlViewInfo;
begin
  Result := TdxRibbonGroupBarControlViewInfo(FViewInfo);
end;

procedure TdxRibbonGroupBarControl.PaintGroupBackground(ACanvas: TcxCanvas);
begin
  ACanvas.ExcludeClipRect(ClientBounds);
  if not dxFader.DrawFadeImage(Self, ACanvas.Handle, NCRect) then
    Ribbon.GroupsPainter.DrawToolbarNonContentPart(Self, ACanvas.Handle);
end;

procedure TdxRibbonGroupBarControl.PaintGroupCaptionText(ACanvas: TcxCanvas);
begin
  if not Ribbon.IsSimplifiedGroupsLayout then
    Ribbon.GroupsPainter.DrawToolbarNonContentPartCaption(Self, ACanvas.Handle);
end;

procedure TdxRibbonGroupBarControl.PaintDesignObjects(ACanvas: TcxCanvas);
begin
  cxDrawDesignRect(ACanvas, GetGroupDesignRect, IsComponentSelected);
  if IsComponentSelected then
    DrawSelectedFrame(ACanvas.Handle, NCRect);
end;

procedure TdxRibbonGroupBarControl.InitDesignMenu(AItemLinks: TdxBarItemLinks);
begin
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDEMPTYGROUP), DesignMenuClick, 0);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDGROUPWITHTOOLBAR), DesignMenuClick, 1);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONDELETEGROUP), DesignMenuClick, 2, True);
end;

procedure TdxRibbonGroupBarControl.ShowGroupDesignMenu;
begin
  BarDesignController.ShowCustomCustomizePopup(BarManager, InitDesignMenu, Painter);
end;

procedure TdxRibbonGroupBarControl.WMGestureNotify(var Message: TWMGestureNotify);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.Unused, LPARAM(Message.NotifyStruct));
end;

procedure TdxRibbonGroupBarControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if Collapsed then
  begin
    Message.Result := HTCLIENT;
    HitTest := HTCLIENT;
  end
  else
    inherited;
end;

procedure TdxRibbonGroupBarControl.WMPaint(var Message: TWMPaint);
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
    dxBufferedPaintControl(Self);
end;

{ TdxRibbonGroupBarControlViewInfo }

procedure TdxRibbonGroupBarControlViewInfo.Calculate;
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  I: Integer;
begin
  FNonContentAreaSize := GetNonContentAreaSize;
  if Collapsed then
  begin
    ContentSize := cxSize(Ribbon.GroupsPainter.GetCollapsedGroupWidth(BarControl), Ribbon.GetGroupHeight);
    RemoveSeparatorInfos;
    for I := 0 to ItemControlCount - 1 do
      IdxBarItemControlViewInfo(ItemControlViewInfos[I]).SetBounds(cxEmptyRect);
  end
  else
  begin
    if Ribbon.IsSimplifiedGroupsLayout then
      for I := 0 to ItemControlCount - 1 do
      begin
        AItemControlViewInfo := IdxBarItemControlViewInfo(ItemControlViewInfos[I]);
        AItemControlViewInfo.SetViewLevel(AItemControlViewInfo.GetViewLevelForButtonGroup);
      end;
    LayoutCalculator.CalcLayout(CreateCalculateHelper);
  end;
end;

procedure TdxRibbonGroupBarControlViewInfo.AfterCalculate;
begin
  if Ribbon.UseRightToLeftAlignment then
    DoRightToLeftConversion(cxRect(ContentSize));
  FKeyTipsBaseLinePositions.Calculated := False;
  FLayoutCalculator := nil;
  if FPrevCollapsedState <> FCollapsed then
  begin
    if BarControl.HandleAllocated then
      BarControl.FrameChanged;
  end;
  UpdateItemRects;
end;

procedure TdxRibbonGroupBarControlViewInfo.BeforeCalculate(ACollapsed: Boolean);
begin
  FPrevCollapsedState := Collapsed;
  Collapsed := ACollapsed;
  RecreateItemControlViewInfos;
  FLayoutCalculator := CreateLayoutCalculator;
  FLayoutCalculator.CalcInit(CreateCalculateHelper);
end;

function TdxRibbonGroupBarControlViewInfo.Reduce(AStage: TdxRibbonGroupsReduceStage; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
begin
  Result := False;
  if BarControl.Group.Restriction <> rtgrNoExpand then
    case AStage of
      rgrsMultiColumnItemControlsColumnCount:
        Result := LayoutCalculator.DecreaseMultiColumnItemControlsColumnCount(CreateCalculateHelper);
      rgrsMultiColumnItemControlsCollapsing:
        Result := LayoutCalculator.CollapseMultiColumnItemControls(CreateCalculateHelper);
      rgrsItemControlsViewLevel:
        Result := LayoutCalculator.Reduce(CreateCalculateHelper, AUpToViewLevel);
    end;

  if (AStage = rgrsGroupsCollapsing) and (BarControl.Group.Restriction <> rtgrNoCollapse) then
  begin
    Collapsed := True;
    Calculate;
  end;
end;

procedure TdxRibbonGroupBarControlViewInfo.ReduceInit;
begin
  LayoutCalculator.ReduceInit(CreateCalculateHelper);
end;

procedure TdxRibbonGroupBarControlViewInfo.CalculateKeyTipsBaseLinePositions;
begin
  if not BarControl.HandleAllocated or not IsWindowVisible(BarControl.Handle) then
    raise EdxException.Create('TdxRibbonGroupBarControlViewInfo.CalculateKeyTipsBaseLinePositions fails');
  if not FKeyTipsBaseLinePositions.Calculated then
  begin
    DoCalculateKeyTipsBaseLinePositions;
    FKeyTipsBaseLinePositions.Calculated := True;
  end;
end;

procedure TdxRibbonGroupBarControlViewInfo.CheckGroupCollapsedStates;
begin
  if Collapsed <> FPrevCollapsedState then
  begin
    if Collapsed then
      BarControl.Group.Tab.Ribbon.DoTabGroupCollapsed(BarControl.Group.Tab, BarControl.Group)
    else
      BarControl.Group.Tab.Ribbon.DoTabGroupExpanded(BarControl.Group.Tab, BarControl.Group);
  end;
end;

function TdxRibbonGroupBarControlViewInfo.CreateLayoutCalculator: IdxRibbonGroupLayoutCalculator;
begin
  FGroupRowHeight := Ribbon.GetGroupRowHeight;
  Result := TdxRibbonGroupLayoutCalculator.Create(FGroupRowHeight, Ribbon.GroupsPainter.GetGroupRowCount, Ribbon.ScaleFactor);
end;

procedure TdxRibbonGroupBarControlViewInfo.DoCalculateKeyTipsBaseLinePositions;
begin
  SetLength(FKeyTipsBaseLinePositions.RowKeyTipsBaseLinePositions, Ribbon.GroupsPainter.GetGroupRowCount);
  FKeyTipsBaseLinePositions.RowKeyTipsBaseLinePositions[0] := BarControl.ClientOrigin.Y - BarControl.WindowRect.Top;
  if not Ribbon.IsSimplifiedGroupsLayout then
  begin
    FKeyTipsBaseLinePositions.RowKeyTipsBaseLinePositions[2] := BarControl.GroupCaptionRect.Top;
    with FKeyTipsBaseLinePositions do
      RowKeyTipsBaseLinePositions[1] := (RowKeyTipsBaseLinePositions[0] + RowKeyTipsBaseLinePositions[2]) div 2;
  end;
  FKeyTipsBaseLinePositions.BottomKeyTipsBaseLinePosition :=
    BarControl.Height - Ribbon.SkinGetContentOffsets(DXBAR_COLLAPSEDTOOLBAR).Bottom;
end;

function TdxRibbonGroupBarControlViewInfo.GetNonContentAreaSize: TSize;
var
  R: TRect;
begin
  R := BarControl.Painter.GetToolbarContentOffsets(BarControl.Bar, dsNone, BarControl.ScaleFactor, False);
  Result := cxSize(cxMarginsWidth(R), cxMarginsHeight(R));
end;

procedure TdxRibbonGroupBarControlViewInfo.RecreateItemControlViewInfos;

  function IsAllControlsCreated(AItemLinks: TdxBarItemLinks): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to AItemLinks.CanVisibleItemCount - 1 do
      Result := Result and (AItemLinks.CanVisibleItems[I].Control <> nil);
  end;

var
  AItemControl: TdxBarItemControl;
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AItemLinks: TdxBarItemLinks;
  I: Integer;
begin
  Clear;
  AItemLinks := BarControl.ItemLinks;
  if IsAllControlsCreated(AItemLinks) then
    for I := 0 to AItemLinks.CanVisibleItemCount - 1 do
    begin
      AItemControl := BarControl.ItemLinks.CanVisibleItems[I].Control;
      TdxBarItemControlAccess(AItemControl).LastInRow := I = AItemLinks.CanVisibleItemCount - 1;
      AItemControlViewInfo := IdxBarItemControlViewInfo(AItemControl.ViewInfo);
      if not Ribbon.IsSimplifiedGroupsLayout or (AItemControlViewInfo.GetViewLevelForButtonGroup in AItemControlViewInfo.GetAllowedViewLevels) then
        AddItemControlViewInfo(AItemControl.ViewInfo);
    end;
end;

procedure TdxRibbonGroupBarControlViewInfo.UpdateItemRects;

  function GetItemControlBounds(AIndex: Integer): TRect;
  begin
    if Collapsed then
      Result := cxEmptyRect
    else
      Result := ItemControlViewInfos[AIndex].Bounds;
  end;

var
  AItemLink: TdxBarItemLink;
  ANeedsInvalidateBarControl: Boolean;
  I: Integer;
begin
  TdxBarItemLinksAccess(BarControl.ItemLinks).BeginCalcItemRects;
  try
    ANeedsInvalidateBarControl := False;
    for I := 0 to ItemControlCount - 1 do
    begin
      AItemLink := ItemControlViewInfos[I].Control.ItemLink;
      ANeedsInvalidateBarControl := ANeedsInvalidateBarControl or not EqualRect(AItemLink.ItemRect, GetItemControlBounds(I));
      AItemLink.ItemRect := GetItemControlBounds(I);
    end;
  finally
    TdxBarItemLinksAccess(BarControl.ItemLinks).EndCalcItemRects;
  end;
  if ANeedsInvalidateBarControl and BarControl.HandleAllocated then
    InvalidateRect(BarControl.Handle, nil, False);
end;

function TdxRibbonGroupBarControlViewInfo.CreateCalculateHelper: IdxRibbonGroupViewInfo;
begin
  Result := TdxRibbonGroupBarControlViewInfoHelper.Create(Self);
end;

procedure TdxRibbonGroupBarControlViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
  AItemBounds: TRect;
  ASeparatorInfo: TdxBarItemSeparatorInfo;
begin
  for I := 0 to ItemControlCount - 1 do
  begin
    AItemBounds := IdxBarItemControlViewInfo(ItemControlViewInfos[I]).GetBounds;
    IdxBarItemControlViewInfo(ItemControlViewInfos[I]).SetBounds(TdxRightToLeftLayoutConverter.ConvertRect(AItemBounds, ABounds));
  end;
  for I := 0 to SeparatorCount - 1 do
  begin
    ASeparatorInfo := SeparatorInfos[I];
    ASeparatorInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(SeparatorInfos[I].Bounds, ABounds);
    SeparatorInfos[I] := ASeparatorInfo;
  end;
end;

function TdxRibbonGroupBarControlViewInfo.GetBarControl: TdxRibbonGroupBarControl;
begin
  Result := FBarControl as TdxRibbonGroupBarControl;
end;

function TdxRibbonGroupBarControlViewInfo.GetBottomKeyTipsBaseLinePosition: Integer;
begin
  CalculateKeyTipsBaseLinePositions;
  Result := FKeyTipsBaseLinePositions.BottomKeyTipsBaseLinePosition +
    BarControl.WindowRect.Top;
end;

function TdxRibbonGroupBarControlViewInfo.GetRibbon: TdxCustomRibbon;
begin
  Result := BarControl.Ribbon;
end;

function TdxRibbonGroupBarControlViewInfo.GetRowKeyTipsBaseLinePosition(ARowIndex: Integer): Integer;
begin
  CalculateKeyTipsBaseLinePositions;
  if (ARowIndex < 0) or (ARowIndex > High(FKeyTipsBaseLinePositions.RowKeyTipsBaseLinePositions)) then
    raise EdxException.Create('TdxRibbonGroupBarControlViewInfo.GetRowKeyTipsBaseLinePosition fails');
  Result := FKeyTipsBaseLinePositions.RowKeyTipsBaseLinePositions[ARowIndex] +
    BarControl.WindowRect.Top;
end;

function TdxRibbonGroupBarControlViewInfo.GetSize: TSize;
begin
  Result := cxSize(FContentSize.cx + FNonContentAreaSize.cx, FContentSize.cy + FNonContentAreaSize.cy);
end;

{ TdxRibbonGroupBarControlDesignHelper }

class function TdxRibbonGroupBarControlDesignHelper.CanDynamicallyChangeViewLevels: Boolean;
begin
  Result := True;
end;

class function TdxRibbonGroupBarControlDesignHelper.GetForbiddenActions: TdxBarCustomizationActions;
begin
  Result := [caChangeButtonPaintStyle, caChangeRecentList];
end;

{ TdxRibbonCollapsedGroupPopupBarControl }

destructor TdxRibbonCollapsedGroupPopupBarControl.Destroy;
begin
  IsActive := False;
  inherited Destroy;
end;

procedure TdxRibbonCollapsedGroupPopupBarControl.Hide;
begin
  CloseUp;
end;

procedure TdxRibbonCollapsedGroupPopupBarControl.Popup(const AOwnerRect: TRect);
begin
  FAllowNCPaint := False;
  try
    inherited Popup(AOwnerRect);
  finally
    UpdateWindow(Handle);
    FAllowNCPaint := True;
    Perform(WM_NCPAINT, 1, 0);
  end;
end;

procedure TdxRibbonCollapsedGroupPopupBarControl.InitializeForPopup(AParentBarControl: TdxBarControl; ABar: TdxBar);
begin
  ABar.BarManager.Bars.BeginUpdate;
  try
    inherited;
    Bar.ItemLinks := ABar.ItemLinks;
    Bar.CaptionButtons := ABar.CaptionButtons;
  finally
    ABar.BarManager.Bars.EndUpdate;
  end;
  CreateControls;
  UpdateDoubleBuffered;
end;

function TdxRibbonCollapsedGroupPopupBarControl.AllowNCPaint: Boolean;
begin
  Result := FAllowNCPaint and inherited AllowNCPaint;
end;

procedure TdxRibbonCollapsedGroupPopupBarControl.DoBarMouseRightButtonDown(
  Shift: TShiftState; const APoint: TPoint; AItemControl: TdxBarItemControl;
  APrevSelectedControl: TdxBarItemControl; APointInClientRect: Boolean);
begin
  FocusItemControl(AItemControl);
  if CanShowPopupMenuOnMouseClick(True) then
    ShowPopup(AItemControl)
end;

function TdxRibbonCollapsedGroupPopupBarControl.GetCaption: TCaption;
begin
  Result := dxRibbonGetGroupCaption(MasterBar);
end;

function TdxRibbonCollapsedGroupPopupBarControl.GetPainter: TdxBarPainter;
begin
  Result := Ribbon.GroupsPainter;
end;

function TdxRibbonCollapsedGroupPopupBarControl.GetPopupSize: TSize;
begin
  HandleNeeded;
  ViewInfo.BeforeCalculate(False);
  try
    ViewInfo.Calculate;
  finally
    ViewInfo.AfterCalculate;
  end;
  Result := ViewInfo.GetSize;
end;

function TdxRibbonCollapsedGroupPopupBarControl.GetSizeForWidth(
  AStyle: TdxBarDockingStyle; AWidth: Integer): TSize;
begin
  Result := cxSize(ClientWidth, ClientHeight);
end;

function TdxRibbonCollapsedGroupPopupBarControl.IgnoreClickAreaWhenHidePopup: TRect;
begin
  Result := TdxRibbonGroupBarControl(ParentBar).WindowRect;
end;

function TdxRibbonCollapsedGroupPopupBarControl.IsPopup: Boolean;
begin
  Result := True;
end;

function TdxRibbonCollapsedGroupPopupBarControl.NeedHideOnNCMouseClick: Boolean;
begin
  Result := False;
end;

{ TdxRibbonTabGroup }

constructor TdxRibbonTabGroup.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRestriction := rtgrNone;
  FDesignSelectionHelper := GetSelectableItem(TdxDesignSelectionHelper.Create(Ribbon, Self, Tab));
end;

destructor TdxRibbonTabGroup.Destroy;
begin
  CheckUndockToolbar;
  inherited Destroy;
  FDesignSelectionHelper := nil;
end;

function TdxRibbonTabGroup.IsMergeAllowed: Boolean;
begin
  Result := Visible and (MergeKind <> rmkNone) and not IsToolBarShared;
end;

procedure TdxRibbonTabGroup.Assign(Source: TPersistent);

  function IsInheritanceUpdating: Boolean;
  begin
    Result := (Tab <> nil) and (csUpdating in Tab.ComponentState);
  end;

  function GetSourceToolbar: TdxBar;
  begin
    if (TdxRibbonTabGroup(Source).Toolbar <> nil) and IsInheritanceUpdating then
      Result := Tab.Ribbon.BarManager.BarByComponentName(TdxRibbonTabGroup(Source).Toolbar.Name)
    else
      Result := TdxRibbonTabGroup(Source).Toolbar;
  end;

begin
  if Source is TdxRibbonTabGroup then
  begin
    Caption := TdxRibbonTabGroup(Source).Caption;
    Restriction := TdxRibbonTabGroup(Source).Restriction;
    AssignedValues := TdxRibbonTabGroup(Source).AssignedValues;
    MergeOrder := TdxRibbonTabGroup(Source).MergeOrder;
    MergeKind := TdxRibbonTabGroup(Source).MergeKind;
    ToolBar := GetSourceToolbar;
  end
  else
    inherited Assign(Source);
end;

function TdxRibbonTabGroup.CreatedByMerging: Boolean;
begin
  Result := IsMerged and TdxBarAccess(ToolBar).MergeData.CreatedByMerging;
end;

procedure TdxRibbonTabGroup.DefineProperties(Filer: TFiler);

  function NeedWriteToolbarName: Boolean;
  var
    AAncestorToolbar: TdxBar;
  begin
    if Filer.Ancestor <> nil then
    begin
      AAncestorToolbar := TdxRibbonTabGroup(Filer.Ancestor).ToolBar;
      Result := (AAncestorToolbar = nil) and (Toolbar <> nil) or
        (AAncestorToolbar <> nil) and (ToolBar = nil) or
        (AAncestorToolbar <> nil) and (AAncestorToolbar.Name <> Toolbar.Name);
    end
    else
      Result := ToolBar <> nil;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Caption', ReadCaption, WriteCaption, GetIsCaptionAssigned and (Caption = ''));
  Filer.DefineProperty('ToolbarName', ReadToolbarName, WriteToolbarName, NeedWriteToolbarName);
end;

procedure TdxRibbonTabGroup.DockToolbar(AToolbar: TdxBar);
var
  ADockControl: TdxRibbonGroupsDockControl;
begin
  if Tab.Active or not (AToolbar.DockControl is TdxRibbonGroupsDockControl) then
    RibbonDockToolBar(AToolbar, Tab.DockControl)
  else
  begin
    ADockControl := TdxRibbonGroupsDockControl(AToolbar.DockControl);
    if (ADockControl.Ribbon <> Ribbon) or not ADockControl.Tab.Active then
      RibbonDockToolBar(AToolbar, Tab.DockControl);
  end;
end;

function TdxRibbonTabGroup.IsActuallyVisible: Boolean;
begin
  Result := Visible or (TdxBarManagerAccess(Ribbon.BarManager).GetBarIndexInRestoringList(ToolBar) <> -1);
end;

function TdxRibbonTabGroup.IsMerged: Boolean;
begin
  Result := (ToolBar <> nil) and ToolBar.IsMerged;
end;

function TdxRibbonTabGroup.IsToolbarAcceptable(AToolbar: TdxBar): Boolean;
begin
  Result := (AToolbar = nil) or (Ribbon.BarManager <> nil) and (Ribbon.BarManager = AToolbar.BarManager);
end;

procedure TdxRibbonTabGroup.Merge(AGroup: TdxRibbonTabGroup;
  AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);
begin
  TdxRibbonMergeHelper.MergeToolbarContainers(Self, AGroup, rmoCanCreateNewGroup in AMergeOptions);
end;

procedure TdxRibbonTabGroup.ReadCaption(AReader: TReader);
begin
  Caption := AReader.ReadString;
end;

procedure TdxRibbonTabGroup.ReadToolbarName(AReader: TReader);
begin
  FLoadedToolbarName := AReader.ReadString;
end;

procedure TdxRibbonTabGroup.Unmerge(AGroup: TdxRibbonTabGroup = nil);
begin
  TdxRibbonMergeHelper.UnmergeToolbarContainers(Self, AGroup);
end;

procedure TdxRibbonTabGroup.MoveToTab(ATab: TdxRibbonTab);
begin
  inherited SetCollection(ATab.Groups);
  if (FToolbar <> nil) and (ATab.Groups <> nil) then
  begin
    ValidateToolbar(FToolbar);
    if FToolbar <> nil then
      Ribbon.QuickAccessToolbar.UpdateGroupButton(FToolbar, True);
    FToolbar.DockControl := ATab.DockControl;
  end;
end;

procedure TdxRibbonTabGroup.SetCollection(Value: TCollection);
begin
  if (FToolbar <> nil) and (Value <> nil) then
  begin
    CheckUndockToolbar;
    inherited SetCollection(Value);
    ValidateToolbar(FToolbar);
    if FToolbar <> nil then
      Ribbon.QuickAccessToolbar.UpdateGroupButton(FToolbar, True);
    DockToolbar(FToolbar);
  end
  else
    inherited SetCollection(Value);
end;

procedure TdxRibbonTabGroup.UpdateBarManager(ABarManager: TdxBarManager);
begin
  UpdateToolbarValue;
  if ToolBar <> nil then
    ToolBar.DockControl := Tab.DockControl;
end;

procedure TdxRibbonTabGroup.UpdateToolbarValue;
begin
  if (FLoadedToolbarName <> '') and Ribbon.IsBarManagerValid then
  begin
    ToolBar := Ribbon.BarManager.BarByComponentName(FLoadedToolbarName);
    FLoadedToolbarName := '';
  end;
end;

function TdxRibbonTabGroup.VisibleBeforeMerging: Boolean;
begin
  Result := IsMerged and TdxBarAccess(ToolBar).MergeData.VisibleBeforeMerging;
end;

function TdxRibbonTabGroup.GetIniSection(const ABaseSection, ADelimiter: string): string;
begin
  Result := ABaseSection + ADelimiter + 'Group' + IntToStr(Index);
end;

procedure TdxRibbonTabGroup.LoadFromIni(ASource: TCustomIniFile; const ABaseSection, ADelimiter: string);
var
  ASection, AToolBarName: string;
begin
  ASection := GetIniSection(ABaseSection, ADelimiter);
  Caption := ASource.ReadString(ASection, 'Caption', Caption);
  AToolBarName := ASource.ReadString(ASection, 'ToolBar', EmptyStr);
  ToolBar := Ribbon.BarManager.BarByComponentName(AToolBarName);
  if ToolBar <> nil then
    ToolBar.Visible := ASource.ReadBool(ASection, 'ToolBarVisible', ToolBar.Visible);
end;

procedure TdxRibbonTabGroup.SaveToIni(ADestination: TCustomIniFile; const ABaseSection, ADelimiter: string);
var
  ASection: string;
begin
  ASection := GetIniSection(ABaseSection, ADelimiter);
  ADestination.WriteString(ASection, 'Caption', Caption);
  if ToolBar <> nil then
  begin
    ADestination.WriteString(ASection, 'ToolBar', ToolBar.Name);
    ADestination.WriteBool(ASection, 'ToolBarVisible', IsActuallyVisible or VisibleBeforeMerging);
  end
  else
  begin
    ADestination.WriteString(ASection, 'ToolBar', '');
    ADestination.WriteBool(ASection, 'ToolBarVisible', False);
  end;
end;

function TdxRibbonTabGroup.GetAdornerTargetElementBounds: TRect;
begin
  Result := ToolBar.Control.ClientRect;
end;

function TdxRibbonTabGroup.GetAdornerTargetElementControl: TWinControl;
begin
  if ToolBar <> nil then
    Result := ToolBar.Control
  else
    Result := nil;
end;

function TdxRibbonTabGroup.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible and Ribbon.ViewInfo.IsTabGroupsVisible;
end;

procedure TdxRibbonTabGroup.GetAdornerTargetElements(AList: TStrings);
begin
  if (ToolBar <> nil) and IsActuallyVisible then
    TdxBarAccess(ToolBar).GetAdornerTargetElements(AList);
end;

procedure TdxRibbonTabGroup.CheckUndockToolbar;
var
  ATab: TdxRibbonTab;
begin
  if GetAnotherTabWithOurToolbar(ATab) then
    RibbonDockToolBar(Toolbar, ATab.DockControl)
  else
    RibbonUndockToolBar(Toolbar);
end;

function TdxRibbonTabGroup.GetAnotherTabWithOurToolbar(out ATab: TdxRibbonTab): Boolean;
var
  ATabIndex: Integer;
  ATempTab: TdxRibbonTab;
begin
  if ToolBar <> nil then
    for ATabIndex := 0 to Ribbon.TabCount - 1 do
    begin
      ATempTab := Ribbon.Tabs[ATabIndex];
      if (ATempTab <> Tab) and ATempTab.Groups.ContainsToolBar(ToolBar) then
      begin
        ATab := ATempTab;
        Exit(True);
      end;
    end;

  Result := False;
end;

function TdxRibbonTabGroup.GetCanCollapse: Boolean;
begin
  Result := Restriction <> rtgrNoCollapse;
end;

function TdxRibbonTabGroup.GetCaption: string;
begin
  if GetIsCaptionAssigned then
    Result := FCaption
  else
    if ToolBar <> nil then
      Result := ToolBar.Caption
    else
      Result := ''
end;

function TdxRibbonTabGroup.GetIsCaptionAssigned: Boolean;
begin
  Result := rtgavCaption in AssignedValues;
end;

function TdxRibbonTabGroup.GetIsToolBarShared: Boolean;
var
  ATab: TdxRibbonTab;
begin
  Result := GetAnotherTabWithOurToolbar(ATab);
end;

function TdxRibbonTabGroup.GetRibbon: TdxCustomRibbon;
begin
  if Tab <> nil then
    Result := Tab.Ribbon
  else
    Result := nil
end;

function TdxRibbonTabGroup.GetTab: TdxRibbonTab;
begin
  if Collection <> nil then
    Result := (Collection as TdxRibbonTabGroups).Tab
  else
    Result := nil;
end;

function TdxRibbonTabGroup.GetToolbar: TdxBar;
begin
  if (FLoadedToolbarName <> '') and (Tab <> nil) and Ribbon.IsBarManagerValid and IsAncestorComponentDifferencesDetection(Tab) then
    Result := Ribbon.BarManager.BarByComponentName(FLoadedToolbarName)
  else
    Result := FToolbar;
end;

function TdxRibbonTabGroup.GetVisible: Boolean;
begin
  Result := (ToolBar <> nil) and ToolBar.Visible;
end;

procedure TdxRibbonTabGroup.SetAssignedValues(const AValue: TdxRibbonTabGroupAssignedValues);
begin
  if AssignedValues <> AValue then
  begin
    FAssignedValues := AValue;
    Changed(True);
  end;
end;

procedure TdxRibbonTabGroup.SetCanCollapse(Value: Boolean);
begin
  if Value then
    Restriction := rtgrNone
  else
    Restriction := rtgrNoCollapse;
end;

procedure TdxRibbonTabGroup.SetCaption(const AValue: string);
begin
  Include(FAssignedValues, rtgavCaption);
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed(True);
  end;
end;

procedure TdxRibbonTabGroup.SetMergeOrder(const Value: Integer);
begin
  FMergeOrder := Max(Value, 0);
end;

procedure TdxRibbonTabGroup.SetRestriction(AValue: TdxRibbonTabGroupRestriction);
begin
  if AValue <> FRestriction then
  begin
    FRestriction := AValue;
    if (ToolBar <> nil) and (Toolbar.Control <> nil) then
      ToolBar.Control.RepaintBar;
  end;
end;

procedure TdxRibbonTabGroup.SetToolbar(Value: TdxBar);
begin
  if IsToolbarAcceptable(Value) and (FToolbar <> Value) then
  begin
    CheckUndockToolbar;
    if Value = nil then
    begin
      Ribbon.QuickAccessToolbar.UpdateGroupButton(FToolbar, True);
      FToolbar := Value;
      Free;
    end
    else
    begin
      ValidateToolbar(Value);
      if FToolbar <> nil then
        Ribbon.QuickAccessToolbar.UpdateGroupButton(FToolbar, True);
      FToolbar := Value;
      Value.FreeNotification(Tab);
      DockToolbar(Value);
    end;
  end;
end;

procedure TdxRibbonTabGroup.ValidateToolbar(Value: TdxBar);
var
  I: Integer;
begin
  if Value = Ribbon.QuickAccessToolbar.Toolbar then
    raise EdxException.Create(sdxErrorToolbarUsedAsQAT);
  for I := 0 to Tab.Groups.Count - 1 do
    if (Tab.Groups[I] <> Self) and (Tab.Groups[I].ToolBar = Value) then
      raise EdxException.Create(sdxErrorToolbarUsedInAnotherGroupInThisTab);
end;

procedure TdxRibbonTabGroup.WriteCaption(AWriter: TWriter);
begin
  AWriter.WriteString(Caption);
end;

procedure TdxRibbonTabGroup.WriteToolbarName(AWriter: TWriter);
begin
  if ToolBar <> nil then
    AWriter.WriteString(ToolBar.Name)
  else
    AWriter.WriteString('');
end;

{ TdxRibbonTabGroups }

constructor TdxRibbonTabGroups.Create(ATab: TdxRibbonTab);
begin
  inherited Create(ATab.GetGroupClass);
  FTab := ATab;
end;

function TdxRibbonTabGroups.Add: TdxRibbonTabGroup;
begin
  Result := TdxRibbonTabGroup(inherited Add);
end;

function TdxRibbonTabGroups.ContainsToolBar(AToolBar: TdxBar): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := AToolBar = Items[I].ToolBar;
    if Result then Break;
  end;
end;

function TdxRibbonTabGroups.Find(const ACaption: string; out AGroup: TdxRibbonTabGroup): Boolean;
var
  I: Integer;
begin
  AGroup := nil;
  for I := 0 to Count - 1 do
    if ACaption = Items[I].Caption then
    begin
      AGroup := Items[I];
      Break;
    end;

  Result := AGroup <> nil;
end;

function TdxRibbonTabGroups.FindByToolBar(AToolBar: TdxBar): TdxRibbonTabGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].ToolBar = AToolBar then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TdxRibbonTabGroups.FreeNotification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].Toolbar = AComponent then
      Items[I].Toolbar := nil;
  end;
end;

function TdxRibbonTabGroups.GetIniSection(const AGroupIndex: Integer; ASource: TCustomIniFile): string;
begin
  Result := TdxRibbonTabCollection(Tab.Collection).GetIniSection(Tab.Name, ASource) + '.' + 'Group' + IntToStr(AGroupIndex);
end;

function TdxRibbonTabGroups.GetOwner: TPersistent;
begin
  Result := Tab;
end;

function TdxRibbonTabGroups.IndexOf(AGroup: TdxRibbonTabGroup): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I] = AGroup then
    begin
      Result := I;
      Break;
    end;
end;

function TdxRibbonTabGroups.Insert(AIndex: Integer): TdxRibbonTabGroup;
begin
  Result := TdxRibbonTabGroup(inherited Insert(AIndex));
end;

procedure TdxRibbonTabGroups.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited;
  Tab.Ribbon.Changed;
end;

procedure TdxRibbonTabGroups.Unmerge(AGroup: TdxRibbonTabGroup = nil);
var
  AItem: TdxRibbonTabGroup;
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    AItem := Items[I];
    if (AGroup = nil) or TdxRibbonMergeHelper.IsMergedWith(AGroup.ToolBar, AItem.ToolBar) then
      AItem.Unmerge(AGroup);
  end;
end;

procedure TdxRibbonTabGroups.Update(Item: TCollectionItem);
begin
  Tab.Ribbon.Changed;
  if not Tab.IsDestroying then
  begin
    if Tab.Active and Tab.DockControl.HandleAllocated then
    begin
      Tab.DockControl.UpdateDock;
      Tab.DockControl.FullInvalidate;
    end;
    if Tab.MergeData.CreatedByMerging and (Tab.Groups.Count = 0) then
      Tab.Free;
  end;
end;

procedure TdxRibbonTabGroups.UpdateGroupToolbarValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateToolbarValue;
end;

function TdxRibbonTabGroups.GetItem(Index: Integer): TdxRibbonTabGroup;
begin
  Result := TdxRibbonTabGroup(inherited Items[Index]);
end;

procedure TdxRibbonTabGroups.SetItem(Index: Integer; const Value: TdxRibbonTabGroup);
begin
  TdxRibbonTabGroup(inherited Items[Index]).Assign(Value);
end;

{ TdxRibbonCustomToolbar }

constructor TdxRibbonCustomToolbar.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FVisible := True;
  FDockControl := CreateDockControl;
end;

destructor TdxRibbonCustomToolbar.Destroy;
begin
  Toolbar := nil;
  FreeAndNil(FDockControl);
  inherited Destroy;
end;

procedure TdxRibbonCustomToolbar.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonCustomToolbar then
  begin
    Ribbon.BeginUpdate;
    try
      DoAssign(TdxRibbonCustomToolbar(Source));
    finally
      Ribbon.EndUpdate;
    end;
  end;
end;

function TdxRibbonCustomToolbar.Contains(AItemLink: TdxBarItemLink): Boolean;
begin
  if (AItemLink <> nil) and (AItemLink.OriginalItemLink <> nil) then
    AItemLink := AItemLink.OriginalItemLink;
  Result := Toolbar.ItemLinks.IndexOf(AItemLink) <> -1;
end;

procedure TdxRibbonCustomToolbar.DoAssign(Source: TdxRibbonCustomToolbar);
begin
  Toolbar := Source.Toolbar;
  Visible := Source.Visible;
end;

procedure TdxRibbonCustomToolbar.DoToolbarRemoving;
begin
  // do nothing
end;

procedure TdxRibbonCustomToolbar.UpdateColorScheme;
begin
  if Visible and DockControl.Visible then
    DockControl.UpdateColorScheme;
end;

procedure TdxRibbonCustomToolbar.UpdateRibbon;
begin
  if not Ribbon.IsDestroying then
  begin
    Ribbon.UpdateColorScheme;
    Ribbon.Update;
  end;
end;

function TdxRibbonCustomToolbar.GetRibbon: TdxCustomRibbon;
begin
  Result := FRibbon;
end;

function TdxRibbonCustomToolbar.GetToolbar: TdxBar;
begin
  Result := FToolbar;
end;

procedure TdxRibbonCustomToolbar.CheckUndockGroupToolbar(AValue: TdxBar);
var
  I, J: Integer;
begin
  for I := 0 to Ribbon.TabCount - 1 do
    for J := Ribbon.Tabs[I].Groups.Count - 1 downto 0 do
    begin
      with Ribbon.Tabs[I].Groups[J] do
        if ToolBar = AValue then
          ToolBar := nil;
    end;
end;

procedure TdxRibbonCustomToolbar.SetToolbar(AValue: TdxBar);
begin
  if FToolbar <> AValue then
  begin
    Ribbon.BeginUpdate;
    try
      if (FToolbar <> nil) and not (csDestroying in FToolbar.ComponentState) then
      begin
        DoToolbarRemoving;
        FToolbar.RemoveFreeNotification(Ribbon);
        RibbonUndockToolBar(FToolbar);
      end;
      FToolbar := AValue;
      if FToolbar <> nil then
      begin
        CheckUndockGroupToolbar(AValue);
        FToolbar.FreeNotification(Ribbon);
        RibbonDockToolBar(FToolbar, DockControl);
      end
      else
        DockControl.Visible := False;
    finally
      Ribbon.CancelUpdate;
      if not Ribbon.IsLocked then
        UpdateRibbon;
    end;
  end;
end;

function TdxRibbonCustomToolbar.GetActualVisible: Boolean;
begin
  Result := Visible and (Toolbar <> nil) and Toolbar.Visible;
end;

function TdxRibbonCustomToolbar.GetIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if (Toolbar <> nil) and (Toolbar.Control <> nil) then
    Result := Toolbar.Control.IAccessibilityHelper
  else
    Result := nil;
end;

procedure TdxRibbonCustomToolbar.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DockControl.Visible := AValue and (Toolbar <> nil);
    UpdateRibbon;
  end;
end;

{ TdxRibbonTabAreaToolbar }

function TdxRibbonTabAreaToolbar.CreateDockControl: TdxCustomRibbonDockControl;
begin
  Result := TdxRibbonTabAreaToolbarDockControl.Create(Ribbon);
end;

function TdxRibbonTabAreaToolbar.IsMerged: Boolean;
begin
  Result := (Toolbar <> nil) and Toolbar.IsMerged;
end;

procedure TdxRibbonTabAreaToolbar.Merge(ATabAreaToolbar: TdxRibbonTabAreaToolbar;
  AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);
begin
  if (ATabAreaToolbar <> nil) and (ATabAreaToolbar.Toolbar <> nil) and ATabAreaToolbar.DockControl.Visible then
    TdxRibbonMergeHelper.MergeToolbarContainers(Self, ATabAreaToolbar, rmoCanCreateTabAreaToolbar in AMergeOptions);
end;

procedure TdxRibbonTabAreaToolbar.Unmerge(ATabAreaToolbar: TdxRibbonTabAreaToolbar = nil);
begin
  TdxRibbonMergeHelper.UnmergeToolbarContainers(Self, ATabAreaToolbar);
end;

{ TdxRibbonTabAreaSearchToolbar }

function TdxRibbonTabAreaSearchToolbar.IsMerged: Boolean;
begin
  Result := (Toolbar <> nil) and Toolbar.IsMerged;
end;

procedure  TdxRibbonTabAreaSearchToolbar.Merge(ATabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar;
  AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);
begin
  if (ATabAreaSearchToolbar <> nil) and (ATabAreaSearchToolbar.Toolbar <> nil) and ATabAreaSearchToolbar.DockControl.Visible then
    TdxRibbonMergeHelper.MergeToolbarContainers(Self, ATabAreaSearchToolbar, rmoCanCreateTabAreaSearchToolbar in AMergeOptions);
end;

procedure TdxRibbonTabAreaSearchToolbar.Unmerge(ATabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar = nil);
begin
  TdxRibbonMergeHelper.UnmergeToolbarContainers(Self, ATabAreaSearchToolbar);
end;

function TdxRibbonTabAreaSearchToolbar.CreateDockControl: TdxCustomRibbonDockControl;
begin
  Result := TdxRibbonTabAreaSearchToolbarDockControl.Create(Ribbon);
end;

{ TdxRibbonQuickAccessToolbar }

constructor TdxRibbonQuickAccessToolbar.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(ARibbon);
  FPosition := qtpAboveRibbon;
end;

function TdxRibbonQuickAccessToolbar.AddGroupButton(AToolbar: TdxBar): TdxRibbonQuickAccessGroupButton;
begin
  Result := nil;
  if AToolbar <> nil then
  begin
    Result := TdxRibbonQuickAccessGroupButton(Toolbar.ItemLinks.AddItem(TdxRibbonQuickAccessGroupButton).Item);
    if Result.IsToolbarAcceptable(AToolbar) then
    begin
      Result.Toolbar := AToolbar;
      Result.Name := 'QAT' + Result.Toolbar.Name;
    end
    else
      FreeAndNil(Result);
  end;
end;

procedure TdxRibbonQuickAccessToolbar.BarManagerLoadIni(const AEventData: TdxBarIniFileEventData);
var
  ABar: TdxBar;
  ABarSection, AItemSection: string;
  AGroupButton: TdxRibbonQuickAccessGroupButton;
  I, ALinkCount: Integer;
begin
  if Toolbar <> nil then
  begin
    ABarSection := TdxBarAccess(Toolbar).GetIniSection(AEventData.BaseSection, Toolbar.Index);
    ALinkCount := AEventData.IniFile.ReadInteger(ABarSection, 'ItemLinkCount', 0);
    for I := 0 to ALinkCount - 1 do
    begin
      AItemSection := TdxBarItemLinkAccess.GetIniSection(ABarSection, I, AEventData.StoringKind);
      ABar := Ribbon.BarManager.BarByComponentName(AEventData.IniFile.ReadString(AItemSection, 'ToolbarName', ''));
      if ABar <> nil then
      begin
        AGroupButton := AddGroupButton(ABar);
        if (AGroupButton <> nil) and (I <> ALinkCount - 1) then
          Toolbar.ItemLinks.Move(Toolbar.ItemLinks.Count - 1, I);
      end;
    end;
  end;
end;

procedure TdxRibbonQuickAccessToolbar.BarManagerSaveIni(const AEventData: TdxBarIniFileEventData);
var
  AItemLink: TdxBarItemLink;
  ASection: string;
  AToolbarName: string;
  I: Integer;
begin
  if Toolbar <> nil then
    for I := 0 to Toolbar.ItemLinks.Count - 1 do
    begin
      AItemLink := Toolbar.ItemLinks[I];
      if AItemLink.Item is TdxRibbonQuickAccessGroupButton then
      begin
        if TdxBarItemLinkAccess(AItemLink).CreatedFromMergingWith = nil then
        begin
          ASection := TdxBarAccess(Toolbar).GetIniSection(AEventData.BaseSection, Toolbar.Index);
          ASection := TdxBarItemLinkAccess(AItemLink).GetIniSection(ASection, I, AEventData.StoringKind);
          if TdxRibbonQuickAccessGroupButton(AItemLink.Item).Toolbar <> nil then
            AToolbarName := TdxRibbonQuickAccessGroupButton(AItemLink.Item).Toolbar.Name
          else
            AToolbarName := '';
          AEventData.IniFile.WriteString(ASection, 'ToolbarName', AToolbarName);
        end;
      end;
    end;
end;

function TdxRibbonQuickAccessToolbar.HasGroupButtonForToolbar(AToolbar: TdxBar): Boolean;
begin
  Result := (Toolbar <> nil) and
    TdxRibbonQuickAccessToolbarHelper.HasGroupButtonForToolbar(Toolbar.ItemLinks, AToolbar);
end;

procedure TdxRibbonQuickAccessToolbar.DoAssign(Source: TdxRibbonCustomToolbar);
begin
  inherited DoAssign(Source);
  if Source is TdxRibbonQuickAccessToolbar then
    Position := TdxRibbonQuickAccessToolbar(Source).Position;
end;

procedure TdxRibbonQuickAccessToolbar.DoToolbarRemoving;

  procedure RemoveGroupButtons(AToolbar: TdxBar);
  var
    I: Integer;
  begin
    for I := AToolbar.ItemLinks.Count - 1 downto 0 do
      if AToolbar.ItemLinks[I].Item is TdxRibbonQuickAccessGroupButton then
        AToolbar.ItemLinks[I].Item.Free;
  end;

begin
  RemoveGroupButtons(FToolbar);
  inherited DoToolbarRemoving;
end;

function TdxRibbonQuickAccessToolbar.CreateDockControl: TdxCustomRibbonDockControl;
begin
  Result := TdxRibbonQuickAccessToolbarDockControl.Create(Ribbon);
end;

function TdxRibbonQuickAccessToolbar.GetMenuItemsForMark: TdxRibbonPopupMenuItems;
begin
  Result := Ribbon.GetValidPopupMenuItems - [rpmiQATAddRemoveItem];
end;

function TdxRibbonQuickAccessToolbar.IsMerged: Boolean;
begin
  Result := (Toolbar <> nil) and Toolbar.IsMerged;
end;

procedure TdxRibbonQuickAccessToolbar.Merge(AQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
  AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);
begin
  if (AQuickAccessToolbar <> nil) and (AQuickAccessToolbar.Toolbar <> nil) and AQuickAccessToolbar.DockControl.Visible then
    TdxRibbonMergeHelper.MergeToolbarContainers(Self, AQuickAccessToolbar, rmoCanCreateQATToolbar in AMergeOptions);
end;

procedure TdxRibbonQuickAccessToolbar.Unmerge(AQuickAccessToolbar: TdxRibbonQuickAccessToolbar = nil);
begin
  TdxRibbonMergeHelper.UnmergeToolbarContainers(Self, AQuickAccessToolbar);
end;

procedure TdxRibbonQuickAccessToolbar.UpdateGroupButton(AForToolbar: TdxBar; ABeforeUndock: Boolean);
var
  AGroupButton: TdxRibbonQuickAccessGroupButton;
  I: Integer;
begin
  if Toolbar = nil then
    Exit;
  for I := 0 to Toolbar.ItemLinks.Count - 1 do
    if Toolbar.ItemLinks[I].Item is TdxRibbonQuickAccessGroupButton then
    begin
      AGroupButton := TdxRibbonQuickAccessGroupButton(Toolbar.ItemLinks[I].Item);
      if AGroupButton.Toolbar = AForToolbar then
      begin
        if ABeforeUndock then
          AGroupButton.Toolbar := nil
        else
          AGroupButton.Update;
        Break;
      end;
    end;
end;

procedure TdxRibbonQuickAccessToolbar.UpdateMenuItems(AItems: TdxBarItemLinks);
begin
  Ribbon.PopulatePopupMenuItems(AItems, GetMenuItemsForMark, Ribbon.PopupMenuItemClick);
end;

procedure TdxRibbonQuickAccessToolbar.SetPosition(AValue: TdxQuickAccessToolbarPosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    UpdateRibbon;
  end;
end;

{ TdxRibbonCustomButtonPersistent }

constructor TdxRibbonCustomButtonPersistent.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FStretchGlyph := True;
end;

destructor TdxRibbonCustomButtonPersistent.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxRibbonCustomButtonPersistent.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonApplicationButton then
  begin
    Ribbon.BeginUpdate;
    try
      Glyph := TdxRibbonApplicationButton(Source).Glyph;
      ScreenTip := TdxRibbonApplicationButton(Source).ScreenTip;
      StretchGlyph := TdxRibbonApplicationButton(Source).StretchGlyph;
    finally
      Ribbon.EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxRibbonCustomButtonPersistent.GlyphChanged(Sender: TObject);
begin
  Update;
end;

procedure TdxRibbonCustomButtonPersistent.SetGlyph(Value: TdxSmartGlyph);
begin
  FGlyph.Assign(Value);
end;

procedure TdxRibbonCustomButtonPersistent.SetScreenTip(const Value: TdxScreenTip);
begin
  if FScreenTip <> Value then
  begin
    if FScreenTip <> nil then
      FScreenTip.RemoveFreeNotification(Ribbon);
    FScreenTip := Value;
    if FScreenTip <> nil then
      FScreenTip.FreeNotification(Ribbon);
  end;
end;

procedure TdxRibbonCustomButtonPersistent.SetStretchGlyph(const Value: Boolean);
begin
  if FStretchGlyph <> Value then
  begin
    FStretchGlyph := Value;
    Update;
  end;
end;

procedure TdxRibbonCustomButtonPersistent.Update;
begin
  Ribbon.Changed;
  Ribbon.FullInvalidate;
end;

{ TdxRibbonApplicationButton }

constructor TdxRibbonApplicationButton.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(ARibbon);
  FVisible := True;
end;

destructor TdxRibbonApplicationButton.Destroy;
begin
  BarAccessibilityHelperOwnerObjectDestroyed(FIAccessibilityHelper);
  Menu := nil;
  inherited Destroy;
end;

procedure TdxRibbonApplicationButton.Assign(Source: TPersistent);
begin
  Ribbon.BeginUpdate;
  try
    if Source is TdxRibbonApplicationButton then
    begin
      KeyTip := TdxRibbonApplicationButton(Source).KeyTip;
      Menu := TdxRibbonApplicationButton(Source).Menu;
      Visible := TdxRibbonApplicationButton(Source).Visible;
    end;
    inherited Assign(Source);
  finally
    Ribbon.EndUpdate;
  end;
end;

function TdxRibbonApplicationButton.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonApplicationButtonAccessibilityHelper;
end;

function TdxRibbonApplicationButton.CanShowPopup: Boolean;
begin
  Result := (IMenu <> nil) and IMenu.CanShowPopup(Ribbon);
end;

function TdxRibbonApplicationButton.ClosePopup: Boolean;
begin
  Result := (IMenu <> nil) and IMenu.ClosePopup;
end;

procedure TdxRibbonApplicationButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = Menu then
      Menu := nil;
    if AComponent = ScreenTip then
      ScreenTip := nil;
  end;
end;

function TdxRibbonApplicationButton.Popup(var AClosedByEscape: Boolean): Boolean;
begin
  Result := IMenu.Popup(Ribbon, AClosedByEscape)
end;

function TdxRibbonApplicationButton.GetIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := GetAccessibilityHelperClass.Create(Self);
  Result := FIAccessibilityHelper;
end;

procedure TdxRibbonApplicationButton.SetMenu(const Value: TComponent);
var
  ANewIMenu: IdxRibbonApplicationMenu;
begin
  if FMenu <> Value then
  begin
    if (Value = nil) or Supports(Value, IdxRibbonApplicationMenu, ANewIMenu) then
    begin
      if FMenu <> nil then
      begin
        ClosePopup;
        FMenu.RemoveFreeNotification(Ribbon);
        FIMenu := nil;
        FMenu := nil;
      end;
      if Value <> nil then
      begin
        FMenu := Value;
        FMenu.FreeNotification(Ribbon);
        FIMenu := ANewIMenu;
      end;
    end;
  end;
end;

procedure TdxRibbonApplicationButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Update;
  end;
end;

procedure TdxRibbonApplicationButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

{ TdxRibbonContext }

constructor TdxRibbonContext.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAlpha := MaxByte;
  FColor := clWhite;
  FVisible := False;
end;

destructor TdxRibbonContext.Destroy;
begin
  FreeAndNil(FAlphaAnimation);
  inherited Destroy;
end;

function TdxRibbonContext.CanAnimateContextShowing: Boolean;
begin
  Result := (dxRibbonContextualTabLabelShowAnimationTime > 0) and (Ribbon.Style >= rs2019) and not Ribbon.IsDesigning;
end;

function TdxRibbonContext.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TdxRibbonContext.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonContext then
  begin
    FCaption := TdxRibbonContext(Source).Caption;
    FColor := TdxRibbonContext(Source).Color;
    FMergeOrder := TdxRibbonContext(Source).MergeOrder;
    FVisible := TdxRibbonContext(Source).Visible;
  end
  else
    inherited;
end;

procedure TdxRibbonContext.Activate(AActivateFirstTab: Boolean = True);
begin
  if TabCount > 0 then
  begin
    if not Visible then
    begin
      Ribbon.BeginUpdate;
      try
        Visible := True;
        Tabs[0].Active := True;
      finally
        Ribbon.EndUpdate;
      end;
    end
    else
      if AActivateFirstTab then
        Tabs[0].Active := True;

    if GetMergedWithContext <> nil then
      GetMergedWithContext.Activate(AActivateFirstTab)
  end;
end;

procedure TdxRibbonContext.ColorChanged;
var
  ATab: TdxRibbonTab;
begin
  Changed(False);

  ATab := Ribbon.ActiveTab;
  if (ATab <> nil) and (ATab.Context = Self) then
  begin
    if ATab.DockControl <> nil then
      ATab.DockControl.FullInvalidate;
  end;
end;

procedure TdxRibbonContext.UpdateMergeTargetVisibility;
begin
  if GetMergedWithContext <> nil then
    GetMergedWithContext.Visible := Visible;
end;

procedure TdxRibbonContext.VisibleChanged;
var
  ATab: TdxRibbonTab;
begin
  FreeAndNil(FAlphaAnimation);
  UpdateMergeTargetVisibility;

  if Visible then
  begin
    if CanAnimateContextShowing then
    begin
      FAlphaAnimation := TdxRibbonContextAnimation.Create(dxRibbonContextualTabLabelShowAnimationTime, Self);
      FAlphaAnimation.Resume;
    end;
  end
  else
  begin
    ATab := Ribbon.ActiveTab;
    if (ATab <> nil) and (ATab.Context = Self) then
    begin
      Ribbon.ActiveTab := Ribbon.GetNextActiveTab(nil);
      Exit;
    end;
  end;

  Changed(True);
end;

function TdxRibbonContext.GetCollection: TdxRibbonContexts;
begin
  Result := TdxRibbonContexts(inherited Collection);
end;

function TdxRibbonContext.GetMergedWithContext: TdxRibbonContext;
var
  ATab: TdxRibbonTab;
begin
  Result := nil;
  if TabCount > 0 then
  begin
    ATab := Tabs[0];
    if (ATab.MergeData.MergedWith <> nil) then
      Result := ATab.MergeData.MergedWith.Context;
  end;
end;

function TdxRibbonContext.GetRibbon: TdxCustomRibbon;
begin
  Result := Collection.Ribbon;
end;

function TdxRibbonContext.GetTab(Index: Integer): TdxRibbonTab;
var
  I: Integer;
begin
  Result := nil;
  if Index >= 0 then
    for I := 0 to Ribbon.Tabs.Count - 1 do
      if Ribbon.Tabs[I].Context = Self then
      begin
        if Index = 0 then
          Exit(Ribbon.Tabs[I]);
        Dec(Index);
      end;
end;

function TdxRibbonContext.GetTabCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Ribbon.Tabs.Count - 1 do
  begin
    if Ribbon.Tabs[I].Context = Self then
      Inc(Result);
  end;
end;

procedure TdxRibbonContext.SetAlpha(AValue: Byte);
begin
  if FAlpha <> AValue then
  begin
    FAlpha := AValue;
    Changed(False);
  end;
end;

procedure TdxRibbonContext.SetCaption(const AValue: string);
begin
  if Caption <> AValue then
  begin
    FCaption := AValue;
    Changed(True);
  end;
end;

procedure TdxRibbonContext.SetColor(AValue: TColor);
begin
  if Color <> AValue then
  begin
    FColor := AValue;
    ColorChanged;
  end;
end;

procedure TdxRibbonContext.SetMergeOrder(const AValue: Integer);
begin
  FMergeOrder := Max(AValue, 0);
end;

procedure TdxRibbonContext.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    VisibleChanged;
  end;
end;

{ TdxRibbonContexts }

constructor TdxRibbonContexts.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(TdxRibbonContext);
  FRibbon := ARibbon;
end;

function TdxRibbonContexts.Find(const ACaption: string): TdxRibbonContext;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Caption = ACaption then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxRibbonContexts.Add: TdxRibbonContext;
begin
  Result := TdxRibbonContext(inherited Add);
end;

function TdxRibbonContexts.GetItemFromIndex(AIndex: Integer): TdxRibbonContext;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    Result := nil
  else
    Result := Items[AIndex];
end;

function TdxRibbonContexts.IndexOf(AContext: TdxRibbonContext): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I] = AContext then
    begin
      Result := I;
      Break;
    end;
end;

function TdxRibbonContexts.Insert(AIndex: Integer): TdxRibbonContext;
begin
  Result := TdxRibbonContext(inherited Insert(AIndex));
end;

procedure TdxRibbonContexts.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnExtracting) and not Ribbon.IsDestroying then
    Ribbon.Tabs.RemoveContext(TdxRibbonContext(Item));
end;

procedure TdxRibbonContexts.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    Ribbon.Invalidate
  else
    Ribbon.Changed;
end;

function TdxRibbonContexts.GetActiveContext: TdxRibbonContext;
begin
  if Ribbon.ActiveTab <> nil then
    Result := Ribbon.ActiveTab.Context
  else
    Result := nil;
end;

function TdxRibbonContexts.GetItem(Index: Integer): TdxRibbonContext;
begin
  Result := TdxRibbonContext(inherited Items[Index]);
end;

procedure TdxRibbonContexts.SetItem(Index: Integer; const Value: TdxRibbonContext);
begin
  TdxRibbonContext(inherited Items[Index]).Assign(Value);
end;

{ TdxRibbonContextAnimation }

constructor TdxRibbonContextAnimation.Create(ATime: Cardinal; AContext: TdxRibbonContext);
begin
  inherited Create(ATime, ateLinear, MaxByte);
  FContext := AContext;
  FContext.Alpha := 0;
  FreeOnTerminate := False;
end;

procedure TdxRibbonContextAnimation.DoAnimate;
begin
  FContext.Alpha := Position;
end;

{ TdxRibbonTab }

constructor TdxRibbonTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMergeData := TdxRibbonTabMergeData.Create;
  FDockControl := TdxRibbonGroupsDockControl.Create(Self);
  FDockControl.Visible := False;
  FDockControl.Align := dalNone;
  FVisible := True;
  FLoadedIndex := -1;
  FContextIndex := -1;
  FSaveContextIndex := -1;
  FGroups := TdxRibbonTabGroups.Create(Self);
end;

destructor TdxRibbonTab.Destroy;
begin
  UnmergeBeforeDestroy;
  Context := nil;
  BarAccessibilityHelperOwnerObjectDestroyed(FIAccessibilityHelper);
  FLastIndex := Index;
  dxFader.Remove(Self);
  Groups.Clear;
  FreeAndNil(FGroups);
  FreeAndNil(FDockControl);
  FreeAndNil(FMergeData);
  inherited Destroy;
  FDesignSelectionHelper := nil;
end;

procedure TdxRibbonTab.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonTab then
  begin
    Ribbon.BeginUpdate;
    try
      AssignCommonProperties(TdxRibbonTab(Source));
      Active := TdxRibbonTab(Source).Active;
      Groups := TdxRibbonTab(Source).Groups;
      Visible := TdxRibbonTab(Source).Visible;
    finally
      Ribbon.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonTab.AssignCommonProperties(ASource: TdxRibbonTab);
begin
  Ribbon.BeginUpdate;
  try
    KeyTip := ASource.KeyTip;
    Caption := ASource.Caption;
    MergeKind := ASource.MergeKind;
    MergeOrder := ASource.MergeOrder;
  finally
    Ribbon.EndUpdate;
  end;
end;

procedure TdxRibbonTab.AddToolBar(AToolBar: TdxBar);
begin
  if AToolbar <> nil then
    Groups.Add.Toolbar := AToolBar;
end;

function TdxRibbonTab.IsMergeAllowed: Boolean;
begin
  Result := Visible and (MergeKind <> rmkNone);
end;

procedure TdxRibbonTab.Invalidate;
begin
  if Visible then
    Ribbon.InvalidateRect(ViewInfo.Bounds);
end;

procedure TdxRibbonTab.MakeVisible;
begin
  Visible := True;
  Ribbon.ViewInfo.TabsAreaViewInfo.TabsViewInfo.MakeTabVisible(Self);
end;

procedure TdxRibbonTab.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Index', ReadIndex, WriteIndex, True);
  Filer.DefineProperty('ContextIndex', ReadContextIndex, WriteContextIndex, FContext <> nil);
end;

function TdxRibbonTab.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := (AParent as TdxCustomRibbon).Tabs;
end;

function TdxRibbonTab.GetDisplayCaption: string;
begin
  Result := Caption;
  if dxDefaultBooleanToBoolean(Ribbon.CapitalizeTabCaptions, Ribbon.Style = rs2013) then
    Result := UpperCase(Result);
end;

function TdxRibbonTab.GetDisplayName: string;
begin
  Result := Format('%s - ''%s''', [Name, Caption]);
end;

procedure TdxRibbonTab.Loaded;
begin
  inherited Loaded;
  FIsPredefined := True;
  Groups.UpdateGroupToolbarValues;
end;

procedure TdxRibbonTab.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Groups <> nil then
      Groups.FreeNotification(AComponent);
  end;
end;

procedure TdxRibbonTab.Merge(ATab: TdxRibbonTab; AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);

  function GetInsertPosition(ASourceGroup: TdxRibbonTabGroup): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Groups.Count - 1 do
    begin
      if ASourceGroup.MergeOrder >= Groups[I].MergeOrder then
        Result := I + 1;
    end;
  end;

  function CanMerge(AGroup, ATargetGroup: TdxRibbonTabGroup): Boolean;
  begin
    Result := (AGroup.MergeKind = ATargetGroup.MergeKind) and
      (ATargetGroup.Caption = AGroup.Caption) and not ATargetGroup.IsToolBarShared;
  end;

  function FindTargetGroup(AGroup: TdxRibbonTabGroup; out ATargetGroup: TdxRibbonTabGroup): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Groups.Count - 1 do
      if CanMerge(AGroup, Groups.Items[I]) then
      begin
        ATargetGroup := Groups.Items[I];
        Exit(True);
      end;
  end;

var
  AGroup: TdxRibbonTabGroup;
  ATargetGroup: TdxRibbonTabGroup;
  I: Integer;
begin
  if (ATab = nil) or (ATab = Self) then
    RaiseMergingError(@dxSBAR_RIBBONCANTMERGETAB);
  if not (Ribbon.IsBarManagerValid and ATab.Ribbon.IsBarManagerValid) then
    RaiseMergingError(@dxSBAR_RIBBONCANTMERGEWITHOUTBARMANAGER);
  if ATab.IsMerged then
    RaiseMergingError(@dxSBAR_RIBBONONEOFTABGROUPSALREADYMERGED);

  Ribbon.BeginUpdate;
  try
    MergeData.Children.Add(ATab);
    ATab.MergeData.MergedWith := Self;
    ATab.MergeData.VisibleBeforeMerging := ATab.Visible;
    ATab.Visible := False;

    for I := 0 to ATab.Groups.Count - 1 do
    begin
      AGroup := ATab.Groups[I];
      if AGroup.IsMergeAllowed and (AGroup.MergeKind = rmkMerge) then
      begin
        if FindTargetGroup(AGroup, ATargetGroup) then
          ATargetGroup.Merge(AGroup, AMergeOptions);
      end;
    end;

    if rmoCanCreateNewGroup in AMergeOptions then
    begin
      for I := 0 to ATab.Groups.Count - 1 do
      begin
        AGroup := ATab.Groups[I];
        if AGroup.IsMergeAllowed and not AGroup.IsMerged then
        begin
          ATargetGroup := Groups.Insert(GetInsertPosition(AGroup));
          ATargetGroup.Caption := AGroup.Caption;
          ATargetGroup.MergeKind := AGroup.MergeKind;
          ATargetGroup.MergeOrder := AGroup.MergeOrder;
          ATargetGroup.Merge(AGroup, AMergeOptions);
        end;
      end;
    end;
  finally
    Ribbon.EndUpdate;
  end;
end;

function TdxRibbonTab.InternalUnmerge(ATab: TdxRibbonTab): Boolean;
var
  I: Integer;
begin
  Result := (ATab <> nil) and (ATab.MergeData.MergedWith = Self);
  if Result then
  begin
    Groups.BeginUpdate;
    try
      ATab.MergeData.MergedWith := nil;
      ATab.Visible := ATab.MergeData.VisibleBeforeMerging;
      MergeData.Children.Remove(ATab);
      for I := ATab.Groups.Count - 1 downto 0 do
        Groups.Unmerge(ATab.Groups[I]);
    finally
      Groups.EndUpdate;
    end;
  end;
end;

procedure TdxRibbonTab.Unmerge(ATab: TdxRibbonTab = nil);
var
  I: Integer;
begin
  if ATab = Self then
    RaiseMergingError(@dxSBAR_RIBBONCANTUNMERGETAB);
  if (ATab <> nil) and (ATab.MergeData.MergedWith = nil) then
    Exit;
  if (ATab <> nil) and (ATab.MergeData.MergedWith <> Self) then
    RaiseMergingError(@dxSBAR_RIBBONTABSARENOTMERGED, Self, ATab);
  if MergeData.Children.Count = 0 then
    Exit;

  if ATab <> nil then
    InternalUnmerge(ATab)
  else
    for I := MergeData.Children.Count - 1 downto 0 do
      InternalUnmerge(TdxRibbonTab(MergeData.Children[I]));
end;

procedure TdxRibbonTab.UnmergeBeforeDestroy;
begin
  if MergeData.MergedWith <> nil then
    MergeData.MergedWith.Unmerge(Self);
  Unmerge;
end;

procedure TdxRibbonTab.SetName(const Value: TComponentName);
var
  AChangeText: Boolean;
begin
  AChangeText := not (csLoading in ComponentState) and (Name = Caption) and
    ((Owner = nil) or not (Owner is TControl) or not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if AChangeText then
    Caption := Value;
end;

procedure TdxRibbonTab.Activate;
begin
  MakeVisible;
  if Ribbon.ShowTabGroups then
  begin
    UpdateDockControl;
    CheckGroupToolbarsDockControl;
    DockControl.Visible := True;
  end
  else
    CheckGroupToolbarsDockControl;
end;

procedure TdxRibbonTab.ChangeAllGroupsVisibility(const AValue: Boolean);
var
  I: Integer;
begin
  for I := 0 to Groups.Count - 1 do
    if Groups[I].ToolBar <> nil then
      Groups[I].ToolBar.Visible := AValue;
end;

procedure TdxRibbonTab.CheckGroupToolbarsDockControl;
var
  I: Integer;
  AToolbar: TdxBar;
begin
  for I := 0 to Groups.Count - 1 do
  begin
    AToolbar := Groups[I].ToolBar;
    if (AToolBar <> nil) and (AToolBar.DockControl <> DockControl) then
      AToolBar.DockControl := DockControl;
  end;
end;

procedure TdxRibbonTab.Deactivate;
begin
  if not IsDestroying then
    DockControl.Visible := False;
end;

function TdxRibbonTab.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonTabAccessibilityHelper;
end;

function TdxRibbonTab.CanFade: Boolean;
begin
  Result := Ribbon.CanFade and (ViewInfo <> nil);
end;

function TdxRibbonTab.GetActive: Boolean;
begin
  Result := Ribbon.ActiveTab = Self;
end;

function TdxRibbonTab.GetFadingOptions: TdxFadingOptions;
begin
  Result := Ribbon.OptionsFading.Tabs;
end;

function TdxRibbonTab.GetFocused: Boolean;
begin
  Result := IAccessibilityHelper.IsSelected;
end;

function TdxRibbonTab.GetHighlighted: Boolean;
begin
  Result := Ribbon.HighlightedTab = Self;
end;

function TdxRibbonTab.GetIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := GetAccessibilityHelperClass.Create(Self);
  Result := FIAccessibilityHelper;
end;

function TdxRibbonTab.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxRibbonTab.GetViewInfo: TdxRibbonTabViewInfo;
begin
  Result := Ribbon.ViewInfo.TabsAreaViewInfo.TabsViewInfo.Find(Self);
  dxTestCheck(Result <> nil, 'TdxRibbonTab.GetViewInfo');
end;

function TdxRibbonTab.GetVisibleIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Ribbon.VisibleTabCount - 1 do
    if Ribbon.VisibleTabs[I] = Self then
    begin
      Result := I;
      break;
    end;
end;

procedure TdxRibbonTab.ReadContextIndex(Reader: TReader);
begin
  FContextIndex := Reader.ReadInteger;
end;

procedure TdxRibbonTab.ReadIndex(Reader: TReader);
begin
  FLoadedIndex := Reader.ReadInteger;
end;

procedure TdxRibbonTab.SetActive(Value: Boolean);
begin
  if Value then
    Ribbon.ActiveTab := Self;
end;

procedure TdxRibbonTab.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Ribbon.Changed;
  end;
end;

procedure TdxRibbonTab.SetContext(AValue: TdxRibbonContext);
var
  APrevContext: TdxRibbonContext;
begin
  if FContext <> AValue then
  begin
    APrevContext := Context;
    try
      FContext := AValue;
      UpdateContextIndex;
      if Active and not IsVisible then
        Ribbon.SetNextActiveTab(Self)
      else
      begin
        Ribbon.Changed;
        Ribbon.UpdateActiveTab;
      end;
    finally
      if not Ribbon.IsDestroying and (APrevContext <> nil) and APrevContext.FCreatedByMerging then
      begin
        if APrevContext.TabCount = 0 then
          APrevContext.Free;
      end;
    end;
  end;
end;

procedure TdxRibbonTab.SetHighlighted(Value: Boolean);
begin
  if Value then
    Ribbon.HighlightedTab := Self;
end;

procedure TdxRibbonTab.SetMergeOrder(const AValue: Integer);
begin
  FMergeOrder := Max(AValue, 0);
end;

procedure TdxRibbonTab.SetRibbon(Value: TdxCustomRibbon);
begin
  if FRibbon <> Value then
  begin
    FRibbon := Value;
    if FRibbon <> nil then
    begin
      FDesignSelectionHelper := nil;
      FDesignSelectionHelper := GetSelectableItem(TdxDesignSelectionHelper.Create(Ribbon, Self, Ribbon));
      FDockControl.BarManager := FRibbon.BarManager;
      FDockControl.Parent := FRibbon.GroupsDockControlSite;
    end;
  end;
end;

function TdxRibbonTab.GetDockControlBounds: TRect;
var
  ATabGroupsDockControlOffset: TRect;
begin
  if Ribbon.UseRightToLeftAlignment then
    ATabGroupsDockControlOffset := TdxRightToLeftLayoutConverter.ConvertOffsets(Ribbon.ViewInfo.GetTabGroupsDockControlOffset)
  else
    ATabGroupsDockControlOffset := Ribbon.ViewInfo.GetTabGroupsDockControlOffset;
  Result := cxRectContent(GetControlRect(Ribbon.GroupsDockControlSite), ATabGroupsDockControlOffset);
end;

function TdxRibbonTab.GetGroupClass: TdxRibbonTabGroupClass;
begin
  Result := TdxRibbonTabGroup;
end;

function TdxRibbonTab.IsMerged: Boolean;
var
  I: Integer;
begin
  Result := (MergeData.MergedWith <> nil) or (MergeData.Children.Count > 0);
  for I := 0 to Groups.Count - 1 do
    Result := Result or Groups[I].IsMerged;
end;

function TdxRibbonTab.IsVisible: Boolean;
begin
  Result := Visible and ((Context = nil) or Context.Visible);
end;

procedure TdxRibbonTab.RecalculateBars;
begin
  DockControl.RecalculateBars;
end;

procedure TdxRibbonTab.RestoreContext;
begin
  FContextIndex := FSaveContextIndex;
end;

procedure TdxRibbonTab.RestoreIndex;
begin
  if LoadedIndex >= 0 then
    Index := LoadedIndex;
end;

procedure TdxRibbonTab.SaveContext;
begin
  FSaveContextIndex := FContextIndex;
end;

procedure TdxRibbonTab.ScrollDockControlGroups(AScrollLeft, AOnTimer: Boolean);
begin
  DockControl.ViewInfo.ScrollGroups(AScrollLeft, Ribbon.GroupsDockControlSite.Width -
    cxMarginsWidth(Ribbon.ViewInfo.GetTabGroupsDockControlOffset));
end;

procedure TdxRibbonTab.UpdateBarManager(ABarManager: TdxBarManager);
var
  I: Integer;
begin
  FDockControl.BarManager := ABarManager;
  for I := 0 to Groups.Count - 1 do
    Groups[I].UpdateBarManager(ABarManager);
end;

procedure TdxRibbonTab.UpdateColorScheme;
begin
  DockControl.UpdateColorScheme;
end;

procedure TdxRibbonTab.UpdateContextIndex;
begin
  if Context <> nil then
    FContextIndex := Context.Index
  else
    FContextIndex := -1;
end;

procedure TdxRibbonTab.UpdateDockControl;
var
  AIsDockControlVisible: Boolean;
begin
  if not Ribbon.IsLocked then
  begin
    if dxBarGetParentPopupWindow(DockControl, True) = nil then
    begin
      AIsDockControlVisible := Visible and not Ribbon.Hidden and Ribbon.ShowTabGroups and Active;
      if AIsDockControlVisible then
      begin
        DockControl.ViewInfo.Calculate(GetDockControlBounds);
        UpdateDockControlBounds;
      end;
      DockControl.Visible := AIsDockControlVisible;
    end
    else
    begin
      DockControl.ViewInfo.Calculate(DockControl.ClientRect);
      DockControl.UpdateGroupPositions;
    end;
  end;
end;

procedure TdxRibbonTab.UpdateDockControlBounds;
begin
  if not Ribbon.IsLocked then
    DockControl.BoundsRect := GetDockControlBounds;
end;

procedure TdxRibbonTab.UpdateGroupsFont;
begin
  DockControl.UpdateBarControlsFont;
end;

function TdxRibbonTab.VisibleBeforeMerging: Boolean;
begin
  Result := IsMerged and MergeData.VisibleBeforeMerging;
end;

function TdxRibbonTab.GetIniSection(const ABaseSection, ADelimiter: string): string;
begin
  Result := ABaseSection + ADelimiter + 'Tab' + IntToStr(Index);
end;

procedure TdxRibbonTab.LoadFromIni(ASource: TCustomIniFile; const ABaseSection, ADelimiter: string);
var
  AContextCaption: string;
  AGroup: TdxRibbonTabGroup;
  AGroupCount: Integer;
  AGroupSection: string;
  ASection: string;
  AToolBar: TdxBar;
  I: Integer;
begin
  ASection := GetIniSection(ABaseSection, ADelimiter);
  Caption := ASource.ReadString(ASection, 'Caption', Caption);
  Name := ASource.ReadString(ASection, 'Name', Name);
  Visible := ASource.ReadBool(ASection, 'Visible', False);

  AContextCaption := ASource.ReadString(ASection, 'ContextCaption', '');
  if AContextCaption <> '' then
    Context := Ribbon.Contexts.Find(AContextCaption)
  else
    Context := nil;

  Groups.Unmerge;
  AGroupCount := ASource.ReadInteger(ASection, 'GroupCount', 0);
  for I := 0 to Groups.Count - 1 do
  begin
    AGroup := Groups[I];
    if AGroup.ToolBar <> nil then
      AGroup.ToolBar.Visible := (I < AGroupCount) or (AGroup.IsToolBarShared and AGroup.ToolBar.Visible);
  end;

  for I := 0 to AGroupCount - 1 do
  begin
    if I >= Groups.Count then
      Groups.Add;
    AGroupSection := Groups[I].GetIniSection(ASection, ADelimiter);
    AToolBar := Ribbon.BarManager.BarByComponentName(ASource.ReadString(AGroupSection, 'ToolBar', ''));
    if (AToolBar <> nil) and Groups.ContainsToolBar(AToolBar) then
      Groups.FindByToolBar(AToolBar).Index := I;
    Groups[I].LoadFromIni(ASource, ASection, ADelimiter);
  end;
end;

procedure TdxRibbonTab.SaveToIni(ADestination: TCustomIniFile; const ABaseSection, ADelimiter: string);
var
  ASection: string;
  I: Integer;
  AActualGroupCount: Integer;
begin
  ASection := GetIniSection(ABaseSection, ADelimiter);
  ADestination.WriteString(ASection, 'Name', Name);
  ADestination.WriteString(ASection, 'Caption', Caption);
  if Context <> nil then
    ADestination.WriteString(ASection, 'ContextCaption', Context.Caption)
  else
    ADestination.WriteString(ASection, 'ContextCaption', '');
  ADestination.WriteBool(ASection, 'Visible', Visible or VisibleBeforeMerging);
  AActualGroupCount := Groups.Count;
  for I := 0 to Groups.Count - 1 do
    if not Groups[I].CreatedByMerging then
      Groups[I].SaveToIni(ADestination, ASection, ADelimiter)
    else
      Dec(AActualGroupCount);
  ADestination.WriteInteger(ASection, 'GroupCount', AActualGroupCount);
end;

function TdxRibbonTab.GetAdornerTargetElementBounds: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TdxRibbonTab.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Ribbon;
end;

function TdxRibbonTab.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible and (ViewInfo <> nil) and not cxRectIsEmpty(ViewInfo.Bounds);
end;

procedure TdxRibbonTab.GetAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  inherited GetAdornerTargetElements(AList);
  for I := 0 to Groups.Count - 1 do
    AList.AddObject('Group' + IntToStr(I), Groups[I]);
end;

procedure TdxRibbonTab.WriteContextIndex(Writer: TWriter);
begin
  UpdateContextIndex;
  Writer.WriteInteger(FContextIndex);
end;

procedure TdxRibbonTab.WriteIndex(Writer: TWriter);
begin
  FLoadedIndex := Index;
  Writer.WriteInteger(FLoadedIndex);
end;

procedure TdxRibbonTab.SetGroups(const Value: TdxRibbonTabGroups);
begin
  FGroups.Assign(Value);
end;

procedure TdxRibbonTab.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Active and not Value then
      Ribbon.SetNextActiveTab(Self);
    Ribbon.Changed;
  end;
end;

procedure TdxRibbonTab.FadingDrawFadeImage;
begin
  if not IsDestroying then
    Invalidate;
end;

procedure TdxRibbonTab.FadingGetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
var
  AViewInfo: TdxRibbonTabViewInfo;
begin
  AViewInfo := ViewInfo;
  AFadeOutImage := AViewInfo.PrepareFadeImage(False);
  AFadeInImage := AViewInfo.PrepareFadeImage(True);
end;

{ TdxRibbonTabCollection }

constructor TdxRibbonTabCollection.Create(AOwner: TdxCustomRibbon);
begin
  inherited Create(AOwner, AOwner.GetTabClass);
  FOwner := AOwner;
end;

destructor TdxRibbonTabCollection.Destroy;
var
  I: Integer;
begin
  BarAccessibilityHelperOwnerObjectDestroyed(FIAccessibilityHelper);
  for I := Count - 1 downto 0 do
    Items[I].Free;
  inherited Destroy;
end;

function TdxRibbonTabCollection.Add: TdxRibbonTab;
begin
  Result := TdxRibbonTab(inherited Add);
end;

function TdxRibbonTabCollection.ContainsToolBar(AToolBar: TdxBar): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Groups.ContainsToolBar(AToolBar);
    if Result then Break;
  end;
end;

function TdxRibbonTabCollection.Insert(AIndex: Integer): TdxRibbonTab;
begin
  Result := TdxRibbonTab(inherited Insert(AIndex));
end;

function TdxRibbonTabCollection.IsMerged: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    Result := Result or Items[I].IsMerged;
end;

function TdxRibbonTabCollection.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonTabCollectionAccessibilityHelper;
end;

function TdxRibbonTabCollection.GetIniSection(const ABaseSection, ADelimiter: string; const ATabIndex: Integer): string;
begin
  Result := ABaseSection + ADelimiter + 'Tab' + IntToStr(ATabIndex);
end;

function TdxRibbonTabCollection.GetIniSection(const ATabName: string; ASource: TCustomIniFile): string;
var
  ABaseSection: string;
  I: Integer;
begin
  ABaseSection := Owner.GetIniSection('.', TdxBarManagerAccess(Owner.BarManager).GetBaseIniSection);
  for I := 0 to ASource.ReadInteger(ABaseSection, 'TabCount', 0) - 1 do
    if SameText(ATabName, ASource.ReadString(GetIniSection(ABaseSection, '.', I), 'Name', '')) then
      Break;
  Result := GetIniSection(ABaseSection, '.', I);
end;

function TdxRibbonTabCollection.Find(const ACaption: string): TdxRibbonTab;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Caption = ACaption then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxRibbonTabCollection.FindByComponentName(const AName: string): TdxRibbonTab;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if SameText(Items[I].Name, AName) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TdxRibbonTabCollection.FindByLoadedIndex(AIndex: Integer): TdxRibbonTab;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].LoadedIndex = AIndex then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TdxRibbonTabCollection.Notify(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  case AAction of
    ccnAdded:
      Owner.AddTab(TdxRibbonTab(AItem));
    ccnExtracted:
      Owner.RemoveTab(TdxRibbonTab(AItem));
  end;
  inherited;
end;

procedure TdxRibbonTabCollection.InternalUnmerge(ATab: TdxRibbonTab = nil);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      Items[I].InternalUnmerge(ATab);
  finally
    EndUpdate;
  end;
end;

procedure TdxRibbonTabCollection.Update(AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  inherited;
  if (AItem = nil) and not Owner.IsLocked then
    Owner.Changed;
end;

procedure TdxRibbonTabCollection.UpdateBarManager(ABarManager: TdxBarManager);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateBarManager(ABarManager);
end;

procedure TdxRibbonTabCollection.UpdateContexts;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Context := FOwner.Contexts.GetItemFromIndex(Items[I].FContextIndex);
end;

procedure TdxRibbonTabCollection.RemoveContext(AContext: TdxRibbonContext);
var
  I: Integer;
begin
  if AContext <> nil then
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].Context = AContext then
        Items[I].Context := nil;
    end;
    Changed;
  end;
end;

procedure TdxRibbonTabCollection.RestoreContexts;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RestoreContext;
  UpdateContexts;
end;

procedure TdxRibbonTabCollection.RestoreOrder;
var
  I: Integer;
  AItem: TdxRibbonTab;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := FindByLoadedIndex(I);
    if (AItem <> nil) and (AItem.LoadedIndex < Count) then
      AItem.RestoreIndex;
  end;
end;

procedure TdxRibbonTabCollection.SaveContexts;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].SaveContext;
end;

procedure TdxRibbonTabCollection.SetItemName(AItem: TcxComponentCollectionItem; ABaseIndex: Integer = -1);
begin
  AItem.Name := CreateUniqueName(TdxRibbonTab(AItem).Ribbon.Owner,
    TdxRibbonTab(AItem).Ribbon, AItem, 'TdxRibbon', '', ABaseIndex);
end;

function TdxRibbonTabCollection.GetIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := GetAccessibilityHelperClass.Create(Self);
  Result := FIAccessibilityHelper;
end;

function TdxRibbonTabCollection.GetItem(Index: Integer): TdxRibbonTab;
begin
  Result := TdxRibbonTab(inherited Items[Index]);
end;

procedure TdxRibbonTabCollection.SetItem(Index: Integer;
  const Value: TdxRibbonTab);
begin
  Items[Index].Assign(Value);
end;

{ TdxRibbonFonts }

constructor TdxRibbonFonts.Create(AOwner: TdxCustomRibbon);
var
  I: TdxRibbonAssignedFont;
begin
  inherited Create;
  FRibbon := AOwner;
  FDocumentNameColor := clDefault;
  FCaptionFont := TFont.Create;
  FInternalCaptionFont := TFont.Create;
  for I := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
  begin
    FFonts[I] := TFont.Create;
    FFonts[I].OnChange := FontChanged;
    FInternalFonts[I] := TFont.Create;
  end;
end;

destructor TdxRibbonFonts.Destroy;
var
  I: TdxRibbonAssignedFont;
begin
  for I := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
  begin
    FreeAndNil(FInternalFonts[I]);
    FreeAndNil(FFonts[I]);
  end;
  FreeAndNil(FInternalCaptionFont);
  FreeAndNil(FCaptionFont);
  inherited Destroy;
end;

procedure TdxRibbonFonts.Assign(Source: TPersistent);
var
  I: TdxRibbonAssignedFont;
begin
  if Source is TdxRibbonFonts then
  begin
    Ribbon.BeginUpdate;
    try
      FDocumentNameColor := TdxRibbonFonts(Source).DocumentNameColor;
      for I := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
        FFonts[I].Assign(TdxRibbonFonts(Source).FFonts[I]);
      FAssignedFonts := TdxRibbonFonts(Source).FAssignedFonts;
    finally
      Ribbon.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TdxRibbonFonts.GetApplicationButtonFont(AState: TdxRibbonApplicationButtonState): TFont;
begin
  Result := GetInternalFont(afApplicationButton);
  Ribbon.ColorScheme.AdjustApplicationButtonFont(Result, AState);
end;

function TdxRibbonFonts.GetContextFont(AContextColor: TColor = clDefault): TFont;
begin
  Result := GetInternalFont(afTabHeader);
  Ribbon.Painter.AdjustContextFont(Result, AContextColor);
end;

function TdxRibbonFonts.GetFormCaptionFont: TFont;
begin
  Result := FInternalCaptionFont;
  Result.Assign(FCaptionFont);
  Result.Size := Ribbon.ViewInfo.AdjustCaptionFontSize(Result.Size);
end;

function TdxRibbonFonts.GetFormCaptionFont(AIsActive: Boolean): TFont;
begin
  Result := GetFormCaptionFont;
  Result.Color := GetDefaultCaptionTextColor(AIsActive);
end;

function TdxRibbonFonts.GetGroupFont: TFont;
begin
  Result := GetInternalFont(afGroup);
end;

function TdxRibbonFonts.GetGroupHeaderFont: TFont;
begin
  Result := GetInternalFont(afGroupHeader);
  Result.Color := GetPartColor(rspTabGroupHeaderText);
end;

function TdxRibbonFonts.GetTabHeaderFont(AState: Integer; AContext: TdxRibbonContext): TFont;
begin
  Result := GetInternalFont(afTabHeader);
  if AContext <> nil then
    Ribbon.ColorScheme.AdjustContextTabFont(Result, AState, AContext.Color)
  else
    Ribbon.ColorScheme.AdjustTabFont(Result, AState);
end;

procedure TdxRibbonFonts.ChangeScale(M, D: Integer);
var
  AFontIndex: TdxRibbonAssignedFont;
begin
  for AFontIndex := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
  begin
    if AFontIndex in AssignedFonts then
      FFonts[AFontIndex].Height := MulDiv(FFonts[AFontIndex].Height, M, D);
  end;
  UpdateFonts;
end;

function TdxRibbonFonts.GetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  Result := Ribbon.SkinGetPartColor(APart, AState);
end;

procedure TdxRibbonFonts.Invalidate;
begin
  if Ribbon.Visible and (Ribbon.ActiveTab <> nil) then
    Ribbon.ActiveTab.UpdateColorScheme;
  Ribbon.RibbonFormInvalidate;
end;

procedure TdxRibbonFonts.UpdateFonts;
var
  AFont: TFont;
  AFontIndex: TdxRibbonAssignedFont;
  AMasterFont: TFont;
begin
  FCaptionFont.Handle := CreateFontIndirect(dxSystemInfo.NonClientMetrics.lfCaptionFont);
  FCaptionFont.Height := Ribbon.ScaleFactor.Apply(FCaptionFont.Height, dxSystemScaleFactor);

  if Ribbon.IsBarManagerValid then
    AMasterFont := Ribbon.BarManager.Font
  else
    AMasterFont := Ribbon.Font;

  FLocked := True;
  try
    for AFontIndex := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
      if not (AFontIndex in AssignedFonts) then
      begin
        AFont := FFonts[AFontIndex];
        AFont.Assign(AMasterFont);
        case AFontIndex of
          afGroup:
            AFont.Color := GetPartColor(rspTabGroupText);
          afGroupHeader:
            AFont.Color := GetPartColor(rspTabGroupHeaderText);
        end;
      end;
  finally
    FLocked := False;
  end;
end;

procedure TdxRibbonFonts.FontChanged(Sender: TObject);
var
  I: TdxRibbonAssignedFont;
begin
  if not (Locked or Ribbon.IsLoading) then
  begin
    Ribbon.BeginUpdate;
    try
      for I := Low(TdxRibbonAssignedFont) to High(TdxRibbonAssignedFont) do
        if Sender = FFonts[I] then
        begin
          Include(FAssignedFonts, TdxRibbonAssignedFont(I));
          Break;
        end;

      Ribbon.UpdateToolbarsFonts;
    finally
      Ribbon.EndUpdate;
      Invalidate;
    end;
  end;
end;

function TdxRibbonFonts.GetDefaultCaptionTextColor(AIsActive: Boolean): TColor;

   function IsFormZoomed(AForm: TCustomForm): Boolean;
   begin
     Result := (AForm <> nil) and AForm.HandleAllocated and IsZoomed(AForm.Handle);
   end;

begin
  if Ribbon.ViewInfo.UseGlass then
  begin
    if IsWinSevenOrLater then
      Result := clBlack
    else
      if IsFormZoomed(Ribbon.RibbonForm) then
        Result := clWindow
      else
        if AIsActive then
          Result := clCaptionText
        else
          Result := clInactiveCaptionText
  end
  else
    Result := GetPartColor(rspFormCaptionText, Ord(not AIsActive));
end;

function TdxRibbonFonts.GetFont(const Index: Integer): TFont;
begin
  Result := FFonts[TdxRibbonAssignedFont(Index)]
end;

function TdxRibbonFonts.GetInternalFont(AIndex: TdxRibbonAssignedFont): TFont;
begin
  Result := FInternalFonts[AIndex];
  Result.Assign(FFonts[AIndex]);
end;

function TdxRibbonFonts.IsFontStored(const Index: Integer): Boolean;
begin
  Result := TdxRibbonAssignedFont(Index) in FAssignedFonts;
end;

procedure TdxRibbonFonts.SetAssignedFonts(const Value: TdxRibbonAssignedFonts);
begin
  if (FAssignedFonts <> Value) then
  begin
    FAssignedFonts := Value;
    UpdateFonts;
    FontChanged(nil);
  end;
end;

procedure TdxRibbonFonts.SetDocumentNameColor(const Value: TColor);
begin
  if FDocumentNameColor <> Value then
  begin
    FDocumentNameColor := Value;
    Ribbon.RibbonFormInvalidate;
  end;
end;

procedure TdxRibbonFonts.SetFont(const Index: Integer; const Value: TFont);
begin
  FFonts[TdxRibbonAssignedFont(Index)].Assign(Value);
end;

{ TdxRibbonComponentHelper }

constructor TdxRibbonHolderComponent.Create(AOwner: TComponent);
begin
  inherited Create;
  CheckAssignRibbon(AOwner);
end;

procedure TdxRibbonHolderComponent.CheckAssignRibbon(AOwner: TComponent);
begin
  if csDesigning in AOwner.ComponentState then
    Ribbon := FindRibbonForComponent(AOwner);
end;

procedure TdxRibbonHolderComponent.SetRibbon(Value: TdxCustomRibbon);
begin
  Component := Value;
end;

function TdxRibbonHolderComponent.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxCustomRibbon(Component);
end;

{ TdxRibbonCustomPopupComponent }

constructor TdxRibbonCustomPopupComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRibbonHolder := TdxRibbonHolderComponent.Create(Self);
end;

destructor TdxRibbonCustomPopupComponent.Destroy;
begin
  FreeAndNil(FRibbonHolder);
  inherited Destroy;
end;

function TdxRibbonCustomPopupComponent.GetRibbon: TdxCustomRibbon;
begin
  Result := FRibbonHolder.Ribbon;
end;

procedure TdxRibbonCustomPopupComponent.SetRibbon(const Value: TdxCustomRibbon);
begin
  FRibbonHolder.Ribbon := Value;
end;

{ TdxRibbonCustomPopupMenu }

constructor TdxRibbonCustomPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FRibbonHolder := TdxRibbonHolderComponent.Create(Self);
end;

destructor TdxRibbonCustomPopupMenu.Destroy;
begin
  FreeAndNil(FRibbonHolder);
  inherited;
end;

function TdxRibbonCustomPopupMenu.CreateBarControl: TCustomdxBarControl;
begin
  Result := inherited CreateBarControl;

  if Ribbon <> nil then
    TdxRibbonPopupMenuControl(Result).FPainter := Ribbon.GroupsPainter;
end;

function TdxRibbonCustomPopupMenu.GetControlClass: TCustomdxBarControlClass;
begin
  Result := TdxRibbonPopupMenuControl;
end;

procedure TdxRibbonCustomPopupMenu.SetRibbon(Value: TdxCustomRibbon);
begin
  FRibbonHolder.Ribbon := Value;
end;

function TdxRibbonCustomPopupMenu.GetRibbon: TdxCustomRibbon;
begin
  Result := FRibbonHolder.Ribbon;
end;

{ TdxRibbonPopupMenuControl }

function TdxRibbonPopupMenuControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := dxRibbonBarBehaviorOptions +
    [bboAllowSelectWindowItemsWithoutFocusing, bboExtendItemWhenAlignedToClient] -
    [bboMouseCantUnselectNavigationItem, bboSubMenuCaptureMouse];
end;

{ TdxBarApplicationMenu }

function TdxBarApplicationMenu.ApplicationMenuPopup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean;
var
  AOwnerBounds: TRect;
  AOwnerOffset: Integer;
  P: TPoint;
begin
  Result := True;
  FRibbonPainter := ARibbon.GroupsPainter;
  try
    P.Y := ARibbon.ViewInfo.TabsAreaViewInfo.TabsViewInfo.Bounds.Top;
    if ARibbon.UseRightToLeftAlignment then
      P.X := ARibbon.ApplicationButtonViewInfo.Bounds.Right
    else
      P.X := ARibbon.ApplicationButtonViewInfo.Bounds.Left;
    AOwnerOffset := P.Y - ARibbon.ApplicationButtonViewInfo.ClientBounds.Top;
    AOwnerBounds := ARibbon.ApplicationButtonViewInfo.ClientBounds;
    P := ARibbon.ClientToScreen(P);
    PopupEx(P.X, P.Y, cxRectWidth(AOwnerBounds), AOwnerOffset, False, @AOwnerBounds, True, ARibbon);
    AClosedByEscape := FClosedByEscape;
  finally
    FRibbonPainter := nil;
  end;
end;

function TdxBarApplicationMenu.ClosePopup: Boolean;
begin
  Result := Visible;
  if Result then
    SubMenuControl.HideAll;
end;

procedure TdxBarApplicationMenu.DoCloseUp;
begin
  inherited DoCloseUp;
  FClosedByEscape := TdxBarSubMenuControlAccess(ItemLinks.BarControl).CloseUpReason = bcrEscape;
end;

procedure TdxBarApplicationMenu.DoPopup;
begin
  if FRibbonPainter <> nil then
    TCustomdxBarControlAccess(SubMenuControl).FPainter := FRibbonPainter;
  inherited DoPopup;
end;

function TdxBarApplicationMenu.GetControlClass: TCustomdxBarControlClass;
begin
  Result := TdxRibbonApplicationMenuControl;
end;

function TdxBarApplicationMenu.CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
begin
  Result := True;
end;

function TdxBarApplicationMenu.GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
begin
  Result := amdmPopup;
end;

procedure TdxBarApplicationMenu.Popup(X, Y: Integer);
begin
  if FRibbonPainter = nil then
    raise EdxException.Create(cxGetResourceString(@dxSBAR_APPMENUOUTSIDERIBBON));
  inherited Popup(X, Y);
end;

procedure TdxBarApplicationMenu.UpdateNonClientArea;
begin
  // do nothing
end;

function TdxBarApplicationMenu.GetOrigin(AIsClientArea: Boolean): TPoint;
begin
  if AIsClientArea then
    Result := SubMenuControl.ClientOrigin
  else
    Result := cxGetWindowRect(SubMenuControl).TopLeft;
end;

procedure TdxBarApplicationMenu.GetTabOrderList(List: TList);
begin
  // do nothing
end;

function TdxBarApplicationMenu.GetRootAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  Result := nil;
end;

function TdxBarApplicationMenu.IsVisible: Boolean;
begin
  Result := Visible;
end;

procedure TdxBarApplicationMenu.RibbonFormResized;
begin
  // do nothing
end;

procedure TdxBarApplicationMenu.SelectAppMenuFirstItemControl;
begin
  SelectFirstSelectableAccessibleObject(ItemLinks.BarControl.IAccessibilityHelper.GetBarHelper);
end;

procedure TdxBarApplicationMenu.ShowKeyTips;
begin
  BarNavigationController.SetKeyTipsShowingState(ItemLinks.BarControl.IAccessibilityHelper, '');
end;

{ TdxRibbonApplicationMenuControl }

function TdxRibbonApplicationMenuControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := inherited GetBehaviorOptions + [bboItemCustomizePopup];
end;

procedure TdxRibbonApplicationMenuControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
begin
  if Ribbon <> nil then
    Ribbon.PopulatePopupMenuItems(AItemLinks, GetPopupMenuItems, PopupMenuClick);
end;

function TdxRibbonApplicationMenuControl.GetPopupMenuItems: TdxRibbonPopupMenuItems;
begin
  Result := Ribbon.GetValidPopupMenuItems;
  if ExtraPaneItemLinks.IndexOf(BarDesignController.CustomizingItemLink) <> -1 then
    Exclude(Result, rpmiQATAddRemoveItem);
end;

procedure TdxRibbonApplicationMenuControl.PopupMenuClick(Sender: TObject); // see TdxRibbonCustomBarControl
var
  ALinkSelf: TcxObjectLink;
begin
  ALinkSelf := cxAddObjectLink(Self);
  try
    DoPopupMenuClick(Sender);
    if ALinkSelf.Ref <> nil then
      HideAll;
  finally
    cxRemoveObjectLink(ALinkSelf);
  end;
end;

function TdxRibbonApplicationMenuControl.GetRibbon: TdxCustomRibbon;
begin
  if OwnerControl is TdxCustomRibbon then
    Result := TdxCustomRibbon(OwnerControl)
  else
    Result := nil;
end;

procedure TdxRibbonApplicationMenuControl.DoPopupMenuClick(Sender: TObject);
begin
  Ribbon.PopupMenuItemClick(Sender);
end;

procedure TdxRibbonApplicationMenuControl.WMNCHitTest(var Message: TWMNCHitTest);
var
  ARect: TRect;
begin
  if (Ribbon <> nil) and Ribbon.HandleAllocated then
  begin
    ARect := dxMapWindowRect(Ribbon.Handle, 0, Ribbon.ApplicationButtonViewInfo.Bounds);
    if cxRectPtIn(ARect, SmallPointToPoint(Message.Pos)) then
      Message.Result := HTTRANSPARENT
    else
      inherited;
  end
  else
    inherited;
end;

{ TdxRibbonController }

constructor TdxRibbonController.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FHitInfo := TdxRibbonHitInfo.Create(Ribbon);
  FHintInfo := TdxRibbonHitInfo.Create(Ribbon);
  CreateTimer;
end;

destructor TdxRibbonController.Destroy;
begin
  FreeAndNil(FScrollTimer);
  FreeAndNil(FHitInfo);
  FreeAndNil(FHintInfo);
  inherited Destroy;
end;

function TdxRibbonController.NextTab(ATab: TdxRibbonTab): TdxRibbonTab;
begin
  Result := NextTabCore(ATab, 1);
end;

function TdxRibbonController.PrevTab(ATab: TdxRibbonTab): TdxRibbonTab;
begin
  Result := NextTabCore(ATab, -1);
end;

procedure TdxRibbonController.DoScroll(AOnTimer: Boolean);
begin
  CancelHint;
  case FScrollKind of
    rhtTabScrollLeft, rhtTabScrollRight:
      ScrollTabs(FScrollKind = rhtTabScrollRight, AOnTimer);
    rhtGroupScrollLeft, rhtGroupScrollRight:
      ScrollGroups(FScrollKind = rhtGroupScrollRight, AOnTimer);
  end;
end;

procedure TdxRibbonController.InitTabDesignMenu(AItemLinks: TdxBarItemLinks);
begin
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDTAB), DesignTabMenuClick, 0);
  if BarDesignController.LastSelectedItem <> nil then
    BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
      cxGetResourceString(@dxSBAR_RIBBONDELETETAB), DesignTabMenuClick, 1);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDEMPTYGROUP), DesignTabMenuClick, 2, True);
  BarDesignController.AddInternalItem(AItemLinks, TdxBarButton,
    cxGetResourceString(@dxSBAR_RIBBONADDGROUPWITHTOOLBAR), DesignTabMenuClick, 3);
end;

function TdxRibbonController.IsApplicationMenuDropped: Boolean;
begin
  Result := risAppMenuActive in Ribbon.InternalState;
end;

function TdxRibbonController.IsNeedShowHint(AObject: TdxRibbonHitTest): Boolean;
begin
  Result := IsOwnerForHintObject(AObject);
  if Result then
  begin
    case AObject of
      rhtTab:
        Result := (HintInfo.HitObjectAsTab <> nil) and ViewInfo.TabsAreaViewInfo.TabsViewInfo.NeedShowHint;
      rhtApplicationMenu:
        Result := (ApplicationButton.ScreenTip <> nil) and not IsApplicationMenuDropped;
      rhtHelpButton:
        Result := Ribbon.HelpButton.ScreenTip <> nil;
      rhtContext:
        Result := ViewInfo.ContextsViewInfo.NeedShowHint(HintInfo.HitObjectAsContext);
      rhtCustomButton:
        Result := HintInfo.HitObjectAsButton.Enabled and (HintInfo.HitObjectAsButton.Hint <> '');
    end;
  end;
end;

function TdxRibbonController.IsOwnerForHintObject(AObject: TdxRibbonHitTest): Boolean;
begin
  Result := AObject in [rhtTab, rhtApplicationMenu, rhtHelpButton, rhtCustomButton, rhtContext];
end;

procedure TdxRibbonController.HideHint;
begin
  if Ribbon.IsBarManagerValid then
    Ribbon.BarManager.HideHint;
end;

procedure TdxRibbonController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HideHint;
end;

procedure TdxRibbonController.KeyPress(var Key: Char);
begin
end;

procedure TdxRibbonController.KeyUp(var Key: Word; Shift: TShiftState);
begin
end;

procedure TdxRibbonController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HideHint;
  if IsApplicationMenuDropped then
  begin
    if ApplicationMenuState = ramsShownAsMenu then
      ApplicationButton.ClosePopup;
  end;

  HitInfo.Calculate(Point(X, Y));
  case HitInfo.HitTest of
    rhtTab:
      ProcessTabOnMouseDown(Button, Shift);
    rhtApplicationMenu:
      if ProcessApplicationButtonMouseDown(Button, Shift) then
        Exit;
    rhtTabScrollLeft..rhtGroupScrollRight:
      if Button = mbLeft then
        StartScroll(HitInfo.HitTest);
  else
    if Button = mbRight then
    begin
      if cxRectPtIn(ViewInfo.TabsAreaViewInfo.TabsViewInfo.GetRealBounds, X, Y) or
        (Ribbon.IsQuickAccessToolbarValid and ViewInfo.QuickAccessToolbarViewInfo.AtBottom and
        cxRectPtIn(ViewInfo.QuickAccessToolbarViewInfo.Bounds, X, Y))
      then
        Ribbon.ShowCustomizePopup;
    end;
  end;
  if Button = mbLeft then
  begin
    PressedButton := HitInfo.HitObjectAsButton;
    PressedObject := HitInfo.HitTest;
  end;
  ProcessContextsOnMouseDown(ssDouble in Shift);

  if Ribbon.TabGroupsPopupWindow <> nil then
    Ribbon.TabGroupsPopupWindow.JustClosed;
end;

procedure TdxRibbonController.MouseLeave;
begin
  if IsOwnerForHintObject(HintInfo.HitTest) then
    CancelHint;
  ApplicationButtonViewInfo.IsHot := False;
  Ribbon.HighlightedTab := nil;
  HotButton := nil;
  HotObject := rhtNone;
end;

procedure TdxRibbonController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  HitInfo.Calculate(cxPoint(X, Y));
  ApplicationButtonViewInfo.IsHot := HitInfo.HitTest = rhtApplicationMenu;
  if not NotHandleMouseMove(cxPoint(X, Y)) then
  begin
    HotButton := HitInfo.HitObjectAsButton;
    Ribbon.HighlightedTab := HitInfo.HitObjectAsTab;
    HotObject := HitInfo.HitTest;
    HintInfo := HitInfo;
  end;
end;

procedure TdxRibbonController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CancelScroll;
  HitInfo.Calculate(cxPoint(X, Y));
  if Button = mbLeft then
  begin
    ReleaseCapture;
    if HitInfo.HitObject = PressedButton then
    begin
      if (PressedButton <> nil) and PressedButton.Enabled then
        PressedButton.Click;
    end;
    PressedButton := nil;
    PressedObject := rhtNone;
  end;
  if (FPressedContext <> nil) and (HitInfo.HitObject = FPressedContext) then
  begin
    FPressedContext.Activate;
    FPressedContext := nil;
  end;
end;

function TdxRibbonController.MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  ATab: TdxRibbonTab;
begin
  Result := Ribbon.CanScrollTabs;
  if Result then
  begin
    ATab := NextTab(Ribbon.ActiveTab);
    if ATab <> nil then
      Ribbon.ActiveTab := ATab;
  end;
end;

function TdxRibbonController.MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  ATab: TdxRibbonTab;
begin
  Result := Ribbon.CanScrollTabs;
  if Result then
  begin
    ATab := PrevTab(Ribbon.ActiveTab);
    if ATab <> nil then
      Ribbon.ActiveTab := ATab;
  end;
end;

function TdxRibbonController.NextTabCore(ATab: TdxRibbonTab; ADirection: Integer): TdxRibbonTab;
var
  AIndex: Integer;
  ATabs: TdxRibbonTabsViewInfo;
begin
  Result := nil;
  ATabs := ViewInfo.TabsAreaViewInfo.TabsViewInfo;
  if ATab = nil then
  begin
    if ATabs.Count > 0 then
      Result := ATabs[IfThen(ADirection < 0, ATabs.Count - 1)].Tab;
  end
  else
  begin
    AIndex := ATabs.IndexOf(ATab);
    if AIndex >= 0 then
    begin
      if InRange(AIndex + ADirection, 0, ATabs.Count - 1) then
        Result := ATabs.Items[AIndex + ADirection].Tab;
    end;
  end;
end;

function TdxRibbonController.DoHint(var ANeedDeactivate: Boolean; out AHintText: string; out AShortCut: string): Boolean;
begin
  ANeedDeactivate := False;
  Result := IsNeedShowHint(HintInfo.HitTest);
  AHintText := '';
  AShortCut := '';
  if Result then
  begin
    case HintInfo.HitTest of
      rhtTab:
        AHintText := HintInfo.HitObjectAsTab.DisplayCaption;
      rhtApplicationMenu:
        AHintText := ApplicationButton.ScreenTip.Header.Text;
      rhtHelpButton:
        AHintText := Ribbon.HelpButton.ScreenTip.Header.Text;
      rhtCustomButton:
        AHintText := HintInfo.HitObjectAsButton.Hint;
      rhtContext:
        AHintText := HintInfo.HitObjectAsContext.Caption;
    end;
  end;
end;

function TdxRibbonController.CreateHintViewInfo(const AHintText, AShortCut: string): TdxBarCustomHintViewInfo;
var
  ABarManager: TdxBarManager;
  AScreenTip: TdxScreenTip;
begin
  if Ribbon.IsBarManagerValid then
    ABarManager := Ribbon.BarManager
  else
    ABarManager := nil;

  case HintInfo.HitTest of
    rhtApplicationMenu:
      AScreenTip := ApplicationButton.ScreenTip;
    rhtHelpButton:
      AScreenTip := Ribbon.HelpButton.ScreenTip;
    else
      AScreenTip := nil;
  end;
  Result := dxBarCreateScreenTipViewInfo(ABarManager, AHintText, AShortCut, AScreenTip, Ribbon.GroupsPainter);
end;

function TdxRibbonController.GetEnabled: Boolean;
begin
  Result := True;
end;

function TdxRibbonController.GetHintPosition(const ACursorPos: TPoint; AHeight: Integer): TPoint;
var
  R: TRect;
  ALeftPos: Integer;
begin
  if HintInfo.HitTest = rhtApplicationMenu then
  begin
    R := ApplicationButtonViewInfo.Bounds;
    if Ribbon.UseRightToLeftAlignment then
      ALeftPos := R.Right
    else
      ALeftPos := R.Left;
    Result := cxPoint(ALeftPos, R.Bottom);
    Result := Ribbon.ClientToScreen(Result);
    if GetDesktopWorkArea(Result).Bottom - Result.Y < AHeight then
    begin
      Result := Ribbon.ClientToScreen(cxPoint(ALeftPos, 0));
      Dec(Result.Y, AHeight);
    end;
  end
  else
    Result := cxPoint(ACursorPos.X, ACursorPos.Y + Ribbon.ScaleFactor.Apply(dxRibbonHintOffset));
end;

procedure TdxRibbonController.CancelHint;
begin
  HintInfo.Reset;
  HideHint;
end;

procedure TdxRibbonController.CancelMode;
begin
  Ribbon.HighlightedTab := nil;
  CancelScroll;
  CancelHint;
end;

function TdxRibbonController.CanSwitchMinimizedOnDblClick: Boolean;
begin
  Result := not Ribbon.IsDesigning and Ribbon.MinimizeOnTabDblClick and
    (not Ribbon.ShowTabGroups or (not FSkipDblClick and HitInfo.HitObjectAsTab.Active));
end;

function TdxRibbonController.CloseApplicationMenu: Boolean;
begin
  Result := IsApplicationMenuDropped and ApplicationButton.ClosePopup;
end;

procedure TdxRibbonController.DesignTabMenuClick(Sender: TObject);
begin
  case TdxBarButton(Sender).Tag of
    0: Ribbon.Tabs.Add;
    1: BarDesignController.DeleteSelectedObjects(True, True);
    2: Ribbon.DesignAddTabGroup(nil, False);
    3: Ribbon.DesignAddTabGroup(nil, True);
  end;
  Ribbon.Modified;
end;

procedure TdxRibbonController.ScrollGroups(AScrollLeft, AOnTimer: Boolean);
begin
  Ribbon.ActiveTab.ScrollDockControlGroups(AScrollLeft, AOnTimer);
end;

procedure TdxRibbonController.ScrollTabs(AScrollLeft, AOnTimer: Boolean);
const
  ScrollDelta: array[Boolean, Boolean] of Integer = ((-dxRibbonTabMinWidth div 2, dxRibbonTabMinWidth div 2), (-3, 3));
begin
  with ViewInfo.TabsAreaViewInfo.TabsViewInfo do
    ScrollPosition := ScrollPosition + ScaleFactor.Apply(ScrollDelta[AOnTimer, AScrollLeft]);
end;

procedure TdxRibbonController.SetHintInfo(const Value: TdxRibbonHitInfo);
var
  ANeedHide: Boolean;
begin
  if Ribbon.IsLocked or not Ribbon.IsBarManagerValid then
    Exit;

  if not HintInfo.Compare(Value) then
  begin
    ANeedHide := IsOwnerForHintObject(HintInfo.HitTest);
    HintInfo.HitObject := Value.HitObject;
    HintInfo.HitTest := Value.HitTest;
    if IsOwnerForHintObject(HintInfo.HitTest) then
      Ribbon.BarManager.ActivateHint(True, '', Self)
    else
      if ANeedHide then
        HideHint;
  end;
end;

procedure TdxRibbonController.SetHotButton(AValue: TdxRibbonCustomButtonViewInfo);
begin
  if FHotButton <> AValue then
  begin
    FHotButton := AValue;
    UpdateButtonsStates;
  end;
end;

procedure TdxRibbonController.SetPressedButton(AValue: TdxRibbonCustomButtonViewInfo);
begin
  if FPressedButton <> AValue then
  begin
    FPressedButton := AValue;
    UpdateButtonsStates;
  end;
end;

procedure TdxRibbonController.SetPressedObject(const Value: TdxRibbonHitTest);
begin
  if FPressedObject <> Value then
  begin
    FPressedObject := Value;
    if not (FPressedObject in [rhtNone, rhtTab, rhtApplicationMenu]) then
      SetCapture(Ribbon.Handle);
  end;
end;

procedure TdxRibbonController.ShowTabDesignMenu;
begin
  BarDesignController.ShowCustomCustomizePopup(Ribbon.BarManager, InitTabDesignMenu, Ribbon.GroupsPainter);
end;

function TdxRibbonController.NotHandleMouseMove(P: TPoint): Boolean;

  function IsOwnedFormActive: Boolean;
  begin
    Result := IsFormActive(Ribbon.BarManager.ParentForm) or IsFormActive(Ribbon.TabGroupsPopupWindow) or
      (Ribbon.RibbonForm <> nil) and Ribbon.RibbonForm.IsActive;
  end;

begin
  if Ribbon.IsBarManagerValid and not Ribbon.IsDesigning then
    Result := (FScrollKind <> rhtNone) or not IsOwnedFormActive or dxBarHasPopupWindowAbove(nil, False)
  else
    Result := True;
end;

function TdxRibbonController.ProcessApplicationButtonMouseDown(
  Button: TMouseButton; AShift: TShiftState): Boolean;
var
  ARibbonParentForm: TCustomForm;
begin
  Result := False;
  if Button = mbLeft then
  begin
    if ssDouble in AShift then
    begin
      if Ribbon.Style = rs2007 then
      begin
        ARibbonParentForm := GetParentForm(Ribbon);
        if ARibbonParentForm <> nil then
          ARibbonParentForm.Close;
      end;
    end
    else
      if not CloseApplicationMenu then
      begin
        if CloseActiveRibbonApplicationMenus then
        begin
          PostMessage(Ribbon.Handle, DXM_RIBBON_SHOWAPPLICATIONMENU, 0, 0);
          Result := True;
        end
        else
          Result := Ribbon.ApplicationMenuPopup;
      end;
  end;
end;

procedure TdxRibbonController.ProcessContextsOnMouseDown(AIsDoubleClick: Boolean);
var
  AForm: TForm;
begin
  if HitInfo.HitTest = rhtContext then
  begin
    FPressedContext := HitInfo.HitObjectAsContext;
    CloseApplicationMenu;
    if AIsDoubleClick then
    begin
      FPressedContext := nil;
      AForm := Ribbon.RibbonForm;
      if AForm.WindowState = wsNormal then
        AForm.WindowState := wsMaximized
      else if AForm.WindowState = wsMaximized then
        AForm.WindowState := wsNormal;
    end
    else
      if not Ribbon.ShowTabGroups and (Ribbon.TabGroupsPopupWindow = nil) then
        Ribbon.ShowTabGroups := True;
  end
  else
    FPressedContext := nil;
end;

procedure TdxRibbonController.ProcessTabClick(ATab: TdxRibbonTab; Button: TMouseButton; Shift: TShiftState);
begin
  if CanProcessDesignTime then
  begin
    Ribbon.ActiveTab := ATab;
    BarDesignController.SelectItem(ATab);
    if Button = mbRight then
      ShowTabDesignMenu;
  end;

  if Ribbon.IsDesigning then
    Exit;

  case Button of
    mbRight:
      Ribbon.ShowCustomizePopup;
    mbLeft:
      if (ssDouble in Shift) or Ribbon.ShowTabGroups then
        Ribbon.ActiveTab := ATab
      else
        if (Ribbon.ActiveTab <> ATab) or (Ribbon.TabGroupsPopupWindow = nil) or not Ribbon.TabGroupsPopupWindow.JustClosed then
        begin
          Ribbon.ActiveTab := ATab;
          if not CloseApplicationMenu then
            Ribbon.ShowTabGroupsPopupWindow;
        end;
  end;
end;

procedure TdxRibbonController.ProcessTabOnMouseDown(AButton: TMouseButton; AShift: TShiftState);
var
  APrevActiveTab: TdxRibbonTab;
begin
  if not IsApplicationMenuDropped or CloseApplicationMenu then
  begin
    APrevActiveTab := Ribbon.ActiveTab;
    if (AButton = mbLeft) and (ssDouble in AShift) and CanSwitchMinimizedOnDblClick then
      Ribbon.ShowTabGroups := not Ribbon.ShowTabGroups
    else
      ProcessTabClick(HitInfo.HitObjectAsTab, AButton, AShift);

    FSkipDblClick := APrevActiveTab <> Ribbon.ActiveTab;
  end;
end;

procedure TdxRibbonController.CancelScroll;
begin
  FScrollKind := rhtNone;
  FScrollTimer.Enabled := False;
  UpdateButtonsStates;
end;

function TdxRibbonController.CanProcessDesignTime: Boolean;
begin
  Result := Ribbon.IsDesigning and Ribbon.IsBarManagerValid;
end;

procedure TdxRibbonController.CreateTimer;
begin
  FScrollTimer := TcxTimer.Create(nil);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := ScrollTimerHandler;
end;

function TdxRibbonController.GetViewInfo: TdxRibbonViewInfo;
begin
  Result := Ribbon.ViewInfo;
end;

function TdxRibbonController.GetApplicationButton: TdxRibbonApplicationButton;
begin
  Result := Ribbon.ApplicationButton;
end;

function TdxRibbonController.GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
begin
  Result := ViewInfo.ApplicationButtonViewInfo;
end;

function TdxRibbonController.GetApplicationMenuState: TdxRibbonApplicationMenuState;
const
  StateMap: array[TdxRibbonApplicationMenuDisplayMode] of TdxRibbonApplicationMenuState = (
    ramsShownAsMenu, ramsShownAsFrame, ramsShownAsFullScreenFrame
  );
begin
  if IsApplicationMenuDropped then
    Result := StateMap[ApplicationButton.IMenu.GetDisplayMode]
  else
    Result := ramsHidden;
end;

procedure TdxRibbonController.StartScroll(AScrollKind: TdxRibbonHitTest);
begin
  if AScrollKind in [rhtTabScrollLeft..rhtGroupScrollRight] then
  begin
    FScrollKind := AScrollKind;
    FScrollTimer.Interval := dxRibbonScrollDelay;
    DoScroll(False);
    FScrollTimer.Enabled := True;
  end;
end;

procedure TdxRibbonController.UpdateButtonsStates;
begin
  ViewInfo.UpdateButtonsStates;
end;

procedure TdxRibbonController.ScrollTimerHandler(Sender: TObject);
begin
  FScrollTimer.Interval := dxRibbonScrollInterval;
  HitInfo.Calculate(Ribbon.ScreenToClient(GetMouseCursorPos));
  if HitInfo.HitTest = FScrollKind then
    DoScroll(True);
end;

{ TdxRibbonGroupsDockControlScrollButtonViewInfo }

procedure TdxRibbonGroupsDockControlScrollButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  inherited Draw(ACanvas);
  ACanvas.ExcludeClipRect(Bounds);
end;

procedure TdxRibbonGroupsDockControlScrollButtonViewInfo.DrawButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawGroupsScrollButton(ACanvas, R, (FScrollButton = rsbLeft) xor Ribbon.UseRightToLeftAlignment, AState);
end;

procedure TdxRibbonGroupsDockControlScrollButtonViewInfo.DrawButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawGroupsScrollButtonArrow(ACanvas, R, (FScrollButton = rsbLeft) xor Ribbon.UseRightToLeftAlignment);
end;

function TdxRibbonGroupsDockControlScrollButtonViewInfo.GetHitInfo(const AHitInfo: TdxRibbonHitInfo): Boolean;
const
  HitTestMap: array[TdxRibbonScrollButton] of TdxRibbonHitTest = (rhtGroupScrollLeft, rhtGroupScrollRight);
begin
  Result := inherited GetHitInfo(AHitInfo);
  if Result then
    AHitInfo.HitTest := HitTestMap[FScrollButton];
end;

{ TdxRibbonGroupsDockControlSiteViewInfo }

constructor TdxRibbonGroupsDockControlSiteViewInfo.Create(AOwner: TdxRibbonViewInfo);

  function AddButton(AScrollButton: TdxRibbonScrollButton): TdxRibbonGroupsDockControlScrollButtonViewInfo;
  begin
    Result := TdxRibbonGroupsDockControlScrollButtonViewInfo.Create(Self);
    Result.FScrollButton := AScrollButton;
    AddItem(Result);
  end;

begin
  FSite := AOwner.Ribbon.GroupsDockControlSite;
  inherited Create(AOwner);
  FTabGroupsScrollButtonLeft := AddButton(rsbLeft);
  FTabGroupsScrollButtonRight := AddButton(rsbRight);
  FMinimizeRibbonButton := TdxRibbonMinimizeButtonViewInfo.Create(Self);
  AddItem(FMinimizeRibbonButton);
end;

procedure TdxRibbonGroupsDockControlSiteViewInfo.Calculate(const ABounds: TRect);
begin
  if RibbonViewInfo.IsTabGroupsVisible and (RibbonActiveTab <> nil) then
    FTabGroupsScrollButtons := RibbonActiveTab.DockControl.ViewInfo.ScrollButtons
  else
    FTabGroupsScrollButtons := [];

  inherited Calculate(ABounds);
end;

procedure TdxRibbonGroupsDockControlSiteViewInfo.CalculateButtonsBounds(const ABounds: TRect);

  function CalculateMinimizeRibbonButtonBounds: TRect;
  var
    ASize: TSize;
  begin
    ASize := GetMinimizeRibbonButtonSize;
    Result := cxRectSetRight(ABounds, ABounds.Right, ASize.cx);
    Result := cxRectSetBottom(Result, Result.Bottom, ASize.cy);
    Result := cxRectOffset(Result, RibbonViewInfo.GetTabGroupsContentOffset.BottomRight, False);
  end;

  procedure CalculateScrollButtons(AButtonWidth: Integer);
  var
    ARightSide: Integer;
  begin
    if cxRectIsEmpty(FMinimizeRibbonButton.Bounds) then
      ARightSide := ABounds.Right
    else
      ARightSide := FMinimizeRibbonButton.Bounds.Left;

    TabGroupsScrollButtonLeft.Bounds := cxRectSetWidth(
      ABounds, IfThen(rsbLeft in TabGroupsScrollButtons, AButtonWidth));
    TabGroupsScrollButtonRight.Bounds := cxRectSetRight(
      ABounds, ARightSide, IfThen(rsbRight in TabGroupsScrollButtons, AButtonWidth));
  end;

begin
  FMinimizeRibbonButton.Bounds := CalculateMinimizeRibbonButtonBounds;
  CalculateScrollButtons(RibbonViewInfo.ScrollButtonWidth);
end;

procedure TdxRibbonGroupsDockControlSiteViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Ribbon.Painter.DrawGroupsArea(ACanvas, Bounds);
  DrawRibbonParts(ACanvas);
  inherited Draw(ACanvas);
end;

procedure TdxRibbonGroupsDockControlSiteViewInfo.DrawRibbonParts(ACanvas: TcxCanvas);
var
  ASavedOrg: TPoint;
begin
  ASavedOrg := ACanvas.WindowOrg;
  try
    ACanvas.WindowOrg := dxMapWindowPoint(FSite.Handle, Ribbon.Handle, ASavedOrg);
    RibbonViewInfo.ApplicationButtonViewInfo.Draw(ACanvas);
    ACanvas.ExcludeClipRect(RibbonViewInfo.ApplicationButtonViewInfo.Bounds);

    if (RibbonActiveTab <> nil) and (RibbonActiveTab.ViewInfo <> nil) then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(RibbonViewInfo.TabsAreaViewInfo.TabsViewInfo.GetRealBounds);
        RibbonActiveTab.ViewInfo.Draw(ACanvas);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  finally
    ACanvas.WindowOrg := ASavedOrg;
  end;
end;

procedure TdxRibbonGroupsDockControlSiteViewInfo.InvalidateRect(const R: TRect);
begin
  if FSite.HandleAllocated then
    cxInvalidateRect(FSite.Handle, R);
end;

function TdxRibbonGroupsDockControlSiteViewInfo.GetMinimizeRibbonButtonSize: TSize;
begin
  if (Ribbon.Style >= rs2013) and Ribbon.ShowMinimizeButton then
    Result := Ribbon.ScaleFactor.Apply(cxSize(30, 17))
  else
    Result := cxNullSize;
end;

function TdxRibbonGroupsDockControlSiteViewInfo.GetRibbonActiveTab: TdxRibbonTab;
begin
  Result := FSite.Ribbon.ActiveTab;
end;

function TdxRibbonGroupsDockControlSiteViewInfo.GetRibbonViewInfo: TdxRibbonViewInfo;
begin
  Result := FSite.Ribbon.ViewInfo;
end;

{ TdxRibbonGroupsDockControlSite }

constructor TdxRibbonGroupsDockControlSite.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create(ARibbon);
  FRibbon := ARibbon;
  DoubleBuffered := True;
end;

function TdxRibbonGroupsDockControlSite.CanFocus: Boolean;
begin
  Result := False;
end;

// IdxGestureClient
function TdxRibbonGroupsDockControlSite.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := (AScrollKind = sbHorizontal) and (Ribbon.ViewInfo.GroupsDockControlSiteViewInfo.TabGroupsScrollButtons <> []);
end;

procedure TdxRibbonGroupsDockControlSite.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  DockControl.ViewInfo.InternalScrollGroups(ADeltaX,
    Width - cxMarginsWidth(Ribbon.ViewInfo.GetTabGroupsDockControlOffset));
  Update;
end;

function TdxRibbonGroupsDockControlSite.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := True;
end;

function TdxRibbonGroupsDockControlSite.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

function TdxRibbonGroupsDockControlSite.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := IsChildEx(Handle, AWnd);
end;

procedure TdxRibbonGroupsDockControlSite.AlignControls(AControl: TControl; var Rect: TRect);
begin
end;

procedure TdxRibbonGroupsDockControlSite.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TdxRibbonGroupsDockControlSite.DoCancelMode;
begin
  inherited DoCancelMode;
  Ribbon.Controller.CancelMode;
end;

function TdxRibbonGroupsDockControlSite.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    Result := Ribbon.Controller.MouseWheelDown(Shift, MousePos);
end;

function TdxRibbonGroupsDockControlSite.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    Result := Ribbon.Controller.MouseWheelUp(Shift, MousePos);
end;

function TdxRibbonGroupsDockControlSite.MayFocus: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonGroupsDockControlSite.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  with Ribbon.ScreenToClient(ClientToScreen(Point(X, Y))) do
    Ribbon.Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxRibbonGroupsDockControlSite.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Ribbon.Controller.MouseLeave;
end;

procedure TdxRibbonGroupsDockControlSite.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  with Ribbon.ScreenToClient(ClientToScreen(Point(X, Y))) do
    Ribbon.Controller.MouseMove(Shift, X, Y);
end;

procedure TdxRibbonGroupsDockControlSite.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  with Ribbon.ScreenToClient(ClientToScreen(Point(X, Y))) do
    Ribbon.Controller.MouseUp(Button, Shift, X, Y);
end;

function TdxRibbonGroupsDockControlSite.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonGroupsDockControlSite.Paint;
begin
  Ribbon.ViewInfo.GroupsDockControlSiteViewInfo.Draw(Canvas);
end;

procedure TdxRibbonGroupsDockControlSite.SetRedraw(ARedraw: Boolean);
begin
  if HandleAllocated then
  begin
    SendMessage(Handle, WM_SETREDRAW, Ord(ARedraw), 0);
    if ARedraw and IsWindowVisible(Handle) then
      InvalidateWithChildren;
  end;
end;

function TdxRibbonGroupsDockControlSite.GetDockControl: TdxRibbonGroupsDockControl;
begin
  Result := Ribbon.ActiveTab.DockControl;
end;

{ TdxRibbonOptionsFading }

constructor TdxRibbonOptionsFading.Create;
begin
  FApplicationButton := TdxFadingOptions.Create;
  FBarItems := TdxFadingOptions.Create;
  FTabGroups := TdxFadingOptions.Create;
  FTabs := TdxFadingOptions.Create;
end;

destructor TdxRibbonOptionsFading.Destroy;
begin
  FreeAndNil(FApplicationButton);
  FreeAndNil(FBarItems);
  FreeAndNil(FTabGroups);
  FreeAndNil(FTabs);
  inherited Destroy;
end;

procedure TdxRibbonOptionsFading.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonOptionsFading then
  begin
    ApplicationButton := TdxRibbonOptionsFading(Source).ApplicationButton;
    BarItems := TdxRibbonOptionsFading(Source).BarItems;
    TabGroups := TdxRibbonOptionsFading(Source).TabGroups;
    Tabs := TdxRibbonOptionsFading(Source).Tabs;
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonOptionsFading.SetApplicationButton(AValue: TdxFadingOptions);
begin
  FApplicationButton.Assign(AValue);
end;

procedure TdxRibbonOptionsFading.SetBarItems(AValue: TdxFadingOptions);
begin
  FBarItems.Assign(AValue);
end;

procedure TdxRibbonOptionsFading.SetTabGroups(AValue: TdxFadingOptions);
begin
  FTabGroups.Assign(AValue);
end;

procedure TdxRibbonOptionsFading.SetTabs(AValue: TdxFadingOptions);
begin
  FTabs.Assign(AValue);
end;

{ TdxRibbonTouchModeHelper }

constructor TdxRibbonTouchModeHelper.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
end;

procedure TdxRibbonTouchModeHelper.AdjustFormBorderIconSize(
  AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; var ASize: TSize);
var
  AScaleFactor: Single;
begin
  if IsEnabled and (ASize.cx > 0) then
  begin
    AScaleFactor := Max(1, Ribbon.ScaleFactor.Apply(29) / ASize.cy);
    ASize.cx := Trunc(ASize.cx * AScaleFactor);
    ASize.cy := Trunc(ASize.cy * AScaleFactor);
  end;
end;

procedure TdxRibbonTouchModeHelper.AdjustFormCaptionHeight(var AValue: Integer);
begin
  if IsEnabled then
    AValue := Max(AValue, Ribbon.ScaleFactor.Apply(48));
end;

procedure TdxRibbonTouchModeHelper.AdjustMargins(var AMargins: TRect; AMaxTargetValue: Integer);
var
  AMaxValue: Integer;
begin
  if IsEnabled then
  begin
    AMaxValue := MaxIntValue([1, AMargins.Left, AMargins.Top, AMargins.Right, AMargins.Bottom]);
    AMaxTargetValue := Ribbon.ScaleFactor.Apply(AMaxTargetValue);
    if AMaxValue < AMaxTargetValue then
      AMargins := cxRectScale(AMargins, AMaxTargetValue, AMaxValue);
  end;
end;

procedure TdxRibbonTouchModeHelper.AdjustPartOffsets(APart: Integer; var AValue: TRect);
const
  TargetValue = 7;
begin
  if IsEnabled then
  begin
    AdjustMargins(AValue, TargetValue);
    if APart = DXBAR_APPLICATIONMENUCONTENT then
      AValue.Bottom := Ribbon.ScaleFactor.Apply(25)+  cxMarginsHeight(Ribbon.SkinGetContentOffsets(DXBAR_APPLICATIONMENUBUTTON));
  end;
end;

procedure TdxRibbonTouchModeHelper.AdjustPartSize(APart: Integer; var AValue: Integer);
begin
  // do nothing
end;

function TdxRibbonTouchModeHelper.IsEnabled: Boolean;
begin
  Result := cxIsTouchModeEnabled;
end;

{ TdxCustomRibbon }

constructor TdxCustomRibbon.Create(AOwner: TComponent);
begin
  Include(FInternalState, risCreating);
  RibbonCheckCreateComponent(AOwner, ClassType);
  Exclude(FInternalState, risCreating);
  inherited Create(AOwner);
  DoubleBuffered := True;
  FOptionsFading := TdxRibbonOptionsFading.Create;
  FPainter := CreatePainter;
  FHorizontalNavigationList := TInterfaceList.Create;
  FListeners := TInterfaceList.Create;
  FGroupsDockControlSite := TdxRibbonGroupsDockControlSite.Create(Self);
  FGroupsDockControlSite.Parent := Self;
  FGroupsPainter := CreateGroupsPainter;
  FFonts := TdxRibbonFonts.Create(Self);
  FContexts := TdxRibbonContexts.Create(Self);
  FTabs := TdxRibbonTabCollection.Create(Self);
  FMergeData := TdxRibbonMergeData.Create;
  Align := alTop;
  FShowMinimizeButton := True;
  FShowTabGroups := True;
  FShowTabHeaders := True;
  FEnableTabAero := True;
  FApplicationButton := CreateApplicationButton;
  FHelpButton := CreateHelpButton;
  FQuickAccessToolbar := CreateQuickAccessToolbar;
  FTabAreaSearchToolbar := CreateTabAreaSearchToolbar;
  FTabAreaToolbar := CreateTabAreaToolbar;
  FController := CreateController;
  FTouchModeHelper := CreateTouchModeHelper;
  FPopupMenuItems := dxRibbonDefaultPopupMenuItems;
  FColorSchemeHandlers := TcxEventHandlerCollection.Create;
  FAffiliatedBarControlsForAccessibility := TComponentList.Create(False);
  FBackgroundImage := TdxSmartGlyph.Create;
  FBackgroundImage.OnChange := BackgroundImageChangeHandler;
  FCapitalizeTabCaptions := bFalse;

  Include(FState, rsModifiedLocked);
  try
    InitColorScheme;
    if IsDesigning then
    begin
      BarManager := GetBarManagerByComponent(AOwner);
      Tabs.Add;
    end;
  finally
    Exclude(FState, rsModifiedLocked);
  end;

  Fading := True;
  FInternalItems := TComponentList.Create;
  FRibbonFormNonClientParts := TObjectList.Create(False);
  FMinimizeOnTabDblClick := True;
  InitializeRibbonForm;
end;

destructor TdxCustomRibbon.Destroy;
begin
  if risCreating in FInternalState then Exit;
  UnmergeBeforeDestroy;
  SupportNonClientDrawing := False;
  FreeAndNil(FRibbonFormNonClientParts);
  FreeAndNil(FTabGroupsPopupWindow);
  FreeAndNil(FInternalItems);
  FreeAndNil(FController);
  FreeAndNil(FAffiliatedBarControlsForAccessibility);
  FreeAndNil(FColorSchemeHandlers);
  BarManager := nil;
  FreeAndNil(FApplicationButton);
  FreeAndNil(FBackgroundImage);
  FreeAndNil(FHelpButton);
  FreeAndNil(FQuickAccessToolbar);
  FreeAndNil(FTabAreaSearchToolbar);
  FreeAndNil(FTabAreaToolbar);
  ColorScheme := nil;
  FreeAndNil(FMergeData);
  FreeAndNil(FTabs);
  FreeAndNil(FContexts);
  FreeAndNil(FViewInfo);
  FreeAndNil(FTouchModeHelper);
  FreeAndNil(FHorizontalNavigationList);
  FreeAndNil(FGroupsPainter);
  FreeAndNil(FListeners);
  FreeAndNil(FPainter);
  FreeAndNil(FOptionsFading);
  FreeAndNil(FFonts);
  inherited Destroy;
end;

function TdxCustomRibbon.ApplicationMenuPopup: Boolean;
var
  AClosedByEscape: Boolean;
  AObjectLink: TcxObjectLink;
begin
  Result := False;
  if not Controller.IsApplicationMenuDropped then
  begin
    AObjectLink := cxAddObjectLink(Self);
    try
      ApplicationButtonViewInfo.IsPressed := True;
      try
        if DoApplicationMenuClick or (ApplicationButton.IMenu = nil) then
          BarNavigationController.StopKeyboardHandling
        else
          if ApplicationButton.CanShowPopup then
          begin
            BeforeApplicationMenuPopup;
            Result := ApplicationButton.Popup(AClosedByEscape);
            if AObjectLink.Ref <> nil then
            begin
              if Result and AClosedByEscape and BarNavigationController.NavigationMode then
                ApplicationButton.IAccessibilityHelper.Select(False);
              AfterApplicationMenuPopup;
            end;
          end;
      finally
        if AObjectLink.Ref <> nil then
        begin
          ApplicationButtonViewInfo.IsPressed := False;
          Controller.PressedObject := rhtNone;
        end;
      end;
    finally
      cxRemoveObjectLink(AObjectLink);
    end;
  end;
end;

function TdxCustomRibbon.AreGroupsVisible: Boolean;
begin
  Result := not Hidden and (ShowTabGroups or IsPopupGroupsMode);
end;

procedure TdxCustomRibbon.BeginUpdate;
begin
  Inc(FLockCount);
  GroupsDockControlSite.SetRedraw(False);
end;

function TdxCustomRibbon.CanFocus: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbon.CheckHide;
var
  AForm: TCustomForm;
  DC: HDC;
begin
  if Hidden <> ViewInfo.IsNeedHideControl then
  begin
    AForm := GetParentForm(Self, False);
    if not Hidden and (AForm <> nil) and AForm.HandleAllocated then
    begin
      Changed;
      AForm.Invalidate;
      DC := GetDC(AForm.Handle);
      try
        SendMessage(AForm.Handle, WM_ERASEBKGND, DC, DC);
      finally
        ReleaseDC(AForm.Handle, DC);
      end;
    end
    else
      Changed;
  end;
end;

procedure TdxCustomRibbon.CloseTabGroupsPopupWindow;
begin
  InternalCloseTabGroupsPopupWindow;
end;

function TdxCustomRibbon.ContainsToolBar(AToolBar: TdxBar): Boolean;
begin
  Result := (QuickAccessToolbar.Toolbar = AToolBar) or Tabs.ContainsToolBar(AToolBar);
end;

procedure TdxCustomRibbon.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and not IsDestroying then
  begin
    Changed;
    GroupsDockControlSite.SetRedraw(True);
    RibbonFormInvalidate;
  end;
end;

function TdxCustomRibbon.GetTabAtPos(X, Y: Integer): TdxRibbonTab;
begin
  Result := ViewInfo.GetTabAtPos(X, Y);
end;

procedure TdxCustomRibbon.InvalidateRect(const R: TRect);
var
  R1: TRect;
begin
  if HandleAllocated and not IsDestroying then
  begin
    cxInvalidateRect(Handle, R, False);
    if cxRectIntersect(R1, R, GroupsDockControlSite.BoundsRect) then
      GroupsDockControlSite.InvalidateRect(R1, False);
  end;
end;

function TdxCustomRibbon.ShowCustomizationForm(AMode: TdxRibbonCustomizationFormMode): Boolean;
begin
  Result := True;
  if Assigned(FShowRibbonCustomizationFormFunc) then
    Result := FShowRibbonCustomizationFormFunc(Self, AMode)
  else
    BarManager.Customizing(True);
end;

procedure TdxCustomRibbon.ShowTabGroupsPopupWindow;
begin
  if ShowTabGroups then
    Exit;
  if FTabGroupsPopupWindow = nil then
    FTabGroupsPopupWindow := TdxRibbonTabGroupsPopupWindow.Create(Self);
  FTabGroupsPopupWindow.AllowShowHideAnimation := Style >= rs2013;
  FTabGroupsPopupWindow.OwnerBounds := BoundsRect;
  FTabGroupsPopupWindow.OwnerParent := Parent;
  FTabGroupsPopupWindow.Popup(nil);
  FTabGroupsPopupWindow.Invalidate;
  Invalidate;
end;

procedure TdxCustomRibbon.AddListener(AListener: IdxRibbonListener);
begin
  FListeners.Add(AListener);
end;

procedure TdxCustomRibbon.RemoveListener(AListener: IdxRibbonListener);
begin
  if FListeners <> nil then
    FListeners.Remove(AListener);
end;

procedure TdxCustomRibbon.AddTab(ATab: TdxRibbonTab);
begin
  if ATab = nil then Exit;
  ATab.Ribbon := Self;
  if ActiveTab = nil then
    ActiveTab := ATab;
  Changed;
end;

procedure TdxCustomRibbon.RemoveTab(ATab: TdxRibbonTab);
begin
  if ATab = nil then Exit;
  BarDesignController.LockDesignerModified;
  try
    ATab.Ribbon := nil;
    if ActiveTab = ATab then
      SetNextActiveTab(ATab);
    if FLoadedActiveTab = ATab then
      FLoadedActiveTab := nil;
    if FActiveTab = ATab then
      FActiveTab := nil;
    if FPreviousActiveTab = ATab then
      FPreviousActiveTab := nil;
  finally
    BarDesignController.UnLockDesignerModified;
  end;
  Changed;
end;

procedure TdxCustomRibbon.SetNextActiveTab(ATab: TdxRibbonTab);
begin
  if csDestroying in ATab.ComponentState then
    Include(FState, rsModifiedLocked);
  try
    ActiveTab := GetNextActiveTab(ATab);
  finally
    Exclude(FState, rsModifiedLocked);
  end;
end;

procedure TdxCustomRibbon.FullInvalidate;
begin
  if IsDestroying or not (HandleAllocated and Visible) then
    Exit;

  QuickAccessToolbar.UpdateColorScheme;
  TabAreaSearchToolbar.UpdateColorScheme;
  TabAreaToolbar.UpdateColorScheme;
  UpdateActiveTab;
  RibbonFormInvalidate;
  FGroupsDockControlSite.Invalidate; //for CBuilder
  Invalidate;
end;

procedure TdxCustomRibbon.Changed;
begin
  if IsLocked then
    Exit;
  if not (IsDesigning or (rsCancelHintLocked in FState)) then
    dxFader.Clear;
  if not (rsCancelHintLocked in FState) then
    Controller.CancelHint;
  if FormCaptionHelper <> nil then
    FormCaptionHelper.Calculate;
  ViewInfo.Calculate;
  Invalidate;
end;

procedure TdxCustomRibbon.RecalculateBars;
var
  AControl: TdxBarControl;
  I: Integer;
begin
  if IsBarManagerValid then
  begin
    RibbonRecalculateBar(TabAreaToolbar.Toolbar);
    RibbonRecalculateBar(TabAreaSearchToolbar.Toolbar);

    if Assigned(QuickAccessToolbar.Toolbar) then
    begin
      if QuickAccessToolbar.Toolbar.Control is TdxRibbonQuickAccessToolbarBarControl then
      begin
        with TdxRibbonQuickAccessToolbarBarControl(QuickAccessToolbar.Toolbar.Control) do
          UpdateDefaultGlyph(FDefaultGlyph);
      end;
      RibbonRecalculateBar(QuickAccessToolbar.Toolbar);
    end;

    if ActiveTab <> nil then
      ActiveTab.RecalculateBars;

    with BarManager do
    begin
      BeginUpdate;
      try
        for I := 0 to Bars.Count - 1 do
        begin
          AControl := Bars.Items[I].Control;
          if AControl <> nil then
            TdxBarControlAccess(AControl).CalcDrawingConsts;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

function TdxCustomRibbon.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := (AScrollKind = sbHorizontal) and (ViewInfo.TabsAreaViewInfo.TabsViewInfo.ScrollButtons <> []);
end;

procedure TdxCustomRibbon.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  with ViewInfo.TabsAreaViewInfo.TabsViewInfo do
    ScrollPosition := ScrollPosition - ADeltaX;
end;

function TdxCustomRibbon.IsPanArea(const APoint: TPoint): Boolean;
var
  AHitInfo: TdxRibbonHitInfo;
begin
  AHitInfo := TdxRibbonHitInfo.Create(Self);
  try
    AHitInfo.Calculate(APoint);
    Result := AHitInfo.HitTest = rhtTab;
  finally
    AHitInfo.Free;
  end;
end;

function TdxCustomRibbon.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbon.AdjustRibbonFormBorderIconSize(
  AIcon: TdxRibbonBorderDrawIcon; AIsToolWindow: Boolean; ACaptionHeight: Integer; var ASize: TSize);
begin
  ColorScheme.AdjustRibbonFormBorderIconSize(AIcon, AIsToolWindow, ACaptionHeight, ASize);
  TouchModeHelper.AdjustFormBorderIconSize(AIcon, AIsToolWindow, ASize);
end;

procedure TdxCustomRibbon.DrawRibbonFormBackground(DC: HDC; const ARect: TRect);
begin
  ColorScheme.InitializePaintData(Painter);
  ColorScheme.UseRightToLeftAlignment := RibbonForm.UseRightToLeftAlignment;
  ColorScheme.DrawRibbonFormBackground(DC, ARect, ViewInfo.Bounds.Bottom - ARect.Top);
  ColorScheme.UseRightToLeftAlignment := False;
end;

procedure TdxCustomRibbon.DrawRibbonFormBorderIcon(ACanvas: TcxCanvas; const ABounds: TRect;
  AIcon: TdxRibbonBorderDrawIcon; AState: TdxRibbonBorderIconState);
begin
  Painter.DrawRibbonFormBorderIcon(ACanvas, ABounds, AIcon, AState);
end;

procedure TdxCustomRibbon.DrawRibbonFormBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect);
begin
  CheckDrawRibbonFormStatusBarBorders(ACanvas, ABordersWidth);
  Painter.DrawRibbonFormBorders(ACanvas, ABordersWidth);
end;

procedure TdxCustomRibbon.DrawRibbonFormCaption(ACanvas: TcxCanvas; const ABounds: TRect; const ACaption: string);
begin
  Painter.DrawRibbonFormCaption(ACanvas, ABounds);
end;

function TdxCustomRibbon.GetApplicationMenuState: TdxRibbonApplicationMenuState;
begin
  Result := Controller.ApplicationMenuState;
end;

procedure TdxCustomRibbon.GetApplicationMenuTabOrderList(List: TList);
begin
  if ApplicationButton.IMenu <> nil then
    ApplicationButton.IMenu.GetTabOrderList(List)
end;

function TdxCustomRibbon.GetRibbonFormCaptionAreaExtension: Integer;
begin
  if ColorScheme.ExtendCaptionAreaOnTabs and (not ViewInfo.UseGlass or EnableTabAero) then
    Result := ViewInfo.TabsAreaViewInfo.MeasuredHeight
  else
    Result := 0;
end;

function TdxCustomRibbon.GetRibbonFormCaptionHeight: Integer;
begin
  Result := CalculateFormCaptionHeight(True);
end;

function TdxCustomRibbon.GetRibbonFormCaptionHeightForHiddenRibbon: Integer;
begin
  Result := CalculateFormCaptionHeight(False);
end;

function TdxCustomRibbon.GetRibbonFormColor: TColor;
begin
  Result := SkinGetPartColor(rfspRibbonForm);
end;

function TdxCustomRibbon.GetRibbonFormExtendedCaptionAreaRegion: HRGN;
var
  ARect: TRect;
begin
  Result := 0;
  if (GetRibbonFormCaptionAreaExtension > 0) and not IsAutoHideModeActive then
  begin
    ARect := ViewInfo.TabsAreaViewInfo.ExtendedCaptionAreaRect;
    if not cxRectIsEmpty(ARect) then
      Result := CreateRectRgnIndirect(ARect);
  end;
end;

function TdxCustomRibbon.GetRibbonLoadedHeight: Integer;
begin
  Result := FLoadedHeight;
end;

function TdxCustomRibbon.GetRibbonNonClientAreaObjectsRegion: HRGN;
begin
  Result := ViewInfo.GetNonClientAreaObjectsRegion;
end;

function TdxCustomRibbon.GetRibbonQATNonClientAreaBounds: TRect;
begin
  if ViewInfo.QuickAccessToolbarViewInfo.AtNonClientArea then
    Result := ViewInfo.QuickAccessToolbarViewInfo.DockControlBounds
  else
    Result := cxNullRect;
end;

function TdxCustomRibbon.GetRibbonQATOptionAddItemCaption: string;
begin
  if (BarDesignController.CustomizingItemLink = nil) or (BarDesignController.CustomizingItemLink.Item.GetAddMessageName = '') then
    Result := cxGetResourceString(@dxSBAR_ADDTOQAT)
  else
    Result := Format(cxGetResourceString(@dxSBAR_ADDTOQATITEMNAME), [BarDesignController.CustomizingItemLink.Item.GetAddMessageName]);
end;

function TdxCustomRibbon.GetRibbonQATOptionAddItemEnabled: Boolean;
var
  ABar: TdxBar;
  I: Integer;
begin
  if BarDesignController.CustomizingItemLink = nil then
  begin
    ABar := GetBar(BarDesignController.CustomizingBarControl);
    Result := not QuickAccessToolbar.HasGroupButtonForToolbar(ABar);
  end
  else
  begin
    Result := IsQuickAccessToolbarValid;
    if Result then
    begin
      ABar := QuickAccessToolbar.Toolbar;
      for I := 0 to ABar.ItemLinks.Count - 1 do
        if ABar.ItemLinks[I].Item = BarDesignController.CustomizingItemLink.Item then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
end;

function TdxCustomRibbon.GetRibbonQATPositionButtonCaption: string;
begin
  if QuickAccessToolbar.Position = qtpAboveRibbon then
    Result := cxGetResourceString(@dxSBAR_SHOWBELOWRIBBON)
  else
    Result := cxGetResourceString(@dxSBAR_SHOWABOVERIBBON);
end;

function TdxCustomRibbon.GetRibbonStyle: TdxRibbonStyle;
begin
  Result := Style;
end;

function TdxCustomRibbon.GetTaskbarCaption: TCaption;
begin
  if (RibbonForm <> nil) and (RibbonForm.FormStyle = fsMDIForm) then
  begin
    Result := RibbonForm.Caption;
    if DocumentName <> '' then
      Result := Result + ' - ' + DocumentName;
  end
  else
    Result := ViewInfo.GetFormCaptionText;
end;

function TdxCustomRibbon.GetValidPopupMenuItems: TdxRibbonPopupMenuItems;
begin
  Result := PopupMenuItems;
  if not IsQuickAccessToolbarValid then
    Result := Result - [rpmiQATPosition, rpmiQATAddRemoveItem, rpmiCustomizeQAT];
  if Style = rs2016Tablet then
    Result := Result - [rpmiQATPosition];
  if not Assigned(FShowRibbonCustomizationFormFunc) then
    Result := Result - [rpmiCustomizeRibbon];
  if Assigned(FShowRibbonCustomizationFormFunc) then
    Result := Result - [rpmiMoreCommands]
  else
    Result := Result - [rpmiCustomizeQAT];
end;

function TdxCustomRibbon.GetWindowBordersWidth: TRect;
begin
  Result := ColorScheme.GetWindowBordersWidth(HasStatusBar);
end;

function TdxCustomRibbon.HasExternalRibbonFormShadow: Boolean;
begin
  Result := ColorScheme.HasExternalRibbonFormShadow;
end;

function TdxCustomRibbon.HasHelpButton: Boolean;
begin
  Result := Assigned(OnHelpButtonClick)
end;

function TdxCustomRibbon.HasStatusBar: Boolean;
begin
  Result := GetStatusBarInterface <> nil;
end;

function TdxCustomRibbon.UseRoundedWindowCorners: Boolean;
begin
  Result := ColorScheme.UseRoundedWindowCorners;
end;

procedure TdxCustomRibbon.RibbonFormCaptionChanged;
var
  AOnMDIForm: Boolean;
  AForm: TForm;
begin
  AOnMDIForm := IsOnRibbonMDIForm;
  Include(FState, rsCancelHintLocked);
  Inc(FLockCount);
  try
    if AOnMDIForm then
    begin
      AForm := RibbonForm.ActiveMDIChild;
      if (AForm <> nil) and IsZoomed(AForm.Handle) then
        DocumentName := AForm.Caption
      else
        DocumentName := '';
    end;
  finally
    Dec(FLockCount);
    Exclude(FState, rsCancelHintLocked);
    if RibbonForm <> nil then
    begin
      ViewInfo.Calculate;
      UpdateNonClientArea;
      if AOnMDIForm then
        Application.Title := GetTaskbarCaption;
    end;
  end;
end;

procedure TdxCustomRibbon.RibbonFormIconChanged;
begin
  Painter.FlushCache;
  Invalidate;
end;

procedure TdxCustomRibbon.RibbonFormSizing;
begin
  Include(FState, rsSizing);
end;

procedure TdxCustomRibbon.RibbonFormSized;
begin
  Exclude(FState, rsSizing);
  CheckHide;
  if Controller.IsApplicationMenuDropped then
    ApplicationButton.IMenu.RibbonFormResized;
end;

procedure TdxCustomRibbon.UpdateNonClientArea;
const
  RedrawFlags = RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ERASENOW;
  RedrawAllFlags = RedrawFlags or RDW_ALLCHILDREN;
begin
  if HandleAllocated and Visible then
  begin
    cxRedrawWindow(Handle, RedrawFlags);
    if SupportNonClientDrawing then
    begin
      if ViewInfo.TabsAreaViewInfo.IsToolbarVisible then
        cxRedrawWindow(TabAreaToolbar.DockControl.Handle, RedrawAllFlags);
      if ViewInfo.TabsAreaViewInfo.IsSearchToolbarVisible then
        cxRedrawWindow(TabAreaSearchToolbar.DockControl.Handle, RedrawAllFlags);
    end;
    if ViewInfo.QuickAccessToolbarViewInfo.AtNonClientArea then
      cxRedrawWindow(QuickAccessToolbar.DockControl.Handle, RedrawAllFlags);
    if ApplicationButton.IMenu <> nil then
      ApplicationButton.IMenu.UpdateNonClientArea;
  end;
end;

procedure TdxCustomRibbon.DrawTabGroupBackground(DC: HDC; const ARect: TRect; AState: Integer; AIsInPopup: Boolean);
var
  R: TRect;
begin
  R := ARect;
  Dec(R.Bottom, GetGroupCaptionHeight + Painter.GetGroupCaptionBottomOffset(AIsInPopup));
  ColorScheme.UseRightToLeftAlignment := UseRightToLeftAlignment;
  try
    ColorScheme.DrawTabGroupBackground(DC, R, AState, AIsInPopup);
    ColorScheme.DrawTabGroupHeaderBackground(DC, Rect(R.Left, R.Bottom, R.Right, ARect.Bottom), AState, AIsInPopup);
  finally
    ColorScheme.UseRightToLeftAlignment := False;
  end;
end;

procedure TdxCustomRibbon.DXMRecalculate(var Message: TMessage);
begin
  Changed;
end;

procedure TdxCustomRibbon.DXMShowApplicationMenu(var Message: TMessage);
begin
  ApplicationMenuPopup;
end;

function TdxCustomRibbon.GetGroupCaptionHeight: Integer;
begin
  Result := GroupsPainter.GetGroupCaptionHeight;
end;

function TdxCustomRibbon.GetGroupContentHeight: Integer;
begin
  Result := GetGroupRowHeight * GroupsPainter.GetGroupRowCount;
end;

function TdxCustomRibbon.GetGroupHeight: Integer;
begin
  Result := GetGroupContentHeight + cxMarginsHeight(SkinGetContentOffsets(DXBAR_TOOLBAR));
end;

function TdxCustomRibbon.GetGroupRowHeight: Integer;
begin
  Result := GroupsPainter.GetGroupRowHeight(GroupsPainter.GetSmallIconSize);
end;

function TdxCustomRibbon.GetHelpButtonScreenTip: TdxScreenTip;
begin
  Result := HelpButton.ScreenTip;
end;

function TdxCustomRibbon.GetHidden: Boolean;
begin
  Result := ViewInfo.Hidden;
end;

procedure TdxCustomRibbon.SkinDrawBackground(DC: HDC; const ARect: TRect; APart, AState: Integer);
begin
  SkinDrawBackgroundEx(DC, ARect, ARect, APart, AState);
end;

procedure TdxCustomRibbon.SkinDrawBackgroundEx(DC: HDC; const ARect: TRect;
  const AContentRect: TRect; APart: Integer; AState: Integer = 0);
begin
  ColorScheme.InitializePaintData(Painter);
  ColorScheme.UseRightToLeftAlignment := UseRightToLeftAlignment;
  case APart of
    DXBAR_BACKSTAGEVIEW:
      ColorScheme.DrawBackstageViewBackground(DC, ARect);
    DXBAR_BACKSTAGEVIEW_BACKBUTTON:
      ColorScheme.DrawBackstageViewBackButton(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_MENUBAR:
      ColorScheme.DrawBackstageViewMenuBackground(DC, ARect);
    DXBAR_BACKSTAGEVIEW_MENUBAR_HEADER:
      ColorScheme.DrawBackstageViewMenuHeader(DC, ARect);
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
      ColorScheme.DrawBackstageViewMenuButton(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR:
      ColorScheme.DrawBackstageViewMenuSeparator(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_MENUBAR_TAB:
      ColorScheme.DrawBackstageViewTabButton(DC, ARect, AState);

    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL:
      ColorScheme.DrawBackstageViewGalleryBackground(DC, ARect);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEM:
      ColorScheme.DrawBackstageViewGalleryItem(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_GROUPHEADER:
      ColorScheme.DrawBackstageViewGalleryGroupHeader(DC, ARect);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTON:
      ColorScheme.DrawBackstageViewGalleryItemPinButton(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINBUTTONGLYPH:
      ColorScheme.DrawBackstageViewGalleryItemPinButtonGlyph(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_ITEMPINTAG:
      ColorScheme.DrawBackstageViewGalleryItemPinTag(DC, ARect, AState);
    DXBAR_BACKSTAGEVIEW_GALLERYCONTROL_SEPARATOR:
      ColorScheme.DrawBackstageViewGallerySeparator(DC, ARect);

    DXBAR_SCROLLBOX_SIZEGRIPAREA:
      ColorScheme.DrawScrollBoxSizeGripArea(DC, ARect);
    DXBAR_SCROLLBARHORZ_BACKGROUND, DXBAR_SCROLLBARVERT_BACKGROUND:
      ColorScheme.DrawScrollBarBackground(DC, ARect, APart = DXBAR_SCROLLBARHORZ_BACKGROUND);
    DXBAR_SCROLLBARHORZ_LINEDOWN, DXBAR_SCROLLBARVERT_LINEDOWN:
      ColorScheme.DrawScrollBarPart(DC, ARect, sbpLineDown, AState, APart = DXBAR_SCROLLBARHORZ_LINEDOWN);
    DXBAR_SCROLLBARHORZ_LINEUP, DXBAR_SCROLLBARVERT_LINEUP:
      ColorScheme.DrawScrollBarPart(DC, ARect, sbpLineUp, AState, APart = DXBAR_SCROLLBARHORZ_LINEUP);
    DXBAR_SCROLLBARHORZ_THUMBNAIL, DXBAR_SCROLLBARVERT_THUMBNAIL:
      ColorScheme.DrawScrollBarPart(DC, ARect, sbpThumbnail, AState, APart = DXBAR_SCROLLBARHORZ_THUMBNAIL);
    DXBAR_SCROLLBARHORZ_PAGEDOWN, DXBAR_SCROLLBARVERT_PAGEDOWN:
      ColorScheme.DrawScrollBarPart(DC, ARect, sbpPageDown, AState, APart = DXBAR_SCROLLBARHORZ_PAGEDOWN);
    DXBAR_SCROLLBARHORZ_PAGEUP, DXBAR_SCROLLBARVERT_PAGEUP:
      ColorScheme.DrawScrollBarPart(DC, ARect, sbpPageUp, AState, APart = DXBAR_SCROLLBARHORZ_PAGEUP);

    DXBAR_PROGRESSSOLIDBAND:
      ColorScheme.DrawProgressSolidBand(DC, ARect);
    DXBAR_PROGRESSSUBSTRATE:
      ColorScheme.DrawProgressSubstrate(DC, ARect);
    DXBAR_PROGRESSDISCRETEBAND:
      ColorScheme.DrawProgressDiscreteBand(DC, ARect);

    DXBAR_INRIBBONGALLERY:
      ColorScheme.DrawInRibbonGalleryBackground(DC, ARect, AState);
    DXBAR_INRIBBONGALLERYSCROLLBAR_LINEUPBUTTON:
      ColorScheme.DrawInRibbonGalleryScrollBarButton(DC, ARect, gsbkLineUp, AState);
    DXBAR_INRIBBONGALLERYSCROLLBAR_LINEDOWNBUTTON:
      ColorScheme.DrawInRibbonGalleryScrollBarButton(DC, ARect, gsbkLineDown, AState);
    DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON:
      ColorScheme.DrawInRibbonGalleryScrollBarButton(DC, ARect, gsbkDropDown, AState);
    DXBAR_INRIBBONGALLERYSCROLLBAR_DROPDOWNBUTTON_TOUCH:
      ColorScheme.DrawInRibbonGalleryScrollBarDropDownTouchButton(DC, ARect, AState);
    DXBAR_INRIBBONGALLERYSCROLLBAR_BACKGROUND:
      ColorScheme.DrawInRibbonGalleryScrollBarBackground(DC, ARect, AState);

    DXBAR_DROPDOWNGALLERY:
      ColorScheme.DrawDropDownGalleryBackground(DC, ARect);
    DXBAR_DROPDOWNGALLERY_TOPSIZINGBAND:
      ColorScheme.DrawDropDownGalleryTopSizingBand(DC, ARect);
    DXBAR_DROPDOWNGALLERY_BOTTOMSIZINGBAND:
      ColorScheme.DrawDropDownGalleryBottomSizingBand(DC, ARect);
    DXBAR_DROPDOWNGALLERY_TOPSIZEGRIP:
      ColorScheme.DrawDropDownGalleryTopSizeGrip(DC, ARect);
    DXBAR_DROPDOWNGALLERY_BOTTOMSIZEGRIP:
      ColorScheme.DrawDropDownGalleryBottomSizeGrip(DC, ARect);
    DXBAR_DROPDOWNGALLERY_TOPVERTICALSIZEGRIP:
      ColorScheme.DrawDropDownGalleryTopVerticalSizeGrip(DC, ARect);
    DXBAR_DROPDOWNGALLERY_BOTTOMVERTICALSIZEGRIP:
      ColorScheme.DrawDropDownGalleryBottomVerticalSizeGrip(DC, ARect);

    DXBAR_GALLERYGROUPHEADERBACKGROUND:
      ColorScheme.DrawGalleryGroupHeaderBackground(DC, ARect);
    DXBAR_GALLERYFILTERBAND:
      ColorScheme.DrawGalleryFilterBandBackground(DC, ARect);
    DXBAR_GALLERYGROUPITEM_SELECTIONFRAME:
      ColorScheme.DrawGalleryGroupItemSelectionFrame(DC, ARect, AState);

    DXBAR_SEPARATOR_LINE:
      ColorScheme.DrawSeparatorLine(DC, ARect);
    DXBAR_SEPARATOR_BACKGROUND:
      ColorScheme.DrawSeparatorBackground(DC, ARect);

    DXBAR_MENUARROWDOWN:
      ColorScheme.DrawMenuArrowDown(DC, ARect);
    DXBAR_MENUARROWRIGHT:
      ColorScheme.DrawMenuArrowRight(DC, ARect);
    DXBAR_MENUBACKBUTTON:
      ColorScheme.DrawMenuBackButton(DC, ARect);
    DXBAR_MENUCHECK:
    begin
      ColorScheme.UseRightToLeftAlignment := False;
      ColorScheme.DrawMenuCheck(DC, ARect, AState);
    end;
    DXBAR_MENUCHECKMARK:
    begin
      ColorScheme.UseRightToLeftAlignment := False;
      ColorScheme.DrawMenuCheckMark(DC, ARect, AState);
    end;
    DXBAR_MENUCONTENT:
      ColorScheme.DrawMenuContent(DC, ARect);
    DXBAR_MENUDETACHCAPTION:
      ColorScheme.DrawMenuDetachCaption(DC, ARect, AState);
    DXBAR_MENUGLYPH:
      ColorScheme.DrawMenuGlyph(DC, ARect);
    DXBAR_MENUITEM:
      ColorScheme.DrawMenuItem(DC, ARect, AState);
    DXBAR_MENUITEM_GLYPH:
      ColorScheme.DrawMenuItemDropButtonMainPart(DC, ARect, AState);
    DXBAR_MENUITEM_DROPBUTTON:
      ColorScheme.DrawMenuItemDropButtonArrowPart(DC, ARect, AState);
    DXBAR_MENUMARK:
      ColorScheme.DrawMenuMark(DC, ARect);
    DXBAR_MENUSEPARATORHORZ:
      ColorScheme.DrawMenuSeparatorHorz(DC, ARect);
    DXBAR_MENUSEPARATORVERT:
      ColorScheme.DrawMenuSeparatorVert(DC, ARect);
    DXBAR_MENUSCROLLAREA:
      ColorScheme.DrawMenuScrollArea(DC, ARect, AState);

    DXBAR_MENUEXTRAPANE_PINBUTTON:
      ColorScheme.DrawApplicationMenuExtraPanePinButton(DC, ARect, AState);
    DXBAR_MENUEXTRAPANE_BUTTON:
      ColorScheme.DrawApplicationMenuExtraPaneButton(DC, ARect, AState);
    DXBAR_MENUEXTRAPANE_SEPARATOR, DXBAR_MENUEXTRAPANE_SEPARATOR_VERT:
      ColorScheme.DrawMenuExtraSeparator(DC, ARect, APart = DXBAR_MENUEXTRAPANE_SEPARATOR);

    DXBAR_COLLAPSEDTOOLBAR:
      ColorScheme.DrawCollapsedToolbarBackground(DC, ARect, AState);
    DXBAR_COLLAPSEDTOOLBARGLYPHBACKGROUND:
      ColorScheme.DrawCollapsedToolbarGlyphBackground(DC, ARect, AState);

    DXBAR_ARROWDOWN:
      ColorScheme.DrawArrowDown(DC, ARect, AState);
    DXBAR_MARKARROW, DXBAR_MARKARROWINPOPUP:
      ColorScheme.DrawMarkArrow(DC, ARect, AState);
    DXBAR_MARKTRUNCATED:
      ColorScheme.DrawMarkTruncated(DC, ARect, AState);
    DXBAR_SCROLLARROW:
      ColorScheme.DrawScrollArrow(DC, ARect, AState);

    DXBAR_APPLICATIONMENU:
      begin
        ColorScheme.DrawApplicationMenuBackground(DC, ARect, AContentRect);
        DrawApplicationMenuHeader(DC, True);
      end;
    DXBAR_APPLICATIONMENUBUTTON:
      ColorScheme.DrawApplicationMenuButton(DC, ARect, AState);

    DXBAR_EDIT_ARROWBUTTON:
      ColorScheme.DrawEditArrowButton(DC, ARect, AState);
    DXBAR_EDIT_BUTTON:
      ColorScheme.DrawEditButton(DC, ARect, AState);
    DXBAR_EDIT_ELLIPSISBUTTON:
      ColorScheme.DrawEditEllipsisButton(DC, ARect, AState);

    DXBAR_SPINEDIT_UPBUTTON:
      ColorScheme.DrawEditSpinUpButton(DC, ARect, AState);
    DXBAR_SPINEDIT_DOWNBUTTON:
      ColorScheme.DrawEditSpinDownButton(DC, ARect, AState);

    DXBAR_SMALLBUTTON:
      ColorScheme.DrawSmallButton(DC, ARect, AState);
    DXBAR_SMALLBUTTON_GLYPH:
      ColorScheme.DrawSmallButtonDropButtonMainPart(DC, ARect, AState);
    DXBAR_SMALLBUTTON_DROPBUTTON:
      ColorScheme.DrawSmallButtonDropButtonArrowPart(DC, ARect, AState);

    DXRIBBON_TAT_SMALLBUTTON:
      ColorScheme.DrawTabAreaButton(DC, ARect, RibbonStateToButtonState(AState));
    DXRIBBON_TAT_SMALLBUTTON_DROPBUTTON:
      ColorScheme.DrawTabAreaButtonDropButtonArrowPart(DC, ARect, AState);
    DXRIBBON_TAT_SMALLBUTTON_GLYPH:
      ColorScheme.DrawTabAreaButtonDropButtonMainPart(DC, ARect, AState);
    DXRIBBON_TAT_MARKTRUNCATED:
      ColorScheme.DrawTabAreaMarkTruncated(DC, ARect, AState);
    DXRIBBON_TAT_MARKARROW:
      ColorScheme.DrawTabAreaMarkArrow(DC, ARect, AState);
    DXRIBBON_TAT_ARROWDOWN:
      ColorScheme.DrawTabAreaArrowDown(DC, ARect, AState);

    DXBAR_LARGEBUTTON:
      ColorScheme.DrawLargeButton(DC, ARect, AState);
    DXBAR_LARGEBUTTON_GLYPH:
      ColorScheme.DrawLargeButtonDropButtonMainPart(DC, ARect, AState);
    DXBAR_LARGEBUTTON_DROPBUTTON:
      ColorScheme.DrawLargeButtonDropButtonArrowPart(DC, ARect, AState);

    DXBAR_BUTTONGROUP:
      ColorScheme.DrawButtonGroup(DC, ARect, AState);
    DXBAR_BUTTONGROUP_DROPBUTTON:
      ColorScheme.DrawButtonGroupDropButtonArrowPart(DC, ARect, AState);
    DXBAR_BUTTONGROUP_GLYPH:
      ColorScheme.DrawButtonGroupDropButtonMainPart(DC, ARect, AState);
    DXBAR_BUTTONGROUPBORDERLEFT:
      ColorScheme.DrawButtonGroupBorderLeft(DC, ARect);
    DXBAR_BUTTONGROUPBORDERMIDDLE:
      ColorScheme.DrawButtonGroupBorderMiddle(DC, ARect, AState);
    DXBAR_BUTTONGROUPBORDERRIGHT:
      ColorScheme.DrawButtonGroupBorderRight(DC, ARect);
    DXBAR_BUTTONGROUPSPLITBUTTONSEPARATOR:
      ColorScheme.DrawButtonGroupSplitButtonSeparator(DC, ARect, AState);

    DXBAR_LAUNCHBUTTONDEFAULTGLYPH:
      ColorScheme.DrawLaunchButtonDefaultGlyph(DC, ARect, AState);
    DXBAR_LAUNCHBUTTONBACKGROUND:
      ColorScheme.DrawLaunchButtonBackground(DC, ARect, AState);

    DXRIBBON_QAT_ARROWDOWN:
      ColorScheme.DrawQuickAccessToolbarArrowDown(DC, ARect, AState, ViewInfo.QuickAccessToolbarViewInfo.AtBottom);
    DXRIBBON_QAT_SMALLBUTTON:
      ColorScheme.DrawQuickAccessToolbarSmallButton(DC, ARect, AState);
    DXRIBBON_QAT_SMALLBUTTON_GLYPH:
      ColorScheme.DrawQuickAccessToolbarSmallButtonDropButtonMainPart(DC, ARect, AState);
    DXRIBBON_QAT_SMALLBUTTON_DROPBUTTON:
      ColorScheme.DrawQuickAccessToolbarSmallButtonDropButtonArrowPart(DC, ARect, AState);
    DXRIBBON_QAT_MARKARROW:
      ColorScheme.DrawQuickAccessToolbarMarkArrow(DC, ARect, AState, ViewInfo.QuickAccessToolbarViewInfo.AtBottom);
    DXRIBBON_QAT_MARKTRUNCATED:
      ColorScheme.DrawQuickAccessToolbarMarkTruncated(DC, ARect, AState, ViewInfo.QuickAccessToolbarViewInfo.AtBottom);
    DXRIBBON_QAT_GROUPBUTTON:
      ColorScheme.DrawQuickAccessToolbarGroupButton(DC, ARect,
        ViewInfo.QuickAccessToolbarViewInfo.AtBottom,
        ViewInfo.SupportNonClientDrawing, ViewInfo.IsFormCaptionActive, AState);

    DXBAR_SCREENTIP:
      ColorScheme.DrawScreenTip(DC, ARect);
    DXBAR_DROPDOWNBORDER:
      ColorScheme.DrawDropDownBorder(DC, ARect);
    DXBAR_MINITOOLBAR_BACKGROUND:
      ColorScheme.DrawMiniToolbarBackground(DC, ARect);
    DXBAR_ITEMSEPARATOR, DXBAR_ITEMSEPARATOR_VERT:
      ColorScheme.DrawItemSeparator(DC, ARect, APart = DXBAR_ITEMSEPARATOR);
    DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
      DrawTabGroupBackground(DC, ARect, AState, APart = DXBAR_TOOLBARINPOPUP);
  end;
  ColorScheme.UseRightToLeftAlignment := False;
end;

procedure TdxCustomRibbon.SkinDrawCaption(DC: HDC; const ACaption: string;
  const ARect: TRect; APart, AState: Integer);
var
  ACaptionRect: TRect;
  APrevFont: HFONT;
  APrevTextColor: TColor;
  AFont: TFont;
  AFlags: Cardinal;
begin
  if (APart = DXBAR_TOOLBAR) or (APart = DXBAR_TOOLBARINPOPUP) then
  begin
    AFont := Fonts.GetGroupHeaderFont;
    SetBkMode(DC, Windows.TRANSPARENT);
    APrevFont := SelectObject(DC, AFont.Handle);
    APrevTextColor := GetTextColor(DC);
    SetTextColor(DC, ColorToRGB(AFont.Color));
    ACaptionRect := ARect;
    Inc(ACaptionRect.Top, dxRibbonGroupCaptionHeightCorrection);
    AFlags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX;
    if UseRightToLeftReading then
      AFlags := AFlags or DT_RTLREADING;
    cxDrawText(DC, ACaption, ACaptionRect, AFlags);
    SelectObject(DC, APrevFont);
    SetTextColor(DC, APrevTextColor);
    SetBkMode(DC, OPAQUE);
  end;
end;

function TdxCustomRibbon.SkinGetCaptionRect(const ARect: TRect; APart: Integer): TRect;
var
  AOffsets: TRect;
begin
  case APart of
    DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
      begin
        AOffsets := SkinGetContentOffsets(APart);
        AOffsets := Rect(AOffsets.Left, 0, AOffsets.Right, Painter.GetGroupCaptionBottomOffset(APart = DXBAR_TOOLBARINPOPUP));
        if UseRightToLeftAlignment then
          AOffsets := TdxRightToLeftLayoutConverter.ConvertOffsets(AOffsets);
        Result := cxRectContent(ARect, AOffsets);
        Result.Top := Result.Bottom - GetGroupCaptionHeight;
      end;
    else
      Result := cxEmptyRect;
  end;
end;

function TdxCustomRibbon.SkinGetContentOffsets(APart: Integer): TRect;
begin
  case APart of
    DXBAR_APPLICATIONMENUCONTENT:
      Result := ColorScheme.GetApplicationMenuContentOffset(ViewInfo.TabsAreaViewInfo.TabsViewInfo.Bounds);

    DXBAR_TOOLBAR, DXBAR_TOOLBARINPOPUP:
      begin
        Result := ColorScheme.GetPartContentOffsets(APart);
        Inc(Result.Bottom, GetGroupCaptionHeight + Painter.GetGroupCaptionBottomOffset(APart = DXBAR_TOOLBARINPOPUP));
      end

    else
      Result := ColorScheme.GetPartContentOffsets(APart);
  end;
  TouchModeHelper.AdjustPartOffsets(APart, Result);
end;

function TdxCustomRibbon.SkinGetIsAlphaUsed(APart: Integer): Boolean;
begin
  Result := ColorScheme.GetIsAlphaUsed(APart);
end;

function TdxCustomRibbon.SkinGetName: string;
begin
  Result := ColorScheme.GetSkinName;
end;

function TdxCustomRibbon.SkinGetPartColor(APart: Integer; AState: Integer = 0): TColor;
begin
  ColorScheme.InitializePaintData(Painter);
  Result := ColorScheme.GetPartColor(APart, AState);
end;

function TdxCustomRibbon.SkinGetPartColorPalette(APart: Integer; AState: Integer = 0): IdxColorPalette;
begin
  ColorScheme.InitializePaintData(Painter);
  case APart of
    DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM:
      Result := ColorScheme.GetBackstageViewMenuButtonColorPalette(AState);
  else
    Result := nil;
  end;
end;

function TdxCustomRibbon.SkinGetPartSize(APart: Integer): Integer;
begin
  Result := ColorScheme.GetPartSize(APart);
  TouchModeHelper.AdjustPartSize(APart, Result);
end;

function TdxCustomRibbon.CanProcessMouseWheel: Boolean;
begin
  Result := IsWindowEnabled(Handle) and not Hidden and (ShowTabHeaders or ShowTabGroups) and
    not BarNavigationController.KeyTipsHandlingMode;
end;

procedure TdxCustomRibbon.FormKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not IsDestroying and HandleAllocated then
  begin
    Controller.HideHint;
    if (Key = VK_F1) and (ssCtrl in Shift) and not IsPopupGroupsMode then
    begin
      ShowTabGroups := not ShowTabGroups;
      Key := 0;
    end;
  end;
end;

function TdxCustomRibbon.GetAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  Result := IAccessibilityHelper;
end;

procedure TdxCustomRibbon.ProcessMergeOperation(ABarManager: TdxBarManager;
  AOperation: TdxBarMergeOperation; var AHandled: Boolean);
var
  ARibbon: TdxCustomRibbon;
begin
  ARibbon := FindRibbonForComponent(ABarManager);
  if AOperation = bmoUnmerge then
    AHandled := (ABarManager = nil) or (ARibbon <> nil) and (ARibbon.MergeData.MergedWith = Self)
  else
    AHandled := (ARibbon <> nil) and (ARibbon <> Self);

  if AHandled then
  begin
    case AOperation of
      bmoMerge:
        Merge(ARibbon);
      bmoUnmerge:
        Unmerge(ARibbon);
    end;
  end;
end;

procedure TdxCustomRibbon.RibbonFormNonClientDrawAdd(AObject: TObject);
begin
  if RibbonFormNonClientParts.IndexOf(AObject) = -1 then
    RibbonFormNonClientParts.Add(AObject);
end;

procedure TdxCustomRibbon.RibbonFormNonClientDrawChanged(AObject: TObject);
begin
  if not (rsSizing in FState) then
    RibbonFormInvalidate;
end;

procedure TdxCustomRibbon.RibbonFormNonClientDrawRemove(AObject: TObject);
begin
  RibbonFormNonClientParts.Remove(AObject);
end;

procedure TdxCustomRibbon.AfterApplicationMenuPopup;
var
  AHelper: IdxRibbonFormControllerHelper;
begin
  Exclude(FInternalState, risAppMenuActive);
  if Supports(RibbonForm, IdxRibbonFormControllerHelper, AHelper) then
    AHelper.DoAfterApplicationMenuPopup;
  Changed;
end;

procedure TdxCustomRibbon.AfterConstruction;
begin
  inherited AfterConstruction;
  if dxRibbonList <> nil then
    dxRibbonList.Add(Self);
end;

procedure TdxCustomRibbon.BeforeApplicationMenuPopup;
var
  AHelper: IdxRibbonFormControllerHelper;
begin
  Include(FInternalState, risAppMenuActive);
  if Supports(RibbonForm, IdxRibbonFormControllerHelper, AHelper) then
    AHelper.DoBeforeApplicationMenuPopup;
  Changed;
end;

procedure TdxCustomRibbon.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if dxRibbonList <> nil then
    dxRibbonList.Remove(Self)
end;

procedure TdxCustomRibbon.BoundsChanged;
begin
  inherited BoundsChanged;
  Changed;
end;

function TdxCustomRibbon.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  NewHeight := ViewInfo.GetRibbonHeight;
end;

function TdxCustomRibbon.CanScrollTabs: Boolean;
begin
  Result := AreGroupsVisible and not IsPopupGroupsMode and
    not ((ActiveBarControl is TdxRibbonCollapsedGroupPopupBarControl) or
    dxBarHasPopupWindowAbove(ActiveBarControl, False));
end;

procedure TdxCustomRibbon.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Fonts.ChangeScale(M, D);
  SetColorSchemeName(ColorSchemeName);
end;

procedure TdxCustomRibbon.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not(CS_HREDRAW or CS_VREDRAW);
end;

procedure TdxCustomRibbon.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if not (FIsMouseHookInstalled or IsDesigning) and IsWin2KOrLater then
  begin
    dxSetHook(htMouse, dxRibbonMouseHook);
    FIsMouseHookInstalled := True;
  end;
end;

procedure TdxCustomRibbon.DoCancelMode;
begin
  inherited DoCancelMode;
  Controller.CancelMode;
end;

procedure TdxCustomRibbon.DoMinimizeChanged;
begin
  if Assigned(OnMinimizedChanged) then OnMinimizedChanged(Self);
end;

procedure TdxCustomRibbon.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if not IsLoading then
    UpdateColorScheme;
end;

procedure TdxCustomRibbon.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if IsDesigning then
    inherited DoContextPopup(MousePos, Handled)
  else
    Handled := True;
end;

function TdxCustomRibbon.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    Result := Controller.MouseWheelDown(Shift, MousePos);
end;

function TdxCustomRibbon.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    Result := Controller.MouseWheelUp(Shift, MousePos);
end;

procedure TdxCustomRibbon.FontChanged;
begin
  BeginUpdate;
  try
    inherited FontChanged;
    Fonts.UpdateFonts;
    UpdateToolbarsFonts;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomRibbon.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  ATab: TdxRibbonTab;
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
  begin
    ATab := Tabs[I];
    if ATab.Owner = Root then
      Proc(ATab);
  end;
end;

function TdxCustomRibbon.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift) or ViewInfo.GetDesignHitTest(Point(X, Y));
end;

procedure TdxCustomRibbon.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Controller.KeyDown(Key, Shift);
  inherited;
end;

procedure TdxCustomRibbon.KeyPress(var Key: Char);
begin
  Controller.KeyPress(Key);
  inherited;
end;

procedure TdxCustomRibbon.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Controller.KeyUp(Key, Shift);
  inherited;
end;

procedure TdxCustomRibbon.Loaded;
begin
  BeginUpdate;
  try
    Tabs.UpdateBarManager(BarManager);
    inherited Loaded;
    ActiveTab := FLoadedActiveTab;
    if ActiveTab <> nil then
      ActiveTab.CheckGroupToolbarsDockControl;
    Tabs.RestoreOrder;
    Tabs.UpdateContexts;
    BarManagerSaveState;
    UpdateColorScheme;
  finally
    EndUpdate;
  end;
end;

function TdxCustomRibbon.MayFocus: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbon.Modified;
begin
  if not (rsModifiedLocked in FState) then
    inherited;
end;

procedure TdxCustomRibbon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomRibbon.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

procedure TdxCustomRibbon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Shift, X, Y);
end;

procedure TdxCustomRibbon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TdxCustomRibbon.Paint;

  procedure DoDrawToolbar(AToolbar: TdxRibbonCustomToolbar);
  var
    AControl: TWinControl;
    APoint: TPoint;
  begin
    Canvas.SaveDC;
    try
      AControl := AToolbar.Toolbar.Control;
      APoint := dxMapWindowPoint(AControl.Handle, Handle, cxNullPoint);
      AControl.PaintTo(Canvas.Canvas.Handle, APoint.X, APoint.Y);
    finally
      Canvas.RestoreDC;
    end;
  end;

begin
  ColorScheme.InitializePaintData(Painter);

  if SupportNonClientDrawing and (FormCaptionHelper <> nil) then
  begin
    if ViewInfo.UseGlass then
      Canvas.FillRect(cxRectSetHeight(ClientRect, GetRibbonFormCaptionHeight), clBlack);
    FormCaptionHelper.UpdateCaptionArea;
  end;

  ViewInfo.Paint(Canvas);

  if IsDesigning and (csPaintCopy in ControlState) and HandleAllocated then
  begin
    if ViewInfo.QuickAccessToolbarViewInfo.Visible then
      DoDrawToolbar(QuickAccessToolbar);
    if ViewInfo.TabsAreaViewInfo.IsSearchToolbarVisible then
      DoDrawToolbar(TabAreaSearchToolbar);
    if ViewInfo.TabsAreaViewInfo.IsToolbarVisible then
      DoDrawToolbar(TabAreaToolbar);
  end;
end;

function TdxCustomRibbon.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbon.Notification(AComponent: TComponent; Operation: TOperation);

  procedure Check(AToolbar: TdxRibbonCustomToolbar);
  begin
    if AToolbar <> nil then
    begin
      if (AComponent = BarManager) or (AComponent = AToolbar.Toolbar) then
        AToolbar.Toolbar := nil;
    end;
  end;

begin
  inherited Notification(AComponent, Operation);

  if ApplicationButton <> nil then
    ApplicationButton.Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if not IsDestroying then
    begin
      if (HelpButton <> nil) and (AComponent = HelpButton.ScreenTip) then
        HelpButton.ScreenTip := nil;
      Check(QuickAccessToolbar);
      Check(TabAreaSearchToolbar);
      Check(TabAreaToolbar);
    end;
    if AComponent = BarManager then
      BarManager := nil;
  end;
end;

procedure TdxCustomRibbon.ReadState(Reader: TReader);
begin
  if not (rsTabsLoaded in FState) then
  begin
    Tabs.Clear;
    Include(FState, rsTabsLoaded);
  end;
  inherited ReadState(Reader);
  FLoadedHeight := Height;
end;

function TdxCustomRibbon.RecreateWndOnBiDiModeChanged: Boolean;
begin
  Result := False;
end;

procedure TdxCustomRibbon.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);

  if not (rsTabsLoaded in FState) and IsDesigning and (Tabs.Count > 0) then
  begin
    Tabs.SetItemName(Tabs[0]);
    Tabs[0].Caption := Tabs[0].Name;
  end;
end;

procedure TdxCustomRibbon.SetParent(AParent: TWinControl);
begin
  if Assigned(AParent) then
  begin
    AParent := GetParentForm(AParent, not (csDesigning in ComponentState));
    if Assigned(AParent) and not (AParent is TCustomForm) then
      raise EdxException.CreateFmt(cxGetResourceString(@dxSBAR_RIBBONBADPARENT), [ClassName]);
  end;
  inherited SetParent(AParent);
  Top := 0;
  if FSupportNonClientDrawing and not IsLoading and (AParent <> nil) then
    UpdateNonClientDrawing;
end;

procedure TdxCustomRibbon.VisibleChanged;
begin
  if SupportNonClientDrawing and Assigned(RibbonForm) then
  begin
    RibbonForm.DisableAlign;
    try
      inherited VisibleChanged;
      UpdateNonClientDrawing;
    finally
      RibbonForm.EnableAlign;
      RibbonForm.FullUpdate;
    end;
  end
  else
    inherited VisibleChanged;
end;

procedure TdxCustomRibbon.Updating;
begin
  inherited Updating;
  Tabs.SaveContexts;
end;

procedure TdxCustomRibbon.Updated;
begin
  Tabs.RestoreContexts;
  inherited Updated;
end;

procedure TdxCustomRibbon.GetAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  inherited GetAdornerTargetElements(AList);
  for I := 0 to Tabs.Count - 1 do
    AList.AddObject(Tabs[I].Name, Tabs[I]);
end;

procedure TdxCustomRibbon.BackgroundImageChangeHandler(Sender: TObject);
begin
  UpdateColorScheme;
end;

procedure TdxCustomRibbon.CheckDrawRibbonFormStatusBarBorders(ACanvas: TcxCanvas; const ABordersWidth: TRect);
var
  AFormData: IdxRibbonFormPaintData;
  AIntf: IdxRibbonFormStatusBar;
  AIsRectangular: Boolean;
  ALeftStatusBarBounds, ARightStatusBarBounds: TRect;
  ATop, ABottom: Integer;
  APrevLayoutMode: Integer;
begin
  AFormData := Painter.GetRibbonFormPaintData;
  if (AFormData.GetState <> wsNormal) or (ApplicationMenuState >= ramsShownAsFrame) then
    Exit;
  if Supports(GetStatusBarInterface, IdxRibbonFormStatusBar, AIntf) then
  begin
    ATop := AFormData.GetBounds.Bottom - AIntf.GetHeight - GetWindowBordersWidth.Bottom;
    ABottom := AFormData.GetBounds.Bottom;
    AIsRectangular := IsRectangularFormBottom(AFormData);
    if not AIsRectangular then
      Dec(ABottom);

    APrevLayoutMode := SetLayout(ACanvas.Handle, LAYOUT_LTR);
    try
      ALeftStatusBarBounds := cxRect(0, ATop, ABordersWidth.Left, ABottom);
      ColorScheme.UseRightToLeftAlignment := UseRightToLeftAlignment;
      ColorScheme.DrawFormStatusBarPart(ACanvas.Handle, ALeftStatusBarBounds, not UseRightToLeftAlignment, AFormData.GetIsActive,
        AIntf.GetIsRaised(not UseRightToLeftAlignment), AIsRectangular);
      ACanvas.ExcludeClipRect(ALeftStatusBarBounds);

      ARightStatusBarBounds := cxRect(AFormData.GetBounds.Right - ABordersWidth.Right, ATop, AFormData.GetBounds.Right, ABottom);
      ColorScheme.DrawFormStatusBarPart(ACanvas.Handle, ARightStatusBarBounds, UseRightToLeftAlignment, AFormData.GetIsActive,
        AIntf.GetIsRaised(UseRightToLeftAlignment), AIsRectangular);
      ACanvas.ExcludeClipRect(ARightStatusBarBounds);
      ColorScheme.UseRightToLeftAlignment := False;
    finally
      SetLayout(ACanvas.Handle, APrevLayoutMode);
    end;
  end;
end;

procedure TdxCustomRibbon.DrawApplicationMenuHeader(ADC: THandle; AIsClientArea: Boolean);

  function GetOffset: TPoint;
  var
    ADestinationOrigin: TPoint;
    AWindowRect: TRect;
  begin
    AWindowRect := cxGetWindowRect(Self);
    ADestinationOrigin := ApplicationButton.IMenu.GetOrigin(AIsClientArea);
    Result := Point(AWindowRect.Left - ADestinationOrigin.X,
      AWindowRect.Top + (Height - ClientHeight) - ADestinationOrigin.Y);
  end;

var
  AOffset: TPoint;
begin
  cxPaintCanvas.BeginPaint(ADC);
  try
    AOffset := GetOffset;
    MoveWindowOrg(cxPaintCanvas.Handle, AOffset.X, AOffset.Y);
    ApplicationButtonViewInfo.Draw(cxPaintCanvas);
    MoveWindowOrg(cxPaintCanvas.Handle, -AOffset.X, -AOffset.Y);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxCustomRibbon.AddGroupButtonToQAT(ABar: TdxBar): TdxRibbonQuickAccessGroupButton;
begin
  Result := QuickAccessToolbar.AddGroupButton(ABar);
end;

function TdxCustomRibbon.CalculateFormCaptionHeight(AIsQuickAccessToolbarAtNonClientArea: Boolean): Integer;
var
  H: Integer;
begin
  if SupportNonClientDrawing and (RibbonForm <> nil) then
  begin
    //text part
    if RibbonForm.IsUseAeroNCPaint then
      H := GetDefaultWindowNCSize(RibbonForm.Handle, ScaleFactor).Top
    else
      H := Max(dxGetSystemMetrics(SM_CYCAPTION, ScaleFactor) - 1, dxGetSystemMetrics(SM_CYSIZE, ScaleFactor)) + ScaleFactor.Apply(6);

    H := Max(H, Abs(Fonts.GetFormCaptionFont.Height) * 2);

    //quick access toolbar
    if AIsQuickAccessToolbarAtNonClientArea and IsBarManagerValid then
      H := Max(H, GetGroupRowHeight + IfThen(RibbonForm.IsUseAeroNCPaint, 7, 9));
    Result := H + RibbonForm.GetCaptionHeightDelta(AIsQuickAccessToolbarAtNonClientArea);
    TouchModeHelper.AdjustFormCaptionHeight(Result);
  end
  else
    Result := 0;
end;

procedure TdxCustomRibbon.CancelUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and not IsDestroying then
    GroupsDockControlSite.SetRedraw(True);
end;

function TdxCustomRibbon.CanFade: Boolean;
begin
  Result := Fading and dxFader.IsReady and not (IsLocked or IsDesigning or TdxCustomRibbonSkinAccess(ColorScheme).LowColors);
end;

function TdxCustomRibbon.CanPaint: Boolean;
begin
  Result := ComponentState * [csLoading, csReading, csDestroying] = [];
end;

function TdxCustomRibbon.CreateApplicationButton: TdxRibbonApplicationButton;
begin
  Result := TdxRibbonApplicationButton.Create(Self);
end;

function TdxCustomRibbon.CreateController: TdxRibbonController;
begin
  Result := TdxRibbonController.Create(Self);
end;

function TdxCustomRibbon.CreateFormCaptionHelper: TdxRibbonFormCaptionHelper;
begin
  Result := TdxRibbonFormCaptionHelper.Create(Self);
end;

function TdxCustomRibbon.CreateGroupsPainter: TdxRibbonBarPainter;
begin
  Result := TdxRibbonBarPainter.Create(TdxNativeUInt(Self));
end;

function TdxCustomRibbon.CreateHelpButton: TdxRibbonHelpButton;
begin
  Result := TdxRibbonHelpButton.Create(Self);
end;

function TdxCustomRibbon.CreatePainter: TdxRibbonPainter;
begin
  Result := TdxRibbonPainter.Create(Self);
end;

function TdxCustomRibbon.CreateQuickAccessToolbar: TdxRibbonQuickAccessToolbar;
begin
  Result := TdxRibbonQuickAccessToolbar.Create(Self);
end;

function TdxCustomRibbon.CreateTabAreaSearchToolbar: TdxRibbonTabAreaSearchToolbar;
begin
  Result := TdxRibbonTabAreaSearchToolbar.Create(Self);
end;

function TdxCustomRibbon.CreateTabAreaToolbar: TdxRibbonTabAreaToolbar;
begin
  Result := TdxRibbonTabAreaToolbar.Create(Self);
end;

function TdxCustomRibbon.CreateContextAsByMerging(ASourceContext: TdxRibbonContext): TdxRibbonContext;

  function GetInsertPosition(ASourceContext: TdxRibbonContext): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Contexts.Count - 1 do
    begin
      if ASourceContext.MergeOrder >= Contexts[I].MergeOrder then
        Result := I + 1;
    end;
  end;

begin
  Result := Contexts.Insert(GetInsertPosition(ASourceContext));
  Result.FCreatedByMerging := True;
  Result.FCreatedByMergingWith := ASourceContext;
  Result.Assign(ASourceContext);
end;

function TdxCustomRibbon.CreateTabAsByMerging(ASourceTab: TdxRibbonTab): TdxRibbonTab;

  function GetInsertPosition(ASourceTab: TdxRibbonTab): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Tabs.Count - 1 do
    begin
      if ASourceTab.MergeOrder >= Tabs[I].MergeOrder then
        Result := I + 1;
    end;
  end;

  function GetTargetTabContext(ASourceContext: TdxRibbonContext): TdxRibbonContext;
  var
    AContext: TdxRibbonContext;
    I: Integer;
  begin
    Result := nil;
    if ASourceContext <> nil then
    begin
      for I := 0 to Contexts.Count - 1 do
      begin
        AContext := Contexts[I];
        if AContext.FCreatedByMerging and (AContext.FCreatedByMergingWith = ASourceContext) then
        begin
          Result := AContext;
          Break;
        end;
      end;
      if Result = nil then
        Result := CreateContextAsByMerging(ASourceContext);
    end;
  end;

begin
  Result := Tabs.Insert(GetInsertPosition(ASourceTab));
  Result.AssignCommonProperties(ASourceTab);
  Result.MergeData.CreatedByMerging := True;
  Result.Context := GetTargetTabContext(ASourceTab.Context);
end;

function TdxCustomRibbon.CreateToolbarAsByMerging: TdxBar;
begin
  Result := TdxBarManagerAccess(BarManager).CreateBarAsByMerging;
end;

function TdxCustomRibbon.CreateTouchModeHelper: TdxRibbonTouchModeHelper;
begin
  Result := TdxRibbonTouchModeHelper.Create(Self);
end;

function TdxCustomRibbon.CreateViewInfo: TdxRibbonViewInfo;
const
  ClassMap: array[TdxRibbonStyle] of TdxRibbonViewInfoClass = (
    TdxRibbonViewInfo2007,
    TdxRibbonViewInfo2010,
    TdxRibbonViewInfo2013OrNewer,
    TdxRibbonViewInfo2013OrNewer,
    TdxRibbonViewInfo2016Tablet,
    TdxRibbonViewInfo2019
  );
begin
  Result := ClassMap[Style].Create(Self);
end;

procedure TdxCustomRibbon.DesignAddTabGroup(ATab: TdxRibbonTab; ANewToolbar: Boolean);
var
  AGroup: TdxRibbonTabGroup;
begin
  if ATab = nil then
    ATab := ActiveTab;
  if (ATab = nil) or not IsDesigning then Exit;
  if ANewToolbar then
  begin
    BarManager.BeginUpdate;
    try
      AGroup := ATab.Groups.Add;
      AGroup.ToolBar := BarManager.AddToolBar;
      BarDesignController.SelectItem(AGroup.ToolBar);
    finally
      BarManager.EndUpdate;
    end;
  end
  else
    ATab.Groups.Add.DesignSelectionHelper.SelectComponent;
end;

function TdxCustomRibbon.DoApplicationMenuClick: Boolean;
begin
  Result := False;
  if Assigned(OnApplicationMenuClick) then
    OnApplicationMenuClick(Self, Result);
end;

procedure TdxCustomRibbon.DoHelpButtonClick;
begin
  if Assigned(OnHelpButtonClick) then
    OnHelpButtonClick(Self);
end;

function TdxCustomRibbon.DoHideMinimizedByClick(
  AWnd: HWND; AShift: TShiftState; const APos: TPoint): Boolean;
begin
  Result := True;
  if Assigned(OnHideMinimizedByClick) then
    OnHideMinimizedByClick(Self, AWnd, AShift, APos, Result);
end;

function TdxCustomRibbon.DoTabChanging(ANewTab: TdxRibbonTab): Boolean;
begin
  Result := True;
  if Assigned(OnTabChanging) then
    OnTabChanging(Self, ANewTab, Result);
end;

procedure TdxCustomRibbon.DoTabChanged;
begin
  if Assigned(OnTabChanged) then
    OnTabChanged(Self);
end;

procedure TdxCustomRibbon.DoTabGroupCollapsed(ATab: TdxRibbonTab; AGroup: TdxRibbonTabGroup);
begin
  if Assigned(OnTabGroupCollapsed) then
    OnTabGroupCollapsed(Self, ATab, AGroup);
end;

procedure TdxCustomRibbon.DoTabGroupExpanded(ATab: TdxRibbonTab; AGroup: TdxRibbonTabGroup);
begin
  if Assigned(OnTabGroupExpanded) then
    OnTabGroupExpanded(Self, ATab, AGroup);
end;

procedure TdxCustomRibbon.DoMoreCommandsExecute;
begin
  if Assigned(OnMoreCommandsExecute) then
    OnMoreCommandsExecute(Self)
  else
    BarManager.Customizing(True);
end;

function TdxCustomRibbon.GetVisibleTab(Index: Integer): TdxRibbonTab;
var
  I, J: Integer;
begin
  Result := nil;
  J := 0;
  for I := 0 to FTabs.Count - 1 do
    if Tabs[I].Visible then
    begin
      if J = Index then
      begin
        Result := Tabs[I];
        break;
      end;
      Inc(J);
    end;
end;

function TdxCustomRibbon.GetVisibleTabCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FTabs.Count - 1 do
    if Tabs[I].Visible then Inc(Result);
end;

procedure TdxCustomRibbon.InitCustomizePopupMenu(AItemLinks: TdxBarItemLinks);
var
  AItems: TdxRibbonPopupMenuItems;
begin
  AItems := GetValidPopupMenuItems - [rpmiItems, rpmiQATAddRemoveItem];
  PopulatePopupMenuItems(AItemLinks, AItems, PopupMenuItemClick);
end;

procedure TdxCustomRibbon.AddAffiliatedBarControlForAccessibility(ABarControl: TCustomdxBarControl);
begin
  FAffiliatedBarControlsForAccessibility.Add(ABarControl);
end;

function TdxCustomRibbon.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonAccessibilityHelper;
end;

function TdxCustomRibbon.GetBar(ACustomizingBarControl: TCustomdxBarControl): TdxBar;
var
  ABarControl: TdxRibbonCustomBarControl;
begin
  ABarControl := ACustomizingBarControl as TdxRibbonCustomBarControl;
  if ABarControl.IsPopup then
    Result := ABarControl.MasterBar
  else
    Result := ABarControl.Bar;
end;

function TdxCustomRibbon.GetBarItemControlFadingOptions: TdxFadingOptions;
begin
  Result := OptionsFading.BarItems;
end;

function TdxCustomRibbon.GetBarManager: TdxBarManager;
begin
  if IsBarManagerValid then
    Result := BarManager
  else
    Result := nil;
end;

function TdxCustomRibbon.GetBarPainter: TdxBarPainter;
begin
  Result := GroupsPainter;
end;

function TdxCustomRibbon.GetNextHorizontalAccessibleObject(ACurrentObject: IdxBarAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
var
  AIndex: Integer;
begin
  Result := nil;
  AIndex := HorizontalNavigationList.IndexOf(ACurrentObject);
  if AIndex <> -1 then
  begin
    if (ADirection = andRight) then
      Inc(AIndex)
    else
      Dec(AIndex);

    if AIndex < 0 then
      AIndex := HorizontalNavigationList.Count - 1
    else
      if AIndex > HorizontalNavigationList.Count - 1 then
        AIndex := 0;

    Result := HorizontalNavigationList[AIndex] as IdxBarAccessibilityHelper;
  end;
end;

function TdxCustomRibbon.GetTabClass: TdxRibbonTabClass;
begin
  Result := TdxRibbonTab;
end;

function TdxCustomRibbon.IsAutoHidden: Boolean;
begin
  Result := IsAutoHideModeActive and not RibbonForm.RibbonAutoHideMode.IsUIShown;
end;

function TdxCustomRibbon.IsAutoHideAnimationIsInProgress: Boolean;
begin
  Result := IsAutoHideModeActive and TdxRibbonAutoHideModeAccess(RibbonForm.RibbonAutoHideMode).IsAnimationInProgress;
end;

function TdxCustomRibbon.IsAutoHideModeActive: Boolean;
begin
  Result := (RibbonForm <> nil) and RibbonForm.RibbonAutoHideMode.Active;
end;

function TdxCustomRibbon.IsBarManagerValid: Boolean;
begin
  Result := (BarManager <> nil) and not (csDestroying in BarManager.ComponentState);
end;

function TdxCustomRibbon.IsHelpButtonPlacedOnTabsArea: Boolean;
begin
  Result := HasHelpButton and ViewInfo.IsTabsVisible and
    ((FormCaptionHelper = nil) or not FormCaptionHelper.IsRibbonHelpButtonPlacedOnCaption);
end;

function TdxCustomRibbon.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or not HandleAllocated or not CanAllocateHandle(Self) or
    ([csDestroying, csLoading] * ComponentState <> []);
end;

function TdxCustomRibbon.IsMerged: Boolean;
begin
  Result :=
    TabAreaToolbar.IsMerged or TabAreaSearchToolbar.IsMerged or
    QuickAccessToolbar.IsMerged or Tabs.IsMerged or
   (MergeData.MergedWith <> nil) or (MergeData.Children.Count > 0);
end;

function TdxCustomRibbon.IsQuickAccessToolbarValid: Boolean;
begin
  Result := QuickAccessToolbar.Visible and (QuickAccessToolbar.Toolbar <> nil);
end;

function TdxCustomRibbon.IsSimplifiedGroupsLayout: Boolean;
begin
  Result := Style = rs2016Tablet;
end;

procedure TdxCustomRibbon.InitializeRibbonForm;
begin
  if not IsDesigning then
  begin
    if RibbonForm <> nil then
      RibbonForm.RibbonControl := Self;
  end;
end;

procedure TdxCustomRibbon.InternalCloseTabGroupsPopupWindow(AAllowAnimation: Boolean);
begin
  if IsPopupGroupsMode then
  begin
    if not AAllowAnimation then
      TabGroupsPopupWindow.AllowShowHideAnimation := False;
    TabGroupsPopupWindow.CloseUp;
  end;
end;

procedure TdxCustomRibbon.PopulatePopupMenuItems(ALinks: TdxBarItemLinks; AItems: TdxRibbonPopupMenuItems; AOnClick: TNotifyEvent);

  function AddMenuItem(const ACaption: string; ANeedSeparator: Boolean;
    AItem: TdxRibbonPopupMenuItem; AData: TObject = nil): TdxBarButton;
  begin
    Result := TdxBarButton.Create(BarManager);
    BarDesignController.AddInternalItem(Result, FInternalItems);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;
    Result.Tag := Ord(AItem);
    Result.Data := AData;
    ALinks.Add(Result).BeginGroup := ANeedSeparator;
  end;

var
  ANeedSeparator: Boolean;
  AButton: TdxBarButton;
begin
  FInternalItems.Clear;
  if not (rpmiItems in AItems) then
    ALinks.Clear;
  if rpmiQATAddRemoveItem in AItems then
    if QuickAccessToolbar.Contains(BarDesignController.CustomizingItemLink) then
      AddMenuItem(cxGetResourceString(@dxSBAR_REMOVEFROMQAT), False, rpmiQATAddRemoveItem)
    else
      AddMenuItem(GetRibbonQATOptionAddItemCaption, False, rpmiQATAddRemoveItem,
        BarDesignController.CustomizingBarControl).Enabled := GetRibbonQATOptionAddItemEnabled;
  ANeedSeparator := ALinks.Count > 0;
  if rpmiMoreCommands in AItems then
  begin
    AddMenuItem(cxGetResourceString(@dxSBAR_MORECOMMANDS), ANeedSeparator, rpmiMoreCommands);
    ANeedSeparator := False;
  end;
  if rpmiCustomizeQAT in AItems then
  begin
    AddMenuItem(cxGetResourceString(@dxSBAR_CUSTOMIZERIBBONQAT), ANeedSeparator, rpmiCustomizeQAT);
    ANeedSeparator := False;
  end;
  if rpmiQATPosition in AItems then
    AddMenuItem(GetRibbonQATPositionButtonCaption, ANeedSeparator, rpmiQATPosition);
  ANeedSeparator := True;
  if rpmiCustomizeRibbon in AItems then
  begin
    AddMenuItem(cxGetResourceString(@dxSBAR_CUSTOMIZERIBBON), ANeedSeparator, rpmiCustomizeRibbon);
    ANeedSeparator := False;
  end;
  if rpmiMinimizeRibbon in AItems then
  begin
    AButton := AddMenuItem(cxGetResourceString(@dxSBAR_MINIMIZERIBBON), ANeedSeparator, rpmiMinimizeRibbon);
    if not ShowTabGroups then
    begin
      AButton.ButtonStyle := bsChecked;
      AButton.Down := True;
    end;
    AButton.Enabled := ViewInfo.TabsAreaViewInfo.TabsViewInfo.Count > 0;
  end;
end;

procedure TdxCustomRibbon.PopupMenuItemClick(Sender: TObject);

  procedure AddItemToQAT;
  var
    ACustomizingLink: TdxBarItemLink;
  begin
    ACustomizingLink := BarDesignController.CustomizingItemLink;
    if ACustomizingLink <> nil then
      QuickAccessToolbar.Toolbar.ItemLinks.Add.Item := ACustomizingLink.Item
    else
      AddGroupButtonToQAT(GetBar(TCustomdxBarControl(TdxBarItem(Sender).Data)));
  end;

  procedure RemoveItemFromQAT;
  var
    ACustomizingLink: TdxBarItemLink;
  begin
    ACustomizingLink := BarDesignController.CustomizingItemLink;
    if ACustomizingLink.Item is TdxRibbonQuickAccessGroupButton then
      BarDesignController.DeleteCustomizingItem
    else
    begin
      if ACustomizingLink.OriginalItemLink <> nil then
        ACustomizingLink.OriginalItemLink.Free
      else
        BarDesignController.DeleteCustomizingItemLink;
    end;
  end;

begin
  case TdxRibbonPopupMenuItem(TdxBarButton(Sender).Tag) of
    rpmiMinimizeRibbon:
      ShowTabGroups := not ShowTabGroups;
    rpmiMoreCommands:
      DoMoreCommandsExecute;
    rpmiCustomizeRibbon:
      ShowCustomizationForm(rcfmCustomizeRibbon);
    rpmiCustomizeQAT:
      ShowCustomizationForm(rcfmCustomizeQuickAccessToolbar);
    rpmiQATAddRemoveItem:
      if QuickAccessToolbar.Contains(BarDesignController.CustomizingItemLink) then
        RemoveItemFromQAT
      else
        AddItemToQAT;

    rpmiQATPosition:
      if QuickAccessToolbar.Position = qtpAboveRibbon then
        QuickAccessToolbar.Position := qtpBelowRibbon
      else
        QuickAccessToolbar.Position := qtpAboveRibbon;
  end;
end;

procedure TdxCustomRibbon.SetRedraw(ARedraw: Boolean);
begin
  if HandleAllocated and Visible then
  begin
    if ARedraw then
    begin
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      FullInvalidate;
    end
    else
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
  end;
end;

procedure TdxCustomRibbon.ShowCustomizePopup;
begin
  if IsBarManagerValid then
    BarDesignController.ShowCustomCustomizePopup(BarManager, InitCustomizePopupMenu, GroupsPainter);
end;

procedure TdxCustomRibbon.UpdateActiveTab;
begin
  if ActiveTab <> nil then
    ActiveTab.UpdateColorScheme;
end;

procedure TdxCustomRibbon.UpdateHorizontalNavigationList;
begin
  FHorizontalNavigationList.Clear;
  ViewInfo.PopulateHorizontalNavigationList(
    procedure (AIntf: IdxBarAccessibilityHelper)
    begin
      if AIntf.GetHelper.Selectable then
        FHorizontalNavigationList.Add(AIntf);
    end);
end;

procedure TdxCustomRibbon.UpdateHeight;
begin
  if not IsLoading then
  begin
    if Hidden and not IsDesigning then
      Height := ViewInfo.GetNonClientAreaHeight
    else
      Height := ViewInfo.GetRibbonHeight;
  end;
end;

procedure TdxCustomRibbon.UpdateHiddenActiveTabDockControl;
begin
  if ActiveTab <> nil then
    ActiveTab.UpdateDockControl;
end;

procedure TdxCustomRibbon.UpdateFormActionControl(ASetControl: Boolean);
var
  ARibbonForm: TdxCustomRibbonForm;
begin
  ARibbonForm := GetRibbonForm;
  if ARibbonForm <> nil then
  begin
    if ASetControl then
    begin
      if (ARibbonForm.FormStyle = fsMDIForm) and Assigned(ARibbonForm.ActiveMDIChild) then
        ARibbonForm.PrevActiveControl := ARibbonForm.ActiveMDIChild.ActiveControl
      else
        ARibbonForm.PrevActiveControl := ARibbonForm.ActiveControl
    end
    else
      ARibbonForm.PrevActiveControl := nil;
  end;
end;

function TdxCustomRibbon.GetActiveTab: TdxRibbonTab;
begin
  if csLoading in ComponentState then
    Result := FLoadedActiveTab
  else
    Result := FActiveTab;
end;

function TdxCustomRibbon.GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
begin
  Result := ViewInfo.ApplicationButtonViewInfo;
end;

function TdxCustomRibbon.GetColorSchemeName: string;
begin
  if FColorScheme <> nil then
    Result := FColorScheme.Name
  else
    Result := '';
end;

function TdxCustomRibbon.GetIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := GetAccessibilityHelperClass.Create(Self);
  Result := FIAccessibilityHelper;
end;

function TdxCustomRibbon.GetTabsIAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  Result := Tabs.IAccessibilityHelper;
end;

function TdxCustomRibbon.GetIniSection(const ADelimiter: string; const ASection: string): string;
var
  AOwner: TComponent;
begin
  Result := Name;
  AOwner := Owner;
  while (AOwner <> nil) and (AOwner.Name <> '') do
  begin
    Result := AOwner.Name + '_' + Result;
    AOwner := AOwner.Owner;
  end;

  Result := ASection + Result;
end;

function TdxCustomRibbon.GetIsPopupGroupsMode: Boolean;
begin
  Result := (TabGroupsPopupWindow <> nil) and TabGroupsPopupWindow.IsVisible;
end;

function TdxCustomRibbon.GetNextActiveTab(ATab: TdxRibbonTab): TdxRibbonTab;

  function GetIndex(ATab: TdxRibbonTab): Integer;
  begin
    if ATab = nil then
      Result := 0
    else
      if not (csDestroying in ATab.ComponentState) then
        Result := ATab.Index
      else
        Result := ATab.LastIndex;
  end;

var
  I, AIndex: Integer;
begin
  Result := nil;
  if not IsDestroying then
  begin
    AIndex := GetIndex(ATab);
    for I := AIndex to TabCount - 1 do
      if Tabs[I].IsVisible then
        Exit(Tabs[I]);

    for I := AIndex - 1 downto 0 do
      if Tabs[I].IsVisible then
        Exit(Tabs[I]);
  end;
end;

function TdxCustomRibbon.GetRibbonForm: TdxCustomRibbonForm;
begin
  if Owner is TdxCustomRibbonForm then
    Result := TdxCustomRibbonForm(Owner)
  else
    Result := nil;
end;

function TdxCustomRibbon.GetStatusBarInterface: IUnknown;
var
  AForm: TCustomForm;
  AIntf: IdxRibbonFormStatusBar;
  I: Integer;
begin
  Result := nil;
  if SupportNonClientDrawing and not ViewInfo.UseGlass then
  begin
    AForm := RibbonForm;
    if (AForm <> nil) and AForm.HandleAllocated then
      for I := 0 to RibbonFormNonClientParts.Count - 1 do
      begin
        if Supports(RibbonFormNonClientParts[I], IdxRibbonFormStatusBar, AIntf) and AIntf.GetActive(AForm) then
        begin
          Result := AIntf;
          Break;
        end;
      end;
  end;
end;

function TdxCustomRibbon.GetStyle: TdxRibbonStyle;
begin
  Result := ColorScheme.Style;
end;

function TdxCustomRibbon.GetTabCount: Integer;
begin
  if FTabs <> nil then
    Result := FTabs.Count
  else
    Result := 0;
end;

procedure TdxCustomRibbon.InitColorScheme;
begin
  ColorScheme := dxRibbonSkinsManager.Default;
end;

function TdxCustomRibbon.IsOnRibbonMDIForm: Boolean;
var
  AForm: TForm;
begin
  AForm := RibbonForm;
  Result := (AForm <> nil) and (AForm.FormStyle = fsMDIForm);
end;

procedure TdxCustomRibbon.RecreateViewInfo;
begin
  if not IsDestroying then
  begin
    FreeAndNil(FViewInfo);
    FViewInfo := CreateViewInfo;
  end;
end;

procedure TdxCustomRibbon.RibbonFormInvalidate;
begin
  if SupportNonClientDrawing then
    WinControlFullInvalidate(RibbonForm);
end;

procedure TdxCustomRibbon.SetActiveTab(Value: TdxRibbonTab);
begin
  if csLoading in ComponentState then
    FLoadedActiveTab := Value
  else
    if (FActiveTab <> Value) and not (csDestroying in ComponentState) then
    begin
      if not IsDesigning then
      begin
        if IsBarManagerValid then
          BarManager.HideAll;
        dxFader.Remove(FActiveTab);
        dxFader.Remove(Value);
        if not DoTabChanging(Value) then Exit;
      end;
      CloseTabGroupsPopupWindow;
      if FActiveTab <> nil then
        FActiveTab.Deactivate;
      FPreviousActiveTab := FActiveTab;
      FActiveTab := Value;
      GroupsDockControlSite.SetRedraw(False);
      try
        if FActiveTab <> nil then
          FActiveTab.Activate;
        Changed;
        DoTabChanged;
      finally
        Changed;
        GroupsDockControlSite.SetRedraw(True);
        if HandleAllocated then
          SendMessage(Handle, DXM_UIADORNERMANAGERUPDATE, 0, 0);
      end;
      Modified;
    end;
end;

procedure TdxCustomRibbon.SetApplicationButton(AValue: TdxRibbonApplicationButton);
begin
  FApplicationButton.Assign(AValue);
end;

procedure TdxCustomRibbon.SetBackgroundImage(AValue: TdxSmartGlyph);
begin
  FBackgroundImage.Assign(AValue);
end;

procedure TdxCustomRibbon.SetBarManager(Value: TdxBarManager);

  procedure RemoveChangeHandler(AHandlerCollection: TcxEventHandlerCollection; AEvent: TcxEventHandler);
  begin
    if AHandlerCollection <> nil then
      AHandlerCollection.Remove(AEvent);
  end;

var
  ALockedBarManager: TdxBarManager;
begin
  if FBarManager <> Value then
  begin
    ALockedBarManager := nil;
    BarManagerBeforeChange;
    try
      if FBarManager <> nil then
      begin
        if FBarManager.MergeOperationHandlers <> nil then
          FBarManager.MergeOperationHandlers.Remove(Self);
        RemoveChangeHandler(FBarManager.MDIStateChangedHandlers, MDIStateChanged);
        RemoveChangeHandler(FBarManager.SystemFontChangedHandlers, SystemFontChanged);
        RemoveChangeHandler(FBarManager.ReadIniFileHandlers, BarManagerLoadIni);
        RemoveChangeHandler(FBarManager.WriteIniFileHandlers, BarManagerSaveIni);
        FBarManager.RemoveFreeNotification(Self);
        if (Value = nil) and IsBarManagerValid then
          ALockedBarManager := FBarManager;
      end;

      FBarManager := Value;

      if IsBarManagerValid then
      begin
        FBarManager.FreeNotification(Self);
        FBarManager.MergeOperationHandlers.Add(Self);
        FBarManager.MDIStateChangedHandlers.Add(MDIStateChanged);
        FBarManager.SystemFontChangedHandlers.Add(SystemFontChanged);
        FBarManager.ReadIniFileHandlers.Add(BarManagerLoadIni);
        FBarManager.WriteIniFileHandlers.Add(BarManagerSaveIni);
        Fonts.UpdateFonts;
      end;

      if Assigned(ALockedBarManager) then
        ALockedBarManager.BeginUpdate;
      try
        Tabs.UpdateBarManager(Value);
        QuickAccessToolbar.DockControl.BarManager := Value;
        TabAreaSearchToolbar.DockControl.BarManager := Value;
        TabAreaToolbar.DockControl.BarManager := Value;
      finally
        if Assigned(ALockedBarManager) then
          ALockedBarManager.EndUpdate;
      end;
    finally
      BarManagerAfterChange;
      Changed;
    end;
  end;
end;

procedure TdxCustomRibbon.SetCapitalizeTabCaptions(const Value: TdxDefaultBoolean);
begin
  if FCapitalizeTabCaptions <> Value then
  begin
    FCapitalizeTabCaptions := Value;
    InvalidateWithChildren;
  end;
end;

procedure TdxCustomRibbon.SetColorScheme(const AName: string; AStyle: TdxRibbonStyle);
begin
  ColorScheme := dxRibbonSkinsManager.GetMostSuitable(AName, AStyle, ScaleFactor.Apply(dxDefaultDPI));
end;

procedure TdxCustomRibbon.SetColorScheme(Value: TdxCustomRibbonSkin);
begin
  if (FColorScheme <> Value) and ((Value <> nil) or IsDestroying) then
  begin
    if FColorScheme <> nil then
    begin
      FColorScheme.InitializePaintData(nil);
      FColorScheme.RemoveReference;
      FColorScheme := nil;
    end;
    if Value <> nil then
    begin
      FColorScheme := Value;
      FColorScheme.AddReference;
    end;
    RecreateViewInfo;
    if not (IsLoading or IsDestroying) then
    begin
      UpdateColorScheme;
      Modified;
    end;
  end;
end;

procedure TdxCustomRibbon.SetColorSchemeAccent(AValue: TdxRibbonColorSchemeAccent);
begin
  if AValue <> FColorSchemeAccent then
  begin
    FColorSchemeAccent := AValue;
    UpdateColorScheme;
  end;
end;

procedure TdxCustomRibbon.SetColorSchemeName(const Value: string);
begin
  SetColorScheme(Value, Style);
end;

procedure TdxCustomRibbon.SetContexts(const Value: TdxRibbonContexts);
begin
  FContexts.Assign(Value);
end;

procedure TdxCustomRibbon.SetDocumentName(const Value: TCaption);
var
  AForm: TCustomForm;
begin
  if FDocumentName <> Value then
  begin
    FDocumentName := Value;
    AForm := RibbonForm;
    if (AForm <> nil) and (Application.MainForm = AForm) and (AForm.HandleAllocated) then
      TdxFormHelper.SetWindowTextWithoutRedrawing(AForm.Handle, ViewInfo.GetFormCaptionText);
    Changed;
  end;
end;

procedure TdxCustomRibbon.SetEnableTabAero(AValue: Boolean);
var
  ARibbonForm: TdxCustomRibbonForm;
begin
  if AValue <> FEnableTabAero then
  begin
    FEnableTabAero := AValue;
    ARibbonForm := RibbonForm;
    if ARibbonForm <> nil then
      ARibbonForm.UpdateBorders;
  end;
end;

procedure TdxCustomRibbon.SetFading(const Value: Boolean);
begin
  if FFading <> Value then
  begin
    FFading := Value;
    if not Value then
      dxFader.Clear;
    Invalidate;
  end;
end;

procedure TdxCustomRibbon.SetFonts(const Value: TdxRibbonFonts);
begin
  FFonts.Assign(Value);
end;

procedure TdxCustomRibbon.SetHelpButton(const Value: TdxRibbonHelpButton);
begin
  FHelpButton.Assign(Value);
end;

procedure TdxCustomRibbon.SetHelpButtonScreenTip(const Value: TdxScreenTip);
begin
  HelpButton.ScreenTip := Value;
end;

procedure TdxCustomRibbon.SetHighlightedTab(const Value: TdxRibbonTab);

  procedure UpdateTab(ATab: TdxRibbonTab; AState: TdxFadingState);
  begin
    if ATab <> nil then
    begin
      if CanFade then
        dxFader.Fade(ATab, AState)
      else
        ATab.Invalidate;
    end;
  end;

begin
  if FHighlightedTab <> Value then
  begin
    UpdateTab(FHighlightedTab, fsFadeOut);
    FHighlightedTab := Value;
    UpdateTab(FHighlightedTab, fsFadeIn);
    ViewInfo.UpdateButtonsStates;
  end;
end;

procedure TdxCustomRibbon.SetOnHelpButtonClick(const Value: TdxRibbonEvent);
begin
  FOnHelpButtonClick := Value;
  Changed;
  if RibbonForm <> nil then
    RibbonForm.UpdateBorders;
end;

procedure TdxCustomRibbon.SetOptionsFading(AValue: TdxRibbonOptionsFading);
begin
  FOptionsFading.Assign(AValue);
end;

procedure TdxCustomRibbon.SetPopupMenuItems(const Value: TdxRibbonPopupMenuItems);
begin
  if Value <> FPopupMenuItems then
  begin
    FPopupMenuItems := Value;
    Changed;
  end;
end;

procedure TdxCustomRibbon.SetQuickAccessToolbar(const Value: TdxRibbonQuickAccessToolbar);
begin
  FQuickAccessToolbar.Assign(Value);
end;

procedure TdxCustomRibbon.SetShowMinimizeButton(const Value: Boolean);
begin
  if FShowMinimizeButton <> Value then
  begin
    FShowMinimizeButton := Value;
    Changed;
  end;
end;

procedure TdxCustomRibbon.SetShowTabGroups(const Value: Boolean);
begin
  if FShowTabGroups <> Value then
  begin
    FShowTabGroups := Value;
    InternalCloseTabGroupsPopupWindow(False);
    if not ShowTabGroups then
    begin
      if RibbonForm <> nil then
        RibbonForm.RibbonAutoHideMode.Active := False;
      if ActiveTab <> nil then
        ActiveTab.DockControl.Visible := False;
    end;
    DoMinimizeChanged;
    Changed;
  end;
end;

procedure TdxCustomRibbon.SetShowTabHeaders(const Value: Boolean);
begin
  if FShowTabHeaders <> Value then
  begin
    FShowTabHeaders := Value;
    InternalCloseTabGroupsPopupWindow(False);
    UpdateColorScheme;
  end;
end;

procedure TdxCustomRibbon.SetStyle(AValue: TdxRibbonStyle);
begin
  SetColorScheme(ColorSchemeName, AValue);
end;

procedure TdxCustomRibbon.SetSupportNonClientDrawing(const Value: Boolean);
begin
  if FSupportNonClientDrawing <> Value then
  begin
    if ComponentState * [csReading, csDesigning, csDestroying] = [] then
      Application.ProcessMessages;
    FSupportNonClientDrawing := Value;
    UpdateNonClientDrawing;
  end;
end;

procedure TdxCustomRibbon.SetTabAreaSearchToolbar(const Value: TdxRibbonTabAreaSearchToolbar);
begin
  FTabAreaSearchToolbar := Value;
end;

procedure TdxCustomRibbon.SetTabs(Value: TdxRibbonTabCollection);
begin
  FTabs.Assign(Value);
end;

procedure TdxCustomRibbon.SetTabAreaToolbar(const Value: TdxRibbonTabAreaToolbar);
begin
  FTabAreaToolbar := Value;
end;

procedure TdxCustomRibbon.UpdateColorSchemeListeners;
begin
  FColorSchemeHandlers.CallEvents(Self, []);
end;

procedure TdxCustomRibbon.UpdateNonClientDrawing;
var
  AForm: TdxCustomRibbonForm;
begin
  AForm := RibbonForm;
  if AForm <> nil then
  begin
    CloseTabGroupsPopupWindow;
    ViewInfo.UpdateNonClientParams;

    if ViewInfo.SupportNonClientDrawing then
    begin
      if FFormCaptionHelper = nil then
        FFormCaptionHelper := CreateFormCaptionHelper;
    end
    else
      FreeAndNil(FFormCaptionHelper);

    AForm.DisableAlign;
    try
      AForm.RibbonNonClientHelper := FormCaptionHelper;
      Changed;
    finally
      AForm.EnableAlign;
    end;
    AForm.UpdateColorScheme;
    AForm.FullUpdate;
  end
  else
    FreeAndNil(FFormCaptionHelper);
end;

procedure TdxCustomRibbon.UpdateToolbarsFonts;
var
  I: Integer;
begin
  GroupsPainter.CalculateDrawParams;
  for I := 0 to TabCount - 1 do
    Tabs[I].UpdateGroupsFont;

  TabAreaToolbar.DockControl.UpdateBarControlsFont;
  TabAreaSearchToolbar.DockControl.UpdateBarControlsFont;
  QuickAccessToolbar.DockControl.UpdateBarControlsFont;
end;

procedure TdxCustomRibbon.WMPaint(var Message: TWMPaint);
begin
  if CanPaint then
  begin
    if not FDoubleBuffered or (Message.DC <> 0) then
    begin
      if not (csCustomPaint in ControlState) and (ControlCount = 0) then
        inherited
      else
        PaintHandler(Message);
    end
    else
      dxBufferedPaintControl(Self);
  end;
end;

procedure TdxCustomRibbon.CMSelectAppMenuFirstItemControl(var Message: TMessage);
begin
  if Controller.IsApplicationMenuDropped then
    ApplicationButton.IMenu.SelectAppMenuFirstItemControl;
end;

procedure TdxCustomRibbon.CMShowKeyTips(var Message: TMessage);
begin
  if Controller.IsApplicationMenuDropped then
    ApplicationButton.IMenu.ShowKeyTips;
end;

procedure TdxCustomRibbon.BarManagerAfterChange;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxRibbonListener).AfterBarManagerChange;
end;

procedure TdxCustomRibbon.BarManagerBeforeChange;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxRibbonListener).BeforeBarManagerChange;
end;

procedure TdxCustomRibbon.BarManagerLoadIni(Sender: TObject; const AEventArgs);
var
  ASection: string;
  AEventData: TdxBarIniFileEventData;
begin
  AEventData := TdxBarIniFileEventData(AEventArgs);
  ASection := GetIniSection(AEventData.Delimiter, AEventData.BaseSection);
  if AEventData.IniFile.SectionExists(ASection) then
  begin
    QuickAccessToolbar.Position := TdxQuickAccessToolbarPosition(
      AEventData.IniFile.ReadInteger(ASection, 'QuickAccessToolbarPosition', 0));
    ShowTabGroups := AEventData.IniFile.ReadBool(ASection, 'ShowTabGroups', True);
    if AEventData.IniFile.ValueExists(ASection, 'TabCount') then
      LoadTabsFromIni(AEventData);
  end;
  QuickAccessToolbar.BarManagerLoadIni(AEventData);
end;

procedure TdxCustomRibbon.BarManagerSaveIni(Sender: TObject; const AEventArgs);
var
  ASection: string;
  AEventData: TdxBarIniFileEventData;
begin
  AEventData := TdxBarIniFileEventData(AEventArgs);
  ASection := GetIniSection(AEventData.Delimiter, AEventData.BaseSection);
  AEventData.IniFile.WriteInteger(ASection, 'QuickAccessToolbarPosition', Ord(QuickAccessToolbar.Position));
  AEventData.IniFile.WriteBool(ASection, 'ShowTabGroups', ShowTabGroups);
  QuickAccessToolbar.BarManagerSaveIni(AEventData);
  SaveTabsToIni(AEventData);
end;

procedure TdxCustomRibbon.BarManagerSaveState;
begin
  if BarManager <> nil then
    TdxBarManagerAccess(BarManager).SaveState;
end;

procedure TdxCustomRibbon.LoadTabsFromIni(AEventData: TdxBarIniFileEventData);
var
  ASource: TCustomIniFile;
  ADelimiter, ASection, ATabSection, ATabName: string;
  ATab: TdxRibbonTab;
  ATabCount, I: Integer;
begin
  BeginUpdate;
  try
    ASource := AEventData.IniFile;
    ADelimiter := AEventData.Delimiter;
    ASection := GetIniSection(ADelimiter, AEventData.BaseSection);
    for I := 0 to TabCount - 1 do
      Tabs[I].Unmerge;
    ATabCount := ASource.ReadInteger(ASection, 'TabCount', 0);
    for I := 0 to ATabCount - 1 do
    begin
      ATabSection := Tabs.GetIniSection(ASection, ADelimiter, I);
      ATabName := ASource.ReadString(ATabSection, 'Name', EmptyStr);
      ATab := Tabs.FindByComponentName(ATabName);
      if ATab = nil then
        ATab := Tabs.Add;
      ATab.Index := I;
      Tabs[I].LoadFromIni(ASource, ASection, ADelimiter);
    end;
    for I := ATabCount to TabCount - 1 do
    begin
      ATab := Tabs[I];
      ATab.ChangeAllGroupsVisibility(True);
      ATab.Visible := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomRibbon.SaveTabsToIni(AEventData: TdxBarIniFileEventData);
var
  ASource: TCustomIniFile;
  ADelimiter, ASection: string;
  I: Integer;
begin
  ASource := AEventData.IniFile;
  ADelimiter := AEventData.Delimiter;
  ASection := GetIniSection(ADelimiter, AEventData.BaseSection);
  ASource.WriteInteger(ASection, 'TabCount', TabCount);
  for I := 0 to TabCount - 1 do
    if not Tabs[I].MergeData.CreatedByMerging then
      Tabs[I].SaveToIni(ASource, ASection, ADelimiter);
end;

procedure TdxCustomRibbon.MDIStateChanged(Sender: TObject; const AEventArgs);
var
  AEventData: TdxBarMDIStateChangeEventData;
begin
  if not IsDesigning and IsOnRibbonMDIForm then
  begin
    AEventData := TdxBarMDIStateChangeEventData(AEventArgs);
    if AEventData.Change in [scMaximizedChanged, scChildActivated] then
    begin
      BeginUpdate;
      try
        if IsZoomed(AEventData.Wnd) then
          DocumentName := cxGetWindowText(AEventData.Wnd)
        else
          DocumentName := '';

        Application.Title := GetTaskbarCaption;
      finally
        CancelUpdate;
      end;
      if HandleAllocated then
        PostMessage(Handle, DXM_RIBBON_RECALCULATE, 0, 0)
      else
        Changed;
    end;
  end;
end;

procedure TdxCustomRibbon.Merge(ARibbon: TdxCustomRibbon;
  AMergeOptions: TdxRibbonMergeOptions = dxRibbonDefaultMergeOptions);

  function CanMergeTab(ASourceTab, ATargetTab: TdxRibbonTab): Boolean;
  begin
    Result := (ASourceTab.MergeKind = ATargetTab.MergeKind) and
      (ATargetTab.Context = nil) and (ASourceTab.Caption = ATargetTab.Caption);
  end;

  function FindTargetTab(ASourceTab: TdxRibbonTab; out ATargetTab: TdxRibbonTab): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Tabs.Count - 1 do
    begin
      Result := CanMergeTab(ASourceTab, Tabs[I]);
      if Result then
      begin
        ATargetTab := Tabs[I];
        Break;
      end;
    end;
  end;

var
  ATab: TdxRibbonTab;
  ATargetTab: TdxRibbonTab;
  I: Integer;
begin
  if (ARibbon = nil) or (ARibbon = Self) then
    RaiseMergingError(@dxSBAR_RIBBONCANTMERGE);
  if not (ARibbon.IsBarManagerValid and IsBarManagerValid) then
    RaiseMergingError(@dxSBAR_RIBBONCANTMERGEWITHOUTBARMANAGER);
  if ARibbon.IsMerged then
    RaiseMergingError(@dxSBAR_RIBBONONEOFTABGROUPSALREADYMERGED);

  BeginUpdate;
  try
    MergeData.Children.Add(ARibbon);
    ARibbon.MergeData.MergedWith := Self;
    ARibbon.MergeData.ActiveTabBeforeMerging := ARibbon.ActiveTab;
    ARibbon.MergeData.VisibleBeforeMerging := ARibbon.Visible;
    ARibbon.Visible := False;

    for I := 0 to ARibbon.Tabs.Count - 1 do
    begin
      ATab := ARibbon.Tabs[I];
      if ATab.IsMergeAllowed and (ATab.MergeKind = rmkMerge) then
      begin
        if FindTargetTab(ATab, ATargetTab) then
          ATargetTab.Merge(ATab, AMergeOptions);
      end;
    end;

    if rmoCanCreateNewTab in AMergeOptions then
    begin
      for I := 0 to ARibbon.Tabs.Count - 1 do
      begin
        ATab := ARibbon.Tabs[I];
        if ATab.IsMergeAllowed and not ATab.IsMerged then
        begin
          ATargetTab := CreateTabAsByMerging(ATab);
          ATargetTab.Merge(ATab, AMergeOptions);
          if ATargetTab.Groups.Count = 0 then
            ATargetTab.Free;
        end;
      end;
    end;

    TabAreaToolbar.Merge(ARibbon.TabAreaToolbar, AMergeOptions);
    TabAreaSearchToolbar.Merge(ARibbon.TabAreaSearchToolbar, AMergeOptions);
    QuickAccessToolbar.Merge(ARibbon.QuickAccessToolbar, AMergeOptions);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomRibbon.Unmerge(ARibbon: TdxCustomRibbon = nil);

  procedure InternalUnmerge(ARibbon: TdxCustomRibbon);
  var
    I: Integer;
  begin
    MergeData.Children.Remove(ARibbon);
    ARibbon.MergeData.MergedWith := nil;
    if not ARibbon.IsDestroying then
    begin
      ARibbon.ActiveTab := ARibbon.MergeData.ActiveTabBeforeMerging;
      ARibbon.Visible := ARibbon.MergeData.VisibleBeforeMerging;
    end;
    TabAreaToolbar.Unmerge(ARibbon.TabAreaToolbar);
    TabAreaSearchToolbar.Unmerge(ARibbon.TabAreaSearchToolbar);
    QuickAccessToolbar.Unmerge(ARibbon.QuickAccessToolbar);
    for I := 0 to ARibbon.Tabs.Count - 1 do
      Tabs.InternalUnmerge(ARibbon.Tabs[I]);
  end;

var
  I: Integer;
begin
  if (ARibbon = Self) then
    RaiseMergingError(@dxSBAR_RIBBONCANTUNMERGE);
  if (ARibbon <> nil) and (ARibbon.MergeData.MergedWith = nil) then
    Exit;
  if (ARibbon <> nil) and (ARibbon.MergeData.MergedWith <> Self) then
    RaiseMergingError(@dxSBAR_RIBBONSARENOTMERGED, Self, ARibbon);
  if MergeData.Children.Count = 0 then
    Exit;

  BeginUpdate;
  try
    if ARibbon <> nil then
      InternalUnmerge(ARibbon)
    else
      for I := MergeData.Children.Count - 1 downto 0 do
        InternalUnmerge(TdxCustomRibbon(MergeData.Children[I]));
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomRibbon.UnmergeBeforeDestroy;
begin
  if MergeData.MergedWith <> nil then
    MergeData.MergedWith.Unmerge(Self);
  Unmerge;
end;

procedure TdxCustomRibbon.SystemFontChanged(Sender: TObject; const AEventArgs);
begin
  FontChanged;
end;

procedure TdxCustomRibbon.UpdateColorScheme;
var
  AForm: TCustomForm;
  AValid: Boolean;
begin
  Include(FState, rsUpdatingColorScheme);
  try
    AForm := GetParentForm(Self, False);
    AValid := Assigned(AForm) and AForm.HandleAllocated and IsWindowVisible(AForm.Handle);
    if AValid then
      SendMessage(AForm.Handle, WM_SETREDRAW, 0, 0);
    try
      Fonts.UpdateFonts;
      GroupsPainter.CalculateDrawParams;
      ColorScheme.FlushCache;
      ColorScheme.UpdateBitsPerPixel;
      RecalculateBars;
      Changed;
    finally
      UpdateColorSchemeListeners;
      if SupportNonClientDrawing then
        RecalculateNonClient(AForm);
      if AValid then
      begin
        SendMessage(AForm.Handle, WM_SETREDRAW, 1, 0);
        WinControlFullInvalidate(AForm, True, True);
      end;
      if RibbonForm <> nil then
        RibbonForm.UpdateColorScheme;
    end;
  finally
    Exclude(FState, rsUpdatingColorScheme);
  end;
end;

{ TdxRibbonQuickAccessToolbarHelper }

class function TdxRibbonQuickAccessToolbarHelper.HasGroupButtonForToolbar(
  AQATLinks: TdxBarItemLinks; AToolbar: TdxBar): Boolean;
var
  AGroupButton: TdxRibbonQuickAccessGroupButton;
  ALink: TdxBarItemLinkAccess;
  I: Integer;
begin
  Result := False;
  if (AQATLinks <> nil) and (AToolbar <> nil) then
    for I := 0 to AQATLinks.Count - 1 do
    begin
      ALink := TdxBarItemLinkAccess(AQATLinks[I]);
      if not ALink.IsMarkedForDeletion and (ALink.Item is TdxRibbonQuickAccessGroupButton) then
      begin
        AGroupButton := TdxRibbonQuickAccessGroupButton(ALink.Item);
        if (AGroupButton.Toolbar = AToolbar) or TdxRibbonMergeHelper.IsMergedWith(AToolbar, AGroupButton.Toolbar) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
end;

class function TdxRibbonQuickAccessToolbarHelper.IsToolbarDockedInRibbon(
  ARibbon: TdxCustomRibbon; AToolbar: TdxBar): Boolean;
begin
  Result := (AToolbar <> nil) and (AToolbar.DockControl is TdxRibbonGroupsDockControl) and
    (TdxRibbonGroupsDockControl(AToolbar.DockControl).Ribbon = ARibbon);
end;

{ TdxRibbonQuickAccessGroupButton }

function TdxRibbonQuickAccessGroupButton.IsToolbarAcceptable(AToolbar: TdxBar): Boolean;
var
  AQATBar: TdxBar;
begin
  Result := TdxBarManagerAccess(BarManager).IsInitializing or (AToolbar = nil) or (LinkCount = 0);
  if not Result then
  begin
    AQATBar := TdxBar(Links[0].Owner.Owner);
    Result := TdxRibbonQuickAccessToolbarHelper.IsToolbarDockedInRibbon(
      TdxRibbonQuickAccessToolbarDockControl(AQATBar.RealDockControl).Ribbon, AToolbar) and
      not HasGroupButtonForToolbar(AQATBar, AToolbar);
  end;
end;

function TdxRibbonQuickAccessGroupButton.CanBePlacedOn(
  AParentKind: TdxBarItemControlParentKind; AToolbar: TdxBar; out AErrorText: string): Boolean;

  function IsDockedInRibbon(ABar: TdxBar): Boolean;
  var
    ARibbon: TdxCustomRibbon;
  begin
    if AToolbar.Control is TdxRibbonQuickAccessToolbarBarControl then
      ARibbon := TdxRibbonQuickAccessToolbarBarControl(AToolbar.Control).Ribbon
    else
      if AToolbar.DockControl is TdxRibbonQuickAccessToolbarDockControl then
        ARibbon := TdxRibbonQuickAccessToolbarDockControl(AToolbar.DockControl).Ribbon
      else
        ARibbon := nil;

    Result := TdxRibbonQuickAccessToolbarHelper.IsToolbarDockedInRibbon(ARibbon, ABar);
  end;

  function IsToolbarDockedInRibbon: Boolean;
  begin
    Result := (Toolbar.DockControl is TdxRibbonGroupsDockControl) and
      (TdxRibbonGroupsDockControl(Toolbar.DockControl).BarManager = BarManager) and
      (IsDockedInRibbon(Toolbar) or IsDockedInRibbon(TdxBarAccess(Toolbar).MergeData.MergedWith));
  end;

begin
  Result := TdxBarManagerAccess(BarManager).IsInitializing;
  if Result then
    Exit;

  if (AParentKind <> pkBar) or not GetBarControlClass(AToolbar).InheritsFrom(TdxRibbonQuickAccessToolbarBarControl) then
  begin
    AErrorText := cxGetResourceString(@dxSBAR_CANTPLACEQUICKACCESSGROUPBUTTON);
    Exit;
  end;

  if (Toolbar <> nil) and not IsToolbarDockedInRibbon then
  begin
    AErrorText := cxGetResourceString(@dxSBAR_QUICKACCESSGROUPBUTTONTOOLBARNOTDOCKEDINRIBBON);
    Exit;
  end;

  if HasGroupButtonForToolbar(AToolbar, Toolbar) then
  begin
    AErrorText := cxGetResourceString(@dxSBAR_QUICKACCESSALREADYHASGROUPBUTTON);
    Exit;
  end;
  Result := True;
end;

procedure TdxRibbonQuickAccessGroupButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Toolbar) then
    Toolbar := nil;
end;

function TdxRibbonQuickAccessGroupButton.GetActualToolbar: TdxBar;
begin
  if (Toolbar <> nil) and (TdxBarAccess(Toolbar).MergeData.MergedWith <> nil) then
    Result := TdxBarAccess(Toolbar).MergeData.MergedWith
  else
    Result := Toolbar;
end;

function TdxRibbonQuickAccessGroupButton.GetCaption: string;
begin
  if Toolbar = nil then
    Result := 'GroupButton'
  else
    Result := dxRibbonGetGroupCaption(Toolbar);
end;

function TdxRibbonQuickAccessGroupButton.IsCaptionStored: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonQuickAccessGroupButton.SetCaption(const Value: string);
begin
end;

function TdxRibbonQuickAccessGroupButton.HasGroupButtonForToolbar(AParentBar, AToolbar: TdxBar): Boolean;
var
  AParentBarItemLinks: TdxBarItemLinks;
begin
  if AParentBar.Control <> nil then
    AParentBarItemLinks := AParentBar.Control.ItemLinks
  else
    AParentBarItemLinks := AParentBar.ItemLinks;

  Result := TdxRibbonQuickAccessToolbarHelper.HasGroupButtonForToolbar(AParentBarItemLinks, AToolbar);
end;

procedure TdxRibbonQuickAccessGroupButton.SetToolbar(Value: TdxBar);
begin
  if IsToolbarAcceptable(Value) and (Value <> FToolbar) then
  begin
    if FToolbar <> nil then
    begin
      FToolbar.RemoveFreeNotification(Self);
      FToolbar := nil;
    end;
    if Value <> nil then
    begin
      FToolbar := Value;
      FToolbar.FreeNotification(Self);
    end;
    Update;
  end;
end;

{ TdxRibbonQuickAccessGroupButtonControl }

destructor TdxRibbonQuickAccessGroupButtonControl.Destroy;
begin
  ClosePopup;
  inherited Destroy;
end;

function TdxRibbonQuickAccessGroupButtonControl.IsDroppedDown: Boolean;
begin
  Result := FPopupBarControl <> nil;
end;

procedure TdxRibbonQuickAccessGroupButtonControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited CalcDrawParams(AFull);
  DrawParams.DroppedDown := IsDroppedDown;
  DrawParams.Enabled := DrawParams.Enabled and (Item.IsDesigning or (Item.Toolbar <> nil));
end;

function TdxRibbonQuickAccessGroupButtonControl.CanActivate: Boolean;
begin
  Result := not BarManager.Designing and inherited CanActivate;
end;

procedure TdxRibbonQuickAccessGroupButtonControl.ControlClick(AByMouse: Boolean;
  AKey: Char = #0);
begin
  inherited ControlClick(AByMouse, AKey);
  if TdxRibbonQuickAccessToolbarBarControl(Parent).IsDowned then
    ControlActivate(True, AByMouse);
end;

procedure TdxRibbonQuickAccessGroupButtonControl.DoCloseUp(
  AHadSubMenuControl: Boolean);
begin
  ClosePopup;
  Repaint;
end;

procedure TdxRibbonQuickAccessGroupButtonControl.DoDropDown(AByMouse: Boolean);
var
  AToolbar: TdxBar;
  R: TRect;
begin
  AToolbar := Item.Toolbar;
  if AToolbar <> nil then
  begin
    FPopupBarControl := TdxRibbonQuickAccessGroupButtonPopupBarControl.CreateForPopup(Self);
    R := ItemLink.ItemRect;
    R.TopLeft := Parent.ClientToScreen(R.TopLeft);
    R.BottomRight := Parent.ClientToScreen(R.BottomRight);
    BarDesignController.ShowQuickControl(FPopupBarControl, Painter, R);
    if BarNavigationController.NavigationMode and not BarNavigationController.KeyTipsHandlingMode then
      SelectFirstSelectableAccessibleObject(FPopupBarControl.IAccessibilityHelper.GetBarHelper);
    Repaint;
  end;
end;

procedure TdxRibbonQuickAccessGroupButtonControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  TdxRibbonQuickAccessToolbarPainter(Painter).DrawGroupButtonControl(DrawParams, ARect);
end;

procedure TdxRibbonQuickAccessGroupButtonControl.DropDown(AByMouse: Boolean);
begin
  inherited DropDown(AByMouse);
  Click(AByMouse);
end;

function TdxRibbonQuickAccessGroupButtonControl.CanDestroyOnClick: Boolean;
begin
  Result := False;
end;

function TdxRibbonQuickAccessGroupButtonControl.IsDropDown: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonQuickAccessGroupButtonControl.ClosePopup;
begin
  FreeAndNil(FPopupBarControl);
end;

function TdxRibbonQuickAccessGroupButtonControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper;
end;

function TdxRibbonQuickAccessGroupButtonControl.GetCurrentImage(
  AViewSize: TdxBarItemControlViewSize; ASelected: Boolean; out AGlyph: TdxSmartGlyph;
  out AImages: TCustomImageList; out AImageIndex: Integer; out AIsImageEnabled: TdxDefaultBoolean): Boolean;
begin
  if Item.Toolbar <> nil then
  begin
    AGlyph := Item.Toolbar.Glyph;
    if not IsGlyphAssigned(AGlyph) then
      AGlyph := nil;
  end
  else
    AGlyph := nil;

  AImages := nil;
  AImageIndex := -1;
  AIsImageEnabled := bDefault;

  Result := AGlyph <> nil;
end;

function TdxRibbonQuickAccessGroupButtonControl.GetHint: string;
begin
  Result := ItemLink.Caption;
end;

function TdxRibbonQuickAccessGroupButtonControl.GetItem: TdxRibbonQuickAccessGroupButton;
begin
  Result := TdxRibbonQuickAccessGroupButton(inherited Item);
end;

function TdxRibbonQuickAccessGroupButtonControl.GetViewStructure: TdxBarItemControlViewStructure;
begin
  Result := [cpIcon];
end;

{ TdxRibbonQuickAccessGroupButtonPopupBarControl }

constructor TdxRibbonQuickAccessGroupButtonPopupBarControl.CreateForPopup(
  AGroupButtonControl: TdxRibbonQuickAccessGroupButtonControl);
var
  AToolbar: TdxBar;
begin
  AToolbar := AGroupButtonControl.Item.ActualToolbar;
  inherited Create(AToolbar.BarManager);
  InitializeForPopup(TdxBarControl(AGroupButtonControl.Parent), AToolbar);
  FGroupButtonControl := AGroupButtonControl;
end;

procedure TdxRibbonQuickAccessGroupButtonPopupBarControl.CloseUp;
begin
  FGroupButtonControl.ClosePopup;
end;

function TdxRibbonQuickAccessGroupButtonPopupBarControl.CanActiveChange: Boolean;
begin
  Result := not FIsActiveChangeLocked and inherited CanActiveChange;
end;

procedure TdxRibbonQuickAccessGroupButtonPopupBarControl.CreateWnd;
begin
  inherited CreateWnd;
  IsActive := True;
end;

procedure TdxRibbonQuickAccessGroupButtonPopupBarControl.DestroyWnd;
begin
  if FGroupButtonControl.MousePressed then
    TdxRibbonQuickAccessToolbarBarControl(FGroupButtonControl.Parent).IgnoreMouseClick := True;
  inherited DestroyWnd;
end;

procedure TdxRibbonQuickAccessGroupButtonPopupBarControl.FocusItemControl(
  AItemControl: TdxBarItemControl);
begin
  if AItemControl <> nil then
  begin
    FIsActiveChangeLocked := True;
    try
      inherited FocusItemControl(AItemControl);
    finally
      FIsActiveChangeLocked := False;
    end;
  end;
end;

function TdxRibbonQuickAccessGroupButtonPopupBarControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := inherited GetBehaviorOptions + [bboNeedsFocusWhenActive];
end;

procedure TdxRibbonQuickAccessGroupButtonPopupBarControl.HideAllByEscape;
var
  AGroupButtonControlToSelect: TdxRibbonQuickAccessGroupButtonControl;
begin
  if BarNavigationController.NavigationMode then
    AGroupButtonControlToSelect := FGroupButtonControl
  else
    AGroupButtonControlToSelect := nil;
  inherited HideAllByEscape;
  if AGroupButtonControlToSelect <> nil then
    AGroupButtonControlToSelect.IAccessibilityHelper.Select(False);
end;

function TdxRibbonQuickAccessGroupButtonPopupBarControl.IsAllowContextPaint: Boolean;
begin
  Result := False;
end;

{ TdxAddGroupButtonEditor }

class function TdxAddGroupButtonEditor.GetAddedItemClass(const AAddedItemName: string): TdxBarItemClass;
begin
  Result := TdxRibbonQuickAccessGroupButton;
end;

class function TdxAddGroupButtonEditor.GetPopupItemCaption: string;
begin
  Result := dxSBAR_CP_ADDGROUPBUTTON;
end;

{ TdxRibbonAccessibilityHelper }

constructor TdxRibbonAccessibilityHelper.Create(AOwnerObject: TObject);
begin
  inherited;
  FAccessibilityChildren := TList.Create;
end;

destructor TdxRibbonAccessibilityHelper.Destroy;
begin
  FreeAndNil(FAccessibilityChildren);
  inherited;
end;

// IdxBarAccessibilityHelper
function TdxRibbonAccessibilityHelper.AreKeyTipsSupported(
  out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean;
begin
  Result := True;
  if FKeyTipWindowsManager = nil then
    FKeyTipWindowsManager := TdxRibbonKeyTipWindows.Create(Ribbon);
  AKeyTipWindowsManager := FKeyTipWindowsManager;
end;

function TdxRibbonAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Ribbon.BarManager;
end;

function TdxRibbonAccessibilityHelper.GetDefaultAccessibleObject: IdxBarAccessibilityHelper;
begin
  Result := nil;
  if Ribbon.AreGroupsVisible then
    Result := Ribbon.TabsIAccessibilityHelper.GetDefaultAccessibleObject;
  if (Result = nil) and Ribbon.ApplicationButton.IAccessibilityHelper.GetHelper.Visible then
    Result := Ribbon.ApplicationButton.IAccessibilityHelper;
end;

function TdxRibbonAccessibilityHelper.GetChild(AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := IdxBarAccessibilityHelper(FAccessibilityChildren[AIndex]).GetHelper;
end;

function TdxRibbonAccessibilityHelper.GetChildCount: Integer;

  procedure AddAccessibilityChild(AChild: IdxBarAccessibilityHelper);
  begin
    FAccessibilityChildren.Add(Pointer(AChild));
  end;

var
  I: Integer;
begin
  FAccessibilityChildren.Clear;
  AddAccessibilityChild(Ribbon.TabsIAccessibilityHelper);
  for I := 0 to Ribbon.ViewInfo.TabsAreaViewInfo.ButtonsViewInfo.Count - 1 do
    AddAccessibilityChild(Ribbon.ViewInfo.TabsAreaViewInfo.ButtonsViewInfo[I].AccessibilityHelper);
  AddAccessibilityChild(Ribbon.ApplicationButton.IAccessibilityHelper);
  for I := 0 to Ribbon.FAffiliatedBarControlsForAccessibility.Count - 1 do
    AddAccessibilityChild(TCustomdxBarControl(Ribbon.FAffiliatedBarControlsForAccessibility[I]).IAccessibilityHelper);
  if Ribbon.QuickAccessToolbar.IAccessibilityHelper <> nil then
    AddAccessibilityChild(Ribbon.QuickAccessToolbar.IAccessibilityHelper);
  if Ribbon.TabAreaSearchToolbar.IAccessibilityHelper <> nil then
    AddAccessibilityChild(Ribbon.TabAreaSearchToolbar.IAccessibilityHelper);
  if Ribbon.TabAreaToolbar.IAccessibilityHelper <> nil then
    AddAccessibilityChild(Ribbon.TabAreaToolbar.IAccessibilityHelper);
  Result := FAccessibilityChildren.Count;
end;

function TdxRibbonAccessibilityHelper.GetChildIndex(AChild: TcxAccessibilityHelper): Integer;
begin
  Result := FAccessibilityChildren.IndexOf(Pointer(dxBar.GetAccessibilityHelper(AChild)));
end;

function TdxRibbonAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  if Ribbon.HandleAllocated then
    Result := Ribbon.Handle
  else
    Result := 0;
end;

function TdxRibbonAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
    Result := cxGetWindowRect(GetOwnerObjectWindow)
  else
    Result := cxEmptyRect;
end;

function TdxRibbonAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
var
  AHandle: HWND;
begin
  Result := cxSTATE_SYSTEM_NORMAL;
  AHandle := GetOwnerObjectWindow;
  if (AHandle = 0) or not IsWindowVisible(AHandle) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxRibbonAccessibilityHelper.LogicalNavigationGetChild(
  AIndex: Integer): TdxBarAccessibilityHelper;
begin
  Result := Childs[AIndex];
end;

function TdxRibbonAccessibilityHelper.LogicalNavigationGetChildIndex(
  AChild: TdxBarAccessibilityHelper): Integer;
begin
  Result := GetChildIndex(AChild);
end;

function TdxRibbonAccessibilityHelper.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxCustomRibbon(FOwnerObject);
end;

function TdxRibbonAccessibilityHelper.GetTabsAreaButtonsViewInfo: TdxRibbonButtonsContainerViewInfo;
begin
  Result := Ribbon.ViewInfo.TabsAreaViewInfo.ButtonsViewInfo;
end;

{ TdxRibbonTabCollectionAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxRibbonTabCollectionAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := TabCollection.Owner.BarManager;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetDefaultAccessibleObject: IdxBarAccessibilityHelper;
begin
  if TabCollection.Owner.ActiveTab <> nil then
    Result := TabCollection.Owner.ActiveTab.IAccessibilityHelper
  else
    Result := nil;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetChild(AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := TabsViewInfo[AIndex].Tab.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetChildCount: Integer;
begin
  Result := TabsViewInfo.Count
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetChildIndex(AChild: TcxAccessibilityHelper): Integer;
begin
  if (AChild is TdxRibbonTabAccessibilityHelper) and (TdxRibbonTabAccessibilityHelper(AChild).Parent = Self) then
    Result := TabsViewInfo.FCells.IndexOf(TdxRibbonTabAccessibilityHelper(AChild).Tab)
  else
    Result := -1;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := TabCollection.Owner.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetScreenBounds(AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := TabsViewInfo.Bounds;
    with TabCollection.Owner do
    begin
      Result.TopLeft := ClientToScreen(Result.TopLeft);
      Result.BottomRight := ClientToScreen(Result.BottomRight);
    end;
  end
  else
    Result := cxEmptyRect;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not TabCollection.Owner.ViewInfo.IsTabsVisible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxRibbonTabCollectionAccessibilityHelper.LogicalNavigationGetChild(AIndex: Integer): TdxBarAccessibilityHelper;
begin
  if TabCollection.Owner.AreGroupsVisible then
    Result := TabCollection.Owner.ActiveTab.IAccessibilityHelper.GetBarHelper
  else
    Result := GetFirstSelectableObject;
end;

function TdxRibbonTabCollectionAccessibilityHelper.LogicalNavigationGetChildCount: Integer;
var
  AAreGroupsVisible: Boolean;
begin
  AAreGroupsVisible := TabCollection.Owner.AreGroupsVisible;
  if AAreGroupsVisible and (TabCollection.Owner.ActiveTab <> nil) or
    not AAreGroupsVisible and (GetFirstSelectableObject <> nil) then
      Result := 1
  else
    Result := 0;
end;

function TdxRibbonTabCollectionAccessibilityHelper.LogicalNavigationGetChildIndex(
  AChild: TdxBarAccessibilityHelper): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to LogicalNavigationGetChildCount - 1 do
    if AChild = LogicalNavigationGetChild(I) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetTabCollection: TdxRibbonTabCollection;
begin
  Result := TdxRibbonTabCollection(FOwnerObject);
end;

function TdxRibbonTabCollectionAccessibilityHelper.GetTabsViewInfo: TdxRibbonTabsViewInfo;
begin
  Result := TabCollection.Owner.ViewInfo.TabsAreaViewInfo.TabsViewInfo;
end;

{ TdxRibbonTabAccessibilityHelper }

procedure TdxRibbonTabAccessibilityHelper.CloseUpHandler(AReason: TdxBarCloseUpReason);
begin
  if (AReason = bcrEscape) and BarNavigationController.NavigationMode then
    Select(False);
end;

// IdxBarAccessibilityHelper
function TdxRibbonTabAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Ribbon.BarManager;
end;

function TdxRibbonTabAccessibilityHelper.GetNextAccessibleObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
var
  ADockControlObject: TdxBarAccessibilityHelper;
begin
  Result := nil;
  case ADirection of
    andLeft, andRight:
      Result := Ribbon.GetNextHorizontalAccessibleObject(Self, ADirection);

    andUp:
      if Ribbon.QuickAccessToolbar.IAccessibilityHelper = nil then
        Result := Self;

    andDown:
      begin
        ADockControlObject := Tab.DockControl.IAccessibilityHelper.GetBarHelper;
        if ADockControlObject.Visible then
          Result := ADockControlObject.GetFirstSelectableObject;
      end;
  end;

  if Result = nil then
    Result := inherited GetNextAccessibleObject(ADirection);
end;

function TdxRibbonTabAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if not Result and not Ribbon.AreGroupsVisible then
  begin
    Result := AKey in [VK_RETURN, VK_SPACE];
    if Result then
    begin
      Tab.Active := True;
      Ribbon.ShowTabGroupsPopupWindow;
      SelectFirstSelectableAccessibleObject(Tab.DockControl.IAccessibilityHelper.GetBarHelper);
    end;
  end;
end;

function TdxRibbonTabAccessibilityHelper.IsNavigationKey(AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey) or (AKey = VK_ESCAPE);
  if not Ribbon.AreGroupsVisible then
    Result := Result or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxRibbonTabAccessibilityHelper.LogicalNavigationGetNextAccessibleObject(
  AShift: TShiftState): IdxBarAccessibilityHelper;
var
  AGoForward: Boolean;
begin
  AGoForward := not(ssShift in AShift);
  if AGoForward then
    Result := LogicalNavigationGetNextChild(-1, True)
  else
    Result := inherited LogicalNavigationGetNextAccessibleObject(AShift);
end;

procedure TdxRibbonTabAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  if Ribbon.AreGroupsVisible then
    Tab.Active := True;
  Tab.Invalidate;
end;

procedure TdxRibbonTabAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  Tab.Invalidate;
end;

function TdxRibbonTabAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := Tab.DockControl.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonTabAccessibilityHelper.GetChildCount: Integer;
begin
  Result := 1;
end;

function TdxRibbonTabAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  if AChild = Tab.DockControl.IAccessibilityHelper.GetHelper then
    Result := 0
  else
    Result := -1;
end;

function TdxRibbonTabAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxRibbonTabAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := TdxRibbonTabCollection(Tab.Collection).IAccessibilityHelper.GetHelper;
end;

function TdxRibbonTabAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := Tab.ViewInfo.Bounds;
    Result.TopLeft := Ribbon.ClientToScreen(Result.TopLeft);
    Result.BottomRight := Ribbon.ClientToScreen(Result.BottomRight);
  end
  else
    Result := cxEmptyRect;
end;

function TdxRibbonTabAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxRibbonTabAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not Tab.IsVisible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxRibbonTabAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := Tab.KeyTip;
  if (Length(Result) > 0) and dxCharInSet(Result[1], ['0'..'9']) then
    Result := '';
end;

function TdxRibbonTabAccessibilityHelper.GetDefaultKeyTip: string;
begin
  Result := 'Y';
end;

procedure TdxRibbonTabAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
var
  ABasePoint: TPoint;
  ATextMetric: TTextMetric;
begin
  inherited GetKeyTipInfo(AKeyTipInfo);
  cxGetTextMetrics(Tab.ViewInfo.Font, ATextMetric);
  with Tab.ViewInfo.TextBounds do
  begin
    ABasePoint.X := (Left + Right) div 2;
    ABasePoint.Y := Bottom - ATextMetric.tmDescent;
  end;
  AKeyTipInfo.BasePoint := Tab.Ribbon.ClientToScreen(ABasePoint);
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaBottom;
  AKeyTipInfo.Enabled := True;
end;

procedure TdxRibbonTabAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  BarNavigationController.ChangeSelectedObject(True, Self);
  if not Ribbon.AreGroupsVisible then
  begin
    Tab.Active := True;
    BarNavigationController.UnselectAssignedSelectedObject;
    Ribbon.ShowTabGroupsPopupWindow;
  end;
  BarNavigationController.SetKeyTipsShowingState(Self, '');
end;

procedure TdxRibbonTabAccessibilityHelper.KeyTipsEscapeHandler;
begin
  Ribbon.CloseTabGroupsPopupWindow;
  inherited KeyTipsEscapeHandler;
end;

function TdxRibbonTabAccessibilityHelper.GetRibbon: TdxCustomRibbon;
begin
  Result := Tab.Ribbon;
end;

function TdxRibbonTabAccessibilityHelper.GetTab: TdxRibbonTab;
begin
  Result := FOwnerObject as TdxRibbonTab;
end;

{ TdxRibbonApplicationButtonAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxRibbonApplicationButtonAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := Ribbon.BarManager;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetNextAccessibleObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
begin
  Result := nil;
  case ADirection of
    andLeft, andRight:
      Result := Ribbon.GetNextHorizontalAccessibleObject(Self, ADirection);
    andUp:
      Result := Self;
  end;
  if Result = nil then
    Result := inherited GetNextAccessibleObject(ADirection);
end;

function TdxRibbonApplicationButtonAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if not Result then
  begin
    Result := AKey in [VK_DOWN, VK_SPACE, VK_RETURN];
    if Result then
      ShowApplicationMenu(DXM_BAR_SELECTAPPMENUFIRSTITEMCONTROL);
  end;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.IsNavigationKey(AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey) or (AKey in [VK_ESCAPE, VK_SPACE, VK_RETURN]);
end;

procedure TdxRibbonApplicationButtonAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  inherited Select(ASetFocus);
  ApplicationButtonViewInfo.Invalidate;
end;

procedure TdxRibbonApplicationButtonAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  inherited Unselect(ANextSelectedObject);
  ApplicationButtonViewInfo.Invalidate;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetOwnerObjectWindow: HWND;
begin
  Result := Parent.OwnerObjectWindow;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := Ribbon.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetScreenBounds(
  AChildID: TcxAccessibleSimpleChildElementID): TRect;
begin
  if Visible then
  begin
    Result := ApplicationButtonViewInfo.Bounds;
    Result.TopLeft := Ribbon.ClientToScreen(Result.TopLeft);
    Result.BottomRight := Ribbon.ClientToScreen(Result.BottomRight);
  end
  else
    Result := cxEmptyRect;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetSelectable: Boolean;
begin
  Result := Visible;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not ApplicationButtonViewInfo.IsVisible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := Ribbon.ApplicationButton.KeyTip;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetDefaultKeyTip: string;
begin
  Result := 'F';
end;

procedure TdxRibbonApplicationButtonAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
begin
  inherited GetKeyTipInfo(AKeyTipInfo);
  AKeyTipInfo.BasePoint := cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID));
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaCenter;
  AKeyTipInfo.Enabled := True;
end;

procedure TdxRibbonApplicationButtonAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  ShowApplicationMenu(DXM_BAR_SHOWKEYTIPS);
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetApplicationButtonViewInfo: TdxRibbonApplicationButtonViewInfo;
begin
  Result := Ribbon.ApplicationButtonViewInfo;
end;

function TdxRibbonApplicationButtonAccessibilityHelper.GetRibbon: TdxCustomRibbon;
begin
  Result := TdxRibbonApplicationButton(FOwnerObject).Ribbon;
end;

procedure TdxRibbonApplicationButtonAccessibilityHelper.ShowApplicationMenu(
  APostMessage: UINT);
begin
  BarNavigationController.UnselectAssignedSelectedObject;
  CloseActiveRibbonApplicationMenus;
  BarNavigationController.SetKeyTipsShowingState(nil, '');
  PostMessage(Ribbon.Handle, DXM_RIBBON_SHOWAPPLICATIONMENU, 0, 0);
  if Ribbon.ApplicationButton.IMenu <> nil then
    PostMessage(Ribbon.Handle, APostMessage, 0, 0);
end;

{ TdxRibbonGroupsDockControlAccessibilityHelper }

function TdxRibbonGroupsDockControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := DockControl.ViewInfo.GroupViewInfos[AIndex].BarControl.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := DockControl.ViewInfo.GroupCount;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ChildCount - 1 do
    if Childs[I] = AChild then
    begin
      Result := I;
      Break;
    end;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  if dxBarGetParentPopupWindow(DockControl, False) <> nil then
    Result := nil
  else
    Result := TdxRibbonGroupsDockControl(DockControl).Tab.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetState(
  AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := inherited GetState(AChildID);
  if not TdxRibbonGroupsDockControl(DockControl).Tab.Ribbon.AreGroupsVisible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetParentForKeyTip: TdxBarAccessibilityHelper;
begin
  if dxBarGetParentPopupWindow(DockControl, False) <> nil then
    Result := DockControl.Tab.IAccessibilityHelper.GetBarHelper
  else
    Result := inherited GetParentForKeyTip;
end;

function TdxRibbonGroupsDockControlAccessibilityHelper.GetDockControl: TdxRibbonGroupsDockControl;
begin
  Result := TdxRibbonGroupsDockControl(FOwnerObject);
end;

{ TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper }

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetChild(AIndex: Integer): TcxAccessibilityHelper;
begin
  if (AIndex = ChildCount - 1) and MarkExists then
    Result := BarControl.MarkIAccessibilityHelper.GetHelper
  else
    Result := inherited GetChild(AIndex);
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount;
  if MarkExists then
    Inc(Result);
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetChildIndex(AChild: TcxAccessibilityHelper): Integer;
begin
  if AChild = BarControl.MarkIAccessibilityHelper.GetHelper then
    Result := inherited GetChildCount
  else
    Result := inherited GetChildIndex(AChild);
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetParent: TcxAccessibilityHelper;
begin
  Result := TdxRibbonQuickAccessToolbarBarControl(BarControl).Ribbon.IAccessibilityHelper.GetHelper;
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.MarkExists: Boolean;
begin
  Result := TCustomdxBarControlAccess(BarControl).MarkExists;
end;

procedure TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.DoGetKeyTipsData(AKeyTipsData: TList);

  procedure GetItemsKeyTipsData(ABarControl: TCustomdxBarControl;
    AStartIndex, AEndIndex: Integer; AKeyTipsData: TList; AVisible: Boolean);
  var
    I: Integer;
    AChild: TdxBarAccessibilityHelper;
    AKeyTipData: TdxBarKeyTipData;
  begin
    for I := AStartIndex to AEndIndex do
    begin
      AChild := ABarControl.ItemLinks.VisibleItems[I].Control.IAccessibilityHelper.GetBarHelper;
      AKeyTipData := TdxBarAccessibilityHelperAccess(AChild).CreateKeyTipData;
      AKeyTipData.Visible := AVisible;
      AKeyTipsData.Add(AKeyTipData);
    end;
  end;

  procedure GenerateKeyTips(AItemKeyTipsData: TList);
  var
    I: Integer;
  begin
    for I := 0 to AItemKeyTipsData.Count - 1 do
    begin
      case I of
        0..8:
          TdxBarKeyTipData(AItemKeyTipsData[I]).KeyTip := IntToStr(I + 1);                   // '1'..'9'
        9..17:
          TdxBarKeyTipData(AItemKeyTipsData[I]).KeyTip := '0' + IntToStr(18 - I);           // '09'..'01'
        18..44:
          TdxBarKeyTipData(AItemKeyTipsData[I]).KeyTip := '0' + Char(Ord('A') + (I - 18)); // '0A'..'0Z'
      else
        TdxBarKeyTipData(AItemKeyTipsData[I]).KeyTip := '';
      end;
      AKeyTipsData.Add(AItemKeyTipsData[I])
    end;
  end;

var
  VisibleItemCount, ARealVisibleItemCount: Integer;
  AItemKeyTipsData: TList;
begin
  AItemKeyTipsData := TList.Create;
  try
    VisibleItemCount := TdxBarItemLinksAccess(BarControl.ItemLinks).VisibleItemCount;
    if BarControl.IsPopup then
    begin
      ARealVisibleItemCount := TdxBarItemLinksAccess(BarControl.ParentBar.ItemLinks).RealVisibleItemCount;
      GetItemsKeyTipsData(BarControl.ParentBar, 0, ARealVisibleItemCount - 1, AItemKeyTipsData, False);
      GetItemsKeyTipsData(BarControl, 0, VisibleItemCount - 1 - 1{Mark!!!}, AItemKeyTipsData, True);
    end
    else
    begin
      ARealVisibleItemCount := TdxBarItemLinksAccess(BarControl.ItemLinks).RealVisibleItemCount;
      GetItemsKeyTipsData(BarControl, 0, ARealVisibleItemCount - 1, AItemKeyTipsData, True);
      GetItemsKeyTipsData(BarControl, ARealVisibleItemCount, VisibleItemCount - 1, AItemKeyTipsData, False);
      if not BarControl.AllItemsVisible then
        TdxBarAccessibilityHelperAccess(BarControl.MarkIAccessibilityHelper.GetBarHelper).GetKeyTipData(AKeyTipsData);
    end;
    GenerateKeyTips(AItemKeyTipsData);
  finally
    AItemKeyTipsData.Free;
  end;
end;

procedure TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
begin
  inherited;
  AKeyTipInfo.VertAlign := vaBottom;
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;

  function InternalGetRootObject: TdxBarAccessibilityHelper;
  begin
    if TdxRibbonQuickAccessToolbarBarControl(BarControl).IsPopup then
      Result := Self
    else
      Result := GetRootAccessibleObject.GetBarHelper;
  end;

var
  AObjects: TList;
begin
  AObjects := TList.Create;
  try
    GetChildrenForNavigation(AItemLinkHelper, InternalGetRootObject,
      AItemLinkHelper.GetScreenBounds(cxAccessibleObjectSelfID),
      ADirection, True, AObjects);
    Result := dxBar.GetNextAccessibleObject(AItemLinkHelper, AObjects, ADirection, True);
  finally
    AObjects.Free;
  end;
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetParentForKeyTip: TdxBarAccessibilityHelper;
begin
  if BarControl.IsPopup then
    Result := BarControl.ParentBar.IAccessibilityHelper.GetBarHelper
  else
    Result := inherited GetParentForKeyTip;
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.IsKeyTipContainer: Boolean;
begin
  Result := BarControl.IsPopup;
end;

procedure TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.KeyTipsEscapeHandler;
var
  AMarkAccessibleObject: IdxBarAccessibilityHelper;
  ASelectedControl: TdxBarItemControl;
begin
  if not BarControl.IsPopup then
  begin
    ASelectedControl := TdxRibbonQuickAccessToolbarBarControl(BarControl).SelectedControl;
    inherited KeyTipsEscapeHandler;
    BarNavigationController.ChangeSelectedObject(True, ASelectedControl.IAccessibilityHelper);
  end
  else
  begin
    BarNavigationController.SetKeyTipsShowingState(GetKeyTipContainerParent(Self), '');
    AMarkAccessibleObject := TdxRibbonQuickAccessToolbarBarControl(BarControl.ParentBar).MarkIAccessibilityHelper;
    TdxRibbonQuickAccessToolbarBarControl(BarControl.ParentBar).MarkState := msNone;
    if AMarkAccessibleObject.GetHelper.IsOwnerObjectLive then
      BarNavigationController.ChangeSelectedObject(True, AMarkAccessibleObject);
  end;
end;

function TdxRibbonQuickAccessToolbarBarControlAccessibilityHelper.GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
begin
  Result := TdxRibbonQuickAccessToolbarBarControl(FOwnerObject);
end;

{ TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper }

// IdxBarAccessibilityHelper
function TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if (BarControl.MarkState = msPressed) and not BarControl.AllItemsVisible then
    SelectFirstSelectableAccessibleObject(BarDesignController.QuickControl.IAccessibilityHelper.GetBarHelper);
end;

procedure TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
begin
  inherited;
  AKeyTipInfo.BasePoint := cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID));
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaBottom;
  AKeyTipInfo.Enabled := True;
end;

function TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper.GetKeyTip: string;
begin
  if BarControl.AllItemsVisible then
    Result := ''
  else
    Result := '00';
end;

procedure TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  DropDown;
  BarNavigationController.SetKeyTipsShowingState(BarDesignController.QuickControl.IAccessibilityHelper, '');
end;

function TdxRibbonQuickAccessToolbarBarControlMarkAccessibilityHelper.GetBarControl: TdxRibbonQuickAccessToolbarBarControl;
begin
  Result := TdxRibbonQuickAccessToolbarBarControl(FOwnerObject);
end;

{ TdxRibbonGroupBarControlAccessibilityHelper }

procedure TdxRibbonGroupBarControlAccessibilityHelper.CloseUpHandler(AReason: TdxBarCloseUpReason);
begin
  if (AReason = bcrEscape) and BarNavigationController.NavigationMode then
    Select(False);
end;

// IdxBarAccessibilityHelper
function TdxRibbonGroupBarControlAccessibilityHelper.GetNextAccessibleObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
begin
  if BarControl.Collapsed then
    Result := GetNextAccessibleObject(nil, ADirection)
  else
    Result := inherited GetNextAccessibleObject(ADirection);
end;

function TdxRibbonGroupBarControlAccessibilityHelper.HandleNavigationKey(
  var AKey: Word): Boolean;
begin
  Result := inherited HandleNavigationKey(AKey);
  if Result then
    Exit;
  Result := BarControl.Collapsed and (AKey in [VK_RETURN, VK_SPACE]);
  if Result then
  begin
    ShowPopupBarControl;
    SelectFirstSelectableAccessibleObject(
      BarDesignController.QuickControl.IAccessibilityHelper.GetBarHelper);
  end;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.IsNavigationKey(AKey: Word): Boolean;
begin
  Result := inherited IsNavigationKey(AKey);
  if BarControl.Collapsed then
    Result := Result or (AKey in [VK_ESCAPE, VK_RETURN, VK_SPACE]);
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.Select(ASetFocus: Boolean);
begin
  if not BarControl.Collapsed then
    inherited Select(ASetFocus)
  else
  begin
    BarNavigationController.SelectedObject := Self;
    BarNavigationController.SelectedObjectParent := Parent;
    BarControl.Invalidate;

    TdxRibbonGroupsDockControl(BarControl.DockControl).MakeRectFullyVisible(BarControl.BoundsRect);
  end;
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.Unselect(
  ANextSelectedObject: IdxBarAccessibilityHelper);
begin
  if not BarControl.Collapsed then
    inherited Unselect(ANextSelectedObject)
  else
  begin
    BarNavigationController.SelectedObject := nil;
    BarNavigationController.SelectedObjectParent := nil;
    BarControl.Invalidate;
  end;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetSelectable: Boolean;
begin
  if BarControl.Collapsed then
    Result := Visible
  else
    Result := inherited GetSelectable;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.Expand: TCustomdxBarControlAccessibilityHelper;
begin
  if not IsCollapsed then
    raise EdxException.Create('TdxRibbonGroupBarControlAccessibilityHelper.Expand fails');
  ShowPopupBarControl;
  Result := TCustomdxBarControlAccessibilityHelper(BarDesignController.QuickControl.IAccessibilityHelper.GetHelper);
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.GetCaptionButtonKeyTipPosition(
  ACaptionButton: TdxBarCaptionButton; out ABasePointY: Integer;
  out AVertAlign: TcxAlignmentVert);
begin
  ABasePointY := BarControl.ViewInfo.BottomKeyTipsBaseLinePosition;
  AVertAlign := vaBottom;
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
var
  AItemControl: TdxBarItemControl;
  AOneRowHeightItemControl: Boolean;
  ARow: Integer;
begin
  AItemControl := AItemLinkHelper.ItemControl;
  AOneRowHeightItemControl := not BarControl.Ribbon.IsSimplifiedGroupsLayout and (AItemControl.ViewInfo.ViewLevel <> ivlLargeIconWithText);

  if AOneRowHeightItemControl then
  begin
    ARow := IdxBarItemControlViewInfo(AItemControl.ViewInfo).GetRow;
    if (IdxBarItemControlViewInfo(AItemControl.ViewInfo).GetColumnRowCount = 2) and (ARow = 1) then
      ARow := 2;
    if IdxBarItemControlViewInfo(AItemControl.ViewInfo).GetColumnRowCount = 1 then
      ARow := 1;
    AKeyTipInfo.BasePoint.Y := BarControl.ViewInfo.RowKeyTipsBaseLinePositions[ARow];
  end
  else
    AKeyTipInfo.BasePoint.Y := BarControl.ViewInfo.RowKeyTipsBaseLinePositions[BarControl.Ribbon.GroupsPainter.GetGroupRowCount - 1];

  AKeyTipInfo.VertAlign := vaCenter;

  if AOneRowHeightItemControl and (cpIcon in TdxBarItemControlAccess(AItemControl).FDrawParams.ViewStructure) then
  begin
    ProcessPaintMessages; // AItemControl.ViewInfo.ImageBounds are calculated on painting
    AKeyTipInfo.BasePoint.X := cxRectCenter(AItemControl.ViewInfo.ImageBounds).X;
    AKeyTipInfo.BasePoint.X := AItemControl.Parent.ClientToScreen(AKeyTipInfo.BasePoint).X;
    AKeyTipInfo.HorzAlign := taRightJustify;
  end
  else
  begin
    AKeyTipInfo.BasePoint.X := cxRectCenter(GetItemScreenBounds(AItemLinkHelper)).X;
    AKeyTipInfo.HorzAlign := taCenter;
  end;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetAssignedKeyTip: string;
begin
  Result := BarControl.Bar.KeyTip;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetDefaultKeyTip: string;

  function GetFirstChar(const AText: string): string;
  begin
    if Length(AText) > 0 then
      Result := AText[1]
    else
      Result := '';
  end;

begin
  Result := 'Z' + GetFirstChar(BarControl.Bar.Caption);
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.GetKeyTipInfo(out AKeyTipInfo: TdxBarKeyTipInfo);
var
  AKeyTipBasePoint: TPoint;
begin
  inherited;
  with GetScreenBounds(cxAccessibleObjectSelfID) do
    AKeyTipBasePoint.X := (Left + Right) div 2;
  AKeyTipBasePoint.Y := BarControl.ViewInfo.BottomKeyTipsBaseLinePosition;
  AKeyTipInfo.BasePoint := AKeyTipBasePoint;
  AKeyTipInfo.HorzAlign := taCenter;
  AKeyTipInfo.VertAlign := vaBottom;
  AKeyTipInfo.Enabled := True;
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.GetKeyTipData(AKeyTipsData: TList);

  procedure AddKeyTipsForItemControls;
  var
    AItemControl: TdxBarItemControl;
    I: Integer;
  begin
    for I := 0 to BarControl.ViewInfo.ItemControlCount - 1 do
    begin
      AItemControl := BarControl.ViewInfo.ItemControlViewInfos[I].Control;
      with TdxBarItemControlAccessibilityHelperAccess(AItemControl.IAccessibilityHelper.GetBarHelper) do
        if CanSelect then
          GetKeyTipData(AKeyTipsData);
    end;
  end;

  procedure AddKeyTipsForCaptionButtons;
  var
    ACaptionButton: TdxBarCaptionButton;
    I: Integer;
  begin
    for I := 0 to BarControl.Bar.CaptionButtons.Count - 1 do
    begin
      ACaptionButton := BarControl.Bar.CaptionButtons[I];
      TdxBarCaptionButtonAccessibilityHelperAccess(ACaptionButton.IAccessibilityHelper.GetHelper).GetKeyTipData(AKeyTipsData);
    end;
  end;

begin
  inherited GetKeyTipData(AKeyTipsData);
  AddKeyTipsForItemControls;
  AddKeyTipsForCaptionButtons;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetNextAccessibleObject(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper;
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;

  function FindAmongItemControlsAndCollapsedBarControls(
    ASelectedObject: TdxBarAccessibilityHelper;
    const ASelectedObjectScreenBounds: TRect): IdxBarAccessibilityHelper;

    procedure GetBarControlChildren(ABarControl: TdxRibbonGroupBarControl;
      AObjects: TList);
    var
      AItemControl1: TdxBarItemControl;
      I: Integer;
    begin
      for I := 0 to ABarControl.ViewInfo.ItemControlCount - 1 do
      begin
        AItemControl1 := ABarControl.ViewInfo.ItemControlViewInfos[I].Control;
        GetChildrenForNavigation(ASelectedObject,
          AItemControl1.IAccessibilityHelper.GetBarHelper, ASelectedObjectScreenBounds,
          ADirection, False, AObjects);
      end;
    end;

  var
    ABarControl: TdxRibbonGroupBarControl;
    AObjects: TList;
    I: Integer;
  begin
    Result := nil;
    AObjects := TList.Create;
    try
      if BarControl.IsPopup then
        GetBarControlChildren(BarControl, AObjects)
      else
        for I := 0 to Parent.ChildCount - 1 do
        begin
          ABarControl := TdxRibbonGroupBarControlAccessibilityHelper(Parent.Childs[I]).BarControl;
          if ABarControl.Collapsed then
            GetChildrenForNavigation(ASelectedObject,
              ABarControl.IAccessibilityHelper.GetBarHelper, ASelectedObjectScreenBounds, ADirection, False, AObjects)
          else
            if not ((ADirection in [andUp, andDown]) and (ABarControl <> BarControl)) then
              GetBarControlChildren(ABarControl, AObjects);
        end;
      Result := dxBar.GetNextAccessibleObject(ASelectedObject, AObjects, ADirection, True);
    finally
      AObjects.Free;
    end;
  end;

var
  ACaptionButtonIndex: Integer;
  AObjects: TList;
  AScreenBounds: TRect;
  ASelectedObject: TdxBarAccessibilityHelper;
begin
  if AItemLinkHelper <> nil then
    ASelectedObject := AItemLinkHelper
  else
    ASelectedObject := Self;
  AScreenBounds := TdxBarAccessibilityHelperAccess(ASelectedObject).GetScreenBounds(cxAccessibleObjectSelfID);
  Result := FindAmongItemControlsAndCollapsedBarControls(ASelectedObject, AScreenBounds);
  if Result <> nil then
    Exit;
  case ADirection of
    andUp:
      if not (BarControl.IsPopup or TdxRibbonGroupBarControl(BarControl).Ribbon.IsPopupGroupsMode) then
        Result := TdxRibbonGroupBarControl(BarControl).Ribbon.ActiveTab.IAccessibilityHelper;
    andDown:
      begin
        ACaptionButtonIndex := -1;
        if not BarControl.Collapsed then
          ACaptionButtonIndex := BarControl.Bar.CaptionButtons.IAccessibilityHelper.GetHelper.GetNextSelectableChildIndex(-1, False);
        if ACaptionButtonIndex <> -1 then
          Result := BarControl.Bar.CaptionButtons[ACaptionButtonIndex].IAccessibilityHelper
        else
          if not (BarControl.IsPopup or TdxRibbonGroupBarControl(BarControl).Ribbon.IsPopupGroupsMode) and
            (BarControl.Ribbon.QuickAccessToolbar.IAccessibilityHelper <> nil) then
          begin
            AObjects := TList.Create;
            try
              GetChildrenForNavigation(ASelectedObject,
                BarControl.Ribbon.QuickAccessToolbar.IAccessibilityHelper.GetBarHelper,
                AScreenBounds, ADirection, False, AObjects);
              Result := dxBar.GetNextAccessibleObject(ASelectedObject, AObjects, ADirection, True);
            finally
              AObjects.Free;
            end;
          end;
      end;
  end;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetParentForKeyTip: TdxBarAccessibilityHelper;
begin
  if BarControl.IsPopup then
    Result := BarControl.ParentBar.IAccessibilityHelper.GetBarHelper
  else
    Result := inherited GetParentForKeyTip;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.IsCollapsed: Boolean;
begin
  Result := BarControl.Collapsed;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.IsKeyTipContainer: Boolean;
begin
  Result := BarControl.IsPopup;
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  ShowPopupBarControl;
  BarNavigationController.SetKeyTipsShowingState(
    BarDesignController.QuickControl.IAccessibilityHelper, '');
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.KeyTipsEscapeHandler;
var
  ASelectedItemControl: TdxBarItemControl;
begin
  if BarControl.IsPopup then
  begin
    if BarControl = BarDesignController.QuickControl then
    begin
      TdxRibbonGroupBarControlAccessibilityHelper(BarControl.ParentBar.IAccessibilityHelper.GetHelper).KeyTipsEscapeHandler;
      TCustomdxBarControlAccess(BarControl.ParentBar).MarkState := msNone;
    end
    else
    begin
      if not TCustomdxBarControlAccess(BarControl.ParentBar).IsPopup then
        TdxRibbonGroupBarControlAccessibilityHelper(BarControl.ParentBar.IAccessibilityHelper.GetHelper).KeyTipsEscapeHandler
      else
      begin
        ASelectedItemControl := TCustomdxBarControlAccess(BarControl.ParentBar).SelectedControl;
        BarControl.Hide;
        BarNavigationController.ChangeSelectedObject(True, ASelectedItemControl.IAccessibilityHelper);
        BarNavigationController.SetKeyTipsShowingState(ASelectedItemControl.Parent.IAccessibilityHelper, '');
      end;
    end;
  end
  else
    inherited KeyTipsEscapeHandler;
end;

function TdxRibbonGroupBarControlAccessibilityHelper.GetBarControl: TdxRibbonGroupBarControl;
begin
  Result := TdxRibbonGroupBarControl(FOwnerObject);
end;

procedure TdxRibbonGroupBarControlAccessibilityHelper.ShowPopupBarControl;
begin
  BarNavigationController.UnselectAssignedSelectedObject;
  BarControl.MarkState := msPressed;
end;

{ TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper }

function TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper.IsDropDownControl: Boolean;
begin
  Result := True;
end;

function TdxRibbonQuickAccessGroupButtonControlAccessibilityHelper.ShowDropDownWindow: Boolean;
begin
  TdxRibbonQuickAccessGroupButtonControl(ItemControl).DropDown(True);
  Result := ItemControl.IsDroppedDown;
end;

{ TdxRibbonKeyTipWindow }

constructor TdxRibbonKeyTipWindow.Create(AColorScheme: TdxCustomRibbonSkin);
begin
  inherited Create(nil);
  FColorScheme := AColorScheme;
  FRibbon := nil;
  Canvas.Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
end;

constructor TdxRibbonKeyTipWindow.Create(ARibbon: TdxCustomRibbon);
begin
  Create(ARibbon.ColorScheme);
  FRibbon := ARibbon;
  Canvas.Font.Height := GetScaled(Canvas.Font.Height);
end;

procedure TdxRibbonKeyTipWindow.ShowKeyTip;
begin
  SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Width + 1, Height + 1, GetScaled(2), GetScaled(2)), True);
  Invalidate;
end;

function TdxRibbonKeyTipWindow.CalcBoundsRect: TRect;
var
  ATempCanvas: TcxScreenCanvas;
  AFlags: Cardinal;
begin
  Result := cxEmptyRect;
  ATempCanvas := TcxScreenCanvas.Create;
  try
    ATempCanvas.Font := Canvas.Font;
    AFlags := DT_CALCRECT or DT_SINGLELINE or DT_LEFT or DT_NOPREFIX;
    if FRibbon.UseRightToLeftReading then
      AFlags := AFlags or DT_RTLREADING;
    cxDrawText(ATempCanvas.Handle, Caption, Result, AFlags);
  finally
    ATempCanvas.Free;
  end;
  Inc(Result.Right, GetScaled(6));
  Result.Right := Max(Result.Right, GetScaled(16));
  Inc(Result.Bottom, GetScaled(2));
end;

procedure TdxRibbonKeyTipWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
end;

procedure TdxRibbonKeyTipWindow.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  if not Enabled then
    cxSetLayeredWindowAttributes(Handle, 153);
end;

procedure TdxRibbonKeyTipWindow.Paint;
var
  R: TRect;
  AFlags: Cardinal;
begin
  R := ClientRect;
  FColorScheme.DrawKeyTip(Canvas.Handle, R);
  Canvas.Font.Color := FColorScheme.GetPartColor(DXBAR_KEYTIP_TEXTCOLOR);
  AFlags := DT_SINGLELINE or DT_CENTER or DT_NOPREFIX or DT_VCENTER;
  if FRibbon.UseRightToLeftReading then
    AFlags := AFlags or DT_RTLREADING;
  cxDrawText(Canvas.Handle, Caption, R, AFlags);
end;

procedure TdxRibbonKeyTipWindow.UpdateBounds;
var
  R: TRect;
begin
  R := CalcBoundsRect;
  UpdateBoundsRect(R);
end;

function TdxRibbonKeyTipWindow.GetScaled(AValue: Integer): Integer;
begin
  if FRibbon <> nil then
    Result := FRibbon.ScaleFactor.Apply(AValue)
  else
    Result := AValue;
end;

procedure TdxRibbonKeyTipWindow.CMEnabledChanged(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TdxRibbonKeyTipWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateBounds;
end;

procedure TdxRibbonKeyTipWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

{ TdxRibbonCustomKeyTipWindows }

constructor TdxRibbonCustomKeyTipWindows.Create(ARibbon: TdxCustomRibbon);
begin
  inherited Create;
  FRibbon := ARibbon;
  FWindowList := TcxObjectList.Create;
  FNotificator := TcxFreeNotificator.Create(nil);
  FNotificator.AddSender(ARibbon);
  FNotificator.OnFreeNotification := FreeNotificationHandler;
end;

destructor TdxRibbonCustomKeyTipWindows.Destroy;
begin
  FNotificator.RemoveSender(Ribbon);
  FreeAndNil(FNotificator);
  FreeAndNil(FWindowList);
  inherited Destroy;
end;

// IdxBarKeyTipWindowsManager
procedure TdxRibbonCustomKeyTipWindows.Add(const ACaption: string;
  const ABasePoint: TPoint; AHorzAlign: TAlignment;
  AVertAlign: TcxAlignmentVert; AEnabled: Boolean; out AWindow: TObject);

  function GetWindowPosition(const AWindowSize: TSize; const ABasePoint: TPoint;
    AVertAlign: TcxAlignmentVert): TPoint;
  begin
    if Ribbon.UseRightToLeftAlignment then
      AHorzAlign := TdxRightToLeftLayoutConverter.ConvertAlignment(AHorzAlign);
    case AHorzAlign of
      taLeftJustify:
        Result.X := ABasePoint.X - AWindowSize.cx;
      taCenter:
        Result.X := ABasePoint.X - AWindowSize.cx div 2;
      taRightJustify:
        Result.X := ABasePoint.X;
    end;
    case AVertAlign of
      vaTop:
        Result.Y := ABasePoint.Y - AWindowSize.cy;
      vaCenter:
        Result.Y := ABasePoint.Y - AWindowSize.cy div 2;
      vaBottom:
        Result.Y := ABasePoint.Y;
    end;
  end;

  function GetKeyTipDisplayText: string;
  begin
    if ACaption = #27 then
      Result := 'ESC'
    else
      Result := ACaption;
  end;

var
  ATempWindow: TdxRibbonKeyTipWindow;
begin
  ATempWindow := TdxRibbonKeyTipWindow.Create(Ribbon);
  ATempWindow.Caption := GetKeyTipDisplayText;
  ATempWindow.Enabled := AEnabled;
  with GetWindowPosition(cxSize(ATempWindow.Width, ATempWindow.Height), ABasePoint, AVertAlign) do
  begin
    ATempWindow.Left := X;
    ATempWindow.Top := Y;
  end;
  FWindowList.Add(ATempWindow);
  AWindow := ATempWindow;
end;

procedure TdxRibbonCustomKeyTipWindows.Delete(AWindow: TObject);
var
  AIndex: Integer;
begin
  AIndex := FWindowList.IndexOf(AWindow);
  TdxRibbonKeyTipWindow(FWindowList[AIndex]).Free;
  FWindowList.Delete(AIndex);
end;

procedure TdxRibbonCustomKeyTipWindows.Initialize;
begin
  // nothing to do
end;

procedure TdxRibbonCustomKeyTipWindows.Show;
var
  AWindow: TdxRibbonKeyTipWindow;
  I: Integer;
  WP: HDWP;
begin
  if Count > 0 then
  begin
    WP := BeginDeferWindowPos(Count);
    try
      for I := 0 to Count - 1 do
      begin
        AWindow := TdxRibbonKeyTipWindow(FWindowList[I]);
        AWindow.ParentWindow := Application.Handle;
        DeferWindowPos(WP, AWindow.Handle, HWND_TOPMOST,
          AWindow.Left, AWindow.Top, AWindow.Width, AWindow.Height,
          SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER or SWP_SHOWWINDOW);
        AWindow.ShowKeyTip;
      end;
    finally
      EndDeferWindowPos(WP);
    end;
  end;
end;

procedure TdxRibbonCustomKeyTipWindows.FreeNotificationHandler(Sender: TComponent);
begin
  if Sender = FRibbon then
  begin
    _AddRef;
    try
      BarNavigationController.StopKeyboardHandling;
      FNotificator.RemoveSender(FRibbon);
      FRibbon := nil;
    finally
      _Release;
    end;
  end;
end;

function TdxRibbonCustomKeyTipWindows.GetColorScheme: TdxCustomRibbonSkin;
begin
  Result := FRibbon.ColorScheme;
end;

function TdxRibbonCustomKeyTipWindows.GetCount: Integer;
begin
  Result := FWindowList.Count;
end;

{ TdxRibbonKeyTipWindows }

procedure TdxRibbonKeyTipWindows.Initialize;
var
  AHelper: IdxRibbonFormControllerHelper;
begin
  if Supports(Ribbon.RibbonForm, IdxRibbonFormControllerHelper, AHelper) then
    AHelper.DoBeforeShowKeyTips;
end;

{ TdxRibbonScrollBar }

function TdxRibbonScrollBar.GetHelperClass: TcxScrollBarHelperClass;
begin
  Result := TdxRibbonScrollBarHelper;
end;

function TdxRibbonScrollBar.GetRibbon: TdxCustomRibbon;
begin
  if Parent is TdxCustomRibbon then
    Result := TdxCustomRibbon(Parent)
  else
    Result := nil;
end;

{ TdxRibbonScrollBarHelper }

function TdxRibbonScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TdxRibbonScrollBarPainter;
end;

{ TdxRibbonScrollBarPainter }

procedure TdxRibbonScrollBarPainter.DoDrawScrollBarPart(
  ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer = (
    DXBAR_NORMAL, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_DISABLED
  );
  PartMap: array[TScrollBarKind, TcxScrollBarPart] of Integer = (
    (0, DXBAR_SCROLLBARHORZ_LINEUP, DXBAR_SCROLLBARHORZ_LINEDOWN, DXBAR_SCROLLBARHORZ_THUMBNAIL,
      DXBAR_SCROLLBARHORZ_PAGEUP, DXBAR_SCROLLBARHORZ_PAGEDOWN),
    (0, DXBAR_SCROLLBARVERT_LINEUP, DXBAR_SCROLLBARVERT_LINEDOWN, DXBAR_SCROLLBARVERT_THUMBNAIL,
      DXBAR_SCROLLBARVERT_PAGEUP, DXBAR_SCROLLBARVERT_PAGEDOWN)
  );
begin
  if Skin <> nil then
    Skin.DrawBackground(ACanvas.Handle, R, PartMap[ScrollBar.Kind, APart], StateMap[AState])
  else
    inherited DoDrawScrollBarPart(ACanvas, R, APart, AState);
end;

procedure TdxRibbonScrollBarPainter.DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect);
const
  PartMap: array[TScrollBarKind] of Integer = (DXBAR_SCROLLBARHORZ_BACKGROUND, DXBAR_SCROLLBARVERT_BACKGROUND);
begin
  if Skin <> nil then
    Skin.DrawBackground(ACanvas.Handle, R, PartMap[ScrollBar.Kind])
  else
    inherited DrawScrollBarBackground(ACanvas, R);
end;

function TdxRibbonScrollBarPainter.GetScrollBar: TdxRibbonScrollBar;
begin
  Result := TdxRibbonScrollBarHelper(inherited ScrollBar).Owner.GetControl as TdxRibbonScrollBar;
end;

function TdxRibbonScrollBarPainter.GetSkin: IdxSkin;
begin
  Supports(ScrollBar.Ribbon, IdxSkin, Result);
end;

{ TdxRibbonSizeGrip }

procedure TdxRibbonSizeGrip.Draw(ACanvas: TCanvas);
var
  ASkin: IdxSkin;
begin
  if Supports(Ribbon, IdxSkin, ASkin) then
    ASkin.DrawBackground(ACanvas.Handle, ClientRect, DXBAR_SCROLLBOX_SIZEGRIPAREA)
  else
    inherited Draw(ACanvas);
end;

function TdxRibbonSizeGrip.GetRibbon: TdxCustomRibbon;
begin
  if Parent is TdxCustomRibbon then
    Result := TdxCustomRibbon(Parent)
  else
    Result := nil;
end;

initialization
  FRibbonList := TList.Create;
  RegisterClasses([TdxRibbonTab]);
  dxBarRegisterItem(TdxRibbonQuickAccessPopupSubItem, TdxRibbonQuickAccessPopupSubItemControl, False);
  dxBarRegisterItem(TdxRibbonQuickAccessPopupSubItemButton, TdxRibbonQuickAccessPopupSubItemButtonControl, False);
  dxBarRegisterItem(TdxRibbonQuickAccessGroupButton, TdxRibbonQuickAccessGroupButtonControl, False);
  BarDesignController.RegisterBarControlDesignHelper(TdxRibbonGroupBarControl, TdxRibbonGroupBarControlDesignHelper);
  BarDesignController.RegisterBarControlDesignHelper(TdxRibbonQuickAccessToolbarBarControl, TdxRibbonQuickAccessToolbarBarControlDesignHelper);

  dxBarGetRootAccessibleObject := GetRootRibbonAccessibilityHelper;

finalization
  dxBarGetRootAccessibleObject := nil;
  BarDesignController.UnregisterBarControlDesignHelper(TdxRibbonQuickAccessToolbarBarControl, TdxRibbonQuickAccessToolbarBarControlDesignHelper);
  BarDesignController.UnregisterBarControlDesignHelper(TdxRibbonGroupBarControl, TdxRibbonGroupBarControlDesignHelper);
  dxBarUnregisterItem(TdxRibbonQuickAccessGroupButton);
  dxBarUnregisterItem(TdxRibbonQuickAccessPopupSubItemButton);
  dxBarUnregisterItem(TdxRibbonQuickAccessPopupSubItem);

  dxReleaseHook(dxRibbonMouseHook);
  FreeAndNil(FRibbonList);
end.

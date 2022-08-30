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

unit dxRibbonBackstageView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, SysUtils, Messages, Controls, Graphics, Math, Forms, ImgList,
  cxGraphics, cxGeometry, cxControls, cxClasses, dxBar, dxRibbon, dxRibbonSkins,
  dxCore, dxCoreClasses, dxMessages, dxBarStrs, StdCtrls,
  cxAccessibility, dxBarAccessibility,
  dxAnimation, dxRibbonFormCaptionHelper, cxLookAndFeelPainters, cxScrollBar, cxStyles, cxLookAndFeels, dxCoreGraphics,
  dxGDIPlusClasses;

const
  dxRibbonBackstageViewMinMenuWidth: Integer = 132;
  dxRibbonBackstageViewMinOwnerHeight: Integer = 400;
  dxRibbonBackstageViewMinOwnerWidth: Integer = 500;

  dxRibbonBackstageViewMenuScrollAnimationTime: Integer = 500;

type
  TdxRibbonBackstageViewFonts = class;
  TdxRibbonBackstageViewMenuBarButton = class;
  TdxRibbonBackstageViewMenuBarControl = class;
  TdxRibbonBackstageViewMenuButton = class;
  TdxRibbonBackstageViewMenuButtons = class;
  TdxRibbonBackstageViewMenuDockControl = class;
  TdxRibbonBackstageViewMenuViewInfo = class;
  TdxRibbonBackstageViewPainter = class;
  TdxRibbonBackstageViewTabSheet = class;
  TdxRibbonBackstageViewTabSheetButtonList = class;
  TdxRibbonCustomBackstageView = class;

  TdxRibbonBackstageViewChange = (rbvcStruct, rbvcItemsData, rbvcTabsData);
  TdxRibbonBackstageViewChanges = set of TdxRibbonBackstageViewChange;

  TdxRibbonBackstageViewMenuButtonPosition = (mbpBeforeTabs, mbpAfterTabs);

  { IdxRibbonBackstageViewSelectableItem }

  IdxRibbonBackstageViewSelectableItem = interface
  ['{D5E058AB-1C90-4D21-BE0A-EB48530EF53B}']
    procedure SelectionChanged;
  end;

  { IdxRibbonBackstageViewKeyTipPositionInfo }

  IdxRibbonBackstageViewKeyTipPositionInfo = interface
  ['{7947E0B2-BBED-4AE0-9C34-5B578B412780}']
    function GetKeyTipBasePoint: TPoint;
  end;

  { TdxRibbonBackstageViewCustomObject }

  TdxRibbonBackstageViewCustomObject = class(TcxIUnknownObject)
  private
    FBackstageView: TdxRibbonCustomBackstageView;

    function GetBarManager: TdxBarManager;
    function GetIsBarManagerValid: Boolean;
    function GetPainter: TdxRibbonBackstageViewPainter;
    function GetRibbon: TdxCustomRibbon;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); virtual;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
    property BarManager: TdxBarManager read GetBarManager;
    property IsBarManagerValid: Boolean read GetIsBarManagerValid;
    property Painter: TdxRibbonBackstageViewPainter read GetPainter;
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonBackstageViewCustomViewInfo }

  TdxRibbonBackstageViewCustomViewInfo = class(TdxRibbonBackstageViewCustomObject)
  strict private
    FBounds: TRect;
    FIsRightToLeftConverted: Boolean;
  protected
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
  public
    procedure Calculate(const ABounds: TRect); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    //
    property Bounds: TRect read FBounds;
  end;

  { TdxRibbonBackstageViewMenuBarAccessibilityHelper }

  TdxRibbonBackstageViewMenuBarAccessibilityHelper = class(TdxRibbonBarControlAccessibilityHelper)
  strict private
    FKeyTipWindowsManager: IdxBarKeyTipWindowsManager;

    function GetActiveTab: TdxRibbonBackstageViewTabSheet;
    function GetBackstageView: TdxRibbonCustomBackstageView;
    function GetMenuBarControl: TdxRibbonBackstageViewMenuBarControl;
  protected
    function AreKeyTipsSupported(out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean; override;
    function CanUnselectOnKeyDown(AKey: Word; AKeyTipsWereHidden: Boolean): Boolean; override;
    function HandleNavigationKey(var AKey: Word): Boolean; override;
    procedure InitializeItemKeyTipPosition(AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo); override;
    procedure KeyTipsEscapeHandler; override;
    procedure Select(ASetFocus: Boolean); override;
  public
    function GetDefaultSelectableObject(ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper; override;
    //
    property ActiveTab: TdxRibbonBackstageViewTabSheet read GetActiveTab;
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
    property MenuBarControl: TdxRibbonBackstageViewMenuBarControl read GetMenuBarControl;
  end;

  { TdxRibbonBackstageViewMenuBarControl }

  TdxRibbonBackstageViewMenuBarControl = class(TdxRibbonCustomBarControl)
  strict private
    function GetBackstageView: TdxRibbonCustomBackstageView;
    function GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure DoBarGetFocus(ASelectedItem: TdxBarItemControl); override;
    procedure DoBarMouseDown(Button: TMouseButton; Shift: TShiftState;
      const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean); override;
    function CanAlignControl(AControl: TdxBarItemControl): Boolean; override;
    function CanCustomizing: Boolean; override;
    function CanDrawClippedItem(AItemRect: TRect): Boolean; override;
    function CalcColumnItemRect(AItemLink: TdxBarItemLink; const AItemsRect: TRect): TRect; override;
    procedure CalcColumnItemRects(ATopIndex: Integer; out ALastItemBottom: Integer); override;
    procedure CalcControlsPositions; override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetCaption: TCaption; override;
    function GetFont: TFont; override;
    function GetItemControlOffset(AItemLink: TdxBarItemLink): Integer; override;
    function GetItemsRectOffset: TRect; override;
    function GetMaxWidth(AStyle: TdxBarDockingStyle): Integer; override;
    function GetMinWidth(AStyle: TdxBarDockingStyle): Integer; override;
    function GetNextBarControl(AForward: Boolean): TdxBarControl; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    function HasCaptionButtons: Boolean; override;
    function IsInternal: Boolean; override;
    procedure NavigationHandler(var ACharCode: Word; AShiftState: TShiftState); override;
    function NeedsMouseWheel: Boolean; override;
    procedure MakeItemControlFullyVisible(AItemControl: TdxBarItemControl); override;
    function PreProcessKey(AKey: Word; AShift: TShiftState): Boolean; override;
    procedure SetLayeredAttributes; override;
    procedure ShowPopup(AItem: TdxBarItemControl); override;
    procedure WndProc(var Message: TMessage); override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read GetMenuViewInfo;
  public
    function IsVertical: Boolean; override;
  end;

  { TdxRibbonBackstageViewMenuBarControlViewInfo }

  TdxRibbonBackstageViewMenuBarControlViewInfo = class(TdxBarControlViewInfo)
  protected
    procedure DoCalcSeparatorInfo(AItemLink: TdxBarItemLink; const AItemRect: TRect); override;
  end;

  { TdxRibbonBackstageViewKeyTipWindows }

  TdxRibbonBackstageViewKeyTipWindows = class(TdxRibbonCustomKeyTipWindows);

  { TdxRibbonBackstageViewPainter }

  TdxRibbonBackstageViewPainter = class(TcxIUnknownObject, IdxCustomSkinnedContainer)
  strict private
    FBackstageView: TdxRibbonCustomBackstageView;
  protected
    function GetBackButtonOffset: Integer; virtual;
    function GetBackButtonSize: TSize; virtual;
    function GetContentOffsets: TRect; virtual;
    function GetSkin: IdxSkin; virtual;

    // IdxCustomSkinnedContainer
    function GetDefaultTextColor(AEnabled: Boolean): TColor;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView);
    function CanShowTabAreaToolbar: Boolean; virtual;
    procedure DrawBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawMenuBarHeader(ACanvas: TcxCanvas; const R: TRect); virtual;
    //
    property BackButtonOffset: Integer read GetBackButtonOffset;
    property BackButtonSize: TSize read GetBackButtonSize;
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
    property ContentOffsets: TRect read GetContentOffsets;
    property Skin: IdxSkin read GetSkin;
  end;

  { TdxRibbonBackstageViewMenuPainter }

  TdxRibbonBackstageViewMenuPainter = class(TdxBarSkinnedPainter)
  protected
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    procedure DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC); override;
    procedure GetDisabledTextColors(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;
    function GetEnabledTextColor(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean): TColor; override;
  public
    function BarBeginGroupSize: Integer; override;
    procedure BarDrawBeginGroup(ABarControl: TCustomdxBarControl; DC: HDC;
      ABeginGroupRect: TRect; AToolbarBrush: HBRUSH; AHorz: Boolean); override;
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl;
      APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette; override;
    function GetToolbarContentOffsets(ABar: TdxBar; ADockingStyle: TdxBarDockingStyle;
      AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect; override;
    procedure DockControlFillBackground(ADockControl: TdxDockControl; DC: HDC;
      ADestR: TRect; ASourceR: TRect; AWholeR: TRect; ABrush: HBRUSH; AColor: TColor); override;

    // Button
    procedure DrawButtonBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams); override;
    function MenuBarButtonContentOffset: TRect; virtual;
    function MenuBarButtonTextColor(const ADrawParams: TdxBarButtonLikeControlDrawParams): TColor; virtual;

    // Tabs
    procedure DrawTabButtonBackground(DC: HDC; R: TRect; AState: Integer); virtual;
    function GetTabButtonState(const ADrawParams: TdxBarButtonLikeControlDrawParams): Integer; virtual;
    function TabButtonContentOffset: TRect; virtual;
    function TabButtonDefaultHeight: Integer; virtual;
    function TabButtonTextColor(AState: Integer): TColor; overload; virtual;
    function TabButtonTextColor(const ADrawParams: TdxBarButtonLikeControlDrawParams): TColor; overload; virtual;

    function MenuBarDefaultItemHeight: Integer; virtual;
    function MenuBarGetMinWidth: Integer; virtual;
    function MenuBarIndentBetweenItems: Integer; virtual;
    function MenuBarItemsRectOffset: TRect; virtual;
  end;

  { TdxRibbonBackstageViewMenuDockControl }

  TdxRibbonBackstageViewMenuDockControl = class(TdxBarDockControl)
  strict private
    FMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;

    function GetBackstageView: TdxRibbonCustomBackstageView;
    function GetRibbon: TdxCustomRibbon;
  protected
    procedure CalcLayout; override;
    function CanCustomize: Boolean; override;
    function GetClientSize: Integer; override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
    function GetDockingStyle: TdxBarDockingStyle; override;
    function GetMinSize: Integer; override;
    function GetPainter: TdxBarPainter; override;
    function GetSunkenBorder: Boolean; override;
    function IsDrawDesignBorder: Boolean; override;
    procedure ShowCustomizePopup; override;

    // IdxRibbonToolBarContainer
    function GetContainer: TObject;
    //
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  public
    constructor Create(AMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo); reintroduce;
    //
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read FMenuViewInfo;
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonBackstageViewMenuBarCustomButtonControl }

  TdxRibbonBackstageViewMenuBarCustomButtonControl = class(TdxBarButtonControl,
    IdxRibbonBackstageViewKeyTipPositionInfo)
  strict private
    function GetMenuPainter: TdxRibbonBackstageViewMenuPainter; inline;
    function GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo; inline;
  protected
    procedure CalcDrawParams(AFull: Boolean = True); override;
    function CalculateContentOffsets: TRect; virtual;
    function CanCustomize: Boolean; override;
    function CanDestroyOnClick: Boolean; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    procedure Draw(const ADrawParams: TdxBarButtonLikeControlDrawParams; const R: TRect); virtual;
    procedure DrawBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams; const R: TRect); virtual;
    procedure DrawContent(const ADrawParams: TdxBarButtonLikeControlDrawParams; R: TRect); virtual;
    function GetBackstageView: TdxRibbonCustomBackstageView; virtual; abstract;
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetViewStructure: TdxBarItemControlViewStructure; override;
    function HasImage: Boolean; virtual;
    function Images: TCustomImageList;
    function IsTabButtonStyle: Boolean; virtual;
    function IsWordWrapSupported: Boolean; virtual;

     // IdxRibbonBackstageViewKeyTipPositionInfo
    function GetKeyTipBasePoint: TPoint; virtual;
  public
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
    property MenuPainter: TdxRibbonBackstageViewMenuPainter read GetMenuPainter;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read GetMenuViewInfo;
  end;

  { TdxRibbonBackstageViewTabSheetViewInfo }

  TdxRibbonBackstageViewTabSheetViewInfo = class
  strict private
    FBounds: TRect;
    FTab: TdxRibbonBackstageViewTabSheet;

    function GetFrameAreaVisibleBounds: TRect;
  protected
    function GetMinHeight: Integer; virtual;
    function GetMinWidth: Integer; virtual;
  public
    constructor Create(ATab: TdxRibbonBackstageViewTabSheet); virtual;
    procedure Calculate(const R: TRect); virtual;
    procedure ValidateWindowPos(var APos: TWindowPos);
    //
    property Bounds: TRect read FBounds;
    property FrameAreaVisibleBounds: TRect read GetFrameAreaVisibleBounds;
    property MinHeight: Integer read GetMinHeight;
    property MinWidth: Integer read GetMinWidth;
    property Tab: TdxRibbonBackstageViewTabSheet read FTab;
  end;

  { TdxRibbonBackstageViewTabSheetSizeOptions }

  TdxRibbonBackstageViewTabSheetSizeOptions = class(TPersistent)
  strict private
    FHasChanges: Boolean;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FTab: TdxRibbonBackstageViewTabSheet;
    FUpdateCount: Integer;

    function GetAutoSize: Boolean;
    procedure SetAutoSize(AValue: Boolean);
    procedure SetMinHeight(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
  protected
    procedure Changed;
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(ATab: TdxRibbonBackstageViewTabSheet); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    property MinHeight: Integer read FMinHeight write SetMinHeight default 0;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
  end;

  { TdxRibbonBackstageViewTabSheet }

  TdxRibbonBackstageViewTabSheet = class(TcxControl, IdxCustomSkinnedContainer)
  strict private
    FBackstageView: TdxRibbonCustomBackstageView;
    FBeginGroup: Boolean;
    FGlyph: TdxSmartGlyph;
    FImageIndex: TcxImageIndex;
    FKeyTip: string;
    FSizeOptions: TdxRibbonBackstageViewTabSheetSizeOptions;
    FTabVisible: Boolean;
    FViewInfo: TdxRibbonBackstageViewTabSheetViewInfo;

    function GetActive: Boolean;
    function GetCanBeActive: Boolean;
    function GetImages: TCustomImageList;
    function GetPageIndex: Integer;
    function GetPainter: TdxRibbonBackstageViewPainter;
    procedure SetActive(AValue: Boolean);
    procedure SetBackstageView(AValue: TdxRibbonCustomBackstageView);
    procedure SetBeginGroup(AValue: Boolean);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetKeyTip(const AValue: string);
    procedure SetPageIndex(AValue: Integer);
    procedure SetSizeOptions(AValue: TdxRibbonBackstageViewTabSheetSizeOptions);
    procedure SetTabVisible(AValue: Boolean);
    //
    procedure HandlerGlyphChanged(Sender: TObject);
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CreateViewInfo: TdxRibbonBackstageViewTabSheetViewInfo; virtual;
    procedure Activate; virtual;
    procedure AdjustSize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Calculate(const R: TRect); virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure Changed(AChanges: TdxRibbonBackstageViewChanges = [rbvcTabsData]); virtual;
    procedure Deactivate; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure EnabledChanged; override;
    procedure NCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure RefreshNonClientArea;
    procedure SetParent(AParent: TWinControl); override;
    //
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WndProc(var Message: TMessage); override;
    //
    property CanBeActive: Boolean read GetCanBeActive;
    property Painter: TdxRibbonBackstageViewPainter read GetPainter implements IdxCustomSkinnedContainer;
    property ViewInfo: TdxRibbonBackstageViewTabSheetViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView write SetBackstageView;
    property Images: TCustomImageList read GetImages;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property BeginGroup: Boolean read FBeginGroup write SetBeginGroup default False;
    property BorderWidth;
    property Caption;
    property Enabled;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Height stored False;
    property KeyTip: string read FKeyTip write SetKeyTip;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property SizeOptions: TdxRibbonBackstageViewTabSheetSizeOptions read FSizeOptions write SetSizeOptions;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Width stored False;
    //
    property OnResize;
  end;

  { TdxRibbonBackstageViewTabSheets }

  TdxRibbonBackstageViewTabSheets = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer): TdxRibbonBackstageViewTabSheet;
  public
    property Items[Index: Integer]: TdxRibbonBackstageViewTabSheet read GetItem; default;
  end;

  { TdxRibbonBackstageViewTabSheetButton }

  TdxRibbonBackstageViewTabSheetButton = class(TdxBarButton)
  strict private
    FTab: TdxRibbonBackstageViewTabSheet;

    procedure SetTab(AValue: TdxRibbonBackstageViewTabSheet);
  protected
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DirectClick; override;
    procedure RefreshInfo;
    //
    property Tab: TdxRibbonBackstageViewTabSheet read FTab write SetTab;
  end;

  { TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper }

  TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper = class(TdxBarButtonControlAccessibilityHelper)
  strict private
    FAccessibilityChildren: TList;
    FKeyTipWindowsManager: IdxBarKeyTipWindowsManager;

    function GetTabSheet: TdxRibbonBackstageViewTabSheet;
  protected
    function GetBarManager: TdxBarManager; override;
    function GetDefaultAccessibleObject: IdxBarAccessibilityHelper; override;

    procedure AddChild(AChild: TcxAccessibilityHelper); virtual;
    procedure DeleteChild(AChild: TcxAccessibilityHelper); virtual;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetChildIndex(AChild: TcxAccessibilityHelper): Integer; override;

    function AreKeyTipsSupported(out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean; override;
    procedure KeyTipHandler(Sender: TObject); override;
    procedure KeyTipsEscapeHandler; override;

    property TabSheet: TdxRibbonBackstageViewTabSheet read GetTabSheet;
  public
    constructor Create(AOwnerObject: TObject); override;
    destructor Destroy; override;
  end;

  { TdxRibbonBackstageViewTabSheetButtonControl }

  TdxRibbonBackstageViewTabSheetButtonControl = class(TdxRibbonBackstageViewMenuBarCustomButtonControl)
  strict private
    function GetItem: TdxRibbonBackstageViewTabSheetButton;
  protected
    procedure ActivateTab;
    procedure ControlActivate(AImmediately: Boolean; AByMouse: Boolean); override;
    procedure ControlClick(AByMouse: Boolean; AKey: Char = #0); override;
    procedure ControlUnclick(AByMouse: Boolean); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetBackstageView: TdxRibbonCustomBackstageView; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
    function GetViewStructure: TdxBarItemControlViewStructure; override;
    function IsTabButtonStyle: Boolean; override;
    function IsWordWrapSupported: Boolean; override;
    procedure PrepareCanvasFont(ABaseFont: HFONT; AStyle: TcxStyle; out ASavedFont: TdxBarSavedFont); override;
  public
    property Item: TdxRibbonBackstageViewTabSheetButton read GetItem;
  end;

  { TdxRibbonBackstageViewTabSheetButtonList }

  TdxRibbonBackstageViewTabSheetButtonList = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer): TdxRibbonBackstageViewTabSheetButton;
  public
    function GetItemByTab(ATab: TdxRibbonBackstageViewTabSheet): TdxRibbonBackstageViewTabSheetButton;
    procedure RefreshInfo;
    procedure RemoveTab(ATab: TdxRibbonBackstageViewTabSheet);
    //
    property Items[Index: Integer]: TdxRibbonBackstageViewTabSheetButton read GetItem; default;
  end;

  { TdxRibbonBackstageViewMenuButtonList }

  TdxRibbonBackstageViewMenuButtonList = class(TcxObjectList)
  strict private
    function GetItem(Index: Integer): TdxRibbonBackstageViewMenuBarButton;
  public
    procedure RefreshInfo;
    //
    property Items[Index: Integer]: TdxRibbonBackstageViewMenuBarButton read GetItem; default;
  end;

  { TdxRibbonBackstageViewMenuViewInfo }

  TdxRibbonBackstageViewMenuViewInfo = class(TdxRibbonBackstageViewCustomViewInfo, IdxBarLinksOwner)
  strict private
    FDockControl: TdxRibbonBackstageViewMenuDockControl;
    FItemLinks: TdxBarItemLinks;
    FMenuButtonList: TdxRibbonBackstageViewMenuButtonList;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FPainter: TdxRibbonBackstageViewMenuPainter;
    FTabButtonList: TdxRibbonBackstageViewTabSheetButtonList;

    function GetAccessibilityHelper: IdxBarAccessibilityHelper;
    function GetBarControl: TdxRibbonBackstageViewMenuBarControl;
    function GetFonts: TdxRibbonBackstageViewFonts;
    function GetIsDesigning: Boolean;
  protected
    procedure AddButton(AButton: TdxRibbonBackstageViewMenuButton;
      APosition: TdxRibbonBackstageViewMenuButtonPosition; var ABeginGroup: Boolean);
    procedure AddButtons(APosition: TdxRibbonBackstageViewMenuButtonPosition; ABeginGroup: Boolean);
    procedure AddTabButton(ATab: TdxRibbonBackstageViewTabSheet; var ABeginGroup: Boolean);
    procedure AddTabButtons(ABeginGroup: Boolean);

    function CreatePainter(AData: TdxNativeUInt): TdxRibbonBackstageViewMenuPainter; virtual;
    procedure CreateBarControl;
    procedure CreateViewInfoItems;
    procedure DestroyViewInfoItems;

    // IdxBarLinksOwner
    function CanContainItem(AItem: TdxBarItem; out AErrorText: string): Boolean;
    function CreateItemLinksBarControl: TCustomdxBarControl;
    function GetImages: TCustomImageList;
    function GetInstance: TComponent;
    function GetItemLinks: TdxBarItemLinks;
    function IdxBarLinksOwner.CreateBarControl = CreateItemLinksBarControl;
    function IsLoading: Boolean;

    function ShowSeparatorBetweenTabsAndButtons: Boolean;

    property IsDesigning: Boolean read GetIsDesigning;
    property MenuButtonList: TdxRibbonBackstageViewMenuButtonList read FMenuButtonList;
    property Painter: TdxRibbonBackstageViewMenuPainter read FPainter;
    property TabButtonList: TdxRibbonBackstageViewTabSheetButtonList read FTabButtonList;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateSizes;
    procedure ClearInternalLists;
    procedure InitiateActions;
    procedure RecreateItemLinks;
    procedure RefreshMenuButtonsInfo;
    procedure RefreshTabsInfo;
    procedure UpdateFont;
    //
    property AccessibilityHelper: IdxBarAccessibilityHelper read GetAccessibilityHelper;
    property BarControl: TdxRibbonBackstageViewMenuBarControl read GetBarControl;
    property DockControl: TdxRibbonBackstageViewMenuDockControl read FDockControl;
    property Fonts: TdxRibbonBackstageViewFonts read GetFonts;
    property ItemLinks: TdxBarItemLinks read FItemLinks;
    property MinHeight: Integer read FMinHeight;
    property MinWidth: Integer read FMinWidth;
  end;

  { TdxRibbonBackstageViewViewInfo }

  TdxRibbonBackstageViewViewInfo = class(TdxRibbonBackstageViewCustomViewInfo)
  strict private
    FContentHeight: Integer;
    FContentWidth: Integer;
    FFrameAreaBounds: TRect;
    FMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
    FScrollPositionX: Integer;
    FScrollPositionY: Integer;

    function GetActiveTab: TdxRibbonBackstageViewTabSheet;
    function GetActiveTabItemControl: TdxBarItemControl;
    function GetContentBounds: TRect;
    function GetDesignSelectorRect: TRect;
    function GetFrameAreaVisibleBounds: TRect;
    function GetMenuBarControl: TdxBarControl;
    procedure CheckScrollPosition(var AValue: Integer; AContentSize, ADisplaySize: Integer);
    procedure SetScrollPositionX(AValue: Integer);
    procedure SetScrollPositionY(AValue: Integer);
  protected
    function CalculateFrameBounds: TRect; virtual;
    function CalculateMenuBounds: TRect; virtual;
    function CalculateMinHeight: Integer; virtual;
    function CalculateMinWidth: Integer; virtual;
    function CreateMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure DeactivateControls; virtual;
    function ProcessMouseWheel(ALineDown: Boolean): Boolean;
    //
    property ActiveTab: TdxRibbonBackstageViewTabSheet read GetActiveTab;
    property ActiveTabItemControl: TdxBarItemControl read GetActiveTabItemControl;
    property ContentBounds: TRect read GetContentBounds;
    property ContentHeight: Integer read FContentHeight;
    property ContentWidth: Integer read FContentWidth;
    property DesignSelectorRect: TRect read GetDesignSelectorRect;
    property FrameAreaBounds: TRect read FFrameAreaBounds;
    property FrameAreaVisibleBounds: TRect read GetFrameAreaVisibleBounds;
    property MenuBarControl: TdxBarControl read GetMenuBarControl;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read FMenuViewInfo;
    property ScrollPositionX: Integer read FScrollPositionX write SetScrollPositionX;
    property ScrollPositionY: Integer read FScrollPositionY write SetScrollPositionY;
  end;

  { TdxRibbonBackstageViewMenuBarButton }

  TdxRibbonBackstageViewMenuBarButton = class(TdxBarButton)
  strict private
    FMenuButton: TdxRibbonBackstageViewMenuButton;

    procedure SetMenuButton(AValue: TdxRibbonBackstageViewMenuButton);
  protected
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;
  public
    procedure DirectClick; override;
    procedure RefreshInfo;
    //
    property MenuButton: TdxRibbonBackstageViewMenuButton read FMenuButton write SetMenuButton;
  end;

  { TdxRibbonBackstageViewMenuButton }

  TdxRibbonBackstageViewMenuButton = class(TcxInterfacedCollectionItem,
    IdxRibbonBackstageViewSelectableItem,
    IdxBarComponentListener)
  strict private
    FBeginGroup: Boolean;
    FItem: TdxBarCustomButton;
    FPosition: TdxRibbonBackstageViewMenuButtonPosition;

    function GetCollection: TdxRibbonBackstageViewMenuButtons;
    procedure SetBeginGroup(AValue: Boolean);
    procedure SetItem(AValue: TdxBarCustomButton);
    procedure SetPosition(AValue: TdxRibbonBackstageViewMenuButtonPosition);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    // IdxRibbonBackstageViewSelectableItem
    procedure SelectionChanged;
    // IdxBarComponentListener
    procedure IdxBarComponentListener.EnabledChanged = BarComponentChanged;
    procedure IdxBarComponentListener.Changed = BarComponentChanged;
    procedure BarComponentChanged(AComponent: TdxBarComponent);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //
    property Collection: TdxRibbonBackstageViewMenuButtons read GetCollection;
  published
    property BeginGroup: Boolean read FBeginGroup write SetBeginGroup default False;
    property Item: TdxBarCustomButton read FItem write SetItem;
    property Position: TdxRibbonBackstageViewMenuButtonPosition read FPosition write SetPosition default mbpBeforeTabs;
  end;

  { TdxRibbonBackstageViewMenuButtons }

  TdxRibbonBackstageViewMenuButtons = class(TCollection)
  strict private
    FBackstageView: TdxRibbonCustomBackstageView;

    function GetItem(Index: Integer): TdxRibbonBackstageViewMenuButton;
    procedure SetItem(Index: Integer; Value: TdxRibbonBackstageViewMenuButton);
  protected
    function GetOwner: TPersistent; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure Update(Item: TCollectionItem); override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView);
    function Add: TdxRibbonBackstageViewMenuButton;
    function IndexOf(AItem: TdxRibbonBackstageViewMenuButton): Integer;
    function Insert(AIndex: Integer): TdxRibbonBackstageViewMenuButton;
    //
    property Items[Index: Integer]: TdxRibbonBackstageViewMenuButton read GetItem write SetItem; default;
  end;

  { TdxRibbonBackstageViewMenuBarButtonControl }

  TdxRibbonBackstageViewMenuBarButtonControl = class(TdxRibbonBackstageViewMenuBarCustomButtonControl)
  strict private
    function GetItem: TdxRibbonBackstageViewMenuBarButton;
  protected
    function GetBackstageView: TdxRibbonCustomBackstageView; override;
    function IsTabButtonStyle: Boolean; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
  public
    property Item: TdxRibbonBackstageViewMenuBarButton read GetItem;
  end;

  { TdxRibbonBackstageViewMenuHelper }

  TdxRibbonBackstageViewMenuHelper = class(TcxMessageWindow)
  strict private
    FBackstageView: TdxRibbonCustomBackstageView;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); reintroduce;
    procedure PostClick(AButton: TdxRibbonBackstageViewMenuButton);
    procedure PostShowRibbonKeyTips;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
  end;

  { TdxRibbonBackstageViewApplicationMenuHelper }

  TdxRibbonBackstageViewApplicationMenuHelper = class(TInterfacedObject, IdxRibbonApplicationMenu)
  strict private
    FLinkToBackstageView: TcxObjectLink;

    function GetBackstageView: TdxRibbonCustomBackstageView;
  protected
    // IdxRibbonApplicationMenu
    function CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
    function ClosePopup: Boolean;
    function GetDisplayMode: TdxRibbonApplicationMenuDisplayMode; virtual;
    function GetOrigin(AIsClientArea: Boolean): TPoint; virtual;
    function GetRootAccessibilityHelper: IdxBarAccessibilityHelper; virtual;
    function IsVisible: Boolean;
    function Popup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean; virtual;
    procedure GetTabOrderList(AList: TList); virtual;
    procedure RibbonFormResized; virtual;
    procedure SelectAppMenuFirstItemControl; virtual;
    procedure ShowKeyTips; virtual;
    procedure UpdateNonClientArea; virtual;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); virtual;
    destructor Destroy; override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read GetBackstageView;
  end;

  { TdxRibbonBackstageViewCustomButtonViewInfo }

  TdxRibbonBackstageViewCustomButtonViewInfo = class(TdxRibbonBackstageViewCustomViewInfo)
  strict private
    function GetState: TcxButtonState;
  public
    procedure Click; virtual; abstract;
    //
    property State: TcxButtonState read GetState;
  end;

  { TdxRibbonBackstageViewBackButtonViewInfo }

  TdxRibbonBackstageViewBackButtonViewInfo = class(TdxRibbonBackstageViewCustomButtonViewInfo)
  public
    procedure Click; override;
    procedure Draw(ACanvas: TcxCanvas); virtual;
  end;

  { TdxRibbonBackstageViewNonClientViewInfo }

  TdxRibbonBackstageViewNonClientViewInfo = class(TdxRibbonBackstageViewCustomViewInfo)
  strict private
    FBackButtonViewInfo: TdxRibbonBackstageViewBackButtonViewInfo;

    function GetClientRect: TRect;
    function GetContentRect: TRect;
    function GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
    function GetHasNonClientArea: Boolean;
    function GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
    function GetRibbon: TdxCustomRibbon;
    function GetRibbonFormCaptionHelper: TdxRibbonFormCaptionHelper;
  protected
    FBackgroundImageRect: TRect;
    FBorderIconsArea: TRect;
    FCaptionArea: TRect;
    FCaptionTextRect: TRect;
    FMenuBarHeaderRect: TRect;
    FTabsArea: TRect;
    FTabAreaToolbarArea: TRect;

    procedure CalculateBackButton; virtual;
    function CalculateBackButtonBounds: TRect; virtual;
    procedure CalculateCaptionArea; virtual;
    procedure CalculateMenuBarHeader; virtual;
    function CreateBackButtonViewInfo: TdxRibbonBackstageViewBackButtonViewInfo; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawCaptionArea(ACanvas: TcxCanvas); virtual;
    procedure DrawMenuBarHeader(ACanvas: TcxCanvas); virtual;
    function GetClientOffsets: TRect; virtual;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); override;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    function CreateWindowRegion: TcxRegionHandle; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure Recalculate;
    //
    property BackButtonViewInfo: TdxRibbonBackstageViewBackButtonViewInfo read FBackButtonViewInfo;
    property BackgroundImageRect: TRect read FBackgroundImageRect;
    property BorderIconsArea: TRect read FBorderIconsArea;
    property CaptionArea: TRect read FCaptionArea;
    property CaptionTextRect: TRect read FCaptionTextRect;
    property ClientOffsets: TRect read GetClientOffsets;
    property ClientRect: TRect read GetClientRect;
    property ContentRect: TRect read GetContentRect;
    property DisplayMode: TdxRibbonApplicationMenuDisplayMode read GetDisplayMode;
    property HasNonClientArea: Boolean read GetHasNonClientArea;
    property MenuBarHeaderRect: TRect read FMenuBarHeaderRect;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read GetMenuViewInfo;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property RibbonFormCaptionHelper: TdxRibbonFormCaptionHelper read GetRibbonFormCaptionHelper;
    property TabsArea: TRect read FTabsArea;
    property TabAreaToolbarArea: TRect read FTabAreaToolbarArea;
  end;

  { TdxRibbonBackstageViewNonClientController }

  TdxRibbonBackstageViewNonClientController = class(TdxRibbonBackstageViewCustomObject,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  strict private
    FHoveredCell: TdxRibbonBackstageViewCustomButtonViewInfo;
    FPressedCell: TdxRibbonBackstageViewCustomButtonViewInfo;

    function GetViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
    procedure SetHoveredCell(AValue: TdxRibbonBackstageViewCustomButtonViewInfo);
    procedure SetPressedCell(AValue: TdxRibbonBackstageViewCustomButtonViewInfo);
  protected
    function ScreenToLocal(const P: TPoint): TPoint; overload;
    function ScreenToLocal(const P: TSmallPoint): TPoint; overload;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
  public
    destructor Destroy; override;
    function HitTest(const P: TPoint): TdxRibbonBackstageViewCustomButtonViewInfo; virtual;
    procedure MouseDown(AButton: TMouseButton; const P: TPoint); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(const P: TPoint); virtual;
    procedure MouseUp(AButton: TMouseButton; const P: TPoint); virtual;
    procedure ProcessMessage(var AMessage: TMessage); virtual;
    procedure RefreshState;
    //
    property HoveredCell: TdxRibbonBackstageViewCustomButtonViewInfo read FHoveredCell write SetHoveredCell;
    property PressedCell: TdxRibbonBackstageViewCustomButtonViewInfo read FPressedCell write SetPressedCell;
    property ViewInfo: TdxRibbonBackstageViewNonClientViewInfo read GetViewInfo;
  end;

  { TdxRibbonBackstageViewAnimationTransition }

  TdxRibbonBackstageViewAnimationTransitionMode = (bvatmShow, bvatmHide);

  TdxRibbonBackstageViewAnimationTransition = class(TdxAnimationTransition)
  strict private
    FActiveTabBuffer: TcxBitmap;
    FActiveTabIsSolidBackground: Boolean;
    FActiveTabRect: TRect;
    FBackstageView: TdxRibbonCustomBackstageView;
    FBackstageViewBuffer: TcxBitmap;
    FBackstageViewMenuBuffer: TcxBitmap;
    FBackstageViewRect: TRect;
    FMode: TdxRibbonBackstageViewAnimationTransitionMode;

    function GetActiveTab: TdxRibbonBackstageViewTabSheet;
    function GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
    function GetNonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
    function GetViewInfo: TdxRibbonBackstageViewViewInfo;
  protected
    function CalculateContentAlpha: Byte; virtual;
    procedure CalculateViewInfos; virtual;
    procedure DoAnimate; override;
    procedure Finalize; virtual;
    procedure Initialize; virtual;
    procedure PrepareBackstageViewBuffer(ACanvas: TcxCanvas);
    //
    property ActiveTab: TdxRibbonBackstageViewTabSheet read GetActiveTab;
    property ActiveTabBuffer: TcxBitmap read FActiveTabBuffer;
    property ActiveTabIsSolidBackground: Boolean read FActiveTabIsSolidBackground;
    property ActiveTabRect: TRect read FActiveTabRect;
    property BackstageViewBuffer: TcxBitmap read FBackstageViewBuffer;
    property BackstageViewMenuBuffer: TcxBitmap read FBackstageViewMenuBuffer;
    property BackstageViewRect: TRect read FBackstageViewRect;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read GetMenuViewInfo;
    property NonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo read GetNonClientViewInfo;
    property ViewInfo: TdxRibbonBackstageViewViewInfo read GetViewInfo;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView;
      AMode: TdxRibbonBackstageViewAnimationTransitionMode); reintroduce; virtual;
    destructor Destroy; override;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
    property Mode: TdxRibbonBackstageViewAnimationTransitionMode read FMode;
  end;

  { TdxRibbonBackstageViewSizeGrip }

  TdxRibbonBackstageViewSizeGrip = class(TdxRibbonSizeGrip)
  protected
    function GetRibbon: TdxCustomRibbon; override;
  end;

  { TdxRibbonBackstageViewScrollBar }

  TdxRibbonBackstageViewScrollBar = class(TdxRibbonScrollBar)
  protected
    function GetRibbon: TdxCustomRibbon; override;
  end;

  { TdxRibbonBackstageViewFonts }

  TdxRibbonBackstageViewAssignedFont = (bvafMainMenuButton, bvafMainMenuTab);
  TdxRibbonBackstageViewAssignedFonts = set of TdxRibbonBackstageViewAssignedFont;

  TdxRibbonBackstageViewFonts = class(TPersistent)
  strict private
    FAssignedFonts: TdxRibbonBackstageViewAssignedFonts;
    FBackstageView: TdxRibbonCustomBackstageView;
    FFonts: array [TdxRibbonBackstageViewAssignedFont] of TFont;
    FLocked: Boolean;

    procedure FontChanged(Sender: TObject);
    function GetDefaultFont: TFont;
    function GetFont(AIndex: Integer): TFont;
    function IsFontStored(AIndex: Integer): Boolean;
    procedure SetAssignedFonts(AValue: TdxRibbonBackstageViewAssignedFonts);
    procedure SetFont(AIndex: Integer; AValue: TFont);
  protected
    procedure ChangeScale(M, D: Integer);
    procedure ResetFont(AFont: TFont; AIndex: TdxRibbonBackstageViewAssignedFont); virtual;
    procedure UpdateFonts; virtual;
    //
    property BackstageView: TdxRibbonCustomBackstageView read FBackstageView;
    property DefaultFont: TFont read GetDefaultFont;
  public
    constructor Create(ABackstageView: TdxRibbonCustomBackstageView); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AssignedFonts: TdxRibbonBackstageViewAssignedFonts read FAssignedFonts write SetAssignedFonts default [];
    property MainMenuButton: TFont index 0 read GetFont write SetFont stored IsFontStored;
    property MainMenuTab: TFont index 1 read GetFont write SetFont stored IsFontStored;
  end;

  { TdxRibbonBackstageViewDesignSelectorHelper }

  TdxRibbonBackstageViewDesignSelectorHelper = class(TdxControlsDesignSelectorHelper)
  protected
    function IsHitTestTransparent(const P: TPoint): Boolean; override;
  end;

  { TdxRibbonCustomBackstageView }

  TdxRibbonBackstageViewTabChanging = procedure (Sender: TObject;
    ANewTab: TdxRibbonBackstageViewTabSheet; var AAllowChange: Boolean) of object;
  TdxRibbonBackstageViewTabClickEvent = procedure (Sender: TObject; ATab: TdxRibbonBackstageViewTabSheet) of object;

  TdxRibbonCustomBackstageView = class(TcxControl,
    IdxRibbonListener,
    IdxRibbonBackstageViewSelectableItem,
    IdxRibbonMouseWheelReceiver)
  strict private
    FActiveTab: TdxRibbonBackstageViewTabSheet;
    FApplicationWndProcHooked: Boolean;
    FBarManagerHolder: TcxComponentHolder;
    FButtons: TdxRibbonBackstageViewMenuButtons;
    FChanges: TdxRibbonBackstageViewChanges;
    FDesignSelector: TdxRibbonBackstageViewDesignSelectorHelper;
    FFonts: TdxRibbonBackstageViewFonts;
    FIsRightToLeftConverted: Boolean;
    FMenuHelper: TdxRibbonBackstageViewMenuHelper;
    FNonClientController: TdxRibbonBackstageViewNonClientController;
    FNonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
    FPainter: TdxRibbonBackstageViewPainter;
    FRibbon: TdxCustomRibbon;
    FShowMainMenu: Boolean;
    FUpdateCount: Integer;
    FViewInfo: TdxRibbonBackstageViewViewInfo;

    FOnCanClose: TCloseQueryEvent;
    FOnCloseUp: TNotifyEvent;
    FOnPopup: TNotifyEvent;
    FOnTabChanged: TNotifyEvent;
    FOnTabChanging: TdxRibbonBackstageViewTabChanging;
    FOnTabClick: TdxRibbonBackstageViewTabClickEvent;

    procedure CheckAssignRibbon;
    procedure CheckZOrder;
    function CanActivateTab(ATab: TdxRibbonBackstageViewTabSheet): Boolean;
    function GetBarManager: TdxBarManager;
    function GetIsBarManagerValid: Boolean;
    function GetIsRibbonValid: Boolean;
    function GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
    function GetRibbonStyle: TdxRibbonStyle;
    function GetTabCount: Integer;
    function GetTabs(Index: Integer): TdxRibbonBackstageViewTabSheet;
    procedure RibbonAfterChange;
    procedure RibbonBeforeChange;
    procedure SetActiveTab(AValue: TdxRibbonBackstageViewTabSheet);
    procedure SetButtons(AValue: TdxRibbonBackstageViewMenuButtons);
    procedure SetFonts(AValue: TdxRibbonBackstageViewFonts);
    procedure SetRibbon(AValue: TdxCustomRibbon);
    procedure SetShowMainMenu(AValue: Boolean);
  protected
    FDesignHelper: IcxDesignHelper;
    FTabs: TdxRibbonBackstageViewTabSheets;

    function CanShowPopup(ARibbon: TdxCustomRibbon): Boolean; virtual;
    procedure ColorSchemeChangeHandler(Sender: TObject; const AEventArgs);
    function CreateApplicationMenuHelper: TdxRibbonBackstageViewApplicationMenuHelper; virtual;
    function CreateNonClientController: TdxRibbonBackstageViewNonClientController; virtual;
    function CreateNonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo; virtual;
    function CreatePainter: TdxRibbonBackstageViewPainter; virtual;
    function CreateViewInfo: TdxRibbonBackstageViewViewInfo; virtual;

    function AllowTouchScrollUIMode: Boolean; override;
    procedure BoundsChanged; override;
    procedure Calculate; virtual;
    procedure CalculatePlace; virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure Changed(const AChanges: TdxRibbonBackstageViewChanges = []); virtual;
    procedure CreateWnd; override;
    procedure FocusChanged; override;
    procedure FullInvalidate;
    function Popup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean; virtual;
    procedure PrepareForPopup(AForm: TCustomForm); virtual;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateWindowRegion; virtual;
    //
    function ApplicationWndProcHook(var Message: TMessage): Boolean; virtual;
    procedure HookApplicationWndProc;
    procedure UnhookApplicationWndProc;

    // Design Selector
    function CreateDesignSelector: TdxRibbonBackstageViewDesignSelectorHelper;
    procedure InvalidateDesignSelectorArea;
    procedure HideDesignSelector;
    procedure ShowDesignSelector;

    procedure DoAddTab(ATab: TdxRibbonBackstageViewTabSheet);
    procedure DoAfterBarManagerChange(Sender: TObject);
    procedure DoBeforeBarManagerChange(Sender: TObject);
    procedure DoRemoveTab(ATab: TdxRibbonBackstageViewTabSheet);
    procedure DoTabVisibleChanged(ATab: TdxRibbonBackstageViewTabSheet);

    function GetNextTab(AIndex: Integer): TdxRibbonBackstageViewTabSheet; overload;
    function GetNextTab(ATab: TdxRibbonBackstageViewTabSheet): TdxRibbonBackstageViewTabSheet; overload;
    procedure ValidateActiveTab;
    //
    function DoCanClose: Boolean;
    function DoClosePopup: Boolean; virtual;
    procedure DoCloseUp;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoPopup;
    procedure DoTabChanged;
    function DoTabChanging(ANewTab: TdxRibbonBackstageViewTabSheet): Boolean;
    procedure DoTabClick(ATab: TdxRibbonBackstageViewTabSheet);
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure InitScrollBarsParameters; override;
    function GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass; override;
    function GetSizeGripClass: TcxSizeGripClass; override;
    function NeedsToBringInternalControlsToFront: Boolean; override;
    procedure NCPaint(DC: HDC); virtual;
    procedure MakeFullyVisible(R: TRect);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure PopupMessageLoop(AParentForm: TCustomForm);
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SystemFontChanged(Sender: TObject; const AEventArgs);
    // Design
    function IsPersistentSelected(AObject: TPersistent): Boolean;
    procedure SelectPersistent(AObject: TPersistent);
    // IdxRibbonListener
    procedure AfterBarManagerChange;
    procedure BeforeBarManagerChange;
    // IdxRibbonMouseWheelReceiver
    function CanProcessMouseWheel: Boolean;
    // IdxRibbonBackstageViewSelectableItem
    procedure SelectionChanged;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;
    //
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WndProc(var Message: TMessage); override;
    //
    property BarManager: TdxBarManager read GetBarManager;
    property BarManagerHolder: TcxComponentHolder read FBarManagerHolder;
    property DesignSelector: TdxRibbonBackstageViewDesignSelectorHelper read FDesignSelector;
    property IsBarManagerValid: Boolean read GetIsBarManagerValid;
    property IsInPopupLoop: Boolean read FApplicationWndProcHooked;
    property IsRibbonValid: Boolean read GetIsRibbonValid;
    property MenuHelper: TdxRibbonBackstageViewMenuHelper read FMenuHelper;
    property MenuViewInfo: TdxRibbonBackstageViewMenuViewInfo read GetMenuViewInfo;
    property NonClientController: TdxRibbonBackstageViewNonClientController read FNonClientController;
    property NonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo read FNonClientViewInfo;
    property Painter: TdxRibbonBackstageViewPainter read FPainter;
    property RibbonStyle: TdxRibbonStyle read GetRibbonStyle;
    property ViewInfo: TdxRibbonBackstageViewViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTab: TdxRibbonBackstageViewTabSheet;
    procedure ClosePopup; virtual;
    procedure DeleteAllTabs;
    procedure DeleteTab(AIndex: Integer);
    procedure FullRefresh;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RightToLeftConversion(const ABounds: TRect);
    //
    property ActiveTab: TdxRibbonBackstageViewTabSheet read FActiveTab write SetActiveTab;
    property Buttons: TdxRibbonBackstageViewMenuButtons read FButtons write SetButtons;
    property Fonts: TdxRibbonBackstageViewFonts read FFonts write SetFonts;
    property Ribbon: TdxCustomRibbon read FRibbon write SetRibbon;
    property ShowMainMenu: Boolean read FShowMainMenu write SetShowMainMenu default True;
    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TdxRibbonBackstageViewTabSheet read GetTabs;
    //
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;
    property OnTabChanging: TdxRibbonBackstageViewTabChanging read FOnTabChanging write FOnTabChanging;
    property OnTabClick: TdxRibbonBackstageViewTabClickEvent read FOnTabClick write FOnTabClick;
  end;

  { TdxRibbonBackstageView }

  TdxRibbonBackstageView = class(TdxRibbonCustomBackstageView)
  published
    property Buttons;
    property Fonts;
    property Ribbon;
    property ShowMainMenu;

    property OnCanClose;
    property OnCloseUp;
    property OnPopup;
    property OnTabChanged;
    property OnTabChanging;
    property OnTabClick;
  end;

var
  FOnRegisterBackstageView: TcxNotifyProcedure;
  FOnUnregisterBackstageView: TcxNotifyProcedure;

implementation

uses
  cxDrawTextUtils, dxBarSkinConsts, dxFading, dxOffice11, dxRibbonForm;

const
  dxBackstageViewScrollLineSize = 17;

type
  TdxBarButtonAccess = class(TdxBarCustomButton);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarItemLinksAccess = class(TdxBarItemLinks);
  TdxCustomRibbonAccess = class(TdxCustomRibbon);
  TWinControlAccess = class(TWinControl);

procedure RegisterBackstageView(ABackstageView: TdxRibbonCustomBackstageView);
begin
  if Assigned(FOnRegisterBackstageView) then
    FOnRegisterBackstageView(ABackstageView);
end;

procedure UnregisterBackstageView(ABackstageView: TdxRibbonCustomBackstageView);
begin
  if Assigned(FOnUnregisterBackstageView) then
    FOnUnregisterBackstageView(ABackstageView);
end;

{ TdxRibbonBackstageViewCustomObject }

constructor TdxRibbonBackstageViewCustomObject.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create;
  FBackstageView := ABackstageView;
end;

function TdxRibbonBackstageViewCustomObject.GetBarManager: TdxBarManager;
begin
  Result := BackstageView.BarManager;
end;

function TdxRibbonBackstageViewCustomObject.GetIsBarManagerValid: Boolean;
begin
  Result := BackstageView.IsBarManagerValid;
end;

function TdxRibbonBackstageViewCustomObject.GetPainter: TdxRibbonBackstageViewPainter;
begin
  Result := BackstageView.Painter;
end;

function TdxRibbonBackstageViewCustomObject.GetRibbon: TdxCustomRibbon;
begin
  Result := BackstageView.Ribbon;
end;

{ TdxRibbonBackstageViewCustomViewInfo }

procedure TdxRibbonBackstageViewCustomViewInfo.Calculate(const ABounds: TRect);
begin
  FIsRightToLeftConverted := False;
  FBounds := ABounds;
end;

procedure TdxRibbonBackstageViewCustomViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
end;

procedure TdxRibbonBackstageViewCustomViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TdxRibbonBackstageViewMenuBarAccessibilityHelper }

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.AreKeyTipsSupported(
  out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean;
begin
  Result := True;
  if FKeyTipWindowsManager = nil then
    FKeyTipWindowsManager := TdxRibbonBackstageViewKeyTipWindows.Create(BackstageView.Ribbon);
  AKeyTipWindowsManager := FKeyTipWindowsManager;
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.CanUnselectOnKeyDown(
  AKey: Word; AKeyTipsWereHidden: Boolean): Boolean;
begin
  Result := not ((AKey = VK_MENU) and AKeyTipsWereHidden);
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.HandleNavigationKey(var AKey: Word): Boolean;
begin
  Result := (AKey = VK_TAB) and (ActiveTab <> nil);
  if Result then
  begin
    UnselectSelectedItemControl;
    ActiveTab.SelectNext(nil, True, True);
  end
  else
    Result := inherited HandleNavigationKey(AKey);
end;

procedure TdxRibbonBackstageViewMenuBarAccessibilityHelper.KeyTipsEscapeHandler;
begin
  BackstageView.ClosePopup;
  BackstageView.MenuHelper.PostShowRibbonKeyTips;
end;

procedure TdxRibbonBackstageViewMenuBarAccessibilityHelper.Select(ASetFocus: Boolean);
var
  AItemControl: TdxBarItemControl;
begin
  AItemControl := MenuBarControl.BackstageView.ViewInfo.ActiveTabItemControl;
  if AItemControl = nil then
    AItemControl := MenuBarControl.ItemLinks.First.Control;
  if AItemControl <> nil then
  begin
    BarNavigationController.SelectedObject := AItemControl.IAccessibilityHelper;
    BarNavigationController.SelectedObjectParent := Self;
  end;
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.GetDefaultSelectableObject(
  ADirection: TcxAccessibilityNavigationDirection): IdxBarAccessibilityHelper;
var
  AItemControl: TdxBarItemControl;
begin
  AItemControl := MenuBarControl.BackstageView.ViewInfo.ActiveTabItemControl;
  if AItemControl <> nil then
    Result := AItemControl.IAccessibilityHelper.GetNextAccessibleObject(ADirection)
  else
    Result := inherited GetDefaultSelectableObject(ADirection);
end;

procedure TdxRibbonBackstageViewMenuBarAccessibilityHelper.InitializeItemKeyTipPosition(
  AItemLinkHelper: TdxBarItemLinkAccessibilityHelper; var AKeyTipInfo: TdxBarKeyTipInfo);
var
  APositionInfo: IdxRibbonBackstageViewKeyTipPositionInfo;
  R: TRect;
begin
  if Supports(AItemLinkHelper.ItemControl, IdxRibbonBackstageViewKeyTipPositionInfo, APositionInfo) then
  begin
    R := GetItemScreenBounds(AItemLinkHelper);
    if BackstageView.UseRightToLeftAlignment then
      AKeyTipInfo.BasePoint := Point(R.Right, R.Top)
    else
      AKeyTipInfo.BasePoint := R.TopLeft;
    AKeyTipInfo.BasePoint := cxPointOffset(AKeyTipInfo.BasePoint, APositionInfo.GetKeyTipBasePoint,
      not BackstageView.UseRightToLeftAlignment);
    AKeyTipInfo.HorzAlign := taCenter;
    AKeyTipInfo.VertAlign := vaCenter;
  end
  else
    inherited InitializeItemKeyTipPosition(AItemLinkHelper, AKeyTipInfo);
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.GetActiveTab: TdxRibbonBackstageViewTabSheet;
begin
  Result := MenuBarControl.BackstageView.ActiveTab;
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.GetMenuBarControl: TdxRibbonBackstageViewMenuBarControl;
begin
  Result := TdxRibbonBackstageViewMenuBarControl(FOwnerObject);
end;

function TdxRibbonBackstageViewMenuBarAccessibilityHelper.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := MenuBarControl.BackstageView;
end;

{ TdxRibbonBackstageViewMenuBarControl }

function TdxRibbonBackstageViewMenuBarControl.CalcColumnItemRect(
  AItemLink: TdxBarItemLink; const AItemsRect: TRect): TRect;
begin
  Result := inherited CalcColumnItemRect(AItemLink, AItemsRect);

  if AItemLink.Control is TdxRibbonBackstageViewTabSheetButtonControl then
  begin
    with GetItemsRectOffset do
    begin
      Inc(Result.Right, Right);
      Dec(Result.Left, Left);
    end;
  end;
end;

procedure TdxRibbonBackstageViewMenuBarControl.CalcColumnItemRects(ATopIndex: Integer; out ALastItemBottom: Integer);
var
  AItemLink: TdxBarItemLink;
  AItemRect: TRect;
  AItemsRect: TRect;
  I: Integer;
begin
  AItemsRect := ItemsRect;

  // Place top-aligned items
  for I := ATopIndex to ItemLinks.VisibleItemCount - 1 do
  begin
    AItemLink := ItemLinks.VisibleItems[I];
    if AItemLink.Data = Ord(mbpAfterTabs) then
      Break;
    if AItemLink.Control = nil then
      Break;
    AItemRect := CalcColumnItemRect(AItemLink, AItemsRect);
    AItemLink.ItemRect := AItemRect;
    AItemsRect.Top := AItemRect.Bottom;
  end;

  // Place bottom-aligned items
  for I := ItemLinks.VisibleItemCount - 1 downto ATopIndex do
  begin
    AItemLink := ItemLinks.VisibleItems[I];
    if AItemLink.Data <> Ord(mbpAfterTabs) then
      Break;
    if AItemLink.Control = nil then
      Break;

    AItemRect := CalcColumnItemRect(AItemLink, AItemsRect);
    AItemRect := cxRectOffset(AItemRect, 0, AItemsRect.Bottom - AItemRect.Bottom);
    AItemLink.ItemRect := AItemRect;
    AItemsRect.Bottom := AItemRect.Top;
  end;

  ALastItemBottom := ItemsRect.Bottom;
end;

procedure TdxRibbonBackstageViewMenuBarControl.CalcControlsPositions;
begin
  inherited CalcControlsPositions;
  CalcItemRects(ptMenu);
end;

function TdxRibbonBackstageViewMenuBarControl.CanAlignControl(AControl: TdxBarItemControl): Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuBarControl.CanCustomizing: Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuBarControl.CanDrawClippedItem(AItemRect: TRect): Boolean;
begin
  Result := True;
end;

procedure TdxRibbonBackstageViewMenuBarControl.DoBarGetFocus(ASelectedItem: TdxBarItemControl);
begin
  if ASelectedItem = nil then
    ASelectedItem := BackstageView.ViewInfo.ActiveTabItemControl;
  inherited DoBarGetFocus(ASelectedItem);
end;

procedure TdxRibbonBackstageViewMenuBarControl.DoBarMouseDown(Button: TMouseButton;
  Shift: TShiftState; const APoint: TPoint; AItemControl: TdxBarItemControl; APointInClientRect: Boolean);

  function GetItemControlAtPos(const APos: TPoint): TdxBarItemControl;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ViewInfo.ItemControlCount - 1 do
      if PtInRect(ViewInfo.ItemControlViewInfos[I].Bounds, APos) then
      begin
        Result := ViewInfo.ItemControlViewInfos[I].Control;
        Break;
      end;
  end;

  procedure SelectItemControlOwner(AItemControl: TdxBarItemControl);
  begin
    if AItemControl is TdxRibbonBackstageViewMenuBarButtonControl then
      BackstageView.SelectPersistent(TdxRibbonBackstageViewMenuBarButtonControl(AItemControl).Item.MenuButton)
    else
      if AItemControl is TdxRibbonBackstageViewTabSheetButtonControl then
        BackstageView.SelectPersistent(TdxRibbonBackstageViewTabSheetButtonControl(AItemControl).Item.Tab);
  end;

begin
  if BackstageView.IsDesigning then
  begin
    if AItemControl <> nil then
      SelectItemControlOwner(AItemControl)
    else
      SelectItemControlOwner(GetItemControlAtPos(APoint));
  end;
  inherited DoBarMouseDown(Button, Shift, APoint, AItemControl, APointInClientRect);
end;

function TdxRibbonBackstageViewMenuBarControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonBackstageViewMenuBarAccessibilityHelper;
end;

function TdxRibbonBackstageViewMenuBarControl.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := MenuViewInfo.BackstageView;
end;

function TdxRibbonBackstageViewMenuBarControl.GetCaption: TCaption;
begin
  Result := '';
end;

function TdxRibbonBackstageViewMenuBarControl.GetFont: TFont;
begin
  Result := BackstageView.Fonts.MainMenuButton;
end;

function TdxRibbonBackstageViewMenuBarControl.GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := TdxRibbonBackstageViewMenuDockControl(DockControl).MenuViewInfo;
end;

function TdxRibbonBackstageViewMenuBarControl.GetItemControlOffset(AItemLink: TdxBarItemLink): Integer;
begin
  Result := inherited GetItemControlOffset(AItemLink) + MenuViewInfo.Painter.MenuBarIndentBetweenItems;
end;

function TdxRibbonBackstageViewMenuBarControl.GetItemsRectOffset: TRect;
begin
  Result := MenuViewInfo.Painter.MenuBarItemsRectOffset;
end;

function TdxRibbonBackstageViewMenuBarControl.GetMaxWidth(AStyle: TdxBarDockingStyle): Integer;
begin
  Result := GetMinWidth(AStyle);
end;

function TdxRibbonBackstageViewMenuBarControl.GetMinWidth(AStyle: TdxBarDockingStyle): Integer;
var
  AControl: TdxBarItemControl;
  I: Integer;
begin
  Result := 0;
  for I := 0 to ItemLinks.CanVisibleItemCount - 1 do
  begin
    AControl := ItemLinks.CanVisibleItems[I].Control;
    if AControl <> nil then
    begin
      if not (AControl is TdxRibbonBackstageViewTabSheetButtonControl) then
        Result := Max(Result, TdxBarItemControlAccess(AControl).Width);
    end;
  end;
  Result := Max(Result + cxMarginsWidth(GetItemsRectOffset), MenuViewInfo.Painter.MenuBarGetMinWidth);
  Result := Max(Result, ScaleFactor.Apply(dxRibbonBackstageViewMinMenuWidth));
end;

function TdxRibbonBackstageViewMenuBarControl.GetNextBarControl(AForward: Boolean): TdxBarControl;
begin
  Result := nil;
end;

function TdxRibbonBackstageViewMenuBarControl.GetRibbon: TdxCustomRibbon;
begin
  Result := MenuViewInfo.Ribbon;
end;

function TdxRibbonBackstageViewMenuBarControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonBackstageViewMenuBarControlViewInfo;
end;

function TdxRibbonBackstageViewMenuBarControl.HasCaptionButtons: Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuBarControl.IsInternal: Boolean;
begin
  Result := True;
end;

function TdxRibbonBackstageViewMenuBarControl.IsVertical: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonBackstageViewMenuBarControl.MakeItemControlFullyVisible(AItemControl: TdxBarItemControl);
begin
  if DockControl <> nil then
    BackstageView.MakeFullyVisible(dxMapWindowRect(Handle, BackstageView.Handle, AItemControl.ViewInfo.Bounds));
end;

procedure TdxRibbonBackstageViewMenuBarControl.NavigationHandler(var ACharCode: Word; AShiftState: TShiftState);
begin
  case ACharCode of
    VK_ESCAPE:
      begin
        BackstageView.ClosePopup;
        ACharCode := 0;
      end;

  else
    inherited NavigationHandler(ACharCode, AShiftState);
  end;
end;

function TdxRibbonBackstageViewMenuBarControl.NeedsMouseWheel: Boolean;
begin
  Result := BackstageView.CanProcessMouseWheel;
end;

function TdxRibbonBackstageViewMenuBarControl.PreProcessKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := AKey = VK_MENU;
  if Result then
    HideAll;
end;

procedure TdxRibbonBackstageViewMenuBarControl.SetLayeredAttributes;
begin
  //nothing
end;

procedure TdxRibbonBackstageViewMenuBarControl.ShowPopup(AItem: TdxBarItemControl);
begin
  //nothing
end;

procedure TdxRibbonBackstageViewMenuBarControl.WndProc(var Message: TMessage);
begin
  if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and BackstageView.IsDesigning then
  begin
    if Perform(CM_DESIGNHITTEST, Message.WParam, Message.LParam) = 0 then
      inherited WndProc(Message)
    else
      if not IsControlMouseMsg(TWMMouse(Message)) then
      begin
        ControlState := ControlState + [csLButtonDown];
        Dispatch(Message);
        ControlState := ControlState - [csLButtonDown];
      end;
  end
  else
    inherited WndProc(Message);
end;

procedure TdxRibbonBackstageViewMenuBarControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  I: Integer;
begin
  Message.Result := 0;
  for I := 0 to ViewInfo.ItemControlCount - 1 do
    if PtInRect(ViewInfo.ItemControlViewInfos[I].Bounds, SmallPointToPoint(Message.Pos)) then
    begin
      Message.Result := 1;
      Break;
    end;
end;

procedure TdxRibbonBackstageViewMenuBarControl.WMMouseWheel(var Message: TWMMouseWheel);
begin
  BackstageView.DoMouseWheel(KeysToShiftState(Message.Keys), Message.WheelDelta, SmallPointToPoint(Message.Pos));
  Message.Result := 1;
end;

{ TdxRibbonBackstageViewMenuBarControlViewInfo }

procedure TdxRibbonBackstageViewMenuBarControlViewInfo.DoCalcSeparatorInfo(
  AItemLink: TdxBarItemLink; const AItemRect: TRect);
begin
  AddSeparatorInfo(cxRectSetBottom(AItemRect, AItemRect.Top,
    TdxRibbonBackstageViewMenuBarControl(BarControl).BeginGroupSize),
    skHorizontal, AItemLink.Control);
end;

{ TdxRibbonBackstageViewPainter }

constructor TdxRibbonBackstageViewPainter.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create;
  FBackstageView := ABackstageView;
end;

function TdxRibbonBackstageViewPainter.CanShowTabAreaToolbar: Boolean;
begin
  Result := BackstageView.IsRibbonValid and BackstageView.Ribbon.ColorScheme.CanShowTabAreaToolbarInBackstageView;
end;

procedure TdxRibbonBackstageViewPainter.DrawBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
const
  StateMap: array[TcxButtonState] of Integer = (
    DXBAR_ACTIVE, DXBAR_NORMAL, DXBAR_HOT, DXBAR_PRESSED, DXBAR_DISABLED
  );
begin
  if Skin <> nil then
    Skin.DrawBackground(ACanvas.Handle, R, DXBAR_BACKSTAGEVIEW_BACKBUTTON, StateMap[AState])
  else
    BackstageView.LookAndFeelPainter.DrawScaledBackButton(ACanvas, R, AState, BackstageView.ScaleFactor)
end;

procedure TdxRibbonBackstageViewPainter.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if Skin <> nil then
    Skin.DrawBackground(ACanvas.Handle, R, DXBAR_BACKSTAGEVIEW)
  else
    ACanvas.FillRect(R, clCream);
end;

procedure TdxRibbonBackstageViewPainter.DrawMenuBarHeader(ACanvas: TcxCanvas; const R: TRect);
begin
  if Skin <> nil then
    Skin.DrawBackground(ACanvas.Handle, R, DXBAR_BACKSTAGEVIEW_MENUBAR_HEADER)
  else
    ACanvas.FillRect(R, clCream)
end;

function TdxRibbonBackstageViewPainter.GetBackButtonOffset: Integer;
begin
  if Skin <> nil then
    Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_BACKBUTTON_OFFSET)
  else
    Result := 0;
end;

function TdxRibbonBackstageViewPainter.GetBackButtonSize: TSize;
begin
  if Skin <> nil then
    Result := cxSize(Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_BACKBUTTON))
  else
    Result := BackstageView.LookAndFeelPainter.GetScaledBackButtonSize(BackstageView.ScaleFactor)
end;

function TdxRibbonBackstageViewPainter.GetContentOffsets: TRect;
begin
  if Skin <> nil then
    Result := Skin.GetContentOffsets(DXBAR_BACKSTAGEVIEW)
  else
    Result := cxNullRect
end;

function TdxRibbonBackstageViewPainter.GetSkin: IdxSkin;
begin
  if not (BackstageView.IsRibbonValid and Supports(BackstageView.Ribbon, IdxSkin, Result)) then
    Result := nil;
end;

function TdxRibbonBackstageViewPainter.GetDefaultTextColor(AEnabled: Boolean): TColor;
begin
  if Skin <> nil then
    Result := Skin.GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR, IfThen(AEnabled, DXBAR_NORMAL, DXBAR_DISABLED))
  else
    Result := clDefault;
end;

{ TdxRibbonBackstageViewPainter }

function TdxRibbonBackstageViewMenuPainter.BarBeginGroupSize: Integer;
begin
  Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR);
end;

procedure TdxRibbonBackstageViewMenuPainter.BarDrawBeginGroup(
  ABarControl: TCustomdxBarControl; DC: HDC; ABeginGroupRect: TRect;
  AToolbarBrush: HBRUSH; AHorz: Boolean);
begin
  Skin.DrawBackground(DC, ABeginGroupRect, DXBAR_BACKSTAGEVIEW_MENUBAR_SEPARATOR);
end;

procedure TdxRibbonBackstageViewMenuPainter.DrawButtonBackground(
  const ADrawParams: TdxBarButtonLikeControlDrawParams);
begin
  Skin.DrawBackground(ADrawParams.Canvas.Handle,
    ADrawParams.BarItemControl.ItemBounds, DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM,
    GetButtonPartState(ADrawParams, icpControl));
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarButtonContentOffset: TRect;
begin
  if Skin <> nil then
    Result := Skin.GetContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM)
  else
    Result := cxNullRect;
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarButtonTextColor(const ADrawParams: TdxBarButtonLikeControlDrawParams): TColor;
var
  AColor: TColor;
begin
  GetDefaultTextColors(ADrawParams.BarItemControl, ADrawParams.Enabled, ADrawParams.DrawSelected, True, Result, AColor);
end;

procedure TdxRibbonBackstageViewMenuPainter.DockControlFillBackground(
  ADockControl: TdxDockControl; DC: HDC; ADestR: TRect; ASourceR, AWholeR: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  Skin.DrawBackground(DC, AWholeR, DXBAR_BACKSTAGEVIEW_MENUBAR);
end;

procedure TdxRibbonBackstageViewMenuPainter.DrawTabButtonBackground(DC: HDC; R: TRect; AState: Integer);
begin
  Skin.DrawBackground(DC, R, DXBAR_BACKSTAGEVIEW_MENUBAR_TAB, AState);
end;

function TdxRibbonBackstageViewMenuPainter.GetTabButtonState(const ADrawParams: TdxBarButtonLikeControlDrawParams): Integer;
begin
  if ADrawParams.IsPressed then
    Result := DXBAR_PRESSED
  else
    if not ADrawParams.Downed then
      Result := GetPartState(ADrawParams, icpControl)
    else
      if ADrawParams.HotPartIndex = icpControl then
        Result := DXBAR_HOTCHECK
      else
        Result := DXBAR_CHECKED;
end;

function TdxRibbonBackstageViewMenuPainter.TabButtonContentOffset: TRect;
begin
  if Skin <> nil then
    Result := Skin.GetContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB)
  else
    Result := cxNullRect;
end;

function TdxRibbonBackstageViewMenuPainter.TabButtonDefaultHeight: Integer;
begin
  if Skin <> nil then
    Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB)
  else
    Result := 0;
end;

function TdxRibbonBackstageViewMenuPainter.TabButtonTextColor(AState: Integer): TColor;
begin
  if Skin <> nil then
    Result := Skin.GetPartColor(DXBAR_BACKSTAGEVIEW_MENUBAR_TAB_TEXTCOLOR, AState)
  else
    Result := clWindowText;
end;

function TdxRibbonBackstageViewMenuPainter.TabButtonTextColor(const ADrawParams: TdxBarButtonLikeControlDrawParams): TColor;

  function GetTextColorState(AButtonState: Integer): Integer;
  begin
    Result := AButtonState;
    if ADrawParams.IsCustomizing then
    begin
      if not ADrawParams.Enabled then
        Result := DXBAR_DISABLED;
    end;
  end;

begin
  Result := TabButtonTextColor(GetTextColorState(GetTabButtonState(ADrawParams)));
end;

procedure TdxRibbonBackstageViewMenuPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
var
  R: TRect;
begin
  ACanvas.SaveClipRegion;
  try
    R := dxMapWindowRect(ABarControl.DockControl.Handle, ABarControl.Handle, ABarControl.DockControl.ClientRect);
    ACanvas.IntersectClipRect(ABarControl.ClientRect);
    Skin.DrawBackground(ACanvas.Handle, R, DXBAR_BACKSTAGEVIEW_MENUBAR);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxRibbonBackstageViewMenuPainter.DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC);
begin
end;

procedure TdxRibbonBackstageViewMenuPainter.GetDisabledTextColors(
  ABarItemControl: TdxBarItemControl; ASelected: Boolean; AFlat: Boolean;
  var AColor1, AColor2: TColor);
const
  StateMap: array[Boolean] of Integer = (DXBAR_DISABLED, DXBAR_ACTIVEDISABLED);
begin
  AColor1 := Skin.GetPartColor(DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR, StateMap[ASelected]);
  AColor2 := AColor1;
end;

function TdxRibbonBackstageViewMenuPainter.GetEnabledTextColor(
  ABarItemControl: TdxBarItemControl; ASelected: Boolean; AFlat: Boolean): TColor;

  function GetPartState: Integer;
  begin
    if not ABarItemControl.Enabled then
      Result := DXBAR_DISABLED
    else
      if TdxBarItemControlAccess(ABarItemControl).Pressed then
        Result := DXBAR_PRESSED
      else
        if ABarItemControl.IsSelected then
          Result := DXBAR_HOT
        else
          Result := DXBAR_NORMAL;
  end;

begin
  if Skin <> nil then
    Result := Skin.GetPartColor(DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM_TEXTCOLOR, GetPartState)
  else
    Result := inherited GetEnabledTextColor(ABarItemControl, ASelected, AFlat);
end;

function TdxRibbonBackstageViewMenuPainter.GetGlyphColorPalette(
  ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette;
begin
  if Skin <> nil then
    Result := Skin.GetPartColorPalette(DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM, AState)
  else
    Result := nil;
end;

function TdxRibbonBackstageViewMenuPainter.GetToolbarContentOffsets(
  ABar: TdxBar; ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect;
begin
  Result := cxNullRect;
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarDefaultItemHeight: Integer;
begin
  if Skin <> nil then
    Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_ITEM)
  else
    Result := 0;
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarGetMinWidth: Integer;
begin
  if Skin <> nil then
    Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR)
  else
    Result := 0;
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarIndentBetweenItems: Integer;
begin
  if Skin <> nil then
    Result := Skin.GetPartSize(DXBAR_BACKSTAGEVIEW_MENUBAR_INDENTBETWEENITEMS)
  else
    Result := 0;
end;

function TdxRibbonBackstageViewMenuPainter.MenuBarItemsRectOffset: TRect;
begin
  if Skin <> nil then
    Result := Skin.GetContentOffsets(DXBAR_BACKSTAGEVIEW_MENUBAR)
  else
    Result := cxNullRect;
end;

{ TdxRibbonBackstageViewMenuDockControl }

constructor TdxRibbonBackstageViewMenuDockControl.Create(AMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo);
begin
  inherited Create(AMenuViewInfo.BackstageView);
  FMenuViewInfo := AMenuViewInfo;
end;

procedure TdxRibbonBackstageViewMenuDockControl.CalcLayout;
var
  ABounds: TRect;
  AControl: TdxRibbonBackstageViewMenuBarControl;
  AControlHeight: Integer;
begin
  AControl := MenuViewInfo.BarControl;
  if AControl <> nil then
  begin
    AControlHeight := MenuViewInfo.MinHeight;
    if BackstageView.RibbonStyle = rs2019 then
      AControlHeight := Max(AControlHeight, ClientHeight);
    ABounds := cxRectSetHeight(ClientRect, AControlHeight);
    if cxRectIsEqual(ABounds, AControl.BoundsRect) then
      AControl.CalcLayout
    else
      AControl.BoundsRect := ABounds;
  end;
end;

function TdxRibbonBackstageViewMenuDockControl.CanCustomize: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonBackstageViewMenuDockControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 0;
end;

function TdxRibbonBackstageViewMenuDockControl.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := MenuViewInfo.BackstageView;
end;

function TdxRibbonBackstageViewMenuDockControl.GetContainer: TObject;
begin
  Result := BackstageView;
end;

function TdxRibbonBackstageViewMenuDockControl.GetClientSize: Integer;
begin
  Result := MenuViewInfo.MinWidth;
end;

function TdxRibbonBackstageViewMenuDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxRibbonBackstageViewMenuBarControl;
end;

function TdxRibbonBackstageViewMenuDockControl.GetDockingStyle: TdxBarDockingStyle;
begin
  Result := dsLeft;
end;

function TdxRibbonBackstageViewMenuDockControl.GetMinSize: Integer;
begin
  if MenuViewInfo <> nil then
    Result := MenuViewInfo.MinWidth
  else
    Result := 0;
end;

function TdxRibbonBackstageViewMenuDockControl.GetPainter: TdxBarPainter;
begin
  Result := MenuViewInfo.Painter;
end;

function TdxRibbonBackstageViewMenuDockControl.GetRibbon: TdxCustomRibbon;
begin
  Result := MenuViewInfo.Ribbon;
end;

function TdxRibbonBackstageViewMenuDockControl.GetSunkenBorder: Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuDockControl.IsDrawDesignBorder: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonBackstageViewMenuDockControl.ShowCustomizePopup;
begin
  // do nothing
end;

{ TdxRibbonBackstageViewMenuBarCustomButtonControl }

procedure TdxRibbonBackstageViewMenuBarCustomButtonControl.CalcDrawParams(AFull: Boolean);
begin
  inherited;
  DrawParams.ContentOffset := CalculateContentOffsets;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.CalculateContentOffsets: TRect;
begin
  if IsTabButtonStyle then
    Result := MenuPainter.TabButtonContentOffset
  else
    Result := MenuPainter.MenuBarButtonContentOffset;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.CanCustomize: Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.CanDestroyOnClick: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonBackstageViewMenuBarCustomButtonControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  Draw(DrawParams, ARect);
end;

procedure TdxRibbonBackstageViewMenuBarCustomButtonControl.Draw(
  const ADrawParams: TdxBarButtonLikeControlDrawParams; const R: TRect);
begin
  if not dxFader.DrawFadeImage(ADrawParams.BarItemControl, ADrawParams.Canvas.Handle, R) then
    DrawBackground(ADrawParams, R);
  DrawContent(ADrawParams, R);
end;

procedure TdxRibbonBackstageViewMenuBarCustomButtonControl.DrawBackground(
  const ADrawParams: TdxBarButtonLikeControlDrawParams; const R: TRect);
begin
  if IsTabButtonStyle then
    MenuPainter.DrawTabButtonBackground(ADrawParams.Canvas.Handle, R, MenuPainter.GetTabButtonState(ADrawParams))
  else
    MenuPainter.DrawButtonBackground(ADrawParams);
end;

procedure TdxRibbonBackstageViewMenuBarCustomButtonControl.DrawContent(
  const ADrawParams: TdxBarButtonLikeControlDrawParams; R: TRect);
var
  AGlyphAreaWidth: Integer;
  AImageRect: TRect;
  AImageSize: TSize;
  AContentOffset: TRect;
  AIsRightToLeft: Boolean;
  ATextFlags: Cardinal;
begin
  AIsRightToLeft := ADrawParams.UseRightToLeftAlignment;
  if AIsRightToLeft then
    AContentOffset := TdxRightToLeftLayoutConverter.ConvertOffsets(ADrawParams.ContentOffset)
  else
    AContentOffset := ADrawParams.ContentOffset;
  R := cxRectContent(R, AContentOffset);

  if cpIcon in ADrawParams.ViewStructure then
  begin
    AGlyphAreaWidth := GetGlyphAreaWidth;
    AImageSize := TdxBarItemControlAccess(ADrawParams.BarItemControl).GetGlyphSize(ADrawParams.ViewSize);
    if AImageSize.cx > AGlyphAreaWidth then
      AImageSize := cxSizeScale(AImageSize, AGlyphAreaWidth, AImageSize.cx);
    AImageRect := cxRectCenter(cxRectSetWidth(R, AGlyphAreaWidth), AImageSize);
    if AIsRightToLeft then
      AImageRect := TdxRightToLeftLayoutConverter.ConvertRect(AImageRect, R);

    cxDrawImage(ADrawParams.Canvas.Handle, AImageRect, AImageRect,
      Glyph, Images, ImageIndex, idmNormal, True, 0, clDefault, True,
      MenuPainter.GetGlyphColorPalette(Self,
        ADrawParams.PaintType, ADrawParams.DrawSelected, ADrawParams.Downed,
        ADrawParams.DrawDowned, ADrawParams.DroppedDown, False));

    if AIsRightToLeft then
      R.Right := AImageRect.Left - 2 * ScaleFactor.Apply(cxTextOffset)
    else
      R.Left := AImageRect.Right + 2 * ScaleFactor.Apply(cxTextOffset);
  end;

  if cpText in ADrawParams.ViewStructure then
  begin
    if IsTabButtonStyle then
      ADrawParams.Canvas.Font.Color := MenuPainter.TabButtonTextColor(ADrawParams)
    else
      ADrawParams.Canvas.Font.Color := MenuPainter.MenuBarButtonTextColor(ADrawParams);

    ATextFlags := CXTO_EDITCONTROL or CXTO_CENTER_VERTICALLY or CXTO_HIDEPREFIX or IfThen(IsWordWrapSupported, CXTO_WORDBREAK);
    if AIsRightToLeft then
      ATextFlags := ATextFlags or CXTO_RIGHT;
    if ADrawParams.Canvas.TextFlags and ETO_RTLREADING = ETO_RTLREADING then
      ATextFlags := ATextFlags or CXTO_RTLREADING;

    cxTextOut(ADrawParams.Canvas.Handle, ADrawParams.Caption, R, ATextFlags);
  end;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetDefaultHeight: Integer;
var
  ARect: TRect;
  ASavedFont: TdxBarSavedFont;
begin
  if IsTabButtonStyle then
    Result := MenuPainter.TabButtonDefaultHeight
  else
    Result := MenuPainter.MenuBarDefaultItemHeight;

  if cpIcon in FDrawParams.ViewStructure then
    Result := Max(Result, GetGlyphAreaWidth + cxMarginsHeight(DrawParams.ContentOffset));

  if cpText in DrawParams.ViewStructure then
  begin
    if IsWordWrapSupported then
    begin
      PrepareCanvasFont(0, Item.Style, ASavedFont);
      try
        ARect := cxRect(DrawParams.ContentOffset.Left, 0, MenuViewInfo.MinWidth - DrawParams.ContentOffset.Right, 0);
        if cpIcon in FDrawParams.ViewStructure then
          Inc(ARect.Left, GetGlyphAreaWidth + 2 * ScaleFactor.Apply(cxTextOffset));
        cxTextOut(Canvas.Handle, Item.Caption, ARect, CXTO_EDITCONTROL or CXTO_CALCRECT or CXTO_WORDBREAK);
        Result := Max(Result, cxRectHeight(ARect) + cxMarginsHeight(DrawParams.ContentOffset));
      finally
        RestoreCanvasFont(ASavedFont);
      end;
    end
    else
      Result := Max(Result, GetTextSize + cxMarginsHeight(DrawParams.ContentOffset));
  end;

  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetDefaultWidth: Integer;
begin
  Result := GetGlyphAreaWidth + GetCaptionAreaWidth +
    cxMarginsWidth(DrawParams.ContentOffset) + 2 * ScaleFactor.Apply(cxTextOffset);
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetKeyTipBasePoint: TPoint;
begin
  Result := cxPointOffset(DrawParams.ContentOffset.TopLeft, -2 * cxTextOffset, 0);
  if cpIcon in DrawParams.ViewStructure then
    Result := cxPointOffset(Result, GetGlyphAreaWidth, 0);
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetMenuPainter: TdxRibbonBackstageViewMenuPainter;
begin
  Result := MenuViewInfo.Painter;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := BackstageView.MenuViewInfo;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.GetViewStructure: TdxBarItemControlViewStructure;
begin
  Result := [cpIcon, cpText];
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.HasImage: Boolean;
begin
  Result := IsImageAssigned(Glyph) or IsImageAssigned(Images, ImageIndex);
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.Images: TCustomImageList;
var
  AImageEnabled: TdxDefaultBoolean;
begin
  Result := GetImages(Enabled, AImageEnabled);
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.IsTabButtonStyle: Boolean;
begin
  Result := False;
end;

function TdxRibbonBackstageViewMenuBarCustomButtonControl.IsWordWrapSupported: Boolean;
begin
  Result := False;
end;

{ TdxRibbonBackstageViewTabSheetViewInfo }

constructor TdxRibbonBackstageViewTabSheetViewInfo.Create(ATab: TdxRibbonBackstageViewTabSheet);
begin
  inherited Create;
  FTab := ATab;
end;

procedure TdxRibbonBackstageViewTabSheetViewInfo.Calculate(const R: TRect);
begin
  FBounds := R;
end;

procedure TdxRibbonBackstageViewTabSheetViewInfo.ValidateWindowPos(var APos: TWindowPos);
begin
  APos.x := Bounds.Left;
  APos.y := Bounds.Top;
  APos.cx := cxRectWidth(Bounds);
  APos.cy := cxRectHeight(Bounds);
end;

function TdxRibbonBackstageViewTabSheetViewInfo.GetFrameAreaVisibleBounds: TRect;
begin
  Result := Tab.BackstageView.ViewInfo.FrameAreaVisibleBounds;
end;

function TdxRibbonBackstageViewTabSheetViewInfo.GetMinHeight: Integer;
var
  ANewWidth, ANewHeight: Integer;
begin
  Result := Tab.SizeOptions.MinHeight;
  if Tab.SizeOptions.AutoSize then
  begin
    Tab.CanAutoSize(ANewWidth, ANewHeight);
    Result := Max(Result, ANewHeight);
  end;
end;

function TdxRibbonBackstageViewTabSheetViewInfo.GetMinWidth: Integer;
var
  ANewWidth, ANewHeight: Integer;
begin
  Result := Tab.SizeOptions.MinWidth;
  if Tab.SizeOptions.AutoSize then
  begin
    Tab.CanAutoSize(ANewWidth, ANewHeight);
    Result := Max(Result, ANewWidth);
  end;
end;

{ TdxRibbonBackstageViewTabSheetSizeOptions }

constructor TdxRibbonBackstageViewTabSheetSizeOptions.Create(ATab: TdxRibbonBackstageViewTabSheet);
begin
  inherited Create;
  FTab := ATab;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonBackstageViewTabSheetSizeOptions then
  begin
    BeginUpdate;
    try
      MinWidth := TdxRibbonBackstageViewTabSheetSizeOptions(Source).MinWidth;
      MinHeight := TdxRibbonBackstageViewTabSheetSizeOptions(Source).MinHeight;
      AutoSize := TdxRibbonBackstageViewTabSheetSizeOptions(Source).AutoSize;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FHasChanges then
    Changed;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.Changed;
begin
  if FUpdateCount > 0 then
    FHasChanges := True
  else
  begin
    FHasChanges := False;
    FTab.Changed;
  end;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.ChangeScale(M, D: Integer);
begin
  MinWidth := MulDiv(MinWidth, M, D);
  MinHeight := MulDiv(MinHeight, M, D);
end;

function TdxRibbonBackstageViewTabSheetSizeOptions.GetAutoSize: Boolean;
begin
  Result := FTab.AutoSize;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.SetAutoSize(AValue: Boolean);
begin
  FTab.AutoSize := AValue;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.SetMinHeight(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FMinHeight then
  begin
    FMinHeight := AValue;
    Changed;
  end;
end;

procedure TdxRibbonBackstageViewTabSheetSizeOptions.SetMinWidth(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FMinWidth then
  begin
    FMinWidth := AValue;
    Changed;
  end;
end;

{ TdxRibbonBackstageViewTabSheet }

constructor TdxRibbonBackstageViewTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FSizeOptions := TdxRibbonBackstageViewTabSheetSizeOptions.Create(Self);
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := HandlerGlyphChanged;
  FViewInfo := CreateViewInfo;
  FImageIndex := -1;
  FTabVisible := True;
  ParentShowHint := False;
  ShowHint := False;
  TabStop := False;
  Visible := False;
end;

destructor TdxRibbonBackstageViewTabSheet.Destroy;
begin
  FreeAndNil(FGlyph);
  FreeAndNil(FViewInfo);
  FreeAndNil(FSizeOptions);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewTabSheet.Activate;
begin
  Visible := True;
  BringToFront;
end;

procedure TdxRibbonBackstageViewTabSheet.AdjustSize;
begin
  inherited AdjustSize;
  Changed;
end;

procedure TdxRibbonBackstageViewTabSheet.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  RefreshNonClientArea;
end;

procedure TdxRibbonBackstageViewTabSheet.Calculate(const R: TRect);
begin
  ViewInfo.Calculate(R);
  BoundsRect := ViewInfo.Bounds;
end;

procedure TdxRibbonBackstageViewTabSheet.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  SizeOptions.BeginUpdate;
  try
    SizeOptions.ChangeScale(M, D);
    inherited;
  finally
    SizeOptions.EndUpdate;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.Changed(AChanges: TdxRibbonBackstageViewChanges);
begin
  if BackstageView <> nil then
  begin
    BarDesignController.LockDesignerModified;
    try
      BackstageView.Changed(AChanges);
    finally
      BarDesignController.UnLockDesignerModified;
    end;
  end;
end;

function TdxRibbonBackstageViewTabSheet.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  if Result then
  begin
    NewHeight := Max(NewHeight, ViewInfo.MinHeight);
    NewWidth := Max(NewWidth, ViewInfo.MinWidth);
  end;
end;

function TdxRibbonBackstageViewTabSheet.CreateViewInfo: TdxRibbonBackstageViewTabSheetViewInfo;
begin
  Result := TdxRibbonBackstageViewTabSheetViewInfo.Create(Self);
end;

procedure TdxRibbonBackstageViewTabSheet.Deactivate;
begin
  Visible := False;
end;

procedure TdxRibbonBackstageViewTabSheet.DrawBackground(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := dxMapWindowRect(BackstageView.Handle, Handle, cxGetWindowBounds(BackstageView), False);
  R := cxRectOffset(R, cxGetClientOffset(Handle), False);
  Painter.DrawBackground(ACanvas, R);
end;

procedure TdxRibbonBackstageViewTabSheet.NCPaint(DC: HDC);
var
  R: TRect;
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    R := Rect(0, 0, Width, Height);
    cxPaintCanvas.ExcludeClipRect(cxRectInflate(R, -BorderWidth, -BorderWidth));
    cxPaintCanvas.WindowOrg := cxPointInvert(cxGetClientOffset(Handle));
    DrawBackground(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.RefreshNonClientArea;
begin
  if (BorderWidth > 0) and HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TdxRibbonBackstageViewTabSheet.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Changed;
end;

procedure TdxRibbonBackstageViewTabSheet.EnabledChanged;
begin
  Changed;
end;

procedure TdxRibbonBackstageViewTabSheet.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
begin
  if BorderWidth > 0 then
  begin
    DC := GetWindowDC(Handle);
    try
      NCPaint(DC);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.WMPrint(var Message: TWMPrint);
begin
  if (PRF_NONCLIENT and Message.Flags <> 0) and (BorderWidth > 0) then
    NCPaint(Message.DC);
  inherited;
end;

function TdxRibbonBackstageViewTabSheet.GetActive: Boolean;
begin
  Result := (BackstageView <> nil) and (BackstageView.ActiveTab = Self);
end;

function TdxRibbonBackstageViewTabSheet.GetCanBeActive: Boolean;
begin
  Result := IsDesigning or TabVisible and Enabled;
end;

function TdxRibbonBackstageViewTabSheet.GetImages: TCustomImageList;
begin
  if (BackstageView <> nil) and BackstageView.IsBarManagerValid then
    Result := BackstageView.BarManager.Images
  else
    Result := nil;
end;

function TdxRibbonBackstageViewTabSheet.GetPageIndex: Integer;
begin
  if BackstageView <> nil then
    Result := BackstageView.FTabs.IndexOf(Self)
  else
    Result := -1;
end;

function TdxRibbonBackstageViewTabSheet.GetPainter: TdxRibbonBackstageViewPainter;
begin
  Result := BackstageView.Painter;
end;

procedure TdxRibbonBackstageViewTabSheet.Paint;
begin
  DrawBackground(Canvas);
end;

procedure TdxRibbonBackstageViewTabSheet.SetPageIndex(AValue: Integer);
begin
  if Assigned(BackstageView) and (AValue <> PageIndex) then
  begin
    if (AValue >= 0) and (AValue < BackstageView.TabCount) then
    begin
      BackstageView.FTabs.Move(PageIndex, AValue);
      BackstageView.Changed([rbvcStruct]);
    end;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.SetActive(AValue: Boolean);
begin
  if AValue and (BackstageView <> nil) then
    BackstageView.ActiveTab := Self;
end;

procedure TdxRibbonBackstageViewTabSheet.SetBackstageView(AValue: TdxRibbonCustomBackstageView);
begin
  if AValue <> BackstageView then
  begin
    if BackstageView <> nil then
      BackstageView.DoRemoveTab(Self);
    FBackstageView := AValue;
    inherited SetParent(BackstageView);
    if BackstageView <> nil then
      BackstageView.DoAddTab(Self);
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.SetBeginGroup(AValue: Boolean);
begin
  if FBeginGroup <> AValue then
  begin
    FBeginGroup := AValue;
    Changed([rbvcStruct]);
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Assigned(ViewInfo) then
  begin
    ATop := ViewInfo.Bounds.Top;
    ALeft := ViewInfo.Bounds.Left;
    AWidth := cxRectWidth(ViewInfo.Bounds);
    AHeight := cxRectHeight(ViewInfo.Bounds);
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxRibbonBackstageViewTabSheet.SetGlyph(AValue: TdxSmartGlyph);
begin
  FGlyph.Assign(AValue);
end;

procedure TdxRibbonBackstageViewTabSheet.SetImageIndex(AValue: TcxImageIndex);
begin
  AValue := Max(AValue, -1);
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.SetKeyTip(const AValue: string);
begin
  if AValue <> FKeyTip then
  begin
    FKeyTip := AValue;
    Changed;
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.SetParent(AParent: TWinControl);
begin
  if (AParent = nil) or (AParent is TdxRibbonCustomBackstageView) then
    BackstageView := TdxRibbonCustomBackstageView(AParent)
  else
    Abort;
end;

procedure TdxRibbonBackstageViewTabSheet.SetSizeOptions(AValue: TdxRibbonBackstageViewTabSheetSizeOptions);
begin
  FSizeOptions.Assign(AValue);
end;

procedure TdxRibbonBackstageViewTabSheet.SetTabVisible(AValue: Boolean);
begin
  if AValue <> FTabVisible then
  begin
    FTabVisible := AValue;
    if BackstageView <> nil then
      BackstageView.DoTabVisibleChanged(Self);
  end;
end;

procedure TdxRibbonBackstageViewTabSheet.HandlerGlyphChanged(Sender: TObject);
begin
  if not IsDestroying then
    Changed;
end;

procedure TdxRibbonBackstageViewTabSheet.WndProc(var Message: TMessage);
begin
  if (BackstageView <> nil) and BackstageView.IsPopupScrollBars and ScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
    BackstageView.ShowTouchScrollUI(BackstageView, True);
  inherited;
end;

{ TdxRibbonBackstageViewTabSheets }

function TdxRibbonBackstageViewTabSheets.GetItem(Index: Integer): TdxRibbonBackstageViewTabSheet;
begin
  Result := TdxRibbonBackstageViewTabSheet(inherited Items[Index]);
end;

{ TdxRibbonBackstageViewTabSheetButton }

constructor TdxRibbonBackstageViewTabSheetButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ButtonStyle := bsChecked;
end;

procedure TdxRibbonBackstageViewTabSheetButton.DirectClick;
begin
  inherited DirectClick;
  Tab.BackstageView.DoTabClick(Tab);
end;

procedure TdxRibbonBackstageViewTabSheetButton.RefreshInfo;
begin
  if Tab <> nil then
  begin
    Caption := Tab.Caption;
    Enabled := Tab.Enabled;
    KeyTip := Tab.KeyTip;
    Down := Tab.Active;
    Hint := Tab.Hint;
    ImageIndex := Tab.ImageIndex;
    Glyph := Tab.Glyph;
  end;
end;

function TdxRibbonBackstageViewTabSheetButton.GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass;
begin
  Result := TdxRibbonBackstageViewTabSheetButtonControl;
end;

procedure TdxRibbonBackstageViewTabSheetButton.SetTab(AValue: TdxRibbonBackstageViewTabSheet);
begin
  FTab := AValue;
  RefreshInfo;
end;

{ TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper }

constructor TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.Create(AOwnerObject: TObject);
begin
  inherited Create(AOwnerObject);
  FAccessibilityChildren := TList.Create;
end;

destructor TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.Destroy;
begin
  FreeAndNil(FAccessibilityChildren);
  inherited Destroy;
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetBarManager: TdxBarManager;
begin
  Result := TabSheet.BackstageView.BarManager;
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetDefaultAccessibleObject: IdxBarAccessibilityHelper;
begin
  if GetChildCount > 0 then
    Result := IdxBarAccessibilityHelper(FAccessibilityChildren[0])
  else
    Result := nil;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.AddChild(AChild: TcxAccessibilityHelper);
begin
  if GetChildIndex(AChild) = -1 then
    FAccessibilityChildren.Add(Pointer(dxBar.GetAccessibilityHelper(AChild)));
end;

procedure TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.DeleteChild(AChild: TcxAccessibilityHelper);
begin
  FAccessibilityChildren.Remove(Pointer(dxBar.GetAccessibilityHelper(AChild)));
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetChild(
  AIndex: Integer): TcxAccessibilityHelper;
begin
  Result := IdxBarAccessibilityHelper(FAccessibilityChildren[AIndex]).GetHelper;
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetChildCount: Integer;
begin
  Result := FAccessibilityChildren.Count;
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetChildIndex(
  AChild: TcxAccessibilityHelper): Integer;
begin
  Result := FAccessibilityChildren.IndexOf(Pointer(dxBar.GetAccessibilityHelper(AChild)));
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.AreKeyTipsSupported(
  out AKeyTipWindowsManager: IdxBarKeyTipWindowsManager): Boolean;
begin
  Result := True;
  if FKeyTipWindowsManager = nil then
    FKeyTipWindowsManager := TdxRibbonBackstageViewKeyTipWindows.Create(TabSheet.BackstageView.Ribbon);
  AKeyTipWindowsManager := FKeyTipWindowsManager;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.KeyTipHandler(Sender: TObject);
begin
  inherited KeyTipHandler(Sender);
  BarNavigationController.SetKeyTipsShowingState(Self, '');
end;

procedure TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.KeyTipsEscapeHandler;
begin
  inherited KeyTipsEscapeHandler;
  BarNavigationController.SetKeyTipsShowingState(Parent, '');
end;

function TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper.GetTabSheet: TdxRibbonBackstageViewTabSheet;
begin
  Result := (FOwnerObject as TdxRibbonBackstageViewTabSheetButtonControl).Item.Tab;
end;

{ TdxRibbonBackstageViewTabSheetButtonControl }

procedure TdxRibbonBackstageViewTabSheetButtonControl.ActivateTab;
begin
  if Enabled then
  begin
    Item.Tab.Active := True;
    Item.RefreshInfo;
    IAccessibilityHelper.Select(True);
  end;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControl.ControlActivate(AImmediately: Boolean; AByMouse: Boolean);
begin
  inherited ControlActivate(AImmediately, AByMouse);
  if not AByMouse then
    ActivateTab;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControl.ControlClick(AByMouse: Boolean; AKey: Char = #0);
begin
  inherited ControlClick(AByMouse, AKey);
  ActivateTab;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControl.ControlUnclick(AByMouse: Boolean);
begin
  inherited ControlUnclick(AByMouse);
  ActivateTab;
end;

function TdxRibbonBackstageViewTabSheetButtonControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxRibbonBackstageViewTabSheetButtonControlAccessibilityHelper;
end;

function TdxRibbonBackstageViewTabSheetButtonControl.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := Item.Tab.BackstageView;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControl.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareImage(const R: TRect; AState: Integer): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(R, True);
    MenuPainter.DrawTabButtonBackground(Result.Canvas.Handle, R, AState);
  end;

var
  ARect: TRect;
begin
  if Down then
  begin
    AFadeInImage := nil;
    AFadeOutImage := nil;
  end
  else
  begin
    ARect := cxRectSetNullOrigin(ItemBounds);
    AFadeOutImage := PrepareImage(ARect, DXBAR_NORMAL);
    AFadeInImage := PrepareImage(ARect, DXBAR_HOT);
  end;
end;

function TdxRibbonBackstageViewTabSheetButtonControl.GetViewStructure: TdxBarItemControlViewStructure;
begin
  Result := inherited;
  if BackstageView.RibbonStyle < rs2019 then
    Exclude(Result, cpIcon);
end;

function TdxRibbonBackstageViewTabSheetButtonControl.IsTabButtonStyle: Boolean;
begin
  Result := True;
end;

function TdxRibbonBackstageViewTabSheetButtonControl.IsWordWrapSupported: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonBackstageViewTabSheetButtonControl.PrepareCanvasFont(
  ABaseFont: HFONT; AStyle: TcxStyle; out ASavedFont: TdxBarSavedFont);
begin
  inherited PrepareCanvasFont(ABaseFont, AStyle, ASavedFont);

  if not ASavedFont.Saved then
  begin
    ASavedFont.Saved := True;
    ASavedFont.LogFont := dxGetFontData(Canvas.Font.Handle);
    Canvas.Font := MenuViewInfo.Fonts.MainMenuTab;
  end;
end;

function TdxRibbonBackstageViewTabSheetButtonControl.GetItem: TdxRibbonBackstageViewTabSheetButton;
begin
  Result := TdxRibbonBackstageViewTabSheetButton(inherited Item);
end;

{ TdxRibbonBackstageViewTabSheetButtonList }

function TdxRibbonBackstageViewTabSheetButtonList.GetItemByTab(
  ATab: TdxRibbonBackstageViewTabSheet): TdxRibbonBackstageViewTabSheetButton;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Tab = ATab then
      Exit(Items[I]);
  Result := nil;
end;

procedure TdxRibbonBackstageViewTabSheetButtonList.RefreshInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RefreshInfo;
end;

procedure TdxRibbonBackstageViewTabSheetButtonList.RemoveTab(ATab: TdxRibbonBackstageViewTabSheet);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Tab = ATab then
    begin
      FreeAndDelete(I);
      Break;
    end;
end;

function TdxRibbonBackstageViewTabSheetButtonList.GetItem(Index: Integer): TdxRibbonBackstageViewTabSheetButton;
begin
  Result := TdxRibbonBackstageViewTabSheetButton(inherited Items[Index]);
end;

{ TdxRibbonBackstageViewMenuButtonList }

procedure TdxRibbonBackstageViewMenuButtonList.RefreshInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RefreshInfo;
end;

function TdxRibbonBackstageViewMenuButtonList.GetItem(Index: Integer): TdxRibbonBackstageViewMenuBarButton;
begin
  Result := TdxRibbonBackstageViewMenuBarButton(inherited Items[Index]);
end;

{ TdxRibbonBackstageViewMenuViewInfo }

constructor TdxRibbonBackstageViewMenuViewInfo.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create(ABackstageView);
  FMenuButtonList := TdxRibbonBackstageViewMenuButtonList.Create;
  FTabButtonList := TdxRibbonBackstageViewTabSheetButtonList.Create;
  FDockControl := TdxRibbonBackstageViewMenuDockControl.Create(Self);
  FDockControl.AllowDocking := False;
  FDockControl.Parent := BackstageView;
  FDockControl.Visible := True;
  FDockControl.Align := dalNone;
  CreateViewInfoItems;
end;

destructor TdxRibbonBackstageViewMenuViewInfo.Destroy;
begin
  DestroyViewInfoItems;
  FreeAndNil(FMenuButtonList);
  FreeAndNil(FTabButtonList);
  FreeAndNil(FDockControl);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.AddButton(AButton: TdxRibbonBackstageViewMenuButton;
  APosition: TdxRibbonBackstageViewMenuButtonPosition; var ABeginGroup: Boolean);
var
  AItem: TdxRibbonBackstageViewMenuBarButton;
  AItemLink: TdxBarItemLink;
begin
  if Assigned(AButton.Item) then
  begin
    AItem := TdxRibbonBackstageViewMenuBarButton(BarManager.AddItem(TdxRibbonBackstageViewMenuBarButton));
    BarDesignController.AddInternalItem(AItem, MenuButtonList);
    AItem.MenuButton := AButton;
    AItemLink := ItemLinks.Add;
    AItemLink.Item := AItem;
    AItemLink.BeginGroup := (AButton.BeginGroup or ABeginGroup) and (ItemLinks.Count > 1);
    AItemLink.Data := Ord(APosition);
    ABeginGroup := False;
  end;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.AddButtons(
  APosition: TdxRibbonBackstageViewMenuButtonPosition; ABeginGroup: Boolean);
var
  AButton: TdxRibbonBackstageViewMenuButton;
  I: Integer;
begin
  for I := 0 to BackstageView.Buttons.Count - 1 do
  begin
    AButton := BackstageView.Buttons[I];
    if AButton.Position = APosition then
      AddButton(AButton, APosition, ABeginGroup);
  end;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.AddTabButton(
  ATab: TdxRibbonBackstageViewTabSheet; var ABeginGroup: Boolean);
var
  ATabButton: TdxRibbonBackstageViewTabSheetButton;
  AItemLink: TdxBarItemLink;
begin
  if ATab.TabVisible then
  begin
    ATabButton := TdxRibbonBackstageViewTabSheetButton(BarManager.AddItem(TdxRibbonBackstageViewTabSheetButton));
    BarDesignController.AddInternalItem(ATabButton, TabButtonList);
    ATabButton.Tab := ATab;
    AItemLink := ItemLinks.Add;
    AItemLink.Item := ATabButton;
    AItemLink.BeginGroup := (ATab.BeginGroup or ABeginGroup) and (ItemLinks.Count > 1);
    ABeginGroup := False;
  end;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.AddTabButtons(ABeginGroup: Boolean);
var
  I: Integer;
begin
  for I := 0 to BackstageView.TabCount - 1 do
    AddTabButton(BackstageView.Tabs[I], ABeginGroup);
end;

procedure TdxRibbonBackstageViewMenuViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  DockControl.BoundsRect := ABounds;
  DockControl.Visible := BackstageView.ShowMainMenu;
  DockControl.UpdateDock;
  if BarControl <> nil then
  begin
    BarControl.CalcLayout;
    BarControl.Invalidate;
  end;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.CalculateSizes;
begin
  if Assigned(BarControl) and IsBarManagerValid and BackstageView.ShowMainMenu then
  begin
    BarControl.HandleNeeded;
    BarControl.CreateControls;
    FMinWidth := BarControl.GetMaxWidth(dsLeft);
    FMinHeight := BarControl.GetMaxHeight(dsLeft);
  end
  else
  begin
    FMinHeight := 0;
    FMinWidth := 0;
  end;
end;

function TdxRibbonBackstageViewMenuViewInfo.CanContainItem(AItem: TdxBarItem; out AErrorText: string): Boolean;
begin
  Result := True;
end;

function TdxRibbonBackstageViewMenuViewInfo.CreateItemLinksBarControl: TCustomdxBarControl;
begin
  Result := TdxRibbonBackstageViewMenuBarControl.Create(BarManager);
end;

function TdxRibbonBackstageViewMenuViewInfo.CreatePainter(AData: TdxNativeUInt): TdxRibbonBackstageViewMenuPainter;
begin
  Result := TdxRibbonBackstageViewMenuPainter.Create(AData);
end;

procedure TdxRibbonBackstageViewMenuViewInfo.CreateBarControl;
begin
  if Assigned(BarManager) then
  begin
    ItemLinks.CreateBarControl;
    BarControl.DockControl := DockControl;
    BarControl.DockingStyle := dsLeft;
    BarControl.Parent := DockControl;
    BarControl.Visible := True;
  end;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.CreateViewInfoItems;

  function GetPainterData: TdxNativeUInt;
  begin
    if IsBarManagerValid then
      Result := TdxNativeUInt(Ribbon)
    else
      Result := 0;
  end;

begin
  FPainter := CreatePainter(GetPainterData);
  FItemLinks := TdxBarControlItemLinks.Create(BarManager, Self);
  DockControl.BarManager := BarManager;
  CreateBarControl;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.DestroyViewInfoItems;
begin
  DockControl.BarManager := nil;
  FreeAndNil(FItemLinks);
  FreeAndNil(FPainter);
end;

function TdxRibbonBackstageViewMenuViewInfo.GetAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if Assigned(ItemLinks) then
    Result := ItemLinks.BarControl.IAccessibilityHelper
  else
    Result := nil;
end;

function TdxRibbonBackstageViewMenuViewInfo.GetImages: TCustomImageList;
begin
  Result := nil;
end;

function TdxRibbonBackstageViewMenuViewInfo.GetInstance: TComponent;
begin
  Result := BackstageView;
end;

function TdxRibbonBackstageViewMenuViewInfo.GetItemLinks: TdxBarItemLinks;
begin
  Result := ItemLinks;
end;

function TdxRibbonBackstageViewMenuViewInfo.IsLoading: Boolean;
begin
  Result := BackstageView.IsLoading;
end;

function TdxRibbonBackstageViewMenuViewInfo.ShowSeparatorBetweenTabsAndButtons: Boolean;
begin
  Result := BackstageView.RibbonStyle = rs2019;
end;

function TdxRibbonBackstageViewMenuViewInfo.GetBarControl: TdxRibbonBackstageViewMenuBarControl;
begin
  Result := TdxRibbonBackstageViewMenuBarControl(ItemLinks.BarControl);
end;

function TdxRibbonBackstageViewMenuViewInfo.GetFonts: TdxRibbonBackstageViewFonts;
begin
  Result := BackstageView.Fonts;
end;

function TdxRibbonBackstageViewMenuViewInfo.GetIsDesigning: Boolean;
begin
  Result := BackstageView.IsDesigning;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.ClearInternalLists;
begin
  if ItemLinks <> nil then
    ItemLinks.Clear;
  TabButtonList.Clear;
  MenuButtonList.Clear;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.InitiateActions;
begin
  if ItemLinks <> nil then
    TdxBarItemLinksAccess(ItemLinks).InitiateActions;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.RecreateItemLinks;
begin
  if IsBarManagerValid and BackstageView.HandleAllocated then
  begin
    BarManager.BeginUpdate;
    try
      ItemLinks.BeginUpdate;
      try
        ClearInternalLists;
        AddButtons(mbpBeforeTabs, False);
        AddTabButtons(ShowSeparatorBetweenTabsAndButtons);
        AddButtons(mbpAfterTabs, ShowSeparatorBetweenTabsAndButtons);
      finally
        ItemLinks.EndUpdate;
      end;
    finally
      BarManager.EndUpdate;
    end;
  end
  else
    ClearInternalLists;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.RefreshMenuButtonsInfo;
begin
  MenuButtonList.RefreshInfo;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.RefreshTabsInfo;
begin
  TabButtonList.RefreshInfo;
end;

procedure TdxRibbonBackstageViewMenuViewInfo.UpdateFont;
begin
  if BarControl <> nil then
    BarControl.UpdateFont;
end;

{ TdxRibbonBackstageViewViewInfo }

constructor TdxRibbonBackstageViewViewInfo.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create(ABackstageView);
  FMenuViewInfo := CreateMenuViewInfo;
end;

destructor TdxRibbonBackstageViewViewInfo.Destroy;
begin
  FreeAndNil(FMenuViewInfo);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  MenuViewInfo.CalculateSizes;
  FContentWidth := Max(CalculateMinWidth, cxRectWidth(Bounds));
  FContentHeight := Max(CalculateMinHeight, cxRectHeight(Bounds));
  CheckScrollPosition(FScrollPositionX, ContentWidth, cxRectWidth(Bounds));
  CheckScrollPosition(FScrollPositionY, ContentHeight, cxRectHeight(Bounds));
  FFrameAreaBounds := CalculateFrameBounds;
  if not BackstageView.UseRightToLeftAlignment then
    MenuViewInfo.Calculate(CalculateMenuBounds);
end;

procedure TdxRibbonBackstageViewViewInfo.DeactivateControls;
begin
  if MenuBarControl <> nil then
    MenuBarControl.IsActive := False;
end;

function TdxRibbonBackstageViewViewInfo.CalculateFrameBounds: TRect;
begin
  Result := ContentBounds;
  Result.Left := CalculateMenuBounds.Right;
end;

function TdxRibbonBackstageViewViewInfo.CalculateMenuBounds: TRect;
begin
  Result := cxRectSetWidth(ContentBounds, MenuViewInfo.MinWidth);
  Result.Bottom := Bounds.Bottom;
end;

function TdxRibbonBackstageViewViewInfo.CalculateMinHeight: Integer;
begin
  Result := MenuViewInfo.MinHeight;
  if Assigned(ActiveTab) then
    Result := Max(Result, ActiveTab.ViewInfo.MinHeight);
end;

function TdxRibbonBackstageViewViewInfo.CalculateMinWidth: Integer;
begin
  Result := MenuViewInfo.MinWidth;
  if Assigned(ActiveTab) then
    Inc(Result, ActiveTab.ViewInfo.MinWidth);
end;

procedure TdxRibbonBackstageViewViewInfo.CheckScrollPosition(
  var AValue: Integer; AContentSize, ADisplaySize: Integer);
begin
  AValue := Max(0, Min(AValue, AContentSize - ADisplaySize));
end;

function TdxRibbonBackstageViewViewInfo.CreateMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := TdxRibbonBackstageViewMenuViewInfo.Create(BackstageView);
end;

procedure TdxRibbonBackstageViewViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  AMenuBounds: TRect;
begin
  inherited DoRightToLeftConversion(ABounds);
  FFrameAreaBounds := TdxRightToLeftLayoutConverter.ConvertRect(FFrameAreaBounds, ABounds);
  AMenuBounds := TdxRightToLeftLayoutConverter.ConvertRect(CalculateMenuBounds, ABounds);
  MenuViewInfo.Calculate(AMenuBounds);
end;

function TdxRibbonBackstageViewViewInfo.ProcessMouseWheel(ALineDown: Boolean): Boolean;
const
  Signs: array[Boolean] of Integer = (-1, 1);
begin
  Result := ContentHeight > cxRectHeight(Bounds);
  if Result then
    ScrollPositionY := ScrollPositionY + Signs[ALineDown] *
      Mouse.WheelScrollLines * dxBackstageViewScrollLineSize;
end;

function TdxRibbonBackstageViewViewInfo.GetActiveTab: TdxRibbonBackstageViewTabSheet;
begin
  Result := BackstageView.ActiveTab;
end;

function TdxRibbonBackstageViewViewInfo.GetActiveTabItemControl: TdxBarItemControl;
var
  AItem: TdxRibbonBackstageViewTabSheetButton;
begin
  AItem := MenuViewInfo.TabButtonList.GetItemByTab(ActiveTab);
  if (AItem <> nil) and (AItem.LinkCount > 0) and (AItem.Links[0].Control <> nil) then
    Result := AItem.Links[0].Control
  else
    Result := nil;
end;

function TdxRibbonBackstageViewViewInfo.GetContentBounds: TRect;
begin
  Result := Classes.Bounds(Bounds.Left - ScrollPositionX, Bounds.Top - ScrollPositionY, ContentWidth, ContentHeight);
end;

function TdxRibbonBackstageViewViewInfo.GetDesignSelectorRect: TRect;
begin
  Result := TdxControlsDesignSelectorHelper.CalculateBounds(Bounds, BackstageView.ScaleFactor);
end;

function TdxRibbonBackstageViewViewInfo.GetFrameAreaVisibleBounds: TRect;
begin
  Result := Bounds;
  Result.Left := CalculateMenuBounds.Right;
end;

function TdxRibbonBackstageViewViewInfo.GetMenuBarControl: TdxBarControl;
begin
  Result := MenuViewInfo.BarControl;
end;

procedure TdxRibbonBackstageViewViewInfo.SetScrollPositionX(AValue: Integer);
begin
  CheckScrollPosition(AValue, ContentWidth, cxRectWidth(Bounds));
  if FScrollPositionX <> AValue then
  begin
    FScrollPositionX := AValue;
    BackstageView.Changed;
  end;
end;

procedure TdxRibbonBackstageViewViewInfo.SetScrollPositionY(AValue: Integer);
begin
  CheckScrollPosition(AValue, ContentHeight, cxRectHeight(Bounds));
  if FScrollPositionY <> AValue then
  begin
    FScrollPositionY := AValue;
    BackstageView.Changed;
  end;
end;

{ TdxRibbonBackstageViewMenuBarButton }

procedure TdxRibbonBackstageViewMenuBarButton.DirectClick;
var
  ABackstageView: TdxRibbonCustomBackstageView;
begin
  if Enabled and (MenuButton <> nil) then
  begin
    ABackstageView := MenuButton.Collection.BackstageView;
    ABackstageView.ClosePopup;
    ABackstageView.MenuHelper.PostClick(MenuButton);
  end;
end;

procedure TdxRibbonBackstageViewMenuBarButton.RefreshInfo;
begin
  if MenuButton <> nil then
    Assign(MenuButton.Item);
end;

function TdxRibbonBackstageViewMenuBarButton.GetControlClass(
  AIsVertical: Boolean): TdxBarItemControlClass;
begin
  Result := TdxRibbonBackstageViewMenuBarButtonControl;
end;

procedure TdxRibbonBackstageViewMenuBarButton.SetMenuButton(AValue: TdxRibbonBackstageViewMenuButton);
begin
  if FMenuButton <> AValue then
  begin
    FMenuButton := AValue;
    RefreshInfo;
  end;
end;

{ TdxRibbonBackstageViewMenuBarButtonControl }

procedure TdxRibbonBackstageViewMenuBarButtonControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  inherited;
  if BackstageView.IsPersistentSelected(Item.MenuButton) then
    dxBarFocusRect(Canvas.Handle, ARect);
end;

function TdxRibbonBackstageViewMenuBarButtonControl.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := Item.MenuButton.Collection.BackstageView;
end;

function TdxRibbonBackstageViewMenuBarButtonControl.IsTabButtonStyle: Boolean;
begin
  Result := not HasImage and (BackstageView.RibbonStyle in [rs2013..rs2016Tablet]);
end;

function TdxRibbonBackstageViewMenuBarButtonControl.GetItem: TdxRibbonBackstageViewMenuBarButton;
begin
  Result := TdxRibbonBackstageViewMenuBarButton(inherited Item);
end;

{ TdxRibbonBackstageViewMenuButton }

destructor TdxRibbonBackstageViewMenuButton.Destroy;
begin
  Item := nil;
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewMenuButton.Assign(Source: TPersistent);
begin
  if Source is TdxRibbonBackstageViewMenuButton then
  begin
    Item := TdxRibbonBackstageViewMenuButton(Source).Item;
    Position := TdxRibbonBackstageViewMenuButton(Source).Position;
    BeginGroup := TdxRibbonBackstageViewMenuButton(Source).BeginGroup;
  end
  else
    inherited Assign(Source);
end;

procedure TdxRibbonBackstageViewMenuButton.BarComponentChanged(AComponent: TdxBarComponent);
begin
  Changed(AComponent = nil);
end;

procedure TdxRibbonBackstageViewMenuButton.SelectionChanged;
begin
  Collection.BackstageView.InvalidateWithChildren;
end;

procedure TdxRibbonBackstageViewMenuButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FItem) and (Operation = opRemove) then
  begin
    BarDesignController.LockDesignerModified;
    try
      FItem := nil;
      Changed(True);
    finally
      BarDesignController.UnLockDesignerModified;
    end;
  end;
end;

function TdxRibbonBackstageViewMenuButton.GetCollection: TdxRibbonBackstageViewMenuButtons;
begin
  Result := TdxRibbonBackstageViewMenuButtons(inherited Collection);
end;

procedure TdxRibbonBackstageViewMenuButton.SetBeginGroup(AValue: Boolean);
begin
  if FBeginGroup <> AValue then
  begin
    FBeginGroup := AValue;
    Changed(True);
  end;
end;

procedure TdxRibbonBackstageViewMenuButton.SetItem(AValue: TdxBarCustomButton);
begin
  if AValue <> Item then
  begin
    if Item <> nil then
    begin
      TdxBarButtonAccess(Item).RemoveChangeNotify(Self);
      Item.RemoveFreeNotification(Collection.BackstageView);
    end;
    FItem := AValue;
    if Item <> nil then
    begin
      Item.FreeNotification(Collection.BackstageView);
      TdxBarButtonAccess(Item).AddChangeNotify(Self);
    end;
    Changed(True);
  end;
end;

procedure TdxRibbonBackstageViewMenuButton.SetPosition(AValue: TdxRibbonBackstageViewMenuButtonPosition);
begin
  if AValue <> FPosition then
  begin
    FPosition := AValue;
    Changed(True);
  end;
end;

{ TdxRibbonBackstageViewMenuButtons }

constructor TdxRibbonBackstageViewMenuButtons.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create(TdxRibbonBackstageViewMenuButton);
  FBackstageView := ABackstageView
end;

function TdxRibbonBackstageViewMenuButtons.Add: TdxRibbonBackstageViewMenuButton;
begin
  Result := TdxRibbonBackstageViewMenuButton(inherited Add);
end;

function TdxRibbonBackstageViewMenuButtons.IndexOf(AItem: TdxRibbonBackstageViewMenuButton): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I] = AItem then
    begin
      Result := I;
      Break;
    end;
end;

function TdxRibbonBackstageViewMenuButtons.Insert(AIndex: Integer): TdxRibbonBackstageViewMenuButton;
begin
  Result := TdxRibbonBackstageViewMenuButton(inherited Insert(AIndex));
end;

function TdxRibbonBackstageViewMenuButtons.GetOwner: TPersistent;
begin
  Result := BackstageView;
end;

procedure TdxRibbonBackstageViewMenuButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Item = nil then
    BackstageView.Changed([rbvcStruct])
  else
    BackstageView.Changed([rbvcItemsData]);
end;

procedure TdxRibbonBackstageViewMenuButtons.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Notification(AComponent, Operation);
end;

function TdxRibbonBackstageViewMenuButtons.GetItem(Index: Integer): TdxRibbonBackstageViewMenuButton;
begin
  Result := TdxRibbonBackstageViewMenuButton(inherited GetItem(Index));
end;

procedure TdxRibbonBackstageViewMenuButtons.SetItem(Index: Integer; Value: TdxRibbonBackstageViewMenuButton);
begin
  inherited SetItem(Index, Value);
end;

{ TdxRibbonBackstageViewMenuHelper }

constructor TdxRibbonBackstageViewMenuHelper.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create;
  FBackstageView := ABackstageView;
end;

procedure TdxRibbonBackstageViewMenuHelper.PostClick(AButton: TdxRibbonBackstageViewMenuButton);
begin
  PostMessage(Handle, DXM_BAR_FASTCOMMAND, 0, LPARAM(AButton));
end;

procedure TdxRibbonBackstageViewMenuHelper.PostShowRibbonKeyTips;
begin
  PostMessage(Handle, DXM_BAR_SHOWKEYTIPS, 0, 0);
end;

procedure TdxRibbonBackstageViewMenuHelper.WndProc(var Message: TMessage);
var
  AMenuButton: TdxRibbonBackstageViewMenuButton;
begin
  case Message.Msg of
    DXM_BAR_SHOWKEYTIPS:
      BarNavigationController.SetKeyTipsShowingState(BackstageView.Ribbon.IAccessibilityHelper, '');

    DXM_BAR_FASTCOMMAND:
      begin
        AMenuButton := TdxRibbonBackstageViewMenuButton(Message.LParam);
        if BackstageView.Buttons.IndexOf(AMenuButton) >= 0 then
        begin
          if AMenuButton.Item <> nil then
            TdxBarButtonAccess(AMenuButton.Item).DirectClick;
        end;
      end
  end;
  inherited WndProc(Message);
end;

{ TdxRibbonBackstageViewApplicationMenuHelper }

constructor TdxRibbonBackstageViewApplicationMenuHelper.Create(
  ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create;
  FLinkToBackstageView := cxAddObjectLink(ABackstageView);
end;

destructor TdxRibbonBackstageViewApplicationMenuHelper.Destroy;
begin
  cxRemoveObjectLink(FLinkToBackstageView);
  inherited Destroy;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
begin
  Result := BackstageView.CanShowPopup(ARibbon);
end;

function TdxRibbonBackstageViewApplicationMenuHelper.ClosePopup: Boolean;
begin
  Result := (BackstageView <> nil) and BackstageView.DoClosePopup;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
begin
  Result := BackstageView.NonClientViewInfo.DisplayMode;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.Popup(
  ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean;
begin
  Result := BackstageView.Popup(ARibbon, AClosedByEscape);
end;

procedure TdxRibbonBackstageViewApplicationMenuHelper.RibbonFormResized;
begin
  BackstageView.CalculatePlace;
end;

procedure TdxRibbonBackstageViewApplicationMenuHelper.SelectAppMenuFirstItemControl;
begin
  BarNavigationController.ChangeSelectedObject(False,
    BackstageView.MenuViewInfo.AccessibilityHelper.GetBarHelper.GetFirstSelectableObject);
end;

procedure TdxRibbonBackstageViewApplicationMenuHelper.ShowKeyTips;
begin
  BarNavigationController.SetKeyTipsShowingState(BackstageView.MenuViewInfo.AccessibilityHelper, '');
  SelectAppMenuFirstItemControl;
end;

procedure TdxRibbonBackstageViewApplicationMenuHelper.UpdateNonClientArea;
begin
  BackstageView.FullInvalidate;
  BackstageView.Update;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.GetBackstageView: TdxRibbonCustomBackstageView;
begin
  Result := TdxRibbonCustomBackstageView(FLinkToBackstageView.Ref);
end;

function TdxRibbonBackstageViewApplicationMenuHelper.GetOrigin(AIsClientArea: Boolean): TPoint;
begin
  Result := cxNullPoint;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.GetRootAccessibilityHelper: IdxBarAccessibilityHelper;
begin
  if Assigned(BackstageView.MenuViewInfo.BarControl) and BackstageView.ShowMainMenu then
    Result := BackstageView.MenuViewInfo.BarControl.IAccessibilityHelper
  else
    Result := nil;
end;

function TdxRibbonBackstageViewApplicationMenuHelper.IsVisible: Boolean;
begin
  Result := (BackstageView <> nil) and BackstageView.Visible;
end;

procedure TdxRibbonBackstageViewApplicationMenuHelper.GetTabOrderList(AList: TList);
begin
  BackstageView.GetTabOrderList(AList);
  if AList.IndexOf(BackstageView) < 0 then
    AList.Add(BackstageView);
end;

{ TdxRibbonBackstageViewCustomButtonViewInfo }

function TdxRibbonBackstageViewCustomButtonViewInfo.GetState: TcxButtonState;
begin
  if Self <> BackstageView.NonClientController.HoveredCell then
    Result := cxbsNormal
  else
    if Self <> BackstageView.NonClientController.PressedCell then
      Result := cxbsHot
    else
      Result := cxbsPressed;
end;

{ TdxRibbonBackstageViewBackButtonViewInfo }

procedure TdxRibbonBackstageViewBackButtonViewInfo.Click;
begin
  BackstageView.ClosePopup;
end;

procedure TdxRibbonBackstageViewBackButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Painter.DrawBackButton(ACanvas, Bounds, State);
end;

{ TdxRibbonBackstageViewNonClientViewInfo }

constructor TdxRibbonBackstageViewNonClientViewInfo.Create(ABackstageView: TdxRibbonCustomBackstageView);
begin
  inherited Create(ABackstageView);
  FBackButtonViewInfo := CreateBackButtonViewInfo;
end;

destructor TdxRibbonBackstageViewNonClientViewInfo.Destroy;
begin
  FreeAndNil(FBackButtonViewInfo);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  CalculateCaptionArea;
  CalculateBackButton;
  CalculateMenuBarHeader;
  if BackstageView.UseRightToLeftAlignment then
    RightToLeftConversion(ABounds);
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.CalculateBackButton;
begin
  BackButtonViewInfo.Calculate(CalculateBackButtonBounds);
end;

function TdxRibbonBackstageViewNonClientViewInfo.CalculateBackButtonBounds: TRect;
var
  ASize: TSize;
begin
  ASize := Painter.BackButtonSize;
  if BackstageView.RibbonStyle = rs2019 then
    ASize.cx := cxRectWidth(MenuViewInfo.Bounds);
  Result := cxRectBounds(Bounds.Left + Painter.BackButtonOffset, TabsArea.Bottom - ASize.cy, ASize.cx, ASize.cy);
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.CalculateCaptionArea;
var
  AOffset: TPoint;
begin
  FTabAreaToolbarArea := cxNullRect;
  if DisplayMode = amdmFrameFullScreen then
  begin
    AOffset := dxMapWindowPoint(Ribbon.Handle, BackstageView.Handle, cxNullPoint, False);
    FBorderIconsArea := cxRectOffset(RibbonFormCaptionHelper.BorderIconsArea, AOffset);
    if Ribbon.ViewInfo.TabsAreaViewInfo.IsToolbarVisibleOnBackstageView then
      FTabAreaToolbarArea := cxRectOffset(Ribbon.ViewInfo.TabsAreaViewInfo.ToolbarBounds, AOffset);
    FCaptionArea := cxRectOffset(RibbonFormCaptionHelper.FormCaptionDrawBounds, AOffset);
    FCaptionTextRect := FCaptionArea;
    if BackstageView.UseRightToLeftAlignment then
    begin
      FCaptionTextRect.Left := BorderIconsArea.Right;
      FCaptionTextRect.Right := Min(FCaptionTextRect.Right, Bounds.Right - MenuViewInfo.MinWidth -
        TdxCustomRibbonAccess(Ribbon).ScaleFactor.Apply(dxRibbonFormCaptionTextSpace));
    end
    else
    begin
      FCaptionTextRect.Right := BorderIconsArea.Left;
      FCaptionTextRect.Left := Max(FCaptionTextRect.Left, Bounds.Left + MenuViewInfo.MinWidth +
        TdxCustomRibbonAccess(Ribbon).ScaleFactor.Apply(dxRibbonFormCaptionTextSpace));
    end;
    FBackgroundImageRect := cxRectOffset(Ribbon.ViewInfo.BackgroundImageBounds, AOffset);
    FTabsArea := cxRectOffset(Ribbon.ViewInfo.TabsAreaViewInfo.Bounds, AOffset);
    if TabsArea.Bottom <= TabsArea.Top then
      FTabsArea.Bottom := TabsArea.Top + cxRectHeight(CaptionArea);
   if BackstageView.RibbonStyle = rs2019 then
      FTabsArea.Bottom := Max(TabsArea.Bottom, TabsArea.Top + MenuViewInfo.Painter.TabButtonDefaultHeight);
  end
  else
  begin
    FBackgroundImageRect := cxNullRect;
    FTabsArea := cxRectSetHeight(Bounds, 0);
    FCaptionArea := TabsArea;
    FCaptionTextRect := FCaptionArea;
    FBorderIconsArea := cxNullRect;
  end;
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.CalculateMenuBarHeader;
begin
  FMenuBarHeaderRect := cxRectOffset(MenuViewInfo.Bounds, ClientOffsets.TopLeft);
  FMenuBarHeaderRect.Top := Bounds.Top;
  FMenuBarHeaderRect.Bottom := TabsArea.Bottom;
end;

function TdxRibbonBackstageViewNonClientViewInfo.CreateBackButtonViewInfo: TdxRibbonBackstageViewBackButtonViewInfo;
begin
  Result := TdxRibbonBackstageViewBackButtonViewInfo.Create(BackstageView);
end;

function TdxRibbonBackstageViewNonClientViewInfo.CreateWindowRegion: TcxRegionHandle;

  procedure ExcludeRect(var ARegion: TcxRegionHandle; const R: TRect);
  var
    ATempRegion: TcxRegionHandle;
  begin
    if ARegion = 0 then
      ARegion := CreateRectRgnIndirect(Bounds);
    if not cxRectIsEmpty(R) then
    begin
      ATempRegion := CreateRectRgnIndirect(R);
      CombineRgn(ARegion, ARegion, ATempRegion, RGN_DIFF);
      DeleteObject(ATempRegion);
    end;
  end;

begin
  Result := 0;
  if DisplayMode = amdmFrameFullScreen then
  begin
    ExcludeRect(Result, BorderIconsArea);
    if Painter.CanShowTabAreaToolbar then
      ExcludeRect(Result, TabAreaToolbarArea);
  end;
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  BackButtonViewInfo.RightToLeftConversion(ABounds);
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.Draw(ACanvas: TcxCanvas);
begin
  DrawBackground(ACanvas);
  DrawCaptionArea(ACanvas);
  DrawMenuBarHeader(ACanvas);
  BackButtonViewInfo.Draw(ACanvas);
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.DrawCaptionArea(ACanvas: TcxCanvas);
var
  R: TRect;
  ATextFlags: Cardinal;
begin
  if DisplayMode = amdmFrameFullScreen then
  begin
    Ribbon.ViewInfo.Painter.DrawBackgroundImage(ACanvas, BackgroundImageRect);

    R := CaptionTextRect;
    ACanvas.Font := Ribbon.ViewInfo.GetFormCaptionFont(RibbonFormCaptionHelper.FormPaintData.GetIsActive);
    ACanvas.Font.Color := Ribbon.ColorScheme.GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR);
    ATextFlags := CXTO_PREVENT_LEFT_EXCEED or CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS;
    if BackstageView.UseRightToLeftReading then
      ATextFlags := ATextFlags or CXTO_RTLREADING;
    cxTextOut(ACanvas.Handle, Ribbon.ViewInfo.DocumentName + Ribbon.ViewInfo.Caption, R, ATextFlags);
  end;
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawBackground(ACanvas, Bounds);
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.DrawMenuBarHeader(ACanvas: TcxCanvas);
begin
  if ACanvas.RectVisible(MenuBarHeaderRect) then
  begin
    if cxRectPtIn(ContentRect, MenuBarHeaderRect.TopLeft) and cxRectPtIn(ContentRect, MenuBarHeaderRect.BottomRight) then
      Painter.DrawMenuBarHeader(ACanvas, MenuBarHeaderRect)
    else
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ContentRect);
        Painter.DrawMenuBarHeader(ACanvas, MenuBarHeaderRect);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  end;
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetClientOffsets: TRect;
begin
  Result := Painter.ContentOffsets;
  Inc(Result.Top, TabsArea.Bottom);
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetClientRect: TRect;
begin
  Result := cxRectContent(Bounds, ClientOffsets);
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetContentRect: TRect;
begin
  Result := cxRectContent(Bounds, Painter.ContentOffsets);
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetDisplayMode: TdxRibbonApplicationMenuDisplayMode;
begin
  if (RibbonFormCaptionHelper <> nil) and not UseAeroNCPaint(RibbonFormCaptionHelper.FormPaintData) and
    (BackstageView.RibbonStyle >= rs2013) and not BackstageView.IsDesigning
  then
    Result := amdmFrameFullScreen
  else
    Result := amdmFrame;
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetHasNonClientArea: Boolean;
begin
  Result := not cxRectIsEqual(ClientOffsets, cxNullRect);
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := BackstageView.MenuViewInfo;
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetRibbon: TdxCustomRibbon;
begin
  Result := BackstageView.Ribbon;
end;

function TdxRibbonBackstageViewNonClientViewInfo.GetRibbonFormCaptionHelper: TdxRibbonFormCaptionHelper;
begin
  if Ribbon <> nil then
    Result := TdxCustomRibbonAccess(Ribbon).FormCaptionHelper
  else
    Result := nil;
end;

procedure TdxRibbonBackstageViewNonClientViewInfo.Recalculate;
begin
  Calculate(Bounds);
end;

{ TdxRibbonBackstageViewNonClientController }

destructor TdxRibbonBackstageViewNonClientController.Destroy;
begin
  EndMouseTracking(Self);
  inherited Destroy;
end;

function TdxRibbonBackstageViewNonClientController.HitTest(const P: TPoint): TdxRibbonBackstageViewCustomButtonViewInfo;
begin
  if PtInRect(ViewInfo.BackButtonViewInfo.Bounds, P) then
    Result := ViewInfo.BackButtonViewInfo
  else
    Result := nil;
end;

procedure TdxRibbonBackstageViewNonClientController.MouseDown(AButton: TMouseButton; const P: TPoint);
begin
  PressedCell := HitTest(P);
end;

procedure TdxRibbonBackstageViewNonClientController.MouseLeave;
begin
  HoveredCell := nil;
end;

procedure TdxRibbonBackstageViewNonClientController.MouseMove(const P: TPoint);
begin
  if PressedCell <> nil then
  begin
    if not (ssLeft in KeyboardStateToShiftState) then
      PressedCell := nil;
  end;
  BeginMouseTracking(nil, cxNullRect, Self);
  HoveredCell := HitTest(P);
end;

procedure TdxRibbonBackstageViewNonClientController.MouseUp(AButton: TMouseButton; const P: TPoint);
var
  ALastPressedCell: TdxRibbonBackstageViewCustomButtonViewInfo;
begin
  if PressedCell <> nil then
  begin
    ALastPressedCell := PressedCell;
    PressedCell := nil;
    if ALastPressedCell = HoveredCell then
      ALastPressedCell.Click;
  end;
end;

procedure TdxRibbonBackstageViewNonClientController.ProcessMessage(var AMessage: TMessage);

  function DoScreenToLocal(const AMessage: TWMNCHitMessage): TPoint;
  begin
    Result := ScreenToLocal(Point(AMessage.XCursor, AMessage.YCursor));
  end;

var
  P: TPoint;
begin
  case AMessage.Msg of
    WM_NCLBUTTONDOWN:
      MouseDown(mbLeft, DoScreenToLocal(TWMNCLButtonDown(AMessage)));
    WM_NCMOUSEMOVE:
      MouseMove(DoScreenToLocal(TWMNCMouseMove(AMessage)));
    WM_NCLBUTTONUP:
      MouseUp(mbLeft, DoScreenToLocal(TWMNCLButtonUp(AMessage)));
    WM_NCHITTEST:
      begin
        P := ScreenToLocal(TWMNCHitTest(AMessage).Pos);
        if HitTest(P) <> nil then
          AMessage.Result := HTOBJECT
        else
          if PtInRect(ViewInfo.CaptionTextRect, P) then
            AMessage.Result := HTTRANSPARENT;
      end;
  end;
end;

function TdxRibbonBackstageViewNonClientController.PtInCaller(const P: TPoint): Boolean;
begin
  Result := HitTest(ScreenToLocal(P)) <> nil;
end;

procedure TdxRibbonBackstageViewNonClientController.RefreshState;
begin
  cxRedrawWindow(BackstageView.Handle, RDW_FRAME or RDW_INVALIDATE);
end;

function TdxRibbonBackstageViewNonClientController.ScreenToLocal(const P: TPoint): TPoint;
begin
  Result := cxPointOffset(P, cxGetWindowRect(BackstageView).TopLeft, False);
end;

function TdxRibbonBackstageViewNonClientController.ScreenToLocal(const P: TSmallPoint): TPoint;
begin
  Result := ScreenToLocal(SmallPointToPoint(P));
end;

procedure TdxRibbonBackstageViewNonClientController.SetHoveredCell(
  AValue: TdxRibbonBackstageViewCustomButtonViewInfo);
begin
  if FHoveredCell <> AValue then
  begin
    FHoveredCell := AValue;
    RefreshState;
  end;
end;

procedure TdxRibbonBackstageViewNonClientController.SetPressedCell(
  AValue: TdxRibbonBackstageViewCustomButtonViewInfo);
begin
  if FPressedCell <> AValue then
  begin
    FPressedCell := AValue;
    RefreshState;
  end;
end;

function TdxRibbonBackstageViewNonClientController.GetViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
begin
  Result := BackstageView.NonClientViewInfo;
end;

{ TdxRibbonBackstageViewAnimationTransition }

constructor TdxRibbonBackstageViewAnimationTransition.Create(
  ABackstageView: TdxRibbonCustomBackstageView; AMode: TdxRibbonBackstageViewAnimationTransitionMode);
begin
  FMode := AMode;
  FBackstageView := ABackstageView;
  inherited Create(dxRibbonBackstageViewMenuScrollAnimationTime,
    ateAccelerateDecelerate, Max(MaxByte, MenuViewInfo.MinWidth));
  FActiveTabIsSolidBackground := True;
  Initialize;
end;

destructor TdxRibbonBackstageViewAnimationTransition.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

function TdxRibbonBackstageViewAnimationTransition.CalculateContentAlpha: Byte;
begin
  Result := MulDiv(Position, MaxByte, Length);
  if Mode = bvatmHide then
    Result := MaxByte - Result;
end;

procedure TdxRibbonBackstageViewAnimationTransition.CalculateViewInfos;
var
  AOffset: Integer;
  AMenuBounds: TRect;
begin
  AOffset := -MulDiv(MenuViewInfo.MinWidth, Position, Length);
  if Mode = bvatmShow then
    AOffset := -(MenuViewInfo.MinWidth + AOffset);

  AMenuBounds := ViewInfo.CalculateMenuBounds;
  if BackstageView.UseRightToLeftAlignment then
  begin
    AOffset := - AOffset;
    AMenuBounds := TdxRightToLeftLayoutConverter.ConvertRect(AMenuBounds, ViewInfo.Bounds);
  end;
  MenuViewInfo.Calculate(cxRectOffset(AMenuBounds, AOffset, 0));
  NonClientViewInfo.Recalculate;
  NonClientViewInfo.BackButtonViewInfo.Calculate(cxRectOffset(NonClientViewInfo.BackButtonViewInfo.Bounds, AOffset, 0));

  if ActiveTab <> nil then
    ActiveTab.Calculate(cxRectOffset(ViewInfo.FrameAreaBounds, AOffset, 0));
end;

procedure TdxRibbonBackstageViewAnimationTransition.DoAnimate;
var
  DC: HDC;
begin
  CalculateViewInfos;
  PrepareBackstageViewBuffer(BackstageViewBuffer.cxCanvas);
  DC := GetDCEx(BackstageView.Handle, 0, DCX_CACHE or DCX_WINDOW or DCX_LOCKWINDOWUPDATE);
  try
    cxBitBlt(DC, BackstageViewBuffer.Canvas.Handle, BackstageViewRect, cxNullPoint, SRCCOPY);
  finally
    ReleaseDC(BackstageView.Handle, DC);
  end;
end;

procedure TdxRibbonBackstageViewAnimationTransition.Finalize;
begin
  FreeAndNil(FBackstageViewMenuBuffer);
  FreeAndNil(FBackstageViewBuffer);
  FreeAndNil(FActiveTabBuffer);
  if Mode = bvatmShow then
  begin
    if ActiveTab <> nil then
      ActiveTab.Visible := True;
  end;
end;

procedure TdxRibbonBackstageViewAnimationTransition.Initialize;
begin
  if ActiveTab <> nil then
    ActiveTab.Visible := False;
  UpdateWindow(BackstageView.Ribbon.Handle);

  if ActiveTab <> nil then
  begin
    FActiveTabRect := cxGetWindowBounds(ActiveTab);
    FActiveTabBuffer := TcxBitmap.CreateSize(ActiveTabRect, pf24bit);
  end;

  FBackstageViewRect := cxGetWindowBounds(BackstageView);
  FBackstageViewBuffer := TcxBitmap.CreateSize(FBackstageViewRect);

  FBackstageViewMenuBuffer := TcxBitmap.CreateSize(BackstageView.MenuViewInfo.Bounds);
  cxPaintTo(BackstageView.MenuViewInfo.DockControl,
    BackstageViewMenuBuffer.cxCanvas, cxNullPoint, BackstageViewMenuBuffer.ClientRect);

  if (ActiveTab <> nil) and ActiveTabIsSolidBackground then
    cxPaintTo(ActiveTab, ActiveTabBuffer.cxCanvas, cxNullPoint, ActiveTabRect);
end;

procedure TdxRibbonBackstageViewAnimationTransition.PrepareBackstageViewBuffer(ACanvas: TcxCanvas);
var
  AControl: TControl;
  ASkipList: TList;
  I: Integer;
  R: TRect;
begin
  ACanvas.Lock;
  try
    ASkipList := TList.Create;
    try
      if ActiveTab <> nil then
        ASkipList.Add(Pointer(ActiveTab.Handle));
      cxPaintTo(BackstageView, ACanvas, cxNullPoint, BackstageViewRect, ASkipList);
    finally
      ASkipList.Free;
    end;

    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(NonClientViewInfo.ClientRect);

      for I := BackstageView.ControlCount - 1 downto 0 do
      begin
        AControl := BackstageView.Controls[I];
        if AControl.Visible and BackstageView.IsInternalControl(AControl) then
          ACanvas.ExcludeClipRect(cxRectOffset(AControl.BoundsRect, NonClientViewInfo.ClientRect.TopLeft));
      end;

      cxBitBlt(ACanvas.Handle, BackstageViewMenuBuffer.Canvas.Handle,
        cxRectOffset(BackstageView.MenuViewInfo.Bounds, NonClientViewInfo.ClientRect.TopLeft),
        cxNullPoint, SRCCOPY);

      if ActiveTab <> nil then
      begin
        if not ActiveTabIsSolidBackground then
          cxPaintTo(ActiveTab, ActiveTabBuffer.cxCanvas, cxNullPoint, ActiveTabRect);

        R := dxMapWindowRect(ActiveTab.Handle, BackstageView.Handle, ActiveTabRect, False);
        if not SystemAlphaBlend(ACanvas.Handle, ActiveTabBuffer.Canvas.Handle,
          R, ActiveTabBuffer.ClientRect, CalculateContentAlpha, False)
        then
          cxBitBlt(ACanvas.Handle, ActiveTabBuffer.Canvas.Handle, R, cxNullPoint, SRCCOPY);
      end;
    finally
      ACanvas.RestoreClipRegion;
    end;
  finally
    ACanvas.Unlock;
  end;
end;

function TdxRibbonBackstageViewAnimationTransition.GetActiveTab: TdxRibbonBackstageViewTabSheet;
begin
  Result := BackstageView.ActiveTab;
end;

function TdxRibbonBackstageViewAnimationTransition.GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := BackstageView.MenuViewInfo;
end;

function TdxRibbonBackstageViewAnimationTransition.GetNonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
begin
  Result := BackstageView.NonClientViewInfo;
end;

function TdxRibbonBackstageViewAnimationTransition.GetViewInfo: TdxRibbonBackstageViewViewInfo;
begin
  Result := BackstageView.ViewInfo;
end;

{ TdxRibbonBackstageViewSizeGrip }

function TdxRibbonBackstageViewSizeGrip.GetRibbon: TdxCustomRibbon;
begin
  if Parent is TdxRibbonCustomBackstageView then
    Result := TdxRibbonCustomBackstageView(Parent).Ribbon
  else
    Result := nil;
end;

{ TdxRibbonBackstageViewScrollBar }

function TdxRibbonBackstageViewScrollBar.GetRibbon: TdxCustomRibbon;
begin
  if Parent is TdxRibbonCustomBackstageView then
    Result := TdxRibbonCustomBackstageView(Parent).Ribbon
  else
    Result := nil;
end;

{ TdxRibbonBackstageViewFonts }

constructor TdxRibbonBackstageViewFonts.Create(ABackstageView: TdxRibbonCustomBackstageView);
var
  AFontIndex: TdxRibbonBackstageViewAssignedFont;
begin
  FBackstageView := ABackstageView;
  for AFontIndex := Low(TdxRibbonBackstageViewAssignedFont) to High(TdxRibbonBackstageViewAssignedFont) do
  begin
    FFonts[AFontIndex] := TFont.Create;
    FFonts[AFontIndex].OnChange := FontChanged;
  end;
end;

destructor TdxRibbonBackstageViewFonts.Destroy;
var
  AFontIndex: TdxRibbonBackstageViewAssignedFont;
begin
  for AFontIndex := Low(TdxRibbonBackstageViewAssignedFont) to High(TdxRibbonBackstageViewAssignedFont) do
    FreeAndNil(FFonts[AFontIndex]);
  inherited Destroy;
end;

procedure TdxRibbonBackstageViewFonts.Assign(ASource: TPersistent);
var
  AFontIndex: Integer;
begin
  if ASource is TdxRibbonBackstageViewFonts then
  begin
    for AFontIndex := Ord(Low(TdxRibbonBackstageViewAssignedFont)) to Ord(High(TdxRibbonBackstageViewAssignedFont)) do
      SetFont(AFontIndex, TdxRibbonBackstageViewFonts(ASource).GetFont(AFontIndex));
    AssignedFonts := TdxRibbonBackstageViewFonts(ASource).AssignedFonts;
  end;
end;

procedure TdxRibbonBackstageViewFonts.ChangeScale(M, D: Integer);
var
  AFontIndex: TdxRibbonBackstageViewAssignedFont;
begin
  for AFontIndex := Low(TdxRibbonBackstageViewAssignedFont) to High(TdxRibbonBackstageViewAssignedFont) do
  begin
    if AFontIndex in AssignedFonts then
      FFonts[AFontIndex].Height := MulDiv(FFonts[AFontIndex].Height, M, D);
  end;
  UpdateFonts;
end;

procedure TdxRibbonBackstageViewFonts.ResetFont(AFont: TFont; AIndex: TdxRibbonBackstageViewAssignedFont);
begin
  AFont.Assign(DefaultFont);
  if AIndex = bvafMainMenuTab then
  begin
    if BackstageView.Ribbon <> nil then
      BackstageView.Ribbon.ColorScheme.AdjustBackstageViewTabButtonFont(AFont);
  end;
end;

procedure TdxRibbonBackstageViewFonts.UpdateFonts;
var
  AFontIndex: TdxRibbonBackstageViewAssignedFont;
begin
  FLocked := True;
  try
    for AFontIndex := Low(TdxRibbonBackstageViewAssignedFont) to High(TdxRibbonBackstageViewAssignedFont) do
    begin
      if not (AFontIndex in AssignedFonts) then
        ResetFont(FFonts[AFontIndex], AFontIndex);
    end;
  finally
    FLocked := False;
  end;
end;

procedure TdxRibbonBackstageViewFonts.FontChanged(Sender: TObject);

  function GetFontIndex(out AIndex: TdxRibbonBackstageViewAssignedFont): Boolean;
  var
    I: TdxRibbonBackstageViewAssignedFont;
  begin
    Result := False;
    for I := Low(TdxRibbonBackstageViewAssignedFont) to High(TdxRibbonBackstageViewAssignedFont) do
      if Sender = FFonts[I] then
      begin
        Result := True;
        AIndex := I;
        Break;
      end;
  end;

var
  AFontIndex: TdxRibbonBackstageViewAssignedFont;
begin
  if not FLocked then
  begin
    if GetFontIndex(AFontIndex) then
      Include(FAssignedFonts, AFontIndex);
    BackstageView.FullRefresh;
  end;
end;

function TdxRibbonBackstageViewFonts.GetDefaultFont: TFont;
begin
  if BackstageView.IsBarManagerValid then
    Result := BackstageView.BarManager.Font
  else
    Result := BackstageView.Font;
end;

function TdxRibbonBackstageViewFonts.GetFont(AIndex: Integer): TFont;
begin
  Result := FFonts[TdxRibbonBackstageViewAssignedFont(AIndex)];
end;

function TdxRibbonBackstageViewFonts.IsFontStored(AIndex: Integer): Boolean;
begin
  Result := TdxRibbonBackstageViewAssignedFont(AIndex) in AssignedFonts;
end;

procedure TdxRibbonBackstageViewFonts.SetAssignedFonts(AValue: TdxRibbonBackstageViewAssignedFonts);
begin
  if AssignedFonts <> AValue then
  begin
    FAssignedFonts := AValue;
    UpdateFonts;
    FontChanged(nil);
  end;
end;

procedure TdxRibbonBackstageViewFonts.SetFont(AIndex: Integer; AValue: TFont);
begin
  FFonts[TdxRibbonBackstageViewAssignedFont(AIndex)].Assign(AValue);
end;

{ TdxRibbonBackstageViewDesignSelectorHelper }

function TdxRibbonBackstageViewDesignSelectorHelper.IsHitTestTransparent(const P: TPoint): Boolean;
begin
  Result := False;
end;

{ TdxRibbonCustomBackstageView }

constructor TdxRibbonCustomBackstageView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowMainMenu := True;
  FBarManagerHolder := TcxComponentHolder.Create;
  FBarManagerHolder.OnAfterComponentChange := DoAfterBarManagerChange;
  FBarManagerHolder.OnBeforeComponentChange := DoBeforeBarManagerChange;
  if IsDesigning then
    FDesignSelector := CreateDesignSelector;
  FMenuHelper := TdxRibbonBackstageViewMenuHelper.Create(Self);
  FFonts := TdxRibbonBackstageViewFonts.Create(Self);
  FPainter := CreatePainter;
  FNonClientController := CreateNonClientController;
  FNonClientViewInfo := CreateNonClientViewInfo;
  FViewInfo := CreateViewInfo;
  FTabs := TdxRibbonBackstageViewTabSheets.Create;
  FButtons := TdxRibbonBackstageViewMenuButtons.Create(Self);
  CheckAssignRibbon;
  LookAndFeel.NativeStyle := True;
  RegisterBackstageView(Self);
  SetBounds(Left, Top, 450, 300);
  ParentShowHint := False;
  ShowHint := False;
  Visible := IsDesigning;
end;

destructor TdxRibbonCustomBackstageView.Destroy;
begin
  UnhookApplicationWndProc;
  UnregisterBackstageView(Self);
  ActiveTab := nil;
  DeleteAllTabs;
  Ribbon := nil;
  FreeAndNil(FTabs);
  FreeAndNil(FFonts);
  FreeAndNil(FPainter);
  FreeAndNil(FButtons);
  FreeAndNil(FViewInfo);
  FreeAndNil(FMenuHelper);
  FreeAndNil(FDesignSelector);
  FreeAndNil(FNonClientController);
  FreeAndNil(FNonClientViewInfo);
  FreeAndNil(FBarManagerHolder);
  inherited Destroy;
end;

function TdxRibbonCustomBackstageView.AddTab: TdxRibbonBackstageViewTabSheet;
begin
  Result := TdxRibbonBackstageViewTabSheet.Create(Owner);
  Result.BackstageView := Self;
end;

procedure TdxRibbonCustomBackstageView.AfterBarManagerChange;
begin
  if Ribbon <> nil then
    BarManagerHolder.Component := Ribbon.BarManager
  else
    BarManagerHolder.Component := nil;
end;

function TdxRibbonCustomBackstageView.ApplicationWndProcHook(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Message.Msg = DXM_POSTAPPKEYDOWN) and (Message.WParam = VK_ESCAPE) then
  begin
    Result := Screen.ActiveForm = GetParentForm(Self);
    if Result then
    begin
      Message.Result := 1;
      ClosePopup;
    end;
  end;
end;

procedure TdxRibbonCustomBackstageView.BeforeBarManagerChange;
begin
  BarManagerHolder.Component := nil;
end;

function TdxRibbonCustomBackstageView.CanProcessMouseWheel: Boolean;
begin
  Result := Enabled and Visible;
end;

function TdxRibbonCustomBackstageView.CanShowPopup(ARibbon: TdxCustomRibbon): Boolean;
begin
  Result := Assigned(Ribbon) and (ARibbon = Ribbon);
end;

procedure TdxRibbonCustomBackstageView.ClosePopup;
begin
  DoClosePopup;
end;

procedure TdxRibbonCustomBackstageView.ColorSchemeChangeHandler(Sender: TObject; const AEventArgs);
begin
  FullRefresh;
end;

function TdxRibbonCustomBackstageView.CreateApplicationMenuHelper: TdxRibbonBackstageViewApplicationMenuHelper;
begin
  Result := TdxRibbonBackstageViewApplicationMenuHelper.Create(Self);
end;

function TdxRibbonCustomBackstageView.CreateNonClientController: TdxRibbonBackstageViewNonClientController;
begin
  Result := TdxRibbonBackstageViewNonClientController.Create(Self);
end;

function TdxRibbonCustomBackstageView.CreateNonClientViewInfo: TdxRibbonBackstageViewNonClientViewInfo;
begin
  Result := TdxRibbonBackstageViewNonClientViewInfo.Create(Self);
end;

function TdxRibbonCustomBackstageView.CreatePainter: TdxRibbonBackstageViewPainter;
begin
  Result := TdxRibbonBackstageViewPainter.Create(Self);
end;

function TdxRibbonCustomBackstageView.CreateViewInfo: TdxRibbonBackstageViewViewInfo;
begin
  Result := TdxRibbonBackstageViewViewInfo.Create(Self);
end;

function TdxRibbonCustomBackstageView.Popup(ARibbon: TdxCustomRibbon; var AClosedByEscape: Boolean): Boolean;

  function CanRestoreFocus(AFocus: HWND): Boolean;
  var
    AControl: TWinControl;
    AForm: TCustomForm;
  begin
    Result := IsChildEx(GetActiveWindow, AFocus);
    if Result then
    begin
      AControl := FindControl(AFocus);
      if AControl <> nil then
      begin
        AForm := GetParentForm(AControl);
        Result := (AForm <> nil) and AForm.Active;
      end;
    end;
  end;

var
  AForm: TCustomForm;
  AObjectLink: TcxObjectLink;
  ASavedBounds, R: TRect;
  ASavedConstraints: TSizeConstraints;
  ASavedFocus: HWND;
  ASavedParent: TWinControl;
begin
  Result := CanShowPopup(ARibbon);
  if Result then
  begin
    AObjectLink := cxAddObjectLink(Self);
    try
      ASavedConstraints := TSizeConstraints.Create(nil);
      try
        DoPopup;
        AForm := GetParentForm(Ribbon);
        ASavedParent := Parent;
        ASavedBounds := AForm.BoundsRect;
        ASavedConstraints.Assign(AForm.Constraints);
        ASavedFocus := GetFocus;
        try
          PrepareForPopup(AForm);
          R := AForm.BoundsRect;
          HookApplicationWndProc;
          BringToFront;
          Show;
          SetFocus;
          PopupMessageLoop(AForm);
        finally
          if AObjectLink.Ref <> nil then
          begin
            ViewInfo.DeactivateControls;
            Visible := False;
            UnhookApplicationWndProc;
            Parent := ASavedParent;
            AForm.Constraints.Assign(ASavedConstraints);
            if EqualRect(AForm.BoundsRect, R) then
              AForm.BoundsRect := ASavedBounds;
            DoCloseUp;
            if CanRestoreFocus(ASavedFocus) then
              Windows.SetFocus(ASavedFocus);
          end;
        end;
      finally
        ASavedConstraints.Free;
      end;
    finally
      cxRemoveObjectLink(AObjectLink);
    end;
  end;
end;

procedure TdxRibbonCustomBackstageView.PopupMessageLoop(AParentForm: TCustomForm);

  function IsModalFormClosing(AForm: TCustomForm): Boolean;
  begin
    Result := (fsModal in AForm.FormState) and (AForm.ModalResult <> mrNone);
  end;

begin
  while HandleAllocated and IsWindowVisible(Handle) do
  begin
    if Application.Terminated or IsModalFormClosing(AParentForm) then
      Break;
    Application.HandleMessage;
  end;
end;

procedure TdxRibbonCustomBackstageView.PrepareForPopup(AForm: TCustomForm);
begin
  Parent := AForm;
  MenuViewInfo.InitiateActions;
  ViewInfo.ScrollPositionX := 0;
  ViewInfo.ScrollPositionY := 0;
  AForm.Constraints.MinWidth := Max(AForm.Constraints.MinWidth, dxRibbonBackstageViewMinOwnerWidth);
  AForm.Constraints.MinHeight := Max(AForm.Constraints.MinHeight, dxRibbonBackstageViewMinOwnerHeight);
  CalculatePlace;
  NonClientViewInfo.Recalculate;
  dxRecalculateNonClientPart(Handle);
  UpdateWindowRegion;
  if ActiveTab <> nil then
    ActiveTab.Visible := True;
end;

function TdxRibbonCustomBackstageView.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if IsEqualGUID(IID, IdxRibbonApplicationMenu) then
  begin
    IdxRibbonApplicationMenu(Obj) := CreateApplicationMenuHelper;
    Result := S_OK;
  end
  else
    Result := inherited QueryInterface(IID, Obj);
end;

procedure TdxRibbonCustomBackstageView.SelectionChanged;
begin
  Invalidate;
end;

procedure TdxRibbonCustomBackstageView.ShowControl(AControl: TControl);
begin
  if AControl is TdxRibbonBackstageViewTabSheet then
    TdxRibbonBackstageViewTabSheet(AControl).Active := True;
  inherited ShowControl(AControl);
end;

function TdxRibbonCustomBackstageView.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxRibbonCustomBackstageView.BoundsChanged;
begin
  inherited BoundsChanged;
  Changed;
end;

procedure TdxRibbonCustomBackstageView.Calculate;
var
  I: Integer;
begin
  FIsRightToLeftConverted := False;
  ViewInfo.Calculate(ClientBounds);
  if not UseRightToLeftAlignment then
    for I := 0 to TabCount - 1 do
      Tabs[I].Calculate(ViewInfo.FrameAreaBounds);
  if DesignSelector <> nil then
    DesignSelector.SelectorBounds := ViewInfo.DesignSelectorRect;
  if UseRightToLeftAlignment then
    RightToLeftConversion(ClientBounds);
end;

procedure TdxRibbonCustomBackstageView.CalculatePlace;
var
  P: TPoint;
  R: TRect;
begin
  if Parent <> nil then
  begin
    R := Parent.ClientRect;
    if NonClientViewInfo.DisplayMode = amdmFrame then
    begin
      P := Ribbon.ViewInfo.TabsAreaViewInfo.TabsViewInfo.Bounds.BottomRight;
      P := dxMapWindowPoint(Ribbon.Handle, Parent.Handle, P);
      R.Top := P.Y;
    end
    else
      Inc(R.Top);

    BoundsRect := R;
  end;
end;

procedure TdxRibbonCustomBackstageView.CheckAssignRibbon;
begin
  if IsDesigning then
    Ribbon := FindRibbonForComponent(Self);
end;

procedure TdxRibbonCustomBackstageView.CheckZOrder;

  function GetZOrder(AControl: TControl): Integer;
  var
    I: Integer;
  begin
    for I := 0 to Parent.ControlCount - 1 do
    begin
      if Parent.Controls[I] = AControl then
        Exit(I);
    end;
    Result := -1;
  end;

var
  ARibbonZOrder: Integer;
begin
  // T259484
  if Parent <> nil then
  begin
    ARibbonZOrder := GetZOrder(Ribbon);
    if (ARibbonZOrder >= 0) and (ARibbonZOrder > GetZOrder(Self)) then
      TWinControlAccess(Parent).SetChildOrder(Self, ARibbonZOrder);
  end;
end;

function TdxRibbonCustomBackstageView.CanActivateTab(ATab: TdxRibbonBackstageViewTabSheet): Boolean;
begin
  Result := ((ATab = nil) or ATab.CanBeActive) and (IsDesigning or DoTabChanging(ATab));
end;

procedure TdxRibbonCustomBackstageView.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  BeginUpdate;
  try
    inherited;
    Fonts.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TdxRibbonCustomBackstageView.Changed(const AChanges: TdxRibbonBackstageViewChanges);
begin
  if FUpdateCount > 0 then
    FChanges := FChanges + AChanges
  else
    if not IsDestroying and HandleAllocated then
    begin
      if rbvcStruct in AChanges then
        MenuViewInfo.RecreateItemLinks
      else
      begin
        if rbvcItemsData in AChanges then
          MenuViewInfo.RefreshMenuButtonsInfo;
        if rbvcTabsData in AChanges then
          MenuViewInfo.RefreshTabsInfo;
      end;
      Calculate;
      NonClientViewInfo.Recalculate;
      ValidateActiveTab;
      UpdateScrollBars;
      FullInvalidate;
    end;
end;

procedure TdxRibbonCustomBackstageView.CreateWnd;
begin
  inherited CreateWnd;
  FullRefresh;
end;

procedure TdxRibbonCustomBackstageView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxRibbonCustomBackstageView.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    Changed(FChanges);
    FChanges := [];
  end;
end;

procedure TdxRibbonCustomBackstageView.DeleteAllTabs;
begin
  BeginUpdate;
  try
    while TabCount > 0 do
      Tabs[TabCount - 1].Free;
  finally
    EndUpdate;
  end;
end;

procedure TdxRibbonCustomBackstageView.DeleteTab(AIndex: Integer);
begin
  Tabs[AIndex].Free;
end;

procedure TdxRibbonCustomBackstageView.DoAddTab(ATab: TdxRibbonBackstageViewTabSheet);
begin
  FTabs.Add(ATab);
  if ActiveTab = nil then
    ValidateActiveTab;
  if ActiveTab <> nil then
    ActiveTab.BringToFront;
  Changed([rbvcStruct]);
end;

procedure TdxRibbonCustomBackstageView.DoAfterBarManagerChange(Sender: TObject);
begin
  if IsBarManagerValid then
    BarManager.SystemFontChangedHandlers.Add(SystemFontChanged);
  MenuViewInfo.CreateViewInfoItems;
  FullRefresh;
end;

procedure TdxRibbonCustomBackstageView.DoBeforeBarManagerChange(Sender: TObject);
begin
  if IsBarManagerValid then
    BarManager.SystemFontChangedHandlers.Remove(SystemFontChanged);
  MenuViewInfo.ClearInternalLists;
  MenuViewInfo.DestroyViewInfoItems;
end;

function TdxRibbonCustomBackstageView.DoCanClose: Boolean;
begin
  Result := True;
  if Assigned(OnCanClose) then
    OnCanClose(Self, Result);
end;

function TdxRibbonCustomBackstageView.DoClosePopup: Boolean;
begin
  Result := DoCanClose;
  if Result then
    Hide;
end;

procedure TdxRibbonCustomBackstageView.DoCloseUp;
begin
  CallNotify(OnCloseUp, Self);
end;

function TdxRibbonCustomBackstageView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
    Result := ViewInfo.ProcessMouseWheel(True);
end;

function TdxRibbonCustomBackstageView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
    Result := ViewInfo.ProcessMouseWheel(False);
end;

procedure TdxRibbonCustomBackstageView.DoPopup;
begin
  CallNotify(FOnPopup, Self);
end;

procedure TdxRibbonCustomBackstageView.DoRemoveTab(ATab: TdxRibbonBackstageViewTabSheet);

  function GetNewActiveTab: TdxRibbonBackstageViewTabSheet;
  begin
    if ActiveTab = ATab then
      Result := GetNextTab(ATab.PageIndex)
    else
      Result := ActiveTab;
  end;

var
  ANewActiveTab: TdxRibbonBackstageViewTabSheet;
begin
  ANewActiveTab := GetNewActiveTab;
  MenuViewInfo.TabButtonList.RemoveTab(ATab);
  FTabs.Extract(ATab);
  ActiveTab := ANewActiveTab;
  Changed([rbvcStruct]);
end;

procedure TdxRibbonCustomBackstageView.DoTabChanged;
begin
  CallNotify(OnTabChanged, Self);
end;

function TdxRibbonCustomBackstageView.DoTabChanging(ANewTab: TdxRibbonBackstageViewTabSheet): Boolean;
begin
  Result := True;
  if Assigned(OnTabChanging) then
    OnTabChanging(Self, ANewTab, Result);
end;

procedure TdxRibbonCustomBackstageView.DoTabClick(ATab: TdxRibbonBackstageViewTabSheet);
begin
  if Assigned(OnTabClick) then
    OnTabClick(Self, ATab);
end;

procedure TdxRibbonCustomBackstageView.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  ViewInfo.RightToLeftConversion(ABounds);
  for I := 0 to TabCount - 1 do
    Tabs[I].Calculate(ViewInfo.FrameAreaBounds);
end;

procedure TdxRibbonCustomBackstageView.DoTabVisibleChanged(ATab: TdxRibbonBackstageViewTabSheet);
begin
  BeginUpdate;
  try
    if (ActiveTab = ATab) and not ATab.TabVisible then
      ActiveTab := GetNextTab(ATab);
    Changed([rbvcStruct, rbvcTabsData]);
  finally
    EndUpdate;
  end;
end;

procedure TdxRibbonCustomBackstageView.FullInvalidate;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TdxRibbonCustomBackstageView.FullRefresh;
begin
  Fonts.UpdateFonts;
  MenuViewInfo.UpdateFont;
  if HandleAllocated then
    dxRecalculateNonClientPart(Handle);
  if ActiveTab <> nil then
    ActiveTab.RefreshNonClientArea;
  Changed([rbvcStruct, rbvcItemsData, rbvcTabsData]);
  if not IsDestroying and HandleAllocated then
    UpdateWindowRegion;
end;

procedure TdxRibbonCustomBackstageView.FocusChanged;
var
  AItemControl: TdxBarItemControl;
begin
  inherited FocusChanged;
  if Focused then
  begin
    AItemControl := ViewInfo.ActiveTabItemControl;
    if AItemControl <> nil then
      AItemControl.IAccessibilityHelper.Select(True);
  end;
end;

procedure TdxRibbonCustomBackstageView.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbHorizontal, 0, ViewInfo.ContentWidth, dxBackstageViewScrollLineSize,
    cxRectWidth(ViewInfo.Bounds) + 1, ViewInfo.ScrollPositionX, True, True);
  SetScrollBarInfo(sbVertical, 0, ViewInfo.ContentHeight, dxBackstageViewScrollLineSize,
    cxRectHeight(ViewInfo.Bounds) + 1, ViewInfo.ScrollPositionY, True, True);
end;

procedure TdxRibbonCustomBackstageView.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Buttons <> nil then
    Buttons.Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Ribbon) then
    Ribbon := nil;
end;

procedure TdxRibbonCustomBackstageView.MakeFullyVisible(R: TRect);

  function CalculateScrollPosition(ASourceBound1, ASourceBound2: Integer;
    ATargetBound1, ATargetBound2, APosition: Integer): Integer;
  begin
    if ATargetBound1 < ASourceBound1 then
      Result := APosition - ASourceBound1 + ATargetBound1
    else
      if ATargetBound2 > ASourceBound2 then
        Result := APosition - ASourceBound2 + ATargetBound2
      else
        Result := APosition;
  end;

begin
  ViewInfo.ScrollPositionX := CalculateScrollPosition(ViewInfo.Bounds.Left,
    ViewInfo.Bounds.Right, R.Left, R.Right, ViewInfo.ScrollPositionX);
  ViewInfo.ScrollPositionY := CalculateScrollPosition(ViewInfo.Bounds.Top,
    ViewInfo.Bounds.Bottom, R.Top, R.Bottom, ViewInfo.ScrollPositionY);
end;

procedure TdxRibbonCustomBackstageView.NCPaint(DC: HDC);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.ExcludeClipRect(NonClientViewInfo.ClientRect);
    NonClientViewInfo.Draw(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxRibbonCustomBackstageView.GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass;
begin
  if IsPopupScrollBars then
    Result := inherited GetScrollBarClass(AKind)
  else
    Result := TdxRibbonBackstageViewScrollBar;
end;

function TdxRibbonCustomBackstageView.GetSizeGripClass: TcxSizeGripClass;
begin
  if IsPopupScrollBars then
    Result := inherited GetSizeGripClass
  else
    Result := TdxRibbonBackstageViewSizeGrip;
end;

function TdxRibbonCustomBackstageView.NeedsToBringInternalControlsToFront: Boolean;
begin
  Result := True;
end;

function TdxRibbonCustomBackstageView.IsPersistentSelected(AObject: TPersistent): Boolean;
begin
  Result := Assigned(FDesignHelper) and FDesignHelper.IsObjectSelected(Self, AObject);
end;

procedure TdxRibbonCustomBackstageView.Paint;
begin
  Painter.DrawBackground(Canvas, cxRectOffset(cxGetWindowBounds(Self), NonClientViewInfo.ClientOffsets.TopLeft, False));
end;

procedure TdxRibbonCustomBackstageView.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  HideDesignSelector;
  case AScrollBarKind of
    sbHorizontal:
      begin
        ViewInfo.ScrollPositionX := AScrollPos;
        AScrollPos := ViewInfo.ScrollPositionX;
      end;

    sbVertical:
      begin
        ViewInfo.ScrollPositionY := AScrollPos;
        AScrollPos := ViewInfo.ScrollPositionY;
      end;
  end;

  if (AScrollCode = scEndScroll) and IsDesigning then
    ShowDesignSelector;
end;

procedure TdxRibbonCustomBackstageView.SetChildOrder(Child: TComponent; Order: Integer);
begin
  if Child is TdxRibbonBackstageViewTabSheet then
    TdxRibbonBackstageViewTabSheet(Child).PageIndex := Order
  else
    inherited;
end;

procedure TdxRibbonCustomBackstageView.SelectPersistent(AObject: TPersistent);
begin
  if Assigned(FDesignHelper) then
    FDesignHelper.SelectObject(Self, AObject);
end;

procedure TdxRibbonCustomBackstageView.SystemFontChanged(Sender: TObject; const AEventArgs);
begin
  FullRefresh;
end;

procedure TdxRibbonCustomBackstageView.HookApplicationWndProc;
begin
  if not FApplicationWndProcHooked then
  begin
    Application.HookMainWindow(ApplicationWndProcHook);
    FApplicationWndProcHooked := True;
  end;
end;

procedure TdxRibbonCustomBackstageView.UnhookApplicationWndProc;
begin
  if FApplicationWndProcHooked then
  begin
    Application.UnhookMainWindow(ApplicationWndProcHook);
    FApplicationWndProcHooked := False;
  end;
end;

function TdxRibbonCustomBackstageView.CreateDesignSelector: TdxRibbonBackstageViewDesignSelectorHelper;
begin
  Result := TdxRibbonBackstageViewDesignSelectorHelper.Create(Self);
end;

procedure TdxRibbonCustomBackstageView.InvalidateDesignSelectorArea;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, cxRectInflate(ViewInfo.DesignSelectorRect, 1), RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_NOERASE);
end;

procedure TdxRibbonCustomBackstageView.HideDesignSelector;
begin
  if DesignSelector <> nil then
  begin
    FreeAndNil(FDesignSelector);
    InvalidateDesignSelectorArea;
  end;
end;

procedure TdxRibbonCustomBackstageView.ShowDesignSelector;
begin
  if FDesignSelector = nil then
  begin
    FDesignSelector := CreateDesignSelector;
    FDesignSelector.SelectorBounds := ViewInfo.DesignSelectorRect;
    InvalidateDesignSelectorArea;
  end;
end;

procedure TdxRibbonCustomBackstageView.UpdateWindowRegion;
var
  R: TRect;
begin
  R := cxGetWindowRect(Handle);
  R := cxRectOffset(R, R.TopLeft, False);
  NonClientViewInfo.Calculate(R);
  SetWindowRgn(Handle, NonClientViewInfo.CreateWindowRegion, True);
end;

procedure TdxRibbonCustomBackstageView.RibbonAfterChange;
begin
  if Ribbon <> nil then
  begin
    Ribbon.AddListener(Self);
    Ribbon.ColorSchemeHandlers.Add(ColorSchemeChangeHandler);
    CheckZOrder;
  end;
  AfterBarManagerChange;
end;

procedure TdxRibbonCustomBackstageView.RibbonBeforeChange;
begin
  BeforeBarManagerChange;
  if Ribbon <> nil then
  begin
    Ribbon.RemoveListener(Self);
    if Ribbon.ColorSchemeHandlers <> nil then
      Ribbon.ColorSchemeHandlers.Remove(ColorSchemeChangeHandler);
  end;
end;

procedure TdxRibbonCustomBackstageView.ValidateActiveTab;
begin
  if (ActiveTab = nil) or not ActiveTab.CanBeActive then
    ActiveTab := GetNextTab(ActiveTab);
end;

procedure TdxRibbonCustomBackstageView.CMVisibleChanged(var Message: TMessage);
var
  AAnimationTransition: TdxRibbonBackstageViewAnimationTransition;
begin
  if IsInPopupLoop and (RibbonStyle >= rs2013) then
  begin
    LockWindowUpdate(Handle);
    try
      if Visible then
      begin
        inherited;
        AAnimationTransition := TdxRibbonBackstageViewAnimationTransition.Create(Self, bvatmShow);
        AAnimationTransition.ImmediateAnimation;
      end
      else
      begin
        BarNavigationController.SetKeyTipsShowingState(nil, '');
        AAnimationTransition := TdxRibbonBackstageViewAnimationTransition.Create(Self, bvatmHide);
        AAnimationTransition.ImmediateAnimation;
        inherited;
      end;
    finally
      LockWindowUpdate(0);
      cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME or RDW_UPDATENOW);
    end;
  end
  else
    inherited;
end;

procedure TdxRibbonCustomBackstageView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  with Message.CalcSize_Params^ do
    rgrc[0] := cxRectContent(rgrc[0], NonClientViewInfo.ClientOffsets);
end;

procedure TdxRibbonCustomBackstageView.WMNCPaint(var Message: TWMNCPaint);
var
  AMemBmp: HBITMAP;
  AMemDC: HDC;
  DC: HDC;
begin
  if NonClientViewInfo.HasNonClientArea then
  begin
    DC := GetWindowDC(Handle);
    try
      AMemDC := CreateCompatibleDC(DC);
      AMemBmp := CreateCompatibleBitmap(DC, Width, Height);
      try
        SelectObject(AMemDC, AMemBmp);
        NCPaint(AMemDC);
        cxPaintCanvas.BeginPaint(DC);
        try
          cxPaintCanvas.ExcludeClipRect(NonClientViewInfo.ClientRect);
          cxBitBlt(cxPaintCanvas.Handle, AMemDC, cxGetWindowBounds(Self), cxNullPoint, SRCCOPY);
        finally
          cxPaintCanvas.EndPaint;
        end;
      finally
        DeleteObject(AMemBmp);
        DeleteObject(AMemDC);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TdxRibbonCustomBackstageView.WMPrint(var Message: TWMPrint);
begin
  if PRF_NONCLIENT and Message.Flags <> 0 then
    NCPaint(Message.DC);
  inherited;
end;

procedure TdxRibbonCustomBackstageView.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateWindowRegion;
end;

procedure TdxRibbonCustomBackstageView.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if NonClientController <> nil then
    NonClientController.ProcessMessage(Message);
end;

function TdxRibbonCustomBackstageView.GetBarManager: TdxBarManager;
begin
  Result := TdxBarManager(BarManagerHolder.Component);
end;

procedure TdxRibbonCustomBackstageView.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to TabCount - 1 do
    Proc(Tabs[I]);
end;

function TdxRibbonCustomBackstageView.GetIsBarManagerValid: Boolean;
begin
  Result := (BarManager <> nil) and not (csDestroying in BarManager.ComponentState);
end;

function TdxRibbonCustomBackstageView.GetIsRibbonValid: Boolean;
begin
  Result := (Ribbon <> nil) and not Ribbon.IsDestroying;
end;

function TdxRibbonCustomBackstageView.GetMenuViewInfo: TdxRibbonBackstageViewMenuViewInfo;
begin
  Result := ViewInfo.MenuViewInfo;
end;

function TdxRibbonCustomBackstageView.GetRibbonStyle: TdxRibbonStyle;
begin
  if IsRibbonValid then
    Result := Ribbon.Style
  else
    Result := rs2007;
end;

function TdxRibbonCustomBackstageView.GetNextTab(ATab: TdxRibbonBackstageViewTabSheet): TdxRibbonBackstageViewTabSheet;
var
  AIndex: Integer;
begin
  if ATab = nil then
    AIndex := -1
  else
    AIndex := ATab.PageIndex;

  Result := GetNextTab(AIndex);
end;

function TdxRibbonCustomBackstageView.GetNextTab(AIndex: Integer): TdxRibbonBackstageViewTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := AIndex + 1 to TabCount - 1 do
    if Tabs[I].CanBeActive then
    begin
      Result := Tabs[I];
      Exit;
    end;

  for I := AIndex - 1 downto 0 do
    if Tabs[I].CanBeActive then
    begin
      Result := Tabs[I];
      Exit;
    end;
end;

function TdxRibbonCustomBackstageView.GetTabs(Index: Integer): TdxRibbonBackstageViewTabSheet;
begin
  Result := FTabs[Index];
end;

function TdxRibbonCustomBackstageView.GetTabCount: Integer;
begin
  Result := FTabs.Count;
end;

procedure TdxRibbonCustomBackstageView.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxRibbonCustomBackstageView.SetActiveTab(AValue: TdxRibbonBackstageViewTabSheet);
begin
  if (FActiveTab <> AValue) and CanActivateTab(AValue) then
  begin
    if ActiveTab <> nil then
      ActiveTab.Deactivate;
    FActiveTab := AValue;
    if ActiveTab <> nil then
      ActiveTab.Activate;
    if IsDesigning and not IsLoading then
      Modified;
    Changed([rbvcTabsData]);
    DoTabChanged;
  end;
end;

procedure TdxRibbonCustomBackstageView.SetButtons(AValue: TdxRibbonBackstageViewMenuButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TdxRibbonCustomBackstageView.SetFonts(AValue: TdxRibbonBackstageViewFonts);
begin
  FFonts.Assign(AValue);
end;

procedure TdxRibbonCustomBackstageView.SetRibbon(AValue: TdxCustomRibbon);
begin
  if AValue <> FRibbon then
  begin
    RibbonBeforeChange;
    FRibbon := AValue;
    RibbonAfterChange;
  end;
end;

procedure TdxRibbonCustomBackstageView.SetShowMainMenu(AValue: Boolean);
begin
  if FShowMainMenu <> AValue then
  begin
    FShowMainMenu := AValue;
    FullRefresh;
  end;
end;

initialization
  RegisterClass(TdxRibbonBackstageViewTabSheet);
end.

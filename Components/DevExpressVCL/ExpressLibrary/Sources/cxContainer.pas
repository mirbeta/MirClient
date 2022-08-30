{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCommonLibrary                                     }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL          }
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

unit cxContainer;

{$I cxVer.inc}
{$DEFINE USETCXSCROLLBAR}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, Controls, Forms, Graphics, Menus, StdCtrls, SysUtils, Math,
  dxCore, dxCoreClasses, dxMessages, dxThemeManager, dxUxTheme, cxClasses, cxControls,
  dxTouch, cxGraphics, cxLookAndFeels, cxScrollBar, cxLookAndFeelPainters, cxGeometry, dxTypeHelpers;

const
  cxDefaultAutoCompleteDelay = 500;
  cxContainerShadowWidth = 3;

  cxEmptyBrush: TBrushHandle = 0;

  cxContainerDefaultBorderExtent: TRect = (
    Left: cxContainerMaxBorderWidth;
    Top: cxContainerMaxBorderWidth;
    Right: cxContainerMaxBorderWidth;
    Bottom: cxContainerMaxBorderWidth
  );

type
  TcxContainerHotState = (chsNoHotTrack, chsNormal, chsSelected);
  TcxContainerStateItem = (csNormal, csActive, csDisabled, csHotTrack);
  TcxContainerState = set of TcxContainerStateItem;
  TcxMouseButton = (cxmbNone, cxmbLeft, cxmbRight, cxmbMiddle);

  TcxContainerStyleValue = 0..SizeOf(Integer) * 8 - 1;
  TcxContainerStyleValues = set of TcxContainerStyleValue;

const
  csvBorderColor       = 0;
  csvBorderStyle       = 1;
  csvColor             = 2;
  csvEdges             = 3;
  csvFont              = 4;
  csvHotTrack          = 5;
  csvShadow            = 6;
  csvTextColor         = 7;
  csvTextStyle         = 8;
  csvTransparentBorder = 9;

  cxContainerStyleValueCount = 10;

  cxContainerStyleValueNameA: array[0..cxContainerStyleValueCount - 1] of string = (
    'BorderColor',
    'BorderStyle',
    'Color',
    'Edges',
    'Font',
    'HotTrack',
    'Shadow',
    'TextColor',
    'TextStyle',
    'TransparentBorder'
  );

type
  TcxContainer = class;
  TcxContainerClass = class of TcxContainer;
  TcxContainerStyle = class;

  { TcxContainerViewInfo }

  TcxContainerViewInfo = class
  private
    FBackgroundColor: TColor;
  protected
    FCalculated: Boolean;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); virtual;
    function GetContainerBorderStyle: TcxContainerBorderStyle; virtual;
    procedure InternalPaint(ACanvas: TcxCanvas); virtual;
    procedure SetBackgroundColor(Value: TColor); virtual;
  public
    BorderColor: TColor;
    BorderRect: TRect;
    BorderStyle: TcxContainerBorderStyle;
    BorderWidth: Integer;
    Bounds: TRect;
    ClientRect: TRect;
    ContainerState: TcxContainerState;
    Edges: TcxBorders;
    Font: TFont;
    HotState: TcxContainerHotState;
    NativePart: Integer;
    NativeState: Integer;
    NativeStyle: Boolean;
    Owner: TObject;
    Painter: TcxCustomLookAndFeelPainter;
    Shadow: Boolean;
    ThemedObjectType: TdxThemedObjectType;
    UseSkins: Boolean;

    constructor Create; virtual;
    procedure Assign(Source: TObject); virtual;
    function GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion; virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure UpdateStyle(AStyle: TcxContainerStyle); virtual;
    //
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Calculated: Boolean read FCalculated;
  end;

  TcxContainerViewInfoClass = class of TcxContainerViewInfo;

  { TcxContainerStyle }

  TcxStyleController = class;

  TcxContainerStyleData = record
    Color: TColor;
    Font: TFont;
    FontColor: TColor;
  end;

  TcxContainerStyle = class(TcxInterfacedPersistent, IdxSkinSupport)
  private
    FBorderColor: TColor;
    FBorderStyle: TcxContainerBorderStyle;
    FEdges: TcxBorders;
    FHotTrack: Boolean;
    FShadow: Boolean;
    FTransparentBorder: Boolean;

    FDirectAccessMode: Boolean;
    FFontAssignedValueLockCount: Integer;
    FIsDestroying: Boolean;
    FLookAndFeel: TcxLookAndFeel;
    FModified: Boolean;
    FOwner: TPersistent;
    FParentStyle: TcxContainerStyle;
    FState: TcxContainerStateItem;
    FTextStyle: TFontStyles;
    FUpdateCount: Integer;
    FVisibleFont: TFont;
    FOnChanged: TNotifyEvent;

    function GetAssignedValues: TcxContainerStyleValues;
    function GetBorderColor: TColor;
    function GetBorderStyle: TcxContainerBorderStyle;
    function GetEdges: TcxBorders;
    function GetFont: TFont;
    function GetHotTrack: Boolean;
    function GetShadow: Boolean;
    function GetTransparentBorder: Boolean;

    function InternalGetBorderColor(var BorderColor: TColor): Boolean;
    function InternalGetBorderStyle(var BorderStyle: TcxContainerBorderStyle): Boolean;
    function InternalGetEdges(var Edges: TcxBorders): Boolean;
    function InternalGetFont(var Font: TFont): Boolean;
    function InternalGetHotTrack(var HotTrack: Boolean): Boolean;
    function InternalGetShadow(var Shadow: Boolean): Boolean;
    function InternalGetTextColor(var TextColor: TColor): Boolean;
    function InternalGetTextStyle(var TextStyle: TFontStyles): Boolean;
    function InternalGetTransparentBorder(var TransparentBorder: Boolean): Boolean;

    function IsBorderColorStored: Boolean;
    function IsBorderStyleStored: Boolean;
    function IsColorStored: Boolean;
    function IsEdgesStored: Boolean;
    function IsFontStored: Boolean;
    function IsHotTrackStored: Boolean;
    function IsShadowStored: Boolean;
    function IsStyleControllerStored: Boolean;
    function IsTextColorStored: Boolean;
    function IsTextStyleStored: Boolean;
    function IsTransparentBorderStored: Boolean;

    procedure SetAssignedValues(Value: TcxContainerStyleValues);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderStyle(Value: TcxContainerBorderStyle);
    procedure SetColor(Value: TColor);
    procedure SetEdges(Value: TcxBorders);
    procedure SetFont(Value: TFont);
    procedure SetHotTrack(Value: Boolean);
    procedure SetShadow(Value: Boolean);
    procedure SetTextColor(Value: TColor);
    procedure SetTextStyle(Value: TFontStyles);
    procedure SetTransparentBorder(Value: Boolean);

    procedure CheckChanges;
    procedure CreateFont;
    function GetActiveStyleController: TcxStyleController;
    function GetBaseStyle: TcxContainerStyle;
    function GetContainer: TcxContainer;
    function GetLookAndFeel: TcxLookAndFeel;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure ReadIsFontAssigned(Reader: TReader);
    procedure RestoreFont(AFont: TFont);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure UpdateVisibleFont;
    procedure WriteIsFontAssigned(Writer: TWriter);
  protected
    FAssignedValues: TcxContainerStyleValues;
    FStyleController: TcxStyleController;
    procedure DefineProperties(Filer: TFiler); override;
    function BaseGetStyleController: TcxStyleController;
    procedure BaseSetStyleController(Value: TcxStyleController);
    procedure Changed; virtual;
    procedure ControllerChangedNotification(AStyleController: TcxStyleController); virtual;
    procedure ControllerFreeNotification(AStyleController: TcxStyleController); virtual;

    function DefaultBorderColor: TColor; virtual;
    function DefaultBorderStyle: TcxContainerBorderStyle; virtual;
    function DefaultColor: TColor; virtual;
    function DefaultDisabledTextColor: TColor; virtual;
    function DefaultEdges: TcxBorders; virtual;
    function DefaultHotTrack: Boolean; virtual;
    function DefaultShadow: Boolean; virtual;
    function DefaultTextColor: TColor; virtual;
    function DefaultTextStyle: TFontStyles; virtual;
    function DefaultTransparentBorder: Boolean; virtual;

    procedure FontChanged(Sender: TObject); virtual;
    function GetColor: TColor; virtual;
    function GetDefaultStyleController: TcxStyleController; virtual;
    function GetStyleColor: TColor; virtual;
    function GetTextColor: TColor; virtual;
    function GetTextStyle: TFontStyles; virtual;
    function InternalGetColor(var Color: TColor): Boolean;
    function InternalGetNotPublishedExtendedStyleValues: TcxContainerStyleValues; virtual;
    function IsBaseStyle: Boolean;
    function IsDestroying: Boolean;
    function IsFontAssignedValueLocked: Boolean;
    procedure LockFontAssignedValue(ALock: Boolean);
    procedure UpdateFont;

    property ParentStyle: TcxContainerStyle read FParentStyle;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    StyleData: TcxContainerStyleData;

    constructor Create(AOwner: TPersistent; ADirectAccessMode: Boolean;
      AParentStyle: TcxContainerStyle = nil; AState: TcxContainerStateItem = csNormal); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetStyleValue(const APropertyName: string; out StyleValue: TcxContainerStyleValue): Boolean;
    function GetStyleValueCount: Integer; virtual;
    function GetStyleValueName(AStyleValue: TcxContainerStyleValue; out StyleValueName: string): Boolean; virtual;
    function GetVisibleFont: TFont;
    function HasBorder: Boolean; virtual;
    function IsExtendedStylePropertyPublished(const APropertyName: string): Boolean;
    function IsValueAssigned(AValue: TcxContainerStyleValue): Boolean; virtual;
    procedure RestoreDefaults; virtual;

    property ActiveStyleController: TcxStyleController read GetActiveStyleController;
    property BaseStyle: TcxContainerStyle read GetBaseStyle;
    property Container: TcxContainer read GetContainer;
    property DirectAccessMode: Boolean read FDirectAccessMode;
    property State: TcxContainerStateItem read FState;
  published
    property AssignedValues: TcxContainerStyleValues read GetAssignedValues write SetAssignedValues stored False;
    property BorderColor: TColor read GetBorderColor write SetBorderColor stored IsBorderColorStored;
    property BorderStyle: TcxContainerBorderStyle read GetBorderStyle write SetBorderStyle stored IsBorderStyleStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Edges: TcxBorders read GetEdges write SetEdges stored IsEdgesStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack stored IsHotTrackStored;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property Shadow: Boolean read GetShadow write SetShadow stored IsShadowStored;
    property StyleController: TcxStyleController read BaseGetStyleController write BaseSetStyleController stored IsStyleControllerStored;
    property TextColor: TColor read GetTextColor write SetTextColor stored IsTextColorStored;
    property TextStyle: TFontStyles read GetTextStyle write SetTextStyle stored IsTextStyleStored;
    property TransparentBorder: Boolean read GetTransparentBorder write SetTransparentBorder stored IsTransparentBorderStored;
  end;

  TcxCustomContainerStyle = TcxContainerStyle; // TODO Remove

  TcxContainerStyleClass = class of TcxContainerStyle;

  { TcxContainerStyles }

  TcxContainerStyles = class
  private
    FStyles: array[TcxContainerStateItem] of TcxContainerStyle;
    function GetStyle(AState: TcxContainerStateItem): TcxContainerStyle;
    function GetStyleDisabled: TcxContainerStyle;
    function GetStyleFocused: TcxContainerStyle;
    function GetStyleHot: TcxContainerStyle;
    function GetStyleNormal: TcxContainerStyle;
    procedure SetOnChanged(Value: TNotifyEvent);
    procedure SetStyle(AState: TcxContainerStateItem; Value: TcxContainerStyle);
    procedure SetStyleDisabled(Value: TcxContainerStyle);
    procedure SetStyleFocused(Value: TcxContainerStyle);
    procedure SetStyleHot(Value: TcxContainerStyle);
    procedure SetStyleNormal(Value: TcxContainerStyle);
  public
    constructor Create(AOwner: TPersistent; AStyleClass: TcxContainerStyleClass); virtual;
    destructor Destroy; override;
    procedure RestoreDefaults;
    property Style: TcxContainerStyle read GetStyleNormal write SetStyleNormal;
    property StyleDisabled: TcxContainerStyle read GetStyleDisabled write SetStyleDisabled;
    property StyleFocused: TcxContainerStyle read GetStyleFocused write SetStyleFocused;
    property StyleHot: TcxContainerStyle read GetStyleHot write SetStyleHot;
    property Styles[AState: TcxContainerStateItem]: TcxContainerStyle read GetStyle write SetStyle; default;
    property OnChanged: TNotifyEvent write SetOnChanged;
  end;

  TcxContainerStylesClass = class of TcxContainerStyles;

  { TcxStyleController }

  TcxStyleController = class(TcxScalableComponent)
  private
    FIsDestruction: Boolean;
    FListeners: TList;

    FOnStyleChanged: TNotifyEvent;

    function GetFakeStyleController: TcxStyleController;
    function GetStyle: TcxContainerStyle;
    function GetInternalStyle(AState: TcxContainerStateItem): TcxContainerStyle;
    procedure SetFakeStyleController(Value: TcxStyleController);
    procedure SetInternalStyle(AState: TcxContainerStateItem; Value: TcxContainerStyle);
    procedure StyleChanged(Sender: TObject);
  protected
    FStyles: TcxContainerStyles;

    procedure ChangeScale(M, D: Integer); override;
    procedure Loaded; override;

    procedure AddListener(AListener: TcxContainerStyle); virtual;
    procedure Changed;
    function GetStyleClass: TcxContainerStyleClass; virtual;
    function GetStylesClass: TcxContainerStylesClass; virtual;
    function IsDestruction: Boolean;
    procedure RemoveListener(AListener: TcxContainerStyle); virtual;
    property Style: TcxContainerStyle read GetStyle;
    property Listeners: TList read FListeners;
    property OnStyleChanged: TNotifyEvent read FOnStyleChanged write FOnStyleChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RestoreStyles;
    property Styles[AState: TcxContainerStateItem]: TcxContainerStyle read GetInternalStyle write SetInternalStyle;
  published
    property Scalable;
    property FakeStyleController: TcxStyleController read GetFakeStyleController write SetFakeStyleController stored False;
  end;

  { IcxContainerInnerControl }

  IcxContainerInnerControl = interface
  ['{1B111318-D9C9-4C35-9EFF-5D95793C0106}']
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;
    property Control: TWinControl read GetControl;
    property ControlContainer: TcxContainer read GetControlContainer;
  end;

  TcxScrollBarInfo = TScrollBarInfo;

  { TcxContainer }

  TcxContainerActiveStyleData = record
    ContainerState: TcxContainerState;
    ActiveStyle: TcxContainerStyle;
  end;

  TcxContainerInnerControlBounds = record
    IsEmpty: Boolean;
    Rect: TRect;
  end;

  TcxContainerSizeGripData = record
    Bounds: TRect;
    Visible: Boolean;
  end;

  TcxContainer = class(TcxControl, IUnknown, IcxCompoundControl, IcxMouseTrackingCaller)
  private
  {$IFNDEF VCLGLASSPAINT}
    FInnerControlBufferedPaint: Boolean;
    FOnGlass: Boolean;
    FRepaintOnGlass: Boolean;
  {$ENDIF}
    FActiveStyleData: TcxContainerActiveStyleData;
    FHasChanges: Boolean;
    FInnerControl: TWinControl;
    FInnerControlBounds: TcxContainerInnerControlBounds;
    FInnerControlMouseDown: Boolean;
    FIsViewInfoCalculated: Boolean;
    FLockAlignControlsCount: Integer;
    FPopupMenuLockCount: Integer;
    FProcessEventsLockCount: Integer;
    FRefreshLockCount: Integer;
    FScrollBarsCalculating: Boolean;
    FSizeGripData: TcxContainerSizeGripData;

    function GetFakeStyleController: TcxStyleController;
    function GetInternalStyle(AState: TcxContainerStateItem): TcxContainerStyle;
    function GetIsDestroying: Boolean;
    procedure GetStandardScrollBarParameters(AKind: TScrollBarKind; out AScrollInfo: TScrollInfo);
    function GetStyle: TcxContainerStyle;
    function GetStyleDisabled: TcxContainerStyle;
    function GetStyleFocused: TcxContainerStyle;
    function GetStyleHot: TcxContainerStyle;
    function GetVisibleFont: TFont;
    procedure SetFakeStyleController(Value: TcxStyleController);
    procedure SetInnerControl(Value: TWinControl);
    procedure SetParentFont(Value: Boolean);
    procedure SetStyle(Value: TcxContainerStyle);
    procedure SetStyleDisabled(Value: TcxContainerStyle);
    procedure SetStyleFocused(Value: TcxContainerStyle);
    procedure SetStyleHot(Value: TcxContainerStyle);
    procedure SetInternalStyle(AState: TcxContainerStateItem; Value: TcxContainerStyle);
    function GetDragKind: TDragKind;

    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;

    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMShortRefreshContainer(var Message: TMessage); message DXM_SHORTREFRESHCONTAINER;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMUpdateScrollBars(var Message: TMessage); message DXM_UPDATESCROLLBARS;

    procedure DXMUpdateWindowRegion(var Message: TMessage); message DXM_UPDATEWINDOWREGION;
  protected
    FInternalSetting: Integer;
    FIsCreating: Boolean;
    FStyles: TcxContainerStyles;
    FViewInfo: TcxContainerViewInfo;

    // WindowRegion
    FUpdateWindowRegionCount: Integer;
    FInternalWindowRegionSetting: Boolean;
    FNewWindowRegion: TcxRegionHandle;

    // IcxLookAndFeelContainer
    function GetLookAndFeelValue: TcxLookAndFeel; override;

    // IcxCompoundControl
    function GetActiveControl: TWinControl;

    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingCallerMouseLeave;
    procedure MouseTrackingCallerMouseLeave; virtual;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function CanFocusOnClick: Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure ColorChanged; override;
    procedure CorrectAlignControlRect(var R: TRect); virtual;
    procedure CursorChanged; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DragCanceled; override;
    procedure FocusChanged; override;
    function FocusWhenChildIsClicked(AChild: TControl): Boolean; override;
    function GetClientBounds: TRect; override;
    function IsGestureTarget(AWnd: THandle): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    function MayFocus: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure SetParent(AParent: TWinControl); override;

    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWnd; override;
    procedure UpdateScrollBars; override;
    function UseSystemRightToLeftLayoutForInnerControl: Boolean; virtual;
    function UseInnerControlScrollBarParameters: Boolean; virtual;
    procedure WndProc(var Message: TMessage); override;

    procedure AdjustInnerControl; virtual;
    procedure AdjustScrollBarPosition(AScrollBar: IcxControlScrollBar); virtual;
    procedure AdjustScrollBarPositions; virtual;
    procedure AdjustVisibleFontHeight(AVisibleFont: TFont); virtual;
    procedure CalculateViewInfo(const P: TPoint; AMouseTracking: Boolean); virtual;
    function CanContainerHandleTabs: Boolean; virtual;
    function CanHaveTransparentBorder: Boolean; virtual;
    function CanRefreshContainer: Boolean; virtual;
    function CanShowPopupMenu(const P: TPoint): Boolean; virtual;
    procedure CheckIsViewInfoCalculated;
    procedure ContainerStyleChanged(Sender: TObject); virtual;
    function CreateWindowRegion: TcxRegionHandle; virtual;
    procedure DataChange; virtual;
    procedure DataSetChange; virtual;
    function DefaultParentColor: Boolean; virtual;
    function DoInnerControlDefaultHandler(var Message: TMessage): Boolean; virtual;
    procedure DoProcessEventsOnViewInfoChanging; virtual;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton;
      Shift: TShiftState; AIsMouseEvent: Boolean): Boolean; virtual;
    procedure EnabledChanged; override;
    procedure EndMouseTracking; virtual;
    function GetActiveStyle: TcxContainerStyle; virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetBorderColor: TColor; virtual;
    function GetBorderExtent: TRect; virtual;
    function GetEditStateColorKind: TcxEditStateColorKind; virtual;
    function GetInnerControlBounds(const AInnerControlsRegion: TRect;
      AInnerControl: TControl): TcxContainerInnerControlBounds; virtual;
    function GetScrollBarHandle(AKind: TScrollBarKind): THandle;
    function GetShadowBounds: TRect; virtual;
    function GetShadowBoundsExtent: TRect; virtual;
    function GetStyleClass: TcxContainerStyleClass; virtual;
    function GetStylesClass: TcxContainerStylesClass; virtual;
    function GetViewInfoClass: TcxContainerViewInfoClass; virtual;
    function GetWindowRegionAddon: TRect; virtual;
    function HasShadow: Boolean; virtual;
    function InternalGetActiveStyle: TcxContainerStyle; virtual;
    function InternalGetNotPublishedStyleValues: TcxContainerStyleValues; virtual;

    function GetBorderColorByPainter(AIsHighlight: Boolean): TColor; virtual;
    procedure GetColorSettingsByPainter(out ABackgroundColor, ATextColor: TColor); virtual;
    function GetDefaultBackgroundColorByPainter: TColor; virtual;
    function GetDefaultTextColorByPainter: TColor; virtual;

    function CanDisableAlignOnCreateInnerControl: Boolean; virtual;
    function HandleInnerContolGestures: Boolean; virtual;
    function IsAlignControlsLocked: Boolean;
    function IsContainerFocused: Boolean; virtual;
    function IsInnerControlBoundsChanged(AControl: TWinControl; const ABounds: TcxContainerInnerControlBounds): Boolean;
    function IsContainerClass: Boolean; virtual;
    function IsMouseTracking: Boolean; virtual;
    function IsNativeStyle: Boolean; virtual;
    function IsPopupMenuLocked: Boolean;
    function IsReadOnly: Boolean; virtual;
    function IsStyleAssigned(AValue: TcxContainerStyleValue): Boolean;

    procedure BeginRefreshContainer;
    function EndRefreshContainer(AIsMouseEvent: Boolean = False): Boolean;
    function IsRefreshContainerLocked: Boolean;
    function RefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;

    procedure SaveInnerControlBounds(AControl: TWinControl; const ABounds: TcxContainerInnerControlBounds);
    procedure DoSetSize; virtual;
    procedure SetSize;
    procedure SetVisibleBoundsClipRect; virtual;
    procedure UpdateData; virtual;
    procedure UpdateWindowRegion; virtual;
    function GetBackgroundThemedObjectType: TdxThemedObjectType; virtual;
    function GetBackgroundNativePart: Integer; virtual;
    function GetBackgroundNativeState: Integer; virtual;
    function GetScrollContentForegroundColor: TColor; override;
    function GetScrollBarBounds(const AScrollBarRect: TRect): TRect; virtual;
    function GetScrollBarEnabled(AScrollBar: IcxControlScrollBar; const AScrollBarinfo: TcxScrollBarInfo): Boolean; virtual;
    function GetScrollBarInfo(var AScrollBarInfo: TcxScrollBarInfo; const AKind: TScrollBarKind): Boolean; virtual;
    function IsScrollBarVisible(AKind: TScrollBarKind; out AScrollbarInfo: TScrollBarInfo): Boolean;
    procedure ProcessEventsOnViewInfoChanging;
    procedure SafeSelectionFocusInnerControl; virtual;
    procedure ScaleFactorChanged; override;
    procedure SetDragKind(Value: TDragKind); virtual;
    procedure SetScrollBarVisible(AScrollBar: IcxControlScrollBar; AVisible: Boolean); virtual;

    property ActiveStyle: TcxContainerStyle read GetActiveStyle;
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    property HasChanges: Boolean read FHasChanges write FHasChanges;
    property IsViewInfoCalculated: Boolean read FIsViewInfoCalculated write FIsViewInfoCalculated;
    property ScrollBarsCalculating: Boolean read FScrollBarsCalculating;
    property Style: TcxContainerStyle read GetStyle write SetStyle;
    property StyleDisabled: TcxContainerStyle read GetStyleDisabled write SetStyleDisabled;
    property StyleFocused: TcxContainerStyle read GetStyleFocused write SetStyleFocused;
    property StyleHot: TcxContainerStyle read GetStyleHot write SetStyleHot;
    property ViewInfo: TcxContainerViewInfo read FViewInfo;
    property VisibleFont: TFont read GetVisibleFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Focused: Boolean; override;
    function GetDragImages: TDragImageList; override;
    procedure GetTabOrderList(List: TList); override;
    procedure SetFocus; override;
    procedure Invalidate; override;
    procedure Update; override;

    procedure ClearSavedChildControlRegions; virtual;
    function GetVisibleBounds: TRect; virtual;
    function HasInnerControl: Boolean; inline;
    function HasInnerControlHandle: Boolean; inline;
    function HasPopupWindow: Boolean; virtual;
    function InnerControlDefaultHandler(var Message: TMessage): Boolean;
    function InnerControlMenuHandler(var Message: TMessage): Boolean; virtual;
    function IsInplace: Boolean; virtual;
    function IsStylePropertyPublished(const APropertyName: string; AExtendedStyle: Boolean): Boolean;
    procedure LockAlignControls(ALock: Boolean);
    procedure LockPopupMenu(ALock: Boolean);
    procedure RestoreStyles;
    procedure SetScrollBarsParameters(AIsScrolling: Boolean = False); virtual;
    function ShortRefreshContainer(AIsMouseEvent: Boolean): Boolean;
    procedure UpdateScrollBarsParameters;

    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property ActiveControl: TWinControl read GetActiveControl;
    property InnerControl: TWinControl read FInnerControl write SetInnerControl;
    property InnerControlMouseDown: Boolean read FInnerControlMouseDown write FInnerControlMouseDown;
    property IsDestroying: Boolean read GetIsDestroying;
  {$IFNDEF VCLGLASSPAINT}
    property OnGlass: Boolean read FOnGlass write FOnGlass;
  {$ENDIF}
    property ParentColor;
    property ParentFont write SetParentFont;
    property Styles[AState: TcxContainerStateItem]: TcxContainerStyle read GetInternalStyle write SetInternalStyle;
    property VisibleBounds: TRect read GetVisibleBounds;
  published
    property FakeStyleController: TcxStyleController read GetFakeStyleController
      write SetFakeStyleController stored False;
    property TabStop default True;
  end;

  TcxPrevPopupControlData = record
    Align: TAlign;
    Bounds: TRect;
    Parent: TWinControl;
    Visible: Boolean;
    BorderStyle: TFormBorderStyle;
    ActiveControl: TWinControl;
  end;

  { TcxCustomPopupWindow }

  TcxCustomPopupWindow = class(TcxPopupWindow, IdxPopupControl)
  private
    FCaptureFocus: Boolean;
    FDeactivateLockCount: Integer;
    FDeactivation: Boolean;
    FFocusedControl: TWinControl;
    FIsPopup: Boolean;
    FIsTopMost: Boolean;
    FJustClosed: Boolean;
    FModalMode: Boolean;
    FTerminateOnDestroy: Boolean;
    FOwnerControl: TWinControl;

    FOnBeforeClosing: TNotifyEvent;
    FOnClosed: TNotifyEvent;
    FOnClosing: TNotifyEvent;
    FOnShowed: TNotifyEvent;
    FOnShowing: TNotifyEvent;

    function GetJustClosed: Boolean;
    procedure SetCaptureFocus(Value: Boolean);
    procedure SetIsTopMost(Value: Boolean);
    procedure CMClosePopupWindow(var Message: TMessage); message DXM_CLOSEPOPUPWINDOW;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMShowPopupWindow(var Message: TMessage); message DXM_SHOWPOPUPWINDOW;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  protected
    FStyle: TcxContainerStyle;
    FViewInfo: TcxContainerViewInfo;
    FPrevActiveForm: TCustomForm;
    FPrevActiveControl: TWinControl;
    FShowWithoutActivation: Boolean;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Deactivate; override;
    procedure InitPopup; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure VisibleChanged; override;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function AcceptsAnySize: Boolean; virtual;
    procedure DoBeforeClosing; virtual;
    procedure DoClosed; virtual;
    procedure DoClosing; virtual;
    procedure DoShowed; virtual;
    procedure DoShowing; virtual;
    function GetFirstFocusControl(AControl: TWinControl): TWinControl;

    // IdxPopupControl
    function GetOwnerControl: TWinControl;

    function HasBackground: Boolean; virtual;
    procedure InternalEnableWindow(AEnable: Boolean);
    function IsDeactivateLocked: Boolean;
    function IsOwnerControlVisible: Boolean;
    function IsSysKeyAccepted(Key: Word): Boolean; virtual;
    procedure ModalCloseUp; virtual;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    function NeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle; AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean; virtual;
    procedure PopupWindowStyleChanged(Sender: TObject); virtual;
    procedure RecreateWindow;
    procedure UpdateScaleFactor;
    function UseOwnerParentToGetScreenBounds: Boolean; override;

    property Style: TcxContainerStyle read FStyle;
    property ViewInfo: TcxContainerViewInfo read FViewInfo;
  public
    constructor Create(AOwnerControl: TWinControl); reintroduce; virtual;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    function Focused: Boolean; override;

    procedure ClosePopup;
    procedure CloseUp; override;

    procedure CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize); virtual;
    function GetStyleClass: TcxContainerStyleClass; virtual;
    function GetViewInfoClass: TcxContainerViewInfoClass; virtual;
    function HasCapture: Boolean;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    function IsVisible: Boolean;

    procedure LockDeactivate(ALock: Boolean);
    procedure Popup(AFocusedControl: TWinControl); reintroduce; virtual;
    function SetFocusedControl(Control: TWinControl): Boolean; override;

    property CaptureFocus: Boolean read FCaptureFocus write SetCaptureFocus default True;
    property FocusedControl: TWinControl read FFocusedControl write FFocusedControl;
    property IsTopMost: Boolean read FIsTopMost write SetIsTopMost;
    property JustClosed: Boolean read GetJustClosed;
    property ModalMode: Boolean read FModalMode write FModalMode default True;
    property OwnerControl: TWinControl read GetOwnerControl;
    property TerminateOnDestroy: Boolean read FTerminateOnDestroy write FTerminateOnDestroy;
    property OnBeforeClosing: TNotifyEvent read FOnBeforeClosing write FOnBeforeClosing;
    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnClosing: TNotifyEvent read FOnClosing write FOnClosing;
    property OnCloseQuery;
    property OnShowed: TNotifyEvent read FOnShowed write FOnShowed;
    property OnShowing: TNotifyEvent read FOnShowing write FOnShowing;
  end;

  { TcxCustomInnerListBox }

  TcxCustomInnerListBox = class(TListBox, IcxContainerInnerControl)
  private
    FAutoCompleteFilter: string;
    FCanvas: TcxCanvas;
    FHScrollBar: TcxScrollBar;
    FIsRedrawLocked: Boolean;
    FLookAndFeel: TcxLookAndFeel;
    FPrevBrushColor: TColor;
    FPrevFontColor: TColor;
    FPrevKeyPressTime: DWORD;
    FScrollBarsCalculating: Boolean;
    FScrollBarsLockCount: Integer;
    FScrollUIActivityHelper: TdxTouchScrollUIActivityHelper;
    FVScrollBar: TcxScrollBar;

    procedure CreateScrollBars;
    function FindAutoCompleteString(const S: string): Integer;
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;
    function GetScaleFactor: TdxScaleFactor;
    procedure HScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    function IsSizeGripVisible: Boolean;
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure VScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetRedraw(var Message: TWMSetRedraw); message WM_SETREDRAW;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    FContainer: TcxContainer;

    procedure Click; override;
  {$IFNDEF DELPHI101BERLIN}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyWindowHandle; override;
    procedure DoAutoComplete(var Key: Char); virtual;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoScroll(AKind: TScrollBarKind; AScrollCode: TScrollCode; AScrollPos: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    function GetRootContainer: TcxContainer; virtual;
    function GetSizeGripRect: TRect;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawSizeGrip(ADC: HDC);
    function NeedDrawFocusRect: Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RestoreCanvasParametersForFocusRect;
    procedure SaveCanvasParametersForFocusRect;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
    procedure WndProc(var Message: TMessage); override;

    property Container: TcxContainer read FContainer write FContainer;
    property IsRedrawLocked: Boolean read FIsRedrawLocked;
    property RootContainer: TcxContainer read GetRootContainer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultHandler(var Message); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure SetExternalScrollBarsParameters; virtual;
    function ItemVisible(Index: Integer): Boolean;

    property Canvas: TcxCanvas read FCanvas;
    property HScrollBar: TcxScrollBar read FHScrollBar;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property VScrollBar: TcxScrollBar read FVScrollBar;
  end;

  TcxCustomListBox = TcxCustomInnerListBox; // TODO Remove

  { _TcxContainerAccess }

  _TcxContainerAccess = class
  public
    class procedure BeginAutoDrag(AInstance: TcxContainer);
    class procedure Click(AInstance: TcxContainer);
    class procedure DblClick(AInstance: TcxContainer);
    class function DoMouseWheel(AInstance: TcxContainer; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean;
    class procedure DragOver(AInstance: TcxContainer; Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    class procedure KeyDown(AInstance: TcxContainer; var Key: Word;
      Shift: TShiftState);
    class procedure KeyPress(AInstance: TcxContainer; var Key: Char);
    class procedure KeyUp(AInstance: TcxContainer; var Key: Word;
      Shift: TShiftState);
    class procedure MouseDown(AInstance: TcxContainer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    class procedure MouseMove(AInstance: TcxContainer; Shift: TShiftState;
      X, Y: Integer);
    class procedure MouseUp(AInstance: TcxContainer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  end;

function ButtonTocxButton(Button: TMouseButton): TcxMouseButton;
function cxCanShowHint(AControl: TWinControl): Boolean;
function cxGetScrollBarInfo(hwnd: HWND; idObject: Longint; var psbi: TcxScrollBarInfo): Boolean; overload;
function cxGetScrollBarInfo(hwnd: HWND; idObject: Longint; var psbi: TcxScrollBarInfo;
  AScaleFactor: TdxScaleFactor; AIsRightToLeftLayout: Boolean): Boolean; overload;
function DefaultContainerStyleController: TcxStyleController;
procedure DrawContainerShadow(ACanvas: TcxCanvas; const ARect: TRect);
procedure ExtendRectByBorders(var R: TRect; ABorderWidth: Integer; AEdges: TcxBorders);

// PARENT STRAIN
function CheckParentsNativeHandle(AControl: TWinControl; ANativeHandle: THandle): Boolean;
function FindFirstNonChildParentWindow(AWnd: HWND): HWND;
function IsChildWindow(AParent: TWinControl; AChildHandle: THandle): Boolean;

// HANDLE
function HasHandle(AControl: TWinControl; AHandle: THandle): Boolean;
function HasNativeHandle(AControl: TWinControl;
  AHandle: THandle; ACheckChildren: Boolean = False): Boolean;

function GetContainerBorderColor(AIsHighlight, AIsOffice11Style: Boolean): TColor;
function GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer; overload;
function GetContainerBorderWidth(ALookAndFeelKind: TcxLookAndFeelKind): Integer; overload;
function GetControlRect(AControl: TControl): TRect;
function GetcxContainer(AControl: TWinControl): TcxContainer;
function GetInnerControlContainer(AControl: TWinControl): TWinControl;
function GetPopupOwnerControl(AWnd: HWND): HWND;
function HasOpenedPopupWindow(AControl: TWinControl): Boolean;
procedure InflateRectByBorders(var R: TRect; ABorderWidth: Integer;
  AEdges: TcxBorders);
function InternalCompareString(const S1, S2: TCaption;
  ACaseSensitive: Boolean): Boolean;
procedure InternalInvalidate(AHandle: THandle; const AOuterRect, AInternalRect: TRect;
  AEraseBackground: Boolean = False; ARedrawNC: Boolean = False);
function IsRelatedWindow(AParentHandle, AChildHandle: THandle): Boolean; overload;
function IsRelatedWindow(AParent: TWinControl; AChildHandle: THandle): Boolean; overload;
function IsFormActive(AForm: TCustomForm): Boolean;
function MouseButtonToShift(Button: TMouseButton): TShiftState;
function UsecxScrollBars: Boolean;
function AreVisualStylesMustBeUsed(ANativeStyle: Boolean; AThemedObjectType:
  TdxThemedObjectType): Boolean;
procedure SetHooksSettingMode(ASetHooksOnlyWhenPopupsAreVisible: Boolean);

function GetWindowRegion(AWindowHandle: THandle): TcxRegionHandle;
procedure SetWindowEmptyRegion(AWindowHandle: THandle);
function SetWindowRegion(AWindowHandle: THandle; ARegionHandle: TcxRegionHandle; ARedraw: Boolean = True): Boolean; overload;
procedure SetWindowRegion(AWindowHandle: THandle; ARegionHandle: TcxRegionHandle; AOperation: TcxRegionOperation; ARedraw: Boolean = True); overload;
procedure SetWindowRegion(AControl: TWinControl; const ABounds: TRect; AOperation: TcxRegionOperation = roSet; ARedraw: Boolean = True); overload;
procedure SetWindowRegion(AControl: TWinControl; const ABounds: TcxContainerInnerControlBounds); overload;
procedure SetWindowShadowRegion(AWindowHandle: THandle;
  const AShadowBounds, AShadowBoundsExtent: TRect;
  ANativeStyle, AShadow: Boolean; const AExcludeRect: TRect);

function GetSizeGripRect(AControl: IcxContainerInnerControl): TRect; overload;
function GetSizeGripRect(AContainer: TcxContainer): TRect; overload;
procedure cxFillSizeGrip(AContainer: TcxContainer); overload;
procedure cxFillSizeGrip(AContainer: TcxContainer; const ARect: TRect); overload;

procedure DisableWindow(AWindowList: TList; AWnd: HWND);
procedure EnableWindows(AWindowList: TList);
procedure DisableAppWindows(ANeedDisable: Boolean = True);
procedure EnableAppWindows;
function IsInternalWindowsDisabled: Boolean;
function IsInternalWindowsDisabling: Boolean;

//messages
function IsMouseDownMessage(AMsg: WPARAM): Boolean;
function IsMessageInQueue(AWnd: HWND; AMessage: DWORD): Boolean; deprecated;
function KillMessages(AWnd: HWND; AMsgFilterMin: UINT; AMsgFilterMax: UINT = 0;
  AKillAllMessages: Boolean = True): Boolean; deprecated;
procedure LockCMActivateMessages(ALock: Boolean);

procedure CloseRelatedPopups(AWnd: HWND);
procedure CloseUnrelatedPopups(AWnd: HWND);

var
  cxContainerDefaultStyleController: TcxStyleController;
  cxContainerShadowColor: TColor = clBtnShadow;
  TopMostComboBoxes: Boolean = True;

implementation

uses
  dxThemeConsts, dxOffice11, cxDWMApi, dxHooks, dxDPIAwareUtils;

type
  TCanvasAccess = class(TCanvas);
  TCustomFormAccess = class(TCustomForm);
  TWinControlAccess = class(TWinControl);

  TGetScrollBarInfo = function(hwnd: HWND; idObject: Longint;
    var psbi: TScrollBarInfo): BOOL; stdcall;

  TcxPopupVisibleController = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TcxCustomPopupWindow;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseRelated(AWnd: HWND);
    procedure CloseUnRelated(AWnd: HWND);
    function IsAnyVisible: Boolean;
    function IsVisible(AItem: TcxCustomPopupWindow): Boolean;
    procedure Register(AItem: TcxCustomPopupWindow);
    procedure UnRegister(AItem: TcxCustomPopupWindow);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxCustomPopupWindow read GetItems; default;
  end;

var
  FBeingShownPopupWindow: TcxPopupWindow;
  FcxPopupVisibleController: TcxPopupVisibleController;
  FPopupWindowShowing: Boolean = False;
  FShiftState: TShiftState;
  FUsecxScrollBars: Boolean;
  FCaptionInactivationLocked: Boolean;
  FSetHooksOnlyWhenPopupsAreVisible: Boolean;
  GetScrollBarInfoProc: TGetScrollBarInfo = nil;

procedure RemoveHooks; forward;
procedure SetHooks; forward;

function cxPopupVisibleController: TcxPopupVisibleController;
begin
  if FcxPopupVisibleController = nil then
    FcxPopupVisibleController := TcxPopupVisibleController.Create;
  Result := FcxPopupVisibleController;
end;

{ TcxPopupWindowController }

constructor TcxPopupVisibleController.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxPopupVisibleController.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TcxPopupVisibleController.CloseRelated(AWnd: HWND);
var
  I, J: Integer;
  APopupWindow: TcxCustomPopupWindow;
begin
  for I := 0 to Count - 1 do
  begin
    APopupWindow := Items[I];
    if csDestroying in APopupWindow.ComponentState then
      Continue;
    with APopupWindow.OwnerControl do
      if (csDestroying in ComponentState) or not HandleAllocated then
        Continue;
    if IsRelatedWindow(AWnd, APopupWindow.Handle) then
    begin
      for J := Count - 1 downto I do
        Items[J].CloseUp;
      Break;
    end;
  end;
end;

procedure TcxPopupVisibleController.CloseUnRelated(AWnd: HWND);
var
  I: Integer;
  APopupWindow: TcxCustomPopupWindow;
begin
  I := 0;
  while I < Count do
  begin
    APopupWindow := Items[I];
    if APopupWindow.CaptureFocus and not APopupWindow.Active or
      HasNativeHandle(APopupWindow, AWnd, True) then
      Inc(I)
    else
    begin
      APopupWindow.CloseUp;
      I := 0;
    end;
  end;
end;

function TcxPopupVisibleController.IsAnyVisible: Boolean;
begin
  Result := Count > 0;
end;

function TcxPopupVisibleController.IsVisible(AItem: TcxCustomPopupWindow): Boolean;
begin
  Result := FItems.IndexOf(AItem) <> -1;
end;

procedure TcxPopupVisibleController.Register(AItem: TcxCustomPopupWindow);
begin
  // Requires
  Assert((AItem <> nil) and (FItems.IndexOf(AItem) = -1));
  //
  FItems.Add(AItem);
  if FSetHooksOnlyWhenPopupsAreVisible and (Count = 1) then
    SetHooks;
end;

procedure TcxPopupVisibleController.UnRegister(AItem: TcxCustomPopupWindow);
begin
  // Requires
  Assert((AItem <> nil) and (FItems.IndexOf(AItem) <> -1));
  //
  FItems.Remove(AItem);
  if FSetHooksOnlyWhenPopupsAreVisible and (Count = 0) then
    RemoveHooks;
end;

function TcxPopupVisibleController.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxPopupVisibleController.GetItems(
  Index: Integer): TcxCustomPopupWindow;
begin
  Result := TcxCustomPopupWindow(FItems[Index]);
end;

{ _TcxContainerAccess }

class procedure _TcxContainerAccess.BeginAutoDrag(AInstance: TcxContainer);
begin
  AInstance.BeginAutoDrag;
end;

class procedure _TcxContainerAccess.Click(AInstance: TcxContainer);
begin
  AInstance.Click;
end;

class procedure _TcxContainerAccess.DblClick(AInstance: TcxContainer);
begin
  AInstance.DblClick;
end;

class function _TcxContainerAccess.DoMouseWheel(AInstance: TcxContainer;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := AInstance.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

class procedure _TcxContainerAccess.DragOver(AInstance: TcxContainer;
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  AInstance.DragOver(Source, X, Y, State, Accept);
end;

class procedure _TcxContainerAccess.KeyDown(AInstance: TcxContainer;
  var Key: Word; Shift: TShiftState);
begin
  AInstance.KeyDown(Key, Shift);
end;

class procedure _TcxContainerAccess.KeyPress(AInstance: TcxContainer;
  var Key: Char);
begin
  AInstance.KeyPress(Key);
end;

class procedure _TcxContainerAccess.KeyUp(AInstance: TcxContainer; var Key: Word;
  Shift: TShiftState);
begin
  AInstance.KeyUp(Key, Shift);
end;

class procedure _TcxContainerAccess.MouseDown(AInstance: TcxContainer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AInstance.MouseDown(Button, Shift, X, Y);
end;

class procedure _TcxContainerAccess.MouseMove(AInstance: TcxContainer;
  Shift: TShiftState; X, Y: Integer);
begin
  AInstance.MouseMove(Shift, X, Y);
end;

class procedure _TcxContainerAccess.MouseUp(AInstance: TcxContainer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AInstance.MouseUp(Button, Shift, X, Y);
end;

function ButtonTocxButton(Button: TMouseButton): TcxMouseButton;
const
  AButtonMap: array[TMouseButton] of TcxMouseButton = (cxmbLeft, cxmbRight, cxmbMiddle);
begin
  Result := AButtonMap[Button];
end;

function cxCanShowHint(AControl: TWinControl): Boolean;

  function GetForm(AWnd: HWND; out AForm: TCustomForm;
    out AFormStyle: TFormStyle): Boolean;
  var
    AControl: TWinControl;
  begin
    AControl := FindControl(AWnd);
    Result := AControl is TCustomForm;
    if Result then
    begin
      AForm := TCustomForm(AControl);
      AFormStyle := TCustomFormAccess(AForm).FormStyle;
    end;
  end;

  function GetRootParent(AWnd: HWND): HWND;
  var
    AFormStyle: TFormStyle;
    AParentForm: TCustomForm;
  begin
    repeat
      if IsChildClassWindow(AWnd) then
        Result := GetParent(AWnd)
      else
        Result := 0;
      if Result = 0 then
        Break;
      AWnd := Result;
      if GetForm(AWnd, AParentForm, AFormStyle) and
        (AFormStyle = fsMDIChild) then
          Break;
    until False;
    Result := AWnd;
  end;

var
  AForm: TCustomForm;
  AFormStyle: TFormStyle;
  ARootParent: HWND;
begin
  Result := Application.Active and AControl.HandleAllocated and
    IsWindowVisible(AControl.Handle) and
    (FindVCLWindow(GetMouseCursorPos) = AControl);
  if Result then
  begin
    ARootParent := GetRootParent(AControl.Handle);
    Result := not GetForm(ARootParent, AForm, AFormStyle) or
      AForm.Active or (AFormStyle = fsMDIForm) or cxPopupVisibleController.IsAnyVisible and
      IsChild(ARootParent, cxPopupVisibleController[0].OwnerControl.Handle);
  end;
end;

function CheckParentsNativeHandle(AControl: TWinControl; ANativeHandle: THandle): Boolean;
var
  AParentForm: TCustomForm;
  AParentHandle, AParentHandle1: HWND;
begin
  Result := False;
  if AControl = nil then
    Exit;
  AParentForm := GetParentForm(AControl);
  if AParentForm = nil then
    Exit;
  Result := HasNativeHandle(AParentForm, ANativeHandle, True);
  if not Result and (AParentForm.Parent = nil) then
  begin
    AParentHandle := AParentForm.Handle;
    repeat
      AParentHandle1 := GetParent(AParentHandle);
      if (AParentHandle1 = 0) or not IsChild(AParentHandle1, AParentHandle) then
        Break;
      AParentHandle := AParentHandle1;
    until False;
    if AParentHandle <> AParentForm.Handle then
      Result := (AParentHandle = ANativeHandle) or IsChild(AParentHandle, ANativeHandle);
  end;
end;

function cxGetScrollBarInfo(hwnd: HWND; idObject: Longint; var psbi: TcxScrollBarInfo): Boolean;
begin
  psbi.cbSize := SizeOf(psbi);
  Result := FUsecxScrollBars and GetScrollBarInfoProc(hwnd, idObject, psbi);
end;

function cxGetScrollBarInfo(hwnd: HWND; idObject: Longint;
  var psbi: TcxScrollBarInfo; AScaleFactor: TdxScaleFactor; AIsRightToLeftLayout: Boolean): Boolean;
begin
  Result := cxGetScrollBarInfo(hwnd, idObject, psbi);

  if Result and dxIsProcessPerMonitorV2Aware then
  begin
    if idObject = Longint(OBJID_HSCROLL) then
      psbi.rcScrollBar := cxRectSetHeight(psbi.rcScrollBar, dxGetSystemMetrics(SM_CYHSCROLL, AScaleFactor))
    else
      if AIsRightToLeftLayout then
        psbi.rcScrollBar := cxRectSetRight(psbi.rcScrollBar, psbi.rcScrollBar.Right, dxGetSystemMetrics(SM_CXVSCROLL, AScaleFactor))
      else
        psbi.rcScrollBar := cxRectSetWidth(psbi.rcScrollBar, dxGetSystemMetrics(SM_CXVSCROLL, AScaleFactor));
  end;
end;

function DefaultContainerStyleController: TcxStyleController;
begin
  Result := cxContainerDefaultStyleController;
end;

procedure DrawContainerShadow(ACanvas: TcxCanvas; const ARect: TRect);
var
  R: TRect;
begin
  with ACanvas do
  begin
    Brush.Color := cxContainerShadowColor;
    with R do
    begin
      Left := ARect.Left + cxContainerShadowWidth;
      Top := ARect.Bottom;
      Right := ARect.Right;
      Bottom := Top + cxContainerShadowWidth;
      FillRect(R);
      ExcludeClipRect(R);

      Left := ARect.Right;
      Top := ARect.Top + cxContainerShadowWidth;
      Right := Left + cxContainerShadowWidth;
      Bottom := ARect.Bottom + cxContainerShadowWidth;
      FillRect(R);
      ExcludeClipRect(R);
    end;
  end;
end;

procedure ExtendRectByBorders(var R: TRect; ABorderWidth: Integer; AEdges: TcxBorders);
begin
  if bLeft in AEdges then
    Dec(R.Left, ABorderWidth);
  if bTop in AEdges then
    Dec(R.Top, ABorderWidth);
  if bRight in AEdges then
    Inc(R.Right, ABorderWidth);
  if bBottom in AEdges then
    Inc(R.Bottom, ABorderWidth);
end;

function FindFirstNonChildParentWindow(AWnd: HWND): HWND;
begin
  Result := 0;
  while (AWnd <> 0) and (Result = 0) do
  begin
    if not IsChildClassWindow(AWnd) then
      Result := AWnd;
    AWnd := GetParent(AWnd);
  end;
end;

function GetContainerBorderColor(AIsHighlight, AIsOffice11Style: Boolean): TColor;
begin
  if not AIsHighlight then
    Result := clBtnShadow
  else
    if AIsOffice11Style then
      Result := dxOffice11SelectedBorderColor
    else
      Result := clHighlight;
end;

function GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer;
begin
  Result := cxLookAndFeelPaintersManager.GetPainter(lfsStandard).GetContainerBorderWidth(ABorderStyle);
end;

function GetContainerBorderWidth(ALookAndFeelKind: TcxLookAndFeelKind): Integer;
const
  BorderStyle: array [TcxLookAndFeelKind] of TcxContainerBorderStyle = (
    cbsFlat, cbs3D, cbsUltraFlat, cbsOffice11);
begin
  Result := GetContainerBorderWidth(BorderStyle[ALookAndFeelKind]);
end;

function GetControlRect(AControl: TControl): TRect;
begin
  Result := Rect(0, 0, AControl.Width, AControl.Height);
end;

function GetcxContainer(AControl: TWinControl): TcxContainer;
var
  AIContainerInnerControl: IcxContainerInnerControl;
begin
  Result := nil;
  if AControl is TcxContainer then
    Result := TcxContainer(AControl)
  else
    if (AControl <> nil) and Supports(AControl, IcxContainerInnerControl, AIContainerInnerControl) then
      Result := AIContainerInnerControl.ControlContainer;
end;

function GetInnerControlContainer(AControl: TWinControl): TWinControl;
var
  AInnerControl: IcxContainerInnerControl;
begin
  if Supports(AControl, IcxContainerInnerControl, AInnerControl) then
    Result := AInnerControl.ControlContainer
  else
    Result := AControl;
end;

function GetWindowShadowRegion(AWindowHandle: THandle;
  AShadowBounds, AShadowBoundsExtent: TRect; ANativeStyle, AShadow: Boolean;
  const AExcludeRect: TRect): TcxRegionHandle;
var
  ATempRegion: TcxRegionHandle;
begin
  Result := 0;
  if not ANativeStyle and AShadow then
  begin
    Result := CreateRectRgnIndirect(AShadowBounds);
    OffsetRect(AShadowBounds, cxContainerShadowWidth, cxContainerShadowWidth);
    Inc(AShadowBounds.Top, AShadowBoundsExtent.Top);
    Inc(AShadowBounds.Left, AShadowBoundsExtent.Left);
    Dec(AShadowBounds.Right, AShadowBoundsExtent.Right);
    Dec(AShadowBounds.Bottom, AShadowBoundsExtent.Bottom);
    ATempRegion := CreateRectRgnIndirect(AShadowBounds);
    CombineRgn(Result, Result, ATempRegion, RGN_OR);
    DeleteObject(ATempRegion);
    if not IsRectEmpty(AExcludeRect) then
    begin
      ATempRegion := CreateRectRgnIndirect(AExcludeRect);
      CombineRgn(Result, Result, ATempRegion, RGN_DIFF);
      DeleteObject(ATempRegion);
    end;
  end;
end;

function GetPopupOwnerControl(AWnd: HWND): HWND;
var
  AControl: TWinControl;
begin
  Result := AWnd;
  while AWnd <> 0 do
  begin
    AControl := FindControl(AWnd);
    if AControl is TcxCustomPopupWindow then
    begin
      if TcxCustomPopupWindow(AControl).OwnerControl.HandleAllocated then
        Result := TcxCustomPopupWindow(AControl).OwnerControl.Handle;
      Break;
    end;
    AWnd := GetParent(AWnd);
  end;
end;

function HasHandle(AControl: TWinControl; AHandle: THandle): Boolean;
begin
  Result := HasNativeHandle(AControl, AHandle);
end;

function HasNativeHandle(AControl: TWinControl; AHandle: THandle;
  ACheckChildren: Boolean = False): Boolean;
begin
  Result := (AControl <> nil) and AControl.HandleAllocated and
    ((AControl.Handle = AHandle) or ACheckChildren and IsRelatedWindow(AControl, AHandle));
end;

function HasOpenedPopupWindow(AControl: TWinControl): Boolean;
var
  AContainer: TcxContainer;
begin
  AContainer := GetcxContainer(AControl);
  Result := (AContainer <> nil) and AContainer.HasPopupWindow;
end;

procedure InflateRectByBorders(var R: TRect; ABorderWidth: Integer;
  AEdges: TcxBorders);
begin
  if not(bLeft in AEdges) then
    Inc(R.Left, ABorderWidth);
  if not(bTop in AEdges) then
    Inc(R.Top, ABorderWidth);
  if not(bRight in AEdges) then
    Dec(R.Right, ABorderWidth);
  if not(bBottom in AEdges) then
    Dec(R.Bottom, ABorderWidth);
end;

function InternalCompareString(const S1, S2: TCaption; ACaseSensitive: Boolean): Boolean;
begin
  if ACaseSensitive then
    Result := AnsiCompareStr(S1, S2) = 0
  else
    Result := AnsiUpperCase(S1) = AnsiUpperCase(S2);
end;

procedure InternalFillRect(ACanvas: TcxCanvas; const AOuterRect, AInternalRect: TRect;
  AColor: TColor);
begin
  if IsRectEmpty(AOuterRect) or EqualRect(AOuterRect, AInternalRect) then
    Exit;
  with ACanvas do
  begin
    Brush.Color := AColor;
    if IsRectEmpty(AInternalRect) then
      FillRect(AOuterRect)
    else
    begin
      FillRect(Rect(AOuterRect.Left, AOuterRect.Top,
        AInternalRect.Left, AOuterRect.Bottom));
      FillRect(Rect(AInternalRect.Left, AOuterRect.Top,
        AInternalRect.Right, AInternalRect.Top));
      FillRect(Rect(AInternalRect.Right, AOuterRect.Top,
        AOuterRect.Right, AOuterRect.Bottom));
      FillRect(Rect(AInternalRect.Left, AInternalRect.Bottom,
        AInternalRect.Right, AOuterRect.Bottom));
    end;
  end;
end;

procedure InternalInvalidate(AHandle: THandle; const AOuterRect, AInternalRect: TRect;
  AEraseBackground: Boolean = False; ARedrawNC: Boolean = False);

  procedure InternalInvalidateRect(const R: TRect);
  begin
    cxRedrawWindow(AHandle, R, AEraseBackground, ARedrawNC);
  end;

begin
  if IsRectEmpty(AInternalRect) then
    InternalInvalidateRect(AOuterRect)
  else
  begin
    InternalInvalidateRect(Rect(AOuterRect.Left, AOuterRect.Top, AInternalRect.Left, AOuterRect.Bottom));
    InternalInvalidateRect(Rect(AInternalRect.Left, AOuterRect.Top, AInternalRect.Right, AInternalRect.Top));
    InternalInvalidateRect(Rect(AInternalRect.Right, AOuterRect.Top, AOuterRect.Right, AOuterRect.Bottom));
    InternalInvalidateRect(Rect(AInternalRect.Left, AInternalRect.Bottom, AInternalRect.Right, AOuterRect.Bottom));
  end;
end;

function IsChildWindow(AParent: TWinControl; AChildHandle: THandle): Boolean;

  function InternalNativeIsChildWindow(AParent: TWinControl): Boolean;
  begin
    Result := AParent.HandleAllocated and IsChild(AParent.Handle, AChildHandle);
  end;

  function InternalIsChildWindow(AParent: TWinControl): Boolean;
  var
    I: Integer;
    APopupWindow: TcxCustomPopupWindow;
  begin
    with AParent do
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TWinControl then
        begin
          if HasNativeHandle(TWinControl(Controls[I]), AChildHandle) then
          begin
            Result := True;
            Exit;
          end else
          begin
            Result := InternalIsChildWindow(TWinControl(Controls[I]));
            if Result then
              Exit;
          end;
          Result := InternalNativeIsChildWindow(TWinControl(Controls[I]));
          if Result then
            Exit;
        end;
    if AParent is TcxCustomPopupWindow then
      for I := 0 to cxPopupVisibleController.Count - 1 do
      begin
        APopupWindow := cxPopupVisibleController[I];
        if (APopupWindow = AParent) or (GetParentForm(APopupWindow.OwnerControl) <> AParent) then
          Continue;
        if HasNativeHandle(APopupWindow, AChildHandle) then
        begin
          Result := True;
          Exit;
        end else
        begin
          Result := InternalIsChildWindow(APopupWindow);
          if Result then Exit;
        end;
      end;
    for I := 0 to cxPopupVisibleController.Count - 1 do
    begin
      APopupWindow := cxPopupVisibleController[I];
      if APopupWindow.OwnerControl = AParent then
      begin
        Result := HasNativeHandle(APopupWindow, AChildHandle) or
          InternalIsChildWindow(APopupWindow);
        if Result then Exit;
      end;
    end;
    Result := False;
  end;

begin
  Result := False;
  if (AParent = nil) or (AChildHandle = 0) or HasNativeHandle(AParent, AChildHandle) then
    Exit;
  Result := InternalNativeIsChildWindow(AParent);
  if not Result then
    Result := InternalIsChildWindow(AParent);
end;

function IsRelatedWindow(AParentHandle, AChildHandle: THandle): Boolean;

  function GetOwnerControl(var AChildHandle: THandle): Boolean;
  var
    AControl: TWinControl;
    APopup: IdxPopupControl;
  begin
    Result := False;
    AControl := FindControl(GetAncestor(AChildHandle, GA_ROOT));
    if (AControl <> nil) and Supports(AControl, IdxPopupControl, APopup) then
    begin
      AControl := APopup.GetOwnerControl;
      if (AControl <> nil) and AControl.HandleAllocated then
      begin
        AChildHandle := AControl.Handle;
        Result := AChildHandle <> 0;
      end;
    end
  end;

begin
  Result := False;
  if (AParentHandle <> 0) and (AChildHandle <> 0) and
    (AParentHandle <> AChildHandle) then
    repeat
      Result := (AParentHandle = AChildHandle) or
        IsChild(AParentHandle, AChildHandle);
    until Result or not GetOwnerControl(AChildHandle);
end;

function IsRelatedWindow(AParent: TWinControl; AChildHandle: THandle): Boolean;
begin
  Result := (AParent <> nil) and AParent.HandleAllocated and
    IsRelatedWindow(AParent.Handle, AChildHandle);
end;

function IsFormActive(AForm: TCustomForm): Boolean;

  function IsWindowActive(AWindowHandle: HWND): Boolean;
  begin
    Result := AWindowHandle = GetActiveWindow;
  end;

  function IsMDIChildActive(AForm: TCustomForm): Boolean;
  begin
    Result := IsMDIChild(AForm) and IsWindowActive(Application.MainForm.Handle) and (Application.MainForm.ActiveMDIChild = AForm);
  end;

  function IsParentActive(AForm: TCustomForm): Boolean;
  begin
    Result := not IsMDIChild(AForm) and IsWindowActive(FindFirstNonChildParentWindow(AForm.Handle));
  end;

begin
  Result := (AForm <> nil) and AForm.HandleAllocated and
    (IsWindowActive(AForm.Handle) or IsMDIChildActive(AForm) or IsParentActive(AForm));
end;

function IsMouseDownMessage(AMsg: WPARAM): Boolean;
begin
  case AMsg of
    WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
    WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      Result := True
  else
    Result := False;
  end;
end;

function IsMessageInQueue(AWnd: HWND; AMessage: DWORD): Boolean;
begin
  Result := dxMessagesController.IsMessageInQueue(AWnd, AMessage);
end;

function KillMessages(AWnd: HWND; AMsgFilterMin: UINT; AMsgFilterMax: UINT = 0;
  AKillAllMessages: Boolean = True): Boolean;
begin
  Result := dxMessagesController.KillMessages(AWnd, AMsgFilterMin, AMsgFilterMax, AKillAllMessages);
end;

procedure LockCMActivateMessages(ALock: Boolean);
begin
  if ALock then
    dxMessagesController.LockMessages([CM_ACTIVATE, CM_DEACTIVATE])
  else
    dxMessagesController.UnlockMessages([CM_ACTIVATE, CM_DEACTIVATE]);
end;

procedure CloseRelatedPopups(AWnd: HWND);
begin
  cxPopupVisibleController.CloseRelated(AWnd);
end;

procedure CloseUnrelatedPopups(AWnd: HWND);
begin
  cxPopupVisibleController.CloseUnRelated(AWnd);
end;

function MouseButtonToShift(Button: TMouseButton): TShiftState;
begin
  case Button of
    mbLeft:
      Result := [ssLeft];
    mbMiddle:
      Result := [ssMiddle];
    mbRight:
      Result := [ssRight];
  end;
end;

procedure SetUsecxScrollBars;
var
  ALibrary: HMODULE;
begin
{$IFDEF USETCXSCROLLBAR}
  ALibrary := GetModuleHandle(user32);
  if ALibrary <> 0 then
  begin
    @GetScrollBarInfoProc := GetProcAddress(ALibrary, 'GetScrollBarInfo');
    FUsecxScrollBars := Assigned(GetScrollBarInfoProc);
  end;
{$ELSE}
  FUsecxScrollBars := False;
{$ENDIF}
end;

function GetWindowRegion(AWindowHandle: THandle): TcxRegionHandle;
begin
  Result := CreateRectRgnIndirect(cxEmptyRect);
  if GetWindowRgn(AWindowHandle, Result) = ERROR then
  begin
    DeleteObject(Result);
    Result := 0;
  end;
end;

procedure SetWindowEmptyRegion(AWindowHandle: THandle);
begin
  SetWindowRegion(AWindowHandle, CreateRectRgnIndirect(cxEmptyRect));
end;

function SetWindowRegion(AWindowHandle: THandle; ARegionHandle: TcxRegionHandle; ARedraw: Boolean): Boolean;

  function IsWindowRegionChanged(AWindowHandle: THandle; ARegionHandle: TcxRegionHandle): Boolean;
  var
    ACurrentRegion: HRGN;
  begin
    ACurrentRegion := GetWindowRegion(AWindowHandle);
    Result := (ARegionHandle = 0) and (ACurrentRegion <> 0) or
      (ACurrentRegion = 0) and (ARegionHandle <> 0) or
      (ARegionHandle <> 0) and (ACurrentRegion <> 0) and not EqualRgn(ARegionHandle, ACurrentRegion);
    DeleteObject(ACurrentRegion);
  end;

begin
  if IsWindowRegionChanged(AWindowHandle, ARegionHandle) then
    Result := SetWindowRgn(AWindowHandle, ARegionHandle, ARedraw) <> 0
  else
      Result := DeleteObject(ARegionHandle);
end;

procedure SetWindowRegion(AWindowHandle: THandle; ARegionHandle: TcxRegionHandle; AOperation: TcxRegionOperation; ARedraw: Boolean);
var
  ARegion: TcxRegion;
begin
  if AOperation = roSet then
    SetWindowRegion(AWindowHandle, ARegionHandle, ARedraw)
  else
  begin
    ARegion := TcxRegion.Create;
    try
      if GetWindowRgn(AWindowHandle, ARegion.Handle) <> Error then
        ARegion.Combine(TcxRegion.Create(ARegionHandle), AOperation)
      else
        ARegion.Combine(TcxRegion.Create(ARegionHandle), roSet);
      SetWindowRegion(AWindowHandle, ARegion.Handle, ARedraw);
      ARegion.Handle := 0;
    finally
      ARegion.Free;
    end;
  end;
end;

procedure SetWindowRegion(AControl: TWinControl; const ABounds: TRect; AOperation: TcxRegionOperation; ARedraw: Boolean);
begin
  SetWindowRegion(AControl.Handle, CreateRectRgnIndirect(ABounds), AOperation, ARedraw);
end;

procedure SetWindowRegion(AControl: TWinControl; const ABounds: TcxContainerInnerControlBounds);
begin
  if ABounds.IsEmpty then
    Windows.SetWindowRgn(AControl.Handle, 0, True)
  else
    SetWindowRegion(AControl, ABounds.Rect);
end;

procedure SetWindowShadowRegion(AWindowHandle: THandle;
  const AShadowBounds, AShadowBoundsExtent: TRect;
  ANativeStyle, AShadow: Boolean; const AExcludeRect: TRect);
begin
  SetWindowRegion(AWindowHandle, GetWindowShadowRegion(AWindowHandle,
    AShadowBounds, AShadowBoundsExtent, ANativeStyle, AShadow, AExcludeRect), True);
end;

function UsecxScrollBars: Boolean;
begin
  Result := FUsecxScrollBars;
end;

function AreVisualStylesMustBeUsed(ANativeStyle: Boolean;
  AThemedObjectType: TdxThemedObjectType): Boolean;
begin
  Result := ANativeStyle and (OpenTheme(AThemedObjectType) <> 0);
end;

function GetSizeGripRect(AControl: IcxContainerInnerControl): TRect; overload;
begin
  Result := GetSizeGripRect(AControl.ControlContainer);
end;

function GetSizeGripRect(AContainer: TcxContainer): TRect; overload;
var
  R: TRect;
begin
  if AContainer.HScrollBar.Visible and AContainer.VScrollBar.Visible then
  begin
    Result.TopLeft := AContainer.ClientToScreen(Point(AContainer.VScrollBar.Left, AContainer.HScrollBar.Top));
    R := cxGetWindowRect(AContainer.InnerControl);
    Dec(Result.Left, R.Left);
    Dec(Result.Top, R.Top);
    Result.Right := Result.Left + AContainer.VScrollBar.Width;
    Result.Bottom := Result.Top + AContainer.HScrollBar.Height;
  end
  else
    Result := cxEmptyRect;
end;

procedure cxFillSizeGrip(AContainer: TcxContainer; const ARect: TRect);
var
  DC: HDC;
  ABrush: HBRUSH;
  APrevLayoutMode: Integer;
begin
  if not IsRectEmpty(ARect) then
  begin
    DC := GetWindowDC(AContainer.InnerControl.Handle);
    ABrush := 0;
    try
      ABrush := CreateSolidBrush(ColorToRGB(AContainer.LookAndFeel.Painter.DefaultSizeGripAreaColor));
      APrevLayoutMode := SetLayout(DC, LAYOUT_LTR);
      try
        FillRect(DC, ARect, ABrush);
      finally
        SetLayout(DC, APrevLayoutMode);
      end;
    finally
      if ABrush <> 0 then
        DeleteObject(ABrush);
      ReleaseDC(AContainer.InnerControl.Handle, DC);
    end;
  end;
end;

procedure cxFillSizeGrip(AContainer: TcxContainer);
begin
  cxFillSizeGrip(AContainer, GetSizeGripRect(AContainer));
end;

procedure SetHooksSettingMode(ASetHooksOnlyWhenPopupsAreVisible: Boolean);
begin
  if ASetHooksOnlyWhenPopupsAreVisible <> FSetHooksOnlyWhenPopupsAreVisible then
  begin
    FSetHooksOnlyWhenPopupsAreVisible := ASetHooksOnlyWhenPopupsAreVisible;
    if not cxPopupVisibleController.IsAnyVisible then
      if ASetHooksOnlyWhenPopupsAreVisible then
        RemoveHooks
      else
        SetHooks;
  end;
end;

var
  FDisablingWindowsCounter: Integer;
  FTopLevelWindowList: TList;
  FInternalWindowsEnabling: Boolean;

procedure DisableWindow(AWindowList: TList; AWnd: HWND);
begin
  if IsWindowEnabled(AWnd) then
  begin
    AWindowList.Add(Pointer(AWnd));
    EnableWindow(AWnd, False);
  end;
end;

procedure EnableWindows(AWindowList: TList);
var
  I: Integer;
  AWnd: HWND;
begin
  if AWindowList <> nil then
    for I := 0 to AWindowList.Count - 1 do
    begin
      AWnd := HWND(AWindowList[I]);
      if IsWindow(AWnd) then
        EnableWindow(AWnd, True);
    end;
end;

function DisableTopLevelWindow(AWnd: HWND; AInfo: Pointer): BOOL; stdcall;
var
  AProcessId, AThreadID: Cardinal;
begin
  Result := True;
  AThreadID := GetWindowThreadProcessId(AWnd, @AProcessId);
  if (AProcessId = GetCurrentProcessId) and (AThreadID = MainThreadID) then
    DisableWindow(FTopLevelWindowList, AWnd);
end;

procedure DisableAppWindows(ANeedDisable: Boolean = True);
begin
  Inc(FDisablingWindowsCounter);

  if (FDisablingWindowsCounter = 1) and ANeedDisable then
  try
    FInternalWindowsEnabling := True;
    FTopLevelWindowList := TList.Create;
    EnumWindows(@DisableTopLevelWindow, 0);
  finally
    FInternalWindowsEnabling := False;
  end;
end;

procedure EnableAppWindows;
begin
  Dec(FDisablingWindowsCounter);

  if FDisablingWindowsCounter = 0 then
  try
    FInternalWindowsEnabling := True;
    EnableWindows(FTopLevelWindowList);
    FreeAndNil(FTopLevelWindowList);
  finally
    FInternalWindowsEnabling := False;
  end;
end;

function IsInternalWindowsDisabled: Boolean;
begin
  Result := FDisablingWindowsCounter > 0;
end;

function IsInternalWindowsDisabling: Boolean;
begin
  Result := FInternalWindowsEnabling;
end;

{ TcxContainerViewInfo }

constructor TcxContainerViewInfo.Create;
begin
  inherited Create;
  ContainerState := [csNormal];
end;

procedure TcxContainerViewInfo.Assign(Source: TObject);
begin
  if Source is TcxContainerViewInfo then
    ClientRect := TcxContainerViewInfo(Source).ClientRect;
end;

procedure TcxContainerViewInfo.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  Painter.DrawContainerBorder(ACanvas, R, BorderStyle, BorderWidth, BorderColor, Edges);
end;

function TcxContainerViewInfo.GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion;
begin
  Result := TcxRegion.Create;
end;

procedure TcxContainerViewInfo.Offset(DX, DY: Integer);
begin
  if FCalculated then
  begin
    OffsetRect(BorderRect, DX, DY);
    OffsetRect(Bounds, DX, DY);
    OffsetRect(ClientRect, DX, DY);
  end;
end;

procedure TcxContainerViewInfo.Paint(ACanvas: TcxCanvas);
begin
  InternalPaint(ACanvas);
end;

procedure TcxContainerViewInfo.UpdateStyle(AStyle: TcxContainerStyle);
begin
  Font := AStyle.Font;
  Painter := AStyle.LookAndFeel.Painter;
  UseSkins := AStyle.LookAndFeel.SkinPainter <> nil;
end;

function TcxContainerViewInfo.GetContainerBorderStyle: TcxContainerBorderStyle;
begin
  Result := BorderStyle;
end;

procedure TcxContainerViewInfo.InternalPaint(ACanvas: TcxCanvas);

  procedure DrawBackground;
  var
    R: TRect;
  begin
    R := BorderRect;
    Dec(R.Left, BorderWidth);
    Dec(R.Top, BorderWidth);
    if bRight in Edges then Inc(R.Right, BorderWidth);
    if bBottom in Edges then Inc(R.Bottom, BorderWidth);
    if Shadow then
      DrawContainerShadow(ACanvas, R);
    if not(bRight in Edges) then Inc(R.Right, BorderWidth);
    if not(bBottom in Edges) then Inc(R.Bottom, BorderWidth);
    DrawBorder(ACanvas, R);
    with ACanvas do
    begin
      Brush.Color := BackgroundColor;
      FillRect(BorderRect);
    end;
  end;

  procedure DrawNativeStyleBackground;

    function IsBorderNeeded: Boolean;
    begin
      Result := BorderStyle <> cbsNone;
    end;

  var
    AThemedObjectType: TdxThemedObjectType;
    APart, AState: Integer;
    R: TRect;
    AColor: COLORREF;
  begin
    if not IsBorderNeeded then
    begin
      ACanvas.Brush.Color := BackgroundColor;
      ACanvas.FillRect(Bounds);
    end
    else
    begin
      GetThemeBackgroundContentRect(OpenTheme(ThemedObjectType), ACanvas.Handle, EP_EDITTEXT,
        NativeState, Bounds, R);
      ACanvas.Brush.Color := BackgroundColor;
      ACanvas.FillRect(R);
      if IsCompositionEnabled then
      begin
        AThemedObjectType := totListBox;
        APart := LBCP_BORDER_NOSCROLL;
        AState := LBPSN_NORMAL;
      end
      else
      begin
        AThemedObjectType := totComboBox;
        APart := CP_DROPDOWNBUTTON;
        AState := CBXS_NORMAL;
      end;
      GetThemeColor(OpenTheme(AThemedObjectType), APart, AState, TMT_BORDERCOLOR, AColor);
      InternalFillRect(ACanvas, Bounds, R, AColor);
    end;
  end;

begin
  if NativeStyle then
    DrawNativeStyleBackground
  else
    DrawBackground;
end;

procedure TcxContainerViewInfo.SetBackgroundColor(Value: TColor);
begin
  FBackgroundColor := Value;
end;

{ TcxContainerStyles }

constructor TcxContainerStyles.Create(AOwner: TPersistent;
  AStyleClass: TcxContainerStyleClass);

  function CreateStyle(AState: TcxContainerStateItem): TcxContainerStyle;
  begin
    if AState = csNormal then
      Result := AStyleClass.Create(AOwner, False, nil, AState)
    else
      Result := AStyleClass.Create(AOwner, False, FStyles[csNormal], AState);
  end;

var
  AState: TcxContainerStateItem;
begin
  inherited Create;
  for AState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
    FStyles[AState] := CreateStyle(AState);
end;

destructor TcxContainerStyles.Destroy;
var
  AState: TcxContainerStateItem;
begin
  for AState := High(TcxContainerStateItem) downto Low(TcxContainerStateItem) do
    FreeAndNil(FStyles[AState]);
  inherited Destroy;
end;

procedure TcxContainerStyles.RestoreDefaults;
var
  AState: TcxContainerStateItem;
begin
  for AState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
    FStyles[AState].RestoreDefaults;
end;

function TcxContainerStyles.GetStyle(AState: TcxContainerStateItem): TcxContainerStyle;
begin
  Result := FStyles[AState];
end;

function TcxContainerStyles.GetStyleDisabled: TcxContainerStyle;
begin
  Result := FStyles[csDisabled];
end;

function TcxContainerStyles.GetStyleFocused: TcxContainerStyle;
begin
  Result := FStyles[csActive];
end;

function TcxContainerStyles.GetStyleHot: TcxContainerStyle;
begin
  Result := FStyles[csHotTrack];
end;

function TcxContainerStyles.GetStyleNormal: TcxContainerStyle;
begin
  Result := FStyles[csNormal];
end;

procedure TcxContainerStyles.SetOnChanged(Value: TNotifyEvent);
var
  AState: TcxContainerStateItem;
begin
  for AState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
    FStyles[AState].OnChanged := Value;
end;

procedure TcxContainerStyles.SetStyle(AState: TcxContainerStateItem; Value: TcxContainerStyle);
begin
  FStyles[AState].Assign(Value);
end;

procedure TcxContainerStyles.SetStyleDisabled(Value: TcxContainerStyle);
begin
  FStyles[csDisabled].Assign(Value);
end;

procedure TcxContainerStyles.SetStyleFocused(Value: TcxContainerStyle);
begin
  FStyles[csActive].Assign(Value);
end;

procedure TcxContainerStyles.SetStyleHot(Value: TcxContainerStyle);
begin
  FStyles[csHotTrack].Assign(Value);
end;

procedure TcxContainerStyles.SetStyleNormal(Value: TcxContainerStyle);
begin
  FStyles[csNormal].Assign(Value);
end;

{ TcxStyleController }

constructor TcxStyleController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
  FStyles := TcxContainerStyles.Create(Self, GetStyleClass);
  FStyles.OnChanged := StyleChanged;
end;

destructor TcxStyleController.Destroy;
var
  I: Integer;
begin
  FIsDestruction := True;
  for I := FListeners.Count - 1 downto 0 do
    TcxContainerStyle(FListeners[I]).ControllerFreeNotification(Self);
  FreeAndNil(FStyles);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TcxStyleController.RestoreStyles;
begin
  FStyles.RestoreDefaults;
end;

procedure TcxStyleController.Loaded;
begin
  inherited Loaded;
  Changed;
end;

procedure TcxStyleController.ChangeScale(M, D: Integer);
var
  AState: TcxContainerStateItem;
  AStyle: TcxContainerStyle;
begin
  inherited ChangeScale(M, D);
  for AState := Low(TcxContainerStateItem) to High(TcxContainerStateItem) do
  begin
    AStyle := Styles[AState];
    if csvFont in AStyle.AssignedValues then
      AStyle.Font.Height := MulDiv(AStyle.Font.Height, M, D);
  end;
end;

procedure TcxStyleController.AddListener(AListener: TcxContainerStyle);
begin
  if (AListener = nil) or (FListeners.IndexOf(AListener) >= 0) then
    Exit;
  FListeners.Add(AListener);
  AListener.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
end;

procedure TcxStyleController.Changed;
var
  I: Integer;
begin
  if Assigned(FOnStyleChanged) then
    FOnStyleChanged(Self);
  if not IsDestruction then
    for I := 0 to Listeners.Count - 1 do
      TcxContainerStyle(Listeners[I]).ControllerChangedNotification(Self);
end;

function TcxStyleController.GetStyleClass: TcxContainerStyleClass;
begin
  Result := TcxContainerStyle;
end;

function TcxStyleController.GetStylesClass: TcxContainerStylesClass;
begin
  Result := TcxContainerStyles;
end;

function TcxStyleController.IsDestruction: Boolean;
begin
  Result := FIsDestruction;
end;

procedure TcxStyleController.RemoveListener(AListener: TcxContainerStyle);
begin
  if (AListener = nil) or (FListeners.IndexOf(AListener) < 0) then
    Exit;
  FListeners.Remove(AListener);
  AListener.LookAndFeel.MasterLookAndFeel := nil;
end;

function TcxStyleController.GetFakeStyleController: TcxStyleController;
begin
  Result := Style.StyleController;
end;

function TcxStyleController.GetStyle: TcxContainerStyle;
begin
  Result := FStyles.Style;
end;

function TcxStyleController.GetInternalStyle(AState: TcxContainerStateItem): TcxContainerStyle;
begin
  Result := FStyles[AState];
end;

procedure TcxStyleController.SetFakeStyleController(Value: TcxStyleController);
begin
end;

procedure TcxStyleController.SetInternalStyle(AState: TcxContainerStateItem;
  Value: TcxContainerStyle);
begin
  FStyles[AState].Assign(Value);
end;

procedure TcxStyleController.StyleChanged(Sender: TObject);
begin
  Changed;
end;

{ TcxContainerStyle }

constructor TcxContainerStyle.Create(AOwner: TPersistent;
  ADirectAccessMode: Boolean; AParentStyle: TcxContainerStyle = nil;
  AState: TcxContainerStateItem = csNormal);
begin
  inherited Create(AOwner);
  FDirectAccessMode := ADirectAccessMode;
  FOwner := AOwner;
  if AState <> csNormal then
    FParentStyle := AParentStyle;
  FState := AState;
  if DirectAccessMode then
    FAssignedValues := [csvColor, csvFont, csvTextColor, csvTextStyle]; // TODO ???
  CreateFont;
  FVisibleFont := TFont.Create;
  if IsBaseStyle then
  begin
    FLookAndFeel := TcxLookAndFeel.Create(Self);
    FLookAndFeel.OnChanged := LookAndFeelChanged;
    StyleController := GetDefaultStyleController;
  end;
end;

destructor TcxContainerStyle.Destroy;
begin
  FIsDestroying := True;
  if IsBaseStyle and (ActiveStyleController <> nil) then
    ActiveStyleController.RemoveListener(Self);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FVisibleFont);
  if not DirectAccessMode and IsBaseStyle then
    FreeAndNil(StyleData.Font);
  inherited Destroy;
end;

procedure TcxContainerStyle.Assign(Source: TPersistent);
begin
  if Source is TcxContainerStyle then
  begin
    BeginUpdate;
    try
      with Source as TcxContainerStyle do
      begin
        if Self.IsBaseStyle then
        begin
          Self.StyleController := StyleController;
          Self.LookAndFeel := LookAndFeel;
        end;

        Self.FBorderColor := FBorderColor;
        Self.FBorderStyle := FBorderStyle;
        Self.FEdges := FEdges;
        Self.FHotTrack := FHotTrack;
        Self.FShadow := FShadow;
        Self.FTransparentBorder := FTransparentBorder;
        Self.StyleData.Color := StyleData.Color;

        if Self.DirectAccessMode then
        begin
          Self.StyleData.Font := Font;
          Self.StyleData.FontColor := TextColor;
        end
        else
        begin
          if Self.StyleData.Font <> nil then
          begin
            Self.StyleData.Font.Assign(Font);
            if DirectAccessMode then
              Self.StyleData.Font.Color := TextColor;
          end;
          Self.StyleData.FontColor := TextColor;
          Self.FTextStyle := TextStyle;
        end;

        Self.FAssignedValues := FAssignedValues;

        Self.Changed;
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

procedure TcxContainerStyle.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcxContainerStyle.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    CheckChanges;
  end;
end;

function TcxContainerStyle.GetStyleValue(const APropertyName: string; out StyleValue: TcxContainerStyleValue): Boolean;
var
  I: TcxContainerStyleValue;
  S: string;
begin
  Result := False;
  for I := 0 to GetStyleValueCount - 1 do
  begin
    GetStyleValueName(I, S);
    if InternalCompareString(S, APropertyName, False) then
    begin
      StyleValue := I;
      Result := True;
      Break;
    end;
  end;
end;

function TcxContainerStyle.GetStyleValueCount: Integer;
begin
  Result := cxContainerStyleValueCount;
end;

function TcxContainerStyle.GetStyleValueName(AStyleValue: TcxContainerStyleValue; out StyleValueName: string): Boolean;
begin
  Result := AStyleValue < cxContainerStyleValueCount;
  if Result then
    StyleValueName := cxContainerStyleValueNameA[AStyleValue];
end;

function TcxContainerStyle.GetVisibleFont: TFont;
begin
  UpdateVisibleFont;
  Result := FVisibleFont;
end;

function TcxContainerStyle.HasBorder: Boolean;
begin
  if IsBaseStyle then
    Result := True
  else
    Result := ParentStyle.HasBorder;
end;

function TcxContainerStyle.IsExtendedStylePropertyPublished(const APropertyName: string): Boolean;
var
  AStyleValue: TcxContainerStyleValue;
begin
  if (APropertyName = 'LookAndFeel') or (APropertyName = 'StyleController') then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  if GetStyleValue(APropertyName, AStyleValue) then
    Result := not(AStyleValue in InternalGetNotPublishedExtendedStyleValues);
end;

function TcxContainerStyle.IsValueAssigned(AValue: TcxContainerStyleValue): Boolean;
var
  ABorderStyle: TcxContainerBorderStyle;
  AColor: TColor;
  AEdges: TcxBorders;
  AFont: TFont;
  ATempBool: Boolean;
  ATextStyle: TFontStyles;
begin
  Result := False;
  case AValue of
    csvBorderColor:
      Result := InternalGetBorderColor(AColor);
    csvBorderStyle:
      Result := InternalGetBorderStyle(ABorderStyle);
    csvColor:
      Result := InternalGetColor(AColor);
    csvEdges:
      Result := InternalGetEdges(AEdges);
    csvFont:
      Result := InternalGetFont(AFont);
    csvHotTrack:
      Result := InternalGetHotTrack(ATempBool);
    csvShadow:
      Result := InternalGetShadow(ATempBool);
    csvTextColor:
      Result := InternalGetTextColor(AColor);
    csvTextStyle:
      Result := InternalGetTextStyle(ATextStyle);
  end;
end;

procedure TcxContainerStyle.RestoreDefaults;
begin
  BeginUpdate;
  try
    AssignedValues := [];
    if IsBaseStyle then
    begin
      LookAndFeel.Reset;
      if Container <> nil then
      begin
        Container.ParentColor := False;
        Container.ParentFont := True;
      end
      else
        if not DirectAccessMode then
          RestoreFont(StyleData.Font);
      if (Container <> nil) and Container.DefaultParentColor and
        ((ActiveStyleController = nil) or not ActiveStyleController.Style.IsValueAssigned(csvColor)) then
          Container.ParentColor := True;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxContainerStyle.DefineProperties(Filer: TFiler);

  function HasFontData: Boolean;
  begin
    Result := IsFontStored and ((Filer.Ancestor = nil) or (TcxContainerStyle(Filer.Ancestor).Font.Handle <> Font.Handle));
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsFontAssigned', ReadIsFontAssigned, WriteIsFontAssigned, HasFontData);
end;

function TcxContainerStyle.BaseGetStyleController: TcxStyleController;
begin
  if IsBaseStyle then
    if FStyleController = GetDefaultStyleController then
      Result := nil
    else
      Result := FStyleController
  else
    Result := ParentStyle.StyleController;
end;

procedure TcxContainerStyle.BaseSetStyleController(Value: TcxStyleController);

  function CheckStyleController(AStyleController: TcxStyleController): Boolean;
  begin
    Result := False;
    if AStyleController.Style = Self then
      Exit;
    repeat
      AStyleController := AStyleController.Style.StyleController;
      if AStyleController = nil then
        Break;
      if AStyleController.Style = Self then
        Exit;
    until False;
    Result := True;
  end;

begin
  if not IsBaseStyle then
    ParentStyle.StyleController := Value
  else
  begin
    if FOwner = GetDefaultStyleController then
      Exit;
    if Value = nil then
      Value := GetDefaultStyleController;
    if (Value <> nil) and (not CheckStyleController(Value)) then
      Exit;

    if Value <> FStyleController then
    begin
      if FStyleController <> nil then
        FStyleController.RemoveListener(Self);
      FStyleController := Value;
      if FStyleController <> nil then
        FStyleController.AddListener(Self);
      ControllerChangedNotification(FStyleController);
    end;
  end;
end;

procedure TcxContainerStyle.Changed;
begin
  FModified := True;
  CheckChanges;
end;

procedure TcxContainerStyle.ControllerChangedNotification(AStyleController: TcxStyleController);
begin
  UpdateFont;
  Changed;
end;

procedure TcxContainerStyle.ControllerFreeNotification(AStyleController: TcxStyleController);
begin
  if AStyleController = ActiveStyleController then
    StyleController := nil;
end;

function TcxContainerStyle.DefaultBorderColor: TColor;
begin
  if State = csDisabled then
    Result := clBtnShadow
  else
    if IsBaseStyle then
      Result := clWindowFrame
    else
      Result := ParentStyle.BorderColor;
end;

function TcxContainerStyle.DefaultBorderStyle: TcxContainerBorderStyle;
const
  AStyleBorderStyles: array [TcxLookAndFeelKind] of TcxContainerBorderStyle =
    (cbsFlat, cbs3D, cbsUltraFlat, cbsOffice11);
  ABorderStyles: array [TcxContainerStateItem, TcxContainerBorderStyle] of TcxContainerBorderStyle = (
    (cbsNone, cbsSingle, cbsThick, cbsFlat, cbs3D, cbsUltraFlat, cbsOffice11),
    (cbsFlat, cbsThick, cbsThick, cbs3D, cbs3D, cbsUltraFlat, cbsOffice11),
    (cbsNone, cbsSingle, cbsThick, cbsFlat, cbs3D, cbsUltraFlat, cbsOffice11),
    (cbsFlat, cbsThick, cbsThick, cbs3D, cbs3D, cbsUltraFlat, cbsOffice11)
  );
var
  AState: TcxContainerStateItem;
begin
  if IsBaseStyle then
    Result := AStyleBorderStyles[LookAndFeel.Kind]
  else
  begin
    if HotTrack or (State = csDisabled) then
      AState := State
    else
      AState := csNormal;
    Result := ABorderStyles[AState, ParentStyle.BorderStyle];
  end;
end;

function TcxContainerStyle.DefaultColor: TColor;
var
  AIsDefaultParentColor: Boolean;
begin
  AIsDefaultParentColor := (Container = nil) or Container.DefaultParentColor;
  if IsBaseStyle then
  begin
    if AIsDefaultParentColor then
      Result := clBtnFace
    else
      Result := clWindow;
  end
  else
    if (State = csDisabled) and not AIsDefaultParentColor then
      Result := clBtnFace
    else
      Result := ParentStyle.Color;
end;

function TcxContainerStyle.DefaultDisabledTextColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxContainerStyle.DefaultEdges: TcxBorders;
begin
  Result := [bLeft, bTop, bRight, bBottom];
end;

function TcxContainerStyle.DefaultHotTrack: Boolean;
begin
  Result := True;
end;

function TcxContainerStyle.DefaultShadow: Boolean;
begin
  Result := False;
end;

function TcxContainerStyle.DefaultTextColor: TColor;
begin
  if State = csDisabled then
    Result := DefaultDisabledTextColor
  else
    if IsBaseStyle then
      Result := StyleData.Font.Color
    else
      Result := ParentStyle.TextColor;
end;

function TcxContainerStyle.DefaultTextStyle: TFontStyles;
begin
  if IsBaseStyle then
    Result := StyleData.Font.Style
  else
    Result := ParentStyle.TextStyle;
end;

function TcxContainerStyle.DefaultTransparentBorder: Boolean;
begin
  Result := True;
end;

procedure TcxContainerStyle.FontChanged(Sender: TObject);
begin
  if not IsFontAssignedValueLocked then
    Include(FAssignedValues, csvFont);
  Changed;
end;

function TcxContainerStyle.GetColor: TColor;
var
  AContainer: TcxContainer;
begin
  if DirectAccessMode then
    Result := StyleData.Color
  else
  begin
    AContainer := Container;
    if IsBaseStyle and (AContainer <> nil) and
      AContainer.ParentColor and (AContainer.Parent <> nil) then
        Result := TWinControlAccess(AContainer.Parent).Color
    else
      if not InternalGetColor(Result) then
        Result := DefaultColor;
  end;
end;

function TcxContainerStyle.GetDefaultStyleController: TcxStyleController;
begin
  Result := DefaultContainerStyleController;
end;

function TcxContainerStyle.GetStyleColor: TColor;
var
  AContainer: TcxContainer;
begin
  if FDirectAccessMode then
    Result := StyleData.Color
  else
    if not InternalGetColor(Result) then
      if not IsBaseStyle then
        Result := DefaultColor
      else
      begin
        AContainer := Container;
        if (AContainer <> nil) and not AContainer.IsInplace and
          AContainer.ParentColor and (AContainer.Parent <> nil) then
            Result := TWinControlAccess(AContainer.Parent).Color
        else
          Result := DefaultColor;
      end;
end;

function TcxContainerStyle.GetTextColor: TColor;
begin
  if DirectAccessMode then
    Result := StyleData.FontColor
  else
    if not InternalGetTextColor(Result) then
      Result := DefaultTextColor;
end;

function TcxContainerStyle.GetTextStyle: TFontStyles;
begin
  if DirectAccessMode then
    Result := StyleData.Font.Style
  else
    if not InternalGetTextStyle(Result) then
      Result := DefaultTextStyle;
end;

function TcxContainerStyle.InternalGetColor(var Color: TColor): Boolean;
begin
  Result := csvColor in FAssignedValues;
  if Result then
    Color := StyleData.Color
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetColor(Color);
end;

function TcxContainerStyle.InternalGetNotPublishedExtendedStyleValues: TcxContainerStyleValues;
begin
  Result := [csvEdges, csvFont, csvHotTrack, csvShadow, csvTransparentBorder];
end;

function TcxContainerStyle.IsBaseStyle: Boolean;
begin
  Result := ParentStyle = nil;
end;

function TcxContainerStyle.IsDestroying: Boolean;
begin
  Result := FIsDestroying;
end;

function TcxContainerStyle.IsFontAssignedValueLocked: Boolean;
begin
  Result := FFontAssignedValueLockCount > 0;
end;

procedure TcxContainerStyle.LockFontAssignedValue(ALock: Boolean);
begin
  if ALock then
    Inc(FFontAssignedValueLockCount)
  else
    if FFontAssignedValueLockCount > 0 then
      Dec(FFontAssignedValueLockCount);
end;

procedure TcxContainerStyle.UpdateFont;
var
  AFont: TFont;
begin
  if DirectAccessMode or (csvFont in AssignedValues) then
    Exit;
  LockFontAssignedValue(True);
  try
    if InternalGetFont(AFont) then
    begin
      StyleData.Font.Assign(AFont);
      if ActiveStyleController <> nil then
        StyleData.Font.Height := dxGetScaleFactor(Owner).Apply(AFont.Height, dxGetScaleFactor(ActiveStyleController));
    end
    else
      if (Container = nil) or not Container.ParentFont then
        RestoreFont(StyleData.Font);
  finally
    LockFontAssignedValue(False);
  end;
end;

function TcxContainerStyle.GetAssignedValues: TcxContainerStyleValues;
begin
  if DirectAccessMode then
    Result := [0..GetStyleValueCount - 1]
  else
    Result := FAssignedValues;
end;

function TcxContainerStyle.GetBorderColor: TColor;
begin
  if DirectAccessMode then
    Result := clDefault
  else
    if not InternalGetBorderColor(Result) then
      Result := DefaultBorderColor;
end;

function TcxContainerStyle.GetBorderStyle: TcxContainerBorderStyle;
begin
  if DirectAccessMode then
    if csvBorderStyle in FAssignedValues then
      Result := FBorderStyle
    else
      Result := DefaultBorderStyle
  else
    if not InternalGetBorderStyle(Result) then
      Result := DefaultBorderStyle;
end;

function TcxContainerStyle.GetEdges: TcxBorders;
begin
  if DirectAccessMode then
    Result := []
  else
    if not IsBaseStyle then
      Result := ParentStyle.Edges
    else
      if not InternalGetEdges(Result) then
        Result := DefaultEdges;
end;

function TcxContainerStyle.GetFont: TFont;
begin
  if IsBaseStyle then
    Result := StyleData.Font
  else
    Result := ParentStyle.Font;
end;

function TcxContainerStyle.GetHotTrack: Boolean;
begin
  if DirectAccessMode then
  begin
    if csvHotTrack in FAssignedValues then
      Result := FHotTrack
    else
      Result := DefaultHotTrack;
  end
  else
    if not IsBaseStyle then
      Result := ParentStyle.HotTrack
    else
      if not InternalGetHotTrack(Result) then
        Result := DefaultHotTrack;
end;

function TcxContainerStyle.GetShadow: Boolean;
begin
  if DirectAccessMode then
    Result := False
  else
    if not IsBaseStyle then
      Result := ParentStyle.Shadow
    else
      if not InternalGetShadow(Result) then
        Result := DefaultShadow;
end;

function TcxContainerStyle.GetTransparentBorder: Boolean;
begin
  if DirectAccessMode then
    Result := True
  else
    if not IsBaseStyle then
      Result := ParentStyle.TransparentBorder
    else
      if not InternalGetTransparentBorder(Result) then
        Result := DefaultTransparentBorder;
end;

function TcxContainerStyle.InternalGetBorderColor(var BorderColor: TColor): Boolean;
begin
  Result := csvBorderColor in FAssignedValues;
  if Result then
    BorderColor := FBorderColor
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetBorderColor(BorderColor);
end;

function TcxContainerStyle.InternalGetBorderStyle(
  var BorderStyle: TcxContainerBorderStyle): Boolean;
begin
  Result := csvBorderStyle in FAssignedValues;
  if Result then
    BorderStyle := FBorderStyle
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetBorderStyle(BorderStyle);
end;

function TcxContainerStyle.InternalGetEdges(var Edges: TcxBorders): Boolean;
begin
  Result := csvEdges in FAssignedValues;
  if Result then
    Edges := FEdges
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetEdges(Edges);
end;

function TcxContainerStyle.InternalGetFont(var Font: TFont): Boolean;
begin
  Result := csvFont in FAssignedValues;
  if Result then
    Font := StyleData.Font
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetFont(Font);
end;

function TcxContainerStyle.InternalGetHotTrack(var HotTrack: Boolean): Boolean;
begin
  Result := csvHotTrack in FAssignedValues;
  if Result then
    HotTrack := FHotTrack
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetHotTrack(HotTrack);
end;

function TcxContainerStyle.InternalGetShadow(var Shadow: Boolean): Boolean;
begin
  Result := csvShadow in FAssignedValues;
  if Result then
    Shadow := FShadow
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetShadow(Shadow);
end;

function TcxContainerStyle.InternalGetTextColor(var TextColor: TColor): Boolean;
begin
  Result := csvTextColor in FAssignedValues;
  if Result then
    TextColor := StyleData.FontColor
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetTextColor(TextColor);
end;

function TcxContainerStyle.InternalGetTextStyle(var TextStyle: TFontStyles): Boolean;
begin
  Result := csvTextStyle in FAssignedValues;
  if Result then
    TextStyle := FTextStyle
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetTextStyle(TextStyle);
end;

function TcxContainerStyle.InternalGetTransparentBorder(var TransparentBorder: Boolean): Boolean;
begin
  Result := csvTransparentBorder in FAssignedValues;
  if Result then
    TransparentBorder := FTransparentBorder
  else
    if ActiveStyleController <> nil then
      Result := ActiveStyleController.Styles[State].InternalGetTransparentBorder(TransparentBorder);
end;

function TcxContainerStyle.IsBorderColorStored: Boolean;
begin
  Result := (csvBorderColor in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('BorderColor', State <> csNormal));
end;

function TcxContainerStyle.IsBorderStyleStored: Boolean;
begin
  Result := (csvBorderStyle in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('BorderStyle', State <> csNormal));
end;

function TcxContainerStyle.IsColorStored: Boolean;
begin
  Result := (csvColor in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('Color', State <> csNormal));
end;

function TcxContainerStyle.IsEdgesStored: Boolean;
begin
  Result := (csvEdges in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('Edges', State <> csNormal));
end;

function TcxContainerStyle.IsFontStored: Boolean;
begin
  Result := (csvFont in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('Font', State <> csNormal));
end;

function TcxContainerStyle.IsHotTrackStored: Boolean;
begin
  Result := (csvHotTrack in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('HotTrack', State <> csNormal));
end;

function TcxContainerStyle.IsShadowStored: Boolean;
begin
  Result := (csvShadow in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('Shadow', State <> csNormal));
end;

function TcxContainerStyle.IsStyleControllerStored: Boolean;
begin
  Result := (State = csNormal);
end;

function TcxContainerStyle.IsTextColorStored: Boolean;
begin
  Result := (csvTextColor in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('TextColor', State <> csNormal));
end;

function TcxContainerStyle.IsTextStyleStored: Boolean;
begin
  Result := (csvTextStyle in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('TextStyle', State <> csNormal));
end;

function TcxContainerStyle.IsTransparentBorderStored: Boolean;
begin
  Result := (csvTransparentBorder in FAssignedValues) and ((Container = nil) or
    Container.IsStylePropertyPublished('TransparentBorder', State <> csNormal));
end;

procedure TcxContainerStyle.SetAssignedValues(Value: TcxContainerStyleValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    if IsBaseStyle then
      UpdateFont;
    Changed;
  end;
end;

procedure TcxContainerStyle.SetBorderColor(Value: TColor);
begin
  if (csvBorderColor in FAssignedValues) and (Value = FBorderColor) then
    Exit;
  FBorderColor := Value;
  Include(FAssignedValues, csvBorderColor);
  Changed;
end;

procedure TcxContainerStyle.SetBorderStyle(Value: TcxContainerBorderStyle);
begin
  if (csvBorderStyle in FAssignedValues) and (Value = FBorderStyle) then
    Exit;
  FBorderStyle := Value;
  Include(FAssignedValues, csvBorderStyle);
  Changed;
end;

procedure TcxContainerStyle.SetColor(Value: TColor);
begin
  if (csvColor in FAssignedValues) and (Value = StyleData.Color) then
    Exit;
  StyleData.Color := Value;
  Include(FAssignedValues, csvColor);
  Changed;
end;

procedure TcxContainerStyle.SetEdges(Value: TcxBorders);
begin
  if not IsBaseStyle then
    ParentStyle.Edges := Value
  else
  begin
    if (csvEdges in FAssignedValues) and (Value = FEdges) then
      Exit;
    FEdges := Value;
    Include(FAssignedValues, csvEdges);
    Changed;
  end;
end;

procedure TcxContainerStyle.SetFont(Value: TFont);
begin
  if DirectAccessMode then
    StyleData.Font := Value
  else
    if not IsBaseStyle then
      ParentStyle.Font := Value
    else
    begin
      StyleData.Font.Assign(Value);
      Include(FAssignedValues, csvFont);
      Changed;
    end;
end;

procedure TcxContainerStyle.SetHotTrack(Value: Boolean);
begin
  if not IsBaseStyle then
    ParentStyle.HotTrack := Value
  else
  begin
    if (csvHotTrack in FAssignedValues) and (Value = FHotTrack) then
      Exit;
    FHotTrack := Value;
    Include(FAssignedValues, csvHotTrack);
    Changed;
  end;
end;

procedure TcxContainerStyle.SetShadow(Value: Boolean);
begin
  if not IsBaseStyle then
    ParentStyle.Shadow := Value
  else
  begin
    if (csvShadow in FAssignedValues) and (Value = FShadow) then
      Exit;
    FShadow := Value;
    Include(FAssignedValues, csvShadow);
    Changed;
  end;
end;

procedure TcxContainerStyle.SetTextColor(Value: TColor);
begin
  if (csvTextColor in FAssignedValues) and (Value = TextColor) then
    Exit;
  StyleData.FontColor := Value;
  Include(FAssignedValues, csvTextColor);
  Changed;
end;

procedure TcxContainerStyle.SetTextStyle(Value: TFontStyles);
begin
  if (csvTextStyle in FAssignedValues) and (Value = TextStyle) then
    Exit;
  FTextStyle := Value;
  Include(FAssignedValues, csvTextStyle);
  Changed;
end;

procedure TcxContainerStyle.SetTransparentBorder(Value: Boolean);
begin
  if not IsBaseStyle then
    ParentStyle.TransparentBorder := Value
  else
  begin
    if (csvTransparentBorder in FAssignedValues) and (Value = FTransparentBorder) then
      Exit;
    FTransparentBorder := Value;
    Include(FAssignedValues, csvTransparentBorder);
    Changed;
  end;
end;

procedure TcxContainerStyle.CheckChanges;
begin
  if FModified and (FUpdateCount = 0) then
  begin
    FModified := False;
    if not IsDestroying and not DirectAccessMode and Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TcxContainerStyle.CreateFont;
begin
  if not DirectAccessMode and IsBaseStyle then
  begin
    StyleData.Font := TFont.Create;
    StyleData.Font.OnChange := FontChanged;
  end
  else
    StyleData.Font := nil;
end;

function TcxContainerStyle.GetActiveStyleController: TcxStyleController;
begin
  if IsBaseStyle then
    Result := FStyleController
  else
    Result := ParentStyle.FStyleController;
end;

function TcxContainerStyle.GetBaseStyle: TcxContainerStyle;
begin
  if IsBaseStyle then
    Result := Self
  else
    Result := ParentStyle;
end;

function TcxContainerStyle.GetContainer: TcxContainer;
begin
  if FOwner is TcxContainer then
    Result := TcxContainer(FOwner)
  else
    Result := nil;
end;

function TcxContainerStyle.GetLookAndFeel: TcxLookAndFeel;
begin
  if IsBaseStyle then
    Result := FLookAndFeel
  else
    Result := ParentStyle.LookAndFeel;
end;

procedure TcxContainerStyle.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  Changed;
end;

procedure TcxContainerStyle.ReadIsFontAssigned(Reader: TReader);
begin
  Reader.ReadBoolean;
  AssignedValues := AssignedValues + [csvFont];
end;

procedure TcxContainerStyle.RestoreFont(AFont: TFont);
begin
  cxResetFont(AFont);
end;

procedure TcxContainerStyle.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  if IsBaseStyle then
    FLookAndFeel.Assign(Value)
  else
    ParentStyle.LookAndFeel := Value;
end;

procedure TcxContainerStyle.UpdateVisibleFont;
begin
  FVisibleFont.Assign(Font);
  FVisibleFont.Color := TextColor;
  FVisibleFont.Style := TextStyle;
end;

procedure TcxContainerStyle.WriteIsFontAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

{ TcxContainer }

constructor TcxContainer.Create(AOwner: TComponent);
var
  AColor: TColor;
begin
  inherited Create(AOwner);
  FIsCreating := True;

  FSizeGripData.Visible := False;

  FStyles := GetStylesClass.Create(Self, GetStyleClass);
  FStyles.OnChanged := ContainerStyleChanged;

  FActiveStyleData.ContainerState := [csNormal];
  FActiveStyleData.ActiveStyle := FStyles.Style;

  FViewInfo := GetViewInfoClass.Create;
  FViewInfo.Owner := Self;
  LookAndFeel.MasterLookAndFeel := FStyles.Style.LookAndFeel;

  ControlStyle := ControlStyle + [csSetCaption, csCaptureMouse];
  TabStop := True;
  ParentColor := DefaultParentColor and not IsInplace and
    not Style.InternalGetColor(AColor);
  if not ParentColor then
    Color := clWindow;
  ClearSavedChildControlRegions;

  FIsCreating := False;
end;

destructor TcxContainer.Destroy;
begin
  if FNewWindowRegion <> 0 then
  begin
    DeleteObject(FNewWindowRegion);
    FNewWindowRegion := 0;
  end;
  cxControls.EndMouseTracking(Self);
  FreeAndNil(FViewInfo);
  FStyles.OnChanged := nil;
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TcxContainer.Focused: Boolean;
begin
  if IsDesigning or (csDestroyingHandle in ControlState) then
    Result := False
  else
  begin
    Result := inherited Focused;
    Result := Result or HasInnerControl and InnerControl.Focused;
  end;
end;

function TcxContainer.GetDragImages: TDragImageList;
begin
  if HasInnerControl then
    Result := TWinControlAccess(InnerControl).GetDragImages
  else
    Result := inherited GetDragImages;
end;

procedure TcxContainer.GetTabOrderList(List: TList);
var
  AActiveControl: TWinControl;
begin
  if IsContainerClass then
    inherited GetTabOrderList(List)
  else
  begin
    AActiveControl := GetParentForm(Self).ActiveControl;
    if (AActiveControl <> Self) and CanFocus and HasInnerControl and
      (InnerControl = AActiveControl) then
    begin
      List.Add(InnerControl);
      List.Remove(Self);
    end;
  end;
end;

procedure TcxContainer.SetFocus;
var
  AParentForm: TCustomForm;
begin
  if IsDesigning or IsContainerFocused then
    Exit;
  inherited SetFocus;
  if HasInnerControlHandle and (GetFocus = Handle) then
  begin
    AParentForm := GetParentForm(Self);
    SafeSelectionFocusInnerControl;
    AParentForm.FocusControl(InnerControl);
  end;
end;

procedure TcxContainer.ClearSavedChildControlRegions;
begin
  FInnerControlBounds.IsEmpty := True;
end;

function TcxContainer.GetVisibleBounds: TRect;
var
  ABorderWidth, ABorderMaskedPartWidth: Integer;
begin
  Result := GetControlRect(Self);
  if IsInplace then
    Exit;
  try
    if ViewInfo.Shadow then
    begin
      Dec(Result.Right, cxContainerShadowWidth);
      Dec(Result.Bottom, cxContainerShadowWidth);
    end;

    if not Style.HasBorder then
    begin
      if Style.TransparentBorder then
        InflateRect(Result, -cxContainerMaxBorderWidth, -cxContainerMaxBorderWidth);
      Exit;
    end;
    if ViewInfo.NativeStyle then
    begin
      if Style.TransparentBorder and (Style.BorderStyle = cbsNone) then
        InflateRect(Result, -cxContainerMaxBorderWidth, -cxContainerMaxBorderWidth);
      Exit;
    end;
    if not Style.TransparentBorder then
      Exit;

    ABorderWidth := GetContainerBorderWidth(ViewInfo.BorderStyle);
    ABorderMaskedPartWidth := cxContainerMaxBorderWidth - ABorderWidth;
    InflateRect(Result, -ABorderMaskedPartWidth, -ABorderMaskedPartWidth);
    InflateRectByBorders(Result, ABorderWidth, ViewInfo.Edges);
  finally
    if Result.Top > Result.Bottom then
      Result := Rect(Result.Left, 0, Result.Right, 0);
  end;
end;

function TcxContainer.HasInnerControl: Boolean;
begin
  Result := FInnerControl <> nil;
end;

function TcxContainer.HasInnerControlHandle: Boolean;
begin
  Result := HasInnerControl and InnerControl.HandleAllocated;
end;

function TcxContainer.HasPopupWindow: Boolean;
begin
  Result := False;
end;

function TcxContainer.InnerControlDefaultHandler(var Message: TMessage): Boolean;
begin
  Result := HasInnerControlHandle and not (csDestroying in ComponentState) and DoInnerControlDefaultHandler(Message);
end;

function TcxContainer.InnerControlMenuHandler(var Message: TMessage): Boolean;
begin
  case Message.Msg of
    CN_KEYDOWN, CN_SYSKEYDOWN:
      begin
        Message.Result := 1;
        Result := IsMenuKey(TWMKey(Message));
        if not Result then
          Message.Result := 0;
      end;
    else
      Result := False;
  end;
end;

procedure TcxContainer.Invalidate;
begin
  inherited Invalidate;
  if HasInnerControl then
    InnerControl.Invalidate;
end;

procedure TcxContainer.Update;
begin
  inherited Update;
  if HasInnerControl then
    InnerControl.Update;
end;

function TcxContainer.IsInplace: Boolean;
begin
  Result := False;
end;

function TcxContainer.IsStylePropertyPublished(const APropertyName: string;
  AExtendedStyle: Boolean): Boolean;
var
  AStyleValue: TcxContainerStyleValue;
begin
  if AExtendedStyle then
    Result := Style.IsExtendedStylePropertyPublished(APropertyName)
  else
    Result := True;
  if Result and Style.GetStyleValue(APropertyName, AStyleValue) then
    Result := not(AStyleValue in InternalGetNotPublishedStyleValues);
end;

procedure TcxContainer.LockAlignControls(ALock: Boolean);
begin
  if ALock then
    Inc(FLockAlignControlsCount)
  else
    if FLockAlignControlsCount > 0 then
      Dec(FLockAlignControlsCount);
end;

procedure TcxContainer.LockPopupMenu(ALock: Boolean);
begin
  if ALock then
    Inc(FPopupMenuLockCount)
  else
    if FPopupMenuLockCount > 0 then
      Dec(FPopupMenuLockCount);
end;

procedure TcxContainer.RestoreStyles;
begin
  FStyles.RestoreDefaults;
end;

procedure TcxContainer.SetScrollBarsParameters(AIsScrolling: Boolean = False);

  procedure SetScrollBarParameters(AScrollBar: IcxControlScrollBar);
  var
    AScrollBarVisible: Boolean;
    AScrollInfo: TScrollInfo;
  begin

    if not IsPopupScrollBars then
    begin
      if not AIsScrolling then
        AdjustScrollBarPosition(AScrollBar);
      if not AScrollBar.Visible then
        Exit;
    end
    else
      if not AScrollBar.Data.Visible then
      begin
        AScrollBar.ApplyData;
        Exit;
      end;

    GetStandardScrollBarParameters(AScrollBar.Kind, AScrollInfo);
    if not UseSystemRightToLeftLayoutForInnerControl and (AScrollBar.Kind = sbHorizontal) then
      AScrollBar.Control.BiDiMode := bdLeftToRight;
    if IsPopupScrollBars then
    begin
      AScrollBarVisible := AScrollBar.Data.Visible;
      SetScrollBarInfo(AScrollBar.Kind, AScrollInfo.nMin, AScrollInfo.nMax, AScrollBar.SmallChange,
        AScrollInfo.nPage, AScrollInfo.nPos, True, True);
      AScrollBar.Data.Visible := AScrollBarVisible;
      AScrollBar.ApplyData;
    end
    else
      AScrollBar.SetScrollParams(AScrollInfo.nMin, AScrollInfo.nMax, AScrollInfo.nPos, AScrollInfo.nPage, True);
  end;

  procedure GetSizeGripData(out ASizeGripData: TcxContainerSizeGripData);
  var
    AIContainerInnerControl: IcxContainerInnerControl;
  begin
    ASizeGripData.Visible := HScrollBar.Visible and VScrollBar.Visible and
      HasInnerControlHandle and Supports(InnerControl, IcxContainerInnerControl, AIContainerInnerControl);
    if ASizeGripData.Visible then
      ASizeGripData.Bounds := GetSizeGripRect(AIContainerInnerControl);
  end;

  function NeedsRepaintSizeGrip(
    const APrevSizeGripData, ASizeGripData: TcxContainerSizeGripData): Boolean;
  begin
    Result := not APrevSizeGripData.Visible and ASizeGripData.Visible or
      APrevSizeGripData.Visible and ASizeGripData.Visible and
      not EqualRect(APrevSizeGripData.Bounds, ASizeGripData.Bounds);
  end;

  procedure RepaintSizeGrip(const ASizeGripRect: TRect);
  var
    ARgn: HRGN;
  begin
    ARgn := CreateRectRgnIndirect(ASizeGripRect);
    SendMessage(InnerControl.Handle, WM_NCPAINT, ARgn, 0);
    DeleteObject(ARgn);
  end;

var
  ASizeGripData: TcxContainerSizeGripData;
begin
{$IFDEF USETCXSCROLLBAR}
  if not HasInnerControl or not NeedsScrollBars or IsDestroying or not UsecxScrollBars then
    Exit;
  FScrollBarsCalculating := True;
  try
    if IsPopupScrollBars and not AIsScrolling then
      AdjustScrollBarPositions;
    SetScrollBarParameters(HScrollBar);
    SetScrollBarParameters(VScrollBar);
    if IsPopupScrollBars then
      SizeGrip.Visible := IsSizeGripVisible
    else
    begin
      GetSizeGripData(ASizeGripData);
      if NeedsRepaintSizeGrip(FSizeGripData, ASizeGripData) then
        RepaintSizeGrip(ASizeGripData.Bounds);
      FSizeGripData := ASizeGripData;
    end;
  finally
    FScrollBarsCalculating := False;
  end;
{$ENDIF}
end;

function TcxContainer.ShortRefreshContainer(AIsMouseEvent: Boolean): Boolean;
var
  ACursorPos: TPoint;
  AShift: TShiftState;
begin
  if CanRefreshContainer and HandleAllocated and IsChildEx(Handle, WindowFromPoint(GetMouseCursorPos)) then
    ACursorPos := GetMouseCursorClientPos
  else
    ACursorPos := cxInvalidPoint;

  if CanRefreshContainer then
    AShift := KeyboardStateToShiftState
  else
    AShift := [];

  Result := RefreshContainer(ACursorPos, cxmbNone, AShift, AIsMouseEvent);
end;

procedure TcxContainer.UpdateScrollBarsParameters;
begin
  if not IsDestroying and HandleAllocated and HasInnerControl then
    PostMessage(Handle, DXM_UPDATESCROLLBARS, 0, 0);
end;

procedure TcxContainer.TranslationChanged;
begin
  inherited;
  ShortRefreshContainer(False);
end;

procedure TcxContainer.MouseTrackingCallerMouseLeave;
begin
  EndMouseTracking;
end;

function TcxContainer.GetLookAndFeelValue: TcxLookAndFeel;
begin
  Result := Style.LookAndFeel;
end;

procedure TcxContainer.AdjustClientRect(var Rect: TRect);
begin
  if not IsDestroying and not IsLoading and ViewInfo.FCalculated then
    Rect := ViewInfo.ClientRect;
end;

procedure TcxContainer.AlignControls(AControl: TControl; var Rect: TRect);
var
  AInnerControlBounds: TcxContainerInnerControlBounds;
begin
  if IsContainerClass then
    inherited AlignControls(AControl, Rect)
  else
    if not IsAlignControlsLocked and not IsInternalControl(AControl) and (NeedsScrollBars or (AControl <> nil)) then
      if AControl = nil then
        inherited AlignControls(AControl, Rect)
      else
        if not FIsCreating and (AControl is TWinControl) and TWinControl(AControl).HandleAllocated then
        begin
          CorrectAlignControlRect(Rect);
          AInnerControlBounds := GetInnerControlBounds(Rect, AControl);
          if IsInnerControlBoundsChanged(TWinControl(AControl), AInnerControlBounds) then
          begin
            SetWindowRegion(TWinControl(AControl), AInnerControlBounds);
            SaveInnerControlBounds(TWinControl(AControl), AInnerControlBounds);
          end;
        end;
end;

function TcxContainer.AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxContainer.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and not (HasInnerControlHandle and FInnerControl.Focused or InnerControlMouseDown);
end;

procedure TcxContainer.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);

  function NeedFontScaling: Boolean;
  begin
    Result := (M <> D) and (not IsLoading or (sfFont in ScalingFlags)) and (csvFont in Style.AssignedValues);
  end;

var
  ANeedFontScaling: Boolean;
  AOriginalParentSize: TPoint;
begin
  ANeedFontScaling := NeedFontScaling;

  if ParentFont then
    inherited ChangeScaleEx(M, D, isDpiChange)
  else
  begin
    Inc(FInternalSetting);
    try
      inherited ChangeScaleEx(M, D, isDpiChange);
    finally
      Dec(FInternalSetting);
    end;
  end;

  AOriginalParentSize := FOriginalParentSize;
  if ANeedFontScaling then
  begin
  {$IFDEF DELPHIXE8}
    Style.Font.Height := MulDiv(Style.Font.Height, M, D);
  {$ELSE}
    Style.Font.Size := MulDiv(Style.Font.Size, M, D);
  {$ENDIF}
  end
  else
    Style.UpdateFont;
  FOriginalParentSize := AOriginalParentSize;
end;

procedure TcxContainer.ColorChanged;
begin
  if FInternalSetting = 0 then
    FStyles.Style.Color := Color
  else
  begin
    inherited ColorChanged;
    ShortRefreshContainer(False);
  end;
end;

procedure TcxContainer.CorrectAlignControlRect(var R: TRect);
begin
end;

procedure TcxContainer.CursorChanged;
begin
  inherited CursorChanged;
  if HasInnerControl then
    FInnerControl.Cursor := Cursor;
end;

procedure TcxContainer.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if not IsPopupMenuLocked and ((MousePos.X = -1) and (MousePos.Y = -1) or CanShowPopupMenu(MousePos)) then
    inherited DoContextPopup(MousePos, Handled)
  else
    Handled := True;
end;

procedure TcxContainer.DragCanceled;
begin
  inherited DragCanceled;
  if HasInnerControl then
    TWinControlAccess(InnerControl).DragCanceled;
end;

procedure TcxContainer.FocusChanged;
begin
  inherited FocusChanged;
  ShortRefreshContainer(False);
end;

function TcxContainer.FocusWhenChildIsClicked(AChild: TControl): Boolean;
begin
  Result := False;
end;

function TcxContainer.GetClientBounds: TRect;
begin
  Result := ViewInfo.ClientRect;
end;

function TcxContainer.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := inherited IsGestureTarget(AWnd) or
    HandleInnerContolGestures and (InnerControl.Handle = AWnd);
end;

function TcxContainer.IsContainerFocused: Boolean;
begin
  Result := Focused;
end;

procedure TcxContainer.KeyDown(var Key: Word; Shift: TShiftState);
var
  AParentForm: TCustomForm;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_TAB:
      if Focused and (Shift * [ssAlt, ssCtrl] = []) and CanContainerHandleTabs then
      begin
        Key := 0;
        AParentForm := GetParentForm(Self);
        TWinControlAccess(AParentForm).SelectNext(AParentForm.ActiveControl,
          not(ssShift in Shift), True);
        if HandleAllocated and HasInnerControl then
          if GetFocus = Handle then
            InnerControl.SetFocus;
      end;
  end;
end;

procedure TcxContainer.Loaded;
begin
  inherited Loaded;
  SetSize;
  ContainerStyleChanged(FStyles.Style);
end;

function TcxContainer.MayFocus: Boolean;
begin
  Result := not (HasInnerControl and InnerControl.Focused);
end;

procedure TcxContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if IsMouseTracking then
  begin
    FShiftState := Shift;
    RefreshContainer(Point(X, Y), ButtonTocxButton(Button), FShiftState, True);
  end;
end;

procedure TcxContainer.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  ShortRefreshContainer(True);
  BeginMouseTracking(Self, Bounds, Self);
end;

procedure TcxContainer.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  FShiftState := [];
  ShortRefreshContainer(True);
  cxControls.EndMouseTracking(Self);
end;

procedure TcxContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  RefreshContainer(Point(X, Y), cxmbNone, Shift, True);
  BeginMouseTracking(Self, Bounds, Self);
end;

procedure TcxContainer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FShiftState := Shift;
  RefreshContainer(Point(X, Y), ButtonTocxButton(Button), FShiftState, True);
end;

function TcxContainer.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TcxContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = InnerControl) then
    InnerControl := nil;
end;

procedure TcxContainer.Paint;
begin
  if RectVisible(Canvas.Handle, ViewInfo.Bounds) then
  begin
    if csPaintCopy in ControlState then
      SetVisibleBoundsClipRect;
    CheckIsViewInfoCalculated;
    if IsViewInfoCalculated then
      ViewInfo.Paint(Canvas);
  end;
  UpdateInternalControlsState;
end;

procedure TcxContainer.Resize;
begin
  inherited Resize;
  if IsPopupScrollBars and HasVisibleTouchScrollUI then
    UpdateScrollBarsParameters;
end;

procedure TcxContainer.SetDragMode(Value: TDragMode);
begin
  inherited SetDragMode(Value);
  if HasInnerControl then
    TWinControlAccess(InnerControl).DragMode := Value;
end;

procedure TcxContainer.SetParent(AParent: TWinControl);
begin
  if CanDisableAlignOnCreateInnerControl then
  begin
    DisableAlign;
    BeginRefreshContainer;
    try
      inherited;
    finally
      EndRefreshContainer;
      EnableAlign;
    end;
  end
  else
    inherited;
end;

procedure TcxContainer.CreateHandle;
begin
  if not CanDisableAlignOnCreateInnerControl then
  begin
    inherited CreateHandle;
    if HasInnerControl and CanAllocateHandle(InnerControl) then
      InnerControl.HandleNeeded;
  end
  else
  begin
    DisableAlign;
    BeginRefreshContainer;
    try
      inherited CreateHandle;
      if HasInnerControl and CanAllocateHandle(InnerControl) then
        InnerControl.HandleNeeded;
    finally
      EndRefreshContainer;
      EnableAlign;
    end;
  end;
end;

procedure TcxContainer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not(CS_HREDRAW or CS_VREDRAW);
end;

procedure TcxContainer.CreateWindowHandle(const Params: TCreateParams);
var
  AParams: TCreateParams;
begin
  if Length(Params.Caption) > $FFFF then
  begin
    AParams := Params;
    AParams.Caption := '';
    inherited CreateWindowHandle(AParams);
    if HandleAllocated then
      CallWindowProc(DefWndProc, Handle, WM_SETTEXT, 0, LPARAM(WindowText));
  end
  else
    inherited CreateWindowHandle(Params);
end;

procedure TcxContainer.DestroyWnd;
begin
{$IFDEF VCLGLASSPAINT}
  dxForceProcessBufferedPaintMessages(InnerControl);
  dxForceProcessBufferedPaintMessages(Self);
{$ELSE}
  FInnerControlBufferedPaint := False;
  FRepaintOnGlass := False;
{$ENDIF}
  inherited DestroyWnd;
end;

procedure TcxContainer.UpdateScrollBars;
begin
  if UseInnerControlScrollBarParameters then
    SetScrollBarsParameters
  else
    inherited UpdateScrollBars;
end;

function TcxContainer.UseSystemRightToLeftLayoutForInnerControl: Boolean;
begin
  Result := False;
end;

function TcxContainer.UseInnerControlScrollBarParameters: Boolean;
begin
  Result := HasInnerControl;
end;

procedure TcxContainer.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CHAR, WM_KEYDOWN, WM_KEYUP, CN_CHAR, CN_KEYDOWN, CN_KEYUP:
      if HasInnerControl then
      begin
        with TMessage(Message) do
          Result := SendMessage(InnerControl.Handle, Msg, WParam, LParam);
        Exit;
      end;
    WM_SETFOCUS:
      if not Visible then
        Exit;
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if (DragMode = dmAutomatic) and not IsDesigning then
      begin
        BeginAutoDrag;
        Exit;
      end;
  {$IFNDEF VCLGLASSPAINT}
    WM_PAINT:
      begin
        if OnGlass and IsCompositionEnabled then
        begin
          dxPaintWindowOnGlass(Handle);
          Message.Result := 0;
          Exit;
        end;
      end;
    CN_CTLCOLOREDIT, CN_CTLCOLORSTATIC:
      begin
        inherited WndProc(Message);
        if not FRepaintOnGlass and OnGlass and IsCompositionEnabled then
        begin
          FRepaintOnGlass := True;
          PostMessage(Handle, DXM_BUFFEREDPAINTONGLASS, 0, 0);
        end;
        Exit;
      end;
    DXM_BUFFEREDPAINTONGLASS:
      if FRepaintOnGlass then
      begin
        dxDrawWindowOnGlass(Handle);
        FRepaintOnGlass := False;
        Exit;
      end;
  {$ENDIF}
    WM_NCCALCSIZE:
      if FRefreshLockCount > 0 then
      begin
        Message.Result := 0;
        Exit;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TcxContainer.AdjustInnerControl;
begin
  if HasInnerControl then
    with TWinControlAccess(FInnerControl) do
    begin
      Color := ViewInfo.BackgroundColor;
      Font := GetVisibleFont;
    end;
end;

procedure TcxContainer.CalculateViewInfo(const P: TPoint; AMouseTracking: Boolean);

  function GetContainerState: TcxContainerState;
  begin
    if Enabled then
      if IsDesigning then
        Result := [csNormal]
      else
      begin
        if Focused then
          Result := [csActive]
        else
          Result := [csNormal];
        if PtInRect(GetVisibleBounds, P) and AMouseTracking then
          Include(Result, csHotTrack);
      end
    else
      Result := [csDisabled];
  end;

  procedure CalculateContainerState;
  var
    ASelected: Boolean;
  begin
    ViewInfo.ContainerState := GetContainerState;

    ASelected := ViewInfo.ContainerState * [csActive, csHotTrack] <> [];
    if not ActiveStyle.HotTrack then
      ViewInfo.HotState := chsNoHotTrack
    else
      if ASelected then
        ViewInfo.HotState := chsSelected
      else
        ViewInfo.HotState := chsNormal;

    if ViewInfo.NativeStyle then
      ViewInfo.BorderStyle := Style.BorderStyle
    else
    begin
      ViewInfo.BorderStyle := ActiveStyle.BorderStyle;
      if ViewInfo.BorderStyle in [cbsUltraFlat, cbsOffice11] then
        ViewInfo.BorderStyle := cbsSingle;
    end;
  end;

var
  APrevBorderWidth: Integer;
begin
  ViewInfo.FCalculated := True;

  ViewInfo.NativeStyle := IsNativeStyle;
  ViewInfo.UpdateStyle(Style);
  APrevBorderWidth := GetContainerBorderWidth(ViewInfo.BorderStyle);
  CalculateContainerState;
  if not ViewInfo.NativeStyle and
    (GetContainerBorderWidth(ViewInfo.BorderStyle) < APrevBorderWidth) then
      CalculateContainerState;

  if ViewInfo.NativeStyle then
  begin
    ViewInfo.ThemedObjectType := GetBackgroundThemedObjectType;
    ViewInfo.NativePart := GetBackgroundNativePart;
    ViewInfo.NativeState := GetBackgroundNativeState;
  end;

  ViewInfo.Bounds := GetControlRect(Self);
  ViewInfo.BorderRect := cxRectContent(ViewInfo.Bounds, GetBorderExtent);
  ViewInfo.ClientRect := ViewInfo.BorderRect;
  ViewInfo.BorderWidth := ViewInfo.Painter.GetContainerBorderWidth(ViewInfo.BorderStyle);
  ViewInfo.Edges := ActiveStyle.Edges;
  ViewInfo.Shadow := HasShadow;
  ViewInfo.BorderColor := GetBorderColor;
  ViewInfo.BackgroundColor := GetBackgroundColor;
end;

function TcxContainer.CanContainerHandleTabs: Boolean;
begin
  Result := True;
end;

function TcxContainer.CanHaveTransparentBorder: Boolean;
begin
  Result := not (ViewInfo.NativeStyle and (ViewInfo.BorderStyle <> cbsNone)) and ActiveStyle.TransparentBorder;
end;

function TcxContainer.CanRefreshContainer: Boolean;
begin
  Result := (FRefreshLockCount = 0) and
    not FIsCreating and not IsDestroying and not IsLoading and
    not (csDestroyingHandle in ControlState) and HandleAllocated;
end;

function TcxContainer.CanShowPopupMenu(const P: TPoint): Boolean;
begin
  Result := True;//PtInRect(ViewInfo.ClientRect, P);
end;

procedure TcxContainer.CheckIsViewInfoCalculated;
begin
  if not IsViewInfoCalculated then
    ShortRefreshContainer(False);
end;

procedure TcxContainer.ContainerStyleChanged(Sender: TObject);
begin
  if FIsCreating or IsLoading then
    Exit;

  BeginRefreshContainer;
  try
    if not ParentColor or (csvColor in Style.AssignedValues) then
    begin
      Inc(FInternalSetting);
      try
        Color := Style.GetStyleColor;
      finally
        Dec(FInternalSetting);
      end;
    end;

    Inc(FInternalSetting);
    try
      Font := Style.Font;
    finally
      Dec(FInternalSetting);
    end;

    if Style.IsValueAssigned(csvFont) then
      ParentFont := False;
  finally
    EndRefreshContainer;
  end;
end;

function TcxContainer.CreateWindowRegion: TcxRegionHandle;
var
  ARegion: TcxRegionHandle;
begin
  Result := GetWindowShadowRegion(Handle, GetShadowBounds, GetShadowBoundsExtent,
    ViewInfo.NativeStyle and (ViewInfo.BorderStyle <> cbsNone), ViewInfo.Shadow,
    cxEmptyRect);

  if (Result = 0) and CanHaveTransparentBorder then
    Result := CreateRectRgnIndirect(GetShadowBounds);

  if Result <> 0 then
  begin
    ARegion := CreateRectRgnIndirect(GetWindowRegionAddon);
    CombineRgn(Result, Result, ARegion, RGN_OR);
    DeleteObject(ARegion);
  end;
end;

procedure TcxContainer.DataChange;
begin
end;

procedure TcxContainer.DataSetChange;
begin
  ShortRefreshContainer(False);
end;

function TcxContainer.DefaultParentColor: Boolean;
begin
  Result := False;
end;

function TcxContainer.DoInnerControlDefaultHandler(var Message: TMessage): Boolean;
begin
  Result := TcxControlDefaultHandlerHelper.Process(Message);
{$IFNDEF VCLGLASSPAINT}
  case Message.Msg of
    CN_CTLCOLOREDIT:
      if OnGlass and IsCompositionEnabled and not FInnerControlBufferedPaint then
      begin
        FInnerControlBufferedPaint := True;
        PostMessage(InnerControl.Handle, DXM_BUFFEREDPAINTONGLASS, 0, 0);
      end;

    DXM_BUFFEREDPAINTONGLASS:
      if FInnerControlBufferedPaint then
      try
        dxDrawWindowOnGlass(Handle);
      finally
        FInnerControlBufferedPaint := False;
      end;
  end;
{$ENDIF}
end;

procedure TcxContainer.DoProcessEventsOnViewInfoChanging;
begin
end;

function TcxContainer.DoRefreshContainer(const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  CalculateViewInfo(P, cxShiftStateMoveOnly(Shift) and ActiveStyle.HotTrack);
  SetSize;
  AdjustInnerControl;
  UpdateWindowRegion;
  if not HasInnerControl then
    InvalidateRect(GetControlRect(Self), False)
  else
    InternalInvalidate(Handle, GetControlRect(Self), ViewInfo.BorderRect, False);

  if csHotTrack in ViewInfo.ContainerState then
    BeginMouseTracking(Self, Bounds, Self);
  Result := True;
end;

procedure TcxContainer.EnabledChanged;
var
  I: Integer;
begin
  if not Enabled and Focused then
    Windows.SetFocus(0);

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TcxScrollBar then
      TcxScrollBar(Controls[I]).Enabled := Enabled;
  if not Enabled then
    HideTouchScrollUI(Self, True);
  ShortRefreshContainer(False);
end;

procedure TcxContainer.EndMouseTracking;
begin
  MouseLeave(nil);
  cxControls.EndMouseTracking(Self);
end;

function TcxContainer.GetActiveStyle: TcxContainerStyle;
begin
  if FActiveStyleData.ContainerState = ViewInfo.ContainerState then
    Result := FActiveStyleData.ActiveStyle
  else
  begin
    FActiveStyleData.ContainerState := ViewInfo.ContainerState;
    FActiveStyleData.ActiveStyle := InternalGetActiveStyle;
    Result := FActiveStyleData.ActiveStyle;
  end;
end;

function TcxContainer.GetBackgroundColor: TColor;
var
  AColor: COLORREF;
  ATextColor: TColor;
begin
  GetColorSettingsByPainter(Result, ATextColor);
  if Result = clDefault then
  begin
    Result := ActiveStyle.Color;
    if ViewInfo.NativeStyle and not (Enabled or ActiveStyle.IsValueAssigned(csvColor)) and
       (GetThemeColor(OpenTheme(ViewInfo.ThemedObjectType), ViewInfo.NativePart,
         ViewInfo.NativeState, TMT_FILLCOLOR, AColor) = S_OK)
    then
      Result := AColor;
  end;
end;

function TcxContainer.GetBorderColor: TColor;
var
  AIsHighlightBorder: Boolean;
begin
  AIsHighlightBorder := (csActive in ViewInfo.ContainerState) or
    (csHotTrack in ViewInfo.ContainerState) and ActiveStyle.HotTrack or
    IsDesigning and Enabled;

  Result := GetBorderColorByPainter(AIsHighlightBorder);
  if Result = clDefault then
  begin
    if ActiveStyle.BorderStyle in [cbsUltraFlat, cbsOffice11] then
      Result := GetContainerBorderColor(AIsHighlightBorder, ActiveStyle.BorderStyle = cbsOffice11)
    else
      Result := ActiveStyle.BorderColor
  end;
end;

function TcxContainer.GetBorderExtent: TRect;
var
  ABorderWidth: Integer;
  ANativeStyle: Boolean;
begin
  ANativeStyle := IsNativeStyle;
  if ActiveStyle.TransparentBorder then
    Result := cxContainerDefaultBorderExtent
  else
    if not ActiveStyle.HasBorder or ANativeStyle and (Style.BorderStyle = cbsNone) then
      Result := cxEmptyRect
    else
      if ANativeStyle then
        Result := cxContainerDefaultBorderExtent
      else
      begin
        ABorderWidth := GetContainerBorderWidth(ViewInfo.BorderStyle);
        Result := cxEmptyRect;
        if bLeft in ActiveStyle.Edges then
          Result.Left := ABorderWidth;
        if bTop in ActiveStyle.Edges then
          Result.Top := ABorderWidth;
        if bRight in ActiveStyle.Edges then
          Result.Right := ABorderWidth;
        if bBottom in ActiveStyle.Edges then
          Result.Bottom := ABorderWidth;
      end;
  if HasShadow then
  begin
    Inc(Result.Right, cxContainerShadowWidth);
    Inc(Result.Bottom, cxContainerShadowWidth);
  end;
end;

function TcxContainer.IsStyleAssigned(AValue: TcxContainerStyleValue): Boolean;
begin
  Result := Style.IsValueAssigned(AValue) or ActiveStyle.IsValueAssigned(AValue);
end;

function TcxContainer.GetEditStateColorKind: TcxEditStateColorKind;
begin
  if not Enabled then
    Result := esckDisabled
  else
    if Focused then
      Result := esckNormal
    else
      Result := esckInactive;
end;

function TcxContainer.GetInnerControlBounds(const AInnerControlsRegion: TRect;
  AInnerControl: TControl): TcxContainerInnerControlBounds;
var
  R: TRect;
  AInnerRect: TRect;
begin
  if AInnerControl = nil then
  begin
    Result.IsEmpty := True;
    Exit;
  end;

  Result.IsEmpty := False;
  Result.Rect := GetControlRect(AInnerControl);
  R := Result.Rect;
  AInnerRect := AInnerControl.BoundsRect;
  if AInnerRect.Left < AInnerControlsRegion.Left then
    Result.Rect.Left := AInnerControlsRegion.Left - AInnerRect.Left;
  if AInnerRect.Top < AInnerControlsRegion.Top then
    Result.Rect.Top := AInnerControlsRegion.Top - AInnerRect.Top;
  if AInnerRect.Right > AInnerControlsRegion.Right then
    Dec(Result.Rect.Right, AInnerRect.Right - AInnerControlsRegion.Right);
  if AInnerRect.Bottom > AInnerControlsRegion.Bottom then
    Dec(Result.Rect.Bottom, AInnerRect.Bottom - AInnerControlsRegion.Bottom);
  if EqualRect(Result.Rect, R) then
    Result.IsEmpty := True;
end;

function TcxContainer.GetScrollBarHandle(AKind: TScrollBarKind): THandle;
begin
  if GetScrollBar(AKind).Control <> nil then
    Result := GetScrollBar(AKind).Control.Handle
  else
    Result := 0;
end;

function TcxContainer.GetShadowBounds: TRect;
var
  ABorderWidth: Integer;
begin
  Result := cxRectContent(ViewInfo.Bounds, GetBorderExtent);
  ABorderWidth := ViewInfo.Painter.GetContainerBorderWidth(ViewInfo.GetContainerBorderStyle);
  InflateRect(Result, ABorderWidth, ABorderWidth);
  InflateRectByBorders(Result, ABorderWidth, ActiveStyle.Edges);
end;

function TcxContainer.GetShadowBoundsExtent: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxContainer.GetStyleClass: TcxContainerStyleClass;
begin
  Result := TcxContainerStyle;
end;

function TcxContainer.GetStylesClass: TcxContainerStylesClass;
begin
  Result := TcxContainerStyles;
end;

function TcxContainer.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxContainerViewInfo;
end;

function TcxContainer.GetWindowRegionAddon: TRect;
begin
  Result := cxNullRect;
end;

function TcxContainer.HasShadow: Boolean;
begin
  Result := ActiveStyle.Shadow and not IsNativeStyle;
end;

function TcxContainer.InternalGetActiveStyle: TcxContainerStyle;
begin
  if csDisabled in ViewInfo.ContainerState then
    Result := FStyles.StyleDisabled
  else if csActive in ViewInfo.ContainerState then
    Result := FStyles.StyleFocused
  else if Style.HotTrack and (csHotTrack in ViewInfo.ContainerState) then
    Result := FStyles.StyleHot
  else
    Result := FStyles.Style;
end;

function TcxContainer.InternalGetNotPublishedStyleValues: TcxContainerStyleValues;
begin
  Result := [];
end;

function TcxContainer.GetBorderColorByPainter(AIsHighlight: Boolean): TColor;
begin
  if not ViewInfo.UseSkins then
    Result := clDefault
  else
    if IsStyleAssigned(csvBorderColor) then
      Result := ActiveStyle.BorderColor
    else
      Result := ViewInfo.Painter.GetContainerBorderColor(AIsHighlight);
end;

procedure TcxContainer.GetColorSettingsByPainter(out ABackgroundColor, ATextColor: TColor);
begin
  ATextColor := clDefault;
  ABackgroundColor := clDefault;
  if ViewInfo.UseSkins then
  begin
    if not IsStyleAssigned(csvTextColor) then
      ATextColor := GetDefaultTextColorByPainter;
    if not (ParentColor or IsStyleAssigned(csvColor)) then
      ABackgroundColor := GetDefaultBackgroundColorByPainter;
  end;
end;

function TcxContainer.GetDefaultBackgroundColorByPainter: TColor;
begin
  Result := ViewInfo.Painter.DefaultEditorBackgroundColorEx(GetEditStateColorKind);
end;

function TcxContainer.GetDefaultTextColorByPainter: TColor;
begin
  Result := ViewInfo.Painter.DefaultEditorTextColorEx(GetEditStateColorKind);
end;

function TcxContainer.IsAlignControlsLocked: Boolean;
begin
  Result := FLockAlignControlsCount > 0;
end;

function TcxContainer.IsInnerControlBoundsChanged(AControl: TWinControl;
  const ABounds: TcxContainerInnerControlBounds): Boolean;
begin
  Result := (AControl = nil) or (AControl <> FInnerControl);
  if not Result and not (FInnerControlBounds.IsEmpty and ABounds.IsEmpty) then
  begin
    Result := FInnerControlBounds.IsEmpty or ABounds.IsEmpty;
    if not Result then
      Result := not EqualRect(FInnerControlBounds.Rect, ABounds.Rect);
  end;
end;

function TcxContainer.IsContainerClass: Boolean;
begin
  Result := False;
end;

function TcxContainer.IsMouseTracking: Boolean;
begin
  Result := cxControls.IsMouseTracking(Self);
end;

function TcxContainer.IsPopupMenuLocked: Boolean;
begin
  Result := FPopupMenuLockCount > 0;
end;

function TcxContainer.IsReadOnly: Boolean;
begin
  Result := False;
end;

procedure TcxContainer.BeginRefreshContainer;
begin
  Inc(FRefreshLockCount);
end;

function TcxContainer.EndRefreshContainer(AIsMouseEvent: Boolean): Boolean;
begin
  Dec(FRefreshLockCount);
  Result := ShortRefreshContainer(AIsMouseEvent);
end;

function TcxContainer.IsRefreshContainerLocked: Boolean;
begin
  Result := FRefreshLockCount > 0;
end;

function TcxContainer.RefreshContainer(const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := False;
  HasChanges := True;
  if CanRefreshContainer then
  begin
    Inc(FRefreshLockCount);
    try
      HasChanges := False;
      Result := DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
    finally
      Dec(FRefreshLockCount);
    end;
  end;
  if CanRefreshContainer and AIsMouseEvent then
    ProcessEventsOnViewInfoChanging;
  IsViewInfoCalculated := Result and not HasChanges;
end;

procedure TcxContainer.SaveInnerControlBounds(AControl: TWinControl;
  const ABounds: TcxContainerInnerControlBounds);
begin
  FInnerControlBounds := ABounds;
end;

procedure TcxContainer.UpdateWindowRegion;
begin
  if HandleAllocated and (Width > 0) and (Height > 0) and not FInternalWindowRegionSetting then
  begin
    Inc(FUpdateWindowRegionCount);
    if FNewWindowRegion <> 0 then
      DeleteObject(FNewWindowRegion);
    FNewWindowRegion := CreateWindowRegion;
    Perform(DXM_UPDATEWINDOWREGION, 0, 0);
  end;
end;

procedure TcxContainer.DoSetSize;

  procedure SetInnerControlBounds(const R: TRect);
  begin
    InnerControl.SetBounds(R.Left, R.Top, R.Width, R.Height);
  end;

  function NeedUpdateInnerControlBounds(const R: TRect): Boolean;
  begin
    Result := ViewInfo.FCalculated and HasInnerControl and not EqualRect(InnerControl.BoundsRect, R);
  end;

  function GetNeededInnerControlBounds: TRect;
  var
    AVScrollbarInfo, AHScrollbarInfo: TScrollBarInfo;
    AHScrollbarVisible, AVScrollbarVisible: Boolean;
  begin
    Result := ViewInfo.ClientRect;
    if IsPopupScrollBars then
    begin
      AVScrollbarVisible := IsScrollBarVisible(sbVertical, AVScrollbarInfo);
      AHScrollbarVisible := IsScrollBarVisible(sbHorizontal, AHScrollbarInfo);

      if AVScrollbarVisible then
        if UseRightToLeftScrollBar then
          Result.Left := Result.Left - AVScrollbarInfo.rcScrollBar.Width
        else
          Result.Right := Result.Right + AVScrollbarInfo.rcScrollBar.Width;
      if AHScrollbarVisible then
         Result.Bottom := Result.Bottom + AHScrollbarInfo.rcScrollBar.Height
      else
        if InnerControl is TcxCustomInnerListBox then
          Result.Bottom := Result.Bottom + cxScrollBar.GetScrollBarSize.cy;
    end;
  end;

var
  R: TRect;
begin
  R := GetNeededInnerControlBounds;
  if NeedUpdateInnerControlBounds(R) then
  begin
    SetInnerControlBounds(R);
    if IsPopupScrollBars then
    begin
      R := GetNeededInnerControlBounds;
      if NeedUpdateInnerControlBounds(R) then
        SetInnerControlBounds(R);
    end;
  end;
end;

procedure TcxContainer.SetSize;
begin
  Inc(FRefreshLockCount);
  try
    DoSetSize;
  finally
    Dec(FRefreshLockCount);
  end;
end;

procedure TcxContainer.SetVisibleBoundsClipRect;
var
  AClipRgn: TcxRegionHandle;
begin
  AClipRgn := GetWindowShadowRegion(Handle, GetShadowBounds, GetShadowBoundsExtent,
    not CanHaveTransparentBorder, ViewInfo.Shadow, cxEmptyRect);
  if AClipRgn <> 0 then
    Canvas.SetClipRegion(TcxRegion.Create(AClipRgn), roIntersect);
end;

procedure TcxContainer.UpdateData;
begin
end;

procedure TcxContainer.AdjustScrollBarPosition(AScrollBar: IcxControlScrollBar);
var
  AScaleFactor: IdxScaleFactor;
  AScrollBarInfo: TcxScrollBarInfo;
  R: TRect;
begin
  if IsScrollBarVisible(AScrollBar.Kind, AScrollBarInfo) then
  begin
    AScrollBar.Enabled := GetScrollBarEnabled(AScrollBar, AScrollBarInfo);
    R := GetScrollBarBounds(AScrollBarInfo.rcScrollBar);
    if not dxIsProcessPerMonitorV2Aware then
    begin
      if Supports(AScrollBar, IdxScaleFactor, AScaleFactor) then
        AScaleFactor.Value.Assign(dxSystemScaleFactor);
    end;
    AScrollBar.BoundsRect := cxRectBounds(R.Left, R.Top, R.Right, R.Bottom);
    SetScrollBarVisible(AScrollBar, (R.Right > 0) and (R.Bottom > 0));
  end
  else
    SetScrollBarVisible(AScrollBar, False);
end;

procedure TcxContainer.AdjustScrollBarPositions;
var
  AScrollBarInfo: TcxScrollBarInfo;
  AHScrollBarVisible, AVScrollBarVisible: Boolean;
  R: TRect;
begin
  AHScrollBarVisible := IsScrollBarVisible(sbHorizontal, AScrollBarInfo) and GetScrollBarEnabled(HScrollBar, AScrollBarInfo);
  AVScrollBarVisible := IsScrollBarVisible(sbVertical, AScrollBarInfo) and GetScrollBarEnabled(VScrollBar, AScrollBarInfo);

  if AHScrollBarVisible then
  begin
    R := ViewInfo.ClientRect;
    R.Top := R.Bottom  - GetScrollBarSize.cy;
    if AVScrollBarVisible then
      R.Right := R.Right - GetScrollBarSize.cx;
    if UseRightToLeftScrollBar then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, ViewInfo.ClientRect);
    (HScrollBar as TcxControlPopupScrollBar).SetOwnerControlRelativeBounds(R);
  end;

  if AVScrollBarVisible then
  begin
    R := ViewInfo.ClientRect;
    R.Left := R.Right  - GetScrollBarSize.cx;
    if AHScrollBarVisible then
      R.Bottom := R.Bottom - GetScrollBarSize.cy;
    if UseRightToLeftScrollBar then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, ViewInfo.ClientRect);
    (VScrollBar as TcxControlPopupScrollBar).SetOwnerControlRelativeBounds(R);
  end;

  HScrollBar.Data.Visible := AHScrollBarVisible;
  VScrollBar.Data.Visible := AVScrollBarVisible;
  SizeGrip.SetOwnerControlRelativeBounds(GetSizeGripBounds);
end;

procedure TcxContainer.AdjustVisibleFontHeight(AVisibleFont: TFont);
begin
// do nothing
end;

function TcxContainer.GetBackgroundThemedObjectType: TdxThemedObjectType;
begin
  Result := totEdit;
end;

function TcxContainer.GetBackgroundNativePart: Integer;
begin
  if IsWinVistaOrLater then
    Result := EP_BACKGROUND
  else
    Result := EP_EDITTEXT;
end;

function TcxContainer.GetBackgroundNativeState: Integer;
begin
  with ViewInfo do
  begin
    if not Enabled then
      Result := ETS_DISABLED
    else if IsReadOnly then
      Result := ETS_READONLY
    else if Focused then
      Result := ETS_FOCUSED
    else if csHotTrack in ContainerState then
      Result := ETS_HOT
    else
      Result := ETS_NORMAL;
  end;
end;

function TcxContainer.GetScrollBarBounds(const AScrollBarRect: TRect): TRect;
begin
  Result.TopLeft := ScreenToClient(AScrollBarRect.TopLeft);
  Result.Right := AScrollBarRect.Right - AScrollBarRect.Left;
  Result.Bottom := AScrollBarRect.Bottom - AScrollBarRect.Top;
  if (Result.Left < 0) or (Result.Right > Width) or
    (Result.Top < 0) or (Result.Bottom > Height) then
      Result := cxEmptyRect;
end;

function TcxContainer.GetScrollBarEnabled(AScrollBar: IcxControlScrollBar;
  const AScrollBarinfo: TcxScrollBarInfo): Boolean;
begin
  Result := Enabled and (AScrollBarInfo.rgstate[0] and STATE_SYSTEM_UNAVAILABLE = 0);
end;

function TcxContainer.GetScrollBarInfo(var AScrollBarInfo: TcxScrollBarInfo; const AKind: TScrollBarKind): Boolean;
const
  AScrollBarObjects: array [TScrollBarKind] of Longword = (OBJID_HSCROLL, OBJID_VSCROLL);
begin
  Result := not IsDestroying and (Parent <> nil) and HandleAllocated and HasInnerControlHandle;
  if Result then
    Result := cxGetScrollBarInfo(FInnerControl.Handle, Integer(AScrollBarObjects[AKind]),
      AScrollBarInfo, ScaleFactor, UseRightToLeftScrollBar);
end;

function TcxContainer.CanDisableAlignOnCreateInnerControl: Boolean;
begin
  Result := not IsContainerClass;
end;

function TcxContainer.HandleInnerContolGestures: Boolean;
begin
  Result := False;
end;

function TcxContainer.IsNativeStyle: Boolean;
begin
  Result := AreVisualStylesMustBeUsed(Style.LookAndFeel.NativeStyle, GetBackgroundThemedObjectType);
end;

procedure TcxContainer.ProcessEventsOnViewInfoChanging;
begin
  if FProcessEventsLockCount = 0 then
  begin
    Inc(FProcessEventsLockCount);
    try
      DoProcessEventsOnViewInfoChanging;
    finally
      Dec(FProcessEventsLockCount);
    end;
  end;
end;

procedure TcxContainer.SafeSelectionFocusInnerControl;
begin
  InnerControl.SetFocus;
end;

procedure TcxContainer.ScaleFactorChanged;
begin
  BeginRefreshContainer;
  try
    inherited ScaleFactorChanged;
  finally
    EndRefreshContainer;
  end;
end;

procedure TcxContainer.SetDragKind(Value: TDragKind);
begin
  inherited DragKind := Value;
  if HasInnerControl then
    TWinControlAccess(InnerControl).DragKind := Value;
end;

procedure TcxContainer.SetScrollBarVisible(AScrollBar: IcxControlScrollBar;
  AVisible: Boolean);
begin
  AScrollBar.Visible := AVisible;
  if AVisible and (AScrollBar.Control <> nil) then
  begin
    AScrollBar.Control.Ctl3D := False;
    AScrollBar.Control.BringToFront;
  end;
end;

function TcxContainer.GetActiveControl: TWinControl;
begin
  if HasInnerControl then
    Result := FInnerControl
  else
    Result := Self;
end;

function TcxContainer.GetFakeStyleController: TcxStyleController;
begin
  Result := Style.StyleController;
end;

function TcxContainer.GetInternalStyle(AState: TcxContainerStateItem): TcxContainerStyle;
begin
  Result := FStyles[AState];
end;

function TcxContainer.GetIsDestroying: Boolean;
begin
  Result := (csDestroying in ComponentState);
end;

procedure TcxContainer.GetStandardScrollBarParameters(AKind: TScrollBarKind; out AScrollInfo: TScrollInfo);
const
  ABarFlags: array [TScrollBarKind] of Integer = (SB_HORZ, SB_VERT);
begin
  AScrollInfo.cbSize := SizeOf(AScrollInfo);
  AScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(FInnerControl.Handle, ABarFlags[AKind], AScrollInfo);
  if Integer(AScrollInfo.nPage) > AScrollInfo.nMax then
    Integer(AScrollInfo.nPage) := AScrollInfo.nMax;
end;

function TcxContainer.GetStyle: TcxContainerStyle;
begin
  Result := TcxContainerStyle(FStyles.Style);
end;

function TcxContainer.GetStyleDisabled: TcxContainerStyle;
begin
  Result := TcxContainerStyle(FStyles.StyleDisabled);
end;

function TcxContainer.GetStyleFocused: TcxContainerStyle;
begin
  Result := TcxContainerStyle(FStyles.StyleFocused);
end;

function TcxContainer.GetStyleHot: TcxContainerStyle;
begin
  Result := TcxContainerStyle(FStyles.StyleHot);
end;

function TcxContainer.GetVisibleFont: TFont;
var
  ABkColor, ATextColor: TColor;
begin
  Result := ActiveStyle.GetVisibleFont;
  AdjustVisibleFontHeight(Result);
  GetColorSettingsByPainter(ABkColor, ATextColor);
  if ATextColor <> clDefault then
    Result.Color := ATextColor;
end;

function TcxContainer.IsScrollBarVisible(AKind: TScrollBarKind; out AScrollbarInfo: TScrollBarInfo): Boolean;
begin
  Result := GetScrollBarInfo(AScrollbarInfo, AKind) and
    (AScrollBarInfo.rgstate[0] and (STATE_SYSTEM_INVISIBLE or STATE_SYSTEM_OFFSCREEN) = 0) and
    (AScrollbarInfo.rcScrollBar.Width > 0) and
    (AScrollbarInfo.rcScrollBar.Height > 0);
end;

procedure TcxContainer.SetFakeStyleController(Value: TcxStyleController);
begin
end;

procedure TcxContainer.SetInnerControl(Value: TWinControl);
begin
  if FInnerControl <> Value then
  begin
    if HasInnerControl then
      FInnerControl.RemoveFreeNotification(Self);
    FInnerControl := Value;
    if HasInnerControl then
      FInnerControl.FreeNotification(Self);
  end;
end;

procedure TcxContainer.SetParentFont(Value: Boolean);
begin
  inherited ParentFont := Value;

  if not Value and ([csReading, csLoading] * ComponentState = []) then
  begin
    if not Style.IsValueAssigned(csvFont) then
      Style.AssignedValues := Style.AssignedValues + [csvFont];
  end;
end;

procedure TcxContainer.SetStyle(Value: TcxContainerStyle);
begin
  FStyles.Style := Value;
end;

procedure TcxContainer.SetStyleDisabled(Value: TcxContainerStyle);
begin
  FStyles.StyleDisabled := Value;
end;

procedure TcxContainer.SetStyleFocused(Value: TcxContainerStyle);
begin
  FStyles.StyleFocused := Value;
end;

procedure TcxContainer.SetStyleHot(Value: TcxContainerStyle);
begin
  FStyles.StyleHot := Value;
end;

procedure TcxContainer.SetInternalStyle(AState: TcxContainerStateItem;
  Value: TcxContainerStyle);
begin
  FStyles[AState] := Value;
end;

function TcxContainer.GetDragKind: TDragKind;
begin
  Result := inherited DragKind;
end;

procedure TcxContainer.WMDestroy(var Message: TWMDestroy);
begin
  FUpdateWindowRegionCount := 0;
  inherited;
end;

procedure TcxContainer.WMKillFocus(var Message: TWMKillFocus);
begin
  if HasInnerControl then
  begin
    if not InnerControl.HandleAllocated or (Message.FocusedWnd <> InnerControl.Handle) then
    begin
      inherited;
      Exit;
    end;
    Message.Msg := 0;
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TcxContainer.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message do
    if HasInnerControl and (CursorWnd = Handle) and
      (Smallint(HitTest) = HTCLIENT) and not PtInRect(ViewInfo.ClientRect, GetMouseCursorClientPos) then
    begin
      Windows.SetCursor(Screen.Cursors[crArrow]);
      Result := 1;
      Exit;
    end;
  inherited;
end;

procedure TcxContainer.WMSetFocus(var Message: TWMSetFocus);
begin
  if not IsDestroying and HandleAllocated and HasInnerControlHandle then
  begin
    if Message.FocusedWnd <> InnerControl.Handle then
    begin
      inherited;
      if InnerControl.CanFocus then
        InnerControl.SetFocus;
      Exit;
    end;
    Message.Msg := 0;
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TcxContainer.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;

  if not IsDestroying and not FInternalWindowRegionSetting then
    ShortRefreshContainer(False);
end;

procedure TcxContainer.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  AParentForm: TCustomForm;
  ASize: TSize;
begin
  if IsDestroying then
    Message.Result := 0
  else
  begin
    ASize := cxSize(Message.WindowPos.cx, Message.WindowPos.cy);

    inherited;

    if (Align = alRight) and (ASize.cx <> Message.WindowPos.cx) then
      Message.WindowPos.x := Message.WindowPos.x + ASize.cx - Message.WindowPos.cx;
    if (Align = alBottom) and (ASize.cy <> Message.WindowPos.cy) then
      Message.WindowPos.y := Message.WindowPos.y + ASize.cy - Message.WindowPos.cy;

    if (GetFocus = Handle) and HasInnerControlHandle and
      InnerControl.CanFocus and not (csFocusing in ControlState) then
    begin
      AParentForm := GetParentForm(Self);
      AParentForm.FocusControl(InnerControl);
    end;
  end;
end;

procedure TcxContainer.CMBiDiModeChanged(var Message: TMessage);
var
  APrevWParam: Integer;
begin
  APrevWParam := Message.WParam;
  try
    Message.wParam := 1;
    inherited;
  finally
    Message.wParam := APrevWParam;
  end;
  if (SysLocale.MiddleEast) and (Message.wParam = 0) then RecreateWnd;
end;

procedure TcxContainer.CMCtl3DChanged(var Message: TMessage);
begin
  BeginRefreshContainer;
  try
    inherited;
  finally
    EndRefreshContainer;
  end;
end;

procedure TcxContainer.CMFontChanged(var Message: TMessage);
var
  APrevIsFontAssigned: Boolean;
begin
  if FInternalSetting = 0 then
  begin
    APrevIsFontAssigned := csvFont in Style.FAssignedValues;
    Style.Font := Font;
    if not APrevIsFontAssigned then
      Exclude(Style.FAssignedValues, csvFont);
    inherited;
  end
  else
  begin
    BeginRefreshContainer;
    try
      inherited;
      SetSize;
      SetScrollBarsParameters;
    finally
      EndRefreshContainer;
    end;
  end;
end;

procedure TcxContainer.CMParentColorChanged(var Message: TMessage);
var
  APrevIsStyleColorAssigned: Boolean;
begin
  APrevIsStyleColorAssigned := csvColor in FStyles.Style.FAssignedValues;
  inherited; // TODO CLX ???
  if not APrevIsStyleColorAssigned or ParentColor then
    Exclude(FStyles.Style.FAssignedValues, csvColor);
  if Color <> Style.Color then
    ContainerStyleChanged(Style);

  if IsTransparentBackground and not ParentColor then
    Invalidate;
end;

procedure TcxContainer.CMShortRefreshContainer(var Message: TMessage);
begin
  ShortRefreshContainer(False);
end;

procedure TcxContainer.CMParentFontChanged(var Message: TMessage);
var
  APrevIsStyleFontAssigned: Boolean;
begin
  APrevIsStyleFontAssigned := csvFont in FStyles.Style.FAssignedValues;
  inherited; // TODO CLX ???
  if not ParentFont then
    FStyles.Style.UpdateFont;
  if not APrevIsStyleFontAssigned or ParentFont then
    Exclude(FStyles.Style.FAssignedValues, csvFont);
end;

procedure TcxContainer.CMSysColorChange(var Message: TMessage);
begin
  BeginRefreshContainer;
  try
    inherited;
  finally
    EndRefreshContainer;
  end;
end;

procedure TcxContainer.CMUpdateScrollBars(var Message: TMessage);
begin
  SetScrollBarsParameters;
end;

procedure TcxContainer.DXMUpdateWindowRegion(var Message: TMessage);
begin
  Dec(FUpdateWindowRegionCount);
  if FUpdateWindowRegionCount = 0 then
  begin
    FInternalWindowRegionSetting := True;
    if SetWindowRegion(Handle, FNewWindowRegion) then
      FNewWindowRegion := 0;
    FInternalWindowRegionSetting := False;
  end;
end;

function TcxContainer.GetScrollContentForegroundColor: TColor;
begin
  Result := VisibleFont.Color;
end;

{ TcxCustomPopupWindow }

constructor TcxCustomPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create;
  Scaled := not Supports(AOwnerControl, IdxScaleFactor);
  FormStyle := fsNormal;
  Visible := False;

  FStyle := GetStyleClass.Create(Self, False);
  FStyle.OnChanged := PopupWindowStyleChanged;
  FViewInfo := GetViewInfoClass.Create;
  FViewInfo.UpdateStyle(FStyle);

  FCaptureFocus := True;
  FOwnerControl := AOwnerControl;

  KeyPreview := True;
  FModalMode := True;
end;

destructor TcxCustomPopupWindow.Destroy;
begin
  cxClearObjectLinks(Self);
  PopupMode := pmNone;  // to set FInternalPopupParent to nil
//  PopupParent := nil;
  if IsVisible then
    cxPopupVisibleController.UnRegister(Self);
  FreeAndNil(FViewInfo);
  FStyle.OnChanged := nil;
  FreeAndNil(FStyle);
  inherited Destroy;
end;

function TcxCustomPopupWindow.Focused: Boolean;
var
  AFocusedControl: THandle;
begin
  Result := False;
  if IsVisible and CaptureFocus then
  begin
    AFocusedControl := GetFocus;
    Result := HasNativeHandle(Self, AFocusedControl, True);
  end;
end;

function TcxCustomPopupWindow.CanFocus: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomPopupWindow.ClosePopup;
begin
  PostMessage(Handle, DXM_CLOSEPOPUPWINDOW, 0, 0);
end;

procedure TcxCustomPopupWindow.CloseUp;

  procedure SafePassFocus;
  begin
    if FPrevActiveForm <> nil then
    begin
      if Screen.ActiveControl <> FPrevActiveForm.ActiveControl then
        if (FPrevActiveControl <> nil) and FPrevActiveControl.HandleAllocated then
          FPrevActiveForm.SetFocusedControl(FPrevActiveControl)
        else
          FPrevActiveForm.SetFocusedControl(FPrevActiveForm);
    end;
  end;

var
  AParentForm: TCustomForm;
begin
  try
    DoBeforeClosing;
  except
    on E: Exception do
    begin
      Application.HandleException(E);
    end;
  end;

  FIsPopup := False;
  if not IsVisible then
    Exit;

  LockCMActivateMessages(True);
  try
    cxPopupVisibleController.UnRegister(Self);
    DoClosing;

    FPopupWindowShowing := True;
    FBeingShownPopupWindow := Self;
    ShowWindow(Handle, SW_HIDE);
    Hide;
    if not FShowWithoutActivation then
      SafePassFocus;

    DoClosed;

    if HandleAllocated and HasNativeHandle(Self, GetCapture, True) then
      SetCaptureControl(nil);
  finally
    FPopupWindowShowing := False;
    LockCMActivateMessages(False);
  end;

  if not cxPopupVisibleController.IsAnyVisible then
  begin
    AParentForm := GetParentForm(OwnerControl);
    if (AParentForm <> nil) and AParentForm.HandleAllocated and
     ((TCustomFormAccess(AParentForm).FormStyle <> fsMDIForm) and not AParentForm.Active) then
      SendMessage(AParentForm.Handle, WM_NCACTIVATE, 0, 0);
  end;
end;

procedure TcxCustomPopupWindow.CorrectBoundsWithDesktopWorkArea(
  var APosition: TPoint; var ASize: TSize);
var
  ADesktopWorkArea: TRect;
  AWidth, AHeight: Integer;
begin
  if not AcceptsAnySize then
    Exit;

  AWidth := ASize.cx;
  AHeight := ASize.cy;
  ADesktopWorkArea := GetDesktopWorkArea(OwnerScreenBounds.TopLeft);
  with APosition do
  begin
    if X < ADesktopWorkArea.Left then
    begin
      AWidth := AWidth + X - ADesktopWorkArea.Left;
      X := ADesktopWorkArea.Left;
    end;
    if Y < ADesktopWorkArea.Top then
    begin
      AHeight := AHeight + Y - ADesktopWorkArea.Top;
      Y := ADesktopWorkArea.Top;
    end;
    if AWidth > ADesktopWorkArea.Right - X then
      AWidth := ADesktopWorkArea.Right - X;
    if AHeight > ADesktopWorkArea.Bottom - Y then
      AHeight := ADesktopWorkArea.Bottom - Y;
    ASize.cx := AWidth;
    ASize.cy := AHeight;
    if (ASize.cy <> AHeight) and (Y = ADesktopWorkArea.Top) then
      Inc(APosition.Y, AHeight - ASize.cy);
  end;
end;

function TcxCustomPopupWindow.GetStyleClass: TcxContainerStyleClass;
begin
  Result := TcxContainerStyle;
end;

function TcxCustomPopupWindow.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxContainerViewInfo;
end;

function TcxCustomPopupWindow.HasCapture: Boolean;
begin
  Result := HasNativeHandle(Self, GetCapture);
end;

function TcxCustomPopupWindow.IsShortCut(var Message: TWMKey): Boolean;
var
  AParentForm: TCustomForm;
begin
  Result := inherited IsShortCut(Message);
  if not Result then
  begin
    AParentForm := GetParentForm(OwnerControl);
    if AParentForm <> nil then
      Result := AParentForm.IsShortCut(Message);
  end;
end;

function TcxCustomPopupWindow.IsVisible: Boolean;
begin
  Result := cxPopupVisibleController.IsVisible(Self);
end;

procedure TcxCustomPopupWindow.LockDeactivate(ALock: Boolean);
begin
  if ALock then
    Inc(FDeactivateLockCount)
  else
    Dec(FDeactivateLockCount);
end;

procedure TcxCustomPopupWindow.AdjustClientRect(var Rect: TRect);
begin
  Rect := ViewInfo.ClientRect;
end;

procedure TcxCustomPopupWindow.Deactivate;
var
  AActiveWnd: THandle;
  APopupWindow: TcxCustomPopupWindow;
  I: Integer;
begin
  FDeactivation := False;
  if IsDeactivateLocked then
    Exit;

  if cxPopupVisibleController.IsAnyVisible then
  begin
    AActiveWnd := GetActiveWindow;
    for I := cxPopupVisibleController.Count - 1 downto 0 do
    begin
      APopupWindow := cxPopupVisibleController[I];
      if HasHandle(APopupWindow, AActiveWnd) then
        Exit;
      if IsWindowEnabled(APopupWindow.Handle) and not APopupWindow.IsDeactivateLocked then
        APopupWindow.CloseUp;
    end;
  end;
  if cxPopupVisibleController.IsAnyVisible then
    CloseUp;
end;

procedure TcxCustomPopupWindow.InitPopup;
begin
  FPrevActiveForm := dxGetScreenActiveForm;
  cxAddFreeNotification(Self, FPrevActiveForm);
  FPrevActiveControl := Screen.ActiveControl;
  cxAddFreeNotification(Self, FPrevActiveControl);
  UpdateScaleFactor;
end;

procedure TcxCustomPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if IsVisible and HasCapture and (Key = VK_ESCAPE) then
  begin
    SetCaptureControl(nil);
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomPopupWindow.Paint;
begin
  ViewInfo.Paint(Canvas);
end;

procedure TcxCustomPopupWindow.VisibleChanged;
var
  AParentForm: TCustomForm;
begin
  inherited VisibleChanged;
  if CaptureFocus and (OwnerControl <> nil) and HandleAllocated and
    not IsWindowVisible(Handle) then
  begin
    AParentForm := GetParentForm(FOwnerControl);
    if (AParentForm <> nil) and HasNativeHandle(AParentForm, GetFocus) and
      OwnerControl.CanFocus and not (csDesigning in AParentForm.ComponentState) then
        OwnerControl.SetFocus;
  end;
end;

procedure TcxCustomPopupWindow.CreateHandle;
var
  AIsInVisiblePopupWindowList: Boolean;
begin
  AIsInVisiblePopupWindowList := False;
  if not cxPopupVisibleController.IsVisible(Self) then
    cxPopupVisibleController.Register(Self)
  else
    AIsInVisiblePopupWindowList := True;
  try
    inherited CreateHandle;
  finally
    if not AIsInVisiblePopupWindowList then
      cxPopupVisibleController.UnRegister(Self);
  end;
end;

procedure TcxCustomPopupWindow.CreateParams(var Params: TCreateParams);
var
  AParentForm: TCustomForm;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if CaptureFocus then
    begin
      AParentForm := GetParentForm(OwnerControl);
      if AParentForm <> nil then
        WndParent := AParentForm.Handle;
      Style := Style and not WS_CHILD;
      Style := Style or WS_POPUP;
//      ExStyle := ExStyle or WS_EX_TOOLWINDOW;
      if FIsTopMost and ((AParentForm = nil) or (GetWindowLong(AParentForm.Handle,
          GWL_EXSTYLE) and WS_EX_TOPMOST <> 0)) then
        ExStyle := ExStyle or WS_EX_TOPMOST;
    end
    else
    begin
      Style := WS_CHILD;
      ExStyle := ExStyle or WS_EX_TOOLWINDOW;
      Params.WndParent := GetDesktopWindow;
    end;
    Style := Style or WS_CLIPCHILDREN;
  end;
end;

procedure TcxCustomPopupWindow.CreateWnd;
begin
  if CaptureFocus then
    PopupParent := GetParentForm(FOwnerControl)
  else
    PopupParent := nil;

  inherited CreateWnd;
end;

procedure TcxCustomPopupWindow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FPrevActiveForm then
      FPrevActiveForm := nil;
    if AComponent = FPrevActiveControl then
      FPrevActiveControl := nil;
  end;
end;

procedure TcxCustomPopupWindow.PopupWindowStyleChanged(Sender: TObject);
begin
end;

procedure TcxCustomPopupWindow.RecreateWindow;
begin
  if HandleAllocated then
    RecreateWnd;
end;

procedure TcxCustomPopupWindow.UpdateScaleFactor;
var
  ABounds: TRect;
  ATargetBounds: PRect;
  AScaleFactor: IdxScaleFactor;
begin
  if Supports(OwnerControl, IdxScaleFactor, AScaleFactor) then
  begin
    if Adjustable then
    begin
      ABounds := BoundsRect;
      ATargetBounds := @ABounds;
    end
    else
      ATargetBounds := nil;
    ScaleForPPI(AScaleFactor.Value.Apply(dxDefaultDPI), ATargetBounds);
  end;
end;

function TcxCustomPopupWindow.UseOwnerParentToGetScreenBounds: Boolean;
begin
  Result := IsChildClassWindow(OwnerControl.Handle);
end;

procedure TcxCustomPopupWindow.Popup(AFocusedControl: TWinControl);

  function IsTopMostPopupWindow: Boolean;
  var
    I: Integer;
    AParentForm: TCustomForm;
  begin
    AParentForm := GetParentForm(OwnerControl);
    Result := (AParentForm = nil) or (GetWindowLong(AParentForm.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0);
    if not Result and not CaptureFocus then
      for I := 0 to Screen.FormCount - 1 do
      begin
        if (Screen.Forms[I].FormStyle = fsStayOnTop) and not Screen.Forms[I].Visible then
          Exit(True);
      end;
  end;

  function GetPopupWindowShowingFlags: HWND;
  begin
    if IsTopMostPopupWindow or (not CaptureFocus and TopMostComboBoxes) then
      Result := HWND_TOPMOST
    else
      Result := 0;
  end;

  procedure ShowPopupWindow;
  var
    P: TPoint;
    ASize: TSize;
  begin
    FIsPopup := True;
    InitPopup;
    ASize := CalculateSize;
    P := CalculatePosition(ASize);
    CorrectBoundsWithDesktopWorkArea(P, ASize);
    FPopupWindowShowing := True;
    FCaptionInactivationLocked := True;
    FBeingShownPopupWindow := Self;
    try
      cxPopupVisibleController.Register(Self);
      DoShowing;
      SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
      if FShowWithoutActivation then
        Visible := True
      else
        Show;
      SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
      if not FShowWithoutActivation then
      begin
        if CaptureFocus then
          FFocusedControl := GetFirstFocusControl(AFocusedControl)
        else
          FFocusedControl := AFocusedControl;

        if (FFocusedControl <> nil) and FFocusedControl.CanFocus then
          FFocusedControl.SetFocus
        else
          SetFocus;
      end;
      SetWindowPos(Handle, GetPopupWindowShowingFlags, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
    finally
      FPopupWindowShowing := False;
      FCaptionInactivationLocked := False;
    end;
  end;

var
  Msg: TMsg;
  ALinkSelf: TcxObjectLink;
begin
  if IsVisible or not IsOwnerControlVisible then
    Exit;

  if cxPopupVisibleController.IsAnyVisible and FShowWithoutActivation then
    Exit;

  ShowPopupWindow;
  ModalResult := mrNone;
  DoShowed;
  if FCaptureFocus and ModalMode then
  begin
    ALinkSelf := cxAddObjectLink(Self);
    try
      while (ALinkSelf.Ref <> nil) and Visible and not Application.Terminated do
      begin
        if PeekMessage(Msg, 0, WM_SYSKEYDOWN, WM_SYSKEYDOWN, PM_NOREMOVE) then
          case Msg.wParam of
            Windows.VK_MENU, Windows.VK_RETURN, Windows.VK_SPACE:
              PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE);
          end;
        if ModalResult <> mrNone then
          if CloseQuery then
            ModalCloseUp
          else
            ModalResult := mrNone;
        Application.HandleMessage;
      end;
    finally
      cxRemoveObjectLink(ALinkSelf);
    end;
  end;
end;

function TcxCustomPopupWindow.SetFocusedControl(Control: TWinControl): Boolean;
begin
  Result := False;
  if csDestroying in Control.ComponentState then
    Exit;

  LockCMActivateMessages(True);
  try
    Result := inherited SetFocusedControl(Control);
  finally
    LockCMActivateMessages(False);
  end;
end;

function TcxCustomPopupWindow.AcceptsAnySize: Boolean;
begin
  Result := False;
end;

procedure TcxCustomPopupWindow.DoBeforeClosing;
begin
  if Assigned(FOnBeforeClosing) then
    FOnBeforeClosing(Self);
end;

procedure TcxCustomPopupWindow.DoClosed;
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

procedure TcxCustomPopupWindow.DoClosing;
begin
  if Assigned(FOnClosing) then
    FOnClosing(Self);
end;

procedure TcxCustomPopupWindow.DoShowed;
begin
  if Assigned(FOnShowed) then
    FOnShowed(Self);
end;

procedure TcxCustomPopupWindow.DoShowing;
begin
  if Assigned(FOnShowing) then
    FOnShowing(Self);
end;

function TcxCustomPopupWindow.GetFirstFocusControl(AControl: TWinControl): TWinControl;
begin
  if AControl = nil then
    Result := Self
  else
    if AControl.CanFocus and AControl.TabStop then
      Result := AControl
    else
    begin
      Result := FindNextControl(nil, True, True, False);
      if Result = nil then
        Result := Self;
    end;
end;

function TcxCustomPopupWindow.GetOwnerControl: TWinControl;
begin
  Result := FOwnerControl;
end;

function TcxCustomPopupWindow.HasBackground;
begin
  Result := False;
end;

procedure TcxCustomPopupWindow.InternalEnableWindow(AEnable: Boolean);
begin
  FInternalWindowsEnabling := True;
  try
    if HandleAllocated then
      EnableWindow(Handle, AEnable);
  finally
    FInternalWindowsEnabling := False;
  end;
end;

function TcxCustomPopupWindow.IsDeactivateLocked: Boolean;
begin
  Result := FDeactivateLockCount <> 0;
end;

function TcxCustomPopupWindow.IsOwnerControlVisible: Boolean;
begin
  Result := OwnerControl.HandleAllocated and IsWindowVisible(OwnerControl.Handle);
end;

function TcxCustomPopupWindow.IsSysKeyAccepted(Key: Word): Boolean;
begin
  case Key of
    Windows.VK_F4, Windows.VK_LEFT, Windows.VK_RIGHT, Windows.VK_UP,
    Windows.VK_DOWN, Windows.VK_PRIOR, Windows.VK_NEXT, Windows.VK_HOME,
    Windows.VK_END:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TcxCustomPopupWindow.ModalCloseUp;
begin
  CloseUp;
end;

procedure TcxCustomPopupWindow.MouseEnter(AControl: TControl);
begin
end;

procedure TcxCustomPopupWindow.MouseLeave(AControl: TControl);
begin
end;

function TcxCustomPopupWindow.NeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle;
  AMsg: Cardinal; AShift: TShiftState; const APos: TPoint): Boolean;
begin
  Result := False;
end;

function TcxCustomPopupWindow.GetJustClosed: Boolean;
begin
  Result := FJustClosed;
  FJustClosed := False;
end;

procedure TcxCustomPopupWindow.SetCaptureFocus(Value: Boolean);
begin
  if Value <> FCaptureFocus then
  begin
    FCaptureFocus := Value;
    RecreateWindow;
    if IsVisible then
    begin
      CloseUp;
      Popup(FocusedControl);
    end;
  end;
end;

procedure TcxCustomPopupWindow.SetIsTopMost(Value: Boolean);
begin
  if Value <> FIsTopMost then
  begin
    FIsTopMost := Value;
    RecreateWindow;
    if IsVisible then
    begin
      CloseUp;
      Popup(FocusedControl);
    end;
  end;
end;

procedure TcxCustomPopupWindow.WMActivateApp(var Message: TWMActivateApp);
begin
end;

procedure TcxCustomPopupWindow.WMActivate(var Message: TWMActivate);
begin
  if FIsPopup or (Message.Active = WA_INACTIVE) then
    inherited;
end;

procedure TcxCustomPopupWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if HasBackground then
    inherited
  else
    Message.Result := 1;
end;

procedure TcxCustomPopupWindow.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
var
  AWindowPos: PWindowPos;
begin
  AWindowPos := Message.WindowPos;
  if not FIsPopup and ((AWindowPos.flags and SWP_SHOWWINDOW) <> 0) then
    AWindowPos.flags := AWindowPos.flags and not SWP_SHOWWINDOW;
  inherited;
end;

procedure TcxCustomPopupWindow.CMClosePopupWindow(var Message: TMessage);
begin
  dxMessagesController.LockMessages([CM_ENTER, CM_EXIT]);
  try
    LockDeactivate(True);
    try
      CloseUp;
    finally
      LockDeactivate(False);
    end;
    if OwnerControl.HandleAllocated then
      SendMessage(OwnerControl.Handle, WM_SETFOCUS, 0, 0);
  finally
    dxMessagesController.UnlockMessages([CM_ENTER, CM_EXIT]);
  end;
end;

procedure TcxCustomPopupWindow.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxCustomPopupWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxCustomPopupWindow.CMRecreateWnd(var Message: TMessage);
begin
  if csDestroying in ComponentState then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxCustomPopupWindow.CMShowingChanged(var Message: TMessage);
begin
  if FShowWithoutActivation and Showing then
  begin
    Include(FFormState, fsShowing);
    try
      try
        DoShow;
      except
        Application.HandleException(Self);
      end;

      ShowWindow(Handle, SW_SHOWNOACTIVATE);
    finally
      Exclude(FFormState, fsShowing);
    end;
  end
  else
    inherited;
end;

procedure TcxCustomPopupWindow.CMShowPopupWindow(var Message: TMessage);
begin
  Popup(FocusedControl);
end;

{ TcxCustomInnerListBox }

constructor TcxCustomInnerListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TcxCanvas.Create(inherited Canvas);
  CreateScrollBars;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FHScrollBar.LookAndFeel.MasterLookAndFeel := FLookAndFeel;
  FVScrollBar.LookAndFeel.MasterLookAndFeel := FLookAndFeel;
  FScrollUIActivityHelper := TdxTouchScrollUIActivityHelper.Create;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle + [csDoubleClicks{$IFDEF DELPHI16},
    csOverrideStylePaint{$ENDIF}] - [csOpaque];
  ParentColor := False;
  ParentFont := True;
end;

destructor TcxCustomInnerListBox.Destroy;
begin
  FreeAndNil(FScrollUIActivityHelper);
  if (FHScrollBar <> nil) and (FHScrollBar.Parent = nil) then
    FreeAndNil(FHScrollBar);
  if (FVScrollBar <> nil) and (FVScrollBar.Parent = nil) then
    FreeAndNil(FVScrollBar);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TcxCustomInnerListBox.DefaultHandler(var Message);
begin
  if (RootContainer = nil) or not RootContainer.InnerControlDefaultHandler(TMessage(Message)) then
    inherited DefaultHandler(Message);
end;

procedure TcxCustomInnerListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if RootContainer <> nil then
    RootContainer.DragDrop(Source, Left + X, Top + Y);
end;

procedure TcxCustomInnerListBox.SetExternalScrollBarsParameters;

  procedure AdjustScrollBarPosition(AScrollBar: TcxScrollBar);
  const
    ScrollBarObjects: array [TScrollBarKind] of Longword = (OBJID_HSCROLL, OBJID_VSCROLL);
  var
    ABounds: TRect;
    AScrollBarInfo: TcxScrollBarInfo;
    AScrollBarState: DWORD;
    AScrollBarVisible: Boolean;
  begin
    AScrollBarVisible := False;
    if cxGetScrollBarInfo(Handle, Integer(ScrollBarObjects[AScrollBar.Kind]), AScrollBarInfo, ScaleFactor, UseRightToLeftScrollBar) then
    begin
      AScrollBarState := AScrollBarInfo.rgstate[0];
      if AScrollBarState and (STATE_SYSTEM_INVISIBLE or STATE_SYSTEM_OFFSCREEN) = 0 then
      begin
        AScrollBar.Enabled := (AScrollBarState and STATE_SYSTEM_UNAVAILABLE = 0) and (AScrollBar.Parent <> nil) and AScrollBar.Parent.Enabled;
        ABounds.TopLeft := Parent.ScreenToClient(AScrollBarInfo.rcScrollBar.TopLeft);
        ABounds.BottomRight := Parent.ScreenToClient(AScrollBarInfo.rcScrollBar.BottomRight);
        AScrollBarVisible := cxRectIntersect(ABounds, Parent.ClientRect);
      end;
    end;

    if AScrollBarVisible then
    begin
      Inc(FScrollBarsLockCount);
      try
        AScrollBar.Parent := Parent;
        if not dxIsProcessPerMonitorV2Aware then
          TcxControlHelper.ChangeScaleFactor(AScrollBar, dxSystemScaleFactor);
        AScrollBar.BoundsRect := ABounds;
      finally
        Dec(FScrollBarsLockCount);
      end;
      AScrollBar.Ctl3D := False;
      if (Container <> nil) and Container.NeedsScrollBars then
        AScrollBar.Visible := False
      else
      begin
        AScrollBar.BringToFront;
        AScrollBar.Visible := True;
      end;
    end
    else
    begin
      AScrollBar.BoundsRect := cxNullRect;
      AScrollBar.Visible := False;
    end;
  end;

  procedure SetScrollBarParameters(AScrollBar: TcxScrollBar);
  const
    BarFlags: array [TScrollBarKind] of Integer = (SB_HORZ, SB_VERT);
  var
    AScrollInfo: TScrollInfo;
  begin
    AdjustScrollBarPosition(AScrollBar);
    if AScrollBar.Visible then
    begin
      AScrollInfo.cbSize := SizeOf(AScrollInfo);
      AScrollInfo.fMask := SIF_ALL;
      GetScrollInfo(Handle, BarFlags[AScrollBar.Kind], AScrollInfo);
      with AScrollInfo do
      begin
        if Integer(nPage) > nMax then
          Integer(nPage) := nMax;
        AScrollBar.SetScrollParams(nMin, nMax, nPos, nPage, True);
      end;
    end;
  end;

begin
  if (csDestroying in ComponentState) or (FScrollBarsLockCount > 0) or not UsecxScrollBars or
    IsRedrawLocked or (Parent = nil) or not Parent.HandleAllocated then
    Exit;
  FScrollBarsCalculating := True;
  try
    SetScrollBarParameters(FHScrollBar);
    SetScrollBarParameters(FVScrollBar);
  finally
    FScrollBarsCalculating := False;
  end;
  if Container <> nil then
    Container.SetScrollBarsParameters;
end;

function TcxCustomInnerListBox.ItemVisible(Index: Integer): Boolean;
var
  R: TRect;
begin
  R := GetControlRect(Self);
  with ItemRect(Index) do
  begin
    Result := PtInRect(R, TopLeft);
    Result := Result or PtInRect(R, Point(Right - 1, Top));
    Result := Result or PtInRect(R, Point(Left, Bottom - 1));
    Result := Result or PtInRect(R, Point(Right - 1, Bottom - 1));
  end;
end;

procedure TcxCustomInnerListBox.Click;
begin
  inherited Click;
  FVScrollBar.Position := TopIndex;
  if RootContainer <> nil then
    RootContainer.Click;
  Container.ShowTouchScrollUI(Container, True);
end;

{$IFNDEF DELPHI101BERLIN}
procedure TcxCustomInnerListBox.ChangeScale(M, D: Integer);
begin
  inherited;
  ItemHeight := MulDiv(ItemHeight, M, D);
end;
{$ENDIF}

procedure TcxCustomInnerListBox.DblClick;
begin
  inherited DblClick;
  if RootContainer <> nil then
    RootContainer.DblClick;
end;

procedure TcxCustomInnerListBox.DestroyWindowHandle;
begin
  FIsRedrawLocked := False;
  inherited DestroyWindowHandle;
end;

procedure TcxCustomInnerListBox.DoAutoComplete(var Key: Char);
var
  AIndex: Integer;
  AMsg: TMsg;
begin
  if not AutoComplete then
    Exit;
  if GetTickCount - FPrevKeyPressTime >= AutoCompleteDelay then
    FAutoCompleteFilter := '';
  FPrevKeyPressTime := GetTickCount;

  if Key = Char(VK_BACK) then
  begin
    AIndex := Length(FAutoCompleteFilter);
    while ByteType(FAutoCompleteFilter, AIndex) = mbTrailByte do
      Dec(AIndex);
    Delete(FAutoCompleteFilter, AIndex, Length(FAutoCompleteFilter) - AIndex + 1);
  end
  else
    if dxCharInSet(Key, LeadBytes) then
    begin
      if PeekMessage(AMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
      begin
        FAutoCompleteFilter := FAutoCompleteFilter + Key + Chr(AMsg.wParam);
        Key := #0;
      end;
    end
    else
      FAutoCompleteFilter := FAutoCompleteFilter + Key;

  if Length(FAutoCompleteFilter) > 0 then
  begin
    AIndex := FindAutoCompleteString(FAutoCompleteFilter);
    if AIndex <> -1 then
    begin
      if MultiSelect then
      begin
        ClearSelection;
        SendMessage(Handle, LB_SELITEMRANGE, 1, MakeLParam(AIndex, AIndex));
      end;
      ItemIndex := AIndex;
      Click;
    end;
    if not (Ord(Key) in [VK_RETURN, VK_BACK, VK_ESCAPE]) then
      Key := #0;
  end
  else
  begin
    ItemIndex := 0;
    Click;
  end;
end;

function TcxCustomInnerListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := (RootContainer <> nil) and RootContainer.DoMouseWheel(Shift,
    WheelDelta, MousePos);
  if not Result then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TcxCustomInnerListBox.DoScroll(AKind: TScrollBarKind;
  AScrollCode: TScrollCode; AScrollPos: Integer);
begin
  if AKind = sbHorizontal then
    CallWindowProc(DefWndProc, Handle, WM_HSCROLL, Word(AScrollCode) +
      Word(AScrollPos) shl 16, FHScrollBar.Handle)
  else
    if AScrollCode in [scLineUp, scLineDown] then
      TopIndex := AScrollPos
    else
      if AScrollCode in [scPosition, scTrack] then
        TopIndex := AScrollPos
      else
        CallWindowProc(DefWndProc, Handle, WM_VSCROLL, Word(AScrollCode) +
          Word(AScrollPos) shl 16, FVScrollBar.Handle);
end;

procedure TcxCustomInnerListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if RootContainer <> nil then
    RootContainer.DragOver(Source, Left + X, Top + Y, State, Accept);
  if not Accept then
    inherited;
end;

function TcxCustomInnerListBox.GetPopupMenu: TPopupMenu;
begin
  if RootContainer = nil then
    Result := inherited GetPopupMenu
  else
    Result := RootContainer.GetPopupMenu;
end;

function TcxCustomInnerListBox.GetRootContainer: TcxContainer;
begin
  Result := FContainer;
end;

function TcxCustomInnerListBox.GetScaleFactor: TdxScaleFactor;
begin
  Result := RootContainer.ScaleFactor;
end;

function TcxCustomInnerListBox.GetSizeGripRect: TRect;
var
  R: TRect;
begin
  if IsSizeGripVisible then
  begin
    Result.TopLeft := Parent.ClientToScreen(Point(VScrollBar.Left, FHScrollBar.Top));
    R := cxGetWindowRect(Self);
    Dec(Result.Left, R.Left);
    Dec(Result.Top, R.Top);
    Result.Right := Result.Left + FVScrollBar.Width;
    Result.Bottom := Result.Top + FHScrollBar.Height;
  end
  else
    Result := cxEmptyRect;
end;

procedure TcxCustomInnerListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if RootContainer <> nil then
    RootContainer.KeyDown(Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomInnerListBox.KeyPress(var Key: Char);
begin
  if Key = Char(VK_TAB) then
    Key := #0;
  if (Key <> #0) and (RootContainer <> nil) then
    RootContainer.KeyPress(Key);
  if Key = Char(VK_RETURN) then
    Key := #0;
  if Key <> #0 then
  begin
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, Key);
    if Key <> #0 then
      DoAutoComplete(Key);
  end;
end;

procedure TcxCustomInnerListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) then
    Key := 0;
  if RootContainer <> nil then
    RootContainer.KeyUp(Key, Shift);
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TcxCustomInnerListBox.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  if HandleAllocated then
    Invalidate;
end;

procedure TcxCustomInnerListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if RootContainer <> nil then
  begin
    RootContainer.InnerControlMouseDown := True;
    try
      P := cxClientToParent(Self, Point(X, Y), RootContainer);
      RootContainer.MouseDown(Button, Shift, P.X, P.Y);
    finally
      RootContainer.InnerControlMouseDown := False;
    end;
  end;
end;

procedure TcxCustomInnerListBox.MouseEnter(AControl: TControl);
begin
end;

procedure TcxCustomInnerListBox.MouseLeave(AControl: TControl);
begin
  if RootContainer <> nil then
    RootContainer.ShortRefreshContainer(True);
end;

procedure TcxCustomInnerListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if RootContainer <> nil then
  begin
    P := cxClientToParent(Self, Point(X, Y), RootContainer);
    RootContainer.MouseMove(Shift, P.X, P.Y);
  end;
end;

procedure TcxCustomInnerListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if RootContainer <> nil then
  begin
    P := cxClientToParent(Self, Point(X, Y), RootContainer);
    RootContainer.MouseUp(Button, Shift, P.X, P.Y);
  end;
end;

procedure TcxCustomInnerListBox.DrawSizeGrip(ADC: HDC);
begin
  if (Container <> nil) and UsecxScrollBars and IsSizeGripVisible then
    cxFillSizeGrip(Container, GetSizeGripRect);
end;

function TcxCustomInnerListBox.NeedDrawFocusRect: Boolean;
begin
  Result := not Assigned(OnDrawItem);
end;

procedure TcxCustomInnerListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FHScrollBar then
      FHScrollBar := nil
    else if AComponent = FVScrollBar then
      FVScrollBar := nil;
end;

procedure TcxCustomInnerListBox.RestoreCanvasParametersForFocusRect;
begin
  Canvas.Brush.Color := FPrevBrushColor;
  Canvas.Font.Color := FPrevFontColor;
  TCanvasAccess(Canvas.Canvas).RequiredState([csHandleValid, csBrushValid]);
end;

procedure TcxCustomInnerListBox.SaveCanvasParametersForFocusRect;
begin
  FPrevBrushColor := Canvas.Brush.Color;
  FPrevFontColor := Canvas.Font.Color;
end;

procedure TcxCustomInnerListBox.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  nBar: Integer;
begin
  DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
  if AScrollBarKind = sbHorizontal then
    nBar := SB_HORZ
  else
    nBar := SB_VERT;
  AScrollPos := GetScrollPos(Handle, nBar);
end;

procedure TcxCustomInnerListBox.WndProc(var Message: TMessage);
begin
  if (RootContainer <> nil) and RootContainer.InnerControlMenuHandler(Message) then
    Exit;
  if (Container <> nil) and Container.IsPopupScrollBars and
    FScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
    Container.ShowTouchScrollUI(FContainer, True);
  inherited WndProc(Message);
  case Message.Msg of
    CM_RECREATEWND,
    CM_WININICHANGE,
    LB_ADDSTRING,
    LB_DELETESTRING,
    LB_INSERTSTRING,
    LB_RESETCONTENT,
    LB_SETCARETINDEX,
    LB_SETCURSEL,
    LB_SETHORIZONTALEXTENT,
    LB_SETTOPINDEX,
    WM_MOUSEWHEEL,
    WM_NCCALCSIZE,
    WM_WINDOWPOSCHANGED:
      SetExternalScrollBarsParameters;
    WM_HSCROLL,
    WM_VSCROLL:
      begin
        SetExternalScrollBarsParameters;
        Container.ShowTouchScrollUI(Container, True);
      end;
    WM_SETREDRAW:
      if Message.WParam <> 0 then
        SetExternalScrollBarsParameters;
  end;
end;

procedure TcxCustomInnerListBox.CreateScrollBars;

  procedure InitializeScrollBar(AScrollBar: TcxScrollBar);
  begin
    AScrollBar.SmallChange := 1;
    AScrollBar.Visible := False;
  end;

begin
  FHScrollBar := TcxScrollBar.Create(Self);
  FHScrollBar.FreeNotification(Self);
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.OnScroll := HScrollHandler;
  InitializeScrollBar(FHScrollBar);

  FVScrollBar := TcxScrollBar.Create(Self);
  FVScrollBar.FreeNotification(Self);
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.OnScroll := VScrollHandler;
  InitializeScrollBar(FVScrollBar);
end;

function TcxCustomInnerListBox.FindAutoCompleteString(const S: string): Integer;
begin
  if Style in [lbVirtual, lbVirtualOwnerDraw] then
    Result := DoFindData(S)
  else
    Result := SendMessage(Handle, LB_FINDSTRING, -1, LPARAM(PChar(S)));
end;

function TcxCustomInnerListBox.GetControlContainer: TcxContainer;
begin
  Result := Container;
end;

function TcxCustomInnerListBox.GetControl: TWinControl;
begin
  Result := Self;
end;

procedure TcxCustomInnerListBox.HScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if FHScrollBar.HandleAllocated then
    Scroll(sbHorizontal, ScrollCode, ScrollPos);
end;

function TcxCustomInnerListBox.IsSizeGripVisible: Boolean;
begin
  Result := Container.HScrollBar.Visible and Container.VScrollBar.Visible;
end;

procedure TcxCustomInnerListBox.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxCustomInnerListBox.VScrollHandler(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if FVScrollBar.HandleAllocated then
    Scroll(sbVertical, ScrollCode, ScrollPos);
end;

procedure TcxCustomInnerListBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if RootContainer <> nil then
    with Message do
    begin
      Result := Result or DLGC_WANTCHARS;
      if GetKeyState(VK_CONTROL) >= 0 then
        Result := Result or DLGC_WANTTAB;
    end;
end;

procedure TcxCustomInnerListBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (RootContainer <> nil) and not RootContainer.IsDestroying then
    RootContainer.FocusChanged;
end;

procedure TcxCustomInnerListBox.WMLButtonDown(var Message: TWMLButtonDown);

  function NeedImmediateBeginDrag: Boolean;
  var
    AItemIndex : Integer;
    AShiftState: TShiftState;
  begin
    Result := False;
    AShiftState := KeysToShiftState(Message.Keys);
    if MultiSelect then
      if not(ssShift in AShiftState) or (ssCtrl in AShiftState) then
      begin
        AItemIndex := ItemAtPos(SmallPointToPoint(Message.Pos), True);
        Result := (AItemIndex >= 0) and Selected[AItemIndex];
      end;
  end;

  function NeedBeginDrag: Boolean;
  var
    AShiftState: TShiftState;
  begin
    AShiftState := KeysToShiftState(Message.Keys);
    Result := not(MultiSelect and ((ssCtrl in AShiftState) or
      (ssShift in AShiftState)));
  end;

var
  APrevDragMode: TDragMode;
begin
  if not((RootContainer <> nil) and (DragMode = dmAutomatic) and
    not RootContainer.IsDesigning) then
  begin
    inherited;
    Exit;
  end;

  APrevDragMode := DragMode;
  try
    DragMode := dmManual;
    if NeedImmediateBeginDrag then
    begin
      RootContainer.BeginDrag(False);
      Exit;
    end;
    inherited;
    if NeedBeginDrag then
      RootContainer.BeginDrag(False);
  finally
    DragMode := APrevDragMode;
  end;
end;

procedure TcxCustomInnerListBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if not FScrollBarsCalculating then
    SetExternalScrollBarsParameters;
end;

procedure TcxCustomInnerListBox.WMNCPaint(var Message: TWMNCPaint);
var
  ADC: HDC;
begin
  inherited;
  if UsecxScrollBars and IsSizeGripVisible then
  begin
    ADC := GetWindowDC(Handle);
    try
      DrawSizeGrip(ADC);
    finally
      ReleaseDC(Handle, ADC);
    end;
  end;
end;

procedure TcxCustomInnerListBox.WMPrint(var Message: TWMPrint);
begin
  if UsecxScrollBars and (Message.Flags and PRF_NONCLIENT <> 0) then
    DrawSizeGrip(Message.DC);
  inherited;
end;

procedure TcxCustomInnerListBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (RootContainer <> nil) and not RootContainer.IsDestroying and not(csDestroying in ComponentState)
      and (Message.FocusedWnd <> RootContainer.Handle) then
    RootContainer.FocusChanged;
end;

procedure TcxCustomInnerListBox.WMSetRedraw(var Message: TWMSetRedraw);
begin
  inherited;
  FIsRedrawLocked := Message.Redraw = 0;
  if not (csDestroying in ComponentState) and not FIsRedrawLocked then
    SetExternalScrollBarsParameters;
end;

procedure TcxCustomInnerListBox.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  ARgn: HRGN;
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    ARgn := CreateRectRgnIndirect(GetSizeGripRect);
    SendMessage(Handle, WM_NCPAINT, ARgn, 0);
    DeleteObject(ARgn);
  end;
end;

procedure TcxCustomInnerListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxCustomInnerListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxCustomInnerListBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    LBN_DBLCLK:
      DblClick;
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
  end;
end;

procedure TcxCustomInnerListBox.CNDrawItem(var Message: TWMDrawItem);
var
  ACanvas: TCanvas;
  AItemState: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    AItemState := TOwnerDrawState(LongRec(itemState).Lo);
    ACanvas := inherited Canvas;
    ACanvas.Handle := hDC;
    ACanvas.Font := Font;
    ACanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in AItemState) then
    begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, AItemState)
    else
      ACanvas.FillRect(rcItem);
    if (odFocused in AItemState) and NeedDrawFocusRect then
      DrawFocusRect(hDC, rcItem);
    ACanvas.Handle := 0;
  end;
end;

procedure TcxCustomInnerListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Container.IsPopupScrollBars then
    Params.Style := Params.Style or LBS_DISABLENOSCROLL;
end;

procedure TcxCustomInnerListBox.CreateWnd;
begin
  inherited CreateWnd;
  Container.ClearSavedChildControlRegions;
  RequestAlign;
end;

procedure TcxCustomInnerListBox.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiPan then
    if gfBegin in EventInfo.Flags then
      Container.ShowTouchScrollUI(Container)
    else
      if gfEnd in EventInfo.Flags then
        Container.HideTouchScrollUI(Container);
  Handled := False;
end;

procedure TcxCustomInnerListBox.DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions);
begin
  if Container <> nil then
  begin
    Gestures := Container.Touch.InteractiveGestures;
    Options := Container.Touch.InteractiveGestureOptions;
  end
  else
    inherited DoGetGestureOptions(Gestures, Options);
end;

procedure CloseUnrelatedPopupsOnGesture(AWnd: HWND);
begin
  CloseUnrelatedPopups(AWnd);
end;

procedure CloseUnrelatedPopupsOnMouseClick(AWnd: HWND; const APoint: TPoint;
  AMouseMessage: Cardinal; out AHandled: Boolean);

  function MDIParentOrAnotherMDIChild(APopupWindow: TcxCustomPopupWindow; AWnd: HWND): Boolean;
  var
    AMDIChildForm, AMDIParentForm: TCustomForm;
    AMDIClientHandle, AParentHandle: THandle;
    AParentForm: TCustomForm;
    I, J: Integer;
  begin
    AParentForm := GetParentForm(APopupWindow.OwnerControl);
    AParentHandle := GetParent(AParentForm.Handle);
    Result := AParentHandle = AWnd;
    if Result then
      Exit;
    for I := 0 to Screen.FormCount - 1 do
    begin
      AMDIParentForm := Screen.Forms[I];
      if (TCustomFormAccess(AMDIParentForm).FormStyle = fsMDIForm) and (TCustomFormAccess(AMDIParentForm).ClientHandle <> 0) then
      begin
        AMDIClientHandle := TCustomFormAccess(AMDIParentForm).ClientHandle;
        if AParentHandle = AMDIClientHandle then // TODO Check CLX
        begin
          Result := HasNativeHandle(AMDIParentForm, AWnd, True);
          if Result then
            Break;

          for J := 0 to TCustomFormAccess(AMDIParentForm).MDIChildCount - 1 do
          begin
            AMDIChildForm := TCustomFormAccess(AMDIParentForm).MDIChildren[J];
            if AMDIChildForm = AParentForm then
              Continue;
            Result := HasNativeHandle(AMDIChildForm, AWnd, True);
            if Result then
              Break;
          end;

          Break;
        end;
      end;
    end;
  end;

  function InternalNeedIgnoreMouseMessageAfterCloseUp(AWnd: THandle;
    APopupWindow: TcxCustomPopupWindow): Boolean;
  begin
    Result := APopupWindow.NeedIgnoreMouseMessageAfterCloseUp(AWnd, AMouseMessage,
      KeyboardStateToShiftState, APoint);
  end;

var
  I: Integer;
  ACallNextHook, ANeedCheckIgnoreMouseMessage: Boolean;
  APopupWindow: TcxCustomPopupWindow;
  AParentForm: TCustomForm;
begin
  AHandled := False;
  ACallNextHook := True;
  I := 0;
  while I < cxPopupVisibleController.Count do
  begin
    ANeedCheckIgnoreMouseMessage := True;
    APopupWindow := cxPopupVisibleController[I];
    if APopupWindow.CaptureFocus and not APopupWindow.Active or
      HasNativeHandle(APopupWindow, AWnd, True) then
      Inc(I)
    else
    begin
      AParentForm := GetParentForm(APopupWindow.OwnerControl);
      if CheckParentsNativeHandle(APopupWindow.OwnerControl, AWnd) or
        ((AParentForm is TcxCustomPopupWindow) and not TcxCustomPopupWindow(AParentForm).IsVisible) then
      begin
        if HasNativeHandle(APopupWindow.OwnerControl, AWnd, True) then
          if (AMouseMessage = WM_LBUTTONDOWN) or (AMouseMessage = WM_LBUTTONDBLCLK) then
            if PtInRect(APopupWindow.OwnerScreenBounds, APoint) then
            begin
              ACallNextHook := False;
              if InternalNeedIgnoreMouseMessageAfterCloseUp(AWnd, APopupWindow) then
                APopupWindow.FJustClosed := True;
              ANeedCheckIgnoreMouseMessage := False;
            end;

        FCaptionInactivationLocked := True;
        APopupWindow.LockDeactivate(True);
        try
          if not AHandled and ANeedCheckIgnoreMouseMessage then
            AHandled := InternalNeedIgnoreMouseMessageAfterCloseUp(AWnd, APopupWindow);
          APopupWindow.CloseUp;
          if not ACallNextHook and ((csDestroying in APopupWindow.OwnerControl.ComponentState)
            or not APopupWindow.OwnerControl.Visible) then
            AHandled := True;
        finally
          APopupWindow.LockDeactivate(False);
          FCaptionInactivationLocked := False;
        end;
        I := 0;
      end
      else
      begin
        AParentForm := GetParentForm(APopupWindow.OwnerControl);
        if (AParentForm <> nil) and (TCustomFormAccess(AParentForm).FormStyle = fsMDIChild) and MDIParentOrAnotherMDIChild(APopupWindow, AWnd) then
        begin
          AHandled := AHandled or InternalNeedIgnoreMouseMessageAfterCloseUp(AWnd, APopupWindow);
          APopupWindow.CloseUp;
          I := 0;
        end
        else
          Inc(I);
      end;
    end;
  end;
end;

procedure cxContainerWndProcHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);

  function IsParentFormPlaced(AHandle1, AHandle2: THandle): Boolean;
  var
    AForm1, AForm2: TCustomForm;
  begin
    AForm1 := dxGetParentForm(AHandle1);
    AForm2 := dxGetParentForm(AHandle2);

    Result := (AForm1 <> AForm2) and (AForm1 <> nil) and (AForm2 <> nil) and
      (AForm1.HandleAllocated) and (AForm2.HandleAllocated) and
      (IsChildEx(AForm1.Handle, AForm2.Handle) and Supports(AForm2, IdxPlaceForm) or
       IsChildEx(AForm2.handle, AForm1.Handle) and Supports(AForm1, IdxPlaceForm));


  end;

var
  AParentForm: TCustomForm;
  AParentWindow: HWND;
  APopupWindow: TcxCustomPopupWindow;
  I, J: Integer;
begin
  if (ACode <> HC_ACTION) or (dxMessagesController = nil) then
    Exit;

  with Windows.PCWPStruct(LParam)^ do
    dxMessagesController.BlockLockedMessage(hwnd, message);

  with Windows.PCWPStruct(LParam)^ do
    case message of
      WM_SETFOCUS:
        if IsParentFormPlaced(wparam, hwnd) then
          dxPerformMessageByQueue(dxFindParentControl(hwnd), CM_ENTER);
      WM_KILLFOCUS:
        if IsParentFormPlaced(wparam, hwnd) then
          dxPerformMessageByQueue(dxFindParentControl(hwnd), CM_EXIT);
    end;

  if not cxPopupVisibleController.IsAnyVisible then
    Exit;

  with Windows.PCWPStruct(LParam)^ do
    case message of
      WM_NCACTIVATE:
        if wParam = 0 then
          if FCaptionInactivationLocked then
            dxMessagesController.BlockMessage(hwnd)
          else
            for I := 0 to cxPopupVisibleController.Count - 1 do
            begin
              APopupWindow := cxPopupVisibleController[I];
              if csDestroying in APopupWindow.ComponentState then
                Continue;
              AParentForm := GetParentForm(APopupWindow.OwnerControl);
              if (AParentForm <> nil) and (AParentForm.Handle = hwnd) then
                dxMessagesController.BlockMessage(hwnd);
            end;

      WM_ACTIVATEAPP:
          if wParam = 0 then
          begin
            I := 0;
            while I < cxPopupVisibleController.Count do
            begin
              APopupWindow := cxPopupVisibleController[I];
              AParentForm := GetParentForm(APopupWindow.OwnerControl);
              if AParentForm <> nil then
                PostMessage(AParentForm.Handle, WM_NCACTIVATE, 0, 0);
              if APopupWindow.CaptureFocus and APopupWindow.Active or not IsWindowEnabled(APopupWindow.Handle) then
              begin
                Inc(I);
                Continue;
              end;
              APopupWindow.CloseUp;
              if APopupWindow.OwnerControl is TcxContainer then
                TcxContainer(APopupWindow.OwnerControl).FocusChanged;
              I := 0;
            end;
          end;

      WM_DESTROY:
        for I := 0 to cxPopupVisibleController.Count - 1 do
        begin
          APopupWindow := cxPopupVisibleController[I];
          if csDestroying in APopupWindow.ComponentState then
            Continue;
          if HasNativeHandle(APopupWindow, hwnd) then
          begin
            APopupWindow.Close;
            if not APopupWindow.IsVisible and
               not (csDestroying in APopupWindow.ComponentState) and
                APopupWindow.FTerminateOnDestroy then
                  Application.Terminate;
            Break;
          end;
        end;

      WM_CLOSE:
        for I := 0 to cxPopupVisibleController.Count - 1 do
        begin
          APopupWindow := cxPopupVisibleController[I];
          if csDestroying in APopupWindow.ComponentState then
            Continue;
          if HasNativeHandle(APopupWindow, hwnd) then
          begin
            if APopupWindow.CaptureFocus and not(csDestroying in APopupWindow.OwnerControl.ComponentState) then
              AParentWindow := FindFirstNonChildParentWindow(APopupWindow.OwnerControl.Handle)
            else
              AParentWindow := 0;
            if APopupWindow.CloseQuery then
              APopupWindow.CloseUp;
            if not APopupWindow.IsVisible and (AParentWindow <> 0) then
              SendMessage(AParentWindow, WM_CLOSE, 0, 0);
            Break;
          end;
          if not APopupWindow.CaptureFocus and not(csDestroying in APopupWindow.OwnerControl.ComponentState) then
          begin
            AParentForm := GetParentForm(APopupWindow.OwnerControl);
            if not AParentForm.HandleAllocated or HasNativeHandle(AParentForm, hwnd) then
              APopupWindow.CloseUp;
            Break;
          end;
        end;

      WM_GESTURE:
        CloseUnrelatedPopupsOnGesture(hwnd);

      WM_SHOWWINDOW:
        if wParam = 0 then
          for I := 0 to cxPopupVisibleController.Count - 1 do
          begin
            APopupWindow := cxPopupVisibleController[I];
            if csDestroying in APopupWindow.ComponentState then
              Continue;
            with APopupWindow.OwnerControl do
              if (csDestroying in ComponentState) or not HandleAllocated then
                Continue;
            if IsRelatedWindow(hwnd, APopupWindow.Handle) then
            begin
              for J := cxPopupVisibleController.Count - 1 downto I do
                cxPopupVisibleController[J].CloseUp;
              Break;
            end;
          end;

      WM_WINDOWPOSCHANGED:
        begin
          I := 0;
          while I < cxPopupVisibleController.Count do
          begin
            APopupWindow := cxPopupVisibleController[I];
            if (csDestroying in APopupWindow.ComponentState) or not APopupWindow.HandleAllocated then
            begin
              Inc(I);
              Continue;
            end;
            with APopupWindow.OwnerControl do
              if (csDestroying in ComponentState) or not HandleAllocated then
              begin
                Inc(I);
                Continue;
              end;
            if not HasNativeHandle(APopupWindow.OwnerControl, hwnd) then
            begin
              Inc(I);
              Continue;
            end;
            if IsControlVisible(APopupWindow.OwnerControl) then
              Inc(I)
            else
            begin
              APopupWindow.CloseUp;
              I := 0;
              Continue;
            end;
          end;
        end;
    end;
end;

procedure cxContainerGetMessageHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  APMsg: PMSG;
  APopupWindow: TcxCustomPopupWindow;
begin
  APMsg := PMSG(LParam);
  if cxPopupVisibleController.IsAnyVisible and
    (ACode = HC_ACTION) and (WParam = PM_REMOVE) and
    (APMsg^.message = WM_SYSKEYDOWN) then
  begin
    APopupWindow := cxPopupVisibleController[cxPopupVisibleController.Count - 1];
    if (not APopupWindow.CaptureFocus or not APopupWindow.ModalMode) and
        not APopupWindow.IsSysKeyAccepted(APMsg^.wParam) then
      APMsg^.message := 0;
  end;
end;

procedure cxContainerMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AHandled: Boolean;
begin
  if (ACode = HC_ACTION) and IsMouseDownMessage(WParam) and cxPopupVisibleController.IsAnyVisible then
  begin
    CloseUnrelatedPopupsOnMouseClick(PMouseHookStruct(LParam)^.hwnd, PMouseHookStruct(LParam)^.pt, wParam, AHandled);
    if AHandled then
      AHookResult := 1;
  end;
end;

procedure RemoveHooks;
begin
  dxReleaseHook(cxContainerMouseHook);
  dxReleaseHook(cxContainerGetMessageHook);
  dxReleaseHook(cxContainerWndProcHook);
end;

procedure SetHooks;
begin
  dxSetHook(htWndProc, cxContainerWndProcHook);
  dxSetHook(htGetMessage, cxContainerGetMessageHook);
  dxSetHook(htMouse, cxContainerMouseHook);
end;

initialization
  SetUsecxScrollBars;
  StartClassGroup(TControl);
  GroupDescendentsWith(TcxStyleController, TControl);
  if not FSetHooksOnlyWhenPopupsAreVisible then
    SetHooks;
  cxControls.cxGetParentWndForDocking := GetPopupOwnerControl;

finalization
  cxControls.cxGetParentWndForDocking := nil;
  RemoveHooks;
  FreeAndNil(FcxPopupVisibleController);

end.

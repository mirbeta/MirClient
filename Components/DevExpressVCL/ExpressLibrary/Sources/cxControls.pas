{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit cxControls;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Windows, Messages, MultiMon, Types, Forms, Controls, Graphics, Dialogs, Classes, Themes,
  StdCtrls, Menus, ExtCtrls, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxMessages, cxGraphics, cxScrollBar, cxGeometry, cxDrawTextUtils,
  dxTouch, cxLookAndFeels, cxLookAndFeelPainters, dxForms, dxCoreGraphics, dxGDIPlusClasses, dxAnimation, cxClasses;

const
  FAPPCOMMAND_MASK = $0000F000;
 {$EXTERNALSYM FAPPCOMMAND_MASK}
  APPCOMMAND_BROWSER_BACKWARD = 1;
 {$EXTERNALSYM APPCOMMAND_BROWSER_BACKWARD}
  APPCOMMAND_BROWSER_FORWARD  = 2;
 {$EXTERNALSYM APPCOMMAND_BROWSER_FORWARD}

 cxDesignSelectorIndentFromBorder = 8;
 cxDesignSelectorSize = 15;

const
  cxInvisibleCoordinate = 30000;

type
  TcxSystemMenuType = (smSystem, smChild, smDialog);

  TcxHandle = HWND;

  TcxDragDetect = (ddNone, ddDrag, ddCancel);

  TcxNumberType = (ntInteger, ntFloat, ntExponent);

{$IFDEF DELPHI17}
  TcxScrollStyle = System.UITypes.TScrollStyle;
{$ELSE}
  TcxScrollStyle = StdCtrls.TScrollStyle;
{$ENDIF}
  TScrollBarKinds = set of TScrollBarKind;

  TcxScrollControlContent = procedure (ADirection: TcxDirection) of object;

  TcxRecordScrollMode = (rsmDefault, rsmByRecord, rsmByPixel);

  TLBGetItemRect = packed record
    Msg: Cardinal;
    ItemIndex: Integer;
    Rect: PRect;
    Result: Longint;
  end;

  TcxControl = class;
  TcxControlScrollBar = class;
  TdxHybridScrollbarsManager = class;

  TDragControlObjectClass = class of TDragControlObject;

  { IdxPopupControl }

  IdxPopupControl = interface
  ['{631D4C30-8543-4A08-A50E-9ABA2FA7EF33}']
    function GetOwnerControl: TWinControl;
  end;

  { IdxSystemInfoListener }

  IdxSystemInfoListener = interface
  ['{F137963E-6165-47F9-A4C7-96BB4EB91AE0}']
    procedure Changed(AParameter: Cardinal);
  end;

  { IcxMouseCaptureObject }

  IcxMouseCaptureObject = interface
    ['{ACB73657-6066-4564-9A3D-D4D0273AA82F}']
    procedure DoCancelMode;
  end;

  { IcxMouseTrackingCaller }

  IcxMouseTrackingCaller = interface
    ['{84A4BCBE-E001-4D60-B7A6-75E2B0DCD3E9}']
    procedure MouseLeave;
  end;

  { IcxMouseTrackingCaller2 }

  IcxMouseTrackingCaller2 = interface(IcxMouseTrackingCaller)
    ['{3A5D973B-F016-4F22-80B6-1D35668D7743}']
    function PtInCaller(const P: TPoint): Boolean;
  end;

  { IcxMouseTrackingCaller3 }

  IcxMouseTrackingCaller3 = interface(IcxMouseTrackingCaller2)
  ['{94877AC7-D2D8-4F2B-8396-DF3CB45E01DF}']
    function IsCaptureMouse: Boolean;
  end;

  { IcxCompoundControl }

  IcxCompoundControl = interface
  ['{A4A77F28-1D03-425B-9A83-0B853B5D8DEF}']
    function GetActiveControl: TWinControl;
    property ActiveControl: TWinControl read GetActiveControl;
  end;

  { IcxControlComponentState }

  IcxControlComponentState = interface
  ['{E29BF582-E8C0-4868-A799-DB4C41AC3BC8}']
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;
  end;

  { IcxPopupMenu }

  IcxPopupMenu = interface
  ['{61EEDA7D-88CC-45BF-8A00-5C25174D6501}']
    function IsShortCutKey(var Message: TWMKey): Boolean;
    procedure Popup(X, Y: Integer);
  end;

  { IcxPopupMenu2 }

  IcxPopupMenu2 = interface(IcxPopupMenu)
  ['{E6DC09CE-3252-457B-B6D2-09D8335C2DED}']
    procedure Popup(X, Y: Integer; const AOwnerBounds: TRect; APopupAlignment: TPopupAlignment);
  end;

  { IcxTransparentControl }

  IcxTransparentControl = interface
  ['{F4980C69-029E-4B14-B3AD-953DA5C18BE7}']
    function IsTransparentRegionsPresent: Boolean;
  end;

  { IdxPlaceForm }

  IdxPlaceForm = interface
  ['{DEA877D7-1646-477A-A722-818DFE41EE47}']
  end;

  { IdxDialogOwner }

  IdxDialogOwner = interface
  ['{09028781-0230-48C9-A8D3-02FD987F872B}']
    function GetLookAndFeel: TcxLookAndFeel;
    function GetOwner: TComponent;
    function GetParentForm: TCustomForm;
  end;

  { IdxNavigationItem }

  IdxNavigationItem = interface
  ['{9B9F932E-4E46-4BBB-B532-503F739A4559}']
    function GetID: Integer;
    function GetText: string;
    function GetImageIndex: TcxImageIndex;
    property ID: Integer read GetID;
    property Text: string read GetText;
    property ImageIndex: TcxImageIndex read GetImageIndex;
  end;

  { IdxNavigationClientListener }

  IdxNavigationClientListener = interface
  ['{54AC756F-768D-46CD-84BF-8E6C49D2ABAF}']
    procedure ItemsChanged;
    procedure SelectionChanged;
  end;

  { IdxNavigationClient }

  IdxNavigationClient = interface
  ['{2DB4C89E-A411-4D9E-904D-87898439B315}']
    function GetItems: IEnumerable<IdxNavigationItem>;
    procedure SetSelectedItem(AItem: IdxNavigationItem);
    function GetSelectedItem: IdxNavigationItem;
    procedure AddListener(AListener: IdxNavigationClientListener);
    procedure RemoveListener(AListener: IdxNavigationClientListener);
    property SelectedItem: IdxNavigationItem read GetSelectedItem write SetSelectedItem;
  end;

  IdxTouchScrollUIOwner = interface
  ['{CA4C2043-1558-4D5D-8DEB-DAD8652AFFB9}']
    procedure CheckUIPosition;
    function GetOwnerControl: TcxControl;
    function HasVisibleUI: Boolean;
    procedure HideUI;
  end;

  IdxHybridScrollbarOwner = interface(IdxTouchScrollUIOwner)
  ['{2DFA6D14-84FD-42EE-AF0C-CB418ABCA905}']
    function GetBaseColor: TColor;
    function GetManager: TdxHybridScrollbarsManager;
    procedure Invalidate;
  end;

  { TdxApplicationActivateWindowHelper }

  TdxApplicationActivateWindowHelper = class
  private
    FFreeNotificator: TcxFreeNotificator;
    FWindowsList: TList;

    procedure FreeNotificationHandler(Sender: TComponent);
    function IsFormShowedBeforeLastModalWindow(AForm: TCustomForm): Boolean;
    function NeedNormalize(AForm: TCustomForm): Boolean;
    procedure NormalizeFocusedWindow;
    procedure NormalizeRegisteredWindows;
  protected
    property WindowsList: TList read FWindowsList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddWindow(AForm: TCustomForm);
    procedure NormalizeTopMostWindows(AForm: TCustomForm);
    procedure RemoveWindow(AForm: TCustomForm);
  end;

  { control child component }

  TcxControlChildComponent = class(TcxComponent,
    IdxScaleFactor)
  strict private
    FControl: TcxControl;
    FScaleFactor: TdxOwnedScaleFactor;

    function GetIsLoading: Boolean;
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    function GetIsDestroying: Boolean; virtual;
    procedure Initialize; virtual;
    procedure ScaleFactorChanged(M, D: Integer); virtual;
    procedure SetControl(Value: TcxControl); virtual;
    //
    property ScaleFactor: TdxOwnedScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True); virtual;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetParentComponent(Value: TComponent); override;

    property Control: TcxControl read FControl write SetControl;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
  end;

  { scrollbar and size grip }

  TcxScrollBarData = class
  private
    FAllowHide: Boolean;
    FAllowShow: Boolean;
    FEnabled: Boolean;
    FIsValid: Boolean;
    FLargeChange: TScrollBarInc;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FPageSize: Integer;
    FSmallChange: TScrollBarInc;
    FVisible: Boolean;
  protected
    procedure Validate;
  public
    property AllowHide: Boolean read FAllowHide write FAllowHide;
    property AllowShow: Boolean read FAllowShow write FAllowShow;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IsValid: Boolean read FIsValid;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange;
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    property Position: Integer read FPosition write FPosition;
    property PageSize: Integer read FPageSize write FPageSize;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange;
    property Visible: Boolean read FVisible write FVisible;
  end;

  IcxControlScrollBar = interface
    ['{D70735C6-82E8-4D17-AE14-9317D2A11D0C}']
    procedure ApplyData;
    function GetControl: TcxControlScrollBar;
    function GetBoundsRect: TRect;
    function GetData: TcxScrollBarData;
    function GetEnabled: Boolean;
    function GetHeight: Integer;
    function GetKind: TScrollBarKind;
    function GetLargeChange: TScrollBarInc;
    function GetLeft: Integer;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetSmallChange: TScrollBarInc;
    function GetTop: Integer;
    function GetUnlimitedTracking: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetLargeChange(AValue: TScrollBarInc);
    procedure SetLeft(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetScrollParams(AMin, AMax, APosition, APageSize: Integer; ARedraw: Boolean = True);
    procedure SetSmallChange(AValue: TScrollBarInc);
    procedure SetTop(AValue: Integer);
    procedure SetUnlimitedTracking(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Control: TcxControlScrollBar read GetControl;
    property Data: TcxScrollBarData read GetData;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Height: Integer read GetHeight write SetHeight;
    property Kind: TScrollBarKind read GetKind write SetKind;
    property LargeChange: TScrollBarInc read GetLargeChange write SetLargeChange;
    property Left: Integer read GetLeft write SetLeft;
    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property Position: Integer read GetPosition write SetPosition;
    property SmallChange: TScrollBarInc read GetSmallChange write SetSmallChange;
    property Top: Integer read GetTop write SetTop;
    property UnlimitedTracking: Boolean read GetUnlimitedTracking write SetUnlimitedTracking;
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TcxControlScrollBar = class(TcxScrollBar, IcxControlScrollBar)
  strict private
    FData: TcxScrollBarData;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    OwnerControl: TcxControl;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    procedure DoWndProc(var Message: TMessage); virtual;
    procedure FocusParent; virtual;
    procedure InitControl; virtual;
    function IsActuallyVisible: Boolean; virtual;
    procedure SetZOrder(TopMost: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TObject); reintroduce; virtual;
    destructor Destroy; override;
    // IcxControlScrollBar
    procedure ApplyData; virtual;
    function GetBoundsRect: TRect;
    function GetControl: TcxControlScrollBar;
    function GetData: TcxScrollBarData;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetOwnerControlRelativeBounds(const ABounds: TRect); virtual;
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean); virtual;
    procedure SetWidth(AValue: Integer);
    property Control: TcxControlScrollBar read GetControl;
    property Visible: Boolean read GetVisible write SetVisible;
  end;
  TcxControlScrollBarClass = class of TcxControlScrollBar;

  TcxPopupScrollBarViewInfo = class(TcxScrollBarViewInfo)
  protected
    function GetArrowButtonLength: Integer; override;
  end;

  TcxPopupScrollBarPainter = class(TcxScrollBarPainter)
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState); override;
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect); override;
    function FadingAvailable: Boolean; override;
  public
    function IsButtonHotTrack: Boolean; override;
  end;

  TdxHybridScrollBarPainter = class(TcxScrollBarPainter)
  strict private
    procedure DrawArrowButton(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
    procedure DrawThumb(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
    function GetBaseColor: TColor;
    function GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
    function GetVisibleSize: Integer;
    function IsActive: Boolean;
    function IsRightToLeft: Boolean;
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState); override;
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect); override;
    function FadingAvailable: Boolean; override;
  public
    function IsButtonHotTrack: Boolean; override;
  end;

  TcxCustomPopupScrollBarHelper = class(TcxScrollBarHelper)
  protected
    procedure InvalidateRect(const ARect: TRect; AEraseBackground: Boolean = False); override;
    function NeedBuffering: Boolean; override;
  end;

  TcxPopupScrollBarHelper = class(TcxCustomPopupScrollBarHelper)
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
    function GetScrollBarSize: TSize; override;
    function GetViewInfoClass: TcxScrollBarViewInfoClass; override;
  end;

  TdxHybridScrollBarHelper = class(TcxCustomPopupScrollBarHelper)
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
    function GetScrollBarSize: TSize; override;
  end;

  TdxTouchScrollUIActivityHelper = class
  strict private
    FLastMousePos: TPoint;
    FMouseEnter: Boolean;
  public
    function CheckScrollActivity(AControl: TWinControl; var Message: TMessage): Boolean;
  end;

  TdxTouchScrollUIModeManager = class
  strict private
    class var
      FActiveScrollUIOwner: TObject;
      FHidingLockCount: Integer;
      FHidingTimer: TTimer;
      FLockOwner: Boolean;
      FScrollInfoAllowVisible: Boolean;
{$IFDEF DELPHIXE}
    class destructor Finalize;
{$ELSE}
  {$HINTS OFF} // #Ch http://qc.embarcadero.com/wc/qcmain.aspx?d=80785
    class destructor Finalize;
  {$HINTS ON}
{$ENDIF}
    class procedure Activate(AValue: IdxTouchScrollUIOwner);
    class function ActiveScrollUIOwner: IdxTouchScrollUIOwner;
    class procedure DoHideTouchScrollUI(AImmediately: Boolean);
    class procedure HidingTimerHandler(ASender: TObject);
    class procedure LockHiding;
    class procedure StartHidingTimer;
    class procedure StopHidingTimer;
    class procedure UnlockHiding;
  protected
    class procedure CheckUIPosition(AChangedWnd: HWND);
    class procedure CheckUIVisibility(AChangedWnd: HWND);
    class function IsActive(AUIOwner: IdxTouchScrollUIOwner): Boolean;
    class procedure LockOwner;
    class procedure UnlockOwner;
  public
    class procedure Deactivate(AUIOwner: IdxTouchScrollUIOwner);
    class procedure HideScrollUI(AUIOwner: IdxTouchScrollUIOwner; AImmediately: Boolean);
    class function IsScrollUIHidden(AUIOwner: IdxTouchScrollUIOwner): Boolean;
    class procedure ShowScrollUI(AUIOwner: IdxTouchScrollUIOwner; AControlledByTimer: Boolean);
  end;

  TdxHybridScrollbarsManager = class
  strict private const
    MaxFadingValue = 255;
    MaxVisibleSize = 100;
  strict private type
    TExpandState = (estNone, estExpanding, estCollapsing);
    TFadingState = (fstNone, fstShowing, fstHiding);
  strict private
    // fade animation
    FFadingState: TFadingState;
    FFadingAnimation: TdxAnimationTransition;
    FFadingValue: Byte;
    FStartFadingValue: Byte;
    // Size animation
    FExpandState: TExpandState;
    FSizeAnimation: TdxAnimationTransition;
    FCollapsingTimer: TcxTimer;
    FExpandLockCount: Integer;
    FStartSize: Integer;
    FVisibleSize: Integer;

    FAllowVisible: Boolean;
    FAnimateObject: TObject;
    FHidingLockCount: Integer;
    FHidingTimer: TTimer;
    FIsDeactivating: Boolean;
    FOwner: TObject;

    procedure CheckStartHidingTimer;
    procedure DoHide(AAnimated: Boolean);
    function GetScrollbarsOwner: IdxHybridScrollbarOwner;
    procedure Invalidate;
    procedure HidingTimerHandler(ASender: TObject);
    procedure StartHidingTimer;
    procedure StopHidingTimer;
    function UseAnimation: Boolean;
    // fade animation
    procedure FadeOut;
    procedure FadeIn;
    procedure DoFadingAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure DoFadingAnimationTerminate(Sender: TObject);
    procedure StartFadingAnimation(AState: TFadingState);
    procedure StopFadingAnimation;
    // Size animation
    procedure DoAfterCollapse;
    procedure DoAfterExpand;
    procedure DoExpand;
    procedure DoCollapse;
    procedure DoSizeAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure DoSizeAnimationTerminate(Sender: TObject);
    procedure SizeChangingTimerHandler(ASender: TObject);
    procedure StartSizeAnimation(AState: TExpandState);
    procedure StartSizeChangingTimer;
    procedure StopSizeAnimation;
    procedure StopSizeChangingTimer;

    function IsWaitingForCollapse: Boolean;
    function IsWaitingForHide: Boolean;
  protected
    procedure Collapse;
    procedure Deactivate;
    procedure Expand(AAnimateObject: TObject);
    function GetFadingValue: Byte;
    function GetVisibleSize: Integer;
    function IsActive(AScrollbar: TObject): Boolean;
    procedure ScrollbarHiding(AScrollbar: TObject);
    procedure ResetSize;

    procedure CheckScrollbarsPosition(AChangedWnd: HWND);
    procedure CheckScrollbarsVisibility(AChangedWnd: HWND);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    procedure Hide(AImmediately: Boolean; AAnimated: Boolean);
    procedure Show(AHideByTimer: Boolean);

    property AllowVisible: Boolean read FAllowVisible;
    property ScrollbarsOwner: IdxHybridScrollbarOwner read GetScrollbarsOwner;
  end;

  TdxHybridScrollbarManagers = class
  strict private
    class var
      FItems: TList<TdxHybridScrollbarsManager>;
    class constructor Initialize;
{$IFDEF DELPHIXE}
    class destructor Finalize;
{$ELSE}
  {$HINTS OFF} // #Ch http://qc.embarcadero.com/wc/qcmain.aspx?d=80785
    class destructor Finalize;
  {$HINTS ON}
{$ENDIF}
  protected
    class procedure CheckScrollbarsPosition(AChangedWnd: HWND);
      class procedure CheckScrollbarsVisibility(AChangedWnd: HWND);
  public
    class procedure Activate(AManager: TdxHybridScrollbarsManager);
    class procedure Deactivate(AManager: TdxHybridScrollbarsManager);
    class function IsActive(AManager: TdxHybridScrollbarsManager): Boolean;
  end;

  TcxControlPopupScrollBar = class(TcxControlScrollBar, IdxPopupControl)
  strict private
    FActivityHelper: TdxTouchScrollUIActivityHelper;
    FAlpha: Byte;
    FAllowVisible: Boolean;
    FMouseEnter: Boolean;
    FScrollbarOwner: TObject;
    FVisibleSize: Integer;
    function GetMaxVisibleSize: Integer;
    function GetMinVisibleSize: Integer;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  private
    procedure UpdateLayer;
  protected
    //IdxPopupControl
    function GetOwnerControl: TWinControl;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DestroyWnd; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoPaint(ACanvas: TcxCanvas); override;
    function GetInitialSize: TSize; override;
    function GetHelperClass: TcxScrollBarHelperClass; override;
    function GetScrollbarOwner: IdxTouchScrollUIOwner;
    function GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
    procedure InitControl; override;
    procedure InternalSetOwnerControlRelativeBounds(const ABounds, AVisibleBounds: TRect);
    function IsActuallyVisible: Boolean; override;
    procedure Resize; override;
    procedure DoWndProc(var Message: TMessage); override;

    property AllowVisible: Boolean read FAllowVisible;
    property VisibleSize: Integer read FVisibleSize;
  public
    constructor CreateEx(AOwner: TObject); override;
    destructor Destroy; override;
    // IcxControlScrollBar
    procedure ApplyData; override;
    procedure SetVisible(AValue: Boolean); override;
    //
    procedure Invalidate; override;
    procedure SetOwnerControlRelativeBounds(const ABounds: TRect); override;
  end;

  TcxControlPopupScrollBarClass = class of TcxControlPopupScrollBar;

  TcxControlScrollBarHelper = class(TcxScrollBarHelper, IcxControlScrollBar)
  private
    FData: TcxScrollBarData;
  public
    constructor Create(AOwner: IcxScrollBarOwner); override;
    destructor Destroy; override;
    procedure Repaint; override;
    // IcxControlScrollBar
    procedure ApplyData;
    function GetControl: TcxControlScrollBar;
    function GetData: TcxScrollBarData;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
    property Control: TcxControlScrollBar read GetControl;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;
  TcxControlScrollBarHelperClass = class of TcxControlScrollBarHelper;

  { TcxSizeGrip }

  TcxSizeGripClass = class of TcxSizeGrip;
  TcxSizeGrip = class(TCustomControl)
  strict private
    FOwnerControl: TcxControl;
    FLookAndFeel: TcxLookAndFeel;
    procedure WMEraseBkgnd(var AMessage: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure InitControl; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure Paint; override;
    procedure SetZOrder(TopMost: Boolean); override;
    property OwnerControl: TcxControl read FOwnerControl;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TObject); reintroduce; virtual;
    destructor Destroy; override;
    class function IsAvailable(AStatusBar: TWinControl): Boolean;
    procedure SetOwnerControlRelativeBounds(const ABounds: TRect); virtual;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
  end;

  { TcxPopupSizeGrip }

  TcxPopupSizeGrip = class(TcxSizeGrip)
  strict private
    FScrollbarOwner: TObject;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Draw(ACanvas: TCanvas); override;
    function GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
    function GetScrollbarOwner: IdxTouchScrollUIOwner;
    procedure InitControl; override;
  public
    constructor CreateEx(AOwner: TObject); override;
    procedure SetOwnerControlRelativeBounds(const ABounds: TRect); override;
  end;

  { TcxSizeGripHelper }

  TcxSizeGripHelperClass = class of TcxSizeGripHelper;
  TcxSizeGripHelper = class
  strict private
    FBoundsRect: TRect;
    FIsNonClient: Boolean;
    FOwner: IcxScrollBarOwner;
    FVisible: Boolean;

    procedure RefreshNCPart(const ARect: TRect);
  protected
    property Owner: IcxScrollBarOwner read FOwner;
  public
    constructor Create(AOwner: IcxScrollBarOwner); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure Repaint; virtual;
    //
    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property IsNonClient: Boolean read FIsNonClient write FIsNonClient;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { TcxControlScrollBarsManager }

  TcxControlScrollBarsManager = class(TcxIUnknownObject,
    IcxMouseTrackingCaller, IcxMouseTrackingCaller2, IcxMouseTrackingCaller3)
  private
    FCheckCaptureTimer: TcxTimer;
    FIsCapture: Boolean;
    FHotScrollBar: TcxControlScrollBarHelper;
    FOwner: TcxControl;
    FScrollBars: TList;
    procedure CheckCaptureTimerHandler(ASender: TObject);
    function GetScrollBars(AIndex: Integer): TcxControlScrollBarHelper;
    function IsMouseButtonDown: Boolean;
    procedure SetHotScrollBar(AValue: TcxControlScrollBarHelper);
    procedure SetIsCapture(AValue: Boolean);
  protected
    function ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function ProcessMouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    // IcxMouseTrackingCaller
    procedure MouseLeave;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    // IcxMouseTrackingCaller3
    function IsCaptureMouse: Boolean;

    property HotScrollBar: TcxControlScrollBarHelper read FHotScrollBar write SetHotScrollBar;
    property ScrollBars[Index: Integer]: TcxControlScrollBarHelper read GetScrollBars;
  public
    constructor Create(AOwner: TcxControl); virtual;
    destructor Destroy; override;
    function HandleMessage(var Message: TMessage): Boolean; virtual;
    function IsScrollBarsArea(const APoint: TPoint): Boolean; virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure RegisterScrollBar(AScrollBar: TcxControlScrollBarHelper);
    procedure UnRegisterScrollBar(AScrollBar: TcxControlScrollBarHelper);
    property IsCapture: Boolean read FIsCapture write SetIsCapture;
  end;

  TdxScrollBarWrapper = class(TcxIUnknownObject, IcxControlScrollBar)
  private
    FInnerScrollBar: TObject;
    FIsTouchScrollUIMode: Boolean;
    FOwnerControl: TcxControl;
    FTouchScrollUIOwner: TObject;
    FOnScroll: TScrollEvent;
    function GetInnerScrollBar: IcxControlScrollBar;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetTouchScrollUIOwner: IdxTouchScrollUIOwner;
    procedure SetOwnerControlRelativeBounds(const ABounds: TRect);
  protected
    procedure DoScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure UpdateBounds(const ABoundsRect, AVisibleBoundsRect: TRect);
    property InnerScrollBar: IcxControlScrollBar read GetInnerScrollBar;
    property TouchScrollUIOwner: IdxTouchScrollUIOwner read GetTouchScrollUIOwner;
  public
    constructor Create(AOwner: IdxTouchScrollUIOwner);
    destructor Destroy; override;
    // IcxControlScrollBar
    procedure ApplyData;
    function GetControl: TcxControlScrollBar;
    function GetBoundsRect: TRect;
    function GetData: TcxScrollBarData;
    function GetEnabled: Boolean;
    function GetHeight: Integer;
    function GetKind: TScrollBarKind;
    function GetLargeChange: TScrollBarInc;
    function GetLeft: Integer;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetSmallChange: TScrollBarInc;
    function GetTop: Integer;
    function GetUnlimitedTracking: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetLargeChange(AValue: TScrollBarInc);
    procedure SetLeft(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
    procedure SetScrollParams(AMin, AMax, APosition, APageSize: Integer; ARedraw: Boolean = True);
    procedure SetSmallChange(AValue: TScrollBarInc);
    procedure SetTop(AValue: Integer);
    procedure SetUnlimitedTracking(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);

    procedure Calculate;
    procedure CreateInnerScrollBar;
    procedure DestroyInnerScrollBar;
    function GetDefaultSize: TSize;
    procedure InitControl;
    procedure Invalidate;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Control: TcxControlScrollBar read GetControl;
    property Data: TcxScrollBarData read GetData;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Height: Integer read GetHeight write SetHeight;
    property Kind: TScrollBarKind read GetKind write SetKind;
    property LargeChange: TScrollBarInc read GetLargeChange write SetLargeChange;
    property Left: Integer read GetLeft write SetLeft;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property Position: Integer read GetPosition write SetPosition;
    property SmallChange: TScrollBarInc read GetSmallChange write SetSmallChange;
    property Top: Integer read GetTop write SetTop;
    property UnlimitedTracking: Boolean read GetUnlimitedTracking write SetUnlimitedTracking;
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: Integer read GetWidth write SetWidth;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

  TcxControlCustomScrollBars = class(TcxIUnknownObject)
  private
    FCalculating: Boolean;
    FOwner: TcxControl;
    FHScrollBar: TObject;
    FSizeGrip: TObject;
    FVScrollBar: TObject;
  protected
    procedure ApplyData;
    procedure BringInternalControlsToFront; virtual;
    function CheckSize(AKind: TScrollBarKind): Boolean; virtual;
    procedure CheckSizeGripVisible(AValue: Boolean); virtual; abstract;
    function CreateScrollBar(AKind: TScrollBarKind): TObject; virtual; abstract;
    procedure CreateScrollBars; virtual;
    function CreateSizeGrip: TObject; virtual; abstract;
    procedure DestroyHScrollBar; virtual;
    procedure DestroyScrollBars; virtual;
    procedure DestroySizeGrip; virtual;
    procedure DestroyVScrollBar; virtual;
    procedure DoScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer); virtual;
    function GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar; virtual;
    procedure Hide; virtual;
    procedure InitScrollBars; virtual;
    procedure UnInitScrollBars; virtual;
    function IsInternalControl(AControl: TControl): Boolean; virtual;
    function IsScrollBarActive(AKind: TScrollBarKind): Boolean; virtual;
    function IsScrollBarVisible(AKind: TScrollBarKind): Boolean; virtual;
    function IsSizeGripArea(const APoint: TPoint): Boolean; virtual;
    procedure SetInternalControlsBounds; virtual;
    procedure UpdateInternalControlsState; virtual;

    property Calculating: Boolean read FCalculating write FCalculating;
    property Owner: TcxControl read FOwner;
  public
    constructor Create(AOwner: TcxControl); virtual;
    destructor Destroy; override;
    procedure Invalidate; virtual;
    procedure DrawSizeGrip(ACanvas: TcxCanvas); virtual;
  end;
  TcxControlCustomScrollBarsClass = class of TcxControlCustomScrollBars;

  TcxControlScrollBars = class(TcxControlCustomScrollBars, IcxScrollBarOwner)
  private
    function GetHScrollBar: TcxControlScrollBarHelper;
    function GetSizeGrip: TcxSizeGripHelper;
    function GetVScrollBar: TcxControlScrollBarHelper;
  protected
    // IcxScrollBarOwner
    function GetControl: TWinControl; virtual;
    function GetLookAndFeel: TcxLookAndFeel; virtual;
    function GetScaleFactor: TdxScaleFactor;

    function CheckSize(AKind: TScrollBarKind): Boolean; override;
    procedure CheckSizeGripVisible(AValue: Boolean); override;
    function CreateScrollBar(AKind: TScrollBarKind): TObject; override;
    function CreateSizeGrip: TObject; override;
    procedure DestroyHScrollBar; override;
    procedure DestroyVScrollBar; override;
    function GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarHelperClass; virtual;
    function GetSizeGripClass: TcxSizeGripHelperClass; virtual;
    function IsSizeGripArea(const APoint: TPoint): Boolean; override;
    procedure SetInternalControlsBounds; override;

    property HScrollBar: TcxControlScrollBarHelper read GetHScrollBar;
    property VScrollBar: TcxControlScrollBarHelper read GetVScrollBar;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property SizeGrip: TcxSizeGripHelper read GetSizeGrip;
  public
    procedure Invalidate; override;
    procedure DrawSizeGrip(ACanvas: TcxCanvas); override;
  end;

  TcxControlWindowedScrollBars = class(TcxControlCustomScrollBars)
  private
    function GetHScrollBar: TcxControlScrollBar;
    function GetScrollBarBounds(AKind: TScrollBarKind): TRect;
    function GetSizeGrip: TcxSizeGrip;
    function GetVScrollBar: TcxControlScrollBar;
    procedure ScrollBarMouseEnter(Sender: TObject);
    procedure ScrollBarMouseLeave(Sender: TObject);
  protected
    procedure BringInternalControlsToFront; override;
    function CheckSize(AKind: TScrollBarKind): Boolean; override;
    procedure CheckSizeGripVisible(AValue: Boolean); override;
    function CreateScrollBar(AKind: TScrollBarKind): TObject; override;
    function CreateSizeGrip: TObject; override;
    procedure DestroyScrollBars; override;
    procedure Hide; override;
    procedure InitScrollBars; override;
    procedure UnInitScrollBars; override;
    function IsInternalControl(AControl: TControl): Boolean; override;
    function IsScrollBarActive(AKind: TScrollBarKind): Boolean; override;
    procedure SetInternalControlsBounds; override;
    procedure UpdateInternalControlsState; override;

    property HScrollBar: TcxControlScrollBar read GetHScrollBar;
    property VScrollBar: TcxControlScrollBar read GetVScrollBar;
    property SizeGrip: TcxSizeGrip read GetSizeGrip;
  public
    procedure Invalidate; override;
  end;

  { drag & drop objects }

  TcxDragAndDropObjectClass = class of TcxDragAndDropObject;

  TcxDragAndDropObject = class
  private
    FCanvas: TcxCanvas;
    FControl: TcxControl;
    FDirty: Boolean;
    procedure SetDirty(Value: Boolean);
  protected
    procedure ChangeMousePos(const P: TPoint);
    procedure DirtyChanged; virtual;
    function GetClientCursorPos: TPoint; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; virtual;
    function GetImmediateStart: Boolean; virtual;
    function GetZoomFactor: Integer; virtual;

    procedure AfterDragAndDrop(Accepted: Boolean); virtual;
    procedure BeforeBeginDragAndDrop; virtual;
    procedure BeginDragAndDrop; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean; virtual;
    function ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean; virtual;
    function TranslateCoords(const P: TPoint): TPoint; virtual;

    property Canvas: TcxCanvas read FCanvas write FCanvas;
    property Control: TcxControl read FControl;
    property Dirty: Boolean read FDirty write SetDirty;
  public
    CurMousePos: TPoint;
    PrevMousePos: TPoint;
    constructor Create(AControl: TcxControl); virtual;

    procedure DoBeginDragAndDrop;
    procedure DoDragAndDrop(const P: TPoint; var Accepted: Boolean);
    procedure DoEndDragAndDrop(Accepted: Boolean);
    function DoProcessKeyDown(var Message: TWMKeyDown): Boolean; virtual;
    function DoProcessKeyUp(var Message: TWMKeyUp): Boolean; virtual;

    property ImmediateStart: Boolean read GetImmediateStart;
    property ZoomFactor: Integer read GetZoomFactor;
  end;

  TcxDragControlObject = class(TDragControlObject)
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  end;

  TcxDragImageListClass = class of TcxDragImageList;

  TcxDragImageList = class(TDragImageList);

  { control }

  TcxControlBorderStyle = (cxcbsNone, cxcbsDefault);
  TcxDragAndDropState = (ddsNone, ddsStarting, ddsInProcess);

  TcxKey = (kAll, kArrows, kChars, kTab);
  TcxKeys = set of TcxKey;

  TcxMouseWheelScrollingKind = (mwskNone, mwskHorizontal, mwskVertical);
  TcxControlActivateType = (atOther, atByMouse, atByMouseNonClientArea);

  TcxControlListernerLink = class
    Ref: TcxControl;
  end;

  { TcxCustomControl }

  TcxCustomControl = class(TCustomControl,
    IdxScaleFactor,
    IdxAdornerTargetElement,
    IdxAdornerRootTargetElement,
    IdxAdornerTargetElementCollection)
  strict private
    FScaleChangeCounter: Word;
    FScaleFactor: TdxScaleFactor;

    procedure BeginScaleChange;
    procedure EndScaleChange;
    function GetIsScaleChanging: Boolean;
    procedure DXMScaleChanged(var Message: TMessage); message DXM_SCALECHANGED;
    procedure DXMScaleChanging(var Message: TMessage); message DXM_SCALECHANGING;
  protected
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
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override; final;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); overload; override; final;
  {$ENDIF}
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); virtual;
  {$IFDEF DELPHIBERLIN}
    procedure ScaleControlsForDpi(NewPPI: Integer); override;
  {$ENDIF}
    procedure ScaleFactorChanging; virtual;
    procedure ScaleFactorChanged; virtual;
    procedure SetParent(AValue: TWinControl); override;
    //
    property IsScaleChanging: Boolean read GetIsScaleChanging;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TcxControl }

  TcxControlClass = class of TcxControl;
  TcxControl = class(TcxCustomControl,
    IcxLookAndFeelContainer,
    IcxScrollBarOwner,
    IdxGestureClient,
    IdxGestureOwner,
    IdxLocalizerListener,
    IdxSystemInfoListener,
    IdxTouchScrollUIOwner,
    IdxHybridScrollbarOwner)
  strict private
    FHybridScrollbarBaseColor: TColor;
    FHybridScrollbarsManager: TdxHybridScrollbarsManager;
    FIsScrollUIModeChanging: Boolean;
    FScrollbarMode: TdxScrollbarMode;
    FScrollUIActivityHelper: TdxTouchScrollUIActivityHelper;

    FActivateType: TcxControlActivateType;
    FActiveCanvas: TcxCanvas;
    FBorderStyle: TcxControlBorderStyle;
    FCanvas: TcxCanvas;
    FCreatingWindow: Boolean;
    FFinishingDragAndDrop: Boolean;
    FFocusOnClick: Boolean;
    FFontListenerList: IInterfaceList;
    FLookAndFeel: TcxLookAndFeel;
    FOriginalHint: string;
    FPopupMenu: TComponent;
    FTransparent: Boolean;

    // D&D
    FDragAndDropObject: TcxDragAndDropObject;
    FDragAndDropObjectClass: TcxDragAndDropObjectClass;
    FDragAndDropPrevCursor: TCursor;
    FDragAndDropState: TcxDragAndDropState;
    FDragImages: TcxDragImageList;
    // touch
    FGestureHelper: TdxGestureHelper;
    FGestureAccumulatedDelta: TPoint;
    // scroll
    FAllowScrollContentOnDrag: Boolean;
    FInitialHScrollBarActive: Boolean;
    FInitialVScrollBarActive: Boolean;
    FIsScrollingContent: Boolean;
    FMainScrollBars: TcxControlCustomScrollBars;
    FScrollBars: TcxScrollStyle;
    FScrollBarsManager: TcxControlScrollBarsManager;
    FScrollBarsLockCount: Integer;
    FScrollBarsUpdateNeeded: Boolean;
    FUpdateScrollBarsCount: Integer;
    // key+mouse
    FKeys: TcxKeys;
    FMouseButtonPressed: Boolean;
    FMouseCaptureObject: TObject;
    FMouseDownPos: TPoint;
    FMouseEnterCounter: Integer;
    FMouseRightButtonReleased: Boolean;
    FMouseWheelAccumulator: Integer;
    // events
    FOnFocusChanged: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;

    function GetActiveCanvas: TcxCanvas;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetHint: string;
    function GetHScrollBar: IcxControlScrollBar;
    function GetHScrollBarVisible: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetSizeGrip: TcxSizeGrip;
    function GetUpdatingScrollBars: Boolean;
    function GetVScrollBar: IcxControlScrollBar;
    function GetVScrollBarVisible: Boolean;
    function IsHintStored: Boolean;
    procedure SetBorderStyle(Value: TcxControlBorderStyle);
    procedure SetDragAndDropState(Value: TcxDragAndDropState);
    procedure SetHint(const Value: string);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetKeys(Value: TcxKeys);
    procedure SetMouseCaptureObject(Value: TObject);
    procedure SetPopupMenu(Value: TComponent);
    procedure SetScrollBars(Value: TcxScrollStyle);
    procedure SetTransparent(Value: Boolean);

    procedure CreateScrollBars;
    procedure DestroyScrollBars;
    procedure DoNCPaint(AWindowDC: HDC);
    procedure DoScrollBarBasedGestureScroll(AKind: TScrollBarKind;
      var AAccumulatedDelta: Integer; ADeltaX, ADeltaY: Integer);
    function GetLookAndFeelScrollbarMode: TdxScrollbarMode;
    function IsScrollBarModeChanged: Boolean;

    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPrint(var Message: TWMPrint); message WM_PRINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMNCSizeChanged(var Message: TMessage); message DXM_NCSIZECHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyDown); message CN_KEYUP;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
  protected
    FBounds: TRect;

    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure ScaleFactorChanged; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DestroyWindowHandle; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    // touch
    procedure BeginGestureScroll(APos: TPoint); virtual;
    function CanScrollContentByGestureWithoutScrollBars: Boolean; virtual;
    procedure CheckOverpan(AScrollKind: TScrollBarKind;
      ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer); virtual;
    procedure DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer); virtual;
    procedure EndGestureScroll; virtual;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions); override;
    function GetDefaultInteractiveGestures: TInteractiveGestures; virtual;
    function GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions; virtual;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; virtual;
    function IsTouchPropertyStored(AProperty: TTouchProperty): Boolean; override;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer); virtual;
    function CanProcessScrollEvents(var Message: TMessage): Boolean; virtual;
    function IsDefaultGesture(AGestureID: Integer): Boolean; virtual;
    function IsGestureHelperMessage(var Message: TMessage): Boolean; virtual;
    function IsGestureScrolling: Boolean; virtual;
    function IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean; virtual;
    function GetDefaultPanOptions: Integer; virtual;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;
    //
    procedure DoMouseEnter(AControl: TControl);
    procedure DoMouseLeave(AControl: TControl);
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean; virtual;
    function GetPopupMenu: TPopupMenu; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function IsDoubleBufferedNeeded: Boolean; virtual;
    function IsMenuKey(var Message: TWMKey): Boolean;
    procedure Modified; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParentBackground(Value: Boolean); override;

    // drawing
    procedure DoPaint; virtual;
    procedure DrawBorder(ACanvas: TcxCanvas); virtual;
    procedure DrawScrollBars(ACanvas: TcxCanvas); virtual;
    procedure EraseBackground(DC: HDC); overload; virtual;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); overload; virtual;
    procedure Paint; override;
    procedure StandardPaintHandler(var Message: TWMPaint);
    procedure PaintNonClientArea(ACanvas: TcxCanvas); virtual;
    procedure PaintWindow(DC: HDC); override;

    procedure AddChildComponent(AComponent: TcxControlChildComponent); dynamic;
    procedure RemoveChildComponent(AComponent: TcxControlChildComponent); dynamic;

    procedure AfterMouseDown(AButton: TMouseButton; X, Y: Integer); virtual;
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure BringInternalControlsToFront; virtual;
    procedure CancelMouseOperations; virtual;
    procedure DoCancelMode; dynamic;
    function GetBorderSize: Integer; virtual;
    function GetBounds: TRect; virtual;
    function GetClientBounds: TRect; virtual;
    function GetClientOffsets: TRect; virtual;
    function GetCurrentCursor(X, Y: Integer): TCursor; virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; dynamic;
    function GetDragObjectClass: TDragControlObjectClass; dynamic;
    function GetIsDesigning: Boolean; virtual;
    function GetIsFocused: Boolean; virtual;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; virtual;
    function GetMouseCursorClientPos: TPoint;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; virtual;
    function GetPaintBlackOpaqueOnGlass: Boolean; virtual;
    function GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass; virtual;
    function GetSizeGripClass: TcxSizeGripClass; virtual;
    procedure InitControl; virtual;
    class procedure InvalidateControl(AControl: TWinControl; ANeedInvalidateSelf, ANeedInvalidateChildren: Boolean);
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure FocusEnter; dynamic;
    procedure FocusLeave; dynamic;
    procedure SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer); virtual;
    procedure SetPaintRegion; virtual;
    procedure UpdateStatusHint(const APoint: TPoint);
    function GetStatusHint(const APoint: TPoint): string; virtual;

    // Conditions
    function AllowCompositionPainting: Boolean; virtual;
    function CanFocusOnClick: Boolean; overload; virtual;
    function CanFocusOnClick(X, Y: Integer): Boolean; overload; virtual;
    function FocusWhenChildIsClicked(AChild: TControl): Boolean; virtual;
    function HasBackground: Boolean; virtual;
    function HasNonClientArea: Boolean; virtual;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function IsTransparentBackground: Boolean; virtual;
    function IsInternalControl(AControl: TControl): Boolean; virtual;
    function MayFocus: Boolean; dynamic;
    function NeedRedrawOnResize: Boolean; virtual;
    function RecreateWndOnBiDiModeChanged: Boolean; virtual;
    function UpdateMousePositionIfControlMoved: Boolean; virtual;

    // Notifications
    procedure BiDiModeChanged; virtual;
    procedure BorderStyleChanged; dynamic;
    procedure BoundsChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure CursorChanged; dynamic;
    procedure EnabledChanged; virtual;
    procedure FocusChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ParentBackgroundChanged; virtual;
    procedure DoScrollUIModeChanged; virtual;
    procedure TextChanged; dynamic;
    procedure TransparentChanged; dynamic;
    procedure VisibleChanged; dynamic;

    // IdxGestureClient
    function AllowGesture(AGestureId: Integer): Boolean; virtual;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; virtual;
    function GetPanOptions: Integer; virtual;
    function IsPanArea(const APoint: TPoint): Boolean; virtual;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; virtual;
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; virtual;
    function IdxGestureOwner.GetHandle = GetGestureClientHandle;
    function GetGestureClientHandle: THandle; virtual;
    function IsGestureTarget(AWnd: THandle): Boolean; virtual;
    // IcxLookAndFeelContainer
    function IcxLookAndFeelContainer.GetLookAndFeel = GetLookAndFeelValue;
    function GetLookAndFeelValue: TcxLookAndFeel; virtual;
    // IcxScrollBarOwner
    function GetControl: TWinControl;
    function IcxScrollBarOwner.GetLookAndFeel = GetScrollBarLookAndFeel;
    function GetScrollBarLookAndFeel: TcxLookAndFeel; virtual;
    // IdxTouchScrollUIOwner
    procedure IdxTouchScrollUIOwner.CheckUIPosition=CheckTouchScrollUIPosition;
    procedure CheckTouchScrollUIPosition; virtual;
    function IdxTouchScrollUIOwner.GetOwnerControl=GetTouchScrolUIOwnerControl;
    function GetTouchScrolUIOwnerControl: TcxControl; virtual;
    function IdxTouchScrollUIOwner.HasVisibleUI=HasVisibleTouchScrollUI;
    function HasVisibleTouchScrollUI: Boolean; virtual;
    procedure IdxTouchScrollUIOwner.HideUI=HideTouchScrollUIDirectly;
    procedure HideTouchScrollUIDirectly; virtual;
    // IdxHybridScrollbarOwner
    procedure IdxHybridScrollbarOwner.CheckUIPosition=CheckTouchScrollUIPosition;
    function IdxHybridScrollbarOwner.GetOwnerControl=GetTouchScrolUIOwnerControl;
    function IdxHybridScrollbarOwner.HasVisibleUI=HasVisibleTouchScrollUI;
    procedure IdxHybridScrollbarOwner.HideUI=HideTouchScrollUIDirectly;
    function IdxHybridScrollbarOwner.GetBaseColor=GetHybridScrollbarBaseColor;
    function GetHybridScrollbarBaseColor: TColor; virtual;
    function GetManager: TdxHybridScrollbarsManager;
    procedure IdxHybridScrollbarOwner.Invalidate=InvalidateScrollbars;
    procedure InvalidateScrollbars; virtual;
    // look&feel
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual;
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;

    // scrollbars
    function AllowHybridScrollbarMode: Boolean; virtual;
    function AllowTouchScrollUIMode: Boolean; virtual;
    function CanScrollLineWithoutScrollBars(ADirection: TcxDirection): Boolean; virtual;
    function CanUpdateScrollBars: Boolean; virtual;
    procedure CheckNeedsScrollBars;
    procedure CheckTouchScrollUIMode; virtual;
    procedure Deactivate(AValue: IdxTouchScrollUIOwner = nil);
    procedure DoCreateScrollBars; virtual;
    procedure DoDestroyScrollBars; virtual;
    procedure DoScrolling;
    procedure DoUpdateScrollBars; virtual;
    function GetHScrollBarAreaHeight: Integer; virtual;
    function GetHScrollBarBounds: TRect; virtual;
    function GetHScrollBarDefaultAreaHeight: Integer; virtual;
    function GetSizeGripBounds: TRect; virtual;
    function GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar;
    function GetScrollBarSize: TSize;
    function GetScrollbarMode: TdxScrollbarMode;
    function GetScrollContentForegroundColor: TColor; virtual;
    function GetSystemSizeScrollBars: TcxScrollStyle; virtual;
    function GetVScrollBarAreaWidth: Integer; virtual;
    function GetVScrollBarBounds: TRect; virtual;
    function GetVScrollBarDefaultAreaWidth: Integer; virtual;
    function HasScrollBarArea: Boolean; virtual;
    function HasScrollBars: Boolean; virtual;
    procedure HideScrollBars; virtual;
    procedure HideTouchScrollUI(AValue: IdxTouchScrollUIOwner; AImmediately: Boolean = False);
    procedure InitScrollBars; virtual;
    procedure InitScrollBarsParameters; virtual;
    procedure InitScrollBarsParametersCache; virtual;
    function IsPixelScrollBar(AKind: TScrollBarKind): Boolean; virtual;
    function IsPopupScrollBars: Boolean; virtual;
    function IsScrollBarActive(AKind: TScrollBarKind): Boolean;
    function IsScrollBarsArea(const APoint: TPoint): Boolean; virtual;
    function IsScrollBarsCapture: Boolean;
    function IsTouchScrollUIHidden(ATouchScrollBarOwner: IdxTouchScrollUIOwner = nil): Boolean;
    function IsTouchScrollUIMode: Boolean; virtual;
    function IsSizeGripArea(const APoint: TPoint): Boolean; virtual;
    function IsSizeGripVisible: Boolean; virtual;
    function NeedsScrollBars: Boolean; virtual;
    function NeedsToBringInternalControlsToFront: Boolean; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollBarVisibilityChanged(AScrollBars: TScrollBarKinds); virtual;
    procedure SetInternalControlsBounds; virtual;
    procedure ShowTouchScrollUI(AValue: IdxTouchScrollUIOwner; AControlledByTimer: Boolean = False); virtual;
    procedure UpdateInternalControlsState; virtual;
    procedure UpdateScrollBarBounds;
    procedure UpdateScrollBars; virtual;

    property AllowScrollContentOnDrag: Boolean read FAllowScrollContentOnDrag write FAllowScrollContentOnDrag;
    property HScrollBar: IcxControlScrollBar read GetHScrollBar;
    property HScrollBarVisible: Boolean read GetHScrollBarVisible;
    property MainScrollBars: TcxControlCustomScrollBars read FMainScrollBars;
    property ScrollBarsManager: TcxControlScrollBarsManager read FScrollBarsManager;
    property ScrollBars: TcxScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property SizeGrip: TcxSizeGrip read GetSizeGrip;
    property UpdatingScrollBars: Boolean read GetUpdatingScrollBars;
    property VScrollBar: IcxControlScrollBar read GetVScrollBar;
    property VScrollBarVisible: Boolean read GetVScrollBarVisible;

    // internal drag and drop (columns moving, ...)
    function AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean; virtual;
    function AllowDragAndDropWithoutFocus: Boolean; dynamic;
    function CanCancelDragStartOnCaptureObjectClear: Boolean; virtual;
    function CreateDragAndDropObject: TcxDragAndDropObject; virtual;
    procedure DoFinishDragAndDrop(Accepted: Boolean); virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); dynamic;
    procedure EndDragAndDrop(Accepted: Boolean); dynamic;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; dynamic;

    // delphi drag and drop
    function CanDrag(X, Y: Integer): Boolean; dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DrawDragImage(ACanvas: TcxCanvas; const R: TRect); virtual;
    function GetDragImagesClass: TcxDragImageListClass; virtual;
    function GetDragImagesSize: TPoint; virtual;
    function GetIsCopyDragDrop: Boolean; virtual;
    function HasDragImages: Boolean; virtual;
    procedure HideDragImage;
    procedure InitDragImages(ADragImages: TcxDragImageList); virtual;
    procedure ScrollUIModeChanged; virtual;
    procedure ShowDragImage;
    property DragImages: TcxDragImageList read FDragImages;
    property GestureHelper: TdxGestureHelper read FGestureHelper;
    property IsCopyDragDrop: Boolean read GetIsCopyDragDrop;

    property ActivateType: TcxControlActivateType read FActivateType write FActivateType;
    property BorderSize: Integer read GetBorderSize;
    property BorderStyle: TcxControlBorderStyle read FBorderStyle write SetBorderStyle;
    property CreatingWindow: Boolean read FCreatingWindow;
    property FocusOnClick: Boolean read FFocusOnClick write FFocusOnClick default True;
    property FontListenerList: IInterfaceList read FFontListenerList;
    property HybridScrollbarBaseColor: TColor read FHybridScrollbarBaseColor write FHybridScrollbarBaseColor;
    property IsScrollingContent: Boolean read FIsScrollingContent;
    property IsScrollUIModeChanging: Boolean read FIsScrollUIModeChanging;
    property Keys: TcxKeys read FKeys write SetKeys;
    property MouseRightButtonReleased: Boolean read FMouseRightButtonReleased write FMouseRightButtonReleased;
    property MouseWheelScrollingKind: TcxMouseWheelScrollingKind read GetMouseWheelScrollingKind;
    property ParentBackground default True;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
    property ScrollUIActivityHelper: TdxTouchScrollUIActivityHelper read FScrollUIActivityHelper;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnFocusChanged: TNotifyEvent read FOnFocusChanged write FOnFocusChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDragImages: TDragImageList; override;
    function CanFocusEx: Boolean; virtual;
    function AcceptMousePosForClick(X, Y: Integer): Boolean; virtual;
    procedure DefaultHandler(var Message); override;
    procedure InvalidateRect(const R: TRect; EraseBackground: Boolean);
    procedure InvalidateRgn(ARegion: TcxRegion; EraseBackground: Boolean);
    procedure InvalidateWithChildren;
    function IsMouseInPressedArea(X, Y: Integer): Boolean; virtual;
    procedure PostMouseMove(AMousePos: TPoint); overload;
    procedure PostMouseMove; overload;
    procedure ScrollContent(ADirection: TcxDirection); virtual;
    procedure ScrollWindow(DX, DY: Integer; const AScrollRect: TRect);
    procedure SetFocus; override;
    procedure SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
      AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
    function StartDrag(DragObject: TDragObject): Boolean; dynamic;
    procedure UpdateWithChildren;

    // internal drag and drop (columns moving, ...)
    procedure BeginDragAndDrop; dynamic;
    procedure FinishDragAndDrop(Accepted: Boolean);
    property DragAndDropObject: TcxDragAndDropObject read GetDragAndDropObject;
    property DragAndDropObjectClass: TcxDragAndDropObjectClass read GetDragAndDropObjectClass write FDragAndDropObjectClass;
    property DragAndDropState: TcxDragAndDropState read FDragAndDropState write SetDragAndDropState;

    procedure AddFontListener(AListener: IcxFontListener);
    procedure RemoveFontListener(AListener: IcxFontListener);

    procedure LockScrollBars;
    procedure UnlockScrollBars;

    // IdxLocalizerListener
    procedure TranslationChanged; virtual;

    // IdxSystemInfoListener
    procedure IdxSystemInfoListener.Changed = SystemInfoChanged;
    procedure SystemInfoChanged(AParameter: Cardinal); virtual;

    property ActiveCanvas: TcxCanvas read GetActiveCanvas;
    property Bounds: TRect read GetBounds;
    property Canvas: TcxCanvas read FCanvas;
    property ClientBounds: TRect read GetClientBounds;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsFocused: Boolean read GetIsFocused;
    property MouseCaptureObject: TObject read FMouseCaptureObject write SetMouseCaptureObject;
    property MouseDownPos: TPoint read FMouseDownPos write FMouseDownPos;

    property TabStop default True; // MayFocus = True
  published
    property Touch;
    property OnGesture;
    property Hint: string read GetHint write SetHint stored IsHintStored;
  end;

  { TcxControlHelper }

  TcxControlHelper = class
  private
    class function GetControlByObject(AObject: TObject): TControl;
  public
    class function CanSetParent(AControl, ANewParent: TControl): Boolean;
    class procedure ChangeScaleFactor(AControl: TControl; ATargetScaleFactor: TdxScaleFactor); overload;
    class procedure ChangeScaleFactor(AControl: TControl; ATargetNumerator, ATargetDenominator: Integer); overload;
    class function GetCurrentDPIFromControl(AObject: TObject): Integer;
    class function GetOriginalParentSize(AObject: TObject): TPoint;
    class procedure ScaleForPPI(AObject: TObject; ATargetPPI: Integer);
    class procedure SetOriginalParentSize(AObject: TObject; const APoint: TPoint);
    class procedure UpdateScaleFactorOnParentChange(AControl: TControl);
  end;

  { TcxControlDefaultHandlerHelper }

  TcxControlDefaultHandlerHelper = class
  public
    class function Process(var Message): Boolean;
  end;

  { TcxScrollingControl }

  TdxAutoSizeMode = (asNone, asAutoWidth, asAutoHeight, asAutoSize);
  TdxChangeType = (ctLight, ctMedium, ctHard);
  TdxVisibilityType = (vtPartialy, vtFully, vtCentered);

  IdxScrollingControl = interface
  ['{7F141990-5975-4E6B-BFAF-03D378860F20}']
    function GetLeftPos: Integer;
    procedure SetLeftPos(Value: Integer);
    function GetTopPos: Integer;
    procedure SetTopPos(Value: Integer);
    function GetContentSize: TSize;

    function GetInstance: TcxControl;
  end;

  TcxScrollingControl = class(TcxControl, IdxScrollingControl)
  strict private
    function GetTopPos: Integer;
    function GetLeftPos: Integer;
    function GetInstance: TcxControl;
  protected
    FAutoSizeMode: TdxAutoSizeMode;
    FAutoSizeModeSetting: Boolean;
    FLeftPos: Integer;
    FTopPos: Integer;

    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;

    procedure BoundsChanged; override;
    function CanCalculate: Boolean; virtual;
    procedure Calculate(AType: TdxChangeType); virtual;
    procedure CreateHandle; override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); virtual;
    procedure Loaded; override;
    procedure ScrollPosChanged(const AOffset: TPoint); virtual;

    function IsScrollDataValid: Boolean; virtual;
    function GetScrollStep: Integer; virtual;

    procedure CheckPositions;
    procedure CheckLeftTopPos(var P: TPoint);
    procedure CheckLeftPos(var AValue: Integer);
    procedure CheckTopPos(var AValue: Integer);
    function GetContentSize: TSize; virtual;
    function GetClientSize: TSize; virtual;
    procedure InitScrollBarsParameters; override;
    procedure MakeVisible(const ARect: TRect; AType: TdxVisibilityType);
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetAutoSizeMode(AValue: TdxAutoSizeMode); virtual;
    procedure SetLeftTop(P: TPoint);
    procedure SetLeftPos(AValue: Integer); virtual;
    procedure SetTopPos(AValue: Integer); virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;

    property AutoSizeMode: TdxAutoSizeMode read FAutoSizeMode write SetAutoSizeMode;
    property LeftPos: Integer read GetLeftPos write SetLeftPos;
    property TopPos: Integer read GetTopPos write SetTopPos;
    property ScrollStep: Integer read GetScrollStep;
  end;

  TdxScrollHelper = class
  protected const
    ScrollStep = 10;
  strict private
    class procedure CheckLeftPos(AControl: IdxScrollingControl; var AValue: Integer);
    class procedure CheckTopPos(AControl: IdxScrollingControl; var AValue: Integer);

//    class procedure ScrollContent(AControl: IdxScrollingControl; APrevPos, ACurPos: Integer; AHorzScrolling: Boolean);
    class procedure ScrollContent(AControl: IdxScrollingControl);
  public
    class procedure CheckPositions(AControl: IdxScrollingControl);
    class procedure SetPos(AControl: IdxScrollingControl; X, Y: Integer);
    class procedure SetLeftPos(AControl: IdxScrollingControl; AValue: Integer);
    class procedure SetTopPos(AControl: IdxScrollingControl; AValue: Integer);
    class function IsScrollDataValid(AControl: IdxScrollingControl): Boolean;
    class procedure GestureScroll(AControl: IdxScrollingControl; ADeltaX, ADeltaY: Integer);
    class function GetClientSize(AControl: IdxScrollingControl): TSize;
    class procedure InitScrollBarsParameters(AControl: IdxScrollingControl);
    class procedure Scroll(AControl: IdxScrollingControl; AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
  end;

  { customize listbox }

  TcxCustomizeListBox = class(TListBox)
  private
    FDragAndDropItemIndex: Integer;
    FMouseDownPos: TPoint;

    function GetDragAndDropItemObject: TObject;
    function GetItemObject: TObject;
    procedure SetItemObject(Value: TObject);
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  protected
  {$IFNDEF DELPHI101BERLIN}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure BeginDragAndDrop; dynamic;

    property DragAndDropItemIndex: Integer read FDragAndDropItemIndex;
    property DragAndDropItemObject: TObject read GetDragAndDropItemObject;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemObject: TObject read GetItemObject write SetItemObject;
  end;

  { TcxMessageWindow }

  TcxMessageWindow = class(TcxIUnknownObject)
  private
    FHandle: HWND;
    procedure MainWndProc(var Message: TMessage);
  protected
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Handle: HWND read FHandle;
  end;

  { TcxSystemController }

  TcxSystemController = class(TcxMessageWindow)
  private
    FPrevWakeMainThread: TNotifyEvent;
    procedure WakeMainThread(Sender: TObject);
    procedure HookSynchronizeWakeup;
    procedure UnhookSynchronizeWakeup;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TdxSystemInfo }

  TdxSystemInfo = class(TcxMessageWindow)
  private
    FIsDropShadow: Boolean;
    FListeners: TInterfaceList;
    FNonClientMetrics: TNonClientMetrics;
    function GetIsRemoteSession: Boolean;
    procedure UpdateCache(AParameter: Cardinal);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure NotifyListeners(AParameter: Cardinal);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddListener(AListener: IdxSystemInfoListener);
    function GetParameter(AParameter: Cardinal; var AValue): Boolean;
    procedure RemoveListener(AListener: IdxSystemInfoListener);

    property IsDropShadow: Boolean read FIsDropShadow;
    property IsRemoteSession: Boolean read GetIsRemoteSession;
    property NonClientMetrics: TNonClientMetrics read FNonClientMetrics;
  end;

  { TdxMessagesController }

  TdxMessagesController = class
  private
    FLockedMessages: TList;
  protected
    FOldWndProc: Pointer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function IsMessageInQueue(AWnd: THandle; AMessage: UINT): Boolean;
    function KillMessages(AWnd: THandle; AMsgFilterMin: UINT; AMsgFilterMax: UINT = 0;
      AKillAllMessages: Boolean = True): Boolean;

    function IsMessageLocked(AMessage: UINT): Boolean;
    procedure LockMessages(AMessages: array of UINT);
    procedure UnlockMessages(AMessages: array of UINT);
    procedure BlockMessage(AHandle: THandle);
    procedure BlockLockedMessage(AHandle: THandle; var AMessage: UINT);
  end;

  { popup }

  TcxPopupAlignHorz = (pahLeft, pahCenter, pahRight);
  TcxPopupAlignVert = (pavTop, pavCenter, pavBottom);
  TcxPopupDirection = (pdHorizontal, pdVertical);

  TcxPopupWindow = class(TdxCustomForm, IdxSkinSupport2)
  private
    FAcceptAnyPosition: Boolean;
    FAdjustable: Boolean;
    FAlignHorz: TcxPopupAlignHorz;
    FAlignVert: TcxPopupAlignVert;
    FBorderSpace: Integer;
    FBorderStyle: TcxPopupBorderStyle;
    FCanvas: TcxCanvas;
    FDirection: TcxPopupDirection;
    FFrameColor: TColor;
    FIsOwnerBoundsRTLDependent: Boolean;
    FOwnerBounds: TRect;
    FOwnerParent: TControl;
    FPrevActiveWindow: HWND;
    function GetNCHeight: Integer;
    function GetNCWidth: Integer;
    procedure SetAdjustable(AValue: Boolean);
    procedure SetBorderSpace(Value: Integer);
    procedure SetBorderStyle(Value: TcxPopupBorderStyle);
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure Deactivate; override;
    procedure Paint; override;
    procedure VisibleChanged; dynamic;

    procedure InternalCalculateHorizontalDirectionPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;
    procedure InternalCalculateVerticalDirectionPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;
    procedure InternalCalculateTargetPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;

    procedure AdjustableChanged; virtual;
    procedure BordersChanged; virtual;
    procedure CalculateCommonPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;
    function CalculatePosition(const ASize: TSize): TPoint; virtual;
    function CalculateSize: TSize; virtual;
    procedure CheckPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;
    procedure CorrectPositionWithDesktopWorkArea(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); virtual;
    function GetBorderWidth(ABorder: TcxBorder): Integer; virtual;
    function GetClientBounds: TRect; virtual;
    function GetDefaultAlignHorz: TcxPopupAlignHorz;
    function GetFrameWidth(ABorder: TcxBorder): Integer; virtual;
    function GetOwnerScreenBounds: TRect; virtual;
    procedure InitPopup; virtual;
    procedure RestoreControlsBounds;
    function UseOwnerParentToGetScreenBounds: Boolean; virtual;

    procedure DrawFrame; virtual;

    // IdxSkinSupport2
    function IsSkinnable: Boolean;

    property AcceptAnyPosition: Boolean read FAcceptAnyPosition write FAcceptAnyPosition;
    property BorderWidths[ABorder: TcxBorder]: Integer read GetBorderWidth;
    property Canvas: TcxCanvas read FCanvas;
    property FrameWidths[ABorder: TcxBorder]: Integer read GetFrameWidth;
    property NCHeight: Integer read GetNCHeight;
    property NCWidth: Integer read GetNCWidth;
    property IsOwnerBoundsRTLDependent: Boolean read FIsOwnerBoundsRTLDependent write FIsOwnerBoundsRTLDependent;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure CloseUp; virtual;
    procedure Popup; virtual;

    property Adjustable: Boolean read FAdjustable write SetAdjustable;
    property AlignHorz: TcxPopupAlignHorz read FAlignHorz write FAlignHorz;
    property AlignVert: TcxPopupAlignVert read FAlignVert write FAlignVert;
    property BorderSpace: Integer read FBorderSpace write SetBorderSpace;
    property BorderStyle: TcxPopupBorderStyle read FBorderStyle write SetBorderStyle;
    property ClientBounds: TRect read GetClientBounds;
    property Direction: TcxPopupDirection read FDirection write FDirection;
    property FrameColor: TColor read FFrameColor write FFrameColor;
    property OwnerBounds: TRect read FOwnerBounds write FOwnerBounds;
    property OwnerParent: TControl read FOwnerParent write FOwnerParent;
    property OwnerScreenBounds: TRect read GetOwnerScreenBounds;
    // #Ch copy from TForm (T618363)
    property AlphaBlend;
    property AlphaBlendValue;
    property AutoScroll;
    property AutoSize;
    property BorderIcons;
    property BorderWidth;
    property TransparentColor;
    property TransparentColorValue;
    property Ctl3D;
    property DefaultMonitor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont;
    property FormStyle;
    property Icon;
    property ObjectMenuItem;
    property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    property Position;
    property PrintScale;
  {$IFDEF DELPHIXE3}
    property TipMode;
  {$ENDIF}
    property WindowMenu;
    property OnActivate;
    property OnCanResize;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
  end;

  { TcxCustomDragImage }

  TcxCustomDragImage = class(TCustomControl,
    IdxSkinSupport,
    IdxSkinSupport2)
  strict private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FCanvas: TcxCanvas;
    FPopupParent: TWinControl;
    FPositionOffset: TPoint;
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;

    function GetAlphaBlended: Boolean;
    function GetVisible: Boolean;
    procedure SetAlphaBlend(Value: Boolean);
    procedure SetAlphaBlendValue(Value: Byte);
    procedure SetPopupParent(const Value: TWinControl);
    procedure SetVisible(Value: Boolean);
    procedure SetTransparentColor(Value: Boolean);
    procedure SetTransparentColorValue(Value: TColor);
    // Messages
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateLayeredAttributes; virtual;
    // IdxSkinSupport2
    function IsSkinnable: Boolean; virtual;
    //
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property Canvas: TcxCanvas read FCanvas;
    property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
    property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Init(const ASourceBounds: TRect; const ASourcePoint: TPoint);
    procedure MoveTo(const APosition: TPoint);
    procedure Offset(X, Y: Integer);
    procedure Show(ACmdShow: Integer = SW_SHOWNA);
    procedure Hide;

    property AlphaBlended: Boolean read GetAlphaBlended;
    property PopupParent: TWinControl read FPopupParent write SetPopupParent;
    property PositionOffset: TPoint read FPositionOffset write FPositionOffset;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  { TcxDragImage }

  TcxDragImageClass = class of TcxDragImage;
  TcxDragImage = class(TcxCustomDragImage)
  strict private
    FImage: TcxAlphaBitmap;

    function GetImageCanvas: TcxCanvas;
    function GetWindowCanvas: TcxCanvas;
  protected
    procedure Paint; override;
    //
    property Image: TcxAlphaBitmap read FImage;
    property WindowCanvas: TcxCanvas read GetWindowCanvas;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //
    property Canvas: TcxCanvas read GetImageCanvas;
  end;

  { TcxLayeredDragImage }

  TcxLayeredDragImage = class(TcxDragImage)
  protected
    procedure Paint; override;
    procedure UpdateLayeredAttributes; override;
  public
    procedure Invalidate; override;
    procedure Update; override;
  end;

  { TcxSizeFrame }

  TcxSizeFrame = class(TcxCustomDragImage)
  private
    FFillSelection: Boolean;
    FFrameWidth: Integer;
    FRegion: TcxRegion;

    procedure InitializeFrameRegion;
    procedure SetWindowRegion;
  protected
    procedure Paint; override;

    property FrameWidth: Integer read FFrameWidth;
  public
    constructor Create(AFrameWidth: Integer = 2); reintroduce; virtual;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure DrawSizeFrame(const ARect: TRect); overload;
    procedure DrawSizeFrame(const ARect: TRect; const ARegion: TcxRegion); overload;

    property FillSelection: Boolean read FFillSelection write FFillSelection;
  end;

  { TcxDragAndDropArrow }

  TcxArrowPlace = (apLeft, apTop, apRight, apBottom);

  TcxDragAndDropArrowClass = class of TcxDragAndDropArrow;
  TcxDragAndDropArrow = class(TcxDragImage)
  strict private const
    DragAndDropArrowBorderColor = clBlack;
    DragAndDropArrowColor = clLime;
  strict private
    FBorderColor: TColor;
    FColor: TColor;
    FTransparent: Boolean;

    class procedure ApplyColors(ABitmap: TBitmap; AArrowBorderColor, AArrowColor: TColor);
    function GetTransparent: Boolean;
  protected
    function GetImageBackColor: TColor; virtual;
  public
    constructor Create(ATransparent: Boolean); reintroduce; virtual;
    procedure Init(AOwner: TControl; const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace); overload;
    procedure Init(const AOwnerOrigin: TPoint; const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace); overload;
    class function CalculateBounds(const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace;
      AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean): TRect; overload;
    class function CalculateBounds(const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace;
      AScaleFactor: TdxScaleFactor): TRect; overload;
    class function GetGlyph(APlace: TcxArrowPlace; AScaleFactor: TdxScaleFactor): TBitmap;
    class procedure Draw(ACanvas: TcxCanvas; const ABounds: TRect;
      APlace: TcxArrowPlace; AScaleFactor: TdxScaleFactor); overload;
    class procedure Draw(ACanvas: TcxCanvas; const ABounds: TRect;
      APlace: TcxArrowPlace; AArrowBorderColor, AArrowColor: TColor; AScaleFactor: TdxScaleFactor); overload;

    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Color: TColor read FColor write FColor;
    property ImageBackColor: TColor read GetImageBackColor;
    property Transparent: Boolean read GetTransparent;
  end;

  { TcxDesignController }

  TcxDesignState = (dsDesignerModifying);
  TcxDesignStates = set of TcxDesignState;

  TcxDesignController = class
  private
    FLockDesignerModifiedCount: Integer;
  protected
    FState: TcxDesignStates;
  public
    procedure DesignerModified(AForm: TCustomForm); overload;
    function IsDesignerModifiedLocked: Boolean;
    procedure LockDesignerModified;
    procedure UnLockDesignerModified;
  end;

  TcxWindowProcLinkedObject = class(TcxDoublyLinkedObject)
  private
    FControl: TControl;
    FHandle: THandle;
    FWindowProc: TWndMethod;
  protected
    property WindowProc: TWndMethod read FWindowProc write FWindowProc;
  public
    constructor Create(AControl: TControl); overload;
    constructor Create(AHandle: THandle); overload;
    procedure DefaultProc(var Message: TMessage);
    property Control: TControl read FControl;
    property Handle: THandle read FHandle;
  end;

  TcxSubclassingHelper = class
  protected
    FDefaultWindowProc: TWndMethod;
    function CreateLinkedObject: TcxDoublyLinkedObject; virtual; abstract;
    procedure RestoreDefaultProc; virtual; abstract;
    procedure StoreAndReplaceDefaultProc(AWndProc: TWndMethod); virtual; abstract;
    property DefaultWindowProc: TWndMethod read FDefaultWindowProc;
  end;

  TcxVCLSubclassingHelper = class(TcxSubclassingHelper)
  private
    FControl: TControl;
  protected
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    procedure RestoreDefaultProc; override;
    procedure StoreAndReplaceDefaultProc(AWndProc: TWndMethod); override;
  public
    constructor Create(AControl: TControl);
  end;

  TcxWin32SubclassingHelper = class(TcxSubclassingHelper)
  private
    FAPIDefaultWndProc: Pointer;
    FAPIWndProc: Pointer;
    FHandle: THandle;
  protected
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    procedure DefaultWndProc(var Message: TMessage);
    procedure RestoreDefaultProc; override;
    procedure StoreAndReplaceDefaultProc(AWndProc: TWndMethod); override;
  public
    constructor Create(AHandle: THandle);
  end;

  TcxWindowProcLinkedObjectList = class(TcxDoublyLinkedObjectList)
  private
    FControl: TControl;
    FHandle: THandle;
    FHelper: TcxSubclassingHelper;
  protected
    function AddProc(AWndProc: TWndMethod): TcxWindowProcLinkedObject;
    function CreateLinkedObject: TcxDoublyLinkedObject; override;
    procedure Initialize; virtual;
    function IsEmpty: Boolean;
    procedure WndProc(var Message: TMessage);
    property Control: TControl read FControl;
    property Handle: THandle read FHandle;
  public
    constructor Create(AControl: TControl); overload;
    constructor Create(AHandle: THandle); overload;
    destructor Destroy; override;
  end;

  TcxWindowProcController = class
  private
    FWindowProcObjects: TcxObjectList;
    function GetControlWindowProcObjects(AControl: TControl): TcxWindowProcLinkedObjectList; overload;
    function GetControlWindowProcObjects(AHandle: THandle): TcxWindowProcLinkedObjectList; overload;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AControl: TControl; AWndProc: TWndMethod): TcxWindowProcLinkedObject; overload;
    function Add(AHandle: THandle; AWndProc: TWndMethod): TcxWindowProcLinkedObject; overload;
    procedure Remove(var AWndProcObject: TcxWindowProcLinkedObject);
  end;

// LOCKED STATE

  TcxLockedStatePaintHelper = class;

  IcxLockedStatePaint = interface
  ['{EB45AB76-3681-4838-BDA8-7D0081B4C184}']
    function GetImage: TcxBitmap32;
    function GetTopmostControl: TcxControl;
  end;

  IcxLockedStateFontChanged = interface
  ['{825BFA90-77C6-4725-BE95-B0A1EA8F934D}']
    procedure FontChanged(AFont: TFont);
  end;

  { TcxLockedStateImageOptions }

  TcxLockedStateImageShowingMode = (lsimNever, lsimPending, lsimImmediate);
  TcxLockedStateImageEffect = (lsieNone, lsieLight, lsieDark);

  TcxLockedStateImageAssignedValue = (lsiavFont, lsiavColor);
  TcxLockedStateImageAssignedValues = set of TcxLockedStateImageAssignedValue;

  TcxLockedStateImageOptions = class(TPersistent)
  strict private
    FAssignedValues: TcxLockedStateImageAssignedValues;
    FColor: TColor;
    FEffect: TcxLockedStateImageEffect;
    FEnabled: Boolean;
    FFont: TFont;
    FOwner: TComponent;
    FShowText: Boolean;
    FText: string;

    procedure FontChanged(Sender: TObject);
    function IsFontStored: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetAssignedValues(const Value: TcxLockedStateImageAssignedValues);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    function GetFont: TFont; virtual; abstract;
    function IsTextStored: Boolean; virtual;

    property Owner: TComponent read FOwner;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateFont(AFont: TFont);

    property Enabled: Boolean read FEnabled write FEnabled default True;
  published
    property AssignedValues: TcxLockedStateImageAssignedValues read FAssignedValues write SetAssignedValues default [];
    property Color: TColor read FColor write SetColor default clNone;
    property Effect: TcxLockedStateImageEffect read FEffect write FEffect default lsieNone;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ShowText: Boolean read FShowText write FShowText default False;
    property Text: string read FText write FText stored IsTextStored;
  end;

  { TcxLockedStatePaintHelper }

  TcxLockedStatePaintHelper = class
  strict private
    FBitmap: TcxBitmap32;
    FCount: Integer;
    FCreatingImage: Boolean;
    FIsImageReady: Boolean;
    FOwner: TComponent;

    function GetColor: TColor; inline;
    function GetEffect: TcxLockedStateImageEffect; inline;
    function GetFont: TFont; inline;
    function GetText: string; inline;
  protected
    procedure CreateImage;
    procedure DrawText;
    procedure PrepareImage; virtual;
    procedure ValidateImage;
    //virtual
    procedure AfterDestroyingImage; virtual;
    procedure BeforeCreatingImage; virtual;
    function CanCreateLockedImage: Boolean; virtual;
    function CanControlPaint: Boolean; virtual;
    function DoPrepareImage: Boolean; virtual;
    function GetOptions: TcxLockedStateImageOptions; virtual; abstract;
    function GetControl: TcxControl; virtual; abstract;
    function GetPainter: TcxCustomLookAndFeelPainter; virtual;

    property Bitmap: TcxBitmap32 read FBitmap;
    property Color: TColor read GetColor;
    property Control: TcxControl read GetControl;
    property CreatingImage: Boolean read FCreatingImage;
    property Effect: TcxLockedStateImageEffect read GetEffect;
    property Font: TFont read GetFont;
    property IsImageReady: Boolean read FIsImageReady;
    property Options: TcxLockedStateImageOptions read GetOptions;
    property Owner: TComponent read FOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property Text: string read GetText;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure BeginLockedPaint(AMode: TcxLockedStateImageShowingMode);
    procedure EndLockedPaint;
    function GetActiveCanvas: TcxCanvas; virtual;
    function GetImage: TcxBitmap32;
    function IsActive: Boolean;
  end;

  { IdxCustomizeControlsHelper }

  IdxCustomizeControlsHelper = interface
  ['{AD2130FB-EDA5-4034-A551-9C26A8DAAE41}']
    function CanProcessChildren: Boolean;
  end;

  { TdxControlsDesignSelectorHelper }

  TdxControlsDesignSelectorHelperClass = class of TdxControlsDesignSelectorHelper;

  TdxControlsDesignSelectorHelper = class(TComponent)
  private
    FControl: TControl;
    FChecked: Boolean;
    FChildren: TcxComponentList;
    FParent: TdxControlsDesignSelectorHelper;
    FSelectorBounds: TRect;
    FWindowProcObject: TcxWindowProcLinkedObject;

    function GetControlWnd: THandle;
    function GetParentControl: TWinControl;
    function PointInSelectorBounds(const P: TPoint): Boolean;
    procedure SetSelectorBounds(const AValue: TRect);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CallDefaultWndProc(var Message: TMessage);
    procedure ControlWndProc(var Message: TMessage);
    function DoControlWndProc(var Message: TMessage): Boolean;  virtual;
    function IsHitTestTransparent(const P: TPoint): Boolean; virtual;

    procedure CheckChildren; virtual;
    procedure DestroyChild(AChild: TdxControlsDesignSelectorHelper); virtual;
    function GetChildClass: TdxControlsDesignSelectorHelperClass; virtual;

    procedure PrepareChild(AItem: TdxControlsDesignSelectorHelper); virtual;

    function GetSelectorBoundsForChild(AChild: TdxControlsDesignSelectorHelper): TRect;

    function ClientToScreen(const P: TPoint): TPoint;
    function ScreenToClient(const P: TPoint): TPoint;

    function IsActiveDesignSelector: Boolean; virtual;
    function IsSelected: Boolean; virtual;
    function IsValid: Boolean; virtual;

    function IsWinControl: Boolean;
    function ControlAsWinControl: TWinControl;

    // Draw
    function CanDrawDesignSelector: Boolean; virtual;
    procedure DoDrawDesignSelector(DC: HDC); virtual;
    procedure DrawDesignSelector(DC: HDC);

    procedure StoreWndProc;
    procedure RestoreWndProc;

    property Children: TcxComponentList read FChildren;
    property ControlWnd: THandle read GetControlWnd;
    property Parent: TdxControlsDesignSelectorHelper read FParent write FParent;
    property ParentControl: TWinControl read GetParentControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function CalculateBounds(const R: TRect; AScaleFactor: TdxScaleFactor): TRect; overload;
    class function CalculateBounds(const R: TRect; AOffset, ASize: Integer; AScaleFactor: TdxScaleFactor): TRect; overload;

    property Control: TControl read FControl;
    property SelectorBounds: TRect read FSelectorBounds write SetSelectorBounds;
  end;

  { TdxRightToLeftLayoutConverter }

  TdxRightToLeftLayoutConverter = class
  public
    class function ConvertAlignment(AAlignment: TLeftRight): TLeftRight;
    class function ConvertAnchors(const AAnchors: TAnchors): TAnchors;
    class function ConvertArrowDirection(ADirection: TcxArrowDirection): TcxArrowDirection;
    class function ConvertcxDrawTextAlignment(AAlignment: Integer): Integer;
    class function ConvertcxTextAlignX(AAlign: TcxTextAlignX): TcxTextAlignX;
    class function ConvertBorders(const ABorders: TcxBorders): TcxBorders;
    class function ConvertDirection(ADirection: TcxDirection): TcxDirection;
    class function ConvertPoint(const APoint: TPoint; const AParentRect: TRect): TPoint;
    class function ConvertPopupAlignHorz(APopupAlignHorz: TcxPopupAlignHorz): TcxPopupAlignHorz;
    class function ConvertPopupAlignment(AAlignment: TPopupAlignment): TPopupAlignment;
    class function ConvertNeighbors(const ANeighbors: TcxNeighbors): TcxNeighbors;
    class function ConvertRect(const ARect, AParentRect: TRect): TRect;
    class function ConvertOffsets(const AOffsets: TRect): TRect;
    class function ConvertVirtualKeyCode(AKey: Word): Word;
    class function ConvertXCoordinate(const X: Integer; const AParentRect: TRect): Integer;
  end;

  { TdxSemiTransparentDragHelper }

  TdxSemiTransparentDragHelper = class
  strict private
    FAlpha: Byte;
    FBackgroundBitmap: TcxBitmap32;
    FControl: TcxControl;
    FPaintOffset: TPoint;
    FSelectionBitmap: TcxBitmap32;
    FSelectionsBounds: TRect;
    FSpecialControlPainting: Boolean;
    procedure CreateBackgroundBitmap;
    procedure CreateSelectionBitmap;
    procedure SetSelectionsBounds(const Value: TRect);
  strict protected
    procedure AfterSelectionPaint(ACanvas: TcxCanvas); overload; virtual;
    procedure AfterSelectionPaint(AGraphics: TdxGPGraphics); overload; virtual;
    function GetOpaqueBits(ABitmap: TcxBitmap32): TRGBColors;
    function GetSelectionImage: TdxGPImage;
    procedure PaintSelection(ACanvas: TcxCanvas); virtual;

    function GetSelectionBounds: TRect; virtual; abstract;
    procedure PaintControlSelection(ACanvas: TcxCanvas); virtual; abstract;

    property Alpha: Byte read FAlpha;
    property Control: TcxControl read FControl;
    property SelectionsBounds: TRect read FSelectionsBounds write SetSelectionsBounds;
  public
    constructor Create(AControl: TcxControl; AAlpha: Byte); virtual;
    destructor Destroy; override;
    procedure BeginSpecialPaint; virtual;
    procedure EndSpecialPaint; virtual;
    procedure Offset(X, Y: Integer);
    procedure PaintControl(ACanvas: TcxCanvas); virtual;
    procedure Resize(const ANewSelectionBounds: TRect); overload; virtual;
    procedure Resize; overload;
    procedure UpdateBackground;

    property SpecialControlPainting: Boolean read FSpecialControlPainting;
  end;

  TdxSetOperation = (soSet, soAdd, soSubtract);
  TdxWindowStyleIndex = (wsiStyle, wsiExStyle);

type
  TcxGetParentFormForDockingFunc = function (AControl: TControl): TCustomForm;
  TcxGetParentWndForDockingFunc = function (AWnd: HWND): HWND;
  TcxContextPopupHookFunc = function (ASender: TObject; APopup: TComponent; const P: TPoint): Boolean;

var
  cxContextPopupHookFunc: TcxContextPopupHookFunc = nil;
  cxDragAndDropWindowTransparency: Byte = 180;
  cxGetParentWndForDocking: TcxGetParentWndForDockingFunc = nil;
  dxApplicationActivateWindowHelper: TdxApplicationActivateWindowHelper;
  dxTouchScrollBarClass: TcxControlPopupScrollBarClass = TcxControlPopupScrollBar;

// WINDOW HANDLE
function CanAllocateHandle(AControl: TWinControl): Boolean;
function dxMapWindowPoint(AHandleFrom, AHandleTo: TcxHandle; const P: TPoint; AClient: Boolean = True): TPoint;
function dxMapWindowRect(AHandleFrom, AHandleTo: TcxHandle; const R: TRect; AClient: Boolean = True): TRect;
procedure cxRecreateControlWnd(AControl: TWinControl; APostponed: Boolean = False);
function cxClientToScreen(AHandle: THandle; const APoint: TPoint): TPoint;
function cxGetClientRect(AControl: TWinControl): TRect; overload;
function cxGetClientRect(AHandle: THandle): TRect; overload;
function cxGetClientOffset(AHandle: THandle): TPoint;
function cxGetWindowRect(AControl: TWinControl): TRect; overload;
function cxGetWindowRect(AHandle: THandle): TRect; overload;
function cxGetWindowText(AHandle: THandle): string;
function cxGetWindowBounds(AControl: TWinControl): TRect; overload;
function cxGetWindowBounds(AHandle: THandle): TRect; overload;
function cxWindowFromPoint(P: TPoint): HWND;
procedure dxSetZOrder(AHandle: THandle; AWndAfter: THandle = HWND_TOPMOST; AActivate: Boolean = False; AFlags: DWORD = 0);

//  WINDOW REPAINTING
procedure cxInvalidateRect(AHandle: THandle; const ARect: TRect; AEraseBackground: Boolean = True); overload; inline;
procedure cxInvalidateRect(AHandle: THandle; AEraseBackground: Boolean = True); overload; inline;
procedure cxInvalidateRect(AControl: TWinControl; const R: TRect; AEraseBackground: Boolean = True); overload; inline;
procedure cxInvalidateRect(AControl: TWinControl; AEraseBackground: Boolean = True); overload; inline;
procedure cxRedrawWindow(AHandle: THandle; AFlags: UINT); overload; inline;
procedure cxRedrawWindow(AHandle: THandle; const ARect: TRect; AFlags: UINT); overload; inline;
procedure cxRedrawWindow(AHandle: THandle; const ARect: TRect; AEraseBackground: Boolean = True; ARedrawNC: Boolean = False); overload; inline;
procedure cxRedrawNCRect(AHandle: THandle; const ARect: TRect);

// SYSTEM MENU
function cxLoadSysMenu(AMenuType: TcxSystemMenuType): THandle;
procedure cxModifySystemMenu(ASysMenu: THandle; AWindowHandle: THandle; AIsDialog: Boolean;
  ABorderIcons: TBorderIcons; AWindowState: TWindowState;ALockRepaint: Boolean = True);
procedure cxMoveMenuItems(ASource, ADest: THandle);

// MOUSE
function GetMouseKeys: WPARAM;
function GetDblClickInterval: Integer;
function GetMouseCursorPos: TPoint;
function GetPointPosition(const ARect: TRect; const P: TPoint; AHorzSeparation, AVertSeparation: Boolean): TcxPosition;
procedure cxSetCapture(AHandle: THandle);
function cxShiftStateMoveOnly(const AShift: TShiftState): Boolean;
function cxShiftStateLeftOnly(AShift: TShiftState; ACanIncludeDoubleClick: Boolean = False): Boolean;
function IsMouseDownMessage(AMsg: Cardinal): Boolean;

// MONITOR
function GetDesktopWorkArea(const P: TPoint): TRect;
function GetMonitorWorkArea(const AMonitor: HMONITOR): TRect;
procedure MakeVisibleOnDesktop(var ABounds: TRect; const ADesktopPoint: TPoint); overload;
procedure MakeVisibleOnDesktop(AControl: TControl); overload;

// PARENT STRAIN
function IsChildClassWindow(AWnd: HWND): Boolean;
function IsChildEx(AParentWnd, AWnd: HWND): Boolean;
function IsMDIChild(AForm: TCustomForm): Boolean;
function dxIsMDIChildWindow(AWnd: HWND): Boolean;
function IsMDIForm(AForm: TCustomForm): Boolean;
function IsOwner(AOwnerWnd, AWnd: HWND): Boolean;
function IsOwnerEx(AOwnerWnd, AWnd: HWND): Boolean;
function IsWindowEnabledEx(AWindowHandle: HWND): Boolean;
function IsControlVisible(AControl: TWinControl): Boolean;
function IsParent(AParent, AChild: TControl): Boolean;
function cxFindVCLControl(AWnd: HWND): TWinControl;
function cxFindComponent(AParentComponent: TComponent; AClass: TClass): TComponent;
function cxClientToParent(AControl: TControl; const P: TPoint; AParent: TWinControl): TPoint;
function cxIsParentFocused(AParent: THandle): Boolean;
function cxParentToClient(AControl: TControl; const P: TPoint; AParent: TWinControl): TPoint;
function dxFindParentControl(AHandle: THandle): TWinControl;
function dxGetParentForm(AHandle: THandle): TCustomForm;
procedure dxPerformMessageByQueue(AControl: TControl; AMessage: Cardinal);
function dxGetParentFormForDocking(AControl: TControl): TCustomForm;
function dxGetScreenActiveForm: TCustomForm;

// CHARS
function GetCharFromKeyCode(ACode: Word): Char;
function IsCtrlPressed: Boolean;
function IsEditStartChar(C: Char): Boolean;
function IsIncSearchStartChar(C: Char): Boolean;
function IsNumericChar(C: Char; AType: TcxNumberType): Boolean;
function IsSysKey(AKey: Word): Boolean;
function IsTextChar(C: Char): Boolean;
function ShiftStateToKeys(AShift: TShiftState): WORD;
function TranslateKey(Key: Word): Word;

// MOUSE TRACKING
procedure BeginMouseTracking(AControl: TWinControl; const ABounds: TRect; ACaller: IcxMouseTrackingCaller);
procedure EndMouseTracking(ACaller: IcxMouseTrackingCaller);
function IsMouseTracking(ACaller: IcxMouseTrackingCaller): Boolean;

// HOURGLASS
procedure HideHourglassCursor;
procedure ShowHourglassCursor;

// POPUPMENU
function GetPopupMenuHeight(APopupMenu: TPopupMenu): Integer;
function IsPopupMenu(APopupMenu: TComponent): Boolean;
function IsPopupMenuShortCut(APopupMenu: TComponent; var Message: TWMKey): Boolean;
function ShowPopupMenu(ACaller, APopupMenu: TComponent; const P: TPoint): Boolean; overload;
function ShowPopupMenu(ACaller, APopupMenu: TComponent; X, Y: Integer): Boolean; overload;
function ShowPopupMenu(ACaller, APopupMenu: TComponent; const AOwnerBounds: TRect; APopupAlignment: TPopupAlignment): Boolean; overload;
function ShowPopupMenuFromCursorPos(ACaller, AComponent: TComponent): Boolean;

// DRAG&DROP
function cxExtractDragObjectSource(ADragObject: TObject): TObject;
function cxDragDetect(Wnd: HWND; const AStartPoint: TPoint): TcxDragDetect;
function GetDragObject: TDragObject;
function IsPointInDragDetectArea(const AMouseDownPos: TPoint; X, Y: Integer): Boolean;

// WINDOWS
function dxGetUpdateRgn(AControl: TWinControl; ARegion: HRGN): Boolean;
function cxIsDrawToMemory(const AMessage: TWMEraseBkgnd): Boolean;
procedure DialogApplyFont(ADialog: TCustomForm; AFont: TFont; AFontOwner: TObject);
function cxIsUpdateLayeredWindowAvailable: Boolean;
procedure cxSetLayeredWindowAttributes(AHandle: HWND; AAlphaBlendValue: Integer);
procedure cxUpdateLayeredWindow(AHandle: HWND; ABitmap: TBitmap; AAlpha: Integer = 255); overload;
procedure cxUpdateLayeredWindow(AHandle: HWND; ABitmap: TBitmap; const ASize: TSize; AAlpha: Integer = 255); overload;
function cxWindowProcController: TcxWindowProcController;
function dxIsCurrentProcessWindow(AWnd: HWND): Boolean;
function dxIsWindowStyleSet(AHandle: THandle; AStyle: DWORD): Boolean;
procedure dxRecalculateNonClientPart(AHandle: THandle; AUpdate: Boolean = False);
function dxSetWindowProc(AHandle: THandle; ANewWindowProc: Pointer): Pointer;
function dxSetWindowStyle(AHandle: THandle; ANewStyle: TdxNativeInt;
  AOperation: TdxSetOperation = soSet; AStyleIndex: TdxWindowStyleIndex = wsiStyle): TdxNativeInt;
function dxSystemInfo: TdxSystemInfo;
function dxWindowHasRightToLeftLayout(AHandle: THandle): Boolean;
procedure cxProcessControlScrollingOnMiddleButton(AControl: TWinControl; AllowHorScrolling, AllowVerScrolling: Boolean;
  AScrollContentProc: TcxScrollControlContent; var AScrollContentFlag: Boolean);

// VISTA EXTENSION
procedure dxDrawTextOnGlass(DC: HDC; const AText: string; AFont: TFont;
  const ABounds: TRect; AColor: TColor; AFlags: DWORD; AGlowingSize: Integer; ATransparent: Boolean);
procedure dxDrawWindowOnGlass(AWnd, ADC: THandle; const ABounds: TRect; APaintBlackOpaque: Boolean = False); overload;
procedure dxDrawWindowOnGlass(AWnd: THandle; APaintBlackOpaque: Boolean = False); overload;
procedure dxPaintWindowOnGlass(AWnd: THandle; APaintBlackOpaque: Boolean = False);
procedure dxBufferedPaintControl(AControl: TWinControl);
function dxIsPaintOnGlass(AControl: TWinControl): Boolean;
{$IFDEF VCLGLASSPAINT}
procedure dxForceProcessBufferedPaintMessages(AControl: TWinControl);
{$ENDIF}

// WINDOW OBJECTS
function cxMessageWindow: TcxMessageWindow;
function dxMessagesController: TdxMessagesController;

// DESIGNER
function DesignController: TcxDesignController;
procedure SetDesignerModified(AComponent: TComponent);
function cxGetFullComponentName(AComponent: TComponent): string;
procedure cxReleaseForm(var AForm); // usually AForm is Designer

// THEMES
function cxIsVCLThemesAvailable: Boolean;
function cxIsVCLThemesEnabled: Boolean;
function cxStyleServices:{$IFDEF DELPHI16}TCustomStyleServices{$ELSE}TThemeServices{$ENDIF};

procedure dxDrawThemeEdge(ADC: HDC; ADetails: TThemedElementDetails; const R: TRect; AEdge, AFlags: Cardinal;
  AContentRect: PRect = nil);

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_APPCOMMAND_LPARAM}


implementation

{$R cxControls.res}

uses
  SysUtils, ActnList, Math, Registry, dxUxTheme, dxThemeConsts, dxTypeHelpers,
  cxLibraryConsts, cxLibraryStrs, dxGDIPlusAPI, cxDWMApi, cxFormats, dxThemeManager, dxDPIAwareUtils, dxHooks;

const
  crFullScroll = crBase + 1;
  crHorScroll = crBase + 2;
  crVerScroll = crBase + 3;
  crUpScroll = crBase + 4;
  crRightScroll = crBase + 5;
  crDownScroll = crBase + 6;
  crLeftScroll = crBase + 7;

  ScreenHandle = 0;

type
  TWinControlAccess = class(TWinControl);
  TControlAccess = class(TControl);
  TControlActionLinkAccess = class(TControlActionLink);
  TCustomFormAccess = class(TCustomForm);
  TdxScaleFactorAccess = class(TdxScaleFactor);
  TMenuItemAccess = class(TMenuItem);

  TUpdateLayeredWindowFunc = function(Handle: HWND; hdcDest: HDC; pptDst: PPoint; Size: PSize;
    hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBLENDFUNCTION; dwFlags: DWORD): Boolean; stdcall;

var
  FcxMessageWindow: TcxMessageWindow;
  FcxWindowProcController: TcxWindowProcController;
  FDesignController: TcxDesignController;
  FDragObject: TDragObject;
  FdxSystemInfo: TdxSystemInfo;
  FMessagesController: TdxMessagesController;
  FSystemController: TcxSystemController;
  FUnitIsFinalized: Boolean;
  FUpdateLayeredWindow: TUpdateLayeredWindowFunc = nil;

{ TdxTouchScrollUIActivityHelper }

function TdxTouchScrollUIActivityHelper.CheckScrollActivity(AControl: TWinControl; var Message: TMessage): Boolean;

  function GetMousePos: TPoint;
  begin
    if AControl.HandleAllocated and ((AControl.Width > 32768) or (AControl.Height > 32768)) then
      Result := AControl.ScreenToClient(GetMouseCursorPos)
    else
      Result := SmallPointToPoint(TWMMouse(Message).Pos);
  end;

  procedure DoMouseEnter;
  begin
    FMouseEnter := True;
    FLastMousePos := AControl.ScreenToClient(GetMouseCursorPos);
  end;

begin
  Result := False;
  case Message.Msg of
    CM_MOUSELEAVE:
      FMouseEnter := False;
    WM_MOUSEMOVE:
      begin
        if AControl is TcxControl then
          Result := TcxControl(AControl).IsScrollBarsArea(GetMousePos);
        if not Result then
          if FMouseEnter then
          begin
            if ((Abs(FLastMousePos.X - GetMousePos.X) >= 10) or
              (Abs(FLastMousePos.Y - GetMousePos.Y) >= 10)) then
            begin
              Result := True;
              FLastMousePos := GetMousePos;
            end;
          end
          else
            DoMouseEnter;
      end;
  end;
end;

function CanShowTouchScrollbar(AOwner: IdxTouchScrollUIOwner): Boolean;
begin
  Result := AOwner.GetOwnerControl.HandleAllocated and
    IsWindowVisible(AOwner.GetOwnerControl.Handle);
end;

function CheckTouchScrollbarPosition(AOwner: IdxTouchScrollUIOwner; AChangedWnd: HWND): Boolean;
var
  AOwnerControl: TcxControl;
begin
  AOwnerControl := AOwner.GetOwnerControl;
  Result := (AOwnerControl <> nil) and AOwnerControl.HandleAllocated and
    (IsChild(AChangedWnd, AOwnerControl.Handle) or dxIsMDIChildWindow(AChangedWnd));
end;

function CheckTouchScrollbarVisibility(AOwner: IdxTouchScrollUIOwner; AChangedWnd: HWND): Boolean;
var
  AOwnerControl: TcxControl;
begin
  AOwnerControl := AOwner.GetOwnerControl;
  Result := (AOwnerControl <> nil) and AOwnerControl.HandleAllocated and
    ((AChangedWnd = AOwnerControl.Handle) or IsChild(AChangedWnd, AOwnerControl.Handle));
end;

{ TdxTouchScrollUIModeManager }

class destructor TdxTouchScrollUIModeManager.Finalize;
begin
  FreeAndNil(FHidingTimer);
end;

class procedure TdxTouchScrollUIModeManager.Activate(AValue: IdxTouchScrollUIOwner);
begin
  if ActiveScrollUIOwner <> AValue then
  begin
    DoHideTouchScrollUI(True);
    FHidingLockCount := 0;
    FScrollInfoAllowVisible := False;
  end;
  FActiveScrollUIOwner := AValue as TObject;
end;

class procedure TdxTouchScrollUIModeManager.DoHideTouchScrollUI(AImmediately: Boolean);
begin
  if AImmediately then
  begin
    UnlockOwner;
    StopHidingTimer;
    FScrollInfoAllowVisible := False;
    FHidingLockCount := 0;
    if FActiveScrollUIOwner <> nil then
      ActiveScrollUIOwner.HideUI;
  end
  else
    UnlockHiding;
end;

class procedure TdxTouchScrollUIModeManager.CheckUIPosition(AChangedWnd: HWND);
var
  AActiveScrollUIOwner: IdxTouchScrollUIOwner;
begin
  AActiveScrollUIOwner := ActiveScrollUIOwner;
  if (AActiveScrollUIOwner <> nil) and
    AActiveScrollUIOwner.HasVisibleUI and
    CheckTouchScrollbarPosition(AActiveScrollUIOwner, AChangedWnd) then
    AActiveScrollUIOwner.CheckUIPosition;
end;

class procedure TdxTouchScrollUIModeManager.CheckUIVisibility(AChangedWnd: HWND);
var
  AActiveScrollUIOwner: IdxTouchScrollUIOwner;
begin
  AActiveScrollUIOwner := ActiveScrollUIOwner;
  if (AActiveScrollUIOwner <> nil) and
    AActiveScrollUIOwner.HasVisibleUI and
    CheckTouchScrollbarVisibility(AActiveScrollUIOwner, AChangedWnd) then
      DoHideTouchScrollUI(True);
end;

class function TdxTouchScrollUIModeManager.IsActive(AUIOwner: IdxTouchScrollUIOwner): Boolean;
begin
  Result := (AUIOwner <> nil) and (ActiveScrollUIOwner = AUIOwner);
end;

class procedure TdxTouchScrollUIModeManager.Deactivate(AUIOwner: IdxTouchScrollUIOwner);
begin
  if ActiveScrollUIOwner = AUIOwner then
  begin
//    DoHideTouchScrollUI(True);
    FActiveScrollUIOwner := nil;
    FScrollInfoAllowVisible := False;
    FHidingLockCount := 0;
    UnlockOwner;
  end;
end;

class procedure TdxTouchScrollUIModeManager.HideScrollUI(AUIOwner: IdxTouchScrollUIOwner; AImmediately: Boolean);
begin
  if ActiveScrollUIOwner = AUIOwner then
    DoHideTouchScrollUI(AImmediately);
end;

class function TdxTouchScrollUIModeManager.IsScrollUIHidden(AUIOwner: IdxTouchScrollUIOwner): Boolean;
begin
  Result := not FScrollInfoAllowVisible or not IsActive(AUIOwner);
end;

class function TdxTouchScrollUIModeManager.ActiveScrollUIOwner: IdxTouchScrollUIOwner;
begin
  Supports(FActiveScrollUIOwner, IdxTouchScrollUIOwner, Result);
end;

class procedure TdxTouchScrollUIModeManager.LockHiding;
begin
  StopHidingTimer;
  Inc(FHidingLockCount);
end;

class procedure TdxTouchScrollUIModeManager.LockOwner;
begin
  FLockOwner := True;
end;

class procedure TdxTouchScrollUIModeManager.HidingTimerHandler(ASender: TObject);
begin
  StopHidingTimer;
  DoHideTouchScrollUI(True);
end;

class procedure TdxTouchScrollUIModeManager.ShowScrollUI(AUIOwner: IdxTouchScrollUIOwner; AControlledByTimer: Boolean);
begin
  if FLockOwner and (AUIOwner <> ActiveScrollUIOwner) then
    Exit;
  if (AUIOwner = nil) or CanShowTouchScrollbar(AUIOwner) then
  begin
    Activate(AUIOwner);
    LockHiding;
    if not FScrollInfoAllowVisible then
    begin
      FScrollInfoAllowVisible := True;
      if FActiveScrollUIOwner <> nil then
        ActiveScrollUIOwner.CheckUIPosition;
    end;
    if AControlledByTimer then
      DoHideTouchScrollUI(False);
  end;
end;

class procedure TdxTouchScrollUIModeManager.StartHidingTimer;
const
  HidingTime = 1000;
begin
  if FHidingTimer = nil then
  begin
    FHidingTimer := TTimer.Create(nil);
    FHidingTimer.Interval := HidingTime;
    FHidingTimer.OnTimer := HidingTimerHandler;
  end;
  StopHidingTimer;
  FHidingTimer.Enabled := True;
end;

class procedure TdxTouchScrollUIModeManager.StopHidingTimer;
begin
  if FHidingTimer <> nil then
    FHidingTimer.Enabled := False;
end;

class procedure TdxTouchScrollUIModeManager.UnlockHiding;
begin
  if FHidingLockCount > 0 then
    Dec(FHidingLockCount);
  if (FHidingLockCount = 0) and FScrollInfoAllowVisible then
    StartHidingTimer;
end;

class procedure TdxTouchScrollUIModeManager.UnlockOwner;
begin
  FLockOwner := False;
end;

{ TdxHybridScrollbarsManager }

constructor TdxHybridScrollbarsManager.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TdxHybridScrollbarsManager.Destroy;
begin
  TdxHybridScrollbarManagers.Deactivate(Self);
  FreeAndNil(FSizeAnimation);
  FreeAndNil(FFadingAnimation);
  FreeAndNil(FCollapsingTimer);
  FreeAndNil(FHidingTimer);
  inherited Destroy;
end;

procedure TdxHybridScrollbarsManager.FadeOut;
begin
  StartFadingAnimation(fstHiding);
end;

procedure TdxHybridScrollbarsManager.FadeIn;
begin
  StartFadingAnimation(fstShowing);
end;

procedure TdxHybridScrollbarsManager.CheckScrollbarsPosition(AChangedWnd: HWND);
var
  AScrollbarsOwner: IdxHybridScrollbarOwner;
begin
  AScrollbarsOwner := ScrollbarsOwner;
  if CheckTouchScrollbarPosition(AScrollbarsOwner, AChangedWnd) then
    AScrollbarsOwner.CheckUIPosition;
end;

procedure TdxHybridScrollbarsManager.CheckScrollbarsVisibility(AChangedWnd: HWND);
begin
  if CheckTouchScrollbarVisibility(ScrollbarsOwner, AChangedWnd) then
    Hide(True, False);
end;

procedure TdxHybridScrollbarsManager.CheckStartHidingTimer;
begin
  if FHidingLockCount = 0 then
    StartHidingTimer;
end;

procedure TdxHybridScrollbarsManager.DoHide(AAnimated: Boolean);
begin
  AAnimated := AAnimated and UseAnimation;
  StopHidingTimer;
  StopSizeChangingTimer;
  if AAnimated then
    FadeOut
  else
  begin
    FAllowVisible := False;
    FHidingLockCount := 0;
    ScrollbarsOwner.HideUI;
    TdxHybridScrollbarManagers.Deactivate(Self);
    if UseAnimation then
      FFadingValue := 0;
  end;
end;

procedure TdxHybridScrollbarsManager.DoFadingAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  if FFadingState = fstShowing then
    FFadingValue := FStartFadingValue + APosition
  else
    FFadingValue := FStartFadingValue - APosition;
  Invalidate;
end;

procedure TdxHybridScrollbarsManager.DoFadingAnimationTerminate(Sender: TObject);
begin
  if FFadingState = fstHiding then
    DoHide(False)
  else
    CheckStartHidingTimer;
  FFadingState := fstNone;
end;

procedure TdxHybridScrollbarsManager.DoAfterCollapse;
begin
  if FExpandLockCount = 0 then
  begin
    FAnimateObject := nil;
    FVisibleSize := 0;
    Invalidate;
    if FHidingLockCount > 0 then
      Dec(FHidingLockCount);
    if TdxHybridScrollbarManagers.IsActive(Self) and not FIsDeactivating then
    begin
      if FAllowVisible then
        CheckStartHidingTimer;
    end
    else
      if FHidingLockCount = 0 then
        DoHide(True);
  end;
end;

procedure TdxHybridScrollbarsManager.DoSizeAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  if FExpandState = estExpanding then
    FVisibleSize := FStartSize + APosition
  else
    FVisibleSize := FStartSize - APosition;
  Invalidate;
end;

procedure TdxHybridScrollbarsManager.DoSizeAnimationTerminate(Sender: TObject);
begin
  if FExpandState = estExpanding then
    DoAfterExpand
  else
    DoAfterCollapse;

  FExpandState := estNone;
  Invalidate;
end;

function TdxHybridScrollbarsManager.GetFadingValue: Byte;
begin
  Result := FFadingValue;
end;

procedure TdxHybridScrollbarsManager.Hide(AImmediately: Boolean; AAnimated: Boolean);
begin
  if AImmediately then
  begin
    StopFadingAnimation;
    StopSizeAnimation;
    DoHide(AAnimated);
  end
  else
  begin
    if FHidingLockCount > 0 then
      Dec(FHidingLockCount);
    if FAllowVisible then
      CheckStartHidingTimer;
  end;
end;

procedure TdxHybridScrollbarsManager.Show(AHideByTimer: Boolean);
begin
  if CanShowTouchScrollbar(ScrollbarsOwner) then
  begin
    FIsDeactivating := False;
    if not UseAnimation then
      FFadingValue := MaxFadingValue;
    if not FAllowVisible then
    begin
      FAllowVisible := True;
      ScrollbarsOwner.CheckUIPosition;
      if not UseAnimation then
        Invalidate;
    end;

    if ScrollbarsOwner.HasVisibleUI then
      TdxHybridScrollbarManagers.Activate(Self);

    if not AHideByTimer then
    begin
      Inc(FHidingLockCount);
      if not UseAnimation then
        StopHidingTimer
      else
        if IsWaitingForHide then
          StopHidingTimer
        else
          if FFadingValue < MaxFadingValue then
            FadeIn;
    end
    else
      if not UseAnimation then
        CheckStartHidingTimer
      else
      begin
        if IsWaitingForHide then
          StartHidingTimer
        else
          if FFadingValue < MaxFadingValue then
            FadeIn;
      end;
  end;
end;

function TdxHybridScrollbarsManager.GetScrollbarsOwner: IdxHybridScrollbarOwner;
begin
  Supports(FOwner, IdxHybridScrollbarOwner, Result);
end;

function TdxHybridScrollbarsManager.GetVisibleSize: Integer;
begin
  Result := FVisibleSize;
end;

procedure TdxHybridScrollbarsManager.HidingTimerHandler(ASender: TObject);
begin
  StopHidingTimer;
  DoHide(True);
end;

procedure TdxHybridScrollbarsManager.Invalidate;
begin
  ScrollbarsOwner.Invalidate;
end;

procedure TdxHybridScrollbarsManager.Expand(AAnimateObject: TObject);
begin
  if FAnimateObject <> AAnimateObject then
  begin
    if FAnimateObject <> nil then
    begin
      if IsWaitingForCollapse or (FExpandState <> estNone) then
      begin
        ResetSize;
        Dec(FHidingLockCount);
      end;
    end;
    FAnimateObject := AAnimateObject;
  end;
  Inc(FExpandLockCount);
  DoExpand;
end;

procedure TdxHybridScrollbarsManager.Collapse;
begin
  if FExpandLockCount > 0 then
    Dec(FExpandLockCount);
  if FExpandState = estNone then
    if TdxHybridScrollbarManagers.IsActive(Self) and not FIsDeactivating then
      StartSizeChangingTimer
    else
      DoCollapse;
end;

procedure TdxHybridScrollbarsManager.Deactivate;
begin
  FIsDeactivating := True;
  if IsWaitingForCollapse then
  begin
    StopSizeChangingTimer;
    DoCollapse;
  end
  else
    if FExpandState = estExpanding then
    begin
      StopSizeAnimation;
      Collapse;
    end
    else
      if FFadingState = fstShowing then
      begin
        if FHidingLockCount = 0 then
        begin
          StopFadingAnimation;
          DoHide(True);
        end
      end
      else
        if IsWaitingForHide then
          DoHide(True);
end;

procedure TdxHybridScrollbarsManager.DoAfterExpand;
begin
  if FExpandLockCount = 0 then
    StartSizeChangingTimer;
end;

procedure TdxHybridScrollbarsManager.DoCollapse;
begin
  if UseAnimation then
    StartSizeAnimation(estCollapsing)
  else
    DoAfterCollapse;
end;

procedure TdxHybridScrollbarsManager.DoExpand;
begin
  if not UseAnimation then
  begin
    FVisibleSize := MaxVisibleSize;
    Invalidate;
    if IsWaitingForCollapse then
      StopSizeChangingTimer
    else
      Show(False);
  end
  else
    if FExpandState <> estExpanding then
    begin
      if IsWaitingForCollapse then
        StopSizeChangingTimer
      else
        if FExpandState = estNone then
          Show(False);
      if FVisibleSize < MaxVisibleSize then
        StartSizeAnimation(estExpanding);
    end;
end;

function TdxHybridScrollbarsManager.IsWaitingForCollapse: Boolean;
begin
  Result := (FCollapsingTimer <> nil) and FCollapsingTimer.Enabled;
end;

function TdxHybridScrollbarsManager.IsWaitingForHide: Boolean;
begin
  Result := (FHidingTimer <> nil) and FHidingTimer.Enabled;
end;

function TdxHybridScrollbarsManager.IsActive(AScrollbar: TObject): Boolean;
begin
  Result := FAnimateObject = AScrollbar;
end;

procedure TdxHybridScrollbarsManager.ScrollbarHiding(AScrollbar: TObject);
begin
  if IsActive(AScrollbar) then
  begin
    ResetSize;
    FHidingLockCount := 0;
    FFadingValue := 0;
    StopHidingTimer;
  end;
end;

procedure TdxHybridScrollbarsManager.ResetSize;
begin
  StopSizeAnimation;
  StopSizeChangingTimer;
  FVisibleSize := 0;
  FAnimateObject := nil;
end;

procedure TdxHybridScrollbarsManager.SizeChangingTimerHandler(ASender: TObject);
begin
  FCollapsingTimer.Enabled := False;
  DoCollapse;
end;

procedure TdxHybridScrollbarsManager.StartFadingAnimation(AState: TFadingState);
var
  AMaxLength, ALength: Integer;
  AMaxTime, ATime: Cardinal;
begin
  StopFadingAnimation;
  FFadingState := AState;
  AMaxLength := MaxFadingValue;
  AMaxTime := 1000;
  if FFadingState = fstShowing then
    ALength := AMaxLength - FFadingValue
  else
    ALength := FFadingValue;
  FStartFadingValue := FFadingValue;
  ATime := Round(ALength / AMaxLength * AMaxTime);
  FFadingAnimation := TdxAnimationTransition.Create(ATime, ateAccelerateDecelerate, ALength);
  FFadingAnimation.OnAnimate := DoFadingAnimate;
  FFadingAnimation.OnTerminate := DoFadingAnimationTerminate;
  FFadingAnimation.FreeOnTerminate := False;
  FFadingAnimation.Resume;
end;

procedure TdxHybridScrollbarsManager.StopFadingAnimation;
begin
  FreeAndNil(FFadingAnimation);
  FFadingState := fstNone;
end;

procedure TdxHybridScrollbarsManager.StartHidingTimer;
const
  HidingTime = 1000;
begin
  StopHidingTimer;
  if FHidingTimer = nil then
  begin
    FHidingTimer := TTimer.Create(nil);
    FHidingTimer.Interval := HidingTime;
    FHidingTimer.OnTimer := HidingTimerHandler;
  end;
  FHidingTimer.Enabled := True;
end;

procedure TdxHybridScrollbarsManager.StartSizeAnimation(AState: TExpandState);
var
  ATime, AMaxTime: Cardinal;
  ALength, AMaxLength: Integer;
begin
  StopSizeAnimation;
  FExpandState := AState;
  FStartSize := FVisibleSize;
  AMaxTime := 100;
  AMaxLength := MaxVisibleSize;
  if FExpandState = estExpanding then
    ALength := AMaxLength - FVisibleSize
  else
    ALength := FVisibleSize;
  ATime := Round(ALength / AMaxLength * AMaxTime);
  FSizeAnimation := TdxAnimationTransition.Create(ATime, ateLinear, ALength);
  FSizeAnimation.OnAnimate := DoSizeAnimate;
  FSizeAnimation.OnTerminate := DoSizeAnimationTerminate;
  FSizeAnimation.FreeOnTerminate := False;
  FSizeAnimation.Resume;
end;

procedure TdxHybridScrollbarsManager.StartSizeChangingTimer;
begin
  StopSizeChangingTimer;
  FCollapsingTimer := TcxTimer.Create(nil);
  FCollapsingTimer.Interval := 500;
  FCollapsingTimer.OnTimer := SizeChangingTimerHandler;
  FCollapsingTimer.Enabled := True;
end;

procedure TdxHybridScrollbarsManager.StopHidingTimer;
begin
  if IsWaitingForHide then
    FHidingTimer.Enabled := False;
end;

function TdxHybridScrollbarsManager.UseAnimation: Boolean;
begin
  Result := True;
end;

procedure TdxHybridScrollbarsManager.StopSizeAnimation;
begin
  FreeAndNil(FSizeAnimation);
  FExpandState := estNone;
end;

procedure TdxHybridScrollbarsManager.StopSizeChangingTimer;
begin
  FreeAndNil(FCollapsingTimer);
end;

{ TdxHybridScrollbarManagers }

class procedure TdxHybridScrollbarManagers.Activate(AManager: TdxHybridScrollbarsManager);
var
  I: Integer;
  AIndex: Integer;
begin
  AIndex := FItems.IndexOf(AManager);
  if AIndex = -1 then
    FItems.Add(AManager)
  else
    FItems.Move(AIndex, FItems.Count - 1);
  for I := FItems.Count - 2 downto 0 do
    FItems[I].Deactivate;
end;

class procedure TdxHybridScrollbarManagers.Deactivate(AManager: TdxHybridScrollbarsManager);
begin
  if FItems <> nil then
    FItems.Remove(AManager);
end;

class function TdxHybridScrollbarManagers.IsActive(AManager: TdxHybridScrollbarsManager): Boolean;
begin
  Result := (FItems.Count > 0) and
    (FItems.Last = AManager);
end;

class procedure TdxHybridScrollbarManagers.CheckScrollbarsPosition(AChangedWnd: HWND);
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FItems[I].CheckScrollbarsPosition(AChangedWnd);
end;

class procedure TdxHybridScrollbarManagers.CheckScrollbarsVisibility(AChangedWnd: HWND);
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FItems[I].CheckScrollbarsVisibility(AChangedWnd);
end;

class constructor TdxHybridScrollbarManagers.Initialize;
begin
  FItems := TList<TdxHybridScrollbarsManager>.Create;
end;

class destructor TdxHybridScrollbarManagers.Finalize;
begin
  FreeAndNil(FItems);
end;

function GetTouchScrollUIControlOwnerWnd(AOwnerControl: TWinControl): HWND;
var
  AForm: TCustomForm;
  APopup: IdxPopupControl;
begin
  Result := 0;
  AForm := GetParentForm(AOwnerControl);
  if (AForm <> nil) and AForm.HandleAllocated then
    if IsMDIChild(AForm) then
      Result := Application.MainFormHandle
    else
      if not IsChildClassWindow(AForm.Handle) then
        Result := AForm.Handle
      else
        if Supports(AForm, IdxPopupControl, APopup) then
          Result := GetTouchScrollUIControlOwnerWnd(APopup.GetOwnerControl);
  if Result = 0 then
    Result := Application.ActiveFormHandle;
end;

function dxGetUpdateRgn(AControl: TWinControl; ARegion: HRGN): Boolean;
const
  ValidClipRegions: set of Byte = [SIMPLEREGION, COMPLEXREGION];
begin
  if IsWinSeven and not IsCompositionEnabled then
    Result := False
  else
    Result := GetUpdateRgn(AControl.Handle, ARegion, False) in ValidClipRegions;
end;

function cxIsUpdateLayeredWindowAvailable: Boolean;
begin
  Result := Assigned(FUpdateLayeredWindow);
end;

procedure cxSetLayeredWindowAttributes(AHandle: HWND; AAlphaBlendValue: Integer);
var
  AStyle: Integer;
begin
  if Assigned(SetLayeredWindowAttributes) then
  begin
    AStyle := GetWindowLong(AHandle, GWL_EXSTYLE);
    if AAlphaBlendValue < 255 then
    begin
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(AHandle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
      SetLayeredWindowAttributes(AHandle, 0, AAlphaBlendValue, LWA_ALPHA);
    end
    else
      if (AStyle and WS_EX_LAYERED) <> 0 then
      begin
        SetWindowLong(AHandle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
        cxRedrawWindow(AHandle, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
      end;
  end;
end;

procedure cxUpdateLayeredWindow(AHandle: HWND; ABitmap: TBitmap; AAlpha: Integer = 255);
begin
  cxUpdateLayeredWindow(AHandle, ABitmap, cxSize(ABitmap.Width, ABitmap.Height), AAlpha);
end;

procedure cxUpdateLayeredWindow(AHandle: HWND; ABitmap: TBitmap; const ASize: TSize; AAlpha: Integer = 255);
var
  ABlend: TBlendFunction;
  APoint: TPoint;
begin
  APoint := cxNullPoint;
  ABlend.BlendOp := AC_SRC_OVER;
  ABlend.BlendFlags := 0;
  ABlend.SourceConstantAlpha := AAlpha;
  ABlend.AlphaFormat := AC_SRC_ALPHA;
  if cxIsUpdateLayeredWindowAvailable then
    FUpdateLayeredWindow(AHandle, 0, nil, @ASize, ABitmap.Canvas.Handle, @APoint, 0, @ABlend, LWA_ALPHA);
end;

function cxWindowProcController: TcxWindowProcController;
begin
  if (FcxWindowProcController = nil) and not FUnitIsFinalized then
    FcxWindowProcController := TcxWindowProcController.Create;
  Result := FcxWindowProcController;
end;

function dxIsCurrentProcessWindow(AWnd: HWND): Boolean;
var
  AProcessId: Cardinal;
begin
  GetWindowThreadProcessId(AWnd, @AProcessId);
  Result := (AWnd <> 0) and (AProcessId = GetCurrentProcessId);
end;

function dxIsWindowStyleSet(AHandle: THandle; AStyle: DWORD): Boolean;
begin
  Result := GetWindowLong(AHandle, GWL_STYLE) and AStyle = AStyle;
end;

procedure dxRecalculateNonClientPart(AHandle: THandle; AUpdate: Boolean = False);
begin
  SetWindowPos(AHandle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
  if AUpdate then
    UpdateWindow(AHandle);
end;

function dxSetWindowProc(AHandle: THandle; ANewWindowProc: Pointer): Pointer;
begin
  Result := Pointer(SetWindowLong(AHandle, GWL_WNDPROC, TdxNativeInt(ANewWindowProc)));
end;

function dxSetWindowStyle(AHandle: THandle; ANewStyle: TdxNativeInt;
  AOperation: TdxSetOperation = soSet; AStyleIndex: TdxWindowStyleIndex = wsiStyle): TdxNativeInt;
const
  StyleIndexMap: array[TdxWindowStyleIndex] of TdxNativeInt = (GWL_STYLE, GWL_EXSTYLE);
var
  AStyle: Integer;
begin
  case AOperation of
    soAdd:
      AStyle := GetWindowLong(AHandle, StyleIndexMap[AStyleIndex]) or ANewStyle;
    soSubtract:
      AStyle := GetWindowLong(AHandle, StyleIndexMap[AStyleIndex]) and not ANewStyle;
    else // soSet
      AStyle := ANewStyle;
  end;
  Result := SetWindowLong(AHandle, StyleIndexMap[AStyleIndex], AStyle);
end;

function dxSystemInfo: TdxSystemInfo;
begin
  if (FdxSystemInfo = nil) and not FUnitIsFinalized then
    FdxSystemInfo := TdxSystemInfo.Create;
  Result := FdxSystemInfo;
end;

function dxWindowHasRightToLeftLayout(AHandle: THandle): Boolean;
begin
  Result := GetWindowLong(AHandle, GWL_EXSTYLE) and WS_EX_LAYOUTRTL <> 0;
end;

procedure dxDrawTextOnGlass(DC: HDC; const AText: string; AFont: TFont;
  const ABounds: TRect; AColor: TColor; AFlags: DWORD; AGlowingSize: Integer;
  ATransparent: Boolean);
var
  AInfo: TBitmapInfo;
  AMemoryDC: HDC;
  AOptions: TdxDTTOpts;
  ATheme: TdxTheme;
  DIB, AOldBitmap: HBITMAP;
  P: Pointer;
  R: TRect;
begin
  AMemoryDC := CreateCompatibleDC(DC);

  AInfo.bmiHeader.biSize := SizeOf(TBitmapInfo);
  AInfo.bmiHeader.biWidth := cxRectWidth(ABounds);
  AInfo.bmiHeader.biHeight := -cxRectHeight(ABounds);
  AInfo.bmiHeader.biPlanes := 1;
  AInfo.bmiHeader.biBitCount := 32;
  AInfo.bmiHeader.biCompression := BI_RGB;

  DIB := CreateDIBSection(DC, AInfo, DIB_RGB_COLORS, P, 0, 0);
  AOldBitmap := SelectObject(AMemoryDC, dib);

  R := cxRect(0, 0, ABounds.Right - ABounds.Left, ABounds.Bottom - ABounds.Top);
  if ATransparent then
    cxBitBlt(AMemoryDC, DC, R, ABounds.TopLeft, SRCCOPY);

  if AFont <> nil then
    SelectObject(AMemoryDC, AFont.Handle)
  else
    SelectObject(AMemoryDC, GetCurrentObject(DC, OBJ_FONT));

  AOptions.dwSize := SizeOf(TdxDTTOpts);
  AOptions.dwFlags := DTT_COMPOSITED or DTT_TEXTCOLOR;

  if AColor <> clDefault then
    AOptions.crText := ColorToRGB(AColor)
  else
    AOptions.crText := GetTextColor(DC);

  if AGlowingSize > 0 then
  begin
    AOptions.dwFlags := AOptions.dwFlags or DTT_GLOWSIZE;
    AOptions.iGlowSize := AGlowingSize;
  end;

  ATheme := OpenTheme(totWindow);
  DrawThemeTextEx(ATheme, AMemoryDC, 0, 0, AText, -1, AFlags, R, AOptions);
  cxBitBlt(DC, AMemoryDC, ABounds, cxNullPoint, SRCCOPY);

  SelectObject(AMemoryDC, AOldBitmap);
  DeleteObject(DIB);
  DeleteDC(AMemoryDC);
end;

procedure dxDrawWindowOnGlass(AWnd, ADC: THandle; const ABounds: TRect; APaintBlackOpaque: Boolean);

  procedure DoDraw(ADC: THandle);
  begin
    SendMessage(AWnd, WM_ERASEBKGND, ADC, ADC);
    SendMessage(AWnd, WM_PRINTCLIENT, ADC, PRF_CLIENT);
  end;

var
  AMemDC: HDC;
  APaintBuffer: THandle;
begin
  if not cxRectIsEmpty(ABounds) then
  begin
    APaintBuffer := BeginBufferedPaint(ADC, @ABounds, BPBF_COMPOSITED, nil, AMemDC);
    if APaintBuffer <> 0 then
    try
      BufferedPaintClear(APaintBuffer, ABounds);
      DoDraw(AMemDC);
      if not APaintBlackOpaque then
        BufferedPaintSetAlpha(APaintBuffer, @ABounds, 255);
    finally
      if GetFocus = AWnd then
      begin
        HideCaret(AWnd);
        EndBufferedPaint(APaintBuffer, True);
        ShowCaret(AWnd);
      end
      else
        EndBufferedPaint(APaintBuffer, True);
    end
    else
      DoDraw(ADC);
  end;
end;

procedure dxDrawWindowOnGlass(AWnd: THandle; APaintBlackOpaque: Boolean);
var
  R: TRect;
  ADC: THandle;
begin
  ADC := GetDC(AWnd);
  try
    GetClientRect(AWnd, R);
    dxDrawWindowOnGlass(AWnd, ADC, R, APaintBlackOpaque);
  finally
    ReleaseDC(AWnd, ADC);
  end;
end;

procedure dxPaintWindowOnGlass(AWnd: THandle; APaintBlackOpaque: Boolean);
var
  ADC: HDC;
  PS: TPaintStruct;
begin
  ADC := BeginPaint(AWnd, PS);
  try
    dxDrawWindowOnGlass(AWnd, ADC, PS.rcPaint, APaintBlackOpaque);
  finally
    EndPaint(AWnd, PS);
  end;
end;

procedure dxBufferedPaintControl(AControl: TWinControl);
var
  ADC, AMemDC: HDC;
  AClipRgn: HRgn;
  AHasClipRgn: Boolean;
  AMemBitmap, AOldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  AClipRgn := CreateRectRgn(0, 0, 0, 0);
  AHasClipRgn := dxGetUpdateRgn(AControl, AClipRgn);
  ADC := BeginPaint(AControl.Handle, PS);
  if cxRectIsEmpty(PS.rcPaint) then
  begin
    DeleteObject(AClipRgn);
    EndPaint(AControl.Handle, PS);
    Exit;
  end;
  AMemBitmap := CreateCompatibleBitmap(ADC, cxRectWidth(PS.rcPaint), cxRectHeight(PS.rcPaint));
  try
    AMemDC := CreateCompatibleDC(ADC);
    AOldBitmap := SelectObject(AMemDC, AMemBitmap);
    try
      SetWindowOrgEx(AMemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
      if AHasClipRgn then
      begin
        OffsetRgn(AClipRgn, -PS.rcPaint.Left, -PS.rcPaint.Top);
        SelectClipRgn(AMemDC, AClipRgn);
      end;
      AControl.Perform(WM_ERASEBKGND, AMemDC, AMemDC);
      AControl.Perform(WM_PAINT, AMemDC, 0);
      cxBitBlt(ADC, AMemDC, PS.rcPaint, PS.rcPaint.TopLeft, SRCCOPY);
    finally
      SelectObject(AMemDC, AOldBitmap);
      DeleteDC(AMemDC);
    end;
  finally
    DeleteObject(AClipRgn);
    DeleteObject(AMemBitmap);
    EndPaint(AControl.Handle, PS);
  end;
end;

function dxIsPaintOnGlass(AControl: TWinControl): Boolean;
begin
  Result := AControl.HandleAllocated and (csGlassPaint in AControl.ControlState) and IsCompositionEnabled;
end;

{$IFDEF VCLGLASSPAINT}
procedure dxForceProcessBufferedPaintMessages(AControl: TWinControl);
var
  AMessage: TMsg;
begin
  if (AControl <> nil) and AControl.HandleAllocated and not (csDestroying in AControl.ComponentState) then
  begin
    if dxIsPaintOnGlass(AControl) then
    begin
      while PeekMessage(AMessage, AControl.Handle, CM_BUFFEREDPRINTCLIENT, CM_BUFFEREDPRINTCLIENT, PM_REMOVE) do
        DispatchMessage(AMessage);
    end;
  end;
end;
{$ENDIF}

function CanAllocateHandle(AControl: TWinControl): Boolean;
begin
  Result :=
    (AControl <> nil) and
    (AControl.HandleAllocated or
      not (csDestroying in AControl.ComponentState) and
      ((AControl.ParentWindow <> 0) or
         CanAllocateHandle(AControl.Parent) or
         ((AControl.Parent = nil) and (Application <> nil) and (Application.Handle <> 0) and
           ((AControl is TCustomForm) and (not IsMDIChild(TCustomForm(AControl)) or CanAllocateHandle(Application.MainForm)) or (AControl is TCustomFrame)))));
end;

function cxIsDrawToMemory(const AMessage: TWMEraseBkgnd): Boolean;
begin
  Result := TMessage(AMessage).wParam = WPARAM(TMessage(AMessage).lParam);
end;

function cxMessageWindow: TcxMessageWindow;
begin
  if (FcxMessageWindow = nil) and not FUnitIsFinalized then
    FcxMessageWindow := TcxMessageWindow.Create;
  Result := FcxMessageWindow;
end;

function dxMessagesController: TdxMessagesController;
begin
  if (FMessagesController = nil) and not FUnitIsFinalized then
    FMessagesController := TdxMessagesController.Create;
  Result := FMessagesController;
end;

function cxIsVCLThemesAvailable: Boolean;
begin
{$IFDEF DELPHI16}
  Result := cxStyleServices.Available;
{$ELSE}
  Result := cxStyleServices.ThemesAvailable;
{$ENDIF}
end;

function cxIsVCLThemesEnabled: Boolean;
begin
{$IFDEF DELPHI16}
  Result := cxStyleServices.Enabled;
{$ELSE}
  Result := cxStyleServices.ThemesEnabled;
{$ENDIF}
end;

function cxStyleServices:{$IFDEF DELPHI16}TCustomStyleServices{$ELSE}TThemeServices{$ENDIF};
begin
  Result := {$IFDEF DELPHI16}StyleServices{$ELSE}ThemeServices{$ENDIF};
end;

procedure dxDrawThemeEdge(ADC: HDC; ADetails: TThemedElementDetails;
  const R: TRect; AEdge, AFlags: Cardinal; AContentRect: PRect = nil);
begin
{$IFDEF DELPHI16}
  cxStyleServices.DrawEdge(ADC, ADetails, R,
    TElementEdges(TStyleElementEdges(AEdge)), TStyleElementEdgeFlags(AFlags),
    AContentRect);
{$ELSE}
  cxStyleServices.DrawEdge(ADC, ADetails, R, AEdge, AFlags, AContentRect);
{$ENDIF}
end;

function cxDragDetect(Wnd: HWND; const AStartPoint: TPoint): TcxDragDetect;
var
  NoDragZone: TRect;
  P: TPoint;
  Msg: TMsg;
begin
  Result := ddCancel;

  NoDragZone.Right := 2 * Mouse.DragThreshold;//GetSystemMetrics(SM_CXDRAG);
  NoDragZone.Bottom := 2 * Mouse.DragThreshold;//GetSystemMetrics(SM_CYDRAG);
  NoDragZone.Left := AStartPoint.X - NoDragZone.Right div 2;
  NoDragZone.Top := AStartPoint.Y - NoDragZone.Bottom div 2;
  Inc(NoDragZone.Right, NoDragZone.Left);
  Inc(NoDragZone.Bottom, NoDragZone.Top);

  cxSetCapture(Wnd);
  try
    while GetCapture = Wnd do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
            PostQuitMessage(Msg.wParam);
            Break;
          end;
      end;
      try
        case Msg.message of
          WM_KEYDOWN, WM_KEYUP:
            if Msg.wParam = VK_ESCAPE then Break;
          WM_MOUSEMOVE:
            if Msg.hwnd = Wnd then
            begin
              P := Point(LoWord(Msg.lParam), HiWord(Msg.lParam));
              ClientToScreen(Msg.hwnd, P);
              if not PtInRect(NoDragZone, P) then
              begin
                Result := ddDrag;
                Break;
              end;
            end;
          WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
            begin
              Result := ddNone;
              Break;
            end;
        end;
      finally
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = Wnd then ReleaseCapture;
  end;
end;

function GetCharFromKeyCode(ACode: Word): Char;
const
  MAPVK_VK_TO_VSC = 0;

var
  AScanCode: UINT;
  AKeyState: TKeyboardState;
  ABufChar: Word;
begin
  ABufChar := 0;
  AScanCode := MapVirtualKey(ACode, MAPVK_VK_TO_VSC);
  GetKeyboardState(AKeyState);
  if ToUnicode(ACode, AScanCode, AKeyState, ABufChar, 1, 0) = 1 then
    Result := Char(ABufChar)
  else
    Result := #0;
end;

function GetMouseKeys: WPARAM;
begin
  Result := 0;
  if GetAsyncKeyState(VK_LBUTTON) < 0 then Inc(Result, MK_LBUTTON);
  if GetAsyncKeyState(VK_MBUTTON) < 0 then Inc(Result, MK_MBUTTON);
  if GetAsyncKeyState(VK_RBUTTON) < 0 then Inc(Result, MK_RBUTTON);
  if GetAsyncKeyState(VK_CONTROL) < 0 then Inc(Result, MK_CONTROL);
  if GetAsyncKeyState(VK_SHIFT) < 0 then Inc(Result, MK_SHIFT);
end;

function GetDblClickInterval: Integer;
begin
  Result := GetDoubleClickTime;
end;

function GetDesktopWorkArea(const P: TPoint): TRect;
begin
  Result := GetMonitorWorkArea(MonitorFromPoint(P, MONITOR_DEFAULTTONEAREST));
end;

function GetMonitorWorkArea(const AMonitor: HMONITOR): TRect;
var
  Info: TMonitorInfo;
begin
  if AMonitor = 0 then
    Result := Screen.WorkAreaRect
  else
  begin
    Info.cbSize := SizeOf(Info);
    GetMonitorInfo(AMonitor, @Info);
    Result := Info.rcWork;
  end;
end;

function GetMouseCursorPos: TPoint;
begin
  if not Windows.GetCursorPos(Result) then
    Result := cxInvalidPoint;
end;

function GetPointPosition(const ARect: TRect; const P: TPoint; AHorzSeparation, AVertSeparation: Boolean): TcxPosition;
const
  Positions: array[Boolean, Boolean] of TcxPosition = ((posBottom, posRight), (posLeft, posTop));

  function GetCornerCoords(ACorner: TdxCorner): TPoint;
  begin
    case ACorner of
      coBottomLeft:
        Result := Point(ARect.Left, ARect.Bottom);
      coTopLeft:
        Result := ARect.TopLeft;
      coTopRight:
        Result := Point(ARect.Right, ARect.Top);
      coBottomRight:
        Result := ARect.BottomRight;
    end;
  end;

  function GetSign(const P1, P2, P: TPoint): Integer;
  begin
    Result := (P.X - P1.X) * (P2.Y - P1.Y) - (P.Y - P1.Y) * (P2.X - P1.X);
  end;

var
  ASign1, ASign2: Integer;
begin
  if AHorzSeparation and AVertSeparation then
  begin
    ASign1 := GetSign(GetCornerCoords(coBottomLeft), GetCornerCoords(coTopRight), P);
    ASign2 := GetSign(GetCornerCoords(coTopLeft), GetCornerCoords(coBottomRight), P);
    Result := Positions[ASign1 >= 0, ASign2 >= 0];
  end
  else
    if AHorzSeparation then
      if P.X < GetRangeCenter(ARect.Left, ARect.Right) then
        Result := posLeft
      else
        Result := posRight
    else
      if AVertSeparation then
        if P.Y < GetRangeCenter(ARect.Top, ARect.Bottom) then
          Result := posTop
        else
          Result := posBottom
      else
        Result := posNone;
end;

procedure cxSetCapture(AHandle: THandle);
begin
  if GetCapture <> AHandle then
    SetCapture(AHandle);
end;

function cxShiftStateMoveOnly(const AShift: TShiftState): Boolean;
begin
  Result := AShift * [ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble] = [];
end;

function cxShiftStateLeftOnly(AShift: TShiftState; ACanIncludeDoubleClick: Boolean = False): Boolean;
begin
  Result := (ssLeft in AShift) and
    (AShift * [ssShift, ssAlt, ssCtrl, ssRight, ssMiddle] = []) and
    (ACanIncludeDoubleClick or not (ssDouble in AShift));
end;

function IsMouseDownMessage(AMsg: Cardinal): Boolean;
begin
  Result :=
    (AMsg = WM_LBUTTONDOWN) or (AMsg = WM_LBUTTONDBLCLK) or
    (AMsg = WM_RBUTTONDOWN) or (AMsg = WM_RBUTTONDBLCLK) or
    (AMsg = WM_MBUTTONDOWN) or (AMsg = WM_MBUTTONDBLCLK) or
    (AMsg = WM_NCLBUTTONDOWN) or (AMsg = WM_NCLBUTTONDBLCLK) or
    (AMsg = WM_NCRBUTTONDOWN) or (AMsg = WM_NCRBUTTONDBLCLK) or
    (AMsg = WM_NCMBUTTONDOWN) or (AMsg = WM_NCMBUTTONDBLCLK);
end;

function IsChildClassWindow(AWnd: HWND): Boolean;
begin
  Result := GetWindowLong(AWnd, GWL_STYLE) and WS_CHILD = WS_CHILD;
end;

function IsChildEx(AParentWnd, AWnd: HWND): Boolean;
begin
  Result := (AParentWnd <> 0) and ((AParentWnd = AWnd) or IsChild(AParentWnd, AWnd));
end;

function FormStyle(AForm: TCustomForm): TFormStyle;
begin
  Result := TCustomFormAccess(AForm).FormStyle;
end;

function IsMDIChild(AForm: TCustomForm): Boolean;
begin
  Result := (AForm <> nil) and not (csDesigning in AForm.ComponentState) and
    (FormStyle(AForm) = fsMDIChild);
end;

function dxIsMDIChildWindow(AWnd: HWND): Boolean;
var
  AControl: TWinControl;
begin
  AControl := FindControl(AWnd);
  Result := (AControl is TCustomForm) and IsMDIChild(TCustomForm(AControl));
end;

function IsMDIForm(AForm: TCustomForm): Boolean;
begin
  Result := (AForm <> nil) and not (csDesigning in AForm.ComponentState) and
    (FormStyle(AForm) = fsMDIForm);
end;

function IsCtrlPressed: Boolean;
begin
  Result := GetAsyncKeyState(VK_CONTROL) < 0;
end;

function IsEditStartChar(C: Char): Boolean;
begin
  Result := IsTextChar(C) or (C = #8) or (C = ^V) or (C = ^X);
end;

function IsIncSearchStartChar(C: Char): Boolean;
begin
  Result := IsTextChar(C) or (C = #8);
end;

function IsOwner(AOwnerWnd, AWnd: HWND): Boolean;
begin
  if AOwnerWnd <> 0 then
    repeat
      AWnd := GetParent(AWnd);
      Result := AWnd = AOwnerWnd;
    until Result or (AWnd = 0)
  else
    Result := False;
end;

function IsOwnerEx(AOwnerWnd, AWnd: HWND): Boolean;
begin
  Result := (AOwnerWnd <> 0) and ((AOwnerWnd = AWnd) or IsOwner(AOwnerWnd, AWnd));
end;

function IsWindowEnabledEx(AWindowHandle: HWND): Boolean;
begin
  Result := IsWindowEnabled(AWindowHandle);
  if IsChildClassWindow(AWindowHandle) then
    Result := Result and IsWindowEnabledEx(GetAncestor(AWindowHandle, GA_PARENT));
end;

function IsControlVisible(AControl: TWinControl): Boolean;
begin
  Result := (AControl <> nil) and AControl.HandleAllocated and IsWindowVisible(AControl.Handle);
end;

function IsParent(AParent, AChild: TControl): Boolean;
begin
  Result := False;
  if AParent = nil then Exit;
  while AChild <> nil do
  begin
    Result := (AChild.Parent = AParent);
    if Result then
      Break;
    AChild := AChild.Parent;
  end;
end;

function cxFindVCLControl(AWnd: HWND): TWinControl;
begin
  Result := nil;
  while (Result = nil) and (AWnd <> 0) do
  begin
    Result := FindControl(AWnd);
    AWnd := GetAncestor(AWnd, GA_PARENT);
  end;
end;

function cxFindComponent(AParentComponent: TComponent; AClass: TClass): TComponent;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AParentComponent.ComponentCount - 1 do
    if AParentComponent.Components[I] is AClass then
    begin
      Result := AParentComponent.Components[I];
      Break;
    end;
end;

function cxClientToParent(AControl: TControl; const P: TPoint;
  AParent: TWinControl): TPoint;
begin
  Result := AControl.ClientToParent(P, AParent);
end;

function cxParentToClient(AControl: TControl; const P: TPoint;
  AParent: TWinControl): TPoint;
begin
  Result := AControl.ParentToClient(P, AParent);
end;

function cxIsParentFocused(AParent: THandle): Boolean;
begin
  Result := (AParent <> 0) and ((AParent = GetFocus) or IsChildClassWindow(AParent) and cxIsParentFocused(GetParent(AParent)));
end;

function dxFindParentControl(AHandle: THandle): TWinControl;
begin
  repeat
    Result := FindControl(AHandle);
    if Result <> nil then
      Break;
    AHandle := GetAncestor(AHandle, GA_PARENT);
  until AHandle = 0;
end;

function dxGetParentForm(AHandle: THandle): TCustomForm;
var
  AControl: TWinControl;
begin
  AControl := dxFindParentControl(AHandle);
  if AControl <> nil then
    Result := GetParentForm(AControl)
  else
    Result := nil;
end;

function dxGetParentFormForDocking(AControl: TControl): TCustomForm;
var
  AFloatForm: IdxFloatForm;
begin
  Result := GetParentForm(AControl);
  if Supports(Result, IdxFloatForm, AFloatForm)  then
    Result := AFloatForm.GetParentForm;
end;

function dxGetScreenActiveForm: TCustomForm;
begin
  if Screen.ActiveCustomForm <> nil then
    Result := Screen.ActiveCustomForm
  else
    Result := dxGetParentForm(GetActiveWindow);
end;

procedure dxPerformMessageByQueue(AControl: TControl; AMessage: Cardinal);
begin
  while AControl <> nil do
  begin
    AControl.Perform(AMessage, 0, 0);
    AControl := AControl.Parent;
    if AControl is TCustomForm then
      Break;
  end;
end;

function IsPointInDragDetectArea(const AMouseDownPos: TPoint; X, Y: Integer): Boolean;
begin
  Result :=
    (Abs(X - AMouseDownPos.X) < Mouse.DragThreshold) and
    (Abs(Y - AMouseDownPos.Y) < Mouse.DragThreshold);
end;

function IsNumericChar(C: Char; AType: TcxNumberType): Boolean;
begin
  Result := (C = '-') or (C = '+') or (dxGetWideCharCType1(C) and C1_DIGIT > 0);
  if AType in [ntFloat, ntExponent] then
  begin
    Result := Result or (C = dxFormatSettings.DecimalSeparator);
    if AType = ntExponent then
      Result := Result or (C = 'e') or (C = 'E');
  end;
end;

function IsSysKey(AKey: Word): Boolean;
begin
  Result := AKey in [VK_CANCEL,
    VK_TAB, VK_CLEAR, VK_RETURN, VK_SHIFT, VK_CONTROL,
    VK_MENU, VK_PAUSE, VK_CAPITAL, VK_ESCAPE, VK_PRIOR..VK_HELP,
    VK_LWIN..VK_SCROLL, VK_LSHIFT..VK_RMENU, VK_PROCESSKEY, VK_ATTN..VK_OEM_CLEAR];
end;

function IsTextChar(C: Char): Boolean;
begin
  Result := not (dxGetWideCharCType1(C) and C1_CNTRL > 0);
end;

procedure MakeVisibleOnDesktop(var ABounds: TRect; const ADesktopPoint: TPoint);
var
  ADesktopBounds: TRect;
begin
  ADesktopBounds := GetDesktopWorkArea(ADesktopPoint);
  if ABounds.Right > ADesktopBounds.Right then
    Offsetrect(ABounds, ADesktopBounds.Right - ABounds.Right, 0);
  if ABounds.Bottom > ADesktopBounds.Bottom then
    OffsetRect(ABounds, 0, ADesktopBounds.Bottom - ABounds.Bottom);
  if ABounds.Left < ADesktopBounds.Left then
    OffsetRect(ABounds, ADesktopBounds.Left - ABounds.Left, 0);
  if ABounds.Top < ADesktopBounds.Top then
    OffsetRect(ABounds, 0, ADesktopBounds.Top - ABounds.Top);
end;

procedure MakeVisibleOnDesktop(AControl: TControl);
var
  ABounds: TRect;
begin
  ABounds := AControl.BoundsRect;
  MakeVisibleOnDesktop(ABounds, ABounds.TopLeft);
  AControl.BoundsRect := ABounds;
end;

function dxMapWindowPoint(AHandleFrom, AHandleTo: TcxHandle; const P: TPoint; AClient: Boolean = True): TPoint;
var
  AWindowRectFrom, AWindowRectTo: TRect;
begin
  Result := P;
  if AClient then
    MapWindowPoints(AHandleFrom, AHandleTo, Result, 1)
  else
  begin
    AWindowRectFrom := cxGetWindowRect(AHandleFrom);
    AWindowRectTo := cxGetWindowRect(AHandleTo);
    Result := cxPointOffset(Result, AWindowRectFrom.TopLeft);
    Result := cxPointOffset(Result, AWindowRectTo.TopLeft, False);
  end;
end;

function dxMapWindowRect(AHandleFrom, AHandleTo: TcxHandle; const R: TRect; AClient: Boolean): TRect;
begin
  Result := cxRectSetOrigin(R, dxMapWindowPoint(AHandleFrom, AHandleTo, R.TopLeft, AClient));
end;

procedure cxRecreateControlWnd(AControl: TWinControl; APostponed: Boolean = False);
begin
  if (AControl <> nil) and AControl.HandleAllocated then
    if APostponed then
      PostMessage(AControl.Handle, CM_RECREATEWND, 0, 0)
    else
      AControl.Perform(CM_RECREATEWND, 0, 0);
end;

function cxClientToScreen(AHandle: THandle; const APoint: TPoint): TPoint;
begin
  Result := APoint;
  ClientToScreen(AHandle, Result);
end;

function cxGetClientRect(AHandle: THandle): TRect;
begin
  if not GetClientRect(AHandle, Result) then
    Result := cxEmptyRect;
end;

function cxGetClientOffset(AHandle: THandle): TPoint;
var
  R: TRect;
  P: TPoint;
begin
  R := cxGetWindowRect(AHandle);
  if dxWindowHasRightToLeftLayout(AHandle) then
  begin
    P := cxClientToScreen(AHandle, cxNullPoint);
    Result.X := R.Right - P.X;
    Result.Y := P.Y - R.Top;
  end
  else
    Result:= cxPointOffset(cxClientToScreen(AHandle, cxNullPoint), R.TopLeft, False);
end;

function cxGetClientRect(AControl: TWinControl): TRect;
begin
  if not AControl.HandleAllocated or not GetClientRect(AControl.Handle, Result) then
    Result := cxEmptyRect;
end;

function cxGetWindowRect(AHandle: THandle): TRect;
begin
  if not GetWindowRect(AHandle, Result) then
    Result := cxEmptyRect;
end;

function cxGetWindowText(AHandle: THandle): string;
var
  ABuffer: array[0..1023] of Char;
begin
  FillChar(ABuffer, SizeOf(ABuffer), 0);
  if GetWindowText(AHandle, PChar(@ABuffer[0]), Length(ABuffer)) > 0 then
    Result := ABuffer
  else
    Result := '';
end;

function cxGetWindowRect(AControl: TWinControl): TRect;
begin
  if AControl.HandleAllocated then
    Result := cxGetWindowRect(AControl.Handle)
  else
    Result := cxEmptyRect;
end;

function cxGetWindowBounds(AControl: TWinControl): TRect;
begin
  Result := cxGetWindowBounds(AControl.Handle);
end;

function cxGetWindowBounds(AHandle: THandle): TRect;
begin
  Result := cxGetWindowRect(AHandle);
  Result := cxRectSetNullOrigin(Result);
end;

function cxWindowFromPoint(P: TPoint): HWND;

  function FindOne(Wnd: HWND; P: TPoint): HWND;
  begin
    Result := ChildWindowFromPointEx(Wnd, P, CWP_SKIPINVISIBLE);
    if Result = 0 then
      Result := Wnd
    else
      if Result <> Wnd then
        Result := FindOne(Result, dxMapWindowPoint(Wnd, Result, P));
  end;

begin
  Result := WindowFromPoint(P);
  if Result <> 0 then
  begin
    ScreenToClient(Result, P);
    Result := FindOne(Result, P);
  end;
end;

procedure dxSetZOrder(AHandle: THandle; AWndAfter: THandle; AActivate: Boolean; AFlags: DWORD);
const
  AActivateMap: array[Boolean] of Byte = (SWP_NOACTIVATE, 0);
begin
  SetWindowPos(AHandle, AWndAfter, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or AActivateMap[AActivate] or AFlags);
end;

procedure cxInvalidateRect(AHandle: THandle; const ARect: TRect; AEraseBackground: Boolean = True);
begin
  InvalidateRect(AHandle, @ARect, AEraseBackground);
end;

procedure cxInvalidateRect(AHandle: THandle; AEraseBackground: Boolean = True);
begin
  InvalidateRect(AHandle, nil, AEraseBackground);
end;

procedure cxInvalidateRect(AControl: TWinControl; const R: TRect; AEraseBackground: Boolean = True);
begin
  if AControl.HandleAllocated then
    cxInvalidateRect(AControl.Handle, R, AEraseBackground);
end;

procedure cxInvalidateRect(AControl: TWinControl; AEraseBackground: Boolean = True);
begin
  if AControl.HandleAllocated then
    cxInvalidateRect(AControl.Handle, AEraseBackground);
end;

procedure cxRedrawWindow(AHandle: THandle; AFlags: UINT);
begin
  RedrawWindow(AHandle, nil, 0, AFlags);
end;

procedure cxRedrawWindow(AHandle: THandle; const ARect: TRect; AFlags: UINT);
begin
  RedrawWindow(AHandle, @ARect, 0, AFlags);
end;

procedure cxRedrawWindow(AHandle: THandle; const ARect: TRect; AEraseBackground: Boolean = True; ARedrawNC: Boolean = False);
var
  AFlags: Cardinal;
begin
  AFlags := RDW_INVALIDATE;
  if AEraseBackground then
    AFlags := AFlags or RDW_ERASE;
  if ARedrawNC then
    AFlags := AFlags or RDW_FRAME;
  cxRedrawWindow(AHandle, ARect, AFlags);
end;

procedure cxRedrawNCRect(AHandle: THandle; const ARect: TRect);
var
  ARgn: TcxRegionHandle;
begin
  ARgn := CreateRectRgnIndirect(ARect);
  SendMessage(AHandle, WM_NCPAINT, ARgn, 0);
  DeleteObject(ARgn);
end;

function cxLoadSysMenu(AMenuType: TcxSystemMenuType): THandle;
const
  ID_SYSMENU       = $10;
  ID_CLOSEMENU     = $20;
  CHILDSYSMENU     = ID_CLOSEMENU;
  ID_DIALOGSYSMENU = $30;
  MenuTypes: array[TcxSystemMenuType] of UINT = (ID_SYSMENU, CHILDSYSMENU, ID_DIALOGSYSMENU);
var
  AMenuInfo: TMenuInfo;
  AMenuItemInfo: TMenuItemInfo;
begin
  Result := LoadMenu(GetModuleHandle(user32), MAKEINTRESOURCE(MenuTypes[AMenuType]));
  if Result = 0 then
    Exit;
  //Add the check or bmp style. (draw bitmaps and checkmarks on the same column)
  AMenuInfo.cbSize := sizeof(AMenuInfo);
  AMenuInfo.fMask  := MIM_STYLE or MIM_APPLYTOSUBMENUS;
  AMenuInfo.dwStyle := MNS_CHECKORBMP;
  SetMenuInfo(Result, AMenuInfo);
  // Add the bitmaps for close, minimize, maximize and restore items.
  if not IsWin95 then
    AMenuItemInfo.cbSize := SizeOf(AMenuItemInfo)
  else
  begin
    AMenuItemInfo.cbSize := 44;
    AMenuItemInfo.hbmpItem := 0;
  end;
  AMenuItemInfo.fMask := MIIM_BITMAP;
  AMenuItemInfo.hbmpItem := HBMMENU_POPUP_CLOSE;
  SetMenuItemInfo(Result, SC_CLOSE, False, AMenuItemInfo);
  if AMenuType <> smDialog then
  begin
    AMenuItemInfo.hbmpItem := HBMMENU_POPUP_MINIMIZE;
    SetMenuItemInfo(Result, SC_MINIMIZE, False, AMenuItemInfo);
    AMenuItemInfo.hbmpItem := HBMMENU_POPUP_MAXIMIZE;
    SetMenuItemInfo(Result, SC_MAXIMIZE, False, AMenuItemInfo);
    AMenuItemInfo.hbmpItem := HBMMENU_POPUP_RESTORE;
    SetMenuItemInfo(Result, SC_RESTORE, False, AMenuItemInfo);
  end;
end;

function IsSmallerThanScreen(AWnd: THandle): Boolean;
var
  dxMax, dyMax: Integer;
  AMonitor: HMONITOR;
  AWorkArea, AWindowRect: TRect;
begin
  AMonitor  := MonitorFromWindow(AWnd, MONITOR_DEFAULTTOPRIMARY);
  AWorkArea := GetMonitorWorkArea(AMonitor);
  dxMax := AWorkArea.Right - AWorkArea.Left;
  dyMax := AWorkArea.Bottom - AWorkArea.Top;
  GetWindowRect(AWnd, AWindowRect);
  Result := (AWindowRect.Right - AWindowRect.Left < dxMax) or
    (AWindowRect.Bottom - AWindowRect.Top < dyMax);
end;

procedure SetupSystemMenuItems(AWnd: THandle; AMenu: THandle = 0);
var
  ASize, AMinimize, AMaximize, AMove, ARestore, ADefault: UINT;
  AStyles: Integer;

  function TestWF(AFlag: Integer): Boolean;
  begin
    Result := AStyles and AFlag = AFlag;
  end;

begin
  AStyles := GetWindowLong(AWnd, GWL_STYLE);
  ASize := 0;
  AMaximize := 0;
  AMinimize := 0;
  AMove := 0;
  ARestore := MFS_GRAYED;
  ADefault := SC_CLOSE;
  // Minimized state: no minimize, restore.
  if IsIconic(AWnd) then
  begin
    ARestore  := 0;
    AMinimize := MFS_GRAYED;
    ASize     := MFS_GRAYED;
    ADefault  := SC_RESTORE;
  end
  else
    if not TestWF(WS_MINIMIZEBOX) then
       AMinimize := MFS_GRAYED;
  // Maximized state: no maximize, restore.
  if not TestWF(WS_MAXIMIZEBOX)  then
    AMaximize := MFS_GRAYED
  else
    if IsZoomed(AWnd) then
    begin
      ARestore := 0;
      AMove := MFS_GRAYED;
      if not TestWF(WS_CHILD) and IsSmallerThanScreen(AWnd) then
        AMove := 0;
      ASize     := MFS_GRAYED;
      AMaximize := MFS_GRAYED;
    end;
  if not TestWF(WS_SIZEBOX) then
    ASize := MFS_GRAYED;
  // Update menuitems
  EnableMenuItem(AMenu, SC_SIZE, ASize or MF_BYCOMMAND);
  EnableMenuItem(AMenu, SC_MINIMIZE, AMinimize or MF_BYCOMMAND);
  EnableMenuItem(AMenu, SC_MAXIMIZE, AMaximize or MF_BYCOMMAND);
  EnableMenuItem(AMenu, SC_RESTORE, ARestore or MF_BYCOMMAND);
  EnableMenuItem(AMenu, SC_MOVE, AMove or MF_BYCOMMAND);
  SetMenuDefaultItem(AMenu, ADefault, MF_BYCOMMAND);
end;

procedure cxModifySystemMenu(ASysMenu, AWindowHandle: THandle;
  AIsDialog: Boolean; ABorderIcons: TBorderIcons; AWindowState: TWindowState;
  ALockRepaint: Boolean = True);
begin
  if biSystemMenu in ABorderIcons then
  try
    if ALockRepaint then
      LockWindowUpdate(AWindowHandle);
    if AIsDialog then
    begin
      DeleteMenu(ASysMenu, SC_TASKLIST, MF_BYCOMMAND);
      DeleteMenu(ASysMenu, 7, MF_BYPOSITION);
      DeleteMenu(ASysMenu, 5, MF_BYPOSITION);
      DeleteMenu(ASysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
      DeleteMenu(ASysMenu, SC_MINIMIZE, MF_BYCOMMAND);
      DeleteMenu(ASysMenu, SC_SIZE, MF_BYCOMMAND);
      DeleteMenu(ASysMenu, SC_RESTORE, MF_BYCOMMAND);
    end;
    SetupSystemMenuItems(AWindowHandle, ASysMenu);
  finally
    if ALockRepaint then
      LockWindowUpdate(0);
  end;
end;

procedure cxMoveMenuItems(ASource, ADest: THandle);
const
  dxItemInfoMaskGeneral = MIIM_CHECKMARKS or MIIM_ID or MIIM_STATE or
    MIIM_SUBMENU or MIIM_TYPE or MIIM_DATA;
var
  I: Integer;
  ABuffer: array[0..511] of Char;
  AItemID: UINT;
  AMenuItemInfo: TMenuItemInfo;

  procedure InitItemInfo(AMask: UINT);
  begin
    FillChar(AMenuItemInfo, SIzeOf(AMenuItemInfo), 0);
    if not IsWin95 then
      AMenuItemInfo.cbSize := SizeOf(AMenuItemInfo)
    else
      AMenuItemInfo.cbSize := 44;
    AMenuItemInfo.dwTypeData := ABuffer;
    AMenuItemInfo.cch := 512;
    AMenuItemInfo.fMask := AMask;
  end;

  procedure SetItemInfo(ASourceItemIndex, ADestItem: Integer; AByPos: Boolean; AMask: UINT);
  begin
    InitItemInfo(AMask);
    GetMenuItemInfo(ASource, ASourceItemIndex, True, AMenuItemInfo);
    SetMenuItemInfo(ADest, ADestItem, AByPos, AMenuItemInfo);
  end;

begin
  if (ASource = ADest) or not (IsMenu(ASource) and IsMenu(ADest)) then
    Exit;
  for I := GetMenuItemCount(ADest) - 1 downto 0 do
  begin
    AItemID := GetMenuItemID(ADest, I);
    DeleteMenu(ADest, AItemID, MF_BYCOMMAND);
  end;
  for I := 0 to GetMenuItemCount(ASource) - 1 do
  begin
    InitItemInfo(dxItemInfoMaskGeneral);
    GetMenuItemInfo(ASource, I, True, AMenuItemInfo);
    InsertMenuItem(ADest, I, True, AMenuItemInfo);
    SetItemInfo(I, I, True, MIIM_STRING);
    if not IsWin95 then
      SetItemInfo(I, I, True, MIIM_BITMAP);
  end;
  // some magic for aero
  EnableMenuItem(ADest, SC_CLOSE, MF_BYCOMMAND or MF_GRAYED);
  EnableMenuItem(ADest, SC_CLOSE, MF_BYCOMMAND or MF_ENABLED);
end;

procedure SetDesignerModified(AComponent: TComponent);

  function IsValidComponentState(AComponent: TComponent): Boolean;
  begin
    Result := AComponent.ComponentState * [csLoading, csWriting, csDestroying] = [];
  end;

  function CanSetModified(AControl: TControl): Boolean;
  begin
    Result := IsValidComponentState(AControl) and ((AControl.Parent = nil) or CanSetModified(AControl.Parent));
  end;

  function IsDesigning: Boolean;
  begin
    Result := ((AComponent is TcxControl) and (AComponent as TcxControl).IsDesigning) or
      (not (AComponent is TcxControl) and (csDesigning in AComponent.ComponentState));
  end;

var
  ADesigner: IDesignerNotify;
begin
  if not IsDesigning or not IsValidComponentState(AComponent) or
      ((AComponent is TControl) and not CanSetModified(AComponent as TControl)) then
    Exit;
  ADesigner := FindRootDesigner(AComponent);
  if ADesigner <> nil then
    ADesigner.Modified;
end;

function cxGetFullComponentName(AComponent: TComponent): string;
begin
  Result := '';

  while (AComponent <> nil) do
  begin
    if AComponent.Name <> '' then
      if Result <> '' then
        Result := AComponent.Name + '.' + Result
      else
        Result := AComponent.Name;
    AComponent := AComponent.Owner;
  end;
end;

procedure cxReleaseForm(var AForm);
var
  ATempForm: TCustomForm;
begin
  if TObject(AForm) = nil then
    Exit;
  ATempForm := TObject(AForm) as TCustomForm;
  TObject(AForm) := nil;
  if ATempForm.HandleAllocated then
    ATempForm.Release
  else
    ATempForm.Free;
end;

function ShiftStateToKeys(AShift: TShiftState): WORD;
begin
  Result := 0;
  if ssShift in AShift then Inc(Result, MK_SHIFT);
  if ssCtrl in AShift then Inc(Result, MK_CONTROL);
  if ssLeft in AShift then Inc(Result, MK_LBUTTON);
  if ssRight in AShift then Inc(Result, MK_RBUTTON);
  if ssMiddle in AShift then Inc(Result, MK_MBUTTON);
end;

function TranslateKey(Key: Word): Word;
begin
  Result := Key;
end;

procedure cxProcessControlScrollingOnMiddleButton(AControl: TWinControl; AllowHorScrolling, AllowVerScrolling: Boolean;
  AScrollContentProc: TcxScrollControlContent; var AScrollContentFlag: Boolean);
const
  ScrollTimeStep = 20;
  ScrollValueStep = 5;
  MaxSpeed = 12;
var
  BreakOnMouseUp: Boolean;
  P, PrevP, AnchorPos: TPoint;
  AnchorSize, Speed, TimerHits: Integer;
  AnchorWnd, CaptureWnd: HWND;
  ADirection: TcxDirection;
  ATimer: TdxNativeUInt;
  Msg: TMsg;

  function CreateScrollingAnchorWnd: HWND;
  var
    B: TBitmap;
    W, H: Integer;
    Rgn: HRGN;
    DC: HDC;

    function GetResourceBitmapName: string;
    begin
      if AllowHorScrolling and AllowVerScrolling then
        Result := 'CX_FULLSCROLLBITMAP'
      else if AllowHorScrolling then
        Result := 'CX_HORSCROLLBITMAP'
      else
        Result := 'CX_VERSCROLLBITMAP';
    end;

  begin
    B := TBitmap.Create;
    try
      B.LoadFromResourceName(HInstance, GetResourceBitmapName);

      W := B.Width;
      H := B.Height;
      AnchorSize := W;
      with AnchorPos do
        Result := CreateWindowEx(WS_EX_TOPMOST, 'STATIC', nil, WS_POPUP,
          X - W div 2, Y - H div 2, W, H, AControl.Handle, 0, HInstance, nil);
      Rgn := CreateEllipticRgn(0, 0, W + 1, H + 1);
      SetWindowRgn(Result, Rgn, True);
      SetWindowPos(Result, 0, 0, 0, 0, 0,
        SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);

      DC := GetWindowDC(Result);
      BitBlt(DC, 0, 0, W, H, B.Canvas.Handle, 0, 0, SRCCOPY);
      Rgn := CreateEllipticRgn(0, 0, W + 1, H + 1);
      FrameRgn(DC, Rgn, GetSysColorBrush(COLOR_WINDOWTEXT), 1, 1);
      DeleteObject(Rgn);
      ReleaseDC(Result, DC);
    finally
      B.Free;
    end;
  end;

  procedure CalcDirectionAndSpeed(const P: TPoint);
  var
    DeltaX, DeltaY, SpeedValue: Integer;

    function GetNeutralZone: TRect;
    begin
      Result := Classes.Bounds(AnchorPos.X - AnchorSize div 2, AnchorPos.Y - AnchorSize div 2, AnchorSize, AnchorSize);
      if not AllowHorScrolling then
      begin
        Result.Left := 0;
        Result.Right := Screen.Width;
      end;
      if not AllowVerScrolling then
      begin
        Result.Top := 0;
        Result.Bottom := Screen.Height;
      end;
    end;

  begin
    if PtInRect(GetNeutralZone, P) then
    begin
      ADirection := dirNone;
      Speed := 0;
      Exit;
    end
    else
    begin
      BreakOnMouseUp := True;
      DeltaX := P.X - AnchorPos.X;
      DeltaY := P.Y - AnchorPos.Y;
      if AllowHorScrolling and (not AllowVerScrolling or (Abs(DeltaX) > Abs(DeltaY))) then
      begin
        if DeltaX < 0 then
          ADirection := dirLeft
        else
          ADirection := dirRight;
        SpeedValue := Abs(DeltaX);
      end
      else
      begin
        if DeltaY < 0 then
          ADirection := dirUp
        else
          ADirection := dirDown;
        SpeedValue := Abs(DeltaY);
      end;
    end;
    Dec(SpeedValue, AnchorSize div 2);
    Speed := 1 + SpeedValue div ScrollValueStep;
    if Speed > MaxSpeed then Speed := MaxSpeed;
  end;

  procedure SetMouseCursor;
  var
    Cursor: TCursor;
  begin
    case ADirection of
      dirLeft:
        Cursor := crLeftScroll;
      dirUp:
        Cursor := crUpScroll;
      dirRight:
        Cursor := crRightScroll;
      dirDown:
        Cursor := crDownScroll;
    else
      if AllowHorScrolling and AllowVerScrolling then
        Cursor := crFullScroll
      else
        if AllowHorScrolling then
          Cursor := crHorScroll
        else
          Cursor := crVerScroll;
    end;
    SetCursor(Screen.Cursors[Cursor]);
  end;

var
  AMessage: TMessage;
begin
  if not (AllowHorScrolling or AllowVerScrolling) then Exit;

  AScrollContentFlag := True;
  BreakOnMouseUp := False;
  PrevP := GetMouseCursorPos;
  AnchorPos := PrevP;
  AnchorWnd := CreateScrollingAnchorWnd;
  ADirection := dirNone;
  SetMouseCursor;
  Speed := 1;
  TimerHits := 0;
  ATimer := SetTimer(0, 0, ScrollTimeStep, nil);

  CaptureWnd := AControl.Handle;
  cxSetCapture(CaptureWnd);
  try
    while GetCapture = CaptureWnd do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
            PostQuitMessage(Msg.wParam);
            Break;
          end;
      end;
      AMessage.Msg := Msg.message;
      AMessage.WParam := Msg.wParam;
      AMessage.LParam := msg.lParam;
      if (Msg.message = WM_PAINT) and (Msg.hwnd = AnchorWnd) then
      begin
        ValidateRect(AnchorWnd, nil);
        Continue;
      end;
      if (Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST) and
        (Msg.message <> WM_MOUSEMOVE) and (Msg.message <> WM_MBUTTONUP) then
        Break;
      try
        case Msg.message of
          WM_KEYDOWN, WM_KEYUP:
            if TWMKey(AMessage).CharCode = VK_ESCAPE then Break;
          WM_MOUSEMOVE:
            begin
              P := SmallPointToPoint(TWMMouse(AMessage).Pos);
              Windows.ClientToScreen(Msg.hwnd, P);
              if (P.X <> PrevP.X) or (P.Y <> PrevP.Y) then
              begin
                CalcDirectionAndSpeed(P);
                SetMouseCursor;
                PrevP := P;
              end;
            end;
          WM_LBUTTONDOWN, WM_RBUTTONDOWN:
            Break;
          WM_MBUTTONUP:
            if BreakOnMouseUp then Break;
          WM_TIMER:
            if TWMTimer(AMessage).TimerID = WPARAM(ATimer) then
            begin
              Inc(TimerHits);
              if TimerHits mod (MaxSpeed - Speed + 1) = 0 then
                AScrollContentProc(ADirection);
            end;
        end;
      finally
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = CaptureWnd then ReleaseCapture;

    KillTimer(0, ATimer);
    DestroyWindow(AnchorWnd);
    AScrollContentFlag := False;
  end;
end;

{ mouse tracking }

var
  FMouseTrackingTimerList: TList;

function MouseTrackingTimerList: TList;
begin
  if not FUnitIsFinalized and (FMouseTrackingTimerList = nil) then
    FMouseTrackingTimerList := TList.Create;
  Result := FMouseTrackingTimerList;
end;

type
  TMouseTrackingTimer = class(TcxTimer)
  protected
    procedure TimerHandler(Sender: TObject);
  public
    Caller: IcxMouseTrackingCaller;
    Control: TWinControl;
    Bounds: TRect;
    constructor Create(AOwner: TComponent); override;
  end;

constructor TMouseTrackingTimer.Create(AOwner: TComponent);
begin
  inherited;
  Interval := 10;
  OnTimer := TimerHandler;
end;

procedure TMouseTrackingTimer.TimerHandler(Sender: TObject);

  function IsParentFormDisabled: Boolean;
  begin
    Result := not (Control.HandleAllocated and IsWindowEnabledEx(Control.Handle));
  end;

  function IsPointInControl: Boolean;
  begin
    Result := PtInRect(Control.ClientRect, Control.ScreenToClient(GetMouseCursorPos));
  end;

  function IsMouseCursorInOverlaidWindow(const APoint: TPoint): Boolean;
  begin
    Result := (Control <> nil) and Control.HandleAllocated and
      not IsChildEx(Control.Handle, cxWindowFromPoint(APoint)) and
       (GetCapture <> Control.Handle);
  end;

  function PtInCaller: Boolean;
  var
    ACaller2: IcxMouseTrackingCaller2;
    APoint: TPoint;
  begin
    APoint := GetMouseCursorPos;
    if IsMouseCursorInOverlaidWindow(APoint) then
      Result := False
    else
      if not Supports(Caller, IcxMouseTrackingCaller2, ACaller2) then
        Result := PtInRect(Bounds, APoint)
      else
        if Control <> nil then
          Result := Control.HandleAllocated and ACaller2.PtInCaller(Control.ScreenToClient(APoint))
        else
          Result := ACaller2.PtInCaller(APoint);
  end;

  function NeedFinishMouseTracking: Boolean;
  var
    ACaller3: IcxMouseTrackingCaller3;
  begin
    Result := not PtInCaller and
      (not Supports(Caller, IcxMouseTrackingCaller3, ACaller3) or not ACaller3.IsCaptureMouse)
      or (Control <> nil) and IsParentFormDisabled;
  end;

var
  ACaller: IcxMouseTrackingCaller;
  AControl: TWinControl;
begin
  if NeedFinishMouseTracking then
  begin
    ACaller := Caller;
    AControl := Control;
    Enabled := False;
    if (AControl <> nil) and AControl.HandleAllocated and (not IsPointInControl or IsParentFormDisabled) then
      SendMessage(AControl.Handle, CM_MOUSELEAVE, 0, LPARAM(AControl));
    if ACaller <> nil then
      ACaller.MouseLeave;
    EndMouseTracking(ACaller);
  end;
end;

procedure BeginMouseTracking(AControl: TWinControl; const ABounds: TRect; ACaller: IcxMouseTrackingCaller);
var
  ATimer: TMouseTrackingTimer;
begin
  if FUnitIsFinalized or IsMouseTracking(ACaller) then Exit;
  ATimer := TMouseTrackingTimer.Create(nil);
  ATimer.Control := AControl;
  ATimer.Bounds := ABounds;
  if ATimer.Control <> nil then
    ATimer.Bounds := dxMapWindowRect(ATimer.Control.Handle, ScreenHandle, ATimer.Bounds);
  ATimer.Caller := ACaller;
  MouseTrackingTimerList.Add(ATimer);
end;

function GetMouseTrackingTimer(ACaller: IcxMouseTrackingCaller): TMouseTrackingTimer;
var
  I: Integer;
begin
  if not FUnitIsFinalized then
  begin
    for I := 0 to MouseTrackingTimerList.Count - 1 do
    begin
      Result := TMouseTrackingTimer(MouseTrackingTimerList[I]);
      if Result.Caller = ACaller then Exit;
    end;
  end;
  Result := nil;
end;

procedure EndMouseTracking(ACaller: IcxMouseTrackingCaller);
var
  ATimer: TMouseTrackingTimer;
begin
  ATimer := GetMouseTrackingTimer(ACaller);
  if ATimer <> nil then
  begin
    MouseTrackingTimerList.Remove(ATimer);
    ATimer.Free;
  end;
end;

{ hourglass cursor showing }

var
  FPrevScreenCursor: TCursor;
  FHourglassCursorUseCount: Integer;

function IsMouseTracking(ACaller: IcxMouseTrackingCaller): Boolean;
begin
  Result := not FUnitIsFinalized and (GetMouseTrackingTimer(ACaller) <> nil);
end;

procedure HideHourglassCursor;
begin
  if FHourglassCursorUseCount <> 0 then
  begin
    Dec(FHourglassCursorUseCount);
    if FHourglassCursorUseCount = 0 then
      Screen.Cursor := FPrevScreenCursor;
  end;
end;

procedure ShowHourglassCursor;
begin
  if FHourglassCursorUseCount = 0 then
  begin
    FPrevScreenCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
  end;
  Inc(FHourglassCursorUseCount);
end;

{ popup menu routines }

function GetPopupMenuHeight(APopupMenu: TPopupMenu): Integer;

  function IsOwnerDrawItem(AMenuItem: TMenuItem): Boolean;
  begin
    Result := APopupMenu.OwnerDraw or (AMenuItem.GetImageList <> nil) or
      not AMenuItem.Bitmap.Empty;
  end;

const
  AMenuItemHeightCorrection = 4;
  APopupMenuHeightCorrection = 4;
var
  ACanvas: TcxScreenCanvas;
  AMenuItemDefaultHeight, AMenuItemHeight, AMenuItemWidth, AMenuColumnHeight, I: Integer;
  AMenuItem: TMenuItem;
begin
  ACanvas := TcxScreenCanvas.Create;
  try
    ACanvas.Font.Assign(Screen.MenuFont);
    AMenuItemDefaultHeight := ACanvas.TextHeight('Qg') + AMenuItemHeightCorrection;
    AMenuColumnHeight := 0;
    Result := 0;
    for I := 0 to APopupMenu.Items.Count - 1 do
    begin
      AMenuItem := APopupMenu.Items[I];
      AMenuItem.InitiateAction;
      if AMenuItem.Visible then
      begin
        AMenuItemHeight := AMenuItemDefaultHeight;
        if IsOwnerDrawItem(AMenuItem) then
          TMenuItemAccess(AMenuItem).MeasureItem(ACanvas.Canvas,
            AMenuItemWidth, AMenuItemHeight);
        if AMenuItem.Break <> mbNone then
        begin
          Result := Max(AMenuColumnHeight, Result);
          AMenuColumnHeight := 0;
        end;
        Inc(AMenuColumnHeight, AMenuItemHeight);
      end;
    end;
    Result := Max(AMenuColumnHeight, Result);
    Inc(Result, APopupMenuHeightCorrection);
  finally
    ACanvas.Free;
  end;
end;

function IsPopupMenu(APopupMenu: TComponent): Boolean;
begin
  Result := (APopupMenu is TPopupMenu) or Supports(APopupMenu, IcxPopupMenu);
end;

function IsPopupMenuShortCut(APopupMenu: TComponent; var Message: TWMKey): Boolean;
var
  AIcxPopupMenu: IcxPopupMenu;
begin
  if Supports(APopupMenu, IcxPopupMenu, AIcxPopupMenu) then
    Result := AIcxPopupMenu.IsShortCutKey(Message)
  else
    Result := (APopupMenu is TPopupMenu) and (TPopupMenu(APopupMenu).WindowHandle <> 0) and
      TPopupMenu(APopupMenu).IsShortCut(Message);
end;

function ShowPopupMenu(ACaller, APopupMenu: TComponent; const P: TPoint): Boolean;
begin
  Result := ShowPopupMenu(ACaller, APopupMenu, P.X, P.Y);
end;

function ShowPopupMenu(ACaller, APopupMenu: TComponent; X, Y: Integer): Boolean;
var
  AIcxPopupMenu: IcxPopupMenu;
  AStdPopupMenu: TPopupMenu;
begin
  Result := True;
  if Supports(APopupMenu, IcxPopupMenu, AIcxPopupMenu) then
    AIcxPopupMenu.Popup(X, Y)
  else
    if (APopupMenu is TPopupMenu) and TPopupMenu(APopupMenu).AutoPopup then
    begin
      AStdPopupMenu := TPopupMenu(APopupMenu);
      AStdPopupMenu.PopupComponent := ACaller;
      AStdPopupMenu.Popup(X, Y);
    end
    else
      Result := False;
end;

function ShowPopupMenu(ACaller, APopupMenu: TComponent;
  const AOwnerBounds: TRect; APopupAlignment: TPopupAlignment): Boolean;

  function GetPopupPoint: TPoint;
  var
    AAlignment: TPopupAlignment;
  begin
    Result := cxPoint(AOwnerBounds.Left, AOwnerBounds.Bottom);
    if Application.UseRightToLeftAlignment then
      AAlignment := TdxRightToLeftLayoutConverter.ConvertPopupAlignment(APopupAlignment)
    else
      AAlignment := APopupAlignment;
    case AAlignment of
      paRight:
        Inc(Result.X, cxRectWidth(AOwnerBounds));
      paCenter:
        Inc(Result.X, cxRectWidth(AOwnerBounds) div 2);
    end;
  end;

  procedure CheckPopupMenuPosition(APopupMenu: TPopupMenu; var P: TPoint);
  var
    AMenuHeight: Integer;
  begin
    AMenuHeight := GetPopupMenuHeight(APopupMenu);
    if P.Y + AMenuHeight > GetDesktopWorkArea(P).Bottom then
      Dec(P.Y, cxRectHeight(AOwnerBounds) + AMenuHeight + 2);
  end;

  procedure CheckPopupAlignment(var APopupAlignment: TPopupAlignment; const P: TPoint);
  var
    ADesktopWorkArea: TRect;
  begin
    ADesktopWorkArea := GetDesktopWorkArea(P);
    if P.X <= ADesktopWorkArea.Left then
      APopupAlignment := paRight
    else
      if P.X >= ADesktopWorkArea.Right then
        APopupAlignment := paLeft;
  end;

var
  AIcxPopupMenu: IcxPopupMenu2;
  AStdPopupMenu: TPopupMenu;
  APoint: TPoint;
  APrevPopupAlignment: TPopupAlignment;
begin
  Result := True;
  APoint := GetPopupPoint;
  if Supports(APopupMenu, IcxPopupMenu2, AIcxPopupMenu) then
    AIcxPopupMenu.Popup(APoint.X, APoint.Y, AOwnerBounds, APopupAlignment)
  else
    if (APopupMenu is TPopupMenu) and TPopupMenu(APopupMenu).AutoPopup then
    begin
      AStdPopupMenu := TPopupMenu(APopupMenu);
      APrevPopupAlignment := AStdPopupMenu.Alignment;
      try
        CheckPopupMenuPosition(AStdPopupMenu, APoint);
        CheckPopupAlignment(APopupAlignment, APoint);
        AStdPopupMenu.Alignment := APopupAlignment;
        AStdPopupMenu.PopupComponent := ACaller;
        AStdPopupMenu.Popup(APoint.X, APoint.Y);
      finally
        AStdPopupMenu.Alignment := APrevPopupAlignment;
      end;
    end
    else
      Result := False;
end;

function ShowPopupMenuFromCursorPos(ACaller, AComponent: TComponent): Boolean;
begin
  Result := ShowPopupMenu(ACaller, AComponent, GetMouseCursorPos);
end;

function cxExtractDragObjectSource(ADragObject: TObject): TObject;
begin
  if ADragObject is TcxDragControlObject then
    Result := TcxDragControlObject(ADragObject).Control
  else
    Result := ADragObject;
end;

function GetDragObject: TDragObject;
begin
  Result := FDragObject;
end;

{ other }

procedure DialogApplyFont(ADialog: TCustomForm; AFont: TFont; AFontOwner: TObject);
begin
  if AFont <> nil then
  begin
    ADialog.Font.Assign(AFont);
    ADialog.Font.Height := dxGetScaleFactor(ADialog).Apply(AFont.Height, dxGetScaleFactor(AFontOwner));
  end;
end;

function DesignController: TcxDesignController;
begin
  if (FDesignController = nil) and not FUnitIsFinalized then
    FDesignController := TcxDesignController.Create;
  Result := FDesignController;
end;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Integer;
begin
  Result := Short(HiWord(lParam) and not FAPPCOMMAND_MASK);
end;

{ TdxApplicationActivateWindowHelper }

constructor TdxApplicationActivateWindowHelper.Create;
begin
  inherited Create;
  FWindowsList := TList.Create;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
end;

destructor TdxApplicationActivateWindowHelper.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FWindowsList);
  inherited Destroy;
end;

procedure TdxApplicationActivateWindowHelper.AddWindow(AForm: TCustomForm);
begin
  FFreeNotificator.AddSender(AForm);
  WindowsList.Add(AForm);
end;

procedure TdxApplicationActivateWindowHelper.NormalizeTopMostWindows(AForm: TCustomForm);
begin
  if NeedNormalize(AForm) then
  begin
    NormalizeRegisteredWindows;
    NormalizeFocusedWindow;
  end;
end;

procedure TdxApplicationActivateWindowHelper.RemoveWindow(AForm: TCustomForm);
begin
  FFreeNotificator.RemoveSender(AForm);
  WindowsList.Remove(AForm);
end;

procedure TdxApplicationActivateWindowHelper.FreeNotificationHandler(Sender: TComponent);
begin
  RemoveWindow(TCustomForm(Sender));
end;

function TdxApplicationActivateWindowHelper.IsFormShowedBeforeLastModalWindow(AForm: TCustomForm): Boolean;
var
  I: Integer;
  AWindow: TCustomForm;
begin
  Result := True;
  for I := WindowsList.Count - 1 downto 0 do
  begin
    AWindow := TCustomForm(WindowsList[I]);
    Result := AWindow <> AForm;
    if not Result or (fsModal in AWindow.FormState) then
      Break;
  end;
end;

function TdxApplicationActivateWindowHelper.NeedNormalize(AForm: TCustomForm): Boolean;
begin
  Result := (Application.ModalLevel > 0) and (AForm <> Screen.FocusedForm) and IsFormShowedBeforeLastModalWindow(AForm);
end;

procedure TdxApplicationActivateWindowHelper.NormalizeFocusedWindow;
begin
  if (Screen.FocusedForm <> nil) and Screen.FocusedForm.HandleAllocated then
    SetWindowPos(Screen.FocusedForm.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TdxApplicationActivateWindowHelper.NormalizeRegisteredWindows;
var
  I: Integer;
  AForm: TCustomForm;
begin
  for I := WindowsList.Count - 1 downto 0 do
  begin
    AForm := TCustomForm(WindowsList[I]);
    if AForm.HandleAllocated then
      SetWindowPos(AForm.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
  end;
end;

{ TcxControlChildComponent }

constructor TcxControlChildComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := TdxOwnedScaleFactor.Create;
  FScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
  Initialize;
end;

constructor TcxControlChildComponent.CreateEx(AControl: TcxControl; AAssignOwner: Boolean = True);
begin
  //FControl := AControl;
  if AAssignOwner then
    Create(AControl.Owner)
  else
    Create(nil);
  Control := AControl;
end;

destructor TcxControlChildComponent.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveChildComponent(Self);
  FScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  FreeAndNil(FScaleFactor);
  inherited;
end;

procedure TcxControlChildComponent.Initialize;
begin
  // do nothing
end;

procedure TcxControlChildComponent.ScaleFactorChanged(M, D: Integer);
begin
  // do nothing
end;

procedure TcxControlChildComponent.SetControl(Value: TcxControl);
begin
  FControl := Value;
  if FControl <> nil then
  begin
    FScaleFactor.Owner := FControl.ScaleFactor;
    FScaleFactor.UseOwnerValue := True;
  end
  else
    FScaleFactor.Owner := nil;
end;

procedure TcxControlChildComponent.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value is TcxControl then
    TcxControl(Value).AddChildComponent(Self);
end;

function TcxControlChildComponent.GetParentComponent: TComponent;
begin
  Result := FControl;
end;

function TcxControlChildComponent.HasParent: Boolean;
begin
  Result := FControl <> nil;
end;

function TcxControlChildComponent.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxControlChildComponent.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

procedure TcxControlChildComponent.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ScaleFactorChanged(M, D);
end;

function TcxControlChildComponent.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

{ TcxScrollBarData }

procedure TcxScrollBarData.Validate;
begin
  FIsValid := (Max >= Min) and (Max - Min + 1 > PageSize) and (Position >= Min);
end;

{ TcxControlScrollBar }

constructor TcxControlScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clBtnFace;
  ControlStyle := ControlStyle - [csFramed] + [csNoDesignVisible];
  TabStop := False;
  Visible := False;
  FData := TcxScrollBarData.Create;
  ParentShowHint := False;
  ShowHint := True;
end;

constructor TcxControlScrollBar.CreateEx(AOwner: TObject);
begin
  OwnerControl := AOwner as TcxControl;
  Create(nil);
end;

destructor TcxControlScrollBar.Destroy;
begin
  FreeAndNil(FData);
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TcxControlScrollBar.ApplyData;
var
  AActuallyVisible: Boolean;
begin
  if FData.Visible and FData.IsValid then
  begin
    SetScrollParams(FData.Min, FData.Max, FData.Position, FData.PageSize, True);
    SmallChange := FData.SmallChange;
    LargeChange := FData.LargeChange;
  end;
  Enabled := FData.Enabled;
  AActuallyVisible := IsActuallyVisible;
  if Visible <> AActuallyVisible then
  begin
    Visible := AActuallyVisible;
    Perform(CM_SHOWINGCHANGED, 0, 0);
  end;
end;

function TcxControlScrollBar.GetBoundsRect: TRect;
begin
  Result := inherited BoundsRect;
end;

function TcxControlScrollBar.GetControl: TcxControlScrollBar;
begin
  Result := Self;
end;

function TcxControlScrollBar.GetData: TcxScrollBarData;
begin
  Result := FData;
end;

function TcxControlScrollBar.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TcxControlScrollBar.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TcxControlScrollBar.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TcxControlScrollBar.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

function TcxControlScrollBar.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TcxControlScrollBar.SetBoundsRect(const AValue: TRect);
begin
  inherited BoundsRect := AValue;
end;

procedure TcxControlScrollBar.SetHeight(AValue: Integer);
begin
  inherited Height := AValue;
end;

procedure TcxControlScrollBar.SetLeft(AValue: Integer);
begin
  inherited Left := AValue;
end;

procedure TcxControlScrollBar.SetTop(AValue: Integer);
begin
  inherited Top := AValue;
end;

procedure TcxControlScrollBar.SetVisible(AValue: Boolean);
begin
  inherited Visible := AValue;
end;

procedure TcxControlScrollBar.SetWidth(AValue: Integer);
begin
  inherited Width := AValue;
end;

procedure TcxControlScrollBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

procedure TcxControlScrollBar.CMHintShow(var Message: TCMHintShow);
begin
  Message.Result := 1;
end;

procedure TcxControlScrollBar.WndProc(var Message: TMessage);
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    DoWndProc(Message);
    if ALink.Ref <> nil then
      inherited;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxControlScrollBar.DoPaint(ACanvas: TcxCanvas);
var
  AIntf: IcxLockedStatePaint;
  ATopMostControl: TWinControl;
  P: TPoint;
begin
  if Supports(Parent, IcxLockedStatePaint, AIntf) and (AIntf.GetImage <> nil) then
  begin
    ATopMostControl := AIntf.GetTopmostControl;
    if ATopmostControl = Parent then
      P := BoundsRect.TopLeft
    else
      P := cxClientToParent(Self, cxNullPoint, ATopMostControl);
    cxBitBlt(ACanvas.Handle, AIntf.GetImage.cxCanvas.Handle, ClientRect, P, SRCCOPY);
  end
  else
    inherited DoPaint(ACanvas);
end;

procedure TcxControlScrollBar.DoWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONDOWN then
    FocusParent;
end;

procedure TcxControlScrollBar.FocusParent;
begin
  if OwnerControl <> nil then
  begin
    if OwnerControl.FocusWhenChildIsClicked(Self) then
      OwnerControl.SetFocus
  end
  else
    if Parent <> nil then
    begin
      if (Parent is TcxControl) and TcxControl(Parent).FocusWhenChildIsClicked(Self) or
        not (Parent is TcxControl) and Parent.CanFocus then
          Parent.SetFocus;
    end;
end;

procedure TcxControlScrollBar.InitControl;
begin
  Parent := OwnerControl;
end;

function TcxControlScrollBar.IsActuallyVisible: Boolean;
begin
  Result := FData.Visible;
end;

procedure TcxControlScrollBar.SetOwnerControlRelativeBounds(const ABounds: TRect);
begin
  BoundsRect := ABounds;
end;

procedure TcxControlScrollBar.SetZOrder(TopMost: Boolean);
begin
  if HandleAllocated then
    SetWindowPos(WindowHandle, HWND_TOP, 0, 0, 0, 0,
      SWP_NOMOVE + SWP_NOSIZE + SWP_DEFERERASE);
end;

{ TcxCustomPopupScrollBarHelper }

procedure TcxCustomPopupScrollBarHelper.InvalidateRect(const ARect: TRect; AEraseBackground: Boolean = False);
begin
  if HandleAllocated then
    (Owner.GetControl as TcxControlPopupScrollBar).UpdateLayer;
end;

function TcxCustomPopupScrollBarHelper.NeedBuffering: Boolean;
begin
  Result := False;
end;

{ TcxPopupScrollBarHelper }

function TcxPopupScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TcxPopupScrollBarPainter;
end;

function TcxPopupScrollBarHelper.GetScrollBarSize: TSize;
begin
  Result := GetTouchScrollBarSize(ScaleFactor);
end;

function TcxPopupScrollBarHelper.GetViewInfoClass: TcxScrollBarViewInfoClass;
begin
  Result := TcxPopupScrollBarViewInfo;
end;

{ TdxHybridScrollBarHelper }

function TdxHybridScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TdxHybridScrollBarPainter;
end;

function TdxHybridScrollBarHelper.GetScrollBarSize: TSize;
begin
  Result := GetHybridScrollBarSize(ScaleFactor, True);
end;

{ TcxPopupScrollBarPainter }

function TcxPopupScrollBarPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

procedure TcxPopupScrollBarPainter.DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
var
  AThumbBounds: TRect;
begin
  if APart = sbpThumbnail then
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    if ScrollBar.Kind = sbHorizontal then
      AThumbBounds := cxRectInflate(R, 0, -cxRectHeight(R) div 3)
    else
      AThumbBounds := cxRectInflate(R, -cxRectWidth(R) div 3, 0);
    dxGPPaintCanvas.Rectangle(AThumbBounds, clWhite, clDkGray, 1, psSolid, 200, 150);
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TcxPopupScrollBarPainter.DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  dxGPPaintCanvas.FillRectangle(R, dxColorToAlphaColor(clLtGray, IfThen(ViewInfo.HotPart <> sbpNone, 50, 1)));
  dxGPPaintCanvas.EndPaint;
end;

function TcxPopupScrollBarPainter.FadingAvailable: Boolean;
begin
  Result := False;
end;

{ TdxHybridScrollBarPainter }

procedure TdxHybridScrollBarPainter.DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart;
  AState: TcxButtonState);
begin
  if APart = sbpThumbnail then
    DrawThumb(ACanvas, R, APart, AState)
  else
    if (APart in [sbpLineUp, sbpLineDown]) and IsActive then
      DrawArrowButton(ACanvas, R, APart, AState);
end;

procedure TdxHybridScrollBarPainter.DrawArrowButton(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart;
  AState: TcxButtonState);

  function GetArrowDirection(AHorizontal: Boolean;
    APart: TcxScrollBarPart): TcxArrowDirection;
  const
    ArrowKind: array[Boolean, Boolean] of TcxArrowDirection = ((adUp, adDown), (adLeft, adRight));
  begin
    Result := ArrowKind[AHorizontal, APart <> sbpLineUp];
    if IsRightToLeft then
      if Result = adLeft then
        Result := adRight
      else
        if Result = adRight then
          Result := adLeft;
  end;

const
  ArrowAlpha = $90;
  ArrowButtonAlpha = 0;
  ArrowButtonHotAlpha = $2D;
  ArrowButtonPressedAlpha = $A2;
var
  AAlpha: Byte;
  P: TcxArrowPoints;
  AColor: TColor;
  AArrowColor: TColor;
begin
  AColor := GetBaseColor;
  cxLookAndFeelPaintersManager.GetPainter(lfsStandard).CalculateArrowPoints(
    R, P, GetArrowDirection(ScrollBar.Kind = sbHorizontal, APart), False);
  if ViewInfo.PressedPart = APart then
  begin
    AAlpha := ArrowButtonPressedAlpha;
    AArrowColor := dxInvertColor(AColor);
  end
  else
  begin
    AArrowColor := AColor;
    if ViewInfo.HotPart = APart then
      AAlpha := ArrowButtonHotAlpha
    else
      AAlpha := ArrowButtonAlpha;
  end;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    dxGPPaintCanvas.FillRectangle(R, dxColorToAlphaColor(AColor, AAlpha));
    dxGPPaintCanvas.SmoothingMode := smAntiAlias;
    dxGPPaintCanvas.Polyline(P, dxColorToAlphaColor(AArrowColor, ArrowAlpha));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxHybridScrollBarPainter.DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect);

  function GetActiveScrollBarBackgroundAlpha: Byte;
  var
    AAlpha: Integer;
  begin
    AAlpha := GetHybridScrollbarOwner.GetManager.GetVisibleSize;
    Result := Round($16 * AAlpha / 100);
  end;

var
  ARect: TRect;
  AAlpha: Integer;
begin
  ARect := R;
  if IsActive then
    AAlpha := GetActiveScrollBarBackgroundAlpha
  else
  begin
    if ScrollBar.Kind = sbHorizontal then
      ARect.Top := ARect.Bottom - GetHybridScrollBarSize(ScaleFactor, False).cy
    else
    begin
      ARect.Left := ARect.Right - GetHybridScrollBarSize(ScaleFactor, False).cx;
      if IsRightToLeft then
        ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, R);
    end;
    AAlpha := 1;
  end;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ARect);
  try
    dxGPPaintCanvas.FillRectangle(ARect, dxColorToAlphaColor(GetBaseColor, AAlpha));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxHybridScrollBarPainter.DrawThumb(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
const
  ThumbAlpha = $45;
  ThumbHotAlpha = $73;
  ThumbPressedAlpha = $A2;
  ThinThumbAlpha = $85;
var
  AThumbBounds: TRect;
  AAlpha: Byte;
  AThinThumbSize: Integer;
  AThinThumbOffset: Integer;
begin
  AThumbBounds := R;
  if ViewInfo.PressedPart = sbpThumbnail then
    AAlpha := ThumbPressedAlpha
  else
  begin
    if ViewInfo.HotPart = sbpThumbnail then
      AAlpha := ThumbHotAlpha
    else
      AAlpha := ThumbAlpha;
  end;
  if ScrollBar.Kind = sbHorizontal then
    AThumbBounds.Top := AThumbBounds.Bottom - GetVisibleSize
  else
  begin
    AThumbBounds.Left := AThumbBounds.Right - GetVisibleSize;
    if IsRightToLeft then
      AThumbBounds := TdxRightToLeftLayoutConverter.ConvertRect(AThumbBounds, R);
  end;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
  try
    if IsActive then
      dxGPPaintCanvas.FillRectangle(AThumbBounds, dxColorToAlphaColor(GetBaseColor, AAlpha))
    else
    begin
      dxGPPaintCanvas.FillRectangle(AThumbBounds, dxColorToAlphaColor(clWhite, 1));
      AThumbBounds := R;
      if ScrollBar.Kind = sbHorizontal then
      begin
        AThinThumbSize := GetHybridScrollBarSize(ScaleFactor, False).cy div 3;
        AThinThumbOffset := AThinThumbSize;
        AThumbBounds.Bottom := AThumbBounds.Bottom - AThinThumbOffset;
        AThumbBounds.Top := AThumbBounds.Bottom - AThinThumbSize;
      end
      else
      begin
        AThinThumbSize := GetHybridScrollBarSize(ScaleFactor, False).cx div 3;
        AThinThumbOffset := AThinThumbSize;
        AThumbBounds.Right := AThumbBounds.Right - AThinThumbOffset;
        AThumbBounds.Left := AThumbBounds.Right - AThinThumbSize;
        if IsRightToLeft then
          AThumbBounds := TdxRightToLeftLayoutConverter.ConvertRect(AThumbBounds, R);
      end;
      dxGPPaintCanvas.FillRectangle(AThumbBounds, dxColorToAlphaColor(GetBaseColor, ThinThumbAlpha));
    end;
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TdxHybridScrollBarPainter.FadingAvailable: Boolean;
begin
  Result := False;
end;

function TdxHybridScrollBarPainter.GetBaseColor: TColor;
begin
  Result := GetHybridScrollbarOwner.GetBaseColor;
end;

function TdxHybridScrollBarPainter.GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  Result := (TdxHybridScrollBarHelper(ScrollBar).Owner.GetControl as TcxControlPopupScrollBar).GetHybridScrollbarOwner;
end;

function TdxHybridScrollBarPainter.GetVisibleSize: Integer;
begin
  Result := (TdxHybridScrollBarHelper(ScrollBar).Owner.GetControl as TcxControlPopupScrollBar).VisibleSize;
end;

function TdxHybridScrollBarPainter.IsActive: Boolean;
begin
  Result := GetHybridScrollbarOwner.GetManager.IsActive(TdxHybridScrollBarHelper(ScrollBar).Owner.GetControl)
end;

function TdxHybridScrollBarPainter.IsRightToLeft: Boolean;
begin
  Result := (ScrollBar as TdxHybridScrollBarHelper).IsRightToLeft;
end;

function TdxHybridScrollBarPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

{ TcxPopupScrollBarViewInfo }

function TcxPopupScrollBarViewInfo.GetArrowButtonLength: Integer;
begin
  Result := 0;
end;

function CalculatePopupScrollElementVisibleRect(AOwnerControl: TcxControl; const ABounds: TRect): TRect;
var
  ADC: HDC;
  R: TRect;
begin
  R := dxMapWindowRect(AOwnerControl.Handle, 0, ABounds);
  ADC := GetDCEx(AOwnerControl.Handle, CreateRectRgnIndirect(R), DCX_WINDOW or DCX_PARENTCLIP or DCX_CLIPSIBLINGS or DCX_INTERSECTRGN);
  GetClipBox(ADC, Result);
  ReleaseDC(AOwnerControl.Handle, ADC);
end;

function CalculatePopupScrollElementVisibleRgn(AOwnerControl: TcxControl; const ABounds: TRect): HRGN;
var
  ADC: HDC;
  R: TRect;
begin
  R := dxMapWindowRect(AOwnerControl.Handle, 0, ABounds);
  ADC := GetDCEx(AOwnerControl.Handle, CreateRectRgnIndirect(R), DCX_WINDOW or DCX_PARENTCLIP or DCX_CLIPSIBLINGS or DCX_INTERSECTRGN);
  Result := CreateRectRgnIndirect(cxNullRect);
  GetRandomRgn(ADC, Result, SYSRGN);
  ReleaseDC(AOwnerControl.Handle, ADC);
  OffsetRgn(Result, -R.Left, -R.Top);
end;

procedure UpdatePopupScrollElementZOrder(AElementHandle: HWND; AOwnerControl: TcxControl);

  function IsOwnerControlTopMost: Boolean;
  var
    AParentForm: TCustomForm;
  begin
    AParentForm := GetParentForm(AOwnerControl);
    Result := (AParentForm = nil) or (GetWindowLong(AParentForm.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0);
  end;

  procedure UpdateZOrder(AWndAfter: THandle);
  begin
    dxSetZOrder(AElementHandle, AWndAfter, False, SWP_NOOWNERZORDER);
  end;

var
  AOwner, APrevWnd: HWND;
begin
  if IsOwnerControlTopMost then
    UpdateZOrder(HWND_TOPMOST)
  else
  begin
    AOwner := GetParent(AElementHandle);
    APrevWnd := GetNextWindow(AOwner, GW_HWNDPREV);
    if APrevWnd <> 0 then
      UpdateZOrder(APrevWnd)
  end;
end;

{ TcxControlPopupScrollBar }

constructor TcxControlPopupScrollBar.CreateEx(AOwner: TObject);
begin
  FAlpha := 255;
  FScrollbarOwner := AOwner;
  FActivityHelper := TdxTouchScrollUIActivityHelper.Create;
  inherited CreateEx(GetScrollbarOwner.GetOwnerControl);
  ScaleFactor.UseOwnerValue := True;
  ScaleFactor.Owner := OwnerControl.ScaleFactor;
end;

destructor TcxControlPopupScrollBar.Destroy;
begin
  FreeAndNil(FActivityHelper);
  inherited;
end;

procedure TcxControlPopupScrollBar.ApplyData;
begin
  FAllowVisible := GetData.Visible;
  inherited ApplyData;
end;

function TcxControlPopupScrollBar.GetOwnerControl: TWinControl;
begin
  Result := OwnerControl;
end;

procedure TcxControlPopupScrollBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := WS_EX_LAYERED;
end;

procedure TcxControlPopupScrollBar.DestroyWnd;
begin
  (Helper as TcxCustomPopupScrollBarHelper).VisibleRgn := 0;
  inherited DestroyWnd;
end;

function TcxControlPopupScrollBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := (OwnerControl <> nil) and OwnerControl.DoMouseWheel(Shift,
    WheelDelta, MousePos);
  if not Result then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TcxControlPopupScrollBar.DoPaint(ACanvas: TcxCanvas);
begin
  Helper.Paint(ACanvas);
end;

function TcxControlPopupScrollBar.GetInitialSize: TSize;
begin
  if OwnerControl.GetScrollbarMode = sbmTouch then
    Result := GetTouchScrollBarSize(ScaleFactor)
  else // sbmHybrid
    Result := GetHybridScrollBarSize(ScaleFactor, True);
end;

function TcxControlPopupScrollBar.GetMaxVisibleSize: Integer;
begin
  if Kind = sbVertical then
    Result := GetHybridScrollBarSize(ScaleFactor, True).cx
  else
    Result := GetHybridScrollBarSize(ScaleFactor, True).cy;
end;

function TcxControlPopupScrollBar.GetMinVisibleSize: Integer;
begin
  if Kind = sbVertical then
    Result := GetHybridScrollBarSize(ScaleFactor, False).cx
  else
    Result := GetHybridScrollBarSize(ScaleFactor, False).cy;
end;

function TcxControlPopupScrollBar.GetHelperClass: TcxScrollBarHelperClass;
begin
  if OwnerControl.GetScrollbarMode = sbmTouch then
    Result := TcxPopupScrollBarHelper
  else // sbmHybrid
    Result := TdxHybridScrollBarHelper;
end;

function TcxControlPopupScrollBar.GetScrollbarOwner: IdxTouchScrollUIOwner;
begin
  Supports(FScrollBarOwner, IdxTouchScrollUIOwner, Result);
end;

function TcxControlPopupScrollBar.GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  Supports(FScrollBarOwner, IdxHybridScrollbarOwner, Result);
end;

procedure TcxControlPopupScrollBar.InitControl;
var
  AOwnerWnd: HWND;
begin
  AOwnerWnd := GetTouchScrollUIControlOwnerWnd(OwnerControl);
  if AOwnerWnd <> ParentWindow then
  begin
    ParentWindow := AOwnerWnd;
    RecreateWnd;
  end;
  BiDiMode := OwnerControl.BiDiMode;
end;

procedure TcxControlPopupScrollBar.InternalSetOwnerControlRelativeBounds(const ABounds, AVisibleBounds: TRect);
var
  ARgn, ARgn1: HRGN;
begin
  if OwnerControl.HandleAllocated then
  begin
    BoundsRect := dxMapWindowRect(OwnerControl.Handle, 0, ABounds);
    ARgn1 := CreateRectRgnIndirect(AVisibleBounds);
    OffsetRgn(ARgn1, -ABounds.Left, -ABounds.Top);
    ARgn := CalculatePopupScrollElementVisibleRgn(OwnerControl, ABounds);
    CombineRgn(ARgn, ARgn, ARgn1, RGN_AND);
    DeleteObject(ARgn1);
    (Helper as TcxCustomPopupScrollBarHelper).VisibleRgn := ARgn;
    Invalidate;
  end;
end;

function TcxControlPopupScrollBar.IsActuallyVisible: Boolean;
begin
  Result := inherited IsActuallyVisible and not OwnerControl.IsTouchScrollUIHidden(GetScrollbarOwner);
end;

procedure TcxControlPopupScrollBar.Invalidate;
begin
  if Visible then
  begin
    if OwnerControl.GetScrollbarMode = sbmHybrid then
    begin
      FVisibleSize := GetMinVisibleSize +
        Round(GetHybridScrollbarOwner.GetManager.GetVisibleSize * (GetMaxVisibleSize - GetMinVisibleSize)/ 100);
      FAlpha := GetHybridScrollbarOwner.GetManager.GetFadingValue;
    end;
    UpdateLayer;
  end;
end;

procedure TcxControlPopupScrollBar.SetOwnerControlRelativeBounds(const ABounds: TRect);
var
  ARgn: HRGN;
begin
  if OwnerControl.HandleAllocated then
  begin
    BoundsRect := dxMapWindowRect(OwnerControl.Handle, 0, ABounds);
    ARgn := CalculatePopupScrollElementVisibleRgn(OwnerControl, ABounds);
    (Helper as TcxCustomPopupScrollBarHelper).VisibleRgn := ARgn;
    Invalidate;
  end;
end;

procedure TcxControlPopupScrollBar.Resize;
begin
  inherited Resize;
  if Visible then
    UpdateLayer;
end;

procedure TcxControlPopupScrollBar.SetVisible(AValue: Boolean);
begin
  if (OwnerControl.GetScrollbarMode = sbmHybrid) and Visible and not AValue then
    GetHybridScrollbarOwner.GetManager.ScrollbarHiding(Self);
  inherited SetVisible(AValue);
  if Visible then
  begin
    UpdateLayer;
    UpdatePopupScrollElementZOrder(WindowHandle, OwnerControl);
  end;
end;

procedure TcxControlPopupScrollBar.UpdateLayer;
var
  ABitmap: TcxBitmap32;
begin
  if HandleAllocated and Visible then
  begin
    ABitmap := TcxBitmap32.CreateSize(ClientRect, True);
    try
      DoPaint(ABitmap.cxCanvas);
      cxUpdateLayeredWindow(Handle, ABitmap, FAlpha);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TcxControlPopupScrollBar.DoWndProc(var Message: TMessage);
begin
  inherited;
  if OwnerControl.GetScrollbarMode = sbmTouch then
    if FActivityHelper.CheckScrollActivity(Self, Message) then
      OwnerControl.ShowTouchScrollUI(GetScrollbarOwner, True)
    else
      case Message.Msg of
        WM_LBUTTONDOWN:
          begin
            OwnerControl.ShowTouchScrollUI(GetScrollbarOwner);
            TdxTouchScrollUIModeManager.LockOwner;
          end;
        WM_LBUTTONUP:
          begin
            TdxTouchScrollUIModeManager.UnlockOwner;
            OwnerControl.ShowTouchScrollUI(GetScrollbarOwner, True);
          end;
        WM_LBUTTONDBLCLK..WM_MOUSELAST:
          OwnerControl.ShowTouchScrollUI(GetScrollbarOwner, True);
        WM_CAPTURECHANGED, WM_CANCELMODE:
          OwnerControl.HideTouchScrollUI(GetScrollbarOwner);
        WM_ACTIVATEAPP:
          if not TWMActivateApp(Message).Active then
            OwnerControl.HideTouchScrollUI(GetScrollbarOwner, True);
      end
  else // sbmHybrid
    case Message.Msg of
      WM_MOUSEMOVE:
        begin
          if not FMouseEnter then
          begin
            FMouseEnter := True;
            GetHybridScrollbarOwner.GetManager.Expand(Self);
          end;
        end;
      CM_MOUSELEAVE:
        begin
          FMouseEnter := False;
          GetHybridScrollbarOwner.GetManager.Collapse;
        end;
     WM_CANCELMODE:
        OwnerControl.HideTouchScrollUI(GetScrollbarOwner);
      WM_ACTIVATEAPP:
        if not TWMActivateApp(Message).Active then
          OwnerControl.HideTouchScrollUI(GetScrollbarOwner, True);
    end;
end;

procedure TcxControlPopupScrollBar.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATE;
end;

{ TcxControlScrollBarHelper }

constructor TcxControlScrollBarHelper.Create(AOwner: IcxScrollBarOwner);
begin
  inherited;
  FData := TcxScrollBarData.Create;
end;

destructor TcxControlScrollBarHelper.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TcxControlScrollBarHelper.Repaint;
var
  ADC: HDC;
  AIntf: IcxLockedStatePaint;
begin
  if not Supports(Owner.GetControl, IcxLockedStatePaint, AIntf) or (AIntf.GetImage = nil) then
  begin
    if not IsNonClient then
    begin
      ADC := GetDC(Handle);
      try
        Paint(ADC);
      finally
        ReleaseDC(Handle, ADC);
      end;
    end
    else
      inherited;
  end;
end;

procedure TcxControlScrollBarHelper.ApplyData;
begin
  if FData.Visible and FData.IsValid then
  begin
    SetScrollParams(FData.Min, FData.Max, FData.Position, FData.PageSize, Visible);
    SmallChange := FData.SmallChange;
    LargeChange := FData.LargeChange;
  end;
  Enabled := FData.Enabled;
  if Visible <> FData.Visible then
    Visible := FData.Visible;
end;

function TcxControlScrollBarHelper.GetControl: TcxControlScrollBar;
begin
  Result := nil;
end;

function TcxControlScrollBarHelper.GetData: TcxScrollBarData;
begin
  Result := FData;
end;

function TcxControlScrollBarHelper.GetHeight: Integer;
begin
  Result := cxRectHeight(BoundsRect);
end;

function TcxControlScrollBarHelper.GetLeft: Integer;
begin
  Result := BoundsRect.Left;
end;

function TcxControlScrollBarHelper.GetTop: Integer;
begin
  Result := BoundsRect.Top;
end;

function TcxControlScrollBarHelper.GetWidth: Integer;
begin
  Result := cxRectWidth(BoundsRect);
end;

procedure TcxControlScrollBarHelper.SetHeight(Value: Integer);
begin
  BoundsRect := cxRectSetHeight(BoundsRect, Value);
end;

procedure TcxControlScrollBarHelper.SetLeft(Value: Integer);
begin
  BoundsRect := cxRectSetLeft(BoundsRect, Value);
end;

procedure TcxControlScrollBarHelper.SetTop(Value: Integer);
begin
  BoundsRect := cxRectSetTop(BoundsRect, Value);
end;

procedure TcxControlScrollBarHelper.SetWidth(Value: Integer);
begin
  BoundsRect := cxRectSetWidth(BoundsRect, Value);
end;

{ TcxSizeGrip }

constructor TcxSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  Color := clBtnFace;
  ControlStyle := ControlStyle + [csNoDesignVisible];
  TabStop := False;
  Visible := False;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
end;

constructor TcxSizeGrip.CreateEx(AOwner: TObject);
begin
  Create(nil);
  FOwnerControl := AOwner as TcxControl;
end;

destructor TcxSizeGrip.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

class function TcxSizeGrip.IsAvailable(AStatusBar: TWinControl): Boolean;
var
  AParentForm: TCustomForm;
  APoint: TPoint;
begin
  Result := False;
  if AStatusBar <> nil then
  begin
    AParentForm := GetParentForm(AStatusBar);
    if (AParentForm <> nil) and (GetWindowLong(AParentForm.Handle, GWL_STYLE) and WS_THICKFRAME = WS_THICKFRAME) then
    begin
      if not (IsZoomed(AParentForm.Handle) or IsIconic(AParentForm.Handle)) then
      begin
        APoint := AStatusBar.ClientToParent(Point(AStatusBar.Width, AStatusBar.Height), AParentForm);
        Result := (APoint.X = AParentForm.ClientWidth) and (APoint.Y >= AParentForm.ClientHeight);
      end;
    end;
  end;
end;

procedure TcxSizeGrip.Draw(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := LookAndFeel.Painter.DefaultSizeGripAreaColor;
  ACanvas.FillRect(ClientRect);
end;

procedure TcxSizeGrip.InitControl;
begin
  Parent := OwnerControl;
end;

procedure TcxSizeGrip.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  Invalidate;
end;

procedure TcxSizeGrip.Paint;
var
  AIntf: IcxLockedStatePaint;
  ATopMostControl: TWinControl;
  P: TPoint;
begin
  if Supports(Parent, IcxLockedStatePaint, AIntf) and (AIntf.GetImage <> nil) then
  begin
    ATopMostControl := AIntf.GetTopmostControl;
    if ATopmostControl = Parent then
      P := BoundsRect.TopLeft
    else
      P := cxClientToParent(Self, cxNullPoint, ATopMostControl);
    cxBitBlt(Canvas.Handle, AIntf.GetImage.cxCanvas.Handle, ClientRect, P, SRCCOPY);
  end
  else
    Draw(Canvas);
end;

procedure TcxSizeGrip.SetOwnerControlRelativeBounds(const ABounds: TRect);
begin
  BoundsRect := ABounds;
end;

procedure TcxSizeGrip.SetZOrder(TopMost: Boolean);
begin
  if HandleAllocated then
    SetWindowPos(WindowHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_DEFERERASE);
end;

procedure TcxSizeGrip.WMEraseBkgnd(var AMessage: TWMEraseBkgnd);
begin
  AMessage.Result := 1;
end;

{ TcxPopupSizeGrip }

constructor TcxPopupSizeGrip.CreateEx(AOwner: TObject);
begin
  FScrollbarOwner := AOwner;
  inherited CreateEx(GetScrollbarOwner.GetOwnerControl);
end;

procedure TcxPopupSizeGrip.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := WS_EX_LAYERED;
end;

procedure TcxPopupSizeGrip.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  SetLayeredWindowAttributes(Handle, 0, 50, LWA_ALPHA);
end;

procedure TcxPopupSizeGrip.Draw(ACanvas: TCanvas);
var
  AAlpha: Integer;
  AColor: TColor;
begin
  if OwnerControl.GetScrollbarMode = sbmHybrid then
  begin
    AColor := GetHybridScrollbarOwner.GetBaseColor;
    AAlpha := GetHybridScrollbarOwner.GetManager.GetVisibleSize;
    AAlpha := Round($16 * AAlpha / 100);
    SetLayeredWindowAttributes(Handle, 0, AAlpha, LWA_ALPHA);
  end
  else
    AColor := clLtGray;
  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(ClientRect);
end;

function TcxPopupSizeGrip.GetHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  Supports(FScrollBarOwner, IdxHybridScrollbarOwner, Result);
end;

function TcxPopupSizeGrip.GetScrollbarOwner: IdxTouchScrollUIOwner;
begin
  Supports(FScrollBarOwner, IdxTouchScrollUIOwner, Result);
end;

procedure TcxPopupSizeGrip.InitControl;
var
  AOwnerWnd: HWND;
begin
  AOwnerWnd := GetTouchScrollUIControlOwnerWnd(OwnerControl);
  if AOwnerWnd <> ParentWindow then
  begin
    ParentWindow := AOwnerWnd;
    RecreateWnd;
  end;
end;

procedure TcxPopupSizeGrip.SetOwnerControlRelativeBounds(const ABounds: TRect);
var
  R: TRect;
begin
  if OwnerControl.HandleAllocated then
  begin
    R := CalculatePopupScrollElementVisibleRect(OwnerControl, ABounds);
    BoundsRect := dxMapWindowRect(OwnerControl.Handle, 0, R, False);
  end;
end;

procedure TcxPopupSizeGrip.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATE;
end;

{ TcxSizeGripHelper }

constructor TcxSizeGripHelper.Create(AOwner: IcxScrollBarOwner);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TcxSizeGripHelper.Paint(ACanvas: TcxCanvas);
begin
  ACanvas.Brush.Color := FOwner.GetLookAndFeel.Painter.DefaultSizeGripAreaColor;
  ACanvas.FillRect(FBoundsRect);
end;

procedure TcxSizeGripHelper.Repaint;
begin
  if FOwner.GetControl.HandleAllocated then
    if IsNonClient then
      RefreshNCPart(FBoundsRect)
    else
      cxInvalidateRect(FOwner.GetControl.Handle, FBoundsRect, False);
end;

procedure TcxSizeGripHelper.RefreshNCPart(const ARect: TRect);
var
  ANCOrigin: TPoint;
begin
  ANCOrigin := FOwner.GetControl.ScreenToClient(cxGetWindowRect(FOwner.GetControl.Handle).TopLeft);
  cxRedrawWindow(FOwner.GetControl.Handle, cxRectOffset(ARect, ANCOrigin), RDW_INVALIDATE or RDW_FRAME);
end;

{ TcxControlScrollBarsManager }

constructor TcxControlScrollBarsManager.Create(AOwner: TcxControl);
begin
  inherited Create;
  FOwner := AOwner;
  FScrollBars := TList.Create;
end;

destructor TcxControlScrollBarsManager.Destroy;
begin
  FreeAndNil(FCheckCaptureTimer);
  FreeAndNil(FScrollBars);
  inherited;
end;

function TcxControlScrollBarsManager.HandleMessage(var Message: TMessage): Boolean;

  function GetMousePos: TPoint;
  begin
    if FOwner.HandleAllocated and ((FOwner.Width > 32768) or (FOwner.Height > 32768)) then
      Result := FOwner.ScreenToClient(GetMouseCursorPos)
    else
      Result := SmallPointToPoint(TWMMouse(Message).Pos);
  end;

  function GetMouseButton: TMouseButton;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        Result := mbLeft;
      WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
        Result := mbRight;
    else
      Result := mbMiddle;
    end;
  end;

  function ShiftState: TShiftState;
  begin
    Result := KeysToShiftState(TWMMouse(Message).Keys);
  end;

begin
  Result := False;
  case Message.Msg of
    WM_MOUSEMOVE:
      begin
        with GetMousePos do
          Result := ProcessMouseMove(ShiftState, X, Y);
        if Result then
          Application.CancelHint;
      end;
    WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK,
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
      begin
        with GetMousePos do
          Result := ProcessMouseDown(GetMouseButton, ShiftState, X, Y);
      end;
    WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
      begin
        with GetMousePos do
          Result := ProcessMouseUp(GetMouseButton, ShiftState, X, Y);
      end;
    WM_CAPTURECHANGED:
      begin
        FIsCapture := False;
        if HotScrollBar <> nil then
          HotScrollBar.CancelMode;
      end;
    WM_DESTROY:
      begin
        if FIsCapture then
        begin
          FreeAndNil(FCheckCaptureTimer);
          FCheckCaptureTimer := TcxTimer.Create(nil);
          FCheckCaptureTimer.Interval := 100;
          FCheckCaptureTimer.OnTimer := CheckCaptureTimerHandler;
        end;
      end;
    WM_CREATE:
      begin
        if FIsCapture then
        begin
          FIsCapture := False;
          if (GetCapture = 0) and IsMouseButtonDown then
            IsCapture := True;
        end;
     end;
  end;
end;

function TcxControlScrollBarsManager.IsScrollBarsArea(const APoint: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FScrollBars.Count - 1 do
    if ScrollBars[I].Visible and PtInRect(ScrollBars[I].BoundsRect, APoint) then
    begin
      Result := True;
      Break;
    end;
end;

procedure TcxControlScrollBarsManager.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FScrollBars.Count - 1 do
    if ScrollBars[I].Visible and ACanvas.RectVisible(ScrollBars[I].BoundsRect) then
      ScrollBars[I].Paint(ACanvas);
end;

procedure TcxControlScrollBarsManager.RegisterScrollBar(
  AScrollBar: TcxControlScrollBarHelper);
begin
  FScrollBars.Add(AScrollBar);
end;

procedure TcxControlScrollBarsManager.UnRegisterScrollBar(
  AScrollBar: TcxControlScrollBarHelper);
begin
  if AScrollBar = FHotScrollBar then
    HotScrollBar := nil;
  FScrollBars.Remove(AScrollBar);
end;

function TcxControlScrollBarsManager.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if FOwner.IsSizeGripArea(Point(X, Y)) then
    Result := True
  else
  begin
    Result := FHotScrollBar <> nil;
    if Result and (Button = mbLeft) and FHotScrollBar.Enabled then
    begin
      IsCapture := True;
      if FOwner.CanFocusOnClick(X, Y) then
      begin
        FOwner.ActivateType := atByMouseNonClientArea;
        FOwner.SetFocus;
      end;
      if FHotScrollBar <> nil then
        FHotScrollBar.MouseDown(Button, Shift, X, Y)
      else
        IsCapture := False;
    end;
  end;
end;

function TcxControlScrollBarsManager.ProcessMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;

  function GetScrollBarAtPos: TcxControlScrollBarHelper;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to FScrollBars.Count - 1 do
      if ScrollBars[I].Visible and ScrollBars[I].Enabled and ScrollBars[I].PtInVisibleRgn(Point(X, Y)) then
        Result := ScrollBars[I];
  end;

begin
  Result := False;
  if (GetCapture = FOwner.Handle) and not FIsCapture and not FOwner.AllowScrollContentOnDrag then
    Exit;
  if not FIsCapture then
    HotScrollBar := GetScrollBarAtPos;

  if HotScrollBar <> nil then
  begin
    HotScrollBar.MouseMove(Shift, X, Y);
    Result := True;
  end
  else
    if FOwner.IsSizeGripArea(Point(X, Y)) then
      Result := True;
end;

function TcxControlScrollBarsManager.ProcessMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if FIsCapture then
  begin
    Result := True;
    try
      FHotScrollBar.MouseUp(Button, Shift, X, Y);
    finally
      IsCapture := False;
    end;
  end;
end;

procedure TcxControlScrollBarsManager.MouseLeave;
begin
  HotScrollBar.CancelMode;
  HotScrollBar := nil;
end;

function TcxControlScrollBarsManager.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(FHotScrollBar.BoundsRect, P);
end;

function TcxControlScrollBarsManager.IsCaptureMouse: Boolean;
begin
  Result := FIsCapture;
end;

procedure TcxControlScrollBarsManager.CheckCaptureTimerHandler(ASender: TObject);
begin
  if not IsMouseButtonDown then
  begin
    FCheckCaptureTimer.Enabled := False;
    if HotScrollBar <> nil then
      HotScrollBar.CancelMode;
    IsCapture := False;
  end;
end;

function TcxControlScrollBarsManager.GetScrollBars(AIndex: Integer): TcxControlScrollBarHelper;
begin
  Result := TcxControlScrollBarHelper(FScrollBars[AIndex]);
end;

function TcxControlScrollBarsManager.IsMouseButtonDown: Boolean;
begin
  Result := GetAsyncKeyState(VK_LBUTTON) < 0;
end;

procedure TcxControlScrollBarsManager.SetHotScrollBar(
  AValue: TcxControlScrollBarHelper);
begin
  if FHotScrollBar <> AValue then
  begin
    if FHotScrollBar <> nil then
    begin
      FHotScrollBar.MouseLeave(nil);
      EndMouseTracking(Self);
    end;
    FHotScrollBar := AValue;
    if FHotScrollBar <> nil then
    begin
      FHotScrollBar.MouseEnter(nil);
      BeginMouseTracking(FOwner, FHotScrollBar.BoundsRect, Self);
    end;
  end;
end;

procedure TcxControlScrollBarsManager.SetIsCapture(AValue: Boolean);
begin
  if AValue <> FIsCapture then
  begin
    FIsCapture := AValue;
    if FIsCapture then
      cxSetCapture(FOwner.Handle)
    else
    begin
      FreeAndNil(FCheckCaptureTimer);
      ReleaseCapture;
    end;
  end;
end;

{ TdxScrollBarWrapper }

constructor TdxScrollBarWrapper.Create(AOwner: IdxTouchScrollUIOwner);
begin
  inherited Create;
  FOwnerControl := AOwner.GetOwnerControl;
  FTouchScrollUIOwner := AOwner as TObject;
  CreateInnerScrollBar;
end;

destructor TdxScrollBarWrapper.Destroy;
begin
  DestroyInnerScrollBar;
  inherited Destroy;
end;

procedure TdxScrollBarWrapper.ApplyData;
begin
  InnerScrollBar.ApplyData;
end;

procedure TdxScrollBarWrapper.SetBoundsRect(const AValue: TRect);
begin
  SetOwnerControlRelativeBounds(AValue);
end;

procedure TdxScrollBarWrapper.SetEnabled(AValue: Boolean);
begin
  InnerScrollBar.SetEnabled(AValue);
end;

procedure TdxScrollBarWrapper.SetHeight(AValue: Integer);
begin
  InnerScrollBar.SetHeight(AValue);
end;

procedure TdxScrollBarWrapper.SetKind(AValue: TScrollBarKind);
begin
  InnerScrollBar.SetKind(AValue);
end;

procedure TdxScrollBarWrapper.SetLargeChange(AValue: TScrollBarInc);
begin
  InnerScrollBar.SetLargeChange(AValue);
end;

procedure TdxScrollBarWrapper.SetLeft(AValue: Integer);
begin
  InnerScrollBar.SetLeft(AValue);
end;

procedure TdxScrollBarWrapper.SetMax(AValue: Integer);
begin
  InnerScrollBar.SetMax(AValue);
end;

procedure TdxScrollBarWrapper.SetMin(AValue: Integer);
begin
  InnerScrollBar.SetMin(AValue);
end;

function TdxScrollBarWrapper.GetBoundsRect: TRect;
var
  R: TRect;
begin
  R := InnerScrollBar.BoundsRect;
  if FIsTouchScrollUIMode and FOwnerControl.HandleAllocated then
    R := dxMapWindowRect(0, FOwnerControl.Handle, R);
  Result := R;
end;

function TdxScrollBarWrapper.GetControl: TcxControlScrollBar;
begin
  Result := InnerScrollBar.GetControl;
end;

function TdxScrollBarWrapper.GetData: TcxScrollBarData;
begin
  Result := InnerScrollBar.GetData;
end;

function TdxScrollBarWrapper.GetEnabled: Boolean;
begin
  Result := InnerScrollBar.GetEnabled;
end;

function TdxScrollBarWrapper.GetHeight: Integer;
begin
  Result := InnerScrollBar.GetHeight;
end;

function TdxScrollBarWrapper.GetKind: TScrollBarKind;
begin
  Result := InnerScrollBar.GetKind;
end;

function TdxScrollBarWrapper.GetLargeChange: TScrollBarInc;
begin
  Result := InnerScrollBar.GetLargeChange;
end;

function TdxScrollBarWrapper.GetLeft: Integer;
begin
  Result := InnerScrollBar.GetLeft;
end;

function TdxScrollBarWrapper.GetMax: Integer;
begin
  Result := InnerScrollBar.GetMax;
end;

function TdxScrollBarWrapper.GetMin: Integer;
begin
  Result := InnerScrollBar.GetMin;
end;

function TdxScrollBarWrapper.GetPageSize: Integer;
begin
  Result := InnerScrollBar.GetPageSize;
end;

function TdxScrollBarWrapper.GetPosition: Integer;
begin
  Result := InnerScrollBar.GetPosition;
end;

function TdxScrollBarWrapper.GetSmallChange: TScrollBarInc;
begin
  Result := InnerScrollBar.GetSmallChange;
end;

function TdxScrollBarWrapper.GetTop: Integer;
begin
  Result := InnerScrollBar.GetTop;
end;

function TdxScrollBarWrapper.GetUnlimitedTracking: Boolean;
begin
  Result := InnerScrollBar.GetUnlimitedTracking;
end;

function TdxScrollBarWrapper.GetVisible: Boolean;
begin
  Result := InnerScrollBar.GetVisible;
end;

function TdxScrollBarWrapper.GetWidth: Integer;
begin
  Result := InnerScrollBar.GetWidth;
end;

procedure TdxScrollBarWrapper.SetPageSize(AValue: Integer);
begin
  InnerScrollBar.SetPageSize(AValue);
end;

procedure TdxScrollBarWrapper.SetPosition(AValue: Integer);
begin
  InnerScrollBar.SetPosition(AValue);
end;

procedure TdxScrollBarWrapper.SetScrollParams(AMin, AMax, APosition, APageSize: Integer; ARedraw: Boolean);
begin
  InnerScrollBar.SetScrollParams(AMin, AMax, APosition, APageSize, ARedraw);
end;

procedure TdxScrollBarWrapper.SetSmallChange(AValue: TScrollBarInc);
begin
  InnerScrollBar.SetSmallChange(AValue);
end;

procedure TdxScrollBarWrapper.SetTop(AValue: Integer);
begin
  InnerScrollBar.SetTop(AValue);
end;

procedure TdxScrollBarWrapper.SetUnlimitedTracking(AValue: Boolean);
begin
  InnerScrollBar.SetUnlimitedTracking(AValue);
end;

procedure TdxScrollBarWrapper.SetVisible(AValue: Boolean);
begin
  AValue := AValue and
    not (FIsTouchScrollUIMode and FOwnerControl.IsTouchScrollUIHidden(TouchScrollUIOwner));
  InnerScrollBar.SetVisible(AValue);
end;

procedure TdxScrollBarWrapper.SetWidth(AValue: Integer);
begin
  InnerScrollBar.SetWidth(AValue);
end;

procedure TdxScrollBarWrapper.DoScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TdxScrollBarWrapper.UpdateBounds(const ABoundsRect, AVisibleBoundsRect: TRect);
begin
  if FIsTouchScrollUIMode then
    (InnerScrollBar as TcxControlPopupScrollBar).InternalSetOwnerControlRelativeBounds(ABoundsRect, AVisibleBoundsRect)
  else
  begin
    BoundsRect := ABoundsRect;
    (InnerScrollBar as TcxControlScrollBarHelper).VisibleRgn := CreateRectRgnIndirect(AVisibleBoundsRect);
  end;
end;

procedure TdxScrollBarWrapper.Calculate;
begin
  if not FIsTouchScrollUIMode then
    (FInnerScrollBar as TcxControlScrollBarHelper).Calculate;
end;

procedure TdxScrollBarWrapper.CreateInnerScrollBar;
begin
  FIsTouchScrollUIMode := FOwnerControl.IsPopupScrollBars;
  if FIsTouchScrollUIMode then
  begin
    FInnerScrollBar := dxTouchScrollBarClass.CreateEx(FTouchScrollUIOwner);
    TcxControlPopupScrollBar(FInnerScrollBar).OnScroll := DoScroll;
    if FOwnerControl.HandleAllocated then
      InitControl;
  end
  else
  begin
    FInnerScrollBar := TcxControlScrollBarHelper.Create(FOwnerControl);
    TcxControlScrollBarHelper(FInnerScrollBar).OnScroll := DoScroll;
    FOwnerControl.ScrollBarsManager.RegisterScrollBar(TcxControlScrollBarHelper(FInnerScrollBar));
  end;
end;

procedure TdxScrollBarWrapper.DestroyInnerScrollBar;
begin
  if FInnerScrollBar is TcxControlScrollBarHelper then
    FOwnerControl.ScrollBarsManager.UnRegisterScrollBar(FInnerScrollBar as TcxControlScrollBarHelper)
  else
    FOwnerControl.Deactivate(TouchScrollUIOwner);
  FreeAndNil(FInnerScrollBar);
end;

function TdxScrollBarWrapper.GetDefaultSize: TSize;
begin
  if FIsTouchScrollUIMode then
    Result := (InnerScrollBar as TcxControlPopupScrollBar).GetInitialSize
  else
    Result := (InnerScrollBar as TcxControlScrollBarHelper).GetScrollBarSize;
end;

procedure TdxScrollBarWrapper.InitControl;
begin
  if FIsTouchScrollUIMode then
  begin
    (FInnerScrollBar as TcxControlScrollBar).InitControl;
    (FInnerScrollBar as TcxControlScrollBar).HandleNeeded;
  end;
end;

procedure TdxScrollBarWrapper.Invalidate;
begin
  if FIsTouchScrollUIMode then
    (FInnerScrollBar as TcxControlScrollBar).Invalidate;
end;

function TdxScrollBarWrapper.GetInnerScrollBar: IcxControlScrollBar;
begin
  Supports(FInnerScrollBar, IcxControlScrollBar, Result);
end;

function TdxScrollBarWrapper.GetLookAndFeel: TcxLookAndFeel;
begin
  if FIsTouchScrollUIMode then
    Result := (InnerScrollBar as TcxControlPopupScrollBar).LookAndFeel
  else
    Result := (InnerScrollBar as TcxControlScrollBarHelper).LookAndFeel;
end;

function TdxScrollBarWrapper.GetTouchScrollUIOwner: IdxTouchScrollUIOwner;
begin
  Supports(FTouchScrollUIOwner, IdxTouchScrollUIOwner, Result);
end;

procedure TdxScrollBarWrapper.SetOwnerControlRelativeBounds(const ABounds: TRect);
begin
  if FIsTouchScrollUIMode then
    (FInnerScrollBar as TcxControlScrollBar).SetOwnerControlRelativeBounds(ABounds)
  else
  begin
    InnerScrollBar.BoundsRect := ABounds;
    Calculate;
  end;
end;

{ TcxControlCustomScrollBars }

constructor TcxControlCustomScrollBars.Create(AOwner: TcxControl);
begin
  inherited Create;
  FOwner := AOwner;
  CreateScrollBars;
end;

destructor TcxControlCustomScrollBars.Destroy;
begin
  DestroyScrollBars;
  inherited;
end;

procedure TcxControlCustomScrollBars.Invalidate;
begin
end;

procedure TcxControlCustomScrollBars.DrawSizeGrip(ACanvas: TcxCanvas);
begin
end;

procedure TcxControlCustomScrollBars.ApplyData;
begin
  GetScrollBar(sbHorizontal).ApplyData;
  GetScrollBar(sbVertical).ApplyData;
end;

procedure TcxControlCustomScrollBars.BringInternalControlsToFront;
begin
end;

function TcxControlCustomScrollBars.CheckSize(AKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TcxControlCustomScrollBars.CreateScrollBars;
begin
  FHScrollBar := CreateScrollBar(sbHorizontal);
  FVScrollBar := CreateScrollBar(sbVertical);
  FSizeGrip := CreateSizeGrip;
end;

procedure TcxControlCustomScrollBars.DestroyHScrollBar;
begin
  FreeAndNil(FHScrollBar);
end;

procedure TcxControlCustomScrollBars.DestroyScrollBars;
begin
  DestroySizeGrip;
  DestroyVScrollBar;
  DestroyHScrollBar;
end;

procedure TcxControlCustomScrollBars.DestroySizeGrip;
begin
  FreeAndNil(FSizeGrip);
end;

procedure TcxControlCustomScrollBars.DestroyVScrollBar;
begin
  FreeAndNil(FVScrollBar);
end;

procedure TcxControlCustomScrollBars.DoScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  AScrollBar: IcxControlScrollBar;
begin
  Supports(Sender, IcxControlScrollBar, AScrollBar);
  FOwner.Scroll(AScrollBar.Kind, ScrollCode, ScrollPos);
end;

function TcxControlCustomScrollBars.GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar;
var
  AScrollBar: TObject;
begin
  if AKind = sbHorizontal then
    AScrollBar := FHScrollBar
  else
    AScrollBar := FVScrollBar;
  Supports(AScrollBar, IcxControlScrollBar, Result);
end;

procedure TcxControlCustomScrollBars.Hide;
begin
end;

procedure TcxControlCustomScrollBars.InitScrollBars;
begin
end;

function TcxControlCustomScrollBars.IsInternalControl(AControl: TControl): Boolean;
begin
  Result := False;
end;

function TcxControlCustomScrollBars.IsScrollBarActive(AKind: TScrollBarKind): Boolean;
begin
  Result := IsScrollBarVisible(AKind);
end;

function TcxControlCustomScrollBars.IsScrollBarVisible(AKind: TScrollBarKind): Boolean;
begin
  if FCalculating then
    Result := GetScrollBar(AKind).Data.Visible
  else
    Result := GetScrollBar(AKind).Visible;
end;

function TcxControlCustomScrollBars.IsSizeGripArea(const APoint: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxControlCustomScrollBars.SetInternalControlsBounds;
begin
end;

procedure TcxControlCustomScrollBars.UnInitScrollBars;
begin
end;

procedure TcxControlCustomScrollBars.UpdateInternalControlsState;
begin
end;

{ TcxControlScrollBars }

procedure TcxControlScrollBars.Invalidate;
begin
  if HScrollBar.Visible then
    HScrollBar.Invalidate;
  if VScrollBar.Visible then
    VScrollBar.Invalidate;
  if SizeGrip.Visible then
    SizeGrip.Repaint;
end;

procedure TcxControlScrollBars.DrawSizeGrip(ACanvas: TcxCanvas);
begin
  if SizeGrip.Visible and ACanvas.RectVisible(SizeGrip.BoundsRect) then
  begin
    SizeGrip.Paint(ACanvas);
    ACanvas.ExcludeClipRect(SizeGrip.BoundsRect);
  end;
end;

function TcxControlScrollBars.GetControl: TWinControl;
begin
  Result := FOwner;
end;

function TcxControlScrollBars.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.GetScrollBarLookAndFeel;
end;

function TcxControlScrollBars.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TcxControlScrollBars.CheckSize(AKind: TScrollBarKind): Boolean;
var
  ASize: Integer;
begin
  Result := False;
  if AKind = sbHorizontal then
  begin
    ASize := Owner.GetScrollBarSize.cy;
    if HScrollBar.Height <> ASize then
    begin
      HScrollBar.Height := ASize;
      HScrollBar.Calculate;
      Result := True;
    end;
  end
  else
  begin
    ASize := Owner.GetScrollBarSize.cx;
    if VScrollBar.Width <> ASize then
    begin
      VScrollBar.Width := ASize;
      VScrollBar.Calculate;
      Result := True;
    end;
  end;
end;

procedure TcxControlScrollBars.CheckSizeGripVisible(AValue: Boolean);
begin
  SizeGrip.Visible := AValue;
end;

function TcxControlScrollBars.CreateScrollBar(AKind: TScrollBarKind): TObject;
var
  AScrollBar: TcxControlScrollBarHelper;
begin
  AScrollBar := GetScrollBarClass(AKind).Create(Self);
  Result := AScrollBar;
  AScrollBar.Kind := AKind;
  AScrollBar.IsNonClient := False;
  AScrollBar.OnScroll := DoScroll;
  if AKind = sbHorizontal then
    AScrollBar.Height := Owner.GetScrollBarSize.cy
  else
    AScrollBar.Width := Owner.GetScrollBarSize.cx;
  Owner.ScrollBarsManager.RegisterScrollBar(AScrollBar);
end;

function TcxControlScrollBars.CreateSizeGrip: TObject;
var
  ASizeGrip: TcxSizeGripHelper;
begin
  ASizeGrip := GetSizeGripClass.Create(Self);
  ASizeGrip.IsNonClient := False;
  Result := ASizeGrip;
end;

procedure TcxControlScrollBars.DestroyHScrollBar;
begin
  Owner.ScrollBarsManager.UnRegisterScrollBar(HScrollBar);
  inherited;
end;

procedure TcxControlScrollBars.DestroyVScrollBar;
begin
  Owner.ScrollBarsManager.UnRegisterScrollBar(VScrollBar);
  inherited;
end;

function TcxControlScrollBars.GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarHelperClass;
begin
  Result := TcxControlScrollBarHelper;
end;

function TcxControlScrollBars.GetSizeGripClass: TcxSizeGripHelperClass;
begin
  Result := TcxSizeGripHelper;
end;

function TcxControlScrollBars.IsSizeGripArea(const APoint: TPoint): Boolean;
begin
  Result := SizeGrip.Visible and cxRectPtIn(SizeGrip.BoundsRect, APoint);
end;

procedure TcxControlScrollBars.SetInternalControlsBounds;
begin
  if HScrollBar.Visible then
  begin
    HScrollBar.BoundsRect := FOwner.GetHScrollBarBounds;
    HScrollBar.Calculate;
  end;
  if VScrollBar.Visible then
  begin
    VScrollBar.BoundsRect := FOwner.GetVScrollBarBounds;
    VScrollBar.Calculate;
  end;
  if SizeGrip.Visible then
    SizeGrip.BoundsRect := FOwner.GetSizeGripBounds;
end;

function TcxControlScrollBars.GetHScrollBar: TcxControlScrollBarHelper;
begin
  Result := FHScrollBar as TcxControlScrollBarHelper;
end;

function TcxControlScrollBars.GetSizeGrip: TcxSizeGripHelper;
begin
  Result := FSizeGrip as TcxSizeGripHelper;
end;

function TcxControlScrollBars.GetVScrollBar: TcxControlScrollBarHelper;
begin
  Result := FVScrollBar as TcxControlScrollBarHelper;
end;

{ TcxControlWindowedScrollBars }

procedure TcxControlWindowedScrollBars.Invalidate;
begin
  if HScrollBar.Visible then
    HScrollBar.Invalidate;
  if VScrollBar.Visible then
    VScrollBar.Invalidate;
  if SizeGrip.Visible then
    SizeGrip.Invalidate;
end;

procedure TcxControlWindowedScrollBars.BringInternalControlsToFront;
begin
  if Owner.GetScrollbarMode = sbmClassic then
  begin
    if HScrollBar.Visible then
      HScrollBar.BringToFront;
    if VScrollBar.Visible then
      VScrollBar.BringToFront;
    if SizeGrip.Visible then
      SizeGrip.BringToFront;
  end;
end;

function TcxControlWindowedScrollBars.CheckSize(AKind: TScrollBarKind): Boolean;
var
  ASize: Integer;
begin
  Result := False;
  if AKind = sbHorizontal then
  begin
    ASize := Owner.GetScrollBarSize.cy;
    if HScrollBar.Height <> ASize then
    begin
      HScrollBar.Height := ASize;
      Result := True;
    end;
  end
  else
  begin
    ASize := Owner.GetScrollBarSize.cx;
    if VScrollBar.Width <> ASize then
    begin
      VScrollBar.Width := ASize;
      Result := True;
    end;
  end;
end;

procedure TcxControlWindowedScrollBars.CheckSizeGripVisible(AValue: Boolean);
begin
  SizeGrip.Visible := AValue;
  if SizeGrip.Visible and (SizeGrip.Parent = nil) and SizeGrip.HandleAllocated then
    UpdatePopupScrollElementZOrder(SizeGrip.WindowHandle, SizeGrip.OwnerControl);
end;

function TcxControlWindowedScrollBars.CreateScrollBar(AKind: TScrollBarKind): TObject;
var
  AScrollBar: TcxControlScrollBar;
begin
  AScrollBar := FOwner.GetScrollBarClass(AKind).CreateEx(Owner);
  Result := AScrollBar;
  AScrollBar.Kind := AKind;
  AScrollBar.OnScroll := DoScroll;
  AScrollBar.LookAndFeel.MasterLookAndFeel := FOwner.LookAndFeel;
  AScrollBar.ControlStyle := AScrollBar.ControlStyle + [csReplicatable];
  AScrollBar.OnMouseEnter := ScrollBarMouseEnter;
  AScrollBar.OnMouseLeave := ScrollBarMouseLeave;
end;

function TcxControlWindowedScrollBars.CreateSizeGrip: TObject;
var
  ASizeGrip: TcxSizeGrip;
begin
  ASizeGrip := FOwner.GetSizeGripClass.CreateEx(FOwner);
  Result := ASizeGrip;
  ASizeGrip.LookAndFeel.MasterLookAndFeel := FOwner.LookAndFeel;
  ASizeGrip.ControlStyle := ASizeGrip.ControlStyle + [csReplicatable];
end;

procedure TcxControlWindowedScrollBars.DestroyScrollBars;
begin
  if Owner.IsDestroying then
  begin
    if SizeGrip.Parent = nil then
      DestroySizeGrip;
    if VScrollBar.Parent = nil then
      DestroyVScrollBar;
    if HScrollBar.Parent = nil then
      DestroyHScrollBar;
  end
  else
    inherited DestroyScrollBars;
end;

procedure TcxControlWindowedScrollBars.Hide;
begin
  VScrollBar.Visible := False;
  HScrollBar.Visible := False;
  SizeGrip.Visible := False;
end;

procedure TcxControlWindowedScrollBars.InitScrollBars;
begin
  HScrollBar.InitControl;
  VScrollBar.InitControl;
  SizeGrip.InitControl;
  HScrollBar.HandleNeeded;
  VScrollBar.HandleNeeded;
  SizeGrip.HandleNeeded;
end;

function TcxControlWindowedScrollBars.IsInternalControl(AControl: TControl): Boolean;
begin
  Result := (AControl = HScrollBar) or (AControl = VScrollBar) or (AControl = SizeGrip);
end;

procedure TcxControlWindowedScrollBars.SetInternalControlsBounds;
begin
  if HScrollBar.Visible then
    HScrollBar.SetOwnerControlRelativeBounds(GetScrollBarBounds(sbHorizontal));
  if VScrollBar.Visible then
    VScrollBar.SetOwnerControlRelativeBounds(GetScrollBarBounds(sbVertical));
  if SizeGrip.Visible then
    SizeGrip.SetOwnerControlRelativeBounds(Owner.GetSizeGripBounds);
end;

procedure TcxControlWindowedScrollBars.UnInitScrollBars;
begin
  inherited;
  if Owner.IsPopupScrollBars then
  begin
    if SizeGrip.Parent = nil then
      SizeGrip.DestroyHandle;
    if VScrollBar.Parent = nil then
      VScrollBar.DestroyHandle;
    if HScrollBar.Parent = nil then
      HScrollBar.DestroyHandle;
  end;
end;

procedure TcxControlWindowedScrollBars.UpdateInternalControlsState;
begin
  if HScrollBar.Visible then
    HScrollBar.UpdateControlState;
  if VScrollBar.Visible then
    VScrollBar.UpdateControlState;
  if SizeGrip.Visible then
    SizeGrip.UpdateControlState;
end;

function TcxControlWindowedScrollBars.GetHScrollBar: TcxControlScrollBar;
begin
  Result := FHScrollBar as TcxControlScrollBar;
end;

function TcxControlWindowedScrollBars.GetScrollBarBounds(AKind: TScrollBarKind): TRect;
begin
  case AKind of
    sbHorizontal:
      Result := Owner.GetHScrollBarBounds
  else
    Result := Owner.GetVScrollBarBounds;
  end;
end;

function TcxControlWindowedScrollBars.GetSizeGrip: TcxSizeGrip;
begin
  Result := FSizeGrip as TcxSizeGrip;
end;

function TcxControlWindowedScrollBars.GetVScrollBar: TcxControlScrollBar;
begin
  Result := FVScrollBar as TcxControlScrollBar;
end;

function TcxControlWindowedScrollBars.IsScrollBarActive(AKind: TScrollBarKind): Boolean;
begin
  if Owner.IsTouchScrollUIMode or not FCalculating and Owner.IsPopupScrollBars then
    Result := (GetScrollBar(AKind) as TcxControlPopupScrollBar).AllowVisible
  else
    Result := inherited IsScrollBarActive(AKind);
end;

procedure TcxControlWindowedScrollBars.ScrollBarMouseEnter(Sender: TObject);
begin
  if Owner.GetScrollbarMode = sbmTouch then
    Owner.HideTouchScrollUI(Owner);
end;

procedure TcxControlWindowedScrollBars.ScrollBarMouseLeave(Sender: TObject);
begin
end;

{ TcxDragAndDropObject }

constructor TcxDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create;
  FControl := AControl;
  if AControl <> nil then
    FCanvas := Control.Canvas;
  CurMousePos := cxInvalidPoint;
  PrevMousePos := cxInvalidPoint;
end;

procedure TcxDragAndDropObject.DoBeginDragAndDrop;
begin
  BeforeBeginDragAndDrop;
  CurMousePos := TranslateCoords(GetClientCursorPos);
  PrevMousePos := CurMousePos;
  BeginDragAndDrop;
end;

procedure TcxDragAndDropObject.DoDragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  ChangeMousePos(P);
  DragAndDrop(CurMousePos, Accepted);
end;

procedure TcxDragAndDropObject.DoEndDragAndDrop(Accepted: Boolean);
begin
  EndDragAndDrop(Accepted);
  AfterDragAndDrop(Accepted);
end;

function TcxDragAndDropObject.DoProcessKeyDown(var Message: TWMKeyDown): Boolean;
begin
  Result := ProcessKeyDown(Message.CharCode, KeyDataToShiftState(Message.KeyData));
end;

function TcxDragAndDropObject.DoProcessKeyUp(var Message: TWMKeyUp): Boolean;
begin
  Result := ProcessKeyUp(Message.CharCode, KeyDataToShiftState(Message.KeyData));
end;

procedure TcxDragAndDropObject.ChangeMousePos(const P: TPoint);
begin
  PrevMousePos := CurMousePos;
  CurMousePos := TranslateCoords(P);
end;

procedure TcxDragAndDropObject.DirtyChanged;
begin
end;

function TcxDragAndDropObject.GetClientCursorPos: TPoint;
begin
  Result := Control.ScreenToClient(GetMouseCursorPos);
end;

function TcxDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crNoDrop, crDrag);
begin
  Result := Cursors[Accepted];
end;

function TcxDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := False;
end;

function TcxDragAndDropObject.GetZoomFactor: Integer;
begin
  Result := 100;
end;

procedure TcxDragAndDropObject.AfterDragAndDrop(Accepted: Boolean);
begin
  // do nothing
end;

procedure TcxDragAndDropObject.BeforeBeginDragAndDrop;
begin
  // do nothing
end;

procedure TcxDragAndDropObject.BeginDragAndDrop;
begin
  DirtyChanged;
end;

procedure TcxDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Dirty := False;
  Screen.Cursor := GetDragAndDropCursor(Accepted);
end;

procedure TcxDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  Dirty := True;
end;

function TcxDragAndDropObject.ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxDragAndDropObject.ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := cxPointScale(P, 100, ZoomFactor);
end;

procedure TcxDragAndDropObject.SetDirty(Value: Boolean);
begin
  if FDirty <> Value then
  begin
    FDirty := Value;
    DirtyChanged;
  end;
end;

{ TcxDragControlObject }

procedure TcxDragControlObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  inherited;
  Free;
end;

function TcxDragControlObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted and (Control as TcxControl).IsCopyDragDrop then
    Result := crDragCopy
  else
    Result := inherited GetDragCursor(Accepted, X, Y);
end;

{ TcxCustomControl }

constructor TcxCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := TdxScaleFactor.Create;
end;

destructor TcxCustomControl.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

function TcxCustomControl.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TcxCustomControl.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomControl.GetAdornerTargetElementBounds: TRect;
begin
  Result := ClientRect;
end;

function TcxCustomControl.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomControl.GetAdornerTargetElements(AList: TStrings);
begin
//do nothing
end;

{$IFDEF DELPHIBERLIN}
procedure TcxCustomControl.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  BeginScaleChange;
  try
    ChangeScaleEx(M, D, isDpiChange);
  finally
    EndScaleChange;
  end;
end;
{$ELSE}
procedure TcxCustomControl.ChangeScale(M, D: Integer);
begin
  BeginScaleChange;
  try
    ChangeScaleEx(M, D, False);
  finally
    EndScaleChange;
  end;
end;
{$ENDIF}

procedure TcxCustomControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  ScaleFactor.Change(M, D);
  inherited ChangeScale(M, D{$IFDEF DELPHIBERLIN}, isDpiChange{$ENDIF});
end;

{$IFDEF DELPHIBERLIN}
procedure TcxCustomControl.ScaleControlsForDpi(NewPPI: Integer);
begin
  DisableAlign;
  BeginScaleChange;
  try
    inherited;
  finally
    EndScaleChange;
    EnableAlign;
  end;
end;
{$ENDIF}

procedure TcxCustomControl.ScaleFactorChanging;
begin
  // do nothing
end;

procedure TcxCustomControl.ScaleFactorChanged;
begin
  // do nothing
end;

procedure TcxCustomControl.SetParent(AValue: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AValue) then
  begin
    inherited SetParent(AValue);
    TcxControlHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

procedure TcxCustomControl.BeginScaleChange;
begin
  Inc(FScaleChangeCounter);
  if FScaleChangeCounter = 1 then
    ScaleFactorChanging;
end;

procedure TcxCustomControl.EndScaleChange;
begin
  Dec(FScaleChangeCounter);
  if FScaleChangeCounter = 0 then
    ScaleFactorChanged;
end;

function TcxCustomControl.GetIsScaleChanging: Boolean;
begin
  Result := FScaleChangeCounter > 0;
end;

procedure TcxCustomControl.DXMScaleChanged(var Message: TMessage);
begin
  EndScaleChange;
end;

procedure TcxCustomControl.DXMScaleChanging(var Message: TMessage);
begin
  BeginScaleChange;
end;

{ TcxControl }

constructor TcxControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if HasDragImages then
    ControlStyle := ControlStyle + [csDisplayDragImage];
  FCanvas := TcxControlCanvas.Create(inherited Canvas);
  FFocusOnClick := True;
  FFontListenerList := TInterfaceList.Create;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChangeHandler;
  if AllowTouchScrollUIMode then
    FScrollbarMode := GetLookAndFeelScrollbarMode
  else
    FScrollbarMode := sbmClassic;
  FScrollBars := ssBoth;
  FScrollBarsManager := TcxControlScrollBarsManager.Create(Self);
  FScrollUIActivityHelper := TdxTouchScrollUIActivityHelper.Create;
  FHybridScrollbarsManager := TdxHybridScrollbarsManager.Create(Self);
  FHybridScrollbarBaseColor := clDefault;
  CreateScrollBars;
  TabStop := MayFocus;
  FGestureHelper := TdxGestureHelper.Create(Self);
  Touch.InteractiveGestures := GetDefaultInteractiveGestures;
  Touch.InteractiveGestureOptions := GetDefaultInteractiveGestureOptions;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TcxControl.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  Deactivate;
  FreeAndNil(FGestureHelper);
  EndDrag(False);
  DestroyScrollBars;
  FreeAndNil(FHybridScrollbarsManager);
  FreeAndNil(FScrollUIActivityHelper);
  FreeAndNil(FScrollBarsManager);
  FFontListenerList := nil;
  FreeAndNil(FActiveCanvas);
  FreeAndNil(FCanvas);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TcxControl.DoFinishDragAndDrop(Accepted: Boolean);
begin
  try
    if DragAndDropState = ddsInProcess then
      EndDragAndDrop(Accepted)
    else
      DragAndDropState := ddsNone;
  finally
    FreeAndNil(FDragAndDropObject);
  end;
end;

function TcxControl.GetActiveCanvas: TcxCanvas;
begin
  if HandleAllocated then
  begin
    FreeAndNil(FActiveCanvas);
    Result := Canvas;
  end
  else
  begin
    if FActiveCanvas = nil then
      FActiveCanvas := TcxScreenCanvas.Create;
    Result := FActiveCanvas;
  end;
end;

function TcxControl.GetDragAndDropObject: TcxDragAndDropObject;
begin
  if FDragAndDropObject = nil then
    FDragAndDropObject := CreateDragAndDropObject;
  Result := FDragAndDropObject;
end;

function TcxControl.GetHint: string;
begin
  Result := FOriginalHint;
end;

function TcxControl.GetHScrollBar: IcxControlScrollBar;
begin
  Result := GetScrollBar(sbHorizontal);
end;

function TcxControl.GetHScrollBarVisible: Boolean;
begin
  Result := (FMainScrollBars <> nil) and FMainScrollBars.IsScrollBarVisible(sbHorizontal);
end;

function TcxControl.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxControl.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxControl.GetSizeGrip: TcxSizeGrip;
begin
  if (FMainScrollBars <> nil) and (FMainScrollBars is TcxControlWindowedScrollBars) then
    Result := TcxControlWindowedScrollBars(FMainScrollBars).SizeGrip
  else
    Result := nil;
end;

function TcxControl.GetUpdatingScrollBars: Boolean;
begin
  Result := FUpdateScrollBarsCount > 0;
end;

function TcxControl.GetVScrollBarVisible: Boolean;
begin
  Result := (FMainScrollBars <> nil) and FMainScrollBars.IsScrollBarVisible(sbVertical)
end;

function TcxControl.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not TControlActionLinkAccess(ActionLink).IsHintLinked;
{$IFDEF DELPHI17}
  Result := Result and (FOriginalHint <> '');
{$ENDIF}
end;

procedure TcxControl.SetBorderStyle(Value: TcxControlBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    BorderStyleChanged;
  end;
end;

procedure TcxControl.SetDragAndDropState(Value: TcxDragAndDropState);
begin
  if FDragAndDropState <> Value then
  begin
    FDragAndDropState := Value;
    if (Value = ddsNone) and not FFinishingDragAndDrop then DoCancelMode;
  end;
end;

procedure TcxControl.SetHint(const Value: string);
begin
  inherited Hint := Value;
  FOriginalHint := Value;
end;

procedure TcxControl.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  LookAndFeel.Assign(Value);
end;

procedure TcxControl.SetKeys(Value: TcxKeys);
begin
  FKeys := Value;
end;

procedure TcxControl.SetMouseCaptureObject(Value: TObject);
var
  AIMouseCaptureObject: IcxMouseCaptureObject;
  AMouseWasCaught: Boolean;
begin
  if FMouseCaptureObject <> Value then
  begin
    if (FMouseCaptureObject <> nil) and
      Supports(FMouseCaptureObject, IcxMouseCaptureObject, AIMouseCaptureObject) then
      AIMouseCaptureObject.DoCancelMode;
    FMouseCaptureObject := Value;
    AMouseWasCaught := MouseCapture;
    MouseCapture := FMouseCaptureObject <> nil;
    if AMouseWasCaught and not MouseCapture and (DragAndDropState = ddsStarting) and
       CanCancelDragStartOnCaptureObjectClear then
      Perform(WM_CANCELMODE, 0, 0);
  end;
end;

procedure TcxControl.SetPopupMenu(Value: TComponent);
begin
  if not IsPopupMenu(Value) then
    Value := nil;
  if FPopupMenu <> Value then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
  end;
end;

procedure TcxControl.SetScrollBars(Value: TcxScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    BoundsChanged;
  end;
end;

procedure TcxControl.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    TransparentChanged;
  end;
end;

procedure TcxControl.DoNCPaint(AWindowDC: HDC);
var
  ABitmap: TcxBitmap;
  ABounds, AClientRect, ACustomNonClientBounds: TRect;
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(AWindowDC);
  try
    AClientRect := cxRectOffset(ClientRect, cxGetClientOffset(Handle));
    ExcludeClipRect(AWindowDC, AClientRect.Left, AClientRect.Top, AClientRect.Right, AClientRect.Bottom);
    ACustomNonClientBounds := cxRectInflate(AClientRect, GetClientOffsets);
    IntersectClipRect(AWindowDC, ACustomNonClientBounds.Left, ACustomNonClientBounds.Top,
      ACustomNonClientBounds.Right, ACustomNonClientBounds.Bottom);

    ABounds := cxGetWindowBounds(Self);
    ABitmap := TcxBitmap.CreateSize(ABounds);
    try
      PaintNonClientArea(ABitmap.cxCanvas);
      cxBitBlt(AWindowDC, ABitmap.cxCanvas.Handle, ABounds, ABounds.TopLeft, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  finally
    RestoreDC(AWindowDC, ASaveIndex);
  end;
end;

procedure TcxControl.DoScrollBarBasedGestureScroll(AKind: TScrollBarKind;
  var AAccumulatedDelta: Integer; ADeltaX, ADeltaY: Integer);

  procedure GetScrollParameters(AKind: TScrollBarKind;
    out AMin, AMax, APageSize, APosition: Integer);
  var
    AScrollBar: IcxControlScrollBar;
  begin
    AScrollBar := GetScrollBar(AKind);
    if CanScrollContentByGestureWithoutScrollBars then
    begin
      AMin := AScrollBar.Data.Min;
      AMax := AScrollBar.Data.Max;
      APageSize := AScrollBar.Data.PageSize;
      APosition := AScrollBar.Data.Position;
    end
    else
    begin
      AMin := AScrollBar.Min;
      AMax := AScrollBar.Max;
      APageSize := AScrollBar.PageSize;
      APosition := AScrollBar.Position;
    end;
    AMax := AMax - AMin - APageSize + 1;
  end;

  function GetScale(AKind: TScrollBarKind; APageSize: Integer): Integer;
  const
    AMaxScale = 30;
  var
    AThumbPage: Integer;
  begin
    if APageSize = 0 then
      Result := AMaxScale
    else
    begin
      AThumbPage := IfThen(AKind = sbHorizontal, ClientWidth, ClientHeight);
      Result := Min(AThumbPage div APageSize, AMaxScale);
    end;
  end;

var
  ANewDataPos: Integer;
  AScale: Integer;
  AMin, AMax, APageSize, APosition: Integer;
begin
  GetScrollParameters(AKind, AMin, AMax, APageSize, APosition);
  AScale := GetScale(AKind, APageSize);
  ANewDataPos := APosition - AAccumulatedDelta div AScale;
  if ANewDataPos <> APosition then
  begin
    AAccumulatedDelta := AAccumulatedDelta mod AScale;
    CheckOverpan(AKind, ANewDataPos, AMin, AMax, ADeltaX, ADeltaY);
    ANewDataPos := EnsureRange(ANewDataPos, AMin, AMax);
    DoGestureScroll(AKind, ANewDataPos);
    Update;
  end;
end;

procedure TcxControl.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FinishDragAndDrop(False);
  DoCancelMode;
end;

procedure TcxControl.WMContextMenu(var Message: TWMContextMenu);
begin
  if IsScrollBarsCapture or IsScrollBarsArea(ScreenToClient(SmallPointToPoint(Message.Pos))) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  ADrawToMemory: Boolean;
begin
  ADrawToMemory := cxIsDrawToMemory(Message);
  if HasBackground or ADrawToMemory then
  begin
    if not IsDoubleBufferedNeeded or ADrawToMemory then
      EraseBackground(Message.DC);
    Message.Result := 1;
  end
  else
    Message.Result := 0;
end;

procedure TcxControl.WMGetDlgCode(var Message: TWMGetDlgCode);
const
  DlgCodes: array[TcxKey] of Integer =
    (DLGC_WANTALLKEYS, DLGC_WANTARROWS, DLGC_WANTCHARS, DLGC_WANTTAB);
var
  I: TcxKey;
  Res: Integer;
begin
  Res := 0;
  for I := Low(TcxKey) to High(TcxKey) do
    if (I in FKeys) and ((I <> kTab) or (GetAsyncKeyState(VK_CONTROL) >= 0)) then
      Inc(Res, DlgCodes[I]);
  Message.Result := Res;
end;

procedure TcxControl.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  PostMessage(Handle, DXM_NCSIZECHANGED, 0, 0);
end;

procedure TcxControl.WMNCPaint(var Message: TWMNCPaint);
var
  DC: THandle;
  AFlags: Integer;
  ARegion, AUpdateRegion: HRGN;
begin
  inherited;
  if HasNonClientArea then
  begin
    AFlags := DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE;
    AUpdateRegion := Message.RGN;

    if AUpdateRegion <> 1 then
    begin
      ARegion := CreateRectRgnIndirect(cxEmptyRect);
      CombineRgn(ARegion, AUpdateRegion, 0, RGN_COPY);
      AFlags := AFlags or DCX_INTERSECTRGN;
    end
    else
      ARegion := 0;

    DC := GetDCEx(Handle, ARegion, AFlags);
    DoNCPaint(DC);
    ReleaseDC(Handle, DC);
    Message.Result := 0;
  end;
end;

procedure TcxControl.StandardPaintHandler(var Message: TWMPaint);

  function GetGraphicControls: TList;
  var
    ASubControl: TControl;
    I: Integer;
  begin
    Result := TList.Create;
    Result.Capacity := ControlCount;
    for I := 0 to ControlCount - 1 do
    begin
      ASubControl := Controls[I];
      if not (ASubControl is TWinControl) then
        Result.Add(ASubControl);
    end;
  end;

var
  AClientOffset, AAncestorRelativeNCOffset: TPoint;
  AClientRect: TRect;
  AClipRgn: TcxRegion;
  AGraphicControls: TList;
  AHasClipRgn: Boolean;
  ASaveIndex, I, AClip: Integer;
  ASubControl: TControl;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := Message.DC;
  AClipRgn := TcxRegion.Create;
  try
    if DC = 0 then
    begin
      AHasClipRgn := dxGetUpdateRgn(Self, AClipRgn.Handle);
      DC := BeginPaint(Handle, PS);
      if AHasClipRgn then
        SelectClipRgn(DC, AClipRgn.Handle);
    end;
    try
      AGraphicControls := GetGraphicControls;
      try
        if (csPaintCopy in ControlState) and HasNonClientArea then
        begin
          AClientOffset := GetClientOffsets.TopLeft;
          AAncestorRelativeNCOffset := cxPointOffset(cxGetClientOffset(Handle),  AClientOffset, False);
          cxPaintCanvas.BeginPaint(DC);
          try
            MoveWindowOrg(cxPaintCanvas.Handle, -AAncestorRelativeNCOffset.X, -AAncestorRelativeNCOffset.Y);
            PaintNonClientArea(cxPaintCanvas);
          finally
            cxPaintCanvas.EndPaint;
          end;
          AClientRect := ClientRect;
          MoveWindowOrg(DC, AClientOffset.X, AClientOffset.Y);
          IntersectClipRect(DC, AClientRect.Left, AClientRect.Top, AClientRect.Right, AClientRect.Bottom);
        end;
        if AGraphicControls.Count = 0 then
          PaintWindow(DC)
        else
        begin
          ASaveIndex := SaveDC(DC);
          try
            AClip := SimpleRegion;
            for I := 0 to AGraphicControls.Count - 1 do
            begin
              ASubControl := TControl(AGraphicControls[I]);
              if (ASubControl.Visible or (csDesigning in ASubControl.ComponentState) and
                not (csNoDesignVisible in ASubControl.ControlStyle)) and
                (csOpaque in ASubControl.ControlStyle) then
              begin
                AClip := ExcludeClipRect(DC, ASubControl.Left, ASubControl.Top,
                  ASubControl.Left + ASubControl.Width, ASubControl.Top + ASubControl.Height);
                if AClip = NullRegion then Break;
              end;
            end;
            if AClip <> NullRegion then
              PaintWindow(DC);
          finally
            RestoreDC(DC, ASaveIndex);
          end;
        end;
        PaintControls(DC, nil);
      finally
        AGraphicControls.Free;
      end;
    finally
      if Message.DC = 0 then
        EndPaint(Handle, PS);
    end
  finally
    AClipRgn.Free;
  end;
end;

procedure TcxControl.WMPaint(var Message: TWMPaint);
begin
  if (Message.DC <> 0) or not IsDoubleBufferedNeeded then
    StandardPaintHandler(Message)
  else
    if IsCompositionEnabled and AllowCompositionPainting then
      dxPaintWindowOnGlass(Handle, GetPaintBlackOpaqueOnGlass)
    else
      dxBufferedPaintControl(Self);
end;

procedure TcxControl.WMPrint(var Message: TWMPrint);
begin
  inherited;
  if Message.Flags and PRF_NONCLIENT <> 0 then
  begin
    if HasNonClientArea then
      DoNCPaint(Message.DC);
  end;
end;

procedure TcxControl.WMSetCursor(var Message: TWMSetCursor);

  function InternalSetCursor: Boolean;
  var
    P: TPoint;
    ACursor: TCursor;
  begin
    ACursor := crDefault;
    if (DragAndDropState = ddsNone) and (Screen.Cursor = crDefault) then
    begin
      P := ScreenToClient(GetMouseCursorPos);
      ACursor := GetCurrentCursor(P.X, P.Y);
    end;
    Result := ACursor <> crDefault;
    if Result then
      SetCursor(Screen.Cursors[ACursor]);
  end;

begin
  if (Message.CursorWnd <> Handle) or not InternalSetCursor then
    inherited;
end;

procedure TcxControl.CMBiDiModeChanged(var Message: TMessage);
var
  APrevWParam: Integer;
begin
  APrevWParam := Message.WParam;
  try
    if not RecreateWndOnBiDiModeChanged then
      Message.wParam := 1;
    inherited;
  finally
    Message.wParam := APrevWParam;
  end;
  BiDiModeChanged;
end;

procedure TcxControl.CMColorChanged(var Message: TMessage);
begin
  ColorChanged;
  inherited;
end;

procedure TcxControl.CMCursorChanged(var Message: TMessage);
begin
  CursorChanged;
  inherited;
end;

procedure TcxControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  with Message do
    if Result = 0 then
      Result := Integer(GetDesignHitTest(XPos, YPos, KeysToShiftState(Keys)));
end;

procedure TcxControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  EnabledChanged;
end;

procedure TcxControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FontChanged;
end;

procedure TcxControl.CMInvalidate(var Message: TMessage);
begin
  if HandleAllocated and not IsDestroying then
    InvalidateControl(Self, Message.WParam = 0, NeedRedrawOnResize);
end;

procedure TcxControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxControl.CMNCSizeChanged(var Message: TMessage);

  procedure CheckSize(AKind: TScrollBarKind; var AChanged: Boolean);
  begin
    AChanged := FMainScrollBars.CheckSize(AKind) or AChanged;
  end;

var
  ABoundsChanged: Boolean;
begin
  if not NeedsScrollBars then
    Exit;
  ABoundsChanged := False;
  case GetSystemSizeScrollBars of
    ssHorizontal:
       CheckSize(sbHorizontal, ABoundsChanged);
    ssVertical:
       CheckSize(sbVertical, ABoundsChanged);
    ssBoth:
    begin
      CheckSize(sbHorizontal, ABoundsChanged);
      CheckSize(sbVertical, ABoundsChanged);
    end;
  end;
  if ABoundsChanged then
    BoundsChanged;
end;

procedure TcxControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  TextChanged;
end;

procedure TcxControl.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  VisibleChanged;
end;

procedure TcxControl.CNKeyDown(var Message: TWMKeyDown);
begin
  if IsMenuKey(Message) then
  begin
    Message.Result := 1;
    Exit;
  end;
  if DragAndDropState <> ddsNone then
  begin
    if not DragAndDropObject.DoProcessKeyDown(Message) then
      FinishDragAndDrop(False);
    Message.Result := 1;
    Exit;
  end;
  inherited;
end;

procedure TcxControl.CNKeyUp(var Message: TWMKeyDown);
begin
  if (DragAndDropState = ddsNone) or not DragAndDropObject.DoProcessKeyUp(Message) then
    inherited;
end;

procedure TcxControl.CNSysKeyDown(var Message: TWMKeyDown);
begin
  if IsMenuKey(Message) then
  begin
    Message.Result := 1;
    Exit;
  end;
  inherited;
end;

procedure TcxControl.CreateScrollBars;
begin
  if not NeedsScrollBars then Exit;
  if FMainScrollBars = nil then
  begin
    FMainScrollBars := GetMainScrollBarsClass.Create(Self);
    dxSystemInfo.AddListener(Self);
  end;
  if HandleAllocated then
    InitScrollBars;
end;

procedure TcxControl.DestroyScrollBars;
begin
  if FMainScrollBars = nil then Exit;
  dxSystemInfo.RemoveListener(Self);
  FreeAndNil(FMainScrollBars);
end;

procedure TcxControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    if not CheckDefaults or (Self.Hint = '') then
      Self.Hint := TCustomAction(Sender).Hint;
end;

procedure TcxControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TcxControl.CreateWnd;
begin
  FCreatingWindow := True;
  try
    inherited;
    CheckNeedsScrollBars;
    InitControl;
    UpdateScrollBars;
  finally
    FCreatingWindow := False;
  end;
end;

procedure TcxControl.Resize;
begin
  inherited;
  BoundsChanged;
end;

procedure TcxControl.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  LookAndFeel.Refresh;
end;

procedure TcxControl.WndProc(var Message: TMessage);

  function GetMousePos: TPoint;
  begin
    if HandleAllocated and ((Width > 32768) or (Height > 32768)) then
      Result := ScreenToClient(GetMouseCursorPos)
    else
      Result := SmallPointToPoint(TWMMouse(Message).Pos);
  end;

  function GetMouseButton: TMouseButton;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN:
        Result := mbLeft;
      WM_RBUTTONDOWN:
        Result := mbRight;
    else
      Result := mbMiddle;
    end;
  end;

  procedure DoAfterMouseDown;
  begin
    case Message.Msg of
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
        with GetMousePos do
          AfterMouseDown(GetMouseButton, X, Y);
    end;
  end;

  function IsScrollBarsManagerMessage: Boolean;
  begin
    Result := (ScrollBarsManager <> nil) and ScrollBarsManager.HandleMessage(Message);
  end;

var
  ALink: TcxObjectLink;
begin
  if CanProcessScrollEvents(Message) and (IsGestureHelperMessage(Message) or IsScrollBarsManagerMessage) then
    Exit;
  if (FScrollbarMode <> sbmClassic) and FScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
    ShowTouchScrollUI(GetTouchScrollUIOwner(GetMousePos), True);
  ALink := cxAddObjectLink(Self);
  try
    if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging and
      (IsDesigning and GetDesignHitTest(GetMousePos.X, GetMousePos.Y, [ssLeft]) or
       not IsDesigning and (DragMode = dmAutomatic)) then
    begin
      if not IsControlMouseMsg(TWMMouse(Message)) then
      begin
        ControlState := ControlState + [csLButtonDown];
        Dispatch(Message);
        ControlState := ControlState - [csLButtonDown];
      end;
      Exit;
    end;
    if Message.Msg = WM_RBUTTONUP then
      FMouseRightButtonReleased := True;
    inherited;
  finally
    try
      if (ALink.Ref <> nil) and not IsDestroying then
      begin
        case Message.Msg of
          (*WM_KEYDOWN:
            if Message.wParam = VK_ESCAPE then FinishDragAndDrop(False);//!!!*)
          WM_RBUTTONUP:
            FMouseRightButtonReleased := False;
          WM_SETFOCUS:
            begin
              FocusEnter;
              FocusChanged;
            end;
          WM_KILLFOCUS:
            begin
              FocusLeave;
              FocusChanged;
            end;
        end;
        DoAfterMouseDown;
      end;
    finally
      cxRemoveObjectLink(ALink);
    end;
  end;
end;

procedure TcxControl.DestroyWindowHandle;
begin
  if not IsDestroying and NeedsScrollBars then
    FMainScrollBars.UnInitScrollBars;
  inherited DestroyWindowHandle;
  ControlState := ControlState - [csClicked];
end;

procedure TcxControl.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  inherited;
  if not Handled then
  begin
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      P := ClientToScreen(Point(0, 0)) // TODO: GetOffsetPos method
    else
      P := ClientToScreen(MousePos);

    Handled := Assigned(cxContextPopupHookFunc) and cxContextPopupHookFunc(Self, PopupMenu, MousePos) or
      DoShowPopupMenu(PopupMenu, P.X, P.Y);
  end;
end;

procedure TcxControl.BeginGestureScroll(APos: TPoint);
begin
  FGestureAccumulatedDelta := cxNullPoint;
  ShowTouchScrollUI(Self);
end;

function TcxControl.CanScrollContentByGestureWithoutScrollBars: Boolean;
begin
  Result := False;
end;

procedure TcxControl.CheckOverpan(AScrollKind: TScrollBarKind;
  ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer);
begin
  FGestureHelper.CheckOverpan(AScrollKind, ANewDataPos, AMinDataPos, AMaxDataPos, ADeltaX, ADeltaY);
end;

procedure TcxControl.DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer);
begin
  Scroll(AScrollKind, scTrack, ANewScrollPos);
end;

procedure TcxControl.EndGestureScroll;
begin
  HideTouchScrollUI(Self);
end;

procedure TcxControl.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  FGestureHelper.DoGesture(EventInfo, Handled);
end;

procedure TcxControl.DoGetGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  inherited;
  FGestureHelper.CheckGestureOptions(Gestures, Options);
end;

function TcxControl.GetDefaultInteractiveGestures: TInteractiveGestures;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to dxSupportedGestureCount - 1 do
    if IsDefaultGesture(dxSupportedGestureIDs[I]) then
      Include(Result, GetInteractiveGestureByGestureID(dxSupportedGestureIDs[I]));
end;

function TcxControl.GetDefaultInteractiveGestureOptions: TInteractiveGestureOptions;
begin
  Result := [igoParentPassthrough] + GetInteractiveGestureOptionsByPanOptions(GetDefaultPanOptions);
end;

function TcxControl.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := Self;
end;

function TcxControl.IsTouchPropertyStored(AProperty: TTouchProperty): Boolean;
begin
  case AProperty of
    tpInteractiveGestures: Result := Touch.InteractiveGestures <> GetDefaultInteractiveGestures;
    tpInteractiveGestureOptions: Result := Touch.InteractiveGestureOptions <> GetDefaultInteractiveGestureOptions;
  else
    Result := inherited IsTouchPropertyStored(AProperty);
  end;
end;

procedure TcxControl.GestureScroll(ADeltaX, ADeltaY: Integer);
var
  AAccumulatedDelta: Integer;
  AScrollingControl: IdxScrollingControl;
begin
  if Supports(Self, IdxScrollingControl, AScrollingControl) then
    TdxScrollHelper.GestureScroll(AScrollingControl, ADeltaX, ADeltaY)
  else
  begin
    if not IsScrollBarBasedGestureScroll(sbHorizontal) then
      ScrollContentByGesture(sbHorizontal, ADeltaX)
    else
      if IsScrollBarActive(sbHorizontal) and HScrollBar.Enabled or
        CanScrollContentByGestureWithoutScrollBars and HScrollBar.Data.IsValid then
      begin
        AAccumulatedDelta := FGestureAccumulatedDelta.X;
        Inc(AAccumulatedDelta, ADeltaX);
        DoScrollBarBasedGestureScroll(sbHorizontal, AAccumulatedDelta, ADeltaX, ADeltaY);
        FGestureAccumulatedDelta.X := AAccumulatedDelta;
      end;

    if not IsScrollBarBasedGestureScroll(sbVertical) then
      ScrollContentByGesture(sbVertical, ADeltaY)
    else
      if IsScrollBarActive(sbVertical) and VScrollBar.Enabled or
        CanScrollContentByGestureWithoutScrollBars and VScrollBar.Data.IsValid then
      begin
        AAccumulatedDelta := FGestureAccumulatedDelta.Y;
        Inc(AAccumulatedDelta, ADeltaY);
        DoScrollBarBasedGestureScroll(sbVertical, AAccumulatedDelta, ADeltaX, ADeltaY);
        FGestureAccumulatedDelta.Y := AAccumulatedDelta;
      end;
  end;
end;

function TcxControl.CanProcessScrollEvents(var Message: TMessage): Boolean;
begin
  Result := Enabled;
end;

function TcxControl.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := AGestureID in [GID_PAN, GID_PRESSANDTAP];
end;

function TcxControl.IsGestureHelperMessage(var Message: TMessage): Boolean;
begin
  Result := (FGestureHelper <> nil) and FGestureHelper.HandleMessage(Message);
end;

function TcxControl.IsGestureScrolling: Boolean;
begin
  Result := FGestureHelper.IsPanning;
end;

function TcxControl.IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TcxControl.GetDefaultPanOptions: Integer;
begin
  Result := dxTouchPanOptions;
end;

procedure TcxControl.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin

end;

procedure TcxControl.DoMouseEnter(AControl: TControl);
begin
  if ((AControl = Self) or (AControl = nil)) and (FMouseEnterCounter = 0) then
  begin
    Inc(FMouseEnterCounter);
    dxCallNotify(FOnMouseEnter, Self);
  end;
end;

procedure TcxControl.DoMouseLeave(AControl: TControl);
begin
  if ((AControl = Self) or (AControl = nil)) and (FMouseEnterCounter = 1) then
  begin
    Dec(FMouseEnterCounter);
    dxCallNotify(FOnMouseLeave, Self);
  end;
end;

function TcxControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) then
  begin
    WheelDelta := EnsureRange(WheelDelta, -WHEEL_DELTA, WHEEL_DELTA);
    if FMouseWheelAccumulator * WheelDelta > 0 then
      Inc(FMouseWheelAccumulator, WheelDelta)
    else
      FMouseWheelAccumulator := WheelDelta;

    if Abs(FMouseWheelAccumulator) >= WHEEL_DELTA then
    begin
      InternalMouseWheel(Shift, FMouseWheelAccumulator, MousePos);
      FMouseWheelAccumulator := 0;
    end;
    Result := True;
  end;
end;

function TcxControl.DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean;
begin
  Result := ShowPopupMenu(Self, AMenu, X, Y);
end;

procedure TcxControl.EraseBackground(DC: HDC);
begin
  if IsLoading then Exit;
  cxPaintCanvas.BeginPaint(DC);
  try
    EraseBackground(cxPaintCanvas, ClientRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TcxControl.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if IsTransparentBackground then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect)
  else
    FillRect(ACanvas.Handle, ARect, Brush.Handle);
end;

function TcxControl.GetPopupMenu: TPopupMenu;
begin
  if FPopupMenu is TPopupMenu then
    Result := TPopupMenu(FPopupMenu)
  else
    Result := nil;
end;

function TcxControl.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  ADirections: array[Boolean, Boolean] of TcxDirection = ((dirLeft, dirRight), (dirUp, dirDown));
  AScrollCode: array[Boolean] of TScrollCode = (scPageUp, scPageDown);
var
  I: Integer;
  AScrollPos: Integer;
begin
  Result := MouseWheelScrollingKind <> mwskNone;
  if Result then
    if Mouse.WheelScrollLines = -1 then
      Scroll(sbVertical, AScrollCode[WheelDelta < 0], AScrollPos)
    else
      for I := 0 to Mouse.WheelScrollLines - 1 do
        ScrollContent(ADirections[MouseWheelScrollingKind = mwskVertical, WheelDelta < 0]);
end;

function TcxControl.IsMenuKey(var Message: TWMKey): Boolean;
var
  AControl: TWinControl;
begin
  Result := False;
  if not IsDesigning then
  begin
    AControl := Self;
    repeat
      if (AControl is TcxControl) and
        IsPopupMenuShortCut(TcxControl(AControl).PopupMenu, Message) then
      begin
        Result := True;
        Break;
      end;
      AControl := AControl.Parent;
    until AControl = nil;
  end;
end;

function TcxControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := DoubleBuffered or IsWinSevenOrLater;
end;

procedure TcxControl.Modified;
begin
  SetDesignerModified(Self);
end;

procedure TcxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure ProcessDragAndDrop;
  begin
    if FFinishingDragAndDrop then Exit;
    if (Button = mbLeft) and not (ssDouble in Shift) and StartDragAndDrop(Point(X, Y)) then
      if DragAndDropObject.ImmediateStart then
        BeginDragAndDrop
      else
        DragAndDropState := ddsStarting
    else
      FinishDragAndDrop(False);
  end;

var
  ALink: TcxObjectLink;
  AOriginalBounds: TRect;
begin
  FMouseDownPos := Point(X, Y);
  ALink := cxAddObjectLink(Self);
  try
    if CanFocusOnClick(X, Y) and not (ssDouble in Shift) then  // to allow form showing on dbl click
    begin
      AOriginalBounds := BoundsRect;
      FActivateType := atByMouse;
      SetFocusOnMouseClick(Button, X, Y);
      if ALink.Ref = nil then Exit;
      // to workaround the bug in VCL with parented forms
      if (GetParentForm(Self) <> nil) and (GetParentForm(Self).ActiveControl = Self) and not IsFocused then
        Windows.SetFocus(Handle);
      if not IsFocused and not AllowDragAndDropWithoutFocus then
      begin
        MouseCapture := False;
        Exit;
      end;
      if UpdateMousePositionIfControlMoved then
      begin
        Inc(X, AOriginalBounds.Left - Left);
        Inc(Y, AOriginalBounds.Top - Top);
      end;
    end;
    ProcessDragAndDrop;
    if ALink.Ref = nil then Exit;
    BeforeMouseDown(Button, Shift, X, Y);
    if ALink.Ref = nil then Exit;
    inherited;
  finally
    if ALink.Ref <> nil then
    begin
      if MouseCapture then
        FMouseButtonPressed := True;
    end;
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AAccepted: Boolean;
begin
  inherited;
  if (DragAndDropState = ddsStarting) and not IsMouseInPressedArea(X, Y) then
    BeginDragAndDrop;
  if DragAndDropState = ddsInProcess then
  begin
    AAccepted := False;
    DragAndDrop(Point(X, Y), AAccepted);
  end;
  UpdateStatusHint(Point(X, Y));
end;

procedure TcxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonPressed := False;
  CancelMouseOperations;
  inherited;
end;

procedure TcxControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

procedure TcxControl.SetParentBackground(Value: Boolean);
var
  AParentBackgroundChanged: Boolean;
begin
  AParentBackgroundChanged := Value <> ParentBackground;
  inherited;
  if AParentBackgroundChanged then
    ParentBackgroundChanged;
end;

procedure TcxControl.Paint;
var
  AIntf: IcxLockedStatePaint;
  ATopMostControl: TWinControl;
  P: TPoint;
begin
  if IsLoading then Exit;
  if Supports(Self, IcxLockedStatePaint, AIntf) and (AIntf.GetImage <> nil) then
  begin
    ATopMostControl := AIntf.GetTopmostControl;
    if ATopmostControl = Self then
      ActiveCanvas.Canvas.StretchDraw(ClientRect, AIntf.GetImage)
    else
    begin
      P := ClientToParent(cxNullPoint, ATopMostControl);
      cxBitBlt(ActiveCanvas.Handle, AIntf.GetImage.cxCanvas.Handle, ClientRect, P, SRCCOPY);
    end;
  end
  else
  begin
    (Canvas as TcxControlCanvas).BeginPaint;
    try
      DoPaint;
    finally
      (Canvas as TcxControlCanvas).EndPaint;
    end;
  end;
end;

procedure TcxControl.PaintNonClientArea(ACanvas: TcxCanvas);
begin
end;

procedure TcxControl.PaintWindow(DC: HDC);


begin
  if IsLoading then Exit;
  if Canvas.Canvas.HandleAllocated then
  begin
    Canvas.SaveDC;
    try
      inherited;
    finally
      Canvas.RestoreDC;
    end;
  end
  else
    inherited;
end;

procedure TcxControl.ColorChanged;
begin
end;

procedure TcxControl.DoScrolling;
begin
  if NeedsScrollBars then
    cxProcessControlScrollingOnMiddleButton(Self, IsScrollBarActive(sbHorizontal),
      IsScrollBarActive(sbVertical), ScrollContent, FIsScrollingContent);
end;

procedure TcxControl.DoUpdateScrollBars;

  procedure CalculateScrollBarsParams;
  var
    APrevHScrollBarActive, APrevVScrollBarActive: Boolean;

    procedure CheckPixelScrollBars;
    var
      AHideScrollBar: array[TScrollBarKind] of Boolean;
      I: TScrollBarKind;

      function CanHide(AScrollBarKind: TScrollBarKind): Boolean;
      var
        AData: TcxScrollBarData;
      begin
        AData := GetScrollBar(AScrollBarKind).Data;
        Result := AData.Visible and AData.Enabled and AData.AllowHide;
      end;

      function GetOppositeScrollBarSize(AScrollBarKind: TScrollBarKind): Integer;
      begin
        if AScrollBarKind = sbHorizontal then
          Result := VScrollBar.Width
        else
          Result := HScrollBar.Height;
      end;

      procedure CheckPixelScrollBar(AScrollBarKind: TScrollBarKind);
      var
        AData: TcxScrollBarData;
      begin
        AData := GetScrollBar(AScrollBarKind).Data;
        if IsPixelScrollBar(AScrollBarKind) and
          (AData.PageSize + GetOppositeScrollBarSize(AScrollBarKind) >= AData.Max - AData.Min + 1) then
          AHideScrollBar[AScrollBarKind] := True;
      end;

    begin
      if not CanHide(sbHorizontal) or not CanHide(sbVertical) then Exit;
      AHideScrollBar[sbHorizontal] := False;
      AHideScrollBar[sbVertical] := False;
      CheckPixelScrollBar(sbHorizontal);
      CheckPixelScrollBar(sbVertical);
      if AHideScrollBar[sbHorizontal] and AHideScrollBar[sbVertical] then
        for I := Low(TScrollBarKind) to High(TScrollBarKind) do
          with GetScrollBar(I).Data do
            SetScrollBarInfo(I, Min, Max, SmallChange, PageSize + GetOppositeScrollBarSize(I),
              Position, AllowShow, AllowHide);
    end;

  begin
    if FMainScrollBars.Calculating then Exit;
    FMainScrollBars.Calculating := True;
    try
      HScrollBar.Data.Visible := False;
      VScrollBar.Data.Visible := False;
      InitScrollBarsParametersCache;
      repeat
        APrevHScrollBarActive := IsScrollBarActive(sbHorizontal);
        APrevVScrollBarActive := IsScrollBarActive(sbVertical);
        //BoundsChanged; - causes things like Left/TopPos to be recalculated during scrolling
        InitScrollBarsParameters;
      until (IsScrollBarActive(sbHorizontal) = APrevHScrollBarActive) and
        (IsScrollBarActive(sbVertical) = APrevVScrollBarActive);
      CheckPixelScrollBars;
    finally
      FMainScrollBars.Calculating := False;
    end;
  end;

  function IsInitialScrollBarsParams: Boolean;
  begin
    Result :=
      (HScrollBar.Data.Visible = FInitialHScrollBarActive) and
      (VScrollBar.Data.Visible = FInitialVScrollBarActive);
  end;

  function GetChangedScrollBars(APrevHScrollBarActive, APrevVScrollBarActive: Boolean): TScrollBarKinds;
  begin
    Result := [];
    if IsScrollBarActive(sbHorizontal) <> APrevHScrollBarActive then
      Result := Result + [sbHorizontal];
    if IsScrollBarActive(sbVertical) <> APrevVScrollBarActive then
      Result := Result + [sbVertical];
  end;

var
  APrevHScrollBarActive, APrevVScrollBarActive: Boolean;
  AChangedScrollBars: TScrollBarKinds;
begin
  if FUpdateScrollBarsCount = 0 then
  begin
    FInitialHScrollBarActive := IsScrollBarActive(sbHorizontal);
    FInitialVScrollBarActive := IsScrollBarActive(sbVertical);
  end
  else
    if IsInitialScrollBarsParams {IsLoopingDetected} then
    begin
      UpdateScrollBarBounds;
      Exit;
    end;

  Inc(FUpdateScrollBarsCount);
  try
    if HasScrollBarArea then
    begin
      APrevHScrollBarActive := IsScrollBarActive(sbHorizontal);
      APrevVScrollBarActive := IsScrollBarActive(sbVertical);

      CalculateScrollBarsParams;
      MainScrollBars.ApplyData;

      AChangedScrollBars := GetChangedScrollBars(APrevHScrollBarActive, APrevVScrollBarActive);
      if AChangedScrollBars <> [] then
        ScrollBarVisibilityChanged(AChangedScrollBars)
      else
        UpdateScrollBarBounds;
    end
    else
    begin
      CalculateScrollBarsParams;
      MainScrollBars.ApplyData;
      UpdateScrollBarBounds;
    end;
  finally
    Dec(FUpdateScrollBarsCount);
  end;
end;

procedure TcxControl.ParentBackgroundChanged;
begin
end;

procedure TcxControl.DoScrollUIModeChanged;
begin
end;

procedure TcxControl.TextChanged;
begin
end;

procedure TcxControl.TransparentChanged;
begin
  if Transparent then
    ControlStyle := ControlStyle - [csOpaque] + [csParentBackground]
  else
    ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];
  Invalidate;
end;

procedure TcxControl.VisibleChanged;
begin
  if not Visible then
    HideTouchScrollUI(Self, True);
end;

procedure TcxControl.AddChildComponent(AComponent: TcxControlChildComponent);
begin
  AComponent.Control := Self;
end;

procedure TcxControl.RemoveChildComponent(AComponent: TcxControlChildComponent);
begin
  AComponent.Control := nil;
end;

procedure TcxControl.AfterMouseDown(AButton: TMouseButton; X, Y: Integer);
begin
  if (DragMode = dmAutomatic) and (AButton = mbLeft) and
    MouseCapture and { to prevent drag and drop when mouse button is released already }
    (not IsDesigning or AllowAutoDragAndDropAtDesignTime(X, Y, [])) and
    CanDrag(X, Y) and (cxDragDetect(Handle, ClientToScreen(Point(X, Y))) = ddDrag) then
    BeginDrag(True{False});
  if AButton = mbMiddle then DoScrolling;
end;

function TcxControl.AllowAutoDragAndDropAtDesignTime(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TcxControl.AllowDragAndDropWithoutFocus: Boolean;
begin
  Result := False;
end;

function TcxControl.CanCancelDragStartOnCaptureObjectClear: Boolean;
begin
  Result := True;
end;

function TcxControl.CreateDragAndDropObject: TcxDragAndDropObject;
begin
  Result := GetDragAndDropObjectClass.Create(Self);
end;

procedure TcxControl.BeforeMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
end;

procedure TcxControl.BiDiModeChanged;
begin
end;

procedure TcxControl.BorderStyleChanged;
begin
  BoundsChanged;
end;

procedure TcxControl.BoundsChanged;
begin
  UpdateScrollBars;
  if NeedRedrawOnResize then
    Invalidate;
end;

procedure TcxControl.BringInternalControlsToFront;
begin
  FMainScrollBars.BringInternalControlsToFront;
end;

procedure TcxControl.CancelMouseOperations;
begin
  FinishDragAndDrop(True);
  MouseCaptureObject := nil;
end;

function TcxControl.CanDrag(X, Y: Integer): Boolean;
begin
  Result := DragAndDropState = ddsNone;
end;

function TcxControl.AllowCompositionPainting: Boolean;
begin
  Result := True;
end;

function TcxControl.CanFocusOnClick: Boolean;
begin
  Result := not IsDesigning and FFocusOnClick and MayFocus and CanFocus and IsWindowVisible(Handle);
end;

function TcxControl.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := CanFocusOnClick;
end;

procedure TcxControl.CursorChanged;
begin
end;

procedure TcxControl.DoCancelMode;
begin
  FMouseButtonPressed := False;
  MouseCaptureObject := nil;
end;

procedure TcxControl.DoPaint;
begin
  DrawScrollBars(Canvas);
  if FBorderStyle = cxcbsDefault then
  begin
    DrawBorder(Canvas);
    Canvas.IntersectClipRect(cxRectInflate(Bounds, -BorderSize));
    SetPaintRegion;
  end;
  // CB9021 - bug in VCL: to actually show internal controls
  // if they were made visible when one of the parent's Showing was False
  UpdateInternalControlsState;
end;

procedure TcxControl.DrawBorder(ACanvas: TcxCanvas);
begin
  LookAndFeelPainter.DrawBorder(ACanvas, Bounds);
end;

procedure TcxControl.DrawScrollBars(ACanvas: TcxCanvas);
begin
  FScrollBarsManager.Paint(ACanvas);
  if FMainScrollBars <> nil then
    FMainScrollBars.DrawSizeGrip(ACanvas);
end;

procedure TcxControl.EnabledChanged;
begin
  Invalidate;
end;

procedure TcxControl.FocusChanged;
begin
  if Assigned(FOnFocusChanged) then FOnFocusChanged(Self);
end;

function TcxControl.FocusWhenChildIsClicked(AChild: TControl): Boolean;
begin
  Result := CanFocusOnClick;
end;

procedure TcxControl.FontChanged;
var
  I: Integer;
  AIntf: IcxLockedStateFontChanged;
begin
  if Supports(Self, IcxLockedStateFontChanged, AIntf) then
    AIntf.FontChanged(Font);
  for I := 0 to FFontListenerList.Count - 1 do
    IcxFontListener(FFontListenerList[I]).Changed(Self, Font);
  Invalidate;
end;

function TcxControl.GetBorderSize: Integer;
begin
  Result := IfThen(FBorderStyle = cxcbsDefault, LookAndFeelPainter.BorderSize);
end;

function TcxControl.GetBounds: TRect;
begin
  if IsRectEmpty(FBounds) then
    if HandleAllocated then
      Result := ClientRect
    else
      Result := Rect(0, 0, Width, Height)
  else
    Result := FBounds;
end;

function TcxControl.GetClientBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, -BorderSize, -BorderSize);
  if not IsPopupScrollBars then
  begin
    if IsScrollBarActive(sbHorizontal) then
      Dec(Result.Bottom, GetHScrollBarAreaHeight);
    if IsScrollBarActive(sbVertical) then
    begin
      if not UseRightToLeftScrollBar then
        Dec(Result.Right, GetVScrollBarAreaWidth)
      else
        Inc(Result.Left, GetVScrollBarAreaWidth);
    end;
  end;
end;

function TcxControl.GetClientOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TcxControl.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  if IsScrollBarsArea(Point(X, Y)) then
    Result := crArrow
  else
    if IsDesigning then
      Result := crDefault
    else
      Result := Cursor;
end;

function TcxControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := (DragAndDropState <> ddsNone) or FMouseButtonPressed or IsScrollBarsArea(Point(X, Y));
end;

function TcxControl.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TcxDragControlObject;
end;

function TcxControl.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TcxControl.GetIsFocused: Boolean;
begin                                {7}
  Result := Focused;
end;

function TcxControl.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  Result := TcxControlWindowedScrollBars;
end;

function TcxControl.GetMouseCursorClientPos: TPoint;
begin
  Result := ScreenToClient(GetMouseCursorPos);
end;

function TcxControl.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  if IsScrollBarActive(sbVertical) then
    Result := mwskVertical
  else
    Result := mwskNone;
end;

function TcxControl.GetPaintBlackOpaqueOnGlass: Boolean;
begin
{$IFDEF DELPHI15}
  Result := csPaintBlackOpaqueOnGlass in ControlStyle;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TcxControl.GetScrollBarClass(AKind: TScrollBarKind): TcxControlScrollBarClass;
begin
  if IsPopupScrollBars then
    Result := dxTouchScrollBarClass
  else
    Result := TcxControlScrollBar;
end;

function TcxControl.GetSizeGripClass: TcxSizeGripClass;
begin
  if IsPopupScrollBars then
    Result := TcxPopupSizeGrip
  else
    Result := TcxSizeGrip;
end;

function TcxControl.HasBackground: Boolean;
begin
  Result := IsTransparentBackground;
end;

function TcxControl.HasNonClientArea: Boolean;
begin
  Result := False;
end;

function TcxControl.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := MouseWheelScrollingKind <> mwskNone;
end;

function TcxControl.IsTransparentBackground: Boolean;
begin
  Result := cxIsVCLThemesEnabled and Assigned(Parent) and (csParentBackground in ControlStyle)
end;

procedure TcxControl.InitControl;
begin
  InitScrollBars;
end;

class procedure TcxControl.InvalidateControl(AControl: TWinControl; ANeedInvalidateSelf, ANeedInvalidateChildren: Boolean);

  function NeedInvalidateControl(AControl: TControl): Boolean;
  var
    AcxTransparentControl: IcxTransparentControl;
  begin
    if Supports(AControl, IcxTransparentControl, AcxTransparentControl) then
      Result := AcxTransparentControl.IsTransparentRegionsPresent
    else
      Result := (AControl is TWinControl) and
        (not (csOpaque in AControl.ControlStyle) or (csParentBackground in AControl.ControlStyle));
  end;

var
  I: Integer;
begin
  if AControl.HandleAllocated then
  begin
    if AControl.Parent <> nil then
      AControl.Parent.Perform(CM_INVALIDATE, 1, 0);
    if ANeedInvalidateSelf then
    begin
      cxInvalidateRect(AControl.Handle, not (csOpaque in AControl.ControlStyle));
      if ANeedInvalidateChildren then
      begin
        for I := 0 to AControl.ControlCount - 1 do
          if NeedInvalidateControl(AControl.Controls[I]) then
            AControl.Controls[I].Invalidate;
      end;
    end;
  end;
end;

function TcxControl.IsInternalControl(AControl: TControl): Boolean;
begin
  Result := (FMainScrollBars <> nil) and FMainScrollBars.IsInternalControl(AControl);
end;

function TcxControl.MayFocus: Boolean;
begin
  Result := True;
end;

procedure TcxControl.MouseEnter(AControl: TControl);
begin
  DoMouseEnter(AControl);
end;

procedure TcxControl.MouseLeave(AControl: TControl);
begin
  DoMouseLeave(AControl);
end;

procedure TcxControl.FocusEnter;
begin
//do nothing
end;

procedure TcxControl.FocusLeave;
begin
//do nothing
end;

procedure TcxControl.SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer);
begin
  SetFocus;
end;

procedure TcxControl.SetPaintRegion;
begin
  Canvas.IntersectClipRect(ClientBounds);
end;

procedure TcxControl.UpdateStatusHint(const APoint: TPoint);
begin
  inherited Hint := GetStatusHint(APoint);
end;

function TcxControl.GetStatusHint(const APoint: TPoint): string;
begin
  Result := Hint;
end;

function TcxControl.NeedRedrawOnResize: Boolean;
begin
  Result := False;
end;

function TcxControl.RecreateWndOnBiDiModeChanged: Boolean;
begin
  Result := True;
end;

function TcxControl.UpdateMousePositionIfControlMoved: Boolean;
begin
  Result := True;
end;

function TcxControl.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := (GetInteractiveGestureByGestureID(AGestureId) in Touch.InteractiveGestures);
end;

function TcxControl.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  if CanScrollContentByGestureWithoutScrollBars then
    if AScrollKind = sbHorizontal then
      Result := MouseWheelScrollingKind <> mwskVertical
    else
      Result := MouseWheelScrollingKind = mwskVertical
  else
    if AScrollKind = sbHorizontal then
      Result := IsScrollBarActive(sbHorizontal) and
       ((MouseWheelScrollingKind = mwskHorizontal) or not IsScrollBarActive(sbVertical))
    else
      Result := IsScrollBarActive(sbVertical) and
       ((MouseWheelScrollingKind = mwskVertical) or not IsScrollBarActive(sbHorizontal));
end;

function TcxControl.AllowHybridScrollbarMode: Boolean;
begin
  Result := AllowTouchScrollUIMode;
end;

function TcxControl.AllowTouchScrollUIMode: Boolean;
begin
  Result := False;
end;

function TcxControl.GetPanOptions: Integer;
begin
  Result := GetPanOptionsByInteractiveGestureOptions(Touch.InteractiveGestureOptions);
  if igPan in Touch.InteractiveGestures then
    Result := GC_PAN or Result;
end;

function TcxControl.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ClientBounds, APoint) and not IsScrollBarsArea(APoint);
end;

function TcxControl.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TcxControl.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := Self;
end;

function TcxControl.GetGestureClientHandle: THandle;
begin
  Result := Handle;
end;

function TcxControl.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := AWnd = Handle;
end;

function TcxControl.GetLookAndFeelValue: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

function TcxControl.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxControl.GetScrollBarLookAndFeel: TcxLookAndFeel;
begin
  Result := FLookAndFeel;
end;

// IdxTouchScrollUIOwner
procedure TcxControl.CheckTouchScrollUIPosition;
begin
  UpdateScrollBars;
end;

function TcxControl.GetTouchScrolUIOwnerControl: TcxControl;
begin
  Result := Self;
end;

function TcxControl.HasVisibleTouchScrollUI: Boolean;
begin
  Result := VScrollBarVisible or HScrollBarVisible;
end;

procedure TcxControl.HideTouchScrollUIDirectly;
begin
  MainScrollBars.Hide;
end;

function TcxControl.GetHybridScrollbarBaseColor: TColor;
var
  AColor: TColor;
begin
  if FHybridScrollbarBaseColor <> clDefault then
    Result := FHybridScrollbarBaseColor
  else
  begin
    AColor := GetScrollContentForegroundColor;
    if TdxColorSpaceConverter.ColorToHSL(AColor).L > 0.6 then
      Result := $FFFFFF
    else
      Result := 0;
  end;
end;

function TcxControl.GetManager: TdxHybridScrollbarsManager;
begin
  Result := FHybridScrollbarsManager;
end;

procedure TcxControl.InvalidateScrollbars;
begin
  MainScrollBars.Invalidate;
end;

function TcxControl.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

procedure TcxControl.LookAndFeelChangeHandler(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  if not (csDestroying in (Application.ComponentState + ComponentState)) then
    LookAndFeelChanged(Sender, AChangedValues);
end;

procedure TcxControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  CheckTouchScrollUIMode;
end;

procedure TcxControl.InitScrollBarsParameters;
begin
end;

procedure TcxControl.InitScrollBarsParametersCache;
begin
end;

function TcxControl.IsPixelScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

function TcxControl.IsPopupScrollBars: Boolean;
begin
  Result := FScrollbarMode <> sbmClassic;
end;

function TcxControl.IsScrollBarActive(AKind: TScrollBarKind): Boolean;
begin
  Result := (FMainScrollBars <> nil) and FMainScrollBars.IsScrollBarActive(AKind);
end;

function TcxControl.IsScrollBarsArea(const APoint: TPoint): Boolean;
var
  R: TRect;
begin
  Result := False;
  if MainScrollBars <> nil then
  begin
    if FScrollbarMode = sbmClassic then
      Result := FScrollBarsManager.IsScrollBarsArea(APoint) or IsSizeGripArea(APoint)
    else
      if FScrollbarMode = sbmHybrid then
      begin
        if IsScrollBarActive(sbVertical) then
        begin
          R := GetVScrollBarBounds;
          if UseRightToLeftScrollBar then
            R.Right := R.Left + GetHybridScrollBarSize(ScaleFactor, False).cx
          else
            R.Left := R.Right - GetHybridScrollBarSize(ScaleFactor, False).cx;
          Result := PtInRect(R, APoint);
        end;
        if not Result and IsScrollBarActive(sbHorizontal) then
        begin
          R := GetHScrollBarBounds;
          R.Top := R.Bottom - GetHybridScrollBarSize(ScaleFactor, False).cy;
          Result := PtInRect(R, APoint);
        end;
      end;
  end;
end;

function TcxControl.IsScrollBarsCapture: Boolean;
begin
  Result := FScrollBarsManager.IsCapture;
end;

function TcxControl.IsTouchScrollUIHidden(ATouchScrollBarOwner: IdxTouchScrollUIOwner = nil): Boolean;
var
  AOwner: IdxTouchScrollUIOwner;
  AHybridScrollbarsManager: TdxHybridScrollbarsManager;
  AHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  AOwner := ATouchScrollBarOwner;
  if AOwner = nil then
    AOwner := Self;
  Result := False;
  if FScrollbarMode = sbmTouch then
    Result := TdxTouchScrollUIModeManager.IsScrollUIHidden(AOwner)
  else
    if (FScrollbarMode = sbmHybrid) and Supports(AOwner, IdxHybridScrollbarOwner, AHybridScrollbarOwner) then
    begin
      AHybridScrollbarsManager := AHybridScrollbarOwner.GetManager;
      Result := not AHybridScrollbarsManager.AllowVisible;
    end;
end;

function TcxControl.IsTouchScrollUIMode: Boolean;
begin
  Result := FScrollbarMode = sbmTouch;
end;

function TcxControl.IsSizeGripArea(const APoint: TPoint): Boolean;
begin
  Result := (FMainScrollBars <> nil) and FMainScrollBars.IsSizeGripArea(APoint);
end;

function TcxControl.IsSizeGripVisible: Boolean;
begin
  Result := HScrollBar.Visible and VScrollBar.Visible;
end;

function TcxControl.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

function TcxControl.NeedsToBringInternalControlsToFront: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TcxControl.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);
begin
end;

procedure TcxControl.ScrollBarVisibilityChanged(AScrollBars: TScrollBarKinds);
begin
  BoundsChanged;
end;

function TcxControl.CanScrollLineWithoutScrollBars(ADirection: TcxDirection): Boolean;
begin
  Result := False;
end;

function TcxControl.CanUpdateScrollBars: Boolean;
begin
  Result := not IsLoading and NeedsScrollBars;
end;

procedure TcxControl.CheckNeedsScrollBars;
begin
  if NeedsScrollBars then
    CreateScrollBars
  else
    DestroyScrollBars;
end;

procedure TcxControl.CheckTouchScrollUIMode;
begin
  if AllowTouchScrollUIMode and
    HasScrollBars and IsScrollBarModeChanged then
    ScrollUIModeChanged;
end;

function TcxControl.GetHScrollBarAreaHeight: Integer;
begin
  if not HasScrollBarArea then
    Result := 0
  else
    case FScrollbarMode of
      sbmClassic:
        Result := HScrollBar.Height
    else // sbmTouch, sbmHybrid
      Result := GetHScrollBarDefaultAreaHeight;
    end;
end;

function TcxControl.GetHScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  case FScrollbarMode of
    sbmTouch:
      begin
        Result.Top := Result.Bottom  - HScrollBar.Height;
        if VScrollBarVisible then
          Result.Right := Result.Right - VScrollBar.Width;
      end;
    sbmClassic:
      begin
        Result.Top := Result.Bottom;
        Result.Bottom := Result.Top + HScrollBar.Height;
      end;
  else // sbmHybrid
    Result.Top := Result.Bottom - HScrollBar.Height;
    if IsScrollBarActive(sbVertical) then
      Result.Right := Result.Right - VScrollBar.Width;
  end;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
end;

function TcxControl.GetHScrollBarDefaultAreaHeight: Integer;
begin
  case FScrollbarMode of
    sbmClassic:
      Result := GetScaledScrollBarSize(ScaleFactor).cy;
    sbmHybrid:
      Result := GetHybridScrollBarSize(ScaleFactor, False).cy
  else // sbmTouch
    Result := 0;
  end;
end;

function TcxControl.GetSizeGripBounds: TRect;
var
  AVertScrollBarBounds, AHorzScrollBarBounds: TRect;
begin
  AVertScrollBarBounds := GetVScrollBarBounds;
  Result.Left := AVertScrollBarBounds.Left;
  Result.Right := AVertScrollBarBounds.Right;
  AHorzScrollBarBounds := GetHScrollBarBounds;
  Result.Top := AHorzScrollBarBounds.Top;
  Result.Bottom := AHorzScrollBarBounds.Bottom;
end;

function TcxControl.GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar;
begin
  if FMainScrollBars <> nil then
    Result := FMainScrollBars.GetScrollBar(AKind)
  else
    Result := nil;
end;

function TcxControl.GetScrollBarSize: TSize;
begin
  case FScrollbarMode of
    sbmClassic:
      Result := GetScaledScrollBarSize(ScaleFactor);
    sbmTouch:
      Result := GetTouchScrollBarSize(ScaleFactor)
  else // sbmHybrid
    Result := GetHybridScrollBarSize(ScaleFactor, True);
  end;
end;

function TcxControl.GetScrollbarMode: TdxScrollbarMode;
begin
  Result := FScrollbarMode;
end;

function TcxControl.GetScrollContentForegroundColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultContentTextColor;
end;

function TcxControl.GetSystemSizeScrollBars: TcxScrollStyle;
begin
  Result := ssBoth;
end;

function TcxControl.GetVScrollBar: IcxControlScrollBar;
begin
  Result := GetScrollBar(sbVertical);
end;

function TcxControl.GetVScrollBarAreaWidth: Integer;
begin
  if not HasScrollBarArea then
    Result := 0
  else
    case FScrollbarMode of
      sbmClassic:
        Result := VScrollBar.Width
    else // sbmTouch, sbmHybrid
      Result := GetVScrollBarDefaultAreaWidth;
    end;
end;

function TcxControl.GetVScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  case FScrollbarMode of
    sbmTouch:
      begin
        Result.Left := Result.Right - VScrollBar.Width;
        if HScrollBarVisible then
          Result.Bottom := Result.Bottom - HScrollBar.Height;
      end;
    sbmClassic:
      begin
        Result.Left := Result.Right;
        Result.Right := Result.Left + VScrollBar.Width;
      end;
  else // sbmHybrid
    Result.Left := Result.Right - VScrollBar.Width;
    if IsScrollBarActive(sbHorizontal) then
      Result.Bottom := Result.Bottom - HScrollBar.Height;
  end;
  if UseRightToLeftScrollBar then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
end;

function TcxControl.GetVScrollBarDefaultAreaWidth: Integer;
begin
  case FScrollbarMode of
    sbmClassic:
      Result := GetScaledScrollBarSize(ScaleFactor).cx;
    sbmHybrid:
      Result := GetHybridScrollBarSize(ScaleFactor, False).cx
  else // sbmTouch
    Result := 0;
  end;
end;

function TcxControl.HasScrollBarArea: Boolean;
begin
  Result := GetScrollbarMode = sbmClassic;
end;

function TcxControl.HasScrollBars: Boolean;
begin
  Result := NeedsScrollBars;
end;

procedure TcxControl.HideScrollBars;
begin
  if IsPopupScrollBars then
    HideTouchScrollUI(Self, True)
  else
    MainScrollBars.Hide;
end;

procedure TcxControl.HideTouchScrollUI(AValue: IdxTouchScrollUIOwner; AImmediately: Boolean = False);
var
  AOwner: IdxTouchScrollUIOwner;
  AHybridScrollbarsManager: TdxHybridScrollbarsManager;
  AHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  AOwner := AValue;
  if AOwner = nil then
    AOwner := Self;
  if FScrollbarMode = sbmTouch then
    TdxTouchScrollUIModeManager.HideScrollUI(AOwner, AImmediately)
  else
    if (FScrollbarMode = sbmHybrid) and Supports(AOwner, IdxHybridScrollbarOwner, AHybridScrollbarOwner) then
    begin
      AHybridScrollbarsManager := AHybridScrollbarOwner.GetManager;
      AHybridScrollbarsManager.Hide(AImmediately, not AImmediately);
    end;
end;

procedure TcxControl.InitScrollBars;
begin
  if NeedsScrollBars then
    FMainScrollBars.InitScrollBars;
end;

procedure TcxControl.SetInternalControlsBounds;
begin
  FMainScrollBars.SetInternalControlsBounds;
end;

procedure TcxControl.ShowTouchScrollUI(AValue: IdxTouchScrollUIOwner; AControlledByTimer: Boolean = False);
var
  AHybridScrollbarsManager: TdxHybridScrollbarsManager;
  AHybridScrollbarOwner: IdxHybridScrollbarOwner;
begin
  if FScrollbarMode = sbmTouch then
    TdxTouchScrollUIModeManager.ShowScrollUI(AValue, AControlledByTimer)
  else
    if (FScrollbarMode = sbmHybrid) and Supports(AValue, IdxHybridScrollbarOwner, AHybridScrollbarOwner) then
    begin
      AHybridScrollbarsManager := AHybridScrollbarOwner.GetManager;
      AHybridScrollbarsManager.Show(AControlledByTimer);
    end;
end;

procedure TcxControl.UpdateInternalControlsState;
begin
  if NeedsScrollBars then
    FMainScrollBars.UpdateInternalControlsState;
end;

procedure TcxControl.UpdateScrollBarBounds;
begin
  FMainScrollBars.CheckSizeGripVisible(IsSizeGripVisible);
  SetInternalControlsBounds;
  if NeedsToBringInternalControlsToFront then
    BringInternalControlsToFront;
end;

procedure TcxControl.UpdateScrollBars;
begin
  if not CanUpdateScrollBars then Exit;
  if FScrollBarsLockCount > 0 then
  begin
    FScrollBarsUpdateNeeded := True;
    Exit;
  end;
  DoUpdateScrollBars;
end;

procedure TcxControl.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  DragAndDropObject.DoDragAndDrop(P, Accepted);
end;

procedure TcxControl.EndDragAndDrop(Accepted: Boolean);
begin
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;
  MouseCapture := False;
  MouseCaptureObject := nil;
  DragAndDropObject.DoEndDragAndDrop(Accepted);
  {DragAndDropObject.DoEndDragAndDrop(Accepted);
  SetCaptureControl(nil);
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;}
end;

function TcxControl.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := FDragAndDropObjectClass;
  if Result = nil then
    Result := TcxDragAndDropObject;
end;

function TcxControl.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxControl.DoStartDrag(var DragObject: TDragObject);
begin
  Update;
  inherited;
  if (DragObject = nil) and (GetDragObjectClass <> nil) then
    DragObject := GetDragObjectClass.Create(Self);
  if not StartDrag(DragObject) then
  begin
    DragObject.Free;
    DragObject := nil;
    CancelDrag;
  end;
end;

procedure TcxControl.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  FreeAndNil(FDragImages);
end;

procedure TcxControl.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if Source is TDragObject then
    if State = dsDragLeave then
      FDragObject := nil
    else
      FDragObject := TDragObject(Source);
  inherited;
end;

procedure TcxControl.DrawDragImage(ACanvas: TcxCanvas; const R: TRect);
begin
end;

function TcxControl.GetDragImages: TDragImageList;
begin
  if HasDragImages then
  begin
    if FDragImages = nil then
    begin
      FDragImages := GetDragImagesClass.Create(nil);
      InitDragImages(FDragImages);
    end;
    if FDragImages.Count = 0 then
      Result := nil
    else
      Result := FDragImages;
  end
  else
    Result := nil;
end;

function TcxControl.GetDragImagesClass: TcxDragImageListClass;
begin
  Result := TcxDragImageList;
end;

function TcxControl.GetDragImagesSize: TPoint;
begin
  Result := Point(0, 0);
end;

function TcxControl.GetIsCopyDragDrop: Boolean;
begin
  Result := IsCtrlPressed;
end;

function TcxControl.HasDragImages: Boolean;
begin
  Result := False;
end;

procedure TcxControl.HideDragImage;
begin
  if GetDragObject <> nil then
    GetDragObject.HideDragImage;
end;

procedure TcxControl.InitDragImages(ADragImages: TcxDragImageList);
var
  B: TBitmap;
  ACanvas: TcxCanvas;
begin
  with GetDragImagesSize do
  begin
    if (X = 0) or (Y = 0) then
    begin
      ADragImages.Clear;
      Exit;
    end;
    ADragImages.Width := X;
    ADragImages.Height := Y;
  end;

  B := TBitmap.Create;
  try
    with B do
    begin
      Width := ADragImages.Width;
      Height := ADragImages.Height;
      ACanvas := TcxCanvas.Create(Canvas);
      try
        DrawDragImage(ACanvas, Rect(0, 0, Width, Height));
      finally
        ACanvas.Free;
      end;
    end;
    ADragImages.AddMasked(B, B.TransparentColor);
  finally
    B.Free;
  end;
end;

procedure TcxControl.ShowDragImage;
begin
  if GetDragObject <> nil then
    GetDragObject.ShowDragImage;
end;

function TcxControl.CanFocusEx: Boolean;
var
  AParentForm: TCustomForm;
begin
  AParentForm := GetParentForm(Self);
  Result := CanFocus and ((AParentForm = nil) or
    AParentForm.CanFocus and AParentForm.Enabled and AParentForm.Visible);
end;

function TcxControl.AcceptMousePosForClick(X, Y: Integer): Boolean;
begin
  Result := (DragMode = dmManual) or IsMouseInPressedArea(X, Y);
end;

procedure TcxControl.DefaultHandler(var Message);
begin
  if not TcxControlDefaultHandlerHelper.Process(Message) then
    inherited DefaultHandler(Message);
end;

procedure TcxControl.InvalidateRect(const R: TRect; EraseBackground: Boolean);
begin
  if HandleAllocated then
    cxInvalidateRect(Handle, R, EraseBackground);
end;

procedure TcxControl.InvalidateRgn(ARegion: TcxRegion; EraseBackground: Boolean);
begin
  if HandleAllocated and (ARegion <> nil) and not ARegion.IsEmpty then
    Windows.InvalidateRgn(Handle, ARegion.Handle, EraseBackground);
end;

procedure TcxControl.InvalidateWithChildren;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_NOERASE);
end;

function TcxControl.IsMouseInPressedArea(X, Y: Integer): Boolean;
begin
  Result := IsPointInDragDetectArea(MouseDownPos, X, Y);
end;

procedure TcxControl.PostMouseMove;
begin
  if HandleAllocated then
    PostMouseMove(ScreenToClient(GetMouseCursorPos));
end;

procedure TcxControl.PostMouseMove(AMousePos: TPoint);
begin
  if HandleAllocated and (GetCapture = 0) then
    with AMousePos do
      PostMessage(Handle, WM_MOUSEMOVE, 0, MakeLParam(X, Y));
end;

procedure TcxControl.ScrollContent(ADirection: TcxDirection);

  function GetScrollBarKind: TScrollBarKind;
  begin
    if ADirection in [dirLeft, dirRight] then
      Result := sbHorizontal
    else
      Result := sbVertical;
  end;

  function GetScrollCode: TScrollCode;
  begin
    if ADirection in [dirLeft, dirUp] then
      Result := scLineUp
    else
      Result := scLineDown;
  end;

  function GetScrollPos: Integer;
  begin
    if not NeedsScrollBars then
      Result := 0
    else
      Result := GetScrollBar(GetScrollBarKind).Position;
  end;

var
  AScrollPos: Integer;
begin
  if (ADirection = dirNone) or
    not NeedsScrollBars and not CanScrollLineWithoutScrollBars(ADirection) then
      Exit;
  ShowTouchScrollUI(Self);
  try
    AScrollPos := GetScrollPos;
    Scroll(GetScrollBarKind, GetScrollCode, AScrollPos);
    Update;
  finally
    HideTouchScrollUI(Self);
  end;
end;

procedure TcxControl.ScrollWindow(DX, DY: Integer; const AScrollRect: TRect);
begin
  HideDragImage;
  try
    ScrollWindowEx(Handle, DX, DY, @AScrollRect, nil, 0, nil, SW_ERASE or SW_INVALIDATE);
  finally
    ShowDragImage;
  end;
end;

procedure TcxControl.SetFocus;
begin
  inherited SetFocus;
  FActivateType := atOther;
end;

procedure TcxControl.SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
  AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
var
  AScrollBarData: TcxScrollBarData;
  AAllowShowInOptions: Boolean;
begin
  AScrollBarData := GetScrollBar(AScrollBarKind).Data;
  AScrollBarData.PageSize := APage;
  AScrollBarData.Min := AMin;
  AScrollBarData.Max := AMax;
  AScrollBarData.SmallChange := AStep;
  AScrollBarData.LargeChange := APage;
  AScrollBarData.Position := APos;
  AScrollBarData.Validate;

  if AScrollBarKind = sbHorizontal then
    AAllowShowInOptions := FScrollBars in [ssHorizontal, ssBoth]
  else
    AAllowShowInOptions := FScrollBars in [ssVertical, ssBoth];

  AScrollBarData.AllowShow := AAllowShow and AAllowShowInOptions and
    ((FScrollbarMode = sbmClassic) or AScrollBarData.IsValid);
  AScrollBarData.AllowHide := AAllowHide;

  if AScrollBarData.AllowShow then
    if not AScrollBarData.IsValid then
      if AScrollBarData.AllowHide then
        AScrollBarData.Visible := False
      else
      begin
        AScrollBarData.Enabled := False;
        AScrollBarData.Visible := True;
      end
    else
    begin
      AScrollBarData.Enabled := True;
      AScrollBarData.Visible := True;
    end
  else
    AScrollBarData.Visible := False;
end;

function TcxControl.StartDrag(DragObject: TDragObject): Boolean;
begin
  Result := True;
end;

procedure TcxControl.UpdateWithChildren;
begin
  if HandleAllocated then
    cxRedrawWindow(Handle, RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

procedure TcxControl.BeginDragAndDrop;
begin
  DragAndDropObject.DoBeginDragAndDrop;
  MouseCapture := True;
  FDragAndDropPrevCursor := Screen.Cursor;
  DragAndDropState := ddsInProcess;
end;

procedure TcxControl.FinishDragAndDrop(Accepted: Boolean);
begin
  if FFinishingDragAndDrop then Exit;
  FFinishingDragAndDrop := True;
  try
    DoFinishDragAndDrop(Accepted);
  finally
    FFinishingDragAndDrop := False;
  end;
end;

procedure TcxControl.AddFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Add(AListener);
end;

procedure TcxControl.Deactivate(AValue: IdxTouchScrollUIOwner = nil);
var
  AOwner: IdxTouchScrollUIOwner;
begin
  if IsTouchScrollUIMode then
  begin
    AOwner := AValue;
    if AOwner = nil then
      AOwner := Self;
    TdxTouchScrollUIModeManager.Deactivate(AOwner);
  end;
end;

procedure TcxControl.DoCreateScrollBars;
begin
  CreateScrollBars;
end;

procedure TcxControl.DoDestroyScrollBars;
begin
  DestroyScrollBars;
end;

function TcxControl.GetLookAndFeelScrollbarMode: TdxScrollbarMode;
begin
  if GetScrollBarLookAndFeel.ScrollbarMode = sbmDefault then
    if cxIsTouchScrollUIModeEnabled then
      Result := sbmTouch
    else
      Result := sbmClassic
  else
  begin
    Result := GetScrollBarLookAndFeel.ScrollbarMode;
    if (Result = sbmHybrid) and not AllowHybridScrollbarMode then
      Result := sbmTouch;
  end;
end;

function TcxControl.IsScrollBarModeChanged: Boolean;
begin
  Result := FScrollbarMode <> GetLookAndFeelScrollbarMode;
end;

procedure TcxControl.RemoveFontListener(AListener: IcxFontListener);
begin
  FFontListenerList.Remove(AListener);
end;

procedure TcxControl.LockScrollBars;
begin
  if FScrollBarsLockCount = 0 then
    FScrollBarsUpdateNeeded := False;
  Inc(FScrollBarsLockCount);
end;

procedure TcxControl.ScrollUIModeChanged;
begin
  FIsScrollUIModeChanging := True;
  try
    HideScrollBars;
    Deactivate;
    DoDestroyScrollBars;
    FScrollbarMode := GetLookAndFeelScrollbarMode;
    DoCreateScrollBars;
  finally
    FIsScrollUIModeChanging := False;
  end;
  DoScrollUIModeChanged;
end;

procedure TcxControl.UnlockScrollBars;
begin
  if FScrollBarsLockCount > 0 then
  begin
    Dec(FScrollBarsLockCount);
    if (FScrollBarsLockCount = 0) and FScrollBarsUpdateNeeded then
      UpdateScrollBars;
  end;
end;

procedure TcxControl.TranslationChanged;
begin
end;

procedure TcxControl.SystemInfoChanged(AParameter: Cardinal);
begin
  if HandleAllocated then
    SendNotifyMessage(Handle, DXM_NCSIZECHANGED, 0, 0);
end;

procedure TcxControl.WMWindowPosChanged(var Message: TWMWindowPosChanged);

  function IsPositionChanged: Boolean;
  begin
    Result := (Message.WindowPos.Flags and SWP_NOMOVE = 0) and
      ((Left <> Message.WindowPos.x) or (Top <> Message.WindowPos.y));
  end;

  function IsSizeChanged: Boolean;
  begin
    Result := (Message.WindowPos.Flags and SWP_NOSIZE = 0) and
      ((Width <> Message.WindowPos.cx) or (Height <> Message.WindowPos.cy));
  end;

var
  ANeedTouchScrollUIChange: Boolean;
begin
  ANeedTouchScrollUIChange := IsPopupScrollBars and HasVisibleTouchScrollUI and
    IsPositionChanged and not IsSizeChanged;
  inherited;
  if ANeedTouchScrollUIChange then
    CheckTouchScrollUIPosition;
end;

{ TcxControlHelper }

class function TcxControlHelper.CanSetParent(AControl, ANewParent: TControl): Boolean;
begin
  Result := (AControl.Parent <> ANewParent) or (csReading in AControl.ComponentState);
end;

class procedure TcxControlHelper.ChangeScaleFactor(AControl: TControl; ATargetScaleFactor: TdxScaleFactor);
begin
  ChangeScaleFactor(AControl, ATargetScaleFactor.Numerator, ATargetScaleFactor.Denominator);
end;

class procedure TcxControlHelper.ChangeScaleFactor(AControl: TControl; ATargetNumerator, ATargetDenominator: Integer);
var
  ADenominator: Integer;
  ANumerator: Integer;
  ASource: IdxScaleFactor;
begin
  if Supports(AControl, IdxScaleFactor, ASource) then
  begin
    ANumerator := ATargetNumerator * ASource.Value.Denominator;
    ADenominator := ATargetDenominator * ASource.Value.Numerator;
    cxReduceFraction(ANumerator, ADenominator);
    if csLoading in AControl.ComponentState then
    begin
      TdxScaleFactorAccess(ASource.Value).Assign(ATargetNumerator, ATargetDenominator, True);
    {$IFDEF DELPHIBERLIN}
      TControlAccess(AControl).FCurrentPPI := MulDiv(TControlAccess(AControl).FCurrentPPI, ANumerator, ADenominator);
    {$ENDIF}
    end
    else
      if ANumerator <> ADenominator then
      {$IFDEF DELPHIBERLIN}
        TControlAccess(AControl).ScaleForPPI(MulDiv(TControlAccess(AControl).FCurrentPPI, ANumerator, ADenominator));
      {$ELSE}
        TControlAccess(AControl).ChangeScale(ANumerator, ADenominator);
      {$ENDIF}
  end;
end;

class function TcxControlHelper.GetCurrentDPIFromControl(AObject: TObject): Integer;
var
{$IFNDEF DELPHI101BERLIN}
  AScaleFactor: IdxScaleFactor;
{$ENDIF}
  AControl: TControl;
begin
  AControl := GetControlByObject(AObject);
  if AControl <> nil then
  begin
  {$IFDEF DELPHI101BERLIN}
    Result := TControlAccess(AControl).FCurrentPPI;
  {$ELSE}
    if Supports(AObject, IdxScaleFactor, AScaleFactor) then
      Result := AScaleFactor.Value.TargetDPI
    else
      Result := dxGetCurrentDPI(AControl);
  {$ENDIF}
  end
  else
    Result := cxGetCurrentDPI;
end;

class function TcxControlHelper.GetOriginalParentSize(AObject: TObject): TPoint;
var
  AControl: TControl;
begin
  Result := cxNullPoint;
  AControl := GetControlByObject(AObject);
  if (AControl <> nil) and (AControl.Parent <> nil) then
    TWinControlAccess(AControl.Parent).UpdateControlOriginalParentSize(AControl, Result);
end;

class procedure TcxControlHelper.ScaleForPPI(AObject: TObject; ATargetPPI: Integer);
var
  AChildComponent: TcxControlChildComponent;
{$IFNDEF DELPHI101BERLIN}
  ACurrentDPI: Integer;
  AScaleFactor: IdxScaleFactor;
{$ENDIF}
  AIntf: IcxScalableComponent;
begin
  if Supports(AObject, IcxScalableComponent, AIntf) then
    AIntf.ScaleForPPI(ATargetPPI)
  else

  if AObject is TControl then
  begin
  {$IFDEF DELPHI101BERLIN}
    TControlAccess(AObject).ScaleForPPI(ATargetPPI);
  {$ELSE}
    if Supports(AObject, IdxScaleFactor, AScaleFactor) then
      ACurrentDPI := AScaleFactor.Value.TargetDPI
    else
      ACurrentDPI := dxGetCurrentDPI(TControl(AObject));

    if ACurrentDPI <> ATargetPPI then
      TControlAccess(AObject).ChangeScale(ATargetPPI, ACurrentDPI);
  {$ENDIF}
  end
  else

  if AObject is TcxControlChildComponent then
  begin
    AChildComponent := TcxControlChildComponent(AObject);
    if AChildComponent.Control <> nil then
      ScaleForPPI(AChildComponent.Control, ATargetPPI)
    else
      AChildComponent.ScaleFactor.Change(ATargetPPI, AChildComponent.ScaleFactor.TargetDPI);
  end;
end;

class procedure TcxControlHelper.SetOriginalParentSize(AObject: TObject; const APoint: TPoint);
var
  AControl: TControl;
begin
  AControl := GetControlByObject(AObject);
  if (AControl <> nil) and (AControl.Parent <> nil) then
    TControlAccess(AControl).FOriginalParentSize := APoint;
end;

class procedure TcxControlHelper.UpdateScaleFactorOnParentChange(AControl: TControl);
var
  ADenominator: Integer;
  ANumerator: Integer;
begin
  if dxGetCurrentScaleFactor(AControl.Parent, ANumerator, ADenominator) then
    ChangeScaleFactor(AControl, ANumerator, ADenominator);
end;

class function TcxControlHelper.GetControlByObject(AObject: TObject): TControl;
begin
  if AObject is TControl then
    Result := TControl(AObject)
  else
    if AObject is TcxControlChildComponent then
      Result := TControl(TcxControlChildComponent(AObject).Control)
    else
      Result := nil;
end;

{ TcxControlDefaultHandlerHelper }

class function TcxControlDefaultHandlerHelper.Process(var Message): Boolean;
begin
  Result := False;
  case TMessage(Message).Msg of
    WM_KEYDOWN, WM_SYSKEYDOWN:
      Result := SendAppMessage(DXM_POSTAPPKEYDOWN, TMessage(Message).WParam, TMessage(Message).LParam) <> 0;
  end;
end;

{ TcxScrollingControl }

procedure TcxScrollingControl.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  LayoutChanged;
end;

procedure TcxScrollingControl.BoundsChanged;
begin
  LayoutChanged;
end;

function TcxScrollingControl.CanCalculate: Boolean;
begin
  Result := HandleAllocated;
end;

procedure TcxScrollingControl.Calculate(AType: TdxChangeType);
begin
  // do nothing
end;

procedure TcxScrollingControl.CreateHandle;
begin
  inherited CreateHandle;
  LayoutChanged;
end;

procedure TcxScrollingControl.LayoutChanged(AType: TdxChangeType);
begin
  if CanCalculate then
  begin
    Calculate(AType);
    if AutoSize then
      AdjustSize;
    UpdateScrollBars;
    CheckPositions;
    Invalidate;
  end;
end;

procedure TcxScrollingControl.Loaded;
begin
  inherited Loaded;
  LayoutChanged;
end;

procedure TcxScrollingControl.ScrollPosChanged(const AOffset: TPoint);
begin
  LayoutChanged(ctLight);
  Invalidate;
  Update;
end;

function TcxScrollingControl.IsScrollDataValid: Boolean;
begin
  Result := HandleAllocated;
end;

function TcxScrollingControl.GetScrollStep: Integer;
begin
  Result := TdxScrollHelper.ScrollStep;
end;

procedure TcxScrollingControl.CheckPositions;
begin
  SetLeftTop(Point(LeftPos, TopPos));
end;

procedure TcxScrollingControl.CheckLeftTopPos(var P: TPoint);
var
  AContentSize, AClientSize: TSize;
begin
  AContentSize := GetContentSize;
  AClientSize := GetClientSize;
  P.X := Max(Min(P.X, AContentSize.cx - AClientSize.cx), 0);
  P.Y := Max(Min(P.Y, AContentSize.cy - AClientSize.cy), 0);
end;

procedure TcxScrollingControl.CheckLeftPos(var AValue: Integer);
begin
  AValue := Max(Min(AValue, GetContentSize.cx - GetClientSize.cx), 0);
end;

procedure TcxScrollingControl.CheckTopPos(var AValue: Integer);
begin
  AValue := Max(Min(AValue, GetContentSize.cy - GetClientSize.cy), 0);
end;

function TcxScrollingControl.GetContentSize: TSize;
begin
  Result := cxNullSize;
end;

function TcxScrollingControl.GetClientSize: TSize;
begin
  Result := cxSize(ClientBounds);
end;

procedure TcxScrollingControl.InitScrollBarsParameters;
begin
  inherited;
  if IsScrollDataValid then
  begin
    SetScrollBarInfo(sbHorizontal, 0, GetContentSize.cx - 1,
      ScrollStep, GetClientSize.cx, LeftPos, True, True);
    SetScrollBarInfo(sbVertical, 0, GetContentSize.cy - 1,
      ScrollStep, GetClientSize.cy, TopPos, True, True);
  end;
end;

procedure TcxScrollingControl.MakeVisible(const ARect: TRect; AType: TdxVisibilityType);

  function GetOffset(AItemMin, AItemMax, AClientMin, AClientMax: Integer): Integer;
  var
    ACenter: Integer;
  begin
    Result := 0;
    case AType of
      vtFully:
        if AItemMin < AClientMin then
          Result := AItemMin - AClientMin
        else
          if AItemMax > AClientMax then
            Result := Min(AItemMax - AClientMax, AItemMin - AClientMin);
      vtCentered:
        begin
          ACenter := (AClientMax - AClientMin) div 2;
          Result := Min(AItemMax - ACenter, AItemMin - ACenter);
        end
    else
      if AItemMin > AClientMax then
        Result := AItemMax - AClientMax
      else
        if AItemMax < AClientMin then
          Result := AItemMin - AClientMin;
    end;
  end;

var
  AClientRect: TRect;
  P: TPoint;
begin
  AClientRect := GetClientBounds;
  P.X := LeftPos + GetOffset(ARect.Left, ARect.Right, AClientRect.Left, AClientRect.Right);
  P.Y := TopPos + GetOffset(ARect.Top, ARect.Bottom, AClientRect.Top, AClientRect.Bottom);
  SetLeftTop(P);
end;

procedure TcxScrollingControl.SetAutoSize(Value: Boolean);
const
  AutoSizeModeMap: array[Boolean] of TdxAutoSizeMode = (asNone, asAutoSize);
begin
  if not FAutoSizeModeSetting and (Autosize <> Value) then
    FAutoSizeMode := AutoSizeModeMap[Value];

  inherited;
end;

procedure TcxScrollingControl.SetAutoSizeMode(AValue: TdxAutoSizeMode);
begin
  if FAutoSizeMode <> AValue then
  begin
    FAutoSizeModeSetting := True;
    try
      FAutoSizeMode := AValue;
      if AutoSize and (FAutoSizeMode <> asNone) then
        AdjustSize
      else
        AutoSize := FAutoSizeMode <> asNone;
    finally
     FAutoSizeModeSetting := False;
    end;
  end;
end;

procedure TcxScrollingControl.SetLeftTop(P: TPoint);
var
  AOffset: TPoint;
begin
  CheckLeftTopPos(P);
  if (LeftPos <> P.X) or (TopPos <> P.Y) then
  begin
    ShowTouchScrollUI(Self, True);
    AOffset := Point(FLeftPos - P.X, FTopPos - P.Y);
    FLeftPos := P.X;
    FTopPos := P.Y;
    if IsScrollDataValid then
      ScrollPosChanged(AOffset);
  end;
end;

procedure TcxScrollingControl.SetLeftPos(AValue: Integer);
begin
  SetLeftTop(Point(AValue, TopPos));
end;

procedure TcxScrollingControl.SetTopPos(AValue: Integer);
begin
  SetLeftTop(Point(LeftPos, AValue));
end;

procedure TcxScrollingControl.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);

  function GetContentPos: Integer;
  begin
    if AScrollBarKind = sbHorizontal then
      Result := LeftPos
    else
      Result := TopPos;
  end;

  procedure SetContentPos(Value: Integer);
  begin
    if AScrollBarKind = sbHorizontal then
      LeftPos := Value
    else
      TopPos := Value;
  end;

  function GetPageScrollStep: Integer;
  begin
    if AScrollBarKind = sbHorizontal then
      Result := ClientWidth
    else
      Result := ClientHeight;
  end;

begin
  inherited;
  case AScrollCode of
    scLineUp:
      SetContentPos(GetContentPos - ScrollStep);
    scLineDown:
      SetContentPos(GetContentPos + ScrollStep);
    scPageUp:
      SetContentPos(GetContentPos - GetPageScrollStep);
    scPageDown:
      SetContentPos(GetContentPos + GetPageScrollStep);
    scTrack:
      SetContentPos(AScrollPos);
  end;
  AScrollPos := GetContentPos;
end;

function TcxScrollingControl.GetTopPos: Integer;
begin
  Result := FTopPos;
end;

function TcxScrollingControl.GetLeftPos: Integer;
begin
  Result := FLeftPos;
end;

function TcxScrollingControl.GetInstance: TcxControl;
begin
  Result := Self;
end;

{ TdxScrollHelper }

class function TdxScrollHelper.IsScrollDataValid(AControl: IdxScrollingControl): Boolean;
begin
  Result := AControl.GetInstance.HandleAllocated;
end;

class procedure TdxScrollHelper.GestureScroll(AControl: IdxScrollingControl; ADeltaX, ADeltaY: Integer);
var
  ANewPos: TPoint;
  AHScrollEnabled, AVScrollEnabled: Boolean;
  AInstance: TcxControl;
begin
  AInstance := AControl.GetInstance;
  AHScrollEnabled := AInstance.IsScrollBarActive(sbHorizontal) and AInstance.HScrollBar.Enabled;
  AVScrollEnabled := AInstance.IsScrollBarActive(sbVertical) and AInstance.VScrollBar.Enabled;
  if not AHScrollEnabled then
    ADeltaX := 0;
  if not AVScrollEnabled then
    ADeltaY := 0;
  ANewPos := Point(AControl.GetLeftPos - ADeltaX, AControl.GetTopPos - ADeltaY);
  if AHScrollEnabled and (ADeltaX <> 0) then
    AInstance.GestureHelper.CheckOverpan(sbHorizontal, ANewPos.X, 0,
      AControl.GetContentSize.cx - GetClientSize(AControl).cx, ADeltaX, ADeltaY);
  if AVScrollEnabled and (ADeltaY <> 0) then
    AInstance.GestureHelper.CheckOverpan(sbVertical, ANewPos.Y, 0,
      AControl.GetContentSize.cy - GetClientSize(AControl).cy, ADeltaX, ADeltaY);
  SetPos(AControl, ANewPos.X, ANewPos.Y);
end;

class function TdxScrollHelper.GetClientSize(AControl: IdxScrollingControl): TSize;
begin
  Result := cxSize(AControl.GetInstance.ClientBounds);
end;

class procedure TdxScrollHelper.InitScrollBarsParameters(AControl: IdxScrollingControl);
begin
  AControl.GetInstance.SetScrollBarInfo(sbHorizontal, 0, AControl.GetContentSize.cx - 1,
    ScrollStep, GetClientSize(AControl).cx, AControl.GetLeftPos, True, True);
  AControl.GetInstance.SetScrollBarInfo(sbVertical, 0, AControl.GetContentSize.cy - 1,
    ScrollStep, GetClientSize(AControl).cy, AControl.GetTopPos, True, True);
end;

class procedure TdxScrollHelper.Scroll(AControl: IdxScrollingControl;
  AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);

  function GetContentPos: Integer;
  begin
    if AScrollBarKind = sbHorizontal then
      Result := AControl.GetLeftPos
    else
      Result := AControl.GetTopPos;
  end;

  procedure SetContentPos(Value: Integer);
  begin
    if AScrollBarKind = sbHorizontal then
      SetLeftPos(AControl, Value)
    else
      SetTopPos(AControl, Value);
  end;

  function GetPageScrollStep: Integer;
  begin
    if AScrollBarKind = sbHorizontal then
      Result := AControl.GetInstance.ClientWidth
    else
      Result := AControl.GetInstance.ClientHeight;
  end;

begin
  inherited;
  case AScrollCode of
    scLineUp:
      SetContentPos(GetContentPos - ScrollStep);
    scLineDown:
      SetContentPos(GetContentPos + ScrollStep);
    scPageUp:
      SetContentPos(GetContentPos - GetPageScrollStep);
    scPageDown:
      SetContentPos(GetContentPos + GetPageScrollStep);
    scTrack:
      SetContentPos(AScrollPos);
  end;
  AScrollPos := GetContentPos;
end;

class procedure TdxScrollHelper.CheckPositions(AControl: IdxScrollingControl);
begin
  SetPos(AControl, AControl.GetLeftPos, AControl.GetTopPos);
end;

class procedure TdxScrollHelper.CheckLeftPos(AControl: IdxScrollingControl; var AValue: Integer);
begin
  AValue := Max(Min(AValue, AControl.GetContentSize.cx - GetClientSize(AControl).cx), 0);
end;

class procedure TdxScrollHelper.CheckTopPos(AControl: IdxScrollingControl; var AValue: Integer);
begin
  AValue := Max(Min(AValue, AControl.GetContentSize.cy - GetClientSize(AControl).cy), 0);
end;

class procedure TdxScrollHelper.SetPos(AControl: IdxScrollingControl; X, Y: Integer);
begin
  CheckLeftPos(AControl, X);
  CheckTopPos(AControl, Y);
  if (AControl.GetLeftPos <> X) or (AControl.GetTopPos <> Y) then
  begin
    AControl.SetLeftPos(X);
    AControl.SetTopPos(Y);
    if IsScrollDataValid(AControl) then
      ScrollContent(AControl);
  end;
end;

class procedure TdxScrollHelper.SetLeftPos(AControl: IdxScrollingControl; AValue: Integer);
begin
  CheckLeftPos(AControl, AValue);
  if AControl.GetLeftPos <> AValue then
  begin
    AControl.SetLeftPos(AValue);
    if IsScrollDataValid(AControl) then
      ScrollContent(AControl);
  end;
end;

class procedure TdxScrollHelper.SetTopPos(AControl: IdxScrollingControl; AValue: Integer);
begin
  CheckTopPos(AControl, AValue);
  if AControl.GetTopPos <> AValue then
  begin
    AControl.SetTopPos(AValue);
    if IsScrollDataValid(AControl) then
      ScrollContent(AControl);
  end;
end;

class procedure TdxScrollHelper.ScrollContent(AControl: IdxScrollingControl);
begin
  AControl.GetInstance.UpdateScrollBars;
  CheckPositions(AControl);
  AControl.GetInstance.Invalidate;
  AControl.GetInstance.Update;
end;

{ TcxCustomizeListBox }

constructor TcxCustomizeListBox.Create(AOwner: TComponent);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

function TcxCustomizeListBox.GetDragAndDropItemObject: TObject;
begin
  Result := Items.Objects[FDragAndDropItemIndex];
end;

function TcxCustomizeListBox.GetItemObject: TObject;
begin
  if ItemIndex = -1 then
    Result := nil
  else
    Result := Items.Objects[ItemIndex];
end;

procedure TcxCustomizeListBox.SetItemObject(Value: TObject);
begin
  if ItemObject <> Value then
  begin
    ItemIndex := Items.IndexOfObject(Value);
    Click;
  end;
end;

procedure TcxCustomizeListBox.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

procedure TcxCustomizeListBox.WMMouseMove(var Message: TWMMouseMove);
begin
  if FDragAndDropItemIndex = -1 then
    inherited
  else
    with Message do
      MouseMove(KeysToShiftState(Keys), XPos, YPos);
end;

{$IFNDEF DELPHI101BERLIN}
procedure TcxCustomizeListBox.ChangeScale(M, D: Integer);
begin
  inherited;
  ItemHeight := MulDiv(ItemHeight, M, D);
end;
{$ENDIF}

procedure TcxCustomizeListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do
    Style := Style or CS_HREDRAW;
end;

procedure TcxCustomizeListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (ItemAtPos(Point(X, Y), True) <> -1) and (ItemObject <> nil) then
  begin
    FDragAndDropItemIndex := ItemIndex;
    FMouseDownPos := Point(X, Y);
  end;
end;

procedure TcxCustomizeListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FDragAndDropItemIndex <> -1) and
    (not IsPointInDragDetectArea(FMouseDownPos, X, Y) or
     (ItemAtPos(Point(X, Y), True) <> FDragAndDropItemIndex)) then
  begin
    ItemIndex := FDragAndDropItemIndex;
    BeginDragAndDrop;
    FDragAndDropItemIndex := -1;
  end;
end;

procedure TcxCustomizeListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

procedure TcxCustomizeListBox.BeginDragAndDrop;
var
  I: Integer;
begin
  if MultiSelect then
    with Items do
    begin
      BeginUpdate;
      try
        for I := 0 to Count - 1 do
          Selected[I] := I = ItemIndex;
      finally
        EndUpdate;
      end;
    end;
end;

{ TcxMessageWindow }

constructor TcxMessageWindow.Create;
begin
  inherited Create;
  FHandle := AllocateHWnd(MainWndProc);
  SetParent(FHandle, HWND_MESSAGE);
end;

destructor TcxMessageWindow.Destroy;
begin
  DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TcxMessageWindow.WndProc(var Message: TMessage);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TcxMessageWindow.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    Application.HandleException(Self);
  end;
end;

{ TcxSystemController }

constructor TcxSystemController.Create;
begin
  inherited;
  if IsLibrary then
    HookSynchronizeWakeup;
end;

destructor TcxSystemController.Destroy;
begin
  if IsLibrary then
    UnhookSynchronizeWakeup;
  inherited;
end;

procedure TcxSystemController.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    DXM_SYNCHRONIZETHREADS: CheckSynchronize;
  else
    inherited;
  end;
end;

procedure TcxSystemController.WakeMainThread(Sender: TObject);
begin
  PostMessage(Handle, DXM_SYNCHRONIZETHREADS, 0, 0);
end;

procedure TcxSystemController.HookSynchronizeWakeup;
begin
  FPrevWakeMainThread := Classes.WakeMainThread;
  Classes.WakeMainThread := WakeMainThread;
end;

procedure TcxSystemController.UnhookSynchronizeWakeup;
begin
  Classes.WakeMainThread := FPrevWakeMainThread;
end;

{ TdxSystemInfo }

constructor TdxSystemInfo.Create;
begin
  inherited;
  FListeners := TInterfaceList.Create;
  UpdateCache(SPI_SETDROPSHADOW);
  UpdateCache(SPI_SETNONCLIENTMETRICS);
end;

destructor TdxSystemInfo.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TdxSystemInfo.AddListener(AListener: IdxSystemInfoListener);
begin
  FListeners.Add(AListener);
end;

function TdxSystemInfo.GetParameter(AParameter: Cardinal; var AValue): Boolean;
var
  Auiparam: Cardinal;
begin
  Auiparam := 0;
  case AParameter of
    SPI_GETANIMATION:
      begin
        Auiparam := SizeOf(TAnimationInfo);
        TAnimationInfo(AValue).cbSize := Auiparam;
      end;
    SPI_GETICONTITLELOGFONT:
      Auiparam := SizeOf(TLogFont);
    SPI_GETNONCLIENTMETRICS:
      TNonClientMetrics(AValue).cbSize := TNonClientMetrics.SizeOf;
  end;
  Result := SystemParametersInfo(AParameter, Auiparam, @AValue, 0);
end;

procedure TdxSystemInfo.RemoveListener(AListener: IdxSystemInfoListener);
begin
  if (Self <> nil) and (FListeners <> nil) then
    FListeners.Remove(AListener);
end;

procedure TdxSystemInfo.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_SETTINGCHANGE) and (Message.WParam <> 0) then
  begin
    UpdateCache(Message.WParam);
    NotifyListeners(Message.WParam);
    Message.Result := 0;
  end
  else
    inherited;
end;

procedure TdxSystemInfo.NotifyListeners(AParameter: Cardinal);
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxSystemInfoListener).Changed(AParameter);
end;

function TdxSystemInfo.GetIsRemoteSession: Boolean;
begin
  Result := GetSystemMetrics(SM_REMOTESESSION) = 1;
end;

procedure TdxSystemInfo.UpdateCache(AParameter: Cardinal);
var
  AIsDropShadow: BOOL;
begin
  case AParameter of
    SPI_SETDROPSHADOW:
      begin
        GetParameter(SPI_GETDROPSHADOW, AIsDropShadow);
        FIsDropShadow := AIsDropShadow;
      end;
    SPI_SETNONCLIENTMETRICS:
      GetParameter(SPI_GETNONCLIENTMETRICS, FNonClientMetrics);
  end;
end;

{ TdxMessagesController }

constructor TdxMessagesController.Create;
begin
  inherited Create;
  FLockedMessages := TList.Create;
end;

destructor TdxMessagesController.Destroy;
begin
  FreeAndNil(FLockedMessages);
  inherited;
end;

function TdxMessagesController.IsMessageInQueue(AWnd: THandle; AMessage: UINT): Boolean;
var
  AMsg: TMSG;
begin
  Result := PeekMessage(AMsg, AWnd, AMessage, AMessage, PM_NOREMOVE) and (AMsg.hwnd = AWnd);
end;

function TdxMessagesController.KillMessages(AWnd: THandle; AMsgFilterMin, AMsgFilterMax: UINT;
  AKillAllMessages: Boolean): Boolean;
var
  AMsg: TMsg;
begin
  if AMsgFilterMax = 0 then
    AMsgFilterMax := AMsgFilterMin;
  Result := False;
  while PeekMessage(AMsg, AWnd, AMsgFilterMin, AMsgFilterMax, PM_REMOVE) do
    if AMsg.message = WM_QUIT then
    begin
      PostQuitMessage(AMsg.wParam);
      Break;
    end
    else
    begin
      Result := True;
      if not AKillAllMessages then
        Break;
    end;
end;

function TdxMessagesController.IsMessageLocked(AMessage: UINT): Boolean;
begin
  Result := FLockedMessages.IndexOf(Pointer(AMessage)) <> -1;
end;

procedure TdxMessagesController.LockMessages(AMessages: array of UINT);
var
  I: Integer;
begin
  for I := Low(AMessages) to High(AMessages) do
    FLockedMessages.Add(Pointer(AMessages[I]));
end;

procedure TdxMessagesController.UnlockMessages(AMessages: array of UINT);
var
  I: Integer;
begin
  for I := Low(AMessages) to High(AMessages) do
    FLockedMessages.Remove(Pointer(AMessages[I]));
end;

function TdxMessagesController_LockWndProc(hWnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT stdcall;
begin
  dxSetWindowProc(hwnd, dxMessagesController.FOldWndProc);
  dxMessagesController.FOldWndProc := nil;
  Result := 1;
end;

procedure TdxMessagesController.BlockMessage(AHandle: THandle);
begin
  if FOldWndProc <> nil then
    raise EdxException.Create('dxLockMessage fails');
  FOldWndProc := dxSetWindowProc(AHandle, @TdxMessagesController_LockWndProc);
end;

procedure TdxMessagesController.BlockLockedMessage(AHandle: THandle; var AMessage: UINT);
begin
  if IsMessageLocked(AMessage) then
  begin
    BlockMessage(AHandle);
    AMessage := 0;
  end;
end;

{ TcxPopupWindow }

constructor TcxPopupWindow.Create;
begin
  CreateNew(nil);
  inherited BorderStyle := bsNone;
  RightToLeftLayout := bFalse;
  DefaultMonitor := dmDesktop;
  FormStyle := fsStayOnTop;
  FAdjustable := True;
  FAlignVert := pavBottom;
  FCanvas := TcxCanvas.Create(inherited Canvas);
  FDirection := pdVertical;
  FFrameColor := clWindowText;
{$IFDEF DELPHI16}
  ControlStyle := ControlStyle + [csOverrideStylePaint];
{$ENDIF}
end;

destructor TcxPopupWindow.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TcxPopupWindow.GetNCHeight: Integer;
begin
  Result := BorderWidths[bTop] + BorderWidths[bBottom];
end;

function TcxPopupWindow.GetNCWidth: Integer;
begin
  Result := BorderWidths[bLeft] + BorderWidths[bRight];
end;

procedure TcxPopupWindow.SetAdjustable(AValue: Boolean);
begin
  if Adjustable <> AValue then
  begin
    FAdjustable := AValue;
    AdjustableChanged;
  end;
end;

procedure TcxPopupWindow.SetBorderSpace(Value: Integer);
begin
  RestoreControlsBounds;
  if BorderSpace <> Value then
  begin
    FBorderSpace := Value;
    BordersChanged;
  end;
end;

procedure TcxPopupWindow.SetBorderStyle(Value: TcxPopupBorderStyle);
begin
  RestoreControlsBounds;
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    BordersChanged;
  end;
end;

procedure TcxPopupWindow.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active <> WA_INACTIVE then
  begin
    FPrevActiveWindow := Message.ActiveWindow;
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(True), 0);
  end;
end;

procedure TcxPopupWindow.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
  begin
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(False), 0);
    CloseUp;
  end;
end;

procedure TcxPopupWindow.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  VisibleChanged;
end;

procedure TcxPopupWindow.Deactivate;
begin
  inherited;
  CloseUp;
end;

procedure TcxPopupWindow.Paint;
begin
  inherited;
  DrawFrame;
end;

procedure TcxPopupWindow.VisibleChanged;
begin
end;

procedure TcxPopupWindow.InternalCalculateHorizontalDirectionPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);
var
  AOwnerScreenBounds: TRect;
begin
  AOwnerScreenBounds := OwnerScreenBounds;
  case FAlignHorz of
    pahLeft:
      begin
        APosition.X := AOwnerScreenBounds.Left - ASize.cx;
        AOrigin.X := AOwnerScreenBounds.Left;
      end;
    pahRight:
      begin
        APosition.X := AOwnerScreenBounds.Right;
        AOrigin.X := AOwnerScreenBounds.Right - 1;
      end;
  end;
  case FAlignVert of
    pavTop:
      begin
        APosition.Y := AOwnerScreenBounds.Top;
        AOrigin.Y := AOwnerScreenBounds.Top;
      end;
    pavBottom:
      begin
        APosition.Y := AOwnerScreenBounds.Bottom - ASize.cy;
        AOrigin.Y := AOwnerScreenBounds.Bottom - 1;
      end;
  end;
end;

procedure TcxPopupWindow.InternalCalculateVerticalDirectionPosition(var APosition: TPoint; var AOrigin: TPoint;
  const ASize: TSize);
var
  AOwnerScreenBounds: TRect;
begin
  AOwnerScreenBounds := OwnerScreenBounds;
  case FAlignHorz of
    pahLeft:
      begin
        APosition.X := AOwnerScreenBounds.Left;
        AOrigin.X := AOwnerScreenBounds.Left;
      end;
    pahRight:
      begin
        APosition.X := AOwnerScreenBounds.Right - ASize.cx;
        AOrigin.X := AOwnerScreenBounds.Right - 1;
      end;
  end;
  case FAlignVert of
    pavTop:
      begin
        APosition.Y := AOwnerScreenBounds.Top - ASize.cy;
        AOrigin.Y := AOwnerScreenBounds.Top;
      end;
    pavBottom:
      begin
        APosition.Y := AOwnerScreenBounds.Bottom;
        AOrigin.Y := AOwnerScreenBounds.Bottom - 1;
      end;
  end;
end;

procedure TcxPopupWindow.InternalCalculateTargetPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);
begin
  CalculateCommonPosition(APosition, AOrigin, ASize);
  if FDirection = pdHorizontal then
    InternalCalculateHorizontalDirectionPosition(APosition, AOrigin, ASize)
  else
    InternalCalculateVerticalDirectionPosition(APosition, AOrigin, ASize);
end;

procedure TcxPopupWindow.AdjustableChanged;
begin
//do nothing
end;

procedure TcxPopupWindow.BordersChanged;
begin
//do nothing
end;

procedure TcxPopupWindow.CalculateCommonPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);
var
  AWidth, AHeight: Integer;
  AOwnerScreenBounds: TRect;
begin
  AOwnerScreenBounds := OwnerScreenBounds;
  if FAlignHorz = pahCenter then
  begin
    AWidth := AOwnerScreenBounds.Left + AOwnerScreenBounds.Right;
    APosition.X := (AWidth - ASize.cx) div 2;
    AOrigin.X := APosition.X;
  end;
  if FAlignVert = pavCenter then
  begin
    AHeight := AOwnerScreenBounds.Top + AOwnerScreenBounds.Bottom;
    APosition.Y := (AHeight - ASize.cy) div 2;
    AOrigin.Y := APosition.Y;
  end;
end;

function TcxPopupWindow.CalculatePosition(const ASize: TSize): TPoint;
var
  AOrigin: TPoint;
begin
  InternalCalculateTargetPosition(Result, AOrigin, ASize);
  CheckPosition(Result, AOrigin, ASize);
end;

function TcxPopupWindow.CalculateSize: TSize;
var
  AClientWidth, AClientHeight, I: Integer;
  ABounds: TRect;

  procedure CheckClientSize;
  begin
    with ABounds do
    begin
      if Right > AClientWidth then AClientWidth := Right;
      if Bottom > AClientHeight then AClientHeight := Bottom;
    end;
  end;

begin
  if not FAdjustable then
  begin
    Result.cx := Width;
    Result.cy := Height;
    Exit;
  end;

  AClientWidth := 0;
  AClientHeight := 0;
  for I := 0 to ControlCount - 1 do
  begin
    ABounds := Controls[I].BoundsRect;
    CheckClientSize;
    OffsetRect(ABounds, BorderWidths[bLeft], BorderWidths[bTop]);
    Controls[I].BoundsRect := ABounds;
  end;

  if (AClientWidth <> 0) or (ControlCount <> 0) then
    Result.cx := BorderWidths[bLeft] + AClientWidth + BorderWidths[bRight];
  if (AClientHeight <> 0) or (ControlCount <> 0) then
    Result.cy := BorderWidths[bTop] + AClientHeight + BorderWidths[bBottom];
end;

procedure TcxPopupWindow.CheckPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);

  procedure CheckHorizontalDirectionPosition;
  var
    AOwnerScreenBounds: TRect;
    ADesktopWorkArea: TRect;
    AMoreSpaceOnLeft: Boolean;
  begin
    if not AcceptAnyPosition then
    begin
      AOwnerScreenBounds := OwnerScreenBounds;
      ADesktopWorkArea := GetDesktopWorkArea(AOrigin);
      AMoreSpaceOnLeft := AOwnerScreenBounds.Left - ADesktopWorkArea.Left > ADesktopWorkArea.Right - AOwnerScreenBounds.Right;
      case FAlignHorz of
        pahLeft:
          if (APosition.X < ADesktopWorkArea.Left) and not AMoreSpaceOnLeft then
            FAlignHorz := pahRight;
        pahRight:
          if (APosition.X + ASize.cx > ADesktopWorkArea.Right) and AMoreSpaceOnLeft then
            FAlignHorz := pahLeft;
      end;
    end;
    InternalCalculateHorizontalDirectionPosition(APosition, AOrigin, ASize);
  end;

  procedure CheckVerticalDirectionPosition;
  var
    AOwnerScreenBounds: TRect;
    ADesktopWorkArea: TRect;
    AMoreSpaceOnTop: Boolean;
  begin
    if not AcceptAnyPosition then
    begin
      AOwnerScreenBounds := OwnerScreenBounds;
      ADesktopWorkArea := GetDesktopWorkArea(AOrigin);
      AMoreSpaceOnTop := AOwnerScreenBounds.Top - ADesktopWorkArea.Top > ADesktopWorkArea.Bottom - AOwnerScreenBounds.Bottom;
      case FAlignVert of
        pavTop:
          if (APosition.Y < ADesktopWorkArea.Top) and not AMoreSpaceOnTop then
            FAlignVert := pavBottom;
        pavBottom:
          if (APosition.Y + ASize.cy > ADesktopWorkArea.Bottom) and AMoreSpaceOnTop then
            FAlignVert := pavTop;
      end;
    end;
    InternalCalculateVerticalDirectionPosition(APosition, AOrigin, ASize);
  end;

begin
  if FDirection = pdHorizontal then
    CheckHorizontalDirectionPosition
  else
    CheckVerticalDirectionPosition;
  CorrectPositionWithDesktopWorkArea(APosition, AOrigin, ASize);
end;

procedure TcxPopupWindow.CorrectPositionWithDesktopWorkArea(
  var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);
var
  ADesktopWorkArea: TRect;
begin
  ADesktopWorkArea := GetDesktopWorkArea(AOrigin);
  if (FDirection = pdVertical) or (FAlignHorz = pahCenter) then
  begin
    if APosition.X + ASize.cx > ADesktopWorkArea.Right then
      APosition.X := ADesktopWorkArea.Right - ASize.cx;
    if APosition.X < ADesktopWorkArea.Left then
      APosition.X := ADesktopWorkArea.Left;
  end;
  if (FDirection = pdHorizontal) or (FAlignVert = pavCenter) then
  begin
    if APosition.Y + ASize.cy > ADesktopWorkArea.Bottom then
      APosition.Y := ADesktopWorkArea.Bottom - ASize.cy;
    if APosition.Y < ADesktopWorkArea.Top then
      APosition.Y := ADesktopWorkArea.Top;
  end;
end;

function TcxPopupWindow.GetBorderWidth(ABorder: TcxBorder): Integer;
begin
  Result := FBorderSpace + FrameWidths[ABorder];
end;

function TcxPopupWindow.GetClientBounds: TRect;
var
  ABorder: TcxBorder;
begin
  Result := ClientRect;
  for ABorder := Low(ABorder) to High(ABorder) do
    case ABorder of
      bLeft:
        Inc(Result.Left, BorderWidths[ABorder]);
      bTop:
        Inc(Result.Top, BorderWidths[ABorder]);
      bRight:
        Dec(Result.Right, BorderWidths[ABorder]);
      bBottom:
        Dec(Result.Bottom, BorderWidths[ABorder]);
    end;
end;

function TcxPopupWindow.GetDefaultAlignHorz: TcxPopupAlignHorz;
begin
  Result := pahLeft;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertPopupAlignHorz(Result);
end;

function TcxPopupWindow.GetFrameWidth(ABorder: TcxBorder): Integer;
begin
  case FBorderStyle of
    pbsUltraFlat:
      Result := 1;
    pbsFlat:
      Result := 1;
    pbs3D:
      Result := 2;
  else
    Result := 0;
  end;
end;

function TcxPopupWindow.GetOwnerScreenBounds: TRect;
begin
  Result := OwnerBounds;
  if UseOwnerParentToGetScreenBounds then
  begin
    if IsOwnerBoundsRTLDependent and (OwnerParent is TWinControl) and
      dxWindowHasRightToLeftLayout(TWinControl(OwnerParent).Handle) then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, OwnerParent.ClientRect);
    Result := cxRectSetOrigin(Result, OwnerParent.ClientToScreen(Result.TopLeft));
  end;
end;

procedure TcxPopupWindow.InitPopup;
begin
  if FOwnerParent <> nil then
    Font := TControlAccess(FOwnerParent).Font;
end;

function TcxPopupWindow.IsSkinnable: Boolean;
begin
  Result := False;
end;

procedure TcxPopupWindow.RestoreControlsBounds;
var
  I: Integer;
  ABounds: TRect;
begin
  if not Adjustable then
    Exit;
  for I := 0 to ControlCount - 1 do
  begin
    ABounds := Controls[I].BoundsRect;
    OffsetRect(ABounds, -BorderWidths[bLeft], -BorderWidths[bTop]);
    Controls[I].BoundsRect := ABounds;
  end;
end;

function TcxPopupWindow.UseOwnerParentToGetScreenBounds: Boolean;
begin
  Result := True;
end;

procedure TcxPopupWindow.DrawFrame;
var
  R: TRect;

  procedure DrawUltraFlatBorder;
  begin
    Canvas.FrameRect(R, FrameColor);
  end;

  procedure DrawFlatBorder;
  begin
    Canvas.DrawEdge(R, False, False);
  end;

  procedure Draw3DBorder;
  begin
    Canvas.DrawEdge(R, False, True);
    InflateRect(R, -1, -1);
    Canvas.DrawEdge(R, False, False);
  end;

begin
  R := Bounds(0, 0, Width, Height);
  case FBorderStyle of
    pbsUltraFlat:
      DrawUltraFlatBorder;
    pbsFlat:
      DrawFlatBorder;
    pbs3D:
      Draw3DBorder;
  end;
end;

procedure TcxPopupWindow.CloseUp;
begin
  Hide;
end;

procedure TcxPopupWindow.Popup;
var
  ASize: TSize;
  P: TPoint;
begin
  InitPopup;
  ASize := CalculateSize;
  P := CalculatePosition(ASize);
  SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
  Show;
end;

{ TcxCustomDragImage }

constructor TcxCustomDragImage.Create;
begin
  inherited Create(nil);
  FCanvas := TcxCanvas.Create(inherited Canvas);
  SetBounds(0, 0, 0, 0);
  AlphaBlend := True;
  AlphaBlendValue := cxDragAndDropWindowTransparency;
end;

destructor TcxCustomDragImage.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

function TcxCustomDragImage.GetAlphaBlended: Boolean;
begin
  Result := FAlphaBlend and Assigned(SetLayeredWindowAttributes);
end;

function TcxCustomDragImage.GetVisible: Boolean;
begin
  Result := IsControlVisible(Self);
end;

procedure TcxCustomDragImage.SetPopupParent(const Value: TWinControl);
begin
  if FPopupParent <> Value then
  begin
    FPopupParent := Value;
    RecreateWnd;
  end;
end;

procedure TcxCustomDragImage.SetTransparentColor(Value: Boolean);
begin
  FTransparentColor := Value;
end;

procedure TcxCustomDragImage.SetTransparentColorValue(Value: TColor);
begin
  FTransparentColorValue := Value;
end;

procedure TcxCustomDragImage.SetVisible(Value: Boolean);
begin
  if Visible <> Value then
    if Value then
      Show
    else
      Hide;
end;

procedure TcxCustomDragImage.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TcxCustomDragImage.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TcxCustomDragImage.CreateParams(var Params: TCreateParams);

  function GetMainWindow: HWND;
  begin
    if Application.MainFormOnTaskbar then
    begin
      if Assigned(Application.MainForm) and Application.MainForm.HandleAllocated then
        Result := Application.MainForm.Handle
      else
        Result := 0;
    end
    else
      Result := Application.Handle;
  end;

begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := WS_EX_TOPMOST;
  if not (csDesigning in ComponentState) and Assigned(SetLayeredWindowAttributes) then
    if FAlphaBlend or FTransparentColor then
      Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;

  if FPopupParent <> nil then
    Params.WndParent := FPopupParent.Handle
  else
    if Screen.ActiveForm <> nil then
      Params.WndParent := Screen.ActiveForm.Handle
    else
      Params.WndParent := GetMainWindow;
// #AI: see B185455
//  with Params.WindowClass do
//    Style := Style or CS_SAVEBITS;
end;

procedure TcxCustomDragImage.UpdateLayeredAttributes;
const
  cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
  cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
var
  AStyle: Integer;
begin
  if not (csDesigning in ComponentState) and Assigned(SetLayeredWindowAttributes) and HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if FAlphaBlend or FTransparentColor then
    begin
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
      SetLayeredWindowAttributes(Handle, ColorToRGB(FTransparentColorValue),
        FAlphaBlendValue, cUseAlpha[FAlphaBlend] or cUseColorKey[FTransparentColor]);
    end
    else
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
      cxRedrawWindow(Handle, RDW_INVALIDATE);
    end;
  end;
end;

procedure TcxCustomDragImage.Init(const ASourceBounds: TRect; const ASourcePoint: TPoint);

  function CalculatePositionOffset: TPoint;
  begin
    Result.X := ASourcePoint.X - ASourceBounds.Left;
    Result.Y := ASourcePoint.Y - ASourceBounds.Top;
  end;

begin
  Width := ASourceBounds.Right - ASourceBounds.Left;
  Height := ASourceBounds.Bottom - ASourceBounds.Top;
  PositionOffset := CalculatePositionOffset;
end;

function TcxCustomDragImage.IsSkinnable: Boolean;
begin
  Result := False;
end;

procedure TcxCustomDragImage.MoveTo(const APosition: TPoint);
begin
  SetBounds(APosition.X - PositionOffset.X, APosition.Y - PositionOffset.Y, Width, Height);
end;

procedure TcxCustomDragImage.Offset(X, Y: Integer);
begin
  SetBounds(Left + X, Top + Y, Width, Height);
end;

procedure TcxCustomDragImage.Show(ACmdShow: Integer = SW_SHOWNA);
begin
  dxSetZOrder(Handle, HWND_TOPMOST, False, SWP_NOOWNERZORDER);
  ShowWindow(Handle, ACmdShow);
  UpdateLayeredAttributes;
  Update;
end;

procedure TcxCustomDragImage.Hide;
begin
  if HandleAllocated then
    ShowWindow(Handle, SW_HIDE);
end;

procedure TcxCustomDragImage.SetAlphaBlend(Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    UpdateLayeredAttributes;
  end;
end;

procedure TcxCustomDragImage.SetAlphaBlendValue(Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    UpdateLayeredAttributes;
  end;
end;

{ TcxDragImage }

constructor TcxDragImage.Create;
begin
  inherited;
  FImage := TcxAlphaBitmap.Create;
end;

destructor TcxDragImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

function TcxDragImage.GetImageCanvas: TcxCanvas;
begin
  Result := Image.cxCanvas;
end;

function TcxDragImage.GetWindowCanvas: TcxCanvas;
begin
  Result := inherited Canvas;
end;

procedure TcxDragImage.Paint;
begin
  inherited;
  WindowCanvas.Draw(0, 0, Image);
end;

procedure TcxDragImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Image <> nil then
    Image.SetSize(Width, Height);
end;

{ TcxLayeredDragImage }

procedure TcxLayeredDragImage.Invalidate;
begin
  Update;
end;

procedure TcxLayeredDragImage.Paint;
begin
  Update;
end;

procedure TcxLayeredDragImage.Update;
begin
  cxUpdateLayeredWindow(Handle, Image);
end;

procedure TcxLayeredDragImage.UpdateLayeredAttributes;
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
end;

{ TcxSizeFrame }

constructor TcxSizeFrame.Create(AFrameWidth: Integer = 2);
begin
  inherited Create;
  AlphaBlend := False;
  FFrameWidth := AFrameWidth;
  FRegion := TcxRegion.Create;
  Canvas.Brush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
end;

destructor TcxSizeFrame.Destroy;
begin
  FreeAndNil(FRegion);
  inherited;
end;

procedure TcxSizeFrame.Paint;
begin
  Canvas.Canvas.FillRect(ClientRect);
end;

procedure TcxSizeFrame.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HandleAllocated and (AWidth > FrameWidth * 2) and (AHeight > FrameWidth * 2) then
  begin
    if not FillSelection then
      InitializeFrameRegion;
    SetWindowRegion;
  end;
end;

procedure TcxSizeFrame.DrawSizeFrame(const ARect: TRect);
begin
  FRegion.Combine(cxRectBounds(0, 0, cxRectWidth(ARect), cxRectHeight(ARect)), roSet);
  SetBounds(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
end;

procedure TcxSizeFrame.DrawSizeFrame(const ARect: TRect; const ARegion: TcxRegion);
begin
  FRegion.Combine(ARegion, roSet, False);
  SetBounds(ARect.Left, ARect.Top, cxRectWidth(ARect), cxRectHeight(ARect));
end;

procedure TcxSizeFrame.InitializeFrameRegion;

  procedure OffsetFrameRegion(AFrameRegion: TcxRegion; AOffsetX, AOffsetY: Integer);
  var
    ARegion: TcxRegion;
  begin
    ARegion := TcxRegion.Create;
    try
      ARegion.Combine(AFrameRegion, roSet, False);
      ARegion.Offset(AOffsetX, AOffsetY);
      AFrameRegion.Combine(ARegion, roIntersect, False);
    finally
      ARegion.Free;
    end;
  end;

var
  AFrameRegion: TcxRegion;
begin
  AFrameRegion := TcxRegion.Create;
  try
    AFrameRegion.Combine(FRegion, roSet, False);

    OffsetFrameRegion(AFrameRegion, FrameWidth, 0);
    OffsetFrameRegion(AFrameRegion, 0, FrameWidth);
    OffsetFrameRegion(AFrameRegion, -FrameWidth, 0);
    OffsetFrameRegion(AFrameRegion, 0, -FrameWidth);

    FRegion.Combine(AFrameRegion, roSubtract, False);
  finally
    AFrameRegion.Free;
  end;
end;

procedure TcxSizeFrame.SetWindowRegion;
var
  ANewWindowRegion, AOldWindowRegion: TcxRegion;
begin
  ANewWindowRegion := TcxRegion.Create;
  AOldWindowRegion := TcxRegion.Create;
  try
    ANewWindowRegion.Combine(FRegion, roSet, False);
    GetWindowRgn(Handle, AOldWindowRegion.Handle);
    if not ANewWindowRegion.IsEqual(AOldWindowRegion) then
    begin
      SetWindowRgn(Handle, ANewWindowRegion.Handle, True);
      ANewWindowRegion.Handle := 0;
    end;
  finally
    AOldWindowRegion.Free;
    ANewWindowRegion.Free;
  end;
end;

{ TcxDragDropArrow }

constructor TcxDragAndDropArrow.Create(ATransparent: Boolean);
begin
  inherited Create;
  FTransparent := ATransparent;
  FBorderColor := clDefault;
  FColor := clDefault;
  AlphaBlend := False;
  if Transparent then
  begin
    TransparentColorValue := ImageBackColor;
    TransparentColor := True;
  end;
end;

procedure TcxDragAndDropArrow.Init(AOwner: TControl; const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace);
var
  AOrigin: TPoint;
begin
  if AOwner <> nil then
    AOrigin := AOwner.ClientOrigin
  else
    AOrigin := cxNullPoint;

  Init(AOrigin, AAreaBounds, AClientRect, APlace);
end;

procedure TcxDragAndDropArrow.Init(const AOwnerOrigin: TPoint; const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace);

  procedure DrawArrow(AScaleFactor: TdxScaleFactor);
  begin
    Canvas.Brush.Color := ImageBackColor;
    Canvas.FillRect(ClientRect);
    Draw(Canvas, ClientRect, APlace, BorderColor, Color, AScaleFactor);
  end;

  function GetArrowBounds(AScaleFactor: TdxScaleFactor): TRect;
  begin
    Result := CalculateBounds(AAreaBounds, AClientRect, APlace, AScaleFactor, UseRightToLeftAlignment);
    Result := cxRectOffset(Result, AOwnerOrigin);
  end;

  procedure SetArrowRegion(AScaleFactor: TdxScaleFactor);
  var
    AGlyph: TBitmap;
  begin
    AGlyph := GetGlyph(APlace, AScaleFactor);
    try
      SetWindowRgn(Handle, cxCreateRegionFromBitmap(AGlyph, clFuchsia), True);
    finally
      AGlyph.Free;
    end;
  end;

var
  ABounds: TRect;
  AScaleFactor: TdxScaleFactor;
begin
  AScaleFactor := TdxScaleFactor.Create;
  try
    AScaleFactor.Assign(dxGetMonitorDPI(AOwnerOrigin), dxDefaultDPI);

    ABounds := GetArrowBounds(AScaleFactor);
    HandleNeeded;  // so that later CreateHandle won't reset Left and Top
    BoundsRect := ABounds;

    DrawArrow(AScaleFactor);
    if not Transparent then
      SetArrowRegion(AScaleFactor);
  finally
    AScaleFactor.Free;
  end;
end;

class function TcxDragAndDropArrow.CalculateBounds(
  const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := CalculateBounds(AAreaBounds, AClientRect, APlace, AScaleFactor, False);
end;

class function TcxDragAndDropArrow.CalculateBounds(const AAreaBounds, AClientRect: TRect; APlace: TcxArrowPlace;
  AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean): TRect;

  procedure CheckResult;
  begin
    if IsRectEmpty(AClientRect) then
       Exit;
    with AClientRect do
    begin
      Result.Left := Max(Result.Left, Left);
      Result.Right := Max(Result.Right, Left);
      Result.Left := Min(Result.Left, Right - 1);
      Result.Right := Min(Result.Right, Right - 1);

      Result.Top := Max(Result.Top, Top);
      Result.Bottom := Max(Result.Bottom, Top);
      Result.Top := Min(Result.Top, Bottom - 1);
      Result.Bottom := Min(Result.Bottom, Bottom - 1);
    end;
  end;

  procedure CalculateHorizontalArrowBounds(const ASize: TSize);
  begin
    Result.Bottom := Result.Top + 1;
    Result := cxRectCenterVertically(Result, ASize.cy);
    if APlace = apLeft then
    begin
      Result.Right := Result.Left;
      Dec(Result.Left, ASize.cx);
    end
    else
    begin
      Result.Left := Result.Right;
      Inc(Result.Right, ASize.cx);
    end;
  end;

  procedure CalculateVerticalArrowBounds(const ASize: TSize);
  begin
    if AUseRightToLeftAlignment then
      Result.Left := Result.Right - 1
    else
      Result.Right := Result.Left + 1;
    Result := cxRectCenterHorizontally(Result, ASize.cx);
    if APlace = apTop then
    begin
      Result.Bottom := Result.Top;
      Dec(Result.Top, ASize.cy);
    end
    else
    begin
      Result.Top := Result.Bottom;
      Inc(Result.Bottom, ASize.cy);
    end;
  end;

var
  AGlyph: TBitmap;
  AGlyphSize: TSize;
begin
  AGlyph := GetGlyph(APlace, AScaleFactor);
  try
    Result := AAreaBounds;
    CheckResult;
    AGlyphSize := cxSize(AGlyph.Width, AGlyph.Height);
    if APlace in [apLeft, apRight] then
      CalculateHorizontalArrowBounds(AGlyphSize)
    else
      CalculateVerticalArrowBounds(AGlyphSize);
  finally
    AGlyph.Free;
  end;
end;

class function TcxDragAndDropArrow.GetGlyph(APlace: TcxArrowPlace; AScaleFactor: TdxScaleFactor): TBitmap;

  function GetResName(AScaleFactor: Integer): string;
  begin
    Result := 'CX_DROPARROW';
    if AScaleFactor >= 200 then
      Result := Result + '_200'
    else
      if AScaleFactor >= 150 then
        Result := Result + '_150';
  end;

begin
  Result := TcxBitmap.Create;
  Result.LoadFromResourceName(HInstance, GetResName(AScaleFactor.Apply(100)));
  Result.Transparent := True;
  case APlace of
    apLeft:
      cxScreenCanvas.RotateBitmap(Result, raMinus90);
    apRight:
      cxScreenCanvas.RotateBitmap(Result, raPlus90);
    apTop:
      cxScreenCanvas.RotateBitmap(Result, ra180);
  end;
end;

class procedure TcxDragAndDropArrow.Draw(ACanvas: TcxCanvas;
  const ABounds: TRect; APlace: TcxArrowPlace; AScaleFactor: TdxScaleFactor);
begin
  Draw(ACanvas, ABounds, APlace, clDefault, clDefault, AScaleFactor);
end;

class procedure TcxDragAndDropArrow.Draw(ACanvas: TcxCanvas; const ABounds: TRect;
  APlace: TcxArrowPlace; AArrowBorderColor, AArrowColor: TColor; AScaleFactor: TdxScaleFactor);
var
  AGlyph: TBitmap;
begin
  AGlyph := GetGlyph(APlace, AScaleFactor);
  try
    ApplyColors(AGlyph, AArrowBorderColor, AArrowColor);
    ACanvas.Draw(ABounds.Left, ABounds.Top, AGlyph);
  finally
    AGlyph.Free;
  end;
end;

class procedure TcxDragAndDropArrow.ApplyColors(ABitmap: TBitmap; AArrowBorderColor, AArrowColor: TColor);
var
  AColor: TColor;
  AColors: TRGBColors;
  I: Integer;
begin
  if cxColorIsValid(AArrowColor) and cxColorIsValid(AArrowBorderColor) then
  begin
    AArrowColor := ColorToRGB(AArrowColor);
    AArrowBorderColor := ColorToRGB(AArrowBorderColor);

    GetBitmapBits(ABitmap, AColors, True);
    for I := 0 to Length(AColors) - 1 do
    begin
      AColor := dxRGBQuadToColor(AColors[I]);
      if AColor = DragAndDropArrowBorderColor then
        AColors[I] := dxColorToRGBQuad(AArrowBorderColor);
      if AColor = DragAndDropArrowColor then
        AColors[I] := dxColorToRGBQuad(AArrowColor);
    end;
    SetBitmapBits(ABitmap, AColors, True);
  end;
end;

function TcxDragAndDropArrow.GetTransparent: Boolean;
begin
  Result := FTransparent and Assigned(SetLayeredWindowAttributes);
end;

function TcxDragAndDropArrow.GetImageBackColor: TColor;
begin
  Result := clFuchsia;
end;

{ TcxDesignController }

procedure TcxDesignController.DesignerModified(AForm: TCustomForm);
begin
  if not (IsDesignerModifiedLocked or (dsDesignerModifying in FState)) then
  begin
    Include(FState, dsDesignerModifying);
    try
      SetDesignerModified(AForm);
    finally
      Exclude(FState, dsDesignerModifying);
    end;
  end;
end;

function TcxDesignController.IsDesignerModifiedLocked: Boolean;
begin
  Result := FLockDesignerModifiedCount > 0;
end;

procedure TcxDesignController.LockDesignerModified;
begin
  Inc(FLockDesignerModifiedCount);
end;

procedure TcxDesignController.UnLockDesignerModified;
begin
  if FLockDesignerModifiedCount > 0 then
    Dec(FLockDesignerModifiedCount);
end;

{ TcxWindowProcLinkedObject }

constructor TcxWindowProcLinkedObject.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

constructor TcxWindowProcLinkedObject.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

procedure TcxWindowProcLinkedObject.DefaultProc(
  var Message: TMessage);
begin
  TcxWindowProcLinkedObject(Prev).WindowProc(Message);
end;

{ TcxVCLSubclassingHelper }

constructor TcxVCLSubclassingHelper.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TcxVCLSubclassingHelper.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TcxWindowProcLinkedObject.Create(FControl);
end;

procedure TcxVCLSubclassingHelper.RestoreDefaultProc;
begin
  FControl.WindowProc := FDefaultWindowProc;
end;

procedure TcxVCLSubclassingHelper.StoreAndReplaceDefaultProc(AWndProc: TWndMethod);
begin
  FDefaultWindowProc := FControl.WindowProc;
  FControl.WindowProc := AWndProc;
end;

{ TcxWin32SubclassingHelper }

constructor TcxWin32SubclassingHelper.Create(
  AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TcxWin32SubclassingHelper.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := TcxWindowProcLinkedObject.Create(FHandle);
end;

procedure TcxWin32SubclassingHelper.DefaultWndProc(
  var Message: TMessage);
begin
  Message.Result := CallWindowProc(FAPIDefaultWndProc, FHandle,
    Message.Msg, Message.WParam, Message.LParam);
end;

procedure TcxWin32SubclassingHelper.RestoreDefaultProc;
begin
  dxSetWindowProc(FHandle, FAPIDefaultWndProc);
  Classes.FreeObjectInstance(FAPIWndProc);
end;

procedure TcxWin32SubclassingHelper.StoreAndReplaceDefaultProc(AWndProc: TWndMethod);
begin
  FAPIWndProc := Classes.MakeObjectInstance(AWndProc);
  FAPIDefaultWndProc := dxSetWindowProc(FHandle, FAPIWndProc);
  FDefaultWindowProc := DefaultWndProc;
end;

{ TcxWindowProcLinkedObjectList }

constructor TcxWindowProcLinkedObjectList.Create(AControl: TControl);
begin
  inherited Create;
  FHelper := TcxVCLSubclassingHelper.Create(AControl);
  FControl := AControl;
  Initialize;
end;

constructor TcxWindowProcLinkedObjectList.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
  FHelper := TcxWin32SubclassingHelper.Create(AHandle);
  Initialize;
end;

destructor TcxWindowProcLinkedObjectList.Destroy;
begin
  FHelper.RestoreDefaultProc;
  FreeAndNil(FHelper);
  inherited Destroy;
end;

function TcxWindowProcLinkedObjectList.AddProc(AWndProc: TWndMethod): TcxWindowProcLinkedObject;
begin
  Result := TcxWindowProcLinkedObject(inherited Add);
  Result.WindowProc := AWndProc;
end;

function TcxWindowProcLinkedObjectList.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := FHelper.CreateLinkedObject;
end;

procedure TcxWindowProcLinkedObjectList.Initialize;
begin
  FHelper.StoreAndReplaceDefaultProc(WndProc);
  AddProc(FHelper.DefaultWindowProc);
end;

function TcxWindowProcLinkedObjectList.IsEmpty: Boolean;
begin
  Result := Last.Prev = nil;
end;

procedure TcxWindowProcLinkedObjectList.WndProc(
  var Message: TMessage);
begin
  TcxWindowProcLinkedObject(Last).WindowProc(Message);
end;

{ TcxWindowProcController }

constructor TcxWindowProcController.Create;
begin
  inherited Create;
  FWindowProcObjects := TcxObjectList.Create;
end;

destructor TcxWindowProcController.Destroy;
begin
  FreeAndNil(FWindowProcObjects);
  inherited Destroy;
end;

function TcxWindowProcController.Add(AControl: TControl;
  AWndProc: TWndMethod): TcxWindowProcLinkedObject;
var
  AControlWindowProcObjects: TcxWindowProcLinkedObjectList;
begin
  Result := nil;
  if AControl = nil then
    Exit;
  AControlWindowProcObjects := GetControlWindowProcObjects(AControl);
  if AControlWindowProcObjects = nil then
  begin
    AControlWindowProcObjects := TcxWindowProcLinkedObjectList.Create(AControl);
    FWindowProcObjects.Add(AControlWindowProcObjects);
  end;
  Result := TcxWindowProcLinkedObject(AControlWindowProcObjects.AddProc(AWndProc));
end;

function TcxWindowProcController.Add(AHandle: THandle; AWndProc: TWndMethod): TcxWindowProcLinkedObject;
var
  AControlWindowProcObjects: TcxWindowProcLinkedObjectList;
begin
  Result := nil;
  if AHandle = 0 then
    Exit;
  AControlWindowProcObjects := GetControlWindowProcObjects(AHandle);
  if AControlWindowProcObjects = nil then
  begin
    AControlWindowProcObjects := TcxWindowProcLinkedObjectList.Create(AHandle);
    FWindowProcObjects.Add(AControlWindowProcObjects);
  end;
  Result := TcxWindowProcLinkedObject(AControlWindowProcObjects.AddProc(AWndProc));
end;

procedure TcxWindowProcController.Remove(var AWndProcObject: TcxWindowProcLinkedObject);
var
  AControlWindowProcObjects: TcxWindowProcLinkedObjectList;
begin
  if AWndProcObject = nil then
    Exit;
  if AWndProcObject.Control <> nil then
    AControlWindowProcObjects := GetControlWindowProcObjects(AWndProcObject.Control)
  else
    AControlWindowProcObjects := GetControlWindowProcObjects(AWndProcObject.Handle);
  if AControlWindowProcObjects <> nil then
  begin
    AControlWindowProcObjects.Remove(AWndProcObject);
    AWndProcObject := nil;
    if AControlWindowProcObjects.IsEmpty then
      FWindowProcObjects.FreeAndRemove(AControlWindowProcObjects);
  end;
end;

function TcxWindowProcController.GetControlWindowProcObjects(
  AControl: TControl): TcxWindowProcLinkedObjectList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FWindowProcObjects.Count - 1 do
    if TcxWindowProcLinkedObjectList(FWindowProcObjects[I]).Control = AControl then
    begin
      Result := TcxWindowProcLinkedObjectList(FWindowProcObjects[I]);
      Break;
    end;
end;

function TcxWindowProcController.GetControlWindowProcObjects(AHandle: THandle): TcxWindowProcLinkedObjectList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FWindowProcObjects.Count - 1 do
    if TcxWindowProcLinkedObjectList(FWindowProcObjects[I]).Handle = AHandle then
    begin
      Result := TcxWindowProcLinkedObjectList(FWindowProcObjects[I]);
      Break;
    end;
end;

{ TcxLockedStateImageOptions }

constructor TcxLockedStateImageOptions.Create(AOwner: TComponent);
begin
  inherited Create;
  FColor := clNone;
  FOwner := AOwner;
  FFont := TFont.Create;
  FEnabled := True;
  FEffect := lsieNone;
  FText := cxGetResourceString(@scxLockedStateText);
end;

destructor TcxLockedStateImageOptions.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TcxLockedStateImageOptions.AfterConstruction;
begin
  inherited AfterConstruction;
  FFont.Assign(GetFont);
  FFont.OnChange := FontChanged;
end;

procedure TcxLockedStateImageOptions.Assign(Source: TPersistent);
begin
  if Source is TcxLockedStateImageOptions then
  begin
    Color := TcxLockedStateImageOptions(Source).Color;
    Effect := TcxLockedStateImageOptions(Source).Effect;
    Enabled := TcxLockedStateImageOptions(Source).Enabled;
    Font := TcxLockedStateImageOptions(Source).Font;
    ShowText := TcxLockedStateImageOptions(Source).ShowText;
    Text := TcxLockedStateImageOptions(Source).Text;
  end
  else
    inherited Assign(Source);
end;

procedure TcxLockedStateImageOptions.SetAssignedValues(const Value: TcxLockedStateImageAssignedValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    if not (lsiavFont in AssignedValues) then
    begin
      FFont.Assign(GetFont);
      Exclude(FAssignedValues, lsiavFont);
    end;
    if not (lsiavColor in AssignedValues) then
      FColor := clNone;
  end;
end;

procedure TcxLockedStateImageOptions.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Include(FAssignedValues, lsiavColor);
  end;
end;

procedure TcxLockedStateImageOptions.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TcxLockedStateImageOptions.FontChanged(Sender: TObject);
begin
  Include(FAssignedValues, lsiavFont);
end;

function TcxLockedStateImageOptions.IsFontStored: Boolean;
begin
  Result := lsiavFont in FAssignedValues;
end;

procedure TcxLockedStateImageOptions.ChangeScale(M, D: Integer);
begin
  if lsiavFont in AssignedValues then
  begin
  {$IFDEF DELPHIXE8}
    Font.Height := MulDiv(Font.Height, M, D);
  {$ELSE}
    Font.Size := MulDiv(Font.Size, M, D);
  {$ENDIF}
  end;
end;

function TcxLockedStateImageOptions.IsTextStored: Boolean;
begin
  Result := FText <> cxGetResourceString(@scxLockedStateText);
end;

procedure TcxLockedStateImageOptions.UpdateFont(AFont: TFont);
begin
  if not (lsiavFont in AssignedValues) then
  begin
    FFont.Assign(AFont);
    AssignedValues := AssignedValues - [lsiavFont];
  end;
end;

{ TcxLockedStatePaintHelper }

constructor TcxLockedStatePaintHelper.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TcxLockedStatePaintHelper.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TcxLockedStatePaintHelper.AfterDestroyingImage;
begin
end;

procedure TcxLockedStatePaintHelper.BeforeCreatingImage;
begin
end;

procedure TcxLockedStatePaintHelper.BeginLockedPaint(AMode: TcxLockedStateImageShowingMode);
begin
  Inc(FCount);
  if FCount = 1 then
  begin
    if (AMode <> lsimNever) and CanControlPaint and CanCreateLockedImage then
    begin
      BeforeCreatingImage;
      CreateImage;
      if AMode = lsimImmediate then
        cxRedrawWindow(Control.Handle, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
    end;
  end;
end;

procedure TcxLockedStatePaintHelper.EndLockedPaint;
begin
  Dec(FCount);
  if FCount = 0 then
  begin
    if FBitmap <> nil then
    begin
      FreeAndNil(FBitmap);
      AfterDestroyingImage;
    end;
    if FIsImageReady then
    begin
      FIsImageReady := False;
      if CanControlPaint then
        Control.InvalidateWithChildren;
    end;
  end;
end;

function TcxLockedStatePaintHelper.GetActiveCanvas: TcxCanvas;
begin
  if IsImageReady then
    Result := Bitmap.cxCanvas
  else
    Result := Control.ActiveCanvas;
end;

function TcxLockedStatePaintHelper.CanCreateLockedImage: Boolean;
begin
  Result := Assigned(Options) and Options.Enabled and
    ([csLoading, csDestroying] * Owner.ComponentState = []);
end;

function TcxLockedStatePaintHelper.CanControlPaint: Boolean;
begin
  Result := Control.HandleAllocated and Control.Visible;
end;

function TcxLockedStatePaintHelper.DoPrepareImage: Boolean;
begin
  Result := False;
end;

procedure TcxLockedStatePaintHelper.CreateImage;
begin
  if Assigned(FBitmap) then
    FBitmap.SetSize(Control.ClientRect)
  else
    FBitmap := TcxBitmap32.CreateSize(Control.ClientRect);
  FCreatingImage := True;
  try
    cxPaintToBitmap(Control, FBitmap);
    Control.Update;
  finally
    FCreatingImage := False;
  end;
end;

procedure TcxLockedStatePaintHelper.DrawText;
var
  H: Integer;
  R: TRect;
  ASize: TSize;
begin
  if not Options.ShowText or (Text = '') then Exit;
  R := Bitmap.ClientRect;
  ASize := cxTextExtent(Font, Text);
  H := ASize.cy;
  Inc(ASize.cx, H * 3);
  Inc(ASize.cy, H * 2);
  R := cxRectCenter(Bitmap.ClientRect, ASize);
  if cxRectContain(Bitmap.ClientRect, R) then
    Painter.DrawMessageBox(Bitmap.cxCanvas, R, Text, Font, Color);
end;

function TcxLockedStatePaintHelper.GetImage: TcxBitmap32;
begin
  if IsActive then
  begin
    ValidateImage;
    Result := Bitmap;
  end
  else
    Result := nil;
end;

function TcxLockedStatePaintHelper.GetColor: TColor;
begin
  Result := Options.Color;
end;

function TcxLockedStatePaintHelper.GetEffect: TcxLockedStateImageEffect;
begin
  Result := Options.Effect;
end;

function TcxLockedStatePaintHelper.GetFont: TFont;
begin
  Result := Options.Font;
end;

function TcxLockedStatePaintHelper.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Control.LookAndFeelPainter;
end;

function TcxLockedStatePaintHelper.GetText: string;
begin
  Result := Options.Text;
end;

function TcxLockedStatePaintHelper.IsActive: Boolean;
begin
  Result := Assigned(Bitmap) and not CreatingImage;
end;

procedure TcxLockedStatePaintHelper.PrepareImage;
begin
  if not DoPrepareImage then
  begin
    case Effect of
      lsieLight: Bitmap.Lighten(40);
      lsieDark: Bitmap.Darken(60);
    end;
    DrawText;
  end;
end;

procedure TcxLockedStatePaintHelper.ValidateImage;
begin
  if not FIsImageReady then
  try
    PrepareImage;
  finally
    FIsImageReady := True;
  end;
end;

{ TdxControlsDesignSelectorHelper }

constructor TdxControlsDesignSelectorHelper.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  FControl := AOwner as TControl;
  FSelectorBounds := cxInvalidRect;
  FChildren := TcxComponentList.Create(True);
  StoreWndProc;
  Control.FreeNotification(Self);
end;

destructor TdxControlsDesignSelectorHelper.Destroy;
begin
  Control.RemoveFreeNotification(Self);
  RestoreWndProc;
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxControlsDesignSelectorHelper.Assign(Source: TPersistent);
var
  AItem: TdxControlsDesignSelectorHelper;
begin
  if Source is TdxControlsDesignSelectorHelper then
  begin
    AItem := Source as TdxControlsDesignSelectorHelper;
    FSelectorBounds := AItem.GetSelectorBoundsForChild(Self);
    CheckChildren;
  end
  else
    inherited Assign(Source);
end;

class function TdxControlsDesignSelectorHelper.CalculateBounds(
  const R: TRect; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := CalculateBounds(R, cxDesignSelectorIndentFromBorder, cxDesignSelectorSize, AScaleFactor);
end;

class function TdxControlsDesignSelectorHelper.CalculateBounds(
  const R: TRect; AOffset, ASize: Integer; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := cxRectInflate(R, -AScaleFactor.Apply(AOffset));
  Result.Left := Result.Right - AScaleFactor.Apply(ASize);
  Result.Top := Result.Bottom - AScaleFactor.Apply(ASize);
end;

procedure TdxControlsDesignSelectorHelper.ControlWndProc(var Message: TMessage);

  procedure InternalPaint(DC: HDC);
  var
    ADC: HDC;
  begin
    if not IsActiveDesignSelector then
      Exit;
    if DC <> 0 then
      DrawDesignSelector(DC)
    else
      if IsWinControl then
      begin
        ADC := GetWindowDC(ControlWnd);
        DrawDesignSelector(ADC);
        ReleaseDC(ControlWnd, ADC);
      end;
  end;

begin
  if not DoControlWndProc(Message) then
    CallDefaultWndProc(Message);
  case Message.Msg of
    CM_CONTROLCHANGE:
      CheckChildren;
    WM_PARENTNOTIFY:
      if Message.WParamLo in [WM_CREATE, WM_DESTROY] then
        CheckChildren;
    WM_PAINT, WM_PRINTCLIENT:
      InternalPaint(TWMPaint(Message).DC);
    WM_WINDOWPOSCHANGED:
      if Parent <> nil then
        SelectorBounds := Parent.GetSelectorBoundsForChild(Self);
  end;
end;

function TdxControlsDesignSelectorHelper.DoControlWndProc(var Message: TMessage): Boolean;
begin
  Result := False;
  case Message.Msg of
    WM_NCHITTEST:
      if IsActiveDesignSelector then
        if IsHitTestTransparent(ScreenToClient(Point(Message.LParamLo, Message.LParamHi))) then
        begin
          Message.Result := HTTRANSPARENT;
          Result := True;
        end;
    CM_HITTEST:
      if IsActiveDesignSelector then
        if PointInSelectorBounds(Point(Message.LParamLo, Message.LParamHi)) then
        begin
          Message.Result := HTNOWHERE;
          Result := True;
        end;
    end;
end;

function TdxControlsDesignSelectorHelper.IsHitTestTransparent(const P: TPoint): Boolean;
begin
  Result := PointInSelectorBounds(P);
end;

procedure TdxControlsDesignSelectorHelper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) and (Parent <> nil) then
    Parent.DestroyChild(Self);
end;

procedure TdxControlsDesignSelectorHelper.CallDefaultWndProc(var Message: TMessage);
begin
  FWindowProcObject.DefaultProc(Message);
end;

procedure TdxControlsDesignSelectorHelper.CheckChildren;

  function GetChild(AControl: TControl): TdxControlsDesignSelectorHelper;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to FChildren.Count - 1 do
      if (FChildren[I] as TdxControlsDesignSelectorHelper).Control = AControl then
      begin
        Result := FChildren[I] as TdxControlsDesignSelectorHelper;
        Break;
      end;
    if Result = nil then
      Result := GetChildClass.Create(AControl);
  end;

  procedure TryAddChild(AControl: TControl); overload;
  var
    AIntf: IdxCustomizeControlsHelper;
  begin
    if (csDestroying in AControl.ComponentState) or ((Supports(AControl, IdxCustomizeControlsHelper, AIntf) and AIntf.CanProcessChildren)) or
        not (AControl is TWinControl) or not (AControl as TWinControl).HandleAllocated then
      Exit;

    PrepareChild(GetChild(AControl));
  end;

var
  I: Integer;
  AIntf: IdxCustomizeControlsHelper;
begin
  for I := 0 to FChildren.Count - 1 do
    (FChildren[I] as TdxControlsDesignSelectorHelper).FChecked := False;
  if IsWinControl and ControlAsWinControl.HandleAllocated and (not Supports(Control, IdxCustomizeControlsHelper, AIntf) or not AIntf.CanProcessChildren) then
  begin
    for I := 0 to ControlAsWinControl.ControlCount - 1 do
      TryAddChild(ControlAsWinControl.Controls[I]);
  end;
  for I := FChildren.Count - 1 downto 0 do
    if not (FChildren[I] as TdxControlsDesignSelectorHelper).FChecked then
      FChildren[I].Free;
end;

procedure TdxControlsDesignSelectorHelper.DestroyChild(AChild: TdxControlsDesignSelectorHelper);
begin
  AChild.Free;
  if not (csDestroying in ComponentState) then
    CheckChildren;
end;

function TdxControlsDesignSelectorHelper.GetChildClass: TdxControlsDesignSelectorHelperClass;
begin
  Result := TdxControlsDesignSelectorHelper;
end;

procedure TdxControlsDesignSelectorHelper.PrepareChild(AItem: TdxControlsDesignSelectorHelper);
begin
  if FChildren.IndexOf(AItem) = -1 then
    FChildren.Add(AItem);
  AItem.Parent := Self;
  AItem.Assign(Self);
  AItem.FChecked := True;
end;

function TdxControlsDesignSelectorHelper.GetSelectorBoundsForChild(AChild: TdxControlsDesignSelectorHelper): TRect;
var
  P: TPoint;
begin
  Result := SelectorBounds;
  P := ScreenToClient(AChild.ClientToScreen(cxNullPoint));
  Result := cxRectOffset(Result, P, False);
end;

function TdxControlsDesignSelectorHelper.ClientToScreen(const P: TPoint): TPoint;
begin
  Result := Control.ClientToScreen(P);
end;

function TdxControlsDesignSelectorHelper.ScreenToClient(const P: TPoint): TPoint;
begin
  Result := Control.ScreenToClient(P);
end;

function TdxControlsDesignSelectorHelper.IsActiveDesignSelector: Boolean;
begin
  Result := IsValid;
end;

function TdxControlsDesignSelectorHelper.IsSelected: Boolean;
begin
  Result := ((Parent = nil) and (cxDesignHelper <> nil) and cxDesignHelper.IsObjectSelected(Control, Control)) or
    ((Parent <> nil) and Parent.IsSelected);
end;

function TdxControlsDesignSelectorHelper.IsValid: Boolean;
begin
  Result := not (csDestroying in ComponentState) and
    (Control <> nil) and not (csDestroying in Control.ComponentState);
end;

function TdxControlsDesignSelectorHelper.IsWinControl: Boolean;
begin
  Result := Control is TWinControl;
end;

function TdxControlsDesignSelectorHelper.ControlAsWinControl: TWinControl;
begin
  Result := Control as TWinControl;
end;

function TdxControlsDesignSelectorHelper.CanDrawDesignSelector: Boolean;
begin
  Result := IsActiveDesignSelector and ((Parent = nil) or (IsWinControl and IsWindowVisible(ControlWnd)));
end;

procedure TdxControlsDesignSelectorHelper.DoDrawDesignSelector(DC: HDC);
var
  R: TRect;
  P: TPoint;
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    SelectClipRgn(cxPaintCanvas.Handle, 0);
    R := SelectorBounds;
    if (Control <> nil) and not (csPaintCopy in Control.ControlState) and (Control.Parent <> nil) then
    begin
      P := Control.Parent.ScreenToClient(Control.ClientToScreen(cxNullPoint));
      P := cxPointOffset(P, Control.BoundsRect.TopLeft, False);
      R := cxRectOffset(R, P);
    end;
    cxDrawDesignRect(cxPaintCanvas, R, IsSelected);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxControlsDesignSelectorHelper.DrawDesignSelector(DC: HDC);
begin
  if CanDrawDesignSelector then
    DoDrawDesignSelector(DC);
end;

procedure TdxControlsDesignSelectorHelper.StoreWndProc;
begin
  if Control <> nil then
    FWindowProcObject := cxWindowProcController.Add(Control, ControlWndProc);
end;

procedure TdxControlsDesignSelectorHelper.RestoreWndProc;
begin
  if Assigned(FWindowProcObject) then
    cxWindowProcController.Remove(FWindowProcObject);
end;

function TdxControlsDesignSelectorHelper.GetControlWnd: THandle;
begin
  if IsWinControl and ControlAsWinControl.HandleAllocated then
    Result := ControlAsWincontrol.Handle
  else
    Result := 0;
end;

function TdxControlsDesignSelectorHelper.GetParentControl: TWinControl;
begin
  Result := Control.Parent;
end;

procedure TdxControlsDesignSelectorHelper.SetSelectorBounds(const AValue: TRect);
begin
  if not EqualRect(AValue, FSelectorBounds) then
  begin
    FSelectorBounds := AValue;
    CheckChildren;
    if Control.Visible then
      Control.Invalidate;
  end;
end;

function TdxControlsDesignSelectorHelper.PointInSelectorBounds(const P: TPoint): Boolean;
begin
  Result := cxRectPtIn(SelectorBounds, P);
end;

//

procedure RegisterAssistants;
begin
  if IsWinVistaOrLater then
    BufferedPaintInit;
end;

procedure UnregisterAssistants;
begin
  if IsWinVistaOrLater then
    BufferedPaintUnInit;
end;

procedure cxControlWndProcHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  ACWPStruct: TCWPStruct;
begin
  if ACode <> HC_ACTION then
    Exit;

  ACWPStruct := Windows.PCWPStruct(LParam)^;
  if (ACWPStruct.message = WM_MOVE) or (ACWPStruct.message = WM_SIZE) or
   (ACWPStruct.message = WM_MDIACTIVATE) or (ACWPStruct.message = WM_MDIRESTORE) then
  begin
    TdxHybridScrollbarManagers.CheckScrollbarsPosition(ACWPStruct.hwnd);
    TdxTouchScrollUIModeManager.CheckUIPosition(ACWPStruct.hwnd);
  end
  else
    if (ACWPStruct.message = WM_ENABLE) and (ACWPStruct.lParam = 0) or
      (ACWPStruct.message = WM_WINDOWPOSCHANGED) and
      ((Windows.PWindowPos(ACWPStruct.lParam).flags and SWP_HIDEWINDOW) <> 0) or (ACWPStruct.message = WM_CLOSE) or
     (ACWPStruct.message = WM_SYSCOMMAND) and (ACWPStruct.wParam and $FFF0 = SC_MINIMIZE) then
    begin
      TdxHybridScrollbarManagers.CheckScrollbarsVisibility(ACWPStruct.hwnd);
      TdxTouchScrollUIModeManager.CheckUIVisibility(ACWPStruct.hwnd);
    end;
end;

{ TdxRightToLeftLayoutConverter }

class function TdxRightToLeftLayoutConverter.ConvertAlignment(AAlignment: TLeftRight): TLeftRight;
begin
  Result := AAlignment;
  case Result of
    taLeftJustify:  Result := taRightJustify;
    taRightJustify: Result := taLeftJustify;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertAnchors(const AAnchors: TAnchors): TAnchors;
var
  AIntersection: TAnchors;
begin
  Result := AAnchors;
  AIntersection := [akLeft, akRight] * Result;
  if (AIntersection = [akLeft]) or (AIntersection = [akRight]) then
    if AIntersection = [akLeft] then
    begin
      Exclude(Result, akLeft);
      Include(Result, akRight);
    end
    else
    begin
      Exclude(Result, akRight);
      Include(Result, akLeft);
    end;
end;

class function TdxRightToLeftLayoutConverter.ConvertArrowDirection(ADirection: TcxArrowDirection): TcxArrowDirection;
begin
  Result := ADirection;
  case Result of
    adLeft:  Result := adRight;
    adRight: Result := adLeft;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertcxDrawTextAlignment(AAlignment: Integer): Integer;
begin
  Result := AAlignment;
  case Result of
    cxAlignLeft:  Result := cxAlignRight;
    cxAlignRight: Result := cxAlignLeft;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertcxTextAlignX(AAlign: TcxTextAlignX): TcxTextAlignX;
begin
  Result := AAlign;
  case Result of
    taLeft:  Result := taRight;
    taRight: Result := taLeft;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertBorders(const ABorders: TcxBorders): TcxBorders;
var
  AIntersection: TcxBorders;
begin
  Result := ABorders;
  AIntersection := [bLeft, bRight] * Result;
  if (AIntersection = [bLeft]) or (AIntersection = [bRight]) then
    if AIntersection = [bLeft] then
    begin
      Exclude(Result, bLeft);
      Include(Result, bRight);
    end
    else
    begin
      Exclude(Result, bRight);
      Include(Result, bLeft);
    end;
end;

class function TdxRightToLeftLayoutConverter.ConvertDirection(ADirection: TcxDirection): TcxDirection;
begin
  Result := ADirection;
  case Result of
    dirLeft:  Result := dirRight;
    dirRight: Result := dirLeft;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertPoint(const APoint: TPoint; const AParentRect: TRect): TPoint;
begin
  Result.X := ConvertXCoordinate(APoint.X, AParentRect);
  Result.Y := APoint.Y;
end;

class function TdxRightToLeftLayoutConverter.ConvertPopupAlignHorz(
  APopupAlignHorz: TcxPopupAlignHorz): TcxPopupAlignHorz;
begin
  case APopupAlignHorz of
    pahLeft:
      Result := pahRight;
    pahRight:
      Result := pahLeft
  else
    Result := APopupAlignHorz;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertPopupAlignment(AAlignment: TPopupAlignment): TPopupAlignment;
begin
  case AAlignment of
    paLeft:
      Result := paRight;
    paRight:
      Result := paLeft
  else
    Result := AAlignment;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertNeighbors(const ANeighbors: TcxNeighbors): TcxNeighbors;
var
  AIntersection: TcxNeighbors;
begin
  Result := ANeighbors;
  AIntersection := [nLeft, nRight] * Result;
  if (AIntersection = [nLeft]) or (AIntersection = [nRight]) then
    if AIntersection = [nLeft] then
    begin
      Exclude(Result, nLeft);
      Include(Result, nRight);
    end
    else
    begin
      Exclude(Result, nRight);
      Include(Result, nLeft);
    end;
end;

class function TdxRightToLeftLayoutConverter.ConvertRect(const ARect, AParentRect: TRect): TRect;
begin
  Result := ARect;
  if not Result.IsZero then
  begin
    Result.Left := AParentRect.Left + AParentRect.Right - ARect.Right;
    Result.Right := Result.Left + ARect.Width;
  end;
end;

class function TdxRightToLeftLayoutConverter.ConvertXCoordinate(const X: Integer; const AParentRect: TRect): Integer;
begin
  Result := AParentRect.Left + AParentRect.Right - X;
end;

class function TdxRightToLeftLayoutConverter.ConvertOffsets(const AOffsets: TRect): TRect;
begin
  Result := AOffsets;
  Result.Left := AOffsets.Right;
  Result.Right := AOffsets.Left;
end;

class function TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(AKey: Word): Word;
begin
  case AKey of
    VK_LEFT:
      Result := VK_RIGHT;
    VK_RIGHT:
      Result := VK_LEFT;
  else
    Result := AKey;
  end;
end;

{ TdxSemitransparentDragHelper }

constructor TdxSemitransparentDragHelper.Create(AControl: TcxControl; AAlpha: Byte);
begin
  FControl := AControl;
  FAlpha := AAlpha;
end;

destructor TdxSemitransparentDragHelper.Destroy;
begin
  EndSpecialPaint;
  inherited Destroy;
end;

procedure TdxSemiTransparentDragHelper.AfterSelectionPaint(ACanvas: TcxCanvas);
begin
end;

procedure TdxSemitransparentDragHelper.AfterSelectionPaint(AGraphics: TdxGPGraphics);
begin
end;

procedure TdxSemitransparentDragHelper.BeginSpecialPaint;
begin
  SelectionsBounds := GetSelectionBounds;
  CreateBackgroundBitmap;
  FSpecialControlPainting := True;
end;

procedure TdxSemitransparentDragHelper.EndSpecialPaint;
begin
  FSpecialControlPainting := False;
  FreeAndNil(FSelectionBitmap);
  FreeAndNil(FBackgroundBitmap);
  FControl.Invalidate;
end;

procedure TdxSemitransparentDragHelper.CreateBackgroundBitmap;
begin
  FreeAndNil(FBackgroundBitmap);
  FBackgroundBitmap := TcxBitmap32.CreateSize(FControl.ClientRect);
  FControl.PaintTo(FBackgroundBitmap.Canvas, 0, 0);
end;

procedure TdxSemitransparentDragHelper.CreateSelectionBitmap;
begin
  FreeAndNil(FSelectionBitmap);
  FSelectionBitmap := TcxBitmap32.CreateSize(FSelectionsBounds);
end;

procedure TdxSemitransparentDragHelper.Offset(X, Y: Integer);
begin
  FSelectionsBounds.Offset(X, Y);
  FControl.Invalidate;
end;

function TdxSemitransparentDragHelper.GetOpaqueBits(ABitmap: TcxBitmap32): TRGBColors;
var
  I: Integer;
  B: PByte;
  AEnd: PByte;
begin
  GetBitmapBits(ABitmap, Result, True);
  I := Length(Result);
  B := @Result[0];
  Inc(B, 3);
  AEnd := B;
  Inc(AEnd, (I shr 2) shl 4);
  while B < AEnd do
  begin
    B[0] := $FF;
    B[4] := $FF;
    B[8] := $FF;
    B[12] := $FF;
    Inc(B, 4 * 4);
  end;
  Inc(AEnd, (I and 3) shl 2);
  while B < AEnd do
  begin
    B^ := $FF;
    Inc(B, 4);
  end;
end;

procedure TdxSemitransparentDragHelper.PaintSelection(ACanvas: TcxCanvas);
begin
  ACanvas.SaveDC;
  try
    ACanvas.WindowOrg := FPaintOffset;
    PaintControlSelection(ACanvas);
  finally
    ACanvas.RestoreDC;
  end;
end;

function TdxSemitransparentDragHelper.GetSelectionImage: TdxGPImage;
var
  AColors: TRGBColors;
begin
  cxBitBlt(FSelectionBitmap.cxCanvas.Handle, FBackgroundBitmap.cxCanvas.Handle, FSelectionBitmap.ClientRect, FSelectionsBounds.TopLeft, SRCCOPY);
  PaintSelection(FSelectionBitmap.cxCanvas);
  AColors := GetOpaqueBits(FSelectionBitmap);
  Result := TdxGPImage.CreateFromBits(FSelectionBitmap.Width, FSelectionBitmap.Height, AColors, True);
end;

procedure TdxSemitransparentDragHelper.PaintControl(ACanvas: TcxCanvas);
var
  AGraphics: TdxGPGraphics;
  AImage: TdxGPImage;
begin
  cxBitBlt(ACanvas.Handle, FBackgroundBitmap.cxCanvas.Handle, FBackgroundBitmap.ClientRect, TPoint.Null, SRCCOPY);
  AGraphics := dxGpBeginPaint(ACanvas.Handle, FBackgroundBitmap.ClientRect);
  try
    AGraphics.InterpolationMode := dxGpSmoothStretchModeMap[False];
    AGraphics.PixelOffsetMode := PixelOffsetModeHighSpeed;
    AImage := GetSelectionImage;
    try
      AGraphics.Draw(AImage, FSelectionsBounds, FAlpha);
    finally
      AImage.Free;
    end;
    AfterSelectionPaint(AGraphics);
  finally
    dxGpEndPaint(AGraphics);
  end;
  AfterSelectionPaint(ACanvas);
end;

procedure TdxSemiTransparentDragHelper.Resize(const ANewSelectionBounds: TRect);
begin
  SelectionsBounds := ANewSelectionBounds;
  Control.Invalidate;
end;

procedure TdxSemiTransparentDragHelper.Resize;
begin
  Resize(GetSelectionBounds);
end;

procedure TdxSemiTransparentDragHelper.SetSelectionsBounds(const Value: TRect);
begin
  if FSelectionsBounds.IsEqual(Value) then
    Exit;
  FSelectionsBounds := Value;
  FPaintOffset := FSelectionsBounds.TopLeft;
  CreateSelectionBitmap;
end;

procedure TdxSemiTransparentDragHelper.UpdateBackground;
begin
  if SpecialControlPainting then
  begin
    EndSpecialPaint;
    BeginSpecialPaint;
  end;
end;

initialization
  FUnitIsFinalized := False; // D10 bug
  Screen.Cursors[crDragCopy] := LoadCursor(HInstance, 'CX_DRAGCOPYCURSOR');
  Screen.Cursors[crFullScroll] := LoadCursor(HInstance, 'CX_FULLSCROLLCURSOR');
  Screen.Cursors[crHorScroll] := LoadCursor(HInstance, 'CX_HORSCROLLCURSOR');
  Screen.Cursors[crVerScroll] := LoadCursor(HInstance, 'CX_VERSCROLLCURSOR');
  Screen.Cursors[crUpScroll] := LoadCursor(HInstance, 'CX_UPSCROLLCURSOR');
  Screen.Cursors[crRightScroll] := LoadCursor(HInstance, 'CX_RIGHTSCROLLCURSOR');
  Screen.Cursors[crDownScroll] := LoadCursor(HInstance, 'CX_DOWNSCROLLCURSOR');
  Screen.Cursors[crLeftScroll] := LoadCursor(HInstance, 'CX_LEFTSCROLLCURSOR');
  Screen.Cursors[crcxRemove] := LoadCursor(HInstance, 'CX_REMOVECURSOR');
  Screen.Cursors[crcxVertSize] := LoadCursor(HInstance, 'CX_VERTSIZECURSOR');
  Screen.Cursors[crcxHorzSize] := LoadCursor(HInstance, 'CX_HORZSIZECURSOR');
  Screen.Cursors[crcxDragMulti] := LoadCursor(HInstance, 'CX_MULTIDRAGCURSOR');
  Screen.Cursors[crcxNoDrop] := LoadCursor(HInstance, 'CX_NODROPCURSOR');
  Screen.Cursors[crcxDrag] := LoadCursor(HInstance, 'CX_DRAGCURSOR');
  Screen.Cursors[crcxHandPoint] := LoadCursor(0{HInstance}, IDC_HAND{'CX_HANDPOINTCURSOR'});
  Screen.Cursors[crcxColorPicker] := LoadCursor(HInstance, 'CX_COLORPICKERCURSOR');
  Screen.Cursors[crcxMultiDragCopy] := LoadCursor(HInstance, 'CX_MULTIDRAGCOPYCURSOR');
  Screen.Cursors[crcxHand] := LoadCursor(HInstance, 'CX_HANDCURSOR');
  Screen.Cursors[crcxHandDrag] := LoadCursor(HInstance, 'CX_HANDDRAGCURSOR');

  Screen.Cursors[crdxLayoutControlNoDrop] := LoadCursor(HInstance, 'DXLAYOUTCONTROLNODROP');
  Screen.Cursors[crdxLayoutControlRemove] := LoadCursor(HInstance, 'DXLAYOUTCONTROLREMOVE');
  Screen.Cursors[crdxLayoutControlDropAfter] := LoadCursor(HInstance, 'DXLAYOUTCONTROLDROPAFTER');
  Screen.Cursors[crdxLayoutControlDropBefore] := LoadCursor(HInstance, 'DXLAYOUTCONTROLDROPBEFORE');
  Screen.Cursors[crdxLayoutControlDropInside] := LoadCursor(HInstance, 'DXLAYOUTCONTROLDROPINSIDE');
  Screen.Cursors[crcxDropAfterCopy] := LoadCursor(HInstance, 'CXDROPAFTERCOPY');
  Screen.Cursors[crcxDropBeforeCopy] := LoadCursor(HInstance, 'CXDROPBEFORECOPY');
  Screen.Cursors[crcxDropInsideCopy] := LoadCursor(HInstance, 'CXDROPINSIDECOPY');

  //StartClassGroup(TControl);
  GroupDescendentsWith(TcxControlChildComponent, TControl);

  FSystemController := TcxSystemController.Create;
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);
  dxApplicationActivateWindowHelper := TdxApplicationActivateWindowHelper.Create;
  @FUpdateLayeredWindow := GetProcAddress(GetModuleHandle(user32), 'UpdateLayeredWindow');
  dxSetHook(htWndProc, cxControlWndProcHook);

finalization
  dxReleaseHook(cxControlWndProcHook);
  FreeAndNil(dxApplicationActivateWindowHelper);
  FUnitIsFinalized := True;
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

  FreeAndNil(FSystemController);
  FreeAndNil(FDesignController);
  if FMouseTrackingTimerList <> nil then
  begin
    if FMouseTrackingTimerList.Count <> 0 then
      raise EdxException.Create('MouseTrackingTimerList.Count <> 0');
    FreeAndNil(FMouseTrackingTimerList);
  end;

  FreeAndNil(FcxMessageWindow);
  FreeAndNil(FcxWindowProcController);
  FreeAndNil(FMessagesController);
  FreeAndNil(FdxSystemInfo);
end.

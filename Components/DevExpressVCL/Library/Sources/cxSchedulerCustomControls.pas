{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
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

unit cxSchedulerCustomControls;

{$I cxVer.inc}

interface
uses
  Classes, SysUtils, Windows, Messages, Forms, StdCtrls, Controls, Graphics, ImgList,
  Types, Math, ExtCtrls, Menus, Clipbrd,
  dxCore, dxCoreClasses, cxControls, cxGraphics, cxGeometry, cxLookAndFeels, cxLookAndFeelPainters,
  dxCustomHint, cxFormats, cxSchedulerUtils, cxSchedulerStorage, cxStyles, cxClasses, cxEdit,
  cxDateUtils, cxStorage, cxTextEdit, cxNavigator, cxVariants, dxUxTheme, dxTouch, Contnrs,
  dxRangeControl, dxCalloutPopup, dxBuiltInPopupMenu;

const
  LastAvailableDate = $2D2462;

  //
  cxDefaultTimeScale = 30;

  //HitTest codes
  //base class
  htcControl  = $0;
  htcTime     = $1;
  //custom ViewInfo
  htcResource = $2;

  // style indexes
  cxcsBackground      = 0;
  cxcsContent         = 1;
  cxcsEvent           = 2;
  cxcsGroupSeparator  = 3;
  cxcsDayHeader       = 4;
  cxcsSelection       = 5;
  cxcsHSplitter       = 6;
  cxcsVSplitter       = 7;
  cxcsResourceHeader  = 8;
  cxcsMaxValue        = cxcsResourceHeader;
  // for PS
  cxcsSchedulerStyleFirst = cxcsBackground;
  cxcsSchedulerStyleLast  = cxcsMaxValue;

  // default property values
  cxDefaultSchedulerHeight     = 250;
  cxDefaultSchedulerWidth      = 350;
  cxDefaultSplitterWidth       = 5;
  cxDefaultGroupSeparatorWidth = 11;
  cxDefaultResourcesPerPage    = 0;

  // minimal property values
  cxMinSplitterWidth   = 3;

  // hint timings
  cxscMinHintWidth: Integer = 150;
  cxscMaxHintWidth: Integer = 1000;

  //
  cxScrollInterval   = 25;
  cxScrollZoneSize   = 10;
  //
  cxNavigatorStartTimer = 300;

  // navigator's buttons
  cxSchedulerFirstButton             = 0;
  cxSchedulerPrevPageButton          = 1;
  cxSchedulerPrevButton              = 2;
  cxSchedulerNextButton              = 3;
  cxSchedulerNextPageButton          = 4;
  cxSchedulerLastButton              = 5;
  cxSchedulerShowMoreResourcesButton = 6;
  cxSchedulerShowFewerResourcesButton = 7;

  cxSchedulerNavigatorVisibility: array[0..7] of Boolean =
    (True, False, False, False, False, True, True, True);

  SCF_SCHEDULERCLIPBOARDFORMAT = 'ExpressScheduler 2.0';

  cxSchedulerDefaultViewStyle = svsModern;

type
  TcxCustomScheduler = class;

  TcxSchedulerSubControl = class;
  TcxSchedulerSubControlController = class;
  TcxSchedulerSubControlHitTest = class;
  TcxSchedulerSubControlPainter = class;
  TcxSchedulerSubControlViewInfo = class;
  TcxSchedulerSplitter = class;
  TcxSchedulerStyles = class;
  // popup menus
  TcxSchedulerContentPopupMenu = class;
  TcxSchedulerEventPopupMenu = class;

  TcxSchedulerViewController = class;
  TcxSchedulerCustomView = class;
  TcxSchedulerCustomViewClass = class of TcxSchedulerCustomView;
  TcxSchedulerCustomViewViewInfo = class;
  TcxDragHelper = class;
  TcxDragEventHelper = class;
  TcxEventSizingHelper = class;
  TcxSchedulerCustomDateNavigator = class;

  TcxSchedulerNavigatorButton = class;
  TcxSchedulerResourceNavigator = class;

  TcxSchedulerOptionsView = class;

  TcxSchedulerHintController = class;

  TcxControlFlag = (cfInvalidLayout, cfLocked, cfViewValid);
  TcxControlFlags = set of TcxControlFlag;

  TcxSchedulerSplitterKind = (skHorizontal, skVertical);
  TcxSchedulerViewPosition = (vpRight, vpLeft, vpTop, vpBottom);

  TcxSchedulerGroupingKind = (gkDefault, gkNone, gkByDate, gkByResource);
  TcxSchedulerViewMode = (vmDay, vmWeek, vmMonth, vmWorkWeek, vmAgenda);

  TcxSchedulerDragAndDropObject = class;

  TcxSchedulerContentPopupMenuItem = (cpmiNewEvent, cpmiNewAllDayEvent,
    cpmiNewReccuringEvent, cpmiToday, cpmiGoToDate, cpmiGoToThisDay, cpmiResourcesLayout);
  TcxSchedulerContentPopupMenuItems = set of TcxSchedulerContentPopupMenuItem;

  TcxSchedulerEventModernStyleHintInfo = class
  public
    Caption: string;
    Location: string;
    Finish: string;
    Reminder: string;
    Start: string;
    TaskComplete: Integer;
    ShowLocation: Boolean;
    ShowFinish: Boolean;
    ShowReminder: Boolean;
    ShowStart: Boolean;
    ShowTaskComplete: Boolean;
    Visible: Boolean;
  end;

  { TcxSchedulerSubControl }

  TcxSchedulerSubControl = class(TInterfacedPersistent)
  private
    FController: TcxSchedulerSubControlController;
    FCursor: TCursor;
    FHeight: Integer;
    FHitTest: TcxSchedulerSubControlHitTest;
    FLeft: Integer;
    FPainter: TcxSchedulerSubControlPainter;
    FScheduler: TcxCustomScheduler;
    FTop: Integer;
    FViewInfo: TcxSchedulerSubControlViewInfo;
    FVisible: Boolean;
    FWidth: Integer;

    function GetBottom: Integer;
    function GetBounds: TRect;
    function GetCanvas: TcxCanvas;
    function GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
    function GetIsGestureScrolling: Boolean;
    function GetIsScrollingContent: Boolean;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetPainterHelperClass: TcxSchedulerPainterHelperClass;
    function GetRight: Integer;
    function GetScaleFactor: TdxScaleFactor;
    function GetStyles: TcxSchedulerStyles;
    function GetViewStyle: TcxSchedulerViewStyle;
    procedure InternalSetBounds(const AValue: TRect);
    procedure SetBottom(const Value: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRight(Value: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected
    // store interface
    procedure GetProperties(AProperties: TStrings); virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    //
    function AllowDesignHitTest(X, Y: Integer; AShift: TShiftState): Boolean; virtual;
    procedure BoundsChanged; virtual;
    procedure CalculateViewInfo; virtual;
    function CanCapture(const APoint: TPoint): Boolean; virtual;
    procedure Changed; virtual;
    procedure ClearCachedData; virtual;
    function CreateController: TcxSchedulerSubControlController; virtual;
    function CreateHitTest: TcxSchedulerSubControlHitTest; virtual;
    function CreatePainter: TcxSchedulerSubControlPainter; virtual;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; virtual;

    procedure BoundsChanging; virtual;
    procedure CheckBiDiMode; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoBeforeMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
    procedure DoCancelMode; virtual;
    procedure DoLayoutChanged; virtual;
    procedure DoMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(AShift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
    procedure DoPaint; virtual;
    procedure DoScaleScroll; virtual;
    procedure FormatChanged; virtual;
    function GetClientRect: TRect; virtual;
    function GetOwner: TPersistent; override;
    function GetHScrollBarBounds: TRect; virtual;
    function GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar;
    function GetSizeGripBounds: TRect; virtual;
    function GetStartOfWeek: TDay; virtual;
    function GetVScrollBarBounds: TRect; virtual;
    procedure InitScrollBarsParameters; virtual;
    function IsSpecialPaint: Boolean; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MousePositionChanged(var X, Y: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure Paint;
    procedure PeriodChanged; virtual;
    function UseRightToLeftAlignment: Boolean;
    procedure SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
      AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
    procedure VisibleChanged; virtual;

    property Bottom: Integer read GetBottom write SetBottom;
    property Canvas: TcxCanvas read GetCanvas;
    property ClientRect: TRect read GetClientRect;
    property Controller: TcxSchedulerSubControlController read FController;
    property Cursor: TCursor read FCursor write FCursor;
    property DateTimeHelper: TcxSchedulerDateTimeHelperClass read GetDateTimeHelperClass;
    property HitTest: TcxSchedulerSubControlHitTest read FHitTest;
    property IsGestureScrolling: Boolean read GetIsGestureScrolling;
    property IsScrollingContent: Boolean read GetIsScrollingContent;
    property Left: Integer read FLeft write SetLeft;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Painter: TcxSchedulerSubControlPainter read FPainter;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelperClass;
    property Right: Integer read GetRight write SetRight;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Scheduler: TcxCustomScheduler read FScheduler;
    property StartOfWeek: TDay read GetStartOfWeek;
    property Styles: TcxSchedulerStyles read GetStyles;
    property Top: Integer read FTop write SetTop;
    property ViewInfo: TcxSchedulerSubControlViewInfo read FViewInfo;
    property ViewStyle: TcxSchedulerViewStyle read GetViewStyle;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    procedure Invalidate;
    procedure InvalidateRect(const ARect: TRect);
    procedure LayoutChanged;
    procedure Refresh;
    procedure Repaint;
    procedure RepaintRect(const ARect: TRect);
    function ScreenToClient(const APos: TPoint): TPoint;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;

    property Bounds: TRect read GetBounds write InternalSetBounds;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
  end;

  { TcxSchedulerSubControlController }

  TcxSchedulerSubControlController = class(TcxIUnknownObject)
  private
    FCanProcessMouseMove: Boolean;
    FOwner: TcxSchedulerSubControl;
    function GetHitTest: TcxSchedulerSubControlHitTest;
    function GetStartOfWeek: TDay;
  protected
    // drag'n'drop
    procedure BeginDragAndDrop; virtual;
    function CanDrag(X, Y: Integer): Boolean; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; virtual;
    procedure StartDrag(var DragObject: TDragObject); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; virtual;
    // virtual
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoCancelMode; virtual;
    function GetCursor(X, Y: Integer): TCursor; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure Reset; virtual;

    property CanProcessMouseMove: Boolean read FCanProcessMouseMove;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); virtual;
    function UseRightToLeftAlignment: Boolean;

    property HitTest: TcxSchedulerSubControlHitTest read GetHitTest;
    property Owner: TcxSchedulerSubControl read FOwner;
    property StartOfWeek: TDay read GetStartOfWeek;
  end;

  { TcxSchedulerSubControlHitTest }

  TcxSchedulerSubControlHitTest = class
  private
    FOwner: TcxSchedulerSubControl;
    FHitPoint: TPoint;
    function GetPosValue(AIndex: Integer): Integer;
    function GetScheduler: TcxCustomScheduler;
    procedure SetHitPoint(const APoint: TPoint);
    procedure SetPosValue(AIndex, AValue: Integer);
  protected
    FTime: TDateTime;
    Flags: Int64;
    procedure Clear; virtual;
    procedure DoCalculate; virtual;
    function GetBitState(AIndex: Integer): Boolean;
    function GetMaskState(AMask: Integer): Boolean;
    function GetMaskStateEx(AMask: Integer): Boolean;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
    procedure SetMaskState(AMask: Integer; AValue: Boolean);
    property Owner: TcxSchedulerSubControl read FOwner;
    property Scheduler: TcxCustomScheduler read GetScheduler;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); virtual;
    destructor Destroy; override;
    procedure Recalculate;

    property HitPoint: TPoint read FHitPoint write SetHitPoint;
    property HitX: Integer index 0 read GetPosValue write SetPosValue;
    property HitY: Integer index 1 read GetPosValue write SetPosValue;
    property HitAtControl: Boolean index htcControl read GetBitState;
    property HitAtTime: Boolean index htcTime read GetBitState;
    property Time: TDateTime read FTime;
  end;

  { TcxSchedulerSubControlPainter }

  TcxSchedulerSubControlPainter = class
  strict private
    FOwner: TcxSchedulerSubControl;

    function GetCanvas: TcxCanvas;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TcxSchedulerSubControlViewInfo;
  protected
    property Owner: TcxSchedulerSubControl read FOwner;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TcxSchedulerSubControlViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); virtual;
    procedure AfterPaint; virtual;
    procedure BeforePaint; virtual;
    procedure InitializePainter; virtual;
    procedure Paint; virtual;

    property Canvas: TcxCanvas read GetCanvas;
  end;

  { TcxSchedulerSubControlViewInfo }

  TcxSchedulerSubControlViewInfo = class(TcxIUnknownObject)
  private
    FOwner: TcxSchedulerSubControl;

    function GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
    function GetDefaultFont: TFont;
    function GetIsSchedulerCreated: Boolean;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetPainterHelperClass: TcxSchedulerPainterHelperClass;
    function GetScaleFactor: TdxScaleFactor;
    function GetStyles: TcxSchedulerStyles;
    function GetViewStyle: TcxSchedulerViewStyle;
  protected
    FBounds: TRect;
    procedure AfterCalculate; virtual;
    procedure Clear; virtual;
    procedure DoCalculate; virtual;
    function GetBounds: TRect; virtual;
    function UseRightToLeftAlignment: Boolean; virtual;

    property DateTimeHelper: TcxSchedulerDateTimeHelperClass read GetDateTimeHelperClass;
    property DefaultFont: TFont read GetDefaultFont;
    property IsSchedulerCreated: Boolean read GetIsSchedulerCreated;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Bounds: TRect read FBounds;
    property Owner: TcxSchedulerSubControl read FOwner;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelperClass;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Styles: TcxSchedulerStyles read GetStyles;
    property ViewStyle: TcxSchedulerViewStyle read GetViewStyle;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); virtual;
    procedure Calculate; virtual;
  end;

  { TcxSchedulerBackground }

  TcxSchedulerBackground = class(TcxSchedulerSubControl)
  protected
    function IsSpecialPaint: Boolean; override;
  end;

  { TcxSchedulerControlBox }

  TcxSchedulerControlBox = class(TcxSchedulerSubControl)
  private
    FContainer: TWinControl;
    FControlAlign: TAlign;
    FControlParent: TWinControl;
    FControlRect: TRect;
    FControl: TControl;
    FViewParams: TcxViewParams;
    procedure RestorePosition;
    procedure SetControl(AValue: TControl);
    procedure StorePosition;
  protected
    // store interface
    procedure GetProperties(AProperties: TStrings); override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    //
    procedure ApplyActualBounds;
    function CreateWndContainerControl: TWinControl; virtual;
    procedure DoPaint; override;
    procedure DoLayoutChanged; override;
    function HasAsParent(AValue: TControl): Boolean;
    procedure VisibleChanged; override;

    property ViewParams: TcxViewParams read FViewParams;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;

    property Container: TWinControl read FContainer;
    property Height;
    property Width;
  published
    property Control: TControl read FControl write SetControl;
    property Visible default True;
  end;

  { TcxSchedulerSplitterController }

  TcxSchedulerSplitterController = class(TcxSchedulerSubControlController)
  private
    FHitPoint: TPoint;
    FPrevInvertRect: TRect;
    FPrevRect: TRect;
    FSaveKeyboardListener: TcxSchedulerSubControl;
    FSizingBoundsRect: TRect;
    FStartBounds: TRect;
    FScreenCanvasClipRect: TRect;
    function GetDrawClipRect: TRect;
    function GetHorzSizingRect(const P: TPoint): TRect;
    function GetScheduler: TcxCustomScheduler;
    function GetScreenOffset: TPoint;
    function GetSplitter: TcxSchedulerSplitter;
    function GetVertSizingRect(const P: TPoint): TRect;
    procedure SetHorzBounds(var R: TRect);
    procedure SetHorzDelta(ADelta: Integer);
    procedure SetVertBounds(var R: TRect);
    procedure SetVertDelta(ADelta: Integer);
  protected
    // override
    procedure DoCancelMode; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    // methods
    procedure DrawInvertRect(const R: TRect);
    procedure EraseInvertRect;
    function GetMonthSize: TSize;
    function GetOwnerMousePos(X, Y: Integer): TPoint;
    function GetSizingBoundsRect: TRect;
    function GetSizingIncrement: Integer;
    function GetSizingRect(const P: TPoint): TRect;
    procedure InvertRect(ACanvas: TcxCanvas; R: TRect);
    function IsIntegralSizing: Boolean; virtual;
    function IsDynamicUpdate: Boolean;
    procedure Modified;
    procedure SetSizeDelta(ADelta: Integer); virtual;
    procedure UpdateSizing(const R: TRect);

    property PrevInvertRect: TRect read FPrevInvertRect;
    property Scheduler: TcxCustomScheduler read GetScheduler;
    property ScreenCanvasClipRect: TRect read FScreenCanvasClipRect;
    property ScreenOffset: TPoint read GetScreenOffset;
    property SizingBoundsRect: TRect read FSizingBoundsRect;
    property Splitter: TcxSchedulerSplitter read GetSplitter;
  end;

  { TcxSchedulerSplitterHitTest }

  TcxSchedulerSplitterHitTest = class(TcxSchedulerSubControlHitTest)
  private
    function GetSplitter: TcxSchedulerSplitter;
  public
    property Splitter: TcxSchedulerSplitter read GetSplitter;
  end;

  { TcxSchedulerSplitter }

  TcxSchedulerSplitter = class(TcxSchedulerBackground)
  private
    FKind: TcxSchedulerSplitterKind;
    FViewParams: TcxViewParams;
    function GetHitTest: TcxSchedulerSplitterHitTest;
  protected
    function AllowDesignHitTest(X, Y: Integer; AShift: TShiftState): Boolean; override;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    procedure DoLayoutChanged; override;
    procedure DoPaint; override;
    procedure SetKind(AKind: TcxSchedulerSplitterKind); virtual;
    procedure UpdateCursor;

    property HitTest: TcxSchedulerSplitterHitTest read GetHitTest;
    property ViewParams: TcxViewParams read FViewParams;
  public
    property Kind: TcxSchedulerSplitterKind read FKind;
  end;

 { TcxSchedulerNavigatorButton }

  TcxSchedulerNavigatorButtonClickEvent = procedure(Sender: TcxSchedulerResourceNavigator;
    AButton: TcxSchedulerNavigatorButton; var AHandled: Boolean) of object;

  TcxSchedulerNavigatorButton = class(TCollectionItem)
  private
    FCommand: Integer;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: Integer;
    FVisible: Boolean;
    function GetScheduler: TcxCustomScheduler;
    procedure SetEnabled(AValue: Boolean);
    procedure SetImageIndex(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    function IsHintStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsVisibleStored: Boolean;
  protected
    FBounds: TRect;
    FRotated: Boolean;
    FState: TcxButtonState;
    FVisibleIndex: Integer;
    procedure Changed; virtual;
    procedure Click; virtual;
    function GetDisplayName: string; override;
    function GetHintText: string;
    procedure Draw(APainter: TcxCustomLookAndFeelPainter; ACanvas: TcxCanvas); virtual;
    function GetActualImageIndex: Integer; virtual;
    function GetActualImageList: TCustomImageList; virtual;
    function GetState(ACanDisabled: Boolean = True): TcxButtonState;
    function GetIsStandard: Boolean; virtual;

    property Command: Integer read FCommand write FCommand;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    property ActualImageIndex: Integer read GetActualImageIndex;
    property ActualImageList: TCustomImageList read GetActualImageList;
    property Bounds: TRect read FBounds;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored;
    property Rotated: Boolean read FRotated;
    property Scheduler: TcxCustomScheduler read GetScheduler;
    property IsStandard: Boolean read GetIsStandard;
    property State: TcxButtonState read FState;
    property VisibleIndex: Integer read FVisibleIndex;
  published
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  { TcxSchedulerNavigatorCustomButton }

  TcxSchedulerNavigatorCustomButton = class(TcxSchedulerNavigatorButton)
  published
    property Enabled;
  end;

  TcxSchedulerNavigatorButtonClass = class of TcxSchedulerNavigatorButton;

  { TcxSchedulerNavigatorCustomButtons }

  TcxSchedulerNavigatorCustomButtons = class(TCollection)
  private
    FOwner: TPersistent;
    FScheduler: TcxCustomScheduler;
    function GetItem(AIndex: Integer): TcxSchedulerNavigatorCustomButton;
    function GetVisibleCount: Integer;
    procedure SetItem(AIndex: Integer; AValue: TcxSchedulerNavigatorCustomButton);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    property Scheduler: TcxCustomScheduler read FScheduler;
  public
    constructor CreateEx(AOwner: TPersistent; AItemClass: TCollectionItemClass); virtual;

    property VisibleCount: Integer read GetVisibleCount;
    property Items[AIndex: Integer]: TcxSchedulerNavigatorCustomButton read GetItem write SetItem; default;
  end;

  { TcxSchedulerNavigatorButtons }

  TcxSchedulerNavigatorButtons = class(TPersistent)
  private
    FButtons: TcxSchedulerNavigatorCustomButtons;
    FOwner: TcxSchedulerResourceNavigator;
    function GetButtonByIndex(AIndex: Integer): TcxSchedulerNavigatorButton;
    procedure SetButtonByIndex(AIndex: Integer; AValue: TcxSchedulerNavigatorButton);
  protected
    function AddButton(ACommand: Integer; AVisible: Boolean = True): TcxSchedulerNavigatorButton;
    procedure CreateButtons; virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TcxSchedulerResourceNavigator); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Buttons: TcxSchedulerNavigatorCustomButtons read FButtons;
    property ResourceNavigator: TcxSchedulerResourceNavigator read FOwner;
  published
    property First: TcxSchedulerNavigatorButton index cxSchedulerFirstButton read GetButtonByIndex write SetButtonByIndex;
    property PrevPage: TcxSchedulerNavigatorButton index cxSchedulerPrevPageButton read GetButtonByIndex write SetButtonByIndex;
    property Prev: TcxSchedulerNavigatorButton index cxSchedulerPrevButton read GetButtonByIndex write SetButtonByIndex;
    property Next: TcxSchedulerNavigatorButton index cxSchedulerNextButton read GetButtonByIndex write SetButtonByIndex;
    property NextPage: TcxSchedulerNavigatorButton index cxSchedulerNextPageButton read GetButtonByIndex write SetButtonByIndex;
    property Last: TcxSchedulerNavigatorButton index cxSchedulerLastButton read GetButtonByIndex write SetButtonByIndex;
    property ShowFewerResources: TcxSchedulerNavigatorButton index cxSchedulerShowFewerResourcesButton read GetButtonByIndex write SetButtonByIndex;
    property ShowMoreResources: TcxSchedulerNavigatorButton index cxSchedulerShowMoreResourcesButton read GetButtonByIndex write SetButtonByIndex;
  end;

  { TcxSchedulerResourceNavigatorController }

  TcxSchedulerResourceNavigatorController = class(TcxSchedulerSubControlController)
  private
    FHotTrackButton: TcxSchedulerNavigatorButton;
    function GetHintController: TcxSchedulerHintController;
    function GetResourceNavigator: TcxSchedulerResourceNavigator;
    procedure SetHotTrackButton(Value: TcxSchedulerNavigatorButton);
  protected
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CheckButtonDown(Button: TMouseButton; Shift: TShiftState); virtual;
    function GetHotTrackButton(ACanDisabled: Boolean = True): TcxSchedulerNavigatorButton;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(AShift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;

    property HintController: TcxSchedulerHintController read GetHintController;
    property HotTrackButton: TcxSchedulerNavigatorButton read FHotTrackButton write SetHotTrackButton;
    property ResourceNavigator: TcxSchedulerResourceNavigator read GetResourceNavigator;
  end;

  { TcxSchedulerResourceNavigatorHitTest }

  TcxSchedulerResourceNavigatorHitTest = class(TcxSchedulerSubControlHitTest)
  private
    function GetCurrentButton(AButtons: TcxSchedulerNavigatorCustomButtons): TcxSchedulerNavigatorButton;
    function GetHitAtButton: Boolean;
    function GetHitButton: TcxSchedulerNavigatorButton;
    function GetResourceNavigator: TcxSchedulerResourceNavigator;
  public
    property HitAtButton: Boolean read GetHitAtButton;
    property HitButton: TcxSchedulerNavigatorButton read GetHitButton;
    property ResourceNavigator: TcxSchedulerResourceNavigator read GetResourceNavigator;
  end;

  { TcxSchedulerResourceNavigator }

  TcxSchedulerNavigatorCustomDrawButtonEvent = procedure(Sender: TcxSchedulerResourceNavigator;
    ACanvas: TcxCanvas; AButton: TcxSchedulerNavigatorButton; var ADone: Boolean) of object;

  TcxSchedulerNavigatorVisibilityMode = (snvNever, snvAlways, snvAuto);

  TcxSchedulerResourceNavigator = class(TcxSchedulerSubControl)
  private
    FButtons: TcxSchedulerNavigatorButtons;
    FButtonImages: TCustomImageList;
    FCustomButtons: TcxSchedulerNavigatorCustomButtons;
    FShowButtons: Boolean;
    FTimer: TTimer;
    FScrollBarKind: TScrollBarKind;
    FVisibility: TcxSchedulerNavigatorVisibilityMode;
    FOnButtonClick: TcxSchedulerNavigatorButtonClickEvent;
    FOnCustomDrawButton: TcxSchedulerNavigatorCustomDrawButtonEvent;
    function GetFirstVisibleResourceIndex: Integer;
    function GetHitTest: TcxSchedulerResourceNavigatorHitTest;
    function GetItem(AIndex: Integer): TcxSchedulerNavigatorButton;
    function GetItemCount: Integer;
    function GetResourceCount: Integer;
    function GetResourcesPerPage: Integer;
    function GetVisibleButtonCount: Integer;
    procedure SetButtonImages(Value: TCustomImageList);
    procedure SetButtons(Value: TcxSchedulerNavigatorButtons);
    procedure SetCustomButtons(Value: TcxSchedulerNavigatorCustomButtons);
    procedure SetFirstVisibleResourceIndex(AValue: Integer);
    procedure SetResourcesPerPage(AValue: Integer);
    procedure SetShowButtons(AValue: Boolean);
    procedure SetVisibility(AValue: TcxSchedulerNavigatorVisibilityMode);
    function IsCustomButtonsStored: Boolean;
  protected
    FPressedButton: TcxSchedulerNavigatorButton;
    FVisibleButtonCount: Integer;
    procedure BoundsChanged; override;
    procedure ButtonClickHandler(AButton: TcxSchedulerNavigatorButton); virtual;
    procedure CalculateBounds; virtual;
    procedure CheckButtonsState; virtual;
    procedure Click(Sender: TcxSchedulerNavigatorButton);
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreateButtons: TcxSchedulerNavigatorCustomButtons; virtual;
    function CreateStandardButtons: TcxSchedulerNavigatorButtons; virtual;
    function DoCustomDrawButton(AButton: TcxSchedulerNavigatorButton): Boolean; virtual;
    function DoOnClick(Sender: TcxSchedulerNavigatorButton): Boolean; virtual;
    procedure DoPaint; override;
    procedure FirstVisibleResourceChanged;
    function GetCustomButtonClass: TcxSchedulerNavigatorButtonClass; virtual;
    function GetScrollerHint: string;
    procedure InitScrollBarsParameters; override;
    procedure InvalidateButton(AButton: TcxSchedulerNavigatorButton);
    procedure Scroll(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    //
    function ActualCountPerPage: Integer;
    function ActualFirstResourceIndex: Integer;
    function ButtonSize: TSize;
    function MeasureHeight: Integer; virtual;
    function MeasureWidth: Integer; virtual;

    property FirstVisibleResourceIndex: Integer read GetFirstVisibleResourceIndex write SetFirstVisibleResourceIndex;
    property HitTest: TcxSchedulerResourceNavigatorHitTest read GetHitTest;
    property ResourcesPerPage: Integer read GetResourcesPerPage write SetResourcesPerPage;
    property Timer: TTimer read FTimer;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function NeedScrollBar: Boolean; virtual;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TcxSchedulerNavigatorButton read GetItem;
    property ResourceCount: Integer read GetResourceCount;
    property Scheduler;
    property ScrollBarKind: TScrollBarKind read FScrollBarKind;
    property VisibleButtonCount: Integer read FVisibleButtonCount;
  published
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property Buttons: TcxSchedulerNavigatorButtons read FButtons write SetButtons;
    property CustomButtons: TcxSchedulerNavigatorCustomButtons read FCustomButtons write SetCustomButtons stored IsCustomButtonsStored;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default True;
    property Visibility: TcxSchedulerNavigatorVisibilityMode read FVisibility write SetVisibility default snvAuto;
    property OnButtonClick: TcxSchedulerNavigatorButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnCustomDrawButton: TcxSchedulerNavigatorCustomDrawButtonEvent read FOnCustomDrawButton write FOnCustomDrawButton;
  end;

  { TcxSchedulerOptionsData }

  TcxSchedulerEventOperations = class(TPersistent)
  private
    FScheduler: TcxCustomScheduler;
    FCreating: Boolean;
    FDeleting: Boolean;
    FDialogEditing: Boolean;
    FDialogShowing: Boolean;
    FInplaceEditing: Boolean;
    FIntersection: Boolean;
    FMoving: Boolean;
    FMovingBetweenResources: Boolean;
    FReadOnly: Boolean;
    FRecurrence: Boolean;
    FSharingBetweenResources: Boolean;
    FSizing: Boolean;
    function GetCreating: Boolean;
    function GetCreatingStored: Boolean;
    function GetDeleting: Boolean;
    function GetDeletingStored: Boolean;
    function GetDialogEditing: Boolean;
    function GetDialogEditingStored: Boolean;
    function GetInplaceEditing: Boolean;
    function GetInplaceEditingStored: Boolean;
    function GetMoving: Boolean;
    function GetMovingBetweenResources: Boolean;
    function GetMovingBetweenResourcesStored: Boolean;
    function GetMovingStored: Boolean;
    function GetSizing: Boolean;
    function GetSizingStored: Boolean;
  protected
    function GetOwner: TPersistent; override;

    property Scheduler: TcxCustomScheduler read FScheduler;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Creating: Boolean
      read GetCreating write FCreating stored GetCreatingStored;
    property Deleting: Boolean
      read GetDeleting write FDeleting stored GetDeletingStored;
    property DialogEditing: Boolean
      read GetDialogEditing write FDialogEditing stored GetDialogEditingStored;
    property DialogShowing: Boolean read FDialogShowing write FDialogShowing default True;
    property InplaceEditing: Boolean
      read GetInplaceEditing write FInplaceEditing stored GetInplaceEditingStored;
    property Intersection: Boolean read FIntersection write FIntersection default True;
    property MovingBetweenResources: Boolean
      read GetMovingBetweenResources write FMovingBetweenResources
      stored GetMovingBetweenResourcesStored;
    property Moving: Boolean
      read GetMoving write FMoving stored GetMovingStored;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Recurrence: Boolean read FRecurrence write FRecurrence default True;
    property SharingBetweenResources: Boolean read FSharingBetweenResources write FSharingBetweenResources default False;
    property Sizing: Boolean read GetSizing write FSizing stored GetSizingStored;
  end;

  { TcxSchedulerOptionsCustomize }

  TcxSchedulerOptionsCustomize = class(TPersistent)
  private
    FControlsSizing: Boolean;
    FDynamicSizing: Boolean;
    FIntegralSizing: Boolean;
    FScheduler: TcxCustomScheduler;
    procedure SetControlsSizing(AValue: Boolean);
    procedure SetIntegralSizing(AValue: Boolean);
  protected
    procedure Changed; virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    procedure Assign(Source: TPersistent); override;

    property Scheduler: TcxCustomScheduler read FScheduler;
  published
    property ControlsSizing: Boolean read FControlsSizing write SetControlsSizing default True;
    property DynamicSizing: Boolean read FDynamicSizing write FDynamicSizing default False;
    property IntegralSizing: Boolean read FIntegralSizing write SetIntegralSizing default True;
  end;

  { TcxSchedulerResourceHeaders }

  TcxSchedulerHeaderImagePosition = (ipLeft, ipTop, ipRight, ipBottom);

  TcxSchedulerResourceHeaders = class(TPersistent)
  private
    FHeight: Integer;
    FImagePosition: TcxSchedulerHeaderImagePosition;
    FMultilineCaptions: Boolean;
    FOwner: TcxSchedulerOptionsView;
    FRotateCaptions: Boolean;
    procedure SetHeight(AValue: Integer);
    procedure SetImagePosition(AValue: TcxSchedulerHeaderImagePosition);
    procedure SetMultilineCaptions(AValue: Boolean);
    procedure SetRotateCations(AValue: Boolean);
    function IsImagePositionStored: Boolean;
  protected
    procedure Changed;
    procedure ChangeScale(M, D: Integer);
    function GetOwner: TPersistent; override;
    property Owner: TcxSchedulerOptionsView read FOwner;
  public
    constructor Create(AOwner: TcxSchedulerOptionsView); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write SetHeight default 0;
    property MultilineCaptions: Boolean read FMultilineCaptions write SetMultilineCaptions default False;
    property ImagePosition: TcxSchedulerHeaderImagePosition read FImagePosition write SetImagePosition stored IsImagePositionStored;
    property RotateCaptions: Boolean read FRotateCaptions write SetRotateCations default True;
  end;

  { TcxSchedulerOptionsView }

  TcxSchedulerOptionsView = class(TPersistent)
  private
    FActualStartOfWeek: TDay;
    FAdditionalTimeZoneDaylightSaving: Boolean;
    FCurrentTimeZoneDaylightSaving: Boolean;
    FDayBorderColor: TColor;
    FEventBorderColor: TColor;
    FEventHeight: Integer;
    FGroupingKind: TcxSchedulerGroupingKind;
    FGroupSeparatorWidth: Integer;
    FHideSelection: Boolean;
    FHorzSplitterWidth: Integer;
    FHotTrack: Boolean;
    FResourceHeaders: TcxSchedulerResourceHeaders;
    FResourcesPerPage: Integer;
    FScheduler: TcxCustomScheduler;
    FShowAdditionalTimeZone: Boolean;
    FShowEventsWithoutResource: Boolean;
    FShowHints: Boolean;
    FShowNavigationButtons: Boolean;
    FStartOfWeek: TcxStartOfWeek;
    FStyle: TcxSchedulerViewStyle;
    FTimeZoneLabels: array[0..1] of string;
    FTimeZones: array[0..1] of Integer;
    FVertSplitterWidth: Integer;
    FViewPosition: TcxSchedulerViewPosition;
    FWorkDays: TDays;
    FWorkFinish: TTime;
    FWorkFinishAssigned: Boolean;
    FWorkStart: TTime;
    FWorkStartAssigned: Boolean;
    function GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
    function GetRotateResourceCaptions: Boolean;
    function GetTimeZone(AIndex: Integer): Integer;
    function GetTimeZoneLabel(AIndex: Integer): string;
    function IsTimeZoneLabelStored(AIndex: Integer): Boolean;
    procedure SetATZDaylightSaving(AValue: Boolean);
    procedure SetCTZDaylightSaving(AValue: Boolean);
    procedure SetDayBorderColor(AValue: TColor);
    procedure SetEventBorderColor(AValue: TColor);
    procedure SetEventHeight(AValue: Integer);
    procedure SetGroupingKind(AValue: TcxSchedulerGroupingKind);
    procedure SetGroupSeparatorWidth(AValue: Integer);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetHorzSplitterWidth(AValue: Integer);
    procedure SetResourceHeaders(AValue: TcxSchedulerResourceHeaders);
    procedure SetResourcesPerPage(AValue: Integer);
    procedure SetRotateResourceCaptions(AValue: Boolean);
    procedure SetShowAdditionalTimeZone(AValue: Boolean);
    procedure SetShowEventsWithoutResource(AValue: Boolean);
    procedure SetShowNavigationButtons(AValue: Boolean);
    procedure SetSplitterWidth(AValue: Integer; var AWidth: Integer);
    procedure SetStartOfWeek(AValue: TcxStartOfWeek);
    procedure SetStyle(AValue: TcxSchedulerViewStyle);
    procedure SetTimeZone(AIndex, AValue: Integer);
    procedure SetTimeZoneLabel(AIndex: Integer; const AValue: string);
    procedure SetVertSplitterWidth(AValue: Integer);
    procedure SetViewPosition(AValue: TcxSchedulerViewPosition);
    procedure SetWorkDays(AValue: TDays);
    procedure SetWorkFinish(AValue: TTime);
    procedure SetWorkStart(AValue: TTime);
    //
    procedure ReadWorkFinish(AReader: TReader);
    procedure ReadWorkStart(AReader: TReader);
    procedure WriteWorkFinish(AWriter: TWriter);
    procedure WriteWorkStart(AWriter: TWriter);
  protected
    procedure CalculateActualStartOfWeek;
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer);
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; override;
    function IsWorkDaysStored: Boolean;
    function IsWorkTime(AResourceItem: TcxSchedulerStorageResourceItem; const ADateTime: TDateTime): Boolean;

    property DateTimeHelper: TcxSchedulerDateTimeHelperClass read GetDateTimeHelperClass;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;//True;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property ActualStartOfWeek: TDay read FActualStartOfWeek;
    property Scheduler: TcxCustomScheduler read FScheduler;
  published
    property AdditionalTimeZone: Integer index 0 read GetTimeZone write SetTimeZone default -1;
    property AdditionalTimeZoneDaylightSaving: Boolean read FAdditionalTimeZoneDaylightSaving write SetATZDaylightSaving default False;
    property AdditionalTimeZoneLabel: string index 0 read GetTimeZoneLabel write SetTimeZoneLabel stored IsTimeZoneLabelStored;
    property CurrentTimeZone: Integer index 1 read GetTimeZone write SetTimeZone default -1;
    property CurrentTimeZoneDaylightSaving: Boolean read FCurrentTimeZoneDaylightSaving write SetCTZDaylightSaving default False;
    property CurrentTimeZoneLabel: string index 1 read GetTimeZoneLabel write SetTimeZoneLabel stored IsTimeZoneLabelStored;
    property DayBorderColor: TColor read FDayBorderColor write SetDayBorderColor default clDefault;
    property EventBorderColor: TColor read FEventBorderColor write SetEventBorderColor default clBlack;
    property EventHeight: Integer read FEventHeight write SetEventHeight default 0;
    property GroupingKind: TcxSchedulerGroupingKind read FGroupingKind write SetGroupingKind default gkDefault;
    property GroupSeparatorWidth: Integer read FGroupSeparatorWidth write SetGroupSeparatorWidth default cxDefaultGroupSeparatorWidth;
    property HideSelection: Boolean read  FHideSelection write SetHideSelection default False;
    property HorzSplitterWidth: Integer read FHorzSplitterWidth write SetHorzSplitterWidth default cxDefaultSplitterWidth;
    property ResourceHeaders: TcxSchedulerResourceHeaders read FResourceHeaders write SetResourceHeaders;
    property ResourcesPerPage: Integer read FResourcesPerPage write SetResourcesPerPage default cxDefaultResourcesPerPage;
    property RotateResourceCaptions: Boolean read GetRotateResourceCaptions write SetRotateResourceCaptions default True;
    property ShowAdditionalTimeZone: Boolean read FShowAdditionalTimeZone write SetShowAdditionalTimeZone default False;
    property ShowEventsWithoutResource: Boolean read FShowEventsWithoutResource write SetShowEventsWithoutResource default False;
    property ShowHints: Boolean read FShowHints write FShowHints default True;
    property ShowNavigationButtons: Boolean read FShowNavigationButtons write SetShowNavigationButtons default True;
    property StartOfWeek: TcxStartOfWeek read FStartOfWeek write SetStartOfWeek default swSystem;
    property Style: TcxSchedulerViewStyle read FStyle write SetStyle default cxSchedulerDefaultViewStyle;
    property VertSplitterWidth: Integer read FVertSplitterWidth write SetVertSplitterWidth default cxDefaultSplitterWidth;
    property ViewPosition: TcxSchedulerViewPosition read FViewPosition write SetViewPosition default vpLeft;
    property WorkDays: TDays read FWorkDays write SetWorkDays stored IsWorkDaysStored;
    property WorkFinish: TTime read FWorkFinish write SetWorkFinish stored False;
    property WorkStart: TTime read FWorkStart write SetWorkStart stored False;
  end;

  { TcxSchedulerViewHitTest }

  TcxEventDragKind = (edkNone, edkEventDragRect, edkMoveEvent, edkResizeStart, edkResizeEnd);

  TcxSchedulerViewHitTest = class(TcxSchedulerSubControlHitTest)
  private
    function GetHitAtEvent: Boolean;
    function GetNeedShowHint: Boolean;
  protected
    FNeedShowHint: Boolean;
    FEventBounds: TRect;
    FResource: TcxSchedulerStorageResourceItem;
    procedure Clear; override;
    function GetHitEvent: TcxSchedulerControlEvent; virtual;
    property NeedShowHint: Boolean read GetNeedShowHint;
  public
    function GetDragKind: TcxEventDragKind; virtual;

    property HitAtEvent: Boolean read GetHitAtEvent;
    property HitAtResource: Boolean index htcResource read GetBitState;
    property Event: TcxSchedulerControlEvent read GetHitEvent;
    property Resource: TcxSchedulerStorageResourceItem read FResource;
  end;

  { TcxSchedulerEditController }

  TcxSchedulerEditController = class
  private
    FEdit: TcxCustomEdit;
    FEditData: TcxCustomEditData;
    FEditDate: TDateTime;
    FEditList: TcxInplaceEditList;
    FEditProperties: TcxCustomEditProperties;
    FEditResource: TcxSchedulerStorageResourceItem;
    FEvent: TcxSchedulerControlEvent;
    FFocused: Boolean;
    FIsEditing: Boolean;
    FIsNewEvent: Boolean;
    FOwner: TcxCustomScheduler;
    function CanAccept: Boolean;
    function GetController: TcxSchedulerViewController;
    function GetEditVisible: Boolean;
    function GetView: TcxSchedulerCustomView;
    procedure SetEditVisible(Value: Boolean);
  protected
    function GetEditRect(var R: TRect; const ADate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem; AMakeVisible: Boolean = False): Boolean; virtual;
    procedure EditAfterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditExit(Sender: TObject); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditPostEditValue(Sender: TObject); virtual;
    function InitEdit(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function IsKeyForControl(var AKey: Word; Shift: TShiftState): Boolean; virtual;
    procedure PrepareEdit(AEdit: TcxCustomEdit); virtual;

    property EditProperties: TcxCustomEditProperties read FEditProperties;
    property EditData: TcxCustomEditData read FEditData;
    property Event: TcxSchedulerControlEvent read FEvent;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    procedure Activate(AEvent: TcxSchedulerControlEvent); overload;
    procedure Activate(AEvent: TcxSchedulerControlEvent; const APos: TPoint; AShift: TShiftState); overload;
    procedure Activate(AEvent: TcxSchedulerControlEvent; Key: Char); overload;
    procedure CloseEdit(Accepted: Boolean); virtual;
    procedure DeleteEvent(AEvent: TcxSchedulerControlEvent);
    procedure Init(const AEditDate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem; AIsNewEvent: Boolean = False);
    procedure UpdateEdit; virtual;
    procedure UpdateValue; virtual;

    property Controller: TcxSchedulerViewController read GetController;
    property Edit: TcxCustomEdit read FEdit;
    property EditVisible: Boolean read GetEditVisible write SetEditVisible;
    property Focused: Boolean read FFocused write FFocused;
    property IsEditing: Boolean read FIsEditing;
    property Scheduler: TcxCustomScheduler read FOwner;
    property View: TcxSchedulerCustomView read GetView;
  end;

  { TcxSchedulerViewNavigation }

  TcxSchedulerViewNavigation = class
  private
    FView: TcxSchedulerCustomView;
    function GetResourceNavigator: TcxSchedulerResourceNavigator;
    function GetScheduler: TcxCustomScheduler;
    function GetSelAnchor: TDateTime;
    function GetSelRealStart: TDateTime;
    function GetSelFinish: TDateTime;
    function GetSelResource: TcxSchedulerStorageResourceItem;
    function GetSelStart: TDateTime;
    function GetTimeIncrement: TDateTime;
    function GetVisibleResource(AIndex: Integer): TcxSchedulerStorageResourceItem;
    function GetVisibleResourceCount: Integer;
  protected
    FCurrentAnchor: TDateTime;
    FCurrentResource: TcxSchedulerStorageResourceItem;
    FShift: TShiftState;
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); virtual;
    function IsKeyNavigation(var AKey: Word; AShift: TShiftState): Boolean; virtual;
    function IsSingleLine: Boolean;
    function GetResourceItem: TcxSchedulerStorageResourceItem; virtual;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); virtual;
    procedure ReplaceDate(ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem = nil);
  public
    constructor Create(AView: TcxSchedulerCustomView);
    procedure CheckSelection;
    procedure ReplaceSelParams(const ASelStart, ASelFinish: TDateTime); overload;
    procedure ReplaceSelParams(const ASelStart, ASelFinish: TDateTime;
      AResource: TcxSchedulerStorageResourceItem); overload;
    procedure ReplaceSelParams(AResource: TcxSchedulerStorageResourceItem); overload;
    function ScrollResources(AGoForward: Boolean): Boolean;
    function ScrollResourcesCycled(AGoForward: Boolean; var AResource: TcxSchedulerStorageResourceItem): Boolean;
    function ScrollResourcesEx(AGoForward: Boolean; var AResource: TcxSchedulerStorageResourceItem): Boolean; virtual;
    procedure SetSelAnchor(const Anchor: TDateTime; AShift: TShiftState); overload;
    procedure SetSelAnchor(const Anchor: TDateTime; AShift: TShiftState;
      AResource: TcxSchedulerStorageResourceItem); overload;
    procedure ValidateSelection(var ASelStart, ASelFinish: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem); virtual;

    property ResourceNavigator: TcxSchedulerResourceNavigator read GetResourceNavigator;
    property SelAnchor: TDateTime read GetSelAnchor;
    property SelFinish: TDateTime read GetSelFinish;
    property SelRealStart: TDateTime read GetSelRealStart;
    property SelResource: TcxSchedulerStorageResourceItem read GetSelResource;
    property SelStart: TDateTime read GetSelStart;
    property Scheduler: TcxCustomScheduler read GetScheduler;
    property TimeIncrement: TDateTime read GetTimeIncrement;
    property View: TcxSchedulerCustomView read FView;
    property VisibleResourceCount: Integer read GetVisibleResourceCount;
    property VisibleResources[AIndex: Integer]: TcxSchedulerStorageResourceItem read GetVisibleResource;
  end;

  { TcxSchedulerViewController }

  TcxSchedulerViewController = class(TcxSchedulerSubControlController)
  private
    FEditShowingTimer: TTimer;
    FEditShowingTimerItem: TcxSchedulerControlEvent;
    FDragEventHelper: TcxDragEventHelper;
    FDragEvent: TcxSchedulerControlEvent;
    FDragKind: TcxEventDragKind;
    FNavigation: TcxSchedulerViewNavigation;
    FStartDragFlags: Int64;
    FStartDragHitTime: TDateTime;
    FStartDragResource: TcxSchedulerStorageResourceItem;
    procedure EditShowingTimerHandler(Sender: TObject);
    function GetEditController: TcxSchedulerEditController;
    function GetHitTest: TcxSchedulerViewHitTest;
    function GetIsEditing: Boolean;
    function GetNavigatorTimer: TTimer;
    function GetScheduler: TcxCustomScheduler;
    function GetView: TcxSchedulerCustomView;
    procedure StartEditShowingTimer(AEvent: TcxSchedulerControlEvent);
    procedure StopEditShowingTimer;
  protected
    DragAndDropObject: TcxSchedulerDragAndDropObject;
    FDownScrollArea: TRect;
    FBeforeFocusedEvent: TcxSchedulerControlEvent;
    FStartSelAnchor: TDateTime;
    FUpScrollArea: TRect;
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CancelScroll; virtual;
    function CanDrag(X, Y: Integer): Boolean; override;
    procedure CheckOpenInplaceEditorOnMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure CheckScrolling(const APos: TPoint); virtual;
    procedure CheckScrollOnDragOver(const P: TPoint; State: TDragState);
    procedure CheckUpdateEventBounds;
    procedure CloseInplaceEdit;
    function ConsiderHiddenEvents: Boolean; virtual;
    function CreateDragEventHelper: TcxDragEventHelper; virtual;
    function CreateNavigation: TcxSchedulerViewNavigation; virtual;
    function CreateResizeEventHelper: TcxEventSizingHelper; virtual;
    procedure DoSchedulerDragOver(const P: TPoint; AState: TDragState; var AAccept: Boolean);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetResourceReadOnly: Boolean;
    function IsCaptionAvailable: Boolean;
    function IsCopyDragDrop: Boolean;
    function IsDragOperation: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RecreateNavigation;
    procedure SelectNextEvent(AForward: Boolean); virtual;
    procedure ShowEventEditor(AEvent: TcxSchedulerControlEvent); virtual;
    procedure StartDrag(var DragObject: TDragObject); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure SyncEventSelection(AEvent: TcxSchedulerControlEvent); virtual;
    procedure UnselectEvents; virtual;
    procedure UpdateEventSelection(AEvent: TcxSchedulerControlEvent;
      Button: TMouseButton; Shift: TShiftState);

    // resource scrolling on event drag
    procedure CheckNavigatorScrollArea(const APoint: TPoint); virtual;
    procedure DoneNavigatorScrollArea; virtual;
    procedure InitNavigatorScrollArea; virtual;
    procedure NavigatorTimerHandler(Sender: TObject); virtual;
    function PtInArea(const ARect: TRect; const P: TPoint; IsUpArea: Boolean): Boolean;

    property EditController: TcxSchedulerEditController read GetEditController;
    property EditShowingTimer: TTimer read FEditShowingTimer;
    property DragEventHelper: TcxDragEventHelper read FDragEventHelper;
    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
    property NavigatorTimer: TTimer read GetNavigatorTimer;
    property View: TcxSchedulerCustomView read GetView;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    function CanCreateEventUsingDialog: Boolean; virtual;
    function CanCreateEventUsingInplaceEdit: Boolean; virtual;
    function CanEditEvent(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean; virtual;
    function CanShowEventDialog: Boolean; virtual;
    procedure DeleteSelectedEvents;
    function IsEventEditing(AEvent: TcxSchedulerControlEvent; AResource: TcxSchedulerStorageResourceItem): Boolean;
    procedure SelectSingleEvent(AEvent: TcxSchedulerControlEvent; ADate: TDateTime; AMakeVisible: Boolean = True);

    property DragEvent: TcxSchedulerControlEvent read FDragEvent;
    property DragKind: TcxEventDragKind read FDragKind;
    property IsEditing: Boolean read GetIsEditing;
    property Navigation: TcxSchedulerViewNavigation read FNavigation;
    property Scheduler: TcxCustomScheduler read GetScheduler;
    property StartDragFlags: Int64 read FStartDragFlags;
    property StartDragHitTime: TDateTime read FStartDragHitTime;
    property StartDragResource: TcxSchedulerStorageResourceItem read FStartDragResource;
  end;

  { TcxDragHelper }

  TcxSchedulerDragOverDestination = (dodView, dodControlBox, dodDateNavigator, dodOther);

  TcxDragHelper = class
  private
    FActualHitTime: TDateTime;
    FAcceptedChanged: Boolean;
    FDestination: TcxSchedulerDragOverDestination;
    FSaveCursor: TCursor;
    FScheduler: TcxCustomScheduler;
    function GetActualHitTime: TDateTime;
    function GetController: TcxSchedulerViewController;
    function GetDateNavigator: TcxSchedulerCustomDateNavigator;
    function GetEvents: TcxSchedulerCachedEventList;
    function GetHitTest: TcxSchedulerViewHitTest;
  protected
    FHasConflicts: Boolean;
    FPrevAccepted: Boolean;
    FPrevHitFlags: Int64;
    FPrevHitTime: TDateTime;
    FPrevHitResource: TcxSchedulerStorageResourceItem;
    FStartHitTime: TDateTime;
    FStartHitFlags: Int64;
    FStartResource: TcxSchedulerStorageResourceItem;
    // virtual
    procedure BeginDrag; virtual;
    procedure CalculateConflicts; virtual;
    procedure CalculateDestination;
    function CanProcessDragOver: Boolean; virtual;
    function CanUpdateEventState(AEvent: TcxSchedulerControlEvent): Boolean;
    procedure CheckAccepted(var Accepted: Boolean); virtual;
    procedure CheckEventState(AEvent: TcxSchedulerControlEvent);
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); virtual;
    procedure EndDrag(Accepted: Boolean); virtual;
    function GetOriginHitTestMask: Int64; virtual;
    procedure GetOriginState; virtual;
    function HasChangedState: Boolean; virtual;
    function IsAtOrigin: Boolean; virtual;
    function IsShowResources: Boolean; virtual;
    function IsValidTime: Boolean; virtual;
    procedure RefreshCurrentView; virtual;
    procedure SetSelection; virtual;
    procedure UpdateHelperState(Accepted: Boolean); virtual;

    property SaveCursor: TCursor read FSaveCursor;
  public
    constructor Create(AScheduler: TcxCustomScheduler); virtual;

    property ActualHitTime: TDateTime read FActualHitTime;
    property Controller: TcxSchedulerViewController read GetController;
    property DateNavigator: TcxSchedulerCustomDateNavigator read GetDateNavigator;
    property Destination: TcxSchedulerDragOverDestination read FDestination;
    property Events: TcxSchedulerCachedEventList read GetEvents;
    property HasConflicts: Boolean read FHasConflicts;
    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
    property Scheduler: TcxCustomScheduler read FScheduler;
  end;

  { TcxDragEventHelper }

  TcxSchedulerDragObject = class(TcxDragControlObject)
  private
    FUseInternalCursors: Boolean;
    function GetDragEventHelper: TcxDragEventHelper;
    function GetDragEvents: TcxSchedulerFilteredEventList;
    function GetHasConflicts: Boolean;
    function GetScheduler: TcxCustomScheduler;
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  public
    constructor Create(AControl: TControl); override;
    procedure CalculateConflictsForDateNavigator(ADateNavigator: TcxSchedulerCustomDateNavigator);
    procedure DropToDateNavigator(ADateNavigator: TcxSchedulerCustomDateNavigator);

    property DragEventHelper: TcxDragEventHelper read GetDragEventHelper;
    property DragEvents: TcxSchedulerFilteredEventList read GetDragEvents;
    property HasConflicts: Boolean read GetHasConflicts;
    property Scheduler: TcxCustomScheduler read GetScheduler;
  end;

  TcxDragEventHelper = class(TcxDragHelper)
  private
    FDragObject: TDragObject;
    FValidatingDestinationTime: Boolean;
    function GetClones: TcxSchedulerFilteredEventList;
    function GetViewInfo: TcxSchedulerCustomViewViewInfo;
  protected
    FPrevIsDragCopy: Boolean;
    FTarget: TObject;
    procedure ApplyChanges; virtual;
    procedure BeginDrag; override;
    function CanProcessDragOver: Boolean; override;
    procedure CheckAccepted(var Accepted: Boolean); override;
    procedure CheckVisibility(Accepted: Boolean);
    procedure DateNavigatorEndDrag;
    procedure DeleteClone(AEvent: TcxSchedulerControlEvent; AStatus: TcxOccurrenceDateStatus);
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); override;
    procedure EndDrag(Accepted: Boolean); override;
    function GetClonesVisible(Accepted: Boolean): Boolean; virtual;
    function GetIsDragCopy: Boolean; virtual;
    procedure GetOriginState; override;
    function GetSourcesVisible(Accepted: Boolean): Boolean; virtual;
    function HasChangedState: Boolean; override;
    function IsValidNavigatorDate: Boolean; virtual;
    function IsValidTime: Boolean; override;
    procedure PrepareClones; virtual;
    procedure ProcessDateNavigator(ADateNavigator: TcxSchedulerCustomDateNavigator);
    procedure SetSelection; override;
    procedure Update(Accepted: Boolean = True);
    procedure UpdateClones;
    procedure UpdateHelperState(Accepted: Boolean); override;
    //DateNavigator processing
    procedure UpdateDateNavigatorClones(ADateNavigator: TcxSchedulerCustomDateNavigator); virtual;
    procedure UpdateDateNavigator(var Accepted: Boolean); virtual;
    //current view processing
    procedure UpdateViewClones; virtual;
    procedure UpdateViewClonesResources; virtual;
    procedure UpdateViewClonesTime; virtual;
    procedure ValidateDestinationTime;

    property Clones: TcxSchedulerFilteredEventList read GetClones;
    property DragObject: TDragObject read FDragObject;
    property IsDragCopy: Boolean read GetIsDragCopy;
    property ViewInfo: TcxSchedulerCustomViewViewInfo read GetViewInfo;
  end;

  { TcxEventSizingHelper }

  TcxEventSizingHelper = class(TcxDragHelper)
  private
    function GetEvent: TcxSchedulerControlEvent;
    function GetHitTest: TcxSchedulerViewHitTest;
  protected
    procedure BeginDrag; override;
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); override;
    procedure EndDrag(Accepted: Boolean); override;
    function GetDragCursor(Accepted: Boolean): TCursor; virtual;
    function IsValidTime: Boolean; override;
    // Event handling
    procedure CalcAllDayEvent; virtual;
    function CanApplyBounds(const AStart, AFinish: TDateTime): Boolean;
    function GetFinishTime: TDateTime; virtual;
    function GetStartTime: TDateTime; virtual;
    procedure UpdateEventBounds; virtual;

    property Event: TcxSchedulerControlEvent read GetEvent;
    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
  end;

  { TcxSchedulerDragAndDropObject }

  TcxSchedulerDragAndDropObject = class(TcxDragAndDropObject)
  private
    FSizingHelper: TcxEventSizingHelper;
    FScheduler: TcxCustomScheduler;
    function GetController: TcxSchedulerViewController;
    function GetHitTest: TcxSchedulerViewHitTest;
  protected
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    property Controller: TcxSchedulerViewController read GetController;
    property SizingHelper: TcxEventSizingHelper read FSizingHelper;
    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
    property Scheduler: TcxCustomScheduler read FScheduler;
  end;

  { TcxSchedulerHintController }

  TcxSchedulerHintController = class(TcxIUnknownObject, IUnknown, IcxMouseTrackingCaller)
  private
    FModernHint: TControl;
    FModernHintHelperContainer: TControl;
    FModernHintInfo: TcxSchedulerEventModernStyleHintInfo;
  protected
    FAutoHide: Boolean;
    FEvent: TcxSchedulerControlEvent;
    FHintWindow: THintWindow;
    FOwner: TcxCustomScheduler;
    FHintFlags: Integer;
    FHintRect: TRect;
    FHintText: string;
    HintWindowAnchor: Integer;
    FLockHint: Boolean;
    FShowing: Boolean;
    FTimer: TTimer;
    FViewMode: Boolean;
    // IcxMouseTrackingCaller
    procedure MouseLeave;
    // methods
    function CanShowHint: Boolean; virtual;
    procedure CheckHintClass;
    function GetSchedulerViewStyle: TcxSchedulerViewStyle;
    procedure HideHint; virtual;
    procedure OnModernStyleHintHide(Sender: TObject);
    procedure ShowHint; virtual;
    procedure StartHideHintTimer;
    procedure StartShowHintTimer;
    procedure StopTimer;
    procedure TimerHandler(Sender: TObject);

    property SchedulerViewStyle: TcxSchedulerViewStyle read GetSchedulerViewStyle;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    procedure Activate(const AHintRect: TRect; const AHintText: string;
      AImmediateHint: Boolean = False; AAutoHide: Boolean = True); overload; virtual;
    procedure Activate(AEvent: TcxSchedulerControlEvent; const AHintRect: TRect;
      AImmediateHint: Boolean = False; AAutoHide: Boolean = False); overload; virtual;
    function CalcHintRect(AMaxWidth: Integer;
      const AHintText: string; AFlags: Integer): TRect;
    procedure Hide;
    procedure Reset;
    property Scheduler: TcxCustomScheduler read FOwner;
    property ViewMode: Boolean read FViewMode write FViewMode;
    property Showing: Boolean read FShowing;
  end;

  { TcxSchedulerEventHitTestController }

  TcxSchedulerEventHitTestController = class
  private
    function GetHintController: TcxSchedulerHintController;
    function GetHitTest: TcxSchedulerViewHitTest;
  protected
    FOwner: TcxCustomScheduler;
    FPrevHintEvent: TcxSchedulerControlEvent;
  public
    constructor Create(AOwner: TcxCustomScheduler); virtual;
    procedure HideEventHint;
    procedure MouseMove(X, Y: Integer; AShift: TShiftState); virtual;

    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
    property HintController: TcxSchedulerHintController read GetHintController;
    property Scheduler: TcxCustomScheduler read FOwner;
  end;

  { TcxSchedulerCustomView }

  TcxSchedulerCustomView = class(TcxSchedulerSubControl)
  private
    FCalculatedHintBounds: Boolean;
    FCanShow: Boolean;
    function GetActive: Boolean;
    function GetCanShow: Boolean;
    function GetController: TcxSchedulerViewController;
    function GetDragCloneEventList: TcxSchedulerFilteredEventList;
    function GetEventList: TcxSchedulerCachedEventList;
    function GetHitTest: TcxSchedulerViewHitTest;
    function GetOptionsView: TcxSchedulerOptionsView;
    function GetResources: TcxSchedulerStorageResourceItems;
    function GetSelectedDays: TcxSchedulerDateList;
    function GetWorkDays: TDays;
    function GetWorkFinish: TDateTime;
    function GetWorkStart: TDateTime;
    procedure SetActive(AValue: Boolean);
    procedure SetCanShow(AValue: Boolean);
  protected
    function CanDeactivateOnDateNavigatorSelectionChange: Boolean; virtual;
    function CanEventEdit(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean; virtual;
    function CanSelectIndividualDays: Boolean; virtual;
    function CanSelectPeriod: Boolean; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckResourceNavigatorKind; virtual;
    function CheckEventsVisibility: Boolean; virtual;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    procedure DateChanged; virtual;
    procedure DeactivateView; virtual;
    procedure DoAfterEventDeleting; virtual;
    procedure DoBeforeReallyDeleting(AEvent: TcxSchedulerControlEvent); virtual;
    procedure DoLayoutChanged; override;
    function DoShowPopupMenu(X, Y: Integer): Boolean; virtual;
    function EventContentSelected(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    procedure EventsListChanged; virtual;
    function GetClientRect: TRect; override;
    function GetControlCanvas: TcxCanvas; virtual;
    function GetDateOffset: Integer;
    function GetDragObjectClass: TDragControlObjectClass; virtual;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; virtual;
    function GetGroupingKind: TcxSchedulerGroupingKind; virtual;
    function GetHScrollBarBounds: TRect; override;
    function GetEditRectForEvent(AEvent: TcxSchedulerControlEvent;
      const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem): TRect; virtual;
    function GetEditStyle(AEvent: TcxSchedulerControlEvent): TcxCustomEditStyle; virtual;
    function GetEditProperties(AEvent: TcxSchedulerControlEvent): TcxCustomEditProperties; virtual;
    function GetEditWithSingleLineEditor(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function GetEventHintText(AEvent: TcxSchedulerControlEvent): string; virtual;
    function GetSelFinishForInitEventBySelectedTime: TDateTime; virtual;
    function GetSelStartForInitEventBySelectedTime: TDateTime; virtual;
    function GetEventVisibility(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function GetFirstVisibleDate: TDateTime; virtual;
    function GetFirstVisibleTime: TDateTime; virtual;
    function GetLastVisibleDate: TDateTime; virtual;
    function GetLastVisibleTime: TDateTime; virtual;
    procedure GetRangeControlRange(out AMin, AMax: TDateTime); virtual;
    procedure GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList); virtual;
    procedure GetRangeControlTotalRange(out AMin, AMax: TDateTime); virtual;
    function GetSchedulerLookAndFeel(ADialogs: Boolean = False): TcxLookAndFeel;
    function GetScrollTimeHint: string; virtual;
    function GetShowEventsWithoutResource: Boolean; virtual;
    function GetSizeGripBounds: TRect; override;
    function GetTimeIncrement: TDateTime; virtual;
    function GetVScrollBarBounds: TRect; override;
    function GetViewContentRect: TRect; virtual;
    function GetVisibleDaysRange: Integer; virtual;
    procedure InitEventBySelectedTime(AEvent: TcxSchedulerEvent; AllDay: Boolean;
      ARecurrence: Boolean; AInplaceEditing: Boolean); virtual;
    function IsAllDaySelection: Boolean;
    function IsDayView: Boolean; virtual;
    function IsInplaceEditingEnabled: Boolean; virtual;
    function IsResourceNavigatorAllowed: Boolean; virtual;
    function IsShowResources: Boolean; virtual;
    function IsWorkTime(AResourceItem: TcxSchedulerStorageResourceItem; const ADateTime: TDateTime): Boolean;
    procedure MakeEventVisible(AEvent: TcxSchedulerControlEvent;
      const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem); virtual;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; virtual;
    procedure PeriodChanged; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollSelectedDays(AScrollDelta: Integer); overload; virtual;
    procedure ScrollSelectedDays(AForward: Boolean;
      ANeedDate: TDateTime; AIsByPage: Boolean); overload; virtual;
    procedure ScrollVisibleDays(AScrollUp: Boolean); virtual;
    procedure SelectedDaysChanged; virtual;
    function ShowTaskComplete: Boolean; virtual;
    procedure TimeChanged; virtual;
    procedure UpdateDateNavigatorSelection;
    procedure ValidateContentPopupMenuItems(var AItems: TcxSchedulerContentPopupMenuItems); virtual;
    procedure ValidateSelectionFinishTime(var ADateTime: TDateTime); virtual;
    procedure VisibleChanged; override;
    // hint
    procedure HideHintOnScroll(AScrollCode: TScrollCode); virtual;
    procedure ShowHintOnScroll(const ADate: TDateTime); overload; virtual;
    procedure ShowHintOnScroll(const AHintText: string; AScrollBarKind: TScrollBarKind); overload; virtual;
    procedure VisibleRangeChanged;

    // touch scrollbars
    procedure DoCreateScrollBars; virtual;
    procedure DoDestroyScrollBars; virtual;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; virtual;
    procedure InitScrollBars; virtual;

    property CalculatedHintBounds: Boolean read FCalculatedHintBounds;
    property Controller: TcxSchedulerViewController read GetController;
    property EventList: TcxSchedulerCachedEventList read GetEventList;
    property FirstVisibleTime: TDateTime read GetFirstVisibleTime;
    property LastVisibleTime: TDateTime read GetLastVisibleTime;
    property OptionsView: TcxSchedulerOptionsView read GetOptionsView;
    property Resources: TcxSchedulerStorageResourceItems read GetResources;
    property WorkDays: TDays read GetWorkDays;
    property WorkStart: TDateTime read GetWorkStart;
    property WorkFinish: TDateTime read GetWorkFinish;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;

    property Active: Boolean read GetActive write SetActive default False;
    property CanShow: Boolean read GetCanShow write SetCanShow default True;
    property DragCloneEventList: TcxSchedulerFilteredEventList read GetDragCloneEventList;
    property FirstVisibleDate: TDateTime read GetFirstVisibleDate;
    property HitTest: TcxSchedulerViewHitTest read GetHitTest;
    property LastVisibleDate: TDateTime read GetLastVisibleDate;
    property SelectedDays: TcxSchedulerDateList read GetSelectedDays;
    property StartOfWeek;
  end;

  { TcxSchedulerCustomViewViewInfo }

  TcxSchedulerCustomViewViewInfo = class(TcxSchedulerSubControlViewInfo)
  private
    function GetResourceNavigator: TcxSchedulerResourceNavigator; inline;
    function GetScheduler: TcxCustomScheduler; inline;
    function GetSchedulerStyle: TcxSchedulerViewStyle; inline;
    function GetView: TcxSchedulerCustomView; inline;
  protected
    FEvents: TcxSchedulerCachedEventList;
    FSelectedDays: TcxSchedulerDateList;
    procedure CheckResourceNavigator; virtual;
    procedure CheckResourceNavigatorKind; virtual;
    function DoGetEventDisplayText(AEvent: TcxSchedulerControlEvent): string; virtual;
    function DoSchedulerMoreEventsButtonClick: Boolean;
    function DoSchedulerNavigationButtonClick(AnInterval: TDateTime;
      AResource: TcxSchedulerStorageResourceItem): Boolean;
    function GetEventHint(AEvent: TcxSchedulerControlEvent): string; virtual;
    function GetResourceScrollBarKind: TScrollBarKind; virtual;
    function GetSchedulerEventsList: TcxSchedulerCachedEventList;
    procedure DeleteClone(AEvent: TcxSchedulerControlEvent); virtual;
    procedure SetEventsVisibility(AShowSources, AShowClones: Boolean; AForceRepaint: Boolean = False); virtual;

    property Events: TcxSchedulerCachedEventList read FEvents;
    property ResourceNavigator: TcxSchedulerResourceNavigator read GetResourceNavigator;
    property Scheduler: TcxCustomScheduler read GetScheduler;
    property SchedulerStyle: TcxSchedulerViewStyle read GetSchedulerStyle;
    property SelectedDays: TcxSchedulerDateList read FSelectedDays;
    property View: TcxSchedulerCustomView read GetView;
  end;

  { TcxSchedulerCustomDataNavigator }

  IcxExternalDateNavigatorListener = interface
  ['{32293211-4D89-4383-A95C-23B95C3A783D}']
    procedure StorageChanged;
    procedure SchedulerChanged;
    procedure SchedulerRemoved;
  end;

  TcxSchedulerCustomDateNavigator = class(TcxSchedulerSubControl)
  private
    FLockCount: Integer;
    FPrevColCount: Integer;
    FPrevRowCount: Integer;
    FSaveRealFirstDate: TDateTime;
    FSaveRealLastDate: TDateTime;
    FSaveSelectionList: TcxSchedulerDateList;
    function GetEventDays: TcxSchedulerDateList;
    function GetHintController: TcxSchedulerHintController;
    function GetHolidayDays: TcxSchedulerDateList;
  protected
    FSavedSize: TSize;
    procedure BoundsChanging; override;
    procedure BoundsChanged; override;
    function CanMultiSelect: Boolean; virtual;
    procedure CheckSizes; virtual; abstract;
    procedure CheckChanges; virtual;
    procedure CheckCurrentDate; virtual;
    procedure ClearDragging; virtual; abstract;
    procedure DoPeriodChangedEvent; virtual; abstract;
    procedure DoSelectionChangedEvent; virtual; abstract;
    procedure DoScrollSelection(AForward: Boolean; ANeedDate: TDateTime; AIsByPage: Boolean); overload; virtual; abstract;
    procedure DoScrollSelection(AScrollDelta: Integer); overload; virtual; abstract;
    procedure EnsureSelectionVisible; virtual; abstract;
    procedure GetCalendarDimension(out AColCount, ARowCount: Integer); virtual; abstract;
    function GetMonthSize: TSize; virtual;
    function GetSelection: TcxSchedulerDateList; virtual;
    function GetShowDatesContainingEventsInBold: Boolean; virtual; abstract;
    function GetShowDatesContainingHolidaysInColor: Boolean; virtual; abstract;
    function GetRealFirstDate: TDateTime; virtual; abstract;
    function GetRealLastDate: TDateTime; virtual; abstract;
    function IsSchedulerLocked: Boolean;
    procedure Loaded; virtual;
    procedure MakeSelectionVisible; virtual; abstract;
    procedure PeriodChanged; override;
    procedure RefreshDays; virtual; abstract;
    procedure SaveSize;
    procedure SaveState; virtual;
    procedure ScrollSelection(AForward: Boolean; ANeedDate: TDateTime; AIsByPage: Boolean); overload;
    procedure ScrollSelection(AScrollDelta: Integer); overload;
    procedure SetIntegralSizes; virtual; abstract;
    procedure UpdateDragging; virtual; abstract;
    procedure UpdateSelection; virtual; abstract;

    property EventDays: TcxSchedulerDateList read GetEventDays;
    property HintController: TcxSchedulerHintController read GetHintController;
    property HolidayDays: TcxSchedulerDateList read GetHolidayDays;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CancelUpdates;
    procedure EndUpdate;
  end;

  { TcxSchedulerClipboardController }

  TcxSchedulerClipboardController = class
  private
    FScheduler: TcxCustomScheduler;
    FStream: TMemoryStream;
    FStreamReader: TcxReader;
    FStreamWriter: TcxWriter;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure RegisterClipboardFormat;
  protected
    procedure CalculateAnchorForResource(AEvents: TcxSchedulerFilteredEventList;
      const AResourceID: Variant; var Anchor: TDateTime);
    procedure DeleteSelectedEvents; virtual;
    function GetClipboard: TClipboard; virtual;
    function GetClipboardToStream: Boolean;
    function GetSelectionAsStream: Boolean;
    function GetStreamAsEvents(AEvents: TcxSchedulerFilteredEventList; var Anchor: TDateTime): Boolean;
    function IsClipboardBusy: Boolean;
    procedure InsertEvents(AEvents: TcxSchedulerFilteredEventList; Anchor: TDateTime);
    function KeyDown(var AKey: Word; AShift: TShiftState): Boolean;
    function KeyPress(var AKey: Char): Boolean;
    procedure RestoreEvent(var AEvent: TcxSchedulerControlEvent);
    procedure SaveEvent(AEvent: TcxSchedulerControlEvent);
    procedure SetStreamToClipboard;
    function ValidateStream: Boolean;

    property Stream: TMemoryStream read FStream;
    property StreamReader: TcxReader read FStreamReader;
    property StreamWriter: TcxWriter read FStreamWriter;
  public
    constructor Create(AScheduler: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    function CanCopy: Boolean; virtual;
    function CanPaste: Boolean; virtual;
    procedure Copy;
    procedure Cut;
    procedure Paste;

    property Clipboard: TClipboard read GetClipboard;
    property Scheduler: TcxCustomScheduler read FScheduler;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  end;

  { IcxSchedulerStylesAdapter }

  IcxSchedulerStylesAdapter = interface
  ['{0BFEA90D-0CE8-4ED1-88E8-71A3396186F3}']
    function GetContentParams(const ADateTime: TDateTime;
      AResource: TcxSchedulerStorageResourceItem): TcxViewParams; overload;
    function GetContentParams(const ADateTime: TDateTime; ALightColor: Boolean;
      AResource: TcxSchedulerStorageResourceItem): TcxViewParams; overload;
    function GetDayHeaderParams(const ADateTime: TDateTime): TcxViewParams;
    function GetEventParams(AEvent: TcxSchedulerEvent): TcxViewParams;
    function GetResourceHeaderParams(AResource: TcxSchedulerStorageResourceItem): TcxViewParams;
    // hard style
    function GetDayHeaderStyle: TcxStyle;
    function GetResourceHeaderStyle: TcxStyle;
  end;

  { TcxSchedulerStyles }

  TcxSchedulerOnGetDayHeaderStyleEvent = procedure(Sender: TObject;
    const ADate: TDateTime; var AStyle: TcxStyle) of object;
  TcxSchedulerOnGetResourceHeaderStyleEvent = procedure(Sender: TObject;
    AResource: TcxSchedulerStorageResourceItem; var AStyle: TcxStyle) of object;
  TcxSchedulerOnGetContentStyleEvent = procedure(Sender: TObject;
    AResource: TcxSchedulerStorageResourceItem; const ADateTime: TDateTime;
    var AStyle: TcxStyle) of object;
  TcxSchedulerOnGetEventStyleEvent = procedure(Sender: TObject;
    AEvent: TcxSchedulerEvent; var AStyle: TcxStyle) of object;

  TcxSchedulerStyles = class(TcxStyles, IcxSchedulerStylesAdapter)
  private
    FScheduler: TcxCustomScheduler;
    FOnGetContentStyle: TcxSchedulerOnGetContentStyleEvent;
    FOnGetDayHeaderStyle: TcxSchedulerOnGetDayHeaderStyleEvent;
    FOnGetEventStyle: TcxSchedulerOnGetEventStyleEvent;
    FOnGetResourceHeaderStyle: TcxSchedulerOnGetResourceHeaderStyleEvent;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetPainterHelperClass: TcxSchedulerPainterHelperClass;
    function GetResources: TcxSchedulerStorageResourceItems;
    function GetSchedulerViewStyle: TcxSchedulerViewStyle;
  protected
    procedure Changed(AIndex: Integer); override;

    function EventContentSelected(AEvent: TcxSchedulerControlEvent): Boolean;
    function GetDefaultContentColor(AResourceIndex: Integer): TColor;
    function GetDefaultEventColor(AIsHeaderEvent: Boolean): TColor;
    procedure GetDefaultHeaderViewParams(var AParams: TcxViewParams);
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;

    // IcxSchedulerStylesAdapter
    function GetDayHeaderStyle: TcxStyle;
    function GetEventParams(AEvent: TcxSchedulerEvent): TcxViewParams;
    function GetResourceHeaderStyle: TcxStyle;

    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelperClass;
    property Resources: TcxSchedulerStorageResourceItems read GetResources;
    property SchedulerViewStyle: TcxSchedulerViewStyle read GetSchedulerViewStyle;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetBackgroundParams: TcxViewParams;
    function GetContentParams(const ADateTime: TDateTime; AResource: TcxSchedulerStorageResourceItem = nil): TcxViewParams; overload;
    function GetContentParams(const ADateTime: TDateTime; ALightColor: Boolean; AResource: TcxSchedulerStorageResourceItem = nil): TcxViewParams; overload;
    function GetDayHeaderParams(const ADate: TDateTime): TcxViewParams;
    function GetGroupSeparatorParams: TcxViewParams;
    function GetResourceHeaderParams(AResource: TcxSchedulerStorageResourceItem): TcxViewParams;
    function GetSelectionParams: TcxViewParams;
    function GetSplitterParams(AKind: TcxSchedulerSplitterKind): TcxViewParams;
    function GetEventContentParams(AEvent: TcxSchedulerEvent): TcxViewParams;
    function IsEventStyleAssigned(AEvent: TcxSchedulerEvent): Boolean;
    // define colors functions
    property Scheduler: TcxCustomScheduler read FScheduler;
  published
    property Background: TcxStyle index cxcsBackground read GetValue write SetValue;
    property Content: TcxStyle index cxcsContent read GetValue write SetValue;
    property Event: TcxStyle index cxcsEvent read GetValue write SetValue;
    property GroupSeparator: TcxStyle index cxcsGroupSeparator read GetValue write SetValue;
    property DayHeader: TcxStyle index cxcsDayHeader read GetValue write SetValue;
    property HorzSplitter: TcxStyle index cxcsHSplitter read GetValue write SetValue;
    property ResourceHeader: TcxStyle index cxcsResourceHeader read GetValue write SetValue;
    property Selection: TcxStyle index cxcsSelection read GetValue write SetValue;
    property VertSplitter: TcxStyle index cxcsVSplitter read GetValue write SetValue;

    property OnGetContentStyle: TcxSchedulerOnGetContentStyleEvent read FOnGetContentStyle write FOnGetContentStyle;
    property OnGetDayHeaderStyle: TcxSchedulerOnGetDayHeaderStyleEvent read FOnGetDayHeaderStyle write FOnGetDayHeaderStyle;
    property OnGetEventStyle: TcxSchedulerOnGetEventStyleEvent read FOnGetEventStyle write FOnGetEventStyle;
    property OnGetResourceHeaderStyle: TcxSchedulerOnGetResourceHeaderStyleEvent read FOnGetResourceHeaderStyle write FOnGetResourceHeaderStyle;
  end;

  { TcxSchedulerOptionsBehavior }

  TcxSchedulerOptionsBehavior = class(TPersistent)
  private
    FHotTrack: Boolean;
    FModalDialogs: Boolean;
    FOwner: TcxCustomScheduler;
    FSelectOnRightClick: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TcxCustomScheduler);

    procedure Assign(Source: TPersistent); override;
  published
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
    property ModalDialogs: Boolean read FModalDialogs write FModalDialogs default False;
    property SelectOnRightClick: Boolean read FSelectOnRightClick
      write FSelectOnRightClick default False;
  end;

  { TcxCustomScheduler }

  TcxSchedulerCanShowViewEvent = procedure(Sender: TObject;
    AView: TcxSchedulerCustomView; var Allow: Boolean) of object;
  TcxSchedulerGetEventEditPropertiesEvent = procedure(Sender: TObject;
    AEvent: TcxSchedulerControlEvent; var AProperties: TcxCustomEditProperties) of object;
  TcxSchedulerGetEventModernStyleHintInfo = procedure(Sender: TObject;
    AEvent: TcxSchedulerControlEvent; AInfo: TcxSchedulerEventModernStyleHintInfo) of object;
  TcxSchedulerGetEventText = procedure(Sender: TObject;
    AEvent: TcxSchedulerControlEvent; var AText: string) of object;
  TcxSchedulerInitEditEvent = procedure(Sender: TObject; AEdit: TcxCustomEdit) of object;
  TcxSchedulerViewTypeChangedEvent = procedure(Sender: TObject;
    APrevView, ANewView: TcxSchedulerCustomView) of object;
  TcxSchedulerIsWorkTimeEvent = procedure(Sender: TObject; AResource: TcxSchedulerStorageResourceItem;
    const ATime: TDateTime; var AIsWork: Boolean) of object;
  TcxSchedulerBeforeDeleting = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; var Allow: Boolean) of object;
  TcxSchedulerBeforeDragEvent = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; X, Y: Integer; var Allow: Boolean) of object;
  TcxSchedulerBeforeEditing = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; AInplace: Boolean; var Allow: Boolean) of object;
  TcxSchedulerAfterDragEvent = procedure (Sender: TcxCustomScheduler;
    Target: TObject; X, Y: Integer; var Accept: Boolean) of object;
  TcxSchedulerBeforeSizingEvent = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; X, Y: Integer; var Allow: Boolean) of object;
  TcxSchedulerAfterSizingEvent = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; X, Y: Integer; var Accept: Boolean) of object;
  TcxSchedulerAfterEditing = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent) of object;
  TcxSchedulerMoreEventsButtonClickEvent = procedure (Sender: TcxCustomScheduler;
    var AHandled: Boolean) of object;
  TcxSchedulerNavigationButtonClickEvent = procedure (Sender: TcxCustomScheduler;
    AnInterval: TDateTime; AResource: TcxSchedulerStorageResourceItem;
    var AHandled: Boolean) of object;
  TcxSchedulerScaleScrollEvent = procedure (Sender: TcxCustomScheduler;
    AStartDateTime, AFinishDateTime: TDateTime) of object;
  TcxSchedulerEventSelectionChangedEvent = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent) of object;
  TcxSchedulerShowDateHintEvent = procedure (Sender: TObject; const ADate: TDateTime;
    var AHintText: string; var AAllow: Boolean) of object;

  TcxCustomScheduler = class(TcxControl, IcxSchedulerStorageListener, IcxFormatControllerListener,
      IcxFormatControllerListener2, IcxStoredObject, IdxSkinSupport)
  private
    FActiveControl: TcxSchedulerSubControl;
    FAligningSubControls: Boolean;
    FBackground: TcxSchedulerSubControl;
    FBoundsChanging: Boolean;
    FCanModified: Boolean;
    FCaptureControl: TcxSchedulerSubControl;
    FClipboardController: TcxSchedulerClipboardController;
    FContentPopupMenu: TcxSchedulerContentPopupMenu;
    FContentPopupMenuEvents: TNotifyEvent;
    FControlBox: TcxSchedulerControlBox;
    FControlFlags: TcxControlFlags;
    FCurrentView: TcxSchedulerCustomView;
    FDateNavigator: TcxSchedulerCustomDateNavigator;
    FDefaultProperties: TcxCustomEditProperties;
    FDialogsLookAndFeel: TcxLookAndFeel;
    FDialogsStyle: string;
    FEditController: TcxSchedulerEditController;
    FEditStyle: TcxCustomEditStyle;
    FEventDays: TcxSchedulerDateList;
    FEventEditInfo: TcxSchedulerEventEditInfo;
    FEventHitTestController: TcxSchedulerEventHitTestController;
    FEventImages: TCustomImageList;
    FEventList: TcxSchedulerCachedEventList;
    FEventOperations: TcxSchedulerEventOperations;
    FEventPopupMenu: TcxSchedulerEventPopupMenu;
    FEventPopupMenuEvents: TNotifyEvent;
    FFirstVisibleResourceIndex: Integer;
    FHintController: TcxSchedulerHintController;
    FHolidayDays: TcxSchedulerDateList;
    FHorzSplitter: TcxSchedulerSplitter;
    FHorzSplitterShowing: Boolean;
    FIsDragCanceled: Boolean;
    FKeyboardListener: TcxSchedulerSubControl;
    FListeners: TInterfaceList;
    FLockCount: Integer;
    FLockRefresh: Integer;
    FOnInitEdit: TcxSchedulerInitEditEvent;
    FOptionsBehavior: TcxSchedulerOptionsBehavior;
    FOptionsCustomize: TcxSchedulerOptionsCustomize;
    FOptionsView: TcxSchedulerOptionsView;
    FPrevBounds: TRect;
    FPrevCopyDragDrop: Boolean;
    FPrevMousePos: TPoint;
    FResourceNavigator: TcxSchedulerResourceNavigator;
    FResourceNavigatorEvents: TNotifyEvent;
    FSelectedDays: TcxSchedulerDateList;
    FSelFinish: TDateTime;
    FSelResource: TcxSchedulerStorageResourceItem;
    FSelStart: TDateTime;
    FStorage: TcxCustomSchedulerStorage;
    FStoringName: string;
    FStyles: TcxSchedulerStyles;
    FStylesEvents: TNotifyEvent;
    FSubControls: TList;
    FSubControlsCreated: Boolean;
    FTabOrdersList: TcxSchedulerEventList;
    FTextEditProperties: TcxTextEditProperties;
    FVertSplitter: TcxSchedulerSplitter;
    FVertSplitterShowing: Boolean;
    FVisibleChangedCount: Integer;
    FUpdateTimeTimer: TTimer;
    FOnAfterDragEvent: TcxSchedulerAfterDragEvent;
    FOnAfterEditing: TcxSchedulerAfterEditing;
    FOnAfterSizingEvent: TcxSchedulerAfterSizingEvent;
    FOnBeforeDeleting: TcxSchedulerBeforeDeleting;
    FOnBeforeDragEvent: TcxSchedulerBeforeDragEvent;
    FOnBeforeEditing: TcxSchedulerBeforeEditing;
    FOnBeforeSizingEvent: TcxSchedulerBeforeSizingEvent;
    FOnCanShowView: TcxSchedulerCanShowViewEvent;
    FOnEventSelectionChanged: TcxSchedulerEventSelectionChangedEvent;
    FOnFirstVisibleResourceChanged: TNotifyEvent;
    FOnGetEventDisplayText: TcxSchedulerGetEventText;
    FOnGetEventEditProperties: TcxSchedulerGetEventEditPropertiesEvent;
    FOnGetEventHintText: TcxSchedulerGetEventText;
    FOnGetEventModernStyleHintInfo: TcxSchedulerGetEventModernStyleHintInfo;
    FOnIsWorkTime: TcxSchedulerIsWorkTimeEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnMoreEventsButtonClick: TcxSchedulerMoreEventsButtonClickEvent;
    FOnNavigationButtonClick: TcxSchedulerNavigationButtonClickEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnScaleScroll: TcxSchedulerScaleScrollEvent;
    FOnShowDateHint: TcxSchedulerShowDateHintEvent;
    FOnViewTypeChanged: TcxSchedulerViewTypeChangedEvent;
    procedure CreateUpdateTimeTimer;
    function GetActiveHitTest: TcxSchedulerSubControlHitTest;
    function GetCaptureControl: TcxSchedulerSubControl;
    function GetCaptureController: TcxSchedulerSubControlController;
    function GetSelectedEventCount: Integer;
    function GetSelectedEvent(AIndex: Integer): TcxSchedulerControlEvent;
    function GetSelFinish: TDateTime;
    function GetSelStart: TDateTime;
    function GetIsDynamicUpdate: Boolean;
    function GetStartOfWeek: TDay;
    function GetStorageActive: Boolean;
    function GetStorageValid: Boolean;
    function GetSubControl(AIndex: Integer): TcxSchedulerSubControl;
    function GetSubControlCount: Integer;
    function GetVisibleEventCount: Integer;
    function GetVisibleEvent(AIndex: Integer): TcxSchedulerControlEvent;
    procedure InitEventBySelection(AEvent: TcxSchedulerEvent;
      AllDay: Boolean; ARecurrence: Boolean; AInplaceEditing: Boolean);
    procedure SetCaptureControl(AValue: TcxSchedulerSubControl);
    procedure SetContentPopupMenu(AValue: TcxSchedulerContentPopupMenu);
    procedure SetControlBox(AValue: TcxSchedulerControlBox);
    procedure SetDialogsLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetDialogsStyle(const Value: string);
    procedure SetEventImages(AValue: TCustomImageList);
    procedure SetEventOperations(AValue: TcxSchedulerEventOperations);
    procedure SetEventPopupMenu(AValue: TcxSchedulerEventPopupMenu);
    procedure SetFirstVisibleResourceIndex(AValue: Integer);
    procedure SetOptionsBehavior(AValue: TcxSchedulerOptionsBehavior);
    procedure SetOptionsCustomize(AValue: TcxSchedulerOptionsCustomize);
    procedure SetOptionsView(AValue: TcxSchedulerOptionsView);
    procedure SetResourceNavigator(AValue: TcxSchedulerResourceNavigator);
    procedure SetStyles(AValue: TcxSchedulerStyles);
    procedure SetStorage(AValue: TcxCustomSchedulerStorage);
    procedure UpdateTimeHandler(Sender: TObject);
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMTimeChange(var Message: TWMTimeChange); message WM_TIMECHANGE;
    procedure ReadSelectionData(AReader: TReader);
    procedure WriteSelectionData(AWriter: TWriter);
  protected
    FStoredClientBounds: TRect;

    procedure BiDiModeChanged; override;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;

    // IcxSchedulerStorageListener
    procedure StorageChanged(Sender: TObject); virtual;
    procedure StorageRemoved(Sender: TObject);
    // IcxFormatControllerListener
    procedure DoStartOfWeekChanged(AOldStartOfWeek, ANewStartOfWeek: TDay); virtual;
    procedure FormatChanged; virtual;
    // IcxFormatControllerListener2
    procedure TimeChanged;
    // IcxStoredObject
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // from TControl
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // IcxExternalDateNavigatorListener
    procedure AddListener(AListener: IcxExternalDateNavigatorListener);
    procedure NotifySchedulerChanged;
    procedure NotifySchedulerRemoved;
    procedure NotifyStorageChanged; virtual;
    procedure RemoveListener(AListener: IcxExternalDateNavigatorListener);

    // methods
    function AllowCompositionPainting: Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;

    //layout
    procedure AlignSubControls(Sender: TcxSchedulerSubControl = nil); virtual;
    procedure BoundsChanged; override;
    procedure CalcHorizontalSplitterBounds; virtual;
    procedure CalcVerticalSplitterBounds; virtual;
    procedure CalcLayout; virtual;
    procedure CalcLayoutViewRight; virtual;
    procedure CalcLayoutViewLeft; virtual;
    procedure CalcLayoutViewTop; virtual;
    procedure CalcLayoutViewBottom; virtual;
    procedure CalcSplittersBounds;
    procedure CheckHorzSplitterBounds;
    procedure CheckSplittersVisibilityChanging; virtual;
    procedure ClearAllCachedData; virtual;
    function GetActualBiDiModeRect(const R: TRect): TRect;
    function IsHorzSplitterVisible: Boolean; virtual;
    function IsVertSplitterVisible: Boolean; virtual;
    procedure UpdateControlsBoundsOnHorzSplitterShowing;
    procedure UpdateControlsBoundsOnVertSplitterShowing;

    function AllowTouchScrollUIMode: Boolean; override;
    function CanDeactivateOnSelectionChanged(AView: TcxSchedulerCustomView): Boolean; virtual;
    function CanIntersect(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function CanModified: Boolean; virtual;
    function CanSelectPeriod: Boolean; virtual;
    function CanShowEventDialog: Boolean;
    procedure CheckEventListTimeRangeUsing;
    function CreateBackground: TcxSchedulerSubControl; virtual;
    function CreateClipboardController: TcxSchedulerClipboardController; virtual;
    function CreateContentPopupMenu: TcxSchedulerContentPopupMenu; virtual;
    function CreateControlBox: TcxSchedulerControlBox; virtual;
    function CreateDateNavigator: TcxSchedulerCustomDateNavigator; virtual;
    function CreateDefaultView: TcxSchedulerCustomView; virtual;
    function CreateDefaultEditProperties: TcxCustomEditProperties; virtual;
    function CreateEditController: TcxSchedulerEditController; virtual;
    function CreateEventHitTestController: TcxSchedulerEventHitTestController; virtual;
    function CreateEventList: TcxSchedulerCachedEventList; virtual;
    function CreateEventOperations: TcxSchedulerEventOperations; virtual;
    function CreateEventPopupMenu: TcxSchedulerEventPopupMenu; virtual;
    function CreateHintController: TcxSchedulerHintController; virtual;
    function CreateOptionsCustomize: TcxSchedulerOptionsCustomize; virtual;
    function CreateOptionsView: TcxSchedulerOptionsView; virtual;
    function CreateResourceNavigator: TcxSchedulerResourceNavigator; virtual;
    function CreateSplitter(AKind: TcxSchedulerSplitterKind): TcxSchedulerSplitter; virtual;
    function CreateStyles: TcxSchedulerStyles; virtual;
    procedure CreateSubClasses; virtual;
    procedure DateNavigatorSelectionChanged; virtual;
    procedure DblClick; override;
    procedure DestroySubClasses; virtual;
    procedure DoAfterDragEvent(Target: TObject; X, Y: Integer; var Accept: Boolean); virtual;
    procedure DoAfterEditing(AEvent: TcxSchedulerControlEvent); virtual;
    procedure DoAfterSizingEvent(AEvent: TcxSchedulerControlEvent; X, Y: Integer; var Accept: Boolean); virtual;
    function DoBeforeDeleting(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function DoBeforeDragEvent(AEvent: TcxSchedulerControlEvent; X, Y: Integer): Boolean; virtual;
    function DoBeforeEditing(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean; virtual;
    function DoBeforeSizingEvent(AEvent: TcxSchedulerControlEvent; X, Y: Integer): Boolean; virtual;
    procedure DoCalculateLayout(AControl: TcxSchedulerSubControl); virtual;
    procedure DoCancelMode; override;
    procedure DoControllerReset(AControl: TcxSchedulerSubControl); virtual;
    procedure DoCreateEventUsingInplaceEdit(AKey: Char = #0); virtual;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    procedure DoEventSelectionChanged(AEvent: TcxSchedulerControlEvent);
    procedure DoFirstVisibleResourceChanged; virtual;
    procedure DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer); override;
    procedure DoGetEventDisplayText(AEvent: TcxSchedulerControlEvent; var AText: string); virtual;
    procedure DoGetEventEditProperties(AEvent: TcxSchedulerControlEvent; var AProperties: TcxCustomEditProperties); virtual;
    procedure DoHitTestRecalculate(AControl: TcxSchedulerSubControl); virtual;
    procedure DoInitEdit(AEdit: TcxCustomEdit); virtual;
    function DoIsWorkTime(AResourceItem: TcxSchedulerStorageResourceItem; const ADateTime: TDateTime): Boolean;
    procedure DoCanShowView(AView: TcxSchedulerCustomView; var Allow: Boolean); virtual;
    procedure DoLayoutChanged; virtual;
    procedure DoLayoutChangedEvent; virtual;
    function DoMoreEventsButtonClick: Boolean; virtual;
    function DoNavigationButtonClick(AnInterval: TDateTime; AResource: TcxSchedulerStorageResourceItem): Boolean; virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoScaleScroll; virtual;
    function DoShowDateHint(const ADate: TDateTime; var AHintText: string): Boolean; virtual;
    function DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean; override;
    procedure DoViewTypeChanged(ANewView: TcxSchedulerCustomView); virtual;
    procedure DoUpdateTime; virtual;
    procedure DragCanceled; override;
    procedure DrawSplitters; virtual;
    procedure FirstVisibleResourceChanged; virtual;
    procedure FontChanged; override;
    procedure FocusChanged; override;
    function GetClientBounds: TRect; override;
    function GetControlFromPoint(const APoint: TPoint): TcxSchedulerSubControl; virtual;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetDateTimeHelper: TcxSchedulerDateTimeHelperClass; virtual;
    function GetDateNavigatorLoadedSize: TSize;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetEventEditInfo(AEvent: TcxSchedulerControlEvent;
      ARecurrence: Boolean = False; AReadOnly: Boolean = False): TcxSchedulerEventEditInfo;
    function GetEventHintText(AEvent: TcxSchedulerControlEvent): string; virtual;
    function GetEventModernStyleHintInfo(AEvent: TcxSchedulerControlEvent): TcxSchedulerEventModernStyleHintInfo; virtual;
    function GetEventUserHintText(AEvent: TcxSchedulerControlEvent; var AText: string): Boolean; virtual;
    function GetEventUserModernStyleHintInfo(AEvent: TcxSchedulerControlEvent; AInfo: TcxSchedulerEventModernStyleHintInfo): Boolean; virtual;
    function GetHScrollBarBounds: TRect; override;
    function GetInternalCanvas: TcxCanvas; virtual;
    function GetIsLocked: Boolean; virtual;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;
    function GetNextView(AView: TcxSchedulerCustomView): TcxSchedulerCustomView; virtual;
    function GetOnShowDateHint: TcxSchedulerShowDateHintEvent; virtual;
    function GetPainterHelper: TcxSchedulerPainterHelperClass; virtual;
    function GetSizeGripBounds: TRect; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;
    function GetTimeBias(AStart: TDateTime): Double; virtual;
    function GetVScrollBarBounds: TRect; override;
    function HasConflict(AStartDrag: Boolean): Boolean;
    function HasResources: Boolean;
    procedure InitControl; override;
    procedure InitScrollBarsParameters; override;
    procedure InternalDeleteEvent(AEvent: TcxSchedulerControlEvent; AIgnoreRecurring: Boolean);
    procedure InternalDeleteSelectedEvents(AForceDelete, ACheckReadOnly: Boolean);
    function IsDaysIntervalChanged(var AStart, AFinish: TDateTime): Boolean;
    function IsViewAtLeft: Boolean;
    function IsViewAtTop: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure LockUpdateChanged(ALocking: Boolean); virtual;
    procedure Modified; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    function NeedShowHint(AEvent: TcxSchedulerControlEvent; var AHintText: string;
      AllowShow: Boolean): Boolean; virtual;
    procedure DoPaint; override;
    procedure PaintControl(AControl: TcxSchedulerSubControl; ASpecialPaint: Boolean = False); virtual;
    procedure PeriodChanged; virtual;
    procedure RemoveSubControl(AControl: TcxSchedulerSubControl);
    procedure ReplaceSelParams(ASelStart, ASelFinish: TDateTime; AResource: TcxSchedulerStorageResourceItem); virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SelectedDaysChanged(AView: TcxSchedulerCustomView);
    procedure SetCurrentView(AView: TcxSchedulerCustomView); virtual;
    procedure SubControlAdd(AControl: TcxSchedulerSubControl);
    procedure SubControlRemove(AControl: TcxSchedulerSubControl);
    procedure SynchronizeRangeControl; virtual;
    procedure SynchronizeVisibleDays; virtual;
    procedure UpdateDateNavigatorDragging(Accept: Boolean);
    procedure UpdateHolidayDays(ABegin, AEnd: TDate);
    procedure UpdateEventsCache(ACheckDaysInterval: Boolean); virtual;
    procedure ValidateSelection(ASelection: TcxSchedulerDateList); virtual;
    procedure ValidateState;
    procedure ViewVisibleChanged(AView: TcxSchedulerCustomView); virtual;
    function VisibleGroupingKind: TcxSchedulerGroupingKind;
    // drag'n'drop support
    function CanDrag(X, Y: Integer): Boolean; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetDragObjectClass: TDragControlObjectClass; override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetScrollContentForegroundColor: TColor; override;
    property Background: TcxSchedulerSubControl read FBackground;
    property AligningSubControls: Boolean read FAligningSubControls;
    property BoundsChanging: Boolean read FBoundsChanging;
    property Capture: TcxSchedulerSubControl read GetCaptureControl write SetCaptureControl;
    property CaptureController: TcxSchedulerSubControlController read GetCaptureController;
    property ClipboardController: TcxSchedulerClipboardController read FClipboardController;
    property ControlFlags: TcxControlFlags read FControlFlags write FControlFlags;
    property DefaultProperties: TcxCustomEditProperties read FDefaultProperties;
    property DialogsStyle: string read FDialogsStyle write SetDialogsStyle;
    property EditController: TcxSchedulerEditController read FEditController;
    property EditStyle: TcxCustomEditStyle read FEditStyle; //remove to EditingController
    property EventHitTestController: TcxSchedulerEventHitTestController read FEventHitTestController write FEventHitTestController;
    property EventList: TcxSchedulerCachedEventList read FEventList;
    property HintController: TcxSchedulerHintController read FHintController;
    property HorzSplitter: TcxSchedulerSplitter read FHorzSplitter;
    property IsDragCanceled: Boolean read FIsDragCanceled;
    property IsDynamicUpdate: Boolean read GetIsDynamicUpdate;
    property IsLocked: Boolean read GetIsLocked;
    property KeyboardListener: TcxSchedulerSubControl read FKeyboardListener write FKeyboardListener;
    property LockCount: Integer read FLockCount;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelper;
    property ParentFont default False;
    property StartOfWeek: TDay read GetStartOfWeek;
    property StorageActive: Boolean read GetStorageActive;
    property StorageValid: Boolean read GetStorageValid;
    property SubControlCount: Integer read GetSubControlCount;
    property SubControlsCreated: Boolean read FSubControlsCreated;
    property SubControls[Index: Integer]: TcxSchedulerSubControl read GetSubControl;
    property TabOrdersList: TcxSchedulerEventList read FTabOrdersList;
    property VertSplitter: TcxSchedulerSplitter read FVertSplitter;
    property DragMode default dmAutomatic;
    property BorderStyle default cxcbsDefault;

    property OnAfterDragEvent: TcxSchedulerAfterDragEvent read FOnAfterDragEvent write FOnAfterDragEvent;
    property OnAfterEditing: TcxSchedulerAfterEditing read FOnAfterEditing write FOnAfterEditing;
    property OnAfterSizingEvent: TcxSchedulerAfterSizingEvent read FOnAfterSizingEvent write FOnAfterSizingEvent;
    property OnBeforeDeleting: TcxSchedulerBeforeDeleting read FOnBeforeDeleting write FOnBeforeDeleting;
    property OnBeforeDragEvent: TcxSchedulerBeforeDragEvent read FOnBeforeDragEvent write FOnBeforeDragEvent;
    property OnBeforeEditing: TcxSchedulerBeforeEditing read FOnBeforeEditing write FOnBeforeEditing;
    property OnBeforeSizingEvent: TcxSchedulerBeforeSizingEvent read FOnBeforeSizingEvent write FOnBeforeSizingEvent;
    property OnCanShowView: TcxSchedulerCanShowViewEvent read FOnCanShowView write FOnCanShowView;
    property OnEventSelectionChanged: TcxSchedulerEventSelectionChangedEvent read FOnEventSelectionChanged write FOnEventSelectionChanged;
    property OnFirstVisibleResourceChanged: TNotifyEvent read FOnFirstVisibleResourceChanged write FOnFirstVisibleResourceChanged;
    property OnGetEventDisplayText: TcxSchedulerGetEventText read FOnGetEventDisplayText write FOnGetEventDisplayText;
    property OnGetEventHintText: TcxSchedulerGetEventText read FOnGetEventHintText write FOnGetEventHintText;
    property OnGetEventEditProperties: TcxSchedulerGetEventEditPropertiesEvent read FOnGetEventEditProperties write FOnGetEventEditProperties;
    property OnGetEventModernStyleHintInfo: TcxSchedulerGetEventModernStyleHintInfo read FOnGetEventModernStyleHintInfo write FOnGetEventModernStyleHintInfo;
    property OnInitEdit: TcxSchedulerInitEditEvent read FOnInitEdit write FOnInitEdit;
    property OnIsWorkTime: TcxSchedulerIsWorkTimeEvent read FOnIsWorkTime write FOnIsWorkTime;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnMoreEventsButtonClick: TcxSchedulerMoreEventsButtonClickEvent read FOnMoreEventsButtonClick write FOnMoreEventsButtonClick;
    property OnNavigationButtonClick: TcxSchedulerNavigationButtonClickEvent read FOnNavigationButtonClick write FOnNavigationButtonClick;
    property OnScaleScroll: TcxSchedulerScaleScrollEvent read FOnScaleScroll write FOnScaleScroll;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnShowDateHint: TcxSchedulerShowDateHintEvent read GetOnShowDateHint write FOnShowDateHint;
    property OnViewTypeChanged: TcxSchedulerViewTypeChangedEvent read FOnViewTypeChanged write FOnViewTypeChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CopyToClipboard;
    procedure CreateEventUsingDialog(AllDay: Boolean = False; ARecurrence: Boolean = False); virtual;
    procedure CreateEventUsingInplaceEdit;
    procedure CutToClipboard;
    procedure DeleteEvent(AEvent: TcxSchedulerControlEvent); virtual;
    procedure DeleteSelectedEvents(ACheckReadOnly: Boolean = True);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EditEventUsingDialog(AEvent: TcxSchedulerControlEvent;
      ACheckReadOnly: Boolean = True; AForcePatternEditing: Boolean = False);
    procedure EditEventUsingInplaceEdit(AEvent: TcxSchedulerControlEvent);
    procedure EndUpdate;
    procedure FullRefresh; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GoToDate(ADate: TDateTime): Boolean; overload; virtual;
    function GoToDate(ADate: TDateTime; AViewMode: TcxSchedulerViewMode): Boolean; overload; virtual;
    procedure LayoutChanged;
    procedure MakeEventVisible(AEvent: TcxSchedulerControlEvent;
      const ADate: TDateTime = NullDate; AResource: TcxSchedulerStorageResourceItem = nil);
    procedure MakeResourceVisible(AResource: TcxSchedulerStorageResourceItem);
    procedure PasteFromClipboard;
    procedure SelectEvent(AEvent: TcxSchedulerControlEvent; AShift: TShiftState = []);
    procedure SelectTime(const ASelStart, ASelFinish: TDateTime;
      AResource: TcxSchedulerStorageResourceItem);
    procedure UnselectEvents;
    procedure ValidateFirstVisibleResourceIndex;
    // store/load
    procedure RestoreFromIniFile(const AStorageName: string; ARestoreResources: Boolean = True);
    procedure RestoreFromRegistry(const AStorageName: string; ARestoreResources: Boolean = True);
    procedure RestoreFromStream(AStream: TStream; ARestoreResources: Boolean = True);
    procedure StoreToIniFile(const AStorageName: string; AReCreate: Boolean = True; AStoreResources: Boolean = True);
    procedure StoreToRegistry(const AStorageName: string; AReCreate: Boolean = True; AStoreResources: Boolean = True);
    procedure StoreToStream(AStream: TStream; AStoreResources: Boolean = True);
    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property ActiveHitTest: TcxSchedulerSubControlHitTest read GetActiveHitTest;
    property Color;
    property ContentPopupMenu: TcxSchedulerContentPopupMenu read FContentPopupMenu write SetContentPopupMenu;
    property ControlBox: TcxSchedulerControlBox read FControlBox write SetControlBox;
    property CurrentView: TcxSchedulerCustomView read FCurrentView write SetCurrentView;
    property DateNavigator: TcxSchedulerCustomDateNavigator read FDateNavigator;
    property DateTimeHelper: TcxSchedulerDateTimeHelperClass read GetDateTimeHelper;
    property DialogsLookAndFeel: TcxLookAndFeel read FDialogsLookAndFeel write SetDialogsLookAndFeel;
    property EventDays: TcxSchedulerDateList read FEventDays;
    property EventImages: TCustomImageList read FEventImages write SetEventImages;
    property EventOperations: TcxSchedulerEventOperations read FEventOperations write SetEventOperations;
    property EventPopupMenu: TcxSchedulerEventPopupMenu read FEventPopupMenu write SetEventPopupMenu;
    property FirstVisibleResourceIndex: Integer read FFirstVisibleResourceIndex write SetFirstVisibleResourceIndex default 0;
    property HolidayDays: TcxSchedulerDateList read FHolidayDays;
    property OptionsBehavior: TcxSchedulerOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxSchedulerOptionsCustomize read FOptionsCustomize write SetOptionsCustomize;
    property OptionsView: TcxSchedulerOptionsView read FOptionsView write SetOptionsView;
    property ResourceNavigator: TcxSchedulerResourceNavigator read FResourceNavigator write SetResourceNavigator;
    property SelectedDays: TcxSchedulerDateList read FSelectedDays;
    property SelectedEventCount: Integer read GetSelectedEventCount;
    property SelectedEvents[Index: Integer]: TcxSchedulerControlEvent read GetSelectedEvent;
    property SelFinish: TDateTime read GetSelFinish;
    property SelResource: TcxSchedulerStorageResourceItem read FSelResource;
    property SelStart: TDateTime read GetSelStart;
    property Storage: TcxCustomSchedulerStorage read FStorage write SetStorage;
    property StoringName: string read FStoringName write FStoringName;
    property Styles: TcxSchedulerStyles read FStyles write SetStyles;
    property VisibleEventCount: Integer read GetVisibleEventCount;
    property VisibleEvents[AIndex: Integer]: TcxSchedulerControlEvent read GetVisibleEvent;
    property TabStop default True;
    property Font;
  published
    property StylesEvents: TNotifyEvent read FStylesEvents write FStylesEvents;
    property ResourceNavigatorEvents: TNotifyEvent read FResourceNavigatorEvents write FResourceNavigatorEvents;
    property ContentPopupMenuEvents: TNotifyEvent read FContentPopupMenuEvents write FContentPopupMenuEvents;
    property EventPopupMenuEvents: TNotifyEvent read FEventPopupMenuEvents write FEventPopupMenuEvents;
  end;

  { TcxSchedulerPopupMenu }

  TcxSchedulerPopupMenu = class(TPersistent)
  strict private
    FScheduler: TcxCustomScheduler;
    FPopupMenu: TComponent;
    FInternalMenu: TPopupMenu;
    FUseBuiltInPopupMenu: Boolean;

    procedure AdaptedMenuItemClickHandler(Sender: TObject);
    function GetRoot: TMenuItem;
    procedure SetPopupMenu(const Value: TComponent);
  protected
    function AddValidSeparator(AOwner: TMenuItem): TMenuItem;
    procedure CreateInternalMenu;
    procedure CreateItems; virtual;
    function CreateSeparator(AOwner: TMenuItem): TMenuItem;
    function CreateSubItem(AOwner: TMenuItem; const ACaption: string; ACommand: Integer = -1;
      AImageIndex: Integer = -1; AEnabled: Boolean = True; AChecked: Boolean = False): TMenuItem;
    procedure DoExecute(ACommand: Integer); virtual;
    function DoOnClick(ACommand: Integer): Boolean; virtual;
    function DoOnPopup: Boolean; virtual;
    procedure ExecuteCommand(ACommand: Integer);
    function FindItemByCommand(AOwnerItem: TMenuItem; ACommand: Integer): TMenuItem;
    function IsValidCommand(ACommand: Integer): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure OnItemClickHandler(Sender: TObject);
    function Storage: TcxCustomSchedulerStorage;

    property InternalMenu: TPopupMenu read FInternalMenu;
    property Root: TMenuItem read GetRoot;
  public
    constructor Create(AScheduler: TcxCustomScheduler); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Popup(X, Y: Integer): Boolean; virtual;

    property Scheduler: TcxCustomScheduler read FScheduler;
  published
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
    property UseBuiltInPopupMenu: Boolean read FUseBuiltInPopupMenu write FUseBuiltInPopupMenu default True;
  end;

  { TcxSchedulerCustomContentPopupMenu }

  TcxSchedulerCustomContentPopupMenu = class(TcxSchedulerPopupMenu)
  private
    FNewID: Integer;
    FAllDayID: Integer;
    FRecurrenceID: Integer;
  protected
    function CanCreateEvent: Boolean;
    procedure CreateNewEventItems(ANew, AllDay, AReccurence: Boolean;
      ANewID, AllDayID, ARecurrenceID: Integer);
    procedure DoExecute(ACommand: Integer); override;
  public
    constructor Create(AScheduler: TcxCustomScheduler); override;
  end;

  { TcxSchedulerContentPopupMenu }

  TcxSchedulerContentPopupMenuPopupEvent = procedure (Sender: TcxSchedulerContentPopupMenu;
    ABuiltInMenu: TPopupMenu; var AHandled: Boolean) of object;
  TcxSchedulerContentPopupMenuClickEvent = procedure (Sender: TcxSchedulerContentPopupMenu;
    AItem: TcxSchedulerContentPopupMenuItem; var AHandled: Boolean) of object;

  TcxSchedulerContentPopupMenu = class(TcxSchedulerCustomContentPopupMenu)
  private
    FActualItems: TcxSchedulerContentPopupMenuItems;
    FItems: TcxSchedulerContentPopupMenuItems;
    FSavedDate: TDateTime;
    FOnPopup: TcxSchedulerContentPopupMenuPopupEvent;
    FOnClick: TcxSchedulerContentPopupMenuClickEvent;
    procedure CreateGoToThisDayItem;
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function DoOnClick(ACommand: Integer): Boolean; override;
    function DoOnPopup: Boolean; override;
    function IsValidCommand(ACommand: Integer): Boolean; override;

    property ActualItems: TcxSchedulerContentPopupMenuItems read FActualItems;
  public
    constructor Create(AScheduler: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(AItem: TcxSchedulerContentPopupMenuItem);
    function GetMenuItem(AItem: TcxSchedulerContentPopupMenuItem): TMenuItem;
  published
    property Items: TcxSchedulerContentPopupMenuItems
      read FItems write FItems default [cpmiNewEvent, cpmiNewAllDayEvent,
        cpmiNewReccuringEvent, cpmiToday, cpmiGoToDate, cpmiGoToThisDay,
        cpmiResourcesLayout];
    property PopupMenu;
    property UseBuiltInPopupMenu;

    property OnClick: TcxSchedulerContentPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxSchedulerContentPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

  { TcxSchedulerEventPopupMenu }

  TcxSchedulerEventPopupMenuItem = (epmiOpen, epmiEditSeries,
    epmiShowTimeAs, epmiLabel, epmiDelete);
  TcxSchedulerEventPopupMenuItems = set of TcxSchedulerEventPopupMenuItem;

  TcxSchedulerEventPopupMenuPopupEvent = procedure (Sender: TcxSchedulerEventPopupMenu;
    ABuiltInMenu: TPopupMenu; var AHandled: Boolean) of object;
  TcxSchedulerEventPopupMenuClickEvent = procedure (Sender: TcxSchedulerEventPopupMenu;
    AItem: TcxSchedulerEventPopupMenuItem; ASubItemIndex: Integer;
    var AHandled: Boolean) of object;

  TcxSchedulerEventPopupMenu = class(TcxSchedulerPopupMenu)
  private
    FEvent: TcxSchedulerControlEvent;
    FItems: TcxSchedulerEventPopupMenuItems;
    FOnPopup: TcxSchedulerEventPopupMenuPopupEvent;
    FOnClick: TcxSchedulerEventPopupMenuClickEvent;
    procedure CreateDeleteItem;
    procedure CreateLabelItems;
    procedure CreateTimeItems;
    function GetCommand(AItem: TcxSchedulerEventPopupMenuItem;
      ASubItemIndex: Integer): Integer;
    function CanEdit: Boolean;
    function GetEvent: TcxSchedulerControlEvent;
    procedure UnpackCommand(ACommand: Integer;
      out AItem: TcxSchedulerEventPopupMenuItem; out ASubItemIndex: Integer);
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function DoOnClick(ACommand: Integer): Boolean; override;
    function DoOnPopup: Boolean; override;
    function IsValidCommand(ACommand: Integer): Boolean; override;
    procedure SetEventLabelColor(AColor: Integer);
    procedure SetEventState(AState: Integer);
  public
    constructor Create(AScheduler: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;
    function GetMenuItem(AItem: TcxSchedulerEventPopupMenuItem;
      ASubItemIndex: Integer = -1): TMenuItem;
    property Event: TcxSchedulerControlEvent read FEvent;
  published
    property Items: TcxSchedulerEventPopupMenuItems
      read FItems write FItems default [epmiOpen, epmiEditSeries,
        epmiShowTimeAs, epmiLabel, epmiDelete];
    property PopupMenu;
    property UseBuiltInPopupMenu;

    property OnClick: TcxSchedulerEventPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxSchedulerEventPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

function IsHeaderEvent(AEvent: TcxSchedulerEvent): Boolean;

var
  CF_SCHEDULERDATA: Integer;

implementation

uses
  DateUtils, Variants, RTLConsts, cxSchedulerEditorFormManager,
  cxSchedulerDialogs, cxMemo, cxSchedulerStrs,
  cxSchedulerRecurrenceSelectionDialog, cxLibraryConsts, cxSchedulerRangeControlClientProperties,
  cxSchedulerAgendaView, cxSchedulerEventModernInfoContainer, cxSchedulerCustomResourceView, dxDPIAwareUtils;

type
  TcxSchedulerControlEventAccess = class(TcxSchedulerControlEvent);
  TcxSchedulerEventCellViewInfoAccess = class(TcxSchedulerEventCellViewInfo);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);
  TcxCustomSchedulerStorageAccess = class(TcxCustomSchedulerStorage);

{ TcxSchedulerModernStyleHintForm }

  TcxSchedulerModernStyleHintForm = class(TdxCustomCalloutPopupWindow);


function cxCompareTabOrders(AEvent1, AEvent2: TcxSchedulerControlEvent): Integer;
var
  AAllDay1, AAllDay2: Boolean;
begin
  Result := 0;
  AAllDay1 := AEvent1.IsAllDayOrLonger;
  AAllDay2 := AEvent2.IsAllDayOrLonger;
  if dxDateOf(AEvent1.Start) = dxDateOf(AEvent2.Start) then
    Result := Byte(AAllDay2) - Byte(AAllDay1);
  if Result <> 0 then Exit;
  if AEvent1.Start < AEvent2.Start then
    Result := -1
  else
    if AEvent1.Start > AEvent2.Start then
      Result := 1
    else
      if AEvent1.Finish > AEvent2.Finish then
        Result := -1
      else
        if AEvent1.Finish < AEvent2.Finish then
          Result := 1
        else
          Result := AEvent1.Index - AEvent2.Index;
end;

function IsHeaderEvent(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent.IsAllDayOrLonger;
end;

{ TcxSchedulerSubControl }

constructor TcxSchedulerSubControl.Create(
  AOwner: TcxCustomScheduler);
begin
  FScheduler := AOwner;
  FVisible := True;
  CreateSubClasses;
  FCursor := crDefault;
  Scheduler.SubControlAdd(Self);
end;

destructor TcxSchedulerSubControl.Destroy;
begin
  DestroySubClasses;
  Scheduler.SubControlRemove(Self);
  inherited Destroy;
end;

procedure TcxSchedulerSubControl.Invalidate;
begin
  InvalidateRect(ClientRect);
end;

procedure TcxSchedulerSubControl.InvalidateRect(
  const ARect: TRect);
begin
  Scheduler.InvalidateRect(cxRectOffset(ARect, Left, Top), False);
end;

procedure TcxSchedulerSubControl.LayoutChanged;
begin
  if not Scheduler.HandleAllocated then Exit;
  if not Visible then
    ViewInfo.Clear
  else
    Painter.InitializePainter;
  if not Visible or (Controller = nil) then Exit;
  Controller.Reset;
  DoLayoutChanged;
  if not Scheduler.AligningSubControls then
  begin
    HitTest.Recalculate;
    Invalidate;
  end;
  CheckBiDiMode;
end;

procedure TcxSchedulerSubControl.Refresh;
begin
  LayoutChanged;
end;

procedure TcxSchedulerSubControl.Repaint;
begin
  Invalidate;
end;

procedure TcxSchedulerSubControl.RepaintRect(const ARect: TRect);
begin
  InvalidateRect(ARect);
end;

function TcxSchedulerSubControl.ScreenToClient(const APos: TPoint): TPoint;
begin
  Result := Scheduler.ScreenToClient(APos);
  Dec(Result.X, Left);
  Dec(Result.Y, Top);
end;

procedure TcxSchedulerSubControl.SetBounds(
  ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (ALeft <> FLeft) or (ATop <> FTop) or (AWidth <> FWidth) or
    (AHeight <> FHeight) then
  begin
    BoundsChanging;
    FLeft := ALeft;
    FTop := ATop;
    FWidth := AWidth;
    FHeight := AHeight;
    BoundsChanged;
  end;
end;

procedure TcxSchedulerSubControl.GetProperties(AProperties: TStrings);
begin
end;

procedure TcxSchedulerSubControl.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
end;

procedure TcxSchedulerSubControl.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
end;

function TcxSchedulerSubControl.AllowDesignHitTest(
  X, Y: Integer; AShift: TShiftState): Boolean;
begin
  MousePositionChanged(X, Y);
  Result := False;
end;

procedure TcxSchedulerSubControl.BoundsChanged;
begin
  Scheduler.AlignSubControls(Self);
  Changed;
end;

procedure TcxSchedulerSubControl.CalculateViewInfo;
begin
  ViewInfo.Calculate;
end;

function TcxSchedulerSubControl.CanCapture(
  const APoint: TPoint): Boolean;
begin
  Result := Visible and cxRectPtIn(Bounds, APoint);
end;

procedure TcxSchedulerSubControl.Changed;
begin
  Scheduler.LayoutChanged;
end;

procedure TcxSchedulerSubControl.ClearCachedData;
begin
end;

function TcxSchedulerSubControl.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerSubControlController.Create(Self);
end;

function TcxSchedulerSubControl.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerSubControlHitTest.Create(Self);
end;

function TcxSchedulerSubControl.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerSubControlPainter.Create(Self);
end;

function TcxSchedulerSubControl.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerSubControlViewInfo.Create(Self);
end;

procedure TcxSchedulerSubControl.BoundsChanging;
begin
end;

procedure TcxSchedulerSubControl.CheckBiDiMode;
begin
//
end;

procedure TcxSchedulerSubControl.CreateSubClasses;
begin
  FViewInfo := CreateViewInfo;
  FHitTest := CreateHitTest;
  FController := CreateController;
  FPainter := CreatePainter;
end;

procedure TcxSchedulerSubControl.DestroySubClasses;
begin
  FPainter.Free;
  FHitTest.Free;
  FController.Free;
  FViewInfo.Free;
end;

procedure TcxSchedulerSubControl.DoBeforeMouseDown(AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer);
begin
  MousePositionChanged(X, Y);
  Controller.BeforeMouseDown(AButton, AShift, X, Y);
end;

procedure TcxSchedulerSubControl.DoCancelMode;
begin
  Mouse.Capture := 0;
end;

procedure TcxSchedulerSubControl.DoLayoutChanged;
begin
  if (cxRectWidth(Bounds) > 0) and (cxRectHeight(Bounds) > 0) then
    CalculateViewInfo;
end;

procedure TcxSchedulerSubControl.DoMouseDown(
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  MousePositionChanged(X, Y);
  Controller.MouseDown(AButton, AShift, X, Y);
end;

procedure TcxSchedulerSubControl.DoMouseMove(
  AShift: TShiftState; X, Y: Integer);
begin
  MousePositionChanged(X, Y);
  Controller.MouseMove(AShift, X, Y);
end;

procedure TcxSchedulerSubControl.DoMouseUp(
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  MousePositionChanged(X, Y);
  Controller.MouseUp(AButton, AShift, X, Y);
end;

procedure TcxSchedulerSubControl.DoPaint;
begin
  Painter.BeforePaint;
  Painter.Paint;
  Painter.AfterPaint;
end;

procedure TcxSchedulerSubControl.DoScaleScroll;
begin
  Scheduler.DoScaleScroll;
end;

procedure TcxSchedulerSubControl.FormatChanged;
begin
end;

function TcxSchedulerSubControl.GetClientRect: TRect;
begin
  Result := cxRectBounds(0, 0, FWidth, FHeight);
end;

function TcxSchedulerSubControl.GetOwner: TPersistent;
begin
  Result := FScheduler;
end;

function TcxSchedulerSubControl.GetHScrollBarBounds: TRect;
begin
  Result := cxNullRect;
end;

function TcxSchedulerSubControl.GetScaleFactor: TdxScaleFactor;
begin
  Result := Scheduler.ScaleFactor;
end;

function TcxSchedulerSubControl.GetScrollBar(AKind: TScrollBarKind): IcxControlScrollBar;
begin
  if AKind = sbHorizontal then
    Result := Scheduler.HScrollBar
  else
    Result := Scheduler.VScrollBar;
end;

function TcxSchedulerSubControl.GetSizeGripBounds: TRect;
begin
  Result := cxNullRect;
end;

function TcxSchedulerSubControl.GetStartOfWeek: TDay;
begin
  Result := Scheduler.OptionsView.ActualStartOfWeek;
end;

function TcxSchedulerSubControl.GetVScrollBarBounds: TRect;
begin
  Result := cxNullRect;
end;

procedure TcxSchedulerSubControl.InitScrollBarsParameters;
begin
end;

function TcxSchedulerSubControl.IsSpecialPaint: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerSubControl.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
end;

procedure TcxSchedulerSubControl.MousePositionChanged(
  var X, Y: Integer);
begin
  Dec(X, FLeft);
  Dec(Y, FTop);
  HitTest.HitPoint := cxPoint(X, Y);
end;

procedure TcxSchedulerSubControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
end;

procedure TcxSchedulerSubControl.Paint;
var
  ASavePos: TPoint;
begin
  if not Visible or cxRectIsEmpty(Bounds) or not Canvas.RectVisible(Bounds) then Exit;
  ASavePos := Canvas.WindowOrg;
  try
    Canvas.WindowOrg := cxPoint(-Left, -Top);
    if not IsSpecialPaint then
      Canvas.IntersectClipRect(ClientRect);
    DoPaint;
  finally
    Canvas.WindowOrg := ASavePos;
  end;
end;

procedure TcxSchedulerSubControl.PeriodChanged;
begin
  Scheduler.PeriodChanged;
end;

procedure TcxSchedulerSubControl.SetScrollBarInfo(
  AScrollBarKind: TScrollBarKind; AMin, AMax, AStep, APage, APos: Integer;
  AAllowShow, AAllowHide: Boolean);
begin
  Scheduler.SetScrollBarInfo(AScrollBarKind, AMin, AMax,
    AStep, APage, APos, AAllowShow and Visible, AAllowHide);
end;

function TcxSchedulerSubControl.UseRightToLeftAlignment: Boolean;
begin
  Result := Scheduler.UseRightToLeftAlignment;
end;

procedure TcxSchedulerSubControl.VisibleChanged;
begin
  Scheduler.AlignSubControls(Self);
  Changed;
end;

function TcxSchedulerSubControl.GetBottom: Integer;
begin
  Result := FTop + FHeight;
end;

function TcxSchedulerSubControl.GetBounds: TRect;
begin
  Result := cxRectBounds(FLeft, FTop, FWidth, FHeight);
end;

function TcxSchedulerSubControl.GetCanvas: TcxCanvas;
begin
  Result := Scheduler.Canvas;
end;

function TcxSchedulerSubControl.GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
begin
  Result := Scheduler.DateTimeHelper;
end;

function TcxSchedulerSubControl.GetIsGestureScrolling: Boolean;
begin
  Result := Scheduler.IsGestureScrolling;
end;

function TcxSchedulerSubControl.GetIsScrollingContent: Boolean;
begin
  Result := Scheduler.IsScrollingContent;
end;

function TcxSchedulerSubControl.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Scheduler.LookAndFeelPainter;
end;

function TcxSchedulerSubControl.GetPainterHelperClass: TcxSchedulerPainterHelperClass;
begin
  Result := Scheduler.PainterHelper;
end;

function TcxSchedulerSubControl.GetRight: Integer;
begin
  Result := FLeft + FWidth;
end;

function TcxSchedulerSubControl.GetStyles: TcxSchedulerStyles;
begin
  Result := Scheduler.Styles;
end;

function TcxSchedulerSubControl.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := Scheduler.OptionsView.Style;
end;

procedure TcxSchedulerSubControl.InternalSetBounds(const AValue: TRect);
begin
  SetBounds(AValue.Left, AValue.Top, AValue.Right - AValue.Left,
    AValue.Bottom - AValue.Top);
end;

procedure TcxSchedulerSubControl.SetBottom(const Value: Integer);
begin
  InternalSetBounds(cxRectSetBottom(Bounds, Value));
end;

procedure TcxSchedulerSubControl.SetHeight(AValue: Integer);
begin
  SetBounds(FLeft, FTop, FWidth, AValue);
end;

procedure TcxSchedulerSubControl.SetLeft(AValue: Integer);
begin
  SetBounds(AValue, FTop, FWidth, FHeight);
end;

procedure TcxSchedulerSubControl.SetRight(Value: Integer);
begin
  InternalSetBounds(cxRectSetRight(Bounds, Value));
end;

procedure TcxSchedulerSubControl.SetTop(AValue: Integer);
begin
  SetBounds(FLeft, AValue, FWidth, FHeight);
end;

procedure TcxSchedulerSubControl.SetVisible(
  AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    VisibleChanged;
  end;
end;

procedure TcxSchedulerSubControl.SetWidth(AValue: Integer);
begin
  SetBounds(FLeft, FTop, AValue, FHeight);
end;

{ TcxSchedulerSubControlController }

constructor TcxSchedulerSubControlController.Create(
  AOwner: TcxSchedulerSubControl);
begin
  FOwner := AOwner;
end;

procedure TcxSchedulerSubControlController.BeginDragAndDrop;
begin
end;

function TcxSchedulerSubControlController.CanDrag(
  X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerSubControlController.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
begin
end;

procedure TcxSchedulerSubControlController.DragDrop(
  Source: TObject; X, Y: Integer);
begin
end;

procedure TcxSchedulerSubControlController.DragOver(
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TcxSchedulerSubControlController.EndDrag(
  Target: TObject; X, Y: Integer);
begin
end;

procedure TcxSchedulerSubControlController.EndDragAndDrop(
  Accepted: Boolean);
begin
end;

function TcxSchedulerSubControlController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := nil;
end;

procedure TcxSchedulerSubControlController.StartDrag(
  var DragObject: TDragObject);
begin
end;

function TcxSchedulerSubControlController.StartDragAndDrop(
  const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerSubControlController.BeforeMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCanProcessMouseMove := Button = mbLeft;
end;

procedure TcxSchedulerSubControlController.DoCancelMode;
begin
  FCanProcessMouseMove := False;
end;

function TcxSchedulerSubControlController.GetCursor(
  X, Y: Integer): TCursor;
begin
  Result := Owner.Cursor;
end;

procedure TcxSchedulerSubControlController.KeyDown(
  var Key: Word; Shift: TShiftState);
begin
end;

procedure TcxSchedulerSubControlController.KeyPress(
  var Key: Char);
begin
end;

procedure TcxSchedulerSubControlController.KeyUp(
  var Key: Word; Shift: TShiftState);
begin
end;

procedure TcxSchedulerSubControlController.MouseEnter;
begin
end;

procedure TcxSchedulerSubControlController.MouseLeave;
begin
end;

procedure TcxSchedulerSubControlController.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TcxSchedulerSubControlController.MouseMove(
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TcxSchedulerSubControlController.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FCanProcessMouseMove := False;
end;

procedure TcxSchedulerSubControlController.Reset;
begin
end;

function TcxSchedulerSubControlController.GetHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := Owner.HitTest;
end;

function TcxSchedulerSubControlController.GetStartOfWeek: TDay;
begin
  Result := Owner.StartOfWeek;
end;

function TcxSchedulerSubControlController.UseRightToLeftAlignment: Boolean;
begin
  Result := Owner.UseRightToLeftAlignment;
end;

{ TcxSchedulerSubControlHitTest }

constructor TcxSchedulerSubControlHitTest.Create(
  AOwner: TcxSchedulerSubControl);
begin
  FOwner := AOwner;
end;

destructor TcxSchedulerSubControlHitTest.Destroy;
begin
  inherited Destroy;
end;

procedure TcxSchedulerSubControlHitTest.Recalculate;
begin
  if Scheduler.HandleAllocated then
    DoCalculate;
end;

procedure TcxSchedulerSubControlHitTest.Clear;
begin
  Flags := 0;
  FTime := NullDate;
end;

procedure TcxSchedulerSubControlHitTest.DoCalculate;
begin
  Clear;
  SetBitState(htcControl, cxRectPtIn(Owner.Bounds,
    cxPointOffset(HitPoint, Owner.Left, Owner.Top)));
end;

function TcxSchedulerSubControlHitTest.GetBitState(
  AIndex: Integer): Boolean;
begin
  Result := (Flags and (1 shl AIndex)) <> 0
end;

function TcxSchedulerSubControlHitTest.GetMaskState(
  AMask: Integer): Boolean;
begin
  Result := Flags and AMask = AMask;
end;

function TcxSchedulerSubControlHitTest.GetMaskStateEx(
  AMask: Integer): Boolean;
begin
  Result := Flags and AMask <> 0;
end;

procedure TcxSchedulerSubControlHitTest.SetBitState(
  AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    Flags := Flags or (1 shl AIndex)
  else
    Flags := Flags and not (1 shl AIndex);
end;

procedure TcxSchedulerSubControlHitTest.SetMaskState(
  AMask: Integer; AValue: Boolean);
begin
  if AValue then
    Flags := Flags or AMask
  else
    Flags := Flags and not AMask;
end;

function TcxSchedulerSubControlHitTest.GetPosValue(
  AIndex: Integer): Integer;
begin
  Result := cxPointGetItem(FHitPoint, AIndex);
end;

function TcxSchedulerSubControlHitTest.GetScheduler: TcxCustomScheduler;
begin
  Result := FOwner.FScheduler;
end;

procedure TcxSchedulerSubControlHitTest.SetHitPoint(
  const APoint: TPoint);
begin
  FHitPoint := APoint;
  Recalculate;
end;

procedure TcxSchedulerSubControlHitTest.SetPosValue(
  AIndex, AValue: Integer);
begin
  FHitPoint := cxPointReplaceItem(FHitPoint, AIndex, AValue);
  Recalculate;
end;

{ TcxSchedulerSubControlPainter }

constructor TcxSchedulerSubControlPainter.Create(
  AOwner: TcxSchedulerSubControl);
begin
  FOwner := AOwner;
end;

procedure TcxSchedulerSubControlPainter.AfterPaint;
begin
end;

procedure TcxSchedulerSubControlPainter.BeforePaint;
begin
end;

procedure TcxSchedulerSubControlPainter.InitializePainter;
begin
end;

procedure TcxSchedulerSubControlPainter.Paint;
begin
end;

function TcxSchedulerSubControlPainter.GetCanvas: TcxCanvas;
begin
  Result := Owner.Canvas;
end;

function TcxSchedulerSubControlPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TcxSchedulerSubControlPainter.GetViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := Owner.ViewInfo;
end;

{ TcxSchedulerSubControlViewInfo }

constructor TcxSchedulerSubControlViewInfo.Create(
  AOwner: TcxSchedulerSubControl);
begin
  FOwner := AOwner;
end;

procedure TcxSchedulerSubControlViewInfo.Calculate;
begin
  if not IsSchedulerCreated then Exit;
  FBounds := GetBounds;
  Clear;
  DoCalculate;
  AfterCalculate;
end;

procedure TcxSchedulerSubControlViewInfo.AfterCalculate;
begin
end;

procedure TcxSchedulerSubControlViewInfo.Clear;
begin
end;

procedure TcxSchedulerSubControlViewInfo.DoCalculate;
begin
end;

function TcxSchedulerSubControlViewInfo.GetBounds: TRect;
begin
  Result := Owner.ClientRect;
end;

function TcxSchedulerSubControlViewInfo.GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
begin
  Result := Owner.DateTimeHelper;
end;

function TcxSchedulerSubControlViewInfo.GetDefaultFont: TFont;
begin
  Result := Owner.Scheduler.Font;
end;

function TcxSchedulerSubControlViewInfo.GetIsSchedulerCreated: Boolean;
begin
  Result := Owner.Scheduler.FSubControlsCreated;
end;

function TcxSchedulerSubControlViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.Scheduler.LookAndFeelPainter;
end;

function TcxSchedulerSubControlViewInfo.GetPainterHelperClass: TcxSchedulerPainterHelperClass;
begin
  Result := Owner.PainterHelper;
end;

function TcxSchedulerSubControlViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TcxSchedulerSubControlViewInfo.GetStyles: TcxSchedulerStyles;
begin
  Result := Owner.Styles;
end;

function TcxSchedulerSubControlViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := Owner.ViewStyle;
end;

function TcxSchedulerSubControlViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := Owner.UseRightToLeftAlignment;
end;

{ TContainer }

type
  TContainer = class(TWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  TabStop := False;
end;

procedure TContainer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    ExStyle := WS_EX_CONTROLPARENT;
    WindowClass.Style := CS_DBLCLKS;
  end;
end;

{ TcxSchedulerSplitterController }

procedure TcxSchedulerSplitterController.DoCancelMode;
begin
  EraseInvertRect;
  Scheduler.FCaptureControl := nil;
  Scheduler.FKeyboardListener := FSaveKeyboardListener;
end;

procedure TcxSchedulerSplitterController.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if Scheduler.OptionsCustomize.ControlsSizing and Scheduler.IsDragCanceled then
  begin
    DoCancelMode;
    Mouse.Capture := 0;
  end;
end;

procedure TcxSchedulerSplitterController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Scheduler.OptionsCustomize.ControlsSizing and (Button = mbLeft) then
  begin
    FSaveKeyboardListener := Scheduler.KeyboardListener;
    Scheduler.KeyboardListener := Splitter;
    FHitPoint := GetOwnerMousePos(X, Y);
    FStartBounds := Splitter.Bounds;
    FSizingBoundsRect := GetSizingBoundsRect;
    FScreenCanvasClipRect := GetDrawClipRect;
    if not IsDynamicUpdate then
    begin
      Scheduler.Update; //force repaint before on-screen drawing
      DrawInvertRect(FStartBounds);
    end;
    FPrevRect := FStartBounds;
  end;
end;

procedure TcxSchedulerSplitterController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  if not Scheduler.OptionsCustomize.ControlsSizing or not (ssLeft in Shift) or
    (Scheduler.CaptureController <> Self) or (Scheduler.KeyboardListener <> Splitter) or
    Scheduler.IsLocked then Exit;
  R := GetSizingRect(GetOwnerMousePos(X, Y));
  if not cxRectIsEqual(R, FPrevRect) then
  begin
    if IsDynamicUpdate then
      UpdateSizing(R)
    else
      DrawInvertRect(R);
    FPrevRect := R;
  end;
end;

procedure TcxSchedulerSplitterController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Scheduler.OptionsCustomize.ControlsSizing and (Button = mbLeft) then
  begin
    Scheduler.KeyboardListener := FSaveKeyboardListener;
    EraseInvertRect;
    UpdateSizing(GetSizingRect(GetOwnerMousePos(X, Y)));
  end;
  Scheduler.LayoutChanged;
  Scheduler.Refresh;
end;

procedure TcxSchedulerSplitterController.DrawInvertRect(const R: TRect);
var
  Temp: TRect;
  AOffset: TPoint;
  ACanvas: TcxCanvas;
begin
  if not cxRectIsEmpty(FPrevInvertRect) and cxRectIntersect(Temp, R, FPrevInvertRect) then
  begin
    SubtractRect(FPrevInvertRect, FPrevInvertRect, Temp);
    SubtractRect(Temp, R, Temp);
  end
  else
    Temp := R;
  AOffset := ScreenOffset;
  ACanvas := TcxScreenCanvas.Create;
  InvertRect(ACanvas, cxRectOffset(FPrevInvertRect, AOffset));
  InvertRect(ACanvas, cxRectOffset(Temp, AOffset));
  ACanvas.Free;
  FPrevInvertRect := R;
end;

procedure TcxSchedulerSplitterController.EraseInvertRect;
begin
  if cxRectIsEmpty(FPrevInvertRect) then Exit;
  InvertRect(cxScreenCanvas, cxRectOffset(FPrevInvertRect, ScreenOffset));
  FPrevInvertRect := cxNullRect;
end;

function TcxSchedulerSplitterController.GetMonthSize: TSize;
begin
  Result := Scheduler.DateNavigator.GetMonthSize;
end;

function TcxSchedulerSplitterController.GetOwnerMousePos(X, Y: Integer): TPoint;
begin
  Result := cxPoint(X + Owner.Bounds.Left, Y + Owner.Bounds.Top);
end;

function TcxSchedulerSplitterController.GetSizingBoundsRect: TRect;
begin
  Result := Scheduler.ClientBounds;
  if Splitter.Kind = skHorizontal then
    SetVertBounds(Result)
  else
    SetHorzBounds(Result);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Scheduler.ClientBounds);
end;

function TcxSchedulerSplitterController.GetSizingIncrement: Integer;
begin
  if not IsIntegralSizing then
    Result := 1
  else
    if Splitter.Kind = skVertical then
      Result := GetMonthSize.cx
    else
      Result := GetMonthSize.cy;
end;

function TcxSchedulerSplitterController.GetSizingRect(const P: TPoint): TRect;
begin
  if Splitter.Kind = skHorizontal then
    Result := GetVertSizingRect(P)
  else
    Result := GetHorzSizingRect(P);
  with SizingBoundsRect do
  begin
    if Result.Left < Left then
      Result := cxRectSetLeft(Result, Left);
    if Result.Top < Top then
      Result := cxRectSetTop(Result, Top);
    if Result.Right > Right then
      Result := cxRectSetRight(Result, Right);
    if Result.Bottom > Bottom then
      Result := cxRectSetBottom(Result, Bottom);
  end;
end;

procedure TcxSchedulerSplitterController.InvertRect(ACanvas: TcxCanvas;
  R: TRect);
begin
  if cxRectIsEmpty(R) then Exit;
  if Splitter.Kind = skHorizontal then
  begin
    R.Left := Max(R.Left, ScreenCanvasClipRect.Left);
    R.Right := Min(R.Right, ScreenCanvasClipRect.Right);
  end
  else
  begin
    R.Top := Max(R.Top, ScreenCanvasClipRect.Top);
    R.Bottom := Min(R.Bottom, ScreenCanvasClipRect.Bottom);
  end;
  if not cxRectIsEmpty(R) then
    ACanvas.InvertRect(R);
end;

function TcxSchedulerSplitterController.IsIntegralSizing: Boolean;
begin
  Result := Scheduler.OptionsCustomize.IntegralSizing;
end;

function TcxSchedulerSplitterController.IsDynamicUpdate: Boolean;
begin
  Result := Scheduler.IsDynamicUpdate;
end;

procedure TcxSchedulerSplitterController.SetSizeDelta(ADelta: Integer);
begin
  if Splitter.Kind = skHorizontal then
    SetVertDelta(ADelta)
  else
    SetHorzDelta(ADelta);
end;

procedure TcxSchedulerSplitterController.Modified;
begin
  Scheduler.Modified;
end;

procedure TcxSchedulerSplitterController.UpdateSizing(const R: TRect);

  function GetDelta: Integer;
  begin
    if Splitter.Kind = skHorizontal then
      Result := Splitter.Top - R.Top
    else
    begin
      Result := Splitter.Left - R.Left;
      if UseRightToLeftAlignment then
        Result := -Result;
    end;
  end;

var
  ADelta: Integer;
begin
  ADelta := GetDelta;
  if ADelta <> 0 then
  begin
    SetSizeDelta(ADelta);
    if IsDynamicUpdate then
      Scheduler.Update;
    Scheduler.DateNavigator.SaveSize;
  end;
end;

function TcxSchedulerSplitterController.GetDrawClipRect: TRect;
var
  AParent: TControl;
  R: TRect;
begin
  with Scheduler do
  begin
    Result := cxRectOffset(ClientBounds, ClientToScreen(cxNullPoint));
    AParent := Parent;
  end;
  while AParent <> nil do
  begin
    R := cxRectOffset(AParent.ClientRect, AParent.ClientOrigin);
    if AParent.Parent <> nil then
      cxRectOffset(R, AParent.ClientToScreen(cxNullPoint));
    cxRectIntersect(Result, Result, R);
    AParent := AParent.Parent;
  end;
end;

function TcxSchedulerSplitterController.GetHorzSizingRect(
  const P: TPoint): TRect;
var
  AStep, ANewPos: Integer;
begin
  AStep := GetSizingIncrement;
  ANewPos := FStartBounds.Left - Round((FHitPoint.X - P.X) / AStep) * AStep;
  Result := cxRectSetLeft(FStartBounds, ANewPos);
end;

function TcxSchedulerSplitterController.GetScheduler: TcxCustomScheduler;
begin
  Result := Owner.Scheduler;
end;

function TcxSchedulerSplitterController.GetScreenOffset: TPoint;
begin
  Result := Scheduler.ClientToScreen(cxNullPoint);
end;

function TcxSchedulerSplitterController.GetSplitter: TcxSchedulerSplitter;
begin
  Result := TcxSchedulerSplitter(inherited Owner);
end;

function TcxSchedulerSplitterController.GetVertSizingRect(
  const P: TPoint): TRect;
var
  AStep, ANewPos: Integer;
begin
  AStep := GetSizingIncrement;
  ANewPos := FStartBounds.Top - Round((FHitPoint.Y - P.Y) / AStep) * AStep;
  Result := cxRectSetTop(FStartBounds, ANewPos);
end;

procedure TcxSchedulerSplitterController.SetHorzBounds(var R: TRect);
var
  W: Integer;
begin
  W := ((R.Right - R.Left - Splitter.Width) div GetMonthSize.cx) *
    GetMonthSize.cx + Splitter.Width;
  if Scheduler.IsViewAtLeft then
  begin
    if IsIntegralSizing then
      R.Left := R.Right - W;
    Dec(R.Right, GetMonthSize.cx);
  end
  else
  begin
    if IsIntegralSizing then
      R.Right := R.Left + W;
    Inc(R.Left, GetMonthSize.cx);
  end;
end;

procedure TcxSchedulerSplitterController.SetHorzDelta(ADelta: Integer);
begin
  with Scheduler do
  begin
    case OptionsView.ViewPosition of
      vpLeft:
        DateNavigator.Width := DateNavigator.Width + ADelta;
      else
        DateNavigator.Width := DateNavigator.Width - ADelta;
    end;
  end;
end;

procedure TcxSchedulerSplitterController.SetVertBounds(var R: TRect);
begin
  if IsIntegralSizing then
    R.Bottom := R.Top + ((R.Bottom - R.Top - Splitter.Height) div GetMonthSize.cy) *
     GetMonthSize.cy + Splitter.Height;
  Inc(R.Top, GetMonthSize.cy);
end;

procedure TcxSchedulerSplitterController.SetVertDelta(ADelta: Integer);
begin
  with Scheduler do
  begin
    case OptionsView.ViewPosition of
      vpLeft, vpRight, vpBottom:
        DateNavigator.Height := DateNavigator.Height - ADelta;
      vpTop:
        DateNavigator.Height := DateNavigator.Height + ADelta;
    end;
  end;
end;

{ TcxSchedulerSplitterHitTest }

function TcxSchedulerSplitterHitTest.GetSplitter: TcxSchedulerSplitter;
begin
  Result := TcxSchedulerSplitter(Owner);
end;

{ TcxSchedulerSplitter }

function TcxSchedulerSplitter.AllowDesignHitTest(
  X, Y: Integer; AShift: TShiftState): Boolean;
begin
  Result := True;
end;

function TcxSchedulerSplitter.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerSplitterController.Create(Self);
end;

function TcxSchedulerSplitter.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerSplitterHitTest.Create(Self);
end;

procedure TcxSchedulerSplitter.DoLayoutChanged;
begin
  FViewParams := Styles.GetSplitterParams(Kind);
  inherited DoLayoutChanged;
end;

procedure TcxSchedulerSplitter.DoPaint;
begin
  LookAndFeelPainter.DrawSchedulerSplitterBorder(Canvas, ClientRect,
    ViewParams, Kind = skHorizontal);
end;

function TcxSchedulerSplitter.GetHitTest: TcxSchedulerSplitterHitTest;
begin
  Result := TcxSchedulerSplitterHitTest(inherited HitTest);
end;

procedure TcxSchedulerSplitter.UpdateCursor;
const
  Cursors: array[TcxSchedulerSplitterKind] of TCursor = (
    crSchedulerVertSplit, crSchedulerHorzSplit);
begin
  if Scheduler.OptionsCustomize.ControlsSizing then
    Cursor := Cursors[Kind]
  else
    Cursor := crDefault;
end;

procedure TcxSchedulerSplitter.SetKind(AKind: TcxSchedulerSplitterKind);
begin
  FKind := AKind;
  UpdateCursor;
end;

{ TcxSchedulerNavigatorButton }

constructor TcxSchedulerNavigatorButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FCommand := -1;
  FImageIndex := -1;
  FVisible := True
end;

procedure TcxSchedulerNavigatorButton.Assign(Source: TPersistent);
begin
 if Source is TcxSchedulerNavigatorButton then
   with TcxSchedulerNavigatorButton(Source) do
   begin
     Self.Enabled := FEnabled;
     Self.Hint := FHint;
     Self.FCommand := FCommand;
     Self.FImageIndex := FImageIndex;
     Self.FVisible := FVisible;
   end
 else
   inherited Assign(Source);
end;

procedure TcxSchedulerNavigatorButton.Changed;
begin
  if (Scheduler <> nil) and (Scheduler.ResourceNavigator <> nil) and
    Scheduler.ResourceNavigator.NeedScrollBar then Scheduler.LayoutChanged;
end;

procedure TcxSchedulerNavigatorButton.Click;
begin
  Scheduler.ResourceNavigator.Click(Self);
end;

function TcxSchedulerNavigatorButton.GetDisplayName: string;
begin
  Result := GetHintText;
  if Result = '' then
    Result := ClassName;
end;

function TcxSchedulerNavigatorButton.GetHintText: string;
const
  InternalHint: array[0..7] of Pointer =
  (@scxFirstButtonHint, @scxPrevPageButtonHint, @scxPrevButtonHint,
   @scxNextButtonHint, @scxNextPageButtonHint, @scxLastButtonHint,
   @scxShowMoreResourcesButtonHint, @scxShowFewerResourcesButtonHint);
begin
  Result := Hint;
  if (Result = '') and IsStandard then
    Result := cxGetResourceString(InternalHint[Command]);
end;

procedure TcxSchedulerNavigatorButton.Draw(
  APainter: TcxCustomLookAndFeelPainter; ACanvas: TcxCanvas);
var
  AGlyphRect: TRect;
  AImages: TCustomImageList;
  AImageIndex: Integer;
begin
  APainter.DrawSchedulerScaledNavigatorButton(ACanvas, Bounds, State, Scheduler.ScaleFactor);
  AImages := GetActualImageList;
  AImageIndex := GetActualImageIndex;
  if (AImages <> nil) and (AImageIndex >=0) then
  begin
    AGlyphRect := cxRectCenter(Bounds, dxGetImageSize(AImages, Scheduler.ScaleFactor));
    if State = cxbsPressed then
      AGlyphRect := cxRectOffset(AGlyphRect, APainter.NavigatorButtonPressedGlyphOffset);
    ACanvas.SaveState;
    try
      APainter.DrawNavigatorScaledButtonGlyph(ACanvas, AImages, AImageIndex,
        AGlyphRect, Enabled, not IsStandard or (ImageIndex >= 0), Scheduler.ScaleFactor);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

function TcxSchedulerNavigatorButton.GetState(
  ACanDisabled: Boolean = True): TcxButtonState;
var
  ANavigator: TcxSchedulerResourceNavigator;
begin
  if not Enabled and ACanDisabled then
    Result := cxbsDisabled
  else
  begin
    ANavigator := Scheduler.ResourceNavigator;
    if PtInRect(FBounds, ANavigator.ScreenToClient(GetMouseCursorPos)) and Application.Active then
    begin
      if ANavigator.FPressedButton = Self then
        Result := cxbsPressed
      else
        Result := cxbsHot
    end
    else
      Result := cxbsNormal;
  end;
end;

function TcxSchedulerNavigatorButton.GetActualImageIndex: Integer;
const
  AImageIndexMap: array[0..7] of Integer = (NBDI_FIRST, NBDI_PRIORPAGE,
    NBDI_PRIOR, NBDI_NEXT, NBDI_NEXTPAGE, NBDI_LAST, NBDI_INSERT, NBDI_DELETE);
begin
  if IsStandard and (ImageIndex < 0) then
  begin
    Result := AImageIndexMap[Command];
    if FRotated and (Result < 6) then
      Inc(Result, NavigatorButtonCount);
  end
  else
    Result := ImageIndex;
  if (Result in [NBDI_FIRST, NBDI_PRIORPAGE, NBDI_PRIOR, NBDI_NEXT, NBDI_NEXTPAGE, NBDI_LAST]) and (Scheduler.CurrentView.UseRightToLeftAlignment) then
  case Result of
    NBDI_FIRST:     Result := NBDI_LAST;
    NBDI_PRIORPAGE: Result := NBDI_NEXTPAGE;
    NBDI_PRIOR:     Result := NBDI_NEXT;
    NBDI_NEXT:      Result := NBDI_PRIOR;
    NBDI_NEXTPAGE:  Result := NBDI_PRIORPAGE;
    NBDI_LAST:      Result := NBDI_FIRST;
  end;
end;

function TcxSchedulerNavigatorButton.GetActualImageList: TCustomImageList;
begin
  if IsStandard and (ImageIndex < 0) then
    Result := NavigatorImages
  else
    Result := Scheduler.ResourceNavigator.ButtonImages;
end;

function TcxSchedulerNavigatorButton.GetIsStandard: Boolean;
begin
  Result := (Command >= cxSchedulerFirstButton) and
    (Command <= cxSchedulerShowFewerResourcesButton);
end;

function TcxSchedulerNavigatorButton.GetScheduler: TcxCustomScheduler;
begin
  if GetOwner <> nil then
    Result := TcxSchedulerNavigatorCustomButtons(GetOwner).Scheduler
  else
    Result := nil;
end;

procedure TcxSchedulerNavigatorButton.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    Scheduler.ResourceNavigator.InvalidateButton(Self);
  end;
end;

procedure TcxSchedulerNavigatorButton.SetImageIndex(AValue: Integer);
begin
  AValue := Max(-1, AValue);
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerNavigatorButton.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

function TcxSchedulerNavigatorButton.IsHintStored: Boolean;
begin
  Result := FHint <> '';
end;

function TcxSchedulerNavigatorButton.IsEnabledStored: Boolean;
begin
  Result := not IsStandard and not Enabled;
end;

function TcxSchedulerNavigatorButton.IsVisibleStored: Boolean;
begin
  if not IsStandard then
    Result := not Visible
  else
    Result := Visible <> cxSchedulerNavigatorVisibility[Command];
end;

{ TcxSchedulerNavigatorCustomButtons }

constructor TcxSchedulerNavigatorCustomButtons.CreateEx(
  AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(AItemClass);
end;

function TcxSchedulerNavigatorCustomButtons.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxSchedulerNavigatorCustomButtons.Update(
  Item: TCollectionItem);
begin
  if (Scheduler <> nil) and not Scheduler.IsLocked then
    Scheduler.LayoutChanged;
end;

function TcxSchedulerNavigatorCustomButtons.GetItem(
 AIndex: Integer): TcxSchedulerNavigatorCustomButton;
begin
 Result := TcxSchedulerNavigatorCustomButton(inherited Items[AIndex]);
end;

function TcxSchedulerNavigatorCustomButtons.GetVisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Visible then Inc(Result);
end;

procedure TcxSchedulerNavigatorCustomButtons.SetItem(
 AIndex: Integer; AValue: TcxSchedulerNavigatorCustomButton);
begin
  Items[AIndex].Assign(AValue);
end;

{ TcxSchedulerNavigatorButtons }

constructor TcxSchedulerNavigatorButtons.Create(
  AOwner: TcxSchedulerResourceNavigator);
begin
  FOwner := AOwner;
  FButtons := TcxSchedulerNavigatorCustomButtons.CreateEx(AOwner,
    TcxSchedulerNavigatorButton);
  FButtons.FScheduler := FOwner.Scheduler;
  CreateButtons;
end;

destructor TcxSchedulerNavigatorButtons.Destroy;
begin
  FButtons.Free;
  inherited Destroy;
end;

procedure TcxSchedulerNavigatorButtons.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerNavigatorButtons then
    FButtons.Assign(TcxSchedulerNavigatorButtons(Source).Buttons)
  else
    inherited Assign(Source);
end;

function TcxSchedulerNavigatorButtons.AddButton(
  ACommand: Integer; AVisible: Boolean = True): TcxSchedulerNavigatorButton;
begin
  Result := TcxSchedulerNavigatorButton(Buttons.Add);
  Result.FCommand := ACommand;
  Result.Visible := cxSchedulerNavigatorVisibility[ACommand];
end;

procedure TcxSchedulerNavigatorButtons.CreateButtons;
begin
  AddButton(cxSchedulerFirstButton, True);
  AddButton(cxSchedulerPrevPageButton);
  AddButton(cxSchedulerPrevButton);
  AddButton(cxSchedulerNextButton);
  AddButton(cxSchedulerNextPageButton);
  AddButton(cxSchedulerLastButton, True);
  AddButton(cxSchedulerShowMoreResourcesButton);
  AddButton(cxSchedulerShowFewerResourcesButton);
end;

function TcxSchedulerNavigatorButtons.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TcxSchedulerNavigatorButtons.GetButtonByIndex(
  AIndex: Integer): TcxSchedulerNavigatorButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Buttons.Count - 1 do
  begin
    Result := Buttons[I];
    if Result.Command = AIndex then Break;
  end;
end;

procedure TcxSchedulerNavigatorButtons.SetButtonByIndex(
  AIndex: Integer; AValue: TcxSchedulerNavigatorButton);
begin
  GetButtonByIndex(AIndex).Assign(AValue);
end;

{ TcxSchedulerResourceNavigatorController }

procedure TcxSchedulerResourceNavigatorController.BeforeMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HintController.HideHint;
  inherited BeforeMouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerResourceNavigatorController.CheckButtonDown(
  Button: TMouseButton; Shift: TShiftState);
var
  APressed: Boolean;
begin
  HintController.ViewMode := False;
  APressed := (Button = mbLeft) or (ssLeft in Shift);
  if not APressed then
    ResourceNavigator.FPressedButton := nil;
  if APressed and (ResourceNavigator.FPressedButton = nil) then
    ResourceNavigator.FPressedButton := GetHotTrackButton;
  ResourceNavigator.Invalidate;
end;

function TcxSchedulerResourceNavigatorController.GetHotTrackButton(
  ACanDisabled: Boolean = True): TcxSchedulerNavigatorButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ResourceNavigator.ItemCount - 1 do
    if ResourceNavigator.Items[I].GetState(ACanDisabled) = cxbsHot then
    begin
      Result := ResourceNavigator.Items[I];
      Break;
    end;
end;

procedure TcxSchedulerResourceNavigatorController.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CheckButtonDown(Button, Shift);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerResourceNavigatorController.MouseMove(
  AShift: TShiftState; X, Y: Integer);
begin
  HotTrackButton := GetHotTrackButton(False);
  if ssLeft in AShift then
    CheckButtonDown(mbLeft, AShift)
  else
    CheckButtonDown(mbRight, AShift);
end;

procedure TcxSchedulerResourceNavigatorController.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with ResourceNavigator do
  begin
    if (FPressedButton <> nil) and (FPressedButton.GetState = cxbsPressed) then
      FPressedButton.Click;
  end;
  CheckButtonDown(mbRight, Shift);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxSchedulerResourceNavigatorController.MouseEnter;
begin
  HotTrackButton := nil;
  ResourceNavigator.Invalidate;
end;

procedure TcxSchedulerResourceNavigatorController.MouseLeave;
begin
  HotTrackButton := nil;
  ResourceNavigator.Invalidate;
end;

function TcxSchedulerResourceNavigatorController.GetHintController: TcxSchedulerHintController;
begin
  Result := Owner.Scheduler.HintController;
end;

function TcxSchedulerResourceNavigatorController.GetResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := TcxSchedulerResourceNavigator(Owner);
end;

procedure TcxSchedulerResourceNavigatorController.SetHotTrackButton(
  Value: TcxSchedulerNavigatorButton);
var
  AHintText: string;
  AImmediate: Boolean;
begin
  if Value <> FHotTrackButton then
  begin
    AImmediate := (FHotTrackButton <> nil) and HintController.Showing;
    FHotTrackButton := Value;
    if HotTrackButton <> nil then
    begin
      AHintText := HotTrackButton.GetHintText;
      if AHintText <> '' then
      begin
        HintController.Activate(HintController.CalcHintRect(
          cxscMaxHintWidth, AHintText, cxAlignBottom), AHintText, AImmediate);
      end;
    end
    else
    begin
      HintController.Hide;
      HintController.ViewMode := True;
    end;
  end;
end;

{ TcxSchedulerResourceNavigatorHitTest }

function TcxSchedulerResourceNavigatorHitTest.GetCurrentButton(AButtons: TcxSchedulerNavigatorCustomButtons): TcxSchedulerNavigatorButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AButtons.Count - 1 do
    if cxRectPtIn(AButtons[I].Bounds, HitPoint) then
    begin
      Result := AButtons[I];
      Exit;
    end;
end;

function TcxSchedulerResourceNavigatorHitTest.GetHitAtButton: Boolean;
begin
  Result := GetHitButton <> nil;
end;

function TcxSchedulerResourceNavigatorHitTest.GetHitButton: TcxSchedulerNavigatorButton;
begin
  with TcxSchedulerResourceNavigator(Owner) do
  begin
    Result := GetCurrentButton(Buttons.Buttons);
    if not Assigned(Result) then
      Result := GetCurrentButton(CustomButtons);
  end;
end;

function TcxSchedulerResourceNavigatorHitTest.GetResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := TcxSchedulerResourceNavigator(Owner);
end;

{ TcxSchedulerResourceNavigator }

constructor TcxSchedulerResourceNavigator.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Scheduler);
  FButtons := CreateStandardButtons;
  FCustomButtons := CreateButtons;
  FCustomButtons.FScheduler := Scheduler;
  FShowButtons := True;
  FVisibility := snvAuto;
end;

destructor TcxSchedulerResourceNavigator.Destroy;
begin
  FTimer.Free;
  FButtons.Free;
  FCustomButtons.Free;
  inherited Destroy;
end;

procedure TcxSchedulerResourceNavigator.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerResourceNavigator then
    with TcxSchedulerResourceNavigator(Source) do
    begin
      Self.FShowButtons := FShowButtons;
      Self.FVisibility := FVisibility;
      Self.FButtonImages := FButtonImages;
      Self.Buttons := Buttons;
    end;
  inherited Assign(Source);
end;

function TcxSchedulerResourceNavigator.NeedScrollBar: Boolean;
begin
  Result := Visibility <> snvNever;
  if (Scheduler.CurrentView = nil) or (Scheduler.CurrentView.GetGroupingKind = gkNone) then
    Result := False;
  if not Result or (Visibility = snvAlways) then Exit;
  Result := (ResourceCount > 0) and (ResourcesPerPage > 0);
end;

procedure TcxSchedulerResourceNavigator.BoundsChanged;
begin
end;

procedure TcxSchedulerResourceNavigator.ButtonClickHandler(
  AButton: TcxSchedulerNavigatorButton);
var
  APrevIndex: Integer;
begin
  APrevIndex := FirstVisibleResourceIndex;
  case AButton.Command of
    cxSchedulerFirstButton:
      FirstVisibleResourceIndex := 0;
    cxSchedulerPrevPageButton:
      FirstVisibleResourceIndex := FirstVisibleResourceIndex - ResourcesPerPage;
    cxSchedulerPrevButton:
      FirstVisibleResourceIndex := FirstVisibleResourceIndex - 1;
    cxSchedulerNextButton:
      FirstVisibleResourceIndex := FirstVisibleResourceIndex + 1;
    cxSchedulerNextPageButton:
      FirstVisibleResourceIndex := FirstVisibleResourceIndex + ResourcesPerPage;
    cxSchedulerLastButton:
      FirstVisibleResourceIndex := ResourceCount;
   cxSchedulerShowMoreResourcesButton:
     ResourcesPerPage := ResourcesPerPage + 1;
   cxSchedulerShowFewerResourcesButton:
     ResourcesPerPage := ResourcesPerPage - 1;
  end;
  if APrevIndex <> FirstVisibleResourceIndex then
    Scheduler.FirstVisibleResourceChanged;
end;

procedure TcxSchedulerResourceNavigator.CalculateBounds;
var
  R: TRect;
begin
  if Scheduler.CurrentView = nil then Exit;
  R := Scheduler.CurrentView.Bounds;
  if ScrollBarKind = sbHorizontal then
  begin
    R.Top := R.Bottom - Scheduler.HScrollBar.Height;
    R.Right := R.Left + MeasureWidth;
    if Scheduler.UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, Scheduler.CurrentView.Bounds)
    else
      if Scheduler.UseRightToLeftScrollBar and not Scheduler.IsPopupScrollBars and Scheduler.VScrollBarVisible then
        R := cxRectOffsetHorz(R, Scheduler.VScrollBar.Width);
  end
  else
  begin
    R.Top := R.Bottom - MeasureHeight;
    R.Left := R.Right - Scheduler.VScrollBar.Width;
    if not Scheduler.IsPopupScrollBars and Scheduler.IsSizeGripVisible then
      OffsetRect(R, 0, - cxRectHeight(Scheduler.GetSizeGripBounds));
    if Scheduler.UseRightToLeftScrollBar then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, Scheduler.CurrentView.Bounds);
  end;
  Bounds := R;
end;

procedure TcxSchedulerResourceNavigator.CheckButtonsState;
var
  I: Integer;
  AButton: TcxSchedulerNavigatorButton;
begin
  for I := 0 to Buttons.Buttons.Count - 1 do
  begin
    AButton := Buttons.Buttons[I];
    case AButton.Command of
      cxSchedulerFirstButton, cxSchedulerPrevPageButton, cxSchedulerPrevButton:
        AButton.Enabled := (ResourcesPerPage > 0) and (FirstVisibleResourceIndex > 0);
      cxSchedulerNextButton, cxSchedulerNextPageButton, cxSchedulerLastButton:
        AButton.Enabled := (ResourcesPerPage > 0) and
          (FirstVisibleResourceIndex + ResourcesPerPage < ResourceCount);
     cxSchedulerShowMoreResourcesButton:
        AButton.Enabled := (ResourcesPerPage > 0) and
          (ResourcesPerPage < ResourceCount) and (ResourceCount > 1);
     cxSchedulerShowFewerResourcesButton:
        AButton.Enabled := (ResourcesPerPage > 1) and (ResourceCount > 1);
    end;
  end;
end;

procedure TcxSchedulerResourceNavigator.Click(
  Sender: TcxSchedulerNavigatorButton);
begin
  if not DoOnClick(Sender) and Sender.IsStandard then
    ButtonClickHandler(Sender);
end;

function TcxSchedulerResourceNavigator.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerResourceNavigatorController.Create(Self);
end;

function TcxSchedulerResourceNavigator.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerResourceNavigatorHitTest.Create(Self);
end;

function TcxSchedulerResourceNavigator.CreateButtons: TcxSchedulerNavigatorCustomButtons;
begin
  Result := TcxSchedulerNavigatorCustomButtons.CreateEx(Self, GetCustomButtonClass);
end;

function TcxSchedulerResourceNavigator.CreateStandardButtons: TcxSchedulerNavigatorButtons;
begin
  Result := TcxSchedulerNavigatorButtons.Create(Self);
end;

function TcxSchedulerResourceNavigator.DoCustomDrawButton(
  AButton: TcxSchedulerNavigatorButton): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawButton) then
    OnCustomDrawButton(Self, Canvas, AButton, Result);
end;

function TcxSchedulerResourceNavigator.DoOnClick(
  Sender: TcxSchedulerNavigatorButton): Boolean;
begin
  Result := False;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Sender, Result);
end;

procedure TcxSchedulerResourceNavigator.DoPaint;
var
  R: TRect;
  ASize: TSize;
  I, AIndex: Integer;
  AButton: TcxSchedulerNavigatorButton;
begin
  FVisibleButtonCount := GetVisibleButtonCount;
  if VisibleButtonCount = 0 then Exit;
  CheckButtonsState;
  Canvas.Brush.Color := LookAndFeelPainter.DefaultSchedulerNavigatorColor;
  Canvas.FillRect(ClientRect);

  ASize := ButtonSize;
  AIndex := 0;
  R := Rect(0, 0, ASize.cx, ASize.cy);
  if ScrollBarKind <> sbHorizontal then
    OffsetRect(R, (VisibleButtonCount - 1) * ASize.cx, 0);
  for I := 0 to ItemCount - 1 do
  begin
    AButton := Items[I];
    if AButton.Visible then
    begin
      AButton.FVisibleIndex := AIndex;
      AButton.FBounds := R;
      AButton.FRotated := ScrollBarKind <> sbHorizontal;
      if AButton.FRotated then
        AButton.FBounds := Rect(0, ASize.cx * AIndex, ASize.cy, ASize.cx * (AIndex + 1))
      else
      begin
        AButton.FBounds := Rect(ASize.cx * AIndex, 0, ASize.cx * (AIndex + 1), ASize.cy);
        if Scheduler.UseRightToLeftAlignment then
          AButton.FBounds := TdxRightToLeftLayoutConverter.ConvertRect(AButton.FBounds, ClientRect);
      end;
      OffsetRect(R, (Byte(ScrollBarKind = sbHorizontal) * 2 - 1) * ASize.cx, 0);
      AButton.FState := AButton.GetState;

      if not DoCustomDrawButton(AButton) then
         AButton.Draw(LookAndFeelPainter, Canvas);

      Inc(AIndex);
    end
    else
    begin
      AButton.FBounds := cxNullRect;
      AButton.FVisibleIndex := -1;
    end;
  end;
end;

procedure TcxSchedulerResourceNavigator.FirstVisibleResourceChanged;
begin
  CheckButtonsState;
end;

function TcxSchedulerResourceNavigator.GetCustomButtonClass: TcxSchedulerNavigatorButtonClass;
begin
  Result := TcxSchedulerNavigatorCustomButton;
end;

function TcxSchedulerResourceNavigator.GetScrollerHint: string;
var
  AIndex1, AIndex2: Integer;
  AResources: TcxSchedulerStorageResourceItems;
begin
  AIndex1 := Max(0, FirstVisibleResourceIndex);
  AIndex2 := Min(FirstVisibleResourceIndex + ResourcesPerPage - 1, ResourceCount - 1);
  AResources := Scheduler.Storage.Resources.ResourceItems;
  Result := AResources.VisibleResources[AIndex1].Name;
  if AIndex2 > AIndex1 then
    Result := Result + ' - ' + AResources.VisibleResources[AIndex2].Name;
end;

procedure TcxSchedulerResourceNavigator.InitScrollBarsParameters;
begin
  inherited Visible := NeedScrollBar;
  if not Visible then
    SetScrollBarInfo(ScrollBarKind, 0, 1, 1, 0, 1, False, True)
  else
  begin
    if (ResourcesPerPage = 0) or (ResourceCount = 0) then
      SetScrollBarInfo(ScrollBarKind, 0, 0, 1, 1, 0, True, Visibility = snvAuto)
    else
      SetScrollBarInfo(ScrollBarKind, 0, ResourceCount - 1, 1,
        ResourcesPerPage, FirstVisibleResourceIndex, True, False);
    CalculateBounds;
  end;
end;

procedure TcxSchedulerResourceNavigator.InvalidateButton(
  AButton: TcxSchedulerNavigatorButton);
begin
  if AButton.Visible and (AButton.GetState <> AButton.State) then
    InvalidateRect(AButton.Bounds);
end;

procedure TcxSchedulerResourceNavigator.Scroll(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  AFirstResourceChanged: Boolean;
begin
  Scheduler.CurrentView.HideHintOnScroll(AScrollCode);
  if AScrollCode = scEndScroll then Exit;
  if not (AScrollCode in [scPosition, scTrack]) then
    AScrollPos := FirstVisibleResourceIndex;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, 1);
    scLineDown:
      Inc(AScrollPos, 1);
    scPageUp:
      Dec(AScrollPos, ResourcesPerPage);
    scPageDown:
      Inc(AScrollPos, ResourcesPerPage);
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := ResourceCount;
  end;
  if (AScrollPos + ResourcesPerPage) > (ResourceCount - 1) then
    AScrollPos := ResourceCount - ResourcesPerPage;
  if AScrollPos < 0 then
    AScrollPos := 0;
  if (AScrollPos <> FirstVisibleResourceIndex) or (AScrollCode = scTrack) then
  begin
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
    AFirstResourceChanged := AScrollPos <> FirstVisibleResourceIndex;
    Scheduler.FFirstVisibleResourceIndex := AScrollPos;
    if AScrollCode = scTrack then
      Scheduler.CurrentView.ShowHintOnScroll(GetScrollerHint, ScrollBarKind);
    if ScrollBarKind = sbHorizontal then
      Scheduler.HScrollBar.Position := AScrollPos
    else
      Scheduler.VScrollBar.Position := AScrollPos;
    Scheduler.CurrentView.Refresh;
    if AFirstResourceChanged then
      Scheduler.FirstVisibleResourceChanged;
  end;
end;

function TcxSchedulerResourceNavigator.ActualCountPerPage: Integer;
begin
  if ResourcesPerPage = 0 then
    Result := Max(0, ResourceCount)
  else
    Result := Min(ResourcesPerPage, ResourceCount - 1);
end;

function TcxSchedulerResourceNavigator.ActualFirstResourceIndex: Integer;
begin
  Result := Max(0, FirstVisibleResourceIndex);
  Result := Min(Result - 1, ResourceCount);
end;

function TcxSchedulerResourceNavigator.ButtonSize: TSize;
begin
  if ScrollBarKind = sbHorizontal then
  begin
    Result.cx := (Scheduler.HScrollBar.Height + ScaleFactor.Apply(cxTextOffset));
    Result.cy := Scheduler.HScrollBar.Height {+ Scheduler.BorderSize};
  end
  else
  begin
    Result.cx := Scheduler.VScrollBar.Width + ScaleFactor.Apply(cxTextOffset);
    Result.cy := Scheduler.VScrollBar.Width {+ Scheduler.BorderSize};
  end;
end;

function TcxSchedulerResourceNavigator.MeasureHeight: Integer;
begin
  Result := GetVisibleButtonCount * ButtonSize.cx;
end;

function TcxSchedulerResourceNavigator.MeasureWidth: Integer;
begin
  Result := GetVisibleButtonCount * ButtonSize.cx;
end;

function TcxSchedulerResourceNavigator.GetVisibleButtonCount: Integer;
begin
  if ShowButtons then
    Result := Buttons.Buttons.VisibleCount + CustomButtons.VisibleCount
  else
    Result := 0;
end;

function TcxSchedulerResourceNavigator.GetItemCount: Integer;
begin
  Result := Buttons.Buttons.Count + CustomButtons.Count;
end;

function TcxSchedulerResourceNavigator.GetFirstVisibleResourceIndex: Integer;
begin
  Result := Scheduler.FirstVisibleResourceIndex;
end;

function TcxSchedulerResourceNavigator.GetHitTest: TcxSchedulerResourceNavigatorHitTest;
begin
  Result := TcxSchedulerResourceNavigatorHitTest(inherited HitTest);
end;

function TcxSchedulerResourceNavigator.GetItem(AIndex: Integer): TcxSchedulerNavigatorButton;
begin
  if AIndex < Buttons.Buttons.Count then
    Result := Buttons.Buttons[AIndex]
  else
    Result := CustomButtons[AIndex - Buttons.Buttons.Count];
end;

function TcxSchedulerResourceNavigator.GetResourceCount: Integer;
begin
  with Scheduler do
  begin
    if (CurrentView <> nil) and (CurrentView.Resources <> nil) then
      Result := CurrentView.Resources.VisibleResourceCount
    else
      Result := 0;
  end;
end;

function TcxSchedulerResourceNavigator.GetResourcesPerPage: Integer;
begin
  Result := Scheduler.OptionsView.ResourcesPerPage;
end;

procedure TcxSchedulerResourceNavigator.SetCustomButtons(
  Value: TcxSchedulerNavigatorCustomButtons);
begin
  FCustomButtons.Assign(Value);
end;

procedure TcxSchedulerResourceNavigator.SetButtonImages(Value: TCustomImageList);
begin
  FButtonImages := Value;
  Scheduler.LayoutChanged;
end;

procedure TcxSchedulerResourceNavigator.SetButtons(
  Value: TcxSchedulerNavigatorButtons);
begin
  FButtons.Assign(Value);
end;

procedure TcxSchedulerResourceNavigator.SetResourcesPerPage(AValue: Integer);
begin
  Scheduler.OptionsView.ResourcesPerPage := AValue;
end;

procedure TcxSchedulerResourceNavigator.SetShowButtons(AValue: Boolean);
begin
  if AValue <> FShowButtons then
  begin
    FShowButtons := AValue;
    Scheduler.LayoutChanged;
  end;
end;

procedure TcxSchedulerResourceNavigator.SetFirstVisibleResourceIndex(AValue: Integer);
begin
  Scheduler.FirstVisibleResourceIndex := AValue;
end;

procedure TcxSchedulerResourceNavigator.SetVisibility(
  AValue: TcxSchedulerNavigatorVisibilityMode);
begin
  if AValue <> FVisibility then
  begin
    FVisibility := AValue;
    Changed;
  end;
end;

function TcxSchedulerResourceNavigator.IsCustomButtonsStored: Boolean;
begin
  Result := CustomButtons.Count > 0;
end;

{ TcxSchedulerEventOperations }

constructor TcxSchedulerEventOperations.Create(
  AOwner: TcxCustomScheduler);
begin
  FScheduler := AOwner;
  FCreating := True;
  FDeleting := True;
  FDialogEditing := True;
  FDialogShowing := True;
  FInplaceEditing := True;
  FIntersection := True;
  FMoving := True;
  FMovingBetweenResources := True;
  FReadOnly := False;
  FRecurrence := True;
  FSizing := True;
end;

procedure TcxSchedulerEventOperations.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerEventOperations then
    with TcxSchedulerEventOperations(Source) do
    begin
      Self.FCreating := FCreating;
      Self.FDeleting := FDeleting;
      Self.FDialogEditing := FDialogEditing;
      Self.FDialogShowing := FDialogShowing;
      Self.FInplaceEditing := FInplaceEditing;
      Self.FMoving := FMoving;
      Self.FMovingBetweenResources := FMovingBetweenResources;
      Self.FReadOnly := FReadOnly;
      Self.FRecurrence := FRecurrence;
      Self.FSharingBetweenResources := FSharingBetweenResources;
      Self.FSizing := FSizing;
      Self.FIntersection := FIntersection;
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerEventOperations.GetOwner: TPersistent;
begin
  Result := FScheduler;
end;

function TcxSchedulerEventOperations.GetCreating: Boolean;
begin
  Result := not FReadOnly and FCreating;
end;

function TcxSchedulerEventOperations.GetCreatingStored: Boolean;
begin
  Result := not (FReadOnly or FCreating);
end;

function TcxSchedulerEventOperations.GetDeleting: Boolean;
begin
  Result := not FReadOnly and FDeleting;
end;

function TcxSchedulerEventOperations.GetDeletingStored: Boolean;
begin
  Result := not (FReadOnly or FDeleting);
end;

function TcxSchedulerEventOperations.GetDialogEditing: Boolean;
begin
  Result := not FReadOnly and FDialogEditing;
end;

function TcxSchedulerEventOperations.GetDialogEditingStored: Boolean;
begin
  Result := not (FReadOnly or FDialogEditing);
end;

function TcxSchedulerEventOperations.GetInplaceEditing: Boolean;
begin
  Result := not FReadOnly and FInplaceEditing;
end;

function TcxSchedulerEventOperations.GetInplaceEditingStored: Boolean;
begin
  Result := not (FReadOnly or FInplaceEditing);
end;

function TcxSchedulerEventOperations.GetMoving: Boolean;
begin
  Result := not FReadOnly and FMoving;
end;

function TcxSchedulerEventOperations.GetMovingBetweenResources: Boolean;
begin
  Result := not FReadOnly and FMovingBetweenResources;
end;

function TcxSchedulerEventOperations.GetMovingBetweenResourcesStored: Boolean;
begin
  Result := not (FReadOnly or FMovingBetweenResources);
end;

function TcxSchedulerEventOperations.GetMovingStored: Boolean;
begin
  Result := not (FReadOnly or FMoving);
end;

function TcxSchedulerEventOperations.GetSizing: Boolean;
begin
  Result := not FReadOnly and FSizing;
end;

function TcxSchedulerEventOperations.GetSizingStored: Boolean;
begin
  Result := not (FReadOnly or FSizing);
end;

{ TcxSchedulerOptionsCustomize }

constructor TcxSchedulerOptionsCustomize.Create(
  AOwner: TcxCustomScheduler);
begin
  FScheduler := AOwner;
  FControlsSizing := True;
  FIntegralSizing := True;
end;

procedure TcxSchedulerOptionsCustomize.Assign(Source: TPersistent);
var
  ASourceOptionsCustomize: TcxSchedulerOptionsCustomize;
begin
  if Source is TcxSchedulerOptionsCustomize then
  begin
    ASourceOptionsCustomize := TcxSchedulerOptionsCustomize(Source);
    FControlsSizing := ASourceOptionsCustomize.ControlsSizing;
    FDynamicSizing := ASourceOptionsCustomize.DynamicSizing;
    IntegralSizing := ASourceOptionsCustomize.IntegralSizing;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSchedulerOptionsCustomize.Changed;
begin
  FScheduler.LayoutChanged;
end;

function TcxSchedulerOptionsCustomize.GetOwner: TPersistent;
begin
  Result := FScheduler;
end;

procedure TcxSchedulerOptionsCustomize.SetControlsSizing(AValue: Boolean);
begin
  FControlsSizing := AValue;
  Scheduler.HorzSplitter.UpdateCursor;
  Scheduler.VertSplitter.UpdateCursor;
end;

procedure TcxSchedulerOptionsCustomize.SetIntegralSizing(AValue: Boolean);
begin
  if AValue <> FIntegralSizing then
  begin
    FIntegralSizing := AValue;
    if AValue then
      Scheduler.DateNavigator.SetIntegralSizes;
    Changed;
  end;
end;

{ TcxSchedulerResourceHeaders }

constructor TcxSchedulerResourceHeaders.Create(AOwner: TcxSchedulerOptionsView);
begin
  FRotateCaptions := True;
  FOwner := AOwner;
end;

procedure TcxSchedulerResourceHeaders.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerResourceHeaders then
  begin
    FHeight := TcxSchedulerResourceHeaders(Source).FHeight;
    FImagePosition := TcxSchedulerResourceHeaders(Source).FImagePosition;
    FMultilineCaptions := TcxSchedulerResourceHeaders(Source).FMultilineCaptions;
    FRotateCaptions := TcxSchedulerResourceHeaders(Source).FRotateCaptions;
  end
  else
    inherited;
end;

procedure TcxSchedulerResourceHeaders.Changed;
begin
  FOwner.Changed;
end;

procedure TcxSchedulerResourceHeaders.ChangeScale(M, D: Integer);
begin
  Height := MulDiv(Height, M, D);
end;

function TcxSchedulerResourceHeaders.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxSchedulerResourceHeaders.SetHeight(AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerResourceHeaders.SetImagePosition(
  AValue: TcxSchedulerHeaderImagePosition);
begin
  if AValue <> FImagePosition then
  begin
    FImagePosition := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerResourceHeaders.SetMultilineCaptions(AValue: Boolean);
begin
  if AValue <> FMultilineCaptions then
  begin
    FMultilineCaptions := AValue;

    Changed;
  end;
end;

procedure TcxSchedulerResourceHeaders.SetRotateCations(AValue: Boolean);
begin
  if AValue <> FRotateCaptions then
  begin
    FRotateCaptions := AValue;
    Changed;
  end;
end;

function TcxSchedulerResourceHeaders.IsImagePositionStored: Boolean;
begin
  Result := FImagePosition <> ipLeft;
end;

{ TcxSchedulerOptionsView }

constructor TcxSchedulerOptionsView.Create(
  AOwner: TcxCustomScheduler);
begin
  FScheduler := AOwner;
  FStyle := cxSchedulerDefaultViewStyle;
  FTimeZones[0] := -1;
  FTimeZones[1] := -1;
  FDayBorderColor := clDefault;
  FEventHeight := 0;
  FEventBorderColor := clBlack;
  FGroupSeparatorWidth := cxDefaultGroupSeparatorWidth;
  FHorzSplitterWidth := cxDefaultSplitterWidth;
  FVertSplitterWidth := cxDefaultSplitterWidth;
  FHideSelection := False;
  FHotTrack := False; //for the next version True;
  FResourceHeaders := TcxSchedulerResourceHeaders.Create(Self);
  FWorkDays := DateTimeHelper.WorkDays;
  FStartOfWeek := swSystem;
  CalculateActualStartOfWeek;
  FWorkFinish := DateTimeHelper.WorkFinish;
  FWorkStart := DateTimeHelper.WorkStart;
  FViewPosition := vpLeft;
  FResourcesPerPage := cxDefaultResourcesPerPage;
  ShowHints := True;
  ShowNavigationButtons := True;
end;

destructor TcxSchedulerOptionsView.Destroy;
begin
  FResourceHeaders.Free;
  inherited Destroy;
end;

procedure TcxSchedulerOptionsView.Assign(
  Source: TPersistent);
var
  AItem: Integer;
  ASourceOptionsView: TcxSchedulerOptionsView;
begin
  if Source is TcxSchedulerOptionsView then
  begin
    ASourceOptionsView := TcxSchedulerOptionsView(Source);
    for AItem := 0 to 1 do
    begin
      FTimeZones[AItem] := ASourceOptionsView.FTimeZones[AItem];
      FTimeZoneLabels[AItem] := ASourceOptionsView.FTimeZoneLabels[AItem];
    end;
    FResourcesPerPage := ASourceOptionsView.FResourcesPerPage;
    FDayBorderColor := ASourceOptionsView.DayBorderColor;
    FEventBorderColor := ASourceOptionsView.EventBorderColor;
    FEventHeight := ASourceOptionsView.EventHeight;
    FGroupingKind := ASourceOptionsView.FGroupingKind;
    FGroupSeparatorWidth := ASourceOptionsView.FGroupSeparatorWidth;
    FHideSelection := ASourceOptionsView.HideSelection;
    FHorzSplitterWidth := ASourceOptionsView.FHorzSplitterWidth;
    FHotTrack := ASourceOptionsView.HotTrack;
    FResourceHeaders.Assign(ASourceOptionsView.ResourceHeaders);
    FShowHints := ASourceOptionsView.ShowHints;
    FShowNavigationButtons := ASourceOptionsView.FShowNavigationButtons;
    FVertSplitterWidth := ASourceOptionsView.FVertSplitterWidth;
    FViewPosition := ASourceOptionsView.ViewPosition;
    FWorkDays := ASourceOptionsView.WorkDays;
    FWorkFinish := ASourceOptionsView.WorkFinish;
    FWorkStart := ASourceOptionsView.WorkStart;
    FStyle := ASourceOptionsView.Style;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSchedulerOptionsView.CalculateActualStartOfWeek;
begin
  if FStartOfWeek = swSystem then
    FActualStartOfWeek := TDay(DateTimeHelper.StartOfWeek)
  else
    FActualStartOfWeek := TDay(Pred(FStartOfWeek));
end;

procedure TcxSchedulerOptionsView.Changed;
begin
  Scheduler.LayoutChanged;
end;

procedure TcxSchedulerOptionsView.ChangeScale(M, D: Integer);
begin
  EventHeight := MulDiv(EventHeight, M, D);
  GroupSeparatorWidth := MulDiv(GroupSeparatorWidth, M, D);
  HorzSplitterWidth := MulDiv(HorzSplitterWidth, M, D);
  VertSplitterWidth := MulDiv(VertSplitterWidth, M, D);
  ResourceHeaders.ChangeScale(M, D);
end;

procedure TcxSchedulerOptionsView.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('WorkStart', ReadWorkStart, WriteWorkStart, FWorkStartAssigned);
  Filer.DefineProperty('WorkFinish', ReadWorkFinish, WriteWorkFinish, FWorkFinishAssigned);
end;

function TcxSchedulerOptionsView.GetOwner: TPersistent;
begin
  Result := FScheduler;
end;

function TcxSchedulerOptionsView.IsWorkDaysStored: Boolean;
begin
  Result := FWorkDays <> DateTimeHelper.WorkDays;
end;

function TcxSchedulerOptionsView.IsWorkTime(AResourceItem: TcxSchedulerStorageResourceItem;
  const ADateTime: TDateTime): Boolean;
var
  AWorkDays: TDays;
  AWorkFinish: TTime;
  AWorkStart: TTime;
begin
  if Assigned(AResourceItem) then
  begin
    AWorkDays := AResourceItem.WorkDays;
    AWorkFinish := AResourceItem.WorkFinish;
    AWorkStart := AResourceItem.WorkStart;
  end
  else
  begin
    AWorkDays := WorkDays;
    AWorkStart := WorkStart;
    AWorkFinish := WorkFinish;
  end;

  Result := (ADateTime <> NullDate) and (TDay(DayOfWeek(ADateTime) - 1) in AWorkDays);
  if Result then
  begin
    if AWorkStart < AWorkFinish then
      Result := ((DateTimeHelper.RoundTime(dxTimeOf(ADateTime)) >= AWorkStart) and
       (DateTimeHelper.RoundTime(dxTimeOf(ADateTime)) < AWorkFinish))
    else
      Result := ((DateTimeHelper.RoundTime(dxTimeOf(ADateTime)) >= AWorkStart) or
       (DateTimeHelper.RoundTime(dxTimeOf(ADateTime)) < AWorkFinish))
  end;
end;

function TcxSchedulerOptionsView.GetDateTimeHelperClass: TcxSchedulerDateTimeHelperClass;
begin
  Result := Scheduler.DateTimeHelper;
end;

function TcxSchedulerOptionsView.GetRotateResourceCaptions: Boolean;
begin
  Result := FResourceHeaders.RotateCaptions;
end;

function TcxSchedulerOptionsView.GetTimeZone(
  AIndex: Integer): Integer;
begin
  Result := FTimeZones[AIndex];
  if (Result < 0) or (Result >= DateTimeHelper.TimeZoneCount) then
    Result := -1;
end;

function TcxSchedulerOptionsView.GetTimeZoneLabel(
  AIndex: Integer): string;
begin
  Result := FTimeZoneLabels[AIndex];
end;

function TcxSchedulerOptionsView.IsTimeZoneLabelStored(
  AIndex: Integer): Boolean;
begin
  Result := FTimeZoneLabels[AIndex] <> '';
end;

procedure TcxSchedulerOptionsView.SetATZDaylightSaving(AValue: Boolean);
begin
  if AValue <> FAdditionalTimeZoneDaylightSaving then
  begin
    FAdditionalTimeZoneDaylightSaving := AValue;
    Scheduler.FullRefresh;
  end;
end;

procedure TcxSchedulerOptionsView.SetCTZDaylightSaving(AValue: Boolean);
begin
  if AValue <> FCurrentTimeZoneDaylightSaving then
  begin
    FCurrentTimeZoneDaylightSaving := AValue;
    Scheduler.FullRefresh;
  end;
end;

procedure TcxSchedulerOptionsView.SetDayBorderColor(AValue: TColor);
begin
  if AValue <> FDayBorderColor then
  begin
    FDayBorderColor := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetEventBorderColor(AValue: TColor);
begin
  if AValue <> FEventBorderColor then
  begin
    FEventBorderColor := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetEventHeight(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if AValue <> FEventHeight then
  begin
    FEventHeight := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetGroupingKind(
  AValue: TcxSchedulerGroupingKind);
begin
  if FGroupingKind <> AValue then
  begin
    FGroupingKind := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetGroupSeparatorWidth(
  AValue: Integer);
begin
  FGroupSeparatorWidth := Max(0, AValue);
  Changed;
end;

procedure TcxSchedulerOptionsView.SetHideSelection(AValue: Boolean);
begin
  if FHideSelection <> AValue then
  begin
    FHideSelection := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetHorzSplitterWidth(AValue: Integer);
begin
  SetSplitterWidth(AValue, FHorzSplitterWidth);
end;

procedure TcxSchedulerOptionsView.SetResourceHeaders(
  AValue: TcxSchedulerResourceHeaders);
begin
  FResourceHeaders.Assign(AValue)
end;

procedure TcxSchedulerOptionsView.SetRotateResourceCaptions(AValue: Boolean);
begin
  ResourceHeaders.RotateCaptions := AValue;
end;

procedure TcxSchedulerOptionsView.SetResourcesPerPage(
  AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if AValue <> FResourcesPerPage then
  begin
    FResourcesPerPage := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetShowAdditionalTimeZone(AValue: Boolean);
begin
  if FShowAdditionalTimeZone <> AValue then
  begin
    FShowAdditionalTimeZone := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetShowEventsWithoutResource(
  AValue: Boolean);
begin
  if AValue <> FShowEventsWithoutResource then
  begin
    FShowEventsWithoutResource := AValue;
    Scheduler.FullRefresh;
  end;
end;

procedure TcxSchedulerOptionsView.SetShowNavigationButtons(AValue: Boolean);
begin
  if AValue <> FShowNavigationButtons then
  begin
    FShowNavigationButtons := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetSplitterWidth(AValue: Integer; var AWidth: Integer);
begin
  AValue := Max(AValue, FScheduler.ScaleFactor.Apply(cxMinSplitterWidth));
  if AValue <> AWidth then
  begin
    AWidth := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetStartOfWeek(AValue: TcxStartOfWeek);
var
  AOldActualValue: TDay;
begin
  if AValue <> FStartOfWeek then
  begin
    FStartOfWeek := AValue;
    AOldActualValue := ActualStartOfWeek;
    CalculateActualStartOfWeek;
    if not Scheduler.IsLoading then
      Scheduler.DoStartOfWeekChanged(AOldActualValue, ActualStartOfWeek);
  end;
end;

procedure TcxSchedulerOptionsView.SetStyle(AValue: TcxSchedulerViewStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    Scheduler.FullRefresh;
  end;
end;

procedure TcxSchedulerOptionsView.SetTimeZone(
  AIndex, AValue: Integer);
begin
  AValue := Max(Min(AValue, DateTimeHelper.TimeZoneCount - 1), -1);
  if AValue <> FTimeZones[AIndex] then
  begin
    FTimeZones[AIndex] := AValue;
    Scheduler.FullRefresh;
  end;
end;

procedure TcxSchedulerOptionsView.SetTimeZoneLabel(
  AIndex: Integer; const AValue: string);
begin
  if AValue <> FTimeZoneLabels[AIndex] then
  begin
    FTimeZoneLabels[AIndex] := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetVertSplitterWidth(AValue: Integer);
begin
  SetSplitterWidth(AValue, FVertSplitterWidth);
end;

procedure TcxSchedulerOptionsView.SetViewPosition(
  AValue: TcxSchedulerViewPosition);
begin
  if AValue <> FViewPosition then
  begin
    FViewPosition := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerOptionsView.SetWorkDays(AValue: TDays);
begin
  if AValue <> FWorkDays then
  begin
    FWorkDays := AValue;
    Scheduler.PeriodChanged;
  end;
end;

procedure TcxSchedulerOptionsView.SetWorkFinish(AValue: TTime);
begin
  if AValue <> FWorkFinish then
  begin
    FWorkFinish := DateTimeHelper.RoundTime(AValue);
    FWorkFinishAssigned := FWorkFinish <> DateTimeHelper.WorkFinish;
    Scheduler.PeriodChanged;
  end;
end;

procedure TcxSchedulerOptionsView.SetWorkStart(AValue: TTime);
begin
  if AValue <> FWorkStart then
  begin
    FWorkStart := DateTimeHelper.RoundTime(AValue);
    FWorkStartAssigned := FWorkStart <> DateTimeHelper.WorkStart;
    Scheduler.PeriodChanged;
  end;
end;

procedure TcxSchedulerOptionsView.ReadWorkFinish(AReader: TReader);
begin
  FWorkFinish := AReader.ReadFloat;
end;

procedure TcxSchedulerOptionsView.ReadWorkStart(AReader: TReader);
begin
  FWorkStart := AReader.ReadFloat;
end;

procedure TcxSchedulerOptionsView.WriteWorkFinish(AWriter: TWriter);
begin
  AWriter.WriteFloat(FWorkFinish);
end;

procedure TcxSchedulerOptionsView.WriteWorkStart(AWriter: TWriter);
begin
  AWriter.WriteFloat(FWorkStart);
end;

{ TcxSchedulerViewHitTest }

function TcxSchedulerViewHitTest.GetDragKind: TcxEventDragKind;
begin
  Result := edkNone;
end;

procedure TcxSchedulerViewHitTest.Clear;
begin
  inherited Clear;
  FNeedShowHint := False;
  FTime := NullDate;
end;

function TcxSchedulerViewHitTest.GetHitEvent: TcxSchedulerControlEvent;
begin
  Result := nil;
end;

function TcxSchedulerViewHitTest.GetHitAtEvent: Boolean;
begin
  Result := Event <> nil;
end;

function TcxSchedulerViewHitTest.GetNeedShowHint: Boolean;
var
  AText: string;
begin
  Result := FNeedShowHint or (HitAtEvent and Scheduler.NeedShowHint(Event, AText, False));
end;

{ TcxDragHelper }

constructor TcxDragHelper.Create(AScheduler: TcxCustomScheduler);
begin
  FScheduler := AScheduler;
  BeginDrag;
end;

procedure TcxDragHelper.BeginDrag;
begin
  FSaveCursor := Scheduler.DragCursor;
  GetOriginState;
  FPrevAccepted := True;
end;

procedure TcxDragHelper.CalculateConflicts;
begin
  FHasConflicts := Scheduler.HasConflict(False);
end;

procedure TcxDragHelper.CalculateDestination;
var
  P: TPoint;
begin
  FDestination := dodOther;
  with Scheduler do
  begin
    P := ScreenToClient(GetMouseCursorPos);
    if CurrentView.Visible and
       cxRectPtIn(cxRectOffset(CurrentView.ViewInfo.Bounds, CurrentView.Bounds.TopLeft), P) then
      FDestination := dodView
    else
      if DateNavigator.Visible and cxRectPtIn(DateNavigator.Bounds, P) then
      begin
        FDestination := dodDateNavigator;
        DateNavigator.MousePositionChanged(P.X, P.Y);
      end
      else
        if ControlBox.Visible and cxRectPtIn(ControlBox.Bounds, P) then
          FDestination := dodControlBox;
  end;
end;

function TcxDragHelper.CanProcessDragOver: Boolean;
begin
  Result := True;
end;

function TcxDragHelper.CanUpdateEventState(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  with AEvent.Source do
    Result := State = cxOriginalEventStates[AllDayEvent];
end;

procedure TcxDragHelper.CheckAccepted(var Accepted: Boolean);
begin
  Accepted := Accepted and (Destination <> dodOther) and IsValidTime;
end;

procedure TcxDragHelper.CheckEventState(AEvent: TcxSchedulerControlEvent);
var
  ANewState: Integer;
begin
  if CanUpdateEventState(AEvent) then
  begin
    ANewState := cxOriginalEventStates[AEvent.AllDayEvent];
    if AEvent.State <> ANewState then
      AEvent.State := ANewState;
  end;
end;

procedure TcxDragHelper.DragOver(const P: TPoint; State: TDragState;
  var Accepted: Boolean);
begin
  CalculateDestination;
  FActualHitTime := GetActualHitTime;
  CheckAccepted(Accepted);
  FAcceptedChanged := Accepted <> FPrevAccepted;
end;

procedure TcxDragHelper.EndDrag(Accepted: Boolean);
begin
end;

function TcxDragHelper.GetOriginHitTestMask: Int64;
const
  Mask = 1 shl htcControl;
begin
  Result := Mask;
end;

procedure TcxDragHelper.GetOriginState;
begin
  with Controller do
  begin
    FStartHitFlags := StartDragFlags and GetOriginHitTestMask;
    FStartHitTime := StartDragHitTime;
    FStartResource := StartDragResource;
  end;
  FPrevHitTime := FStartHitTime;
  FPrevHitFlags := FStartHitFlags;
  FPrevHitResource := FStartResource;
end;

function TcxDragHelper.HasChangedState: Boolean;
begin
  with HitTest do
    Result := FAcceptedChanged or
      (FPrevHitFlags <> (Flags and GetOriginHitTestMask)) or
      (IsValidTime and (FPrevHitTime <> GetActualHitTime)) or
      (HitAtResource and (FPrevHitResource <> Resource));
end;

function TcxDragHelper.IsAtOrigin: Boolean;
begin
  with HitTest do
  begin
    Result := (FStartHitFlags = (Flags and GetOriginHitTestMask)) and
      (Controller.StartDragHitTime = Time);
    if Result and IsShowResources then
      Result := HitAtResource and (FStartResource = Resource);
  end;
end;

function TcxDragHelper.IsShowResources: Boolean;
begin
  Result := Scheduler.CurrentView.IsShowResources;
end;

function TcxDragHelper.IsValidTime: Boolean;
begin
  Result := HitTest.HitAtTime;
end;

procedure TcxDragHelper.RefreshCurrentView;
begin
  Scheduler.CurrentView.Refresh;
end;

procedure TcxDragHelper.SetSelection;
begin
  Controller.SyncEventSelection(Controller.DragEvent);
end;

procedure TcxDragHelper.UpdateHelperState(Accepted: Boolean);
begin
  with HitTest do
  begin
    FPrevHitFlags := Flags and GetOriginHitTestMask;
    FPrevHitTime := GetActualHitTime;
    FPrevHitResource := Resource;
  end;
  FPrevAccepted := Accepted;
end;

function TcxDragHelper.GetActualHitTime: TDateTime;
begin
  if Destination = dodDateNavigator then
    Result := DateNavigator.HitTest.Time
  else
    Result := HitTest.Time;
end;

function TcxDragHelper.GetController: TcxSchedulerViewController;
begin
  Result := Scheduler.CurrentView.Controller;
end;

function TcxDragHelper.GetDateNavigator: TcxSchedulerCustomDateNavigator;
begin
  Result := Scheduler.DateNavigator;
end;

function TcxDragHelper.GetEvents: TcxSchedulerCachedEventList;
begin
  Result := Scheduler.EventList;
end;

function TcxDragHelper.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := Scheduler.CurrentView.HitTest;
end;

{ TcxSchedulerDragObject}

constructor TcxSchedulerDragObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FUseInternalCursors := (Control is TcxCustomScheduler) and
    (Scheduler.DragCursor = crDrag);
end;

procedure TcxSchedulerDragObject.DropToDateNavigator(
  ADateNavigator: TcxSchedulerCustomDateNavigator);
begin
  if not ADateNavigator.HitTest.HitAtTime then Exit;
  DragEventHelper.ProcessDateNavigator(ADateNavigator);
end;

procedure TcxSchedulerDragObject.CalculateConflictsForDateNavigator(
  ADateNavigator: TcxSchedulerCustomDateNavigator);
begin
  with DragEventHelper do
  begin
    UpdateDateNavigatorClones(ADateNavigator);
    CalculateConflicts;
  end;
end;

procedure TcxSchedulerDragObject.Finished(
  Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  if not Accepted and (Target is TcxCustomScheduler) and (Target <> Scheduler) then
    TcxCustomScheduler(Target).DragCanceled;
  inherited Finished(Target, X, Y, Accepted);
end;

function TcxSchedulerDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;

   function CanAcceptEvent: Boolean;
   begin
     with DragEventHelper do
       Result := not HasConflicts and FPrevAccepted;
   end;

var
  R: TRect;
begin
  if Assigned(DragEventHelper) and Accepted and FUseInternalCursors then
  begin
    R := cxGetWindowRect(Scheduler);
    if CanAcceptEvent or not cxRectPtIn(R, X, Y) then
      if Scheduler.IsCopyDragDrop then
        Result := crSchedulerCopyEvent
      else
        Result := crSchedulerMoveEvent
    else
      Result := crNoDrop
  end
  else
    Result := inherited GetDragCursor(Accepted, X, Y);
end;

function TcxSchedulerDragObject.GetDragEventHelper: TcxDragEventHelper;
begin
  Result := Scheduler.CurrentView.Controller.DragEventHelper;
end;

function TcxSchedulerDragObject.GetDragEvents: TcxSchedulerFilteredEventList;
begin
  Result := Scheduler.EventList.Clones;
end;

function TcxSchedulerDragObject.GetHasConflicts: Boolean;
begin
  Result := (DragEventHelper <> nil) and DragEventHelper.HasConflicts;
end;

function TcxSchedulerDragObject.GetScheduler: TcxCustomScheduler;
begin
  Result := TcxCustomScheduler(Control);
end;

{ TcxDragEventHelper }

procedure TcxDragEventHelper.ApplyChanges;
begin
  ValidateDestinationTime;
  SetSelection;
  Events.PostClones(FPrevIsDragCopy);
end;

procedure TcxDragEventHelper.BeginDrag;
begin
  inherited BeginDrag;
  PrepareClones;
  CheckVisibility(True);
end;

function TcxDragEventHelper.CanProcessDragOver: Boolean;
begin
  Result := not FValidatingDestinationTime;
end;

procedure TcxDragEventHelper.CheckAccepted(var Accepted: Boolean);

  function CanMoveBetweenResources: Boolean;
  var
    AResource: TcxSchedulerStorageResourceItem;
  begin
    AResource := HitTest.Resource;
    Result := (FStartResource = AResource) or
      (Scheduler.EventOperations.MovingBetweenResources and
      Scheduler.Storage.IsEventSharingAllowed and
       ((AResource = nil) or AResource.CanModify));
  end;

begin
  inherited CheckAccepted(Accepted);
  if not Accepted or (Destination = dodControlBox) then Exit;
  if (Destination = dodView) and IsShowResources and HitTest.HitAtResource and
    not CanMoveBetweenResources then Accepted := False;
  if DateNavigator.Visible then
    UpdateDateNavigator(Accepted);
end;

procedure TcxDragEventHelper.CheckVisibility(Accepted: Boolean);
begin
  Inc(Scheduler.FLockRefresh);
  try
    Events.AlwaysShowSelectedEvent := not Accepted or (Destination <> dodView);
    ViewInfo.SetEventsVisibility(GetSourcesVisible(Accepted),
      GetClonesVisible(Accepted), True);
  finally
    Dec(Scheduler.FLockRefresh);
  end;
end;

procedure TcxDragEventHelper.DragOver(const P: TPoint; State: TDragState;
  var Accepted: Boolean);
var
  ANeedUpdate: Boolean;
begin
  inherited DragOver(P, State, Accepted);
  ANeedUpdate := HasChangedState or (State in [dsDragLeave, dsDragEnter]);
  if Accepted and ANeedUpdate then
  begin
    Accepted := DragObject.Dropped or (State <> dsDragLeave);
    if Accepted then
      UpdateClones;
  end;
  if ANeedUpdate then
  begin
    CalculateConflicts;
    CheckVisibility(Accepted);
    if State = dsDragLeave then
      Scheduler.UpdateDateNavigatorDragging(False);
  end;
  Controller.CheckScrollOnDragOver(P, State);
end;

procedure TcxDragEventHelper.EndDrag(Accepted: Boolean);
var
  AAccepted: Boolean;
begin
  AAccepted := False;
  try
    Events.AlwaysShowSelectedEvent := False;
    CalculateDestination;
    CheckAccepted(Accepted);
    Controller.CancelScroll;
    Scheduler.DragCursor := SaveCursor;
    FPrevIsDragCopy := IsDragCopy;
    if Accepted and IsValidTime and not (IsAtOrigin and not FPrevIsDragCopy) then
    begin
      UpdateClones;
      CalculateConflicts;
      if not HasConflicts then
      begin
        Scheduler.DoAfterDragEvent(FTarget, HitTest.HitX, HitTest.HitY, Accepted);
        if Accepted then
        begin
          ApplyChanges;
          AAccepted := True;
        end;
      end;
    end;
  finally
    Events.CancelClones;
    DateNavigatorEndDrag;
    if not AAccepted then
      Scheduler.FullRefresh;
  end;
end;

procedure TcxDragEventHelper.DateNavigatorEndDrag;
begin
  if DateNavigator.Visible then
    DateNavigator.Controller.DoCancelMode;
end;

procedure TcxDragEventHelper.DeleteClone(AEvent: TcxSchedulerControlEvent;
  AStatus: TcxOccurrenceDateStatus);
var
  AMessage: string;
begin
  AMessage := cxGetOccurrenceDateStatusMessage(AEvent, AStatus);
  try
    ViewInfo.DeleteClone(AEvent);
    MessageBox(Scheduler.Handle, PChar(AMessage), nil, MB_ICONEXCLAMATION or MB_OK);
  finally
    AEvent.Free;
  end;
end;

function TcxDragEventHelper.GetClonesVisible(Accepted: Boolean): Boolean;
begin
  Result := Accepted and ((Destination = dodView) and not (IsAtOrigin and not IsDragCopy));
end;

function TcxDragEventHelper.GetIsDragCopy: Boolean;
begin
  Result := Scheduler.IsCopyDragDrop;
end;

procedure TcxDragEventHelper.GetOriginState;
begin
  inherited GetOriginState;
  FPrevIsDragCopy := IsDragCopy;
end;

function TcxDragEventHelper.GetSourcesVisible(Accepted: Boolean): Boolean;
begin
  Result := not Accepted or
    ((Destination in [dodDateNavigator, dodControlBox]) or IsAtOrigin or IsDragCopy);
end;

function TcxDragEventHelper.HasChangedState: Boolean;
begin
  Result := inherited HasChangedState or (FPrevIsDragCopy <> IsDragCopy);
end;

function TcxDragEventHelper.IsValidNavigatorDate: Boolean;
begin
  Result := (Destination = dodDateNavigator) and DateNavigator.HitTest.HitAtTime;
end;

function TcxDragEventHelper.IsValidTime: Boolean;
begin
  Result := HitTest.HitAtTime or IsValidNavigatorDate;
end;

procedure TcxDragEventHelper.PrepareClones;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
  AResource, ADragEventResource: TcxSchedulerStorageResourceItem;
begin
  ADragEventResource := Controller.DragEvent.GetResourceItem;
  for I := Events.Selection.Count - 1 downto 0 do
  begin
    AEvent := Events.Selection.Items[I];
    AResource := AEvent.GetResourceItem;
    if IsShowResources and (AResource <> nil) and (AResource <> ADragEventResource) then
      Events.Selection.Add(AEvent, [ssCtrl]);
  end;
  Events.CreateClones;
end;

procedure TcxDragEventHelper.ProcessDateNavigator(
  ADateNavigator: TcxSchedulerCustomDateNavigator);
begin
  try
    UpdateDateNavigatorClones(ADateNavigator);
    ValidateDestinationTime;
  finally
    Events.PostClones(IsDragCopy);
  end;
end;

procedure TcxDragEventHelper.SetSelection;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  for I := 0 to Clones.Count - 1 do
  begin
    AEvent := Clones[I];
    if AEvent.Source = Controller.DragEvent then
      Controller.SyncEventSelection(AEvent);
  end;
end;

procedure TcxDragEventHelper.Update(Accepted: Boolean = True);
begin
  DragOver(ViewInfo.Owner.ScreenToClient(GetMouseCursorPos), dsDragMove, Accepted);
end;

procedure TcxDragEventHelper.UpdateHelperState(Accepted: Boolean);
begin
  inherited UpdateHelperState(Accepted);
  FPrevIsDragCopy := IsDragCopy;
end;

procedure TcxDragEventHelper.UpdateClones;
begin
  if Destination = dodView then
    UpdateViewClones
  else
    if Destination = dodDateNavigator then
      UpdateDateNavigatorClones(DateNavigator);
end;

procedure TcxDragEventHelper.UpdateDateNavigatorClones(
  ADateNavigator: TcxSchedulerCustomDateNavigator);
var
  ADelta: TDateTime;
  I: Integer;
begin
  ADelta := ADateNavigator.HitTest.Time - dxDateOf(Controller.DragEvent.{Source.}Start);
  for I := 0 to Clones.Count - 1 do
    with Clones[I] do
    begin
      State := Source.State;
      AllDayEvent := Source.AllDayEvent;
      Duration := Source.Duration;
      MoveTo(Source.Start + ADelta);
      ResourceID := Source.ResourceID;
    end;
end;

procedure TcxDragEventHelper.UpdateDateNavigator(var Accepted: Boolean);
var
  P: TPoint;
begin
  P := Scheduler.ScreenToClient(GetMouseCursorPos);
  with DateNavigator do
  begin
    MousePositionChanged(P.X, P.Y);
    if HitTest.HitAtControl then
      Accepted := HitTest.HitAtTime;
    UpdateDragging;
  end;
end;

procedure TcxDragEventHelper.UpdateViewClones;
begin
  UpdateViewClonesResources;
  UpdateViewClonesTime;
end;

procedure TcxDragEventHelper.UpdateViewClonesResources;
var
  I: Integer;
  AResource: TcxSchedulerStorageResourceItem;
begin
  with HitTest do
  begin
    if HitAtResource then
      AResource := Resource
    else
      AResource := FStartResource;
  end;
  if AResource <> nil then
  begin
    for I := 0 to Clones.Count - 1 do
      with Clones[I] do
        if not VarIsNull(Source.ResourceID) and
          (not Source.Shared or Source.IsSharedWithResource(AResource)) then
          ReplaceResourceID(AResource.ResourceID);
  end;
end;

procedure TcxDragEventHelper.UpdateViewClonesTime;
begin
end;

procedure TcxDragEventHelper.ValidateDestinationTime;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
  AStatus: TcxOccurrenceDateStatus;
begin
  FValidatingDestinationTime := True;
  try
    for I := Clones.Count - 1 downto 0 do
    begin
      AEvent := Clones[I];
      if AEvent.EventType in [etOccurrence, etCustom] then
      begin
        AStatus := AEvent.Pattern.ValidateOccurrenceTimeBounds(AEvent.Source, AEvent.Start, AEvent.Finish);
        if AStatus <> odsValid then
          DeleteClone(AEvent, AStatus);
      end;
    end;
  finally
    FValidatingDestinationTime := False;
  end;
end;

function TcxDragEventHelper.GetClones: TcxSchedulerFilteredEventList;
begin
  Result := Scheduler.EventList.Clones;
end;

function TcxDragEventHelper.GetViewInfo: TcxSchedulerCustomViewViewInfo;
begin
  Result := TcxSchedulerCustomViewViewInfo(Scheduler.CurrentView.ViewInfo);
end;

{ TcxSchedulerEditController }

constructor TcxSchedulerEditController.Create(AOwner: TcxCustomScheduler);
begin
  FOwner := AOwner;
  FEditList := TcxInplaceEditList.Create(Scheduler);
end;

destructor TcxSchedulerEditController.Destroy;
begin
  CloseEdit(False);
  FEditList.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEditController.Activate(
  AEvent: TcxSchedulerControlEvent);
begin
  if InitEdit(AEvent) then
    FEdit.Activate(FEditData);
end;

procedure TcxSchedulerEditController.Activate(
  AEvent: TcxSchedulerControlEvent; const APos: TPoint; AShift: TShiftState);
begin
  if InitEdit(AEvent) then
    FEdit.ActivateByMouse(AShift, APos.X, APos.Y, FEditData);
end;

procedure TcxSchedulerEditController.Activate(
  AEvent: TcxSchedulerControlEvent; Key: Char);
begin
  if InitEdit(AEvent) then
    FEdit.ActivateByKey(Key, FEditData);
end;

procedure TcxSchedulerEditController.CloseEdit(Accepted: Boolean);

  function IsDataRefreshing: Boolean;
  begin
    Result := Scheduler.FLockRefresh > 0;
  end;

var
  AEditedSource: TcxSchedulerEvent;
  AEditedEvent: TcxSchedulerControlEvent;
begin
  if not IsEditing then Exit;
  AEditedSource := Event.Source;
  FIsEditing := False;
  FFocused := False;
  try
    if not (Scheduler.IsDestroying or IsDataRefreshing) then
    begin
      Scheduler.EventList.AfterEditing(Event);
      if Accepted and CanAccept then
      begin
        FEdit.Deactivate;
        UpdateValue;
        AEditedSource := Scheduler.EventList.PostEvent(Event);
        if Scheduler.CanFocus then
          Scheduler.SetFocus;
      end
      else
      begin
        if FIsNewEvent then
        begin
          DeleteEvent(Event);
          FEvent := nil;
          AEditedSource := nil;
        end;
        Scheduler.FullRefresh;
      end;
    end;
  finally
    FEdit.Parent := nil;
    FEdit := nil;
    FreeAndNil(FEditData);
    FIsNewEvent := False;
    if not Scheduler.IsDestroying then
    begin
      AEditedEvent := nil;
      if Assigned(AEditedSource) then
        AEditedEvent := TcxSchedulerControlEvent.Create(AEditedSource);
      try
        Scheduler.DoAfterEditing(AEditedEvent);
        if not IsDataRefreshing then
          Scheduler.CurrentView.Refresh;
      finally
        FreeAndNil(AEditedEvent);
      end;
    end;
    FEvent := nil;
  end;
end;

procedure TcxSchedulerEditController.Init(const AEditDate: TDateTime;
  AResource: TcxSchedulerStorageResourceItem; AIsNewEvent: Boolean = False);
begin
  FEditDate := dxDateOf(AEditDate);
  FEditResource := AResource;
  FIsNewEvent := AIsNewEvent;
end;

procedure TcxSchedulerEditController.DeleteEvent(AEvent: TcxSchedulerControlEvent);
begin
  Scheduler.EventList.DeleteEvent(AEvent);
  if Scheduler.CurrentView <> nil then
    Scheduler.CurrentView.Refresh;
end;

procedure TcxSchedulerEditController.UpdateEdit;
var
  R: TRect;
begin
  if not IsEditing then Exit;
  if GetEditRect(R, FEditDate, FEditResource) then
  begin
    FEdit.BoundsRect := R;
    FEdit.Visible := True;
  end
  else
    FEdit.Visible := False;
  FEdit.EditValue := FEvent.Caption;
end;

procedure TcxSchedulerEditController.UpdateValue;
begin
  if (FEdit <> nil) and  FEdit.EditModified then
  begin
    FEdit.ValidateEdit(True);
    FEvent.Caption := FEdit.EditValue;
  end;
end;

function TcxSchedulerEditController.GetEditRect(var R: TRect;
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem;
  AMakeVisible: Boolean = False): Boolean;
begin
  Scheduler.MakeEventVisible(Event, ADate, AResource);
  R := cxRectOffset(Scheduler.CurrentView.GetEditRectForEvent(Event, ADate, AResource),
    Scheduler.CurrentView.Bounds.TopLeft);
  Result := not cxRectIsEmpty(R);
end;

procedure TcxSchedulerEditController.EditAfterKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if IsKeyForControl(Key, Shift) then
  begin
    Scheduler.SetFocus;
    CloseEdit(True);
  end;
end;

procedure TcxSchedulerEditController.EditExit(Sender: TObject);
begin
  if IsEditing then CloseEdit(True);
end;

procedure TcxSchedulerEditController.EditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if IsKeyForControl(Key, Shift) then
  begin
    CloseEdit(Key <> VK_ESCAPE);
    Scheduler.SetFocus;
  end;
  if Assigned(Scheduler.OnKeyDown) then
    Scheduler.OnKeyDown(Scheduler, Key, Shift);
end;

procedure TcxSchedulerEditController.EditKeyPress(
  Sender: TObject; var Key: Char);
begin
  if Assigned(Scheduler.OnKeyPress) then
    Scheduler.OnKeyPress(Scheduler, Key);
end;

procedure TcxSchedulerEditController.EditKeyUp(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Scheduler.OnKeyUp) then
    Scheduler.OnKeyUp(Scheduler, Key, Shift);
end;

procedure TcxSchedulerEditController.EditPostEditValue(Sender: TObject);
begin
  UpdateValue;
end;

function TcxSchedulerEditController.InitEdit(AEvent: TcxSchedulerControlEvent): Boolean;
var
  R: TRect;
begin
  Result := False;
  if AEvent = nil then Exit;
  if IsEditing then
  begin
    if FEvent = AEvent then
    begin
      UpdateValue;
      UpdateEdit;
    end
    else
      CloseEdit(True);
    Exit;
  end;
  FIsEditing := False;
  FEvent := AEvent;
{  Controller.SelectSingleEvent(AEvent, NullDate, False);     }
  if not (Scheduler.DoBeforeEditing(AEvent, True) and
    GetEditRect(R, FEditDate, FEditResource, True)) then Exit;
  Scheduler.EventList.BeforeEditing(FEvent, True);
  Scheduler.DoGetEventEditProperties(Event, FEditProperties);
  FEdit := FEditList.GetEdit(EditProperties);
  Result :=  FEdit <> nil;
  if not Result then Exit;
  FIsEditing := True;
  PrepareEdit(FEdit);
  if FIsEditing then
    Scheduler.CurrentView.Refresh;
end;

function TcxSchedulerEditController.IsKeyForControl(
  var AKey: Word; Shift: TShiftState): Boolean;
begin
  Result := (AKey = VK_TAB) or (AKey = VK_ESCAPE) or (AKey = VK_RETURN);
end;

procedure TcxSchedulerEditController.PrepareEdit(AEdit: TcxCustomEdit);
const
  VertAlignments: array[Boolean] of TcxEditVertAlignment = (taTopJustify, taVCenter);
var
  R: TRect;
  AVisible: Boolean;
begin
  FFocused := True;
  with FEdit do
  begin
    Visible := False;
    AVisible := GetEditRect(R, FEditDate, FEditResource);
    InternalProperties.Alignment.Vert := VertAlignments[View.GetEditWithSingleLineEditor(Event)];
    Parent := Scheduler;
    BoundsRect := R;
    EditValue := FEvent.Caption;
    OnAfterKeyDown := EditAfterKeyDown;
    OnExit := EditExit;
    OnKeyDown := EditKeyDown;
    OnKeyPress := EditKeyPress;
    OnKeyUp := EditKeyUp;
    OnPostEditValue := EditPostEditValue;
    Style.Init(Scheduler.Styles.GetEventContentParams(Event));
    Visible := AVisible;
  end;
  Scheduler.DoInitEdit(FEdit);
end;

function TcxSchedulerEditController.CanAccept: Boolean;
begin
  Result := FEdit.EditModified or FIsNewEvent;
end;

function TcxSchedulerEditController.GetController: TcxSchedulerViewController;
begin
  Result := View.Controller;
end;

function TcxSchedulerEditController.GetEditVisible: Boolean;
begin
  Result := IsEditing and FEdit.Visible;
end;

function TcxSchedulerEditController.GetView: TcxSchedulerCustomView;
begin
  Result := Scheduler.CurrentView;
end;

procedure TcxSchedulerEditController.SetEditVisible(Value: Boolean);
begin
  if not IsEditing then Exit;
  if Value <> EditVisible then
  begin
    FEdit.Visible := Value;
    UpdateEdit;
  end;
end;

{ TcxSchedulerViewNavigation }

constructor TcxSchedulerViewNavigation.Create(
  AView: TcxSchedulerCustomView);
begin
  FView := AView;
end;

procedure TcxSchedulerViewNavigation.CheckSelection;
begin
  with Scheduler do
    Self.ValidateSelection(FSelStart, FSelFinish, FSelResource);
end;

procedure TcxSchedulerViewNavigation.ReplaceSelParams(
  const ASelStart, ASelFinish: TDateTime);
begin
  ReplaceSelParams(ASelStart, ASelFinish, GetResourceItem);
end;

procedure TcxSchedulerViewNavigation.ReplaceSelParams(
  const ASelStart, ASelFinish: TDateTime; AResource: TcxSchedulerStorageResourceItem);
begin
  Scheduler.ReplaceSelParams(ASelStart, ASelFinish, AResource);
end;

procedure TcxSchedulerViewNavigation.ReplaceSelParams(
  AResource: TcxSchedulerStorageResourceItem);
begin
  Scheduler.ReplaceSelParams(SelStart, SelFinish, AResource);
end;

function TcxSchedulerViewNavigation.ScrollResources(AGoForward: Boolean): Boolean;
const
  ACode: array[Boolean] of TScrollCode = (scLineUp, scLineDown);
var
  APos: Integer;
begin
  with ResourceNavigator do
  begin
    if not AGoForward then
      Result := FirstVisibleResourceIndex > 0
    else
      Result := (ResourcesPerPage <> 0) and ((ResourcesPerPage + FirstVisibleResourceIndex) < VisibleResourceCount);
    if Result then
    begin
      APos := FirstVisibleResourceIndex;
      Scroll(ACode[AGoForward], APos);
    end;
  end;
end;

function TcxSchedulerViewNavigation.ScrollResourcesEx(
  AGoForward: Boolean; var AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  Result := ScrollResources(AGoForward);
  if Result then
  begin
    if not AGoForward then
      AResource := VisibleResources[ResourceNavigator.FirstVisibleResourceIndex]
    else
      with ResourceNavigator do
        AResource := VisibleResources[ResourcesPerPage + FirstVisibleResourceIndex - 1];
  end;
end;

function TcxSchedulerViewNavigation.ScrollResourcesCycled(
  AGoForward: Boolean; var AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  Result := False;
  if AGoForward and (ResourceNavigator.FirstVisibleResourceIndex > 0) then
  begin
    ResourceNavigator.FirstVisibleResourceIndex := 0;
    AResource := VisibleResources[0];
    Result := True;
  end
  else
    if not AGoForward and (ResourceNavigator.FirstVisibleResourceIndex = 0) then
    begin
      AResource := VisibleResources[VisibleResourceCount - 1];
      ResourceNavigator.FirstVisibleResourceIndex :=
        VisibleResourceCount - ResourceNavigator.ResourcesPerPage;
      Result := True;
    end;
end;

procedure TcxSchedulerViewNavigation.SetSelAnchor(
  const Anchor: TDateTime; AShift: TShiftState);
begin
  SetSelAnchor(Anchor, AShift, SelResource);
end;

procedure TcxSchedulerViewNavigation.SetSelAnchor(
  const Anchor: TDateTime; AShift: TShiftState;
  AResource: TcxSchedulerStorageResourceItem);
var
  APrevSelResource: TObject;
  APrevSelStart, APrevSelFinish: TDateTime;

  procedure StoreSelection;
  begin
    with Scheduler do
    begin
      APrevSelResource := SelResource;
      APrevSelStart := SelStart;
      APrevSelFinish := SelFinish;
    end;
  end;

  function NeedUpdateOnMouseAction: Boolean;
  begin
    with Scheduler do
      Result := (ssLeft in AShift) and ((SelResource <> APrevSelResource) or
        (SelStart <> APrevSelStart) or (SelFinish <> APrevSelFinish));
  end;

begin
  StoreSelection;
  if not (ssShift in AShift) or (AResource <> SelResource) then
    Scheduler.ReplaceSelParams(Anchor, Anchor, AResource)
  else
    Scheduler.ReplaceSelParams(Scheduler.FSelStart, Anchor, AResource);
  if NeedUpdateOnMouseAction then
    View.LayoutChanged;
end;

procedure TcxSchedulerViewNavigation.ValidateSelection(var ASelStart,
  ASelFinish: TDateTime; var AResource: TcxSchedulerStorageResourceItem);
begin
end;

procedure TcxSchedulerViewNavigation.DoKeyDown(
  var AKey: Word; AShift: TShiftState);
begin
  Scheduler.BeginUpdate;
  try
    FCurrentAnchor := SelAnchor;
    FCurrentResource := SelResource;
    FShift := AShift;
    KeyDown(AKey, AShift);
    Scheduler.LayoutChanged;
  finally
    Scheduler.EndUpdate;
  end;
end;

function TcxSchedulerViewNavigation.IsKeyNavigation(
  var AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := (AKey = VK_UP) or (AKey = VK_DOWN) or (AKey = VK_Next) or
    (AKey = VK_Prior) or (AKey = VK_Left) or (AKey = VK_Right) or
      (AKey = VK_Home) or (AKey = VK_End);
end;

function TcxSchedulerViewNavigation.IsSingleLine: Boolean;
begin
  Result := dxTimeOf(Abs(Scheduler.FSelStart -
    Scheduler.FSelFinish)) <= View.GetTimeIncrement;
end;

function TcxSchedulerViewNavigation.GetResourceItem: TcxSchedulerStorageResourceItem;
begin
  Result := nil;
end;

procedure TcxSchedulerViewNavigation.KeyDown(var AKey: Word; AShift: TShiftState);
begin
end;

procedure TcxSchedulerViewNavigation.ReplaceDate(
  ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem);
begin
  if ADate >= LastAvailableDate then Exit;
  if AResource = nil then
    AResource := SelResource;
  with Scheduler do
  begin
    ADate := ADate - dxDateOf(FSelFinish);
    ReplaceSelParams(FSelStart + ADate, FSelFinish + ADate, AResource);
  end;
end;

function TcxSchedulerViewNavigation.GetResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := Scheduler.ResourceNavigator;
end;

function TcxSchedulerViewNavigation.GetScheduler: TcxCustomScheduler;
begin
  Result := View.Scheduler;
end;

function TcxSchedulerViewNavigation.GetSelAnchor: TDateTime;
begin
  Result := Scheduler.FSelFinish
end;

function TcxSchedulerViewNavigation.GetSelRealStart: TDateTime;
begin
  Result := Scheduler.FSelStart;
end;

function TcxSchedulerViewNavigation.GetSelFinish: TDateTime;
begin
  Result := Max(Scheduler.FSelFinish, Scheduler.FSelStart);
end;

function TcxSchedulerViewNavigation.GetSelResource: TcxSchedulerStorageResourceItem;
begin
  Result := Scheduler.SelResource;
end;

function TcxSchedulerViewNavigation.GetSelStart: TDateTime;
begin
  Result := Min(Scheduler.FSelFinish, Scheduler.FSelStart);
end;

function TcxSchedulerViewNavigation.GetTimeIncrement: TDateTime;
begin
  Result := View.GetTimeIncrement;
end;

function TcxSchedulerViewNavigation.GetVisibleResource(
  AIndex: Integer): TcxSchedulerStorageResourceItem;
begin
  if Scheduler.StorageValid then
    Result := Scheduler.Storage.Resources.ResourceItems.VisibleResources[AIndex]
  else
    Result := nil;
end;

function TcxSchedulerViewNavigation.GetVisibleResourceCount: Integer;
begin
  if Scheduler.StorageValid then
    Result := Scheduler.Storage.Resources.ResourceItems.VisibleResourceCount
  else
    Result := 0;
end;

{ TcxSchedulerViewController }

constructor TcxSchedulerViewController.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FNavigation := CreateNavigation;
end;

destructor TcxSchedulerViewController.Destroy;
begin
  StopEditShowingTimer;
  FNavigation.Free;
  inherited Destroy;
end;

function TcxSchedulerViewController.CanCreateEventUsingDialog: Boolean;
begin
  with Scheduler.EventOperations do
    Result := Scheduler.StorageActive and CanShowEventDialog and Creating and
      DialogEditing and not GetResourceReadOnly;
end;

function TcxSchedulerViewController.CanCreateEventUsingInplaceEdit: Boolean;
begin
  with Scheduler.EventOperations do
    Result := Scheduler.StorageActive and Creating and InplaceEditing and
      not GetResourceReadOnly and IsCaptionAvailable and (Scheduler.SelectedEventCount = 0);
end;

function TcxSchedulerViewController.CanEditEvent(
  AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean;
begin
  with Scheduler.EventOperations do
  begin
    if AInplace then
      Result := InplaceEditing and IsCaptionAvailable and View.IsInplaceEditingEnabled
    else
      Result := DialogEditing and DialogShowing;
  end;
  Result := Result and (AEvent <> nil);
  if AInplace and Result then
    Result :=  not AEvent.ReadOnly;
end;

function TcxSchedulerViewController.CanShowEventDialog: Boolean;
begin
  Result := Scheduler.EventOperations.DialogShowing and
    Assigned(cxEventEditorClass);
end;

procedure TcxSchedulerViewController.DeleteSelectedEvents;
begin
  Scheduler.InternalDeleteSelectedEvents(False, True);
end;

function TcxSchedulerViewController.IsEventEditing(
  AEvent: TcxSchedulerControlEvent; AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  Result := IsEditing and (EditController.Event = AEvent) and
    (EditController.FEditResource = AResource);
end;

procedure TcxSchedulerViewController.SelectSingleEvent(
  AEvent: TcxSchedulerControlEvent; ADate: TDateTime; AMakeVisible: Boolean = True);
begin
  if AMakeVisible then View.MakeEventVisible(AEvent, ADate, nil);
  Scheduler.EventList.Selection.Add(AEvent, []);
end;

procedure TcxSchedulerViewController.BeforeMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited BeforeMouseDown(Button, Shift, X, Y);
  StopEditShowingTimer;
  Scheduler.HintController.Hide;
  if not CanProcessMouseMove then Exit;
  HitTest.Recalculate;
  FDragKind := HitTest.GetDragKind;
  if DragKind <> edkNone then
  begin
    CloseInplaceEdit;
    FDragEvent := HitTest.Event;
    if FDragEvent <> nil then
    begin
      if FDragEvent.IsClone and (FDragEvent.Source <> nil) then
        FDragEvent := TcxSchedulerControlEvent(FDragEvent.Source);
      FStartDragHitTime  := HitTest.Time;
      FStartDragFlags    := HitTest.Flags;
      FStartDragResource := HitTest.Resource;
    end;
  end;
end;

procedure TcxSchedulerViewController.CancelScroll;
begin
end;

function TcxSchedulerViewController.CanDrag(X, Y: Integer): Boolean;
begin
  Result := (DragKind in [edkEventDragRect, edkMoveEvent]) and HitTest.HitAtTime and (DragEvent <> nil);
  if Result then
  begin
    if Scheduler.EventList.Selection.IsSelected(DragEvent) and not Scheduler.HasConflict(True) and
      not DragEvent.OpenedInEditor then
      Result := Scheduler.DoBeforeDragEvent(DragEvent, X, Y)
    else
    begin
      Result := False;
      SendMessage(Scheduler.Handle, WM_CANCELMODE, 0, 0);
    end;
  end;
end;

procedure TcxSchedulerViewController.CheckOpenInplaceEditorOnMouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AEvent: TcxSchedulerControlEvent;
begin
  if (HitTest.HitAtEvent and (HitTest.Event = FBeforeFocusedEvent)) and (Button = mbLeft) and
  ([ssShift, ssCtrl] * Shift = []) and not (DragKind in [edkResizeStart, edkResizeEnd]) then
  begin
    AEvent := HitTest.Event;
    if cxRectPtIn(View.GetEditRectForEvent(AEvent, dxDateOf(HitTest.Time),
      HitTest.Resource), X, Y) and CanEditEvent(AEvent, True) then
    begin
      EditController.Init(HitTest.Time, HitTest.Resource);
      StartEditShowingTimer(AEvent);
    end;
  end;
  FBeforeFocusedEvent := nil;
end;

procedure TcxSchedulerViewController.CheckScrolling(
  const APos: TPoint);
begin
end;

procedure TcxSchedulerViewController.CheckScrollOnDragOver(
  const P: TPoint; State: TDragState);
begin
  if State <> dsDragLeave then
    CheckScrolling(P)
  else
    CancelScroll;
end;

procedure TcxSchedulerViewController.CheckUpdateEventBounds;
var
  ASizingHelper: TcxEventSizingHelper;
begin
  if (Scheduler.DragAndDropState = ddsInProcess) and
     (Scheduler.DragAndDropObject is TcxSchedulerDragAndDropObject) then
  begin
    ASizingHelper := TcxSchedulerDragAndDropObject(Scheduler.DragAndDropObject).SizingHelper;
    if ASizingHelper <> nil then
      ASizingHelper.UpdateEventBounds;
  end;
end;

procedure TcxSchedulerViewController.CloseInplaceEdit;
begin
  if EditController.IsEditing then
  begin
    EditController.CloseEdit(True);
    HitTest.Recalculate;
  end;
end;

function TcxSchedulerViewController.CreateResizeEventHelper: TcxEventSizingHelper;
begin
  Result := TcxEventSizingHelper.Create(Scheduler);
end;

function TcxSchedulerViewController.ConsiderHiddenEvents: Boolean;
begin
  Result := True;
end;

function TcxSchedulerViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxDragEventHelper.Create(Scheduler);
end;

function TcxSchedulerViewController.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerViewNavigation.Create(View);
end;

procedure TcxSchedulerViewController.DoSchedulerDragOver(const P: TPoint;
  AState: TDragState; var AAccept: Boolean);
begin
  if Assigned(Scheduler.OnDragOver) then
    Scheduler.OnDragOver(Scheduler, DragEvent, P.X, P.Y, AState, AAccept);
end;

procedure TcxSchedulerViewController.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  P: TPoint;
begin
  Owner.MousePositionChanged(X, Y);
  P := cxPoint(X, Y);
  if Assigned(DragEventHelper) and DragEventHelper.CanProcessDragOver then
  begin
    if State = dsDragMove then
      CheckNavigatorScrollArea(P);
    if Accept then
      DragEventHelper.DragOver(P, State, Accept);
    DragEventHelper.UpdateHelperState(Accept);
  end;
  CheckScrollOnDragOver(P, State);
end;

procedure TcxSchedulerViewController.EndDrag(Target: TObject; X, Y: Integer);
var
  APos: TPoint;
begin
  DoneNavigatorScrollArea;
  if Assigned(DragEventHelper) then
  try
    APos := Scheduler.ScreenToClient(GetMouseCursorPos);
    Owner.MousePositionChanged(APos.X, APos.Y);
    FDragEventHelper.FTarget := Target;
    FDragEventHelper.EndDrag(not Scheduler.IsDragCanceled);
  finally
    FreeAndNil(FDragEventHelper);
  end;
end;

function TcxSchedulerViewController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TcxSchedulerDragAndDropObject;
end;

function TcxSchedulerViewController.GetResourceReadOnly: Boolean;
begin
  with Scheduler do
    Result :=  not StorageValid or
      ((SelResource <> nil) and not SelResource.CanModify) or
      ((Storage.ResourceCount > 0) and ((SelResource = nil) and not View.GetShowEventsWithoutResource));
end;

function TcxSchedulerViewController.IsCaptionAvailable: Boolean;
begin
  Result := Scheduler.StorageValid and Scheduler.Storage.IsCaptionAvailable;
end;

function TcxSchedulerViewController.IsCopyDragDrop: Boolean;
begin
  Result := Scheduler.IsCopyDragDrop;
end;

function TcxSchedulerViewController.IsDragOperation: Boolean;
begin
  Result := Scheduler.Dragging or (Scheduler.DragAndDropState <> ddsNone);
end;

procedure TcxSchedulerViewController.KeyDown(
  var Key: Word; Shift: TShiftState);
begin
  if View.Active and not EditController.IsEditing then
  begin
    Scheduler.HintController.HideHint;
    if Navigation.IsKeyNavigation(Key, Shift) then
    begin
      Scheduler.BeginUpdate;
      try
        UnselectEvents;
        Navigation.DoKeyDown(Key, Shift);
      finally
        Scheduler.EndUpdate;
        Scheduler.Update;
      end;
    end;
    case Key of
      VK_ESCAPE:
      begin
        if Scheduler.CaptureController = Self then
          UnselectEvents;
      end;
      VK_TAB:
        SelectNextEvent(not (ssShift in Shift));
      VK_DELETE, VK_INSERT:
        if not Scheduler.ClipboardController.KeyDown(Key, Shift) and (Key = VK_DELETE) then
          DeleteSelectedEvents;
      VK_SHIFT:
        FStartSelAnchor := Scheduler.FSelStart;
      VK_PROCESSKEY:
        if CanCreateEventUsingInplaceEdit then
          Scheduler.CreateEventUsingInplaceEdit;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TcxSchedulerViewController.KeyPress(var Key: Char);
var
  AEvent: TcxSchedulerControlEvent;
begin
  with Scheduler do
  begin
    if IsLocked or not StorageActive or GetResourceReadOnly or
      Scheduler.ClipboardController.KeyPress(Key) then Exit;
    if (Key = #13) or IsEditStartChar(Key) then
    begin
      if Key = #13 then
      begin
        if SelectedEventCount > 0 then
        begin
          AEvent := SelectedEvents[SelectedEventCount - 1];
          if CanShowEventDialog then
            EditEventUsingDialog(AEvent);
        end
        else
        begin
          if CanCreateEventUsingInplaceEdit then
            CreateEventUsingInplaceEdit
          else
            if CanCreateEventUsingDialog then
              CreateEventUsingDialog;
        end;
      end
      else
        if CanCreateEventUsingInplaceEdit then
          DoCreateEventUsingInplaceEdit(Key);
    end;
  end;
end;

procedure TcxSchedulerViewController.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AEvent: TcxSchedulerControlEvent;
  ANeedFocusingEvent: Boolean;
begin
  CloseInplaceEdit;
  if HitTest.HitAtEvent then
  begin
    AEvent := HitTest.Event;
    ANeedFocusingEvent := not (AEvent.OpenedInEditor or
      (Shift = [ssLeft, ssDouble]));
    if ANeedFocusingEvent then
      FBeforeFocusedEvent := AEvent;
    UpdateEventSelection(AEvent, Button, Shift);
    if (Shift = [ssLeft, ssDouble]) then
      ShowEventEditor(AEvent)
  end
  else
    if Button = mbLeft then
    begin
      UnselectEvents;
      if HitTest.HitAtTime then
      begin
        FStartSelAnchor := HitTest.Time;
        if HitTest.Resource = nil then
          Navigation.SetSelAnchor(HitTest.Time, Shift)
        else
          Navigation.SetSelAnchor(HitTest.Time, Shift, HitTest.Resource);
        if (Shift = [ssLeft, ssDouble]) and CanCreateEventUsingDialog then
          Scheduler.CreateEventUsingDialog;
      end;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerViewController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if CanProcessMouseMove and (ssLeft in Shift) and not EditController.IsEditing then
  begin
    if (DragKind = edkNone) and HitTest.HitAtTime then
      Navigation.SetSelAnchor(HitTest.Time, [ssShift] + Shift);
  end;
end;

procedure TcxSchedulerViewController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  CheckOpenInplaceEditorOnMouseUp(Button, Shift, X, Y);
  FStartSelAnchor := NullDate;
  FDragKind := edkNone;
end;

procedure TcxSchedulerViewController.RecreateNavigation;
begin
  FNavigation.Free;
  FNavigation := CreateNavigation;
  FNavigation.CheckSelection;
end;

procedure TcxSchedulerViewController.SelectNextEvent(AForward: Boolean);

  procedure GetNextSelectedIndex(var AIndex: Integer);
  begin
    if AForward then
      Inc(AIndex)
    else
      Dec(AIndex);
  end;

var
  I, ASelectedIndex: Integer;
begin
  if Scheduler.EventList.Count = 0 then
    Exit;
  with Scheduler do
  begin
    ASelectedIndex := -1;
    for I := 0 to EventList.Count - 1 do
      if EventList[I].Selected then
        ASelectedIndex := I;
    if (ASelectedIndex >= 0) and (TabOrdersList.Count > 0) then
      ASelectedIndex := TabOrdersList.IndexOf(EventList[ASelectedIndex]);
    GetNextSelectedIndex(ASelectedIndex);
    if ASelectedIndex < -1 then
      ASelectedIndex := TabOrdersList.Count - 1;
    while (ASelectedIndex < TabOrdersList.Count) and (ASelectedIndex >= 0) and
        not CurrentView.GetEventVisibility(TcxSchedulerControlEvent(TabOrdersList[ASelectedIndex])) do
      GetNextSelectedIndex(ASelectedIndex);
    if (ASelectedIndex < EventList.Count) and (ASelectedIndex >= 0) then
      SelectSingleEvent(TcxSchedulerControlEvent(TabOrdersList[ASelectedIndex]), NullDate)
    else
      UnselectEvents;
    CurrentView.Refresh;
  end;
end;

procedure TcxSchedulerViewController.StartDrag(var DragObject: TDragObject);
begin
  InitNavigatorScrollArea;
  FDragEventHelper := CreateDragEventHelper;
  if FDragEventHelper <> nil then
    FDragEventHelper.FDragObject := DragObject;
end;

function TcxSchedulerViewController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := (DragKind in [edkResizeStart, edkResizeEnd]) and not DragEvent.OpenedInEditor and
    Scheduler.DoBeforeSizingEvent(DragEvent, P.X, P.Y);
end;

procedure TcxSchedulerViewController.SyncEventSelection(
  AEvent: TcxSchedulerControlEvent);
var
  AResource: TcxSchedulerStorageResourceItem;
begin
  if HitTest.HitAtResource and AEvent.IsResourceEvent(HitTest.Resource, True) then
    AResource := HitTest.Resource
  else
    AResource := AEvent.GetResourceItem;
  if View.GetTimeIncrement = 1 then
    Scheduler.ReplaceSelParams(dxDateOf(AEvent.Start),
      dxDateOf(AEvent.Finish) - Byte(AEvent.AllDayEvent or
      ((dxTimeOf(AEvent.Finish) = 0) and (AEvent.Start <> AEvent.Finish))) * View.GetTimeIncrement, AResource)
  else
  begin
    if AEvent.Start <> AEvent.Finish then
      Scheduler.ReplaceSelParams(AEvent.Start,
        AEvent.Finish - View.GetTimeIncrement, AResource)
    else
      Scheduler.ReplaceSelParams(AEvent.Start, AEvent.Finish, AResource);
  end;
end;

procedure TcxSchedulerViewController.UnselectEvents;
begin
  Scheduler.UnselectEvents;
end;

procedure TcxSchedulerViewController.UpdateEventSelection(
  AEvent: TcxSchedulerControlEvent; Button: TMouseButton; Shift: TShiftState);
begin
  if AEvent = nil then Exit;
  with Scheduler.EventList.Selection do
    if (Button = mbRight) and (Shift * [ssCtrl, ssShift] <> [])then
    begin
      if not IsSelected(AEvent) then
        Add(AEvent, [ssCtrl]);
    end
    else
      Add(AEvent, Shift);
  SyncEventSelection(AEvent);
  View.Refresh;
end;

procedure TcxSchedulerViewController.CheckNavigatorScrollArea(const APoint: TPoint);
var
  ACode: TScrollCode;
begin
  if PtInArea(FUpScrollArea, APoint, False) then
    ACode := scLineDown
  else
    if PtInArea(FDownScrollArea, APoint, True) then
      ACode := scLineUp
    else
      ACode := scEndScroll;
  NavigatorTimer.Tag := Byte(ACode);
  if not NavigatorTimer.Enabled then
    NavigatorTimer.Interval := cxNavigatorStartTimer;
  NavigatorTimer.Enabled := ACode <> scEndScroll;
end;

procedure TcxSchedulerViewController.DoneNavigatorScrollArea;
begin
  NavigatorTimer.OnTimer := nil;
  FUpScrollArea := cxNullRect;
  FDownScrollArea := cxNullRect;
end;

procedure TcxSchedulerViewController.InitNavigatorScrollArea;
begin
  if Scheduler.ResourceNavigator.NeedScrollBar then
  begin
    FUpScrollArea := TcxSchedulerCustomView(Owner).GetViewContentRect;
    FDownScrollArea := FUpScrollArea;
    if Scheduler.ResourceNavigator.ScrollBarKind = sbHorizontal then
    begin
      FUpScrollArea.Left := FUpScrollArea.Right - cxScrollZoneSize;
      FDownScrollArea.Right := FDownScrollArea.Left + cxScrollZoneSize;
    end
    else
    begin
      FDownScrollArea.Bottom := FDownScrollArea.Top + cxScrollZoneSize;
      FUpScrollArea.Top := FUpScrollArea.Bottom - cxScrollZoneSize;
    end;
    NavigatorTimer.OnTimer := NavigatorTimerHandler;
  end
end;

procedure TcxSchedulerViewController.NavigatorTimerHandler(Sender: TObject);
var
  APos: Integer;
  APoint: TPoint;
  ACode: TScrollCode;
begin
  APoint := cxPointOffset(Scheduler.ScreenToClient(GetMouseCursorPos),
    -View.Left, -View.Top);
  if not PtInRect(View.ClientRect, APoint) then Exit;
  ACode := TScrollCode(NavigatorTimer.Tag);
  APos := Scheduler.ResourceNavigator.FirstVisibleResourceIndex;
  Scheduler.ResourceNavigator.Scroll(ACode, APos);
  if DragEventHelper <> nil then
    DragEventHelper.Update;
  Scheduler.Update;
end;

function TcxSchedulerViewController.PtInArea(
  const ARect: TRect; const P: TPoint; IsUpArea: Boolean): Boolean;
var
  AHorz: Boolean;
begin
  if not PtInRect(View.ClientRect, P) then
  begin
    Result := False;
    Exit;
  end;
  Result := PtInRect(ARect, P);
  if not Result then
  begin
    AHorz := Scheduler.ResourceNavigator.ScrollBarKind = sbHorizontal;
    if IsUpArea then
      Result := (AHorz and (P.X < ARect.Left)) or
        (not AHorz and (P.Y < ARect.Top))
    else
      Result := (AHorz and (P.X > ARect.Right)) or
        (not AHorz and (P.Y > ARect.Bottom))
  end;
end;

procedure TcxSchedulerViewController.EditShowingTimerHandler(Sender: TObject);
begin
  StopEditShowingTimer;
  EditController.Activate(FEditShowingTimerItem);
end;

function TcxSchedulerViewController.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := View.HitTest;
end;

function TcxSchedulerViewController.GetEditController: TcxSchedulerEditController;
begin
  Result := Scheduler.EditController;
end;

function TcxSchedulerViewController.GetIsEditing: Boolean;
begin
  Result := EditController.IsEditing;
end;

function TcxSchedulerViewController.GetNavigatorTimer: TTimer;
begin
  Result := Scheduler.ResourceNavigator.Timer;
end;

function TcxSchedulerViewController.GetScheduler: TcxCustomScheduler;
begin
  Result := Owner.Scheduler;
end;

function TcxSchedulerViewController.GetView: TcxSchedulerCustomView;
begin
  Result := TcxSchedulerCustomView(inherited Owner);
end;

procedure TcxSchedulerViewController.ShowEventEditor(
  AEvent: TcxSchedulerControlEvent);
begin
  StopEditShowingTimer;
  if CanShowEventDialog then
    Scheduler.EditEventUsingDialog(AEvent);
end;

procedure TcxSchedulerViewController.StartEditShowingTimer(
  AEvent: TcxSchedulerControlEvent);
begin
  StopEditShowingTimer;
  FEditShowingTimerItem := AEvent;
  FEditShowingTimer := TTimer.Create(nil);
  FEditShowingTimer.Interval := GetDblClickInterval;
  FEditShowingTimer.OnTimer := EditShowingTimerHandler;
end;

procedure TcxSchedulerViewController.StopEditShowingTimer;
begin
  FreeAndNil(FEditShowingTimer);
end;

{ TcxEventSizingHelper }

procedure TcxEventSizingHelper.BeginDrag;
begin
  inherited BeginDrag;
  if IsValidTime then
  begin
    UpdateEventBounds;
    CalculateConflicts;
  end;
end;

procedure TcxEventSizingHelper.DragOver(const P: TPoint;
  State: TDragState; var Accepted: Boolean);
begin
  Accepted := IsValidTime;
  if HasChangedState and Accepted then
  begin
    UpdateEventBounds;
    CalculateConflicts;
  end;
  Controller.CheckScrolling(P);
end;

procedure TcxEventSizingHelper.EndDrag(Accepted: Boolean);
begin
  if Accepted and not HasConflicts then
  begin
    SetSelection;
    Events.Storage.BeginUpdate;
    try
      Events.PostEvent(Event);
    finally
      Events.Storage.EndUpdate;
    end;
  end
  else
  begin
    Event.Start := Event.Source.Start;
    Event.Finish := Event.Source.Finish;
    Scheduler.FullRefresh;
  end;
end;

function TcxEventSizingHelper.GetDragCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crNoDrop, crSchedulerHorzResize);
begin
  Result := Cursors[not HasConflicts];
end;

function TcxEventSizingHelper.IsValidTime: Boolean;
begin
  with HitTest do
    Result := HitAtTime and (not HitAtResource or (FStartResource = Resource));
end;

procedure TcxEventSizingHelper.CalcAllDayEvent;
begin
end;

function TcxEventSizingHelper.CanApplyBounds(const AStart, AFinish: TDateTime): Boolean;
begin
  Result := True;
  if Event.EventType in [etCustom, etOccurrence] then
    Result := Event.Pattern.ValidateOccurrenceTimeBounds(Event, AStart, AFinish) = odsValid;
end;

function TcxEventSizingHelper.GetFinishTime: TDateTime;
begin
  Result := HitTest.Time;
end;

function TcxEventSizingHelper.GetStartTime: TDateTime;
begin
  Result := HitTest.Time;
end;

procedure TcxEventSizingHelper.UpdateEventBounds;
var
  AValue, ASaveStart, ASaveFinish: TDateTime;
  ASaveAllDay: Boolean;

  procedure Save;
  begin
    ASaveStart  := Event.Start;
    ASaveFinish := Event.Finish;
    ASaveAllDay := Event.AllDayEvent;
  end;

  procedure Reset;
  begin
    Event.AllDayEvent := ASaveAllDay;
    Event.MoveTo(ASaveStart);
    Event.Finish := ASaveFinish;
  end;

begin
  Save;
  CalcAllDayEvent;
  case Controller.DragKind of
    edkResizeStart:
      begin
        AValue := GetStartTime;
        if AValue > Event.Finish - Ord(Event.AllDayEvent) then
          AValue := Event.Finish - Ord(Event.AllDayEvent);
        if CanApplyBounds(AValue, Event.Finish) then
          Event.Start := AValue
        else
          Reset;
      end;
    edkResizeEnd:
      begin
        AValue := GetFinishTime;
        if AValue < Event.Start then
          AValue := Event.Start;
        if Event.AllDayEvent then
          AValue := AValue + 1;
        if CanApplyBounds(Event.Start, AValue) then
          Event.Finish := AValue
        else
          Reset;
      end;
  end;
  RefreshCurrentView;
end;

function TcxEventSizingHelper.GetEvent: TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerViewController(Controller).DragEvent;
end;

function TcxEventSizingHelper.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := TcxSchedulerViewHitTest(inherited HitTest);
end;

{ TcxSchedulerDragAndDropObject }

constructor TcxSchedulerDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FScheduler := AControl as TcxCustomScheduler;
  Controller.DragAndDropObject := Self;
end;

destructor TcxSchedulerDragAndDropObject.Destroy;
begin
  Controller.DragAndDropObject := nil;
  inherited Destroy;
end;

procedure TcxSchedulerDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  if Controller.FDragEvent = nil then
    raise EAbort.Create('');
  FSizingHelper := Controller.CreateResizeEventHelper;
end;

procedure TcxSchedulerDragAndDropObject.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
var
  AP: TPoint;
begin
  if Assigned(FSizingHelper) then
  begin
    AP := P;
    Controller.Owner.MousePositionChanged(AP.X, AP.Y);
    HitTest.HitPoint := AP;
    SizingHelper.DragOver(AP, dsDragMove, Accepted);
    SizingHelper.UpdateHelperState(Accepted);
  end;
  inherited DragAndDrop(P, Accepted);
end;

procedure TcxSchedulerDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  AP: TPoint;
begin
  if Assigned(SizingHelper) then
  try
    AP := CurMousePos;
    Controller.Owner.MousePositionChanged(AP.X, AP.Y);
    HitTest.HitPoint := AP;
    Scheduler.DoAfterSizingEvent(SizingHelper.Event, AP.X, AP.Y, Accepted);
    SizingHelper.EndDrag(Accepted);
  finally
    FreeAndNil(FSizingHelper);
  end;
  inherited EndDragAndDrop(Accepted);
end;

function TcxSchedulerDragAndDropObject.GetController: TcxSchedulerViewController;
begin
  Result := FScheduler.CurrentView.Controller;
end;

function TcxSchedulerDragAndDropObject.GetDragAndDropCursor(
  Accepted: Boolean): TCursor;
begin
  if Assigned(FSizingHelper) then
    Result := SizingHelper.GetDragCursor(Accepted)
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TcxSchedulerDragAndDropObject.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := FScheduler.CurrentView.HitTest;
end;

{ TcxSchedulerHintController }

constructor TcxSchedulerHintController.Create(AOwner: TcxCustomScheduler);
begin
  FOwner := AOwner;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerHandler;
  FHintWindow := cxGetHintWindowClass.Create(nil);
  FEvent := nil;
  FModernHint := TcxSchedulerModernStyleHintForm.Create(Scheduler);
  (FModernHint as TcxSchedulerModernStyleHintForm).OnClosed := OnModernStyleHintHide;
end;

destructor TcxSchedulerHintController.Destroy;
begin
  Hide;
  FreeAndNil(FTimer);
  FreeAndNil(FHintWindow);
  if (FModernHint as TcxSchedulerModernStyleHintForm).Visible then
    (FModernHint as TcxSchedulerModernStyleHintForm).Close;
  FModernHint.Free;
  inherited Destroy;
end;

procedure TcxSchedulerHintController.Activate(const AHintRect: TRect;
  const AHintText: string; AImmediateHint: Boolean = False; AAutoHide: Boolean = True);
begin
  Hide;
  FHintText := AHintText;
  FHintRect := AHintRect;
  FAutoHide := AAutoHide;
  FEvent := nil;
  if AImmediateHint then
    ShowHint
  else
    StartShowHintTimer;
end;

procedure TcxSchedulerHintController.Activate(AEvent: TcxSchedulerControlEvent; const AHintRect: TRect;
  AImmediateHint: Boolean = False; AAutoHide: Boolean = False);
begin
  Hide;
  FEvent := AEvent;
  FHintRect := AHintRect;
  FAutoHide := AAutoHide;
  if AImmediateHint then
    ShowHint
  else
    StartShowHintTimer;
end;

function TcxSchedulerHintController.CalcHintRect(AMaxWidth: Integer;
  const AHintText: string; AFlags: Integer): TRect;
begin
  CheckHintClass;
  FHintFlags := AFlags;
  Result := FHintWindow.CalcHintRect(AMaxWidth, AHintText, nil);
end;

procedure TcxSchedulerHintController.Hide;
begin
  HideHint;
end;

procedure TcxSchedulerHintController.Reset;
begin
  Hide;
end;

procedure TcxSchedulerHintController.MouseLeave;
begin
  Scheduler.MouseLeave(Scheduler);
end;

function TcxSchedulerHintController.CanShowHint: Boolean;
begin
  Result := not FLockHint and Application.Active and Scheduler.OptionsView.ShowHints and
    (Scheduler.DragAndDropState = ddsNone);
end;

procedure TcxSchedulerHintController.CheckHintClass;
begin
  if FHintWindow.ClassType <> cxGetHintWindowClass then
  begin
    FHintWindow.Free;
    FHintWindow := cxGetHintWindowClass.Create(nil);
  end;
end;

function TcxSchedulerHintController.GetSchedulerViewStyle: TcxSchedulerViewStyle;
begin
  Result := Scheduler.OptionsView.Style;
end;

procedure TcxSchedulerHintController.HideHint;
begin
  StopTimer;
  if not Showing then Exit;
  EndMouseTracking(Self);
  FHintWindow.Hide;
  ShowWindow(FHintWindow.Handle, SW_HIDE); //MUST USE ShowWindow - WIN32 BUG
  (FModernHint as TcxSchedulerModernStyleHintForm).CloseUp;
  FShowing := False;
end;

procedure TcxSchedulerHintController.OnModernStyleHintHide(Sender: TObject);
begin
  FEvent := nil;
  FreeAndNil(FModernHintHelperContainer);
  FreeAndNil(FModernHintInfo);
end;

procedure TcxSchedulerHintController.ShowHint;

  function InitializeModernHintPopup(AEventCell: TcxSchedulerEventCellViewInfo): Boolean;
  var
    R: TRect;
    AIsTooLong: Boolean;
    APos: TPoint;
    AContainer: TcxSchedulerEventModernInfoContainer;
    AModernHint: TcxSchedulerModernStyleHintForm;
  begin
    FModernHintInfo := Scheduler.GetEventModernStyleHintInfo(AEventCell.Event);
    Result := FModernHintInfo.Visible;
    if not Result then
    begin
      FModernHintInfo.Free;
      Exit;
    end;
    FModernHintHelperContainer := cxSchedulerEventModernInfoContainerClass.Create(Scheduler);
    AContainer := FModernHintHelperContainer as TcxSchedulerEventModernInfoContainer;
    AContainer.BiDiMode := Scheduler.BiDiMode;
    AContainer.Scaled := False;
    AContainer.ScaleForPPI(Scheduler.ScaleFactor.TargetDPI);
    AContainer.Initialize(AEventCell, FModernHintInfo);

    R := AEventCell.Bounds;
    R.Left := Min(R.Left, TcxSchedulerEventCellViewInfoAccess(AEventCell).TimeLineRect.Left);
    cxRectIntersect(R, R, AEventCell.ClipRect);
    R := cxRectOffset(R, Scheduler.CurrentView.Bounds.TopLeft);
    R := cxRectInflate(R, Scheduler.ScaleFactor.Apply(4), Scheduler.ScaleFactor.Apply(4));
    APos := Scheduler.ScreenToClient(GetMouseCursorPos);
    AIsTooLong := (R.Right - APos.X) > AContainer.lcMain.Width;

    AModernHint := FModernHint as TcxSchedulerModernStyleHintForm;
    AModernHint.BiDiMode := Scheduler.BiDiMode;
    AModernHint.UpdateScaleFactor;
    AModernHint.OwnerParent := Scheduler;
    AModernHint.Style.LookAndFeel := Scheduler.LookAndFeel;
    AModernHint.Style.BorderColor := clDefault;
    AModernHint.Style.Color := clDefault;
    AModernHint.PopupControl := AContainer.lcMain;
    AModernHint.FadeAnimation := True;
    AModernHint.MoveAnimation := True;
    AModernHint.ModalMode := False;
    AModernHint.AnimationTime[atShow] := 300;
    AModernHint.AnimationTime[atHide] := 100;
    AModernHint.FShowWithoutActivation := True;

    if AIsTooLong or
      (Scheduler.ClientToScreen(R.TopLeft).X - AModernHint.PopupControl.Width < GetDesktopWorkArea(Scheduler.ClientToScreen(R.TopLeft)).Left) and
      (Scheduler.ClientToScreen(R.BottomRight).X + AModernHint.PopupControl.Width > GetDesktopWorkArea(Scheduler.ClientToScreen(R.BottomRight)).Right) then
    begin
      AModernHint.Direction := pdVertical;
      AModernHint.AlignVert := pavTop;
      AModernHint.AlignHorz := pahCenter;
      R.Left := APos.X - 10;
      R.Right := APos.X + 10;
    end
    else
    begin
      AModernHint.Direction := pdHorizontal;
      AModernHint.AlignVert := pavCenter;
      AModernHint.AlignHorz := pahRight;
    end;
    AModernHint.OwnerBounds := R;
  end;

var
  AHintInfo: THintInfo;
  AModernEvent: TcxSchedulerEventCellViewInfo;
begin
  HideHint;
  if not CanShowHint or
    ((SchedulerViewStyle = svsModern) and (FEvent <> nil) and
      not PtInRect(cxRectOffset(FHintRect, Scheduler.CurrentView.Bounds.TopLeft), Scheduler.ScreenToClient(GetMouseCursorPos))) then
    Exit;
  AModernEvent := nil;
  if (SchedulerViewStyle = svsModern) and (FEvent <> nil) then
    AModernEvent := (Scheduler.CurrentView.HitTest as TcxSchedulerCustomResourceViewHitTest).EventCell;
  if AModernEvent = nil then
  begin
    CheckHintClass;
    FHintWindow.Color := clInfoBk;
    FHintWindow.BiDiMode := Scheduler.BiDiMode;
    with GetMouseCursorPos do
    begin
      if FAutoHide then
      begin
        FHintRect := cxRectOffset(FHintRect, X, Y);
        if FHintFlags and cxAlignRight = cxAlignRight then
          FHintRect := cxRectSetRight(FHintRect, X);
        if FHintFlags and cxAlignBottom = cxAlignBottom then
          FHintRect := cxRectSetTop(FHintRect, Y + cxGetCursorSize.cy);
      end;
    end;
    if FHintWindow.UseRightToLeftAlignment then
      OffsetRect(FHintRect, -cxRectWidth(FHintRect), 0);
    if cxProcessControlHintInfo(AHintInfo, FHintWindow, Scheduler, FHintText, FHintRect) then
    begin
      FHintText := AHintInfo.HintStr;
      cxActivateHint(FHintWindow, FHintRect, FHintText);
      BeginMouseTracking(Scheduler, Scheduler.ClientBounds, Self);
      FShowing := True;
    end;
  end
  else
  if InitializeModernHintPopup(AModernEvent) then
  begin
    (FModernHint as TcxSchedulerModernStyleHintForm).Popup(nil);
    FShowing := True;
  end;
  if FShowing and FAutoHide then
    StartHideHintTimer;
end;

procedure TcxSchedulerHintController.StartHideHintTimer;
begin
  FTimer.Tag := 0;
  FTimer.Interval := Application.HintHidePause;
  FTimer.Enabled := True;
end;

procedure TcxSchedulerHintController.StartShowHintTimer;
begin
  FTimer.Tag := 1;
  if SchedulerViewStyle = svsModern then
    FTimer.Interval := Max(Application.HintPause, 300)
  else
    FTimer.Interval := Max(Application.HintShortPause, 1);
  FTimer.Enabled := True;
end;

procedure TcxSchedulerHintController.StopTimer;
begin
  FTimer.Enabled := False;
end;

procedure TcxSchedulerHintController.TimerHandler(Sender: TObject);
begin
  if FTimer.Tag = 0 then
    HideHint
  else
    ShowHint;
end;

{ TcxSchedulerControlBox }

constructor TcxSchedulerControlBox.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FContainer := CreateWndContainerControl;
end;

procedure TcxSchedulerControlBox.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerControlBox then
    with TcxSchedulerControlBox(Source) do
    begin
      Self.Control := Control;
      Self.Visible := Visible;
    end
    else inherited Assign(Source);
end;

function TcxSchedulerControlBox.HasAsParent(
  AValue: TControl): Boolean;
var
  AParent: TWinControl;
begin
  Result := False;
  AParent := Scheduler;
  while not Result and (AParent <> nil) do
  begin
    Result := AValue = AParent;
    AParent := AParent.Parent;
  end;
end;

procedure TcxSchedulerControlBox.VisibleChanged;
begin
  FContainer.Visible := Visible;
  inherited VisibleChanged;
end;

procedure TcxSchedulerControlBox.DoLayoutChanged;
begin
  FViewParams := Styles.GetBackgroundParams;
  inherited DoLayoutChanged;
end;

procedure TcxSchedulerControlBox.GetProperties(AProperties: TStrings);
begin
  if Scheduler.DateNavigator.Visible then Exit;
  AProperties.Add('DNWidth');
end;

procedure TcxSchedulerControlBox.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if Scheduler.DateNavigator.Visible then Exit;
  if AName = 'DNWidth' then
    AValue := Width
end;

procedure TcxSchedulerControlBox.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if Scheduler.DateNavigator.Visible then Exit;
  if AName = 'DNWidth' then
  begin
    Scheduler.DateNavigator.Width := AValue;
    Scheduler.DateNavigator.DoLayoutChanged;
    Width := AValue;
  end;
end;

procedure TcxSchedulerControlBox.ApplyActualBounds;
begin
  FContainer.BoundsRect := Bounds;
  FContainer.Update;
end;

function TcxSchedulerControlBox.CreateWndContainerControl: TWinControl;
begin
  Result := TContainer.Create(Scheduler);
  Result.Parent := Scheduler;
end;

procedure TcxSchedulerControlBox.DoPaint;
begin
  Canvas.FillRect(ClientRect, ViewParams);
end;

procedure TcxSchedulerControlBox.RestorePosition;
begin
  if (FControl = nil) or
    (csDestroying in Control.ComponentState) then Exit;
  Control.RemoveFreeNotification(Scheduler);
  Control.Align := FControlAlign;
  Control.Parent := FControlParent;
  Control.BoundsRect := FControlRect;
end;

procedure TcxSchedulerControlBox.SetControl(
  AValue: TControl);
begin
  if HasAsParent(AValue) then Exit;
  if FControl <> AValue then
  begin
    RestorePosition;
    FControl := AValue;
    StorePosition;
    FContainer.BoundsRect := Bounds;
    FContainer.Visible := Visible and (Control <> nil);
    Changed;
  end
  else
    FContainer.Visible := False;
end;

procedure TcxSchedulerControlBox.StorePosition;
begin
  if Control = nil then Exit;
  FControlAlign := Control.Align;
  FControlParent := Control.Parent;
  FControlRect := Control.BoundsRect;
  Control.FreeNotification(Scheduler);
  Control.Parent := FContainer;
  Control.Align := alClient;
end;

{ TcxSchedulerEventHitTestController }

constructor TcxSchedulerEventHitTestController.Create(AOwner: TcxCustomScheduler);
begin
  FOwner := AOwner;
end;

procedure TcxSchedulerEventHitTestController.HideEventHint;
begin
  if FPrevHintEvent <> nil then
  begin
    HintController.HideHint;
    FPrevHintEvent := nil;
  end;
end;

procedure TcxSchedulerEventHitTestController.MouseMove(X, Y: Integer; AShift: TShiftState);
var
  R: TRect;
  AHintText: string;
begin
  if Scheduler.IsDesigning then Exit;
  HitTest.HitPoint := Point(X, Y);
  if ((ssLeft in AShift) or not HitTest.HitAtEvent) {and HintController.ViewMode} then
    HideEventHint
  else
  if HitTest.HitAtEvent then
    if FPrevHintEvent <> HitTest.Event then
    begin
      HitTest.FNeedShowHint := Scheduler.NeedShowHint(HitTest.Event, AHintText, HitTest.NeedShowHint);
      if HitTest.NeedShowHint then
      begin
        FPrevHintEvent := HitTest.Event;
        if Scheduler.OptionsView.Style = svsModern then
          HintController.Activate(HitTest.Event, HitTest.FEventBounds)
        else
        begin
          R := HintController.CalcHintRect(Max(cxscMinHintWidth,
            cxRectWidth(HitTest.FEventBounds)), AHintText, cxAlignBottom);
          HintController.Activate(R, AHintText);
        end;
      end
      else
        HideEventHint;
      if Scheduler.OptionsView.HotTrack then
        Scheduler.CurrentView.InvalidateRect(HitTest.FEventBounds);
    end;
end;

function TcxSchedulerEventHitTestController.GetHintController: TcxSchedulerHintController;
begin
  Result := Scheduler.HintController;
end;

function TcxSchedulerEventHitTestController.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := Scheduler.CurrentView.HitTest;
end;

{ TcxSchedulerCustomView }

constructor TcxSchedulerCustomView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FVisible := False;
  FCanShow := True;
end;

procedure TcxSchedulerCustomView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerCustomView then
    with TcxSchedulerCustomView(Source) do
    begin
      Self.CanShow := CanShow;
      Self.Active := Active;
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerCustomView.CanDeactivateOnDateNavigatorSelectionChange: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.CanEventEdit(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean;
begin
  Result := (AEvent.Source <> nil) or (not AInplace or Scheduler.CanIntersect(AEvent));
end;

function TcxSchedulerCustomView.CanSelectIndividualDays: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.CanSelectPeriod: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerCustomView.ChangeScale(M, D: Integer);
begin
// do nothing
end;

procedure TcxSchedulerCustomView.CheckResourceNavigatorKind;
begin
  // do nothing
end;

function TcxSchedulerCustomView.CheckEventsVisibility: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerViewController.Create(Self);
end;

function TcxSchedulerCustomView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerViewHitTest.Create(Self);
end;

procedure TcxSchedulerCustomView.DateChanged;
begin
  Refresh;
end;

procedure TcxSchedulerCustomView.DeactivateView;
begin
end;

procedure TcxSchedulerCustomView.DoAfterEventDeleting;
begin
  EventList.Selection.Clear;
end;

procedure TcxSchedulerCustomView.DoBeforeReallyDeleting(AEvent: TcxSchedulerControlEvent);
begin
end;

procedure TcxSchedulerCustomView.DoLayoutChanged;
begin
  TcxSchedulerCustomViewViewInfo(ViewInfo).FSelectedDays := Scheduler.SelectedDays;
  TcxSchedulerCustomViewViewInfo(ViewInfo).FEvents := Scheduler.EventList;
  inherited DoLayoutChanged;
  Scheduler.UpdateScrollBars;
end;

function TcxSchedulerCustomView.DoShowPopupMenu(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TcxSchedulerCustomView.EventContentSelected(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerCustomView.EventsListChanged;
begin
end;

function TcxSchedulerCustomView.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if not Scheduler.IsPopupScrollBars then
  begin
    if Scheduler.HScrollBarVisible then
      Dec(Result.Bottom, Scheduler.HScrollBar.Height);
    if Scheduler.VScrollBarVisible then
      if Scheduler.UseRightToLeftScrollBar then
        Inc(Result.Left, Scheduler.VScrollBar.Width)
      else
        Dec(Result.Right, Scheduler.VScrollBar.Width);
  end
  else
  begin
    if IsResourceNavigatorAllowed and Scheduler.ResourceNavigator.NeedScrollBar then
    begin
      CheckResourceNavigatorKind;
      if Scheduler.ResourceNavigator.ScrollBarKind = sbVertical then
        if Scheduler.UseRightToLeftScrollBar then
          Inc(Result.Left, Scheduler.VScrollBar.Width)
        else
          Dec(Result.Right, Scheduler.VScrollBar.Width);
      if Scheduler.ResourceNavigator.ScrollBarKind = sbHorizontal then
        Dec(Result.Bottom, Scheduler.HScrollBar.Height);
    end;
  end;
end;

function TcxSchedulerCustomView.GetControlCanvas: TcxCanvas;
begin
  Result := Scheduler.GetInternalCanvas;
end;

function TcxSchedulerCustomView.GetDateOffset: Integer;
begin
  Result := 1 - Ord(OptionsView.ActualStartOfWeek);
end;

function TcxSchedulerCustomView.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TcxSchedulerDragObject;
end;

function TcxSchedulerCustomView.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := nil;
end;

function TcxSchedulerCustomView.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := gkNone;
end;

function TcxSchedulerCustomView.GetHScrollBarBounds: TRect;
begin
  Result := ClientRect;
  if Scheduler.IsPopupScrollBars then
  begin
    Result.Top := Result.Bottom - Scheduler.HScrollBar.Height;
    if Scheduler.VScrollBarVisible then
      Dec(Result.Right, Scheduler.GetScrollBarSize.cx);
  end
  else
  begin
    Result.Top := Result.Bottom;
    Result.Bottom := Result.Top + Scheduler.HScrollBar.Height;
  end;
  OffsetRect(Result, Bounds.Left, Bounds.Top);
  if not Scheduler.IsPopupScrollBars then
    if Scheduler.ResourceNavigator.NeedScrollBar and (Scheduler.ResourceNavigator.ScrollBarKind = sbHorizontal) then
      Inc(Result.Left, Scheduler.ResourceNavigator.MeasureWidth);
  if Scheduler.UseRightToLeftAlignment then
  begin
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
    if Scheduler.VScrollBarVisible and (not Scheduler.IsPopupScrollBars or
       Scheduler.ResourceNavigator.NeedScrollBar and (Scheduler.ResourceNavigator.ScrollBarKind = sbVertical)) then
      OffsetRect(Result, Scheduler.VScrollBar.Width, 0);
  end;
end;

function TcxSchedulerCustomView.GetEditRectForEvent(
  AEvent: TcxSchedulerControlEvent; const ADate: TDateTime;
  AResource: TcxSchedulerStorageResourceItem): TRect;
begin
  Result := cxNullRect;
end;

function TcxSchedulerCustomView.GetEditStyle(
  AEvent: TcxSchedulerControlEvent): TcxCustomEditStyle;
begin
  Result := Scheduler.EditStyle;
end;

function TcxSchedulerCustomView.GetEditProperties(
  AEvent: TcxSchedulerControlEvent): TcxCustomEditProperties;
begin
  Scheduler.DoGetEventEditProperties(AEvent, Result);
end;

function TcxSchedulerCustomView.GetEditWithSingleLineEditor(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.GetEventHintText(AEvent: TcxSchedulerControlEvent): string;
begin
  Result := Scheduler.GetEventHintText(AEvent);
end;

function TcxSchedulerCustomView.GetSelFinishForInitEventBySelectedTime: TDateTime;
begin
  Result := Scheduler.SelFinish;
end;

function TcxSchedulerCustomView.GetSelStartForInitEventBySelectedTime: TDateTime;
begin
  Result := Scheduler.SelStart;
end;

function TcxSchedulerCustomView.GetEventVisibility(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := False;
end;

function TcxSchedulerCustomView.GetFirstVisibleDate: TDateTime;
begin
  if SelectedDays.Count = 0 then
    Result := Date
  else
    Result := Integer(SelectedDays.First)
end;

function TcxSchedulerCustomView.GetFirstVisibleTime: TDateTime;
begin
  Result := 0;
end;

function TcxSchedulerCustomView.GetLastVisibleDate: TDateTime;
begin
  if SelectedDays.Count = 0 then
    Result := Date
  else
    Result := Integer(SelectedDays.Last)
end;

function TcxSchedulerCustomView.GetLastVisibleTime: TDateTime;
begin
  Result := 1 - 1 / 24 / 60;
end;

function TcxSchedulerCustomView.GetSizeGripBounds: TRect;
var
  AVertScrollBarBounds, AHorzScrollBarBounds: TRect;
begin
  AVertScrollBarBounds := Scheduler.GetVScrollBarBounds;
  Result.Left := AVertScrollBarBounds.Left;
  Result.Right := AVertScrollBarBounds.Right;
  AHorzScrollBarBounds := Scheduler.GetHScrollBarBounds;
  Result.Top := AHorzScrollBarBounds.Top;
  Result.Bottom := AHorzScrollBarBounds.Bottom;
end;

function TcxSchedulerCustomView.GetSchedulerLookAndFeel(
  ADialogs: Boolean = False): TcxLookAndFeel;
begin
  if ADialogs then
    Result := Scheduler.DialogsLookAndFeel
  else
    Result := Scheduler.LookAndFeel;
end;

function TcxSchedulerCustomView.GetScrollTimeHint: string;
begin
  Result := cxTimeToStr(GetFirstVisibleTime, dxFormatSettings.ShortTimeFormat) + ' - ' +
    cxTimeToStr(GetLastVisibleTime, dxFormatSettings.ShortTimeFormat);
end;

function TcxSchedulerCustomView.GetShowEventsWithoutResource: Boolean;
begin
  Result := Scheduler.OptionsView.ShowEventsWithoutResource;
end;

function TcxSchedulerCustomView.GetTimeIncrement: TDateTime;
begin
  Result := 1;
end;

function TcxSchedulerCustomView.GetVScrollBarBounds: TRect;
begin
  Result := ClientRect;
  if Scheduler.IsPopupScrollBars then
  begin
    if Scheduler.UseRightToLeftScrollBar then
      Result.Right := Result.Left + Scheduler.VScrollBar.Width
    else
      Result.Left := Result.Right - Scheduler.VScrollBar.Width;
    if Scheduler.HScrollBarVisible then
      Dec(Result.Bottom, Scheduler.GetScrollBarSize.cy);
  end
  else
    if Scheduler.UseRightToLeftScrollBar then
    begin
      Result.Right := Result.Left;
      Result.Left := Result.Right - Scheduler.VScrollBar.Width;
    end
    else
    begin
      Result.Left := Result.Right;
      Result.Right := Result.Left + Scheduler.VScrollBar.Width;
    end;
  OffsetRect(Result, Bounds.Left, Bounds.Top);
  if not Scheduler.IsPopupScrollBars then
    if Scheduler.ResourceNavigator.NeedScrollBar and (Scheduler.ResourceNavigator.ScrollBarKind = sbVertical) then
      Dec(Result.Bottom, Scheduler.ResourceNavigator.MeasureHeight);
end;

function TcxSchedulerCustomView.GetViewContentRect: TRect;
begin
  Result := ClientRect;
end;

function TcxSchedulerCustomView.GetVisibleDaysRange: Integer;
begin
  Result := 31;
end;

procedure TcxSchedulerCustomView.InitEventBySelectedTime(
  AEvent: TcxSchedulerEvent; AllDay: Boolean;
  ARecurrence: Boolean; AInplaceEditing: Boolean);
var
  AIsAllDaySelection: Boolean;
begin
  AIsAllDaySelection := IsAllDaySelection;
  AEvent.Start := GetSelStartForInitEventBySelectedTime;
  AEvent.AllDayEvent := AllDay or AIsAllDaySelection;
  if AllDay and not AIsAllDaySelection then
    AEvent.Finish := GetSelFinishForInitEventBySelectedTime + 1
  else
    AEvent.Finish := GetSelFinishForInitEventBySelectedTime;
end;

function TcxSchedulerCustomView.IsAllDaySelection: Boolean;
var
  AStart, AFinish, H, M, S, MS: Word;
begin
  if dxDateOf(Scheduler.SelFinish - Scheduler.SelStart) >= 1 then
  begin
    DecodeTime(Scheduler.SelStart, H, M, S, MS);
    AStart := H * 60 + M;
    DecodeTime(Scheduler.SelFinish, H, M, S, MS);
    AFinish := H * 60 + M;
    Result := (AStart = 0) and (AFinish = 0);
  end
  else
    Result := False;
end;

function TcxSchedulerCustomView.IsDayView: Boolean;
begin
  Result := False;
end;

function TcxSchedulerCustomView.IsInplaceEditingEnabled: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.IsResourceNavigatorAllowed: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomView.IsShowResources: Boolean;
begin
  Result := Scheduler.HasResources;
end;

function TcxSchedulerCustomView.IsWorkTime(
  AResourceItem: TcxSchedulerStorageResourceItem;
  const ADateTime: TDateTime): Boolean;
begin
  Result := Scheduler.DoIsWorkTime(AResourceItem, ADateTime);
end;

procedure TcxSchedulerCustomView.MakeEventVisible(
  AEvent: TcxSchedulerControlEvent; const ADate: TDateTime;
  AResource: TcxSchedulerStorageResourceItem);
begin
end;

function TcxSchedulerCustomView.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerCustomView.PeriodChanged;
begin
  Scheduler.ValidateSelection(SelectedDays);
end;

procedure TcxSchedulerCustomView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  DoScaleScroll;
  if AScrollBarKind = sbHorizontal then
    Scheduler.HScrollBar.Position := AScrollPos
  else
    Scheduler.VScrollBar.Position := AScrollPos;
end;

procedure TcxSchedulerCustomView.ScrollSelectedDays(AScrollDelta: Integer);
begin
  Scheduler.DateNavigator.ScrollSelection(AScrollDelta);
end;

procedure TcxSchedulerCustomView.ScrollSelectedDays(AForward: Boolean;
  ANeedDate: TDateTime; AIsByPage: Boolean);
begin
  Scheduler.DateNavigator.ScrollSelection(AForward, ANeedDate, AIsByPage);
end;

procedure TcxSchedulerCustomView.ScrollVisibleDays(
  AScrollUp: Boolean);
begin
end;

procedure TcxSchedulerCustomView.SelectedDaysChanged;
begin
end;

function TcxSchedulerCustomView.ShowTaskComplete: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerCustomView.TimeChanged;
begin
end;

procedure TcxSchedulerCustomView.UpdateDateNavigatorSelection;
begin
  Scheduler.DateNavigator.MakeSelectionVisible;
end;

procedure TcxSchedulerCustomView.ValidateContentPopupMenuItems(
  var AItems: TcxSchedulerContentPopupMenuItems);
begin
end;

procedure TcxSchedulerCustomView.ValidateSelectionFinishTime(
  var ADateTime: TDateTime);
begin
end;

procedure TcxSchedulerCustomView.VisibleChanged;
begin
  if Visible and not CanShow then
    Visible := False
  else
    Scheduler.ViewVisibleChanged(Self);
  if Visible then
    Scheduler.UpdateEventsCache(True);
end;

procedure TcxSchedulerCustomView.HideHintOnScroll(AScrollCode: TScrollCode);
begin
  Scheduler.HintController.HideHint;
  if AScrollCode = scEndScroll then
    FCalculatedHintBounds := False;
end;

procedure TcxSchedulerCustomView.ShowHintOnScroll(const ADate: TDateTime);
var
  AResourceNavigator: TcxSchedulerResourceNavigator;
begin
  AResourceNavigator := Scheduler.ResourceNavigator;
  if AResourceNavigator.NeedScrollBar and (AResourceNavigator.ScrollBarKind = sbHorizontal) then
    ShowHintOnScroll(DateTimeToStr(ADate), sbVertical)
  else
    ShowHintOnScroll(DateTimeToStr(ADate), sbHorizontal);
end;

procedure TcxSchedulerCustomView.ShowHintOnScroll(
  const AHintText: string; AScrollBarKind: TScrollBarKind);
var
  R: TRect;
  P: TPoint;
begin
  if not CalculatedHintBounds then
  begin
    if AScrollBarKind = sbVertical then
      Scheduler.HintController.HintWindowAnchor := GetMouseCursorPos.Y
    else
      Scheduler.HintController.HintWindowAnchor := GetMouseCursorPos.X;
  end;
  R := Scheduler.HintController.CalcHintRect(cxscMaxHintWidth, AHintText, 0);
  if AScrollBarKind = sbVertical then
  begin
    if Scheduler.UseRightToLeftScrollBar then
    begin
      P := Point(Scheduler.GetVScrollBarBounds.Right, Scheduler.GetVScrollBarBounds.Top);
      R := cxRectSetLeft(R, Scheduler.ClientToScreen(P).X + cxRectWidth(R) + 5);
    end
    else
      R := cxRectSetRight(R, Scheduler.ClientToScreen(Scheduler.GetVScrollBarBounds.TopLeft).X - 5);
    R := cxRectSetTop(R, Scheduler.HintController.HintWindowAnchor - ((R.Bottom - R.Top) div 2))
  end
  else
  begin
    R := cxRectSetBottom(R, Scheduler.ClientToScreen(Scheduler.GetHScrollBarBounds.TopLeft).Y - 5);
    if Scheduler.UseRightToLeftScrollBar then
      R := cxRectSetLeft(R, Scheduler.HintController.HintWindowAnchor + ((R.Right - R.Left) div 2))
    else
      R := cxRectSetLeft(R, Scheduler.HintController.HintWindowAnchor - ((R.Right - R.Left) div 2));
  end;
  Scheduler.HintController.Activate(R, AHintText, True, False);
  FCalculatedHintBounds := True;
end;

function TcxSchedulerCustomView.GetActive: Boolean;
begin
  Result := Visible;
end;

function TcxSchedulerCustomView.GetCanShow: Boolean;
begin
  Result := FCanShow;
  Scheduler.DoCanShowView(Self, Result);
end;

function TcxSchedulerCustomView.GetController: TcxSchedulerViewController;
begin
  Result := TcxSchedulerViewController(inherited Controller);
end;

function TcxSchedulerCustomView.GetDragCloneEventList: TcxSchedulerFilteredEventList;
begin
  if Assigned(Controller.DragEventHelper) then
    Result := Scheduler.EventList.Clones
  else
    Result := nil;
end;

function TcxSchedulerCustomView.GetEventList: TcxSchedulerCachedEventList;
begin
  Result := Scheduler.EventList;
end;

function TcxSchedulerCustomView.GetHitTest: TcxSchedulerViewHitTest;
begin
  Result := TcxSchedulerViewHitTest(inherited HitTest);
end;

function TcxSchedulerCustomView.GetSelectedDays: TcxSchedulerDateList;
begin
  Result := Scheduler.SelectedDays
end;

function TcxSchedulerCustomView.GetOptionsView: TcxSchedulerOptionsView;
begin
  Result := Scheduler.OptionsView;
end;

procedure TcxSchedulerCustomView.GetRangeControlRange(out AMin,
  AMax: TDateTime);
begin
  AMin := FirstVisibleDate;
  AMax := LastVisibleDate;
end;

procedure TcxSchedulerCustomView.GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList);
begin
  AValues.Add(AScales.Week);
  AValues.Add(AScales.Day);
end;

procedure TcxSchedulerCustomView.GetRangeControlTotalRange(out AMin, AMax: TDateTime);
var
  AStart: TDateTime;
begin
  AStart := FirstVisibleDate;
  AMin := IncMonth(AStart, -1);
  AMax := IncMonth(AStart, 1) + 1;
end;

function TcxSchedulerCustomView.GetResources: TcxSchedulerStorageResourceItems;
begin
  if Scheduler.StorageValid then
    Result := Scheduler.Storage.Resources.ResourceItems
  else
    Result := nil;
end;

function TcxSchedulerCustomView.GetWorkDays: TDays;
begin
  Result := OptionsView.WorkDays;
end;

function TcxSchedulerCustomView.GetWorkFinish: TDateTime;
begin
  Result := OptionsView.WorkFinish;
end;

function TcxSchedulerCustomView.GetWorkStart: TDateTime;
begin
  Result := OptionsView.WorkStart;
end;

procedure TcxSchedulerCustomView.SetActive(AValue: Boolean);
begin
  Visible := FCanShow and AValue;
end;

procedure TcxSchedulerCustomView.SetCanShow(AValue: Boolean);
begin
  if FCanShow <> AValue then
  begin
    FCanShow := AValue;
    if not AValue then
      Visible := False
    else
    begin
      if not Scheduler.CurrentView.Visible then
        Visible := True;
      Changed;
    end;
  end;
end;

procedure TcxSchedulerCustomView.VisibleRangeChanged;
begin
  Scheduler.SynchronizeRangeControl;
end;

procedure TcxSchedulerCustomView.DoCreateScrollBars;
begin
end;

procedure TcxSchedulerCustomView.DoDestroyScrollBars;
begin
end;

function TcxSchedulerCustomView.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := nil;
end;

procedure TcxSchedulerCustomView.InitScrollBars;
begin
end;

{ TcxSchedulerCustomViewViewInfo }

procedure TcxSchedulerCustomViewViewInfo.CheckResourceNavigator;
begin
  CheckResourceNavigatorKind;
  ResourceNavigator.InitScrollBarsParameters;
end;

procedure TcxSchedulerCustomViewViewInfo.CheckResourceNavigatorKind;
begin
  ResourceNavigator.FScrollBarKind := GetResourceScrollBarKind;
end;

function TcxSchedulerCustomViewViewInfo.GetResourceScrollBarKind: TScrollBarKind;
begin
  Result := sbHorizontal;
end;

function TcxSchedulerCustomViewViewInfo.DoGetEventDisplayText(
  AEvent: TcxSchedulerControlEvent): string;
begin
  Scheduler.DoGetEventDisplayText(AEvent, Result);
end;

function TcxSchedulerCustomViewViewInfo.DoSchedulerMoreEventsButtonClick: Boolean;
begin
  Result := Scheduler.DoMoreEventsButtonClick;
end;

function TcxSchedulerCustomViewViewInfo.DoSchedulerNavigationButtonClick(
  AnInterval: TDateTime; AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  Result := Scheduler.DoNavigationButtonClick(AnInterval, AResource);
end;

function TcxSchedulerCustomViewViewInfo.GetEventHint(
  AEvent: TcxSchedulerControlEvent): string;
begin
  Result := Scheduler.GetEventHintText(AEvent);
end;

function TcxSchedulerCustomViewViewInfo.GetSchedulerEventsList: TcxSchedulerCachedEventList;
begin
  Result := Scheduler.EventList;
end;

procedure TcxSchedulerCustomViewViewInfo.DeleteClone(AEvent: TcxSchedulerControlEvent);
begin
  Events.DeleteClone(AEvent);
  View.Refresh;
end;

procedure TcxSchedulerCustomViewViewInfo.SetEventsVisibility(
  AShowSources, AShowClones: Boolean; AForceRepaint: Boolean = False);
begin
end;

function TcxSchedulerCustomViewViewInfo.GetResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := Scheduler.ResourceNavigator;
end;

function TcxSchedulerCustomViewViewInfo.GetScheduler: TcxCustomScheduler;
begin
  Result := Owner.Scheduler;
end;

function TcxSchedulerCustomViewViewInfo.GetSchedulerStyle: TcxSchedulerViewStyle;
begin
  Result := Scheduler.OptionsView.Style;
end;

function TcxSchedulerCustomViewViewInfo.GetView: TcxSchedulerCustomView;
begin
  Result := TcxSchedulerCustomView(Owner);
end;

{ TcxSchedulerCustomDataNavigator }

constructor TcxSchedulerCustomDateNavigator.Create(
  AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FSaveSelectionList := TcxSchedulerDateList.Create;
end;

destructor TcxSchedulerCustomDateNavigator.Destroy;
begin
  FSaveSelectionList.Free;
  inherited Destroy;
end;

procedure TcxSchedulerCustomDateNavigator.BeginUpdate;
begin
  if FLockCount = 0 then
    SaveState;
  Inc(FLockCount);
end;

procedure TcxSchedulerCustomDateNavigator.CancelUpdates;
begin
  FLockCount := 0;
end;

procedure TcxSchedulerCustomDateNavigator.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount <= 0 then
  begin
    FLockCount := 0;
    CheckChanges;
  end;
end;

procedure TcxSchedulerCustomDateNavigator.BoundsChanging;
begin
  BeginUpdate;
  GetCalendarDimension(FPrevColCount, FPrevRowCount);
end;

procedure TcxSchedulerCustomDateNavigator.BoundsChanged;
var
  AColCount, ARowCount: Integer;
begin
  inherited BoundsChanged;
  if not Scheduler.BoundsChanging and Scheduler.SubControlsCreated then
  begin
    GetCalendarDimension(AColCount, ARowCount);
    if AColCount * ARowCount <> FPrevColCount * FPrevRowCount then
      Scheduler.FullRefresh;
  end;
  EndUpdate;
end;

function TcxSchedulerCustomDateNavigator.CanMultiSelect: Boolean;
begin
  Result := (Scheduler.CurrentView = nil) or
    Scheduler.CurrentView.CanSelectPeriod;
  if Result then
    Result := Scheduler.CanSelectPeriod;
end;

procedure TcxSchedulerCustomDateNavigator.CheckChanges;
var
  I: Integer;
  ACallEvent: Boolean;
begin
  if (FSaveRealFirstDate <> GetRealFirstDate) or (FSaveRealLastDate <> GetRealLastDate) then
    DoPeriodChangedEvent;
  Scheduler.UpdateHolidayDays(GetRealFirstDate, GetRealLastDate);
  GetSelection.Changed := False;
  ACallEvent := GetSelection.Count <> FSaveSelectionList.Count;
  if not ACallEvent then
    for I := 0 to FSaveSelectionList.Count - 1 do
      if FSaveSelectionList[I] <> GetSelection[I] then
      begin
        ACallEvent := True;
        break;
      end;
  if ACallEvent then
    DoSelectionChangedEvent;
  Scheduler.NotifySchedulerChanged;
  RefreshDays;
  SaveState;
end;

procedure TcxSchedulerCustomDateNavigator.CheckCurrentDate;
begin
end;

function TcxSchedulerCustomDateNavigator.GetMonthSize: TSize;
begin
  Result := cxNullSize;
end;

function TcxSchedulerCustomDateNavigator.GetSelection: TcxSchedulerDateList;
begin
  Result := Scheduler.SelectedDays;
end;

function TcxSchedulerCustomDateNavigator.IsSchedulerLocked: Boolean;
begin
  Result := Scheduler.AligningSubControls or Scheduler.IsLocked or
    not Scheduler.HandleAllocated;
end;

procedure TcxSchedulerCustomDateNavigator.Loaded;
begin
  FSavedSize := Scheduler.GetDateNavigatorLoadedSize;
end;

procedure TcxSchedulerCustomDateNavigator.PeriodChanged;
begin
  Scheduler.DateNavigatorSelectionChanged;
end;

procedure TcxSchedulerCustomDateNavigator.SaveSize;
begin
  FSavedSize := cxSize(Width, Height);
end;

procedure TcxSchedulerCustomDateNavigator.SaveState;
begin
  FSaveSelectionList.Assign(GetSelection);
  FSaveRealFirstDate := GetRealFirstDate;
  FSaveRealLastDate := GetRealLastDate;
end;

procedure TcxSchedulerCustomDateNavigator.ScrollSelection(AForward: Boolean;
  ANeedDate: TDateTime; AIsByPage: Boolean);
begin
  BeginUpdate;
  try
    DoScrollSelection(AForward, ANeedDate, AIsByPage);
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerCustomDateNavigator.ScrollSelection(AScrollDelta: Integer);
begin
  BeginUpdate;
  try
    DoScrollSelection(AScrollDelta);
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerCustomDateNavigator.GetEventDays: TcxSchedulerDateList;
begin
  Result := Scheduler.EventDays;
end;

function TcxSchedulerCustomDateNavigator.GetHintController: TcxSchedulerHintController;
begin
  Result := Scheduler.HintController;
end;

function TcxSchedulerCustomDateNavigator.GetHolidayDays: TcxSchedulerDateList;
begin
  Result := Scheduler.HolidayDays;
end;

{ TcxSchedulerClipboardController }

constructor TcxSchedulerClipboardController.Create(AScheduler: TcxCustomScheduler);
begin
  RegisterClipboardFormat;
  FScheduler := AScheduler;
  FStream := TMemoryStream.Create;
  FStreamReader := TcxReader.Create(FStream);
  FStreamWriter := TcxWriter.Create(FStream);
end;

destructor TcxSchedulerClipboardController.Destroy;
begin
  FStream.Free;
  FStreamReader.Free;
  FStreamWriter.Free;
  inherited Destroy;
end;

function TcxSchedulerClipboardController.CanCopy: Boolean;
begin
  Result := Scheduler.SelectedEventCount > 0;
end;

function TcxSchedulerClipboardController.CanPaste: Boolean;
begin
  Result := GetClipboardToStream;
  if Result then
    Result := ValidateStream;
end;

procedure TcxSchedulerClipboardController.Copy;
begin
  if GetSelectionAsStream then
  try
    SetStreamToClipboard;
  finally
    Stream.Clear;
  end;
end;

procedure TcxSchedulerClipboardController.Cut;
begin
  Copy;
  if Scheduler.EventOperations.Deleting then
    DeleteSelectedEvents;
end;

procedure TcxSchedulerClipboardController.Paste;
var
  AEvents: TcxSchedulerFilteredEventList;
  Anchor: TDateTime;
begin
  if GetClipboardToStream then
  begin
    AEvents := TcxSchedulerFilteredEventList.Create;
    try
      if GetStreamAsEvents(AEvents, Anchor) then
        InsertEvents(AEvents, Anchor);
    finally
      AEvents.Free;
    end;
  end;
end;

procedure TcxSchedulerClipboardController.DeleteSelectedEvents;
begin
  Scheduler.DeleteSelectedEvents;
end;

function TcxSchedulerClipboardController.GetClipboard: TClipboard;
begin
  Result := Clipbrd.Clipboard;
end;

function TcxSchedulerClipboardController.GetClipboardToStream: Boolean;
var
  AData: THandle;
  ADataPtr: Pointer;
begin
  Result := not IsClipboardBusy and IsClipboardFormatAvailable(CF_SCHEDULERDATA);
  if not Result then Exit;
  Clipboard.Open;
  try
    AData := GetClipboardData(CF_SCHEDULERDATA);
    if AData = 0 then Exit;
    ADataPtr := GlobalLock(AData);
    Result := ADataPtr <> nil;
    if not Result then Exit;
    try
      Stream.Size := GlobalSize(AData);
      Stream.Position := 0;
      Stream.Write(ADataPtr^, Stream.Size);
    finally
      GlobalUnlock(AData);
    end;
  finally
    Clipboard.Close;
  end;
end;

function TcxSchedulerClipboardController.GetSelectionAsStream: Boolean;
var
  I: Integer;
  Anchor: TDateTime;
begin
  Stream.Clear;
  Result := Scheduler.SelectedEventCount > 0;
  if not Result then Exit;
  StreamWriter.WriteInteger(0);
  StreamWriter.WriteInteger(Scheduler.SelectedEventCount);
  Anchor := MaxInt;
  for I := 0 to Scheduler.SelectedEventCount - 1 do
  begin
    if Anchor > Scheduler.SelectedEvents[I].Start then
      Anchor := Scheduler.SelectedEvents[I].Start;
  end;
  StreamWriter.WriteDateTime(Anchor);
  for I := 0 to Scheduler.SelectedEventCount - 1 do
    SaveEvent(Scheduler.SelectedEvents[I]);
  Stream.Position := 0;
  StreamWriter.WriteInteger(Stream.Size);
end;

function TcxSchedulerClipboardController.GetStreamAsEvents(
  AEvents: TcxSchedulerFilteredEventList; var Anchor: TDateTime): Boolean;
var
  ACount: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  Result := ValidateStream;
  if Result then
  try
    ACount := StreamReader.ReadInteger;
    Anchor := StreamReader.ReadDateTime;
    while Stream.Position <> Stream.Size do
    begin
      RestoreEvent(AEvent);
      AEvents.Add(AEvent);
    end;
    Result := (AEvents.Count > 0) and (AEvents.Count = ACount);
  finally
    Stream.Clear;
  end;
end;

function TcxSchedulerClipboardController.IsClipboardBusy: Boolean;
begin
  Result := not OpenClipboard(Application.Handle);
  if not Result then
    CloseClipboard;
end;

procedure TcxSchedulerClipboardController.CalculateAnchorForResource(
  AEvents: TcxSchedulerFilteredEventList;
  const AResourceID: Variant; var Anchor: TDateTime);
var
  I: Integer;
  ANewAnchor: TDateTime;
begin
  ANewAnchor := MaxInt;
  for I := 0 to AEvents.Count - 1 do
    if AEvents[I].IsSharedWithResource(AResourceID) and (ANewAnchor > AEvents[I].Start) then
      ANewAnchor := AEvents[I].Start;
  if ANewAnchor <> MaxInt then
    Anchor := ANewAnchor;
end;

procedure TcxSchedulerClipboardController.InsertEvents(
  AEvents: TcxSchedulerFilteredEventList; Anchor: TDateTime);
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
  AllDayDuration: Boolean;
  IsDifferenceResources: Boolean;
begin
  if not Scheduler.EventOperations.Creating then Exit;
  IsDifferenceResources := False;
  for I := 1 to AEvents.Count - 1 do
  begin
    IsDifferenceResources := not VarEquals(AEvents[I - 1].ResourceID, AEvents[I].ResourceID);
    if IsDifferenceResources then Break;
  end;
  if IsDifferenceResources and (Scheduler.SelResource <> nil) then
    CalculateAnchorForResource(AEvents, Scheduler.SelResource.ResourceID, Anchor);
  Anchor := Scheduler.SelStart - Anchor;
  AllDayDuration := Round(Frac(Anchor) / MinuteToTime) = 0;
  Storage.BeginUpdate;
  try
    for I := 0 to AEvents.Count - 1 do
    begin
      AEvent := AEvents[I];
      AEvent.MoveTo(AEvent.Start + Anchor);
      AEvent.EventType := etNone;
      if AEvent.AllDayEvent and not AllDayDuration then
      begin
        AEvent.AllDayEvent := False;
        AEvent.Start := Scheduler.SelStart;
        AEvent.Duration := 30 * MinuteToTime;
        if AEvent.State = tlsFree then
          AEvent.State := tlsBusy;
      end;
      if not IsDifferenceResources or (Scheduler.SelResource = nil) then
      begin
        if Scheduler.SelResource = nil then
          AEvent.ResourceID := Null
        else
          AEvent.ResourceID := Scheduler.SelResource.ResourceID;
      end;
      if Scheduler.CanIntersect(AEvent) then
        Storage.createEvent.Assign(AEvent)
    end;
  finally
    Storage.EndUpdate;
  end;
end;

function TcxSchedulerClipboardController.KeyDown(
  var AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := False;
  case AKey of
    VK_INSERT:
      if [ssShift, ssCtrl] * AShift <> [] then
      begin
        if ssShift in AShift then
          Paste
        else
          if ssCtrl in AShift then
            Copy;
        Result := True;
      end;
    VK_DELETE:
      if ssShift in AShift then
      begin
        Cut;
        Result := True;
      end;
  end;
end;

function TcxSchedulerClipboardController.KeyPress(var AKey: Char): Boolean;
begin
  Result := dxCharInSet(AKey, [^C, ^X, ^V]);
  if Result then
  begin
    case AKey of
      ^C:
        Copy;
      ^X:
        Cut;
      ^V:
        Paste;
    end;
    AKey := #0;
  end;
end;

procedure TcxSchedulerClipboardController.RestoreEvent(
  var AEvent: TcxSchedulerControlEvent);

  function CheckAndFixUpByteArray(const AValue: Variant): Variant;
  var
    I: Integer;
    V: Variant;
  begin
    Result := AValue;
    if VarIsArray(AValue) and (VarArrayDimCount(AValue) = 1) then
    begin
      V := VarArrayCreate([VarArrayLowBound(AValue, 1), VarArrayHighBound(AValue, 1)], varByte);
      for I := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
        if VarType(AValue[I]) in [varByte, varShortInt, varSmallint] then
          V[I] := AValue[I]
        else
          Exit;
      Result := V;
    end;
  end;

var
  I, ACount, ALastPos: Integer;
begin
  ALastPos := StreamReader.ReadInteger;
  if ALastPos <= Stream.Size then
  begin
    AEvent := TcxSchedulerControlEvent.Create(Storage);
    ACount := StreamReader.ReadInteger;
    if ACount > AEvent.ValueCount then
      ACount := AEvent.ValueCount;
    for I := 0 to ACount - 1 do
      AEvent.Values[I] := CheckAndFixUpByteArray(StreamReader.ReadVariant);
    AEvent.IsDataValid := False;
  end;
  Stream.Position := ALastPos;
end;

procedure TcxSchedulerClipboardController.SaveEvent(
  AEvent: TcxSchedulerControlEvent);
var
  I, APos: Integer;
begin
  APos := Stream.Position;
  StreamWriter.WriteInteger(APos);
  StreamWriter.WriteInteger(AEvent.ValueCount);
  for I := 0 to AEvent.ValueCount - 1 do
    StreamWriter.WriteVariant(AEvent.Values[I]);
  Stream.Position := APos;
  StreamWriter.WriteInteger(Stream.Size);
  Stream.Position := Stream.Size;
end;

procedure TcxSchedulerClipboardController.SetStreamToClipboard;
var
  Data: THandle;
  DataPtr: Pointer;
begin
  if (Stream.Size = 0) or IsClipboardBusy then Exit;
  Clipboard.Open;
  try
    EmptyClipboard;
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Stream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Stream.Position := 0;
        Stream.ReadBuffer(DataPtr^, Stream.Size);
        if SetClipboardData(CF_SCHEDULERDATA, Data) = 0 then
          RaiseLastOSError;
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

function TcxSchedulerClipboardController.ValidateStream: Boolean;
var
  APos, ACount, ASavedCount: Integer;
begin
  Stream.Position := 0;
  Result := (Stream.Size > SizeOf(Integer) * 2 + SizeOf(TDateTime)) and
    (StreamReader.ReadInteger = Stream.Size);
  if not Result then Exit;
  ASavedCount := StreamReader.ReadInteger;
  ACount := 0;
  StreamReader.ReadDateTime;
  while Stream.Position < (Stream.Size - SizeOf(Integer)) do
  begin
    APos := StreamReader.ReadInteger;
    if APos <= Stream.Size then
    begin
      Stream.Position := APos;
      Inc(ACount);
    end
    else
      Break;
  end;
  Result := ASavedCount = ACount;
  if Result then
    Stream.Position := SizeOf(Integer);
end;

function TcxSchedulerClipboardController.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Scheduler.Storage;
end;

procedure TcxSchedulerClipboardController.RegisterClipboardFormat;
begin
  CF_SCHEDULERDATA := Windows.RegisterClipboardFormat(SCF_SCHEDULERCLIPBOARDFORMAT);
end;

{ TcxSchedulerStyles }

constructor TcxSchedulerStyles.Create(AOwner: TPersistent);
begin
  if AOwner is TcxCustomScheduler then
    FScheduler := TcxCustomScheduler(AOwner);
  inherited Create(AOwner);
  BitmapInViewParams := True;
end;

procedure TcxSchedulerStyles.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerStyles then
  begin
    for I := 0 to cxcsMaxValue do
      SetValue(I, TcxSchedulerStyles(Source).GetValue(I))
  end;
  inherited Assign(Source);
end;

function TcxSchedulerStyles.GetBackgroundParams: TcxViewParams;
begin
  GetViewParams(cxcsBackground, nil, nil, Result);
end;

function TcxSchedulerStyles.GetContentParams(const ADateTime: TDateTime;
  AResource: TcxSchedulerStorageResourceItem = nil): TcxViewParams;
begin
  Result := GetContentParams(ADateTime, Scheduler.DoIsWorkTime(AResource, ADateTime), AResource);
end;

function TcxSchedulerStyles.GetContentParams(const ADateTime: TDateTime;
  ALightColor: Boolean; AResource: TcxSchedulerStorageResourceItem = nil): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetContentStyle) then
    FOnGetContentStyle(Scheduler, AResource, ADateTime, AStyle);
  GetViewParams(cxcsContent, AResource, AStyle, Result);
  Result.Color := PainterHelper.GetContentColor(Result.Color, ALightColor, SchedulerViewStyle);
end;

function TcxSchedulerStyles.GetEventContentParams(
  AEvent: TcxSchedulerEvent): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(OnGetEventStyle) then
    OnGetEventStyle(Scheduler, AEvent, AStyle);
  GetViewParams(cxcsEvent, AEvent, AStyle, Result);
end;

function TcxSchedulerStyles.IsEventStyleAssigned(
  AEvent: TcxSchedulerEvent): Boolean;
var
  AStyle: TcxStyle;
begin
  Result := (AEvent.LabelColor <> clDefault) or (Event <> nil);
  if not Result and Assigned(FOnGetEventStyle) then
  begin
    AStyle := nil;
    FOnGetEventStyle(Scheduler, AEvent, AStyle);
    Result := AStyle <> nil;
  end;
end;

function TcxSchedulerStyles.GetDayHeaderParams(
  const ADate: TDateTime): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetDayHeaderStyle) then
    FOnGetDayHeaderStyle(Scheduler, ADate, AStyle);
  GetViewParams(cxcsDayHeader, Pointer(Trunc(ADate)), AStyle, Result);
end;

function TcxSchedulerStyles.GetGroupSeparatorParams: TcxViewParams;
begin
  GetViewParams(cxcsGroupSeparator, nil, nil, Result);
end;

function TcxSchedulerStyles.GetResourceHeaderParams(
  AResource: TcxSchedulerStorageResourceItem): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetResourceHeaderStyle) then
    FOnGetResourceHeaderStyle(Scheduler, AResource, AStyle);
  GetViewParams(cxcsResourceHeader, nil, AStyle, Result);
end;

function TcxSchedulerStyles.GetSplitterParams(
  AKind: TcxSchedulerSplitterKind): TcxViewParams;
begin
  GetViewParams(cxcsHSplitter + Byte(AKind), nil, nil, Result);
end;

function TcxSchedulerStyles.GetSelectionParams: TcxViewParams;
begin
  GetViewParams(cxcsSelection, nil, nil, Result);
end;

procedure TcxSchedulerStyles.Changed(AIndex: Integer);
begin
  inherited Changed(AIndex);
  if GetOwner is TcxCustomScheduler then
  begin
    Scheduler.DateNavigator.CheckSizes;
    Scheduler.LookAndFeelChanged(Scheduler.LookAndFeel, []);
  end;
end;

function TcxSchedulerStyles.EventContentSelected(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := AEvent.Selected and Scheduler.CurrentView.EventContentSelected(AEvent);
  if Scheduler.EditController.IsEditing and (Scheduler.EditController.Event = AEvent) then
    Result := False;
end;

function TcxSchedulerStyles.GetDefaultContentColor(AResourceIndex: Integer): TColor;
begin
  Result := Resources[AResourceIndex].BackgroundColor;
  if Result = clDefault then
    Result := Painter.DefaultSchedulerContentColor(AResourceIndex);
  if Result = clDefault then
    Result := PainterHelper.GetResourceContentColor(AResourceIndex);
end;

function TcxSchedulerStyles.GetDefaultEventColor(AIsHeaderEvent: Boolean): TColor;
begin
  if SchedulerViewStyle = svsClassic then
    Result := Painter.DefaultSchedulerEventColorClassic(AIsHeaderEvent)
  else
    Result := Painter.DefaultSchedulerEventColor(AIsHeaderEvent);
end;

procedure TcxSchedulerStyles.GetDefaultHeaderViewParams(var AParams: TcxViewParams);
begin
  if SchedulerViewStyle = svsClassic then
  begin
    AParams.Color := Painter.DefaultHeaderColor;
    AParams.TextColor := Painter.DefaultHeaderTextColor;
  end
  else
  begin
    AParams.Color := Painter.DefaultSchedulerDayHeaderColor;
    AParams.TextColor := Painter.DefaultSchedulerDayHeaderTextColor;
  end
end;

procedure TcxSchedulerStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);

  procedure AssignTextParamsForNullDate(var ATextParams: TcxViewParams);
  begin
    with GetContentParams(NullDate) do
    begin
      ATextParams.TextColor := TextColor;
      ATextParams.Font := Font;
    end;
  end;

var
  AEvent: TcxSchedulerControlEvent;
  ASelected: Boolean;
begin
  AParams.Bitmap := nil;
  AParams.Font := Scheduler.Font;
  AParams.TextColor := AParams.Font.Color;
  AParams.Color := Scheduler.Color;
  case Index of
    cxcsDayHeader, cxcsResourceHeader:
      GetDefaultHeaderViewParams(AParams);

    cxcsBackground:
      begin
        AParams.Color := Painter.DefaultSchedulerBackgroundColor;
        AParams.TextColor := Painter.DefaultSchedulerTextColor;
      end;

    cxcsContent:
      begin
        AParams.TextColor := Painter.DefaultSchedulerViewTextColor;
        if AData <> nil then
          AParams.Color := GetDefaultContentColor(TcxSchedulerStorageResourceItem(AData).Index)
        else
          if SchedulerViewStyle = svsClassic then
            AParams.Color := Painter.DefaultSchedulerViewContentColorClassic
          else
            AParams.Color := Painter.DefaultSchedulerViewContentColor;
      end;

    cxcsSelection:
      begin
        AParams.Color := Painter.DefaultSelectionColor;
        if SchedulerViewStyle = svsClassic then
          AParams.TextColor := Painter.DefaultSchedulerViewSelectedTextColor
        else
          AParams.TextColor := Painter.DefaultSelectionTextColor;
      end;

    cxcsHSplitter, cxcsVSplitter:
      AParams.Color := Painter.DefaultSchedulerControlColor;

    cxcsGroupSeparator:
      AParams.Color := Painter.DefaultHeaderColor;

    cxcsEvent:
      if AData <> nil then
      begin
        AEvent := TcxSchedulerControlEvent(AData);
        ASelected := EventContentSelected(AEvent);
        if (SchedulerViewStyle = svsClassic) and ((AEvent.ActualLabelColor = clDefault) or ASelected) then
        begin
          if ASelected then
          begin
            with GetSelectionParams do
            begin
              AParams.Color := Color;
              AParams.TextColor := TextColor;
              AParams.Font := Font;
            end;
          end
          else
          begin
            AParams.Color := GetDefaultEventColor(AEvent.IsAllDayOrLonger);
            AssignTextParamsForNullDate(AParams);
          end;
        end
        else
        begin
          AParams.Color := AEvent.ActualLabelColor;
          if AParams.Color = clDefault then
            AParams.Color := GetDefaultEventColor(AEvent.IsAllDayOrLonger);
          AssignTextParamsForNullDate(AParams);
        end;
      end;
  end;
end;

function TcxSchedulerStyles.GetDayHeaderStyle: TcxStyle;
begin
  Result := DayHeader;
end;

function TcxSchedulerStyles.GetEventParams(AEvent: TcxSchedulerEvent): TcxViewParams;
begin
  Result := GetEventContentParams(AEvent);
end;

function TcxSchedulerStyles.GetResourceHeaderStyle: TcxStyle;
begin
  Result := ResourceHeader;
end;

function TcxSchedulerStyles.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Scheduler.LookAndFeelPainter;
end;

function TcxSchedulerStyles.GetPainterHelperClass: TcxSchedulerPainterHelperClass;
begin
  Result := Scheduler.PainterHelper
end;

function TcxSchedulerStyles.GetResources: TcxSchedulerStorageResourceItems;
begin
  Result := Scheduler.CurrentView.Resources;
end;

function TcxSchedulerStyles.GetSchedulerViewStyle: TcxSchedulerViewStyle;
begin
  Result := Scheduler.OptionsView.Style;
end;

{ TcxSchedulerBackground }

function TcxSchedulerBackground.IsSpecialPaint: Boolean;
begin
  Result := True;
end;

{ TcxSchedulerOptionsBehavior }

constructor TcxSchedulerOptionsBehavior.Create(AOwner: TcxCustomScheduler);
begin
  FOwner := AOwner;
  FHotTrack := True;
  FSelectOnRightClick := False;
end;

procedure TcxSchedulerOptionsBehavior.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerOptionsBehavior then
    with (Source as TcxSchedulerOptionsBehavior) do
    begin
      Self.ModalDialogs := ModalDialogs;
      Self.SelectOnRightClick := SelectOnRightClick;
      Self.HotTrack := HotTrack;
    end;
end;

function TcxSchedulerOptionsBehavior.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TcxCustomScheduler }

constructor TcxCustomScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogsLookAndFeel := TcxLookAndFeel.Create(Self);
  FDialogsLookAndFeel.MasterLookAndFeel := LookAndFeel;
  FListeners := TInterfaceList.Create;
  Color := clBtnFace;
  ParentFont := False;
  Font.Name := 'Tahoma';
  Keys := [kAll, kArrows, kChars, kTab];
  BeginUpdate;
  SetBounds(0, 0, ScaleFactor.Apply(cxDefaultSchedulerWidth), ScaleFactor.Apply(cxDefaultSchedulerHeight));
  FSubControls := TList.Create;
  FStoredClientBounds := ClientBounds;
  FResourceNavigator := CreateResourceNavigator;
  CreateSubClasses;
  FBackground := CreateBackground;
  FSubControlsCreated := True;
  BorderStyle := cxcbsDefault;
  EndUpdate;
  DragMode := dmAutomatic;
  cxFormatController.AddListener(Self);
  FOptionsBehavior := TcxSchedulerOptionsBehavior.Create(Self);
end;

destructor TcxCustomScheduler.Destroy;
begin
  cxFormatController.RemoveListener(Self);
  FSubControlsCreated := False;
  Storage := nil;
  DestroySubClasses;
  while SubControlCount > 0 do
    SubControls[0].Free;
  FSubControls.Free;
  NotifySchedulerRemoved;
  FListeners.Free;
  FreeAndNil(FDialogsLookAndFeel);
  FreeAndNil(FOptionsBehavior);
  inherited Destroy;
end;

procedure TcxCustomScheduler.BeginUpdate;
begin
  Inc(FLockCount);
  Include(FControlFlags, cfLocked);
  LockUpdateChanged(True);
end;

procedure TcxCustomScheduler.CopyToClipboard;
begin
  ClipboardController.Copy;
end;

procedure TcxCustomScheduler.CreateEventUsingDialog(AllDay: Boolean = False;
  ARecurrence: Boolean = False);
var
  AEvent: TcxSchedulerControlEvent;
  AModified: Boolean;
begin
  if not StorageActive then Exit;
  AEvent := EventList.CreateEvent(False);
  EditController.FFocused := True;
  try
    InitEventBySelection(AEvent, AllDay, ARecurrence, False);
    EventList.ValidateTimeBias(AEvent);
    InitEventBySelection(AEvent, AllDay, ARecurrence, False);
    if DoBeforeEditing(AEvent, False) then
    begin
      EventList.BeforeEditing(AEvent, False);
      cxShowEventEditorEx(GetEventEditInfo(AEvent, ARecurrence), AModified);
    end
  finally
    EditController.FFocused := False;
    AEvent.Free;
  end;
end;

procedure TcxCustomScheduler.CreateEventUsingInplaceEdit;
begin
  DoCreateEventUsingInplaceEdit;
end;

procedure TcxCustomScheduler.CutToClipboard;
begin
  ClipboardController.Cut;
end;

procedure TcxCustomScheduler.DeleteEvent(AEvent: TcxSchedulerControlEvent);
begin
  if not StorageActive or (AEvent = nil) or not EventOperations.Deleting or
    AEvent.ReadOnly then Exit;
  InternalDeleteEvent(AEvent, False);
end;

procedure TcxCustomScheduler.DeleteSelectedEvents(
  ACheckReadOnly: Boolean = True);
begin
  if StorageActive then
    InternalDeleteSelectedEvents(True, ACheckReadOnly);
end;

procedure TcxCustomScheduler.EditEventUsingDialog(
  AEvent: TcxSchedulerControlEvent; ACheckReadOnly: Boolean = True;
  AForcePatternEditing: Boolean = False);
var
  AReadOnly: Boolean;
  AModified: Boolean;
begin
  if (AEvent = nil) or not StorageActive or (cxEventEditorClass = nil) or
    not DoBeforeEditing(AEvent, False) then Exit;
  AReadOnly := ACheckReadOnly and
    (not CurrentView.Controller.CanEditEvent(AEvent, False) or AEvent.ReadOnly);
  EditController.FFocused := True;
  try
    EventList.BeforeEditing(AEvent, False);
    GetEventEditInfo(AEvent, False, AReadOnly);
    FEventEditInfo.ForcePatternEditing := AForcePatternEditing and AEvent.IsRecurring;
    cxShowEventEditorEx(FEventEditInfo, AModified);
  finally
    EditController.FFocused := False;
  end;
end;

procedure TcxCustomScheduler.EditEventUsingInplaceEdit(
  AEvent: TcxSchedulerControlEvent);
begin
  if (AEvent = nil) or not StorageActive then Exit;
  if CanFocusEx then
  begin
    SetFocus;
    EditController.FEditResource := nil;
    EditController.Activate(AEvent);
  end;
end;

procedure TcxCustomScheduler.DragDrop(
  Source: TObject; X, Y: Integer);
begin
  inherited DragDrop(Source, X, Y);
  CaptureController.DragDrop(Source, X, Y);
end;

procedure TcxCustomScheduler.EndUpdate;
begin
  LockUpdateChanged(False);
  Dec(FLockCount);
  if FLockCount = 0 then
  begin
    Exclude(FControlFlags, cfLocked);
    if cfInvalidLayout in FControlFlags then
    begin
      Exclude(FControlFlags, cfInvalidLayout);
      LayoutChanged;
    end
    else
      DateNavigator.RefreshDays;
  end;
end;

function TcxCustomScheduler.GoToDate(ADate: TDateTime): Boolean;
begin
  Result := False;
end;

function TcxCustomScheduler.GoToDate(ADate: TDateTime;
  AViewMode: TcxSchedulerViewMode): Boolean;
begin
  if ADate = NullDate then
    Result := False
  else
  begin
    FSelectedDays.Clear;
    FSelectedDays.Add(ADate);
    CurrentView.PeriodChanged;
    Result := True;
  end;
end;

procedure TcxCustomScheduler.FullRefresh;
begin
  if IsDestroying then Exit;
  Inc(FLockRefresh);
  try
    ClearAllCachedData;
    UpdateEventsCache(False);
    LayoutChanged;
  finally
    Dec(FLockRefresh);
  end;
end;

procedure TcxCustomScheduler.LayoutChanged;
begin
  if IsLocked or IsLoading or IsDestroying or not HandleAllocated then
    Include(FControlFlags, cfInvalidLayout)
  else
  begin
    if EditController <> nil then
      EditController.CloseEdit(True);
    if CurrentView.Visible and not CurrentView.CanShow then
      CurrentView.Visible := False;
    AlignSubControls;
    if not (cfViewValid in FControlFlags) then
    begin
      SynchronizeVisibleDays;
      UpdateEventsCache(False);
    end;
    DoLayoutChanged;
    Exclude(FControlFlags, cfInvalidLayout);
    if not FAligningSubControls then
      ControlBox.ApplyActualBounds;
    UpdateScrollBars;
    NotifySchedulerChanged;
  end;
  Invalidate;
end;

procedure TcxCustomScheduler.MakeEventVisible(
  AEvent: TcxSchedulerControlEvent; const ADate: TDateTime = NullDate;
  AResource: TcxSchedulerStorageResourceItem = nil);
var
  AEventDate: TDateTime;
begin
  if AResource = nil then
    AResource := AEvent.GetResourceItem;
  MakeResourceVisible(AResource);
  AEventDate := ADate;
  if AEventDate = NullDate then
    AEventDate := dxDateOf(AEvent.Start);
  CurrentView.MakeEventVisible(AEvent, AEventDate, AResource);
end;

procedure TcxCustomScheduler.MakeResourceVisible(
  AResource: TcxSchedulerStorageResourceItem);
var
  AVisibleIndex, I: Integer;
begin
  if (AResource = nil) or not AResource.Visible or
    (OptionsView.ResourcesPerPage = 0) then Exit;
  AVisibleIndex := -1;
  with Storage.Resources.ResourceItems do
  begin
    for I := 0 to VisibleResourceCount - 1 do
      if AResource = VisibleResources[I] then
      begin
        AVisibleIndex := I;
        Break;
      end;
  end;
  if AVisibleIndex < FirstVisibleResourceIndex then
    FirstVisibleResourceIndex := AVisibleIndex
  else
    if AVisibleIndex >= (FirstVisibleResourceIndex + OptionsView.ResourcesPerPage) then
       FirstVisibleResourceIndex := Max(AVisibleIndex - (OptionsView.ResourcesPerPage - 1), 0);
end;

procedure TcxCustomScheduler.PasteFromClipboard;
begin
  ClipboardController.Paste;
end;

procedure TcxCustomScheduler.SelectEvent(
  AEvent: TcxSchedulerControlEvent; AShift: TShiftState = []);
begin
  EventList.Selection.Add(AEvent, AShift);
  if not IsLocked then
    CurrentView.Refresh;
end;

procedure TcxCustomScheduler.SelectTime(const ASelStart,
  ASelFinish: TDateTime; AResource: TcxSchedulerStorageResourceItem);
begin
  if DateTimeHelper.RoundTime(Abs(ASelStart - ASelFinish)) <= CurrentView.GetTimeIncrement then
    ReplaceSelParams(ASelStart, ASelStart, AResource)
  else
    ReplaceSelParams(ASelStart, (ASelFinish - CurrentView.GetTimeIncrement), AResource);
  LayoutChanged;
end;

procedure TcxCustomScheduler.UnselectEvents;
begin
  if EventList.Selection.Count = 0 then Exit;
  EventList.Selection.Clear;
  CurrentView.LayoutChanged;
end;

procedure TcxCustomScheduler.RestoreFromIniFile(const AStorageName: string;
  ARestoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.RestoreFromIni(Self);
      if ARestoreResources and HasResources then
      begin
        Storage.InitRestore;
        try
          AStorage.RestoreFromIni(Storage.Resources);
        finally
          Storage.DoneRestore;
        end;
      end;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.RestoreFromRegistry(const AStorageName: string;
  ARestoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.RestoreFromRegistry(Self);
      if ARestoreResources and HasResources then
      begin
        Storage.InitRestore;
        try
          AStorage.RestoreFromRegistry(Storage.Resources);
        finally
          Storage.DoneRestore;
        end;
      end;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.RestoreFromStream(AStream: TStream;
  ARestoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStream);
  try
    BeginUpdate;
    try
      AStorage.RestoreFromStream(Self);
      if ARestoreResources and HasResources then
      begin
        Storage.InitRestore;
        try
          AStorage.RestoreFromStream(Storage.Resources);
        finally
          Storage.DoneRestore;
        end;
      end;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.StoreToIniFile(const AStorageName: string;
  AReCreate: Boolean = True; AStoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.ReCreate := ARecreate;
      AStorage.StoreToIni(Self);
      AStorage.ReCreate := False;
      if AStoreResources and HasResources then
        AStorage.StoreToIni(Storage.Resources);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.StoreToRegistry(const AStorageName: string;
  AReCreate: Boolean = True; AStoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStorageName);
  try
    BeginUpdate;
    try
      AStorage.ReCreate := ARecreate;
      AStorage.StoreToRegistry(Self);
      AStorage.ReCreate := False;
      if AStoreResources and HasResources then
        AStorage.StoreToRegistry(Storage.Resources);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.StoreToStream(AStream: TStream;
  AStoreResources: Boolean = True);
var
  AStorage: TcxStorage;
begin
  AStorage := TcxStorage.Create(AStream);
  try
    BeginUpdate;
    try
      AStorage.StoreToStream(Self);
      AStorage.ReCreate := False;
      if AStoreResources and HasResources then
        AStorage.StoreToStream(Storage.Resources);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomScheduler.TranslationChanged;
begin
  LayoutChanged;
end;

procedure TcxCustomScheduler.AlignControls(AControl: TControl; var Rect: TRect);
begin
end;

procedure TcxCustomScheduler.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Selection', ReadSelectionData, WriteSelectionData,
    (FSelectedDays.Count > 1) or (SelectedDays[0] <> Date));
end;

procedure TcxCustomScheduler.GetChildren(
  Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
  if ControlBox.Control <> nil then
    Proc(ControlBox.Control);
end;

procedure TcxCustomScheduler.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if not IsDestroying and FSubControlsCreated then
  begin
    if Operation = opRemove then
    begin
      if AComponent = ControlBox.Control then
      begin
        FCanModified := False;
        ControlBox.Control := nil;
      end;
      if AComponent = EventImages then
        EventImages := nil;
    end;
    ContentPopupMenu.Notification(AComponent, Operation);
    EventPopupMenu.Notification(AComponent, Operation);
    for I := 0 to SubControlCount - 1 do
      SubControls[I].Notification(AComponent, Operation);
  end;
end;

procedure TcxCustomScheduler.AddListener(
  AListener: IcxExternalDateNavigatorListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TcxCustomScheduler.NotifySchedulerChanged;
var
  I: Integer;
begin
  if FLockRefresh > 0 then Exit;
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IcxExternalDateNavigatorListener).SchedulerChanged;
end;

procedure TcxCustomScheduler.NotifySchedulerRemoved;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IcxExternalDateNavigatorListener).SchedulerRemoved;
end;

procedure TcxCustomScheduler.NotifyStorageChanged;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IcxExternalDateNavigatorListener).StorageChanged;
end;


procedure TcxCustomScheduler.RemoveListener(
  AListener: IcxExternalDateNavigatorListener);
begin
  FListeners.Remove(AListener);
end;

function TcxCustomScheduler.AllowCompositionPainting: Boolean;
begin
  Result := False;
end;

function TcxCustomScheduler.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

////////////////////////////// LAYOUT //////////////////////////////////////////

procedure TcxCustomScheduler.AlignSubControls(
  Sender: TcxSchedulerSubControl = nil);
begin
  if FAligningSubControls or IsLoading or not FSubControlsCreated then Exit;
  FAligningSubControls := True;
  try
    CheckSplittersVisibilityChanging;
    CalcLayout;
  finally
    FAligningSubControls := False;
  end;
end;

procedure TcxCustomScheduler.BoundsChanged;
begin
  FBoundsChanging := True;
  Include(FControlFlags, cfLocked);
  try
    if not (IsLoading or IsDestroying) and EditController.IsEditing then
      CurrentView.Controller.CloseInplaceEdit;
    inherited BoundsChanged;
    AlignSubControls;
    FPrevBounds := ClientBounds;
  finally
    Exclude(FControlFlags, cfLocked);
    LayoutChanged;
    FBoundsChanging := False;
  end;
end;

procedure TcxCustomScheduler.CalcVerticalSplitterBounds;
var
  R: TRect;
begin
  R := ClientBounds;
  case OptionsView.ViewPosition of
    vpRight:
      R := cxRectSetLeft(R, R.Left + DateNavigator.Width, OptionsView.VertSplitterWidth);
    vpLeft:
      R := cxRectSetRight(R, R.Right - DateNavigator.Width, OptionsView.VertSplitterWidth);
    vpTop:
      begin
        R.Top := R.Bottom - DateNavigator.Height;
        R := cxRectSetLeft(R, R.Left + DateNavigator.Width, OptionsView.VertSplitterWidth);
      end;
    vpBottom:
      begin
        R.Bottom := R.Top + DateNavigator.Height;
        R := cxRectSetLeft(R, R.Left + DateNavigator.Width, OptionsView.VertSplitterWidth);
      end;
  end;
  FVertSplitter.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CalcLayout;
begin
  CalcSplittersBounds;
  case OptionsView.ViewPosition of
    vpRight: CalcLayoutViewRight;
    vpLeft: CalcLayoutViewLeft;
    vpTop: CalcLayoutViewTop;
    vpBottom: CalcLayoutViewBottom;
  end;
end;

procedure TcxCustomScheduler.CalcHorizontalSplitterBounds;
var
  R: TRect;
begin
  R := ClientBounds;
  case OptionsView.ViewPosition of
    vpRight:
      begin
        R.Right := R.Left + DateNavigator.Width;
        R := cxRectSetTop(R, R.Top + DateNavigator.Height, OptionsView.HorzSplitterWidth);
      end;
    vpLeft:
      begin
        R.Left := R.Right - DateNavigator.Width;
        R := cxRectSetTop(R, R.Top + DateNavigator.Height, OptionsView.HorzSplitterWidth);
      end;
    vpTop:
      R := cxRectSetBottom(R, R.Bottom - DateNavigator.Height, OptionsView.HorzSplitterWidth);
    vpBottom:
      R := cxRectSetTop(R, R.Top + DateNavigator.Height, OptionsView.HorzSplitterWidth);
  end;
  FHorzSplitter.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CalcSplittersBounds;
begin
  if IsVertSplitterVisible then
    CalcVerticalSplitterBounds;
  if IsHorzSplitterVisible then
    CalcHorizontalSplitterBounds;
end;

procedure TcxCustomScheduler.CalcLayoutViewRight;
var
  R: TRect;
begin
  R := ClientBounds;
  if IsVertSplitterVisible then
  begin
    if DateNavigator.Visible then
    begin
      R.Right := R.Left + DateNavigator.Width;
      if IsHorzSplitterVisible then
        R.Bottom := R.Top + DateNavigator.Height;
      DateNavigator.Bounds := GetActualBiDiModeRect(R);
    end;
    if ControlBox.Visible then
    begin
      R.Bottom := ClientBounds.Bottom;
      R.Right := R.Left + DateNavigator.Width;
      if IsHorzSplitterVisible then
        R.Top := DateNavigator.Bottom + OptionsView.HorzSplitterWidth;
      ControlBox.Bounds := GetActualBiDiModeRect(R);
    end;
    R.Top  := ClientBounds.Top;
    R.Left := R.Right + OptionsView.VertSplitterWidth;
    R.BottomRight := ClientBounds.BottomRight;
  end;
  CurrentView.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CalcLayoutViewLeft;
var
  R: TRect;
begin
  R := ClientBounds;
  if IsVertSplitterVisible then
  begin
    if DateNavigator.Visible then
    begin
      R.Left := R.Right - DateNavigator.Width;
      if IsHorzSplitterVisible then
        R.Bottom := R.Top + DateNavigator.Height;
      DateNavigator.Bounds := GetActualBiDiModeRect(R);
    end;
    if ControlBox.Visible then
    begin
      R.Bottom := ClientBounds.Bottom;
      R.Left := R.Right - DateNavigator.Width;
      if IsHorzSplitterVisible then
        R.Top := DateNavigator.Bottom + OptionsView.HorzSplitterWidth;
      ControlBox.Bounds := GetActualBiDiModeRect(R);
    end;
    R.TopLeft  := ClientBounds.TopLeft;
    R.Right := ClientBounds.Right - (OptionsView.VertSplitterWidth + DateNavigator.Width);
    R.Bottom := ClientBounds.Bottom;
  end;
  CurrentView.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CalcLayoutViewTop;
var
  R: TRect;
begin
  R := ClientBounds;
  if IsHorzSplitterVisible then
  begin
    R.Top := R.Bottom - DateNavigator.Height;
    if DateNavigator.Visible then
    begin
      if IsVertSplitterVisible then
        R.Right := R.Left + DateNavigator.Width;
      DateNavigator.Bounds := GetActualBiDiModeRect(R);
    end;
    if ControlBox.Visible then
    begin
      R.Right := ClientBounds.Right;
      if IsVertSplitterVisible then
        R.Left := ClientBounds.Left + DateNavigator.Width + OptionsView.VertSplitterWidth
      else
        R.Left := ClientBounds.Left;
      ControlBox.Bounds := GetActualBiDiModeRect(R);
    end;
    R.TopLeft := ClientBounds.TopLeft;
    R.Bottom  := ClientBounds.Bottom - (DateNavigator.Height + OptionsView.HorzSplitterWidth);
    R.Right := ClientBounds.Right;
  end;
  CurrentView.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CalcLayoutViewBottom;
var
  R: TRect;
begin
  R := ClientBounds;
  if IsHorzSplitterVisible then
  begin
    R.Bottom := R.Top + DateNavigator.Height;
    if DateNavigator.Visible then
    begin
      if IsVertSplitterVisible then
        R.Right := R.Left + DateNavigator.Width;
      DateNavigator.Bounds := GetActualBiDiModeRect(R);
    end;
    if ControlBox.Visible then
    begin
      R.Right := ClientBounds.Right;
      if IsVertSplitterVisible then
        R.Left := ClientBounds.Left + DateNavigator.Width + OptionsView.VertSplitterWidth
      else
        R.Left := ClientBounds.Left;
      ControlBox.Bounds := GetActualBiDiModeRect(R);
    end;
    R.Left := ClientBounds.Left;
    R.Top  := R.Bottom + OptionsView.HorzSplitterWidth;
    R.BottomRight := ClientBounds.BottomRight;
  end;
  CurrentView.Bounds := GetActualBiDiModeRect(R);
end;

procedure TcxCustomScheduler.CheckHorzSplitterBounds;
var
  R: TRect;
begin
  //todo:
  R := FHorzSplitter.Bounds;
  if IsVertSplitterVisible then
  begin
    if not UseRightToLeftAlignment then
      if IsViewAtLeft then
      begin
        R.Left := FVertSplitter.Right;
        R.Right := ClientBounds.Right;
      end
      else
      begin
        R.Left := ClientBounds.Left;
        R.Right := FVertSplitter.Left;
      end
    else
      if IsViewAtLeft then
      begin
        R.Left := ClientBounds.Left;
        R.Right := FVertSplitter.Left;
      end
      else
      begin
        R.Left := FVertSplitter.Right;
        R.Right := ClientBounds.Right;
      end
  end
  else
  begin
    R.Left := ClientBounds.Left;
    R.Right := ClientBounds.Right;
  end;
  FHorzSplitter.Bounds := cxRectSetHeight(R, OptionsView.HorzSplitterWidth);
end;

procedure TcxCustomScheduler.CheckSplittersVisibilityChanging; //1
begin
  FVertSplitterShowing := not FVertSplitter.Visible and IsVertSplitterVisible;
  if FVertSplitterShowing then
    UpdateControlsBoundsOnVertSplitterShowing;
  FVertSplitter.Visible := IsVertSplitterVisible;

  FHorzSplitterShowing := not FHorzSplitter.Visible and IsHorzSplitterVisible;
  if FHorzSplitterShowing then
    UpdateControlsBoundsOnHorzSplitterShowing;
  FHorzSplitter.Visible := IsHorzSplitterVisible;
end;

procedure TcxCustomScheduler.ClearAllCachedData;
var
  I: Integer;
begin
  for I := 0 to SubControlCount - 1 do
    SubControls[I].ClearCachedData;
end;

function TcxCustomScheduler.IsHorzSplitterVisible: Boolean; //1
begin
  if OptionsView.ViewPosition in [vpLeft, vpRight] then
    Result := DateNavigator.Visible and ControlBox.Visible
  else
    Result := DateNavigator.Visible or ControlBox.Visible;
end;

function TcxCustomScheduler.IsVertSplitterVisible: Boolean; //1
begin
  if OptionsView.ViewPosition in [vpLeft, vpRight] then
    Result := CurrentView.Visible and (DateNavigator.Visible or ControlBox.Visible)
  else
    Result := DateNavigator.Visible and ControlBox.Visible;
end;

procedure TcxCustomScheduler.UpdateControlsBoundsOnHorzSplitterShowing;
var
  R: TRect;
begin
  //todo:
  CheckHorzSplitterBounds;
  case OptionsView.ViewPosition of
    vpRight, vpLeft:
      begin
        R := DateNavigator.Bounds;
        R.Top := ClientBounds.Top;
        R.Bottom := FHorzSplitter.Top;
        DateNavigator.Bounds := R;
        R.Top := FHorzSplitter.Bottom;
        R.Bottom := ClientBounds.Bottom;
        ControlBox.Bounds := R;
      end;
    vpTop: ;
    vpBottom:
      begin
        R := ClientBounds;
        R.Bottom := FHorzSplitter.Bottom;
        if not UseRightToLeftAlignment then
        begin
          R.Right := FVertSplitter.Left;
          DateNavigator.Bounds := R;
          R.Left := FVertSplitter.Right;
          R.Right := ClientBounds.Right;
        end
        else
        begin
          R.Left := FVertSplitter.Right;
          DateNavigator.Bounds := R;
          R.Left := ClientBounds.Left;
          R.Right := FVertSplitter.Left;
        end;
        ControlBox.Bounds := R;
      end;
  end;
end;

procedure TcxCustomScheduler.UpdateControlsBoundsOnVertSplitterShowing;
var
  R: TRect;
begin
  //todo:
  CheckHorzSplitterBounds;
  FVertSplitter.Width := OptionsView.VertSplitterWidth;
  R := ClientBounds;
  if not UseRightToLeftAlignment then
    if IsViewAtLeft then
    begin
      R.Right := FVertSplitter.Left;
      CurrentView.Bounds := R;
      R := DateNavigator.Bounds;
      R.Left := FVertSplitter.Right;
      R.Right := ClientBounds.Right;
    end
    else
    begin
      R.Left := FVertSplitter.Right;
      CurrentView.Bounds := R;
      R := DateNavigator.Bounds;
      R.Left := ClientBounds.Left;
      R.Right := FVertSplitter.Left;
    end
  else
    if IsViewAtLeft then
    begin
      R.Left := FVertSplitter.Right;
      CurrentView.Bounds := R;
      R := DateNavigator.Bounds;
      R.Left := ClientBounds.Left;
      R.Right := FVertSplitter.Left;
    end
    else
    begin
      R.Right := FVertSplitter.Left;
      CurrentView.Bounds := R;
      R := DateNavigator.Bounds;
      R.Left := FVertSplitter.Right;
      R.Right := ClientBounds.Right;
    end;
  DateNavigator.Bounds := R;
  R.Top := ControlBox.Bounds.Top;
  R.Bottom := ClientBounds.Bottom;
  ControlBox.Bounds := R;
end;

///////////////////////////////////////////////////////////////////////////////////////////

function TcxCustomScheduler.CanSelectPeriod: Boolean;
begin
  Result := True;
end;

function TcxCustomScheduler.CanShowEventDialog: Boolean;
begin
  Result := CurrentView.Controller.CanShowEventDialog;
end;

function TcxCustomScheduler.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TcxCustomScheduler.CanDeactivateOnSelectionChanged(AView: TcxSchedulerCustomView): Boolean;
begin
  Result := (AView <> nil) and AView.CanDeactivateOnDateNavigatorSelectionChange;
end;

function TcxCustomScheduler.CanIntersect(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := EventOperations.Intersection or
    not AEvent.Conflicts(OptionsView.ShowEventsWithoutResource or not HasResources);
end;

function TcxCustomScheduler.CanModified: Boolean;
begin
  Result := FCanModified;
end;

procedure TcxCustomScheduler.CheckEventListTimeRangeUsing;
begin
  EventList.UseTimeRange := CurrentView.CheckEventsVisibility;
end;

function TcxCustomScheduler.CreateBackground: TcxSchedulerSubControl;
begin
  Result := TcxSchedulerBackground.Create(Self);
end;

function TcxCustomScheduler.CreateClipboardController: TcxSchedulerClipboardController;
begin
  Result := TcxSchedulerClipboardController.Create(Self);
end;

function TcxCustomScheduler.CreateContentPopupMenu: TcxSchedulerContentPopupMenu;
begin
  Result := TcxSchedulerContentPopupMenu.Create(Self);
end;

function TcxCustomScheduler.CreateControlBox: TcxSchedulerControlBox;
begin
  Result := TcxSchedulerControlBox.Create(Self)
end;

function TcxCustomScheduler.CreateDateNavigator: TcxSchedulerCustomDateNavigator;
begin
  Result := nil;
end;

function TcxCustomScheduler.CreateDefaultView: TcxSchedulerCustomView;
begin
  Result := nil;
end;

function TcxCustomScheduler.CreateDefaultEditProperties: TcxCustomEditProperties;
begin
  Result := TcxMemoProperties.Create(Self);
end;

function TcxCustomScheduler.CreateEditController: TcxSchedulerEditController;
begin
  Result := TcxSchedulerEditController.Create(Self);
end;

function TcxCustomScheduler.CreateEventHitTestController: TcxSchedulerEventHitTestController;
begin
  Result := TcxSchedulerEventHitTestController.Create(Self);
end;

function TcxCustomScheduler.CreateEventList: TcxSchedulerCachedEventList;
begin
  Result := TcxSchedulerCachedEventList.Create;
  Result.Selection.OnEventSelectionChanged := DoEventSelectionChanged;
end;

function TcxCustomScheduler.CreateEventOperations: TcxSchedulerEventOperations;
begin
  Result := TcxSchedulerEventOperations.Create(Self);
end;

function TcxCustomScheduler.CreateEventPopupMenu: TcxSchedulerEventPopupMenu;
begin
  Result := TcxSchedulerEventPopupMenu.Create(Self);
end;

function TcxCustomScheduler.CreateHintController: TcxSchedulerHintController;
begin
  Result := TcxSchedulerHintController.Create(Self);
end;

function TcxCustomScheduler.CreateOptionsCustomize: TcxSchedulerOptionsCustomize;
begin
  Result := TcxSchedulerOptionsCustomize.Create(Self);
end;

function TcxCustomScheduler.CreateOptionsView: TcxSchedulerOptionsView;
begin
  Result := TcxSchedulerOptionsView.Create(Self);
end;

function TcxCustomScheduler.CreateResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := TcxSchedulerResourceNavigator.Create(Self);
end;

function TcxCustomScheduler.CreateSplitter(
  AKind: TcxSchedulerSplitterKind): TcxSchedulerSplitter;
begin
  Result := TcxSchedulerSplitter.Create(Self);
  Result.SetKind(AKind);
end;

function TcxCustomScheduler.CreateStyles: TcxSchedulerStyles;
begin
  Result := TcxSchedulerStyles.Create(Self);
end;

procedure TcxCustomScheduler.CreateSubClasses;
begin
  FTabOrdersList := TcxSchedulerEventList.Create;
  FClipboardController := CreateClipboardController;
  FEventEditInfo := TcxSchedulerEventEditInfo.Create;
  FTextEditProperties := TcxTextEditProperties.Create(Self);
  FEventHitTestController := CreateEventHitTestController;
  FHintController := CreateHintController;
  FEditController := CreateEditController;
  FEditStyle := TcxCustomEditStyle.Create(Self, True);
  FDefaultProperties := TcxMemoProperties.Create(Self);
  FEventList := CreateEventList;

  FEventDays := TcxSchedulerDateList.Create;
  FHolidayDays := TcxSchedulerDateList.Create;
  FSelectedDays := TcxSchedulerDateList.Create;
  FSelectedDays.Add(Date);
  FStyles := CreateStyles;
  FEventOperations := CreateEventOperations;
  FOptionsCustomize := CreateOptionsCustomize;
  FOptionsView := CreateOptionsView;
  // SubControls
  FControlBox := CreateControlBox;
  FDateNavigator := CreateDateNavigator;
  // this order
  FVertSplitter := CreateSplitter(skVertical);
  FHorzSplitter := CreateSplitter(skHorizontal);
  // view
  FCurrentView := CreateDefaultView;
  FCurrentView.Visible := True;
  // popups
  FContentPopupMenu := CreateContentPopupMenu;
  FEventPopupMenu := CreateEventPopupMenu;
  CreateUpdateTimeTimer;
end;

procedure TcxCustomScheduler.DateNavigatorSelectionChanged;
begin
  if not (cfViewValid in FControlFlags) then Exit;
  Exclude(FControlFlags, cfViewValid);
  PeriodChanged;
  Modified;
end;

procedure TcxCustomScheduler.DblClick;
var
  P: TPoint;
begin
  P := ScreenToClient(GetMouseCursorPos);
  if FActiveControl <> nil then
    FActiveControl.MousePositionChanged(P.X, P.Y);
  inherited DblClick;
end;

procedure TcxCustomScheduler.DestroySubClasses;
begin
  FTabOrdersList.Free;
  FClipboardController.Free;
  FEditController.Free;
  FEventHitTestController.Free;
  FUpdateTimeTimer.Free;
  FContentPopupMenu.Free;
  FEventPopupMenu.Free;
  FEventOperations.Free;
  FEventList.Free;
  FEventDays.Free;
  FHolidayDays.Free;
  FOptionsCustomize.Free;
  FOptionsView.Free;
  FStyles.Free;
  FSelectedDays.Free;
  FEditStyle.Free;
  FDefaultProperties.Free;
  FTextEditProperties.Free;
  FHintController.Free;
  FEventEditInfo.Free;
end;

procedure TcxCustomScheduler.DoAfterDragEvent(Target: TObject;
  X, Y: Integer; var Accept: Boolean);
begin
  if Assigned(FOnAfterDragEvent) then
    FOnAfterDragEvent(Self, Target, X, Y, Accept);
end;

procedure TcxCustomScheduler.DoAfterEditing(AEvent: TcxSchedulerControlEvent);
begin
  if Assigned(FOnAfterEditing) then
    FOnAfterEditing(Self, AEvent);
end;

procedure TcxCustomScheduler.DoAfterSizingEvent(
  AEvent: TcxSchedulerControlEvent; X, Y: Integer; var Accept: Boolean);
begin
  if Assigned(FOnAfterSizingEvent) then
    FOnAfterSizingEvent(Self, AEvent, X, Y, Accept);
end;

function TcxCustomScheduler.DoBeforeDeleting(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeDeleting) then
    FOnBeforeDeleting(Self, AEvent, Result);
end;

function TcxCustomScheduler.DoBeforeDragEvent(
  AEvent: TcxSchedulerControlEvent; X, Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeDragEvent) then
    FOnBeforeDragEvent(Self, AEvent, X, Y, Result);
end;

function TcxCustomScheduler.DoBeforeEditing(
  AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean;
begin
  Result := CurrentView.CanEventEdit(AEvent, AInplace);
  if Result and Assigned(FOnBeforeEditing) then
    FOnBeforeEditing(Self, AEvent, AInplace, Result);
end;

function TcxCustomScheduler.DoBeforeSizingEvent(
  AEvent: TcxSchedulerControlEvent; X, Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeSizingEvent) then
    FOnBeforeSizingEvent(Self, AEvent, X, Y, Result);
end;

procedure TcxCustomScheduler.DoCalculateLayout(
  AControl: TcxSchedulerSubControl);
begin
  AControl.LayoutChanged;
end;

procedure TcxCustomScheduler.DoCancelMode;
begin
  if FCaptureControl <> nil then
    FCaptureControl.Controller.DoCancelMode;
  FCaptureControl := nil;
end;

procedure TcxCustomScheduler.DoControllerReset(
  AControl: TcxSchedulerSubControl);
begin
  AControl.Controller.Reset;
end;

procedure TcxCustomScheduler.DoCreateEventUsingInplaceEdit(
  AKey: Char = #0);
var
  AControlEvent: TcxSchedulerControlEvent;
begin
  if not StorageActive then Exit;
  AControlEvent := EventList.CreateEvent(True);
  InitEventBySelection(AControlEvent, False, False, True);
  EventList.ValidateTimeBias(AControlEvent);
  InitEventBySelection(AControlEvent, False, False, True);
  if AControlEvent.Source <> nil then
    AControlEvent.MoveTo(AControlEvent.Start + GetTimeBias(AControlEvent.Start));
  EventList.Sort(@cxCompareSchedulerControlEvents);
  CurrentView.Refresh;
  EditController.Init(SelStart, SelResource, True);
  if AKey = #0 then
    EditController.Activate(AControlEvent)
  else
    EditController.Activate(AControlEvent, AKey);
  if not EditController.IsEditing then
    EditController.DeleteEvent(AControlEvent);
end;

procedure TcxCustomScheduler.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  CurrentView.DoCreateScrollBars;
end;

procedure TcxCustomScheduler.DoDestroyScrollBars;
begin
  inherited DoDestroyScrollBars;
  CurrentView.DoDestroyScrollBars;
end;

procedure TcxCustomScheduler.DoEventSelectionChanged(
  AEvent: TcxSchedulerControlEvent);
begin
  if Assigned(FOnEventSelectionChanged) then
    FOnEventSelectionChanged(Self, AEvent);
end;

procedure TcxCustomScheduler.DoFirstVisibleResourceChanged;
begin
  if Assigned(FOnFirstVisibleResourceChanged) then
    FOnFirstVisibleResourceChanged(Self);
end;

procedure TcxCustomScheduler.DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer);
begin
  Scroll(AScrollKind, scPosition, ANewScrollPos);
//      if AScrollBar.Kind <> sbHorizontal then
//        if ADeltaY > 0 then
//          AScrollCode := scLineUp
//        else
//          AScrollCode := scLineDown
//      else
//        if ADeltaX > 0 then
//          AScrollCode := scLineUp
//        else
//          AScrollCode := scLineDown;
//      for I := 0 to Abs(ANewDataPos - AScrollBar.Position) - 1 do
//      begin
//        ANewDataPos := AScrollBar.Position;
//        Scroll(AScrollBar.Kind, AScrollCode, ANewDataPos);
//        Update;
//      end;
end;

procedure TcxCustomScheduler.DoGetEventDisplayText(AEvent: TcxSchedulerControlEvent; var AText: string);
begin
  AText := AEvent.Caption;
  if Assigned(FOnGetEventDisplayText) then
    FOnGetEventDisplayText(Self, AEvent, AText);
end;

procedure TcxCustomScheduler.DoGetEventEditProperties(
  AEvent: TcxSchedulerControlEvent; var AProperties: TcxCustomEditProperties);
begin
  if CurrentView.GetEditWithSingleLineEditor(AEvent) then
    AProperties := FTextEditProperties
  else
    AProperties := DefaultProperties;
  if Assigned(OnGetEventEditProperties) then
  begin
    OnGetEventEditProperties(Self, AEvent, AProperties);
    if AProperties = nil then
      AProperties := DefaultProperties;
  end;
end;

procedure TcxCustomScheduler.DoHitTestRecalculate(AControl: TcxSchedulerSubControl);
begin
  AControl.Controller.HitTest.Recalculate;
end;

procedure TcxCustomScheduler.DoInitEdit(AEdit: TcxCustomEdit);
begin
  if Assigned(FOnInitEdit) then
    FOnInitEdit(Self, AEdit);
end;

function TcxCustomScheduler.DoIsWorkTime(
  AResourceItem: TcxSchedulerStorageResourceItem; const ADateTime: TDateTime): Boolean;
begin
  Result := OptionsView.IsWorkTime(AResourceItem, ADateTime);
  if Assigned(FOnIsWorkTime) then
    FOnIsWorkTime(Self, AResourceItem, ADateTime, Result);
end;

procedure TcxCustomScheduler.DoLayoutChanged;
var
  I: Integer;
begin
  for I := 0 to FSubControls.Count - 1 do
    SubControls[I].LayoutChanged;
  UpdateScrollBars;
  DoLayoutChangedEvent;
end;

procedure TcxCustomScheduler.DoLayoutChangedEvent;
begin
  if Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
end;

function TcxCustomScheduler.DoMoreEventsButtonClick: Boolean;
begin
  Result := False;
  if Assigned(FOnMoreEventsButtonClick) then
    FOnMoreEventsButtonClick(Self, Result);
end;

function TcxCustomScheduler.DoNavigationButtonClick(AnInterval: TDateTime;
  AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  Result := False;
  if Assigned(FOnNavigationButtonClick) then
    FOnNavigationButtonClick(Self, AnInterval, AResource, Result);
end;

procedure TcxCustomScheduler.DoCanShowView(
  AView: TcxSchedulerCustomView; var Allow: Boolean);
begin
  if Assigned(FOnCanShowView) then
    FOnCanShowView(Self, AView, Allow);
end;

procedure TcxCustomScheduler.DoSelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TcxCustomScheduler.DoScaleScroll;
begin
  if Assigned(OnScaleScroll) then
    with CurrentView do
      OnScaleScroll(Self, GetFirstVisibleDate + GetFirstVisibleTime,
        GetLastVisibleDate + GetLastVisibleTime);
end;

function TcxCustomScheduler.DoShowDateHint(const ADate: TDateTime; var AHintText: string): Boolean;
var
  AOnShowDateHint: TcxSchedulerShowDateHintEvent;
begin
  Result := False;
  if not FOptionsView.ShowHints then
    Exit;
  Result := (Storage <> nil) and Storage.GetHolidayNamesByDate(ADate, AHintText);
  AOnShowDateHint := OnShowDateHint;
  if Assigned(AOnShowDateHint) then
    AOnShowDateHint(Self, ADate, AHintText, Result);
  Result := Result and (AHintText <> '');
end;

function TcxCustomScheduler.DoShowPopupMenu(AMenu: TComponent;
  X, Y: Integer): Boolean;
begin
  with CurrentView do
    Result := Visible and cxRectPtIn(ViewInfo.Bounds, ScreenToClient(cxPoint(X, Y))) and
      DoShowPopupMenu(X, Y);
  if not Result then
    Result := inherited DoShowPopupMenu(AMenu, X, Y);
end;

procedure TcxCustomScheduler.DoViewTypeChanged(
  ANewView: TcxSchedulerCustomView);
begin
  if Assigned(FOnViewTypeChanged) then
    FOnViewTypeChanged(Self, FCurrentView, ANewView);
end;

procedure TcxCustomScheduler.DoUpdateTime;
begin
  if not FSubControlsCreated or not HandleAllocated then Exit;
  if CurrentView.Visible then
    CurrentView.DateChanged;
  if DateNavigator.Visible then
    DateNavigator.CheckCurrentDate;
end;

procedure TcxCustomScheduler.DragCanceled;
begin
  FIsDragCanceled := True;
end;

procedure TcxCustomScheduler.DrawSplitters;
begin
  if OptionsView.ViewPosition in [vpLeft, vpRight] then
  begin
    PaintControl(FVertSplitter, True);
    PaintControl(FHorzSplitter, True);
  end
  else
  begin
    PaintControl(FHorzSplitter, True);
    PaintControl(FVertSplitter, True);
  end;
end;

procedure TcxCustomScheduler.FirstVisibleResourceChanged;
begin
  ResourceNavigator.FirstVisibleResourceChanged;
  DoFirstVisibleResourceChanged;
end;

procedure TcxCustomScheduler.FontChanged;
begin
  BeginUpdate;
  if not IsLoading and FSubControlsCreated then
    DateNavigator.SetIntegralSizes;
  inherited FontChanged;
  LayoutChanged;
  EndUpdate;
end;

function TcxCustomScheduler.GetActualBiDiModeRect(const R: TRect): TRect;
begin
  Result := R;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientRect);
end;

function TcxCustomScheduler.GetClientBounds: TRect;
begin
  Result := GetBounds;
  InflateRect(Result, -BorderSize, -BorderSize);
end;

procedure TcxCustomScheduler.FocusChanged;
begin
  if IsDestroying then Exit;
  if OptionsView.HideSelection and not EditController.Focused then
  begin
    if (CurrentView <> nil) and CurrentView.Visible then
      CurrentView.Refresh;
    if DateNavigator.Visible then
      DateNavigator.Refresh;
  end;
  inherited FocusChanged;
  if not Focused then
    HintController.HideHint;
end;

function TcxCustomScheduler.GetControlFromPoint(
  const APoint: TPoint): TcxSchedulerSubControl;
var
  I: Integer;
begin
  Result := Background;
  if IsScrollBarsArea(APoint) then
    Exit;
  for I := 0 to SubControlCount - 1 do
    if SubControls[I].CanCapture(APoint) then
    begin
      Result := SubControls[I];
      Break;
    end;
end;

function TcxCustomScheduler.GetCurrentCursor(X, Y: Integer): TCursor;
var
  AControl: TcxSchedulerSubControl;
begin
  Result := crDefault;
  if not IsDesigning and ([cfInvalidLayout, cfLocked] * FControlFlags = []) then
  begin
    AControl := GetControlFromPoint(cxPoint(X, Y));
    Result := AControl.Controller.GetCursor(X - AControl.Left, Y - AControl.Top);
  end;
  if Result = crDefault then
    Result := inherited GetCurrentCursor(X, Y);
end;

function TcxCustomScheduler.GetDateTimeHelper: TcxSchedulerDateTimeHelperClass;
begin
  Result := cxSchedulerUtils.DateTimeHelper;
end;

function TcxCustomScheduler.GetDateNavigatorLoadedSize: TSize;
var
  AStoredVertSplitterBounds: TRect;
begin
  AStoredVertSplitterBounds := VertSplitter.Bounds;
  if UseRightToLeftAlignment then
    AStoredVertSplitterBounds := TdxRightToLeftLayoutConverter.ConvertRect(AStoredVertSplitterBounds, FStoredClientBounds);
  if IsViewAtLeft then
    Result.cx := FStoredClientBounds.Right - AStoredVertSplitterBounds.Right
  else
    Result.cx := AStoredVertSplitterBounds.Left - FStoredClientBounds.Left;
  if IsViewAtTop then
    Result.cy := FStoredClientBounds.Bottom - HorzSplitter.Bottom
  else
    Result.cy := HorzSplitter.Top -  FStoredClientBounds.Top;
end;

function TcxCustomScheduler.GetDesignHitTest(
  X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift);
  if not Result then
  begin
    Result := Capture.AllowDesignHitTest(X, Y, Shift);
    if not Result then
      Result := GetControlFromPoint(cxPoint(X, Y)).AllowDesignHitTest(X, Y, Shift);
  end;
end;

function TcxCustomScheduler.GetEventEditInfo(AEvent: TcxSchedulerControlEvent;
  ARecurrence: Boolean = False; AReadOnly: Boolean = False): TcxSchedulerEventEditInfo;
begin
  Result := FEventEditInfo;
  Result.Event := AEvent;
  Result.OnAfterEditingProc := DoAfterEditing;
  Result.AllowHiddenEvents := OptionsView.ShowEventsWithoutResource or not HasResources;
  Result.LookAndFeel := DialogsLookAndFeel;
  Result.Intersection := EventOperations.Intersection;
  Result.ReadOnly := AReadOnly;
  Result.RecurrenceButton := EventOperations.Recurrence;
  Result.Recurrence := ARecurrence;
  Result.ForcePatternEditing := ARecurrence;
  Result.BiasTime := GetTimeBias(AEvent.Start);
  Result.ShowResources := EventOperations.SharingBetweenResources and AEvent.Storage.IsEventSharingAllowed;
  Result.ShowTaskComplete := CurrentView.ShowTaskComplete;
  Result.ShowModal := OptionsBehavior.ModalDialogs;
  Result.AllowDelete := EventOperations.Deleting;
  Result.DisableShare := not AEvent.Storage.IsEventSharingAllowed;
  Result.OnDeleteFunc := DoBeforeDeleting;
  Result.DialogsStyle := DialogsStyle;
  Result.IsRemindersActive := AEvent.Storage.IsReminderAvailable and
    AEvent.Storage.Reminders.Active;
end;

function TcxCustomScheduler.GetEventHintText(
  AEvent: TcxSchedulerControlEvent): string;
begin
  Result := AEvent.Caption;
  if not AEvent.AllDayEvent then
  begin
    with DateTimeHelper do
      Result := TimeToStr(AEvent.Start) + '-' + TimeToStr(AEvent.Finish) + ' ' + Result;
  end;
  GetEventUserHintText(AEvent, Result);
end;

function TcxCustomScheduler.GetEventModernStyleHintInfo(AEvent: TcxSchedulerControlEvent): TcxSchedulerEventModernStyleHintInfo;
begin
  Result := TcxSchedulerEventModernStyleHintInfo.Create;
  Result.Visible := True;
  DoGetEventDisplayText(AEvent, Result.Caption);
  Result.Location := AEvent.Location;
  Result.TaskComplete := AEvent.TaskComplete;
  Result.Reminder := cxGetResourceString(@scxModernStyleHintReminderNone);
  if AEvent.Reminder then
    Result.Reminder := cxMinutesToText(AEvent.ReminderMinutesBeforeStart);
  Result.Start := Format('%s  %s', [DateToStr(Int(AEvent.Start)), TcxSchedulerDateTimeHelper.TimeToStr(AEvent.Start)]);
  Result.Finish := Format('%s  %s', [DateToStr(Int(AEvent.Finish)), TcxSchedulerDateTimeHelper.TimeToStr(AEvent.Finish)]);
  Result.ShowLocation := Result.Location <> '';
  Result.ShowTaskComplete := CurrentView.ShowTaskComplete;
  Result.ShowReminder := True;
  Result.ShowStart := True;
  Result.ShowFinish := True;
  GetEventUserModernStyleHintInfo(AEvent, Result);
end;

function TcxCustomScheduler.GetEventUserHintText(AEvent: TcxSchedulerControlEvent;
  var AText: string): Boolean;
begin
  Result := Assigned(FOnGetEventHintText);
  if Result then
    FOnGetEventHintText(Self, AEvent, AText);
end;

function TcxCustomScheduler.GetEventUserModernStyleHintInfo(AEvent: TcxSchedulerControlEvent;
  AInfo: TcxSchedulerEventModernStyleHintInfo): Boolean;
begin
  Result := Assigned(FOnGetEventModernStyleHintInfo);
  if Result then
    FOnGetEventModernStyleHintInfo(Self, AEvent, AInfo);
end;

function TcxCustomScheduler.GetHScrollBarBounds: TRect;
begin
  Result := CurrentView.GetHScrollBarBounds;
end;

function TcxCustomScheduler.GetInternalCanvas: TcxCanvas;
begin
  Result := inherited Canvas;
end;

function TcxCustomScheduler.GetIsLocked: Boolean;
begin
  Result := cfLocked in FControlFlags;
end;

function TcxCustomScheduler.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  if IsPopupScrollBars then
    Result := inherited GetMainScrollBarsClass
  else
    Result := TcxControlScrollBars;
end;

function TcxCustomScheduler.GetNextView(
  AView: TcxSchedulerCustomView): TcxSchedulerCustomView;
begin
  Result := AView;
end;

function TcxCustomScheduler.GetOnShowDateHint: TcxSchedulerShowDateHintEvent;
begin
  Result := FOnShowDateHint;
end;

function TcxCustomScheduler.GetPainterHelper: TcxSchedulerPainterHelperClass;
begin
  Result := TcxSchedulerPainterHelper;
end;

function TcxCustomScheduler.GetSizeGripBounds: TRect;
begin
  Result := CurrentView.GetSizeGripBounds;
end;

function TcxCustomScheduler.GetTimeBias(AStart: TDateTime): Double;
begin
  Result := 0;
  if StorageValid then
  begin
    with DateTimeHelper do
    begin
      Result := TimeZoneBias(OptionsView.CurrentTimeZone) + Storage.TimeBias;
      if OptionsView.CurrentTimeZoneDaylightSaving then
        Result := Result + TimeZoneDaylightBias(AStart, OptionsView.CurrentTimeZone) * MinuteToTime;
    end;
  end;
end;

function TcxCustomScheduler.GetVScrollBarBounds: TRect;
begin
  Result := CurrentView.GetVScrollBarBounds;
end;

function TcxCustomScheduler.HasConflict(AStartDrag: Boolean): Boolean;
begin
  if EventOperations.Intersection then
    Result := False
  else
    Result := EventList.HasConflict(IsCopyDragDrop, AStartDrag);
end;

function TcxCustomScheduler.HasResources: Boolean;
begin
  Result := (Storage <> nil) and (Storage.ResourceCount > 0);
end;

procedure TcxCustomScheduler.InitControl;
begin
  inherited InitControl;
  if not IsLoading and OptionsCustomize.IntegralSizing then
    DateNavigator.SetIntegralSizes;
  CurrentView.InitScrollBars;
end;

procedure TcxCustomScheduler.InitScrollBarsParameters;
begin
  CurrentView.InitScrollBarsParameters;
end;

procedure TcxCustomScheduler.InternalDeleteEvent(AEvent: TcxSchedulerControlEvent;
  AIgnoreRecurring: Boolean);
var
  ADeleteOccurrence: Boolean;
begin
  if not DoBeforeDeleting(AEvent) then Exit;
  if not AIgnoreRecurring and AEvent.IsRecurring then
  begin
    if cxShowRecurrenceSelectionDialog(AEvent, rsmDeleting,
      DialogsLookAndFeel, ADeleteOccurrence) then
      begin
        CurrentView.DoBeforeReallyDeleting(AEvent);
        if ADeleteOccurrence then
          AEvent.Delete
        else
          AEvent.Pattern.Delete;
      end;
  end
  else
  begin
    CurrentView.DoBeforeReallyDeleting(AEvent);
    AEvent.Delete;
  end;
end;

procedure TcxCustomScheduler.InternalDeleteSelectedEvents(AForceDelete, ACheckReadOnly: Boolean);
var
  I, AIndex, ACount: Integer;
  AEvent: TcxSchedulerControlEvent;
  ASelectedIDs: array of Variant;

  procedure GetSelection;
  var
    I: Integer;
  begin
    SetLength(ASelectedIDs, SelectedEventCount);
    for I := 0 to SelectedEventCount - 1 do
      ASelectedIDs[I] := SelectedEvents[I].ID;
  end;

  function GetIndexOfID(const AID: Variant): Integer;
  begin
    for Result := 0 to High(ASelectedIDs) do
      if VarEquals(AID, ASelectedIDs[Result]) then Exit;
    Result := -1;
  end;

  procedure DeleteIDByIndex(AIndex: Integer);
  var
    I: Integer;
  begin
    for I := AIndex + 1 to High(ASelectedIDs) do
      ASelectedIDs[I - 1] := ASelectedIDs[I];
    SetLength(ASelectedIDs, Length(ASelectedIDs) - 1);
  end;

begin
  if CurrentView <> nil then
    CurrentView.Controller.StopEditShowingTimer;
  GetSelection;
  if AForceDelete then
    BeginUpdate;
  ACount := 0;
  try
    while ACount <> SelectedEventCount do
    begin
      ACount := SelectedEventCount;
      for I := 0 to ACount - 1 do
      begin
        AEvent := SelectedEvents[I];
        if CurrentView.Controller.FDragEvent = AEvent then
          CurrentView.Controller.FDragEvent := nil;
        AIndex := GetIndexOfID(AEvent.ID);
        if AIndex >= 0 then
        begin
          DeleteIDByIndex(AIndex);
          if not AForceDelete then
            DeleteEvent(AEvent)
          else
            if not (ACheckReadOnly and AEvent.ReadOnly) then
              InternalDeleteEvent(AEvent, True);
          if ACount <> SelectedEventCount then break;
        end;
      end;
    end;
    CurrentView.DoAfterEventDeleting;
  finally
    SetLength(ASelectedIDs, 0);
    if AForceDelete then
      EndUpdate;
  end;
end;

function TcxCustomScheduler.IsDaysIntervalChanged(
  var AStart, AFinish: TDateTime): Boolean;
begin
  AStart := Min(CurrentView.FirstVisibleDate, DateNavigator.GetRealFirstDate)
    - CurrentView.GetVisibleDaysRange;
  AFinish := Max(CurrentView.LastVisibleDate + CurrentView.GetTimeIncrement * 2 + 7,
    DateNavigator.GetRealLastDate) + CurrentView.GetVisibleDaysRange;
  Result := not ((AStart >= EventList.Start) and (AFinish <= EventList.Finish));
end;

function TcxCustomScheduler.IsViewAtLeft: Boolean;
begin
  Result := OptionsView.ViewPosition = vpLeft;
end;

function TcxCustomScheduler.IsViewAtTop: Boolean;
begin
  Result := OptionsView.ViewPosition = vpTop;
end;

procedure TcxCustomScheduler.KeyDown(
  var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if KeyboardListener <> nil then
    KeyboardListener.Controller.KeyDown(Key, Shift)
  else
    CurrentView.Controller.KeyDown(Key, Shift);
end;

procedure TcxCustomScheduler.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if KeyboardListener <> nil then
    KeyboardListener.Controller.KeyPress(Key)
  else
    if EditController.Edit <> nil then
      PostMessage(EditController.Edit.Handle, WM_CHAR, Word(Key), 0)
    else
      CurrentView.Controller.KeyPress(Key);
end;

procedure TcxCustomScheduler.KeyUp(
  var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if KeyboardListener <> nil then
    KeyboardListener.Controller.KeyUp(Key, Shift)
  else
    CurrentView.Controller.KeyUp(Key, Shift);
end;

procedure TcxCustomScheduler.Loaded;
begin
  inherited Loaded;
  DateNavigator.Loaded;
  ControlBox.Width := DateNavigator.Width;
  if CurrentView <> nil then
    CurrentView.VisibleChanged;
  FullRefresh;
end;

procedure TcxCustomScheduler.LockUpdateChanged(
  ALocking: Boolean);
begin
end;

procedure TcxCustomScheduler.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
var
  I: Integer;
begin
  for I := 0 to SubControlCount - 1 do
    SubControls[I].LookAndFeelChanged(Sender, AChangedValues);
  inherited LookAndFeelChanged(Sender, AChangedValues);
  LayoutChanged;
end;

procedure TcxCustomScheduler.Modified;
begin
  if not FBoundsChanging and CanModified then
    inherited Modified;
end;

procedure TcxCustomScheduler.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AShift: TShiftState;
begin
  AShift := Shift;
  AShift := AShift - [ssPen, ssTouch];
  FPrevMousePos := cxInvalidPoint;
  FActiveControl := GetControlFromPoint(cxPoint(X, Y));
  Capture := FActiveControl;
  Capture.DoBeforeMouseDown(Button, AShift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
  FCaptureControl := FActiveControl; //can be changed on BeginDrag
  Capture.DoMouseDown(Button, AShift, X, Y);
end;

procedure TcxCustomScheduler.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if FActiveControl <> nil then
    FActiveControl.Controller.MouseLeave;
  FActiveControl := nil;
  if CurrentView <> nil then
    CurrentView.HitTest.SetHitPoint(cxInvalidPoint);
end;

procedure TcxCustomScheduler.MouseMove(
  Shift: TShiftState; X, Y: Integer);
var
  APrevControl: TcxSchedulerSubControl;
  AShift: TShiftState;
begin
  if (FPrevMousePos.X = X) and (FPrevMousePos.Y = Y) then Exit;
  AShift := Shift;
  AShift := AShift - [ssPen, ssTouch];
  FPrevMousePos := cxPoint(X, Y);
  APrevControl := FActiveControl;
  FActiveControl := FCaptureControl;
  if FActiveControl = nil then
    FActiveControl := GetControlFromPoint(cxPoint(X, Y));
  FActiveControl.DoMouseMove(AShift, X, Y);
  inherited MouseMove(Shift, X, Y);
  if OptionsView.HotTrack or OptionsView.ShowHints then
  begin
    CurrentView.MousePositionChanged(X, Y);
    if (ssLeft in AShift) and not CurrentView.HitTest.HitAtControl and (FSelStart <> FSelFinish) then
    begin
      ReplaceSelParams(FSelStart, FSelStart, FSelResource);
      CurrentView.Refresh;
    end;
    EventHitTestController.MouseMove(X, Y, AShift);
  end;
  if (APrevControl <> FActiveControl) and (APrevControl <> nil) then
    APrevControl.Controller.MouseLeave;
end;

procedure TcxCustomScheduler.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Shift := Shift - [ssPen, ssTouch];
  Capture.DoMouseUp(Button, Shift, X, Y);
  Capture := nil;
  CurrentView.MousePositionChanged(X, Y);
  EventHitTestController.MouseMove(X, Y, Shift);
end;

function TcxCustomScheduler.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := ResourceNavigator.NeedScrollBar and (AScrollKind = ResourceNavigator.ScrollBarKind) or
    CurrentView.NeedPanningFeedback(AScrollKind);
end;

function TcxCustomScheduler.NeedShowHint(AEvent: TcxSchedulerControlEvent;
  var AHintText: string; AllowShow: Boolean): Boolean;
begin
  Result := EditController.Controller.EditShowingTimer = nil;
  if Result then
    if OptionsView.Style = svsModern then
      Result := Storage.EditingEventInfoList.ActiveEditingEvent = nil
    else
    begin
      if (CurrentView <> nil) and not Assigned(FOnGetEventHintText) then
        AHintText := CurrentView.GetEventHintText(AEvent)
      else
        AHintText := GetEventHintText(AEvent);
      Result := (Length(AHintText) > 0) and
        (Assigned(FOnGetEventHintText) or AllowShow);
    end;
end;

procedure TcxCustomScheduler.DoPaint;
var
  I: Integer;
begin
  if IsLoading then
    Exit;
  if IsLocked then
    Exit;
  if BorderStyle = cxcbsDefault then
  begin
    LookAndFeelPainter.DrawSchedulerBorder(Canvas, Bounds);
    Canvas.IntersectClipRect(ClientBounds);
  end;
  DrawScrollBars(Canvas);
 for I := 0 to SubControlCount - 1 do
    PaintControl(SubControls[I], False);
  DrawSplitters;
  FCanModified := True;
end;

procedure TcxCustomScheduler.PaintControl(AControl: TcxSchedulerSubControl; ASpecialPaint: Boolean = False);
begin
  if not AControl.Visible or (AControl.IsSpecialPaint <> ASpecialPaint) or
    not Canvas.RectVisible(AControl.Bounds) then Exit;
  Canvas.SaveClipRegion;
  try
    AControl.Paint;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TcxCustomScheduler.PeriodChanged;
begin
  FullRefresh;
  DateNavigator.DoPeriodChangedEvent;
end;

procedure TcxCustomScheduler.RemoveSubControl(
  AControl: TcxSchedulerSubControl);
begin
  if (FSubControls.Remove(AControl) <> -1) and not IsDestroying then
  begin
    LayoutChanged;
    if Capture = AControl then
      Capture := nil;
  end;
end;

procedure TcxCustomScheduler.ReplaceSelParams(ASelStart, ASelFinish: TDateTime;
  AResource: TcxSchedulerStorageResourceItem);
begin
  with DateTimeHelper do
  begin
    if (FSelResource <> AResource) or (FSelStart <> RoundTime(ASelStart)) or
      (FSelFinish <> RoundTime(ASelFinish)) then
    begin
      FSelResource :=  AResource;
      FSelStart := RoundTime(ASelStart);
      FSelFinish := RoundTime(ASelFinish);
      MakeResourceVisible(AResource);
      DoSelectionChanged;
    end;
  end;
end;

procedure TcxCustomScheduler.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if EditController.IsEditing then Exit;
  if ResourceNavigator.NeedScrollBar and (AScrollBarKind = ResourceNavigator.ScrollBarKind) then
    ResourceNavigator.Scroll(AScrollCode, AScrollPos)
  else
  begin
    CurrentView.Scroll(AScrollBarKind, AScrollCode, AScrollPos);
    DoScaleScroll;
  end;
  if AScrollCode <> scEndScroll then
     Update;
end;

procedure TcxCustomScheduler.SelectedDaysChanged(AView: TcxSchedulerCustomView);
begin
  if AView <> nil then
    AView.SelectedDaysChanged;
end;

procedure TcxCustomScheduler.SetCurrentView(
  AView: TcxSchedulerCustomView);
begin
  if (AView <> nil) and (AView.Scheduler = Self) then
  begin
    if Assigned(FCurrentView) then
      FCurrentView.DeactivateView;
    FCurrentView := AView;
    if HandleAllocated then
      FCurrentView.InitScrollBars;
  end;
end;

procedure TcxCustomScheduler.SubControlAdd(
  AControl: TcxSchedulerSubControl);
begin
  FSubControls.Add(AControl);
  LayoutChanged;
end;

procedure TcxCustomScheduler.SubControlRemove(
  AControl: TcxSchedulerSubControl);
begin
  FSubControls.Remove(AControl);
  LayoutChanged;
end;

procedure TcxCustomScheduler.SynchronizeRangeControl;
begin
end;

procedure TcxCustomScheduler.SynchronizeVisibleDays;
begin
  Include(FControlFlags, cfViewValid);
end;

procedure TcxCustomScheduler.UpdateDateNavigatorDragging(Accept: Boolean);
begin
  if not DateNavigator.Visible then Exit;
  if Accept then
    DateNavigator.UpdateDragging
  else
    DateNavigator.ClearDragging;
end;

procedure TcxCustomScheduler.UpdateHolidayDays(ABegin, AEnd: TDate);
begin
  FHolidayDays.Clear;
  if (Storage <> nil) then
    Storage.PopulateHolidayDates(FHolidayDays, ABegin, AEnd);
end;

procedure TcxCustomScheduler.UpdateEventsCache(
  ACheckDaysInterval: Boolean);
var
  AChanged: Boolean;
  AStart, AFinish: TDateTime;
begin
  CurrentView.Controller.CloseInplaceEdit;
  if not StorageValid then
  begin
    FreeAndNil(FEventList);
    FEventList := CreateEventList;
  end;
  FEventList.NeedForceUpdate := FEventList.NeedForceUpdate or (FEventList.TimeZone <> OptionsView.CurrentTimeZone);
  FEventList.TimeZone := OptionsView.CurrentTimeZone;
  FEventList.DaylightSaving := OptionsView.CurrentTimeZoneDaylightSaving;
  FEventList.ShowEventsWithoutResource := OptionsView.ShowEventsWithoutResource or
    (VisibleGroupingKind = gkNone);
  AChanged := IsDaysIntervalChanged(AStart, AFinish);
  if ACheckDaysInterval and not AChanged then Exit;
  FEventList.BeforeUpdate;
  if FEventList.Clones.Count > 0 then
  begin
    FEventList.SelStart := Min(FEventList.Clones.First.Start, CurrentView.FirstVisibleDate) - 7;
    FEventList.SelFinish := Max(FEventList.Clones.Last.Finish, CurrentView.LastVisibleDate) + CurrentView.GetTimeIncrement * 2 + 7;
    AStart := Min(AStart, FEventList.SelStart);
    AFinish := Max(AFinish, FEventList.SelFinish);
  end
  else
  begin
    FEventList.SelStart := CurrentView.FirstVisibleDate - 7;
    FEventList.SelFinish := CurrentView.LastVisibleDate + CurrentView.GetTimeIncrement * 2 + 7;
  end;
  CheckEventListTimeRangeUsing;
  if StorageValid then
    Storage.GetEvents(FEventList, AStart, AFinish);
  TabOrdersList.Assign(EventList);
  TabOrdersList.Sort(@cxCompareTabOrders);
  FEventDays.Clear;
  if DateNavigator.GetShowDatesContainingEventsInBold then
    FEventList.ExtractUsedDays(FEventDays);
  if FEventList.Selection.Count > 0 then
    CurrentView.Controller.FDragEvent := FEventList.Selection[0]
  else
    CurrentView.Controller.FDragEvent := nil;
  CurrentView.EventsListChanged;
end;

procedure TcxCustomScheduler.ValidateFirstVisibleResourceIndex;
begin
  if (OptionsView.ResourcesPerPage = 0) and (FFirstVisibleResourceIndex <> 0) then
  begin
    FFirstVisibleResourceIndex := 0;
    FirstVisibleResourceChanged;
  end
  else
    if FFirstVisibleResourceIndex + OptionsView.ResourcesPerPage > ResourceNavigator.ResourceCount then
    begin
      FFirstVisibleResourceIndex := Max(0, ResourceNavigator.ResourceCount -
        OptionsView.ResourcesPerPage);
    end;
end;

procedure TcxCustomScheduler.ValidateSelection(
  ASelection: TcxSchedulerDateList);
begin
  DateNavigator.UpdateSelection;
end;

procedure TcxCustomScheduler.ValidateState;
var
  AIsLocked: Boolean;
begin
  AIsLocked := cfLocked in FControlFlags;
  if cfInvalidLayout in FControlFlags then
  try
    LayoutChanged;
  finally
    FControlFlags := [];
    if AIsLocked then
      FControlFlags := [cfLocked];
  end;
end;

procedure TcxCustomScheduler.ViewVisibleChanged(
  AView: TcxSchedulerCustomView);
begin
  Exclude(FControlFlags, cfViewValid);
  if FVisibleChangedCount = 0 then
    DateNavigator.BeginUpdate;
  Inc(FVisibleChangedCount);
  try
    if AView.Visible and (CurrentView <> AView) then
    begin
     CurrentView.Visible := False;
     DoViewTypeChanged(AView);
     CurrentView := AView;
    end
    else
      if not FCurrentView.Visible and (FVisibleChangedCount = 1)  then
        GetNextView(FCurrentView).Visible := True;
  finally
    Dec(FVisibleChangedCount);
    if FVisibleChangedCount = 0 then
    begin
      AlignSubControls;
      LayoutChanged;
      DateNavigator.EndUpdate;
    end;
  end;
  Modified;
end;

function TcxCustomScheduler.VisibleGroupingKind: TcxSchedulerGroupingKind;
var
  I, C: Integer;
begin
  Result := CurrentView.GetGroupingKind;
  C := 0;
  if HasResources then
  begin
    for I := 0 to Storage.ResourceCount - 1 do
      if Storage.Resources.ResourceItems[I].Visible then Inc(C);
  end;
  if C = 0 then
    Result := gkNone
  else
    if Result = gkDefault then
      Result := gkByResource;
end;

function TcxCustomScheduler.CanDrag(X, Y: Integer): Boolean;
begin
  Result := inherited CanDrag(X, Y) and not IsDesigning and
    CaptureController.CanDrag(X, Y);
end;

procedure TcxCustomScheduler.DoEndDrag(
  Target: TObject; X, Y: Integer);
begin
  CaptureController.EndDrag(Target, X, Y);
  inherited DoEndDrag(Target, X, Y);
  Capture := nil;
end;

procedure TcxCustomScheduler.DoStartDrag(
  var DragObject: TDragObject);
begin
  FIsDragCanceled := False;
  HintController.HideHint;
  inherited DoStartDrag(DragObject);
  CaptureController.StartDrag(DragObject);
end;

procedure TcxCustomScheduler.DragAndDrop(
  const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  CaptureController.DragAndDrop(P, Accepted);
end;

procedure TcxCustomScheduler.DragOver(
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  AAccept, ACopyDragDrop: Boolean;
  AControl: TcxSchedulerSubControl;
begin
  ACopyDragDrop := IsCopyDragDrop;
  if State <> dsDragMove then
    FPrevMousePos := cxInvalidPoint
  else
    if (FPrevMousePos.X = X) and (FPrevMousePos.Y = Y) and (FPrevCopyDragDrop = ACopyDragDrop) then
      Exit
    else
      FPrevMousePos := cxPoint(X, Y);
  if not Dragging then
  begin
    AControl := GetControlFromPoint(Point(X, Y));
    if AControl <> nil then
      SetCaptureControl(AControl);
  end;
  AAccept := Accept and (DragMode = dmAutomatic);
  inherited DragOver(Source, X, Y, State, Accept);
  if not Assigned(OnDragOver) then Accept := AAccept;
  CaptureController.DragOver(Source, X, Y, State, Accept);
  FPrevCopyDragDrop := ACopyDragDrop;
end;

procedure TcxCustomScheduler.EndDragAndDrop(
  Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  CaptureController.EndDragAndDrop(Accepted);
end;

function TcxCustomScheduler.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := CaptureController.GetDragAndDropObjectClass;
end;

function TcxCustomScheduler.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := CurrentView.GetDragObjectClass;
end;

function TcxCustomScheduler.StartDragAndDrop(
  const P: TPoint): Boolean;
begin
  Result := CaptureController.StartDragAndDrop(P)
end;

function TcxCustomScheduler.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  Result := CurrentView.GetGestureClient(APoint);
  if Result = nil then
    Result := inherited GetGestureClient(APoint);
end;

procedure TcxCustomScheduler.BiDiModeChanged;
begin
  inherited BiDiModeChanged;
  FullRefresh;
end;

procedure TcxCustomScheduler.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  OptionsView.ChangeScale(M, D);
end;

// IcxSchedulerStorageListener
procedure TcxCustomScheduler.StorageChanged(Sender: TObject);
begin
  FullRefresh;
end;

procedure TcxCustomScheduler.StorageRemoved(Sender: TObject);
begin
  Storage := nil;
end;

// IcxFormatControllerListener
procedure TcxCustomScheduler.DoStartOfWeekChanged(
  AOldStartOfWeek, ANewStartOfWeek: TDay);
var
  I, ADelta: Integer;
begin
  ADelta := Ord(ANewStartOfWeek) - Ord(AOldStartOfWeek);
  if (ADelta <> 0) and not CurrentView.IsDayView then
  begin
    for I := 0 to SelectedDays.Count - 1 do
      SelectedDays[I] := SelectedDays[I] + ADelta;
  end;
  OptionsView.CalculateActualStartOfWeek;
  FullRefresh;
end;

procedure TcxCustomScheduler.FormatChanged;
var
  I: Integer;
begin
  if DateTimeHelper <> nil then DateTimeHelper.Refresh;
  if OptionsView.StartOfWeek = swSystem then
    DoStartOfWeekChanged(OptionsView.ActualStartOfWeek,
      TDay(DateTimeHelper.StartOfWeek))
  else
  begin
    for I := 0 to SubControlCount - 1 do
      SubControls[I].FormatChanged;
    FullRefresh;
  end;
end;

// IcxFormatControllerListener2
procedure TcxCustomScheduler.TimeChanged;
begin
  DoUpdateTime;
end;

// IcxStoredObject
function TcxCustomScheduler.GetObjectName: string;
begin
  if FStoringName <> '' then
    Result := FStoringName
  else
    Result := Name;
end;

function TcxCustomScheduler.GetProperties(AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  AProperties.Add('SelectedDayCount');
  AProperties.Add('ResourcesPerPage');
  for I := 0 to SubControlCount - 1 do
    SubControls[I].GetProperties(AProperties);
  Result := True;
end;

procedure TcxCustomScheduler.GetPropertyValue(const AName: string;
  var AValue: Variant);
var
  I: Integer;
begin
  if AName = 'SelectedDayCount' then
    AValue := SelectedDays.Count
  else
    if AName = 'ResourcesPerPage' then
      AValue := OptionsView.ResourcesPerPage
    else
      for I := 0 to SubControlCount - 1 do
        SubControls[I].GetPropertyValue(AName, AValue);
end;

procedure TcxCustomScheduler.SetPropertyValue(const AName: string;
  const AValue: Variant);
var
  I: Integer;
  ADate: TDateTime;
begin
  if AName = 'SelectedDayCount' then
  begin
    SelectedDays.Clear;
    ADate := Date;
    for I := 0 to Min(Max(AValue - 1, 0), 41) do
      SelectedDays.Add(ADate + I);
  end
  else
    if AName = 'ResourcesPerPage' then
      OptionsView.ResourcesPerPage := AValue
    else
      for I := 0 to SubControlCount - 1 do
        SubControls[I].SetPropertyValue(AName, AValue);
end;

procedure TcxCustomScheduler.CreateUpdateTimeTimer;
begin
  FUpdateTimeTimer := TTimer.Create(nil);
  FUpdateTimeTimer.Interval := 60 * 1000;
  FUpdateTimeTimer.OnTimer := UpdateTimeHandler;
end;

function TcxCustomScheduler.GetActiveHitTest: TcxSchedulerSubControlHitTest;
var
  AControl: TcxSchedulerSubControl;
  APos: TPoint;
begin
  if (FActiveControl <> nil) and (FCaptureControl = nil) then
    Result := FActiveControl.HitTest
  else
  begin
    APos := ScreenToClient(GetMouseCursorPos);
    AControl := GetControlFromPoint(APos);
    if AControl = Background then
      Result := nil
    else
    begin
      AControl.MousePositionChanged(APos.X, APos.Y);
      Result := AControl.HitTest;
    end;
  end;
end;

function TcxCustomScheduler.GetCaptureControl: TcxSchedulerSubControl;
begin
  Result := FCaptureControl;
  if FCaptureControl = nil then
    Result := Background;
end;

function TcxCustomScheduler.GetCaptureController: TcxSchedulerSubControlController;
begin
  Result := Capture.Controller;
end;

function TcxCustomScheduler.GetSelectedEventCount: Integer;
begin
  Result := EventList.Selection.Count;
end;

function TcxCustomScheduler.GetSelectedEvent(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := EventList.Selection[AIndex]
end;

function TcxCustomScheduler.GetSelFinish: TDateTime;
begin
  Result := Max(FSelStart, FSelFinish);
  Result := DateTimeHelper.RoundTime(Result + CurrentView.GetTimeIncrement);
  CurrentView.ValidateSelectionFinishTime(Result);
end;

function TcxCustomScheduler.GetSelStart: TDateTime;
begin
  Result := Min(FSelStart, FSelFinish);
end;

function TcxCustomScheduler.GetIsDynamicUpdate: Boolean;
begin
  Result := OptionsCustomize.DynamicSizing and not IsDesigning;
end;

function TcxCustomScheduler.GetScrollContentForegroundColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultSchedulerViewTextColor;
end;

function TcxCustomScheduler.GetStartOfWeek: TDay;
begin
  Result := OptionsView.ActualStartOfWeek;
end;

function TcxCustomScheduler.GetStorageActive: Boolean;
begin
  Result := (FStorage <> nil) and FStorage.IsActive;
end;

function TcxCustomScheduler.GetStorageValid: Boolean;
begin
  Result := FStorage <> nil;
end;

function TcxCustomScheduler.GetSubControl(
  AIndex: Integer): TcxSchedulerSubControl;
begin
  Result := TcxSchedulerSubControl(FSubControls[AIndex])
end;

function TcxCustomScheduler.GetSubControlCount: Integer;
begin
  Result := FSubControls.Count;
end;

function TcxCustomScheduler.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := CurrentView.GetTouchScrollUIOwner(APoint);
  if Result = nil then
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

function TcxCustomScheduler.GetVisibleEventCount: Integer;
begin
  Result := EventList.Count;
end;

function TcxCustomScheduler.GetVisibleEvent(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := EventList[AIndex];
end;

procedure TcxCustomScheduler.InitEventBySelection(
  AEvent: TcxSchedulerEvent; AllDay: Boolean;
  ARecurrence: Boolean; AInplaceEditing: Boolean);
const
  States: array[Boolean] of Byte = (tlsBusy, tlsFree);
begin
  if SelResource <> nil then
    AEvent.ResourceID := SelResource.ResourceID;
  CurrentView.InitEventBySelectedTime(AEvent, AllDay, ARecurrence, AInplaceEditing);
  AEvent.State := States[AEvent.AllDayEvent];
  if ARecurrence then
    AEvent.RecurrenceInfo.OccurDays := [TDay(DayOfWeek(AEvent.Start) - 1)];
end;

procedure TcxCustomScheduler.SetCaptureControl(
  AValue: TcxSchedulerSubControl);
begin
  if FCaptureControl <> AValue then
  begin
    CaptureController.DoCancelMode;
    FCaptureControl := AValue;
  end;
end;

procedure TcxCustomScheduler.SetContentPopupMenu(
  AValue: TcxSchedulerContentPopupMenu);
begin
  FContentPopupMenu.Assign(AValue);
end;

procedure TcxCustomScheduler.SetControlBox(
  AValue: TcxSchedulerControlBox);
begin
  FControlBox.Assign(AValue);
end;

procedure TcxCustomScheduler.SetDialogsLookAndFeel(
  AValue: TcxLookAndFeel);
begin
  FDialogsLookAndFeel := AValue;
end;

procedure TcxCustomScheduler.SetDialogsStyle(const Value: string);
var
  AIndex: Integer;
begin
  if CompareText(FDialogsStyle, Value) <> 0 then
  begin
    if Value = '' then
      FDialogsStyle := Value
    else
    begin
      AIndex := cxSchedulerEditorManager.GetIndexByName(Value);
      if AIndex <> -1 then
        FDialogsStyle := cxSchedulerEditorManager.Items[AIndex].GetName;
    end;
  end;
end;

procedure TcxCustomScheduler.SetEventImages(AValue: TCustomImageList);
begin
  if AValue <> FEventImages then
  begin
    if FEventImages <> nil then FEventImages.RemoveFreeNotification(Self);
    FEventImages := AValue;
    if FEventImages <> nil then FEventImages.FreeNotification(Self);
  end;
end;

procedure TcxCustomScheduler.SetEventOperations(
  AValue: TcxSchedulerEventOperations);
begin
  FEventOperations.Assign(AValue);
end;

procedure TcxCustomScheduler.SetEventPopupMenu(
  AValue: TcxSchedulerEventPopupMenu);
begin
  FEventPopupMenu.Assign(AValue);
end;

procedure TcxCustomScheduler.SetFirstVisibleResourceIndex(AValue: Integer);
var
  APrevSelResource: TcxSchedulerStorageResourceItem;
begin
  AValue := Max(0, AValue);
  if AValue <> FFirstVisibleResourceIndex then
  begin
    FFirstVisibleResourceIndex := AValue;
    if CurrentView <> nil then
    begin
      APrevSelResource := SelResource;
      CurrentView.LayoutChanged;
      if APrevSelResource <> SelResource then
        DoSelectionChanged;
    end;
  end;
end;

procedure TcxCustomScheduler.SetOptionsBehavior(AValue: TcxSchedulerOptionsBehavior);
begin
  if AValue <> FOptionsBehavior then
    FOptionsBehavior.Assign(AValue);
end;

procedure TcxCustomScheduler.SetOptionsCustomize(
  AValue: TcxSchedulerOptionsCustomize);
begin
  FOptionsCustomize.Assign(AValue);
end;

procedure TcxCustomScheduler.SetOptionsView(
  AValue: TcxSchedulerOptionsView);
begin
  FOptionsView.Assign(AValue);
end;

procedure TcxCustomScheduler.SetStorage(
  AValue: TcxCustomSchedulerStorage);
begin
  if FStorage <> AValue then
  begin
    if CurrentView <> nil then
      CurrentView.Controller.CloseInplaceEdit;
    if FStorage <> nil then
    begin
      FStorage.RemoveListener(Self);
      FStorage.RemoveListener(FEventList);
    end;
    FreeAndNil(FEventList);
    FEventList := CreateEventList;
    FStorage := AValue;
    if FStorage <> nil then
    begin
      FStorage.AddListener(Self);
      FStorage.AddListener(FEventList);
    end;
    FullRefresh;
    NotifyStorageChanged;
  end;
end;

procedure TcxCustomScheduler.SetResourceNavigator(
  AValue: TcxSchedulerResourceNavigator);
begin
  FResourceNavigator.Assign(AValue);
end;

procedure TcxCustomScheduler.SetStyles(
  AValue: TcxSchedulerStyles);
begin
  FStyles.Assign(AValue);
end;

procedure TcxCustomScheduler.UpdateTimeHandler(Sender: TObject);
begin
  if CurrentView.Visible then
    CurrentView.TimeChanged;
  if DateNavigator.Visible then
    DateNavigator.CheckCurrentDate;
end;

procedure TcxCustomScheduler.WMCancelMode(var Message: TWMCancelMode);
begin
  FIsDragCanceled := True;
  try
    inherited;
  finally
    FIsDragCanceled := False;
  end;
end;

procedure TcxCustomScheduler.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  ACursor: TCursor;
begin
  ACursor := crDefault;
  P := ScreenToClient(GetMouseCursorPos);
  if IsDesigning and (DragAndDropState = ddsNone) and GetDesignHitTest(P.X, P.Y, [ssLeft]) then
    ACursor := GetControlFromPoint(P).Controller.GetCursor(P.X, P.Y);
  if ACursor <> crDefault then
    SetCursor(Screen.Cursors[ACursor])
  else
    inherited;
end;

procedure TcxCustomScheduler.WMTimeChange(var Message: TWMTimeChange);
begin
  DoUpdateTime;
  Message.Result := 0;
end;

procedure TcxCustomScheduler.ReadSelectionData(AReader: TReader);
var
  I: Integer;
begin
  FSelectedDays.Count := AReader.ReadInteger and $FF;
  for I := 0 to FSelectedDays.Count - 1 do
    FSelectedDays[I] := Date + I;
end;

procedure TcxCustomScheduler.WriteSelectionData(AWriter: TWriter);
begin
  AWriter.WriteInteger(FSelectedDays.Count);
end;

{ TcxSchedulerPopupMenu }

constructor TcxSchedulerPopupMenu.Create(
  AScheduler: TcxCustomScheduler);
begin
  FScheduler := AScheduler;
  FUseBuiltInPopupMenu := True;
end;

destructor TcxSchedulerPopupMenu.Destroy;
begin
  PopupMenu := nil;
  FreeAndNil(FInternalMenu);
  inherited Destroy;
end;

procedure TcxSchedulerPopupMenu.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerPopupMenu then
  begin
    PopupMenu := TcxSchedulerPopupMenu(Source).PopupMenu;
    UseBuiltInPopupMenu := TcxSchedulerPopupMenu(Source).UseBuiltInPopupMenu;
  end;
end;

function TcxSchedulerPopupMenu.AddValidSeparator(AOwner: TMenuItem): TMenuItem;
begin
  if AOwner.Count > 0 then
    Result := CreateSeparator(AOwner)
  else
    Result := nil
end;

procedure TcxSchedulerPopupMenu.CreateInternalMenu;
begin
  FreeAndNil(FInternalMenu);
  FInternalMenu := TPopupMenu.Create(nil);
  FInternalMenu.Images := MenuImages;
  CreateItems;
  FInternalMenu.BiDiMode := Scheduler.BiDiMode;
end;

procedure TcxSchedulerPopupMenu.CreateItems;
begin
end;

function TcxSchedulerPopupMenu.CreateSeparator(AOwner: TMenuItem): TMenuItem;
begin
  Result := CreateSubItem(AOwner, '-');
end;

function TcxSchedulerPopupMenu.CreateSubItem(
  AOwner: TMenuItem; const ACaption: string; ACommand: Integer = -1;
  AImageIndex: Integer = -1; AEnabled: Boolean = True; AChecked: Boolean = False): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := ACaption;
  Result.Enabled := AEnabled;
  Result.ImageIndex := AImageIndex;
  Result.Checked := AChecked;
  Result.Tag := ACommand;
  Result.OnClick := OnItemClickHandler;
  AOwner.Add(Result);
end;

procedure TcxSchedulerPopupMenu.DoExecute(ACommand: Integer);
begin
end;

function TcxSchedulerPopupMenu.DoOnClick(ACommand: Integer): Boolean;
begin
  Result := False;
end;

function TcxSchedulerPopupMenu.DoOnPopup: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerPopupMenu.ExecuteCommand(ACommand: Integer);
begin
  if IsValidCommand(ACommand) and not DoOnClick(ACommand) then
    DoExecute(ACommand);
end;

function TcxSchedulerPopupMenu.FindItemByCommand(AOwnerItem: TMenuItem;
  ACommand: Integer): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  if (InternalMenu = nil) or (AOwnerItem = nil) then Exit;
  with AOwnerItem do
    for I := 0 to Count - 1 do
      if (Items[I].Tag = Ord(ACommand)) and (Items[I].Caption <> '-') then
      begin
        Result := Items[I];
        system.Break;
      end;
end;

function TcxSchedulerPopupMenu.IsValidCommand(ACommand: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerPopupMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = PopupMenu) then
    PopupMenu := nil;
end;

function TcxSchedulerPopupMenu.Popup(X, Y: Integer): Boolean;
var
  AAdapter: TdxCustomBuiltInPopupMenuAdapter;
begin
  Scheduler.HintController.HideHint;
  Scheduler.HintController.FLockHint := True;
  try
    CreateInternalMenu;
    Result := DoOnPopup;
    if not Result then
    begin
      if not UseBuiltInPopupMenu then
        Result := ShowPopupMenu(Scheduler, FPopupMenu, X, Y)
      else
        if TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
          Result := ShowPopupMenu(Scheduler, FInternalMenu, X, Y)
        else
        begin
          AAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(Scheduler);
          try
            TdxBuiltInPopupMenuAdapterHelper.AddMenu(AAdapter, FInternalMenu, AdaptedMenuItemClickHandler);
            Result := AAdapter.Popup(Point(X, Y));
          finally
            AAdapter.Free;
          end;
        end;
    end;
  finally
    Scheduler.HintController.FLockHint := False;
  end;
end;

procedure TcxSchedulerPopupMenu.OnItemClickHandler(Sender: TObject);
begin
  if Sender is TComponent then
    ExecuteCommand(TComponent(Sender).Tag);
end;

function TcxSchedulerPopupMenu.Storage: TcxCustomSchedulerStorage;
begin
  Result := Scheduler.Storage;
end;

procedure TcxSchedulerPopupMenu.AdaptedMenuItemClickHandler(Sender: TObject);
begin
  TMenuItem((Sender as TComponent).Tag).Click;
end;

function TcxSchedulerPopupMenu.GetRoot: TMenuItem;
begin
  Result := FInternalMenu.Items;
end;

procedure TcxSchedulerPopupMenu.SetPopupMenu(const Value: TComponent);
begin
  if FPopupMenu <> Value then
  begin
    if (FPopupMenu <> nil) and not (csDestroying in FPopupMenu.ComponentState) then
      FPopupMenu.RemoveFreeNotification(Scheduler);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Scheduler);
  end;
end;

{ TcxSchedulerCustomContentPopupMenu }

constructor TcxSchedulerCustomContentPopupMenu.Create(
  AScheduler: TcxCustomScheduler);
begin
  inherited Create(AScheduler);
  FNewID := -1;
  FAllDayID := -1;
  FRecurrenceID := -1;
end;

function TcxSchedulerCustomContentPopupMenu.CanCreateEvent: Boolean;
begin
  Result := Scheduler.CurrentView.Controller.CanCreateEventUsingDialog;
end;

procedure TcxSchedulerCustomContentPopupMenu.CreateNewEventItems(ANew,
  AllDay, AReccurence: Boolean; ANewID, AllDayID, ARecurrenceID: Integer);
var
  AEnabled: Boolean;
begin
  AEnabled := CanCreateEvent;
  if ANew then
  begin
    CreateSubItem(Root, cxGetResourceString(@scxpmNewEvent),
      ANewID, 2, AEnabled);
    FNewID := ANewID;
  end;
  if AllDay then
  begin
    CreateSubItem(Root, cxGetResourceString(@scxpmNewAllDayEvent),
      AllDayID, -1, AEnabled);
    FAllDayID := AllDayID;
  end;
  if Scheduler.StorageActive and Storage.IsRecurrenceAvailable and
    AReccurence and Scheduler.EventOperations.Recurrence then
  begin
    CreateSubItem(Root, cxGetResourceString(@scxpmNewRecurringEvent),
      ARecurrenceID, 3, AEnabled);
    FRecurrenceID := ARecurrenceID;
  end;
  AddValidSeparator(Root);
end;

procedure TcxSchedulerCustomContentPopupMenu.DoExecute(ACommand: Integer);
begin
  if ACommand <> -1 then
  begin
    if ACommand = FNewID then
      Scheduler.CreateEventUsingDialog
    else if ACommand = FAllDayID then
      Scheduler.CreateEventUsingDialog(True)
    else if ACommand = FRecurrenceID then
      Scheduler.CreateEventUsingDialog(False, True);
  end;
end;

{ TcxSchedulerContentPopupMenu }

constructor TcxSchedulerContentPopupMenu.Create(
  AScheduler: TcxCustomScheduler);
begin
  inherited Create(AScheduler);
  FItems := [cpmiNewEvent, cpmiNewAllDayEvent, cpmiNewReccuringEvent,
    cpmiToday, cpmiGoToDate, cpmiGoToThisDay, cpmiResourcesLayout];
end;

procedure TcxSchedulerContentPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerContentPopupMenu then
  begin
    Items := TcxSchedulerContentPopupMenu(Source).Items;
    OnClick := TcxSchedulerContentPopupMenu(Source).OnClick;
    OnPopup := TcxSchedulerContentPopupMenu(Source).OnPopup;
  end;
end;

procedure TcxSchedulerContentPopupMenu.Execute(
  AItem: TcxSchedulerContentPopupMenuItem);
begin
  ExecuteCommand(Ord(AItem));
end;

function TcxSchedulerContentPopupMenu.GetMenuItem(
  AItem: TcxSchedulerContentPopupMenuItem): TMenuItem;
begin
  Result := FindItemByCommand(Root, Ord(AItem));
end;

procedure TcxSchedulerContentPopupMenu.CreateItems;
begin
  FActualItems := FItems;
  if Scheduler.CurrentView <> nil then
    Scheduler.CurrentView.ValidateContentPopupMenuItems(FActualItems);
  CreateNewEventItems(cpmiNewEvent in ActualItems, cpmiNewAllDayEvent in ActualItems,
    cpmiNewReccuringEvent in ActualItems, Ord(cpmiNewEvent), Ord(cpmiNewAllDayEvent),
    Ord(cpmiNewReccuringEvent));
  CreateGoToThisDayItem;
  if cpmiToday in ActualItems then
    CreateSubItem(Root, cxGetResourceString(@scxpmToday), Ord(cpmiToday));
  if cpmiGoToDate in ActualItems then
    CreateSubItem(Root, cxGetResourceString(@scxpmGoToDate), Ord(cpmiGoToDate));
  if (cpmiResourcesLayout in ActualItems) and Scheduler.CurrentView.IsShowResources then
  begin
    AddValidSeparator(Root);
    CreateSubItem(Root, cxGetResourceString(@scxpmResourcesLayout), Ord(cpmiResourcesLayout));
  end;
end;

procedure TcxSchedulerContentPopupMenu.DoExecute(ACommand: Integer);
const
  AGoToThisDayMode: array[Boolean] of TcxSchedulerViewMode = (vmDay, vmAgenda);
var
  ADate: TDateTime;
  AViewMode: TcxSchedulerViewMode;
begin
  case ACommand of
    Ord(cpmiNewEvent), Ord(cpmiNewAllDayEvent), Ord(cpmiNewReccuringEvent):
      inherited DoExecute(ACommand);
    Ord(cpmiResourcesLayout):
      cxShowResourcesLayoutEditor(Scheduler.Storage, Scheduler.DialogsLookAndFeel);
    Ord(cpmiGoToThisDay):
      Scheduler.GoToDate(FSavedDate, AGoToThisDayMode[Scheduler.CurrentView is TcxSchedulerAgendaView]);
    Ord(cpmiToday):
      Scheduler.GoToDate(Date);
    Ord(cpmiGoToDate):
      begin
        if Scheduler.CurrentView is TcxSchedulerAgendaView then
          ADate := Scheduler.CurrentView.GetSelStartForInitEventBySelectedTime
        else
          ADate := dxDateOf(Scheduler.SelStart);
        if cxShowGoToDateDialog(Scheduler, Scheduler.DialogsLookAndFeel, ADate, AViewMode) then
          Scheduler.GoToDate(ADate, AViewMode);
      end;
  end;
end;

function TcxSchedulerContentPopupMenu.DoOnClick(ACommand: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnClick) then
    FOnClick(Self, TcxSchedulerContentPopupMenuItem(ACommand), Result);
end;

function TcxSchedulerContentPopupMenu.DoOnPopup: Boolean;
begin
  Result := False;
  if Assigned(FOnPopup) then
    FOnPopup(Self, InternalMenu, Result);
end;

function TcxSchedulerContentPopupMenu.IsValidCommand(
  ACommand: Integer): Boolean;
begin
  Result := (ACommand >= Ord(cpmiNewEvent)) and (ACommand <= Ord(cpmiResourcesLayout));
end;

procedure TcxSchedulerContentPopupMenu.CreateGoToThisDayItem;
var
  AItem: TMenuItem;
begin
  if (cpmiGoToThisDay in ActualItems) and not Scheduler.CurrentView.IsDayView then
  begin
    AItem := CreateSubItem(Root, cxGetResourceString(@scxpmGotoThisDay), Ord(cpmiGoToThisDay));
    AItem.Enabled := (Scheduler.ActiveHitTest <> nil) and Scheduler.ActiveHitTest.HitAtTime;
    if AItem.Enabled then
      FSavedDate := Scheduler.ActiveHitTest.Time;
  end;
end;

{ TcxSchedulerEventPopupMenu }

constructor TcxSchedulerEventPopupMenu.Create(
  AScheduler: TcxCustomScheduler);
begin
  inherited Create(AScheduler);
  FItems := [epmiOpen, epmiEditSeries, epmiShowTimeAs, epmiLabel, epmiDelete];
end;

procedure TcxSchedulerEventPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerEventPopupMenu then
  begin
    Items := TcxSchedulerEventPopupMenu(Source).Items;
    OnClick := TcxSchedulerEventPopupMenu(Source).OnClick;
    OnPopup := TcxSchedulerEventPopupMenu(Source).OnPopup;
  end;
end;

procedure TcxSchedulerEventPopupMenu.CreateItems;

  function GetEditEnabled: Boolean;
  begin
    Result := (Event <> nil) and Scheduler.CanShowEventDialog;
  end;

begin
  FEvent := GetEvent;
  if epmiOpen in Items then
    CreateSubItem(Root, cxGetResourceString(@scxpmOpen), Ord(epmiOpen)).Enabled :=
      GetEditEnabled;
  if (epmiEditSeries in Items) and (Event <> nil) and (Event.EventType <> etNone) then
    CreateSubItem(Root, cxGetResourceString(@scxpmEditSeries), Ord(epmiEditSeries), 8).Enabled :=
      GetEditEnabled;
  if Items * [epmiLabel, epmiShowTimeAs] <> [] then
    AddValidSeparator(Root);
  CreateTimeItems;
  CreateLabelItems;
  CreateDeleteItem;
end;

procedure TcxSchedulerEventPopupMenu.DoExecute(ACommand: Integer);
begin
  case ACommand of
    Ord(epmiOpen):
      Scheduler.EditEventUsingDialog(Event);
    Ord(epmiEditSeries):
      Scheduler.EditEventUsingDialog(Event, True, True);
    Ord(epmiDelete):
      Scheduler.InternalDeleteSelectedEvents(False, True);
    10..13:
      SetEventState(ACommand - 10);
    else
      if (ACommand >= 20) and (ACommand < 20 + EventLabels.Count) then
        SetEventLabelColor(EventLabels[ACommand - 20].Color);
  end;
end;

function TcxSchedulerEventPopupMenu.DoOnClick(ACommand: Integer): Boolean;
var
  AItem: TcxSchedulerEventPopupMenuItem;
  ASubItemIndex: Integer;
begin
  Result := False;
  UnpackCommand(ACommand, AItem, ASubItemIndex);
  if Assigned(FOnClick) then
    FOnClick(Self, AItem, ASubItemIndex, Result);
end;

function TcxSchedulerEventPopupMenu.DoOnPopup: Boolean;
begin
  Result := False;
  if Assigned(FOnPopup) then
    FOnPopup(Self, InternalMenu, Result);
end;

function TcxSchedulerEventPopupMenu.GetMenuItem(
  AItem: TcxSchedulerEventPopupMenuItem; ASubItemIndex: Integer = -1): TMenuItem;
begin
  Result := FindItemByCommand(Root, Ord(AItem));
  if Result <> nil then
  begin
    if (AItem = epmiLabel) and (ASubItemIndex in [0..10]) then
      Result := FindItemByCommand(Result, ASubItemIndex + 20)
    else
      if (AItem = epmiShowTimeAs) and (ASubItemIndex in [0..3]) then
        Result := FindItemByCommand(Result, ASubItemIndex + 10)
  end;
end;

function TcxSchedulerEventPopupMenu.IsValidCommand(
  ACommand: Integer): Boolean;
begin
  Result := (ACommand >= Ord(epmiOpen)) and (ACommand <= Ord(epmiDelete)) or
   (ACommand in [20..20 + EventLabels.Count - 1]) or //Label
   (ACommand in [10..13]);   //TimeLine
end;

procedure TcxSchedulerEventPopupMenu.SetEventLabelColor(AColor: Integer);
begin
  if Event = nil then Exit;
  if Event.EventType = etOccurrence then
    Event.Pattern.LabelColor := AColor
  else
    Event.Source.LabelColor := AColor;
end;

procedure TcxSchedulerEventPopupMenu.SetEventState(AState: Integer);
begin
  if Event = nil then Exit;
  Event.State := AState;
  Scheduler.EventList.PostEvent(Event);
end;

procedure TcxSchedulerEventPopupMenu.CreateDeleteItem;

  function CanDelete: Boolean;
  var
    I: Integer;
  begin
    with Scheduler do
    begin
      Result := EventOperations.Deleting;
      if Result then
      begin
        for I := 0 to SelectedEventCount - 1 do
          if not SelectedEvents[I].ReadOnly then
            Exit;
        Result := False;
      end;
    end;
  end;

begin
  if epmiDelete in Items then
  begin
    AddValidSeparator(Root);
    CreateSubItem(Root, cxGetResourceString(@scxpmDelete), Ord(epmiDelete), 10).Enabled := CanDelete;
  end;
end;

procedure TcxSchedulerEventPopupMenu.CreateLabelItems;
var
  AOwner: TMenuItem;
  I: Integer;

  function GetColorChecked(AColor: TColor): Boolean;
  begin
    if Event = nil then
      Result := False
    else
      Result := ColorToRgb(Event.LabelColor) = ColorToRgb(AColor);
  end;

  procedure CreateEventColorItem(ALabel: TcxSchedulerEventLabel);
  var
    AChecked: Boolean;
    AItem: TMenuItem;
  begin
    AChecked := GetColorChecked(ALabel.Color);
    AItem := CreateSubItem(AOwner, ALabel.Caption,
      GetCommand(epmiLabel, ALabel.Index), -1, CanEdit, AChecked);
    AItem.Default := AChecked;
    EventLabels.Images.GetBitmap(ALabel.Index, AItem.Bitmap);
  end;

begin
  if not (epmiLabel in Items) or not Scheduler.StorageActive or
    not Storage.IsLabelColorAvailable or (EventLabels.Count = 0) then Exit;
  AOwner := CreateSubItem(Root, cxGetResourceString(@scxpmLabel), Ord(epmiLabel));
  //label colors
  CreateEventColorItem(EventLabels[0]);
  if EventLabels.Count > 1 then
  begin
    CreateSeparator(AOwner);
    for I := 1 to EventLabels.Count - 1 do
      CreateEventColorItem(EventLabels[I]);
  end;
  AOwner.Enabled := not (Assigned(Event) and Event.OpenedInEditor);
end;

procedure TcxSchedulerEventPopupMenu.CreateTimeItems;
var
  AOwner: TMenuItem;
  ACanSetNonFree: Boolean;

  function GetTimeLineChecked(AIndex: Integer): Boolean;
  begin
    if Event = nil then
      Result := False
    else
      Result := Integer(Event.State) = AIndex;
  end;

  function CanSetNonFree: Boolean;
  var
    APrevState: Integer;
  begin
    if Event = nil then
    begin
      Result := False;
      Exit;
    end;
    APrevState := Event.State;
    Event.State := tlsBusy;
    try
      Result := Scheduler.CanIntersect(Event);
    finally
      Event.State := APrevState;
    end;
  end;

  procedure CreateTimeLineItem(const ACaption: string; AIndex: Integer);
  var
    AChecked: Boolean;
  begin
    AChecked := GetTimeLineChecked(AIndex);
    CreateSubItem(AOwner, ACaption, GetCommand(epmiShowTimeAs, AIndex), AIndex,
      CanEdit and ((AIndex = 0) or (ACanSetNonFree)), AChecked).Default := AChecked;
  end;

begin
  if not (epmiShowTimeAs in Items) or not Scheduler.StorageActive or
    not Storage.IsStateAvailable then Exit;
  AOwner := CreateSubItem(Root, cxGetResourceString(@scxpmShowTimeAs), Ord(epmiShowTimeAs));
  AOwner.SubMenuImages := TimeLinePatterns;
  // TimeLine styles
  ACanSetNonFree := CanSetNonFree;
  CreateTimeLineItem(cxGetResourceString(@scxpmFree), 0);
  CreateTimeLineItem(cxGetResourceString(@scxpmTentative), 1);
  CreateTimeLineItem(cxGetResourceString(@scxpmBusy), 2);
  CreateTimeLineItem(cxGetResourceString(@scxpmOutOfOffice), 3);
  AOwner.Enabled := not (Assigned(Event) and Event.OpenedInEditor);
end;

function TcxSchedulerEventPopupMenu.GetCommand(
  AItem: TcxSchedulerEventPopupMenuItem; ASubItemIndex: Integer): Integer;
begin
  Result := Ord(AItem);
  case AItem of
    epmiShowTimeAs:
      if (ASubItemIndex >= 0) and (ASubItemIndex <= 3) then
        Result := 10 + ASubItemIndex;
    epmiLabel:
      if (ASubItemIndex >= 0) and (ASubItemIndex < EventLabels.Count) then
        Result := 20 + ASubItemIndex;
  end;
end;

function TcxSchedulerEventPopupMenu.CanEdit: Boolean;
begin
  Result := (Event <> nil) and not Event.ReadOnly and
    not Scheduler.EventOperations.ReadOnly;
end;

function TcxSchedulerEventPopupMenu.GetEvent: TcxSchedulerControlEvent;
begin
  with Scheduler.EventList.Selection do
  begin
    if Count = 1 then
      Result := Items[0]
    else
      Result := nil;
  end;
end;

procedure TcxSchedulerEventPopupMenu.UnpackCommand(ACommand: Integer;
  out AItem: TcxSchedulerEventPopupMenuItem; out ASubItemIndex: Integer);
begin
  AItem := epmiOpen;
  ASubItemIndex := -1;
  if not IsValidCommand(ACommand) then Exit;
  if ACommand in [20..20 + EventLabels.Count - 1] then
  begin
    AItem := epmiLabel;
    ASubItemIndex := ACommand - 20;
  end
  else
    if ACommand in [10..13] then
    begin
      AItem := epmiShowTimeAs;
      ASubItemIndex := ACommand - 10;
    end
    else
      AItem := TcxSchedulerEventPopupMenuItem(ACommand);
end;

end.

{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGrid;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages,
  Classes, Controls, Graphics, ImgList, Forms, StdCtrls,
  dxCore, dxCoreClasses, dxMessages, cxClasses, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, cxControls, cxPC, cxNavigator, cxCustomData,
  cxGridCommon, cxGridLevel, cxGridDetailsSite, cxGridCustomView, dxTouch;

const
(*
  CM_DEFERUPDATES = WM_USER + 333;
*)

  cxGridLevelTabsDefaultCaptionAlignment = taCenter;
  cxGridLevelTabsDefaultImageBorder = 0;
  cxGridRootLevelDefaultDetailFrameWidth = 0;
  cxGridDefaultDragOpeningWaitTime = 800;//500;

type
  TcxGridViewInfo = class;
  TcxCustomGrid = class;

  { changes }

  TcxGridLayoutChange = class(TcxCustomGridViewChange)
  public
    procedure Execute; override;
    function IsCompatibleWith(AChange: TcxCustomGridChange): Boolean; override;
  end;

  TcxGridSizeChange = class(TcxCustomGridViewChange)
  private
    FKeepMaster: Boolean;
    FUpdateGridViewOnly: Boolean;
  public
    constructor Create(AGridView: TcxCustomGridView;
      AUpdateGridViewOnly: Boolean = False; AKeepMaster: Boolean = False); reintroduce; virtual;
    procedure Execute; override;
    function IsCompatibleWith(AChange: TcxCustomGridChange): Boolean; override;
    function IsEqual(AChange: TcxCustomGridChange): Boolean; override;
    property KeepMaster: Boolean read FKeepMaster;
    property UpdateGridViewOnly: Boolean read FUpdateGridViewOnly
      write FUpdateGridViewOnly;
  end;

  TcxGridViewChange = class(TcxCustomGridViewChange)
  private
    FUpdateBounds: TRect;
    FUpdateRegion: TcxRegion;
  public
    constructor Create(AGridView: TcxCustomGridView;
      const AUpdateBounds: TRect); reintroduce; overload; virtual;
    constructor Create(AGridView: TcxCustomGridView;
      const AUpdateRegion: TcxRegion); reintroduce; overload; virtual;
    constructor Create(AGridView: TcxCustomGridView); reintroduce; overload;
    destructor Destroy; override;
    procedure Execute; override;
    function IsCompatibleWith(AChange: TcxCustomGridChange): Boolean; override;
    function IsEqual(AChange: TcxCustomGridChange): Boolean; override;
    property UpdateBounds: TRect read FUpdateBounds;
    property UpdateRegion: TcxRegion read FUpdateRegion;
  end;

  { notification }

  TcxGridNotificationKind = (gnkFocusedViewChanged, gnkFocusedRecordChanged,
    gnkRecordCountChanged, gnkContextMenu, gnkCustomization, gnkKeyDown);
  TcxGridNotificationKinds = set of TcxGridNotificationKind;

  TcxCustomGridNotification = class
  protected
    function NotificationKinds: TcxGridNotificationKinds; virtual; abstract;
    procedure Notify(AKind: TcxGridNotificationKind; AData: TObject;
      var AHandled: Boolean); virtual; abstract;
  end;

  { structure navigator }

  TcxCustomGridStructureNavigatorClass = class of TcxCustomGridStructureNavigator;

  TcxCustomGridStructureNavigator = class(TcxControl)
  private
    FGrid: TcxCustomGrid;
  protected
    procedure Changed; virtual;
    procedure FontChanged; override;
    function MayFocus: Boolean; override;

    function CalculateBoundsRect: TRect; virtual; abstract;
  public
    constructor Create(AGrid: TcxCustomGrid); reintroduce; virtual;
    procedure BeforeGridLoading; virtual;
    function CanAddComponent: Boolean; virtual; abstract;
    function CanDeleteComponent(AComponent: TComponent): Boolean; virtual; abstract;
    procedure GetSelection(AList: TList); virtual; abstract;
    function IsObjectSelected(AObject: TPersistent): Boolean; virtual; abstract;
    procedure NotifyEditors; virtual; abstract;
    procedure SelectionChanged(ASelection: TList); virtual; abstract;
    procedure SelectObject(AObject: TPersistent; AClearSelection: Boolean); virtual; abstract;
    procedure SelectObjects(AObjects: TList); virtual; abstract;
    procedure UnselectObject(AObject: TPersistent); virtual; abstract;
    property Grid: TcxCustomGrid read FGrid;
  end;

  { handlers }

  TcxCustomGridHandler = class
  private
    FControl: TcxCustomGrid;
    function GetActiveController: TcxCustomGridController;
    function GetActiveGridView: TcxCustomGridView;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetViewInfo: TcxGridViewInfo;
  protected
    property ActiveController: TcxCustomGridController read GetActiveController;
    property ActiveGridView: TcxCustomGridView read GetActiveGridView;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property ViewInfo: TcxGridViewInfo read GetViewInfo;
  public
    constructor Create(AControl: TcxCustomGrid); virtual;
    function UseRightToLeftAlignment: Boolean;
    property Control: TcxCustomGrid read FControl;
  end;

  TcxGridDesignControllerClass = class of TcxGridDesignController;

  TcxGridDesignController = class(TcxCustomGridDesignController)
  private
    FControl: TcxCustomGrid;
  protected
    function GetControl: TcxControl; override;
  public
    constructor Create(AControl: TcxCustomGrid); virtual;
  end;

  TcxGridDragOpenInfoTab = class(TcxCustomGridDragOpenInfo)
  public
    Level: TcxGridLevel;
    constructor Create(ALevel: TcxGridLevel); virtual;
    function Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean; override;
    procedure Run; override;
  end;

  TcxGridControllerClass = class of TcxGridController;

  TcxGridController = class(TcxCustomGridHandler)
  private
    FDesignController: TcxGridDesignController;
    FDragOpenInfo: TcxCustomGridDragOpenInfo;
    FDragOpenTimer: TcxTimer;
    function GetDesignController: TcxGridDesignController;
    procedure DragOpenTimerHandler(Sender: TObject);
  protected
    function GetDesignControllerClass: TcxGridDesignControllerClass; virtual;
  public
    destructor Destroy; override;

    procedure DoCancelMode; virtual;
    procedure FocusChanged; virtual;

    function GetCursor(X, Y: Integer): TCursor; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;

    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure StartDrag(var DragObject: TDragObject); virtual;

    function GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo; virtual;
    function IsDragOpenHitTest(AHitTest: TcxCustomGridHitTest;
      out ADragOpenInfo: TcxCustomGridDragOpenInfo): Boolean;
    procedure StartDragOpen(ADragOpenInfo: TcxCustomGridDragOpenInfo);
    procedure StopDragOpen;
    property DragOpenInfo: TcxCustomGridDragOpenInfo read FDragOpenInfo;

    property DesignController: TcxGridDesignController read GetDesignController;
  end;

  TcxGridPainterClass = class of TcxGridPainter;

  TcxGridPainter = class(TcxCustomGridHandler)
  private
    function GetCanvas: TcxCanvas;
  protected
    procedure DrawDetailsSite; virtual;
    //procedure DrawEmptyArea; virtual;
  public
    procedure Invalidate(AInvalidateDetails: Boolean); overload; virtual;
    procedure Invalidate(const R: TRect); overload; virtual;
    procedure Paint; virtual;
    property Canvas: TcxCanvas read GetCanvas;
  end;

  TcxGridTopDetailsSiteViewInfoClass = class of TcxGridTopDetailsSiteViewInfo;

  TcxGridTopDetailsSiteViewInfo = class(TcxCustomGridDetailsSiteViewInfo)
  private
    function GetControl: TcxCustomGrid;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetActiveGridView: TcxCustomGridView; override;
    function GetActiveLevel: TcxGridLevel; override;
    function GetCanvas: TcxCanvas; override;
    function GetContainer: TcxControl; override;
    function GetDesignController: TcxCustomGridDesignController; override;
    function GetMasterRecord: TObject; override;
    function GetMaxHeight: Integer; override;
    function GetMaxWidth: Integer; override;
    procedure InitTabHitTest(AHitTest: TcxGridDetailsSiteTabHitTest); override;
    property Control: TcxCustomGrid read GetControl;
  public
    procedure ChangeActiveTab(ALevel: TcxGridLevel; AFocusView: Boolean = False); override;
    function DetailHasData(ALevel: TcxGridLevel): Boolean; override;
    function SupportsTabAccelerators: Boolean; override;
    procedure VisibilityChanged(AVisible: Boolean); override;
  end;

  TcxGridViewInfoClass = class of TcxGridViewInfo;

  TcxGridViewInfo = class(TcxCustomGridHandler)
  private
    FDetailsSiteViewInfo: TcxGridTopDetailsSiteViewInfo;
    FDetailsSiteViewInfoCachedInfo: TcxCustomGridDetailsSiteViewInfoCachedInfo;
    FIsCalculating: Boolean;
    function GetBounds: TRect;
    function GetClientBounds: TRect;
    function GetEmptyAreaColor: TColor;
  protected
    procedure CreateViewInfos; virtual;
    procedure DestroyViewInfos; virtual;
    function GetDetailsSiteViewInfoClass: TcxGridTopDetailsSiteViewInfoClass; virtual;
    procedure RecreateViewInfos;

    procedure Calculate; virtual;
  public
    constructor Create(AControl: TcxCustomGrid); override;
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetHitTest(X, Y: Integer): TcxCustomGridHitTest; virtual;
    procedure MainCalculate;
    property Bounds: TRect read GetBounds;
    property ClientBounds: TRect read GetClientBounds;
    property DetailsSiteViewInfo: TcxGridTopDetailsSiteViewInfo read FDetailsSiteViewInfo;
    property EmptyAreaColor: TColor read GetEmptyAreaColor;
    property IsCalculating: Boolean read FIsCalculating;
  end;

  { controls }

  TcxGridLevelTabsClass = class of TcxGridLevelTabs;

  TcxGridLevelTabs = class(TPersistent)
  strict private
    FCaptionAlignment: TAlignment;
    FFreeNotificator: TcxFreeNotificator;
    FImageBorder: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FOwner: TcxCustomGrid;
    FSlants: TcxTabSlants;
    FStyle: TcxPCStyleID;

    procedure SetCaptionAlignment(Value: TAlignment);
    procedure SetImageBorder(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetSlants(Value: TcxTabSlants);
    procedure SetStyle(Value: TcxPCStyleID);
    procedure FreeNotification(Sender: TComponent);
    procedure ImagesChanged(Sender: TObject);
    procedure SlantsChanged(Sender: TObject);
  protected
    procedure Changed;
    procedure ChangeScale(M, D: Integer); virtual;
  public
    constructor Create(AOwner: TcxCustomGrid); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetImages: TCustomImageList;

    property Owner: TcxCustomGrid read FOwner;
  published
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default cxGridLevelTabsDefaultCaptionAlignment;
    property ImageBorder: Integer read FImageBorder write SetImageBorder default cxGridLevelTabsDefaultImageBorder;
    property Images: TCustomImageList read FImages write SetImages;
    property Slants: TcxTabSlants read FSlants write SetSlants;
    property Style: TcxPCStyleID read FStyle write SetStyle default cxPCDefaultStyle;
  end;

  TcxGridRootLevelOptions = class(TcxGridLevelOptions)
  public
    constructor Create(ALevel: TcxGridLevel); override;
  published
    property DetailFrameWidth default cxGridRootLevelDefaultDetailFrameWidth;
  end;

  TcxGridRootLevel = class(TcxGridLevel)
  protected
    function GetOptionsClass: TcxGridLevelOptionsClass; override;
  end;

  { TcxGridLockedStateImageOptions }

  TcxGridLockedStateImageOptionsClass = class of TcxGridLockedStateImageOptions;
  TcxGridLockedStateImageOptions = class(TcxLockedStateImageOptions)
  strict private
    FGrid: TcxCustomGrid;
  protected
    function GetFont: TFont; override;
    function IsTextStored: Boolean; override;

    property Grid: TcxCustomGrid read FGrid;
  public
    constructor Create(AGrid: TcxCustomGrid); reintroduce; virtual;
  published
    property AssignedValues;
    property Color;
    property Effect;
    property Enabled;
    property Font;
    property ShowText;
    property Text;
  end;

  { TcxGridLockedStatePaintHelper }

  TcxGridLockedStatePaintHelper = class(TcxLockedStatePaintHelper)
  private
    function GetGrid: TcxCustomGrid;
  protected
    //override
    procedure AfterDestroyingImage; override;
    procedure BeforeCreatingImage; override;
    function CanCreateLockedImage: Boolean; override;
    function DoPrepareImage: Boolean; override;
    function GetOptions: TcxLockedStateImageOptions; override;
    function GetControl: TcxControl; override;

    property Grid: TcxCustomGrid read GetGrid;
  end;
  TcxGridLockedStatePaintHelperClass = class of TcxGridLockedStatePaintHelper;

  TcxGridActiveTabChangedEvent = procedure(Sender: TcxCustomGrid; ALevel: TcxGridLevel) of object;
  TcxGridActiveTabChangedExEvent = procedure(Sender: TcxCustomGrid; ALevel: TcxGridLevel;
    ARecordIndex: Integer; var ADone: Boolean) of object;
  TcxGridFocusedViewChangedEvent = procedure(Sender: TcxCustomGrid;
    APrevFocusedView, AFocusedView: TcxCustomGridView) of object;
  TcxGridLayoutChangedEvent = procedure(Sender: TcxCustomGrid; AGridView: TcxCustomGridView) of object;
  TcxGridPrepareLockedStateImageEvent = procedure(Sender: TcxCustomGrid; AImage: TcxBitmap32; var ADone: Boolean) of object;

  TcxCustomGrid = class(TcxControl,
    IcxNavigator,
    IcxNavigatorRecordPosition,
    IdxSkinSupport,
    IcxLockedStatePaint,
    IcxLockedStateFontChanged)
  private
    FActiveLevel: TcxGridLevel;
    FChanges: TList;
    FController: TcxGridController;
    FCreatingStructureNavigator: Boolean;
    FDragOpening: Boolean;
    FDragOpeningWaitTime: Integer;
    FFocusedView: TcxCustomGridView;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FIsExportMode: Boolean;
    FIsPopupControl: Boolean;
    FLastFocused: Boolean;
    FLevels: TcxGridLevel;
    FLevelTabs: TcxGridLevelTabs;
    FLockedStatePaintHelper: TcxGridLockedStatePaintHelper;
    FLockedStateImageOptions: TcxGridLockedStateImageOptions;
    FNavigatorNotifier: TcxNavigatorControlNotifier;
    FNotifications: TList;
    FPainter: TcxGridPainter;
    FStructureNavigator: TcxCustomGridStructureNavigator;
    FTabStop: Boolean;
    FTag: TObject;
    FUpdateCount: Integer;
    FUpdateLockCount: Integer;
    FViewInfo: TcxGridViewInfo;
    FViews: TList;
    FOnActiveTabChanged: TcxGridActiveTabChangedEvent;
    FOnActiveTabChangedEx: TcxGridActiveTabChangedExEvent;
    FOnFocusedViewChanged: TcxGridFocusedViewChangedEvent;
    FOnLayoutChanged: TcxGridLayoutChangedEvent;
    FOnPrepareLockedStateImage: TcxGridPrepareLockedStateImageEvent;
    FSubClassEvents: TNotifyEvent;

    function GetActiveView: TcxCustomGridView;
    function GetFocusedViewNavigator: IcxNavigator;
    function GetView(Index: Integer): TcxCustomGridView;
    function GetViewCount: Integer;
    function GetRootLevelOptions: TcxGridLevelOptions;
    function GetRootLevelStyles: TcxGridLevelStyles;
    function GetStructureNavigator: TcxCustomGridStructureNavigator;
    function GetUpdateLocked: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetActiveLevel(Value: TcxGridLevel);
    procedure SetDragOpeningWaitTime(Value: Integer);
    procedure SetFocusedView(Value: TcxCustomGridView);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLevels(Value: TcxGridLevel);
    procedure SetLevelTabs(Value: TcxGridLevelTabs);
    procedure SetLockedStateImageOptions(Value: TcxGridLockedStateImageOptions);
    procedure SetRootLevelOptions(Value: TcxGridLevelOptions);
    procedure SetRootLevelStyles(Value: TcxGridLevelStyles);
    procedure SetTabStop(Value: Boolean);

    procedure AddView(AView: TcxCustomGridView);
    procedure RemoveView(AView: TcxCustomGridView);
    procedure DestroyViews;

    procedure DestroyChanges(AChanges: TList);
    procedure DestroyViewChanges(AView: TcxCustomGridView);

    procedure CreateStructureNavigator;

    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    //procedure CMDeferUpdates(var Message: TMessage); message CM_DEFERUPDATES;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    // IcxNavigator
    function IcxNavigator.IsActive = NavigatorIsActive;
    function IcxNavigator.IsBof = NavigatorIsBof;
    function IcxNavigator.IsEof = NavigatorIsEof;
    function IcxNavigator.CanAppend = NavigatorCanAppend;
    function IcxNavigator.CanEdit = NavigatorCanEdit;
    function IcxNavigator.CanDelete = NavigatorCanDelete;
    function IcxNavigator.CanInsert = NavigatorCanInsert;
    procedure IcxNavigator.ClearBookmark = NavigatorClearBookmark;
    function IcxNavigator.IsEditing = NavigatorIsEditing;
    function IcxNavigator.IsBookmarkAvailable = NavigatorIsBookmarkAvailable;
    procedure IcxNavigator.DoAction = NavigatorDoAction;
    function IcxNavigator.GetNotifier = NavigatorGetNotifier;
    function IcxNavigator.IsActionSupported = NavigatorIsActionSupported;
    function NavigatorIsActive: Boolean;
    function NavigatorIsBof: Boolean;
    function NavigatorIsEof: Boolean;
    function NavigatorCanAppend: Boolean;
    function NavigatorCanEdit: Boolean;
    function NavigatorCanDelete: Boolean;
    function NavigatorCanInsert: Boolean;
    function NavigatorIsEditing: Boolean;
    procedure NavigatorClearBookmark;
    function NavigatorIsBookmarkAvailable: Boolean;
    procedure NavigatorDoAction(AButtonIndex: Integer);
    function NavigatorGetNotifier: TcxNavigatorControlNotifier;
    function NavigatorIsActionSupported(AButtonIndex: Integer): Boolean;
     // IcxNavigatorRecordPosition
    function IcxNavigatorRecordPosition.GetRecordCount = NavigatorGetRecordCount;
    function IcxNavigatorRecordPosition.GetRecordIndex = NavigatorGetRecordIndex;
    function NavigatorGetRecordCount: Integer;
    function NavigatorGetRecordIndex: Integer;
    // IcxLockedStatePaint
    function IcxLockedStatePaint.GetImage = GetLockedStateImage;
    function IcxLockedStatePaint.GetTopmostControl = GetLockedStateTopmostControl;
    function GetLockedStateImage: TcxBitmap32;
    function GetLockedStateTopmostControl: TcxControl;
    // IcxLockedStateFontChanged
    procedure IcxLockedStateFontChanged.FontChanged = UpdateLockedStateFont;
    procedure UpdateLockedStateFont(AFont: TFont);
    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function IsGestureTarget(AWnd: THandle): Boolean; override;
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    procedure AddChildComponent(AComponent: TcxControlChildComponent); override;
    procedure RemoveChildComponent(AComponent: TcxControlChildComponent); override;

    procedure BoundsChanged; override;
    procedure ChangeScaleEx(M, D: Integer; IsDpiChange: Boolean); override;
    procedure DoCancelMode; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoPaint; override;
    procedure FocusChanged; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetIsFocused: Boolean; override;
    procedure Loaded; override;
    function MayFocus: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure WndProc(var Message: TMessage); override;

    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;

    procedure CreateHandlers; virtual;
    procedure DestroyHandlers; virtual;

    function DoPrepareLockedStateImage: Boolean; virtual;
    function GetLockedStatePaintHelperClass: TcxGridLockedStatePaintHelperClass; virtual;
    function GetLockedStateImageOptionsClass: TcxGridLockedStateImageOptionsClass; virtual;

    property LockedStatePaintHelper: TcxGridLockedStatePaintHelper read FLockedStatePaintHelper;
    property OnPrepareLockedStateImage: TcxGridPrepareLockedStateImageEvent read FOnPrepareLockedStateImage write FOnPrepareLockedStateImage;

    procedure DoActiveTabChanged(ALevel: TcxGridLevel); virtual;
    procedure DoActiveTabChangedEx(ALevel: TcxGridLevel; ARecordIndex: Integer); virtual;
    procedure DoChange(AGridChange: TcxCustomGridChange);
    procedure DoLayoutChanged(AGridView: TcxCustomGridView); virtual;
    procedure DoUpdate(AChanges: TList);
    procedure FocusedViewChanged(APrevFocusedView, AFocusedView: TcxCustomGridView); virtual;
    function GetControllerClass: TcxGridControllerClass; virtual;
    function GetDefaultViewClass: TcxCustomGridViewClass; virtual;
    function GetLevelsClass: TcxGridLevelClass; virtual;
    function GetLevelTabsClass: TcxGridLevelTabsClass; virtual;
    function GetPainterClass: TcxGridPainterClass; virtual;
    function GetViewInfoClass: TcxGridViewInfoClass; virtual;
    function IsUpdating: Boolean;
    procedure LevelChanged(ALevel: TcxGridLevel; AChangeKind: TcxGridLevelChangeKind);
    procedure RefreshNavigators;
    procedure StructureNavigatorChanged;
    function UpdateOnRootViewDataChange: Boolean; virtual;
    procedure ViewChanged(AView: TcxCustomGridView; AChangeKind: TcxGridViewChangeNotificationKind);

    procedure UpdateFocusing(AChildFocused: Boolean);
    property LastFocused: Boolean read FLastFocused;

    property FocusedViewNavigator: IcxNavigator read GetFocusedViewNavigator;
    property IsExportMode: Boolean read FIsExportMode;
    property IsPopupControl: Boolean read FIsPopupControl write FIsPopupControl;
    property NavigatorNotifier: TcxNavigatorControlNotifier read FNavigatorNotifier;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Invalidate(AHardUpdate: Boolean = False); reintroduce;
    procedure RemoveFocus(AGoForward: Boolean);

    procedure BeginLockedStatePaint(AMode: TcxGridShowLockedStateImageMode);
    procedure EndLockedStatePaint;
    procedure BeginUpdate(AMode: TcxGridShowLockedStateImageMode = lsimNever);
    procedure CancelUpdate;
    procedure BeginExport;
    procedure EndExport;
    procedure Changed(AGridChange: TcxCustomGridChange);
    procedure CheckFocusedView;
    procedure EndUpdate;
    procedure LayoutChanged; virtual;

    procedure SizeChanged; virtual;
    property UpdateLocked: Boolean read GetUpdateLocked;

    function CreateView(AViewClass: TcxCustomGridViewClass): TcxCustomGridView;
    function ViewExists(AView: TcxCustomGridView): Boolean;

    procedure RegisterNotification(ANotification: TcxCustomGridNotification);
    procedure UnregisterNotification(ANotification: TcxCustomGridNotification);
    function SendNotifications(AKind: TcxGridNotificationKind; AData: TObject = nil): Boolean;

    procedure RootViewDataChanged(AView: TcxCustomGridView); virtual;

    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property ActiveView: TcxCustomGridView read GetActiveView;
    property ActiveLevel: TcxGridLevel read FActiveLevel write SetActiveLevel;
    property BorderStyle default cxcbsDefault;
    property Color;
    property Controller: TcxGridController read FController;
    property DragOpening: Boolean read FDragOpening write FDragOpening default True;
    property DragOpeningWaitTime: Integer read FDragOpeningWaitTime write SetDragOpeningWaitTime default cxGridDefaultDragOpeningWaitTime;
    property FocusedView: TcxCustomGridView read FFocusedView write SetFocusedView;
    property Font;
    property Images: TCustomImageList read FImages write SetImages;
    property Levels: TcxGridLevel read FLevels write SetLevels;
    property LevelTabs: TcxGridLevelTabs read FLevelTabs write SetLevelTabs;
    property LockedStateImageOptions: TcxGridLockedStateImageOptions read FLockedStateImageOptions write SetLockedStateImageOptions;
    property LookAndFeel;
    property LookAndFeelPainter;
    property Painter: TcxGridPainter read FPainter;
    property RootLevelOptions: TcxGridLevelOptions read GetRootLevelOptions write SetRootLevelOptions;
    property RootLevelStyles: TcxGridLevelStyles read GetRootLevelStyles write SetRootLevelStyles;
    property StructureNavigator: TcxCustomGridStructureNavigator read GetStructureNavigator;
    property TabStop: Boolean read FTabStop write SetTabStop default True;
    property ViewCount: Integer read GetViewCount;
    property ViewInfo: TcxGridViewInfo read FViewInfo;
    property Views[Index: Integer]: TcxCustomGridView read GetView;
    property OnActiveTabChanged: TcxGridActiveTabChangedEvent read FOnActiveTabChanged write FOnActiveTabChanged;
    property OnActiveTabChangedEx: TcxGridActiveTabChangedExEvent read FOnActiveTabChangedEx write FOnActiveTabChangedEx;
    property OnFocusedViewChanged: TcxGridFocusedViewChangedEvent read FOnFocusedViewChanged write FOnFocusedViewChanged;
    property OnLayoutChanged: TcxGridLayoutChangedEvent read FOnLayoutChanged write FOnLayoutChanged;
  published
    property RootLevelStylesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
  end;

  TcxGrid = class(TcxCustomGrid)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    property DragOpening;
    property DragOpeningWaitTime;
    property LevelTabs;
    property LockedStateImageOptions;
    property LookAndFeel;
    property RootLevelOptions;
    property RootLevelStyles;

    property OnActiveTabChanged;
    property OnFocusedViewChanged;
    property OnLayoutChanged;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPrepareLockedStateImage;
  end;

  { TcxGridViewRepository }

  TcxGridViewRepository = class(TComponent)  {5}
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxCustomGridView;
    procedure DestroyItems;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure AddItem(AItem: TcxCustomGridView);
    procedure RemoveItem(AItem: TcxCustomGridView);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem(AItemClass: TcxCustomGridViewClass): TcxCustomGridView;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxCustomGridView read GetItem; default;
  end;

var
  cxGridStructureNavigatorClass: TcxCustomGridStructureNavigatorClass;

function GetParentGrid(AControl: TControl): TcxCustomGrid;

implementation

uses
  SysUtils, Math, cxGridCustomTableView, dxOffice11, cxGeometry, cxGridStrs;

function GetLevel(ACaller: TComponent; Index: Integer): TComponent;
begin
  Result := TComponent(TList(TcxCustomGrid(ACaller).FTag)[Index]);
end;

function GetView(ACaller: TComponent; Index: Integer): TComponent;
begin
  Result := TcxCustomGrid(ACaller).Views[Index];
end;

{ TcxGridLayoutChange }

procedure TcxGridLayoutChange.Execute;
begin
  if GridView <> nil then
  begin
    if TcxCustomGridViewAccess.GetChangeable(GridView) and
      not IsRectEmpty(GridView.Site.BoundsRect) then
    begin
      GridView.ViewInfo.Recalculate;
      GridView.Painter.Invalidate;
    end;
  end
  else
    if (Control <> nil) and (TcxCustomGrid(Control).ViewInfo <> nil) then
    begin
      TcxCustomGrid(Control).ViewInfo.MainCalculate;
      TcxCustomGrid(Control).Painter.Invalidate(True);
    end;

  if Control <> nil then  {7}
  begin
    TcxCustomGrid(Control).DoLayoutChanged(GridView);
    if IsControlVisible(Control) then
      SendMessage(Control.Handle, DXM_UIADORNERMANAGERUPDATE, 0, 0);
  end;
end;

function TcxGridLayoutChange.IsCompatibleWith(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsCompatibleWith(AChange) or
    (AChange is TcxGridLayoutChange) and
      ((TcxGridLayoutChange(AChange).GridView = nil) or
       (GridView <> nil) and GridView.HasAsMaster(TcxGridLayoutChange(AChange).GridView)) or
    (AChange is TcxGridSizeChange) and
      ((TcxGridSizeChange(AChange).GridView = nil) or
       (GridView <> nil) and
         ((GridView = TcxGridSizeChange(AChange).GridView) or
          GridView.HasAsMaster(TcxGridSizeChange(AChange).GridView)));
end;

{ TcxGridSizeChange }

constructor TcxGridSizeChange.Create(AGridView: TcxCustomGridView;
  AUpdateGridViewOnly: Boolean = False; AKeepMaster: Boolean = False);
begin
  inherited Create(AGridView);
  FUpdateGridViewOnly := AUpdateGridViewOnly;
  FKeepMaster := AKeepMaster;
end;

procedure TcxGridSizeChange.Execute;
begin
  if GridView = nil then
    with TcxCustomGrid(Control) do
      if ActiveView <> nil then
        ActiveView.SizeChanged(False, FKeepMaster)
      else
        LayoutChanged
  else
    with GridView do
      if TcxCustomGridViewAccess.GetChangeable(GridView) then
      begin
        if ViewInfoCache <> nil then
          ViewInfoCache.UnassignValues(FKeepMaster);
        if FUpdateGridViewOnly and FKeepMaster then
          LayoutChanged
        else
          if MasterGridView = nil then
            if {(Control = nil) or }IsPattern then
              {//}LayoutChanged
            else
              TcxCustomGrid(Control).LayoutChanged
          else
            if not IsPattern and not MasterGridView.SizeChanged(False, FKeepMaster) then
              LayoutChanged;
      end;
end;

function TcxGridSizeChange.IsCompatibleWith(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsCompatibleWith(AChange) or
    (AChange is TcxGridSizeChange) and (TcxGridSizeChange(AChange).GridView = GridView) and
    (TcxGridSizeChange(AChange).KeepMaster = FKeepMaster) and FUpdateGridViewOnly or
    (AChange is TcxGridDataChange) and (TcxGridDataChange(AChange).GridView = GridView);
end;

function TcxGridSizeChange.IsEqual(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsEqual(AChange) and
    (FKeepMaster = TcxGridSizeChange(AChange).KeepMaster) and
    (FUpdateGridViewOnly = TcxGridSizeChange(AChange).UpdateGridViewOnly);
end;

{ TcxGridViewChange }

constructor TcxGridViewChange.Create(AGridView: TcxCustomGridView;
  const AUpdateBounds: TRect);
begin
  inherited Create(AGridView);
  FUpdateBounds := AUpdateBounds;
end;

constructor TcxGridViewChange.Create(AGridView: TcxCustomGridView;
  const AUpdateRegion: TcxRegion);
begin
  inherited Create(AGridView);
  FUpdateRegion := TcxRegion.Create;
  FUpdateRegion.Combine(AUpdateRegion, roSet, False);
end;

constructor TcxGridViewChange.Create(AGridView: TcxCustomGridView);
begin
  Create(AGridView, Rect(0, 0, 0, 0));
end;

destructor TcxGridViewChange.Destroy;
begin
  FUpdateRegion.Free;
  inherited;
end;

procedure TcxGridViewChange.Execute;
begin
  if not IsRectEmpty(FUpdateBounds) then
    GridView.Painter.Invalidate(FUpdateBounds)
  else
    if FUpdateRegion <> nil then
      GridView.Painter.Invalidate(FUpdateRegion)
    else
      GridView.Painter.Invalidate;
end;

function TcxGridViewChange.IsCompatibleWith(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsCompatibleWith(AChange) or
    (AChange is TcxGridLayoutChange) and (TcxGridLayoutChange(AChange).GridView = GridView) or
    (AChange is TcxGridSizeChange) and (TcxGridSizeChange(AChange).GridView = GridView);
end;

function TcxGridViewChange.IsEqual(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsEqual(AChange) and
    ((FUpdateRegion = nil) and (TcxGridViewChange(AChange).UpdateRegion = nil) and
     EqualRect(FUpdateBounds, TcxGridViewChange(AChange).UpdateBounds) or
     (FUpdateRegion <> nil) and (TcxGridViewChange(AChange).UpdateRegion <> nil) and
     FUpdateRegion.IsEqual(TcxGridViewChange(AChange).UpdateRegion));
end;

{ TcxCustomGridStructureNavigator }

constructor TcxCustomGridStructureNavigator.Create(AGrid: TcxCustomGrid);
begin
  inherited Create(nil);
  FGrid := AGrid;
  Parent := FGrid;
end;

procedure TcxCustomGridStructureNavigator.Changed;
begin
  BoundsRect := CalculateBoundsRect;
  BringToFront;
  Invalidate;
end;

procedure TcxCustomGridStructureNavigator.FontChanged;
begin
  inherited;
  Changed;
end;

function TcxCustomGridStructureNavigator.MayFocus: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridStructureNavigator.BeforeGridLoading;
begin
end;

{ TcxCustomGridHandler }

constructor TcxCustomGridHandler.Create(AControl: TcxCustomGrid);
begin
  inherited Create;
  FControl := AControl;
end;

function TcxCustomGridHandler.GetActiveController: TcxCustomGridController;
begin
  if ActiveGridView <> nil then
    Result := ActiveGridView.Controller
  else
    Result := nil;
end;

function TcxCustomGridHandler.GetActiveGridView: TcxCustomGridView;
begin
  Result := FControl.ActiveView;
end;

function TcxCustomGridHandler.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FControl.LookAndFeelPainter;
end;

function TcxCustomGridHandler.GetViewInfo: TcxGridViewInfo;
begin
  Result := FControl.ViewInfo;
end;

function TcxCustomGridHandler.UseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

{ TcxGridDesignController }

constructor TcxGridDesignController.Create(AControl: TcxCustomGrid);
begin
  inherited Create;
  FControl := AControl;
end;

function TcxGridDesignController.GetControl: TcxControl;
begin
  Result := FControl;
end;

{ TcxGridDragOpenInfoTab }

constructor TcxGridDragOpenInfoTab.Create(ALevel: TcxGridLevel);
begin
  inherited Create;
  Level := ALevel;
end;

function TcxGridDragOpenInfoTab.Equals(AInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  Result := inherited Equals(AInfo) and
    (Level = TcxGridDragOpenInfoTab(AInfo).Level);
end;

procedure TcxGridDragOpenInfoTab.Run;
begin
  Level.Active := True;
end;

{ TcxGridController }

destructor TcxGridController.Destroy;
begin
  FreeAndNil(FDesignController);
  inherited;
end;

function TcxGridController.GetDesignController: TcxGridDesignController;
begin
  if (FDesignController = nil) and Control.IsDesigning then
    FDesignController := GetDesignControllerClass.Create(Control);
  Result := FDesignController;
end;

procedure TcxGridController.DragOpenTimerHandler(Sender: TObject);
begin
  FDragOpenTimer.Enabled := False;
  try
    FDragOpenInfo.Run;
  finally
    StopDragOpen;
  end;
end;

function TcxGridController.GetDesignControllerClass: TcxGridDesignControllerClass;
begin
  Result := TcxGridDesignController;
end;

procedure TcxGridController.DoCancelMode;
begin
  if ActiveController <> nil then
    ActiveController.DoCancelMode;
end;

procedure TcxGridController.FocusChanged;
begin
  if ActiveController <> nil then
    ActiveController.DoControlFocusChanged;
end;

function TcxGridController.GetCursor(X, Y: Integer): TCursor;
begin
  Result := crDefault;
end;

procedure TcxGridController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest.ViewInfo <> nil then
    AHitTest.ViewInfo.MouseDown(AHitTest, Button, Shift);
end;

procedure TcxGridController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest.ViewInfo <> nil then
    AHitTest.ViewInfo.MouseMove(AHitTest, Shift);
end;

procedure TcxGridController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest.ViewInfo <> nil then
    AHitTest.ViewInfo.MouseUp(AHitTest, Button, Shift);
end;

procedure TcxGridController.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

  procedure ProcessOpening;
  var
    AHitTest: TcxCustomGridHitTest;
    ADragOpenInfo: TcxCustomGridDragOpenInfo;
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    if (State <> dsDragLeave) and IsDragOpenHitTest(AHitTest, ADragOpenInfo) then
      StartDragOpen(ADragOpenInfo)
    else
      StopDragOpen;
  end;

begin
  if Control.DragOpening then
    ProcessOpening;
end;

procedure TcxGridController.EndDrag(Target: TObject; X, Y: Integer);
begin
  StopDragOpen;
end;

procedure TcxGridController.StartDrag(var DragObject: TDragObject);
begin
end;

function TcxGridController.GetDragOpenInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridDragOpenInfo;
begin
  if AHitTest.HitTestCode = htTab then
    with TcxGridDetailsSiteTabHitTest(AHitTest) do
      Result := TcxGridDragOpenInfoTab.Create(Level)
  else
    Result := nil;
end;

function TcxGridController.IsDragOpenHitTest(AHitTest: TcxCustomGridHitTest;
  out ADragOpenInfo: TcxCustomGridDragOpenInfo): Boolean;
begin
  ADragOpenInfo := GetDragOpenInfo(AHitTest);
  Result := ADragOpenInfo <> nil;
end;

procedure TcxGridController.StartDragOpen(ADragOpenInfo: TcxCustomGridDragOpenInfo);
begin
  if (FDragOpenInfo <> nil) and FDragOpenInfo.Equals(ADragOpenInfo) then
  begin
    ADragOpenInfo.Free;
    Exit;
  end;
  FDragOpenInfo.Free;
  FDragOpenInfo := ADragOpenInfo;
  if FDragOpenTimer = nil then
  begin
    FDragOpenTimer := TcxTimer.Create(nil);
    with FDragOpenTimer do
    begin
      Interval := Control.DragOpeningWaitTime;
      OnTimer := DragOpenTimerHandler;
    end;
  end
  else
    with FDragOpenTimer do
    begin
      Enabled := False;
      Enabled := True;
    end;
end;

procedure TcxGridController.StopDragOpen;
begin
  FreeAndNil(FDragOpenTimer);
  FreeAndNil(FDragOpenInfo);
end;

{ TcxGridPainter }

function TcxGridPainter.GetCanvas: TcxCanvas;
begin
  Result := Control.LockedStatePaintHelper.GetActiveCanvas;
end;

procedure TcxGridPainter.DrawDetailsSite;
begin
  with TcxGridTopDetailsSiteViewInfo(ViewInfo.DetailsSiteViewInfo) do
    if Visible then Paint(Self.Canvas);
end;

{procedure TcxGridPainter.DrawEmptyArea;
begin
  Canvas.Brush.Color := ViewInfo.EmptyAreaColor;
  Canvas.FillRect(ViewInfo.ClientBounds);
end;}

procedure TcxGridPainter.Invalidate(AInvalidateDetails: Boolean);
var
  I: Integer;
  AControl: TControl;
begin
  Control.Invalidate;
  if AInvalidateDetails then
    for I := 0 to Control.ControlCount - 1 do
    begin
      AControl := Control.Controls[I];
      if AControl is TcxGridSite then
        AControl.Invalidate;
    end;
end;

procedure TcxGridPainter.Invalidate(const R: TRect);
begin
  Control.InvalidateRect(R, False);
end;

procedure TcxGridPainter.Paint;
begin
  DrawDetailsSite;
  //DrawEmptyArea;
end;

{ TcxGridTopDetailsSiteViewInfo }

function TcxGridTopDetailsSiteViewInfo.GetControl: TcxCustomGrid;
begin
  Result := TcxCustomGrid(Level.Control);
end;

function TcxGridTopDetailsSiteViewInfo.CalculateHeight: Integer;
begin
  Result := MaxHeight;
end;

function TcxGridTopDetailsSiteViewInfo.CalculateWidth: Integer;
begin
  Result := MaxWidth;
end;

function TcxGridTopDetailsSiteViewInfo.GetActiveGridView: TcxCustomGridView;
begin
  Result := Control.ActiveView;
end;

function TcxGridTopDetailsSiteViewInfo.GetActiveLevel: TcxGridLevel;
begin
  Result := Control.ActiveLevel;
end;

function TcxGridTopDetailsSiteViewInfo.GetCanvas: TcxCanvas;
begin
  Result := Control.Painter.Canvas;
end;

function TcxGridTopDetailsSiteViewInfo.GetContainer: TcxControl;
begin
  Result := Control;
end;

function TcxGridTopDetailsSiteViewInfo.GetDesignController: TcxCustomGridDesignController;
begin
  Result := Control.Controller.DesignController;
end;

function TcxGridTopDetailsSiteViewInfo.GetMasterRecord: TObject;
begin
  Result := nil;
end;

function TcxGridTopDetailsSiteViewInfo.GetMaxHeight: Integer;
begin
  with Control.ViewInfo.ClientBounds do
    Result := Bottom - Top;
end;

function TcxGridTopDetailsSiteViewInfo.GetMaxWidth: Integer;
begin
  with Control.ViewInfo.ClientBounds do
    Result := Right - Left;
end;

procedure TcxGridTopDetailsSiteViewInfo.InitTabHitTest(AHitTest: TcxGridDetailsSiteTabHitTest);
begin
  AHitTest.Owner := Control;
end;

procedure TcxGridTopDetailsSiteViewInfo.ChangeActiveTab(ALevel: TcxGridLevel;
  AFocusView: Boolean = False);
begin
  Control.ActiveLevel := ALevel;
end;

function TcxGridTopDetailsSiteViewInfo.DetailHasData(ALevel: TcxGridLevel): Boolean;
begin
  Result := (ALevel.GridView <> nil) and not ALevel.GridView.ViewData.IsEmpty;
end;

function TcxGridTopDetailsSiteViewInfo.SupportsTabAccelerators: Boolean;
begin
  Result := True;
end;

procedure TcxGridTopDetailsSiteViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  if not Control.IsDestroying then inherited;
end;

{ TcxGridViewInfo }

constructor TcxGridViewInfo.Create(AControl: TcxCustomGrid);
begin
  inherited;
  CreateViewInfos;
end;

destructor TcxGridViewInfo.Destroy;
begin
  DestroyViewInfos;
  FDetailsSiteViewInfoCachedInfo.Free;
  inherited;
end;

function TcxGridViewInfo.GetBounds: TRect;
begin
  Result := Control.Bounds;
end;

function TcxGridViewInfo.GetClientBounds: TRect;
begin
  Result := Control.ClientBounds;
end;

function TcxGridViewInfo.GetEmptyAreaColor: TColor;
begin
  Result := Control.Color;
end;

procedure TcxGridViewInfo.CreateViewInfos;
begin
  FDetailsSiteViewInfo :=
    TcxGridTopDetailsSiteViewInfoClass(GetDetailsSiteViewInfoClass).Create(Control.Levels);
  if FDetailsSiteViewInfoCachedInfo <> nil then
    FDetailsSiteViewInfo.SetCachedInfo(FDetailsSiteViewInfoCachedInfo);
end;

procedure TcxGridViewInfo.DestroyViewInfos;
begin
  FDetailsSiteViewInfo.GetCachedInfo(FDetailsSiteViewInfoCachedInfo);
  FDetailsSiteViewInfo.Free;
end;

function TcxGridViewInfo.GetDetailsSiteViewInfoClass: TcxGridTopDetailsSiteViewInfoClass;
begin
  Result := TcxGridTopDetailsSiteViewInfo;
end;

procedure TcxGridViewInfo.RecreateViewInfos;
begin
  DestroyViewInfos;
  CreateViewInfos;
end;

procedure TcxGridViewInfo.Calculate;
begin
  RecreateViewInfos;
  with ClientBounds, TcxGridTopDetailsSiteViewInfo(FDetailsSiteViewInfo) do
    if Visible then Calculate(Left, Top);
end;

procedure TcxGridViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  TcxGridTopDetailsSiteViewInfo(FDetailsSiteViewInfo).RightToLeftConversion(ABounds);
end;

function TcxGridViewInfo.GetHitTest(X, Y: Integer): TcxCustomGridHitTest;
begin
  Result := TcxGridTopDetailsSiteViewInfo(FDetailsSiteViewInfo).GetHitTest(Point(X, Y));
  if Result = nil then
    Result := TcxGridNoneHitTest.Instance(Point(X, Y));
end;

procedure TcxGridViewInfo.MainCalculate;
var
  APrevIsCalculating: Boolean;
begin
  APrevIsCalculating := IsCalculating;
  FIsCalculating := True;
  try
    Calculate;
  finally
    FIsCalculating := APrevIsCalculating;
  end;
  if UseRightToLeftAlignment then
    DoRightToLeftConversion(Bounds);
end;

{ TcxGridLevelTabs }

constructor TcxGridLevelTabs.Create(AOwner: TcxCustomGrid);
begin
  inherited Create;
  FOwner := AOwner;
  FCaptionAlignment := cxGridLevelTabsDefaultCaptionAlignment;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
  FImageBorder := cxGridLevelTabsDefaultImageBorder;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChanged;
  FSlants := TcxTabSlants.Create(Self);
  FSlants.OnChange := SlantsChanged;
  FStyle := cxPCDefaultStyle;
end;

destructor TcxGridLevelTabs.Destroy;
begin
  FSlants.Free;
  FImagesChangeLink.Free;
  FFreeNotificator.Free;
  inherited Destroy;
end;

procedure TcxGridLevelTabs.SetCaptionAlignment(Value: TAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
    Changed;
  end;
end;

procedure TcxGridLevelTabs.SetImageBorder(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FImageBorder <> Value then
  begin
    FImageBorder := Value;
    Changed;
  end;
end;

procedure TcxGridLevelTabs.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImagesChangeLink, FFreeNotificator);
end;

procedure TcxGridLevelTabs.SetSlants(Value: TcxTabSlants);
begin
  FSlants.Assign(Value);
end;

procedure TcxGridLevelTabs.SetStyle(Value: TcxPCStyleID);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TcxGridLevelTabs.FreeNotification(Sender: TComponent);
begin
  if Sender = Images then Images := nil;
end;

procedure TcxGridLevelTabs.ImagesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxGridLevelTabs.SlantsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxGridLevelTabs.Changed;
begin
  FOwner.SizeChanged;
end;

procedure TcxGridLevelTabs.ChangeScale(M, D: Integer);
begin
  if ImageBorder > 0 then
    ImageBorder := Max(MulDiv(ImageBorder, M, D), 1);
end;

function TcxGridLevelTabs.GetImages: TCustomImageList;
begin
  Result := FImages;
  if Result = nil then
    Result := Owner.Images;
end;

procedure TcxGridLevelTabs.Assign(Source: TPersistent);
begin
  if Source is TcxGridLevelTabs then
    with TcxGridLevelTabs(Source) do
    begin
      Self.CaptionAlignment := CaptionAlignment;
      Self.ImageBorder := ImageBorder;
      Self.Images := Images;
      Self.Slants := Slants;
      Self.Style := Style;
    end
  else
    inherited;
end;

{ TcxGridRootLevelOptions }

constructor TcxGridRootLevelOptions.Create(ALevel: TcxGridLevel);
begin
  inherited;
  DetailFrameWidth := cxGridRootLevelDefaultDetailFrameWidth;
end;

{ TcxGridRootLevel }

function TcxGridRootLevel.GetOptionsClass: TcxGridLevelOptionsClass;
begin
  Result := TcxGridRootLevelOptions;
end;

{ TcxGridLockedStateImageOptions }

constructor TcxGridLockedStateImageOptions.Create(AGrid: TcxCustomGrid);
begin
  inherited Create(AGrid);
  FGrid := AGrid;
  Text := cxGetResourceString(@scxGridLockedStateImageText);
end;

function TcxGridLockedStateImageOptions.GetFont: TFont;
begin
  Result := Grid.Font;
end;

function TcxGridLockedStateImageOptions.IsTextStored: Boolean;
begin
  Result := Text <> cxGetResourceString(@scxGridLockedStateImageText);
end;

{ TcxGridLockedStatePaintHelper }

procedure TcxGridLockedStatePaintHelper.AfterDestroyingImage;
begin
  if Grid.ActiveView <> nil then
    TcxCustomGridViewAccess.AfterDestroyingLockedStateImage(Grid.ActiveView);
end;

procedure TcxGridLockedStatePaintHelper.BeforeCreatingImage;
begin
  if Grid.ActiveView <> nil then
    TcxCustomGridViewAccess.BeforeCreatingLockedStateImage(Grid.ActiveView);
end;

function TcxGridLockedStatePaintHelper.CanCreateLockedImage: Boolean;
begin
  Result := inherited CanCreateLockedImage and
    not Grid.IsUpdating and not Grid.UpdateLocked;
end;

function TcxGridLockedStatePaintHelper.DoPrepareImage: Boolean;
begin
  Result := Grid.DoPrepareLockedStateImage;
end;

function TcxGridLockedStatePaintHelper.GetControl: TcxControl;
begin
  Result := Grid;
end;

function TcxGridLockedStatePaintHelper.GetGrid: TcxCustomGrid;
begin
  Result := TcxCustomGrid(Owner);
end;

function TcxGridLockedStatePaintHelper.GetOptions: TcxLockedStateImageOptions;
begin
  Result := Grid.LockedStateImageOptions;
end;

{ TcxCustomGrid }

constructor TcxCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := cxcbsDefault;
  ControlStyle := ControlStyle + [csDisplayDragImage];
  FDragOpening := True;
  FDragOpeningWaitTime := cxGridDefaultDragOpeningWaitTime;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FLevelTabs := GetLevelTabsClass.Create(Self);
  FNotifications := TList.Create;
  FViews := TList.Create;
  FChanges := TList.Create;
  FTabStop := True;
  Levels := GetLevelsClass.Create(Self);
  FLockedStateImageOptions := GetLockedStateImageOptionsClass.Create(Self);
  FLockedStatePaintHelper := GetLockedStatePaintHelperClass.Create(Self);
  CreateHandlers;
  SetBounds(Left, Top, 250, 200);
end;

destructor TcxCustomGrid.Destroy;

  procedure HideRootViews;
  var
    I: Integer;
  begin
    for I := 0 to Levels.VisibleCount - 1 do
      with Levels.VisibleItems[I] do
        if GridView <> nil then GridView.ViewInfo.DoVisibilityChanged(False);
  end;

begin
  HideRootViews;
  DestroyHandlers;
  Levels := nil;
  DestroyViews;
  FreeAndNil(FViews);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FNotifications);
  FreeAndNil(FLevelTabs);
  DestroyChanges(FChanges);
  FreeAndNil(FChanges);
  FreeAndNil(FLockedStatePaintHelper);
  FreeAndNil(FLockedStateImageOptions);
  inherited Destroy;
end;

function TcxCustomGrid.GetActiveView: TcxCustomGridView;
begin
  if FActiveLevel = nil then
    Result := nil
  else
    Result := FActiveLevel.GridView;
end;

function TcxCustomGrid.GetFocusedViewNavigator: IcxNavigator;
begin
  if (FocusedView = nil) or not Supports(FocusedView, IcxNavigator) then
    Result := nil
  else
    Result := FocusedView as IcxNavigator;
end;

function TcxCustomGrid.GetView(Index: Integer): TcxCustomGridView;
begin
  Result := TcxCustomGridView(FViews[Index]);
end;

function TcxCustomGrid.GetViewCount: Integer;
begin
  Result := FViews.Count;
end;

function TcxCustomGrid.GetRootLevelOptions: TcxGridLevelOptions;
begin
  Result := FLevels.Options;
end;

function TcxCustomGrid.GetRootLevelStyles: TcxGridLevelStyles;
begin
  Result := FLevels.Styles;
end;

function TcxCustomGrid.GetStructureNavigator: TcxCustomGridStructureNavigator;
begin
  CreateStructureNavigator;
  Result := FStructureNavigator;
end;

function TcxCustomGrid.GetUpdateLocked: Boolean;
begin
  Result := (FUpdateLockCount <> 0);
end;

procedure TcxCustomGrid.ImageListChange(Sender: TObject);
begin
  LayoutChanged;
end;

procedure TcxCustomGrid.SetActiveLevel(Value: TcxGridLevel);
begin
  if FActiveLevel <> Value then
  begin
    if Value = nil then
      Value := FLevels.GetAvailableItem
    else
      if not Value.Visible then
        Exit;
    TcxGridLevelAccess.Deactivate(FActiveLevel);
    FActiveLevel := Value;
    SizeChanged;
    if not IsLoading and (FActiveLevel <> nil) then
      DoActiveTabChanged(FActiveLevel);
    if FActiveLevel <> nil then
      FocusedView := FActiveLevel.GridView
    else
      FocusedView := nil;
  end;
end;

procedure TcxCustomGrid.SetDragOpeningWaitTime(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FDragOpeningWaitTime := Value;
end;

procedure TcxCustomGrid.SetFocusedView(Value: TcxCustomGridView);

  function CheckMasterView(AView: TcxCustomGridView): TcxCustomGridView;
  begin
    Result := AView;
    repeat
      Result := Result.MasterGridView;
    until (Result = nil) or not Result.IsDestroying;
  end;

  function GetAvailableView: TcxCustomGridView;
  var
    I: Integer;
  begin
    Result := CheckMasterView(FFocusedView);
    if Result = nil then
    begin
      for I := 0 to Levels.Count - 1 do
      begin
        Result := Levels[I].GridView;
        if (Result <> nil) and not Result.IsDestroying then Exit;
      end;
      Result := nil;
    end;
  end;

var
  APrevFocusedView: TcxCustomGridView;
  APrevFocused: Boolean;
begin
  if (Value <> nil) and not TcxCustomGridViewAccess.CanFocus(Value) then Exit;
  if FFocusedView <> Value then
  begin
    TcxCustomGridViewAccess.Deactivate(FFocusedView);

    if not IsDestroying and not IsDesigning and
      ((Value = nil) and (ActiveLevel <> nil) and (ActiveLevel.GridView <> nil) or
       (Value <> nil) and Value.IsDestroying) then
      Value := GetAvailableView;

    APrevFocusedView := FFocusedView;
    FFocusedView := Value;
    if APrevFocusedView <> nil then
    begin
      APrevFocused := IsFocused;
      try
        TcxCustomGridViewAccess.FocusChanged(APrevFocusedView, False);
      except
        FocusedView := APrevFocusedView;
        raise;
      end;
      if APrevFocused and not IsFocused then
        SetFocus;
    end;
    if FFocusedView <> nil then
    begin
      TcxGridLevel(FFocusedView.Level).Active := True;
      TcxCustomGridViewAccess.FocusChanged(FFocusedView, True);
    end
    else
      if IsFocused and CanFocusEx then
        SetFocus;
    FocusedViewChanged(APrevFocusedView, FFocusedView);
  end;
end;

procedure TcxCustomGrid.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TcxCustomGrid.SetLevels(Value: TcxGridLevel);
begin
  FLevels.Free;
  FLevels := Value;
  if FLevels <> nil then
    FLevels.Control := Self;
end;

procedure TcxCustomGrid.SetLevelTabs(Value: TcxGridLevelTabs);
begin
  FLevelTabs.Assign(Value);
end;

procedure TcxCustomGrid.SetLockedStateImageOptions(
  Value: TcxGridLockedStateImageOptions);
begin
  FLockedStateImageOptions.Assign(Value);
end;

procedure TcxCustomGrid.SetRootLevelOptions(Value: TcxGridLevelOptions);
begin
end;

procedure TcxCustomGrid.SetRootLevelStyles(Value: TcxGridLevelStyles);
begin
end;

procedure TcxCustomGrid.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    if FocusedView <> nil then
      FocusedView.TabStop := FTabStop;
  end;
end;

procedure TcxCustomGrid.AddView(AView: TcxCustomGridView);
begin
  FViews.Add(AView);
  AView.DataController.SetMasterMode(nil, True);
end;

procedure TcxCustomGrid.RemoveView(AView: TcxCustomGridView);
begin
  FViews.Remove(AView);
  DestroyViewChanges(AView);
  //ViewChanged(AView, vsRemoved);
end;

procedure TcxCustomGrid.DestroyViews;
var
  I: Integer;
begin
  for I := ViewCount - 1 downto 0 do
    Views[I].Free;
end;

procedure TcxCustomGrid.DestroyChanges(AChanges: TList);
var
  I: Integer;
begin
  for I := 0 to AChanges.Count - 1 do
    TObject(AChanges[I]).Free;
  AChanges.Clear;
end;

procedure TcxCustomGrid.DestroyViewChanges(AView: TcxCustomGridView);
var
  I: Integer;
begin
  for I := FChanges.Count - 1 downto 0 do
    if (TcxCustomGridChange(FChanges[I]) is TcxCustomGridViewChange) and
      (TcxCustomGridViewChange(FChanges[I]).GridView = AView) then
    begin
      TObject(FChanges[I]).Free;
      FChanges.Delete(I);
    end;
end;

procedure TcxCustomGrid.CreateStructureNavigator;
begin
  if (FStructureNavigator = nil) and
    IsDesigning and (cxGridStructureNavigatorClass <> nil) and
    (FController <> nil) and not FCreatingStructureNavigator then
  begin
    FCreatingStructureNavigator := True;
    try
      FStructureNavigator := cxGridStructureNavigatorClass.Create(Self);
    finally
      FCreatingStructureNavigator := False;
    end;
  end;
end;

procedure TcxCustomGrid.CMBiDiModeChanged(var Message: TMessage);
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

{procedure TcxCustomGrid.CMDeferUpdates(var Message: TMessage);
begin
  DoProcessChangesStack;
end;}

procedure TcxCustomGrid.CMDialogChar(var Message: TCMDialogChar);
begin
  if TcxGridTopDetailsSiteViewInfo(ViewInfo.DetailsSiteViewInfo).ProcessDialogChar(Message.CharCode) then
    Message.Result := 1
  else
    inherited;
end;

function TcxCustomGrid.NavigatorIsActive: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.IsActive;
end;

function TcxCustomGrid.NavigatorIsBof: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.IsBof;
end;

function TcxCustomGrid.NavigatorIsEof: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.IsEof;
end;

function TcxCustomGrid.NavigatorCanAppend: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.CanAppend;
end;

function TcxCustomGrid.NavigatorCanEdit: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.CanEdit;
end;

function TcxCustomGrid.NavigatorCanDelete: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.CanDelete;
end;

function TcxCustomGrid.NavigatorCanInsert: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.CanInsert;
end;

function TcxCustomGrid.NavigatorIsEditing: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.IsEditing;
end;

procedure TcxCustomGrid.NavigatorClearBookmark;
begin
  if FocusedViewNavigator <> nil then
    FocusedViewNavigator.ClearBookmark;
end;

function TcxCustomGrid.NavigatorIsBookmarkAvailable: Boolean;
begin
  if FocusedViewNavigator = nil then
    Result := False
  else
    Result := FocusedViewNavigator.IsBookmarkAvailable;
end;

procedure TcxCustomGrid.NavigatorDoAction(AButtonIndex: Integer);
begin
  if FocusedViewNavigator <> nil then
    FocusedViewNavigator.DoAction(AButtonIndex);
end;

function TcxCustomGrid.NavigatorGetNotifier: TcxNavigatorControlNotifier;
begin
  Result := FNavigatorNotifier;
end;

function TcxCustomGrid.NavigatorIsActionSupported(AButtonIndex: Integer): Boolean;
begin
  Result := True;
end;

function TcxCustomGrid.NavigatorGetRecordCount: Integer;
var
  AIntf: IcxNavigatorRecordPosition;
begin
  Result := 0;
  if (FocusedViewNavigator <> nil) and Supports(FocusedView, IcxNavigatorRecordPosition, AIntf) then
    Result := AIntf.GetRecordCount;
end;

function TcxCustomGrid.NavigatorGetRecordIndex: Integer;
var
  AIntf: IcxNavigatorRecordPosition;
begin
  Result := 0;
  if (FocusedViewNavigator <> nil) and Supports(FocusedView, IcxNavigatorRecordPosition, AIntf) then
    Result := AIntf.GetRecordIndex;
end;

function TcxCustomGrid.GetLockedStateImage: TcxBitmap32;
begin
  Result := LockedStatePaintHelper.GetImage;
end;

function TcxCustomGrid.GetLockedStateTopmostControl: TcxControl;
begin
  Result := Self;
end;

procedure TcxCustomGrid.UpdateLockedStateFont(AFont: TFont);
begin
  if Assigned(LockedStateImageOptions) then
    LockedStateImageOptions.UpdateFont(AFont);
end;

function TcxCustomGrid.GetGestureClient(const APoint: TPoint): IdxGestureClient;
var
  AControl: TControl;
begin
  AControl := FindVCLWindow(cxClientToScreen(Handle, APoint));
  if (AControl <> nil) and (AControl is TcxGridSite) then
    Result := AControl as IdxGestureClient
  else
    Result := inherited GetGestureClient(APoint);
end;

function TcxCustomGrid.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := inherited IsGestureTarget(AWnd) or IsChild(Handle, AWnd);
end;

procedure TcxCustomGrid.GetAdornerTargetElements(AList: TStrings);
var
  I: Integer;
  ALevel: TcxGridLevel;
begin
  inherited GetAdornerTargetElements(AList);
  for I := 0 to Levels.Count - 1 do
  begin
    ALevel := Levels[I];
    AList.AddObject(ALevel.Name, ALevel);
  end;
end;

procedure TcxCustomGrid.AddChildComponent(AComponent: TcxControlChildComponent);
begin
  inherited;
  if AComponent is TcxCustomGridView then
    AddView(TcxCustomGridView(AComponent));
end;

procedure TcxCustomGrid.RemoveChildComponent(AComponent: TcxControlChildComponent);
begin
  inherited;
  if AComponent is TcxCustomGridView then
    RemoveView(TcxCustomGridView(AComponent));
end;

procedure TcxCustomGrid.BoundsChanged;
begin
  if (ViewInfo = nil) or not ViewInfo.IsCalculating then
    SizeChanged;
  inherited;
  StructureNavigatorChanged;
end;

procedure TcxCustomGrid.ChangeScaleEx(M, D: Integer; IsDpiChange: Boolean);
begin
  BeginUpdate;
  try
    inherited;
    LockedStateImageOptions.ChangeScale(M, D);
    LevelTabs.ChangeScale(M, D);
    Levels.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGrid.DoCancelMode;
begin
  inherited;
  FController.DoCancelMode;
end;

procedure TcxCustomGrid.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if not Handled then
    Handled := SendNotifications(gnkContextMenu, TObject((MousePos.X = -1) and (MousePos.Y = -1)));
end;

procedure TcxCustomGrid.DoPaint;
begin
  inherited;
  FPainter.Paint;
end;

procedure TcxCustomGrid.FocusChanged;
begin
  inherited;
  if not IsDestroying and Focused and (FocusedView <> nil) then
    TcxCustomGridViewAccess.FocusChanged(FocusedView, True);
end;

procedure TcxCustomGrid.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ViewCount - 1 do
    if Views[I].Owner = Root then
      Proc(Views[I]);
  for I := 0 to FLevels.Count - 1 do
    if FLevels[I].Owner = Root then
      Proc(FLevels[I]);
end;

function TcxCustomGrid.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := FController.GetCursor(X, Y);
  if Result = crDefault then
    Result := inherited GetCurrentCursor(X, Y);
end;

function TcxCustomGrid.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift);
  if not Result and (ssLeft in Shift) then
  begin
    AHitTest := ViewInfo.GetHitTest(X, Y);
    Result := AHitTest.HitTestCode = htTab;
  end;
end;

function TcxCustomGrid.GetIsFocused: Boolean;
var
  AForm: TCustomForm;
begin
  Result := FIsPopupControl{8} or inherited GetIsFocused;
  if not Result then
  begin
    AForm := GetParentForm(Self);
    Result := (AForm <> nil) and (AForm.ActiveControl <> Self) and
      ContainsControl(AForm.ActiveControl) and AForm.ActiveControl.Focused and
      (not (AForm.ActiveControl is TcxControl) or TcxControl(AForm.ActiveControl).IsFocused);
  end;
end;

procedure TcxCustomGrid.Loaded;
begin
  inherited;
  BeginUpdate;
  try
    LayoutChanged;
  finally
    EndUpdate;
  end;
  StructureNavigatorChanged;
end;

function TcxCustomGrid.MayFocus: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not UpdateLocked then
    FController.MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not UpdateLocked then
    FController.MouseMove(Shift, X, Y);
end;

procedure TcxCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not UpdateLocked then
    FController.MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomGrid.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
var
  I: Integer;
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  for I := 0 to ViewCount - 1 do
    TcxCustomGridViewAccess.LookAndFeelChanged(Views[I]);
  SizeChanged;
end;

procedure TcxCustomGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TcxCustomGrid.ReadState(Reader: TReader);
begin
  if StructureNavigator <> nil then StructureNavigator.BeforeGridLoading;
  inherited;
end;

procedure TcxCustomGrid.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited;
  if Child is TcxGridLevel then
    TcxGridLevel(Child).Index := Order - ViewCount;
end;

procedure TcxCustomGrid.SetName(const NewName: TComponentName);
var
  AOldName: TComponentName;
  ALevels: TList;

  procedure RetrieveLevels;

    procedure AddLevel(ALevel: TcxGridLevel);
    var
      I: Integer;
    begin
      if ALevel.Name <> '' then ALevels.Add(ALevel);
      for I := 0 to ALevel.Count - 1 do
        AddLevel(ALevel[I]);
    end;

  begin
    AddLevel(Levels);
  end;

begin
  AOldName := Name;
  inherited;
  if Name <> AOldName then
  begin
    RenameComponents(Self, Owner, Name, AOldName, ViewCount, @cxGrid.GetView);
    ALevels := TList.Create;
    try
      RetrieveLevels;
      FTag := ALevels;
      RenameComponents(Self, Owner, Name, AOldName, ALevels.Count, @cxGrid.GetLevel);
    finally
      ALevels.Free;
    end;
    StructureNavigatorChanged;
  end;
end;

procedure TcxCustomGrid.WndProc(var Message: TMessage);
begin
  {if (Message.Msg = WM_MOUSEACTIVATE) and IsDesigning then
    Exit;}
  inherited WndProc(Message);
end;

procedure TcxCustomGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  FController.EndDrag(Target, X, Y);
  inherited;
end;

procedure TcxCustomGrid.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
  FController.StartDrag(DragObject);
end;

procedure TcxCustomGrid.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;
  FController.DragOver(Source, X, Y, State, Accept);
end;

procedure TcxCustomGrid.CreateHandlers;
begin
  FController := GetControllerClass.Create(Self);
  FPainter := GetPainterClass.Create(Self);
  FViewInfo := GetViewInfoClass.Create(Self);
  FNavigatorNotifier := TcxNavigatorControlNotifier.Create;
  CreateStructureNavigator;
end;

procedure TcxCustomGrid.DestroyHandlers;
begin
  FreeAndNil(FStructureNavigator);
  FreeAndNil(FNavigatorNotifier);
  FreeAndNil(FViewInfo);
  FreeAndNil(FPainter);
  FreeAndNil(FController);
end;

procedure TcxCustomGrid.BeginLockedStatePaint(AMode: TcxGridShowLockedStateImageMode);
begin
  LockedStatePaintHelper.BeginLockedPaint(AMode);
end;

procedure TcxCustomGrid.EndLockedStatePaint;
begin
  LockedStatePaintHelper.EndLockedPaint;
end;

function TcxCustomGrid.DoPrepareLockedStateImage: Boolean;
begin
  Result := False;
  if Assigned(OnPrepareLockedStateImage) then
    OnPrepareLockedStateImage(Self, LockedStatePaintHelper.Bitmap, Result);
end;

function TcxCustomGrid.GetLockedStatePaintHelperClass: TcxGridLockedStatePaintHelperClass;
begin
  Result := TcxGridLockedStatePaintHelper;
end;

function TcxCustomGrid.GetLockedStateImageOptionsClass: TcxGridLockedStateImageOptionsClass;
begin
  Result := TcxGridLockedStateImageOptions;
end;

procedure TcxCustomGrid.DoActiveTabChanged(ALevel: TcxGridLevel);
begin
  if StructureNavigator <> nil then StructureNavigator.BringToFront;
  if Assigned(FOnActiveTabChanged) then FOnActiveTabChanged(Self, ALevel);
end;

procedure TcxCustomGrid.DoActiveTabChangedEx(ALevel: TcxGridLevel; ARecordIndex: Integer);
var
  ADone: Boolean;
begin
  ADone := False;
  if Assigned(FOnActiveTabChangedEx) then
    FOnActiveTabChangedEx(Self, ALevel, ARecordIndex, ADone);
  if not ADone then
    DoActiveTabChanged(ALevel);
end;

procedure TcxCustomGrid.DoChange(AGridChange: TcxCustomGridChange);
begin
  try
    if {IsLoading or - because of details} IsDestroying then Exit;
    with AGridChange do
    begin
      Control := Self;
      Execute;
    end;
  finally
    AGridChange.Free;
  end;
end;

procedure TcxCustomGrid.DoLayoutChanged(AGridView: TcxCustomGridView);
begin
  if Assigned(FOnLayoutChanged) then FOnLayoutChanged(Self, AGridView);
end;

procedure TcxCustomGrid.DoUpdate(AChanges: TList);
var
  AFinalChanges: TList;

  procedure BuildFinalChangesList;
  var
    I, J: Integer;
    AChange: TcxCustomGridChange;

    function FinalChangesContainClass(AChange: TcxCustomGridChange): Boolean;
    var
      I: Integer;
    begin
      for I := 0 to AFinalChanges.Count - 1 do
      begin
        Result := TcxCustomGridChange(AFinalChanges[I]).IsEqual(AChange);
        if Result then Exit;
      end;
      Result := False;
    end;

  begin
    for I := 0 to AChanges.Count - 1 do
    begin
      AChange := TcxCustomGridChange(AChanges[I]);
      if not AChange.IsCumulative or not FinalChangesContainClass(AChange) then
        AFinalChanges.Add(AChange)
      else
        AChange.Free;
    end;
    AChanges.Clear;

    I := 0;
    while I < AFinalChanges.Count do
    begin
      AChange := TcxCustomGridChange(AFinalChanges[I]);
      for J := I + 1 to AFinalChanges.Count - 1 do
        if AChange.IsCompatibleWith(TcxCustomGridChange(AFinalChanges[J])) then
        begin
          AChange.Free;
          AFinalChanges.Delete(I);
          Dec(I);
          Break;
        end;
      Inc(I);
    end;

    //!!!
    for I := AFinalChanges.Count - 1 downto 0 do
    begin
      AChange := TcxCustomGridChange(AFinalChanges[I]);
      for J := I - 1 downto 0 do
        if AChange.IsCompatibleWith(TcxCustomGridChange(AFinalChanges[J])) then
        begin
          AChange.Free;
          AFinalChanges.Delete(I);
          Break;
        end;
    end;
  end;

  procedure DoFinalChanges;
  var
    ALockChanges: Boolean;
    I: Integer;
    AChange: TcxCustomGridChange;
  begin

    if AFinalChanges.Count = 0 then Exit;
    ALockChanges := AFinalChanges.Count > 1;
    if ALockChanges then
      BeginUpdate;
    try
      for I := 0 to AFinalChanges.Count - 1 do
      begin
        AChange := TcxCustomGridChange(AFinalChanges[I]);
        if ALockChanges and not AChange.CanExecuteWhenLocked then
          EndUpdate;
        DoChange(AChange);
        if ALockChanges and not UpdateLocked then
          BeginUpdate;
      end;
    finally
      if ALockChanges then
        EndUpdate;
    end;

  end;

begin
  Inc(FUpdateCount);
  AFinalChanges := TList.Create;
  try
    BuildFinalChangesList;
    DoFinalChanges;
  finally
    Dec(FUpdateCount);
    AFinalChanges.Free;
  end;
end;

procedure TcxCustomGrid.FocusedViewChanged(APrevFocusedView, AFocusedView: TcxCustomGridView);
begin
  if IsDestroying then Exit;
  RefreshNavigators;
  SendNotifications(gnkFocusedViewChanged);
  if Assigned(FOnFocusedViewChanged) then
    FOnFocusedViewChanged(Self, APrevFocusedView, AFocusedView);
end;

function TcxCustomGrid.GetControllerClass: TcxGridControllerClass;
begin
  Result := TcxGridController;
end;

function TcxCustomGrid.GetDefaultViewClass: TcxCustomGridViewClass;
begin
  Result := nil;
end;

function TcxCustomGrid.GetLevelsClass: TcxGridLevelClass;
begin
  Result := TcxGridRootLevel;
end;

function TcxCustomGrid.GetLevelTabsClass: TcxGridLevelTabsClass;
begin
  Result := TcxGridLevelTabs;
end;

function TcxCustomGrid.GetPainterClass: TcxGridPainterClass;
begin
  Result := TcxGridPainter;
end;

function TcxCustomGrid.GetViewInfoClass: TcxGridViewInfoClass;
begin
  Result := TcxGridViewInfo;
end;

function TcxCustomGrid.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TcxCustomGrid.StructureNavigatorChanged;
begin
  if StructureNavigator <> nil then StructureNavigator.Changed;
end;

function TcxCustomGrid.UpdateOnRootViewDataChange: Boolean;
begin
  Result := (RootLevelOptions.DetailTabsPosition <> dtpNone) and
    not RootLevelOptions.TabsForEmptyDetails;
end;

procedure TcxCustomGrid.Invalidate(AHardUpdate: Boolean = False);

  procedure InvalidateView(AView: TcxCustomGridView);
  var
    I: Integer;
  begin
    if AView = nil then Exit;
    AView.Invalidate(AHardUpdate);
    for I := 0 to TcxGridLevel(AView.Level).VisibleCount - 1 do
      InvalidateView(TcxGridLevel(AView.Level).VisibleItems[I].GridView);
  end;

begin
  if IsDestroying then Exit;
  if AHardUpdate then
    LayoutChanged
  else
    inherited Invalidate;
  InvalidateView(ActiveView);
end;

procedure TcxCustomGrid.RemoveFocus(AGoForward: Boolean);
begin
  if IsFocused then
    PostMessage(GetParentForm(Self).Handle, WM_NEXTDLGCTL, WPARAM(not AGoForward), LPARAM(False));
end;

procedure TcxCustomGrid.BeginUpdate(
  AMode: TcxGridShowLockedStateImageMode = lsimNever);
begin
  if FUpdateLockCount = 0 then
    BeginLockedStatePaint(AMode);
  Inc(FUpdateLockCount);
end;

procedure TcxCustomGrid.CancelUpdate;
begin
  if FUpdateLockCount > 0 then
  begin
    Dec(FUpdateLockCount);
    if FUpdateLockCount = 0 then
    begin
      DestroyChanges(FChanges);
      EndLockedStatePaint;
    end;
  end;
end;

procedure TcxCustomGrid.BeginExport;
begin
  BeginLockedStatePaint(lsimImmediate);
  FIsExportMode := True;
  if ActiveView <> nil then
    ActiveView.Invalidate(True);
end;

procedure TcxCustomGrid.EndExport;
begin
  FIsExportMode := False;
  if ActiveView <> nil then
    ActiveView.Invalidate(True);
  EndLockedStatePaint;
end;

procedure TcxCustomGrid.Changed(AGridChange: TcxCustomGridChange);
begin
  if AGridChange.IsLockable then
    if UpdateLocked then
      FChanges.Add(AGridChange)
    else
(*      if HandleAllocated and not FProcessingChangesStack then
      begin
        if FChangesStack.Count = 0 then
          PostMessage(Handle, CM_DEFERUPDATES, 0, 0);
        FChangesStack.Add(AGridChange);
      end
      else  *)
        DoChange(AGridChange)
  else
    DoChange(AGridChange);
end;

procedure TcxCustomGrid.CheckFocusedView;
begin
  if (FFocusedView <> nil) and not TcxCustomGridViewAccess.CanFocus(FFocusedView) then
    if (ActiveView <> nil) and TcxCustomGridViewAccess.CanFocus(ActiveView) then
      ActiveView.Focused := True
    else
      FocusedView := nil;
end;

procedure TcxCustomGrid.EndUpdate;
begin
  if FUpdateLockCount > 0 then
  begin
    Dec(FUpdateLockCount);
    if FUpdateLockCount = 0 then
    try
      DoUpdate(FChanges);
    finally
      EndLockedStatePaint;
    end;
  end;
end;

procedure TcxCustomGrid.LayoutChanged;
begin
  if not IsLoading then
    Changed(TcxGridLayoutChange.Create(nil));
end;

procedure TcxCustomGrid.LevelChanged(ALevel: TcxGridLevel; AChangeKind: TcxGridLevelChangeKind);
begin
  StructureNavigatorChanged;
end;

procedure TcxCustomGrid.RefreshNavigators;
begin
  if FNavigatorNotifier <> nil then
    FNavigatorNotifier.RefreshNavigatorButtons;
end;

procedure TcxCustomGrid.SizeChanged;
begin
  if not IsLoading then
    Changed(TcxGridSizeChange.Create(nil));
end;

procedure TcxCustomGrid.UpdateFocusing(AChildFocused: Boolean);

  function NeedFocusingUpdate: Boolean;
  begin
    Result :=
      AChildFocused and not FLastFocused or
      not AChildFocused and not IsFocused;
  end;

begin
  if IsDestroying then Exit;
  if NeedFocusingUpdate then
  begin
    FLastFocused := AChildFocused;
    FController.FocusChanged;
    if AChildFocused then
      Perform(DXM_CONTAINERSETFOCUS, 0, 0);
  end;
end;

procedure TcxCustomGrid.ViewChanged(AView: TcxCustomGridView;
  AChangeKind: TcxGridViewChangeNotificationKind);
begin
  StructureNavigatorChanged;
end;

function TcxCustomGrid.CreateView(AViewClass: TcxCustomGridViewClass): TcxCustomGridView;
begin
  Result := AViewClass.Create(Owner);
  Result.Control := Self;
//  Result := AViewClass.CreateEx(Self);
  AddView(Result);
end;

function TcxCustomGrid.ViewExists(AView: TcxCustomGridView): Boolean;
var
  I: Integer;
begin
  for I := 0 to ViewCount - 1 do
  begin
    Result := (Views[I] = AView) or Views[I].HasAsClone(AView);
    if Result then Exit;
  end;
  Result := False;
end;

procedure TcxCustomGrid.RegisterNotification(ANotification: TcxCustomGridNotification);
begin
  if FNotifications.IndexOf(ANotification) = -1 then
    FNotifications.Add(ANotification);
end;

procedure TcxCustomGrid.UnregisterNotification(ANotification: TcxCustomGridNotification);
begin
  FNotifications.Remove(ANotification);
end;

function TcxCustomGrid.SendNotifications(AKind: TcxGridNotificationKind;
  AData: TObject = nil): Boolean;
var
  I: Integer;
  ANotification: TcxCustomGridNotification;
begin
  Result := False;
  if (AKind = gnkContextMenu) and IsDesigning then Exit;
  for I := 0 to FNotifications.Count - 1 do
  begin
    ANotification := TcxCustomGridNotification(FNotifications[I]);
    if AKind in ANotification.NotificationKinds then
    begin
      ANotification.Notify(AKind, AData, Result);
      if Result then Break;
    end;
  end;
end;

procedure TcxCustomGrid.RootViewDataChanged(AView: TcxCustomGridView);
begin
  if UpdateOnRootViewDataChange then
    LayoutChanged;
end;

procedure TcxCustomGrid.TranslationChanged;
begin
  SizeChanged;
end;

{ TcxGridViewRepository }

constructor TcxGridViewRepository.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList.Create;
end;

destructor TcxGridViewRepository.Destroy;
begin
  DestroyItems;
  FItems.Free;
  inherited;
end;

function TcxGridViewRepository.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridViewRepository.GetItem(Index: Integer): TcxCustomGridView;
begin
  Result := TcxCustomGridView(FItems[Index]);
end;

procedure TcxGridViewRepository.DestroyItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
end;

procedure TcxGridViewRepository.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    if Items[I].Owner = Root then
      Proc(Items[I]);
end;

procedure TcxGridViewRepository.AddItem(AItem: TcxCustomGridView);
begin
  FItems.Add(AItem);
  AItem.Repository := Self;
  AItem.DataController.SetMasterMode(nil, True);
end;

function TcxGridViewRepository.CreateItem(AItemClass: TcxCustomGridViewClass): TcxCustomGridView;
begin
  Result := AItemClass.Create(Owner);
  AddItem(Result);
end;

procedure TcxGridViewRepository.RemoveItem(AItem: TcxCustomGridView);
begin
  FItems.Remove(AItem);
  AItem.Repository := nil;
  if AItem.Control <> nil then
    TcxCustomGrid(AItem.Control).DestroyViewChanges(AItem);
end;

{ functions }

function GetParentGrid(AControl: TControl): TcxCustomGrid;
var
  AGridView: TcxCustomGridView;
begin
  AGridView := GetParentGridView(AControl);
  if AGridView = nil then
    Result := nil
  else
    Result := TcxCustomGrid(AGridView.Control);
end;

initialization
  RegisterClass(TcxGridLevel);
end.

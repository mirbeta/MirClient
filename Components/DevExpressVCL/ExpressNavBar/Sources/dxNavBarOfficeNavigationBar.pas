{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
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

unit dxNavBarOfficeNavigationBar;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Classes, Types, IniFiles, Registry, Windows, Messages, RTLConsts, SysUtils, Math, Menus,
  Graphics, Forms, Controls, ImgList, StdCtrls, ExtCtrls, Contnrs, Generics.Defaults, Generics.Collections,
  dxCore, cxScrollBar, cxClasses, cxGraphics, cxGeometry, cxLookAndFeelPainters,
  dxCoreClasses, dxAnimation, dxGdiPlusClasses, cxControls, dxThemeManager, cxLookAndFeels,
  cxAccessibility, cxLibraryConsts, dxCustomHint, dxCoreGraphics, cxContainer, dxGdiPlusApi,
  dxCalloutPopup, dxBuiltInPopupMenu;

const
  // hit test constants
  nbhtNone                = 0;
  nbhtBackground          = 1;
  nbhtItem                = 2;
  nbhtCustomizationButton = 3;

type
  TdxNavBarCustomOfficeNavigationBar = class;
  TdxNavBarOfficeNavigationBarController = class;
  TdxNavBarOfficeNavigationBarItem = class;
  TdxNavBarOfficeNavigationBarPeekForm = class;

  TdxNavBarOfficeNavigationBarSubclass = class
  private
    FNavigationBar: TdxNavBarCustomOfficeNavigationBar;

    function GetScaleFactor: TdxScaleFactor;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar); virtual;

    property NavigationBar: TdxNavBarCustomOfficeNavigationBar read FNavigationBar;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  TdxNavBarOfficeNavigationBarPersistent = class(TcxOwnedPersistent)
  private
    function GetNavigationBar: TdxNavBarCustomOfficeNavigationBar;
  protected
    procedure Changed; virtual;
  protected
    property NavigationBar: TdxNavBarCustomOfficeNavigationBar read GetNavigationBar;
  end;

  TdxNavBarOfficeNavigationBarCustomizationButtonVisibility = (cbvShowAfterItems, cbvShowBeforeItems, cbvHidden);

  TdxNavBarOfficeNavigationBarOptionsView = class(TdxNavBarOfficeNavigationBarPersistent)
  private
    FOrientation: TdxOrientation;
    FCustomizationButtonVisibility: TdxNavBarOfficeNavigationBarCustomizationButtonVisibility;
    FMaxVisibleItemCount: Integer;
    FPaddings: TcxMargin;
    FShowItemsAsButtons: Boolean;
    FItemAlignment: TcxAlignment;
    FItemSpacing: Integer;
    FDrawParentBackground: Boolean;
    FCompactNavigation: Boolean;

    procedure AlignmentChanged(Sender: TObject);
    procedure PaddingsChanged(Sender: TObject);
    procedure SetCustomizationButtonVisibility(const Value: TdxNavBarOfficeNavigationBarCustomizationButtonVisibility);
    procedure SetItemAlignment(AValue: TcxAlignment);
    procedure SetItemSpacing(Value: Integer);
    procedure SetMaxVisibleItemCount(Value: Integer);
    procedure SetOrientation(const Value: TdxOrientation);
    procedure SetPaddings(const Value: TcxMargin);
    procedure SetShowItemsAsButtons(const Value: Boolean);
    procedure SetDrawParentBackground(const Value: Boolean);
    procedure SetCompactNavigation(const Value: Boolean);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property CompactNavigation: Boolean read FCompactNavigation write SetCompactNavigation default False;
    property CustomizationButtonVisibility: TdxNavBarOfficeNavigationBarCustomizationButtonVisibility read FCustomizationButtonVisibility write SetCustomizationButtonVisibility default cbvShowAfterItems;
    property DrawParentBackground: Boolean read FDrawParentBackground write SetDrawParentBackground default True;
    property ItemAlignment: TcxAlignment read FItemAlignment write SetItemAlignment;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing default 4;
    property MaxVisibleItemCount: Integer read FMaxVisibleItemCount write SetMaxVisibleItemCount default 0;
    property Orientation: TdxOrientation read FOrientation write SetOrientation default orHorizontal;
    property Paddings: TcxMargin read FPaddings write SetPaddings;
    property ShowItemsAsButtons: Boolean read FShowItemsAsButtons write SetShowItemsAsButtons default False;
  end;

  TdxNavBarOfficeNavigationBarOptionsBehavior = class(TdxNavBarOfficeNavigationBarPersistent)
  private
    FAllowDragDrop: Boolean;
    FAnimation: Boolean;
    FShowPeekFormOnItemHover: Boolean;
  protected
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property AllowDragDrop: Boolean read FAllowDragDrop write FAllowDragDrop default True;
    property Animation: Boolean read FAnimation write FAnimation default True;
    property ShowPeekFormOnItemHover: Boolean read FShowPeekFormOnItemHover write FShowPeekFormOnItemHover default True;
  end;

  { TdxNavBarOfficeNavigationBarCustomViewInfo }

  TdxNavBarOfficeNavigationBarCustomViewInfo = class(TdxNavBarOfficeNavigationBarSubclass)
  strict private
    FBounds: TRect;

    function GetController: TdxNavBarOfficeNavigationBarController;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    FIsRightToLeftConverted: Boolean;

    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    procedure DropIsRightToLeftConverted; virtual;
    procedure RightToLeftConversion(const AClientBounds: TRect);

  public
    procedure Paint(ACanvas: TcxCanvas); virtual; abstract;
    //
    property Bounds: TRect read FBounds write FBounds;
    property Controller: TdxNavBarOfficeNavigationBarController read GetController;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

  { TdxNavBarOfficeNavigationBarCustomItemViewInfo }

  TdxNavBarOfficeNavigationBarCustomItemViewInfo = class(TdxNavBarOfficeNavigationBarCustomViewInfo)
  private
    FIsDragging: Boolean;
    FIsDragImagePainting: Boolean;
    FRequiredSize: TSize;
    FState: TcxCalendarElementState;
    FVisibleElements: TList<TdxNavBarOfficeNavigationBarCustomItemViewInfo>;

    function GetTextColor: TdxAlphaColor;
    procedure SetState(const Value: TcxCalendarElementState);
    procedure SetIsDragging(const Value: Boolean);
  protected
    procedure Add(AItemViewInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
    procedure AddVisibleElements; virtual;
    procedure CalculateChildRequiredSizes(const AAvailableSize: TSize); virtual;
    procedure ClearVisibleElements; virtual;
    procedure CreateElements; virtual;
    procedure DestroyElements; virtual;
    function DoCalculateRequiredSize(const AAvailableSize: TSize): TSize; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DoSetIsDragging(Value: Boolean); virtual;
    procedure DoSetState(Value: TcxCalendarElementState); virtual;
    procedure DropIsRightToLeftConverted; override;
    function GetDrawState: TcxCalendarElementState; virtual;
    function GetHitTest(AHitTest: TObject): Boolean; virtual;
    function GetHitTestIndex: Integer; virtual;
    procedure Initialize; virtual;
    procedure InitializeVisibleElements; virtual;
    procedure Invalidate; virtual;
    procedure PaintChildren(ACanvas: TcxCanvas); virtual;
    procedure PaintDragImage(ACanvas: TcxCanvas);
    procedure DoPaintDragImage(ACanvas: TcxCanvas); virtual;
    function PtInElement(const APoint: TPoint): Boolean; virtual;
    procedure SetHitTest(AHitTest: TObject); virtual;

    property IsDragging: Boolean read FIsDragging write SetIsDragging;
    property IsDragImagePainting: Boolean read FIsDragImagePainting;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar); override;
    destructor Destroy; override;
    procedure CalculateBounds; virtual;
    procedure CalculateRequiredSize(const AAvailableSize: TSize); virtual;
    procedure Click; virtual;
    function CreateDragImage: TdxSmartImage;
    procedure Paint(ACanvas: TcxCanvas); override;

    property RequiredSize: TSize read FRequiredSize;
    property State: TcxCalendarElementState read FState write SetState;
  end;

  TdxNavBarOfficeNavigationBarItemViewInfo = class(TdxNavBarOfficeNavigationBarCustomItemViewInfo)
  private const
    MoveAnimationTime = 100;
    MoveAnimationTransitionEffect: TdxAnimationTransitionEffect = ateAccelerateDecelerate;
    PressedAnimationTime = 100;
    PressedAnimationFrameCount = 100;
    PressedAnimationScaleFactor = 0.9;
    PressedImageScaleFactor = PressedAnimationFrameCount/(1- PressedAnimationScaleFactor);
    PressedAnimationTransitionEffect: TdxAnimationTransitionEffect = ateAccelerateDecelerate;
  private
    FMoveAnimation: TdxAnimationTransition;
    FMoveAnimationBounds: TRect;
    FMoveAnimationLength: Integer;
    FMoveAnimationActive: Boolean;
    FDragDropImage: TdxSmartImage;

    FPressAnimation: TdxAnimationTransition;
    FPressAnimationDirection: TcxDirection;
    FPressAnimationPosition: Integer;
    FPressAnimationActive: Boolean;
    FPressAnimationStartPos: Integer;
    FPressedItemImage: TdxSmartImage;

    FImageIndex: TcxImageIndex;
    FItem: TObject;
    FText: string;
    FContentBounds: TRect;
    procedure DoDrawItemText(AGpCanvas: TdxGPCanvas);
    procedure DoMoveAnimate(Sender: TdxAnimationTransition;
      var APosition: Integer; var AFinished: Boolean);
    procedure DoMoveAnimationTerminate(Sender: TObject);
    procedure DoPressAnimate(Sender: TdxAnimationTransition;
      var APosition: Integer; var AFinished: Boolean);
    procedure DoPressAnimationTerminate(Sender: TObject);
    procedure DrawImageSelection(ACanvas: TcxCanvas);
    procedure DrawItemContent(ACanvas: TcxCanvas);
    function GetContentOffsets: TRect;
    function IsButton: Boolean;
    function IsCompact: Boolean;
    function GetItem: IdxNavigationItem;
    procedure StopMoveAnimation;
    procedure StopPressAnimation;
  protected
    procedure DoBeforeMoveAnimation;
    function DoCalculateRequiredSize(const AAvailableSize: TSize): TSize; override;
    procedure DoMoveAnimation;
    procedure DoPaintDragImage(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DoSetIsDragging(Value: Boolean); override;
    procedure DoSetState(Value: TcxCalendarElementState); override;
    function GetContentSize: TSize; virtual;
    function GetDrawState: TcxCalendarElementState; override;
    function GetHitTestIndex: Integer; override;
    function IsSelected: Boolean;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar; AItem: TObject); reintroduce; virtual;
    destructor Destroy; override;
    procedure CalculateBounds; override;
    procedure Click; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property Text: string read FText write FText;
    property ImageIndex: TcxImageIndex read FImageIndex write FImageIndex;
    property Item: IdxNavigationItem read GetItem;
  end;

  TdxNavBarOfficeNavigationBarItemsViewInfo = class(TdxNavBarOfficeNavigationBarCustomItemViewInfo)
  private
    FItems: TObjectList<TdxNavBarOfficeNavigationBarItemViewInfo>;
    procedure CreateItems;
    procedure DestroyItems;
    function GetItemDistance: Integer;
  protected
    function CreateItemInfo(AItem: TObject): TdxNavBarOfficeNavigationBarItemViewInfo; virtual;

    procedure AddVisibleElements; override;
    procedure ClearVisibleElements; override;
    function DoCalculateRequiredSize(const AAvailableSize: TSize): TSize; override;
  public
    destructor Destroy; override;
    procedure CalculateBounds; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);
  end;

  TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo = class(TdxNavBarOfficeNavigationBarCustomItemViewInfo)
  protected
    function DoCalculateRequiredSize(const AAvailableSize: TSize): TSize; override;
    function GetHitTestIndex: Integer; override;
  public
    procedure Click; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxNavBarOfficeNavigationBarDragItemViewInfo = class(TdxNavBarOfficeNavigationBarCustomViewInfo)
  private
    FAnimationStartPos: Integer;
    FAnimationDirection: TcxDirection;
    FImage: TdxSmartImage;
    FIsAnimationActive: Boolean;
    FDragOffset: TPoint;
    procedure DoAnimate(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
  protected
    procedure DoEndDragAnimation;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar;
      ADraggedItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo; const ADragPoint: TPoint); reintroduce; virtual;
    destructor Destroy; override;
    procedure CalculateBounds;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxNavBarOfficeNavigationBarViewInfo }

  TdxNavBarOfficeNavigationBarViewInfo = class(TdxNavBarOfficeNavigationBarCustomItemViewInfo)
  private
    FCustomizationButtonViewInfo: TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo;
    FDragItem: TdxNavBarOfficeNavigationBarDragItemViewInfo;
    FItemFont: TFont;
    FItems: TdxNavBarOfficeNavigationBarItemsViewInfo;

    function GetContentOffsets: TRect;
    function GetItemFontSize: Integer;
    procedure InitializeDefaultItemFont;
  protected
    function CreateCustomizationButtonInfo: TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo; virtual;
    function CreateItemsInfo: TdxNavBarOfficeNavigationBarItemsViewInfo; virtual;

    procedure AddVisibleElements; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    function DoCalculateRequiredSize(const AAvailableSize: TSize): TSize; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);
    function GetHitTestIndex: Integer; override;
    procedure Initialize; override;

    procedure CheckBiDiMode;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    function UseRightToLeftScrollBar: Boolean;

    property ItemFont: TFont read FItemFont;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar); override;
    destructor Destroy; override;

    procedure CreateDragItem(ADraggedItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo; const ADragPoint: TPoint);
    procedure DestroyDragItem;

    procedure CalculateBounds; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TdxNavBarOfficeNavigationBarHitTest = class(TdxNavBarOfficeNavigationBarSubclass)
  private
    FHitObject: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
    FFlags: Int64;
    FHitPoint: TPoint;
    function GetBitState(AIndex: Integer): Boolean;
  public
    procedure Clear;
    procedure Recalculate;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
    procedure Calculate(const APoint: TPoint); virtual;

    property HitAtBackground: Boolean index nbhtBackground read GetBitState;
    property HitAtItem: Boolean index nbhtItem read GetBitState;
    property HitAtCustomizationButton: Boolean index nbhtCustomizationButton read GetBitState;
    property HitObject: TdxNavBarOfficeNavigationBarCustomItemViewInfo read FHitObject write FHitObject;
    property HitPoint: TPoint read FHitPoint;
  end;

  TdxNavBarOfficeNavigationBarPeekForm = class(TdxCustomCalloutPopupWindow);

  TdxNavBarOfficeNavigationBarController = class(TdxNavBarOfficeNavigationBarSubclass)
  private
    type
      TPeekFormVisibilityController = class
      private const
        HideTimerInterval = 1000;
        TrackTimerInterval = 500;
        ShowTimerShortInterval = 200;
        ShowTimerLongInterval = 1000;
       private
        FController: TdxNavBarOfficeNavigationBarController;
        FHideTimer: TcxTimer;
        FPeekForm: TdxNavBarOfficeNavigationBarPeekForm;
        FPeekFormOwnerElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
        FPrevHotElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
        FShowTimer: TcxTimer;
        FTrackTimer: TcxTimer;
        procedure HideTimerExpired(Sender: TObject);
        function IsVisible: Boolean;
        procedure ShowTimerExpired(Sender: TObject);
        procedure StartHideTimer;
        procedure StartShowTimer;
        procedure StartTrackTimer;
        procedure StopHideTimer;
        procedure StopShowTimer;
        procedure StopTrackTimer;
        procedure TrackTimerExpired(Sender: TObject);
      public
        constructor Create(AController: TdxNavBarOfficeNavigationBarController);
        destructor Destroy; override;
        procedure Check(AHotElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
        procedure ElementDestroying(AElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
        procedure Hide;
        procedure MouseEnter;
        procedure MouseLeave;
        procedure PeekFormClosed(Sender: TObject);
      end;
  private
    FDragItemInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
    FDragStartPoint: TPoint;
    FHitTest: TdxNavBarOfficeNavigationBarHitTest;
    FHotElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
    FPeekFormVisibilityController: TPeekFormVisibilityController;
    FPressedElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;

    procedure CheckHotElement(AShift: TShiftState; const APoint: TPoint);
    procedure SetHotElement(const Value: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
    procedure SetPressedElement(const Value: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
    procedure HidePeekForm;
    function CanShowPeekForm: Boolean;
    procedure DoShowPeekForm(AItemInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo; var AControl: TWinControl);
  protected
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ElementDestroying(AElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);

    procedure BeginDragAndDrop;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);
    procedure EndDragAndDrop(Accepted: Boolean);
    function IsDragged(AItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo): Boolean;
    function StartDragAndDrop(const P: TPoint): Boolean;

    property DragItemInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo read FDragItemInfo;
    property HotElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo read FHotElement write SetHotElement;
    property PressedElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo read FPressedElement write SetPressedElement;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseEnter;
    procedure MouseLeave;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property HitTest: TdxNavBarOfficeNavigationBarHitTest read FHitTest;
  end;

  TdxNavBarOfficeNavigationBarItem = class(TcxInterfacedCollectionItem, IdxNavigationItem)
  private
    FText: string;
    FVisible: Boolean;
    FImageIndex: TcxImageIndex;
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetText(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    // IdxNavigationItem
    function GetID: Integer;
    function GetText: string;
    function GetImageIndex: TcxImageIndex;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TdxNavBarOfficeNavigationBarItems = class(TCollection, IUnknown,
    IEnumerable<IdxNavigationItem>, IEnumerable, IdxNavigationClient)
  private
    FNavigationBar: TdxNavBarCustomOfficeNavigationBar;
    FSelectedItem: TdxNavBarOfficeNavigationBarItem;
    function GetItem(Index: Integer): TdxNavBarOfficeNavigationBarItem;
    procedure SetItem(Index: Integer;
      const Value: TdxNavBarOfficeNavigationBarItem);
    procedure SetSelectedItem(const Value: TdxNavBarOfficeNavigationBarItem);
  private
    type
      TNavigationItemEnumerator = class(TInterfacedObject, IEnumerator<IdxNavigationItem>, IEnumerator)
      private
        FIndex: Integer;
        FCollection: TdxNavBarOfficeNavigationBarItems;
      public
        constructor Create(ACollection: TdxNavBarOfficeNavigationBarItems);
        function GetCurrent: TObject;
        function IEnumerator<IdxNavigationItem>.GetCurrent = GetCurrentNavigationItem;
        function GetCurrentNavigationItem: IdxNavigationItem;
        function MoveNext: Boolean;
        procedure Reset;
      end;
  protected
    procedure Notify(Item: TCollectionItem; Action: Classes.TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IEnumerable
    function GetEnumerator: IEnumerator;
    // IEnumerable<IdxNavigationItem>
    function IEnumerable<IdxNavigationItem>.GetEnumerator = GetNavigationItemEnumerator;
    function GetNavigationItemEnumerator: IEnumerator<IdxNavigationItem>;
    // IdxNavigationClient
    function IdxNavigationClient.GetItems = GetNavigationClientItems;
    procedure IdxNavigationClient.SetSelectedItem = SetNavigationClientSelectedItem;
    function IdxNavigationClient.GetSelectedItem = GetNavigationClientSelectedItem;
    function GetNavigationClientItems: IEnumerable<IdxNavigationItem>;
    procedure SetNavigationClientSelectedItem(AItem: IdxNavigationItem);
    function GetNavigationClientSelectedItem: IdxNavigationItem;
    procedure AddListener(AListener: IdxNavigationClientListener);
    procedure RemoveListener(AListener: IdxNavigationClientListener);
    property NavigationBar: TdxNavBarCustomOfficeNavigationBar read FNavigationBar;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar; AItemClass: TCollectionItemClass);
    function Add: TdxNavBarOfficeNavigationBarItem;
    property Items[Index: Integer]: TdxNavBarOfficeNavigationBarItem read GetItem write SetItem; default;
    property SelectedItem: TdxNavBarOfficeNavigationBarItem read FSelectedItem write SetSelectedItem;
  end;

  TdxNavBarOfficeNavigationBarQueryPeekFormContent = procedure (ASender: TObject;
    ANavigationItem: IdxNavigationItem; var AControl: TWinControl) of object;
  TdxNavBarOfficeNavigationBarSelectionChanging = procedure (ASender: TObject;
    ANavigationItem: IdxNavigationItem; var AAllow: Boolean) of object;

  TdxNavBarCustomOfficeNavigationBar = class(TcxControl,
    IdxSkinSupport,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2,
    IcxMouseTrackingCaller3,
    IdxNavigationClientListener)
  private
    FAutoSize: Boolean;
    FChangedCounter: Integer;
    FOptionsBehavior: TdxNavBarOfficeNavigationBarOptionsBehavior;
    FOptionsView: TdxNavBarOfficeNavigationBarOptionsView;
    FController: TdxNavBarOfficeNavigationBarController;
    FCustomizationButtonPopup: TdxCustomBuiltInPopupMenuAdapter;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FItemProvider: TComponent;
    FItems: TdxNavBarOfficeNavigationBarItems;
    FSortedItems: TdxFastObjectList;
    FOrderList: TList<Integer>;
    FViewInfo: TdxNavBarOfficeNavigationBarViewInfo;
    FIsSizeCalculated: Boolean;
    FIsViewInfoDirty: Boolean;
    FLoadedItemProvider: TComponent;
    FLockCount: Integer;
    FOnQueryPeekFormContent: TdxNavBarOfficeNavigationBarQueryPeekFormContent;
    FOnSelectionChanged: TNotifyEvent;
    FOnSelectionChanging: TdxNavBarOfficeNavigationBarSelectionChanging;

    procedure DoCustomizationButtonClick(Sender: TObject);
    function GetSortedItems(Index: Integer): IdxNavigationItem;
    function GetSortedItemCount: Integer;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItemProvider(Value: TComponent);
    procedure SetItems(const Value: TdxNavBarOfficeNavigationBarItems);
    procedure SetOptionsBehavior(const Value: TdxNavBarOfficeNavigationBarOptionsBehavior);
    procedure SetOptionsView(const Value: TdxNavBarOfficeNavigationBarOptionsView);
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure BoundsChanged; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DoPaint; override;
    procedure FontChanged; override;
    procedure InitControl; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
    // Drag&Drop
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    // IcxMouseTrackingCaller
    procedure MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller2.MouseLeave = MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller3.MouseLeave = MouseTrackingMouseLeave;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    // IcxMouseTrackingCaller3
    function IsCaptureMouse: Boolean;

    // IdxNavigationClientListener
    procedure ItemsChanged;
    procedure SelectionChanged;

    function CreateController: TdxNavBarOfficeNavigationBarController; virtual;
    function CreateViewInfo: TdxNavBarOfficeNavigationBarViewInfo; virtual;

    function CreateOptionsBehavior: TdxNavBarOfficeNavigationBarOptionsBehavior; virtual;
    function CreateOptionsView: TdxNavBarOfficeNavigationBarOptionsView; virtual;

    procedure CreateSubclasses; virtual;
    procedure DestroySubclasses; virtual;

    procedure CustomizationButtonClick;
    procedure Changed;
    procedure CheckChanges;
    procedure DoShowPeekForm(AItem: IdxNavigationItem; var AControl: TWinControl);
    function IsAutoHeight: Boolean; virtual;
    function IsAutoWidth: Boolean; virtual;
    function IsLocked: Boolean;
    procedure ItemClick(AItem: TdxNavBarOfficeNavigationBarItemViewInfo);
    procedure UpdateSortedItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDragAndDrop; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure HidePeekForm;
    function GetActiveItemProvider: IdxNavigationClient;
    procedure ShowCustomizationForm;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Controller: TdxNavBarOfficeNavigationBarController read FController;
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TdxNavBarOfficeNavigationBarItems read FItems write SetItems;
    property ItemProvider: TComponent read FItemProvider write SetItemProvider;
    property OptionsBehavior: TdxNavBarOfficeNavigationBarOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsView: TdxNavBarOfficeNavigationBarOptionsView read FOptionsView write SetOptionsView;
    property ViewInfo: TdxNavBarOfficeNavigationBarViewInfo read FViewInfo;
    property SortedItems[Index: Integer]: IdxNavigationItem read GetSortedItems;
    property SortedItemCount: Integer read GetSortedItemCount;
    //
    property OnQueryPeekFormContent: TdxNavBarOfficeNavigationBarQueryPeekFormContent read FOnQueryPeekFormContent write FOnQueryPeekFormContent;
    property OnSelectionChanged:  TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnSelectionChanging: TdxNavBarOfficeNavigationBarSelectionChanging read FOnSelectionChanging write FOnSelectionChanging;
  end;

  { TdxNavBarOfficeNavigationBar }

  TdxNavBarOfficeNavigationBar = class(TdxNavBarCustomOfficeNavigationBar)
  published
    property Align default alBottom;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property ItemProvider;
    property Items;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsView;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
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
    property OnQueryPeekFormContent;
    property OnResize;
    property OnSelectionChanged;
    property OnSelectionChanging;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TdxNavBarOfficeNavigationBarSubclass }

uses
  dxNavBarOfficeNavigationBarCustomizationDlg, dxNavBarConsts, dxDPIAwareUtils;

constructor TdxNavBarOfficeNavigationBarSubclass.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar);
begin
  inherited Create;
  FNavigationBar := ANavigationBar;
end;

function TdxNavBarOfficeNavigationBarSubclass.GetScaleFactor: TdxScaleFactor;
begin
  Result := NavigationBar.ScaleFactor;
end;

{ TdxNavBarOfficeNavigationBarPersistent }

procedure TdxNavBarOfficeNavigationBarPersistent.Changed;
begin
  NavigationBar.Changed;
end;

function TdxNavBarOfficeNavigationBarPersistent.GetNavigationBar: TdxNavBarCustomOfficeNavigationBar;
begin
  Result := Owner as TdxNavBarCustomOfficeNavigationBar;
end;

{ TdxNavBarOfficeNavigationBarOptionsView }

constructor TdxNavBarOfficeNavigationBarOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItemAlignment := TcxAlignment.Create(Self, False, taLeftJustify, vaCenter);
  FItemAlignment.OnChanged := AlignmentChanged;
  FItemSpacing := 4;
  FPaddings := TcxMargin.Create(Self);
  FPaddings.OnChange := PaddingsChanged;
  FDrawParentBackground := True;
end;

destructor TdxNavBarOfficeNavigationBarOptionsView.Destroy;
begin
  FreeAndNil(FPaddings);
  FreeAndNil(FItemAlignment);
  inherited Destroy;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.ChangeScale(M, D: Integer);
begin
  ItemSpacing := MulDiv(ItemSpacing, M, D);
  Paddings.ChangeScale(M, D);
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.DoAssign(Source: TPersistent);
var
  ASourceOptions: TdxNavBarOfficeNavigationBarOptionsView;
begin
  if Source is TdxNavBarOfficeNavigationBarOptionsView then
  begin
    ASourceOptions := TdxNavBarOfficeNavigationBarOptionsView(Source);
    NavigationBar.BeginUpdate;
    try
      CompactNavigation := ASourceOptions.CompactNavigation;
      CustomizationButtonVisibility := ASourceOptions.CustomizationButtonVisibility;
      DrawParentBackground := ASourceOptions.DrawParentBackground;
      ItemAlignment := ASourceOptions.ItemAlignment;
      ItemSpacing := ASourceOptions.ItemSpacing;
      MaxVisibleItemCount := ASourceOptions.MaxVisibleItemCount;
      Orientation := ASourceOptions.Orientation;
      Paddings := ASourceOptions.Paddings;
      ShowItemsAsButtons := ASourceOptions.ShowItemsAsButtons;
    finally
      NavigationBar.EndUpdate;
    end;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.AlignmentChanged(
  Sender: TObject);
begin
  Changed;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.PaddingsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetCompactNavigation(
  const Value: Boolean);
begin
  if FCompactNavigation <> Value then
  begin
    FCompactNavigation := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetCustomizationButtonVisibility(
  const Value: TdxNavBarOfficeNavigationBarCustomizationButtonVisibility);
begin
  if FCustomizationButtonVisibility <> Value then
  begin
    FCustomizationButtonVisibility := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetItemAlignment(
  AValue: TcxAlignment);
begin
  FItemAlignment.Assign(AValue);
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetItemSpacing(
  Value: Integer);
begin
  Value := Max(Value, 0);
  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetMaxVisibleItemCount(
  Value: Integer);
begin
  Value := Max(Value, 0);
  if FMaxVisibleItemCount <> Value then
  begin
    FMaxVisibleItemCount := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetOrientation(
  const Value: TdxOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetPaddings(
  const Value: TcxMargin);
begin
  FPaddings.Assign(Value);
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetShowItemsAsButtons(
  const Value: Boolean);
begin
  if FShowItemsAsButtons <> Value then
  begin
    FShowItemsAsButtons := Value;
    Changed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarOptionsView.SetDrawParentBackground(
  const Value: Boolean);
begin
  if FDrawParentBackground <> Value then
  begin
    FDrawParentBackground := Value;
    Changed;
  end;
end;

{ TdxNavBarOfficeNavigationBarOptionsBehavior }

constructor TdxNavBarOfficeNavigationBarOptionsBehavior.Create(
  AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAllowDragDrop := True;
  FAnimation := True;
  FShowPeekFormOnItemHover := True;
end;

procedure TdxNavBarOfficeNavigationBarOptionsBehavior.DoAssign(
  Source: TPersistent);
var
  ASourceOptions: TdxNavBarOfficeNavigationBarOptionsBehavior;
begin
  if Source is TdxNavBarOfficeNavigationBarOptionsBehavior then
  begin
    ASourceOptions := TdxNavBarOfficeNavigationBarOptionsBehavior(Source);
  //  NavigationBar.BeginUpdate;
    try
      AllowDragDrop := ASourceOptions.AllowDragDrop;
      Animation := ASourceOptions.Animation;
      ShowPeekFormOnItemHover := ASourceOptions.ShowPeekFormOnItemHover;
    finally
 //     NavigationBar.EndUpdate;
    end;
  end;
end;

{ TdxNavBarOfficeNavigationBarCustomViewInfo }

function TdxNavBarOfficeNavigationBarCustomViewInfo.GetController: TdxNavBarOfficeNavigationBarController;
begin
  Result := NavigationBar.Controller;
end;

function TdxNavBarOfficeNavigationBarCustomViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := NavigationBar.LookAndFeelPainter;
end;

procedure TdxNavBarOfficeNavigationBarCustomViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
end;

procedure TdxNavBarOfficeNavigationBarCustomViewInfo.DropIsRightToLeftConverted;
begin
  FIsRightToLeftConverted := False;
end;

procedure TdxNavBarOfficeNavigationBarCustomViewInfo.RightToLeftConversion(const AClientBounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(AClientBounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TdxNavBarOfficeNavigationBarCustomItemViewInfo }

constructor TdxNavBarOfficeNavigationBarCustomItemViewInfo.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar);
begin
  inherited Create(ANavigationBar);
  CreateElements;
  FVisibleElements := TList<TdxNavBarOfficeNavigationBarCustomItemViewInfo>.Create;
end;

destructor TdxNavBarOfficeNavigationBarCustomItemViewInfo.Destroy;
begin
  NavigationBar.Controller.ElementDestroying(Self);
  FreeAndNil(FVisibleElements);
  DestroyElements;
  inherited Destroy;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.Add(
  AItemViewInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
begin
  FVisibleElements.Add(AItemViewInfo);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.AddVisibleElements;
begin
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.CalculateChildRequiredSizes(const AAvailableSize: TSize);
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].CalculateRequiredSize(AAvailableSize);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.CalculateBounds;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].CalculateBounds;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.CalculateRequiredSize(const AAvailableSize: TSize);
begin
  CalculateChildRequiredSizes(AAvailableSize);
  FRequiredSize := DoCalculateRequiredSize(AAvailableSize);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.Click;
begin
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.CreateDragImage: TdxSmartImage;
var
  ABitmap: TcxAlphaBitmap;
  ACanvas: TcxCanvas;
begin
  ABitmap := TcxAlphaBitmap.CreateSize(Bounds);
  try
    ABitmap.Clear;
    ACanvas := ABitmap.cxCanvas;
    ACanvas.WindowOrg := Bounds.TopLeft;
    PaintDragImage(ACanvas);
    Result := TdxSmartImage.CreateFromBitmap(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.Paint(
  ACanvas: TcxCanvas);
begin
  PaintChildren(ACanvas);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.ClearVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].ClearVisibleElements;
  FVisibleElements.Clear;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.CreateElements;
begin
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DestroyElements;
begin
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.DoCalculateRequiredSize(const AAvailableSize: TSize): TSize;
begin
  Result := cxNullSize;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(AClientBounds);
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].RightToLeftConversion(AClientBounds);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DoSetIsDragging(Value: Boolean);
begin
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DoSetState(Value: TcxCalendarElementState);
begin
  FState := Value;
  Invalidate;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DropIsRightToLeftConverted;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].DropIsRightToLeftConverted;
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.GetDrawState: TcxCalendarElementState;
begin
  Result := State;
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.GetHitTest(
  AHitTest: TObject): Boolean;
var
  I: Integer;
begin
  Result := (GetHitTestIndex <> nbhtNone) and PtInElement((AHitTest as TdxNavBarOfficeNavigationBarHitTest).HitPoint);
  if Result then
    SetHitTest(AHitTest);
  for I := FVisibleElements.Count - 1 downto 0 do
    if FVisibleElements[I].GetHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.GetHitTestIndex: Integer;
begin
  Result := nbhtNone;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.Initialize;
begin
  ClearVisibleElements;
  AddVisibleElements;
  InitializeVisibleElements;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.InitializeVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].Initialize;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.Invalidate;
begin
  NavigationBar.InvalidateRect(Bounds, True);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.PaintChildren(
  ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].Paint(ACanvas);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.PaintDragImage(
  ACanvas: TcxCanvas);
begin
  FIsDragImagePainting := True;
  try
    DoPaintDragImage(ACanvas);
  finally
    FIsDragImagePainting := False;
  end;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.DoPaintDragImage(ACanvas: TcxCanvas);
begin
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.PtInElement(
  const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, APoint);
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.SetHitTest(
  AHitTest: TObject);
var
  ANavigationBarHitTest: TdxNavBarOfficeNavigationBarHitTest;
begin
  ANavigationBarHitTest := AHitTest as TdxNavBarOfficeNavigationBarHitTest;
  ANavigationBarHitTest.SetBitState(GetHitTestIndex, True);
  ANavigationBarHitTest.HitObject := Self;
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.SetIsDragging(
  const Value: Boolean);
var
  I: Integer;
begin
  FIsDragging := Value;
  for I := 0 to FVisibleElements.Count - 1 do
    FVisibleElements[I].IsDragging := Value;
  DoSetIsDragging(Value);
end;

function TdxNavBarOfficeNavigationBarCustomItemViewInfo.GetTextColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(Painter.OfficeNavigationBarItemTextColor(GetDrawState));
end;

procedure TdxNavBarOfficeNavigationBarCustomItemViewInfo.SetState(const Value: TcxCalendarElementState);
begin
  if FState <> Value then
    DoSetState(Value);
end;

{ TdxNavBarOfficeNavigationBarItemViewInfo }

constructor TdxNavBarOfficeNavigationBarItemViewInfo.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar; AItem: TObject);
begin
  inherited Create(ANavigationBar);
  FItem := AItem;
end;

destructor TdxNavBarOfficeNavigationBarItemViewInfo.Destroy;
begin
  StopPressAnimation;
  FPressedItemImage.Free;
  inherited;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.CalculateBounds;
begin
  FContentBounds := cxRectContent(Bounds, GetContentOffsets);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.Click;
begin
  NavigationBar.ItemClick(Self);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoBeforeMoveAnimation;
begin
  if FMoveAnimationActive then
    StopMoveAnimation
  else
    FMoveAnimationBounds := Bounds;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.DoCalculateRequiredSize(const AAvailableSize: TSize): TSize;
begin
  Result := GetContentSize;
  if not cxSizeIsEmpty(Result) then
    Result := cxSize(Result.cx + cxMarginsWidth(GetContentOffsets),
      Result.cy + cxMarginsHeight(GetContentOffsets));
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoMoveAnimation;
begin
  if NavigationBar.OptionsView.Orientation = orHorizontal then
    FMoveAnimationLength := Bounds.Left - FMoveAnimationBounds.Left
  else
    FMoveAnimationLength := Bounds.Top - FMoveAnimationBounds.Top;

  FMoveAnimation := TdxAnimationTransition.Create(MoveAnimationTime, MoveAnimationTransitionEffect, Abs(FMoveAnimationLength));
  FMoveAnimation.OnAnimate := DoMoveAnimate;
  FMoveAnimation.OnTerminate := DoMoveAnimationTerminate;
  FMoveAnimation.FreeOnTerminate := False;
  FMoveAnimationActive := True;
  FMoveAnimation.Resume;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoMoveAnimationTerminate(
  Sender: TObject);
begin
  FMoveAnimationActive := False;
  Invalidate;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoPaintDragImage(ACanvas: TcxCanvas);

  function IsNotCleared(ABitmap: TcxBitmap): Boolean;
  var
    AColors: TRGBColors;
    I: Integer;
  begin
    Result := False;
    GetBitmapBits(ABitmap, AColors, False);
    for I := Low(AColors) to High(AColors) do
    begin
      Result := (AColors[I].rgbBlue <> 0) or (AColors[I].rgbGreen <> 0) or
        (AColors[I].rgbRed <> 0) or (AColors[I].rgbReserved <> 0);
      if Result then
        Break;
    end;
  end;

  procedure CorrectDragBitmap;
  var
    ABitmap: TcxAlphaBitmap;
  begin
    ABitmap := TcxAlphaBitmap.CreateSize(Bounds);
    try
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
      if not ABitmap.IsAlphaUsed and IsNotCleared(ABitmap) then
        ABitmap.SetAlphaChannel($FF);
      cxDrawBitmap(ACanvas.Handle, ABitmap, Bounds, cxNullPoint);
    finally
      ABitmap.Free;
    end;
  end;

begin
  if IsButton then
  begin
    Painter.OfficeNavigationBarDrawScaledButtonItemBackground(ACanvas, Bounds, GetDrawState, ScaleFactor);
    CorrectDragBitmap;
  end
  else
    if IsCompact then
    begin
      DrawImageSelection(ACanvas);
      CorrectDragBitmap;
    end;

  DrawItemContent(ACanvas);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(FContentBounds, AClientBounds);
  inherited DoRightToLeftConversion(AClientBounds);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoSetIsDragging(Value: Boolean);
begin
  inherited DoSetIsDragging(Value);
  if IsDragging then
  begin
    if NavigationBar.OptionsBehavior.Animation then
      FDragDropImage := CreateDragImage;
    FMoveAnimationBounds := Bounds;
  end
  else
  begin
    StopMoveAnimation;
    FreeAndNil(FDragDropImage);
  end;
  if IsDragging and FPressAnimationActive then
    StopPressAnimation;
  FPressAnimationPosition := 0;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoSetState(
  Value: TcxCalendarElementState);

  procedure RecreatePressedItemImage;
  begin
    FPressedItemImage.Free;
    FPressedItemImage := CreateDragImage;
  end;

  procedure DoPressAnimation(ADirection: TcxDirection);
  var
    ALength: Integer;
    ATime: Cardinal;
  begin
    FPressAnimationDirection := ADirection;
    FPressAnimationStartPos := FPressAnimationPosition;
    if FPressAnimationDirection = dirDown then
      ALength := PressedAnimationFrameCount - FPressAnimationStartPos
    else // dirUp
      ALength := FPressAnimationStartPos;
    ATime := Round(PressedAnimationTime * ALength/PressedAnimationFrameCount);
    FPressAnimation := TdxAnimationTransition.Create(ATime, PressedAnimationTransitionEffect, ALength);
    FPressAnimation.FreeOnTerminate := False;
    FPressAnimation.OnAnimate := DoPressAnimate;
    FPressAnimation.OnTerminate := DoPressAnimationTerminate;
    FPressAnimation.Resume;
    FPressAnimationActive := True;
  end;

var
  AWasPressed: Boolean;
begin
  AWasPressed := State = cesPressed;
  inherited DoSetState(Value);
  if NavigationBar.OptionsBehavior.Animation then
    if State = cesPressed then
    begin
      if FPressAnimationActive then
        StopPressAnimation
      else
        RecreatePressedItemImage;
      DoPressAnimation(dirDown);
    end
    else
      if AWasPressed then
      begin
        if FPressAnimationActive then
          StopPressAnimation;
        DoPressAnimation(dirUp);
      end;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.GetContentSize: TSize;
var
  AGpCanvas: TdxGPGraphics;
  ATextRect: TRect;
begin
  if IsCompact then
  begin
    Result := dxGetImageSize(NavigationBar.Images, ScaleFactor);
    if NavigationBar.OptionsView.Orientation = orVertical then
      Result := cxSize(Result.cy, Result.cx);
  end
  else
    if FText <> '' then
    begin
      AGpCanvas := TdxGPGraphics.Create(cxScreenCanvas.Handle);
      try
        dxGPGetTextRect(AGpCanvas, FText, NavigationBar.ViewInfo.ItemFont, False, cxNullRect, ATextRect);
        if NavigationBar.OptionsView.Orientation = orHorizontal then
          Result := cxSize(ATextRect)
        else
          Result := cxSize(ATextRect.Bottom, ATextRect.Right);
      finally
        AGpCanvas.Free;
        cxScreenCanvas.Dormant;
      end;
    end
    else
      Result := cxNullSize;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.GetDrawState: TcxCalendarElementState;
begin
  if IsSelected and (State <> cesPressed) then
    Result := cesSelected
  else
    Result := inherited GetDrawState;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.GetHitTestIndex: Integer;
begin
  Result := nbhtItem;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.GetItem: IdxNavigationItem;
begin
  Supports(FItem, IdxNavigationItem, Result);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.StopMoveAnimation;
begin
  FreeAndNil(FMoveAnimation);
  FMoveAnimationActive := False;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.StopPressAnimation;
begin
  FreeAndNil(FPressAnimation);
  FPressAnimationActive := False;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.IsSelected: Boolean;
begin
  Result := NavigationBar.GetActiveItemProvider.GetSelectedItem = Item;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.Paint(ACanvas: TcxCanvas);
var
  AWidth, AHeight: Integer;
  APressedImageScaleFactor: Double;
begin
  if not NavigationBar.Controller.IsDragged(Self) then
  begin
    if NavigationBar.OptionsBehavior.Animation and IsDragging then
      FDragDropImage.StretchDraw(ACanvas.Handle, FMoveAnimationBounds)
    else
      if NavigationBar.OptionsBehavior.Animation and
        (FPressAnimationActive or (State = cesPressed)) then
      begin
        APressedImageScaleFactor := 1 - (FPressAnimationPosition)/PressedImageScaleFactor;
        AWidth := Round(cxRectWidth(Bounds)* APressedImageScaleFactor);
        AHeight := Round(cxRectHeight(Bounds)* APressedImageScaleFactor);
        FPressedItemImage.StretchDraw(ACanvas.Handle, cxRectCenter(Bounds, AWidth, AHeight));
      end
      else
      begin
        if IsButton then
          Painter.OfficeNavigationBarDrawScaledButtonItemBackground(ACanvas, Bounds, GetDrawState, ScaleFactor)
        else
        begin
          Painter.OfficeNavigationBarDrawScaledItemBackground(ACanvas, Bounds, GetDrawState, ScaleFactor);
          if IsCompact then
            DrawImageSelection(ACanvas);
        end;
        DrawItemContent(ACanvas);
      end;
  end;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoDrawItemText(AGpCanvas: TdxGPCanvas);

  procedure DrawText(AGpCanvas: TdxGPCanvas; const R: TRect; AColor: TColor);
  const
    AAlignment: array[Boolean] of TAlignment = (taLeftJustify, taRightJustify);
  const
    ARendering: array [Boolean] of TdxGpTextRenderingHint = (TextRenderingHintSystemDefault, TextRenderingHintAntiAliasGridFit);
  begin
    dxGPDrawText(AGpCanvas, Text, R, NavigationBar.ViewInfo.ItemFont, dxColorToAlphaColor(AColor),
      AAlignment[NavigationBar.UseRightToLeftAlignment], taVerticalCenter, False, ARendering[IsDragImagePainting],
      StringTrimmingNone, NavigationBar.UseRightToLeftReading);
  end;

  function GetTextColor: TColor;
  begin
    if IsButton then
      Result := Painter.OfficeNavigationBarButtonItemTextColor(GetDrawState)
    else
      Result := Painter.OfficeNavigationBarItemTextColor(GetDrawState);
  end;

var
  R: TRect;
begin
  AGpCanvas.SmoothingMode := smAntiAlias;
  if NavigationBar.OptionsView.Orientation = orVertical then
  begin
    R.Left := FContentBounds.Left;
    R.Top:= FContentBounds.Bottom;
    R.Bottom := R.Top + cxRectWidth(FContentBounds);
    R.Right := R.Left + cxRectHeight(FContentBounds);
    AGpCanvas.SaveWorldTransform;
    AGpCanvas.RotateWorldTransform(-90, R.TopLeft);
    DrawText(AGpCanvas, R, GetTextColor);
    AGpCanvas.RestoreWorldTransform;
  end
  else
    DrawText(AGpCanvas, FContentBounds, GetTextColor);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoMoveAnimate(
  Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
var
  ARemainder: Integer;
  AOffset: TPoint;
begin
  NavigationBar.InvalidateRect(FMoveAnimationBounds, True);
  ARemainder := Sign(FMoveAnimationLength) * APosition - FMoveAnimationLength;
  if NavigationBar.OptionsView.Orientation = orHorizontal then
    AOffset := cxPoint(ARemainder, 0)
  else
    AOffset := cxPoint(0, ARemainder);
  FMoveAnimationBounds := cxRectOffset(Bounds, AOffset);
  NavigationBar.InvalidateRect(FMoveAnimationBounds, True);
  NavigationBar.Update;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoPressAnimate(Sender: TdxAnimationTransition;
  var APosition: Integer; var AFinished: Boolean);
begin
  if FPressAnimationDirection = dirDown then
    FPressAnimationPosition := FPressAnimationStartPos + APosition
  else // dirUp
    FPressAnimationPosition := FPressAnimationStartPos - APosition;
  Invalidate;
  NavigationBar.Update;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DoPressAnimationTerminate(Sender: TObject);
begin
  FPressAnimationActive := False;
  Invalidate;
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DrawImageSelection(ACanvas: TcxCanvas);
begin
  Painter.OfficeNavigationBarDrawScaledImageSelection(ACanvas, Bounds, GetDrawState, ScaleFactor);
end;

procedure TdxNavBarOfficeNavigationBarItemViewInfo.DrawItemContent(ACanvas: TcxCanvas);

  procedure DrawItemImage(ACanvas: TcxCanvas; const R: TRect);
  begin
    cxDrawImage(ACanvas, R, nil, NavigationBar.Images, ImageIndex, True, nil, ScaleFactor);
  end;

  procedure DrawItemImageRotated;
  var
    ABitmap: TcxAlphaBitmap;
  begin
    ABitmap := TcxAlphaBitmap.CreateSize(FContentBounds);
    try
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, FContentBounds.TopLeft, SRCCOPY);
      ABitmap.Rotate(raMinus90);
      DrawItemImage(ABitmap.cxCanvas, ABitmap.ClientRect);
      ABitmap.Rotate(raPlus90);
      cxDrawBitmap(ACanvas.Handle, ABitmap, FContentBounds, cxNullPoint);
    finally
      ABitmap.Free;
    end;
  end;

begin
  if IsCompact then
  begin
    if ImageIndex >= 0 then
    begin
      if NavigationBar.OptionsView.Orientation = orHorizontal then
        DrawItemImage(ACanvas, FContentBounds)
      else
        DrawItemImageRotated;
    end;
  end
  else
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, Bounds);
    try
      DoDrawItemText(dxGPPaintCanvas);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.GetContentOffsets: TRect;
begin
  if IsButton or IsCompact then
    Result := Painter.OfficeNavigationBarScaledButtonItemContentOffsets(ScaleFactor)
  else
    Result := cxNullRect;

  if NavigationBar.OptionsView.Orientation = orVertical then
    Result := cxRect(Result.Top, Result.Right, Result.Bottom, Result.Left);
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.IsButton: Boolean;
begin
  Result := NavigationBar.OptionsView.ShowItemsAsButtons;
end;

function TdxNavBarOfficeNavigationBarItemViewInfo.IsCompact: Boolean;
begin
  Result := NavigationBar.OptionsView.CompactNavigation and (NavigationBar.Images <> nil);
end;

{ TdxNavBarOfficeNavigationBarItemsViewInfo }

destructor TdxNavBarOfficeNavigationBarItemsViewInfo.Destroy;
begin
  DestroyItems;
  inherited;
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.CalculateBounds;
var
  I: Integer;
  ALeft, ABottom: Integer;
  R: TRect;
  AItemSize: TSize;
begin
  if NavigationBar.OptionsView.Orientation = orHorizontal then
  begin
    ALeft := Bounds.Left;
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      AItemSize := FVisibleElements[I].RequiredSize;
      R := cxRectCenterVertically(Bounds, AItemSize.cy);
      R.Left := ALeft;
      R.Right := R.Left + AItemSize.cx;
      ALeft := R.Right + GetItemDistance;
      FVisibleElements[I].Bounds := R;
    end;
  end
  else
  begin
    ABottom := Bounds.Bottom;
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      AItemSize := FVisibleElements[I].RequiredSize;
      R := cxRectCenterHorizontally(Bounds, AItemSize.cx);
      R.Bottom := ABottom;
      R.Top := R.Bottom - AItemSize.cy;
      ABottom := R.Top - GetItemDistance;
      FVisibleElements[I].Bounds := R;
    end;
  end;
  inherited CalculateBounds;
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.DragAndDrop(const P: TPoint; var Accepted: Boolean);

  procedure DoItemMoveAnimation(ADragItemIndex, ADestinationIndex: Integer);
  var
    AItem: TdxNavBarOfficeNavigationBarItemViewInfo;
    AIsRightToLeftConverted: Boolean;
    I: Integer;
  begin
    AItem := FVisibleElements[ADestinationIndex] as TdxNavBarOfficeNavigationBarItemViewInfo;
    AItem.DoBeforeMoveAnimation;
    FVisibleElements.Move(ADragItemIndex, ADestinationIndex);
    AIsRightToLeftConverted := IsRightToLeftConverted;
    CalculateBounds;
    if AIsRightToLeftConverted then
    begin
      for I := 0 to FVisibleElements.Count - 1 do
        FVisibleElements[I].DoRightToLeftConversion(Bounds);
      RightToLeftConversion(Bounds);
      FIsRightToLeftConverted := True;
    end;
    if NavigationBar.OptionsBehavior.Animation then
      AItem.DoMoveAnimation;
  end;

var
  ASourceIndex: Integer;
  I: Integer;
  ADraggedItemBounds: TRect;
begin
  ASourceIndex := FVisibleElements.IndexOf(NavigationBar.Controller.DragItemInfo);
  ADraggedItemBounds := NavigationBar.Controller.DragItemInfo.Bounds;
  if NavigationBar.OptionsView.Orientation = orHorizontal then
  begin
    if P.X < ADraggedItemBounds.Left then
    begin
      if not NavigationBar.UseRightToLeftAlignment then
        for I := 0 to ASourceIndex - 1 do
        begin
          if P.X < cxRectCenter(FVisibleElements[I].Bounds).X then
          begin
            DoItemMoveAnimation(ASourceIndex, I);
            Break;
          end;
        end
      else
        for I := FVisibleElements.Count - 1 downto ASourceIndex + 1 do
        begin
          if P.X < cxRectCenter(FVisibleElements[I].Bounds).X then
          begin
            DoItemMoveAnimation(ASourceIndex, I);
            Break;
          end;
        end
    end
    else
      if P.X > ADraggedItemBounds.Right then
        if not NavigationBar.UseRightToLeftAlignment then
          for I := FVisibleElements.Count - 1 downto ASourceIndex + 1 do
          begin
            if P.X > cxRectCenter(FVisibleElements[I].Bounds).X then
            begin
              DoItemMoveAnimation(ASourceIndex, I);
              Break;
            end;
          end
        else
          for I := 0 to ASourceIndex - 1 do
          begin
            if P.X > cxRectCenter(FVisibleElements[I].Bounds).X then
            begin
              DoItemMoveAnimation(ASourceIndex, I);
              Break;
            end;
          end
  end
  else
  begin
    if P.Y > ADraggedItemBounds.Bottom then
      for I := 0 to ASourceIndex - 1 do
      begin
        if P.Y > cxRectCenter(FVisibleElements[I].Bounds).Y then
        begin
          DoItemMoveAnimation(ASourceIndex, I);
          Break;
        end;
      end
    else
      if P.Y < ADraggedItemBounds.Top then
        for I := FVisibleElements.Count - 1 downto ASourceIndex + 1 do
        begin
          if P.Y < cxRectCenter(FVisibleElements[I].Bounds).Y then
          begin
            DoItemMoveAnimation(ASourceIndex, I);
            Break;
          end;
        end;
  end;
end;

function TdxNavBarOfficeNavigationBarItemsViewInfo.CreateItemInfo(AItem: TObject): TdxNavBarOfficeNavigationBarItemViewInfo;
begin
  Result := TdxNavBarOfficeNavigationBarItemViewInfo.Create(NavigationBar, AItem);
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.AddVisibleElements;
var
  I: Integer;
  AVisibleItemCount: Integer;
begin
  CreateItems;
  if NavigationBar.OptionsView.MaxVisibleItemCount = 0 then
    AVisibleItemCount := FItems.Count
  else
    AVisibleItemCount := Min(FItems.Count, NavigationBar.OptionsView.MaxVisibleItemCount);
  for I := 0 to AVisibleItemCount - 1 do
    Add(FItems[I]);
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.ClearVisibleElements;
begin
  inherited;
  DestroyItems;
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.CreateItems;
var
  I: Integer;
  AItem: IdxNavigationItem;
begin
  FItems := TObjectList<TdxNavBarOfficeNavigationBarItemViewInfo>.Create;
  for I := 0 to NavigationBar.SortedItemCount - 1 do
  begin
    AItem := NavigationBar.SortedItems[I];
    FItems.Add(CreateItemInfo(NavigationBar.FSortedItems[I]));
    FItems[I].Text := RemoveAccelChars(AItem.Text);
    FItems[I].ImageIndex := AItem.ImageIndex;
  end;
end;

procedure TdxNavBarOfficeNavigationBarItemsViewInfo.DestroyItems;
begin
  FreeAndNil(FItems);
end;

function TdxNavBarOfficeNavigationBarItemsViewInfo.GetItemDistance: Integer;
begin
  Result := NavigationBar.OptionsView.ItemSpacing;
end;

function TdxNavBarOfficeNavigationBarItemsViewInfo.DoCalculateRequiredSize(const AAvailableSize: TSize): TSize;
var
  I: Integer;
  ASize: Integer;
  AIndex: Integer;
begin
  Result := inherited DoCalculateRequiredSize(AAvailableSize);
  AIndex := FVisibleElements.Count;
  for I := 0 to FVisibleElements.Count - 1 do
    if NavigationBar.OptionsView.Orientation = orHorizontal then
    begin
      ASize := Result.cx + FVisibleElements[I].RequiredSize.cx + IfThen(I < FVisibleElements.Count - 1, GetItemDistance);
      if ASize > AAvailableSize.cx then
      begin
        AIndex := I;
        Break;
      end
      else
      begin
        Result.cx := ASize;
        Result.cy := Max(Result.cy, FVisibleElements[I].RequiredSize.cy);
      end;
    end
    else
    begin
      ASize := Result.cy + FVisibleElements[I].RequiredSize.cy + IfThen(I < FVisibleElements.Count - 1, GetItemDistance);
      if ASize > AAvailableSize.cy then
      begin
        AIndex := I;
        Break;
      end
      else
      begin
        Result.cy := ASize;
        Result.cx := Max(Result.cx, FVisibleElements[I].RequiredSize.cx);
      end;
    end;

    for I := FVisibleElements.Count - 1 downto AIndex do
      FVisibleElements.Remove(FVisibleElements[I]);
end;

{ TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo }

procedure TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo.Click;
begin
  NavigationBar.CustomizationButtonClick;
end;

function TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo.DoCalculateRequiredSize(const AAvailableSize: TSize): TSize;
begin
  Result := Painter.OfficeNavigationBarScaledCustomizationButtonSize(ScaleFactor);
end;

function TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo.GetHitTestIndex: Integer;
begin
  Result := nbhtCustomizationButton;
end;

procedure TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.OfficeNavigationBarDrawScaledCustomizationButton(ACanvas, Bounds, State, ScaleFactor, GetTextColor);
end;

{ TdxNavBarOfficeNavigationBarDragItemViewInfo }

constructor TdxNavBarOfficeNavigationBarDragItemViewInfo.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar;
  ADraggedItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo; const ADragPoint: TPoint);
begin
  inherited Create(ANavigationBar);
  FDragOffset := cxPointOffset(ADragPoint,
    ADraggedItem.Bounds.TopLeft, False);
  FImage := ADraggedItem.CreateDragImage;
end;

destructor TdxNavBarOfficeNavigationBarDragItemViewInfo.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxNavBarOfficeNavigationBarDragItemViewInfo.CalculateBounds;
var
  APos: TPoint;
  ADragItemBounds: TRect;
begin
  APos := cxPointOffset(NavigationBar.ScreenToClient(GetMouseCursorPos), FDragOffset, False);
  ADragItemBounds := Controller.DragItemInfo.Bounds;
  if NavigationBar.OptionsView.Orientation = orHorizontal then
    APos.Y := ADragItemBounds.Top
  else
    APos.X := ADragItemBounds.Left;
  Bounds := cxRectSetOrigin(FImage.ClientRect, APos);
end;

procedure TdxNavBarOfficeNavigationBarDragItemViewInfo.Paint(
  ACanvas: TcxCanvas);
const
  DragObjectAlphaValue = 150;
begin
  FImage.StretchDraw(ACanvas.Handle, Bounds, DragObjectAlphaValue);
end;

procedure TdxNavBarOfficeNavigationBarDragItemViewInfo.DoEndDragAnimation;
var
  ADragObjectAnimation: TdxAnimationTransition;
  AEndPos: Integer;
begin
  if NavigationBar.OptionsView.Orientation = orHorizontal then
  begin
    AEndPos := Controller.DragItemInfo.Bounds.Left;
    FAnimationStartPos := Bounds.Left;
    if AEndPos > FAnimationStartPos then
      FAnimationDirection := dirRight
    else
      FAnimationDirection := dirLeft;
  end
  else
  begin
    AEndPos := Controller.DragItemInfo.Bounds.Top;
    FAnimationStartPos := Bounds.Top;
    if AEndPos > FAnimationStartPos then
      FAnimationDirection := dirDown
    else
      FAnimationDirection := dirUp;
  end;
  ADragObjectAnimation := TdxAnimationTransition.Create(100, ateAccelerateDecelerate, Abs(AEndPos - FAnimationStartPos));
  ADragObjectAnimation.OnAnimate := DoAnimate;
  FIsAnimationActive := True;
  try
    ADragObjectAnimation.ImmediateAnimation;
  finally
    FIsAnimationActive := False;
  end;
end;

procedure TdxNavBarOfficeNavigationBarDragItemViewInfo.DoAnimate(
  Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
var
  APos: Integer;
begin
  NavigationBar.InvalidateRect(Bounds, True);
  if FAnimationDirection in [dirRight, dirDown] then
    APos := FAnimationStartPos + APosition
  else
    APos := FAnimationStartPos - APosition;
  if NavigationBar.OptionsView.Orientation = orHorizontal then
    Bounds := cxRectSetOrigin(Bounds, cxPoint(APos, Bounds.Top))
  else
    Bounds := cxRectSetOrigin(Bounds, cxPoint(Bounds.Left, APos));
  NavigationBar.InvalidateRect(Bounds, True);
  NavigationBar.Update;
end;

{ TdxNavBarOfficeNavigationBarViewInfo }

constructor TdxNavBarOfficeNavigationBarViewInfo.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar);
begin
  inherited Create(ANavigationBar);
  FItemFont := TFont.Create;
end;

destructor TdxNavBarOfficeNavigationBarViewInfo.Destroy;
begin
  FreeAndNil(FItemFont);
  inherited Destroy;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.CreateDragItem(
  ADraggedItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo; const ADragPoint: TPoint);
begin
  FDragItem := TdxNavBarOfficeNavigationBarDragItemViewInfo.Create(NavigationBar, ADraggedItem, ADragPoint);
  IsDragging := True;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.DestroyDragItem;
begin
  if NavigationBar.OptionsBehavior.Animation then
    FDragItem.DoEndDragAnimation;
  IsDragging := False;
  FreeAndNil(FDragItem);
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.CalculateBounds;
var
  AAvailableSize, AVisibleElementsSize: Integer;
  ACustomizationButtonRequiredSize: Integer;
  I: Integer;
  ALeft, ABottom: Integer;
  AContentRect, R: TRect;
  AItemSize: TSize;
begin
  Bounds := NavigationBar.Bounds;
  AContentRect := cxRectContent(Bounds, GetContentOffsets);
  if NavigationBar.OptionsView.CustomizationButtonVisibility <> cbvHidden then
    if NavigationBar.OptionsView.Orientation = orHorizontal then
      ACustomizationButtonRequiredSize := FCustomizationButtonViewInfo.RequiredSize.cx
    else
      ACustomizationButtonRequiredSize := FCustomizationButtonViewInfo.RequiredSize.cy
  else
    ACustomizationButtonRequiredSize := 0;
  if NavigationBar.OptionsView.Orientation = orHorizontal then
  begin
    AAvailableSize := cxRectWidth(AContentRect);
    Dec(AAvailableSize, ACustomizationButtonRequiredSize);
    FItems.CalculateRequiredSize(cxSize(AAvailableSize, cxRectHeight(AContentRect)));
    AVisibleElementsSize := FItems.RequiredSize.cx + ACustomizationButtonRequiredSize;
    case NavigationBar.OptionsView.ItemAlignment.Horz of
      taLeftJustify:
        ALeft := AContentRect.Left;
      taRightJustify:
        ALeft := AContentRect.Right - AVisibleElementsSize;
    else //taCenter
      ALeft := cxHalfCoordinate(AContentRect.Left + AContentRect.Right - AVisibleElementsSize);
    end;
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      AItemSize := FVisibleElements[I].RequiredSize;
      R := AContentRect;
      case NavigationBar.OptionsView.ItemAlignment.Vert of
        vaTop:
          R.Bottom := R.Top + AItemSize.cy;
        vaBottom:
          R.Top := R.Bottom - AItemSize.cy
      else //vaCenter
        R := cxRectCenterVertically(AContentRect, AItemSize.cy);
      end;
      R.Left := ALeft;
      R.Right := R.Left + AItemSize.cx;
      ALeft := R.Right;
      FVisibleElements[I].Bounds := R;
    end;
  end
  else
  begin
    AAvailableSize := cxRectHeight(AContentRect);
    Dec(AAvailableSize, ACustomizationButtonRequiredSize);
    FItems.CalculateRequiredSize(cxSize(cxRectWidth(AContentRect), AAvailableSize));
    AVisibleElementsSize := FItems.RequiredSize.cy + ACustomizationButtonRequiredSize;
    case NavigationBar.OptionsView.ItemAlignment.Horz of
      taLeftJustify:
        ABottom := AContentRect.Bottom;
      taRightJustify:
        ABottom := AContentRect.Top + AVisibleElementsSize;
    else //taCenter
      ABottom := cxHalfCoordinate(AContentRect.Bottom + AContentRect.Top + AVisibleElementsSize);
    end;
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      AItemSize := FVisibleElements[I].RequiredSize;
      R := AContentRect;
      case NavigationBar.OptionsView.ItemAlignment.Vert of
        vaTop:
          R.Right := R.Left + AItemSize.cx;
        vaBottom:
          R.Left := R.Right - AItemSize.cx
      else //vaCenter
        R := cxRectCenterHorizontally(AContentRect, AItemSize.cx);
      end;
      R.Bottom := ABottom;
      R.Top := R.Bottom - AItemSize.cy;
      ABottom := R.Top;
      FVisibleElements[I].Bounds := R;
    end;
  end;
  inherited CalculateBounds;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.Paint(ACanvas: TcxCanvas);
begin
  if NavigationBar.OptionsView.DrawParentBackground then
    cxDrawTransparentControlBackground(NavigationBar, ACanvas, Bounds, False)
  else
    Painter.OfficeNavigationBarDrawBackground(ACanvas, Bounds);

  inherited Paint(ACanvas);
  if FDragItem <> nil then
    FDragItem.Paint(ACanvas);
end;

function TdxNavBarOfficeNavigationBarViewInfo.CreateCustomizationButtonInfo: TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo;
begin
  Result := TdxNavBarOfficeNavigationBarCustomizationButtonViewInfo.Create(NavigationBar);
end;

function TdxNavBarOfficeNavigationBarViewInfo.CreateItemsInfo: TdxNavBarOfficeNavigationBarItemsViewInfo;
begin
  Result := TdxNavBarOfficeNavigationBarItemsViewInfo.Create(NavigationBar);
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.AddVisibleElements;
begin
  if NavigationBar.OptionsView.CustomizationButtonVisibility = cbvShowBeforeItems then
    Add(FCustomizationButtonViewInfo);
  Add(FItems);
  if NavigationBar.OptionsView.CustomizationButtonVisibility = cbvShowAfterItems then
    Add(FCustomizationButtonViewInfo);
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.CreateElements;
begin
  FCustomizationButtonViewInfo := CreateCustomizationButtonInfo;
  FItems := CreateItemsInfo;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.DestroyElements;
begin
  FreeAndNil(FItems);
  FreeAndNil(FCustomizationButtonViewInfo);
end;

function TdxNavBarOfficeNavigationBarViewInfo.DoCalculateRequiredSize(const AAvailableSize: TSize): TSize;
var
  I: Integer;
begin
  Result := inherited DoCalculateRequiredSize(AAvailableSize);
  for I := 0 to FVisibleElements.Count - 1 do
    if NavigationBar.OptionsView.Orientation = orHorizontal then
    begin
      Result.cx := Result.cx + FVisibleElements[I].RequiredSize.cx;
      Result.cy := Max(Result.cy, FVisibleElements[I].RequiredSize.cy);
    end
    else
    begin
      Result.cy := Result.cy + FVisibleElements[I].RequiredSize.cy;
      Result.cx := Max(Result.cx, FVisibleElements[I].RequiredSize.cx);
    end;
  Result.cx := Result.cx + cxMarginsWidth(GetContentOffsets);
  Result.cy := Result.cy + cxMarginsHeight(GetContentOffsets);
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  FItems.DragAndDrop(P, Accepted);
  FDragItem.CalculateBounds;
end;

function TdxNavBarOfficeNavigationBarViewInfo.GetHitTestIndex: Integer;
begin
  Result := nbhtBackground;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.Initialize;
begin
  DropIsRightToLeftConverted;
  InitializeDefaultItemFont;
  inherited Initialize;
end;

function TdxNavBarOfficeNavigationBarViewInfo.GetContentOffsets: TRect;
begin
  if NavigationBar.OptionsView.Paddings.All <> 0 then
    Result := NavigationBar.OptionsView.Paddings.Margin
  else
  begin
    Result := Painter.OfficeNavigationBarScaledContentOffsets(ScaleFactor);
    if NavigationBar.OptionsView.Orientation = orVertical then
      Result := cxRectRotate(Result);
  end;
end;

function TdxNavBarOfficeNavigationBarViewInfo.GetItemFontSize: Integer;
begin
  if NavigationBar.OptionsView.ShowItemsAsButtons then
    Result := Painter.OfficeNavigationBarScaledButtonItemFontSize(ScaleFactor)
  else
    Result := Painter.OfficeNavigationBarScaledItemFontSize(ScaleFactor);
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.InitializeDefaultItemFont;
const
  FontNameMap: array[Boolean] of string = ('Segoe UI Light', 'Segoe UI');
begin
  FItemFont.Name := FontNameMap[NavigationBar.OptionsView.ShowItemsAsButtons];
  FItemFont.PixelsPerInch := dxDefaultDPI;
  FItemFont.Size := GetItemFontSize;
end;

procedure TdxNavBarOfficeNavigationBarViewInfo.CheckBiDiMode;
var
  I: Integer;
begin
  if UseRightToLeftAlignment and not FIsRightToLeftConverted then
  begin
    for I := 0 to FVisibleElements.Count - 1 do
      FVisibleElements[I].RightToLeftConversion(Bounds);
    FIsRightToLeftConverted := True;
  end;
end;

function TdxNavBarOfficeNavigationBarViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := NavigationBar.UseRightToLeftAlignment;
end;

function TdxNavBarOfficeNavigationBarViewInfo.UseRightToLeftReading: Boolean;
begin
  Result := NavigationBar.UseRightToLeftReading;
end;

function TdxNavBarOfficeNavigationBarViewInfo.UseRightToLeftScrollBar: Boolean;
begin
  Result := NavigationBar.UseRightToLeftScrollBar;
end;

{ TdxNavBarOfficeNavigationBarHitTest }

procedure TdxNavBarOfficeNavigationBarHitTest.Calculate(const APoint: TPoint);
begin
  FHitPoint := APoint;
  Recalculate;
end;

procedure TdxNavBarOfficeNavigationBarHitTest.Clear;
begin
  FFlags := 0;
  FHitObject := nil;
end;

procedure TdxNavBarOfficeNavigationBarHitTest.Recalculate;
begin
  Clear;
  NavigationBar.ViewInfo.GetHitTest(Self);
end;

procedure TdxNavBarOfficeNavigationBarHitTest.SetBitState(AIndex: Integer;
  AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

function TdxNavBarOfficeNavigationBarHitTest.GetBitState(
  AIndex: Integer): Boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

{ TdxNavBarOfficeNavigationBarController }

constructor TdxNavBarOfficeNavigationBarController.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar);
begin
  inherited;
  FHitTest := TdxNavBarOfficeNavigationBarHitTest.Create(ANavigationBar);
  FPeekFormVisibilityController := TPeekFormVisibilityController.Create(Self);
end;

destructor TdxNavBarOfficeNavigationBarController.Destroy;
begin
  FreeAndNil(FPeekFormVisibilityController);
  FreeAndNil(FHitTest);
  inherited;
end;

procedure TdxNavBarOfficeNavigationBarController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.MouseEnter;
begin
  DoMouseEnter;
end;

procedure TdxNavBarOfficeNavigationBarController.MouseLeave;
begin
  DoMouseLeave;
end;

procedure TdxNavBarOfficeNavigationBarController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  DoMouseMove(Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.DoShowPeekForm(AItemInfo: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
  var AControl: TWinControl);
begin
  NavigationBar.DoShowPeekForm((AItemInfo as TdxNavBarOfficeNavigationBarItemViewInfo).Item, AControl);
end;

function TdxNavBarOfficeNavigationBarController.CanShowPeekForm: Boolean;
begin
  Result := NavigationBar.OptionsBehavior.ShowPeekFormOnItemHover and
    (NavigationBar.DragAndDropState = ddsNone);
end;

procedure TdxNavBarOfficeNavigationBarController.DoMouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button <> mbLeft) or (ssDouble in Shift) then
    Exit;
  CheckHotElement(Shift, cxPoint(X, Y));
  PressedElement := HotElement;
  HidePeekForm;
//  if PressedElement <> nil then
//    PressedElement.MouseDown(Button, Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.DoMouseEnter;
begin
  FPeekFormVisibilityController.MouseEnter;
end;

procedure TdxNavBarOfficeNavigationBarController.DoMouseLeave;
begin
  CheckHotElement([], cxInvisiblePoint);
  PressedElement := nil;
  FPeekFormVisibilityController.MouseLeave;
end;

procedure TdxNavBarOfficeNavigationBarController.DoMouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  if NavigationBar.DragAndDropState <> ddsNone then
    Exit;
  CheckHotElement(Shift, cxPoint(X, Y));
//  if HotElement <> nil then
//    HotElement.MouseMove(Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APreviousPressedElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
begin
  CheckHotElement(Shift, cxPoint(X, Y));
  APreviousPressedElement := PressedElement;
  PressedElement := nil;
  if (HotElement <> nil) and (HotElement = APreviousPressedElement) then
    HotElement.Click;
//
//  if HotElement <> nil then
//    HotElement.MouseUp(Button, Shift, X, Y);
end;

procedure TdxNavBarOfficeNavigationBarController.ElementDestroying(
  AElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
begin
  if HitTest.HitObject = AElement then
    HitTest.Clear;
  if FPressedElement = AElement then
    FPressedElement := nil;
  if FHotElement = AElement then
    FHotElement := nil;
  FPeekFormVisibilityController.ElementDestroying(AElement);
end;

procedure TdxNavBarOfficeNavigationBarController.BeginDragAndDrop;
begin
  FDragItemInfo := HitTest.HitObject as TdxNavBarOfficeNavigationBarCustomItemViewInfo;
  NavigationBar.ViewInfo.CreateDragItem(FDragItemInfo, FDragStartPoint);
end;

procedure TdxNavBarOfficeNavigationBarController.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  Accepted := True;
  NavigationBar.ViewInfo.DragAndDrop(P, Accepted);
  NavigationBar.Invalidate;
end;

procedure TdxNavBarOfficeNavigationBarController.EndDragAndDrop(
  Accepted: Boolean);
begin
  NavigationBar.ViewInfo.DestroyDragItem;
  FDragItemInfo := nil;
end;


function TdxNavBarOfficeNavigationBarController.IsDragged(
  AItem: TdxNavBarOfficeNavigationBarCustomItemViewInfo): Boolean;
begin
  Result := FDragItemInfo = AItem;
end;

function TdxNavBarOfficeNavigationBarController.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := NavigationBar.OptionsBehavior.AllowDragDrop and HitTest.HitAtItem;
  if Result then
    FDragStartPoint := HitTest.HitPoint;
end;

procedure TdxNavBarOfficeNavigationBarController.CheckHotElement(
  AShift: TShiftState; const APoint: TPoint);
begin
  FHitTest.Calculate(APoint);
  HotElement := FHitTest.HitObject;
  if CanShowPeekForm then
    FPeekFormVisibilityController.Check(HotElement)
  else
    HidePeekForm;
end;

procedure TdxNavBarOfficeNavigationBarController.SetHotElement(
  const Value: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
begin
  if FHotElement <> Value then
  begin
    if (FHotElement <> nil) then
      if FHotElement = FPressedElement then
        FHotElement.State := cesPressed
      else
        FHotElement.State := cesNormal;
    FHotElement := Value;
    if (FHotElement <> nil) then
      if FHotElement = FPressedElement then
        FHotElement.State := cesPressed
      else
        FHotElement.State := cesHot;
  end;
end;

procedure TdxNavBarOfficeNavigationBarController.SetPressedElement(
  const Value: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
begin
  if FPressedElement <> Value then
  begin
    if FPressedElement <> nil then
      if FPressedElement = FHotElement then
        FPressedElement.State := cesHot
      else
        FPressedElement.State := cesNormal;
    FPressedElement := Value;
    if FPressedElement <> nil then
      FPressedElement.State := cesPressed;
  end;
end;

procedure TdxNavBarOfficeNavigationBarController.HidePeekForm;
begin
  FPeekFormVisibilityController.Hide;
end;

{ TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController }

constructor TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.Create(
  AController: TdxNavBarOfficeNavigationBarController);
begin
  inherited Create;
  FController := AController;
  FShowTimer := TcxTimer.Create(nil);
  FShowTimer.Enabled := False;
  FShowTimer.OnTimer := ShowTimerExpired;
  FHideTimer := TcxTimer.Create(nil);
  FHideTimer.Enabled := False;
  FHideTimer.OnTimer := HideTimerExpired;
  FHideTimer.Interval := HideTimerInterval;
  FTrackTimer := TcxTimer.Create(nil);
  FTrackTimer.Enabled := False;
  FTrackTimer.OnTimer := TrackTimerExpired;
  FTrackTimer.Interval := TrackTimerInterval;
  FPrevHotElement := nil;
end;

destructor TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.Destroy;
begin
  FreeAndNil(FTrackTimer);
  FreeAndNil(FHideTimer);
  FreeAndNil(FShowTimer);
  FreeAndNil(FPeekForm);
  inherited Destroy;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.Check(
  AHotElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
var
  ANewPeekFormElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo;
  AWasVisible: Boolean;
begin
  if FPrevHotElement = AHotElement then
    Exit;
  FPrevHotElement := AHotElement;
  if FController.HitTest.HitAtItem then
    ANewPeekFormElement := AHotElement
  else
    ANewPeekFormElement := nil;
  if (ANewPeekFormElement <> FPeekFormOwnerElement) and
    not (IsVisible and IsRelatedWindow(FPeekForm, GetFocus)) then
  begin
    if ANewPeekFormElement <> nil then
    begin
      StopHideTimer;
      AWasVisible := IsVisible;
      Hide;
      if AWasVisible then
        FShowTimer.Interval := ShowTimerShortInterval
      else
        FShowTimer.Interval := ShowTimerLongInterval;
      FPeekFormOwnerElement := ANewPeekFormElement;
      StartShowTimer;
    end
    else
      StartHideTimer;
  end
  else
    StopHideTimer;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.ElementDestroying(
  AElement: TdxNavBarOfficeNavigationBarCustomItemViewInfo);
begin
  if AElement = FPeekFormOwnerElement then
    FPeekFormOwnerElement := nil;
  if AElement = FPrevHotElement then
    FPrevHotElement := nil;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.Hide;
begin
  StopShowTimer;
  if IsVisible then
    FPeekForm.CloseUp;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.MouseEnter;
begin
  StopTrackTimer;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.MouseLeave;
begin
  if IsVisible then
    StartTrackTimer;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.PeekFormClosed(Sender: TObject);
begin
  StopHideTimer;
  StopShowTimer;
  StopTrackTimer;
  FPeekFormOwnerElement := nil;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.HideTimerExpired(Sender: TObject);
begin
  StopHideTimer;
  Hide;
end;

function TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.IsVisible: Boolean;
begin
  Result := (FPeekForm <> nil) and FPeekForm.Visible;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.ShowTimerExpired(Sender: TObject);
var
  AControl: TWinControl;
begin
  StopShowTimer;
  AControl := nil;
  FController.DoShowPeekForm(FPeekFormOwnerElement, AControl);
  if AControl <> nil then
  begin
    if FPeekForm = nil then
    begin
      FPeekForm := TdxNavBarOfficeNavigationBarPeekForm.Create(FController.NavigationBar);
      FPeekForm.OnClosed := PeekFormClosed;
    end;
    FPeekForm.UpdateScaleFactor;
    FPeekForm.PopupControl := AControl;
    FPeekForm.BiDiMode := FController.NavigationBar.BiDiMode;
    FPeekForm.PopupControl.BiDiMode := FPeekForm.BiDiMode;
    FPeekForm.OwnerBounds := cxRectSetOrigin(FPeekFormOwnerElement.Bounds,
      FController.NavigationBar.ClientToParent(FPeekFormOwnerElement.Bounds.TopLeft));
    FPeekForm.OwnerParent := FController.NavigationBar.Parent;
    FPeekForm.FadeAnimation := FController.NavigationBar.OptionsBehavior.Animation;
    FPeekForm.MoveAnimation := FController.NavigationBar.OptionsBehavior.Animation;
    FPeekForm.AnimationTime[atShow] := 200;
    FPeekForm.AnimationTime[atHide] := 100;
    FPeekForm.Style.LookAndFeel := FController.NavigationBar.LookAndFeel;
    FPeekForm.Style.BorderColor := clDefault;
    FPeekForm.Style.Color := clDefault;
    FPeekForm.FShowWithoutActivation := True;
    if FController.NavigationBar.OptionsView.Orientation = orHorizontal then
    begin
      FPeekForm.Direction := pdVertical;
      FPeekForm.AlignVert := pavTop;
      FPeekForm.AlignHorz := pahCenter;
    end
    else
    begin
      FPeekForm.Direction := pdHorizontal;
      FPeekForm.AlignVert := pavCenter;
      FPeekForm.AlignHorz := pahRight;
    end;
    FPeekForm.Popup(nil);
  end;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StartHideTimer;
begin
  StopShowTimer;
  if not IsVisible then
    FPeekFormOwnerElement := nil;
  FHideTimer.Enabled := True;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StartShowTimer;
begin
  FShowTimer.Enabled := True;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StartTrackTimer;
begin
  FTrackTimer.Enabled := True;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StopHideTimer;
begin
  FHideTimer.Enabled := False;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StopShowTimer;
begin
  FShowTimer.Enabled := False;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.StopTrackTimer;
begin
  FTrackTimer.Enabled := False;
end;

procedure TdxNavBarOfficeNavigationBarController.TPeekFormVisibilityController.TrackTimerExpired(
  Sender: TObject);
var
  AHandle: HWND;
begin
  StopTrackTimer;
  if IsRelatedWindow(FPeekForm, GetFocus) then
  begin
    StopHideTimer;
    Exit;
  end;

  AHandle := WindowFromPoint(GetMouseCursorPos);
  if IsRelatedWindow(FPeekForm, AHandle) then
    StopHideTimer
  else
    StartHideTimer;
  StartTrackTimer;
end;

{ TdxNavBarOfficeNavigationBarItem }

constructor TdxNavBarOfficeNavigationBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
  FVisible := True;
end;

procedure TdxNavBarOfficeNavigationBarItem.Assign(Source: TPersistent);
var
  AItem: TdxNavBarOfficeNavigationBarItem;
begin
  if Source is TdxNavBarOfficeNavigationBarItem then
  begin
    AItem := TdxNavBarOfficeNavigationBarItem(Source);
    Collection.BeginUpdate;
    try
      Text := AItem.Text;
      Visible := AItem.Visible;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

function TdxNavBarOfficeNavigationBarItem.GetID: Integer;
begin
  Result := ID;
end;

function TdxNavBarOfficeNavigationBarItem.GetImageIndex: TcxImageIndex;
begin
  Result := FImageIndex;
end;

function TdxNavBarOfficeNavigationBarItem.GetText: string;
begin
  Result := FText;
end;

procedure TdxNavBarOfficeNavigationBarItem.SetImageIndex(
  const Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TdxNavBarOfficeNavigationBarItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(True);
  end;
end;

procedure TdxNavBarOfficeNavigationBarItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

{ TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator }

constructor TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator.Create(
  ACollection: TdxNavBarOfficeNavigationBarItems);
begin
  inherited Create;
  FCollection := ACollection;
  FIndex := -1;
end;

function TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator.GetCurrent: TObject;
begin
  Result := FCollection[FIndex];
end;

function TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator.GetCurrentNavigationItem: IdxNavigationItem;
begin
  Supports(GetCurrent, IdxNavigationItem, Result);
end;

function TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator.MoveNext: Boolean;
begin
  repeat
    Inc(FIndex);
    Result := FIndex < FCollection.Count;
  until not Result or FCollection[FIndex].Visible;
end;

procedure TdxNavBarOfficeNavigationBarItems.TNavigationItemEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TdxNavBarOfficeNavigationBarItems }

constructor TdxNavBarOfficeNavigationBarItems.Create(
  ANavigationBar: TdxNavBarCustomOfficeNavigationBar;
  AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FNavigationBar := ANavigationBar;
end;

function TdxNavBarOfficeNavigationBarItems.Add: TdxNavBarOfficeNavigationBarItem;
begin
  Result := TdxNavBarOfficeNavigationBarItem(inherited Add);
end;

procedure TdxNavBarOfficeNavigationBarItems.Notify(Item: TCollectionItem; Action: Classes.TCollectionNotification);
begin
  inherited;
  case Action of
    cnExtracting:
      if Item = FSelectedItem then
        SelectedItem := nil;
  end;
end;

procedure TdxNavBarOfficeNavigationBarItems.Update(Item: TCollectionItem);
begin
  FNavigationBar.ItemsChanged;
end;

function TdxNavBarOfficeNavigationBarItems.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxNavBarOfficeNavigationBarItems._AddRef: Integer;
begin
  Result := -1;
end;

function TdxNavBarOfficeNavigationBarItems._Release: Integer;
begin
  Result := -1;
end;

function TdxNavBarOfficeNavigationBarItems.GetEnumerator: IEnumerator;
begin
  Result := GetNavigationItemEnumerator;
end;

function TdxNavBarOfficeNavigationBarItems.GetNavigationItemEnumerator: IEnumerator<IdxNavigationItem>;
begin
  Result := TNavigationItemEnumerator.Create(Self);
end;

function TdxNavBarOfficeNavigationBarItems.GetNavigationClientItems: IEnumerable<IdxNavigationItem>;
begin
  Result := Self;
end;

procedure TdxNavBarOfficeNavigationBarItems.SetNavigationClientSelectedItem(
  AItem: IdxNavigationItem);
begin
  SelectedItem := AItem as TdxNavBarOfficeNavigationBarItem;
end;

procedure TdxNavBarOfficeNavigationBarItems.SetSelectedItem(
  const Value: TdxNavBarOfficeNavigationBarItem);
begin
  if FSelectedItem <> Value then
  begin
    FSelectedItem := Value;
    FNavigationBar.SelectionChanged;
  end;
end;

function TdxNavBarOfficeNavigationBarItems.GetNavigationClientSelectedItem: IdxNavigationItem;
begin
  Supports(FSelectedItem, IdxNavigationItem, Result);
end;

procedure TdxNavBarOfficeNavigationBarItems.AddListener(
  AListener: IdxNavigationClientListener);
begin
end;

procedure TdxNavBarOfficeNavigationBarItems.RemoveListener(
  AListener: IdxNavigationClientListener);
begin
end;

function TdxNavBarOfficeNavigationBarItems.GetItem(
  Index: Integer): TdxNavBarOfficeNavigationBarItem;
begin
  Result := TdxNavBarOfficeNavigationBarItem(inherited GetItem(Index));
end;

procedure TdxNavBarOfficeNavigationBarItems.SetItem(Index: Integer;
  const Value: TdxNavBarOfficeNavigationBarItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxNavBarCustomOfficeNavigationBar }

constructor TdxNavBarCustomOfficeNavigationBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSubclasses;
  Width := 150;
  Height := 30;
  Align := alBottom;
  FAutoSize := True;
  FIsViewInfoDirty := True;
  FItems := TdxNavBarOfficeNavigationBarItems.Create(Self, TdxNavBarOfficeNavigationBarItem);
  FSortedItems := TdxFastObjectList.Create(False);
  FOrderList := TList<Integer>.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TdxNavBarCustomOfficeNavigationBar.Destroy;
begin
  EndMouseTracking(Self);
  ItemProvider := nil;
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FOrderList);
  FreeAndNil(FSortedItems);
  FreeAndNil(FItems);
  DestroySubclasses;
  inherited Destroy;
end;

procedure TdxNavBarCustomOfficeNavigationBar.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  Controller.BeginDragAndDrop;
end;

procedure TdxNavBarCustomOfficeNavigationBar.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxNavBarCustomOfficeNavigationBar.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

function TdxNavBarCustomOfficeNavigationBar.GetActiveItemProvider: IdxNavigationClient;
begin
  if ItemProvider <> nil then
    Supports(ItemProvider, IdxNavigationClient, Result)
  else
    Supports(Items, IdxNavigationClient, Result);
end;

procedure TdxNavBarCustomOfficeNavigationBar.ShowCustomizationForm;
var
  ADlg: TfrmOfficeNavigationBarCustomizationDlg;
begin
  ADlg := TfrmOfficeNavigationBarCustomizationDlg.Create(Self);
  try
    ADlg.BiDiMode := BiDiMode;
    ADlg.OrderList := FOrderList;
    if ADlg.ShowModal = mrOk then
    begin
      BeginUpdate;
      try
        FOrderList.Clear;
        FOrderList.AddRange(ADlg.OrderList);
        UpdateSortedItems;
        OptionsView.MaxVisibleItemCount := ADlg.MaxVisibleItemCount;
        OptionsView.CompactNavigation := ADlg.CompactNavigation;
      finally
        EndUpdate;
      end;
    end;
  finally
    ADlg.Free;
  end;
end;

procedure TdxNavBarCustomOfficeNavigationBar.CreateSubclasses;
begin
  FOptionsBehavior := CreateOptionsBehavior;
  FOptionsView := CreateOptionsView;
  FController := CreateController;
  FViewInfo := CreateViewInfo;
end;

procedure TdxNavBarCustomOfficeNavigationBar.DestroySubclasses;
begin
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  FreeAndNil(FOptionsView);
  FreeAndNil(FCustomizationButtonPopup);
end;

procedure TdxNavBarCustomOfficeNavigationBar.CustomizationButtonClick;
begin
  if FCustomizationButtonPopup = nil then
    FCustomizationButtonPopup := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(nil);
  FCustomizationButtonPopup.Clear;
  FCustomizationButtonPopup.SetLookAndFeel(LookAndFeel);
  FCustomizationButtonPopup.Add(cxGetResourceString(@sdxOfficeNavigationBarNavigationOptionsMenuItem), DoCustomizationButtonClick);
  FCustomizationButtonPopup.BiDiMode := BiDiMode;
  FCustomizationButtonPopup.Popup(GetMouseCursorPos);
end;

procedure TdxNavBarCustomOfficeNavigationBar.Changed;
begin
  if IsLocked then
  begin
    FIsSizeCalculated := False;
    FIsViewInfoDirty := True;
    Exit;
  end;
  Inc(FChangedCounter);
  ViewInfo.Initialize;
  ViewInfo.CalculateRequiredSize(cxSize(MaxInt, MaxInt));
  FIsSizeCalculated := True;
  if IsAutoHeight then
    Height := ViewInfo.RequiredSize.cy;
  if IsAutoWidth then
    Width := ViewInfo.RequiredSize.cx;
  ViewInfo.CalculateBounds;
  Dec(FChangedCounter);
  if FChangedCounter = 0 then
    ViewInfo.CheckBiDiMode;
  FIsViewInfoDirty := False;
  Invalidate;
end;

procedure TdxNavBarCustomOfficeNavigationBar.CheckChanges;
begin
  if FIsViewInfoDirty then
    Changed;
end;

procedure TdxNavBarCustomOfficeNavigationBar.DoShowPeekForm(AItem: IdxNavigationItem; var AControl: TWinControl);
begin
  if Assigned(FOnQueryPeekFormContent) then
    FOnQueryPeekFormContent(Self, AItem, AControl);
end;

function TdxNavBarCustomOfficeNavigationBar.IsAutoHeight: Boolean;
begin
  Result := AutoSize and (FOptionsView.Orientation = orHorizontal) and
    not (Align in [alLeft, alRight, alClient, alCustom]);
end;

function TdxNavBarCustomOfficeNavigationBar.IsAutoWidth: Boolean;
begin
  Result := AutoSize and (FOptionsView.Orientation = orVertical) and
    not (Align in [alTop, alBottom, alClient, alCustom]);
end;

function TdxNavBarCustomOfficeNavigationBar.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading or IsDestroying or not HandleAllocated;
end;

procedure TdxNavBarCustomOfficeNavigationBar.ItemClick(AItem: TdxNavBarOfficeNavigationBarItemViewInfo);
var
  AAllowChange: Boolean;
begin
  AAllowChange := True;
  if Assigned(FOnSelectionChanging) then
    FOnSelectionChanging(Self, AItem.Item, AAllowChange);
  if AAllowChange then
    GetActiveItemProvider.SetSelectedItem(AItem.Item);
end;

procedure TdxNavBarCustomOfficeNavigationBar.UpdateSortedItems;
var
  AItem: IdxNavigationItem;
  AItemObject: TObject;
  ADictionary: TDictionary<Integer, TObject>;
  I: Integer;
begin
  FSortedItems.Clear;
  if GetActiveItemProvider <> nil then
    if FOrderList.Count > 0 then
    begin
      ADictionary := TDictionary<Integer, TObject>.Create;
      try
        for AItem in GetActiveItemProvider.GetItems do
          ADictionary.Add(AItem.ID, AItem as TObject);
        for I := FOrderList.Count - 1 downto 0 do
          if not ADictionary.TryGetValue(FOrderList[I], AItemObject) then
            FOrderList.Delete(I)
          else
          begin
            FSortedItems.Insert(0, AItemObject);
            ADictionary.Remove(FOrderList[I]);
          end;
        for AItemObject in ADictionary.Values do
          FSortedItems.Add(AItemObject);
      finally
        ADictionary.Free;
      end;
    end
    else
      for AItem in GetActiveItemProvider.GetItems do
        FSortedItems.Add(AItem as TObject);
end;

function TdxNavBarCustomOfficeNavigationBar.CreateController: TdxNavBarOfficeNavigationBarController;
begin
  Result := TdxNavBarOfficeNavigationBarController.Create(Self);
end;

function TdxNavBarCustomOfficeNavigationBar.CreateViewInfo: TdxNavBarOfficeNavigationBarViewInfo;
begin
  Result := TdxNavBarOfficeNavigationBarViewInfo.Create(Self);
end;

function TdxNavBarCustomOfficeNavigationBar.CreateOptionsBehavior: TdxNavBarOfficeNavigationBarOptionsBehavior;
begin
  Result := TdxNavBarOfficeNavigationBarOptionsBehavior.Create(Self);
end;

function TdxNavBarCustomOfficeNavigationBar.CreateOptionsView: TdxNavBarOfficeNavigationBarOptionsView;
begin
  Result := TdxNavBarOfficeNavigationBarOptionsView.Create(Self);
end;

procedure TdxNavBarCustomOfficeNavigationBar.BoundsChanged;
begin
  Changed;
end;

function TdxNavBarCustomOfficeNavigationBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if not FIsSizeCalculated then
    raise EdxException.Create('CanResize: FIsSizeCalculated is False');
  Result := inherited CanResize(NewWidth, NewHeight);
  if IsAutoWidth then
    NewWidth := ViewInfo.RequiredSize.cx;
  if IsAutoHeight then
    NewHeight := ViewInfo.RequiredSize.cy;
end;

procedure TdxNavBarCustomOfficeNavigationBar.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScaleEx(M, D, isDpiChange);
  OptionsView.ChangeScale(M, D);
end;

procedure TdxNavBarCustomOfficeNavigationBar.DoPaint;
begin
  FViewInfo.Paint(Canvas);
end;

procedure TdxNavBarCustomOfficeNavigationBar.FontChanged;
begin
  inherited;
  Changed;
end;

procedure TdxNavBarCustomOfficeNavigationBar.InitControl;
begin
  inherited;
  CheckChanges;
end;

function TdxNavBarCustomOfficeNavigationBar.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarCustomOfficeNavigationBar.Loaded;
begin
  inherited Loaded;
  ItemProvider := FLoadedItemProvider;
  CheckChanges;
end;

procedure TdxNavBarCustomOfficeNavigationBar.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  Changed;
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FController.MouseDown(Button, Shift, X, Y);
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  BeginMouseTracking(Self, GetControlRect(Self), Self);
  FController.MouseEnter;
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  FController.MouseLeave;
  EndMouseTracking(Self);
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  BeginMouseTracking(Self, GetControlRect(Self), Self);
  FController.MouseMove(Shift, X, Y);
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AWasDragging: Boolean;
begin
  AWasDragging := DragAndDropState = ddsInProcess;
  inherited MouseUp(Button, Shift, X, Y);
  FController.MouseUp(Button, Shift, X, Y);
  if not AWasDragging and PtInRect(ClientRect, Point(X, Y)) then
    Click;
end;

function TdxNavBarCustomOfficeNavigationBar.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxNavBarCustomOfficeNavigationBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FItemProvider then
      ItemProvider := nil;
    if AComponent = FImages then
      Images := nil;
  end;
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TdxNavBarCustomOfficeNavigationBar.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  Controller.DragAndDrop(P, Accepted);
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxNavBarCustomOfficeNavigationBar.HidePeekForm;
begin
  Controller.HidePeekForm;
end;

procedure TdxNavBarCustomOfficeNavigationBar.EndDragAndDrop(Accepted: Boolean);
var
  I: Integer;
begin
  inherited EndDragAndDrop(Accepted);
  Controller.EndDragAndDrop(Accepted);
  Invalidate;
  if Accepted then
  begin
    if FOrderList.Count = 0 then
      for I := 0 to SortedItemCount - 1 do
        FOrderList.Add(SortedItems[I].ID);

    for I := 0 to ViewInfo.FItems.FVisibleElements.Count - 1 do
      FOrderList[I] := (ViewInfo.FItems.FVisibleElements[I] as TdxNavBarOfficeNavigationBarItemViewInfo).Item.ID;

    UpdateSortedItems;
  end
  else
    Changed;
end;

function TdxNavBarCustomOfficeNavigationBar.StartDragAndDrop(
  const P: TPoint): Boolean;
begin
  Result := Controller.StartDragAndDrop(P);
end;

procedure TdxNavBarCustomOfficeNavigationBar.MouseTrackingMouseLeave;
begin
  FController.MouseLeave;
  EndMouseTracking(Self);
end;

function TdxNavBarCustomOfficeNavigationBar.PtInCaller(
  const P: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, P);
end;

function TdxNavBarCustomOfficeNavigationBar.IsCaptureMouse: Boolean;
begin
  Result := MouseCapture;
end;

procedure TdxNavBarCustomOfficeNavigationBar.ItemsChanged;
begin
  UpdateSortedItems;
  Changed;
end;

procedure TdxNavBarCustomOfficeNavigationBar.SelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
  Invalidate;
end;

procedure TdxNavBarCustomOfficeNavigationBar.DoCustomizationButtonClick(
  Sender: TObject);
begin
  ShowCustomizationForm;
end;

function TdxNavBarCustomOfficeNavigationBar.GetSortedItems(
  Index: Integer): IdxNavigationItem;
begin
  Supports(FSortedItems[Index], IdxNavigationItem, Result);
end;

function TdxNavBarCustomOfficeNavigationBar.GetSortedItemCount: Integer;
begin
  Result := FSortedItems.Count;
end;

procedure TdxNavBarCustomOfficeNavigationBar.ImageListChange(Sender: TObject);
begin
  Changed;
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetImages(
  const Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetItemProvider(
  Value: TComponent);
begin
  if IsLoading then
    FLoadedItemProvider := Value
  else
  begin
    if not Supports(Value, IdxNavigationClient) then
      Value := nil;
    if FItemProvider <> Value then
    begin
      FOrderList.Clear;
      if FItemProvider <> nil then
      begin
        GetActiveItemProvider.RemoveListener(Self);
        FItemProvider.RemoveFreeNotification(Self);
      end;
      FItemProvider := Value;
      if FItemProvider <> nil then
      begin
        FItemProvider.FreeNotification(Self);
        GetActiveItemProvider.AddListener(Self);
      end;
      ItemsChanged;
    end;
  end;
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetItems(
  const Value: TdxNavBarOfficeNavigationBarItems);
begin
  FItems.Assign(Value);
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetOptionsBehavior(
  const Value: TdxNavBarOfficeNavigationBarOptionsBehavior);
begin
  FOptionsBehavior.Assign(Value);
end;

procedure TdxNavBarCustomOfficeNavigationBar.SetOptionsView(
  const Value: TdxNavBarOfficeNavigationBarOptionsView);
begin
  FOptionsView.Assign(Value);
end;

procedure TdxNavBarCustomOfficeNavigationBar.WMLButtonUp(
  var Message: TWMLButtonUp);
begin
  ControlState := ControlState - [csClicked];
  inherited;
end;

end.

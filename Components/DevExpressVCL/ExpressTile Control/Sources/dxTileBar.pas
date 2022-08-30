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

unit dxTileBar;

{$I cxVer.inc}
interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Forms, SysUtils, Classes, Controls, StdCtrls, Graphics, Math, ImgList, Dialogs, Types, Menus,
  dxCoreClasses, dxCore, dxCoreGraphics, cxGeometry, cxClasses, cxGraphics, cxControls, cxLookAndFeels, dxSkinInfo,
  cxLookAndFeelPainters, dxGDIPlusClasses, dxGDIPlusAPI, cxInplaceContainer, cxStorage, dxSkinsCore, cxDrawTextUtils,
  cxLibraryConsts, dxHooks, dxAnimation, cxButtonEdit, cxEdit, cxEditConsts, dxMessages, cxContainer,
  dxCustomTileControl;

const
  dxTileBarDefaultGroupIndent = 10;
  dxTileBarDefaultIndentHorz = 24;
  dxTileBarDefaultIndentVert = 14;
  dxTileBarDefaultItemIndent = 10;
  dxTileBarDefaultItemHeight = 60;
  dxTileBarDefaultItemWidth = 80;

  dxTileBarPopupWindowHideTime = 100;
  dxTileBarPopupWindowShowTime = 200;

  dxTileBarItemHighlightMaskColor = $4FFFFFFF;

  tbhtDropDownButton = 101;

type
  TdxCustomTileBar = class;
  TdxTileBarController = class;
  TdxTileBarHitTest = class;
  TdxTileBarViewInfo = class;
  TdxTileBarItem = class;
  TdxTileBarOptionsView = class;
  TdxTileBarPopupWindow = class;
  TdxTileBarDetailSite = class;
  TdxTileBarItemDetailOptions = class;

  TdxTileBarItemSize = (tbisRegular, tbisLarge);
  TdxTileBarPosition = (tbpTop, tbpBottom, tbpLeft, tbpRight);

  { TdxTileBarDetailSiteHitTest }

  TdxTileBarDetailSiteHitTest = class(TdxTileControlHitTest)
  private
    FOwner: TdxTileBarDetailSite;
  protected
    function GetActiveViewInfo: TdxTileControlCustomViewInfo; override;
  public
    constructor Create(AOwner: TdxTileBarDetailSite);

    property Owner: TdxTileBarDetailSite read FOwner;
  end;

  { TdxTileBarDetailSiteTitleViewInfo }

  TdxTileBarDetailSiteTitleViewInfo = class(TdxTileControlDetailSiteTitleViewInfo)
  private
    function GetOptions: TdxTileBarItemDetailOptions;
  protected
    procedure DoCalculate; override;
    function GetHeight: Integer; override;
  public
    property Options: TdxTileBarItemDetailOptions read GetOptions;
  end;

  { TdxTileBarDetailSite }

  TdxTileBarDetailSite = class(TdxTileControlDetailSite)
  private
    FHitTest: TdxTileControlHitTest;
    function GetBarItem: TdxTileBarItem;
    function GetOptions: TdxTileBarItemDetailOptions;
    function GetTileBar: TdxCustomTileBar;
  protected
    function GetHitTest: TdxTileControlHitTest; override;
    function GetTitleViewInfoClass: TdxTileControlDetailSiteTitleViewInfoClass; override;
    procedure InitializeSite(ALeft, ATop: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Options: TdxTileBarItemDetailOptions read GetOptions;
    property BarItem: TdxTileBarItem read GetBarItem;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarItemDropDownButtonViewInfo }

  TdxTileBarItemDropDownButtonViewInfo = class(TdxTileControlCustomCellViewInfo,
    IcxMouseTrackingCaller, IcxMouseTrackingCaller2)
  private
    FItem: TdxTileBarItem;
    FState: TcxButtonState;

    function GetTileBar: TdxCustomTileBar;
    procedure SetState(AValue: TcxButtonState);
  protected
    procedure Click;
    procedure DoCalculate; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetVisibleBounds: TRect; override;

    // IcxMouseTrackingCaller
    procedure MouseLeave; virtual;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
  public
    constructor Create(AItem: TdxTileBarItem); virtual;
    destructor Destroy; override;

    property Item: TdxTileBarItem read FItem;
    property State: TcxButtonState read FState write SetState;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarItemViewInfo }

  TdxTileBarItemViewInfo = class(TdxTileControlItemViewInfo)
  private
    FDropDownButton: TdxTileBarItemDropDownButtonViewInfo;
    function GetItem: TdxTileBarItem;
  protected
    function CanFocusInDesigning: Boolean; override;
    procedure DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect); override;
    function GetHighlightBounds(const ABounds: TRect): TRect; override;
    function GetHitTest(AHitTest: TdxTileControlHitTest): Boolean; override;
    procedure Scroll(const DX, DY: Integer); override;
    procedure SetInflateDelta(AValue: Integer); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    constructor Create(AOwner: TdxTileControlItem); override;
    destructor Destroy; override;

    property DropDownButton: TdxTileBarItemDropDownButtonViewInfo read FDropDownButton;
    property Item: TdxTileBarItem read GetItem;
  end;

  { TdxTileBarItemDetailOptions }

  TdxTileBarItemDetailOptions = class(TdxTileControlItemDetailOptions)
  private
    function GetBarItem: TdxTileBarItem;
    function GetTileBar: TdxCustomTileBar;
  protected
    function GetDetailSiteHeight: Integer; override;
    function GetDetailSiteWidth: Integer; override;
    procedure SetDetailControl(AValue: TWinControl); override;
  public
    property BarItem: TdxTileBarItem read GetBarItem;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarItemPopupOptions }

  TdxTileBarSavedControlData = record
    Align: TAlign;
    Anchors: TAnchors;
    BorderStyle: TFormBorderStyle;
    BoundsRect: TRect;
    Parent: TWinControl;
    Visible: Boolean;
  end;

  TdxTileBarItemPopupOptions = class(TcxOwnedPersistent)
  private
    FBorderColor: TColor;
    FBorderHeight: Integer;
    FBorderWidth: Integer;
    FColor: TColor;
    FPopupControl: TWinControl;
    FPopupControlNotifyComponent: TcxFreeNotificator;
    FSavedControlData: TdxTileBarSavedControlData;

    function GetBarItem: TdxTileBarItem;
    function GetTileBar: TdxCustomTileBar;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderHeight(AValue: Integer);
    procedure SetBorderWidth(AValue: Integer);
    procedure SetColor(AValue: TColor);
    procedure SetPopupControl(AControl: TWinControl);
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure PopupControlFreeNotification(AComponent: TComponent);
    function IsChanged: Boolean;
    procedure RestoreControlData(AControl: TWinControl); virtual;
    procedure SaveControlData(AControl: TWinControl); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;

    property BarItem: TdxTileBarItem read GetBarItem;
    property TileBar: TdxCustomTileBar read GetTileBar;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderHeight: Integer read FBorderHeight write SetBorderHeight default 1;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Color: TColor read FColor write SetColor default clDefault;
    property PopupControl: TWinControl read FPopupControl write SetPopupControl;
  end;

 { TdxTileBarItem }

  TdxTileBarItem = class(TdxTileControlItem)
  private
    FPopupOptions: TdxTileBarItemPopupOptions;
    FSize: TdxTileBarItemSize;

    function GetDetailOptions: TdxTileBarItemDetailOptions;
    function GetTileBar: TdxCustomTileBar;
    function GetViewInfo: TdxTileBarItemViewInfo;
    procedure SetPopupOptions(AValue: TdxTileBarItemPopupOptions);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function CreateDetailSite: TdxTileControlDetailSite; override;
    function CreateViewInfo: TdxTileControlItemViewInfo; override;
    function GetDetailOptionsClass: TdxTileControlItemDetailOptionsClass; override;
    function IsPopupControlAssigned: Boolean;
    function IsPopupOptionsStored: Boolean;
    function IsNeedDropDownButton: Boolean;
    procedure SetDetailOptions(AValue: TdxTileBarItemDetailOptions); reintroduce; virtual;
    procedure SetSize(AValue: TdxTileBarItemSize); reintroduce;

    property ViewInfo: TdxTileBarItemViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TileBar: TdxCustomTileBar read GetTileBar;
  published
    property DetailOptions: TdxTileBarItemDetailOptions read GetDetailOptions write SetDetailOptions stored IsDetailOptionsStored;
    property PopupOptions: TdxTileBarItemPopupOptions read FPopupOptions write SetPopupOptions stored IsPopupOptionsStored;
    property Size: TdxTileBarItemSize read FSize write SetSize;
  end;

  { TdxTileBarScrollButtonViewInfo }

  TdxTileBarScrollButtonViewInfo = class(TdxTileControlScrollButtonViewInfo)
  private
    function GetOptionsView: TdxTileBarOptionsView;
    function GetViewInfo: TdxTileBarViewInfo;
  protected
    procedure DoCalculateTopLeft(const ASizeX, ASizeY: Integer; var ATop, ALeft: Integer); override;

    property OptionsView: TdxTileBarOptionsView read GetOptionsView;
    property ViewInfo: TdxTileBarViewInfo read GetViewInfo;
  end;

  { TdxTileBarViewInfo }

  TdxTileBarViewInfo = class(TdxTileControlViewInfo)
  private
    function GetOptionsView: TdxTileBarOptionsView;
    function GetTileBar: TdxCustomTileBar;
  protected
    procedure AddItem(AItem: TdxTileControlItem); override;
    function CalculateTilesZone: TRect; override;
    function GetDetailSiteArea: TRect; override;
    function GetItemsMaxHeight: Integer;
    function GetScrollButtonViewInfoClass: TdxTileControlScrollButtonViewInfoClass; override;

    property OptionsView: TdxTileBarOptionsView read GetOptionsView;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarPainter }

  TdxTileBarPainter = class(TdxTileControlPainter)
  private
    function GetTileBar: TdxCustomTileBar;
  protected
    procedure DrawDefaultDropDownButtonArrow(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; const ABorderColor: TColor); overload; virtual;
    procedure DrawDropDownButton(ACanvas: TcxCanvas; const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxButtonState;
      const AHighlightColor: TdxAlphaColor; AItem: TdxTileControlCustomItem); virtual;
  public
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarOptionsBehavior }

  TdxTileBarOptionsBehavior = class(TdxTileControlOptionsBehavior)
  protected
    function GetDefaultItemCheckMode: TdxTileControlItemCheckMode; override;
    function GetDefaultItemFocusMode: TdxTileControlItemFocusMode; override;
    function GetDefaultItemHotTrackHighlightColor: TdxAlphaColor; override;
    function GetDefaultItemHotTrackMode: TdxTileControlItemHotTrackMode; override;
    function GetDefaultItemMoving: Boolean; override;
    function GetDefaultItemPressAnimation: Boolean; override;
    function GetDefaultScrollMode: TdxTileControlScrollMode; override;
  end;

  { TdxTileBarOptionsView }

  TdxTileBarOptionsView = class(TdxTileControlOptionsView)
  private
    FPosition: TdxTileBarPosition;

    function GetActualPosition: TdxTileBarPosition;
    function GetTileBar: TdxCustomTileBar;
    procedure SetPosition(AValue: TdxTileBarPosition);
  protected
    function GetDefaultFixedIndentHorz: Boolean; override;
    function GetDefaultFixedIndentVert: Boolean; override;
    function GetDefaultGroupBlockMaxColumnCount: Integer; override;
    function GetDefaultGroupIndent: Integer; override;
    function GetDefaultGroupMaxRowCount: Integer; override;
    function GetDefaultIndentHorz: Integer; override;
    function GetDefaultIndentVert: Integer; override;
    function GetDefaultItemHeight: Integer; override;
    function GetDefaultItemIndent: Integer; override;
    function GetDefaultItemWidth: Integer; override;
    function GetDropDownButtonWidth: Integer; virtual;

    function IsAllowableWidth(const AWidth: Integer): Boolean; override;

    class function CheckGroupLayout(const APosition: TdxTileBarPosition): TdxTileControlGroupLayout;
    procedure DoItemChanged(AItem: TdxTileControlItem; const AData: Pointer);
    class function GetGroupMaxRowCount(const ALayout: TdxTileControlGroupLayout): Integer; overload;
    class function GetGroupMaxRowCount(const APosition: TdxTileBarPosition): Integer; overload;
    procedure SetGroupLayout(AValue: TdxTileControlGroupLayout); override;

    property DropDownButtonWidth: Integer read GetDropDownButtonWidth;
    property ActualPosition: TdxTileBarPosition read GetActualPosition;
    property TileBar: TdxCustomTileBar read GetTileBar;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Position: TdxTileBarPosition read FPosition write SetPosition default tbpTop;
  end;

  { TdxTileBarPopupWindowViewInfo }

  TdxTileBarPopupWindowViewInfo = class
  private
    FBounds: TRect;
    FCallOutElementSize: TSize;
    FCallOutPoints1: array [0..3] of TPoint;
    FCallOutPoints2: array [0..3] of TPoint;
    FClientBounds: TRect;
    FPopupControlBounds: TRect;
    FOwner: TdxTileBarPopupWindow;
    FOutTilesArea1: TRect;
    FOutTilesArea2: TRect;
    FRequiredSize: TSize;

    function GetActualControl: TWinControl;
    function GetBorderColor: TColor;
    function GetBorderHeight: Integer;
    function GetBorderWidth: Integer;
    function GetBorders: TRect;
    function GetColor: TColor;
    function GetTileBar: TdxCustomTileBar;
  protected
    procedure CheckCallOutElementSize;
    function CreateRegion: HRGN;

    property ActualControl: TWinControl read GetActualControl;
    property BorderColor: TColor read GetBorderColor;
    property BorderHeight: Integer read GetBorderHeight;
    property BorderWidth: Integer read GetBorderWidth;
    property Borders: TRect read GetBorders;
    property Color: TColor read GetColor;
    property TileBar: TdxCustomTileBar read GetTileBar;
  public
    constructor Create(AOwner: TdxTileBarPopupWindow); virtual;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateCallOutPoints;
    procedure CalculateOutTilesRects;
    function CalculatePosition: TPoint;
    procedure CalculateRequiredSize;
    procedure Paint(ACanvas: TcxCanvas); virtual;

    property Bounds: TRect read FBounds;
    property PopupControlBounds: TRect read FPopupControlBounds;
    property RequiredSize: TSize read FRequiredSize;
  end;

  { TdxTileBarPopupWindow }

  TdxTileBarPopupWindow = class(TcxCustomPopupWindow)
  private
    FActualBarItem: TdxTileBarItem;
    FCallOutPos: Integer;
    FLastActualBarItem: TdxTileBarItem;
    FActualControlRealSize: TSize;
    FViewInfo: TdxTileBarPopupWindowViewInfo;

    procedure CMShowPopupWindow(var Message: TMessage); message DXM_SHOWPOPUPWINDOW;
    function GetActualControl: TWinControl;
    function GetTileBar: TdxCustomTileBar;
    procedure SetActualBarItem(AItem: TdxTileBarItem);
  protected
    function CalculatePosition(const ASize: TSize): TPoint; override;
    function CalculateSize: TSize; override;
    procedure DoShowed; override;
    procedure DoShowing; override;
    function GetPopupControlSize: TSize;
    function HasBorder: Boolean;
    procedure InitPopup; override;
    function InternalBorderColor: TColor; virtual;
    function InternalBorderHeight: Integer; virtual;
    function InternalBorderWidth: Integer; virtual;
    function InternalColor: TColor; virtual;
    procedure Paint; override;
    procedure SetActualControlVisibility(AVisible: Boolean);
    procedure SetLastActualBarItem(AItem: TdxTileBarItem);
    procedure StoreActualControlSize;
    procedure RestoreActualControlSize;

    property ActualControl: TWinControl read GetActualControl;
    property ActualBarItem: TdxTileBarItem read FActualBarItem write SetActualBarItem;
    property BorderColor: TColor read InternalBorderColor;
    property BorderHeight: Integer read InternalBorderHeight;
    property BorderWidth: Integer read InternalBorderWidth;
    property CallOutPos: Integer read FCallOutPos;
    property Color: TColor read InternalColor;
    property TileBar: TdxCustomTileBar read GetTileBar;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;
    procedure CloseUp; override;
    procedure CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize); override;
  end;

  { TdxTileBarPopupWindowAnimation }

  TdxTileBarPopupWindowAnimation = class(TdxAnimationTransition)
  private
    FTileBar: TdxCustomTileBar;
    FIsShowing: Boolean;

    function GetPopupWindow: TdxTileBarPopupWindow;
  protected
    procedure DoAnimate; override;
  public
    constructor Create(ATileBar: TdxCustomTileBar; AIsShowing: Boolean); reintroduce; virtual;

    property IsShowing: Boolean read FIsShowing;
    property PopupWindow: TdxTileBarPopupWindow read GetPopupWindow;
    property TileBar: TdxCustomTileBar read FTileBar;
  end;

  { TdxTileBarHitTest }

  TdxTileBarHitTest = class(TdxTileControlHitTest)
  private
    function GetTileBar: TdxCustomTileBar;
  protected
    function GetActiveViewInfo: TdxTileControlCustomViewInfo; override;
    function GetDropDownButton: TdxTileBarItemDropDownButtonViewInfo;
  public
    property DropDownButton: TdxTileBarItemDropDownButtonViewInfo read GetDropDownButton;
    property HitAtDropDownButton: Boolean index tbhtDropDownButton read GetBitState;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarController }

  TdxTileBarController = class(TdxTileControlController)
  private
    FPressedDropDownButton: TdxTileBarItemDropDownButtonViewInfo;
    function GetHitTest: TdxTileBarHitTest;
    function GetTileBar: TdxCustomTileBar;
  protected
    function CanPopupByKey(const Key: Word): Boolean;
    function CanShowActionBarsOnRightClick: Boolean; override;
    function HitAtDropDownButton: Boolean;
    procedure InternalMouseLeftDown(Shift: TShiftState; X, Y: Integer); override;
    procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetFocusedItem(AValue: TdxTileControlItem); override;

    // design selection
    function CreateDesignHelper: TdxTileControlCustomDesignHelper; override;
    function IsDesignHelperClassInitialized: Boolean; override;
    // design popup menu
    function CreateRegularItem(AGroup: TdxTileControlGroup): TdxTileControlItem; override;
    function IsDesignerMenuItemChecked(const AItemSizeIndex: Integer; AItem: TdxTileControlItem): Boolean; override;
    function GetIsLargeItem(AItem: TdxTileControlItem): Boolean; override;
    function GetIsRegularItem(AItem: TdxTileControlItem): Boolean; override;
    procedure PopulateOneGroupDesignPopupMenu; override;
    procedure PopulateOneItemDesignPopupMenu; override;
    procedure SetDesignItemSize(AItem: TdxTileControlItem; AMenuItemTag: Longint); override;

    property HitTest: TdxTileBarHitTest read GetHitTest;
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxCustomTileBar }

  TdxCustomTileBar = class(TdxCustomTileControl)
  private
    FDeactivatingDetailInProcess: Boolean;
    FPopupWindow: TdxTileBarPopupWindow;
    FQueuePrevDetails: TcxObjectList;
    FOnPopupActivating: TdxTileControlItemAllowOperationEvent;
    FOnPopupActivate: TdxTileControlItemOperationEvent;
    FOnPopupDeactivating: TdxTileControlItemAllowOperationEvent;
    FOnPopupDeactivate: TdxTileControlItemOperationEvent;

    function GetController: TdxTileBarController;
    function GetPopupWindow: TdxTileBarPopupWindow;
    function GetFocusedItem: TdxTileBarItem;
    function GetHitTest: TdxTileBarHitTest;
    function GetGroupLayout: TdxTileControlGroupLayout;
    function GetOptionsBehavior: TdxTileBarOptionsBehavior;
    function GetOptionsView: TdxTileBarOptionsView;
    procedure SetFocusedItem(AItem: TdxTileBarItem);
  protected
    function ActiveDetailOccupiesAllTileControl: Boolean; override;
    procedure AfterActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite); override;
    procedure CheckChanges; override;
    procedure CheckDesignTimeAlign;
    procedure CheckDesignTimeSize;
    procedure CheckDetailLayout(ADetail: TdxTileControlDetailSite); override;
    function CreateActionBarTop: TdxTileControlCustomActionBar; override;
    function CreateController: TdxTileControlController; override;
    function CreatePopupWindow: TdxTileBarPopupWindow; virtual;
    function CreateHitTest: TdxTileControlHitTest; override;
    function CreatePainter: TdxTileControlPainter; override;
    function CreateViewInfo: TdxTileControlViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoActivateDetail(ADetail: TdxTileControlDetailSite); override;
    procedure DoCustomPaint; override;
    procedure DoLoaded; override;
    procedure ExecuteDetailDeactivating(ADetail: TdxTileControlDetailSite); override;
    procedure ExpandQueuePrevDetails; virtual;
    function GetActiveHitTest: TdxTileControlHitTest; override;
    function GetAnimationScrollFadeMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode; override;
    function GetAnimationScrollMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode; override;
    function GetHScrollBarBounds: TRect; override;
    function GetOptionsBehaviorClass: TdxTileControlOptionsBehaviorClass; override;
    function GetOptionsViewClass: TdxTileControlOptionsViewClass; override;
    function GetVScrollBarBounds: TRect; override;
    procedure InitializeAlign; override;
    procedure InvalidateChanges; override;
    function NeedStopAnimationsBeforeActiveDetailChanging(AActiveDetail: TdxTileControlDetailSite): Boolean; override;

    procedure SetOptionsBehavior(AValue: TdxTileBarOptionsBehavior); reintroduce; virtual;
    procedure SetOptionsView(AValue: TdxTileBarOptionsView); reintroduce; virtual;

    property PopupWindow: TdxTileBarPopupWindow read GetPopupWindow;
    property GroupLayout: TdxTileControlGroupLayout read GetGroupLayout;
    property HitTest: TdxTileBarHitTest read GetHitTest;
    property QueuePrevDetails: TcxObjectList read FQueuePrevDetails;
    property OnPopupActivating: TdxTileControlItemAllowOperationEvent read FOnPopupActivating write FOnPopupActivating;
    property OnPopupActivate: TdxTileControlItemOperationEvent read FOnPopupActivate write FOnPopupActivate;
    property OnPopupDeactivating: TdxTileControlItemAllowOperationEvent read FOnPopupDeactivating write FOnPopupDeactivating;
    property OnPopupDeactivate: TdxTileControlItemOperationEvent read FOnPopupDeactivate write FOnPopupDeactivate;
  public
    function CreateItem(ASize: TdxTileBarItemSize; AGroup: TdxTileControlGroup = nil): TdxTileBarItem; reintroduce;
    function CreateItemsCollection: TdxTileControlItemCollection; override;

    procedure HidePopupWindow;
    procedure ShowPopupWindow(AItem: TdxTileBarItem);

    property Controller: TdxTileBarController read GetController;
    property FocusedItem: TdxTileBarItem read GetFocusedItem write SetFocusedItem;
    property OptionsBehavior: TdxTileBarOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsView: TdxTileBarOptionsView read GetOptionsView write SetOptionsView;
  end;

  { TdxTileControlTopActionBar }

  TdxTileBarTopActionBar = class(TdxTileControlCustomActionBar)
  protected
    function CreateViewInfo: TdxTileControlCustomActionBarViewInfo; override;
  end;

  { TdxTileBarActionBarViewInfo }

  TdxTileBarActionBarViewInfo = class(TdxTileControlCustomActionBarViewInfo)
  protected
    function GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean; override;
  end;

  TdxTileBar = class(TdxCustomTileBar)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle default cxcbsNone;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusedItem;
    property Groups;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property Items;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsDetailAnimate;
    property OptionsItemAnimate;
    property OptionsView;
    property ParentBiDiMode;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Transparent;
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
    property OnPopupActivating;
    property OnPopupActivate;
    property OnPopupDeactivating;
    property OnPopupDeactivate;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetStoredProperties;
    property OnGetStoredPropertyValue;
    property OnGroupDragBegin;
    property OnGroupDragEnd;
    property OnGroupDragOver;
    property OnInitStoredObject;
    property OnItemActivateDetail;
    property OnItemBeforeCheck;
    property OnItemCheck;
    property OnItemDeactivateDetail;
    property OnItemDeactivatingDetail;
    property OnItemDragBegin;
    property OnItemDragEnd;
    property OnItemDragOver;
    property OnItemFocusChange;
    property OnItemFocusChanging;
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
    property OnSetStoredPropertyValue;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

var
  dxTileBarDesignHelperClass: TdxTileControlCustomDesignHelperClass = nil;

implementation

{$R dxTileBar.res}

const
  dxTileBarDefaultCallOutWidth = 14;
  dxTileBarDefaultCallOutHeight = 7;
  dxTileBarItemDropDownButtonWidth = 25;

  dxSignature: array[Boolean] of Integer = (-1, 1);

type
  TdxTileControlAccess = class(TdxCustomTileControl);
  TdxTileControlControllerAccess = class(TdxTileControlController);
  TdxTileControlCustomDetailSiteAccess = class(TdxTileControlDetailSite);
  TdxTileControlCustomStyleAccess = class(TdxTileControlCustomStyle);
  TdxTileControlGroupAccess = class(TdxTileControlGroup);
  TdxTileControlGroupCaptionAccess = class(TdxTileControlGroupCaption);
  TdxTileControlHitTestAccess = class(TdxTileControlHitTest);
  TdxTileControlItemCollectionAccess = class(TdxTileControlItemCollection);
  TdxTileControlTitleAccess = class(TdxTileControlTitle);

{ TdxTileBarDetailSiteHitTest }

constructor TdxTileBarDetailSiteHitTest.Create(AOwner: TdxTileBarDetailSite);
begin
  inherited Create(AOwner.TileBar);
  FOwner := AOwner;
end;

function TdxTileBarDetailSiteHitTest.GetActiveViewInfo: TdxTileControlCustomViewInfo;
begin
  Result := Owner.TitleViewInfo;
end;

{ TdxTileBarDetailSiteTitleViewInfo }

procedure TdxTileBarDetailSiteTitleViewInfo.DoCalculate;
begin
end;

function TdxTileBarDetailSiteTitleViewInfo.GetHeight: Integer;
begin
  Result := 0;
end;

function TdxTileBarDetailSiteTitleViewInfo.GetOptions: TdxTileBarItemDetailOptions;
begin
  Result := (Owner as TdxTileBarDetailSite).Options;
end;

{ TdxTileBarDetailSite }

constructor TdxTileBarDetailSite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHitTest := TdxTileBarDetailSiteHitTest.Create(Self);
end;

destructor TdxTileBarDetailSite.Destroy;
begin
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

function TdxTileBarDetailSite.GetBarItem: TdxTileBarItem;
begin
  Result := GetOwner as TdxTileBarItem;
end;

function TdxTileBarDetailSite.GetHitTest: TdxTileControlHitTest;
begin
  Result := FHitTest;
end;

function TdxTileBarDetailSite.GetOptions: TdxTileBarItemDetailOptions;
begin
  Result := BarItem.DetailOptions;
end;

function TdxTileBarDetailSite.GetTileBar: TdxCustomTileBar;
begin
   Result := inherited TileControl as TdxCustomTileBar;
end;

function TdxTileBarDetailSite.GetTitleViewInfoClass: TdxTileControlDetailSiteTitleViewInfoClass;
begin
  Result := TdxTileBarDetailSiteTitleViewInfo;
end;

procedure TdxTileBarDetailSite.InitializeSite(ALeft, ATop: Integer);
var
  AArea: TRect;
begin
  Align := alNone;
  Parent := TileControl;
  AArea := TileBar.ViewInfo.DetailSiteArea;
  SetBounds(ALeft, ATop, cxRectWidth(AArea), cxRectHeight(AArea));
  Visible := True;
  Realign;
end;

procedure TdxTileBarDetailSite.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  AArea, R: TRect;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if ActiveControl <> nil then
  begin
    AArea := cxRectBounds(ALeft, ATop, AWidth, AHeight);
    ActiveControl.Align := alNone;
    R := cxRectOffsetVert(cxRectOffsetHorz(AArea, -AArea.Left), -AArea.Top);
    Inc(R.Top, cxRectHeight(TitleViewInfo.Bounds));
    ActiveControl.BoundsRect := R;
    ActiveControl.Anchors := [akLeft..akBottom];
    ActiveControl.Visible := True;
  end;
end;

{ TdxTileBarItemDropDownButtonViewInfo }

constructor TdxTileBarItemDropDownButtonViewInfo.Create(AItem: TdxTileBarItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor TdxTileBarItemDropDownButtonViewInfo.Destroy;
begin
  EndMouseTracking(Self);
  inherited Destroy;
end;

procedure TdxTileBarItemDropDownButtonViewInfo.Click;
begin
  Item.MakeVisible;
  TileBar.Refresh;
  TileBar.ShowPopupWindow(Item);
end;

procedure TdxTileBarItemDropDownButtonViewInfo.DoCalculate;
const
  AState: array[Boolean] of TcxButtonState = (cxbsDisabled, cxbsNormal);
var
  ABounds: TRect;
  AInflateDelta: Integer;
begin
  if (Item.Group = nil) or not Item.IsNeedDropDownButton then
  begin
    Bounds := cxNullRect;
    inherited DoCalculate;
    Exit;
  end;
  ABounds := Item.ViewInfo.Bounds;
  AInflateDelta := ScaleFactor.Apply(Item.ViewInfo.InflateDelta);
  if (TileBar.OptionsView.Position = tbpRight) xor TileBar.ViewInfo.UseRightToLeftAlignment then
    ABounds.Right := ABounds.Left + TileBar.OptionsView.DropDownButtonWidth - AInflateDelta
  else
    ABounds.Left := ABounds.Right - TileBar.OptionsView.DropDownButtonWidth + AInflateDelta;
  if AInflateDelta <> 0 then
    ABounds := cxRectInflate(ABounds, AInflateDelta);
  Bounds := ABounds;
  inherited DoCalculate;
  State := AState[Item.Enabled];
end;

procedure TdxTileBarItemDropDownButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
const
  ADirection: array[TdxTileBarPosition] of TcxArrowDirection = (adDown, adUp, adRight, adLeft);
begin
  (TileBar.Painter as TdxTileBarPainter).DrawDropDownButton(ACanvas, Bounds,
    ADirection[TileBar.OptionsView.ActualPosition], State, TileBar.OptionsBehavior.ItemHotTrackHighlightColor, Item);
end;

function TdxTileBarItemDropDownButtonViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := inherited GetHitTest(AHitTest) and Item.Enabled;
  if Result then
  begin
    BeginMouseTracking(TileBar, Bounds, Self);
    AHitTest.HitObject := Self;
    TdxTileBarHitTest(AHitTest).BitState[tbhtDropDownButton] := True;
    State := cxbsHot;
  end;
end;

function TdxTileBarItemDropDownButtonViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TileBar.ScaleFactor;
end;

function TdxTileBarItemDropDownButtonViewInfo.GetTileBar: TdxCustomTileBar;
begin
  Result := Item.TileBar;
end;

function TdxTileBarItemDropDownButtonViewInfo.GetVisibleBounds: TRect;
begin
  if Item.IsPopupControlAssigned then
    Result := Item.ViewInfo.VisibleBounds
  else
    Result := cxNullRect;
end;

procedure TdxTileBarItemDropDownButtonViewInfo.SetState(AValue: TcxButtonState);
begin
  if TileBar.PopupWindow.Visible and (TileBar.PopupWindow.ActualBarItem = Item) then
    AValue := cxbsHot;
  if FState <> AValue then
  begin
    FState := AValue;
    TileBar.InvalidateRect(Bounds, False);
  end;
end;

procedure TdxTileBarItemDropDownButtonViewInfo.MouseLeave;
begin
  EndMouseTracking(Self);
  State := cxbsNormal;
end;

function TdxTileBarItemDropDownButtonViewInfo.PtInCaller(const P: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, P);
end;

{ TdxTileBarItemViewInfo }

constructor TdxTileBarItemViewInfo.Create(AOwner: TdxTileControlItem);
begin
  inherited Create(AOwner);
  FDropDownButton := TdxTileBarItemDropDownButtonViewInfo.Create(Item);
end;

destructor TdxTileBarItemViewInfo.Destroy;
begin
  FreeAndNil(FDropDownButton);
  inherited Destroy;
end;

function TdxTileBarItemViewInfo.CanFocusInDesigning: Boolean;
begin
  Result := True;
end;

procedure TdxTileBarItemViewInfo.DrawContent(ACanvas: TcxCanvas; const ADrawRect: TRect);
begin
  inherited DrawContent(ACanvas, ADrawRect);
  if DropDownButton.Visible then
    DropDownButton.Draw(ACanvas);
end;

function TdxTileBarItemViewInfo.GetHighlightBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
  if Item.IsPopupControlAssigned then
    if (Item.TileBar.OptionsView.Position = tbpRight) xor Item.TileBar.ViewInfo.UseRightToLeftAlignment then
      Result.Left := Result.Left + cxRectWidth(DropDownButton.Bounds)
    else
      Result.Right := Result.Right - cxRectWidth(DropDownButton.Bounds);
end;

function TdxTileBarItemViewInfo.GetHitTest(AHitTest: TdxTileControlHitTest): Boolean;
begin
  Result := Visible and PtInRect(ClipRect, AHitTest.HitPoint);
  if Result and (Item.TileBar.IsDesigning or not DropDownButton.GetHitTest(AHitTest)) then
  begin
    AHitTest.HitObject := Self;
    TdxTileBarHitTest(AHitTest).BitState[tchtItem] := True;
  end;
end;

function TdxTileBarItemViewInfo.GetItem: TdxTileBarItem;
begin
  Result := inherited Item as TdxTileBarItem;
end;

procedure TdxTileBarItemViewInfo.Scroll(const DX, DY: Integer);
begin
  DropDownButton.Scroll(DX, DY);
  inherited Scroll(DX, DY);
end;

procedure TdxTileBarItemViewInfo.SetInflateDelta(AValue: Integer);
begin
  inherited SetInflateDelta(AValue);
  if Item.IsPopupControlAssigned then
  begin
    DropDownButton.Recalculate;
    TileControl.InvalidateRect(DropDownButton.Bounds, False);
  end;
end;

procedure TdxTileBarItemViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  if not cxRectIsNull(DropDownButton.Bounds) then
    DropDownButton.DoRightToLeftConversion(AClientBounds);
end;

{ TdxTileBarItemDetailOptions }

function TdxTileBarItemDetailOptions.GetBarItem: TdxTileBarItem;
begin
  Result := GetOwner as TdxTileBarItem;
end;

function TdxTileBarItemDetailOptions.GetDetailSiteHeight: Integer;
begin
  Result := cxRectHeight(TileBar.ViewInfo.DetailSiteArea);
end;

function TdxTileBarItemDetailOptions.GetDetailSiteWidth: Integer;
begin
  Result := cxRectWidth(TileBar.ViewInfo.DetailSiteArea);
end;

function TdxTileBarItemDetailOptions.GetTileBar: TdxCustomTileBar;
begin
  Result := BarItem.TileBar;
end;

procedure TdxTileBarItemDetailOptions.SetDetailControl(AValue: TWinControl);
begin
  if DetailControl <> AValue then
  begin
    if (AValue <> nil) and (TdxTileBarItem(TileItem).PopupOptions.PopupControl = AValue) then
      TdxTileBarItem(TileItem).PopupOptions.PopupControl := nil;
    inherited SetDetailControl(AValue);
  end;
end;

{ TdxTileBarItemPopupOptions }

constructor TdxTileBarItemPopupOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBorderColor := clNone;
  FBorderHeight := 1;
  FBorderWidth := 1;
  FColor := clDefault;
  FPopupControl := nil;
  FPopupControlNotifyComponent := TcxFreeNotificator.Create(nil);
  FPopupControlNotifyComponent.OnFreeNotification := PopupControlFreeNotification;
end;

destructor TdxTileBarItemPopupOptions.Destroy;
begin
  PopupControl := nil;
  FPopupControlNotifyComponent.OnFreeNotification := nil;
  FreeAndNil(FPopupControlNotifyComponent);
  inherited Destroy;
end;

procedure TdxTileBarItemPopupOptions.Assign(ASource: TPersistent);
begin
  if ASource is TdxTileBarItemPopupOptions then
  begin
    FPopupControl := TdxTileBarItemPopupOptions(ASource).PopupControl;
    FBorderColor := TdxTileBarItemPopupOptions(ASource).BorderColor;
    FColor := TdxTileBarItemPopupOptions(ASource).Color;
    Changed;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxTileBarItemPopupOptions.Changed;
begin
  BarItem.Changed;
end;

procedure TdxTileBarItemPopupOptions.ChangeScale(M, D: Integer);
begin
  FBorderHeight := MulDiv(FBorderHeight, M, D);
  FBorderWidth := MulDiv(FBorderWidth, M, D);
end;

procedure TdxTileBarItemPopupOptions.PopupControlFreeNotification(AComponent: TComponent);
begin
  FPopupControl := nil;
  BarItem.Changed;
  TileBar.LayoutChanged;
end;

function TdxTileBarItemPopupOptions.IsChanged: Boolean;
begin
  Result := (FPopupControl <> nil) or (FBorderColor <> clNone) or (FBorderWidth <> 1) or (FBorderHeight <> 1) or
    (FColor <> clDefault);
end;

function TdxTileBarItemPopupOptions.GetBarItem: TdxTileBarItem;
begin
  Result := GetOwner as TdxTileBarItem;
end;

function TdxTileBarItemPopupOptions.GetTileBar: TdxCustomTileBar;
begin
  Result := BarItem.TileBar;
end;

procedure TdxTileBarItemPopupOptions.RestoreControlData(AControl: TWinControl);
begin
  if AControl = nil then Exit;
  AControl.Parent := FSavedControlData.Parent;
  AControl.Anchors := FSavedControlData.Anchors;
  AControl.Align := FSavedControlData.Align;
  if AControl is TCustomForm then
    TCustomForm(AControl).BorderStyle := FSavedControlData.BorderStyle;
  AControl.BoundsRect := FSavedControlData.BoundsRect;
  AControl.Visible := FSavedControlData.Visible;
end;

procedure TdxTileBarItemPopupOptions.SaveControlData(AControl: TWinControl);
begin
  FSavedControlData.Parent := AControl.Parent;
  FSavedControlData.Anchors := AControl.Anchors;
  FSavedControlData.Align := AControl.Align;
  if AControl is TCustomForm then
  begin
    FSavedControlData.BorderStyle := TCustomForm(AControl).BorderStyle;
    TCustomForm(AControl).BorderStyle := bsNone;
  end;
  FSavedControlData.BoundsRect := AControl.BoundsRect;
  FSavedControlData.Visible := AControl.Visible;
end;

procedure TdxTileBarItemPopupOptions.SetBorderColor(AValue: TColor);
begin
  if FBorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Changed;
  end;
end;

procedure TdxTileBarItemPopupOptions.SetBorderHeight(AValue: Integer);
begin
  if FBorderHeight <> AValue then
  begin
    FBorderHeight := AValue;
    Changed;
  end;
end;

procedure TdxTileBarItemPopupOptions.SetBorderWidth(AValue: Integer);
begin
  if FBorderWidth <> AValue then
  begin
    FBorderWidth := AValue;
    Changed;
  end;
end;

procedure TdxTileBarItemPopupOptions.SetColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxTileBarItemPopupOptions.SetPopupControl(AControl: TWinControl);

  function ControlHasTileBarAsParent: Boolean;
  var
    AParent: TControl;
  begin
    Result := AControl = TileBar;
    AParent := TileBar;
    while AParent <> nil do
    begin
      if AParent = AControl then
      begin
        Result := True;
        Break;
      end;
      AParent := AParent.Parent;
    end;
  end;

begin
  if FPopupControl = AControl then
    Exit;
  FPopupControlNotifyComponent.RemoveSender(FPopupControl);
  if AControl = nil then
  begin
    if not TileBar.IsDesigning then
      RestoreControlData(FPopupControl);
  end
  else
    if ControlHasTileBarAsParent then
      raise EcxEditError.Create(cxGetResourceString(@cxSEditPopupCircularReferencingError))
    else
    begin
      if BarItem.DetailOptions.DetailControl = AControl then
        BarItem.DetailOptions.DetailControl := nil;
      if not TileBar.IsDesigning then
      begin
        RestoreControlData(FPopupControl);
        SaveControlData(AControl);
        AControl.Parent := nil;
        AControl.Visible := False;
      end;
    end;
  FPopupControl := AControl;
  FPopupControlNotifyComponent.AddSender(FPopupControl);
  BarItem.Changed;
  TileBar.LayoutChanged;
end;

{ TdxTileBarItem }

constructor TdxTileBarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupOptions := TdxTileBarItemPopupOptions.Create(Self);
end;

destructor TdxTileBarItem.Destroy;
begin
  FreeAndNil(FPopupOptions);
  inherited Destroy;
end;

procedure TdxTileBarItem.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  PopupOptions.ChangeScale(M, D);
end;

function TdxTileBarItem.CreateDetailSite: TdxTileControlDetailSite;
begin
  Result := TdxTileBarDetailSite.Create(Self);
end;

function TdxTileBarItem.CreateViewInfo: TdxTileControlItemViewInfo;
begin
  Result := TdxTileBarItemViewInfo.Create(Self);
end;

function TdxTileBarItem.GetDetailOptions: TdxTileBarItemDetailOptions;
begin
  Result := inherited DetailOptions as TdxTileBarItemDetailOptions;
end;

function TdxTileBarItem.GetDetailOptionsClass: TdxTileControlItemDetailOptionsClass;
begin
  Result := TdxTileBarItemDetailOptions;
end;

function TdxTileBarItem.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

function TdxTileBarItem.GetViewInfo: TdxTileBarItemViewInfo;
begin
  Result := inherited ViewInfo as TdxTileBarItemViewInfo;
end;

function TdxTileBarItem.IsPopupControlAssigned: Boolean;
begin
  Result := PopupOptions.PopupControl <> nil;
end;

function TdxTileBarItem.IsPopupOptionsStored: Boolean;
begin
  Result := PopupOptions.IsChanged;
end;

function TdxTileBarItem.IsNeedDropDownButton: Boolean;
begin
  Result := IsPopupControlAssigned and not (IsDragged or TdxTileControlGroupAccess(Group).IsDragged);
end;

procedure TdxTileBarItem.SetDetailOptions(AValue: TdxTileBarItemDetailOptions);
begin
  inherited SetDetailOptions(AValue);
end;

procedure TdxTileBarItem.SetPopupOptions(AValue: TdxTileBarItemPopupOptions);
begin
  FPopupOptions.Assign(AValue);
  Changed;
end;

procedure TdxTileBarItem.SetSize(AValue: TdxTileBarItemSize);
begin
  if AValue = tbisRegular then
    inherited SetSize(tcisRegular)
  else
    inherited SetSize(tcisLarge);
  FSize := AValue;
end;

{ TdxTileBarScrollButtonViewInfo }

procedure TdxTileBarScrollButtonViewInfo.DoCalculateTopLeft(const ASizeX, ASizeY: Integer; var ATop, ALeft: Integer);
const
  AMinOffset = 2;
var
  R: TRect;
  dX, dY: Integer;
  ADPI_MinOffset: Integer;
begin
  dX := ASizeX div 2;
  dY := ASizeY div 2;
  ADPI_MinOffset := ScaleFactor.Apply(AMinOffset);
  R := ViewInfo.TilesZone;
  if OptionsView.GroupLayout = glHorizontal then
    Inc(R.Top, ViewInfo.GetGroupsCaptionsMaxHeight);

  case Direction of
    adUp:
    begin
      ALeft := (R.Left + R.Right) div 2 - dX;
      if OptionsView.FixedIndentVert then
        ATop := Max(ADPI_MinOffset, R.Top + OptionsView.IndentVert div 2 - dY)
      else
        ATop := R.Top + dY;
    end;

    adDown:
    begin
      ALeft := (R.Left + R.Right) div 2 - dX;
      if OptionsView.FixedIndentVert then
        ATop := Min(R.Bottom - ASizeY - ADPI_MinOffset, R.Bottom - OptionsView.IndentVert div 2 - dY)
      else
        ATop := R.Bottom - ASizeY - dY;
    end;

    adLeft:
      begin
        ATop := (R.Top + R.Bottom) div 2 - dY;
        if OptionsView.FixedIndentHorz then
          ALeft := Max(ADPI_MinOffset, R.Left + OptionsView.IndentHorz div 2 - dX)
        else
          ALeft := R.Left + dX;
      end;
  else
    begin
      ATop := (R.Top + R.Bottom) div 2 - dY;
      if OptionsView.FixedIndentHorz then
        ALeft := Min(R.Right - ASizeX - ADPI_MinOffset, R.Right - OptionsView.IndentHorz div 2 - dX)
      else
        ALeft := R.Right - ASizeX - dX;
    end;
  end;
end;

function TdxTileBarScrollButtonViewInfo.GetOptionsView: TdxTileBarOptionsView;
begin
  Result := ViewInfo.TileBar.OptionsView;
end;

function TdxTileBarScrollButtonViewInfo.GetViewInfo: TdxTileBarViewInfo;
begin
  Result := inherited ViewInfo as TdxTileBarViewInfo;
end;

{ TdxTileBarViewInfo }

procedure TdxTileBarViewInfo.AddItem(AItem: TdxTileControlItem);
var
  ABarItem: TdxTileBarItem;
begin
  if csDestroying in AItem.ComponentState then
    Exit;
  inherited AddItem(AItem);
  ABarItem := AItem as TdxTileBarItem;
  ABarItem.ViewInfo.DropDownButton.DoCalculate;
end;

function TdxTileBarViewInfo.CalculateTilesZone: TRect;
var
  APosition: TdxTileBarPosition;
  R: TRect;
  AItemsWidth: Integer;
begin
  Result := inherited CalculateTilesZone;
  R := Result;
  APosition := OptionsView.Position;
  if GroupLayout = glHorizontal then
  begin
    MeasureGroupsCaptionsHeights;
    Result.Bottom := R.Top + GetGroupsCaptionsMaxHeight + 2 * OptionsView.IndentVert + Max(GetItemsMaxHeight, OptionsView.ItemHeight);
    if APosition = tbpBottom then
      Result := cxRectOffsetVert(Result, R.Bottom - Result.Bottom)
  end
  else
  begin
    AItemsWidth := GetItemsMaxWidth;
    if AItemsWidth = 0 then
      AItemsWidth := 2 * OptionsView.ItemWidth + OptionsView.ItemIndent;
    Result.Right := Result.Left + 2 * OptionsView.IndentHorz + AItemsWidth;
    if APosition = tbpRight then
      Result := cxRectOffsetHorz(Result, R.Right - Result.Right);
  end;
end;

function TdxTileBarViewInfo.GetDetailSiteArea: TRect;
begin
  Result := cxRectInflate(inherited GetDetailSiteArea, -TileBar.BorderSize, -TileBar.BorderSize);
  case OptionsView.ActualPosition of
    tbpBottom:
      begin
        Result.Top := TitleHeight + 1;
        Result.Bottom := TilesZone.Top - 1;
      end;
    tbpLeft:
      begin
        Result.Top := TilesZone.Top;
        Result.Left := TilesZone.Right + 1;
      end;
    tbpRight:
      begin
        Result.Top := TilesZone.Top;
        Result.Right := TilesZone.Left - 1;
      end;
  else
    Result.Top := TilesZone.Bottom + 1;
  end;
end;

function TdxTileBarViewInfo.GetItemsMaxHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TileControl.Items.Count - 1 do
    Result := Max(Result, (TileControl.Items[I].ViewInfo as TdxTileBarItemViewInfo).GetHeight);
end;

function TdxTileBarViewInfo.GetOptionsView: TdxTileBarOptionsView;
begin
  Result := TileBar.OptionsView;
end;

function TdxTileBarViewInfo.GetScrollButtonViewInfoClass: TdxTileControlScrollButtonViewInfoClass;
begin
  Result := TdxTileBarScrollButtonViewInfo;
end;

function TdxTileBarViewInfo.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

{ TdxTileBarPopupWindowViewInfo }

constructor TdxTileBarPopupWindowViewInfo.Create(AOwner: TdxTileBarPopupWindow);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxTileBarPopupWindowViewInfo.Calculate(const ABounds: TRect);
begin
  CheckCallOutElementSize;
  FBounds := ABounds;
  CalculateCallOutPoints;
  CalculateOutTilesRects;
  FClientBounds := cxRectContent(FBounds, Borders);
  case TileBar.OptionsView.ActualPosition of
    tbpTop:
      Inc(FClientBounds.Top, FCallOutElementSize.cy);
    tbpBottom:
      Dec(FClientBounds.Bottom, FCallOutElementSize.cy);
    tbpLeft:
      Inc(FClientBounds.Left, FCallOutElementSize.cx);
  else
    Dec(FClientBounds.Right, FCallOutElementSize.cx);
  end;
  FPopupControlBounds := FClientBounds;
end;

procedure TdxTileBarPopupWindowViewInfo.CalculateCallOutPoints;
var
  APosition: TdxTileBarPosition;
begin
  APosition := TileBar.OptionsView.ActualPosition;
  if TileBar.GroupLayout = glHorizontal then
  begin
    FCallOutPoints1[0].X := Bounds.Left;
    if APosition = tbpTop then
      FCallOutPoints1[0].Y := Bounds.Top
    else
      FCallOutPoints1[0].Y := Bounds.Bottom;
    FCallOutPoints1[1].X := FOwner.CallOutPos;
    FCallOutPoints1[1].Y := FCallOutPoints1[0].Y;
    FCallOutPoints1[2].X := FCallOutPoints1[1].X - FCallOutElementSize.cx div 2 + Integer(APosition = tbpBottom);
    FCallOutPoints1[2].Y := FCallOutPoints1[1].Y + dxSignature[APosition = tbpTop] * FCallOutElementSize.cy;
    FCallOutPoints1[3].X := FCallOutPoints1[0].X;
    FCallOutPoints1[3].Y := FCallOutPoints1[2].Y;

    FCallOutPoints2[0].X := FCallOutPoints1[1].X + 1 - Integer(APosition = tbpBottom);
    FCallOutPoints2[0].Y := FCallOutPoints1[1].Y;
    FCallOutPoints2[1].X := Bounds.Right;
    FCallOutPoints2[1].Y := FCallOutPoints2[0].Y;
    FCallOutPoints2[2].X := FCallOutPoints2[1].X;
    FCallOutPoints2[2].Y := FCallOutPoints2[1].Y + dxSignature[APosition = tbpTop] * FCallOutElementSize.cy;
    FCallOutPoints2[3].X := FCallOutPoints2[0].X + FCallOutElementSize.cx div 2;
    FCallOutPoints2[3].Y := FCallOutPoints2[2].Y;
  end
  else
  begin
    if APosition = tbpLeft then
      FCallOutPoints1[0].X := Bounds.Left
    else
      FCallOutPoints1[0].X := Bounds.Right;
    FCallOutPoints1[0].Y := Bounds.Top;
    FCallOutPoints1[1].X := FCallOutPoints1[0].X;
    FCallOutPoints1[1].Y := FOwner.CallOutPos;
    FCallOutPoints1[2].X := FCallOutPoints1[1].X + dxSignature[APosition = tbpLeft] * FCallOutElementSize.cx;
    FCallOutPoints1[2].Y := FCallOutPoints1[1].Y - FCallOutElementSize.cy div 2;
    FCallOutPoints1[3].X := FCallOutPoints1[2].X;
    FCallOutPoints1[3].Y := FCallOutPoints1[0].Y;

    FCallOutPoints2[0].X := FCallOutPoints1[1].X;
    FCallOutPoints2[0].Y := FCallOutPoints1[1].Y;
    FCallOutPoints2[1].X := FCallOutPoints2[0].X;
    FCallOutPoints2[1].Y := Bounds.Bottom;
    FCallOutPoints2[2].X := FCallOutPoints2[1].X + dxSignature[APosition = tbpLeft] * FCallOutElementSize.cx;
    FCallOutPoints2[2].Y := FCallOutPoints2[1].Y;
    FCallOutPoints2[3].X := FCallOutPoints2[2].X;
    FCallOutPoints2[3].Y := FCallOutPoints2[0].Y + FCallOutElementSize.cy div 2;
  end;
end;

procedure TdxTileBarPopupWindowViewInfo.CalculateOutTilesRects;
var
  APosition: TdxTileBarPosition;
begin
  APosition := TileBar.OptionsView.ActualPosition;
  if TileBar.GroupLayout = glHorizontal then
  begin
    FOutTilesArea1.Left := 0;
    FOutTilesArea1.Right := TileBar.ViewInfo.TilesArea.Left;
    FOutTilesArea2.Left := TileBar.ViewInfo.TilesArea.Right;
    FOutTilesArea2.Right := Bounds.Right;
    if APosition = tbpTop then
    begin
      FOutTilesArea1.Top := 0;
      FOutTilesArea1.Bottom := FCallOutElementSize.cy;
    end
    else
    begin
      FOutTilesArea1.Top := Bounds.Bottom - FCallOutElementSize.cy;
      FOutTilesArea1.Bottom := Bounds.Bottom;
    end;
    FOutTilesArea2.Top := FOutTilesArea1.Top;
    FOutTilesArea2.Bottom := FOutTilesArea1.Bottom;
  end
  else
  begin
    FOutTilesArea1.Top := 0;
    FOutTilesArea1.Bottom := TileBar.ViewInfo.TilesArea.Top;
    FOutTilesArea2.Top := TileBar.ViewInfo.TilesArea.Bottom;
    FOutTilesArea2.Bottom := Bounds.Bottom;
    if APosition = tbpLeft then
    begin
      FOutTilesArea1.Left := 0;
      FOutTilesArea1.Right := FCallOutElementSize.cx;
    end
    else
    begin
      FOutTilesArea1.Left := Bounds.Right - FCallOutElementSize.cx;
      FOutTilesArea1.Right := Bounds.Right;
    end;
    FOutTilesArea2.Left := FOutTilesArea1.Left;
    FOutTilesArea2.Right := FOutTilesArea1.Right;
  end;
end;

function TdxTileBarPopupWindowViewInfo.CalculatePosition: TPoint;
var
  ATilesArea, ADetailArea: TRect;
begin
  ATilesArea := TileBar.ViewInfo.TilesArea;
  ADetailArea := TileBar.ViewInfo.DetailSiteArea;
  case TileBar.OptionsView.ActualPosition of
    tbpTop:
      Result := cxPoint(0, ADetailArea.Top - FCallOutElementSize.cy);

    tbpBottom:
      Result := cxPoint(0, ADetailArea.Bottom - FRequiredSize.cy + FCallOutElementSize.cy);

    tbpLeft:
      Result := cxPoint(ADetailArea.Left - FCallOutElementSize.cx, 0);

  else
    Result := cxPoint(ADetailArea.Right - FRequiredSize.cx + FCallOutElementSize.cx, 0);
  end;
  ClientToScreen(TileBar.Handle, Result);
end;

procedure TdxTileBarPopupWindowViewInfo.CalculateRequiredSize;
var
  AHeight, AWidth: Integer;
begin
  CheckCallOutElementSize;
  if TileBar.OptionsView.GroupLayout = glHorizontal then
  begin
    AHeight := ActualControl.Height + FCallOutElementSize.cy;
    if FOwner.HasBorder then
      AHeight := AHeight + 2 * BorderHeight;
    AWidth := TileBar.ClientWidth;
  end
  else
  begin
    AHeight := TileBar.ClientHeight;
    AWidth := ActualControl.Width + FCallOutElementSize.cx;
    if FOwner.HasBorder then
      AWidth := AWidth + 2 * BorderWidth;
  end;
  FRequiredSize.cx := AWidth;
  FRequiredSize.cy := AHeight;
end;

procedure TdxTileBarPopupWindowViewInfo.CheckCallOutElementSize;
begin
  FCallOutElementSize.cx := dxTileBarDefaultCallOutWidth;
  FCallOutElementSize.cy := dxTileBarDefaultCallOutHeight;
  FCallOutElementSize := TileBar.ScaleFactor.Apply(FCallOutElementSize);
  if TileBar.GroupLayout = glVertical then
    ExchangeLongWords(FCallOutElementSize.cx, FCallOutElementSize.cy);
end;

function TdxTileBarPopupWindowViewInfo.CreateRegion: HRGN;
var
  ARegion: HRGN;
begin
  Result := CreateRectRgnIndirect(Bounds);

  ARegion := CreatePolygonRgn(FCallOutPoints1[0], Length(FCallOutPoints1), WINDING);
  if ARegion <> 0 then
  begin
    CombineRgn(Result, Result, ARegion, RGN_DIFF);
    DeleteObject(ARegion);
  end;
  ARegion := CreatePolygonRgn(FCallOutPoints2[0], Length(FCallOutPoints2), WINDING);
  if ARegion <> 0 then
  begin
    CombineRgn(Result, Result, ARegion, RGN_DIFF);
    DeleteObject(ARegion);
  end;

  ARegion := CreateRectRgn(FOutTilesArea1.Left, FOutTilesArea1.Top, FOutTilesArea1.Right, FOutTilesArea1.Bottom);
  if ARegion <> 0 then
  begin
    CombineRgn(Result, Result, ARegion, RGN_DIFF);
    DeleteObject(ARegion);
  end;
  ARegion := CreateRectRgn(FOutTilesArea2.Left, FOutTilesArea2.Top, FOutTilesArea2.Right, FOutTilesArea2.Bottom);
  if ARegion <> 0 then
  begin
    CombineRgn(Result, Result, ARegion, RGN_DIFF);
    DeleteObject(ARegion);
  end;
end;

function TdxTileBarPopupWindowViewInfo.GetActualControl: TWinControl;
begin
  Result := FOwner.ActualControl;
end;

function TdxTileBarPopupWindowViewInfo.GetBorderColor: TColor;
begin
  Result := FOwner.BorderColor;
end;

function TdxTileBarPopupWindowViewInfo.GetBorderHeight: Integer;
begin
  Result := FOwner.BorderHeight;
end;

function TdxTileBarPopupWindowViewInfo.GetBorderWidth: Integer;
begin
  Result := FOwner.BorderWidth;
end;

function TdxTileBarPopupWindowViewInfo.GetBorders: TRect;
begin
  if FOwner.HasBorder then
    Result := cxRect(BorderWidth, BorderHeight, BorderWidth, BorderHeight)
  else
    Result := cxNullRect;
end;

function TdxTileBarPopupWindowViewInfo.GetColor: TColor;
begin
  Result := FOwner.Color;
end;

function TdxTileBarPopupWindowViewInfo.GetTileBar: TdxCustomTileBar;
begin
  Result := FOwner.TileBar;
end;

procedure TdxTileBarPopupWindowViewInfo.Paint(ACanvas: TcxCanvas);
var
  ARegion: HRGN;
begin
  ARegion := CreateRegion;
  SetWindowRgn(FOwner.Handle, CreateRegion, True);
  if ARegion <> 0 then
  begin
    ACanvas.FillRegion(ARegion, FOwner.Color);
    if FOwner.HasBorder then
      ACanvas.FrameRegion(ARegion, FOwner.BorderColor, FOwner.BorderWidth, FOwner.BorderHeight);
    DeleteObject(ARegion);
  end;
end;

{ TdxTileBarPopupWindow }

constructor TdxTileBarPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  FViewInfo := TdxTileBarPopupWindowViewInfo.Create(Self);
  ModalMode := True;
end;

destructor TdxTileBarPopupWindow.Destroy;
begin
  FreeAndNil(FViewInfo);
  inherited;
end;

procedure TdxTileBarPopupWindow.CMShowPopupWindow(var Message: TMessage);
begin
  BringToFront;
  OwnerBounds := cxRectSetOrigin(ActualBarItem.ViewInfo.DropDownButton.Bounds,
    TileBar.ClientToParent(ActualBarItem.ViewInfo.DropDownButton.Bounds.TopLeft));
  OwnerParent := TileBar.Parent;
  StoreActualControlSize;
  Popup(ActualControl);
end;

procedure TdxTileBarPopupWindow.CloseUp;
var
  AAllow: Boolean;
  P: TPoint;
begin
  AAllow := True;
  if Assigned(TileBar.OnPopupDeactivating) then
    TileBar.OnPopupDeactivating(TileBar, ActualBarItem, AAllow);
  if AAllow then
  begin
    P := TileBar.GetMouseCursorClientPos;
    TileBar.HitTest.Calculate(P.X, P.Y);
    if TileBar.HitTest.DropDownButton <> nil then
      SetLastActualBarItem(ActualBarItem);
    TdxTileBarPopupWindowAnimation.Create(TileBar, False).ImmediateAnimation;
    inherited CloseUp;
    if Assigned(TileBar.OnPopupDeactivate) then
      TileBar.OnPopupDeactivate(TileBar, ActualBarItem);
    RestoreActualControlSize;
  end;
  ActualBarItem.ViewInfo.DropDownButton.State := cxbsDefault;
end;

procedure TdxTileBarPopupWindow.CorrectBoundsWithDesktopWorkArea(
  var APosition: TPoint; var ASize: TSize);
var
  ADesktopWorkArea, R: TRect;
begin
  inherited CorrectBoundsWithDesktopWorkArea(APosition, ASize);
  ADesktopWorkArea := GetDesktopWorkArea(OwnerScreenBounds.TopLeft);
  cxRectIntersect(R, OwnerScreenBounds, ADesktopWorkArea);
  if TileBar.GroupLayout = glHorizontal then
    FCallOutPos := R.Left + cxHalfCoordinate(cxRectWidth(R)) - APosition.X
  else
    FCallOutPos := R.Top + cxHalfCoordinate(cxRectHeight(R)) - APosition.Y;
  FViewInfo.Calculate(cxRect(FViewInfo.FRequiredSize));
end;

procedure TdxTileBarPopupWindow.DoShowed;
begin
  inherited;
  ActualControl.Align := alNone;
  ActualControl.BoundsRect := FViewInfo.PopupControlBounds;
  TdxTileBarPopupWindowAnimation.Create(TileBar, True).ImmediateAnimation;
  if Assigned(TileBar.OnPopupActivate) then
    TileBar.OnPopupActivate(TileBar, ActualBarItem);
end;

procedure TdxTileBarPopupWindow.DoShowing;
begin
  inherited;
  AlphaBlend := True;
  AlphaBlendValue := 0;
end;

function TdxTileBarPopupWindow.CalculatePosition(const ASize: TSize): TPoint;
begin
  Result := FViewInfo.CalculatePosition;
end;

function TdxTileBarPopupWindow.CalculateSize: TSize;
begin
  FViewInfo.CalculateRequiredSize;
  Result := FViewInfo.RequiredSize;
end;

procedure TdxTileBarPopupWindow.InitPopup;
begin
  inherited InitPopup;
  ActualControl.Parent := Self;
  Width := 0;
end;

function TdxTileBarPopupWindow.InternalBorderColor: TColor;
begin
  Result := ActualBarItem.PopupOptions.BorderColor;
end;

function TdxTileBarPopupWindow.InternalBorderHeight: Integer;
begin
  Result := ActualBarItem.PopupOptions.BorderHeight;
end;

function TdxTileBarPopupWindow.InternalBorderWidth: Integer;
begin
  Result := ActualBarItem.PopupOptions.BorderWidth;
end;

function TdxTileBarPopupWindow.InternalColor: TColor;
var
  AItem: TdxTileBarItem;
  AColor: TColor;
begin
  AItem := ActualBarItem;
  AColor := AItem.PopupOptions.Color;
  if AColor = clDefault then
    Result := TdxTileControlPainter.GetColorAsItemBackground(AItem)
  else
    Result := AColor;
end;

procedure TdxTileBarPopupWindow.Paint;
begin
  FViewInfo.Paint(Canvas);
end;

function TdxTileBarPopupWindow.GetPopupControlSize: TSize;
begin
  Result := cxSize(ActualControl.BoundsRect);
end;

function TdxTileBarPopupWindow.GetActualControl: TWinControl;
begin
  Result := nil;
  if ActualBarItem <> nil then
    Result := ActualBarItem.PopupOptions.PopupControl;
end;

function TdxTileBarPopupWindow.GetTileBar: TdxCustomTileBar;
begin
  Result := GetOwnerControl as TdxCustomTileBar;
end;

function TdxTileBarPopupWindow.HasBorder: Boolean;
begin
  Result := (BorderColor <> clNone) and (BorderColor <> clDefault);
end;

procedure TdxTileBarPopupWindow.SetActualBarItem(AItem: TdxTileBarItem);
begin
  if FActualBarItem <> AItem then
  begin
    SetActualControlVisibility(False);
    FActualBarItem := AItem;
    SetActualControlVisibility(True);
  end
  else
    if FActualBarItem <> nil then
      SetActualControlVisibility(True);
end;

procedure TdxTileBarPopupWindow.SetActualControlVisibility(AVisible: Boolean);
begin
  if ActualControl <> nil then
    ActualControl.Visible := AVisible;
end;

procedure TdxTileBarPopupWindow.SetLastActualBarItem(AItem: TdxTileBarItem);
begin
  FLastActualBarItem := AItem;
end;

procedure TdxTileBarPopupWindow.StoreActualControlSize;
begin
  FActualControlRealSize.cx := ActualControl.Width;
  FActualControlRealSize.cy := ActualControl.Height;
end;

procedure TdxTileBarPopupWindow.RestoreActualControlSize;
begin
  ActualControl.Width := FActualControlRealSize.cx;
  ActualControl.Height := FActualControlRealSize.cy;
end;

{ TdxTileBarPopupWindowAnimation }

constructor TdxTileBarPopupWindowAnimation.Create(ATileBar: TdxCustomTileBar; AIsShowing: Boolean);
const
  AAnimationTime: array [Boolean] of Cardinal = (dxTileBarPopupWindowHideTime, dxTileBarPopupWindowShowTime);
begin
  FTileBar := ATileBar;
  FIsShowing := AIsShowing;
  PopupWindow.Refresh;
  inherited Create(AAnimationTime[FIsShowing], ateLinear, 255);
end;

procedure TdxTileBarPopupWindowAnimation.DoAnimate;
begin
  if IsShowing then
    PopupWindow.AlphaBlendValue := Position
  else
    PopupWindow.AlphaBlendValue := 255 - Position;
end;

function TdxTileBarPopupWindowAnimation.GetPopupWindow: TdxTileBarPopupWindow;
begin
  Result := FTileBar.PopupWindow;
end;

{ TdxTileBarPainter }

procedure TdxTileBarPainter.DrawDefaultDropDownButtonArrow(ACanvas: TcxCanvas; const ARect: TRect; ADirection: TcxArrowDirection;
  const ABorderColor: TColor);
var
  APolygon: TcxArrowPoints;

  function GetArrowRect: TRect;
  const
    AWidths: array[TcxArrowDirection] of Integer = (8, 8, 4, 4);
    AHeights: array[TcxArrowDirection] of Integer = (4, 4, 8, 8);
  var
    P: TPoint;
    AWidth, AHeight: Integer;
  begin
    AWidth :=  TileBar.ScaleFactor.Apply(AWidths[ADirection]);
    AHeight :=  TileBar.ScaleFactor.Apply(AHeights[ADirection]);
    P := cxRectCenter(ARect);
    Result.Left := P.X - AWidth div 2;
    Result.Top := P.Y - AHeight div 2;
    Result.Right := Result.Left + AWidth;
    Result.Bottom := Result.Top + AHeight;
  end;

  procedure CalculatePolygon(const R: TRect);
  begin
    case ADirection of
      adUp:
      begin
        APolygon[0] := cxPoint(R.Left, R.Bottom);
        APolygon[1] := cxPoint((R.Left + R.Right) div 2, R.Top);
        APolygon[2] := R.BottomRight;
      end;

      adDown:
      begin
        APolygon[0] := R.TopLeft;
        APolygon[1] := cxPoint((R.Left + R.Right) div 2, R.Bottom);
        APolygon[2] := cxPoint(R.Right, R.Top);
      end;

      adLeft:
      begin
        APolygon[0] := cxPoint(R.Right, R.Top);
        APolygon[1] := cxPoint(R.Left, (R.Top + R.Bottom) div 2);
        APolygon[2] := R.BottomRight;
      end;
    else
      begin
        APolygon[0] := R.TopLeft;
        APolygon[1] := cxPoint(R.Right, (R.Top + R.Bottom) div 2);
        APolygon[2] := cxPoint(R.Left, R.Bottom)
      end;
    end;
  end;

var
  R: TRect;
begin
  R := GetArrowRect;
  CalculatePolygon(R);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.SetBrushColor(clWhite);
  ACanvas.Pen.Color := ABorderColor;
  ACanvas.Polygon(APolygon);
  ACanvas.Pen.Color := clWhite;
  if ADirection in [adUp, adDown] then
    Inc(APolygon[0].X)
  else
    Inc(APolygon[0].Y);
  ACanvas.Line(APolygon[0], APolygon[2]);
end;

procedure TdxTileBarPainter.DrawDropDownButton(ACanvas: TcxCanvas; const ARect: TRect; ADirection: TcxArrowDirection;
  AState: TcxButtonState; const AHighlightColor: TdxAlphaColor; AItem: TdxTileControlCustomItem);
var
  R: TRect;
  ASplitLineColor: TColor;
begin
  ASplitLineColor := dxGetLighterColor(GetColorAsItemBackground(AItem), 70);
  DrawDefaultDropDownButtonArrow(ACanvas, ARect, ADirection, ASplitLineColor);

  if not TileBar.IsDesigning and (AState in [cxbsHot, cxbsPressed]) then
    DrawHighlightRect(ACanvas, ARect, AHighlightColor, AItem.Style.GradientBeginColor);

  R := ARect;
  if (TileBar.OptionsView.Position = tbpRight) xor TileBar.ViewInfo.UseRightToLeftAlignment then
    R.Left := R.Right - 1
  else
    R.Right := R.Left + 1;
  R := cxRectInflate(R, 0, -TileBar.ScaleFactor.Apply(7));
  dxGpFillRect(ACanvas.Handle, R, ASplitLineColor);
end;

function TdxTileBarPainter.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

{ TdxTileBarOptionsBehavior }

function TdxTileBarOptionsBehavior.GetDefaultItemCheckMode: TdxTileControlItemCheckMode;
begin
  Result := tcicmNone;
end;

function TdxTileBarOptionsBehavior.GetDefaultItemFocusMode: TdxTileControlItemFocusMode;
begin
  Result := tcifmOuterFrame;
end;

function TdxTileBarOptionsBehavior.GetDefaultItemHotTrackMode: TdxTileControlItemHotTrackMode;
begin
  Result := tcihtmHighlight;
end;

function TdxTileBarOptionsBehavior.GetDefaultItemHotTrackHighlightColor: TdxAlphaColor;
begin
  Result := dxTileBarItemHighlightMaskColor;
end;

function TdxTileBarOptionsBehavior.GetDefaultItemMoving: Boolean;
begin
  Result := False;
end;

function TdxTileBarOptionsBehavior.GetDefaultItemPressAnimation: Boolean;
begin
  Result := False;
end;

function TdxTileBarOptionsBehavior.GetDefaultScrollMode: TdxTileControlScrollMode;
begin
  Result := smScrollButtons;
end;

{ TdxTileBarOptionsView }

constructor TdxTileBarOptionsView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FPosition := tbpTop;
end;

procedure TdxTileBarOptionsView.Assign(ASource: TPersistent);
begin
  inherited Assign(ASource);
  if ASource is TdxTileBarOptionsView then
    FPosition := TdxTileBarOptionsView(ASource).Position;
end;

class function TdxTileBarOptionsView.CheckGroupLayout(const APosition: TdxTileBarPosition): TdxTileControlGroupLayout;
begin
  if APosition in [tbpTop, tbpBottom] then
    Result := glHorizontal
  else
    Result := glVertical;
end;

procedure TdxTileBarOptionsView.DoItemChanged(AItem: TdxTileControlItem; const AData: Pointer);
begin
  (AItem as TdxTileBarItem).Changed;
end;

function TdxTileBarOptionsView.GetActualPosition: TdxTileBarPosition;
const
  APosition: array[Boolean] of TdxTileBarPosition = (tbpLeft, tbpRight);
begin
  Result := FPosition;
  if TileBar.ViewInfo.UseRightToLeftAlignment and (Result in [tbpLeft, tbpRight]) then
    Result := APosition[Result = tbpLeft];
end;

function TdxTileBarOptionsView.GetDefaultFixedIndentHorz: Boolean;
begin
  Result := True;
end;

function TdxTileBarOptionsView.GetDefaultFixedIndentVert: Boolean;
begin
  Result := True;
end;

function TdxTileBarOptionsView.GetDefaultGroupBlockMaxColumnCount: Integer;
begin
  Result := 1;
end;

function TdxTileBarOptionsView.GetDefaultGroupIndent: Integer;
begin
  Result := dxTileBarDefaultGroupIndent;
end;

function TdxTileBarOptionsView.GetDefaultGroupMaxRowCount: Integer;
begin
  Result := GetGroupMaxRowCount(glHorizontal);
end;

function TdxTileBarOptionsView.GetDefaultIndentHorz: Integer;
begin
  Result := dxTileBarDefaultIndentHorz;
end;

function TdxTileBarOptionsView.GetDefaultIndentVert: Integer;
begin
  Result := dxTileBarDefaultIndentVert;
end;

function TdxTileBarOptionsView.GetDefaultItemHeight: Integer;
begin
  Result := dxTileBarDefaultItemHeight;
end;

function TdxTileBarOptionsView.GetDefaultItemIndent: Integer;
begin
  Result := dxTileBarDefaultItemIndent;
end;

function TdxTileBarOptionsView.GetDefaultItemWidth: Integer;
begin
  Result := dxTileBarDefaultItemWidth;
end;

function TdxTileBarOptionsView.GetDropDownButtonWidth: Integer;
begin
  Result := TileBar.ScaleFactor.Apply(dxTileBarItemDropDownButtonWidth);
end;

class function TdxTileBarOptionsView.GetGroupMaxRowCount(const ALayout: TdxTileControlGroupLayout): Integer;
begin
  if ALayout = glHorizontal then
    Result := 1
  else
    Result := MaxInt div 2;
end;

class function TdxTileBarOptionsView.GetGroupMaxRowCount(const APosition: TdxTileBarPosition): Integer;
begin
  Result := GetGroupMaxRowCount(CheckGroupLayout(APosition));
end;

function TdxTileBarOptionsView.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

function TdxTileBarOptionsView.IsAllowableWidth(const AWidth: Integer): Boolean;
begin
  Result := AWidth > DropDownButtonWidth;
end;

procedure TdxTileBarOptionsView.SetPosition(AValue: TdxTileBarPosition);
begin
  if FPosition = AValue then Exit;
  TileBar.BeginUpdate;
  try
    FPosition := AValue;
    GroupLayout := CheckGroupLayout(FPosition);
    TileBar.ViewInfo.LeftScrollPos := 0;
    TileBar.ViewInfo.TopScrollPos := 0;
    TileBar.ForceCalculate := True;
    TdxTileControlItemCollectionAccess(TileBar.Items).ForEach(DoItemChanged, nil, False);
  finally
    TileBar.EndUpdate;
    TileBar.ForceCalculate := False;
  end;
  TileBar.CheckDesignTimeAlign;
  if not TileBar.IsLoading then
    TileBar.CheckDesignTimeSize;
  TileBar.Refresh;
end;

procedure TdxTileBarOptionsView.SetGroupLayout(AValue: TdxTileControlGroupLayout);

  procedure InternalCheckPosition;
  begin
    if (GroupLayout = glHorizontal) and (Position in [tbpLeft, tbpRight]) then
      Position := tbpTop;
    if (GroupLayout = glVertical) and (Position in [tbpTop, tbpBottom]) then
      Position := tbpRight;
  end;

begin
  if AValue = GroupLayout then
    Exit;
  TileBar.ForceCalculate := True;
  TileBar.BeginUpdate;
  try
    GroupMaxRowCount := GetGroupMaxRowCount(AValue);
    inherited SetGroupLayout(AValue);
    InternalCheckPosition;
  finally
    TileBar.EndUpdate;
    TileBar.ForceCalculate := False;
  end;
  TileBar.Refresh;
end;

{ TdxTileBarController }

function TdxTileBarController.CanPopupByKey(const Key: Word): Boolean;
begin
  case TileBar.OptionsView.Position of
    tbpTop:
      Result := Key = VK_DOWN;
    tbpBottom:
      Result := Key = VK_UP;
    tbpLeft:
      Result := Key = VK_RIGHT;
  else
    Result := Key = VK_LEFT;
  end;
end;

function TdxTileBarController.CanShowActionBarsOnRightClick: Boolean;
var
  I: Integer;
  AItems: TdxTileControlActionBarItems;
begin
  AItems := TileBar.ActionBars.Items;
  Result := inherited CanShowActionBarsOnRightClick and (AItems.Count > 0);
  if Result then
  begin
    Result := False;
    for I := 0 to AItems.Count - 1 do
    begin
      Result := AItems[I].Visible;
      if Result then
        Break;
    end;
  end;
end;

procedure TdxTileBarController.InternalMouseLeftDown(Shift: TShiftState; X, Y: Integer);
begin
  HitTest.Calculate(X, Y);
  FPressedDropDownButton := HitTest.DropDownButton;
  if (FPressedDropDownButton <> nil) and not FPressedDropDownButton.Item.Enabled then
    FPressedDropDownButton := nil;
  if FPressedDropDownButton <> nil then
    FPressedDropDownButton.State := cxbsPressed;
end;

procedure TdxTileBarController.InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AButton: TdxTileBarItemDropDownButtonViewInfo;
begin
  HitTest.Calculate(X, Y);
  if HitAtDropDownButton and (HitTest.DropDownButton = FPressedDropDownButton) and (Button = mbLeft) then
  begin
    AButton := HitTest.DropDownButton;
    AButton.Item.Click;
    AButton.Click;
  end;
  TileBar.PopupWindow.SetLastActualBarItem(nil);
end;

function TdxTileBarController.GetHitTest: TdxTileBarHitTest;
begin
  Result := TileBar.HitTest;
end;

function TdxTileBarController.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

function TdxTileBarController.HitAtDropDownButton: Boolean;
begin
  Result := HitTest.HitAtDropDownButton;
end;

procedure TdxTileBarController.KeyDown(var Key: Word; Shift: TShiftState);
var
  AFocusedItem: TdxTileBarItem;
begin
  AFocusedItem := FocusedItem as TdxTileBarItem;
  if (AFocusedItem <> nil) and AFocusedItem.Enabled and AFocusedItem.IsPopupControlAssigned and CanPopupByKey(Key) then
  begin
    TileBar.ShowPopupWindow(AFocusedItem);
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TdxTileBarController.SetFocusedItem(AValue: TdxTileControlItem);
var
  AOldFocusedItem: TdxTileControlItem;
begin
  AOldFocusedItem := FocusedItem;
  inherited SetFocusedItem(AValue);
  if (AOldFocusedItem <> AValue) and TileBar.IsDesigning then
    SetDesignerModified(TileBar);
end;

  // design selection
function TdxTileBarController.CreateDesignHelper: TdxTileControlCustomDesignHelper;
begin
  Result := dxTileBarDesignHelperClass.Create(TileBar);
end;

function TdxTileBarController.IsDesignHelperClassInitialized: Boolean;
begin
  Result := dxTileBarDesignHelperClass <> nil;
end;

function TdxTileBarController.CreateRegularItem(AGroup: TdxTileControlGroup): TdxTileControlItem;
begin
  Result := TileBar.CreateItem(tbisRegular, AGroup);
end;

function TdxTileBarController.IsDesignerMenuItemChecked(const AItemSizeIndex: Integer; AItem: TdxTileControlItem): Boolean;
begin
  if AItemSizeIndex = Integer(tbisRegular) then
    Result := IsDesignerMenuItemChecked(AItem, GetIsRegularItem)
  else
    Result := IsDesignerMenuItemChecked(AItem, GetIsLargeItem);
end;

function TdxTileBarController.GetIsLargeItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := ((AItem as TdxTileBarItem).Size = tbisLarge) and (AItem.RowCount = 1);
end;

function TdxTileBarController.GetIsRegularItem(AItem: TdxTileControlItem): Boolean;
begin
  Result := ((AItem as TdxTileBarItem).Size = tbisRegular) and (AItem.RowCount = 1);
end;

procedure TdxTileBarController.PopulateOneGroupDesignPopupMenu;
const
  AMenuItemCaption: array[Low(TdxTileBarItemSize)..High(TdxTileBarItemSize)] of string = ('Add Regular Item', 'Add Large Item');
var
  I: TdxTileBarItemSize;
begin
  for I := Low(TdxTileBarItemSize) to High(TdxTileBarItemSize) do
    AddDesignPopupMenuItem(AMenuItemCaption[I], Integer(I), 0, False, False, CreateTileControlItem);
end;

procedure TdxTileBarController.PopulateOneItemDesignPopupMenu;
const
  AMenuItemCaption: array[Low(TdxTileBarItemSize)..High(TdxTileBarItemSize)] of string = ('Regular', 'Large');
var
  I: TdxTileBarItemSize;
  AItem: TdxTileControlItem;
begin
  AItem := DesignPopupMenuCaller as TdxTileControlItem;
  for I := Low(TdxTileBarItemSize) to High(TdxTileBarItemSize) do
    AddDesignPopupMenuItem(AMenuItemCaption[I], Integer(I), 1, IsDesignerMenuItemChecked(Integer(I), AItem), True, SwitchItemsProperty);
end;

procedure TdxTileBarController.SetDesignItemSize(AItem: TdxTileControlItem; AMenuItemTag: Longint);
begin
  TdxTileBarItem(AItem).Size := TdxTileBarItemSize(AMenuItemTag);
end;

{ TdxTileBarHitTest }

function TdxTileBarHitTest.GetActiveViewInfo: TdxTileControlCustomViewInfo;
begin
  Result := TileBar.ViewInfo;
end;

function TdxTileBarHitTest.GetDropDownButton: TdxTileBarItemDropDownButtonViewInfo;
begin
  if HitObject is TdxTileBarItemDropDownButtonViewInfo then
    Result := TdxTileBarItemDropDownButtonViewInfo(HitObject)
  else
    Result := nil;
end;

function TdxTileBarHitTest.GetTileBar: TdxCustomTileBar;
begin
  Result := TileControl as TdxCustomTileBar;
end;

{ TdxCustomTileBar }

function TdxCustomTileBar.ActiveDetailOccupiesAllTileControl: Boolean;
begin
  Result := False;
end;

procedure TdxCustomTileBar.AfterActiveDetailChangingAnimation(AActiveDetail, ANewDetail: TdxTileControlDetailSite);
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
    CheckDetailLayout(ANewDetail);
    ANewDetail.InvalidateWithChildren;
    if not JustLoaded then
      if ANewDetail.ActiveControl <> nil then
        if ANewDetail.ActiveControl.CanFocus then
          ANewDetail.ActiveControl.SetFocus
        else
      else
        if ANewDetail.CanFocus then
          ANewDetail.SetFocus;
  end;
end;

procedure TdxCustomTileBar.CheckChanges;
begin
  ForceCalculate := True;
  try
    inherited CheckChanges;
  finally
    ForceCalculate := False;
  end;
end;

procedure TdxCustomTileBar.CheckDesignTimeAlign;
const
  DesignTimeAlign: array[TdxTileBarPosition] of TAlign = (alTop, alBottom, alLeft, alRight);
begin
  if IsDesigning then
    Align := DesignTimeAlign[OptionsView.Position];
end;

procedure TdxCustomTileBar.CheckDesignTimeSize;
begin
  if IsDesigning then
    if OptionsView.GroupLayout = glHorizontal then
      Height := ViewInfo.TitleHeight + cxRectHeight(ViewInfo.TilesZone)
    else
      Width := cxRectWidth(ViewInfo.TilesZone);
end;

procedure TdxCustomTileBar.CheckDetailLayout(ADetail: TdxTileControlDetailSite);
var
  AArea: TRect;
begin
  ADetail.Align := alNone;
  AArea := ViewInfo.DetailSiteArea;
  ADetail.SetBounds(AArea.Left, AArea.Top, cxRectWidth(AArea), cxRectHeight(AArea));
end;

function TdxCustomTileBar.CreateActionBarTop: TdxTileControlCustomActionBar;
begin
  Result := TdxTileBarTopActionBar.Create(Self);
end;

function TdxCustomTileBar.CreateController: TdxTileControlController;
begin
  Result := TdxTileBarController.Create(Self);
end;

function TdxCustomTileBar.CreatePopupWindow: TdxTileBarPopupWindow;
begin
  Result := TdxTileBarPopupWindow.Create(Self);
end;

function TdxCustomTileBar.CreateHitTest: TdxTileControlHitTest;
begin
  Result := TdxTileBarHitTest.Create(Self);
end;

function TdxCustomTileBar.CreatePainter: TdxTileControlPainter;
begin
  Result := TdxTileBarPainter.Create(Self);
end;

function TdxCustomTileBar.CreateItem(ASize: TdxTileBarItemSize; AGroup: TdxTileControlGroup = nil): TdxTileBarItem;
begin
  BeginUpdate;
  try
    if ASize = tbisRegular then
      Result := TdxTileBarItem(inherited CreateItem(tcisRegular, AGroup))
    else
      Result := TdxTileBarItem(inherited CreateItem(tcisLarge, AGroup));
    Result.Size := ASize;
  finally
    EndUpdate;
  end;
  Result.MakeVisible;
end;

function TdxCustomTileBar.CreateItemsCollection: TdxTileControlItemCollection;
begin
  Result := TdxTileControlItemCollection.Create(Self, TdxTileBarItem);
end;

function TdxCustomTileBar.CreateViewInfo: TdxTileControlViewInfo;
begin
  Result := TdxTileBarViewInfo.Create(Self);
end;

procedure TdxCustomTileBar.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FQueuePrevDetails := TcxObjectList.Create(False);
  if not IsLoading then
  begin
    ViewInfo.Calculate;
    CheckDesignTimeSize;
  end;
end;

procedure TdxCustomTileBar.DestroySubClasses;
begin
  FreeAndNil(FQueuePrevDetails);
  FreeAndNil(FPopupWindow);
  inherited DestroySubClasses;
end;

procedure TdxCustomTileBar.DoActivateDetail(ADetail: TdxTileControlDetailSite);
begin
  ExpandQueuePrevDetails;
  inherited DoActivateDetail(ADetail);
end;

procedure TdxCustomTileBar.DoCustomPaint;
begin
  ViewInfo.Draw(Canvas);
  if Animation <> nil then
    Animation.Draw(Canvas.Canvas, ViewInfo.DetailSiteArea);
end;

procedure TdxCustomTileBar.DoLoaded;
begin
  inherited DoLoaded;
  CheckDesignTimeAlign;
  ViewInfo.Calculate;
  if not IsDesigning and (FocusedItem <> nil) and FocusedItem.Visible and FocusedItem.Enabled and
      FocusedItem.DetailOptions.HasDetail then
    FocusedItem.ActivateDetail;
end;

procedure TdxCustomTileBar.ExecuteDetailDeactivating(ADetail: TdxTileControlDetailSite);
var
  APrevDetail: TdxTileBarDetailSite;
  APrevItem: TdxTileBarItem;
begin
  FDeactivatingDetailInProcess := True;
  try
    if QueuePrevDetails.Count > 0 then
    begin
      APrevDetail := QueuePrevDetails[QueuePrevDetails.Count - 1] as TdxTileBarDetailSite;
      QueuePrevDetails.Remove(APrevDetail);
      APrevItem := APrevDetail.TileItem as TdxTileBarItem;
      if APrevItem.IsEnabled and APrevItem.DetailOptions.HasDetail then
      begin
        Controller.FocusedItem := APrevItem;
        APrevItem.ActivateDetail;
      end
      else
        ActiveDetail := nil;
    end
    else
      ActiveDetail := nil;
  finally
    FDeactivatingDetailInProcess := False;
  end;
end;

procedure TdxCustomTileBar.ExpandQueuePrevDetails;
begin
  if (ActiveDetail <> nil) and not FDeactivatingDetailInProcess then
    FQueuePrevDetails.Add(ActiveDetail);
end;

function TdxCustomTileBar.GetActiveHitTest: TdxTileControlHitTest;
begin
  Result := inherited GetActiveHitTest;
  if (Result = HitTest) and (ActiveDetail <> nil) then
  begin
    ActiveDetail.HitTest.HitPoint := ActiveDetail.ScreenToClient(ActiveHitTestPos);
    if ActiveDetail.HitTest.HitObject <> nil then
      Result := ActiveDetail.HitTest;
  end;
end;

function TdxCustomTileBar.GetAnimationScrollFadeMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode;
begin
  Result := inherited GetAnimationScrollFadeMode(AIndex1, AIndex2);
  if GroupLayout = glVertical then
    Result := TdxDrawAnimationMode(Integer(Result) + 1);
end;

function TdxCustomTileBar.GetAnimationScrollMode(AIndex1, AIndex2: Integer): TdxDrawAnimationMode;
begin
  Result := inherited GetAnimationScrollMode(AIndex1, AIndex2);
  if GroupLayout = glVertical then
    Result := TdxDrawAnimationMode(Integer(Result) + 1);
end;

function TdxCustomTileBar.GetController: TdxTileBarController;
begin
  Result := inherited Controller as TdxTileBarController;
end;

function TdxCustomTileBar.GetPopupWindow: TdxTileBarPopupWindow;
begin
  if FPopupWindow = nil then
    FPopupWindow := CreatePopupWindow;
  Result := FPopupWindow;
end;

function TdxCustomTileBar.GetFocusedItem: TdxTileBarItem;
begin
  Result := Controller.FocusedItem as TdxTileBarItem;
end;

function TdxCustomTileBar.GetHitTest: TdxTileBarHitTest;
begin
  Result := inherited HitTest as TdxTileBarHitTest;
end;

function TdxCustomTileBar.GetHScrollBarBounds: TRect;
begin
  Result := ViewInfo.TilesZone;
  Result.Top := Result.Bottom - HScrollBar.Height;
  if OptionsView.FixedIndentHorz then
    InflateRect(Result, -OptionsView.IndentHorz, 0);
end;

function TdxCustomTileBar.GetVScrollBarBounds: TRect;
begin
  Result := ViewInfo.TilesZone;
  Result.Left := Result.Right - VScrollBar.Width;
  if OptionsView.FixedIndentVert then
    InflateRect(Result, 0, -OptionsView.IndentVert);
end;

function TdxCustomTileBar.GetGroupLayout: TdxTileControlGroupLayout;
begin
  Result := OptionsView.GroupLayout;
end;

function TdxCustomTileBar.GetOptionsBehaviorClass: TdxTileControlOptionsBehaviorClass;
begin
  Result := TdxTileBarOptionsBehavior;
end;

function TdxCustomTileBar.GetOptionsViewClass: TdxTileControlOptionsViewClass;
begin
  Result := TdxTileBarOptionsView;
end;

function TdxCustomTileBar.GetOptionsBehavior: TdxTileBarOptionsBehavior;
begin
  Result := inherited OptionsBehavior as TdxTileBarOptionsBehavior;
end;

function TdxCustomTileBar.GetOptionsView: TdxTileBarOptionsView;
begin
  Result := inherited OptionsView as TdxTileBarOptionsView;
end;

procedure TdxCustomTileBar.InitializeAlign;
begin
  if IsDesigning then
    Align := alTop
  else
    inherited InitializeAlign;
end;

procedure TdxCustomTileBar.InvalidateChanges;
begin
  Invalidate;
  if ActiveDetail <> nil then
    ActiveDetail.Invalidate;
end;

function TdxCustomTileBar.NeedStopAnimationsBeforeActiveDetailChanging(AActiveDetail: TdxTileControlDetailSite): Boolean;
begin
  Result := False;
end;

procedure TdxCustomTileBar.SetFocusedItem(AItem: TdxTileBarItem);
begin
  Controller.FocusedItem := AItem;
end;

procedure TdxCustomTileBar.SetOptionsBehavior(AValue: TdxTileBarOptionsBehavior);
begin
  inherited SetOptionsBehavior(AValue)
end;

procedure TdxCustomTileBar.SetOptionsView(AValue: TdxTileBarOptionsView);
begin
  inherited SetOptionsView(AValue)
end;

procedure TdxCustomTileBar.HidePopupWindow;
begin
  if PopupWindow.Visible then
  begin
    PopupWindow.CloseUp;
    PopupWindow.SetLastActualBarItem(nil);
  end;
end;

procedure TdxCustomTileBar.ShowPopupWindow(AItem: TdxTileBarItem);
var
  AAllow: Boolean;
begin
  if (AItem <> nil) and AItem.ViewInfo.Visible and (PopupWindow.FLastActualBarItem <> AItem) then
  begin
    AItem.MakeVisible;
    PopupWindow.BiDiMode := BiDiMode;
    AAllow := True;
    if Assigned(OnPopupActivating) then
      OnPopupActivating(Self, AItem, AAllow);
    if AAllow then
    begin
      PopupWindow.ActualBarItem := AItem;
      PostMessage(PopupWindow.Handle, DXM_SHOWPOPUPWINDOW, 0, 0);
    end;
  end;
end;

{ TdxTileControlTopActionBar }

function TdxTileBarTopActionBar.CreateViewInfo: TdxTileControlCustomActionBarViewInfo;
begin
  Result := TdxTileBarActionBarViewInfo.Create(Self);
end;

{ TdxTileBarActionBarViewInfo }

function TdxTileBarActionBarViewInfo.GetCanDisplayItem(AItem: TdxTileControlActionBarItem): Boolean;
begin
  Result := inherited GetCanDisplayItem(AItem) and (AItem.Position = abipTopBar);
end;

end.

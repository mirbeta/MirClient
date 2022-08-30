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

unit cxGridCustomView;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Classes, Graphics,
  Forms, Controls {after Forms for D12},
  StdCtrls, Menus, ImgList, dxCore, dxTypeHelpers,
  dxMessages, dxCoreClasses, cxClasses, cxControls, cxContainer, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, dxCustomHint, cxStyles, cxStorage,
  cxGridCommon, cxCustomData, cxData, cxListBox, cxPC, cxEdit, dxTouch,
  dxIncrementalFiltering, dxUIElementPopupWindow, cxGeometry, dxForms;

const
  cxGridCustomizationFormDefaultWidth = 180;
  cxGridCustomizationFormDefaultHeight = 300;

  htError = -1;
  htNone = 0;
  htNavigator = 1;
  htCustomizationForm = 2;
  htDesignSelector = 3;

  ckNone = 0;
  ckCustomizationForm = 1;

  bbCustomFirst = 0;
  bbBackground = bbCustomFirst;
  bbCustomLast = bbBackground;

  vsCustomFirst = 0;
  vsBackground = vsCustomFirst;
  vsCustomLast = vsBackground;

  StoringVersion = 1;

type
  TcxCustomGridDragAndDropObjectClass = class of TcxCustomGridDragAndDropObject;
  TcxCustomGridItemsListBox = class;
  TcxCustomGridCustomizationForm = class;
  TcxGridPopupListBox = class;
  TcxCustomGridController = class;
  TcxCustomGridPainter = class;
  TcxCustomGridViewData = class;
  TcxCustomGridCellViewInfo = class;
  TcxCustomGridViewCellViewInfo = class;
  TcxGridDesignSelectorViewInfo = class;
  TcxCustomGridViewInfo = class;
  TcxCustomGridViewInfoCache = class;
  TcxGridSite = class;
  TcxCustomGridView = class;
  TcxCustomGridOptionsBehavior = class;

  TcxGridShowLockedStateImageMode = type TcxLockedStateImageShowingMode;//(lsimNever, lsimPending, lsimImmediate);

  IcxGridViewLayoutEditorSupport = interface  // it is here because of problem in CBuilder
    ['{9C5EC9C0-A912-4822-BBD0-87AB45FDCC78}']
    procedure BeforeEditLayout(ALayoutView: TcxCustomGridView);
    function CanEditViewLayoutAndData: Boolean;
    procedure DoAssignLayout(ALayoutView: TcxCustomGridView);
    function GetLayoutCustomizationFormButtonCaption: string;
    function HasLayoutCustomizationForm: Boolean;
    function IsLayoutChangeable: Boolean;
    procedure RunLayoutCustomizationForm;
  end;

  { change }

  TcxCustomGridViewChange = class(TcxCustomGridChange)
  private
    FGridView: TcxCustomGridView;
  public
    constructor Create(AGridView: TcxCustomGridView); virtual;
    property GridView: TcxCustomGridView read FGridView write FGridView;
    function IsEqual(AChange: TcxCustomGridChange): Boolean; override;
  end;

  TcxGridControlFocusChange = class(TcxCustomGridViewChange)
  public
    procedure Execute; override;
  end;

  { hit tests }

  TcxCustomGridHitTestClass = class of TcxCustomGridHitTest;

  TcxCustomGridHitTest = class
  private
    FIsClone: Boolean;
    FPos: TPoint;
    FViewInfo: TcxCustomGridCellViewInfo;
    procedure SetViewInfo(Value: TcxCustomGridCellViewInfo);
  protected
    procedure Assign(Source: TcxCustomGridHitTest); virtual;
    class function GetHitTestCode: Integer; virtual;
    procedure Init(const APos: TPoint);
  public
    destructor Destroy; override;
    function Clone: TcxCustomGridHitTest;
    function Cursor: TCursor; virtual;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; virtual;
    class function HitTestCode: Integer;
    class function Instance(const APos: TPoint): TcxCustomGridHitTest;

    property Pos: TPoint read FPos;
    property ViewInfo: TcxCustomGridCellViewInfo read FViewInfo write SetViewInfo;
  end;

  TcxGridNoneHitTest = class(TcxCustomGridHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxCustomGridViewHitTest = class(TcxCustomGridHitTest)
  private
    FGridView: TcxCustomGridView;
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
  public
    property GridView: TcxCustomGridView read FGridView write FGridView;
  end;

  TcxGridViewNoneHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridNavigatorHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridCustomizationFormHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridDesignSelectorHitTest = class(TcxCustomGridViewHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function Cursor: TCursor; override;
  end;

  { custom handler }

  TcxGridViewHandler = class(TcxInterfacedPersistent)
  private
    FGridView: TcxCustomGridView;
    function GetControl: TcxControl; inline;
    function GetController: TcxCustomGridController;
    function GetDataController: TcxCustomDataController; inline;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
    function GetPainter: TcxCustomGridPainter; inline;
    function GetSite: TcxGridSite;
    function GetViewData: TcxCustomGridViewData;
    function GetViewInfo: TcxCustomGridViewInfo;
  protected
    property Control: TcxControl read GetControl;
    property Controller: TcxCustomGridController read GetController;
    property DataController: TcxCustomDataController read GetDataController;
    property ViewData: TcxCustomGridViewData read GetViewData;
  public
    constructor Create(AGridView: TcxCustomGridView); reintroduce; virtual;
    procedure BeginUpdate(AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
    procedure EndUpdate;
    function UseRightToLeftAlignment: Boolean;
    property GridView: TcxCustomGridView read FGridView;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Painter: TcxCustomGridPainter read GetPainter;
    property Site: TcxGridSite read GetSite;
    property ViewInfo: TcxCustomGridViewInfo read GetViewInfo;
  end;

  { controller }

  // drag & drop objects

  TcxCustomGridDragAndDropObject = class(TcxDragAndDropObject)
  private
    function GetController: TcxCustomGridController;
    function GetGridView: TcxCustomGridView;
    function GetViewInfo: TcxCustomGridViewInfo;
  protected
    procedure AfterDragAndDrop(Accepted: Boolean); override;
    procedure AfterPaint; virtual;
    procedure BeforePaint; virtual;
    property Controller: TcxCustomGridController read GetController;
    property ViewInfo: TcxCustomGridViewInfo read GetViewInfo;
  public
    SourcePoint: TPoint;
    procedure AfterScrolling; virtual;
    procedure BeforeScrolling; virtual;
    procedure AfterViewChange; virtual;
    procedure BeforeViewChange; virtual;
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); virtual;
    property GridView: TcxCustomGridView read GetGridView;
  end;

  TcxGridArrowNumber = (anFirst, anLast);
  TcxGridArrowPlace = TcxArrowPlace;

  TcxCustomGridMovingObject = class(TcxCustomGridDragAndDropObject)
  private
    FDragImage: TcxDragImage;
    FSourceItem: TObject;
    function GetArrowPlace(AArrowNumber: TcxGridArrowNumber): TcxGridArrowPlace;
    function GetCustomizationForm: TcxCustomGridCustomizationForm;
    function HasArrows: Boolean;
  protected
    Arrows: array[TcxGridArrowNumber] of TcxDragAndDropArrow;

    procedure DirtyChanged; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;

    procedure ChangeArrowsPosition(AVisible: Boolean = True);
    procedure ChangeDragImagePosition(AVisible: Boolean = True);

    function AreArrowsVertical: Boolean; virtual;
    function CanRemove: Boolean; virtual; abstract;
    function GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect; virtual; abstract;
    function GetArrowClass: TcxDragAndDropArrowClass; virtual;
    function GetArrowsClientRect: TRect; virtual;
    function GetCustomizationFormListBox: TcxCustomGridItemsListBox; virtual; abstract;
    function GetDragImageClass: TcxDragImageClass; virtual;
    function GetSourceItemBounds: TRect; virtual;
    function GetSourceItemViewInfo: TcxCustomGridCellViewInfo; virtual;
    procedure InitDragImage; virtual;
    procedure InitDragImageUsingCustomizationForm(ACanvas: TcxCanvas;
      const R: TRect; AItem: TObject); virtual;
    procedure InitDragObjects; virtual;
    function IsSourceCustomizationForm: Boolean; virtual; abstract;
    function IsValidDestination: Boolean; virtual; abstract;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    property ArrowPlaces[AArrowNumber: TcxGridArrowNumber]: TcxGridArrowPlace read GetArrowPlace;
    property ArrowsClientRect: TRect read GetArrowsClientRect;
    property CustomizationForm: TcxCustomGridCustomizationForm read GetCustomizationForm;
    property CustomizationFormListBox: TcxCustomGridItemsListBox read GetCustomizationFormListBox;
    property DragImage: TcxDragImage read FDragImage;
    property SourceItem: TObject read FSourceItem write FSourceItem;
    property SourceItemBounds: TRect read GetSourceItemBounds;
    property SourceItemViewInfo: TcxCustomGridCellViewInfo read GetSourceItemViewInfo;
  public
    procedure AfterScrolling; override;
  end;

  // customization form

  TcxCustomGridItemsInnerListBox = class(TcxInnerListBox)
  private
    FDragAndDropItemIndex: Integer;
    FMouseDownPos: TPoint;
    function GetContainer: TcxCustomGridItemsListBox;
    function GetDragAndDropItem: TObject;
    function GetGridView: TcxCustomGridView;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawBorder(ACanvas: TCanvas; R: TRect); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure CalculateBorderStyle;
    procedure CalculateItemHeight;

    property Container: TcxCustomGridItemsListBox read GetContainer;
    property DragAndDropItem: TObject read GetDragAndDropItem;
    property DragAndDropItemIndex: Integer read FDragAndDropItemIndex;
    property GridView: TcxCustomGridView read GetGridView;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TcxCustomGridItemsListBoxClass = class of TcxCustomGridItemsListBox;

  TcxCustomGridItemsListBox = class(TcxListBox)
  private
    function GetDragAndDropItem: TObject;
    function GetGridView: TcxCustomGridView;
    function GetInnerListBox: TcxCustomGridItemsInnerListBox;
  protected
    function CalculateItemHeight: Integer; virtual; abstract;
    procedure DoRefreshItems; virtual; abstract;
    function GetInnerListBoxClass: TcxInnerListBoxClass; override;
    function GetDragAndDropParams: TcxCustomGridHitTest; virtual; abstract;
    property DragAndDropItem: TObject read GetDragAndDropItem;
    property GridView: TcxCustomGridView read GetGridView;
  public
    constructor Create(AOwner: TComponent); override;
    function IndexOfItem(AItem: TObject): Integer;
    procedure PaintDragAndDropItem(ACanvas: TcxCanvas; const R: TRect; AItem: TObject);
    procedure PaintItem(ACanvas: TcxCanvas; R: TRect; AIndex: Integer; AFocused: Boolean); virtual; abstract;
    procedure RefreshItems;
    property InnerListBox: TcxCustomGridItemsInnerListBox read GetInnerListBox;
  end;

  { IcxGridCustomizationForm }

  IcxGridCustomizationForm = interface
  ['{D702C868-B1BC-41B6-BDD0-5CF5030E03C3}']
    procedure GridViewChanged;
    function GetController: TcxCustomGridController;
    procedure Initialize(AController: TcxCustomGridController);
    procedure RefreshData;

    property Controller: TcxCustomGridController read GetController;
  end;

  TcxCustomGridCustomizationFormClass = class of TcxCustomGridCustomizationForm;

  TcxCustomGridCustomizationForm = class(TdxForm, IcxGridCustomizationForm)
  private
    FController: TcxCustomGridController;
    FHookTimer: TcxTimer;
    FOffset: Integer;
    FPageControl: TcxPageControl;
    function GetGridView: TcxCustomGridView;
    function GetViewInfo: TcxCustomGridViewInfo;
    procedure HookTimerHandler(Sender: TObject);
  protected
    // IcxGridCustomizationForm
    procedure GridViewChanged; virtual;
    function GetController: TcxCustomGridController;
    procedure Initialize(AController: TcxCustomGridController);

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    function CreatePage(const ACaption: string; AVisible: Boolean): TcxTabSheet;

    procedure CalculateConsts; virtual;
    procedure CreateControls; virtual;

    function GetContentBounds: TRect; virtual;
    function GetPageControlBounds: TRect; virtual;
    function HasDFM: Boolean; virtual;
    procedure InitPageControl; virtual;

    property ContentBounds: TRect read GetContentBounds;
    property GridView: TcxCustomGridView read GetGridView;
    property Offset: Integer read FOffset write FOffset;
    property PageControl: TcxPageControl read FPageControl;
    property ViewInfo: TcxCustomGridViewInfo read GetViewInfo;
  public
    constructor Create(AController: TcxCustomGridController); reintroduce; virtual;
    destructor Destroy; override;
    property Controller: TcxCustomGridController read FController;
    procedure ActivatePage(APage: TcxTabSheet);
    procedure RefreshData; virtual;
  end;

  // popup

  IcxCustomGridPopupOwner = IdxUIElementPopupWindowOwner;

  TcxCustomGridPopup = class(TdxUIElementPopupWindow)
  private
    FGridView: TcxCustomGridView;
  public
    constructor Create(AGridView: TcxCustomGridView); reintroduce; virtual;

    property GridView: TcxCustomGridView read FGridView;
  end;

  TcxGridIndexes = TdxIntegerIndexes;

  TcxGridPopupListBox = class(TdxCustomCheckListBox)
  strict private
    function GetPopup: TcxCustomGridPopup;
  protected
    procedure DoItemAction(AItemIndex: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function NeedHotTrack: Boolean; override;
  public
    constructor Create(APopup: TcxCustomGridPopup); reintroduce; virtual;

    property Popup: TcxCustomGridPopup read GetPopup;
  end;

  TcxGridPopupListBoxClass = class of TcxGridPopupListBox;

  // controllers

  TcxCustomGridDesignController = class
  protected
    function GetControl: TcxControl; virtual; abstract;
    function GetDesignObject(AObject: TPersistent): TPersistent; virtual;
  public
    function CanAddComponent: Boolean;
    function CanDeleteComponent(AComponent: TComponent): Boolean;
    procedure GetSelection(AList: TList);
    function IsObjectSelected(AObject: TPersistent): Boolean;
    procedure NotifyEditors;
    procedure SelectObject(AObject: TPersistent; AClearSelection: Boolean);
    procedure SelectObjects(AObjects: TList);
    procedure UnselectObject(AObject: TPersistent);
    property Control: TcxControl read GetControl;  // TcxCustomGrid
  end;

  TcxGridViewDesignControllerClass = class of TcxGridViewDesignController;

  TcxGridViewDesignController = class(TcxCustomGridDesignController)
  private
    FController: TcxCustomGridController;
  protected
    function GetControl: TcxControl; override;
    function GetDesignObject(AObject: TPersistent): TPersistent; override;
  public
    constructor Create(AController: TcxCustomGridController); virtual;
    property Controller: TcxCustomGridController read FController;
  end;

  { TcxGridHintHelper }

  TcxGridHintHelper = class(TcxControlHintHelper)
  private
    FController: TcxCustomGridController;
    function GetOptionsBehavior: TcxCustomGridOptionsBehavior;
  protected
    function GetHintControl: TcxControl; override;
    function GetHintHidePause: Integer;  override;
    function GetHintWindowClass: THintWindowClass; override;
    function GetOwnerControl: TcxControl; override;
    function IsSuppressHintOnMouseDown: Boolean; override;

    property OptionsBehavior: TcxCustomGridOptionsBehavior read GetOptionsBehavior;
  public
    constructor Create(AController: TcxCustomGridController); virtual;
  end;

  TcxGridHintHelperClass = class of TcxGridHintHelper;

  TcxCustomGridController = class(TcxGridViewHandler)
  private
    FCustomization: Boolean;
    FCustomizationForm: TForm;
    FCustomizationFormBounds: TRect;
    FDesignController: TcxGridViewDesignController;
    FDesignPopupMenuInvoker: TcxCustomGridCellViewInfo;
    FHintHelper: TcxGridHintHelper;
    FIsCheckingCoordinates: Boolean;
    FIsDblClick: Boolean;
    FIsFocusing: Boolean;
    FIsScrolling: Boolean;
    function GetDesignController: TcxGridViewDesignController;
    function GetDragAndDropObject: TcxCustomGridDragAndDropObject;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
    function GetDragImages: TcxDragImageList;
    function GetHintCellViewInfo: TcxCustomGridViewCellViewInfo;
    function GetHintWindow: THintWindow;
    function GetICustomizationForm: IcxGridCustomizationForm;
    function GetIsDragging: Boolean;
    function GetMouseCaptureViewInfo: TcxCustomGridCellViewInfo;
    procedure SetCustomization(Value: Boolean);
    procedure SetDragAndDropObjectClass(Value: TcxDragAndDropObjectClass);
    procedure SetMouseCaptureViewInfo(Value: TcxCustomGridCellViewInfo);
  protected
    procedure AfterPaint; virtual;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; virtual;
    procedure BeforePaint; virtual;
    function CanCancelDragStartOnCaptureObjectClear: Boolean; virtual;
    function CanFocusOnClick(X, Y: Integer): Boolean; virtual;
    function CanHandleHitTest(AHitTest: TcxCustomGridHitTest): Boolean; virtual;
    procedure CheckCoordinates; virtual;
    function CreateFieldControls(X, Y: Integer; ADataSource: TObject; AFieldList: TList): Boolean; virtual;
    procedure DetailFocused(ADetail: TcxCustomGridView); virtual;
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    procedure DoSetFocus(ANotifyMaster: Boolean); virtual;
    function GetActiveCellViewInfo(AHitTest: TcxCustomGridHitTest): TcxCustomGridCellViewInfo;
    function GetDesignControllerClass: TcxGridViewDesignControllerClass; virtual;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; virtual;
    function GetDlgCode: Integer; virtual;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; virtual;
    function GetPatternObject(AObject: TPersistent): TPersistent; virtual;
    procedure GridViewChanged; virtual;
    function IsPixelScrollBar(AKind: TScrollBarKind): Boolean; virtual;
    function IsSuppressHintOnMouseDown: Boolean;
    function MayFocus: Boolean; virtual;
    procedure MouseLeave; virtual;
    function ProcessDesignPopupMenu(AViewInfo: TcxCustomGridCellViewInfo): Boolean; virtual;
    procedure RemoveFocus; virtual;
    procedure SetFocus(ANotifyMaster: Boolean); virtual;
    procedure SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer); virtual;
    procedure VisibilityChanged(AVisible: Boolean); virtual;
    function WantSpecialKey(AKey: Word): Boolean; virtual;
    // customization
    function CanCustomize: Boolean; virtual;
    procedure CheckCustomizationFormBounds(var R: TRect); virtual;
    function CreateCustomizationForm: TForm; virtual;
    procedure CustomizationChanged; virtual;
    procedure DoCreateCustomizationForm; virtual;
    procedure DoCustomization;
    function GetCustomizationFormBounds: TRect; virtual;
    function GetCustomizationFormClass: TcxCustomGridCustomizationFormClass; virtual;
    function GetCustomizationFormDefaultWidth: Integer; virtual;
    function GetCustomizationFormDefaultHeight: Integer; virtual;
    procedure HideCustomizationForm;
    procedure InitializeCustomizationForm(AForm: TForm); virtual;
    procedure ShowCustomizationForm;
    //hints
    function GetHintHelperClass: TcxGridHintHelperClass; virtual;
    //scrolling
    procedure AfterScrolling; virtual;
    procedure BeforeScrolling; virtual;
    procedure BeginGestureScroll(APos: TPoint); virtual;
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); virtual;
    procedure EndGestureScroll; virtual;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;

    property HintHelper: TcxGridHintHelper read FHintHelper;
    property ICustomizationForm: IcxGridCustomizationForm read GetICustomizationForm;
    property DesignPopupMenuInvoker: TcxCustomGridCellViewInfo read FDesignPopupMenuInvoker;
    property DragAndDropObject: TcxCustomGridDragAndDropObject read GetDragAndDropObject;
    property IsCheckingCoordinates: Boolean read FIsCheckingCoordinates;
    property IsDragging: Boolean read GetIsDragging;
    property IsFocusing: Boolean read FIsFocusing;
    property IsScrolling: Boolean read FIsScrolling write FIsScrolling;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure ControlFocusChanged; virtual;
    procedure DesignerModified;
    procedure DoCancelMode; virtual;
    procedure DoCheckCoordinates;
    procedure DoControlFocusChanged;
    function GetCursor(X, Y: Integer): TCursor; virtual;
    function HasFocusedControls: Boolean; virtual;

    procedure InitScrollBarsParameters; virtual;
    function IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); virtual;
    procedure SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
      AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
    procedure UpdateScrollBars(AIgnoreUpdateLock: Boolean = False);

    procedure BeginDragAndDrop; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; virtual;

    // delphi drag and drop
    procedure BeforeStartDrag; virtual;
    function CanDrag(X, Y: Integer): Boolean; virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure DrawDragImage(ACanvas: TcxCanvas; R: TRect); virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    function GetDragImagesSize: TPoint; virtual;
    function HasDragImages: Boolean; virtual;
    procedure StartDrag(var DragObject: TDragObject); virtual;
    property DragImages: TcxDragImageList read GetDragImages;

    // hints
    procedure CancelHint;
    procedure HideHint;
    function GetHintWindowClass: THintWindowClass; virtual;
    procedure ShowHint(const AHintAreaBounds, ATextRect: TRect; const AText: string;
      AIsHintMultiLine: Boolean; AFont: TFont; AHintCellViewInfo: TcxCustomGridViewCellViewInfo);
    property HintCellViewInfo: TcxCustomGridViewCellViewInfo read GetHintCellViewInfo;
    property HintWindow: THintWindow read GetHintWindow;

    // keyboard
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
    function IMEComposition(var AMessage: TMessage): Boolean; virtual;
    function IMEStartComposition: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    function ProcessDetailDialogChar(ADetail: TcxCustomGridView; ACharCode: Word): Boolean; virtual;
    function ProcessDialogChar(ACharCode: Word): Boolean; virtual;
    procedure WndProc(var Message: TMessage); virtual;
    procedure RefreshCustomizationForm;

    property Customization: Boolean read FCustomization write SetCustomization;
    property CustomizationForm: TForm read FCustomizationForm;
    property CustomizationFormBounds: TRect read FCustomizationFormBounds write FCustomizationFormBounds;
    property DesignController: TcxGridViewDesignController read GetDesignController;
    property DragAndDropObjectClass: TcxDragAndDropObjectClass read GetDragAndDropObjectClass
      write SetDragAndDropObjectClass;
    property IsDblClick: Boolean read FIsDblClick;
    property MouseCaptureViewInfo: TcxCustomGridCellViewInfo read GetMouseCaptureViewInfo
      write SetMouseCaptureViewInfo;
  end;

  TcxCustomGridControllerClass = class of TcxCustomGridController;

  { DataController }

  // sorting by summary

  TcxDataGroupNode = class
  private
    FChildren: TdxFastObjectList;
    FDataGroupInfo: TcxDataGroupInfo;
    FParent: TcxDataGroupNode;
    function GetChild(Index: Integer): TcxDataGroupNode;
    function GetChildCount: Integer;
  protected
    procedure SortChildren(ACompare: TCompareItems);
  public
    constructor Create(AParent: TcxDataGroupNode; ADataGroupInfo: TcxDataGroupInfo);
    destructor Destroy; override;
    function AddChild(ADataGroupInfo: TcxDataGroupInfo): TcxDataGroupNode;

    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TcxDataGroupNode read GetChild; default;
    property DataGroupInfo: TcxDataGroupInfo read FDataGroupInfo;
    property Parent: TcxDataGroupNode read FParent;
  end;

  TcxSortingBySummaryInfo = class
  public
    SortOrder: TcxDataSortOrder;
    SummaryItemIndex: Integer;
  end;

  TcxGridSortingBySummaryEngine = class(TcxSortingBySummaryEngine)
  private
    FInfos: TdxFastObjectList;
    FRootNode: TcxDataGroupNode;
    function GetDataGroups: TcxDataGroups;
    function GetInfo(Index: Integer): TcxSortingBySummaryInfo;
    function GetInfoCount: Integer;
  protected
    procedure AddInfo(AInfo: TcxSortingBySummaryInfo);
    procedure BuildNodes(AParentNode: TcxDataGroupNode; ALevel: Integer; var ACurIndex: Integer);
    procedure ClearInfos;
    function CompareGroupsBySummary(AInfo1, AInfo2: Pointer): Integer;
    procedure RebuildDataGroupRecursive(AParentNode: TcxDataGroupNode; var AFirstRecordListIndex: Integer);
    procedure RebuildDataGroups;
    procedure SortNodeRecursive(ANode: TcxDataGroupNode; ALevel: Integer);
    procedure SortNodes;

    property DataGroups: TcxDataGroups read GetDataGroups;
    property InfoCount: Integer read GetInfoCount;
    property Infos[Index: Integer]: TcxSortingBySummaryInfo read GetInfo;
  public
    constructor Create(ADataControllerInfo: TcxCustomDataControllerInfo); override;
    destructor Destroy; override;
    procedure Sort; override;
  end;

  // interface to data controller

  IcxCustomGridDataController = interface
    ['{B9ABDC6B-1A4A-4F11-A629-09B6FB9FB4BA}']
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure CreateAllItems(AMissingItemsOnly: Boolean);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;
    property GridView: TcxCustomGridView read GetGridView;
  end;

  { painters }

  TcxCustomGridCellPainterClass = class of TcxCustomGridCellPainter;

  TcxCustomGridCellPainter = class
  strict private
    FCanvas: TcxCanvas;
    FViewInfo: TcxCustomGridCellViewInfo;

    function GetIsMainCanvasInUseValue: Boolean;
    function GetScaleFactor: TdxScaleFactor;
  protected
    procedure AfterPaint; virtual;
    procedure BeforePaint; virtual;
    function CanDrawDesignSelection: Boolean; virtual;
    procedure DoExcludeFromClipRect; virtual;
    procedure DrawBackground; overload; virtual;
    procedure DrawBackground(const R: TRect); overload; virtual;
    function DrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual;
    procedure DrawBorder(ABorder: TcxBorder); virtual;
    procedure DrawBorders; virtual;
    procedure DrawContent; virtual;
    class procedure DrawDesignSelection(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo); virtual;
    procedure DrawText; virtual;
    function ExcludeFromClipRect: Boolean; virtual;
    class function GetIsMainCanvasInUse(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo): Boolean;
    function NeedsPainting: Boolean; virtual;
    procedure Paint; virtual;
    procedure PrepareCanvasForDrawText; virtual;
    procedure UnprepareCanvasForDrawText; virtual;

    property Canvas: TcxCanvas read FCanvas;
    property IsMainCanvasInUse: Boolean read GetIsMainCanvasInUseValue;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TcxCustomGridCellViewInfo read FViewInfo;
  public
    constructor Create(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo); virtual;
    class procedure DoDrawDesignSelection(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo); virtual;
    procedure MainPaint; virtual;
  end;

  TcxGridDesignSelectorPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridDesignSelectorViewInfo;
  protected
    procedure DoExcludeFromClipRect; override;
    procedure DrawSign(AColor: TColor); virtual;
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;
    property ViewInfo: TcxGridDesignSelectorViewInfo read GetViewInfo;
  end;

  TcxCustomGridPainterClass = class of TcxCustomGridPainter;

  TcxCustomGridPainter = class(TcxGridViewHandler)
  private
    FBackgroundRegion: TcxRegion;
    FBeforePaintClipRegion: TcxRegion;
    FCanvas: TcxCanvas;
    FViewInfo: TcxCustomGridViewInfo;
    function GetCanvas: TcxCanvas;
    function GetViewInfo: TcxCustomGridViewInfo;
  protected
    procedure DrawBackground; virtual;
    procedure PaintAfter; virtual;
    procedure PaintBefore; virtual;
    procedure PaintContent; virtual;
    property Canvas: TcxCanvas read GetCanvas write FCanvas;
    property ViewInfo: TcxCustomGridViewInfo read GetViewInfo write FViewInfo;
  public
    procedure DrawFocusRect(const R: TRect; AHideFocusRect: Boolean); virtual;
    procedure ExcludeFromBackground(const R: TRect);
    procedure Paint(ACanvas: TcxCanvas = nil; AViewInfo: TcxCustomGridViewInfo = nil);
    procedure Invalidate; overload;
    procedure Invalidate(const R: TRect); overload;
    procedure Invalidate(ARegion: TcxRegion); overload;
  end;

  { ViewData }

  TcxCustomGridViewDataClass = class of TcxCustomGridViewData;

  TcxCustomGridViewData = class(TcxGridViewHandler)
  public
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; virtual;
    function IsEmpty: Boolean; virtual;
    function MakeDetailVisible(ADetailLevel: TComponent{TcxGridLevel}): TcxCustomGridView; virtual;
    property DataController;
  end;

  { ViewInfos }

  { TcxGridSite }

  TcxGridSite = class(TcxControl,
    IcxLockedStatePaint,
    IcxEditorFieldLink)
  private
    FBoundsChangedTimer: TcxTimer;
    FDesignPopupMenuInvoker: TcxCustomGridCellViewInfo;
    FIgnoreUpdateLock: Boolean;
    FIsWindowRegionAssigned: Boolean;
    FLeftOnMouseDown: Integer;
    FTopOnMouseDown: Integer;
    FSize: TSize;
    FViewInfo: TcxCustomGridViewInfo;
    function GetContainer: TcxControl; inline;
    function GetController: TcxCustomGridController; inline;
    function GetGridView: TcxCustomGridView; inline;
    function GetLocked: Boolean;
    function GetPainter: TcxCustomGridPainter; inline;
    procedure SendKeyDownNotification(var Message: TWMKeyDown);
    procedure BoundsChangedTimerHandler(Sender: TObject);
    procedure SaveOrigin;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMGestureNotify(var Message: TWMGestureNotify); message WM_GESTURENOTIFY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMIMEStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
  protected
    // IcxLockedStatePaint
    function IcxLockedStatePaint.GetImage = GetLockedStateImage;
    function IcxLockedStatePaint.GetTopmostControl = GetLockedStateTopmostControl;
    function GetLockedStateImage: TcxBitmap32;
    function GetLockedStateTopmostControl: TcxControl;
    // IcxEditorFieldLink
    function CreateFieldControls(X, Y: Integer; ADataSource: TObject; AFieldList: TList): Boolean;

    // touch
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure CheckOverpan(AScrollKind: TScrollBarKind; ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer); override;
    function IsGestureTarget(AWnd: THandle): Boolean; override;

    function AllowAutoDragAndDropAtDesignTime(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function AllowDragAndDropWithoutFocus: Boolean; override;
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure BoundsChanged; override;
    function CanDrag(X, Y: Integer): Boolean; override;
    function CanFocusOnClick(X, Y: Integer): Boolean; override;
    function CanProcessScrollEvents(var Message: TMessage): Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckCancelDrag;
    procedure DestroyHandle; override;
    procedure DoCancelMode; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer); override;

    procedure BeginGestureScroll(APos: TPoint); override;
    function CanScrollContentByGestureWithoutScrollBars: Boolean; override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions); override;
    procedure DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer); override;
    procedure EndGestureScroll; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsGestureScrolling: Boolean; override;
    function IsGestureHelperMessage(var Message: TMessage): Boolean; override;
    function IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean; override;
    procedure ResetRegion;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); override;
    procedure SetRegion(const AVisibleRect: TRect);

    function AllowTouchScrollUIMode: Boolean; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;

    procedure DoPaint; override;
    procedure DoScrollUIModeChanged; override;
    procedure FocusChanged; override;
    function FocusWhenChildIsClicked(AChild: TControl): Boolean; override;
    function GetClientBounds: TRect; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetIsDesigning: Boolean; override;
    function GetIsFocused: Boolean; override;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetVScrollBarBounds: TRect; override;
    function GetSystemSizeScrollBars: TcxScrollStyle; override;
    procedure HideWindow;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function IsPixelScrollBar(AKind: TScrollBarKind): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function MayFocus: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveWindow(const ANewBounds: TRect);
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure SetParent(AParent: TWinControl); override;
    function UpdateMousePositionIfControlMoved: Boolean; override;
    function WasMovedOnMouseDown: Boolean; virtual;
    procedure WndProc(var Message: TMessage); override;

    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DrawDragImage(ACanvas: TcxCanvas; const R: TRect); override;
    function GetDragImagesSize: TPoint; override;
    function HasDragImages: Boolean; override;

    procedure InitScrollBarsParameters; override;
    function GetScrollContentForegroundColor: TColor; override;
    function GetHScrollBarBounds: TRect; override;
    function HasScrollBarArea: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure UpdateScrollBars; override;

    function CanCancelDragStartOnCaptureObjectClear: Boolean; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    procedure CancelPostBoundsChanged;
    procedure CheckClipping; virtual;
    procedure InitTabStop(AParent: TWinControl);
    function IsLockedStatePaint(out ALockedStateImage: TcxBitmap32): Boolean;
    procedure PostBoundsChanged;
    procedure ScrollBarVisibilityChanged(AScrollBars: TScrollBarKinds); override;
    procedure UpdateSize; virtual;

    property Controller: TcxCustomGridController read GetController;
    property IgnoreUpdateLock: Boolean read FIgnoreUpdateLock write FIgnoreUpdateLock;
    property Painter: TcxCustomGridPainter read GetPainter;
  public
    constructor Create(AViewInfo: TcxCustomGridViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeginDragAndDrop; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
  {$IFDEF DELPHIBERLIN}
    procedure ScaleForPPI(NewPPI: Integer); override;
  {$ENDIF}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetFocus; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;

    property Container: TcxControl read GetContainer;
    property GridView: TcxCustomGridView read GetGridView;
    property HScrollBar;
    property HScrollBarVisible;
    property Keys; //!!!
    property Locked: Boolean read GetLocked;
    property MouseCapture;
    property ViewInfo: TcxCustomGridViewInfo read FViewInfo;
    property VScrollBar;
    property VScrollBarVisible;
  end;
  TcxGridSiteClass = class of TcxGridSite;

  TcxCustomGridCellViewInfoClass = class of TcxCustomGridCellViewInfo;

  TcxCustomGridCellViewInfo = class(TcxOwnedInterfacedPersistent,
    IcxMouseCaptureObject,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2,
    IcxCustomGridPopupOwner,
    IdxScaleFactor,
    IdxAdornerTargetElement,
    IdxAdornerTargetElementCollection)
  private
    FCalculated: Boolean;
    FCalculatingParams: Boolean;
    FIsRightToLeftConverted: Boolean;
    FAlignmentHorz: TAlignment;
    FAlignmentVert: TcxAlignmentVert;
    FBorders: TcxBorders;
    FIsDestroying: Boolean;
    FLinkedHitTest: TcxCustomGridHitTest;
    FParamsCalculated: Boolean;
    FState: TcxGridCellState;
    FText: string;
    FVisible: Boolean;
    function GetBorderSize(AIndex: TcxBorder): Integer;
    function GetClientBounds: TRect;
    function GetContentBounds: TRect;
    function GetTextBoundsValue: TRect;
    function GetTextHeight: Integer;
    function GetTextWidth: Integer;
    procedure SetLinkedHitTest(Value: TcxCustomGridHitTest);
    procedure SetState(Value: TcxGridCellState);
  protected
    FClientBounds: TRect;
    FContentBounds: TRect;

    { IcxMouseCaptureObject }
    procedure DoCancelMode; virtual;
    { IcxMouseTrackingCaller }
    procedure TrackingMouseLeave;
    procedure IcxMouseTrackingCaller.MouseLeave = TrackingMouseLeave;
    { IcxMouseTrackingCaller2 }
    procedure MouseLeave; virtual;
    function PtInCaller(const P: TPoint): Boolean; virtual;
    { IcxCustomGridPopupOwner }
    function IcxCustomGridPopupOwner.ClosePopupWhenSetNil = CloseDropDownWindowOnDestruction;
    procedure IcxCustomGridPopupOwner.InitPopup = InitDropDownWindow;
    procedure IcxCustomGridPopupOwner.PopupClosed = CloseUp;
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
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

    procedure AfterCalculateBounds(var ABounds: TRect); virtual;
    procedure AfterCustomDraw(ACanvas: TcxCanvas); virtual;
    function AllowChangeStateOnMouseDown: Boolean; virtual;
    function AllowRightToLeftConversionOnRecalculate: Boolean; virtual;
    procedure BeforeCustomDraw(ACanvas: TcxCanvas); virtual;
    procedure BeforeStateChange; virtual;
    function CalculateClientBounds: TRect; virtual;
    function CalculateContentBounds: TRect; virtual;
    function CalculateTextWidth(AAngle: Integer = 0): Integer;
    function CalculateTextHeight(AForPainting: Boolean; AAngle: Integer = 0): Integer;
    procedure CalculateParams;
    procedure CalculateParamsNeeded;
    function CalculateHeight: Integer; virtual;
    function CalculateWidth: Integer; virtual;
    function CanProcessMouseLeave: Boolean; virtual;
    function CaptureMouseOnPress: Boolean; virtual;
    procedure Click; virtual;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; virtual;
    function CustomDrawBackground(ACanvas: TcxCanvas): Boolean; virtual;
    procedure Destroying;
    procedure DoCalculateParams; virtual;
    procedure DoInvalidate; virtual;
    function DoCustomDraw(ACanvas: TcxCanvas): Boolean; virtual;
    function DoCustomDrawBackground(ACanvas: TcxCanvas): Boolean; virtual;
    function GetActualState: TcxGridCellState; virtual;
    function GetAlignmentHorz: TAlignment; virtual;
    function GetAlignmentVert: TcxAlignmentVert; virtual;
    function GetAreaBounds: TRect; virtual;
    function GetBackgroundBitmap: TBitmap; virtual;
    function GetBorderBounds(AIndex: TcxBorder): TRect; virtual;
    function GetBorderColor(AIndex: TcxBorder): TColor; virtual;
    function GetBorders: TcxBorders; virtual;
    function GetBorderWidth(AIndex: TcxBorder): Integer; virtual;
    function GetBounds: TRect;
    function GetButtonState: TcxButtonState; virtual;
    function GetCanvas: TcxCanvas; virtual; abstract;
    class function GetCellHeight(ATextHeight: Integer;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function GetContentHeight: Integer; virtual;
    function GetContentWidth: Integer; virtual;
    function GetControl: TcxControl; virtual;
    function GetDesignSelectionBounds: TRect; virtual;
    function GetDesignSelectionWidth: Integer; virtual;
    function GetDrawTextRotationAngle: TcxRotationAngle; virtual;
    function GetHeight: Integer; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; virtual; abstract;
    function GetHotTrack: Boolean; virtual;
    function GetIsCheck: Boolean; virtual;
    function GetIsDesignSelected: Boolean; virtual;
    function GetIsVisibleForPainting: Boolean; virtual;
    function GetMouseCapture: Boolean; virtual;
    function GetMultiLine: Boolean; virtual;
    function GetMultiLinePainting: Boolean; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; virtual;
    function GetRealBounds: TRect; virtual;
    function GetRealTextAreaBounds: TRect; virtual;
    function GetShowEndEllipsis: Boolean; virtual;
    function GetText: string; virtual;
    function GetTextAreaBounds: TRect; virtual;
    function GetTextAttributes(AForPainting: Boolean): Integer; virtual;
    function GetTextBounds(AHorizontal, AVertical: Boolean): TRect;
    function GetTextCellHeight(AGridViewInfo: TcxCustomGridViewInfo;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer; virtual;
    function GetTextHeightWithOffset: Integer; virtual;
    function GetTextWidthWithOffset: Integer; virtual;
    function GetTextForPainting: string; virtual;
    function GetTransparent: Boolean; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); virtual;
    function GetVisible: Boolean; virtual;
    function GetVisibleForHitTest: Boolean; virtual;
    function GetWidth: Integer; virtual;
    function HasBackground: Boolean; virtual;
    function HasCustomDraw: Boolean; virtual;
    function HasCustomDrawBackground: Boolean; virtual;
    function HasDesignPopupMenu: Boolean; virtual;
    function HasHitTestPoint(const P: TPoint): Boolean; virtual;
    function HasMouse(AHitTest: TcxCustomGridHitTest): Boolean; virtual;
    procedure HotTrackChanged; virtual;
    procedure InitDropDownWindow(APopup: TdxUIElementPopupWindow); virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); virtual;
    function InvalidateOnStateChange: Boolean; virtual;
    function IsHotTrackChanged(APrevState: TcxGridCellState): Boolean; virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure PopulateDesignPopupMenu(APopupMenu: TPopupMenu); virtual;
    procedure PrepareCanvas(ACanvas: TcxCanvas); virtual;
    procedure RestoreParams(const AParams: TcxViewParams); virtual;
    procedure SaveParams(out AParams: TcxViewParams); virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetMouseCapture(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual;
    procedure StateChanged(APrevState: TcxGridCellState); virtual;

    procedure DropDown; virtual;
    procedure CloseUp; virtual;
    function CloseDropDownWindowOnDestruction: Boolean; virtual;
    function DropDownWindowExists: Boolean; virtual;
    function GetDropDownWindow: TdxUIElementPopupWindow; virtual;
    function GetDropDownWindowAlignHorz: TcxPopupAlignHorz; virtual;
    function GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz; virtual;
    function GetDropDownWindowOwnerBounds: TRect; virtual;
    function IsDropDownWindowOwner: Boolean; virtual;
    property DropDownWindow: TdxUIElementPopupWindow read GetDropDownWindow;

    property BorderWidth[AIndex: TcxBorder]: Integer read GetBorderWidth;
    property CalculatingParams: Boolean read FCalculatingParams;
    property Canvas: TcxCanvas read GetCanvas;
    property Control: TcxControl read GetControl;
    property DesignSelectionBounds: TRect read GetDesignSelectionBounds;
    property DesignSelectionWidth: Integer read GetDesignSelectionWidth;
    property HotTrack: Boolean read GetHotTrack;
    property IsCheck: Boolean read GetIsCheck;
    property IsDesignSelected: Boolean read GetIsDesignSelected;
    property IsDestroying: Boolean read FIsDestroying;
    property IsVisibleForPainting: Boolean read GetIsVisibleForPainting;
    property LinkedHitTest: TcxCustomGridHitTest read FLinkedHitTest write SetLinkedHitTest;
    property ShowEndEllipsis: Boolean read GetShowEndEllipsis;
  public
    Bounds: TRect;
    MultiLine: Boolean;
    MultiLinePainting: Boolean;
    Params: TcxViewParams;

    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AfterRecalculation; virtual;
    procedure BeforeDestruction; override;
    procedure BeforeRecalculation; virtual;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); overload; virtual;
    procedure Calculate(const ABounds: TRect); overload; virtual;
    function GetAreaBoundsForPainting: TRect; virtual;
    function GetBestFitWidth: Integer; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function HasPoint(const P: TPoint): Boolean; virtual;
    procedure Invalidate; virtual;
    function IsHotTracked(ACheckChildren: Boolean = True): Boolean; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; virtual;
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; virtual;
    function MouseUp(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; virtual;
    procedure DoOffset(DX, DY: Integer);
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure Paint(ACanvas: TcxCanvas = nil); virtual;
    procedure Recalculate;
    procedure ResetContentBounds;
    procedure RightToLeftConversion(const ABounds: TRect);
    procedure Update;
    function UseRightToLeftAlignment: Boolean;

    property ActualState: TcxGridCellState read GetActualState;
    property AlignmentHorz: TAlignment read FAlignmentHorz write FAlignmentHorz;
    property AlignmentVert: TcxAlignmentVert read FAlignmentVert write FAlignmentVert;
    property BackgroundBitmap: TBitmap read GetBackgroundBitmap;
    property BorderBounds[AIndex: TcxBorder]: TRect read GetBorderBounds;
    property BorderColor[AIndex: TcxBorder]: TColor read GetBorderColor;
    property BorderSize[AIndex: TcxBorder]: Integer read GetBorderSize;
    property Borders: TcxBorders read FBorders write FBorders;
    property ButtonState: TcxButtonState read GetButtonState;
    property Calculated: Boolean read FCalculated write FCalculated;
    property ClientBounds: TRect read GetClientBounds;
    property ContentBounds: TRect read GetContentBounds;
    property ContentHeight: Integer read GetContentHeight;
    property ContentWidth: Integer read GetContentWidth;
    property Height: Integer read GetHeight write SetHeight;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property RealBounds: TRect read GetRealBounds;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxGridCellState read FState write SetState;
    property Text: string read FText write FText;
    property TextBounds: TRect read GetTextBoundsValue;
    property TextForPainting: string read GetTextForPainting;
    property TextHeightWithOffset: Integer read GetTextHeightWithOffset;
    property TextWidthWithOffset: Integer read GetTextWidthWithOffset;
    property TextHeight: Integer read GetTextHeight;
    property TextWidth: Integer read GetTextWidth;
    property TextAreaBounds: TRect read GetRealTextAreaBounds;
    property Transparent: Boolean read GetTransparent;
    property Visible: Boolean read GetVisible write FVisible;
    property VisibleForHitTest: Boolean read GetVisibleForHitTest;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TcxCustomGridViewCellViewInfo = class(TcxCustomGridCellViewInfo,
    IcxHintableObject)
  private
    FGridViewInfo: TcxCustomGridViewInfo;
    function GetController: TcxCustomGridController;
    function GetGridView: TcxCustomGridView;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    // IdxAdornerTargetElement
    function GetAdornerTargetElementControl: TWinControl; override;
    function GetAdornerTargetElementBounds: TRect; override;

    procedure AfterCalculateBounds(var ABounds: TRect); override;
    function AllowRightToLeftConversionOnRecalculate: Boolean; override;
    procedure DoInvalidate; override;
    function EmulateMouseMoveAfterCalculate: Boolean; virtual;
    function GetCanvas: TcxCanvas; override;
    function GetControl: TcxControl; override;
    function HasMouse(AHitTest: TcxCustomGridHitTest): Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure InitDropDownWindow(APopup: TdxUIElementPopupWindow); override;

    //IcxHintableObject
    function IsHintAtMousePos: Boolean; virtual;
    function UseHintHidePause: Boolean; virtual;
    function HasHintPoint(const P: TPoint): Boolean; virtual;
    //hint
    function CanShowHint: Boolean; virtual;
    procedure CheckHint(AHitTest: TcxCustomGridHitTest);
    function GetAreaBoundsForHint: TRect; virtual;
    function GetBoundsForHint: TRect;
    function GetCellBoundsForHint: TRect; virtual;
    function GetHintText: string; virtual;
    function GetHintTextRect(const AMousePos: TPoint): TRect; virtual;
    procedure InitHint(const AMousePos: TPoint; out AHintText: TCaption;
      out AIsHintMultiLine: Boolean; out ATextRect: TRect); virtual;
    function IsHintForText: Boolean; virtual;
    function IsHintMultiLine: Boolean; virtual;
     function NeedShowHint(const AMousePos: TPoint; out AHintText: TCaption;
      out AIsHintMultiLine: Boolean; out ATextRect: TRect): Boolean; virtual;

    property Controller: TcxCustomGridController read GetController;
  public
    constructor Create(AGridViewInfo: TcxCustomGridViewInfo); virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Invalidate; override;
    function MouseMove(AHitTest: TcxCustomGridHitTest; AShift: TShiftState): Boolean; override;

    property GridView: TcxCustomGridView read GetGridView;
    property GridViewInfo: TcxCustomGridViewInfo read FGridViewInfo;
    property HintText: string read GetHintText;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

  TcxGridDesignSelectorViewInfoClass = class of TcxGridDesignSelectorViewInfo;

  TcxGridDesignSelectorViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FRegion: TcxRegion;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure CreateRegion;
    procedure DestroyRegion;

    property Region: TcxRegion read FRegion;
  public
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function HasPoint(const P: TPoint): Boolean; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
  end;

  TcxCustomGridViewInfoClass = class of TcxCustomGridViewInfo;

  TcxCustomGridViewInfo = class(TcxGridViewHandler,
    IdxScaleFactor)
  private
    FAllowCheckCoordinates: Boolean;
    FAllowHideSite: Boolean;
    FBounds: TRect;
    FClientBounds: TRect;
    FClientBoundsAssigned: Boolean;
    FDesignSelectorViewInfo: TcxGridDesignSelectorViewInfo;
    FIsCalculating: Boolean;
    FIsInternalUse: Boolean;
    FIsRightToLeftConverted: Boolean;
    FLock: TRTLCriticalSection;
    FMousePos: TPoint;
    FSite: TcxGridSite;
    FSizeCalculating: Boolean;
    FVisibilityChanging: Boolean;

    function GetCalculated: Boolean; inline;
    function GetCanvas: TcxCanvas;
    function GetClientBounds: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetIsInternalUseValue: Boolean;
    procedure SetClientBounds(const Value: TRect);
  protected
    procedure CreateViewInfos; virtual;
    procedure DestroyViewInfos(AIsRecreating: Boolean); virtual;
    procedure RecreateViewInfos; virtual;
    function GetDesignSelectorViewInfoClass: TcxGridDesignSelectorViewInfoClass; virtual;

    function CanHideSite: Boolean; virtual;
    procedure CreateSite;
    procedure DestroySite;
    function GetSiteClass: TcxGridSiteClass; virtual;
    function GetSiteParent: TWinControl;

    procedure AddScrollBarHeight(var AHeight: Integer);
    procedure AdjustClientBounds(var ABounds: TRect); virtual;
    function GetBottomNonClientHeight: Integer; virtual;
    function GetRightNonClientWidth: Integer; virtual;

    procedure AfterCalculating; virtual;
    procedure BeforeCalculating; virtual;
    procedure Calculate; virtual;
    function CalculateClientBounds: TRect; virtual;
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean); virtual;
    procedure CalculateWidth(const AMaxSize: TPoint; var AWidth: Integer); virtual;
    function CanCheckCoordinates: Boolean; virtual;
    function CanHandleDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; virtual;
    procedure ControlFocusChanged; virtual;
    function DoGetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetAllowBoundsChangedNotification: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetContentBounds: TRect; virtual;
    function GetDesignSelectorPos: TPoint; virtual;
    procedure GetHScrollBarBounds(var ABounds: TRect); virtual;
    function GetIsInternalUse: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); virtual;
    function IsDoubleBufferedNeeded: Boolean; virtual;
    function SiteCanBeClipped: Boolean; virtual;
    procedure UpdateMousePos;
    procedure VisibilityChanged(AVisible: Boolean); virtual;

    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
    procedure GetVScrollBarBounds(var ABounds: TRect); virtual;

    property AllowBoundsChangedNotification: Boolean read GetAllowBoundsChangedNotification;
    property AllowCheckCoordinates: Boolean read FAllowCheckCoordinates write FAllowCheckCoordinates;
    property ClientBoundsAssigned: Boolean read FClientBoundsAssigned write FClientBoundsAssigned;
    property SizeCalculating: Boolean read FSizeCalculating;
    property VisibilityChanging: Boolean read FVisibilityChanging;
    property Visible: Boolean read GetVisible;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DoVisibilityChanged(AVisible: Boolean);
    function GetFontHeight(AFont: TFont): Integer;
    procedure GetFontMetrics(AFont: TFont; out AMetrics: TTextMetric);
    procedure GetHeight(const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean);
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; overload; virtual;
    function GetHitTest(X, Y: Integer): TcxCustomGridHitTest; overload; virtual;
    procedure GetWidth(const AMaxSize: TPoint; var AWidth: Integer);
    procedure MainCalculate(const ABounds: TRect);
    procedure Recalculate;
    procedure RightToLeftConversion(const ABounds: TRect);

    property AllowHideSite: Boolean read FAllowHideSite write FAllowHideSite;
    property BackgroundColor: TColor read GetBackgroundColor;
    property BackgroundBitmap: TBitmap read GetBackgroundBitmap;
    property Bounds: TRect read FBounds;
    property Calculated: Boolean read GetCalculated;
    property Canvas: TcxCanvas read GetCanvas;
    property ClientBounds: TRect read GetClientBounds write SetClientBounds;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property DesignSelectorViewInfo: TcxGridDesignSelectorViewInfo read FDesignSelectorViewInfo;
    property IsCalculating: Boolean read FIsCalculating;
    property IsInternalUse: Boolean read GetIsInternalUseValue write FIsInternalUse;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property MousePos: TPoint read FMousePos;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Site: TcxGridSite read FSite;
  end;

  TcxCustomGridViewInfoCacheItemClass = class of TcxCustomGridViewInfoCacheItem;

  TcxCustomGridViewInfoCacheItem = class
  private
    FIndex: Integer;
    FOwner: TcxCustomGridViewInfoCache;
  protected
    property Index: Integer read FIndex;
    property Owner: TcxCustomGridViewInfoCache read FOwner;
  public
    constructor Create(AOwner: TcxCustomGridViewInfoCache; AIndex: Integer); virtual;
    procedure UnassignValues(AKeepMaster: Boolean); virtual;
  end;

  TcxCustomGridViewInfoCacheClass = class of TcxCustomGridViewInfoCache;

  TcxCustomGridViewInfoCache = class(TcxGridViewHandler)
  private
    FItems: TdxFastList;
    FUnassigningValues: Boolean;
    function GetCount: Integer;
    function GetInternalItem(Index: Integer): TcxCustomGridViewInfoCacheItem; inline;
    function GetItem(Index: Integer): TcxCustomGridViewInfoCacheItem;
    procedure SetCount(Value: Integer);
    procedure DestroyItems;
  protected
    function GetItemClass(Index: Integer): TcxCustomGridViewInfoCacheItemClass; virtual; abstract;
    property InternalItems[Index: Integer]: TcxCustomGridViewInfoCacheItem read GetInternalItem;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure UnassignValues(AKeepMaster: Boolean = False); virtual;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TcxCustomGridViewInfoCacheItem read GetItem; default;
  end;

  { custom view }

  TcxGridViewChangeKind = (vcProperty, vcLayout, vcSize);

  TcxCustomGridOptions = class(TcxInterfacedPersistent)
  strict private
    FGridView: TcxCustomGridView;

    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; inline;
  protected
    procedure Changed(AChangeKind: TcxGridViewChangeKind); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetGridViewValue: TcxCustomGridView; virtual;
    function IsLoading: Boolean;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;

    procedure GetStoredProperties(AProperties: TStrings); virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    constructor Create(AGridView: TcxCustomGridView); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxCustomGridView read GetGridViewValue;
  end;

  { TcxCustomGridBackgroundBitmaps }

  TcxCustomGridBackgroundBitmapsClass = class of TcxCustomGridBackgroundBitmaps;
  TcxCustomGridBackgroundBitmaps = class(TcxCustomGridOptions)
  private
    FItems: TdxFastList;
    function GetCount: Integer; inline;
    procedure BitmapChanged(Sender: TObject);
  protected
    function GetBitmapStyleIndex(Index: Integer): Integer; virtual;
    function GetDefaultBitmap(Index: Integer): TBitmap; virtual;
    function GetValue(Index: Integer): TBitmap;
    procedure SetValue(Index: Integer; Value: TBitmap);
    property Count: Integer read GetCount;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap(Index: Integer): TBitmap; virtual;
    property Values[Index: Integer]: TBitmap read GetValue write SetValue; default;
  published
    property Background: TBitmap index bbBackground read GetValue write SetValue;
  end;

  { TcxGridLockedImageOptions }

  TcxCustomGridShowLockedStateImageOptionsClass = class of TcxCustomGridShowLockedStateImageOptions;
  TcxCustomGridShowLockedStateImageOptions = class(TPersistent)
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxCustomGridOptionsBehavior }

  TcxCustomGridOptionsBehaviorClass = class of TcxCustomGridOptionsBehavior;
  TcxCustomGridOptionsBehavior = class(TcxCustomGridOptions)
  strict private
    FHintHidePause: Integer;
    FShowLockedStateImageOptions: TcxCustomGridShowLockedStateImageOptions;
    FPostponedSynchronization: Boolean;
    FShowHourglassCursor: Boolean;
    FSuppressHintOnMouseDown: Boolean;

    function GetPostponedSynchronization: Boolean;
    procedure SetShowLockedStateImageOptions(Value: TcxCustomGridShowLockedStateImageOptions);
  protected
    function GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass; virtual;

    property HintHidePause: Integer read FHintHidePause write FHintHidePause default 0;
    property ShowHourglassCursor: Boolean read FShowHourglassCursor write FShowHourglassCursor default True;
    property ShowLockedStateImageOptions: TcxCustomGridShowLockedStateImageOptions read FShowLockedStateImageOptions write SetShowLockedStateImageOptions;
    property SuppressHintOnMouseDown: Boolean read FSuppressHintOnMouseDown write FSuppressHintOnMouseDown default True;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property PostponedSynchronization: Boolean read GetPostponedSynchronization write FPostponedSynchronization default True;  // should be published in all descendants
  end;

  { TcxCustomGridOptionsData }

  TcxCustomGridOptionsData = class(TcxCustomGridOptions);
  TcxCustomGridOptionsDataClass = class of TcxCustomGridOptionsData;

  { TcxCustomGridOptionsSelection }

  TcxCustomGridOptionsSelection = class(TcxCustomGridOptions);
  TcxCustomGridOptionsSelectionClass = class of TcxCustomGridOptionsSelection;

  { TcxCustomGridOptionsView }

  TcxCustomGridOptionsViewClass = class of TcxCustomGridOptionsView;
  TcxCustomGridOptionsView = class(TcxCustomGridOptions)
  strict private
    function GetScrollBars: TcxScrollStyle;
    procedure SetScrollBars(Value: TcxScrollStyle);
  protected
    property ScrollBars: TcxScrollStyle read GetScrollBars write SetScrollBars default ssBoth;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TcxCustomGridStyles = class(TcxStyles)
  private
    function GetIsSkinsAvailable: Boolean;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure Changed(AIndex: Integer); override;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    procedure GetFakeComponentLinks(AList: TList); virtual;
    function GetGridView: TcxCustomGridView; virtual; abstract;
    //
    property IsSkinsAvailable: Boolean read GetIsSkinsAvailable;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    property GridView: TcxCustomGridView read GetGridView;
  end;

  TcxCustomGridViewStyles = class(TcxCustomGridStyles)
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetGridView: TcxCustomGridView; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TcxStyle index vsBackground read GetValue write SetValue;
  end;
  TcxCustomGridViewStylesClass = class of TcxCustomGridViewStyles;

  { TcxGridViewNotificationList }

  TcxGridViewNotificationList = class(TdxFastObjectList)
  private
    FGridView: TcxCustomGridView;
    function GetItem(Index: Integer): TcxUpdateControlInfo; inline;
  public
    constructor Create(AGridView: TcxCustomGridView);
    procedure Add(AInfo: TcxUpdateControlInfo);

    property Items[Index: Integer]: TcxUpdateControlInfo read GetItem; default;
  end;

  TcxGridStorageOption = (gsoUseFilter, gsoUseSummary);
  TcxGridStorageOptions = set of TcxGridStorageOption;
  TcxGridViewChangeNotificationKind = (vcnName{, vsRemoved});

  TcxGridInitStoredObjectEvent = procedure(Sender: TcxCustomGridView; AObject: TObject) of object;
  TcxGridViewGetStoredPropertiesEvent = procedure(Sender: TcxCustomGridView;
    AProperties: TStrings) of object;
  TcxGridViewGetStoredPropertyValueEvent = procedure (Sender: TcxCustomGridView; const AName: string; var AValue: Variant) of object;
  TcxGridViewSetStoredPropertyValueEvent = procedure (Sender: TcxCustomGridView; const AName: string; const AValue: Variant) of object;

  TcxCustomGridViewClass = class of TcxCustomGridView;
  TcxCustomGridView = class(TcxControlChildComponent,
    IcxStoredObject,
    IcxStoredParent,
    IcxGridViewLayoutEditorSupport,
    IdxAdornerTargetElement)
  private
    FAssigningPattern: Boolean;
    FAssigningSettings: Boolean;
    FBackgroundBitmaps: TcxCustomGridBackgroundBitmaps;
    FClones: TdxFastList;
    FController: TcxCustomGridController;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FIsChangeNotificationForControllerNeeded: Boolean;
    FIsCustomizationFormRefreshNeeded: Boolean;
    FIsRestoring: Boolean;
    FIsSynchronizing: Boolean;
    FLevel: TComponent;
    FNotifications: TcxGridViewNotificationList;
    FOptionsList: TList;
    FOptionsBehavior: TcxCustomGridOptionsBehavior;
    FOptionsData: TcxCustomGridOptionsData;
    FOptionsSelection: TcxCustomGridOptionsSelection;
    FOptionsView: TcxCustomGridOptionsView;
    FPainter: TcxCustomGridPainter;
    FPatternGridView: TcxCustomGridView;
    FRepository: TComponent;
    FStorageControl: TcxControl;
    FStorageOptions: TcxGridStorageOptions;
    FStoredVersion: Integer;
    FStoringName: string;
    FStyles: TcxCustomGridStyles;
    FSynchronization: Boolean;
    FSynchronizationAssignNeeded: Boolean;
    FSynchronizationNeeded: Boolean;
    FUpdateLockCount: Integer;
    FViewData: TcxCustomGridViewData;
    FViewInfo: TcxCustomGridViewInfo;
    FViewInfoCache: TcxCustomGridViewInfoCache;

    FOnCustomization: TNotifyEvent;
    FOnGetStoredProperties: TcxGridViewGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGridViewGetStoredPropertyValueEvent;
    FOnInitStoredObject: TcxGridInitStoredObjectEvent;
    FOnSetStoredPropertyValue: TcxGridViewSetStoredPropertyValueEvent;

    function GetClone(Index: Integer): TcxCustomGridView; inline;
    function GetCloneCount: Integer; inline;
    function GetDragMode: TDragMode;
    function GetFocused: Boolean;
    function GetIsControlFocusedValue: Boolean;
    function GetIsDetail: Boolean;
    function GetIsExportMode: Boolean;
    function GetIsMaster: Boolean;
    function GetIsPattern: Boolean;
    function GetIsStoringNameMode: Boolean;
    function GetIsUpdating: Boolean;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetMasterGridView: TcxCustomGridView;
    function GetMasterGridRecordIndex: Integer;
    function GetMasterRecordIndex: Integer;
    function GetOnDblClick: TNotifyEvent;
    function GetOnDragDrop: TDragDropEvent;
    function GetOnDragOver: TDragOverEvent;
    function GetOnEndDrag: TEndDragEvent;
    function GetOnKeyDown: TKeyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnKeyUp: TKeyEvent;
    function GetOnMouseDown: TMouseEvent;
    function GetOnMouseEnter: TNotifyEvent;
    function GetOnMouseLeave: TNotifyEvent;
    function GetOnMouseMove: TMouseMoveEvent;
    function GetOnMouseUp: TMouseEvent;
    function GetOnMouseWheel: TMouseWheelEvent;
    function GetOnMouseWheelDown: TMouseWheelUpDownEvent;
    function GetOnMouseWheelUp: TMouseWheelUpDownEvent;
    function GetOnStartDrag: TStartDragEvent;
    function GetPatternGridView: TcxCustomGridView;
    function GetPopupMenu: TComponent;
    function GetSite: TcxGridSite;
    function GetSynchronization: Boolean;
    procedure SetBackgroundBitmaps(Value: TcxCustomGridBackgroundBitmaps);
    procedure SetDragMode(Value: TDragMode);
    procedure SetFocused(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetIsRestoring(Value: Boolean);
    procedure SetSynchronization(Value: Boolean);
    procedure SetOnCustomization(Value: TNotifyEvent);
    procedure SetOnDblClick(Value: TNotifyEvent);
    procedure SetOnDragDrop(Value: TDragDropEvent);
    procedure SetOnDragOver(Value: TDragOverEvent);
    procedure SetOnEndDrag(Value: TEndDragEvent);
    procedure SetOnGetStoredProperties(Value: TcxGridViewGetStoredPropertiesEvent);
    procedure SetOnGetStoredPropertyValue(Value: TcxGridViewGetStoredPropertyValueEvent);
    procedure SetOnInitStoredObject(Value: TcxGridInitStoredObjectEvent);
    procedure SetOnKeyDown(Value: TKeyEvent);
    procedure SetOnKeyPress(Value: TKeyPressEvent);
    procedure SetOnKeyUp(Value: TKeyEvent);
    procedure SetOnMouseDown(Value: TMouseEvent);
    procedure SetOnMouseEnter(Value: TNotifyEvent);
    procedure SetOnMouseLeave(Value: TNotifyEvent);
    procedure SetOnMouseMove(Value: TMouseMoveEvent);
    procedure SetOnMouseUp(Value: TMouseEvent);
    procedure SetOnMouseWheel(Value: TMouseWheelEvent);
    procedure SetOnMouseWheelDown(Value: TMouseWheelUpDownEvent);
    procedure SetOnMouseWheelUp(Value: TMouseWheelUpDownEvent);
    procedure SetOnSetStoredPropertyValue(Value: TcxGridViewSetStoredPropertyValueEvent);
    procedure SetOnStartDrag(Value: TStartDragEvent);
    procedure SetOptionsBehavior(Value: TcxCustomGridOptionsBehavior);
    procedure SetOptionsData(Value: TcxCustomGridOptionsData);
    procedure SetOptionsSelection(Value: TcxCustomGridOptionsSelection);
    procedure SetOptionsView(Value: TcxCustomGridOptionsView);
    procedure SetPopupMenu(Value: TComponent);
    procedure SetStyles(Value: TcxCustomGridStyles);

    procedure ClearNotifications;
  protected
    FDataController: TcxCustomDataController;
    FSubClassEvents: TNotifyEvent;

    // IcxStoredObject
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject;
    function CreateStoredObject(const AObjectName, AClassName: string): TObject; virtual;
    procedure DeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    procedure InitChildComponent(AObject: TObject; const AObjectName: string); virtual;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); virtual;
    procedure BeforeEditLayout(ALayoutView: TcxCustomGridView); virtual;
    function CanEditViewLayoutAndData: Boolean; virtual;
    procedure DoAssignLayout(ALayoutView: TcxCustomGridView);
    function GetLayoutCustomizationFormButtonCaption: string; virtual;
    function HasLayoutCustomizationForm: Boolean; virtual;
    function IsLayoutChangeable: Boolean; virtual;
    procedure RunLayoutCustomizationForm; virtual;
    function ShowGridViewEditor: Boolean; virtual;
    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    procedure ChangeScale(M, D: Integer); virtual;
    procedure GetFakeComponentLinks(AList: TList); override;
    function GetIsDestroying: Boolean; override;
    function GetSystemSizeScrollBars: TcxScrollStyle; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScaleFactorChanged(M, D: Integer); override; final;
    procedure SetControl(Value: TcxControl); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure Updated; override;
    procedure Updating; override;

    procedure CreateHandlers; virtual;
    procedure DestroyHandlers; virtual;
    procedure InitDataController; virtual;

    procedure CreateOptions; virtual;
    procedure DestroyOptions; virtual;

    procedure AddOptions(AOptions: TcxCustomGridOptions);
    procedure RemoveOptions(AOptions: TcxCustomGridOptions);
    procedure NotifyOptions(AComponent: TComponent; AOperation: TOperation);

    procedure AddClone(AClone: TcxCustomGridView);
    procedure RemoveClone(AClone: TcxCustomGridView);

    procedure GridBeginUpdate(AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
    procedure GridCancelUpdate;
    procedure GridEndUpdate;
    procedure AfterDestroyingLockedStateImage; virtual;
    procedure BeforeCreatingLockedStateImage; virtual;
    procedure GridBeginLockedStatePaint(AMode: TcxGridShowLockedStateImageMode); virtual;
    procedure GridEndLockedStatePaint; virtual;

    procedure Synchronize(ACheckUpdateLock: Boolean = True); overload;
    procedure Synchronize(AView: TcxCustomGridView); overload;
    property SynchronizationAssignNeeded: Boolean read FSynchronizationAssignNeeded;
    property SynchronizationNeeded: Boolean read FSynchronizationNeeded;

    procedure AssignPattern(APattern: TcxCustomGridView);
    procedure BeforeAssign(ASource: TcxCustomGridView); virtual;
    procedure DoAssign(ASource: TcxCustomGridView); virtual;
    procedure DoAssignSettings(ASource: TcxCustomGridView); virtual;
    procedure AfterAssign(ASource: TcxCustomGridView); virtual;
    property AssigningPattern: Boolean read FAssigningPattern;
    property AssigningSettings: Boolean read FAssigningSettings;

    procedure BeforeRestoring; virtual;
    procedure AfterRestoring; virtual;
    procedure RestoreFrom(const AStorageName: string; AStream: TStream;
      AReaderClass: TcxCustomReaderClass; AChildrenCreating, AChildrenDeleting: Boolean;
      AOptions: TcxGridStorageOptions; const ARestoreViewName: string;
      const AOwnerName: string = ''); virtual;
    procedure StoreTo(const AStorageName: string; AStream: TStream;
      AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
      AOptions: TcxGridStorageOptions; const ASaveViewName: string;
      const AOwnerName: string = '');
    function HandleSetStoredPropertyValueError(Sender: TcxStorage; const AName: string; const AValue: Variant): Boolean; virtual;
    property IsRestoring: Boolean read FIsRestoring write SetIsRestoring;

    procedure AddNotification(AUpdateInfo: TcxUpdateControlInfo);
    //procedure BeforeLevelChange; virtual;
    function CanBeUsedAsDetail: Boolean; virtual;
    function CanBeUsedAsMaster: Boolean; virtual;
    procedure UnsupportedMasterDetailError;
    procedure ValidateMasterDetailRelationship(AIsMaster: Boolean);

    procedure BiDiModeChanged; virtual;
    function CanFocus: Boolean; virtual;
    function CanGetHitTest: Boolean; virtual;
    function CanTabStop: Boolean; virtual;
    procedure Deactivate; virtual;
    procedure DestroyingSiteHandle; virtual;
    procedure DetailDataChanged(ADetail: TcxCustomGridView); virtual;
    procedure DetailVisibleChanged(ADetailLevel: TComponent; APrevVisibleDetailCount, AVisibleDetailCount: Integer); virtual;
    procedure DoChanged(AChangeKind: TcxGridViewChangeKind); virtual;
    procedure DoStylesChanged; virtual;
    procedure DoUnlockNotification(AInfo: TcxUpdateControlInfo); virtual;
    function GetChangeable: Boolean; virtual;
    function GetCustomVisible(ALevelVisible: Boolean): Boolean; virtual;
    function GetHorizontalScrollBarAreaHeight: Integer;
    function GetIsControlFocused: Boolean; virtual;
    function GetIsControlLocked: Boolean; virtual;
    function GetResizeOnBoundsChange: Boolean; virtual;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; virtual;
    function GetVerticalScrollBarAreaWidth: Integer;
    function GetVisible: Boolean; virtual;
    procedure HideTouchScrollUI(AImmediately: Boolean); virtual;
    procedure ImageListChange(Sender: TObject);
    procedure Init; virtual;
    function IsDetailVisible(AGridView: TcxCustomGridView): Boolean; virtual;
    function IsRecordPixelScrolling: Boolean; virtual;
    procedure FocusChanged(AFocused: Boolean); virtual;
    procedure LoadingComplete; virtual;
    procedure LookAndFeelChanged; virtual;
    procedure NotifyControl(AChangeKind: TcxGridViewChangeNotificationKind);
    procedure NotifyControllerAboutChange;
    procedure RestoringComplete; virtual;
    procedure ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer); virtual;
    procedure SetLevel(Value: TComponent); virtual;
    procedure SetTabStop(Value: Boolean); virtual;
    function SpaceForHorizontalScrollbarNeeded: Boolean; virtual;
    procedure StylesChanged;
    procedure UnlockNotifications;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateUnlocked; virtual;

    function GetControllerClass: TcxCustomGridControllerClass; virtual; abstract;
    function GetDataControllerClass: TcxCustomDataControllerClass; virtual; abstract;
    function GetPainterClass: TcxCustomGridPainterClass; virtual; abstract;
    function GetViewDataClass: TcxCustomGridViewDataClass; virtual; abstract;
    function GetViewInfoCacheClass: TcxCustomGridViewInfoCacheClass; virtual;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; virtual; abstract;

    function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; virtual;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; virtual;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; virtual;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; virtual;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; virtual;
    function GetStylesClass: TcxCustomGridViewStylesClass; virtual;

    procedure Initialize; override;

    procedure RefreshCustomizationForm;

    procedure DoCustomization; virtual;
    procedure DoInitStoredObject(AObject: TObject); virtual;

    property BackgroundBitmaps: TcxCustomGridBackgroundBitmaps read FBackgroundBitmaps write SetBackgroundBitmaps;  {4}
    property Changeable: Boolean read GetChangeable;
    property ImageChangeLink: TChangeLink read FImageChangeLink;
    property Images: TCustomImageList read FImages write SetImages;
    property IsExportMode: Boolean read GetIsExportMode;
    property IsStoringNameMode: Boolean read GetIsStoringNameMode;
    property IsSynchronizing: Boolean read FIsSynchronizing;
    property IsUpdating: Boolean read GetIsUpdating;
    property OptionsBehavior: TcxCustomGridOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsData: TcxCustomGridOptionsData read FOptionsData write SetOptionsData;
    property OptionsSelection: TcxCustomGridOptionsSelection read FOptionsSelection write SetOptionsSelection;
    property OptionsView: TcxCustomGridOptionsView read FOptionsView write SetOptionsView;
    property ResizeOnBoundsChange: Boolean read GetResizeOnBoundsChange;
    property StoredVersion: Integer read FStoredVersion;
    property Styles: TcxCustomGridStyles read FStyles write SetStyles;
    property OnCustomization: TNotifyEvent read FOnCustomization write SetOnCustomization;
    property OnInitStoredObject: TcxGridInitStoredObjectEvent read FOnInitStoredObject write SetOnInitStoredObject;
  public
    constructor CreateCloned(AControl: TcxControl); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignSettings(ASource: TcxCustomGridView);
    function GetImages: TCustomImageList;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function HasAsClone(AGridView: TcxCustomGridView): Boolean;
    function HasAsMaster(AGridView: TcxCustomGridView): Boolean;
    procedure Invalidate(AHardUpdate: Boolean = False);
    procedure RestoreDefaults; virtual;
    procedure SetParentComponent(Value: TComponent); override;
    function UseRightToLeftAlignment: Boolean;

    procedure CheckSynchronizationAssignNeeded;
    function IsSynchronization: Boolean;

    procedure BeginUpdate(AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
    procedure CancelUpdate;
    function Changed(AGridChange: TObject): Boolean; overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure EndUpdate;
    function IsUpdateLocked: Boolean;
    procedure HideHourglassCursor;
    procedure ShowHourglassCursor;

    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; overload; virtual;
    function GetHitTest(X, Y: Integer): TcxCustomGridHitTest; overload; virtual;

    procedure BoundsChanged(AUpdateSelfOnly: Boolean = False; AKeepMaster: Boolean = False); virtual;
    procedure Changed(AChangeKind: TcxGridViewChangeKind); overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure LayoutChanged(AUpdateSelfOnly: Boolean = True);
    function SizeChanged(AUpdateSelfOnly: Boolean = False; AKeepMaster: Boolean = False): Boolean; virtual;
    procedure ViewChanged; overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure ViewChanged(const AUpdateRect: TRect); overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}
    procedure ViewChanged(ARegion: TcxRegion); overload; {$IFDEF BCBCOMPATIBLE}virtual;{$ENDIF}

    procedure RestoreFromIniFile(const AStorageName: string; AChildrenCreating: Boolean = True;
      AChildrenDeleting: Boolean = False; AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary]{[]};
      const ARestoreViewName: string = ''; const AOwnerName: string = '');
    procedure RestoreFromRegistry(const AStorageName: string; AChildrenCreating: Boolean = True;
      AChildrenDeleting: Boolean = False; AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
      const ARestoreViewName: string = ''; const AOwnerName: string = '');
    procedure RestoreFromStream(AStream: TStream; AChildrenCreating: Boolean = True;
      AChildrenDeleting: Boolean = False; AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
      const ARestoreViewName: string = ''; const AOwnerName: string = '');
    procedure RestoreFromStorage(const AStorageName: string; AReaderClass: TcxCustomReaderClass;
      AChildrenCreating: Boolean = True; AChildrenDeleting: Boolean = False;
      AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
      const ARestoreViewName: string = ''; const AOwnerName: string = '');

    procedure StoreToIniFile(const AStorageName: string; AReCreate: Boolean = True;
      AOptions: TcxGridStorageOptions = []; const ASaveViewName: string = ''; const AOwnerName: string = '');
    procedure StoreToRegistry(const AStorageName: string; AReCreate: Boolean = True;
      AOptions: TcxGridStorageOptions = []; const ASaveViewName: string = ''; const AOwnerName: string = '');
    procedure StoreToStream(AStream: TStream; AOptions: TcxGridStorageOptions = [];
      const ASaveViewName: string = ''; const AOwnerName: string = '');
    procedure StoreToStorage(const AStorageName: string; AWriterClass: TcxCustomWriterClass;
      AReCreate: Boolean = True; AOptions: TcxGridStorageOptions = [];
      const ASaveViewName: string = ''; const AOwnerName: string = '');

    function CreateViewInfo: TcxCustomGridViewInfo;

    property Focused: Boolean read GetFocused write SetFocused;
    property StorageOptions: TcxGridStorageOptions read FStorageOptions write FStorageOptions;
    property StoringName: string read FStoringName write FStoringName;
    property TabStop: Boolean write SetTabStop;

    property CloneCount: Integer read GetCloneCount;
    property Clones[Index: Integer]: TcxCustomGridView read GetClone;
    property IsControlFocused: Boolean read GetIsControlFocusedValue;
    property IsControlLocked: Boolean read GetIsControlLocked;
    property IsDetail: Boolean read GetIsDetail;
    property IsMaster: Boolean read GetIsMaster;
    property IsPattern: Boolean read GetIsPattern;
    property Level: TComponent read FLevel;  // TcxGridLevel
    property MasterGridView: TcxCustomGridView read GetMasterGridView;
    property MasterGridRecordIndex: Integer read GetMasterGridRecordIndex;
    property MasterRecordIndex: Integer read GetMasterRecordIndex;
    property PatternGridView: TcxCustomGridView read GetPatternGridView;
    property Repository: TComponent read FRepository write FRepository;  {5}

    property Controller: TcxCustomGridController read FController;
    property DataController: TcxCustomDataController read FDataController;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Painter: TcxCustomGridPainter read FPainter;
    property Site: TcxGridSite read GetSite;
    property StorageControl: TcxControl read FStorageControl;
    property ViewData: TcxCustomGridViewData read FViewData;
    property ViewInfo: TcxCustomGridViewInfo read FViewInfo;
    property ViewInfoCache: TcxCustomGridViewInfoCache read FViewInfoCache;
    property Visible: Boolean read GetVisible;
  published
    property DataControllerEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property DragMode: TDragMode read GetDragMode write SetDragMode default dmManual;
    property PopupMenu: TComponent read GetPopupMenu write SetPopupMenu;
    property StylesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property Synchronization: Boolean read GetSynchronization write SetSynchronization default True{False};

    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnDragDrop: TDragDropEvent read GetOnDragDrop write SetOnDragDrop;
    property OnDragOver: TDragOverEvent read GetOnDragOver write SetOnDragOver;
    property OnEndDrag: TEndDragEvent read GetOnEndDrag write SetOnEndDrag;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read GetOnMouseWheel write SetOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read GetOnMouseWheelDown write SetOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read GetOnMouseWheelUp write SetOnMouseWheelUp;
    property OnStartDrag: TStartDragEvent read GetOnStartDrag write SetOnStartDrag;

    property OnGetStoredProperties: TcxGridViewGetStoredPropertiesEvent
      read FOnGetStoredProperties write SetOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGridViewGetStoredPropertyValueEvent
      read FOnGetStoredPropertyValue write SetOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxGridViewSetStoredPropertyValueEvent
      read FOnSetStoredPropertyValue write SetOnSetStoredPropertyValue;
  end;

  { TcxCustomGridViewAccess }

  TcxCustomGridViewAccess = class
  public
    class procedure AddClone(AInstance: TcxCustomGridView; AClone: TcxCustomGridView); static;
    class procedure AssignPattern(AInstance: TcxCustomGridView; APattern: TcxCustomGridView); static;
    class function CanBeUsedAsDetail(AInstance: TcxCustomGridView): Boolean; static;
    class function CanBeUsedAsMaster(AInstance: TcxCustomGridView): Boolean; static;
    class function CanFocus(AInstance: TcxCustomGridView): Boolean; static;
    class procedure Deactivate(AInstance: TcxCustomGridView); static;
    class procedure DetailVisibleChanged(AInstance: TcxCustomGridView;
      ADetailLevel: TComponent; APrevVisibleDetailCount, AVisibleDetailCount: Integer); static;
    class procedure FocusChanged(AInstance: TcxCustomGridView; AFocused: Boolean); static;
    class procedure LookAndFeelChanged(AInstance: TcxCustomGridView); static;
    class procedure SetLevel(AInstance: TcxCustomGridView; Value: TComponent); static;

    class function GetChangeable(AInstance: TcxCustomGridView): Boolean; static;
    class function GetStyles(AInstance: TcxCustomGridView): TcxCustomGridStyles; static;

    class procedure AfterDestroyingLockedStateImage(AInstance: TcxCustomGridView); static;
    class procedure BeforeCreatingLockedStateImage(AInstance: TcxCustomGridView); static;
  end;

function GetViewItemUniqueName(AView: TcxCustomGridView; AItem: TComponent;
  const AItemName: string): string;
function GetGridViewDataController(AView: TcxCustomGridView): TcxCustomDataController;
function GetParentGridView(AControl: TControl): TcxCustomGridView;

var
  cxGridRegisteredViews: TcxRegisteredClasses;

implementation

uses
  RTLConsts,
  SysUtils, Math, cxScrollBar, cxVariants, cxLibraryConsts,
  cxGrid, cxGridLevel, cxGridStrs, dxDPIAwareUtils;

const
  DesignSelectorWidth = 20;
  DesignSelectorHeight = 20;

type
  TCustomFormAccess = class(TCustomForm);
  TcxControlAccess = class(TcxControl);
  TcxCustomGridAccess = class(TcxCustomGrid);
  TcxGridViewRepositoryAccess = class(TcxGridViewRepository);

{ TGridHitTests }

type
  TGridHitTests = class
  private
    FItems: TList;
    function GetCount: Integer; inline;
    function GetInstance(AClass: TcxCustomGridHitTestClass): TcxCustomGridHitTest;
    function GetItem(Index: Integer): TcxCustomGridHitTest; inline;
  protected
    function GetObjectByClass(AClass: TcxCustomGridHitTestClass): TcxCustomGridHitTest;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxCustomGridHitTest read GetItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Instances[AClass: TcxCustomGridHitTestClass]: TcxCustomGridHitTest read GetInstance; default;
  end;

var
  GridHitTests: TGridHitTests;

constructor TGridHitTests.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FItems.Capacity := 32;
end;

destructor TGridHitTests.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Free;
  inherited Destroy;
end;

function TGridHitTests.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGridHitTests.GetInstance(AClass: TcxCustomGridHitTestClass): TcxCustomGridHitTest;
begin
  Result := GetObjectByClass(AClass);
  if Result = nil then
  begin
    Result := AClass.Create;
    FItems.Add(Result);
  end;
end;

function TGridHitTests.GetItem(Index: Integer): TcxCustomGridHitTest;
begin
  Result := TcxCustomGridHitTest(FItems[Index]);
end;

function TGridHitTests.GetObjectByClass(AClass: TcxCustomGridHitTestClass): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ClassType = AClass then Exit;
  end;
  Result := nil;
end;

{ TcxCustomGridViewChange }

constructor TcxCustomGridViewChange.Create(AGridView: TcxCustomGridView);
begin
  inherited Create;
  FGridView := AGridView;
end;

function TcxCustomGridViewChange.IsEqual(AChange: TcxCustomGridChange): Boolean;
begin
  Result := inherited IsEqual(AChange) and
    (FGridView = TcxCustomGridViewChange(AChange).GridView);
end;

{ TcxGridControlFocusChange }

procedure TcxGridControlFocusChange.Execute;
begin
  if GridView.Changeable then
    GridView.Controller.ControlFocusChanged;
end;

{ TcxCustomGridHitTest }

destructor TcxCustomGridHitTest.Destroy;
begin
  ViewInfo := nil;
  inherited Destroy;
end;

procedure TcxCustomGridHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  if Source is TcxCustomGridHitTest then
  begin
    FPos := TcxCustomGridHitTest(Source).FPos;
    FViewInfo := TcxCustomGridHitTest(Source).FViewInfo;
  end;
end;

procedure TcxCustomGridHitTest.SetViewInfo(Value: TcxCustomGridCellViewInfo);
var
  APrevViewInfo: TcxCustomGridCellViewInfo;
begin
  if not FIsClone and (FViewInfo <> Value) then
  begin
    APrevViewInfo := FViewInfo;
    FViewInfo := Value;
    if (APrevViewInfo <> nil) and (APrevViewInfo.LinkedHitTest = Self) then
      APrevViewInfo.LinkedHitTest := nil;
    if FViewInfo <> nil then
      FViewInfo.LinkedHitTest := Self;
  end;
end;

class function TcxCustomGridHitTest.GetHitTestCode: Integer;
begin
  Result := htError;
end;

procedure TcxCustomGridHitTest.Init(const APos: TPoint);
begin
  FPos := APos;
end;

function TcxCustomGridHitTest.Clone: TcxCustomGridHitTest;
begin
  Result := TcxCustomGridHitTestClass(ClassType).Create;
  Result.FIsClone := True;
  Result.Assign(Self);
end;

function TcxCustomGridHitTest.Cursor: TCursor;
begin
  Result := crDefault;
end;

function TcxCustomGridHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := nil;
end;

class function TcxCustomGridHitTest.HitTestCode: Integer;
begin
  Result := GetHitTestCode;
end;

class function TcxCustomGridHitTest.Instance(const APos: TPoint): TcxCustomGridHitTest;
begin
  Result := GridHitTests.Instances[Self];
  Result.Init(APos);
end;

{ TcxGridHitTestNone }

class function TcxGridNoneHitTest.GetHitTestCode: Integer;
begin
  Result := htNone;
end;

{ TcxGridViewNoneHitTest }

class function TcxGridViewNoneHitTest.GetHitTestCode: Integer;
begin
  Result := htNone;
end;

{ TcxCustomGridViewHitTest }

procedure TcxCustomGridViewHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxCustomGridViewHitTest then
    FGridView := TcxCustomGridViewHitTest(Source).FGridView;
end;

{ TcxGridNavigatorHitTest }

class function TcxGridNavigatorHitTest.GetHitTestCode: Integer;
begin
  Result := htNavigator;
end;

{ TcxGridCustomizationFormHitTest }

class function TcxGridCustomizationFormHitTest.GetHitTestCode: Integer;
begin
  Result := htCustomizationForm;
end;

{ TcxGridDesignSelectorHitTest }

class function TcxGridDesignSelectorHitTest.GetHitTestCode: Integer;
begin
  Result := htDesignSelector;
end;

function TcxGridDesignSelectorHitTest.Cursor: TCursor;
begin
  Result := crcxHandPoint;
end;

{ TcxGridViewHandler }

constructor TcxGridViewHandler.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(nil);
  FGridView := AGridView;
end;

function TcxGridViewHandler.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FGridView.LookAndFeelPainter;
end;

function TcxGridViewHandler.GetControl: TcxControl;
begin
  Result := FGridView.Control;
end;

function TcxGridViewHandler.GetController: TcxCustomGridController;
begin
  Result := FGridView.Controller;
end;

function TcxGridViewHandler.GetDataController: TcxCustomDataController;
begin
  Result := FGridView.DataController;
end;

function TcxGridViewHandler.GetPainter: TcxCustomGridPainter;
begin
  Result := FGridView.Painter;
end;

function TcxGridViewHandler.GetSite: TcxGridSite;
begin
  Result := FGridView.Site;
end;

function TcxGridViewHandler.GetViewData: TcxCustomGridViewData;
begin
  Result := FGridView.ViewData;
end;

function TcxGridViewHandler.GetViewInfo: TcxCustomGridViewInfo;
begin
  Result := FGridView.ViewInfo;
end;

procedure TcxGridViewHandler.BeginUpdate(
  AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
begin
  FGridView.BeginUpdate(AShowLockedStateImage);
end;

procedure TcxGridViewHandler.EndUpdate;
begin
  FGridView.EndUpdate;
end;

function TcxGridViewHandler.UseRightToLeftAlignment: Boolean;
begin
  Result := FGridView.UseRightToLeftAlignment;
end;

{ TcxCustomGridDragAndDropObject }

function TcxCustomGridDragAndDropObject.GetController: TcxCustomGridController;
begin
  Result := GridView.Controller;
end;

function TcxCustomGridDragAndDropObject.GetGridView: TcxCustomGridView;
begin
  Result := TcxGridSite(Control).GridView;
end;

function TcxCustomGridDragAndDropObject.GetViewInfo: TcxCustomGridViewInfo;
begin
  Result := GridView.ViewInfo;
end;

procedure TcxCustomGridDragAndDropObject.AfterDragAndDrop(Accepted: Boolean);
begin
  inherited;
  if Accepted then Controller.DesignerModified;
end;

procedure TcxCustomGridDragAndDropObject.AfterPaint;
begin
  AfterViewChange;
end;

procedure TcxCustomGridDragAndDropObject.BeforePaint;
begin
  BeforeViewChange;
end;

procedure TcxCustomGridDragAndDropObject.AfterScrolling;
begin
end;

procedure TcxCustomGridDragAndDropObject.BeforeScrolling;
begin
end;

procedure TcxCustomGridDragAndDropObject.AfterViewChange;
begin
end;

procedure TcxCustomGridDragAndDropObject.BeforeViewChange;
begin
end;

procedure TcxCustomGridDragAndDropObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  SourcePoint := P;
end;

{ TcxCustomGridMovingObject }

function TcxCustomGridMovingObject.GetArrowPlace(AArrowNumber: TcxGridArrowNumber): TcxGridArrowPlace;
begin
  if AreArrowsVertical then
    if AArrowNumber = anFirst then
      Result := apTop
    else
      Result := apBottom
  else
    if AArrowNumber = anFirst then
      Result := apLeft
    else
      Result := apRight;
end;

function TcxCustomGridMovingObject.GetCustomizationForm: TcxCustomGridCustomizationForm;
begin
  Result := Controller.CustomizationForm as TcxCustomGridCustomizationForm;
end;

function TcxCustomGridMovingObject.HasArrows: Boolean;
var
  AArrowNumber: TcxGridArrowNumber;
begin
  Result := True;
  for AArrowNumber := Low(Arrows) to High(Arrows) do
    if Arrows[AArrowNumber] = nil then
    begin
      Result := False;
      Break;
    end;
end;

procedure TcxCustomGridMovingObject.DirtyChanged;
begin
  inherited;
  ChangeArrowsPosition(not Dirty);
  ChangeDragImagePosition(not Dirty);
end;

function TcxCustomGridMovingObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crArrow
  else
    if CanRemove then
      Result := crcxGridRemove
    else
      Result := crcxGridNoDrop;
end;

procedure TcxCustomGridMovingObject.ChangeArrowsPosition(AVisible: Boolean = True);
var
  AArrowNumber: TcxGridArrowNumber;
begin
  if not HasArrows then
    Exit;
  if AVisible and not IsValidDestination then
    AVisible := False;
  for AArrowNumber := Low(Arrows) to High(Arrows) do
  begin
    if AVisible then
      Arrows[AArrowNumber].Init(GridView.Site, GetArrowAreaBounds(ArrowPlaces[AArrowNumber]),
        ArrowsClientRect, ArrowPlaces[AArrowNumber]);
    Arrows[AArrowNumber].Visible := AVisible;
  end;
end;

procedure TcxCustomGridMovingObject.ChangeDragImagePosition(AVisible: Boolean = True);
begin
  if AVisible then
    DragImage.MoveTo(Control.ClientToScreen(CurMousePos));
  DragImage.Visible := AVisible;
end;

function TcxCustomGridMovingObject.AreArrowsVertical: Boolean;
begin
  Result := True;
end;

function TcxCustomGridMovingObject.GetArrowClass: TcxDragAndDropArrowClass;
begin
  Result := TcxDragAndDropArrow;
end;

function TcxCustomGridMovingObject.GetArrowsClientRect: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TcxCustomGridMovingObject.GetDragImageClass: TcxDragImageClass;
begin
  Result := TcxDragImage;
end;

function TcxCustomGridMovingObject.GetSourceItemBounds: TRect;
begin
  if SourceItemViewInfo <> nil then
    Result := SourceItemViewInfo.Bounds
  else
    if IsSourceCustomizationForm and (CustomizationFormListBox <> nil) then
    begin
      with CustomizationFormListBox do
        Result := dxMapWindowRect(Handle, Control.Handle, ItemRect(IndexOfItem(SourceItem)));
    end
    else
      Result := cxNullRect;
end;

function TcxCustomGridMovingObject.GetSourceItemViewInfo: TcxCustomGridCellViewInfo;
begin
  Result := nil;
end;

procedure TcxCustomGridMovingObject.InitDragImage;

  procedure InitUsingSourceItemViewInfo;
  var
    AViewInfo: TcxCustomGridCellViewInfo;
  begin
    AViewInfo := SourceItemViewInfo;
    with AViewInfo.Bounds.TopLeft do
      SetViewportOrgEx(DragImage.Canvas.Handle, -X, -Y, nil);
    AViewInfo.Paint(DragImage.Canvas);
  end;

  procedure InitFromCustomizationForm;
  var
    R: TRect;
  begin
    R := SourceItemBounds;
    with R do
      OffsetRect(R, -Left, -Top);
    InitDragImageUsingCustomizationForm(DragImage.Canvas, R, SourceItem);
  end;

begin
  DragImage.Init(SourceItemBounds, SourcePoint);
  if SourceItemViewInfo <> nil then
    InitUsingSourceItemViewInfo
  else
    if IsSourceCustomizationForm then
      InitFromCustomizationForm;
end;

procedure TcxCustomGridMovingObject.InitDragImageUsingCustomizationForm(ACanvas: TcxCanvas;
  const R: TRect; AItem: TObject);
begin
  if CustomizationFormListBox <> nil then
    CustomizationFormListBox.PaintDragAndDropItem(ACanvas, R, AItem);
end;

procedure TcxCustomGridMovingObject.InitDragObjects;
var
  AArrowNumber: TcxGridArrowNumber;
begin
  FDragImage := GetDragImageClass.Create;
  InitDragImage;
  for AArrowNumber := Low(Arrows) to High(Arrows) do
  begin
    Arrows[AArrowNumber] := GetArrowClass.Create(DragImage.AlphaBlended);
    Arrows[AArrowNumber].BiDiMode := Control.BiDiMode;
  end;
end;

procedure TcxCustomGridMovingObject.BeginDragAndDrop;
begin
  InitDragObjects;
  GridView.Control.UpdateWithChildren;
  inherited;
end;

procedure TcxCustomGridMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited;
  ChangeDragImagePosition;
end;

procedure TcxCustomGridMovingObject.EndDragAndDrop(Accepted: Boolean);
var
  AArrowNumber: TcxGridArrowNumber;
begin
  inherited EndDragAndDrop(Accepted);
  for AArrowNumber := Low(Arrows) to High(Arrows) do
    FreeAndNil(Arrows[AArrowNumber]);
  FreeAndNil(FDragImage);
end;

procedure TcxCustomGridMovingObject.AfterScrolling;
begin
  ChangeArrowsPosition;
  inherited;
end;

{ TcxCustomGridItemsInnerListBox }

constructor TcxCustomGridItemsInnerListBox.Create(AOwner: TComponent);
begin
  inherited;
  Sorted := True;
  Style := lbOwnerDrawFixed;
  CalculateBorderStyle;
  FDragAndDropItemIndex := -1;
end;

function TcxCustomGridItemsInnerListBox.GetContainer: TcxCustomGridItemsListBox;
begin
  Result := TcxCustomGridItemsListBox(inherited Container);
end;

function TcxCustomGridItemsInnerListBox.GetDragAndDropItem: TObject;
begin
  Result := Items.Objects[FDragAndDropItemIndex];
end;

function TcxCustomGridItemsInnerListBox.GetGridView: TcxCustomGridView;
begin
  Result := Container.GridView;
end;

function TcxCustomGridItemsInnerListBox.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

procedure TcxCustomGridItemsInnerListBox.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

procedure TcxCustomGridItemsInnerListBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if BorderStyle = bsNone then
    InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;

procedure TcxCustomGridItemsInnerListBox.WMNCPaint(var Message: TWMNCPaint);
var
  ACanvas: TCanvas;
  DC: HDC;
  R: TRect;
begin
  inherited;
  if BorderStyle = bsNone then
  begin
    R := cxGetWindowRect(Handle);
    OffsetRect(R, -R.Left, -R.Top);
    DC := GetWindowDC(Handle);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Lock;
      try
        ACanvas.Handle := DC;
        DrawBorder(ACanvas, R);
        ACanvas.Handle := 0;
      finally
        ACanvas.Unlock;
      end;
    finally
      ACanvas.Free;
    end;
    ReleaseDC(Handle, DC);
  end;
end;

procedure TcxCustomGridItemsInnerListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    WindowClass.Style := WindowClass.Style or CS_HREDRAW;
end;

procedure TcxCustomGridItemsInnerListBox.CreateWnd;
begin
  inherited;
  CalculateItemHeight;
end;

procedure TcxCustomGridItemsInnerListBox.DrawBorder(ACanvas: TCanvas; R: TRect);
var
  AcxCanvas: TcxCanvas;
begin
  if LookAndFeel.SkinPainter = nil then
    DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_RECT)
  else
  begin
    AcxCanvas := TcxCanvas.Create(ACanvas);
    try
      LookAndFeel.SkinPainter.DrawBorder(AcxCanvas, R);
    finally
      AcxCanvas.Free;
    end;
  end;
end;

procedure TcxCustomGridItemsInnerListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  Container.PaintItem(Canvas, Rect, Index, odFocused in State);
  if odFocused in State then Canvas.DrawFocusRect(Rect);
end;

procedure TcxCustomGridItemsInnerListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
    GridView.Site.FinishDragAndDrop(False);
end;

procedure TcxCustomGridItemsInnerListBox.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  CalculateBorderStyle;
  CalculateItemHeight;
  inherited;
end;

procedure TcxCustomGridItemsInnerListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AItemIndex: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    AItemIndex := ItemAtPos(Point(X, Y), True);
    if AItemIndex <> -1 then
    begin
      FDragAndDropItemIndex := AItemIndex;
      FMouseDownPos := Point(X, Y);
    end;
  end;
end;

procedure TcxCustomGridItemsInnerListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AParams: TcxCustomGridHitTest;
  P: TPoint;
begin
  inherited;
  if (FDragAndDropItemIndex <> -1) and
    (not IsPointInDragDetectArea(FMouseDownPos, X, Y) or
     (ItemAtPos(Point(X, Y), False) <> FDragAndDropItemIndex)) then
  begin
    ItemIndex := FDragAndDropItemIndex;
    AParams := Container.GetDragAndDropParams;
    with GridView do
    begin
      P := FMouseDownPos;
      P := Site.ScreenToClient(ClientToScreen(P));
      Controller.DragAndDropObjectClass := AParams.DragAndDropObjectClass;
      Controller.DragAndDropObject.Init(P, AParams);
      Site.BeginDragAndDrop;
    end;
    FDragAndDropItemIndex := -1;
  end;
end;

procedure TcxCustomGridItemsInnerListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FDragAndDropItemIndex := -1;
end;

procedure TcxCustomGridItemsInnerListBox.CalculateBorderStyle;
begin
  if LookAndFeelPainter.HeaderBorderSize <= 1 then
    BorderStyle := bsNone
  else
    BorderStyle := bsSingle;
end;

procedure TcxCustomGridItemsInnerListBox.CalculateItemHeight;
begin
  if HandleAllocated then
    ItemHeight := Container.CalculateItemHeight;
end;

{ TcxCustomGridItemsListBox }

constructor TcxCustomGridItemsListBox.Create(AOwner: TComponent);
begin
  inherited;
  with Style do
  begin
    BorderStyle := cbsNone;
    HotTrack := False;
    LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
    TransparentBorder := False;
  end;
  StyleFocused.BorderStyle := cbsNone;
  StyleHot.BorderStyle := cbsNone;
  ParentFont := True;
end;

function TcxCustomGridItemsListBox.GetDragAndDropItem: TObject;
begin
  Result := InnerListBox.DragAndDropItem;
end;

function TcxCustomGridItemsListBox.GetGridView: TcxCustomGridView;
begin
  Result := (Owner as TcxCustomGridCustomizationForm).GridView;
end;

function TcxCustomGridItemsListBox.GetInnerListBox: TcxCustomGridItemsInnerListBox;
begin
  Result := TcxCustomGridItemsInnerListBox(inherited InnerListBox);
end;

function TcxCustomGridItemsListBox.GetInnerListBoxClass: TcxInnerListBoxClass;
begin
  Result := TcxCustomGridItemsInnerListBox;
end;

function TcxCustomGridItemsListBox.IndexOfItem(AItem: TObject): Integer;
begin
  Result := Items.IndexOfObject(AItem);
end;

procedure TcxCustomGridItemsListBox.PaintDragAndDropItem(ACanvas: TcxCanvas;
  const R: TRect; AItem: TObject);
begin
  ACanvas.Brush.Color := Color;
  ACanvas.Font := Font;
  PaintItem(ACanvas, R, IndexOfItem(AItem), False);
end;

procedure TcxCustomGridItemsListBox.RefreshItems;
var
  ATopIndex, AItemIndex: Integer;
begin
  InnerListBox.CalculateItemHeight;

  ATopIndex := TopIndex;
  AItemIndex := ItemIndex;
  Items.BeginUpdate;
  try
    DoRefreshItems;
    TopIndex := ATopIndex;
    ItemIndex := Min(AItemIndex, Items.Count - 1);
  finally
    Items.EndUpdate;
  end;
end;

{ TcxCustomGridCustomizationForm }

// TODO: system menu

constructor TcxCustomGridCustomizationForm.Create(AController: TcxCustomGridController);
begin
  FController := AController;
  inherited CreateNew(nil);
  BorderStyle := bsSizeToolWin;
  HandleNeeded;
  Controller.InitializeCustomizationForm(Self);
  CalculateConsts;
  CreateControls;
end;

destructor TcxCustomGridCustomizationForm.Destroy;
begin
  FHookTimer.Free;
  FController.Customization := False;
  inherited Destroy;
end;

function TcxCustomGridCustomizationForm.GetGridView: TcxCustomGridView;
begin
  Result := FController.GridView;
end;

function TcxCustomGridCustomizationForm.GetViewInfo: TcxCustomGridViewInfo;
begin
  Result := FController.ViewInfo;
end;

procedure TcxCustomGridCustomizationForm.HookTimerHandler(Sender: TObject);
begin
  if Parent <> nil then
    Exit;

  if IsIconic(Application.Handle) then
    Visible := False
  else
    if not IsControlVisible(Controller.Site) then
      FController.Customization := False
    else
      if not Visible then
      begin
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
        Visible := True;
      end;
end;

procedure TcxCustomGridCustomizationForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Parent = nil then
  begin
    Params.Style := Params.Style or WS_POPUP;
    Params.WndParent := FController.Site.Handle;
  end;
end;

procedure TcxCustomGridCustomizationForm.DoClose(var Action: TCloseAction);
begin
  FreeAndNil(FHookTimer);
  Action := caFree;
end;

procedure TcxCustomGridCustomizationForm.DoShow;
begin
  if FHookTimer = nil then
  begin
    FHookTimer := TcxTimer.Create(nil);
    FHookTimer.Interval := 100;
    FHookTimer.OnTimer := HookTimerHandler;
  end;
  inherited DoShow;
end;

procedure TcxCustomGridCustomizationForm.CalculateConsts;
begin
  FOffset := cxTextHeight(Canvas.Handle) div 6;
end;

procedure TcxCustomGridCustomizationForm.CreateControls;
begin
  FPageControl := TcxPageControl.Create(Self);
  FPageControl.Anchors := [akTop, akLeft, akRight, akBottom];
  FPageControl.Focusable := False;
  FPageControl.HotTrack := True;
  FPageControl.LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
  FPageControl.Parent := Self;
  FPageControl.BoundsRect := GetPageControlBounds;
  InitPageControl;
end;

function TcxCustomGridCustomizationForm.CreatePage(const ACaption: string; AVisible: Boolean): TcxTabSheet;
begin
  Result := TcxTabSheet.Create(FPageControl);
  Result.Caption := ACaption;
  Result.TabVisible := AVisible;
  Result.PageControl := FPageControl;
end;

procedure TcxCustomGridCustomizationForm.GridViewChanged;
begin
end;

function TcxCustomGridCustomizationForm.GetContentBounds: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -Offset, -Offset);
end;

function TcxCustomGridCustomizationForm.GetController: TcxCustomGridController;
begin
  Result := FController;
end;

function TcxCustomGridCustomizationForm.GetPageControlBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxCustomGridCustomizationForm.HasDFM: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridCustomizationForm.Initialize(AController: TcxCustomGridController);
begin
//do nothing
end;

procedure TcxCustomGridCustomizationForm.InitPageControl;
begin
end;

procedure TcxCustomGridCustomizationForm.ActivatePage(APage: TcxTabSheet);
begin
  if APage.TabVisible then
    FPageControl.ActivePage := APage;
end;

procedure TcxCustomGridCustomizationForm.RefreshData;
begin
end;

{ TcxCustomGridPopup }

constructor TcxCustomGridPopup.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView.Site);
  FGridView := AGridView;
end;

{ TcxGridPopupListBox }

constructor TcxGridPopupListBox.Create(APopup: TcxCustomGridPopup);
begin
  inherited Create(APopup);
  BorderStyle := cxcbsNone;
  LookAndFeel.MasterLookAndFeel := Popup.LookAndFeel;
  ShowCheckBoxes := False;
  Parent := Popup;
end;

procedure TcxGridPopupListBox.DoItemAction(AItemIndex: Integer);
begin
  inherited DoItemAction(AItemIndex);
  if not HasCheckBox(AItemIndex) then
    Popup.CloseUp;
end;

procedure TcxGridPopupListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_ESCAPE:
      Popup.CloseUp;
  end;
end;

function TcxGridPopupListBox.NeedHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridPopupListBox.GetPopup: TcxCustomGridPopup;
begin
  Result := TcxCustomGridPopup(Owner);
end;

{ TcxCustomGridDesignController }

function TcxCustomGridDesignController.GetDesignObject(AObject: TPersistent): TPersistent;
begin
  Result := AObject;
end;

function TcxCustomGridDesignController.CanAddComponent: Boolean;
begin
  Result := (TcxCustomGrid(Control).StructureNavigator <> nil) and
    TcxCustomGrid(Control).StructureNavigator.CanAddComponent;
end;

function TcxCustomGridDesignController.CanDeleteComponent(AComponent: TComponent): Boolean;
begin
  Result := (TcxCustomGrid(Control).StructureNavigator <> nil) and
    TcxCustomGrid(Control).StructureNavigator.CanDeleteComponent(AComponent);
end;

procedure TcxCustomGridDesignController.GetSelection(AList: TList);
begin
  if TcxCustomGrid(Control).StructureNavigator <> nil then
    TcxCustomGrid(Control).StructureNavigator.GetSelection(AList);
end;

function TcxCustomGridDesignController.IsObjectSelected(AObject: TPersistent): Boolean;
begin
  if TcxCustomGrid(Control).StructureNavigator = nil then
    Result := False
  else
    Result := TcxCustomGrid(Control).StructureNavigator.IsObjectSelected(GetDesignObject(AObject));
end;

procedure TcxCustomGridDesignController.NotifyEditors;
begin
  if TcxCustomGrid(Control).StructureNavigator <> nil then
    TcxCustomGrid(Control).StructureNavigator.NotifyEditors;
end;

procedure TcxCustomGridDesignController.SelectObject(AObject: TPersistent; AClearSelection: Boolean);
begin
  TcxCustomGrid(Control).StructureNavigator.SelectObject(GetDesignObject(AObject), AClearSelection);
end;

procedure TcxCustomGridDesignController.SelectObjects(AObjects: TList);
var
  I: Integer;
begin
  for I := 0 to AObjects.Count - 1 do
    AObjects[I] := GetDesignObject(AObjects[I]);
  TcxCustomGrid(Control).StructureNavigator.SelectObjects(AObjects);
end;

procedure TcxCustomGridDesignController.UnselectObject(AObject: TPersistent);
begin
  if Control <> nil then
    TcxCustomGrid(Control).StructureNavigator.UnselectObject(GetDesignObject(AObject));
end;

{ TcxGridViewDesignController }

constructor TcxGridViewDesignController.Create(AController: TcxCustomGridController);
begin
  inherited Create;
  FController := AController;
end;

function TcxGridViewDesignController.GetControl: TcxControl;
begin
  Result := FController.Control;
end;

function TcxGridViewDesignController.GetDesignObject(AObject: TPersistent): TPersistent;
begin
  if FController.GridView.IsPattern then
    Result := inherited GetDesignObject(AObject)
  else
    Result := FController.GetPatternObject(AObject);
end;

{ TcxGridHintHelper }

constructor TcxGridHintHelper.Create(AController: TcxCustomGridController);
begin
  inherited Create;
  FController := AController;
end;

function TcxGridHintHelper.GetHintControl: TcxControl;
begin
  Result := FController.Site;
end;

function TcxGridHintHelper.GetHintHidePause: Integer;
begin
  if UseHintHidePause then
    Result := OptionsBehavior.HintHidePause
  else
    Result := MaxInt;
end;

function TcxGridHintHelper.GetHintWindowClass: THintWindowClass;
begin
  Result := FController.GetHintWindowClass;
end;

function TcxGridHintHelper.GetOptionsBehavior: TcxCustomGridOptionsBehavior;
begin
  Result := FController.GridView.OptionsBehavior;
end;

function TcxGridHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FController.Control;
end;

function TcxGridHintHelper.IsSuppressHintOnMouseDown: Boolean;
begin
  Result := FController.IsSuppressHintOnMouseDown;
end;

{ TcxCustomGridController }

constructor TcxCustomGridController.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FHintHelper := GetHintHelperClass.Create(Self);
end;

destructor TcxCustomGridController.Destroy;
begin
  FreeAndNil(FHintHelper);
  FreeAndNil(FDesignController);
  inherited Destroy;
end;

function TcxCustomGridController.GetDesignController: TcxGridViewDesignController;
begin
  if (FDesignController = nil) and GridView.IsDesigning then
    FDesignController := GetDesignControllerClass.Create(Self);
  Result := FDesignController;
end;

function TcxCustomGridController.GetDragAndDropObject: TcxCustomGridDragAndDropObject;
begin
  Result := Site.DragAndDropObject as TcxCustomGridDragAndDropObject;
end;

function TcxCustomGridController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := Site.DragAndDropObjectClass;
end;

function TcxCustomGridController.GetDragImages: TcxDragImageList;
begin
  Result := Site.DragImages;
end;

function TcxCustomGridController.GetHintCellViewInfo: TcxCustomGridViewCellViewInfo;
begin
  Result := HintHelper.HintableObject as TcxCustomGridViewCellViewInfo;
end;

function TcxCustomGridController.GetHintWindow: THintWindow;
begin
  Result := HintHelper.HintWindow;
end;

function TcxCustomGridController.GetICustomizationForm: IcxGridCustomizationForm;
begin
  if CustomizationForm = nil then
    Result := nil
  else
    Supports(CustomizationForm, IcxGridCustomizationForm, Result);
end;

function TcxCustomGridController.GetIsDragging: Boolean;
begin
  Result := Site.DragAndDropState = ddsInProcess;
end;

function TcxCustomGridController.GetMouseCaptureViewInfo: TcxCustomGridCellViewInfo;
begin
  if Site.MouseCaptureObject is TcxCustomGridCellViewInfo then
    Result := TcxCustomGridCellViewInfo(Site.MouseCaptureObject)
  else
    Result := nil;
end;

procedure TcxCustomGridController.SetCustomization(Value: Boolean);
begin
  if Value and not CanCustomize then Exit;
  if FCustomization <> Value then
  begin
    FCustomization := Value;
    CustomizationChanged;
  end;
end;

procedure TcxCustomGridController.SetDragAndDropObjectClass(Value: TcxDragAndDropObjectClass);
begin
  Site.DragAndDropObjectClass := Value;
end;

procedure TcxCustomGridController.SetMouseCaptureViewInfo(Value: TcxCustomGridCellViewInfo);
begin
  Site.MouseCaptureObject := Value;
end;

procedure TcxCustomGridController.AfterPaint;
begin
  if IsDragging then
    DragAndDropObject.AfterPaint;
end;

function TcxCustomGridController.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridController.BeforePaint;
begin
  if IsDragging then
    DragAndDropObject.BeforePaint;
end;

function TcxCustomGridController.CanCancelDragStartOnCaptureObjectClear: Boolean;
begin
  Result := True;
end;

function TcxCustomGridController.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := True;
end;

function TcxCustomGridController.CanHandleHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := not (AHitTest is TcxCustomGridViewHitTest) or
    (TcxCustomGridViewHitTest(AHitTest).GridView = FGridView);
end;

procedure TcxCustomGridController.CheckCoordinates;
begin
end;

function TcxCustomGridController.CreateFieldControls(X, Y: Integer;
  ADataSource: TObject; AFieldList: TList): Boolean;
var
  AIntf: IcxEditorFieldLink;
begin
  if Supports(DataController, IcxEditorFieldLink, AIntf) then
    Result := AIntf.CreateFieldControls(X, Y, ADataSource, AFieldList)
  else
    Result := True;
end;

procedure TcxCustomGridController.DetailFocused(ADetail: TcxCustomGridView);
begin
end;

procedure TcxCustomGridController.DoEnter;
begin
end;

procedure TcxCustomGridController.DoExit;
begin
end;

procedure TcxCustomGridController.DoSetFocus(ANotifyMaster: Boolean);
begin
  FIsFocusing := True;
  try
    SetFocus(ANotifyMaster);
  finally
    FIsFocusing := False;
  end;
end;

function TcxCustomGridController.GetActiveCellViewInfo(
  AHitTest: TcxCustomGridHitTest): TcxCustomGridCellViewInfo;
begin
  if MouseCaptureViewInfo <> nil then
    Result := MouseCaptureViewInfo
  else
    if AHitTest.ViewInfo <> nil then
      Result := AHitTest.ViewInfo
    else
      Result := nil;
end;

function TcxCustomGridController.GetDesignControllerClass: TcxGridViewDesignControllerClass;
begin
  Result := TcxGridViewDesignController;
end;

function TcxCustomGridController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := (AHitTest.DragAndDropObjectClass <> nil) or
    (AHitTest.HitTestCode = htDesignSelector);
end;

function TcxCustomGridController.GetDlgCode: Integer;
begin
  Result := 0;
end;

function TcxCustomGridController.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskNone;
end;

function TcxCustomGridController.GetPatternObject(AObject: TPersistent): TPersistent;
begin
  if AObject is TcxCustomGridView then
    Result := TcxCustomGridView(AObject).PatternGridView
  else
    if AObject is TcxDataSummaryItem then
      Result := DataController.Summary.GetPatternSummaryItems(GridView.PatternGridView.DataController.Summary,
        TcxDataSummaryItem(AObject).SummaryItems)[TcxDataSummaryItem(AObject).Index]
    else
      Result := AObject;
end;

procedure TcxCustomGridController.GridViewChanged;
begin
  if ICustomizationForm <> nil then
    ICustomizationForm.GridViewChanged;
end;

function TcxCustomGridController.IsPixelScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

function TcxCustomGridController.IsSuppressHintOnMouseDown: Boolean;
begin
  Result := GridView.OptionsBehavior.SuppressHintOnMouseDown;
end;

function TcxCustomGridController.MayFocus: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridController.MouseLeave;
begin
  HintHelper.MouseLeave;
end;

function TcxCustomGridController.ProcessDesignPopupMenu(AViewInfo: TcxCustomGridCellViewInfo): Boolean;
var
  AMenu: TPopupMenu;
begin
  Result := AViewInfo.HasDesignPopupMenu;
  if Result then
  begin
    AMenu := TPopupMenu.Create(nil);
    try
      AViewInfo.PopulateDesignPopupMenu(AMenu);
      if AMenu.Items.Count > 0 then
      begin
        FDesignPopupMenuInvoker := AViewInfo;
        try
          with GetMouseCursorPos do
            AMenu.Popup(X, Y);
          Application.ProcessMessages;
        finally
          FDesignPopupMenuInvoker := nil;
        end;
      end;
    finally
      AMenu.Free;
    end;
  end;
end;

procedure TcxCustomGridController.RemoveFocus;
begin
  GridView.Deactivate;
  GridView.TabStop := False;
  if not GridView.Visible then
    Site.Parent := nil;
end;

procedure TcxCustomGridController.SetFocus(ANotifyMaster: Boolean);
begin
  GridView.TabStop := True;
  ViewInfo.DoVisibilityChanged(True);
  if Control.IsFocused and Site.CanFocusEx then
    Site.SetFocus;
  if ANotifyMaster and GridView.IsDetail then
    GridView.MasterGridView.Controller.DetailFocused(GridView);
end;

procedure TcxCustomGridController.SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer);
begin
end;

procedure TcxCustomGridController.VisibilityChanged(AVisible: Boolean);
begin
  if not AVisible then
    Customization := False;
end;

function TcxCustomGridController.WantSpecialKey(AKey: Word): Boolean;
begin
  Result := False;
end;

function TcxCustomGridController.CanCustomize: Boolean;
begin
  Result := not (GridView.IsPattern or not Site.HandleAllocated);
end;

procedure TcxCustomGridController.CheckCustomizationFormBounds(var R: TRect);
begin
end;

function TcxCustomGridController.CreateCustomizationForm: TForm;
begin
  Result := GetCustomizationFormClass.Create(Self);
end;

procedure TcxCustomGridController.CustomizationChanged;
begin
  if Customization then
    ShowCustomizationForm
  else
    HideCustomizationForm;
  DoCustomization;
end;

function TcxCustomGridController.GetCustomizationFormBounds: TRect;
begin
  if IsRectEmpty(FCustomizationFormBounds) then
  begin
    Result.BottomRight := Site.ClientToScreen(Site.ClientRect.BottomRight);
    Result.Left := Result.Right - Site.ScaleFactor.Apply(GetCustomizationFormDefaultWidth);
    Result.Top := Result.Bottom - Site.ScaleFactor.Apply(GetCustomizationFormDefaultHeight);
    CheckCustomizationFormBounds(Result);
  end
  else
    Result := FCustomizationFormBounds;
end;

function TcxCustomGridController.GetCustomizationFormClass: TcxCustomGridCustomizationFormClass;
begin
  Result := TcxCustomGridCustomizationForm;
end;

function TcxCustomGridController.GetCustomizationFormDefaultWidth: Integer;
begin
  Result := cxGridCustomizationFormDefaultWidth;
end;

function TcxCustomGridController.GetCustomizationFormDefaultHeight: Integer;
begin
  Result := cxGridCustomizationFormDefaultHeight;
end;

procedure TcxCustomGridController.HideCustomizationForm;
begin
  if not (csDestroying in FCustomizationForm.ComponentState) then
    FCustomizationForm.Free;
  FCustomizationForm := nil;
end;

procedure TcxCustomGridController.InitializeCustomizationForm(AForm: TForm);
begin
  AForm.Caption := cxGetResourceString(@scxGridCustomizationFormCaption);
  AForm.BoundsRect := GetCustomizationFormBounds;
  if Control <> nil then
  begin
    AForm.Font := TcxControlAccess(Control).Font;
    AForm.BiDiMode := Control.BiDiMode;
  end;
end;

procedure TcxCustomGridController.ShowCustomizationForm;
begin
  DoCreateCustomizationForm;
  FCustomizationForm.Show;
end;

function TcxCustomGridController.GetHintHelperClass: TcxGridHintHelperClass;
begin
  Result := TcxGridHintHelper;
end;

procedure TcxCustomGridController.AfterScrolling;
begin
  FIsScrolling := False;
  if IsControlVisible(Control) then
    SendMessage(Control.Handle, DXM_UIADORNERMANAGERUPDATE, 0, 0);
end;

procedure TcxCustomGridController.BeforeScrolling;
begin
  FIsScrolling := True;
end;

procedure TcxCustomGridController.BeginGestureScroll(APos: TPoint);
begin
end;

procedure TcxCustomGridController.DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
  var AScrollPos: Integer);
begin
end;

procedure TcxCustomGridController.EndGestureScroll;
begin
end;

procedure TcxCustomGridController.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
end;

procedure TcxCustomGridController.DoCreateCustomizationForm;
begin
  if FCustomizationForm = nil then
    FCustomizationForm := CreateCustomizationForm;
end;

procedure TcxCustomGridController.DoCustomization;
begin
  if Control <> nil then
    TcxCustomGrid(Control).SendNotifications(gnkCustomization);
  GridView.DoCustomization;
end;

procedure TcxCustomGridController.ControlFocusChanged;
begin
  ViewInfo.ControlFocusChanged;
end;

procedure TcxCustomGridController.DesignerModified;
begin
  Site.Modified;
end;

procedure TcxCustomGridController.DoCancelMode;
begin
  HintHelper.ResetLastHintElement;
end;

procedure TcxCustomGridController.DoCheckCoordinates;
var
  APrevIsCheckingCoordinates: Boolean;
begin
  APrevIsCheckingCoordinates := FIsCheckingCoordinates;
  FIsCheckingCoordinates := True;
  try
    CheckCoordinates;
  finally
    FIsCheckingCoordinates := APrevIsCheckingCoordinates;
  end;
end;

procedure TcxCustomGridController.DoControlFocusChanged;
begin
  GridView.Changed(TcxGridControlFocusChange.Create(GridView));
end;

function TcxCustomGridController.GetCursor(X, Y: Integer): TCursor;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := crDefault;
  if not IsScrolling and GridView.CanGetHitTest then
  begin
    AHitTest := ViewInfo.GetHitTest(Point(X, Y));
    if CanHandleHitTest(AHitTest) then
      Result := AHitTest.Cursor;
  end;
end;

function TcxCustomGridController.HasFocusedControls: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridController.InitScrollBarsParameters;
begin
end;

function TcxCustomGridController.IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridController.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  BeforeScrolling;
  DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
  AfterScrolling;
end;

procedure TcxCustomGridController.SetScrollBarInfo(AScrollBarKind: TScrollBarKind;
  AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);
begin
  Site.SetScrollBarInfo(AScrollBarKind, AMin, AMax, AStep, APage, APos, AAllowShow, AAllowHide);
end;

procedure TcxCustomGridController.UpdateScrollBars(AIgnoreUpdateLock: Boolean = False);
var
  APrevIgnoreUpdateLock: Boolean;
begin
  APrevIgnoreUpdateLock := Site.IgnoreUpdateLock;
  Site.IgnoreUpdateLock := AIgnoreUpdateLock;
  try
    Site.UpdateScrollBars;
  finally
    Site.IgnoreUpdateLock := APrevIgnoreUpdateLock;
  end;
end;

procedure TcxCustomGridController.BeginDragAndDrop;
begin
end;

procedure TcxCustomGridController.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
end;

procedure TcxCustomGridController.EndDragAndDrop(Accepted: Boolean);
begin
end;

function TcxCustomGridController.StartDragAndDrop(const P: TPoint): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(P);
  Result := CanHandleHitTest(AHitTest);
  if Result then
  begin
    Result := AHitTest.DragAndDropObjectClass <> nil;
    if Result then
    begin
      CancelHint;
      DragAndDropObjectClass := AHitTest.DragAndDropObjectClass;
      DragAndDropObject.Init(P, AHitTest);
    end;
  end;
end;

procedure TcxCustomGridController.BeforeStartDrag;
begin
end;

function TcxCustomGridController.CanDrag(X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridController.DragDrop(Source: TObject; X, Y: Integer);
begin
end;

procedure TcxCustomGridController.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
end;

procedure TcxCustomGridController.DrawDragImage(ACanvas: TcxCanvas; R: TRect);
begin
end;

procedure TcxCustomGridController.EndDrag(Target: TObject; X, Y: Integer);
begin
end;

function TcxCustomGridController.GetDragImagesSize: TPoint;
begin
  Result := Point(0, 0);
end;

function TcxCustomGridController.HasDragImages: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridController.StartDrag(var DragObject: TDragObject);
begin
end;

procedure TcxCustomGridController.CancelHint;
begin
  FHintHelper.CancelHint;
end;

procedure TcxCustomGridController.HideHint;
begin
  FHintHelper.HideHint;
end;

function TcxCustomGridController.GetHintWindowClass: THintWindowClass;
begin
  Result := cxGetHintWindowClass;
end;

procedure TcxCustomGridController.ShowHint(const AHintAreaBounds, ATextRect: TRect;
  const AText: string; AIsHintMultiLine: Boolean; AFont: TFont;
  AHintCellViewInfo: TcxCustomGridViewCellViewInfo);
begin
  HintHelper.ShowHint(AHintAreaBounds, ATextRect, AText, AIsHintMultiLine, AHintCellViewInfo, AFont);
end;

procedure TcxCustomGridController.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

function TcxCustomGridController.IMEComposition(var AMessage: TMessage): Boolean;
begin
  Result := False;
end;

function TcxCustomGridController.IMEStartComposition: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridController.KeyDown(var Key: Word; Shift: TShiftState);
begin
end;

procedure TcxCustomGridController.KeyPress(var Key: Char);
begin
end;

procedure TcxCustomGridController.KeyUp(var Key: Word; Shift: TShiftState);
begin
end;

procedure TcxCustomGridController.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  HintHelper.MouseDown;
  FIsDblClick := ssDouble in Shift;
  AHitTest := ViewInfo.GetHitTest(X, Y);
  if AHitTest.ViewInfo <> nil then
    AHitTest.ViewInfo.MouseDown(AHitTest, Button, Shift);
end;

procedure TcxCustomGridController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  ACellViewInfo: TcxCustomGridCellViewInfo;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  ACellViewInfo := GetActiveCellViewInfo(AHitTest);
  if ACellViewInfo <> nil then
    ACellViewInfo.MouseMove(AHitTest, Shift)
  else
    HintHelper.ResetLastHintElement;
end;

procedure TcxCustomGridController.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  ACellViewInfo: TcxCustomGridCellViewInfo;
begin
  AHitTest := ViewInfo.GetHitTest(X, Y);
  ACellViewInfo := GetActiveCellViewInfo(AHitTest);
  if ACellViewInfo <> nil then
    ACellViewInfo.MouseUp(AHitTest, Button, Shift);
end;

function TcxCustomGridController.ProcessDetailDialogChar(ADetail: TcxCustomGridView;
  ACharCode: Word): Boolean;
begin
  Result := False;
end;

function TcxCustomGridController.ProcessDialogChar(ACharCode: Word): Boolean;
begin
  Result := (GridView.MasterGridView <> nil) and
    GridView.MasterGridView.Controller.ProcessDetailDialogChar(GridView, ACharCode);
end;

procedure TcxCustomGridController.WndProc(var Message: TMessage);
begin
  if (Message.Msg = CN_SYSCHAR) and ProcessDialogChar(Message.WParam) then
    Message.Msg := WM_NULL;
end;

procedure TcxCustomGridController.RefreshCustomizationForm;
begin
  if ICustomizationForm <> nil then
    ICustomizationForm.RefreshData;
end;

{procedure TcxCustomGridController.BeginCellMouseTracking(AViewInfo: TcxCustomGridCellViewInfo);
begin
  BeginMouseTracking(Site, AViewInfo.Bounds, AViewInfo);
end;

procedure TcxCustomGridController.EndCellMouseTracking(AViewInfo: TcxCustomGridCellViewInfo);
begin
  EndMouseTracking(AViewInfo);
end;}

{ TcxDataGroupNode }

constructor TcxDataGroupNode.Create(AParent: TcxDataGroupNode; ADataGroupInfo: TcxDataGroupInfo);
begin
  inherited Create;
  FChildren := TdxFastObjectList.Create;
  FDataGroupInfo := ADataGroupInfo;
  FParent := AParent;
end;

destructor TcxDataGroupNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TcxDataGroupNode.GetChild(Index: Integer): TcxDataGroupNode;
begin
  Result := TcxDataGroupNode(FChildren[Index]);
end;

function TcxDataGroupNode.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TcxDataGroupNode.AddChild(ADataGroupInfo: TcxDataGroupInfo): TcxDataGroupNode;
begin
  Result := TcxDataGroupNode.Create(Self, ADataGroupInfo);
  FChildren.Add(Result);
end;

procedure TcxDataGroupNode.SortChildren(ACompare: TCompareItems);
begin
  FChildren.Sort(ACompare);
end;

{ TcxGridSortingBySummaryEngine }

constructor TcxGridSortingBySummaryEngine.Create(ADataControllerInfo: TcxCustomDataControllerInfo);
begin
  inherited;
  FInfos := TdxFastObjectList.Create;
  FRootNode := TcxDataGroupNode.Create(nil, nil);
end;

destructor TcxGridSortingBySummaryEngine.Destroy;
begin
  FRootNode.Free;
  FInfos.Free;
  inherited Destroy;
end;

function TcxGridSortingBySummaryEngine.GetDataGroups: TcxDataGroups;
begin
  Result := DataControllerInfo.DataGroups;
end;

function TcxGridSortingBySummaryEngine.GetInfo(Index: Integer): TcxSortingBySummaryInfo;
begin
  Result := TcxSortingBySummaryInfo(FInfos[Index]);
end;

function TcxGridSortingBySummaryEngine.GetInfoCount: Integer;
begin
  Result := FInfos.Count;
end;

procedure TcxGridSortingBySummaryEngine.AddInfo(AInfo: TcxSortingBySummaryInfo);
begin
  FInfos.Add(AInfo);
end;

procedure TcxGridSortingBySummaryEngine.BuildNodes(AParentNode: TcxDataGroupNode; ALevel: Integer;
  var ACurIndex: Integer);
var
  ANode: TcxDataGroupNode;
begin
  while ACurIndex < DataGroups.Count do
    if DataGroups[ACurIndex].Level = ALevel then
    begin
      ANode := AParentNode.AddChild(DataGroups[ACurIndex]);
      Inc(ACurIndex);
      if ALevel < DataGroups.LevelCount - 1 then // build children
        BuildNodes(ANode, ALevel + 1, ACurIndex);
    end
    else
      Break;
end;

procedure TcxGridSortingBySummaryEngine.ClearInfos;
begin
  FInfos.Clear;
end;

function TcxGridSortingBySummaryEngine.CompareGroupsBySummary(AInfo1, AInfo2:
  Pointer): Integer;
var
  AInfo: TcxSortingBySummaryInfo;
begin
  AInfo := Infos[TcxDataGroupNode(AInfo1).DataGroupInfo.Level];
  Result := VarCompare(
    TcxDataGroupNode(AInfo1).DataGroupInfo.SummaryValues[AInfo.SummaryItemIndex],
    TcxDataGroupNode(AInfo2).DataGroupInfo.SummaryValues[AInfo.SummaryItemIndex]);
  if Result = 0 then
  begin
    Result := DataControllerInfo.CompareGroupRecords(
      {DataControllerInfo.GetRowInfo(TcxDataGroupNode(AInfo1).DataGroupInfo.RowIndex).RecordIndex,
      DataControllerInfo.GetRowInfo(TcxDataGroupNode(AInfo2).DataGroupInfo.RowIndex).RecordIndex,}
      DataControllerInfo.GetInternalRecordIndex(
        DataGroups.GetDataRecordListIndex(TcxDataGroupNode(AInfo1).DataGroupInfo)),
      DataControllerInfo.GetInternalRecordIndex(
        DataGroups.GetDataRecordListIndex(TcxDataGroupNode(AInfo2).DataGroupInfo)),
      TcxDataGroupNode(AInfo1).DataGroupInfo.Level)
  end
  else
    if AInfo.SortOrder = soDescending then
      Result := -Result;
end;

procedure TcxGridSortingBySummaryEngine.RebuildDataGroupRecursive(AParentNode: TcxDataGroupNode;
  var AFirstRecordListIndex: Integer);
var
  I: Integer;
begin
  DataGroups.SetItem(AFirstRecordListIndex, AParentNode.DataGroupInfo);
  Inc(AFirstRecordListIndex);
  if AParentNode.DataGroupInfo.Level < (DataGroups.LevelCount - 1) then
  begin
    AParentNode.DataGroupInfo.FirstRecordListIndex := AFirstRecordListIndex;
    for I := 0 to AParentNode.ChildCount - 1 do
      RebuildDataGroupRecursive(AParentNode.Children[I], AFirstRecordListIndex);
    AParentNode.DataGroupInfo.LastRecordListIndex := AFirstRecordListIndex - 1;
  end;
end;

procedure TcxGridSortingBySummaryEngine.RebuildDataGroups;
var
  I: Integer;
  AFirstRecordListIndex: Integer;
begin
  AFirstRecordListIndex := 0;
  for I := 0 to FRootNode.ChildCount - 1 do
    RebuildDataGroupRecursive(FRootNode.Children[I], AFirstRecordListIndex);
  DataGroups.Rebuild;
end;

procedure TcxGridSortingBySummaryEngine.SortNodeRecursive(ANode: TcxDataGroupNode; ALevel: Integer);
var
  I: Integer;
begin
  if Infos[ALevel].SummaryItemIndex <> -1 then
    ANode.SortChildren(CompareGroupsBySummary);
  if (ALevel + 1) < DataGroups.LevelCount then // Last Level?
    for I := 0 to ANode.ChildCount - 1 do
      SortNodeRecursive(ANode.Children[I], ALevel + 1);
end;

procedure TcxGridSortingBySummaryEngine.SortNodes;
var
  ALevel, AItemGroupIndex: Integer;
  AInfo: TcxSortingBySummaryInfo;
  ASortedSummaryItem: TcxDataSummaryItem;
begin
  ClearInfos;
  for ALevel := 0 to DataController.Groups.LevelCount - 1 do
  begin
    AInfo := TcxSortingBySummaryInfo.Create;
    ASortedSummaryItem := DataController.Summary.GroupSummaryItems[ALevel].SortedSummaryItem;
    if ASortedSummaryItem <> nil then
    begin
      AInfo.SummaryItemIndex := ASortedSummaryItem.Index;
      if DataController.SortingBySummaryDataItemIndex <> -1 then
        AInfo.SortOrder := DataController.GetItemSortOrder(DataController.SortingBySummaryDataItemIndex)
      else
      begin
        AItemGroupIndex := DataController.Groups.GetItemGroupIndexByLevelGroupedItemIndex(ALevel, 0);
        AInfo.SortOrder := DataControllerInfo.GroupingFieldList[AItemGroupIndex].SortOrder;
      end;
    end
    else
      AInfo.SummaryItemIndex := -1;
    AddInfo(AInfo);
  end;

  SortNodeRecursive(FRootNode, 0);
end;

procedure TcxGridSortingBySummaryEngine.Sort;
var
  ACurIndex: Integer;
begin
  if DataGroups.Count = 0 then Exit;
  ACurIndex := 0;
  BuildNodes(FRootNode, 0, ACurIndex);
  SortNodes;
  RebuildDataGroups;
end;

{ TcxCustomGridCellPainter }

constructor TcxCustomGridCellPainter.Create(ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo);
begin
  inherited Create;
  FCanvas := ACanvas;
  FViewInfo := AViewInfo;
end;

function TcxCustomGridCellPainter.GetIsMainCanvasInUseValue: Boolean;
begin
  Result := GetIsMainCanvasInUse(FCanvas, FViewInfo);
end;

function TcxCustomGridCellPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := ViewInfo.ScaleFactor;
end;

procedure TcxCustomGridCellPainter.AfterPaint;
begin
  if CanDrawDesignSelection then DoDrawDesignSelection(Canvas, ViewInfo);
end;

procedure TcxCustomGridCellPainter.BeforePaint;
begin
end;

function TcxCustomGridCellPainter.CanDrawDesignSelection: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridCellPainter.DoExcludeFromClipRect;
begin
  Canvas.ExcludeClipRect(ViewInfo.Bounds);
end;

procedure TcxCustomGridCellPainter.DrawBackground;
begin
  DrawBackground(ViewInfo.ClientBounds);
end;

procedure TcxCustomGridCellPainter.DrawBackground(const R: TRect);
begin
  with Canvas, ViewInfo do
    if not Transparent then
    begin
      Brush.Color := Params.Color;
      FillRect(R);
    end
    else
      if BackgroundBitmap <> nil then
        FillRect(R, BackgroundBitmap);
end;

function TcxCustomGridCellPainter.DrawBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin  {4}
  Result := ViewInfo.BackgroundBitmap <> nil;
  if Result then
    ACanvas.FillRect(ABounds, ViewInfo.BackgroundBitmap);
end;

procedure TcxCustomGridCellPainter.DrawBorder(ABorder: TcxBorder);
begin
  with Canvas do
  begin
    SetBrushColor(ViewInfo.BorderColor[ABorder]);
    FillRect(ViewInfo.BorderBounds[ABorder]);
  end;
end;

procedure TcxCustomGridCellPainter.DrawBorders;
var
  ABorder: TcxBorder;
begin
  for ABorder := Low(ABorder) to High(ABorder) do
    if ABorder in ViewInfo.Borders then
      DrawBorder(ABorder);
end;

procedure TcxCustomGridCellPainter.DrawContent;
begin
  if ViewInfo.HasBackground and not ViewInfo.DoCustomDrawBackground(Canvas) then
    DrawBackground;
  DrawText;
end;

class procedure TcxCustomGridCellPainter.DrawDesignSelection(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo);
begin
  ACanvas.DrawDesignSelection(AViewInfo.DesignSelectionBounds, AViewInfo.DesignSelectionWidth);
end;

procedure TcxCustomGridCellPainter.DrawText;
var
  R: TRect;
begin
  with Canvas, ViewInfo do
    if TextForPainting <> '' then
    begin
      R := TextAreaBounds;  // can call font change
      PrepareCanvas(Canvas);
      Brush.Style := bsClear;
      PrepareCanvasForDrawText;
      DrawText(TextForPainting, R, GetTextAttributes(True), True, GetDrawTextRotationAngle);
      UnprepareCanvasForDrawText;
      Brush.Style := bsSolid;
    end;
end;

function TcxCustomGridCellPainter.ExcludeFromClipRect: Boolean;
begin
  Result := False;
end;

class function TcxCustomGridCellPainter.GetIsMainCanvasInUse(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo): Boolean;
begin
  Result := ACanvas = AViewInfo.Canvas;
end;

function TcxCustomGridCellPainter.NeedsPainting: Boolean;
begin
  Result := ViewInfo.Calculated and Canvas.RectVisible(ViewInfo.GetAreaBoundsForPainting);
end;

procedure TcxCustomGridCellPainter.Paint;
var
  ASavedParams: TcxViewParams;
begin
  ViewInfo.SaveParams(ASavedParams);
  try
    if not ViewInfo.DoCustomDraw(Canvas) then
      DrawContent;
  finally
    ViewInfo.RestoreParams(ASavedParams);
  end;
  DrawBorders;
end;

procedure TcxCustomGridCellPainter.PrepareCanvasForDrawText;
begin
  ViewInfo.PrepareCanvas(Canvas);
end;

procedure TcxCustomGridCellPainter.UnprepareCanvasForDrawText;
begin
end;

class procedure TcxCustomGridCellPainter.DoDrawDesignSelection(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo);
begin
  if AViewInfo.IsDesignSelected and GetIsMainCanvasInUse(ACanvas, AViewInfo) then
    DrawDesignSelection(ACanvas, AViewInfo);
end;

procedure TcxCustomGridCellPainter.MainPaint;
begin
  if not NeedsPainting then Exit;
  BeforePaint;
  Paint;
  AfterPaint;
  if ExcludeFromClipRect then DoExcludeFromClipRect;
end;

{ TcxGridDesignSelectorPainter }

function TcxGridDesignSelectorPainter.GetViewInfo: TcxGridDesignSelectorViewInfo;
begin
  Result := TcxGridDesignSelectorViewInfo(inherited ViewInfo);
end;

procedure TcxGridDesignSelectorPainter.DoExcludeFromClipRect;
begin
  Canvas.SetClipRegion(ViewInfo.Region, roSubtract, False);
end;

procedure TcxGridDesignSelectorPainter.DrawSign(AColor: TColor);
const
  SignOffsetStart = 3;
  SignOffsetEnd = 8;
  SignElementSize = 2;
  SignLineWidth = 1;
var
  R: TRect;
  I: Integer;
begin
  R := ViewInfo.Bounds;
  with R do
  begin
    Inc(Left, SignOffsetStart);
    Inc(Top, SignOffsetStart);
    Dec(Right, SignOffsetEnd);
    Dec(Bottom, SignOffsetEnd);
  end;
  Canvas.Brush.Color := AColor;

  Canvas.FillRect(Rect(R.Left, R.Top, R.Left + SignLineWidth, R.Bottom));
  Inc(R.Left, SignLineWidth);
  for I := 0 to (R.Bottom - R.Top) div SignElementSize - 1 do
  begin
    R.Bottom := R.Top + SignLineWidth;
    Canvas.FillRect(R);
    R.Top := R.Bottom + SignElementSize - SignLineWidth;
    Dec(R.Right, SignElementSize);
  end;
end;

function TcxGridDesignSelectorPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxGridDesignSelectorPainter.Paint;
var
  AParams: TcxViewParams;
begin
  ViewInfo.GetViewParams(AParams);
  Canvas.DrawRegion(ViewInfo.Region, AParams.Color, AParams.TextColor);
  DrawSign(AParams.TextColor);
end;

{ TcxCustomGridPainter }

function TcxCustomGridPainter.GetCanvas: TcxCanvas;
begin
  if FCanvas = nil then
    Result := ViewInfo.Canvas
  else
    Result := FCanvas;
end;

function TcxCustomGridPainter.GetViewInfo: TcxCustomGridViewInfo;
begin
  if FViewInfo = nil then
    Result := inherited ViewInfo
  else
    Result := FViewInfo;
end;

procedure TcxCustomGridPainter.DrawBackground;

  procedure DrawBackgroundUsingBrush;
  begin
    with Canvas do
    begin
      Brush.Color := ViewInfo.BackgroundColor;
      FillRgn(Handle, FBackgroundRegion.Handle, Brush.Handle);
    end;
  end;

  procedure DrawBackgroundUsingBitmap;
  var
    AClipRegion: TcxRegion;
  begin
    with Canvas do
    begin
      AClipRegion := GetClipRegion;
      SetClipRegion(FBackgroundRegion, roIntersect, False);
      FillRect(ViewInfo.Bounds, ViewInfo.BackgroundBitmap);
      SetClipRegion(AClipRegion, roSet);
    end;
  end;

begin
  if ViewInfo.BackgroundBitmap = nil then
    DrawBackgroundUsingBrush
  else
    DrawBackgroundUsingBitmap;  {4}
  //Canvas.ExcludeClipRect(ViewInfo.Bounds);
end;

procedure TcxCustomGridPainter.PaintAfter;
begin
  DrawBackground;
  FreeAndNil(FBackgroundRegion);
  Canvas.SetClipRegion(FBeforePaintClipRegion, roSet);
end;

procedure TcxCustomGridPainter.PaintBefore;
begin
  FBeforePaintClipRegion := Canvas.GetClipRegion;
  FBackgroundRegion := TcxRegion.Create(ViewInfo.Bounds);
  if ViewInfo.DesignSelectorViewInfo <> nil then
    ViewInfo.DesignSelectorViewInfo.Paint(Canvas);
end;

procedure TcxCustomGridPainter.PaintContent;
begin
end;

procedure TcxCustomGridPainter.DrawFocusRect(const R: TRect; AHideFocusRect: Boolean);
begin
  if GridView.IsControlFocused then
    Canvas.DrawFocusRect(R)
  else
    if not AHideFocusRect then
      with Canvas, R do
      begin
        InvertRect(Rect(Left, Top, Right, Top + 1));
        InvertRect(Rect(Left, Bottom - 1, Right, Bottom));
        InvertRect(Rect(Left, Top + 1, Left + 1, Bottom - 1));
        InvertRect(Rect(Right - 1, Top + 1, Right, Bottom - 1));
      end;
end;

procedure TcxCustomGridPainter.ExcludeFromBackground(const R: TRect);
begin
  FBackgroundRegion.Combine(R, roSubtract);
end;

procedure TcxCustomGridPainter.Paint(ACanvas: TcxCanvas = nil;
  AViewInfo: TcxCustomGridViewInfo = nil);
begin

  ViewInfo := AViewInfo;
  Canvas := ACanvas;

  Controller.BeforePaint;
  PaintBefore;
  PaintContent;
  PaintAfter;
  Controller.AfterPaint;

  Canvas := nil;
  ViewInfo := nil;
end;

procedure TcxCustomGridPainter.Invalidate;
begin
  Site.Invalidate;
end;

procedure TcxCustomGridPainter.Invalidate(const R: TRect);
begin
  Site.InvalidateRect(R, False);
end;

procedure TcxCustomGridPainter.Invalidate(ARegion: TcxRegion);
begin
  Site.InvalidateRgn(ARegion, False);
end;

{ TcxCustomGridViewData }

function TcxCustomGridViewData.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := TcxGridSortingBySummaryEngine;
end;

function TcxCustomGridViewData.IsEmpty: Boolean;
begin
  Result := DataController.RowCount = 0;
end;

function TcxCustomGridViewData.MakeDetailVisible(ADetailLevel: TComponent{TcxGridLevel}): TcxCustomGridView;
begin
  Result := nil;
end;

{ TcxGridSite }

constructor TcxGridSite.Create(AViewInfo: TcxCustomGridViewInfo);
begin
  FViewInfo := AViewInfo;
  inherited Create(nil);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  Keys := [kArrows, kChars];
  ParentColor := False;
  HScrollBar.UnlimitedTracking := True;
  VScrollBar.UnlimitedTracking := True;
end;

destructor TcxGridSite.Destroy;
begin
  cxClearObjectLinks(Self);
  CancelPostBoundsChanged;
  //!!!!FViewInfo.FSite := nil; should always be nil here already;
  inherited Destroy;
end;

function TcxGridSite.GetContainer: TcxControl;
begin
  Result := GridView.Control;
end;

function TcxGridSite.GetController: TcxCustomGridController;
begin
  Result := GridView.Controller;
end;

function TcxGridSite.GetGridView: TcxCustomGridView;
begin
  Result := FViewInfo.GridView;
end;

function TcxGridSite.GetPainter: TcxCustomGridPainter;
begin
  Result := GridView.Painter;
end;

procedure TcxGridSite.SendKeyDownNotification(var Message: TWMKeyDown);
begin
  TcxCustomGrid(GridView.Control).SendNotifications(gnkKeyDown,
    @Message);
end;

procedure TcxGridSite.BoundsChangedTimerHandler(Sender: TObject);
begin
  CancelPostBoundsChanged;
  GridView.BoundsChanged;
end;

function TcxGridSite.GetLocked: Boolean;
begin
  Result := GridView.IsControlLocked;
end;

function TcxGridSite.GetLockedStateImage: TcxBitmap32;
var
  AGrid: TcxCustomGridAccess;
begin
  AGrid := TcxCustomGridAccess(Container);
  Result := AGrid.LockedStatePaintHelper.GetImage;
end;

function TcxGridSite.GetLockedStateTopmostControl: TcxControl;
begin
  Result := Container;
end;

function TcxGridSite.CreateFieldControls(X, Y: Integer;
  ADataSource: TObject; AFieldList: TList): Boolean;
begin
  if GridView.IsPattern then
    Result := GridView.Controller.CreateFieldControls(X, Y, ADataSource, AFieldList)
  else
    Result := GridView.PatternGridView.Controller.CreateFieldControls(X, Y, ADataSource, AFieldList);
end;

function TcxGridSite.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := inherited AllowPan(AScrollKind) and ((Controller = nil) or Controller.AllowPan(AScrollKind));
end;

procedure TcxGridSite.CheckOverpan(AScrollKind: TScrollBarKind;
  ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer);
begin
  TcxControlAccess(Container).CheckOverpan(AScrollKind, ANewDataPos, AMinDataPos, AMaxDataPos, ADeltaX, ADeltaY);
end;

function TcxGridSite.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := False;
end;

function TcxGridSite.AllowAutoDragAndDropAtDesignTime(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxGridSite.AllowDragAndDropWithoutFocus: Boolean;
begin
  if TcxCustomGridAccess(GridView.Control).IsPopupControl then  {8}
    Result := True
  else
    Result := inherited AllowDragAndDropWithoutFocus;
end;

procedure TcxGridSite.BeforeMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if HandleAllocated and not Locked then
    Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TcxGridSite.BoundsChanged;

  function AllowNotification: Boolean;
  var
    AGridView: TcxCustomGridView;
  begin
    AGridView := GridView;
    repeat
      Result := AGridView.ViewInfo.AllowBoundsChangedNotification;
      AGridView := AGridView.MasterGridView;
    until not Result or (AGridView = nil);
  end;


var
  APrevSize: TSize;
begin
  if CreatingWindow then Exit;
  APrevSize := FSize;
  UpdateSize;
  if ViewInfo.VisibilityChanging then Exit;
  if ((APrevSize.cx <> 0) or (APrevSize.cy <> 0)) and
    ((APrevSize.cx <> FSize.cx) or (APrevSize.cy <> FSize.cy)) then
    if AllowNotification then
      GridView.BoundsChanged(True)
    else
      if UpdatingScrollBars then PostBoundsChanged;
  inherited;
end;

function TcxGridSite.CanDrag(X, Y: Integer): Boolean;
begin
  Result := inherited CanDrag(X, Y) and Controller.CanDrag(X, Y);
end;

function TcxGridSite.CanFocusOnClick(X, Y: Integer): Boolean;
begin
  Result := inherited CanFocusOnClick(X, Y) and Controller.CanFocusOnClick(X, Y) and
    (not IsScrollBarsArea(Point(X, Y)) or not Container.IsFocused);
end;

function TcxGridSite.CanProcessScrollEvents(var Message: TMessage): Boolean;
begin
  Result := not Locked or (ScrollBarsManager <> nil) and ScrollBarsManager.IsCapture and (Message.Msg = WM_CAPTURECHANGED);
end;

procedure TcxGridSite.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
var
  APrevBounds: TRect;
begin
  APrevBounds := BoundsRect;
  inherited;
  BoundsRect := APrevBounds;

  if (M <> D) and (GridView <> nil) then
  begin
    if GridView.ViewInfo.VisibilityChanging then
      GridView.ViewInfo.Recalculate;
  end;
end;

procedure TcxGridSite.CheckCancelDrag;
var
  ADragObject: TDragObject;
begin
  ADragObject := GetDragObject;
  if (ADragObject <> nil) and (Self = ADragObject.DragTarget) then
    ADragObject.DragTarget := nil;
end;

procedure TcxGridSite.DestroyHandle;
begin
  if GridView <> nil then
    GridView.DestroyingSiteHandle;
  inherited DestroyHandle;
end;

procedure TcxGridSite.DoCancelMode;
begin
  inherited;
  if not (Locked or GridView.IsDestroying) then
    Controller.DoCancelMode;
end;

procedure TcxGridSite.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if not Handled and not Locked then
    Handled := TcxCustomGrid(GridView.Control).SendNotifications(gnkContextMenu,
      TObject((MousePos.X = -1) and (MousePos.Y = -1)));
end;

procedure TcxGridSite.DoEnter;
begin
  inherited DoEnter;
  if not (Locked or GridView.IsDestroying) then
    Controller.DoEnter;
end;

procedure TcxGridSite.DoExit;
begin
  if not (Locked or GridView.IsDestroying) then
    Controller.DoExit;
  inherited;
end;

procedure TcxGridSite.SetFocusOnMouseClick(AButton: TMouseButton; X, Y: Integer);
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    inherited SetFocusOnMouseClick(AButton, X, Y);
    if ALink.Ref = nil then
      Exit;
    Controller.SetFocusOnMouseClick(AButton, X, Y);
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxGridSite.BeginGestureScroll(APos: TPoint);
begin
  inherited;
  Controller.BeginGestureScroll(APos);
end;

function TcxGridSite.CanScrollContentByGestureWithoutScrollBars: Boolean;
begin
  Result := cxIsTouchModeEnabled and not Locked;
end;

procedure TcxGridSite.DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions);
begin
  Touch.InteractiveGestures := [];
  Touch.InteractiveGestureOptions := [];
  Gestures := Touch.InteractiveGestures;
  Options := Touch.InteractiveGestureOptions;
end;

procedure TcxGridSite.DoGestureScroll(AScrollKind: TScrollBarKind; ANewScrollPos: Integer);
begin
  if GridView.DataController.IsGridMode and (AScrollKind = sbVertical) then
    Scroll(AScrollKind, scPosition, ANewScrollPos)
  else
    inherited;
end;

procedure TcxGridSite.EndGestureScroll;
begin
  inherited EndGestureScroll;
  Controller.EndGestureScroll;
end;

function TcxGridSite.IsDoubleBufferedNeeded: Boolean;
begin
  Result := ((ViewInfo <> nil) and ViewInfo.IsDoubleBufferedNeeded) or
    inherited IsDoubleBufferedNeeded;
end;

function TcxGridSite.IsGestureScrolling: Boolean;
begin
  Result := TcxControlAccess(Container).IsGestureScrolling;
end;

function TcxGridSite.IsGestureHelperMessage(var Message: TMessage): Boolean;
begin
  Result := False;
end;

function TcxGridSite.IsScrollBarBasedGestureScroll(AScrollKind: TScrollBarKind): Boolean;

  function IsDataScrollDirection(AScrollKind: TScrollBarKind): Boolean;
  begin
    Result := (AScrollKind = sbVertical) and (MouseWheelScrollingKind = mwskVertical) or
      (AScrollKind = sbHorizontal) and (MouseWheelScrollingKind = mwskHorizontal);
  end;

begin
  Result := not GridView.IsRecordPixelScrolling or not IsDataScrollDirection(AScrollKind);
end;

function TcxGridSite.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TcxGridSite.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := GridView.GetTouchScrollUIOwner(APoint);
  if Result = nil then
    Result := Self;
end;

procedure TcxGridSite.ResetRegion;
begin
  if FIsWindowRegionAssigned then
  begin
    SetWindowRgn(Handle, 0, True);
    FIsWindowRegionAssigned := False;
  end;
end;

procedure TcxGridSite.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
  GridView.ScrollContentByGesture(AScrollKind, ADelta);
end;

procedure TcxGridSite.SetRegion(const AVisibleRect: TRect);
begin
  SetWindowRegion(Handle, CreateRectRgnIndirect(AVisibleRect), roSet, False);
  FIsWindowRegionAssigned := True;
end;

procedure TcxGridSite.DoPaint;
begin
  if not Locked and not FViewInfo.IsCalculating then
  begin
    inherited DoPaint;
    Painter.Paint;
  end;
end;

procedure TcxGridSite.FocusChanged;
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    inherited;
    if IsFocused then
      GridView.Focused := True;
    if ALink.Ref = nil then Exit;
    TcxCustomGridAccess(Container).UpdateFocusing(GridView.IsControlFocused);
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

function TcxGridSite.FocusWhenChildIsClicked(AChild: TControl): Boolean;
begin
  Result := inherited FocusWhenChildIsClicked(AChild) and not Container.IsFocused;
end;

function TcxGridSite.GetClientBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, -BorderSize, -BorderSize);
  ViewInfo.AdjustClientBounds(Result);
end;

function TcxGridSite.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := Controller.GetCursor(X, Y);
  if Result = crDefault then
    Result := inherited GetCurrentCursor(X, Y);
end;

function TcxGridSite.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift);
  if Result or Locked then
    Exit;
  if not ViewInfo.CanHandleDesignHitTest(X, Y, Shift) then
    Exit;

  AHitTest := ViewInfo.GetHitTest(X, Y);
  if (ssRight in Shift) or MouseRightButtonReleased then
  begin
    Result := Assigned(AHitTest.ViewInfo) and AHitTest.ViewInfo.HasDesignPopupMenu;
    if Result and MouseRightButtonReleased then
      FDesignPopupMenuInvoker := AHitTest.ViewInfo;
    if Result then
      Exit;
  end;
  if not ((ssRight in Shift) or cxShiftStateMoveOnly(Shift) and MouseRightButtonReleased) then
    Result := Controller.GetDesignHitTest(AHitTest);
  MouseRightButtonReleased := False;
end;

function TcxGridSite.GetIsDesigning: Boolean;
begin
  if Container = nil then
    Result := GridView.IsDesigning
  else
    Result := Container.IsDesigning;
end;

function TcxGridSite.GetIsFocused: Boolean;
begin
  Result := (GridView.Control <> nil) and
    TcxCustomGridAccess(GridView.Control).IsPopupControl or {8}
    inherited GetIsFocused or Controller.HasFocusedControls;
end;

function TcxGridSite.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  if not IsPopupScrollBars then
    Result := TcxControlScrollBars
  else
    Result := inherited GetMainScrollBarsClass;
end;

function TcxGridSite.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := Controller.GetMouseWheelScrollingKind;
end;

function TcxGridSite.GetSystemSizeScrollBars: TcxScrollStyle;
begin
  Result := GridView.GetSystemSizeScrollBars;
end;

function TcxGridSite.GetVScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  ViewInfo.GetVScrollBarBounds(Result);
end;

procedure TcxGridSite.HideWindow;
var
  R: TRect;
begin
  R := cxRectSetLeft(BoundsRect, cxInvisibleCoordinate);
  MoveWindow(R);
end;

function TcxGridSite.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.ClientBounds,
    dxMapWindowPoint(Container.Handle, Handle, APoint));
end;

function TcxGridSite.IsPixelScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := Controller.IsPixelScrollBar(AKind);
end;

procedure TcxGridSite.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not Locked then
    Controller.DoKeyDown(Key, Shift);
end;

procedure TcxGridSite.KeyPress(var Key: Char);
begin
  inherited;
  if not Locked then
    Controller.KeyPress(Key);
end;

procedure TcxGridSite.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not Locked then
    Controller.KeyUp(Key, Shift);
end;

function TcxGridSite.MayFocus: Boolean;
begin
  Result := inherited MayFocus and ((Parent = nil) or Controller.MayFocus);
end;

procedure TcxGridSite.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SaveOrigin;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxGridSite.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if not Locked then
    Controller.MouseLeave;
end;

procedure TcxGridSite.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not Locked and (DragAndDropState = ddsNone) then
    Controller.MouseMove(Shift, X, Y);
end;

procedure TcxGridSite.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ALink: TcxObjectLink;
begin
  if Locked then
    inherited MouseUp(Button, Shift, X, Y)
  else
  begin
    if DragAndDropState = ddsNone then
    begin
      ALink := cxAddObjectLink(Self);
      try
        if (Button = mbRight) and Assigned(FDesignPopupMenuInvoker) then
        begin
          try
            Controller.ProcessDesignPopupMenu(FDesignPopupMenuInvoker);
          finally
            FDesignPopupMenuInvoker := nil;
          end;
        end;
        Controller.MouseUp(Button, Shift, X, Y);
        if ALink.Ref <> nil then
          inherited MouseUp(Button, Shift, X, Y);
      finally
        cxRemoveObjectLink(ALink);
      end;
    end
    else
    begin
      inherited MouseUp(Button, Shift, X, Y);
      Controller.MouseUp(Button, Shift, X, Y);
    end;
  end;
end;

procedure TcxGridSite.MoveWindow(const ANewBounds: TRect);
begin
  if cxRectIsEqual(BoundsRect, ANewBounds) then Exit;
  UpdateBoundsRect(ANewBounds);
  if HandleAllocated then
  begin
    with ANewBounds do
      Windows.MoveWindow(Handle, Left, Top, Right - Left, Bottom - Top,
        ANewBounds.Left <> cxInvisibleCoordinate);
  end;
end;

procedure TcxGridSite.Paint;
begin
  if Left = cxInvisibleCoordinate then Exit;
  inherited Paint;
end;

procedure TcxGridSite.RequestAlign;
begin
end;

procedure TcxGridSite.SetParent(AParent: TWinControl);
begin
  if Parent <> AParent then
  begin
    if AParent <> nil then
      LookAndFeel.MasterLookAndFeel := GridView.LookAndFeel;
    InitTabStop(AParent);
    if (AParent = nil) and GridView.Focused and
      not (csDestroying in Parent.ComponentState) and Parent.CanFocus then
      Parent.SetFocus;
  end;
  inherited;
end;

function TcxGridSite.UpdateMousePositionIfControlMoved: Boolean;
begin
  Result := False;
end;

function TcxGridSite.WasMovedOnMouseDown: Boolean;
begin
  Result := (FLeftOnMouseDown <> Left) or (FTopOnMouseDown <> Top);
end;

procedure TcxGridSite.WndProc(var Message: TMessage);
begin
  if not Locked then
    Controller.WndProc(Message);
  inherited WndProc(Message);
end;

procedure TcxGridSite.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  Controller.EndDrag(Target, X, Y);
  inherited;
end;

procedure TcxGridSite.DoStartDrag(var DragObject: TDragObject);
begin
  Controller.BeforeStartDrag;
  inherited;
  Controller.StartDrag(DragObject);
end;

procedure TcxGridSite.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;
  Controller.DragOver(Source, X, Y, State, Accept);
end;

procedure TcxGridSite.DrawDragImage(ACanvas: TcxCanvas; const R: TRect);
begin
  Controller.DrawDragImage(ACanvas, R);
end;

function TcxGridSite.GetDragImagesSize: TPoint;
begin
  Result := Controller.GetDragImagesSize;
end;

function TcxGridSite.HasDragImages: Boolean;
begin
  Result := Controller.HasDragImages;
end;

function TcxGridSite.GetHScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  ViewInfo.GetHScrollBarBounds(Result);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
end;

procedure TcxGridSite.InitScrollBarsParameters;
begin
  Controller.InitScrollBarsParameters;
end;

procedure TcxGridSite.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if not Locked then
    Controller.Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxGridSite.UpdateScrollBars;
begin
  if not CreatingWindow and not FViewInfo.IsCalculating and GridView.Visible and (IgnoreUpdateLock or not Locked) then
    inherited UpdateScrollBars;
end;

function TcxGridSite.CanCancelDragStartOnCaptureObjectClear: Boolean;
begin
  Result := (Controller = nil) or Controller.CanCancelDragStartOnCaptureObjectClear;
end;

procedure TcxGridSite.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  Controller.DragAndDrop(P, Accepted);
end;

procedure TcxGridSite.EndDragAndDrop(Accepted: Boolean);
begin
  Controller.EndDragAndDrop(Accepted);
  inherited EndDragAndDrop(Accepted);
end;

function TcxGridSite.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := not WasMovedOnMouseDown and not Locked and
    Controller.StartDragAndDrop(P);
end;

procedure TcxGridSite.CancelPostBoundsChanged;
begin
  FreeAndNil(FBoundsChangedTimer);
end;

procedure TcxGridSite.CheckClipping;
var
  AVisibleRect: TRect;
  ATopSiteOriginDelta, ABottomSiteOriginDelta: Integer;
begin
  ATopSiteOriginDelta := Top - GridView.MasterGridView.ViewInfo.GetContentBounds.Top;
  ABottomSiteOriginDelta := GridView.MasterGridView.ViewInfo.GetContentBounds.Bottom - BoundsRect.Bottom;
  if (ATopSiteOriginDelta < 0) or (ABottomSiteOriginDelta < 0) then
  begin
    AVisibleRect := cxRectSetNullOrigin(BoundsRect);
    if ATopSiteOriginDelta < 0 then
      Dec(AVisibleRect.Top, ATopSiteOriginDelta);
    if ABottomSiteOriginDelta < 0 then
      Inc(AVisibleRect.Bottom, ABottomSiteOriginDelta);
    SetRegion(AVisibleRect);
  end
  else
    ResetRegion;
end;

procedure TcxGridSite.InitTabStop(AParent: TWinControl);
begin
  if (AParent <> nil) and (GridView.Level <> nil) then
    GridView.TabStop := TcxGridLevel(GridView.Level).IsTop;
end;

function TcxGridSite.IsLockedStatePaint(out ALockedStateImage: TcxBitmap32): Boolean;
var
  AGrid: TcxCustomGridAccess;
begin
  AGrid := TcxCustomGridAccess(Container);
  ALockedStateImage := AGrid.GetLockedStateImage;
  Result := Assigned(ALockedStateImage);
end;

procedure TcxGridSite.PostBoundsChanged;
begin
  if FBoundsChangedTimer <> nil then Exit;
  FBoundsChangedTimer := TcxTimer.Create(nil);
  with FBoundsChangedTimer do
  begin
    Interval := 1;
    OnTimer := BoundsChangedTimerHandler;
  end;
end;

procedure TcxGridSite.UpdateSize;
begin
  FSize.cx := ClientBounds.Width;
  FSize.cy := ClientBounds.Height;
end;

procedure TcxGridSite.BeginDragAndDrop;
begin
  Controller.BeginDragAndDrop;
  inherited BeginDragAndDrop;
end;

procedure TcxGridSite.DragDrop(Source: TObject; X, Y: Integer);
begin
  Controller.DragDrop(Source, X, Y);
  inherited DragDrop(Source, X, Y);
end;

function TcxGridSite.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or GridView.Focused and GridView.DataController.ExecuteAction(Action);
end;

{$IFDEF DELPHIBERLIN}
procedure TcxGridSite.ScaleForPPI(NewPPI: Integer);
var
  APrevBounds: TRect;
begin
  APrevBounds := BoundsRect;
  inherited;
  BoundsRect := APrevBounds;
end;
{$ENDIF}

procedure TcxGridSite.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  APrevWidth, APrevHeight: Integer;
begin
  APrevWidth := Width;
  APrevHeight := Height;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if (Width <> APrevWidth) or (Height <> APrevHeight) then
    UpdateSize;
end;

procedure TcxGridSite.SetFocus;
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    if not GridView.Focused then
      TcxCustomGrid(Container).FocusedView.Deactivate;
    if ALink.Ref = nil then Exit;
    inherited SetFocus;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

function TcxGridSite.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    GridView.Focused and GridView.DataController.UpdateAction(Action);
end;

procedure TcxGridSite.SaveOrigin;
begin
  FLeftOnMouseDown := Left;
  FTopOnMouseDown := Top;
end;

procedure TcxGridSite.CMBiDiModeChanged(var Message: TMessage);
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
  GridView.BiDiModeChanged;
end;

procedure TcxGridSite.CNKeyDown(var Message: TWMKeyDown);
begin
  SendKeyDownNotification(Message);
  if Message.Result = 0 then
    inherited;
end;

procedure TcxGridSite.CNSysKeyDown(var Message: TWMKeyDown);
begin
  SendKeyDownNotification(Message);
  if Message.Result = 0 then
    inherited;
end;

procedure TcxGridSite.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  if (Controller <> nil) and Controller.WantSpecialKey(Message.CharCode) then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxGridSite.DoScrollUIModeChanged;
begin
  GridView.LayoutChanged;
  UpdateScrollBarBounds;
end;

function TcxGridSite.GetScrollContentForegroundColor: TColor;
begin
  Result := LookAndFeelPainter.GridLikeControlContentTextColor;
end;

function TcxGridSite.HasScrollBarArea: Boolean;
begin
  Result := GetScrollbarMode in [sbmClassic, sbmHybrid];
end;

procedure TcxGridSite.ScrollBarVisibilityChanged(AScrollBars: TScrollBarKinds);
begin
  if (GetScrollbarMode = sbmHybrid) then
    PostBoundsChanged
  else
    inherited ScrollBarVisibilityChanged(AScrollBars);
end;

procedure TcxGridSite.WMContextMenu(var Message: TWMContextMenu);
begin
  if Locked then
    Message.Result := 1
  else
    inherited;
end;

procedure TcxGridSite.WMGestureNotify(var Message: TWMGestureNotify);
begin
  Message.Result := DefWindowProc(Handle, Message.Msg, Message.Unused, LPARAM(Message.NotifyStruct));
end;

procedure TcxGridSite.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if (Controller <> nil) then
    Message.Result := Message.Result or Controller.GetDlgCode;
end;

procedure TcxGridSite.WMIMEComposition(var Message: TMessage);
begin
  if not Controller.IMEComposition(Message) then
    inherited;
end;

procedure TcxGridSite.WMIMEStartComposition(var Message: TMessage);
begin
  if not Controller.IMEStartComposition then
    inherited;
end;

{ TcxCustomGridCellViewInfo }

constructor TcxCustomGridCellViewInfo.Create;
begin
  inherited Create(nil);
  FVisible := True;
end;

destructor TcxCustomGridCellViewInfo.Destroy;
begin
  cxClearObjectLinks(Self);
  LinkedHitTest := nil;
  if DropDownWindowExists and (DropDownWindow.Owner = Self) then
    DropDownWindow.Owner := nil;
  MouseCapture := False;
  State := gcsNone;
  inherited Destroy;
end;

function TcxCustomGridCellViewInfo.GetBorderSize(AIndex: TcxBorder): Integer;
begin
  if AIndex in Borders then
    Result := BorderWidth[AIndex]
  else
    Result := 0;
end;

function TcxCustomGridCellViewInfo.GetButtonState: TcxButtonState;
begin
  Result := GridCellStateToButtonState(ActualState);
end;

function TcxCustomGridCellViewInfo.GetClientBounds: TRect;
begin
  if IsRectEmpty(FClientBounds) then
    FClientBounds := CalculateClientBounds;
  Result := FClientBounds;
end;

function TcxCustomGridCellViewInfo.GetContentBounds: TRect;
begin
  if IsRectEmpty(FContentBounds) then
    FContentBounds := CalculateContentBounds;
  Result := FContentBounds;
end;

function TcxCustomGridCellViewInfo.GetTextBoundsValue: TRect;
begin
  Result := GetTextBounds(True, True);
end;

function TcxCustomGridCellViewInfo.GetTextHeight: Integer;
begin
  Result := CalculateTextHeight(False);
end;

function TcxCustomGridCellViewInfo.GetTextHeightWithOffset: Integer;
begin
  Result := TextHeight;
  GetCellTextAreaSize(Result, ScaleFactor);
end;

function TcxCustomGridCellViewInfo.GetTextWidth: Integer;
begin
  Result := CalculateTextWidth;
end;

function TcxCustomGridCellViewInfo.GetTextWidthWithOffset: Integer;
begin
  Result := TextWidth;
  GetCellTextAreaSize(Result, ScaleFactor);
end;

procedure TcxCustomGridCellViewInfo.SetLinkedHitTest(Value: TcxCustomGridHitTest);
var
  APrevLinkedHitTest: TcxCustomGridHitTest;
begin
  if FLinkedHitTest <> Value then
  begin
    APrevLinkedHitTest := FLinkedHitTest;
    FLinkedHitTest := Value;
    if (APrevLinkedHitTest <> nil) and (APrevLinkedHitTest.ViewInfo = Self) then
      APrevLinkedHitTest.ViewInfo := nil;
    if FLinkedHitTest <> nil then
      FLinkedHitTest.ViewInfo := Self;
  end;
end;

procedure TcxCustomGridCellViewInfo.SetState(Value: TcxGridCellState);
var
  APrevState: TcxGridCellState;
begin
  if FState <> Value then
  begin
    BeforeStateChange;
    APrevState := FState;
    FState := Value;
    StateChanged(APrevState);
  end;
end;

procedure TcxCustomGridCellViewInfo.DoCancelMode;
begin
  State := gcsNone;
end;

procedure TcxCustomGridCellViewInfo.TrackingMouseLeave;
begin
  if CanProcessMouseLeave then
    MouseLeave;
end;

procedure TcxCustomGridCellViewInfo.MouseLeave;
begin
  if not MouseCapture and
    ((FState = gcsSelected) or not IsCheck and (FState = gcsPressed)) then
    State := gcsNone;
end;

function TcxCustomGridCellViewInfo.PtInCaller(const P: TPoint): Boolean;
begin
  Result := HasPoint(P);
end;

function TcxCustomGridCellViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactor(Control);
end;

function TcxCustomGridCellViewInfo.GetAdornerTargetElementControl: TWinControl;
begin
  Result := nil;
end;

function TcxCustomGridCellViewInfo.GetAdornerTargetElementBounds: TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomGridCellViewInfo.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomGridCellViewInfo.GetAdornerTargetElements(AList: TStrings);
begin
//do nothing
end;

procedure TcxCustomGridCellViewInfo.AfterCalculateBounds(var ABounds: TRect);
begin
  ResetContentBounds;
end;

procedure TcxCustomGridCellViewInfo.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  ACanvas.GetParams(Params);
end;

function TcxCustomGridCellViewInfo.AllowChangeStateOnMouseDown: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCellViewInfo.AllowRightToLeftConversionOnRecalculate: Boolean;
begin
  Result := UseRightToLeftAlignment;
end;

procedure TcxCustomGridCellViewInfo.BeforeCustomDraw(ACanvas: TcxCanvas);
begin
  ACanvas.SetParams(Params);
end;

procedure TcxCustomGridCellViewInfo.BeforeStateChange;
begin
end;

function TcxCustomGridCellViewInfo.CalculateClientBounds: TRect;
var
  ABorder: TcxBorder;
  ABorderSize: Integer;
begin
  Result := Bounds;
  for ABorder := Low(ABorder) to High(ABorder) do
  begin
    ABorderSize := BorderSize[ABorder];
    with Result do
      case ABorder of
        bLeft:
          Inc(Left, ABorderSize);
        bTop:
          Inc(Top, ABorderSize);
        bRight:
          Dec(Right, ABorderSize);
        bBottom:
          Dec(Bottom, ABorderSize);
      end;
  end;
end;

function TcxCustomGridCellViewInfo.CalculateContentBounds: TRect;
begin
  Result := ClientBounds;
end;

function TcxCustomGridCellViewInfo.CalculateTextWidth(AAngle: Integer = 0): Integer;
var
  R: TRect;
begin
  CalculateParams;
  R := Rect(0, 0, cxMaxRectSize, cxMaxRectSize);
  PrepareCanvas(Canvas);
  if AAngle <> 0 then
    Canvas.SetFontAngle(AAngle);
//  Result := Canvas.TextWidth(Text);
  Canvas.TextExtent(Text, R, GetTextAttributes(False{AForPainting}) and not (cxAlignRight or cxAlignCenter) or cxAlignLeft);
  if AAngle <> 0 then
    Canvas.SetFontAngle(0);
  Result := R.Right - R.Left;
end;

function TcxCustomGridCellViewInfo.CalculateTextHeight(AForPainting: Boolean; AAngle: Integer = 0): Integer;
var
  R: TRect;
begin
  CalculateParams;
  R := TextAreaBounds;
  PrepareCanvas(Canvas);
  if AAngle <> 0 then
    Canvas.SetFontAngle(AAngle);
  Canvas.TextExtent(Text, R, GetTextAttributes(AForPainting) and not (cxAlignBottom or cxAlignVCenter) or cxAlignTop);  {1}
  if AAngle <> 0 then
    Canvas.SetFontAngle(0);
  Result := R.Bottom - R.Top;
end;

procedure TcxCustomGridCellViewInfo.CalculateParams;
begin
  if FParamsCalculated or FCalculatingParams then Exit;
  FCalculatingParams := True;
  try
    DoCalculateParams;
  finally
    FCalculatingParams := False;
    FParamsCalculated := True;
  end;
end;

procedure TcxCustomGridCellViewInfo.CalculateParamsNeeded;
begin
  FParamsCalculated := False;
end;

function TcxCustomGridCellViewInfo.CalculateHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

function TcxCustomGridCellViewInfo.CalculateWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

function TcxCustomGridCellViewInfo.CanProcessMouseLeave: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCellViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridCellViewInfo.Click;
begin
end;

function TcxCustomGridCellViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.CustomDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridCellViewInfo.Destroying;
begin
  FIsDestroying := True;
end;

procedure TcxCustomGridCellViewInfo.DoCalculateParams;
begin
  AlignmentHorz := GetAlignmentHorz;
  AlignmentVert := GetAlignmentVert;
  GetViewParams(Params);
  Borders := GetBorders;
  Text := GetText;
  MultiLine := GetMultiLine;
  MultiLinePainting := GetMultiLinePainting;
  Width := CalculateWidth;
end;

procedure TcxCustomGridCellViewInfo.DoInvalidate;
begin
  if Control <> nil then
    Control.InvalidateRect(GetAreaBoundsForPainting, False);
end;

function TcxCustomGridCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := HasCustomDraw;
  if Result then
  begin
    BeforeCustomDraw(ACanvas);
    Result := CustomDraw(ACanvas);
    if not Result then
      AfterCustomDraw(ACanvas);
  end;
end;

function TcxCustomGridCellViewInfo.DoCustomDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := HasCustomDrawBackground;
  if Result then
  begin
    BeforeCustomDraw(ACanvas);
    Result := CustomDrawBackground(ACanvas);
    if not Result then
      AfterCustomDraw(ACanvas);
  end;
end;

function TcxCustomGridCellViewInfo.GetActualState: TcxGridCellState;
begin
  Result := FState;
end;

function TcxCustomGridCellViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := taLeftJustify;
end;

function TcxCustomGridCellViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaTop;
end;

function TcxCustomGridCellViewInfo.GetAreaBounds: TRect;
begin
  SetRectEmpty(Result);
end;

function TcxCustomGridCellViewInfo.GetBackgroundBitmap: TBitmap;
begin  {4}
  Result := Params.Bitmap;
  //Result := nil;
end;

function TcxCustomGridCellViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := clDefault;
end;

function TcxCustomGridCellViewInfo.GetBorderBounds(AIndex: TcxBorder): TRect;
begin
  Result := ClientBounds;
  with Result do
  begin
    case AIndex of
      bLeft:
        begin
          Right := Left;
          Dec(Left, BorderWidth[AIndex]);
        end;
      bTop:
        begin
          Bottom := Top;
          Dec(Top, BorderWidth[AIndex]);
        end;
      bRight:
        begin
          Left := Right;
          Inc(Right, BorderWidth[AIndex]);
        end;
      bBottom:
        begin
          Top := Bottom;
          Inc(Bottom, BorderWidth[AIndex]);
        end;
    end;
    if AIndex in [bLeft, bRight] then
    begin
      if bTop in Borders then
        Dec(Top, BorderWidth[bTop]);
      if bBottom in Borders then
        Inc(Bottom, BorderWidth[bBottom]);
    end;
  end;
end;

function TcxCustomGridCellViewInfo.GetBorders: TcxBorders;
begin
  Result := [];
end;

function TcxCustomGridCellViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := 0;
end;

function TcxCustomGridCellViewInfo.GetBounds: TRect;
var
  AArea: TRect;
begin
  Result := RealBounds;
  AArea := GetAreaBounds;
  if not IsRectEmpty(AArea) then
    IntersectRect(Result, Result, AArea);
end;

class function TcxCustomGridCellViewInfo.GetCellHeight(ATextHeight: Integer;
  ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ATextHeight;
  GetCellTextAreaSize(Result, AScaleFactor);
end;

function TcxCustomGridCellViewInfo.GetContentHeight: Integer;
begin
  with ContentBounds do
    Result := Bottom - Top;
end;

function TcxCustomGridCellViewInfo.GetContentWidth: Integer;
begin
  with ContentBounds do
    Result := Right - Left;
end;

function TcxCustomGridCellViewInfo.GetControl: TcxControl;
begin
  if (Canvas.Canvas is TControlCanvas) and
    (TControlCanvas(Canvas.Canvas).Control is TcxControl) then
      Result := TControlCanvas(Canvas.Canvas).Control as TcxControl
  else
    Result := nil;
end;

function TcxCustomGridCellViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := Bounds;
  InflateRect(Result, -1, -1);
end;

function TcxCustomGridCellViewInfo.GetDesignSelectionWidth: Integer;
begin
  Result := cxDesignSelectionWidth;
end;

function TcxCustomGridCellViewInfo.GetDrawTextRotationAngle: TcxRotationAngle;
begin
  Result := ra0;
end;

function TcxCustomGridCellViewInfo.GetHeight: Integer;
begin
  with Bounds do
    Result := Bottom - Top;
end;

function TcxCustomGridCellViewInfo.GetHotTrack: Boolean;
begin
  Result := IsCheck;
end;

function TcxCustomGridCellViewInfo.GetIsCheck: Boolean;
begin
  Result := DropDownWindow <> nil;
end;

function TcxCustomGridCellViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.GetIsVisibleForPainting: Boolean;
begin
  Result := Visible;
end;

function TcxCustomGridCellViewInfo.GetMouseCapture: Boolean;
begin
  if Control = nil then
    Result := False
  else
    Result := Control.MouseCaptureObject = Self;
end;

function TcxCustomGridCellViewInfo.GetMultiLine: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.GetMultiLinePainting: Boolean;
begin
  Result := MultiLine;
end;

function TcxCustomGridCellViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxCustomGridCellPainter;
end;

function TcxCustomGridCellViewInfo.GetRealBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridCellViewInfo.GetRealTextAreaBounds: TRect;
begin
  Result := GetTextAreaBounds;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ContentBounds);
end;

function TcxCustomGridCellViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.GetText: string;
begin
  Result := '';
end;

function TcxCustomGridCellViewInfo.GetTextAreaBounds: TRect;
begin
  Result := cxRectInflate(ContentBounds, -ScaleFactor.Apply(cxGridCellTextOffset));
end;

function TcxCustomGridCellViewInfo.GetTextAttributes(AForPainting: Boolean): Integer;
const
  AMultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
  AShowEndEllipsis: array[Boolean] of Integer = (0, cxShowEndEllipsis);
begin
  Result :=
    cxAlignmentsHorz[AlignmentHorz] or cxAlignmentsVert[AlignmentVert] or
    AMultiLines[AForPainting and MultiLinePainting or not AForPainting and MultiLine] or
    AShowEndEllipsis[ShowEndEllipsis];
  if Control.UseRightToLeftReading then
    Result := Result or cxRtlReading;
end;

function TcxCustomGridCellViewInfo.GetTextBounds(AHorizontal, AVertical: Boolean): TRect;
var
  ATextWidth, ATextHeight: Integer;
begin
  Result := TextAreaBounds;
  if AHorizontal then
  begin
    ATextWidth := TextWidth;
    with Result do
    begin
      if MultiLinePainting and (ATextWidth > Right - Left) then Exit;
      case AlignmentHorz of
        taLeftJustify:
          Right := Left + ATextWidth;
        taRightJustify:
          Left := Right - ATextWidth;
        taCenter:
          begin
            Left := (Left + Right - ATextWidth) div 2;
            Right := Left + ATextWidth;
          end;
      end;
    end;
  end;
  if AVertical then
  begin
    ATextHeight := TextHeight;
    with Result do
      case AlignmentVert of
        vaTop:
          Bottom := Top + ATextHeight;
        vaBottom:
          Top := Bottom - ATextHeight;
        vaCenter:
          begin
            Top := (Top + Bottom - ATextHeight) div 2;
            Bottom := Top + ATextHeight;
          end;
      end;
  end;
end;

function TcxCustomGridCellViewInfo.GetTextCellHeight(AGridViewInfo: TcxCustomGridViewInfo;
  ALookAndFeelPainter: TcxCustomLookAndFeelPainter): Integer;
begin
  CalculateParams;
  if MultiLine then
    Result := TextHeight
  else
    Result := AGridViewInfo.GetFontHeight(Params.Font);
  Result := GetCellHeight(Result, ALookAndFeelPainter, ScaleFactor);
end;

function TcxCustomGridCellViewInfo.GetTextForPainting: string;
begin
  Result := Text;
end;

function TcxCustomGridCellViewInfo.GetTransparent: Boolean;
begin                               {4}
  Result := BackgroundBitmap <> nil;
end;

procedure TcxCustomGridCellViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
end;

function TcxCustomGridCellViewInfo.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TcxCustomGridCellViewInfo.GetVisibleForHitTest: Boolean;
begin
  Result := Visible;
end;

function TcxCustomGridCellViewInfo.GetWidth: Integer;
begin
  with Bounds do
    Result := Right - Left;
end;

function TcxCustomGridCellViewInfo.HasBackground: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCellViewInfo.HasCustomDraw: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.HasCustomDrawBackground: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.HasDesignPopupMenu: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.HasHitTestPoint(const P: TPoint): Boolean;
begin
  Result := HasPoint(P);
end;

function TcxCustomGridCellViewInfo.HasMouse(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := AHitTest is GetHitTestClass;
end;

procedure TcxCustomGridCellViewInfo.HotTrackChanged;
begin
//do nothing
end;

procedure TcxCustomGridCellViewInfo.InitDropDownWindow(APopup: TdxUIElementPopupWindow);
begin
  APopup.OwnerBounds := GetDropDownWindowOwnerBounds;
  APopup.BiDiMode := Control.BiDiMode;
end;

procedure TcxCustomGridCellViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  AHitTest.ViewInfo := Self;
end;

function TcxCustomGridCellViewInfo.InvalidateOnStateChange: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCellViewInfo.IsHotTracked(ACheckChildren: Boolean = True): Boolean;
begin
  Result := ButtonState = cxbsHot;
end;

function TcxCustomGridCellViewInfo.IsHotTrackChanged(APrevState: TcxGridCellState): Boolean;
begin
  Result := not IsDestroying and (gcsNone in [State, APrevState]);
end;

procedure TcxCustomGridCellViewInfo.Offset(DX, DY: Integer);
begin
  OffsetRect(Bounds, DX, DY);
  if not IsRectEmpty(FClientBounds) then
    OffsetRect(FClientBounds, DX, DY);
  if not IsRectEmpty(FContentBounds) then
    OffsetRect(FContentBounds, DX, DY);
end;

procedure TcxCustomGridCellViewInfo.PopulateDesignPopupMenu(APopupMenu: TPopupMenu);
begin
end;

procedure TcxCustomGridCellViewInfo.PrepareCanvas(ACanvas: TcxCanvas);
begin
  ACanvas.Font := Params.Font;
  ACanvas.Font.Color := Params.TextColor;
end;

procedure TcxCustomGridCellViewInfo.RestoreParams(const AParams: TcxViewParams);
begin
  Params := AParams;
end;

procedure TcxCustomGridCellViewInfo.SaveParams(out AParams: TcxViewParams);
begin
  AParams := Params;
end;

procedure TcxCustomGridCellViewInfo.SetHeight(Value: Integer);
begin
  with Bounds do
    Bottom := Top + Value;
end;

procedure TcxCustomGridCellViewInfo.SetMouseCapture(Value: Boolean);
begin
  if (Control <> nil) and (MouseCapture <> Value) then
    if Value then
      Control.MouseCaptureObject := Self
    else
      Control.MouseCaptureObject := nil;
end;

procedure TcxCustomGridCellViewInfo.SetWidth(Value: Integer);
begin
  with Bounds do
    Right := Left + Value;
end;

procedure TcxCustomGridCellViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  if InvalidateOnStateChange then
    Invalidate;
  if IsHotTrackChanged(APrevState) then
    HotTrackChanged;
  case State of
    gcsNone:
      EndMouseTracking(Self);
    gcsSelected:
      if Control <> nil then
        BeginMouseTracking(Control, RealBounds, Self);
  end;
  if CaptureMouseOnPress then
    case State of
      gcsPressed:
        MouseCapture := True;
      {gcsNone:
        MouseCapture := False; - commented because of the Offset happening during drag & drop}
    end;
  if (State = gcsPressed) and (DropDownWindow <> nil) then
    DropDown;
end;

procedure TcxCustomGridCellViewInfo.DropDown;
begin
  if DropDownWindow.Visible then
    Exit;
  DropDownWindow.Owner := Self;
  DropDownWindow.BiDiMode := Control.BiDiMode;
  DropDownWindow.AlignHorz := GetDropDownWindowAlignHorz;
  DropDownWindow.Popup;
end;

procedure TcxCustomGridCellViewInfo.CloseUp;
begin
  State := gcsNone;
end;

function TcxCustomGridCellViewInfo.CloseDropDownWindowOnDestruction: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCellViewInfo.DropDownWindowExists: Boolean;
begin
  Result := False;
end;

function TcxCustomGridCellViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := nil;
end;

function TcxCustomGridCellViewInfo.GetDropDownWindowAlignHorz: TcxPopupAlignHorz;
var
  AAlign: TcxPopupAlignHorz;
begin
  AAlign := GetDropDownWindowDefaultAlignHorz;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertPopupAlignHorz(AAlign)
  else
    Result := AAlign;
end;

function TcxCustomGridCellViewInfo.GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz;
begin
  Result := pahRight;
end;

function TcxCustomGridCellViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridCellViewInfo.IsDropDownWindowOwner: Boolean;
begin
  Result := DropDownWindow.Owner = nil;
end;

procedure TcxCustomGridCellViewInfo.AfterConstruction;
begin
  inherited;
  if not CloseDropDownWindowOnDestruction and DropDownWindowExists and
    DropDownWindow.Visible and IsDropDownWindowOwner then
  begin
    DropDownWindow.Owner := Self;
    State := gcsPressed;
  end;
end;

procedure TcxCustomGridCellViewInfo.AfterRecalculation;
begin
end;

procedure TcxCustomGridCellViewInfo.BeforeDestruction;
begin
  Destroying;
  inherited;
end;

procedure TcxCustomGridCellViewInfo.BeforeRecalculation;
begin
  CalculateParamsNeeded;
  Bounds := RealBounds;
  //ResetContentBounds;
end;

procedure TcxCustomGridCellViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  IsRightToLeftConverted := False;
  CalculateParams;
  if AWidth = -1 then
    AWidth := CalculateWidth;
  if AHeight = -1 then
    AHeight := CalculateHeight;
  Bounds := cxRectBounds(ALeftBound, ATopBound, AWidth, AHeight);
  AfterCalculateBounds(Bounds);
  FCalculated := True;
end;

procedure TcxCustomGridCellViewInfo.Calculate(const ABounds: TRect);
begin
  with ABounds do
    Calculate(Left, Top, Max(0, Right - Left), Max(0, Bottom - Top));
end;

function TcxCustomGridCellViewInfo.GetAreaBoundsForPainting: TRect;
begin
  Result := Bounds;
end;

function TcxCustomGridCellViewInfo.GetBestFitWidth: Integer;
begin
  Result := BorderSize[bLeft] + TextWidthWithOffset + BorderSize[bRight];
end;

function TcxCustomGridCellViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if VisibleForHitTest and HasHitTestPoint(P) and (GetHitTestClass <> nil) then
  begin
    Result := GetHitTestClass.Instance(P);
    InitHitTest(Result);
  end
  else
    Result := nil;
end;

function TcxCustomGridCellViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetBounds, P);
end;

procedure TcxCustomGridCellViewInfo.Invalidate;
begin
  DoInvalidate;
end;

function TcxCustomGridCellViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := False;
  if (AButton = mbLeft) and HotTrack and AllowChangeStateOnMouseDown and HasMouse(AHitTest) then
  begin
    if IsCheck then
      case FState of
        gcsSelected:
          State := gcsPressed;
        gcsPressed:
          State := gcsSelected;
      end
    else
      State := gcsPressed;
    Result := True;
  end;
end;

function TcxCustomGridCellViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := False;
  if HotTrack then
    if IsCheck then
      if State <> gcsPressed then
      begin
        if HasMouse(AHitTest) then
          State := gcsSelected
        else
          State := gcsNone;
        Result := True;
      end
      else
    else
    begin
      if HasMouse(AHitTest) then
        if MouseCapture then
          State := gcsPressed
        else
          State := gcsSelected
      else
        if MouseCapture then
          State := gcsSelected
        else
          State := gcsNone;
      Result := True;
    end;
end;

function TcxCustomGridCellViewInfo.MouseUp(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := False;
  if (AButton = mbLeft) and not IsCheck then
  begin
    Result := State = gcsPressed;
    State := gcsNone;
    if Result then Click;
  end;
end;

procedure TcxCustomGridCellViewInfo.DoOffset(DX, DY: Integer);
begin
  if not Calculated then
    Exit;
  Offset(DX, DY);
  State := gcsNone;
  MouseLeave;
end;

procedure TcxCustomGridCellViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  AAlignmentHorz: TAlignment;
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
  FClientBounds := TdxRightToLeftLayoutConverter.ConvertRect(FClientBounds, ABounds);
  FContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(FContentBounds, ABounds);
  AAlignmentHorz := GetAlignmentHorz;
  ChangeBiDiModeAlignment(AAlignmentHorz);
  AlignmentHorz := AAlignmentHorz;
  Borders := TdxRightToLeftLayoutConverter.ConvertBorders(FBorders);
end;

procedure TcxCustomGridCellViewInfo.Paint(ACanvas: TcxCanvas = nil);
var
  APainter: TcxCustomGridCellPainter;
begin
  if IsVisibleForPainting and (GetPainterClass <> nil) then
  begin
    if ACanvas = nil then
      ACanvas := Canvas;

    APainter := GetPainterClass.Create(ACanvas, Self);
    try
      APainter.MainPaint;
    finally
      APainter.Free;
    end;
  end;
end;

procedure TcxCustomGridCellViewInfo.Recalculate;
begin
  BeforeRecalculation;
  Calculate(Bounds);
  if AllowRightToLeftConversionOnRecalculate then
    RightToLeftConversion(Bounds);
end;

procedure TcxCustomGridCellViewInfo.ResetContentBounds;
begin
  SetRectEmpty(FClientBounds);
  SetRectEmpty(FContentBounds);
end;

procedure TcxCustomGridCellViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

procedure TcxCustomGridCellViewInfo.Update;
begin
  Recalculate;
  Invalidate;
end;

function TcxCustomGridCellViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

{ TcxCustomGridViewCellViewInfo }

constructor TcxCustomGridViewCellViewInfo.Create(AGridViewInfo: TcxCustomGridViewInfo);
begin
  FGridViewInfo := AGridViewInfo;
  inherited Create;
end;

destructor TcxCustomGridViewCellViewInfo.Destroy;
begin
  if Controller.HintCellViewInfo = Self then
    Controller.CancelHint;
  inherited Destroy;
end;

function TcxCustomGridViewCellViewInfo.GetController: TcxCustomGridController;
begin
  Result := GridView.Controller;
end;

function TcxCustomGridViewCellViewInfo.GetGridView: TcxCustomGridView;
begin
  Result := FGridViewInfo.GridView;
end;

function TcxCustomGridViewCellViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FGridViewInfo.LookAndFeelPainter;
end;

function TcxCustomGridViewCellViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FGridViewInfo.Canvas;
end;

function TcxCustomGridViewCellViewInfo.GetAdornerTargetElementControl: TWinControl;
begin
  Result := GridView.Site;
end;

function TcxCustomGridViewCellViewInfo.GetAdornerTargetElementBounds: TRect;
begin
  Result := Bounds;
end;

procedure TcxCustomGridViewCellViewInfo.AfterCalculateBounds(var ABounds: TRect);
var
  AHitTest: TcxCustomGridHitTest;
begin
  inherited;
  if EmulateMouseMoveAfterCalculate and HotTrack and
    Control.HandleAllocated and GridViewInfo.Visible then
  begin
    AHitTest := GetHitTest(GridViewInfo.MousePos);
    if (AHitTest <> nil) and (AHitTest.ViewInfo = Self) then
      MouseMove(AHitTest, []);
  end;
end;

function TcxCustomGridViewCellViewInfo.AllowRightToLeftConversionOnRecalculate: Boolean;
begin
  Result := inherited AllowRightToLeftConversionOnRecalculate and not GridViewInfo.IsCalculating;
end;

procedure TcxCustomGridViewCellViewInfo.DoInvalidate;
begin
  GridView.ViewChanged(Bounds);
end;

function TcxCustomGridViewCellViewInfo.EmulateMouseMoveAfterCalculate: Boolean;
begin
  Result := False;
end;

function TcxCustomGridViewCellViewInfo.GetControl: TcxControl;
begin
  Result := GridView.Site;
end;

function TcxCustomGridViewCellViewInfo.HasMouse(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited HasMouse(AHitTest) and
    (TcxCustomGridViewHitTest(AHitTest).GridView = GridView);
end;

procedure TcxCustomGridViewCellViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  GridViewInfo.InitHitTest(AHitTest);
end;

procedure TcxCustomGridViewCellViewInfo.InitDropDownWindow(APopup: TdxUIElementPopupWindow);
begin
  APopup.OwnerParent := GridView.Site;
  APopup.Font := TcxControlAccess(GridView.Control).Font;
  APopup.LookAndFeel := GridView.LookAndFeel;
  APopup.BorderStyle := GridView.LookAndFeelPainter.PopupBorderStyle;
  inherited InitDropDownWindow(APopup);
end;

function TcxCustomGridViewCellViewInfo.CanShowHint: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridViewCellViewInfo.CheckHint(AHitTest: TcxCustomGridHitTest);
var
  AHintText: TCaption;
  AIsHintMultiLine: Boolean;
  ATextRect, R: TRect;
begin
  if not (Visible and CanShowHint) then
    Exit;
  if NeedShowHint(AHitTest.Pos, AHintText, AIsHintMultiLine, ATextRect) then
  begin
    R := GetAreaBoundsForHint;
    if not IsRectEmpty(R) then
    begin
      IntersectRect(R, ATextRect, R);
      if not IsRectEmpty(R) then ATextRect := R;
    end;
    Controller.ShowHint(GetBoundsForHint, ATextRect, AHintText, AIsHintMultiLine,
      Params.Font, Self);
  end
  else
    Controller.CancelHint;
end;

function TcxCustomGridViewCellViewInfo.GetAreaBoundsForHint: TRect;
begin
  Result := GetAreaBounds;
end;

function TcxCustomGridViewCellViewInfo.GetBoundsForHint: TRect;
var
  R: TRect;
begin
  Result := GetCellBoundsForHint;
  R := GetAreaBoundsForHint;
  if not IsRectEmpty(R) then
    IntersectRect(Result, Result, R);
end;

function TcxCustomGridViewCellViewInfo.GetCellBoundsForHint: TRect;
begin
  Result := TextAreaBounds;
end;

function TcxCustomGridViewCellViewInfo.GetHintText: string;
begin
  Result := Text;
end;

function TcxCustomGridViewCellViewInfo.GetHintTextRect(const AMousePos: TPoint): TRect;
begin
  Result := TextAreaBounds;
end;

function TcxCustomGridViewCellViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(GetBoundsForHint, P);
end;

procedure TcxCustomGridViewCellViewInfo.InitHint(const AMousePos: TPoint;
  out AHintText: TCaption; out AIsHintMultiLine: Boolean; out ATextRect: TRect);
begin
  AHintText := HintText;
  AIsHintMultiLine := IsHintMultiLine;
  ATextRect := GetHintTextRect(AMousePos);
end;

function TcxCustomGridViewCellViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TcxCustomGridViewCellViewInfo.IsHintForText: Boolean;
begin
  Result := True;
end;

function TcxCustomGridViewCellViewInfo.IsHintMultiLine: Boolean;
begin
  Result := MultiLinePainting;
end;

function TcxCustomGridViewCellViewInfo.NeedShowHint(const AMousePos: TPoint;
  out AHintText: TCaption; out AIsHintMultiLine: Boolean; out ATextRect: TRect): Boolean;
var
  ATextBounds, R: TRect;
begin
  Result := (HintText <> '') and HasHintPoint(AMousePos) and (State <> gcsPressed);
  if Result then
  begin
    if IsHintForText then
    begin
      with TextAreaBounds do
        Result := (CalculateTextHeight(True) > Bottom - Top) or
          not MultiLinePainting and (TextWidth > Right - Left);
      if not Result then
      begin
        ATextBounds := TextBounds;
        IntersectRect(R, GetBoundsForHint, ATextBounds);
        Result := not EqualRect(R, ATextBounds);
      end;
    end;
    if Result then
      InitHint(AMousePos, AHintText, AIsHintMultiLine, ATextRect);
  end;
end;

function TcxCustomGridViewCellViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridViewCellViewInfo.BeforeRecalculation;
begin
  GridViewInfo.UpdateMousePos;
  inherited;
end;

procedure TcxCustomGridViewCellViewInfo.Invalidate;
begin
  if IsDestroying then Exit;
  CalculateParams;
  inherited Invalidate;
end;

function TcxCustomGridViewCellViewInfo.MouseMove(AHitTest: TcxCustomGridHitTest;
  AShift: TShiftState): Boolean;
begin
  Result := inherited MouseMove(AHitTest, AShift);
  CheckHint(AHitTest);
end;

{ TcxGridDesignSelectorViewInfo }

destructor TcxGridDesignSelectorViewInfo.Destroy;
begin
  DestroyRegion;
  inherited Destroy;
end;

function TcxGridDesignSelectorViewInfo.CalculateHeight: Integer;
begin
  Result := DesignSelectorHeight;
end;

function TcxGridDesignSelectorViewInfo.CalculateWidth: Integer;
begin
  Result := DesignSelectorWidth;
end;

function TcxGridDesignSelectorViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridDesignSelectorHitTest;
end;

function TcxGridDesignSelectorViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridDesignSelectorViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridDesignSelectorPainter;
end;

procedure TcxGridDesignSelectorViewInfo.GetViewParams(var AParams: TcxViewParams);
const
  Colors: array[Boolean, Boolean] of TColor =
    ((cxGridDesignViewColor, cxGridDesignViewHotColor),
     (cxGridDesignSelectedColor, cxGridDesignSelectedHotColor));
  BorderColors: array[Boolean, Boolean] of TColor =
    ((cxGridDesignViewBorderColor, cxGridDesignViewBorderColor),
     (cxGridDesignSelectedBorderColor, cxGridDesignSelectedHotBorderColor));
var
  AObjectSelected: Boolean;
begin
  AObjectSelected := GridView.Controller.DesignController.IsObjectSelected(GridView);
  AParams.Color := dxGetNearestColor(Colors[AObjectSelected, State = gcsSelected]);
  AParams.TextColor := dxGetNearestColor(BorderColors[AObjectSelected, State = gcsSelected]);
end;

procedure TcxGridDesignSelectorViewInfo.CreateRegion;
var
  Points: array[1..3] of TPoint;
begin
  Points[1] := Point(0, 0);
  Points[2] := Point(Width, 0);
  Points[3] := Point(0, Height);
  FRegion := TcxRegion.Create(CreatePolygonRgn(Points, Length(Points), WINDING));
  FRegion.Offset(Bounds.Left, Bounds.Top);
end;

procedure TcxGridDesignSelectorViewInfo.DestroyRegion;
begin
  FreeAndNil(FRegion);
end;

procedure TcxGridDesignSelectorViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  DestroyRegion;
  CreateRegion;
end;

function TcxGridDesignSelectorViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := Region.PtInRegion(P);
end;

function TcxGridDesignSelectorViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  if AButton = mbLeft then
  begin
    GridView.Controller.DesignController.SelectObject(GridView, not (ssShift in AShift));
    Result := True;
  end
  else
    Result := inherited MouseDown(AHitTest, AButton, AShift);
end;

{ TcxCustomGridViewInfo }

constructor TcxCustomGridViewInfo.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  InitializeCriticalSection(FLock);
  FAllowCheckCoordinates := True;
  FAllowHideSite := True;
  CreateSite;
  CreateViewInfos;
end;

destructor TcxCustomGridViewInfo.Destroy;
begin
  DestroySite;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TcxCustomGridViewInfo.GetCalculated: Boolean;
begin
  Result := not IsRectEmpty(Bounds);
end;

function TcxCustomGridViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FSite.ActiveCanvas;
end;

function TcxCustomGridViewInfo.GetClientBounds: TRect;
begin
  if FClientBoundsAssigned then
    Result := FClientBounds
  else
    Result := CalculateClientBounds;
end;

function TcxCustomGridViewInfo.GetClientHeight: Integer;
begin
  with ClientBounds do
    Result := Bottom - Top;
end;

function TcxCustomGridViewInfo.GetClientWidth: Integer;
begin
  with ClientBounds do
    Result := Right - Left;
end;

{function TcxCustomGridViewInfo.GetIsCalculating: Boolean;
begin
  Result := FIsCalculating or
    GridView.IsDetail and GridView.MasterGridView.ViewInfo.IsCalculating;
end;}

function TcxCustomGridViewInfo.GetIsInternalUseValue: Boolean;
begin
  Result := GetIsInternalUse or
    GridView.IsDetail and
    (GridView.MasterGridView.ViewInfo <> nil) and
    GridView.MasterGridView.ViewInfo.IsInternalUse;
end;

procedure TcxCustomGridViewInfo.SetClientBounds(const Value: TRect);
begin
  FClientBounds := Value;
  FClientBoundsAssigned := True;
end;

procedure TcxCustomGridViewInfo.CreateViewInfos;
begin
  if GridView.IsDesigning then
    FDesignSelectorViewInfo := GetDesignSelectorViewInfoClass.Create(Self);
end;

procedure TcxCustomGridViewInfo.DestroyViewInfos(AIsRecreating: Boolean);
begin
  FreeAndNil(FDesignSelectorViewInfo);
end;

procedure TcxCustomGridViewInfo.RecreateViewInfos;
begin
  FClientBoundsAssigned := False;
  DestroyViewInfos(True);
  CreateViewInfos;
end;

function TcxCustomGridViewInfo.GetDesignSelectorViewInfoClass: TcxGridDesignSelectorViewInfoClass;
begin
  Result := TcxGridDesignSelectorViewInfo;
end;

function TcxCustomGridViewInfo.CanHideSite: Boolean;
var
  AGrid: TcxCustomGrid;
begin
  AGrid := TcxCustomGrid(Control);
  Result := (Control = nil) or GridView.IsDestroying or
    FAllowHideSite and not FSite.IsFocused and
      ((AGrid.FocusedView = nil) or not FSite.ContainsControl(AGrid.FocusedView.Site));
end;

procedure TcxCustomGridViewInfo.CreateSite;
begin
  FSite := GetSiteClass.Create(Self);
end;

procedure TcxCustomGridViewInfo.DestroySite;
begin
  FreeAndNil(FSite);
end;

function TcxCustomGridViewInfo.GetSiteClass: TcxGridSiteClass;
begin
  Result := TcxGridSite;
end;

function TcxCustomGridViewInfo.GetSiteParent: TWinControl;
begin
  if GridView.IsDetail then
    Result := GridView.MasterGridView.Site
  else
    Result := Control;
end;

procedure TcxCustomGridViewInfo.AddScrollBarHeight(var AHeight: Integer);
begin
  if not Site.IsPopupScrollBars and Site.IsScrollBarActive(sbHorizontal) then
    Inc(AHeight, Site.GetHScrollBarAreaHeight);
end;

procedure TcxCustomGridViewInfo.AdjustClientBounds(var ABounds: TRect);
begin
  if Site.UseRightToLeftScrollBar then
    Inc(ABounds.Left, GetRightNonClientWidth)
  else
    Dec(ABounds.Right, GetRightNonClientWidth);
  Dec(ABounds.Bottom, GetBottomNonClientHeight);
end;

function TcxCustomGridViewInfo.GetBottomNonClientHeight: Integer;
begin
  if not Site.IsPopupScrollBars and Site.IsScrollBarActive(sbHorizontal) then
    Result := Site.GetScrollBarSize.cy // Site.HScrollBar.Height
  else
    Result := 0;
end;

function TcxCustomGridViewInfo.GetRightNonClientWidth: Integer;
begin
  if not Site.IsPopupScrollBars and Site.IsScrollBarActive(sbVertical) then
    Result := Site.GetVScrollBarAreaWidth
  else
    Result := 0;
end;

procedure TcxCustomGridViewInfo.AfterCalculating;
begin
  if not IsInternalUse then Controller.UpdateScrollBars(True);
  if CanCheckCoordinates then Controller.DoCheckCoordinates;
end;

procedure TcxCustomGridViewInfo.BeforeCalculating;
begin
  UpdateMousePos;
end;

procedure TcxCustomGridViewInfo.Calculate;
begin
  if DesignSelectorViewInfo <> nil then
    with GetDesignSelectorPos do
      DesignSelectorViewInfo.Calculate(X, Y);
end;

function TcxCustomGridViewInfo.CalculateClientBounds: TRect;
begin
  Result := Bounds;
end;

procedure TcxCustomGridViewInfo.CalculateHeight(const AMaxSize: TPoint;
  var AHeight: Integer; var AFullyVisible: Boolean);
begin
  AddScrollBarHeight(AHeight);
end;

procedure TcxCustomGridViewInfo.CalculateWidth(const AMaxSize: TPoint; var AWidth: Integer);
begin
  if Site.IsScrollBarActive(sbVertical) then
    Inc(AWidth, Site.GetVScrollBarAreaWidth);
  if AWidth > AMaxSize.X then AWidth := AMaxSize.X;
end;

function TcxCustomGridViewInfo.CanCheckCoordinates: Boolean;
begin
  Result := not IsInternalUse and FAllowCheckCoordinates;
end;

function TcxCustomGridViewInfo.CanHandleDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := GridView.CanGetHitTest;
end;

procedure TcxCustomGridViewInfo.ControlFocusChanged;
begin
end;

function TcxCustomGridViewInfo.DoGetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := TcxGridViewNoneHitTest.Instance(P);
  InitHitTest(Result);
end;

function TcxCustomGridViewInfo.GetAllowBoundsChangedNotification: Boolean;
begin
  Result := not IsCalculating and not VisibilityChanging;
end;

function TcxCustomGridViewInfo.GetBackgroundBitmap: TBitmap;
begin       {4}
  Result := GridView.BackgroundBitmaps.GetBitmap(bbBackground);
end;

function TcxCustomGridViewInfo.GetBackgroundColor: TColor;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetViewParams(vsBackground, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxCustomGridViewInfo.GetContentBounds: TRect;
begin
  Result := ClientBounds;
end;

function TcxCustomGridViewInfo.GetDesignSelectorPos: TPoint;
begin
  Result := Bounds.TopLeft;
end;

procedure TcxCustomGridViewInfo.GetHScrollBarBounds(var ABounds: TRect);
begin
  case Site.GetScrollbarMode of
    sbmClassic:
      begin
        ABounds.Top := ABounds.Bottom;
        ABounds.Bottom := ABounds.Top + GetBottomNonClientHeight;
      end;
    sbmHybrid:
      begin
        ABounds.Top := ABounds.Bottom - Site.HScrollBar.Height;
        if Site.IsScrollBarActive(sbVertical) then
          ABounds.Right := ABounds.Right - Site.VScrollBar.Width;
      end
  else //tsmTouch
    ABounds.Top := ABounds.Bottom - Site.HScrollBar.Height;
    if Site.VScrollBarVisible then
      ABounds.Right := ABounds.Right - Site.VScrollBar.Width;
  end;
end;

function TcxCustomGridViewInfo.GetIsInternalUse: Boolean;
begin
  Result := FIsInternalUse;
end;

function TcxCustomGridViewInfo.GetVisible: Boolean;
begin
  Result := not IsInternalUse and not FSizeCalculating and
    not GridView.DataController.IsUpdatingItems;
end;

procedure TcxCustomGridViewInfo.GetVScrollBarBounds(var ABounds: TRect);
var
  AParentRect: TRect;
begin
  AParentRect := ABounds;
  case Site.GetScrollbarMode of
    sbmClassic:
      begin
        ABounds.Left := ABounds.Right;
        ABounds.Right := ABounds.Left + GetRightNonClientWidth;
      end
  else //tsmTouch, sbmHybrid
    ABounds.Left := ABounds.Right - Site.VScrollBar.Width;
    if Site.IsScrollBarActive(sbHorizontal) then
      ABounds.Bottom := ABounds.Bottom - Site.HScrollBar.Height;
  end;
  if Site.UseRightToLeftScrollBar then
    ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds, AParentRect);
end;

procedure TcxCustomGridViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  (AHitTest as TcxCustomGridViewHitTest).GridView := GridView;
end;

function TcxCustomGridViewInfo.IsDoubleBufferedNeeded: Boolean;
begin
  Result := False;
end;

function TcxCustomGridViewInfo.SiteCanBeClipped: Boolean;
begin
  Result := GridView.IsDetail and Site.HandleAllocated;
end;

procedure TcxCustomGridViewInfo.UpdateMousePos;
begin
  if FSite.Left = cxInvisibleCoordinate then Exit;
  if FSite.HandleAllocated and not FSite.IsDesigning then
    FMousePos := FSite.ScreenToClient(GetMouseCursorPos)
  else
    FMousePos := Point(MaxInt, MaxInt);
end;

procedure TcxCustomGridViewInfo.VisibilityChanged(AVisible: Boolean);
begin
  UpdateMousePos;
  if AVisible then
  begin
    if SiteCanBeClipped then
      FSite.CheckClipping;
    FSite.Parent := GetSiteParent;
  end
  else
  begin
    FSite.CheckCancelDrag;

    if CanHideSite then
      FSite.Parent := nil
    else
      FSite.HideWindow;
  end;
end;

function TcxCustomGridViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactor(Control);
end;

procedure TcxCustomGridViewInfo.BeforeDestruction;
begin
  inherited;
  DestroyViewInfos(False);
end;

procedure TcxCustomGridViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FClientBounds := TdxRightToLeftLayoutConverter.ConvertRect(FClientBounds, ABounds);
end;

procedure TcxCustomGridViewInfo.DoVisibilityChanged(AVisible: Boolean);
begin
  if Visible then
  begin
    FVisibilityChanging := True;
    try
      VisibilityChanged(AVisible and GridView.Visible);
    finally
      FVisibilityChanging := False;
    end;
  end;
end;

function TcxCustomGridViewInfo.GetFontHeight(AFont: TFont): Integer;
begin
  Canvas.Font := AFont;
  Result := cxTextHeight(Canvas.Handle);
end;

procedure TcxCustomGridViewInfo.GetFontMetrics(AFont: TFont; out AMetrics: TTextMetric);
begin
  Canvas.Font := AFont;
  GetTextMetrics(Canvas.Handle, AMetrics);
end;

procedure TcxCustomGridViewInfo.GetHeight(const AMaxSize: TPoint; var AHeight: Integer;
  var AFullyVisible: Boolean);
begin
  FSizeCalculating := True;
  CalculateHeight(AMaxSize, AHeight, AFullyVisible);
  FSizeCalculating := False;
end;

function TcxCustomGridViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := nil;
  if (Controller.CustomizationForm <> nil) and Site.HandleAllocated and
    PtInRect(cxGetWindowRect(Controller.CustomizationForm), Site.ClientToScreen(P)) then
  begin
    Result := TcxGridCustomizationFormHitTest.Instance(P);
    InitHitTest(Result);
  end
  else
    if DesignSelectorViewInfo <> nil then
      Result := DesignSelectorViewInfo.GetHitTest(P);
  if Result = nil then
    Result := DoGetHitTest(P);
end;

function TcxCustomGridViewInfo.GetHitTest(X, Y: Integer): TcxCustomGridHitTest;
begin
  Result := GetHitTest(Point(X, Y));
end;

procedure TcxCustomGridViewInfo.GetWidth(const AMaxSize: TPoint; var AWidth: Integer);
begin
  FSizeCalculating := True;
  CalculateWidth(AMaxSize, AWidth);
  FSizeCalculating := False;
end;

procedure TcxCustomGridViewInfo.MainCalculate(const ABounds: TRect);
begin
  if FIsCalculating {or GridView.IsPattern} then Exit;
  IsRightToLeftConverted := False;
  BeforeCalculating;
  FIsCalculating := True;
  try
    FSite.BiDiMode := Control.BiDiMode;
    FSite.BoundsRect := ABounds;
    FBounds := FSite.ClientBounds;
    Calculate;
  finally
    FIsCalculating := False;
    AfterCalculating;
    DoVisibilityChanged(True);
  end;
end;

procedure TcxCustomGridViewInfo.Recalculate;
begin
  MainCalculate(FSite.BoundsRect);
  if UseRightToLeftAlignment then
    RightToLeftConversion(FSite.BoundsRect);
end;

procedure TcxCustomGridViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    FSite.BoundsRect := ABounds;
    FBounds := FSite.ClientBounds;
    DoRightToLeftConversion(FBounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxCustomGridViewInfoCacheItem }

constructor TcxCustomGridViewInfoCacheItem.Create(AOwner: TcxCustomGridViewInfoCache;
  AIndex: Integer);
begin
  inherited Create;
  FIndex := AIndex;
  FOwner := AOwner;
end;

procedure TcxCustomGridViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
end;

{ TcxCustomGridViewInfoCache }

constructor TcxCustomGridViewInfoCache.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FItems := TdxFastList.Create;
end;

destructor TcxCustomGridViewInfoCache.Destroy;
begin
  DestroyItems;
  FItems.Free;
  inherited Destroy;
end;

function TcxCustomGridViewInfoCache.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxCustomGridViewInfoCache.GetInternalItem(Index: Integer): TcxCustomGridViewInfoCacheItem;
begin
  Result := TcxCustomGridViewInfoCacheItem(FItems[Index]);
end;

function TcxCustomGridViewInfoCache.GetItem(Index: Integer): TcxCustomGridViewInfoCacheItem;
begin
  Result := InternalItems[Index];
  if Result = nil then
  begin
    Result := GetItemClass(Index).Create(Self, Index);
    FItems[Index] := Result;
  end;
end;

procedure TcxCustomGridViewInfoCache.SetCount(Value: Integer);
begin
  DestroyItems;
  FItems.Count := Value;
end;

procedure TcxCustomGridViewInfoCache.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(FItems[I]).Free;
  FItems.Clear;
end;

procedure TcxCustomGridViewInfoCache.UnassignValues(AKeepMaster: Boolean = False);

  procedure ProcessItems;
  var
    I: Integer;
    AItem: TcxCustomGridViewInfoCacheItem;
  begin
    for I := 0 to Count - 1 do
    begin
      AItem := InternalItems[I];
      if AItem <> nil then AItem.UnassignValues(AKeepMaster);
    end;
  end;

  procedure ProcessMasterItems;
  var
    AGridView: TcxCustomGridView;
    AGridRecordIndex: Integer;
  begin
    AGridView := GridView;
    while AGridView.IsDetail do
    begin
      AGridRecordIndex := AGridView.MasterGridRecordIndex;
      AGridView := AGridView.MasterGridView;
      if (AGridRecordIndex <> -1) and (AGridView.ViewInfoCache <> nil) then
        AGridView.ViewInfoCache[AGridRecordIndex].UnassignValues(False);
    end;
  end;

begin
  if FUnassigningValues then Exit;
  FUnassigningValues := True;
  try
    ProcessItems;
    if not AKeepMaster then ProcessMasterItems;
  finally
    FUnassigningValues := False;
  end;
end;

{ TcxCustomGridOptions }

constructor TcxCustomGridOptions.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(nil);
  FGridView := AGridView;
  if GridView <> nil then
    GridView.AddOptions(Self);
end;

destructor TcxCustomGridOptions.Destroy;
begin
  if GridView <> nil then
    GridView.RemoveOptions(Self);
  inherited Destroy;
end;

function TcxCustomGridOptions.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GridView.LookAndFeelPainter;
end;

procedure TcxCustomGridOptions.Changed(AChangeKind: TcxGridViewChangeKind);
begin
  GridView.Changed(AChangeKind);
end;

procedure TcxCustomGridOptions.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

function TcxCustomGridOptions.GetGridViewValue: TcxCustomGridView;
begin
  Result := FGridView;
end;

function TcxCustomGridOptions.IsLoading: Boolean;
begin
  Result := GridView.IsLoading;
end;

procedure TcxCustomGridOptions.Notification(AComponent: TComponent; AOperation: TOperation);
begin
end;

procedure TcxCustomGridOptions.GetStoredProperties(AProperties: TStrings);
begin
end;

procedure TcxCustomGridOptions.GetStoredPropertyValue(const AName: string; var AValue: Variant);
begin
end;

procedure TcxCustomGridOptions.SetStoredPropertyValue(const AName: string; const AValue: Variant);
begin
end;

procedure TcxCustomGridOptions.Assign(Source: TPersistent);
begin
  if not (Source is TcxCustomGridOptions) then
    inherited;
end;

{ TcxCustomGridBackgroundBitmaps }

constructor TcxCustomGridBackgroundBitmaps.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FItems := TdxFastList.Create;
end;

destructor TcxCustomGridBackgroundBitmaps.Destroy;

  procedure ClearItems;
  var
    I: Integer;
  begin
    for I := 0 to Count - 1 do
      if FItems[I] <> nil then
        TBitmap(FItems[I]).Free;
  end;

begin
  ClearItems;
  FItems.Free;
  inherited Destroy;
end;

function TcxCustomGridBackgroundBitmaps.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TcxCustomGridBackgroundBitmaps.BitmapChanged(Sender: TObject);
begin
  Changed(vcLayout);
end;

function TcxCustomGridBackgroundBitmaps.GetBitmapStyleIndex(Index: Integer): Integer;
begin
  if Index = bbBackground then
    Result := vsBackground
  else
    Result := -1;
end;

function TcxCustomGridBackgroundBitmaps.GetDefaultBitmap(Index: Integer): TBitmap;
var
  AStyleIndex: Integer;
begin
  AStyleIndex := GetBitmapStyleIndex(Index);
  if AStyleIndex = -1 then
    Result := nil
  else
    Result := GridView.Styles.GetBitmap(AStyleIndex);
end;

function TcxCustomGridBackgroundBitmaps.GetValue(Index: Integer): TBitmap;
begin
  if Index >= Count then
    FItems.Count := Index + 1;
  if FItems[Index] = nil then
  begin
    FItems[Index] := TBitmap.Create;
    TBitmap(FItems[Index]).OnChange := BitmapChanged;
  end;
  Result := TBitmap(FItems[Index]);
end;

procedure TcxCustomGridBackgroundBitmaps.SetValue(Index: Integer; Value: TBitmap);
begin
  Values[Index].Assign(Value);
end;

procedure TcxCustomGridBackgroundBitmaps.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridBackgroundBitmaps then
    with TcxCustomGridBackgroundBitmaps(Source) do
      Self.Background := Background;
  inherited;
end;

function TcxCustomGridBackgroundBitmaps.GetBitmap(Index: Integer): TBitmap;
begin
  Result := Values[Index];
  if Result.Empty then
    Result := GetDefaultBitmap(Index);
end;

{ TcxCustomGridViewLockedStateImageOptions }

constructor TcxCustomGridShowLockedStateImageOptions.Create;
begin
  inherited Create;
end;

procedure TcxCustomGridShowLockedStateImageOptions.Assign(Source: TPersistent);
begin
end;

{ TcxCustomGridOptionsBehavior }

constructor TcxCustomGridOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited Create(AGridView);
  FPostponedSynchronization := True;
  FShowHourglassCursor := True;
  FSuppressHintOnMouseDown := True;
  FShowLockedStateImageOptions := GetShowLockedStateImageOptionsClass.Create;
end;

destructor TcxCustomGridOptionsBehavior.Destroy;
begin
  FreeAndNil(FShowLockedStateImageOptions);
  inherited Destroy;
end;

procedure TcxCustomGridOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridOptionsBehavior then
  begin
    HintHidePause := TcxCustomGridOptionsBehavior(Source).HintHidePause;
    PostponedSynchronization := TcxCustomGridOptionsBehavior(Source).PostponedSynchronization;
    ShowHourglassCursor := TcxCustomGridOptionsBehavior(Source).ShowHourglassCursor;
    ShowLockedStateImageOptions := TcxCustomGridOptionsBehavior(Source).ShowLockedStateImageOptions;
    SuppressHintOnMouseDown := TcxCustomGridOptionsBehavior(Source).SuppressHintOnMouseDown;
  end;
  inherited Assign(Source);
end;

function TcxCustomGridOptionsBehavior.GetShowLockedStateImageOptionsClass: TcxCustomGridShowLockedStateImageOptionsClass;
begin
  Result := TcxCustomGridShowLockedStateImageOptions;
end;

function TcxCustomGridOptionsBehavior.GetPostponedSynchronization: Boolean;
begin
  if GridView.PatternGridView = GridView then
    Result := FPostponedSynchronization
  else
    Result := GridView.PatternGridView.OptionsBehavior.PostponedSynchronization;
end;

procedure TcxCustomGridOptionsBehavior.SetShowLockedStateImageOptions(
  Value: TcxCustomGridShowLockedStateImageOptions);
begin
  FShowLockedStateImageOptions.Assign(Value);
end;

{ TcxCustomGridOptionsView }

function TcxCustomGridOptionsView.GetScrollBars: TcxScrollStyle;
begin
  Result := GridView.Site.ScrollBars;
end;

procedure TcxCustomGridOptionsView.SetScrollBars(Value: TcxScrollStyle);
begin
  if ScrollBars <> Value then
  begin
    GridView.Site.ScrollBars := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxCustomGridOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridOptionsView then
    with TcxCustomGridOptionsView(Source) do
      Self.ScrollBars := ScrollBars;
  inherited;
end;

{ TcxCustomGridStyles }

function TcxCustomGridStyles.GetIsSkinsAvailable: Boolean;
begin
  Result := GridView.LookAndFeel.SkinPainter <> nil;
end;

function TcxCustomGridStyles.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := GridView.LookAndFeelPainter;
end;

procedure TcxCustomGridStyles.Changed(AIndex: Integer);
begin
  inherited;
  if (GridView <> nil) and not GridView.IsDestroying then
    GridView.StylesChanged;
end;

procedure TcxCustomGridStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
var
  AGrid: TcxCustomGrid;
begin
  inherited;
  AGrid := TcxCustomGrid(GridView.Control);
  if AGrid = nil then
    AParams.Font := GridView.Painter.Canvas.Font
  else
    AParams.Font := AGrid.Font;
end;

procedure TcxCustomGridStyles.GetFakeComponentLinks(AList: TList);
var
  I: Integer;
  AOwner: TComponent;
begin
  for I := 0 to Count - 1 do
  begin
    AOwner := Items[I].Item.Owner;
    if (AOwner <> GridView.Owner) and (AList.IndexOf(AOwner) = -1) then
      AList.Add(AOwner);
  end;
end;

{ TcxCustomGridViewStyles }

procedure TcxCustomGridViewStyles.GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams);
begin
  inherited GetDefaultViewParams(Index, AData, AParams);
  case Index of
    vsBackground:
      begin
        AParams.Color := LookAndFeelPainter.GridLikeControlContentColor;
        AParams.TextColor := LookAndFeelPainter.GridLikeControlContentTextColor;
        if (AParams.TextColor = clWindowText) and (AParams.Color = clWindow) then
          AParams.TextColor := clGrayText;
      end;
  end;
end;

function TcxCustomGridViewStyles.GetGridView: TcxCustomGridView;
begin
  if GetOwner is TcxCustomGridView then
    Result := TcxCustomGridView(GetOwner)
  else
    Result := nil;
end;

procedure TcxCustomGridViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxCustomGridViewStyles then
    with TcxCustomGridViewStyles(Source) do
      Self.Background := Background;
end;

{ TcxGridViewNotificationList }

constructor TcxGridViewNotificationList.Create(AGridView: TcxCustomGridView);
begin
  inherited Create;
  FGridView := AGridView;
end;

procedure TcxGridViewNotificationList.Add(AInfo: TcxUpdateControlInfo);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].ClassType = AInfo.ClassType then
    begin
      Delete(I);
      Break;
    end;
  inherited Add(AInfo);
end;

function TcxGridViewNotificationList.GetItem(
  Index: Integer): TcxUpdateControlInfo;
begin
  Result := TcxUpdateControlInfo(inherited Items[Index]);
end;

{ TcxCustomGridView }

constructor TcxCustomGridView.CreateCloned(AControl: TcxControl);
begin
  CreateEx(AControl);
  FComponentStyle := FComponentStyle + [csTransient];
end;

destructor TcxCustomGridView.Destroy;
begin
  Controller.Customization := False;
  FreeAndNil(FNotifications);
  Focused := False;
  if not IsPattern and (PatternGridView <> nil) then
    PatternGridView.RemoveClone(Self);
  if (FLevel <> nil) and (TcxGridLevel(FLevel).GridView = Self) then
    TcxGridLevel(FLevel).GridView := nil;
  if FRepository <> nil then
    TcxGridViewRepositoryAccess(FRepository).RemoveItem(Self);  {5}
  DestroyHandlers;
  DestroyOptions;
  FClones.Free;
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

function TcxCustomGridView.GetClone(Index: Integer): TcxCustomGridView;
begin
  Result := TcxCustomGridView(FClones[Index]);
end;

function TcxCustomGridView.GetCloneCount: Integer;
begin
  Result := FClones.Count;
end;

function TcxCustomGridView.GetDragMode: TDragMode;
begin
  Result := Site.DragMode;
end;

function TcxCustomGridView.GetFocused: Boolean;
begin
  Result := (Control <> nil){5} and (TcxCustomGrid(Control).FocusedView = Self);
end;

function TcxCustomGridView.GetIsControlFocusedValue: Boolean;
var
  AForm: TCustomForm;
begin
  Result := not IsDesigning and (Control <> nil);
  if Result then
  begin
    AForm := GetParentForm(Control);
    if (AForm <> nil) and (TCustomFormAccess(AForm).FormStyle = fsMDIForm) then
      Result := Control.IsFocused
    else
      Result := GetIsControlFocused;
  end;
end;

function TcxCustomGridView.GetIsDetail: Boolean;
begin
  Result := (DataController <> nil) and DataController.IsDetailMode;
end;

function TcxCustomGridView.GetIsExportMode: Boolean;
begin
  Result := TcxCustomGridAccess(Control).IsExportMode;
end;

function TcxCustomGridView.GetIsMaster: Boolean;
begin
  Result := (FLevel <> nil) and TcxGridLevel(FLevel).IsMaster;
end;

function TcxCustomGridView.GetIsPattern: Boolean;
begin
  Result := {(Level = nil) or }DataController.IsPattern;
end;

function TcxCustomGridView.GetIsStoringNameMode: Boolean;
begin
  Result := FStoringName <> '';
end;

function TcxCustomGridView.GetIsUpdating: Boolean;
begin
  Result := csUpdating in ComponentState;
end;

function TcxCustomGridView.GetLookAndFeel: TcxLookAndFeel;
begin
  if Control = nil then
    Result := nil
  else
    Result := TcxCustomGrid(Control).LookAndFeel;
end;

function TcxCustomGridView.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  if Control = nil then
    Result := cxLookAndFeelPaintersManager.GetPainter(lfsStandard)
  else
    Result := TcxCustomGrid(Control).LookAndFeelPainter;
end;

function TcxCustomGridView.GetMasterGridView: TcxCustomGridView;
begin
  if IsDetail then
    Result := (DataController.GetMasterDataController as IcxCustomGridDataController).GridView
  else
    Result := nil;
end;

function TcxCustomGridView.GetMasterGridRecordIndex: Integer;
begin
  if MasterRecordIndex <> -1 then
    Result := MasterGridView.DataController.GetRowIndexByRecordIndex(MasterRecordIndex, False)
  else
    Result := -1;
end;

function TcxCustomGridView.GetMasterRecordIndex: Integer;
begin
  Result := DataController.GetMasterRecordIndex;
end;

function TcxCustomGridView.GetOnDblClick: TNotifyEvent;
begin
  Result := Site.OnDblClick;
end;

function TcxCustomGridView.GetOnDragDrop: TDragDropEvent;
begin
  Result := Site.OnDragDrop;
end;

function TcxCustomGridView.GetOnDragOver: TDragOverEvent;
begin
  Result := Site.OnDragOver;
end;

function TcxCustomGridView.GetOnEndDrag: TEndDragEvent;
begin
  Result := Site.OnEndDrag;
end;

function TcxCustomGridView.GetOnKeyDown: TKeyEvent;
begin
  Result := Site.OnKeyDown;
end;

function TcxCustomGridView.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Site.OnKeyPress;
end;

function TcxCustomGridView.GetOnKeyUp: TKeyEvent;
begin
  Result := Site.OnKeyUp;
end;

function TcxCustomGridView.GetOnMouseDown: TMouseEvent;
begin
  Result := Site.OnMouseDown;
end;

function TcxCustomGridView.GetOnMouseEnter: TNotifyEvent;
begin
  Result := Site.OnMouseEnter;
end;

function TcxCustomGridView.GetOnMouseLeave: TNotifyEvent;
begin
  Result := Site.OnMouseLeave;
end;

function TcxCustomGridView.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := Site.OnMouseMove;
end;

function TcxCustomGridView.GetOnMouseUp: TMouseEvent;
begin
  Result := Site.OnMouseUp;
end;

function TcxCustomGridView.GetOnMouseWheel: TMouseWheelEvent;
begin
  Result := Site.OnMouseWheel;
end;

function TcxCustomGridView.GetOnMouseWheelDown: TMouseWheelUpDownEvent;
begin
  Result := Site.OnMouseWheelDown;
end;

function TcxCustomGridView.GetOnMouseWheelUp: TMouseWheelUpDownEvent;
begin
  Result := Site.OnMouseWheelUp;
end;

function TcxCustomGridView.GetOnStartDrag: TStartDragEvent;
begin
  Result := Site.OnStartDrag;
end;

function TcxCustomGridView.GetPatternGridView: TcxCustomGridView;
begin
  Result := FPatternGridView;
  if Result = nil then Result := Self;
end;

function TcxCustomGridView.GetPopupMenu: TComponent;
begin
  Result := Site.PopupMenu;
end;

function TcxCustomGridView.GetSite: TcxGridSite;
begin
  if FViewInfo = nil then
    Result := nil
  else
    Result := FViewInfo.Site;
end;

function TcxCustomGridView.GetSynchronization: Boolean;
begin
  if PatternGridView = Self then
    Result := FSynchronization
  else
    Result := PatternGridView.Synchronization;
end;

procedure TcxCustomGridView.SetBackgroundBitmaps(Value: TcxCustomGridBackgroundBitmaps);
begin
  FBackgroundBitmaps.Assign(Value);
end;

procedure TcxCustomGridView.SetDragMode(Value: TDragMode);
begin
  if DragMode <> Value then
  begin
    Site.DragMode := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetFocused(Value: Boolean);
begin
  if Focused <> Value then
    with TcxCustomGrid(Control) do
      if Value then
        FocusedView := Self
      else
        FocusedView := nil;
end;

procedure TcxCustomGridView.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TcxCustomGridView.SetIsRestoring(Value: Boolean);
begin
  if FIsRestoring <> Value then
  begin
    FIsRestoring := Value;
    if FIsRestoring then
      BeforeRestoring
    else
      AfterRestoring;
  end;
end;

procedure TcxCustomGridView.SetSynchronization(Value: Boolean);
begin
  if not IsPattern then Exit;
  if FSynchronization <> Value then
  begin
    FSynchronization := Value;
    if Value then
      Synchronize
    else
      FSynchronizationNeeded := False;
  end;
end;

procedure TcxCustomGridView.SetOnCustomization(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnCustomization, Value) then
  begin
    FOnCustomization := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnDblClick(Value: TNotifyEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnDblClick);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnDragDrop(Value: TDragDropEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnDragDrop);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnDragDrop := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnDragOver(Value: TDragOverEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnDragOver);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnDragOver := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnEndDrag(Value: TEndDragEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnEndDrag);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnEndDrag := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnGetStoredProperties(Value: TcxGridViewGetStoredPropertiesEvent);
begin
  if not dxSameMethods(FOnGetStoredProperties, Value) then
  begin
    FOnGetStoredProperties := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnGetStoredPropertyValue(Value: TcxGridViewGetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnGetStoredPropertyValue, Value) then
  begin
    FOnGetStoredPropertyValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnInitStoredObject(Value: TcxGridInitStoredObjectEvent);
begin
  if not dxSameMethods(FOnInitStoredObject, Value) then
  begin
    FOnInitStoredObject := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnKeyDown(Value: TKeyEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnKeyDown);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnKeyDown := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnKeyPress(Value: TKeyPressEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnKeyPress);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnKeyPress := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnKeyUp(Value: TKeyEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnKeyUp);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnKeyUp := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseDown(Value: TMouseEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseDown);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseDown := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseEnter(Value: TNotifyEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseEnter);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseEnter := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseLeave(Value: TNotifyEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseLeave);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseLeave := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseMove(Value: TMouseMoveEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseMove);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseMove := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseUp(Value: TMouseEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseUp);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseUp := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseWheel(Value: TMouseWheelEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseWheel);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseWheel := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseWheelDown(Value: TMouseWheelUpDownEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseWheelDown);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseWheelDown := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnMouseWheelUp(Value: TMouseWheelUpDownEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnMouseWheelUp);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnMouseWheelUp := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnSetStoredPropertyValue(Value: TcxGridViewSetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnSetStoredPropertyValue, Value) then
  begin
    FOnSetStoredPropertyValue := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOnStartDrag(Value: TStartDragEvent);
var
  AMethod: TMethod;
begin
  AMethod := TMethod(OnStartDrag);
  if not dxSameMethods(AMethod, Value) then
  begin
    Site.OnStartDrag := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxCustomGridView.SetOptionsBehavior(Value: TcxCustomGridOptionsBehavior);
begin
  FOptionsBehavior.Assign(Value);
end;

procedure TcxCustomGridView.SetOptionsData(Value: TcxCustomGridOptionsData);
begin
  FOptionsData.Assign(Value);
end;

procedure TcxCustomGridView.SetOptionsSelection(Value: TcxCustomGridOptionsSelection);
begin
  FOptionsSelection.Assign(Value);
end;

procedure TcxCustomGridView.SetOptionsView(Value: TcxCustomGridOptionsView);
begin
  FOptionsView.Assign(Value);
end;

procedure TcxCustomGridView.SetPopupMenu(Value: TComponent);
begin
  if PopupMenu <> Value then
  begin
    Site.PopupMenu := Value;
    Changed(vcProperty)
  end;
end;

procedure TcxCustomGridView.SetStyles(Value: TcxCustomGridStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxCustomGridView.ClearNotifications;
begin
  if Assigned(FNotifications) then
    FNotifications.Clear;
end;

function TcxCustomGridView.GetObjectName: string;
begin
  if IsStoringNameMode then
    Result := FStoringName
  else
    Result := Name;
end;

function TcxCustomGridView.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Version');
  if Assigned(FOnGetStoredProperties) then
    FOnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TcxCustomGridView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Version' then
    AValue := StoringVersion;
  if Assigned(FOnGetStoredPropertyValue) then
    FOnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxCustomGridView.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Version' then
    FStoredVersion := AValue;
  if Assigned(FOnSetStoredPropertyValue) then
    FOnSetStoredPropertyValue(Self, AName, AValue);
end;

function TcxCustomGridView.CreateChild(const AObjectName, AClassName: string): TObject;
begin
  Result := CreateStoredObject(AObjectName, AClassName);
  InitChildComponent(Result, AObjectName);
end;

function TcxCustomGridView.CreateStoredObject(const AObjectName, AClassName: string): TObject;
begin
  Result := nil;
end;

procedure TcxCustomGridView.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

procedure TcxCustomGridView.GetStoredChildren(AChildren: TStringList);
begin
end;

procedure TcxCustomGridView.InitChildComponent(AObject: TObject;
  const AObjectName: string);
begin
  if AObject = nil then Exit;
  if not IsStoringNameMode and (AObject is TComponent) then
    TComponent(AObject).Name := AObjectName;
  DoInitStoredObject(AObject);
end;

procedure TcxCustomGridView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
end;

procedure TcxCustomGridView.BeforeEditLayout(ALayoutView: TcxCustomGridView);
begin
end;

function TcxCustomGridView.CanEditViewLayoutAndData: Boolean;
begin
  Result := DataController.CustomDataSource = nil;
end;

procedure TcxCustomGridView.DoAssignLayout(ALayoutView: TcxCustomGridView);
begin
  BeginUpdate;
  try
    AssignLayout(ALayoutView);
  finally
    EndUpdate;
  end;
end;

function TcxCustomGridView.GetLayoutCustomizationFormButtonCaption: string;
begin
  Result := '';
end;

function TcxCustomGridView.HasLayoutCustomizationForm: Boolean;
begin
  Result := False;
end;

function TcxCustomGridView.IsLayoutChangeable: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridView.RunLayoutCustomizationForm;
begin
  Controller.Customization := True;
end;

function TcxCustomGridView.ShowGridViewEditor: Boolean;
begin
  Result := False;
end;

function TcxCustomGridView.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Site;
end;

function TcxCustomGridView.GetAdornerTargetElementBounds: TRect;
begin
  Result := ViewInfo.Bounds;
end;

function TcxCustomGridView.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomGridView.ChangeScale(M, D: Integer);
begin
  OptionsBehavior.ChangeScale(M, D);
  OptionsData.ChangeScale(M, D);
  OptionsSelection.ChangeScale(M, D);
  OptionsView.ChangeScale(M, D);
end;

procedure TcxCustomGridView.GetFakeComponentLinks(AList: TList);
begin
  inherited;
  (FDataController as IcxCustomGridDataController).GetFakeComponentLinks(AList);
  FStyles.GetFakeComponentLinks(AList);
end;

function TcxCustomGridView.GetIsDestroying: Boolean;
begin
  Result := inherited GetIsDestroying or
    IsDetail and MasterGridView.IsDestroying;
end;

function TcxCustomGridView.GetSystemSizeScrollBars: TcxScrollStyle;
begin
  Result := ssBoth;
end;

procedure TcxCustomGridView.Loaded;
begin
  inherited;
  LoadingComplete;
  DataController.Loaded;
  Init;
end;

procedure TcxCustomGridView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) and not IsDestroying then
    Images := nil;
  NotifyOptions(AComponent, Operation);
end;

procedure TcxCustomGridView.ScaleFactorChanged(M, D: Integer);
begin
  BeginUpdate;
  try
    inherited ScaleFactorChanged(M, D);
    ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomGridView.SetControl(Value: TcxControl);
begin
  if Control <> Value then
  begin
    if Control <> nil then
    begin
      TcxCustomGrid(Control).RemoveFontListener(FStyles);  {5}
      if Control.IsDesigning and (Site <> nil) then
        Control.RemoveComponent(Site);
    end;
    inherited;
    if Control <> nil then
    begin
      if (FStorageControl = nil) and (FRepository = nil) then
        FStorageControl := Value;
      if Control.IsDesigning then
        Control.InsertComponent(Site);
      TcxCustomGrid(Control).AddFontListener(FStyles);
    end;
  end;
end;

procedure TcxCustomGridView.SetName(const NewName: TComponentName);
begin
  inherited;
  NotifyControl(vcnName);
end;

procedure TcxCustomGridView.Updated;
begin
  inherited;
  EndUpdate;
end;

procedure TcxCustomGridView.Updating;
begin
  BeginUpdate;
  inherited;
end;

procedure TcxCustomGridView.CreateHandlers;
begin
  FController := GetControllerClass.Create(Self);
  FDataController := GetDataControllerClass.Create(Self);
  InitDataController;
  FPainter := GetPainterClass.Create(Self);
  FViewData := GetViewDataClass.Create(Self);
  FViewInfo := CreateViewInfo;
  if GetViewInfoCacheClass <> nil then
    FViewInfoCache := GetViewInfoCacheClass.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

procedure TcxCustomGridView.DestroyHandlers;
begin
  FDataController.Free;
  FDataController := nil;
  if (Control <> nil) and Control.IsDesigning then
    Control.RemoveComponent(Site);
  FreeAndNil(FImageChangeLink);
  FViewInfo.Free;
  FViewInfo := nil;
  FreeAndNil(FViewInfoCache);
  FreeAndNil(FViewData);
  FreeAndNil(FPainter);
  FreeAndNil(FController);
end;

procedure TcxCustomGridView.InitDataController;
begin
  FDataController.OnUpdateControl := UpdateControl;
end;

procedure TcxCustomGridView.CreateOptions;
begin
  FOptionsList := TList.Create;
  FBackgroundBitmaps := GetBackgroundBitmapsClass.Create(Self);
  FOptionsBehavior := GetOptionsBehaviorClass.Create(Self);
  FOptionsData := GetOptionsDataClass.Create(Self);
  FOptionsSelection := GetOptionsSelectionClass.Create(Self);
  FOptionsView := GetOptionsViewClass.Create(Self);
  FStyles := GetStylesClass.Create(Self);
end;

procedure TcxCustomGridView.DestroyOptions;
begin
  if Control <> nil then  {5}
    TcxCustomGrid(Control).RemoveFontListener(FStyles);
  FreeAndNil(FStyles);
  FreeAndNil(FOptionsView);
  FreeAndNil(FOptionsSelection);
  FreeAndNil(FOptionsData);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FBackgroundBitmaps);
  FreeAndNil(FOptionsList);
end;

procedure TcxCustomGridView.AddOptions(AOptions: TcxCustomGridOptions);
begin
  FOptionsList.Add(AOptions);
end;

procedure TcxCustomGridView.RemoveOptions(AOptions: TcxCustomGridOptions);
begin
  FOptionsList.Remove(AOptions);
end;

procedure TcxCustomGridView.NotifyOptions(AComponent: TComponent;
  AOperation: TOperation);
var
  I: Integer;
begin
  if FOptionsList = nil then Exit;
  for I := 0 to FOptionsList.Count - 1 do
    TcxCustomGridOptions(FOptionsList[I]).Notification(AComponent, AOperation);
end;

procedure TcxCustomGridView.AddClone(AClone: TcxCustomGridView);
begin
  FClones.Add(AClone);
  AClone.FPatternGridView := Self;
end;

procedure TcxCustomGridView.RemoveClone(AClone: TcxCustomGridView);
begin
  FClones.Remove(AClone);
  AClone.FPatternGridView := nil;
end;

procedure TcxCustomGridView.GridBeginUpdate(
  AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
begin
  if Control <> nil then
    TcxCustomGrid(Control).BeginUpdate(AShowLockedStateImage);
end;

procedure TcxCustomGridView.GridCancelUpdate;
begin
  if Control <> nil then
    TcxCustomGrid(Control).CancelUpdate;
end;

procedure TcxCustomGridView.GridEndUpdate;
begin
  if Control <> nil then
    TcxCustomGrid(Control).EndUpdate;
end;

procedure TcxCustomGridView.AfterDestroyingLockedStateImage;
begin
end;

procedure TcxCustomGridView.BeforeCreatingLockedStateImage;
begin
end;

procedure TcxCustomGridView.GridBeginLockedStatePaint(AMode: TcxGridShowLockedStateImageMode);
begin
  if Control <> nil then
    TcxCustomGrid(Control).BeginLockedStatePaint(AMode);
end;

procedure TcxCustomGridView.GridEndLockedStatePaint;
begin
  if Control <> nil then
    TcxCustomGrid(Control).EndLockedStatePaint;
end;

procedure TcxCustomGridView.Synchronize(ACheckUpdateLock: Boolean = True);
var
  I: Integer;
begin
  if IsLoading or IsDestroying or (Control = nil) or
    not IsSynchronization or FAssigningPattern or FSynchronizationAssignNeeded then
    Exit;
  if ACheckUpdateLock and IsUpdateLocked then
  begin
    FSynchronizationNeeded := True;
    Exit;
  end;
  FIsSynchronizing := True;
  try
    if IsPattern then
    begin
      BeginUpdate;
      try
        for I := 0 to CloneCount - 1 do
          Clones[I].Synchronize(Self);
      finally
        EndUpdate;
      end;
    end
    else
      PatternGridView.Synchronize(Self);
  finally
    FIsSynchronizing := False;
    FSynchronizationNeeded := False;
  end;
end;

procedure TcxCustomGridView.Synchronize(AView: TcxCustomGridView);
begin
  if not IsSynchronization or FIsSynchronizing or FAssigningPattern then Exit;
  if OptionsBehavior.PostponedSynchronization and not IsPattern and not Visible then
  begin
    FSynchronizationAssignNeeded := True;
    Exit;
  end;
  BeginUpdate;
  try
    AssignPattern(AView);
    if IsPattern then Synchronize;
  finally
    EndUpdate;
    FSynchronizationAssignNeeded := False;
  end;
end;

procedure TcxCustomGridView.AssignPattern(APattern: TcxCustomGridView);
begin
  FAssigningPattern := True;
  try
    Assign(APattern);
  finally
    FAssigningPattern := False;
  end;
end;

procedure TcxCustomGridView.BeforeAssign(ASource: TcxCustomGridView);
begin
  if not AssigningSettings then
    DataController.Assign(ASource.DataController);
end;

procedure TcxCustomGridView.DoAssign(ASource: TcxCustomGridView);
begin
  with ASource do
  begin
    Self.BackgroundBitmaps := BackgroundBitmaps;
    Self.DragMode := DragMode;
    Self.Images := Images;
    Self.OptionsBehavior := OptionsBehavior;
    Self.OptionsData := OptionsData;
    Self.OptionsSelection := OptionsSelection;
    Self.OptionsView := OptionsView;
    Self.PopupMenu := PopupMenu;
    Self.Styles := Styles;
    Self.Synchronization := Synchronization;
    Self.OnCustomization := OnCustomization;
    Self.OnDblClick := OnDblClick;
    Self.OnDragDrop := OnDragDrop;
    Self.OnDragOver := OnDragOver;
    Self.OnEndDrag := OnEndDrag;
    Self.OnGetStoredProperties := OnGetStoredProperties;
    Self.OnGetStoredPropertyValue := OnGetStoredPropertyValue;
    Self.OnInitStoredObject := OnInitStoredObject;
    Self.OnKeyDown := OnKeyDown;
    Self.OnKeyPress := OnKeyPress;
    Self.OnKeyUp := OnKeyUp;
    Self.OnMouseDown := OnMouseDown;
    Self.OnMouseEnter := OnMouseEnter;
    Self.OnMouseLeave := OnMouseLeave;
    Self.OnMouseMove := OnMouseMove;
    Self.OnMouseUp := OnMouseUp;
    Self.OnMouseWheel := OnMouseWheel;
    Self.OnMouseWheelDown := OnMouseWheelDown;
    Self.OnMouseWheelUp := OnMouseWheelUp;
    Self.OnSetStoredPropertyValue := OnSetStoredPropertyValue;
    Self.OnStartDrag := OnStartDrag;
  end
end;

procedure TcxCustomGridView.DoAssignSettings(ASource: TcxCustomGridView);
begin
  Assign(ASource);
end;

procedure TcxCustomGridView.AfterAssign(ASource: TcxCustomGridView);
begin
  if not AssigningSettings then
    DataController.Assign(ASource.DataController);
end;

procedure TcxCustomGridView.BeforeRestoring;
begin
end;

procedure TcxCustomGridView.AfterRestoring;
begin
  RestoringComplete;
end;

function TcxCustomGridView.HandleSetStoredPropertyValueError(Sender: TcxStorage; const AName: string; const AValue: Variant): Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridView.RestoreFrom(const AStorageName: string; AStream: TStream;
  AReaderClass: TcxCustomReaderClass; AChildrenCreating, AChildrenDeleting: Boolean;
  AOptions: TcxGridStorageOptions; const ARestoreViewName: string;
  const AOwnerName: string = '');
var
  AStorage: TcxStorage;
  AModes: TcxStorageModes;
begin
  FStoringName := ARestoreViewName;
  AStorage := TcxStorage.Create(AStorageName, AStream);
  try
    if not IsStoringNameMode then
      if AOwnerName = '' then
        AStorage.NamePrefix := Owner.Name
      else
        AStorage.NamePrefix := AOwnerName;
    FStorageOptions := AOptions;
    AModes := [];
    if AChildrenCreating then
      Include(AModes, smChildrenCreating);
    if AChildrenDeleting then
      Include(AModes, smChildrenDeleting);
    AStorage.Modes := AModes;
    AStorage.OnSetStoredPropertyValueError := HandleSetStoredPropertyValueError;
    BeginUpdate;
    try
      IsRestoring := True;
      try
        AStorage.RestoreFrom(Self, AReaderClass);
      finally
        IsRestoring := False;
      end;
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomGridView.StoreTo(const AStorageName: string; AStream: TStream;
  AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
  AOptions: TcxGridStorageOptions; const ASaveViewName: string;
  const AOwnerName: string = '');
var
  AStorage: TcxStorage;
begin
  FStoringName := ASaveViewName;
  AStorage := TcxStorage.Create(AStorageName, AStream);
  try
    FStorageOptions := AOptions;
    if ASaveViewName = '' then
      if AOwnerName = '' then
        AStorage.NamePrefix := Owner.Name
      else
        AStorage.NamePrefix := AOwnerName;
    AStorage.ReCreate := AReCreate;
    AStorage.StoreTo(Self, AWriterClass);
  finally
    AStorage.Free;
  end;
end;

{procedure TcxCustomGridView.BeforeLevelChange;
begin
end;}

function TcxCustomGridView.CanTabStop: Boolean;
begin
  Result := TcxCustomGrid(Control).TabStop;
end;

procedure TcxCustomGridView.Deactivate;
begin
end;

procedure TcxCustomGridView.DestroyingSiteHandle;
begin
end;

procedure TcxCustomGridView.DetailDataChanged(ADetail: TcxCustomGridView);
begin
end;

procedure TcxCustomGridView.DetailVisibleChanged(ADetailLevel: TComponent;
  APrevVisibleDetailCount, AVisibleDetailCount: Integer);
begin
end;

procedure TcxCustomGridView.DoChanged(AChangeKind: TcxGridViewChangeKind);
begin
  NotifyControllerAboutChange;
end;

procedure TcxCustomGridView.DoStylesChanged;
begin
end;

procedure TcxCustomGridView.DoUnlockNotification(AInfo: TcxUpdateControlInfo);
begin
end;

function TcxCustomGridView.GetChangeable: Boolean;
begin
  Result := (Visible or IsPattern) and (Control <> nil);
end;

function TcxCustomGridView.GetCustomVisible(ALevelVisible: Boolean): Boolean;
begin
  Result := ALevelVisible and not IsPattern and
    (not IsDetail or MasterGridView.Visible and MasterGridView.IsDetailVisible(Self));
end;

function TcxCustomGridView.GetHorizontalScrollBarAreaHeight: Integer;
begin
  if SpaceForHorizontalScrollbarNeeded then
    Result := Site.GetHScrollBarAreaHeight
  else
    Result := 0;
end;

function TcxCustomGridView.GetImages: TCustomImageList;
begin
  Result := Images;
  if (Result = nil) and (Control <> nil) then
    Result := TcxCustomGridAccess(Control).Images;
end;

function TcxCustomGridView.GetIsControlFocused: Boolean;
begin
  Result := Control.IsFocused;
end;

function TcxCustomGridView.GetIsControlLocked: Boolean;
begin
  Result := (Control = nil) or TcxCustomGrid(Control).UpdateLocked;
end;

function TcxCustomGridView.GetResizeOnBoundsChange: Boolean;
begin
  Result := False;
end;

function TcxCustomGridView.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := nil;
end;

function TcxCustomGridView.GetVerticalScrollBarAreaWidth: Integer;
begin
  if (Site.GetScrollbarMode = sbmHybrid) and Site.IsScrollBarActive(sbVertical) then
    Result := Site.GetVScrollBarAreaWidth
  else
    Result := 0;
end;

function TcxCustomGridView.GetVisible: Boolean;
begin
  Result := not IsDestroying and
    GetCustomVisible((FLevel <> nil) and TcxGridLevel(FLevel).ActuallyVisible);
end;

procedure TcxCustomGridView.HideTouchScrollUI(AImmediately: Boolean);
begin
  Site.HideTouchScrollUI(Site, AImmediately);
end;

procedure TcxCustomGridView.ImageListChange(Sender: TObject);
begin
  Changed(vcLayout);
end;

procedure TcxCustomGridView.Init;
begin
end;

function TcxCustomGridView.IsDetailVisible(AGridView: TcxCustomGridView): Boolean;
begin
  Result := False;
end;

function TcxCustomGridView.IsRecordPixelScrolling: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridView.LoadingComplete;
begin
  SizeChanged;
end;

procedure TcxCustomGridView.NotifyControl(AChangeKind: TcxGridViewChangeNotificationKind);
begin
  if Control <> nil then
    TcxCustomGridAccess(Control).ViewChanged(Self, AChangeKind);
end;

procedure TcxCustomGridView.NotifyControllerAboutChange;
begin
  if IsUpdateLocked then
    FIsChangeNotificationForControllerNeeded := True
  else
  begin
    FIsChangeNotificationForControllerNeeded := False;
    if Controller <> nil then
      Controller.GridViewChanged;
  end;
end;

procedure TcxCustomGridView.RestoringComplete;
begin
end;

procedure TcxCustomGridView.ScrollContentByGesture(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
  Controller.ScrollContentByGesture(AScrollKind, ADelta);
end;

procedure TcxCustomGridView.SetLevel(Value: TComponent);
begin
  if FLevel <> Value then
  begin
    if Value is TcxGridLevel then
    begin
      if TcxGridLevel(Value).IsMaster then
        ValidateMasterDetailRelationship(True);
      if TcxGridLevel(Value).Level > 0 then
        ValidateMasterDetailRelationship(False);
    end;
    //BeforeLevelChange;
    if FLevel <> nil then
      Controller.VisibilityChanged(False);
    FLevel := Value;
    Site.InitTabStop(Site.Parent);
  end;
end;

procedure TcxCustomGridView.SetTabStop(Value: Boolean);
begin
  Site.TabStop := Value and CanTabStop;
end;

function TcxCustomGridView.SpaceForHorizontalScrollbarNeeded: Boolean;
begin
  Result := (Site.GetScrollbarMode = sbmHybrid) and Site.IsScrollBarActive(sbHorizontal);
end;

procedure TcxCustomGridView.StylesChanged;
begin
  UpdateFakeLinks;
  DoStylesChanged;
  Changed(vcSize);
end;

procedure TcxCustomGridView.UnlockNotifications;
var
  ANotification: TcxUpdateControlInfo;
begin
  if Assigned(FNotifications) then
  try
    ANotification := nil;
    while FNotifications.Count > 0 do
    try
      ANotification := FNotifications.ExtractByIndex(0);
      DoUnlockNotification(ANotification);
    finally
      FreeAndNil(ANotification);
    end;
  except
    FNotifications.Clear;
    raise;
  end;
end;

procedure TcxCustomGridView.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if not (IsLoading or IsDestroying or IsPattern) and (AInfo is TcxDataChangedInfo) then
    if IsDetail then
      if (MasterGridView <> nil) and not MasterGridView.IsUpdateLocked and
        (MasterGridView.DataController.LockCount = 0) and
        (PatternGridView <> nil) and PatternGridView.HasAsClone(Self) then
        MasterGridView.DetailDataChanged(Self)
      else
    else
      if Control <> nil then  // ext lookup editor
        TcxCustomGrid(Control).RootViewDataChanged(Self);
end;

procedure TcxCustomGridView.UpdateUnlocked;
begin
  UnlockNotifications;
  if FIsChangeNotificationForControllerNeeded then
    NotifyControllerAboutChange;
  if FIsCustomizationFormRefreshNeeded then
    RefreshCustomizationForm;
end;

function TcxCustomGridView.GetViewInfoCacheClass: TcxCustomGridViewInfoCacheClass;
begin
  Result := nil;
end;

function TcxCustomGridView.GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass;
begin
  Result := TcxCustomGridBackgroundBitmaps;
end;

function TcxCustomGridView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxCustomGridOptionsBehavior;
end;

function TcxCustomGridView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxCustomGridOptionsData;
end;

function TcxCustomGridView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxCustomGridOptionsSelection;
end;

function TcxCustomGridView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxCustomGridOptionsView;
end;

function TcxCustomGridView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxCustomGridViewStyles;
end;

procedure TcxCustomGridView.Initialize;
begin
  inherited Initialize;
  FClones := TdxFastList.Create;
  FSynchronization := True;
  CreateOptions;
  CreateHandlers;
  FNotifications := TcxGridViewNotificationList.Create(Self);
end;

procedure TcxCustomGridView.RefreshCustomizationForm;
begin
  if IsUpdateLocked then
    FIsCustomizationFormRefreshNeeded := True
  else
  begin
    FIsCustomizationFormRefreshNeeded := False;
    if Controller <> nil then
      Controller.RefreshCustomizationForm;
  end;
end;

procedure TcxCustomGridView.DoCustomization;
begin
  if Assigned(FOnCustomization) then FOnCustomization(Self);
end;

procedure TcxCustomGridView.DoInitStoredObject(AObject: TObject);
begin
  if (AObject <> nil) and Assigned(FOnInitStoredObject) then
    FOnInitStoredObject(Self, AObject);
end;

procedure TcxCustomGridView.Assign(Source: TPersistent);
begin
  if Source is TcxCustomGridView then
  begin
    BeginUpdate;
    try
      ScaleFactor.Assign(TcxCustomGridView(Source).ScaleFactor);
      try
        BeforeAssign(TcxCustomGridView(Source));
        DoAssign(TcxCustomGridView(Source));
        Init;
        AfterAssign(TcxCustomGridView(Source));
      finally
        ScaleFactor.UseOwnerValue := True;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TcxCustomGridView.AssignSettings(ASource: TcxCustomGridView);
begin
  FAssigningSettings := True;
  try
    DoAssignSettings(ASource);
  finally
    FAssigningSettings := False;
  end;
end;

procedure TcxCustomGridView.AddNotification(AUpdateInfo: TcxUpdateControlInfo);
begin
  if Assigned(FNotifications) then
    FNotifications.Add(AUpdateInfo)
  else
    AUpdateInfo.Free;
end;

function TcxCustomGridView.CanBeUsedAsDetail: Boolean;
begin
  Result := True;
end;

function TcxCustomGridView.CanBeUsedAsMaster: Boolean;
begin
  Result := True;
end;

procedure TcxCustomGridView.UnsupportedMasterDetailError;
begin
  MessageBox(0, PChar(Format('%s does not support master-detail relationships!', [ClassName])),
    'Error', MB_OK or MB_ICONSTOP or MB_TASKMODAL);
  Abort;
end;

procedure TcxCustomGridView.ValidateMasterDetailRelationship(AIsMaster: Boolean);
begin
  if AIsMaster then
  begin
    if not CanBeUsedAsMaster then
      UnsupportedMasterDetailError;
  end
  else
    if not CanBeUsedAsDetail then
      UnsupportedMasterDetailError;
end;

procedure TcxCustomGridView.BiDiModeChanged;
begin
end;

function TcxCustomGridView.CanFocus: Boolean;
begin
  Result := GetCustomVisible((FLevel <> nil) and TcxGridLevel(FLevel).CanBeVisible);
end;

function TcxCustomGridView.CanGetHitTest: Boolean;
begin
  Result := not ViewInfo.IsCalculating and not IsControlLocked;
end;

function TcxCustomGridView.GetParentComponent: TComponent;
begin                     {5}
  Result := FStorageControl;
  if Result = nil then
    Result := FRepository;
end;

function TcxCustomGridView.HasParent: Boolean;
begin                     {5}
  Result := (FStorageControl <> nil) or (FRepository <> nil);
end;

function TcxCustomGridView.HasAsClone(AGridView: TcxCustomGridView): Boolean;
begin
  Result := FClones.IndexOf(AGridView) <> -1;
end;

function TcxCustomGridView.HasAsMaster(AGridView: TcxCustomGridView): Boolean;
begin
  Result := (MasterGridView = AGridView) or
    (MasterGridView <> nil) and MasterGridView.HasAsMaster(AGridView);
end;

procedure TcxCustomGridView.Invalidate(AHardUpdate: Boolean = False);
var
  I: Integer;
begin
  if IsPattern then
    for I := 0 to CloneCount - 1 do
      Clones[I].Invalidate(AHardUpdate)
  else
    if AHardUpdate then
      LayoutChanged
    else
      ViewChanged;
end;

procedure TcxCustomGridView.RestoreDefaults;
begin
end;

procedure TcxCustomGridView.SetParentComponent(Value: TComponent);
begin     {5}
  inherited;
  if Value is TcxGridViewRepository then
    TcxGridViewRepositoryAccess(Value).AddItem(Self);
end;

function TcxCustomGridView.UseRightToLeftAlignment: Boolean;
begin
  Result := Site.UseRightToLeftAlignment;
end;

procedure TcxCustomGridView.CheckSynchronizationAssignNeeded;
begin
  if FSynchronizationAssignNeeded then
  begin
    BeginUpdate;
    try
      Synchronize(PatternGridView);
    finally
      CancelUpdate;
    end;
  end;
end;

function TcxCustomGridView.IsSynchronization: Boolean;
begin
  Result := Synchronization or IsDesigning;
end;

procedure TcxCustomGridView.BeginUpdate(
  AShowLockedStateImage: TcxGridShowLockedStateImageMode = lsimNever);
begin
  GridBeginUpdate(AShowLockedStateImage);
  Inc(FUpdateLockCount);
  if DataController <> nil then
    DataController.BeginUpdate;
end;

procedure TcxCustomGridView.CancelUpdate;
begin
  try
    if DataController <> nil then
      DataController.EndUpdate;
    if IsUpdateLocked then
    begin
      Dec(FUpdateLockCount);
      if not IsUpdateLocked then
      begin
        ClearNotifications;
        FSynchronizationNeeded := False;
      end;
    end;
  finally
    GridCancelUpdate;
  end;
end;

function TcxCustomGridView.Changed(AGridChange: TObject): Boolean;
begin
  Result := True;
  if IsLoading or IsDestroying {or IsPattern} or
    (FViewInfo = nil) or FViewInfo.IsCalculating then
  begin
    AGridChange.Free;
    Result := False;
  end
  else
    if Control = nil{5} then
      try
        with AGridChange as TcxCustomGridChange do  {7}
        begin
          Control := nil;
          Execute;
        end
      finally
        AGridChange.Free;
      end
    else
      TcxCustomGrid(Control).Changed(AGridChange as TcxCustomGridChange);
end;

procedure TcxCustomGridView.EndUpdate;
begin
  try
    if DataController <> nil then
      DataController.EndUpdate;
  finally
    try
      if (FUpdateLockCount = 1) and FSynchronizationNeeded then
        Synchronize(False);
    finally
      if IsUpdateLocked then
        Dec(FUpdateLockCount);
      if not IsUpdateLocked then
        UpdateUnlocked;
      GridEndUpdate;
    end;
  end;
end;

function TcxCustomGridView.IsUpdateLocked: Boolean;
begin
  Result := FUpdateLockCount > 0;
end;

procedure TcxCustomGridView.HideHourglassCursor;
begin
  if OptionsBehavior.ShowHourglassCursor then
    cxControls.HideHourglassCursor;
end;

procedure TcxCustomGridView.ShowHourglassCursor;
begin
  if OptionsBehavior.ShowHourglassCursor then
    cxControls.ShowHourglassCursor;
end;

function TcxCustomGridView.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := ViewInfo.GetHitTest(P);
end;

function TcxCustomGridView.GetHitTest(X, Y: Integer): TcxCustomGridHitTest;
begin
  Result := ViewInfo.GetHitTest(X, Y);
end;

procedure TcxCustomGridView.BoundsChanged(AUpdateSelfOnly: Boolean = False;
  AKeepMaster: Boolean = False);
begin
  if ResizeOnBoundsChange then
    SizeChanged(AUpdateSelfOnly, AKeepMaster)
  else
    LayoutChanged(AUpdateSelfOnly);
end;

procedure TcxCustomGridView.Changed(AChangeKind: TcxGridViewChangeKind);
begin
  if (DataController <> nil) and DataController.IsDataLoading then Exit;
  BeginUpdate;
  try
    case AChangeKind of
      vcLayout:
        LayoutChanged;
      vcSize:
        SizeChanged;
    end;
    Synchronize;
  finally
    EndUpdate;
  end;
  DoChanged(AChangeKind);
end;

procedure TcxCustomGridView.FocusChanged(AFocused: Boolean);
begin
  if AFocused then
  begin
    DataController.SetFocus;
    Controller.DoSetFocus(True);
  end
  else
    Controller.RemoveFocus;
end;

procedure TcxCustomGridView.LayoutChanged(AUpdateSelfOnly: Boolean = True);

  function GetParameter: TcxCustomGridView;
  begin
    if AUpdateSelfOnly then
      Result := Self
    else
      Result := nil;
  end;

begin
  Changed(TcxGridLayoutChange.Create(GetParameter));
end;

procedure TcxCustomGridView.LookAndFeelChanged;
var
  I: Integer;
begin
  for I := 0 to CloneCount - 1 do
    Clones[I].LookAndFeelChanged;
end;

function TcxCustomGridView.SizeChanged(AUpdateSelfOnly: Boolean = False;
  AKeepMaster: Boolean = False): Boolean;
begin
  Result := Changed(TcxGridSizeChange.Create(Self,
    AUpdateSelfOnly or (Control = nil){7}, AKeepMaster));
end;

procedure TcxCustomGridView.ViewChanged;
begin
  Changed(TcxGridViewChange.Create(Self));
end;

procedure TcxCustomGridView.ViewChanged(const AUpdateRect: TRect);
begin
  if not IsRectEmpty(AUpdateRect) then
    Changed(TcxGridViewChange.Create(Self, AUpdateRect));
end;

procedure TcxCustomGridView.ViewChanged(ARegion: TcxRegion);
begin
  if (ARegion <> nil) and not ARegion.IsEmpty then
    Changed(TcxGridViewChange.Create(Self, ARegion));
end;

procedure TcxCustomGridView.RestoreFromIniFile(const AStorageName: string;
  AChildrenCreating: Boolean = True; AChildrenDeleting: Boolean = False;
  AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
  const ARestoreViewName: string = ''; const AOwnerName: string = '');
begin
  RestoreFrom(AStorageName, nil, TcxIniFileReader, AChildrenCreating,
    AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName);
end;

procedure TcxCustomGridView.RestoreFromRegistry(const AStorageName: string;
  AChildrenCreating: Boolean = True; AChildrenDeleting: Boolean = False;
  AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
  const ARestoreViewName: string = ''; const AOwnerName: string = '');
begin
  RestoreFrom(AStorageName, nil, TcxRegistryReader, AChildrenCreating,
    AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName);
end;

procedure TcxCustomGridView.RestoreFromStream(AStream: TStream;
  AChildrenCreating: Boolean = True; AChildrenDeleting: Boolean = False;
  AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
  const ARestoreViewName: string = ''; const AOwnerName: string = '');
begin
  RestoreFrom('', AStream, TcxStreamReader, AChildrenCreating,
    AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName);
end;

procedure TcxCustomGridView.RestoreFromStorage(const AStorageName: string;
  AReaderClass: TcxCustomReaderClass;
  AChildrenCreating: Boolean = True; AChildrenDeleting: Boolean = False;
  AOptions: TcxGridStorageOptions = [gsoUseFilter, gsoUseSummary];
  const ARestoreViewName: string = ''; const AOwnerName: string = '');
begin
  RestoreFrom(AStorageName, nil, AReaderClass, AChildrenCreating,
    AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName);
end;

procedure TcxCustomGridView.StoreToIniFile(const AStorageName: string;
  AReCreate: Boolean = True; AOptions: TcxGridStorageOptions = [];
  const ASaveViewName: string = ''; const AOwnerName: string = '');
begin
  StoreTo(AStorageName, nil, TcxIniFileWriter, AReCreate, AOptions, ASaveViewName,
    AOwnerName);
end;

procedure TcxCustomGridView.StoreToRegistry(const AStorageName: string;
  AReCreate: Boolean = True; AOptions: TcxGridStorageOptions = [];
  const ASaveViewName: string = ''; const AOwnerName: string = '');
begin
  StoreTo(AStorageName, nil, TcxRegistryWriter, AReCreate, AOptions, ASaveViewName,
    AOwnerName);
end;

procedure TcxCustomGridView.StoreToStream(AStream: TStream;
  AOptions: TcxGridStorageOptions = []; const ASaveViewName: string = '';
  const AOwnerName: string = '');
begin
  StoreTo('', AStream, TcxStreamWriter, True, AOptions, ASaveViewName, AOwnerName);
end;

procedure TcxCustomGridView.StoreToStorage(const AStorageName: string;
  AWriterClass: TcxCustomWriterClass; AReCreate: Boolean = True; AOptions: TcxGridStorageOptions = [];
  const ASaveViewName: string = ''; const AOwnerName: string = '');
begin
  StoreTo(AStorageName, nil, AWriterClass, AReCreate, AOptions, ASaveViewName,
    AOwnerName);
end;

function TcxCustomGridView.CreateViewInfo: TcxCustomGridViewInfo;
begin
  Result := GetViewInfoClass.Create(Self);
end;

{ functions }

function GetViewItemUniqueName(AView: TcxCustomGridView; AItem: TComponent;
  const AItemName: string): string;
begin
  Result := CreateUniqueName(AView.Owner{!!!}, AView, AItem,
    Copy(AItem.ClassName, 1, Pos(AItemName, AItem.ClassName) - 1), '');
end;

function GetGridViewDataController(AView: TcxCustomGridView): TcxCustomDataController;
begin
  Result := AView.FDataController;
end;

function GetParentGridView(AControl: TControl): TcxCustomGridView;
begin
  while (AControl <> nil) and not (AControl is TcxGridSite) do
    AControl := AControl.Parent;
  if AControl = nil then
    Result := nil
  else
    Result := TcxGridSite(AControl).GridView;
end;

procedure AddGridViewClone(AView, AClone: TcxCustomGridView);
begin
  AView.AddClone(AClone);
end;

procedure AssignGridViewPattern(AView, APattern: TcxCustomGridView);
begin
  AView.AssignPattern(APattern);
end;

procedure SetGridViewLevel(AView: TcxCustomGridView; Value: TComponent);
begin
  AView.SetLevel(Value);
end;

procedure SiteFocusChanged(ASite: TcxGridSite);
begin
  ASite.FocusChanged;
end;

{ TcxCustomGridViewAccess }

class procedure TcxCustomGridViewAccess.AddClone(AInstance: TcxCustomGridView;
  AClone: TcxCustomGridView);
begin
  AInstance.AddClone(AClone);
end;

class procedure TcxCustomGridViewAccess.AssignPattern(AInstance: TcxCustomGridView;
  APattern: TcxCustomGridView);
begin
  AInstance.AssignPattern(APattern);
end;

class function TcxCustomGridViewAccess.CanBeUsedAsDetail(AInstance: TcxCustomGridView): Boolean;
begin
  Result := AInstance.CanBeUsedAsDetail;
end;

class function TcxCustomGridViewAccess.CanBeUsedAsMaster(AInstance: TcxCustomGridView): Boolean;
begin
  Result := AInstance.CanBeUsedAsMaster;
end;

class function TcxCustomGridViewAccess.CanFocus(AInstance: TcxCustomGridView): Boolean;
begin
  Result := AInstance.CanFocus;
end;

class procedure TcxCustomGridViewAccess.Deactivate(AInstance: TcxCustomGridView);
begin
  if AInstance <> nil then
    AInstance.Deactivate;
end;

class procedure TcxCustomGridViewAccess.DetailVisibleChanged(AInstance: TcxCustomGridView;
  ADetailLevel: TComponent; APrevVisibleDetailCount, AVisibleDetailCount: Integer);
begin
  AInstance.DetailVisibleChanged(ADetailLevel, APrevVisibleDetailCount, AVisibleDetailCount);
end;

class procedure TcxCustomGridViewAccess.FocusChanged(AInstance: TcxCustomGridView;
  AFocused: Boolean);
begin
  AInstance.FocusChanged(AFocused);
end;

class procedure TcxCustomGridViewAccess.LookAndFeelChanged(AInstance: TcxCustomGridView);
begin
  AInstance.LookAndFeelChanged;
end;

class procedure TcxCustomGridViewAccess.SetLevel(AInstance: TcxCustomGridView;
  Value: TComponent);
begin
  AInstance.SetLevel(Value);
end;

class function TcxCustomGridViewAccess.GetChangeable(AInstance: TcxCustomGridView): Boolean;
begin
  Result := AInstance.Changeable;
end;

class function TcxCustomGridViewAccess.GetStyles(AInstance: TcxCustomGridView): TcxCustomGridStyles;
begin
  Result := AInstance.Styles;
end;

class procedure TcxCustomGridViewAccess.AfterDestroyingLockedStateImage(AInstance: TcxCustomGridView);
begin
  AInstance.AfterDestroyingLockedStateImage;
end;

class procedure TcxCustomGridViewAccess.BeforeCreatingLockedStateImage(AInstance: TcxCustomGridView);
begin
  AInstance.BeforeCreatingLockedStateImage;
end;

{ TcxGridRegisteredViews }

type
  TcxGridRegisteredViews = class(TcxRegisteredClasses)
  protected
    function CompareItems(AIndex1, AIndex2: Integer): Integer; override;
  public
    constructor Create;
  end;

constructor TcxGridRegisteredViews.Create;
begin
  inherited Create(True);
  Sorted := True;
end;

function TcxGridRegisteredViews.CompareItems(AIndex1, AIndex2: Integer): Integer;
var
  AIsDBView1, AIsDBView2: Boolean;
begin
  AIsDBView1 := Pos('DB', Descriptions[AIndex1]) = 1;
  AIsDBView2 := Pos('DB', Descriptions[AIndex2]) = 1;
  if not AIsDBView1 and AIsDBView2 then
    Result := -1
  else
    if AIsDBView1 and not AIsDBView2 then
      Result := 1
    else
      Result := inherited CompareItems(AIndex1, AIndex2);
end;

initialization
  GridHitTests := TGridHitTests.Create;
  cxGridRegisteredViews := TcxGridRegisteredViews.Create;

finalization
  FreeAndNil(cxGridRegisteredViews);
  FreeAndNil(GridHitTests);

end.

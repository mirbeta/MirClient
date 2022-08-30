{*************************************************************************}
{ TAdvSmoothDock component                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSmoothDock;

{$I TMSDEFS.INC}

interface

uses
  Forms, Windows, Messages, Graphics, Controls, Classes,
  AdvStyleif, GDIPFill, Sysutils, Math,
  ExtCtrls, ActiveX, Menus, AdvSmoothDockDragDrop, OleCtnrs,
  ShellApi, AxCtrls,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  //v1.0.0.0 : first release
  //v1.0.0.1 : Fixed : issue with Array length of Sinus calculation
  //v1.0.0.2 : Fixed : Issue with infinitive jumping of item.
  //v1.0.0.3 : Fixed : Issue with hiding / showing the dock at runtime.
  //         : Fixed : Issue with destroying AdvSmoothDock at runtime.
  //v1.0.1.0 : New : Exposed event OnItemStartDrag to enable / disable dragging
  //v1.0.2.0 : New : Support for Windows Vista and Windows Seven Style
  //         : Fixed : Issue with Parent form show / hide dock
  //         : Fixed : Issue with destroying form at runtime with AdvSmoothDock component
  //v1.0.3.0 : New : Built-in support for reduced color set for use with terminal servers
  //         : Improved : Separator drawing when Separatorsize is < 4
  //v1.0.3.1 : Improved : Reparenting forms with FormHookInit and FormHookDone
  //v1.0.3.2 : Fixed : Issue with Visible property
  //v1.0.3.3 : Improved : Activate / Deactivate of Parent Form
  //v1.0.4.0 : New : Delphi 2010 Touch Support
  //v1.0.4.1 : Fixed : Issue with Background fill not drawing when active
  //         : Fixed : Issue with control size in combination with other controls
  //         : Improved : Items jumping out of bounds (images not visible when jumping)
  //v1.0.5.0 : New : Property AutoSize
  //         : Fixed : Issue with Aligned and control size
  //         : Fixed : Issue with visible / unvisible items
  //         : Fixed : Issue with Platform size
  //         : Fixed : Issue with Spacing
  //         : Fixed : Issue with BackGround size
  //v1.0.5.1 : Fixed : Issue with activating/deactivating when using DockOnDesktop
  //v1.0.5.2 : Fixed : Issue with Changing Parent form position
  //v1.0.5.3 : Fixed : Issue with Selection not showing
  //         : Fixed : Issue with repainting when activating/deactivating
  //         : Improved : Keyboard handling
  //v1.0.6.0 : New : BackGroundSize and BackGroundAutoSize properties to control the background fill size
  //v1.0.6.1 : Improved : OnItemSelected called when selecting item with SelectedItemIndex
  //v1.0.6.2 : Fixed: Issue with OnItemSelected called twice
  //v1.0.7.0 : Improved : Event handling for drag and drop
  //v1.0.7.1 : Improved : Keyboard selection with visible and enabled only items
  //         : Fixed : Issue when setting formstyle to fsMDIForm
  //v1.0.8.0 : New : Built-in support for Office 2010 colors
  //v1.0.8.1 : Fixed : Issue with modal result window form not active
  //v1.1.0.0 : New : PermanentCaption property on ItemAppearance to set a permanent caption on the items
  //         : Fixed : Issue with flickering when mouse between items
  //v1.1.0.1 : Improved : Check if parent form is MDI form
  //         : Fixed : invalid floating point operation
  //v1.1.0.2 : Fixed : Issue with AnimationFactor = 0
  //v1.1.0.3 : New : HoverXY to hover programatically
  //v1.1.0.4 : Fixed : Mouse events not called.
  //v1.1.0.5 : Fixed : Issue with Tag property changed during drag-drop
  //v1.1.1.0 : New : OnDraw event to draw on top of dock
  //v1.1.2.0 : Improved : Automatic Wordwrapped text
  //         : Fixed : Issue with Mouse leave while setting Visible to false
  //         : Fixed : Issue with automatic positioning of text
  //         : Fixed : Issue with hovering and message dialog
  //v1.1.2.1 : Improved : Added Hover parameter to the GetItemRect to get item rectangle in normal state
  //v1.1.2.2 : Fixed : Issue with destroying dock in older Delphi versions
  //v1.1.2.3 : Fixed : Issue with items not hovering after selection
  //         : Fixed : Issue with Reflection in different positions
  //v1.1.2.4 : Fixed : Issue with setting switching visibility
  //v1.2.0.0 : New : Metro style support
  //v1.2.0.1 : Fixed : Issue with active main form and MDI parent form
  //v1.2.0.2 : Fixed : Issue with form unhooking when destroying component at runtime
  //v1.2.1.0 : New : Property DragDropItemInteraction to turn off default item delete / reordering / inserting behavior
  //v1.2.1.1 : Fixed : Issue with 64bit and Windows 8
  //v1.3.0.0 : New : Windows 8, Office 2013 styles added
  //v1.3.0.1 : Fixed : Access violation in Delphi 7 when toggling visibility
  //v1.3.1.0 : Improved : StayOnTop property
  //v1.3.1.1 : Fixed : Issue with RevokeDragDrop() not being called
  //v1.3.1.2 : Fixed : Issue with arrow handling and visible items
  //v1.3.1.3 : Fixed : Issue with keyboard handling and selected item
  //v1.3.1.4 : Fixed : access violation on form level when toggling visibility
  //v1.3.1.5 : Fixed : Issue with alt key
  //v1.3.1.6 : New : DisabledImage property
  //v1.4.0.0 : New : Windows 10, Office 2016 styles added

  DRAGMARGIN = 5;
  WM_USERACTIVATE = WM_USER + 100;

type
  TAdvSmoothDock = class;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothDockForm = class(TForm)
  private
    FMouseEntered: Boolean;
    OldWndProc, NewWndProc: Pointer;
    FMainBuffer: TGPBitmap;
    FDock: TAdvSmoothDock;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure FormHookInit;
    procedure FormHookDone;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;
    procedure DblClick; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;

    // ---- Paint proc
    procedure Draw(graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    //---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
    procedure WndProc(var Message: TMessage); override;
    procedure HookWndProc(var Msg: TMessage);
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property OwnerDock: TAdvSmoothDock read FDock write FDock;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  TAdvSmoothDockStatus = class(TPersistent)
  private
    FOwner: TAdvSmoothDock;
    FOffsetTop: integer;
    FOffsetLeft: integer;
    FVisible: Boolean;
    FCaption: String;
    FAppearance: TGDIPStatus;
    FOnChange: TNotifyEvent;
    procedure SetAppearance(const Value: TGDIPStatus);
    procedure SetCaption(const Value: String);
    procedure SetOffsetLeft(const Value: integer);
    procedure SetOffsetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothDock);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default false;
    property Caption: String read FCaption write SetCaption;
    property OffsetLeft: integer read FOffsetLeft write SetOffsetLeft default 0;
    property OffsetTop: integer read FOffsetTop write SetOffsetTop default 0;
    property Appearance: TGDIPStatus read FAppearance write SetAppearance;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothDockItem = class(TCollectionItem)
  private
    dolastjumpdown, dolastjumpup: Boolean;
    FItemRect: TGPRectF;
    FInsertItem: Boolean;
    FDoItemAnimation: Boolean;
    FOwner: TAdvSmoothDock;
    FJMP, FJMPTO: Double;
    FHoverSizeW, FHoverSizeToW: Double;
    FHoverSizeH, FHoverSizeToH: Double;
    FReflectionImage: TGPBitmap;
    FImage: TAdvGDIPPicture;
    FEnabled: Boolean;
    FVisible: Boolean;
    FCaption: String;
    FHint: String;
    FDoLastJump, FJump, FJumpUp, FJumpDown: Boolean;
    FPopupMenu: TPopupMenu;
    FStatusIndicator: TAdvSmoothDockStatus;
    FSeparator: Boolean;
    FShowCaption: Boolean;
    FProgressMaximum: integer;
    FProgressMinimum: integer;
    FProgressPosition: integer;
    FFilePath: String;
    FObject: TObject;
    FTag: integer;
    FInternal: integer;
    FData: string;
    FWordWrapping: Boolean;
    FDisabledImage: TAdvGDIPPicture;
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetCaption(const Value: String);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetHint(const Value: String);
    procedure SetJump(const Value: Boolean);
    procedure SetStatusIndicator(const Value: TAdvSmoothDockStatus);
    procedure SetSeparator(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetProgressMaximum(const Value: integer);
    procedure SetProgressMinimum(const Value: integer);
    procedure SetProgressPosition(const Value: integer);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetDisabledImage(const Value: TAdvGDIPPicture);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
    procedure StatusIndicatorChanged(Sender: TObject);
    procedure UpdateReflection;
    procedure Popup(Sender: TObject);
  public
    function GetItemRect(Hover: Boolean = True): TGPRectF;
    function GetItemRectCenter(Hover: Boolean = True): TGPRectF;
    function GetHoverRect(Hover: Boolean = True): TGPRectF;
    function GetNormalRect(Hover: Boolean = True): TGPRectF;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FilePath: String read FFilePath write FFilePath;
    function GetVisibleIndex(Item: Integer): Integer;
  published
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property DisabledImage: TAdvGDIPPicture read FDisabledImage write SetDisabledImage;
    property Caption: String read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Data: string read FData write FData;
    property Tag: integer read FTag write FTag;
    property ItemObject: TObject read FObject write FObject;
    property Hint: String read FHint write SetHint;
    property Jump: Boolean read FJump write SetJump default false;
    property StatusIndicator: TAdvSmoothDockStatus read FStatusIndicator write SetStatusIndicator;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Separator: Boolean read FSeparator write SetSeparator default false;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default true;
    property ProgressMaximum: integer read FProgressMaximum write SetProgressMaximum default 100;
    property ProgressMinimum: integer read FProgressMinimum write SetProgressMinimum default 0;
    property ProgressPosition: integer read FProgressPosition write SetProgressPosition default 0;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default True;
  end;

  TAdvSmoothDockItems = class(TCollection)
  private
    FOwner: TAdvSmoothDock;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothDockItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothDockItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothDock);
    function Add: TAdvSmoothDockItem;
    function Insert(Index: Integer): TAdvSmoothDockItem;
    property Items[Index: Integer]: TAdvSmoothDockItem read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothDockItemAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothDock;
    FImageHeight: integer;
    FImageWidth: integer;
    FOnChanged: TNotifyEvent;
    FSpacing: integer;
    FFill: TGDIPFill;
    FMaxImageWidth: integer;
    FMaxImageHeight: integer;
    FBackGroundVisible: Boolean;
    FReflectionSpacing: integer;
    FImageOffset: integer;
    FReflectionStart: Byte;
    FReflectionEnd: Byte;
    FReflectionSize: integer;
    FSelectionFill: TGDIPFill;
    FCaptionFill: TGDIPFill;
    FCaptionFont: TFont;
    FSelectionSize: integer;
    FSelectionOffset: integer;
    FSelectedFill: TGDIPFill;
    FHoverFill: TGDIPFill;
    FDisabledFill: TGDIPFill;
    FItemFill: Boolean;
    FProgressFill: TGDIPFill;
    FSeparatorFill: TGDIPFill;
    FSeparatorSize: integer;
    FAspectRatio: Boolean;
    FJumpMargin: integer;
    FAnimationSpan: integer;
    FShowSelection: Boolean;
    FPermanentCaption: Boolean;
    procedure SetImageHeight(const Value: integer);
    procedure SetImageWidth(const Value: integer);
    procedure SetSpacing(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetMaxImageHeight(const Value: integer);
    procedure SetMaxImageWidth(const Value: integer);
    procedure SetReflectionSpacing(const Value: integer);
    procedure SetImageOffset(const Value: integer);
    procedure SetReflectionEnd(const Value: Byte);
    procedure SetReflectionStart(const Value: Byte);
    procedure SetReflectionSize(const Value: integer);
    procedure SetSelectionFill(const Value: TGDIPFill);
    procedure SetCaptionFill(const Value: TGDIPFill);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetSelectionSize(const Value: integer);
    procedure SetSelectionOffset(const Value: integer);
    procedure SetHoverFill(const Value: TGDIPFill);
    procedure SetSelectedFill(const Value: TGDIPFill);
    procedure SetDisabledFill(const Value: TGDIPFill);
    procedure SetItemFill(const Value: Boolean);
    procedure SetProgressFill(const Value: TGDIPFill);
    procedure SetSeparatorFill(const Value: TGDIPFill);
    procedure SetSeparatorSize(const Value: integer);
    procedure SetAspectRatio(const Value: Boolean);
    procedure SetJumpMargin(const Value: integer);
    procedure SetAnimationSpan(const Value: integer);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetPermanentCaption(const Value: Boolean);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure SeparatorChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothDock);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property JumpMargin: integer read FJumpMargin write SetJumpMargin default 40;
    property Fill: TGDIPFill read FFill write SetFill;
    property AspectRatio: Boolean read FAspectRatio write SetAspectRatio default true;
    property HoverFill: TGDIPFill read FHoverFill write SetHoverFill;
    property SelectedFill: TGDIPFill read FSelectedFill write SetSelectedFill;
    property DisabledFill: TGDIPFill read FDisabledFill write SetDisabledFill;
    property ItemBackGround: Boolean read FItemFill write SetItemFill default false;
    property ImageWidth: integer read FImageWidth write SetImageWidth default 50;
    property ImageHeight: integer read FImageHeight write SetImageHeight default 50;
    property ImageOffset: integer read FImageOffset write SetImageOffset default 40;
    property MaximumImageHeight: integer read FMaxImageHeight write SetMaxImageHeight default 120;
    property MaximumImageWidth: integer read FMaxImageWidth write SetMaxImageWidth default 120;
    property Spacing: integer read FSpacing write SetSpacing default 40;
    property OnChange: TNotifyEvent read FOnChanged write FOnChanged;
    property ReflectionSize: integer read FReflectionSize write SetReflectionSize default 50;
    property ReflectionStart: Byte read FReflectionStart write SetReflectionStart default 100;
    property ReflectionEnd: Byte read FReflectionEnd write SetReflectionEnd default 0;
    property ReflectionSpacing: integer read FReflectionSpacing write SetReflectionSpacing default 0;
    property AnimationSpan: integer read FAnimationSpan write SetAnimationSpan default 400;
    property SelectionFill: TGDIPFill read FSelectionFill write SetSelectionFill;
    property SelectionSize: integer read FSelectionSize write SetSelectionSize default 20;
    property SelectionOffset: integer read FSelectionOffset write SetSelectionOffset default 10;
    property CaptionFill: TGDIPFill read FCaptionFill write SetCaptionFill;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property ProgressFill: TGDIPFill read FProgressFill write SetProgressFill;
    property SeparatorFill: TGDIPFill read FSeparatorFill write SetSeparatorFill;
    property SeparatorSize: integer read FSeparatorSize write SetSeparatorSize default 10;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default false;
    property PermanentCaption: Boolean read FPermanentCaption write SetPermanentCaption default false;
  end;

  TAnimationPoint = record
    pt: TGPPointF;
    factor: Double;
  end;

  TDockItemArray = Array of TAdvSmoothDockItem;

  TAdvSmoothDockPosition = (dpLeft, dpRight, dpTop, dpBottom);

  TAdvSmoothDockBeforeDragOver = procedure(Sender, Source: TObject; HoveredItem: TAdvSmoothDockItem; var CreateNew: Boolean) of object;

  TAdvSmoothDockDragOver = procedure(Sender, Source: TObject; DragItem: TAdvSmoothDockItem; X, Y: Integer; State: TDragState; var Accept: Boolean) of object;

  TAdvSmoothDockDragDrop = procedure(Sender, Source: TObject; DragItem: TAdvSmoothDockItem; X, Y: Integer) of object;

  TAdvSmoothDockItemHint = procedure(Sender: TObject; ItemIndex: integer; var Hint: String) of object;

  TAdvSmoothDockItemClick = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothDockItemDblClick = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothDockItemSelected = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothDockItemHover = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothDockItemDragDelete = procedure(Sender: TObject; ItemIndex: integer; var Allow: Boolean) of object;

  TAdvSmoothDockDragInsert = procedure(Sender: TObject; PreviousItemIndex, NextItemIndex: integer; var AllowInsert: Boolean) of object;

  TAdvSmoothDockItemStartDrag = procedure(Sender: TObject; DragItem: TAdvSmoothDockItem; var Allow: Boolean) of object;

  TAdvSmoothDockAlignDesktop = (adBottom, adLeft, adRight, adTop);

  TAdvSmoothDockDropTarget = class(TDockDropTarget)
  private
    FDock: TAdvSmoothDock;
  public
    constructor Create(ADock:TAdvSmoothDock);
    procedure DropText(pt:TPoint;s:string); override;
    procedure DropCol(pt:TPoint;Col: Integer); override;
    procedure DropRTF(pt:TPoint;s:string); override;
    procedure DropFiles(pt:TPoint;files:tstrings); override;
    procedure DropURL(pt:TPoint;s:string); override;
    procedure DragMouseMove(pt:TPoint;var Allow: Boolean; DropFormats:TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TAdvSmoothDockArrowAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothDock;
    FOpacity: Byte;
    FBorderColor: TColor;
    FColor: TColor;
    FVisible: Boolean;
    FSize: integer;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetSize(const Value: integer);
    procedure SetVisibled(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothDock);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Opacity: Byte read FOpacity write SetOpacity default 200;
    property Size: integer read FSize write SetSize default 35;
    property Visible: Boolean read FVisible write SetVisibled default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCaptionSize = record
    w: Single;
    h: Single;
  end;

  TAdvSmoothDockBounds = record
    Start, Stop: integer;
  end;

  TAdvSmoothDockDrawEvent = procedure(Sender: TObject; g: TGPGraphics; R: TGPRectF) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothDock = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FCur: TCursor;
    FTimeDownOnArrow: integer;
    FAnimateMove: Boolean;
    FCurrentPos, FPosTo: Single;
    FArrowLeftHover, FArrowLeftDown, FArrowRightHover, FArrowRightDown: Boolean;
    UpdateBounds, FMouseOnArrow: Boolean;
    frm: TAdvSmoothDockForm;
    FFocused: Boolean;
    FSel: Integer;
    FDblClick: Boolean;
    FOleDropTargetAssigned: Boolean;
    FDockDropTarget: TAdvSmoothDockDropTarget;
    FSeparatorMemoryStream: TMemoryStream;
    FSeparatorImage: TGPBitmap;
    FConstructed: Boolean;
    FMouseEntered: Boolean;
    FStartPopupTimer, FDoPopup: Boolean;
    FPopupTime: integer;
    FDesigntime: Boolean;
    FMouseDown, FMouseCopy: Boolean;
    FCreateNew: Boolean;
    FDragDropItem, FHoveredDropItem, FInsertDropItem: TAdvSmoothDockItem;
    FClickX, FClickY: integer;
    FDragX, FDragY: integer;
    FPrevX, FPrevY, FCurX, FCurY: integer;
    FHintX, FHintY: integer;
    FHoveredItemIndex, FPopupClickIndex: integer;
    FUpdateCount: integer;
    //FBlockPaint: Boolean;
    FAnimate: TTimer;
    FSin: array of TAnimationPoint;
    FFill: TGDIPFill;
    FTransparent: Boolean;
    FItems: TAdvSmoothDockItems;
    FItemAppearance: TAdvSmoothDockItemAppearance;
    FSelectedItemIndex: integer;
    FPosition: TAdvSmoothDockPosition;
    FPlatformFill: TGDIPFill;
    FPlatformSize: integer;
    FOnDragDrop: TAdvSmoothDockDragDrop;
    FOnDragOver: TAdvSmoothDockDragOver;
    FOnItemHint: TAdvSmoothDockItemHint;
    FPlatForm3D: Boolean;
    FOleDragDrop: Boolean;
    FAnimationFactor: integer;
    FOnDragInsert: TAdvSmoothDockDragInsert;
    FOnItemDblClick: TAdvSmoothDockItemDblClick;
    FOnItemClick: TAdvSmoothDockItemClick;
    FOnItemSelected: TAdvSmoothDockItemSelected;
    FOnItemHover: TAdvSmoothDockItemHover;
    FFocus: Boolean;
    FPlatForm3DColor: TColor;
    FPlatForm3DColorTo: TColor;
    FPlatForm3DOpacity: Byte;
    FPlatForm3DOpacityTo: Byte;
    FDockOnDeskTop: Boolean;
    FDockVisible: Boolean;
    FArrowAppearance: TAdvSmoothDockArrowAppearance;
    FOnScroll: TNotifyEvent;
    FOnItemStartDrag: TAdvSmoothDockItemStartDrag;
    FOnItemDragDelete: TAdvSmoothDockItemDragDelete;
    FAutoSize: Boolean;
    FBackGroundSize: Integer;
    FBackGroundAutoSize: Boolean;
    FOnBeforeDragOver: TAdvSmoothDockBeforeDragOver;
    FOnDraw: TAdvSmoothDockDrawEvent;
    FDragDropItemInteraction: Boolean;
    FStayOnTop: Boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTransparent(const Value: Boolean);
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetItems(const Value: TAdvSmoothDockItems);
    procedure SetItemAppearance(const Value: TAdvSmoothDockItemAppearance);
    procedure SetSelectedItemIndex(const Value: integer);
    procedure SetPosition(const Value: TAdvSmoothDockPosition);
    procedure SetPlatformFill(const Value: TGDIPFill);
    procedure SetPlatformSize(const Value: integer);
    procedure SetPlatForm3D(const Value: Boolean);
    procedure SetOleDragDrop(const Value: Boolean);
    function GetVersion: String;
    procedure SetAnimationFactor(const Value: integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetPlatForm3DColor(const Value: TColor);
    procedure SetPlatForm3DColorTo(const Value: TColor);
    procedure SetPlatForm3DOpacity(const Value: Byte);
    procedure SetPlatForm3DOpacityTo(const Value: Byte);
    procedure SetDockOnDesktop(const Value: Boolean);
    procedure SetDockVisible(const Value: Boolean);
    procedure SetArrowAppearance(const Value: TAdvSmoothDockArrowAppearance);
    function GetFirstIndex: integer;
    function GetVisibleItemCount: integer;
    procedure SetAS(const Value: Boolean);
    procedure SetBackGroundAutoSize(const Value: Boolean);
    procedure SetBackGroundSize(const Value: Integer);
    procedure SetStayOnTop(const Value: Boolean);
  protected
    procedure Changed;
    function GetFirstVisibleItem: Integer;
    function GetLastVisibleItem: Integer;
    procedure FillChanged(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure ItemAppearanceChanged(Sender: TObject);
    procedure ArrowAppearanceChanged(Sender: TObject);
    procedure DrawBackGround(g: TGPGraphics; Control: Boolean);
    procedure DrawPlatForm(g: TGPGraphics);
    procedure DrawItems(g: TGPGraphics);
    procedure DrawCaption(g: TGPGraphics; ItemIndex: Integer = -1);
    procedure DrawArrows(g: TGPGraphics);
    procedure DrawIndicator(g: TGPGraphics; ItemIndex: integer);
    procedure DoMouseEnter(var Msg: TMessage);
    procedure DoMouseLeave(var Msg: TMessage);
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CalculateSin;
    procedure Animate(Sender: TObject);
    procedure DoDblClick;
    procedure DoDrop(X, Y: integer);
    procedure DoDrag(X, Y: integer; Copy: Boolean);
    procedure DoDragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Source: TObject; X, Y: Integer);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure GetAspectSize(var w, h: integer; ow, oh, nw, nh: double; Separator: Boolean);
    function FactorInSinus(X, Y: integer; pt: TGPPointF): Double;
    function InsideRect: TRect;
    function GetBounds: TAdvSmoothDockBounds;
    function GetItemBounds: integer;
    function IsItemBounds(X, Y: integer): Boolean;
    function IsMoving(X, Y: integer): Boolean;
    function GetVersionNr: Integer;
    function GetWidth: integer;
    function GetActive: Boolean;
    function GetHeight: integer;
    function GetTotalSize(Hover: Boolean = True): Double;
    function GetMinimumHeight: integer;
    function GetMinimumWidth: integer;
    function GetMaximumHeight: integer;
    function GetMaximumWidth: integer;
    function GetItemsRectangle: TGPRectF;
    function GetItemsMinimumRectangle: TGPRectF;
    function GetArrowLeft: TGPRectF;
    function GetArrowRight: TGPRectF;
    function IsMouseOnArrow(X, Y: integer): Boolean;
    function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
    function GetPlatFormRectangle: TGPRectF;
    function GetMaxSizeCaption: TCaptionSize;
    function IsJumping: Boolean;
    procedure DoArrowRightDown;
    procedure DoArrowLeftDown;
    function GetCountSelectable: Integer;
    function MaxSel: integer;
    function MinSel: integer;
    property DockVisible: Boolean read FDockVisible write SetDockVisible default false;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    function GetForm: TAdvSmoothDockForm;
    procedure DoItemClick(ItemIndex: Integer); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure AlignDesktop(Align: TAdvSmoothDockAlignDesktop); virtual;
    property DockOnDeskTop: Boolean read FDockOnDeskTop write SetDockOnDesktop default false;
    procedure Resize; override;
    procedure UpdateSize;
    procedure AddImagesFromFolder(AFolder: String; SetImageCaption: boolean = false);
    procedure AddFilesFromFolder(AFolder: String; SetImageCaption: boolean = false; SetFilePath: Boolean = false; LoadIcon: Boolean = false);
    procedure BeginUpdate;
    procedure EndUpdate;
    function XYToItem(X, Y: integer; CountSeparator: Boolean = false; DragDrop: Boolean = false): integer;
    procedure ShowForm;
    property FirstVisibleIndex: integer read GetFirstIndex;
    property VisibleItemCount: integer read GetVisibleItemCount;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    procedure FormHookInit;
    procedure FormHookDone;
    procedure HoverXY(X, Y: Integer);
  published
    property DragDropItemInteraction: Boolean read FDragDropItemInteraction write FDragDropItemInteraction default True;
    property OnDraw: TAdvSmoothDockDrawEvent read FOnDraw write FOnDraw;
    property AutoSize: Boolean read FAutoSize write SetAS default true;
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 2;
    property Fill: TGDIPFill read FFill write SetFill;
    property BackGroundAutoSize: Boolean read FBackGroundAutoSize write SetBackGroundAutoSize default true;
    property BackGroundSize: Integer read FBackGroundSize write SetBackGroundSize default 200;
    property PlatformFill: TGDIPFill read FPlatformFill write SetPlatformFill;
    property PlatformSize: integer read FPlatformSize write SetPlatformSize default 60;
    property Transparent: Boolean read FTransparent write SetTransparent default true;
    property Items: TAdvSmoothDockItems read FItems write SetItems;
    property ItemAppearance: TAdvSmoothDockItemAppearance read FItemAppearance write SetItemAppearance;
    property SelectedItemIndex: integer read FSelectedItemIndex write SetSelectedItemIndex default -1;
    property Position: TAdvSmoothDockPosition read FPosition write SetPosition default dpBottom;
    property PlatForm3D: Boolean read FPlatForm3D write SetPlatForm3D default true;
    property PlatForm3DColor: TColor read FPlatForm3DColor write SetPlatForm3DColor default clWhite;
    property PlatForm3DColorTo: TColor read FPlatForm3DColorTo write SetPlatForm3DColorTo default clWhite;
    property PlatForm3DOpacity: Byte read FPlatForm3DOpacity write SetPlatForm3DOpacity default 30;
    property PlatForm3DOpacityTo: Byte read FPlatForm3DOpacityTo write SetPlatForm3DOpacityTo default 100;
    property ArrowAppearance: TAdvSmoothDockArrowAppearance read FArrowAppearance write SetArrowAppearance;
    property OleDragDrop: Boolean read FOleDragDrop write SetOleDragDrop default false;
    property Version: String read GetVersion;
    property ShowFocus: Boolean read FFocus write SetShowFocus default true;
    property StayOnTop: Boolean read FStayOnTop write SetStayOnTop default True;

    property OnBeforeDragOver: TAdvSmoothDockBeforeDragOver read FOnBeforeDragOver write FOnBeforeDragOver;
    property OnDragOver: TAdvSmoothDockDragOver read FOnDragOver write FOnDragOver;
    property OnDragDrop: TAdvSmoothDockDragDrop read FOnDragDrop write FOnDragDrop;
    property OnItemHint: TAdvSmoothDockItemHint read FOnItemHint write FOnItemHint;
    property OnItemClick: TAdvSmoothDockItemClick read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TAdvSmoothDockItemDblClick read FOnItemDblClick write FOnItemDblClick;
    property OnItemHover: TAdvSmoothDockItemHover read FOnItemHover write FOnItemHover;
    property OnDragInsert: TAdvSmoothDockDragInsert read FOnDragInsert write FOnDragInsert;
    property OnItemSelected: TAdvSmoothDockItemSelected read FOnItemSelected write FOnItemSelected;
    property OnItemStartDrag: TAdvSmoothDockItemStartDrag read FOnItemStartDrag write FOnItemStartDrag;
    property OnItemDragDelete: TAdvSmoothDockItemDragDelete read FOnItemDragDelete write FOnItemDragDelete;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property Hint;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


function SaveRound(Val: Double): Integer;
begin
  if Val < 0 then
    Result := Round(Max(Val, -MaxInt))
  else
    Result := Round(Min(Val, MaxInt))
end;

function Lighter(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r + muldiv(255 - r, Percent, 100); //Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(r, g, b);
end;

function Darker(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := SaveRound(Start + Delta)
    else
      Start := SaveRound(Start - Delta);
  end;
end;

{ TAdvSmoothDock }

type
  TFileInfo = record
    Icon : hIcon;
    Image : Integer;
    DisplayName : String;
    TypeName : String;
    Size : Integer;
    SizeDescription : String;
    DateTime : TDateTime;
    AttrArchive : Boolean;
    AttrReadOnly : Boolean;
    AttrSystem : Boolean;
    AttrHidden : Boolean;
    AttrVolume : Boolean;
    AttrDirectory : Boolean;
  end;

function scGetSizeDescription(const IntSize : Int64) : String;
begin
  if IntSize < 1024 then
    Result := IntToStr(IntSize)+' bytes'
  else
  begin
    if IntSize < (1024 * 1024) then
      Result := FormatFloat('####0.##',IntSize / 1024)+' Kb'
    else
      if IntSize < (1024 * 1024 * 1024) then
        Result := FormatFloat('####0.##',IntSize / 1024 / 1024)+' Mb'
      else
        Result := FormatFloat('####0.##',IntSize / 1024 / 1024 / 1024)+' Gb';
  end;
end;

procedure scGetFileInfo(StrPath : String; var Info : TFileInfo);
var
  SHFileInfo : TSHFileInfo;
  SearchRec : TSearchRec;
begin
  if Trim(StrPath) = '' then
    Exit;

  ShGetFileInfo(PChar(StrPath), 0, SHFileInfo, SizeOf (TSHFileInfo),
    SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX or SHGFI_ICON);

  with Info do
  begin
    Icon  := SHFileInfo.hIcon;
    Image := SHFileInfo.iIcon;
    DisplayName := SHFileInfo.szDisplayName;
    TypeName := SHFileInfo.szTypeName;
  end;

  FindFirst(StrPath, 0, SearchRec);
  with Info do
  begin
    try
//      DateTime := FileDateToDateTime(SearchRec.Time);
    except
      DateTime := Now();
    end;

    {$WARNINGS OFF}
    AttrReadOnly := ((SearchRec.Attr and faReadOnly) > 0);
    AttrSystem := ((SearchRec.Attr and faSysFile) > 0);
    AttrHidden := ((SearchRec.Attr and faHidden) > 0);
    AttrArchive := ((SearchRec.Attr and faArchive) > 0);
    AttrVolume := ((SearchRec.Attr and faVolumeID) > 0);
    AttrDirectory := ((SearchRec.Attr and faDirectory) > 0);
    {$WARNINGS ON}

    Size := SearchRec.Size;

    SizeDescription := scGetSizeDescription(Size);
  end;
end;

procedure TAdvSmoothDock.AddFilesFromFolder(AFolder: String;
  SetImageCaption, SetFilePath, LoadIcon: boolean);
var
  SR: TSearchRec;
  m: TMemoryStream;
  ic: TIcon;
  inf: TFileInfo;

  procedure AddToList(s: string);
  begin
    with Items.Add do
    begin
      try
        Image.LoadFromFile(s);
        if SetFilePath then
          FFilePath := s;
        if SetImageCaption then
          FCaption := ExtractFileName(s);

        if LoadIcon then
        begin
          scGetFileInfo(s, inf);
           //ICON
          ic := TIcon.Create;
          try
           ic.Handle := inf.Icon;
           if not ic.Empty then
           begin
             m := TMemoryStream.Create;
             ic.SaveToStream(m);
             Image.LoadFromStream(m);
             m.Free;
           end;
          finally
           ic.free;
          end;
        end;
      except
        Image.Assign(nil);
      end;
    end;
  end;

begin
  BeginUpdate;
  if FindFirst(AFolder,faAnyFile-faDirectory,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);
  EndUpdate;
end;

procedure TAdvSmoothDock.AddImagesFromFolder(AFolder: String;
  SetImageCaption: boolean);
var
  SR: TSearchRec;

  procedure AddToList(s: string);
  begin
    with Items.Add do
    begin
      try
        Image.LoadFromFile(s);
        if SetImageCaption then
        begin
          FCaption := ExtractFileName(s);
        end;
      except
        Image.Assign(nil);
      end;
    end;
  end;

begin
  BeginUpdate;
  if FindFirst(AFolder,faAnyFile-faDirectory,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);
  EndUpdate;
end;

procedure TAdvSmoothDock.AlignDesktop(Align: TAdvSmoothDockAlignDesktop);
var
  r: TRect;
begin
  if Assigned(frm) then
  begin
    SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
    case Align of
      adBottom:
      begin
        Self.Position := dpBottom;
        frm.Left := r.Left;
        frm.Top := r.Bottom - frm.Height;
        frm.Width := r.Right;
      end;
      adLeft:
      begin
        Self.Position := dpLeft;
        frm.Left := r.Left;
        frm.Top := r.Top;
        frm.Height := r.Bottom;
      end;
      adRight:
      begin
        Self.Position := dpRight;
        frm.Left := r.Right - Width;
        frm.Top := r.Top;
        frm.Height := r.Bottom;
        frm.Width := Width;
      end;
      adTop:
      begin
        Self.Position := dpTop;
        frm.Left := r.Left;
        frm.Top := r.Top;
        frm.Width := r.Right;
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.Animate(Sender: TObject);
var
  d, j, hsW, dW, hsH, dH, Fm: Single;
  dojump, domove, doanimationW, doanimationH: Boolean;
  I: Integer;
  hs: Double;
  f: Double;
  r: TGPREctF;
  w: Double;
  c: TGPPointF;
  b: TAdvSmoothDockBounds;
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(frm) then
  begin
    Inc(FTimeDownOnArrow);
    if FTimeDownOnArrow >= 30 then
    begin
      if FArrowLeftDown then
        DoArrowLeftDown
      else if FArrowRightDown then
        DoArrowRightDown;
    end;

    if FAnimateMove then
    begin
      d := Abs(FCurrentPos - FPosTo) / AnimationFactor;
      fm := FCurrentPos;
      domove := AnimateDouble(fm, FPosTo, d, 1);
      if domove then
      begin
        FCurrentPos := fm;
        Changed;
      end;
    end
    else
    begin
      FPosTo := FCurrentPos;
      FAnimateMove := false;
    end;

    //POPUPMENU
    if FStartPopupTimer then
    begin
      Inc(FPopupTime);
      if (FPopupTime >= 100) and (FPopupClickIndex <> -1) then
      begin
        FStartPopupTimer := false;
        FDoPopup := true;
        with Items[FPopupClickIndex] do
        begin
          if Assigned(PopupMenu) then
          begin
            PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          end;

          FMouseDown := false;
        end;
      end;
    end;
    //
    b := GetBounds;
    for I := b.Start to b.Stop do
    begin
      with Items[i] do
      begin
        if FJump then
        begin
          if FJumpDown then
          begin
            FJMPTO := 0;
            d := Abs(FJMP - FJMPTO) / AnimationFactor;
            if FJMP > 3 then
              d := ((ItemAppearance.FJumpMargin + 5) / AnimationFactor) - d
            else
              FJMP := 0;

            j := FJMP;
            dojump := AnimateDouble(j, FJMPTO, d, 1);
            if dojump then
            begin
              FJMP := Max(0, j);
              Changed;
            end
            else
            begin
              dolastjumpdown := false;
              FJMP := 0;
              FJumpUp := true;
              FJumpDown := false;
            end;
          end;

          if FJumpUp then
          begin
            FJMPTO := ItemAppearance.FJumpMargin;
            d := Abs(FJMP - FJMPTO) / AnimationFactor;
            j := FJMP;
            dojump := AnimateDouble(j, FJMPTO, d, 1);
            if dojump then
            begin
              FJMP := j;
              Changed;
            end
            else
            begin
              dolastjumpup := false;
              FJMP := FJMPTO;
              FJumpUp := false;
              FJumpDown := true;
            end;
          end;
        end
        else if FDoLastJump then
        begin
          j := FJMP;
          d := Abs(FJMP - FJMPTO) / AnimationFactor;
          dojump := AnimateDouble(j, FJMPTO, d, 1);
          if dojump then
          begin
            FJMP := J;
            Changed;
          end
          else
          begin
            FDoLastJump := false;
            FJMP := FJMPTO;
          end;
        end;

        if frm.FMouseEntered and not IsMouseOnArrow(FCurX, FCurY) then
        begin
          if (IsItemBounds(FCurX, FCurY)) and ((FCurX <> FPrevX) or (FCurY <> FPrevY)) then
          begin
            r := GetItemRect;
            case Position of
              dpRight, dpLeft:
              begin
                w := r.Height;
                c.Y := r.Y + (w / 2);
                f := FactorInSinus(FCurX, FCurY, c);
  //              HEIGHT
                hs := f * (ItemAppearance.MaximumImageHeight - ItemAppearance.ImageHeight);
                if hs <= 0 then
                  FHoverSizeToH := 0
                else
                  FHoverSizeToH := hs;

  //              WIDTH
                hs := f * (ItemAppearance.MaximumImageWidth - ItemAppearance.ImageWidth);
                if hs <= 0 then
                  FHoverSizeToW := 0
                else
                  FHoverSizeToW := hs;
              end;
              dpTop, dpBottom:
              begin
                w := r.Width;
                c.X := r.X + (w / 2);
                f := FactorInSinus(FCurX, FCurY, c);
                //HEIGHT
                hs := f * (ItemAppearance.MaximumImageHeight - ItemAppearance.ImageHeight);
                if hs <= 0 then
                  FHoverSizeToH := 0
                else
                  FHoverSizeToH := hs;

                //WIDTH
                hs := f * (ItemAppearance.MaximumImageWidth - ItemAppearance.ImageWidth);
                if hs <= 0 then
                  FHoverSizeToW := 0
                else
                  FHoverSizeToW := hs;
              end;
            end;
            FDoItemAnimation := true;
          end
          else
          begin
            FHoverSizeToW := 0;
            FHoverSizeToH := 0;
            FDoItemAnimation := false;
          end;
        end;

        if FDoItemAnimation then
        begin
          //WIDTH
          dW := Abs(FHoverSizeW - FHoverSizeToW) / AnimationFactor;
          hsW := FHoverSizeW;
          doanimationW := AnimateDouble(hsW, FHoverSizeToW, dW, 1);

          //HEIGHT
          dH := Abs(FHoverSizeH - FHoverSizeToH) / AnimationFactor;
          hsH := FHoverSizeH;
          doanimationH := AnimateDouble(hsH, FHoverSizeToH, dH, 1);


          if doanimationW or doanimationH then
          begin
            FHoverSizeW := hsW;
            FHoverSizeH := hsH;
            Changed;
          end
          else
            FDoItemAnimation := false;
        end;
      end;
    end;

    //dragdropitem needs repaint when dragging AFTER sinus animation on other items
    if FMouseDown and (FDragDropItem <> nil) then
      Changed;
  end;
end;

procedure TAdvSmoothDock.ArrowAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDock.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothDock) then
  begin
    FFill.Assign((Source as TAdvSmoothDock).Fill);
    FTransparent := (Source as TAdvSmoothDock).Transparent;
    FItems.Assign((Source as TAdvSmoothDock).Items);
    FItemAppearance.Assign((Source as TAdvSmoothDock).ItemAppearance);
    FSelectedItemIndex := (Source as TAdvSmoothDock).SelectedItemIndex;
    FPosition := (Source as TAdvSmoothDock).Position;
    FPlatForm3D := (Source as TAdvSmoothDock).PlatForm3D;
    FPlatForm3DColor := (Source as TAdvSmoothDock).PlatForm3DColor;
    FPlatForm3DColorTo := (Source as TAdvSmoothDock).PlatForm3DColorTo;
    FPlatForm3DOpacity := (Source as TAdvSmoothDock).PlatForm3DOpacity;
    FPlatForm3DOpacityTo := (Source as TAdvSmoothDock).PlatForm3DOpacityTo;
    FPlatformFill.Assign((Source as TAdvSmoothDock).PlatformFill);
    FPlatformSize := (Source as TAdvSmoothDock).PlatformSize;
    FArrowAppearance.Assign((Source as TAdvSmoothDock).ArrowAppearance);
    FAnimationFactor := (Source as TAdvSmoothDock).AnimationFactor;
    FOleDragDrop := (Source as TAdvSmoothDock).OleDragDrop;
    FFocus := (Source as TAdvSmoothDock).ShowFocus;
    FAutoSize := (Source as TAdvSmoothDock).AutoSize;
    FBackGroundSize := (Source as TAdvSmoothDock).BackGroundSize;
    FBackGroundAutoSize := (Source as TAdvSmoothDock).BackGroundAutoSize;
    Changed;
  end;
end;

procedure TAdvSmoothDock.DoDrop(X, Y: integer);
var
  AllowItemDelete: Boolean;
begin
  if IsItemBounds(X, Y) then
  begin
    if (FDragDropItem <> nil) and (FInsertDropItem <> nil) then
      FItems.Insert(FInsertDropItem.Index).Assign(FDragDropItem)
    else if (FDragDropItem <> nil) then
      FItems.Add.Assign(FDragDropItem);
  end;

  if (FDragDropItem <> nil) then
  begin
    AllowItemDelete := true;
    if Assigned(FOnItemDragDelete) then
      FOnItemDragDelete(Self, FDragDropItem.Index, AllowItemDelete);

    if not AllowItemDelete then
      FItems.Insert(FDragDropItem.FInternal).Assign(FDragDropItem);

    FDragDropItem.Free;
    FDragDropItem := nil;
  end;
  if (FInsertDropItem <> nil) then
  begin
    FInsertDropItem.Free;
    FInsertDropItem := nil;
  end;
  FHoveredItemIndex := -1;
end;

procedure TAdvSmoothDock.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothDock.CalculateSin;
var
  xp, yp, i: integer;
  o: TPoint;
  rad, iv: Double;
begin
  xp := FItemAppearance.AnimationSpan;
  yp := Height div 2;
  o := Point(-xp div 2,Height div 2);
  rad := -Pi / 2;
  iv := 4.0 * Pi / 175;
  SetLength(FSin, 90);
  for i := 0 to 89 do
  begin
    FSin[i].pt.X := o.x + SaveRound(rad * xp / Pi);
    FSin[i].pt.Y := o.y - SaveRound(sin(rad) * yp);
    FSin[i].factor := sin(rad);
    rad := rad + iv;
  end;
end;

procedure TAdvSmoothDock.Changed;
begin
  if not Assigned(frm) then
    UpdateSize;

  if (csDestroying in ComponentState) then
    Exit;

  if FUpdateCount = 0 then
  begin
    if Assigned(frm) and (frm.handleallocated) then
      frm.invalidate;
    Invalidate;
  end;
end;

procedure TAdvSmoothDock.CMHintShow(var Message: TMessage);
var
  hint: String;
begin
  if FHoveredItemIndex <> -1 then
  begin
    with TCMHintShow(Message).HintInfo^ do
    begin
      hint := Items[FHoveredItemIndex].Hint;
      if Assigned(OnItemHint) then
        OnItemHint(Self, FHoveredItemIndex, hint);
      HintStr := hint;
      ReshowTimeout := 0;
    end;
  end;
end;

procedure TAdvSmoothDock.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(frm) then
    frm.CMMouseLeave(Message);

  if not Visible then
  begin
    FormHookDone;
    if Assigned(frm) then
    begin
      frm.Free;
      frm := nil;
    end;
  end
  else
    ShowForm;
end;

constructor TAdvSmoothDock.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited Create(AOwner);
  FSel := -1;
  FDragDropItemInteraction := True;
  FStayOnTop := True;
  FCreateNew := true;
  FFocus := true;
  FAnimationFactor := 2;
  DoubleBuffered := true;
  Width := 600;
  Height := 200;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FItemAppearance := TAdvSmoothDockItemAppearance.Create(Self);
  FItemAppearance.OnChange := ItemAppearanceChanged;
  FArrowAppearance := TAdvSmoothDockArrowAppearance.Create(Self);
  FArrowAppearance.OnChange := ArrowAppearanceChanged;
  FTransparent := true;
  FSelectedItemIndex := -1;
  FHoveredItemIndex := -1;
  FPosition := dpBottom;
  FPlatForm3D := true;
  FPlatformFill := TGDIPFill.Create;
  FPlatformFill.OnChange := FillChanged;
  FPlatformSize := 60;
  FDragDropItem := nil;
  FItems := TAdvSmoothDockItems.Create(Self);
  FItems.OnChange := ItemsChanged;
  FSeparatorImage := TGPBitmap.Create;
  FSeparatorMemoryStream := TMemoryStream.Create;
  FOleDropTargetAssigned := false;
  FOleDragDrop := false;
  FPlatForm3DColor := clWhite;
  FPlatForm3DColorTo := clWhite;
  FPlatForm3DOpacity := 30;
  FPlatForm3DOpacityTo := 100;
  FDockOnDeskTop := false;
  CalculateSin;
  FAnimate := TTimer.Create(Self);
  FAnimate.OnTimer := Animate;
  FAnimate.Interval := 10;
  FAnimate.Enabled := true;
  FAutoSize := true;
  FBackGroundSize := 200;
  FBackGroundAutoSize := true;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
  ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvSmoothDock.CreateWnd;
var
  i: integer;
begin
  inherited;
  ItemAppearance.SeparatorChanged(Self);
  for I := 0 to Items.Count - 1 do
    Items[i].UpdateReflection;

  if FConstructed then
    Exit;

  FConstructed := true;
end;

procedure TAdvSmoothDock.DoArrowLeftDown;
var
  fi: Integer;
begin
  fi := GetFirstVisibleItem;
  FAnimateMove := true;
  case Position of
    dpRight, dpLeft:
    begin
      if (fi >= 0) and (fi <= Items.Count - 1) and (Items[fi].GetItemRect.Y <= InsideRect.Top) then
      begin
        FPosTo := FPosTo - ItemAppearance.ImageHeight;
        FanimateMove := true;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end;
    dpTop, dpbottom:
    begin
      if (fi >= 0) and (fi <= Items.Count - 1) and (Items[fi].GetItemRect.X <= InsideRect.Left) then
      begin
        FPosTo := FPosTo - ItemAppearance.ImageWidth;
        FanimateMove := true;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.DoArrowRightDown;
var
  li: Integer;
begin
  li := GetLastVisibleItem;
  case Position of
    dpRight, dpLeft:
    begin
      if (li >= 0) and (li <= Items.Count - 1) and (Items[li].GetItemRect.Y + Items[li].GetItemRect.Height >= InsideRect.Bottom) then
      begin
        FPosTo := FPosTo + ItemAppearance.ImageHeight;
        FAnimateMove := true;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end;
    dpTop, dpbottom:
    begin
      if (li >= 0) and (li <= Items.Count - 1) and (Items[li].GetItemRect.X + Items[li].GetItemRect.Width >= InsideRect.Right) then
      begin
        FPosTo := FPosTo + ItemAppearance.ImageWidth;
        FAnimateMove := true;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.DoDblClick;
var
  item: integer;
  pt: TPoint;
begin
  inherited;
  FDblClick := true;
  FMouseDown := false;
  if Assigned(frm) then
    pt := frm.ScreenToClient(Mouse.CursorPos);

  if IsMouseOnArrow(Pt.X, pt.Y) then
    Exit;

  item := XYToItem(pt.X, pt.Y);
  if item <> -1 then
  begin
    if Assigned(FOnItemDblClick) then
      FOnItemDblClick(Self, item);
  end;
end;

destructor TAdvSmoothDock.Destroy;
begin
  FormHookDone;
  if Assigned(frm) and not (csDestroying in Application.ComponentState) then
  begin
    frm.Free;
    frm := nil;
  end;

  if FInsertDropItem <> nil then
  begin
    FInsertDropItem.Free;
    FInsertDropItem := nil;
  end;
  if FDragDropItem <> nil then
  begin
    FDragDropItem.Free;
    FDragDropItem := nil;
  end;

  FArrowAppearance.Free;
  FSeparatorImage.Free;
  FSeparatorMemoryStream.Free;
  FFill.Free;
  FItems.Free;
  FItemAppearance.Free;
  FAnimate.Free;
  FPlatformFill.Free;
  inherited;
end;

procedure TAdvSmoothDock.DoDrag(X, Y: integer; Copy: Boolean);
var
  item: integer;
  allowinsert: Boolean;
  Allowdrag: boolean;
begin
  if not DragDropItemInteraction then
    Exit;

  if FCreateNew then
  begin
    if (FDragDropItem = nil) then
    begin
      item := XYToItem(X, Y, true);
      if item <> -1 then
      begin
        Allowdrag := true;
        if Assigned(OnItemStartDrag) then
          OnItemStartDrag(Self, Items[item], allowdrag);

        if Allowdrag then
        begin
          FDragDropItem := TAdvSmoothDockItem.Create(FItems);
          FDragDropItem.Assign(FItems[item]);
          FDragDropItem.FInternal := item;
          FStartPopupTimer := false;
          if not Copy then
            FItems[item].Free;
        end;
      end;
    end;
    item := XYToItem(X, Y, true, true);
    if item <> -1 then
    begin
      if (FDragDropItem <> nil) then
      begin
        allowinsert := true;
        if Assigned(FOnDragInsert) then
        begin
          case Position of
            dpRight, dpLeft:
            begin
              if FDragY < Items[item].GetItemRect.Y + (Items[item].GetItemRect.Height / 2) then
                FOnDragInsert(Self, item - 1, item, allowinsert)
              else
                FOnDragInsert(Self, item, item + 1, allowinsert);
            end;
            dpTop, dpbottom:
            begin
              if FDragX < Items[item].GetItemRect.X + (Items[item].GetItemRect.Width / 2) then
                FOnDragInsert(Self, item - 1, item, allowinsert)
              else
                FOnDragInsert(Self, item, item + 1, allowinsert);
            end;
          end;
        end;
        if allowinsert then
        begin
          if FInsertDropItem = nil then
          begin
            FInsertDropItem := Items.Insert(item);
            FInsertDropItem.FInsertItem := true;
          end
          else
          begin
            if item <> FInsertDropItem.Index then
            begin
              FInsertDropItem.Free;
              FInsertDropItem := Items.Insert(item);
              FInsertDropItem.FInsertItem := true;
            end;
          end;
        end;
      end;
    end
    else if (FInsertDropItem <> nil) and not IsItemBounds(X, Y) then
    begin
      FInsertDropItem.Free;
      FInsertDropItem := nil;
    end;
  end
  else
  begin

  end;
end;

procedure TAdvSmoothDock.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothDock.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothDock.DoItemClick(ItemIndex: Integer);
//var
//  msg: TMessage;
begin
  if Assigned(OnItemClick) then
    OnItemClick(Self, ItemIndex);

//  if Assigned(frm) then
//    frm.CMMouseLeave(msg);
end;

procedure TAdvSmoothDock.DoDragDrop(Source: TObject; X, Y: Integer);
begin
  FMouseDown := false;
  if Assigned(FOnDragDrop) then
  begin
    if DragDropItemInteraction then
    begin
      if Assigned(FDragDropItem) then
        FOnDragDrop(Self, Source, FDragDropItem, X, Y)
      else if Assigned(FHoveredDropItem) then
        FOnDragDrop(Self, Source, FHoveredDropItem, X, Y);
    end
    else
      FOnDragDrop(Self, Source, nil, X, Y);
  end;

  DoDrop(X, Y);
  inherited;
end;

procedure TAdvSmoothDock.DoDragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  h: integer;
begin
  inherited;

  if not DragDropItemInteraction then
  begin
    if Assigned(FOnDragOver) then
      FOnDragOver(Self, Source, FDragDropItem, X, Y, State, Accept);
    Exit;
  end;

  FDragX := X;
  FDragY := Y;
  FMouseDown := true;

  FHoveredDropItem := nil;
  h := Self.XYToItem(X, Y, true, false);
  if (h >= 0) and (h <= Items.Count - 1) then
    FHoveredDropItem := Items[h];

  if Assigned(OnBeforeDragOver) then
    OnBeforeDragOver(Self, Source, FHoveredDropItem, FCreateNew);

  if FCreateNew then
  begin
    if FDragDropItem = nil then
      FDragDropItem := TAdvSmoothDockItem.Create(FItems)
  end
  else
  begin
    if FDragDropItem <> nil then
    begin
      FDragDropItem.Free;
      FDragDropItem := nil;
    end;
  end;

  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source, FDragDropItem, X, Y, State, Accept);

  if Accept then
  begin
    frm.FMouseEntered := true;
    DoDrag(X, Y, false);
    FPrevX := FCurX;
    FPrevY := FCurY;
    FCurX := X;
    FCurY := Y;
  end
  else if FDragDropItem <> nil then
  begin
    FMouseDown := false;
    FDragDropItem.Free;
    FDragDropItem := nil;
  end;
end;

procedure TAdvSmoothDock.DrawArrows(g: TGPGraphics);
var
  doArrow: Boolean;
  c, bc: TColor;
  p: TGPPen;
  b: TGPBrush;
  path: TGPGraphicsPath;
  rl, rr: TGPRectF;
  x, y: Single;
  fi, li: Integer;
begin
  with ArrowAppearance do
  begin
    fi := GetFirstVisibleItem;
    li := GetLastVisibleItem;
    doArrow := false;
    case Position of
      dpRight, dpLeft: doArrow := Visible and (GetItemsRectangle.Height > (InsideRect.Bottom - InsideRect.Top) + ItemAppearance.ImageHeight);
      dpBottom, dpTop: doArrow := Visible and (GetItemsRectangle.Width > (InsideRect.Right - InsideRect.Left) + ItemAppearance.ImageWidth);
    end;
    if doArrow then
    begin
      rl := GetArrowLeft;
      x := rl.X;
      y := rl.Y;
      //left
      c := Color;
      if FArrowLeftDown then
        c := Darker(c, 30)
      else if FArrowLeftHover then
        c := Lighter(c, 30);

      path := TGPGraphicsPath.Create;
      case Position of
        dpRight, dpLeft:
        begin
          path.AddArc(rl, 0, 180);
          path.CloseFigure;
        end;
        dpBottom, dpTop:
        begin
          path.AddArc(rl, 270, 180);
          path.CloseFigure;
        end;
      end;

      b := TGPSolidBrush.Create(MakeColor(Opacity, c));
      g.FillPath(b, path);
      b.free;
      b := nil;
      case Position of
        dpRight, dpLeft:b := TGPLinearGradientBrush.Create(MakeRect(GetArrowLeft.X, GetArrowLeft.Y, GetArrowLeft.Height, GetArrowLeft.Width / 2), MakeColor(100, clWhite), Makecolor(0, clWhite), LinearGradientModeHorizontal);
        dpBottom, dpTop: b := TGPLinearGradientBrush.Create(MakeRect(GetArrowLeft.X, GetArrowLeft.Y, GetArrowLeft.Height / 2, GetArrowLeft.Width), MakeColor(100, clWhite), Makecolor(0, clWhite), LinearGradientModeVertical);
      end;
      g.FillPath(b, path);
      b.Free;
      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;
      path.Free;

      path := TGPGraphicsPath.Create;
      case Position of
        dpRight, dpLeft:
        begin
          path.AddLine(MakePoint(x + rl.Width / 2, y + rl.Height * 2 / 3), MakePoint(x + rl.Width / 3, y + rl.Height * 4 / 5));
          path.AddLine(MakePoint(x + rl.Width / 2, y + rl.Height * 2 / 3), MakePoint(x + rl.Width * 2 / 3, y + rl.Height * 4 / 5));
        end;
        dpBottom, dpTop:
        begin
          path.AddLine(MakePoint(x + rl.Width * 2 / 3, y + rl.Height / 2), MakePoint(x + rl.Width * 4 / 5, y + rl.Height / 3));
          path.AddLine(MakePoint(x + rl.Width * 2 / 3, y + rl.Height / 2), MakePoint(x + rl.Width * 4 / 5, y + rl.Height * 2 / 3));
        end;
      end;

      bc := BorderColor;
      case Position of
        dpRight, dpLeft:
        begin
          if (fi >= 0) and (fi <= Items.Count - 1) and not (Items[fi].GetItemRect.Y <= InsideRect.Top) then
            bc := clGray;
        end;
        dpTop, dpbottom:
        begin
          if (fi >= 0) and (fi <= Items.Count - 1) and not (Items[fi].GetItemRect.X <= InsideRect.Left) then
            bc := clGray;
        end;
      end;

      p := TGPPen.Create(MakeColor(Opacity, bc), 2);
      g.DrawPath(p, path);
      p.Free;
      path.Free;

      //right
      rr := GetArrowRight;
      x := rr.X;
      y := rr.Y;
      c := Color;
      if FArrowRightDown then
        c := Darker(c, 30)
      else if FArrowRightHover then
        c := Lighter(c, 30);

      path := TGPGraphicsPath.Create;
      case Position of
        dpRight, dpLeft:
        begin
          path.AddArc(rr, 180, 180);
          path.CloseFigure;
        end;
        dpBottom, dpTop:
        begin
          path.AddArc(rr, 90, 180);
          path.CloseFigure;
        end;
      end;


      b := TGPSolidBrush.Create(MakeColor(Opacity, c));
      g.FillPath(b, path);
      b.free;
      b := nil;
      case Position of
        dpRight, dpLeft:b := TGPLinearGradientBrush.Create(MakeRect(GetArrowRight.X, GetArrowRight.Y, GetArrowRight.Height, GetArrowRight.Width / 2), MakeColor(100, clWhite), Makecolor(0, clWhite), LinearGradientModeHorizontal);
        dpBottom, dpTop: b := TGPLinearGradientBrush.Create(MakeRect(GetArrowRight.X, GetArrowRight.Y, GetArrowRight.Height / 2, GetArrowRight.Width), MakeColor(100, clWhite), Makecolor(0, clWhite), LinearGradientModeVertical);
      end;
      g.FillPath(b, path);
      b.Free;
      p := TGPPen.Create(MakeColor(Opacity, BorderColor), 1);
      g.DrawPath(p, path);
      p.Free;
      path.Free;

      path := TGPGraphicsPath.Create;
      case Position of
        dpRight, dpLeft:
        begin
          path.AddLine(MakePoint(x + rr.Width / 2, y + rr.Height / 3), MakePoint(x + rr.Width / 3, y + rr.Height / 5));
          path.AddLine(MakePoint(x + rr.Width / 2, y + rr.Height / 3), MakePoint(x + rr.Width * 2 / 3, y + rr.Height / 5));
        end;
        dpBottom, dpTop:
        begin
          path.AddLine(MakePoint(x + rr.Width / 3, y + rl.Height / 2), MakePoint(x + rl.Width / 5, y + rl.Height / 3));
          path.AddLine(MakePoint(x + rr.Width / 3, y + rl.Height / 2), MakePoint(x + rl.Width / 5, y + rl.Height * 2 / 3));
        end;
      end;

      bc := BorderColor;
      case Position of
        dpRight, dpLeft:
        begin
          if (li >= 0) and (li <= Items.Count - 1) and not (Items[li].GetItemRect.Y + Items[li].GetItemRect.Height >= InsideRect.Bottom) then
            bc := clGray;
        end;
        dpTop, dpbottom:
        begin
          if (li >= 0) and (li <= Items.Count - 1) and not (Items[li].GetItemRect.X + Items[li].GetItemRect.Width >= InsideRect.Right) then
            bc := clGray;
        end;
      end;

      p := TGPPen.Create(MakeColor(Opacity, bc), 2);
      g.DrawPath(p, path);
      p.Free;
      path.Free;
    end;
  end;
end;

procedure TAdvSmoothDock.DrawBackGround(g: TGPGraphics; Control: Boolean);
var
  b: TGPSolidBrush;
begin
  //Transparent layer
  b := TGPSolidBrush.Create(MakeColor(1, clWhite));
  //Transparent layer
  g.FillRectangle(b, GetItemsMinimumRectangle);
  b.Free;
  if not Transparent and Control then
  begin
    if BackGroundAutoSize then
      FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1))
    else
    begin
      case Position of
        dpLeft: FFill.Fill(g, MakeRect(0, 0, BackGroundSize, Height - 1));
        dpRight: FFill.Fill(g, MakeRect(Width - 1 - BackGroundSize, 0, BackGroundSize, Height - 1));
        dpTop: FFill.Fill(g, MakeRect(0, 0, Width - 1, BackGroundSize));
        dpBottom: FFill.Fill(g, MakeRect(0, Height - 1 - BackGroundSize, Width - 1, BackGroundSize));
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.DrawCaption(g: TGPGraphics; ItemIndex: Integer = -1);
var
  item: integer;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  f: TGPFont;
  b: TGPSolidBrush;
  sizer: TGPRectF;
  r: TGPRectF;
  w: Double;
  h: Double;
begin
  if ItemIndex = -1 then
    item := XYToItem(FHintX, FHintY)
  else
    item := ItemIndex;

  if item <> -1 then
  begin
    with Items[item] do
    begin
      if not ShowCaption or Separator or (caption = '') then
        Exit;

      ff := TGPFontFamily.Create(ItemAppearance.CaptionFont.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in ItemAppearance.CaptionFont.Style) then
        fs := fs + 1;
      if (fsItalic in ItemAppearance.CaptionFont.Style) then
        fs := fs + 2;
      if (fsUnderline in ItemAppearance.CaptionFont.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create;
      if not WordWrapping then
        sf.SetFormatFlags(StringFormatFlagsNoWrap);
      f := TGPFont.Create(ff, ItemAppearance.CaptionFont.Size, fs, UnitPoint);
      g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      g.MeasureString(Caption, length(Caption), f, MakeRect(GetItemRect.X, GetItemRect.Y, InsideRect.Right - InsideRect.Left - GetItemRect.X, InsideRect.Bottom - InsideRect.Top - GetItemRect.Y), sf, sizer);

      sizer.Width := sizer.Width + 6;
      sizer.Height := sizer.Height + 4;

      r := MakeRect(sizer.X + (GetItemRect.Width - sizer.Width) / 2, sizer.Y - sizer.Height, sizer.Width, sizer.Height);
      w := r.X + r.Width;
      h := r.Y + r.Height;
      if (w > InsideRect.Right) and ((Position = dpRight) or (Position = dpBottom) or (Position = dpTop)) then
        r.X := r.X - Abs(Insiderect.Right - w)
      else if (r.X < InsideRect.Left) and ((Position = dpLeft) or (Position = dpBottom) or (Position = dpTop)) then
        r.X := r.X + Abs(InsideRect.Left - r.X);

      if (h > InsideRect.Bottom) and ((Position = dpBottom) or (Position = dpRight) or (Position = dpLeft)) then
        r.Y := r.Y - Abs(Insiderect.Bottom - h)
      else if (r.Y < InsideRect.Top) and ((Position = dpTop) or (Position = dpRight) or (Position = dpLeft)) then
        r.Y := r.Y + Abs(Insiderect.Top - r.Y);

      FItemAppearance.CaptionFill.Fill(g, r);

      b := TGPSolidBrush.Create(MakeColor(255, ItemAppearance.CaptionFont.Color));
      g.DrawString(Caption, Length(Caption), f, MakeRect(r.X + 2, r.Y + 2, r.Width, r.Height), sf, b);

      ff.Free;
      sf.Free;
      f.free;
      b.Free;
    end;
  end;
end;

procedure TAdvSmoothDock.DrawIndicator(g: TGPGraphics; ItemIndex: integer);
var
  r: TGPRectF;
  s: Integer;
  off: integer;
begin
  if (ItemIndex >= 0) and (Itemindex <= Items.Count - 1) and ItemAppearance.ShowSelection then
  begin
    with Items[ItemIndex] do
    begin
      r := GetItemRect;
      s := ItemAppearance.SelectionSize;
      off := ItemAppearance.SelectionOffset;
      case Position of
        dpLeft: ItemAppearance.SelectionFill.Fill(g, MakeRect(InsideRect.Left + off, r.Y + (r.Height - s) / 2, s, s));
        dpRight: ItemAppearance.SelectionFill.Fill(g, MakeRect(InsideRect.Right - off - s, r.Y + (r.Height - s) / 2, s, s));
        dpTop: ItemAppearance.SelectionFill.Fill(g, MakeRect(r.X + (r.Width - s) / 2, Insiderect.Top + off, s, s));
        dpBottom: ItemAppearance.SelectionFill.Fill(g, MakeRect(r.X + (r.Width - s) / 2, Insiderect.Bottom - off - s, s, s));
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.DrawItems(g: TGPGraphics);
var
  i: integer;
  r: TGPRectF;
  x, y: integer;
  xs, ys: Double;
  check: Boolean;
  w, h: integer;
  f, foc: TGDIPFill;
  b: TAdvSmoothDockBounds;
  img: TAdvGDIPPicture;
begin
  with ItemAppearance do
  begin
    foc := TGDIPFill.Create;
    foc.Color := clNone;
    //Draw normal items
    b := GetBounds;
    for I := b.Start to b.Stop do
    begin
      with Items[i] do
      begin
        if (FDragDropItem <> nil) and (FInsertDropItem <> nil) then
          check := (Items[i] <> FDragDropItem) and not FInsertItem
        else if (FDragDropItem <> nil) then
          check := (Items[i] <> FdragDropItem)
        else
          check := true;

        if check and Visible then
        begin
          r := GetItemRect;
          if ItemBackGround and not Separator then
          begin
            if Enabled then
            begin
              if FSelectedItemIndex = Index then
                f := FSelectedFill
              else if FHoveredItemIndex = index then
                f := FHoverFill
              else
                f := FFill;
            end
            else
              f := FDisabledFill;

            f.Fill(g, r);
          end;

          foc.Focus := FFocused and ShowFocus and TabStop and (FSel = index);
          foc.Fill(g, r);

          if not Separator then
          begin
            if ProgressPosition > ProgressMinimum then
            begin
              if (ProgressMaximum - ProgressMinimum > 0) and (ProgressPosition >= ProgressMinimum) and (ProgressPosition <= ProgressMaximum) then
                FProgressFill.Fill(g, MakeRect(r.X, r.Y, r.Width / (ProgressMaximum - ProgressMinimum) * ProgressPosition,r.Height));
            end;
          end;

          if not FDisabledImage.Empty and not Enabled then
            img := FDisabledImage
          else
            img := FImage;

          if Assigned(img) then
          begin
            img.GetImageSizes;
            GetAspectSize(w, h, img.Width, img.Height, r.Width, r.Height, Separator);
          end;

          r.X := r.X + (r.Width - w) / 2;
          r.Width := w;
          r.Y := r.Y + (r.Height - h) / 2;
          r.Height := h;

          if Assigned(img) and not img.Empty then
          begin
            img.GDIPDraw(g, Bounds(SaveRound(r.X), SaveRound(R.Y), SaveRound(R.Width), SaveRound(R.Height)));

            if Assigned(FReflectionImage) and (ReflectionSize > 0) then
            begin
              case FOwner.Position of
                dpLeft:
                begin
                  x := SaveRound(r.X - r.Width - (FJMP * 2));
                  y := SaveRound(R.Y);
                  g.DrawImageRect(FReflectionImage, x - FReflectionSpacing, y, SaveRound(r.Width), SaveRound(r.Height))
                end;
                dpRight:
                begin
                  x := SaveRound(r.X + r.Width + (FJMP * 2));
                  y := SaveRound(R.Y);
                  g.DrawImageRect(FReflectionImage, x + FReflectionSpacing, y, SaveRound(r.Width), SaveRound(r.Height))
                end;
                dpTop:
                begin
                  x := SaveRound(r.X);
                  y := SaveRound(R.Y - r.Height - (FJMP * 2));
                  g.DrawImageRect(FReflectionImage, x, y - FReflectionSpacing, SaveRound(r.Width), SaveRound(r.Height))
                end;
                dpBottom:
                begin
                  x := SaveRound(r.X);
                  y := SaveRound(R.Y + r.Height + (FJMP * 2));
                  g.DrawImageRect(FReflectionImage, x, y + FReflectionSpacing, SaveRound(r.Width), SaveRound(r.Height))
                end;
              end;
            end;
          end;
          if not Separator then
          begin
            with StatusIndicator do
            begin
              if Visible then
              begin
                xs := 0;
                ys := 0;
                Appearance.CalculateSize(g, Caption);
                case Position of
                  dpRight, dpLeft, dpTop, dpBottom:
                  begin
                    xs := r.X + R.Width + OffsetLeft - Appearance.GetWidth;
                    ys := r.Y - Appearance.GetHeight div 2 + OffsetTop;
                  end;
                end;
                Appearance.Draw(g, SaveRound(xs), SaveRound(ys), 0, 0, true,Caption);
              end;
            end;
          end;
          if ItemAppearance.PermanentCaption then
            DrawCaption(g, i);
        end;
      end;
    end;
    //
    //Draw DragDropItem
    if FDragDropItem <> nil then
    begin
      with FDragDropItem do
      begin
        if not Visible then
          Exit;

          if not FDisabledImage.Empty and not Enabled then
            img := FDisabledImage
          else
            img := FImage;

        r := GetNormalRect;
        if Assigned(img) then
        begin
          img.GetImageSizes;
          GetAspectSize(w, h, img.Width, img.Height, r.Width, r.Height, Separator);
        end;

        r.X := r.X + (r.Width - w) / 2;
        r.Width := w;
        r.Y := r.Y + (r.Height - h) / 2;
        r.Height := h;
        r.X := FDragX;
        r.Y := FDragY;

        if Separator then
        begin
          case Position of
            dpLeft, dpRight: r.Height := SeparatorSize;
            dpTop, dpBottom: r.Width := SeparatorSize;
          end;
        end;

        if ItemBackGround and not Separator then
        begin
          if Enabled then
          begin
            if FSelectedItemIndex = Index then
              FSelectedFill.Fill(g, MakeRect(r.X - r.Width / 2, r.Y - R.Height / 2, R.Width, R.Height))
            else if FHoveredItemIndex = index then
              FHoverFill.Fill(g, MakeRect(r.X - r.Width / 2, r.Y - R.Height / 2, R.Width, R.Height))
            else
              FFill.Fill(g, MakeRect(r.X - r.Width / 2, r.Y - R.Height / 2, R.Width, R.Height));
          end
          else
            FDisabledFill.Fill(g, MakeRect(r.X - r.Width / 2, r.Y - R.Height / 2, R.Width, R.Height));
        end;

        if Assigned(img) and not img.Empty then
          img.GDIPDraw(g, Bounds(SaveRound(r.X - r.Width / 2), SaveRound(r.Y - R.Height / 2), SaveRound(R.Width), SaveRound(R.Height)));
      end;
    end;
    //
    foc.free;
  end;
end;

procedure TAdvSmoothDock.DrawPlatForm(g: TGPGraphics);
var
  gp: TGPGraphicsPath;
  x, y, w, h: single;
  b: TGPBrush;
  p: TGPPen;
begin
  h := PlatformSize;
  w := h;
  case Position of
    dpLeft:
    begin
      if PlatForm3D then
      begin
        gp := TGPGraphicsPath.Create;
        h := GetPlatFormRectangle.Height;
        x := GetPlatFormRectangle.X;
        y := GetPlatFormRectangle.Y;
        w := w + 12;
        gp.AddLine(MakePoint(x + 12, y), MakePoint(x + w, y + 30));
        gp.AddLine(MakePoint(x + w, y + 30), MakePoint(x + w, y + h - 30));
        gp.AddLine(MakePoint(x + w, y + h - 30), MakePoint(x + 12, y + h));
        gp.CloseFigure;

        b := TGPLinearGradientBrush.Create(MakeRect(x, y, w, h), MakeColor(PlatForm3DOpacity, PlatForm3DColor), MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo), 180);
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillPath(b, gp);
        g.DrawPath(p, gp);
        p.Free;
        b.Free;

        b := TGPSolidBrush.Create(MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo));
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillRectangle(b, MakeRect(x, y, 12, h));
        g.DrawRectangle(p, MakeRect(x, y, 12, h));
        b.Free;
        p.Free;

        gp.Free;
      end
      else
        FPlatformFill.Fill(g, MakeRect(InsideRect.Left, InsideRect.Top, h, InsideRect.Bottom));
    end;
    dpRight:
    begin
      if PlatForm3D then
      begin
        gp := TGPGraphicsPath.Create;
        h := GetPlatFormRectangle.Height;
        x := GetPlatFormRectangle.X + GetPlatFormRectangle.Width;
        y := GetPlatFormRectangle.Y;
        w := w + 12;
        gp.AddLine(MakePoint(x - 12, y), MakePoint(x - w, y + 30));
        gp.AddLine(MakePoint(x - w, y + 30), MakePoint(x - w, y + h - 30));
        gp.AddLine(MakePoint(x - w, y + h - 30), MakePoint(x - 12, y + h));
        gp.CloseFigure;

        b := TGPLinearGradientBrush.Create(MakeRect(x, y, w, h), MakeColor(PlatForm3DOpacity, PlatForm3DColor), MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo), 0);
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillPath(b, gp);
        g.DrawPath(p, gp);
        p.Free;
        b.Free;

        b := TGPSolidBrush.Create(MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo));
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillRectangle(b, MakeRect(x - 12, y, 12, h));
        g.DrawRectangle(p, MakeRect(x - 12, y, 12, h));
        b.Free;
        p.Free;

        gp.Free;
      end
      else
        FPlatformFill.Fill(g, MakeRect(InsideRect.Right - h, InsideRect.Top, h, InsideRect.Bottom));
    end;
    dpTop:
    begin
      if PlatForm3D then
      begin
        gp := TGPGraphicsPath.Create;
        w := GetPlatFormRectangle.Width;
        x := GetPlatFormRectangle.X;
        y := InsideRect.Top + 12;
        gp.AddLine(MakePoint(x, y), MakePoint(x + 30, y + h));
        gp.AddLine(MakePoint(x + 30, y + h), MakePoint(x + w - 30, y + h));
        gp.AddLine(MakePoint(x + w - 30, y + h), MakePoint(x + w, y));
        gp.CloseFigure;

        b := TGPLinearGradientBrush.Create(MakeRect(x, y, w, h), MakeColor(PlatForm3DOpacity, PlatForm3DColor), MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo), -90);
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillPath(b, gp);
        g.DrawPath(p, gp);
        p.Free;
        b.Free;

        b := TGPSolidBrush.Create(MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo));
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillRectangle(b, MakeRect(x, y - 12, w, 12));
        g.DrawRectangle(p, MakeRect(x, y - 12, w, 12));
        b.Free;
        p.Free;

        gp.Free;
      end
      else
        FPlatformFill.Fill(g, MakeRect(InsideRect.Left, InsideRect.Top, InsideRect.Right, h));
    end;
    dpBottom:
    begin
      if PlatForm3D then
      begin
        gp := TGPGraphicsPath.Create;
        w := GetPlatFormRectangle.Width;
        x := GetPlatFormRectangle.X;
        y := InsideRect.Bottom - 12;
        gp.AddLine(MakePoint(x, y), MakePoint(x + 30, y - h));
        gp.AddLine(MakePoint(x + 30, y - h), MakePoint(x + w - 30, y - h));
        gp.AddLine(MakePoint(x + w - 30, y - h), MakePoint(x + w, y));
        gp.CloseFigure;

        b := TGPLinearGradientBrush.Create(MakeRect(x, y, w, h), MakeColor(PlatForm3DOpacity, PlatForm3DColor), MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo), 90);
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillPath(b, gp);
        g.DrawPath(p, gp);
        p.Free;
        b.Free;

        b := TGPSolidBrush.Create(MakeColor(PlatForm3DOpacityTo, PlatForm3DColorTo));
        p := TGPPen.Create(MakeColor(80, PlatForm3DColorTo));
        g.FillRectangle(b, MakeRect(x, y, w, 12));
        g.DrawRectangle(p, MakeRect(x, y, w, 12));
        b.Free;
        p.Free;

        gp.Free;
      end
      else
        FPlatformFill.Fill(g, MakeRect(InsideRect.Left, InsideRect.Bottom - h, InsideRect.Right, h));
    end;
  end;
end;

procedure TAdvSmoothDock.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    Changed;
    if Assigned(frm) then
      frm.UpdateWindow;
  end;
end;

function TAdvSmoothDock.FactorInSinus(X, Y: integer; pt: TGPPointF): Double;
var
  I: Integer;
  ch: Boolean;
begin
  result := 0;
  for I := 0 to Length(FSin) - 2 do
  begin
    ch := false;
    case Position of
      dpLeft, dpRight: ch := (pt.Y >= FSin[i].pt.X + Y) and (pt.Y <= FSin[i + 1].pt.X + Y);
      dpTop, dpBottom: ch := (pt.X >= FSin[i].pt.X + X) and (pt.X <= FSin[i + 1].pt.X + X);
    end;
    if ch then
    begin
      result := FSin[i].factor;
      break;
    end;
  end;
end;

procedure TAdvSmoothDock.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDock.FormHookDone;
begin
  if Assigned(frm) then
    frm.FormHookDone;
end;

procedure TAdvSmoothDock.FormHookInit;
begin
  if Assigned(frm) then
    frm.FormHookInit;
end;

function TAdvSmoothDock.GetActive: Boolean;
var
  pForm: TForm;
  b: Boolean;
begin
  pForm := TForm(GetParentForm(Self));
  b := GetActiveWindow = pForm.Handle;
  Result := Assigned(frm) and (b or DockOnDeskTop);
end;

function TAdvSmoothDock.GetArrowLeft: TGPRectF;
var
  doArrow: Boolean;
begin
  doarrow := false;
  case Position of
    dpRight, dpLeft: doArrow := Visible and (GetItemsRectangle.Height > (InsideRect.Bottom - InsideRect.Top) + ItemAppearance.ImageHeight);
    dpBottom, dpTop: doArrow := Visible and (GetItemsRectangle.Width > (InsideRect.Right - InsideRect.Left) + ItemAppearance.ImageWidth);
  end;

  if Doarrow then
  begin
    case Position of
      dpLeft:Result := MakeRect(InsideRect.Left + (GetMinimumHeight - ArrowAppearance.Size) / 2, InsideRect.Top - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpRight: Result := MakeRect(InsideRect.Right - GetMinimumHeight / 2 - ArrowAppearance.Size / 2, InsideRect.Top - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpTop: Result := MakeRect(InsideRect.Left - ArrowAppearance.Size / 2, InsideRect.Top + (GetMinimumHeight - ArrowAppearance.Size) / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpBottom: Result := MakeRect(InsideRect.Left - ArrowAppearance.Size / 2, InsideRect.Bottom - GetMinimumHeight / 2 - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
    end;
  end
  else
    Result := MakeRect(0, 0, 0, 0);
end;

function TAdvSmoothDock.GetArrowRight: TGPRectF;
var
  doArrow: Boolean;
begin
  doarrow := false;
  case Position of
    dpRight, dpLeft: doArrow := Visible and (GetItemsRectangle.Height > (InsideRect.Bottom - InsideRect.Top) + ItemAppearance.ImageHeight);
    dpBottom, dpTop: doArrow := Visible and (GetItemsRectangle.Width > (InsideRect.Right - InsideRect.Left) + ItemAppearance.ImageWidth);
  end;

  if Doarrow then
  begin
    case Position of
      dpLeft:Result := MakeRect(InsideRect.Left + (GetMinimumHeight - ArrowAppearance.Size) / 2, InsideRect.Bottom - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpRight: Result := MakeRect(InsideRect.Right - GetMinimumHeight / 2 - ArrowAppearance.Size / 2, InsideRect.Bottom - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpTop:Result := MakeRect(InsideRect.Right - ArrowAppearance.Size / 2 , InsideRect.Top + (GetMinimumHeight - ArrowAppearance.Size) / 2, ArrowAppearance.Size, ArrowAppearance.Size);
      dpBottom: Result := MakeRect(InsideRect.Right - ArrowAppearance.Size / 2, InsideRect.Bottom  - GetMinimumHeight / 2 - ArrowAppearance.Size / 2, ArrowAppearance.Size, ArrowAppearance.Size);
    end;
  end
  else
    Result := MakeRect(0, 0, 0, 0);
end;

procedure TAdvSmoothDock.GetAspectSize(var w, h: integer; ow, oh, nw,
  nh: double; Separator: Boolean);
begin
  if ItemAppearance.AspectRatio or Separator then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) then
      begin
        w := SaveRound(ow);
        h := SaveRound(oh);
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := SaveRound(nh);
          w := MulDiv(SaveRound(nh), SaveRound(ow), SaveRound(oh));
        end
        else
        begin
          w := SaveRound(nw);
          h := MulDiv(SaveRound(nw), SaveRound(oh), SaveRound(ow));
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    w := SaveRound(nw);
    h := SaveRound(nh);
  end;
end;

function TAdvSmoothDock.GetBounds: TAdvSmoothDockBounds;
var
  I: Integer;
  res1, res2, res1Found, res2Found: Boolean;
begin
  Result.Start := 0;
  Result.Stop := Items.Count - 1;

  res1 := false;
  res2 := false;
  res1Found := false;
  res2Found := false;
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      case Position of
        dpRight, dpLeft:
        begin
          res1 := GetItemRect.Y >= -ItemAppearance.ImageHeight;
          res2 := GetItemRect.Y >= InsideRect.Bottom - ItemAppearance.ImageHeight;
        end;
        dpBottom, dpTop:
        begin
          res1 := GetItemRect.X >= -ItemAppearance.ImageWidth;
          res2 := GetItemRect.X >= InsideRect.Right - ItemAppearance.ImageWidth;
        end;
      end;

      if not res1Found and res1 then
      begin
        res1Found := true;
        result.Start := I;
      end;

      if not res2Found and res2 then
      begin
        res2Found := true;
        Result.Stop := I;
      end;

      if res1Found and res2Found then
      begin
        Break;
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothDock.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothDock.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothDock.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothDock.GetCountSelectable: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if (Items[I].Enabled) and (Items[i].Visible) then
      Inc(Result);
end;

function TAdvSmoothDock.GetFirstIndex: integer;
begin
  Result := GetBounds.Start;
end;

function TAdvSmoothDock.GetFirstVisibleItem: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].Visible then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothDock.GetForm: TAdvSmoothDockForm;
begin
  Result := frm;
end;

function TAdvSmoothDock.GetHeight: integer;
begin
  if GetActive then
    Result := frm.Height
  else
    Result := Height;
end;

function TAdvSmoothDock.GetMaximumHeight: integer;
begin
  Result :=ItemAppearance.ImageOffset + ItemAppearance.MaximumImageHeight + SaveRound(GetMaxSizeCaption.h) + ItemAppearance.JumpMargin
end;

function TAdvSmoothDock.GetMaximumWidth: integer;
begin
  Result := ItemAppearance.ImageOffset + ItemAppearance.MaximumImageWidth + SaveRound(GetMaxSizeCaption.w) + ItemAppearance.JumpMargin
end;

function TAdvSmoothDock.GetMaxSizeCaption: TCaptionSize;
var
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  f: TGPFont;
  sizer: TGPRectF;
  I: Integer;
  g: TGPGraphics;
  r: TGPRectF;
  sw: Single;
  sh: Single;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);
  Result.h := 0;
  Result.w := 0;
  ff := TGPFontFamily.Create(ItemAppearance.CaptionFont.Name);
  if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ff.Free;
    ff := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in ItemAppearance.CaptionFont.Style) then
    fs := fs + 1;
  if (fsItalic in ItemAppearance.CaptionFont.Style) then
    fs := fs + 2;
  if (fsUnderline in ItemAppearance.CaptionFont.Style) then
    fs := fs + 4;

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, ItemAppearance.CaptionFont.Size, fs, UnitPoint);
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if ShowCaption and not Separator then
      begin
        r := GetItemRect;
        g.MeasureString(Caption, length(Caption), f, MakeRect(r.X, r.Y, 10000, 10000), sf, sizer);
        sw := sizer.Width + 6;
        sh := sizer.Height + 4;
        if sw > Result.w then
          Result.w := sw;
        if sh > Result.h then
          Result.h := sh;

        Result.w := 0;
        Result.h := 0;
      end;
    end;
  end;

  ff.Free;
  sf.Free;
  f.free;
  g.Free;
end;

function TAdvSmoothDock.GetItemsMinimumRectangle: TGPRectF;
begin
  case Position of
    dpLeft: result := MakeRect(InsideRect.Left, InsideRect.Top + (GetHeight - GetTotalSize) / 2, GetMinimumWidth, GetTotalSize);
    dpRight: result := MakeRect(InsideRect.Right - GetMinimumWidth, InsideRect.Top + (GetHeight - GetTotalSize) / 2, GetMinimumWidth, GetTotalSize);
    dpTop: result := MakeRect(InsideRect.Left + (GetWidth - GetTotalSize) / 2, InsideRect.Top, GetTotalSize, GetMinimumHeight);
    dpBottom: result := MakeRect(InsideRect.Left + (GetWidth - GetTotalSize) / 2, InsideRect.Bottom - GetMinimumHeight, GetTotalSize, GetMinimumHeight);
  end;
end;

function TAdvSmoothDock.GetItemsRectangle: TGPRectF;
begin
  case Position of
    dpLeft, dpRight: result := MakeRect(InsideRect.Left, InsideRect.Top + (GetHeight - Max(ItemAppearance.ImageHeight * 2, GetTotalSize)) / 2, InsideRect.Right, Max(ItemAppearance.ImageHeight * 2, GetTotalSize));
    dpTop, dpBottom: result := MakeRect(InsideRect.Left + (GetWidth - Max(ItemAppearance.ImageWidth * 2, GetTotalSize)) / 2, InsideRect.Top, Max(ItemAppearance.ImageWidth * 2, GetTotalSize), InsideRect.Bottom);
  end;
end;

function TAdvSmoothDock.GetLastVisibleItem: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[I].Visible then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothDock.GetMinimumHeight: integer;
begin
  Result := ItemAppearance.ImageOffset + ItemAppearance.ImageHeight + 25
end;

function TAdvSmoothDock.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothDock.GetTotalSize(Hover: Boolean = True): Double;
var
  total: Double;
  i: integer;
  r: TGPRectF;
begin
  total := 0;
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if Items[I].Visible then
      begin
        r := GetItemRectCenter(Hover);
        case FOwner.Position of
          dpRight, dpLeft: total := total + r.Height + ItemAppearance.Spacing;
          dpTop, dpBottom: total := total + r.Width + ItemAppearance.Spacing;
        end;
      end;
    end;
  end;
  result := total + ItemAppearance.Spacing;
end;

function TAdvSmoothDock.GetMinimumWidth: integer;
begin
  Result := ItemAppearance.ImageOffset + ItemAppearance.ImageWidth + 25
end;

function TAdvSmoothDock.GetPlatFormRectangle: TGPRectF;
begin
  case Position of
    dpRight, dpLeft: Result := MakeRect(GetItemsRectangle.X, Max(0, GetItemsRectangle.Y), GetItemsRectangle.Width, Min(InsideRect.Bottom - Insiderect.Left, GetItemsRectangle.Height));
    dpBottom, dpTop: Result := MakeRect(Max(0, GetItemsRectangle.X), GetItemsRectangle.Y, Min(InsideRect.Right - InsideRect.Left, GetItemsRectangle.Width), GetItemsRectangle.Height);
  end;
end;

function TAdvSmoothDock.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothDock.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;


function TAdvSmoothDock.GetVisibleItemCount: integer;
var
  b: TAdvSmoothDockBounds;
begin
  b := GetBounds;
  Result := b.Stop - b.Start;
end;

function TAdvSmoothDock.GetWidth: integer;
begin
  if GetActive then
    Result := frm.Width
  else
    Result := Width;
end;

procedure TAdvSmoothDock.HoverXY(X, Y: Integer);
var
  msg: TMessage;
begin
  if Assigned(frm) then
  begin
    frm.CMMouseEnter(msg);
    frm.MouseMove([], X, Y);
  end;
end;

function TAdvSmoothDock.InsideRect: TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (Fill.ShadowColor <> clNone) {and not Transparent} then
    sh := Fill.ShadowOffset;

  Result := Rect(0, 0, GetWidth, GetHeight);
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1 - sh;
  Result.Bottom := Result.Bottom - 1 - sh;

  if (Fill.BorderColor <> clNone) {and not Transparent} then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

function TAdvSmoothDock.IsItemBounds(X, Y: integer): Boolean;
begin
  result := false;
  case Position of
    dpLeft: result := X <= GetItemBounds;
    dpRight: result := X >= GetItemBounds;
    dpBottom: result := y >= GetItemBounds;
    dpTop: result := y <= GetItemBounds;
  end;
end;

function TAdvSmoothDock.IsJumping: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i].Jump then
    begin
      Result := true;
      Break;
    end;
  end;
end;

function TAdvSmoothDock.IsMouseOnArrow(X, Y: integer): Boolean;
begin
  Result := PtInGPRect(GetArrowLeft, Point(X, Y)) or PtInGPRect(GetArrowRight, Point(X, Y));
end;

function TAdvSmoothDock.IsMoving(X, Y: integer): Boolean;
begin
  Result := not PtInGPRect(MakeRect(FClickX - DRAGMARGIN / 2, FClickY - DRAGMARGIN / 2, DRAGMARGIN, DRAGMARGIN), Point(X, Y));
end;

function TAdvSmoothDock.GetItemBounds: integer;
begin
  result := 0;
  with ItemAppearance do
  begin
    case Position of
      dpLeft: result := InsideRect.Left + MaximumImageWidth + FImageOffset;
      dpRight: result := InsideRect.Right - MaximumImageWidth - FImageOffset;
      dpTop: result := InsideRect.Top + MaximumImageHeight + FImageOffset;
      dpBottom: result := InsideRect.Bottom - MaximumImageHeight - FImageOffset;
    end;
  end;
end;

procedure TAdvSmoothDock.ItemAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDock.ItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDock.KeyDown(var Key: Word; Shift: TShiftState);
var
  r: TGPRectF;
  i: integer;
  prevsel: Integer;
begin
  inherited;
  if GetCountSelectable > 0 then
  begin
    prevsel := FSel;
    case Key of
      VK_LEFT, VK_UP: FSel := FSel - 1;
      VK_RIGHT, VK_DOWN: FSel := FSel + 1;
      VK_HOME : FSel := MinSel;
      VK_END: FSel := MaxSel;
      VK_NEXT : FSel := FSel + 3;
      VK_PRIOR : FSel := FSel - 3;
    end;

    if (FSel > GetLastVisibleItem) and (FSel > prevsel) then
      FSel := GetFirstVisibleItem
    else if (FSel < GetFirstVisibleItem) and (Fsel < prevsel) then
      FSel := GetLastVisibleItem;

    i := Max(GetFirstVisibleItem, Min(GetLastVisibleItem, FSel));

    if (FSel > -1) and (Key in [VK_PRIOR, VK_HOME, VK_LEFT, VK_UP, VK_NEXT, VK_RIGHT, VK_DOWN, VK_END]) then
    begin
      while (Items[i].Enabled = false) or (Items[i].Visible = false) do
      begin
        case Key of
          VK_PRIOR, VK_HOME, VK_LEFT, VK_UP: i := i - 1;
          VK_NEXT, VK_RIGHT, VK_DOWN, VK_END: i := i + 1;
        end;

        if (i > GetLastVisibleItem) then
          i := GetFirstVisibleItem
        else if i < GetFirstVisibleItem then
          i := GetLastVisibleItem;
      end;
    end;

    Fsel := Max(GetFirstVisibleItem, Min(GetLastVisibleItem, i));

    if (Key <> VK_RETURN) and (Key <> VK_SPACE) then
    begin
      Fsel := Max(GetFirstVisibleItem, Min(Fsel, GetLastVisibleItem));
      r := Items[FSel].GetItemRect;
      FPrevX := FCurX;
      FPrevY := FCurY;
      FCurX := SaveRound(r.X + r.Width / 2);
      FCurY := SaveRound(r.Y + r.Height / 2);
    end;

    case Key of
      VK_RETURN:
      begin
        SelectedItemIndex := FSel;
        if (SelectedItemIndex >= GetFirstVisibleItem) and (SelectedItemIndex <= GetLastVisibleItem) then
          DoItemClick(SelectedItemIndex);
      end;
    end;

    Changed;
  end;
end;

procedure TAdvSmoothDock.Loaded;
begin
  inherited;
  FCur := Self.Cursor;
  if FOleDropTargetAssigned then
  begin
    FDockDropTarget.AcceptText := true;
    FDockDropTarget.AcceptFiles := true;
    FDockDropTarget.AcceptURLs := true;
  end;
end;

procedure TAdvSmoothDock.LoadFromTheme(FileName: String);
begin

end;

function TAdvSmoothDock.MaxSel: integer;
var
  i: integer;
begin
  Result := -1;
  for I := Items.Count - 1 downto 0 do
  begin
    if (Items[I].Enabled) and (Items[i].Visible) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothDock.MinSel: integer;
var
  i: integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[I].Enabled) and (Items[i].Visible) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TAdvSmoothDock.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
  if IsMouseOnArrow(X, Y) then
  begin
    FArrowLeftDown := PtInGPRect(GetArrowLeft, Point(x, y));
    FArrowRightDown := PtInGPRect(GetArrowRight, Point(x, Y));
    FTimeDownOnArrow := 0;

    Changed;
    Exit;
  end;

  FMouseDown := true;
  FClickX := X;
  FClickY := Y;
  FMouseCopy := (Button = mbRight);
  FStartPopupTimer := true;
  FDoPopup := false;
  FPopupTime := 0;
  FPopupClickIndex := XYToItem(X, Y);
end;

procedure TAdvSmoothDock.DoMouseEnter(var Msg: TMessage);
var
  i: integer;
begin
  {$IFDEF DELPHI2006_LVL}
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
  {$ENDIF}


  for I := 0 to Items.Count - 1 do
  begin
    Items[i].FHoverSizeToW := 0;
    Items[i].FHoverSizeToH := 0;
    Items[i].FDoItemAnimation := true;
  end;
  FHoveredItemIndex := -1;
  FDoPopup := false;
  FMouseEntered := true;
  Changed;
end;

procedure TAdvSmoothDock.DoMouseLeave(var Msg: TMessage);
var
  I: Integer;
begin
  {$IFDEF DELPHI2006_LVL}
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
  {$ENDIF}

  if FDoPopup then
    Exit;

  for I := 0 to Items.Count - 1 do
  begin
    Items[i].FHoverSizeW := 0;
    Items[i].FHoverSizeH := 0;
    Items[i].FHoverSizeToW := 0;
    Items[i].FHoverSizeToH := 0;
    Items[i].FDoItemAnimation := true;
  end;

  FArrowLeftDown := false;
  FArrowLeftHover := false;
  FArrowRightHover := false;
  FArrowRightDown := false;
  FHintX := -1;
  FHintY := -1;
  FHoveredItemIndex := -1;
  FTimeDownOnArrow := 0;

  Changed;
end;

procedure TAdvSmoothDock.DoMouseMove(Shift: TShiftState; X, Y: Integer);
var
  hi: integer;
  I: Integer;
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);

  FPrevX := FCurX;
  FPrevY := FCurY;
  FCurX := X;
  FCurY := Y;

  if IsMouseOnArrow(X, Y) then
  begin
    if not FMouseOnArrow then
    begin
      FArrowLeftHover := PtInGPRect(GetArrowLeft, Point(x, y));
      FArrowRightHover := PtInGPRect(GetArrowRight, Point(x, Y));

      FMouseOnArrow := true;
      if FDoPopup then
        Exit;

      for I := 0 to Items.Count - 1 do
      begin
        Items[i].FHoverSizeToW := 0;
        Items[i].FHoverSizeToH := 0;
        Items[i].FDoItemAnimation := true;
      end;

      FHintX := -1;
      FHintY := -1;
      FHoveredItemIndex := -1;
      Changed;
      Exit;
    end
    else
      Exit;
  end
  else
  begin
    FMouseOnArrow := false;
    FArrowLeftHover := false;
    FArrowRightHover := false;
  end;

  if IsMoving(X, Y) then
    FPopupTime := 0;

//  if FDoPopup then
//    Exit;

  if FMouseDown then
  begin
    FDragX := X;
    FDragY := Y;
  end;

  if FMouseDown and IsMoving(X, Y) then
    DoDrag(X, Y, FMouseCopy)
  else
  begin
    FDblClick := false;
    FMouseEntered := false;
  end;

  FHintX := X;
  FHintY := Y;

  Application.CancelHint;
  if IsItemBounds(X, Y) then
  begin
    hi := XYToItem(X, Y);
    if hi <> -1 then
    begin
      if Assigned(FOnItemHover) then
        FOnItemHover(Self, hi);

      Screen.Cursor := crHandPoint;
    end
    else
      Screen.Cursor := FCur;

    if hi <> FHoveredItemIndex then
    begin
      FHoveredItemIndex := hi;
      if ItemAppearance.AnimationSpan = 0 then
        Changed;
    end;
  end;
end;

procedure TAdvSmoothDock.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  item: integer;
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);

  FTimeDownOnArrow := 0;
  if IsMouseOnArrow(X, Y) then
  begin
    if FArrowRightDown then
      DoArrowRightDown;

    if FArrowLeftDown then
      DoArrowLeftDown;

    FArrowLeftDown := false;
    FArrowRightDown := false;
    Changed;

    Exit;
  end;

  FArrowLeftDown := false;
  FArrowRightDown := false;
  Changed;

  FFocused := true;

  FMouseCopy := false;
  FMouseDown := false;
  FPopupTime := 0;
  FStartPopupTimer := false;
  FDoPopup := false;

  if FDragDropItem = nil then
  begin
    item := XYToItem(X, Y);
    if item <> -1 then
    begin
      SelectedItemIndex := item;
      FSel := item;
      DoItemClick(SelectedItemIndex);
    end;
  end
  else
    DoDrop(X, Y);
end;

procedure TAdvSmoothDock.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  I: Integer;
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      with Items[i] do
      begin
        if (AComponent = FPopupMenu) then
          FPopupMenu := nil;
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.Paint;
var
  g: TGPGraphics;
  f: TCustomForm;
  chk: Boolean;
begin
  if not (csDesigning in ComponentState) and not Assigned(frm) and Visible  then
    ShowForm;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);
  DrawBackGround(g, true);

  if Assigned(frm) then
  begin
    f := GetParentForm(Self);
    chk := GetActiveWindow = f.Handle;

    if chk then
    begin
      g.Free;
      Exit;
    end;
  end;
  DrawPlatForm(g);
  Drawitems(g);
  DrawArrows(g);
  DrawIndicator(g, SelectedItemIndex);
  if not ItemAppearance.PermanentCaption then
    DrawCaption(g);

  if Assigned(OnDraw) then
    OnDraw(Self, g, MakeRect(0, 0, Width - 1, Height - 1));
  g.Free;
end;

function TAdvSmoothDock.PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TAdvSmoothDock.Resize;
begin
  UpdateBounds := true;
  UpdateSize;
  inherited;
end;

procedure TAdvSmoothDock.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothDock.SetAnimationFactor(const Value: integer);
begin
  if (FAnimationFactor <> Value) and (Value > 0) then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetArrowAppearance(
  const Value: TAdvSmoothDockArrowAppearance);
begin
  if FArrowAppearance <> Value then
  begin
    FArrowAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetAS(const Value: Boolean);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    UpdateBounds := true;
    UpdateSize;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetBackGroundAutoSize(const Value: Boolean);
begin
  if FBackGroundAutoSize <> value then
  begin
    FBackGroundAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetBackGroundSize(const Value: Integer);
begin
  if FBackGroundSize <> Value then
  begin
    FBackGroundSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatForm3D(const Value: Boolean);
begin
  if FPlatForm3D <> Value then
  begin
    FPlatForm3D := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatForm3DColor(const Value: TColor);
begin
  if FPlatForm3DColor <> value then
  begin
    FPlatForm3DColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatForm3DColorTo(const Value: TColor);
begin
  if FPlatForm3DColorTo <> value then
  begin
    FPlatForm3DColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatForm3DOpacity(const Value: Byte);
begin
  if FPlatForm3DOpacity <> value then
  begin
    FPlatForm3DOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatForm3DOpacityTo(const Value: Byte);
begin
  if FPlatForm3DOpacityTo <> value then
  begin
    FPlatForm3DOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetColorTones(ATones: TColorTones);
begin
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  ItemAppearance.Fill.Color := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorTo := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorMirror := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  ItemAppearance.Fill.BorderColor := ATones.Background.BorderColor;
  ItemAppearance.Fill.GradientMirrorType := gtVertical;
  ItemAppearance.Fill.Rounding:= 0;

  ItemAppearance.DisabledFill.Color := ATones.Disabled.BrushColor;
  ItemAppearance.DisabledFill.ColorTo := ATones.Disabled.BrushColor;
  ItemAppearance.DisabledFill.ColorMirror := ATones.Disabled.BrushColor;
  ItemAppearance.DisabledFill.ColorMirrorTo := ATones.Disabled.BrushColor;
  ItemAppearance.DisabledFill.BorderColor := ATones.Disabled.BorderColor;
  ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
  ItemAppearance.DisabledFill.Rounding:= 0;

  ItemAppearance.HoverFill.Color := ATones.Hover.BrushColor;
  ItemAppearance.HoverFill.ColorTo :=  ATones.Hover.BrushColor;
  ItemAppearance.HoverFill.BorderColor :=  ATones.Hover.BrushColor;
  ItemAppearance.HoverFill.ColorMirror := ATones.Hover.BrushColor;
  ItemAppearance.HoverFill.GradientType := gtVertical;
  ItemAppearance.HoverFill.Rounding:= 0;
  //ItemAppearance.HoverFill.Glow:= gmGradient;
  ItemAppearance.HoverFill.GlowGradientColor:= clNone;

  ItemAppearance.SelectedFill.Color := ATones.Selected.BrushColor;
  ItemAppearance.SelectedFill.ColorTo := ATones.Selected.BrushColor;
  ItemAppearance.SelectedFill.ColorMirror := ATones.Selected.BrushColor;
  ItemAppearance.SelectedFill.ColorMirrorTo := ATones.Selected.BrushColor;
  ItemAppearance.SelectedFill.BorderColor := ATones.Selected.BorderColor;
  ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
  ItemAppearance.SelectedFill.Rounding:= 0;
  //ItemAppearance.SelectedFill.Glow:= gmGradient;
  ItemAppearance.SelectedFill.GlowGradientColor:= clNone;
end;

procedure TAdvSmoothDock.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  // TODO : do color settings here
  ItemAppearance.HoverFill.Rounding:= 0;
  ItemAppearance.Fill.Rounding:= 0;
  ItemAppearance.Fill.Glow:= gmNone;
  ItemAppearance.HoverFill.Glow:= gmNone;
  ItemAppearance.SelectedFill.Glow:= gmNone;
  ItemAppearance.Fill.GlowGradientColor:= clWhite;
  ItemAppearance.HoverFill.GlowGradientColor:= clWhite;
  ItemAppearance.Fill.Rounding:= 0;
  ItemAppearance.DisabledFill.Rounding:= 0;

 case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $EEDBC8;
        ItemAppearance.Fill.ColorTo := $F6DDC9;
        ItemAppearance.Fill.ColorMirror := $EDD4C0;
        ItemAppearance.Fill.ColorMirrorTo := $F7E1D0;
        //ItemAppearance.Fill.BorderColor := $E0B99B;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $E6E9E2;
        ItemAppearance.Fill.ColorTo := $00E6D8D8;
        ItemAppearance.Fill.ColorMirror := $C8B2B3;
        ItemAppearance.Fill.ColorMirrorTo := $E6E9E2;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $CFF0EA;
        ItemAppearance.Fill.ColorTo := $CFF0EA;
        ItemAppearance.Fill.ColorMirror := $8CC0B1;
        ItemAppearance.Fill.ColorMirrorTo := $CFF0EA;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := $C9D1D5;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $D2BDB6;
        ItemAppearance.HoverFill.ColorTo := $D2BDB6;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.ColorMirrorTo := clNone;
        ItemAppearance.HoverFill.BorderColor := $808080;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $D8D5D4;
        ItemAppearance.DisabledFill.ColorTo := $D8D5D4;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $B59285;
        ItemAppearance.SelectedFill.ColorTo := $B59285;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $808080;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $FFEFE3;
        ItemAppearance.Fill.ColorTo := $FFDDC4;
        ItemAppearance.Fill.ColorMirror := $FFD1AD;
        ItemAppearance.Fill.ColorMirrorTo := $FFDBC0;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $F9F8F8;
        ItemAppearance.Fill.ColorTo := $E4E2DF;
        ItemAppearance.Fill.ColorMirror := $D1CBC7;
        ItemAppearance.Fill.ColorMirrorTo := $E2DEDB;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;

        ItemAppearance.Fill.Color := clBtnFace;//clWhite;
        ItemAppearance.Fill.ColorTo := clBtnFace;//$B9D8DC;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EFD3C6;
        ItemAppearance.HoverFill.ColorTo := $EFD3C6;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.ColorMirrorTo := clNone;
        ItemAppearance.HoverFill.BorderColor :=  clHighlight;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := clInactiveCaption;
        ItemAppearance.SelectedFill.ColorTo := clInactiveCaption;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := clHighLight;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := $DFEDF0;
        ItemAppearance.Fill.ColorMirror := $DFEDF0;
        ItemAppearance.Fill.ColorMirrorTo := $DFEDF0;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
   end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $F9F8F8;
        ItemAppearance.Fill.ColorTo := $E4E2DF;
        ItemAppearance.Fill.ColorMirror := $D1CBC7;
        ItemAppearance.Fill.ColorMirrorTo := $E2DEDB;
        ItemAppearance.Fill.BorderColor := clNone;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $EBFDFF;
        ItemAppearance.HoverFill.ColorTo := $ACECFF;
        ItemAppearance.HoverFill.ColorMirror := $59DAFF;
        ItemAppearance.HoverFill.ColorMirrorTo := $A4E9FF;
        ItemAppearance.HoverFill.BorderColor :=  $99CEDB;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $AAD9FF;
        ItemAppearance.SelectedFill.ColorTo := $6EBBFF;
        ItemAppearance.SelectedFill.ColorMirror := $42AEFE;
        ItemAppearance.SelectedFill.ColorMirrorTo := $7AE1FE;
        ItemAppearance.SelectedFill.BorderColor := $42AEFE;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FDF8F1;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $FDF8F1;
        ItemAppearance.Fill.ColorTo := $FCEFD5;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $FDDE99;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $FFFDF9;
        ItemAppearance.HoverFill.ColorTo := $FFFAF0;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.ColorMirrorTo := clNone;
        ItemAppearance.HoverFill.BorderColor :=  $FCF2DA;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $FEF9F0;
        ItemAppearance.SelectedFill.ColorTo := $FDF0D7;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $FEDF9A;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;

        PlatForm3DColor := $FEF9F0;
        PlatForm3DColorTo := $FDF0D7;
      end;
      tsWindows7:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FDF8F1;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := $FDF8F1;
        ItemAppearance.Fill.ColorTo := $FCEFD5;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $FDDE99;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.HoverFill.Color := $FDFBFA;
        ItemAppearance.HoverFill.ColorTo := $FDF3EB;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.ColorMirrorTo := clNone;
        ItemAppearance.HoverFill.BorderColor :=  $FBD6B8;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $FCEBDC;
        ItemAppearance.SelectedFill.ColorTo := $FCDBC1;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $CEA27D;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;

        PlatForm3DColor := $FEF9F0;
        PlatForm3DColorTo := $FDF0D7;
      end;
      tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clbtnFace;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clBtnFace;
        ItemAppearance.Fill.ColorTo := clBtnFace;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := clNone;


        ItemAppearance.DisabledFill.Color := clBtnFace;
        ItemAppearance.DisabledFill.ColorTo := clBtnFace;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;


        ItemAppearance.HoverFill.Color := clSilver;
        ItemAppearance.HoverFill.ColorTo := clSilver;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.ColorMirrorTo := clNone;
        ItemAppearance.HoverFill.BorderColor :=  clGray;


        ItemAppearance.SelectedFill.Color := clHighLight;
        ItemAppearance.SelectedFill.ColorTo := clHighLight;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := clGray;
    end;
     tsOffice2010Blue:
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;
        ItemAppearance.Fill.Rounding:= 2;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $D2CDC8;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
        ItemAppearance.DisabledFill.Rounding:= 2;

        ItemAppearance.HoverFill.Color := $8AE3FD;
        ItemAppearance.HoverFill.ColorTo :=  clNone;
        ItemAppearance.HoverFill.BorderColor :=  $58CAF1;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.GradientType := gtVertical;
        ItemAppearance.HoverFill.Rounding:= 2;
        ItemAppearance.HoverFill.Glow:= gmGradient;
        ItemAppearance.HoverFill.GlowGradientColor:= $D9F9FD;

        ItemAppearance.SelectedFill.Color := $6CD0FF;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $308AC2;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Rounding:= 2;
        ItemAppearance.SelectedFill.Glow:= gmGradient;
        ItemAppearance.SelectedFill.GlowGradientColor:= $7BEEFF;
      end;
       tsOffice2010Silver:
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;
        ItemAppearance.Fill.Rounding:= 2;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $D2CDC8;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
        ItemAppearance.DisabledFill.Rounding:= 2;

        ItemAppearance.HoverFill.Color := $8AE3FD;
        ItemAppearance.HoverFill.ColorTo :=  clNone;
        ItemAppearance.HoverFill.BorderColor :=  $58CAF1;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.GradientType := gtVertical;
        ItemAppearance.HoverFill.Rounding:= 2;
        ItemAppearance.HoverFill.Glow:= gmGradient;
        ItemAppearance.HoverFill.GlowGradientColor:= $D9F9FD;

        ItemAppearance.SelectedFill.Color := $6CD0FF;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $308AC2;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Rounding:= 2;
        ItemAppearance.SelectedFill.Glow:= gmGradient;
        ItemAppearance.SelectedFill.GlowGradientColor:= $7BEEFF;
      end;
       tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;
        ItemAppearance.Fill.Rounding:= 2;

        ItemAppearance.DisabledFill.Color := $00F2F2F2;
        ItemAppearance.DisabledFill.ColorTo := $00B6B6B6;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $D2CDC8;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;
        ItemAppearance.DisabledFill.Rounding:= 2;

        ItemAppearance.HoverFill.Color := $8AE3FD;
        ItemAppearance.HoverFill.ColorTo :=  clNone;
        ItemAppearance.HoverFill.BorderColor :=  $58CAF1;
        ItemAppearance.HoverFill.ColorMirror := clNone;
        ItemAppearance.HoverFill.GradientType := gtVertical;
        ItemAppearance.HoverFill.Rounding:= 2;
        ItemAppearance.HoverFill.Glow:= gmGradient;
        ItemAppearance.HoverFill.GlowGradientColor:= $D9F9FD;

        ItemAppearance.SelectedFill.Color := $6CD0FF;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $308AC2;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Rounding:= 2;
        ItemAppearance.SelectedFill.Glow:= gmGradient;
        ItemAppearance.SelectedFill.GlowGradientColor:= $7BEEFF;

      end;
       tsWindows8, tsWindows10:
      begin
        Fill.Color := $F7F6F5;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clWhite;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $E4E3E2;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $F7F7F7;
        ItemAppearance.DisabledFill.ColorTo := $F7F7F7;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $DEDEDE;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $F7E0C9;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $E4A262;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Glow:= gmGradient;
        ItemAppearance.SelectedFill.GlowGradientColor:= $F7E0C9;

        ItemAppearance.HoverFill.Color := $F7EFE8;
        ItemAppearance.HoverFill.ColorTo := $F7EFE8;
        ItemAppearance.HoverFill.ColorMirror := $F7EFE8;
        ItemAppearance.HoverFill.ColorMirrorTo := $F7EFE8;
        ItemAppearance.HoverFill.BorderColor :=  $F9CEA4;
        ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2013White:
      begin
        Fill.Color := $EEEEEE;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

      ItemAppearance.Fill.Color := clWhite;
      ItemAppearance.Fill.ColorTo := clNone;
      ItemAppearance.Fill.ColorMirror := clNone;
      ItemAppearance.Fill.ColorMirrorTo := clNone;
      ItemAppearance.Fill.BorderColor := $D4D4D4;
      ItemAppearance.Fill.GradientMirrorType := gtVertical;

      ItemAppearance.DisabledFill.Color := $EEEEEE;
      ItemAppearance.DisabledFill.ColorTo := $EEEEEE;
      ItemAppearance.DisabledFill.ColorMirror := clNone;
      ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
      ItemAppearance.DisabledFill.BorderColor := $ACACAC;
      ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

      ItemAppearance.SelectedFill.Color := $FCE2C8;
      ItemAppearance.SelectedFill.ColorTo := clNone;
      ItemAppearance.SelectedFill.ColorMirror := clNone;
      ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
      ItemAppearance.SelectedFill.BorderColor := $E59D56;
      ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      ItemAppearance.SelectedFill.Glow:= gmGradient;
      ItemAppearance.SelectedFill.GlowGradientColor:= $FCE2C8;


      ItemAppearance.HoverFill.Color := $FCF0E4;
      ItemAppearance.HoverFill.ColorTo := $FCF0E4;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $EAB47E;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013LightGray:
      begin
        Fill.Color := $FAFAFA;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

      ItemAppearance.Fill.Color := clWhite;
      ItemAppearance.Fill.ColorTo := clNone;
      ItemAppearance.Fill.ColorMirror := clNone;
      ItemAppearance.Fill.ColorMirrorTo := clNone;
      ItemAppearance.Fill.BorderColor := $D4D4D4;
      ItemAppearance.Fill.GradientMirrorType := gtVertical;

      ItemAppearance.DisabledFill.Color := $EEEEEE;
      ItemAppearance.DisabledFill.ColorTo := $EEEEEE;
      ItemAppearance.DisabledFill.ColorMirror := clNone;
      ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
      ItemAppearance.DisabledFill.BorderColor := $ACACAC;
      ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

      ItemAppearance.SelectedFill.Color := $FCE2C8;
      ItemAppearance.SelectedFill.ColorTo := clNone;
      ItemAppearance.SelectedFill.ColorMirror := clNone;
      ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
      ItemAppearance.SelectedFill.BorderColor := $E59D56;
      ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      ItemAppearance.SelectedFill.Glow:= gmGradient;
      ItemAppearance.SelectedFill.GlowGradientColor:= $FCE2C8;


      ItemAppearance.HoverFill.Color := $FCF0E4;
      ItemAppearance.HoverFill.ColorTo := $FCF0E4;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $EAB47E;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013Gray:
      begin
        Fill.Color := $F3F3F3;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;

      ItemAppearance.Fill.Color := clWhite;
      ItemAppearance.Fill.ColorTo := clNone;
      ItemAppearance.Fill.ColorMirror := clNone;
      ItemAppearance.Fill.ColorMirrorTo := clNone;
      ItemAppearance.Fill.BorderColor := $D4D4D4;
      ItemAppearance.Fill.GradientMirrorType := gtVertical;

      ItemAppearance.DisabledFill.Color := $EEEEEE;
      ItemAppearance.DisabledFill.ColorTo := $EEEEEE;
      ItemAppearance.DisabledFill.ColorMirror := clNone;
      ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
      ItemAppearance.DisabledFill.BorderColor := $ACACAC;
      ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

      ItemAppearance.SelectedFill.Color := $FCE2C8;
      ItemAppearance.SelectedFill.ColorTo := clNone;
      ItemAppearance.SelectedFill.ColorMirror := clNone;
      ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
      ItemAppearance.SelectedFill.BorderColor := $E59D56;
      ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
      ItemAppearance.SelectedFill.Glow:= gmGradient;
      ItemAppearance.SelectedFill.GlowGradientColor:= $FCE2C8;


      ItemAppearance.HoverFill.Color := $FCF0E4;
      ItemAppearance.HoverFill.ColorTo := $FCF0E4;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $EAB47E;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $D4D4D4;

               ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $D4D4D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := clWhite;
        ItemAppearance.DisabledFill.ColorTo := clNone;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $D4D4D4;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $E3BDA3;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $E3BDA3;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Glow:= gmNone;
        ItemAppearance.SelectedFill.GlowGradientColor:= clNone;

      ItemAppearance.HoverFill.Color := $F2E1D5;
      ItemAppearance.HoverFill.ColorTo := clNone;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $F2E1D5;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;





      end;
    tsOffice2016Gray:
      begin
        Fill.Color := $444444;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $444444;

      ItemAppearance.Fill.Color := $B2B2B2;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $444444;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.DisabledFill.Color := $B2B2B2;
        ItemAppearance.DisabledFill.ColorTo := clNone;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $444444;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $E3BDA3;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $E3BDA3;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Glow:= gmNone;
        ItemAppearance.SelectedFill.GlowGradientColor:= clNone;



      ItemAppearance.HoverFill.Color := $F2E1D5;
      ItemAppearance.HoverFill.ColorTo := $F2E1D5;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $F2E1D5;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2016Black:
      begin
        Fill.Color := $444444;
        Fill.ColorTo := clNone;
        Fill.BorderColor := $444444;

          ItemAppearance.Fill.Color := $363636;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $444444;;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;


        ItemAppearance.DisabledFill.Color := $363636;
        ItemAppearance.DisabledFill.ColorTo := clNone;
        ItemAppearance.DisabledFill.ColorMirror := clNone;
        ItemAppearance.DisabledFill.ColorMirrorTo := clNone;
        ItemAppearance.DisabledFill.BorderColor := $444444;
        ItemAppearance.DisabledFill.GradientMirrorType := gtVertical;

        ItemAppearance.SelectedFill.Color := $444444;
        ItemAppearance.SelectedFill.ColorTo := clNone;
        ItemAppearance.SelectedFill.ColorMirror := clNone;
        ItemAppearance.SelectedFill.ColorMirrorTo := clNone;
        ItemAppearance.SelectedFill.BorderColor := $444444;
        ItemAppearance.SelectedFill.GradientMirrorType := gtVertical;
        ItemAppearance.SelectedFill.Glow:= gmNone;
        ItemAppearance.SelectedFill.GlowGradientColor:= clNone;


      ItemAppearance.HoverFill.Color := $6A6A6A;
      ItemAppearance.HoverFill.ColorTo := $6A6A6A;
      ItemAppearance.HoverFill.ColorMirror := clNone;
      ItemAppearance.HoverFill.ColorMirrorTo := clNone;
      ItemAppearance.HoverFill.BorderColor :=  $6A6A6A;
      ItemAppearance.HoverFill.GradientMirrorType := gtVertical;
      end;
  end;
end;

procedure TAdvSmoothDock.SetDockOnDesktop(const Value: Boolean);
begin
  if FDockOnDeskTop <> value then
  begin
    FDockOnDeskTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetDockVisible(const Value: Boolean);
begin
  FDockVisible := Value;
  if Assigned(frm) then
    frm.Visible := Value;
  Changed;
end;

procedure TAdvSmoothDock.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDock.SetShowFocus(const Value: Boolean);
begin
  if FFocus <> value then
  begin
    FFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetStayOnTop(const Value: Boolean);
begin
  FStayOnTop := Value;
end;

procedure TAdvSmoothDock.SetItemAppearance(
  const Value: TAdvSmoothDockItemAppearance);
begin
  if FItemAppearance <> Value then
  begin
    FItemAppearance.Assign(value);
    ItemAppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothDock.SetItems(const Value: TAdvSmoothDockItems);
begin
  if FItems <> value then
  begin
    FItems.Assign(Value);
    ItemsChanged(Self);
  end;
end;

procedure TAdvSmoothDock.SetOleDragDrop(const Value: Boolean);
begin
  FOleDragDrop := Value;

  if not (csDesigning in ComponentState) and Assigned(frm) then
  begin
    if FOleDragDrop then
    begin
      FDockDropTarget := TAdvSmoothDockDropTarget.Create(Self);
      FOleDropTargetAssigned := RegisterDragDrop(frm.Handle, FDockDropTarget) = S_OK;
    end
    else if FOleDropTargetAssigned then RevokeDragDrop(frm.Handle);
  end;
end;

procedure TAdvSmoothDock.SetPlatformFill(const Value: TGDIPFill);
begin
  if FPlatformFill <> value then
  begin
    FPlatformFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPlatformSize(const Value: integer);
begin
  if FPlatformSize <> Value then
  begin
    FPlatformSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.SetPosition(const Value: TAdvSmoothDockPosition);
var
  I: Integer;
  b: TAdvSmoothDockBounds;
begin
  if FPosition <> value then
  begin
    if Assigned(frm) then
      frm.FMouseEntered := false;

    FCurrentPos := 0;
    FPosTo := 0;

    FPosition := Value;
    UpdateBounds := true;

    ItemAppearance.SeparatorChanged(Self);
    b := GetBounds;
    for I := 0 to Items.Count - 1 do
      Items[i].UpdateReflection;

    UpdateBounds := true;
    UpdateSize;

    if FUpdateCount = 0 then
      Invalidate;

    if (csDestroying in ComponentState) then
      Exit;

    if Assigned(frm) then
      frm.Invalidate;
  end;
end;

procedure TAdvSmoothDock.SetSelectedItemIndex(const Value: integer);
var
  i: integer;
begin
  if GetCountSelectable > 0 then
  begin
    if FSelectedItemIndex <> value then
    begin
      i := Max(0, Min(Items.Count - 1, Value));
      while (Items[i].Enabled = false) or (Items[i].Visible = false) do
      begin
        Inc(i);
        if (i > Items.Count - 1) then
          i := 0
        else if i < 0 then
          i := Items.Count - 1;
      end;

      FSelectedItemIndex := Max(0, Min(Items.Count - 1, i));
      if Assigned(FOnItemSelected) then
        FOnItemSelected(Self, SelectedItemIndex);
      Changed;
    end;
  end;
end;

procedure TAdvSmoothDock.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDock.ShowForm;
begin
  frm := TAdvSmoothDockForm.CreateNew(Application);
  frm.OwnerDock := Self;

  case Position of
    dpLeft, dpRight:
    begin
      frm.Height := Self.Height;
      frm.Width := GetMaximumWidth;
    end;
    dpTop, dpBottom:
    begin
      frm.Height := GetMaximumHeight;
      frm.Width := Self.Width;
    end;
  end;
  frm.Init;
  SetWindowPos(frm.Handle, 0, frm.Left, frm.Top,
    frm.Width, frm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
  SetOleDragDrop(FOleDragDrop);
end;

procedure TAdvSmoothDock.UpdateSize;
var
  h, dif: integer;
begin
  if not UpdateBounds then
  begin
    if Assigned(frm) and not (csDestroying in ComponentState) then
    begin
      case Position of
        dpLeft:
        begin
          frm.Left := Parent.ClientOrigin.X + Self.Left;
          frm.Top := Parent.ClientOrigin.Y + Self.Top;
        end;
        dpRight:
        begin
          frm.Left := Parent.ClientOrigin.X + Self.Left - (GetMaximumWidth - GetMinimumWidth);
          frm.Top := Parent.ClientOrigin.Y + Self.Top;
        end;
        dpTop:
        begin
          frm.Left := Parent.ClientOrigin.X + Self.Left;
          frm.Top := Parent.ClientOrigin.Y + Self.Top;
        end;
        dpBottom:
        begin
          frm.Left := Parent.ClientOrigin.X + Self.Left;
          frm.Top := Parent.ClientOrigin.Y + Self.Top - (GetMaximumHeight - GetMinimumHeight);
        end;
      end;
    end;
  end
  else
  begin
    Updatebounds := false;
    case Position of
      dpRight:
      begin
        if AutoSize then
          Width := GetMinimumWidth;

        h := GetMaximumWidth;
        dif := h - Self.Width;
        if Assigned(frm) and not (csDestroying in ComponentState) then
          frm.SetBounds(Parent.ClientOrigin.X + Self.Left - dif , Parent.ClientOrigin.Y + Self.Top, h, Self.Height)
      end;
      dpLeft:
      begin
        if AutoSize then
          Width := GetMinimumWidth;
        h := GetMaximumWidth;
        if Assigned(frm) and not (csDestroying in ComponentState) then
          frm.SetBounds(Parent.ClientOrigin.X + Self.Left , Parent.ClientOrigin.Y + Self.Top, h, Self.Height)
      end;
      dpTop:
      begin
        if AutoSize then
          Height := GetMinimumHeight;
        h := GetMaximumHeight;
        if Assigned(frm) and not (csDestroying in ComponentState) then
          frm.SetBounds(Parent.ClientOrigin.X + Self.Left , Parent.ClientOrigin.Y + Self.Top , Self.Width, h)
      end;
      dpBottom:
      begin
        if AutoSize then
          Height := GetMinimumHeight;
        h := GetMaximumHeight;
        dif := h - Self.Height;
        if Assigned(frm) and not (csDestroying in ComponentState) then
          frm.SetBounds(Parent.ClientOrigin.X + Self.Left , Parent.ClientOrigin.Y + Self.Top - dif , Self.Width, h)
      end;
    end;
  end;
end;

procedure TAdvSmoothDock.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothDock.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothDock.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
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
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

function TAdvSmoothDock.XYToItem(X, Y: integer; CountSeparator: Boolean = false; DragDrop: Boolean = false): integer;
var
  I: Integer;
  r: TGPRectF;
  spc: integer;
  chk: Boolean;
  b: TAdvSmoothDockBounds;
begin
  result := -1;
  spc := ItemAppearance.Spacing div 2;
  b := GetBounds;
  for I := b.Start to b.Stop do
  begin
    if Items[i].Visible then
    begin
      if Items[I] <> FDragDropItem then
      begin
        if not (Items[i].Separator and not CountSeparator) and Items[I].Enabled then
        begin
          r := Items[I].GetItemRect;
          case Position of
            dpLeft, dpRight:
            begin
              r.y := r.Y - spc;
              r.Height := r.Height + spc;
            end;
            dpTop, dpBottom:
            begin
              r.X := r.X - spc;
              r.Width := r.Width + spc;
            end;
          end;
          if PtInRect(Bounds(SaveRound(r.X), SaveRound(r.Y), SaveRound(r.Width), SaveRound(r.Height)), Point(X, Y)) then
          begin
            if DragDrop and (FDragDropItem <> nil) then
            begin
              chk := false;
              case Position of
                dpLeft, dpRight:
                begin
                  if FDragY > r.Y then
                    chk := FDragY > r.Y + (R.Height / 2)
                  else if FDragY > r.Y + R.Height then
                    chk := FDragY < r.Y + (R.Height / 2);
                end;
                dpTop, dpBottom:
                begin
                  if FDragX > r.X then
                    chk := FDragX > r.X + (R.Width / 2)
                  else if FDragX > r.X + R.Width then
                    chk := FDragX < r.X + (R.Width / 2);
                end;
              end;

              if chk then
              begin
                Result := I;
                break;
              end;
            end
            else
            begin
              Result := I;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TAdvSmoothDockItems }

function TAdvSmoothDockItems.Add: TAdvSmoothDockItem;
begin
  Result := TAdvSmoothDockItem(inherited Add);
end;

constructor TAdvSmoothDockItems.Create(AOwner: TAdvSmoothDock);
begin
  inherited Create(TAdvSmoothDockItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothDockItems.Delete(Index: Integer);
begin
  Items[index].Free;
end;

function TAdvSmoothDockItems.GetItem(Index: Integer): TAdvSmoothDockItem;
begin
  Result := TAdvSmoothDockItem(inherited Items[index]);
end;

function TAdvSmoothDockItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothDockItems.Insert(Index: Integer): TAdvSmoothDockItem;
begin
  Result := TAdvSmoothDockItem(inherited Insert(Index));
end;

procedure TAdvSmoothDockItems.SetItem(Index: Integer;
  const Value: TAdvSmoothDockItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothDockItem }

procedure TAdvSmoothDockItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothDockItem) then
  begin
    FImage.Assign((Source as TAdvSmoothDockItem).Image);
    FDisabledImage.Assign((Source as TAdvSmoothDockItem).DisabledImage);
    FCaption := (Source as TAdvSmoothDockItem).Caption;
    FVisible := (Source as TAdvSmoothDockItem).Visible;
    FEnabled := (Source as TAdvSmoothDockItem).Enabled;
    FHint := (Source as TAdvSmoothDockItem).Hint;
    FJump := (Source as TAdvSmoothDockItem).Jump;
    FStatusIndicator.Assign((Source as TAdvSmoothDockItem).StatusIndicator);
    FSeparator := (Source as TAdvSmoothDockItem).Separator;
    FShowCaption := (Source as TAdvSmoothDockItem).ShowCaption;
    FProgressMaximum := (Source as TAdvSmoothDockItem).ProgressMaximum;
    FProgressMinimum := (Source as TAdvSmoothDockItem).ProgressMinimum;
    FProgressPosition := (Source as TAdvSmoothDockItem).ProgressPosition;
    FData := (Source as TAdvSmoothDockItem).Data;
    FTag := (Source as TAdvSmoothDockItem).Tag;
    FPopupMenu := (Source as TAdvSmoothDockItem).PopupMenu;
    FObject := (Source as TAdvSmoothDockItem).ItemObject;
    FWordWrapping := (Source as TAdvSmoothDockItem).WordWrapping;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothDockItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothDockItems).FOwner;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FDisabledImage := TAdvGDIPPicture.Create;
  FDisabledImage.OnChange := ImageChanged;
  FVisible := true;
  FEnabled := true;
  FReflectionImage := nil;
  FJump := false;
  FJumpUp := true;
  FStatusIndicator := TAdvSmoothDockStatus.Create(FOwner);
  FStatusIndicator.OnChange := StatusIndicatorChanged;
  FSeparator := false;
  FShowCaption := true;
  FProgressMaximum := 100;
  FProgressMinimum := 0;
  FProgressPosition := 0;
  FWordWrapping := True;
  Changed;
end;

destructor TAdvSmoothDockItem.Destroy;
begin
  FImage.Free;
  FDisabledImage.Free;
  FStatusIndicator.Free;
  if Assigned(FReflectionImage) then
    FReflectionImage.Free;
  inherited;
  Changed;
end;

function TAdvSmoothDockItem.GetHoverRect(Hover: Boolean = True): TGPRectF;
var
  x, w: Single;
  hsh, hsw: Double;
begin
  if Hover then
  begin
    hsh := FHoverSizeH;
    hsw := FHoverSizeW;
  end
  else
  begin
    hsh := 0;
    hsw := 0;
  end;
  if Separator then
  begin
    case FOwner.Position of
      dpRight, dpLeft: hsw := 0;
      dpBottom, dpTop: hsh := 0;
    end;
  end;
  with FOwner.ItemAppearance do
  begin
    case FOwner.Position of
      dpLeft:
      begin
        w := ImageHeight;
        x := ((Index + 1) * Spacing) + (Index * w);
        Result := MakeRect((FOwner.InsideRect.Left + FImageOffset),x, ImageWidth, w);
        Result.X := Result.X;
        Result.Y := Result.Y;
        Result.Width := Result.Width + hsh;
        Result.Height := Result.Height + hsw;
        Result.X := Result.X + FJMP;
      end;
      dpRight:
      begin
        w := ImageHeight;
        x := ((Index + 1) * Spacing) + (Index * w);
        Result := MakeRect((FOwner.InsideRect.Right - FImageOffset - ImageWidth),x, ImageWidth, w);
        Result.X := Result.X - hsh;
        Result.Y := Result.Y;
        Result.Width := Result.Width + hsh;
        Result.Height := Result.Height + hsw;
        Result.X := Result.X - FJMP;
      end;
      dpTop:
      begin
        w := ImageWidth;
        x := ((Index + 1) * Spacing) + (Index * w);
        Result := MakeRect(x, (FOwner.InsideRect.Top + FImageOffset), w, ImageHeight);
        Result.X := Result.X;
        Result.Y := Result.Y;
        Result.Width := Result.Width + hsw;
        Result.Height := Result.Height + hsh;
        Result.Y := Result.Y + FJMP;
      end;
      dpBottom:
      begin
        w := ImageWidth;
        x := ((Index + 1) * Spacing) + (Index * w);
        Result := MakeRect(x, (FOwner.InsideRect.Bottom - FImageOffset - ImageHeight), w, ImageHeight);
        Result.X := Result.X;
        Result.Y := Result.Y - hsh;
        Result.Width := Result.Width + hsw;
        Result.Height := Result.Height + hsh;
        Result.Y := Result.Y - FJMP;
      end;
    end;
  end;
end;

function TAdvSmoothDockItem.GetItemRectCenter(Hover: Boolean = True): TGPRectF;
var
  prevr: TGPRectF;
begin
  with FOwner.ItemAppearance do
  begin
    if (Index = 0) then
    begin
      if Visible then
      begin
        Result := GetHoverRect(Hover);
        case FOwner.Position of
          dpLeft, dpRight: Result.Y := FOwner.ItemAppearance.Spacing;
          dpTop, dpBottom: Result.X := FOwner.ItemAppearance.Spacing;
        end;
        FItemRect := Result;
        Exit;
      end
      else
      begin
        result := MakeRect(0, 0, 0, 0);
        FItemRect := Result;
        Exit;
      end;
    end;

    if (Index > 0) and Visible then
    begin
      if GetVisibleIndex(Index - 1) >= 0 then
      begin
        prevr := FOwner.Items[GetVisibleIndex(Index - 1)].FItemRect;
        result := GetHoverRect(Hover);
        case FOwner.Position of
          dpRight, dpLeft: result.Y := prevr.Y + prevr.Height + Spacing;
          dpTop, dpBottom: result.X := prevr.X + prevr.Width + Spacing;
        end;
        FitemRect := Result;
      end
      else 
      begin
        Result := GetHoverRect(Hover);
        case FOwner.Position of
          dpLeft, dpRight: Result.Y := FOwner.ItemAppearance.Spacing;
          dpTop, dpBottom: Result.X := FOwner.ItemAppearance.Spacing;
        end;
        FItemRect := Result;
      end;
    end
    else
    begin
      result := MakeRect(0, 0, 0, 0);
      FItemRect := Result;
    end;
  end;
end;

function TAdvSmoothDockItem.GetItemRect(Hover: Boolean = True): TGPRectF;
var
  total: Double;
  r: TGPRectF;
begin
  Result := MakeRect(0, 0, 0, 0);
  total := FOwner.GetTotalSize(Hover);
  r := GetItemRectCenter(Hover);
  case FOwner.Position of
    dpRight, dpLeft:
    begin
      if not (FOwner.GetItemsRectangle.Height > (FOwner.InsideRect.Bottom - FOwner.InsideRect.Top) + FOwner.ItemAppearance.ImageHeight) then
      begin
        FOwner.FCurrentPos := 0;
        FOwner.FPosTo := 0;
      end;
      Result := MakeRect(r.X, r.Y - FOwner.FCurrentPos + (FOwner.GetHeight - total) / 2,
        r.Width, r.Height)
    end;
    dpTop, dpBottom:
    begin
      if not (FOwner.GetItemsRectangle.Width > (FOwner.InsideRect.Right - FOwner.InsideRect.Left) + FOwner.ItemAppearance.ImageWidth) then
      begin
        FOwner.FCurrentPos := 0;
        FOwner.FPosTo := 0;
      end;
      Result := MakeRect(r.X - FOwner.FCurrentPos + (FOwner.GetWidth - total) / 2, r.Y,
        r.Width, r.Height)
    end;
  end;
end;

function TAdvSmoothDockItem.GetNormalRect(Hover: Boolean = True): TGPRectF;
var
  x, w: Single;
begin
  with FOwner.ItemAppearance do
  begin
    w := ImageWidth;
    x := ((Index + 1) * Spacing) + (Index * w);
    Result := MakeRect(x, (FOwner.InsideRect.Bottom - FImageOffset), w, ImageHeight);
    Result.X := Result.X;
    Result.Y := Result.Y;
    Result.Width := Result.Width;
    Result.Height := Result.Height;
  end;
end;

function TAdvSmoothDockItem.GetVisibleIndex(Item: Integer): Integer;
var
  K: integer;
begin
  Result := Item;
  for K := Item downto 0 do
  begin
    if not FOwner.Items[K].Visible then
    begin
      Dec(Result)
    end
    else
      break;
  end;
end;

procedure TAdvSmoothDockItem.ImageChanged(Sender: TObject);
begin
  UpdateReflection;
  Changed;
end;

procedure TAdvSmoothDockItem.Popup(Sender: TObject);
begin
  if FOwner.FDoPopup then
    FOwner.FDoPopup := false;
end;

procedure TAdvSmoothDockItem.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetDisabledImage(const Value: TAdvGDIPPicture);
begin
  if FDisabledImage <> Value then
  begin
    FDisabledImage.Assign(Value);
    ImageChanged(Self);
  end;
end;

procedure TAdvSmoothDockItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    ImageChanged(Self);
  end;
end;

procedure TAdvSmoothDockItem.SetHint(const Value: String);
begin
  if FHint <> value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> value then
  begin
    FImage.Assign(Value);
    ImageChanged(Self);
  end;
end;

procedure TAdvSmoothDockItem.SetJump(const Value: Boolean);
begin
  if FJump <> Value then
  begin
    if FJump and not Value then
    begin
      FDoLastJump := true;
      FJMPTO := 0;
    end;
    FJump := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetProgressMaximum(const Value: integer);
begin
  if FProgressMaximum <> value then
  begin
    FProgressMaximum := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetProgressMinimum(const Value: integer);
begin
  if FProgressMinimum <> Value then
  begin
    FProgressMinimum := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetProgressPosition(const Value: integer);
begin
  if FProgressPosition <> value then
  begin
    FProgressPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetSeparator(const Value: Boolean);
begin
  if FSeparator <> value then
  begin
    FSeparator := Value;
    FOwner.ItemAppearance.SeparatorChanged(Self);
    if Value then
      Self.Image.LoadFromStream(FOwner.FSeparatorMemoryStream)
    else
      Self.Image.Assign(nil);
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> value then
  begin
    FShowCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetStatusIndicator(const Value: TAdvSmoothDockStatus);
begin
  if FStatusIndicator <> value then
  begin
    FStatusIndicator.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItem.StatusIndicatorChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDockItem.UpdateReflection;
var
  gpbmp: TGPBitmap;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  w, h, x, y, op, alph: integer;
  clr, clrTemp: TGPColor;
  a: byte;
  s: Double;
  hr: HResult;
  img: TAdvGDIPPicture;
begin
  if not FDisabledImage.Empty and not Enabled then
    img := FDisabledImage
  else
    img := FImage;

  if img.Empty {or (csLoading in FOwner.ComponentState)} then
    Exit;

  with FOwner.ItemAppearance do
  begin
    img.GetImageSizes;

    w := img.Width;
    h := img.Height;

    s := FReflectionSize;

    ms := TMemoryStream.Create;
    img.SaveToStream(ms);
    hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
    if (hGlobal = 0) then
    begin
      ms.Free;
      raise Exception.Create('Could not allocate memory for reflection image');
    end;

    pstm := nil;
    pcbWrite := 0;

      // Create IStream* from global memory
    hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

    if (hr = S_OK) then
    begin
      pstm.Write(ms.Memory, ms.Size, @pcbWrite);

      if (ms.Size = pcbWrite) then
      begin
        gpbmp := TGPBitmap.Create(pstm);

        if Assigned(FReflectionImage) then
        begin
          FReflectionImage.Free;
          FReflectionImage := nil;
        end;

        FReflectionImage := TGPBitmap.Create(w, h{, PixelFormat32bppARGB});

        for y := 0 to h do
        begin
          if (y < FReflectionSize) then
          begin
            op := SaveRound( ((s - y)/FReflectionSize * FReflectionStart) +
                           y/s * FReflectionEnd);
          end
          else
            op := 0;

          if (op < 0) then
            op := 0;
          if (op > 255) then
            op := 255;

          for x := 0 to w do
          begin
            case FOwner.Position of
              dpLeft: gpbmp.GetPixel(y, w - x, clr);
              dpRight: gpbmp.GetPixel(h - y, w - x, clr);
              dpTop: gpbmp.GetPixel(x, y, clr);
              dpBottom: gpbmp.GetPixel(x, h - y, clr);
            end;
            a := GetAlpha(clr);
            if (a = 0) then
              continue;

            alph := SaveRound((op / 255) * a);
            clrTemp := MakeColor(alph, GetRed(clr), GetGreen(clr), GetBlue(clr));
            case FOwner.Position of
              dpLeft: FReflectionImage.SetPixel(h - y, w - x, clrTemp);
              dpRight: FReflectionImage.SetPixel(y, w - x, clrTemp);
              dpTop: FReflectionImage.SetPixel(x, h - y, clrTemp);
              dpBottom: FReflectionImage.SetPixel(x, y, clrTemp);
            end;
          end;
        end;
        gpbmp.Free;
      end;
      pstm := nil;
    end
    else
      GlobalFree(hGlobal);
    ms.Free;
  end;
end;

{ TAdvSmoothDockItemAppearance }

procedure TAdvSmoothDockItemAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothDockItemAppearance then
  begin
    FImageHeight := (Source as TAdvSmoothDockItemAppearance).ImageHeight;
    FImageWidth := (Source as TAdvSmoothDockItemAppearance).ImageWidth;
    FSpacing := (Source as TAdvSmoothDockItemAppearance).Spacing;
    FMaxImageWidth := (Source as TAdvSmoothDockItemAppearance).MaximumImageWidth;
    FMaxImageHeight := (Source as TAdvSmoothDockItemAppearance).MaximumImageHeight;
    FReflectionSize := (Source as TAdvSmoothDockItemAppearance).ReflectionSize;
    FReflectionStart := (Source as TAdvSmoothDockItemAppearance).ReflectionStart;
    FReflectionEnd := (Source as TAdvSmoothDockItemAppearance).ReflectionEnd;
    FReflectionSpacing := (Source as TAdvSmoothDockItemAppearance).ReflectionSpacing;
    FSelectionFill.Assign((Source as TAdvSmoothDockItemAppearance).SelectionFill);
    FCaptionFill.Assign((Source as TAdvSmoothDockItemAppearance).CaptionFill);
    FCaptionFont.Assign((Source as TAdvSmoothDockItemAppearance).CaptionFont);
    FSelectionSize := (Source as TAdvSmoothDockItemAppearance).SelectionSize;
    FSelectionOffset := (Source as TAdvSmoothDockItemAppearance).SelectionOffset;
    FFill.Assign((Source as TAdvSmoothDockItemAppearance).Fill);
    FHoverFill.Assign((Source as TAdvSmoothDockItemAppearance).HoverFill);
    FSelectedFill.Assign((Source as TAdvSmoothDockItemAppearance).SelectedFill);
    FDisabledFill.Assign((Source as TAdvSmoothDockItemAppearance).DisabledFill);
    FItemFill := (Source as TAdvSmoothDockItemAppearance).ItemBackGround;
    FSeparatorFill.Assign((Source as TAdvSmoothDockItemAppearance).SeparatorFill);
    FSeparatorSize := (Source as TAdvSmoothDockItemAppearance).SeparatorSize;
    FAspectRatio := (Source as TAdvSmoothDockItemAppearance).AspectRatio;
    FJumpMargin := (Source as TAdvSmoothDockItemAppearance).JumpMargin;
    FShowSelection := (Source as TAdvSmoothDockItemAppearance).ShowSelection;
    FPermanentCaption := (Source as TAdvSmoothDockItemAppearance).PermanentCaption;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothDockItemAppearance.Create(AOwner: TAdvSmoothDock);
begin
  FOwner := AOwner;
  FAspectRatio := true;
  FImageWidth := 50;
  FImageHeight := 50;
  FSpacing := 15;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FMaxImageWidth := 120;
  FMaxImageHeight := 120;
  FBackGroundVisible := false;
  FReflectionStart := 100;
  FReflectionEnd := 0;
  FReflectionSize := 50;
  FReflectionSpacing := 0;
  FImageOffset := 40;
  FJumpMargin := 40;
  FAnimationSpan := 400;
  FSelectionFill := TGDIPFill.Create;
  FSelectionFill.OnChange := FillChanged;
  FCaptionFill := TGDIPFill.Create;
  FCaptionFill.OnChange := FillChanged;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FCaptionFont.Name := 'Tahoma';
  {$ENDIF}
  FSelectionSize := 20;
  FSelectionOffset := 10;
  FSelectedFill := TGDIPFill.Create;
  FSelectedFill.OnChange := FillChanged;
  FHoverFill := TGDIPFill.Create;
  FHoverFill.OnChange := FillChanged;
  FDisabledFill := TGDIPFill.Create;
  FDisabledFill.OnChange := FillChanged;
  FItemFill := false;
  FProgressFill := TGDIPFill.Create;
  FProgressFill.OnChange := FillChanged;
  FSeparatorFill := TGDIPFill.Create;
  FSeparatorFill.OnChange := SeparatorChanged;
  FSeparatorSize := 10;
  FShowSelection := false;
  FPermanentCaption := false;
end;

destructor TAdvSmoothDockItemAppearance.Destroy;
begin
  FSelectionFill.Free;
  FFill.Free;
  FHoverFill.Free;
  FSelectedFill.Free;
  FDisabledFill.Free;
  FCaptionFill.Free;
  FCaptionFont.Free;
  FProgressFill.Free;
  FSeparatorFill.Free;
  inherited;
end;

procedure TAdvSmoothDockItemAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDockItemAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDockItemAppearance.SeparatorChanged(Sender: TObject);
var
  sep: TGPGraphics;
  g: TGPGraphics;
  clsid: TGUID;
  I: integer;
  p: TGPPen;
begin
  with FOwner do
  begin
    g := TGPGraphics.Create(FOwner.Canvas.Handle);
    g.SetSmoothingMode(SmoothingModeAntiAlias);

    FSeparatorImage.Free;
    FSeparatorImage := nil;
    sep := nil;

    case Position of
      dpLeft, dpRight:
      begin
        FSeparatorImage := TGPBitmap.Create(ImageWidth, SeparatorSize);
        sep := g.FromImage(FSeparatorImage);
        if SeparatorSize < 4 then
        begin
          p := TGPPen.Create(MakeColor(SeparatorFill.BorderOpacity, SeparatorFill.BorderColor), SeparatorSize);
          sep.DrawLine(p, 0, 0, ImageWidth - 1, 0);
          p.Free;
        end
        else
          FSeparatorFill.Fill(sep, MakeRect(0, 0, ImageWidth - 1, SeparatorSize - 1));
      end;
      dpTop, dpBottom:
      begin
        FSeparatorImage := TGPBitmap.Create(SeparatorSize, ImageHeight);
        sep := g.FromImage(FSeparatorImage);
        if SeparatorSize < 4 then
        begin
          p := TGPPen.Create(MakeColor(SeparatorFill.BorderOpacity, SeparatorFill.BorderColor), SeparatorSize);
          sep.DrawLine(p, 0, 0, 0, ImageHeight - 1);
          p.Free;
        end
        else
          FSeparatorFill.Fill(sep, MakeRect(0, 0, SeparatorSize - 1, ImageHeight - 1));
      end;
    end;
    if sep <> nil then
      sep.Free;
    g.Free;

    clsid := GetCLSID(itPNG);
    FOwner.FSeparatorImage.Save(TStreamAdapter.Create(FSeparatorMemoryStream), clsid);
    FSeparatorMemoryStream.Position := 0;

    for I := 0 to Items.Count - 1 do
      with Items[i] do
        if Separator then
          Image.LoadFromStream(FSeparatorMemoryStream);


    Changed;

  end;
end;

procedure TAdvSmoothDockItemAppearance.SetAnimationSpan(const Value: integer);
begin
  if FAnimationSpan <> Value then
  begin
    FAnimationSpan := Value;
    FOwner.CalculateSin;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetAspectRatio(const Value: Boolean);
begin
  if FAspectRatio <> value then
  begin
    FAspectRatio := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetCaptionFill(const Value: TGDIPFill);
begin
  if FCaptionFill <> value then
  begin
    FCaptionFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> value then
  begin
    FCaptionFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetDisabledFill(const Value: TGDIPFill);
begin
  if FDisabledFill <> value then
  begin
    FDisabledFill := Value;
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetHoverFill(const Value: TGDIPFill);
begin
  if FHoverFill <> value then
  begin
    FHoverFill.Assign(Value);
    FillChanged(self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetImageHeight(const Value: integer);
begin
  if FImageHeight <> value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetImageOffset(const Value: integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetImageWidth(const Value: integer);
begin
  if FImageWidth <> value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetItemFill(const Value: Boolean);
begin
  if FItemFill <> value then
  begin
    FItemFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetJumpMargin(const Value: integer);
begin
  if FJumpMargin <> Value then
  begin
    FJumpMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetMaxImageHeight(const Value: integer);
begin
  if FMaxImageHeight <> Value then
  begin
    FMaxImageHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetMaxImageWidth(const Value: integer);
begin
  if FMaxImageWidth <> Value then
  begin
    FMaxImageWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetPermanentCaption(
  const Value: Boolean);
begin
  if FPermanentCaption <> Value then
  begin
    FPermanentCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetProgressFill(const Value: TGDIPFill);
begin
  if FProgressFill <> value then
  begin
    FProgressFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetReflectionEnd(const Value: Byte);
var
  i: integer;
begin
  if FReflectionEnd <> value then
  begin
    FReflectionEnd := value;
    FOwner.BeginUpdate;
    for I := 0 to FOwner.Items.Count - 1 do
      FOwner.Items[I].UpdateReflection;

    FOwner.EndUpdate;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetReflectionSize(const Value: integer);
begin
  if FReflectionSize <> Value then
  begin
    FReflectionSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetReflectionSpacing(
  const Value: integer);
begin
  if FReflectionSpacing <> Value then
  begin
    FReflectionSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetReflectionStart(const Value: Byte);
var
  i: integer;
begin
  if FReflectionStart <> value then
  begin
    FReflectionStart := value;
    FOwner.BeginUpdate;
    for I := 0 to FOwner.Items.Count - 1 do
      FOwner.Items[I].UpdateReflection;

    FOwner.EndUpdate;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSelectedFill(const Value: TGDIPFill);
begin
  if FSelectedFill <> value then
  begin
    FSelectedFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSelectionFill(const Value: TGDIPFill);
begin
  if FSelectionFill <> value then
  begin
    FSelectionFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSelectionOffset(const Value: integer);
begin
  if FSelectionOffset <> value then
  begin
    FSelectionOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSelectionSize(const Value: integer);
begin
  if FSelectionSize <> value then
  begin
    FSelectionSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSeparatorFill(const Value: TGDIPFill);
begin
  if FSeparatorFill <> value then
  begin
    FSeparatorFill := Value;
    SeparatorChanged(Self);
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSeparatorSize(const Value: integer);
var
  I: Integer;
begin
  if FSeparatorSize <> Value then
  begin
    FSeparatorSize := Value;
    FOwner.ItemAppearance.SeparatorChanged(Self);
    for I := 0 to FOwner.FItems.Count - 1 do
    begin
      if FOwner.FItems[I].Separator then
        FOwner.FItems[I].Image.LoadFromStream(FOwner.FSeparatorMemoryStream);
    end;

    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> value then
  begin
    FShowSelection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockItemAppearance.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

{ TAdvSmoothDockStatus }

procedure TAdvSmoothDockStatus.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothDockStatus.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothDockStatus) then
  begin
    FAppearance.Assign((Source as TAdvSmoothDockStatus).Appearance);
    FOffsetTop := (Source as TAdvSmoothDockStatus).OffsetTop;
    FOffsetLeft := (Source as TAdvSmoothDockStatus).OffsetLeft;
    FVisible := (Source as TAdvSmoothDockStatus).Visible;
    FCaption := (Source as TAdvSmoothDockStatus).Caption;
  end;
end;

procedure TAdvSmoothDockStatus.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothDockStatus.Create(AOwner: TAdvSmoothDock);
begin
  FOwner := AOwner;
  FOffsetTop := 0;
  FOffsetLeft := 0;
  FVisible := False;
  FCaption := '0';
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
  if FOwner.FDesigntime then
  begin
    FAppearance.Fill.Color := clRed;
    FAppearance.Fill.GradientType := gtSolid;
    FAppearance.Fill.BorderColor := clGray;
    FAppearance.Font.Color := clWhite;
  end;
end;

destructor TAdvSmoothDockStatus.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TAdvSmoothDockStatus.SetAppearance(const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothDockStatus.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockStatus.SetOffsetLeft(const Value: integer);
begin
  if FOffsetLeft <> value then
  begin
    FOffsetLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockStatus.SetOffsetTop(const Value: integer);
begin
  if FOffsetTop <> value then
  begin
    FOffsetTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockStatus.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;


{ TGridDropTarget }

constructor TAdvSmoothDockDropTarget.Create(ADock: TAdvSmoothDock);
begin
  Inherited Create;
  FDock := ADock;
end;

procedure TAdvSmoothDockDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TAdvSmoothDockDropTarget.DragMouseMove(pt: TPoint; var Allow: Boolean;
  DropFormats: TDropFormats);
begin
  inherited;
  pt := FDock.frm.ScreenToClient(pt);
  FDock.DoDragOver(nil, pt.X, pt.Y, dsDragMove, Allow);
end;

procedure TAdvSmoothDockDropTarget.DropCol(pt: TPoint; Col: Integer);
begin
  inherited;

end;

procedure TAdvSmoothDockDropTarget.DropFiles(pt: TPoint; files: tstrings);
begin
  inherited;
  pt := FDock.frm.ScreenToClient(pt);
  FDock.DoDragDrop(files, pt.X, pt.Y);
end;

procedure TAdvSmoothDockDropTarget.DropRTF(pt: TPoint; s: string);
begin
  inherited;

end;

procedure TAdvSmoothDockDropTarget.DropText(pt: TPoint; s: string);
begin
  inherited;

end;

procedure TAdvSmoothDockDropTarget.DropURL(pt: TPoint; s: string);
begin
  inherited;

end;

{ TAdvSmoothDockForm }

procedure TAdvSmoothDockForm.ClearBuffer(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothDockForm.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseEntered := true;
  if Assigned(OwnerDock) then
  begin
    OwnerDock.DoMouseEnter(Msg);
    Self.Invalidate;
  end;
end;

procedure TAdvSmoothDockForm.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseEntered := false;
  Screen.Cursor := crDefault;
  if Assigned(OwnerDock) then
  begin
    OwnerDock.DoMouseLeave(Msg);
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TAdvSmoothDockForm.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

function TAdvSmoothDockForm.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothDockForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
  begin
    FMainBuffer.Free;
    FMainBuffer := nil;
  end;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothDockForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothDockForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

end;

procedure TAdvSmoothDockForm.CreateWnd;
begin
  inherited;
  if Assigned(FDock) then
    FDock.FormHookInit;
  UpdateWindow;
end;

procedure TAdvSmoothDockForm.DblClick;
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoDblClick;
end;

procedure TAdvSmoothDockForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
  begin
    FMainBuffer.Free;
    FMainBuffer := nil;
  end;
end;

procedure TAdvSmoothDockForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothDockForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothDockForm.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoDragDrop(Source, X, Y);
end;

procedure TAdvSmoothDockForm.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoDragOver(Source, X, Y, State, Accept);
end;

procedure TAdvSmoothDockForm.Draw(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Assigned(OwnerDock) then
  begin
    OwnerDock.DrawBackGround(g, false);
    OwnerDock.DrawPlatForm(g);
    OwnerDock.Drawitems(g);
    OwnerDock.DrawArrows(g);
    OwnerDock.DrawIndicator(g, OwnerDock.SelectedItemIndex);
    if not OwnerDock.ItemAppearance.PermanentCaption then
      OwnerDock.DrawCaption(g);

    if Assigned(OwnerDock.OnDraw) then
      OwnerDock.OnDraw(OwnerDock, g, MakeRect(0, 0, Width - 1, Height - 1));
  end;

  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothDockForm.FormHookDone;
var
  f: TCustomForm;
begin
  if Assigned(FDock) then
  begin
    f := GetParentForm(FDock);
    if Assigned(f) and f.HandleAllocated then
    begin
      if not FDock.DockOnDeskTop then
      {$IFDEF DELPHI_UNICODE}
        SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
        SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(OldWndProc));
      {$ENDIF}
    end;
  end;
end;

procedure TAdvSmoothDockForm.FormHookInit;
var
  f: TCustomForm;
begin
  if Assigned(FDock) then
  begin
    f := GetParentForm(Fdock);
    if not FDock.DockOnDeskTop and assigned(f) then
    begin
       { Hook parent }
      {$IFDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLongPtr(f.Handle, GWL_WNDPROC));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      OldWndProc := TFarProc(GetWindowLong(f.Handle, GWL_WNDPROC));
      {$ENDIF}

      {$IFDEF DELPHI9_LVL}
      NewWndProc := Classes.MakeObjectInstance(HookWndProc);
      {$ELSE}
      NewWndProc := MakeObjectInstance(HookWndProc);
      {$ENDIF}

      {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      SetWindowLong(f.Handle, GWL_WNDPROC, LInteger(NewWndProc));
      {$ENDIF}
    end;
  end;
end;


procedure TAdvSmoothDockForm.HookWndProc(var Msg: TMessage);
var
  f: TCustomForm;
begin
  if csDestroying in ComponentState then
    Exit;

  if Assigned(Fdock) then
  begin     
    f := GetParentForm(FDock);
    if Assigned(f) then
    begin
      Msg.Result := CallWindowProc(OldWndProc, f.Handle, Msg.Msg , Msg.wParam, Msg.lParam);
      case Msg.Msg of
       WM_ACTIVATE:
       begin
         if Fdock.Visible then
           PostMessage(Self.Handle, WM_USERACTIVATE, MSG.WParam, 0);
       end;
       WM_WINDOWPOSCHANGING:
       begin
         FDock.UpdateBounds := true;
         FDock.UpdateSize;
       end;
       WM_SIZE:
       begin
         if not FMouseEntered then
         begin
           FDock.UpdateBounds := true;
           Fdock.UpdateSize;
           FDock.Changed;
         end;
       end;
      end;
    end;
  end;
end;

procedure TAdvSmoothDockForm.Init;
begin
  ShowHint := OwnerDock.ShowHint;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  if OwnerDock.StayOnTop then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
  Color := clWhite;
  Position := poScreenCenter;
  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothDockForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoMouseDown(Button, Shift, X, Y);
end;

procedure TAdvSmoothDockForm.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoMouseMove(Shift, X, Y);
end;

procedure TAdvSmoothDockForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(OwnerDock) then
    OwnerDock.DoMouseUp(Button, Shift, X, Y);
end;

procedure TAdvSmoothDockForm.Paint;
begin
  inherited;
  UpdateWindow;
end;

procedure TAdvSmoothDockForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothDockForm.UpdateLayered;
begin
  ClearBuffer(nil);

  if FDock.StayOnTop then
  begin
    SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);
  end
  else
  begin
    SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);
  end;

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothDockForm.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
//  while BlendFunc.SourceConstantAlpha < 255 do
//  begin
    ScrDC := CreateCompatibleDC(0);
    MemDC := CreateCompatibleDC(ScrDC);

    FMainBuffer.GetHBITMAP(0, BitmapHandle);
    PrevBitmap := SelectObject(MemDC, BitmapHandle);
    Size.cx := Width;
    Size.cy := Height;
    P := Point(Left, Top);
    S := Point(0, 0);

    with BlendFunc do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 255;
      AlphaFormat := AC_SRC_ALPHA;
    end;

    UpdateLayeredWindow(Handle, ScrDC, @P, @Size, MemDC, @S, 0, @BlendFunc, ULW_ALPHA);

    SelectObject(MemDC, PrevBitmap);
    DeleteObject(BitmapHandle);

    DeleteDC(MemDC);
    DeleteDC(ScrDC);
//  end;
end;

procedure TAdvSmoothDockForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothDockForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothDockForm.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
  inherited;
end;

procedure TAdvSmoothDockForm.WMMouseActivate(var msg: TWMMouseActivate);
begin
  msg.result := MA_NOACTIVATE;
end;

procedure TAdvSmoothDockForm.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  if Msg.Result = HTCAPTION then
    Msg.Result := HTNOWHERE;
end;

procedure TAdvSmoothDockForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TAdvSmoothDockForm.WndProc(var Message: TMessage);
begin
  if Assigned(FDock) {and not (csDestroying in ComponentState)} then
  begin
    if (Message.Msg = WM_DESTROY) then
    begin
      if not (csDestroying in ComponentState) then
        FDock.FormHookDone;
      if FDock.FOleDropTargetAssigned then
        RevokeDragDrop(Handle);
    end
    else if Message.Msg = WM_USERACTIVATE then
    begin
      CMMouseLeave(Message);
      FDock.UpdateBounds := true;
      FDock.UpdateSize;
      UpdateWindow;
      case Message.WParam of
        0: ShowWindow(Handle, SW_HIDE);
        1, 2: ShowWindow(Handle, SW_SHOWNA);
      end;
    end;
  end;
  inherited;
  case Message.Msg of
    WM_ACTIVATE:
    outputdebugstring(pchar(inttostr(Message.WParam)));
  end;

end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothDockArrowAppearance }

procedure TAdvSmoothDockArrowAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothDockArrowAppearance) then
  begin
    FBorderColor := (Source as TAdvSmoothDockArrowAppearance).BorderColor;
    FColor := (Source as TAdvSmoothDockArrowAppearance).Color;
    FVisible := (Source as TAdvSmoothDockArrowAppearance).Visible;
    FSize := (Source as TAdvSmoothDockArrowAppearance).Size;
    FOpacity := (Source as TAdvSmoothDockArrowAppearance).Opacity;
  end;
end;

procedure TAdvSmoothDockArrowAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothDockArrowAppearance.Create(AOwner: TAdvSmoothDock);
begin
  FOwner := AOwner;
  FColor := clSilver;
  FOpacity := 200;
  FBorderColor := clBlack;
  FVisible := true;
  FSize := 35;
end;

destructor TAdvSmoothDockArrowAppearance.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothDockArrowAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockArrowAppearance.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockArrowAppearance.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockArrowAppearance.SetSize(const Value: integer);
begin
  if FSize <> value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothDockArrowAppearance.SetVisibled(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.



{***************************************************************************}
{ TAdvSmoothTouchKeyBoard component                                         }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvSmoothTouchKeyBoard;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Classes, Controls, ExtCtrls, Graphics, StdCtrls, Dialogs,
  IniFiles, Messages, Forms, Math, ImgList, DateUtils,
  GDIPFill, GDIPPictureContainer, AdvStyleIF,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}

  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 8; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  WMTABLETDEFBASE = $02C0;
  WMTABLETQUERYSYSTEMGESTURESTATUS = WMTABLETDEFBASE + 12;
  TABLETDISABLEPRESSANDHOLD = $00000001;

  // version history
  // 1.0.0.0 : first release
  // 1.0.1.0 : New : Built-in support for reduced color set for use with terminal servers
  //         : Improved, Show and Hide, Assign of KeyBoard
  // 1.0.1.1 : Fixed : small issue with drawing shift caption on key
  //         : Fixed : Issue with resizing keyboard keys in combination with frames
  // 1.0.1.2 : Improved Resizing KeyBoard issue with LoadKeybdLayout and SaveKeybdLayout
  // 1.0.2.0 : New : Property BackColor to set the Popup form color
  //         : New : Delphi 2010 Touch Support
  //         : New : QueryWindowHandle to dynamically focus a control
  //         : Fixed : Issue with KeyboardSizeChanged event recalculation of Width and Height
  //         : Fixed : Issue with UpperCase and LowerCase displaying special characters
  // v1.0.2.1 : Fixed : Issues with Saving and Loading keyboard layout
  // v1.0.2.2 : Fixed : Issue with KeyDistance
  // v1.0.2.3 : Fixed : Issue with font initialization
  // v1.0.2.4 : Fixed : Issue with Capslock key hanging
  // v1.0.3.0 : New : Built-in support for Office 2010 colors
  // v1.5.0.0 : New : Images on keys with Image List and PictureContainer support
  // v1.5.1.0 : New : Exposed Down property on TAdvSmoothTouchKeyItem
  //          : New : Property AllowAutoZoom to disable automatic zooming of keys
  //          : Fixed : Issue with autocompletion form
  //          : Fixed : Issue with showing form after selecting item in autocompletion form
  //          : Smaller improvements and fixes
  // v1.5.1.1 : Fixed : Issue in C++Builder with TKeyCachedBitmap record
  // v1.5.2.0 : New : Public Shift property to start as upper or lower case
  // v1.5.3.0 : New : LookupSpacing property to change show more or less lookup results in display
  //          : Fixed : Issue with Shift state in OnKeyClick
  // v1.5.4.0 : New : UseFocusedControl property to use TextChanged event on an internal edit control.
  // v1.5.4.1 : Fixed : Issue with pressed key when pressing in empty space
  // v1.5.5.0 : New : Image for downstate
  //          : New : ImageWidth and ImageHeight properties per key
  // v1.6.0.0 : New : Metro Style Support
  //          : Fixed : Issue with client aligned key autozooming
  //          : Fixed : Issue with UseFocusedControl property
  // v1.6.0.1 : Fixed : Issue with capslock key when closing application
  // v1.6.0.2 : Fixed : Issue with closing & opening keyboard at same location
  // v1.6.1.0 : Improved : Ability to post key based on key word or key caption
  // v1.6.2.0 : New : KeyboardWidth / KeyboardHeight properties added in TAdvSmoothPopupTouchKeyboard
  // v1.6.2.1 : Fixed : Issue with 64bit and Windows 8
  //          : Fixed : Issue with autozoom and showatxy in TAdvSmoothPopupTouchKeyBoard
  // v1.6.3.0 : New : OnBeforeKeyClick event
  // v1.6.4.0 : New : Support for special key skDelete added
  // v1.7.0.0 : New : Windows 8, Office 2013 styles added
  // v1.7.0.1 : Fixed : Issue with vertically centering images on keys
  // v1.7.0.2 : Fixed : memory leak in cached bitmap drawing
  // v1.7.0.3 : Fixed : Issue with applying style
  // v1.7.0.4 : Fixed : Issue with popup touch keyboardwidth and keyboardheight
  // v1.7.0.5 : Fixed : access violation with empty lookup values
  // v1.7.1.0 : New : Combination keys
  // v1.7.1.1 : Improved : Touch feedback
  // v1.7.1.2 : Fixed : Issue with use on operating systems older than Windows 7
  // v1.7.1.3 : Fixed : GDI+ leak
  // v1.7.1.4 : Fixed : Issue with unregistering touch window in destroywnd
  // v1.7.2.0 : New : properties to control shading
  // v1.7.3.0 : New : properties on popup keyboard level, support for autocompletion
  // v1.7.3.1 : Fixed : Issue with changing Keydistance at runtime
  // v1.8.0.0 : New : Windows 10, Office 2016 styles added
  // v1.8.1.0 : New : skEscape special key added
  // v1.8.1.1 : Fixed : Issue with posting capslock key in destructor

  WM_USERACTIVATE = WM_USER + 101;

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TKeyCachedBitmap = record
    CachedBitmap: TGPBitmap;
    ID: integer;
    Width: Double;
    Height: Double;
  end;

  TAdvSmoothTouchKeyBoard = class;

  TAdvSmoothTouchKeyItem = class;

  TAdvSmoothSubKeyItem = class;

  TAdvSmoothCompletion = class;

  TAdvSmoothCompletionRangeType = (rtNumber, rtDateTime, rtCustom);

  TAdvSmoothCompletionStepType = (stNumber, stSecond, stMinute, stHour, stDay, stMonth, stYear);

  TAdvSmoothCompletionCustomItem = class(TCollectionItem)
  private
    FRect: TGPRectF;
    FOwner: TAdvSmoothCompletion;
    FValue: Double;
    FText: String;
    FImageIndex: integer;
    FpictureName: string;
    procedure SetText(const Value: String);
    procedure SetValue(const Value: Double);
    procedure SetImageIndex(const Value: integer);
    procedure SetPictureName(const Value: string);
  protected
    procedure Changed;
  published
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: String read FText write SetText;
    property Value: Double read FValue write SetValue;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property PictureName: string read FpictureName write SetPictureName;
  end;

  TAdvSmoothCompletionCustomItems = class(TCollection)
  private
    FOwner: TAdvSmoothCompletion;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothCompletionCustomItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothCompletionCustomItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothCompletion);
    function Add: TAdvSmoothCompletionCustomItem;
    function Insert(Index: Integer): TAdvSmoothCompletionCustomItem;
    property Items[Index: Integer]: TAdvSmoothCompletionCustomItem read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
    procedure Update(Item: TCollectionItem); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothCompletionColumn = class(TCollectionItem)
  private
    FSaveDateRangeto, FSaveDateRangeFrom: TDateTime;
    FDrawIndex: integer;
    FSp: Double;
    FAnimating, FAnimate: Boolean;
    FMouseDown, FMouseUp: Boolean;
    FTimeStart, FTimeStop: integer;
    FDragY, FScrollY: integer;
    FClickX, FClickY: integer;
    FCurrentScPos, FScPosTo: integer;
    Fowner: TAdvSmoothCompletion;
    FEnabled: Boolean;
    FVisible: Boolean;
    FRangeFrom: Double;
    FRangeTo: Double;
    FSelectedValue: Double;
    FRangeType: TAdvSmoothCompletionRangeType;
    FHint: String;
    FStep: Double;
    FValueFormat: String;
    FDateRangeTo: TDateTime;
    FDateRangeFrom: TDateTime;
    FDateTimeValueFormat: String;
    FStepType: TAdvSmoothCompletionStepType;
    FCustomItems: TAdvSmoothCompletionCustomItems;
    FCyclic: Boolean;
    FFont: TFont;
    FWidth: integer;
    FTextAlign: TAlignment;
    FOnlyDate: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetRangeFrom(const Value: Double);
    procedure SetRangeTo(const Value: Double);
    procedure SetSelectedValue(const Value: Double);
    procedure SetRangeType(const Value: TAdvSmoothCompletionRangeType);
    procedure SetHint(const Value: String);
    procedure SetStep(const Value: Double);
    procedure SetValueFormat(const Value: String);
    procedure SetDateRangeFrom(const Value: TDateTime);
    procedure SetDateRangeTo(const Value: TDateTime);
    procedure SetDateTimeValueFormat(const Value: String);
    procedure SetStepType(const Value: TAdvSmoothCompletionStepType);
    procedure SetCustomItems(const Value: TAdvSmoothCompletionCustomItems);
    procedure SetCyclic(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetWidth(const Value: integer);
    function GetSelectedDateTime: TDateTime;
    procedure SetSelectedDateTime(const Value: TDateTime);
    procedure SetTextAlign(const Value: TAlignment);
    function GetSelectedCustomIndex: integer;
    procedure SetSelectedCustomIndex(const Value: integer);
    procedure SetOnlyDate(const Value: Boolean);
  protected
    procedure Changed;
    procedure CustomItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics; r: TGPRectF);
    procedure SetAnimatedValue;
    function GetColumnRect: TGPRectF;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ScrollToValue(Value: double; Animation: Boolean = true; AnimationSpeed: integer = 4);
    procedure Next(Animation: Boolean = true);
    procedure Previous(Animation: Boolean = true);
    function IncSteps(StartValue: Double; nr: integer): double;
    function StepsFromTo(StartValue, EndValue: Double): integer;
    function GetStep: Double;
    function GetRangeCount: integer;
    function GetRangeTo: Double;
    function GetRangeFrom: Double;
    property SelectedDateTime: TDateTime read GetSelectedDateTime write SetSelectedDateTime;
    property SelectedCustomIndex: integer read GetSelectedCustomIndex write SetSelectedCustomIndex;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Hint: String read FHint write SetHint;
    property Width: integer read FWidth write SetWidth default 20;
    property RangeFrom: Double read FRangeFrom write SetRangeFrom;
    property RangeTo: Double read FRangeTo write SetRangeTo;
    property DateRangeFrom: TDateTime read FDateRangeFrom write SetDateRangeFrom;
    property DateRangeTo: TDateTime read FDateRangeTo write SetDateRangeTo;
    property Step: Double read FStep write SetStep;
    property StepType: TAdvSmoothCompletionStepType read FStepType write SetStepType default stNumber;
    property SelectedValue: Double read FSelectedValue write SetSelectedValue;
    property ValueFormat: String read FValueFormat write SetValueFormat;
    property DateTimeValueFormat: String read FDateTimeValueFormat write SetDateTimeValueFormat;
    property RangeType: TAdvSmoothCompletionRangeType read FRangeType write SetRangeType default rtNumber;
    property OnlyDate: Boolean read FOnlyDate write SetOnlyDate default false;
    property CustomItems: TAdvSmoothCompletionCustomItems read FCustomItems write SetCustomItems;
    property Cyclic: Boolean read FCyclic write SetCyclic default false;
    property Font: TFont read FFont write SetFont;
    property TextAlign: TAlignment read FTextAlign write SetTextAlign default taCenter;
  end;

  TAdvSmoothCompletionColumns = class(TCollection)
  private
    FOwner: TAdvSmoothCompletion;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothCompletionColumn;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothCompletionColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothCompletion);
    function Add: TAdvSmoothCompletionColumn;
    function Insert(Index: Integer): TAdvSmoothCompletionColumn;
    property Items[Index: Integer]: TAdvSmoothCompletionColumn read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
    procedure Update(Item: TCollectionItem); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothCompletionColumnAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothCompletion;
    FAutoSize: Boolean;
    FSpacing: integer;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FTextSpacing: integer;
    FDisabledFill: TGDIPFill;
    FHoverFill: TGDIPFill;
    FAllowHovering: Boolean;
    FImageHeight: integer;
    FImageWidth: integer;
    procedure SetSpacing(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTextSpacing(const Value: integer);
    procedure SetDisabledFill(const Value: TGDIPFill);
    procedure SetHoverFill(const Value: TGDIPFill);
    procedure SetAllowHovering(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetImageHeight(const Value: integer);
    procedure SetImageWidth(const Value: integer);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothCompletion);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property AllowHovering: Boolean read FAllowHovering write SetAllowHovering default true;
    property Spacing: integer read FSpacing write SetSpacing default 5;
    property Fill: TGDIPFill read FFill write SetFill;
    property HoverFill: TGDIPFill read FHoverFill write SetHoverFill;
    property DisabledFill: TGDIPFill read FDisabledFill write SetDisabledFill;
    property TextSpacing: integer read FTextSpacing write SetTextSpacing default 25;
    property ImageWidth: integer read FImageWidth write SetImageWidth default 30;
    property ImageHeight: integer read FImageHeight write SetImageHeight default 30;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothCompletionSelectedValueChangedEvent = procedure(Sender: TObject; Column, SelectedCustomIndex: integer; SelectedValue: Double; RangeType: TAdvSmoothCompletionRangeType) of object;

  TAdvSmoothCompletionColumnHintEvent = procedure(Sender: TObject; Column: integer; Hint: String) of object;

  TAdvSmoothCompletion = class(TCustomControl)
  private
    FUpdatecount: integer;
    FRect: TRect;
    FFocused, FConstructed: Boolean;
    FFocusedColumn: integer;
    FAnimateTimer: TTimer;
    FDesignTime: Boolean;
    FHoveredColumn: integer;
    FColumnAppearance: TAdvSmoothCompletionColumnAppearance;
    FFill: TGDIPFill;
    FSelectedFill: TGDIPFill;
    FSelectedHeight: integer;
    FColumns: TAdvSmoothCompletionColumns;
    FAnimationFactor: integer;
    FOnSelectedValueChanged: TAdvSmoothCompletionSelectedValueChangedEvent;
    FOnColumnHint: TAdvSmoothCompletionColumnHintEvent;
    FContainer: TGDIPPictureContainer;
    FImages: TCustomImageList;
    FReadOnly: Boolean;
    FBottomLayerFill: TGDIPfill;
    FTopLayerFill: TGDIPFill;
    FOnChange: TNotifyEvent;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetColumnAppearance(const Value: TAdvSmoothCompletionColumnAppearance);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSelectedFill(const Value: TGDIPFill);
    procedure SetSelectedHeight(const Value: integer);
    procedure SetColumns(const Value: TAdvSmoothCompletionColumns);
    procedure SetAnimationFactor(const Value: integer);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetBottomLayerFill(const Value: TGDIPfill);
    procedure SetTopLayerFill(const Value: TGDIPFill);
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Changed;
    procedure ColumnAppearanceChanged(Sender: TObject);
    procedure ColumnsChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics; r: TRect);
    procedure DrawBackGround(g: TGPGraphics; r: TGPRectF);
    procedure DrawColumns(g: TGPGraphics);
    procedure DrawTopLayer(g: TGPGraphics);
    procedure DrawSelectedLayer(g: TGPGraphics);
    procedure InitPreview;
    procedure Animate(Sender: TObject);
    function InsideRect: TRect;
    function GetSelectedRect: TRect;
    function GetVersionNr: integer;
    function XYToCustomItem(X, Y: integer): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MouseWheelHandler(var Message: TMessage); override;
    function XYToColumn(X, Y: integer): integer;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 4;
    property Columns: TAdvSmoothCompletionColumns read FColumns write SetColumns;
    property ColumnAppearance: TAdvSmoothCompletionColumnAppearance read FColumnAppearance write SetColumnAppearance;
    property SelectedFill: TGDIPFill read FSelectedFill write SetSelectedFill;
    property SelectedHeight: integer read FSelectedHeight write SetSelectedHeight default 30;
    property TopLayerFill: TGDIPFill read FTopLayerFill write SetTopLayerFill;
    property BottomLayerFill: TGDIPfill read FBottomLayerFill write SetBottomLayerFill;
    property Fill: TGDIPFill read FFill write SetFill;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property Images: TCustomImageList read FImages write FImages;
    property Version: String read GetVersion write SetVersion;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property OnSelectedValueChanged: TAdvSmoothCompletionSelectedValueChangedEvent read FOnSelectedValueChanged write FOnSelectedValueChanged;
    property OnColumnHint: TAdvSmoothCompletionColumnHintEvent read FOnColumnHint write FOnColumnHint;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property Enabled;
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
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property Visible;
    property TabStop;
  end;

  TAdvSmoothTouchKeyBoardAutoForm = class(TForm)
  private
    FMmx, FMmy: integer;
    FMdx, FMdy: integer;
    OldWndProc, NewWndProc: Pointer;
    FMainBuffer: TGPBitmap;
    FKeyBoard: TAdvSmoothTouchKeyBoard;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

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
    procedure UpdateMainWindow(Alpha: Byte);
    procedure UpdateWindow;
    procedure SetPosition;
    procedure WndProc(var Message: TMessage); override;
    procedure HookWndProc(var Msg: TMessage);
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property KeyBoard: TAdvSmoothTouchKeyBoard read FKeyBoard write FKeyBoard;
  end;

  TAdvSmoothTouchKeyBoardForm = class(TForm)
  private
    OldWndProc, NewWndProc: Pointer;
    FMainBuffer: TGPBitmap;
    FKeyBoard: TAdvSmoothTouchKeyBoard;
    fKeyBoardItem: TAdvSmoothTouchKeyItem;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

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
    procedure UpdateMainWindow(Alpha: Byte);
    procedure UpdateWindow;
    procedure SetPosition;
    procedure WndProc(var Message: TMessage); override;
    procedure HookWndProc(var Msg: TMessage);
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property KeyBoard: TAdvSmoothTouchKeyBoard read FKeyBoard write FKeyBoard;
    property KeyBoardItem: TAdvSmoothTouchKeyItem read fKeyBoardItem write fKeyBoardItem;
  end;

  TAdvSmoothTouchSpecialKey = (skNone, skAlt, skAltGr, skShift, skCaps, skCtrl, skNum, skScroll,
                 skReturn, skAdd, skDivide, skDecimal, skSubstract, skMultiply,
                 skTab, skWin, skApp, skBackSpace, skSpaceBar, skLeft, skRight, skUp, skDown, skNext, skPrior, skHome, skEnd, skDelete, skEscape);

  TAdvSmoothTouchKeyEvent = procedure(Sender: TObject; Index: integer) of object;
  TAdvSmoothTouchQueryWindowHandleEvent = procedure(Sender: TObject; VAR hw: HWND) of object;

  {$IFNDEF DELPHI9_LVL}
  TSelection = record
    StartPos, EndPos: Integer;
  end;
  {$ENDIF}

  TAdvSmoothSubKeyItem = class(TCollectionItem)
  private
    FSubKeyCaptionRect: TGPRectF;
    FSubKeyRect: TGPRectF;
    FOwner: TAdvSmoothTouchKeyBoard;
    FCaption: String;
    procedure SetCaption(const Value: String);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write SetCaption;
  end;

  TAdvSmoothSubKeyCollection = class(TCollection)
  private
    FOwner: TAdvSmoothTouchKeyBoard;
    function GetItem(Index: Integer): TAdvSmoothSubKeyItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothSubKeyItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothTouchKeyBoard);
    function Add: TAdvSmoothSubKeyItem;
    function Insert(index:integer): TAdvSmoothSubKeyItem;
    property Items[Index: Integer]: TAdvSmoothSubKeyItem read GetItem write SetItem; default;
  end;

{TAdvSmoothTouchKeyItem}

  TAdvSmoothTouchKeyItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothTouchKeyBoard;
    FOldW, FOldH, FOldX, FOldY: Double;
    FKeyNormalPosition: Boolean;
    FDownState: integer;
    FAltGrCaption: string;
    FTextColor: TColor;
    FAltGrKeyValue: Integer;
     FWidth: Integer;
    FColor: TColor;
    FCaption: string;
    FX: integer;
    FY: integer;
    FShortCut: string;
    FShiftCaption: string;
    FTextColorDown: TColor;
    FKeyValue: Integer;
    FSpecialKey: TAdvSmoothTouchSpecialKey;
    fShiftKeyValue: Integer;
    FHeight: Integer;
    FColorDown: TColor;
    FSubKeys: TAdvSmoothSubKeyCollection;
    FImage: TAdvGDIPPicture;
    FImageName: String;
    FImageIndex: Integer;
    FShowBackGround: Boolean;
    FImageIndexDown: Integer;
    FImageDown: TAdvGDIPPicture;
     FImageNameDown: String;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FKeyCombination: Boolean;
    FShiftKeyCombination: Boolean;
    FAltGrKeyCombination: Boolean;
    procedure SetAltGrCaption(const Value: string);
    procedure SetAltGrKeyValue(const Value: Integer);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetHeight(const Value: Integer);
    procedure SetKeyvalue(const Value: Integer);
    procedure SetpecialKey(const Value: TAdvSmoothTouchSpecialKey);
    procedure SetShiftCaption(const Value: string);
    procedure SetShiftKeyValue(const Value: Integer);
    procedure SetShortCut(const Value: string);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextColorDown(const Value: TColor);
    procedure SetWidth(const Value: Integer);
     procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetSubKeys(const Value: TAdvSmoothSubKeyCollection);
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageName(const Value: String);
    procedure SetShowBackGround(const Value: Boolean);
    procedure SetDownState(const Value: boolean);
    function GetDownState: boolean;
    procedure SetImageDown(const Value: TAdvGDIPPicture);
    procedure SetImageIndexDown(const Value: Integer);
    procedure SetImageNameDown(const Value: String);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageWidth(const Value: Integer);
    procedure SetAltGrKeyCombination(const Value: Boolean);
    procedure SetKeyCombination(const Value: Boolean);
    procedure SetShiftKeyCombination(const Value: Boolean);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
     function GetDisplayName: string; override;
    procedure Draw(g: TGPGraphics; r: TGPRectF; f, fs: TGPFont; sf, sfs: TGPStringFormat);
    procedure DrawBG(g: TGPGraphics; r: TGPRectF; c: TColor);
    function GetWidthSubKeys: Double;
    function GetHeightSubKeys: Double;
    procedure InitKeys(StartX, StartY: integer);
    function XYToSubKey(X, Y: integer): integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Down: boolean read GetDownState write SetDownState;
  published
    property KeyCombination: Boolean read FKeyCombination write SetKeyCombination default False;
    property ShiftKeyCombination: Boolean read FShiftKeyCombination write SetShiftKeyCombination default False;
    property AltGrKeyCombination: Boolean read FAltGrKeyCombination write SetAltGrKeyCombination default False;
    property Caption: string read FCaption write SetCaption;
    property ShiftCaption: string read FShiftCaption write SetShiftCaption;
    property AltGrCaption: string read FAltGrCaption write SetAltGrCaption;
    property KeyValue: Integer read FKeyValue write SetKeyvalue;
     property ShiftKeyValue: Integer read fShiftKeyValue write SetShiftKeyValue;
    property AltGrKeyValue: Integer read FAltGrKeyValue write SetAltGrKeyValue;
    property SpecialKey : TAdvSmoothTouchSpecialKey read FSpecialKey write SetpecialKey;
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorDown : TColor read FColorDown write SetColorDown default clGray;
    property TextColor : TColor read FTextColor write SetTextColor default clBlack;
    property TextColorDown : TColor read FTextColorDown write SetTextColorDown default clBlack;
    property ShortCut: string read FShortCut write SetShortCut;
    property X: integer read FX write SetX default -1;
    property Y: integer read FY write SetY default -1;
    property Height : Integer read FHeight write SetHeight default 40;
    property Width : Integer read FWidth write SetWidth default 40;
    property SubKeys: TAdvSmoothSubKeyCollection read FSubKeys write SetSubKeys;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageName: String read FImageName write SetImageName;
    property Image: TAdvGDIPPicture read FImage write SetImage;

    property ImageIndexDown: Integer read FImageIndexDown write SetImageIndexDown default -1;
    property ImageNameDown: String read FImageNameDown write SetImageNameDown;
    property ImageDown: TAdvGDIPPicture read FImageDown write SetImageDown;

     property ImageWidth: Integer read FImageWidth write SetImageWidth default -1;
    property ImageHeight: Integer read FImageHeight write SetImageHeight default -1;

    property ShowBackGround: Boolean read FShowBackGround write SetShowBackGround default True;
  end;

{TAdvSmoothTouchKeyCollection}

  TAdvSmoothTouchKeyCollection = class(TCollection)
  private
    FOwner: TAdvSmoothTouchKeyBoard;
    function GetItem(Index: Integer): TAdvSmoothTouchKeyItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothTouchKeyItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothTouchKeyBoard);
    function Add: TAdvSmoothTouchKeyItem;
    function Insert(index:integer): TAdvSmoothTouchKeyItem;
     property Items[Index: Integer]: TAdvSmoothTouchKeyItem read GetItem write SetItem; default;
  end;

  TAdvSmoothTouchKeyBoardType = (ktQWERTY, ktAZERTY, ktDVORAK, ktNUMERIC, ktCELLPHONE, ktQWERTZ, ktCustom);

  TAdvSmoothTouchKeyBoardDrawKeyEvent = procedure(Sender: TObject; Key: TAdvSmoothTouchKeyItem; Canvas: TCanvas;
                   Down: Boolean; Rect: TRect; var DefaultDraw: boolean) of Object;

  TAdvSmoothTouchKeyBoardCompletionMode = (cmNotVisible, cmAlwaysVisible, cmAutoVisible);

  TAdvSmoothTouchKeyboardTextChangedEvent = procedure(Sender: TObject; var Text: String) of object;

  TAdvSmoothTouchKeyBoardCompletion = class(TPersistent)
  private
    FLookup: String;
    FOwner: TAdvSmoothTouchKeyBoard;
    FCompletion: TAdvSmoothCompletion;
    FItems: TStringList;
    FMode: TAdvSmoothTouchKeyBoardCompletionMode;
    FLookupCaseSensitive: Boolean;
     FLookupFromChars: integer;
    FColor: TColor;
    FFont: TFont;
    FLookupSpacing: Integer;
    procedure SetItems(const Value: TStringList);
    procedure SetMode(const Value: TAdvSmoothTouchKeyBoardCompletionMode);
    procedure SetLookupCaseSensitive(const Value: Boolean);
    procedure SetLookupFromChars(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetLookupSpacing(const Value: Integer);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure CompletionChanged(Sender: TObject);
    procedure UpdateCompletion(Filter: String = '');
  public
    constructor Create(AOwner: TAdvSmoothTouchKeyBoard);
    destructor Destroy; override;
     procedure Assign(Source: TPersistent); override;
    procedure ApplySettings;
  published
    property LookupList: TStringList read FItems write SetItems;
    property LookupFromChars: integer read FLookupFromChars write SetLookupFromChars default 3;
    property LookupCaseSensitive: Boolean read FLookupCaseSensitive write SetLookupCaseSensitive default false;
    property Mode: TAdvSmoothTouchKeyBoardCompletionMode read FMode write SetMode default cmAutoVisible;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor;
    property LookupSpacing: Integer read FLookupSpacing write SetLookupSpacing default 30;
  end;

  TAutoSizeLayout = (aslKeys, aslKeyboard);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothTouchKeyBoard = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    {$IFDEF DELPHIXE_LVL}
     FTouchRegistered: Boolean;
    {$ENDIF}
    FComboKeyValue: String;
    FComboKey: Integer;
    FCombo: Boolean;
    FMetroStyle: Boolean;
    FEditControl: TEdit;
    FAutoCompletionForm: TAdvSmoothTouchKeyBoardAutoForm;
    FCachedBitmaps: array of TKeyCachedBitmap;
    FMouseUp, FMouseDown: Boolean;
    FSubKeyForm: TAdvSmoothTouchKeyBoardForm;
    FDownKey: TAdvSmoothTouchKeyItem;
    FSubKeyTime, FSubKeyCheck: integer;
    FTimer, FCheckTimer: TTimer;
    DoAutoZoom: Boolean;
    FOriginalW, FOriginalH: integer;
    FKeys: TAdvSmoothTouchKeyCollection;
    FOnKeyDown: TKeyEvent;
    FPictureNormalState: TBitmap;
    FPictureDownState: TBitmap;
     FSmallFont: TFont;
    FKeyboardType: TAdvSmoothTouchKeyBoardType;
    FShift: TShiftState;
    FAutoPostKey : Boolean;
    FHighlightCaps: TColor;
    FAutoCapsDisplay: Boolean;
    FHighlightAltGr: TColor;
    FRepeatTimer: TTimer;
    FRepeatTimerCount: integer;
    FRepeatItemIndex: integer;
    FOnKeyClick: TAdvSmoothTouchKeyEvent;
    FOnQueryWindowHandle: TAdvSmoothTouchQueryWindowHandleEvent;
    FKeyDistance: Integer;
    FOldW,FOldH: integer;
    FPostWMCharOnly: boolean;
    FCapsDown: boolean;
    FFill: TGDIPFill;
    FPictureContainer: TGDIPPictureContainer;
    FAutoCompletion: TAdvSmoothTouchKeyBoardCompletion;
    FSubKeyHidePause: integer;
     FSubKeyShowPause: integer;
    FImages: TImageList;
    FAllowAutoZoom: Boolean;
    FUseFocusedControl: Boolean;
    FOnTextChanged: TAdvSmoothTouchKeyboardTextChangedEvent;
    FText: String;
    FKeyDownOffset: Integer;
    FOnBeforeKeyClick: TNotifyEvent;
    FShadingOpacity: Byte;
    FShading: Boolean;
    FShadingOpacityTo: Byte;
    FShadingColor: TColor;
    FShadingColorTo: TColor;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetKeys(const Value: TAdvSmoothTouchKeyCollection);
    procedure SetSmallFont(const Value: TFont);
    procedure SetAdvSmoothTouchKeyBoardType(const Value: TAdvSmoothTouchKeyBoardType);
     procedure AddSubKey(Caption: String);
    procedure AddKey(Caption, ShiftCaption, AltGrCaption : ShortString;
        KeyValue, ShiftKeyValue, AltGrKeyValue, ImageIndex, Width, Height: Integer; var X : Integer; Y : Integer;
        SpecialKey : TAdvSmoothTouchSpecialKey; KeyCombination: Boolean = False; ShiftKeyCombination: Boolean = False; AltGrKeyCombination: Boolean = False; Color: TColor = clSilver);
    procedure NewRow(var X, Y : Integer; Size : Integer);
    procedure PostNormalKeys(Index: Integer);
    procedure PostAdvSmoothTouchSpecialKeys(Key: Word; const pShift: TShiftState; SpecialKey: Boolean);
    function GetKeybdInputHandle: HWND;
    procedure PictureChanged(Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetAutoCapsDisplay(const Value: Boolean);
    procedure SetHighlightAltGr(const Value: TColor);
    procedure SetHighlightCaps(const Value: TColor);
    procedure SetKeyDistance(Value : Integer);
    function IsCapsDown: boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetAutoPostKey(const Value: Boolean);
    procedure SetAutoCompletion(const Value: TAdvSmoothTouchKeyBoardCompletion);
     procedure SetSubKeyHidePause(const Value: integer);
    procedure SetSubKeyShowPause(const Value: integer);
    procedure SetAllowAutoZoom(const Value: Boolean);
    procedure SetShift(const Value: TShiftState);
    procedure SetUseFocusedControl(const Value: Boolean);
    procedure SetText(const Value: String);
    procedure SetShading(const Value: Boolean);
    procedure SetShadingColor(const Value: TColor);
    procedure SetShadingColorTo(const Value: TColor);
    procedure SetShadingOpacity(const Value: Byte);
    procedure SetShadingOpacityTo(const Value: Byte);
  protected
    function IsComboKey(AKey: String): Boolean;
    function GetComboKey(AKey: String): String;
    procedure TextChanged(Sender: TObject);
    function GetItemOnShift: Integer;
    function GetItemOnKey(AKey: Word): Integer;
    function GetItemOnKeyCaption(AKeyCaption: String): Integer;
    procedure Changed;
    procedure DoSubKey(Sender: TObject);
     procedure DoSubKeyCheck(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FillChanged(Sender: TOBject);
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawKeys(g: TGPGraphics);
    procedure ItemKeyDown(Index: Integer);
    procedure ItemKeyUp(Index: Integer);
    procedure SyncEqualKeys(Index: Integer);
    procedure TurnOffShifts;
    procedure BuildQWERTYKeyBoard;
    procedure BuildQWERTZKeyBoard;
    procedure BuildAZERTYKeyBoard;
    procedure BuildDVORAKKeyBoard;
    procedure BuildNumericKeyBoard;
    procedure BuildCellPhoneKeyboard;
    property KeybdInputHandle : HWND read GetKeybdInputHandle;
    procedure Paint; override;
    procedure StartTimer(index: integer);
    procedure StopTimer;
    procedure RepeatTimerProc(Sender: TObject);
    procedure ShowForm(Key: TAdvSmoothTouchKeyItem);
    procedure ShowAutoCompletionForm;
    procedure UpdateAutoCompletionFormPosition;
    function CheckAutoCompletionKey(Key: TAdvSmoothTouchKeyItem): Boolean;
    function CheckAutoCompletionSubKey(SubKey: TAdvSmoothSubKeyItem): Boolean;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DestroyWnd; override;
    procedure Assign(Source: TPersistent); override;
    procedure AutoZoom;
    procedure Zoom(fhorz,fvert: double; keysonly: boolean = false; absoluteZoom : boolean = false);
    procedure SaveKeybdLayout(FileName: string);
    procedure LoadKeybdLayout(FileName: string; AutoSize: TAutoSizeLayout = aslKeyboard);
    procedure PostKey(Key: Word; const pShift: TShiftState; ExtendedKeyBD: Boolean; Index: Integer);
    property PostWMCharOnly: Boolean read FPostWMCharOnly write FPostWMCharOnly;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    function XYtoItem(X, Y: integer): integer;
    property  Shift : TShiftState read FShift write SetShift;
    procedure SetKey(AKey: Word; AKeyCaption: String = ''; pShift: TShiftState = []);
  published
    property KeyDownOffset: Integer read FKeyDownOffset write FKeyDownOffset default 1;
    property OnTextChanged: TAdvSmoothTouchKeyboardTextChangedEvent read FOnTextChanged write FOnTextChanged;
    property UseFocusedControl: Boolean read FUseFocusedControl write SetUseFocusedControl default True;
    property Text: String read FText write SetText;
    property AllowAutoZoom: Boolean read FAllowAutoZoom write SetAllowAutoZoom default True;
    property AutoCompletion: TAdvSmoothTouchKeyBoardCompletion read FAutoCompletion write SetAutoCompletion;
    property SubKeyShowPause: integer read FSubKeyShowPause write SetSubKeyShowPause default 1;
    property SubKeyHidePause: integer read FSubKeyHidePause write SetSubKeyHidePause default 5;
    property Fill: TGDIPFill read FFill write SetFill;
    property AutoPostKey : Boolean read FAutoPostKey write SetAutoPostKey default true;
    property AutoCapsDisplay: Boolean read FAutoCapsDisplay write SetAutoCapsDisplay default false;
    property Shading: Boolean read FShading write SetShading default True;
    property ShadingColor: TColor read FShadingColor write SetShadingColor default clWhite;
    property ShadingOpacity: Byte read FShadingOpacity write SetShadingOpacity default 10;
    property ShadingColorTo: TColor read FShadingColorTo write SetShadingColorTo default clWhite;
    property ShadingOpacityTo: Byte read FShadingOpacityTo write SetShadingOpacityTo default 150;    
    property HighlightCaps: TColor read FHighlightCaps write SetHighlightCaps default clNone;
    property HighlightAltGr: TColor read FHighlightAltGr write SetHighlightAltGr default clNone;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write FPictureContainer;
    property Images: TImageList read FImages write FImages;
    property KeyboardType : TAdvSmoothTouchKeyBoardType read FKeyboardType write SeTAdvSmoothTouchKeyBoardType;
    property KeyDistance : Integer Read FKeyDistance Write SetKeyDistance default 0;
    property Keys : TAdvSmoothTouchKeyCollection read FKeys write SetKeys;
    property SmallFont : TFont read FSmallFont write SetSmallFont;
    property Version: string read GetVersion write SetVersion;
    // Events
    property OnKeyClick: TAdvSmoothTouchKeyEvent read FOnKeyClick write FOnKeyClick;
    property OnKeyDown : TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnQueryWindowHandle: TAdvSmoothTouchQueryWindowHandleEvent read FOnQueryWindowHandle write FOnQueryWindowHandle;
    property OnBeforeKeyClick: TNotifyEvent read FOnBeforeKeyClick write FOnBeforeKeyClick;

    property Align;
    property Font;
    property Visible;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

  TAdvSmoothTouchKeyBoardToolForm = class(TCustomForm)
  private
    FShowCaption: boolean;
    FShowClose: boolean;
    FKeyboard: TAdvSmoothTouchKeyBoard;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMMove(var Message: TMessage); message WM_MOVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    destructor Destroy; override;
  published
    property ShowClose: boolean read FShowClose write FShowClose;
    property ShowCaption: boolean read FShowCaption write FShowCaption;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothPopupTouchKeyBoard = class(TComponent, ITMSStyle, ITMSTones)
  private
    FTimer: TTimer;
    FFrm : TAdvSmoothTouchKeyBoardToolForm;
    FKbd : TAdvSmoothTouchKeyBoard;
    FOwnerform: TCustomForm;
    FShowCaption: boolean;
    FShowClose: boolean;
    FDisableSizing: boolean;
    FOnClose: TNotifyEvent;
    FBackGroundColor: TColor;
    FOnShow: TNotifyEvent;
    FOnKeyboardCreated: TNotifyEvent;
    FLastX,FLastY: integer;
    FOnTextChanged: TAdvSmoothTouchKeyboardTextChangedEvent;
    FAutoHide: Boolean;
    FAutoFollowFocus: Boolean;
    FOnBeforeKeyClick: TNotifyEvent;
    FOnQueryWindowHandle: TAdvSmoothTouchQueryWindowHandleEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyClick: TAdvSmoothTouchKeyEvent;
    procedure SetAutoCapsDisplay(const Value: Boolean);
    procedure SetAutoPostKey(const Value: Boolean);
    procedure SetHighlightAltGr(const Value: TColor);
    procedure SetHighlightCaps(const Value: TColor);
    procedure SeTAdvSmoothTouchKeyBoardType(const Value: TAdvSmoothTouchKeyBoardType);
    procedure KeyboardSizeChanged(Sender: TObject);
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetUseFocusedControl(const Value: Boolean);
    procedure SetKeyboardHeight(const Value: integer);
    procedure SetKeyboardWidth(const Value: integer);
    procedure SetAutoCompletion(const Value: TAdvSmoothTouchKeyBoardCompletion);
    function GetAutoCapsDisplay: Boolean;
    function GetAutoCompletion: TAdvSmoothTouchKeyBoardCompletion;
    function GetAutoPostKey: Boolean;
    function GetHighlightAltGr: TColor;
    function GetHighlightCaps: TColor;
    function GetKeyboardHeight: integer;
    function GetKeyboardType: TAdvSmoothTouchKeyBoardType;
    function GetKeyboardWidth: integer;
    function GetUseFocusedControl: Boolean;
    function GetShading: Boolean;
    function GetShadingColor: TColor;
    function GetShadingColorTo: TColor;
    function GetShadingOpacity: Byte;
    function GetShadingOpacityTo: Byte;
    procedure SetShading(const Value: Boolean);
    procedure SetShadingColor(const Value: TColor);
    procedure SetShadingColorTo(const Value: TColor);
    procedure SetShadingOpacity(const Value: Byte);
    procedure SetShadingOpacityTo(const Value: Byte);
    function GetFill: TGDIPFill;
    function GetImages: TImageList;
    function GetKeyDistance: Integer;
    function GetKeyDownOffset: Integer;
    function GetKeys: TAdvSmoothTouchKeyCollection;
    function GetPictureContainer: TGDIPPictureContainer;
    function GetSmallFont: TFont;
    function GetSubKeyHidePause: integer;
    function GetSubKeyShowPause: integer;
    function GetText: String;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetImages(const Value: TImageList);
    procedure SetKeyDistance(const Value: Integer);
    procedure SetKeys(const Value: TAdvSmoothTouchKeyCollection);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetSmallFont(const Value: TFont);
    procedure SetSubKeyHidePause(const Value: integer);
    procedure SetSubKeyShowPause(const Value: integer);
    procedure SetText(const Value: String);
    procedure SetKeyDownOffset(const Value: Integer);
  protected
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure TimerProc(Sender: TObject);
    procedure TextChanged(Sender: TObject; var Text: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateForm; virtual;
    procedure Show;
    procedure ShowAtXY(x,y: integer); overload;
    procedure ShowAtXY(AParent: TCustomForm; x,y: integer); overload;
    procedure Hide;
    property Keyboard: TAdvSmoothTouchKeyBoard read FKbd;
  published
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default clBtnFace;
    property ShowCaption: boolean read FShowCaption write FShowCaption default true;
    property ShowClose: boolean read FShowClose write FShowClose default true;
    property Shading: Boolean read GetShading write SetShading default True;
    property ShadingColor: TColor read GetShadingColor write SetShadingColor default clWhite;
    property ShadingOpacity: Byte read GetShadingOpacity write SetShadingOpacity default 10;
    property ShadingColorTo: TColor read GetShadingColorTo write SetShadingColorTo default clWhite;
    property ShadingOpacityTo: Byte read GetShadingOpacityTo write SetShadingOpacityTo default 150;
    property AutoCompletion: TAdvSmoothTouchKeyBoardCompletion read GetAutoCompletion write SetAutoCompletion;
    property AutoFollowFocus: Boolean read FAutoFollowFocus write FAutoFollowFocus default false;
    property AutoHide: Boolean read FAutoHide write FAutoHide default false;
    property AutoPostKey : Boolean read GetAutoPostKey write SetAutoPostKey default true;
    property AutoCapsDisplay: Boolean read GetAutoCapsDisplay write SetAutoCapsDisplay default false;
    property HighlightCaps: TColor read GetHighlightCaps write SetHighlightCaps default clNone;
    property HighlightAltGr: TColor read GetHighlightAltGr write SetHighlightAltGr default clNone;
    property KeyboardType : TAdvSmoothTouchKeyBoardType read GetKeyboardType write SeTAdvSmoothTouchKeyBoardType;
    property KeyboardWidth: integer read GetKeyboardWidth write SetKeyboardWidth default 602;
    property KeyboardHeight: integer read GetKeyboardHeight write SetKeyboardHeight default 202;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnKeyboardCreated: TNotifyEvent read FOnKeyboardCreated write FOnKeyboardCreated;
    property OnTextChanged: TAdvSmoothTouchKeyboardTextChangedEvent read FOnTextChanged write FOnTextChanged;
    property UseFocusedControl: Boolean read GetUseFocusedControl write SetUseFocusedControl default True;

    property KeyDownOffset: Integer read GetKeyDownOffset write SetKeyDownOffset default 1;
    property Text: String read GetText write SetText;
    property SubKeyShowPause: integer read GetSubKeyShowPause write SetSubKeyShowPause default 1;
    property SubKeyHidePause: integer read GetSubKeyHidePause write SetSubKeyHidePause default 5;
    property Fill: TGDIPFill read GetFill write SetFill;
    property PictureContainer: TGDIPPictureContainer read GetPictureContainer write SetPictureContainer;
    property Images: TImageList read GetImages write SetImages;
    property KeyDistance: Integer Read GetKeyDistance Write SetKeyDistance default 0;
    property Keys: TAdvSmoothTouchKeyCollection read GetKeys write SetKeys;
    property SmallFont: TFont read GetSmallFont write SetSmallFont;
    // Events
    property OnKeyClick: TAdvSmoothTouchKeyEvent read FOnKeyClick write FOnKeyClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnQueryWindowHandle: TAdvSmoothTouchQueryWindowHandleEvent read FOnQueryWindowHandle write FOnQueryWindowHandle;
    property OnBeforeKeyClick: TNotifyEvent read FOnBeforeKeyClick write FOnBeforeKeyClick;
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


{$R AdvSmoothTouchKeyBoard.res}

function GetWindowsVersion: Double;
var
  OSVersionInfo: TOSVersionInfo;
begin
  Result := 0;
  OSVersionInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
  begin
    Result := OSVersionInfo.dwMajorVersion + OSVersionInfo.dwMinorVersion / 10;
  end;
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

procedure GetPath(l, t, w, h, d, radius: Double; RoundingType: TFillRoundingType; var path: TGPGraphicsPath); overload;
begin
  case RoundingType of
    rtNone:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t); // left
    end;
    rtRight:
    begin
      path.AddLine(l, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtLeft:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtTop:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t + Radius); // left
    end;
    rtBottom:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - Radius, l, t ); // left
    end;
    rtBoth:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
  end;
end;

function CreateRoundRectangle(R: TGPRectF; Radius: Integer; RoundingType: TFillRoundingType): TGPGraphicsPath; overload;
var
  l, t, w, h, d: Double;
begin
  Result := TGPGraphicsPath.Create;
  l := R.X;
  t := R.Y;
  w := R.Width;
  h := R.Height;
  d := Radius shl 1;
  GetPath(l, t, w, h, d, radius, RoundingType, Result);
  Result.CloseFigure();
end;

function AnimateDouble(var Start: integer; Stop: integer; Delta: Double; Margin: integer): Boolean;
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
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

procedure GetPath(l, t, w, h, d, radius: Double; var path: TGPGraphicsPath); overload;
begin
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddLine(l + radius, t, l + w - radius, t); // top
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddLine(l + w - radius, t + h, l, t + h); // bottom
  path.AddLine(l, t + h, l, t + radius); // left
end;

function CreateRoundRectangle(R: TGPRectF; Radius: Integer): TGPGraphicsPath; overload;
var
  l, t, w, h, d: Double;
begin
  Result := TGPGraphicsPath.Create;
  l := R.X;
  t := R.Y;
  w := R.Width;
  h := R.Height;
  d := Radius shl 1;
  GetPath(l, t, w, h, d, radius, Result);
  Result.CloseFigure();
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
    ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

function IsComboBox(hwnd: THandle): boolean;
var
  ClassName: array[0..255] of char;
begin
  GetClassName(hwnd, ClassName, SizeOf(ClassName));

  if Pos('EDIT',AnsiUppercase(StrPas(ClassName))) >  0 then
  begin
    GetClassName(GetParent(hwnd), ClassName, SizeOf(ClassName));
  end;

  Result := Pos('COMBO',AnsiUppercase(StrPas(ClassName))) >  0;
end;

{ TAdvSmoothTouchKeyBoard }

procedure TAdvSmoothTouchKeyBoard.AddKey(Caption, ShiftCaption,
  AltGrCaption: ShortString; KeyValue, ShiftKeyValue, AltGrKeyValue,
  ImageIndex, Width, Height: Integer; var X :Integer; Y: Integer;
  SpecialKey: TAdvSmoothTouchSpecialKey; KeyCombination: Boolean = False; ShiftKeyCombination: Boolean = False; AltGrKeyCombination: Boolean = False; Color: TColor = clSilver);
begin
  with FKeys do
  begin
    Add;
    Items[FKeys.Count - 1].Caption := string(Caption);
    Items[FKeys.Count - 1].ShiftCaption := string(ShiftCaption);
    Items[FKeys.Count - 1].AltGrCaption := string(AltGrCaption);
    Items[FKeys.Count - 1].KeyValue := KeyValue;
    Items[FKeys.Count - 1].ShiftKeyValue := ShiftKeyValue;
    Items[FKeys.Count - 1].AltGrKeyValue := AltGrKeyValue;
    Items[FKeys.Count - 1].SpecialKey := SpecialKey;
    Items[FKeys.Count - 1].KeyCombination := KeyCombination;
    Items[FKeys.Count - 1].ShiftKeyCombination := ShiftKeyCombination;
    Items[FKeys.Count - 1].AltGrKeyCombination := AltGrKeyCombination;
    Items[FKeys.Count - 1].Width := Width;
    Items[FKeys.Count - 1].Height := Height;
    Items[FKeys.Count - 1].X := X;
    Items[FKeys.Count - 1].Y := Y;
    Items[FKeys.Count - 1].FOldW := -1;
    Items[FKeys.Count - 1].FOldH := -1;
    Items[FKeys.Count - 1].FOldX := -1;
    Items[FKeys.Count - 1].FOldY := -1;
    Items[FKeys.Count - 1].Color := Color;
    X := X + Items[FKeys.Count - 1].Width;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.Changed;
begin
  Invalidate;
end;

function TAdvSmoothTouchKeyBoard.CheckAutoCompletionKey(
  Key: TAdvSmoothTouchKeyItem): Boolean;
var
  str: String;
  I: Integer;
  asc, ascex: Boolean;
begin
  Result := false;
  str := Key.Caption;
  for I := 1 to Length(str) do
  begin
    asc := (Ord(str[i]) >= 65) and (Ord(str[i]) <= 90);
    ascex := (Ord(str[i]) >= 192) and (Ord(str[i]) < 255);
    result := asc or ascex;
    //Special Keys . @ : \ / - _ (URL)
    result := result or (Ord(str[i]) = Ord('@'));
    result := result or (Ord(str[i]) = Ord('.'));
    result := result or (Ord(str[i]) = Ord('/'));
    result := result or (Ord(str[i]) = Ord('\'));
    result := result or (Ord(str[i]) = Ord('-'));
    result := result or (Ord(str[i]) = Ord('_'));
    result := result or (Ord(str[i]) = Ord(':'));
  end;
end;

function TAdvSmoothTouchKeyBoard.CheckAutoCompletionSubKey(
  SubKey: TAdvSmoothSubKeyItem): Boolean;
var
  str: String;
  I: Integer;
  asc, ascex: Boolean;
begin
  Result := false;
  str := SubKey.Caption;
  for I := 1 to Length(str) do
  begin
    asc := (Ord(str[i]) >= 65) and (Ord(str[i]) <= 90);
    ascex := (Ord(str[i]) >= 192) and (Ord(str[i]) < 255);
    result := asc or ascex;
    //Special Keys . @ : \ / - _ (URL)
    result := result or (Ord(str[i]) = Ord('@'));
    result := result or (Ord(str[i]) = Ord('.'));
    result := result or (Ord(str[i]) = Ord('/'));
    result := result or (Ord(str[i]) = Ord('\'));
    result := result or (Ord(str[i]) = Ord('-'));
    result := result or (Ord(str[i]) = Ord('_'));
    result := result or (Ord(str[i]) = Ord(':'));
  end;
end;

procedure TAdvSmoothTouchKeyBoard.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TAdvSmoothTouchKeyBoard.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  Changed;
end;

constructor TAdvSmoothTouchKeyBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUseFocusedControl := True;
  FShadingOpacity := 10;
  FShadingOpacityTo := 150;
  FShadingColor := clWhite;
  FShadingColorTo := clWhite;
  FShading := True;  

  FKeyDownOffset := 1;
  DoubleBuffered := true;
  FPictureNormalState := TBitmap.Create;
  FPictureDownState := TBitmap.Create;

  FEditControl := TEdit.Create(Self);
  FEditControl.Parent := Self;
  FEditControl.Width := 0;
  FEditControl.OnChange := TextChanged;

  FPictureDownState.OnChange := PictureChanged;
  FPictureNormalState.OnChange := PictureChanged;

  if (GetKeyState(VK_CAPITAL) = 1) then
  begin
    FShift := [ssShift];
    FCapsDown := true;
  end
  else
  begin
    FShift := [];
    FCapsDown := false;
  end;

  FKeys := TAdvSmoothTouchKeyCollection.Create(self);
  FSmallFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FSmallFont.Name := 'Tahoma';
  {$ENDIF}
  FAllowAutoZoom := True;
  DoAutoZoom := false;
  Height := 200;
  Width  := 160;
  DoAutoZoom := true;
  FAutoPostKey := True;
  FHighlightCaps := clNone;
  FHighlightAltGr := clNone;
  Font.Size := 8;
  {$IFNDEF DELPHI9_LVL}
  Font.Name := 'Tahoma';
  {$ENDIF}
  FSmallFont.Size := 7;
  FKeyDistance := 0;
  KeyboardType := ktQWERTY;
  FRepeatTimer := TTimer.Create(self);
  FRepeatTimer.Enabled := false;
  FRepeatTimer.OnTimer := RepeatTimerProc;
  FOldW := -1;
  FOldH := -1;
  FFill := TGDIPFill.Create;
  Ffill.OnChange := FillChanged;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 1000;
  FTimer.OnTimer := DoSubKey;
  FCheckTimer := TTimer.Create(Self);
  FCheckTimer.Enabled := false;
  FCheckTimer.Interval := 1000;
  FCheckTimer.OnTimer := DoSubKeyCheck;
  FAutoCompletion := TAdvSmoothTouchKeyBoardCompletion.Create(Self);
  FSubKeyShowPause := 1;
  FSubKeyHidePause := 5;
end;

procedure TAdvSmoothTouchKeyBoard.CreateWnd;
begin
  inherited;
  FOriginalW := Width;
  FOriginalH := Height;
  AutoZoom;

  {$IFDEF DELPHIXE_LVL}
  if GetWindowsVersion > 6 then
  begin
    FTouchRegistered := RegisterTouchWindow(Handle, 0);
  end;
  {$ENDIF}
end;

destructor TAdvSmoothTouchKeyBoard.Destroy;
var
  i: integer;
begin

  for i := 0 to Length(FCachedBitmaps) - 1 do
  begin
    FCachedBitmaps[i].CachedBitmap.Free;
  end;

  FEditControl.Free;
  FFill.Free;
  FKeys.Free;
  FPictureNormalState.Free;
  FPictureDownState.Free;
  FSmallFont.Free;
  FRepeatTimer.Free;
  FTimer.Free;
  FCheckTimer.Free;
  FAutoCompletion.Free;
  inherited;
end;

procedure TAdvSmoothTouchKeyBoard.DestroyWnd;
begin
  {$IFDEF DELPHIXE_LVL}
  if GetWindowsVersion > 6 then
  begin
    if FTouchRegistered then
      UnregisterTouchWindow(Handle);
  end;
  {$ENDIF}
  inherited;
end;

procedure TAdvSmoothTouchKeyBoard.DoSubKey(Sender: TObject);
begin
  if (FSubKeyTime >= SubKeyShowPause) and not Assigned(FSubKeyForm) then
  begin
    if Assigned(FDownKey) then
    begin
      if FDownKey.SubKeys.Count > 0 then
      begin
        ShowForm(FDownKey);
        FCheckTimer.Enabled := true;
        FSubKeyCheck := 0;
      end;
    end;
  end;

  Inc(FSubKeyTime);
end;

procedure TAdvSmoothTouchKeyBoard.DoSubKeyCheck(Sender: TObject);
begin
  if (FSubKeyCheck >= SubKeyHidePause) and Assigned(FSubKeyForm) then
  begin
    FCheckTimer.Enabled := false;
    FSubKeyForm.Free;
    FSubKeyForm := nil;
    FSubKeyCheck := 0;
    FMouseUp := false;
    if Assigned(FDownKey) then
    begin
      FDownKey.FKeyNormalPosition := true;
      FDownKey := nil;
      Changed;
    end;
  end;
  if not FMouseDown then
    Inc(FSubKeyCheck);
end;

procedure TAdvSmoothTouchKeyBoard.DrawBackGround(g: TGPGraphics);
begin
  FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1))
end;

procedure TAdvSmoothTouchKeyBoard.DrawKeys(g: TGPGraphics);
var
  I: Integer;
  ff, ffs: TGPFontFamily;
  f, fs: TGPFont;
  sf, sfs: TGPStringFormat;
  fsl, fss: integer;
  r: TGPRectF;
begin
  ff := TGPFontFamily.Create(Font.Name);
  if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ff.Free;
    ff := TGPFontFamily.Create('Arial');
  end;

  fsl := 0;
  if (fsBold in Font.Style) then
    fsl := fsl + 1;
  if (fsItalic in Font.Style) then
    fsl := fsl + 2;
  if (fsUnderline in Font.Style) then
    fsl := fsl + 4;

  sf := TGPStringFormat.Create(0);
  f := TGPFont.Create(ff, Font.Size , fsl, UnitPoint);


  ffs := TGPFontFamily.Create(SmallFont.Name);
  if (ffs.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ffs.Free;
    ffs := TGPFontFamily.Create('Arial');
  end;

  fss := 0;
  if (fsBold in SmallFont.Style) then
    fss := fss + 1;
  if (fsItalic in SmallFont.Style) then
    fss := fss + 2;
  if (fsUnderline in SmallFont.Style) then
    fss := fss + 4;

  sfs := TGPStringFormat.Create(0);
  fs := TGPFont.Create(ffs, SmallFont.Size , fss, UnitPoint);

  for I := 0 to Keys.Count - 1 do
  begin
    r := MakeRect(Keys[I].X, Keys[I].Y, Keys[I].Width, Keys[I].Height);
    Keys[I].Draw(g, r, f, fs, sf, sfs);
  end;

  f.Free;
  sf.Free;
  ff.Free;
  fs.Free;
  sfs.Free;
  ffs.Free;
end;

procedure TAdvSmoothTouchKeyBoard.FillChanged(Sender: TOBject);
begin
  Changed;
end;

function TAdvSmoothTouchKeyBoard.GetComboKey(AKey: String): String;
begin
  Result := AKey;
  if FComboKeyValue = '^' then
  begin
    if AKey = 'a' then
      Result := 'â'
    else if AKey = 'e' then
      Result := 'ê'
    else if AKey = 'o' then
      Result := 'ô'
    else if AKey = 'i' then
      Result := 'î'
    else if AKey = 'u' then
      Result := 'û'
    else if AKey = 'A' then
      Result := 'Â'
    else if AKey = 'E' then
      Result := 'Ê'
    else if AKey = 'O' then
      Result := 'Ô'
    else if AKey = 'I' then
      Result := 'Î'
    else if AKey = 'U' then
      Result := 'Û'
  end
  else if FComboKeyValue = '¨' then
  begin
    if AKey = 'a' then
      Result := 'ä'
    else if AKey = 'e' then
      Result := 'ë'
    else if AKey = 'o' then
      Result := 'ö'
    else if AKey = 'i' then
      Result := 'ï'
    else if AKey = 'u' then
      Result := 'ü'
    else if AKey = 'A' then
      Result := 'Ä'
    else if AKey = 'E' then
      Result := 'Ë'
    else if AKey = 'O' then
      Result := 'Ö'
    else if AKey = 'I' then
      Result := 'Ï'
    else if AKey = 'U' then
      Result := 'Ü'
  end
  else if FComboKeyValue = '´' then
  begin
    if AKey = 'a' then
      Result := 'á'
    else if AKey = 'e' then
      Result := 'é'
    else if AKey = 'o' then
      Result := 'ó'
    else if AKey = 'i' then
      Result := 'í'
    else if AKey = 'u' then
      Result := 'ú'
    else if AKey = 'A' then
      Result := 'Á'
    else if AKey = 'E' then
      Result := 'É'
    else if AKey = 'O' then
      Result := 'Ó'
    else if AKey = 'I' then
      Result := 'Í'
    else if AKey = 'U' then
      Result := 'Ú'
  end
  else if FComboKeyValue = '`' then
  begin
    if AKey = 'a' then
      Result := 'à'
    else if AKey = 'e' then
      Result := 'è'
    else if AKey = 'o' then
      Result := 'ò'
    else if AKey = 'i' then
      Result := 'ì'
    else if AKey = 'u' then
      Result := 'ù'
    else if AKey = 'A' then
      Result := 'À'
    else if AKey = 'E' then
      Result := 'È'
    else if AKey = 'O' then
      Result := 'Ò'
    else if AKey = 'I' then
      Result := 'Ì'
    else if AKey = 'U' then
      Result := 'Ù'
  end
  else if FComboKeyValue = '~' then
  begin
    if AKey = 'a' then
      Result := 'ã'
    else if AKey = 'n' then
      Result := 'ñ'
    else if AKey = 'o' then
      Result := 'õ'
    else if AKey = 'A' then
      Result := 'Ã'
    else if AKey = 'N' then
      Result := 'Ñ'
    else if AKey = 'O' then
      Result := 'Õ'
  end
end;

function TAdvSmoothTouchKeyBoard.GetItemOnKey(AKey: Word): Integer;
var
  I: Integer;
  chk: Boolean;
begin
  Result := -1;
  for I := 0 to Keys.Count - 1 do
  begin
    chk := (Keys[i].KeyValue = AKey);
    if chk then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvSmoothTouchKeyBoard.GetItemOnKeyCaption(
  AKeyCaption: String): Integer;
var
  I: Integer;
  chk: Boolean;
begin
  Result := -1;
  for I := 0 to Keys.Count - 1 do
  begin
    chk := (Keys[i].Caption = AKeyCaption);
    if chk then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvSmoothTouchKeyBoard.GetItemOnShift: Integer;
var
  I: Integer;
  chk: Boolean;
begin
  Result := -1;
  for I := 0 to Keys.Count - 1 do
  begin
    chk := (Keys[i].SpecialKey = skShift);
    if chk then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvSmoothTouchKeyBoard.GetKeybdInputHandle: HWND;
var
 hw : HWND;
begin
  if UseFocusedControl then
    hw := GetFocus
  else
    hw := FEditControl.Handle;

  if Assigned(FOnQueryWindowHandle) then
    FOnQueryWindowHandle(Self, hw);

  Result := hw;
end;

function TAdvSmoothTouchKeyBoard.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothTouchKeyBoard.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothTouchKeyBoard.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSmoothTouchKeyBoard.AddSubKey(Caption: String);
begin
  Keys[Keys.Count - 1].SubKeys.Add.Caption := Caption;
end;

procedure TAdvSmoothTouchKeyBoard.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTouchKeyBoard) then
  begin
    FShading := (Source as TAdvSmoothTouchKeyBoard).Shading;
    FShadingColor := (Source as TAdvSmoothTouchKeyBoard).ShadingColor;
    FShadingColorTo := (Source as TAdvSmoothTouchKeyBoard).ShadingColorTo;
    FShadingOpacity := (Source as TAdvSmoothTouchKeyBoard).ShadingOpacity;
    FShadingOpacityTo := (Source as TAdvSmoothTouchKeyBoard).ShadingOpacityTo;  
    FUseFocusedControl := (Source as TAdvSmoothTouchKeyBoard).UseFocusedControl;
    FAutoCompletion.Assign((Source as TAdvSmoothTouchKeyBoard).AutoCompletion);
    FSubKeyShowPause := (Source as TAdvSmoothTouchKeyBoard).SubKeyShowPause;
    FSubKeyHidePause := (Source as TAdvSmoothTouchKeyBoard).SubKeyHidePause;
    FFill.Assign((Source as TAdvSmoothTouchKeyBoard).Fill);
    FAutoPostKey := (Source as TAdvSmoothTouchKeyBoard).AutoPostKey;
    FAutoCapsDisplay := (Source as TAdvSmoothTouchKeyBoard).AutoCapsDisplay;
    FHighlightCaps := (Source as TAdvSmoothTouchKeyBoard).HighlightCaps;
    FHighlightAltGr := (Source as TAdvSmoothTouchKeyBoard).HighlightAltGr;
    FKeyboardType := (Source as TAdvSmoothTouchKeyBoard).KeyboardType;
    FKeyDistance := (Source as TAdvSmoothTouchKeyBoard).KeyDistance;
    FKeys.Assign((Source as TAdvSmoothTouchKeyBoard).Keys);
    FSmallFont.Assign((Source as TAdvSmoothTouchKeyBoard).SmallFont);
    Changed;
  end;
end;

function SortKey(aKey1, aKey2: Pointer): Integer;
begin
  Result := 0;
  //sort on Y (horz)
  if TAdvSmoothTouchKeyItem(aKey1).Y > TAdvSmoothTouchKeyItem(aKey2).Y then
    Result := 1
  else if TAdvSmoothTouchKeyItem(aKey1).Y < TAdvSmoothTouchKeyItem(aKey2).Y then
    Result := -1
  //sort on X (vert)
  else if TAdvSmoothTouchKeyItem(aKey1).X > TAdvSmoothTouchKeyItem(aKey2).X then
    Result := 1
  else if TAdvSmoothTouchKeyItem(aKey1).X < TAdvSmoothTouchKeyItem(aKey2).X then
    Result := -1
end;

procedure TAdvSmoothTouchKeyBoard.AutoZoom;
var
  i: integer;
  fhorz: Double;
  fvert: Double;
  c: TKeyCachedBitmap;
  {
  l:TList;
  key1, key2: TAdvSmoothTouchKeyItem;
  iDiff: integer;
  }
begin
  if (FOriginalW = 0) or (FOriginalH = 0) then
    Exit;


  if not AllowAutoZoom then
    Exit;


  for I := 0 to Length(FCachedBitmaps) - 1 do
  begin
    c := FCachedBitmaps[I];
    if Assigned(c.CachedBitmap) then
    begin
      c.CachedBitmap.Free;
      c.CachedBitmap := nil;
    end;
    FCachedBitmaps[I] := c;
  end;
  SetLength(FCachedBitmaps, 0);


  fhorz := Width / FOriginalW;
  fvert := Height / FOriginalH;
  if (fhorz > 0) and (fvert >= 0) {and ((fhorz <> 1) or (fvert <> 1))} then
  begin
    for i := 0 to Keys.Count - 1 do
    begin
      if (Keys[i].FOldW < 0) or (Keys[i].FOldH < 0) then
      begin
        Keys[i].FOldW := Keys[i].width;
        Keys[i].FOldH := Keys[i].height;
        Keys[i].FOldX := Keys[i].X;
        Keys[i].FOldY := Keys[i].Y;
      end;
      Keys[i].width := Round(Keys[i].FOldW * fhorz);
      Keys[i].height := Round(Keys[i].FOldH * fvert);
      Keys[i].x := Round(Keys[i].FOldX * fhorz);
      Keys[i].y := Round(Keys[i].FOldY  * fvert);
    end;

    {
    l := TList.Create;
    try
      for i := 0 to Keys.Count - 1 do
        l.Add(Keys[i]);
      l.Sort(SortKey);

      for i := 0 to l.Count - 2 do
      begin
        key1 := TAdvSmoothTouchKeyItem(l.Items[i]);
        key2 := TAdvSmoothTouchKeyItem(l.Items[i+1]);

        if key1.Y = key2.Y then
        begin
          iDiff := (key1.X + key1.Width) - key2.X;
          if iDiff <> KeyDistance then
            key2.X := key2.X + iDiff;
        end;
      end;

    finally
      l.Free;
    end;
    }
  end;
end;

procedure TAdvSmoothTouchKeyBoard.BuildAZERTYKeyBoard;
var
  CurrentX,
  CurrentY,
  Size      : integer;

begin
  CurrentY := 1;
  CurrentX := CurrentY;
  Size := 40;
  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 202;
    Width  := 602;
    FOriginalW := 602;
    FOriginalH := 202;
  end;
  DoAutoZoom := true;

  with Keys do
  begin
  AddKey('²', '³','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('&', '1','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('é', '2','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('"', '3','#', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('''', '4','',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('(', '5','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('§', '6','^',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, false, true);
  AddKey('è', '7','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('!', '8','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('ç', '9','{', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('à', '0','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(')', 'º','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, False, False, False, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, False, False, False, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ä');
  AddSubKey('Â');
  AddSubKey('Á');
  AddSubKey('À');
  AddSubKey('ä');
  AddSubKey('â');
  AddSubKey('á');
  AddSubKey('à');
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','€', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ë');
  AddSubKey('Ê');
  AddSubKey('É');
  AddSubKey('È');
  AddSubKey('ë');
  AddSubKey('ê');
  AddSubKey('é');
  AddSubKey('è');
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('ÿ');
  AddSubKey('Ý');
  AddSubKey('ý');
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ü');
  AddSubKey('Û');
  AddSubKey('Ú');
  AddSubKey('Ù');
  AddSubKey('ü');
  AddSubKey('û');
  AddSubKey('ú');
  AddSubKey('ù');
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ï');
  AddSubKey('Î');
  AddSubKey('Í');
  AddSubKey('Ì');
  AddSubKey('ï');
  AddSubKey('î');
  AddSubKey('í');
  AddSubKey('ì');
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ö');
  AddSubKey('Ô');
  AddSubKey('Ó');
  AddSubKey('Ò');
  AddSubKey('ö');
  AddSubKey('ô');
  AddSubKey('ó');
  AddSubKey('ò');
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('^', '¨','[',-1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, true, true);
  AddKey('$', '*',']', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  CurrentX := CurrentX + (Size div 4);
  AddKey('Enter', '','', VK_RETURN, -1, -1, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, False, False, False, $A0A0A0);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, -1, -1, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, False, False, False, $A0A0A0);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('ù', '%','´', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, false, true);
  AddKey('µ', '£','`', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, false, true);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 5) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  AddKey('<', '>','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ñ');
  AddSubKey('ñ');
  AddKey(';', '.','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(';', ',','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(':', '/','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, False, False, False, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, False, False, False, $A0A0A0);
  AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.BuildDVORAKKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;

begin
  CurrentY := 1;
  CurrentX := CurrentY;
  Size := 40;
  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 202;
    Width  := 602;
    FOriginalW := 602;
    FOriginalH := 202
    ;
  end;
  DoAutoZoom := true;

 with Keys do
  begin
  AddKey('`', '~','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, true, true);
  AddKey('1', '!','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '@','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '#','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '^','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, true);
  AddKey('7', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '*','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '(','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', ')','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('[', '{','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(']', '}','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, False, False, False, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, False, False, False, $A0A0A0);
  AddKey('''', '"','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '<','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '>','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('/', '?','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('\', '|','', -1, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skNone);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, -1, -1, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, False, False, False, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Enter', '','', VK_RETURN, -1, -1, -1, (Size * 9) div 4, Size, CurrentX, CurrentY, skReturn, False, False, False, $A0A0A0);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 9) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  AddKey(';', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, False, False, False, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', VK_NONCONVERT, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, False, False, False, $A0A0A0);
  AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.BuildCellPhoneKeyBoard;
var
  CurrentX,
  CurrentY,
  Size : integer;

begin
  CurrentY := 1;
  CurrentX := CurrentY;
  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 162;
    Width  := 122;
    FOriginalW := 122;
    FOriginalH := 162;
  end;
  DoAutoZoom := true;
  Size := 40;

  with Keys do
  begin
  AddKey('7', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('4', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('1', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('*', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Back', '', '', VK_BACK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skBackspace, False, False, False, $A0A0A0);
  end;
end;


procedure TAdvSmoothTouchKeyBoard.BuildNumericKeyBoard;
var
  CurrentX,
  CurrentY,
  Size : integer;

begin
  CurrentY := 1;
  CurrentX := CurrentY;
  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 202;
    Width  := 162;
    FOriginalW := 162;
    FOriginalH := 202;
  end;
  DoAutoZoom := true;
  Size := 40;

  with Keys do
  begin

  AddKey('Num Lock', '', '', VK_NUMLOCK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNum, False, False, False, $A0A0A0);
  AddKey('/', '', '', VK_DIVIDE, -1, -1, -1, Size, Size, CurrentX, CurrentY, skDivide);
  AddKey('*', '', '', VK_MULTIPLY, -1, -1, -1, Size, Size, CurrentX, CurrentY, skMultiply);
  AddKey('-', '','', VK_SUBTRACT, -1, -1, -1, Size, Size, CurrentX, CurrentY, skSubstract);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('7', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('+', '', '', VK_ADD, -1, -1, -1, Size, Size * 2, CurrentX, CurrentY, skAdd);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('4', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('1', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '', '', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Enter', '', '', VK_RETURN, -1, -1, -1, Size, Size * 2, CurrentX, CurrentY, skReturn, False, False, False, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('0', '', '', -1, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '', '', VK_DECIMAL, -1, -1, -1, Size, Size, CurrentX, CurrentY, skDecimal);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.BuildQWERTYKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;
begin
  CurrentY := 1;
  CurrentX := CurrentY;
  Size := 40;

  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 202;
    Width  := 602;
    FOriginalW := 602;
    FOriginalH := 202;
  end;
  DoAutoZoom := true;

  with Keys do
  begin
  //AddKey('º', 'ª','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('`', '~','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, true, true);
  AddKey('1', '!','|', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  //AddKey('2', '"','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('2', '@','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('3', '#','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('6', '^','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, true);
  AddKey('7', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('8', '*','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('9', '(','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('0', ')','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('=', '+','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('\', '|','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Back', '','', VK_BACK, -1, -1, -1, Size, Size, CurrentX, CurrentY, skBackSpace, False, False, False, $A0A0A0);
  // End Row #1
  NewRow(CurrentX, CurrentY, Size);
  AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab, False, False, False, $A0A0A0);
  AddKey('Q', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('E', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ë');
  AddSubKey('Ê');
  AddSubKey('É');
  AddSubKey('È');
  AddSubKey('ë');
  AddSubKey('ê');
  AddSubKey('é');
  AddSubKey('è');
  AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('ÿ');
  AddSubKey('Ý');
  AddSubKey('ý');
  AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ü');
  AddSubKey('Û');
  AddSubKey('Ú');
  AddSubKey('Ù');
  AddSubKey('ü');
  AddSubKey('û');
  AddSubKey('ú');
  AddSubKey('ù');
  AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ï');
  AddSubKey('Î');
  AddSubKey('Í');
  AddSubKey('Ì');
  AddSubKey('ï');
  AddSubKey('î');
  AddSubKey('í');
  AddSubKey('ì');
  AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ö');
  AddSubKey('Ô');
  AddSubKey('Ó');
  AddSubKey('Ò');
  AddSubKey('ö');
  AddSubKey('ô');
  AddSubKey('ó');
  AddSubKey('ò');
  AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('[', '{','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(']', '}','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  CurrentX := CurrentX + (Size div 4);
  AddKey('Enter', '','', VK_RETURN, 0, 0, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, False, False, False, $A0A0A0);
  // End Row #2
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Caps Lock', '','', VK_CAPITAL, 0, 0, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, False, False, False, $A0A0A0);
  AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ä');
  AddSubKey('Â');
  AddSubKey('Á');
  AddSubKey('À');
  AddSubKey('ä');
  AddSubKey('â');
  AddSubKey('á');
  AddSubKey('à');
  AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(';', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '"','', -1, -1,-1, -1, Size, Size, CurrentX, CurrentY, skNone);
//  AddKey('Ç', '','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  // End Row #3
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  //AddKey('<', '>','', -1, -1 , -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('ç');
  AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddSubKey('Ñ');
  AddSubKey('ñ');
  AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey(',', '<','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('.', '>','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('/', '?','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
  AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
  // End Row #4
  NewRow(CurrentX, CurrentY, Size);
  AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, False, False, False, $A0A0A0);
  AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
  AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, False, False, False, $A0A0A0);
  AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
  AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, False, False, False, $A0A0A0);

  //AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, $A0A0A0);
  AddKey('->', '','', VK_RIGHT, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skRight, False, False, False, $A0A0A0);

  end;
end;
procedure TAdvSmoothTouchKeyBoard.BuildQWERTZKeyBoard;
var
  CurrentX,
  CurrentY,
  Size   : integer;

begin
  CurrentY := 1;
  CurrentX := CurrentY;
  Size := 40;
  DoAutoZoom := false;
  if not (csLoading in ComponentState) then
  begin
    Height := 202;
    Width  := 602;
    FOriginalW := 602;
    FOriginalH := 202;
  end;
  DoAutoZoom := true;

  with Keys do
  begin
    AddKey('^', '°','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, true);
    AddKey('1', '!','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('2', '"','²', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('3', '§','³', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('4', '$','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('5', '%','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('6', '&','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('7', '/','{', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('8', '(','[', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('9', ')',']', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('0', '=','}', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('ß', '?','\', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('´', '`','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, true, true);
    AddKey('Backspace', '','', VK_BACK, -1, -1, -1, Size * 2, Size, CurrentX, CurrentY, skBackSpace, False, False, False, $A0A0A0);
    // End Row #1
    NewRow(CurrentX, CurrentY, Size);
    AddKey('->', '<-','', VK_TAB, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skTab);
    AddKey('Q', '','@', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('W', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('E', '','€', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ë');
    AddSubKey('Ê');
    AddSubKey('É');
    AddSubKey('È');
    AddSubKey('ë');
    AddSubKey('ê');
    AddSubKey('é');
    AddSubKey('è');
    AddKey('R', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('T', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Z', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('U', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ü');
    AddSubKey('Û');
    AddSubKey('Ú');
    AddSubKey('Ù');
    AddSubKey('ü');
    AddSubKey('û');
    AddSubKey('ú');
    AddSubKey('ù');
    AddKey('I', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ï');
    AddSubKey('Î');
    AddSubKey('Í');
    AddSubKey('Ì');
    AddSubKey('ï');
    AddSubKey('î');
    AddSubKey('í');
    AddSubKey('ì');
    AddKey('O', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ö');
    AddSubKey('Ô');
    AddSubKey('Ó');
    AddSubKey('Ò');
    AddSubKey('ö');
    AddSubKey('ô');
    AddSubKey('ó');
    AddSubKey('ò');
    AddKey('P', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Ü', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('+', '*','~', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone, false, false, true);
    CurrentX := CurrentX + (Size div 4);
    AddKey('Enter', '','', VK_RETURN, 0, 0, -1, (Size * 5) div 4, Size * 2, CurrentX, CurrentY, skReturn, False, False, False, $A0A0A0);
    // End Row #2
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Caps Lock', '','', VK_CAPITAL, 0, 0, -1, (Size * 7) div 4, Size, CurrentX, CurrentY, skCaps, False, False, False, $A0A0A0);
    AddKey('A', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ä');
    AddSubKey('Â');
    AddSubKey('Á');
    AddSubKey('À');
    AddSubKey('ä');
    AddSubKey('â');
    AddSubKey('á');
    AddSubKey('à');
    AddKey('S', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('D', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('F', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('G', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('H', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('J', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('K', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('L', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Ö', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Ä', '','', -1, -1,-1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('#', '''','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    // End Row #3
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Shift', '','', VK_LSHIFT, -1, -1, -1, (Size * 5) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
    AddKey('<', '>','', -1, -1 , -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Y', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('ÿ');
    AddSubKey('Ý');
    AddSubKey('ý');
    AddKey('X', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('C', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('V', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('B', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('N', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddSubKey('Ñ');
    AddSubKey('ñ');
    AddKey('M', '','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey(',', ';','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('.', ':','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('-', '_','', -1, -1, -1, -1, Size, Size, CurrentX, CurrentY, skNone);
    AddKey('Shift', '','', VK_RSHIFT, -1, -1, -1, (Size * 11) div 4, Size, CurrentX, CurrentY, skShift, False, False, False, $A0A0A0);
    // End Row #4
    NewRow(CurrentX, CurrentY, Size);
    AddKey('Ctrl', '','', VK_LCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
    AddKey('Win', '','', VK_LWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
    AddKey('Alt', '','', VK_MENU, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAlt, False, False, False, $A0A0A0);
    AddKey('', '','', VK_SPACE, -1, -1, -1, Size * 6, Size, CurrentX, CurrentY, skSpaceBar);
    AddKey('Alt Gr', '','', 0, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skAltGr, False, False, False, $A0A0A0);
    AddKey('Win', '','', VK_RWIN, -1, -1, 0, Size, Size, CurrentX, CurrentY, skWin, False, False, False, $A0A0A0);
    AddKey('Menu', '','', VK_APPS, -1, -1, -1, Size, Size, CurrentX, CurrentY, skApp, False, False, False, $A0A0A0);
    AddKey('Ctrl', '','', VK_RCONTROL, -1, -1, -1, (Size * 6) div 4, Size, CurrentX, CurrentY, skCtrl, False, False, False, $A0A0A0);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.TextChanged(Sender: TObject);
var
  str: String;
begin
  str := FEditControl.Text;
  if Assigned(OnTextChanged) then
    OnTextChanged(Self, str);

  if str <> FEditControl.Text then
    FEditControl.Text := str;
end;

procedure TAdvSmoothTouchKeyBoard.TurnOffShifts;
var
  i: integer;
  invalid: boolean;
begin
  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skCaps) then
    begin
      if Keys[i].FDownState > 0 then
        Exit;
    end;
  end;

  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skShift) or (Keys[i].SpecialKey = skAltGr)
      or (Keys[i].SpecialKey = skCtrl) or (Keys[i].SpecialKey = skAlt) then
    begin
      Keys[i].FDownState := 0;
      Keys[i].FKeyNormalPosition := true;
    end;
  end;

  invalid := (ssShift in Shift) or (ssAlt in Shift) or (ssCtrl in Shift);
  FShift  := FShift - [ssShift] - [ssAlt] - [ssCtrl];

  if invalid then
    Changed;
end;

procedure TAdvSmoothTouchKeyBoard.UpdateAutoCompletionFormPosition;
var
  pt: TPoint;
begin
  if Assigned(FAutoCompletionForm) then
  begin
    pt := ClientToScreen(Point(0, 0));
    SetWindowPos(FAutoCompletionForm.Handle, 0, pt.X + (Self.Width - FAutocompletionForm.Width) div 2
    , pt.Y - FAutoCompletionForm.Height,
      FAutoCompletionForm.Width, FAutoCompletionForm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SyncEqualKeys(Index: Integer);
var
  i: integer;
  ds: integer;
  kn: boolean;
  sk: TAdvSmoothTouchSpecialKey;
begin
  with Keys.Items[Index] do
  begin
    ds := FDownState;
    kn := FKeyNormalPosition;
    sk := SpecialKey;

    for i := 0 to Keys.Count - 1 do
    begin
      if (Keys[i].SpecialKey = sk) then
      begin
        Keys[i].FDownState := ds;
        Keys[i].FKeyNormalPosition := kn;
      end;
    end;
  end;
end;

function TAdvSmoothTouchKeyBoard.IsCapsDown: boolean;
begin
  Result := FCapsDown;
end;

function TAdvSmoothTouchKeyBoard.IsComboKey(AKey: String): Boolean;
begin
  Result := False;
  if (Length(AKey) = 0) or (Length(FComboKeyValue) = 0) then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if CharInSet(FComboKeyValue[1], ['^', '¨', '´', '`']) then
    Result := CharInSet(AKey[1], ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u'])
  else if CharInSet(FComboKeyValue[1], ['~']) then
    Result := CharInSet(AKey[1], ['A', 'N', 'O', 'a', 'n', 'o']);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if FComboKeyValue[1] in ['^', '¨', '´', '`'] then
    Result := AKey[1] in ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u']
  else if FComboKeyValue[1] = '~' then
    Result := AKey[1] in ['A', 'N', 'O', 'a', 'n', 'o'];
  {$ENDIF}
end;

procedure TAdvSmoothTouchKeyBoard.ItemKeyUp(Index: Integer);
begin
  if Assigned(FOnKeyClick) then
    FOnKeyClick(Self, Index);
end;

procedure TAdvSmoothTouchKeyBoard.ItemKeyDown(Index: Integer);
var
  Key : Word;
  i: integer;

begin
  with Keys.Items[Index] do
  begin
    Key := KeyValue;

    if FAutoPostKey then
    begin
      if ShortCut <> '' then
      begin
        for i := 1 to length(ShortCut) do
        begin
          if not FPostWMCharOnly then
            SendMessage(KeybdInputHandle, WM_KEYDOWN, Ord(ShortCut[i]),0);

          SendMessage(KeybdInputHandle, WM_CHAR, Ord(ShortCut[i]),0);

          if not FPostWMCharOnly then
            SendMessage(KeybdInputHandle, WM_KEYUP, Ord(ShortCut[i]),0);
        end;
      end
      else
      begin
        if not (SpecialKey = skNone)  then
        begin
          if KeyValue = -1 then
            case SpecialKey of
            skUp: Key := VK_UP;
            skDown: Key := VK_DOWN;
            skRight: Key := VK_RIGHT;
            skLeft: Key := VK_LEFT;
            skNext: Key := VK_NEXT;
            skPrior: Key := VK_PRIOR;
            skHome: Key := VK_HOME;
            skEnd: Key := VK_END;
            skSpaceBar: Key := VK_SPACE;
            skReturn: Key := VK_RETURN;
            skBackSpace: Key := VK_BACK;
            skDelete: Key := VK_DELETE;
            skEscape: Key := VK_ESCAPE;
            end;
          PostAdvSmoothTouchSpecialKeys(Key, Shift, False)
        end
        else
          PostNormalKeys(Index);
      end;
    end;

    if Assigned(FOnKeyDown) then
      FOnKeyDown(Self.Keys.Items[Index], Key, Shift);

    if (SpecialKey = skApp) then
      Keys.Items[Index].FKeyNormalPosition := True;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.Loaded;
begin
  inherited;
  case AutoCompletion.Mode of
    cmAlwaysVisible:
    begin
      if not Assigned(FAutoCompletionForm) then
        ShowAutoCompletionForm;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothTouchKeyBoard.LoadKeybdLayout(FileName: string; AutoSize: TAutoSizeLayout = aslKeyboard);
var
  IniFile      : TMemIniFile;
  i, TotalKeys : Integer;

begin
  {$IFNDEF DELPHI_UNICODE}
  IniFile := TMemIniFile.Create(FileName);
  {$ELSE}
  IniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
  {$ENDIF}
  with Keys do
  begin
    TotalKeys := IniFile.ReadInteger('Keys','Count', 0);
    Clear;
    try
      for i := 0 to TotalKeys - 1 do
        begin
        Add;
        Items[i].Caption        := IniFile.ReadString  ('Key' + IntToStr(i), 'Caption', '');
        Items[i].ShiftCaption   := IniFile.ReadString  ('Key' + IntToStr(i), 'ShiftCaption', '');
        Items[i].AltGrCaption   := IniFile.ReadString  ('Key' + IntToStr(i), 'AltGrCaption', '');
        Items[i].KeyValue       := IniFile.ReadInteger ('Key' + IntToStr(i), 'KeyValue', -1);
        Items[i].ShiftKeyValue  := IniFile.ReadInteger ('Key' + IntToStr(i), 'ShiftKeyValue', -1);
        Items[i].AltGrKeyValue  := IniFile.ReadInteger ('Key' + IntToStr(i), 'AltGrKeyValue', -1);

        Items[i].SpecialKey     := TAdvSmoothTouchSpecialKey(IniFile.ReadInteger ('Key' + IntToStr(i), 'SpecialKey', 0));

        Items[i].Color          := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'Color', ''));
        Items[i].ColorDown      := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'ColorDown', ''));
        Items[i].TextColor      := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'TextColor', ''));
        Items[i].TextColorDown  := StringToColor(IniFile.ReadString  ('Key' + IntToStr(i), 'TextColorDown', ''));

        Items[i].Width          := IniFile.ReadInteger ('Key' + IntToStr(i), 'Width', 40);
        Items[i].Height         := IniFile.ReadInteger ('Key' + IntToStr(i), 'Height', 40);
        Items[i].X              := IniFile.ReadInteger ('Key' + IntToStr(i), 'X', 0);
        Items[i].Y              := IniFile.ReadInteger ('Key' + IntToStr(i), 'Y', 0);

        if AutoSize = aslKeyboard then
        begin
          FOriginalW := inifile.ReadInteger('KeyBoard', 'Width', Width);
          FOriginalH := inifile.ReadInteger('KeyBoard', 'Height', Height);
          AutoZoom;
        end
        else
        begin
          FOriginalW := Width;
          FOriginalH := Height;
          Width := inifile.ReadInteger('KeyBoard', 'Width', Width);
          Height := inifile.ReadInteger('KeyBoard', 'Height', Height);
          AutoZoom;
        end;
        end;
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  item: integer;
begin
  inherited;
  if Assigned(OnBeforeKeyClick) then
    OnBeforeKeyClick(Self);

  FMouseDown := true;
  if Assigned(FSubKeyForm) then
  begin
    FMouseUp := false;
    FSubKeyForm.Free;
    FSubKeyForm := nil;
    if Assigned(FDownKey) then
    begin
      FDownKey.FKeyNormalPosition := true;
      FDownKey := nil;
    end;

    Changed;
  end;

  item := XYtoItem(X, Y);
  FDownKey := nil;
  if item <> -1 then
  begin
    FDownKey := Keys[item];
    FSubKeyTime := 0;
    FTimer.Enabled := true;
    with Keys[item] do
    begin
      if FSpecialKey in [skShift, skAlt, skAltGr, skCaps, skCtrl, skNum, skScroll] then
      begin
        FDownState := FDownState + 1;

        case FSpecialKey of
          skShift, skCaps : FOwner.FShift :=
               FOwner.FShift + [ssShift];
          skAlt   : FOwner.FShift :=
               FOwner.FShift + [ssAlt];
          skCtrl  : FOwner.FShift :=
               FOwner.FShift + [ssCtrl];
          skAltGr : FOwner.FShift :=
               FOwner.FShift + [ssCtrl, ssAlt];
          end;

        // if FDownState greater then zero then key change to DownState
        if (FDownState > 0) then
          FKeyNormalPosition := False;

        if (FOwner.AutoCapsDisplay) or
           (FOwner.HighlightCaps <> clNone) or
           (FOwner.HighlightAltGr <> clNone) then
        begin
          Changed;
        end;
      end
      else
      begin
//        TODO timer event?
//        if (FSpecialKey <> skApp) Then
//          FOwner.StartTimer(item);
      end;

      FKeyNormalPosition := False;
      if (FSpecialKey in [skShift, skCaps, skAlt, skCtrl, skAltGr]) then
      begin
        FOwner.ItemKeyDown(item);
      end;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  nxtitem, item: integer;
begin
  inherited;
  FMouseDown := false;
  if Assigned(FSubKeyForm) then
  begin
    if FMouseUp then
    begin
      FMouseUp := false;
      FSubKeyForm.Free;
      FSubKeyForm := nil;
      if Assigned(FDownKey) then
      begin
        FDownKey.FKeyNormalPosition := true;
        FDownKey := nil;
      end;
      FTimer.Enabled := false;
      FSubKeyTime := 0;
    end
    else
      FMouseUp := true;

    Changed;
    Exit;
  end;

  if Assigned(FDownKey) then
  begin
    item := FDownKey.Index;
    nxtitem := XYtoItem(X, Y);
    if item <> nxtitem then
    begin
      FDownKey.FKeyNormalPosition := not FDownKey.FKeyNormalPosition;
      Changed;
      Exit;
    end;
  end
  else
  begin
    item := -1;
  end;

  if item <> -1 then
  begin
    with Keys[item] do
    begin
      if (Keys[Item].SpecialKey = skBackSpace) then
      begin
        AutoCompletion.FLookup := Copy(AutoCompletion.FLookup, 0 ,Length(AutoCompletion.FLookup) - 1);
      end
      else
      begin
        if CheckAutoCompletionKey(Keys[item]) then
          AutoCompletion.FLookup := AutoCompletion.FLookup + Keys[item].Caption
        else
          AutoCompletion.FLookup := '';
      end;

      if (Length(AutoCompletion.FLookup) >= AutoCompletion.LookupFromChars) then
        AutoCompletion.UpdateCompletion(AutoCompletion.FLookup);

      case AutoCompletion.Mode of
        cmAutoVisible:
        begin
          if (Length(AutoCompletion.FLookup) >= AutoCompletion.LookupFromChars) and (AutoCompletion.FCompletion.Columns[0].CustomItems.Count > 0) then
          begin
            if not Assigned(FAutoCompletionForm) then
              ShowAutoCompletionForm;
          end
          else if Assigned(FAutoCompletionForm) then
          begin
            FAutoCompletionForm.Free;
            FAutoCompletionForm := nil;
          end;
        end;
      end;

      FSubKeyTime := 0;
      FTimer.Enabled := false;
      // is this specialkey?
      if FSpecialKey in [skShift, skAlt, skAltGr, skCaps, skCtrl, skNum, skScroll] then
      begin
        if (FDownState > 1) then       // is this its second MouseUp?
        begin
          FDownState := 0;
          case FSpecialKey of
            skShift, skCaps:
                FOwner.FShift :=
                    FOwner.FShift - [ssShift];
            skAlt   : FOwner.FShift :=
                FOwner.FShift - [ssAlt];
            skCtrl  : FOwner.FShift :=
                FOwner.FShift - [ssCtrl];
            skAltGr : FOwner.FShift :=
                FOwner.FShift - [ssCtrl, ssAlt];
            end;
        end
        else
          Exit;
      end
      else
      begin
        FOwner.ItemKeyDown(item);
        FOwner.StopTimer;
      end;

      // if FDownState equal to zero then key can change
      if FDownState = 0 then
        FKeyNormalPosition := True;

      Changed;

      if (FSpecialKey in [skShift, skAlt, skCtrl, skAltGr]) then
        FOwner.SyncEqualKeys(item);

      FOwner.ItemKeyUp(item);

      if (SpecialKey in [skShift, skAlt, skCtrl, skAltGr]) then
        SyncEqualKeys(Index)
      else
      begin
        TurnOffShifts;
      end;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.NewRow(var X, Y: Integer; Size: Integer);
begin
    X := 1;
    Y := Y + Size;
end;

procedure TAdvSmoothTouchKeyBoard.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
 if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  inherited;
end;

procedure TAdvSmoothTouchKeyBoard.Paint;
var
  g: TGPgraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);
  DrawBackGround(g);
  DrawKeys(g);
  g.Free;
end;

procedure TAdvSmoothTouchKeyBoard.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSmoothTouchKeyBoard.PostKey(Key: Word; const pShift: TShiftState;
  ExtendedKeyBD: Boolean; Index: Integer);
begin
    if not (Self.Keys.Items[Index].SpecialKey = skNone)  then
      PosTAdvSmoothTouchSpecialKeys(Key, Shift, ExtendedKeyBD)
    else
      PostNormalKeys(Index);
end;

procedure TAdvSmoothTouchKeyBoard.PostNormalKeys(Index: Integer);
type
  TKeyVal = (kvNormal, kvShifted, kvAtlGr, kvCapital);
var
  KeyCaption: string;
  KeyVal    : TKeyVal;
  KVal,i : integer;
  isCombo: boolean;
  isCaps: boolean;
  keycombo: Boolean;

begin
  KeyCaption := '';
  KVal := -1;
  isCaps := false;

  for i := 0 to Keys.Count - 1 do
  begin
    if (Keys[i].SpecialKey = skCaps) then
    begin
      if Keys[i].FDownState > 0 then
        isCaps := true;
    end;
  end;


  with Keys.Items[Index] do
  begin
    if ((ssShift in Shift) or (isCaps)) and not (SpecialKey = skShift) then
    begin
      KeyVal := kvShifted;
    end
    else if IsCapsDown then
      KeyVal := kvCapital
    else if ((ssCtrl in Shift) and (ssAlt in Shift)) and not (SpecialKey = skAltGr) then
      KeyVal := kvAtlGr
    else
      KeyVal := kvNormal;

    keycombo := False;
    case KeyVal of
      kvNormal  :
        begin
          KeyCaption := SysUtils.AnsiLowerCase(Caption);
          KVal := KeyValue;
          keycombo := KeyCombination;
        end;
      kvShifted :
        begin
          if ShiftCaption = '' then
            KeyCaption := Caption
          else
            KeyCaption := ShiftCaption;

          KVal := ShiftKeyValue;
          keycombo := ShiftKeyCombination;
        end;

      kvAtlGr   :
      begin
        KeyCaption := AltGrCaption;
        keycombo := AltGrKeyCombination;
      end;
      kvCapital : KeyCaption := Caption
    end;
  end;

  if FCombo then
  begin
    FCombo := False;
    if IsComboKey(KeyCaption) then
      KeyCaption := GetComboKey(KeyCaption)
    else if Length(FComboKeyValue) > 0 then
      SendMessage(KeybdInputHandle, WM_CHAR, Ord(FComboKeyValue[1]),0);
  end
  else if keycombo then
  begin
    FComboKeyValue := KeyCaption;
    FComboKey := KVal;
    FCombo := True;
    Exit;
  end;

  if (KVal <> -1) then
  begin
    isCombo := IsComboBox(KeybdInputHandle);

    if not FPostWMCharOnly and not isCombo then
      SendMessage(KeybdInputHandle, WM_KEYDOWN, KVal,0);

    SendMessage(KeybdInputHandle, WM_CHAR, KVal,0);

    //if res = 0 then
    //begin
    //  StopTimer;
    //  Keys[Index].FTouchKey.MouseUp(mbLeft,[],0,0);
    //end;

    if not FPostWMCharOnly and not isCombo then
      SendMessage(KeybdInputHandle, WM_KEYUP, KVal,0);
  end
  else
    if (KeyCaption <> '') then
    begin
      isCombo := IsComboBox(KeybdInputHandle);


      if not FPostWMCharOnly and not isCombo then
      begin
        if (Upcase(KeyCaption[1]) >= '0') and
           (Upcase(KeyCaption[1]) <= 'Z') then
           SendMessage(KeybdInputHandle, WM_KEYDOWN, Ord(Upcase(KeyCaption[1])),0);
      end;

      SendMessage(KeybdInputHandle, WM_CHAR, Ord(KeyCaption[1]),0);

      //if res = 0 then
      //begin
      //  StopTimer;
      //  Keys[Index].FTouchKey.MouseUp(mbLeft,[],0,0);
      //end;

      if not FPostWMCharOnly and not isCombo then
      begin
        if (Upcase(KeyCaption[1]) >= '0') and (Upcase(KeyCaption[1]) <= 'Z') then
        SendMessage(KeybdInputHandle, WM_KEYUP, Ord(Upcase(KeyCaption[1])),0);
      end;
    end;
end;

procedure TAdvSmoothTouchKeyBoard.PosTAdvSmoothTouchSpecialKeys(Key: Word; const pShift: TShiftState; SpecialKey: Boolean);
type
  TShiftKeyInfo = record
    Shift: Byte ;
    VKey: Byte ;
  end;
  ByteSet = set of 0..7;

const
  ShiftKeys: array [1..3] of TShiftKeyInfo =
    ((Shift: Ord(ssCtrl ) ; VKey : VK_CONTROL),
    ( Shift: Ord(ssShift) ; VKey : VK_SHIFT  ),
    ( Shift: Ord(ssAlt  ) ; VKey : VK_MENU   )) ;
var
  Flag: DWORD;
  bShift: ByteSet absolute pShift;
  i,res: Integer;
begin
  //SetActiveWindow((Owner as TCustomForm).Handle);

   if Key = VK_CAPITAL then
     FCapsDown := not FCapsDown;

   if Key = VK_SHIFT then
     FCapsDown := false;

   //TargetControl.SetFocus;
   // pressing shift keys
   for i := 1 to 3 do
   begin
     if ShiftKeys[i].Shift in bShift then
     begin
       keybd_event(ShiftKeys[i].VKey, MapVirtualKey(ShiftKeys[i].VKey, 0), 0, 0) ;
     end;
   end;

   // apply KEYEVENTF_EXTENDEDKEY if specialkey selected
   if SpecialKey then
     Flag := KEYEVENTF_EXTENDEDKEY
   else
     Flag := 0;

   if (Key <> VK_TAB) Then
   begin
     if (Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]) then
     begin
       SendMessage(KeybdInputHandle, WM_KEYDOWN, Key,0);
       SendMessage(KeybdInputHandle, WM_KEYUP, Key,0);
     end
     else
     begin
       // pressing the actual key
       keybd_event(Key, MapvirtualKey(Key, 0), Flag, 0) ;
       Flag := Flag or KEYEVENTF_KEYUP;
       // releasing the actual key
       keybd_event(Key, MapvirtualKey(Key, 0), Flag, 0) ;
     end;
   end
   else
   begin
     res := SendMessage(KeybdInputHandle, WM_GETDLGCODE, VK_TAB, 0);

     if res and DLGC_WANTTAB = DLGC_WANTTAB then
     begin
       SendMessage(KeybdInputHandle, WM_KEYDOWN, VK_TAB,0);
       SendMessage(KeybdInputHandle, WM_CHAR, VK_TAB,0);
       SendMessage(KeybdInputHandle, WM_KEYUP, VK_TAB,0);
     end
     else
     begin
       PostMessage(KeybdInputHandle, WM_KEYDOWN, VK_TAB,0);
     end;
   end;

   for i := 3 downto 1 do
   begin
     if ShiftKeys[i].Shift in bShift then
       keybd_event(ShiftKeys[i].VKey, MapVirtualKey(ShiftKeys[i].VKey, 0), KEYEVENTF_KEYUP, 0) ;
   end;
end;

procedure TAdvSmoothTouchKeyBoard.RepeatTimerProc(Sender: TObject);
begin
  inc(FRepeatTimerCount);
  if (FRepeatTimerCount > 2) then
    ItemKeyDown(FRepeatItemIndex);
end;

procedure TAdvSmoothTouchKeyBoard.Resize;
begin
  inherited;
  if DoAutoZoom then
    AutoZoom;
end;

procedure TAdvSmoothTouchKeyBoard.SaveKeybdLayout(FileName: string);
var
  IniFile : TMemIniFile;
  i       : Integer;
begin
  {$IFNDEF DELPHI_UNICODE}
  IniFile := TMemIniFile.Create(FileName);
  {$ELSE}
  IniFile := TMemIniFile.Create(FileName, TEncoding.UTF8);
  {$ENDIF}

  With Keys do
  begin
    IniFile.WriteInteger('Keys','Count', Count);
    try
      for i := 0 to Count - 1 do
        begin
        IniFile.WriteString('Key' + IntToStr(i), 'Caption', Items[i].Caption);
        IniFile.WriteString('Key' + IntToStr(i), 'ShiftCaption', Items[i].ShiftCaption);
        IniFile.WriteString('Key' + IntToStr(i), 'AltGrCaption', Items[i].AltGrCaption);

        IniFile.WriteInteger('Key' + IntToStr(i), 'KeyValue', Items[i].KeyValue);
        IniFile.WriteInteger('Key' + IntToStr(i), 'ShiftKeyValue', Items[i].ShiftKeyValue);
        IniFile.WriteInteger('Key' + IntToStr(i), 'AltGrKeyValue', Items[i].AltGrKeyValue);

        IniFile.WriteInteger('Key' + IntToStr(i), 'SpecialKey', Integer(Items[i].SpecialKey));
        IniFile.WriteInteger('Key' + IntToStr(i), 'Color', Items[i].Color);
        IniFile.WriteInteger('Key' + IntToStr(i), 'ColorDown', Items[i].ColorDown);
        IniFile.WriteInteger('Key' + IntToStr(i), 'TextColor', Items[i].TextColor);
        IniFile.WriteInteger('Key' + IntToStr(i), 'TextColorDown', Items[i].TextColorDown);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Width', Items[i].Width);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Height', Items[i].Height);
        IniFile.WriteInteger('Key' + IntToStr(i), 'X', Items[i].X);
        IniFile.WriteInteger('Key' + IntToStr(i), 'Y', Items[i].Y);
        end;

        inifile.WriteInteger('KeyBoard', 'Width', Width);
        inifile.WriteInteger('KeyBoard', 'Height', Height);
    finally
      IniFile.UpdateFile;
      IniFile.Free;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothTouchKeyBoard.SetAllowAutoZoom(const Value: Boolean);
begin
  if FAllowAutoZoom <> Value then
  begin
    FAllowAutoZoom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetAutoCapsDisplay(const Value: Boolean);
begin
  if FAutoCapsDisplay <> value then
  begin
    FAutoCapsDisplay := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetAutoCompletion(
  const Value: TAdvSmoothTouchKeyBoardCompletion);
begin
  if FAutoCompletion <> value then
  begin
    FAutoCompletion.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetAutoPostKey(const Value: Boolean);
begin
  if FAutoPostKey <> value then
  begin
    FAutoPostKey := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetColorTones(ATones: TColorTones);
var
  I: integer;
begin
  FMetroStyle := True;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.ColorMirror := ATones.Background.BrushColor;
  Fill.ColorMirrorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  for I := 0 to Keys.Count - 1 do
  begin
    Keys[I].color := ATones.Background.BrushColor;
    Keys[I].ColorDown := ATones.Selected.BrushColor;
    Keys[I].TextColor := ATones.Background.TextColor;
    Keys[I].TextColorDown := ATones.Selected.TextColor;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetComponentStyle(AStyle: TTMSStyle);
var
  I: Integer;
begin
  FMetroStyle := False;
  // TODO : do color settings here
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00E6D8D8;
        Fill.ColorTo := $00E6D8D8;
        Fill.BorderColor := clNone;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := RGB(225, 234, 185);
        Fill.ColorTo := RGB(225, 234, 185);
        Fill.BorderColor := clNone;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.ColorTo := $00F2F2F2;
        Fill.BorderColor := clNone;
      end;
    tsOffice2007Luna:
      begin
        Fill.Color := $00F3E5DA;
        Fill.ColorTo := $00F0DED0;
        Fill.BorderColor := clNone;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $5C534C;
        Fill.ColorTo := $5C534C;
        Fill.BorderColor := clNone;
      end;
    tsWindowsXP:
      begin
        Fill.Color := $00B6B6B6;
        Fill.ColorTo := $00B6B6B6;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $F5F9FA;
        Fill.BorderColor := clNone;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.Color := RGB(241, 244, 248);
        Fill.ColorTo := RGB(227, 232, 240);
        Fill.BorderColor := clNone;
      end;
    tsWindowsVista:
    begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FDDE99;
    end;
    tsWindows7:
    begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FDDE99;
    end;
    tsTerminal:
      begin
        Fill.Color := clGray;
        Fill.ColorTo := clGray;
        Fill.BorderColor := clNone;
      end;
      tsOffice2010Blue:
      begin
        Fill.Color := $EAD3BF;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;
      end;
      tsOffice2010Silver:
      begin
        Fill.Color :=  $D4CFCB;
        Fill.ColorTo :=  clNone;
        Fill.BorderColor := clNone;
      end;
      tsOffice2010Black:
      begin
        Fill.Color := $656565;
        Fill.ColorTo := clNone;
        Fill.BorderColor := clNone;
      end;

      tsWindows8, tsWindows10:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $E4E3E2;
      end;
    tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $D4D4D4;
      end;
    tsOffice2013LightGray:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $C6C6C6;
      end;
    tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.BorderColor := $D4D4D4;
      end;
    tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := $B2B2B2;
        Fill.BorderColor := $444444;
      end;
    tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := $363636;
        Fill.BorderColor := $444444;
      end;
  end;

  for I := 0 to Keys.Count - 1 do
  begin
    with Keys[I] do
    begin
      case AStyle of
        tsOffice2003Blue:
          begin
            Color := $EEDBC8;
            ColorDown := $AAD9FF;
          end;
        tsOffice2003Silver:
          begin
            Color := $E6E9E2;
            ColorDown := $AAD9FF;
          end;
        tsOffice2003Olive:
          begin
            Color := $CFF0EA;
            ColorDown := $AAD9FF;
          end;
        tsOffice2003Classic:
          begin
            Color := clWhite;
            ColorDown := $B59285;
          end;
        tsOffice2007Luna:
          begin
            Color := $FFEFE3;
            ColorDown := $AAD9FF;
          end;
        tsOffice2007Obsidian:
          begin
            Color := $F9F8F8;
            ColorDown := $AAD9FF;
          end;
        tsWindowsXP:
          begin
            Color := clBtnFace;//clWhite;
            ColorDown := clInactiveCaption;
          end;
        tsWhidbey:
          begin
            Color := $00F2F2F2;
            ColorDown := $AAD9FF;
          end;
        tsCustom: ;
        tsOffice2007Silver:
          begin
            Color := $F9F8F8;
            ColorDown := $AAD9FF;
          end;
        tsWindowsVista:
          begin
            Color := $FFFAF0;
            ColorDown := $FDF0D7;
          end;
        tsWindows7:
          begin
            Color := $FDF3EB;
            ColorDown := $FCEBDC;
          end;
        tsTerminal:
          begin
            Color := clBtnFace;
            ColorDown := clSilver;
          end;
          tsOffice2010Blue:
          begin
            Color := $F0DAC7;
            ColorDown := RGB(253, 227, 138);
          end;
          tsOffice2010Silver:
          begin
            Color := $EDE5E0;
            ColorDown := RGB(253, 227, 138);
          end;
          tsOffice2010Black:
          begin
            Color := $919191;
            ColorDown := RGB(253, 227, 138);
          end;
          tsWindows8, tsWindows10:
          begin
            Color := $F7F6F5;
            ColorDown := $F7E0C9;
          end;
           tsOffice2013White:
          begin
            Color := clWhite;
            ColorDown := $FCE2C8;
          end;
           tsOffice2013LightGray:
          begin
            Color := $F6F6F6;
            ColorDown := $FCE2C8;
          end;
           tsOffice2013Gray:
          begin
            Color := $E5E5E5;
            ColorDown := $FCE2C8;
          end;
          tsOffice2016White:
          begin
            Color := clWhite;
            ColorDown := $E3BDA3;
          end;
           tsOffice2016Gray:
          begin
            Color := $B2B2B2;
            ColorDown := $E3BDA3;
          end;
           tsOffice2016Black:
          begin
            Color := $363636;
            ColorDown := $444444;
          end;


      end;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetHighlightAltGr(const Value: TColor);
begin
  if FHighlightAltGr <> value then
  begin
    FHighlightAltGr := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetHighlightCaps(const Value: TColor);
begin
  if FHighlightCaps <> value then
  begin
    FHighlightCaps := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SeTAdvSmoothTouchKeyBoardType(const Value: TAdvSmoothTouchKeyBoardType);
begin
  FKeyboardType := Value;
  Keys.Clear;
  FShift := [];
  case Value of
    ktQWERTY    : BuildQWERTYKeyBoard;
    ktQWERTZ    : BuildQWERTZKeyBoard;
    ktAZERTY    : BuildAZERTYKeyBoard;
    ktDVORAK    : BuildDVORAKKeyBoard;
    ktNUMERIC   : BuildNumericKeyBoard;
    ktCELLPHONE : BuildCellPhoneKeyBoard;
    ktCustom    : ;
  end;

  if DoAutoZoom then
    AutoZoom;
end;

procedure TAdvSmoothTouchKeyBoard.SetKeys(const Value: TAdvSmoothTouchKeyCollection);
begin
  FKeys.Assign(Value);
end;

procedure TAdvSmoothTouchKeyBoard.SetShading(const Value: Boolean);
begin
  if FShading <> Value then
  begin
    FShading := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetShadingColor(const Value: TColor);
begin
  if FShadingColor <> Value then
  begin
    FShadingColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetShadingColorTo(const Value: TColor);
begin
  if FShadingColorTo <> Value then
  begin
    FShadingColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetShadingOpacity(const Value: Byte);
begin
  if FShadingOpacity <> Value then
  begin
    FShadingOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetShadingOpacityTo(const Value: Byte);
begin
  if FShadingOpacityTo <> Value then
  begin
    FShadingOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetShift(const Value: TShiftState);
var
  shiftok: Boolean;
  item: Integer;
begin
  if FShift <> Value then
  begin
    shiftok := false;
    if (ssShift in Shift) or (ssShift in Value) then
      shiftok := true;

    FShift := Value;
    if shiftok then
    begin
      item := GetItemOnShift;
      if item <> -1 then
      begin
        MouseDown(mbLeft, Shift, Keys[item].X, Keys[item].Y);
        MouseUp(mbLeft, Shift, Keys[item].X, Keys[item].Y);
      end;
    end;

    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetSmallFont(const Value: TFont);
begin
  if FSmallFont <> value then
  begin
    FSmallFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetSubKeyHidePause(const Value: integer);
begin
  if FSubKeyHidePause <> value then
  begin
    FSubKeyHidePause := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetSubKeyShowPause(const Value: integer);
begin
  if FSubKeyShowPause <> value then
  begin
    FSubKeyShowPause := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetText(const Value: String);
begin
  FText := Value;
  if Assigned(FEditControl) then
  begin
    FEditControl.Text := FText;
    FEditControl.SelStart := Length(FText);
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetUseFocusedControl(const Value: Boolean);
begin
  if FUseFocusedControl <> Value then
  begin
    FUseFocusedControl := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothTouchKeyBoard.ShowAutoCompletionForm;
begin
  if csDesigning in ComponentState then
    Exit;

  FAutoCompletionForm := TAdvSmoothTouchKeyBoardAutoForm.CreateNew(Application);
  FAutoCompletionForm.KeyBoard := Self;
  FAutoCompletionForm.Height := AutoCompletion.FCompletion.Height;
  FAutoCompletionForm.Width := AutoCompletion.FCompletion.Width;
  FAutoCompletionForm.Init;
  UpdateAutoCompletionFormPosition;
end;

procedure TAdvSmoothTouchKeyBoard.ShowForm(Key: TAdvSmoothTouchKeyItem);
var
  pt: TPoint;
begin
  if Assigned(FSubKeyForm) then
  begin
    FSubKeyForm.Free;
    FSubKeyForm := nil;
  end;

  FSubKeyForm := TAdvSmoothTouchKeyBoardForm.CreateNew(Application);
  FSubKeyForm.KeyBoard := Self;
  FSubKeyForm.KeyBoardItem := Key;
  pt := ClientToScreen(Point(Key.X, Key.Y));
  Key.InitKeys(6, 6);
  FSubKeyForm.Height := Round(Key.GetHeightSubKeys) + 12;
  FSubKeyForm.Width := Round(Key.GetWidthSubKeys) + 12;
  FSubKeyForm.Init;
  SetWindowPos(FSubKeyForm.Handle, 0, pt.X - 6, pt.Y - FsubKeyForm.Height + 6,
    FSubKeyForm.Width, FSubKeyForm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);

end;

procedure TAdvSmoothTouchKeyBoard.SetKey(AKey: Word; AKeyCaption: String = ''; pShift: TShiftState = []);
var
  item: Integer;
begin
  Shift := pShift;
  item := GetItemOnKey(AKey);
  if (item = -1) and (AKeyCaption <> '') then
    item := GetItemOnKeyCaption(AKeyCaption);

  if item <> -1 then
  begin
    MouseDown(mbLeft, Shift, Keys[item].X, Keys[item].Y);
    MouseUp(mbLeft, Shift, Keys[item].X, Keys[item].Y);
  end;

  Changed;
end;

procedure TAdvSmoothTouchKeyBoard.SetKeyDistance(Value : Integer);
begin
  if (Value <> FKeyDistance) Then
  begin
    FKeyDistance := Value;
    Changed;
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.StartTimer(index: integer);
begin
  FRepeatItemIndex := index;
  FRepeatTimerCount := 0;
  FRepeatTimer.Enabled := true;
end;

procedure TAdvSmoothTouchKeyBoard.StopTimer;
begin
  FRepeatTimer.Enabled := false;
  FRepeatTimerCount := 0;
end;

procedure TAdvSmoothTouchKeyBoard.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.result := 1;
end;

procedure TAdvSmoothTouchKeyBoard.WMPaint(var Message: TWMPaint);
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

procedure TAdvSmoothTouchKeyBoard.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WMTABLETQUERYSYSTEMGESTURESTATUS:
    begin
      Message.Result := Message.Result or TABLETDISABLEPRESSANDHOLD;
    end;
  end;
end;

function TAdvSmoothTouchKeyBoard.XYtoItem(X, Y: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Keys.Count - 1 do
  begin
    if PtInRect(Bounds(Keys[I].X, Keys[I].Y, Keys[I].Width, Keys[I].Height), Point(X, y))  then
    begin
      result := I;
      break;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoard.Zoom(fhorz,fvert: double; keysonly : boolean = false; absoluteZoom : boolean = false);
var
  i: integer;
begin
  DoAutoZoom := false;
  if (FOldH < 0) Then
  begin
    FOldW := Width;
    FOldH := Height;
  end;

  for i := 0 to Keys.Count - 1 do
  begin
    with Keys[i] do
    begin

      if (FOldW < 0) Then
      begin
         FOldW := width;
         FOldH := height;
         FOldX := X;
         FOldY := Y;
      end;

      if absoluteZoom Then
      begin
         width := round(width * fhorz);
         height := round(height * fvert);
         x := round(x * fhorz);
         y := round(y  * fvert);
      end
      else
      begin
         width := round(FOldW * fhorz);
         height := round(FOldH * fvert);
         x := round(FOldX * fhorz);
         y := round(FOldY  * fvert);
      end;
    end;
  end;
  if not keysonly then
  begin
    iF absoluteZoom then
    begin
      Width := round(Width * fhorz) + FKeyDistance;
      Height := round(Height * fvert) + FKeyDistance;
    end
    else
    begin
      Width := round(FOldW * fhorz) + FKeyDistance;
      Height := round(FOldH * fvert) + FKeyDistance;
    end;
  end;
  DoAutoZoom := true;
end;

{ TAdvSmoothTouchKeyItem }


procedure TAdvSmoothTouchKeyItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTouchKeyItem) then
  begin
    FCaption := (Source as TAdvSmoothTouchKeyItem).Caption;
    FShiftCaption := (Source as TAdvSmoothTouchKeyItem).ShiftCaption;
    FAltGrCaption := (Source as TAdvSmoothTouchKeyItem).AltGrCaption;
    FKeyValue := (Source as TAdvSmoothTouchKeyItem).KeyValue;
    fShiftKeyValue := (Source as TAdvSmoothTouchKeyItem).ShiftKeyValue;
    FAltGrKeyValue := (Source as TAdvSmoothTouchKeyItem).AltGrKeyValue;
    FSpecialKey := (Source as TAdvSmoothTouchKeyItem).SpecialKey;
    FColor := (Source as TAdvSmoothTouchKeyItem).Color;
    FColorDown := (Source as TAdvSmoothTouchKeyItem).ColorDown;
    FTextColor := (Source as TAdvSmoothTouchKeyItem).TextColor;
    FTextColorDown := (Source as TAdvSmoothTouchKeyItem).TextColorDown;
    FShortCut := (Source as TAdvSmoothTouchKeyItem).ShortCut;
    FX := (Source as TAdvSmoothTouchKeyItem).X;
    FY := (Source as TAdvSmoothTouchKeyItem).Y;
    FHeight := (Source as TAdvSmoothTouchKeyItem).Height;
    FWidth := (Source as TAdvSmoothTouchKeyItem).Width;
    FSubKeys.Assign((Source as TAdvSmoothTouchKeyItem).SubKeys);
    FShowBackGround := (Source as TAdvSmoothTouchKeyItem).ShowBackGround;
    FImage.Assign((Source as TAdvSmoothTouchKeyItem).Image);
    FImageIndex := (Source as TAdvSmoothTouchKeyItem).ImageIndex;
    FImageName := (Source as TAdvSmoothTouchKeyItem).ImageName;
    FImageDown.Assign((Source as TAdvSmoothTouchKeyItem).ImageDown);
    FImageIndexDown := (Source as TAdvSmoothTouchKeyItem).ImageIndexDown;
    FImageNameDown := (Source as TAdvSmoothTouchKeyItem).ImageNameDown;
    FImageWidth := (Source as TAdvSmoothTouchKeyItem).ImageWidth;
    FImageHeight := (Source as TAdvSmoothTouchKeyItem).ImageHeight;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTouchKeyItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := (Collection as TAdvSmoothTouchKeyCollection).FOwner;
  FShiftKeyValue := -1;
  FAltGrKeyValue := -1;
  FImageWidth := -1;
  FImageHeight := -1;
  FKeyCombination := False;
  FShiftKeyCombination := False;
  FAltGrKeyCombination := False;
  // defoult colors for none picture keys
  FColor := clSilver;
  FColorDown := clGray;
  FTextColor := clBlack;
  FTextColorDown := clBlack;
  // initial counter state
  FDownState := 0;
  FKeyNormalPosition := True;
  // default size of keys
  FHeight := 40;
  FWidth := 40;
  FX := -1;
  FY := -1;
  FOldH := -1;
  FOldW := -1;
  FOldX := -1;
  FOldY := -1;

  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;

  FImageDown := TAdvGDIPPicture.Create;
  FImageDown.OnChange := ImageChanged;
  FImageIndexDown := -1;

  FShowBackGround := True;

  FSubKeys := TAdvSmoothSubKeyCollection.Create(FOwner);
end;

destructor TAdvSmoothTouchKeyItem.Destroy;
begin
  FImage.Free;
  FImageDown.Free;
  FSubKeys.Free;
  inherited;
end;

procedure TAdvSmoothTouchKeyItem.Draw(g: TGPGraphics; r: TGPRectF; f, fs: TGPFont; sf, sfs: TGPStringFormat);
var
  xw, yh, VMargin, HMargin, xPos, yPos : Double;
  c: TColor;
  paintshift, paintaltgr: boolean;
  cap: string;
  offset: integer;
  curDistance: integer;
  b: TGPBrush;
  sizerect: TGPRectF;
  cf: TGPFont;
  cfs: TGPStringFormat;
  cl: TColor;
  gbmp: TGPGraphics;
  I: Integer;
  Cached: Boolean;
  chbmp: TKeyCachedBitmap;
  h: HDC;
  ca: TCanvas;
  pic: TAdvGDIPPicture;
  rimg: TGPRectF;
  img: TAdvGDIPPicture;
  imgidx: integer;
  imgn: String;
  imgw, imgh: Integer;
begin
  VMargin := 5;
  HMargin := 10;
  offset := 0;
  curDistance := FOwner.KeyDistance;

  if not FKeyNormalPosition then
    offset := FOwner.KeyDownOffset;

  if (SpecialKey = skCaps) then
  begin
    FKeyNormalPosition := not FOwner.IsCapsDown;

    if not FKeyNormalPosition then
      FDownState := 2;
  end;

  if FKeyNormalPosition then
    c := Color
  else
    c := ColorDown;

  chbmp.CachedBitmap := nil;
  Cached := false;
  for I := 0 to Length(FOwner.FCachedBitmaps) - 1 do
  begin
    if (integer(c) = FOwner.FCachedBitmaps[I].id)
      and (r.Width = FOwner.FCachedBitmaps[I].Width)
        and (r.Height = FOwner.FCachedBitmaps[I].Height) then
    begin
      Cached := true;
      chbmp := FOwner.FCachedBitmaps[I];
      break;
    end;
  end;

  if not Cached then
  begin
    setLength(FOwner.FCachedBitmaps, Length(FOwner.FCachedBitmaps) + 1);
    FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1].CachedBitmap := TGPBitmap.Create(Round(r.Width + 1), Round(r.Height + 1), PixelFormat32bppARGB);
    FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1].ID := integer(c);
    FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1].Width := r.Width;
    FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1].Height := r.Height;
    gbmp := g.FromImage(FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1].CachedBitmap);
    gbmp.SetSmoothingMode(SmoothingModeAntiAlias);
    gbmp.Clear(0);
    DrawBG(gbmp, MakeRect(curDistance / 2, curDistance / 2, r.Width - curDistance, r.Height - curDistance), c);
    gbmp.Free;
    chbmp := FOwner.FCachedBitmaps[Length(FOwner.FCachedBitmaps) - 1];
  end;


  if Assigned(chbmp.CachedBitmap) and ShowBackGround then
    g.DrawImage(chbmp.CachedBitmap, r);


  rimg := MakeRect(r.X , r.Y, r.Width, r.Height);
  if not FKeyNormalPosition then
  begin
    rimg.X := rimg.X + offset;
    rimg.Y := rimg.Y + offset;
  end;

  img := Image;
  imgidx := ImageIndex;
  imgn := ImageName;
  if not FKeyNormalPosition then
  begin
    img := ImageDown;
    imgidx := ImageIndexDown;
    imgn := ImageNameDown;
  end;

  if not img.Empty then
  begin
    img.GetImageSizes;
    imgw := img.Width;
    imgh := img.Height;
    if (ImageWidth > -1) and (ImageHeight > -1) then
    begin
      imgw := ImageWidth;
      imgh := ImageHeight;
    end;

    img.GDIPDraw(g, MakeRect(rimg.X + (rimg.Width - imgw) / 2, rimg.Y + (rimg.Height - imgh) / 2,
      imgw, imgh));
  end;

  if Assigned(FOwner.Images) then
  begin
    if (imgidx >= 0) and (imgidx <= FOwner.Images.Count - 1) then
    begin
      h := g.GetHDC;
      ca := TCanvas.Create;
      ca.Handle := h;
      imgw := FOwner.Images.Width;
      imgh := FOwner.Images.Height;
      if (ImageWidth > -1) and (ImageHeight > -1) then
      begin
        imgw := ImageWidth;
        imgh := ImageHeight;
      end;
      FOwner.Images.Draw(ca, Round(rimg.X + (rimg.Width -  imgw) / 2),
        Round(rimg.Y + (rimg.Height -  imgh) / 2), imgidx);
      ca.Free;
      g.ReleaseHDC(h);
    end;
  end;

  if Assigned(FOwner.PictureContainer) and (imgn <> '') then
  begin
    pic := FOwner.PictureContainer.FindPicture(imgn);
    if Assigned(pic) and not (pic.Empty) then
    begin
      pic.GetImageSizes;
      imgw := pic.Width;
      imgh := pic.Height;
      if (ImageWidth > -1) and (ImageHeight > -1) then
      begin
        imgw := ImageWidth;
        imgh := ImageHeight;
      end;
      pic.GDIPDraw(g, MakeRect(rimg.X + (rimg.Width - imgw) / 2,
        rimg.Y + (rimg.Height - imgh) / 2, imgw, imgh));
    end;
  end;

  // default text pos at center
  xw := r.X + (r.Width / 2);
  yh := r.Y + (r.Height / 2);

  // Long caption or ShiftCaption or AltGrCaption = SmallFont else Font
  if (Length(Caption) > 1) or (Length(ShiftCaption) > 0)
     or (Length(AltGrCaption) > 0) then
  begin
    cf := fs;
    cfs := sfs;
  end
  else
  begin
    cf := f;
    cfs := sf;
  end;

  if FKeyNormalPosition then
    cl := TextColor
  else
    cl := TextColorDown;


  paintshift := (ssShift in FOwner.Shift) or FOwner.IsCapsDown;
  paintaltgr := (ssCtrl in FOwner.Shift) and (ssAlt in FOwner.Shift);

  if paintaltgr then
    paintshift := false;

  cap := FCaption;

  if FOwner.AutoCapsDisplay and (length(FCaption) = 1) then
  begin
    if paintshift then
      cap := AnsiUpperCase(FCaption)
    else
      cap := AnsiLowerCase(FCaption);
  end;


  g.MeasureString(cap, Length(cap), cf, r, cfs, sizerect);
  b := TGPSolidBrush.Create(MakeColor(255, cl));
  if (FShiftCaption = '') then
  begin
    xPos := sizerect.Width / 2;
    yPos := sizerect.Height / 2;
    g.DrawString(cap, Length(cap), cf, MakeRect(xw - xPos + offset, yh - yPos + offset, sizerect.Width, sizerect.Height), cfs, b);
  end
  else
  begin
    g.DrawString(cap, Length(cap), cf, MakeRect(r.X + HMargin + offset, r.Y + r.Height + offset - VMargin - sizerect.Height,
      sizerect.Width, sizeRect.Height), cfs, b);

    if paintshift and (FOwner.HighlightCaps <> clNone) then
    begin
      cl := FOwner.HighlightCaps;
      b.Free;
      b := TGPSolidBrush.Create(MakeColor(255, cl));
    end;

    g.MeasureString(FShiftCaption, Length(FShiftCaption), cf, r, cfs, sizerect);
    g.DrawString(FShiftCaption, Length(FShiftCaption), cf, MakeRect(r.X + HMargin + offset,r.Y + VMargin + offset, sizeRect.Width, sizeRect.Height), cfs, b);
  end;

  if FKeyNormalPosition then
    cl := TextColor
  else
    cl := TextColorDown;

  if paintaltgr and (FOwner.HighlightAltGr <> clNone) then
  begin
    cl := FOwner.HighlightAltGr
  end;

  b.Free;

  b := TGPSolidBrush.Create(MakeColor(255, cl));

  if Length(AltGrCaption) > 0 then
  begin
    g.MeasureString('Z', Length('Z'), cf, r, cfs, sizerect);
    g.DrawString(FAltGrCaption, Length(FAltGrCaption), cf, MakeRect(r.X + r.Width + offset - HMargin - sizerect.Width,
      r.Y + r.Height + offset - VMargin - sizerect.Height, sizerect.Width, sizerect.Height), cfs,  b);
  end;

  b.Free;
end;

procedure TAdvSmoothTouchKeyItem.DrawBG(g: TGPGraphics; r: TGPRectF; c: TColor);
var
  path: TGPGraphicsPath;
  b: TGPBrush;
  rg, rb, rgl: TGPRectF;
  p: TGPPen;
begin
  rb := MakeRect(r.X + 1, r.Y + 1, r.Width - 2, r.Height - 2);
  path := CreateRoundRectangle(r, 3, rtBoth);
  b := TGPSolidBrush.Create(MakeColor(255, c));
  g.FillPath(b, path);
  b.Free;
  if not FOwner.FMetroStyle then
  begin
    b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1, r.Width + 1, r.Height + 1), MakeColor(FOwner.ShadingOpacity, FOwner.ShadingColor),
    MakeColor(FOwner.ShadingOpacityTo, FOwner.ShadingColorTo), LinearGradientModeBackwardDiagonal);
    p := TGPPen.Create(MakeColor(200, clBlack));
  end
  else
  begin
    b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1, r.Width + 1, r.Height + 1), MakeColor(255, c),
    MakeColor(255, c), LinearGradientModeBackwardDiagonal);
    p := TGPPen.Create(MakeColor(150, ColorDown));
  end;
  if FOwner.Shading then
    g.FillPath(b, path);
  
  b.Free;
  g.DrawPath(p, path);
  p.Free;
  path.Free;


  if not FOwner.FMetroStyle and FOwner.Shading then
  begin
    rg := MakeRect(rb.X + 2, rb.Y + 2, rb.Width - 4, rb.Height - 4);
    path := TGPGraphicsPath.Create;
    path.AddRectangle(rg);
    b := TGPLinearGradientBrush.Create(rg, MakeColor(100, c), MakeColor(255, c), LinearGradientModeBackwardDiagonal);
    g.FillPath(b, path);
    path.Free;
    b.Free;


    rgl := MakeRect(rg.X, rg.Y, rg.Width, rg.Height);
    path := TGPGraphicsPath.Create;
    path.AddRectangle(rgl);
    b := TGPLinearGradientBrush.Create(MakeRect(rgl.X - 1, rgl.Y - 1, rgl.Width + 1, rgl.Height + 1), MakeColor(FOwner.ShadingOpacityTo, FOwner.ShadingColorTo),
    MakeColor(FOwner.ShadingOpacity, FOwner.ShadingColor), LinearGradientModeBackwardDiagonal);
    g.FillPath(b, path);
    path.Free;
    b.Free;
  end;
end;

function TAdvSmoothTouchKeyItem.GetDisplayName: string;
begin
  Result := Caption;
end;

function TAdvSmoothTouchKeyItem.GetDownState: boolean;
begin
  Result := FDownState > 0;
end;

function TAdvSmoothTouchKeyItem.GetHeightSubKeys: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SubKeys.Count - 1 do
    Result := Max(Result, SubKeys[I].FSubKeyRect.Height + 3);

  Result := Result + 4;
end;

function TAdvSmoothTouchKeyItem.GetWidthSubKeys: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SubKeys.Count - 1 do
    Result := Result + SubKeys[I].FSubKeyRect.Width + 3;

  Result := Result + 4;
end;

procedure TAdvSmoothTouchKeyItem.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTouchKeyItem.InitKeys(StartX, StartY: integer);
var
  ff: TGPFontFamily;
  fsl: integer;
  sf: TGPStringFormat;
  f: TGPFont;
  bmp: TBitmap;
  g: TGPGraphics;
  I: integer;
  sizer: TGPRectF;
  xs, ys, w, h: Double;
begin
  with Fowner do
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);
    ff := TGPFontFamily.Create(Font.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fsl := 0;
    if (fsBold in Font.Style) then
      fsl := fsl + 1;
    if (fsItalic in Font.Style) then
      fsl := fsl + 2;
    if (fsUnderline in Font.Style) then
      fsl := fsl + 4;

    sf := TGPStringFormat.Create(0);
    f := TGPFont.Create(ff, Font.Size , fsl, UnitPoint);


    xs := startx + 3;
    ys := starty + 3;
    for I := 0 to SubKeys.Count - 1 do
    begin
      g.MeasureString(SubKeys[I].Caption, Length(SubKeys[I].Caption), f, MakeRect(0, 0, 10000, 10000), sf, sizer);
      w := sizer.Width + 20;
      h := Sizer.Height + 20;
      SubKeys[I].FSubKeyRect := MakeRect(xs, ys, w, h);
      SubKeys[I].FSubKeyCaptionRect := MakeRect(xs + (w - sizer.Width) / 2, ys + (h - sizer.Height) / 2, sizer.Width, sizer.Height);
      xs := xs + w + 3;
    end;

    f.Free;
    sf.Free;
    ff.Free;
    g.free;
    bmp.Free;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetAltGrCaption(const Value: string);
begin
  if FAltGrCaption <> value then
  begin
    FAltGrCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetAltGrKeyCombination(const Value: Boolean);
begin
  if FAltGrKeyCombination <> Value then
  begin
    FAltGrKeyCombination := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetAltGrKeyValue(const Value: Integer);
begin
  if FAltGrKeyValue <> value then
  begin
    FAltGrKeyValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetCaption(const Value: string);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetColorDown(const Value: TColor);
begin
  if FColorDown <> value then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetDownState(const Value: boolean);
begin
  if (FDownState > 0) <> Value then
  begin
    if Value  then
      FDownState := 1
    else
      FDownState := 0;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetHeight(const Value: Integer);
begin
  if FHeight <> value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageDown(const Value: TAdvGDIPPicture);
begin
  if FImageDown <> Value then
  begin
    FImageDown.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageHeight(const Value: Integer);
begin
  if FImageHeight <> Value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageIndexDown(const Value: Integer);
begin
  if FImageIndexDown <> Value then
  begin
    FImageIndexDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageNameDown(const Value: String);
begin
  if FImageNameDown <> Value then
  begin
    FImageNameDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetImageWidth(const Value: Integer);
begin
  if FImageWidth <> Value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetKeyCombination(const Value: Boolean);
begin
  if FKeyCombination <> Value then
  begin
    FKeyCombination := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetKeyvalue(const Value: Integer);
begin
  if FKeyValue <> value then
  begin
    FKeyValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetpecialKey(
  const Value: TAdvSmoothTouchSpecialKey);
begin
  if FSpecialKey <> value then
  begin
    FSpecialKey := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetShiftCaption(const Value: string);
begin
  if FShiftCaption <> value then
  begin
    FShiftCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetShiftKeyCombination(const Value: Boolean);
begin
  if FShiftKeyCombination <> Value then
  begin
    FShiftKeyCombination := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetShiftKeyValue(const Value: Integer);
begin
  if fShiftKeyValue <> value then
  begin
    fShiftKeyValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetShortCut(const Value: string);
begin
  if FShortCut <> value then
  begin
    FShortCut := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetShowBackGround(const Value: Boolean);
begin
  if FShowBackGround <> Value then
  begin
    FShowBackGround := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetSubKeys(
  const Value: TAdvSmoothSubKeyCollection);
begin
  if FSubKeys <> value then
  begin
    FSubKeys.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetTextColorDown(const Value: TColor);
begin
  if FTextColorDown <> value then
  begin
    FTextColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetWidth(const Value: Integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetX(const Value: integer);
begin
  if FX <> value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyItem.SetY(const Value: integer);
begin
  if FY <> Value then
  begin
    FY := Value;
    Changed;
  end;
end;

function TAdvSmoothTouchKeyItem.XYToSubKey(X, Y: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to SubKeys.Count - 1 do
  begin
    if PtInGPRect(SubKeys[I].FSubKeyRect, Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TAdvSmoothTouchKeyCollection }

function TAdvSmoothTouchKeyCollection.Add: TAdvSmoothTouchKeyItem;
begin
  Result := TAdvSmoothTouchKeyItem(inherited Add);
end;

constructor TAdvSmoothTouchKeyCollection.Create(AOwner: TAdvSmoothTouchKeyBoard);
begin
  inherited Create(TAdvSmoothTouchKeyItem);
  FOwner := AOwner;
end;

function TAdvSmoothTouchKeyCollection.GetItem(Index: Integer): TAdvSmoothTouchKeyItem;
begin
  Result := TAdvSmoothTouchKeyItem(inherited Items[Index]);
end;

function TAdvSmoothTouchKeyCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothTouchKeyCollection.Insert(index: integer): TAdvSmoothTouchKeyItem;
begin
  Result := TAdvSmoothTouchKeyItem(inherited Insert(Index));
end;

procedure TAdvSmoothTouchKeyCollection.SetItem(Index: Integer;
  const Value: TAdvSmoothTouchKeyItem);
begin
  inherited Items[index] := Value;
end;

procedure TAdvSmoothTouchKeyBoardToolForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    WndParent := (Owner as TCustomForm).Handle;
    Style := WS_POPUP;

    if ShowClose then
      Style := Style or WS_SYSMENU;
    if ShowCaption then
      Style := Style or WS_CAPTION;

    ExStyle := WS_EX_PALETTEWINDOW;
  end;
end;

destructor TAdvSmoothTouchKeyBoardToolForm.Destroy;
begin
  if Assigned(FKeyboard) then
    FKeyboard.Parent := nil;
  inherited;
end;

procedure TAdvSmoothTouchKeyBoardToolForm.WMActivate(var Message: TMessage);
begin
  inherited;
//  SetActiveWindow((Owner as TCustomForm).Handle);
  message.Result := 1;
end;

procedure TAdvSmoothTouchKeyBoardToolForm.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TAdvSmoothTouchKeyBoardToolForm.WMMove(var Message: TMessage);
begin
  if Assigned(FKeyboard) then
    FKeyboard.UpdateAutoCompletionFormPosition;
end;

{ TAdvSmoothPopupTouchKeyBoard }

procedure TAdvSmoothPopupTouchKeyBoard.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothPopupTouchKeyBoard) then
  begin
    FShowCaption := (Source as TAdvSmoothPopupTouchKeyBoard).ShowCaption;
    FShowClose := (Source as TAdvSmoothPopupTouchKeyBoard).ShowClose;
    FAutoFollowFocus := (Source as TAdvSmoothPopupTouchKeyBoard).AutoFollowFocus;
    FAutoHide := (Source as TAdvSmoothPopupTouchKeyBoard).AutoHide;
    FBackGroundColor := (Source as TAdvSmoothPopupTouchKeyBoard).BackGroundColor;
    FKbd.Assign((Source as TAdvSmoothPopupTouchKeyBoard).Keyboard);
  end
  else if (Source is TAdvSmoothTouchKeyBoard) then
  begin
    FKbd.Assign(Source);
  end;
end;

constructor TAdvSmoothPopupTouchKeyBoard.Create(AOwner: TComponent);
begin
  inherited;

  FKbd := TAdvSmoothTouchKeyBoard.Create(FFrm);
  FKbd.OnTextChanged := TextChanged;
  FKbd.OnResize := KeyboardSizeChanged;
  FKbd.Align := alClient;

  FFrm := nil;
  FOwnerform := GetParentForm(AOwner as TWinControl);

  FTimer := TTimer.Create(self);
  FTimer.OnTimer := TimerProc;
  FTimer.Interval := 250;
  FTimer.Enabled := false;

  FShowClose := true;
  FShowCaption := true;
  FDisableSizing := false;
  FBackGroundColor := clBtnFace;
  FLastX := -1;
  FLastY := -1;
end;

destructor TAdvSmoothPopupTouchKeyBoard.Destroy;
begin
  if Assigned(FKbd) then
    FKbd.Free;
  FTimer.Free;
  inherited;
end;

procedure TAdvSmoothPopupTouchKeyBoard.Hide;
begin
  FLastX := -1;
  FLastY := -1;
  FTimer.Enabled := false;
  if Assigned(FFrm) then
    FFrm.Free;
  FFrm := nil;
end;


procedure TAdvSmoothPopupTouchKeyBoard.KeyboardSizeChanged(Sender: TObject);
var
  s, sx: integer;
begin
  if not FDisableSizing then
  begin
    s := 0;
    sx := 0;
    if ShowCaption then
    begin
      s := GetSystemMetrics(SM_CYSMCAPTION);
      sx := GetSystemMetrics(SM_CXDLGFRAME) * 2;
    end;

    FFrm.Width := FKbd.Width + sx + FKbd.KeyDistance;
    FFrm.Height := FKbd.Height + sx + s + FKbd.KeyDistance;
  end;

//  FFrm.Width := FKbd.Width;
//  FFrm.Height := FKbd.Height;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetAutoCapsDisplay(const Value: Boolean);
begin
  FKbd.AutoCapsDisplay := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetAutoCompletion(
  const Value: TAdvSmoothTouchKeyBoardCompletion);
begin
  FKbd.AutoCompletion.Assign(Value);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetAutoPostKey(const Value: Boolean);
begin
  FKbd.AutoPostKey := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetBackGroundColor(const Value: TColor);
begin
  FBackGroundColor := Value;
  if Assigned(FFrm) then
    FFrm.Color := FBackGroundColor;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetColorTones(ATones: TColorTones);
begin
  FKbd.SetColorTones(ATones);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetComponentStyle(AStyle: TTMSStyle);
begin
  FKbd.SetComponentStyle(AStyle);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetFill(const Value: TGDIPFill);
begin
  FKbd.Fill.Assign(Value);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetHighlightAltGr(const Value: TColor);
begin
  FKbd.HighlightAltGr := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetHighlightCaps(const Value: TColor);
begin
  FKbd.HighlightCaps := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetImages(const Value: TImageList);
begin
  FKbd.Images := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetKeyboardHeight(const Value: integer);
begin
  FKbd.Height := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetKeyboardWidth(const Value: integer);
begin
  FKbd.Width := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetKeyDistance(const Value: Integer);
begin
  FKbd.KeyDistance := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetKeyDownOffset(const Value: Integer);
begin
  FKbd.KeyDownOffset := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetKeys(
  const Value: TAdvSmoothTouchKeyCollection);
begin
  FKbd.Keys.Assign(Value);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  FKbd.PictureContainer := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetShading(const Value: Boolean);
begin
  FKbd.Shading := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetShadingColor(const Value: TColor);
begin
  FKbd.ShadingColor := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetShadingColorTo(const Value: TColor);
begin
  FKbd.ShadingColorTo := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetShadingOpacity(const Value: Byte);
begin
  FKbd.ShadingOpacity := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetShadingOpacityTo(const Value: Byte);
begin
  FKbd.ShadingOpacityTo := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetSmallFont(const Value: TFont);
begin
  FKbd.SmallFont.Assign(Value);
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetSubKeyHidePause(const Value: integer);
begin
  FKbd.SubKeyHidePause := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetSubKeyShowPause(const Value: integer);
begin
  FKbd.SubKeyShowPause := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetText(const Value: String);
begin
  FKbd.Text := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SetUseFocusedControl(
  const Value: Boolean);
begin
  FKbd.UseFocusedControl := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.SeTAdvSmoothTouchKeyBoardType(const Value: TAdvSmoothTouchKeyBoardType);
begin
 FKbd.KeyboardType := Value;
end;

procedure TAdvSmoothPopupTouchKeyBoard.CreateForm;
begin
  FFrm := TAdvSmoothTouchKeyBoardToolForm.CreateNew(FOwnerForm);
  FFrm.FKeyboard := FKbd;
  FFrm.ShowCaption := FShowCaption;
  FFrm.ShowClose := FShowClose;
  FFrm.OnCloseQuery := FormCloseQuery;
  FFrm.BorderStyle := bsToolWindow;

  if Assigned(OnKeyboardCreated) then
    OnKeyboardCreated(Self);

  if ShowCaption then
  begin
    FFrm.Width := (FKbd.Width + GetSystemMetrics(SM_CXDLGFRAME) * 2) + FKbd.KeyDistance;
    FFrm.Height := (FKbd.Height + GetSystemMetrics(SM_CYDLGFRAME) * 2 + GetSystemMetrics(SM_CYSMCAPTION)) + FKbd.KeyDistance;
  end
  else
  begin
    FFrm.Width := FKbd.Width;
    FFrm.Height := FKbd.Height;
  end;

  //FFrm.Visible := false;
end;

procedure TAdvSmoothPopupTouchKeyBoard.Show;
var
  wnd: THandle;
begin
  wnd := GetActiveWindow;
  if not Assigned(FFrm) then
    CreateForm;

  FKbd.Parent := FFrm;
  FFrm.Visible := true;
  FTimer.Enabled := true;
  SetActiveWindow(wnd);

  if Assigned(OnShow) then
    OnShow(Self);

  if not UseFocusedControl then
    Self.Keyboard.FEditControl.SetFocus;

  FKbd.Loaded;
end;

procedure TAdvSmoothPopupTouchKeyBoard.ShowAtXY(AParent: TCustomForm; x, y: integer);
begin
  FOwnerForm := AParent;
  ShowAtXY(x,y);
end;

procedure TAdvSmoothPopupTouchKeyBoard.ShowAtXY(x, y: integer);
var
  wnd: THandle;
  w,h: integer;
begin
  if (FLastX = x) and (FLastY = y) then
    Exit;

  FLastX := x;
  FLastY := y;

  wnd := GetActiveWindow;
  if not Assigned(FFrm) then
    CreateForm;

  FKbd.Parent := FFrm;
  FKbd.Loaded;
  w := FFrm.Width;
  h := FFrm.Height;

  FDisableSizing := true;

  FFrm.Width := 0;
  FFrm.Height  := 0;
  FFrm.Left := X;
  FFrm.Top := Y;

  FFrm.Visible := true;

  FDisableSizing := false;
  MoveWindow(FFrm.Handle, X,Y, W, H, true);

  (*
  FFrm.Left := X;
  FFrm.Top := Y;
  FFrm.Width := w - 20 ;
  FFrm.Height := h;
  *)

  FTimer.Enabled := true;
  SetActiveWindow(wnd);


  if not UseFocusedControl then
    Self.Keyboard.FEditControl.SetFocus;
end;

procedure TAdvSmoothPopupTouchKeyBoard.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FKbd.FAutoCompletionForm) then
  begin
    FKbd.FAutoCompletionForm.Free;
    FKbd.FAutoCompletionForm := nil;
  end;
  FTimer.Enabled := False;
  if Assigned(FOnClose) then
    FOnClose(self);

  FLastX := -1;
  FLastY := -1;
end;

function TAdvSmoothPopupTouchKeyBoard.GetAutoCapsDisplay: Boolean;
begin
  Result := FKbd.AutoCapsDisplay;
end;

function TAdvSmoothPopupTouchKeyBoard.GetAutoCompletion: TAdvSmoothTouchKeyBoardCompletion;
begin
  Result := FKbd.AutoCompletion;
end;

function TAdvSmoothPopupTouchKeyBoard.GetAutoPostKey: Boolean;
begin
  Result := FKbd.AutoPostKey;
end;

function TAdvSmoothPopupTouchKeyBoard.GetFill: TGDIPFill;
begin
  Result := FKbd.Fill;
end;

function TAdvSmoothPopupTouchKeyBoard.GetHighlightAltGr: TColor;
begin
  Result := FKbd.HighlightAltGr;
end;

function TAdvSmoothPopupTouchKeyBoard.GetHighlightCaps: TColor;
begin
  Result := FKbd.HighlightCaps;
end;

function TAdvSmoothPopupTouchKeyBoard.GetImages: TImageList;
begin
  Result := FKbd.Images;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeyboardHeight: integer;
begin
  Result := FKbd.Height;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeyboardType: TAdvSmoothTouchKeyBoardType;
begin
  Result := FKbd.KeyboardType;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeyboardWidth: integer;
begin
  Result := FKbd.Width;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeyDistance: Integer;
begin
  Result := FKbd.KeyDistance;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeyDownOffset: Integer;
begin
  Result := FKbd.KeyDownOffset;
end;

function TAdvSmoothPopupTouchKeyBoard.GetKeys: TAdvSmoothTouchKeyCollection;
begin
  Result := FKbd.Keys;
end;

function TAdvSmoothPopupTouchKeyBoard.GetPictureContainer: TGDIPPictureContainer;
begin
  Result := FKbd.PictureContainer;
end;

function TAdvSmoothPopupTouchKeyBoard.GetShading: Boolean;
begin
  Result := FKbd.Shading;
end;

function TAdvSmoothPopupTouchKeyBoard.GetShadingColor: TColor;
begin
  Result := FKbd.ShadingColor;
end;

function TAdvSmoothPopupTouchKeyBoard.GetShadingColorTo: TColor;
begin
  Result := FKbd.ShadingColorTo;
end;

function TAdvSmoothPopupTouchKeyBoard.GetShadingOpacity: Byte;
begin
  Result := FKbd.ShadingOpacity;
end;

function TAdvSmoothPopupTouchKeyBoard.GetShadingOpacityTo: Byte;
begin
  Result := FKbd.ShadingOpacityTo;
end;

function TAdvSmoothPopupTouchKeyBoard.GetSmallFont: TFont;
begin
  Result := FKbd.SmallFont;
end;

function TAdvSmoothPopupTouchKeyBoard.GetSubKeyHidePause: integer;
begin
  Result := FKbd.SubKeyHidePause;
end;

function TAdvSmoothPopupTouchKeyBoard.GetSubKeyShowPause: integer;
begin
  Result := FKbd.SubKeyShowPause;
end;

function TAdvSmoothPopupTouchKeyBoard.GetText: String;
begin
  Result := FKbd.Text;
end;

function TAdvSmoothPopupTouchKeyBoard.GetUseFocusedControl: Boolean;
begin
  Result := FKbd.UseFocusedControl;
end;

procedure TAdvSmoothPopupTouchKeyBoard.TextChanged(Sender: TObject;
  var Text: String);
begin
  if Assigned(OnTextChanged) then
    OnTextChanged(Self, Text);
end;

procedure TAdvSmoothPopupTouchKeyBoard.TimerProc(Sender: TObject);
var
  wnd, awnd: THandle;
  r, wa: TRect;
  Selection: TSelection;
  pt: TPoint;
  isEdit, isCombo: boolean;
begin
  awnd := GetActiveWindow;
  if awnd = FFrm.Handle then
    Exit;

  if FAutoFollowFocus then
  begin
    isCombo := false;

    wnd := Windows.GetFocus;
    Selection.StartPos := -1;
    Selection.EndPos := -1;
    SendMessage(wnd, EM_GETSEL, Longint(@Selection.StartPos), LParam(@Selection.EndPos));

    // tests for edit, memo, spin, rich edit & descending components
    isEdit := Selection.StartPos <> -1;

    // tests for combo, datepicker
    if not isEdit then
    begin
      isCombo := SendMessage(wnd,CB_GETTOPINDEX,0,0) > 0;
      //isCombo := SendMessage(wnd,CB_GETCOUNT,0,0) > 0;
    end;

    if isEdit or isCombo then
    begin
      GetWindowRect(wnd,r);

      {$IFNDEF DELPHI7_LVL}

      SystemParametersInfo(SPI_GETWORKAREA, 0, @wa, 0);

      {$ENDIF}

      {$IFDEF DELPHI7_LVL}
      wa := Screen.MonitorFromPoint(Point(r.Left, r.Top)).WorkareaRect;
      {$ENDIF}

      // default X,Y pos
      pt.X := r.Left + 50;
      pt.Y := r.Bottom;

      if r.Left + 50 + FFrm.Width > wa.Right then
         pt.X := wa.Right - FFrm.Width;

      if r.Bottom + FFrm.Height > wa.Bottom then
         pt.Y := wa.Bottom - FFrm.Height;

      if pt.X < 0 then
        pt.X := 0;

      if pt.Y < 0 then
        pt.Y := 0;

      ShowAtXY(pt.X, pt.Y);
      //FFrm.Width := FKbd.Width;
    end
    else
    begin
      if FAutoHide then
      begin
        //FFrm.Width := 0;
        FFrm.Visible := false;
        FLastX := -1;
        FLastY := -1;
      end;
    end;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothSubKeyItem }

procedure TAdvSmoothSubKeyItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSubKeyItem) then
  begin
    FCaption := (Source as TAdvSmoothSubKeyItem).Caption;
    Changed;
  end;
end;

procedure TAdvSmoothSubKeyItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothSubKeyItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := (collection as TAdvSmoothSubKeyCollection).FOwner;
end;

destructor TAdvSmoothSubKeyItem.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothSubKeyItem.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

{ TAdvSmoothSubKeyCollection }

function TAdvSmoothSubKeyCollection.Add: TAdvSmoothSubKeyItem;
begin
  Result := TAdvSmoothSubKeyItem(inherited Add);
end;

constructor TAdvSmoothSubKeyCollection.Create(AOwner: TAdvSmoothTouchKeyBoard);
begin
  inherited Create(TAdvSmoothSubKeyItem);
  FOwner := AOwner;
end;

function TAdvSmoothSubKeyCollection.GetItem(
  Index: Integer): TAdvSmoothSubKeyItem;
begin
  Result := TAdvSmoothSubKeyItem(inherited Items[index]);
end;

function TAdvSmoothSubKeyCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothSubKeyCollection.Insert(
  index: integer): TAdvSmoothSubKeyItem;
begin
  Result := TAdvSmoothSubKeyItem(inherited insert(index));
end;

procedure TAdvSmoothSubKeyCollection.SetItem(Index: Integer;
  const Value: TAdvSmoothSubKeyItem);
begin
  Items[index] := Value;
end;

{ TAdvSmoothTouchKeyBoardForm }

procedure TAdvSmoothTouchKeyBoardForm.ClearBuffer(graphics: TGPGraphics);
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

function TAdvSmoothTouchKeyBoardForm.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothTouchKeyBoardForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothTouchKeyBoardForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothTouchKeyBoardForm.CreateParams(
  var Params: TCreateParams);
begin
  inherited;
end;

procedure TAdvSmoothTouchKeyBoardForm.CreateWnd;
begin
  inherited;
   { Hook parent }
  {$IFDEF DELPHI_UNICODE}
  if FKeyBoard.Owner is TForm then
    OldWndProc := TFarProc(GetWindowLongPtr((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC))
  else if FKeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
    OldWndProc := TFarProc(GetWindowLongPtr((FKeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC));
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  if FKeyBoard.Owner is TForm then
    OldWndProc := TFarProc(GetWindowLong((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC))
  else if FKeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
    OldWndProc := TFarProc(GetWindowLong((FKeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC));
  {$ENDIF}

  {$IFDEF DELPHI9_LVL}
  NewWndProc := Classes.MakeObjectInstance(HookWndProc);
  {$ELSE}
  NewWndProc := MakeObjectInstance(HookWndProc);
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  if FKeyBoard.Owner is TForm then
    SetWindowLongPtr((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc))
  else if FKeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
    SetWindowLongPtr((FKeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  if FKeyBoard.Owner is TForm then
    SetWindowLong((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc))
  else if FKeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
    SetWindowLong((FKeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}

  Invalidate;
end;

procedure TAdvSmoothTouchKeyBoardForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothTouchKeyBoardForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothTouchKeyBoardForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothTouchKeyBoardForm.Draw(graphics: TGPGraphics);
var
  g: TGPGraphics;
  ff: TGPFontFamily;
  f: TGPFont;
  sf: TGPStringFormat;
  fsl: integer;
  b, bc, subkb: TGPBrush;
  r, rd, rgl: TGPRectF;
  c: TColor;
  path: TGPGraphicsPath;
  p: TGPPen;
  I: integer;
  rsub: TGPRectF;
  gbrush: TGPPathGradientBrush;
  cb: array[0..2] of TGPColor;
  pb: array[0..2] of single;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Assigned(KeyBoard) and Assigned(KeyBoardItem) then
  begin
    with KeyBoard do
    begin
      ff := TGPFontFamily.Create(KeyBoard.Font.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fsl := 0;
      if (fsBold in KeyBoard.Font.Style) then
        fsl := fsl + 1;
      if (fsItalic in KeyBoard.Font.Style) then
        fsl := fsl + 2;
      if (fsUnderline in KeyBoard.Font.Style) then
        fsl := fsl + 4;

      sf := TGPStringFormat.Create(0);
      f := TGPFont.Create(ff, KeyBoard.Font.Size , fsl, UnitPoint);
      subkb := TGPSolidBrush.Create(MakeColor(255, KeyBoard.Font.Color));

      r := MakeRect(0, 2, Self.Width - 2, Self.Height - 4);

      path := CreateRoundRectangle(r, 6);
      gBrush := TGPPathGradientBrush.Create(path);
      gBrush.SetWrapMode(WrapModeClamp);

      cb[0] := MakeColor(0,0,0,0);
      cb[1] := MakeColor(75, clBlack);
      cb[2] := MakeColor(75, clBlack);

      pb[0] := 0;
      pb[1] := 0.1;
      pb[2] := 1;

      gBrush.SetInterpolationColors(@cb, @pb, 3);

      g.FillPath(gbrush, path);
      path.Free;
      gBrush.Free;

      rd := MakeRect(6, 6, Self.Width - 1 - 12, Self.Height - 1 - 12);
      with KeyBoardItem do
      begin
        c := KeyBoardItem.ColorDown;
        b := TGPSolidBrush.Create(Makecolor(255, c));
        path := CreateRoundRectangle(rd, 4);
        g.FillPath(b, path);
        b.Free;

        bc := TGPSolidBrush.Create(MakeColor(75, clWhite));
        p := TGPPen.Create(MakeColor(50, clBlack));
        for I := 0 to SubKeys.Count - 1 do
        begin
          rsub := SubKeys[I].FSubKeyRect;
          g.FillRectangle(bc, rsub);
          g.DrawRectangle(p, rsub);

          g.DrawString(SubKeys[I].Caption, Length(SubKeys[I].Caption), f, SubKeys[I].FSubKeyCaptionRect, sf, subkb);
        end;
        p.Free;
        bc.Free;

        rgl := MakeRect(rd.X, rd.Y, rd.Width, rd.Height / 2);
        path := CreateRoundRectangle(rgl, 4);
        b := TGPLinearGradientBrush.Create(MakeRect(rgl.X - 1, rgl.Y - 1, rgl.Width + 1, rgl.Height + 1), MakeColor(100, clWhite),
        MakeColor(40, clWhite), LinearGradientModeVertical);
        g.FillPath(b, path);
        path.Free;
        b.Free;

        p := TGPPen.Create(Makecolor(255, clBlack));
        path := CreateRoundRectangle(rd, 4);
        g.DrawPath(p, path);
        path.Free;
        p.free;

        p := TGPPen.Create(Makecolor(255, c), 5);
        g.DrawLine(p, rd.X, r.Height - 2, Width + 5, r.Height - 2);
        p.free;

      end;


      subkb.Free;
      sf.Free;
      f.Free;
      ff.Free;
    end;
  end;

  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothTouchKeyBoardForm.HookWndProc(var Msg: TMessage);
var
  pt: TPoint;
begin
  if Assigned(FKeyBoard) then
  begin
    if FKeyBoard.Owner is TForm then
      Msg.Result := CallWindowProc(OldWndProc, (FKeyBoard.Owner as TForm).Handle, Msg.Msg , Msg.wParam, Msg.lParam)
    else if FKeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
      Msg.Result := CallWindowProc(OldWndProc, (FKeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, Msg.Msg , Msg.wParam, Msg.lParam);

    case Msg.Msg of
     WM_ACTIVATE: PostMessage(Self.Handle, WM_USERACTIVATE, Msg.WParam, 0);
     WM_WINDOWPOSCHANGING, WM_SIZE:
     begin
       pt := FKeyBoard.ClientToScreen(Point(KeyBoardItem.X, KeyBoardItem.Y));
       Self.Left := pt.X - 6;
       Self.Top := pt.Y - Self.Height + 6;
     end;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoardForm.Init;
begin
  DoubleBuffered := true;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  FormStyle := fsStayOnTop;
  Color := clWhite;

  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  subkey: integer;
  str: string;
  i: integer;
begin
  inherited;
  if Assigned(FKeyBoard) then
    FKeyBoard.FSubKeyCheck := 0;
  if Assigned(KeyBoardItem) and Assigned(KeyBoard) then
  begin
    subkey := KeyBoardItem.XYToSubKey(X, Y);
    if subkey <> -1 then
    begin
      str := KeyBoardItem.SubKeys[subkey].Caption;
      for I := 1 to Length(str) do
        SendMessage(KeyBoard.KeybdInputHandle, WM_CHAR, Ord(str[I]),0)
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoardForm.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Assigned(FKeyBoard) then
    FKeyBoard.FSubKeyCheck := 0;
end;

procedure TAdvSmoothTouchKeyBoardForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  item: Integer;
begin
  inherited;
  if Assigned(FKeyBoard) then
    FKeyBoard.FSubKeyCheck := 0;

  if Assigned(FKeyBoardItem) and Assigned(KeyBoard) then
  begin
    Item := KeyBoardItem.XYToSubKey(X, Y);
    if Item <> -1 then
    begin
      with KeyBoard do
      begin
        if CheckAutoCompletionsubKey(KeyBoardItem.SubKeys[item]) then
          AutoCompletion.FLookup := AutoCompletion.FLookup + KeyBoardItem.SubKeys[item].Caption
        else
          AutoCompletion.FLookup := '';

        if (Length(AutoCompletion.FLookup) >= AutoCompletion.LookupFromChars) then
          AutoCompletion.UpdateCompletion(AutoCompletion.FLookup);

        case AutoCompletion.Mode of
          cmAutoVisible:
          begin
            if (Length(AutoCompletion.FLookup) >= AutoCompletion.LookupFromChars) and (AutoCompletion.FCompletion.Columns[0].CustomItems.Count > 0) then
            begin
              if not Assigned(FAutoCompletionForm) then
                ShowAutoCompletionForm;
            end
            else if Assigned(FAutoCompletionForm) then
            begin
              FAutoCompletionForm.Free;
              FAutoCompletionForm := nil;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothTouchKeyBoardForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardForm.SetPosition;
begin

end;

procedure TAdvSmoothTouchKeyBoardForm.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow(255);
end;

procedure TAdvSmoothTouchKeyBoardForm.UpdateMainWindow(Alpha: Byte);
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
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
end;

procedure TAdvSmoothTouchKeyBoardForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothTouchKeyBoardForm.WMMouseActivate(
  var Msg: TWMMouseActivate);
begin
  msg.result := MA_NOACTIVATE;
end;

procedure TAdvSmoothTouchKeyBoardForm.WndProc(var Message: TMessage);
begin
  if Assigned(FKeyBoard) then
  begin
    if Message.Msg = WM_DESTROY then
    begin
      if (fKeyboard.Owner is TForm) and (fKeyboard.Owner as TForm).HandleAllocated then
      begin
      {$IFDEF DELPHI_UNICODE}
        if KeyBoard.Owner is TForm then
          SetWindowLongPtr((KeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
        else if KeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
          SetWindowLongPtr((KeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
        if KeyBoard.Owner is TForm then
          SetWindowLong((KeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
        else if KeyBoard.Owner is TAdvSmoothTouchKeyBoardToolForm then
          SetWindowLong((KeyBoard.Owner as TAdvSmoothTouchKeyBoardToolForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
      {$ENDIF}
      end;
    end
    else if Message.Msg = WM_USERACTIVATE then
    begin
      case Message.WParam of
        0: ShowWindow(Self.Handle, SW_HIDE);
        1, 2: ShowWindow(Self.Handle, SW_SHOWNA);
      end;
    end;
  end;
  inherited;
end;

procedure TAdVSmoothCompletion.ColumnAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdVSmoothCompletion.ColumnsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdVSmoothCompletion.Animate(Sender: TObject);
var
  d: single;
  posTo: integer;
  i: integer;
begin
  if (csDesigning in ComponentState) or (Columns.Count = 0) then
    Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      if FAnimate and Visible and Enabled then
      begin
        posTo := FScPosTo;
        d := Abs(posto - FCurrentScPos) / 5;
        FAnimating := AnimateDouble(FCurrentScPos, posto, d, 1);
        if FAnimating then
        begin
          if Cyclic then
          begin
            if FScPosTo >= (GetRangeCount + 1) * ColumnAppearance.TextSpacing then
            begin
              FCurrentScPos := FCurrentScPos - (GetRangeCount * ColumnAppearance.TextSpacing) - ColumnAppearance.TextSpacing;
              FScPosTo := FScPosTo - (GetRangeCount * ColumnAppearance.TextSpacing) - ColumnAppearance.TextSpacing;
              SetAnimatedValue;
            end
            else if FScPosTo <= -ColumnAppearance.TextSpacing then
            begin
              FCurrentScPos := FCurrentScPos + (GetRangeCount * ColumnAppearance.TextSpacing) +  ColumnAppearance.TextSpacing;
              FScPosTo := FScPosTo + (GetRangeCount * ColumnAppearance.TextSpacing) +  ColumnAppearance.TextSpacing;
              SetAnimatedValue;
            end;
          end;
          Changed;
        end
        else
        begin
          FAnimate := false;
          SetAnimatedValue;
          if Assigned(FOwner.FOnSelectedValueChanged) then
            FOwner.FOnSelectedValueChanged(Self, Index, Round(FselectedValue - GetRangeFrom), FSelectedValue, RangeType);
        end;
      end;
    end;
  end;
end;

procedure TAdVSmoothCompletion.Assign(Source: TPersistent);
begin
  if (Source is TAdVSmoothCompletion) then
  begin
    FColumnAppearance.Assign((Source as TAdVSmoothCompletion).ColumnAppearance);
    FFill.Assign((Source as TAdVSmoothCompletion).Fill);
    FSelectedFill.Assign((Source as TAdVSmoothCompletion).SelectedFill);
    FSelectedHeight := (Source as TAdVSmoothCompletion).SelectedHeight;
    FColumns.Assign((Source as TAdVSmoothCompletion).Columns);
    FAnimationFactor := (Source as TAdVSmoothCompletion).AnimationFactor;
    FReadOnly := (Source as TAdVSmoothCompletion).ReadOnly;
    FTopLayerFill.Assign((Source as TAdVSmoothCompletion).TopLayerFill);
    FBottomLayerFill.Assign((Source as TAdVSmoothCompletion).BottomLayerFill);
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.BeginUpdate;
begin
  inc(FUpdatecount);
end;

procedure TAdvSmoothCompletion.Changed;
begin
  if Assigned(FOnChange) and (FUpdatecount = 0) then
    FOnChange(Self);
end;

procedure TAdVSmoothCompletion.CMEnabledChanged(var Message: TMessage);
begin
  Changed;
end;

procedure TAdVSmoothCompletion.CMHintShow(var Message: TMessage);
var
  c: integer;
  pt: TPoint;
  dColumn: TAdVSmoothCompletionColumn;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := self.Hint;
    pt := CursorPos;
    c := XYToColumn(pt.X, pt.Y);
    if c <> -1 then
    begin
      dColumn := Columns[c];
      if dColumn <> nil then
      begin
        if dColumn.Hint <> '' then
          HintStr := dColumn.Hint;
        if Assigned(FOnColumnHint) then
          FOnColumnHint(Self, c, HintStr);
      end;
    end;
    ReshowTimeout := 0;
  end;
end;

procedure TAdVSmoothCompletion.CMMouseLeave(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  FHoveredColumn := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      FMouseUp := true;
      FMouseDown := false;
    end;
  end;

  Application.CancelHint;
  Changed;
end;

constructor TAdvSmoothCompletion.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  Width := 200;
  Height := 128;
  FColumnAppearance := TAdvSmoothCompletionColumnAppearance.Create(Self);
  FColumnAppearance.OnChange := ColumnAppearanceChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FSelectedFill := TGDIPFill.Create;
  FSelectedFill.OnChange := FillChanged;
  FColumns := TAdvSmoothCompletionColumns.Create(Self);
  FColumns.OnChange := ColumnsChanged;
  FSelectedHeight := 30;
  FAnimationFactor := 4;
  FHoveredColumn := -1;
  FAnimateTimer := TTimer.Create(Self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.Enabled := true;
  FAnimateTimer.OnTimer := Animate;
  FReadOnly := false;
  FTopLayerFill := TGDIPFill.Create;
  FTopLayerFill.OnChange := FillChanged;
  FBottomLayerFill := TGDIPFill.Create;
  FBottomLayerFill.OnChange := FillChanged;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvSmoothCompletion.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;

  if FDesignTime then
  begin
    InitPreview;
  end;

  FConstructed := true;
end;

destructor TAdvSmoothCompletion.Destroy;
begin
  FAnimateTimer.free;
  FColumnAppearance.Free;
  FFill.Free;
  FSelectedFill.Free;
  FColumns.Free;
  FTopLayerFill.Free;
  FBottomLayerFill.Free;
  inherited;
end;

procedure TAdvSmoothCompletion.DoEnter;
begin
  inherited;
  if ReadOnly then
    Exit;

  FFocused := true;
  Changed;
end;

procedure TAdvSmoothCompletion.DoExit;
begin
  inherited;
  if ReadOnly then
    Exit;

  FFocused := false;
  Changed;
end;

procedure TAdvSmoothCompletion.Draw(g: TGPGraphics; r: TRect);
var
  rgn: TGPRegion;
  rb: TGPRectF;
  rc: TRect;
begin
  FRect := r;
  rb := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
  DrawBackGround(g, rb);

  rc := InsideRect;
  rgn := TGPRegion.Create(columns[0].GetColumnRect);

  g.SetClip(rgn);

  DrawColumns(g);
  DrawTopLayer(g);

  DrawSelectedLayer(g);

  g.ResetClip;
  rgn.Free;
end;

procedure TAdvSmoothCompletion.DrawBackGround(g: TGPGraphics; r: TGPRectF);
begin
  Fill.Fill(g, r);
end;

procedure TAdvSmoothCompletion.DrawColumns;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    with Columns[I] do
      if Visible then
        Draw(g, GetColumnRect);
end;

procedure TAdvSmoothCompletion.DrawSelectedLayer(g: TGPGraphics);
var
  r: TRect;
begin
  r := GetSelectedRect;
  FSelectedFill.Fill(g, MakeRect(r.Left, r.Top, R.Right, r.Bottom - r.Top));
end;

procedure TAdvSmoothCompletion.DrawTopLayer(g: TGPGraphics);
var
  rt: TRect;
begin
  rt := FRect;
  TopLayerFill.Fill(g, MakeRect(rt.Left, rt.Top, rt.Right - rt.Left, 36));
  BottomLayerFill.Fill(g, MakeRect(rt.Left, rt.Bottom - 37, rt.Right - rt.Left, 37));
end;

procedure TAdvSmoothCompletion.EndUpdate;
begin
  Dec(FUpdatecount);
  if FUpdatecount = 0 then
    Changed;
end;

procedure TAdvSmoothCompletion.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothCompletion.GetSelectedRect: TRect;
var
  h, hs: integer;
begin
  h := (InsideRect.Bottom - InsideRect.Top) div 2;
  hs := SelectedHeight div 2;
  Result := Rect(InsideRect.Left + 6, h - hs, InsideRect.Right - InsideRect.Left - 12, h + hs);
end;

function TAdvSmoothCompletion.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothCompletion.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothCompletion.InitPreview;
var
  i: integer;
begin
  for I := 0 to 2 do
    Columns.Add;
end;

function TAdvSmoothCompletion.InsideRect: TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (Fill.ShadowColor <> clNone) {and not Transparent} then
    sh := Fill.ShadowOffset;

  Result := FRect;
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

procedure TAdvSmoothCompletion.KeyDown(var Key: Word; Shift: TShiftState);
var
  sel: Double;
  fc: integer;
begin
  inherited;
  if ReadOnly then
    Exit;

  fc := FFocusedColumn;
  case Key of
    VK_LEFT: fc := fc - 1;
    VK_RIGHT: fc := fc + 1;
  end;

  fc := Min(Columns.Count - 1, Max(0, fc));

  if Columns[fc].Enabled and Columns[fc].Visible then
    FFocusedColumn := fc
  else
    Exit;

  with Columns[FFocusedColumn] do
  begin
    sel := FSelectedValue;
    case Key of
      VK_UP: sel := IncSteps(FSelectedValue, -1);
      VK_DOWN: sel := IncSteps(FSelectedValue, 1);
      VK_HOME: sel := GetRangeFrom;
      VK_END: sel := GetRangeTo;
      VK_PRIOR: sel := IncSteps(FSelectedValue, -10);
      VK_NEXT: sel := IncSteps(FSelectedValue, 10);
    end;

    if Cyclic then
    begin
      if sel = IncSteps(GetRangeFrom, 1) then
        sel := GetRangeFrom
      else if sel = IncSteps(GetRangeFrom, -1) then
        sel := GetRangeTo;
    end;

    sel := Min(GetRangeTo, Max(GetRangeFrom, sel));

    if (FSelectedValue <> sel) then
      ScrollToValue(sel, false);

    Changed;
  end;

end;

procedure TAdvSmoothCompletion.Loaded;
var
  di, i: integer;
begin
  inherited;
  di := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      if Visible then
      begin
        FDrawIndex := di;
        Inc(di);
      end;
      SelectedValue := GetRangeFrom;
    end;
  end;
end;

procedure TAdvSmoothCompletion.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  c: TAdvSmoothCompletionColumn;
  i: integer;
begin
  inherited;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;

  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      FMouseDown := true;
      FDragY := Y;
      FScrollY := Y;
      FTimeStart := GetTickCount;
      FClickY := Y;
      FClickX := X;
    end;
  end;
end;

procedure TAdvSmoothCompletion.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  c: TAdvSmoothCompletionColumn;
begin
  inherited;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;

  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      if (csDesigning in ComponentState) then
      begin
        FMouseDown := false;
        FMouseUp := false;
        exit;
      end;

      if FMouseDown then
      begin
        if ((FDragY < y-ColumnAppearance.TextSpacing / 2) or (FDragY > Y+ColumnAppearance.TextSpacing / 2)) then
        begin
          FSp := 2;
          FAnimate := false;

          if (Y - FDragY) > 0 then
          begin
            FCurrentScPos := FCurrentScPos - Abs(Y - FDragY);
            while not ((FcurrentScPos mod ColumnAppearance.TextSpacing) = 0) do
              FcurrentScpos := FCurrentScpos - 1;
          end
          else
          begin
            FCurrentScPos := FCurrentScPos + Abs(Y - FDragY);
            while not ((FcurrentScPos mod ColumnAppearance.TextSpacing) = 0) do
              FcurrentScpos := FCurrentScpos + 1;
          end;

          if Cyclic then
          begin
            if FCurrentScPos = (GetRangeCount + 1) * ColumnAppearance.TextSpacing then
              FcurrentScPos := 0
            else if FCurrentScPos = -ColumnAppearance.TextSpacing then
              FcurrentScPos := GetRangeCount * ColumnAppearance.TextSpacing;
          end;

          FcurrentScPos := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, FcurrentScPos));
          FSelectedValue := IncSteps(GetRangeFrom, FCurrentScPos div ColumnAppearance.TextSpacing);

          if Assigned(FOnSelectedValueChanged) then
            FOnSelectedValueChanged(Self, c.Index, Round(FSelectedvalue - GetRangeFrom), FSelectedValue, RangeType);

          FDragY := Y;
          FScPosTo := FCurrentScPos;
          Changed;
        end;
      end
      else
      begin
        if FMouseUp then
        begin
          FMouseUp := false;

          if ((FTimeStop - FTimeStart) > 500) or ((FTimeStop - FTimeStart) = 0) then
            exit;

          FSp := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
          if FSp > 0 then
          begin
            if (Y - FScrollY) > 0 then
            begin
              FScPosTo := FScPosTo - Round(Abs(Y - FScrollY) * FSp);
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo - 1;
            end
            else
            begin
              FScPosTo := FScPosTo + Round(Abs(Y - FScrollY) * FSp);
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo + 1;
            end;

            if not Cyclic then
              FScPosTo := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
          end;
        end;
      end;
    end;
  end;

  if i <> FHoveredColumn then
  begin
    FHoveredColumn := i;
    Application.CancelHint;
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: integer;
  c: TAdvSmoothCompletionColumn;
begin
  inherited;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;

  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      if FAnimating and FAnimate then
      begin
        FAnimate := false;
        FScrollY := FCurrentScPos;
        FScPosTo := FCurrentScPos;
        FTimeStart := 0;
        FTimeStop := 0;
      end;
      FMouseDown := false;
      FMouseUp := true;
      FTimeStop := GetTickCount;
      FAnimate := (FTimeStop - FTimeStart > 0);
    end;
    FFocusedColumn := i;
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  if ReadOnly then
    Exit;

  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if (FHoveredColumn > -1) and (FHoveredColumn < Columns.Count) then
      begin
        with Columns[FHoveredColumn] do
        begin
          if integer(Message.WParam) < 0 then
          begin
            FSp := FAnimationFactor;
            FScPosTo := FScPosTo + ColumnAppearance.TextSpacing
          end
          else
          begin
            FSp := FAnimationFactor;
            FScPosTo := FScPosTo - ColumnAppearance.TextSpacing
          end;
          if not Cyclic then
            FScPosTo := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
          FAnimate := true;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothCompletion.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;

    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;
  end;
  inherited;
end;

procedure TAdvSmoothCompletion.SetAnimationFactor(const Value: integer);
begin
  if FAnimationFactor <> value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.SetBottomLayerFill(const Value: TGDIPfill);
begin
  if FBottomLayerFill <> value then
  begin
    FBottomLayerFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetColumnAppearance(
  const Value: TAdvSmoothCompletionColumnAppearance);
begin
  if FColumnAppearance <> value then
  begin
    FColumnAppearance.Assign(Value);
    ColumnAppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetColumns(const Value: TAdvSmoothCompletionColumns);
begin
  if FColumns <> value then
  begin
    FColumns := Value;
    ColumnsChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.SetSelectedFill(const Value: TGDIPFill);
begin
  if FSelectedFill <> value then
  begin
    FSelectedFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetSelectedHeight(const Value: integer);
begin
  if FSelectedHeight <> value then
  begin
    FSelectedHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletion.SetTopLayerFill(const Value: TGDIPFill);
begin
  if FTopLayerFill <> Value then
  begin
    FTopLayerFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletion.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothCompletion.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if ReadOnly then
    Exit;

  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothCompletion.WMPaint(var Message: TWMPaint);
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

function TAdvSmoothCompletion.XYToColumn(X, Y: integer): integer;
var
  I: Integer;
  r: TGPRectF;
begin
  Result := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    with FColumns[i] do
    begin
      if FColumns[i].Visible and Enabled then
      begin
        r := GetColumnRect;
        if PtInRect(Bounds(Round(R.X), Round(R.Y), Round(R.Width), Round(R.Height)), Point(X, Y)) then
        begin
          result := I;
          break;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothCompletion.XYToCustomItem(X, Y: integer): integer;
var
  col: Integer;
  I: integer;
begin
  Result := -1;
  col := XYToColumn(X, Y);
  if col <> -1 then
  begin
    with Columns[col] do
    begin
      for I := 0 to CustomItems.Count - 1 do
      begin
        if PtinGPRect(CustomItems[I].FRect, Point(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

{ TAdvSmoothCompletionAppearance }

procedure TAdvSmoothCompletionColumnAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothCompletionColumnAppearance) then
  begin
    FAutoSize := (Source as TAdvSmoothCompletionColumnAppearance).AutoSize;
    FAllowHovering := (Source as TAdvSmoothCompletionColumnAppearance).AllowHovering;
    FSpacing := (Source as TAdvSmoothCompletionColumnAppearance).Spacing;
    FFill.Assign((Source as TAdvSmoothCompletionColumnAppearance).Fill);
    FHoverFill.Assign((Source as TAdvSmoothCompletionColumnAppearance).HoverFill);
    FDisabledFill.Assign((Source as TAdvSmoothCompletionColumnAppearance).DisabledFill);
    FTextSpacing := (Source as TAdvSmoothCompletionColumnAppearance).TextSpacing;
    FImageWidth := (Source as TAdvSmoothCompletionColumnAppearance).ImageWidth;
    FImageHeight := (Source as TAdvSmoothCompletionColumnAppearance).ImageHeight;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothCompletionColumnAppearance.Create(AOwner: TAdvSmoothCompletion);
begin
  FOwner := AOwner;
  FAutoSize := True;
  FSpacing := 5;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FDisabledFill := TGDIPFill.Create;
  FDisabledFill.OnChange := FillChanged;
  FHoverFill := TGDIPFill.Create;
  FHoverFill.OnChange := FillChanged;
  FTextSpacing := 25;
  FImageWidth := 30;
  FImageHeight := 30;
  FAllowHovering := true;
end;

destructor TAdvSmoothCompletionColumnAppearance.Destroy;
begin
  FFill.Free;
  FHoverFill.Free;
  FDisabledFill.Free;
  inherited;
end;

procedure TAdvSmoothCompletionColumnAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCompletionColumnAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetAllowHovering(
  const Value: Boolean);
begin
  if FAllowHovering <> value then
  begin
    FAllowHovering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetDisabledFill(
  const Value: TGDIPFill);
begin
  if FDisabledFill <> value then
  begin
    FDisabledFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetHoverFill(
  const Value: TGDIPFill);
begin
  if FHoverFill <> value then
  begin
    FHoverFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetImageHeight(
  const Value: integer);
begin
  if FImageHeight <> value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetImageWidth(const Value: integer);
begin
  if FImageWidth <> value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetTextSpacing(
  const Value: integer);
var
  I: Integer;
begin
  if FTextSpacing <> value then
  begin
    FTextSpacing := Max(1, Value);
    for I := 0 to FOwner.Columns.Count - 1 do
    begin
      with FOwner.Columns[i] do
      begin
        FCurrentScPos := StepsFromTo(GetRangeFrom, FSelectedValue) * FOwner.ColumnAppearance.TextSpacing;
        FScPosTo := FcurrentScPos;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumnAppearance.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

{ TAdvSmoothCompletionColumn }

procedure TAdvSmoothCompletionColumn.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothCompletionColumn) then
  begin
    FEnabled := (Source as TAdvSmoothCompletionColumn).Enabled;
    FVisible := (Source as TAdvSmoothCompletionColumn).Visible;
    FWidth := (Source as TAdvSmoothCompletionColumn).Width;
    FRangeFrom := (Source as TAdvSmoothCompletionColumn).RangeFrom;
    FRangeTo := (Source as TAdvSmoothCompletionColumn).RangeTo;
    FSelectedValue := (Source as TAdvSmoothCompletionColumn).SelectedValue;
    FRangeType := (Source as TAdvSmoothCompletionColumn).RangeType;
    FDateRangeTo := (Source as TAdvSmoothCompletionColumn).DateRangeTo;
    FDateRangeFrom := (Source as TAdvSmoothCompletionColumn).DateRangeFrom;
    FDateTimeValueFormat := (Source as TAdvSmoothCompletionColumn).DateTimeValueFormat;
    FCustomItems.Assign((Source as TAdvSmoothCompletionColumn).CustomItems);
    FCyclic := (Source as TAdvSmoothCompletionColumn).Cyclic;
    FFont.Assign((Source as TAdvSmoothCompletionColumn).Font);
    FTextAlign := (Source as TAdvSmoothCompletionColumn).TextAlign;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothCompletionColumn.Create(Collection: TCollection);
begin
  inherited;
  FDrawIndex := Index;
  Fowner := (Collection as TAdvSmoothCompletionColumns).FOwner;
  FEnabled := true;
  FVisible := true;
  FWidth := 20;
  FRangeFrom := 0;
  FRangeTo := 20;
  FRangeType := rtNumber;
  FStep := 1;
  Fowner.Changed;
  FValueFormat := '%g';
  FDateRangeFrom := Now;
  FDateRangeTo := Now + 20;
  FSaveDateRangeto := FDateRangeto;
  FSaveDateRangefrom := FDateRangeFrom;
  FDateTimeValueFormat := 'dd/mm/yy';
  FStepType := stNumber;
  FCustomItems := TAdvSmoothCompletionCustomItems.Create(FOwner);
  FCustomItems.OnChange := CustomItemsChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FOnlyDate := false;
  FTextAlign := taCenter;
end;

procedure TAdvSmoothCompletionColumn.CustomItemsChanged(Sender: TObject);
begin
  Changed;
end;

destructor TAdvSmoothCompletionColumn.Destroy;
begin
  FCustomItems.Free;
  FFont.free;
  if not (csDestroying in FOwner.componentState) then
  begin
    Fowner.Changed;
  end;
  inherited;
end;

procedure TAdvSmoothCompletionColumn.Draw(g: TGPGraphics; r: TGPRectF);
var
  I: Integer;
  f: TGPFont;
  fs, dw: integer;
  sf: TGPStringFormat;
  ff: TGPFontFamily;
  b: TGPSolidBrush;
  s: String;
  sri: TGPRectF;
  pttext, ptimg: TGPPointF;
  d: Double;
  ypostext, yposimg: Double;
  sci, cnt, j, si, siw, sih: integer;
  focus: Boolean;
  st: String;
  cdt, cdf: integer;
begin
  with FOwner.FColumnAppearance do
  begin
    //fill
    focus := FOwner.TabStop and FOwner.FFocused and (FOwner.FFocusedColumn = Index);
    if FEnabled and FOwner.Enabled then
    begin
      if (FOwner.FHoveredColumn = Index) and AllowHovering then
      begin
        FhoverFill.BeginUpdate;
        FhoverFill.FocusRect := r;
        FHoverFill.Focus := focus;
        FHoverFill.EndUpdate;
        FHoverFill.Fill(g, r)
      end
      else
      begin
        FFill.BeginUpdate;
        FFill.FocusRect := r;
        FFill.Focus := focus;
        FFill.EndUpdate;
        FFill.Fill(g, r)
      end
    end
    else
      FDisabledFill.Fill(g, r);

    if RangeType <> rtCustom then
    begin
      if GetRangeCount <= 0 then
        Exit;
    end;

    //textfont not selected
    ff := TGPFontFamily.Create(Font.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in Font.Style) then
      fs := fs + 1;
    if (fsItalic in Font.Style) then
      fs := fs + 2;
    if (fsUnderline in Font.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create(GDIP_NOWRAP);
    sf.SetTrimming(StringTrimmingEllipsisWord);
    f := TGPFont.Create(ff, Font.Size, fs, UnitPoint);
    b := TGPSolidBrush.Create(ColorToARGB(Font.Color));
    g.MeasureString('gh', length('gh'), f, r, sf, sri);

    cnt := Round((FOwner.InsideRect.Bottom - FOwner.InsideRect.Top) / ((TextSpacing / 2) + sri.Height)) + 5;
    i := (FCurrentScPos div TextSpacing) - (cnt div 2);
    j := 0;
    si := -1;
    siw := 0;
    sih := 0;
    while j <= cnt do
    begin
      if (i < 0) and Cyclic then
        sci := ((i mod (GetRangeCount + 1) + GetRangeCount + 1) mod (GetRangeCount + 1))
      else if (i > GetRangeCount) and Cyclic then
        sci := ((i mod (GetRangeCount + 1)) mod (GetRangeCount + 1))
      else
        sci := i;

      d := IncSteps(GetRangeFrom, sci);
      cdt := CompareDateTime(d, GetRangeTo);
      cdf := CompareDateTime(d, GetRangeFrom);
      if (((cdf = 0) or (cdf = 1)) and ((cdt = 0) or (cdt = -1))) or Cyclic then
      begin
        case RangeType of
          rtNumber:
          begin
            if pos('d',ValueFormat) > 0 then
            begin
              dw := round(d);
              try
                s := Format(ValueFormat,[dw]);
              except
                s := '';
              end;
            end
            else
            begin
              try
                s := Format(ValueFormat, [d]);
              except
                s := '';
              end;
            end;
          end;
          rtDateTime:
          begin
            try
              s := FormatDateTime(DateTimeValueFormat, d);
            except
              s := '';
            end;
          end;
          rtCustom:
          begin
            with CustomItems[sci] do
            begin
              s := Text;
              if Assigned(FOwner) then
              begin
                if Assigned(FOwner.FImages) then
                begin
                  si := ImageIndex;
                  if (si > -1) and (si < FOwner.FImages.Count) then
                  begin
                    siw := FOwner.FImages.Width;
                    sih := FOwner.FImages.Height;
                  end;
                end
                else if Assigned(FOwner.FContainer) then
                begin
                  st := PictureName;
                  if Assigned(FOwner.FContainer.FindPicture(st)) then
                  begin
                    siw := ImageWidth;
                    sih := ImageHeight;
                  end;
                end;
              end;
            end;
          end;
        end;

        if s <> '' then
        begin
          ypostext := -FCurrentScPos + (i * TextSpacing) + (FOwner.InsideRect.Bottom - (sri.Height)) / 2;
          g.MeasureString(s, length(s), f, r, sf, sri);
          case TextAlign of
            taLeftJustify: pttext := MakePoint(r.X + 10, ypostext);
            taRightJustify: pttext := MakePoint(r.X + ((r.Width - sri.Width - siw)), ypostext);
            taCenter: pttext := MakePoint(r.X + ((r.Width - sri.Width - siw) / 2), ypostext);
          end;
        end;

        yposimg := -FCurrentScPos + (i * TextSpacing) + (FOwner.InsideRect.Bottom - (sih)) / 2;
        if s <> '' then
          ptimg := MakePoint(pttext.X, yposimg)
        else
          ptimg := MakePoint(r.X + ((r.Width - siw) / 2), yposimg);

        if Assigned(FOwner.FContainer) then
        begin
          if Assigned(FOwner.Fcontainer.FindPicture(st)) then
            FOwner.FContainer.FindPicture(st).GDIPDraw(g, Bounds(Round(ptimg.x), Round(ptimg.y), siw, sih));
        end
        else if Assigned(FOwner.FImages) then
        begin
          if (si > -1) and (si < FOwner.FImages.Count) then
            FOwner.FImages.Draw(FOwner.Canvas, Round(ptimg.x), Round(ptimg.y), si);
        end;

        pttext.X := pttext.X + siw;
        if s <> '' then
          g.DrawString(s, Length(s), f, MakeRect(pttext.X, pttext.Y, sri.Width, sri.Height), sf, b);

        CustomItems[sci].FRect := MakeRect(r.X, pttext.Y, r.Width, sri.Height);
      end;

      Inc(j);
      Inc(i);
    end;

    b.Free;
    f.Free;
    sf.Free;
    ff.Free;
  end;
end;

procedure TAdvSmoothCompletionColumn.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothCompletionColumn.GetColumnRect: TGPRectF;
var
  r: TRect;
  s, st: Double;
  i: integer;
  cc: integer;
begin
  with FOwner do
  begin
    cc := 0;
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Visible then
        Inc(cc);

    r := Fowner.InsideRect;
    if ColumnAppearance.AutoSize then
    begin
      s := (r.Right - r.Left - ((cc + 1) * ColumnAppearance.Spacing)) / cc;
      st := r.Left + (FDrawIndex * s) + (FDrawIndex + 1) * ColumnAppearance.Spacing;
    end
    else
    begin
      s := Self.Width;
      i := FDrawIndex;
      st := r.Left;
      while (i > 0) do
      begin
        st := st + Columns[i - 1].Width + ColumnAppearance.Spacing;
        Dec(i);
      end;
    end;

    Result := Makerect(st, r.Top + ColumnAppearance.Spacing, s, r.Bottom - R.Top -  ColumnAppearance.Spacing * 2);
  end;
end;

function TAdvSmoothCompletionColumn.GetRangeCount: integer;
begin
  result := StepsFromTo(GetRangeFrom, GetRangeTo);
end;

function TAdvSmoothCompletionColumn.GetRangeFrom: Double;
begin
  Result := 0;
  case RangeType of
    rtNumber: result := RangeFrom;
    rtDateTime:
    begin
      if OnlyDate then
        result := Int(FSaveDateRangeFrom)
      else
        result := FSaveDateRangeFrom;
    end;
    rtCustom: result := 0;
  end;
end;

function TAdvSmoothCompletionColumn.GetRangeTo: Double;
begin
  Result := 0;
  case RangeType of
    rtNumber: result := RangeTo;
    rtDateTime:
    begin
      if OnlyDate then
        result := int(FSaveDateRangeto)
      else
        result := FSaveDateRangeTo;
    end;
    rtCustom: Result := CustomItems.Count - 1;
  end;
end;

function TAdvSmoothCompletionColumn.GetSelectedCustomIndex: integer;
begin
  Result := Round(SelectedValue - GetRangeFrom);
end;

function TAdvSmoothCompletionColumn.GetSelectedDateTime: TDateTime;
begin
  result := SelectedValue;
end;

function TAdvSmoothCompletionColumn.IncSteps(StartValue: double; nr: integer): double;
var
  i: integer;
  d: integer;
begin
  case StepType of
    stMonth:
    begin
      if (nr > 0) then d := 1 else d := -1;
      for i := 1 to abs(nr) do
        StartValue := incmonth(StartValue, d);
      result := StartValue;
    end;
    stYear:
    begin
      if (nr > 0) then d := 1 else d := -1;
      for i := 1 to abs(nr) do
        startvalue := incyear(StartValue, d);
      result := StartValue;
    end;
    else result := StartValue + (nr * GetStep);
  end;
end;


function TAdvSmoothCompletionColumn.StepsFromTo(startvalue, endvalue: double): integer;
var
  steps: integer;
begin
  case StepType of
  stMonth:
  begin
    steps := 0;
    while (startvalue < endvalue) do
    begin
      startvalue := incmonth(startvalue);
      inc(steps);
    end;
  end;
  stYear:
  begin
    steps := 0;
    while (startvalue < endvalue) do
    begin
      startvalue := incyear(startvalue);
      inc(steps);
    end;
  end;
  else Steps := Round(Abs(endvalue - startvalue) / getstep);
  end;

  Result := Steps;
end;


function TAdvSmoothCompletionColumn.GetStep: Double;
begin
  Result := 1;
  case StepType of
    stNumber: Result := Step;
    stSecond: Result := Step / 86400;
    stMinute: Result := Step / 1440;
    stHour: Result := Step / 24;
    stDay: Result := Step;
  end;
end;

procedure TAdvSmoothCompletionColumn.Next(Animation: Boolean = true);
begin
  ScrollToValue(IncSteps(SelectedValue, 1), Animation);
end;

procedure TAdvSmoothCompletionColumn.Previous(Animation: Boolean = true);
begin
  ScrollToValue(IncSteps(SelectedValue, -1), Animation);
end;

procedure TAdvSmoothCompletionColumn.ScrollToValue(Value: double; Animation: Boolean = true; AnimationSpeed: integer = 4);
begin
  if (FSelectedValue <> value) and Enabled and Visible then
  begin
    Fanimate := Animation;
    FSp := AnimationSpeed;
    FSelectedValue := Value;
    if Value < GetRangeFrom then
      FScPosTo := -FOwner.ColumnAppearance.TextSpacing
    else
      FScPosTo := StepsFromTo(GetRangeFrom, FSelectedValue) * FOwner.ColumnAppearance.TextSpacing;

    if not Cyclic then
    begin
      FScPosTo := Max(0, Min(FOwner.ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
      FSelectedValue := Min(Max(Value, RangeFrom), RangeTo);
    end;

    if not Animation then
    begin
      if Cyclic then
      begin
        if FScPosTo = (GetRangeCount + 1) * FOwner.ColumnAppearance.TextSpacing then
          FScPosTo := 0
        else if FScPosTo = -FOwner.ColumnAppearance.TextSpacing then
          FScPosTo := GetRangeCount * FOwner.ColumnAppearance.TextSpacing;
      end;

      FScPosTo := Max(0, Min(Fowner.ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
      FSelectedValue := IncSteps(GetRangeFrom, FScPosTo div Fowner.ColumnAppearance.TextSpacing);
      FCurrentScPos := FScPosTo;

      if Assigned(FOwner.FOnSelectedValueChanged) then
        FOwner.FOnSelectedValueChanged(Self, Index, Round(FSelectedValue - GetRangeFrom), FSelectedValue, RangeType);
      Changed;
    end;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetAnimatedValue;
begin
  FSelectedValue := IncSteps(GetRangeFrom, FScPosTo div FOwner.ColumnAppearance.TextSpacing);
end;

procedure TAdvSmoothCompletionColumn.SetCustomItems(
  const Value: TAdvSmoothCompletionCustomItems);
begin
  if FCustomItems <> value then
  begin
    FCustomItems.Assign(Value);
    CustomItemsChanged(Self);
  end;
end;

procedure TAdvSmoothCompletionColumn.SetCyclic(const Value: Boolean);
begin
  if FCyclic <> value then
  begin
    FCyclic := value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetDateRangeFrom(const Value: TDateTime);
begin
  if FDateRangeFrom <> Value then
  begin
    FDateRangeFrom := Value;
    FSaveDateRangeFrom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetDateRangeTo(const Value: TDateTime);
begin
  if FDateRangeTo <> value then
  begin
    FDateRangeTo := Value;
    FSaveDateRangeto := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetDateTimeValueFormat(const Value: String);
begin
  if FDateTimeValueFormat <> value then
  begin
    FDateTimeValueFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothCompletionColumn.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetOnlyDate(const Value: Boolean);
begin
  if FOnlyDate <> value then
  begin
    FOnlyDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetRangeFrom(const Value: Double);
begin
  if FRangeFrom <> value then
  begin
    FRangeFrom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetRangeTo(const Value: Double);
begin
  if FRangeTo <> value then
  begin
    FRangeTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetRangeType(
  const Value: TAdvSmoothCompletionRangeType);
begin
  if FRangeType <> value then
  begin
    FRangeType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetSelectedCustomIndex(const Value: integer);
begin
  FSelectedValue := Value;
end;

procedure TAdvSmoothCompletionColumn.SetSelectedDateTime(const Value: TDateTime);
begin
  FSelectedValue := value;
end;

procedure TAdvSmoothCompletionColumn.SetSelectedValue(const Value: double);
begin
  if (FSelectedValue <> value) and (Value <= GetRangeTo) and (value >= GetRangeFrom) then
  begin
    FSelectedValue := Value;
    if FSelectedValue <> -1 then
    begin
      if Abs(GetRangeTo - GetRangeFrom) = 0 then
      begin
        FSelectedValue := -1;
      end
      else
      begin
        FCurrentScPos := StepsFromTo(GetRangeFrom, FSelectedValue) * FOwner.ColumnAppearance.TextSpacing;
        FScPosTo := FcurrentScPos;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetStep(const Value: Double);
begin
  if (FStep <> value) and (value > 0) then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetStepType(
  const Value: TAdvSmoothCompletionStepType);
begin
  if FStepType <> value then
  begin
    FStepType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetTextAlign(const Value: TAlignment);
begin
  if FTextAlign <> value then
  begin
    FTextAlign := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetValueFormat(const Value: String);
begin
  if FValueFormat <> value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetVisible(const Value: Boolean);
var
  di, i: integer;
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    di := 0;
    for I := 0 to Fowner.Columns.Count - 1 do
    begin
      with FOwner.Columns[I] do
      begin
        if Visible then
        begin
          FDrawIndex := di;
          Inc(di);
        end;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionColumn.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothCompletionColumns }

function TAdvSmoothCompletionColumns.Add: TAdvSmoothCompletionColumn;
begin
  Result := TAdvSmoothCompletionColumn(inherited Add);
end;

constructor TAdvSmoothCompletionColumns.Create(AOwner: TAdvSmoothCompletion);
begin
  inherited Create(TAdvSmoothCompletionColumn);
  FOwner := AOwner;
end;

procedure TAdvSmoothCompletionColumns.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothCompletionColumns.GetItem(
  Index: Integer): TAdvSmoothCompletionColumn;
begin
  Result := TAdvSmoothCompletionColumn(inherited Items[Index]);
end;

function TAdvSmoothCompletionColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothCompletionColumns.Insert(
  Index: Integer): TAdvSmoothCompletionColumn;
begin
  Result := TAdvSmoothCompletionColumn(inherited Insert(Index));
end;

procedure TAdvSmoothCompletionColumns.SetItem(Index: Integer;
  const Value: TAdvSmoothCompletionColumn);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothCompletionColumns.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

{ TAdvSmoothCompletionCustomItem }

procedure TAdvSmoothCompletionCustomItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothCompletionCustomItem) then
  begin
    FText := (Source as TAdvSmoothCompletionCustomItem).Text;
    FValue := (Source as TAdvSmoothCompletionCustomItem).Value;
    FImageIndex := (Source as TAdvSmoothCompletionCustomItem).ImageIndex;
    FpictureName := (source as TAdvSmoothCompletionCustomItem).PictureName;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionCustomItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothCompletionCustomItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothCompletionCustomItems).FOwner;
  FImageIndex := -1;
end;

destructor TAdvSmoothCompletionCustomItem.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothCompletionCustomItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionCustomItem.SetPictureName(const Value: string);
begin
  if FpictureName <> value then
  begin
    FpictureName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionCustomItem.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCompletionCustomItem.SetValue(const Value: Double);
begin
  if FValue <> value then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TAdvSmoothCompletionCustomItems }

function TAdvSmoothCompletionCustomItems.Add: TAdvSmoothCompletionCustomItem;
begin
  Result := TAdvSmoothCompletionCustomItem(inherited Add);
end;

constructor TAdvSmoothCompletionCustomItems.Create(AOwner: TAdvSmoothCompletion);
begin
  inherited Create(TAdvSmoothCompletionCustomItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothCompletionCustomItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothCompletionCustomItems.GetItem(
  Index: Integer): TAdvSmoothCompletionCustomItem;
begin
  Result := TAdvSmoothCompletionCustomItem(inherited Items[Index]);
end;

function TAdvSmoothCompletionCustomItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothCompletionCustomItems.Insert(
  Index: Integer): TAdvSmoothCompletionCustomItem;
begin
  Result := TAdvSmoothCompletionCustomItem(inherited Insert(Index));
end;

procedure TAdvSmoothCompletionCustomItems.SetItem(Index: Integer;
  const Value: TAdvSmoothCompletionCustomItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothCompletionCustomItems.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

{ TAdvSmoothTouchKeyBoardCompletion }

procedure TAdvSmoothTouchKeyBoardCompletion.ApplySettings;
var
  c: TColor;
begin
  c := Color;
  FCompletion.BottomLayerFill.Color := clWhite;
  FCompletion.BottomLayerFill.Opacity := 0;
  FCompletion.BottomLayerFill.GradientType := gtVertical;
  FCompletion.BottomLayerFill.ColorTo := clblack;
  FCompletion.BottomLayerFill.OpacityTo := 0;

  FCompletion.Fill.Color := clSilver;
  FCompletion.Fill.GradientType := gtSolid;
  FCompletion.Fill.Opacity := 200;
  FCompletion.Fill.BorderColor := clDkGray;
  FCompletion.Fill.Rounding := 5;

  FCompletion.ColumnAppearance.Fill.Color := Lighter(c, 50);
  FCompletion.ColumnAppearance.Fill.ColorTo := Lighter(c, 20);
  FCompletion.ColumnAppearance.Fill.BorderColor := clGray;
  FCompletion.ColumnAppearance.Fill.Rounding := 5;

  FCompletion.FSelectedfill.Opacity := 50;
  FCompletion.FSelectedfill.OpacityTo := 50;
  FCompletion.FSelectedFill.Color := clWhite;
  FCompletion.FSelectedFill.ColorTo := clWhite;
  FCompletion.FSelectedfill.GradientType := gtVertical;
  FCompletion.FSelectedfill.BorderColor := clGray;
  FCompletion.FSelectedFill.BorderOpacity := 100;

  FCompletion.ColumnAppearance.TextSpacing := LookupSpacing;

  Fcompletion.Columns[0].Font.Assign(Font);
end;

procedure TAdvSmoothTouchKeyBoardCompletion.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothTouchKeyBoardCompletion then
  begin
    FItems.Assign((Source as TAdvSmoothTouchKeyBoardCompletion).LookupList);
    FMode := (Source as TAdvSmoothTouchKeyBoardCompletion).Mode;
    FLookupCaseSensitive := (Source as TAdvSmoothTouchKeyBoardCompletion).LookupCaseSensitive;
    FLookupFromChars := (Source as TAdvSmoothTouchKeyBoardCompletion).LookupFromChars;
    FFont.Assign((Source as TAdvSmoothTouchKeyBoardCompletion).Font);
    FColor := (Source as TAdvSmoothTouchKeyBoardCompletion).Color;
    FLookupSpacing := (Source as TAdvSmoothTouchKeyBoardCompletion).LookupSpacing;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.Changed;
begin
  FOwner.Changed;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.CompletionChanged(Sender: TObject);
begin
  if Assigned(FOwner.FAutoCompletionForm) then
    FOwner.FAutoCompletionForm.UpdateLayered;
end;

constructor TAdvSmoothTouchKeyBoardCompletion.Create(
  AOwner: TAdvSmoothTouchKeyBoard);
begin
  FOwner := AOwner;
  FCompletion := TAdvSmoothCompletion.Create(FOwner);
  FCompletion.OnChange := CompletionChanged;
  FCompletion.ColumnAppearance.AllowHovering := false;
  FCompletion.ColumnAppearance.Spacing := 3;
  FCompletion.Columns.Add;
  FCompletion.Columns[0].RangeType := rtCustom;
  FCompletion.ColumnAppearance.TextSpacing := 30;
  FCompletion.Columns[0].TextAlign := taLeftJustify;
  FCompletion.TopLayerFill.Opacity := 0;
  FCompletion.TopLayerFill.OpacityTo := 0;
  FCompletion.Height := 100;
  FCompletion.Width := 300;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChanged;
  FMode := cmAutoVisible;

  FLookupCaseSensitive := false;
  FLookupFromChars := 3;
  FLookupSpacing := 30;

  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.Size := 14;
  FFont.Color := clWhite;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}


  ApplySettings;
end;

destructor TAdvSmoothTouchKeyBoardCompletion.Destroy;
begin
  FItems.Free;
  FCompletion.Free;
  FFont.Free;
  inherited;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.FontChanged(Sender: TObject);
begin
  ApplySettings;
  Changed;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.ItemsChanged(Sender: TObject);
begin
  FOwner.Changed;
  UpdateCompletion;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    ApplySettings;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    ApplySettings;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetItems(const Value: TStringList);
begin
  if FItems <> Value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetLookupCaseSensitive(
  const Value: Boolean);
begin
  if FLookupCaseSensitive <> value then
  begin
    FLookupCaseSensitive := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetLookupFromChars(
  const Value: integer);
begin
  if FLookupFromChars <> value then
  begin
    FLookupFromChars := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetLookupSpacing(
  const Value: Integer);
begin
  if FLookupSpacing <> Value then
  begin
    FLookupSpacing := Value;
    ApplySettings;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.SetMode(
  const Value: TAdvSmoothTouchKeyBoardCompletionMode);
begin
  if FMode <> value then
  begin
    FMode := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTouchKeyBoardCompletion.UpdateCompletion(Filter: string = '');
var
  I: Integer;
  str: String;
begin
  if Assigned(FCompletion) then
  begin
    FCompletion.BeginUpdate;
    FCompletion.Columns[0].CustomItems.Clear;
    for I := 0 to LookupList.Count - 1 do
    begin
      str := LookupList[i];
      if not LookupCaseSensitive then
        str := AnsiUpperCase(str);
      if ((Filter <> '') and (CompareStr(Copy(str, 0, Length(Filter)), Filter) = 0)) or (Filter = '') then
      begin
        with FCompletion.Columns[0].CustomItems.Add do
        begin
          Text := LookupList[I];
        end;
      end;
    end;
    FCompletion.EndUpdate;
  end;
end;

{ TAdvSmoothTouchKeyBoardAutoForm }

procedure TAdvSmoothTouchKeyBoardAutoForm.ClearBuffer(graphics: TGPGraphics);
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

procedure TAdvSmoothTouchKeyBoardAutoForm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FKeyBoard) then
    FKeyBoard.AutoCompletion.FCompletion.CMMouseLeave(Message);
end;

function TAdvSmoothTouchKeyBoardAutoForm.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothTouchKeyBoardAutoForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.CreateParams(
  var Params: TCreateParams);
begin
  inherited;

end;

procedure TAdvSmoothTouchKeyBoardAutoForm.CreateWnd;
begin
  inherited;
  if FKeyBoard.Owner is TForm then
    OldWndProc := TFarProc(GetWindowLong((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC));

  {$IFDEF DELPHI9_LVL}
  NewWndProc := Classes.MakeObjectInstance(HookWndProc);
  {$ELSE}
  NewWndProc := MakeObjectInstance(HookWndProc);
  {$ENDIF}

  if FKeyBoard.Owner is TForm then
    SetWindowLong((FKeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));

  invalidate;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.Draw(graphics: TGPGraphics);
var
  g: TGPGraphics;
  r: TRect;
  rborder, rout, rov, rin: TGPRectF;
  path: TGPGraphicsPath;
  rgn: TGPRegion;
  b: TGPBrush;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Assigned(FKeyBoard) then
  begin
    if Assigned(FKeyBoard.AutoCompletion.FCompletion) then
    begin
      r := Bounds(0, 0, Width - 1, Height -1);
      FKeyBoard.AutoCompletion.FCompletion.Draw(g, r);

      rout := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
      rov := rout;
      rov.X := rov.X + 7;
      rov.Y := rov.Y + 7;
      rov.Width := rov.Width - 14;
      rov.Height := rov.Height - 14;
      rov.Height := rov.Height / 4;
      path := CreateRoundRectangle(MakeRect(rov.X - 10, rov.Y - 1, rov.Width + 20, rov.Height + 2), 50, rtBottom);
      b := TGPLinearGradientBrush.Create(MakeRect(rov.X - 1, rov.Y - 1, rov.Width + 2, rov.Height + 2), MakeColor(150, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
      g.FillPath(b, path);
      b.Free;
      path.Free;
      rov := MakeRect(rout.X + 7, rout.Y + rout.Height - 14, rout.Width - 14, 8);
      b := TGPLinearGradientBrush.Create(MakeRect(rov.X - 1, rov.Y - 1, rov.Width + 2, rov.Height + 2), MakeColor(0, clBlack), MakeColor(150, clWhite), LinearGradientModeVertical);
      g.FillRectangle(b, rov);
      b.Free;
      rin := MakeRect(rout.X + 7, rout.Y + 7, rout.Width - 14, rout.Height - 14);
      rgn := TGPRegion.Create(rout);
      path := CreateRoundRectangle(rin, 10, rtBoth);
      rgn.Exclude(path);
      g.SetClip(rgn);
      b := TGPSolidBrush.Create(MakeColor(20, FKeyBoard.AutoCompletion.Color));
      g.FillRectangle(b, rout);
      g.ResetClip;
      b.Free;
      path.Free;
      rgn.Free;


      rborder := Makerect(rin.X - 1, rin.Y, 10, rin.Height);
      path := CreateRoundRectangle(rborder, 10, rtRight);
      b := TGPLinearGradientBrush.Create(Makerect(rborder.X - 1, rborder.Y - 1, rborder.Width + 2, rborder.Height + 2), MakeColor(30, clBlack), Makecolor(0, clBlack), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      path.Free;
      b.Free;


      rborder := Makerect(rin.X + rin.Width - 10, rin.Y, 10, rin.Height);
      path := CreateRoundRectangle(rborder, 10, rtLeft);
      b := TGPLinearGradientBrush.Create(Makerect(rborder.X - 1, rborder.Y - 1, rborder.Width + 2, rborder.Height + 2), MakeColor(0, clBlack), Makecolor(30, clBlack), LinearGradientModeHorizontal);
      g.FillPath(b, path);
      path.Free;
      b.Free;

    end;
  end;

  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.HookWndProc(var Msg: TMessage);
var
  pt: TPoint;
begin
  if Assigned(FKeyBoard) then
  begin
    if FKeyBoard.Owner is TForm then
      Msg.Result := CallWindowProc(OldWndProc, (FKeyBoard.Owner as TForm).Handle, Msg.Msg , Msg.wParam, Msg.lParam);

    case Msg.Msg of
     WM_ACTIVATE: PostMessage(Self.Handle, WM_USERACTIVATE, Msg.WParam, 0);
     WM_WINDOWPOSCHANGING, WM_SIZE:
     begin
       pt := FKeyBoard.ClientToScreen(Point(0, 0));
       Self.Left := pt.X + (keyboard.Width - Self.Width) div 2;
       Self.Top := pt.Y - Self.Height;
     end;
   end;
  end;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.Init;
begin
  DoubleBuffered := true;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  FormStyle := fsStayOnTop;
  Color := clWhite;

  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMdx := X;
  fMDy := Y;
  FMmx := X;
  FMmy := y;
  if Assigned(FKeyBoard) then
  begin
    if Assigned(FKeyBoard.AutoCompletion.FCompletion) then
      FKeyBoard.AutoCompletion.FCompletion.MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMmx := X;
  FMmy := Y;
  if Assigned(FKeyBoard) then
  begin
    if Assigned(FKeyBoard.AutoCompletion.FCompletion) then
      FKeyBoard.AutoCompletion.FCompletion.MouseMove(Shift, X, Y);
  end;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  custom, i, l: integer;
  str: String;
  msg: TMessage;
begin
  inherited;
  if Assigned(FKeyBoard) then
  begin
    if (Abs(Fmmx - FMdx) < 4) and (Abs(FMmy - FMdy) < 4) then
    begin
      custom := KeyBoard.AutoCompletion.FCompletion.XYToCustomItem(X, Y);
      if custom <> -1 then
      begin
        str := KeyBoard.AutoCompletion.FCompletion.Columns[0].CustomItems[custom].Text;
        l := Length(KeyBoard.AutoCompletion.FLookup);
        str := Copy(str, l + 1, Length(str) - l);
        for I := 1 to Length(str) do
          SendMessage(KeyBoard.KeybdInputHandle, WM_CHAR, Ord(str[I]),0);

        KeyBoard.AutoCompletion.FLookup := '';

        if Assigned(KeyBoard.FAutoCompletionForm) and (KeyBoard.AutoCompletion.Mode = cmAutoVisible) then
        begin
          KeyBoard.FAutoCompletionForm.Free;
          KeyBoard.FAutoCompletionForm := nil;
        end;

        KeyBoard.AutoCompletion.FCompletion.CMMouseLeave(Msg);
      end;
    end
    else
      if Assigned(FKeyBoard.AutoCompletion.FCompletion) then
        FKeyBoard.AutoCompletion.FCompletion.MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.SetPosition;
begin

end;

procedure TAdvSmoothTouchKeyBoardAutoForm.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow(255);
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.UpdateMainWindow(Alpha: Byte);
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
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
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.WMMouseActivate(
  var Msg: TWMMouseActivate);
begin
  msg.result := MA_NOACTIVATE;
end;

procedure TAdvSmoothTouchKeyBoardAutoForm.WndProc(var Message: TMessage);
begin
  if Assigned(FKeyBoard) then
  begin
    if Message.Msg = WM_DESTROY then
    begin
      if (fKeyboard.Owner is TForm) and (fKeyboard.Owner as TForm).HandleAllocated then
      begin
        if KeyBoard.Owner is TForm then
        {$IFDEF DELPHI_UNICODE}
          SetWindowLongPtr((KeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
          SetWindowLong((KeyBoard.Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc))
        {$ENDIF}
      end;
    end
    else if Message.Msg = WM_USERACTIVATE then
    begin
      case Message.WParam of
        0: ShowWindow(Handle, SW_HIDE);
        1, 2: ShowWindow(Handle, SW_SHOWNA);
      end;
    end;
  end;
  inherited;
end;

end.


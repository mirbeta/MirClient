unit DWinCtl;
interface
uses
  Windows, Classes, Graphics, SysUtils, Controls, StdCtrls, Messages, Forms,
  Grids, HUtil32, uCommon, WIL, PXL.Canvas, PXL.Types, PXL.Devices, uGameEngine,
  AsphyreTextureFonts, PXL.Textures, Clipbrd, Math, StrUtils;
const
  AllowedChars = [#32..#254];
  LineSpace = 2;
  LineSpace2 = 8;
  DECALW = 6;
  DECALH = 4;
type
  TDControl = class;

  TDBtnState = (tnor, tdown, tmove, tdisable);
  TClickSound = (csNone, csStone, csGlass, csNorm);
  TOnDirectPaint = procedure(Sender: TObject; DSurface: TCustomCanvas) of object;
  TOnKeyPress = procedure(Sender: TObject; var Key: Char) of object;
  TOnKeyDown = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;
  TOnMouseMove = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
  TOnMouseDown = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TOnMouseUp = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TOnClick = procedure(Sender: TObject) of object;
  TOnClickEx = procedure(Sender: TObject; X, Y: Integer) of object;
  TOnInRealArea = procedure(Sender: TObject; X, Y: Integer; var IsRealArea: Boolean) of object;
  TOnGridSelect = procedure(Sender: TObject; X, Y, ACol, ARow: Integer; Shift: TShiftState) of object;
  TOnGridPaint = procedure(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState; DSurface: TCustomCanvas) of object;
  TOnClickSound = procedure(Sender: TObject; Clicksound: TClickSound) of object;
  TOnTextChanged = procedure(Sender: TObject; sText: string) of object;
  TOnEnter = procedure(Sender: TObject) of object;
  TOnLeave = procedure(Sender: TObject) of object;
  TDControl = class(TCustomControl)
  protected
    bMouseMove: Boolean;
    FIsManager: Boolean;
    FPageActive: Boolean;
    FCaption: string;
    FDParent: TDControl;
    FEnableFocus: Boolean;
    FOnDirectPaint: TOnDirectPaint;
    FOnDirectPaintEnd: TOnDirectPaint;
    FOnKeyPress: TOnKeyPress;
    FOnKeyDown: TOnKeyDown;
    FOnMouseMove: TOnMouseMove;
    FOnMouseDown: TOnMouseDown;
    FOnMouseUp: TOnMouseUp;

    FOnMouseEnter: TOnClick;
    FOnMouseLeave: TOnClick;
    FOnRightMouseDown: TOnMouseDown;
    FIsMouseEnter: Boolean;
    FDisableTransparent: Boolean;


    FOnDblClick: TNotifyEvent;
    FOnClick: TOnClickEx;
    FOnEnter: TOnEnter;
    FOnLeave: TOnLeave;
    FOnInRealArea: TOnInRealArea;
    FOnBackgroundClick: TOnClick;
    FOnProcess: TNotifyEvent;
    procedure SetCaption(Str: string);
    function GetMouseMove: Boolean;
    function GetClientRect: TRect; override;
  protected
    FVisible: Boolean;
    procedure CaptionChaged; dynamic;
  public
    ReloadTex: Boolean;
    ImageSurface: TCustomLockableTexture;
    Background: Boolean;
    DControls: TList;
    WLib: TWMImages;
//    ULib: TUIBImages;
    FaceIndex: Integer; //默认
    MoveIndex: Integer; //经过序号
    DownIndex: Integer; //按下序号
    DisabledIndex: Integer; //禁用
    CheckedIndex: Integer;
    AniCount: integer;
    AniInterval: integer;
    HintText: string;
    FaceName: string;
    FRightClick: Boolean;
    WantReturn: Boolean;
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Process; dynamic;
    function SurfaceX(X: Integer): Integer;
    function SurfaceY(Y: Integer): Integer;
    function LocalX(X: Integer): Integer;
    function LocalY(Y: Integer): Integer;
    procedure AddChild(dcon: TDControl);
    procedure ChangeChildOrder(dcon: TDControl);
    function InRange(X, Y: Integer; Shift: TShiftState): Boolean; dynamic;
    function KeyPress(var Key: Char): Boolean; virtual;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; virtual;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function DblClick(X, Y: Integer): Boolean; virtual;
    function Click(X, Y: Integer): Boolean; virtual;
    function CanFocusMsg: Boolean;
    procedure Leave(); dynamic;
    procedure Enter(); dynamic;
    procedure AdjustPos(X, Y: Integer); overload;
    procedure AdjustPos(X, Y, W, H: Integer); overload;
    procedure SetImgIndex(Lib: TWMImages; Index: Integer); overload;
    procedure SetImgIndex(Lib: TWMImages; Index, X, Y: Integer); overload;
//    procedure SetImgName(Lib: TUIBImages; F: string);
    procedure DirectPaint(dsurface: TCustomCanvas); virtual;
    property PageActive: Boolean read FPageActive write FPageActive;
    property MouseMoveing: Boolean read GetMouseMove;
    property ClientRect: TRect read GetClientRect;
    procedure DragModePaint(dsurface: TCustomCanvas);
  published
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnDirectPaint: TOnDirectPaint read FOnDirectPaint write FOnDirectPaint;
    property OnDirectPaint2: TOnDirectPaint read FOnDirectPaintEnd write FOnDirectPaintEnd;
    property OnKeyPress: TOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnKeyDown: TOnKeyDown read FOnKeyDown write FOnKeyDown;
    property OnMouseMove: TOnMouseMove read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TOnMouseDown read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TOnMouseUp read FOnMouseUp write FOnMouseUp;
    //   放开MouseMove  Leave和enter事件
    property OnMouseEnter: TOnClick read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TOnClick read FOnMouseLeave write FOnMouseLeave;
    Property OnRightMouseDown: TOnMouseDown read FOnRightMouseDown write FOnRightMouseDown;
     property DisableTransparent: Boolean read FDisableTransparent write FDisableTransparent;


    property OnEnter: TOnEnter read FOnEnter write FOnEnter;
    property OnLeave: TOnLeave read FOnLeave write FOnLeave;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnInRealArea: TOnInRealArea read FOnInRealArea write FOnInRealArea;
    property OnBackgroundClick: TOnClick read FOnBackgroundClick write FOnBackgroundClick;
    property Caption: string read FCaption write SetCaption;
    property DParent: TDControl read FDParent write FDParent;
    property Visible: Boolean read FVisible write FVisible;
    property EnableFocus: Boolean read FEnableFocus write FEnableFocus;
    property Color;
    property Font;
    property Hint;
    property ShowHint;
    property Align;
  end;
  TDButton = class(TDControl)
  private
    FClickSound: TClickSound;
    FOnClick: TOnClickEx;
    FOnClickSound: TOnClickSound;
  public
    FFloating: Boolean;
    CaptionEx: string;
    FOnbtnState: TDBtnState;
    Downed: Boolean;
    Arrived: Boolean;
    SpotX, SpotY: Integer;
    Clicked: Boolean;
    ClickInv: LongWord;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  published
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
    property Floating: Boolean read FFloating write FFloating;
    property OnbtnState: TDBtnState read FOnbtnState write FOnbtnState;
  end;
  TDCheckBox = class(TDControl)
  private
    FArrived: Boolean;
    FChecked: Boolean;
    FClickSound: TClickSound;
    FOnClick: TOnClickEx;
    FOnClickSound: TOnClickSound;
  public
    Downed: Boolean;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    property Checked: Boolean read FChecked write FChecked;
    property Arrived: Boolean read FArrived write FArrived;
  published
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;
  TDCustomControl = class(TDControl)
  protected
    FEnabled: Boolean;
    FTransparent: Boolean;
    FClickSound: TClickSound;
    FOnClick: TOnClickEx;
    FOnClickSound: TOnClickSound;
    FFrameVisible: Boolean;
    FFrameHot: Boolean;
    FFrameSize: byte;
    FFrameColor: TColor;
    FFrameHotColor: TColor;
    procedure SetTransparent(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetFrameVisible(Value: Boolean);
    procedure SetFrameHot(Value: Boolean);
    procedure SetFrameSize(Value: byte);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameHotColor(Value: TColor);
  protected
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property FrameVisible: Boolean read FFrameVisible write SetFrameVisible default True;
    property FrameHot: Boolean read FFrameHot write SetFrameHot default False;
    property FrameSize: byte read FFrameSize write SetFrameSize default 1;
    property FrameColor: TColor read FFrameColor write SetFrameColor default $00406F77;
    property FrameHotColor: TColor read FFrameHotColor write SetFrameHotColor default $00599AA8;
  public
    Downed: Boolean;
    procedure OnDefaultEnterKey;
    procedure OnDefaultTabKey;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  published
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;


  TDHint = class(TDCustomControl)
  private
    FItems: TStrings;
    FBackColor: TColor;
    FSelectionColor: TColor;
    FParentControl: TDControl;
    function GetItemSelected: Integer;
    procedure SetItems(Value: TStrings);
    procedure SetBackColor(Value: TColor);
    procedure SetSelectionColor(Value: TColor);
    procedure SetItemSelected(Value: Integer);
  public
    FSelected: Integer;
    FOnChangeSelect: procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
    FOnMouseMoveSelect: procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
    property Items: TStrings read FItems write SetItems;
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clSilver;
    property ItemSelected: Integer read GetItemSelected write SetItemSelected;
    property ParentControl: TDControl read FParentControl write FParentControl;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
  end;

  TDGrid = class(TDControl)
  private
    FColCount, FRowCount: Integer;
    FColWidth, FRowHeight: Integer;
    FViewTopLine: Integer;
    SelectCell: TPoint;
    DownPos: TPoint;
    FOnGridSelect: TOnGridSelect;
    FOnGridMouseMove: TOnGridSelect;
    FOnGridPaint: TOnGridPaint;
    FOnButton: TMouseButton;
    function GetColRow(X, Y: Integer; var ACol, ARow: Integer): Boolean;
  public
    tButton: TMouseButton;
    cx, cy: Integer;
    Col, Row: Integer;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function Click(X, Y: Integer): Boolean; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
  published
    property ColCount: Integer read FColCount write FColCount;
    property RowCount: Integer read FRowCount write FRowCount;
    property ColWidth: Integer read FColWidth write FColWidth;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property ViewTopLine: Integer read FViewTopLine write FViewTopLine;
    property OnGridSelect: TOnGridSelect read FOnGridSelect write FOnGridSelect;
    property OnGridMouseMove: TOnGridSelect read FOnGridMouseMove write FOnGridMouseMove;
    property OnGridPaint: TOnGridPaint read FOnGridPaint write FOnGridPaint;
    property OnButton: TMouseButton read FOnButton write FOnButton;
  end;
  TDWindow = class(TDButton)
  private
    FFloating: Boolean;
  protected
    procedure SetVisible(flag: Boolean);
  public
    FMoveRange: Boolean;
    SpotX, SpotY: Integer;
    DialogResult: TModalResult;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure Show;
    function ShowModal: Integer;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Floating: Boolean read FFloating write FFloating;
  end;
  TDWinManager = class
  private
  public
    DWinList: TList;
    constructor Create();
    destructor Destroy; override;
    procedure AddDControl(dcon: TDControl; Visible: Boolean);
    procedure DelDControl(dcon: TDControl);
    procedure ClearAll;
    procedure Process;
    function KeyPress(var Key: Char): Boolean;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function DblClick(X, Y: Integer): Boolean;
    function Click(X, Y: Integer): Boolean;
    procedure DirectPaint(dsurface: TCustomCanvas);
  end;
  TDMoveButton = class(TDButton)
  private
    FFloating: Boolean;
    SpotX, SpotY: Integer;
  protected
    procedure SetVisible(flag: Boolean);
  public
    DialogResult: TModalResult;
    FOnClick: TOnClickEx;
    SlotLen: Integer;
    RLeft: Integer;
    RTop: Integer;
    Position: Integer;
    outHeight: Integer;
    Max: Integer;
    Reverse: Boolean;
    LeftToRight: Boolean;
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure Show;
    function ShowModal: Integer;
    procedure UpdatePos(pos: Integer; force: Boolean = False);
  published
    property Visible: Boolean read FVisible write SetVisible;
    property Floating: Boolean read FFloating write FFloating;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property FBoxMoveTop: Integer read SlotLen write SlotLen;
    property TypeRLeft: Integer read RLeft write RLeft;
    property TypeRTop: Integer read RTop write RTop;
    property TReverse: Boolean read Reverse write Reverse;
  end;
  TDComboBox = class;
  TDx9CustomListBox = class(TDCustomControl)
  private
    FItems: TStrings;
    FBackColor: TColor;
    FSelectionColor: TColor;
    FParentComboBox: TDComboBox;
    function GetItemSelected: Integer;
    procedure SetItems(Value: TStrings);
    procedure SetBackColor(Value: TColor);
    procedure SetSelectionColor(Value: TColor);
    procedure SetItemSelected(Value: Integer);
  public
    ChangingHero: Boolean;
    FSelected: Integer;
    FOnChangeSelect: procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
    FOnMouseMoveSelect: procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;
    property Items: TStrings read FItems write SetItems;
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clSilver;
    property ItemSelected: Integer read GetItemSelected write SetItemSelected;
    property ParentComboBox: TDComboBox read FParentComboBox write FParentComboBox;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
  end;
  TDListBox = class(TDx9CustomListBox)
  published
    property Enabled;
    property Transparent;
    property BackColor;
    property SelectionColor;
    property FrameVisible;
    property FrameHot;
    property FrameSize;
    property FrameColor;
    property FrameHotColor;
    property ParentComboBox;
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;

  TDxCustomEdit = class(TDCustomControl)
  private
    FAtom: Word;
    FHotKey: Cardinal;
    FIsHotKey: Boolean;
    FAlignment: TAlignment;
    FClick: Boolean;
    FSelClickStart: Boolean;
    FSelClickEnd: Boolean;
    FCurPos: Integer;
    FClickX: Integer;
    FSelStart: Integer;
    FSelEnd: Integer;
    FStartTextX: Integer;
    FSelTextStart: Integer;
    FSelTextEnd: Integer;
    FMaxLength: Integer;
    FShowCaretTick: LongWord;
    FShowCaret: Boolean;
    FNomberOnly: Boolean;
    FSecondChineseChar: Boolean;
    FPasswordChar: Char;
    FOnTextChanged: TOnTextChanged;
    FFontSelColor: TColor;
    procedure SetSelStart(Value: Integer);
    procedure SetSelEnd(Value: Integer);
    procedure SetMaxLength(Value: Integer);
    procedure SetPasswordChar(Value: Char);
    procedure SetNomberOnly(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetIsHotKey(Value: Boolean);
    procedure SetHotKey(Value: Cardinal);
    procedure SetAtom(Value: Word);
    procedure SetSelLength(Value: Integer);
    function ReadSelLength(): Integer;
    procedure SetFontSelColor(const Value: TColor);
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property NomberOnly: Boolean read FNomberOnly write SetNomberOnly default False;
    property IsHotKey: Boolean read FIsHotKey write SetIsHotKey default False;
    property Atom: Word read FAtom write SetAtom default 0;
    property HotKey: Cardinal read FHotKey write SetHotKey default 0;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
  public
    DxHint: TDHint;
    m_InputHint: string;
    FMiniCaret: byte;
    FCaretColor: TColor;
    procedure ShowCaret();
    procedure SetFocus(); override;
    procedure Enter(); override;
    procedure Leave(); override;
    procedure ChangeCurPos(nPos: Integer; boLast: Boolean = False);
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
    function KeyPress(var Key: Char): Boolean; override;
    function KeyPressEx(var Key: Char): Boolean;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function SetOfHotKey(HotKey: Cardinal): Word;
    property Text: string read FCaption write SetCaption;
    property SelStart: Integer read FSelStart write SetSelStart;
    property SelEnd: Integer read FSelEnd write SetSelEnd;
    property SelLength: Integer read ReadSelLength write SetSelLength;
    property OnTextChanged: TOnTextChanged read FOnTextChanged write FOnTextChanged;
    property FontSelColor: TColor read FFontSelColor write SetFontSelColor default clWhite;
  end;
  TDEdit = class(TDxCustomEdit)
  published
    property Alignment;
    property IsHotKey;
    property HotKey;
    property Enabled;
    property MaxLength;
    property NomberOnly;
    property Transparent;
    property PasswordChar;
    property FrameVisible;
    property FrameHot;
    property FrameSize;
    property FrameColor;
    property FrameHotColor;
    property OnEnter;
    property OnLeave;
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;
  TDComboBox = class(TDxCustomEdit)
  private
    FDropDownList: TDListBox;
  protected
  public
    constructor Create(aowner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
  published
    property Enabled;
    property MaxLength;
    property NomberOnly;
    property Transparent;
    property PasswordChar;
    property FrameVisible;
    property FrameHot;
    property FrameSize;
    property FrameColor;
    property FrameHotColor;
    property OnEnter;
    property OnLeave;
    property DropDownList: TDListBox read FDropDownList write FDropDownList;
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;
   TDMemo = class(TDControl)
  private
    FLines: TStrings;
    FOnChange: TOnClick;
    FReadOnly: Boolean;
    FFrameColor: TColor;
    FCaretShowTime: LongWord;
    FCaretShow: Boolean;
    FTopIndex: Integer;
    FCaretX: Integer;
    FCaretY: Integer;
    FSCaretX: Integer;
    FSCaretY: Integer;
    FMoveTick: LongWord;
    FInputStr: string;
    bDoubleByte: Boolean;
    KeyByteCount: Integer;
    FTransparent: Boolean;
    FMaxLength: integer;
    procedure DownCaret(X, Y: Integer);
    procedure MoveCaret(X, Y: Integer);
    procedure KeyCaret(Key: Word);
    procedure SetCaret(boBottom: Boolean);
    function ClearKey(): Boolean;
    function GetKey(): string;
    procedure SetCaretY(const Value: Integer);
  public
    Downed: Boolean;
    KeyDowned: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
    function KeyPress(var Key: Char): Boolean; override;
    function KeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure SetFocus();
    procedure TextChange();
    property Lines: TStrings read FLines;
    property ItemIndex: Integer read FCaretY write SetCaretY;
    procedure RefListWidth(ItemIndex: Integer; nCaret: Integer);
  published
    property OnChange: TOnClick read FOnChange write FOnChange;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property FrameColor: TColor read FFrameColor write FFrameColor;
    property boTransparent: Boolean read FTransparent write FTransparent;
    property MaxLength: Integer read FMaxLength write FMaxLength default 0;
  end;

  TDMemoStringList = class(TStringList)
    DMemo: TDMemo;
  private
    procedure Put(Index: Integer; const Value: string);
    function SelfGet(Index: Integer): string;
  published
  public
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure InsertObject(Index: Integer; const S: string;  AObject: TObject); override;
    function Get(Index: Integer): string; override;
    function GetText: PChar; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    property Str[Index: Integer]: string read SelfGet write Put; default;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  end;
  TDLabel = class(TDControl)
  private
    FText: string;                  //显示的字符串 不用标题显示
    FAutoSize: Boolean;             //自动更改边框大小
    FAlignment: TAlignment;         //显示位置
    FClickSound: TClickSound;
    FOnClick: TOnClickEx;
    FOnClickSound: TOnClickSound;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaptionChaged(Value: Boolean);
    procedure SetText(str: string);
  public
    Downed: Boolean;
    OldText: string;
    constructor Create(AOwner: TComponent); override;
    function MouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DirectPaint(dsurface: TCustomCanvas); override;
  published
    property AutoSize: Boolean read FAutoSize write SetCaptionChaged;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Text: string read FText write SetText;
    property ClickCount: TClickSound read FClickSound write FClickSound;
    property OnClick: TOnClickEx read FOnClick write FOnClick;
    property OnClickSound: TOnClickSound read FOnClickSound write FOnClickSound;
  end;

  TJobImageInfo = class//(TPersistent)
  private
     FEffectTick:Integer;
     FEffectIndex:Integer;
//***************************************************
     FFreezeCount:Integer;
     FFreezeTick:Integer;
     FFreezeIndex:Integer;
     FFreezesex:Integer;
//***************************************************
     FFreezingCount:Integer;
     FFreezingTick:Integer;
     FFreezingIndex:Integer;
     FFreezingsex:Integer;
//***************************************************
     FAniIndex:Integer;
     FAniTick:Integer;
//***************************************************
     fDarkLevelTick:Integer;
     fStateIndex1:Integer;
     fStateIndex2:Integer;
     fStatesex:Integer;
//***************************************************
    procedure SetAni(const Index, Value: Integer);
    procedure SetFreezeParam(const Index, Value: Integer);
    procedure SetFreezingParam(const Index, Value: Integer);
    procedure SetStateParam(const Index, Value: Integer);
  public
  published
  //**********************************************************************************************
    property AniIndex:Integer index 0 read FAniIndex write SetAni;
    property AniTick:Integer index 1 read FAniTick write SetAni;
    property EffectTick:Integer index 2 read FEffectTick write SetAni;
    property EffectIndex:Integer index 3 read FEffectIndex write SetAni;
//**********************************************************************************************
    property FreezeCount:Integer index 0 read FFreezeCount write SetFreezeParam;
    property FreezeTick : Integer index 1 read FFreezeTick write SetFreezeParam;
    property FreezeIndex :Integer index 2 read FFreezeIndex write SetFreezeParam;
    property Freezesex :Integer index 3 read FFreezesex write SetFreezeParam;
//**********************************************************************************************
    property FreezingCount:Integer index 0 read FFreezingCount write SetFreezingParam;
    property FreezingTick : Integer index 1 read FFreezingTick write SetFreezingParam;
    property FreezingIndex :Integer index 2 read FFreezingIndex write SetFreezingParam;
    property Freezingsex :Integer index 3 read FFreezingsex write SetFreezingParam;
//**********************************************************************************************
    property DarkLevelTick:Integer index 0 read FDarkLevelTick write SetStateParam;
    property StateIndex1 : Integer index 1 read FStateIndex1 write SetStateParam;
    property StateIndex2 :Integer index 2 read FStateIndex2 write SetStateParam;
    property Statesex :Integer index 3 read FStatesex write SetStateParam;
  end;
  TDXSelChr = class(TDControl)
  private
    FJobWarriorIndex : TJobImageInfo; //战士职业图片索引
    FJobWizardIndex : TJobImageInfo; //法师职业图片索引
    FJobTaoistIndex : TJobImageInfo; //道士职业图片索引
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property JobWarriorIndex : TJobImageInfo read FJobWarriorIndex write FJobWarriorIndex;
    property JobWizardIndex : TJobImageInfo read  FJobWizardIndex write FJobWizardIndex;
    property JobTaoistIndex:TJobImageInfo read  FJobTaoistIndex write FJobTaoistIndex;
  end;


procedure Register;
procedure SetDFocus(dcon: TDControl);
procedure ReleaseDFocus;
procedure SetDCapture(dcon: TDControl);
procedure ReleaseDCapture;
var
  g_DWinMan: TDWinManager;
  MouseCaptureControl: TDControl;
  FocusedControl: TDControl;
  MouseMoveControl: TDControl;
  MainWinHandle: Integer;
  ModalDWindow: TDControl;
  g_MainHWnd: HWnd;
  LastMenuControl: TDEdit = nil;
  g_TranFrame  : Boolean=False;
implementation

procedure Register;
begin
  RegisterComponents('HGEGUI', [TDButton, TDCheckBox,TDMemo,TDLabel,TDHint,
   TDGrid, TDWindow, TDMoveButton, TDEdit, TDComboBox,TDListBox]);
end;
procedure SetDFocus(dcon: TDControl);
begin

  if ( dcon.FEnableFocus)and(FocusedControl <> dcon) then begin
    if (FocusedControl <> nil) then
      FocusedControl.Leave;
    dcon.Enter;
  end;
  FocusedControl := dcon;
end;
procedure ReleaseDFocus;
begin
  if (FocusedControl <> nil) then begin
    FocusedControl.Leave;
  end;
  FocusedControl := nil;
end;
procedure SetDCapture(dcon: TDControl);
begin
    if dcon = nil then begin
    ReleaseDFocus;
  end else begin
    MouseCaptureControl := dcon;
  end;
end;
//释放鼠标捕获
procedure ReleaseDCapture;
begin
  ReleaseCapture;
  MouseCaptureControl := nil;
end;
constructor TDControl.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  DParent := nil;
  inherited Visible := False;
  FEnableFocus := False;
  Background := False;
  FIsManager := False;
  bMouseMove := False;
  FOnDirectPaint := nil;
  FOnDirectPaintEnd := nil;
  FOnKeyPress := nil;
  FOnKeyDown := nil;
  FOnMouseMove := nil;
  FOnMouseDown := nil;
  FOnMouseUp := nil;
  FOnMouseEnter := nil;
  FOnMouseLeave := nil;
  FOnRightMouseDown := nil;
  FOnEnter:= nil;
  FOnLeave:= nil;
  FOnInRealArea := nil;
  DControls := TList.Create;
  FDParent := nil;
  Width := 80;
  Height := 24;
  FCaption := '';
  FVisible := True;
  WLib := nil;
  ImageSurface := nil;
  FaceIndex := 0;
  FaceName := '';
  PageActive := False;
  FRightClick := False;
  ReloadTex := False;
    FDisableTransparent := False;
  FIsMouseEnter := False;
end;


destructor TDControl.Destroy;
begin
  if Self = MouseMoveControl then
    MouseMoveControl := nil;
  DControls.Free;
  inherited Destroy;
end;
procedure TDControl.SetCaption(Str: string);
begin
  FCaption := Str;
  if csDesigning in ComponentState then
  begin
    Refresh;
  end  else
  CaptionChaged;
end;
function TDControl.GetMouseMove: Boolean;
begin
  Result := MouseMoveControl = Self;
end;
function TDControl.GetClientRect: TRect;
begin
  Result.Left := SurfaceX(Left);
  Result.Top := SurfaceY(Top);
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + Height;
end;

procedure TDControl.AdjustPos(X, Y: Integer);
begin
  Top := Y;
  Left := X;
end;
procedure TDControl.AdjustPos(X, Y, W, H: Integer);
begin
  Left := X;
  Top := Y;
  Width := W;
  Height := H;
end;
procedure TDControl.Paint;
begin
  if csDesigning in ComponentState then
  begin
    if self is TDWindow then
    begin
      with Canvas do
      begin
        FillRect(ClipRect);
        Pen.Color := clBlack;
        MoveTo(0, 0);
        LineTo(Width - 1, 0);
        LineTo(Width - 1, Height - 1);
        LineTo(0, Height - 1);
        LineTo(0, 0);
        LineTo(Width - 1, Height - 1);
        MoveTo(Width - 1, 0);
        LineTo(0, Height - 1);
        TextOut((Width - TextWidth(Caption)) div 2, (Height - TextHeight(Caption)) div 2, Caption);
      end;
    end  else
    begin
      with Canvas do
      begin
        FillRect(ClipRect);
        Pen.Color := clBlack;
        MoveTo(0, 0);
        LineTo(Width - 1, 0);
        LineTo(Width - 1, Height - 1);
        LineTo(0, Height - 1);
        LineTo(0, 0);
        TextOut((Width - TextWidth(Caption)) div 2, (Height - TextHeight(Caption)) div 2, Caption);
      end;
    end;
  end;
end;
procedure TDControl.Leave();
begin
  if Assigned(FOnLeave) then
    FOnLeave(Self);
end;

procedure TDControl.Loaded;
var
  i: Integer;
  dcon: TDControl;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Parent <> nil then
    begin
      for i := 0 to TControl(Parent).ComponentCount - 1 do
      begin
        if TControl(Parent).Components[i] is TDControl then
        begin
          dcon := TDControl(TControl(Parent).Components[i]);
          if dcon.DParent = self then
          begin
            AddChild(dcon);
          end;
        end;
      end;
    end;
  end;
end;
procedure TDControl.Process;
var
  I: Integer;
begin
  if Assigned(FOnProcess) then
    FOnProcess(Self);
  for I := 0 to DControls.Count - 1 do
    if TDControl(DControls[I]).Visible then
      TDControl(DControls[I]).Process;
end;
function TDControl.SurfaceX(X: Integer): Integer;
var
  d: TDControl;
begin
  d := self;
  while True do
  begin
    if d.DParent = nil then
      Break;
    X := X + d.DParent.Left;
    d := d.DParent;
  end;
  Result := X;
end;
function TDControl.SurfaceY(Y: Integer): Integer;
var
  d: TDControl;
begin
  d := self;
  while True do
  begin
    if d.DParent = nil then
      Break;
    Y := Y + d.DParent.Top;
    d := d.DParent;
  end;
  Result := Y;
end;
function TDControl.LocalX(X: Integer): Integer;
var
  d: TDControl;
begin
  d := self;
  while True do
  begin
    if d.DParent = nil then
      Break;
    X := X - d.DParent.Left;
    d := d.DParent;
  end;
  Result := X;
end;
function TDControl.LocalY(Y: Integer): Integer;
var
  d: TDControl;
begin
  d := self;
  while True do
  begin
    if d.DParent = nil then
      Break;
    Y := Y - d.DParent.Top;
    d := d.DParent;
  end;
  Result := Y;
end;
procedure TDControl.AddChild(dcon: TDControl);
begin
  DControls.Add(Pointer(dcon));
end;
procedure TDControl.ChangeChildOrder(dcon: TDControl);
var
  i: Integer;
begin
  if not (dcon is TDWindow) then
    Exit;
  if TDWindow(dcon).Floating then
  begin
    for i := 0 to DControls.count - 1 do
    begin
      if dcon = DControls[i] then
      begin
        DControls.Delete(i);
        Break;
      end;
    end;
    DControls.Add(dcon);
  end;
end;

function TDControl.InRange(X, Y: Integer; Shift: TShiftState): Boolean;
var
  boInRange: Boolean;
  d: TCustomLockableTexture;
begin
  if (X >= Left) and (X < (Left + Width)) and (Y >= Top) and (Y < (Top + Height)) and (((ssRight in Shift) and FRightClick) or not (ssRight in Shift)) then
  begin
    boInRange := True;
    if Assigned(FOnInRealArea) then
      FOnInRealArea(self, X - Left, Y - Top, boInRange)
    else if not FDisableTransparent then
    begin
      if ImageSurface <> nil then
      begin
        if ImageSurface.Pixels(X - Left, Y - Top) <= 0 then
          boInRange := False;
      end
      else if WLib <> nil then
      begin
        d := WLib.Images[FaceIndex];
        if d <> nil then
        begin
          if d.Pixels(X - Left, Y - Top) <= 0 then
            boInRange := False;
        end;
//      end
//      else if ULib <> nil then
//      begin
//        d := ULib.Images[FaceName];
//        if d <> nil then
//        begin
//          if d.Pixels[X - Left, Y - Top] <= 0 then
//            boInRange := False;
//        end;
      end;
    end;
    Result := boInRange;
  end
  else
    Result := False;
end;
function TDControl.KeyPress(var Key: Char): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Background then
    Exit;
  for i := DControls.count - 1 downto 0 do
    if TDControl(DControls[i]).Visible then
      if TDControl(DControls[i]).KeyPress(Key) then
      begin
        Result := True;
        Exit;
      end;
  if (FocusedControl = self) then
  begin
    if Assigned(FOnKeyPress) then
      FOnKeyPress(self, Key);
    Result := True;
  end;
end;
function TDControl.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Background then
    Exit;
  for i := DControls.count - 1 downto 0 do
    if TDControl(DControls[i]).Visible then
      if TDControl(DControls[i]).KeyDown(Key, Shift) then
      begin
        Result := True;
        Exit;
      end;
  if (FocusedControl = self) then
  begin
    if Assigned(FOnKeyDown) then
      FOnKeyDown(self, Key, Shift);
    Result := True;
  end;
end;
function TDControl.CanFocusMsg: Boolean;
begin
  if (MouseCaptureControl = nil) or ((MouseCaptureControl <> nil) and ((MouseCaptureControl = self) or (MouseCaptureControl = DParent))) then
    Result := True
  else
    Result := False;
end;
procedure TDControl.CaptionChaged;
begin
end;
function TDControl.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  dc: TDControl;
begin
  Result := False;
  for i := DControls.count - 1 downto 0 do
  begin
    dc := TDControl(DControls[i]);
    if dc.Visible then
    begin
      if (ssRight in Shift) and not dc.FRightClick then
        Continue;
      if dc.MouseMove(Shift, X - Left, Y - Top) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  if (MouseCaptureControl <> nil) then
  begin
    if (MouseCaptureControl = self) then
    begin
      if (ssRight in Shift) and not FRightClick then
        Exit;
      if not bMouseMove and Assigned(FOnMouseMove) then
        FOnMouseMove(self, Shift, X, Y);
      Result := True;
    end;
    Exit;
  end;
  if Background then Exit;

  if InRange(X, Y, Shift) then
  begin
    if not bMouseMove then
    begin
      if not FIsMouseEnter then
      begin
        FIsMouseEnter := True;
        if Assigned(FOnMouseEnter) then
          FOnMouseEnter(Self);
      end;

      if Assigned(FOnMouseMove) then
        FOnMouseMove(self, Shift, X, Y);
    end;

    Result := True;
  end
  else if FIsMouseEnter then
  begin
    FIsMouseEnter := False;
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);
  end;
end;
function TDControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  dc: TDControl;
begin
  Result := False;
  if  Button  =  mbRight then
  begin
    if Assigned(FOnRightMouseDown) then
    begin
      FOnRightMouseDown(Self, Button , Shift, X, Y);
      Exit;
    end;
  end;

  for i := DControls.count - 1 downto 0 do
  begin
    dc := TDControl(DControls[i]);
    if dc.Visible then
    begin
      if dc.MouseDown(Button, Shift, X - Left, Y - Top) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  if Background then
  begin
    if Assigned(FOnBackgroundClick) then
    begin
      WantReturn := False;
      FOnBackgroundClick(self);
      if WantReturn then
        Result := True;
    end;
    ReleaseDFocus;
    Exit;
  end;
  if CanFocusMsg then
  begin
    if InRange(X, Y, Shift) or (MouseCaptureControl = self) then
    begin
      MouseMoveControl := nil;
      if (Button = mbRight) and not FRightClick then
        Exit;
      if Assigned(FOnMouseDown) then
        FOnMouseDown(self, Button, Shift, X, Y);
      if EnableFocus then
      begin
        if (self is TDHint) and (TDHint(self).ParentControl <> nil) then
        begin
          SetDFocus(TDHint(self).ParentControl);
        end
        else
          SetDFocus(self);
      end;
      Result := True;
    end;
  end;
end;
function TDControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  dc: TDControl;
begin
  Result := False;
  for i := DControls.count - 1 downto 0 do
  begin
    dc := TDControl(DControls[i]);
    if dc.Visible then
    begin
      if (dc is TDHint) then
        dc.Visible := False;
      if (Button = mbRight) and not dc.FRightClick then
        Continue;
      if dc.MouseUp(Button, Shift, X - Left, Y - Top) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  if (MouseCaptureControl <> nil) then
  begin
    if (MouseCaptureControl = self) then
    begin
      if (Button = mbRight) and not FRightClick then
        Exit;
      if Assigned(FOnMouseUp) then
        FOnMouseUp(self, Button, Shift, X, Y);
      Result := True;
    end;
    Exit;
  end;
  if Background then
    Exit;
  if InRange(X, Y, Shift) then
  begin
    if Assigned(FOnMouseUp) then
      FOnMouseUp(self, Button, Shift, X, Y);
    Result := True;
  end;
end;
function TDControl.DblClick(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (MouseCaptureControl <> nil) then
  begin
    if (MouseCaptureControl = self) then
    begin
      if Assigned(FOnDblClick) then
        FOnDblClick(self);
      Result := True;
    end;
    Exit;
  end;
  for i := DControls.count - 1 downto 0 do
    if TDControl(DControls[i]).Visible then
      if TDControl(DControls[i]).DblClick(X - Left, Y - Top) then
      begin
        Result := True;
        Exit;
      end;
  if Background then
    Exit;
  if InRange(X, Y, [ssDouble]) then
  begin
    if Assigned(FOnDblClick) then
      FOnDblClick(self);
    Result := True;
  end;
end;
function TDControl.Click(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (MouseCaptureControl <> nil) then
  begin
    if (MouseCaptureControl = self) then
    begin
      if Assigned(FOnClick) then
      begin
        FOnClick(self, X, Y);
      end;
      Result := True;
    end;
    Exit;
  end;
  for i := DControls.count - 1 downto 0 do
    if TDControl(DControls[i]).Visible then
      if TDControl(DControls[i]).Click(X - Left, Y - Top) then
      begin
        Result := True;
        Exit;
      end;
  if Background then
    Exit;
  if InRange(X, Y, [ssDouble]) then
  begin
    if Assigned(FOnClick) then
    begin
      FOnClick(self, X, Y);
    end;
    Result := True;
  end;
end;
procedure TDControl.SetImgIndex(Lib: TWMImages; Index: Integer);
var
  d: TCustomLockableTexture;
  pt: TPoint;
begin
  WLib := Lib;
  FaceIndex := Index;
  if Lib <> nil then
  begin
    d := Lib.Images[FaceIndex];
    if d <> nil then
    begin
      Width := d.Width;
      Height := d.Height;
    end
    else if not Background then
      ReloadTex := True;
  end;
end;



procedure TDControl.SetImgIndex(Lib: TWMImages; Index, X, Y: Integer);
var
  d: TCustomLockableTexture;
  pt: TPoint;
begin
  WLib := Lib;
  FaceIndex := Index;
  Self.Left := X;
  Self.Top := Y;
  if Lib <> nil then
  begin
    d := Lib.Images[FaceIndex];
    if d <> nil then
    begin
      Width := d.Width;
      Height := d.Height;
    end
    else if not Background then
      ReloadTex := True;
  end;
end;

//procedure TDControl.SetImgName(Lib: TUIBImages; F: string);
//var
//  d: TCustomLockableTexture;
//begin
//  try
//    ULib := Lib;
//    FaceName := F;
//    if Lib <> nil then
//    begin
//      d := Lib.Images[F];
//      if d <> nil then
//      begin
//        Width := d.Width;
//        Height := d.Height;
//      end
//      else if not Background then   //123456
//        ;                               //ReloadTex := True;
//    end;
//  except
//
//  end;
//end;

procedure TDControl.DirectPaint(dsurface: TCustomCanvas);
var
  i: Integer;
  d: TCustomLockableTexture;
begin
  if Assigned(FOnDirectPaint) then
  begin
    FOnDirectPaint(self, dsurface);
    if ReloadTex then
    begin
      if (WLib <> nil) and (FaceIndex > 0) then
      begin
        d := WLib.Images[FaceIndex];
        if d <> nil then
        begin
          ReloadTex := False;
          Width := d.Width;
          Height := d.Height;
        end;
      end;
    end;
  end
  else if WLib <> nil then
  begin
    d := WLib.Images[FaceIndex];
    if d <> nil then
      dsurface.Draw(SurfaceX(Left), SurfaceY(Top), d.ClientRect, d, True);
    if not Background and (WLib <> nil) and (FaceIndex > 0) then
    begin
      SetImgIndex(WLib, FaceIndex);
    end;
//  end
//  else if ULib <> nil then
//  begin
//    d := ULib.Images[FaceName];
//    if d <> nil then
//      dsurface.Draw(SurfaceX(Left), SurfaceY(Top), d.ClientRect, d, True);
//    if not Background and (ULib <> nil) and (FaceName <> '') then
//    begin
//      SetImgName(ULib, FaceName);
//    end;
  end;
  for i := 0 to DControls.count - 1 do
    if TDControl(DControls[i]).Visible then
      TDControl(DControls[i]).DirectPaint(dsurface);
  if Assigned(FOnDirectPaintEnd) then
    FOnDirectPaintEnd(self, dsurface);
    DragModePaint(dsurface);
end;
procedure TDControl.DragModePaint(dsurface: TCustomCanvas);
var
  dc, rc: TIntRect;
begin
  if g_TranFrame then  begin
    dc.Left := SurfaceX(Left);
    dc.Top := SurfaceY(Top);
    dc.Right := SurfaceX(left + Width);
    dc.Bottom := SurfaceY(top + Height);

//    if MouseEntry = msIn then
//      dsurface.FrameRect(Rect(dc.Left, dc.Top, dc.Right, dc.Bottom),(clBlue))
//    else
     dsurface.FrameRect(IntRectBDS(dc.Left, dc.Top, dc.Right, dc.Bottom),(clLime));
    if (FocusedControl = self) then begin
      dc.Left := SurfaceX(Left + 1);
      dc.Top := SurfaceY(Top + 1);
      dc.Right := SurfaceX(left + Width - 1);
      dc.Bottom := SurfaceY(top + Height - 1);
      dsurface.FrameRect(IntRectBDS(dc.Left, dc.Top, dc.Right, dc.Bottom),(clRed));
    end;
//    SetSelectedControl(Self);
  end;
end;
procedure TDControl.Enter();
begin
 if FocusedControl = nil then Exit;

  if Assigned(FOnEnter) then
    FOnEnter(Self)
end;
constructor TDButton.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  Downed := False;
  Arrived := False;
  FFloating := False;
  FOnClick := nil;
  CaptionEx := '';
  FEnableFocus := True;
  FClickSound := csNone;
  FOnbtnState := tnor;
  ClickInv := 0;
  Clicked := True;
end;
function TDButton.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  al, at: Integer;
begin
  Result := False;
  if FOnbtnState = tdisable then Exit;
  FOnbtnState := tnor;
  Result := inherited MouseMove(Shift, X, Y);
  Arrived := Result;
  if (not Background) and (not Result) then
  begin
    if MouseCaptureControl = self then
    if InRange(X, Y, Shift) then
    begin
      Downed := True;
    end else
    begin
      Downed := False;
    end;
  end;
  if Result and FFloating and (MouseCaptureControl = self) then
  begin
    if (SpotX <> X) or (SpotY <> Y) then
    begin
      al := Left + (X - SpotX);
      at := Top + (Y - SpotY);
      Left := al;
      Top := at;
      SpotX := X;
      SpotY := Y;
    end;
  end;
end;
function TDButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if FOnbtnState = tdisable then
    Exit;
  if inherited MouseDown(Button, Shift, X, Y) then
  begin
    if GetTickCount - ClickInv <= 150 then
    begin
      Result := True;
      Exit;
    end;
    if (not Background) and (MouseCaptureControl = nil) then
    begin
      Downed := True;
      SetDCapture(self);
    end;
    Result := True;
    if Result then
    begin
      if Floating then
      begin
        if DParent <> nil then
          DParent.ChangeChildOrder(self);
      end;
      SpotX := X;
      SpotY := Y;
    end;
  end;
end;
function TDButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if FOnbtnState = tdisable then
    Exit;
  if inherited MouseUp(Button, Shift, X, Y) then
  begin
    if not Downed then
    begin
      Result := True;
      ClickInv := 0;
      Exit;
    end;
    ReleaseDCapture;
    if not Background then
    begin
      if InRange(X, Y, Shift) then
      begin
        if GetTickCount - ClickInv <= 150 then
        begin
          Downed := False;
          Exit;
        end;
        ClickInv := GetTickCount;
        if Assigned(FOnClickSound) then
          FOnClickSound(self, FClickSound);
        if Assigned(FOnClick) then
          FOnClick(self, X, Y);
      end;
    end;
    Downed := False;
    Result := True;
    Exit;
  end
  else
  begin
    ReleaseDCapture;
    Downed := False;
  end;
end;
constructor TDCheckBox.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FArrived := False;
  Checked := False;
  Downed := False;
  FOnClick := nil;
  FEnableFocus := True;
  FClickSound := csNone;
end;
function TDCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  FArrived := Result;
  if (not Background) and (not Result) then
  begin
    if MouseCaptureControl = self then
      if InRange(X, Y, Shift) then
        Downed := True
      else
        Downed := False;
  end;
end;
function TDCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if inherited MouseDown(Button, Shift, X, Y) then
  begin
    if (not Background) and (MouseCaptureControl = nil) then
    begin
      Downed := True;
      SetDCapture(self);
    end;
    Result := True;
  end;
end;
function TDCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if inherited MouseUp(Button, Shift, X, Y) then
  begin
    ReleaseDCapture;
    if not Background then
    begin
      if InRange(X, Y, Shift) then
      begin
        Checked := not Checked;
        if Assigned(FOnClickSound) then
          FOnClickSound(self, FClickSound);
        if Assigned(FOnClick) then
          FOnClick(self, X, Y);
      end;
    end;
    Downed := False;
    Result := True;
    Exit;
  end
  else
  begin
    ReleaseDCapture;
    Downed := False;
  end;
end;
constructor TDCustomControl.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  Downed := False;
  FOnClick := nil;
  FEnableFocus := True;
  FClickSound := csNone;
  FTransparent := True;
  FEnabled := True;
  FFrameVisible := True;
  FFrameHot := False;
  FFrameSize := 1;
  FFrameColor := $00406F77;
  FFrameHotColor := $00599AA8;
end;
procedure TDCustomControl.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
    FTransparent := Value;
end;
procedure TDCustomControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
    FEnabled := Value;
end;
procedure TDCustomControl.SetFrameVisible(Value: Boolean);
begin
  if FFrameVisible <> Value then
    FFrameVisible := Value;
end;
procedure TDCustomControl.SetFrameHot(Value: Boolean);
begin
  if FFrameHot <> Value then
    FFrameHot := Value;
end;
procedure TDCustomControl.SetFrameSize(Value: byte);
begin
  if FFrameSize <> Value then
    FFrameSize := Value;
end;
procedure TDCustomControl.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
procedure TDCustomControl.SetFrameHotColor(Value: TColor);
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
procedure TDCustomControl.OnDefaultEnterKey;
begin
end;
procedure TDCustomControl.OnDefaultTabKey;
begin
end;
function TDCustomControl.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if FEnabled and not Background then
  begin
    if Result then
      SetFrameHot(True)
    else if FocusedControl <> self then
      SetFrameHot(False);
  end;
end;
function TDCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if inherited MouseDown(Button, Shift, X, Y) then
  begin
    if FEnabled then
    begin
      if (not Background) and (MouseCaptureControl = nil) then
      begin
        Downed := True;
        SetDCapture(self);
      end;
    end;
    Result := True;
  end;
end;
function TDCustomControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if inherited MouseUp(Button, Shift, X, Y) then
  begin
    ReleaseDCapture;
    if FEnabled and not Background then
    begin
      if InRange(X, Y, Shift) then
      begin
        if Assigned(FOnClickSound) then
          FOnClickSound(self, FClickSound);
        if Assigned(FOnClick) then
          FOnClick(self, X, Y);
      end;
    end;
    Downed := False;
    Result := True;
    Exit;
  end
  else
  begin
    ReleaseDCapture;
    Downed := False;
  end;
end;

constructor TDHint.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FSelected := -1;
  FItems := TStringList.Create;
  FBackColor := clWhite;
  FSelectionColor := clSilver;
  FOnChangeSelect := nil;
  FOnMouseMoveSelect := nil;
  FParentControl := nil;
end;
destructor TDHint.Destroy;
begin
  FItems.Free;
  inherited;
end;
function TDHint.GetItemSelected: Integer;
begin
  if (FSelected > FItems.count - 1) or (FSelected < 0) then
    Result := -1
  else
    Result := FSelected;
end;
procedure TDHint.SetItemSelected(Value: Integer);
begin
  if (Value > FItems.count - 1) or (Value < 0) then
    FSelected := -1
  else
    FSelected := Value;
end;
procedure TDHint.SetBackColor(Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
procedure TDHint.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
function TDHint.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
end;
function TDHint.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  TmpSel: Integer;
begin
  FSelected := -1;
  Result := inherited MouseMove(Shift, X, Y);
  if Result and FEnabled and not Background then
  begin
    TmpSel := FSelected;
    if (FItems.count = 0) then
      FSelected := -1
    else
      FSelected := (-Top + Y - LineSpace2 + 2) div (-Font.Height + LineSpace2);
    if FSelected > FItems.count - 1 then
      FSelected := -1;
    if Assigned(FOnMouseMoveSelect) then
      FOnMouseMoveSelect(self, Shift, X, Y);
  end;
end;
function TDHint.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  c: Char;
  ret: Boolean;
  TmpSel: Integer;
begin
  ret := inherited MouseUp(Button, Shift, X, Y);
  if ret then
  begin
    TmpSel := FSelected;
    if (FItems.count = 0) then
      FSelected := -1
    else
      FSelected := (-Top + Y - LineSpace2 + 2) div (-Font.Height + LineSpace2);
    if FSelected > FItems.count - 1 then
      FSelected := -1;
    if (FSelected > -1) and (FSelected < FItems.count) and (FParentControl <> nil) and (FParentControl is TDxCustomEdit) then
    begin
      if (FItems.Objects[FSelected] <> nil) then
      begin
        Result := True;
        Exit;
      end;
      if tag = 0 then
      begin
        c := #0000;
        case FSelected of
          0:c := #0024;
          1:c := #0003;
          2:c := #0022;
          3:c := #0008;
          4:begin
              TDxCustomEdit(FParentControl).SelStart := 0;
              TDxCustomEdit(FParentControl).SelEnd := AnsiTextLength(TDxCustomEdit(FParentControl).Caption);
              TDxCustomEdit(FParentControl).ChangeCurPos(TDxCustomEdit(FParentControl).SelEnd, True);
            end;
        end;
        if (c <> #0000) then
        begin
          TDxCustomEdit(FParentControl).KeyPressEx(c);
        end;
      end
      else if tag = 1 then
      begin
      end;
    end;
    if Assigned(FOnChangeSelect) then
      FOnChangeSelect(self, Button, Shift, X, Y);
    Visible := False;
    ret := True;
  end;
  Result := ret;
end;

function TDHint.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  ret: Boolean;
begin
  ret := inherited KeyDown(Key, Shift);
  if ret then
  begin
    case Key of
      VK_PRIOR:
        begin
          ItemSelected := ItemSelected - Height div  - Font.Height;
          if (ItemSelected = -1) then
            ItemSelected := 0;
        end;
      VK_NEXT:
        begin
          ItemSelected := ItemSelected + Height div  - Font.Height;
          if ItemSelected = -1 then
            ItemSelected := FItems.count - 1;
        end;
      VK_UP:
        if ItemSelected - 1 > -1 then
          ItemSelected := ItemSelected - 1;
      VK_DOWN:
        if ItemSelected + 1 < FItems.count then
          ItemSelected := ItemSelected + 1;
    end;
  end;
  Result := ret;
end;
procedure TDHint.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;
procedure TDHint.DirectPaint(dsurface: TCustomCanvas);
var
  fy, nY, L, T, i, oSize: Integer;
  OldColor: TColor;
begin
  if Assigned(FOnDirectPaint) then
  begin
    FOnDirectPaint(self, dsurface);
    Exit;
  end;
  L := SurfaceX(Left);
  T := SurfaceY(Top);
  try
    dsurface.FillRect(IntRectBDS(L, T + 1, Width, Height - 1), BackColor);
    if (FSelected > -1) and (FSelected < FItems.count) then
    begin
      if (FItems.Objects[FSelected] = nil) then
      begin
        nY := T + (-Font.Height + LineSpace2) * FSelected;
        fy := nY + (-Font.Height + LineSpace2);
        if (nY < T + Self.Height - 1) and (fy > T + 1) then
        begin
          if (fy > T + Height - 1) then
            fy := T + Height - 1;
          if (nY < T + 1) then
            nY := T + 1;
          dsurface.FillRect(IntRectBDS(L + 2, nY + 2, Width - 4, (-Font.Height + LineSpace2) + 5), clBlue);
        end;
      end;
    end;
    for i := 0 to FItems.count - 1 do
    begin
      if (FSelected = i) and (FItems.Objects[i] = nil) then
      begin
        Font.Color := clWhite
      end
      else if (FItems.Objects[i] <> nil) then
        Font.Color := clSilver
      else
      begin
        Font.Color := clBlack;
      end;
      dsurface.TextOut(L + LineSpace2, LineSpace2 + T + (-Font.Height + LineSpace2) * i, FItems.Strings[i], Font.Color);
    end;
  finally
  end;
  Exit;
  try
    if (FSelected > -1) and (FSelected < FItems.count) then
    begin
      if (FItems.Objects[FSelected] = nil) then
      begin
        nY := T + (-Font.Height + LineSpace2) * FSelected;
        fy := nY + (-Font.Height + LineSpace2);
        if (nY < T + Height - 1) and (fy > T + 1) then
        begin
          if (fy > T + Height - 1) then
            fy := T + Height - 1;
          if (nY < T + 1) then
            nY := T + 1;
          dsurface.FillRect(IntRectBDS(L, nY + 2, L + Width, fy + 5),clHighlight);
        end;
      end;
    end;
    for i := 0 to FItems.count - 1 do
    begin
      if (FSelected = i) and (FItems.Objects[i] = nil) then
      begin
        Font.Color := clWhite
      end
      else if (FItems.Objects[i] <> nil) then
        Font.Color := clSilver
      else
      begin
        Font.Color := clWhite;
      end;
      dsurface.TextOut(L + LineSpace2, LineSpace2 + T + (-Font.Height + LineSpace2) * i, FItems.Strings[i], Font.Color);
    end;
  finally
  end;
  DragModePaint(dsurface);
end;
constructor TDGrid.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FColCount := 8;
  FRowCount := 5;
  FColWidth := 36;
  FRowHeight := 32;
  FOnGridSelect := nil;
  FOnGridMouseMove := nil;
  FOnGridPaint := nil;
  tButton := mbLeft;
end;
function TDGrid.GetColRow(X, Y: Integer; var ACol, ARow: Integer): Boolean;
begin
  Result := False;
  if InRange(X, Y, [ssDouble]) then
  begin
    ACol := (X - Left) div FColWidth;
    ARow := (Y - Top) div FRowHeight;
    Result := True;
  end;
end;
function TDGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  ACol, ARow: Integer;
begin
  Result := False;
  if Button in [mbLeft, mbRight] then
  begin
    if GetColRow(X, Y, ACol, ARow) then
    begin
      SelectCell.X := ACol;
      SelectCell.Y := ARow;
      DownPos.X := X;
      DownPos.Y := Y;
      SetDCapture(self);
      Result := True;
    end;
  end;
end;
function TDGrid.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  ACol, ARow: Integer;
begin
  Result := False;
  if InRange(X, Y, Shift) then
  begin
    if GetColRow(X, Y, ACol, ARow) then
    begin
      if Assigned(FOnGridMouseMove) then
        FOnGridMouseMove(self, X, Y, ACol, ARow, Shift);
    end;
    Result := True;
  end;
end;
function TDGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  ACol, ARow: Integer;
begin
  Result := False;
  if Button in [mbLeft, mbRight] then
  begin
    if GetColRow(X, Y, ACol, ARow) then
    begin
      if (SelectCell.X = ACol) and (SelectCell.Y = ARow) then
      begin
        Col := ACol;
        Row := ARow;
        if Assigned(FOnGridSelect) then
        begin
          self.OnButton := Button;
          FOnGridSelect(self, X, Y, ACol, ARow, Shift);
        end;
      end;
      Result := True;
    end;
    ReleaseDCapture;
  end;
end;
function TDGrid.Click(X, Y: Integer): Boolean;
var
  ACol, ARow: Integer;
begin
  Result := False;
end;
procedure TDGrid.DirectPaint(dsurface: TCustomCanvas);
var
  i, j: Integer;
  rc: TRect;
begin
  if Assigned(FOnGridPaint) then
    for i := 0 to FRowCount - 1 do
      for j := 0 to FColCount - 1 do
      begin
        rc := Rect(Left + j * FColWidth, Top + i * FRowHeight, Left + j * (FColWidth + 1) - 1, Top + i * (FRowHeight + 1) - 1);
        if (SelectCell.Y = i) and (SelectCell.X = j) then
          FOnGridPaint(self, j, i, rc, [gdSelected], dsurface)
        else
          FOnGridPaint(self, j, i, rc, [], dsurface);
      end;
      DragModePaint(dsurface);
end;
constructor TDWindow.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FFloating := False;
  FMoveRange := False;
  FEnableFocus := True;
  Width := 120;
  Height := 120;
end;
procedure TDWindow.SetVisible(flag: Boolean);
begin
  FVisible := flag;
  if Floating then
  begin
    if DParent <> nil then
      DParent.ChangeChildOrder(self);
  end;
end;
function TDWindow.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  al, at: Integer;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if Result and FFloating and (MouseCaptureControl = self) then
  begin
    if (SpotX <> X) or (SpotY <> Y) then
    begin
      al := Left + (X - SpotX);
      at := Top + (Y - SpotY);
      if FMoveRange then
      begin
      end;
      Left := al;
      Top := at;
      SpotX := X;
      SpotY := Y;
    end;
  end;
end;
function TDWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  if Result then
  begin
    if Floating then
    begin
      if DParent <> nil then
        DParent.ChangeChildOrder(self);
    end;
    SpotX := X;
    SpotY := Y;
  end;
end;
function TDWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseUp(Button, Shift, X, Y);
end;
procedure TDWindow.Show;
begin
  Visible := True;
  if Floating then
  begin
    if DParent <> nil then
      DParent.ChangeChildOrder(self);
  end;
  if EnableFocus then
    SetDFocus(self);
end;
function TDWindow.ShowModal: Integer;
begin
  Result := 0;
  Visible := True;
  ModalDWindow := self;
  if EnableFocus then
    SetDFocus(self);
end;
constructor TDWinManager.Create();
begin
  DWinList := TList.Create;
  MouseCaptureControl := nil;
  FocusedControl := nil;
end;
destructor TDWinManager.Destroy;
begin
  DWinList.Free;
  inherited Destroy;
end;
procedure TDWinManager.ClearAll;
begin
  DWinList.Clear;
end;
procedure TDWinManager.Process;
var
  I: Integer;
begin
  for I := 0 to DWinList.Count - 1 do
  begin
    if TDControl(DWinList[I]).Visible then
    begin
      TDControl(DWinList[I]).Process;
    end;
  end;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
      with ModalDWindow do
        Process;
  end;
end;
procedure TDWinManager.AddDControl(dcon: TDControl; Visible: Boolean);
begin
  dcon.Visible := Visible;
  DWinList.Add(dcon);
end;
procedure TDWinManager.DelDControl(dcon: TDControl);
var
  i: Integer;
begin
  for i := 0 to DWinList.count - 1 do
    if DWinList[i] = dcon then
    begin
      DWinList.Delete(i);
      Break;
    end;
end;
function TDWinManager.KeyPress(var Key: Char): Boolean;
var
  i: Integer;
begin
  Result := False;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        Result := KeyPress(Key);
      Exit;
    end
    else
      ModalDWindow := nil;
    Key := #0;
  end;
  if FocusedControl <> nil then
  begin
    if FocusedControl.Visible then
    begin
      Result := FocusedControl.KeyPress(Key);
    end
    else
      ReleaseDFocus;
  end;
end;
function TDWinManager.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  i: Integer;
begin
  Result := False;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        Result := KeyDown(Key, Shift);
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if FocusedControl <> nil then
  begin
    if FocusedControl.Visible then
      Result := FocusedControl.KeyDown(Key, Shift)
    else
      ReleaseDFocus;
  end;
end;
function TDWinManager.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        MouseMove(Shift, LocalX(X), LocalY(Y));
      Result := True;
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if MouseCaptureControl <> nil then
  begin
    with MouseCaptureControl do
      Result := MouseMove(Shift, LocalX(X), LocalY(Y));
  end else
  for i := 0 to DWinList.count - 1 do
  begin
    if TDControl(DWinList[i]).Visible then
    begin
      if TDControl(DWinList[i]).MouseMove(Shift, X, Y) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
function TDWinManager.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        MouseDown(Button, Shift, LocalX(X), LocalY(Y));
      Result := True;
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if MouseCaptureControl <> nil then
  begin
    with MouseCaptureControl do
      Result := MouseDown(Button, Shift, LocalX(X), LocalY(Y));
  end else
  for i := 0 to DWinList.count - 1 do
  begin
    if TDControl(DWinList[i]).Visible then
    begin
      if TDControl(DWinList[i]).MouseDown(Button, Shift, X, Y) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
function TDWinManager.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        Result := MouseUp(Button, Shift, LocalX(X), LocalY(Y));
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if MouseCaptureControl <> nil then
  begin
    with MouseCaptureControl do
      Result := MouseUp(Button, Shift, LocalX(X), LocalY(Y));
  end else
  for i := 0 to DWinList.count - 1 do
  begin
    if TDControl(DWinList[i]).Visible then
    begin
      if TDControl(DWinList[i]).MouseUp(Button, Shift, X, Y) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
function TDWinManager.DblClick(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        Result := DblClick(LocalX(X), LocalY(Y));
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if MouseCaptureControl <> nil then
  begin
    with MouseCaptureControl do
      Result := DblClick(LocalX(X), LocalY(Y));
  end  else
  begin
    for i := 0 to DWinList.count - 1 do
    begin
      if TDControl(DWinList[i]).Visible then
      begin
        if TDControl(DWinList[i]).DblClick(X, Y) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;
function TDWinManager.Click(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  if ModalDWindow <> nil then
  begin
    if ModalDWindow.Visible then
    begin
      with ModalDWindow do
        Result := Click(LocalX(X), LocalY(Y));
      Exit;
    end
    else
      ModalDWindow := nil;
  end;
  if MouseCaptureControl <> nil then
  begin
    with MouseCaptureControl do
      Result := Click(LocalX(X), LocalY(Y));
  end  else
  for i := 0 to DWinList.count - 1 do
  begin
    if TDControl(DWinList[i]).Visible then
    begin
      if TDControl(DWinList[i]).Click(X, Y) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
procedure TDWinManager.DirectPaint(dsurface: TCustomCanvas);
var
  i: Integer;
begin
  for i := 0 to DWinList.count - 1 do
  begin
    if TDControl(DWinList[i]).Visible then
    begin
      try
        TDControl(DWinList[i]).DirectPaint(dsurface);
      except
      end;
    end;
  end;
  try
    if ModalDWindow <> nil then
    begin
      if ModalDWindow.Visible then
        with ModalDWindow do
          DirectPaint(dsurface);
    end;
  except
  end;
end;
constructor TDMoveButton.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FFloating := True;
  FEnableFocus := False;
  Width := 30;
  Height := 30;
  LeftToRight := True;
  bMouseMove := True;
end;
procedure TDMoveButton.SetVisible(flag: Boolean);
begin
  FVisible := flag;
  if Floating then
  begin
    if DParent <> nil then
      DParent.ChangeChildOrder(self);
  end;
end;
function TDMoveButton.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  n, al, at, ot: Integer;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if Max <= 0 then
    Exit;
  if ssLeft in Shift then
  begin
    if Result and FFloating and (MouseCaptureControl = self) then
    begin
      n := Position;
      try
        if Max <= 0 then
          Exit;
        if (SpotX <> X) or (SpotY <> Y) then
        begin
          if LeftToRight then
          begin
            if not Reverse then
            begin
              ot := SlotLen - Width;
              al := RTop;
              at := Left + (X - SpotX);
              if at < RLeft then
                at := RLeft;
              if at + Width > RLeft + SlotLen then
                at := RLeft + SlotLen - Width;
              Position := Round((at - RLeft) / (ot / Max));
              if Position < 0 then
                Position := 0;
              if Position > Max then
                Position := Max;
              Left := at;
              Top := al;
              SpotX := X;
              SpotY := Y;
            end
            else
            begin
              al := RTop;
              at := Left + (X - SpotX);
              if at < RLeft - SlotLen then
                at := RLeft - SlotLen;
              if at > RLeft then
                at := RLeft;
              Position := Round((at - RLeft) / (SlotLen / Max));
              if Position < 0 then
                Position := 0;
              if Position > Max then
                Position := Max;
              Left := at;
              Top := al;
              SpotX := X;
              SpotY := Y;
            end;
          end
          else
          begin
            if not Reverse then
            begin
              ot := SlotLen - Height;
              al := RLeft;
              at := Top + (Y - SpotY);
              if at < RTop then
                at := RTop;
              if at + Height > RTop + SlotLen then
                at := RTop + SlotLen - Height;
              Position := Round((at - RTop) / (ot / Max));
              if Position < 0 then
                Position := 0;
              if Position > Max then
                Position := Max;
              Left := al;
              Top := at;
              SpotX := X;
              SpotY := Y;
            end
            else
            begin
              al := RLeft;
              at := Top + (Y - SpotY);
              if at < RTop - SlotLen then
                at := RTop - SlotLen;
              if at > RTop then
                at := RTop;
              Position := Round((at - RTop) / (SlotLen / Max));
              if Position < 0 then
                Position := 0;
              if Position > Max then
                Position := Max;
              Left := al;
              Top := at;
              SpotX := X;
              SpotY := Y;
            end;
          end;
        end;
      finally
        if (n <> Position) and Assigned(FOnMouseMove) then
          FOnMouseMove(self, Shift, X, Y);
      end;
    end;
  end;
end;
procedure TDMoveButton.UpdatePos(pos: Integer; force: Boolean);
begin
  if Max <= 0 then
    Exit;
  Position := pos;
  if Position < 0 then
    Position := 0;
  if Position > Max then
    Position := Max;
  if LeftToRight then
  begin
    Left := RLeft + Round((SlotLen - Width) / Max * Position);
    if Left < RLeft then
      Left := RLeft;
    if Left > RLeft + SlotLen - Width then
      Left := RLeft + SlotLen - Width;
  end
  else
  begin
    Top := RTop + Round((SlotLen - Height) / Max * Position);
    if Top < RTop then
      Top := RTop;
    if Top > RTop + SlotLen - Height then
      Top := RTop + SlotLen - Height;
  end;
end;
function TDMoveButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseDown(Button, Shift, X, Y);
  if Result then
  begin
    if Floating then
    begin
      if DParent <> nil then
        DParent.ChangeChildOrder(self);
    end;
    SpotX := X;
    SpotY := Y;
  end;
end;
function TDMoveButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseUp(Button, Shift, X, Y);
end;
procedure TDMoveButton.Show;
begin
  Visible := True;
  if Floating then
  begin
    if DParent <> nil then
      DParent.ChangeChildOrder(self);
  end;
  if EnableFocus then
    SetDFocus(self);
end;
function TDMoveButton.ShowModal: Integer;
begin
  Result := 0;
  Visible := True;
  ModalDWindow := self;
  if EnableFocus then
    SetDFocus(self);
end;

function IsKeyPressed(Key: Byte): Boolean;
var
  keyvalue          : TKeyBoardState;
begin
  Result := False;
  FillChar(keyvalue, SizeOf(TKeyBoardState), #0);
  if GetKeyboardState(keyvalue) then
    if (keyvalue[Key] and $80) <> 0 then
      Result := True;
end;

const
  // Windows 2000/XP multimedia keys (adapted from winuser.h and renamed to avoid potential conflicts)
  // See also: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
  _VK_BROWSER_BACK = $A6; // Browser Back key
  _VK_BROWSER_FORWARD = $A7; // Browser Forward key
  _VK_BROWSER_REFRESH = $A8; // Browser Refresh key
  _VK_BROWSER_STOP = $A9; // Browser Stop key
  _VK_BROWSER_SEARCH = $AA; // Browser Search key
  _VK_BROWSER_FAVORITES = $AB; // Browser Favorites key
  _VK_BROWSER_HOME = $AC; // Browser Start and Home key
  _VK_VOLUME_MUTE = $AD; // Volume Mute key
  _VK_VOLUME_DOWN = $AE; // Volume Down key
  _VK_VOLUME_UP = $AF; // Volume Up key
  _VK_MEDIA_NEXT_TRACK = $B0; // Next Track key
  _VK_MEDIA_PREV_TRACK = $B1; // Previous Track key
  _VK_MEDIA_STOP = $B2; // Stop Media key
  _VK_MEDIA_PLAY_PAUSE = $B3; // Play/Pause Media key
  _VK_LAUNCH_MAIL = $B4; // Start Mail key
  _VK_LAUNCH_MEDIA_SELECT = $B5; // Select Media key
  _VK_LAUNCH_APP1 = $B6; // Start Application 1 key
  _VK_LAUNCH_APP2 = $B7; // Start Application 2 key
  // Self-invented names for the extended keys
  NAME_VK_BROWSER_BACK = 'Browser Back';
  NAME_VK_BROWSER_FORWARD = 'Browser Forward';
  NAME_VK_BROWSER_REFRESH = 'Browser Refresh';
  NAME_VK_BROWSER_STOP = 'Browser Stop';
  NAME_VK_BROWSER_SEARCH = 'Browser Search';
  NAME_VK_BROWSER_FAVORITES = 'Browser Favorites';
  NAME_VK_BROWSER_HOME = 'Browser Start/Home';
  NAME_VK_VOLUME_MUTE = 'Volume Mute';
  NAME_VK_VOLUME_DOWN = 'Volume Down';
  NAME_VK_VOLUME_UP = 'Volume Up';
  NAME_VK_MEDIA_NEXT_TRACK = 'Next Track';
  NAME_VK_MEDIA_PREV_TRACK = 'Previous Track';
  NAME_VK_MEDIA_STOP = 'Stop Media';
  NAME_VK_MEDIA_PLAY_PAUSE = 'Play/Pause Media';
  NAME_VK_LAUNCH_MAIL = 'Start Mail';
  NAME_VK_LAUNCH_MEDIA_SELECT = 'Select Media';
  NAME_VK_LAUNCH_APP1 = 'Start Application 1';
  NAME_VK_LAUNCH_APP2 = 'Start Application 2';

const
  mmsyst = 'winmm.dll';
  kernel32 = 'kernel32.dll';
  HotKeyAtomPrefix = 'HotKeyManagerHotKey';
  ModName_Shift = 'Shift';
  ModName_Ctrl = 'Ctrl';
  ModName_Alt = 'Alt';
  ModName_Win = 'Win';
  VK2_SHIFT = 32;
  VK2_CONTROL = 64;
  VK2_ALT = 128;
  VK2_WIN = 256;

var
  EnglishKeyboardLayout: HKL;
  ShouldUnloadEnglishKeyboardLayout: Boolean;
  LocalModName_Shift: string = ModName_Shift;
  LocalModName_Ctrl: string = ModName_Ctrl;
  LocalModName_Alt: string = ModName_Alt;
  LocalModName_Win: string = ModName_Win;

function IsExtendedKey(Key: Word): Boolean;
begin
  Result := ((Key >= _VK_BROWSER_BACK) and (Key <= _VK_LAUNCH_APP2));
end;

function GetHotKey(Modifiers, Key: Word): Cardinal;
var
  HK: Cardinal;
begin
  HK := 0;
  if (Modifiers and MOD_ALT) <> 0 then
    Inc(HK, VK2_ALT);
  if (Modifiers and MOD_CONTROL) <> 0 then
    Inc(HK, VK2_CONTROL);
  if (Modifiers and MOD_SHIFT) <> 0 then
    Inc(HK, VK2_SHIFT);
  if (Modifiers and MOD_WIN) <> 0 then
    Inc(HK, VK2_WIN);
  HK := HK shl 8;
  Inc(HK, Key);
  Result := HK;
end;

procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
var
  Virtuals: Integer;
  v: Word;
  X: Word;
begin
  Key := Byte(HotKey);
  X := HotKey shr 8;
  Virtuals := X;
  v := 0;
  if (Virtuals and VK2_WIN) <> 0 then
    Inc(v, MOD_WIN);
  if (Virtuals and VK2_ALT) <> 0 then
    Inc(v, MOD_ALT);
  if (Virtuals and VK2_CONTROL) <> 0 then
    Inc(v, MOD_CONTROL);
  if (Virtuals and VK2_SHIFT) <> 0 then
    Inc(v, MOD_SHIFT);
  Modifiers := v;
end;

function HotKeyToText(HotKey: Cardinal; Localized: Boolean): string;
  function GetExtendedVKName(Key: Word): string;
  begin
    case Key of
      _VK_BROWSER_BACK: Result := NAME_VK_BROWSER_BACK;
      _VK_BROWSER_FORWARD: Result := NAME_VK_BROWSER_FORWARD;
      _VK_BROWSER_REFRESH: Result := NAME_VK_BROWSER_REFRESH;
      _VK_BROWSER_STOP: Result := NAME_VK_BROWSER_STOP;
      _VK_BROWSER_SEARCH: Result := NAME_VK_BROWSER_SEARCH;
      _VK_BROWSER_FAVORITES: Result := NAME_VK_BROWSER_FAVORITES;
      _VK_BROWSER_HOME: Result := NAME_VK_BROWSER_HOME;
      _VK_VOLUME_MUTE: Result := NAME_VK_VOLUME_MUTE;
      _VK_VOLUME_DOWN: Result := NAME_VK_VOLUME_DOWN;
      _VK_VOLUME_UP: Result := NAME_VK_VOLUME_UP;
      _VK_MEDIA_NEXT_TRACK: Result := NAME_VK_MEDIA_NEXT_TRACK;
      _VK_MEDIA_PREV_TRACK: Result := NAME_VK_MEDIA_PREV_TRACK;
      _VK_MEDIA_STOP: Result := NAME_VK_MEDIA_STOP;
      _VK_MEDIA_PLAY_PAUSE: Result := NAME_VK_MEDIA_PLAY_PAUSE;
      _VK_LAUNCH_MAIL: Result := NAME_VK_LAUNCH_MAIL;
      _VK_LAUNCH_MEDIA_SELECT: Result := NAME_VK_LAUNCH_MEDIA_SELECT;
      _VK_LAUNCH_APP1: Result := NAME_VK_LAUNCH_APP1;
      _VK_LAUNCH_APP2: Result := NAME_VK_LAUNCH_APP2;
    else
      Result := '';
    end;
  end;
  function GetModifierNames: string;
  var
    s: string;
  begin
    s := '';
    if Localized then begin
      if (HotKey and $4000) <> 0 then // scCtrl
        s := s + LocalModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then // scShift
        s := s + LocalModName_Shift + '+';
      if (HotKey and $8000) <> 0 then // scAlt
        s := s + LocalModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        s := s + LocalModName_Win + '+';
    end
    else begin
      if (HotKey and $4000) <> 0 then // scCtrl
        s := s + ModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then // scShift
        s := s + ModName_Shift + '+';
      if (HotKey and $8000) <> 0 then // scAlt
        s := s + ModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        s := s + ModName_Win + '+';
    end;
    Result := s;
  end;
  function GetVKName(Special: Boolean): string;
  var
    scanCode: Cardinal;
    KeyName: array[0..255] of Char;
    oldkl: HKL;
    Modifiers, Key: Word;
  begin
    Result := '';
    if Localized then {// Local language key names}  begin
      if Special then
        scanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16) or (1 shl 24)
      else
        scanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16);
      if scanCode <> 0 then begin
        GetKeyNameText(scanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
      end;
    end
    else {// English key names}  begin
      if Special then
        scanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16) or (1 shl 24)
      else
        scanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16);
      if scanCode <> 0 then begin
        oldkl := GetKeyboardLayout(0);
        if oldkl <> EnglishKeyboardLayout then
          ActivateKeyboardLayout(EnglishKeyboardLayout, 0); // Set English kbd. layout
        GetKeyNameText(scanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
        if oldkl <> EnglishKeyboardLayout then begin
          if ShouldUnloadEnglishKeyboardLayout then
            UnloadKeyboardLayout(EnglishKeyboardLayout); // Restore prev. kbd. layout
          ActivateKeyboardLayout(oldkl, 0);
        end;
      end;
    end;
    if Length(Result) <= 1 then begin
      // Try the internally defined names
      SeparateHotKey(HotKey, Modifiers, Key);
      if IsExtendedKey(Key) then
        Result := GetExtendedVKName(Key);
    end;
  end;
var
  KeyName: string;
begin
  case Byte(HotKey) of
    // PgUp, PgDn, End, Home, Left, Up, Right, Down, Ins, Del
    $21..$28, $2D, $2E: KeyName := GetVKName(True);
  else
    KeyName := GetVKName(False);
  end;
  Result := GetModifierNames + KeyName;
end;

constructor TDxCustomEdit.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  Downed := False;
  FMiniCaret := 0;
  m_InputHint := '';
  DxHint := nil;
  FCaretColor := clWhite;
  FOnClick := nil;
  FEnableFocus := True;
  FClick := False;
  FSelClickStart := False;
  FSelClickEnd := False;
  FClickX := 0;
  FSelStart := -1;
  FSelEnd := -1;
  FStartTextX := 0;
  FSelTextStart := 0;
  FSelTextEnd := 0;
  FCurPos := 0;
  FClickSound := csNone;
  FShowCaret := True;
  FNomberOnly := False;
  FIsHotKey := False;
  FHotKey := 0;
  FTransparent := True;
  FEnabled := True;
  FSecondChineseChar := False;
  FShowCaretTick := GetTickCount;
  FFrameVisible := True;
  FFrameHot := False;
  FFrameSize := 1;
  FFrameColor := $00406F77;
  FFrameHotColor := $00599AA8;
  FAlignment := taLeftJustify;
  FRightClick := True;
  FFontSelColor := clWhite;
end;
procedure TDxCustomEdit.SetSelLength(Value: Integer);
begin
  SetSelStart(Value - 1);
  SetSelEnd(Value - 1);
end;
function TDxCustomEdit.ReadSelLength(): Integer;
begin
  Result := abs(FSelStart - FSelEnd);
end;
procedure TDxCustomEdit.SetSelStart(Value: Integer);
begin
  if FSelStart <> Value then
    FSelStart := Value;
end;
procedure TDxCustomEdit.SetSelEnd(Value: Integer);
begin
  if FSelEnd <> Value then
    FSelEnd := Value;
end;
procedure TDxCustomEdit.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
    FMaxLength := Value;
end;
procedure TDxCustomEdit.SetPasswordChar(Value: Char);
begin
  if FPasswordChar <> Value then
    FPasswordChar := Value;
end;
procedure TDxCustomEdit.SetNomberOnly(Value: Boolean);
begin
  if FNomberOnly <> Value then
    FNomberOnly := Value;
end;
procedure TDxCustomEdit.SetIsHotKey(Value: Boolean);
begin
  if FIsHotKey <> Value then
    FIsHotKey := Value;
end;
procedure TDxCustomEdit.SetHotKey(Value: Cardinal);
begin
  if FHotKey <> Value then
    FHotKey := Value;
end;
procedure TDxCustomEdit.SetAtom(Value: Word);
begin
  if FAtom <> Value then
    FAtom := Value;
end;
procedure TDxCustomEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
    FAlignment := Value;
end;
procedure TDxCustomEdit.ShowCaret();
begin
  FShowCaret := True;
  FShowCaretTick := GetTickCount;
end;
procedure TDxCustomEdit.SetFocus();
var
  I: Integer;
begin
  if EnableFocus and Visible then SetDFocus(Self);
  if (FocusedControl = nil) and Visible then begin
    for I := 0 to DControls.Count - 1 do begin
      if (FocusedControl = nil) then
        TDControl(DControls[I]).SetFocus
      else break;
    end;
  end;
end;
procedure TDxCustomEdit.SetFontSelColor(const Value: TColor);
begin
  if FFontSelColor <> Value then
  begin
    FFontSelColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;

procedure TDxCustomEdit.ChangeCurPos(nPos: Integer; boLast: Boolean = False);
begin
  if Caption = '' then
    Exit;
  if boLast then
  begin
    FCurPos := AnsiTextLength(Caption);
    Exit;
  end;
  if nPos = 1 then
  begin
    case ByteType(AnsiString(Caption), FCurPos + 1) of
      mbSingleByte:
        nPos := 1;
      mbLeadByte:
        nPos := 2;
      mbTrailByte:
        nPos := 2;
    end;
  end
  else
  begin
    case ByteType(AnsiString(Caption), FCurPos) of
      mbSingleByte:
        nPos := -1;
      mbLeadByte:
        nPos := -2;
      mbTrailByte:
        nPos := -2;
    end;
  end;
  if ((FCurPos + nPos) <= AnsiTextLength(Caption)) then
  begin
    if ((FCurPos + nPos) >= 0) then
      FCurPos := FCurPos + nPos;
  end;

  if FSelClickStart then
  begin
    FSelClickStart := False;
    FSelStart := FCurPos;
  end;
  if FSelClickEnd then
  begin
    FSelClickEnd := False;
    FSelEnd := FCurPos;
  end;
end;

function TDxCustomEdit.KeyPress(var Key: Char): Boolean;
var
  s, cStr: String;
  i, nlen, cpLen: Integer;
  pStart: Integer;
  pEnd: Integer;
begin
  if not FEnabled or FIsHotKey then
    Exit;
  if not self.Visible then
    Exit;
  if (self.DParent = nil) or (not self.DParent.Visible) then
    Exit;
  s := Caption;
  try
    if (Ord(Key) in [0,VK_RETURN, VK_ESCAPE]) then
    begin
      Result := inherited KeyPress(Key);
      Exit;
    end;
    if not FNomberOnly and IsKeyPressed(VK_CONTROL) and (Ord(Key) in [1..27]) then
    begin

      if (Ord(Key) = 22) then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := Clipboard.AsText;
          if cStr <> '' then
          begin
            cpLen := FMaxLength - AnsiTextLength(Caption) + abs(FSelStart - FSelEnd);
            FSelStart := -1;
            FSelEnd := -1;
            Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
            FCurPos := pStart;
            nlen := AnsiTextLength(cStr);
            if nlen < cpLen then
              cpLen := nlen;
            Caption := CopyAnisText(Caption, 1, FCurPos) + CopyAnisText(cStr, 1, cpLen) + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
            Inc(FCurPos, cpLen);
          end;
        end
        else
        begin
          cpLen := FMaxLength - AnsiTextLength(Caption);
          if cpLen > 0 then
          begin
            cStr := Clipboard.AsText;
            if cStr <> '' then
            begin
              nlen := AnsiTextLength(cStr);
              if nlen < cpLen then
                cpLen := nlen;
              Caption := CopyAnisText(Caption, 1, FCurPos) + CopyAnisText(cStr, 1, cpLen) + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
              Inc(FCurPos, cpLen);
            end;
          end
          else
            Beep;
        end;
      end;
      if (Ord(Key) = 3) and (FPasswordChar = #0000) and (Caption <> '') then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := CopyAnisText(Caption, pStart + 1, abs(FSelStart - FSelEnd));
          if cStr <> '' then
          begin
            Clipboard.AsText := cStr;
          end;
        end;
      end;
      if (Ord(Key) = 24) and (FPasswordChar = #0000) and (Caption <> '') then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := CopyAnisText(Caption, pStart + 1, abs(FSelStart - FSelEnd));
          if cStr <> '' then
          begin
            Clipboard.AsText := cStr;
          end;
          FSelStart := -1;
          FSelEnd := -1;
          Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
          FCurPos := pStart;
        end;
      end;
      if (Ord(Key) = 1) and not FIsHotKey and (Caption <> '') then
      begin
        FSelStart := 0;
        FSelEnd := AnsiTextLength(Caption);
        FCurPos := FSelEnd;
      end;
      Result := inherited KeyPress(Key);
      Exit;
    end;
    Result := inherited KeyPress(Key);
    if Result then
    begin
      ShowCaret();
      case Ord(Key) of
        VK_BACK:
          begin
            if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
            begin
              if FSelStart > FSelEnd then
              begin
                pStart := FSelEnd;
                pEnd := FSelStart;
              end;
              if FSelStart < FSelEnd then
              begin
                pStart := FSelStart;
                pEnd := FSelEnd;
              end;
              FSelStart := -1;
              FSelEnd := -1;
              Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
              FCurPos := pStart;
            end
            else
            begin
              if (FCurPos > 0) then
              begin
                nlen := 1;
                case ByteType(AnsiString(Caption), FCurPos) of
                  mbSingleByte:
                    nlen := 1;
                  mbLeadByte:
                    nlen := 2;
                  mbTrailByte:
                    nlen := 2;
                end;
                Caption := CopyAnisText(Caption, 1, FCurPos - nlen) + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                Dec(FCurPos, nlen);
              end;
            end;
          end;
      else
        begin
          if (FMaxLength <= 0) or (FMaxLength > MaxChar) then
            FMaxLength := MaxChar;
          if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
          begin
            if FSelStart > FSelEnd then
            begin
              pStart := FSelEnd;
              pEnd := FSelStart;
            end;
            if FSelStart < FSelEnd then
            begin
              pStart := FSelStart;
              pEnd := FSelEnd;
            end;
            if FNomberOnly then
            begin
              if (Key >= #$0030) and (Key <= #$0039) then
              begin
                FSelStart := -1;
                FSelEnd := -1;
                Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
                FCurPos := pStart;
                FSecondChineseChar := False;
                if AnsiTextLength(Caption) < FMaxLength then
                begin
                  Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                  Inc(FCurPos);
                end
                else
                  Beep;
              end
              else
                Beep;
            end
            else
            begin
              FSelStart := -1;
              FSelEnd := -1;
              Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
              FCurPos := pStart;
              if Key > #$0080 then
              begin
                if FSecondChineseChar then
                begin
                  FSecondChineseChar := False;
                  if AnsiTextLength(Caption) < FMaxLength then
                  begin
                    Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                    Inc(FCurPos, 2);
                  end
                  else
                    Beep;
                end
                else
                begin
                  if AnsiTextLength(Caption) + 1 < FMaxLength then
                  begin
                    FSecondChineseChar := True;
                    Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                    Inc(FCurPos, 2);
                  end
                  else
                    Beep;
                end;
              end
              else
              begin
                FSecondChineseChar := False;
                if AnsiTextLength(Caption) < FMaxLength then
                begin
                  Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                  Inc(FCurPos);
                end
                else
                  Beep;
              end;
            end;
          end
          else
          begin
            if FNomberOnly then
            begin
              if (Key >= #$0030) and (Key <= #$0039) then
              begin
                FSelStart := -1;
                FSelEnd := -1;
                FSecondChineseChar := False;
                if AnsiTextLength(Caption) < FMaxLength then
                begin
                  Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                  Inc(FCurPos);
                end;
              end
              else
                Beep;
            end
            else
            begin
              FSelStart := -1;
              FSelEnd := -1;
              if Key > #$0080 then
              begin
                if FSecondChineseChar then
                begin
                  FSecondChineseChar := False;
                  if AnsiTextLength(Caption) < FMaxLength then
                  begin
                    Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                    Inc(FCurPos, 2);
                    FSelStart := FCurPos;
                  end
                  else
                    Beep;
                end
                else
                begin
                  if AnsiTextLength(Caption) + 1 < FMaxLength then
                  begin
                    FSecondChineseChar := True;
                    Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                    Inc(FCurPos, 2);
                    FSelStart := FCurPos;
                  end
                  else
                    Beep;
                end;
              end
              else
              begin
                FSecondChineseChar := False;
                if AnsiTextLength(Caption) < FMaxLength then
                begin
                  Caption := CopyAnisText(Caption, 1, FCurPos) + Key + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
                  Inc(FCurPos);
                  FSelStart := FCurPos;
                end
                else
                  Beep;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    if s <> Caption then
    begin
      if Assigned(FOnTextChanged) then
        FOnTextChanged(self, Caption);
    end;
  end;
end;

function TDxCustomEdit.KeyPressEx(var Key: Char): Boolean;
var
  s, cStr: string;
  i, nlen, cpLen: Integer;
  pStart: Integer;
  pEnd: Integer;
begin
  if not FEnabled or FIsHotKey then
    Exit;
  if not self.Visible then
    Exit;
  if (self.DParent = nil) or (not self.DParent.Visible) then
    Exit;
  s := Caption;
  try
    if not FNomberOnly and (Ord(Key) in [1..27]) then
    begin
      if (Ord(Key) = 22) then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := Clipboard.AsText;
          if cStr <> '' then
          begin
            cpLen := FMaxLength - AnsiTextLength(Caption) + abs(FSelStart - FSelEnd);
            FSelStart := -1;
            FSelEnd := -1;
            Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
            FCurPos := pStart;
            nlen := AnsiTextLength(cStr);
            if nlen < cpLen then
              cpLen := nlen;
            Caption := CopyAnisText(Caption, 1, FCurPos) + CopyAnisText(cStr, 1, cpLen) + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
            Inc(FCurPos, cpLen);
          end;
        end
        else
        begin
          cpLen := FMaxLength - AnsiTextLength(Caption);
          if cpLen > 0 then
          begin
            cStr := Clipboard.AsText;
            if cStr <> '' then
            begin
              nlen := AnsiTextLength(cStr);
              if nlen < cpLen then
                cpLen := nlen;
              Caption := CopyAnisText(Caption, 1, FCurPos) + CopyAnisText(cStr, 1, cpLen) + CopyAnisText(Caption, FCurPos + 1, AnsiTextLength(Caption));
              Inc(FCurPos, cpLen);
            end;
          end
          else
            Beep;
        end;
      end;
      if (Ord(Key) = 3) and (FPasswordChar = #0000) and (Caption <> '') then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := CopyAnisText(Caption, pStart + 1, abs(FSelStart - FSelEnd));
          if cStr <> '' then
          begin
            Clipboard.AsText := cStr;
          end;
        end;
      end;
      if (Ord(Key) = 24) and (FPasswordChar = #0000) and (Caption <> '') then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          cStr := CopyAnisText(Caption, pStart + 1, abs(FSelStart - FSelEnd));
          if cStr <> '' then
          begin
            Clipboard.AsText := cStr;
          end;
          FSelStart := -1;
          FSelEnd := -1;
          Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
          FCurPos := pStart;
        end;
      end;
      if (Ord(Key) = 1) and not FIsHotKey and (Caption <> '') then
      begin
        FSelStart := 0;
        FSelEnd := AnsiTextLength(Caption);
        FCurPos := FSelEnd;
      end;
      if (Ord(Key) = VK_BACK) and not FIsHotKey and (Caption <> '') then
      begin
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
        begin
          if FSelStart > FSelEnd then
          begin
            pStart := FSelEnd;
            pEnd := FSelStart;
          end;
          if FSelStart < FSelEnd then
          begin
            pStart := FSelStart;
            pEnd := FSelEnd;
          end;
          FSelStart := -1;
          FSelEnd := -1;
          Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
          FCurPos := pStart;
        end;
      end;
    end;
  finally
    if s <> Caption then
    begin
      if Assigned(FOnTextChanged) then
        FOnTextChanged(self, Caption);
    end;
  end;
end;

procedure TDxCustomEdit.Enter;
begin
  inherited;
end;
procedure TDxCustomEdit.Leave;
begin
  FSelStart := -1;
  FSelEnd := -1;
  inherited;
end;

var
  HotKeyProc: function(HotKey: Cardinal): Boolean of object;

function TDxCustomEdit.SetOfHotKey(HotKey: Cardinal): Word;
var
  Modifiers, Key: Word;
  Atom: Word;
begin
  Result := 0;
  if (HotKey <> 0){ and Assigned(HotKeyProc) and not HotKeyProc(HotKey)} then
  begin
    if FAtom <> 0 then
    begin
      UnregisterHotKey(g_MainHWnd, FAtom);
      GlobalDeleteAtom(FAtom);
    end;
    Result := 0;
    FHotKey := HotKey;
    Caption := HotKeyToText(HotKey, True);
  end;
end;
function TDxCustomEdit.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  pStart, pEnd: Integer;
  M: Word;
  HK: Cardinal;
  ret       : Boolean;
  s, tmpStr: string;
begin
  if not FEnabled then
    Exit;
  s := Caption;
  try
    ret := inherited KeyDown(Key, Shift);
    if ret then
    begin
      if FIsHotKey then
      begin
        if Key in [VK_BACK, VK_DELETE] then
        begin
          if (FHotKey <> 0) then
          begin
            FHotKey := 0;
            FAtom := 0;
          end;
          Caption := '';
          Exit;
        end;
        if (Key = VK_TAB) or (Char(Key) in ['A'..'Z', 'a'..'z']) then
        begin
          M := 0;
          if ssCtrl in Shift then
            M := M or MOD_CONTROL;
          if ssAlt in Shift then
            M := M or MOD_ALT;
          if ssShift in Shift then
            M := M or MOD_SHIFT;
          HK := GetHotKey(M, Key);
          if (HK <> 0) and (FHotKey <> 0) then
          begin
            FAtom := 0;
            FHotKey := 0;
            Caption := '';
          end;
          if (HK <> 0) then
            SetOfHotKey(HK);
        end;
      end
      else
      begin
        if (Char(Key) in ['0'..'9', 'A'..'Z', 'a'..'z']) then
          ShowCaret();
        if ssShift in Shift then
        begin
          case Key of
            VK_RIGHT:
              begin
                FSelClickEnd := True;
                ChangeCurPos(1);
              end;
            VK_LEFT:
              begin
                FSelClickEnd := True;
                ChangeCurPos(-1);
              end;
            VK_HOME:
              begin
                FSelEnd := FCurPos;
                FSelStart := 0;
              end;
            VK_END:
              begin
                FSelStart := FCurPos;
                FSelEnd := AnsiTextLength(Text);
              end;
          end;
        end
        else
        begin
          case Key of
            VK_LEFT:
              begin
                FSelStart := -1;
                FSelEnd := -1;
                FSelClickStart := True;
                ChangeCurPos(-1);
              end;
            VK_RIGHT:
              begin
                FSelStart := -1;
                FSelEnd := -1;
                FSelClickStart := True;
                ChangeCurPos(1);
              end;
            VK_HOME:
              begin
                FSelStart := -1;
                FSelEnd := -1;
                FCurPos := 0;
                FSelClickStart := True;
              end;
            VK_END:
              begin
                FSelStart := -1;
                FSelEnd := -1;
                FCurPos := AnsiTextLength(Text);
                FSelClickStart := True;
              end;
            VK_DELETE:
              begin
                if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) then
                begin
                  if FSelStart > FSelEnd then
                  begin
                    pStart := FSelEnd;
                    pEnd := FSelStart;
                  end;
                  if FSelStart < FSelEnd then
                  begin
                    pStart := FSelStart;
                    pEnd := FSelEnd;
                  end;
                  FSelStart := -1;
                  FSelEnd := -1;
                  Caption := CopyAnisText(Caption, 1, pStart) + CopyAnisText(Caption, pEnd + 1, AnsiTextLength(Caption));
                  FCurPos := pStart;
                end
                else
                begin
                  if FCurPos < AnsiTextLength(Caption) then
                  begin
                    pEnd := 1;
                    case ByteType(AnsiString(Caption), FCurPos + 1) of
                      mbSingleByte:
                        pEnd := 1;
                      mbLeadByte:
                        pEnd := 2;
                      mbTrailByte:
                        pEnd := 2;
                    end;
                    Caption := CopyAnisText(Caption, 1, FCurPos) + CopyAnisText(Caption, FCurPos + pEnd + 1, AnsiTextLength(Caption));

                  end;
                end;
              end;
          end;
        end;
      end;
    end;
    Result := ret;
  finally
    if s <> Caption then
    begin
      if Assigned(FOnTextChanged) then
        FOnTextChanged(self, Caption);
    end;
  end;
end;

procedure TDxCustomEdit.DirectPaint(dsurface: TCustomCanvas);
var
  bFocused, bIsChinese: Boolean;
  i, ii, oCSize, WidthX, nl, nr, nt: Integer;
  tmpword: string;
  tmpColor, OldColor, OldBColor: TColor;
  ob, op, ofc, oFColor: TColor;
  OldFont: TFont;
  off, ss, se, cPos: Integer;
  ATextLine: TCustomLockableTexture;
  ATextureFont: TCustomTextureFont;
begin
  if not Visible then
    Exit;
  ATextureFont := FontManager.GetFont('宋体',9,[]);
  nl := SurfaceX(Left);
  nt := SurfaceY(Top);
  if (FocusedControl <> self) and (DxHint <> nil) then
    DxHint.Visible := False;
  if FEnabled and not FIsHotKey then
  begin
    if GetTickCount - FShowCaretTick >= 400 then
    begin
      FShowCaretTick := GetTickCount;
      FShowCaret := not FShowCaret;
    end;
    if (FCurPos > AnsiTextLength(Caption)) then
      FCurPos := AnsiTextLength(Caption);
  end;
  if (FPasswordChar <> #0000) and not FIsHotKey then
  begin
    tmpword := '';
    for i := 1 to AnsiTextLength(Caption) do
      if Caption[i] <> FPasswordChar then
        tmpword := tmpword + FPasswordChar;
  end
  else
    tmpword := Ansistring(Caption);

  if not FIsHotKey and FEnabled and FClick then
  begin
    FClick := False;
    if (FClickX < 0) then
      FClickX := 0;
    se := FontManager.Default.TextWidth(tmpword);
    if FClickX > se then
      FClickX := se;

    cPos := FClickX div 6;
//    ConsoleDebug('cPos := ' + IntToStr(cPos));
    case ByteType(AnsiString(Caption), cPos + 1) of
      mbSingleByte:
        begin
        FCurPos := cPos;
        end;
      mbLeadByte:
        begin
          FCurPos := cPos;
        end;
      mbTrailByte:
        begin
          if cPos mod 2 = 0 then
          begin
            if FClickX mod 6 in [3..5] then
              FCurPos := cPos + 1
            else
              FCurPos := cPos - 1;
          end
          else
          begin
            if FClickX mod 12 in [6..11] then
              FCurPos := cPos + 1
            else
              FCurPos := cPos - 1;
          end;
        end;
    end;
    if FSelClickStart then
    begin
      FSelClickStart := False;
      FSelStart := FCurPos;
    end;
    if FSelClickEnd then
    begin
      FSelClickEnd := False;
      FSelEnd := FCurPos;
    end;
  end;

  WidthX := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, FCurPos));
  if WidthX + 3 - FStartTextX > Width then
    FStartTextX := WidthX + 3 - Width;
  if ((WidthX - FStartTextX) < 0) then
    FStartTextX := FStartTextX + (WidthX - FStartTextX);
  if FTransparent then
  begin
    if FEnabled then
    begin
      if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) and (FocusedControl = self) then begin
        ss := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, FSelStart));
        se := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, FSelEnd));
        //增加选取复制文字背景
        dsurface.FillRectAlpha(IntRectBDS(_MAX(nl - 1, nl + ss - 1 - FStartTextX),
                                         nt - 1 - Integer(FMiniCaret),
                                         _MIN(nl + self.Width + 1, nl + se + 1 - FStartTextX),
                                         nt + FontManager.Default.TextHeight('c') + 1 - Integer(FMiniCaret)),
                                         clBlue, 255);
      end;
      case FAlignment of
        taCenter: begin
            dsurface.TextOut((nl - FStartTextX) + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2), nt+2, tmpword, self.Font.Color);
          end;
        taLeftJustify: begin
            ss := nl - FStartTextX;
            // 绘制文字
            ATextLine := ATextureFont.TextOut(tmpword);
            if ATextLine <> nil then
            begin
              dsurface.DrawInRect((nl - FStartTextX), nt - Integer(FMiniCaret) * 1,
              IntRectBDS(nl, nt - Integer(FMiniCaret), nl + Width - 1, nt + Height - 1),
              ATextLine, self.Font.Color);
            end;

            if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) and (FocusedControl = self) then begin
              if FSelStart < FSelEnd then begin
                i := FSelStart;
                ii := FSelEnd;
              end else begin
                ii := FSelStart;
                i := FSelEnd;
              end;
              ss := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, i));
              se := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, ii));
              //增加选取复制文字颜色

              ATextLine := ATextureFont.TextOut(tmpword);
              if ATextLine <> nil then
              begin
                dsurface.DrawInRect((nl - FStartTextX), nt - Integer(FMiniCaret) * 1,
                IntRectBDS(_MAX(nl - 1, nl + ss - 1 - FStartTextX),
                                          nt - 1 - Integer(FMiniCaret),
                                          _MIN(nl + self.Width - 1, nl + se + 1 - FStartTextX),
                                          nt + FontManager.Default.TextHeight('c') + 1 - Integer(FMiniCaret)),
                ATextLine, FFontSelColor);
              end;
//                  ConsoleDebug(Format('_MAX(nl - 1, nl + ss - 1 - FStartTextX):%d',[_MAX(nl - 1, nl + ss - 1 - FStartTextX)]));
//                  ConsoleDebug(Format('_MIN(nl + self.Width - 1, nl + se + 1 - FStartTextX):%d',[_MIN(nl + self.Width - 1, nl + se + 1 - FStartTextX)]));
            end;
          end;
      end;
    end;
  end
  else
  begin
    if FFrameVisible then
    begin
      if FEnabled or (self is TDComboBox) then
      begin
        if FFrameHot then
          tmpColor := FFrameHotColor
        else
          tmpColor := FFrameColor;
      end
      else
        tmpColor := clGray;
      dsurface.FrameRect(IntRectBDS(nl - 3, nt - 3, 3 + Width - 1, 3 + Height - 1), tmpColor);
    end;
    if FIsHotKey then
    begin
      bFocused := FocusedControl = self;
      if FEnabled then
      begin
        dsurface.FillRect(IntRectBDS(nl + FFrameSize - 3 + Integer(bFocused), nt + FFrameSize - 3 + Integer(bFocused), Width - FFrameSize - 1 - Integer(bFocused), Height - FFrameSize - 1 - Integer(bFocused)), clBlack);
        if bFocused then
          tmpColor := clLime
        else
          tmpColor := self.Font.Color;
      end
      else
      begin
        dsurface.FillRect(IntRectBDS(nl + FFrameSize - 3, nt + FFrameSize - 3, Width - FFrameSize - 1, Height - FFrameSize - 1), self.Color);
        tmpColor := clGray;
      end;
      case FAlignment of
        taCenter:
          dsurface.TextOut((nl - FStartTextX) + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2), nt, tmpword, tmpColor);
        taLeftJustify:
          begin
            dsurface.TextOut(nl - FStartTextX, nt, tmpword, tmpColor);
          end;
      end;
    end
    else
    begin
      dsurface.FillRect(IntRectBDS(nl - 3 + FFrameSize, nt - 3 + FFrameSize, Width,  Height), self.Color);
      if FEnabled then
      begin
        //复制文字以及背景
        if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) and (FocusedControl = self) then begin
          if FSelStart < FSelEnd then begin
            i := FSelStart;
            ii := FSelEnd;
          end else begin
            ii := FSelStart;
            i := FSelEnd;
          end;
          ss := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, i));
          se := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, ii));
          //增加选取复制文字背景
          dsurface.FillRectAlpha(IntRectBDS(_MAX(nl - 1, nl + ss - 1 - FStartTextX),
                                      nt - 1 - Integer(FMiniCaret),
                                      _MIN(nl + self.Width - 1, nl + se + 1 - FStartTextX),
                                      nt + FontManager.Default.TextHeight('c') + 1 - Integer(FMiniCaret)),
                                    clBlue , 255);
        end;

        case FAlignment of
          taCenter:   begin
              dsurface.TextOut(
                (nl - FStartTextX) + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2),
                nt - Integer(FMiniCaret) * 1,
                tmpword,IntColor(cColor1(self.Font.Color), 255));
            end;
          taLeftJustify: begin
              ss := nl - FStartTextX;
              // 绘制文字
              ATextLine := ATextureFont.TextOut(tmpword);
              if ATextLine <> nil then
              begin
                dsurface.DrawInRect((nl - FStartTextX), nt - Integer(FMiniCaret) * 1,
                IntRectBDS(nl, nt - Integer(FMiniCaret), nl + Width - 1, nt + Height - 1),
                ATextLine, self.Font.Color);
              end;

              if (FSelStart > -1) and (FSelEnd > -1) and (FSelStart <> FSelEnd) and (FocusedControl = self) then begin
                if FSelStart < FSelEnd then begin
                  i := FSelStart;
                  ii := FSelEnd;
                end else begin
                  ii := FSelStart;
                  i := FSelEnd;
                end;
                ss := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, i));
                se := FontManager.Default.TextWidth(CopyAnisText(tmpword, 1, ii));
                //增加选取复制文字颜色

                ATextLine := ATextureFont.TextOut(tmpword);
                if ATextLine <> nil then
                begin
                  dsurface.DrawInRect((nl - FStartTextX), nt - Integer(FMiniCaret) * 1,
                  IntRectBDS(_MAX(nl - 1, nl + ss - 1 - FStartTextX),
                                            nt - 1 - Integer(FMiniCaret),
                                            _MIN(nl + self.Width - 1, nl + se + 1 - FStartTextX),
                                            nt + FontManager.Default.TextHeight('c') + 1 - Integer(FMiniCaret)),
                  ATextLine, self.Font.Color);
                end;
              end;
            end;
        end;
      end
      else
      begin
        case FAlignment of
          taCenter:
            dsurface.TextOut((nl - FStartTextX) + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2), nt - Integer(FMiniCaret) * 1, tmpword, clGray);
          taLeftJustify:
            begin
              ss := nl - FStartTextX;
              Brush.Style := bsClear;
              dsurface.TextOut(ss, nt - Integer(FMiniCaret), string(tmpword), Font.Color);

            end;
        end;
      end;
    end;
    if self is TDComboBox then begin
      dsurface.FillTri(
        Point2f(nl + Width - DECALW * 2 + Integer(Downed),
           nt + (Height - DECALH) div 2 - 2 + Integer(Downed)),
          Point2f(nl + Width - DECALW + Integer(Downed),
          nt + (Height - DECALH) div 2 - 2 + Integer(Downed)),
          Point2f(nl + Width - DECALW - DECALW div 2 + Integer(Downed),
          nt + (Height - DECALH) div 2 + DECALH - 2 + Integer(Downed))
          ,cColor1(tmpColor),cColor1(tmpColor),cColor1(tmpColor));

    end;

  end;
  if FEnabled then
  begin
    if (FocusedControl = self) then
    begin

        SetFrameHot(True);
        if (AnsiTextLength(Caption) >= FCurPos) and (FShowCaret and not FIsHotKey) then
        begin
          case FAlignment of
            taCenter:
              begin
                dsurface.FillRectAlpha(IntRectBDS(nl + WidthX - FStartTextX + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2),
                nt - Integer(FMiniCaret <> 0) * 1,
                nl + WidthX + 2 - Integer(FMiniCaret <> 0) - FStartTextX + ((Width - FontManager.Default.TextWidth(tmpword)) div 2 - 2),
                nt - Integer(FMiniCaret <> 0) * 1 + FontManager.Default.TextHeight('c')), FCaretColor, 255);
              end;
            taLeftJustify:
              begin
                dsurface.FillRectAlpha(IntRectBDS(nl + WidthX - FStartTextX, nt - Integer(FMiniCaret) * 1 - Integer(FMiniCaret = 0),
                nl + WidthX + 2 - FStartTextX - Integer(FMiniCaret <> 0),
                nt - Integer(FMiniCaret) * 1 + FontManager.Default.TextHeight('c') + Integer(FMiniCaret = 0)), FCaretColor, 255);
              end;
          end;
        end;

    end;
  end;
  if (Text = '') and (m_InputHint <> '') then
  begin
    dsurface.TextOut(nl + self.Width - FontManager.Default.TextWidth(m_InputHint) - 4, nt - Integer(FMiniCaret), m_InputHint, clGray);
  end;
  for i := 0 to DControls.count - 1 do
    if TDControl(DControls[i]).Visible then
      TDControl(DControls[i]).DirectPaint(dsurface);
      DragModePaint(dsurface);
end;

function TDxCustomEdit.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  FSelClickEnd := False;
  if inherited MouseMove(Shift, X, Y) then
  begin
    if [ssLeft] = Shift then
    begin
      if FEnabled and not FIsHotKey and (MouseCaptureControl = self) and (Caption <> '') then
      begin
        FClick := True;
        FSelClickEnd := True;
        FClickX := X - Left + FStartTextX;
      end;
    end
    else
    begin

    end;
    Result := True;
  end;
end;

function TDxCustomEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  FSelClickStart := False;
  if inherited MouseDown(Button, Shift, X, Y) then
  begin
    if FEnabled and not FIsHotKey and (MouseCaptureControl = self) then
    begin
      if Button = mbLeft then
      begin
        FSelEnd := -1;
        FSelStart := -1;
        FClick := True;
        FSelClickStart := True;
        FClickX := X - Left + FStartTextX;
      end;
    end
    else
    begin

    end;
    Result := True;
  end;
end;
function TDxCustomEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  FSelClickEnd := False;
  if inherited MouseUp(Button, Shift, X, Y) then
  begin
    if FEnabled and not FIsHotKey and (MouseCaptureControl = self) then
    begin
      if Button = mbLeft then
      begin
        FSelEnd := -1;
        FClick := True;
        FSelClickEnd := True;
        FClickX := X - Left + FStartTextX;
      end;
    end
    else
    begin

    end;
    Result := True;
  end;
end;

constructor TDComboBox.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  DropDownList := nil;
  FShowCaret := False;
  FTransparent := False;
  FEnabled := False;
  FDropDownList := nil;
end;
function TDComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if inherited MouseDown(Button, Shift, X, Y) then
  begin
    if (not Background) and (MouseCaptureControl = nil) then
    begin
      Downed := True;
      SetDCapture(self);
    end;
    if (FDropDownList <> nil) and not FDropDownList.ChangingHero then
    begin
      FDropDownList.Left := Left;
      FDropDownList.Top := Top + Height;
      FDropDownList.Height := (FontManager.Default.TextHeight('A') + LineSpace) * (FDropDownList.Items.Count) + 2;
      FDropDownList.Visible := not FDropDownList.Visible;
    end;
    Result := True;
  end
  else if FDropDownList <> nil then
  begin
    if FDropDownList.Visible then
      FDropDownList.Visible := False;
  end;
end;
function TDComboBox.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if not Background then
  begin
    if Result then
      SetFrameHot(True)
    else if FocusedControl <> self then
      SetFrameHot(False);
  end;
end;

constructor TDx9CustomListBox.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  FSelected := -1;
  ChangingHero := False;
  FItems := TStringList.Create;
  FBackColor := clWhite;
  FSelectionColor := clSilver;
  FOnChangeSelect := nil;
  FOnMouseMoveSelect := nil;
  ParentComboBox := nil;
  FParentComboBox := nil;

end;
destructor TDx9CustomListBox.Destroy;
begin
  FItems.Free;
  inherited;
end;
function TDx9CustomListBox.GetItemSelected: Integer;
begin
  if (FSelected > FItems.count - 1) or (FSelected < 0) then
    Result := -1
  else
    Result := FSelected;

end;
procedure TDx9CustomListBox.SetItemSelected(Value: Integer);
begin
  if (Value > FItems.count - 1) or (Value < 0) then
    FSelected := -1
  else
    FSelected := Value;

end;
procedure TDx9CustomListBox.SetBackColor(Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
procedure TDx9CustomListBox.SetSelectionColor(Value: TColor);
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;
function TDx9CustomListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin

  Result := inherited MouseDown(Button, Shift, X, Y);
end;
function TDx9CustomListBox.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
var
  TmpSel: Integer;
begin

  FSelected := -1;
  Result := inherited MouseMove(Shift, X, Y);
  if Result and FEnabled and not Background then
  begin
    TmpSel := FSelected;
    if (FItems.count = 0) then
      FSelected := -1
    else
      FSelected := (-Top + Y) div (FontManager.Default.TextHeight('A') + LineSpace);
    if FSelected > FItems.count - 1 then
      FSelected := -1;
    if Assigned(FOnMouseMoveSelect) then
      FOnMouseMoveSelect(self, Shift, X, Y);
  end;
end;
function TDx9CustomListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  ret: Boolean;
  TmpSel: Integer;
begin
  ret := inherited MouseUp(Button, Shift, X, Y);
  if ret then
  begin
    TmpSel := FSelected;

    if (FItems.count = 0) then
      FSelected := -1
    else
      FSelected := (-Top + Y) div (FontManager.Default.TextHeight('A') + LineSpace);
    if FSelected > FItems.count - 1 then
      FSelected := -1;
    if FSelected <> -1 then
    begin
      if ParentComboBox <> nil then
      begin
        if ParentComboBox.Caption <> FItems[FSelected] then
        begin
          if Caption = 'SelectHeroList' then
          begin
            ChangingHero := True;
//             frmDlg.QueryChangeHero(FItems[FSelected]);
          end
          else
            ParentComboBox.Caption := FItems[FSelected];
        end;
      end;
      if Integer(FItems.Objects[FSelected]) > 0 then
        ParentComboBox.tag := Integer(FItems.Objects[FSelected]);
    end;
    if Assigned(FOnChangeSelect) then
      FOnChangeSelect(self, Button, Shift, X, Y);
    Visible := False;
    ret := True;
  end;
  Result := ret;
end;
function TDx9CustomListBox.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  ret: Boolean;
begin
  ret := inherited KeyDown(Key, Shift);
  if ret then
  begin
    case Key of
      VK_PRIOR:
        begin
          ItemSelected := ItemSelected - Height div  - FontManager.Default.TextHeight('A');
          if (ItemSelected = -1) then
            ItemSelected := 0;
        end;
      VK_NEXT:
        begin
          ItemSelected := ItemSelected + Height div  - FontManager.Default.TextHeight('A');
          if ItemSelected = -1 then
            ItemSelected := FItems.count - 1;
        end;
      VK_UP:
        if ItemSelected - 1 > -1 then
          ItemSelected := ItemSelected - 1;
      VK_DOWN:
        if ItemSelected + 1 < FItems.count then
          ItemSelected := ItemSelected + 1;
    end;

  end;
  Result := ret;
end;

procedure TDx9CustomListBox.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;
procedure TDx9CustomListBox.DirectPaint(dsurface: TCustomCanvas);
var
  fy, nY, L, T, i, oSize: Integer;
  OldColor: TColor;
begin
  if Assigned(FOnDirectPaint) then
  begin
    FOnDirectPaint(self, dsurface);
    Exit;
  end;
  L := SurfaceX(Left);
  T := SurfaceY(Top);
  try
    dsurface.FillRect(IntRectBDS(L, T, Self.Width, Self.Height), (BackColor));
    if FSelected <> -1 then
    begin
      nY := T + (FontManager.Default.TextHeight('A') + LineSpace) * FSelected;
      fy := nY + (FontManager.Default.TextHeight('A') + LineSpace);
      if (nY < T + Self.Height - 1) and (fy > T + 1) then
      begin
        if (fy > T + Self.Height - 1) then
          fy := T + Self.Height - 1;
        if (nY < T + 1) then
          nY := T + 1;
        dsurface.FillRect(IntRectBDS(L + 1, nY, Self.Width - 1, fy - nY), (SelectionColor));
      end;
    end;
    for i := 0 to FItems.count - 1 do
    begin
      if FSelected = i then
      begin
        dsurface.TextOut(L + 2, 2 + T + (FontManager.Default.TextHeight('A') + LineSpace) * i, FItems.Strings[i], clWhite);
      end
      else
      begin
        dsurface.TextOut(L + 2, 2 + T + (FontManager.Default.TextHeight('A') + LineSpace) * i, FItems.Strings[i], clBlack);
      end;
    end;
  finally
  end;
  DragModePaint(dsurface);
end;


{ TDMemo }

function TDMemo.ClearKey: Boolean;
var
  nStartY, nStopY: Integer;
  nStartX, nStopX: Integer;
  TempStr: WideString;
  i: Integer;
begin
  Result := False;
  if FLines.Count > 0 then begin
    if (FCaretX <> FSCaretX) or (FSCaretY <> FCaretY) then begin

      if FSCaretY < 0 then FSCaretY := 0;
      if FSCaretY >= FLines.Count then FSCaretY := FLines.Count - 1;
      if FCaretY < 0 then FCaretY := 0;
      if FCaretY >= FLines.Count then FCaretY := FLines.Count - 1;

      if FSCaretY = FCaretY then begin
        if FSCaretX > FCaretX then begin
          nStartX := FCaretX;
          nStopX := FSCaretX;
        end else begin
          nStartX := FSCaretX;
          nStopX := FCaretX;
        end;
        TempStr := TDMemoStringList(FLines).Str[FCaretY];
        Delete(TempStr, nStartX + 1, nStopX - nStartX);
        TDMemoStringList(FLines).Str[FCaretY] := TempStr;
        RefListWidth(FCaretY, 0);
        FCaretX := nStartX;
        SetCaret(True);
        Result := True;
      end else begin
        if FSCaretY > FCaretY then begin
          nStartY := FCaretY;
          nStopY := FSCaretY;
          nStartX := FCaretX;
          nStopX := FSCaretX;
        end else begin
          nStartY := FSCaretY;
          nStopY := FCaretY;
          nStartX := FSCaretX;
          nStopX := FCaretX;
        end;
        TempStr := TDMemoStringList(FLines).Str[nStartY];
        Delete(TempStr, nStartX + 1, 255);
        TDMemoStringList(FLines).Str[nStartY] := TempStr;

        TempStr := TDMemoStringList(FLines).Str[nStopY];
        Delete(TempStr, 1, nStopX);
        TDMemoStringList(FLines).Str[nStartY] := TDMemoStringList(FLines).Str[nStartY] + TempStr;
        FLines.Objects[nStartY] := FLines.Objects[nStopY];
        FLines.Delete(nStopY);
        if (nStopY - nStartY) > 1 then
          for i := nStopY - 1 downto nStartY + 1 do
            FLines.Delete(i);
        RefListWidth(nStartY, nStartX);
        SetCaret(True);
        Result := True;
      end;
    end;
  end;
end;

constructor TDMemo.Create(AOwner: TComponent);
begin
  inherited;
  FCaretShowTime := GetTickCount;

  Downed := False;
  KeyDowned := False;

  FTopIndex := 0;
  FCaretY := 0;
  FCaretX := 0;

  FSCaretX := 0;
  FSCaretY := 0;

  FInputStr := '';
  bDoubleByte := False;
  KeyByteCount := 0;

  FTransparent := False;

  FMaxLength := 0;

  FOnChange := nil;
  FReadOnly := False;
  FFrameColor := clBlack;
  Color := clBlack;

  FLines := TDMemoStringList.Create;
  TDMemoStringList(FLines).DMemo := Self;

  Font.Color := clWhite;
  Canvas.Font.Name := Font.Name;
  Canvas.Font.Color := Font.Color;
  Canvas.Font.Size := Font.Size;

  FMoveTick := GetTickCount;
end;

destructor TDMemo.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TDMemo.DirectPaint(dsurface: TCustomCanvas);
var
  dc: TIntRect;
  nShowCount, i: Integer;
  ax, ay: Integer;
  TempStr: string;
  nStartY, nStopY: Integer;
  nStartX, nStopX: Integer;
  addax: Integer;
begin
  dc.Left := SurfaceX(Left);
  dc.Top := SurfaceY(Top);
  dc.Right := SurfaceX(left + Width);
  dc.Bottom := SurfaceY(top + Height);
  with dsurface do begin
    if not FTransparent then begin
      Brush.Color := Color;
      FillRect(IntRectBDS(dc.Left, dc.Top, dc.Right, dc.Bottom),Brush.Color);
    end;
  end;

  if (GetTickCount - FCaretShowTime) > 500 then begin //光标闪动间隔时间
    FCaretShowTime := GetTickCount;
    FCaretShow := not FCaretShow;
  end;

  nShowCount := (Height - 2) div 14;
  if (FTopIndex + nShowCount - 1) > Lines.Count then begin
    FTopIndex := Max(Lines.Count - nShowCount + 1, 0);
  end;
  if (FCaretY >= Lines.Count) then FCaretY := Max(Lines.Count - 1, 0);
  if FCaretY < 0 then begin
    FTopIndex := 0;
    FCaretY := 0;
  end;

  if FSCaretY > FCaretY then begin
    nStartY := FCaretY;
    nStopY := FSCaretY;
    nStartX := FCaretX;
    nStopX := FSCaretX;
  end else begin
    nStartY := FSCaretY;
    nStopY := FCaretY;
    nStartX := FSCaretX;
    nStopX := FCaretX;
  end;
  if FSCaretY = FCaretY then begin
    if FSCaretX > FCaretX then begin
      nStartX := FCaretX;
      nStopX := FSCaretX;
    end else if FSCaretX < FCaretX then begin
      nStartX := FSCaretX;
      nStopX := FCaretX;
    end else begin
      nStartX := -1;
    end;
  end;
  ax := SurfaceX(Left) + 2;
  ay := SurfaceY(Top) + 2;
  with dsurface do begin
    Font.Color := Self.Font.Color;
    for i := FTopIndex to (FTopIndex + nShowCount - 1) do begin
      if i >= Lines.Count then Break;
      if nStartY <> nStopY then begin
        if i = nStartY then begin
          TempStr := Copy(WideString(Lines[i]), 1, nStartX);
          TextOut(ax, ay + (i - FTopIndex) * 14, TempStr,Font.Color);
          addax := FontManager.Default.TextWidth(TempStr);
          TempStr := Copy(WideString(Lines[i]), nStartX + 1, 255);
          FillRect(ax + addax, ay + (i - FTopIndex) * 14 - 1, FontManager.Default.TextWidth(TempStr), 16, $C9C66931);
          TextOut(ax + addax, ay + (i - FTopIndex) * 14,TempStr, clWhite);
        end else if i = nStopY then begin
          TempStr := Copy(WideString(Lines[i]), 1, nStopX);
          addax := FontManager.Default.TextWidth(TempStr);
          FillRect(ax, ay + (i - FTopIndex) * 14 - 1, addax, 16, $C9C66931);
          TextOut(ax, ay + (i - FTopIndex) * 14, TempStr,clWhite);
          TempStr := Copy(WideString(Lines[i]), nStopX + 1, 255);
          TextOut(ax + addax, ay + (i - FTopIndex) * 14, TempStr,clWhite);
        end else if (i > nStartY) and (i < nStopY) then begin
          FillRect(ax, ay + (i - FTopIndex) * 14 - 1, FontManager.Default.TextWidth(Lines[i]), 16, $C9C66931);
          TextOut(ax, ay + (i - FTopIndex) * 14,Lines[i], clWhite);
        end else
          TextOut(ax, ay + (i - FTopIndex) * 14, Lines[i],clWhite);
      end else begin
        if (nStartX <> -1) and (i = FSCaretY) then begin
          TempStr := Copy(WideString(Lines[i]), 1, nStartX);
          TextOut(ax, ay + (i - FTopIndex) * 14, TempStr,clWhite);
          addax := FontManager.Default.TextWidth(TempStr);
          TempStr := Copy(WideString(Lines[i]), nStartX + 1, nStopX - nStartX);
          FillRect(ax + addax, ay + (i - FTopIndex) * 14 - 1, FontManager.Default.TextWidth(TempStr), 16, $C9C66931);
          TextOut(ax + addax, ay + (i - FTopIndex) * 14,TempStr, clWhite);
          addax := addax + FontManager.Default.TextWidth(TempStr);
          TempStr := Copy(WideString(Lines[i]), nStopX + 1, 255);
          TextOut(ax + addax, ay + (i - FTopIndex) * 14,TempStr, clWhite);
        end else
          TextOut(ax, ay + (i - FTopIndex) * 14, Lines[i],clWhite);
      end;
    end;
    if (FCaretY >= FTopIndex) and (FCaretY < (FTopIndex + nShowCount)) then begin
      ay := ay + (Max(FCaretY - FTopIndex, 0)) * 14;
      if FCaretY < Lines.Count then begin
        TempStr := LeftStr(WideString(Lines[FCaretY]), FCaretX);
        ax := ax + FontManager.Default.TextWidth(TempStr);
      end;
      if FCaretShow and (FocusedControl = Self) then begin //光标
        FrameRect(IntRectBDS(ax, ay, ax + 2, ay + 12),Self.Font.Color);
      end;
    end;
  end;
  for i := 0 to DControls.Count - 1 do
    if TDControl(DControls[i]).Visible then
      TDControl(DControls[i]).DirectPaint(dsurface);
  DragModePaint(dsurface);
end;

function TDMemo.GetKey: string;
var
  nStartY, nStopY: Integer;
  nStartX, nStopX: Integer;
  TempStr: WideString;
  i: Integer;
begin
  Result := '';
  if FLines.Count > 0 then begin
    if (FCaretX <> FSCaretX) or (FSCaretY <> FCaretY) then begin
      if FSCaretY < 0 then FSCaretY := 0;
      if FSCaretY >= FLines.Count then FSCaretY := FLines.Count - 1;
      if FCaretY < 0 then FCaretY := 0;
      if FCaretY >= FLines.Count then FCaretY := FLines.Count - 1;

      if FSCaretY = FCaretY then begin
        if FSCaretX > FCaretX then begin
          nStartX := FCaretX;
          nStopX := FSCaretX;
        end else begin
          nStartX := FSCaretX;
          nStopX := FCaretX;
        end;
        TempStr := FLines[FCaretY];
        Result := Copy(TempStr, nStartX + 1, nStopX - nStartX);
      end else begin
        if FSCaretY > FCaretY then begin
          nStartY := FCaretY;
          nStopY := FSCaretY;
          nStartX := FCaretX;
          nStopX := FSCaretX;
        end else begin
          nStartY := FSCaretY;
          nStopY := FCaretY;
          nStartX := FSCaretX;
          nStopX := FCaretX;
        end;
        TempStr := FLines[nStartY];
        Result := Copy(TempStr, nStartX + 1, 255);
        if Integer(FLines.Objects[nStartY]) = 13 then Result := Result + #13#10;
        if (nStopY - nStartY) > 1 then
          for i := nStartY + 1 to nStopY - 1 do begin
            Result := Result + FLines[i];
            if Integer(FLines.Objects[i]) = 13 then Result := Result + #13#10;
          end;
        TempStr := FLines[nStopY];
        Result := Result + Copy(TempStr, 1, nStopX);
        if Integer(FLines.Objects[nStopY]) = 13 then Result := Result + #13#10;
      end;
    end;
  end;
end;

procedure TDMemo.KeyCaret(Key: Word);
var
  TempStr: WideString;
  nShowCount: Integer;
begin
  if FLines.Count > 0 then begin
    if FCaretY < 0 then FCaretY := 0;
    if FCaretY >= FLines.Count then FCaretY := FLines.Count - 1;
    TempStr := TDMemoStringList(FLines).Str[FCaretY];
    case Key of
      VK_UP: begin
          if FCaretY > 0 then Dec(FCaretY);
        end;
      VK_DOWN: begin
          if FCaretY < (FLines.Count - 1) then Inc(FCaretY);
        end;
      VK_RIGHT: begin
          if FCaretX < Length(TempStr) then Inc(FCaretX)
          else begin
            if FCaretY < (FLines.Count - 1) then begin
              Inc(FCaretY);
              FCaretX := 0;
            end;
          end;
        end;
      VK_LEFT: begin
          if FCaretX > 0 then Dec(FCaretX)
          else begin
            if FCaretY > 0 then begin
              Dec(FCaretY);
              FCaretX := Length(WideString(TDMemoStringList(FLines).Str[FCaretY]));
            end;
          end;
        end;
    end;
    nShowCount := (Height - 2) div 14;
    if FCaretY < FTopIndex then FTopIndex := FCaretY
    else begin
      if (FCaretY - FTopIndex) >= nShowCount then begin
        FTopIndex := Max(FCaretY - nShowCount + 1, 0);
      end;
    end;

    if not KeyDowned then SetCaret(False);
  end;
end;

function TDMemo.KeyDown(var Key: Word; Shift: TShiftState): Boolean;
var
  Clipboard: TClipboard;
  AddTx, Data: string;
  boAdd: Boolean;
  TempStr: WideString;
begin
  Result := FALSE;

  if (FocusedControl = self) then begin
    if Assigned(FOnKeyDown) then
      FOnKeyDown(self, Key, Shift);
    if Key = 0 then exit;
    if (ssCtrl in Shift) and (not Downed) and (Key = Word('A')) then begin
      if FLines.Count > 0 then begin
        FCaretY := FLines.Count - 1;
        FCaretX := Length(WideString(TDMemoStringList(FLines).Str[FSCaretY]));
        SetCaret(True);
        FSCaretX := 0;
        FSCaretY := 0;
      end;
      Key := 0;
      Result := True;
      Exit;
    end
    else if (ssCtrl in Shift) and (not Downed) and (Key = Word('X')) then begin
      AddTx := GetKey;
      if AddTx <> '' then begin
        Clipboard := TClipboard.Create;
        Clipboard.AsText := AddTx;
        Clipboard.Free;
        ClearKey();
        TextChange();
      end;
      Key := 0;
      Result := True;
      Exit;
    end else if (ssCtrl in Shift) and (not Downed) and (Key = Word('C')) then begin
      AddTx := GetKey;
      if AddTx <> '' then begin
        Clipboard := TClipboard.Create;
        Clipboard.AsText := AddTx;
        Clipboard.Free;
      end;
      Key := 0;
      Result := True;
      Exit;
    end else if (ssCtrl in Shift) and (not Downed) and (Key = Word('V')) then begin
      ClearKey();
      Clipboard := TClipboard.Create;
      AddTx := Clipboard.AsText;
      boAdd := False;
      while True do begin
        if AddTx = '' then
          break;
        AddTx := GetValidStr3(AddTx, data, [#13]);
        if Data <> '' then begin
          data := AnsiReplaceText(data, #10, '');
          if Data = '' then
            Data := #9;
          if FLines.Count <= 0 then begin
            FLines.AddObject(Data, TObject(13));
            FCaretY := 0;
            RefListWidth(FCaretY, -1);
          end
          else if boAdd then begin
            Inc(FCaretY);
            FLines.InsertObject(FCaretY, Data, TObject(13));
            FCaretX := 0;
            RefListWidth(FCaretY, -1);
          end
          else begin
            TempStr := TDMemoStringList(FLines).Str[FCaretY];
            Insert(Data, TempStr, FCaretX + 1);
            TDMemoStringList(FLines).Str[FCaretY] := TempStr;
            Inc(FCaretX, Length(WideString(Data)));
            FLines.Objects[FCaretY] := TObject(13);
            RefListWidth(FCaretY, FCaretX);
          end;

          boAdd := True;
        end;
      end;
      Clipboard.Free;
      Key := 0;
      Result := True;
      Exit;
    end else if (ssShift in Shift) and (not Downed) then begin
      KeyDowned := True;
    end
    else
      KeyDowned := False;
    if FLines.Count <= 0 then exit;
    case Key of
      VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT: begin
        KeyCaret(Key);
        Key := 0;
        Result := TRUE;
      end;
      VK_BACK: begin
          if (not FReadOnly) then begin
            if not ClearKey then begin
              while True do begin
                TempStr := TDMemoStringList(FLines).Str[FCaretY];
                if FCaretX > 0 then begin
                  Delete(TempStr, FCaretX, 1);
                  if TempStr = '' then begin
                    FLines.Delete(FCaretY);
                    if FCaretY > 0 then begin
                      Dec(FCaretY);
                      FCaretX :=
                        Length(WideString(TDMemoStringList(FLines).Str[FCaretY]));
                      SetCaret(True);
                    end else begin
                      FCaretY := 0;
                      FCaretX := 0;
                      SetCaret(False);
                    end;
                    Exit;
                  end
                  else begin
                    TDMemoStringList(FLines).Str[FCaretY] := TempStr;
                    Dec(FCaretX);
                  end;
                  break;
                end
                else if FCaretX = 0 then begin
                  if FCaretY > 0 then begin
                    if Integer(FLines.Objects[FCaretY - 1]) = 13 then begin
                      FLines.Objects[FCaretY - 1] := nil;
                      Break;
                    end
                    else begin
                      FLines.Objects[FCaretY - 1] := FLines.Objects[FCaretY];
                      FCaretX :=
                        Length(WideString(TDMemoStringList(FLines).Str[FCaretY -
                        1]));
                      TDMemoStringList(FLines).Str[FCaretY - 1] :=
                        TDMemoStringList(FLines).Str[FCaretY - 1] +
                        TDMemoStringList(FLines).Str[FCaretY];
                      FLines.Delete(FCaretY);
                      Dec(FCaretY);
                    end;
                  end
                  else
                    Break;
                end
                else
                  Break;
              end;
              RefListWidth(FCaretY, FCaretX);
              SetCaret(True);
            end;
            TextChange();
          end;
          Key := 0;
          Result := TRUE;
      end;
      VK_DELETE: begin
          if (not FReadOnly) then begin
            if not ClearKey then begin
              while True do begin
                TempStr := TDMemoStringList(FLines).Str[FCaretY];
                if Length(TempStr) > FCaretX then begin
                  Delete(TempStr, FCaretX + 1, 1);
                  if TempStr = '' then begin
                    FLines.Delete(FCaretY);
                    if FCaretY > 0 then begin
                      Dec(FCaretY);
                      FCaretX :=
                        Length(WideString(TDMemoStringList(FLines).Str[FCaretY]));
                      SetCaret(True);
                    end
                    else begin
                      FCaretY := 0;
                      FCaretX := 0;
                      SetCaret(False);
                    end;
                    Exit;
                  end
                  else
                    TDMemoStringList(FLines).Str[FCaretY] := TempStr;
                  break;
                end
                else if Integer(FLines.Objects[FCaretY]) = 13 then begin
                  FLines.Objects[FCaretY] := nil;
                  break;
                end
                else begin
                  if (FCaretY + 1) < FLines.Count then begin
                    TDMemoStringList(FLines).Str[FCaretY] :=
                      TDMemoStringList(FLines).Str[FCaretY] +
                      TDMemoStringList(FLines).Str[FCaretY + 1];
                    FLines.Objects[FCaretY] := FLines.Objects[FCaretY + 1];
                    FLines.Delete(FCaretY + 1);
                  end
                  else
                    Break;
                end;
              end;
              RefListWidth(FCaretY, FCaretX);
              SetCaret(True);
            end;
            TextChange();
          end;
          Key := 0;
          Result := TRUE;
      end;
    end;
  end;
end;

function TDMemo.KeyPress(var Key: Char): Boolean;
var
  TempStr, Temp: WideString;
  OldObject: Integer;
begin
  Result := False;
  if (FocusedControl = Self) then begin
    Result := TRUE;
    if (not Downed) and (not FReadOnly) then begin
      if Assigned(FOnKeyPress) then
        FOnKeyPress(self, Key);
      if Key = #0 then Exit;

      if (FCaretY >= Lines.Count) then
        FCaretY := Max(Lines.Count - 1, 0);
      if FCaretY < 0 then begin
        FTopIndex := 0;
        FCaretY := 0;
      end;
      if Key = #13 then begin
        if FLines.Count <= 0 then begin
          FLines.AddObject('', TObject(13));
          FLines.AddObject('', TObject(13));
          FCaretX := 0;
          FCaretY := 1;
          SetCaret(True);
        end
        else begin
          Temp := TDMemoStringList(FLines).Str[FCaretY];
          OldObject := Integer(FLines.Objects[FCaretY]);

          TempStr := Copy(Temp, 1, FCaretX);
          TDMemoStringList(FLines).Str[FCaretY] := TempStr;
          FLines.Objects[FCaretY] := TObject(13);

          TempStr := Copy(Temp, FCaretX + 1, 255);
          if TempStr <> '' then begin
            FLines.InsertObject(FCaretY + 1, TempStr, TObject(OldObject));
          end else begin
            FLines.InsertObject(FCaretY + 1, '', TObject(OldObject));
          end;
          RefListWidth(FCaretY + 1, 0);
          FCaretY := FCaretY + 1;
          FCaretX := 0;
          SetCaret(True);
        end;
        Exit;
      end;

      if CharInSet(key ,AllowedChars) then begin
        if IsDBCSLeadByte(Ord(Key)) or bDoubleByte then begin
          bDoubleByte := true;
          Inc(KeyByteCount);
          FInputStr := FInputStr + key;
        end;
        if not bDoubleByte then begin
          ClearKey;
          if (MaxLength > 0) and (Length(strpas(FLines.GetText)) >= MaxLength) then begin
            Key := #0;
            exit;
          end;
          if FLines.Count <= 0 then begin
            FLines.AddObject(Key, nil);
            FCaretX := 1;
            FCaretY := 0;
          end else begin
            TempStr := TDMemoStringList(FLines).Str[FCaretY];
            Insert(Key, TempStr, FCaretX + 1);
            TDMemoStringList(FLines).Str[FCaretY] := TempStr;
            Inc(FCaretX);
            RefListWidth(FCaretY, FCaretX);
          end;
          SetCaret(True);
          Key := #0;
          TextChange();
        end else if KeyByteCount >= 2 then begin
          if length(FInputStr) <> 2 then begin
            bDoubleByte := false;
            KeyByteCount := 0;
            FInputStr := '';
            Key := #0;
            exit;
          end;

          ClearKey;
          if (MaxLength > 0) and (Length(string(FLines.GetText)) >= (MaxLength - 1)) then begin
            bDoubleByte := false;
            KeyByteCount := 0;
            FInputStr := '';
            Key := #0;
            exit;
          end;
          if FLines.Count <= 0 then begin
            FLines.AddObject(FInputStr, nil);
            FCaretX := 1;
            FCaretY := 0;
          end else begin
            TempStr := TDMemoStringList(FLines).Str[FCaretY];
            Insert(FInputStr, TempStr, FCaretX + 1);
            TDMemoStringList(FLines).Str[FCaretY] := TempStr;
            Inc(FCaretX);
            RefListWidth(FCaretY, FCaretX);
          end;
          SetCaret(True);
          bDoubleByte := false;
          KeyByteCount := 0;
          FInputStr := '';
          Key := #0;
          TextChange();
        end;
      end;
    end;
  end;
  Key := #0;
end;

function TDMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := FALSE;
  if inherited MouseDown(Button, Shift, X, Y) then begin
    if (not Background) and (MouseCaptureControl = nil) then begin
      KeyDowned := False;
      if (FocusedControl = self) then begin
        DownCaret(X - left, Y - top);
      end;
      SetCaret(False);
      Downed := True;
      SetDCapture(self);
    end;
    Result := TRUE;
  end;
end;

function TDMemo.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if Result and (MouseCaptureControl = self) then begin
    if Downed and (not KeyDowned) then
      MoveCaret(X - left, Y - top);
  end;
end;

function TDMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := FALSE;
  Downed := False;
  if inherited MouseUp(Button, Shift, X, Y) then begin
    ReleaseDCapture;
    if not Background then begin
      if InRange(X, Y,Shift) then begin
        if Assigned(FOnClick) then
          FOnClick(self, X, Y);
      end;
    end;
    Result := TRUE;
    exit;
  end
  else begin
    ReleaseDCapture;
  end;
end;

procedure TDMemo.MoveCaret(X, Y: Integer);
var
  tempstrw: WideString;
  nShowCount, i: Integer;
  tempstr: string;
begin
  nShowCount := (Height - 2) div 14;
  if Y < 0 then begin //往上移动
    if (GetTickCount - FMoveTick) < 50 then Exit;
    if FTopIndex > 0 then Dec(FTopIndex);
    FCaretY := FTopIndex;
  end else if Y > Height then begin //往下移动
    if (GetTickCount - FMoveTick) < 50 then Exit;
    Inc(FCaretY);
    if FCaretY >= FLines.Count then FCaretY := Max(FLines.Count - 1, 0);
    FTopIndex := Max(FCaretY - nShowCount + 1, 0);
  end else FCaretY := (y - 2) div 14 + FTopIndex;
  FMoveTick := GetTickCount;

  if FCaretY >= FLines.Count then FCaretY := Max(FLines.Count - 1, 0);
  FCaretX := 0;
  if FCaretY < FLines.Count then begin
    tempstrw := TDMemoStringList(FLines).Str[FCaretY];
    if tempstrw <> '' then begin
      for i := 1 to Length(tempstrw) do begin
        tempstr := Copy(tempstrw, 1, i);
        if (FontManager.Default.TextWidth(tempstr)) > (X) then Exit;
        FCaretX := i;
      end;
    end;
  end;
end;

procedure TDMemo.RefListWidth(ItemIndex: Integer; nCaret: Integer);
var
  i, Fi, nIndex, nY: Integer;
  TempStr, AddStr: WideString;
begin
  TempStr := '';
  nIndex := 0;
  while True do begin
    if ItemIndex >= FLines.Count then Break;
    TempStr := TempStr + TDMemoStringList(FLines).Str[ItemIndex];
    nIndex := Integer(Lines.Objects[ItemIndex]);
    FLines.Delete(ItemIndex);
    if nIndex = 13 then Break;
  end;
  if TempStr <> '' then begin
    AddStr := '';
    Fi := 1;
    nY := ItemIndex;
    for i := 1 to Length(TempStr) + 1 do begin
      AddStr := Copy(TempStr, Fi, i - Fi + 1);
      if FontManager.Default.TextWidth(AddStr) > (Width - 20) then begin
        AddStr := Copy(TempStr, Fi, i - Fi);
        Fi := i;
        FLines.InsertObject(ItemIndex, AddStr, nil);
        nIndex := ItemIndex;
        Inc(ItemIndex);
        nY := ItemIndex;
        AddStr := '';
      end;
      if i = nCaret then begin
        FCaretX := i - Fi + 1;
        FCaretY := nY;
        SetCaret(True);
      end;
    end;
    if AddStr <> '' then begin
      FLines.InsertObject(ItemIndex, AddStr, TObject(13));
      nIndex := ItemIndex;
    end
    else begin
      FLines.Objects[nIndex] := TObject(13);
    end;
    if nCaret = -1 then begin
      FCaretY := nIndex;
      FCaretX := Length(WideString(TDMemoStringList(FLines).Str[nIndex]));
      SetCaret(True);
    end;
  end;
  if FCaretY >= FLines.Count then begin
    FCaretY := Max(FLines.Count - 1, 0);
    SetCaret(True);
  end;
end;

procedure TDMemo.DownCaret(X, Y: Integer);
var
  tempstrw: WideString;
  i: Integer;
  tempstr: string;
begin
  FCaretY := (y - 2) div 14 + FTopIndex;
  if FCaretY >= FLines.Count then FCaretY := Max(FLines.Count - 1, 0);
  FCaretX := 0;
  if FCaretY < FLines.Count then begin
    tempstrw := TDMemoStringList(FLines).Str[FCaretY];
    if tempstrw <> '' then begin
      for i := 1 to Length(tempstrw) do begin
        tempstr := Copy(tempstrw, 1, i);
        if (FontManager.Default.TextWidth(tempstr)) > (X) then Exit;
        FCaretX := i;
      end;
    end;
  end;
end;

procedure TDMemo.SetCaret(boBottom: Boolean);
var
  nShowCount: Integer;
begin
  FSCaretX := FCaretX;
  FSCaretY := FCaretY;
  if boBottom then begin
    nShowCount := (Height - 2) div 14;
    if FCaretY < FTopIndex then FTopIndex := FCaretY
    else begin
      if (FCaretY - FTopIndex) >= nShowCount then begin
        FTopIndex := Max(FCaretY - nShowCount + 1, 0);
      end;
    end;
  end;
end;

procedure TDMemo.SetCaretY(const Value: Integer);
begin
  FCaretY := Value;
  if FCaretY >= FLines.Count then FCaretY := Max(FLines.Count - 1, 0);
  if FCaretY < 0 then FCaretY := 0;
  SetCaret(True);
end;

procedure TDMemo.SetFocus;
begin
  SetDFocus (self);
end;

procedure TDMemo.TextChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;


{ TDMemoStringList }

function TDMemoStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, TObject(13));
  DMemo.RefListWidth(Result, -1);
end;

function TDMemoStringList.AddObject(const S: string; AObject: TObject): Integer;
var
  AddStr: string;
begin
  AddStr := AnsiReplaceText(S, #13, '');
  AddStr := AnsiReplaceText(AddStr, #10, '');
  if AddStr = '' then begin
    Result := inherited AddObject(#9, AObject);
  end else
    Result := inherited AddObject(AddStr, AObject);
end;

procedure TDMemoStringList.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  DMemo.FCaretY := 0;
  DMemo.FCaretX := 0;
  DMemo.SetCaret(False);
end;

procedure TDMemoStringList.Clear;
begin
  inherited;
  DMemo.FCaretY := 0;
  DMemo.FCaretX := 0;
  DMemo.SetCaret(False);
end;

function TDMemoStringList.Get(Index: Integer): string;
begin
  Result := inherited Get(Index);
  Result := AnsiReplaceText(Result, #9, '');
end;

procedure TDMemoStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
var
  AddStr: string;
begin
  AddStr := AnsiReplaceText(S, #13, '');
  AddStr := AnsiReplaceText(AddStr, #10, '');
  if AddStr = '' then begin
    inherited InsertObject(Index, #9, AObject);
  end
  else
    inherited InsertObject(Index, AddStr, AObject);
end;

function TDMemoStringList.GetText: PChar;
var
  i: Integer;
  AddStr: string;
begin
  AddStr := '';
  for i := 0 to Count - 1 do begin
    AddStr := AddStr + Get(i);
    if Char(Objects[i]) = #13 then begin
      AddStr := AddStr + #13;
    end;
  end;
  Result := StrNew(PChar(AddStr));
end;

procedure TDMemoStringList.SaveToFile(const FileName: string);
var
  TempString: TStringList;
  i: Integer;
  AddStr: string;
begin
  TempString := TStringList.Create;
  try
    AddStr := '';
    for i := 0 to Count - 1 do begin
      AddStr := AddStr + Get(i);
      if Char(Objects[i]) = #13 then begin
        TempString.Add(AddStr);
        AddStr := '';
      end;
    end;
    if AddStr <> '' then
      TempString.Add(AddStr);

    TempString.SaveToFile(FileName);
  finally
    TempString.Free;
  end;
end;

procedure TDMemoStringList.LoadFromFile(const FileName: string);
var
  TempString: TStringList;
  i: Integer;
begin
  Clear;
  TempString := TStringList.Create;
  try
    if FileExists(Filename) then begin
      TempString.LoadFromFile(FileName);
      for i := 0 to TempString.Count - 1 do begin
        Add(TempString[i]);
      end;
    end;
  finally
    TempString.Free;
  end;
end;

procedure TDMemoStringList.Put(Index: Integer; const Value: string);
var
  AddStr: string;
begin
  if Value <> '' then begin
    AddStr := AnsiReplaceText(Value, #13, '');
    AddStr := AnsiReplaceText(AddStr, #10, '');
  end else
    AddStr := #9;
  inherited Put(Index, AddStr);
end;

function TDMemoStringList.SelfGet(Index: Integer): string;
begin
  Result := inherited Get(Index);
end;

{ TDLabel }

{ TDLabel }


constructor TDLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Downed := FALSE;
  FOnClick := nil;

  OldText := '';
  Text := '';
  FClickSound := csNone;
  FAutoSize := True;
  FAlignment := taLeftJustify;

end;

function TDLabel.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseMove(Shift, X, Y);
  if (not Background) and (not Result) then begin
    Result := inherited MouseMove(Shift, X, Y);
    if MouseCaptureControl = self then begin
      if InRange(X, Y,Shift) then
        Downed := TRUE
      else
        Downed := FALSE;
    end;
  end;
end;

procedure TDLabel.DirectPaint(dsurface: TCustomCanvas);
var
  d: TCustomLockableTexture;
  i, ImgWidth, ImgHeight: integer;
begin
  ImgWidth := 0;
  ImgHeight := 0;
  if Assigned(FOnDirectPaint) then begin
    FOnDirectPaint(self, dsurface)
  end else begin
    if WLib <> nil then begin
      d := WLib.Images[FaceIndex];
      if d <> nil then begin
        ImgWidth := d.Width;
        ImgHeight := d.Height;
        dsurface.Draw(SurfaceX(Left), SurfaceY(Top), d.ClientRect, d, True);
      end;
    end;
//    if FTransparent then
//    dsurface.FillRect(IntRect(SurfaceX(Left-4), SurfaceY(Top-4), Width+8, Height+8),NewColorW(Color));
//   if FFrameparent then
//    dsurface.FrameRect(IntRect(SurfaceX(Left-4), SurfaceY(Top-4), Width+8, Height+8), FFrameColor);



    if Text <> '' then begin
      if FAutoSize and (OldText <> Text) then begin  //改变大小
        Width := FontManager.Default.TextWidth(Text) + 2 + ImgWidth;
        Height := FontManager.Default.TextHeight('0') + 2 + ImgHeight;
        OldText := Text;
      end;
      with dsurface do begin
        case FAlignment of //字符串位置
          taLeftJustify: TextOut(SurfaceX(Left) - ImgWidth, SurfaceY(Top) + (Height - FontManager.Default.TextHeight(Text)) div 2, Text, Self.Font.Color);
          taCenter: TextOut(SurfaceX(Left)- ImgWidth + (Width - FontManager.Default.TextWidth(Text)) div 2, SurfaceY(Top) + (Height - FontManager.Default.TextHeight(Text)) div 2, Text, Self.Font.Color);
          taRightJustify: TextOut(SurfaceX(Left)- ImgWidth + (Width - FontManager.Default.TextWidth(Text)), SurfaceY(Top) + (Height - FontManager.Default.TextHeight(Text)) div 2, Text, Self.Font.Color);
        end;
      end;
    end;
  end;

  for i := 0 to DControls.Count - 1 do
    if TDControl(DControls[i]).Visible then
      TDControl(DControls[i]).DirectPaint(dsurface);
  if Assigned(FOnDirectPaintEnd) then
    FOnDirectPaintEnd(self, dsurface);

  DragModePaint(dsurface);
end;

function TDLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := FALSE;
  if inherited MouseDown(Button, Shift, X, Y) and FEnableFocus then begin
    if (not Background) and (MouseCaptureControl = nil) then begin
      Downed := TRUE;
      SetDCapture(self);
    end;
    Result := TRUE;
  end;
end;

function TDLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer): Boolean;
begin
  Result := FALSE;
  if inherited MouseUp(Button, Shift, X, Y) and FEnableFocus then begin
    ReleaseDCapture;
    if not Background then begin
      if Downed and InRange(X, Y,Shift) then begin
        if Assigned(FOnClickSound) then
          FOnClickSound(self, FClickSound);
        if Assigned(FOnClick) then
          FOnClick(self, X, Y);
      end;
    end;
    Downed := FALSE;
    Result := TRUE;
    exit;
  end
  else begin
    ReleaseDCapture;
    Downed := FALSE;
  end;
end;

procedure TDLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
  end;
end;

procedure TDLabel.SetText(str: string);
begin
  FText := str;
end;

procedure TDLabel.SetCaptionChaged(Value: Boolean);
begin
  FAutoSize := Value;
  if not (csDesigning in ComponentState) then begin
    if Text <> '' then begin
      if FAutoSize then begin
        Width := FontManager.Default.TextWidth(Text) + 2;
        Height := FontManager.Default.TextHeight('0') + 2;
      end;
    end;
  end;
end;
{ TChrImageInfo }
procedure TJobImageInfo.SetAni(const Index, Value: Integer);
begin
  case Index of
   0: FAniIndex    := Value;
   1: FAniTick    := Value;
   2: FEffectTick    := Value;
   3: FEffectIndex    := Value;
  end;
end;

procedure TJobImageInfo.SetFreezeParam(const Index, Value: Integer);
begin
  case Index of
   0: FFreezeCount   := Value;
   1: FFreezeTick   := Value;
   2: FFreezeIndex   := Value;
   3: FFreezesex   := Value;
  end;
end;

procedure TJobImageInfo.SetFreezingParam(const Index, Value: Integer);
begin
  case Index of
   0: FFreezingCount  := Value;
   1: FFreezingTick  := Value;
   2: FFreezingIndex  := Value;
   3: FFreezingsex  := Value;
  end;
end;


procedure TJobImageInfo.SetStateParam(const Index, Value: Integer);
begin
  case Index of
   0: FDarkLevelTick := Value;
   1: fStateIndex1 := Value;
   2: fStateIndex2 := Value;
   3: fStatesex := Value;
  end;
end;


{ TDXSelChr }

constructor TDXSelChr.Create(AOwner: TComponent);
begin
  inherited Create(aowner);
  FJobWarriorIndex := TJobImageInfo.Create; //战士职业图片索引
  FJobWizardIndex := TJobImageInfo.Create; //法师职业图片索引
  FJobTaoistIndex := TJobImageInfo.Create; //道士职业图片索引
end;
destructor TDXSelChr.Destroy;
begin
    FJobWarriorIndex.Free;
    FJobWizardIndex.Free;
    FJobTaoistIndex.Free;
   inherited;
end;

initialization

g_DWinMan := TDWinManager.Create;

finalization

FreeAndNil(g_DWinMan);


end.





{*************************************************************************}
{ THTMLButtons components                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{    TMS Software                                                         }
{    copyright © 1999-2014                                                }
{    Email : info@tmssoftware.com                                         }
{    Web : http://www.tmssoftware.com                                     }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit htmlbtns;

{$I TMSDEFS.INC}
{$R htmlbtns.res}
{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, Buttons, ComObj, ActiveX,
  PictureContainer, ImgList, BtnXPVS , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.4.1.0  : added BorderColor to HTMLButton (for flat mode)
  // 1.4.5.0  : added Transparent property
  // 1.4.5.1  : added HTMLCheckGroup.ReadOnly[] property
  // 1.4.5.2  : fixed issue with ShadowColor, ShadowOffset initialization for HTMLRadioGroup, HTMLCheckGroup
  // 1.5.0.0  : New: Added btTheme button type
  // 1.5.1.0  : Improved transparent drawing for HTMLCheckBox, HTMLRadioButton
  // 1.5.1.1  : Improved HTMLRadioButton checked state activation on mouseup
  // 1.5.1.2  : Fixed issue with dbl OnClick event on radiobutton click
  // 1.5.2.0  : Exposed OnAnchorClick, OnAnchorEnter, OnAnchorExit in HTMLRadioGroup, HTMLCheckGroup
  // 1.5.2.1  : Fixed compatibility issue with TRadioGroup of THTMLRadioGroup
  // 1.5.2.2  : Fixed issue with MouseUp handling in THTMLCheckBox, THTMLRadioButton
  // 1.5.3.0  : New : support themed cbGray state in TCustomHTMLCheckBox.Paint
  // 1.5.3.1  : Fixed : issue with repainting for ClearType fonts
  // 1.5.4.0  : New : support for customizing bullets in HTML UL lists
  // 1.5.5.0  : New : DisabledColor/DisabledShadow added in THTMLCheckBox, THTMLRadioButton
  // 1.6.0.0  : New : Support for PNG images via images in associated PictureContainer

type
  TButtonType = (btClassic,btBorland,btFlat,btTMS,btWinXP,btTheme);

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TCustomHTMLCheckBox = class(TCustomControl)
  private
    FDown:Boolean;
    FState:TCheckBoxState;
    FFocused:Boolean;
    FCheckColor:TColor;
    FButtonType:TButtonType;
    FReturnIsTab:Boolean;
    FImages:TImageList;
    FAnchor: string;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FURLColor: TColor;
    FImageCache: THTMLPictureCache;
    FBtnVAlign: TTextLayout;
    FAlignment: TLeftRight;
    FEllipsis: Boolean;
    FCaption: string;
    FContainer: TPictureContainer;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FIsWinXP: Boolean;
    FHot: Boolean;
    FClicksDisabled: Boolean;
    FOldCursor: TCursor;
    FReadOnly: Boolean;
    FMouseDown:boolean;
    FDisabledColor: TColor;
    FDisabledShadow: boolean;
    FBkgBmp: TBitmap;
    FBkgCache: boolean;
    FTransparentCaching: boolean;
    FTransparent:boolean;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetCheckColor(Value:TColor);
    procedure SetState(Value:TCheckBoxState);
    procedure SetChecked(Value:Boolean);
    function  GetChecked:Boolean;
    procedure SetCaption(Value: string);
    procedure SetButtonType(const Value:TButtonType);
    procedure SetImages(const Value: TImageList);
    procedure SetURLColor(const Value:TColor);
    function IsAnchor(x,y:integer):string;
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledShadow(const Value: boolean);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
 protected
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure SetDown(Value:Boolean);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Toggle; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TransparentChaching: boolean read FTransparentCaching write FTransparentCaching;
  published
    property Action;
    property Anchors;
    property Constraints;
    property Color;
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property ButtonType: TButtonType read FButtonType write SetButtonType;
    property ButtonVertAlign: TTextLayout read FBtnVAlign write setButtonVertAlign default tlTop;
    property Caption: string read FCaption write SetCaption;
    property CheckColor: TColor read FCheckColor write SetCheckColor default clBlack;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGray;
    property DisabledShadow: boolean read FDisabledShadow write SetDisabledShadow default true;
    property Down: Boolean read FDown write SetDown default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property Enabled;
    property Font;
    property Images: TImageList read FImages write SetImages;
    property ParentFont;
    property ParentColor;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property PopupMenu;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowHint;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property TabOrder;
    property TabStop;
    property Transparent:boolean read FTransparent write SetTransparent default False;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnAnchorClick: TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter: TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit: TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLCheckBox = class(TCustomHTMLCheckBox)
  published
    property Checked;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLRadioButton = class(TCustomControl)
  private
    FDown: Boolean;
    FChecked: Boolean;
    FFocused: Boolean;
    FCheckColor: TColor;
    FGroupIndex: Byte;
    FButtonType: TButtonType;
    FReturnIsTab: Boolean;
    FImages: TImageList;
    FAnchor: string;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FURLColor: TColor;
    FImageCache: THTMLPictureCache;
    FBtnVAlign: TTextLayout;
    FAlignment: TLeftRight;
    FEllipsis: Boolean;
    FCaption: string;
    FContainer: TPictureContainer;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FIsWinXP: Boolean;
    FHot: Boolean;
    FClicksDisabled: Boolean;
    FOldCursor: TCursor;
    FMouseDown: boolean;
    FDisabledColor: TColor;
    FDisabledShadow: boolean;
    FBkgBmp: TBitmap;
    FBkgCache: boolean;
    FTransparent: Boolean;
    FTransparentCaching: boolean;
    procedure TurnSiblingsOff;
    procedure SetDown(Value:Boolean);
    procedure SetChecked(Value:Boolean);
    procedure SetCheckColor(Value:TColor);
    procedure SetButtonType(const Value:TButtonType);
    procedure SetImages(const Value: TImageList);
    procedure SetURLColor(const Value:TColor);
    function IsAnchor(x,y:integer):string;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledShadow(const Value: boolean);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure Click; override;
    procedure DoClick; virtual;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TransparentChaching: boolean read FTransparentCaching write FTransparentCaching;
  published
    property Action;
    property Anchors;
    property Constraints;
    property Color;
    property Alignment: TLeftRight read fAlignment write SetAlignment;
    property URLColor:TColor read FURLColor write SetURLColor default clBlue;
    property ButtonType:TButtonType read FButtonType write SetButtonType;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property Caption: string read FCaption write SetCaption;
    property CheckColor:TColor read FCheckColor write SetCheckColor default clBlack;
    property Checked:Boolean read FChecked write SetChecked default False;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clGray;
    property DisabledShadow: boolean read FDisabledShadow write SetDisabledShadow default true;
    property Down:Boolean read FDown write SetDown default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property Enabled;
    property Font;
    property GroupIndex:Byte read FGroupIndex write FGroupIndex
      default 0;
    property Images:TImageList read fImages write SetImages;
    property ParentFont;
    property ParentColor;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property PopupMenu;
    property ReturnIsTab:Boolean read FReturnIsTab write FReturnIsTab;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Transparent:boolean read FTransparent write SetTransparent default False;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnAnchorClick:TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter:TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit:TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

  TBtnVAlignment = (vaCenter, vaTop, vaBottom);
  TBtnHAlignment = (haCenter, haHTML, haLeft, haRight);

  TBtnBackground = (stNormal, stNoise, stDiagShade, stHShade, stVShade, stHBump,
    stVBump, stSoftBump, stHardBump, stLMetal, stRMetal, stIRadial, stORadial,
    stHShadeInv, stVShadeInv, stXPTheme);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLButton = class(TButton)
  private
    FCanvas: TCanvas;
    FStyle: TButtonStyle;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FImageCache: THTMLPictureCache;
    IsFocused: Boolean;
    FImages: TImageList;
    FEllipsis: Boolean;
    FCaption: string;
    FContainer: TPictureContainer;
    FFlat: Boolean;
    FHasMouse: Boolean;
    FColor: TColor;
    FDownColor: TColor;
    FHoverColor: TColor;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FBorderColor: TColor;
    FIsWinXP: Boolean;
    FVAlignment: TBtnValignment;
    FAlignment: TBtnHAlignment;
    FShadedBkg: TBitmap;
    FBackground: TBtnBackground;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    function IsCustom: Boolean;
    procedure SetStyle(Value: TButtonStyle);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(Var Msg: TMessage); message CM_MOUSEENTER;
    procedure SetImages(const Value: TImageList);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetFlat(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetShadowColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetVAlignment(const Value: TBtnValignment);
    procedure SetAlignment(const Value: TBtnHAlignment);
    procedure SetBackground(const Value: TBtnBackground);
    procedure ShadeBkg;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Anchors;
    property Constraints;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property Alignment: TBtnHAlignment read FAlignment write SetAlignment;
    property Background: TBtnBackground read FBackground write SetBackground;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Cancel stored IsCustom;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Default stored IsCustom;
    property DownColor: TColor read FDownColor write FDownColor default clBtnFace;
    property DragCursor;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property HoverColor: TColor read FHoverColor write FHoverColor default clBtnFace;
    property Images:TImageList read FImages write SetImages;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult stored IsCustom;
    property ParentFont;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowHint;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property VAlignment: TBtnValignment read FVAlignment write SetVAlignment;
    property Visible;

    property OnEnter;
    property OnExit;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnEndDock;
    property OnStartDrag;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property Version: string read GetVersion write SetVersion;
  end;

  TEnabledEvent = procedure (Sender:TObject; ItemIndex: Integer; var Enabled: Boolean) of object;  

  TCustomHTMLRadioGroup = class(TCustomGroupbox)
  private
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FButtonType: TButtonType;
    FCheckColor: TColor;
    FAlignment: TAlignment;
    FBtnVAlign: TTextLayout;
    FImages: TImageList;
    FContainer: TPictureContainer;
    FEllipsis: Boolean;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FOnIsEnabled: TEnabledEvent;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    procedure SetButtonType(const Value: TButtonType);
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetCheckColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetImages(const Value: TImageList);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure OnAnchorClickEvent(Sender:TObject; Anchor:string);
    procedure OnAnchorEnterEvent(Sender:TObject; Anchor:string);
    procedure OnAnchorExitEvent(Sender:TObject; Anchor:string);
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure PushKey(var Key: Char);
    procedure PushKeyDown(var Key: Word; Shift: TShiftState);
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonType: TButtonType read FButtonType write SetButtonType default btClassic;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property CheckColor: TColor read FCheckColor write SetCheckColor default clBlack;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clSilver;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property OnIsEnabled: TEnabledEvent read FOnIsEnabled write FOnIsEnabled;
    property Version: string read GetVersion write SetVersion;
    property OnAnchorClick:TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter:TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit:TAnchorClick read fAnchorExit write fAnchorExit;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLRadioGroup = class(TCustomHTMLRadioGroup)
  private
  protected
  public
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ButtonType;
    property Caption;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Items;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCustomHTMLCheckGroup = class(TCustomGroupBox)
  private
    FButtons: TList;
    FItems: TStrings;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FButtonType: TButtonType;
    FCheckColor: TColor;
    FAlignment: TAlignment;
    FBtnVAlign: TTextLayout;
    FImages: TImageList;
    FContainer: TPictureContainer;
    FEllipsis: Boolean;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FOnIsEnabled: TEnabledEvent;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    procedure SetButtonType(const Value: TButtonType);
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetCheckColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetImages(const Value: TImageList);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; const Value: Boolean);
    function GetReadOnly(Index: Integer): Boolean;
    procedure SetReadOnly(Index: Integer; const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure OnAnchorClickEvent(Sender:TObject; Anchor:string);
    procedure OnAnchorEnterEvent(Sender:TObject; Anchor:string);
    procedure OnAnchorExitEvent(Sender:TObject; Anchor:string);
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TStrings read FItems write SetItems;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure PushKey(var Key: Char);
    procedure PushKeyDown(var Key: Word; Shift: TShiftState);
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ReadOnly[Index: Integer]: Boolean read GetReadOnly write SetReadOnly;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonType: TButtonType read FButtonType write SetButtonType default btClassic;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property CheckColor: TColor read FCheckColor write SetCheckColor default clBlack;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clSilver;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property OnIsEnabled: TEnabledEvent read FOnIsEnabled write FOnIsEnabled;
    property Version: string read GetVersion write SetVersion;
    property OnAnchorClick:TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter:TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit:TAnchorClick read fAnchorExit write fAnchorExit;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLCheckGroup = class(TCustomHTMLCheckGroup)
  private
  protected
  public
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ButtonType;
    property Caption;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Items;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
  ShellApi, CommCtrl, Math;

{$I HTMLENGO.PAS}

const
  BW = 12;

function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;


function DoThemeDrawing: Boolean;
var
  VerInfo: TOSVersioninfo;
  FIsWinXP,FIsComCtl6: boolean;
  i: integer;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  if FIsWinXP then
  begin
    FIsWinXP := FIsWinXP and IsThemeActive;
  end;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);

  Result := FIsComCtl6 and FIsWinXP;
end;

{ TCustomHTMLCheckBox }

constructor TCustomHTMLCheckBox.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;

begin
  inherited Create(AOwner);
  Width := 98;
  Height := 20;
  FUrlColor := clBlue;
  FBtnVAlign := tlTop;
  FImageCache := THTMLPictureCache.Create;
  FCaption := self.ClassName;
  FShadowOffset := 1;
  FShadowColor := clGray;
  FDisabledColor := clGray;
  FDisabledShadow := true;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  ControlStyle := ControlStyle - [csClickEvents];
  FReadOnly := False;

  FBkgBmp := TBitmap.Create;
  FBkgCache := false;
  FTransparentCaching := false;
end;

function TCustomHTMLCheckBox.IsAnchor(x,y:integer):string;
var
  r,hr: TRect;
  XSize,YSize,HyperLinks,MouseLink: Integer;
  s:string;
  Anchor, Stripped, FocusAnchor:string;
begin
  r := Clientrect;
  s := Caption;
  Anchor:='';

  r.left := r.left + BW + 5;
  r.top := r.top + 4;

  if FButtonType = btTMS then
    r.left := r.left + 5;

  Result := '';

  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0) then
    Result := Anchor;
end;

procedure TCustomHTMLCheckBox.SetTransparent(Value: Boolean);
begin
  if Value <> Ftransparent then
  begin
    Ftransparent := Value;
    FBkgCache := false;
  end;
  Repaint;
end;

procedure TCustomHTMLCheckBox.DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    GetViewportOrgEx(DC, Position);
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);

    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TCustomHTMLCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FTransparent then
  begin
    FBkgCache := false;
    Repaint;
  end;
end;

procedure TCustomHTMLCheckBox.Paint;
var
  BL,BT,BB,BR:Integer;
  R, hr: TRect;
  a,s,fa,text: string;
  xsize,ysize: Integer;
  flg: longint;
  bmp: TBitmap;
  ExtraBW,HyperLinks,MouseLink: Integer;
  HTheme: THandle;
  UseWinXP: Boolean;
  DrawType: TButtonType;
begin
  Canvas.Font := Font;
  ExtraBW := 5;
  BT := 4;

  if FTransparent then
  begin
    if FTransparentCaching then
    begin
      if FBkgCache then
      begin
        Self.Canvas.Draw(0,0,FBkgBmp)
      end
      else
      begin
        FBkgBmp.Width := self.Width;
        FBkgBmp.Height := self.Height;
        DrawParentImage(Self, FBkgBmp.Canvas);
        Self.Canvas.Draw(0,0,FBkgBmp);
        FBkgCache := true;
      end;
    end
    else
      DrawParentImage(Self, self.Canvas);
  end;

  if FIsWinXP then
    UseWinXP := IsThemeActive
  else
    UseWinXP := False;

  with Canvas do
  begin
    Text := Caption;

    DrawType := FButtonType;

    if DrawType = btTheme then
      if DoThemeDrawing then
        DrawType := btWinXP
      else
        DrawType := btClassic;

    case DrawType of
    btBorland:
      begin
        case FBtnVAlign of
        tlTop: BT := 4;
        tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - (BW div 2);
        tlBottom: BT := ClientRect.Bottom - BW - 4;
        end;
        BB := BT + BW;
        if FAlignment = taRightJustify then
          BL := ClientRect.Right - BW - 1
        else
          BL := 1;
        BR := BW + BL;
        Brush.Color := clBtnFace;
        if not FDown then
        begin
          Pen.Color:=clBtnFace;
          Rectangle(BL,BT,BR,BB);
          Pen.Color:=clBtnHighLight;
          MoveTo(BL,BB);
          LineTo(BL,BT);
          LineTo(BR,BT);
          Pen.Color:=clBtnShadow;
          LineTo(BR,BB);
          LineTo(BL,BB);
        end
        else
        begin
          Pen.Color:=clBlack;
          Pen.Width:=2;
          Rectangle(BL+1,BT+1,BR+1,BB+1);
          Pen.Width:=1;
        end;

        case State of
        cbChecked:
          begin
       	    if Enabled then
              Pen.Color := FCheckColor
            else
              Pen.Color := clGray;
            Pen.Width := 1;
            Dec(BT);Dec(BB);
            MoveTo(BL+2,BT+BW div 2+1);
            LineTo(BL+2,BB-1);
            MoveTo(BL+3,BT+BW div 2);
            LineTo(BL+3,BB-2);
            MoveTo(BL+2,BB-1);
            LineTo(BR-2,BT+3);
            MoveTo(BL+3,BB-1);
            LineTo(BR-1,BT+3);
          end;
        cbGrayed:
          begin
            if Down then
            begin
              Pen.Color:=clBtnFace;
              Brush.Color:=clBtnFace;
              Rectangle(BL+2,BT+2,BR-1,BB-1);
            end;
            Brush.Color:=clBtnShadow;
            Rectangle(BL+2,BT+2,BR-1,BB-1);
          end;
        end;
      end;
    btTMS:
      begin
        ExtraBW := 10;
        bmp := TBitmap.Create;
        try
          if state = cbChecked then
          begin
            if Enabled then
              bmp.LoadFromResourceName(hinstance,'HTMBCHK01')
            else
              bmp.LoadFromResourceName(hinstance,'HTMBCHK03');
          end
          else
          begin
            if Enabled then
              bmp.LoadFromResourceName(hinstance,'HTMBCHK02')
            else
              bmp.LoadFromResourceName(hinstance,'HTMBCHK04');
          end;

          bmp.Transparent:=true;
          bmp.TransparentMode :=tmAuto;

          case fBtnVAlign of
          tlTop: BT := 0;
          tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - (bmp.Height div 2);
          tlBottom: BT := ClientRect.Bottom - bmp.Height;
          end;
          if fAlignment = taRightJustify then
            BL := ClientRect.Right - bmp.Width - 1
          else
          BL := 0;
          Canvas.Draw(BL,BT,bmp);
        finally
          bmp.free;
        end;
      end;
    btWinXP:
      begin
        if not UseWinXP then
        begin
          ExtraBW := 10;
          bmp := TBitmap.Create;
          try
            if state = cbChecked then
            begin
              if Enabled then
                bmp.LoadFromResourceName(hinstance,'HTMBCHK05')
              else
                bmp.LoadFromResourceName(hinstance,'HTMBCHK07');
            end
            else
            begin
              if Enabled then
                bmp.LoadFromResourceName(hinstance,'HTMBCHK06')
              else
                bmp.LoadFromResourceName(hinstance,'HTMBCHK08');
            end;

            bmp.Transparent := True;
            bmp.TransparentMode := tmAuto;

            case FBtnVAlign of
            tlTop: BT := 2;
            tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - (bmp.Height div 2);
            tlBottom: BT := ClientRect.Bottom - bmp.Height;
            end;
            if fAlignment = taRightJustify then
              BL := ClientRect.Right - bmp.Width - 1
            else
              BL := 0;
            Canvas.Draw(BL,BT,bmp);
          finally
            bmp.free;
          end;
        end
        else
        begin
          case FBtnVAlign of
          tlTop: BT := 4;
          tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - 6;
          tlBottom: BT := ClientRect.Bottom - 14;
          end;
          if FAlignment = taRightJustify then
            BL := ClientRect.Right - 15
          else
            BL := 0;

          HTheme := OpenThemeData(Self.Handle,'button');

          r := Rect(BL, BT, BL + 14, BT + 14);

          if State = cbChecked then
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDPRESSED,@r,nil)
              else
              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil);
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil);
          end
          else if State = cbGrayed then
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDPRESSED,@r,nil)
              else
              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,@r,nil);
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,@r,nil);
          end
          else
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDPRESSED,@r,nil)
              else

              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil)
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil);
          end;

          CloseThemeData(HTheme);

        end;
      end;

    btClassic,btFlat:
      begin
        if fAlignment = taRightJustify then
          r.Left := ClientRect.Right - 13
        else
          r.left:=0;
        r.Right := r.Left + 13;
      	case FBtnVAlign of
        tlTop: r.Top := 4;
        tlCenter: r.Top := (ClientRect.Bottom-ClientRect.Top) div 2 - 6;
        tlBottom: r.Top := ClientRect.Bottom - 17;
        end;
        r.Bottom := r.Top + 13;
        flg:=0;
        if not Enabled then flg := flg or DFCS_INACTIVE;
        if State = cbGrayed then flg := flg or DFCS_INACTIVE;
        if State = cbChecked then flg := flg or DFCS_CHECKED;
        if FButtonType=btFlat then flg := flg or  DFCS_FLAT;
        DrawFrameControl(canvas.handle,r,DFC_BUTTON,DFCS_BUTTONCHECK or flg);
      end;
    end;

    R := GetClientRect;

    if FAlignment = taRightJustify then
    begin
      r.Left := 0;
      r.Right := r.Right - BW - ExtraBW;
    end
    else																																	
      r.left := r.left + BW + ExtraBW;

    r.Top := r.Top + 4;

    if not Enabled then
    begin
      if FDisabledShadow then
      begin
        OffsetRect(r,1,1);
        Canvas.Font.Color := clWhite;
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clWhite,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
        Offsetrect(r,-1,-1);
      end;

      Canvas.Font.Color := FDisabledColor;

      HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clGray,
        clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
    end
    else
      HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
        clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

    if FFocused then
    begin
      r.right := r.left + xsize + 3;
      r.bottom := r.top + ysize + 1;
      DrawFocusRect(R);
    end;
  end;
end;

procedure TCustomHTMLCheckBox.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckBox.SetDisabledShadow(const Value: boolean);
begin
  if (FDisabledShadow <> Value) then
  begin
    FDisabledShadow := Value;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckBox.SetDown(Value:Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
  end;
end;

procedure TCustomHTMLCheckBox.SetState(Value:TCheckBoxState);
var
  r: TRect;
begin
  if FState <> Value then
  begin
    FState := Value;
    r := GetClientRect;
    case Alignment of
    taLeftJustify: r.Right := 20;
    taRightJustify: r.Left := r.Right - 20;
    end;
    InvalidateRect(self.Handle,@r,True);
  end;
end;

function TCustomHTMLCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TCustomHTMLCheckBox.SetChecked(Value:Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.SetCheckColor(Value:TColor);
begin
  FCheckColor := Value;
  Paint;
end;

procedure TCustomHTMLCheckBox.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
  R: TRect;
begin
  Anchor := '';
  FMouseDown := true;

  if FFocused then
  begin
    Anchor := IsAnchor(X,Y);

    if Anchor <> '' then
    begin
      if (Pos('://',Anchor) > 0) or (Pos('mailto:',anchor) > 0) then
        Shellexecute(0,'open',pchar(anchor),nil,nil,SW_NORMAL)
      else
      begin
        if Assigned(FAnchorClick) then
          FAnchorClick(self,anchor);
      end;
    end;
  end
  else
  begin
    SetFocus;
    FFocused := True;
  end;

  if Anchor = '' then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    MouseCapture := True;
    Down := True;
  end;

  if FIsWinXP and (ButtonType in [btWinXP,btTheme]) then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@R,false);
  end;
end;

procedure TCustomHTMLCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
var
  R: TRect;                                  
begin
  MouseCapture := False;

  Down := False;

  if (X >= 0) and (X<=Width) and (Y>=0) and (Y<=Height) and FFocused and FMouseDown then
  begin
    ClicksDisabled := True;
    Toggle;
    ClicksDisabled := False;
    Click;
  end;
  FMouseDown := false;

  inherited MouseUp(Button, Shift, X, Y);

  if FIsWinXP and (ButtonType in [btWinXP,btTheme]) then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@r,True);
  end;
end;

procedure TCustomHTMLCheckBox.MouseMove(Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
begin
  if MouseCapture then
     Down:=(X>=0) and (X<=Width) and (Y>=0) and (Y<=Height);

//  if fFocused then
    Anchor := IsAnchor(x,y);
//  else
//    Anchor := '';

  if Anchor <> '' then
  begin
    if (self.Cursor = crDefault) or (FAnchor <> Anchor) then
    begin
      FAnchor := Anchor;
      self.Cursor := crHandPoint;
      if Assigned(FAnchorEnter) then
        FAnchorEnter(self,Anchor);
    end;
  end
  else
  begin
    if self.Cursor = crHandPoint then
    begin
      self.Cursor := FOldCursor;
      if Assigned(FAnchorExit) then
        FAnchorExit(self,Anchor);
    end;
  end;

  inherited MouseMove(Shift,X,Y);
end;

procedure TCustomHTMLCheckBox.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  if (Key=vk_return) and (fReturnIsTab) then
  begin
    Key := vk_tab;
    PostMessage(self.Handle,wm_keydown,VK_TAB,0);
  end;

  if Key = vk_Space then
    Down := True;
  inherited KeyDown(Key,Shift);
end;

procedure TCustomHTMLCheckBox.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
  begin
    Down := False;
    Toggle;
    Click;
  end;
end;

procedure TCustomHTMLCheckBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    Invalidate;
  end;  
end;

procedure TCustomHTMLCheckBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages:=nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomHTMLCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.SetButtonType(const Value: TButtonType);
begin
  FButtonType := Value;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.SetButtonVertAlign(const Value: TTextLayout);
begin
  if Value <> FBtnVAlign then
  begin
    FBtnVAlign := Value;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckBox.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

destructor TCustomHTMLCheckBox.Destroy;
begin
  FBkgBmp.Free;
  FImageCache.Free;
  inherited;
end;

procedure TCustomHTMLCheckBox.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate
  end;
end;

procedure TCustomHTMLCheckBox.SetCaption(Value: string);
begin
  SetWindowText(Handle,pchar(Value));
  FCaption := Value;
  Invalidate;
end;


procedure TCustomHTMLCheckBox.Toggle;
begin
  if not FReadOnly then
    Checked := not Checked;
end;

procedure TCustomHTMLCheckBox.WMEraseBkGnd(var Message: TMessage);
begin
 if Transparent then
    Message.Result := 1
  else
   inherited;
end;

procedure TCustomHTMLCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, FCaption) and CanFocus then
    begin
      Toggle;
      if Assigned(OnClick) then
        OnClick(self);
      if TabStop then
        SetFocus;
      Result := 1;
    end
    else
      inherited;
  end;
end;

procedure TCustomHTMLCheckBox.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure TCustomHTMLCheckBox.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckBox.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckBox.CMMouseEnter(var Message: TMessage);
var
  R: TRect;
begin
  FHot := True;
  if (FButtonType in [btWinXP,btTheme]) then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@r,True);
  end;
end;

procedure TCustomHTMLCheckBox.CMMouseLeave(var Message: TMessage);
var
  R: TRect;
begin
  FHot := False;
  if (FButtonType in [btWinXP, btTheme]) then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@r,True);
  end;
end;

procedure TCustomHTMLCheckBox.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

function TCustomHTMLCheckBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomHTMLCheckBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomHTMLCheckBox.SetVersion(const Value: string);
begin

end;

{ THTMLRadioButton }

constructor THTMLRadioButton.Create(AOwner: TComponent);
var
  VerInfo: TOSVersionInfo;

begin
  inherited Create(AOwner);
  Width := 98;
  Height := 20;
  FURLColor := clBlue;
  FBtnVAlign := tlTop;
  FImageCache := THTMLPictureCache.Create;
  FCaption := self.ClassName;
  FShadowOffset := 1;
  FShadowColor := clGray;
  FDisabledColor := clGray;
  FDisabledShadow := true;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);


  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  FBkgBmp := TBitmap.Create;
  FBkgCache := false;
  FTransparentCaching := false;
end;

function THTMLRadioButton.IsAnchor(x,y:integer):string;
var
  r,hr: TRect;
  XSize,YSize,HyperLinks,MouseLink: Integer;
  s: string;
  Anchor,Stripped,FocusAnchor: string;
begin
  r := Clientrect;
  s := Caption;
  Anchor := '';

  r.left := r.left + BW + 5;
  r.top := r.top + 4;
  if FButtonType = btTMS then r.left:=r.left+5;

  Result := '';

  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0) then
    Result := Anchor;
end;

procedure THTMLRadioButton.SetTransparent(value:boolean);
begin
  if Value <> FTransparent then
  begin
    Ftransparent := Value;
    FBkgCache := false;
  end;
  Repaint;
end;

procedure THTMLRadioButton.DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    GetViewportOrgEx(DC, Position);
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    RestoreDC(DC, SaveIndex);
  end;
end;


procedure THTMLRadioButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FTransparent then
  begin
    FBkgCache := false;
    Repaint;
  end;
end;



procedure THTMLRadioButton.Paint;
var
  BL,BT,BR,BB,BM:Integer;
  CX,CY:Integer;
  R,hr: TRect;
  a,s,fa,text: string;
  XSize,YSize,HyperLinks,MouseLink: Integer;
  flg: Longint;
  bmp: TBitmap;
  HTheme: THandle;
  UseWinXP: Boolean;
  DrawType: TButtonType;

begin
  Canvas.Font := Font;
  Text := Caption;

  if FTransparent then
  begin
    if FTransparentCaching then
    begin
      if FBkgCache then
      begin
        Self.Canvas.Draw(0,0,FBkgBmp)
      end
      else
      begin
        FBkgBmp.Width := self.Width;
        FBkgBmp.Height := self.Height;
        DrawParentImage(Self, FBkgBmp.Canvas);
        Self.Canvas.Draw(0,0,FBkgBmp);
        FBkgCache := true;
      end;
    end
    else
      DrawParentImage(Self, self.Canvas);
  end;



  if FIsWinXP then
    UseWinXP := IsThemeActive
  else
    UseWinXP := False;

  with Canvas do
  begin
    BR := 0;
    BT := 4;


    DrawType := FButtonType;

    if DrawType = btTheme then
      if DoThemeDrawing then
        DrawType := btWinXP
      else
        DrawType := btClassic;

    case DrawType of
    btBorland:
      begin
        BM := BW div 2;
        case FBtnVAlign of
        tlTop: BT := 4;
        tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - BM;
        tlBottom: BT := ClientRect.Bottom - BW - 4;
        end;
        BB := BT + BW;
        if FAlignment = taRightJustify then
          BL := ClientRect.Right - BW - 1
        else
          BL := 1;
        BR := BW + BL;
        Brush.Color := clBtnFace;
        if Down then
        begin
          Pen.Color:=clBlack;
          MoveTo(BL+BM,BT);
          LineTo(BL,BT+BM);
          LineTo(BL+BM,BB);
          LineTo(BR,BT+BM);
          LineTo(BL+BM,BT);
          MoveTo(BL+BM,BT+1);
          LineTo(BL+1,BT+BM);
          LineTo(BL+BM,BB-1);
          LineTo(BR-1,BT+BM);
          LineTo(BL+BM,BT+1);
        end
        else
        begin
          Pen.Color := clBtnFace;
          if Checked then
            Pen.Color := clGray
          else
            Pen.Color := clSilver;
          MoveTo(BL+BM,BT);
          LineTo(BL,BT+BM);
          LineTo(BL+BM,BB);
          if Checked then
            Pen.Color := clSilver
          else
            Pen.Color := clGray;
          LineTo(BR,BT+BM);
          LineTo(BL+BM,BT);
        end;
        if Checked then
        begin
          if Enabled then
            Pen.Color := CheckColor
          else
            Pen.Color := clGray;
          CX:=BL+BM;CY:=BT+BM;
          MoveTo(CX-1,CY-1);
          LineTo(CX+2,CY-1);
          MoveTo(CX-2,CY);
          LineTo(CX+3,CY);
          MoveTo(CX-1,CY+1);
          LineTo(CX+2,CY+1);
          MoveTo(CX,CY-2);
          LineTo(CX,CY+3);
        end;
        BR := BW;
      end;
    btTMS:
      begin
        BR := 13;
        bmp := TBitmap.Create;
        try
          if (Checked) then
          begin
            if Enabled then
              bmp.LoadFromResourceName(hinstance,'HTMBRAD01')
            else
              bmp.LoadFromResourceName(hinstance,'HTMBRAD03');
          end
          else
          begin
            if Enabled then
              bmp.LoadFromResourceName(hinstance,'HTMBRAD02')
            else
              bmp.LoadFromResourceName(hinstance,'HTMBRAD04');
          end;

          bmp.Transparent:=true;
          bmp.TransparentMode :=tmAuto;

          case FBtnVAlign of
          tlTop: BT := 2;
          tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - (bmp.Height div 2);
          tlBottom: BT := ClientRect.Bottom - bmp.Height - 2;
          end;
          if fAlignment = taRightJustify then
            BL := ClientRect.Right - bmp.Width - 1
          else
            BL := 0;
          Canvas.Draw(BL,BT,bmp);
        finally
          bmp.Free;
        end;
      end;
    btWinXP:
      begin
        BR := 13;

        if not UseWinXP then
        begin
          bmp := TBitmap.Create;
          try
            if (Checked) then
            begin
              if Enabled then
                bmp.LoadFromResourceName(hinstance,'HTMBRAD05')
              else
                bmp.LoadFromResourceName(hinstance,'HTMBRAD07');
            end
            else
            begin
              if Enabled then
                bmp.LoadFromResourceName(hinstance,'HTMBRAD06')
              else
                bmp.LoadFromResourceName(hinstance,'HTMBRAD08');
            end;

            bmp.Transparent := True;
            bmp.TransparentMode := tmAuto;

            case FBtnVAlign of
            tlTop: BT := 2;
            tlCenter: BT := (ClientRect.Bottom-ClientRect.Top) div 2 - (bmp.Height div 2);
            tlBottom: BT := ClientRect.Bottom - bmp.Height - 2;
            end;
            if fAlignment = taRightJustify then
              BL := ClientRect.Right - bmp.Width - 1
            else
              BL := 0;
            Canvas.Draw(BL,BT,bmp);
          finally
            bmp.Free;
          end;
        end
        else
        begin
          case FBtnVAlign of
          tlTop: BT := 4;
          tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - 6;
          tlBottom: BT := ClientRect.Bottom - 14;
          end;
          if FAlignment = taRightJustify then
            BL := ClientRect.Right - 14
          else
            BL := 0;

          HTheme := OpenThemeData(Self.Handle,'button');

          r := Rect(BL, BT, BL + 13, BT + 13);

          if Checked then
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDPRESSED,@r,nil)
              else
              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,@r,nil);
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDDISABLED,@r,nil);
          end
          else
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDPRESSED,@r,nil)

              else

              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,@r,nil)
             end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDDISABLED,@r,nil);
          end;

          CloseThemeData(HTheme);

        end;
      end;
    btClassic,btFlat:
      begin
        if fAlignment = taRightJustify then
          r.Left := ClientRect.Right - 13
        else
          r.Left:=0;

        r.Right := r.Left + 13;
        case FBtnVAlign of
        tlTop: r.Top := 4;
        tlCenter: r.Top := (ClientRect.Bottom-ClientRect.Top) div 2 - 6;
        tlBottom: r.Top := ClientRect.Bottom - 17;
        end;

        r.bottom := r.Top + 13;																						
        flg:=0;
        BR:=13;
        if Checked then flg := flg or DFCS_CHECKED;
        if FButtonType=btFlat then flg := flg or  DFCS_FLAT;
        if not Enabled then flg := flg or DFCS_INACTIVE;
        DrawFrameControl(Canvas.Handle,r,DFC_BUTTON,DFCS_BUTTONRADIO or flg);
      end;
    end;

      r := GetClientRect;
      if FAlignment = taRightJustify then
      begin
        r.Left := 0;
        r.Right := r.Right - BR - 5;
      end
      else																																				
        r.Left := r.Left + BR + 5;

      r.Top := r.Top + 4;

      if not Enabled then
      begin
        if FDisabledShadow then
        begin
          OffsetRect(R,1,1);
          Canvas.Font.Color := clWhite;
          HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clGray,
            clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
          Offsetrect(R,-1,-1);
        end;

        Canvas.Font.Color := FDisabledColor;

        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clWhite,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
      end
      else
      begin
        Canvas.Font.Color := Font.Color;
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
      end;

      if FFocused then
      begin
        r.Right := r.Left + xsize + 3;
        r.Bottom := r.Top + ysize + 1;
        DrawFocusRect(R);
      end;
    end;

end;

procedure THTMLRadioButton.SetURLColor(const Value: TColor);
begin
  FURLColor := Value;
  Invalidate;
end;


procedure THTMLRadioButton.SetDisabledColor(const Value: TColor);
begin
  if (FDisabledColor <> Value) then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetDisabledShadow(const Value: boolean);
begin
  if (FDisabledShadow <> Value) then
  begin
    FDisabledShadow := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetDown(Value:Boolean);
begin
  if FDown<>Value then
  begin
    FDown:=Value;
  end;
end;


procedure THTMLRadioButton.TurnSiblingsOff;
var
  i:Integer;
  Sibling: THTMLRadioButton;

begin
  if (Parent <> nil) then
  for i:=0 to Parent.ControlCount-1 do
    if Parent.Controls[i] is THTMLRadioButton then
    begin
      Sibling := THTMLRadioButton(Parent.Controls[i]);
      if (Sibling <> Self) and
         (Sibling.GroupIndex = GroupIndex) then
          Sibling.SetChecked(False);
    end;
end;

procedure THTMLRadioButton.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    TabStop := Value;
    FChecked := Value;
    if Value then
    begin
      TurnSiblingsOff;
      //if not FClicksDisabled then
      DoClick;
    end;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetButtonType(const Value:TButtonType);
begin
  FButtonType := Value;
  Invalidate;
end;

procedure THTMLRadioButton.SetCheckColor(Value:TColor);
begin
  FCheckColor := Value;
  Invalidate;
end;

procedure THTMLRadioButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure THTMLRadioButton.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Checked := True;
  Invalidate;
end;

procedure THTMLRadioButton.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  Invalidate;
end;

procedure THTMLRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Anchor:string;
begin
  Anchor := '';
  FMouseDown := true;

  if FFocused then
  begin
    Anchor := IsAnchor(X,Y);
    if Anchor <> '' then
    begin
      if (Pos('://',Anchor)>0) or (Pos('mailto:',Anchor)>0) then
        ShellExecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
      else
      begin
        if Assigned(FAnchorClick) then
          FAnchorClick(self,anchor);
      end;
    end;
  end
  else
  begin
    SetFocus;
    FFocused := True;
  end;

  if Anchor = '' then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    MouseCapture := True;
    Down := True;
  end;

  if FIsWinXP then
    Invalidate;
end;

procedure THTMLRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  MouseCapture := False;
  Down := False;

  if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and not Checked and FMouseDown then
  begin
    Checked := true;
  end;

  FMouseDown := false;


  inherited MouseUp(Button, Shift, X, Y);

  if FIsWinXP and (ButtonType in [btWinXP,btTheme]) then
    Invalidate;
end;

procedure THTMLRadioButton.MouseMove(Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
begin
  if MouseCapture then
    Down := (X>=0) and (X<=Width) and (Y>=0) and (Y<=Height);

//  if FFocused then
    Anchor := IsAnchor(x,y);
//  else
//    Anchor := '';

  if Anchor <> '' then
  begin
    if (self.Cursor = crDefault) or (fAnchor<>Anchor) then
    begin
      FAnchor := Anchor;
      self.Cursor := crHandPoint;
      if Assigned(FAnchorEnter) then
        FAnchorEnter(self,anchor);
    end;
  end
  else
  begin
    if self.Cursor = crHandPoint then
    begin
      self.Cursor := FOldCursor;
      if Assigned(FAnchorExit) then
        FAnchorExit(self,anchor);
    end;
  end;

  inherited MouseMove(Shift,X,Y);
end;

procedure THTMLRadioButton.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  if (Key=vk_return) and (fReturnIsTab) then
  begin
    Key := vk_tab;
    PostMessage(self.Handle,wm_keydown,VK_TAB,0);
  end;

  if Key = vk_Space then
    Down := True;
  inherited KeyDown(Key,Shift);
end;

procedure THTMLRadioButton.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
  begin
     Down := False;
    if not Checked then Checked := True;
  end;
end;

procedure THTMLRadioButton.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLRadioButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure THTMLRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLRadioButton.SetButtonVertAlign(const Value: TTextLayout);
begin
  if Value <> FBtnVAlign then
  begin
    FBtnVAlign := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

destructor THTMLRadioButton.Destroy;
begin
  FBkgBmp.Free;
  FImageCache.Free;
  inherited;
end;

procedure THTMLRadioButton.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  FCaption := Value;
  Invalidate;
end;

procedure THTMLRadioButton.Click;
begin
//  inherited;
end;

procedure THTMLRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, FCaption) and CanFocus then
    begin
      Checked := True;
      if TabStop then
        SetFocus;
      Result := 1;
    end
    else
      inherited;
  end;
end;

procedure THTMLRadioButton.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure THTMLRadioButton.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure THTMLRadioButton.CMMouseEnter(var Message: TMessage);
begin
  FHot := True;
  if FIsWinXP and (ButtonType in [btWinXP, btTheme]) then
    Invalidate;
end;

procedure THTMLRadioButton.CMMouseLeave(var Message: TMessage);
begin
  FHot := False;
  if FIsWinXP and (ButtonType in [btWinXP, btTheme]) then
    Invalidate;
end;


procedure THTMLRadioButton.WMEraseBkGnd(var Message: TMessage);
begin
  if Transparent then
    Message.Result := 1
  else
   inherited;
end;


procedure THTMLRadioButton.WMLButtonDown(var Message: TMessage);
begin
  FClicksDisabled := True;
  SetFocus;
  FClicksDisabled := False;
  inherited;
end;

procedure THTMLRadioButton.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

function THTMLRadioButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLRadioButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLRadioButton.SetVersion(const Value: string);
begin

end;

{ THTMLButton }

constructor THTMLButton.Create(AOwner: TComponent);
var
  VerInfo: TOSVersionInfo;
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FStyle := bsAutoDetect;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  ControlStyle := ControlStyle + [csReflector];
  FImageCache := THTMLPictureCache.Create;
  FCaption := self.ClassName;
  FColor := clBtnFace;
  FHoverColor := clBtnFace;
  FDownColor := clBtnFace;
  FShadowColor := clGray;
  FBorderColor:= clGray;
  FShadowOffset := 1;
  DoubleBuffered := True;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FShadedBkg := TBitmap.Create;

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

destructor THTMLButton.Destroy;
begin
  FImageCache.Free;
  FShadedBkg.Free;
  inherited Destroy;
  FCanvas.Free;
end;

procedure THTMLButton.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure THTMLButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

procedure THTMLButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure THTMLButton.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then Form.Close
        else inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then Application.HelpContext(Control.HelpContext)
        else inherited Click;
      end;
    else
      inherited Click;
  end;
end;


procedure THTMLButton.ShadeBkg;
var
  a,x,y,xs,i,j,h,k,s,sizeX,sizeY: Integer;
  d : TColor;
  R: Trect;
  Light: Byte;
  rr,br,gr: Integer;

  function Dist(x1,y1,x2,y2: Integer): Integer;
  begin
    Result := Round(sqrt( sqr(x1-x2) + sqr(y1 - y2)));
  end;

begin
  rr := GetRValue(ColorToRGB(Color));
  gr := GetGValue(ColorToRGB(Color));
  br := GetBValue(ColorToRGB(Color));

  Light := 255;
  FShadedBkg.Width := Width + 2;
  FShadedBkg.Height := Height + 2; 

  Randomize;
  SizeX := FShadedBkg.Width;
  SizeY := FShadedBkg.Height;
  FShadedBkg.Canvas.Brush.Color := clWhite;
  r := Rect(0,0,SizeX,SizeY);
  FShadedBkg.Canvas.FillRect(r); //clear the bitmap

  case Background of
  stIRADIAL,stORADIAL:
    begin
      h := Dist(0,SizeX,0,SizeY);
      x := sizeX div 2;
      y := sizeY div 2;

      for i := 0 to x do
        for j := 0 to y do
        begin
          k := Dist(i,j,x,y);

          if Background = stIRADIAL then
            k := Round((h - k) / h * Light)
          else
            k := Round(k / h * Light);

          d := RGB( (rr*k) div 255,(gr*k) div 255,(br*k) div 255);

          FShadedBkg.Canvas.Pixels[i,j] := d;
          FShadedBkg.Canvas.Pixels[sizex - i,sizey - j] := d;
          FShadedBkg.Canvas.Pixels[sizex - i,j] := d;
          FShadedBkg.Canvas.Pixels[i,sizey - j] := d;
        end;
    end;
  stLMETAL,stRMETAL:
    begin
      for a := 0 to 250 do
      begin
        x := Random(sizeX);
        y := Random(sizeY);
        xs := Random(Min(sizeX,sizeY) div 2);
        i := Light - Random(40);
        d := RGB( (rr*i) div 255,(gr*i) div 255,(br*i) div 255);
        for i := 0 to xs - 1 do
        begin
          if Background = stLMetal then
          begin
            if (((x-i)>0)and((y+i)<sizeY)) then
              FShadedBkg.Canvas.Pixels[x + i,y + i] := d;
            if (((x+i)<sizeX)and((y-i)>0)) then
              FShadedBkg.Canvas.Pixels[x - i,y - i] := d;
          end
          else
          begin
            if (((x-i)>0)and((y+i)<sizeY)) then
              FShadedBkg.Canvas.Pixels[x - i,y + i] := d;
            if (((x+i)<sizeX)and((y-i)>0)) then
              FShadedBkg.Canvas.Pixels[x + i,y - i] := d;
          end;
        end;
      end;
      a := 120;
      for i := 0 to sizeX do
        for j := 0 to sizeY do
        begin
          d := FShadedBkg.Canvas.Pixels[i,j];
          x := GetBValue(d);
          x := Light - x;
          x := x + ((a*i) div sizeX)+((a*j) div sizeY);
          x := Light - x div 2;
          d := RGB( (rr*x) div 255,(gr*x) div 255,(br*x) div 255);
          FShadedBkg.Canvas.Pixels[i,j] := d;
        end;
    end;
  stHARDBUMP:
    begin
      for i := 0 to sizeY do
      begin
        x := (255*i div sizeY)-127;
        x := (x*(x*x) div 128) div 128;
        x := ((x*112) div 128) +128;
        for j:= 0 to sizeX do
        begin
          y := Light - x div 2; //offset
          d := RGB( (rr*y) div 255,(gr*y) div 255,(br*y) div 255);
          FShadedBkg.Canvas.Pixels[j,i] := d;
        end;
      end;
      k := min(16, sizeX div 6);
      a := (sizeY*sizeY) div 4;
      for i := 0 to sizeY do
      begin
        y := i - sizeY div 2;
        for j := 0 to sizeX do
        begin
          x  := j - sizeX div 2;
          xs := sizeX div 2 - k + (y*y*k) div a;
          if (x > xs)   then
          begin
            s := 8 + (((sizeX-j)*128) div k);
            s := Light - s div 2;//offset
            d := RGB( (rr*s) div 255,(gr*s) div 255,(br*s) div 255);
            FShadedBkg.Canvas.Pixels[j,i] := d;
          end;
          if (x + xs) < 0   then
          begin
            s := 247 - ((j*128) div k);
            s := Light - s div 2;//offset
            d := RGB( (rr*s) div 255,(gr*s) div 255,(br*s) div 255);
            FShadedBkg.Canvas.Pixels[j,i] := d;
          end;
        end;
      end;
    end;
  stSOFTBUMP:
    begin
      for i := 0 to sizeY do
      begin
        h := ((255 * i) div sizeY) - 127;
        for j := 0 to sizeX do
        begin
          k := 255 * (sizeX - j) div sizeX - 127;
          k := ((h * (h * h)) div 128) div 128 + (k * ( k * k) div 128) div 128;
          k := k * (128 - 8) div 128 + 128;
          if (k < 8)  then k := 8;
          if (k > 247) then k := 247;
          s := Light - k div 2;  //offset
          d := RGB( (rr*s) div 255,(gr*s) div 255,(br*s) div 255);
          FShadedBkg.Canvas.Pixels[j,i] := d;
        end;
      end;
    end;
  stHBUMP:
    begin
      for j := 0 to sizeX do
      begin
        k := (255*(sizeX - j)div sizeX)-127;
        k := (k*(k*k)div 128)div 128;
        k := (k*(128 - 8))div 128 + 128;
        for i := 0 to sizeY do
        begin
          s := Light - k div 2;//offset
          d := RGB( (rr*s) div 255,(gr*s) div 255,(br*s) div 255);
          FShadedBkg.Canvas.Pixels[j,i] := d;
        end;
      end;
    end;
  stVBUMP:
    begin
      for i := 0 to sizeY do
      begin
        k := (255*i div sizeY)-127;
        k := (k*(k*k)div 128)div 128;
        k := (k*(128 - 8))div 128 + 128;
        for j := 0 to sizeX do
        begin
          s := Light - k div 2;//offset
          d := RGB( (rr*s) div 255,(gr*s) div 255,(br*s) div 255);
          FShadedBkg.Canvas.Pixels[j,i] := d;
        end;
      end;
    end;
  stDIAGSHADE:
    begin
      a := 129;
      for i := 0 to sizeX do
        for j := 0 to sizeY do
        begin
          d := FShadedBkg.Canvas.Pixels[i,j];
          x := GetBValue(d);
          x := Light-x;
          x := x+((a*i) div sizeX)+((a*j) div sizeY);
          x := Light-x div 2;//offset
          d := RGB( (rr*x) div 255,(gr*x) div 255,(br*x) div 255);
          FShadedBkg.Canvas.Pixels[i,j] := d;
        end;
      end;
  stVSHADE,stVSHADEInv:
    begin
      a := 239;
      for i := 0 to sizeY do
      begin
        k := a * i div sizeY +8;
        k := Light-k div 2;//offset
        d := RGB( (rr*k) div 255,(gr*k) div 255,(br*k) div 255);
        for j := 0 to sizeX do
          if Background = stVSHADEInv then
            FShadedBkg.Canvas.Pixels[j,i] := d
          else
            FShadedBkg.Canvas.Pixels[sizeX - j,i] := d
      end;
    end;
  stHSHADE,stHShadeInv:
    begin
      a := 239;
      for j := 0 to sizeX do
      begin
        k := a * (sizeX-j) div sizeX +8;
        k := Light-k div 2;//offset
        d := RGB( (rr*k) div 255,(gr*k) div 255,(br*k) div 255);
        for i := 0 to sizeY do
          if Background = stHSHADE then
            FShadedBkg.Canvas.Pixels[j,i] := d
          else
            FShadedBkg.Canvas.Pixels[sizeX - j,i] := d
      end;
    end;
  stNOISE:
    begin
      for i := 0 to sizeX do
        for j := 0 to sizeY do
        begin
          k := 128 + Random(32); // grain
          k := Light-k div 2;//offset
          d := RGB( (rr*k) div 255,(gr*k) div 255,(br*k) div 255);
          FShadedBkg.Canvas.Pixels[i,j] := d;
        end;
      end;
  stNORMAL,stXPTheme:
    begin  //for normal we use the panel caption color
      FShadedBkg.Canvas.Brush.Color:= Color;
      FShadedBkg.Canvas.FillRect(r);
    end;
  end;
end;


procedure THTMLButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    ItemWidth := Width;
    ItemHeight := Height;
  end;
end;

procedure THTMLButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure THTMLButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  R,hr: TRect;
  Flags: Longint;
  Anchor,Stripped,FocusAnchor:string;
  XSize,YSize,HyperLinks,MouseLink:integer;
  s: string;
  HTheme: THandle;
  Pt: TPoint;
  UseWinXP: Boolean;

begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    IsDown := ItemState and ODS_SELECTED <> 0;
    IsDefault := ItemState and ODS_FOCUS <> 0;
  end;

  if FIsWinXP then
    UseWinXP := IsThemeActive and (Background = stXPTheme)
  else
    UseWinXP := False;

  FCanvas.Font.Assign(Font);

  if not (Background in [stXPTheme,stNormal]) then
  begin
    if IsDown then
      FCanvas.Draw(0,0,FShadedBkg)
    else
      FCanvas.Draw(-2,-2,FShadedBkg);

      FCanvas.Pen.Color := clWhite;
      FCanvas.Pen.Width := 1;
      FCanvas.MoveTo(0,R.Bottom);
      FCanvas.LineTo(0,0);
      FCanvas.LineTo(R.Right,0);
      FCanvas.Pen.Color := clGray;
      FCanvas.LineTo(R.Right,R.Bottom);
      FCanvas.LineTo(R.Left,R.Bottom);
  end
  else
  begin
    if FFlat then
    begin
      if not IsDown then
      begin
        FCanvas.Pen.Color := FBorderColor;
        FCanvas.Pen.Width := 1;
        if FHasMouse then
          FCanvas.Brush.Color := FHoverColor
        else
          FCanvas.Brush.Color := FColor;

        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

        if IsFocused then
        begin
          InflateRect(R,-1,-1);
          FCanvas.Pen.Color := FCanvas.Brush.Color;
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;

        if FHasMouse then
        begin
          FCanvas.Pen.Color := clWhite;
          FCanvas.Pen.Width := 1;
          FCanvas.MoveTo(R.Left,R.Bottom);
          FCanvas.LineTo(R.Left,R.Top);
          FCanvas.LineTo(R.Right,R.Top);

          FCanvas.Pen.Color := clBtnShadow;
          FCanvas.Pen.Width := 1;
          FCanvas.MoveTo(R.Right - 1,R.Top);
          FCanvas.LineTo(R.Right - 1,R.Bottom - 1);
          FCanvas.LineTo(R.Left,R.Bottom - 1);
        end;
      end
      else
      begin
        FCanvas.Pen.Color := clBtnShadow;
        FCanvas.Pen.Width := 2;
        FCanvas.Brush.Color := FDownColor;
        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        FCanvas.Pen.Color := clBtnShadow;
        FCanvas.Pen.Width := 1;
        FCanvas.MoveTo(R.Left,R.Bottom);
        FCanvas.LineTo(R.Left,R.Top);
        FCanvas.LineTo(R.Right,R.Top);
      end;
    end
    else
    begin
      if UseWinXP then
      begin
        HTheme := OpenThemeData(Self.Handle,'button');

        GetCursorPos(pt);
        pt := ScreenToClient(pt);

        if IsDown then
          DrawThemeBackground(HTheme,FCanvas.Handle, BP_PUSHBUTTON,PBS_PRESSED ,@R,nil)

        else
          if PtInRect(R,pt) then
            DrawThemeBackground(HTheme,FCanvas.Handle, BP_PUSHBUTTON,PBS_HOT ,@R,nil)

          else
            DrawThemeBackground(HTheme,FCanvas.Handle, BP_PUSHBUTTON,PBS_NORMAL ,@R,nil);

        CloseThemeData(HTheme);
      end
      else
      begin
        Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;

        if IsDown then Flags := Flags or DFCS_PUSHED;

        if DrawItemStruct.ItemState and ODS_DISABLED <> 0 then
          Flags := Flags or DFCS_INACTIVE;

        // DrawFrameControl doesn't allow for drawing a button as the
        //  default button, so it must be done here.
        if IsFocused or IsDefault then
        begin
          FCanvas.Pen.Color := clWindowFrame;
          FCanvas.Pen.Width := 1;
          FCanvas.Brush.Style := bsClear;
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          // DrawFrameControl must draw within this border
          InflateRect(R, -1, -1);
        end;

        // DrawFrameControl does not draw a pressed button correctly }
        if IsDown then
        begin
          FCanvas.Pen.Color := clBtnShadow;
          FCanvas.Pen.Width := 1;
          FCanvas.Brush.Color := clBtnFace;
          FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          InflateRect(R, -1, -1);
        end
        else
          DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);
      end;
    end;
  end;

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
  end;

  FCanvas.Font := Self.Font;

  if IsDown then
    OffsetRect(R, 1, 1);

  if IsFocused and (IsDefault or not (Background in [stNormal,stXPTheme])) then
  begin
    R := ClientRect;

    if not (Background in [stNormal,stXPTheme]) then
      InflateRect(R, -2, -2)
    else
      InflateRect(R, -4, -4);

    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, R);
  end;

  R := ClientRect;

  Inflaterect(r,-4,-4);

  s := Caption;

  HTMLDrawEx(FCanvas,s,R,FImages,0,0,-1,-1,FShadowOffset,False,True,False,False,False,False,not FEllipsis,1.0,clBlue,
                clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

  case Alignment of
  haCenter:
    if XSize < R.Right - R.Left then
      R.Left := R.Left + ((R.Right - R.Left - XSize) shr  1);
  haRight:
    if XSize < R.Right - R.Left then
      R.Left := R.Right - XSize;
  end;


  case FVAlignment of
  vaCenter:
    if YSize < R.bottom - R.Top then
      R.top := R.top + ((R.Bottom - R.top - YSize) shr 1);
  vaBottom:
    R.Top := R.Bottom - YSize;
  end;

  if IsDown then
    OffsetRect(r,1,1);

  if not Enabled then
  begin
    Offsetrect(R,1,1);
    FCanvas.Font.Color := clWhite;

    HTMLDrawEx(FCanvas,s,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clWhite,
               clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

    FCanvas.Font.Color := clGray;
    Offsetrect(r,-1,-1);

    HTMLDrawEx(FCanvas,s,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clGray,
               clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

  end
  else
    HTMLDrawEx(FCanvas,s,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clBlue,
              clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

  FCanvas.Handle := 0;
end;

procedure THTMLButton.SetVAlignment(const Value: TBtnValignment);
begin
  FVAlignment := Value;
  Invalidate;
end;



procedure THTMLButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, MakeLParam(Message.Pos.x, Message.Pos.y));
end;

function THTMLButton.IsCustom: Boolean;
begin
  Result := true;
end;

procedure THTMLButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;


procedure THTMLButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
end;

procedure THTMLButton.SetImages(const Value: TImageList);
begin
   FImages := Value;
   Repaint;
end;

procedure THTMLButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure THTMLButton.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  FCaption := Value;
  Invalidate;
end;

procedure THTMLButton.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.CMMouseEnter(var Msg: TMessage);
begin
  FHasMouse := True;
  inherited;
  Invalidate;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure THTMLButton.CMMouseLeave(var Msg: TMessage);
begin
  FHasMouse := False;
  inherited;
  Invalidate;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure THTMLButton.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ShadeBkg;
    Invalidate;
  end;
end;

procedure THTMLButton.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure THTMLButton.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;


procedure THTMLButton.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure THTMLButton.SetAlignment(const Value: TBtnHAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure THTMLButton.SetBackground(const Value: TBtnBackground);
begin
  FBackground := Value;
  ShadeBkg;
  Invalidate;
end;

procedure THTMLButton.WMSize(var Msg: TMessage);
begin
  inherited;
  if Width <> FShadedBkg.Width then
    ShadeBkg;
end;

function THTMLButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLButton.SetVersion(const Value: string);
begin

end;

{ TGroupButton }

type
  THTMLGroupButton = class(THTMLRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TCustomHTMLRadioGroup);
    destructor Destroy; override;
  end;

constructor THTMLGroupButton.InternalCreate(RadioGroup: TCustomHTMLRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  OnAnchorClick := RadioGroup.OnAnchorClickEvent;
  OnAnchorEnter := RadioGroup.OnAnchorEnterEvent;
  OnAnchorExit  := RadioGroup.OnAnchorExitEvent;
  Parent := RadioGroup;
end;

destructor THTMLGroupButton.Destroy;
begin
  TCustomHTMLRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure THTMLGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;

    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TCustomHTMLRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;

    FInClick := False;
  end;
end;

procedure THTMLGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TCustomHTMLRadioGroup(Parent).PushKey(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TCustomHTMLRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure THTMLGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TCustomHTMLRadioGroup(Parent).PushKeyDown(Key, Shift);
end;

{ TCustomHTMLRadioGroup }

constructor TCustomHTMLRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
  FAlignment := taLeftJustify;
  FCheckColor := clBlack;
  FButtonType := btClassic;
  FBtnVAlign := tlTop;
  ShadowOffset := 1;
  ShadowColor := clSilver;
end;

destructor TCustomHTMLRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TCustomHTMLRadioGroup.PushKey(var Key: Char);
begin
  KeyPress(Key);
end;

procedure TCustomHTMLRadioGroup.PushKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key,Shift);
end;


procedure TCustomHTMLRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TCustomHTMLRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
  RadioEnable: Boolean;

begin
 
  if (csLoading in ComponentState) then
    Exit;

  if not HandleAllocated then
    Exit;


  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with THTMLGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;

          Alignment := Self.Alignment;
          ButtonType := Self.ButtonType;
          ButtonVertAlign := Self.ButtonVertAlign;
          CheckColor := Self.CheckColor;
          Images := Self.Images;
          PictureContainer := Self.PictureContainer;
          Ellipsis := Self.Ellipsis;
          ShadowOffset := Self.ShadowOffset;
          ShadowColor := Self.ShadowColor;

          RadioEnable := Self.Enabled;
          if Assigned(FOnIsEnabled) then
            FOnIsEnabled(Self,I,RadioEnable);

          Enabled := RadioEnable;

          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);

        //  Left := ALeft;
        //  Top := (I mod ButtonsPerCol) * ButtonHeight + TopMargin;
          Visible := True;

        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;
procedure TCustomHTMLRadioGroup.OnAnchorClickEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorClick) then begin
  FAnchorClick(Sender,Anchor);
  end;
end;

procedure TCustomHTMLRadioGroup.OnAnchorEnterEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorEnter) then begin
  FAnchorEnter(Sender,Anchor);
  end;
end;

procedure TCustomHTMLRadioGroup.OnAnchorExitEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorExit) then begin
  FAnchorExit(Sender,Anchor);
  end;
end;


procedure TCustomHTMLRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TCustomHTMLRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TCustomHTMLRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TCustomHTMLRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do THTMLGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do THTMLGroupButton(FButtons.Last).Free;
end;

procedure TCustomHTMLRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TCustomHTMLRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        THTMLGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        THTMLGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TCustomHTMLRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomHTMLRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    THTMLGroupButton(FButtons[I]).Caption := FItems[I];
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    THTMLGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TCustomHTMLRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    THTMLGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TCustomHTMLRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TCustomHTMLRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomHTMLRadioGroup.SetButtonType(const Value: TButtonType);
begin
  FButtonType := Value;
  ArrangeButtons;
end;


procedure TCustomHTMLRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TCustomHTMLRadioGroup.SetCheckColor(const Value: TColor);
begin
  FCheckColor := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetButtonVertAlign(
  const Value: TTextLayout);
begin
  fBtnVAlign := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetContainer(
  const Value: TPictureContainer);
begin
  FContainer := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetImages(const Value: TImageList);
begin
  FImages := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages:=nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomHTMLRadioGroup.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLRadioGroup.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  ArrangeButtons;
end;

function TCustomHTMLRadioGroup.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomHTMLRadioGroup.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomHTMLRadioGroup.SetVersion(const Value: string);
begin

end;


{ TGroupCheck }

type
  TGroupCheck = class(THTMLCheckBox)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(CheckGroup: TCustomHTMLCheckGroup);
    destructor Destroy; override;
  end;

constructor TGroupCheck.InternalCreate(CheckGroup: TCustomHTMLCheckGroup);
begin
  inherited Create(CheckGroup);
  CheckGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := CheckGroup.Enabled;
  ParentShowHint := False;
  OnClick := CheckGroup.ButtonClick;
  OnAnchorClick := CheckGroup.OnAnchorClickEvent;
  OnAnchorEnter := CheckGroup.OnAnchorEnterEvent;
  OnAnchorExit  := CheckGroup.OnAnchorExitEvent;
  
  Parent := CheckGroup;
end;

destructor TGroupCheck.Destroy;
begin
  TCustomHTMLCheckGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TGroupCheck.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TCustomHTMLCheckGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TGroupCheck.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TCustomHTMLCheckGroup(Parent).PushKey(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TCustomHTMLCheckGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TGroupCheck.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TCustomHTMLCheckGroup(Parent).PushKeyDown(Key, Shift);
end;


{ TCustomHTMLCheckGroup }

constructor TCustomHTMLCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FColumns := 1;
  FAlignment := taLeftJustify;
  FCheckColor := clBlack;
  FButtonType := btClassic;
  FBtnVAlign := tlTop;
  ShadowOffset := 1;
  ShadowColor := clSilver;
end;

destructor TCustomHTMLCheckGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TCustomHTMLCheckGroup.PushKey(var Key: Char);
begin
  KeyPress(Key);
end;

procedure TCustomHTMLCheckGroup.PushKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key,Shift);
end;

procedure TCustomHTMLCheckGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TCustomHTMLCheckGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
  RadioEnable: Boolean;

begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := I div ButtonsPerCol;
    TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TGroupCheck(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;

          Alignment := Self.Alignment;
          ButtonType := Self.ButtonType;
          ButtonVertAlign := Self.ButtonVertAlign;
          CheckColor := Self.CheckColor;
          Images := Self.Images;
          PictureContainer := Self.PictureContainer;
          Ellipsis := Self.Ellipsis;
          ShadowOffset := Self.ShadowOffset;
          ShadowColor := Self.ShadowColor;

          RadioEnable := self.Enabled;
          if Assigned(FOnIsEnabled) then
            FOnIsEnabled(Self,I,RadioEnable);

          Enabled := RadioEnable;

          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;

        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TCustomHTMLCheckGroup.OnAnchorClickEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorClick) then begin
  FAnchorClick(Sender,Anchor);
  end;
end;

procedure TCustomHTMLCheckGroup.OnAnchorEnterEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorEnter) then begin
  FAnchorEnter(Sender,Anchor);
  end;
end;

procedure TCustomHTMLCheckGroup.OnAnchorExitEvent(Sender:TObject; Anchor:string);
begin
if Assigned(FAnchorExit) then begin
  FAnchorExit(Sender,Anchor);
  end;
end;



procedure TCustomHTMLCheckGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    Changed;
    Click;
  end;
end;

procedure TCustomHTMLCheckGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    UpdateButtons;
  end;
end;

procedure TCustomHTMLCheckGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;

end;

procedure TCustomHTMLCheckGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TCustomHTMLCheckGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do TGroupCheck.InternalCreate(Self);
  while FButtons.Count > Value do TGroupCheck(FButtons.Last).Free;
end;

procedure TCustomHTMLCheckGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TCustomHTMLCheckGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomHTMLCheckGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TGroupCheck(FButtons[I]).Caption := FItems[I];

  ArrangeButtons;
  Invalidate;
end;

procedure TCustomHTMLCheckGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TGroupCheck(FButtons[I]).Enabled := Enabled;
end;

procedure TCustomHTMLCheckGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TCustomHTMLCheckGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomHTMLCheckGroup.SetButtonType(const Value: TButtonType);
begin
  FButtonType := Value;
  ArrangeButtons;
end;


procedure TCustomHTMLCheckGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TCustomHTMLCheckGroup.SetCheckColor(const Value: TColor);
begin
  FCheckColor := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetButtonVertAlign(
  const Value: TTextLayout);
begin
  fBtnVAlign := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetContainer(
  const Value: TPictureContainer);
begin
  FContainer := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetImages(const Value: TImageList);
begin
  FImages := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages:=nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomHTMLCheckGroup.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  ArrangeButtons;
end;

procedure TCustomHTMLCheckGroup.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  ArrangeButtons;
end;


function TCustomHTMLCheckGroup.GetChecked(Index: Integer): Boolean;
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    Result := TGroupCheck(FButtons[Index]).Checked
  else
    raise Exception.Create('Invalid checkbox index');
end;

procedure TCustomHTMLCheckGroup.SetChecked(Index: Integer;
  const Value: Boolean);
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    TGroupCheck(FButtons[Index]).Checked := Value;
end;

function TCustomHTMLCheckGroup.GetReadOnly(Index: Integer): Boolean;
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    Result := not TGroupCheck(FButtons[Index]).Enabled
  else
    raise Exception.Create('Invalid checkbox index');
end;

procedure TCustomHTMLCheckGroup.SetReadOnly(Index: Integer;
  const Value: Boolean);
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    TGroupCheck(FButtons[Index]).Enabled := not Value;
end;


function TCustomHTMLCheckGroup.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomHTMLCheckGroup.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomHTMLCheckGroup.SetVersion(const Value: string);
begin

end;

end.
